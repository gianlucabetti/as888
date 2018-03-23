000100990519     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990519     H*PARMS ACTGRP(QILE)
000300990114     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400870324     F* ANG300R  *-AN/38-*-------------------------------------------*
000500870324     F*------------------*                                           *
000600870324     F*       - MANUTENZIONE CLIENTI   PARTICOLARITA'      -         *
000700870324     F*       ----------------------------------------------         *
000800870324     F*                                                   AZN.: G300 *
000900870324     F*                                                              *
001000990317     F*         PGM. : TII119R   - RICERCA RAG.SOC.USO INTERNO       *
001100990317     F*         PGM. : tii106R   - RICERCA RESE
001200891211     F*         PGM. : CNC0M1R   - RICERCA SPEDIZIONIERI
001300990317     F*         PGM. : tii101R   - RICERCA IMBALLI
001400990317     F*                TII114R   - RICERCA GRUPPO MERCEOLOGICO       *
001500990317     F*                TII115R   - RICERCA SETTORE MERCEOLOGICO      *
001600990317     F*                TII116R   - RICERCA LISTINI                   *
001700870324     F*                XPARCCC   - RICERCA PARAMETRI VARI            *
001800870324     F*                                                              *
001900870324     F*--------------------------------------------------------------*
002000990114     Ftmclp01l  uf a e           k disk
002100870324     F                                     INFDS(INF1)
002200990114     F                                     RENAME(tmCLP000:ANCLP1)
002300870324     F*************************************************************
002400990114     Fancln01l  IF   E           K DISK
002500990114     Fanind01l  IF   E           K DISK
002600990114     Fansog01l  IF   E           K DISK
002700990408     Fancdp01l  IF   E           K DISK
002800870324     FTABEL00F  IF   E           K DISK
002900870324     F*FTAB01LIF  E           K        DISK
003000870324     F*************************************************************
003100870324     FANTAB00F  IF   E           K DISK
003200970801     FFNSPE01L  IF   E           K DISK
003300990317     Ftii004D   CF   E             WORKSTN
003400870324     F*--------------------------------------------------------------*
003500870324     D CAS             S              1    DIM(7)                               SCHIERA CASUALITA
003600870324     D NUO             S              1    DIM(7)                               SCHIERA COD.NUOVO
003700870324     D INF1            DS           502
003800870324     D*-----INFORMATION DS X FILE ANCLP01L
003900870324     D  NREL1                397    400B 0
004000870324     D/EJECT
004100870324     D****************************************************************
004200870324RIC  D PAGAM           DS
004300870324RIC  D*---- KEY PAGAMENTI PER TABEL00F
004400870324RIC  D  VIDCPS                 1      3
004500870324RIC  D  LINGUA                 4      4
004600870324RIC  D  BLANCO                 5      8
004700870324RIC  D  KEYTBL                 1      8
004800870324     D****************************************************************
004900870324     D                 DS
005000870324     D*---- COMMENTI E INFORMAZIONI FORNITORE
005100870324     D  VIDIN1                 1     50
005200870324     D  VIDIN2                51    100
005300870324     D  VIDIN3               101    150
005400870324     D  VIDIN4               151    200
005500870324     D  VIDINF                 1    200
005600870324     D KPJBA         E DS
005700870324     D*---- PASSAGGIO PARAMETRI
005800870324     D  MSGERR               453    502
005900990114     D soc001        E DS                  EXTNAME(xsoc001ds)
006000990114     D xsocds          DS          1000
006100990312     D* Ricerca divise
006200990312     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
006300990519     D*-------------
006400990519     D*  Prototipazione delle procedure
006500990519     D/COPY *LIBL/SRCCPY,PJX002PR
006600870324     C*----------------------------------------------------*
006700870324     C     *ENTRY        PLIST                                                  *
006800870324     C                   PARM                    KPJBA                          *
006900990114     C*---------- RICERCA DITTA :
007000990114     C                   MOVEL     'SOC001'      TIPXSC
007100990114     C                   MOVEL     *blank        SOCXSC
007200990114     C                   EXSR      REPSOC
007300990114     C     RTNXSC        IFNE      '1'
007400990114     C                   MOVEL     XSOCDS        SOC001
007500990114     C                   MOVEL     xscrgs        RSUT             20
007600990115     C                   MOVEL     xscrgs        desdit
007700990115     c                   movel     xscsoc        wsoc
007800990114     c                   end
007900870324     C*                                                    *
008000990114     c     kcln          klist
008100990114     c                   kfld                    wsoc              3
008200990114     c                   kfld                    wksc              8
008300990408     c     kcdp          klist
008400990408     c                   kfld                    wsoc
008500990408     c                   kfld                    clncondpag
008600870324     C******************************************************
008700870324     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
008800870324     C     KEYANT        KLIST                                                  *
008900870324     C                   KFLD                    TABGRU                         *
009000870324     C                   KFLD                    TABCOD                         *
009100870324     C******************************************************
009200870324     C     KEYTAB        KLIST                                                  *
009300990115RIC  C                   KFLD                    CODUTE            1 0          *
009400870324RIC  C                   KFLD                    TIPTAB            2            *
009500870324RIC  C                   KFLD                    KEYTBL                         *
009600990115     c                   z-add     1             codute
009700870324     C*--------- EMISSIONE PROMPT -------------------------*
009800870324     C                   WRITE     ANG3001                                      *
009900990115     C                   MOVE      '?       '    CODFOR                         *
010000870324     C                   MOVE      *BLANKS       DESFOR                         *
010100870324     C     INIZIO        TAG                                                    *
010200870324     C   91                                                                     *
010300870324     COR 92              SETON                                            90    *
010400870324     C                   EXFMT     ANG3002                                      *
010500870324     C                   MOVEA     *ZEROS        *IN(1)                         *
010600990408     C   Kc              GOTO      FINE                                         *
010700990115     C   Kd              CALL      'ANA300R'
010800870415     C                   PARM                    KPJBA
010900990114     C   Kd              GOTO      INIZIO
011000990114     C** GESTIONE PIANO DEI CONTI CMD4
011100870324     C*---------- CONTROLLO DATI INPUT :                   *
011200870324     C     CODFOR        CABEQ     *BLANKS       INIZIO                   91    *
011300870324     C*---------- RICERCA ALFABETICA :                     *
011400990408     c                   if        codfor = '?      ' or codfor = '0000000?'
011500990408     C*    CODFOR        IFEQ      '?       '                                   *
011600870324     C                   MOVE      *BLANKS       CODFOR                         *
011700990317     C                   CALL      'TII119R'                                    *
011800870324     C                   PARM                    CODFOR                         *
011900870324     C                   PARM                    DESFOR                         *
012000870324     C                   GOTO      INIZIO                                       *
012100870324     C                   END                                                    *
012200870324     C**--- CONTROLLO PRESENZA FORNITORE SUI FILES ------***
012300990114     C     CODFOR        CHAIN     tmCLP01L                           10        *
012400870324     C   10              EXSR      RUVIAR                                       * FORN.NUOVO
012500870324     C  N10              EXSR      RUVIVE                                       * VAR. FORNIT.
012600870324     C  N10              Z-ADD     NREL1         NRRSAV            5 0          *
012700870324     C                   TESTN                   CODFOR               70        *
012800870324     C  N10*IN(70)       CABEQ     '0'           SOPRA                          *
012900870324     C   10*IN(70)       CABEQ     '0'           SOPRA1                         *
013000870324     C                   SETON                                        09        *
013100990114     C                   MOVE      CODFOR        wksc                           *
013200870324     C*--- RILASCIO RECORD ANCLP01L                        *
013300870324     C  N10              EXCEPT    RILFOP                                       *
013400870324     C******************************************************
013500990312RIC  C*--- RICERCA RECORD
013600990114RIC  C     Kcln          CHAIN     ancln01l                           15        *
013700990408RIC  C  n15clnsogg       CHAIN     ansog01l                           15        *
013800990408RIC  C  n15kcdp          CHAIN     ancdp01l                           15        *
013900990114RIC  C  N15              MOVEl     sogdes        VIDRAG                         *
014000990114RIC  C  N15              MOVE      clnKCC        VIDCFO                         *
014100990114     C     clnsogg       CHAIN     aNIND000                           15
014200990114     C  N15              MOVE      clncondpag    VIDCDP
014300870401     C   15              MOVE      *BLANKS       VIDCDP
014400990408RIC  C  N15              MOVEL     cdpdescr      DESCPS                         *
014500990408     C   15              MOVEL     *BLANKS       DESCPS                         *
014600990312     C* CONDIZIONI DI PAGAMENTO
014700870324     C     *IN(15)       CABEQ     '1'           INIZIO                   92    *
014800870324     C   10              GOTO      SOPRA1                                       *
014900870324     C*-------------------------------                     *
015000870324     C     SOPRA         TAG                                                    *
015100870324     C                   EXSR      RUTCTL                                       * CONTR. DATI
015200870324     C*--- ERRORI SULLA PRIMA VIDEATA DI MANUTENZIONE -----*
015300870324     C     SOPRA1        TAG                                                    *
015400870324     C   98                                                                     *
015500870324     COR 97                                                                     *
015600870324     COR 96                                                                     *
015700870324     COR 95                                                                     *
015800870324     COR 94                                                                     *
015900870324     COR 93              SETON                                        90        *
016000870324     C   91              SETON                                        90        *
016100870324     C   92              SETON                                        90        *
016200870401     C   88              SETON                                        90        *
016300870401     C   87              SETON                                        90        *
016400870324     C*--- EMISSIONE VIDEATA DI MANUTENZIONE :             *
016500870324     C                   EXFMT     ANG3003                                      *
016600870401     C                   MOVEA     *ZEROS        *IN(87)                        *
016700990408     C   Kl              MOVE      *BLANKS       DESFOR                         *
016800990408     C   Kl              GOTO      INIZIO                                       *
016900870324     C*--- PASSAGGIO DA FORN.POTENZIALE A EFFETTIVO :      *
017000870324     C                   GOTO      SOPRA                                        *
017100870324     C*----------------------------------------------------*
017200870324     C     FINE          TAG                                                    *
017300870324     C                   SETON                                            LR    *
017400870324     C*--- CONTROLLO VALIDITA' CAMPI ----------------------*
017500870324     C***------------------------------------------------***
017600870324     CSR   RUTCTL        BEGSR                                                  *
017700870324     C*---------                                           * RAG.SOC.
017800870324     C   10VIDRSI        CABEQ     *BLANKS       SOPRA1                   92    *
017900870324     C  N10VIDRSI        IFEQ      *BLANKS                                      *
018000870324     C                   TESTN                   CODFOR               70        *
018100870324     C     *IN(70)       CABEQ     '0'           SOTRSI                         *
018200870324     C                   MOVEL     VIDRAG        VIDRSI                         *
018300870324     C     SOTRSI        TAG                                                    *
018400870324     C                   END                                                    *
018500870324     C*---------                                           * GRUP.MERC.
018600870324     C     VIDGPM        IFEQ      '? '                                         *
018700870324     C                   MOVE      *BLANKS       VIDGPM                         *
018800870324     C                   MOVE      *BLANKS       DESGPM                         *
018900990317     C                   CALL      'TII114R'                                    *
019000870324     C                   PARM                    VIDGPM                         *
019100870324     C                   PARM                    DESGPM                         *
019200870324     C     VIDGPM        COMP      '99'                                   93    *
019300870324     C                   GOTO      SOPRA1                                       *
019400870324     C                   ELSE                                                   *
019500870324     C                   MOVE      '159'         TABGRU                         *
019600990115     c                   MOVE      *ZEROS        TABCOD                         *
019700870324     C                   MOVE      VIDGPM        TABCOD                         *
019800870324     C     KEYANT        CHAIN     ANTAB00F                           93        *
019900870324     C   93              GOTO      SOPRA1                                       *
020000870324     C                   MOVEL     TABDES        DESGPM                         *
020100870324     C                   END                                                    *
020200870324     C*---------                                           * SETTORE
020300870324     C     VIDSTM        IFEQ      '? '                                         *
020400870324     C                   MOVE      *BLANKS       VIDSTM                         *
020500871120     C                   MOVE      *BLANKS       DESSTM                         *
020600990317     C                   CALL      'TII115R'                                    *
020700870324     C                   PARM                    VIDSTM                         *
020800870324     C                   PARM                    DESSTM                         *
020900870324     C     VIDSTM        COMP      '99'                                   94    *
021000870324     C                   GOTO      SOPRA1                                       *
021100870324     C                   ELSE                                                   *
021200870324     C                   MOVE      '160'         TABGRU                         *
021300870324     C                   MOVE      *ZEROS        TABCOD                         *
021400870324     C                   MOVE      VIDSTM        TABCOD                         *
021500870324     C     KEYANT        CHAIN     ANTAB00F                           94        *
021600870324     C   94              GOTO      SOPRA1                                       *
021700870324     C                   MOVEL     TABDES        DESSTM                         *
021800870324     C                   END                                                    *
021900870324     C*---------                                           * PAGAMENTO
022000870324     C*--------- LISTINO ----------------------------------* LISTINO
022100870324     C     VIDLIS        IFEQ      '?  '                                        *
022200870324     C                   MOVE      *BLANKS       VIDLIS                         *
022300870324     C                   MOVE      *BLANKS       DESCRI           75            *
022400990317     C                   CALL      'TII116R'                                    *
022500870324     C                   PARM                    VIDLIS                         *
022600870324     C                   PARM                    DESCRI                         *
022700870324     C                   MOVEL     DESCRI        DESLIS                         *
022800870324     C     VIDLIS        COMP      '999'                                  97    *
022900870324     C                   GOTO      SOPRA1                                       *
023000870324     C                   ELSE                                                   *
023100870324     C                   MOVE      '161'         TABGRU                         *
023200870324     C                   MOVE      *ZEROS        TABCOD                         *
023300870324     C                   MOVE      VIDLIS        TABCOD                         *
023400870324     C     KEYANT        CHAIN     ANTAB00F                           97        *
023500870324     C   97              GOTO      SOPRA1                                       *
023600870324     C                   MOVEL     TABDES        DESLIS                         *
023700870324     C                   END                                                    *
023800870324     C*---------                                           * RESA      V.
023900870324     C     VIDRES        IFEQ      '?  '                                         *
024000870324     C                   MOVE      *BLANKS       VIDRES                         *
024100870324     C                   MOVE      *BLANKS       DESRES           15            *
024200990317     C                   CALL      'TII106R'                                    *
024300870324     C                   PARM                    VIDRES                         *
024400870324     C                   PARM                    DESRES                         *
024500870324     C     VIDRES        COMP      '99'                                   98    *
024600870324     C                   GOTO      SOPRA1                                       *
024700870324     C                   ELSE                                                   *
024800870324     C                   MOVE      '019'         TABGRU                         *
024900870324     C                   MOVE      *ZEROS        TABCOD                         *
025000870324     C                   MOVE      VIDRES        TABCOD                         *
025100870324     C     KEYANT        CHAIN     ANTAB00F                           98        *
025200870324     C   98              GOTO      SOPRA1                                       *
025300870324     C                   END                                                    *
025400870324     C*---------                                           *
025500870324     C     VIDIMB        IFEQ      '?  '                                        *IMBALLO
025600870324     C                   MOVE      *BLANKS       VIDIMB                         *
025700870324     C                   MOVE      *BLANKS       DESIMB           10            *
025800990317     C                   CALL      'TII101R'                                    *
025900870324     C                   PARM                    VIDIMB                         *
026000870324     C                   PARM                    DESIMB                         *
026100870324     C     VIDIMB        COMP      '99'                                   95    *
026200870324     C                   GOTO      SOPRA1                                       *
026300870324     C                   ELSE                                                   *
026400870324     C                   MOVE      '008'         TABGRU                         *
026500870324     C                   MOVE      *ZEROS        TABCOD                         *
026600870324     C                   MOVE      VIDIMB        TABCOD                         *
026700870324     C     KEYANT        CHAIN     ANTAB00F                           95        *
026800870324     C   95              GOTO      SOPRA1                                       *
026900870324     C                   END                                                    *
027000870324     C**---------
027100871118     C     VIDSPE        IFEQ      '?  '                                         *
027200891211     C                   CALL      'CNC0M6R'                                    *
027300871118     C                   PARM                    KPJBA
027400871118     C                   GOTO      SOPRA1                                       *
027500871118     C                   END                                                    *
027600871120     C     KSPE          KLIST
027700970801     C                   KFLD                    SPEFLS
027800871120     C                   KFLD                    SPEKSC            7 0
027900871120     C                   KFLD                    VIDSPE
028000970801     C                   MOVEL     'S'           SPEFLS
028100871120     C                   Z-ADD     9999999       SPEKSC
028200970801     C     KSPE          CHAIN     FNSPE01L                           91
028300871120     C   91              GOTO      SOPRA1
028400990312     C*----------RICERCA valuta  :                         *
028500990312     c                   if        vidval = '?   '
028600990312     C                   MOVEL     '01'          OPZ015
028700990312     C                   MOVEL     *blank        Div015
028800990312     C                   MOVEL     *blank        vidvald
028900990312     C                   MOVEL     A015DS        KPJBU
029000990312     C                   CALL      'ANA015R'
029100990312     C                   PARM                    KPJBA
029200990312     C                   MOVEL     KPJBU         A015DS
029300990312     C     Div015        IFNE      *BLANK
029400990312     C                   MOVEL     Div015        vidval
029500990312     C                   GOTO      sopra1                                       *
029600990312     C                   ENDIF
029700990312     C                   ENDIF
029800990312     C     vidval        CABEQ     *BLANKS       sopra1                   87    *
029900990519     C                   if        not *in87
030000990519     C                   clear                   Errore            3 0
030100990519     C                   move      vidval        divisa0202
030200990519     C                   clear                   Dtrif0202
030300990519     C                   eval      Errore = X0201RpDVS(Divisa0202:Dtrif0202:
030400990519     C                                      ANDVS00F: %size(ANDVS00F) :
030500990519     C                                      ANDVX00F: %size(ANDVX00F))
030600990519     c                   if        Errore = 1 or Errore = 3
030700990519     C                   seton                                        87
030800990519     C                   end
030900990519     C                   end
031000990519     c  n87              movel     dvsdesbrev    vidvald
031100990312     c   87              goto      sopra1
031200870324     C     SCRIVI        TAG                                                    *
031300990408     c   kf              do
031400870324     C                   EXSR      RUTFIL                                       *
031500990408     c                   goto      inizio
031600990408     c                   enddo
031700870324     CSR                 ENDSR                                                  *
031800870324     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
031900870324     C**--- INSERIMENTO FORNITORE NUOVO -----------------***
032000870324     C***------------------------------------------------***
032100870324     CSR   RUVIAR        BEGSR                                                  *
032200990114     C                   MOVE      CLnkcc        VIDCFO                         *
032300870324     C                   MOVE      *BLANKS       VIDRAG                         *
032400870324     C*--------------------                                *
032500870324     C                   MOVE      *BLANKS       VIDRSI                         *
032600870324     C                   MOVE      '? '          VIDGPM                         *
032700870324     C                   MOVE      *BLANKS       DESGPM                         *
032800870324     C                   MOVE      '? '          VIDSTM                         *
032900870324     C                   MOVE      *BLANKS       DESSTM                         *
033000870324     C                   MOVE      *ZEROS        VIDSC1                         *
033100870324     C                   MOVE      *ZEROS        VIDSC2                         *
033200871118     C                   MOVE      '1  '         VIDLIS                         *
033300870324     C                   MOVE      *BLANKS       DESLIS                         *
033400870324     C                   MOVE      '?  '         VIDRES                         *
033500871118     C                   MOVE      '1  '         VIDIMB                         *
033600870324     C                   MOVE      '?  '         VIDSPE                         *
033700870324     C                   MOVE      *BLANKS       VIDQCL                         *
033800870324     C                   MOVE      *BLANKS       VIDINF                         *
033900871118     C                   MOVE      '5'           VIDPCL                         *
034000990312     C                   MOVE      xscdiv        VIDval                         *
034100990312     C                   MOVE      *blanks       VIDvald                        *
034200870324     CSR                 ENDSR                                                  *
034300870324     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
034400870324     C**--- AGGIORNAMENTO FORNITORE ESISTENTE : ---------***
034500870324     C***------------------------------------------------***
034600870324     CSR   RUVIVE        BEGSR                                                  *
034700870324     C                   MOVE      CLPRSI        VIDRSI                         *
034800870324     C                   MOVE      CLPKCC        VIDCFO                         *
034900870324     C                   MOVE      CLPGPM        VIDGPM                         *
035000870324     C                   MOVE      *BLANKS       DESGPM                         *
035100870324     C                   MOVE      CLPSTM        VIDSTM                         *
035200870324     C                   MOVE      *BLANKS       DESSTM                         *
035300870324     C                   MOVE      *BLANKS       DESCPS                         *
035400870324     C                   MOVE      CLPSC1        VIDSC1                         *
035500870324     C                   MOVE      CLPSC2        VIDSC2                         *
035600870324     C                   MOVE      CLPLIS        VIDLIS                         *
035700870324     C                   MOVE      *BLANKS       DESLIS                         *
035800870324     C                   MOVE      CLPRES        VIDRES                         *
035900870324     C                   MOVE      CLPQCL        VIDQCL                         *
036000870324     C                   MOVE      CLPINF        VIDINF                         *
036100870401     C                   MOVEL     CLPCDF        VIDCDF
036200870324     C                   MOVE      CLPIMB        VIDIMB
036300870324     C                   MOVE      CLPSPE        VIDSPE
036400870324     C                   MOVE      CLPPCL        VIDPCL
036500990312     C                   MOVE      CLPval        VIDval
036600870324     CSR                 ENDSR                                                  *
036700870324     C***------------------------------------------------***
036800870324     C**--- IMPOSTAZIONE CAMPI X SCRITTURA FILE ---------***
036900870324     C***------------------------------------------------***
037000870324     CSR   RUTFIL        BEGSR                                                  *
037100990115     C*--- FILE tmCLP00F :                                 *
037200990115     C     CODFOR        CHAIN     tmCLP01L                           15        *
037300990115     C                   MOVE      xsccut        CLPUTE                         *
037400870324     C                   MOVE      VIDCFO        CLPKCC                         *
037500870324     C                   MOVE      CODFOR        CLPKSC                         *
037600870324     C                   MOVE      VIDRSI        CLPRSI                         *
037700870324     C                   MOVE      VIDGPM        CLPGPM                         *
037800870324     C                   MOVE      VIDSTM        CLPSTM                         *
037900870324     C                   MOVE      VIDSC1        CLPSC1                         *
038000870324     C                   MOVE      VIDSC2        CLPSC2                         *
038100870324     C                   MOVE      VIDLIS        CLPLIS                         *
038200870324     C                   MOVE      VIDQCL        CLPQCL                         *
038300990312     C                   MOVE      vidval        CLPVAL                         *
038400870324     C                   MOVE      VIDINF        CLPINF                         *
038500891211     C                   MOVE      VIDIMB        CLPIMB
038600891211     C                   MOVE      VIDSPE        CLPSPE
038700870324     C                   MOVE      VIDRES        CLPRES
038800870324     C                   MOVE      VIDPCL        CLPPCL
038900870401     C                   MOVEL     VIDCDF        CLPCDF
039000870324     C   15              WRITE     ANCLP1                                       *
039100870324     C   15              Z-ADD     NREL1         NRRSAV                         *
039200870324     C  N15              UPDATE    ANCLP1                                       *
039300870324     C                   SETOFF                                       10        *
039400870324     CSR                 ENDSR                                                  *
039500990114     C*----------------------------------------------------*
039600990114     C* Reperimento dati società
039700990114     C*----------------------------------------------------*
039800990114     C     REPSOC        BEGSR
039900990114     C*
040000990114     C                   CALLB     'XSOC'
040100990114     C                   PARM                    TIPXSC            6
040200990114     C                   PARM                    SOCXSC            3
040300990114     C                   PARM                    CDSXSC            9 0
040400990114     C                   PARM                    MODXSC            3
040500990114     C                   PARM      *blanks       RTNXSC            1
040600990114     C                   PARM                    XSOCDS
040700990114     C                   PARM                    KPJBA
040800990114     C*
040900990114     C                   ENDSR
041000870324     O*** RILASCIO RECORDS ------------------------------***
041100870324     OANCLP1    E            RILFOP
