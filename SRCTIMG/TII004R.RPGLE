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
003200010725     FTNTBE01l  IF   E           k disk
003300970801     FFNSPE01L  IF   E           K DISK
003400990317     Ftii004D   CF   E             WORKSTN
003500870324     F*--------------------------------------------------------------*
003600870324     D CAS             S              1    DIM(7)                               SCHIERA CASUALITA
003700870324     D NUO             S              1    DIM(7)                               SCHIERA COD.NUOVO
003800870324     D INF1            DS           502
003900870324     D*-----INFORMATION DS X FILE ANCLP01L
004000870324     D  NREL1                397    400B 0
004100870324     D/EJECT
004200870324     D****************************************************************
004300870324RIC  D PAGAM           DS
004400870324RIC  D*---- KEY PAGAMENTI PER TABEL00F
004500870324RIC  D  VIDCPS                 1      3
004600870324RIC  D  LINGUA                 4      4
004700870324RIC  D  BLANCO                 5      8
004800870324RIC  D  KEYTBL                 1      8
004900870324     D****************************************************************
005000870324     D                 DS
005100870324     D*---- COMMENTI E INFORMAZIONI FORNITORE
005200870324     D  VIDIN1                 1     50
005300870324     D  VIDIN2                51    100
005400870324     D  VIDIN3               101    150
005500870324     D  VIDIN4               151    200
005600870324     D  VIDINF                 1    200
005700870324     D KPJBA         E DS
005800870324     D*---- PASSAGGIO PARAMETRI
005900870324     D  MSGERR               453    502
006000990114     D soc001        E DS                  EXTNAME(xsoc001ds)
006100990114     D xsocds          DS          1000
006200990312     D* Ricerca divise
006300990312     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
006400010725    >D DGED          E DS
006500990519     D*-------------
006600990519     D*  Prototipazione delle procedure
006700990519     D/COPY *LIBL/SRCCPY,PJX002PR
006800870324     C*----------------------------------------------------*
006900870324     C     *ENTRY        PLIST                                                  *
007000870324     C                   PARM                    KPJBA                          *
007100990114     C*---------- RICERCA DITTA :
007200990114     C                   MOVEL     'SOC001'      TIPXSC
007300990114     C                   MOVEL     *blank        SOCXSC
007400990114     C                   EXSR      REPSOC
007500990114     C     RTNXSC        IFNE      '1'
007600990114     C                   MOVEL     XSOCDS        SOC001
007700990114     C                   MOVEL     xscrgs        RSUT             20
007800990115     C                   MOVEL     xscrgs        desdit
007900990115     c                   movel     xscsoc        wsoc
008000990114     c                   end
008100870324     C*                                                    *
008200990114     c     kcln          klist
008300990114     c                   kfld                    wsoc              3
008400990114     c                   kfld                    wksc              8
008500990408     c     kcdp          klist
008600990408     c                   kfld                    wsoc
008700990408     c                   kfld                    clncondpag
008800870324     C******************************************************
008900870324     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
009000870324     C     KEYANT        KLIST                                                  *
009100870324     C                   KFLD                    TABGRU                         *
009200870324     C                   KFLD                    TABCOD                         *
009300870324     C******************************************************
009400870324     C     KEYTAB        KLIST                                                  *
009500990115RIC  C                   KFLD                    CODUTE            1 0          *
009600870324RIC  C                   KFLD                    TIPTAB            2            *
009700870324RIC  C                   KFLD                    KEYTBL                         *
009800990115     c                   z-add     1             codute
009900010725     C*--- Reperisco la divisa corrente ------------------------------*
010000010725     C     ktbe          KLIST
010100010725     C                   KFLD                    kcod
010200010725     C                   KFLD                    kke1
010300010725     C     *like         Define    tbecod        kcod
010400010725     C     *like         Define    tbeke1        kke1
010500010725     C                   move      'GED'         kcod
010600010725     C                   move      *blanks       kke1
010700010725     C                   movel     'MAGAZZINO'   kke1
010800010725     c                   clear                   dged
010900010725     C     ktbe          CHAIN     tntbe01l                           54
011000010725     C  n54              movel     tbeuni        dged
011100870324     C*--------- EMISSIONE PROMPT -------------------------*
011200870324     C                   WRITE     ANG3001                                      *
011300990115     C                   MOVE      '?       '    CODFOR                         *
011400870324     C                   MOVE      *BLANKS       DESFOR                         *
011500870324     C     INIZIO        TAG                                                    *
011600870324     C   91                                                                     *
011700870324     COR 92              SETON                                            90    *
011800870324     C                   EXFMT     ANG3002                                      *
011900870324     C                   MOVEA     *ZEROS        *IN(1)                         *
012000990408     C   Kc              GOTO      FINE                                         *
012100990115     C   Kd              CALL      'ANA300R'
012200870415     C                   PARM                    KPJBA
012300990114     C   Kd              GOTO      INIZIO
012400990114     C** GESTIONE PIANO DEI CONTI CMD4
012500870324     C*---------- CONTROLLO DATI INPUT :                   *
012600870324     C     CODFOR        CABEQ     *BLANKS       INIZIO                   91    *
012700870324     C*---------- RICERCA ALFABETICA :                     *
012800990408     c                   if        codfor = '?      ' or codfor = '0000000?'
012900990408     C*    CODFOR        IFEQ      '?       '                                   *
013000870324     C                   MOVE      *BLANKS       CODFOR                         *
013100990317     C                   CALL      'TII119R'                                    *
013200870324     C                   PARM                    CODFOR                         *
013300870324     C                   PARM                    DESFOR                         *
013400870324     C                   GOTO      INIZIO                                       *
013500870324     C                   END                                                    *
013600870324     C**--- CONTROLLO PRESENZA FORNITORE SUI FILES ------***
013700990114     C     CODFOR        CHAIN     tmCLP01L                           10        *
013800870324     C   10              EXSR      RUVIAR                                       * FORN.NUOVO
013900870324     C  N10              EXSR      RUVIVE                                       * VAR. FORNIT.
014000870324     C  N10              Z-ADD     NREL1         NRRSAV            5 0          *
014100870324     C                   TESTN                   CODFOR               70        *
014200870324     C  N10*IN(70)       CABEQ     '0'           SOPRA                          *
014300870324     C   10*IN(70)       CABEQ     '0'           SOPRA1                         *
014400870324     C                   SETON                                        09        *
014500990114     C                   MOVE      CODFOR        wksc                           *
014600870324     C*--- RILASCIO RECORD ANCLP01L                        *
014700870324     C  N10              EXCEPT    RILFOP                                       *
014800870324     C******************************************************
014900990312RIC  C*--- RICERCA RECORD
015000990114RIC  C     Kcln          CHAIN     ancln01l                           15        *
015100990408RIC  C  n15clnsogg       CHAIN     ansog01l                           15        *
015200990408RIC  C  n15kcdp          CHAIN     ancdp01l                           15        *
015300990114RIC  C  N15              MOVEl     sogdes        VIDRAG                         *
015400990114RIC  C  N15              MOVE      clnKCC        VIDCFO                         *
015500990114     C     clnsogg       CHAIN     aNIND000                           15
015600990114     C  N15              MOVE      clncondpag    VIDCDP
015700870401     C   15              MOVE      *BLANKS       VIDCDP
015800990408RIC  C  N15              MOVEL     cdpdescr      DESCPS                         *
015900990408     C   15              MOVEL     *BLANKS       DESCPS                         *
016000990312     C* CONDIZIONI DI PAGAMENTO
016100870324     C     *IN(15)       CABEQ     '1'           INIZIO                   92    *
016200870324     C   10              GOTO      SOPRA1                                       *
016300870324     C*-------------------------------                     *
016400870324     C     SOPRA         TAG                                                    *
016500870324     C                   EXSR      RUTCTL                                       * CONTR. DATI
016600870324     C*--- ERRORI SULLA PRIMA VIDEATA DI MANUTENZIONE -----*
016700870324     C     SOPRA1        TAG                                                    *
016800870324     C   98                                                                     *
016900870324     COR 97                                                                     *
017000870324     COR 96                                                                     *
017100870324     COR 95                                                                     *
017200870324     COR 94                                                                     *
017300870324     COR 93              SETON                                        90        *
017400870324     C   91              SETON                                        90        *
017500870324     C   92              SETON                                        90        *
017600870401     C   88              SETON                                        90        *
017700870401     C   87              SETON                                        90        *
017800870324     C*--- EMISSIONE VIDEATA DI MANUTENZIONE :             *
017900870324     C                   EXFMT     ANG3003                                      *
018000870401     C                   MOVEA     *ZEROS        *IN(87)                        *
018100990408     C   Kl              MOVE      *BLANKS       DESFOR                         *
018200990408     C   Kl              GOTO      INIZIO                                       *
018300870324     C*--- PASSAGGIO DA FORN.POTENZIALE A EFFETTIVO :      *
018400870324     C                   GOTO      SOPRA                                        *
018500870324     C*----------------------------------------------------*
018600870324     C     FINE          TAG                                                    *
018700870324     C                   SETON                                            LR    *
018800870324     C*--- CONTROLLO VALIDITA' CAMPI ----------------------*
018900870324     C***------------------------------------------------***
019000870324     CSR   RUTCTL        BEGSR                                                  *
019100870324     C*---------                                           * RAG.SOC.
019200870324     C   10VIDRSI        CABEQ     *BLANKS       SOPRA1                   92    *
019300870324     C  N10VIDRSI        IFEQ      *BLANKS                                      *
019400870324     C                   TESTN                   CODFOR               70        *
019500870324     C     *IN(70)       CABEQ     '0'           SOTRSI                         *
019600870324     C                   MOVEL     VIDRAG        VIDRSI                         *
019700870324     C     SOTRSI        TAG                                                    *
019800870324     C                   END                                                    *
019900870324     C*---------                                           * GRUP.MERC.
020000870324     C     VIDGPM        IFEQ      '? '                                         *
020100870324     C                   MOVE      *BLANKS       VIDGPM                         *
020200870324     C                   MOVE      *BLANKS       DESGPM                         *
020300990317     C                   CALL      'TII114R'                                    *
020400870324     C                   PARM                    VIDGPM                         *
020500870324     C                   PARM                    DESGPM                         *
020600870324     C     VIDGPM        COMP      '99'                                   93    *
020700870324     C                   GOTO      SOPRA1                                       *
020800870324     C                   ELSE                                                   *
020900870324     C                   MOVE      '159'         TABGRU                         *
021000990115     c                   MOVE      *ZEROS        TABCOD                         *
021100870324     C                   MOVE      VIDGPM        TABCOD                         *
021200870324     C     KEYANT        CHAIN     ANTAB00F                           93        *
021300870324     C   93              GOTO      SOPRA1                                       *
021400870324     C                   MOVEL     TABDES        DESGPM                         *
021500870324     C                   END                                                    *
021600870324     C*---------                                           * SETTORE
021700870324     C     VIDSTM        IFEQ      '? '                                         *
021800870324     C                   MOVE      *BLANKS       VIDSTM                         *
021900871120     C                   MOVE      *BLANKS       DESSTM                         *
022000990317     C                   CALL      'TII115R'                                    *
022100870324     C                   PARM                    VIDSTM                         *
022200870324     C                   PARM                    DESSTM                         *
022300870324     C     VIDSTM        COMP      '99'                                   94    *
022400870324     C                   GOTO      SOPRA1                                       *
022500870324     C                   ELSE                                                   *
022600870324     C                   MOVE      '160'         TABGRU                         *
022700870324     C                   MOVE      *ZEROS        TABCOD                         *
022800870324     C                   MOVE      VIDSTM        TABCOD                         *
022900870324     C     KEYANT        CHAIN     ANTAB00F                           94        *
023000870324     C   94              GOTO      SOPRA1                                       *
023100870324     C                   MOVEL     TABDES        DESSTM                         *
023200870324     C                   END                                                    *
023300870324     C*---------                                           * PAGAMENTO
023400870324     C*--------- LISTINO ----------------------------------* LISTINO
023500870324     C     VIDLIS        IFEQ      '?  '                                        *
023600870324     C                   MOVE      *BLANKS       VIDLIS                         *
023700870324     C                   MOVE      *BLANKS       DESCRI           75            *
023800990317     C                   CALL      'TII116R'                                    *
023900870324     C                   PARM                    VIDLIS                         *
024000870324     C                   PARM                    DESCRI                         *
024100870324     C                   MOVEL     DESCRI        DESLIS                         *
024200870324     C     VIDLIS        COMP      '999'                                  97    *
024300870324     C                   GOTO      SOPRA1                                       *
024400870324     C                   ELSE                                                   *
024500870324     C                   MOVE      '161'         TABGRU                         *
024600870324     C                   MOVE      *ZEROS        TABCOD                         *
024700870324     C                   MOVE      VIDLIS        TABCOD                         *
024800870324     C     KEYANT        CHAIN     ANTAB00F                           97        *
024900870324     C   97              GOTO      SOPRA1                                       *
025000870324     C                   MOVEL     TABDES        DESLIS                         *
025100870324     C                   END                                                    *
025200870324     C*---------                                           * RESA      V.
025300870324     C     VIDRES        IFEQ      '?  '                                         *
025400870324     C                   MOVE      *BLANKS       VIDRES                         *
025500870324     C                   MOVE      *BLANKS       DESRES           15            *
025600990317     C                   CALL      'TII106R'                                    *
025700870324     C                   PARM                    VIDRES                         *
025800870324     C                   PARM                    DESRES                         *
025900870324     C     VIDRES        COMP      '99'                                   98    *
026000870324     C                   GOTO      SOPRA1                                       *
026100870324     C                   ELSE                                                   *
026200870324     C                   MOVE      '019'         TABGRU                         *
026300870324     C                   MOVE      *ZEROS        TABCOD                         *
026400870324     C                   MOVE      VIDRES        TABCOD                         *
026500870324     C     KEYANT        CHAIN     ANTAB00F                           98        *
026600870324     C   98              GOTO      SOPRA1                                       *
026700870324     C                   END                                                    *
026800870324     C*---------                                           *
026900870324     C     VIDIMB        IFEQ      '?  '                                        *IMBALLO
027000870324     C                   MOVE      *BLANKS       VIDIMB                         *
027100870324     C                   MOVE      *BLANKS       DESIMB           10            *
027200990317     C                   CALL      'TII101R'                                    *
027300870324     C                   PARM                    VIDIMB                         *
027400870324     C                   PARM                    DESIMB                         *
027500870324     C     VIDIMB        COMP      '99'                                   95    *
027600870324     C                   GOTO      SOPRA1                                       *
027700870324     C                   ELSE                                                   *
027800870324     C                   MOVE      '008'         TABGRU                         *
027900870324     C                   MOVE      *ZEROS        TABCOD                         *
028000870324     C                   MOVE      VIDIMB        TABCOD                         *
028100870324     C     KEYANT        CHAIN     ANTAB00F                           95        *
028200870324     C   95              GOTO      SOPRA1                                       *
028300870324     C                   END                                                    *
028400870324     C**---------
028500871118     C     VIDSPE        IFEQ      '?  '                                         *
028600891211     C                   CALL      'CNC0M6R'                                    *
028700871118     C                   PARM                    KPJBA
028800871118     C                   GOTO      SOPRA1                                       *
028900871118     C                   END                                                    *
029000871120     C     KSPE          KLIST
029100970801     C                   KFLD                    SPEFLS
029200871120     C                   KFLD                    SPEKSC            7 0
029300871120     C                   KFLD                    VIDSPE
029400970801     C                   MOVEL     'S'           SPEFLS
029500871120     C                   Z-ADD     9999999       SPEKSC
029600970801     C     KSPE          CHAIN     FNSPE01L                           91
029700871120     C   91              GOTO      SOPRA1
029800990312     C*----------RICERCA valuta  :                         *
029900990312     c                   if        vidval = '?   '
030000990312     C                   MOVEL     '01'          OPZ015
030100990312     C                   MOVEL     *blank        Div015
030200990312     C                   MOVEL     *blank        vidvald
030300990312     C                   MOVEL     A015DS        KPJBU
030400990312     C                   CALL      'ANA015R'
030500990312     C                   PARM                    KPJBA
030600990312     C                   MOVEL     KPJBU         A015DS
030700990312     C     Div015        IFNE      *BLANK
030800990312     C                   MOVEL     Div015        vidval
030900990312     C                   GOTO      sopra1                                       *
031000990312     C                   ENDIF
031100990312     C                   ENDIF
031200990312     C     vidval        CABEQ     *BLANKS       sopra1                   87    *
031300990519     C                   if        not *in87
031400990519     C                   clear                   Errore            3 0
031500990519     C                   move      vidval        divisa0202
031600990519     C                   clear                   Dtrif0202
031700990519     C                   eval      Errore = X0201RpDVS(Divisa0202:Dtrif0202:
031800990519     C                                      ANDVS00F: %size(ANDVS00F) :
031900990519     C                                      ANDVX00F: %size(ANDVX00F))
032000990519     c                   if        Errore = 1 or Errore = 3
032100990519     C                   seton                                        87
032200990519     C                   end
032300990519     C                   end
032400990519     c  n87              movel     dvsdesbrev    vidvald
032500990312     c   87              goto      sopra1
032600870324     C     SCRIVI        TAG                                                    *
032700990408     c   kf              do
032800870324     C                   EXSR      RUTFIL                                       *
032900990408     c                   goto      inizio
033000990408     c                   enddo
033100870324     CSR                 ENDSR                                                  *
033200870324     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
033300870324     C**--- INSERIMENTO FORNITORE NUOVO -----------------***
033400870324     C***------------------------------------------------***
033500870324     CSR   RUVIAR        BEGSR                                                  *
033600990114     C                   MOVE      CLnkcc        VIDCFO                         *
033700870324     C                   MOVE      *BLANKS       VIDRAG                         *
033800870324     C*--------------------                                *
033900870324     C                   MOVE      *BLANKS       VIDRSI                         *
034000870324     C                   MOVE      '? '          VIDGPM                         *
034100870324     C                   MOVE      *BLANKS       DESGPM                         *
034200870324     C                   MOVE      '? '          VIDSTM                         *
034300870324     C                   MOVE      *BLANKS       DESSTM                         *
034400870324     C                   MOVE      *ZEROS        VIDSC1                         *
034500870324     C                   MOVE      *ZEROS        VIDSC2                         *
034600871118     C                   MOVE      '1  '         VIDLIS                         *
034700870324     C                   MOVE      *BLANKS       DESLIS                         *
034800870324     C                   MOVE      '?  '         VIDRES                         *
034900871118     C                   MOVE      '1  '         VIDIMB                         *
035000870324     C                   MOVE      '?  '         VIDSPE                         *
035100870324     C                   MOVE      *BLANKS       VIDQCL                         *
035200870324     C                   MOVE      *BLANKS       VIDINF                         *
035300871118     C                   MOVE      '5'           VIDPCL                         *
035400010725     c                   if        §gedcn <> *blank
035500010725     c                   movel     §gedcn        vidval
035600010725     c                   else
035700990312     C                   MOVE      xscdiv        VIDval                         *
035800010725     c                   end
035900990312     C                   MOVE      *blanks       VIDvald                        *
036000870324     CSR                 ENDSR                                                  *
036100870324     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
036200870324     C**--- AGGIORNAMENTO FORNITORE ESISTENTE : ---------***
036300870324     C***------------------------------------------------***
036400870324     CSR   RUVIVE        BEGSR                                                  *
036500870324     C                   MOVE      CLPRSI        VIDRSI                         *
036600870324     C                   MOVE      CLPKCC        VIDCFO                         *
036700870324     C                   MOVE      CLPGPM        VIDGPM                         *
036800870324     C                   MOVE      *BLANKS       DESGPM                         *
036900870324     C                   MOVE      CLPSTM        VIDSTM                         *
037000870324     C                   MOVE      *BLANKS       DESSTM                         *
037100870324     C                   MOVE      *BLANKS       DESCPS                         *
037200870324     C                   MOVE      CLPSC1        VIDSC1                         *
037300870324     C                   MOVE      CLPSC2        VIDSC2                         *
037400870324     C                   MOVE      CLPLIS        VIDLIS                         *
037500870324     C                   MOVE      *BLANKS       DESLIS                         *
037600870324     C                   MOVE      CLPRES        VIDRES                         *
037700870324     C                   MOVE      CLPQCL        VIDQCL                         *
037800870324     C                   MOVE      CLPINF        VIDINF                         *
037900870401     C                   MOVEL     CLPCDF        VIDCDF
038000870324     C                   MOVE      CLPIMB        VIDIMB
038100870324     C                   MOVE      CLPSPE        VIDSPE
038200870324     C                   MOVE      CLPPCL        VIDPCL
038300990312     C                   MOVE      CLPval        VIDval
038400870324     CSR                 ENDSR                                                  *
038500870324     C***------------------------------------------------***
038600870324     C**--- IMPOSTAZIONE CAMPI X SCRITTURA FILE ---------***
038700870324     C***------------------------------------------------***
038800870324     CSR   RUTFIL        BEGSR                                                  *
038900990115     C*--- FILE tmCLP00F :                                 *
039000990115     C     CODFOR        CHAIN     tmCLP01L                           15        *
039100990115     C                   MOVE      xsccut        CLPUTE                         *
039200870324     C                   MOVE      VIDCFO        CLPKCC                         *
039300870324     C                   MOVE      CODFOR        CLPKSC                         *
039400870324     C                   MOVE      VIDRSI        CLPRSI                         *
039500870324     C                   MOVE      VIDGPM        CLPGPM                         *
039600870324     C                   MOVE      VIDSTM        CLPSTM                         *
039700870324     C                   MOVE      VIDSC1        CLPSC1                         *
039800870324     C                   MOVE      VIDSC2        CLPSC2                         *
039900870324     C                   MOVE      VIDLIS        CLPLIS                         *
040000870324     C                   MOVE      VIDQCL        CLPQCL                         *
040100990312     C                   MOVE      vidval        CLPVAL                         *
040200870324     C                   MOVE      VIDINF        CLPINF                         *
040300891211     C                   MOVE      VIDIMB        CLPIMB
040400891211     C                   MOVE      VIDSPE        CLPSPE
040500870324     C                   MOVE      VIDRES        CLPRES
040600870324     C                   MOVE      VIDPCL        CLPPCL
040700870401     C                   MOVEL     VIDCDF        CLPCDF
040800870324     C   15              WRITE     ANCLP1                                       *
040900870324     C   15              Z-ADD     NREL1         NRRSAV                         *
041000870324     C  N15              UPDATE    ANCLP1                                       *
041100870324     C                   SETOFF                                       10        *
041200870324     CSR                 ENDSR                                                  *
041300990114     C*----------------------------------------------------*
041400990114     C* Reperimento dati società
041500990114     C*----------------------------------------------------*
041600990114     C     REPSOC        BEGSR
041700990114     C*
041800990114     C                   CALLB     'XSOC'
041900990114     C                   PARM                    TIPXSC            6
042000990114     C                   PARM                    SOCXSC            3
042100990114     C                   PARM                    CDSXSC            9 0
042200990114     C                   PARM                    MODXSC            3
042300990114     C                   PARM      *blanks       RTNXSC            1
042400990114     C                   PARM                    XSOCDS
042500990114     C                   PARM                    KPJBA
042600990114     C*
042700990114     C                   ENDSR
042800870324     O*** RILASCIO RECORDS ------------------------------***
042900870324     OANCLP1    E            RILFOP
