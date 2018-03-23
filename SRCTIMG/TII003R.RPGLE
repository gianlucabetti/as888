000100990519     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200990519     H*PARMS ACTGRP(QILE)
000300990115     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400000000     F* ANG200R  *-AN/38-*-------------------------------------------*
000500000000     F*------------------*                                           *
000600000000     F*       - MANUTENZIONE FORNITORI PARTICOLARITA'      -         *
000700000000     F*       ----------------------------------------------         *
000800000000     F*                                                   AZN.: G200 *
000900000000     F*         FILE : ANFOP01L - ANFOP00F - ANTAB00F                *
001000870401     F*                         - CNACO00F - ANG200D -               *
001100860521     F*                BFTAB01L (ADB)                                *
001200000000     F*                                                              *
001300990317     F*         PGM. : tii103R   - RICERCA RAG.SOC.USO INTERNO       *
001400990317     F*                TII114R   - RICERCA GRUPPO MERCEOLOGICO       *
001500990317     F*                TII115R   - RICERCA SETTORE MERCEOLOGICO      *
001600990317     F*                TII116R   - RICERCA LISTINI                   *
001700990317     F*                TII112R   - RICERCA CLASSI APPROVIGIONAMENTI  *
001800000000     F*                XPARCCC   - RICERCA PARAMETRI VARI            *
001900000000     F*                                                              *
002000000000     F*--------------------------------------------------------------*
002100990115     FtmFOP01L  UF A E           K DISK
002200000000     F                                     INFDS(INF1)
002300990115     F                                     RENAME(tmFOP000:ANFOP1)
002400990115     FtmFOP00F  UF   E             DISK
002500870401     FTABEL00F  IF   E           K DISK
002600860521     F*************************************************************
002700990115     FanFRN01l  IF   E           K DISK
002800990115     Fansog01l  IF   E           K DISK
002900990408     Fancdp01l  IF   E           K DISK
003000860521     F*************************************************************
003100000000     FANTAB00F  IF   E           K DISK
003200010725     FTNTBE01l  IF   E           k disk
003300990317     Ftii003D   CF   E             WORKSTN
003400000000     F*--------------------------------------------------------------*
003500860521     D INF1            DS           502
003600860521     D*-----INFORMATION DS X FILE ANFOP01L
003700860521     D  NREL1                397    400B 0
003800860521     D****************************************************************
003900870401RIC  D PAGAM           DS
004000870401RIC  D*---- KEY PAGAMENTI PER TABEL00F
004100870401RIC  D  VIDCPS                 1      3
004200870401RIC  D  LINGUA                 4      4
004300870401RIC  D  BLANCO                 5      8
004400870401RIC  D  KEYTBL                 1      8
004500860521     D                 DS
004600860521     D*----- DS X CREAZIONE COD.FORNITORE NUOVO
004700860521     D  CAS                    1      7
004800000000     D                                     DIM(7)                               SCHIERA CASUALITA
004900860521     D  CASUAL                 1      7  0
005000860521     D                 DS
005100860521     D*----- DS X CREAZIONE COD.FORNITORE NUOVO
005200860521     D  NUO                    1      7
005300000000     D                                     DIM(7)                               SCHIERA COD.NUOVO
005400860521     D  NUOCOD                 1      7
005500860521     D                 DS
005600860521     D*---- COMMENTI E INFORMAZIONI FORNITORE
005700860521     D  VIDIN1                 1     50
005800860521     D  VIDIN2                51    100
005900860521     D  VIDIN3               101    150
006000860521     D  VIDIN4               151    200
006100860521     D  VIDINF                 1    200
006200000000     D KPJBA         E DS
006300000000     D*---- PASSAGGIO PARAMETRI
006400860521     D  MSGERR               453    502
006500990115     D soc001        E DS                  EXTNAME(xsoc001ds)
006600990115     D xsocds          DS          1000
006700990312     D* Ricerca divise
006800990312     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
006900010725    >D DGED          E DS
007000990519     D*-------------
007100990519     D*  Prototipazione delle procedure
007200990519     D/COPY *LIBL/SRCCPY,PJX002PR
007300000000     C*----------------------------------------------------*
007400000000     C     *ENTRY        PLIST                                                  *
007500000000     C                   PARM                    KPJBA                          *
007600990115     C*---------- RICERCA DITTA :
007700990115     C                   MOVEL     'SOC001'      TIPXSC
007800990115     C                   MOVEL     *blank        SOCXSC
007900990115     C                   EXSR      REPSOC
008000990115     C     RTNXSC        IFNE      '1'
008100990115     C                   MOVEL     XSOCDS        SOC001
008200990115     C                   MOVEL     xscrgs        RSUT             20
008300990115     C                   MOVEL     xscrgs        desdit
008400990115     c                   movel     xscsoc        wsoc
008500990115     c                   end
008600990115     C*                                                    *
008700990408     c     kFRN          klist
008800990408     c                   kfld                    wsoc              3
008900990408     c                   kfld                    wksc              8
009000990408     c     kcdp          klist
009100990408     c                   kfld                    wsoc
009200990408     c                   kfld                    frncondpag
009300000000     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
009400000000     C     KEYANT        KLIST                                                  *
009500000000     C                   KFLD                    TABGRU                         *
009600000000     C                   KFLD                    TABCOD                         *
009700010725     C*--- Reperisco la divisa corrente ------------------------------*
009800010725     C     ktbe          KLIST
009900010725     C                   KFLD                    kcod
010000010725     C                   KFLD                    kke1
010100010725     C     *like         Define    tbecod        kcod
010200010725     C     *like         Define    tbeke1        kke1
010300010725     C                   move      'GED'         kcod
010400010725     C                   move      *blanks       kke1
010500010725     C                   movel     'MAGAZZINO'   kke1
010600010725     c                   clear                   dged
010700010725     C     ktbe          CHAIN     tntbe01l                           54
010800010725     C  n54              movel     tbeuni        dged
010900860521     C******************************************************
011000860521     C*--------- EMISSIONE PROMPT -------------------------*
011100000000     C                   WRITE     ANG2001                                      *
011200990118     C                   MOVE      '?       '    CODFOR                         *
011300000000     C                   MOVE      *BLANKS       DESFOR                         *
011400000000     C     INIZIO        TAG                                                    *
011500000000     C*--------------------                                *
011600000000     C   91                                                                     *
011700000000     COR 92              SETON                                            90    *
011800000000     C                   EXFMT     ANG2002                                      *
011900000000     C                   MOVEA     *ZEROS        *IN(1)                         *
012000990408     C   Kc              GOTO      FINE                                         *
012100990407     C   Kd              CALL      'ANA400R'
012200870415     C                   PARM                    KPJBA
012300870415     C** RICHIAMO GESTIONE PIANO DEI CONTI
012400990115     C   Kd              GOTO      INIZIO
012500000000     C     CODFOR        CABEQ     *BLANKS       INIZIO                   91    *
012600000000     C*---------- RICERCA ALFABETICA :                     *
012700990407     c                   if        codfor = '?      ' or codfor = '0000000?'
012800990407     C*    CODFOR        IFEQ      '?       '                                   *
012900870415     C                   MOVE      *BLANKS       CODFOR                         *
013000990317     C                   CALL      'TII103R'                                    *
013100000000     C                   PARM                    CODFOR                         *
013200000000     C                   PARM                    DESFOR                         *
013300000000     C                   GOTO      INIZIO                                       *
013400000000     C                   END                                                    *
013500000000     C**--- CONTROLLO PRESENZA FORNITORE SUI FILES ------***
013600990115     C     CODFOR        CHAIN     TMFOP01L                           10        *
013700000000     C   10              EXSR      RUVIAR                                       * FORN.NUOVO
013800000000     C  N10              EXSR      RUVIVE                                       * VAR. FORNIT.
013900000000     C  N10              Z-ADD     NREL1         NRRSAV            5 0          *
014000000000     C                   TESTN                   CODFOR               70        *
014100000000     C  N10*IN(70)       CABEQ     '0'           SOPRA                          *
014200000000     C   10*IN(70)       CABEQ     '0'           SOPRA1                         *
014300000000     C                   SETON                                        09        *
014400990115     C                   MOVE      CODFOR        WKSC                           *
014500000000     C*--- RILASCIO RECORD ANFOP01L                        *
014600000000     C  N10              EXCEPT    RILFOP                                       *
014700860521     C******************************************************
014800990312     C*--- RICERCA RECORD
014900990118RIC  C     Kfrn          CHAIN     anfrn01l                           15        *
015000990408RIC  C  n15frnsogg       CHAIN     ansog01l                           15        *
015100990408RIC  C  n15Kcdp          CHAIN     ancdp01l                           15        *
015200990115RIC  C  N15              MOVEl     sogdes        VIDRAG                         *
015300990118RIC  C  N15              MOVE      frnKCC        VIDCFO                         *
015400990118     C  N15              MOVE      frncondpag    VIDcps
015500990408     C  n15              MOVEl     cdpdescr      descps
015600990118     C   15              MOVE      *BLANKS       VIDCps
015700990407RIC  C   15              MOVEl     *blank        VIDRAG                         *
015800860521     C******************************************************
015900000000     C     *IN(15)       CABEQ     '1'           INIZIO                   92    *
016000000000     C   10              GOTO      SOPRA1                                       *
016100000000     C*-------------------------------                     *
016200000000     C     SOPRA         TAG                                                    *
016300000000     C                   EXSR      RUTCTL                                       * CONTR. DATI
016400860521     C*--- ERRORI SULLA PRIMA VIDEATA DI MANUTENZIONE -----*
016500860521     C     SOPRA1        TAG                                                    *
016600000000     C   99                                                                     *
016700000000     COR 98                                                                     *
016800000000     COR 97                                                                     *
016900000000     COR 96                                                                     *
017000000000     COR 95                                                                     *
017100000000     COR 94                                                                     *
017200000000     COR 93              SETON                                        90        *
017300860521     C   92              SETON                                        90        *
017400870911     C   88              SETON                                        90        *
017500990312     C   87              SETON                                        90        *
017600000000     C*--- EMISSIONE VIDEATA DI MANUTENZIONE :             *
017700000000     C                   EXFMT     ANG2003                                      *
017800000000     C                   MOVEA     *ZEROS        *IN(90)                        *
017900990408     C   Kl              MOVE      *BLANKS       DESFOR                         *
018000990408     C   Kl              GOTO      INIZIO                                       *
018100000000     C                   GOTO      SOPRA                                        *
018200000000     C*----------------------------------------------------*
018300000000     C     FINE          TAG                                                    *
018400000000     C                   SETON                                            LR    *
018500000000     C*--- CONTROLLO VALIDITA' CAMPI ----------------------*
018600000000     C***------------------------------------------------***
018700000000     CSR   RUTCTL        BEGSR                                                  *
018800860521     C*---------                                           * RAG.SOC.
018900860521     C   10VIDRSI        CABEQ     *BLANKS       SOPRA1                   92    *
019000860521     C  N10VIDRSI        IFEQ      *BLANKS                                      *
019100860521     C                   TESTN                   CODFOR               70        *
019200860521     C     *IN(70)       CABEQ     '0'           SOTRSI                         *
019300860521     C                   MOVEL     VIDRAG        VIDRSI                         *
019400860521     C     SOTRSI        TAG                                                    *
019500860521     C                   END                                                    *
019600000000     C*---------                                           * GRUP.MERC.
019700000000     C     VIDGPM        IFEQ      '? '                                         *
019800000000     C                   MOVE      *BLANKS       VIDGPM                         *
019900000000     C                   MOVE      *BLANKS       DESGPM                         *
020000990317     C                   CALL      'TII114R'                                    *
020100000000     C                   PARM                    VIDGPM                         *
020200000000     C                   PARM                    DESGPM                         *
020300000000     C     VIDGPM        COMP      '99'                                   93    *
020400000000     C                   GOTO      SOPRA1                                       *
020500000000     C                   ELSE                                                   *
020600000000     C                   MOVE      '159'         TABGRU                         *
020700000000     C                   MOVE      *ZEROS        TABCOD                         *
020800000000     C                   MOVE      VIDGPM        TABCOD                         *
020900000000     C     KEYANT        CHAIN     ANTAB00F                           93        *
021000000000     C   93              GOTO      SOPRA1                                       *
021100000000     C                   MOVEL     TABDES        DESGPM                         *
021200000000     C                   END                                                    *
021300000000     C*---------                                           * SETTORE
021400000000     C     VIDSTM        IFEQ      '? '                                         *
021500000000     C                   MOVE      *BLANKS       VIDSTM                         *
021600000000     C                   MOVE      *BLANKS       DESSTM                         *
021700990317     C                   CALL      'TII115R'                                    *
021800000000     C                   PARM                    VIDSTM                         *
021900000000     C                   PARM                    DESSTM                         *
022000000000     C     VIDSTM        COMP      '99'                                   94    *
022100000000     C                   GOTO      SOPRA1                                       *
022200000000     C                   ELSE                                                   *
022300000000     C                   MOVE      '160'         TABGRU                         *
022400000000     C                   MOVE      *ZEROS        TABCOD                         *
022500000000     C                   MOVE      VIDSTM        TABCOD                         *
022600000000     C     KEYANT        CHAIN     ANTAB00F                           94        *
022700000000     C   94              GOTO      SOPRA1                                       *
022800000000     C                   MOVEL     TABDES        DESSTM                         *
022900000000     C                   END                                                    *
023000860521     C*--------- LISTINO ----------------------------------* LISTINO
023100000000     C     VIDLIS        IFEQ      '?  '                                        *
023200000000     C                   MOVE      *BLANKS       VIDLIS                         *
023300000000     C                   MOVE      *BLANKS       DESCRI           75            *
023400990317     C                   CALL      'TII116R'                                    *
023500000000     C                   PARM                    VIDLIS                         *
023600000000     C                   PARM                    DESCRI                         *
023700000000     C                   MOVEL     DESCRI        DESLIS                         *
023800000000     C     VIDLIS        COMP      '999'                                  97    *
023900000000     C                   GOTO      SOPRA1                                       *
024000000000     C                   ELSE                                                   *
024100000000     C                   MOVE      '161'         TABGRU                         *
024200000000     C                   MOVE      *ZEROS        TABCOD                         *
024300000000     C                   MOVE      VIDLIS        TABCOD                         *
024400000000     C     KEYANT        CHAIN     ANTAB00F                           97        *
024500000000     C   97              GOTO      SOPRA1                                       *
024600000000     C                   MOVEL     TABDES        DESLIS                         *
024700000000     C                   END                                                    *
024800000000     C*---------                                           * CLAS.APPROV.
024900000000     C     VIDAPP        IFEQ      '? '                                         *
025000000000     C                   MOVE      *BLANKS       VIDAPP                         *
025100000000     C                   MOVE      *BLANKS       DESCRI                         *
025200990317     C                   CALL      'TII112R'                                    *
025300000000     C                   PARM                    VIDAPP                         *
025400000000     C                   PARM                    DESCRI                         *
025500000000     C                   MOVEL     DESCRI        DESAPP                         *
025600000000     C     VIDAPP        COMP      '99'                                   98    *
025700000000     C                   GOTO      SOPRA1                                       *
025800000000     C                   ELSE                                                   *
025900000000     C                   MOVE      '157'         TABGRU                         *
026000000000     C                   MOVE      *ZEROS        TABCOD                         *
026100000000     C                   MOVE      VIDAPP        TABCOD                         *
026200000000     C     KEYANT        CHAIN     ANTAB00F                           98        *
026300000000     C   98              GOTO      SOPRA1                                       *
026400000000     C                   MOVEL     TABDES        DESAPP                         *
026500000000     C                   END                                                    *
026600990312     C*----------RICERCA valuta  :                         *
026700990312     c                   if        vidval = '?   '
026800990312     C                   MOVEL     '01'          OPZ015
026900990312     C                   MOVEL     *blank        Div015
027000990312     C                   MOVEL     *blank        vidvald
027100990312     C                   MOVEL     A015DS        KPJBU
027200990312     C                   CALL      'ANA015R'
027300990312     C                   PARM                    KPJBA
027400990312     C                   MOVEL     KPJBU         A015DS
027500990312     C     Div015        IFNE      *BLANK
027600990312     C                   MOVEL     Div015        vidval
027700990312     C                   GOTO      sopra1                                       *
027800990312     C                   ENDIF
027900990312     C                   ENDIF
028000990312     C     vidval        CABEQ     *BLANKS       sopra1                   87    *
028100990519     C                   if        not *in87
028200990519     C                   clear                   Errore            3 0
028300990519     C                   move      vidval        divisa0202
028400990519     C                   clear                   Dtrif0202
028500990519     C                   eval      Errore = X0201RpDVS(Divisa0202:Dtrif0202:
028600990519     C                                      ANDVS00F: %size(ANDVS00F) :
028700990519     C                                      ANDVX00F: %size(ANDVX00F))
028800990519     c                   if        Errore = 1 or Errore = 3
028900990519     C                   seton                                        87
029000990519     C                   end
029100990519     C                   end
029200990519     c  n87              movel     dvsdesbrev    vidvald
029300990312     c   87              goto      sopra1
029400000000     C*---------                                           *
029500000000     C     SCRIVI        TAG                                                    *
029600990408     c   kf              do
029700990408     C                   EXSR      RUTFIL                                       *
029800990408     c                   goto      inizio
029900990408     c                   enddo
030000990408
030100000000     CSR                 ENDSR                                                  *
030200000000     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
030300000000     C**--- INSERIMENTO FORNITORE NUOVO -----------------***
030400000000     CSR   RUVIAR        BEGSR                                                  *
030500000000     C                   MOVE      *BLANKS       VIDRAG                         *
030600000000     C*--------------------                                *
030700000000     C                   MOVE      *BLANKS       VIDRSI                         *
030800000000     C                   MOVE      '? '          VIDGPM                         *
030900000000     C                   MOVE      *BLANKS       DESGPM                         *
031000000000     C                   MOVE      '? '          VIDSTM                         *
031100000000     C                   MOVE      *BLANKS       DESSTM                         *
031200000000     C                   MOVE      *BLANKS       DESCPS                         *
031300000000     C                   MOVE      *ZEROS        VIDSC1                         *
031400000000     C                   MOVE      *ZEROS        VIDSC2                         *
031500871118     C                   MOVE      '1  '         VIDLIS                         *
031600000000     C                   MOVE      *BLANKS       DESLIS                         *
031700000000     C                   MOVE      '? '          VIDAPP                         *
031800000000     C                   MOVE      *BLANKS       DESAPP                         *
031900000000     C                   MOVE      *BLANKS       VIDDCC                         *
032000000000     C                   MOVE      *BLANKS       VIDQFO                         *
032100990312     C                   MOVE      *blank        VIDVALd                        *
032200010725     c                   if        §gedcn <> *blank
032300010725     C                   MOVEl     §gedcn        VIDVAL                         *
032400010725     c                   else
032500010725     c                   move      xscdiv        vidval
032600010725     c                   end
032700010725     C                   MOVE      *BLANKS       VIDINF                         *
032800000000     CSR                 ENDSR                                                  *
032900000000     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
033000000000     C**--- AGGIORNAMENTO FORNITORE ESISTENTE : ---------***
033100000000     C***------------------------------------------------***
033200000000     CSR   RUVIVE        BEGSR                                                  *
033300860521     C                   MOVE      FOPCFO        VIDCFO                         *
033400000000     C                   MOVE      FOPRSI        VIDRSI                         *
033500000000     C                   MOVE      FOPGPM        VIDGPM                         *
033600000000     C                   MOVE      *BLANKS       DESGPM                         *
033700000000     C                   MOVE      FOPSTM        VIDSTM                         *
033800000000     C                   MOVE      *BLANKS       DESSTM                         *
033900000000     C                   MOVE      *BLANKS       DESCPS                         *
034000000000     C                   MOVE      FOPSC1        VIDSC1                         *
034100000000     C                   MOVE      FOPSC2        VIDSC2                         *
034200000000     C                   MOVE      FOPLIS        VIDLIS                         *
034300000000     C                   MOVE      *BLANKS       DESLIS                         *
034400000000     C                   MOVE      FOPAPP        VIDAPP                         *
034500000000     C                   MOVE      *BLANKS       DESAPP                         *
034600000000     C                   MOVE      FOPDCC        VIDDCC                         *
034700000000     C                   MOVE      FOPQFO        VIDQFO                         *
034800000000     C                   MOVE      FOPVAL        VIDVAL                         *
034900000000     C                   MOVE      FOPINF        VIDINF                         *
035000000000     CSR                 ENDSR                                                  *
035100000000     C**--- IMPOSTAZIONE CAMPI X SCRITTURA FILE ---------***
035200000000     C***------------------------------------------------***
035300000000     CSR   RUTFIL        BEGSR                                                  *
035400000000     C*--- FILE ANFOP00F :                                 *
035500990115     C     CODFOR        CHAIN     tmFOP01L                           15        *
035600990408     C                   MOVE      *BLANKS       FOPATT                         *
035700990118     C                   MOVE      xsccut        FOPUTE                         *
035800000000     C                   MOVE      VIDCFO        FOPCFO                         *
035900000000     C                   MOVE      CODFOR        FOPFOR                         *
036000000000     C                   MOVE      VIDRSI        FOPRSI                         *
036100000000     C                   MOVE      VIDGPM        FOPGPM                         *
036200000000     C                   MOVE      VIDSTM        FOPSTM                         *
036300000000     C                   MOVE      VIDSC1        FOPSC1                         *
036400000000     C                   MOVE      VIDSC2        FOPSC2                         *
036500000000     C                   MOVE      VIDLIS        FOPLIS                         *
036600000000     C                   MOVE      VIDAPP        FOPAPP                         *
036700000000     C                   MOVE      VIDDCC        FOPDCC                         *
036800871030     C                   MOVE      VIDQFO        FOPQFO                         *
036900000000     C                   MOVE      VIDVAL        FOPVAL                         *
037000000000     C                   MOVE      VIDINF        FOPINF                         *
037100000000     C   15              WRITE     ANFOP1                                       *
037200000000     C   15              Z-ADD     NREL1         NRRSAV                         *
037300000000     C  N15              UPDATE    ANFOP1                                       *
037400000000     C                   SETOFF                                       10        *
037500000000     CSR                 ENDSR                                                  *
037600990118     C*----------------------------------------------------*
037700990118     C* Reperimento dati società
037800990118     C*----------------------------------------------------*
037900990118     C     REPSOC        BEGSR
038000990118     C*
038100990118     C                   CALLB     'XSOC'
038200990118     C                   PARM                    TIPXSC            6
038300990118     C                   PARM                    SOCXSC            3
038400990118     C                   PARM                    CDSXSC            9 0
038500990118     C                   PARM                    MODXSC            3
038600990118     C                   PARM      *blanks       RTNXSC            1
038700990118     C                   PARM                    XSOCDS
038800990118     C                   PARM                    KPJBA
038900990118     C*
039000990118     C                   ENDSR
039100990408     O*** RILASCIO RECORDS ------------------------------***
039200990408     OANFOP1    E            RILFOP
039300990118      *----
