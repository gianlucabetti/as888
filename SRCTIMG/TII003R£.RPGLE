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
003200990317     Ftii003D   CF   E             WORKSTN
003300000000     F*--------------------------------------------------------------*
003400860521     D INF1            DS           502
003500860521     D*-----INFORMATION DS X FILE ANFOP01L
003600860521     D  NREL1                397    400B 0
003700860521     D****************************************************************
003800870401RIC  D PAGAM           DS
003900870401RIC  D*---- KEY PAGAMENTI PER TABEL00F
004000870401RIC  D  VIDCPS                 1      3
004100870401RIC  D  LINGUA                 4      4
004200870401RIC  D  BLANCO                 5      8
004300870401RIC  D  KEYTBL                 1      8
004400860521     D                 DS
004500860521     D*----- DS X CREAZIONE COD.FORNITORE NUOVO
004600860521     D  CAS                    1      7
004700000000     D                                     DIM(7)                               SCHIERA CASUALITA
004800860521     D  CASUAL                 1      7  0
004900860521     D                 DS
005000860521     D*----- DS X CREAZIONE COD.FORNITORE NUOVO
005100860521     D  NUO                    1      7
005200000000     D                                     DIM(7)                               SCHIERA COD.NUOVO
005300860521     D  NUOCOD                 1      7
005400860521     D                 DS
005500860521     D*---- COMMENTI E INFORMAZIONI FORNITORE
005600860521     D  VIDIN1                 1     50
005700860521     D  VIDIN2                51    100
005800860521     D  VIDIN3               101    150
005900860521     D  VIDIN4               151    200
006000860521     D  VIDINF                 1    200
006100000000     D KPJBA         E DS
006200000000     D*---- PASSAGGIO PARAMETRI
006300860521     D  MSGERR               453    502
006400990115     D soc001        E DS                  EXTNAME(xsoc001ds)
006500990115     D xsocds          DS          1000
006600990312     D* Ricerca divise
006700990312     D A015DS        E DS                  EXTNAME(ANA015DS) INZ
006800990519     D*-------------
006900990519     D*  Prototipazione delle procedure
007000990519     D/COPY *LIBL/SRCCPY,PJX002PR
007100000000     C*----------------------------------------------------*
007200000000     C     *ENTRY        PLIST                                                  *
007300000000     C                   PARM                    KPJBA                          *
007400990115     C*---------- RICERCA DITTA :
007500990115     C                   MOVEL     'SOC001'      TIPXSC
007600990115     C                   MOVEL     *blank        SOCXSC
007700990115     C                   EXSR      REPSOC
007800990115     C     RTNXSC        IFNE      '1'
007900990115     C                   MOVEL     XSOCDS        SOC001
008000990115     C                   MOVEL     xscrgs        RSUT             20
008100990115     C                   MOVEL     xscrgs        desdit
008200990115     c                   movel     xscsoc        wsoc
008300990115     c                   end
008400990115     C*                                                    *
008500990408     c     kFRN          klist
008600990408     c                   kfld                    wsoc              3
008700990408     c                   kfld                    wksc              8
008800990408     c     kcdp          klist
008900990408     c                   kfld                    wsoc
009000990408     c                   kfld                    frncondpag
009100000000     C*-- KEY PER RICERCA TABELLE SU ANTAB00F :            *
009200000000     C     KEYANT        KLIST                                                  *
009300000000     C                   KFLD                    TABGRU                         *
009400000000     C                   KFLD                    TABCOD                         *
009500860521     C******************************************************
009600860521     C*--------- EMISSIONE PROMPT -------------------------*
009700000000     C                   WRITE     ANG2001                                      *
009800990118     C                   MOVE      '?       '    CODFOR                         *
009900000000     C                   MOVE      *BLANKS       DESFOR                         *
010000000000     C     INIZIO        TAG                                                    *
010100000000     C*--------------------                                *
010200000000     C   91                                                                     *
010300000000     COR 92              SETON                                            90    *
010400000000     C                   EXFMT     ANG2002                                      *
010500000000     C                   MOVEA     *ZEROS        *IN(1)                         *
010600990408     C   Kc              GOTO      FINE                                         *
010700990407     C   Kd              CALL      'ANA400R'
010800870415     C                   PARM                    KPJBA
010900870415     C** RICHIAMO GESTIONE PIANO DEI CONTI
011000990115     C   Kd              GOTO      INIZIO
011100000000     C     CODFOR        CABEQ     *BLANKS       INIZIO                   91    *
011200000000     C*---------- RICERCA ALFABETICA :                     *
011300990407     c                   if        codfor = '?      ' or codfor = '0000000?'
011400990407     C*    CODFOR        IFEQ      '?       '                                   *
011500870415     C                   MOVE      *BLANKS       CODFOR                         *
011600990317     C                   CALL      'TII103R'                                    *
011700000000     C                   PARM                    CODFOR                         *
011800000000     C                   PARM                    DESFOR                         *
011900000000     C                   GOTO      INIZIO                                       *
012000000000     C                   END                                                    *
012100000000     C**--- CONTROLLO PRESENZA FORNITORE SUI FILES ------***
012200990115     C     CODFOR        CHAIN     TMFOP01L                           10        *
012300000000     C   10              EXSR      RUVIAR                                       * FORN.NUOVO
012400000000     C  N10              EXSR      RUVIVE                                       * VAR. FORNIT.
012500000000     C  N10              Z-ADD     NREL1         NRRSAV            5 0          *
012600000000     C                   TESTN                   CODFOR               70        *
012700000000     C  N10*IN(70)       CABEQ     '0'           SOPRA                          *
012800000000     C   10*IN(70)       CABEQ     '0'           SOPRA1                         *
012900000000     C                   SETON                                        09        *
013000990115     C                   MOVE      CODFOR        WKSC                           *
013100000000     C*--- RILASCIO RECORD ANFOP01L                        *
013200000000     C  N10              EXCEPT    RILFOP                                       *
013300860521     C******************************************************
013400990312     C*--- RICERCA RECORD
013500990118RIC  C     Kfrn          CHAIN     anfrn01l                           15        *
013600990408RIC  C  n15frnsogg       CHAIN     ansog01l                           15        *
013700990408RIC  C  n15Kcdp          CHAIN     ancdp01l                           15        *
013800990115RIC  C  N15              MOVEl     sogdes        VIDRAG                         *
013900990118RIC  C  N15              MOVE      frnKCC        VIDCFO                         *
014000990118     C  N15              MOVE      frncondpag    VIDcps
014100990408     C  n15              MOVEl     cdpdescr      descps
014200990118     C   15              MOVE      *BLANKS       VIDCps
014300990407RIC  C   15              MOVEl     *blank        VIDRAG                         *
014400860521     C******************************************************
014500000000     C     *IN(15)       CABEQ     '1'           INIZIO                   92    *
014600000000     C   10              GOTO      SOPRA1                                       *
014700000000     C*-------------------------------                     *
014800000000     C     SOPRA         TAG                                                    *
014900000000     C                   EXSR      RUTCTL                                       * CONTR. DATI
015000860521     C*--- ERRORI SULLA PRIMA VIDEATA DI MANUTENZIONE -----*
015100860521     C     SOPRA1        TAG                                                    *
015200000000     C   99                                                                     *
015300000000     COR 98                                                                     *
015400000000     COR 97                                                                     *
015500000000     COR 96                                                                     *
015600000000     COR 95                                                                     *
015700000000     COR 94                                                                     *
015800000000     COR 93              SETON                                        90        *
015900860521     C   92              SETON                                        90        *
016000870911     C   88              SETON                                        90        *
016100990312     C   87              SETON                                        90        *
016200000000     C*--- EMISSIONE VIDEATA DI MANUTENZIONE :             *
016300000000     C                   EXFMT     ANG2003                                      *
016400000000     C                   MOVEA     *ZEROS        *IN(90)                        *
016500990408     C   Kl              MOVE      *BLANKS       DESFOR                         *
016600990408     C   Kl              GOTO      INIZIO                                       *
016700000000     C                   GOTO      SOPRA                                        *
016800000000     C*----------------------------------------------------*
016900000000     C     FINE          TAG                                                    *
017000000000     C                   SETON                                            LR    *
017100000000     C*--- CONTROLLO VALIDITA' CAMPI ----------------------*
017200000000     C***------------------------------------------------***
017300000000     CSR   RUTCTL        BEGSR                                                  *
017400860521     C*---------                                           * RAG.SOC.
017500860521     C   10VIDRSI        CABEQ     *BLANKS       SOPRA1                   92    *
017600860521     C  N10VIDRSI        IFEQ      *BLANKS                                      *
017700860521     C                   TESTN                   CODFOR               70        *
017800860521     C     *IN(70)       CABEQ     '0'           SOTRSI                         *
017900860521     C                   MOVEL     VIDRAG        VIDRSI                         *
018000860521     C     SOTRSI        TAG                                                    *
018100860521     C                   END                                                    *
018200000000     C*---------                                           * GRUP.MERC.
018300000000     C     VIDGPM        IFEQ      '? '                                         *
018400000000     C                   MOVE      *BLANKS       VIDGPM                         *
018500000000     C                   MOVE      *BLANKS       DESGPM                         *
018600990317     C                   CALL      'TII114R'                                    *
018700000000     C                   PARM                    VIDGPM                         *
018800000000     C                   PARM                    DESGPM                         *
018900000000     C     VIDGPM        COMP      '99'                                   93    *
019000000000     C                   GOTO      SOPRA1                                       *
019100000000     C                   ELSE                                                   *
019200000000     C                   MOVE      '159'         TABGRU                         *
019300000000     C                   MOVE      *ZEROS        TABCOD                         *
019400000000     C                   MOVE      VIDGPM        TABCOD                         *
019500000000     C     KEYANT        CHAIN     ANTAB00F                           93        *
019600000000     C   93              GOTO      SOPRA1                                       *
019700000000     C                   MOVEL     TABDES        DESGPM                         *
019800000000     C                   END                                                    *
019900000000     C*---------                                           * SETTORE
020000000000     C     VIDSTM        IFEQ      '? '                                         *
020100000000     C                   MOVE      *BLANKS       VIDSTM                         *
020200000000     C                   MOVE      *BLANKS       DESSTM                         *
020300990317     C                   CALL      'TII115R'                                    *
020400000000     C                   PARM                    VIDSTM                         *
020500000000     C                   PARM                    DESSTM                         *
020600000000     C     VIDSTM        COMP      '99'                                   94    *
020700000000     C                   GOTO      SOPRA1                                       *
020800000000     C                   ELSE                                                   *
020900000000     C                   MOVE      '160'         TABGRU                         *
021000000000     C                   MOVE      *ZEROS        TABCOD                         *
021100000000     C                   MOVE      VIDSTM        TABCOD                         *
021200000000     C     KEYANT        CHAIN     ANTAB00F                           94        *
021300000000     C   94              GOTO      SOPRA1                                       *
021400000000     C                   MOVEL     TABDES        DESSTM                         *
021500000000     C                   END                                                    *
021600860521     C*--------- LISTINO ----------------------------------* LISTINO
021700000000     C     VIDLIS        IFEQ      '?  '                                        *
021800000000     C                   MOVE      *BLANKS       VIDLIS                         *
021900000000     C                   MOVE      *BLANKS       DESCRI           75            *
022000990317     C                   CALL      'TII116R'                                    *
022100000000     C                   PARM                    VIDLIS                         *
022200000000     C                   PARM                    DESCRI                         *
022300000000     C                   MOVEL     DESCRI        DESLIS                         *
022400000000     C     VIDLIS        COMP      '999'                                  97    *
022500000000     C                   GOTO      SOPRA1                                       *
022600000000     C                   ELSE                                                   *
022700000000     C                   MOVE      '161'         TABGRU                         *
022800000000     C                   MOVE      *ZEROS        TABCOD                         *
022900000000     C                   MOVE      VIDLIS        TABCOD                         *
023000000000     C     KEYANT        CHAIN     ANTAB00F                           97        *
023100000000     C   97              GOTO      SOPRA1                                       *
023200000000     C                   MOVEL     TABDES        DESLIS                         *
023300000000     C                   END                                                    *
023400000000     C*---------                                           * CLAS.APPROV.
023500000000     C     VIDAPP        IFEQ      '? '                                         *
023600000000     C                   MOVE      *BLANKS       VIDAPP                         *
023700000000     C                   MOVE      *BLANKS       DESCRI                         *
023800990317     C                   CALL      'TII112R'                                    *
023900000000     C                   PARM                    VIDAPP                         *
024000000000     C                   PARM                    DESCRI                         *
024100000000     C                   MOVEL     DESCRI        DESAPP                         *
024200000000     C     VIDAPP        COMP      '99'                                   98    *
024300000000     C                   GOTO      SOPRA1                                       *
024400000000     C                   ELSE                                                   *
024500000000     C                   MOVE      '157'         TABGRU                         *
024600000000     C                   MOVE      *ZEROS        TABCOD                         *
024700000000     C                   MOVE      VIDAPP        TABCOD                         *
024800000000     C     KEYANT        CHAIN     ANTAB00F                           98        *
024900000000     C   98              GOTO      SOPRA1                                       *
025000000000     C                   MOVEL     TABDES        DESAPP                         *
025100000000     C                   END                                                    *
025200990312     C*----------RICERCA valuta  :                         *
025300990312     c                   if        vidval = '?   '
025400990312     C                   MOVEL     '01'          OPZ015
025500990312     C                   MOVEL     *blank        Div015
025600990312     C                   MOVEL     *blank        vidvald
025700990312     C                   MOVEL     A015DS        KPJBU
025800990312     C                   CALL      'ANA015R'
025900990312     C                   PARM                    KPJBA
026000990312     C                   MOVEL     KPJBU         A015DS
026100990312     C     Div015        IFNE      *BLANK
026200990312     C                   MOVEL     Div015        vidval
026300990312     C                   GOTO      sopra1                                       *
026400990312     C                   ENDIF
026500990312     C                   ENDIF
026600990312     C     vidval        CABEQ     *BLANKS       sopra1                   87    *
026700990519     C                   if        not *in87
026800990519     C                   clear                   Errore            3 0
026900990519     C                   move      vidval        divisa0202
027000990519     C                   clear                   Dtrif0202
027100990519     C                   eval      Errore = X0201RpDVS(Divisa0202:Dtrif0202:
027200990519     C                                      ANDVS00F: %size(ANDVS00F) :
027300990519     C                                      ANDVX00F: %size(ANDVX00F))
027400990519     c                   if        Errore = 1 or Errore = 3
027500990519     C                   seton                                        87
027600990519     C                   end
027700990519     C                   end
027800990519     c  n87              movel     dvsdesbrev    vidvald
027900990312     c   87              goto      sopra1
028000000000     C*---------                                           *
028100000000     C     SCRIVI        TAG                                                    *
028200990408     c   kf              do
028300990408     C                   EXSR      RUTFIL                                       *
028400990408     c                   goto      inizio
028500990408     c                   enddo
028600990408
028700000000     CSR                 ENDSR                                                  *
028800000000     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
028900000000     C**--- INSERIMENTO FORNITORE NUOVO -----------------***
029000000000     CSR   RUVIAR        BEGSR                                                  *
029100000000     C                   MOVE      *BLANKS       VIDRAG                         *
029200000000     C*--------------------                                *
029300000000     C                   MOVE      *BLANKS       VIDRSI                         *
029400000000     C                   MOVE      '? '          VIDGPM                         *
029500000000     C                   MOVE      *BLANKS       DESGPM                         *
029600000000     C                   MOVE      '? '          VIDSTM                         *
029700000000     C                   MOVE      *BLANKS       DESSTM                         *
029800000000     C                   MOVE      *BLANKS       DESCPS                         *
029900000000     C                   MOVE      *ZEROS        VIDSC1                         *
030000000000     C                   MOVE      *ZEROS        VIDSC2                         *
030100871118     C                   MOVE      '1  '         VIDLIS                         *
030200000000     C                   MOVE      *BLANKS       DESLIS                         *
030300000000     C                   MOVE      '? '          VIDAPP                         *
030400000000     C                   MOVE      *BLANKS       DESAPP                         *
030500000000     C                   MOVE      *BLANKS       VIDDCC                         *
030600000000     C                   MOVE      *BLANKS       VIDQFO                         *
030700990312     C                   MOVE      *blank        VIDVALd                        *
030800990312     C                   MOVE      xscdiv        VIDVAL                         *
030900000000     C                   MOVE      *BLANKS       VIDINF                         *
031000000000     CSR                 ENDSR                                                  *
031100000000     C**--- IMPOSTAZIONE CAMPI VIDEO X : ----------------***
031200000000     C**--- AGGIORNAMENTO FORNITORE ESISTENTE : ---------***
031300000000     C***------------------------------------------------***
031400000000     CSR   RUVIVE        BEGSR                                                  *
031500860521     C                   MOVE      FOPCFO        VIDCFO                         *
031600000000     C                   MOVE      FOPRSI        VIDRSI                         *
031700000000     C                   MOVE      FOPGPM        VIDGPM                         *
031800000000     C                   MOVE      *BLANKS       DESGPM                         *
031900000000     C                   MOVE      FOPSTM        VIDSTM                         *
032000000000     C                   MOVE      *BLANKS       DESSTM                         *
032100000000     C                   MOVE      *BLANKS       DESCPS                         *
032200000000     C                   MOVE      FOPSC1        VIDSC1                         *
032300000000     C                   MOVE      FOPSC2        VIDSC2                         *
032400000000     C                   MOVE      FOPLIS        VIDLIS                         *
032500000000     C                   MOVE      *BLANKS       DESLIS                         *
032600000000     C                   MOVE      FOPAPP        VIDAPP                         *
032700000000     C                   MOVE      *BLANKS       DESAPP                         *
032800000000     C                   MOVE      FOPDCC        VIDDCC                         *
032900000000     C                   MOVE      FOPQFO        VIDQFO                         *
033000000000     C                   MOVE      FOPVAL        VIDVAL                         *
033100000000     C                   MOVE      FOPINF        VIDINF                         *
033200000000     CSR                 ENDSR                                                  *
033300000000     C**--- IMPOSTAZIONE CAMPI X SCRITTURA FILE ---------***
033400000000     C***------------------------------------------------***
033500000000     CSR   RUTFIL        BEGSR                                                  *
033600000000     C*--- FILE ANFOP00F :                                 *
033700990115     C     CODFOR        CHAIN     tmFOP01L                           15        *
033800990408     C                   MOVE      *BLANKS       FOPATT                         *
033900990118     C                   MOVE      xsccut        FOPUTE                         *
034000000000     C                   MOVE      VIDCFO        FOPCFO                         *
034100000000     C                   MOVE      CODFOR        FOPFOR                         *
034200000000     C                   MOVE      VIDRSI        FOPRSI                         *
034300000000     C                   MOVE      VIDGPM        FOPGPM                         *
034400000000     C                   MOVE      VIDSTM        FOPSTM                         *
034500000000     C                   MOVE      VIDSC1        FOPSC1                         *
034600000000     C                   MOVE      VIDSC2        FOPSC2                         *
034700000000     C                   MOVE      VIDLIS        FOPLIS                         *
034800000000     C                   MOVE      VIDAPP        FOPAPP                         *
034900000000     C                   MOVE      VIDDCC        FOPDCC                         *
035000871030     C                   MOVE      VIDQFO        FOPQFO                         *
035100000000     C                   MOVE      VIDVAL        FOPVAL                         *
035200000000     C                   MOVE      VIDINF        FOPINF                         *
035300000000     C   15              WRITE     ANFOP1                                       *
035400000000     C   15              Z-ADD     NREL1         NRRSAV                         *
035500000000     C  N15              UPDATE    ANFOP1                                       *
035600000000     C                   SETOFF                                       10        *
035700000000     CSR                 ENDSR                                                  *
035800990118     C*----------------------------------------------------*
035900990118     C* Reperimento dati società
036000990118     C*----------------------------------------------------*
036100990118     C     REPSOC        BEGSR
036200990118     C*
036300990118     C                   CALLB     'XSOC'
036400990118     C                   PARM                    TIPXSC            6
036500990118     C                   PARM                    SOCXSC            3
036600990118     C                   PARM                    CDSXSC            9 0
036700990118     C                   PARM                    MODXSC            3
036800990118     C                   PARM      *blanks       RTNXSC            1
036900990118     C                   PARM                    XSOCDS
037000990118     C                   PARM                    KPJBA
037100990118     C*
037200990118     C                   ENDSR
037300990408     O*** RILASCIO RECORDS ------------------------------***
037400990408     OANFOP1    E            RILFOP
037500990118      *----
