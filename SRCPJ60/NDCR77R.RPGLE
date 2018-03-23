000100990506     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200990506     H*PARMS BNDDIR(PJXBND PJCBND)
000300990506     H*PARMS CVTOPT(*NONE)
000400990506     H DECEDIT('0,') DATEDIT(*DMY.)
000500990506      *
000600990506     >*            Lancio stampa modello 770 SC/SE
000700990506      *           ---------------------------------
000800990506      *
000900990506      *
001000990506      * DOCUMENTI MICROANALISI :
001100990506      *
001200990506      * NOTE TECNICHE :
001300990506      *
001400990506      *
001500990506      * PERSONALIZZAZIONI
001600990506      *
001700990506      *
001800990506      *
001900990506      *               MAPPA INDICATORI
002000990506      *
002100990506      *  21           GENERICO OPERAZIONI I/O
002200990506      *  22           GENERICO ERRORE OPERAZIONI I/O
002300990506      *  30           SFLDSP
002400990506      * N31           SFLCLR
002500990506      *  31           SFLDSPCTL
002600990506      *  32           SFLNXTCHG
002700990506      *  33           SFLEND
002800990506      *  39           OF PRTF
002900990506      *  40 <---> 49  DSPATR ERRORI SU SFL
003000990506      *  50 <---> 89  ERROSI SU VIDEO
003100990506      *  elenco errori speciali:
003200990506      *  90           REGISTRAZIONE GIA' ESISTENTE PER IMMISSIONE O COPIA
003300990506      *  91           OPZIONE NON VALIDA
003400990506      *  92           IMPOSSIBILE RIPOSIZIONAMENTO CON OPZIONI PRESENTI SU LISTA
003500990506      *  93           LA FUNZIONE RICHIAMATA SI E' CHIUSA IN ERRORE
003600990506      *  95           L'OPERAZIONE DI SCRITTURA NON E' ANDATA A BUON FINE
003700990506      *  94           UTENTE NON AUTORIZZATO ALLA FUNZIONE NEL MODO DESIDERATO
003800990506      *  96           ESISTONO ERRORI IN ALTRI SCHERMI
003900990506      *  97           TASTO   NON ABILITATO
004000990506      *  98           RICERCA NON ABIL. NELLA POSIZ.
004100990506      *  99           INDIC. GENERALE DI ERRORE
004200990506     F*----------------------------------------------------*
004300990506     FNDCR77D   CF   E             WORKSTN
004400990506     F*-
004500990506     F*----------------------------------------------------*
004600990506     D KPJBA         E DS
004700990506     D*------------
004800990506     D ndcr77ds      E DS
004900990506     D*------------
005000990506     D* Dati di ambiente ottenuti da XSOC
005100990506     D SOC001        E DS                  EXTNAME(XSOC001DS)
005200990506     D*-------------
005300990506     D* DS Interna per dati di output di XSOC
005400990506     D XSOCDS          DS          1000
005500990506     I*----------------------------------------------------*
005600990506     D* Reperimento nome PGM
005700990506     D STATUS         SDS           333
005800990506     D  DSPGM            *PROC
005900990506     D  PARMS            *PARMS
006000990506     I*----------------------------------------------------*
006100990506     C*
006200990506     C                   Exsr      Inzvar                                       *
006300990506     C*
006400990506     C     Inizio        Tag                                                    *
006500990506     C                   Z-Add     0             D1Anno                         *
006700990506     C*
006800990506     C                   Write     CR77T1                                       *
006900990506     C     VIDEO         TAG                                                    *
007000990506     C                   Exfmt     CR77D1                                       *
007100990506     C*                                                    *
007200990506     C   KC              GOTO      FINE                                         *
007300990507     C   KL              GOTO      FINE                                         *
007400990506     C*
007500990506     C                   setoff                                       99
007600990506     C     D1Anno        IfEq      0
007700990506     C                   seton                                        5199
007800990506     C                   EndIf
008300990506     C*
008400990507     C   99              GOTO      VIDEO
008500990506     C*
008600990506     C*
008700990506     C*
008800990507     C                   IF        *INKF         = *ON   OR
008900990507     C                             *INKV         = *ON
009000990506     C                   Clear                   NdCR77DS                       *
009100990506     C                   MOVEL     XscSoc        SOCIETAR77                     *
009200990506     C                   Z-Add     D1Anno        ANNOR77
009301020415     C                   MOVE      'AU'          QUAR77
009400990506     C                   MOVEL     NdCR77DS      KPJBU                          *
009500990507     C                   IF        KNMUS     <> *ALL'1'
009600990507     C                   IF        *InKV     = '1'
009700990507     C                   CALL      'BCH09'
009800990507     C                   PARM                    KPJBA
009900990507     C                   EndIf
010000990507     C                   MOVE      'CR78'        KCOAZ
010100990507     C                   CALL      'BCH10'
010200990507     C                   PARM                    KPJBA
010300990507     C                   ELSE
010400990507     C                   CALL      'NDCR78C'
010500990507     C                   PARM                    KPJBA
010600990507     C                   EndIf
010700990507     C                   ELSe
010800990507     C                   GOTO      VIDEO
010900990507     C                   EndIf
011000990506     C*
011100990506     C     FINE          TAG                                                    *
011200990506     C                   Exsr      EndPgm                                       *
011300990506     C************************************************************
011400990506     C* FINE PROGRAMMA
011500990506     C************************************************************
011600990506     C     ENDPGM        BEGSR
011700990506     C*
011800990506     C                   SETON                                            LR
011900990506     C*
012000990506     C                   ENDSR
012100990506     C/EJECT
012200990506     C************************************************************
012300990506     C* OPERAZIONI INIZIALI
012400990506     C************************************************************
012500990506     C     *INZSR        BEGSR
012600990506     C*
012700990506     C* Reperimento parametri
012800990506     C*
012900990506     C     *ENTRY        PLIST
013000990506     C                   PARM                    KPJBA
013100990506     C*
013200990506     C* Reperimento dati società
013300990506     C*
013400990506     C                   MOVEL     'SOC001'      TIPXSC
013500990506     C                   MOVEL     *BLANK        SOCXSC
013600990506     C                   EXSR      REPSOC
013700990506     C*
013800990506     C* Controlli dati società
013900990506     C*
014000990506     C     RTNXSC        IFNE      '1'
014100990506     C                   MOVEL     XSOCDS        SOC001
014200990506     C                   ELSE
014300990506     C                   EXSR      ENDPGM
014400990506     C                   ENDIF
014500990506     C*
014600990506     C                   ENDSR
014700990506     C/EJECT
014800990506     C************************************************************
014900990506     C* INIZIALIZZAZIONE VARIABILI
015000990506     C************************************************************
015100990506     C     INZVAR        BEGSR
015200990506     C*
015300990506     C* Valorizzazione campi univoci testate
015400990506     C*
015500990506     C                   CLEAR                   Cr77T1
015600990506     C                   MOVEL     KNSIF         NOMSIF
015700990506     C                   MOVEL     XSCDSI        NOMDIT
015800990506     C                   MOVEL     DSPGM         NOMPGM
015900990506     C*
016000990506     C                   ENDSR
016100990506     C/EJECT
016200990506     C************************************************************
016300990506     C* REPERIMENTO DATI SOCIETA'
016400990506     C************************************************************
016500990506     C     REPSOC        BEGSR
016600990506     C*
016700990506     C                   CALLB     'XSOC'
016800990506     C                   PARM                    TIPXSC            6
016900990506     C                   PARM                    SOCXSC            3
017000990506     C                   PARM                    CDSXSC            9 0
017100990506     C                   PARM                    MODXSC            3
017200990506     C                   PARM      *BLANKS       RTNXSC            1
017300990506     C                   PARM                    XSOCDS
017400990506     C                   PARM                    KPJBA
017500990506     C*
017600990506     C                   ENDSR
017700990506     C/EJECT
