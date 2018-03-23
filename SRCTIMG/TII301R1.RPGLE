000100990126     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990126     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*-OCO150R1 *-OF/38-*--------------------------------------*
000400000000     F*------------------*                                      *
000500880915     F*      -  INTERROGAZIONE PTF. PER DATA ORDINE
000600880915     F*      ------------PER CLIENTE --------------------- N150 *
000700000000     F*                                                         *
000800990126     F*         FILE : OCO150D  - tmsoc04L - tmamp01L -         *
000900000000     F*                                                         *
001000000000     F*         PGM  : RIC001R  - RICERCA DESCRIZIONE DITTA     *
001100000000     F*                                                         *
001200000000     F*---------------------------------------------------------*
001300990317     FTII301D   CF   E             WORKSTN
001400000000     F                                     IGNORE(SFL1)
001500000000     F                                     IGNORE(SFL1CT)
001600000000     F                                     IGNORE(OCO150PT)
001700000000     F                                     SFILE(SFL2:REC2)
001800990126     Ftmsoc04L  IF   E           K DISK
001900990126     Ftmamp01L  IF   E           K DISK
002000000000     FOCPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
002100000000     F                                     USROPN
002200000000     D*--------------------------------------------------------------*
002300000000     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
002400000000     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
002500000000     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
002600000000     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 4
002700000000     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 5
002800000000     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 6
002900000000     D*--------------------------------------------------------------*
003000000000     D KPJBA         E DS
003100000000     D*--- PARAMETRI X INTERROGAZIONE :
003200990126     D  FOR150               247    254
003300990126     D  DESFOR               255    302
003400990126     D  MAT150               407    418
003500990126     D  DESMAT               311    340
003600990126     D  DAL150               303    310  0
003700990126     D  AL150                345    352  0
003800990126     D  SNV150               353    353
003900990126     D  TOR150               354    355
004000990126     D  TR1150               356    356
004100990126     D  TR2150               357    357
004200990126     D  TR3150               358    358
004300990126     D  TR4150               359    359
004400990126     D  TR5150               360    360
004500990126     D  TR6150               361    361
004600990126     D  TR7150               362    362
004700990126     D  TR8150               363    363
004800990126     D  TR9150               364    364
004900990126     D  TRG                  356    364
005000990126     D                                     DIM(9)
005100990126     D  GRM150               365    366
005200990126     D  SEM150               367    368
005300990126     D  CLM150               369    371
005400990126     D  SCM150               372    376
005500990126     D  FA1150               377    378
005600990126     D  FA2150               379    380
005700990126     D  FA3150               381    382
005800990126     D  FA4150               383    384
005900990126     D  FA5150               385    386
006000990126     D  FAS                  377    386
006100990126     D                                     DIM(5)
006200990126     D  MAG150               387    393
006300990126     D  nOR150               394    398  0
006400990126     D  dOR150               399    406  0
006500990126     D  STA150               441    441
006600990126     D  RIC150               442    442
006700990126     D  UTE150               443    452
006800990126     D  MSGERR               453    502
006900990126     D soc001        E DS                  EXTNAME(xsoc001ds)
007000990126     D xsocds          DS          1000
007100990126     d dataiso         s               d   datfmt(*iso)
007200990126     d dataeur         s               d   datfmt(*eur)
007300000000     C*----------------------------------------------------*
007400000000     C     *ENTRY        PLIST                                                  *
007500000000     C                   PARM                    KPJBA                          *
007600990126     C*---------- RICERCA DITTA :
007700990126     C                   MOVEL     'SOC001'      TIPXSC
007800990126     C                   MOVEL     *blank        SOCXSC
007900990126     C                   EXSR      REPSOC
008000990126     C     RTNXSC        IFNE      '1'
008100990126     C                   MOVEL     XSOCDS        SOC001
008200990126     C                   MOVEL     xscrgs        desdit
008300990126     C                   MOVEL     xscsoc        wsoc              3
008400990126     c                   else
008500990126     c                   seton                                        90
008600990126     c                   goto      fine
008700990126     c                   end
008800990126     c                   time                    como14           14 0
008900990126     c                   move      como14        woggi             8 0
009000990126     c                   move      woggi         dataeur
009100990126     c                   move      dataeur       dataiso
009200990126     c                   move      dataiso       woggig            8 0
009300990126     C*--- FILE ANAGRAFICO ARTICOLI "tmamp01L"             *
009400000000     C     KEYAMP        KLIST                                                  *
009500000000     C                   KFLD                    MOCMAT                         *
009600000000     C*--- IMPOSTAZIONI INIZIALI --------------------------*
009700000000     C     SNV150        COMP      'S'                                    03    *
009800000000     C                   Z-ADD     0             VALOR2           15 2    02    *
009900000000     C                   Z-ADD     0             TOTVAL                         *
010000000000     C                   MOVEL     DESFOR        DESKK2                         *
010100990126     c                   if        dal150 > 0
010200990126     C                   move      DAL150        DATAIso                        *
010300990126     C                   move      dataiso       dataeur                        *
010400990126     C                   move      DATAeur       DAL                            *
010500990126     c                   end
010600990126     c                   if        al150 > 0
010700990126     C                   move      AL150         DATAIso                        *
010800990126     C                   move      dataiso       dataeur                        *
010900990126     C                   move      DATAeur       AL                             *
011000990126     c                   end
011100000000     C*--- PULIZIA SUBFILE --------------------------------*
011200000000     C                   Z-ADD     0             REC2              5 0    70    *
011300000000     C                   Z-ADD     0             RECT              5 0          *
011400000000     C   70              WRITE     SFL2CT                                       *
011500000000     C                   SETOFF                                       7071      *
011600000000     C*--- POSIZIONAMENTO SU FILE ORDINI ------------------*
011700990126     C     FOR150        SETLL     tmsoc04L                                     *
011800000000     C     LEGGI         TAG                                                    *
011900990126     C     FOR150        READE     tmsoc04L                               99    *
012000000000     C   99
012100000000     CANN71              MOVE      'E'           RIC150                         *
012200000000     C   99
012300000000     CANN71              GOTO      FINE                                         *
012400000000     C   99
012500000000     CAN 03              Z-ADD     VALOR2        TOTVAL                         * SI VALORIZZ.
012600000000     C   99              Z-ADD     REC2          RECT                           *
012700000000     C   99              GOTO      EMISFL                                       *
012800000000     C*--- CONTROLLI CON SCELTE DA PROMPT -----------------*
012900000000     C     MOCDOR        CABLT     DAL150        LEGGI                          * DATA ORDINE
013000000000     C     MOCDOR        CABGT     AL150         LEGGI                          *
013100000000     C*-----                                               *
013200000000     C     GRM150        IFNE      '99'                                         * GRUPPO
013300000000     C     GRM150        CABNE     MOCGPM        LEGGI                          *
013400000000     C                   END                                                    *
013500000000     C*-----                                               *
013600000000     C     SEM150        IFNE      '99'                                         * SETTORE
013700000000     C     SEM150        CABNE     MOCSTM        LEGGI                          *
013800000000     C                   END                                                    *
013900000000     C*----------------------------------------------------*
014000000000     C     TOR150        IFNE      '99'                                         * TIPO ORDINE
014100000000     C     TOR150        CABNE     MOCTOR        LEGGI                          *
014200000000     C                   END                                                    *
014300000000     C*-----                                               *
014400000000     C     MOCTRG        LOOKUP    TRG                                    91    * TIPO RIGA
014500000000     C  N91              GOTO      LEGGI                                        *
014600000000     C*-----                                               *
014700000000     C     MOCFAS        LOOKUP    FAS                                    91    * FASE
014800000000     C  N91              GOTO      LEGGI                                        *
014900000000     C     MOCFAS        COMP      'BB'                                   93    *
015000000000     C  N93MOCFAS        COMP      'FA'                                   93    *
015100000000     C*-----                                               *
015200000000     C     MAG150        IFNE      '9999999'                                    *MAGAZZINO
015300000000     C     MAG150        CABNE     MOCMAG        LEGGI                          *
015400000000     C                   END                                                    *
015500000000     C*-----                                               *
015600990126     C                   Z-ADD     0             IMPMER                         *
015700000000     C     MOCTRG        COMP      '1'                                    91    *
015800000000     C  N91MOCTRG        COMP      '2'                                    91    *
015900000000     C  N91MOCTRG        COMP      '3'                                    91    *
016000000000     C     MOCTRG        COMP      'C'                                    92    *
016100000000     C*--- RICERCA ANAGRAFICA PRODOTTO --------------------*
016200000000     C   91              DO                                                     *
016300990126     C     KEYAMP        CHAIN     tmamp01L                           94        *
016400000000     C   94              GOTO      LEGGI                                        *
016500000000     C     CLM150        IFNE      '999'                                        *CLASSE
016600000000     C     CLM150        CABNE     AMPCLM        LEGGI                          *
016700000000     C                   END                                                    *
016800000000     C     SCM150        IFNE      '99999'                                      *S/CLASSE
016900000000     C     SCM150        CABNE     AMPSCM        LEGGI                          *
017000000000     C                   END                                                    *
017100000000     C                   END                                                    *
017200000000     C*--------- CALCOLO VALORE NETTO ---------------------*
017300000000     C   91                                                                     *
017400000000     COR 92              DO                                                     *
017500990126     C                   Z-ADD     MOCPRZ        PRENET           14 5          *
017600990126     C     PRENET        MULT      MOCSC1        SCONTO           14 5          *
017700000000     C     SCONTO        DIV       100           SCONTO                         *
017800000000     C     PRENET        SUB       SCONTO        PRENET                         *
017900000000     C     PRENET        MULT      MOCSC2        SCONTO                         *
018000000000     C     SCONTO        DIV       100           SCONTO                         *
018100000000     C     PRENET        SUB       SCONTO        PRENET                         *
018200000000     C     PRENET        MULT      MOCSC3        SCONTO
018300000000     C     SCONTO        DIV       100           SCONTO                         *
018400000000     C     PRENET        SUB       SCONTO        PRENET                         *
018500000000     C** SOTTRAE SCONTO 1,2,3
018600000000     C     MOCQTA        MULT(H)   PRENET        IMPMER                         *
018700000000     C     VALOR2        ADD       IMPMER        VALOR2                         *
018800000000     C                   Z-ADD     PRENET        PRZMOC                         *
018900000000     C                   END                                                    *
019000000000     C  N91
019100000000     CANN92              Z-ADD     MOCPRZ        PRZMOC                         *
019200000000     C*--- CARICAMENTO SUBFILE ----------------------------*
019300000000     C                   MOVE      *BLANKS       SCELTA                         *
019400000000     C                   MOVEL     MOCDMT        DESAAA                         *
019500000000     C                   Z-ADD     MOCQTA        QTAMOC                         *
019600000000     C     REC2          ADD       1             REC2                 71        *
019700000000     C                   WRITE     SFL2                                         *
019800000000     C                   SETOFF                                           93    *
019900000000     C                   GOTO      LEGGI                                        *
020000000000     C*--- EMISSIONE SUBFILE ------------------------------*
020100000000     C     EMISFL        TAG                                                    *
020200000000     C                   WRITE     OCO150T1                                     * TESTATA
020300000000     C                   WRITE     OCO150FD                                     * FONDO
020400000000     C                   EXFMT     SFL2CT                                       * SFL
020500990126     C   Kl              MOVE      'P'           RIC150                         * PROMPT INIZIO
020600990409     C   Kl              GOTO      fine                                         *
020700000000     C*--- EMISSIONE STAMPA -------------------------------*
020800990409     C   Kh              DO                                                     *
020900000000     C                   OPEN      OCPRTSTD                             95      *
021000000000     C                   EXSR      RUTSTA                                       *
021100000000     C                   EXCEPT    TOTAL                                        *
021200000000     C                   MOVE      'S'           STA150                         *
021300000000     C                   GOTO      FINE                                         *
021400000000     C                   END                                                    *
021500000000     C*--- ESEGUO SCELTA ----------------------------------*
021600000000     C   71              READC     SFL2                                   94    *
021700000000     C   71
021800000000     CAN 94              GOTO      EMISFL                                       * EOF
021900000000     C     SCELTA        CABEQ     *BLANKS       EMISFL                         *
022000000000     C*--- SCELTO INTERROGAZIONE X ARTICOLO  --------------*
022100000000     C     SCELTA        IFEQ      'A'                                          *
022200000000     C                   MOVE      'A'           RIC150                         *
022300000000     C                   MOVE      MOCMAT        MAT150                         *
022400000000     C                   MOVEL     MOCDMT        DESMAT                         *
022500000000     C                   GOTO      FINE                                         *
022600000000     C                   END                                                    *
022700000000     C*--- SCELTO REVISIONE ORDINE ------------------------*
022800000000     C     SCELTA        IFEQ      'O'                                          *
022900000000     C                   MOVE      'O'           RIC150                         *
023000000000     C                   MOVE      MOCCLI        FOR150                         *
023100990126     c                   if        mocdor > 0
023200990126     C                   move      MOCDOR        DATAIso                        *
023300990126     C                   move      dataiso       dataeur                        *
023400990126     C                   move      DATAeur       DOR150                         *
023500990126     c                   end
023600000000     C                   Z-ADD     MOCNOR        NOR150                         *
023700000000     C                   GOTO      FINE                                         *
023800000000     C                   END                                                    *
023900000000     C*--- SCELTO INTERROGAZIONE ORDINI -------------------*
024000000000     C     SCELTA        IFEQ      'I'                                          *
024100000000     C                   MOVE      'I'           RIC150                         *
024200000000     C                   MOVE      MOCCLI        FOR150                         *
024300000000     C                   Z-ADD     0             DOR150                         *
024400000000     C                   Z-ADD     0             NOR150                         *
024500000000     C                   GOTO      FINE                                         *
024600000000     C                   END                                                    *
024700000000     C*--- FINE LAVORO ------------------------------------*
024800000000     C     FINE          TAG                                                    *
024900990126     C   90              MOVEL     'KPJB207'     KMSMN                          *
025000000000     C                   SETON                                            LR    *
025100000000     C**--------- RUTINE EMISSIONE STAMPA ---------------***
025200000000     C***------------------------------------------------***
025300000000     CSR   RUTSTA        BEGSR                                                  *
025400000000     C                   Z-ADD     0             PAGE              3 0          *
025500000000     C                   Z-ADD     50            PAGE1             3 0          *
025600000000     C     1             DO        RECT          REC2              5 0          *
025700000000     C     REC2          CHAIN     SFL2                               91        *
025800000000     C   91              GOTO      ENDSTA                                       *
025900000000     C     PAGE1         IFGE      50                                           *
026000000000     C                   EXCEPT    TESTA                                        *
026100000000     C                   Z-ADD     0             PAGE1                          *
026200000000     C                   END                                                    *
026300990126     c                   if        mocdor > 0
026400990126     C                   move      MOCDOR        DATAIso                        *
026500990126     C                   move      dataiso       dataeur                        *
026600990126     C                   move      DATAeur       DATDOR            8 0          *
026700990126     c                   end
026800000000     C                   EXCEPT    DETAIL                                       *
026900000000     C     ENDSTA        TAG                                                    *
027000000000     C                   END                                                    *
027100000000     CSR                 ENDSR                                                  *
027200990126     C*----------------------------------------------------*
027300990126     C* Reperimento dati società
027400990126     C*----------------------------------------------------*
027500990126     C     REPSOC        BEGSR
027600990126     C*
027700990126     C                   CALLB     'XSOC'
027800990126     C                   PARM                    TIPXSC            6
027900990126     C                   PARM                    SOCXSC            3
028000990126     C                   PARM                    CDSXSC            9 0
028100990126     C                   PARM                    MODXSC            3
028200990126     C                   PARM      *blanks       RTNXSC            1
028300990126     C                   PARM                    XSOCDS
028400990126     C                   PARM                    KPJBA
028500990126     C*
028600990126     C                   ENDSR
028700990126     O*--------------------------------------------------------------*
028800000000     O*     TESTATA                                                  *
028900000000     OOCPRTSTD  E            TESTA            01
029000000000     O                       SK1                132
029100000000     O                       DESDIT              20
029200990209     O                       woggi              120 '  /  /    '
029300000000     O                       PAGE          Z    132
029400000000     O          E            TESTA       1
029500000000     O                       SK2                132
029600990126     O                       DAL                 87 '  /  /    '
029700990126     O                       AL                 101 '  /  /    '
029800000000     O          E            TESTA       1
029900000000     O                       SK3                132
030000000000     O                       KNMUS              132
030100000000     O          E            TESTA       2
030200000000     O                                           55 'Cliente ..:'
030300990126     O                       FOR150              64
030400990126     O                                           65 '-'
030500990126     O                       DESKK2              95
030600000000     O          E            TESTA       0  0
030700990126     O                       FOR150              64
030800990126     O                       DESKK2              95
030900000000     O          E            TESTA       2
031000000000     O                       SK4                132
031100000000     O*--------------------------------------------------------------*
031200000000     O*     DETTAGLIO                                                *
031300000000     O*--------------------------------------------------------------*
031400000000     O          E            DETAIL      1
031500000000     O                       PAGE1              132
031600000000     O                       SK5                132
031700871005     O                       MOCMAT              13
031800871005     O                       MOCDMT              44
031900000000     O                       MOCNOR              51
032000990126     O                       DATDOR              62 '  /  /    '
032100990126     O                       MOCTOR              64
032200000000     O                       MOCFAS              68
032300000000     O                       MOCTRG              72
032400000000     O                       MOCUMQ              78
032500000000     O                       QTAMOC              88 '    0 ,  '
032600000000     O                       MOCVAL              95
032700990126     O                       PRZMOC             106 '       0 ,    '
032800990126     O               03      IMPMER             121 '       0 ,     -'
032900000000     O                       MOCMAG             130
033000000000     O*--------------------------------------------------------------*
033100000000     O*     TOTALE :                                                 *
033200000000     O*--------------------------------------------------------------*
033300000000     O          E    03      TOTAL       1
033400000000     O                       PAGE1              132
033500000000     O                       SK6                132
033600000000     O          E    03      TOTAL       1
033700000000     O                       PAGE1              102
033800000000     O                                          102 'TOTALE VALORE ORDINI :'
033900990126     O                       TOTVAL        2    117
034000000000     O          E    03      TOTAL       0  0
034100990126     O                       TOTVAL        2    117
034200000000     O          E            TOTAL       1
034300000000     O                       PAGE1              132
034400000000     O                       SK6                132
034500000000     O*--------------------------------------------------------------*
034600000000**   SK1
034700000000*-DESDIT-----------****-------------------------------------------SK01
034800000000-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
034900000000**   SK2
035000000000                    ** Gest.Ord.Clienti ..: INTERROG. GENERALE x CSK02
035100990317LIENTE   dal xx/xx/xx al xx/xx/xx     **        PGM.:   TII301R1  SK02
035200000000**   SK3
035300000000                    ***-------------------------------------------SK03
035400000000-------------------------------------***        Utente  XXXXXXXXXXSK03
035500000000**   SK4
035600871005*-Prodotto----Descrizione-------------------*-Ordine---del--Tip-*FSK04
035700000000ase*TRg*-Um. Quantità--*----Prezzo Netto--Valore ------*-Magazz--*SK04
035800000000**   SK5
035900000000*        /                                  *                   * SK05
036000000000   *   *               *                               *         *SK05
036100000000**   SK6
036200000000*-------------------------------------------*-------------------*-SK06
036300000000---*---*---------------*-------------------------------*---------*SK06
