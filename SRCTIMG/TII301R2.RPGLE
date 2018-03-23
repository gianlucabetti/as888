000100990126     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990126     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300870514     F*-OCO150R2 *-OC/38-*--------------------------------------*
000400000000     F*------------------*                                      *
000500880915     F*      -  INTERROGAZIONE PTF. O.C. PER DATA ORDINEO -     *
000600880915     F*      --------------------PER MATERIALE-------------N150 *
000700000000     F*                                                         *
000800990126     F*         FILE : OCO150D  - tmsoc02L - tmclp01L -         *
000900000000     F*                                                         *
001000000000     F*         PGM  : RIC001R  - RICERCA DESCRIZIONE DITTA     *
001100000000     F*                                                         *
001200000000     F*---------------------------------------------------------*
001300990317     FTII301D   CF   E             WORKSTN
001400000000     F                                     IGNORE(SFL2)
001500000000     F                                     IGNORE(SFL2CT)
001600870514     F                                     IGNORE(OCO150PT)
001700000000     F                                     SFILE(SFL1:REC1)
001800990126     Ftmsoc02L  IF   E           K DISK
001900990126     Ftmclp01L  IF   E           K DISK
002000870514     FOCPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
002100870514     F                                     USROPN
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
009300000000     C*--------------                                      *
009400870514     C     KEYMOC        KLIST                                                  *
009500000000     C                   KFLD                    MAT150                         *
009600000000     C*--- IMPOSTAZIONI INIZIALI --------------------------*
009700000000     C     SNV150        COMP      'S'                                    03    *
009800000000     C                   Z-ADD     0             VALOR2           15 2    01    *
009900000000     C                   Z-ADD     0             TOTVAL                         *
010000000000     C                   Z-ADD     0             TOTQTA                         *
010100000000     C                   MOVEL     DESMAT        DESKK1                         *
010200990126     c                   if        dal150 > 0
010300990126     C                   move      DAL150        DATAIso                        *
010400990126     C                   move      dataiso       dataeur                        *
010500990126     C                   move      DATAeur       DAL                            *
010600990126     c                   end
010700990126     c                   if        al150 > 0
010800990126     C                   move      AL150         DATAIso                        *
010900990126     C                   move      dataiso       dataeur                        *
011000990126     C                   move      DATAeur       AL                             *
011100990126     c                   end
011200000000     C*--- PULIZIA SUBFILE --------------------------------*
011300000000     C                   Z-ADD     0             REC1              5 0    70    *
011400000000     C                   Z-ADD     0             RECT              5 0          *
011500000000     C   70              WRITE     SFL1CT                                       *
011600000000     C                   SETOFF                                       7071      *
011700000000     C*--- POSIZIONAMENTO SU FILE ORDINI ------------------*
011800990126     C     KEYMOC        SETLL     tmsoc02L                                     *
011900000000     C     LEGGI         TAG                                                    *
012000990126     C     KEYMOC        READE     tmsoc02L                               99    *
012100000000     C   99
012200000000     CANN71              MOVE      'E'           RIC150                         *
012300000000     C   99
012400000000     CANN71              GOTO      FINE                                         *
012500000000     C   99
012600000000     CAN 03              Z-ADD     VALOR2        TOTVAL                         * SI VALORIZZ.
012700000000     C   99              Z-ADD     REC1          RECT                           *
012800000000     C   99              GOTO      EMISFL                                       *
012900000000     C*--- CONTROLLI CON SCELTE DA PROMPT -----------------*
013000870514     C     MOCDOR        CABLT     DAL150        LEGGI                          * DATA ORDINE
013100870514     C     MOCDOR        CABGT     AL150         LEGGI                          *
013200000000     C*-----                                               *
013300870514     C     MOCTRG        LOOKUP    TRG                                    91    * TIPO RIGA
013400000000     C  N91              GOTO      LEGGI                                        *
013500000000     C*--------                                            *
013600000000     C     TOR150        IFNE      '99'                                         * TIPO ORDINE
013700870514     C     TOR150        CABNE     MOCTOR        LEGGI                          *
013800000000     C                   END                                                    *
013900000000     C*----------------------------------------------------*
014000870514     C     MOCFAS        LOOKUP    FAS                                    91    * FASE
014100000000     C  N91              GOTO      LEGGI                                        *
014200870514     C     MOCFAS        COMP      'BB'                                   93    *
014300870514     C  N93MOCFAS        COMP      'FA'                                   93    *
014400000000     C*-----                                               *
014500000000     C     MAG150        IFNE      '9999999'                                    *MAGAZZINO
014600870514     C     MAG150        CABNE     MOCMAG        LEGGI                          *
014700000000     C                   END                                                    *
014800000000     C*--- RICERCA ANAGRAFICA FORNITORE -------------------*
014900870514     C     OLDFOR        IFNE      MOCCLI                                       *
015000990126     C     MOCCLI        CHAIN     tmclp01L                           92        *
015100000000     C   92              GOTO      LEGGI                                        *
015200870514     C                   MOVEL     CLPRSI        DESFFF                         *
015300880502     C                   MOVE      MOCCLI        MOCFOR
015400990126     C                   MOVE      MOCFOR        OLDFOR            8            *
015500000000     C                   END                                                    *
015600990126     C                   Z-ADD     0             IMPMER                         *
015700000000     C*------
015800870514     C     MOCTRG        COMP      '1'                                    91    *
015900870514     C  N91MOCTRG        COMP      '2'                                    91    *
016000870514     C  N91MOCTRG        COMP      '3'                                    91    *
016100000000     C  N91              GOTO      LEGGI                                        *
016200000000     C     GRM150        IFNE      '99'                                         * GRUPPO
016300870514     C     GRM150        CABNE     MOCGPM        LEGGI                          *
016400000000     C                   END                                                    *
016500000000     C*-----                                               *
016600000000     C     SEM150        IFNE      '99'                                         * SETTORE
016700870514     C     SEM150        CABNE     MOCSTM        LEGGI                          *
016800000000     C                   END                                                    *
016900000000     C*--------- CALCOLO VALORE NETTO ---------------------*
017000990126     C                   Z-ADD     MOCPRZ        PRENET           14 5          *
017100990126     C     PRENET        MULT      MOCSC1        SCONTO           14 5          *
017200000000     C     SCONTO        DIV       100           SCONTO                         *
017300000000     C     PRENET        SUB       SCONTO        PRENET                         *
017400870514     C     PRENET        MULT      MOCSC2        SCONTO                         *
017500000000     C     SCONTO        DIV       100           SCONTO                         *
017600000000     C     PRENET        SUB       SCONTO        PRENET                         *
017700870514     C     PRENET        MULT      MOCSC3        SCONTO
017800000000     C     SCONTO        DIV       100           SCONTO                         *
017900000000     C     PRENET        SUB       SCONTO        PRENET                         *
018000870514     C     MOCQTA        MULT(H)   PRENET        IMPMER                         *
018100000000     C     VALOR2        ADD       IMPMER        VALOR2                         *
018200870514     C                   Z-ADD     PRENET        PRZMOC                         *
018300870514     C     TOTQTA        ADD       MOCQTA        TOTQTA                         *
018400000000     C*--- CARICAMENTO SUBFILE ----------------------------*
018500000000     C                   MOVE      *BLANKS       SCELTA                         *
018600870514     C                   Z-ADD     MOCQTA        QTAMOC                         *
018700000000     C     REC1          ADD       1             REC1                 71        *
018800000000     C                   WRITE     SFL1                                         *
018900000000     C                   SETOFF                                           93    *
019000000000     C                   GOTO      LEGGI                                        *
019100000000     C*--- EMISSIONE SUBFILE ------------------------------*
019200000000     C     EMISFL        TAG                                                    *
019300870514     C                   WRITE     OCO150T1                                     * TESTATA
019400870514     C                   WRITE     OCO150FD                                     * FONDO
019500000000     C                   EXFMT     SFL1CT                                       * SFL
019600990126     C   Kl              MOVE      'P'           RIC150                         * PROMPT INIZIO
019700990409     C   Kl              GOTO      fine                                         *
019800000000     C*--- EMISSIONE STAMPA -------------------------------*
019900990409     C   Kh              DO                                                     *
020000870514     C                   OPEN      OCPRTSTD                             95      *
020100000000     C                   EXSR      RUTSTA                                       *
020200000000     C                   EXCEPT    TOTAL                                        *
020300000000     C                   MOVE      'S'           STA150                         *
020400000000     C                   GOTO      FINE                                         *
020500000000     C                   END                                                    *
020600000000     C*--- ESEGUO SCELTA ----------------------------------*
020700000000     C   71              READC     SFL1                                   92    *
020800000000     C   71
020900000000     CAN 92              GOTO      EMISFL                                       * EOF
021000000000     C     SCELTA        CABEQ     *BLANKS       EMISFL                         *
021100000000     C*--- SCELTO INTERROGAZIONE X FORNITORE --------------*
021200000000     C     SCELTA        IFEQ      'F'                                          *
021300000000     C                   MOVE      'F'           RIC150                         *
021400880519     C                   MOVE      MOCFOR        FOR150                         *
021500000000     C                   MOVEL     DESFFF        DESFOR                         *
021600000000     C                   GOTO      FINE                                         *
021700000000     C                   END                                                    *
021800000000     C*--- SCELTO REVISIONE ORDINE ------------------------*
021900000000     C     SCELTA        IFEQ      'O'                                          *
022000000000     C                   MOVE      'O'           RIC150                         *
022100880519     C                   MOVE      MOCFOR        FOR150                         *
022200990126     c                   if        mocdor > 0
022300990126     C                   move      MOCDOR        DATAIso                        *
022400990126     C                   move      dataiso       dataeur                        *
022500990126     C                   move      DATAeur       DOR150                         *
022600990126     c                   end
022700870514     C                   Z-ADD     MOCNOR        NOR150                         *
022800000000     C                   GOTO      FINE                                         *
022900000000     C                   END                                                    *
023000000000     C*--- SCELTO INTERROGAZIONE ORDINI -------------------*
023100000000     C     SCELTA        IFEQ      'I'                                          *
023200000000     C                   MOVE      'I'           RIC150                         *
023300870514     C                   MOVE      MOCFOR        FOR150                         *
023400000000     C                   Z-ADD     0             DOR150                         *
023500000000     C                   Z-ADD     0             NOR150                         *
023600000000     C                   GOTO      FINE                                         *
023700000000     C                   END                                                    *
023800000000     C*--- FINE LAVORO ------------------------------------*
023900000000     C     FINE          TAG                                                    *
024000000000     C   90              MOVEL     'KPJB207'     KMSMN                          *
024100000000     C                   SETON                                            LR    *
024200000000     C**--------- RUTINE EMISSIONE STAMPA ---------------***
024300000000     C***------------------------------------------------***
024400000000     CSR   RUTSTA        BEGSR                                                  *
024500000000     C                   Z-ADD     0             PAGE              3 0          *
024600000000     C                   Z-ADD     50            PAGE1             3 0          *
024700000000     C     1             DO        RECT          REC1              5 0          *
024800000000     C     REC1          CHAIN     SFL1                               91        *
024900000000     C   91              GOTO      ENDSTA                                       *
025000000000     C     PAGE1         IFGE      50                                           *
025100000000     C                   EXCEPT    TESTA                                        *
025200000000     C                   Z-ADD     0             PAGE1                          *
025300000000     C                   END                                                    *
025400990126     c                   if        mocdor > 0
025500990126     C                   move      MOCDOR        DATAIso                        *
025600990126     C                   move      dataiso       dataeur                        *
025700990126     C                   move      DATAeur       DATDOR            8 0          *
025800990126     c                   end
025900000000     C                   EXCEPT    DETAIL                                       *
026000000000     C     ENDSTA        TAG                                                    *
026100000000     C                   END                                                    *
026200000000     CSR                 ENDSR                                                  *
026300990126     C*----------------------------------------------------*
026400990126     C* Reperimento dati società
026500990126     C*----------------------------------------------------*
026600990126     C     REPSOC        BEGSR
026700990126     C*
026800990126     C                   CALLB     'XSOC'
026900990126     C                   PARM                    TIPXSC            6
027000990126     C                   PARM                    SOCXSC            3
027100990126     C                   PARM                    CDSXSC            9 0
027200990126     C                   PARM                    MODXSC            3
027300990126     C                   PARM      *blanks       RTNXSC            1
027400990126     C                   PARM                    XSOCDS
027500990126     C                   PARM                    KPJBA
027600990126     C*
027700990126     C                   ENDSR
027800000000     O*--------------------------------------------------------------*
027900000000     O*     TESTATA                                                  *
028000000000     O*--------------------------------------------------------------*
028100870514     OOCPRTSTD  E            TESTA            01
028200000000     O                       SK1                132
028300000000     O                       DESDIT              20
028400990209     O                       woggi              120 '  /  /    '
028500000000     O                       PAGE          Z    132
028600000000     O          E            TESTA       1
028700000000     O                       SK2                132
028800990126     O                       DAL                 87 '  /  /    '
028900990126     O                       AL                 101 '  /  /    '
029000000000     O          E            TESTA       1
029100000000     O                       SK3                132
029200000000     O                       KNMUS              132
029300000000     O          E            TESTA       2
029400000000     O                                           55 'Prodotto  :'
029500871005     O                       MAT150              68
029600871005     O                       DESKK1             100
029700000000     O          E            TESTA       0  0
029800871006     O                       MAT150              68
029900871006     O                       DESKK1             100
030000000000     O          E            TESTA       2
030100000000     O                       SK4                132
030200000000     O*--------------------------------------------------------------*
030300000000     O*     DETTAGLIO                                                *
030400000000     O*--------------------------------------------------------------*
030500000000     O          E            DETAIL      1
030600000000     O                       PAGE1              132
030700000000     O                       SK5                132
030800880502     O                       MOCFOR               9
030900880502     O                       DESFFF              43
031000870514     O                       MOCNOR              51
031100990126     O                       DATDOR              62 '  /  /    '
031200990126     O                       MOCTOR              64
031300990126     O                       MOCFAS              68
031400990126     O                       MOCTRG              72
031500990126     O                       MOCUMQ              78
031600990126     O                       QTAMOC              88 '    0 ,  '
031700990126     O                       MOCVAL              95
031800990126     O                       PRZMOC             106 '       0 ,    '
031900990126     O               03      IMPMER             121 '       0 ,     -'
032000990126     O                       MOCMAG             130
032100000000     O*--------------------------------------------------------------*
032200000000     O*     TOTALE :                                                 *
032300000000     O*--------------------------------------------------------------*
032400000000     O          E            TOTAL       1
032500000000     O                       PAGE1              132
032600000000     O                       SK6                132
032700000000     O          E            TOTAL       1
032800000000     O                       PAGE1              102
032900000000     O                                           74 'TOTALE ORDINI : Quantità'
033000000000     O                       TOTQTA              88 '   .   . 0 ,  '
033100990209     O               03                         100 'Valore :'
033200990209     O               03      TOTVAL        2    120
033300000000     O          E            TOTAL       0  0
033400000000     O                       TOTQTA              88 '   .   . 0 ,  '
033500990209     O               03      TOTVAL        2    120
033600000000     O          E            TOTAL       1
033700000000     O                       PAGE1              132
033800000000     O                       SK6                132
033900000000**   SK1
034000000000*-DESDIT-----------* ***------------------------------------------SK01
034100000000-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
034200000000**   SK2
034300870514                    ** Gest.Ord.CLIENTori : INTERROG. GENERALE x PSK02
034400990317RODOTTO  dal xx/xx/xx al xx/xx/xx     **        PGM.:   TII301R2  SK02
034500000000**   SK3
034600000000                    ***-------------------------------------------SK03
034700000000-------------------------------------***        Utente  XXXXXXXXXXSK03
034800000000**   SK4
034900870514*-Cliente    Descrizione--------------------*-Ordine---del--Tip-*FSK04
035000000000ase*TRg*-Um. Quantità--*----Prezzo Netto--Valore ------*-Magazz--*SK04
035100000000**   SK5
035200000000*                                           *                   * SK05
035300000000   *   *               *                               *         *SK05
035400000000**   SK6
035500000000*-------------------------------------------*-------------------*-SK06
035600000000---*---*---------------*-------------------------------*---------*SK06
