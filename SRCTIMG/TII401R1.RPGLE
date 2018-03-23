000100990204     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300860207     F*-OFN150R1 *-OF/38-*--------------------------------------*
000400860207     F*------------------*                                      *
000500880915     F*      -  INTERROGAZIONE PTF. OF PER DATA ORDINE   -      *
000600880915     F*      -------------PER FORNITORE   ---------------- N150 *
000700860207     F*                                                         *
000800990204     F*         FILE : OFN150D  - tmsof04L - tmamp01L -         *
000900860207     F*                                                         *
001000860207     F*         PGM  : RIC001R  - RICERCA DESCRIZIONE DITTA     *
001100860207     F*                                                         *
001200860207     F*---------------------------------------------------------*
001300990317     FTII401D   CF   E             WORKSTN
001400000000     F                                     IGNORE(SFL1)
001500000000     F                                     IGNORE(SFL1CT)
001600860207     F                                     IGNORE(OFN150PT)
001700000000     F                                     SFILE(SFL2:REC2)
001800990204     Ftmsof04L  IF   E           K DISK
001900990204     Ftmamp01L  IF   E           K DISK
002000860328     FOFPRTSTD  O    F  132        PRINTER OFLIND(*INOF)
002100860328     F                                     USROPN
002200860210     D*--------------------------------------------------------------*
002300860210     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
002400860210     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
002500860210     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 3
002600860210     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 4
002700860210     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 5
002800860210     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 6
002900860210     D*--------------------------------------------------------------*
003000990204     D KPJBA         E DS
003100990204     D*--- PARAMETRI X INTERROGAZIONE :
003200990204     D  FOR150               247    254
003300990204     D  DESFOR               255    302
003400990204     D  MAT150               407    418
003500990204     D  DESMAT               311    340
003600990204     D  DAL150               303    310  0
003700990204     D  AL150                345    352  0
003800990204     D  SNV150               353    353
003900990204     D  TOR150               354    355
004000990204     D  TR1150               356    356
004100990204     D  TR2150               357    357
004200990204     D  TR3150               358    358
004300990204     D  TR4150               359    359
004400990204     D  TR5150               360    360
004500990204     D  TR6150               361    361
004600990204     D  TR7150               362    362
004700990204     D  TR8150               363    363
004800990204     D  TR9150               364    364
004900990204     D  TRG                  356    364
005000990204     D                                     DIM(9)
005100990204     D  GRM150               365    366
005200990204     D  SEM150               367    368
005300990204     D  CLM150               369    371
005400990204     D  SCM150               372    376
005500990204     D  FA1150               377    378
005600990204     D  FA2150               379    380
005700990204     D  FA3150               381    382
005800990204     D  FA4150               383    384
005900990204     D  FA5150               385    386
006000990204     D  FAS                  377    386
006100990204     D                                     DIM(5)
006200990204     D  MAG150               387    393
006300990204     D  nOR150               394    398  0
006400990204     D  dOR150               399    406  0
006500990204     D  STA150               441    441
006600990204     D  RIC150               442    442
006700990204     D  UTE150               443    452
006800990204     D  MSGERR               453    502
006900990204     D soc001        E DS                  EXTNAME(xsoc001ds)
007000990204     D xsocds          DS          1000
007100990204     d dataiso         s               d   datfmt(*iso)
007200990204     d dataeur         s               d   datfmt(*eur)
007300000000     C*----------------------------------------------------*
007400860207     C     *ENTRY        PLIST                                                  *
007500860207     C                   PARM                    KPJBA                          *
007600990204     C*---------- RICERCA DITTA :
007700990204     C                   MOVEL     'SOC001'      TIPXSC
007800990204     C                   MOVEL     *blank        SOCXSC
007900990204     C                   EXSR      REPSOC
008000990204     C     RTNXSC        IFNE      '1'
008100990204     C                   MOVEL     XSOCDS        SOC001
008200990204     C                   MOVEL     xscrgs        desdit
008300990204     C                   MOVEL     xscsoc        wsoc              3
008400990204     c                   else
008500990204     c                   seton                                        90
008600990204     c                   goto      fine
008700990204     c                   end
008800990204     c                   time                    como14           14 0
008900990204     c                   move      como14        woggi             8 0
009000990204     c                   move      woggi         dataeur
009100990204     c                   move      dataeur       dataiso
009200990204     c                   move      dataiso       woggig            8 0
009300990204     C*--- FILE ANAGRAFICO ARTICOLI "tmamp01L"             *
009400860207     C     KEYAMP        KLIST                                                  *
009500860207     C                   KFLD                    MOFMAT                         *
009600000000     C*--- IMPOSTAZIONI INIZIALI --------------------------*
009700860207     C     SNV150        COMP      'S'                                    03    *
009800860207     C                   Z-ADD     0             VALOR2           15 2    02    *
009900860208     C                   Z-ADD     0             TOTVAL                         *
010000860207     C                   MOVEL     DESFOR        DESKK2                         *
010100990204     c                   if        dal150 > 0
010200990204     C                   move      DAL150        DATAIso                        *
010300990204     C                   move      dataiso       dataeur                        *
010400990204     C                   move      DATAeur       DAL                            *
010500990204     c                   end
010600990204     c                   if        al150 > 0
010700990204     C                   move      AL150         DATAIso                        *
010800990204     C                   move      dataiso       dataeur                        *
010900990204     C                   move      DATAeur       AL                             *
011000990204     c                   end
011100000000     C*--- PULIZIA SUBFILE --------------------------------*
011200860210     C                   Z-ADD     0             REC2              5 0    70    *
011300860210     C                   Z-ADD     0             RECT              5 0          *
011400860207     C   70              WRITE     SFL2CT                                       *
011500860207     C                   SETOFF                                       7071      *
011600000000     C*--- POSIZIONAMENTO SU FILE ORDINI ------------------*
011700990204     C     FOR150        SETLL     tmsof04L                                     *
011800860207     C     LEGGI         TAG                                                    *
011900990204     C     FOR150        READE     tmsof04L                               99    *
012000860207     C   99
012100860207     CANN71              MOVE      'E'           RIC150                         *
012200860207     C   99
012300860207     CANN71              GOTO      FINE                                         *
012400860208     C   99
012500860208     CAN 03              Z-ADD     VALOR2        TOTVAL                         * SI VALORIZZ.
012600860210     C   99              Z-ADD     REC2          RECT                           *
012700860207     C   99              GOTO      EMISFL                                       *
012800860207     C*--- CONTROLLI CON SCELTE DA PROMPT -----------------*
012900860207     C     MOFDOR        CABLT     DAL150        LEGGI                          * DATA ORDINE
013000860207     C     MOFDOR        CABGT     AL150         LEGGI                          *
013100860207     C*-----                                               *
013200860207     C     GRM150        IFNE      '99'                                         * GRUPPO
013300860207     C     GRM150        CABNE     MOFGPM        LEGGI                          *
013400860207     C                   END                                                    *
013500860207     C*-----                                               *
013600860207     C     SEM150        IFNE      '99'                                         * SETTORE
013700860207     C     SEM150        CABNE     MOFSTM        LEGGI                          *
013800860207     C                   END                                                    *
013900860207     C*----------------------------------------------------*
014000860210     C     TOR150        IFNE      '99'                                         * TIPO ORDINE
014100860210     C     TOR150        CABNE     MOFTOR        LEGGI                          *
014200860210     C                   END                                                    *
014300860210     C*-----                                               *
014400860207     C     MOFTRG        LOOKUP    TRG                                    91    * TIPO RIGA
014500860207     C  N91              GOTO      LEGGI                                        *
014600860207     C*-----                                               *
014700860207     C     MOFFAS        LOOKUP    FAS                                    91    * FASE
014800860207     C  N91              GOTO      LEGGI                                        *
014900860207     C     MOFFAS        COMP      'BB'                                   93    *
015000860207     C  N93MOFFAS        COMP      'FA'                                   93    *
015100860207     C*-----                                               *
015200860207     C     MAG150        IFNE      '9999999'                                    *MAGAZZINO
015300860207     C     MAG150        CABNE     MOFMAG        LEGGI                          *
015400860207     C                   END                                                    *
015500860210     C*-----                                               *
015600990204     C                   Z-ADD     0             IMPMER                         *
015700860207     C     MOFTRG        COMP      '1'                                    91    *
015800860207     C  N91MOFTRG        COMP      '2'                                    91    *
015900860207     C  N91MOFTRG        COMP      '3'                                    91    *
016000860210     C     MOFTRG        COMP      'C'                                    92    *
016100860210     C*--- RICERCA ANAGRAFICA PRODOTTO --------------------*
016200860207     C   91              DO                                                     *
016300990204     C     KEYAMP        CHAIN     tmamp01L                           94        *
016400860210     C   94              GOTO      LEGGI                                        *
016500860207     C     CLM150        IFNE      '999'                                        *CLASSE
016600860207     C     CLM150        CABNE     AMPCLM        LEGGI                          *
016700860207     C                   END                                                    *
016800860207     C     SCM150        IFNE      '99999'                                      *S/CLASSE
016900860207     C     SCM150        CABNE     AMPSCM        LEGGI                          *
017000860207     C                   END                                                    *
017100860210     C                   END                                                    *
017200860207     C*--------- CALCOLO VALORE NETTO ---------------------*
017300860210     C   91                                                                     *
017400860210     COR 92              DO                                                     *
017500990204     C                   Z-ADD     MOFPRZ        PRENET           14 5          *
017600990204     C     PRENET        MULT      MOFSC1        SCONTO           14 5          *
017700860207     C     SCONTO        DIV       100           SCONTO                         *
017800860207     C     PRENET        SUB       SCONTO        PRENET                         *
017900860207     C     PRENET        MULT      MOFSC2        SCONTO                         *
018000860207     C     SCONTO        DIV       100           SCONTO                         *
018100860207     C     PRENET        SUB       SCONTO        PRENET                         *
018200870202     C     PRENET        MULT      MOFSC3        SCONTO
018300870202     C     SCONTO        DIV       100           SCONTO                         *
018400870202     C     PRENET        SUB       SCONTO        PRENET                         *
018500870202     C** SOTTRAE SCONTO 1,2,3
018600860207     C     MOFQTA        MULT(H)   PRENET        IMPMER                         *
018700860207     C     IMPMER        MULT(H)   MOFCMB        IMPMER                         *
018800860207     C     VALOR2        ADD       IMPMER        VALOR2                         *
018900860207     C                   Z-ADD     PRENET        PRZMOF                         *
019000860207     C                   END                                                    *
019100860210     C  N91
019200860210     CANN92              Z-ADD     MOFPRZ        PRZMOF                         *
019300860207     C*--- CARICAMENTO SUBFILE ----------------------------*
019400860207     C                   MOVE      *BLANKS       SCELTA                         *
019500860207     C                   MOVEL     MOFDMT        DESAAA                         *
019600860207     C                   Z-ADD     MOFQTA        QTAMOF                         *
019700860207     C     REC2          ADD       1             REC2                 71        *
019800860207     C                   WRITE     SFL2                                         *
019900860207     C                   SETOFF                                           93    *
020000860207     C                   GOTO      LEGGI                                        *
020100000000     C*--- EMISSIONE SUBFILE ------------------------------*
020200860207     C     EMISFL        TAG                                                    *
020300860207     C                   WRITE     OFN150T1                                     * TESTATA
020400860207     C                   WRITE     OFN150FD                                     * FONDO
020500000000     C                   EXFMT     SFL2CT                                       * SFL
020600990204     C   Kl              MOVE      'P'           RIC150                         * PROMPT INIZIO
020700990409     C   Kl              GOTO      FINE                                         *
020800860210     C*--- EMISSIONE STAMPA -------------------------------*
020900990409     C   Kh              DO                                                     *
021000860328     C                   OPEN      OFPRTSTD                             95      *
021100860328     C                   EXSR      RUTSTA                                       *
021200860328     C                   EXCEPT    TOTAL                                        *
021300860328     C                   MOVE      'S'           STA150                         *
021400860328     C                   GOTO      FINE                                         *
021500860328     C                   END                                                    *
021600000000     C*--- ESEGUO SCELTA ----------------------------------*
021700860210     C   71              READC     SFL2                                   94    *
021800860210     C   71
021900860210     CAN 94              GOTO      EMISFL                                       * EOF
022000860207     C     SCELTA        CABEQ     *BLANKS       EMISFL                         *
022100860207     C*--- SCELTO INTERROGAZIONE X ARTICOLO  --------------*
022200860207     C     SCELTA        IFEQ      'A'                                          *
022300860207     C                   MOVE      'A'           RIC150                         *
022400860207     C                   MOVE      MOFMAT        MAT150                         *
022500860207     C                   MOVEL     MOFDMT        DESMAT                         *
022600860207     C                   GOTO      FINE                                         *
022700860207     C                   END                                                    *
022800860207     C*--- SCELTO REVISIONE ORDINE ------------------------*
022900860207     C     SCELTA        IFEQ      'O'                                          *
023000860207     C                   MOVE      'O'           RIC150                         *
023100860207     C                   MOVE      MOFFOR        FOR150                         *
023200990204     c                   if        mofdor > 0
023300990204     C                   move      MOfDOR        DATAIso                        *
023400990204     C                   move      dataiso       dataeur                        *
023500990204     C                   move      DATAeur       DOR150                         *
023600990204     c                   end
023700860207     C                   Z-ADD     MOFNOR        NOR150                         *
023800860207     C                   GOTO      FINE                                         *
023900860207     C                   END                                                    *
024000860328     C*--- SCELTO INTERROGAZIONE ORDINI -------------------*
024100860328     C     SCELTA        IFEQ      'I'                                          *
024200860328     C                   MOVE      'I'           RIC150                         *
024300860328     C                   MOVE      MOFFOR        FOR150                         *
024400860328     C                   Z-ADD     0             DOR150                         *
024500860328     C                   Z-ADD     0             NOR150                         *
024600860328     C                   GOTO      FINE                                         *
024700860328     C                   END                                                    *
024800000000     C*--- FINE LAVORO ------------------------------------*
024900860207     C     FINE          TAG                                                    *
025000860207     C                   SETON                                            LR    *
025100860210     C**--------- RUTINE EMISSIONE STAMPA ---------------***
025200860210     C***------------------------------------------------***
025300860210     CSR   RUTSTA        BEGSR                                                  *
025400860210     C                   Z-ADD     0             PAGE              3 0          *
025500860210     C                   Z-ADD     50            PAGE1             3 0          *
025600860210     C     1             DO        RECT          REC2              5 0          *
025700860210     C     REC2          CHAIN     SFL2                               91        *
025800860210     C   91              GOTO      ENDSTA                                       *
025900860210     C     PAGE1         IFGE      50                                           *
026000860210     C                   EXCEPT    TESTA                                        *
026100860210     C                   Z-ADD     0             PAGE1                          *
026200860210     C                   END                                                    *
026300990204     c                   if        mofdor > 0
026400990204     C                   move      MOfDOR        DATAIso                        *
026500990204     C                   move      dataiso       dataeur                        *
026600990204     C                   move      DATAeur       DATDOR            8 0          *
026700990204     c                   end
026800860210     C                   EXCEPT    DETAIL                                       *
026900860210     C     ENDSTA        TAG                                                    *
027000860210     C                   END                                                    *
027100860210     CSR                 ENDSR                                                  *
027200990204     C*----------------------------------------------------*
027300990204     C* Reperimento dati società
027400990204     C*----------------------------------------------------*
027500990204     C     REPSOC        BEGSR
027600990204     C*
027700990204     C                   CALLB     'XSOC'
027800990204     C                   PARM                    TIPXSC            6
027900990204     C                   PARM                    SOCXSC            3
028000990204     C                   PARM                    CDSXSC            9 0
028100990204     C                   PARM                    MODXSC            3
028200990204     C                   PARM      *blanks       RTNXSC            1
028300990204     C                   PARM                    XSOCDS
028400990204     C                   PARM                    KPJBA
028500990204     C*
028600990204     C                   ENDSR
028700860210     O*--------------------------------------------------------------*
028800860210     O*     TESTATA                                                  *
028900860210     OOFPRTSTD  E            TESTA            01
029000860210     O                       SK1                132
029100860210     O                       DESDIT              20
029200990209     O                       woggi              120 '  /  /    '
029300860210     O                       PAGE          Z    132
029400860210     O          E            TESTA       1
029500860210     O                       SK2                132
029600990204     O                       DAL                 87 '  /  /    '
029700990204     O                       AL                 101 '  /  /    '
029800860210     O          E            TESTA       1
029900860210     O                       SK3                132
030000860210     O                       KNMUS              132
030100860210     O          E            TESTA       2
030200860210     O                                           55 'Fornitore :'
030300860210     O                       FOR150              63
030400860210     O                                           64 '-'
030500860210     O                       DESKK2              94
030600860210     O          E            TESTA       0  0
030700860210     O                       FOR150              63
030800860210     O                       DESKK2              94
030900860210     O          E            TESTA       2
031000860210     O                       SK4                132
031100860210     O*--------------------------------------------------------------*
031200860210     O*     DETTAGLIO                                                *
031300860210     O*--------------------------------------------------------------*
031400860210     O          E            DETAIL      1
031500860210     O                       PAGE1              132
031600860210     O                       SK5                132
031700871006     O                       MOFMAT              13
031800871006     O                       MOFDMT              44
031900860210     O                       MOFNOR              51
032000990204     O                       DATDOR              62 '  /  /    '
032100990204     O                       MOFTOR              64
032200860210     O                       MOFFAS              68
032300860210     O                       MOFTRG              72
032400860210     O                       MOFUMQ              78
032500860210     O                       QTAMOF              88 '    0 ,  '
032600860210     O                       MOFVAL              95
032700990204     O                       PRZMOF             106 '       0 ,    '
032800990204     O               03      IMPMER             121 '       0 ,     -'
032900860210     O                       MOFMAG             130
033000860210     O*--------------------------------------------------------------*
033100860210     O*     TOTALE :                                                 *
033200860210     O*--------------------------------------------------------------*
033300860210     O          E    03      TOTAL       1
033400860210     O                       PAGE1              132
033500860210     O                       SK6                132
033600860210     O          E    03      TOTAL       1
033700860210     O                       PAGE1              102
033800860210     O                                          102 'TOTALE VALORE ORDINI :'
033900990204     O                       TOTVAL        2    117
034000860210     O          E    03      TOTAL       0  0
034100990204     O                       TOTVAL        2    117
034200860210     O          E            TOTAL       1
034300860210     O                       PAGE1              132
034400860210     O                       SK6                132
034500860210     O*--------------------------------------------------------------*
034600860210**   SK1
034700860210*-DESDIT-----------****-------------------------------------------SK01
034800860210-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
034900860210**   SK2
035000860210                    ** Gest.Ord.Fornitori : INTERROG. GENERALE x FSK02
035100990317ORNITORE dal xx/xx/xx al xx/xx/xx     **        PGM.:   TII401R1  SK02
035200860210**   SK3
035300860210                    ***-------------------------------------------SK03
035400860210-------------------------------------***        Utente  XXXXXXXXXXSK03
035500860210**   SK4
035600871006*-Prodotto----Descrizione-------------------*-Ordine---del--Tip-*FSK04
035700860210ase*TRg*-Um. Quantità--*----Prezzo Netto--Valore ------*-Magazz--*SK04
035800860210**   SK5
035900860210*        /                                  *                   * SK05
036000860210   *   *               *                               *         *SK05
036100860210**   SK6
036200860210*-------------------------------------------*-------------------*-SK06
036300860210---*---*---------------*-------------------------------*---------*SK06
