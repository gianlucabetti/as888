000100990204     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300860207     F*-OFN150R2 *-OF/38-*--------------------------------------*
000400860207     F*------------------*                                      *
000500880915     F*      -  INTERROGAZIONE PORTAFOGLIO OF X DATA ORDINE     *
000600880915     F*      ------------PER MATERIALE --------------------N150 *
000700860207     F*                                                         *
000800990204     F*         FILE : OFN150D  - tmsof02L - tmfop01L -         *
000900860207     F*                                                         *
001000860207     F*         PGM  : RIC001R  - RICERCA DESCRIZIONE DITTA     *
001100860207     F*                                                         *
001200860207     F*---------------------------------------------------------*
001300990317     FTII401D   CF   E             WORKSTN
001400860207     F                                     IGNORE(SFL2)
001500860207     F                                     IGNORE(SFL2CT)
001600860207     F                                     IGNORE(OFN150PT)
001700860207     F                                     SFILE(SFL1:REC1)
001800990204     Ftmsof02L  IF   E           K DISK
001900990204     Ftmfop01L  IF   E           K DISK
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
007300860207     C*----------------------------------------------------*
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
009300860207     C*--------------                                      *
009400860207     C     KEYMOF        KLIST                                                  *
009500860207     C                   KFLD                    MAT150                         *
009600860207     C*--- IMPOSTAZIONI INIZIALI --------------------------*
009700860207     C     SNV150        COMP      'S'                                    03    *
009800860207     C                   Z-ADD     0             VALOR2           15 2    01    *
009900860208     C                   Z-ADD     0             TOTVAL                         *
010000860208     C                   Z-ADD     0             TOTQTA                         *
010100860207     C                   MOVEL     DESMAT        DESKK1                         *
010200990204     c                   if        dal150 > 0
010300990204     C                   move      DAL150        DATAIso                        *
010400990204     C                   move      dataiso       dataeur                        *
010500990204     C                   move      DATAeur       DAL                            *
010600990204     c                   end
010700990204     c                   if        al150 > 0
010800990204     C                   move      AL150         DATAIso                        *
010900990204     C                   move      dataiso       dataeur                        *
011000990204     C                   move      DATAeur       AL                             *
011100990204     c                   end
011200860207     C*--- PULIZIA SUBFILE --------------------------------*
011300860210     C                   Z-ADD     0             REC1              5 0    70    *
011400860210     C                   Z-ADD     0             RECT              5 0          *
011500860207     C   70              WRITE     SFL1CT                                       *
011600860207     C                   SETOFF                                       7071      *
011700860207     C*--- POSIZIONAMENTO SU FILE ORDINI ------------------*
011800990204     C     KEYMOF        SETLL     tmsof02L                                     *
011900860207     C     LEGGI         TAG                                                    *
012000990204     C     KEYMOF        READE     tmsof02L                               99    *
012100860207     C   99
012200860207     CANN71              MOVE      'E'           RIC150                         *
012300860207     C   99
012400860207     CANN71              GOTO      FINE                                         *
012500860208     C   99
012600860208     CAN 03              Z-ADD     VALOR2        TOTVAL                         * SI VALORIZZ.
012700860210     C   99              Z-ADD     REC1          RECT                           *
012800860207     C   99              GOTO      EMISFL                                       *
012900860207     C*--- CONTROLLI CON SCELTE DA PROMPT -----------------*
013000860207     C     MOFDOR        CABLT     DAL150        LEGGI                          * DATA ORDINE
013100860207     C     MOFDOR        CABGT     AL150         LEGGI                          *
013200860207     C*-----                                               *
013300860207     C     MOFTRG        LOOKUP    TRG                                    91    * TIPO RIGA
013400860207     C  N91              GOTO      LEGGI                                        *
013500860207     C*--------                                            *
013600860207     C     TOR150        IFNE      '99'                                         * TIPO ORDINE
013700860207     C     TOR150        CABNE     MOFTOR        LEGGI                          *
013800860207     C                   END                                                    *
013900860207     C*----------------------------------------------------*
014000860207     C     MOFFAS        LOOKUP    FAS                                    91    * FASE
014100860207     C  N91              GOTO      LEGGI                                        *
014200860207     C     MOFFAS        COMP      'BB'                                   93    *
014300860207     C  N93MOFFAS        COMP      'FA'                                   93    *
014400860207     C*-----                                               *
014500860207     C     MAG150        IFNE      '9999999'                                    *MAGAZZINO
014600860207     C     MAG150        CABNE     MOFMAG        LEGGI                          *
014700860207     C                   END                                                    *
014800860207     C*--- RICERCA ANAGRAFICA FORNITORE -------------------*
014900860207     C     OLDFOR        IFNE      MOFFOR                                       *
015000990204     C     MOFFOR        CHAIN     tmfop01L                           92        *
015100860207     C   92              GOTO      LEGGI                                        *
015200860207     C                   MOVEL     FOPRSI        DESFFF                         *
015300990204     C                   MOVE      MOFFOR        OLDFOR            8            *
015400860207     C                   END                                                    *
015500990204     C                   Z-ADD     0             IMPMER                         *
015600860207     C*------
015700860207     C     MOFTRG        COMP      '1'                                    91    *
015800860207     C  N91MOFTRG        COMP      '2'                                    91    *
015900860208     C  N91MOFTRG        COMP      '3'                                    91    *
016000860210     C  N91              GOTO      LEGGI                                        *
016100860207     C     GRM150        IFNE      '99'                                         * GRUPPO
016200860207     C     GRM150        CABNE     MOFGPM        LEGGI                          *
016300860207     C                   END                                                    *
016400860207     C*-----                                               *
016500860207     C     SEM150        IFNE      '99'                                         * SETTORE
016600860207     C     SEM150        CABNE     MOFSTM        LEGGI                          *
016700860207     C                   END                                                    *
016800860207     C*--------- CALCOLO VALORE NETTO ---------------------*
016900990204     C                   Z-ADD     MOFPRZ        PRENET           14 5          *
017000990204     C     PRENET        MULT      MOFSC1        SCONTO           14 5          *
017100860207     C     SCONTO        DIV       100           SCONTO                         *
017200860207     C     PRENET        SUB       SCONTO        PRENET                         *
017300860207     C     PRENET        MULT      MOFSC2        SCONTO                         *
017400860207     C     SCONTO        DIV       100           SCONTO                         *
017500860207     C     PRENET        SUB       SCONTO        PRENET                         *
017600870202     C     PRENET        MULT      MOFSC3        SCONTO
017700870202     C     SCONTO        DIV       100           SCONTO                         *
017800870202     C     PRENET        SUB       SCONTO        PRENET                         *
017900860207     C     MOFQTA        MULT(H)   PRENET        IMPMER                         *
018000860207     C     IMPMER        MULT(H)   MOFCMB        IMPMER                         *
018100860207     C     VALOR2        ADD       IMPMER        VALOR2                         *
018200860207     C                   Z-ADD     PRENET        PRZMOF                         *
018300860208     C     TOTQTA        ADD       MOFQTA        TOTQTA                         *
018400860207     C*--- CARICAMENTO SUBFILE ----------------------------*
018500860207     C                   MOVE      *BLANKS       SCELTA                         *
018600860207     C                   Z-ADD     MOFQTA        QTAMOF                         *
018700860207     C     REC1          ADD       1             REC1                 71        *
018800860207     C                   WRITE     SFL1                                         *
018900860207     C                   SETOFF                                           93    *
019000860207     C                   GOTO      LEGGI                                        *
019100860207     C*--- EMISSIONE SUBFILE ------------------------------*
019200860207     C     EMISFL        TAG                                                    *
019300860207     C                   WRITE     OFN150T1                                     * TESTATA
019400860207     C                   WRITE     OFN150FD                                     * FONDO
019500860207     C                   EXFMT     SFL1CT                                       * SFL
019600990204     C   Kl              MOVE      'P'           RIC150                         * PROMPT INIZIO
019700990204     C   Kl              GOTO      FINE                                         *
019800860210     C*--- EMISSIONE STAMPA -------------------------------*
019900990409     C   Kh              DO                                                     *
020000860328     C                   OPEN      OFPRTSTD                             95      *
020100860328     C                   EXSR      RUTSTA                                       *
020200860328     C                   EXCEPT    TOTAL                                        *
020300860328     C                   MOVE      'S'           STA150                         *
020400860328     C                   GOTO      FINE                                         *
020500860328     C                   END                                                    *
020600860207     C*--- ESEGUO SCELTA ----------------------------------*
020700860207     C   71              READC     SFL1                                   92    *
020800860207     C   71
020900860207     CAN 92              GOTO      EMISFL                                       * EOF
021000860207     C     SCELTA        CABEQ     *BLANKS       EMISFL                         *
021100860207     C*--- SCELTO INTERROGAZIONE X FORNITORE --------------*
021200860207     C     SCELTA        IFEQ      'F'                                          *
021300860207     C                   MOVE      'F'           RIC150                         *
021400860207     C                   MOVE      MOFFOR        FOR150                         *
021500860207     C                   MOVEL     DESFFF        DESFOR                         *
021600860207     C                   GOTO      FINE                                         *
021700860207     C                   END                                                    *
021800860207     C*--- SCELTO REVISIONE ORDINE ------------------------*
021900860207     C     SCELTA        IFEQ      'O'                                          *
022000860207     C                   MOVE      'O'           RIC150                         *
022100860207     C                   MOVE      MOFFOR        FOR150                         *
022200990204     c                   if        mofdor > 0
022300990204     C                   move      MOfDOR        DATAIso                        *
022400990204     C                   move      dataiso       dataeur                        *
022500990204     C                   move      DATAeur       DOR150                         *
022600990204     c                   end
022700860207     C                   Z-ADD     MOFNOR        NOR150                         *
022800860207     C                   GOTO      FINE                                         *
022900860207     C                   END                                                    *
023000860207     C*--- SCELTO INTERROGAZIONE ORDINI -------------------*
023100860207     C     SCELTA        IFEQ      'I'                                          *
023200860207     C                   MOVE      'I'           RIC150                         *
023300860207     C                   MOVE      MOFFOR        FOR150                         *
023400860207     C                   Z-ADD     0             DOR150                         *
023500860207     C                   Z-ADD     0             NOR150                         *
023600860207     C                   GOTO      FINE                                         *
023700860207     C                   END                                                    *
023800860207     C*--- FINE LAVORO ------------------------------------*
023900860207     C     FINE          TAG                                                    *
024000860207     C                   SETON                                            LR    *
024100860210     C**--------- RUTINE EMISSIONE STAMPA ---------------***
024200860210     C***------------------------------------------------***
024300860210     CSR   RUTSTA        BEGSR                                                  *
024400860210     C                   Z-ADD     0             PAGE              3 0          *
024500860210     C                   Z-ADD     50            PAGE1             3 0          *
024600860210     C     1             DO        RECT          REC1              5 0          *
024700860210     C     REC1          CHAIN     SFL1                               91        *
024800860210     C   91              GOTO      ENDSTA                                       *
024900860210     C     PAGE1         IFGE      50                                           *
025000860210     C                   EXCEPT    TESTA                                        *
025100860210     C                   Z-ADD     0             PAGE1                          *
025200860210     C                   END                                                    *
025300990204     c                   if        mofdor > 0
025400990204     C                   move      MOfDOR        DATAIso                        *
025500990204     C                   move      dataiso       dataeur                        *
025600990204     C                   move      DATAeur       DATDOR            8 0          *
025700990204     c                   end
025800860210     C                   EXCEPT    DETAIL                                       *
025900860210     C     ENDSTA        TAG                                                    *
026000860210     C                   END                                                    *
026100860210     CSR                 ENDSR                                                  *
026200990204     C*----------------------------------------------------*
026300990204     C* Reperimento dati società
026400990204     C*----------------------------------------------------*
026500990204     C     REPSOC        BEGSR
026600990204     C*
026700990204     C                   CALLB     'XSOC'
026800990204     C                   PARM                    TIPXSC            6
026900990204     C                   PARM                    SOCXSC            3
027000990204     C                   PARM                    CDSXSC            9 0
027100990204     C                   PARM                    MODXSC            3
027200990204     C                   PARM      *blanks       RTNXSC            1
027300990204     C                   PARM                    XSOCDS
027400990204     C                   PARM                    KPJBA
027500990204     C*
027600990204     C                   ENDSR
027700860210     O*--------------------------------------------------------------*
027800860210     O*     TESTATA                                                  *
027900860210     O*--------------------------------------------------------------*
028000860210     OOFPRTSTD  E            TESTA            01
028100860210     O                       SK1                132
028200860210     O                       DESDIT              20
028300990209     O                       woggi              120 '  /  /    '
028400860210     O                       PAGE          Z    132
028500860210     O          E            TESTA       1
028600860210     O                       SK2                132
028700990204     O                       DAL                 87 '  /  /    '
028800990204     O                       AL                 101 '  /  /    '
028900860210     O          E            TESTA       1
029000860210     O                       SK3                132
029100860210     O                       KNMUS              132
029200860210     O          E            TESTA       2
029300860210     O                                           55 'Prodotto  :'
029400871006     O                       MAT150              68
029500871006     O                       DESKK1             100
029600860210     O          E            TESTA       0  0
029700871006     O                       MAT150              68
029800871006     O                       DESKK1             100
029900860210     O          E            TESTA       2
030000860210     O                       SK4                132
030100860210     O*--------------------------------------------------------------*
030200860210     O*     DETTAGLIO                                                *
030300860210     O*--------------------------------------------------------------*
030400860210     O          E            DETAIL      1
030500860210     O                       PAGE1              132
030600860210     O                       SK5                132
030700860210     O                       MOFFOR               9
030800860210     O                       FOPRSI              43
030900860210     O                       MOFNOR              51
031000990204     O                       DATDOR              62 '  /  /    '
031100990204     O                       MOFTOR              64
031200860210     O                       MOFFAS              68
031300860210     O                       MOFTRG              72
031400860210     O                       MOFUMQ              78
031500860210     O                       QTAMOF              88 '    0 ,  '
031600860210     O                       MOFVAL              95
031700990204     O                       PRZMOF             106 '       0 ,    '
031800990204     O               03      IMPMER             121 '       0 ,     -'
031900860210     O                       MOFMAG             130
032000860210     O*--------------------------------------------------------------*
032100860210     O*     TOTALE :                                                 *
032200860210     O*--------------------------------------------------------------*
032300860210     O          E            TOTAL       1
032400860210     O                       PAGE1              132
032500860210     O                       SK6                132
032600860210     O          E            TOTAL       1
032700860210     O                                           74 'TOTALE ORDINI : Quantità'
032800860210     O                       TOTQTA              88 '   .   . 0 ,  '
032900990209     O               03                         100 'Valore :'
033000990209     O               03      TOTVAL        2    120
033100860210     O          E            TOTAL       0  0
033200860210     O                       TOTQTA              88 '   .   . 0 ,  '
033300990209     O               03      TOTVAL        2    120
033400860210     O          E            TOTAL       1
033500860210     O                       SK6                132
033600860210     O*--------------------------------------------------------------*
033700860210**   SK1
033800990209*-DESDIT----------- ***-------------------------------------------SK01
033900860210-------------------------------------*** il XX/XX/XX      Pag. XXXSK01
034000860210**   SK2
034100860210                    ** Gest.Ord.Fornitori : INTERROG. GENERALE x PSK02
034200990317RODOTTO  dal xx/xx/xx al xx/xx/xx     **        PGM.:   TII401R2  SK02
034300860210**   SK3
034400860210                    ***-------------------------------------------SK03
034500860210-------------------------------------***        Utente  XXXXXXXXXXSK03
034600860210**   SK4
034700860210*-Fornitore--Descrizione--------------------*-Ordine---del--Tip-*FSK04
034800860210ase*TRg*-Um. Quantità--*----Prezzo Netto--Valore ------*-Magazz--*SK04
034900860210**   SK5
035000860210*                                           *                   * SK05
035100860210   *   *               *                               *         *SK05
035200860210**   SK6
035300860210*-------------------------------------------*-------------------*-SK06
035400860210---*---*---------------*-------------------------------*---------*SK06
