000100120305     /*PRM dbgview(*source)
000200120305     /*CMD ovrdbf file(FNBLT01L) tofile(FILTRAPRD/FNBLT01L) +
000300120305     /*CMD        ovrscope(*calllvl)
000400120305     /*END dltovr file(FNBLT01L) lvl(*)
000500120305     /*END
000600910926     H DECEDIT('0,') DATEDIT(*DMY.)
000700000822     H*  TRTR36R *---------------------------------------------------*
000800040326     H*  RICEZIONE IN SEDE DELLE BOLLE DA FILIALE    DAL 01/95       *
000900000000     H*--------------------------------------------------------------*
001000080424     Ffnblt01l  iF   E           K DISK    USROPN extfile(ovrdbf_blt)
001100080424     FFIBLPA0R  UF   E           K DISK    USROPN
001200000906     FTITAH00R  UF   E           K DISK    USROPN
001300000906     F                                     RENAME(TITAH000:TITAHR)
001400060220     FFiar400R  UF   E           K DISK    USROPN
001500030108     FFIAR500R  UF   E           K DISK    USROPN
001600030108     F                                     RENAME(FIAR5000:FIAR5R)
001700991103     FFIAR600R  UF   E           K DISK    USROPN
001800991103     FFIAR700R  UF   E           K DISK    USROPN
001900950116     FFNARBM0E  O    E             DISK
002000991103     FFIBLPA0E  O    E             DISK
002100991103     F                                     RENAME(FIBLPA00:FIBLPEA)
002200030108     FFIAR500E  O    E             DISK
002300030108     F                                     RENAME(FIAR5000:FIAR5E)
002400991103     FFIAR600E  O    E             DISK
002500991103     F                                     RENAME(FIAR6000:FIAR6EC)
002600991103     FFIAR700E  O    E             DISK
002700991103     F                                     RENAME(FIAR7000:FIAR7E)
002800060220     FFiar400E  O    E             DISK
002900060220     F                                     RENAME(Fiar4000:Fiar4E)
003000000906     FTITAH00E  O    E             DISK
003100000906     F                                     RENAME(TITAH000:TITAHE)
003200000823     FFIARBE2C  IF A E           K DISK
003300000823     F                                     IGNORE(FNARBD00)
003400000823     F                                     RENAME(FIARBT00:FIARBTEE)
003500000823     F                                     IGNORE(FNARBK00)
003600000823     F                                     IGNORE(FNARBM00)
003700000823     F                                     IGNORE(FNARBG00)
003800941114     F*
003900000627     FTITAS30C  UF A E           K DISK
004000990621     FTITA430C  UF A E           K DISK
004100990621     FTITAA30C  UF A E           K DISK
004200990621     FTITA730C  UF A E           K DISK
004300991104     FTITAT30C  IF A E           K DISK
004400000906     FTITAH00F  O  A E             DISK
004500080828     FFIAR531c  uf a E           k DISK
004600941114     FTNCSB03L  UF A E           K DISK
004700000628     FTNCSA01L  UF A E           K DISK
004800941114     F*
004900000000     FTABEL00F  IF   E           K DISK
005000000627     FTNTBE01L  IF   E           K DISK
005100900426     FAZORG01L  IF   E           K DISK
005200030401     fFiapd01l  if   e           k Disk
005300061211     ffnlbl01l  uf   e           k Disk
005400061211     ffnlbl02l  if   e           k Disk
005500041119     F                                     RENAME(FNlbl000:FNlbl002)
005600950118     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
005700950118     F                                     USROPN
005800040326     FPRTF198X  O    F  198        PRINTER OFLIND(*INOE)
005900040326     F                                     USROPN
006000911105     D CMA1            S             55    DIM(1) CTDATA PERRCD(1)
006100911105     D CMA2            S             55    DIM(1) CTDATA PERRCD(1)
006200911105     D CMA3            S             55    DIM(1) CTDATA PERRCD(1)
006300911105     D CMC1            S             55    DIM(1) CTDATA PERRCD(1)
006400911105     D CMC2            S             55    DIM(1) CTDATA PERRCD(1)
006500911105     D CMC3            S             55    DIM(1) CTDATA PERRCD(1)
006600900526     D CMD1            S             55    DIM(1) CTDATA PERRCD(1)
006700900526     D CMD2            S             55    DIM(1) CTDATA PERRCD(1)
006800900526     D CMD3            S             55    DIM(1) CTDATA PERRCD(1)
006900970212     D CME1            S             55    DIM(1) CTDATA PERRCD(1)
007000970212     D CME2            S             55    DIM(1) CTDATA PERRCD(1)
007100970212     D CME3            S             55    DIM(1) CTDATA PERRCD(1)
007200000906     D CMH1            S             55    DIM(1) CTDATA PERRCD(1)
007300000906     D CMH2            S             55    DIM(1) CTDATA PERRCD(1)
007400000906     D CMH3            S             55    DIM(1) CTDATA PERRCD(1)
007500111117     D $CMD            S             56    DIM(5) CTDATA PERRCD(1)
007600041119     d
007700041119     drecordlbl      E DS                  extname(fnlbl00f)
007800930222     D*
007900000628     D ER              S             41    DIM(17) CTDATA PERRCD(1)             ERRORI
008000941128     D S               S             66    DIM(12) CTDATA PERRCD(1)             COSTANTI STAMPA
008100000627     D SDL             S              1    DIM(100)                             T.REK.CANCELL.
008200000628     D DVA             S              3    DIM(10)                              DIVISE ACC.ITAA
008300011029     D DEC             S              2  0 DIM(10)                              DIVISE ACC.DECA
008400000629     D MSG             S             70    DIM(13) CTDATA PERRCD(1)             MSG POSTA ELET
008500001002     D TBL             S              2    DIM(50)
008600000823     D EAS             S             35    DIM(1) CTDATA PERRCD(1)              ERRORE UFF.ASSIC.MER
008700030128      *
008800030128     d AR5n            s                   like(AR5trd)  dim(50)
008900000628     D*
009000030128     D KPJBA         E DS
009100030128     D  LEGFIL               483    492
009200030128     D  MBRFIL               493    502
009300030128     D  WMBR                 494    496  0
009400030128     D*
009500030128     D DSYEUR        E DS                  EXTNAME(YEURCODS)
009600030128      * DS PER TIBS02R
009700030128     D TIBS02        E DS                  EXTNAME(TIBS02DS)
009800030128     D DSLV55        E DS                  EXTNAME(FNLV55DS)
009900030128     D UT§DSE0F      E DS
010000030128     D CNCR80        E DS
010100030128     D DSBS69        E DS                  EXTNAME(TIBS69DS)
010200030128     D DSACO         E DS                  EXTNAME(CNACO00F)
010300030128     D DSIND         E DS                  EXTNAME(CNIND00F)
010400030128     D DSCLP         E DS                  EXTNAME(CNCLP00F)
010500030128     D DSCLS         E DS                  EXTNAME(FNCLS00F)
010600030128     D DSCW          E DS
010700931231     D DS3A          E DS
010800941114     D DS1P          E DS
010900950201     D DS1A          E DS
011000941129     D DSPR          E DS
011100000628     D DSCV          E DS
011200000822     D DSTB          E DS
011300991104     D*
011400080828     D Dcli          E DS
011500080828     D DGEI          E DS
011600000627     D DTA4          E DS
011700050531     D DTA4C         E DS
011800991104     D DTA4A         E DS
011900030128     d*
012000151113     d dsbl4m        E DS
012100030128     d dAR5          e ds                  inz
012200030401     d dtasflo       e ds
012300150219      *--------------------------
012400150219     d Fiar5ds       e ds                  extname(FIAR531C)
012500150219     d TISIG20DS     e ds
012600150219      *--------------------------
012700020311
012800020311     d og143         e ds
012900040603     d dar5gen       e ds
013000130322     d dar5emd       e ds
013100151119     d dar5fat       e ds
013200050222     d fidn58ds      e ds
013300110519     d TRUL21DS      e ds
013400020311
013500970714     D* DS PER TISI95R - CONTROLLO CAP
013600970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
013700030128      *
013800950227     D* PER ESPLOSIONE DATI IN RICEZIONE
013900950227     D PARAM5          DS
014000950227     D  PA5FIL               237    246    INZ('FXXX999999')
014100950227     D  PA5MBR               247    250    INZ('MXXX')
014200950227     D  BLKFIL               251    256    INZ
014300030128      *
014400920305     D TCUDS           DS
014500920305     D  F1                     1      1
014600920305     D  F3                     3      3
014700920305     D  F2                     2      2
014800920305     D  F4                     4      4
014900920305     D  F56                    5      6
015000970327     D                 DS
015100970327     D  BLPAAS                 1      4  0
015200970327     D  BLPMGS                 5      8  0
015300970327     D  BLPDSP                 1      8  0
015400030128      *
015500920324     D  TCU                  398    697
015600920305     D                                     DIM(50)                              TAB FLAG CNT
015700920324     D  KCU                  698    847P 0
015800920305     D                                     DIM(50)                              TAB CAPO CONTO
015900920305     D                                     PACKEVEN
016000030108      *
016100030108     D WLBDAT          DS                  INZ
016200030108     D  G02DAT                 1      8  0
016300030108     D  G02INV                 9     16  0
016400030108     D  G02ERR                17     17
016500030108     D  G02TGI                18     22  0
016600030128      *
016700030128     d Status         sds           333
016800030128     d  SDSpgm           *proc
016900030108      *
017000030108     D W0140           S             14  0
017100030108     D DATEU           S              8  0
017200030128     D TimeU           s              6  0
017300030401     d kApdTip         s                   Like(ApdTip) Inz('A')
017400030401     d kApdPdr         s                   Like(ApdPdr)
017500110607     d kTRD            s                   Like(ar5trd)
017600030108     D NRec5           s              7  0 inz
017700030128     d SAVjba          s                   like(KPJBA) inz
017800080424     d libr            s              3
017900080424     d Ovrdbf_blt      s             21
018000151113     d wb4ado          s                   like(§b4ado)
018100000628     D*
018200000628     D COS1            C                   CONST('PAGATO              ')
018300000628     D COS2            C                   CONST('INCASSATO           ')
018400000628     D COS3            C                   CONST('NON PAGATO/INCASSATO')
018500011022      *
018600000627     ITITAS000      05
018700000627     ITITAS010      06
018800000627     ITITASP00      07
018900000627     ITITAA000      08
019000000627     ITITAAP00      09
019100000627     ITITA4000      08
019200000627     ITITA4P00      09
019300000000     C*---------------------------------------------------------------*
019400941114     C*  RIEPILOGO INDICATORI
019500941114     C*---------------------------------------------------------------*
019600991103     C* 02    - LETTURA FIBLPA0R
019700970212     C* 10/13 - ESITONO RECORD PER 1 4 FILE DI ESTENSIONE DELLE BOLLE
019800941114     C* 12    - ON NON ESISTE LA ANAGRAFICA C/A
019900950127     C* 15    - ERRORE SU ANAGRAFICA C/A
020000950117     C* 16    - ERRORE DA SEGNALARE IN FILIALE
020100941129     C* 20    - CONTROLLO ESISTENZA FILE RICEZ: SE NON ESISTONO ESCO
020200941129     C* 21    - CONTROLLO ESISTENZA FILE RICEZ: SE NON ESISTONO NON LI
020300941129     C*         APRO MA ELABORO
020400941114     C* 30    - DI COMODO
020500950127     C* 51    - ERRORE VINCOLANTE NELLA FASE DI CONTROLLO
020600000627     C* 52    - RICEZIONE AGGIORNAMENTO DI BOLLA AUTOGENERATA
020700941114     C* 58    - ON INDICA CHE IN C/A E' SULLA 2 BOLLA
020800941114     C* 59    - ON INDICA CHE C'E' LA 2 BOLLA
020900950112     C* 60    - DI COMODO LETTURA
021000991103     C* 73    - OFF ESISTE FIAR600R
021100030108     C* 91    - APERTURA FILE FIAR500R
021200991103     C* 93    - APERTURA FILE FIAR600R
021300000906     C* 95    - APERTURA FILE TITAH00R
021400060220     C* 96    - APERTURA FILE Fiar400R
021500991103     C* 97    - APERTURA FILE FIAR700R
021600950116     C* 98    - ERRORE: NON CANCELLO LA BOLLA DAI FILES DI RICEZIONE
021700950127     C* 98              E NON SCRIVO NEI FILES DEGLI ERRORI
021800030128     C* 99    - FILE PRTF198 APERTO
021900941114     C*---------------------------------------------------------------*
022000941114     C* APERTURA MEMBRO DA TRASCODIFICARE
022100941114     C                   EXSR      CAEXA
022200941129     C*
022300941114     C* 20 ON - APERTURA FILE NON RIUSCITA
022400030416     c                   if        *in20 = *on
022500030416     c                   exsr      SendMSG
022600041005     c                   goto      STOP
022700030416     c                   endif
022800941129     C*
022900941129     C* DECODIFICO IL MEMBRO DA STAMPARE
023000941129     C     WMBR          CHAIN     AZORG                              30
023100941129     C  N30              MOVEL     ORGDES        DESFIL           20
023200941129     C   30              MOVEL     *BLANKS       DESFIL
023300900528     C**
023400000000     C     START         TAG
023500991103     C                   READ      FIBLPA0R                               02
023600900613     C   02              GOTO      STOP
023700941114     C*
023800900530     C                   ADD       1             NREC              7 0
023900941114     C* PULIZIA CAMPI
024000991108     C                   EXSR      PULAR6
024100920304     C*
024200920304     C*
024300911107     C   93              DO
024400991103     C                   MOVEL     '1'           WTRC
024500991103     C     KASS1         CHAIN     FIAR600R                           73
024600911107     C  N73              ADD       1             NRECC             7 0
024700911107     C                   END
024800900528     C**
024900900528     C                   EXSR      CTRNSP
025000900528     C*
025100900530     C* CANCELLA IL RECORD RICEVUTO
025200970227    1C     *IN98         IFEQ      *OFF
025300991103     C                   DELETE    FIBLPA0R
025400991109     C     *IN93         IFEQ      *ON
025500991109     C     KASS          SETLL     FIAR600R
025600991109     C     KASS          READE     FIAR600R                               32
025700991109    3C     *IN32         DOWEQ     *OFF
025800991109     C                   DELETE    FIAR6000
025900991109     C     KASS          READE     FIAR600R                               32
026000991109    3C                   ENDDO
026100991109     C                   ENDIF
026200000906     C*
026300000906    2C     *IN95         IFEQ      *ON
026400000906     C     KASS          SETLL     TITAH00R
026500000906     C     KASS          READE     TITAH00R                               32
026600000906    3C     *IN32         DOWEQ     *OFF
026700000906     C                   DELETE    TITAHR
026800000906     C     KASS          READE     TITAH00R                               32
026900000906    3C                   ENDDO
027000000906    2C                   ENDIF
027100970227     C*
027200970227    2C     *IN96         IFEQ      *ON
027300060220     C     KASS          SETLL     Fiar400R
027400060220     C     KASS          READE     Fiar400R                               32
027500970227    3C     *IN32         DOWEQ     *OFF
027600060220     C                   DELETE    Fiar4000
027700060220     C     KASS          READE     Fiar400R                               32
027800970227    3C                   ENDDO
027900970227    2C                   ENDIF
028000990621    2C     *IN97         IFEQ      *ON
028100991103     C     KASS          SETLL     FIAR700R
028200991103     C     KASS          READE     FIAR700R                               32
028300990621    3C     *IN32         DOWEQ     *OFF
028400991103     C                   DELETE    FIAR7000
028500991103     C     KASS          READE     FIAR700R                               32
028600990621    3C                   ENDDO
028700990621    2C                   ENDIF
028800030108    2C                   If        *in91
028900030108     C     KASS          setll     FIAR500R
029000030108     C     KASS          reade     FIAR500R                               32
029100030108    3C                   Dow       Not *in32
029200030108     C                   DELETE    FIAR5R
029300030108     C     KASS          reade     FIAR500R                               32
029400030108    3C                   Enddo
029500030108    2C                   Endif
029600970227    1C                   ENDIF
029700900530     C*
029800000000     C                   GOTO      START
029900900613     C     STOP          TAG
030000970714     C*
030100970714     C                   CLEAR                   DSSI95
030200970714     C                   MOVEL     'C'           I95TLA
030300970714     C                   CALL      'TISI95R'
030400970714     C                   PARM                    DSSI95
030500971202     C**
030600971202     C                   CLEAR                   DSLV55
030700971202     C                   MOVEL     'C'           D55TLA
030800971202     C                   CALL      'FNLV55R'
030900971202     C                   PARM                    DSLV55
031000941202     C*
031100981001     C                   CLEAR                   DSBS69
031200981001     C                   MOVEL     'C'           I69TLA
031300981001     C                   CALL      'TIBS69R'
031400981001     C                   PARM                    DSBS69
031500981001     C                   PARM                    DSACO
031600981001     C                   PARM                    DSIND
031700981001     C                   PARM                    DSCLP
031800981001     C                   PARM                    DSCLS
031900150219     C**
032000150219     C                   CLEAR                   TISIG20DS
032100150219     C                   MOVEL     'C'           i20TLA
032200150219     C                   eval       KPJBU = TISIG20DS
032300150318     C                   CALL      'TISIG20R'                           77
032400150219     C                   PARM                    KPJBA
032500000628     C*
032600000000     C                   SETON                                        LR
032700000000     C**----------------------------------------
032800900529     C** CONTROLLO DATI  RICEVUTI
032900000000     C**----------------------------------------
033000900528     C     CTRNSP        BEGSR
033100941114     C                   SETOFF                                       585951
033200941129     C                   SETOFF                                       981516
033300000627     C                   SETOFF                                       52
033400900530     C* CODICE BOLLA
033500900530     C                   MOVE      '3A'          COD                             COD.BOLLA
033600900530     C                   MOVE      *BLANK        KEY
033700950114     C                   MOVEL     BLPCBO        KEY
033800900601     C     KTAB          CHAIN     TABEL                              51
033900900531     C  N51TBLFLG        COMP      '*'                                    51     ANN.TA
034000941129     C   51              MOVEL     ER(1)         ERR              41
034100900531     C   51              GOTO      ENDER
034200941114     C*
034300900530     C                   MOVEL     TBLUNI        DS3A
034400941114     C*
034500000822     C* IMPOSTO FLAG CHE MI INDICA SE E' BOLLA DI RECUPERO:
034600000822     C* WRBL='R' E' DI RECUPERO.
034700000822     C                   MOVEL     §3ARBL        WRBL              1
034800000822     C*
034900000822     C* IMPOSTO CODICE BOLLA DEL CONTRASSEGNO
035000941114     C     §3AFCA        COMP      2                                      58     C/A SU 2°
035100941114     C*
035200900601     C                   MOVEL     §3ATB1        §TBCA             2
035300900601     C   58              MOVEL     §3ATB2        §TBCA
035400900604     C                   MOVEL     §TBCA         §TIBO             1
035500941114     C**
035600000627     C* BOLLA GIA' ESISTENTE SU TITAS
035700941114     C**
035800941114     C                   SETOFF                                       51
035900941114     C*
036000000627     C     KNSP          CHAIN(N)  TITAS30C                           30
036100941209     C  N30              SETON                                        51
036200000627     C* 51 ON - GIA' ESISTENTE - ERRORE SOLO SE BOLLA NON AUTOGENERATA
036300941114     C     *IN51         IFEQ      *ON
036400000627     C     TASSOP        ANDNE     '&'
036500000627     C     *IN51         OREQ      *ON
036600000627     C     BLPSOP        ANDNE     '&'
036700010109     C* Asteriscole specifiche per errore se bolla autogenerata già con
036800010109     C* tabilizzata perchè in questo caso devo sempre caricare
036900010109     C* (Tale esigenza è nata dal fatto che i contrassegni devono esser
037000010109     c*  sempre caricati a prescindere dalla contabilizzazione)
037100010109     C***        *IN51     OREQ *ON
037200010109     C***        TASFIC    ANDEQ'1'
037300010109     C***        TASSOP    IFEQ '&'
037400010109     C***        BLPSOP    ANDEQ'&'
037500010109     C***                  MOVELER,17     ERR
037600010109     C***                  SETON                     98
037700010109     C***                  ELSE
037800941114     C                   MOVEL     ER(2)         ERR
037900010109     C***                  ENDIF
038000941114     C                   GOTO      ENDER
038100941114     C                   ENDIF
038200000627     C* MEMORIZZO CHE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA
038300000627     C   51              SETON                                        52
038400000627     C*
038500000627     C* LE SEGUENTI ISTRUZIONI LE ESEGUO SOLO PER 51 OFF
038600000627     C* INFATTI SE 51 ON SIGNIFICA CHE STO RICEVENDO AGGIORNAMENTO
038700000627     C* DI BOLLA AUTOGENERATA
038800000627   1AC     *IN51         IFEQ      *OFF
038900991104     C**
039000991104     C**
039100991104     C* DETTAGLIO SEGNACOLLI GIA ESISTENTE SU TITAT
039200991104     C**
039300991104     C     KNSP          CHAIN     TITAT30C                           30
039400991104     C  N30              SETON                                        51
039500991104     C* 51 ON - GIA' ESISTENTE
039600991104     C     *IN51         IFEQ      *ON
039700991104     C                   MOVEL     ER(15)        ERR
039800991104     C                   GOTO      ENDER
039900991104     C                   ENDIF
040000990621     C**
040100990621     C* ANAGRAFICA MITTENTE GIA ESISTENTE SU TITAA
040200990621     C**
040300990621     C                   MOVEL     'M'           §TRC
040400990621     C     KNSPE         CHAIN     TITAA30C                           30
040500990621     C  N30              SETON                                        51
040600990621     C* 51 ON - GIA' ESISTENTE
040700990621     C     *IN51         IFEQ      *ON
040800990621     C                   MOVEL     ER(12)        ERR
040900990621     C                   GOTO      ENDER
041000990621     C                   ENDIF
041100991104     C**
041200991104     C**
041300991104     C* MITTENTE ORIGINALE GIA ESISTENTE SU TITAA
041400991104     C**
041500991104     C                   MOVEL     'O'           §TRC
041600991104     C     KNSPE         CHAIN     TITAA30C                           30
041700991104     C  N30              SETON                                        51
041800991104     C* 51 ON - GIA' ESISTENTE
041900991104     C     *IN51         IFEQ      *ON
042000991104     C                   MOVEL     ER(16)        ERR
042100991104     C                   GOTO      ENDER
042200991104     C                   ENDIF
042300990621     C**
042400990621     C* RIFERIMENTI BOLLA GIA ESISTENTI SU TITA4
042500990621     C**
042600061222     C**   KNSP          CHAIN     TITA430C                           30
042700061222     c* Accetto come già esistente solo il record M e Q che può arrivare
042800041105     c*  prima del record bolla
042900061222     c**                 if        (not *in30 and ta4trc='M') or
043000061222     c**                           (not *in30 and ta4trc='Q')
043100061222     C**   KNSP          reade     TITA430C                               30
043200061222     c**                 endif
043300041105     c*
043400061222     C**N30              SETON                                        51
043500990621     C* 51 ON - GIA' ESISTENTE
043600061222     C**   *IN51         IFEQ      *ON
043700061222     C**                 MOVEL     ER(13)        ERR
043800061222     C**                 GOTO      ENDER
043900061222     C**                 ENDIF
044000990621     C**
044100990621     C* VARIE GIA PRESENTI  SU TITA7
044200990621     C**
044300990621     C     KASS          CHAIN     TITA730C                           30
044400990621     C  N30              SETON                                        51
044500990621     C* 51 ON - GIA' ESISTENTE
044600990621     C     *IN51         IFEQ      *ON
044700990621     C                   MOVEL     ER(14)        ERR
044800990621     C                   GOTO      ENDER
044900990621     C                   ENDIF
045000000627   1AC                   ENDIF
045100900531     C*
045200900531     C** SPEDIZIONE CON CONTRASSEGNO
045300900531     C                   SETOFF                                       1251
045400991108    1C     BL9CAS        IFGT      0
045500941114     C     KASS          CHAIN     TNCSB000                           12
045600991108     C* N12      CSBFLE    IFNE 046
045700991108    2C     *IN12         IFEQ      *OFF
045800000627     C     *IN52         ANDEQ     *OFF
045900900531     C                   MOVEL     ER(7)         ERR
046000941129     C                   EXSR      ERROR
046100991108    2C                   END
046200950112     C** SE LA SPEDIZIONE ESISTE SU TNCSB CON STESSO TIPO BOLLA
046300000000     C** DISTINGUO DUE FASI
046400950116     C** IMPORTO UGUALE :   OK !!
046500950116     C** IMPORTO DIVERSO:   ERRORE !!!!!!!!!
046600950112     C** CONTROLLO CONTRASSEGNO SU TNCSB
046700991108    1C                   END
046800900530     C* CREA RECORDS BOLLE
046900900530     C                   EXSR      CRENSP
047000900530     C                   GOTO      ENDCTR
047100900530     C*
047200900530     C     ENDER         TAG
047300900530     C                   EXSR      ERROR
047400900530     C     ENDCTR        ENDSR
047500900528     C/SPACE 005
047600000000     C**----------------------------------------
047700990621     C** CREAZIONE BOLLA SU TITAS
047800000000     C**----------------------------------------
047900900530     C     CRENSP        BEGSR
048000941129     C                   SETOFF                                       511516
048100941126     C*
048200900531     C* CONTROLLO SE ESISTE DOPPIA BOLLA
048300900531     C     §3ATB2        COMP      *BLANK                             59         SI 2° BOLLA
048400941114     C*
048500900601     C* CONTROLLO DOVE VA MESSO IL CONTRASSEGNO
048600900531     C     §3AFCA        COMP      2                                      58     SU 2°
048700941114     C*
048800900601     C                   MOVE      BLPKSC        KSC4              4 0           COD.CLI
048900920304     C*
049000920304     C                   MOVEL     §3ATB1        FLGTB1            1
049100950120     C**
049200950120     C* CONTROLLO CAP MITTENTE SU CAPPARIO
049300950120     C**
049400970714     C                   CLEAR                   DSSI95
049500970714     C                   MOVEL     BLPCAM        I95CAP
049600970714     C                   MOVEL     BLPNZM        I95NAR
049700970714     C                   MOVEL     BLPDSP        I95DAT
049800970714     C                   MOVEL     '3'           I95TCN
049900020311      * Se network FedEx passo flag apposito per dire di prendere i dati
050000020311      * dalla tabella della nazione
050100020311     c     blplnp        chain     azorg01l
050200020311     c                   if        %found(azorg01l) and orgfva = *blanks
050300020311     c                   eval      og143 = orgde3
050400020311     c     §ogntw        ifeq      'FED'
050500020311     c                   eval      i95fi1 = 'S'
050600020311     c                   endif
050700020311     c                   endif
050800020311
050900970714     C                   CALL      'TISI95R'
051000970714     C                   PARM                    DSSI95
051100970714     C     O95ERR        IFNE      *BLANKS
051200000629     C     BLPSOP        IFNE      '&'
051300950120     C                   MOVEL     ER(5)         ERR
051400000629     C                   ELSE
051500000629     C                   MOVEL     ER(6)         ERR
051600000629     C                   ENDIF
051700950120     C                   MOVEL     BLPNZM        ER05             13
051800950120     C                   MOVE      BLPCAM        ER05
051900950120     C                   EXSR      ERROR
052000950120     C                   MOVE      *BLANK        ER05
052100000627     C* Se bolla autogenerata carico lo stesso
052200000627     C     BLPSOP        IFNE      '&'
052300950120     C                   SETON                                        5198
052400000627     C                   ENDIF
052500950120     C                   ENDIF
052600950120     C* SE FRANCO CODIFICATO
052700950120     C* O ASSEGNATO CON CLIENTE MITTENTE CODIFICATO
052800950120    1C     FLGTB1        IFEQ      'F'
052900950120     C     KSC4          ANDNE     8888
053000950120    1C     FLGTB1        OREQ      'A'
053100950120     C     BLPCCM        ANDGT     0
053200941128     C*
053300950120    1C     FLGTB1        IFEQ      'F'
053400950120     C                   MOVEL     BLPKSC        WKSC
053500950120     C                   ELSE
053600981001     C                   MOVEL     BLPCCM        WKSC              7 0
053700950120     C                   ENDIF
053800950117     C* CONTROLLO CLIENTE
053900981001     C                   CLEAR                   DSBS69
054000981001     C                   MOVEL     KCI           I69KCC
054100981001     C                   MOVEL     KNSIF         I69SIF
054200981001     C                   MOVEL     WKSC          I69KAC
054300981001     C                   MOVEL     WKSC          I69KIN
054400981001     C                   CALL      'TIBS69R'
054500981001     C                   PARM                    DSBS69
054600981001     C                   PARM                    DSACO
054700981001     C                   PARM                    DSIND
054800981001     C                   PARM                    DSCLP
054900981001     C                   PARM                    DSCLS
055000941128     C*
055100960918     C* CLIENTE BLOCCATO: CARICO LO STESSO MA SEGNALO
055200111031     c* 03/03/10 ES --> non segnalo più l'errore
055300100303    2C***  O69ERR        IFEQ      ' '
055400100303     C***  ACOABL        ANDEQ     '8'
055500100303     C***                MOVEL     ER(11)        ERR
055600100303     C***                SETON                                        16
055700100303     C***                EXSR      ERROR
055800100303     C***                SETOFF                                       16
055900100303    2C***                ENDIF
056000950117     C*
056100111031     C* MANCA P.D.C.:
056200991104     C* AGGIORNO E SCRIVO PER SEGNALAZIONE A FILIALE X CORREZIONE
056300991104     C     O69ERR        IFNE      ' '
056400950117     C                   MOVEL     ER(3)         ERR
056500950222     C                   SETON                                        16
056600950117     C                   EXSR      ERROR
056700950117     C                   SETOFF                                       16
056800950117     C                   ENDIF
056900941126     C**
057000941128    1C                   END
057100941126     C**
057200950117     C* SE NON CI SONO ERRORI VINCOLANTI CREO COMUNQUE LA BOLLA
057300000822     C* -------------------------------------------------------
057400941126     C     *IN51         IFEQ      *OFF
057500000822     C*
057600000822     C* SOLAMENTE SE VERRÀ CREATA LA BOLLA,
057700000822     C*               SE:
057800000822     C* - NON E' BOLLA DI RECUPERO     (a)
057900000822     C* - HA UN IMPORTO DA ASSICURARE  (b)
058000000822     C* - MA NON E' ASSICURABILE       (c)
058100001002     C* VERRÀ SCRITTO IL FILE DEGLI ERRORI
058200000822     C* CHE SEGNALERÀ L'INCONGRUENZA ALL'UFFICIO ASSICURAZIONE MERCI.
058300100225+--- C***  WRBL          IFNE      'R'                                          (a)
058400100225+--- C***  BLPIAS        ANDGT     0                                            (b)
058500100225     C***  §3ATB1        LOOKUP    TBL                                    38
058600001002     C*
058700100225|+-- C***  *IN38         IFEQ      *ON                                          (c)
058800100225||   C***                EXSR      SCRASS
058900000822||   C*                    -----------
059000100225|+-- C***                ENDIF
059100100225+--- C***                ENDIF
059200941126     C**
059300931231     C* C R E A   1 °   B O L L A :
059400900530     C*
059500000627     C* AZZERO CAMPI DI TITAS SOLO SE BOLLA NON PRESENTE
059600000627     C  N52              CLEAR                   TITAS000
059700010116     C*
059800000627     C     *IN52         IFEQ      *ON
059900000627     C                   CLEAR                   WESCSB
060000000627     C                   MOVE      §3ATB1        WTBL
060100000627     C* N.B.:
060200000628     C* Chaino sempre con tipo bolla dando per scontato che non cambia
060300000628     C* Visto che do anche per scontato che per le bolle autogenerate
060400000628     C* non ci sia mai la seconda bolla avrei anche potuto tenere
060500000628     C* buona la chain fatta nella routine CTRNSP
060600000628     C                   SETOFF                                       050607
060700000627     C     KNSP1         CHAIN     TITAS30C                           53
060800000627     C  N53KASS          CHAIN     TNCSB000                           32
060900000627     C  N53
061000000627     CANN32CSBSTA        IFNE      9
061100000627     C                   MOVE      '1'           WESCSB            1
061200000627     C                   ENDIF
061300000627     C                   ENDIF
061400990615     C* TITAS
061500941114     C* RIEMPO I CAMPI CHE SONO UGUALI PER LA 1 E LA 2 BOLLA
061600990817     C                   MOVEL     §3ATB1        WFLGTB            1
061700941114     C                   EXSR      RIETAS
061800991104     C* SE SEGNACOLLI NON SEQUENZIALI RICEVO ANCHE IL DETTAGLIO
061900080424     C     TASFNS        ifeq      'S'
062000080424     C     KASS          SETLL     FNBLT01l
062100080424     C     KASS          READE     FNBLT01l                               32
062200991104     C     *IN32         DOWEQ     *OFF
062300991104     C                   CLEAR                   TITAT000
062400991104     C                   Z-ADD     BLTAAS        TATAAS
062500991104     C                   Z-ADD     BLTLNP        TATLNP
062600991104     C                   Z-ADD     BLTNRS        TATNRS
062700991104     C                   Z-ADD     BLTNSP        TATNSP
062800991104     C                   Z-ADD     BLTLNA        TATLNA
062900991104     C                   Z-ADD     BLTFLS        TATFLS
063000991104     C                   Z-ADD     BLTNSC        TATNSC
063100991104     C                   WRITE     TITAT000
063200080424     c
063300080424     C     KASS          READE     FNBLT01l                               32
063400991104     C                   ENDDO
063500991104     C                   ENDIF
063600000906     C* RICEVO EVENTUALE ESTENSIONE SEGNACOLLI
063700000906     C     *IN95         IFEQ      *ON
063800000906     C     KASS          SETLL     TITAH00R
063900000906     C                   DO        *HIVAL
064000000906     C     KASS          READE     TITAH00R                               32
064100000906     C   32              LEAVE
064200000907     C                   WRITE     TITAH000                             38      IGNORO SE GIA' PRESE
064300000906     C                   ENDDO
064400000906     C                   ENDIF
064500941114     C*
064600941114     C                   MOVE      §3ATB1        TASTBL
064700000628     C* SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA AGGIORNO
064800010109     C* TASKSC SOLO SE BOLLA FRANCO E NON CONTABILIZZATA
064900000627     C     *IN52         IFEQ      *OFF
065000000627     C     WFLGTB        OREQ      'F'
065100010109     C     TASFIC        ANDEQ     ' '
065200941114     C                   Z-ADD     BLPKSC        TASKSC
065300000627     C                   ENDIF
065400941114     C*
065500950114     C                   MOVEL     BLPTC1        TASFTC
065600950118     C                   MOVEL     BLPTC2        TASTC2
065700991104     C                   MOVEL     BLPTCR        TASTCR
065800991104     C                   Z-ADD     BLPDCR        TASDCR
065900991104     C                   Z-ADD     BLPHCR        TASHCR
066000991104     C                   MOVEL     BLPFST        TASLL1
066100941114     C*
066200900601     C* CONTRASSEGNO A SECONDA DEL FLAG
066300010116     C  N58BL9CAS        IFGT      0
066400941114     C                   MOVEL     'S'           TASFCA
066500941114     C                   ENDIF
066600941114     C*
066700941114     C                   MOVE      BLPCTR        TASCTR
066800950114     C                   Z-ADD     BLPIAS        TASIAS
066900950114     C                   MOVEL     BLPVAS        TASVAS
067000930222     C                   MOVEL     BLPFTM        TASFTM
067100931231     C*
067200990618     C* SE BOLLA FRANCO NON CODIFICATO SCRIVO ANAGRAFICA DEL MITTENTE
067300990618     C* OPPURE SE E' ASSEGNATO CON MITTENTE NON CODIFICATO
067400010109     C* SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA GIA' CONTA
067500010109     C* BILIZZATA LE SEGUENTI OPERAZIONI NON LE DEVO MAI FARE DAL MOMEN
067600010109     C* CHE IN QUESTO CASO NON VARIO MAI TASKSC
067700990618     C     FLGTB1        IFEQ      'F'
067800990618     C     KSC4          ANDEQ     8888
067900010109     C     *IN52         ANDEQ     *OFF
068000010109     C     FLGTB1        OREQ      'F'
068100010109     C     *IN52         ANDEQ     *ON
068200010109     C     TASFIC        ANDEQ     ' '
068300010109     C     KSC4          ANDEQ     8888
068400010109     C*
068500990618     C     FLGTB1        OREQ      'A'
068600990618     C     BLPCCM        ANDEQ     0
068700000627     C                   SETON                                        32
068800000627     C   52              MOVEL     'M'           §TRC
068900000627     C   52              SETOFF                                       0809
069000000627     C   52KNSPE         CHAIN     TITAA30C                           32
069100000627     C   32              CLEAR                   TITAA000
069200990618     C                   Z-ADD     BLPAAS        TAAAAS
069300990618     C                   Z-ADD     BLPLNP        TAALNP
069400990618     C                   Z-ADD     BLPNRS        TAANRS
069500990618     C                   Z-ADD     BLPNSP        TAANSP
069600990618     C                   MOVEL     'M'           TAATRC
069700990618     C                   MOVEL     BLPRSM        TAARSC
069800990618     C                   MOVEL     BLPINM        TAAIND
069900990618     C                   MOVEL     BLPCAM        TAACAP
070000990618     C                   MOVEL     BLPLOM        TAALOC
070100990618     C                   MOVEL     BLPPRM        TAAPRV
070200990618     C                   MOVEL     BLPNZM        TAANAZ
070300990618     C     FLGTB1        IFEQ      'F'
070400990621     C                   MOVEL     BLPCPI        TAACPI
070500990618     C                   END
070600000627     C   32              WRITE     TITAA000
070700000627     C  N32
070800000627     CAN 08              UPDATE    TITAA000
070900000627     C  N32
071000000627     CAN 09              UPDATE    TITAAP00
071100000627     C                   ELSE
071200000627     C     *IN52         IFEQ      *ON
071300010109     C     TASFIC        ANDEQ     ' '
071400000627     C* SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA
071500000627     C* CANCELLO RECORD SU TITAA SE C'E'
071600000627     C                   MOVEL     'M'           §TRC
071700000627     C                   SETOFF                                       0809
071800000627     C     KNSPE         CHAIN     TITAA30C                           32
071900000627     C  N32
072000000627     CAN 08              DELETE    TITAA000
072100000627     C  N32
072200000627     CAN 09              DELETE    TITAAP00
072300000627     C                   ENDIF
072400900601     C                   END
072500991104     C*
072600991104     C* MEMORIZZO IL MITTENTE ORIGINALE IN TITAA TIPO RECORD "O"
072700991104     C     BLPRMO        IFNE      *BLANKS
072800000627     C                   SETON                                        32
072900000627     C   52              MOVEL     'O'           §TRC
073000000627     C   52              SETOFF                                       0809
073100000627     C   52KNSPE         CHAIN     TITAA30C                           32
073200000627     C   32              CLEAR                   TITAA000
073300991104     C                   Z-ADD     BLPAAS        TAAAAS
073400991104     C                   Z-ADD     BLPLNP        TAALNP
073500991104     C                   Z-ADD     BLPNRS        TAANRS
073600991104     C                   Z-ADD     BLPNSP        TAANSP
073700991104     C                   MOVEL     'O'           TAATRC
073800991104     C                   MOVEL     BLPRMO        TAARSC
073900000627     C   32              WRITE     TITAA000
074000000627     C  N32
074100000627     CAN 08              UPDATE    TITAA000
074200000627     C  N32
074300000627     CAN 09              UPDATE    TITAAP00
074400000627     C                   ELSE
074500000627     C     *IN52         IFEQ      *ON
074600000627     C                   MOVEL     'O'           §TRC
074700000627     C                   SETOFF                                       0809
074800000627     C     KNSPE         CHAIN     TITAA30C                           32
074900000627     C  N32
075000000627     CAN 08              DELETE    TITAA000
075100000627     C  N32
075200000627     CAN 09              DELETE    TITAAP00
075300000627     C                   ENDIF
075400991104     C                   END
075500000627     C* SE SONO IN RICEZIONE AGGIORNAMENTO BOLLA AUTOGENERATA
075600000627     C* CANCELLO DA TITA4 TUTTI I TIPI RECORD CANCELLABILI
075700000627     C     *IN52         IFEQ      *ON
075800000627     C                   CLEAR                   SA4TRC
075900000627     C                   SETOFF                                       330809
076000000627     C     KNSP          SETLL     TITA430C
076100000627     C     KNSP          READE     TITA430C                               32
076200000627     C     *IN32         DOWEQ     *OFF
076300000627     C     TA4TRC        IFNE      SA4TRC
076400000627     C                   MOVE      TA4TRC        SA4TRC
076500000627     C     TA4TRC        LOOKUP    SDL                                    33
076600000627     C                   ENDIF
076700000627     C   33
076800000627     CAN 08              DELETE    TITA4000
076900000627     C   33
077000000627     CAN 09              DELETE    TITA4P00
077100000627     C                   SETOFF                                       0809
077200000627     C     KNSP          READE     TITA430C                               32
077300000627     C                   ENDDO
077400000627     C                   ENDIF
077500990618     C*
077600060220     C* 96 ON  - AVVENUTA APERTURA MEMBRO DEL FILE Fiar400R
077700071106     C****               CLEAR                   WTROV
077800071106     C****               CLEAR                   DSBL4E
077900151113     c                   clear                   wb4ado
078000991104    1C     *IN96         IFEQ      *ON
078100060220     C     KASS          CHAIN     Fiar400R                           32
078200991104    2C     *IN32         DOWEQ     *OFF
078300991104     C*
078400071106    3c**** ar4TRC        IFEQ      'E'
078500071106     C****               ADD       1             NREC4             7 0
078600071106     C****               MOVEL     ar4NOT        DSBL4E
078700071106     C*****              MOVEL     'S'           WTROV             1
078800070926   X3C**********         ELSE
078900071106    3c****               endif
079000070926     c* 26/09/07 -ES- memorizzo il record E come è in filiale
079100070926     c
079200060220     C* RICEVO COMUNQUE RECORD DI ar4 IN TITA4  ESCLUDENDO TRC='A'
079300060220    4C     ar4TRC        IFNE      'A'
079400041105     c* Se si tratta di record M verifico se c'e' già e vedo in update
079500060220     c                   if        ar4trc='M'
079600041105     C                   MOVEL     'M'           §TRC
079700041105     c     kNSPE         chain     tita430c
079800041105     C                   ENDIF
079900041105     c
080000071106     C                   ADD       1             NREC4             7 0
080100991213     C                   CLEAR                   TITA4000
080200991213     C                   Z-ADD     BLPAAS        TA4AAS
080300991213     C                   Z-ADD     BLPLNP        TA4LNP
080400991213     C                   Z-ADD     BLPNRS        TA4NRS
080500991213     C                   Z-ADD     BLPNSP        TA4NSP
080600060220     C                   MOVEL     ar4TRC        TA4TRC
080700060220     C                   MOVEL     ar4NOT        TA4NOT
080800060220     C                   IF        ar4TRC<>'M' or not %found(tita430c)
080900010109     C  N07              WRITE     TITA4000                             94
081000010109     C   07              WRITE     TITA4P00                             94
081100041105     c                   else
081200041105     C   08              update    TITA4000
081300041105     C   09              update    TITA4P00
081400041105     c                   endif
081500151113     c* Se tipo record "M" salvo flag addebito orm telefonici da memorizzare
081600151113     c* anche su titas
081700151113     c                   if        ar4trc='M'
081800151113     c                   movel     ar4not        dsbl4m
081900151113     c                   movel     §b4ado        wb4ado
082000151113     c                   endif
082100991213    4C                   ENDIF
082200070926    3C**********         ENDIF
082300041105     c
082400060220     C     KASS          READE     Fiar400R                               32
082500991104    2C                   ENDDO
082600991104    1C                   ENDIF
082700041105     c
082800990618     C* RIFERIMENTO MITTENTE ALFABETICO: IN TITA4 TIPO RIF."A"
082900160120     c* ora anche natura merce nel record A per lasciare vuoto il campo su TITAS
083000160120     c* ai fini  del C/E
083100990630     C     BLPRMA        IFNE      *BLANKS
083200160120     C     BLPnas        orNE      *BLANKS
083300990618     C                   CLEAR                   TITA4000
083400071106     C                   CLEAR                   dta4a
083500990618     C                   Z-ADD     BLPAAS        TA4AAS
083600990618     C                   Z-ADD     BLPLNP        TA4LNP
083700990618     C                   Z-ADD     BLPNRS        TA4NRS
083800990618     C                   Z-ADD     BLPNSP        TA4NSP
083900990618     C                   MOVEL     'A'           TA4TRC
084000160120     C                   MOVEL     BLPRMA        §TA4ARMA
084100160121     C                   MOVEL     BLPnas        §TA4ANAS
084200991104     C                   MOVEL     DTA4A         TA4NOT
084300010109     C  N07              WRITE     TITA4000
084400010109     C   07              WRITE     TITA4P00
084500990630     C                   END
084600050531     c* Se presente in bolla importo da assic. creo tita4 record "C"
084700050531     c* per pgm di elaborazione invio e-mail ad assicurazione per
084800050531     c* spedizioni senza mandato
084900050531     c     *in52         ifeq      *off
085000050531     c     blpias        andgt     *zeros
085100050531     c                   clear                   dta4c
085200050531     c                   z-add     blpaas        ta4aas
085300050531     c                   z-add     blplnp        ta4lnp
085400050531     c                   z-add     blpnrs        ta4nrs
085500050531     C                   Z-ADD     BLPNSP        TA4NSP
085600050609     C                   MOVEL     'c'           TA4TRC
085700050531     C                   MOVEL     'S'           §TA4pias
085800050531     C                   MOVEL(P)  DTA4c         TA4NOT
085900050531     C                   WRITE     TITA4000
086000050531     c                   endif
086100900601     C*
086200000627     C     *IN52         IFEQ      *OFF
086300050318     c* se si tratta di assegnato S.F. imposto anche ksc e ctr
086400050318     c                   move      ar6ksc        fineksc           4 0
086500050318     c                   if        fineksc<>0000 and fineksc<>9999
086600050318     c                   eval      tasksc=ar6ksc
086700050318     c                   eval      tasctr=ar6ctr
086800050318     c                   endif
086900991104     C                   MOVEL     AR6DIV        TASDIV
087000991104     C                   Z-ADD     AR6POR        TASPOR
087100991104     C                   MOVE      AR6SV1        TASSV1
087200991104     C                   Z-ADD     AR6VA1        TASVA1
087300991104     C                   MOVE      AR6SV2        TASSV2
087400991104     C                   Z-ADD     AR6VA2        TASVA2
087500991104     C                   MOVE      AR6SV3        TASSV3
087600991104     C                   Z-ADD     AR6VA3        TASVA3
087700991104     C                   Z-ADD     AR6NFT        TASNFT
087800991104     C                   Z-ADD     AR6DFT        TASDFT
087900991104     C                   Z-ADD     AR6FIV        TASFIV
088000991104     C                   MOVEL     AR6CEI        TASFEI
088100991104     C                   Z-ADD     AR6IMV        TASIMV
088200151120     c                   if        tasimv>0
088300151119     c                   clear                   dar5fat
088400151119     c                   Eval      ktrd='FAT'
088500151119     c     kass2         Chain     Fiar531c
088600151119     c                   If        %Found(Fiar531c)
088700151119     c                   Movel     ar5uni        dar5fat
088800151119     c                   select
088900151119     c                   when      §AR5FPTAS='R'
089000151119     c                   eval      tasfpf='R'
089100151119     c                   when      §AR5FPTAS='D'
089200151119     c                   eval      tasfpf='D'
089300151119     c                   other
089400151119     c                   eval      tasfpf='V'
089500151119     c                   endsl
089600151120     c                   endif
089700151119     c                   endif
089800900601     C* IMPOSTO FLAG IN BASE AL RIEMPIMENTO DEL CAMPI RELATIVI
089900991104     C     AR6DFT        IFGT      *ZERO
090000941114     C                   MOVE      '1'           TASFTS
090100900601     C                   END
090200000627     C                   ENDIF
090300991104     C* CREO LE ALTRE VARIE SE ESISTE IL DETTAGLIO
090400991104     C   97              EXSR      SR7
090500061211     c
090600061218      * Leggo fiar5 x impostare il flag dei colli di valore su titas
090700091214     c                   clear                   War5cdi           7
090800061218     C                   If        *in91
090900110607     c                   eval      ktrd='GEN'
091000061218     C     KASS2         chain     FIAR500R
091100061218     c                   if        %found(fiar500r)
091200061218     c                   Eval      dAr5Gen = Ar5Uni
091300061218     c                   If        §Ar5Bva = 'V'
091400061218     c                   Eval      dtasflo = TasFlo
091500061218     c                   Eval      §FloTba = §Ar5Bva
091600061218     c                   Eval      TasFlo = dtasflo
091700061218     c                   endif
091800091214     c* Se presente beneficiario assegno --> lo memorizzo per CSB
091900091214     c                   if        §ar5cdi>*zeros
092000091214     c                   eval      war5cdi=§ar5cdi
092100091214     c                   endif
092200091214     c
092300061218     c                   endif
092400130925      * leggo fiar5 EMD per impostare il flag avviso al destinatario su titas
092500130325      * Solo se non si tratta di bolla figlia
092600130325     c     knsp          setll     fnlbl01l
092700130325     c                   if        not %equal(fnlbl01l)
092800110607     c                   eval      ktrd='EMD'
092900130322     C     KASS2         chain     FIAR500R
093000130322     c                   if        %found(fiar500r)
093100130322     c                   eval      dar5emd=ar5uni
093200130925     c***                if        §ar5eml<>*blanks
093300130925     c                   if        §ar5smp='S' or §ar5mmp='S'
093400110607     c                   Eval      dtasflo = TasFlo
093500130925     c                   select
093600130925     c                   when      §ar5mmp='S' and §ar5smp=' '
093700130925     c                   eval      §floemd='S'
093800130925     c                   when      §ar5mmp='S' and §ar5smp='S'
093900130925     c                   eval      §floemd='E'
094000130925     c                   when      §ar5smp='S'
094100130925     c                   eval      §floemd='X'
094200130925     c                   endsl
094300110607     c                   Eval      TasFlo = dtasflo
094400130322     c                   endif
094500110607     c                   endif
094600150220      **
094700150220      *(sempre all'interno del controllo sulla MAMMA,)
094800150220      * Gestione BANCALI - Movimenti BAN x clienti
094900150305     c                   if          %subst(tasGVA:1:1) ='B'
095000150220     c                   eval      ktrd='BAN'
095100150220     C     KASS2         chain     FIAR500R
095200150220     c                   if        %found(fiar500r)
095300150220     c                   clear                   tisig20ds
095400150220     c                   eval      i20aas = ar5aas
095500150220     c                   eval      i20lnp = ar5lnp
095600150220     c                   eval      i20nrs = ar5nrs
095700150220     c                   eval      i20nsp = ar5nsp
095800150220     c                   eval      i20trd = ar5trd
095900150220     c                   eval      i20DAT = tasAAS * 10000 + tasMGS
096000150220     c                   move      *all'0'       i20SOG
096100150220     c                   move      TASccm        i20SOG
096200150311     c                   move      TAStbl        i20TBL
096300150311     c                   move      TAScbo        i20CBO
096400150220      *
096500160104     c                   Eval      kpjbux = kpjbu
096600150220     c                   eval        kpjbu = tisig20ds
096700150318     c                   call      'TISIG20R'                           77
096800150220     c                   parm                    kpjba
096900150220     c                   parm                    Fiar5ds
097000160104     c                   Eval      kpjbu = kpjbux
097100150220     c                   endif
097200150220     c                   end
097300150220      *
097400150220      * Gestione BANCALI - Movimenti BNB Filiale
097500150305     c                   if          %subst(tasGVA:1:1) ='O'
097600150220     c                   eval      ktrd='BNB'
097700150220     C     KASS2         chain     FIAR500R
097800150220     c                   if        %found(fiar500r)
097900150220     c                   clear                   tisig20ds
098000150220     c                   eval      i20aas = ar5aas
098100150220     c                   eval      i20lnp = ar5lnp
098200150220     c                   eval      i20nrs = ar5nrs
098300150220     c                   eval      i20nsp = ar5nsp
098400150220     c                   eval      i20trd = ar5trd
098500150220     c                   eval      i20DAT = tasAAS * 10000 + tasMGS
098600150220     c                   move      *all'0'       i20SOG
098700150220     c                   move      TASccm        i20SOG
098800150311     c                   move      TAStbl        i20TBL
098900150311     c                   move      TAScbo        i20CBO
099000150220      *
099100160104     c                   Eval      kpjbux = kpjbu
099200150220     c                   eval        kpjbu = tisig20ds
099300150318     c                   call      'TISIG20R'                           77
099400150220     c                   parm                    kpjba
099500150220     c                   parm                    Fiar5ds
099600160104     c                   Eval      kpjbu = kpjbux
099700150220     c                   endif
099800150220     c                   end
099900150220      **
100000130325     c                   endif
100100110607
100200061218     c                   endif
100300061211     c
100400061211     c* Aggiorno legami bolla come figlia
100500061211     c                   exsr      AggLBL
100600061211
100700061211      * richiamo fidn58r x aggiornare titas se presente c.a.
100800061211     c                   Clear                   Fidn58ds
100900061211     c                   Eval      i58aas = tasaas
101000061211     c                   Eval      i58lnp = taslnp
101100061211     c                   Eval      i58nrs = tasnrs
101200061211     c                   Eval      i58nsp = tasnsp
101300061211     c                   Eval      i58sed = 'S'
101400061211     c                   Eval      kpjbux = kpjbu
101500061211     c                   Clear                   kpjbu
101600061211     c                   movel     fidn58ds      kpjbu
101700061211     c                   call      'FIDN58R'
101800061211     c                   parm                    kpjba
101900061212     C                   MOVEL     KPJBU         FIDN58DS
102000061211     c                   Eval      kpjbu = kpjbux
102100061211     c
102200061211     c                   eval      tasfda=o58fda
102300110519     c
102400110519     c* Verifico, se C/Ass, se auto abilitato
102500110519     C     *IN12         ifeq      *ON
102600110519     C     BL9CAS        ANDGT     0
102700110519     c
102800110519     c                   clear                   trul21ds
102900110519     c                   eval      i21tla = 'L'
103000110519     c                   eval      i21cbo = blpcbo
103100110519     c                   eval      i21tsp = blptsp
103200110519     c                   eval      i21lnp = blplnp
103300110519     c                   eval      i21nzm = blpnzm
103400110519     c                   eval      i21lna = blplna
103500130808     c                   eval      i21ksc = tasccm
103600110519     c                   eval      i21nzd = blpnzd
103700110519     c                   eval      i21tic = bl9tic
103800110519     c                   eval      i21imp = bl9cas
103900110519     c                   eval      i21div = bl9vca
104000110519     c                   eval      i21ute = knmus
104100110519     c                   eval      i21pgm = 'TRTR36R'
104200110519     c                   call      'TRUL21R'
104300110519     c                   parm                    trul21ds
104400110519
104500110519     c                   if        o21fca <> *blanks
104600110519     c                   Eval      dtasflo = TasFlo
104700110519     c                   Eval      §FloFCA = 'S'
104800110519     c                   Eval      TasFlo = dtasflo
104900110519     c                   endif
105000110519     c                   endif
105100151113     c
105200151113     c* aggiorno flag per addebito orm telefonico
105300151113     c                   eval      dtasflo = Tasflo
105400151113     c                   eval      §floado = wb4ado
105500151113     c                   eval      TasFlo=dtasflo
105600110519     c
105700900601     C*
105800000627     C  N52              WRITE     TITAS000
105900000628     C* N.B. HO UGUALMENTE PREVISTO L'AGGIORNAMENTO ANCHE DI TAS10 E
106000000628     C*      TASP ANCHE SE PER ORA E' INUTILE VISTO CHE SE BOLLA
106100000628     C*      GIA CONTABILIZZATA VIENE STAMPATO ERRORE E LA BOLLA NON
106200000628     C*      VIENE CARICATA (V. ROUTINE CTRNSP)
106300010109     C*      09/01/01 : DA OGGI L'AGGIORNAMENTO DI TAS10/P NON SARÀ PIÙ
106400010109     C*                 INUTILE DAL MOMENTO CHE SE BOLLA GIA' CONTABIL.
106500010109     C*                 CARICO LO STESSO L'AGGIORNAMENTO
106600000627     C   52
106700000627     CANN53
106800000627     CAN 05              UPDATE    TITAS000
106900000627     C   52
107000000627     CANN53
107100000627     CAN 06              UPDATE    TITAS010
107200000627     C   52
107300000627     CANN53
107400000627     CAN 07              UPDATE    TITASP00
107500040603      *
107600040603
107700040603      * Sposto la ricezione di FIAR5 dopo la ricezione dei contrassegni
107800040603
107900900601     C*
108000950112     C* CREA TNCSB SE ESISTE CONTRASSEGNO
108100000628     C* SE ESISTE GIA' TNCSB MA IN STATO 9 E STO RICEVENDO
108200000628     C* AGGIORNAMENTO DI BOLLA AUTOGENERATA RIAGGIORNO
108300950112     C** X ASSEGNATO O 8888 O 9999 CREA ANCHE TNCSA00F
108400111019     C**                   CLEAR                   WINMSG
108500900601     C*
108600000629     C                   SELECT
108700000629     C     *IN12         WHENEQ    *ON
108800000628     C     BL9CAS        ANDGT     0
108900091214     c
109000091214     C**   *IN52         OREQ      *ON
109100091214     C**   BL9CAS        ANDGT     0
109200091214     C**   *IN12         ANDEQ     *OFF
109300091214     C**   CSBSTA        ANDEQ     9
109400000628     C*
109500950112     C** TNCSB
109600000628     C   12              CLEAR                   TNCSB000
109700011031     C*
109800011031     C* TIPO INCASSO C/A
109900011031     C                   EXSR      IMPTIC
110000010117     C*
110100111019     C**N12TASDCM        IFGT      *ZEROS
110200111019     C**   TASCCA        ANDNE     *BLANKS
110300010117     C* Se bolla autogenerata e consegnata con consegna anomala e sto
110400010117     c* ricevendo il suo aggiornamento con il contrassegno devo mante-
110500010117     c* nere il contrassegno in stato 9 e di conseguenza non invio msg
110600010117     c* per l'ufficio contrassegni
110700111019     C**                 ELSE
110800111019     C**                 CLEAR                   CSBSTA
110900111019     C**                 MOVEL     '1'           WINMSG            1
111000111019     C**                 ENDIF
111100111019     c
111200010117     C* Se sono in ricezione di aggiornamento bolla autogenerata già
111300010117     c* consegnata alla quale è stato aggiunto il contrassegno:
111400010117     c* - invio messaggio per ufficio C/A di contrassegno aggiunto
111500010117     C*   se la consegna non è anomala
111600010117     c* - ricevo il contrassegno come annullato se la consegna è anomal
111700111019     C**   52
111800111019     C**AN 12TASDCM        IFGT      *ZEROS
111900111019     C**   TASCCA        IFEQ      *BLANKS
112000111019     C*+                 MOVEL     '1'           WINMSG
112100111019     C**                 ELSE
112200111019     C**                 Z-ADD     9             CSBSTA
112300111019     C**                 ENDIF
112400111019     C**                 ENDIF
112500010117     C*
112600910926     C                   MOVEL     §TBCA         CSBTBL
112700910926     C                   Z-ADD     TASLNP        CSBLNP
112800910926     C                   Z-ADD     TASAAS        CSBAAS
112900910926     C                   Z-ADD     TASNRS        CSBNRS
113000910926     C                   Z-ADD     TASNSP        CSBNSP
113100941114     C                   Z-ADD     TASMGS        CSBMGS
113200990615     C                   Z-ADD     TASTFP        CSBFLE
113300950112     C                   Z-ADD     BL9CAS        CSBCAS
113400950112     C                   Z-ADD     BL9CMB        CSBCMB
113500950112     C                   MOVEL     BL9VCA        CSBVCA
113600950117     C                   MOVEL     BL9GCA        CSBGCA
113700941114     C                   MOVEL     BLPLOD        CSBPAR                         PAESE ARRIVO
113800941114     C                   MOVEL     BLPPRD        CSBPRO                         PROV.DEST.
113900910926     C                   MOVEL     BLPRSD        CSBRSD                         DESTINATARIO
114000910926     C                   Z-ADD     TASRMN        CSBRMN
114100910926     C                   Z-ADD     TASLNA        CSBLNA
114200910926     C                   Z-ADD     TASKSC        CSBKSC
114300910926     C                   MOVE      CSBKSC        CSBCDI
114400931108     C                   MOVEL     'R'           CSBFTR
114500030108     C                   Z-ADD     DateU         CSBDTR
114600990618     C                   MOVEL     BLPRMA        CSBRMA
114700050411     C** PER ASSEGNATI COD. METTO XXX9999 DOVE XXX LNA ARRIVO
114800050411     C     §TIBO         IFEQ      'A'
114900050411     C                   Z-ADD     99999         CSBKSC
115000050411     C                   MOVEL     TASLNA        CSBKSC
115100060612     c* Se bolla singola in assegnato:  58 off
115200060612     c*                          imposto CDI=xxx9999 e poi magari il blpccm
115300060612     c* Se bolla doppia franco+assegnato: 58 on
115400060612     c*                          imposto CDI=tasksc (mittente)
115500060612     C  n58              MOVEL     CSBKSC        CSBCDI
115600050411     C                   END
115700920304     C* COD CLIENTE MITTENTE
115800920304     C     BLPCCM        IFGT      0
115900920304     C                   MOVEL     BLPCCM        CSBCDI
116000920304     C                   END
116100091214     c
116200091214     c* Beneficiario = ordinante
116300091214     c                   if        war5cdi>*zeros
116400091214     c                   movel     war5cdi       CSBCDI
116500091214     c                   endif
116600920304     C*
116700900601     C** RAGGRUPPAMENTO C/ASSEGNI
116800900601     C     TASLNA        CHAIN     AZORG                              51
116900910926     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
117000910926     C   51              Z-ADD     TASLNA        CSBRGF
117100941130     C*
117200000628     C                   EXSR      TNCSA
117300920306     C*
117400000628     C* SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA CON C/A
117500000628     C* E C/A GIA' PRESENTE SU TNCSB VEDO SE AGGIORNARE
117600091214     C***  *IN52         WHENEQ    *ON
117700091214     C***  BL9CAS        ANDGT     *ZEROS
117800091214     C***  *IN12         ANDEQ     *OFF
117900091214     C***  CSBSTA        ANDNE     9
118000091214     C***                Z-ADD     BL9CAS        WARCAS
118100091214     C***                MOVE      BL9VCA        WARVCA
118200000628     C* SE DIVISE DIVERSE VERIFICO SE ENTRAMBE ACCETTABILI IN ITALIA
118300000628     C* ED IN QUESTO CASO CONVERTO IMPORTO IN RICEZIONE
118400091214    3C***  BL9VCA        IFNE      CSBVCA
118500091214     C**+  BL9VCA        LOOKUP    DVA                                    31
118600091214     C**+31              Z-ADD     1             II
118700091214     C***31CSBVCA        LOOKUP    DVA(II)                                31
118800091214    4C***  *IN31         IFEQ      *ON
118900091214     C***                CLEAR                   DSYEUR
119000091214     C***                MOVEL     BL9VCA        YECDVI
119100091214     C***                MOVEL     CSBVCA        YECDVO
119200091214     C***                Z-ADD     BL9CAS        YECIMI
119300091214     C***                MOVE      DEC(II)       YECDCO
119400091214     C***
119500091214     C***                CALL      'YEURCO'
119600091214     C***                PARM                    DSYEUR
119700091214     C***
119800091214    5C***  YECESI        IFEQ      ' '
119900091214     C***                Z-ADD     YECIMO        WARCAS
120000091214     C***                MOVEL     CSBVCA        WARVCA
120100091214    5C***                ENDIF
120200091214    4C***                ENDIF
120300091214    3C***                ENDIF
120400091214     C**LEGGO IMPORTO MINIMO ABBUONO SOLO SE STESSA DIVISA
120500091214     C***  WARVCA        IFEQ      CSBVCA
120600091214     C***                CLEAR                   TIBS02
120700091214     C***                MOVEL     KPJBU         KPJBUX
120800091214     C***                CLEAR                   KPJBU
120900091214     C***                MOVE      'C'           T02MOD                         CONTROLLO
121000091214     C***                MOVE      KNSIF         T02SIF                         S.I.
121100091214     C***                MOVE      'GEI'         T02COD                         TABELLA PROFILI
121200091214     C***                MOVEL(P)  WARVCA        T02KE1                         PROFILO IN K1
121300091214     C***                CALL      'TIBS02R'
121400091214     C***                PARM                    KPJBA
121500091214     C***                PARM                    TIBS02
121600091214     C***                MOVEL     KPJBUX        KPJBU
121700091214    3C***  T02ERR        IFEQ      *BLANKS
121800091214     C***                MOVEL     T02UNI        DGEI
121900091214     C***                ELSE
122000091214     C***                CLEAR                   DGEI
122100091214    3C***                ENDIF
122200091214    3C***  §GEMIA        IFGT      0
122300091214     C***  WARCAS        ANDNE     CSBCAS
122400091214     C***  WARCAS        SUB       CSBCAS        WDIFFE
122500091214     C***                MLLZO     1             WDIFFE
122600091214     C**SE LA DIFFERENZA RIENTRA NELLA TOLLERANZA IMPOSTO WARCAS=CSBCAS
122700091214     C***  WDIFFE        IFLE      §GEMIA
122800091214     C***                Z-ADD     CSBCAS        WARCAS
122900091214     C***                ENDIF
123000091214    3C***                ENDIF
123100091214     C***                ENDIF
123200091214     C**IMPORTO DIVERSO
123300091214    3C***  WARCAS        IFNE      CSBCAS
123400091214     C***  WARVCA        ORNE      CSBVCA
123500091214     C**OPPURE MITTENTE DIVERSO
123600091214     C***  TASCCM        ORNE      CSBCDI
123700091214     C**Invio messaggio in posta elettronica
123800091214     C**Stesso tipo di messaggio se no pagato:non invio niente se
123900091214     c**importo non variato
124000091214    4C***  CSBBNA        IFEQ      0
124100091214    5C***  WARCAS        IFNE      CSBCAS
124200091214     C***  WARVCA        ORNE      CSBVCA
124300091214     C***                EXSR      MSGPOS
124400091214    5C***                ENDIF
124500091214   x4C***                ELSE
124600091214     C**MSG se pagato e importo diverso
124700091214    5C***  WARCAS        IFNE      CSBCAS
124800091214     C***  WARVCA        ORNE      CSBVCA
124900091214     C***                EXSR      MSGPO2
125000091214   x5C***                ELSE
125100091214     c**se pagato e importi uguali sengalo solo variazione del mittente
125200091214     C***                EXSR      MSGPO3
125300091214    5C***                ENDIF
125400091214    4C***                ENDIF
125500091214     C**Aggiorno solo se non pagato o incassato
125600091214     C**Il cliente lo vario anche se incassato
125700091214    4C***  CSBBNA        IFEQ      0
125800091214     C***                Z-ADD     TASKSC        CSBKSC
125900091214     C***                MOVE      CSBKSC        CSBCDI
126000091214     C***Per assegnati cod. metto XXX9999 dove XXX lna arrivo
126100091214    5C***  §TIBO         IFEQ      'A'
126200091214     C***                Z-ADD     99999         CSBKSC
126300091214     C***                MOVEL     TASLNA        CSBKSC
126400091214     C***                MOVEL     CSBKSC        CSBCDI
126500091214    5C***                END
126600091214     C**Cod cliente mittente
126700091214    5C***  BLPCCM        IFGT      0
126800091214     C***                MOVEL     BLPCCM        CSBCDI
126900091214    5C***                END
127000091214     C***                EXSR      TNCSA
127100091214    4C***                ENDIF
127200091214     c***
127300091214    4C***  CSBDDC        IFEQ      *ZEROS
127400091214     C***  CSBBNA        ANDEQ     *ZEROS
127500091214    5C***  WARCAS        IFNE      CSBCAS
127600091214     C***  WARVCA        ORNE      CSBVCA
127700091214     C***                Z-ADD     BL9CAS        CSBCAS
127800091214     C**TIPO INCASSO C/A
127900091214     C***                EXSR      IMPTIC
128000091214     C***                MOVEL     BL9VCA        CSBVCA
128100091214     C***                MOVEL     BL9CMB        CSBCMB
128200091214     C***                MOVEL     BL9GCA        CSBGCA
128300091214    5C***                ENDIF
128400091214    4C***                ENDIF
128500091214    3C***                ENDIF
128600000629     C                   ENDSL
128700061218     c*  11/12/06 - ES
128800061218     c*  qui c'era la lettura e scrittura di FIAR5 ma la sposto
128900061211     c*  per evitare un chain e update di TITAS
129000061218     c*  18/12/06 - ES
129100061218     c*  Rimetto la write di fiar5 qui  perchè a causa del C/E (titasque)
129200061218     c*  devo scrivere prima titas00f di fiar5 altrimenti il C/E da errore
129300061218     c                   exsr      AggFIAR5
129400080828     c
129500080828     c* Verifico se devo aggiornare a creare FIAR5 per invetario restituzDDT
129600080902     c* Escludo le bolle di recupero
129700100317     c
129800100317     c* ES 17/03/10 --> non lo faccio più qui spostato nel gpm fnlsa3r per creare
129900100317     c*   il recorrd GEN in filiale ed evitare proboemi di sovrapposizione dati
130000100317+--- C***  WRBL          IFNE      'R'                                          (a)
130100100317     c**+                exsr      contrDCLI
130200100317     c***                endif
130300900601     C*
130400931231     C* C R E A   2 °   B O L L A :
130500000627     C* DO PER SCONTATO CHE PER LE POSTE NON CI SIA MAI E QUINDI
130600000627     C* NON DEVO GESTIRE L'AGGIORNAMENTO DI BOLLA AUTOGENERATA
130700900601     C*
130800900601     C   59              DO
130900900601     C*
131000990621     C* AZZERO CAMPI DI TITAS
131100000627     C                   CLEAR                   TITAS000
131200941114     C*
131300941114     C* RIEMPO I CAMPI CHE SONO UGUALI PER LA 1 E LA 2 BOLLA
131400990817     C                   MOVEL     §3ATB2        WFLGTB            1
131500941114     C                   EXSR      RIETAS
131600900601     C                   MOVE      §3ATB2        TASTBL
131700900601     C** SEMPRE ASSEGNATO:
131800900601     C** COD.CLI.= XXX9999 DOVE XXX =LINEA ARRIVO
131900900601     C                   Z-ADD     9999          TASKSC
132000900601     C                   MOVEL     BLPLNA        TASKSC
132100061211     c* per C.A.
132200061211     c                   eval      tasfda=o58fda
132300900601     C*
132400900601     C* ESISTE ESTENSIONE BOLLA
132500000627     C   93              DO
132600991104     C                   MOVEL     '2'           WTRC
132700991104     C     KASS1         CHAIN     FIAR600R                           73
132800991104     C     *IN73         IFEQ      *OFF
132900991104     C                   ADD       1             NRECC             7 0
133000040112     C*!N52AR6SV1        IFNE      *BLANK
133100040112     c                   If        Not *In52
133200050318     c* se si tratta di assegnato S.F. imposto anche ksc e ctr
133300050318     c                   move      ar6ksc        fineksc           4 0
133400050318     c                   if        fineksc<>0000 and fineksc<>9999
133500050318     c                   eval      tasksc=ar6ksc
133600050318     c                   eval      tasctr=ar6ctr
133700050318     c                   endif
133800991104     C                   MOVEL     AR6DIV        TASDIV
133900991104     C                   MOVE      AR6SV1        TASSV1
134000991104     C                   Z-ADD     AR6VA1        TASVA1
134100040112     c                   Move      Ar6Sv2        TasSv2
134200040112     c                   Z-add     Ar6Va2        TasVa2
134300040112     c                   Move      Ar6Sv3        TasSv3
134400040112     c                   Z-add     Ar6Va3        TasVa3
134500991104     C                   MOVEL     AR6CEI        TASFEI
134600040112     c                   Z-add     Ar6Imv        TasImv
134700950114     C                   END
134800040112      * Creo le altre varie se esiste il dettaglio su FIAR7
134900040112     c   97              ExSr      SR7
135000991104     C                   END
135100991108     C                   END
135200900601     C*
135300950112     C   58BL9CAS        IFGT      0
135400941114     C                   MOVEL     'S'           TASFCA
135500941114     C                   ENDIF
135600061211     c
135700061211     c* Aggiorno legami bolla come figlia
135800061211     c                   exsr      AggLBL
135900900601     C*
136000000627     C                   WRITE     TITAS000
136100900601     C                   END
136200061211     c
136300950522     C** SCRITTURA TNCSB
136400000628     C     *IN12         IFEQ      *ON
136500000628     C     BL9CAS        ANDGT     0
136600091214     c
136700091214     C**   *IN12         OREQ      *OFF
136800091214     C**   *IN52         ANDEQ     *ON
136900091214     C**   BL9CAS        ANDGT     *ZEROS
137000000628     C   12              WRITE     TNCSB000
137100000628     C  N12              UPDATE    TNCSB000
137200010117     C* INVIO MSG DI C/A RIATTIVATO O AGGIUNTO
137300111019     C**   WINMSG        IFEQ      '1'
137400111019     C**                 EXSR      MSGPO1
137500111019     C**                 ENDIF
137600950522     C                   END
137700941126     C                   END
137800061211     c
137900061211     c* Per sicurezza leggo anche come mamma anche se non dovrebbe
138000061211     c*  succedere
138100061211     c     knsp          setll     fnlbl02l
138200061211     c     knsp          reade     fnlbl02l
138300061211     c                   dow       not %eof(fnlbl02l)
138400061211     c                   call      'FNLG25R'
138500061211     c                   parm                    recordlbl
138600061211     c                   parm                    i25tla            1
138700061211     c
138800061211     c     knsp          reade     fnlbl02l
138900061211     c                   enddo
139000041119     c
139100041119     C                   ENDSR
139200000628     C*
139300011022     C**---------------------------------------------------------
139400011022     C** IMPOSTO IL TIPO INCASSO C/A E CONTROLLO LA VALUTA
139500011022     C**---------------------------------------------------------
139600000628     C     IMPTIC        BEGSR
139700000628     C                   MOVEL     'B'           CSBFUS                          DEFAULT
139800000628     C     BL9TIC        IFNE      *BLANK
139900000628     C                   MOVE      '1A'          COD                             COD.BOLLA
140000000628     C                   MOVE      *BLANK        KEY
140100000628     C                   MOVEL     BL9TIC        KEY
140200000628     C     KTAB          CHAIN     TABEL                              54
140300011023     C  N54              MOVEL     TBLUNI        DS1A
140400011023     C  N54              MOVEL     §1AFMB        CSBFUS
140500011022      *
140600000628     C                   END
140700000628     C                   ENDSR
140800000628     C**----------------------------------------
140900000628     C** AGGIORNAMENTO/SCRITTURA/DELET TNCSA00F
141000000628     C**----------------------------------------
141100000628     C     TNCSA         BEGSR
141200000628     C  N12KASS          CHAIN     TNCSA01L                           54
141300000628     C*
141400000628     C** TNCSA
141500000628     C                   Z-ADD     TASLNP        CSALNP
141600000628     C                   Z-ADD     TASAAS        CSAAAS
141700000628     C                   Z-ADD     TASNRS        CSANRS
141800000628     C                   Z-ADD     TASNSP        CSANSP
141900000628     C* SE IL CODICE INTESTAZIONE ASSEGNO E' GENERICO CREO CSA
142000000628     C                   MOVE      CSBCDI        W0040             4 0
142100000628     C*
142200000628     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
142300000628     C** FRANCHI VARI 88888 SENZA COD CLI MITT CHE NEGLI 8888
142400000628     C** E' PER IL COD.INTESTAZIONE ASSEGNO
142500000628     C     W0040         IFEQ      8888
142600000628     C     W0040         OREQ      9999
142700000628     C                   MOVEL     BLPRSM        CSARSD                         MITTENTE
142800000628     C                   MOVEL     BLPINM        CSAVID                            '
142900000628     C                   MOVEL     BLPCAM        CSACAD                            '
143000000628     C                   MOVEL     BLPLOM        CSACID                            '
143100000628     C                   MOVEL     BLPPRM        CSAPRM                            '
143200000628     C   12              WRITE     TNCSA000
143300000628     C  N12
143400000628     CAN 54              WRITE     TNCSA000
143500000628     C  N12
143600000628     CANN54              UPDATE    TNCSA000
143700000628     C                   ELSE
143800000629     C  N12
143900000629     CANN54              DELETE    TNCSA000
144000000628     C                   END
144100000628     C                   ENDSR
144200991108     C**----------------------------------------
144300991108     C** PULIZIA CAMPI FIAR600R
144400991108     C**----------------------------------------
144500991108     C     PULAR6        BEGSR
144600991108     C                   CLEAR                   AR6ATB
144700991108     C                   CLEAR                   AR6AAS
144800991108     C                   CLEAR                   AR6LNP
144900991108     C                   CLEAR                   AR6NRS
145000991108     C                   CLEAR                   AR6NSP
145100991108     C                   CLEAR                   AR6TRC
145200991108     C                   CLEAR                   AR6KSC
145300991108     C                   CLEAR                   AR6CTR
145400991108     C                   CLEAR                   AR6DIV
145500991108     C                   CLEAR                   AR6POR
145600991108     C                   CLEAR                   AR6SV1
145700991108     C                   CLEAR                   AR6VA1
145800991108     C                   CLEAR                   AR6SV2
145900991108     C                   CLEAR                   AR6VA2
146000991108     C                   CLEAR                   AR6SV3
146100991108     C                   CLEAR                   AR6VA3
146200991108     C                   CLEAR                   AR6IMV
146300991108     C                   CLEAR                   AR6FIM
146400991108     C                   CLEAR                   AR6ALI
146500991108     C                   CLEAR                   AR6CEI
146600991108     C                   CLEAR                   AR6IFT
146700991108     C                   CLEAR                   AR6FIV
146800991108     C                   CLEAR                   AR6NFT
146900991108     C                   CLEAR                   AR6DFT
147000991108     C                   ENDSR
147100000000     C**----------------------------------------
147200900207     C** STAMPA ERRORI
147300000000     C**----------------------------------------
147400900530     C     ERROR         BEGSR
147500111031    1c                   if        *in16 = *off
147600111031     c
147700111031    2C     *IN99         IFEQ      *OFF
147800111031     c
147900111031    3c     err           ifne      er(3)
148000040326     c     err           andne     er(11)
148100040326     C                   movel(p)  $Cmd(5)       Comman
148200040326     C                   call      'QCMDEXC'                            21
148300040326     C                   parm                    Comman
148400040326     C                   parm                    Lung
148500980109     C                   OPEN      PRTF198
148600980109     C                   SETON                                        99OF
148700040326     c                   exsr      SendMSGe
148800111031    3c                   endif
148900111031    2C                   ENDIF
149000111031     c
149100111031    2C     *IN89         IFEQ      *OFF
149200040325     C                   movel(p)  $Cmd(4)       Comman
149300040325     C                   call      'QCMDEXC'                            21
149400040325     C                   parm                    Comman
149500040325     C                   parm                    Lung
149600040326     C                   OPEN      PRTF198X
149700040325     C                   SETON                                        89OE
149800111031    2C                   ENDIF
149900950118     C*
150000900711     C                   EXSR      STAMPA
150100941129     C*
150200950127     C* 98 NON SCRIVO IL FILE DEGLI ERRORI SE VIENE LASCIATO NEI FILES
150300950127     C*    DI RICEZIONE
150400950127     C*
150500950127     C     *IN98         IFEQ      *OFF
150600950222     C     *IN16         ANDEQ     *OFF
150700991108     C                   WRITE     FIBLPEA
150800000906     C     *IN95         IFEQ      *ON
150900000906     C     KASS          SETLL     TITAH00R
151000000906     C     KASS          READE     TITAH00R                               32
151100000906    3C     *IN32         DOWEQ     *OFF
151200000906     C                   WRITE     TITAHE
151300000906     C     KASS          READE     TITAH00R                               32
151400000906    3C                   ENDDO
151500000906     C                   ENDIF
151600991104     C     *IN93         IFEQ      *ON
151700991104     C     KASS          SETLL     FIAR600R
151800991104     C     KASS          READE     FIAR600R                               32
151900991104    3C     *IN32         DOWEQ     *OFF
152000991104     C                   WRITE     FIAR6EC
152100991104     C     KASS          READE     FIAR600R                               32
152200991104    3C                   ENDDO
152300991104     C                   ENDIF
152400970227     C     *IN96         IFEQ      *ON
152500060220     C     KASS          SETLL     Fiar400R
152600060220     C     KASS          READE     Fiar400R                               32
152700970227    3C     *IN32         DOWEQ     *OFF
152800060220     C                   WRITE     Fiar4E
152900060220     C     KASS          READE     Fiar400R                               32
153000970227    3C                   ENDDO
153100970227     C                   ENDIF
153200990621     C     *IN97         IFEQ      *ON
153300991104     C     KASS          SETLL     FIAR700R
153400991104     C     KASS          READE     FIAR700R                               32
153500990621    3C     *IN32         DOWEQ     *OFF
153600991104     C                   WRITE     FIAR7E
153700991104     C     KASS          READE     FIAR700R                               32
153800990621    3C                   ENDDO
153900990621     C                   ENDIF
154000030108    2C                   If        *IN91
154100030108     C     KASS          setll     FIAR500R
154200030108     C     KASS          reade     FIAR500R                               32
154300030108    3C                   dow       Not *IN32
154400030108     C                   WRITE     FIAR5E
154500030108     C     KASS          reade     FIAR500R                               32
154600030108   -3C                   enddo
154700030108   -2C                   Endif
154800941129     C                   ENDIF
154900111031    1c                   endif
155000941129     C*
155100111031     c* Manca PdC
155200950117     C   16              DO
155300950117     C                   MOVE      *BLANK        ARBATB
155400950117     C                   MOVE      BLPAAS        ARBAAS
155500950117     C                   MOVE      BLPLNP        ARBLNP
155600950117     C                   MOVE      BLPNRS        ARBNRS
155700950117     C                   MOVE      BLPNSP        ARBNSP
155800950117     C                   MOVE      BLPCBO        ARBCBO
155900950117     C                   MOVEL     BLPDIM        ARBDTV
156000950117     C                   MOVE      *ZERO         ARBORV
156100950117     C                   MOVEL     'MI'          ARBCVB
156200950118     C                   MOVEL     'EDPNEW'      ARBPRU
156300950117     C                   MOVEL     BLPKSC        ARBCCM
156400950117     C                   MOVEL     BLPRSM        ARBRSM
156500950117     C                   MOVEL     BLPINM        ARBINM
156600950117     C                   MOVEL     BLPCAM        ARBCAM
156700950117     C                   MOVEL     BLPLOM        ARBLOM
156800950117     C                   MOVEL     BLPPRM        ARBPRM
156900950117     C                   MOVEL     BLPNZM        ARBNZM
157000950117     C                   MOVEL     BLPFAP        ARBFAP
157100100303     C                   MOVEL     ARBccm        ARBFEV
157200950117     C                   MOVE      *ZERO         ARBDAT
157300960918     C                   MOVEL     ERR           ARBDER
157400100303     c                   clear                   arblna
157500950117     C                   WRITE     FNARBM00
157600950119     C                   ENDDO
157700000000     C                   ENDSR
157800911107     C/SPACE 005
157900000000     C**----------------------------------------
158000000000     C**  STAMPA ERRORI
158100000000     C**----------------------------------------
158200900530     C     STAMPA        BEGSR
158300930222     C* SALTO PAGINA
158400040326     c     err           ifeq      er(3)
158500040326     c     err           oreq      er(11)
158600040325     C   OE              EXCEPT    HDGF
158700040325     C   OE              SETOFF                                       OE
158800040325     c                   else
158900930222     C   OF              EXCEPT    HDG
159000930222     C   OF              SETOFF                                       OF
159100040325     c                   endif
159200900601     C**
159300941128     C                   MOVEL     BLPRSD        STARSD           20
159400950114     C                   MOVE      BLPAAS        WAAS2             2 0
159500040326     c     err           ifeq      er(3)
159600040326     c     err           oreq      er(11)
159700040325     C                   EXCEPT    ERROF
159800040325     c                   else
159900000000     C                   EXCEPT    ERRO
160000040325     c                   endif
160100000000     C                   ENDSR
160200900526     C**---------------------------------------------------------
160300900526     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
160400900526     C**---------------------------------------------------------
160500900526     C     CAEXA         BEGSR
160600971212     C                   MOVE      *BLANK        VAR11            11
160700971212     C                   MOVE      *BLANK        VAR12            12
160800900608     C                   MOVE      ')'           VAR11
160900900526     C                   MOVEL     MBRFIL        VAR11
161000900608     C                   MOVE      '))'          VAR12
161100900526     C                   MOVEL     MBRFIL        VAR12
161200111117     C                   Z-ADD     56            LUNG             15 5
161300900526     C*
161400991103     C** FIBLPA0R
161500941129     C                   SETOFF                                       2021
161600911107     C                   MOVE      VAR11         CMA1
161700900526     C                   MOVEL     *BLANKS       COMMAN
161800911107     C                   MOVEA     CMA1(1)       COMMAN           80
161900900526     C                   CALL      'QCMDEXC'                            20
162000900526     C                   PARM                    COMMAN
162100900526     C                   PARM                    LUNG
162200900526     C   20              GOTO      FINE
162300900526     C** ALLOCO MEMBRO
162400900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
162500900526     C                   SETOFF                                       20
162600911107     C                   MOVE      VAR11         CMA3
162700900526     C                   MOVEL     *BLANKS       COMMAN
162800911107     C                   MOVEA     CMA3(1)       COMMAN
162900900526     C                   CALL      'QCMDEXC'                            20
163000900526     C                   PARM                    COMMAN
163100900526     C                   PARM                    LUNG
163200900526     C   20              GOTO      FINE
163300900526     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
163400900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
163500991103     C                   OPEN      FIBLPA0R
163600000906     C*
163700000906     C** TITAH00R
163800000906     C                   SETOFF                                       2021
163900000906     C                   MOVE      VAR11         CMH1
164000000906     C                   MOVEL     *BLANKS       COMMAN
164100000906     C                   MOVEA     CMH1(1)       COMMAN           80
164200000906     C                   CALL      'QCMDEXC'                            21
164300000906     C                   PARM                    COMMAN
164400000906     C                   PARM                    LUNG
164500000906     C   21              GOTO      SOTTOH
164600000906     C** ALLOCO MEMBRO
164700000906     C** SE NON RIESCE CHIUDO IL PROGRAMMA
164800000906     C                   SETOFF                                       20
164900000906     C                   MOVE      VAR11         CMH3
165000000906     C                   MOVEL     *BLANKS       COMMAN
165100000906     C                   MOVEA     CMH3(1)       COMMAN
165200000906     C                   CALL      'QCMDEXC'                            20
165300000906     C                   PARM                    COMMAN
165400000906     C                   PARM                    LUNG
165500000906     C   20              GOTO      FINE
165600000906     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
165700000906     C** SE NON RIESCE CHIUDO IL PROGRAMMA
165800000906     C                   OPEN      TITAH00R
165900000906     C                   SETON                                        95
166000000906     C     SOTTOH        TAG
166100030108     C*
166200030108     C** FIAR500R
166300030108     C                   setoff                                       2021
166400030108     C                   move      VAR11         $Cmd(1)
166500030108     C                   movel(p)  $Cmd(1)       Comman
166600030108     C                   call      'QCMDEXC'                            21
166700030108     C                   parm                    Comman
166800030108     C                   parm                    Lung
166900030108     C                   IF        Not *in21
167000030108     C** Alloco il membro:
167100030108     C** se NON riesce chiudo il programma
167200030108     C                   move      VAR11         $Cmd(3)
167300030108     C                   movel(p)  $Cmd(3)       Comman
167400030108     C                   call      'QCMDEXC'                            20
167500030108     C                   parm                    Comman
167600030108     C                   parm                    Lung
167700030108     C   20              goto      FINE
167800030108     C** Eseguo OVRDBF del membro desiderato:
167900030108     C** se NON riescr chiudo il programma
168000030108     C                   open      FIAR500R
168100030108     C                   eval      *in91 = *on
168200030108     C                   ENDIF
168300911108     C*
168400991103     C** FIAR600R
168500941129     C                   SETOFF                                       2021
168600911108     C                   MOVE      VAR11         CMC1
168700911108     C                   MOVEL     *BLANKS       COMMAN
168800911108     C                   MOVEA     CMC1(1)       COMMAN           80
168900941129     C                   CALL      'QCMDEXC'                            21
169000911108     C                   PARM                    COMMAN
169100911108     C                   PARM                    LUNG
169200941129     C   21              GOTO      SOTTOC
169300911108     C** ALLOCO MEMBRO
169400911108     C** SE NON RIESCE CHIUDO IL PROGRAMMA
169500911108     C                   SETOFF                                       20
169600911108     C                   MOVE      VAR11         CMC3
169700911108     C                   MOVEL     *BLANKS       COMMAN
169800911108     C                   MOVEA     CMC3(1)       COMMAN
169900911108     C                   CALL      'QCMDEXC'                            20
170000911108     C                   PARM                    COMMAN
170100911108     C                   PARM                    LUNG
170200911108     C   20              GOTO      FINE
170300911108     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
170400911108     C** SE NON RIESCE CHIUDO IL PROGRAMMA
170500991103     C                   OPEN      FIAR600R
170600911108     C                   SETON                                        93
170700911108     C     SOTTOC        TAG
170800900526     C*
170900991103     C** FIAR700R
171000941129     C                   SETOFF                                       2021
171100900526     C                   MOVE      VAR11         CMD1
171200900526     C                   MOVEL     *BLANKS       COMMAN
171300900526     C                   MOVEA     CMD1(1)       COMMAN           80
171400941129     C                   CALL      'QCMDEXC'                            21
171500900526     C                   PARM                    COMMAN
171600900526     C                   PARM                    LUNG
171700941129     C   21              GOTO      SOTTO2
171800941114     C*
171900900526     C** ALLOCO MEMBRO
172000900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
172100900526     C                   SETOFF                                       20
172200900526     C                   MOVE      VAR11         CMD3
172300900526     C                   MOVEL     *BLANKS       COMMAN
172400900526     C                   MOVEA     CMD3(1)       COMMAN
172500900526     C                   CALL      'QCMDEXC'                            20
172600900526     C                   PARM                    COMMAN
172700900526     C                   PARM                    LUNG
172800900526     C   20              GOTO      FINE
172900900526     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
173000900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
173100991103     C                   OPEN      FIAR700R
173200900530     C                   SETON                                        97
173300941129     C     SOTTO2        TAG
173400970212     C*
173500060220     C** Fiar400R
173600970212     C                   SETOFF                                       2021
173700970212     C                   MOVE      VAR11         CME1
173800970212     C                   MOVEL     *BLANKS       COMMAN
173900970212     C                   MOVEA     CME1(1)       COMMAN           80
174000970212     C                   CALL      'QCMDEXC'                            21
174100970212     C                   PARM                    COMMAN
174200970212     C                   PARM                    LUNG
174300970212     C   21              GOTO      SOTTO3
174400970212     C*
174500970212     C** ALLOCO MEMBRO
174600970212     C** SE NON RIESCE CHIUDO IL PROGRAMMA
174700970212     C                   SETOFF                                       20
174800970212     C                   MOVE      VAR11         CME3
174900970212     C                   MOVEL     *BLANKS       COMMAN
175000970212     C                   MOVEA     CME3(1)       COMMAN
175100970212     C                   CALL      'QCMDEXC'                            20
175200970212     C                   PARM                    COMMAN
175300970212     C                   PARM                    LUNG
175400970212     C   20              GOTO      FINE
175500970212     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
175600970212     C** SE NON RIESCE CHIUDO IL PROGRAMMA
175700060220     C                   OPEN      Fiar400R
175800970212     C                   SETON                                        96
175900970212     C     SOTTO3        TAG
176000080424     c
176100080424     c* Apertura file di filiale FNBLT
176200080424     c                   If        %subst(knsif:7:1) = 'P'
176300080424     c                   Eval      ovrdbf_blt = 'FILTRAPRD/FNBLT01L'
176400080424     c                   Else
176500080424     c                   Eval      ovrdbf_blt = 'FILTRA201/FNBLT01L'
176600080424     c                   EndIf
176700080424     c                   open      fnblt01l
176800970212     C*
176900941129     C     FINE          ENDSR
177000941114     C**---------------------------------------------------------
177100950112     C* LEGGO E AGGIORNO LE VARIE IN DETTAGLIO
177200941114     C**---------------------------------------------------------
177300950112     C     SR7           BEGSR
177400040112     C**!!!              MOVEL     '1'           WTRC
177500991104     C     KASS1         SETLL     FIAR700R
177600950112     C                   DO        *HIVAL
177700991104     C     KASS1         READE     FIAR700R                               60
177800950112     C  N60              DO
177900950112     C                   ADD       1             NREC2             7 0
178000000627     C     *IN52         IFEQ      *OFF
178100990618     C                   CLEAR                   TITA7000
178200950112     C                   Z-ADD     TASAAS        TA7AAS
178300950112     C                   Z-ADD     TASLNP        TA7LNP
178400950112     C                   Z-ADD     TASNRS        TA7NRS
178500950112     C                   Z-ADD     TASNSP        TA7NSP
178600950112     C                   MOVEL     TASTBL        TA7TBL
178700991104     C                   MOVEL     AR7SVN        TA7SVN
178800991104     C                   Z-ADD     AR7VAN        TA7VAN
178900990621     C                   WRITE     TITA7000
179000000627     C                   ENDIF
179100950112     C                   END
179200950112     C  N60              END
179300941114     C                   ENDSR
179400990618     C*
179500941114     C**---------------------------------------------------------
179600941114     C* PARTI COMUNI DI RIEMPIMENTO DELLE 2 BOLLE
179700941114     C**---------------------------------------------------------
179800941114     C     RIETAS        BEGSR
179900941114     C                   Z-ADD     BLPLNP        TASLNP
180000941114     C                   Z-ADD     BLPMGS        TASMGS
180100990618     C                   Z-ADD     BLPAAS        TASAAS
180200941114     C                   Z-ADD     BLPNRS        TASNRS
180300941114     C                   Z-ADD     BLPNSP        TASNSP
180400000628     C* Se esiste TNCSB non devo riaggiornare il codice bolla se la
180500000628     C* bolla che sto ricevendo non ha il C/A
180600000628     C* Aggiorno il codice bolla solo se:
180700000628     c* - sto ricevendo una nuova bolla
180800000628     c* - sto ricevendo aggiornamento di bolla autogenerata e non esist
180900010117     c*   record di tncsb
181000000628     c* - sto ricevendo aggiornamento di bolla autogenerata ed esiste
181100000628     c*   record di tncsb ed anche la bolla in ricezione ha il c/a
181200000628     C     *IN52         IFEQ      *OFF
181300000628     C     WESCSB        ORNE      '1'
181400000628     C     §3AFCA        ORGT      0
181500990618     C                   MOVEL     BLPCBO        TASCBO
181600000627     C                   ENDIF
181700941114     C                   MOVE      BLPTSP        TASTSP
181800941114     C     TASTSP        IFEQ      ' '
181900941114     C                   MOVEL     'C'           TASTSP
182000941114     C                   ENDIF
182100941114     C                   MOVE      BLPFIN        TASFIN
182200941114     C     TASFIN        IFEQ      ' '
182300941114     C                   MOVEL     'C'           TASFIN
182400941114     C                   ENDIF
182500990621     C* P.IVA E CCM LI IMPOSTO IN BASE A TIPO BOLLA (FRANCO O ASSEGN.)
182600990621     C* (SE BOLLA FRANCO    CODIFICATO NON HO LA PARTITA IVA
182700990621     C*  SE   "     "   NON CODIFICATO LA PARTITA IVA E' DEL MITTENTE
182800990621     C*     E LA DEVO QUINDI MEMORIZZARE IN TITAA00F (QUINDI NON QUI))
182900990621     C     FLGTB1        IFEQ      'A'
183000990621     C                   Z-ADD     BLPCCM        TASCCM
183100990621     C     TASCCM        IFEQ      *ZEROS
183200990621     C                   MOVEL     BLPLNP        TASCCM
183300990621     C                   MOVE      8888          TASCCM
183400990621     C                   END
183500990621     C                   MOVEL     BLPCPI        TASCPD
183600990621     C                   ELSE
183700990621     C                   Z-ADD     BLPKSC        TASCCM
183800990621     C                   END
183900941114     C                   Z-ADD     BLPLNA        TASLNA
184000941114     C                   Z-ADD     BLPNCL        TASNCL
184100950114     C                   Z-ADD     BLPPKB        TASPKB
184200990628     C                   MOVEL     'R'           TASFPB
184300991104     C                   Z-ADD     BLPPKF        TASPKF
184400990628     C                   MOVEL     'R'           TASFPF
184500950114     C                   Z-ADD     BLPVLC        TASVLC
184600941114     C                   Z-ADD     BLPNCR        TASNCR
184700000627     C     *IN52         IFEQ      *OFF
184800000627     C     TASFVF        ORNE      'T'
184900000627     C     TASFVF        ANDNE     'Z'
185000000627     C     BLPVLF        ANDGT     TASVLF
185100950114     C                   Z-ADD     BLPVLF        TASVLF
185200950114     C                   MOVE      BLPFVF        TASFVF
185300000627     C                   ENDIF
185400950114     C                   Z-ADD     BLPVLB        TASVLB
185500950114     C                   MOVE      BLPFVB        TASFVB
185600990817     C*
185700070223      * LE SPEDIZIONI DOVRANNO ESSERE SEMPRE 'B'
185800070223      * (SDI NON esiste più dal 2001 compreso...)
185900001201     C                   MOVE      'B'           TASSCL
186000971215     C*
186100941114     C                   Z-ADD     BLPRMN        TASRMN
186200990615     C*
186300950114     C                   MOVEL     BLPFLB        TASFLB
186400941114    1C     BLPFLP        IFGT      0
186500941114     C* C'E' TRANSITO SE :
186600941114     C* 1) BLPFLP NON E' = ALLA LINEA DI ARRIVO
186700941114     C* 2) BLPFLP NON E' = AL TERMINAL DI ARRIVO DELLA LINEA ARRIVO
186800040420     c*  20/4/04 il 3° e 4° caso non esistono più: transito ammesso per
186900040420     c*    stesso terminal di partenza <> terminal di arrivo
187000971202     C* 3) BLPFLP NON E' = AL TERMINAL PARTENZA  DELLA LINEA ARRIVO
187100971202     C* 4) TER PARTENZA DI BLPFLP NON E' = AL TERMINAL PARTENZA
187200991104     C*                                        DELLA LINEA ARRIVO
187300991104    2C     BLPFLP        IFNE      BLPTFA
187400991104     C     BLPFLP        ANDNE     BLPLNA
187500040420     C                   Z-ADD     BLPFLP        TASFLP
187600040420     C                   ENDIF
187700040603      * se la spedizione ha data >= 19/07/2004 non devo più impostare TASFLP
187800040603     c                   If        blpdsp >= 20040719
187900040603     c                   Clear                   Tasflp
188000040603     c                   EndIf
188100040420     C
188200941114    1C                   ENDIF
188300941114     C*
188400941114     C                   MOVEL     BLPFAP        TASFAP
188500941114     C     BLPFAP        IFEQ      ' '
188600941114     C                   MOVEL     'C'           TASFAP
188700941114     C                   ENDIF
188800970714     C                   MOVEL     O95CTS        TASMCT
188900990621     C                   MOVEL     BLPRSD        TASRSD
189000990621     C                   MOVEL     BLPIND        TASIND
189100990621     C                   MOVEL     BLPCAD        TASCAD
189200990621     C                   MOVEL     BLPLOD        TASLOD
189300990621     C                   MOVEL     BLPPRD        TASPRD
189400990621     C                   MOVEL     BLPNZD        TASNZD
189500941114     C                   MOVEL     BLPCAM        TASCAM
189600950118     C                   MOVEL     BLPNZM        TASNZM
189700941114     C                   MOVE      BLPFFD        TASFFD
189800941114     C                   MOVE      BLPCTS        TASCTS
189900941114     C                   Z-ADD     BLPTFA        TASTFA
190000941114     C                   Z-ADD     BLPTFP        TASTFP
190100941114     C                   Z-ADD     BLPZNC        TASZNC
190200950114     C                   MOVE      BLPFDN        TASFDN
190300941114     C* CAMPI NUOVI
190400050311     C                   z-add     blpdim        TASDIM
190500950114     C                   Z-ADD     BLPPDR        TASPDR
190600950126     C                   Z-ADD     BLPDRT        TASDRT
190700991104     C                   MOVEL     BLPFPP        TASFPP
190800950117     C                   MOVEL     BLPSOP        TASSOP
190900950117     C                   MOVEL     BLPFBR        TASFBR
191000950117     C                   MOVEL     BLPGGA        TASGGA
191100950117     C                   MOVEL     BLPGMA        TASGMA
191200950117     C                   MOVEL     BLPGVA        TASGVA
191300950118     C                   MOVEL     BLPNRT        TASNRT
191400950118     C                   Z-ADD     BLPQFT        TASQFT
191500991104     C                   MOVEL     BLPFNS        TASFNS
191600991104     C                   MOVEL     BLPGC1        TASGC1
191700991104     C                   MOVEL     BLPGC2        TASGC2
191800160412     C*****              MOVEL     BLPNAS        TASNAS
191900991104     C                   Z-ADD     BLPNCP        TASNCP
192000991104     C                   Z-ADD     BLPPKC        TASPKC
192100991104     C                   Z-ADD     BLPFLS        TASFLS
192200991104     C                   Z-ADD     BLPNCD        TASNCD
192300991104     C                   Z-ADD     BLPNCA        TASNCA
192400991104     C                   MOVEL     BLPXCO        TASXCO
192500991104     C                   MOVEL     BLPCTM        TASCTM
192600991104     C                   Z-ADD     BLPDBR        TASDBR
192700991104     C                   Z-ADD     BLPDUC        TASDUC
192800030401      * imposto i campi per TASFLO
192900030401     c                   Clear                   Dtasflo
193000041202     c   52              movel     tasflo        dtasflo
193100030401     c                   Eval      kApdPdr = TasPdr
193200030401     c     kFiapd        Chain     Fiapd01l
193300030401     c                   If        Not %Found(Fiapd01l) or ApdAtb = 'A' or
193400030401     c                             ApdPdd = 'S'
193500030401     c                   Eval      §FloSca = 'C'
193600030401     c                   Eval      §FloEti = 'C'
193700030401     c                   EndIf
193800030401     c                   Eval      Tasflo = Dtasflo
193900060303     c* se bolla "M "-prepagato annullato imposto data consegna merce = dbr
194000060303     c                   if        blpcbo = 'M '
194100060303     c                   z-add     blpdbr        tasdcm
194200060331     c                   z-add     blpdbr        tasddc
194300060331     c                   z-add     9999999       tasndc
194400060303     c                   endif
194500941114     C*
194600941114     C                   ENDSR
194700000822     C**---------------------------------------------------------
194800000822     C** SCRITTURA FILE ERRORI PER UFFICIO ASSICURAZIONE MERCI
194900000822     C**---------------------------------------------------------
195000000823     C** .. Solo se non già presente
195100000823     C*
195200100225     C***  SCRASS        BEGSR
195300100225     C***  KEAS          SETLL     FIARBE2C                               38
195400100225 --  C***  *IN38         IFEQ      *OFF
195500100225     C***
195600100225     C***                CLEAR                   FIARBTEE
195700100225     C***
195800100225     C***                Z-ADD     BLPAAS        ARBAAS
195900100225     C***                Z-ADD     BLPLNP        ARBLNP
196000100225     C***                Z-ADD     BLPNRS        ARBNRS
196100100225     C***                Z-ADD     BLPNSP        ARBNSP
196200100225     C***                Z-ADD     BLPLNA        ARBLNA
196300100225     C***                MOVE      '1'           ARBTRC
196400100225     C***                MOVE      BLPCBO        ARBCBO
196500100225     C***                Z-ADD     BLPDIM        ARBDTV
196600100225     C***                Z-ADD     BLPIAS        ARBIAS
196700100225     C***                MOVEL     BLPVAS        ARBVAS
196800100225     C***                Z-ADD     603           ARBFEV
196900100225     C***                MOVEL     EAS(1)        ARBDER
197000100225     C***                MOVE      'R'           ARBVAR
197100100225     C***
197200100225     C***                WRITE     FIARBTEE
197300100225     C***
197400100225 --  C***                ENDIF
197500100225     C***                ENDSR
197600941114     C**---------------------------------------------------------
197700941114     C* ROUTINE INIZIALE
197800941114     C**---------------------------------------------------------
197900941114     C     *INZSR        BEGSR
198000941114     C*
198100941114     C     *ENTRY        PLIST
198200941114     C                   PARM                    KPJBA
198300941114     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
198400941114     C* ASSUMO IL PRIMO MEMBRO DEL FILE
198500941114     C     MBRFIL        IFEQ      *BLANK
198600941129     C                   MOVEL     'M000  '      MBRFIL
198700941114     C                   END
198800941114     C*
198900030128     C                   CALL      'X§PARUT'
199000030128     C                   PARM                    UT§DSE0F
199100941114     C                   MOVEL     RAGUT         RSUT             20
199200030128     C                   MOVEL     REC80         CNCR80
199300941114     C*--- RICERCA CAPOCONTI------------------------------------------*
199400941114     C                   DO        50            X                 2 0
199500941114     C                   MOVE      TCU(X)        TCUDS
199600941114     C     F56           CABNE     'CG'          END1
199700941114     C     F4            COMP      '1'                                    21
199800941114     C     F4            COMP      '2'                                    22
199900941114     C     F4            COMP      '3'                                    23
200000941114     C     F4            COMP      '6'                                    27
200100941114     C** 1 CLIENTI   21
200200941114     C** 2 FORNITORI 22
200300941114     C** 3 AGENTI    23
200400941114     C     F3            COMP      '0'                                242425
200500941114     C     F3            COMP      'I'                                    26
200600941114     C** 0 ITALIA   25
200700941114     C** 1 ESTERO   24
200800941114     ** I CAPO CONTO IVA
200900941114     C   21
201000941114     CAN 24              Z-ADD     KCU(X)        KCE               4 0
201100941114     C   21
201200941114     CAN 25              Z-ADD     KCU(X)        KCI               4 0
201300941114     C   22
201400941114     CAN 24              Z-ADD     KCU(X)        KFE               4 0
201500941114     C   22
201600941114     CAN 25              Z-ADD     KCU(X)        KFI               4 0
201700941114     C   23
201800941114     CAN 24              Z-ADD     KCU(X)        KAE               4 0
201900941114     C   23
202000941114     CAN 25              Z-ADD     KCU(X)        KAI               4 0
202100941114     C   26              Z-ADD     KCU(X)        KIVA              4 0
202200941114     C   27              Z-ADD     KCU(X)        KBNA              4 0
202300941114     C     END1          TAG
202400941114     C                   END
202500941114     C*---------------------------------------------------------------*
202600941114     C     *LIKE         DEFINE    TASSV1        WSV1
202700990621     C     *LIKE         DEFINE    TAATRC        §TRC
202800991103     C     *LIKE         DEFINE    TAATRC        WTRC
202900000627     C     *LIKE         DEFINE    TASTBL        WTBL
203000000627     C     *LIKE         DEFINE    TA4TRC        SA4TRC
203100000628     C     *LIKE         DEFINE    CSBCAS        WARCAS
203200000628     C     *LIKE         DEFINE    CSBCAS        WDIFFE
203300000628     C     *LIKE         DEFINE    CSBVCA        WARVCA
203400000628     C     *LIKE         DEFINE    KPJBU         KPJBUX
203500941114     C*---------------------------------------------------------------*
203600941114     C*
203700941114     C     KTAB          KLIST
203800941114     C                   KFLD                    CODUT
203900941114     C                   KFLD                    COD               2
204000941114     C                   KFLD                    KEY               8
204100941114     C     KTAB2         KLIST
204200941114     C                   KFLD                    CODUT
204300941114     C                   KFLD                    COD               2
204400941114     C     KNSP          KLIST
204500990618     C                   KFLD                    BLPAAS
204600941114     C                   KFLD                    BLPLNP
204700941114     C                   KFLD                    BLPNRS
204800941114     C                   KFLD                    BLPNSP
204900000627     C     KNSP1         KLIST
205000000627     C                   KFLD                    BLPAAS
205100000627     C                   KFLD                    BLPLNP
205200000627     C                   KFLD                    BLPNRS
205300000627     C                   KFLD                    BLPNSP
205400000627     C                   KFLD                    WTBL
205500990621     C*
205600990621     C     KNSPE         KLIST
205700990621     C                   KFLD                    BLPAAS
205800990621     C                   KFLD                    BLPLNP
205900990621     C                   KFLD                    BLPNRS
206000990621     C                   KFLD                    BLPNSP
206100990621     C                   KFLD                    §TRC
206200941114     C* ACCESSO PER NUMERO DI SPEDIZIONE
206300941114     C     KASS          KLIST
206400990618     C                   KFLD                    BLPAAS
206500941114     C                   KFLD                    BLPLNP
206600990621     C                   KFLD                    BLPNRS
206700941114     C                   KFLD                    BLPNSP
206800991103     C* ACCESSO PER NUMERO DI SPEDIZIONE
206900991103     C     KASS1         KLIST
207000991103     C                   KFLD                    BLPAAS
207100991103     C                   KFLD                    BLPLNP
207200991103     C                   KFLD                    BLPNRS
207300991103     C                   KFLD                    BLPNSP
207400991103     C                   KFLD                    WTRC
207500061218     C     KASS2         KLIST
207600061218     C                   KFLD                    BLPAAS
207700061218     C                   KFLD                    BLPLNP
207800061218     C                   KFLD                    BLPNRS
207900061218     C                   KFLD                    BLPNSP
208000061218     C                   KFLD                    kTRD
208100941114     C* ACCESSO PER NUMERO DI SPEDIZIONE  AI FILE AGGANCIATI
208200941114     C*§
208300000823     C* SETLL PER UFFICIO ASSICURAZIONE MERCI
208400000823     C     KEAS          KLIST
208500000823     C                   KFLD                    BLPAAS
208600000823     C                   KFLD                    BLPLNP
208700000823     C                   KFLD                    BLPNRS
208800000823     C                   KFLD                    BLPNSP
208900000823     C                   KFLD                    EAS(1)
209000000823     C                   KFLD                    W08ZER            8 0
209100000823     C                   KFLD                    BLPDIM
209200000823     C                   KFLD                    W06ZER            6 0
209300030401
209400030401      * File Fiapd01l
209500030401     c     kFiapd        Klist
209600030401     c                   Kfld                    kApdTip
209700030401     c                   Kfld                    kApdPdr
209800000823     C*
209900000823     C                   Z-ADD     0             W08ZER
210000000823     C                   Z-ADD     0             W06ZER
210100000823     C*
210200030108     C* GIRO DATA DEL GIORNO
210300030108     C                   TIME                    W0140
210400030108     C                   MOVE      W0140         G02DAT
210500030108     C                   MOVEL     *BLANK        G02ERR
210600030108     C                   CALL      'XSRDA8'
210700030108     C                   PARM                    WLBDAT
210800030108     C                   Z-ADD     G02INV        DATEU
210900030128     C                   movel     w0140         TimeU
211000030108     C*
211100941114     C                   SETON                                        OF
211200040325     C                   SETON                                        OE
211300000627     C* CARICO IN SCHIERA ELENCO TIPI RECORD TITA4 CANCELLABILI
211400000628     C                   Z-ADD     0             II                3 0
211500000627     C                   CLEAR                   SDL
211600000627     C     'TA4'         SETLL     TNTBE01L
211700000627     C     'TA4'         READE     TNTBE01L                               32
211800000627    1C     *IN32         DOWEQ     *OFF
211900000627    2C     TBEATB        IFEQ      *BLANKS
212000000627     C                   MOVEL     TBEUNI        DTA4
212100000627    3C     §TA4DL        IFEQ      'S'
212200000627     C                   ADD       1             II
212300000627     C                   MOVEL     TBEKE1        SDL(II)
212400000627    3C                   ENDIF
212500000627    2C                   ENDIF
212600000627     C     'TA4'         READE     TNTBE01L                               32
212700000627    1C                   ENDDO
212800030128      *
212900030128      * Effettuo caricamento dei tipi record FIAR5 da NON trasmettere
213000030128     c                   clear                   ii
213100030128     c                   clear                   AR5n
213200030128     c                   eval      TBEcod   = 'AR5'
213300030128     c     TBEcod        setll     TNTBE000
213400030128     c     TBEcod        reade     TNTBE000
213500030128do  1c                   DOW       Not %Eof(TNTBE01L)
213600030128if  2c                   if        TBEatb   = *blanks
213700030128     c                   eval      dAR5 = TBEuni
213800030128if  3c                   IF        §AR5trmi <> 'S'
213900030128     c                   add       1             ii
214000030128     c                   movel     TBEke1        AR5n(ii)
214100030128e   3c                   ENDIF
214200030128e   2c                   endif
214300030128     c     TBEcod        reade     TNTBE000
214400030128e   1c                   ENDDO
214500000628     C*
214600000628     C* CARICO IN SCHIERA ELENCO DIVISE ACCETTATE IN ITALIA
214700000628     C*
214800000628     C                   MOVEL     'CV'          COD
214900000628     C                   CLEAR                   DVA
215000011029     C                   CLEAR                   DEC
215100000628     C                   CLEAR                   II
215200000628     C     KTAB2         SETLL     TABEL
215300000628     C     KTAB2         READE     TABEL                                  32
215400000628     C     *IN32         DOWEQ     *OFF
215500000628     C     TBLFLG        IFEQ      ' '
215600000628     C                   MOVEL     TBLUNI        DSCV
215700000628     C     §CVDVA        IFEQ      'S'
215800000628     C                   ADD       1             II
215900000628     C                   MOVEL     TBLKEY        DVA(II)
216000011029     C                   Z-ADD     §CVDEC        DEC(II)
216100000628     C                   ENDIF
216200000628     C                   ENDIF
216300000628     C**
216400000628     C     KTAB2         READE     TABEL                                  32
216500000628     C                   ENDDO
216600001002     C*
216700001002     C* CARICO IN SCHIERA I TIPI BOLLA NON PREVISTI DALL'ASSICURAZ
216800001002     C*
216900001002     C                   MOVEL     'TB'          COD
217000001002     C                   CLEAR                   TBL
217100001002     C                   CLEAR                   II
217200001002     C     KTAB2         SETLL     TABEL
217300001002     C     KTAB2         READE     TABEL                                  32
217400001002     C     *IN32         DOWEQ     *OFF
217500001002     C     TBLFLG        IFEQ      ' '
217600001002     C                   MOVEL     TBLUNI        DSTB
217700001002     C     §TBFAS        IFEQ      '0'
217800001002     C                   ADD       1             II
217900001002     C                   MOVEL     TBLKEY        TBL(II)
218000001002     C                   ENDIF
218100001002     C                   ENDIF
218200001002     C**
218300001002     C     KTAB2         READE     TABEL                                  32
218400001002     C                   ENDDO
218500971215     C*
218600941114     C                   ENDSR
218700030416      **-------------------------------------------------------------**
218800030416      ** SENDMSG                                                     **
218900030416      **-------------------------------------------------------------**
219000030416     C     SendMSG       BEGSR
219100030416      *
219200111031     C                   movel     *blanks       wrkEml          253
219300111031     C                   movel     *blanks       wrkMsg         5000
219400111031     C                   movel     *blanks       wrkOgg           44
219500120305     c                   eval      wrkeml='ced@brt.it'
219600120305     c*
219700111031     c                   eval      wrkogg = 'ERRORE in ricezione bolle partite'
219800111031     c                   eval      wrkmsg = 'Membro: ' + MBRFIL
219900030416     c                                    + '   Job: ' + KNMEB
220000030416     c                                    +        '/' + KNMUS
220100030416     c                                    +        '/' + %char(KNRJO)
220200030416     c                                    + '   Pgm: ' + SDSpgm
220300111031     c                                    + '>>>>>  Rieseguire il lavoro <<<<<'
220400030416      *
220500111031     c                   call      'TIS701C'
220600111031     C                   parm                    wrkEml
220700111031     C                   parm                    wrkOgg
220800111031     C                   parm                    wrkMsg
220900030416      *
221000030416     C                   ENDSR
221100040325      **-------------------------------------------------------------**
221200040325      ** SENDMSGE -                                                  **
221300040325      **-------------------------------------------------------------**
221400040325     C     SendMSGE      BEGSR
221500111031      *
221600111031     C                   movel     *blanks       wrkEml          253
221700111031     C                   movel     *blanks       wrkMsg         5000
221800111031     C                   movel     *blanks       wrkOgg           44
221900120305     c                   eval      wrkeml='ced@brt.it'
222000120305     c*
222100111031     c                   eval      wrkogg = 'ERRORI Ricez.bolle partite'
222200111031     c                                    + ' fil. ' +%char(wmbr)
222300111031     c                   eval      wrkmsg = 'Controllare la stampa per ufficio'
222400111031     c                                    + ' CED in PRT01'
222500040325      *
222600111031     c                   call      'TIS701C'
222700111031     C                   parm                    wrkEml
222800111031     C                   parm                    wrkOgg
222900111031     C                   parm                    wrkMsg
223000040325      *
223100040325     C                   ENDSR
223200030416      *
223300061211      **-------------------------------------------------------------**
223400061211      * Ricevo eventuale estensione TESTATA BOLLA
223500061211      **-------------------------------------------------------------**
223600061211     c     AggFIAR5      BEGSR
223700061211     C                   If        *in91
223800061211     C     KASS          setll     FIAR500R
223900061211     C                   Do        *hival
224000061211     C     KASS          reade     FIAR500R                               32
224100061211     C   32              leave
224200110607     c                   if        ar5trd='EMD'
224300110607     c                   iter
224400110607     c                   endif
224500061211      * - fleggo come ricevuto in filiale
224600061211     C                   eval      AR5ft1 = 'R'
224700061211     C                   eval      AR5dt1 = DateU
224800061211      * - fleggo come ricevuto in sede
224900061211     C                   eval      AR5ft2 = 'R'
225000061211     C                   eval      AR5dt2 = DateU
225100061211      * - verifico se per il tipo record di FIAR5 è previsto il
225200061211      *   ritorno info al cliente: se non è previsto imposto
225300061211      *   il record come già trasmesso (cioè da non trasmettere).
225400061211     c                   eval      *in30  =  *off
225500061211     c     AR5trd        lookup    AR5n                                   30
225600061211if  3c                   IF        *in30
225700061211     c                   eval      AR5ft3 = 'T'
225800061211     c                   eval      AR5dt3 = *all'9'
225900061211x   3c                   ELSE
226000061211     c                   clear                   AR5ft3
226100061211     c                   clear                   AR5dt3
226200061211e   3c                   ENDIF
226300061211      * se sto ricevendo un rcd di tipo 'GEN'
226400061211      * controllo per impostare il flag dei colli di valore su titas
226500061218     c****               If        Ar5Trd = 'GEN'
226600061218     c****               Eval      dAr5Gen = Ar5Uni
226700061218     c****               If        §Ar5Bva = 'V'
226800061218     c****               Eval      dtasflo = TasFlo
226900061218     c****               Eval      §FloTba = §Ar5Bva
227000061218     c****               Eval      TasFlo = dtasflo
227100061218     c****               EndIf
227200061218     c****               EndIf
227300061211      *
227400061211     C                   WRITE     FIAR5000                             38      IGNORO SE GIA' PRESE
227500061211     C                   add       1             NRec5
227600061211     C                   Enddo
227700061211     C                   Endif
227800061211     c                   ENDSR
227900061211      **-------------------------------------------------------------**
228000061211      **    Aggiorno flag legami bolla sulla bolla figlia
228100061211      **-------------------------------------------------------------**
228200061211     c     AggLBL        BEGSR
228300061211     c
228400061211     c* Se esiste per la bolla un legame come figlia, aggiorno
228500061211     c     knsp          setll     fnlbl01l
228600061211     c     knsp          reade     fnlbl01l
228700061211     c                   dow       not %eof(fnlbl01l)
228800061211     c                   call      'FNLG25R'
228900061211     c                   parm                    recordlbl
229000061211     c                   parm                    i25tla            1
229100061211     c                   parm      'N'           i25agglbl         1
229200061211     C                   PARM                    o25fa1            1
229300061211     C                   PARM                    o25fca            1
229400061211     C                   PARM                    o25bofi           1
229500061211     C                   PARM                    o25boma           1
229600061211     c
229700061211     c* Aggiorno flag legame bolle e C/A
229800061211     c                   eval      tasfa1=o25fa1
229900061211     c                   eval      tasfca=o25fca
230000061211     c
230100061211     c* se ho trovato bolla mamma, aggiorno legame bolla coi dati
230200061211     c                   if        o25boma='S'
230300061211     c                   eval      lblft1='S'
230400061211     c* solo la prima volta aggiorno la data
230500061211     c                   if        lbldt1=0
230600080516     c                   movel     dateu         lbldt1
230700061211     c                   endif
230800061211     c                   endif
230900061211     c                   eval      lblft2='S'
231000061211     c                   if        lbldt2=0
231100080516     c                   movel     dateu         lbldt2
231200061211     c                   endif
231300061211     c
231400061211     c                   update    fnlbl000
231500061211     c
231600061211     c     knsp          reade     fnlbl01l
231700061211     c                   enddo
231800061211     c                   ENDSR
231900080828      **-------------------------------------------------------------**
232000080828      * controllo da DCLI de devo impostare data inventario ricez DDT
232100080828      **-------------------------------------------------------------**
232200100317     c**
232300100317     c**   contrDCLI     BEGSR
232400100317    1c**                 if        tasccm<>savccm
232500100317     c**                 Clear                   tibs02
232600100317     c**                 Clear                   dcli
232700100317     c**                 Movel     'C'           T02Mod
232800100317     c**                 Movel     KNSIF         T02Sif
232900100317     c**                 Movel     'CLI'         T02Cod
233000100317     c**                 Movel     tasccm        T02Ke1
233100100317     c**                 Call      'TIBS02R'
233200100317     c**                 Parm                    Kpjba
233300100317     c**                 Parm                    tibs02
233400100317    2c**                 If        T02Err = *Blanks
233500100317     c**                 Movel     T02Uni        Dcli
233600100317    2c**                 endif
233700100317     c**                 movel     tasccm        savccm            7 0
233800100317    1c**                 endif
233900100317     c**
234000100317    1c**                 if        §cliidt='S'
234100100317     c**   kass2         chain     fiar531c
234200100317    2c**                 if        %found(fiar531c)
234300100317     c**                 movel     ar5uni        dar5gen
234400100317     c**                 eval      §ar5drdt='99999999'
234500100317     c**                 movel     dar5gen       ar5uni
234600100317     c**                 update    fiar5000
234700100317   x2c**                 else
234800100317     c**                 clear                   fiar5000
234900100317     c**                 eval      ar5aas=tasaas
235000100317     c**                 eval      ar5lnp=taslnp
235100100317     c**                 eval      ar5nrs=tasnrs
235200100317     c**                 eval      ar5nsp=tasnsp
235300100317     c**                 eval      ar5trd='GEN'
235400100317     c**                 movel     dateu         ar5dac
235500100317     c**                 time                    ar5orc
235600100317     c**                 clear                   dar5gen
235700100317     c**                 eval      §ar5drdt='99999999'
235800100317     c**                 movel     dar5gen       ar5uni
235900100317     c**
236000100317     C**                 eval      AR5ft1 = 'R'
236100100317     C**                 eval      AR5dt1 = DateU
236200100317     C**                 eval      AR5ft2 = 'R'
236300100317     C**                 eval      AR5dt2 = DateU
236400100317     c**Per questo dato non devo trasmettere al cliente essendo un dato
236500100317     c** gestionale nei pgm e non per il cliente
236600100317     c**                 eval      AR5ft3 = 'T'
236700100317     c**                 eval      AR5dt3 = *all'9'
236800100317     c**
236900100317     C**                 WRITE     FIAR5000                             38      IGNORO SE GIA' PRESE
237000100317e   2c**                 ENDIF
237100100317    1c**                 ENDIF
237200100317     c**
237300100317     c**                 ENDSR
237400061211      **-------------------------------------------------------------**
237500950118     OPRTF198   E            HDG            2 02
237600000000     O                       RSUT                20
237700930222     O                                           60 '**  RICEZIONE  BOLLE  PA'
237800970303     O                                         +  0 'RTITE  **'
237900040326     o                                         +  5 'UFFICIO CED'
238000900601     O                       UDATE              110 '  /  /  '
238100991117     O                                          120 'TRTR36R'
238200000000     O                                          128 'PAG.'
238300000000     O                       PAGE          Z    132
238400900601     O          E            HDG            2
238500961022     O                                           16 'Punto Operativo:'
238600961022     O                       WMBR                21
238700961022     O                                           23 '-'
238800961022     O                       DESFIL              44
238900900601     O                                         + 10 'MEMBRO:'
239000900601     O                       MBRFIL            +  1
239100910619     O                                         + 10 'FILE:'
239200930222     O                       LEGFIL            +  1
239300941128     O          E            HDG            1
239400000000     O                       S(1)                66
239500000000     O                       S(2)               132
239600900604     O          E            HDG            1
239700000000     O                       S(3)                66
239800941128     O                       S(5)               132
239900941128     O          E            HDG            1
240000941128     O                       S(4)                66
240100941128     O                       S(6)               132
240200900713     O          E            HDG            1
240300000000     O                       S(1)                66
240400000000     O                       S(2)               132
240500000000     O          E            ERRO        1
240600950118     O                       WAAS2                2
240700941128     O                       BLPLNP        Z   +  1
240800941128     O                       BLPNRS        Z   +  1
240900941128     O                       BLPNSP        Z   +  1
241000950114     O                       BLPCBO            +  2
241100950118     O                       BLPKSC            +  1
241200950118     O                       BLPLNA        Z   +  1
241300941128     O                       STARSD            +  2
241400950118     O                       BL9CAS        4   +  1
241500941128     O                       BLPRMN        Z   +  1
241600950118     O                       ERR               +  1
241700950118     O                       ER05              +  1
241800040326     OPRTF198X  E            HDGF           2 02
241900040325     O                       RSUT                20
242000040325     O                                           60 '**  RICEZIONE  BOLLE  PA'
242100040325     O                                         +  0 'RTITE  **'
242200040326     o                                         +  5 'UFFICIO FATTURAZIONE'
242300040325     O                       UDATE              110 '  /  /  '
242400040325     O                                          120 'TRTR36R'
242500040325     O                                          128 'PAG.'
242600040325     O                       PAGE          Z    132
242700040325     O          E            HDGF           2
242800040325     O                                           16 'Punto Operativo:'
242900040325     O                       WMBR                21
243000040325     O                                           23 '-'
243100040325     O                       DESFIL              44
243200040325     O                                         + 10 'MEMBRO:'
243300040325     O                       MBRFIL            +  1
243400040325     O                                         + 10 'FILE:'
243500040325     O                       LEGFIL            +  1
243600040325     O          E            HDGF           1
243700040325     O                       S(1)                66
243800040325     O                       S(2)               132
243900040325     O          E            HDGF           1
244000040325     O                       S(3)                66
244100040325     O                       S(5)               132
244200040325     O          E            HDGF           1
244300040325     O                       S(4)                66
244400040325     O                       S(6)               132
244500040325     O          E            HDGF           1
244600040325     O                       S(1)                66
244700040325     O                       S(2)               132
244800040325     O          E            ERROF       1
244900040325     O                       WAAS2                2
245000040325     O                       BLPLNP        Z   +  1
245100040325     O                       BLPNRS        Z   +  1
245200040325     O                       BLPNSP        Z   +  1
245300040325     O                       BLPCBO            +  2
245400040325     O                       BLPKSC            +  1
245500040325     O                       BLPLNA        Z   +  1
245600040325     O                       STARSD            +  2
245700040325     O                       BL9CAS        4   +  1
245800040325     O                       BLPRMN        Z   +  1
245900040325     O                       ERR               +  1
246000040325     O                       ER05              +  1
246100001002**         CMA1
246200991103CHKOBJ OBJ(FIBLPA0R) OBJTYPE(*FILE)     MBR(M234567890)
246300911105**         CMA2
246400991103ALCOBJ OBJ((FIBLPA0R *FILE *EXCL           M234567890))
246500911105**         CMA3
246600991103OVRDBF FILE(FIBLPA0R)                   MBR(M234567890)
246700911105**         CMC1
246800991103CHKOBJ OBJ(FIAR600R) OBJTYPE(*FILE)     MBR(M234567890)
246900911105**         CMC2
247000991103ALCOBJ OBJ((FIAR600R *FILE *EXCL           M234567890))
247100911105**         CMC3
247200991103OVRDBF FILE(FIAR600R)                   MBR(M234567890)
247300900526**         CMD1
247400991103CHKOBJ OBJ(FIAR700R) OBJTYPE(*FILE)     MBR(M234567890)
247500900526**         CMD2
247600991103ALCOBJ OBJ((FIAR700R *FILE *EXCL           M234567890))
247700900526**         CMD3
247800991103OVRDBF FILE(FIAR700R)                   MBR(M234567890)
247900970212**         CME1
248000060220CHKOBJ OBJ(FIAR400R) OBJTYPE(*FILE)     MBR(M234567890)
248100970212**         CME2
248200060220ALCOBJ OBJ((FIAR400R *FILE *EXCL           M234567890))
248300970212**         CME3
248400060220OVRDBF FILE(FIAR400R)                   MBR(M234567890)
248500000906**         CMH1
248600000906CHKOBJ OBJ(TITAH00R) OBJTYPE(*FILE)     MBR(M234567890)
248700000906**         CMH2
248800000906ALCOBJ OBJ((TITAH00R *FILE *EXCL           M234567890))
248900000906**         CMH3
249000000906OVRDBF FILE(TITAH00R)                   MBR(M234567890)
249100030108** --- $CMD
249200111117CHKOBJ OBJ(FIAR500R) OBJTYPE(*FILE)      MBR(M234567890)
249300111117ALCOBJ OBJ((FIAR500R *FILE *EXCL            M234567890))
249400111117OVRDBF FILE(FIAR500R)                    MBR(M234567890)
249500040326OVRPRTF PRTF198X USRDTA('Uff.Fatt.')
249600111117OVRPRTF PRTF198 USRDTA('Uff.CED') OUTQ(PRT01) HOLD(*YES)
249700030108**     ER
249800950117CODICE BOLLA ERRATO        (NON CARICATA) 1
249900990903SPEDIZIONE ESISTENTE       (NON CARICATA) 2
250000950228COD.MITTENTE INESISTENTE IN ANAGRAF       3 - CARICATA
250100990903                                          4
250200950117CAP MITTENTE INESISTENTE   (NON CARICATA) 5
250300000629CAP MITTENTE INESISTENTE                  6 - CARICATA
250400941129BOLLA CREATA MA C/A GIA' IN TNCSB00F      7
250500941129BOLLA CREATA MA C/A ESISTE CON <> TIP BOL 8
250600941129BOLLA CREATA MA C/A ESISTE CON IMPORTO <> 9
250700041105
250800960918COD.MITTENTE BLOCCATO                    11 - CARICATA
250900041105ESISTE TITAA30C RECORD "M" (NON CARICATA)12
251000041105ESISTENTE RECORD IN TITA430C(NO CARICATA)13
251100041105ESISTENTE RECORD IN TITA730C(NO CARICATA)14
251200041105ESISTENTE RECORD IN TITAT30C(NO CARICATA)15
251300041105ESISTE TITAA30C RECORD "O" (NON CARICATA)16
251400000628SPED.AUTOGENERATA CONTABIL.(NON CARICATA)17
251500030108**     S
251600950118-----------------+---+-------+----+---------------------+---------  A1
251700950118--+---------------+-------------+---------------------------------  A2
251800991104  SPEDIZIONE     'COD'CODICE ' LIN' DESTINATARIO        '    C/ASS  B3
251900950118AN LNP SER NUMERO'BOL'SPEDIZ ' ARR'                     '           C4
252000991104EGNO  ' RIFERIMENTO '   N    O    T    E                            B5
252100991104      '    MITTENTE '                                               C6
252200030108**     MSG
252300000629AGGIOR. BOLLA AUTOGENERATA XXX XX XXXXXXX XXXX  LNA XXX               1
252400000629CON CONTRASSEGNO XXX 1234567890,123 XXXXXXXXXXXXXXXXXXXX              2
252500000629BOLLA PERVENUTA CON CONTRASSEGNO XXX 123456789,123                    3
252600000629MANTENUTO  CONTRASSEGNO INIZIALE XXX 123456789,123                    4
252700000629ASSUNTO    CONTRASSEGNO NUOVO    XXX 123456789,123                    5
252800000628RIATTIVATO CONTRASSEGNO                                               6
252900010117GIA' CONSEGNATA. ATTENZIONE: AGGIUNTO CONTRASSEGNO!                   7
253000000628                                                                      8
253100000629CON CONTRASSEGNO XXX 1234567890,123 PAGATO INTESTATO A 1234567        9
253200000629BOLLA PERVENUTA CON C/A XXX 123456789,123 INTESTATO A 1234567         0
253300000629MANTENUTO C/A INIZIALE XXX 123456789,123 INTEST. A 1234567            1
253400000629BOLLA PERVENUTA CON C/A INTESTATO A 1234567                           2
253500000629MANTENUTA  INTESTAZIONE INIZIALE    1234567                           3
253600000823**     EAS
253700001002IMMESSO IMP.ASSIC.  BOLLA NON ASSIC
