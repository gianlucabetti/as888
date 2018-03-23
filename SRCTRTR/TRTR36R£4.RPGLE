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
011900991104     D  §TA4RM       E                     EXTFLD(§TA4ARMA)
012000071106     D**  §TA4RP       E                     EXTFLD(§TA4ARPT)
012100071106     D** DSBL4E        E DS
012200030128     d*
012201151113     d dsbl4m        E DS
012300030128     d dAR5          e ds                  inz
012400030401     d dtasflo       e ds
012500150219      *--------------------------
012600150219     d Fiar5ds       e ds                  extname(FIAR531C)
012700150219     d TISIG20DS     e ds
012800150219      *--------------------------
012900020311
013000020311     d og143         e ds
013100040603     d dar5gen       e ds
013200130322     d dar5emd       e ds
013201151119     d dar5fat       e ds
013300050222     d fidn58ds      e ds
013400110519     d TRUL21DS      e ds
013500020311
013600970714     D* DS PER TISI95R - CONTROLLO CAP
013700970714     D DSSI95        E DS                  EXTNAME(TISI95DS)
013800030128      *
013900950227     D* PER ESPLOSIONE DATI IN RICEZIONE
014000950227     D PARAM5          DS
014100950227     D  PA5FIL               237    246    INZ('FXXX999999')
014200950227     D  PA5MBR               247    250    INZ('MXXX')
014300950227     D  BLKFIL               251    256    INZ
014400030128      *
014500920305     D TCUDS           DS
014600920305     D  F1                     1      1
014700920305     D  F3                     3      3
014800920305     D  F2                     2      2
014900920305     D  F4                     4      4
015000920305     D  F56                    5      6
015100970327     D                 DS
015200970327     D  BLPAAS                 1      4  0
015300970327     D  BLPMGS                 5      8  0
015400970327     D  BLPDSP                 1      8  0
015500030128      *
015600920324     D  TCU                  398    697
015700920305     D                                     DIM(50)                              TAB FLAG CNT
015800920324     D  KCU                  698    847P 0
015900920305     D                                     DIM(50)                              TAB CAPO CONTO
016000920305     D                                     PACKEVEN
016100030108      *
016200030108     D WLBDAT          DS                  INZ
016300030108     D  G02DAT                 1      8  0
016400030108     D  G02INV                 9     16  0
016500030108     D  G02ERR                17     17
016600030108     D  G02TGI                18     22  0
016700030128      *
016800030128     d Status         sds           333
016900030128     d  SDSpgm           *proc
017000030108      *
017100030108     D W0140           S             14  0
017200030108     D DATEU           S              8  0
017300030128     D TimeU           s              6  0
017400030401     d kApdTip         s                   Like(ApdTip) Inz('A')
017500030401     d kApdPdr         s                   Like(ApdPdr)
017600110607     d kTRD            s                   Like(ar5trd)
017700030108     D NRec5           s              7  0 inz
017800030128     d SAVjba          s                   like(KPJBA) inz
017900080424     d libr            s              3
018000080424     d Ovrdbf_blt      s             21
018001151113     d wb4ado          s                   like(§b4ado)
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
077801151113     c                   clear                   wb4ado
077900991104    1C     *IN96         IFEQ      *ON
078000060220     C     KASS          CHAIN     Fiar400R                           32
078100991104    2C     *IN32         DOWEQ     *OFF
078200991104     C*
078300071106    3c**** ar4TRC        IFEQ      'E'
078400071106     C****               ADD       1             NREC4             7 0
078500071106     C****               MOVEL     ar4NOT        DSBL4E
078600071106     C*****              MOVEL     'S'           WTROV             1
078700070926   X3C**********         ELSE
078800071106    3c****               endif
078900070926     c* 26/09/07 -ES- memorizzo il record E come è in filiale
079000070926     c
079100060220     C* RICEVO COMUNQUE RECORD DI ar4 IN TITA4  ESCLUDENDO TRC='A'
079200060220    4C     ar4TRC        IFNE      'A'
079300041105     c* Se si tratta di record M verifico se c'e' già e vedo in update
079400060220     c                   if        ar4trc='M'
079500041105     C                   MOVEL     'M'           §TRC
079600041105     c     kNSPE         chain     tita430c
079700041105     C                   ENDIF
079800041105     c
079900071106     C                   ADD       1             NREC4             7 0
080000991213     C                   CLEAR                   TITA4000
080100991213     C                   Z-ADD     BLPAAS        TA4AAS
080200991213     C                   Z-ADD     BLPLNP        TA4LNP
080300991213     C                   Z-ADD     BLPNRS        TA4NRS
080400991213     C                   Z-ADD     BLPNSP        TA4NSP
080500060220     C                   MOVEL     ar4TRC        TA4TRC
080600060220     C                   MOVEL     ar4NOT        TA4NOT
080700060220     C                   IF        ar4TRC<>'M' or not %found(tita430c)
080800010109     C  N07              WRITE     TITA4000                             94
080900010109     C   07              WRITE     TITA4P00                             94
081000041105     c                   else
081100041105     C   08              update    TITA4000
081200041105     C   09              update    TITA4P00
081300041105     c                   endif
081301151113     c* Se tipo record "M" salvo flag addebito orm telefonici da memorizzare
081302151113     c* anche su titas
081303151113     c                   if        ar4trc='M'
081304151113     c                   movel     ar4not        dsbl4m
081305151113     c                   movel     §b4ado        wb4ado
081306151113     c                   endif
081400991213    4C                   ENDIF
081500070926    3C**********         ENDIF
081600041105     c
081700060220     C     KASS          READE     Fiar400R                               32
081800991104    2C                   ENDDO
081900991104    1C                   ENDIF
082000041105     c
082100990618     C* RIFERIMENTO MITTENTE ALFABETICO: IN TITA4 TIPO RIF."A"
082200990630     C     BLPRMA        IFNE      *BLANKS
082300991104     C* ANNCHE IL RIFERIMENTO PARTNER
082400071106     c* Il riferimento partenr lo scrivo solo nel record "E"
082500071106     C**** WTROV         OREQ      'S'
082600990618     C                   CLEAR                   TITA4000
082700071106     C                   CLEAR                   dta4a
082800990618     C                   Z-ADD     BLPAAS        TA4AAS
082900990618     C                   Z-ADD     BLPLNP        TA4LNP
083000990618     C                   Z-ADD     BLPNRS        TA4NRS
083100990618     C                   Z-ADD     BLPNSP        TA4NSP
083200990618     C                   MOVEL     'A'           TA4TRC
083300991104     C                   MOVEL     BLPRMA        §TA4RM
083400071106     C******             MOVEL     §B4ERP        §TA4RP
083500991104     C                   MOVEL     DTA4A         TA4NOT
083600010109     C  N07              WRITE     TITA4000
083700010109     C   07              WRITE     TITA4P00
083800990630     C                   END
083900050531     c* Se presente in bolla importo da assic. creo tita4 record "C"
084000050531     c* per pgm di elaborazione invio e-mail ad assicurazione per
084100050531     c* spedizioni senza mandato
084200050531     c     *in52         ifeq      *off
084300050531     c     blpias        andgt     *zeros
084400050531     c                   clear                   dta4c
084500050531     c                   z-add     blpaas        ta4aas
084600050531     c                   z-add     blplnp        ta4lnp
084700050531     c                   z-add     blpnrs        ta4nrs
084800050531     C                   Z-ADD     BLPNSP        TA4NSP
084900050609     C                   MOVEL     'c'           TA4TRC
085000050531     C                   MOVEL     'S'           §TA4pias
085100050531     C                   MOVEL(P)  DTA4c         TA4NOT
085200050531     C                   WRITE     TITA4000
085300050531     c                   endif
085400900601     C*
085500000627     C     *IN52         IFEQ      *OFF
085600050318     c* se si tratta di assegnato S.F. imposto anche ksc e ctr
085700050318     c                   move      ar6ksc        fineksc           4 0
085800050318     c                   if        fineksc<>0000 and fineksc<>9999
085900050318     c                   eval      tasksc=ar6ksc
086000050318     c                   eval      tasctr=ar6ctr
086100050318     c                   endif
086200991104     C                   MOVEL     AR6DIV        TASDIV
086300991104     C                   Z-ADD     AR6POR        TASPOR
086400991104     C                   MOVE      AR6SV1        TASSV1
086500991104     C                   Z-ADD     AR6VA1        TASVA1
086600991104     C                   MOVE      AR6SV2        TASSV2
086700991104     C                   Z-ADD     AR6VA2        TASVA2
086800991104     C                   MOVE      AR6SV3        TASSV3
086900991104     C                   Z-ADD     AR6VA3        TASVA3
087000991104     C                   Z-ADD     AR6NFT        TASNFT
087100991104     C                   Z-ADD     AR6DFT        TASDFT
087200991104     C                   Z-ADD     AR6FIV        TASFIV
087300991104     C                   MOVEL     AR6CEI        TASFEI
087400991104     C                   Z-ADD     AR6IMV        TASIMV
087401151120     c                   if        tasimv>0
087404151119     c                   clear                   dar5fat
087405151119     c                   Eval      ktrd='FAT'
087406151119     c     kass2         Chain     Fiar531c
087407151119     c                   If        %Found(Fiar531c)
087408151119     c                   Movel     ar5uni        dar5fat
087410151119     c                   select
087411151119     c                   when      §AR5FPTAS='R'
087412151119     c                   eval      tasfpf='R'
087413151119     c                   when      §AR5FPTAS='D'
087414151119     c                   eval      tasfpf='D'
087415151119     c                   other
087416151119     c                   eval      tasfpf='V'
087417151119     c                   endsl
087418151120     c                   endif
087419151119     c                   endif
087500900601     C* IMPOSTO FLAG IN BASE AL RIEMPIMENTO DEL CAMPI RELATIVI
087600991104     C     AR6DFT        IFGT      *ZERO
087700941114     C                   MOVE      '1'           TASFTS
087800900601     C                   END
087900000627     C                   ENDIF
088000991104     C* CREO LE ALTRE VARIE SE ESISTE IL DETTAGLIO
088100991104     C   97              EXSR      SR7
088200061211     c
088300061218      * Leggo fiar5 x impostare il flag dei colli di valore su titas
088400091214     c                   clear                   War5cdi           7
088500061218     C                   If        *in91
088600110607     c                   eval      ktrd='GEN'
088700061218     C     KASS2         chain     FIAR500R
088800061218     c                   if        %found(fiar500r)
088900061218     c                   Eval      dAr5Gen = Ar5Uni
089000061218     c                   If        §Ar5Bva = 'V'
089100061218     c                   Eval      dtasflo = TasFlo
089200061218     c                   Eval      §FloTba = §Ar5Bva
089300061218     c                   Eval      TasFlo = dtasflo
089400061218     c                   endif
089500091214     c* Se presente beneficiario assegno --> lo memorizzo per CSB
089600091214     c                   if        §ar5cdi>*zeros
089700091214     c                   eval      war5cdi=§ar5cdi
089800091214     c                   endif
089900091214     c
090000061218     c                   endif
090100130925      * leggo fiar5 EMD per impostare il flag avviso al destinatario su titas
090200130325      * Solo se non si tratta di bolla figlia
090300130325     c     knsp          setll     fnlbl01l
090400130325     c                   if        not %equal(fnlbl01l)
090500110607     c                   eval      ktrd='EMD'
090600130322     C     KASS2         chain     FIAR500R
090700130322     c                   if        %found(fiar500r)
090800130322     c                   eval      dar5emd=ar5uni
090900130925     c***                if        §ar5eml<>*blanks
091000130925     c                   if        §ar5smp='S' or §ar5mmp='S'
091100110607     c                   Eval      dtasflo = TasFlo
091200130925     c                   select
091300130925     c                   when      §ar5mmp='S' and §ar5smp=' '
091400130925     c                   eval      §floemd='S'
091500130925     c                   when      §ar5mmp='S' and §ar5smp='S'
091600130925     c                   eval      §floemd='E'
091700130925     c                   when      §ar5smp='S'
091800130925     c                   eval      §floemd='X'
091900130925     c                   endsl
092000110607     c                   Eval      TasFlo = dtasflo
092100130322     c                   endif
092200110607     c                   endif
092300150220      **
092400150220      *(sempre all'interno del controllo sulla MAMMA,)
092500150220      * Gestione BANCALI - Movimenti BAN x clienti
092600150305     c                   if          %subst(tasGVA:1:1) ='B'
092700150220     c                   eval      ktrd='BAN'
092800150220     C     KASS2         chain     FIAR500R
092900150220     c                   if        %found(fiar500r)
093000150220     c                   clear                   tisig20ds
093100150220     c                   eval      i20aas = ar5aas
093200150220     c                   eval      i20lnp = ar5lnp
093300150220     c                   eval      i20nrs = ar5nrs
093400150220     c                   eval      i20nsp = ar5nsp
093500150220     c                   eval      i20trd = ar5trd
093600150220     c                   eval      i20DAT = tasAAS * 10000 + tasMGS
093700150220     c                   move      *all'0'       i20SOG
093800150220     c                   move      TASccm        i20SOG
093900150311     c                   move      TAStbl        i20TBL
094000150311     c                   move      TAScbo        i20CBO
094100150220      *
094101160104     c                   Eval      kpjbux = kpjbu
094200150220     c                   eval        kpjbu = tisig20ds
094300150318     c                   call      'TISIG20R'                           77
094400150220     c                   parm                    kpjba
094500150220     c                   parm                    Fiar5ds
094501160104     c                   Eval      kpjbu = kpjbux
094600150220     c                   endif
094700150220     c                   end
094800150220      *
094900150220      * Gestione BANCALI - Movimenti BNB Filiale
095000150305     c                   if          %subst(tasGVA:1:1) ='O'
095100150220     c                   eval      ktrd='BNB'
095200150220     C     KASS2         chain     FIAR500R
095300150220     c                   if        %found(fiar500r)
095400150220     c                   clear                   tisig20ds
095500150220     c                   eval      i20aas = ar5aas
095600150220     c                   eval      i20lnp = ar5lnp
095700150220     c                   eval      i20nrs = ar5nrs
095800150220     c                   eval      i20nsp = ar5nsp
095900150220     c                   eval      i20trd = ar5trd
096000150220     c                   eval      i20DAT = tasAAS * 10000 + tasMGS
096100150220     c                   move      *all'0'       i20SOG
096200150220     c                   move      TASccm        i20SOG
096300150311     c                   move      TAStbl        i20TBL
096400150311     c                   move      TAScbo        i20CBO
096500150220      *
096501160104     c                   Eval      kpjbux = kpjbu
096600150220     c                   eval        kpjbu = tisig20ds
096700150318     c                   call      'TISIG20R'                           77
096800150220     c                   parm                    kpjba
096900150220     c                   parm                    Fiar5ds
096901160104     c                   Eval      kpjbu = kpjbux
097000150220     c                   endif
097100150220     c                   end
097200150220      **
097300130325     c                   endif
097400110607
097500061218     c                   endif
097600061211     c
097700061211     c* Aggiorno legami bolla come figlia
097800061211     c                   exsr      AggLBL
097900061211
098000061211      * richiamo fidn58r x aggiornare titas se presente c.a.
098100061211     c                   Clear                   Fidn58ds
098200061211     c                   Eval      i58aas = tasaas
098300061211     c                   Eval      i58lnp = taslnp
098400061211     c                   Eval      i58nrs = tasnrs
098500061211     c                   Eval      i58nsp = tasnsp
098600061211     c                   Eval      i58sed = 'S'
098700061211     c                   Eval      kpjbux = kpjbu
098800061211     c                   Clear                   kpjbu
098900061211     c                   movel     fidn58ds      kpjbu
099000061211     c                   call      'FIDN58R'
099100061211     c                   parm                    kpjba
099200061212     C                   MOVEL     KPJBU         FIDN58DS
099300061211     c                   Eval      kpjbu = kpjbux
099400061211     c
099500061211     c                   eval      tasfda=o58fda
099600110519     c
099700110519     c* Verifico, se C/Ass, se auto abilitato
099800110519     C     *IN12         ifeq      *ON
099900110519     C     BL9CAS        ANDGT     0
100000110519     c
100100110519     c                   clear                   trul21ds
100200110519     c                   eval      i21tla = 'L'
100300110519     c                   eval      i21cbo = blpcbo
100400110519     c                   eval      i21tsp = blptsp
100500110519     c                   eval      i21lnp = blplnp
100600110519     c                   eval      i21nzm = blpnzm
100700110519     c                   eval      i21lna = blplna
100800130808     c                   eval      i21ksc = tasccm
100900110519     c                   eval      i21nzd = blpnzd
101000110519     c                   eval      i21tic = bl9tic
101100110519     c                   eval      i21imp = bl9cas
101200110519     c                   eval      i21div = bl9vca
101300110519     c                   eval      i21ute = knmus
101400110519     c                   eval      i21pgm = 'TRTR36R'
101500110519     c                   call      'TRUL21R'
101600110519     c                   parm                    trul21ds
101700110519
101800110519     c                   if        o21fca <> *blanks
101900110519     c                   Eval      dtasflo = TasFlo
102000110519     c                   Eval      §FloFCA = 'S'
102100110519     c                   Eval      TasFlo = dtasflo
102200110519     c                   endif
102300110519     c                   endif
102301151113     c
102302151113     c* aggiorno flag per addebito orm telefonico
102303151113     c                   eval      dtasflo = Tasflo
102304151113     c                   eval      §floado = wb4ado
102305151113     c                   eval      TasFlo=dtasflo
102400110519     c
102500900601     C*
102600000627     C  N52              WRITE     TITAS000
102700000628     C* N.B. HO UGUALMENTE PREVISTO L'AGGIORNAMENTO ANCHE DI TAS10 E
102800000628     C*      TASP ANCHE SE PER ORA E' INUTILE VISTO CHE SE BOLLA
102900000628     C*      GIA CONTABILIZZATA VIENE STAMPATO ERRORE E LA BOLLA NON
103000000628     C*      VIENE CARICATA (V. ROUTINE CTRNSP)
103100010109     C*      09/01/01 : DA OGGI L'AGGIORNAMENTO DI TAS10/P NON SARÀ PIÙ
103200010109     C*                 INUTILE DAL MOMENTO CHE SE BOLLA GIA' CONTABIL.
103300010109     C*                 CARICO LO STESSO L'AGGIORNAMENTO
103400000627     C   52
103500000627     CANN53
103600000627     CAN 05              UPDATE    TITAS000
103700000627     C   52
103800000627     CANN53
103900000627     CAN 06              UPDATE    TITAS010
104000000627     C   52
104100000627     CANN53
104200000627     CAN 07              UPDATE    TITASP00
104300040603      *
104400040603
104500040603      * Sposto la ricezione di FIAR5 dopo la ricezione dei contrassegni
104600040603
104700900601     C*
104800950112     C* CREA TNCSB SE ESISTE CONTRASSEGNO
104900000628     C* SE ESISTE GIA' TNCSB MA IN STATO 9 E STO RICEVENDO
105000000628     C* AGGIORNAMENTO DI BOLLA AUTOGENERATA RIAGGIORNO
105100950112     C** X ASSEGNATO O 8888 O 9999 CREA ANCHE TNCSA00F
105200111019     C**                   CLEAR                   WINMSG
105300900601     C*
105400000629     C                   SELECT
105500000629     C     *IN12         WHENEQ    *ON
105600000628     C     BL9CAS        ANDGT     0
105700091214     c
105800091214     C**   *IN52         OREQ      *ON
105900091214     C**   BL9CAS        ANDGT     0
106000091214     C**   *IN12         ANDEQ     *OFF
106100091214     C**   CSBSTA        ANDEQ     9
106200000628     C*
106300950112     C** TNCSB
106400000628     C   12              CLEAR                   TNCSB000
106500011031     C*
106600011031     C* TIPO INCASSO C/A
106700011031     C                   EXSR      IMPTIC
106800010117     C*
106900111019     C**N12TASDCM        IFGT      *ZEROS
107000111019     C**   TASCCA        ANDNE     *BLANKS
107100010117     C* Se bolla autogenerata e consegnata con consegna anomala e sto
107200010117     c* ricevendo il suo aggiornamento con il contrassegno devo mante-
107300010117     c* nere il contrassegno in stato 9 e di conseguenza non invio msg
107400010117     c* per l'ufficio contrassegni
107500111019     C**                 ELSE
107600111019     C**                 CLEAR                   CSBSTA
107700111019     C**                 MOVEL     '1'           WINMSG            1
107800111019     C**                 ENDIF
107900111019     c
108000010117     C* Se sono in ricezione di aggiornamento bolla autogenerata già
108100010117     c* consegnata alla quale è stato aggiunto il contrassegno:
108200010117     c* - invio messaggio per ufficio C/A di contrassegno aggiunto
108300010117     C*   se la consegna non è anomala
108400010117     c* - ricevo il contrassegno come annullato se la consegna è anomal
108500111019     C**   52
108600111019     C**AN 12TASDCM        IFGT      *ZEROS
108700111019     C**   TASCCA        IFEQ      *BLANKS
108800111019     C*+                 MOVEL     '1'           WINMSG
108900111019     C**                 ELSE
109000111019     C**                 Z-ADD     9             CSBSTA
109100111019     C**                 ENDIF
109200111019     C**                 ENDIF
109300010117     C*
109400910926     C                   MOVEL     §TBCA         CSBTBL
109500910926     C                   Z-ADD     TASLNP        CSBLNP
109600910926     C                   Z-ADD     TASAAS        CSBAAS
109700910926     C                   Z-ADD     TASNRS        CSBNRS
109800910926     C                   Z-ADD     TASNSP        CSBNSP
109900941114     C                   Z-ADD     TASMGS        CSBMGS
110000990615     C                   Z-ADD     TASTFP        CSBFLE
110100950112     C                   Z-ADD     BL9CAS        CSBCAS
110200950112     C                   Z-ADD     BL9CMB        CSBCMB
110300950112     C                   MOVEL     BL9VCA        CSBVCA
110400950117     C                   MOVEL     BL9GCA        CSBGCA
110500941114     C                   MOVEL     BLPLOD        CSBPAR                         PAESE ARRIVO
110600941114     C                   MOVEL     BLPPRD        CSBPRO                         PROV.DEST.
110700910926     C                   MOVEL     BLPRSD        CSBRSD                         DESTINATARIO
110800910926     C                   Z-ADD     TASRMN        CSBRMN
110900910926     C                   Z-ADD     TASLNA        CSBLNA
111000910926     C                   Z-ADD     TASKSC        CSBKSC
111100910926     C                   MOVE      CSBKSC        CSBCDI
111200931108     C                   MOVEL     'R'           CSBFTR
111300030108     C                   Z-ADD     DateU         CSBDTR
111400990618     C                   MOVEL     BLPRMA        CSBRMA
111500050411     C** PER ASSEGNATI COD. METTO XXX9999 DOVE XXX LNA ARRIVO
111600050411     C     §TIBO         IFEQ      'A'
111700050411     C                   Z-ADD     99999         CSBKSC
111800050411     C                   MOVEL     TASLNA        CSBKSC
111900060612     c* Se bolla singola in assegnato:  58 off
112000060612     c*                          imposto CDI=xxx9999 e poi magari il blpccm
112100060612     c* Se bolla doppia franco+assegnato: 58 on
112200060612     c*                          imposto CDI=tasksc (mittente)
112300060612     C  n58              MOVEL     CSBKSC        CSBCDI
112400050411     C                   END
112500920304     C* COD CLIENTE MITTENTE
112600920304     C     BLPCCM        IFGT      0
112700920304     C                   MOVEL     BLPCCM        CSBCDI
112800920304     C                   END
112900091214     c
113000091214     c* Beneficiario = ordinante
113100091214     c                   if        war5cdi>*zeros
113200091214     c                   movel     war5cdi       CSBCDI
113300091214     c                   endif
113400920304     C*
113500900601     C** RAGGRUPPAMENTO C/ASSEGNI
113600900601     C     TASLNA        CHAIN     AZORG                              51
113700910926     C  N51              Z-ADD     ORGFC5        CSBRGF                         FIL RAGGR.C/A
113800910926     C   51              Z-ADD     TASLNA        CSBRGF
113900941130     C*
114000000628     C                   EXSR      TNCSA
114100920306     C*
114200000628     C* SE STO RICEVENDO AGGIORNAMENTO DI BOLLA AUTOGENERATA CON C/A
114300000628     C* E C/A GIA' PRESENTE SU TNCSB VEDO SE AGGIORNARE
114400091214     C***  *IN52         WHENEQ    *ON
114500091214     C***  BL9CAS        ANDGT     *ZEROS
114600091214     C***  *IN12         ANDEQ     *OFF
114700091214     C***  CSBSTA        ANDNE     9
114800091214     C***                Z-ADD     BL9CAS        WARCAS
114900091214     C***                MOVE      BL9VCA        WARVCA
115000000628     C* SE DIVISE DIVERSE VERIFICO SE ENTRAMBE ACCETTABILI IN ITALIA
115100000628     C* ED IN QUESTO CASO CONVERTO IMPORTO IN RICEZIONE
115200091214    3C***  BL9VCA        IFNE      CSBVCA
115300091214     C**+  BL9VCA        LOOKUP    DVA                                    31
115400091214     C**+31              Z-ADD     1             II
115500091214     C***31CSBVCA        LOOKUP    DVA(II)                                31
115600091214    4C***  *IN31         IFEQ      *ON
115700091214     C***                CLEAR                   DSYEUR
115800091214     C***                MOVEL     BL9VCA        YECDVI
115900091214     C***                MOVEL     CSBVCA        YECDVO
116000091214     C***                Z-ADD     BL9CAS        YECIMI
116100091214     C***                MOVE      DEC(II)       YECDCO
116200091214     C***
116300091214     C***                CALL      'YEURCO'
116400091214     C***                PARM                    DSYEUR
116500091214     C***
116600091214    5C***  YECESI        IFEQ      ' '
116700091214     C***                Z-ADD     YECIMO        WARCAS
116800091214     C***                MOVEL     CSBVCA        WARVCA
116900091214    5C***                ENDIF
117000091214    4C***                ENDIF
117100091214    3C***                ENDIF
117200091214     C**LEGGO IMPORTO MINIMO ABBUONO SOLO SE STESSA DIVISA
117300091214     C***  WARVCA        IFEQ      CSBVCA
117400091214     C***                CLEAR                   TIBS02
117500091214     C***                MOVEL     KPJBU         KPJBUX
117600091214     C***                CLEAR                   KPJBU
117700091214     C***                MOVE      'C'           T02MOD                         CONTROLLO
117800091214     C***                MOVE      KNSIF         T02SIF                         S.I.
117900091214     C***                MOVE      'GEI'         T02COD                         TABELLA PROFILI
118000091214     C***                MOVEL(P)  WARVCA        T02KE1                         PROFILO IN K1
118100091214     C***                CALL      'TIBS02R'
118200091214     C***                PARM                    KPJBA
118300091214     C***                PARM                    TIBS02
118400091214     C***                MOVEL     KPJBUX        KPJBU
118500091214    3C***  T02ERR        IFEQ      *BLANKS
118600091214     C***                MOVEL     T02UNI        DGEI
118700091214     C***                ELSE
118800091214     C***                CLEAR                   DGEI
118900091214    3C***                ENDIF
119000091214    3C***  §GEMIA        IFGT      0
119100091214     C***  WARCAS        ANDNE     CSBCAS
119200091214     C***  WARCAS        SUB       CSBCAS        WDIFFE
119300091214     C***                MLLZO     1             WDIFFE
119400091214     C**SE LA DIFFERENZA RIENTRA NELLA TOLLERANZA IMPOSTO WARCAS=CSBCAS
119500091214     C***  WDIFFE        IFLE      §GEMIA
119600091214     C***                Z-ADD     CSBCAS        WARCAS
119700091214     C***                ENDIF
119800091214    3C***                ENDIF
119900091214     C***                ENDIF
120000091214     C**IMPORTO DIVERSO
120100091214    3C***  WARCAS        IFNE      CSBCAS
120200091214     C***  WARVCA        ORNE      CSBVCA
120300091214     C**OPPURE MITTENTE DIVERSO
120400091214     C***  TASCCM        ORNE      CSBCDI
120500091214     C**Invio messaggio in posta elettronica
120600091214     C**Stesso tipo di messaggio se no pagato:non invio niente se
120700091214     c**importo non variato
120800091214    4C***  CSBBNA        IFEQ      0
120900091214    5C***  WARCAS        IFNE      CSBCAS
121000091214     C***  WARVCA        ORNE      CSBVCA
121100091214     C***                EXSR      MSGPOS
121200091214    5C***                ENDIF
121300091214   x4C***                ELSE
121400091214     C**MSG se pagato e importo diverso
121500091214    5C***  WARCAS        IFNE      CSBCAS
121600091214     C***  WARVCA        ORNE      CSBVCA
121700091214     C***                EXSR      MSGPO2
121800091214   x5C***                ELSE
121900091214     c**se pagato e importi uguali sengalo solo variazione del mittente
122000091214     C***                EXSR      MSGPO3
122100091214    5C***                ENDIF
122200091214    4C***                ENDIF
122300091214     C**Aggiorno solo se non pagato o incassato
122400091214     C**Il cliente lo vario anche se incassato
122500091214    4C***  CSBBNA        IFEQ      0
122600091214     C***                Z-ADD     TASKSC        CSBKSC
122700091214     C***                MOVE      CSBKSC        CSBCDI
122800091214     C***Per assegnati cod. metto XXX9999 dove XXX lna arrivo
122900091214    5C***  §TIBO         IFEQ      'A'
123000091214     C***                Z-ADD     99999         CSBKSC
123100091214     C***                MOVEL     TASLNA        CSBKSC
123200091214     C***                MOVEL     CSBKSC        CSBCDI
123300091214    5C***                END
123400091214     C**Cod cliente mittente
123500091214    5C***  BLPCCM        IFGT      0
123600091214     C***                MOVEL     BLPCCM        CSBCDI
123700091214    5C***                END
123800091214     C***                EXSR      TNCSA
123900091214    4C***                ENDIF
124000091214     c***
124100091214    4C***  CSBDDC        IFEQ      *ZEROS
124200091214     C***  CSBBNA        ANDEQ     *ZEROS
124300091214    5C***  WARCAS        IFNE      CSBCAS
124400091214     C***  WARVCA        ORNE      CSBVCA
124500091214     C***                Z-ADD     BL9CAS        CSBCAS
124600091214     C**TIPO INCASSO C/A
124700091214     C***                EXSR      IMPTIC
124800091214     C***                MOVEL     BL9VCA        CSBVCA
124900091214     C***                MOVEL     BL9CMB        CSBCMB
125000091214     C***                MOVEL     BL9GCA        CSBGCA
125100091214    5C***                ENDIF
125200091214    4C***                ENDIF
125300091214    3C***                ENDIF
125400000629     C                   ENDSL
125500061218     c*  11/12/06 - ES
125600061218     c*  qui c'era la lettura e scrittura di FIAR5 ma la sposto
125700061211     c*  per evitare un chain e update di TITAS
125800061218     c*  18/12/06 - ES
125900061218     c*  Rimetto la write di fiar5 qui  perchè a causa del C/E (titasque)
126000061218     c*  devo scrivere prima titas00f di fiar5 altrimenti il C/E da errore
126100061218     c                   exsr      AggFIAR5
126200080828     c
126300080828     c* Verifico se devo aggiornare a creare FIAR5 per invetario restituzDDT
126400080902     c* Escludo le bolle di recupero
126500100317     c
126600100317     c* ES 17/03/10 --> non lo faccio più qui spostato nel gpm fnlsa3r per creare
126700100317     c*   il recorrd GEN in filiale ed evitare proboemi di sovrapposizione dati
126800100317+--- C***  WRBL          IFNE      'R'                                          (a)
126900100317     c**+                exsr      contrDCLI
127000100317     c***                endif
127100900601     C*
127200931231     C* C R E A   2 °   B O L L A :
127300000627     C* DO PER SCONTATO CHE PER LE POSTE NON CI SIA MAI E QUINDI
127400000627     C* NON DEVO GESTIRE L'AGGIORNAMENTO DI BOLLA AUTOGENERATA
127500900601     C*
127600900601     C   59              DO
127700900601     C*
127800990621     C* AZZERO CAMPI DI TITAS
127900000627     C                   CLEAR                   TITAS000
128000941114     C*
128100941114     C* RIEMPO I CAMPI CHE SONO UGUALI PER LA 1 E LA 2 BOLLA
128200990817     C                   MOVEL     §3ATB2        WFLGTB            1
128300941114     C                   EXSR      RIETAS
128400900601     C                   MOVE      §3ATB2        TASTBL
128500900601     C** SEMPRE ASSEGNATO:
128600900601     C** COD.CLI.= XXX9999 DOVE XXX =LINEA ARRIVO
128700900601     C                   Z-ADD     9999          TASKSC
128800900601     C                   MOVEL     BLPLNA        TASKSC
128900061211     c* per C.A.
129000061211     c                   eval      tasfda=o58fda
129100900601     C*
129200900601     C* ESISTE ESTENSIONE BOLLA
129300000627     C   93              DO
129400991104     C                   MOVEL     '2'           WTRC
129500991104     C     KASS1         CHAIN     FIAR600R                           73
129600991104     C     *IN73         IFEQ      *OFF
129700991104     C                   ADD       1             NRECC             7 0
129800040112     C*!N52AR6SV1        IFNE      *BLANK
129900040112     c                   If        Not *In52
130000050318     c* se si tratta di assegnato S.F. imposto anche ksc e ctr
130100050318     c                   move      ar6ksc        fineksc           4 0
130200050318     c                   if        fineksc<>0000 and fineksc<>9999
130300050318     c                   eval      tasksc=ar6ksc
130400050318     c                   eval      tasctr=ar6ctr
130500050318     c                   endif
130600991104     C                   MOVEL     AR6DIV        TASDIV
130700991104     C                   MOVE      AR6SV1        TASSV1
130800991104     C                   Z-ADD     AR6VA1        TASVA1
130900040112     c                   Move      Ar6Sv2        TasSv2
131000040112     c                   Z-add     Ar6Va2        TasVa2
131100040112     c                   Move      Ar6Sv3        TasSv3
131200040112     c                   Z-add     Ar6Va3        TasVa3
131300991104     C                   MOVEL     AR6CEI        TASFEI
131400040112     c                   Z-add     Ar6Imv        TasImv
131500950114     C                   END
131600040112      * Creo le altre varie se esiste il dettaglio su FIAR7
131700040112     c   97              ExSr      SR7
131800991104     C                   END
131900991108     C                   END
132000900601     C*
132100950112     C   58BL9CAS        IFGT      0
132200941114     C                   MOVEL     'S'           TASFCA
132300941114     C                   ENDIF
132400061211     c
132500061211     c* Aggiorno legami bolla come figlia
132600061211     c                   exsr      AggLBL
132700900601     C*
132800000627     C                   WRITE     TITAS000
132900900601     C                   END
133000061211     c
133100950522     C** SCRITTURA TNCSB
133200000628     C     *IN12         IFEQ      *ON
133300000628     C     BL9CAS        ANDGT     0
133400091214     c
133500091214     C**   *IN12         OREQ      *OFF
133600091214     C**   *IN52         ANDEQ     *ON
133700091214     C**   BL9CAS        ANDGT     *ZEROS
133800000628     C   12              WRITE     TNCSB000
133900000628     C  N12              UPDATE    TNCSB000
134000010117     C* INVIO MSG DI C/A RIATTIVATO O AGGIUNTO
134100111019     C**   WINMSG        IFEQ      '1'
134200111019     C**                 EXSR      MSGPO1
134300111019     C**                 ENDIF
134400950522     C                   END
134500941126     C                   END
134600061211     c
134700061211     c* Per sicurezza leggo anche come mamma anche se non dovrebbe
134800061211     c*  succedere
134900061211     c     knsp          setll     fnlbl02l
135000061211     c     knsp          reade     fnlbl02l
135100061211     c                   dow       not %eof(fnlbl02l)
135200061211     c                   call      'FNLG25R'
135300061211     c                   parm                    recordlbl
135400061211     c                   parm                    i25tla            1
135500061211     c
135600061211     c     knsp          reade     fnlbl02l
135700061211     c                   enddo
135800041119     c
135900041119     C                   ENDSR
136000000628     C*
136100011022     C**---------------------------------------------------------
136200011022     C** IMPOSTO IL TIPO INCASSO C/A E CONTROLLO LA VALUTA
136300011022     C**---------------------------------------------------------
136400000628     C     IMPTIC        BEGSR
136500000628     C                   MOVEL     'B'           CSBFUS                          DEFAULT
136600000628     C     BL9TIC        IFNE      *BLANK
136700000628     C                   MOVE      '1A'          COD                             COD.BOLLA
136800000628     C                   MOVE      *BLANK        KEY
136900000628     C                   MOVEL     BL9TIC        KEY
137000000628     C     KTAB          CHAIN     TABEL                              54
137100011023     C  N54              MOVEL     TBLUNI        DS1A
137200011023     C  N54              MOVEL     §1AFMB        CSBFUS
137300011022      *
137400000628     C                   END
137500000628     C                   ENDSR
137600000628     C**----------------------------------------
137700000628     C** AGGIORNAMENTO/SCRITTURA/DELET TNCSA00F
137800000628     C**----------------------------------------
137900000628     C     TNCSA         BEGSR
138000000628     C  N12KASS          CHAIN     TNCSA01L                           54
138100000628     C*
138200000628     C** TNCSA
138300000628     C                   Z-ADD     TASLNP        CSALNP
138400000628     C                   Z-ADD     TASAAS        CSAAAS
138500000628     C                   Z-ADD     TASNRS        CSANRS
138600000628     C                   Z-ADD     TASNSP        CSANSP
138700000628     C* SE IL CODICE INTESTAZIONE ASSEGNO E' GENERICO CREO CSA
138800000628     C                   MOVE      CSBCDI        W0040             4 0
138900000628     C*
139000000628     C** DATI ANAGRAFICI PER CODICI GENERICI E ASSEGNATI
139100000628     C** FRANCHI VARI 88888 SENZA COD CLI MITT CHE NEGLI 8888
139200000628     C** E' PER IL COD.INTESTAZIONE ASSEGNO
139300000628     C     W0040         IFEQ      8888
139400000628     C     W0040         OREQ      9999
139500000628     C                   MOVEL     BLPRSM        CSARSD                         MITTENTE
139600000628     C                   MOVEL     BLPINM        CSAVID                            '
139700000628     C                   MOVEL     BLPCAM        CSACAD                            '
139800000628     C                   MOVEL     BLPLOM        CSACID                            '
139900000628     C                   MOVEL     BLPPRM        CSAPRM                            '
140000000628     C   12              WRITE     TNCSA000
140100000628     C  N12
140200000628     CAN 54              WRITE     TNCSA000
140300000628     C  N12
140400000628     CANN54              UPDATE    TNCSA000
140500000628     C                   ELSE
140600000629     C  N12
140700000629     CANN54              DELETE    TNCSA000
140800000628     C                   END
140900000628     C                   ENDSR
141000991108     C**----------------------------------------
141100991108     C** PULIZIA CAMPI FIAR600R
141200991108     C**----------------------------------------
141300991108     C     PULAR6        BEGSR
141400991108     C                   CLEAR                   AR6ATB
141500991108     C                   CLEAR                   AR6AAS
141600991108     C                   CLEAR                   AR6LNP
141700991108     C                   CLEAR                   AR6NRS
141800991108     C                   CLEAR                   AR6NSP
141900991108     C                   CLEAR                   AR6TRC
142000991108     C                   CLEAR                   AR6KSC
142100991108     C                   CLEAR                   AR6CTR
142200991108     C                   CLEAR                   AR6DIV
142300991108     C                   CLEAR                   AR6POR
142400991108     C                   CLEAR                   AR6SV1
142500991108     C                   CLEAR                   AR6VA1
142600991108     C                   CLEAR                   AR6SV2
142700991108     C                   CLEAR                   AR6VA2
142800991108     C                   CLEAR                   AR6SV3
142900991108     C                   CLEAR                   AR6VA3
143000991108     C                   CLEAR                   AR6IMV
143100991108     C                   CLEAR                   AR6FIM
143200991108     C                   CLEAR                   AR6ALI
143300991108     C                   CLEAR                   AR6CEI
143400991108     C                   CLEAR                   AR6IFT
143500991108     C                   CLEAR                   AR6FIV
143600991108     C                   CLEAR                   AR6NFT
143700991108     C                   CLEAR                   AR6DFT
143800991108     C                   ENDSR
143900000000     C**----------------------------------------
144000900207     C** STAMPA ERRORI
144100000000     C**----------------------------------------
144200900530     C     ERROR         BEGSR
144300111031    1c                   if        *in16 = *off
144400111031     c
144500111031    2C     *IN99         IFEQ      *OFF
144600111031     c
144700111031    3c     err           ifne      er(3)
144800040326     c     err           andne     er(11)
144900040326     C                   movel(p)  $Cmd(5)       Comman
145000040326     C                   call      'QCMDEXC'                            21
145100040326     C                   parm                    Comman
145200040326     C                   parm                    Lung
145300980109     C                   OPEN      PRTF198
145400980109     C                   SETON                                        99OF
145500040326     c                   exsr      SendMSGe
145600111031    3c                   endif
145700111031    2C                   ENDIF
145800111031     c
145900111031    2C     *IN89         IFEQ      *OFF
146000040325     C                   movel(p)  $Cmd(4)       Comman
146100040325     C                   call      'QCMDEXC'                            21
146200040325     C                   parm                    Comman
146300040325     C                   parm                    Lung
146400040326     C                   OPEN      PRTF198X
146500040325     C                   SETON                                        89OE
146600111031    2C                   ENDIF
146700950118     C*
146800900711     C                   EXSR      STAMPA
146900941129     C*
147000950127     C* 98 NON SCRIVO IL FILE DEGLI ERRORI SE VIENE LASCIATO NEI FILES
147100950127     C*    DI RICEZIONE
147200950127     C*
147300950127     C     *IN98         IFEQ      *OFF
147400950222     C     *IN16         ANDEQ     *OFF
147500991108     C                   WRITE     FIBLPEA
147600000906     C     *IN95         IFEQ      *ON
147700000906     C     KASS          SETLL     TITAH00R
147800000906     C     KASS          READE     TITAH00R                               32
147900000906    3C     *IN32         DOWEQ     *OFF
148000000906     C                   WRITE     TITAHE
148100000906     C     KASS          READE     TITAH00R                               32
148200000906    3C                   ENDDO
148300000906     C                   ENDIF
148400991104     C     *IN93         IFEQ      *ON
148500991104     C     KASS          SETLL     FIAR600R
148600991104     C     KASS          READE     FIAR600R                               32
148700991104    3C     *IN32         DOWEQ     *OFF
148800991104     C                   WRITE     FIAR6EC
148900991104     C     KASS          READE     FIAR600R                               32
149000991104    3C                   ENDDO
149100991104     C                   ENDIF
149200970227     C     *IN96         IFEQ      *ON
149300060220     C     KASS          SETLL     Fiar400R
149400060220     C     KASS          READE     Fiar400R                               32
149500970227    3C     *IN32         DOWEQ     *OFF
149600060220     C                   WRITE     Fiar4E
149700060220     C     KASS          READE     Fiar400R                               32
149800970227    3C                   ENDDO
149900970227     C                   ENDIF
150000990621     C     *IN97         IFEQ      *ON
150100991104     C     KASS          SETLL     FIAR700R
150200991104     C     KASS          READE     FIAR700R                               32
150300990621    3C     *IN32         DOWEQ     *OFF
150400991104     C                   WRITE     FIAR7E
150500991104     C     KASS          READE     FIAR700R                               32
150600990621    3C                   ENDDO
150700990621     C                   ENDIF
150800030108    2C                   If        *IN91
150900030108     C     KASS          setll     FIAR500R
151000030108     C     KASS          reade     FIAR500R                               32
151100030108    3C                   dow       Not *IN32
151200030108     C                   WRITE     FIAR5E
151300030108     C     KASS          reade     FIAR500R                               32
151400030108   -3C                   enddo
151500030108   -2C                   Endif
151600941129     C                   ENDIF
151700111031    1c                   endif
151800941129     C*
151900111031     c* Manca PdC
152000950117     C   16              DO
152100950117     C                   MOVE      *BLANK        ARBATB
152200950117     C                   MOVE      BLPAAS        ARBAAS
152300950117     C                   MOVE      BLPLNP        ARBLNP
152400950117     C                   MOVE      BLPNRS        ARBNRS
152500950117     C                   MOVE      BLPNSP        ARBNSP
152600950117     C                   MOVE      BLPCBO        ARBCBO
152700950117     C                   MOVEL     BLPDIM        ARBDTV
152800950117     C                   MOVE      *ZERO         ARBORV
152900950117     C                   MOVEL     'MI'          ARBCVB
153000950118     C                   MOVEL     'EDPNEW'      ARBPRU
153100950117     C                   MOVEL     BLPKSC        ARBCCM
153200950117     C                   MOVEL     BLPRSM        ARBRSM
153300950117     C                   MOVEL     BLPINM        ARBINM
153400950117     C                   MOVEL     BLPCAM        ARBCAM
153500950117     C                   MOVEL     BLPLOM        ARBLOM
153600950117     C                   MOVEL     BLPPRM        ARBPRM
153700950117     C                   MOVEL     BLPNZM        ARBNZM
153800950117     C                   MOVEL     BLPFAP        ARBFAP
153900100303     C                   MOVEL     ARBccm        ARBFEV
154000950117     C                   MOVE      *ZERO         ARBDAT
154100960918     C                   MOVEL     ERR           ARBDER
154200100303     c                   clear                   arblna
154300950117     C                   WRITE     FNARBM00
154400950119     C                   ENDDO
154500000000     C                   ENDSR
154600911107     C/SPACE 005
154700000000     C**----------------------------------------
154800000000     C**  STAMPA ERRORI
154900000000     C**----------------------------------------
155000900530     C     STAMPA        BEGSR
155100930222     C* SALTO PAGINA
155200040326     c     err           ifeq      er(3)
155300040326     c     err           oreq      er(11)
155400040325     C   OE              EXCEPT    HDGF
155500040325     C   OE              SETOFF                                       OE
155600040325     c                   else
155700930222     C   OF              EXCEPT    HDG
155800930222     C   OF              SETOFF                                       OF
155900040325     c                   endif
156000900601     C**
156100941128     C                   MOVEL     BLPRSD        STARSD           20
156200950114     C                   MOVE      BLPAAS        WAAS2             2 0
156300040326     c     err           ifeq      er(3)
156400040326     c     err           oreq      er(11)
156500040325     C                   EXCEPT    ERROF
156600040325     c                   else
156700000000     C                   EXCEPT    ERRO
156800040325     c                   endif
156900000000     C                   ENDSR
157000900526     C**---------------------------------------------------------
157100900526     C** ESEGUE QCAEXEC DI APERTURA SUI MEMBRI                  -
157200900526     C**---------------------------------------------------------
157300900526     C     CAEXA         BEGSR
157400971212     C                   MOVE      *BLANK        VAR11            11
157500971212     C                   MOVE      *BLANK        VAR12            12
157600900608     C                   MOVE      ')'           VAR11
157700900526     C                   MOVEL     MBRFIL        VAR11
157800900608     C                   MOVE      '))'          VAR12
157900900526     C                   MOVEL     MBRFIL        VAR12
158000111117     C                   Z-ADD     56            LUNG             15 5
158100900526     C*
158200991103     C** FIBLPA0R
158300941129     C                   SETOFF                                       2021
158400911107     C                   MOVE      VAR11         CMA1
158500900526     C                   MOVEL     *BLANKS       COMMAN
158600911107     C                   MOVEA     CMA1(1)       COMMAN           80
158700900526     C                   CALL      'QCMDEXC'                            20
158800900526     C                   PARM                    COMMAN
158900900526     C                   PARM                    LUNG
159000900526     C   20              GOTO      FINE
159100900526     C** ALLOCO MEMBRO
159200900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
159300900526     C                   SETOFF                                       20
159400911107     C                   MOVE      VAR11         CMA3
159500900526     C                   MOVEL     *BLANKS       COMMAN
159600911107     C                   MOVEA     CMA3(1)       COMMAN
159700900526     C                   CALL      'QCMDEXC'                            20
159800900526     C                   PARM                    COMMAN
159900900526     C                   PARM                    LUNG
160000900526     C   20              GOTO      FINE
160100900526     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
160200900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
160300991103     C                   OPEN      FIBLPA0R
160400000906     C*
160500000906     C** TITAH00R
160600000906     C                   SETOFF                                       2021
160700000906     C                   MOVE      VAR11         CMH1
160800000906     C                   MOVEL     *BLANKS       COMMAN
160900000906     C                   MOVEA     CMH1(1)       COMMAN           80
161000000906     C                   CALL      'QCMDEXC'                            21
161100000906     C                   PARM                    COMMAN
161200000906     C                   PARM                    LUNG
161300000906     C   21              GOTO      SOTTOH
161400000906     C** ALLOCO MEMBRO
161500000906     C** SE NON RIESCE CHIUDO IL PROGRAMMA
161600000906     C                   SETOFF                                       20
161700000906     C                   MOVE      VAR11         CMH3
161800000906     C                   MOVEL     *BLANKS       COMMAN
161900000906     C                   MOVEA     CMH3(1)       COMMAN
162000000906     C                   CALL      'QCMDEXC'                            20
162100000906     C                   PARM                    COMMAN
162200000906     C                   PARM                    LUNG
162300000906     C   20              GOTO      FINE
162400000906     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
162500000906     C** SE NON RIESCE CHIUDO IL PROGRAMMA
162600000906     C                   OPEN      TITAH00R
162700000906     C                   SETON                                        95
162800000906     C     SOTTOH        TAG
162900030108     C*
163000030108     C** FIAR500R
163100030108     C                   setoff                                       2021
163200030108     C                   move      VAR11         $Cmd(1)
163300030108     C                   movel(p)  $Cmd(1)       Comman
163400030108     C                   call      'QCMDEXC'                            21
163500030108     C                   parm                    Comman
163600030108     C                   parm                    Lung
163700030108     C                   IF        Not *in21
163800030108     C** Alloco il membro:
163900030108     C** se NON riesce chiudo il programma
164000030108     C                   move      VAR11         $Cmd(3)
164100030108     C                   movel(p)  $Cmd(3)       Comman
164200030108     C                   call      'QCMDEXC'                            20
164300030108     C                   parm                    Comman
164400030108     C                   parm                    Lung
164500030108     C   20              goto      FINE
164600030108     C** Eseguo OVRDBF del membro desiderato:
164700030108     C** se NON riescr chiudo il programma
164800030108     C                   open      FIAR500R
164900030108     C                   eval      *in91 = *on
165000030108     C                   ENDIF
165100911108     C*
165200991103     C** FIAR600R
165300941129     C                   SETOFF                                       2021
165400911108     C                   MOVE      VAR11         CMC1
165500911108     C                   MOVEL     *BLANKS       COMMAN
165600911108     C                   MOVEA     CMC1(1)       COMMAN           80
165700941129     C                   CALL      'QCMDEXC'                            21
165800911108     C                   PARM                    COMMAN
165900911108     C                   PARM                    LUNG
166000941129     C   21              GOTO      SOTTOC
166100911108     C** ALLOCO MEMBRO
166200911108     C** SE NON RIESCE CHIUDO IL PROGRAMMA
166300911108     C                   SETOFF                                       20
166400911108     C                   MOVE      VAR11         CMC3
166500911108     C                   MOVEL     *BLANKS       COMMAN
166600911108     C                   MOVEA     CMC3(1)       COMMAN
166700911108     C                   CALL      'QCMDEXC'                            20
166800911108     C                   PARM                    COMMAN
166900911108     C                   PARM                    LUNG
167000911108     C   20              GOTO      FINE
167100911108     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
167200911108     C** SE NON RIESCE CHIUDO IL PROGRAMMA
167300991103     C                   OPEN      FIAR600R
167400911108     C                   SETON                                        93
167500911108     C     SOTTOC        TAG
167600900526     C*
167700991103     C** FIAR700R
167800941129     C                   SETOFF                                       2021
167900900526     C                   MOVE      VAR11         CMD1
168000900526     C                   MOVEL     *BLANKS       COMMAN
168100900526     C                   MOVEA     CMD1(1)       COMMAN           80
168200941129     C                   CALL      'QCMDEXC'                            21
168300900526     C                   PARM                    COMMAN
168400900526     C                   PARM                    LUNG
168500941129     C   21              GOTO      SOTTO2
168600941114     C*
168700900526     C** ALLOCO MEMBRO
168800900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
168900900526     C                   SETOFF                                       20
169000900526     C                   MOVE      VAR11         CMD3
169100900526     C                   MOVEL     *BLANKS       COMMAN
169200900526     C                   MOVEA     CMD3(1)       COMMAN
169300900526     C                   CALL      'QCMDEXC'                            20
169400900526     C                   PARM                    COMMAN
169500900526     C                   PARM                    LUNG
169600900526     C   20              GOTO      FINE
169700900526     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
169800900526     C** SE NON RIESCE CHIUDO IL PROGRAMMA
169900991103     C                   OPEN      FIAR700R
170000900530     C                   SETON                                        97
170100941129     C     SOTTO2        TAG
170200970212     C*
170300060220     C** Fiar400R
170400970212     C                   SETOFF                                       2021
170500970212     C                   MOVE      VAR11         CME1
170600970212     C                   MOVEL     *BLANKS       COMMAN
170700970212     C                   MOVEA     CME1(1)       COMMAN           80
170800970212     C                   CALL      'QCMDEXC'                            21
170900970212     C                   PARM                    COMMAN
171000970212     C                   PARM                    LUNG
171100970212     C   21              GOTO      SOTTO3
171200970212     C*
171300970212     C** ALLOCO MEMBRO
171400970212     C** SE NON RIESCE CHIUDO IL PROGRAMMA
171500970212     C                   SETOFF                                       20
171600970212     C                   MOVE      VAR11         CME3
171700970212     C                   MOVEL     *BLANKS       COMMAN
171800970212     C                   MOVEA     CME3(1)       COMMAN
171900970212     C                   CALL      'QCMDEXC'                            20
172000970212     C                   PARM                    COMMAN
172100970212     C                   PARM                    LUNG
172200970212     C   20              GOTO      FINE
172300970212     C** ESEGUE OVRDBF DEL MEMBRO DESIDERATO
172400970212     C** SE NON RIESCE CHIUDO IL PROGRAMMA
172500060220     C                   OPEN      Fiar400R
172600970212     C                   SETON                                        96
172700970212     C     SOTTO3        TAG
172800080424     c
172900080424     c* Apertura file di filiale FNBLT
173000080424     c                   If        %subst(knsif:7:1) = 'P'
173100080424     c                   Eval      ovrdbf_blt = 'FILTRAPRD/FNBLT01L'
173200080424     c                   Else
173300080424     c                   Eval      ovrdbf_blt = 'FILTRA201/FNBLT01L'
173400080424     c                   EndIf
173500080424     c                   open      fnblt01l
173600970212     C*
173700941129     C     FINE          ENDSR
173800941114     C**---------------------------------------------------------
173900950112     C* LEGGO E AGGIORNO LE VARIE IN DETTAGLIO
174000941114     C**---------------------------------------------------------
174100950112     C     SR7           BEGSR
174200040112     C**!!!              MOVEL     '1'           WTRC
174300991104     C     KASS1         SETLL     FIAR700R
174400950112     C                   DO        *HIVAL
174500991104     C     KASS1         READE     FIAR700R                               60
174600950112     C  N60              DO
174700950112     C                   ADD       1             NREC2             7 0
174800000627     C     *IN52         IFEQ      *OFF
174900990618     C                   CLEAR                   TITA7000
175000950112     C                   Z-ADD     TASAAS        TA7AAS
175100950112     C                   Z-ADD     TASLNP        TA7LNP
175200950112     C                   Z-ADD     TASNRS        TA7NRS
175300950112     C                   Z-ADD     TASNSP        TA7NSP
175400950112     C                   MOVEL     TASTBL        TA7TBL
175500991104     C                   MOVEL     AR7SVN        TA7SVN
175600991104     C                   Z-ADD     AR7VAN        TA7VAN
175700990621     C                   WRITE     TITA7000
175800000627     C                   ENDIF
175900950112     C                   END
176000950112     C  N60              END
176100941114     C                   ENDSR
176200990618     C*
176300941114     C**---------------------------------------------------------
176400941114     C* PARTI COMUNI DI RIEMPIMENTO DELLE 2 BOLLE
176500941114     C**---------------------------------------------------------
176600941114     C     RIETAS        BEGSR
176700941114     C                   Z-ADD     BLPLNP        TASLNP
176800941114     C                   Z-ADD     BLPMGS        TASMGS
176900990618     C                   Z-ADD     BLPAAS        TASAAS
177000941114     C                   Z-ADD     BLPNRS        TASNRS
177100941114     C                   Z-ADD     BLPNSP        TASNSP
177200000628     C* Se esiste TNCSB non devo riaggiornare il codice bolla se la
177300000628     C* bolla che sto ricevendo non ha il C/A
177400000628     C* Aggiorno il codice bolla solo se:
177500000628     c* - sto ricevendo una nuova bolla
177600000628     c* - sto ricevendo aggiornamento di bolla autogenerata e non esist
177700010117     c*   record di tncsb
177800000628     c* - sto ricevendo aggiornamento di bolla autogenerata ed esiste
177900000628     c*   record di tncsb ed anche la bolla in ricezione ha il c/a
178000000628     C     *IN52         IFEQ      *OFF
178100000628     C     WESCSB        ORNE      '1'
178200000628     C     §3AFCA        ORGT      0
178300990618     C                   MOVEL     BLPCBO        TASCBO
178400000627     C                   ENDIF
178500941114     C                   MOVE      BLPTSP        TASTSP
178600941114     C     TASTSP        IFEQ      ' '
178700941114     C                   MOVEL     'C'           TASTSP
178800941114     C                   ENDIF
178900941114     C                   MOVE      BLPFIN        TASFIN
179000941114     C     TASFIN        IFEQ      ' '
179100941114     C                   MOVEL     'C'           TASFIN
179200941114     C                   ENDIF
179300990621     C* P.IVA E CCM LI IMPOSTO IN BASE A TIPO BOLLA (FRANCO O ASSEGN.)
179400990621     C* (SE BOLLA FRANCO    CODIFICATO NON HO LA PARTITA IVA
179500990621     C*  SE   "     "   NON CODIFICATO LA PARTITA IVA E' DEL MITTENTE
179600990621     C*     E LA DEVO QUINDI MEMORIZZARE IN TITAA00F (QUINDI NON QUI))
179700990621     C     FLGTB1        IFEQ      'A'
179800990621     C                   Z-ADD     BLPCCM        TASCCM
179900990621     C     TASCCM        IFEQ      *ZEROS
180000990621     C                   MOVEL     BLPLNP        TASCCM
180100990621     C                   MOVE      8888          TASCCM
180200990621     C                   END
180300990621     C                   MOVEL     BLPCPI        TASCPD
180400990621     C                   ELSE
180500990621     C                   Z-ADD     BLPKSC        TASCCM
180600990621     C                   END
180700941114     C                   Z-ADD     BLPLNA        TASLNA
180800941114     C                   Z-ADD     BLPNCL        TASNCL
180900950114     C                   Z-ADD     BLPPKB        TASPKB
181000990628     C                   MOVEL     'R'           TASFPB
181100991104     C                   Z-ADD     BLPPKF        TASPKF
181200990628     C                   MOVEL     'R'           TASFPF
181300950114     C                   Z-ADD     BLPVLC        TASVLC
181400941114     C                   Z-ADD     BLPNCR        TASNCR
181500000627     C     *IN52         IFEQ      *OFF
181600000627     C     TASFVF        ORNE      'T'
181700000627     C     TASFVF        ANDNE     'Z'
181800000627     C     BLPVLF        ANDGT     TASVLF
181900950114     C                   Z-ADD     BLPVLF        TASVLF
182000950114     C                   MOVE      BLPFVF        TASFVF
182100000627     C                   ENDIF
182200950114     C                   Z-ADD     BLPVLB        TASVLB
182300950114     C                   MOVE      BLPFVB        TASFVB
182400990817     C*
182500070223      * LE SPEDIZIONI DOVRANNO ESSERE SEMPRE 'B'
182600070223      * (SDI NON esiste più dal 2001 compreso...)
182700001201     C                   MOVE      'B'           TASSCL
182800971215     C*
182900941114     C                   Z-ADD     BLPRMN        TASRMN
183000990615     C*
183100950114     C                   MOVEL     BLPFLB        TASFLB
183200941114    1C     BLPFLP        IFGT      0
183300941114     C* C'E' TRANSITO SE :
183400941114     C* 1) BLPFLP NON E' = ALLA LINEA DI ARRIVO
183500941114     C* 2) BLPFLP NON E' = AL TERMINAL DI ARRIVO DELLA LINEA ARRIVO
183600040420     c*  20/4/04 il 3° e 4° caso non esistono più: transito ammesso per
183700040420     c*    stesso terminal di partenza <> terminal di arrivo
183800971202     C* 3) BLPFLP NON E' = AL TERMINAL PARTENZA  DELLA LINEA ARRIVO
183900971202     C* 4) TER PARTENZA DI BLPFLP NON E' = AL TERMINAL PARTENZA
184000991104     C*                                        DELLA LINEA ARRIVO
184100991104    2C     BLPFLP        IFNE      BLPTFA
184200991104     C     BLPFLP        ANDNE     BLPLNA
184300040420     C                   Z-ADD     BLPFLP        TASFLP
184400040420     C                   ENDIF
184500040603      * se la spedizione ha data >= 19/07/2004 non devo più impostare TASFLP
184600040603     c                   If        blpdsp >= 20040719
184700040603     c                   Clear                   Tasflp
184800040603     c                   EndIf
184900040420     C
185000941114    1C                   ENDIF
185100941114     C*
185200941114     C                   MOVEL     BLPFAP        TASFAP
185300941114     C     BLPFAP        IFEQ      ' '
185400941114     C                   MOVEL     'C'           TASFAP
185500941114     C                   ENDIF
185600970714     C                   MOVEL     O95CTS        TASMCT
185700990621     C                   MOVEL     BLPRSD        TASRSD
185800990621     C                   MOVEL     BLPIND        TASIND
185900990621     C                   MOVEL     BLPCAD        TASCAD
186000990621     C                   MOVEL     BLPLOD        TASLOD
186100990621     C                   MOVEL     BLPPRD        TASPRD
186200990621     C                   MOVEL     BLPNZD        TASNZD
186300941114     C                   MOVEL     BLPCAM        TASCAM
186400950118     C                   MOVEL     BLPNZM        TASNZM
186500941114     C                   MOVE      BLPFFD        TASFFD
186600941114     C                   MOVE      BLPCTS        TASCTS
186700941114     C                   Z-ADD     BLPTFA        TASTFA
186800941114     C                   Z-ADD     BLPTFP        TASTFP
186900941114     C                   Z-ADD     BLPZNC        TASZNC
187000950114     C                   MOVE      BLPFDN        TASFDN
187100941114     C* CAMPI NUOVI
187200050311     C                   z-add     blpdim        TASDIM
187300950114     C                   Z-ADD     BLPPDR        TASPDR
187400950126     C                   Z-ADD     BLPDRT        TASDRT
187500991104     C                   MOVEL     BLPFPP        TASFPP
187600950117     C                   MOVEL     BLPSOP        TASSOP
187700950117     C                   MOVEL     BLPFBR        TASFBR
187800950117     C                   MOVEL     BLPGGA        TASGGA
187900950117     C                   MOVEL     BLPGMA        TASGMA
188000950117     C                   MOVEL     BLPGVA        TASGVA
188100950118     C                   MOVEL     BLPNRT        TASNRT
188200950118     C                   Z-ADD     BLPQFT        TASQFT
188300991104     C                   MOVEL     BLPFNS        TASFNS
188400991104     C                   MOVEL     BLPGC1        TASGC1
188500991104     C                   MOVEL     BLPGC2        TASGC2
188600991104     C                   MOVEL     BLPNAS        TASNAS
188700991104     C                   Z-ADD     BLPNCP        TASNCP
188800991104     C                   Z-ADD     BLPPKC        TASPKC
188900991104     C                   Z-ADD     BLPFLS        TASFLS
189000991104     C                   Z-ADD     BLPNCD        TASNCD
189100991104     C                   Z-ADD     BLPNCA        TASNCA
189200991104     C                   MOVEL     BLPXCO        TASXCO
189300991104     C                   MOVEL     BLPCTM        TASCTM
189400991104     C                   Z-ADD     BLPDBR        TASDBR
189500991104     C                   Z-ADD     BLPDUC        TASDUC
189600030401      * imposto i campi per TASFLO
189700030401     c                   Clear                   Dtasflo
189800041202     c   52              movel     tasflo        dtasflo
189900030401     c                   Eval      kApdPdr = TasPdr
190000030401     c     kFiapd        Chain     Fiapd01l
190100030401     c                   If        Not %Found(Fiapd01l) or ApdAtb = 'A' or
190200030401     c                             ApdPdd = 'S'
190300030401     c                   Eval      §FloSca = 'C'
190400030401     c                   Eval      §FloEti = 'C'
190500030401     c                   EndIf
190600030401     c                   Eval      Tasflo = Dtasflo
190700060303     c* se bolla "M "-prepagato annullato imposto data consegna merce = dbr
190800060303     c                   if        blpcbo = 'M '
190900060303     c                   z-add     blpdbr        tasdcm
191000060331     c                   z-add     blpdbr        tasddc
191100060331     c                   z-add     9999999       tasndc
191200060303     c                   endif
191300941114     C*
191400941114     C                   ENDSR
191500000822     C**---------------------------------------------------------
191600000822     C** SCRITTURA FILE ERRORI PER UFFICIO ASSICURAZIONE MERCI
191700000822     C**---------------------------------------------------------
191800000823     C** .. Solo se non già presente
191900000823     C*
192000100225     C***  SCRASS        BEGSR
192100100225     C***  KEAS          SETLL     FIARBE2C                               38
192200100225 --  C***  *IN38         IFEQ      *OFF
192300100225     C***
192400100225     C***                CLEAR                   FIARBTEE
192500100225     C***
192600100225     C***                Z-ADD     BLPAAS        ARBAAS
192700100225     C***                Z-ADD     BLPLNP        ARBLNP
192800100225     C***                Z-ADD     BLPNRS        ARBNRS
192900100225     C***                Z-ADD     BLPNSP        ARBNSP
193000100225     C***                Z-ADD     BLPLNA        ARBLNA
193100100225     C***                MOVE      '1'           ARBTRC
193200100225     C***                MOVE      BLPCBO        ARBCBO
193300100225     C***                Z-ADD     BLPDIM        ARBDTV
193400100225     C***                Z-ADD     BLPIAS        ARBIAS
193500100225     C***                MOVEL     BLPVAS        ARBVAS
193600100225     C***                Z-ADD     603           ARBFEV
193700100225     C***                MOVEL     EAS(1)        ARBDER
193800100225     C***                MOVE      'R'           ARBVAR
193900100225     C***
194000100225     C***                WRITE     FIARBTEE
194100100225     C***
194200100225 --  C***                ENDIF
194300100225     C***                ENDSR
194400941114     C**---------------------------------------------------------
194500941114     C* ROUTINE INIZIALE
194600941114     C**---------------------------------------------------------
194700941114     C     *INZSR        BEGSR
194800941114     C*
194900941114     C     *ENTRY        PLIST
195000941114     C                   PARM                    KPJBA
195100941114     C* SE IL MEMBRO DA TRASCODIFICARE NON E' STATO PASSATO
195200941114     C* ASSUMO IL PRIMO MEMBRO DEL FILE
195300941114     C     MBRFIL        IFEQ      *BLANK
195400941129     C                   MOVEL     'M000  '      MBRFIL
195500941114     C                   END
195600941114     C*
195700030128     C                   CALL      'X§PARUT'
195800030128     C                   PARM                    UT§DSE0F
195900941114     C                   MOVEL     RAGUT         RSUT             20
196000030128     C                   MOVEL     REC80         CNCR80
196100941114     C*--- RICERCA CAPOCONTI------------------------------------------*
196200941114     C                   DO        50            X                 2 0
196300941114     C                   MOVE      TCU(X)        TCUDS
196400941114     C     F56           CABNE     'CG'          END1
196500941114     C     F4            COMP      '1'                                    21
196600941114     C     F4            COMP      '2'                                    22
196700941114     C     F4            COMP      '3'                                    23
196800941114     C     F4            COMP      '6'                                    27
196900941114     C** 1 CLIENTI   21
197000941114     C** 2 FORNITORI 22
197100941114     C** 3 AGENTI    23
197200941114     C     F3            COMP      '0'                                242425
197300941114     C     F3            COMP      'I'                                    26
197400941114     C** 0 ITALIA   25
197500941114     C** 1 ESTERO   24
197600941114     ** I CAPO CONTO IVA
197700941114     C   21
197800941114     CAN 24              Z-ADD     KCU(X)        KCE               4 0
197900941114     C   21
198000941114     CAN 25              Z-ADD     KCU(X)        KCI               4 0
198100941114     C   22
198200941114     CAN 24              Z-ADD     KCU(X)        KFE               4 0
198300941114     C   22
198400941114     CAN 25              Z-ADD     KCU(X)        KFI               4 0
198500941114     C   23
198600941114     CAN 24              Z-ADD     KCU(X)        KAE               4 0
198700941114     C   23
198800941114     CAN 25              Z-ADD     KCU(X)        KAI               4 0
198900941114     C   26              Z-ADD     KCU(X)        KIVA              4 0
199000941114     C   27              Z-ADD     KCU(X)        KBNA              4 0
199100941114     C     END1          TAG
199200941114     C                   END
199300941114     C*---------------------------------------------------------------*
199400941114     C     *LIKE         DEFINE    TASSV1        WSV1
199500990621     C     *LIKE         DEFINE    TAATRC        §TRC
199600991103     C     *LIKE         DEFINE    TAATRC        WTRC
199700000627     C     *LIKE         DEFINE    TASTBL        WTBL
199800000627     C     *LIKE         DEFINE    TA4TRC        SA4TRC
199900000628     C     *LIKE         DEFINE    CSBCAS        WARCAS
200000000628     C     *LIKE         DEFINE    CSBCAS        WDIFFE
200100000628     C     *LIKE         DEFINE    CSBVCA        WARVCA
200200000628     C     *LIKE         DEFINE    KPJBU         KPJBUX
200300941114     C*---------------------------------------------------------------*
200400941114     C*
200500941114     C     KTAB          KLIST
200600941114     C                   KFLD                    CODUT
200700941114     C                   KFLD                    COD               2
200800941114     C                   KFLD                    KEY               8
200900941114     C     KTAB2         KLIST
201000941114     C                   KFLD                    CODUT
201100941114     C                   KFLD                    COD               2
201200941114     C     KNSP          KLIST
201300990618     C                   KFLD                    BLPAAS
201400941114     C                   KFLD                    BLPLNP
201500941114     C                   KFLD                    BLPNRS
201600941114     C                   KFLD                    BLPNSP
201700000627     C     KNSP1         KLIST
201800000627     C                   KFLD                    BLPAAS
201900000627     C                   KFLD                    BLPLNP
202000000627     C                   KFLD                    BLPNRS
202100000627     C                   KFLD                    BLPNSP
202200000627     C                   KFLD                    WTBL
202300990621     C*
202400990621     C     KNSPE         KLIST
202500990621     C                   KFLD                    BLPAAS
202600990621     C                   KFLD                    BLPLNP
202700990621     C                   KFLD                    BLPNRS
202800990621     C                   KFLD                    BLPNSP
202900990621     C                   KFLD                    §TRC
203000941114     C* ACCESSO PER NUMERO DI SPEDIZIONE
203100941114     C     KASS          KLIST
203200990618     C                   KFLD                    BLPAAS
203300941114     C                   KFLD                    BLPLNP
203400990621     C                   KFLD                    BLPNRS
203500941114     C                   KFLD                    BLPNSP
203600991103     C* ACCESSO PER NUMERO DI SPEDIZIONE
203700991103     C     KASS1         KLIST
203800991103     C                   KFLD                    BLPAAS
203900991103     C                   KFLD                    BLPLNP
204000991103     C                   KFLD                    BLPNRS
204100991103     C                   KFLD                    BLPNSP
204200991103     C                   KFLD                    WTRC
204300061218     C     KASS2         KLIST
204400061218     C                   KFLD                    BLPAAS
204500061218     C                   KFLD                    BLPLNP
204600061218     C                   KFLD                    BLPNRS
204700061218     C                   KFLD                    BLPNSP
204800061218     C                   KFLD                    kTRD
204900941114     C* ACCESSO PER NUMERO DI SPEDIZIONE  AI FILE AGGANCIATI
205000941114     C*§
205100000823     C* SETLL PER UFFICIO ASSICURAZIONE MERCI
205200000823     C     KEAS          KLIST
205300000823     C                   KFLD                    BLPAAS
205400000823     C                   KFLD                    BLPLNP
205500000823     C                   KFLD                    BLPNRS
205600000823     C                   KFLD                    BLPNSP
205700000823     C                   KFLD                    EAS(1)
205800000823     C                   KFLD                    W08ZER            8 0
205900000823     C                   KFLD                    BLPDIM
206000000823     C                   KFLD                    W06ZER            6 0
206100030401
206200030401      * File Fiapd01l
206300030401     c     kFiapd        Klist
206400030401     c                   Kfld                    kApdTip
206500030401     c                   Kfld                    kApdPdr
206600000823     C*
206700000823     C                   Z-ADD     0             W08ZER
206800000823     C                   Z-ADD     0             W06ZER
206900000823     C*
207000030108     C* GIRO DATA DEL GIORNO
207100030108     C                   TIME                    W0140
207200030108     C                   MOVE      W0140         G02DAT
207300030108     C                   MOVEL     *BLANK        G02ERR
207400030108     C                   CALL      'XSRDA8'
207500030108     C                   PARM                    WLBDAT
207600030108     C                   Z-ADD     G02INV        DATEU
207700030128     C                   movel     w0140         TimeU
207800030108     C*
207900941114     C                   SETON                                        OF
208000040325     C                   SETON                                        OE
208100000627     C* CARICO IN SCHIERA ELENCO TIPI RECORD TITA4 CANCELLABILI
208200000628     C                   Z-ADD     0             II                3 0
208300000627     C                   CLEAR                   SDL
208400000627     C     'TA4'         SETLL     TNTBE01L
208500000627     C     'TA4'         READE     TNTBE01L                               32
208600000627    1C     *IN32         DOWEQ     *OFF
208700000627    2C     TBEATB        IFEQ      *BLANKS
208800000627     C                   MOVEL     TBEUNI        DTA4
208900000627    3C     §TA4DL        IFEQ      'S'
209000000627     C                   ADD       1             II
209100000627     C                   MOVEL     TBEKE1        SDL(II)
209200000627    3C                   ENDIF
209300000627    2C                   ENDIF
209400000627     C     'TA4'         READE     TNTBE01L                               32
209500000627    1C                   ENDDO
209600030128      *
209700030128      * Effettuo caricamento dei tipi record FIAR5 da NON trasmettere
209800030128     c                   clear                   ii
209900030128     c                   clear                   AR5n
210000030128     c                   eval      TBEcod   = 'AR5'
210100030128     c     TBEcod        setll     TNTBE000
210200030128     c     TBEcod        reade     TNTBE000
210300030128do  1c                   DOW       Not %Eof(TNTBE01L)
210400030128if  2c                   if        TBEatb   = *blanks
210500030128     c                   eval      dAR5 = TBEuni
210600030128if  3c                   IF        §AR5trmi <> 'S'
210700030128     c                   add       1             ii
210800030128     c                   movel     TBEke1        AR5n(ii)
210900030128e   3c                   ENDIF
211000030128e   2c                   endif
211100030128     c     TBEcod        reade     TNTBE000
211200030128e   1c                   ENDDO
211300000628     C*
211400000628     C* CARICO IN SCHIERA ELENCO DIVISE ACCETTATE IN ITALIA
211500000628     C*
211600000628     C                   MOVEL     'CV'          COD
211700000628     C                   CLEAR                   DVA
211800011029     C                   CLEAR                   DEC
211900000628     C                   CLEAR                   II
212000000628     C     KTAB2         SETLL     TABEL
212100000628     C     KTAB2         READE     TABEL                                  32
212200000628     C     *IN32         DOWEQ     *OFF
212300000628     C     TBLFLG        IFEQ      ' '
212400000628     C                   MOVEL     TBLUNI        DSCV
212500000628     C     §CVDVA        IFEQ      'S'
212600000628     C                   ADD       1             II
212700000628     C                   MOVEL     TBLKEY        DVA(II)
212800011029     C                   Z-ADD     §CVDEC        DEC(II)
212900000628     C                   ENDIF
213000000628     C                   ENDIF
213100000628     C**
213200000628     C     KTAB2         READE     TABEL                                  32
213300000628     C                   ENDDO
213400001002     C*
213500001002     C* CARICO IN SCHIERA I TIPI BOLLA NON PREVISTI DALL'ASSICURAZ
213600001002     C*
213700001002     C                   MOVEL     'TB'          COD
213800001002     C                   CLEAR                   TBL
213900001002     C                   CLEAR                   II
214000001002     C     KTAB2         SETLL     TABEL
214100001002     C     KTAB2         READE     TABEL                                  32
214200001002     C     *IN32         DOWEQ     *OFF
214300001002     C     TBLFLG        IFEQ      ' '
214400001002     C                   MOVEL     TBLUNI        DSTB
214500001002     C     §TBFAS        IFEQ      '0'
214600001002     C                   ADD       1             II
214700001002     C                   MOVEL     TBLKEY        TBL(II)
214800001002     C                   ENDIF
214900001002     C                   ENDIF
215000001002     C**
215100001002     C     KTAB2         READE     TABEL                                  32
215200001002     C                   ENDDO
215300971215     C*
215400941114     C                   ENDSR
215500030416      **-------------------------------------------------------------**
215600030416      ** SENDMSG                                                     **
215700030416      **-------------------------------------------------------------**
215800030416     C     SendMSG       BEGSR
215900030416      *
216000111031     C                   movel     *blanks       wrkEml          253
216100111031     C                   movel     *blanks       wrkMsg         5000
216200111031     C                   movel     *blanks       wrkOgg           44
216300120305     c                   eval      wrkeml='ced@brt.it'
216400120305     c*
216500111031     c                   eval      wrkogg = 'ERRORE in ricezione bolle partite'
216600111031     c                   eval      wrkmsg = 'Membro: ' + MBRFIL
216700030416     c                                    + '   Job: ' + KNMEB
216800030416     c                                    +        '/' + KNMUS
216900030416     c                                    +        '/' + %char(KNRJO)
217000030416     c                                    + '   Pgm: ' + SDSpgm
217100111031     c                                    + '>>>>>  Rieseguire il lavoro <<<<<'
217200030416      *
217300111031     c                   call      'TIS701C'
217400111031     C                   parm                    wrkEml
217500111031     C                   parm                    wrkOgg
217600111031     C                   parm                    wrkMsg
217700030416      *
217800030416     C                   ENDSR
217900040325      **-------------------------------------------------------------**
218000040325      ** SENDMSGE -                                                  **
218100040325      **-------------------------------------------------------------**
218200040325     C     SendMSGE      BEGSR
218300111031      *
218400111031     C                   movel     *blanks       wrkEml          253
218500111031     C                   movel     *blanks       wrkMsg         5000
218600111031     C                   movel     *blanks       wrkOgg           44
218700120305     c                   eval      wrkeml='ced@brt.it'
218800120305     c*
218900111031     c                   eval      wrkogg = 'ERRORI Ricez.bolle partite'
219000111031     c                                    + ' fil. ' +%char(wmbr)
219100111031     c                   eval      wrkmsg = 'Controllare la stampa per ufficio'
219200111031     c                                    + ' CED in PRT01'
219300040325      *
219400111031     c                   call      'TIS701C'
219500111031     C                   parm                    wrkEml
219600111031     C                   parm                    wrkOgg
219700111031     C                   parm                    wrkMsg
219800040325      *
219900040325     C                   ENDSR
220000030416      *
220100061211      **-------------------------------------------------------------**
220200061211      * Ricevo eventuale estensione TESTATA BOLLA
220300061211      **-------------------------------------------------------------**
220400061211     c     AggFIAR5      BEGSR
220500061211     C                   If        *in91
220600061211     C     KASS          setll     FIAR500R
220700061211     C                   Do        *hival
220800061211     C     KASS          reade     FIAR500R                               32
220900061211     C   32              leave
221000110607     c                   if        ar5trd='EMD'
221100110607     c                   iter
221200110607     c                   endif
221300061211      * - fleggo come ricevuto in filiale
221400061211     C                   eval      AR5ft1 = 'R'
221500061211     C                   eval      AR5dt1 = DateU
221600061211      * - fleggo come ricevuto in sede
221700061211     C                   eval      AR5ft2 = 'R'
221800061211     C                   eval      AR5dt2 = DateU
221900061211      * - verifico se per il tipo record di FIAR5 è previsto il
222000061211      *   ritorno info al cliente: se non è previsto imposto
222100061211      *   il record come già trasmesso (cioè da non trasmettere).
222200061211     c                   eval      *in30  =  *off
222300061211     c     AR5trd        lookup    AR5n                                   30
222400061211if  3c                   IF        *in30
222500061211     c                   eval      AR5ft3 = 'T'
222600061211     c                   eval      AR5dt3 = *all'9'
222700061211x   3c                   ELSE
222800061211     c                   clear                   AR5ft3
222900061211     c                   clear                   AR5dt3
223000061211e   3c                   ENDIF
223100061211      * se sto ricevendo un rcd di tipo 'GEN'
223200061211      * controllo per impostare il flag dei colli di valore su titas
223300061218     c****               If        Ar5Trd = 'GEN'
223400061218     c****               Eval      dAr5Gen = Ar5Uni
223500061218     c****               If        §Ar5Bva = 'V'
223600061218     c****               Eval      dtasflo = TasFlo
223700061218     c****               Eval      §FloTba = §Ar5Bva
223800061218     c****               Eval      TasFlo = dtasflo
223900061218     c****               EndIf
224000061218     c****               EndIf
224100061211      *
224200061211     C                   WRITE     FIAR5000                             38      IGNORO SE GIA' PRESE
224300061211     C                   add       1             NRec5
224400061211     C                   Enddo
224500061211     C                   Endif
224600061211     c                   ENDSR
224700061211      **-------------------------------------------------------------**
224800061211      **    Aggiorno flag legami bolla sulla bolla figlia
224900061211      **-------------------------------------------------------------**
225000061211     c     AggLBL        BEGSR
225100061211     c
225200061211     c* Se esiste per la bolla un legame come figlia, aggiorno
225300061211     c     knsp          setll     fnlbl01l
225400061211     c     knsp          reade     fnlbl01l
225500061211     c                   dow       not %eof(fnlbl01l)
225600061211     c                   call      'FNLG25R'
225700061211     c                   parm                    recordlbl
225800061211     c                   parm                    i25tla            1
225900061211     c                   parm      'N'           i25agglbl         1
226000061211     C                   PARM                    o25fa1            1
226100061211     C                   PARM                    o25fca            1
226200061211     C                   PARM                    o25bofi           1
226300061211     C                   PARM                    o25boma           1
226400061211     c
226500061211     c* Aggiorno flag legame bolle e C/A
226600061211     c                   eval      tasfa1=o25fa1
226700061211     c                   eval      tasfca=o25fca
226800061211     c
226900061211     c* se ho trovato bolla mamma, aggiorno legame bolla coi dati
227000061211     c                   if        o25boma='S'
227100061211     c                   eval      lblft1='S'
227200061211     c* solo la prima volta aggiorno la data
227300061211     c                   if        lbldt1=0
227400080516     c                   movel     dateu         lbldt1
227500061211     c                   endif
227600061211     c                   endif
227700061211     c                   eval      lblft2='S'
227800061211     c                   if        lbldt2=0
227900080516     c                   movel     dateu         lbldt2
228000061211     c                   endif
228100061211     c
228200061211     c                   update    fnlbl000
228300061211     c
228400061211     c     knsp          reade     fnlbl01l
228500061211     c                   enddo
228600061211     c                   ENDSR
228700080828      **-------------------------------------------------------------**
228800080828      * controllo da DCLI de devo impostare data inventario ricez DDT
228900080828      **-------------------------------------------------------------**
229000100317     c**
229100100317     c**   contrDCLI     BEGSR
229200100317    1c**                 if        tasccm<>savccm
229300100317     c**                 Clear                   tibs02
229400100317     c**                 Clear                   dcli
229500100317     c**                 Movel     'C'           T02Mod
229600100317     c**                 Movel     KNSIF         T02Sif
229700100317     c**                 Movel     'CLI'         T02Cod
229800100317     c**                 Movel     tasccm        T02Ke1
229900100317     c**                 Call      'TIBS02R'
230000100317     c**                 Parm                    Kpjba
230100100317     c**                 Parm                    tibs02
230200100317    2c**                 If        T02Err = *Blanks
230300100317     c**                 Movel     T02Uni        Dcli
230400100317    2c**                 endif
230500100317     c**                 movel     tasccm        savccm            7 0
230600100317    1c**                 endif
230700100317     c**
230800100317    1c**                 if        §cliidt='S'
230900100317     c**   kass2         chain     fiar531c
231000100317    2c**                 if        %found(fiar531c)
231100100317     c**                 movel     ar5uni        dar5gen
231200100317     c**                 eval      §ar5drdt='99999999'
231300100317     c**                 movel     dar5gen       ar5uni
231400100317     c**                 update    fiar5000
231500100317   x2c**                 else
231600100317     c**                 clear                   fiar5000
231700100317     c**                 eval      ar5aas=tasaas
231800100317     c**                 eval      ar5lnp=taslnp
231900100317     c**                 eval      ar5nrs=tasnrs
232000100317     c**                 eval      ar5nsp=tasnsp
232100100317     c**                 eval      ar5trd='GEN'
232200100317     c**                 movel     dateu         ar5dac
232300100317     c**                 time                    ar5orc
232400100317     c**                 clear                   dar5gen
232500100317     c**                 eval      §ar5drdt='99999999'
232600100317     c**                 movel     dar5gen       ar5uni
232700100317     c**
232800100317     C**                 eval      AR5ft1 = 'R'
232900100317     C**                 eval      AR5dt1 = DateU
233000100317     C**                 eval      AR5ft2 = 'R'
233100100317     C**                 eval      AR5dt2 = DateU
233200100317     c**Per questo dato non devo trasmettere al cliente essendo un dato
233300100317     c** gestionale nei pgm e non per il cliente
233400100317     c**                 eval      AR5ft3 = 'T'
233500100317     c**                 eval      AR5dt3 = *all'9'
233600100317     c**
233700100317     C**                 WRITE     FIAR5000                             38      IGNORO SE GIA' PRESE
233800100317e   2c**                 ENDIF
233900100317    1c**                 ENDIF
234000100317     c**
234100100317     c**                 ENDSR
234200061211      **-------------------------------------------------------------**
234300950118     OPRTF198   E            HDG            2 02
234400000000     O                       RSUT                20
234500930222     O                                           60 '**  RICEZIONE  BOLLE  PA'
234600970303     O                                         +  0 'RTITE  **'
234700040326     o                                         +  5 'UFFICIO CED'
234800900601     O                       UDATE              110 '  /  /  '
234900991117     O                                          120 'TRTR36R'
235000000000     O                                          128 'PAG.'
235100000000     O                       PAGE          Z    132
235200900601     O          E            HDG            2
235300961022     O                                           16 'Punto Operativo:'
235400961022     O                       WMBR                21
235500961022     O                                           23 '-'
235600961022     O                       DESFIL              44
235700900601     O                                         + 10 'MEMBRO:'
235800900601     O                       MBRFIL            +  1
235900910619     O                                         + 10 'FILE:'
236000930222     O                       LEGFIL            +  1
236100941128     O          E            HDG            1
236200000000     O                       S(1)                66
236300000000     O                       S(2)               132
236400900604     O          E            HDG            1
236500000000     O                       S(3)                66
236600941128     O                       S(5)               132
236700941128     O          E            HDG            1
236800941128     O                       S(4)                66
236900941128     O                       S(6)               132
237000900713     O          E            HDG            1
237100000000     O                       S(1)                66
237200000000     O                       S(2)               132
237300000000     O          E            ERRO        1
237400950118     O                       WAAS2                2
237500941128     O                       BLPLNP        Z   +  1
237600941128     O                       BLPNRS        Z   +  1
237700941128     O                       BLPNSP        Z   +  1
237800950114     O                       BLPCBO            +  2
237900950118     O                       BLPKSC            +  1
238000950118     O                       BLPLNA        Z   +  1
238100941128     O                       STARSD            +  2
238200950118     O                       BL9CAS        4   +  1
238300941128     O                       BLPRMN        Z   +  1
238400950118     O                       ERR               +  1
238500950118     O                       ER05              +  1
238600040326     OPRTF198X  E            HDGF           2 02
238700040325     O                       RSUT                20
238800040325     O                                           60 '**  RICEZIONE  BOLLE  PA'
238900040325     O                                         +  0 'RTITE  **'
239000040326     o                                         +  5 'UFFICIO FATTURAZIONE'
239100040325     O                       UDATE              110 '  /  /  '
239200040325     O                                          120 'TRTR36R'
239300040325     O                                          128 'PAG.'
239400040325     O                       PAGE          Z    132
239500040325     O          E            HDGF           2
239600040325     O                                           16 'Punto Operativo:'
239700040325     O                       WMBR                21
239800040325     O                                           23 '-'
239900040325     O                       DESFIL              44
240000040325     O                                         + 10 'MEMBRO:'
240100040325     O                       MBRFIL            +  1
240200040325     O                                         + 10 'FILE:'
240300040325     O                       LEGFIL            +  1
240400040325     O          E            HDGF           1
240500040325     O                       S(1)                66
240600040325     O                       S(2)               132
240700040325     O          E            HDGF           1
240800040325     O                       S(3)                66
240900040325     O                       S(5)               132
241000040325     O          E            HDGF           1
241100040325     O                       S(4)                66
241200040325     O                       S(6)               132
241300040325     O          E            HDGF           1
241400040325     O                       S(1)                66
241500040325     O                       S(2)               132
241600040325     O          E            ERROF       1
241700040325     O                       WAAS2                2
241800040325     O                       BLPLNP        Z   +  1
241900040325     O                       BLPNRS        Z   +  1
242000040325     O                       BLPNSP        Z   +  1
242100040325     O                       BLPCBO            +  2
242200040325     O                       BLPKSC            +  1
242300040325     O                       BLPLNA        Z   +  1
242400040325     O                       STARSD            +  2
242500040325     O                       BL9CAS        4   +  1
242600040325     O                       BLPRMN        Z   +  1
242700040325     O                       ERR               +  1
242800040325     O                       ER05              +  1
242900001002**         CMA1
243000991103CHKOBJ OBJ(FIBLPA0R) OBJTYPE(*FILE)     MBR(M234567890)
243100911105**         CMA2
243200991103ALCOBJ OBJ((FIBLPA0R *FILE *EXCL           M234567890))
243300911105**         CMA3
243400991103OVRDBF FILE(FIBLPA0R)                   MBR(M234567890)
243500911105**         CMC1
243600991103CHKOBJ OBJ(FIAR600R) OBJTYPE(*FILE)     MBR(M234567890)
243700911105**         CMC2
243800991103ALCOBJ OBJ((FIAR600R *FILE *EXCL           M234567890))
243900911105**         CMC3
244000991103OVRDBF FILE(FIAR600R)                   MBR(M234567890)
244100900526**         CMD1
244200991103CHKOBJ OBJ(FIAR700R) OBJTYPE(*FILE)     MBR(M234567890)
244300900526**         CMD2
244400991103ALCOBJ OBJ((FIAR700R *FILE *EXCL           M234567890))
244500900526**         CMD3
244600991103OVRDBF FILE(FIAR700R)                   MBR(M234567890)
244700970212**         CME1
244800060220CHKOBJ OBJ(FIAR400R) OBJTYPE(*FILE)     MBR(M234567890)
244900970212**         CME2
245000060220ALCOBJ OBJ((FIAR400R *FILE *EXCL           M234567890))
245100970212**         CME3
245200060220OVRDBF FILE(FIAR400R)                   MBR(M234567890)
245300000906**         CMH1
245400000906CHKOBJ OBJ(TITAH00R) OBJTYPE(*FILE)     MBR(M234567890)
245500000906**         CMH2
245600000906ALCOBJ OBJ((TITAH00R *FILE *EXCL           M234567890))
245700000906**         CMH3
245800000906OVRDBF FILE(TITAH00R)                   MBR(M234567890)
245900030108** --- $CMD
246000111117CHKOBJ OBJ(FIAR500R) OBJTYPE(*FILE)      MBR(M234567890)
246100111117ALCOBJ OBJ((FIAR500R *FILE *EXCL            M234567890))
246200111117OVRDBF FILE(FIAR500R)                    MBR(M234567890)
246300040326OVRPRTF PRTF198X USRDTA('Uff.Fatt.')
246400111117OVRPRTF PRTF198 USRDTA('Uff.CED') OUTQ(PRT01) HOLD(*YES)
246500030108**     ER
246600950117CODICE BOLLA ERRATO        (NON CARICATA) 1
246700990903SPEDIZIONE ESISTENTE       (NON CARICATA) 2
246800950228COD.MITTENTE INESISTENTE IN ANAGRAF       3 - CARICATA
246900990903                                          4
247000950117CAP MITTENTE INESISTENTE   (NON CARICATA) 5
247100000629CAP MITTENTE INESISTENTE                  6 - CARICATA
247200941129BOLLA CREATA MA C/A GIA' IN TNCSB00F      7
247300941129BOLLA CREATA MA C/A ESISTE CON <> TIP BOL 8
247400941129BOLLA CREATA MA C/A ESISTE CON IMPORTO <> 9
247500041105
247600960918COD.MITTENTE BLOCCATO                    11 - CARICATA
247700041105ESISTE TITAA30C RECORD "M" (NON CARICATA)12
247800041105ESISTENTE RECORD IN TITA430C(NO CARICATA)13
247900041105ESISTENTE RECORD IN TITA730C(NO CARICATA)14
248000041105ESISTENTE RECORD IN TITAT30C(NO CARICATA)15
248100041105ESISTE TITAA30C RECORD "O" (NON CARICATA)16
248200000628SPED.AUTOGENERATA CONTABIL.(NON CARICATA)17
248300030108**     S
248400950118-----------------+---+-------+----+---------------------+---------  A1
248500950118--+---------------+-------------+---------------------------------  A2
248600991104  SPEDIZIONE     'COD'CODICE ' LIN' DESTINATARIO        '    C/ASS  B3
248700950118AN LNP SER NUMERO'BOL'SPEDIZ ' ARR'                     '           C4
248800991104EGNO  ' RIFERIMENTO '   N    O    T    E                            B5
248900991104      '    MITTENTE '                                               C6
249000030108**     MSG
249100000629AGGIOR. BOLLA AUTOGENERATA XXX XX XXXXXXX XXXX  LNA XXX               1
249200000629CON CONTRASSEGNO XXX 1234567890,123 XXXXXXXXXXXXXXXXXXXX              2
249300000629BOLLA PERVENUTA CON CONTRASSEGNO XXX 123456789,123                    3
249400000629MANTENUTO  CONTRASSEGNO INIZIALE XXX 123456789,123                    4
249500000629ASSUNTO    CONTRASSEGNO NUOVO    XXX 123456789,123                    5
249600000628RIATTIVATO CONTRASSEGNO                                               6
249700010117GIA' CONSEGNATA. ATTENZIONE: AGGIUNTO CONTRASSEGNO!                   7
249800000628                                                                      8
249900000629CON CONTRASSEGNO XXX 1234567890,123 PAGATO INTESTATO A 1234567        9
250000000629BOLLA PERVENUTA CON C/A XXX 123456789,123 INTESTATO A 1234567         0
250100000629MANTENUTO C/A INIZIALE XXX 123456789,123 INTEST. A 1234567            1
250200000629BOLLA PERVENUTA CON C/A INTESTATO A 1234567                           2
250300000629MANTENUTA  INTESTAZIONE INIZIALE    1234567                           3
250400000823**     EAS
250500001002IMMESSO IMP.ASSIC.  BOLLA NON ASSIC
