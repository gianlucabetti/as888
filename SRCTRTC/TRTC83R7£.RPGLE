000100001003     H DECEDIT('0,') DATEDIT(*DMY.)
000200000929      ***********************************************************************
000300001010      *                                                                     *
000400010514      *   Il pgm serve a dividere in 2 i Flat Files  - IFCSUM -             *
000500010514      *   della Pracht durante la scrittura del Manifest                    *
000600010514      *   La Pracht vuole 2 IFCSUM separati con le seguenti regole:         *
000700010514      *     1) tutte quelle spedizioni monocollo non in contrassegno        *
000800010727      *        inferiore/uguale ai 30 kg. ed in porto franco.               *
000900050905      *        e per volume inferiori a 0.15 mt.cubi. (tipo file 1)         *
001000050905      *     2) tutte le rimanenti spedizioni.         (tipo file 2)         *
001100050905      *? Se la destinazione è Polonia i relativi tipi files diventano       *
001200050905      *? rispettivamente  1 -->3   e  2 -->4                                *
001300050905      *                                                                     *
001400021009      *   INPUT   -> EDIFCSUM                                               *
001500021009      *   WORKF   -> EDIFCSUM0                                              *
001600010514      *                                                                     *
001700021009      *   OUTPUT  -> EDIFCSUM1                      <= 30 Kg.               *
001800021009      *   OUTPUT  -> EDIFCSUM2                      >  30 Kg.               *
001900000929      *                                                                     *
002000050905      *   OUTPUT  -> EDIFCSUM3                      <= 30 Kg. POLONIA       *
002100050905      *   OUTPUT  -> EDIFCSUM4                      >  30 Kg. POLONIA       *
002200050905      *                                                                     *
002300000929      ***********************************************************************
002400010514     FEDIFCSUM  IF   E             DISK    RENAME(RRIFCSUM:INPUT000)
002500000929     F*---------
002600010514     FEDIFCSUM0 IF A E             DISK    usropn
002700000929     F                                     RENAME(RRIFCSUM:WORKF000)
002800010524     F*---------
002900010524     FEDIFCSUM1 UF   E             DISK    usropn
003000010524     F                                     RENAME(RRIFCSUM:IFCSUM01)
003100010524     F*---------
003200010524     FEDIFCSUM2 UF   E             DISK    usropn
003300010524     F                                     RENAME(RRIFCSUM:IFCSUM02)
003400050905     F*---------
003500050905     FEDIFCSUM3 UF   E             DISK    usropn
003600050905     F                                     RENAME(RRIFCSUM:IFCSUM03)
003700050905     F*---------
003800050905     FEDIFCSUM4 UF   E             DISK    usropn
003900050905     F                                     RENAME(RRIFCSUM:IFCSUM04)
004000001002     F*---------
004100010402     FEDTAB01L  IF   E           K DISK    usropn
004200000120      *---------------------------------------------------------------------*
004300000120      * Schiere
004400000120      *---------------------------------------------------------------------*
004500010516      * Termini di consegna
004600010516     D CTB             S              1    DIM(100)
004700010516     D TCO             S              3    DIM(100)
004800001002      *---------------------------------------------------------------------*
004900001002      * Comandi
005000050905     D CMD             S            100    DIM(25) CTDATA PERRCD(1)
005100000929      *---------------------------------------------------------------------*
005200001005      * Flag oprativi di work
005300050905     D tipo_File       S              1    INZ('1')
005400050905     D xPolonia        S              1    INZ(' ')
005500001005      *---------------------------------------------------------------------*
005600010402     D  MSGDS          S            100
005700001003     D  LENGH          S             15  5
005800001003      *
005900001003     D X               s              3  0 INZ(0)
006000001005     D Y               s              3  0 INZ(0)
006100010524      *
006200010524     D Conta1          s              4  0 INZ(0)
006300010524     D Conta2          s              4  0 INZ(0)
006400050905     D Conta3          s              4  0 INZ(0)
006500050905     D Conta4          s              4  0 INZ(0)
006600010612     D Last1           s              4  0 INZ(0)
006700010612     D Last2           s              4  0 INZ(0)
006800050905     D Last3           s              4  0 INZ(0)
006900050905     D Last4           s              4  0 INZ(0)
007000010612     D Tcolli1         s              9  0 INZ(0)
007100010612     D Tcolli2         s              9  0 INZ(0)
007200050905     D Tcolli3         s              9  0 INZ(0)
007300050905     D Tcolli4         s              9  0 INZ(0)
007400010612     D Tpeso1          s             16  3 INZ(0)
007500010612     D Tpeso2          s             16  3 INZ(0)
007600050905     D Tpeso3          s             16  3 INZ(0)
007700050905     D Tpeso4          s             16  3 INZ(0)
007800010524      *
007900010612      * *LDA Local Data Area per passare i totali da mettere in testata
008000010612     D AreaTot         DS
008100010612     D  Cnt1                   1      4  0 INZ(0)
008200010612     D  Cnt2                   5      8  0 INZ(0)
008300010612     D  Totcl1                 9     17  0 INZ(0)
008400010612     D  Totcl2                18     26  0 INZ(0)
008500010612     D  Totpe1                27     42  3 INZ(0)
008600010612     D  Totpe2                43     58  3 INZ(0)
008700050905     D  Filex                 59     59    INZ(' ')
008800050905     D  Cnt3                  61     64  0 INZ(0)
008900050905     D  Cnt4                  65     68  0 INZ(0)
009000050905     D  Totcl3                69     77  0 INZ(0)
009100050905     D  Totcl4                78     86  0 INZ(0)
009200050905     D  Totpe3                87    102  3 INZ(0)
009300050905     D  Totpe4               103    118  3 INZ(0)
009400050906     D CpysuFil1             104    104    INZ('N')
009500050906     D CpysuFil2             105    105    INZ('N')
009600050906     D CpysuFil3             106    106    INZ('N')
009700050906     D CpysuFil4             107    107    INZ('N')
009800000120      *---------------------------------------------------------------------*
009900000120      * DS
010000000120      *---------------------------------------------------------------------*
010100010518     D KPJBA         E DS
010200010516     D EDTD00DS      E DS
010300010516     D  D00                   77    142    DIM(3)
010400010516     D EDTD10DS      E DS
010500010516     D  D10                   73    186    DIM(3)
010600050905     D EDTD15DS      E DS
010700050905     D  D15                  445    500    DIM(2)
010800010516     D EDTD20DS      E DS
010900010516     D  W1496                 10     14
011000010516     D  W7224                 15     22
011100010516     D EDTD25DS      E DS
011200010730     D  WValore               22     37
011300010516      *  Ds per reperimento dati importi
011400010516     D WTD00           DS
011500010516     D  WTPI                   1      3
011600010516     D  WIMP                   4     19  3
011700010516     D  WVAL                  20     22
011800000120      *---------------------------------------------------------------------*
011900020916     D TIBS55DS      E DS
012000000120     D UTEDSE0F      E DS
012100000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
012200000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
012300000929      *---------------------------------------------------------------------*
012400000929      *                    .. 5 - 8  : tipo record letto
012500000929      *---------------------------------------------------------------------*
012600000929     D WDAT            DS           587
012700010524     D  SUMTPR                 5      8
012800000929     D  SUMINR                 5      6
012900010516      *---------------------------------------------------------------------*
013000010516     D PRACHT          C                   CONST('PRACHT                       -
013100010516     D                                           ')
013200010517     D DIGITS          C                   CONST('0123456789')
013300000929     I*---------------------------------------------------------------------*
013400001002     I* FILE ridenominazione records
013500000929     I*---------------------------------------------------------------------*
013600001003     IINPUT000
013700000929     I              RRIFCSUM                    SUMDAT
013800001003     IWORKF000
013900001002     I              RRIFCSUM                    WRKDAT
014000010524     IIFCSUM01
014100010524     I              RRIFCSUM                    SUMDAT1
014200010524     IIFCSUM02
014300010524     I              RRIFCSUM                    SUMDAT2
014400050905     IIFCSUM03
014500050905     I              RRIFCSUM                    SUMDAT3
014600050905     IIFCSUM04
014700050905     I              RRIFCSUM                    SUMDAT4
014800000120      *---------------------------------------------------------------------*
014900000120      * Ciclo principale
015000000120      *---------------------------------------------------------------------*
015100001002      * Lettura iniziale sul file
015200010517      *               *----------------------------*
015300010517     C                   EXSR      Legge_rec
015400010517      *               *----------------------------*
015500960329      * Lettura fino a fine file
015600010517      *---------------
015700010514     C                   DOW       not %EOF(EDIFCSUM)
015800010516      *
015900010516      * Routine che esegue per controllare le singole spedizioni
016000010516      *   in cui testare i records di dettaglio messi nelle DS.
016100010516      *               *----------------------*
016200050905     C                   EXSR      trova_tipoF
016300010516      *               *----------------------*
016400010516      *
016500001002      *  Copia il record da INPUT su WORKFile
016600010524     C                   If        SUMINR ='TD'
016700001003     C                   eval      WRKdat = SUMdat
016800001003     C                   WRITE     WORKF000
016900010524     C                   End
017000010517      * Lettura sul file
017100010517      *               *----------------------------*
017200010517     C                   EXSR      Legge_rec
017300010517      *               *----------------------------*
017400010517      *
017500010517      * A rottura dettaglio quindi a nuovo TD00 oppure a fine file
017600050905      *  copia il blocco di dettaglio in base alle condizioni trovate poi
017700050905      *   pulisce il file di parcheggio per prepararlo ad un nuovo ciclo.
017800010517     C                   If        not %EOF(EDIFCSUM) and SUMTPR = 'TD00' or
017900010517     C                                 %EOF(EDIFCSUM)
018000010517      *               *----------------------------*
018100010517     C                   EXSR      Cpy_Clr_SUM0
018200010517      *               *----------------------------*
018300050905     c                   eval      xPolonia = *blank
018400010517     C                   ENDif
018500010516      *
018600001002     C                   ENDdo
018700010517      *---------------
018800050905      *  Alla fine rinumera sempre i dettagli generati e calcola i totali
018900010524      *               *----------------------------*
019000010524     C                   EXSR      Rinumera
019100010524      *               *----------------------------*
019200010514      *
019300960329     C                   SETON                                        LR
019400001002     C*----------------------------------------------------------------
019500010517     C*? Lettura del Flat File
019600001002     C*----------------------------------------------------------------
019700010517     C     Legge_Rec     BEGSR
019800010516      *
019900010517      * Lettura successiva
020000010517      *  muove il record su una DS di controllo
020100010517     C                   READ      EDIFCSUM
020200010517     C                   If        not %EOF(EDIFCSUM)
020300010517      *                          ++++++++++++++++++++++++
020400010517     C                   MOVEL     SUMDAT        WDAT
020500010517      *                          ++++++++++++++++++++++++
020600010517     C                   End
020700010517      *
020800010517     C                   ENDSR
020900010517     C*----------------------------------------------------------------
021000050905     C*? Dettaglio x ricercare in base alla regola quale file scrivere
021100010517     C*----------------------------------------------------------------
021200050905     C     trova_tipoF   BEGSR
021300050906      *
021400050905      *?  Adesso determina solo se 1 o 2 e se si tratta di Spedizione in POLONIA.
021500050905      *   ossia
021600050905      *  separa il dettaglio seguendo la 1 o la 2 regola e tiene presente
021700050905      *   se la nazione del Destinatario è la Polonia
021800050905      *    (In tal caso la regola 1 diventerà 3 e la 2 diventerà 4 nelle routine
021900050905      *     successive.)
022000050905      *
022100010516     C                   SELECT
022200050905      *
022300050905      * ?Lo esegue sempre x determinare la Nazione del Destinatario
022400050905      * Dettaglio TD15 = Segmento NAD (se --> Polonia)
022500050905      *  >>>>>>>>>>>>>>
022600050905     C                   When      SUMTPR = 'TD15'
022700050905      *                *------------------*
022800050905     C                   EXSR      DRED15
022900050905      *                *------------------*
023000010516      *
023100050905      * ?Lo esegue sempre perchè è l'inizio del dettaglio
023200010516      * Dettaglio TD00 = Segmento MOA (Monetary Amount)
023300010516      *  >>>>>>>>>>>>>>
023400010516     C                   When      SUMTPR = 'TD00'
023500010516      * Per poter eseguire correttamente il ragionamento, occorre impostare
023600050905      *  subito a (1) il flag per la "Regola"/tipo file .
023700050905      *  Tutte le condizioni successive sono in END in modo che:
023800050905      *   se almeno una delle condizioni è negata imposta a (2) il Flag e non
023900050905      *    esegue le successive.
024000050905     C                   Eval      tipo_File = '1'
024100010516      *
024200010516      *                *------------------*
024300010516     C                   EXSR      DRED00
024400010516      *                *------------------*
024500050905      *
024600010516      * Dettaglio TD10 = Segmento TOD (Terms of Delivery)
024700010516      *  >>>>>>>>>>>>>>
024800050905     C                   When      SUMTPR = 'TD10' and tipo_File = '1'
024900010516      *
025000010516      *                *------------------*
025100010516     C                   EXSR      DRED10
025200010516      *                *------------------*
025300010516      *
025400010516      * Dettaglio TD20 = Segmento GID (Goods Item Details)
025500010516      *  >>>>>>>>>>>>>>
025600050905     C                   When      SUMTPR = 'TD20' and tipo_File = '1'
025700010516      *
025800010516      *                *------------------*
025900010516     C                   EXSR      DRED20
026000010516      *                *------------------*
026100010516      *
026200010516      * Dettaglio TD25 = Segmento MEA (Measurements)
026300010516      *  >>>>>>>>>>>>>>
026400050905     C                   When      SUMTPR = 'TD25' and tipo_File = '1'
026500010516      *
026600010516      *                *------------------*
026700010516     C                   EXSR      DRED25
026800010516      *                *------------------*
026900010516      *
027000010516     C                   ENDsl
027100050905      *
027200010518     C                   ENDSR
027300050905     C*----------------------------------------------------------------
027400050905     C*? DRED15 - DECODIFICA TIPO RECORD TD15
027500050905     C*----------------------------------------------------------------
027600050905     C     DRED15        BEGSR
027700050905      *
027800050905     C                   CLEAR                   EDTD15DS
027900050905     C                   MOVEL     SUMDAT        EDTD15DS
028000050905      *
028100050905      *  Destinatario in POLONIA -->imposta e memorizza il flag
028200050905     c                   if        DD3035 = 'CN '
028300050905     c                   if        DD3207 = 'PL '                               <-- Nazione
028400050905     c                   eval      xPolonia = 'S'
028500050905     c                   end
028600050905     c                   end
028700050905      *
028800050905     C                   ENDSR
028900010516     C*----------------------------------------------------------------
029000010516     C*? DRED00 - DECODIFICA TIPO RECORD TD00
029100010516     C*----------------------------------------------------------------
029200010516     C     DRED00        BEGSR
029300010516     C*
029400010516      * Pulisco ds impostazione dati
029500010516     C                   CLEAR                   EDTD00DS
029600010516     C                   MOVEL     SUMDAT        EDTD00DS
029700010516      *
029800010516      * se c'è un "MOntary Amount" = Contrassegno deve stare nel EDIFCSUM2
029900010516     C                   DO        3             X
030000010516     C* Controllo se campo importo numerico
030100010516     C     D00(X)        IFNE      *BLANKS
030200010516     C                   MOVEL     D00(X)        WTD00
030300010516     C                   MOVEL     D00(X)        WCOM19           19
030400010516     C                   MOVE      WCOM19        WCOM16           16
030500010517      *
030600010517     C     Digits        CHECK     WCOM16
030700010517      *
030800010516     C*  Se importo numerico decodifico tipo
030900010517     C                   IF        not %Found and WTPI = '22 ' and
031000010516     C                             WIMP > 0
031100010516      *                          ++++++++++++++++
031200050905     C                   Eval      tipo_File = '2'
031300010516      *                          ++++++++++++++++
031400010516     C                   EndIF
031500010516     C                   EndIF
031600010516     C*
031700010516     C                   END                                                    Do 3
031800001002      *
031900001003     C                   ENDSR
032000000125      *----------------------------------------------------------------
032100000125      *? DRED10 - DECODIFICA TIPO RECORD TD10
032200000125      *----------------------------------------------------------------
032300960527     C     DRED10        BEGSR
032400000125      *
032500001002      * Pulisco ds impostazione dati
032600001002     C                   CLEAR                   EDTD10DS
032700001003     C                   MOVEL     SUMDAT        EDTD10DS
032800010516      *
032900010516      *  Terms Of Delivery = Porto Franco o Assegnato
033000010516     C                   if        DC4055 = '5  '  and  DC1131 = 'EEL'
033100010524     C                   z-add     1             X
033200010524     C     DC4053        LOOKUP    TCO(X)                                 32
033300010516      *
033400010516      *  Tramite la tabella "TB" può avere assegnati o franchi oppure se
033500010516      *   non l'ha trovato deve stare nel EDIFCSUM2
033600010516     C                   IF        CTB(X) = 'A' and *IN32 = *On
033700010516     C                                           or *IN32 = *Off
033800010516      *                          ++++++++++++++++
033900050905     C                   Eval      tipo_File = '2'
034000010516      *                          ++++++++++++++++
034100010516     C                   END
034200010516      *
034300010516     C                   EndIF
034400010516      *
034500960527     C                   ENDSR
034600001003     C*----------------------------------------------------------------
034700010516     C*? DRED20 - DECODIFICA TIPO RECORD TD20
034800001003     C*----------------------------------------------------------------
034900010516     C     DRED20        BEGSR
035000001003      *
035100010516     C                   CLEAR                   EDTD20DS
035200010516     C                   MOVEL     SUMDAT        EDTD20DS
035300001003      *
035400010516      * Nei Goods Item Details
035500010516      *  se ci sono più colli deve stare nel EDIFCSUM2
035600010516     C*  Controllo validità campi numerici
035700010516     C                   CLEAR                   WS1496            5 0
035800010516     C                   CLEAR                   WS7224            8 0
035900010516      *
036000010517     C     Digits        CHECK     W1496
036100010516      *
036200010517     C                   IF        not %Found
036300010516     C                   move      W1496         WS1496
036400010516      *
036500010516     C                   IF        WS1496 = 99999
036600010517     C     Digits        CHECK     W7224
036700010516      *  se più di 1 collo
036800010517     C                   IF        not %Found
036900010516     C                   move      W7224         WS7224
037000010516      *
037100010516     C                   IF        WS7224 > 1
037200010516      *                          ++++++++++++++++
037300050905     C                   Eval      tipo_File = '2'
037400010516      *                          ++++++++++++++++
037500010516     C                   ENDif
037600010516      *
037700010516     C                   ENDif
037800010516     C                   ENDif
037900010516     C                   ENDif
038000010517      *
038100001003     C                   ENDSR
038200010516     C*----------------------------------------------------------------
038300010516     C*? DRED25 - DECODIFICA TIPO RECORD TD25
038400010516     C*----------------------------------------------------------------
038500010516     C     DRED25        BEGSR
038600010516      *
038700010516     C                   CLEAR                   EDTD25DS
038800010516     C                   MOVEL     SUMDAT        EDTD25DS
038900010516      *
039000010727      * Nei MEAsuraments se sono più di 30 Kg. deve stare nel EDIFCSUM2
039100010516     C*  Controllo validità campi numerici
039200010730     C     Digits        CHECK     WValore
039300010730     C*  Peso/Volume Controllo da TD20 di totale
039400010730     C                   IF        not %Found and WS1496 = 99999
039500010730     C*  Peso  superiore a 30 Kg.
039600010730     C*   e Volume  superiore a 0,15 mt.cubi
039700010730     C                   IF        DF6314 > 30 and
039800010730     C                             DF6311 = 'WT ' and DF6411 = 'KGM' or
039900010730     C                             DF6314 > 0,15 and
040000010730     C                             DF6311 = 'VOL' and DF6411 = 'MTQ'
040100010516      *                          ++++++++++++++++
040200050905     C                   Eval      tipo_File = '2'
040300010516      *                          ++++++++++++++++
040400010516     C                   ENDif
040500010516      *
040600010516     C                   END
040700010516      *
040800010516     C                   ENDSR
040900001003     C*-------------------------------------------------------------------------------------
041000010517     C*?    SPOSTA I Records dal WRKFile ai Files separati
041100001003     C*-------------------------------------------------------------------------------------
041200010517     C     Cpy_Clr_SUM0  BEGSR
041300050905      *
041400050905      *  Imposta i flags di copia generali
041500050905      *   tenendo conto se files x Polonia
041600050905     c                   if        xPolonia = *blank
041700050905      *
041800050905     C                   If           tipo_File = '1'
041900050905     C                   eval      CpysuFil1 = 'S'
042000050905     C                   ENDif
042100050905      *
042200050905     C                   If           tipo_File = '2'
042300050905     C                   eval      CpysuFil2 = 'S'
042400050905     C                   ENDif
042500010516      *
042600050905     c                   else
042700050905      *
042800050905      * se la destinazione è POLONIA il Tipo File è differente:
042900050905      *     1-->3  2-->4
043000050905     C                   If           tipo_File = '1'
043100050905     c                   eval      tipo_File = '3'
043200050905     C                   eval      CpysuFil3 = 'S'
043300050905     C                   ENDif
043400050905      *
043500050905     C                   If           tipo_File = '2'
043600050905     c                   eval      tipo_File = '4'
043700050905     C                   eval      CpysuFil4 = 'S'
043800050905     C                   ENDif
043900050905      *
044000050905     c                   end
044100050905      *
044200010517      *  Chiude il file di work  >>>>>> esegue il CPYF <<<<<< quindi ripulisce
044300010517      *   il WorkFile per prepararlo a ricevere nuovo pacchetto di righe.
044400010517      *
044500010517      * chiude prima di copiarlo e ripulirlo
044600010517     C                   CLOSE     EDIFCSUM0
044700001003      *
044800050905      * copia su File 1 monocollo =/< 30kg.     NO Polonia
044900050905     C                   if        tipo_File = '1'
045000050906     C                   MOVEL     CMD(18)       MSGDS
045100010402     C                   Z-ADD     100           LENGH
045200001003     C                   CALL      'QCMDEXC'
045300001003     C                   PARM                    MSGDS
045400001003     C                   PARM                    LENGH
045500001003     C                   EndIF
045600001003      *
045700050905      * copia su File 2                         NO Polonia
045800050905     C                   if        tipo_File = '2'
045900050906     C                   MOVEL     CMD(19)       MSGDS
046000010402     C                   Z-ADD     100           LENGH
046100001003     C                   CALL      'QCMDEXC'
046200001003     C                   PARM                    MSGDS
046300001003     C                   PARM                    LENGH
046400001003     C                   EndIF
046500050905      *
046600050905      * copia su File 3 Polonia monocollo =/< 30kg.
046700050905     C                   if        tipo_File = '3'
046800050905     C                   MOVEL     CMD(20)       MSGDS
046900050905     C                   Z-ADD     100           LENGH
047000050905     C                   CALL      'QCMDEXC'
047100050905     C                   PARM                    MSGDS
047200050905     C                   PARM                    LENGH
047300050905     C                   EndIF
047400001003      *
047500050905      * copia su File 4 Polonia
047600050905     C                   if        tipo_File = '4'
047700050905     C                   MOVEL     CMD(21)       MSGDS
047800050905     C                   Z-ADD     100           LENGH
047900050905     C                   CALL      'QCMDEXC'
048000050905     C                   PARM                    MSGDS
048100050905     C                   PARM                    LENGH
048200050905     C                   EndIF
048300050905      *
048400050906      * pulizia del workfile EDIFCSUM0 dopo aver copiato la porzione
048500050906      *  di dettaglio nel file 1/2/3/4 secondo le "Regole"
048600050906     C                   MOVEL     CMD(13)       MSGDS
048700010402     C                   Z-ADD     100           LENGH
048800001003     C                   CALL      'QCMDEXC'
048900001003     C                   PARM                    MSGDS
049000001003     C                   PARM                    LENGH
049100010518      *
049200001003      *  riapre dopo la pulizia
049300010514     C                   OPEN      EDIFCSUM0
049400001005      *
049500001005     C                   Endsr
049600001003      *----------------------------------------------------------------
049700010524      *? Rinumera - Deve riordinare le spedizioni nel dettaglio
049800010524      *----------------------------------------------------------------
049900010524     C     Rinumera      BEGSR
050000010524      *
050100010524      *  File (1)
050200010524     C                   OPEN      EDIFCSUM1
050300010524     C                   READ      EDIFCSUM1
050400010524     C                   DOW       not %EOF(EDIFCSUM1)
050500010524     C                   MOVEL     SUMDAT1       WDAT
050600010612      *
050700010612      * reimposta la numerazione delle spedizioni
050800010524     C                   IF        SUMTPR = 'TD00'
050900010524     C                   CLEAR                   EDTD00DS
051000010524     C                   MOVEL     SUMDAT1       EDTD00DS
051100010524     C                   Eval      Conta1 = Conta1 + 1
051200010524     C                   Eval      DA1490 = Conta1
051300010524     C                   MOVEL     EDTD00DS      SUMDAT1
051400010524     C                   Update    IFCSUM01
051500010524     C                   ENDIF
051600010612      *
051700010612      * calcola i totali nr.colli
051800010612     C                   IF        SUMTPR = 'TD20'
051900010612     C                   CLEAR                   EDTD20DS
052000010612     C                   MOVEL     SUMDAT1       EDTD20DS
052100010612     C                   If        DE1496 = 99999
052200010612     C                   eval      Tcolli1 = Tcolli1 + DE7224
052300010612     C                   End
052400010612     C                   ENDIF
052500010612      *
052600010612      * calcola i totali di peso
052700010612     C                   IF        SUMTPR = 'TD25'
052800010612     C                   CLEAR                   EDTD25DS
052900010612     C                   MOVEL     SUMDAT1       EDTD25DS
053000010612     C                   If        DF6311 = 'WT ' and DF6411 = 'KGM'
053100010612     C                   eval      Tpeso1 = Tpeso1 + DF6314
053200010612     C                   End
053300010612     C                   ENDIF
053400010612      *
053500010524     C                   READ      EDIFCSUM1
053600010524     C                   ENDDO
053700010612     C                   Eval      Last1  = Conta1
053800010524     C                   CLOSE     EDIFCSUM1
053900010524      *
054000010524      *  File (2)
054100010524     C                   OPEN      EDIFCSUM2
054200010524     C                   READ      EDIFCSUM2
054300010524     C                   DOW       not %EOF(EDIFCSUM2)
054400010524     C                   MOVEL     SUMDAT2       WDAT
054500010612      *
054600010612      * reimposta la numerazione delle spedizioni
054700010524     C                   IF        SUMTPR = 'TD00'
054800010524     C                   CLEAR                   EDTD00DS
054900010524     C                   MOVEL     SUMDAT2       EDTD00DS
055000010524     C                   Eval      Conta2 = Conta2 + 1
055100010524     C                   Eval      DA1490 = Conta2
055200010524     C                   MOVEL     EDTD00DS      SUMDAT2
055300010524     C                   Update    IFCSUM02
055400010524     C                   ENDIF
055500050906      *
055600050906      * calcola i totali nr.colli
055700050906     C                   IF        SUMTPR = 'TD20'
055800050906     C                   CLEAR                   EDTD20DS
055900050906     C                   MOVEL     SUMDAT2       EDTD20DS
056000050906     C                   If        DE1496 = 99999
056100050906     C                   eval      Tcolli2 = Tcolli2 + DE7224
056200050906     C                   End
056300050906     C                   ENDIF
056400050906      *
056500050906      * calcola i totali di peso
056600050906     C                   IF        SUMTPR = 'TD25'
056700050906     C                   CLEAR                   EDTD25DS
056800050906     C                   MOVEL     SUMDAT2       EDTD25DS
056900050906     C                   If        DF6311 = 'WT ' and DF6411 = 'KGM'
057000050906     C                   eval      Tpeso2 = Tpeso2 + DF6314
057100050906     C                   End
057200050906     C                   ENDIF
057300050906      *
057400050906     C                   READ      EDIFCSUM2
057500050906     C                   ENDDO
057600050906     C                   Eval      Last2  = Conta2
057700050906     C                   CLOSE     EDIFCSUM2
057800050906      *
057900050905      *  File (3)
058000050905     C                   OPEN      EDIFCSUM3
058100050905     C                   READ      EDIFCSUM3
058200050905     C                   DOW       not %EOF(EDIFCSUM3)
058300050905     C                   MOVEL     SUMDAT3       WDAT
058400050905      *
058500050905      * reimposta la numerazione delle spedizioni
058600050905     C                   IF        SUMTPR = 'TD00'
058700050905     C                   CLEAR                   EDTD00DS
058800050905     C                   MOVEL     SUMDAT3       EDTD00DS
058900050905     C                   Eval      Conta3 = Conta3 + 1
059000050905     C                   Eval      DA1490 = Conta3
059100050905     C                   MOVEL     EDTD00DS      SUMDAT3
059200050905     C                   Update    IFCSUM03
059300050905     C                   ENDIF
059400050905      *
059500050905      * calcola i totali nr.colli
059600050905     C                   IF        SUMTPR = 'TD20'
059700050905     C                   CLEAR                   EDTD20DS
059800050905     C                   MOVEL     SUMDAT3       EDTD20DS
059900050905     C                   If        DE1496 = 99999
060000050905     C                   eval      Tcolli3 = Tcolli3 + DE7224
060100050905     C                   End
060200050905     C                   ENDIF
060300050905      *
060400050905      * calcola i totali di peso
060500050905     C                   IF        SUMTPR = 'TD25'
060600050905     C                   CLEAR                   EDTD25DS
060700050905     C                   MOVEL     SUMDAT3       EDTD25DS
060800050905     C                   If        DF6311 = 'WT ' and DF6411 = 'KGM'
060900050905     C                   eval      Tpeso3 = Tpeso3 + DF6314
061000050905     C                   End
061100050905     C                   ENDIF
061200050905      *
061300050905     C                   READ      EDIFCSUM3
061400050905     C                   ENDDO
061500050905     C                   Eval      Last3  = Conta3
061600050905     C                   CLOSE     EDIFCSUM1
061700050905      *
061800050905      *  File (4)
061900050905     C                   OPEN      EDIFCSUM4
062000050905     C                   READ      EDIFCSUM4
062100050905     C                   DOW       not %EOF(EDIFCSUM4)
062200050905     C                   MOVEL     SUMDAT4       WDAT
062300050905      *
062400050905      * reimposta la numerazione delle spedizioni
062500050905     C                   IF        SUMTPR = 'TD00'
062600050905     C                   CLEAR                   EDTD00DS
062700050905     C                   MOVEL     SUMDAT4       EDTD00DS
062800050905     C                   Eval      Conta4 = Conta4 + 1
062900050905     C                   Eval      DA1490 = Conta4
063000050905     C                   MOVEL     EDTD00DS      SUMDAT4
063100050905     C                   Update    IFCSUM04
063200050905     C                   ENDIF
063300050905      *
063400010612      * calcola i totali nr.colli
063500010612     C                   IF        SUMTPR = 'TD20'
063600010612     C                   CLEAR                   EDTD20DS
063700050905     C                   MOVEL     SUMDAT4       EDTD20DS
063800010612     C                   If        DE1496 = 99999
063900050905     C                   eval      Tcolli4 = Tcolli4 + DE7224
064000010612     C                   End
064100010612     C                   ENDIF
064200010612      *
064300010612      * calcola i totali di peso
064400010612     C                   IF        SUMTPR = 'TD25'
064500010612     C                   CLEAR                   EDTD25DS
064600050905     C                   MOVEL     SUMDAT4       EDTD25DS
064700010612     C                   If        DF6311 = 'WT ' and DF6411 = 'KGM'
064800050905     C                   eval      Tpeso4 = Tpeso4 + DF6314
064900010612     C                   End
065000010612     C                   ENDIF
065100010612      *
065200050905     C                   READ      EDIFCSUM4
065300010524     C                   ENDDO
065400050905     C                   Eval      Last4  = Conta4
065500050905     C                   CLOSE     EDIFCSUM4
065600010524      *
065700010612      *  alla fine del giro si hanno i contatori di totale pronti con
065800010612      *  i dati impostati per la testata dei 2 IFCSUM sono inseriti in
065900010612      *  una Local Data Area da passare ai programmi successivi.
066000010612     C                   IN        AreaTot
066100010612     C                   move      Conta1        Cnt1
066200010612     C                   move      Conta2        Cnt2
066300010612     C                   move      TColli1       Totcl1
066400010612     C                   move      TColli2       Totcl2
066500010612     C                   move      TPeso1        Totpe1
066600010612     C                   move      TPeso2        Totpe2
066700050905     C                   move      Conta3        Cnt3
066800050905     C                   move      Conta4        Cnt4
066900050905     C                   move      TColli3       Totcl3
067000050905     C                   move      TColli4       Totcl4
067100050905     C                   move      TPeso3        Totpe3
067200050905     C                   move      TPeso4        Totpe4
067300050906      * FileX --> viene reimpostato dal CLLE dopo chiusura di questo pgm
067400050906     C                   move      ' '           FileX
067500010612     C                   OUT       AreaTot
067600010612      *
067700010524     C                   Endsr
067800010524      *----------------------------------------------------------------
067900010524      *? *INZSR - OPERAZIONI INIZIALI
068000010524      *----------------------------------------------------------------
068100010524     C     *INZSR        BEGSR
068200010524      *
068300010514     C* Riceve un parametro per capire se si trova in ambiente di produzione
068400010514     C*  o in ambiente di test
068500010514     C     *entry        plist
068600010514     C                   PARM                    KPJBA
068700010612     C*
068800010612     C     *DTAARA       DEFINE    *LDA          AreaTot
068900010612     C*
069000010514      * DETERMINO SE SONO BARTOLINI O SDI e se in prova
069100020916     C                   CLEAR                   TIBS55DS
069200001003     C                   MOVEL     'L'           I50TLA
069300020916     C                   CALL      'TIBS55R'
069400020916     C                   PARM                    TIBS55DS
069500001003      *
069600001003      * RECUPERO DATI DELL'UTENTE
069700001003     C                   Z-ADD     1             CODUT
069800001003     C                   CALL      'XPARUT'
069900001003     C                   PARM                    UTEDSE0F
070000001003      *
070100010518      * ------------
070200050905      *  Copia in QTEMP i files di work EDIFCSUM0/1/2 /3/4
070300050905      *
070400050905      * ------------               x test con KPJBU vuota
070500050905     c                   if        kpjbu = *blank
070600050905     c                   exsr      cmd_test
070700050905     c                   endIF
070800050905      *
070900010514      * Punta alle Tabelle in Bartolini prova o produzione
071000010402     C                   clear                   MSGDS
071100050906     C                   MOVEL     CMD(01)       MSGDS
071200010514      *
071300010402     C                   Z-ADD     100           LENGH
071400010402     C                   CALL      'QCMDEXC'
071500010402     C                   PARM                    MSGDS
071600010402     C                   PARM                    LENGH
071700010516      *
071800010517     C                   OPEN      EDTAB01L
071900010517      *
072000010516     C* Caricamento Tabella termini consegna solo da Bartolini
072100010516     C                   Z-ADD     0             Y
072200010516     C                   MOVEL     'TB'          TABCOD
072300010516     C     TABCOD        SETLL     EDTAB01L
072400010516     C     TABCOD        READE     EDTAB01L
072500010516     C                   DOW       not %EOF(EDTAB01L)
072600010516     C     TABFLG        IfEQ      *BLANKS
072700070220     c                   move      tabkey        due_alfa          2
072800070220     c                   if        due_alfa = *blank
072900010516     C                   ADD       1             Y
073000010516     C                   MOVEL     TABKEY        TCO(Y)
073100010516     C                   MOVEL     TABUNI        CTB(Y)
073200070220     C                   end
073300010516     C                   end
073400010516     C     TABCOD        READE     EDTAB01L
073500010516     C                   END
073600010514     C*
073700010402     C                   CLOSE     EDTAB01L
073800010402      * Toglie l'OVRRIDE
073900010402     C                   clear                   MSGDS
074000050906     C                   MOVEL     CMD(02)       MSGDS                          (9)
074100010402     C                   Z-ADD     100           LENGH
074200010402     C                   CALL      'QCMDEXC'
074300010402     C                   PARM                    MSGDS
074400010402     C                   PARM                    LENGH
074500010402     C*
074600001003      * Apertura File UC e lettura estemporanea
074700010514     C                   OPEN      EDIFCSUM0
074800010514     C                   READ      EDIFCSUM0
074900010514     C*
075000001003     C                   ENDSR
075100050905     C*
075200050905      *****************************************************************
075300050905     C*    comandi x testare la procedura
075400050905      *****************************************************************
075500050905     C     cmd_test      BEGSR
075600050905      * ------------               x test con KPJBU vuota
075700050905      *
075800050905      *    in procedura il CLP TRTC83C1  già esegue questi  comandi
075900050905      *      serve farli qui dentro se si vuole provare il programma
076000050905      *        staccato da tutto.
076100050905     C                   clear                   MSGDS
076200050906     C                   MOVEL     CMD(03)       MSGDS
076300050905     C                   Z-ADD     100           LENGH
076400050905     C                   CALL      'QCMDEXC'                            99
076500050905     C                   PARM                    MSGDS
076600050905     C                   PARM                    LENGH
076700050905      *
076800050905     C                   clear                   MSGDS
076900050906     C                   MOVEL     CMD(04)       MSGDS
077000050905     C                   Z-ADD     100           LENGH
077100050905     C                   CALL      'QCMDEXC'                            99
077200050905     C                   PARM                    MSGDS
077300050905     C                   PARM                    LENGH
077400050905      *
077500050905     C                   clear                   MSGDS
077600050906     C                   MOVEL     CMD(05)       MSGDS
077700050905     C                   Z-ADD     100           LENGH
077800050905     C                   CALL      'QCMDEXC'                            99
077900050905     C                   PARM                    MSGDS
078000050905     C                   PARM                    LENGH
078100050905      *
078200050905     C                   clear                   MSGDS
078300050906     C                   MOVEL     CMD(06)       MSGDS
078400050905     C                   Z-ADD     100           LENGH
078500050906     C                   CALL      'QCMDEXC'                            99
078600050905     C                   PARM                    MSGDS
078700050905     C                   PARM                    LENGH
078800050905      *
078900050905     C                   clear                   MSGDS
079000050906     C                   MOVEL     CMD(07)       MSGDS
079100050905     C                   Z-ADD     100           LENGH
079200050906     C                   CALL      'QCMDEXC'                            99
079300050905     C                   PARM                    MSGDS
079400050905     C                   PARM                    LENGH
079500050905      *
079600050906      * Esegue OVRDBF in QTEMP per non sovrapporsi ad altri Manifest
079700050905     C                   clear                   MSGDS
079800050906     C                   MOVEL     CMD(08)       MSGDS
079900050905     C                   Z-ADD     100           LENGH
080000050905     C                   CALL      'QCMDEXC'
080100050905     C                   PARM                    MSGDS
080200050905     C                   PARM                    LENGH
080300050905      *
080400050906     C                   MOVEL     CMD(09)       MSGDS
080500050905     C                   Z-ADD     100           LENGH
080600050905     C                   CALL      'QCMDEXC'
080700050905     C                   PARM                    MSGDS
080800050905     C                   PARM                    LENGH
080900050905      *
081000050906     C                   MOVEL     CMD(10)       MSGDS
081100050905     C                   Z-ADD     100           LENGH
081200050905     C                   CALL      'QCMDEXC'
081300050905     C                   PARM                    MSGDS
081400050905     C                   PARM                    LENGH
081500050905      *
081600050906     C                   MOVEL     CMD(11)       MSGDS
081700050905     C                   Z-ADD     100           LENGH
081800050905     C                   CALL      'QCMDEXC'
081900050905     C                   PARM                    MSGDS
082000050905     C                   PARM                    LENGH
082100050905      *
082200050906     C                   MOVEL     CMD(12)       MSGDS
082300050905     C                   Z-ADD     100           LENGH
082400050905     C                   CALL      'QCMDEXC'
082500050905     C                   PARM                    MSGDS
082600050905     C                   PARM                    LENGH
082700050906      * ------------
082800050905      *
082900050906      * Pulizia dei workfiles EDIFCSUM0/1/2/3/4
083000050906     C                   MOVEL     CMD(13)       MSGDS
083100050905     C                   Z-ADD     100           LENGH
083200050905     C                   CALL      'QCMDEXC'
083300050905     C                   PARM                    MSGDS
083400050905     C                   PARM                    LENGH
083500050905      *
083600050906     C                   MOVEL     CMD(14)       MSGDS
083700050905     C                   Z-ADD     100           LENGH
083800050905     C                   CALL      'QCMDEXC'
083900050905     C                   PARM                    MSGDS
084000050905     C                   PARM                    LENGH
084100050905      *
084200050906     C                   MOVEL     CMD(15)       MSGDS
084300050905     C                   Z-ADD     100           LENGH
084400050905     C                   CALL      'QCMDEXC'
084500050905     C                   PARM                    MSGDS
084600050905     C                   PARM                    LENGH
084700050905      *
084800050906     C                   MOVEL     CMD(16)       MSGDS
084900050905     C                   Z-ADD     100           LENGH
085000050905     C                   CALL      'QCMDEXC'
085100050905     C                   PARM                    MSGDS
085200050905     C                   PARM                    LENGH
085300050905      *
085400050906     C                   MOVEL     CMD(17)       MSGDS
085500050905     C                   Z-ADD     100           LENGH
085600050905     C                   CALL      'QCMDEXC'
085700050905     C                   PARM                    MSGDS
085800050905     C                   PARM                    LENGH
085900050905      *
086000050905     C                   ENDSR
086100001002      *****************************************************************
086200001005** (CMD)
086300050906OVRDBF FILE(EDTAB01L) TOFILE(*LIBL/EDTAB01L)
086400050906DLTOVR FILE(EDTAB01L)
086500050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM0) MBROPT(*REPLACE) CRTFILE(*YES)
086600050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM1) MBROPT(*REPLACE) CRTFILE(*YES)
086700050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM2) MBROPT(*REPLACE) CRTFILE(*YES)
086800050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM3) MBROPT(*REPLACE) CRTFILE(*YES)
086900050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM4) MBROPT(*REPLACE) CRTFILE(*YES)
087000050906OVRDBF FILE(EDIFCSUM0) TOFILE(QTEMP/EDIFCSUM0)
087100050906OVRDBF FILE(EDIFCSUM1) TOFILE(QTEMP/EDIFCSUM1)
087200050906OVRDBF FILE(EDIFCSUM2) TOFILE(QTEMP/EDIFCSUM2)
087300050906OVRDBF FILE(EDIFCSUM3) TOFILE(QTEMP/EDIFCSUM3)
087400050906OVRDBF FILE(EDIFCSUM4) TOFILE(QTEMP/EDIFCSUM4)
087500050906CLRPFM QTEMP/EDIFCSUM0
087600050906CLRPFM QTEMP/EDIFCSUM1
087700050906CLRPFM QTEMP/EDIFCSUM2
087800050906CLRPFM QTEMP/EDIFCSUM3
087900050906CLRPFM QTEMP/EDIFCSUM4
088000050906CPYF FROMFILE(EDIFCSUM0) TOFILE(EDIFCSUM1) MBROPT(*ADD) FMTOPT(*NOCHK)
088100050906CPYF FROMFILE(EDIFCSUM0) TOFILE(EDIFCSUM2) MBROPT(*ADD) FMTOPT(*NOCHK)
088200050905CPYF FROMFILE(EDIFCSUM0) TOFILE(EDIFCSUM3) MBROPT(*ADD) FMTOPT(*NOCHK)
088300050905CPYF FROMFILE(EDIFCSUM0) TOFILE(EDIFCSUM4) MBROPT(*ADD) FMTOPT(*NOCHK)
088400050906
088500050906
088600050905
088700050905
