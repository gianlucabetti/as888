000100001003     H DECEDIT('0,') DATEDIT(*DMY.)
000200000929      ***********************************************************************
000300001010      *                                                                     *
000400010514      *   Il pgm serve a dividere in 2 i Flat Files  - IFCSUM -             *
000500110415      *   della AGILITY durante la scrittura del Manifest                   *
000600110415      *   poichè vuole 2 IFCSUM separati x Danimarca e Finlandia.           *
000700050905      *                                                                     *
000800021009      *   INPUT   -> EDIFCSUM                                               *
000900021009      *   WORKF   -> EDIFCSUM0                                              *
001000010514      *                                                                     *
001100081210      *   OUTPUT  -> EDIFCSUM1                                              *
001200110415      *   OUTPUT  -> EDIFCSUM2                                FINLAND       *
001300050905      *                                                                     *
001400000929      ***********************************************************************
001500010514     FEDIFCSUM  IF   E             DISK    RENAME(RRIFCSUM:INPUT000)
001600000929     F*---------
001700010514     FEDIFCSUM0 IF A E             DISK    usropn
001800000929     F                                     RENAME(RRIFCSUM:WORKF000)
001900010524     F*---------
002000010524     FEDIFCSUM1 UF   E             DISK    usropn
002100010524     F                                     RENAME(RRIFCSUM:IFCSUM01)
002200010524     F*---------
002300010524     FEDIFCSUM2 UF   E             DISK    usropn
002400010524     F                                     RENAME(RRIFCSUM:IFCSUM02)
002500000120      *---------------------------------------------------------------------*
002600000120      * Schiere
002700000120      *---------------------------------------------------------------------*
002800001002      * Comandi
002900050905     D CMD             S            100    DIM(25) CTDATA PERRCD(1)
003000000929      *---------------------------------------------------------------------*
003100001005      * Flag oprativi di work
003200050905     D tipo_File       S              1    INZ('1')
003300110415     D xFinland        S              1    INZ(' ')
003400001005      *---------------------------------------------------------------------*
003500010402     D  MSGDS          S            100
003600001003     D  LENGH          S             15  5
003700001003      *
003800001003     D X               s              3  0 INZ(0)
003900001005     D Y               s              3  0 INZ(0)
004000010524      *
004100010524     D Conta1          s              4  0 INZ(0)
004200010524     D Conta2          s              4  0 INZ(0)
004300010612     D Last1           s              4  0 INZ(0)
004400010612     D Last2           s              4  0 INZ(0)
004500010612     D Tcolli1         s              9  0 INZ(0)
004600010612     D Tcolli2         s              9  0 INZ(0)
004700010612     D Tpeso1          s             16  3 INZ(0)
004800010612     D Tpeso2          s             16  3 INZ(0)
004900010524      *
005000010612      * *LDA Local Data Area per passare i totali da mettere in testata
005100010612     D AreaTot         DS
005200010612     D  Cnt1                   1      4  0 INZ(0)
005300010612     D  Cnt2                   5      8  0 INZ(0)
005400010612     D  Totcl1                 9     17  0 INZ(0)
005500010612     D  Totcl2                18     26  0 INZ(0)
005600010612     D  Totpe1                27     42  3 INZ(0)
005700010612     D  Totpe2                43     58  3 INZ(0)
005800050905     D  Filex                 59     59    INZ(' ')
005900000120      *---------------------------------------------------------------------*
006000000120      * DS
006100000120      *---------------------------------------------------------------------*
006200010518     D KPJBA         E DS
006300010516     D EDTD00DS      E DS
006400010516     D  D00                   77    142    DIM(3)
006500010516     D EDTD10DS      E DS
006600010516     D  D10                   73    186    DIM(3)
006700050905     D EDTD15DS      E DS
006800050905     D  D15                  445    500    DIM(2)
006900010516     D EDTD20DS      E DS
007000010516     D  W1496                 10     14
007100010516     D  W7224                 15     22
007200010516     D EDTD25DS      E DS
007300010730     D  WValore               22     37
007400010516      *  Ds per reperimento dati importi
007500010516     D WTD00           DS
007600010516     D  WTPI                   1      3
007700010516     D  WIMP                   4     19  3
007800010516     D  WVAL                  20     22
007900000120      *---------------------------------------------------------------------*
008000020916     D TIBS55DS      E DS
008100000120     D UTEDSE0F      E DS
008200000120     D  TCU                  398    697    DIM(50)                              Flg 8 tp.conto
008300000120     D  KCU                  698    847P 0 DIM(50)  PACKEVEN                    Capiconto
008400000929      *---------------------------------------------------------------------*
008500000929      *                    .. 5 - 8  : tipo record letto
008600000929      *---------------------------------------------------------------------*
008700000929     D WDAT            DS           587
008800010524     D  SUMTPR                 5      8
008900000929     D  SUMINR                 5      6
009000010516      *---------------------------------------------------------------------*
009100110415     D AGILITYDK       C                   CONST('AGILITYDK                    -
009200010516     D                                           ')
009300010517     D DIGITS          C                   CONST('0123456789')
009400000929     I*---------------------------------------------------------------------*
009500001002     I* FILE ridenominazione records
009600000929     I*---------------------------------------------------------------------*
009700001003     IINPUT000
009800000929     I              RRIFCSUM                    SUMDAT
009900001003     IWORKF000
010000001002     I              RRIFCSUM                    WRKDAT
010100010524     IIFCSUM01
010200010524     I              RRIFCSUM                    SUMDAT1
010300010524     IIFCSUM02
010400010524     I              RRIFCSUM                    SUMDAT2
010500000120      *---------------------------------------------------------------------*
010600000120      * Ciclo principale
010700000120      *---------------------------------------------------------------------*
010800001002      * Lettura iniziale sul file
010900010517      *               *----------------------------*
011000010517     C                   EXSR      Legge_rec
011100010517      *               *----------------------------*
011200960329      * Lettura fino a fine file
011300010517      *---------------
011400010514     C                   DOW       not %EOF(EDIFCSUM)
011500010516      *
011600010516      * Routine che esegue per controllare le singole spedizioni
011700010516      *   in cui testare i records di dettaglio messi nelle DS.
011800010516      *               *----------------------*
011900081210     C                   EXSR      split_x_NAZ
012000010516      *               *----------------------*
012100010516      *
012200001002      *  Copia il record da INPUT su WORKFile
012300010524     C                   If        SUMINR ='TD'
012400001003     C                   eval      WRKdat = SUMdat
012500001003     C                   WRITE     WORKF000
012600010524     C                   End
012700010517      * Lettura sul file
012800010517      *               *----------------------------*
012900010517     C                   EXSR      Legge_rec
013000010517      *               *----------------------------*
013100010517      *
013200010517      * A rottura dettaglio quindi a nuovo TD00 oppure a fine file
013300050905      *  copia il blocco di dettaglio in base alle condizioni trovate poi
013400050905      *   pulisce il file di parcheggio per prepararlo ad un nuovo ciclo.
013500010517     C                   If        not %EOF(EDIFCSUM) and SUMTPR = 'TD00' or
013600010517     C                                 %EOF(EDIFCSUM)
013700010517      *               *----------------------------*
013800010517     C                   EXSR      Cpy_Clr_SUM0
013900010517      *               *----------------------------*
014000110415     c                   eval      xFinland = *blank
014100010517     C                   ENDif
014200010516      *
014300001002     C                   ENDdo
014400010517      *---------------
014500050905      *  Alla fine rinumera sempre i dettagli generati e calcola i totali
014600010524      *               *----------------------------*
014700010524     C                   EXSR      Rinumera
014800010524      *               *----------------------------*
014900010514      *
015000960329     C                   SETON                                        LR
015100001002     C*----------------------------------------------------------------
015200010517     C*? Lettura del Flat File
015300001002     C*----------------------------------------------------------------
015400010517     C     Legge_Rec     BEGSR
015500010516      *
015600010517      * Lettura successiva
015700010517      *  muove il record su una DS di controllo
015800010517     C                   READ      EDIFCSUM
015900010517     C                   If        not %EOF(EDIFCSUM)
016000010517      *                          ++++++++++++++++++++++++
016100010517     C                   MOVEL     SUMDAT        WDAT
016200010517      *                          ++++++++++++++++++++++++
016300010517     C                   End
016400010517      *
016500010517     C                   ENDSR
016600010517     C*----------------------------------------------------------------
016700050905     C*? Dettaglio x ricercare in base alla regola quale file scrivere
016800010517     C*----------------------------------------------------------------
016900081210     C     split_x_NAZ   BEGSR
017000050906      *
017100110415      *?  Adesso determina solo se 1 o 2 e se si tratta di Spedizione in FINLAND.
017200050905      *   ossia
017300081210      *  separa il dettaglio
017400110415      *   se la nazione del Destinatario è la Finland
017500010516     C                   SELECT
017600050905      *
017700050905      * ?Lo esegue sempre x determinare la Nazione del Destinatario
017800110415      * Dettaglio TD15 = Segmento NAD (se --> Finlandia)
017900050905      *  >>>>>>>>>>>>>>
018000050905     C                   When      SUMTPR = 'TD15'
018100050905      *                *------------------*
018200050905     C                   EXSR      DRED15
018300050905      *                *------------------*
018400010516      *
018500081210     C                   other
018600010516      *
018700010516     C                   ENDsl
018800050905      *
018900010518     C                   ENDSR
019000050905     C*----------------------------------------------------------------
019100050905     C*? DRED15 - DECODIFICA TIPO RECORD TD15
019200050905     C*----------------------------------------------------------------
019300050905     C     DRED15        BEGSR
019400050905      *
019500050905     C                   CLEAR                   EDTD15DS
019600050905     C                   MOVEL     SUMDAT        EDTD15DS
019700050905      *
019800110415      *  Destinatario in FINLAND -->imposta e memorizza il flag
019900050905     c                   if        DD3035 = 'CN '
020000110415     c                   if        DD3207 = 'FI '                               <-- Nazione
020100110415     c                   eval      xFinland = 'S'
020200050905     c                   end
020300050905     c                   end
020400050905      *
020500050905     C                   ENDSR
020600001003     C*-------------------------------------------------------------------------------------
020700010517     C*?    SPOSTA I Records dal WRKFile ai Files separati
020800001003     C*-------------------------------------------------------------------------------------
020900010517     C     Cpy_Clr_SUM0  BEGSR
021000050905      *
021100050905      *  Imposta i flags di copia generali
021200110415      *   tenendo conto se files x Finland
021300110415     c                   if        xFinland = *blank
021400081210     C                   EVAL      tipo_File = '1'
021500050905     c                   else
021600110415      * se la destinazione è FINLAND :
021700081210     C                   EVAL      tipo_File = '2'
021800050905     c                   end
021900050905      *
022000010517      *  Chiude il file di work  >>>>>> esegue il CPYF <<<<<< quindi ripulisce
022100010517      *   il WorkFile per prepararlo a ricevere nuovo pacchetto di righe.
022200010517      *
022300010517      * chiude prima di copiarlo e ripulirlo
022400010517     C                   CLOSE     EDIFCSUM0
022500001003      *
022600110415      * copia     NO Finland
022700050905     C                   if        tipo_File = '1'
022800050906     C                   MOVEL     CMD(18)       MSGDS
022900010402     C                   Z-ADD     100           LENGH
023000001003     C                   CALL      'QCMDEXC'
023100001003     C                   PARM                    MSGDS
023200001003     C                   PARM                    LENGH
023300001003     C                   EndIF
023400001003      *
023500110415      * copia        Finland
023600050905     C                   if        tipo_File = '2'
023700050906     C                   MOVEL     CMD(19)       MSGDS
023800010402     C                   Z-ADD     100           LENGH
023900001003     C                   CALL      'QCMDEXC'
024000001003     C                   PARM                    MSGDS
024100001003     C                   PARM                    LENGH
024200001003     C                   EndIF
024300050905      *
024400050906      * pulizia del workfile EDIFCSUM0 dopo aver copiato la porzione
024500081210      *  di dettaglio nel file 1/2 secondo le "Regole"
024600050906     C                   MOVEL     CMD(13)       MSGDS
024700010402     C                   Z-ADD     100           LENGH
024800001003     C                   CALL      'QCMDEXC'
024900001003     C                   PARM                    MSGDS
025000001003     C                   PARM                    LENGH
025100010518      *
025200001003      *  riapre dopo la pulizia
025300010514     C                   OPEN      EDIFCSUM0
025400001005      *
025500001005     C                   Endsr
025600001003      *----------------------------------------------------------------
025700010524      *? Rinumera - Deve riordinare le spedizioni nel dettaglio
025800010524      *----------------------------------------------------------------
025900010524     C     Rinumera      BEGSR
026000010524      *
026100010524      *  File (1)
026200010524     C                   OPEN      EDIFCSUM1
026300010524     C                   READ      EDIFCSUM1
026400010524     C                   DOW       not %EOF(EDIFCSUM1)
026500010524     C                   MOVEL     SUMDAT1       WDAT
026600010612      *
026700010612      * reimposta la numerazione delle spedizioni
026800010524     C                   IF        SUMTPR = 'TD00'
026900010524     C                   CLEAR                   EDTD00DS
027000010524     C                   MOVEL     SUMDAT1       EDTD00DS
027100010524     C                   Eval      Conta1 = Conta1 + 1
027200010524     C                   Eval      DA1490 = Conta1
027300010524     C                   MOVEL     EDTD00DS      SUMDAT1
027400010524     C                   Update    IFCSUM01
027500010524     C                   ENDIF
027600010612      *
027700010612      * calcola i totali nr.colli
027800010612     C                   IF        SUMTPR = 'TD20'
027900010612     C                   CLEAR                   EDTD20DS
028000010612     C                   MOVEL     SUMDAT1       EDTD20DS
028100010612     C                   If        DE1496 = 99999
028200010612     C                   eval      Tcolli1 = Tcolli1 + DE7224
028300010612     C                   End
028400010612     C                   ENDIF
028500010612      *
028600010612      * calcola i totali di peso
028700010612     C                   IF        SUMTPR = 'TD25'
028800010612     C                   CLEAR                   EDTD25DS
028900010612     C                   MOVEL     SUMDAT1       EDTD25DS
029000010612     C                   If        DF6311 = 'WT ' and DF6411 = 'KGM'
029100010612     C                   eval      Tpeso1 = Tpeso1 + DF6314
029200010612     C                   End
029300010612     C                   ENDIF
029400010612      *
029500010524     C                   READ      EDIFCSUM1
029600010524     C                   ENDDO
029700081210      *
029800010612     C                   Eval      Last1  = Conta1
029900010524     C                   CLOSE     EDIFCSUM1
030000010524      *
030100010524      *  File (2)
030200010524     C                   OPEN      EDIFCSUM2
030300010524     C                   READ      EDIFCSUM2
030400010524     C                   DOW       not %EOF(EDIFCSUM2)
030500010524     C                   MOVEL     SUMDAT2       WDAT
030600010612      *
030700010612      * reimposta la numerazione delle spedizioni
030800010524     C                   IF        SUMTPR = 'TD00'
030900010524     C                   CLEAR                   EDTD00DS
031000010524     C                   MOVEL     SUMDAT2       EDTD00DS
031100010524     C                   Eval      Conta2 = Conta2 + 1
031200010524     C                   Eval      DA1490 = Conta2
031300010524     C                   MOVEL     EDTD00DS      SUMDAT2
031400010524     C                   Update    IFCSUM02
031500010524     C                   ENDIF
031600050906      *
031700050906      * calcola i totali nr.colli
031800050906     C                   IF        SUMTPR = 'TD20'
031900050906     C                   CLEAR                   EDTD20DS
032000050906     C                   MOVEL     SUMDAT2       EDTD20DS
032100050906     C                   If        DE1496 = 99999
032200050906     C                   eval      Tcolli2 = Tcolli2 + DE7224
032300050906     C                   End
032400050906     C                   ENDIF
032500050906      *
032600050906      * calcola i totali di peso
032700050906     C                   IF        SUMTPR = 'TD25'
032800050906     C                   CLEAR                   EDTD25DS
032900050906     C                   MOVEL     SUMDAT2       EDTD25DS
033000050906     C                   If        DF6311 = 'WT ' and DF6411 = 'KGM'
033100050906     C                   eval      Tpeso2 = Tpeso2 + DF6314
033200050906     C                   End
033300050906     C                   ENDIF
033400050906      *
033500050906     C                   READ      EDIFCSUM2
033600050906     C                   ENDDO
033700081210      *
033800050906     C                   Eval      Last2  = Conta2
033900050906     C                   CLOSE     EDIFCSUM2
034000010524      *
034100010612      *  alla fine del giro si hanno i contatori di totale pronti con
034200010612      *  i dati impostati per la testata dei 2 IFCSUM sono inseriti in
034300010612      *  una Local Data Area da passare ai programmi successivi.
034400010612     C                   IN        AreaTot
034500010612     C                   move      Conta1        Cnt1
034600010612     C                   move      Conta2        Cnt2
034700010612     C                   move      TColli1       Totcl1
034800010612     C                   move      TColli2       Totcl2
034900010612     C                   move      TPeso1        Totpe1
035000010612     C                   move      TPeso2        Totpe2
035100050906      * FileX --> viene reimpostato dal CLLE dopo chiusura di questo pgm
035200050906     C                   move      ' '           FileX
035300010612     C                   OUT       AreaTot
035400010612      *
035500010524     C                   Endsr
035600010524      *----------------------------------------------------------------
035700010524      *? *INZSR - OPERAZIONI INIZIALI
035800010524      *----------------------------------------------------------------
035900010524     C     *INZSR        BEGSR
036000010524      *
036100010514     C* Riceve un parametro per capire se si trova in ambiente di produzione
036200010514     C*  o in ambiente di test
036300010514     C     *entry        plist
036400010514     C                   PARM                    KPJBA
036500010612     C*
036600010612     C     *DTAARA       DEFINE    *LDA          AreaTot
036700010612     C*
036800010514      * DETERMINO SE SONO BARTOLINI O SDI e se in prova
036900020916     C                   CLEAR                   TIBS55DS
037000001003     C                   MOVEL     'L'           I50TLA
037100020916     C                   CALL      'TIBS55R'
037200020916     C                   PARM                    TIBS55DS
037300001003      *
037400001003      * RECUPERO DATI DELL'UTENTE
037500001003     C                   Z-ADD     1             CODUT
037600001003     C                   CALL      'XPARUT'
037700001003     C                   PARM                    UTEDSE0F
037800001003      *
037900010518      * ------------
038000050905      *  Copia in QTEMP i files di work EDIFCSUM0/1/2 /3/4
038100050905      *
038200050905      * ------------               x test con KPJBU vuota
038300050905     c                   if        kpjbu = *blank
038400050905     c                   exsr      cmd_test
038500050905     c                   endIF
038600050905      *
038700001003      * Apertura File UC e lettura estemporanea
038800010514     C                   OPEN      EDIFCSUM0
038900010514     C                   READ      EDIFCSUM0
039000010514     C*
039100001003     C                   ENDSR
039200050905     C*
039300050905      *****************************************************************
039400050905     C*    comandi x testare la procedura
039500050905      *****************************************************************
039600050905     C     cmd_test      BEGSR
039700050905      * ------------               x test con KPJBU vuota
039800050905      *
039900110415      *    in procedura il CLP TRTC83C3  già esegue questi  comandi
040000050905      *      serve farli qui dentro se si vuole provare il programma
040100050905      *        staccato da tutto.
040200050905     C                   clear                   MSGDS
040300050906     C                   MOVEL     CMD(03)       MSGDS
040400050905     C                   Z-ADD     100           LENGH
040500050905     C                   CALL      'QCMDEXC'                            99
040600050905     C                   PARM                    MSGDS
040700050905     C                   PARM                    LENGH
040800050905      *
040900050905     C                   clear                   MSGDS
041000050906     C                   MOVEL     CMD(04)       MSGDS
041100050905     C                   Z-ADD     100           LENGH
041200050905     C                   CALL      'QCMDEXC'                            99
041300050905     C                   PARM                    MSGDS
041400050905     C                   PARM                    LENGH
041500050905      *
041600050905     C                   clear                   MSGDS
041700050906     C                   MOVEL     CMD(05)       MSGDS
041800050905     C                   Z-ADD     100           LENGH
041900050905     C                   CALL      'QCMDEXC'                            99
042000050905     C                   PARM                    MSGDS
042100050905     C                   PARM                    LENGH
042200050905      *
042300050906      * Esegue OVRDBF in QTEMP per non sovrapporsi ad altri Manifest
042400050905     C                   clear                   MSGDS
042500050906     C                   MOVEL     CMD(08)       MSGDS
042600050905     C                   Z-ADD     100           LENGH
042700050905     C                   CALL      'QCMDEXC'
042800050905     C                   PARM                    MSGDS
042900050905     C                   PARM                    LENGH
043000050905      *
043100050906     C                   MOVEL     CMD(09)       MSGDS
043200050905     C                   Z-ADD     100           LENGH
043300050905     C                   CALL      'QCMDEXC'
043400050905     C                   PARM                    MSGDS
043500050905     C                   PARM                    LENGH
043600050905      *
043700050906     C                   MOVEL     CMD(10)       MSGDS
043800050905     C                   Z-ADD     100           LENGH
043900050905     C                   CALL      'QCMDEXC'
044000050905     C                   PARM                    MSGDS
044100050905     C                   PARM                    LENGH
044200050906      * ------------
044300081210      * Pulizia dei workfiles EDIFCSUM0/1/2
044400050906     C                   MOVEL     CMD(13)       MSGDS
044500050905     C                   Z-ADD     100           LENGH
044600050905     C                   CALL      'QCMDEXC'
044700050905     C                   PARM                    MSGDS
044800050905     C                   PARM                    LENGH
044900050905      *
045000050906     C                   MOVEL     CMD(14)       MSGDS
045100050905     C                   Z-ADD     100           LENGH
045200050905     C                   CALL      'QCMDEXC'
045300050905     C                   PARM                    MSGDS
045400050905     C                   PARM                    LENGH
045500050905      *
045600050906     C                   MOVEL     CMD(15)       MSGDS
045700050905     C                   Z-ADD     100           LENGH
045800050905     C                   CALL      'QCMDEXC'
045900050905     C                   PARM                    MSGDS
046000050905     C                   PARM                    LENGH
046100050905      *
046200050905     C                   ENDSR
046300001002      *****************************************************************
046400001005** (CMD)
046500081210
046600081210
046700050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM0) MBROPT(*REPLACE) CRTFILE(*YES)
046800050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM1) MBROPT(*REPLACE) CRTFILE(*YES)
046900050906CPYF FROMFILE(EDIFCSUM) TOFILE(QTEMP/EDIFCSUM2) MBROPT(*REPLACE) CRTFILE(*YES)
047000081210
047100081210
047200050906OVRDBF FILE(EDIFCSUM0) TOFILE(QTEMP/EDIFCSUM0)
047300050906OVRDBF FILE(EDIFCSUM1) TOFILE(QTEMP/EDIFCSUM1)
047400050906OVRDBF FILE(EDIFCSUM2) TOFILE(QTEMP/EDIFCSUM2)
047500081210
047600081210
047700050906CLRPFM QTEMP/EDIFCSUM0
047800050906CLRPFM QTEMP/EDIFCSUM1
047900050906CLRPFM QTEMP/EDIFCSUM2
048000081210
048100081210
048200050906CPYF FROMFILE(EDIFCSUM0) TOFILE(EDIFCSUM1) MBROPT(*ADD) FMTOPT(*NOCHK)
048300050906CPYF FROMFILE(EDIFCSUM0) TOFILE(EDIFCSUM2) MBROPT(*ADD) FMTOPT(*NOCHK)
048400081210
048500081210
048600050906
048700050906
048800050905
048900050905
