000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300030808     H* Creazione wrk-file VARIE fatturate
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900030808     Fwfvft01l  UF A E           K DISK
001000030912     Fwfvft10f  UF A E             DISK
001100051228     Fwfvftce10fUF A E             DISK    RENAME(wfvft010:wfvft010ce)
001200000810     Ftabel00f  IF   E           K DISK
001300030808     Ftita730c  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500030808     D* SCHIERE
001600030808     D*------------------------------------------------------------------------*
001700030808     D*------------
001800030808     D* SCHIERE SIGLE E DESCRIZIONI VARIE
001900030808     D*------------
002000030808     D SKSVN           S              1    DIM(100)
002100030808     D SKSVNDES        S             20    DIM(100)
002200030811     D*------------
002300030811     D* SCHIERE TIPI BOLLA
002400030811     D*------------
002500030811     D SKTBL           S              2    DIM(100)
002600991214     D*------------
002700970526     D* ARCHITETTURA
002800970526     D*------------
002900970526     D KPJBA         E DS
003000051227     D*------------
003100030811     D* DS RIDEFINIZIONE TABELLA "TB" (TIPI BOLLA)
003200030811     D*------------
003300030811     D DSTB          E DS
003400030808     D*------------
003500030808     D* DS RIDEFINIZIONE TABELLA "CC" (VARIE)
003600030808     D*------------
003700030808     D DSCC          E DS
003800030808     D*------------
003900030808     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
004000030808     D*------------
004100030808     D DSQC2         E DS
004200030808     D*------------------
004300030808     D* DS X PASSAGGIO PARAMETRI
004400030808     D*------------------
004500030808     D PARAM           DS
004600030808     D  MMAAAAFAT              1      6
004700051229     D  MMAAAAFATNUM           1      6  0
004800030808     D  MMFAT                  1      2  0
004900030808     D  AAAAFAT                3      6  0
005000030808     D  DATAFATFINMES          7     14  0
005100030808     D  GGFATFINMES            7      8  0
005200030808     D  MMFATFINMES            9     10  0
005300030808     D  AAAAFATFINMES         11     14  0
005400030916     D  DETTAGLIO             15     15
005500031111     D  SOLOVARIA             16     16
005600051227     D  PERIODOCEAAAA         17     20  0
005700051227     D  PERIODOCEMM           21     22  0
005800051227     D  PERIODOCE             17     22  0
005900051227     D  STORNONACC            23     23
006000051227     D  ELABORA_CE            24     24
006100030808     D*------------------
006200030808     D* DS LETTURA RECORDSET SQL SU FORMATO "TITAS"
006300030808     D*------------------
006400030808     D  DSTAS        E DS                  extname(TITAS00F)
006500011206     D*-------------------
006600011206     D* VARIABILI DI WRK
006700011206     D*-------------------
006800011206     D  SQLline        s           5000    varying
006900030808     D  i              s              3  0 inz(*zeros)
007000030808     D  wSVN           s                   like(vftSVN)
007100030808     D  wVAN           s                   like(vftVAN)
007200030808     D  kvftAMF        s                   like(vftAMF)
007300030808     D  kvftSVN        s                   like(vftSVN)
007400030808     D  kvftFPT        s                   like(vftFPT)
007500970526     C*------------------------------------------------------------------------*
007600991214     C* MAIN LINES
007700970526     C*------------------------------------------------------------------------*
007800030808     C*
007900030808     C* ESEGUE LA ROUTINE PRINCIPALE
008000991214     C                   EXSR      ESEGUI
008100060125     C*
008200060125     C* ESEGUE CALL A *PGM D STAMPA: C/E QUADRATURA RICAVI
008300060125     C                   MOVEL(P)  PERIODOCE     KPJBU
008400060125     C                   CALL      'TREC93R'
008500060125     C                   PARM                    KPJBA
008600000306     C*
008700970526     C                   SETON                                        LR
008800970526     C*------------------------------------------------------------------------*
008900991217     C* ESEGUI - OPERAZIONI PRINCIPALI
009000970526     C*------------------------------------------------------------------------*
009100991214     C     ESEGUI        BEGSR
009200011205     C*
009300051228     C  N65              exsr      dltWVFT
009400051228     C   65              exsr      dltWVFTCE
009500030808     C                   exsr      carTAB
009600030808     C                   exsr      exeVARIE
009700970526     C*
009800970526     C                   ENDSR
009900030808     C*------------------------------------------------------------------------*
010000031111     C* DLTWVFT - Routine di eliminazione record wrk-file dello stesso periodo di fatturazione
010100030808     C*------------------------------------------------------------------------*
010200030808     C     DLTWVFT       BEGSR
010300030808     C*
010400091104     C                   EVAL      SQLline = 'delete from wfvft00f where '  +
010500091104     C                                 'vftamf = '+%subst(MMAAAAFAT:3:4)    +
010600091104     C                                 %subst(MMAAAAFAT:1:2)
010700030808     C*
010800030808     C/EXEC SQL
010900030808     C+ EXECUTE IMMEDIATE: SQLline
011000030808     C/END-EXEC
011100030916     C*
011200030916     C* Se lancio anche x dettaglio elimino il contenuto x periodo d fatturazione anche
011300030916     C* del file dettaglio WFVFT10F
011400030916     C                   IF        DETTAGLIO = 'S'
011500091104     C                   IF        SOLOVARIA <> *BLANKS
011600091104     C                   EVAL      SQLline = 'delete from wfvft10f where '  +
011700091104     C                                 'vftamf = ' + %subst(MMAAAAFAT:3:4)  +
011800091104     C                                 %subst(MMAAAAFAT:1:2)                +
011900091104     C                                 ' and vftsvn = ''' + SOLOVARIA + ''''
012000091104     C                   ELSE
012100091104     C                   EVAL      SQLline = 'delete from wfvft10f'
012200091104     C                   ENDIF
012300030916     C*
012400030916     C/EXEC SQL
012500030916     C+ EXECUTE IMMEDIATE: SQLline
012600030916     C/END-EXEC
012700030916     C                   ENDIF
012800030808     C*
012900030808     C                   ENDSR
013000051228     C*------------------------------------------------------------------------*
013100051228     C* DLTWVFTCE - Routine di eliminazione record wrk-file dello stesso periodo di fatturazione
013200051228     C*------------------------------------------------------------------------*
013300051228     C     DLTWVFTCE     BEGSR
013400051228     C*
013500051228     C                   EVAL      SQLline = 'delete from wfvftce00f where '+
013600091104     C                                 'vftamf = '+%subst(MMAAAAFAT:3:4)    +
013700091104     C                                 %subst(MMAAAAFAT:1:2)
013800051228     C*
013900051228     C/EXEC SQL
014000051228     C+ EXECUTE IMMEDIATE: SQLline
014100051228     C/END-EXEC
014200051228     C*
014300051228     C* Se lancio anche x dettaglio elimino il contenuto x periodo d fatturazione anche
014400051228     C* del file dettaglio WFVFTCE10F
014500051228     C                   IF        DETTAGLIO = 'S'
014600091104     C                   IF        SOLOVARIA <> *BLANKS
014700091104     C                   EVAL      SQLline = 'delete from wfvftce10f where '+
014800091104     C                                 'vftamf = ' + %subst(MMAAAAFAT:3:4)  +
014900091104     C                                 %subst(MMAAAAFAT:1:2)                +
015000091104     C                                 ' and vftsvn = ''' + SOLOVARIA + ''''
015100091104     C                   ELSE
015200091104     C                   EVAL      SQLline = 'delete from wfvftce10f'
015300091104     C                   ENDIF
015400051228     C*
015500051228     C/EXEC SQL
015600051228     C+ EXECUTE IMMEDIATE: SQLline
015700051228     C/END-EXEC
015800051228     C                   ENDIF
015900051228     C*
016000051228     C                   ENDSR
016100000929     C*------------------------------------------------------------------------*
016200030808     C* EXEVARIE - Routine di selezione record da TITAS00F+TITAS10F ed elaborazione VARIE
016300000929     C*------------------------------------------------------------------------*
016400030808     C     EXEVARIE      BEGSR
016500011206     C*
016600030808     C* Costrisco le date x i criteri di esclusione bolle da elaborare
016700030808     C                   Z-ADD     *zeros        wDataFatDa        8 0
016800030808     C                   Z-ADD     *zeros        wDataFatA         8 0
016900030808     C                   Z-ADD     *zeros        wDataFatFinMes    8 0
017000030915     C                   EVAL      wDataFatDa = AAAAFAT*10000+MMFAT*100+01
017100030808     C                   EVAL      wDataFatA  = AAAAFAT*10000+MMFAT*100+99
017200030808     C                   EVAL      wDataFatFinMes = AAAAFATFINMES*10000+
017300030808     C                                              MMFATFINMES*100+GGFATFINMES
017400030808     C*
017500030808     C/EXEC SQL
017600030808     C+ declare C1 cursor for
017700030808     C+ select * from titas00f where
017800031202     C+ ((tasdft between :wDataFatDa AND :wDataFatA AND tasfiv <> 0) OR
017900170712     C+  (tasdft = :wDataFatFinMes AND (tasfiv = :§QCFIV OR tasfiv = :§QCFI2 OR
018000170712     C+   tasfiv = :§QCFI7 OR tasfiv = 0)))
018100050405     C****+ AND tassv1 <> ' '
018200050503     C****+ AND tasimv <> 0
018300030808     C+ union
018400030808     C+ select * from titas10f where
018500031202     C+ ((tasdft between :wDataFatDa AND :wDataFatA AND tasfiv <> 0) OR
018600031202     C+  (tasdft = :wDataFatFinMes AND (tasfiv = :§QCFIV OR tasfiv = :§QCFI2 OR
018700170712     C+   tasfiv = :§QCFI7 OR tasfiv = 0)))
018800050405     C****+ AND tassv1 <> ' '
018900050503     C****+ AND tasimv <> 0
019000030808     C+ for read only
019100030808     C/END-EXEC
019200030808     C
019300030808     C/EXEC SQL
019400030808     C+ open C1
019500030808     C/END-EXEC
019600030808     C
019700030808     C/EXEC SQL
019800030808     C+ Fetch C1 into :DSTAS
019900030808     C/END-EXEC
020000030808     C*
020100030808     C                   dow       sqlcod = *zeros
020200030811     C                   exsr      chkREC
020300030811     C                   if        wrecok = 'S'
020400030808     C                   exsr      exeSVN
020500030811     C                   endif
020600030808     C*
020700030808     C/EXEC SQL
020800030808     C+ Fetch C1 into :DSTAS
020900030808     C/END-EXEC
021000030808     C*
021100030808     C                   enddo
021200030808     C*
021300030808     C/EXEC SQL
021400030808     C+ close C1
021500030808     C/END-EXEC
021600000929     C*
021700000929     C                   ENDSR
021800030811     C*------------------------------------------------------------------------*
021900030811     C* CHKREC - Routine di verifica validità record
022000030811     C*------------------------------------------------------------------------*
022100030811     C     CHKREC        BEGSR
022200030811     C*
022300030811     C                   MOVEL     'S'           wrecok            1
022400030811     C*
022500030811     C* Verifica se il tipo bolla è da considerare
022600030915     C     tasTBL        LOOKUP    SKTBL                                  55
022700030811     C                   IF        %found
022800030811     C                   ELSE
022900030811     C                   MOVEL     'N'           wrecok
023000030811     C                   ENDIF
023100030915     C*
023200030915     C* Verifica le bolle fatturate inizio mese, occorre considerare solo quelle aventi P.O. IVA div
023300030915     C* valori indicati in tabella QT.
023400030915     C                   IF        wrecok = 'S'
023500030915     C                   IF        tasdft = wDataFatDa AND
023600170712     C                             (tasfiv = §QCFIV OR
023700170712     C                              tasfiv = §QCFI2 OR
023800170712     C                              tasfiv = §QCFI7)
023900030915     C                   MOVEL     'N'           wrecok
024000030915     C                   ENDIF
024100030915     C                   ENDIF
024200051227     C*
024300051227     C* Se richiesta elaborazione x verifica C.E.
024400051227     C                   IF        ELABORA_CE = 'S'
024500051227     C*
024600051227     C* Se richiesto nei parametri periodo di competenza x C.E. verifico data sped
024700051227     C                   IF        PERIODOCE <> *zeros
024800051227     C                   MOVEL     tasMGS        wMeseBolla        2 0
024900051227     C                   IF        tasAAS*100+wMeseBolla = PERIODOCE
025000051227     C                   ELSE
025100051227     C                   MOVEL     'N'           wrecok
025200051227     C                   ENDIF
025300051227     C                   ENDIF
025400051227     C*
025500051227     C* Se richiesto nei parametri flag d storno N/ACC x C.E. verifico
025600051227     C                   IF        STORNONACC = 'S'
025700051227     C                   IF        tasFA2 = 'S'
025800051227     C                   MOVEL     'N'           wrecok
025900051227     C                   ENDIF
026000051227     C                   ENDIF
026100051227     C*
026200051227     C                   ENDIF
026300030811     C*
026400030811     C                   ENDSR
026500030808     C*------------------------------------------------------------------------*
026600030808     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
026700030808     C*------------------------------------------------------------------------*
026800030808     C     EXESVN        BEGSR
026900030808     C*
027000030808     C* Inizializzo ad ogni bolla le variabili d appoggio relative a SIGLA e IMPORTO VARIE
027100030808     C                   MOVEL     *blanks       wSVN
027200030808     C                   Z-ADD     *zeros        wVAN
027300050315     C*
027400050315     C* Verifica ed eventuale scrittura del PORTO su TITAS
027500050315     C                   IF        tasPOR <> *zeros
027600050315     C                   EVAL      wSVN = *blanks
027700050315     C                   EVAL      wVAN = tasPOR
027800050315     C                   EXSR      WRISVN
027900050315     C                   ENDIF
028000051227     C*
028100051227     C* Verifica ed eventuale scrittura del totale imponibile (solo x lancio C.E.)
028200051227     C                   IF        ELABORA_CE = 'S'
028300051227     C                   IF        tasIMV <> *zeros
028400051227     C                   EVAL      wSVN = '['
028500051227     C                   EVAL      wVAN = tasIMV
028600051227     C                   EXSR      WRISVN
028700051227     C                   ENDIF
028800051227     C                   ENDIF
028900030808     C*
029000030808     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
029100030916     C                   IF        tasSV1 <> *blanks
029200030808     C                   EVAL      wSVN = tasSV1
029300030808     C                   EVAL      wVAN = tasVA1
029400030808     C                   EXSR      WRISVN
029500030808     C                   ENDIF
029600030808     C*
029700030808     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
029800030916     C                   IF        tasSV2 <> *blanks
029900030808     C                   EVAL      wSVN = tasSV2
030000030808     C                   EVAL      wVAN = tasVA2
030100030808     C                   EXSR      WRISVN
030200030808     C                   ENDIF
030300030808     C*
030400030808     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
030500030808     C                   IF        tasSV3 <> *blanks
030600030808     C                   EVAL      wSVN = tasSV3
030700030808     C                   EVAL      wVAN = tasVA3
030800030808     C                   EXSR      WRISVN
030900030808     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
031000030808     C     KEYta7        SETLL     tita730c
031100030808     C                   IF        %equal(tita730c)
031200030808     C     KEYta7        READE     tita730c
031300030808     C                   DOW       not %eof(tita730c)
031400030912     C                   if        ta7VAN > *zeros
031500030808     C                   EVAL      wSVN = ta7SVN
031600030808     C                   EVAL      wVAN = ta7VAN
031700030808     C                   EXSR      WRISVN
031800030904     C                   endif
031900030808     C     KEYta7        READE     tita730c
032000030808     C                   ENDDO
032100030808     C                   ENDIF
032200030808     C                   ENDIF
032300030808     C*
032400030808     C                   ENDSR
032500030808     C*------------------------------------------------------------------------*
032600030808     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
032700030808     C*------------------------------------------------------------------------*
032800030808     C     WRISVN        BEGSR
032900031111     C*
033000030916     C* Elaboro tutte le varie tranne la 8 (IVA)
033100030916     C                   IF        wSVN <> '8'
033200030808     C                   MOVEL     wDataFatDa    kvftAMF
033300030808     C*
033400030912     C* Inizializzo i buffer del record del wrk-file
033500030808     C                   CLEAR                   WFVFT000
033600030912     C                   CLEAR                   WFVFT010
033700051228     C                   CLEAR                   WFVFT010CE
033800030808     C*
033900030912     C* Scrivo il wrk-file "raggruppato" x periodo/varia
034000030808     C                   EVAL      kvftSVN = wSVN
034100051228     C     KEYvft_1      CHAIN     WFVFT01L
034200051228     C*
034300030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
034400030808     C* incremneto il conteggio e l'importo della VARIA
034500030808     C                   IF        %found(WFVFT01L)
034600030808     C                   EVAL      vftVAN = vftVAN + wVAN
034700090702     C* X la varia 'a' => nn conteggio spedizioni ma sommo colli
034800090702     C                   IF        wSVN = 'a'
034900090702     C                   EVAL      vftCNT = vftCNT + tasNCL
035000090702     C                   ELSE
035100030808     C                   EVAL      vftCNT = vftCNT + 1
035200090702     C                   ENDIF
035300030808     C                   UPDATE    WFVFT000
035400030808     C                   ELSE
035500051228     C*
035600030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
035700030808     C* valorizzo il buffer del wrk-file e inserisco il record
035800030808     C                   EVAL      vftFPT = *blanks
035900030808     C                   EVAL      vftAMF = kvftAMF
036000030808     C                   EVAL      vftSVN = wSVN
036100030808     C                   EVAL      vftVAN = wVAN
036200090702     C* X la varia 'a' => nn conteggio spedizioni ma sommo colli
036300090702     C                   IF        wSVN = 'a'
036400090702     C                   EVAL      vftCNT = tasNCL
036500090702     C                   ELSE
036600090702     C                   EVAL      vftCNT = 1
036700090702     C                   ENDIF
036800030912     C* Reperisco la descrizione della VARIA
036900030808     C                   Z-ADD     1             i
037000030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
037100030808     C                   IF        %found
037200030808     C                   EVAL      vftDES = SKSVNDES(i)
037300030808     C                   ELSE
037400030808     C                   EVAL      vftDES = *all'*'
037500030808     C                   ENDIF
037600030808     C*
037700030808     C                   WRITE     WFVFT000
037800030808     C                   ENDIF
037900051228     C*
038000030808     C* Verifico se trattasi d bolla PT, se bolla POSTE rifaccio il medesimo giro precedente
038100030808     C* x incrementare/creare un "DI CUI" della VARIA corrente x le bolle PT
038200030808     C                   IF        tasLNP = 180
038300030808     C                   EVAL      kvftFPT = 'S'
038400051228     C     KEYvft_2      CHAIN     WFVFT01L
038500051228     C*
038600030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
038700030808     C* incremneto il conteggio e l'importo della VARIA
038800030808     C                   IF        %found(WFVFT01L)
038900030808     C                   EVAL      vftVAN = vftVAN + wVAN
039000090702     C* X la varia 'a' => nn conteggio spedizioni ma sommo colli
039100090702     C                   IF        wSVN = 'a'
039200090702     C                   EVAL      vftCNT = vftCNT + tasNCL
039300090702     C                   ELSE
039400090702     C                   EVAL      vftCNT = vftCNT + 1
039500090702     C                   ENDIF
039600030808     C                   UPDATE    WFVFT000
039700030808     C                   ELSE
039800051228     C*
039900030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
040000030808     C* valorizzo il buffer del wrk-file e inserisco il record
040100030808     C                   EVAL      vftFPT = kvftFPT
040200030808     C                   EVAL      vftAMF = kvftAMF
040300030808     C                   EVAL      vftSVN = wSVN
040400030808     C                   EVAL      vftVAN = wVAN
040500090702     C* X la varia 'a' => nn conteggio spedizioni ma sommo colli
040600090702     C                   IF        wSVN = 'a'
040700090702     C                   EVAL      vftCNT = tasNCL
040800090702     C                   ELSE
040900090702     C                   EVAL      vftCNT = 1
041000090702     C                   ENDIF
041100030808     C* Reperisco la descricione della VARIA
041200030808     C                   Z-ADD     1             i
041300030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
041400030808     C                   IF        %found
041500030808     C                   EVAL      vftDES = SKSVNDES(i)
041600030808     C                   ELSE
041700030808     C                   EVAL      vftDES = *all'*'
041800030808     C                   ENDIF
041900051227     C*
042000030808     C                   WRITE     WFVFT000
042100030808     C                   ENDIF
042200030808     C                   ENDIF
042300030912     C*
042400031111     C* Se richiesto scrivo il wrk-file d dettaglio x key bolla/periodo/varia
042500030916     C                   IF        DETTAGLIO = 'S'
042600031111     C* Se richiesta solo 1 varia in particolare => gestisco
042700031111     C                   SETOFF                                       50
042800031111     C                   IF        SOLOVARIA <> *BLANKS
042900031111     C                   IF        wSVN <> SOLOVARIA
043000031111     C                   SETON                                        50
043100031111     C                   ENDIF
043200031111     C                   ENDIF
043300031111     C                   IF        *IN50 = *OFF
043400030912     C                   EVAL      vftAAS = tasAAS
043500030912     C                   EVAL      vftMGS = tasMGS
043600030912     C                   EVAL      vftLNP = tasLNP
043700030912     C                   EVAL      vftNRS = tasNRS
043800030912     C                   EVAL      vftNSP = tasNSP
043900030912     C                   EVAL      vftTBL = tasTBL
044000050513     C                   EVAL      vftKSC = tasKSC
044100050513     C                   EVAL      vftCTR = tasCTR
044200030912     C                   EVAL      vftFPT = *blanks
044300030912     C                   EVAL      vftAMF = kvftAMF
044400030912     C                   EVAL      vftSVN = wSVN
044500030912     C                   EVAL      vftVAN = wVAN
044600090702     C* X la varia 'a' => nn conteggio spedizioni ma sommo colli
044700090702     C                   IF        wSVN = 'a'
044800090702     C   65              EVAL      vftCNT = 1
044900090702     C  N65              EVAL      vftCNT = tasNCL
045000090702     C                   ELSE
045100090702     C                   EVAL      vftCNT = 1
045200090702     C                   ENDIF
045300051227     C*
045400051227     C* A seconda del tipo elaborazinoe richiesta scrivo o il file "normale"
045500051227     C* o il file "ai fini C.E."
045600051228     C   65              WRITE     WFVFT010CE
045700051228     C  N65              WRITE     WFVFT010
045800031111     C                   ENDIF
045900030916     C                   ENDIF
046000030916     C                   ENDIF
046100030808     C*
046200030808     C                   ENDSR
046300011205     C*------------------------------------------------------------------------*
046400030808     C* CARTAB - CARICA I DATI TABELLATI
046500011205     C*------------------------------------------------------------------------*
046600030808     C     CARTAB        BEGSR
046700030811     C*
046800030811     C* Reperisco i tipi bolla validi come spedizioni
046900030811     C                   Z-ADD     *zeros        i
047000030811     C                   Z-ADD     1             tblKUT
047100030811     C                   MOVEL(P)  'TB'          tblCOD
047200030811     C     KEYtab_1      SETLL     TABEL00F
047300030811     C                   IF        %equal(TABEL00F)
047400030811     C     KEYtab_1      READE     TABEL00F
047500030811     C                   DOW       not %eof(TABEL00F)
047600030811     C                   MOVEL(P)  TBLUNI        DSTB
047700030811     C                   IF        §TBFCB = '1'
047800030811     C                   ADD       1             i
047900030811     C                   MOVEL(P)  TBLKEY        SKTBL(i)
048000030811     C                   ENDIF
048100030811     C     KEYtab_1      READE     TABEL00F
048200030811     C                   ENDDO
048300030811     C                   ENDIF
048400030808     C*
048500030808     C* Reperisco le descrizioni delle varie
048600030811     C                   Z-ADD     *zeros        i
048700030808     C                   Z-ADD     1             tblKUT
048800030808     C                   MOVEL(P)  'CC'          tblCOD
048900030808     C     KEYtab_1      SETLL     TABEL00F
049000030808     C                   IF        %equal(TABEL00F)
049100030808     C     KEYtab_1      READE     TABEL00F
049200030808     C                   DOW       not %eof(TABEL00F)
049300030808     C                   ADD       1             i
049400030808     C                   MOVE(P)   TBLKEY        SKSVN(i)
049500030808     C                   MOVEL(P)  TBLUNI        DSCC
049600030808     C                   MOVEL(P)  §CCDES        SKSVNDES(i)
049700030808     C     KEYtab_1      READE     TABEL00F
049800030808     C                   ENDDO
049900030808     C                   ENDIF
050000030808     C*
050100030808     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
050200030808     C                   CLEAR                   DSQC2
050300030808     C                   Z-ADD     1             tblKUT
050400030808     C                   MOVEL(P)  'QC'          tblCOD
050500030808     C                   MOVEL(P)  '2'           tblKEY
050600030808     C     KEYtab_2      CHAIN     TABEL00F
050700030808     C                   IF        %found(TABEL00F)
050800030808     C                   MOVEL(P)  TBLUNI        DSQC2
050900030808     C                   ENDIF
051000011206     C*
051100011205     C                   ENDSR
051200051229     C*------------------------------------------------------------------------*
051300051229     C* COMPLETAPARAM - COMPLETA I PARAMETRI DI LANCIO RICEVUTI
051400051229     C*------------------------------------------------------------------------*
051500051229     C     COMPLETAPARAM BEGSR
051600051229     C*
051700051229     C                   IF        MMFAT = 12
051800051229     C                   EVAL      DATAFATFINMES = 0101*10000+AAAAFAT+1
051900051229     C                   ELSE
052000051229     C                   EVAL      DATAFATFINMES = 01*1000000+(MMFAT+1)*10000+
052100051229     C                                             AAAAFAT
052200051229     C                   ENDIF
052300051229     C*
052400051229     C                   ENDSR
052500970526     C*------------------------------------------------------------------------*
052600970526     C* *INZSR - OPERAZIONI INIZIALI
052700970526     C*------------------------------------------------------------------------*
052800970526     C     *INZSR        BEGSR
052900970526     C*---
053000970526     C* RICEVIMENTO PARAMETRI
053100970526     C*---
053200970526     C     *ENTRY        PLIST
053300970612     C                   PARM                    KPJBA
053400991214     C                   MOVEL     KPJBU         PARAM
053500051229     C*
053600051229     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA
053700051229     C                   EXSR      COMPLETAPARAM
053800051227     C*
053900051227     C* EFFETTUO SUBITO ALCUNE CONSIDERAZIONI IN RELAZIONE AI PARAMETRI IN INGRESSO
054000051227     C                   IF        ELABORA_CE = 'S'
054100051227     C                   SETON                                        65
054200051227     C                   ELSE
054300051227     C                   SETOFF                                       65
054400051227     C                   ENDIF
054500970526     C*---
054600991214     C* CHIAVI DI LETTURA
054700991214     C*---
054800000810     C*
054900030808     C* LETTURA tabel00f - PARZIALE
055000030808     C     KEYtab_1      KLIST
055100030808     C                   KFLD                    tblKUT                         *UTENTE
055200030808     C                   KFLD                    tblCOD                         *TABELLA
055300030808     C*
055400030808     C* LETTURA tabel00f - COMPLETA
055500030808     C     KEYtab_2      KLIST
055600030808     C                   KFLD                    tblKUT                         *UTENTE
055700030808     C                   KFLD                    tblCOD                         *TABELLA
055800030808     C                   KFLD                    tblKEY                         *CHIAVE
055900030808     C*
056000030808     C* LETTURA wfvft01l - COMPLETA
056100030808     C     KEYvft_1      KLIST
056200030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
056300030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
056400030808     C*
056500030808     C* LETTURA wfvft01l - COMPLETA
056600030808     C     KEYvft_2      KLIST
056700030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
056800030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
056900030808     C                   KFLD                    kvftFPT                        *FLAG POSTE
057000030808     C*
057100030808     C* LETTURA tita730c - PARZIALE
057200030808     C     KEYta7        KLIST
057300030808     C                   KFLD                    tasAAS                         *ANNO BOLLA
057400030808     C                   KFLD                    tasLNP                         *LINEA PARTENZA BOLL
057500030808     C                   KFLD                    tasNRS                         *SERIE BOLLA
057600030808     C                   KFLD                    tasNSP                         *NUMERO SPEDIZIONE B
057700030808     C                   KFLD                    tasTBL                         *TIPO BOLLA
057800991214     C*
057900970526     C                   ENDSR
