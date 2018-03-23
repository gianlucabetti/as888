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
001100051227     Fwfvftce01lUF A E           K DISK    RENAME(wfvft000:wfvft0ce) PREFIX(ce_)
001200051227     Fwfvftce10fUF A E             DISK    RENAME(wfvft010:wfvft1ce) PREFIX(ce_)
001300000810     Ftabel00f  IF   E           K DISK
001400030808     Ftita730c  IF   E           K DISK
001500970526     D*------------------------------------------------------------------------*
001600030808     D* SCHIERE
001700030808     D*------------------------------------------------------------------------*
001800030808     D*------------
001900030808     D* SCHIERE SIGLE E DESCRIZIONI VARIE
002000030808     D*------------
002100030808     D SKSVN           S              1    DIM(100)
002200030808     D SKSVNDES        S             20    DIM(100)
002300030811     D*------------
002400030811     D* SCHIERE TIPI BOLLA
002500030811     D*------------
002600030811     D SKTBL           S              2    DIM(100)
002700991214     D*------------
002800970526     D* ARCHITETTURA
002900970526     D*------------
003000970526     D KPJBA         E DS
003100051227     D*------------
003200030811     D* DS RIDEFINIZIONE TABELLA "TB" (TIPI BOLLA)
003300030811     D*------------
003400030811     D DSTB          E DS
003500030808     D*------------
003600030808     D* DS RIDEFINIZIONE TABELLA "CC" (VARIE)
003700030808     D*------------
003800030808     D DSCC          E DS
003900030808     D*------------
004000030808     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
004100030808     D*------------
004200030808     D DSQC2         E DS
004300030808     D*------------------
004400030808     D* DS X PASSAGGIO PARAMETRI
004500030808     D*------------------
004600030808     D PARAM           DS
004700030808     D  MMAAAAFAT              1      6
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
008100000306     C*
008200970526     C                   SETON                                        LR
008300970526     C*------------------------------------------------------------------------*
008400991217     C* ESEGUI - OPERAZIONI PRINCIPALI
008500970526     C*------------------------------------------------------------------------*
008600991214     C     ESEGUI        BEGSR
008700011205     C*
008800030808     C                   exsr      dltWVFT
008900030808     C                   exsr      carTAB
009000030808     C                   exsr      exeVARIE
009100970526     C*
009200970526     C                   ENDSR
009300030808     C*------------------------------------------------------------------------*
009400031111     C* DLTWVFT - Routine di eliminazione record wrk-file dello stesso periodo di fatturazione
009500030808     C*------------------------------------------------------------------------*
009600030808     C     DLTWVFT       BEGSR
009700030808     C*
009800030808     C                   EVAL      SQLline = 'delete from wfvft00f where '+
009900030808     C                                       'vftamf = '+%subst(MMAAAAFAT:3:4)+
010000030808     C                                       %subst(MMAAAAFAT:1:2)
010100030808     C*
010200030808     C/EXEC SQL
010300030808     C+ EXECUTE IMMEDIATE: SQLline
010400030808     C/END-EXEC
010500030916     C*
010600030916     C* Se lancio anche x dettaglio elimino il contenuto x periodo d fatturazione anche
010700030916     C* del file dettaglio WFVFT10F
010800030916     C                   IF        DETTAGLIO = 'S'
010900030916     C                   EVAL      SQLline = 'delete from wfvft10f where '+
011000030916     C                                       'vftamf = '+%subst(MMAAAAFAT:3:4)+
011100030916     C                                       %subst(MMAAAAFAT:1:2)
011200030916     C*
011300030916     C/EXEC SQL
011400030916     C+ EXECUTE IMMEDIATE: SQLline
011500030916     C/END-EXEC
011600030916     C                   ENDIF
011700030808     C*
011800030808     C                   ENDSR
011900000929     C*------------------------------------------------------------------------*
012000030808     C* EXEVARIE - Routine di selezione record da TITAS00F+TITAS10F ed elaborazione VARIE
012100000929     C*------------------------------------------------------------------------*
012200030808     C     EXEVARIE      BEGSR
012300011206     C*
012400030808     C* Costrisco le date x i criteri di esclusione bolle da elaborare
012500030808     C                   Z-ADD     *zeros        wDataFatDa        8 0
012600030808     C                   Z-ADD     *zeros        wDataFatA         8 0
012700030808     C                   Z-ADD     *zeros        wDataFatFinMes    8 0
012800030915     C                   EVAL      wDataFatDa = AAAAFAT*10000+MMFAT*100+01
012900030808     C                   EVAL      wDataFatA  = AAAAFAT*10000+MMFAT*100+99
013000030808     C                   EVAL      wDataFatFinMes = AAAAFATFINMES*10000+
013100030808     C                                              MMFATFINMES*100+GGFATFINMES
013200030808     C*
013300030808     C/EXEC SQL
013400030808     C+ declare C1 cursor for
013500030808     C+ select * from titas00f where
013600031202     C+ ((tasdft between :wDataFatDa AND :wDataFatA AND tasfiv <> 0) OR
013700031202     C+  (tasdft = :wDataFatFinMes AND (tasfiv = :§QCFIV OR tasfiv = :§QCFI2 OR
013800031202     C+   tasfiv = 0)))
013900050405     C****+ AND tassv1 <> ' '
014000050503     C****+ AND tasimv <> 0
014100030808     C+ union
014200030808     C+ select * from titas10f where
014300031202     C+ ((tasdft between :wDataFatDa AND :wDataFatA AND tasfiv <> 0) OR
014400031202     C+  (tasdft = :wDataFatFinMes AND (tasfiv = :§QCFIV OR tasfiv = :§QCFI2 OR
014500031202     C+   tasfiv = 0)))
014600050405     C****+ AND tassv1 <> ' '
014700050503     C****+ AND tasimv <> 0
014800030808     C+ for read only
014900030808     C/END-EXEC
015000030808     C
015100030808     C/EXEC SQL
015200030808     C+ open C1
015300030808     C/END-EXEC
015400030808     C
015500030808     C/EXEC SQL
015600030808     C+ Fetch C1 into :DSTAS
015700030808     C/END-EXEC
015800030808     C*
015900030808     C                   dow       sqlcod = *zeros
016000030811     C                   exsr      chkREC
016100030811     C                   if        wrecok = 'S'
016200030808     C                   exsr      exeSVN
016300030811     C                   endif
016400030808     C*
016500030808     C/EXEC SQL
016600030808     C+ Fetch C1 into :DSTAS
016700030808     C/END-EXEC
016800030808     C*
016900030808     C                   enddo
017000030808     C*
017100030808     C/EXEC SQL
017200030808     C+ close C1
017300030808     C/END-EXEC
017400000929     C*
017500000929     C                   ENDSR
017600030811     C*------------------------------------------------------------------------*
017700030811     C* CHKREC - Routine di verifica validità record
017800030811     C*------------------------------------------------------------------------*
017900030811     C     CHKREC        BEGSR
018000030811     C*
018100030811     C                   MOVEL     'S'           wrecok            1
018200030811     C*
018300030811     C* Verifica se il tipo bolla è da considerare
018400030915     C     tasTBL        LOOKUP    SKTBL                                  55
018500030811     C                   IF        %found
018600030811     C                   ELSE
018700030811     C                   MOVEL     'N'           wrecok
018800030811     C                   ENDIF
018900030915     C*
019000030915     C* Verifica le bolle fatturate inizio mese, occorre considerare solo quelle aventi P.O. IVA div
019100030915     C* valori indicati in tabella QT.
019200030915     C                   IF        wrecok = 'S'
019300030915     C                   IF        tasdft = wDataFatDa AND
019400030915     C                             (tasfiv = §QCFIV OR tasfiv = §QCFI2)
019500030915     C                   MOVEL     'N'           wrecok
019600030915     C                   ENDIF
019700030915     C                   ENDIF
019800051227     C*
019900051227     C* Se richiesta elaborazione x verifica C.E.
020000051227     C                   IF        ELABORA_CE = 'S'
020100051227     C*
020200051227     C* Se richiesto nei parametri periodo di competenza x C.E. verifico data sped
020300051227     C                   IF        PERIODOCE <> *zeros
020400051227     C                   MOVEL     tasMGS        wMeseBolla        2 0
020500051227     C                   IF        tasAAS*100+wMeseBolla = PERIODOCE
020600051227     C                   ELSE
020700051227     C                   MOVEL     'N'           wrecok
020800051227     C                   ENDIF
020900051227     C                   ENDIF
021000051227     C*
021100051227     C* Se richiesto nei parametri flag d storno N/ACC x C.E. verifico
021200051227     C                   IF        STORNONACC = 'S'
021300051227     C                   IF        tasFA2 = 'S'
021400051227     C                   MOVEL     'N'           wrecok
021500051227     C                   ENDIF
021600051227     C                   ENDIF
021700051227     C*
021800051227     C                   ENDIF
021900030811     C*
022000030811     C                   ENDSR
022100030808     C*------------------------------------------------------------------------*
022200030808     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
022300030808     C*------------------------------------------------------------------------*
022400030808     C     EXESVN        BEGSR
022500030808     C*
022600030808     C* Inizializzo ad ogni bolla le variabili d appoggio relative a SIGLA e IMPORTO VARIE
022700030808     C                   MOVEL     *blanks       wSVN
022800030808     C                   Z-ADD     *zeros        wVAN
022900050315     C*
023000050315     C* Verifica ed eventuale scrittura del PORTO su TITAS
023100050315     C                   IF        tasPOR <> *zeros
023200050315     C                   EVAL      wSVN = *blanks
023300050315     C                   EVAL      wVAN = tasPOR
023400050315     C                   EXSR      WRISVN
023500050315     C                   ENDIF
023600051227     C*
023700051227     C* Verifica ed eventuale scrittura del totale imponibile (solo x lancio C.E.)
023800051227     C                   IF        ELABORA_CE = 'S'
023900051227     C                   IF        tasIMV <> *zeros
024000051227     C                   EVAL      wSVN = '['
024100051227     C                   EVAL      wVAN = tasIMV
024200051227     C                   EXSR      WRISVN
024300051227     C                   ENDIF
024400051227     C                   ENDIF
024500030808     C*
024600030808     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
024700030916     C                   IF        tasSV1 <> *blanks
024800030808     C                   EVAL      wSVN = tasSV1
024900030808     C                   EVAL      wVAN = tasVA1
025000030808     C                   EXSR      WRISVN
025100030808     C                   ENDIF
025200030808     C*
025300030808     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
025400030916     C                   IF        tasSV2 <> *blanks
025500030808     C                   EVAL      wSVN = tasSV2
025600030808     C                   EVAL      wVAN = tasVA2
025700030808     C                   EXSR      WRISVN
025800030808     C                   ENDIF
025900030808     C*
026000030808     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
026100030808     C                   IF        tasSV3 <> *blanks
026200030808     C                   EVAL      wSVN = tasSV3
026300030808     C                   EVAL      wVAN = tasVA3
026400030808     C                   EXSR      WRISVN
026500030808     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
026600030808     C     KEYta7        SETLL     tita730c
026700030808     C                   IF        %equal(tita730c)
026800030808     C     KEYta7        READE     tita730c
026900030808     C                   DOW       not %eof(tita730c)
027000030912     C                   if        ta7VAN > *zeros
027100030808     C                   EVAL      wSVN = ta7SVN
027200030808     C                   EVAL      wVAN = ta7VAN
027300030808     C                   EXSR      WRISVN
027400030904     C                   endif
027500030808     C     KEYta7        READE     tita730c
027600030808     C                   ENDDO
027700030808     C                   ENDIF
027800030808     C                   ENDIF
027900030808     C*
028000030808     C                   ENDSR
028100030808     C*------------------------------------------------------------------------*
028200030808     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
028300030808     C*------------------------------------------------------------------------*
028400030808     C     WRISVN        BEGSR
028500031111     C*
028600030916     C* Elaboro tutte le varie tranne la 8 (IVA)
028700030916     C                   IF        wSVN <> '8'
028800030808     C                   MOVEL     wDataFatDa    kvftAMF
028900030808     C*
029000030912     C* Inizializzo i buffer del record del wrk-file
029100030808     C                   CLEAR                   WFVFT000
029200030912     C                   CLEAR                   WFVFT010
029300030808     C*
029400030912     C* Scrivo il wrk-file "raggruppato" x periodo/varia
029500030808     C                   EVAL      kvftSVN = wSVN
029600051227 xxx C     KEYvft_1      CHAIN     WFVFT01L
029700030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
029800030808     C* incremneto il conteggio e l'importo della VARIA
029900030808     C                   IF        %found(WFVFT01L)
030000030808     C                   EVAL      vftVAN = vftVAN + wVAN
030100030808     C                   EVAL      vftCNT = vftCNT + 1
030200030808     C                   UPDATE    WFVFT000
030300030808     C                   ELSE
030400030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
030500030808     C* valorizzo il buffer del wrk-file e inserisco il record
030600030808     C                   EVAL      vftFPT = *blanks
030700030808     C                   EVAL      vftAMF = kvftAMF
030800030808     C                   EVAL      vftSVN = wSVN
030900030808     C                   EVAL      vftVAN = wVAN
031000030808     C                   EVAL      vftCNT = 1
031100030912     C* Reperisco la descrizione della VARIA
031200030808     C                   Z-ADD     1             i
031300030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
031400030808     C                   IF        %found
031500030808     C                   EVAL      vftDES = SKSVNDES(i)
031600030808     C                   ELSE
031700030808     C                   EVAL      vftDES = *all'*'
031800030808     C                   ENDIF
031900030808     C*
032000051227     C* A seconda del tipo elaborazinoe richiesta scrivo o il file "normale"
032100051227     C* o il file "ai fini C.E."
032200051227     C                   IF        ELABORA_CE = 'S'
032300051227     C                   EXSR      VALDSCE
032400051227     C                   WRITE     WFVFT0CE
032500051227     C                   ELSE
032600030808     C                   WRITE     WFVFT000
032700051227     C                   ENDIF
032800030808     C                   ENDIF
032900030808     C* Verifico se trattasi d bolla PT, se bolla POSTE rifaccio il medesimo giro precedente
033000030808     C* x incrementare/creare un "DI CUI" della VARIA corrente x le bolle PT
033100030808     C                   IF        tasLNP = 180
033200030808     C                   EVAL      kvftFPT = 'S'
033300051227 xxx C     KEYvft_2      CHAIN     WFVFT01L
033400030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
033500030808     C* incremneto il conteggio e l'importo della VARIA
033600030808     C                   IF        %found(WFVFT01L)
033700030808     C                   EVAL      vftVAN = vftVAN + wVAN
033800030808     C                   EVAL      vftCNT = vftCNT + 1
033900030808     C                   UPDATE    WFVFT000
034000030808     C                   ELSE
034100030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
034200030808     C* valorizzo il buffer del wrk-file e inserisco il record
034300030808     C                   EVAL      vftFPT = kvftFPT
034400030808     C                   EVAL      vftAMF = kvftAMF
034500030808     C                   EVAL      vftSVN = wSVN
034600030808     C                   EVAL      vftVAN = wVAN
034700030808     C                   EVAL      vftCNT = 1
034800030808     C* Reperisco la descricione della VARIA
034900030808     C                   Z-ADD     1             i
035000030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
035100030808     C                   IF        %found
035200030808     C                   EVAL      vftDES = SKSVNDES(i)
035300030808     C                   ELSE
035400030808     C                   EVAL      vftDES = *all'*'
035500030808     C                   ENDIF
035600051227     C*
035700051227     C* A seconda del tipo elaborazinoe richiesta scrivo o il file "normale"
035800051227     C* o il file "ai fini C.E."
035900051227     C                   IF        ELABORA_CE = 'S'
036000051227     C                   EXSR      VALDSCE
036100051227     C                   WRITE     WFVFT0CE
036200051227     C                   ELSE
036300030808     C                   WRITE     WFVFT000
036400051227     C                   ENDIF
036500030808     C                   ENDIF
036600030808     C                   ENDIF
036700030912     C*
036800031111     C* Se richiesto scrivo il wrk-file d dettaglio x key bolla/periodo/varia
036900030916     C                   IF        DETTAGLIO = 'S'
037000031111     C* Se richiesta solo 1 varia in particolare => gestisco
037100031111     C                   SETOFF                                       50
037200031111     C                   IF        SOLOVARIA <> *BLANKS
037300031111     C                   IF        wSVN <> SOLOVARIA
037400031111     C                   SETON                                        50
037500031111     C                   ENDIF
037600031111     C                   ENDIF
037700031111     C                   IF        *IN50 = *OFF
037800030912     C                   EVAL      vftAAS = tasAAS
037900030912     C                   EVAL      vftMGS = tasMGS
038000030912     C                   EVAL      vftLNP = tasLNP
038100030912     C                   EVAL      vftNRS = tasNRS
038200030912     C                   EVAL      vftNSP = tasNSP
038300030912     C                   EVAL      vftTBL = tasTBL
038400050513     C                   EVAL      vftKSC = tasKSC
038500050513     C                   EVAL      vftCTR = tasCTR
038600030912     C                   EVAL      vftFPT = *blanks
038700030912     C                   EVAL      vftAMF = kvftAMF
038800030912     C                   EVAL      vftSVN = wSVN
038900030912     C                   EVAL      vftVAN = wVAN
039000030912     C                   EVAL      vftCNT = 1
039100051227     C*
039200051227     C* A seconda del tipo elaborazinoe richiesta scrivo o il file "normale"
039300051227     C* o il file "ai fini C.E."
039400051227     C                   IF        ELABORA_CE = 'S'
039500051227     C                   EXSR      VALDSCE
039600051227     C                   WRITE     WFVFT1CE
039700051227     C                   ELSE
039800030912     C                   WRITE     WFVFT010
039900051227     C                   ENDIF
040000031111     C                   ENDIF
040100030916     C                   ENDIF
040200030916     C                   ENDIF
040300030808     C*
040400030808     C                   ENDSR
040500051227     C*------------------------------------------------------------------------*
040600051227     C* VALDSCE - VALORIZZAZIONE CAMPI FILE "CE"
040700051227     C*------------------------------------------------------------------------*
040800051227     C     VALDSCE       BEGSR
040900051227     C*
041000051227     C                   CLEAR                   wfvft0ce
041100051227     C                   CLEAR                   wfvft1ce
041200051227     C                   EVAL      ce_VFTAAS = VFTAAS
041300051227     C                   EVAL      ce_VFTMGS = VFTMGS
041400051227     C                   EVAL      ce_VFTLNP = VFTLNP
041500051227     C                   EVAL      ce_VFTNRS = VFTNRS
041600051227     C                   EVAL      ce_VFTNSP = VFTNSP
041700051227     C                   EVAL      ce_VFTTBL = VFTTBL
041800051227     C                   EVAL      ce_VFTFPT = VFTFPT
041900051227     C                   EVAL      ce_VFTAMF = VFTAMF
042000051227     C                   EVAL      ce_VFTSVN = VFTSVN
042100051227     C                   EVAL      ce_VFTDES = VFTDES
042200051227     C                   EVAL      ce_VFTCNT = VFTCNT
042300051227     C                   EVAL      ce_VFTVAN = VFTVAN
042400051227     C*
042500051227     C                   ENDSR
042600011205     C*------------------------------------------------------------------------*
042700030808     C* CARTAB - CARICA I DATI TABELLATI
042800011205     C*------------------------------------------------------------------------*
042900030808     C     CARTAB        BEGSR
043000030811     C*
043100030811     C* Reperisco i tipi bolla validi come spedizioni
043200030811     C                   Z-ADD     *zeros        i
043300030811     C                   Z-ADD     1             tblKUT
043400030811     C                   MOVEL(P)  'TB'          tblCOD
043500030811     C     KEYtab_1      SETLL     TABEL00F
043600030811     C                   IF        %equal(TABEL00F)
043700030811     C     KEYtab_1      READE     TABEL00F
043800030811     C                   DOW       not %eof(TABEL00F)
043900030811     C                   MOVEL(P)  TBLUNI        DSTB
044000030811     C                   IF        §TBFCB = '1'
044100030811     C                   ADD       1             i
044200030811     C                   MOVEL(P)  TBLKEY        SKTBL(i)
044300030811     C                   ENDIF
044400030811     C     KEYtab_1      READE     TABEL00F
044500030811     C                   ENDDO
044600030811     C                   ENDIF
044700030808     C*
044800030808     C* Reperisco le descrizioni delle varie
044900030811     C                   Z-ADD     *zeros        i
045000030808     C                   Z-ADD     1             tblKUT
045100030808     C                   MOVEL(P)  'CC'          tblCOD
045200030808     C     KEYtab_1      SETLL     TABEL00F
045300030808     C                   IF        %equal(TABEL00F)
045400030808     C     KEYtab_1      READE     TABEL00F
045500030808     C                   DOW       not %eof(TABEL00F)
045600030808     C                   ADD       1             i
045700030808     C                   MOVE(P)   TBLKEY        SKSVN(i)
045800030808     C                   MOVEL(P)  TBLUNI        DSCC
045900030808     C                   MOVEL(P)  §CCDES        SKSVNDES(i)
046000030808     C     KEYtab_1      READE     TABEL00F
046100030808     C                   ENDDO
046200030808     C                   ENDIF
046300030808     C*
046400030808     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
046500030808     C                   CLEAR                   DSQC2
046600030808     C                   Z-ADD     1             tblKUT
046700030808     C                   MOVEL(P)  'QC'          tblCOD
046800030808     C                   MOVEL(P)  '2'           tblKEY
046900030808     C     KEYtab_2      CHAIN     TABEL00F
047000030808     C                   IF        %found(TABEL00F)
047100030808     C                   MOVEL(P)  TBLUNI        DSQC2
047200030808     C                   ENDIF
047300011206     C*
047400011205     C                   ENDSR
047500970526     C*------------------------------------------------------------------------*
047600970526     C* *INZSR - OPERAZIONI INIZIALI
047700970526     C*------------------------------------------------------------------------*
047800970526     C     *INZSR        BEGSR
047900970526     C*---
048000970526     C* RICEVIMENTO PARAMETRI
048100970526     C*---
048200970526     C     *ENTRY        PLIST
048300970612     C                   PARM                    KPJBA
048400991214     C                   MOVEL     KPJBU         PARAM
048500051227     C*
048600051227     C* EFFETTUO SUBITO ALCUNE CONSIDERAZIONI IN RELAZIONE AI PARAMETRI IN INGRESSO
048700051227     C                   IF        ELABORA_CE = 'S'
048800051227     C                   SETON                                        65
048900051227     C                   ELSE
049000051227     C                   SETOFF                                       65
049100051227     C                   ENDIF
049200970526     C*---
049300991214     C* CHIAVI DI LETTURA
049400991214     C*---
049500000810     C*
049600030808     C* LETTURA tabel00f - PARZIALE
049700030808     C     KEYtab_1      KLIST
049800030808     C                   KFLD                    tblKUT                         *UTENTE
049900030808     C                   KFLD                    tblCOD                         *TABELLA
050000030808     C*
050100030808     C* LETTURA tabel00f - COMPLETA
050200030808     C     KEYtab_2      KLIST
050300030808     C                   KFLD                    tblKUT                         *UTENTE
050400030808     C                   KFLD                    tblCOD                         *TABELLA
050500030808     C                   KFLD                    tblKEY                         *CHIAVE
050600030808     C*
050700030808     C* LETTURA wfvft01l - COMPLETA
050800030808     C     KEYvft_1      KLIST
050900030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
051000030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
051100030808     C*
051200030808     C* LETTURA wfvft01l - COMPLETA
051300030808     C     KEYvft_2      KLIST
051400030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
051500030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
051600030808     C                   KFLD                    kvftFPT                        *FLAG POSTE
051700030808     C*
051800030808     C* LETTURA tita730c - PARZIALE
051900030808     C     KEYta7        KLIST
052000030808     C                   KFLD                    tasAAS                         *ANNO BOLLA
052100030808     C                   KFLD                    tasLNP                         *LINEA PARTENZA BOLL
052200030808     C                   KFLD                    tasNRS                         *SERIE BOLLA
052300030808     C                   KFLD                    tasNSP                         *NUMERO SPEDIZIONE B
052400030808     C                   KFLD                    tasTBL                         *TIPO BOLLA
052500991214     C*
052600970526     C                   ENDSR
