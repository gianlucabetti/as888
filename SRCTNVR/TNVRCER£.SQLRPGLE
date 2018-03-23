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
001100000810     Ftabel00f  IF   E           K DISK
001200030808     Ftita730c  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400030808     D* SCHIERE
001500030808     D*------------------------------------------------------------------------*
001600030808     D*------------
001700030808     D* SCHIERE SIGLE E DESCRIZIONI VARIE
001800030808     D*------------
001900030808     D SKSVN           S              1    DIM(100)
002000030808     D SKSVNDES        S             20    DIM(100)
002100030811     D*------------
002200030811     D* SCHIERE TIPI BOLLA
002300030811     D*------------
002400030811     D SKTBL           S              2    DIM(100)
002500991214     D*------------
002600970526     D* ARCHITETTURA
002700970526     D*------------
002800970526     D KPJBA         E DS
002900030811     D*------------
003000030811     D* DS RIDEFINIZIONE TABELLA "TB" (TIPI BOLLA)
003100030811     D*------------
003200030811     D DSTB          E DS
003300030808     D*------------
003400030808     D* DS RIDEFINIZIONE TABELLA "CC" (VARIE)
003500030808     D*------------
003600030808     D DSCC          E DS
003700030808     D*------------
003800030808     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
003900030808     D*------------
004000030808     D DSQC2         E DS
004100030808     D*------------------
004200030808     D* DS X PASSAGGIO PARAMETRI
004300030808     D*------------------
004400030808     D PARAM           DS
004500030808     D  MMAAAAFAT              1      6
004600030808     D  MMFAT                  1      2  0
004700030808     D  AAAAFAT                3      6  0
004800030808     D  DATAFATFINMES          7     14  0
004900030808     D  GGFATFINMES            7      8  0
005000030808     D  MMFATFINMES            9     10  0
005100030808     D  AAAAFATFINMES         11     14  0
005200030916     D  DETTAGLIO             15     15
005300031111     D  SOLOVARIA             16     16
005400030808     D*------------------
005500030808     D* DS LETTURA RECORDSET SQL SU FORMATO "TITAS"
005600030808     D*------------------
005700030808     D  DSTAS        E DS                  extname(TITAS00F)
005800011206     D*-------------------
005900011206     D* VARIABILI DI WRK
006000011206     D*-------------------
006100011206     D  SQLline        s           5000    varying
006200030808     D  i              s              3  0 inz(*zeros)
006300030808     D  wSVN           s                   like(vftSVN)
006400030808     D  wVAN           s                   like(vftVAN)
006500030808     D  kvftAMF        s                   like(vftAMF)
006600030808     D  kvftSVN        s                   like(vftSVN)
006700030808     D  kvftFPT        s                   like(vftFPT)
006800970526     C*------------------------------------------------------------------------*
006900991214     C* MAIN LINES
007000970526     C*------------------------------------------------------------------------*
007100030808     C*
007200030808     C* ESEGUE LA ROUTINE PRINCIPALE
007300991214     C                   EXSR      ESEGUI
007400000306     C*
007500970526     C                   SETON                                        LR
007600970526     C*------------------------------------------------------------------------*
007700991217     C* ESEGUI - OPERAZIONI PRINCIPALI
007800970526     C*------------------------------------------------------------------------*
007900991214     C     ESEGUI        BEGSR
008000011205     C*
008100030808     C                   exsr      dltWVFT
008200030808     C                   exsr      carTAB
008300030808     C                   exsr      exeVARIE
008400970526     C*
008500970526     C                   ENDSR
008600030808     C*------------------------------------------------------------------------*
008700031111     C* DLTWVFT - Routine di eliminazione record wrk-file dello stesso periodo di fatturazione
008800030808     C*------------------------------------------------------------------------*
008900030808     C     DLTWVFT       BEGSR
009000030808     C*
009100030808     C                   EVAL      SQLline = 'delete from wfvft00f where '+
009200030808     C                                       'vftamf = '+%subst(MMAAAAFAT:3:4)+
009300030808     C                                       %subst(MMAAAAFAT:1:2)
009400030808     C*
009500030808     C/EXEC SQL
009600030808     C+ EXECUTE IMMEDIATE: SQLline
009700030808     C/END-EXEC
009800030916     C*
009900030916     C* Se lancio anche x dettaglio elimino il contenuto x periodo d fatturazione anche
010000030916     C* del file dettaglio WFVFT10F
010100030916     C                   IF        DETTAGLIO = 'S'
010200030916     C                   EVAL      SQLline = 'delete from wfvft10f where '+
010300030916     C                                       'vftamf = '+%subst(MMAAAAFAT:3:4)+
010400030916     C                                       %subst(MMAAAAFAT:1:2)
010500030916     C*
010600030916     C/EXEC SQL
010700030916     C+ EXECUTE IMMEDIATE: SQLline
010800030916     C/END-EXEC
010900030916     C                   ENDIF
011000030808     C*
011100030808     C                   ENDSR
011200000929     C*------------------------------------------------------------------------*
011300030808     C* EXEVARIE - Routine di selezione record da TITAS00F+TITAS10F ed elaborazione VARIE
011400000929     C*------------------------------------------------------------------------*
011500030808     C     EXEVARIE      BEGSR
011600011206     C*
011700030808     C* Costrisco le date x i criteri di esclusione bolle da elaborare
011800030808     C                   Z-ADD     *zeros        wDataFatDa        8 0
011900030808     C                   Z-ADD     *zeros        wDataFatA         8 0
012000030808     C                   Z-ADD     *zeros        wDataFatFinMes    8 0
012100030915     C                   EVAL      wDataFatDa = AAAAFAT*10000+MMFAT*100+01
012200030808     C                   EVAL      wDataFatA  = AAAAFAT*10000+MMFAT*100+99
012300030808     C                   EVAL      wDataFatFinMes = AAAAFATFINMES*10000+
012400030808     C                                              MMFATFINMES*100+GGFATFINMES
012500030808     C*
012600030808     C/EXEC SQL
012700030808     C+ declare C1 cursor for
012800030808     C+ select * from titas00f where
012900031202     C+ ((tasdft between :wDataFatDa AND :wDataFatA AND tasfiv <> 0) OR
013000031202     C+  (tasdft = :wDataFatFinMes AND (tasfiv = :§QCFIV OR tasfiv = :§QCFI2 OR
013100031202     C+   tasfiv = 0)))
013200030808     C+ AND tassv1 <> ' '
013300030808     C+ union
013400030808     C+ select * from titas10f where
013500031202     C+ ((tasdft between :wDataFatDa AND :wDataFatA AND tasfiv <> 0) OR
013600031202     C+  (tasdft = :wDataFatFinMes AND (tasfiv = :§QCFIV OR tasfiv = :§QCFI2 OR
013700031202     C+   tasfiv = 0)))
013800030808     C+ AND tassv1 <> ' '
013900030808     C+ for read only
014000030808     C/END-EXEC
014100030808     C
014200030808     C/EXEC SQL
014300030808     C+ open C1
014400030808     C/END-EXEC
014500030808     C
014600030808     C/EXEC SQL
014700030808     C+ Fetch C1 into :DSTAS
014800030808     C/END-EXEC
014900030808     C*
015000030808     C                   dow       sqlcod = *zeros
015100030811     C                   exsr      chkREC
015200030811     C                   if        wrecok = 'S'
015300030808     C                   exsr      exeSVN
015400030811     C                   endif
015500030808     C*
015600030808     C/EXEC SQL
015700030808     C+ Fetch C1 into :DSTAS
015800030808     C/END-EXEC
015900030808     C*
016000030808     C                   enddo
016100030808     C*
016200030808     C/EXEC SQL
016300030808     C+ close C1
016400030808     C/END-EXEC
016500000929     C*
016600000929     C                   ENDSR
016700030811     C*------------------------------------------------------------------------*
016800030811     C* CHKREC - Routine di verifica validità record
016900030811     C*------------------------------------------------------------------------*
017000030811     C     CHKREC        BEGSR
017100030811     C*
017200030811     C                   MOVEL     'S'           wrecok            1
017300030811     C*
017400030811     C* Verifica se il tipo bolla è da considerare
017500030915     C     tasTBL        LOOKUP    SKTBL                                  55
017600030811     C                   IF        %found
017700030811     C                   ELSE
017800030811     C                   MOVEL     'N'           wrecok
017900030811     C                   ENDIF
018000030915     C*
018100030915     C* Verifica le bolle fatturate inizio mese, occorre considerare solo quelle aventi P.O. IVA div
018200030915     C* valori indicati in tabella QT.
018300030915     C                   IF        wrecok = 'S'
018400030915     C                   IF        tasdft = wDataFatDa AND
018500030915     C                             (tasfiv = §QCFIV OR tasfiv = §QCFI2)
018600030915     C                   MOVEL     'N'           wrecok
018700030915     C                   ENDIF
018800030915     C                   ENDIF
018900030811     C*
019000030811     C                   ENDSR
019100030808     C*------------------------------------------------------------------------*
019200030808     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
019300030808     C*------------------------------------------------------------------------*
019400030808     C     EXESVN        BEGSR
019500030808     C*
019600030808     C* Inizializzo ad ogni bolla le variabili d appoggio relative a SIGLA e IMPORTO VARIE
019700030808     C                   MOVEL     *blanks       wSVN
019800030808     C                   Z-ADD     *zeros        wVAN
019900050315     C*
020000050315     C* Verifica ed eventuale scrittura del PORTO su TITAS
020100050315     C                   IF        tasPOR <> *zeros
020200050315     C                   EVAL      wSVN = *blanks
020300050315     C                   EVAL      wVAN = tasPOR
020400050315     C                   EXSR      WRISVN
020500050315     C                   ENDIF
020600030808     C*
020700030808     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
020800030916     C                   IF        tasSV1 <> *blanks
020900030808     C                   EVAL      wSVN = tasSV1
021000030808     C                   EVAL      wVAN = tasVA1
021100030808     C                   EXSR      WRISVN
021200030808     C                   ENDIF
021300030808     C*
021400030808     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
021500030916     C                   IF        tasSV2 <> *blanks
021600030808     C                   EVAL      wSVN = tasSV2
021700030808     C                   EVAL      wVAN = tasVA2
021800030808     C                   EXSR      WRISVN
021900030808     C                   ENDIF
022000030808     C*
022100030808     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
022200030808     C                   IF        tasSV3 <> *blanks
022300030808     C                   EVAL      wSVN = tasSV3
022400030808     C                   EVAL      wVAN = tasVA3
022500030808     C                   EXSR      WRISVN
022600030808     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
022700030808     C     KEYta7        SETLL     tita730c
022800030808     C                   IF        %equal(tita730c)
022900030808     C     KEYta7        READE     tita730c
023000030808     C                   DOW       not %eof(tita730c)
023100030912     C                   if        ta7VAN > *zeros
023200030808     C                   EVAL      wSVN = ta7SVN
023300030808     C                   EVAL      wVAN = ta7VAN
023400030808     C                   EXSR      WRISVN
023500030904     C                   endif
023600030808     C     KEYta7        READE     tita730c
023700030808     C                   ENDDO
023800030808     C                   ENDIF
023900030808     C                   ENDIF
024000030808     C*
024100030808     C                   ENDSR
024200030808     C*------------------------------------------------------------------------*
024300030808     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
024400030808     C*------------------------------------------------------------------------*
024500030808     C     WRISVN        BEGSR
024600031111     C*
024700030916     C* Elaboro tutte le varie tranne la 8 (IVA)
024800030916     C                   IF        wSVN <> '8'
024900030808     C                   MOVEL     wDataFatDa    kvftAMF
025000030808     C*
025100030912     C* Inizializzo i buffer del record del wrk-file
025200030808     C                   CLEAR                   WFVFT000
025300030912     C                   CLEAR                   WFVFT010
025400030808     C*
025500030912     C* Scrivo il wrk-file "raggruppato" x periodo/varia
025600030808     C                   EVAL      kvftSVN = wSVN
025700030808     C     KEYvft_1      CHAIN     WFVFT01L
025800030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
025900030808     C* incremneto il conteggio e l'importo della VARIA
026000030808     C                   IF        %found(WFVFT01L)
026100030808     C                   EVAL      vftVAN = vftVAN + wVAN
026200030808     C                   EVAL      vftCNT = vftCNT + 1
026300030808     C                   UPDATE    WFVFT000
026400030808     C                   ELSE
026500030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
026600030808     C* valorizzo il buffer del wrk-file e inserisco il record
026700030808     C                   EVAL      vftFPT = *blanks
026800030808     C                   EVAL      vftAMF = kvftAMF
026900030808     C                   EVAL      vftSVN = wSVN
027000030808     C                   EVAL      vftVAN = wVAN
027100030808     C                   EVAL      vftCNT = 1
027200030912     C* Reperisco la descrizione della VARIA
027300030808     C                   Z-ADD     1             i
027400030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
027500030808     C                   IF        %found
027600030808     C                   EVAL      vftDES = SKSVNDES(i)
027700030808     C                   ELSE
027800030808     C                   EVAL      vftDES = *all'*'
027900030808     C                   ENDIF
028000030808     C*
028100030808     C                   WRITE     WFVFT000
028200030808     C                   ENDIF
028300030808     C* Verifico se trattasi d bolla PT, se bolla POSTE rifaccio il medesimo giro precedente
028400030808     C* x incrementare/creare un "DI CUI" della VARIA corrente x le bolle PT
028500030808     C                   IF        tasLNP = 180
028600030808     C                   EVAL      kvftFPT = 'S'
028700030808     C     KEYvft_2      CHAIN     WFVFT01L
028800030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
028900030808     C* incremneto il conteggio e l'importo della VARIA
029000030808     C                   IF        %found(WFVFT01L)
029100030808     C                   EVAL      vftVAN = vftVAN + wVAN
029200030808     C                   EVAL      vftCNT = vftCNT + 1
029300030808     C                   UPDATE    WFVFT000
029400030808     C                   ELSE
029500030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
029600030808     C* valorizzo il buffer del wrk-file e inserisco il record
029700030808     C                   EVAL      vftFPT = kvftFPT
029800030808     C                   EVAL      vftAMF = kvftAMF
029900030808     C                   EVAL      vftSVN = wSVN
030000030808     C                   EVAL      vftVAN = wVAN
030100030808     C                   EVAL      vftCNT = 1
030200030808     C* Reperisco la descricione della VARIA
030300030808     C                   Z-ADD     1             i
030400030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
030500030808     C                   IF        %found
030600030808     C                   EVAL      vftDES = SKSVNDES(i)
030700030808     C                   ELSE
030800030808     C                   EVAL      vftDES = *all'*'
030900030808     C                   ENDIF
031000030808     C*
031100030808     C                   WRITE     WFVFT000
031200030808     C                   ENDIF
031300030808     C                   ENDIF
031400030912     C*
031500031111     C* Se richiesto scrivo il wrk-file d dettaglio x key bolla/periodo/varia
031600030916     C                   IF        DETTAGLIO = 'S'
031700031111     C* Se richiesta solo 1 varia in particolare => gestisco
031800031111     C                   SETOFF                                       50
031900031111     C                   IF        SOLOVARIA <> *BLANKS
032000031111     C                   IF        wSVN <> SOLOVARIA
032100031111     C                   SETON                                        50
032200031111     C                   ENDIF
032300031111     C                   ENDIF
032400031111     C                   IF        *IN50 = *OFF
032500030912     C                   EVAL      vftAAS = tasAAS
032600030912     C                   EVAL      vftMGS = tasMGS
032700030912     C                   EVAL      vftLNP = tasLNP
032800030912     C                   EVAL      vftNRS = tasNRS
032900030912     C                   EVAL      vftNSP = tasNSP
033000030912     C                   EVAL      vftTBL = tasTBL
033100030912     C                   EVAL      vftFPT = *blanks
033200030912     C                   EVAL      vftAMF = kvftAMF
033300030912     C                   EVAL      vftSVN = wSVN
033400030912     C                   EVAL      vftVAN = wVAN
033500030912     C                   EVAL      vftCNT = 1
033600030912     C                   WRITE     WFVFT010
033700031111     C                   ENDIF
033800030916     C                   ENDIF
033900030916     C                   ENDIF
034000030808     C*
034100030808     C                   ENDSR
034200011205     C*------------------------------------------------------------------------*
034300030808     C* CARTAB - CARICA I DATI TABELLATI
034400011205     C*------------------------------------------------------------------------*
034500030808     C     CARTAB        BEGSR
034600030811     C*
034700030811     C* Reperisco i tipi bolla validi come spedizioni
034800030811     C                   Z-ADD     *zeros        i
034900030811     C                   Z-ADD     1             tblKUT
035000030811     C                   MOVEL(P)  'TB'          tblCOD
035100030811     C     KEYtab_1      SETLL     TABEL00F
035200030811     C                   IF        %equal(TABEL00F)
035300030811     C     KEYtab_1      READE     TABEL00F
035400030811     C                   DOW       not %eof(TABEL00F)
035500030811     C                   MOVEL(P)  TBLUNI        DSTB
035600030811     C                   IF        §TBFCB = '1'
035700030811     C                   ADD       1             i
035800030811     C                   MOVEL(P)  TBLKEY        SKTBL(i)
035900030811     C                   ENDIF
036000030811     C     KEYtab_1      READE     TABEL00F
036100030811     C                   ENDDO
036200030811     C                   ENDIF
036300030808     C*
036400030808     C* Reperisco le descrizioni delle varie
036500030811     C                   Z-ADD     *zeros        i
036600030808     C                   Z-ADD     1             tblKUT
036700030808     C                   MOVEL(P)  'CC'          tblCOD
036800030808     C     KEYtab_1      SETLL     TABEL00F
036900030808     C                   IF        %equal(TABEL00F)
037000030808     C     KEYtab_1      READE     TABEL00F
037100030808     C                   DOW       not %eof(TABEL00F)
037200030808     C                   ADD       1             i
037300030808     C                   MOVE(P)   TBLKEY        SKSVN(i)
037400030808     C                   MOVEL(P)  TBLUNI        DSCC
037500030808     C                   MOVEL(P)  §CCDES        SKSVNDES(i)
037600030808     C     KEYtab_1      READE     TABEL00F
037700030808     C                   ENDDO
037800030808     C                   ENDIF
037900030808     C*
038000030808     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
038100030808     C                   CLEAR                   DSQC2
038200030808     C                   Z-ADD     1             tblKUT
038300030808     C                   MOVEL(P)  'QC'          tblCOD
038400030808     C                   MOVEL(P)  '2'           tblKEY
038500030808     C     KEYtab_2      CHAIN     TABEL00F
038600030808     C                   IF        %found(TABEL00F)
038700030808     C                   MOVEL(P)  TBLUNI        DSQC2
038800030808     C                   ENDIF
038900011206     C*
039000011205     C                   ENDSR
039100970526     C*------------------------------------------------------------------------*
039200970526     C* *INZSR - OPERAZIONI INIZIALI
039300970526     C*------------------------------------------------------------------------*
039400970526     C     *INZSR        BEGSR
039500970526     C*---
039600970526     C* RICEVIMENTO PARAMETRI
039700970526     C*---
039800970526     C     *ENTRY        PLIST
039900970612     C                   PARM                    KPJBA
040000991214     C                   MOVEL     KPJBU         PARAM
040100970526     C*---
040200991214     C* CHIAVI DI LETTURA
040300991214     C*---
040400000810     C*
040500030808     C* LETTURA tabel00f - PARZIALE
040600030808     C     KEYtab_1      KLIST
040700030808     C                   KFLD                    tblKUT                         *UTENTE
040800030808     C                   KFLD                    tblCOD                         *TABELLA
040900030808     C*
041000030808     C* LETTURA tabel00f - COMPLETA
041100030808     C     KEYtab_2      KLIST
041200030808     C                   KFLD                    tblKUT                         *UTENTE
041300030808     C                   KFLD                    tblCOD                         *TABELLA
041400030808     C                   KFLD                    tblKEY                         *CHIAVE
041500030808     C*
041600030808     C* LETTURA wfvft01l - COMPLETA
041700030808     C     KEYvft_1      KLIST
041800030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
041900030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
042000030808     C*
042100030808     C* LETTURA wfvft01l - COMPLETA
042200030808     C     KEYvft_2      KLIST
042300030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
042400030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
042500030808     C                   KFLD                    kvftFPT                        *FLAG POSTE
042600030808     C*
042700030808     C* LETTURA tita730c - PARZIALE
042800030808     C     KEYta7        KLIST
042900030808     C                   KFLD                    tasAAS                         *ANNO BOLLA
043000030808     C                   KFLD                    tasLNP                         *LINEA PARTENZA BOLL
043100030808     C                   KFLD                    tasNRS                         *SERIE BOLLA
043200030808     C                   KFLD                    tasNSP                         *NUMERO SPEDIZIONE B
043300030808     C                   KFLD                    tasTBL                         *TIPO BOLLA
043400991214     C*
043500970526     C                   ENDSR
