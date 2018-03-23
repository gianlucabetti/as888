000100010925      *PARMS DYNUSRPRF(*OWNER)
000200970526     H*------------------------------------------------------------------------*
000300030808     H* Creazione wrk-file VARIE fatturate
000400970526     H*------------------------------------------------------------------------*
000500000920     H DECEDIT('0,') DATEDIT(*DMY.)
000600970526     F*------------------------------------------------------------------------*
000700970526     F* DATA BASE
000800970526     F*------------------------------------------------------------------------*
000900030808     Fwfvft01l  UF A E           K DISK
001000000810     Ftabel00f  IF   E           K DISK
001100030808     Ftita730c  IF   E           K DISK
001200970526     D*------------------------------------------------------------------------*
001300030808     D* SCHIERE
001400030808     D*------------------------------------------------------------------------*
001500030808     D*------------
001600030808     D* SCHIERE SIGLE E DESCRIZIONI VARIE
001700030808     D*------------
001800030808     D SKSVN           S              1    DIM(100)
001900030808     D SKSVNDES        S             20    DIM(100)
002000030811     D*------------
002100030811     D* SCHIERE TIPI BOLLA
002200030811     D*------------
002300030811     D SKTBL           S              2    DIM(100)
002400991214     D*------------
002500970526     D* ARCHITETTURA
002600970526     D*------------
002700970526     D KPJBA         E DS
002800030811     D*------------
002900030811     D* DS RIDEFINIZIONE TABELLA "TB" (TIPI BOLLA)
003000030811     D*------------
003100030811     D DSTB          E DS
003200030808     D*------------
003300030808     D* DS RIDEFINIZIONE TABELLA "CC" (VARIE)
003400030808     D*------------
003500030808     D DSCC          E DS
003600030808     D*------------
003700030808     D* DS RIDEFINIZIONE TABELLA "QC" TIPO RECORD "2"
003800030808     D*------------
003900030808     D DSQC2         E DS
004000030808     D*------------------
004100030808     D* DS X PASSAGGIO PARAMETRI
004200030808     D*------------------
004300030808     D PARAM           DS
004400030808     D  MMAAAAFAT              1      6
004500030808     D  MMFAT                  1      2  0
004600030808     D  AAAAFAT                3      6  0
004700030808     D  DATAFATFINMES          7     14  0
004800030808     D  GGFATFINMES            7      8  0
004900030808     D  MMFATFINMES            9     10  0
005000030808     D  AAAAFATFINMES         11     14  0
005100030808     D*------------------
005200030808     D* DS LETTURA RECORDSET SQL SU FORMATO "TITAS"
005300030808     D*------------------
005400030808     D  DSTAS        E DS                  extname(TITAS00F)
005500011206     D*-------------------
005600011206     D* VARIABILI DI WRK
005700011206     D*-------------------
005800011206     D  SQLline        s           5000    varying
005900030808     D  i              s              3  0 inz(*zeros)
006000030808     D  wSVN           s                   like(vftSVN)
006100030808     D  wVAN           s                   like(vftVAN)
006200030808     D  kvftAMF        s                   like(vftAMF)
006300030808     D  kvftSVN        s                   like(vftSVN)
006400030808     D  kvftFPT        s                   like(vftFPT)
006500970526     C*------------------------------------------------------------------------*
006600991214     C* MAIN LINES
006700970526     C*------------------------------------------------------------------------*
006800030808     C*
006900030808     C* ESEGUE LA ROUTINE PRINCIPALE
007000991214     C                   EXSR      ESEGUI
007100000306     C*
007200970526     C                   SETON                                        LR
007300970526     C*------------------------------------------------------------------------*
007400991217     C* ESEGUI - OPERAZIONI PRINCIPALI
007500970526     C*------------------------------------------------------------------------*
007600991214     C     ESEGUI        BEGSR
007700011205     C*
007800030808     C                   exsr      dltWVFT
007900030808     C                   exsr      carTAB
008000030808     C                   exsr      exeVARIE
008100970526     C*
008200970526     C                   ENDSR
008300030808     C*------------------------------------------------------------------------*
008400030808     C* DLTWVFT - Routine di eliminazione record wrk-file dello stesso periodo di fatturazione richi
008500030808     C*------------------------------------------------------------------------*
008600030808     C     DLTWVFT       BEGSR
008700030808     C*
008800030808     C                   EVAL      SQLline = 'delete from wfvft00f where '+
008900030808     C                                       'vftamf = '+%subst(MMAAAAFAT:3:4)+
009000030808     C                                       %subst(MMAAAAFAT:1:2)
009100030808     C*
009200030808     C/EXEC SQL
009300030808     C+ EXECUTE IMMEDIATE: SQLline
009400030808     C/END-EXEC
009500030808     C*
009600030808     C                   ENDSR
009700000929     C*------------------------------------------------------------------------*
009800030808     C* EXEVARIE - Routine di selezione record da TITAS00F+TITAS10F ed elaborazione VARIE
009900000929     C*------------------------------------------------------------------------*
010000030808     C     EXEVARIE      BEGSR
010100011206     C*
010200030808     C* Costrisco le date x i criteri di esclusione bolle da elaborare
010300030808     C                   Z-ADD     *zeros        wDataFatDa        8 0
010400030808     C                   Z-ADD     *zeros        wDataFatA         8 0
010500030808     C                   Z-ADD     *zeros        wDataFatFinMes    8 0
010600030808     C                   EVAL      wDataFatDa = AAAAFAT*10000+MMFAT*100
010700030808     C                   EVAL      wDataFatA  = AAAAFAT*10000+MMFAT*100+99
010800030808     C                   EVAL      wDataFatFinMes = AAAAFATFINMES*10000+
010900030808     C                                              MMFATFINMES*100+GGFATFINMES
011000030808     C*
011100030808     C/EXEC SQL
011200030808     C+ declare C1 cursor for
011300030808     C+ select * from titas00f where
011400030911     C+ tasaas*10000+tasmgs between :wDataFatDa AND :wDataFatA
011500030911     C+ AND tasdft > 0
011600030808     C+ for read only
011700030808     C/END-EXEC
011800030808     C
011900030808     C/EXEC SQL
012000030808     C+ open C1
012100030808     C/END-EXEC
012200030808     C
012300030808     C/EXEC SQL
012400030808     C+ Fetch C1 into :DSTAS
012500030808     C/END-EXEC
012600030808     C*
012700030808     C                   dow       sqlcod = *zeros
012800030811     C                   exsr      chkREC
012900030811     C                   if        wrecok = 'S'
013000030808     C                   exsr      exeSVN
013100030811     C                   endif
013200030808     C*
013300030808     C/EXEC SQL
013400030808     C+ Fetch C1 into :DSTAS
013500030808     C/END-EXEC
013600030808     C*
013700030808     C                   enddo
013800030808     C*
013900030808     C/EXEC SQL
014000030808     C+ close C1
014100030808     C/END-EXEC
014200000929     C*
014300000929     C                   ENDSR
014400030811     C*------------------------------------------------------------------------*
014500030811     C* CHKREC - Routine di verifica validità record
014600030811     C*------------------------------------------------------------------------*
014700030811     C     CHKREC        BEGSR
014800030811     C*
014900030811     C                   MOVEL     'S'           wrecok            1
015000030811     C*
015100030811     C                   ENDSR
015200030808     C*------------------------------------------------------------------------*
015300030808     C* EXESVN - Routine di valorizzazione wrk-file VARIE fatturate
015400030808     C*------------------------------------------------------------------------*
015500030808     C     EXESVN        BEGSR
015600030808     C*
015700030808     C* Inizializzo ad ogni bolla le variabili d appoggio relative a SIGLA e IMPORTO VARIE
015800030808     C                   MOVEL     *blanks       wSVN
015900030808     C                   Z-ADD     *zeros        wVAN
016000030808     C*
016100030808     C* Verifica ed eventuale scrittura 1° VARIA su TITAS
016200030904     C                   IF        tasSV1 <> *blanks  and
016300030904     C                             tasVA1 > *zeros
016400030808     C                   EVAL      wSVN = tasSV1
016500030808     C                   EVAL      wVAN = tasVA1
016600030808     C                   EXSR      WRISVN
016700030808     C                   ENDIF
016800030808     C*
016900030808     C* Verifica ed eventuale scrittura 2° VARIA su TITAS
017000030904     C                   IF        tasSV2 <> *blanks  and
017100030904     C                             tasVA2 > *zeros
017200030808     C                   EVAL      wSVN = tasSV2
017300030808     C                   EVAL      wVAN = tasVA2
017400030808     C                   EXSR      WRISVN
017500030808     C                   ENDIF
017600030808     C*
017700030808     C* Verifica ed eventuale scrittura 3° VARIA su TITAS
017800030808     C                   IF        tasSV3 <> *blanks
017900030904     C                   if        tasVA3 > *zeros
018000030808     C                   EVAL      wSVN = tasSV3
018100030808     C                   EVAL      wVAN = tasVA3
018200030808     C                   EXSR      WRISVN
018300030904     C                   endif
018400030808     C* Se piena la VARIA 3 su TITAS potrebbero esserci anche altre VARIE sul relativo file di esten
018500030808     C     KEYta7        SETLL     tita730c
018600030808     C                   IF        %equal(tita730c)
018700030808     C     KEYta7        READE     tita730c
018800030808     C                   DOW       not %eof(tita730c)
018900030911     C***                if        ta7VAN > *zeros
019000030808     C                   EVAL      wSVN = ta7SVN
019100030808     C                   EVAL      wVAN = ta7VAN
019200030808     C                   EXSR      WRISVN
019300030911     C***                endif
019400030808     C     KEYta7        READE     tita730c
019500030808     C                   ENDDO
019600030808     C                   ENDIF
019700030808     C                   ENDIF
019800030808     C*
019900030808     C                   ENDSR
020000030808     C*------------------------------------------------------------------------*
020100030808     C* WRISVN - Routine di valorizzazione wrk-file VARIE fatturate
020200030808     C*------------------------------------------------------------------------*
020300030808     C     WRISVN        BEGSR
020400030808     C*
020500030808     C                   MOVEL     wDataFatDa    kvftAMF
020600030808     C*
020700030808     C* Inizializzo il buffer del record del wrk-file
020800030808     C                   CLEAR                   WFVFT000
020900030808     C*
021000030808     C                   EVAL      kvftSVN = wSVN
021100030808     C     KEYvft_1      CHAIN     WFVFT01L
021200030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
021300030911     C* incremento il conteggio e l'importo della VARIA
021400030808     C                   IF        %found(WFVFT01L)
021500030808     C                   EVAL      vftVAN = vftVAN + wVAN
021600030808     C                   EVAL      vftCNT = vftCNT + 1
021700030808     C                   UPDATE    WFVFT000
021800030808     C                   ELSE
021900030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
022000030808     C* valorizzo il buffer del wrk-file e inserisco il record
022100030808     C                   EVAL      vftFPT = *blanks
022200030808     C                   EVAL      vftAMF = kvftAMF
022300030808     C                   EVAL      vftSVN = wSVN
022400030808     C                   EVAL      vftVAN = wVAN
022500030808     C                   EVAL      vftCNT = 1
022600030808     C* Reperisco la descricione della VARIA
022700030808     C                   Z-ADD     1             i
022800030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
022900030808     C                   IF        %found
023000030808     C                   EVAL      vftDES = SKSVNDES(i)
023100030808     C                   ELSE
023200030808     C                   EVAL      vftDES = *all'*'
023300030808     C                   ENDIF
023400030808     C*
023500030808     C                   WRITE     WFVFT000
023600030808     C                   ENDIF
023700030808     C* Verifico se trattasi d bolla PT, se bolla POSTE rifaccio il medesimo giro precedente
023800030808     C* x incrementare/creare un "DI CUI" della VARIA corrente x le bolle PT
023900030808     C                   IF        tasLNP = 180
024000030808     C                   EVAL      kvftFPT = 'S'
024100030808     C     KEYvft_2      CHAIN     WFVFT01L
024200030808     C* Se varia già presente su wrk-file per il periodo di fatturazione richiesto
024300030808     C* incremneto il conteggio e l'importo della VARIA
024400030808     C                   IF        %found(WFVFT01L)
024500030808     C                   EVAL      vftVAN = vftVAN + wVAN
024600030808     C                   EVAL      vftCNT = vftCNT + 1
024700030808     C                   UPDATE    WFVFT000
024800030808     C                   ELSE
024900030808     C* Se varia NON presente su wrk-file per il periodo di fatturazione richiesto
025000030808     C* valorizzo il buffer del wrk-file e inserisco il record
025100030808     C                   EVAL      vftFPT = kvftFPT
025200030808     C                   EVAL      vftAMF = kvftAMF
025300030808     C                   EVAL      vftSVN = wSVN
025400030808     C                   EVAL      vftVAN = wVAN
025500030808     C                   EVAL      vftCNT = 1
025600030808     C* Reperisco la descricione della VARIA
025700030808     C                   Z-ADD     1             i
025800030808     C     vftSVN        LOOKUP    SKSVN(i)                               55
025900030808     C                   IF        %found
026000030808     C                   EVAL      vftDES = SKSVNDES(i)
026100030808     C                   ELSE
026200030808     C                   EVAL      vftDES = *all'*'
026300030808     C                   ENDIF
026400030808     C*
026500030808     C                   WRITE     WFVFT000
026600030808     C                   ENDIF
026700030808     C                   ENDIF
026800030808     C*
026900030808     C                   ENDSR
027000011205     C*------------------------------------------------------------------------*
027100030808     C* CARTAB - CARICA I DATI TABELLATI
027200011205     C*------------------------------------------------------------------------*
027300030808     C     CARTAB        BEGSR
027400030811     C*
027500030811     C* Reperisco i tipi bolla validi come spedizioni
027600030811     C                   Z-ADD     *zeros        i
027700030811     C                   Z-ADD     1             tblKUT
027800030811     C                   MOVEL(P)  'TB'          tblCOD
027900030811     C     KEYtab_1      SETLL     TABEL00F
028000030811     C                   IF        %equal(TABEL00F)
028100030811     C     KEYtab_1      READE     TABEL00F
028200030811     C                   DOW       not %eof(TABEL00F)
028300030811     C                   MOVEL(P)  TBLUNI        DSTB
028400030811     C                   IF        §TBFCB = '1'
028500030811     C                   ADD       1             i
028600030811     C                   MOVEL(P)  TBLKEY        SKTBL(i)
028700030811     C                   ENDIF
028800030811     C     KEYtab_1      READE     TABEL00F
028900030811     C                   ENDDO
029000030811     C                   ENDIF
029100030808     C*
029200030808     C* Reperisco le descrizioni delle varie
029300030811     C                   Z-ADD     *zeros        i
029400030808     C                   Z-ADD     1             tblKUT
029500030808     C                   MOVEL(P)  'CC'          tblCOD
029600030808     C     KEYtab_1      SETLL     TABEL00F
029700030808     C                   IF        %equal(TABEL00F)
029800030808     C     KEYtab_1      READE     TABEL00F
029900030808     C                   DOW       not %eof(TABEL00F)
030000030808     C                   ADD       1             i
030100030808     C                   MOVE(P)   TBLKEY        SKSVN(i)
030200030808     C                   MOVEL(P)  TBLUNI        DSCC
030300030808     C                   MOVEL(P)  §CCDES        SKSVNDES(i)
030400030808     C     KEYtab_1      READE     TABEL00F
030500030808     C                   ENDDO
030600030808     C                   ENDIF
030700030808     C*
030800030808     C* Reperisco i P.O. IVA da considerare x le fatture fine mese clienti codificati
030900030808     C                   CLEAR                   DSQC2
031000030808     C                   Z-ADD     1             tblKUT
031100030808     C                   MOVEL(P)  'QC'          tblCOD
031200030808     C                   MOVEL(P)  '2'           tblKEY
031300030808     C     KEYtab_2      CHAIN     TABEL00F
031400030808     C                   IF        %found(TABEL00F)
031500030808     C                   MOVEL(P)  TBLUNI        DSQC2
031600030808     C                   ENDIF
031700011206     C*
031800011205     C                   ENDSR
031900970526     C*------------------------------------------------------------------------*
032000970526     C* *INZSR - OPERAZIONI INIZIALI
032100970526     C*------------------------------------------------------------------------*
032200970526     C     *INZSR        BEGSR
032300970526     C*---
032400970526     C* RICEVIMENTO PARAMETRI
032500970526     C*---
032600970526     C     *ENTRY        PLIST
032700970612     C                   PARM                    KPJBA
032800991214     C                   MOVEL     KPJBU         PARAM
032900970526     C*---
033000991214     C* CHIAVI DI LETTURA
033100991214     C*---
033200000810     C*
033300030808     C* LETTURA tabel00f - PARZIALE
033400030808     C     KEYtab_1      KLIST
033500030808     C                   KFLD                    tblKUT                         *UTENTE
033600030808     C                   KFLD                    tblCOD                         *TABELLA
033700030808     C*
033800030808     C* LETTURA tabel00f - COMPLETA
033900030808     C     KEYtab_2      KLIST
034000030808     C                   KFLD                    tblKUT                         *UTENTE
034100030808     C                   KFLD                    tblCOD                         *TABELLA
034200030808     C                   KFLD                    tblKEY                         *CHIAVE
034300030808     C*
034400030911     C* LETTURA wfvft01l - PARZIALE
034500030808     C     KEYvft_1      KLIST
034600030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
034700030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
034800030808     C*
034900030808     C* LETTURA wfvft01l - COMPLETA
035000030808     C     KEYvft_2      KLIST
035100030808     C                   KFLD                    kvftAMF                        *ANNO MESE FATTURAZI
035200030808     C                   KFLD                    kvftSVN                        *SIGLA VARIA
035300030808     C                   KFLD                    kvftFPT                        *FLAG POSTE
035400030808     C*
035500030808     C* LETTURA tita730c - PARZIALE
035600030808     C     KEYta7        KLIST
035700030808     C                   KFLD                    tasAAS                         *ANNO BOLLA
035800030808     C                   KFLD                    tasLNP                         *LINEA PARTENZA BOLL
035900030808     C                   KFLD                    tasNRS                         *SERIE BOLLA
036000030808     C                   KFLD                    tasNSP                         *NUMERO SPEDIZIONE B
036100030808     C                   KFLD                    tasTBL                         *TIPO BOLLA
036200991214     C*
036300970526     C                   ENDSR
