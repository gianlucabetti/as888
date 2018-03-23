000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300020613     H* STATISTICA EVENTI CLIENTE WIND
000400010509     H*-------------------------------------------------------------------------*
000500020613     FWFWIND00F UF A E             DISK
000600020614     FTITAS31C  IF   E           K DISK
000700010131     F                                     rename(titas000:titasx00)
000800010131     F                                     rename(titas010:titasx10)
000900010131     F                                     rename(titasP00:titasxP0)
001000020613     FFNEVB04L  IF   E           K DISK
001100020613     FAZORG01L  IF   E           K DISK
001200020613     FTABEL00F  IF   E           K DISK
001300001218     D*--------------------
001400001218     D* DS ESTERNE
001500001218     D*--------------------
001600900517     D KPJBA         E DS
001700010509     D DS2A          E DS
001800020614     D DS14          E DS
001900011112     D*--------------------
002000011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE TAS
002100011112     D*--------------------
002200011112     D                 DS
002300011112     D  TASAAS                        4  0
002400011112     D  TASMGS                        4  0
002500011112     D  TASDSP                 1      8  0
002600010509     D*--------------------
002700010509     D* SCHIERA PER CARICAMENTO CODICI E TIPI MANCATE CONSEGNE
002800010509     D*--------------------
002900020614     D SK2AA           S              3    DIM(300)                             cod. eventi L.A.
003000020614     D SK2AG           S              3    DIM(300)                             cod. eventi GIAC.
003100020613     D*------------------
003200020613     D* DS X PASSAGGIO PARAMETRI
003300020613     D*------------------
003400020613     D PARAM           DS                  INZ
003500020613     D  PARKSC                 1      7  0
003600020613     D  PARDAI                 8     15  0
003700020613     D  PARDAF                16     23  0
003800010606     C*---------------------------------------------------------------*
003900001218
004000001218
004100001218
004200920812     C*---------------------------------------------------------------*
004300001218     C* MAIN
004400001218     C*---------------------------------------------------------------*
004500010509     C                   exsr      cartbl
004600020613     C                   exsr      delwfile
004700001218     C                   exsr      procedi
004800001218     C*
004900001218     C                   seton                                        LR
005000010509     C*---------------------------------------------------------------*
005100001218
005200001218
005300001218
005400001218     C*------------------------------------------------------------------------*
005500001218     C* PROCEDI - Routine principale
005600001218     C*------------------------------------------------------------------------*
005700020613     C     PROCEDI       BEGSR
005800001218     C*
005900020614     C* Mi posiziono sul TITAS31C per cliente fatturazione
006000020613     C                   Z-ADD     PARKSC        tasKSC
006100020614     C     KTAS31P       SETLL     TITAS31C
006200020613     C*
006300020613     C* Se posizionamento riuscito => ciclo ed elaboro
006400020614     C                   IF        %found(TITAS31C)
006500020614     C                   MOVEL     '0'           flgFINE           1
006600020613     C*
006700020613     C* Prima lettura fuori ciclo
006800020613     C                   EXSR      LEGGITAS
006900020614     C                   DOW       not %eof(TITAS31C) AND
007000020614     C                             flgFINE = '0'
007100020613     C*
007200020613     C* Verifico se la bolla è da considerare
007300020613     C                   EXSR      CHKRECTAS
007400020613     C                   IF        flgRECOK = 'S'
007500020613     C*
007600020613     C* Se la bolla è da considerare => elaboro e scrivo il WRK-FILE
007700020613     C                   EXSR      WRIWFILE
007800020613     C                   ENDIF
007900020613     C*
008000020613     C* Proseguo le letture dentro ciclo
008100020613     C                   EXSR      LEGGITAS
008200020613     C                   ENDDO
008300001218     C*
008400020613     C                   ENDIF
008500020613     C*
008600001218     C                   ENDSR
008700001218     C*------------------------------------------------------------------------*
008800020613
008900020613
009000020613
009100020613     C*------------------------------------------------------------------------*
009200020613     C* DELWFILE - Routine di cancellazione record del file inclusi nel periodo di lancio
009300020613     C*------------------------------------------------------------------------*
009400020613     C     DELWFILE      BEGSR
009500020613     C*
009600020613     C                   READ      WFWIND00F
009700020613     C                   DOW       not %eof(WFWIND00F)
009800020613     C                   IF        tasDSP >= PARDAI AND
009900020613     C                             tasDSP <= PARDAF
010000020613     C                   DELETE    WFWIND000
010100020613     C                   ENDIF
010200020613     C                   READ      WFWIND00F
010300020613     C                   ENDDO
010400020613     C*
010500020613     C                   ENDSR
010600020613     C*------------------------------------------------------------------------*
010700020613
010800020613
010900020613
011000020613     C*------------------------------------------------------------------------*
011100020614     C* LEGGITAS - Routine di operazioni preliminari cicliche e lettura record TITAS31C
011200020613     C*------------------------------------------------------------------------*
011300020613     C     LEGGITAS      BEGSR
011400020613     C*
011500020614     C* Inizializzo formato record del WRK-FILE prima della lettura del TITAS31C xchè altrimenti
011600020613     C* perderei il contenuto del buffer (in quanto il WRK-FILE ha i campi denominati come quelli
011700020613     C* di TITAS000
011800020613     C                   CLEAR                   WFWIND000
011900020614     C                   READ      TITAS31C
012000020613     C*
012100020613     C                   ENDSR
012200020613     C*------------------------------------------------------------------------*
012300010627
012400010627
012500010627
012600010627     C*------------------------------------------------------------------------*
012700020613     C* CHKRECTAS - Routine di verifica bolla
012800010627     C*------------------------------------------------------------------------*
012900020613     C     CHKRECTAS     BEGSR
013000010627     C*
013100020613     C                   MOVEL     'S'           flgRECOK          1
013200020613     C*
013300020613     C* Testo se la bolla è del cliente richiesto
013400020613     C                   IF        tasKSC = PARKSC
013500020613     C                   ELSE
013600020613     C                   MOVEL     'N'           flgRECOK
013700020614     C* quando cambia il cliente esco dal ciclo
013800020614     C                   MOVEL     '1'           flgFINE
013900020613     C                   ENDIF
014000020613     C*
014100020613     C* Testo se la bolla è del periodo richiesto
014200020613     C                   IF        tasDSP >= PARDAI AND
014300020613     C                             tasDSP <= PARDAF
014400020613     C                   ELSE
014500020613     C                   MOVEL     'N'           flgRECOK
014600020613     C                   ENDIF
014700020613     C*
014800020613     C* Testo se la bolla è consegnata
014900020613     C                   IF        tasDCM > *zeros
015000020613     C                   ELSE
015100020613     C                   MOVEL     'N'           flgRECOK
015200020613     C                   ENDIF
015300020614     C*
015400020614     C* Testo se il tipo bolla è 'F1' o 'A2'
015500020614     C                   IF        tasTBL = 'F1' OR
015600020614     C                             tasTBL = 'A2'
015700020614     C                   ELSE
015800020614     C                   MOVEL     'N'           flgRECOK
015900020614     C                   ENDIF
016000010627     C*
016100010627     C                   ENDSR
016200010627     C*------------------------------------------------------------------------*
016300001218
016400001218
016500010509
016600010627     C*------------------------------------------------------------------------*
016700020613     C* WRIWFILE - Routine di reperimento informazioni e scrittura WRK-FILE
016800010627     C*------------------------------------------------------------------------*
016900020613     C     WRIWFILE      BEGSR
017000010627     C*
017100020614     C*-----------------
017200020614     C* Verifico condizioni x valorizzazione 4° flag (CONSEGNA ANOMALA DI TIPO "RESO")
017300020614     C*-----------------
017400020614     C                   IF        tasCCA = '2'
017500020614     C                   Z-ADD     1             WFWF04
017600020614     C                   ENDIF
017700020614     C*-----------------
017800020614     C* Verifico condizioni x valorizzazione 3° flag (EVENTI DI TIPO "LASCIATO AVVISO")
017900020614     C*-----------------
018000030324     C* Inizializzo i flag di wrk x ogni bolla
018100030324     C                   MOVEL     '0'           flgEvb017         1
018200030324     C                   MOVEL     '0'           flgEvbAVV         1
018300030324     C                   MOVEL     '0'           flgEvbRIC         1
018400030324     C*
018500020614     C                   IF        WFWF04 = *zeros
018600020614     C     *LOVAL        SETLL     FNEVB04L
018700020614     C     KEVB04P       SETLL     FNEVB04L
018800020614     C* ...se trovato leggo ed effettuo considerazioni
018900020614     C                   IF        %found(FNEVB04L)
019000020614     C     KEVB04P       READE     FNEVB04L
019100020614     C* ...ciclo
019200020614     C                   DOW       not %eof(FNEVB04L)
019300030324     C                   IF        evbCEV = '017'
019400030324     C                   MOVEL     '1'           flgEvb017
019500030324     C                   ENDIF
019600030324     C                   IF        evbCEV = 'AVV'
019700030324     C                   MOVEL     '1'           flgEvbAVV
019800030324     C                   ENDIF
019900030324     C                   IF        evbCEV = 'RIC'
020000030324     C                   MOVEL     '1'           flgEvbRIC
020100030324     C                   ENDIF
020200020614     C                   IF        evbCEV <> 'RIC'
020300020614     C     evbCEV        LOOKUP    SK2AA                                  55
020400020614     C                   IF        %found
020500020614     C                   Z-ADD     1             WFWF03
020600020614     C                   ENDIF
020700020614     C                   ENDIF
020800020614     C                   IF        tasFGC <> 'S'
020900020614     C     evbCEV        LOOKUP    SK2AG                                  55
021000020614     C                   IF        %found
021100020614     C                   Z-ADD     1             WFWF03
021200020614     C                   ENDIF
021300020614     C                   ENDIF
021400020614     C     KEVB04P       READE     FNEVB04L
021500020614     C                   ENDDO
021600020614     C                   ENDIF
021700020614     C                   ENDIF
021800030324     C*
021900030324     C* Al termine della lettura degli eventi traggo le dovute considerazioni secondo l'ordine
022000030324     C* 1°:017 - 2°:AVV - 3°:RIC
022100030324     C                   IF        flgEvb017 = '1'
022200030324     C                   EVAL      WFWF09 = 1
022300030324     C                   ELSE
022400030324     C                   IF        flgEvbAVV = '1'
022500030324     C                   EVAL      WFWF08 = 1
022600030324     C                   ELSE
022700030324     C                   IF        flgEvbRIC = '1'
022800030324     C                   EVAL      WFWF07 = 1
022900030324     C                   ENDIF
023000030324     C                   ENDIF
023100030324     C                   ENDIF
023200020614     C*-----------------
023300020614     C* Verifico condizioni x valorizzazione 2° flag (RIC)
023400020614     C*-----------------
023500020614     C                   IF        WFWF04 = *zeros AND
023600020614     C                             WFWF03 = *zeros
023700020614     C                   MOVEL     'RIC'         wrkCEV
023800020614     C     *LOVAL        SETLL     FNEVB04L
023900020614     C     KEVB04C       CHAIN     FNEVB04L
024000020614     C* ...se trovato valorizzo flag a ok
024100020614     C                   IF        %found(FNEVB04L)
024200020614     C                   Z-ADD     1             WFWF02
024300020614     C                   ENDIF
024400020614     C                   ENDIF
024500020613     C*-----------------
024600020613     C* Verifico condizioni x valorizzazione 1° flag (MIC)
024700020613     C*-----------------
024800020614     C                   IF        WFWF04 = *zeros AND
024900020614     C                             WFWF03 = *zeros AND
025000020614     C                             WFWF02 = *zeros
025100020613     C                   MOVEL     'MIC'         wrkCEV
025200020614     C     *LOVAL        SETLL     FNEVB04L
025300020614     C     KEVB04C       SETLL     FNEVB04L
025400020613     C* ...se trovato leggo ed effettuo considerazioni
025500020614     C                   IF        %equal(FNEVB04L)
025600020614     C     KEVB04C       READE     FNEVB04L
025700020613     C* ...ciclo
025800020613     C                   DOW       not %eof(FNEVB04L)
025900020613     C                   IF        evbDTV = tasDCM
026000020613     C                   Z-ADD     1             WFWF01
026100020613     C                   ENDIF
026200020614     C     KEVB04C       READE     FNEVB04L
026300020613     C                   ENDDO
026400020613     C                   ELSE
026500020613     C* ...se non trovato imposto il flag a ok
026600020613     C                   Z-ADD     1             WFWF01
026700020613     C                   ENDIF
026800020614     C                   ENDIF
026900020613     C*-----------------
027000020614     C* Se i primi quattro flag nn sono ancora stato valorizzati => cvalorizzo il 6° flag
027100020614     C*-----------------
027200020614     C                   IF        WFWF01 = *zeros AND
027300020614     C                             WFWF02 = *zeros AND
027400020614     C                             WFWF03 = *zeros AND
027500020614     C                             WFWF04 = *zeros
027600020614     C                   Z-ADD     1             WFWF06
027700020614     C                   ENDIF
027800020614     C*-----------------
027900020613     C* Verifico condizioni x valorizzazione 5° flag (HA AVUTO GIACENZA)
028000020613     C*-----------------
028100020613     C                   IF        tasFGC = 'S'
028200020613     C                   Z-ADD     1             WFWF05
028300020613     C                   ENDIF
028400020613     C*-----------------
028500020614     C* Reperisco distretto, area e regione (sia cod. che desc.) relativamente alla LNA della bolla
028600020613     C*-----------------
028700020613     C     tasLNA        CHAIN     AZORG01L
028800020613     C                   IF        %found(AZORG01L)
028900020613     C                   EVAL      WFWDIS = ORGFL3
029000020613     C                   EVAL      WFWARA = ORGCAR
029100020613     C                   EVAL      WFWCRA = ORGCRE
029200020614     C                   EXSR      REPDESREG
029300020613     C                   ENDIF
029400020613     C*
029500020613     C* Al termine scrivo il record così valorizzato
029600020613     C                   WRITE     WFWIND000
029700020613     C*
029800010627     C                   ENDSR
029900010509     C*------------------------------------------------------------------------*
030000020614
030100020614
030200020614
030300020614
030400020614     C*------------------------------------------------------------------------*
030500020614     C* REPDESREG - Routine di reperimento descrizione regione linea di arrivo
030600020614     C*------------------------------------------------------------------------*
030700020614     C     REPDESREG     BEGSR
030800020614     C*
030900020614     C                   MOVEL     '14'          COD
031000020614     C                   MOVEL(P)  WFWCRA        KEY
031100020614     C     KTABC         CHAIN     TABEL00F
031200020614     C                   IF        %found(TABEL00F)
031300020614     C     TBLFLG        IFEQ      ' '
031400020614     C                   MOVEL     TBLUNI        DS14
031500020614     C                   MOVEL     §14DES        WFWDRA
031600020614     C                   ENDIF
031700020614     C                   ENDIF
031800020614     C*
031900020614     C                   ENDSR
032000020614     C*---------------------------------------------------------------*
032100010626
032200010626
032300010626
032400010626
032500010509     C*------------------------------------------------------------------------*
032600020613     C* CARTBL - Routine di caricamento dati tabellati
032700010509     C*------------------------------------------------------------------------*
032800010509     C     CARTBL        BEGSR
032900010509     C*
033000020614     C                   Z-ADD     0             WA                4 0
033100020614     C                   Z-ADD     0             WG                4 0
033200020614     C                   CLEAR                   SK2AA
033300020614     C                   CLEAR                   SK2AG
033400010509     C                   MOVEL     '2A'          COD
033500020614     C     KTABP         CHAIN     TABEL00F                           31
033600010509     C     *IN31         DOWEQ     '0'
033700010509     C     TBLFLG        IFEQ      ' '
033800010509     C                   MOVEL     TBLUNI        DS2A
033900020613     C     §2AFTC        IFEQ      'A'
034000020614     C                   ADD       1             WA
034100020614     C                   MOVEL     TBLKEY        SK2AA(WA)
034200010509     C                   ENDIF
034300020614     C     §2AFTC        IFEQ      'G'
034400020614     C                   ADD       1             WG
034500020614     C                   MOVEL     TBLKEY        SK2AG(WG)
034600020614     C                   ENDIF
034700010509     C                   ENDIF
034800020614     C     KTABP         READE     TABEL                                  31
034900010509     C                   ENDDO
035000010509     C*
035100010509     C                   ENDSR
035200010509     C*---------------------------------------------------------------*
035300010509
035400010509
035500001218
035600001218     C*------------------------------------------------------------------------*
035700001218     C* *INZSR - ROUTINE INIZIALE
035800001218     C*------------------------------------------------------------------------*
035900001218     C     *INZSR        BEGSR
036000001218     C*
036100010726     C* Ricevo in input il parametro indicante l'ID LANCIO
036200010726     C     *ENTRY        PLIST
036300020613     C                   PARM                    KPJBA
036400020613     C*
036500020613     C                   MOVEL     KPJBU         PARAM
036600010726     C*
036700001218     C                   Z-ADD     1             CODUT
036800001218     C*
036900001218     C* Definizioni chiavi
037000020614     C     KTAS31P       KLIST                                                  TITAS31C - Parziale
037100020613     C                   KFLD                    tasKSC
037200010509     C*
037300020613     C     KEVB04C       KLIST                                                  FNEVB04L - Completa
037400010509     C                   KFLD                    tasAAS
037500020613     C                   KFLD                    tasLNP
037600020613     C                   KFLD                    tasNRS
037700010509     C                   KFLD                    tasNSP
037800020613     C                   KFLD                    wrkCEV            3
037900020613     C*
038000020613     C     KEVB04P       KLIST                                                  FNEVB04L - Parziale
038100020613     C                   KFLD                    tasAAS
038200020613     C                   KFLD                    tasLNP
038300020613     C                   KFLD                    tasNRS
038400020613     C                   KFLD                    tasNSP
038500010509     C*
038600020614     C     KTABP         KLIST                                                  TABEL00F - Parziale
038700010509     C                   KFLD                    CODUT             1 0
038800010509     C                   KFLD                    COD               2
038900020614     C*
039000020614     C     KTABC         KLIST                                                  TABEL00F - Completa
039100020614     C                   KFLD                    CODUT             1 0
039200020614     C                   KFLD                    COD               2
039300020614     C                   KFLD                    KEY               8
039400001218     C*
039500001218     C                   ENDSR
