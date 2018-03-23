000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200010509     H*-------------------------------------------------------------------------*
000300020613     H* STATISTICA EVENTI CLIENTE WIND
000400010509     H*-------------------------------------------------------------------------*
000500020613     FWFWIND00F UF A E             DISK
000600020614     FTITAS31C  IF   E           K DISK
000700010131     F                                     rename(titas000:titasx00)
000800010131     F                                     rename(titas010:titasx10)
000900010131     F                                     rename(titasP00:titasxP0)
001000041125     FFNEVB01L  IF   E           K DISK
001100041125     F                                     rename(fnevb000:fnevb001)
001200041125     FFNEVB04L  IF   E           K DISK
001300041125     F                                     rename(fnevb000:fnevb004)
001400020613     FAZORG01L  IF   E           K DISK
001500020613     FTABEL00F  IF   E           K DISK
001600001218     D*--------------------
001700001218     D* DS ESTERNE
001800001218     D*--------------------
001900900517     D KPJBA         E DS
002000010509     D DS2A          E DS
002100020614     D DS14          E DS
002200011112     D*--------------------
002300011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE TAS
002400011112     D*--------------------
002500011112     D                 DS
002600011112     D  TASAAS                        4  0
002700011112     D  TASMGS                        4  0
002800011112     D  TASDSP                 1      8  0
002900010509     D*--------------------
003000010509     D* SCHIERA PER CARICAMENTO CODICI E TIPI MANCATE CONSEGNE
003100010509     D*--------------------
003200020614     D SK2AA           S              3    DIM(300)                             cod. eventi L.A.
003300020614     D SK2AG           S              3    DIM(300)                             cod. eventi GIAC.
003400020613     D*------------------
003500020613     D* DS X PASSAGGIO PARAMETRI
003600020613     D*------------------
003700020613     D PARAM           DS                  INZ
003800020613     D  PARKSC                 1      7  0
003900020613     D  PARDAI                 8     15  0
004000020613     D  PARDAF                16     23  0
004100041125     D  PARCCA                24     24
004200010606     C*---------------------------------------------------------------*
004300001218
004400001218
004500001218
004600920812     C*---------------------------------------------------------------*
004700001218     C* MAIN
004800001218     C*---------------------------------------------------------------*
004900010509     C                   exsr      cartbl
005000041125     C*                  exsr      delwfile
005100001218     C                   exsr      procedi
005200001218     C*
005300001218     C                   seton                                        LR
005400010509     C*---------------------------------------------------------------*
005500001218
005600001218
005700001218
005800001218     C*------------------------------------------------------------------------*
005900001218     C* PROCEDI - Routine principale
006000001218     C*------------------------------------------------------------------------*
006100020613     C     PROCEDI       BEGSR
006200001218     C*
006300020614     C* Mi posiziono sul TITAS31C per cliente fatturazione
006400020613     C                   Z-ADD     PARKSC        tasKSC
006500020614     C     KTAS31P       SETLL     TITAS31C
006600020613     C*
006700020613     C* Se posizionamento riuscito => ciclo ed elaboro
006800020614     C                   IF        %found(TITAS31C)
006900020614     C                   MOVEL     '0'           flgFINE           1
007000020613     C*
007100020613     C* Prima lettura fuori ciclo
007200020613     C                   EXSR      LEGGITAS
007300020614     C                   DOW       not %eof(TITAS31C) AND
007400020614     C                             flgFINE = '0'
007500020613     C*
007600020613     C* Verifico se la bolla è da considerare
007700020613     C                   EXSR      CHKRECTAS
007800020613     C                   IF        flgRECOK = 'S'
007900020613     C*
008000020613     C* Se la bolla è da considerare => elaboro e scrivo il WRK-FILE
008100020613     C                   EXSR      WRIWFILE
008200020613     C                   ENDIF
008300020613     C*
008400020613     C* Proseguo le letture dentro ciclo
008500020613     C                   EXSR      LEGGITAS
008600020613     C                   ENDDO
008700001218     C*
008800020613     C                   ENDIF
008900020613     C*
009000001218     C                   ENDSR
009100001218     C*------------------------------------------------------------------------*
009200020613
009300020613
009400020613
009500020613     C*------------------------------------------------------------------------*
009600020613     C* DELWFILE - Routine di cancellazione record del file inclusi nel periodo di lancio
009700020613     C*------------------------------------------------------------------------*
009800020613     C     DELWFILE      BEGSR
009900020613     C*
010000020613     C                   READ      WFWIND00F
010100020613     C                   DOW       not %eof(WFWIND00F)
010200020613     C                   IF        tasDSP >= PARDAI AND
010300020613     C                             tasDSP <= PARDAF
010400020613     C                   DELETE    WFWIND000
010500020613     C                   ENDIF
010600020613     C                   READ      WFWIND00F
010700020613     C                   ENDDO
010800020613     C*
010900020613     C                   ENDSR
011000020613     C*------------------------------------------------------------------------*
011100020613
011200020613
011300020613
011400020613     C*------------------------------------------------------------------------*
011500020614     C* LEGGITAS - Routine di operazioni preliminari cicliche e lettura record TITAS31C
011600020613     C*------------------------------------------------------------------------*
011700020613     C     LEGGITAS      BEGSR
011800020613     C*
011900020614     C* Inizializzo formato record del WRK-FILE prima della lettura del TITAS31C xchè altrimenti
012000020613     C* perderei il contenuto del buffer (in quanto il WRK-FILE ha i campi denominati come quelli
012100020613     C* di TITAS000
012200020613     C                   CLEAR                   WFWIND000
012300020614     C                   READ      TITAS31C
012400020613     C*
012500020613     C                   ENDSR
012600020613     C*------------------------------------------------------------------------*
012700010627
012800010627
012900010627
013000010627     C*------------------------------------------------------------------------*
013100020613     C* CHKRECTAS - Routine di verifica bolla
013200010627     C*------------------------------------------------------------------------*
013300020613     C     CHKRECTAS     BEGSR
013400010627     C*
013500020613     C                   MOVEL     'S'           flgRECOK          1
013600020613     C*
013700020613     C* Testo se la bolla è del cliente richiesto
013800020613     C                   IF        tasKSC = PARKSC
013900020613     C                   ELSE
014000020613     C                   MOVEL     'N'           flgRECOK
014100020614     C* quando cambia il cliente esco dal ciclo
014200020614     C                   MOVEL     '1'           flgFINE
014300020613     C                   ENDIF
014400020613     C*
014500020613     C* Testo se la bolla è del periodo richiesto
014600020613     C                   IF        tasDSP >= PARDAI AND
014700020613     C                             tasDSP <= PARDAF
014800020613     C                   ELSE
014900020613     C                   MOVEL     'N'           flgRECOK
015000020613     C                   ENDIF
015100020613     C*
015200020613     C* Testo se la bolla è consegnata
015300020613     C                   IF        tasDCM > *zeros
015400020613     C                   ELSE
015500020613     C                   MOVEL     'N'           flgRECOK
015600020613     C                   ENDIF
015700020614     C*
015800020614     C* Testo se il tipo bolla è 'F1' o 'A2'
015900020614     C                   IF        tasTBL = 'F1' OR
016000020614     C                             tasTBL = 'A2'
016100020614     C                   ELSE
016200020614     C                   MOVEL     'N'           flgRECOK
016300020614     C                   ENDIF
016400041125     C*
016500041125     C* Se richiesto nel lancio testo se la bolla ha il codice consegna anomala indicato
016600041125     C                   IF        PARCCA <> '*'
016700041125     C                   IF        tasCCA = PARCCA
016800041125     C                   ELSE
016900041125     C                   MOVEL     'N'           flgRECOK
017000041125     C                   ENDIF
017100041125     C                   ENDIF
017200010627     C*
017300010627     C                   ENDSR
017400010627     C*------------------------------------------------------------------------*
017500001218
017600001218
017700010509
017800010627     C*------------------------------------------------------------------------*
017900020613     C* WRIWFILE - Routine di reperimento informazioni e scrittura WRK-FILE
018000010627     C*------------------------------------------------------------------------*
018100020613     C     WRIWFILE      BEGSR
018200010627     C*
018300020614     C*-----------------
018400020614     C* Verifico condizioni x valorizzazione 4° flag (CONSEGNA ANOMALA DI TIPO "RESO")
018500020614     C*-----------------
018600020614     C                   IF        tasCCA = '2'
018700020614     C                   Z-ADD     1             WFWF04
018800020614     C                   ENDIF
018900020614     C*-----------------
019000020614     C* Verifico condizioni x valorizzazione 3° flag (EVENTI DI TIPO "LASCIATO AVVISO")
019100020614     C*-----------------
019200030324     C* Inizializzo i flag di wrk x ogni bolla
019300030324     C                   MOVEL     '0'           flgEvb017         1
019400030324     C                   MOVEL     '0'           flgEvbAVV         1
019500030324     C                   MOVEL     '0'           flgEvbRIC         1
019600030324     C*
019700041125     C     *LOVAL        SETLL     FNEVB01L
019800041125     C     KEVB01P       SETLL     FNEVB01L
019900020614     C* ...se trovato leggo ed effettuo considerazioni
020000041125     C                   IF        %found(FNEVB01L)
020100041125     C     KEVB01P       READE     FNEVB01L
020200020614     C* ...ciclo
020300041125     C                   DOW       not %eof(FNEVB01L)
020400041125     C*
020500041125     C* Solo se consegna anomala NN reso
020600041125     C                   IF        WFWF04 = *zeros
020700030324     C                   IF        evbCEV = '017'
020800041125     C                   MOVEL     '1'           flgEvb017
020900030324     C                   ENDIF
021000030324     C                   IF        evbCEV = 'AVV'
021100030324     C                   MOVEL     '1'           flgEvbAVV
021200030324     C                   ENDIF
021300030324     C                   IF        evbCEV = 'RIC'
021400030324     C                   MOVEL     '1'           flgEvbRIC
021500030324     C                   ENDIF
021600020614     C                   IF        evbCEV <> 'RIC'
021700020614     C     evbCEV        LOOKUP    SK2AA                                  55
021800020614     C                   IF        %found
021900020614     C                   Z-ADD     1             WFWF03
022000020614     C                   ENDIF
022100020614     C                   ENDIF
022200020614     C                   IF        tasFGC <> 'S'
022300020614     C     evbCEV        LOOKUP    SK2AG                                  55
022400020614     C                   IF        %found
022500020614     C                   Z-ADD     1             WFWF03
022600020614     C                   ENDIF
022700020614     C                   ENDIF
022800041125     C                   ENDIF
022900041125     C*
023000041125     C* Identifico l'ultimo evento d tipo Lasciato Avviso o Giacenza x gli eventi
023100041125     C* inseriti dai P.O. = LNA della bolla
023200041125     C                   IF        evbFLE = tasLNA
023300041125     C     evbCEV        LOOKUP    SK2AA                                  56
023400041125     C     evbCEV        LOOKUP    SK2AG                                  57
023500041125     C                   IF        *IN56 OR *IN57
023600041125     C                   MOVEL     evbCEV        WFWF10
023700041125     C                   ENDIF
023800041125     C                   ENDIF
023900041125     C*
024000041125     C     KEVB01P       READE     FNEVB01L
024100020614     C                   ENDDO
024200020614     C                   ENDIF
024300030324     C*
024400030324     C* Al termine della lettura degli eventi traggo le dovute considerazioni secondo l'ordine
024500030324     C* 1°:017 - 2°:AVV - 3°:RIC
024600030324     C                   IF        flgEvb017 = '1'
024700030324     C                   EVAL      WFWF09 = 1
024800030324     C                   ELSE
024900030324     C                   IF        flgEvbAVV = '1'
025000030324     C                   EVAL      WFWF08 = 1
025100030324     C                   ELSE
025200030324     C                   IF        flgEvbRIC = '1'
025300030324     C                   EVAL      WFWF07 = 1
025400030324     C                   ENDIF
025500030324     C                   ENDIF
025600030324     C                   ENDIF
025700020614     C*-----------------
025800020614     C* Verifico condizioni x valorizzazione 2° flag (RIC)
025900020614     C*-----------------
026000020614     C                   IF        WFWF04 = *zeros AND
026100020614     C                             WFWF03 = *zeros
026200020614     C                   MOVEL     'RIC'         wrkCEV
026300020614     C     *LOVAL        SETLL     FNEVB04L
026400020614     C     KEVB04C       CHAIN     FNEVB04L
026500020614     C* ...se trovato valorizzo flag a ok
026600020614     C                   IF        %found(FNEVB04L)
026700020614     C                   Z-ADD     1             WFWF02
026800020614     C                   ENDIF
026900020614     C                   ENDIF
027000020613     C*-----------------
027100020613     C* Verifico condizioni x valorizzazione 1° flag (MIC)
027200020613     C*-----------------
027300020614     C                   IF        WFWF04 = *zeros AND
027400020614     C                             WFWF03 = *zeros AND
027500020614     C                             WFWF02 = *zeros
027600020613     C                   MOVEL     'MIC'         wrkCEV
027700020614     C     *LOVAL        SETLL     FNEVB04L
027800020614     C     KEVB04C       SETLL     FNEVB04L
027900020613     C* ...se trovato leggo ed effettuo considerazioni
028000020614     C                   IF        %equal(FNEVB04L)
028100020614     C     KEVB04C       READE     FNEVB04L
028200020613     C* ...ciclo
028300020613     C                   DOW       not %eof(FNEVB04L)
028400020613     C                   IF        evbDTV = tasDCM
028500020613     C                   Z-ADD     1             WFWF01
028600020613     C                   ENDIF
028700020614     C     KEVB04C       READE     FNEVB04L
028800020613     C                   ENDDO
028900020613     C                   ELSE
029000020613     C* ...se non trovato imposto il flag a ok
029100020613     C                   Z-ADD     1             WFWF01
029200020613     C                   ENDIF
029300020614     C                   ENDIF
029400020613     C*-----------------
029500020614     C* Se i primi quattro flag nn sono ancora stato valorizzati => cvalorizzo il 6° flag
029600020614     C*-----------------
029700020614     C                   IF        WFWF01 = *zeros AND
029800020614     C                             WFWF02 = *zeros AND
029900020614     C                             WFWF03 = *zeros AND
030000020614     C                             WFWF04 = *zeros
030100020614     C                   Z-ADD     1             WFWF06
030200020614     C                   ENDIF
030300020614     C*-----------------
030400020613     C* Verifico condizioni x valorizzazione 5° flag (HA AVUTO GIACENZA)
030500020613     C*-----------------
030600020613     C                   IF        tasFGC = 'S'
030700020613     C                   Z-ADD     1             WFWF05
030800020613     C                   ENDIF
030900020613     C*-----------------
031000020614     C* Reperisco distretto, area e regione (sia cod. che desc.) relativamente alla LNA della bolla
031100020613     C*-----------------
031200020613     C     tasLNA        CHAIN     AZORG01L
031300020613     C                   IF        %found(AZORG01L)
031400020613     C                   EVAL      WFWDIS = ORGFL3
031500020613     C                   EVAL      WFWARA = ORGCAR
031600020613     C                   EVAL      WFWCRA = ORGCRE
031700020614     C                   EXSR      REPDESREG
031800020613     C                   ENDIF
031900020613     C*
032000020613     C* Al termine scrivo il record così valorizzato
032100020613     C                   WRITE     WFWIND000
032200020613     C*
032300010627     C                   ENDSR
032400010509     C*------------------------------------------------------------------------*
032500020614
032600020614
032700020614
032800020614
032900020614     C*------------------------------------------------------------------------*
033000020614     C* REPDESREG - Routine di reperimento descrizione regione linea di arrivo
033100020614     C*------------------------------------------------------------------------*
033200020614     C     REPDESREG     BEGSR
033300020614     C*
033400020614     C                   MOVEL     '14'          COD
033500020614     C                   MOVEL(P)  WFWCRA        KEY
033600020614     C     KTABC         CHAIN     TABEL00F
033700020614     C                   IF        %found(TABEL00F)
033800020614     C     TBLFLG        IFEQ      ' '
033900020614     C                   MOVEL     TBLUNI        DS14
034000020614     C                   MOVEL     §14DES        WFWDRA
034100020614     C                   ENDIF
034200020614     C                   ENDIF
034300020614     C*
034400020614     C                   ENDSR
034500020614     C*---------------------------------------------------------------*
034600010626
034700010626
034800010626
034900010626
035000010509     C*------------------------------------------------------------------------*
035100020613     C* CARTBL - Routine di caricamento dati tabellati
035200010509     C*------------------------------------------------------------------------*
035300010509     C     CARTBL        BEGSR
035400010509     C*
035500020614     C                   Z-ADD     0             WA                4 0
035600020614     C                   Z-ADD     0             WG                4 0
035700020614     C                   CLEAR                   SK2AA
035800020614     C                   CLEAR                   SK2AG
035900010509     C                   MOVEL     '2A'          COD
036000020614     C     KTABP         CHAIN     TABEL00F                           31
036100010509     C     *IN31         DOWEQ     '0'
036200010509     C     TBLFLG        IFEQ      ' '
036300010509     C                   MOVEL     TBLUNI        DS2A
036400020613     C     §2AFTC        IFEQ      'A'
036500020614     C                   ADD       1             WA
036600020614     C                   MOVEL     TBLKEY        SK2AA(WA)
036700010509     C                   ENDIF
036800020614     C     §2AFTC        IFEQ      'G'
036900020614     C                   ADD       1             WG
037000020614     C                   MOVEL     TBLKEY        SK2AG(WG)
037100020614     C                   ENDIF
037200010509     C                   ENDIF
037300020614     C     KTABP         READE     TABEL                                  31
037400010509     C                   ENDDO
037500010509     C*
037600010509     C                   ENDSR
037700010509     C*---------------------------------------------------------------*
037800010509
037900010509
038000001218
038100001218     C*------------------------------------------------------------------------*
038200001218     C* *INZSR - ROUTINE INIZIALE
038300001218     C*------------------------------------------------------------------------*
038400001218     C     *INZSR        BEGSR
038500001218     C*
038600010726     C* Ricevo in input il parametro indicante l'ID LANCIO
038700010726     C     *ENTRY        PLIST
038800020613     C                   PARM                    KPJBA
038900020613     C*
039000020613     C                   MOVEL     KPJBU         PARAM
039100010726     C*
039200001218     C                   Z-ADD     1             CODUT
039300001218     C*
039400001218     C* Definizioni chiavi
039500020614     C     KTAS31P       KLIST                                                  TITAS31C - Parziale
039600020613     C                   KFLD                    tasKSC
039700010509     C*
039800020613     C     KEVB04C       KLIST                                                  FNEVB04L - Completa
039900010509     C                   KFLD                    tasAAS
040000020613     C                   KFLD                    tasLNP
040100020613     C                   KFLD                    tasNRS
040200010509     C                   KFLD                    tasNSP
040300020613     C                   KFLD                    wrkCEV            3
040400020613     C*
040500041125     C     KEVB01P       KLIST                                                  FNEVB01L - Parziale
040600020613     C                   KFLD                    tasAAS
040700020613     C                   KFLD                    tasLNP
040800020613     C                   KFLD                    tasNRS
040900020613     C                   KFLD                    tasNSP
041000010509     C*
041100020614     C     KTABP         KLIST                                                  TABEL00F - Parziale
041200010509     C                   KFLD                    CODUT             1 0
041300010509     C                   KFLD                    COD               2
041400020614     C*
041500020614     C     KTABC         KLIST                                                  TABEL00F - Completa
041600020614     C                   KFLD                    CODUT             1 0
041700020614     C                   KFLD                    COD               2
041800020614     C                   KFLD                    KEY               8
041900001218     C*
042000001218     C                   ENDSR
