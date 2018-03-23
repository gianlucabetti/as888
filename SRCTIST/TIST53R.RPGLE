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
002200160208     D DTA4A         E DS
002300011112     D*--------------------
002400011112     D* DS DI RIDEFINIZIOINE CAMPO DATA SPEDIZIONE TAS
002500011112     D*--------------------
002600011112     D                 DS
002700011112     D  TASAAS                        4  0
002800011112     D  TASMGS                        4  0
002900011112     D  TASDSP                 1      8  0
003000010509     D*--------------------
003100010509     D* SCHIERA PER CARICAMENTO CODICI E TIPI MANCATE CONSEGNE
003200010509     D*--------------------
003300020614     D SK2AA           S              3    DIM(300)                             cod. eventi L.A.
003400020614     D SK2AG           S              3    DIM(300)                             cod. eventi GIAC.
003500020613     D*------------------
003600020613     D* DS X PASSAGGIO PARAMETRI
003700020613     D*------------------
003800020613     D PARAM           DS                  INZ
003900020613     D  PARKSC                 1      7  0
004000020613     D  PARDAI                 8     15  0
004100020613     D  PARDAF                16     23  0
004200041125     D  PARCCA                24     24
004300160208     D*--------------------
004400160208     D* DEFINIZIONI A PROCEDURE ESTERNE
004500160208     D*--------------------
004600160208     D/COPY GAITRASRC/SRCPROTOPI,UBTA400R
004700160208     C*---------------------------------------------------------------*
004800160208
004900001218
005000001218
005100001218
005200920812     C*---------------------------------------------------------------*
005300001218     C* MAIN
005400001218     C*---------------------------------------------------------------*
005500010509     C                   exsr      cartbl
005600041125     C*                  exsr      delwfile
005700001218     C                   exsr      procedi
005800160208     C*
005900160208     C* Finita l'elaborazione chiamo driver reperimento TITA4 in chiusura
006000160208     C                   CALL      'UBTA400R'
006100160208     C                   PARM                    UBTA4IOPZ
006200160208     C                   PARM      'C'           UBTA4ITLA
006300160208     C                   PARM                    UBTA4IAAS
006400160208     C                   PARM                    UBTA4ILNP
006500160208     C                   PARM                    UBTA4INRS
006600160208     C                   PARM                    UBTA4INSP
006700160208     C                   PARM                    UBTA4ITRC
006800160208     C                   PARM                    UBTA4OERR
006900160208     C                   PARM                    UBTA4ODS
007000160208     C                   PARM                    UBTA4OLEN
007100160208     C                   PARM                    UBTA4ODATI
007200001218     C*
007300001218     C                   seton                                        LR
007400010509     C*---------------------------------------------------------------*
007500001218
007600001218
007700001218
007800001218     C*------------------------------------------------------------------------*
007900001218     C* PROCEDI - Routine principale
008000001218     C*------------------------------------------------------------------------*
008100020613     C     PROCEDI       BEGSR
008200001218     C*
008300020614     C* Mi posiziono sul TITAS31C per cliente fatturazione
008400020613     C                   Z-ADD     PARKSC        tasKSC
008500020614     C     KTAS31P       SETLL     TITAS31C
008600020613     C*
008700020613     C* Se posizionamento riuscito => ciclo ed elaboro
008800020614     C                   IF        %found(TITAS31C)
008900020614     C                   MOVEL     '0'           flgFINE           1
009000020613     C*
009100020613     C* Prima lettura fuori ciclo
009200020613     C                   EXSR      LEGGITAS
009300020614     C                   DOW       not %eof(TITAS31C) AND
009400020614     C                             flgFINE = '0'
009500020613     C*
009600020613     C* Verifico se la bolla è da considerare
009700020613     C                   EXSR      CHKRECTAS
009800020613     C                   IF        flgRECOK = 'S'
009900160208     C*
010000160208     C* Reprrisco NATURA MERCE
010100160208     C                   EXSR      RTVNAS
010200020613     C*
010300020613     C* Se la bolla è da considerare => elaboro e scrivo il WRK-FILE
010400020613     C                   EXSR      WRIWFILE
010500020613     C                   ENDIF
010600020613     C*
010700020613     C* Proseguo le letture dentro ciclo
010800020613     C                   EXSR      LEGGITAS
010900020613     C                   ENDDO
011000001218     C*
011100020613     C                   ENDIF
011200020613     C*
011300001218     C                   ENDSR
011400001218     C*------------------------------------------------------------------------*
011500020613
011600020613
011700020613
011800020613     C*------------------------------------------------------------------------*
011900020613     C* DELWFILE - Routine di cancellazione record del file inclusi nel periodo di lancio
012000020613     C*------------------------------------------------------------------------*
012100020613     C     DELWFILE      BEGSR
012200020613     C*
012300020613     C                   READ      WFWIND00F
012400020613     C                   DOW       not %eof(WFWIND00F)
012500020613     C                   IF        tasDSP >= PARDAI AND
012600020613     C                             tasDSP <= PARDAF
012700020613     C                   DELETE    WFWIND000
012800020613     C                   ENDIF
012900020613     C                   READ      WFWIND00F
013000020613     C                   ENDDO
013100020613     C*
013200020613     C                   ENDSR
013300020613     C*------------------------------------------------------------------------*
013400020613
013500020613
013600020613
013700020613     C*------------------------------------------------------------------------*
013800020614     C* LEGGITAS - Routine di operazioni preliminari cicliche e lettura record TITAS31C
013900020613     C*------------------------------------------------------------------------*
014000020613     C     LEGGITAS      BEGSR
014100020613     C*
014200020614     C* Inizializzo formato record del WRK-FILE prima della lettura del TITAS31C xchè altrimenti
014300020613     C* perderei il contenuto del buffer (in quanto il WRK-FILE ha i campi denominati come quelli
014400020613     C* di TITAS000
014500020613     C                   CLEAR                   WFWIND000
014600020614     C                   READ      TITAS31C
014700020613     C*
014800020613     C                   ENDSR
014900020613     C*------------------------------------------------------------------------*
015000010627
015100010627
015200010627
015300010627     C*------------------------------------------------------------------------*
015400020613     C* CHKRECTAS - Routine di verifica bolla
015500010627     C*------------------------------------------------------------------------*
015600020613     C     CHKRECTAS     BEGSR
015700010627     C*
015800020613     C                   MOVEL     'S'           flgRECOK          1
015900020613     C*
016000020613     C* Testo se la bolla è del cliente richiesto
016100020613     C                   IF        tasKSC = PARKSC
016200020613     C                   ELSE
016300020613     C                   MOVEL     'N'           flgRECOK
016400020614     C* quando cambia il cliente esco dal ciclo
016500020614     C                   MOVEL     '1'           flgFINE
016600020613     C                   ENDIF
016700020613     C*
016800020613     C* Testo se la bolla è del periodo richiesto
016900020613     C                   IF        tasDSP >= PARDAI AND
017000020613     C                             tasDSP <= PARDAF
017100020613     C                   ELSE
017200020613     C                   MOVEL     'N'           flgRECOK
017300020613     C                   ENDIF
017400020613     C*
017500020613     C* Testo se la bolla è consegnata
017600020613     C                   IF        tasDCM > *zeros
017700020613     C                   ELSE
017800020613     C                   MOVEL     'N'           flgRECOK
017900020613     C                   ENDIF
018000020614     C*
018100020614     C* Testo se il tipo bolla è 'F1' o 'A2'
018200020614     C                   IF        tasTBL = 'F1' OR
018300020614     C                             tasTBL = 'A2'
018400020614     C                   ELSE
018500020614     C                   MOVEL     'N'           flgRECOK
018600020614     C                   ENDIF
018700041125     C*
018800041125     C* Se richiesto nel lancio testo se la bolla ha il codice consegna anomala indicato
018900041125     C                   IF        PARCCA <> '*'
019000041125     C                   IF        tasCCA = PARCCA
019100041125     C                   ELSE
019200041125     C                   MOVEL     'N'           flgRECOK
019300041125     C                   ENDIF
019400041125     C                   ENDIF
019500010627     C*
019600010627     C                   ENDSR
019700010627     C*------------------------------------------------------------------------*
019800001218
019900001218
020000010509
020100010627     C*------------------------------------------------------------------------*
020200020613     C* WRIWFILE - Routine di reperimento informazioni e scrittura WRK-FILE
020300010627     C*------------------------------------------------------------------------*
020400020613     C     WRIWFILE      BEGSR
020500010627     C*
020600020614     C*-----------------
020700020614     C* Verifico condizioni x valorizzazione 4° flag (CONSEGNA ANOMALA DI TIPO "RESO")
020800020614     C*-----------------
020900020614     C                   IF        tasCCA = '2'
021000020614     C                   Z-ADD     1             WFWF04
021100020614     C                   ENDIF
021200020614     C*-----------------
021300020614     C* Verifico condizioni x valorizzazione 3° flag (EVENTI DI TIPO "LASCIATO AVVISO")
021400020614     C*-----------------
021500030324     C* Inizializzo i flag di wrk x ogni bolla
021600030324     C                   MOVEL     '0'           flgEvb017         1
021700030324     C                   MOVEL     '0'           flgEvbAVV         1
021800030324     C                   MOVEL     '0'           flgEvbRIC         1
021900030324     C*
022000041125     C     *LOVAL        SETLL     FNEVB01L
022100041125     C     KEVB01P       SETLL     FNEVB01L
022200020614     C* ...se trovato leggo ed effettuo considerazioni
022300041125     C                   IF        %found(FNEVB01L)
022400041125     C     KEVB01P       READE     FNEVB01L
022500020614     C* ...ciclo
022600041125     C                   DOW       not %eof(FNEVB01L)
022700041125     C*
022800041125     C* Solo se consegna anomala NN reso
022900041125     C                   IF        WFWF04 = *zeros
023000030324     C                   IF        evbCEV = '017'
023100041125     C                   MOVEL     '1'           flgEvb017
023200030324     C                   ENDIF
023300030324     C                   IF        evbCEV = 'AVV'
023400030324     C                   MOVEL     '1'           flgEvbAVV
023500030324     C                   ENDIF
023600030324     C                   IF        evbCEV = 'RIC'
023700030324     C                   MOVEL     '1'           flgEvbRIC
023800030324     C                   ENDIF
023900020614     C                   IF        evbCEV <> 'RIC'
024000020614     C     evbCEV        LOOKUP    SK2AA                                  55
024100020614     C                   IF        %found
024200020614     C                   Z-ADD     1             WFWF03
024300020614     C                   ENDIF
024400020614     C                   ENDIF
024500020614     C                   IF        tasFGC <> 'S'
024600020614     C     evbCEV        LOOKUP    SK2AG                                  55
024700020614     C                   IF        %found
024800020614     C                   Z-ADD     1             WFWF03
024900020614     C                   ENDIF
025000020614     C                   ENDIF
025100041125     C                   ENDIF
025200041125     C*
025300041125     C* Identifico l'ultimo evento d tipo Lasciato Avviso o Giacenza x gli eventi
025400041125     C* inseriti dai P.O. = LNA della bolla
025500041125     C                   IF        evbFLE = tasLNA
025600041125     C     evbCEV        LOOKUP    SK2AA                                  56
025700041125     C     evbCEV        LOOKUP    SK2AG                                  57
025800041125     C                   IF        *IN56 OR *IN57
025900041125     C                   MOVEL     evbCEV        WFWF10
026000041125     C                   ENDIF
026100041125     C                   ENDIF
026200041125     C*
026300041125     C     KEVB01P       READE     FNEVB01L
026400020614     C                   ENDDO
026500020614     C                   ENDIF
026600030324     C*
026700030324     C* Al termine della lettura degli eventi traggo le dovute considerazioni secondo l'ordine
026800030324     C* 1°:017 - 2°:AVV - 3°:RIC
026900030324     C                   IF        flgEvb017 = '1'
027000030324     C                   EVAL      WFWF09 = 1
027100030324     C                   ELSE
027200030324     C                   IF        flgEvbAVV = '1'
027300030324     C                   EVAL      WFWF08 = 1
027400030324     C                   ELSE
027500030324     C                   IF        flgEvbRIC = '1'
027600030324     C                   EVAL      WFWF07 = 1
027700030324     C                   ENDIF
027800030324     C                   ENDIF
027900030324     C                   ENDIF
028000020614     C*-----------------
028100020614     C* Verifico condizioni x valorizzazione 2° flag (RIC)
028200020614     C*-----------------
028300020614     C                   IF        WFWF04 = *zeros AND
028400020614     C                             WFWF03 = *zeros
028500020614     C                   MOVEL     'RIC'         wrkCEV
028600020614     C     *LOVAL        SETLL     FNEVB04L
028700020614     C     KEVB04C       CHAIN     FNEVB04L
028800020614     C* ...se trovato valorizzo flag a ok
028900020614     C                   IF        %found(FNEVB04L)
029000020614     C                   Z-ADD     1             WFWF02
029100020614     C                   ENDIF
029200020614     C                   ENDIF
029300020613     C*-----------------
029400020613     C* Verifico condizioni x valorizzazione 1° flag (MIC)
029500020613     C*-----------------
029600020614     C                   IF        WFWF04 = *zeros AND
029700020614     C                             WFWF03 = *zeros AND
029800020614     C                             WFWF02 = *zeros
029900020613     C                   MOVEL     'MIC'         wrkCEV
030000020614     C     *LOVAL        SETLL     FNEVB04L
030100020614     C     KEVB04C       SETLL     FNEVB04L
030200020613     C* ...se trovato leggo ed effettuo considerazioni
030300020614     C                   IF        %equal(FNEVB04L)
030400020614     C     KEVB04C       READE     FNEVB04L
030500020613     C* ...ciclo
030600020613     C                   DOW       not %eof(FNEVB04L)
030700020613     C                   IF        evbDTV = tasDCM
030800020613     C                   Z-ADD     1             WFWF01
030900020613     C                   ENDIF
031000020614     C     KEVB04C       READE     FNEVB04L
031100020613     C                   ENDDO
031200020613     C                   ELSE
031300020613     C* ...se non trovato imposto il flag a ok
031400020613     C                   Z-ADD     1             WFWF01
031500020613     C                   ENDIF
031600020614     C                   ENDIF
031700020613     C*-----------------
031800020614     C* Se i primi quattro flag nn sono ancora stato valorizzati => cvalorizzo il 6° flag
031900020614     C*-----------------
032000020614     C                   IF        WFWF01 = *zeros AND
032100020614     C                             WFWF02 = *zeros AND
032200020614     C                             WFWF03 = *zeros AND
032300020614     C                             WFWF04 = *zeros
032400020614     C                   Z-ADD     1             WFWF06
032500020614     C                   ENDIF
032600020614     C*-----------------
032700020613     C* Verifico condizioni x valorizzazione 5° flag (HA AVUTO GIACENZA)
032800020613     C*-----------------
032900020613     C                   IF        tasFGC = 'S'
033000020613     C                   Z-ADD     1             WFWF05
033100020613     C                   ENDIF
033200020613     C*-----------------
033300020614     C* Reperisco distretto, area e regione (sia cod. che desc.) relativamente alla LNA della bolla
033400020613     C*-----------------
033500020613     C     tasLNA        CHAIN     AZORG01L
033600020613     C                   IF        %found(AZORG01L)
033700020613     C                   EVAL      WFWDIS = ORGFL3
033800020613     C                   EVAL      WFWARA = ORGCAR
033900020613     C                   EVAL      WFWCRA = ORGCRE
034000020614     C                   EXSR      REPDESREG
034100020613     C                   ENDIF
034200160208     C*
034300160208     C                   EVAL      tasNAS = §TA4ANAS
034400020613     C*
034500020613     C* Al termine scrivo il record così valorizzato
034600020613     C                   WRITE     WFWIND000
034700020613     C*
034800010627     C                   ENDSR
034900010509     C*------------------------------------------------------------------------*
035000020614
035100020614
035200020614
035300020614
035400020614     C*------------------------------------------------------------------------*
035500020614     C* REPDESREG - Routine di reperimento descrizione regione linea di arrivo
035600020614     C*------------------------------------------------------------------------*
035700020614     C     REPDESREG     BEGSR
035800020614     C*
035900020614     C                   MOVEL     '14'          COD
036000020614     C                   MOVEL(P)  WFWCRA        KEY
036100020614     C     KTABC         CHAIN     TABEL00F
036200020614     C                   IF        %found(TABEL00F)
036300020614     C     TBLFLG        IFEQ      ' '
036400020614     C                   MOVEL     TBLUNI        DS14
036500020614     C                   MOVEL     §14DES        WFWDRA
036600020614     C                   ENDIF
036700020614     C                   ENDIF
036800020614     C*
036900020614     C                   ENDSR
037000020614     C*---------------------------------------------------------------*
037100010626
037200010626
037300010626
037400010626
037500010509     C*------------------------------------------------------------------------*
037600020613     C* CARTBL - Routine di caricamento dati tabellati
037700010509     C*------------------------------------------------------------------------*
037800010509     C     CARTBL        BEGSR
037900010509     C*
038000020614     C                   Z-ADD     0             WA                4 0
038100020614     C                   Z-ADD     0             WG                4 0
038200020614     C                   CLEAR                   SK2AA
038300020614     C                   CLEAR                   SK2AG
038400010509     C                   MOVEL     '2A'          COD
038500020614     C     KTABP         CHAIN     TABEL00F                           31
038600010509     C     *IN31         DOWEQ     '0'
038700010509     C     TBLFLG        IFEQ      ' '
038800010509     C                   MOVEL     TBLUNI        DS2A
038900020613     C     §2AFTC        IFEQ      'A'
039000020614     C                   ADD       1             WA
039100020614     C                   MOVEL     TBLKEY        SK2AA(WA)
039200010509     C                   ENDIF
039300020614     C     §2AFTC        IFEQ      'G'
039400020614     C                   ADD       1             WG
039500020614     C                   MOVEL     TBLKEY        SK2AG(WG)
039600020614     C                   ENDIF
039700010509     C                   ENDIF
039800020614     C     KTABP         READE     TABEL                                  31
039900010509     C                   ENDDO
040000010509     C*
040100010509     C                   ENDSR
040200010509     C*---------------------------------------------------------------*
040300160208
040400160208
040500160208
040600160208     C     RTVNAS        BEGSR
040700160208     C*
040800160208     C* Inizializzo le DS relative ai tipi record del TITA4 da gestire
040900160208     C                   CLEAR                   DTA4A
041000160208     C*
041100160208     C* Reperisco dal tipo record 'A' del TITA4 la natura merce della bolla corrente
041200160208     C                   CALL      'UBTA400R'
041300160208     C                   PARM      *blanks       UBTA4IOPZ
041400160208     C                   PARM      *blanks       UBTA4ITLA
041500160208     C                   PARM      tasAAS        UBTA4IAAS
041600160208     C                   PARM      tasLNP        UBTA4ILNP
041700160208     C                   PARM      tasNRS        UBTA4INRS
041800160208     C                   PARM      tasNSP        UBTA4INSP
041900160208     C                   PARM      'A'           UBTA4ITRC
042000160208     C                   PARM                    UBTA4OERR
042100160208     C                   PARM                    UBTA4ODS
042200160208     C                   PARM                    UBTA4OLEN
042300160208     C                   PARM                    UBTA4ODATI
042400160208     C*
042500160208     C                   IF        UBTA4OERR = *zeros
042600160208     C                   SELECT
042700160208     C* Gestione output tipo record 'A'
042800160208     C                   WHEN      UBTA4ODS = 'DTA4A'
042900160208     C                   EVAL      DTA4A = %subst(UBTA4ODATI:1:UBTA4OLEN)
043000160208     C                   ENDSL
043100160208     C*
043200160208     C                   ENDIF
043300160208     C*
043400160208     C                   ENDSR
043500010509
043600010509
043700001218
043800001218     C*------------------------------------------------------------------------*
043900001218     C* *INZSR - ROUTINE INIZIALE
044000001218     C*------------------------------------------------------------------------*
044100001218     C     *INZSR        BEGSR
044200001218     C*
044300010726     C* Ricevo in input il parametro indicante l'ID LANCIO
044400010726     C     *ENTRY        PLIST
044500020613     C                   PARM                    KPJBA
044600020613     C*
044700020613     C                   MOVEL     KPJBU         PARAM
044800010726     C*
044900001218     C                   Z-ADD     1             CODUT
045000001218     C*
045100001218     C* Definizioni chiavi
045200020614     C     KTAS31P       KLIST                                                  TITAS31C - Parziale
045300020613     C                   KFLD                    tasKSC
045400010509     C*
045500020613     C     KEVB04C       KLIST                                                  FNEVB04L - Completa
045600010509     C                   KFLD                    tasAAS
045700020613     C                   KFLD                    tasLNP
045800020613     C                   KFLD                    tasNRS
045900010509     C                   KFLD                    tasNSP
046000020613     C                   KFLD                    wrkCEV            3
046100020613     C*
046200041125     C     KEVB01P       KLIST                                                  FNEVB01L - Parziale
046300020613     C                   KFLD                    tasAAS
046400020613     C                   KFLD                    tasLNP
046500020613     C                   KFLD                    tasNRS
046600020613     C                   KFLD                    tasNSP
046700010509     C*
046800020614     C     KTABP         KLIST                                                  TABEL00F - Parziale
046900010509     C                   KFLD                    CODUT             1 0
047000010509     C                   KFLD                    COD               2
047100020614     C*
047200020614     C     KTABC         KLIST                                                  TABEL00F - Completa
047300020614     C                   KFLD                    CODUT             1 0
047400020614     C                   KFLD                    COD               2
047500020614     C                   KFLD                    KEY               8
047600001218     C*
047700001218     C                   ENDSR
