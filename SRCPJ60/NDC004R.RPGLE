000100980922     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)
000200980922     H*PARMS BNDDIR(PJXBND PJCBND NDC004BND)
000300980922     H*PARMS ACTGRP(PNOTA) CVTOPT(*DATETIME)
000400950727     H DECEDIT('0,') DATEDIT(*dmy)
000500961219      *
000600961219      *  Questo programma è composto anche dai moduli NDC010R e NDC015R !!!
000700990528C1129 *  e NDC019R !!!
000800010102C1456 *  e NDC515R !!!
000900021115C1530 *  e NDC516R !!!
001000961219      *  (quindi va compilato successivamente a questi)
001100961219      *
001200941115      *
001300950614      *            Decisore sequenza prime note
001400950614      *           ------------------------------
001500940224      *
001600940224      *
001700940224      * DOCUMENTI MICROANALISI :
001800940224      *
001900940224      * NOTE TECNICHE :
002000940224      *
002100940224      *
002200940224      * PERSONALIZZAZIONI
002300940224      *
002400040309      *
002500040309      * MODIFICHE:
002600040309      *
002700040309      *  D0229 : aggiunto aggiornamento estremi partita, perchè è
002800040309      *          possibile modificarli quando non è ancora avvenuto
002900040309      *          il pagamento della fattura.
003000010621C1494 *  C1494 : viene utilizzata al posto della data di documento la
003100010621  "   *          data salvata in REGDTLIB2 definita decorrenza
003200010621C1494 *          pagamento al richiamo dei programmi di gestione rate
003300940224      *
003400940224      *
003500940314      *               MAPPA INDICATORI
003600940224      *
003700940307      *  21           GENERICO OPERAZIONI I/O
003800940224      *  22           GENERICO ERRORE OPERAZIONI I/O
003900940224      *  30           SFLDSP
004000940224      * N31           SFLCLR
004100940224      *  31           SFLDSPCTL
004200940224      *  32           SFLNXTCHG
004300940224      *  33           SFLEND
004400940224      *  39           OF PRTF
004500940224      *  40 <---> 49  DSPATR ERRORI SU SFL
004600950524      *  Specificare l'uso dei singoli indicatori
004700940224      *  50 <---> 98  ERRORI SU VIDEO
004800950524      *  Specificare l'uso dei singoli indicatori
004900950526      *  52  ERRORE STANDARD : LA FUNZIONE CHIAMATA SI E' CHIUSA IN ERRORE
005000950526      *  54  ERRORE STANDARD : L'UTENTE NON E' AUTORIZZATO ALLA FUNZIONE ...
005100950526      *  96  ERRORE SPECIALE : ESISTONO ERRORI SU ALTRE VIDEATE
005200950526      *  97  ERRORE SPECIALE : TASTO   NON ABIL.
005300950526      *  98  ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
005400950526      *  99           INDIC. GENERALE DI ERRORE
005500940128     F*----------------------------------------------------*
005600951011     FANALR01L  IF   E           K DISK    usropn
005700940128     D*----------------------------------------------------*
005800950525     D*-------------
005900950525     D* Costanti
006000950920     D Dt0             C                   Const('0001-01-01')
006100950525     D*-------------
006200940211     D* Passaggio Parametri
006300020118     D KPJBA         E DS
006400940211     D*-------------
006500940225     D* Reperimento Autorizzazioni
006600941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006700941124     D*-------------
006800950410     D* Passaggio Autorizzazioni
006900950410     D DSAUT           DS           512
007000950524     D*-------------
007100950524     D* Maschera dati autorizzazioni
007200950524     D CONPNODS      E DS
007300950615     D  IVA                    1     20
007400950615     D  NonIVA                21     40
007500950615     D*-------------
007600950615     D* Dati di ambiente ottenuti da XSOC
007700950615     D SOC001        E DS                  EXTNAME(XSOC001DS)
007800020118C1530D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
007900941209     D*-------------
008000941209     D* DS Interna per dati di output di XSOC
008100941209     D XSOCDS          DS          1000
008200940325     D*-------------
008300940325     D* Parametri in ricezione
008400950614     D C004DS        E DS                  EXTNAME(NDC004DS)
008500951011     D NDIVA         E DS                   Extname(NDIVA00F) inz
008600951011     D  IVADTREG     E                      inz(Dt0)
008700951011     D  IVADTDOC     E                      inz(Dt0)
008800951011     D  IVADTRIF     E                      inz(Dt0)
008900951011     D  IVADTCOMP    E                      inz(Dt0)
009000970124     D*-------------
009100970124     D* Parametri per pn collegata
009200970124     D NDC009DS      E DS
009300110301D2670D*-------------
009400110301  "  D* Parametri del PGM chiusura
009500110301  "  D NDC037DS      E DS
009600110301  "  D  RET037       E                     INZ('0')
009700110301  "  D  OPR037       E                     INZ('0')
009800110301D2670D  ERR037       E                     INZ('0')
009900110301     D*-------------
010000950621     D* Parametri del PGM di testata non IVA
010100950621     D NDC010DS      E DS                  INZ
010200950616     D  RET010       E                     INZ('0')
010300950616     D  OPR010       E                     INZ('0')
010400950616     D  ERR010       E                     INZ('0')
010500950920     D  DTOPE010     E                     INZ(DT0)
010600950920     D  DTVAL010     E                     INZ(DT0)
010700951025     D  ImmExp010    E                     INZ('0')
010800950621     D*-------------
010900950719     D* Parametri del PGM di testata IVA
011000950719     D NDC015DS      E DS                  INZ
011100950719     D  RET015       E                     INZ('0')
011200950719     D  OPR015       E                     INZ('0')
011300950719     D  ERR015       E                     INZ('0')
011400951012     D  ImmExp015    E                     INZ('0')
011500950920     D  DTpar015     E                     INZ(DT0)
011600950801     D*-------------
011700950801     D* Parametri del PGM di rate IVA
011800950801     D NDC016DS      E DS                  INZ
011900950801     D  RET016       E                     INZ('0')
012000950801     D  OPR016       E                     INZ('0')
012100950801     D  ERR016       E                     INZ('0')
012200950920     D  DTreg016     E                     INZ(DT0)
012300950920     D  DTdoc016     E                     INZ(DT0)
012400950920     D  DTpar016     E                     INZ(DT0)
012500950822     D* schiera delle rate aggiornate
012600950908     D RateDDS         DS
012700040309c1248x*** RateD                         62    DIM(100)
012800991014c1248D RateD                         62    DIM(200)
012900950915     D*-------------
013000951113     D* Parametri del PGM di stampa autofattura
013100951113     D NDC168DS      E DS                  INZ
013200951113     D  ERR168       E                     INZ('0')
013300951113     D*-------------
013400950915     D* Parametri del PGM di aggiornamento rate
013500950915     D NDC018DS      E DS                  INZ
013600950915     D  ERR018       E                     INZ('0')
013700950920     D  DTPAR018     E                     INZ(DT0)
013800950920     D*-------------
013900950726     D* Parametri del PGM di righe
014000950621     D NDC020DS      E DS                  INZ
014100950621     D  RET020       E                     INZ('0')
014200950621     D  OPR020       E                     INZ('0')
014300950621     D  ERR020       E                     INZ('0')
014400950920     D  DTOPE020     E                     inz(DT0)
014500950920     D  DTVAL020     E                     inz(DT0)
014600950920     D*-------------
014700951012     D* Parametri del PGM di chiamata gestione autofatture
014800951012     D NDC036DS      E DS                  INZ
014900951012     D  Esito036     E                     INZ('0')
015000951012     D  DTPAR036     E                     inz(dt0)
015100951012     D*-------------
015200951012     D* Parametri del PGM di chiamata gestione effetti
015300951012     D NDC038DS      E DS                  INZ
015400951012     D  ERR038       E                     INZ('0')
015500951012     D  DTREG038     E                     inz(dt0)
015600951012     D  DTDOC038     E                     inz(dt0)
015700951012     D  DTLGIO038    E                     inz(dt0)
015800950824     D*-------------
015900950824     D* Parametri del PGM di chiusura P.N.
016000950824     D NDC039DS      E DS                  INZ
016100950824     D  RET039       E                     INZ('0')
016200950824     D  OPR039       E                     INZ('0')
016300950824     D  ERR039       E                     INZ('0')
016400950920     D  DTreg039     E                     INZ(DT0)
016500950920     D  DTdoc039     E                     INZ(DT0)
016600950920     D  DTpar039     E                     INZ(DT0)
016700961209     D*-------------
016800961209     D* Parametri del PGM di allineamento dati doc. ritenute d'acconto
016900961209     D NDCR03DS      E DS                  INZ
017000950615     D*-------------
017100960806     D* DS esterna per default di causale
017200960806     D ANDFT         E DS                  EXTNAME(ANDFT00F)
017300960806     D*-------------
017400950615     D* Autorizzazioni alla contabilità
017500950615     D XAutRegDS     E DS
017600951005     D*-------------
017700951005     D* Parametri I/O bottoni registrazione
017800951005     D ND003DS       E DS
017900950615     D*-------------
018000950615     D* DS Esterna gestione messaggi per COMIT
018100950615     D XMSGDS        E DS
018200950615     D  STS          E                     EXTFLD(MSGSTS)
018300950615     D                                     DIM(333)
018400950615     D  JBA          E                     EXTFLD(MSGJBA)
018500950615     D                                     DIM(502)
018600980119     D*-------------
018700980119     D* messaggi per XMSGR
018800980119     D MsgID           s             27a   Dim(2) ctdata perrcd(1)
018900980119     D MsgT            s             70a   Dim(5)
019000950619     D*-------------
019100950615     D* Variabili per gestione chiamate ai pgm
019200950615     D $Gest           S              3A
019300950615     D $Fine           S              1A
019400950622     D $Inzxxx         S              1A
019500950615     D $Catena         S              3A   dim(20)
019600950822     D*-------------
019700950822     D* Definizioni parametri standard chiamate moduli
019800950822     D Esito           S              1
019900950907     D GesMsg          S              1
020000040309     D Operazione      S              2
020100040309     D StrutturaO      S             10
020200951005     D LenOut          S              9B 0
020300951005     D WFun            DS
020400951005     D  Fun                           1     dim(100)
020500951011     D*-------------
020600951012     D* campi percipienti
020700951011     D CodOper         S              1
020800951011     D OPENALR         S              1    Inz('0')
020900951011     D WAVERE          S                   Like(Importo039)
021000951011     D WDARE           S                   Like(Importo039)
021100981015     D WAVERED         S                   Like(Importo039)
021200981015     D WDARED          S                   Like(Importo039)
021300951010     D*-------------
021400951010     D* Definizioni parametri NON standard chiamate moduli
021500951010     D NewSys          S                   Like(Sys004)
021600951010     D NewNrAsReg      S                   Like(NrAsReg004)
021700970128     D*-------------
021800970128     D* Numero assoluto registrazione per pncollegata
021900970128     D PncSys          S                   Like(Sys004)
022000970128     D PncNrAsReg      S                   Like(NrAsReg004)
022100951010     D*-------------
022200951010     D* Campi/DS importati
022300951010     D £NDREG        E DS                    extname(NDREG00F)  import
022400960806     D £ANOPETes     E DS                    extname(ANOPE00F)  import
022500951013     D £StReg          S              1                         import
022600970122     D  £Nrasreg       s                   like(REGNRASREG) import
022700950615     D*-------------
022800950615     D* Variabili appoggio sempre presenti in tutte le anagrafiche
022900040309     D WOprSingle      S                   like(Opr004)
023000040309     D WOprGlobal      S                   like(Opr004)
023100040309     D ChiestaCol      S              1
023200950615     D*-------------
023300950615     D* Variabili appoggio specifiche della singola anagrafica
023400950615     D*  campi per parzializzazioni su rcd (solo alfanumerici)
023500950619     D Retxxx          S                   like(Ret004)
023600950920     D DtOpe           S             10    inz(DT0)
023700950920     D DtVal           S             10    inz(DT0)
023800951011     D WNDREG          S                   like(£NDREG)
023900951016     D ErrAutoFat      S              1
024000951016     D ErrGenRate      S              1
024100951113     D WAftSta         S              1
024200960729     D ModoCopia       S              1
024300960806     D WRepDft         S                   like(OPERepDft)
024400961202     D aftacq004       S                   like(OPEaftacq)
024500970130     D SavCausale      S                   like(CausTes015)
024600950615     D*  Indici
024700950615     D X               S              3S 0
024800950526     D*-------------
024900940127     D* Reperimento nome PGM
025000940127     D STATUS         SDS           333
025100940127     D  DSPGM            *PROC
025200950411     D  PARMS            *PARMS
025300960514     D*-------------
025400960514     D* Flag registrazione 02
025500960514     D                 DS
025600960514     D  FlgRegDS                           like(REGFlgReg)
025700960514     D  FlgReg02              02     02
025800950609     D*-------------
025900961209     D* Campi di salvataggio per aggiornamento ritenute d'acconto
026000961209     D SavDtDoc        S                   Like(RegDtDoc)
026100961209     D SavNrDoc        S                   Like(RegNrDoc)
026200961209     D SavSerDoc       S                   Like(RegSerDoc)
026300961209     D SavCausTes      S                   Like(RegCausTes)
026400020212C1530D*-------------
026500020212  "  D* campone contenente le chiavi ProJ per il documento su Laguna
026600020212  "  D* al momento dell'APERTURA della registrazione, cioè prima di
026700020212  "  D* ogni possibile modifica
026800020212  "  D* (servono solo in modifica per sapere se sono state variate);
026900020212  "  D* uso una schiera grande come le 8 chiavi primarie di Laguna che
027000020212  "  D* sono diverse per ogni tipo registrazione. Elenco:
027100020212  "  D*  Doc.IVA Fornitore : (F)attura o (N)ota/estremi doc./sottoconto
027200020212  "  D*  Doc.IVA Cliente   : estremi doc.
027300020212C1530D KeyLgn          S             15A   dim(8) import
027400030124D1270D*-------------
027500030124  "  D* campone contenente i descrittori ProJ per il documento su
027600030124  "  D* Laguna al momento dell'APERTURA della registrazione, cioè prima
027700030124  "  D* di ogni possibile modifica
027800030124  "  D* (servono solo in modifica per sapere se sono stati variati);
027900030124  "  D* uso una schiera grande come i 10 descrittori di Laguna che
028000030124  "  D* sono diversi per ogni tipo registrazione.
028100030124D1270D DesLgn          S             44A   dim(10) import
028200021115C1605D*-------------
028300021115  "  D XPAOUT          DS          2000
028400040205C1605D*-------------
028500040205C1722X*ex C1605 ANP062DS      E DS
028600040205C1722D £ANP062DS     E DS                  extname(ANP062DS)  export
028700040205C1605D*-------------
028800021115  "  D* memo se non c'è il parametro modulo LAGUNA
028900021115  "  D NoParmLGN       s              1a
029000021115C1605D*-------------
029100030225C1627 * Variabile globale
029200030225  "   * (aggiunta per poter gestire knsif in ndmvc116
029300030225C1627D £KPJBA          s                   Import like(KPJBA)
029400030819C1679 *-------------
029500030819  "   * Per reperire dati I° movimento reg. generante
029600030819  "   * la reg. in oggetto (pn collegata)
029700030819  "  D Refresh         s              1
029800030819  "  D NDMOV00F      E DS                  inz
029900030819  "   * Per reperire dati causale reg. generante
030000030819  "   * la reg. in oggetto (pn collegata)
030100030819  "  D ANOPEPnC      E DS                    extname(ANOPE00F) prefix(OP_:3)
030200030819  "   * appoggio
030300030819  "  D $DataISO        s               D
030400030819  "   * Per non 'sporcare' i dati di ANOPE della reg. in oggetto
030500030819  "   * (se reperisco dati causale reg. generante
030600030819  "   * la reg. in oggetto (pn collegata))
030700030819  "  D SAVANOPETes     s                   inz like(£ANOPETes)
030800030819  "  D $NrRigaM        s                   inz like(MOVNrRigaM)
030900030819  "   * Parametri per controllo codice ANATB00F
031000030819  "  D XATBDS        E DS                  INZ
031100030819  "   * tabella pn collegata
031200030819C1679D ANG001DS      E DS                  inz
031300031117C1704D*
031400031117  "  D*-------------
031500031117  "  D* parametri chiamata user exit filtro catalogazione
031600031117  "  D*
031700031117  "  D* classe proj in elaborazione
031800031117  "  D ClassePJ        s              3A
031900031117  "  D* lunghezza effettiva del record
032000031117  "  D LungRcd         s              5s 0
032100031117  "  D* tipo record -> nome del rcd da usare
032200031117  "  D TipoRcd         s             10A
032300031117  "  D* flag catalogabile sì o no
032400031117  "  D Catalogabile    s              1A
032500031117C1704D*
032600051010C1871D* DS per controllo ratei
032700051010  "  D NDC326DS      E DS                  INZ
032800051010  "  D  DTreg326     E                     INZ(DT0)
032900051010C1871D*-------------
033000080916D2369 * Ds di ritorno da NDC015R
033100080916D2369D NDC019DS      E DS
033200090121C2115D*-------------
033300090121  "   * Ds parametro modulo port. passivo
033400090121C2115D NDCHH5DS      E DS
033500110303D2670X*campo di passaggio ANDFT           Ex D2612
033600110303D2670X*DFTInput        S           4000   Ex D2612
033700940127     C*----------------------------------------------------*
033800940127     C*                MAIN LINE PROGRAM
033900940127     C*----------------------------------------------------*
034000940127     C*
034100940223     C* inizializzazione variabili
034200940223     C                   EXSR      INZVAR
034300940223     C*
034400940223     C     $FINE         DOWEQ     *OFF
034500950622     C     $GEST         CASEQ     '010'         Gest010
034600950719     C     $GEST         CASEQ     '015'         Gest015
034700950801     C     $GEST         CASEQ     '016'         Gest016
034800950809     C     $GEST         CASEQ     '017'         Gest017
034900950920     C     $GEST         CASEQ     '018'         Gest018
035000950622     C     $GEST         CASEQ     '020'         Gest020
035100960605     C     $GEST         CASEQ     '028'         Gest020
035200950822     C     $GEST         CASEQ     'XEX'         GestXEX
035300950822     C     $GEST         CASEQ     '039'         Gest039
035400951011     C     $GEST         CASEQ     '036'         Gest036
035500970124     C     $GEST         CASEQ     '009'         Gest009
035600970124     C     $GEST         CASEQ     'FIN'         GestFine
035700941027     C                   ENDCS
035800941027     C                   ENDDO
035900940325     C* fine programma
036000940325     C                   EXSR      ENDPGM
036100940225     C*
036200940325     C************************************************************
036300940325     C* FINE PROGRAMMA
036400940325     C************************************************************
036500940325     C     ENDPGM        BEGSR
036600960403     C*
036700960403     C* se la registrazione su cui abbiamo operato non è stata rilasciata
036800960403     C* per un non ben precisato motivo
036900960403     C                   IF        £StReg <> *blank
037000960403     C* rilascio registrazione
037100960403     C                   CALLB     'NDMVC115'
037200960403     C                   ENDIF
037300940325     C*
037400940325     C* passaggio dati al pgm chiamante
037500950615     C*
037600950615     C* se siamo in immissione passo indietro il flag globale perchè potrei
037700950615     C* avere immesso più di una registrazione e poi aver premuto un tasto
037800950615     C* funzionale tipo F3 o F12
037900950615     C                   IF        Opz004 = '01'
038000950615     C                   MOVEL     WOprGlobal    Opr004
038100950615     C*
038200950615     C* se non siamo in immissione passo indietro il flag della singola
038300950615     C* registrazione perchè tanto lavoro sempre e solo 1 registrazione
038400950615     C* alla volta
038500950615     C                   ELSE
038600950615     C                   MOVEL     WOprSingle    Opr004
038700950615     C                   ENDIF
038800950615     C*
038900041118c1801c* ripasso sys/nrasreg che sono cambiati in caso di imms/copia
039000041118  "  c                   eval      SysNew004 = sys039
039100041118c1801c                   eval      NrasNew004= NrasReg039
039200950615     C                   MOVEL     C004DS        KPJBU
039300940325     C*
039400940325     C                   SETON                                            RT
039500940616     C                   RETURN
039600940325     C*
039700940325     C                   ENDSR
039800940224     C************************************************************
039900950615     C* Chiamata pgm testata documenti non IVA
040000940224     C************************************************************
040100950622     C     Gest010       BEGSR
040200950704     C*
040300950704     C* se la videata va inizializzata
040400950704     C                   IF        $Inzxxx = *on
040500950704     C* significa che vado avanti di 1 pgm nella catena di chiamate
040600950704     C                   EVAL      X = X + 1
040700950704     C                   MOVE      $Gest         $Catena(X)
040800950704     C                   ENDIF
040900940224     C*
041000950622     C                   RESET                   NDC010DS
041100950623     C                   MOVEL     Opz004        OPZ010
041200950615     C* avverto il pgm se deve inizializzarsi o meno
041300950622     C                   MOVEL     $Inzxxx       Inz010
041400950619     C* passo tutti quei campi che possono servire al pgm
041500950619     C                   MOVEL     Ctb004        Ctb010
041600950619     C                   MOVEL     TpRegz004     TpRegz010
041700950619     C                   MOVEL     CausTes004    CausTes010
041800950619     C                   MOVEL     CliFor004     CliFor010
041900950619     C                   MOVEL     SovrEse004    SovrEse010
042000950622     C                   MOVEL     TabPNot004    TabPNot010
042100950619     C                   MOVEL     Unita004      Unita010
042200951025     C                   IF        TakeOut004 = *on
042300951025     C                   MOVEL     *on           ImmExp010
042400951025     C                   ENDIF
042500950615     C*
042600950911     C                   CALLB     'NDC010R'
042700950615     C                   PARM                    KPJBA
042800950807     C                   PARM                    NDC010DS
042900960807     C                   PARM                    ANDFT
043000040402D1573C                   eval      TabPNot004 =  TabPNot010
043100950622     C* spengo l'inizializzazione prossimo pgm chiamato
043200950622     C* (verra attivata nella routine Decide___ solo se si passa ad un
043300950622     C*  nuovo pgm della catena)
043400950622     C                   EVAL      $Inzxxx = *off
043500950615     C* ritorno da PGM gestione
043600950616     C                   EXSR      RTN010
043700950525     C*
043800940117     C                   ENDSR
043900940223     C************************************************************
044000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
044100940223     C************************************************************
044200950616     C     RTN010        BEGSR
044300940223     C*
044400940223     C* modo di ritorno
044500940223     C*
044600040309     C                   SELECT
044700950615     C* 1 = F3
044800950616     C     RET010        WHENEQ    '1'
044900950616     C                   EVAL      Retxxx = Ret010
045000950615     C* testo se ho aggiornamenti attivi per la singola registrazione
045100950615     C* in previsione di uscire dal pgm se non ne ho
045200950615     C                   EXSR      TstOpr
045300940223     C* 2 = F12
045400950616     C     RET010        WHENEQ    '2'
045500950615     C* se esiste un pgm chiamato precedentemente
045600040309     C                   IF        X > 1
045700950615     C* memorizzo che torno indietro di 1 pgm nella catena di chiamate
045800950704     C                   EVAL      X = X - 1
045900950615     C* ritorno al pgm chiamato precedentemente
046000950705     C                   MOVE      $Catena(X)    $Gest
046100950615     C* altrimenti
046200040309     C                   ELSE
046300950616     C                   EVAL      Retxxx = Ret010
046400950615     C* testo se ho aggiornamenti attivi per la singola registrazione
046500950615     C* in previsione di uscire dal pgm se non ne ho
046600950615     C                   EXSR      TstOpr
046700040309     C                   ENDIF
046800940223     C*
046900040309     C                   ENDSL
047000940223     C*
047100940223     C* operazione eseguite dal pgm chiamato
047200940223     C*
047300040309     C                   SELECT
047400950615     C* 1 = eseguito aggiornamento
047500950616     C     OPR010        WHENEQ    '1'
047600950727     C* oppure siamo in visualizzazione
047700950727     C     Opz010        OREQ      '05'
047800950808     C* ed è stato scelto di andare avanti nella catena dei pgm
047900950808     C     Pgm010        ANDNE     *blank
048000950808     C     Pgm010        ANDNE     'FINE      '
048100950615     C* memorizzo che è stato eseguito un aggiornamento
048200950616     C                   MOVE      Opr010        WOprSingle
048300950727     C* memo le date non salvabili su database che servono ai successivi pgm
048400950727     C                   MOVE      DtOpe010      DtOpe
048500950727     C                   MOVE      DtVal010      DtVal
048600950615     C* esegue i test per decidere che pgm chiamare ora
048700950616     C                   EXSR      Decide010
048800950615     C* 0 = non eseguito aggiornamento
048900040309     C     OPR010        WHENEQ    '0'
049000040309     C* ed è stato scelto di uscire dalla catena dei pgm
049100040309     C     Pgm010        ANDEQ     'FINE      '
049200040309     C                   EVAL      Retxxx = Ret010
049300040309     C* testo se ho aggiornamenti attivi per la singola registrazione
049400040309     C* in previsione di uscire dal pgm se non ne ho
049500040309     C                   EXSR      TstOpr
049600040309     C*
049700040309     C                   ENDSL
049800040309     C*
049900040309     C* funzione non eseguibile per errore :
050000040309     C*
050100040309     C                   SELECT
050200040309     C* 1 = funzione richiamata chiusa in qualunque errore
050300040309     C     ERR010        WHENNE    '0'
050400040309     C* rilascio registrazione
050500040309     C                   CALLB     'NDMVC115'
050600950615     C* esco da pgm
050700970124     C                   MOVE      *on           $FINE
050800950615     C* informo il chiamante che si è generato un errore
050900950615     C* passandogli il valore della Errxxx ricevuta
051000000822     C                   EVAL      Err004 = Err010
051100000822C1410 *
051200000822  "   * se si è verificato un errore sulle autorizzazioni
051300000822  "  C                   If        Err010     = 'B'  Or
051400000822  "  C                             Err010     = 'C'  Or
051500000822  "  C                             Err010     = 'D'  Or
051600000822  "  C                             Err010     = 'E'  Or
051700000822  "  C                             Err010     = 'F'
051800000822  "   *  Err004='2' indica il verificarsi dell'errore in modo generico
051900000822  "  C                   Eval      Err004     = '2'
052000000822  "   *  e il 1° carattere  di Flags004, il tipo di errore specifico
052100000822  "   *  (così NDC001/2R emette l'errore esatto)
052200000822  "  C                   Eval      %Subst(Flags004:1:1)=Err010
052300000822C1410C                   EndIf
052400950616     C*
052500040309     C                   ENDSL
052600040309     C*
052700040309     C                   ENDSR
052800040309     C************************************************************
052900040309     C* Decide chi chiamare dopo la testata doc.non IVA
053000040309     C************************************************************
053100040309     C     Decide010     BEGSR
053200040309     C*
053300040309     C                   SELECT
053400040309     C*
053500040309     C* richiesta di aprire il pgm dei movimenti
053600040309     C                   WHEN      Pgm010 = 'RIGHE     '
053700040309     C* se il default del tipo sfl prevede la gestione normale
053800040309     C                   IF        TpSfl010 = '1'  or
053900950622     C* oppure non c'è nessun default
054000950622     C                             TpSfl010 = *blank
054100950622     C* richiamo la gestione std
054200950703     C                   EVAL      $Gest = '020'
054300950622     C* altrimenti
054400950622     C                   ELSE
054500950622     C* richiamo la gestione a sfl aperto
054600960605     C                   EVAL      $Gest = '028'
054700950622     C                   ENDIF
054800950712     C*
054900950712     C* richiesta di ritorno al chiamante dopo conferma
055000950712     C                   WHEN      Pgm010 = 'FINE      '
055100950712     C* esco da pgm
055200970124     C                   MOVE      'FIN'         $GEST
055300950622     C*
055400950622     C                   ENDSL
055500950704     C*
055600950622     C* richiedo l'inizializzazione della videata del pgm da chiamare
055700950622     C                   EVAL      $Inzxxx = *on
055800950616     C*
055900950616     C                   ENDSR
056000950719     C************************************************************
056100950719     C* Chiamata pgm testata documenti IVA
056200950719     C************************************************************
056300950719     C     Gest015       BEGSR
056400950719     C*
056500950719     C* se la videata va inizializzata
056600950719     C                   IF        $Inzxxx = *on
056700950719     C* significa che vado avanti di 1 pgm nella catena di chiamate
056800950719     C                   EVAL      X = X + 1
056900950719     C                   MOVE      $Gest         $Catena(X)
057000950719     C                   ENDIF
057100961209     c*
057200961209     C* Salvo estremi documento e causale
057300961209     C                   eval      SavDtDoc  = RegDtDoc
057400961209     C                   eval      SavNrDoc  = RegNrDoc
057500961209     C                   eval      SavSerDoc = RegSerDoc
057600961209     C                   eval      SavCausTes= RegCausTes
057700950719     C*
057800950719     C                   RESET                   NDC015DS
057900951012     C* se la chiamata prevede la gestione di una registrazione non
058000951012     C* scritta in modo ortodosso :
058100951012     C                   SELECT
058200951012     C*  autofattura non completata per errori
058300951016     C                   WHEN      ErrAutoFat = *on
058400951012     C* se la registrazione in questione è in scrittura
058500951012     C                   IF        £StReg = '1'
058600951012     C* richiedo l'immissione
058700951012     C                   MOVEL     '01'          OPZ015
058800951012     C* imposto la richiesta di recupero dati da database
058900951012     C                   MOVE      '1'           ImmExp015
059000951012     C* altrimenti richiedo la modifica
059100951012     C                   ELSE
059200951012     C                   MOVEL     '02'          OPZ015
059300951012     C                   ENDIF
059400951012     C* se la chiamata prevede la gestione di una registrazione normale
059500951012     C                   OTHER
059600951012     C* passo l'opzione del decisore
059700951012     C                   MOVEL     Opz004        OPZ015
059800951012     C                   ENDSL
059900950719     C* avverto il pgm se deve inizializzarsi o meno
060000950719     C                   MOVEL     $Inzxxx       Inz015
060100950719     C* passo tutti quei campi che possono servire al pgm
060200950719     C                   MOVEL     Ctb004        Ctb015
060300950719     C                   MOVEL     TpRegz004     TpRegz015
060400970130     c
060500950719     C                   MOVEL     CausTes004    CausTes015
060600970130     c* se provengo dai programmi di primanota fattura clienti o fornitori
060700970130     c* propongo l'ultima causale immessa
060800970130     c                   if        Pgm004 = 'NDC005R' or
060900970130     c                             Pgm004 = 'NDC006R'
061000970130     c                   eval      Caustes015 = SavCausale
061100970130     c                   endif
061200950719     C                   MOVEL     CliFor004     CliFor015
061300950719     C                   MOVEL     TabPNot004    TabPNot015
061400950719     C                   MOVEL     Unita004      Unita015
061500951127     C                   IF        TakeOut004 = *on
061600951127     C                   MOVEL     *on           ImmExp015
061700951127     C                   ENDIF
061800980123     C                   MOVE      Pgm004        Pgm004015
061900020212C1530C*
062000020212  "  C* pulisco la chiave di associazione tra ProJ e Laguna
062100020212  "  C* (serve solo in modifica)
062200020212C1530C                   EVAL      KeyLgn = *blank
062300030124D1270C                   EVAL      DesLgn = *blank
062400070424C2015C                   EVAL      VaiRate015=VaiRate004
062500030226C1627 * valorizzo la variabile globale £kpjba utile a ndmvc116
062600030226  "   * per usare knsif nella gestione Laguna
062700030226C1627C                   movel     Kpjba         £Kpjba
062800950719     C*
062900950911     C                   CALLB     'NDC015R'
063000950719     C                   PARM                    KPJBA
063100950807     C                   PARM                    NDC015DS
063200960806     C                   PARM                    ANDFT
063300080916D2369C                   PARM                    NDC019DS
063400040402D1573C                   eval      TabPNot004 =  TabPNot015
063500070503C2015C                   EVAL      VaiRate004=VaiRate015
063600030226C1627 *
063700030226  "   * pulisco la variabile globale £kpjba, per non sporcare
063800030226  "   * eventuali altre chiamate a ndmvc116
063900030226  "  C                   clear                   £Kpjba
064000030226C1627 *
064100950719     C* spengo l'inizializzazione prossimo pgm chiamato
064200950719     C* (verra attivata nella routine Decide___ solo se si passa ad un
064300950719     C*  nuovo pgm della catena)
064400950719     C                   EVAL      $Inzxxx = *off
064500950719     C* ritorno da PGM gestione
064600040309     C                   EXSR      RTN015
064700040309     C*
064800040309     C                   ENDSR
064900040309     C************************************************************
065000040309     C* GESTIONE RITORNO DA PGM DI GESTIONE DOCUMENTI IVA
065100040309     C************************************************************
065200040309     C     RTN015        BEGSR
065300040309     C*
065400040309     C* modo di ritorno
065500040309     C*
065600040309     C                   SELECT
065700040309     C* 1 = F3
065800040309     C     RET015        WHENEQ    '1'
065900040309     C                   EVAL      Retxxx = Ret015
066000040309     C* testo se ho aggiornamenti attivi per la singola registrazione
066100040309     C* in previsione di uscire dal pgm se non ne ho
066200040309     C                   EXSR      TstOpr
066300040309     C* 2 = F12
066400040309     C     RET015        WHENEQ    '2'
066500040309     C* se esiste un pgm chiamato precedentemente
066600040309     C                   IF        X > 1
066700040309     C* memorizzo che torno indietro di 1 pgm nella catena di chiamate
066800040309     C                   EVAL      X = X - 1
066900040309     C* ritorno al pgm chiamato precedentemente
067000040309     C                   MOVE      $Catena(X)    $Gest
067100040309     C* altrimenti
067200040309     C                   ELSE
067300040309     C                   EVAL      Retxxx = Ret015
067400040309     C* testo se ho aggiornamenti attivi per la singola registrazione
067500040309     C* in previsione di uscire dal pgm se non ne ho
067600040309     C                   EXSR      TstOpr
067700040309     C                   ENDIF
067800040309     C*
067900040309     C                   ENDSL
068000040309     c*
068100040309     c* salvo la causale  per riproporla la prossima volta
068200040309     c                   eval      SavCausale = CausTes015
068300040309     C*
068400040309     C* operazione eseguite dal pgm chiamato
068500040309     C*
068600040309     C                   SELECT
068700040309     C* 1 = eseguito aggiornamento
068800040309     C     OPR015        WHENEQ    '1'
068900040309     C* oppure siamo in visualizzazione
069000040309     C     Opz015        OREQ      '05'
069100040309     C* ed è stato scelto di andare avanti nella catena dei pgm
069200040309     C     Pgm015        ANDNE     *blank
069300040309     C     Pgm015        ANDNE     'FINE      '
069400040309     C* memorizzo che è stato eseguito un aggiornamento
069500040309     C                   MOVE      Opr015        WOprSingle
069600040309     C* esegue i test per decidere che pgm chiamare ora
069700040309     C                   EXSR      Decide015
069800040309     C* 0 = non eseguito aggiornamento
069900040309     C     OPR015        WHENEQ    '0'
070000040309     C* ed è stato scelto di uscire dalla catena dei pgm
070100040309     C     Pgm015        ANDEQ     'FINE      '
070200040309     C                   EVAL      Retxxx = Ret015
070300040309     C* testo se ho aggiornamenti attivi per la singola registrazione
070400040309     C* in previsione di uscire dal pgm se non ne ho
070500040309     C                   EXSR      TstOpr
070600040309     C*
070700040309     C                   ENDSL
070800040309     C*
070900040309     C* funzione non eseguibile per errore :
071000040309     C*
071100040309     C                   SELECT
071200040309     C* 1 = funzione richiamata chiusa in qualunque errore
071300950719     C     ERR015        WHENNE    '0'
071400950727     C* rilascio registrazione
071500950719     C                   CALLB     'NDMVC115'
071600950719     C* esco da pgm
071700970124     C                   MOVE      *ON           $FINE
071800950719     C* informo il chiamante che si è generato un errore
071900950719     C* passandogli il valore della Errxxx ricevuta
072000000822     C                   EVAL      Err004 = Err015
072100000822C1410 *
072200000822  "   * se si è verificato un errore sulle autorizzazioni
072300000822  "  C                   If        Err015     = 'B'  Or
072400000822  "  C                             Err015     = 'C'  Or
072500000822  "  C                             Err015     = 'D'  Or
072600000822  "  C                             Err015     = 'E'  Or
072700000822  "  C                             Err015     = 'F'
072800000822  "   *  Err004='2' indica il verificarsi dell'errore in modo generico
072900000822  "  C                   Eval      Err004     = '2'
073000000822  "   *  e il 1° carattere  di Flags004, il tipo di errore specifico
073100000822  "   *  (così NDC001/2R emette l'errore esatto)
073200000822  "  C                   Eval      %Subst(Flags004:1:1)=Err015
073300000822C1410C                   EndIf
073400950719     C*
073500040309     C                   ENDSL
073600040309     C*
073700040309     C                   ENDSR
073800040309     C************************************************************
073900040309     C* Decide chi chiamare dopo la testata doc. IVA
074000040309     C************************************************************
074100040309     C     Decide015     BEGSR
074200040309     C*
074300040309     C                   SELECT
074400040309     C*
074500040309     C* richiesta di aprire il pgm dei movimenti
074600950914     C                   WHEN      Pgm015 = 'RIGHE     '     and
074700950911     C* senza generare le rate
074800950914     C                             GesRate015 = '0'
074900950719     C* se il default del tipo sfl prevede la gestione normale
075000950719     C                   IF        TpSfl015 = '1'  or
075100950719     C* oppure non c'è nessun default
075200950719     C                             TpSfl015 = *blank
075300950719     C* richiamo la gestione std
075400950719     C                   EVAL      $Gest = '020'
075500950719     C* altrimenti
075600950719     C                   ELSE
075700950719     C* richiamo la gestione a sfl aperto
075800960605     C                   EVAL      $Gest = '028'
075900950719     C                   ENDIF
076000950719     C*
076100950911     C* richiesta di aprire il pgm dei movimenti
076200950914     C                   WHEN      Pgm015 = 'RIGHE     '     and
076300950911     C* ma prima generare le rate
076400950914     C                             GesRate015 = '1'
076500950809     C                   EVAL      $Gest = '017'
076600950914     C*
076700950914     C* richiesta di aprire il pgm dei movimenti
076800950914     C                   WHEN      Pgm015 = 'RIGHE     '     and
076900950914     C* ma prima fare un aggiornamento di massa sulle rate
077000950914     C                             ( GesRate015 = '2' or
077100950914     C* oppure valorizzare la schiera delle differenze con tutte le rate
077200950914     C* presenti (serve in copia per richiedere la generazione effetti)
077300950914     C                               GesRate015 = '3' )
077400950914     C                   EVAL      $Gest = '018'
077500950801     C*
077600950801     C* richiesta di ritorno al chiamante dopo conferma
077700950801     C                   WHEN      Pgm015 = 'FINE      '
077800950719     C* esco da pgm
077900970124     C                   MOVE      'FIN'         $GEST
078000950719     C*
078100070424C2015C* richiesta di passare direttamente alla gestione rate
078200070424  "  C                   WHEN      Pgm015 = 'RATE      '
078300070424  "  C* vado a gestione rate
078400070424C2015C                   MOVE      '016'         $GEST
078500070424     C*
078600950719     C                   ENDSL
078700950719     C*
078800950719     C* richiedo l'inizializzazione della videata del pgm da chiamare
078900950719     C                   EVAL      $Inzxxx = *on
079000950719     C*
079100950719     C                   ENDSR
079200950801     C************************************************************
079300950809     C* Chiamata pgm rate manuali
079400950801     C************************************************************
079500950801     C     Gest016       BEGSR
079600950801     C*
079700950801     C* se la videata va inizializzata
079800950801     C                   IF        $Inzxxx = *on
079900950801     C* significa che vado avanti di 1 pgm nella catena di chiamate
080000950801     C                   EVAL      X = X + 1
080100950801     C                   MOVE      $Gest         $Catena(X)
080200950801     C                   ENDIF
080300950801     C*
080400950801     C                   RESET                   NDC016DS
080500950801     C                   MOVEL     Opz004        OPZ016
080600950801     C* passare nei campi appositi della DS NDC016DS il valore
080700950801     C* della chiave della VL univoca
080800951010     C                   MOVEL     RegSys        Sys016
080900951010     C                   MOVEL     RegNrAsReg    NrAsReg016
081000950801     C* avverto il pgm se deve inizializzarsi o meno
081100950801     C                   MOVEL     $Inzxxx       Inz016
081200950801     C* passo tutti quei campi che possono servire al pgm
081300951002     C                   MOVEL     Agente015     Agente016
081400951002     C                   MOVEL     KscFact015    KscFact016
081500951002     C                   MOVEL     KccFact015    KccFact016
081600951002     C                   MOVEL     CliFor015     CliFor016
081700950801     C                   MOVEL     Ksc015        Ksc016
081800950801     C                   MOVEL     SoggD015      SoggD016
081900951010     C                   MOVEL     RegCausTes    CausTes016
082000950801     C                   MOVEL     CausD015      CausD016
082100951010     C                   MOVEL     RegDtReg      DtReg016
082200951010     C                   MOVEL     RegNrReg      NrReg016
082300951010     C                   MOVEL     RegSerReg     SerReg016
082400020304C1547X* DtDoc016 viene usato da NDC016R solo per caricare la data
082500020304C1547X* documento nella videata delle rate manuali della pn
082600020304C1547X**ex C1494         MOVEL     RegDtlib2     DtDoc016
082700020304C1494C*                  MOVEL     RegDtDoc      DtDoc016
082800020304C1547C                   MOVEL     RegDtDoc      DtDoc016
082900951010     C                   MOVEL     RegNrDoc      NrDoc016
083000951010     C                   MOVEL     RegSerDoc     SerDoc016
083100950801     C                   MOVEL     DtPar015      DtPar016
083200950801     C                   MOVEL     NrPar015      NrPar016
083300950801     C                   MOVEL     SerPar015     SerPar016
083400951010     C                   MOVEL     RegDivisa     Divisa016
083500950801     C                   MOVEL     Segno015      Segno016
083600950801     C                   MOVEL     ImportD015    ImportD016
083700950801     C                   MOVEL     Importo015    Importo016
083800950801     C                   MOVEL     ImpostD015    ImpostD016
083900950801     C                   MOVEL     Imposta015    Imposta016
084000951010     C                   MOVEL     RegCambio     Cambio016
084100951010     C                   MOVEL     RegCondPag    CondPag016
084200951010     C                   MOVEL     CondPaD015    CondPaD016
084300951010     C                   MOVE      RegUnita      Unita016
084400950927     C                   MOVE      Fattura015    Fattura016
084500950801     C*
084600950801     C                   CALL      'NDC016R'
084700950801     C                   PARM                    KPJBA
084800950807     C                   PARM                    NDC016DS
084900950908     C                   PARM                    RateDDS
085000950801     C* spengo l'inizializzazione prossimo pgm chiamato
085100950801     C* (verra attivata nella routine Decide___ solo se si passa ad un
085200950801     C*  nuovo pgm della catena)
085300950801     C                   EVAL      $Inzxxx = *off
085400020304C1547X**ex C1494         MOVEL     RegDtDoc      DtDoc016
085500950801     C* ritorno da PGM gestione
085600950801     C                   EXSR      RTN016
085700040309     C*
085800040309     C                   ENDSR
085900040309     C************************************************************
086000040309     C* GESTIONE RITORNO DA PGM DI GESTIONE DOCUMENTI IVA
086100040309     C************************************************************
086200040309     C     RTN016        BEGSR
086300040309     C*
086400040309     C* modo di ritorno
086500040309     C*
086600040309     C                   SELECT
086700040309     C* 1 = F3
086800040309     C     RET016        WHENEQ    '1'
086900040309     C                   EVAL      Retxxx = Ret016
087000040309     C* testo se ho aggiornamenti attivi per la singola registrazione
087100040309     C* in previsione di uscire dal pgm se non ne ho
087200040309     C                   EXSR      TstOpr
087300040309     C* 2 = F12
087400040309     C     RET016        WHENEQ    '2'
087500040309     C* se esiste un pgm chiamato precedentemente
087600040309     C                   IF        X > 1
087700070503C2015C* se non chiesto di andare direttamente alle rate
087800070503c2015c                   if        VaiRate004 <> *on
087900040309     C* memorizzo che torno indietro di 1 pgm nella catena di chiamate
088000040309     C                   EVAL      X = X - 1
088100040309     C* ritorno al pgm chiamato precedentemente
088200040309     C                   MOVE      $Catena(X)    $Gest
088300070503c2015c                   else
088400070503  "  c* se chiesto di andare direttamente alle rate con F12 devo USCIRE
088500070503  "  C* e non tornare indietro, come se fosse F3
088600070503  "  C                   EVAL      Retxxx = Ret016
088700070503  "  C* testo se ho aggiornamenti attivi per la singola registrazione
088800070503  "  C* in previsione di uscire dal pgm se non ne ho
088900070503  "  C                   EXSR      TstOpr
089000070503c2015c                   endif
089100040309     C* altrimenti
089200040309     C                   ELSE
089300040309     C                   EVAL      Retxxx = Ret016
089400040309     C* testo se ho aggiornamenti attivi per la singola registrazione
089500040309     C* in previsione di uscire dal pgm se non ne ho
089600040309     C                   EXSR      TstOpr
089700040309     C                   ENDIF
089800040309     C*
089900040309     C                   ENDSL
090000040309     C*
090100040309     C* operazione eseguite dal pgm chiamato
090200040309     C*
090300040309     C                   SELECT
090400040309     C* 1 = eseguito aggiornamento
090500040309     C     OPR016        WHENEQ    '1'
090600040309     C* oppure siamo in visualizzazione
090700040309     C     Opz016        OREQ      '05'
090800040309     C* ed è stato scelto di andare avanti nella catena dei pgm
090900040309     C     Pgm016        ANDNE     *blank
091000040309     C     Pgm016        ANDNE     'FINE      '
091100040309     C* memorizzo che è stato eseguito un aggiornamento
091200040309     C                   MOVE      Opr016        WOprSingle
091300040309     C* disattivo un eventuale errore nella generazione automatica
091400040309     C                   MOVE      *off          ErrGenRate
091500040309     C* esegue i test per decidere che pgm chiamare ora
091600040309     C                   EXSR      Decide016
091700040309     C* 0 = non eseguito aggiornamento
091800040309     C     OPR016        WHENEQ    '0'
091900040309     C* ed è stato scelto di uscire dalla catena dei pgm
092000040309     C     Pgm016        ANDEQ     'FINE      '
092100040309     C                   EVAL      Retxxx = Ret016
092200040309     C* testo se ho aggiornamenti attivi per la singola registrazione
092300040309     C* in previsione di uscire dal pgm se non ne ho
092400040309     C                   EXSR      TstOpr
092500040309     C*
092600040309     C                   ENDSL
092700040309     C*
092800040309     C* funzione non eseguibile per errore :
092900040309     C*
093000040309     C                   SELECT
093100040309     C* funzione richiamata chiusa in qualunque errore
093200040309     C     ERR016        WHENNE    '0'
093300040309     C* rilascio registrazione
093400040309     C                   CALLB     'NDMVC115'
093500040309     C* esco da pgm
093600040309     C                   MOVE      *ON           $fine
093700040309     C* informo il chiamante che si è generato un errore
093800040309     C* passandogli il valore della Errxxx ricevuta
093900040309     C                   EVAL      Err004 = Err016
094000040309     C*
094100040309     C                   ENDSL
094200040309     C*
094300040309     C                   ENDSR
094400040309     C************************************************************
094500040309     C* Decide chi chiamare dopo le rate IVA
094600040309     C************************************************************
094700040309     C     Decide016     BEGSR
094800040309     C*
094900040309     C                   SELECT
095000950801     C*
095100950907     C* richiesta di aprire il pgm di chiusura registrazione
095200950907     C                   WHEN      Pgm020 = 'CHIUSURA  '
095300070426C2015C                             OR Pgm016 = 'CHIUSURA  '
095400950912     C* se è stato eseguito almeno un aggiornamento
095500950912     C     WOprSingle    IFEQ      '1'
095600950907     C* allora confermo la registrazione
095700950907     C                   EXSR      CallConfer
095800041201D1712c                   if        Esito=*on
095900041202  "  c* se la chiamata ad NDMVC102 va in errore, vado in fondo alla sbr
096000041202  "  c* e rimango quindi nella videata in cui mi trovo(non impostando
096100041202  "  c* $gest
096200041201  "  C                   leavesr
096300041201D1712c                   endif
096400950907     C                   ENDIF
096500950912     C* se non esistono effetti da ri/generare
096600950908     C     RateDDS       IFEQ      *blank
096700950912     C* se siamo in immissione o in copia
096800960729     C                   IF        Opz004 = '01' or Opz004 = '03'   or
096900951006     C* oppure se il documento è IVA
097000951006     C                             DocIVA004 = '1'        and
097100951006     C* e la sottonatura conto è fornitore
097200951006     C                             CliFor015 = 'F'        and
097300951006     C* e questo è un percipiente
097400040309     C                             Percipi015 = '1'       and
097500040309     c* non siamo in visualizzazione
097600040309     C                             OPZ004   <>  '05'
097700040309     C* richiedo il pgm di chiusura registrazione
097800040309     C                   EVAL      $Gest = '039'
097900040309     C* altrimenti
098000040309     C                   ELSE
098100040309     c*
098200040309     C*
098300040309     C* se non è gestita l'autofattura o siamo in visualizzazione
098400040309     C                   IF        AFtAcq015 <> *on  and
098500040309     c                             AftAcq004 <> *on  or opz004 = '05'
098600040309     C* esco da pgm
098700040309     C                   MOVE      'FIN'         $GEST
098800040309     C* se è gestita l'autofattura
098900040309     C                   ELSE
099000040309     C* chiamo il pgm di gestione autofattura
099100040309     C                   MOVE      '036'         $Gest
099200040309     C                   ENDIF
099300040309     C*
099400040309     C                   ENDIF
099500040309     C* se esistono effetti da ri/generare
099600040309     C                   ELSE
099700040309     C* chiamo il pgm di aggiornamento effetti
099800040309     C                   EVAL      $Gest = 'XEX'
099900040309     C                   ENDIF
100000040309     C*
100100040309     C* richiesta di ritorno al chiamante dopo conferma
100200040309     C                   WHEN      Pgm016 = 'FINE      '
100300040309     C* esco da pgm
100400040309     C                   MOVE      'FIN'         $GEST
100500040309     C*
100600040309     C                   ENDSL
100700040309     C*
100800040309     C* richiedo l'inizializzazione della videata del pgm da chiamare
100900040309     C                   EVAL      $Inzxxx = *on
101000040309     C*
101100040309     C                   ENDSR
101200040309     C************************************************************
101300040309     C* Chiamata pgm rate automatiche
101400040309     C************************************************************
101500040309     C     Gest017       BEGSR
101600040309     C*
101700040309     C* questo pgm non gestisce video ed esegue una elaborazione automatica
101800040309     C* per cui non entra nella catena dei pgm (sarà il pgm che lo ha
101900040309     C* richiesto a rifarlo se serve una nuova elaborazione)
102000040309     C*
102100040309     C                   RESET                   NDC016DS
102200040309     C                   MOVEL     Opz004        OPZ016
102300040309     C* passare nei campi appositi della DS NDC016DS il valore
102400950809     C* della chiave della VL univoca
102500951010     C                   MOVEL     RegSys        Sys016
102600951010     C                   MOVEL     RegNrAsReg    NrAsReg016
102700950809     C* passo tutti quei campi che possono servire al pgm
102800951002     C                   MOVEL     KccFact015    KccFact016
102900951002     C                   MOVEL     KscFact015    KscFact016
103000951002     C                   MOVEL     Agente015     Agente016
103100951002     C                   MOVEL     CliFor015     CliFor016
103200950809     C                   MOVEL     Ksc015        Ksc016
103300960429     C                   MOVEL     KccOld015     KccOld016
103400960429     C                   MOVEL     KscOld015     KscOld016
103500950809     C                   MOVEL     SoggD015      SoggD016
103600951010     C                   MOVEL     RegCausTes    CausTes016
103700950809     C                   MOVEL     CausD015      CausD016
103800951010     C                   MOVEL     RegDtReg      DtReg016
103900951010     C                   MOVEL     RegNrReg      NrReg016
104000951010     C                   MOVEL     RegSerReg     SerReg016
104100010621C1494C*                  MOVEL     RegDtDoc      DtDoc016
104200010621C1494C                   MOVEL     RegDtlib2     DtDoc016
104300951010     C                   MOVEL     RegNrDoc      NrDoc016
104400951010     C                   MOVEL     RegSerDoc     SerDoc016
104500950809     C                   MOVEL     DtPar015      DtPar016
104600950809     C                   MOVEL     NrPar015      NrPar016
104700950809     C                   MOVEL     SerPar015     SerPar016
104800951010     C                   MOVEL     RegDivisa     Divisa016
104900950809     C                   MOVEL     Segno015      Segno016
105000950809     C                   MOVEL     ImportD015    ImportD016
105100950809     C                   MOVEL     Importo015    Importo016
105200950809     C                   MOVEL     ImpostD015    ImpostD016
105300950809     C                   MOVEL     Imposta015    Imposta016
105400951010     C                   MOVEL     RegCambio     Cambio016
105500951010     C                   MOVEL     RegCondPag    CondPag016
105600950809     C                   MOVEL     CondPaD015    CondPaD016
105700951010     C                   MOVE      RegUnita      Unita016
105800950809     C*
105900950809     C                   CALL      'NDC017R'
106000950809     C                   PARM                    KPJBA
106100950809     C                   PARM                    NDC016DS
106200950908     C                   PARM                    RateDDS
106300960806     C                   PARM                    ANDFT
106400950908     C*
106500950809     C* spengo l'inizializzazione prossimo pgm chiamato
106600950809     C* (verra attivata nella routine Decide___ solo se si passa ad un
106700950809     C*  nuovo pgm della catena)
106800950809     C                   EVAL      $Inzxxx = *off
106900010621C1494C                   MOVEL     RegDtDoc      DtDoc016
107000950809     C* ritorno da PGM gestione
107100950809     C                   EXSR      RTN017
107200950809     C*
107300950809     C                   ENDSR
107400040309     C************************************************************
107500040309     C* GESTIONE RITORNO DA PGM DI GESTIONE DOCUMENTI IVA
107600040309     C************************************************************
107700040309     C     RTN017        BEGSR
107800040309     C*
107900040309     C* modo di ritorno
108000040309     C* (non avendo video non è possibile che questo flag sia <> da '0'
108100040309     C*
108200040309     C*
108300040309     C* operazione eseguite dal pgm chiamato
108400040309     C*
108500040309     C                   SELECT
108600040309     C* 1 = eseguito aggiornamento
108700040309     C     OPR016        WHENEQ    '1'
108800040309     C* memorizzo che è stato eseguito un aggiornamento
108900040309     C                   MOVE      Opr016        WOprSingle
109000040309     C* esegue i test per decidere che pgm chiamare ora
109100040309     C                   EXSR      Decide017
109200040309     C* 0 = non eseguito aggiornamento
109300040309     C     OPR016        WHENEQ    '0'
109400040309     C* ed è stato scelto di uscire dalla catena dei pgm
109500040309     C     Pgm016        ANDEQ     'FINE      '
109600040309     C                   EVAL      Retxxx = Ret016
109700040309     C* testo se ho aggiornamenti attivi per la singola registrazione
109800040309     C* in previsione di uscire dal pgm se non ne ho
109900040309     C                   EXSR      TstOpr
110000040309     C*
110100040309     C                   ENDSL
110200040309     C*
110300040309     C* funzione non eseguibile per errore :
110400040309     C                   MOVE      *off          ErrGenRate
110500040309     C*
110600040309     C                   SELECT
110700040309     C* 1 = funzione richiamata chiusa in errore (senza aprirsi)
110800040309     C     ERR016        WHENEQ    '1'
110900040309     C* rilascio registrazione
111000040309     C                   CALLB     'NDMVC115'
111100040309     C* esco da pgm
111200040309     C                   MOVE      *on           $fine
111300040309     C* informo il chiamante che si è generato un errore
111400040309     C* passandogli il valore della Errxxx ricevuta
111500040309     C                   EVAL      Err004 = Err016
111600040309     C* 3 = funzione richiamata ha generato rate che presentano errori
111700040309     C     ERR016        WHENEQ    '3'
111800040309     C* tengo memorizzato che comunque passerò per le rate manuali
111900040309     C                   MOVE      *on           ErrGenRate
112000040309     C*
112100040309     C                   ENDSL
112200040309     C*
112300040309     C                   ENDSR
112400040309     C************************************************************
112500040309     C* Decide chi chiamare dopo le rate automatiche
112600040309     C************************************************************
112700040309     C     Decide017     BEGSR
112800040309     C*
112900040309     C                   SELECT
113000040309     C*
113100040309     C* richiesta di aprire il pgm dei movimenti
113200040309     C                   WHEN      Pgm016 = 'RIGHE     '
113300040309     C* se il default del tipo sfl prevede la gestione normale
113400040309     C                   IF        TpSfl015 = '1'  or
113500040309     C* oppure non c'è nessun default
113600040309     C                             TpSfl015 = *blank
113700040309     C* richiamo la gestione std
113800040309     C                   EVAL      $Gest = '020'
113900040309     C* altrimenti
114000040309     C                   ELSE
114100950908     C* richiamo la gestione a sfl aperto
114200960605     C                   EVAL      $Gest = '028'
114300950908     C                   ENDIF
114400950908     C*
114500950908     C* richiesta di ritorno al chiamante dopo conferma
114600950908     C                   WHEN      Pgm016 = 'FINE      '
114700950908     C* esco da pgm
114800970124     C                   MOVE      'FIN'         $GEST
114900950908     C*
115000950908     C                   ENDSL
115100950908     C*
115200950908     C* richiedo l'inizializzazione della videata del pgm da chiamare
115300950908     C                   EVAL      $Inzxxx = *on
115400950908     C*
115500950908     C                   ENDSR
115600950914     C************************************************************
115700950914     C* Chiamata pgm aggiornamento di massa rate
115800950914     C************************************************************
115900950914     C     Gest018       BEGSR
116000950914     C*
116100950914     C* questo pgm non gestisce video ed esegue una elaborazione automatica
116200950914     C* per cui non entra nella catena dei pgm (sarà il pgm che lo ha
116300950914     C* richiesto a rifarlo se serve una nuova elaborazione)
116400950914     C*
116500950914     C                   RESET                   NDC018DS
116600950921     C                   MOVEL     Opz004        OPZ018
116700950914     C                   SELECT
116800950920     C* richiesta modifica rate e aggiornamento schiera effetti
116900950914     C                   WHEN      GesRate015 = '2'
117000950921     C                   MOVEL     '01'          Modo018
117100950920     C* solo aggiornamento schiera effetti con tutte le rate presenti
117200950914     C                   WHEN      GesRate015 = '3'
117300950921     C                   MOVEL     '02'          Modo018
117400950914     C                   ENDSL
117500950914     C* passare nei campi appositi della DS NDC016DS il valore
117600950914     C* della chiave della VL univoca
117700951010     C                   MOVEL     RegSys        Sys018
117800951010     C                   MOVEL     RegNrAsReg    NrAsReg018
117900950914     C* passo tutti quei campi che possono servire al pgm
118000950914     C                   MOVEL     CliFor015     CliFor018
118100950920     C                   MOVEL     DtPar015      DtPar018
118200950920     C                   MOVEL     NrPar015      NrPar018
118300950920     C                   MOVEL     SerPar015     SerPar018
118400960422     C*
118500960422     C                   MOVEL     Segno015      Segno018
118600960422     C                   MOVEL     Agente015     Agente018
118700960422     C                   MOVEL     KscFact015    KscFact018
118800960422     C                   MOVEL     KccFact015    KccFact018
118900960422     C                   MOVEL     RegCondPag    CondPag018
119000950914     C*
119100950914     C                   CALL      'NDC018R'
119200950914     C                   PARM                    KPJBA
119300950914     C                   PARM                    NDC018DS
119400950914     C                   PARM                    RateDDS
119500950914     C*
119600950914     C* spengo l'inizializzazione prossimo pgm chiamato
119700950914     C* (verra attivata nella routine Decide___ solo se si passa ad un
119800950914     C*  nuovo pgm della catena)
119900950914     C                   EVAL      $Inzxxx = *off
120000950914     C* ritorno da PGM gestione
120100950914     C                   EXSR      RTN018
120200950914     C*
120300950914     C                   ENDSR
120400950914     C************************************************************
120500950914     C* GESTIONE RITORNO DA PGM aggiornamento di massa rate
120600950914     C************************************************************
120700040309     C     RTN018        BEGSR
120800040309     C*
120900040309     C* funzione non eseguibile per errore :
121000040309     C*
121100040309     C                   SELECT
121200040309     C* 1 = funzione richiamata chiusa in errore (senza aprirsi)
121300040309     C     ERR018        WHENEQ    '1'
121400040309     C* rilascio registrazione
121500040309     C                   CALLB     'NDMVC115'
121600040309     C* esco da pgm
121700040309     C                   MOVE      *on           $fine
121800040309     C* informo il chiamante che si è generato un errore
121900040309     C* passandogli il valore della Errxxx ricevuta
122000040309     C                   EVAL      Err004 = Err018
122100040309     C*
122200040309     C                   OTHER
122300040309     C* esegue i test per decidere che pgm chiamare ora
122400040309     C                   EXSR      Decide018
122500040309     C*
122600040309     C                   ENDSL
122700040309     C*
122800040309     C                   ENDSR
122900040309     C************************************************************
123000040309     C* Decide chi chiamare dopo l'aggiornamento di massa rate
123100040309     C************************************************************
123200040309     C     Decide018     BEGSR
123300040309     C*
123400040309     C                   SELECT
123500040309     C*
123600040309     C* se la testata voleva aprire il pgm dei movimenti
123700040309     C                   WHEN      Pgm015 = 'RIGHE     '
123800040309     C* se il default del tipo sfl prevede la gestione normale
123900040309     C                   IF        TpSfl015 = '1'  or
124000950914     C* oppure non c'è nessun default
124100950914     C                             TpSfl015 = *blank
124200950914     C* richiamo la gestione std
124300950914     C                   EVAL      $Gest = '020'
124400950914     C* altrimenti
124500950914     C                   ELSE
124600950914     C* richiamo la gestione a sfl aperto
124700960605     C                   EVAL      $Gest = '028'
124800950914     C                   ENDIF
124900950914     C*
125000950914     C                   ENDSL
125100950914     C*
125200950914     C* richiedo l'inizializzazione della videata del pgm da chiamare
125300950914     C                   EVAL      $Inzxxx = *on
125400950914     C*
125500950914     C                   ENDSR
125600950622     C************************************************************
125700950622     C* Chiamata pgm righe generico std
125800950622     C************************************************************
125900950622     C     Gest020       BEGSR
126000950704     C*
126100950704     C* se la videata va inizializzata
126200950704     C                   IF        $Inzxxx = *on
126300950704     C* significa che vado avanti di 1 pgm nella catena di chiamate
126400950704     C                   EVAL      X = X + 1
126500950704     C                   MOVE      $Gest         $Catena(X)
126600950704     C                   ENDIF
126700950622     C*
126800950622     C                   RESET                   NDC020DS
126900950623     C                   MOVEL     Opz004        OPZ020
127000950727     C*
127100950727     C* date non salvabili su database che servono ai successivi pgm
127200950727     C                   MOVE      DtOpe         DtOpe020
127300950727     C                   MOVE      DtVal         DtVal020
127400961128     c*
127500961128     c* Imposto le contropartite utente per le registrazioni IVA
127600950622     C*
127700961128     c                   if        DocIva004 <> '0'
127800961128     c                   eval      KccCpu020 = Kcc015
127900961128     c                   eval      KscCpu020 = Ksc015
128000030819C1679 *
128100030819  "   * Se è una reg generata da una pn collegata (quindi è non iva)
128200030819  "   * la cui tab.001 prevede di reperire la contropartita,
128300030819  "   * reperisco il I° movimento della reg. generante da usare
128400030819  "   * come contropartita utente nei movimenti aggiunti
128500030819  "   * manualmente dall'utente
128600030819  "  C                   else
128700030819  "  C                   exsr      RepCpuPnColl
128800030819C1679 *
128900961128     c                   endif
129000961128     c*
129100950622     C* avverto il pgm se deve inizializzarsi o meno
129200950622     C                   MOVEL     $Inzxxx       Inz020
129800950622     C*
129900950622     C                   MOVEL     NDC020DS      KPJBU
130000960605     C*
130100960605     C                   IF        $Gest = '020'
130200951024     C                   CALL      'NDC020R'
130300950622     C                   PARM                    KPJBA
130400960605     C                   ELSE
130500960605     C                   CALL      'NDC028R'
130600960605     C                   PARM                    KPJBA
130700960605     C                   ENDIF
130800960605     C*
130900950622     C                   MOVEL     KPJBU         NDC020DS
131000950622     C* spengo l'inizializzazione prossimo pgm chiamato
131100950622     C* (verra attivata nella routine Decide___ solo se si passa ad un
131200040309     C*  nuovo pgm della catena)
131300040309     C                   EVAL      $Inzxxx = *off
131400040309     C* ritorno da PGM gestione
131500040309     C                   EXSR      RTN020
131600040309     C*
131700040309     C                   ENDSR
131800040309     C************************************************************
131900040309     C* GESTIONE RITORNO DA PGM DI GESTIONE
132000040309     C************************************************************
132100040309     C     RTN020        BEGSR
132200040309     C*
132300040309     C* modo di ritorno
132400040309     C*
132500040309     C                   SELECT
132600040309     C* 1 = F3
132700040309     C     RET020        WHENEQ    '1'
132800040309     C                   EVAL      Retxxx = Ret020
132900040309     C* testo se ho aggiornamenti attivi per la singola registrazione
133000040309     C* in previsione di uscire dal pgm se non ne ho
133100040309     C                   EXSR      TstOpr
133200040309     C* 2 = F12
133300040309     C     RET020        WHENEQ    '2'
133400040309     C* se esiste un pgm chiamato precedentemente
133500040309     C                   IF        X > 1
133600040309     C* memorizzo che torno indietro di 1 pgm nella catena di chiamate
133700040309     C                   EVAL      X = X - 1
133800040309     C* ritorno al pgm chiamato precedentemente
133900040309     C                   MOVE      $Catena(X)    $Gest
134000040309     C* altrimenti
134100040309     C                   ELSE
134200040309     C                   EVAL      Retxxx = Ret020
134300040309     C* testo se ho aggiornamenti attivi per la singola registrazione
134400040309     C* in previsione di uscire dal pgm se non ne ho
134500040309     C                   EXSR      TstOpr
134600040309     C                   ENDIF
134700040309     C*
134800040309     C                   ENDSL
134900040309     C*
135000040309     C* operazione eseguite dal pgm chiamato
135100040309     C*
135200040309     C                   SELECT
135300040309     C* 1 = eseguito aggiornamento
135400040309     C     OPR020        WHENEQ    '1'
135500040309     C* oppure siamo in visualizzazione
135600040309     C     Opz020        OREQ      '05'
135700040309     C* ed è stato scelto di andare avanti nella catena dei pgm
135800040309     C     Pgm020        ANDNE     *blank
135900040309     C     Pgm020        ANDNE     'FINE      '
136000040309     C* memorizzo che è stato eseguito un aggiornamento
136100040309     C                   MOVE      Opr020        WOprSingle
136200040309     C* esegue i test per decidere che pgm chiamare ora
136300040309     C                   EXSR      Decide020
136400040309     C* 0 = non eseguito aggiornamento
136500040309     C     OPR020        WHENEQ    '0'
136600040309     C* ed è stato scelto di uscire dalla catena dei pgm
136700040309     C     Pgm020        ANDEQ     'FINE      '
136800040309     C                   EVAL      Retxxx = Ret020
136900040309     C* testo se ho aggiornamenti attivi per la singola registrazione
137000040309     C* in previsione di uscire dal pgm se non ne ho
137100040309     C                   EXSR      TstOpr
137200040309     C*
137300040309     C                   ENDSL
137400040309     C*
137500040309     C* funzione non eseguibile per errore :
137600040309     C*
137700040309     C                   SELECT
137800040309     C* 1 = funzione richiamata chiusa in qualunque errore
137900040309     C     ERR020        WHENNE    '0'
138000040309     C* rilascio registrazione
138100040309     C                   CALLB     'NDMVC115'
138200040309     C* esco da pgm
138300040309     C                   MOVE      *on           $fine
138400040309     C* informo il chiamante che si è generato un errore
138500040309     C* passandogli il valore della Errxxx ricevuta
138600040309     C                   EVAL      Err004 = Err020
138700040309     C*
138800040309     C                   ENDSL
138900040309     C*
139000040309     C                   ENDSR
139100040309     C************************************************************
139200040309     C* Decide chi chiamare dopo le righe di PN
139300040309     C************************************************************
139400040309     C     Decide020     BEGSR
139500040309     C*
139600040309     C                   SELECT
139700040309     C*
139800040309     C* richiesta di aprire il pgm di testata
139900040309     C                   WHEN      Pgm020 = 'TESTATA   '
140000040309     C* se stiamo lavorando su doc.non IVA
140100040309     C                   IF        DocIVA004 = '0'
140200040309     C* richiamo la gestione testata doc.non IVA
140300040309     C                   EVAL      $Gest = '010'
140400040309     C* altrimenti
140500040309     C                   ELSE
140600040309     C* richiamo la gestione testata doc.IVA
140700040309     C                   EVAL      $Gest = '015'
140800040309     C                   ENDIF
140900040309     C*
141000040309     C* richiesta di aprire il pgm dei movimenti
141100040309     C                   WHEN      Pgm020 = 'RIGHE     '
141200950622     C* siccome il pgm chiamato è quello delle righe movimento
141300950622     C* se arriva questa richiesta vuol dire che intendo cambiare gestione
141400960605     C* se sono nella gestione a sfl chiuso
141500960605     C                   IF        $Gest = '020'
141600960605     C* a quella a sfl aperto
141700960605     C                   EVAL      $Gest = '028'
141800960605     C* se sono nella gestione a sfl aperto
141900960605     C                   ELSE
142000960605     C* a quella a sfl chiuso
142100960605     C                   EVAL      $Gest = '020'
142200960605     C                   ENDIF
142300950622     C*
142400950622     C* richiesta di aprire il pgm di chiusura registrazione
142500950622     C                   WHEN      Pgm020 = 'CHIUSURA  '
142600950912     C* esistono 3 possibilità di andare a chiusura pgm :
142700950912     C* 1) siamo in fattura e sono richieste rate manuali oppure
142800950912     C*    siamo in fattura e nella ri/generazione rate automatiche si è
142900040309     C*    verificato un errore = devo passare attraverso le rate manuali
143000040309     C* 2) siamo in fattura ma non sono ci sono gli estremi per passare
143100040309     C*    nelle rate manuali, però ci sono degli effetti da ri/generare
143200040309     C*    = prima confermo la registrazione poi chiamo la gestione effetti
143300040309     C* 3) siamo in fattura ma non sono ci sono gli estremi per passare
143400040309     C*    nelle rate manuali e non ci sono degli effetti da ri/generare
143500040309     C*    oppure non siamo in fattura = prima confermo la registrazione poi
143600040309     C*    se sono in immissione o copia chiamo la videata di chiusura
143700040309     C*    altrimenti esco da pgm
143800040309     C                   SELECT
143900040309     C* (1)
144000040309     C* se richieste rate manuali
144100040309     C                   WHEN      (RegPagMan = *on             and
144200040309     C* e la registrazione è un documento IVA
144300040309     C                             DocIVA004 = '1'             or
144400040309     C* oppure se c'è stato un errore nella generazione automatica
144500040309     C                             ErrGenRate = *on            and
144600950908     C* e la registrazione è un documento IVA
144700950921     C                             DocIVA004 = '1')            and
144800950921     C* e contabilità diversa da 'MD' modello primanota
144900950921     C                             Ctb004 <>'MD'
145000950907     C* chiamo il pgm gestione rate manuali
145100950907     C                   EVAL      $Gest = '016'
145200950912     C* (2)
145300950912     C* se esistono degli effetti da ri/generare
145400950912     C     RateDDS       WHENNE    *blank
145500950912     C* se è stato eseguito almeno un aggiornamento
145600950912     C     WOprSingle    IFEQ      '1'
145700950912     C* prima confermo la registrazione
145800950912     C                   EXSR      CallConfer
145900041201D1712c                   if        Esito=*on
146000041202  "  c* se la chiamata ad NDMVC102 va in errore, vado in fondo alla sbr
146100041202  "  c* e rimango quindi nella videata in cui mi trovo(non impostando
146200041202  "  c* $gest
146300041201  "  C                   leavesr
146400041201D1712c                   endif
146500950912     C                   ENDIF
146600950912     C* poi chiamo il pgm di aggiornamento effetti
146700950912     C                   EVAL      $Gest = 'XEX'
146800950912     C* (3)
146900040309     C* altrimenti
147000040309     C                   OTHER
147100040309     C* se è stato eseguito almeno un aggiornamento
147200040309     C     WOprSingle    IFEQ      '1'
147300040309     C* prima confermo la registrazione
147400040309     C                   EXSR      CallConfer
147500041201D1712c                   if        Esito=*on
147600041202  "  c* se la chiamata ad NDMVC102 va in errore, vado in fondo alla sbr
147700041202  "  c* e rimango quindi nella videata in cui mi trovo(non impostando
147800041202  "  c* $gest
147900041201  "  C                   leavesr
148000041201D1712c                   endif
148100040309     C                   ENDIF
148200040309     C* se siamo in immissione o in copia
148300040309     C                   IF        Opz004 = '01' or Opz004 = '03'   or
148400040309     C* oppure se il documento è IVA
148500040309     C                             DocIVA004 = '1'        and
148600040309     C* e la sottonatura conto è fornitore
148700040309     C                             CliFor015 = 'F'        and
148800040309     C* e questo è un percipiente
148900040309     C                             Percipi015 = '1'       and
149000040309     c* non siamo in visualizzazione
149100040309     C                             OPZ004   <>  '05'
149200040309     C* richiedo il pgm di chiusura registrazione
149300040309     C                   EVAL      $Gest = '039'
149400040309     C* altrimenti
149500040309     C                   ELSE
149600040309     C*
149700040309     C* se non è gestita l'autofattura o siamo in visualizzazione
149800040309     C                   IF        AFtAcq015 <> *on   and
149900040309     c                             AftAcq004 <> *on   or opz004 = '05'
150000040309     C* esco da pgm
150100040309     C                   MOVE      'FIN'         $GEST
150200040309     C* se è gestita l'autofattura
150300040309     C                   ELSE
150400040309     C* chiamo il pgm di gestione autofattura
150500040309     C                   MOVE      '036'         $Gest
150600040309     C                   ENDIF
150700040309     C*
150800040309     C                   ENDIF
150900040309     C                   ENDSL
151000950808     C*
151100950912     C* richiesta di ritorno al chiamante
151200950808     C                   WHEN      Pgm020 = 'FINE      '
151300950808     C* esco da pgm
151400970124     C                   MOVE      'FIN'         $GEST
151500950622     C*
151600040309     C                   ENDSL
151700950622     C*
151800950622     C* richiedo l'inizializzazione della videata del pgm da chiamare
151900950622     C                   EVAL      $Inzxxx = *on
152000950622     C*
152100950622     C                   ENDSR
152200030819C1679 ************************************************************
152300030819  "   * Se è una reg generata da una pn collegata
152400030819  "   * la cui tab.001 prevede di reperire la contropartita,
152500030819  "   * reperisco il I° movimento della reg. generante da usare
152600030819  "   * come contropartita utente, nei movimenti aggiunti
152700030819  "   * manualmente dall'utente
152800030819  "   ************************************************************
152900030819  "  C     RepCpuPnColl  Begsr
153000030819  "   *
153100030819  "  C                   eval      KccCpu020 = *blank
153200030819  "  C                   eval      KscCpu020 = *blank
153300030819  "   *
153400030819  "   *
153500030819  "   * Se è una reg generata da una pn collegata
153600030819  "  C                   if        REGNrAsCol <> 0
153700030819  "   *
153800030819  "   *
153900030819  "   *  reperimento I° movimento reg. generante
154000030819  "  C                   eval      LenOut = %size(NDMOV00F)
154100030819  "  C                   callb     'NDDRVMOV'
154200030819  "  C                   parm                    REGSys
154300030819  "  C                   parm                    REGNrAsCol
154400030819  "   *    Con CodOper='1' il driver esegue una Chain su NDMOV00F
154500030819  "   *    con una chiave che comprende anche il N°Riga
154600030819  "   *    quindi è necessario passare come parametro il N°Riga
154700030819  "  C                   parm      1             $NrRigaM
154800030819  "  C                   parm      'NDMOV'       StrutturaO
154900030819  "  C                   parm                    LenOut
155000030819  "  C                   parm      '1'           Codoper
155100030819  "  C                   parm                    NDMOV00F
155200030819  "   *
155300030819  "  C                   if        Codoper <> '9'
155400030819  "   *
155500030819  "   *   memorizzo dati causale reg. in oggetto
155600030819  "  C                   movel     £ANOPETes     SAVANOPETes
155700030819  "   *
155800030819  "   *   reperimento codice pn collegata da causale reg. generante
155900030819  "  C                   eval      LenOut = %Size(AnopePnC)
156000030819  "  C                   move      MOVDtReg      $DataIso
156100030819  "  C                   callb     'NDMVC003'
156200030819  "  C                   parm                    XScSoc
156300030819  "  C                   parm                    MOVCausTes
156400030819  "  C                   parm                    $DataIso
156500030819  "  C                   parm      '1'           Uso               1
156600030819  "  C                   parm      *off          GesMsg
156700030819  "  C                   parm                    Esito
156800030819  "  C                   parm      'ANOPE'       StrutturaO
156900030819  "  C                   parm                    AnopePnC
157000030819  "  C                   parm                    LenOut
157100030819  "  C                   parm      *on           Refresh
157200030819  "   *
157300030819  "  C                   if        Esito = *off
157400030819  "   *
157500030819  "   *    esiste codice pn collegata
157600030819  "  C                   if        Op_Tab <> *blank
157700030819  "   *
157800030819  "   *    reperimento dati codice pn collegata
157900030819  "  C                   eval      XTBRic = '1'
158000030819  "  C                   eval      XTBAzi = XscSoc
158100030819  "  C                   eval      XTBKey = *zero
158200030819  "  C                   eval      XTBCod = '001'
158300030819  "  C                   move      Op_Tab        XTBKey
158400030819  "  C                   callb     'XATB'
158500030819  "  C                   parm                    XATBDS
158600030819  "   *
158700030819  "  C                   if        XTBErr = '0'
158800030819  "  C                   eval      ANG001DS = XTBUni
158900030819  "   *
159000030819  "   *     se tab.001 generazione pn collegata
159100030819  "   *     prevede di reperire la contropartita,
159200030819  "   *     passo la contropartita utente che verrà
159300030819  "   *     usata nei movimenti inseriti manualmente
159400030819  "  C                   if        FLGCCP001 = *on
159500030819  "  C                   eval      KccCpu020 = MOVKcc
159600030819  "  C                   eval      KscCpu020 = MOVKsc
159700030819  "  C                   endif
159800030819  "   *
159900030819  "  C                   endif
160000030819  "  C                   endif
160100030819  "  C                   endif
160200030819  "   *
160300030819  "   *   ripristino dati causale reg. in oggetto
160400030819  "  C                   move      SAVANOPETes   £ANOPETes
160500030819  "   *
160600030819  "  C                   endif
160700030819  "   *
160800030819  "   * resetto le chiavi salvate dall'ultima chain di NDDRVMOV
160900030819  "  C                   callb     'NDDRVMOV'
161000030819  "  C                   parm                    *omit                          Sys
161100030819  "  C                   parm                    *omit                          NrAsReg
161200030819  "  C                   parm                    *omit                          NrRigaM
161300030819  "  C                   parm                    *omit                          StrutturaO
161400030819  "  C                   parm                    *omit                          LenOut
161500030819  "  C                   parm      '0'           Codoper
161600030819  "   *
161700030819  "  C                   endif
161800030819  "   *
161900030819C1679C     XRepCpuPnColl Endsr
162000950907     C************************************************************
162100950907     C* Chiama modulo di conferma registrazione
162200950907     C************************************************************
162300950907     C     CallConfer    BEGSR
162400080916     C*
162500950907     C*
162600951006     C* prima di confermare la registrazione, se è prevista la chiamata
162700951006     C* al pgm di chiusura, recupero dai bottoni quei valori che possono
162800951006     C* servire e che, dopo conferma, perderei
162900951006     C*
163000960806     C                   EVAL      WRepDft = *blank
163100960806     C*
163200951006     C* se sono in immissione o copia
163300951006     C                   IF        Opz004 = '01' or Opz004 = '03'   or
163400951006     C* oppure se il documento è IVA
163500951006     C                             DocIVA004 = '1'        and
163600951006     C* e la sottonatura conto è fornitore
163700951006     C                             CliFor015 = 'F'        and
163800951006     C* e questo è un percipiente
163900951006     C                             Percipi015 = '1'
164000951005     C                   EXSR      RepBotReg
164100960806     C* salvo quei valori importati che devo passare e che potrei perdere
164200960806     C                   EVAL      WRepDft = OPERepDft
164300951005     C                   ENDIF
164400951011     C*
164500951011     C                   EVAL      Operazione = '02'
164600950907     C*
164700950907     C                   CALLB     'NDMVC102'
164800950907     C                   PARM                    Operazione
164900950907     C                   PARM      *on           GesMsg
165000950907     C                   PARM                    Esito
165100950907     C                   PARM      *on           Commit            1
165200951013     C                   PARM                    RegNrReg
165300950907     C*
165400041201D1712C* non è possibile che ci siano errori (non è vero, è successo)
165500041201D1712c                   if        Esito <> *on
165600950907     C*
165700020118C1530C* se Laguna è attivo
165800020118  "  C                   IF        xscLaguna = *on
165900020118  "  C* se la registrazione è di tipo IVA
166000021203C1530C                             and DocIVA004 = '1'
166100021204C1605C* se la registrazione è Definitiva
166200030226C1605C                             and TpRegz004 = 'D'
166300030226C1627X**ex C1605 se la sottonatura è fornitore
166400030226C1627X**ex C1605 ***************** and OPESTp = 'F'
166500021203C1530C* e l'opzione è immissione o copia
166600020206  "  C                             and ( Opz004 = '01' or
166700021115C1530C                                   Opz004 = '03' )
166800021115C1605C* se il parametro modulo LAGUNA c'è
166900021115  "  C                             and NoParmLGN = *off
167000030226C1605C* se è prevista la catalogazione automatica da primanota
167100030226C1627X**ex C1605 ***************** and CatFatC062 = *on
167200030226C1627 *
167300030226  "   * se la sottotipologia della causale è fornitore
167400030226  "   *  e sono autorizzato a catalogare in automatico fatture passive
167500030226  "   *  da ctb (in base al paramentro modulo 'LAGUNA')
167600030226  "  C                             and ((OPESTp='F' and CatFatC062=*on)
167700030226  "   *  o la sottotipologia della causale è cliente
167800030226  "   *  e sono autorizzato a catalogare in automatico fatture attive
167900030226  "   *  da ctb (in base al paramentro modulo 'LAGUNA')
168000030226  "  C                              or (OPESTp='C' and CatFaCC062=*on))
168100030226C1627 *
168200031117C1704C*
168300031117  "  C* controllo se la registrazione è catalogabile
168400031117  "  C                   IF        OPESTp='F'
168500031117  "  C                   EVAL      ClassePJ = '020'
168600031117  "  C                   ELSE
168700031117  "  C                   EVAL      ClassePJ = '030'
168800031117  "  C                   ENDIF
168900031117  "  C*
169000031117  "  C                   EVAL      LungRcd = %size(£NDREG)
169100031117  "  C*
169200031117  "  C                   CALL      'ANA673R'
169300031117  "  C                   PARM                    ClassePJ
169400031117  "  C                   PARM                    £NDREG
169500031117  "  C                   PARM                    LungRcd
169600031117  "  C                   PARM      'NDREG00F'    TipoRcd
169700031117  "  C                   PARM                    Catalogabile
169800031117  "  C*
169900031117  "  C                   IF        Catalogabile = *on
170000031117C1704C*
170100031117C1530x**** se la causale prevede la catalogazione
170200031117  "  x******* funzione non ancora attivata
170300020206  "  C* chiamo il modulo che cataloga (opzioni di immissione o copia) o
170400020206  "  C* decataloga il documento dalla fattura o dalla nota
170500020118  "  C                   CALLB     'NDC516R'
170600020118  "  C                   PARM                    kpjba
170700020206  "  C                   PARM                    Opz004
170800020415  "  C                   PARM                    PartIVA015
170900020415  "  C                   PARM                    SoggD015
171000031117C1530C                   PARM                    Ksc015
171100031117C1704C*
171200031117  "  C                   ENDIF
171300031117C1704C*
171400031117C1530C                   ENDIF
171500020118C1530C*
171600041201     C*
171700041201D1712c                   endif
171800041201     C*
171900950907     C                   ENDSR
172000950726     C************************************************************
172100951005     C* Recupero bottoni di registrazione
172200950726     C************************************************************
172300951005     C     RepBotReg     BEGSR
172400950726     C*
172500951005     C                   CLEAR                   Fun
172600951005     C*
172700951005     C* se il documento è IVA
172800951005     C                   IF        DocIVA004 = '1'        and
172900951005     C* e la sottonatura conto è fornitore
173000951005     C                             CliFor015 = 'F'
173100951005     C* recupero imponibile e imposta del documento
173200951005     C                   EVAL      Fun(5) = '2'
173300951005     C                   ENDIF
173400951005     C*
173500951005     C* se ho fatto almeno una richiesta
173600951005     C                   IF        WFun <> *blank
173700951005     C* richiedo i dati
173800951005     C                   EVAL      LenOut = %Size(ND003DS)
173900951005     C                   CALLB     'NDMVC199'
174000951005     C                   PARM                    Fun
174100951005     C                   PARM                    ND003DS
174200951005     C                   PARM                    LenOut
174300951005     C                   ENDIF
174400951005     C*
174500951005     C                   ENDSR
174600951005     C************************************************************
174700951005     C* Chiamata modulo che chiama pgm gestione effetti
174800951005     C************************************************************
174900951005     C     GestXEX       BEGSR
175000951013     C*
175100951013     C* salvo la registrazione (che potrebbe sporcarsi visto che questo pgm
175200951013     C* crea una o più registrazioni)
175300951013     C                   EVAL      WNDREG = £NDREG
175400950824     C*
175500950824     C                   RESET                   NDC038DS
175600950824     C*
175700951010     C                   MOVEL     RegSys        Sys038
175800951010     C                   MOVEL     RegNrAsReg    NrAsReg038
175900950824     C*
176000950824     C                   MOVEL     CliFor015     CliFor038
176100950824     C                   MOVEL     Kcc015        Kcc038
176200950824     C                   MOVEL     Ksc015        Ksc038
176300950824     C*
176400950824     C                   MOVE      XscSoc        Societa038
176500951010     C                   MOVE      RegCtb        Ctb038
176600951010     C                   MOVE      RegDtReg      DtReg038
176700951010     C                   MOVE      RegDivisa     Divisa038
176800951010     C                   MOVE      RegKnraz      Knraz038
176900951010     C                   MOVE      RegFunGen     FunGen038
177000951010     C                   MOVE      RegUnita      Unita038
177100951010     C                   MOVE      RegClasse     Classe038
177200951010     C                   MOVE      RegDtDoc      DtDoc038
177300951010     C                   MOVE      RegNrDoc      NrDoc038
177400951010     C                   MOVE      RegSerDoc     SerDoc038
177500951010     C                   MOVE      RegDtLGio     DtLGio038
177600951010     C                   MOVE      RegCambio     Cambio038
177700960827     C                   MOVE      *on           Commit038
177800020923D1203C                   z-add     ImportD015    ImpFatD038
177900070413D2165C                   IF        TakeOut004 = *on
178000090727D2165C                   MOVEL     TpAppo004     TpAppo038
178100090727D2536 * Quando bhptpappo non è impostato,in immissione fattura fornit.,
178200090727  "   * uso tpappo standard
178300090727  "  C                   If        TpAppo004     = *blank
178400090727  "  C                             and CliFor015 ='F'
178500090727  "  C                   MOVEL     H5Tpappo      TpAppo038
178600090727D2536C                   ENDIF
178700070413D2165C                   ENDIF
178800090121C2115c* in immissione fattura fornitore, passo tipo appoggio parametro
178900090121  "  c* modulo portafoglio passivo
179000090121  "  C                   IF        TakeOut004 <>*on and
179100090121  "  c                             CliFor015='F'
179200090121  "  C                   MOVEL     H5Tpappo      TpAppo038
179300090121C2115C                   ENDIF
179400950726     C*
179500951206     C                   CALL      'NDC038R'
179600950823     C                   PARM                    Kpjba
179700950824     C                   PARM                    NDC038DS
179800950908     C                   PARM                    RateDDS
179900950803     C*
180000950822     C* se anche gli effetti sono stati aggiornati
180100040309     x****************** IF        Err038 = *off
180200950912     C* se siamo in immissione o in copia
180300951006     C                   IF        Opz004 = '01' or Opz004 = '03'     or
180400951006     C* oppure se il documento è IVA
180500951006     C                             DocIVA004 = '1'        and
180600951006     C* e la sottonatura conto è fornitore
180700951006     C                             CliFor015 = 'F'        and
180800040309     C* e questo è un percipiente
180900040309     C                             Percipi015 = '1'       and
181000040309     c* non siamo in visualizzazione
181100040309     C                             OPZ004   <>  '05'
181200040309     C* richiedo il pgm di chiusura registrazione
181300040309     C                   EVAL      $Gest = '039'
181400040309     C* altrimenti
181500040309     C                   ELSE
181600040309     C*
181700040309     C* se non è gestita l'autofattura o siamo in visualizzazione
181800040309     C                   IF        AFtAcq015 <> *on  and
181900040309     c                             AftAcq004 <> *on  or opz004 = '05'
182000040309     C* esco da pgm
182100040309     C                   MOVE      'FIN'         $GEST
182200040309     C* se è gestita l'autofattura
182300040309     C                   ELSE
182400040309     C* chiamo il pgm di gestione autofattura
182500040309     C                   MOVE      '036'         $Gest
182600040309     C                   ENDIF
182700040309     C*
182800040309     C                   ENDIF
182900040309     x****************** ENDIF
183000040309     C*
183100040309     C* ripristino la registrazione
183200040309     C                   EVAL      £NDREG = WNDREG
183300040309     C*
183400040309     C                   ENDSR
183500040309     C************************************************************
183600040309     C* Chiamata pgm chiusura
183700040309     C************************************************************
183800040309     C     Gest039       BEGSR
183900040309     c*
184000040309     c* memorizzo che sono passata per il riepilogo
184100970120     c                   move      *on           PassaRiep         1
184200950822     C*
184300950822     C* comunque vado avanti di 1 pgm nella catena di chiamate
184400950822     C                   EVAL      X = X + 1
184500950822     C                   MOVE      $Gest         $Catena(X)
184600960112     C*
184700960112     C* se sono in visualizzazione deve reperire i bottoni della
184800960112     C* registrazione perché non è stato fatto prima e adesso servono
184900960112     C                   IF        Opz004 = '05'
185000960112     C                   EXSR      RepBotReg
185100960112     C                   ENDIF
185200080916D2369C* se era stato chiesto di modificare la contropartita utente
185300080916  "  C* ed è presente prima nota collegata, eseguo la modifica
185400080917  "  C* dopo che è stato dato il commit dalla routine CallConfer
185500080917  "  c                   if        ChgCpu019 <> 'N' and
185600080917  "  c                             ChgCpu019 <> *blanks
185700080916  "  c                             and gestitaCol = *on
185800080917  "  c                             and OPZ004 = '02'
185900080916  "  c                   exsr      ChgCpuPNColl
186000080916D2369c                   endif
186100080916     C*
186200951011     C                   RESET                   NDC039DS
186300950822     C* il tipo di opzione è ininfluente
186400950822     C                   MOVEL     Opz004        OPZ039
186500950822     C*
186600950822     C* passaggio parametri input
186700950822     C*
186800950921     C                   Eval      Ctb039     =  Ctb004
186900950921     C                   Eval      Unita039   =  Unita004
187000950921     C                   Eval      SOCIETA039 =  XscSoc
187100950822     C                   Eval      DOCIVA039  =  DocIva004
187200951010     C                   Eval      Sys039     =  RegSys
187300951010     C                   Eval      NrAsReg039 =  RegNrAsReg
187400951011     C
187500951011     C                   Eval      DIVISA039  = RegDivisa
187600951011     C                   Eval      CAUSALE039 = RegCausTes
187700951011     C                   Eval      DTDOC039   = RegDtDoc
187800951011     C                   Eval      NRDOC039   = RegNrDoc
187900951011     C                   Eval      SERDOC039  = RegSerDoc
188000951011     C                   Eval      DTREG039   = RegDtReg
188100951013     C                   Eval      NRREG039   = RegNrReg
188200951011     C                   Eval      SERREG039  = RegSerReg
188300950822     C*
188400951011     C* se documento IVA
188500951011     C                   IF        DocIva004  = '1'
188600950822     C                   Eval      SNATURA039 = CliFor015
188700950822     C                   Eval      SOGGETT039 = Ksc015
188800950822     C                   Eval      KCC039     = Kcc015
188900951023     C                   Eval      AftAcq039  = AftAcq015
189000950822     C                   Eval      DTPAR039   = DtPar015
189100950822     C                   Eval      NRPAR039   = NrPar015
189200950822     C                   Eval      SERPAR039  = SerPar015
189300950822     C                   Eval      IMPORTO039 = Importo015
189400950822     C                   Eval      ImportD039 = ImportD015
189500951010     C                   Eval      CAUSD039   = CausD015
189600950822     C                   Eval      SOGGD039   = SoggD015
189700951011     C                   Eval      FlgIntr039 = Intra015
189800951011     C                   Eval      TipoReg039 = TpReg015
189900951011     C                   Eval      Stato039 = Stato015
190000951011     C                   Eval      PartIVA039 = PartIVA015
190100951011     C*
190200951011     C* se la sottonatura conto è fornitore
190300951011     C                   IF        CliFor015 = 'F'        and
190400951006     C* e questo è un percipiente
190500951006     C                             Percipi015 = '1'
190600000301      * Allineo tutte le ritenute d'acconto di questa fattura
190700000301      * con i dati della fattura stessa;
190800000301      * l'allineamento va fatto sempre, perchè c'è da aggiornare
190900000301      * anche numero/data registrazione
191000961209     c                   reset                   ndcr03ds
191100961209     c                   eval      SocietaR03 = Xscsoc
191200961209     C                   Eval      NasRegfr03 =  RegNrAsReg
191300961209     C                   Eval      SysFr03    =  RegSys
191400961209     C                   Eval      CodR03     = RegCausTes
191500961209     C                   Eval      Ddcr03     = RegDtDoc
191600961209     C                   Eval      NdcR03     = RegNrDoc
191700961209     C                   Eval      SERDOCR03  = RegSerDoc
191800990528     C                   Eval      NrgR03     = RegNrReg
191900990528     C                   Eval      DrgR03     = RegDtReg
192000990528     C                   Eval      SERRegR03  = RegSerReg
192100991006D0229C                   EVAL      NrPR03     = NrPar015
192200991006  "  C                   EVAL      DrPR03     = DtPar015
192300991006D0229C                   EVAL      SerParR03  = SerPar015
192400961209     c                   eval      KPJBU = ndcr03ds
192500961209     c                   call      'NDCR03R'
192600961209     c                   parm                    kpjba
192700951006     C* valorizzo i dati per la chiamata alle ritenute d'acconto
192800951005     C                   EVAL      FlgPerc039  = Percipi015
192900951005     C                   IF        Fattura015 = *on
193000961009     C                   EVAL      DA039      = 'A'
193100951005     C                   ELSE
193200961009     C                   EVAL      DA039      = 'D'
193300951005     C                   ENDIF
193400951005     C                   EVAL      IVA039     = TotIVA003
193500981015     C                   EVAL      IVAD039    = TotIVAD003
193600951011     C* Calcolo del imponibile e spese non asoggettabili
193700951011     C                   Exsr      RepTotPerc
193800951011     C* se non devo gestire il percipiente
193900951006     C                   ELSE
194000951011     C* pulisco i suoi dati
194100951006     C                   CLEAR                   FlgPerc039
194200951006     C                   EVAL      DA039      = *blank
194300951006     C                   EVAL      Imponib039 = 0
194400951006     C                   EVAL      IVA039     = 0
194500951011     C                   EVAL      SNS039     = 0
194600981015     C                   EVAL      ImponD039  = 0
194700981015     C                   EVAL      IVAD039    = 0
194800981015     C                   EVAL      SNSD039    = 0
194900951006     C                   ENDIF
195000951005     C*
195100951005     C                   Else
195200951010     C                   Eval      CAUSALE039 = RegCausTes
195300950822     C                   Eval      CAUSD039   = CausTeD010
195400951010     C                   Eval      DTDOC039   = RegDtDoc
195500951010     C                   Eval      NRDOC039   = RegNrDoc
195600951010     C                   Eval      SERDOC039  = RegSerDoc
195700951010     C                   Eval      DTREG039   = RegDtReg
195800951010     C                   Eval      SERREG039  = RegSerReg
195900950822     C                   Endif
196000960514     C                   Eval      FlgRegDS = REGFlgReg
196100960514     C                   Eval      Cespite039 = FlgReg02
196200981016     C                   Eval      Cambio039  = RegCambio
196300960806     C* se sono in immissione passo anche il codice default
196400960806     C                   IF        Opz004 = '01'
196500960806     C                   EVAL      RepDft039 = WRepDft
196600960806     C                   EVAL      UteUlMo039 = REGUteUlMo
196700960806     C                   ELSE
196800960806     C                   EVAL      RepDft039 = *blank
196900960806     C                   EVAL      UteUlMo039 = *blank
197000960806     C                   ENDIF
197100970122     C* passo la prima nota collegata come da pgm in entrata
197200970123     C                   Eval      PnColl039  = GestitaCol
197300951019     C                   Eval      PGM039     = *blank
197400040401D1573C                   MOVEL     TabPNot004    TabPNot039
197500041118C1801C                   Eval      %Subst(Flags039:2:1)=
197600041118C1801C                             %Subst(Flags004:2:1)
197700110303D2670X*** ex D2612       eval      %subst(DFTInPut:1:%size(ANDFT))=
197800110303D2670X*** ex D2612                 ANDFT
197900950822     C* passo tutti quei campi che possono servire al pgm
198000950822     C                   CALL      'NDC039R'
198100950822     C                   PARM                    KPJBA
198200951011     C                   PARM                    NDC039DS
198300100722D2612X****************** PARM                    ANDFT
198400110303D2670C                   PARM                    ANDFT
198500110303D2670X** ex D2612        PARM                    DftInput
198600020212C1530C                   PARM                    KeyLgn
198700030124D1270C                   PARM                    DesLgn
198800110304D2670C                   PARM                    Opz039Pnc         2
198900950822     C*
199000080325D2366x*** ex D2227       clear                   TabPNot004
199100950912     C* la registrazione in questione è scritta !
199200970128     c                   if        Pncoll039 = *on
199300970128     c* Salvo nrasreg della registrazione generante
199400040428D1596X****************** eval      PncNrasreg = Regnrasreg
199500040428D1596X****************** eval      PncSys     = Regsys
199600040428D1596 *
199700040428  "   * Se dal riepilogo (ndc039r) sono andato a manutenere una reg
199800040428  "   * (<> da quella appena confermata)
199900040428  "   * (ES: _Interrogazioni + _Riconciliazioni + 11=movimenti
200000040428  "   *      + 22=Partite su un movimento non iva )
200100040428  "   * la registr. in 'linea' in questo punto del pgm
200200040428  "   * (identificata con £NDREG variabile globale)
200300040428  "   * è l'ultima aggiornata (quindi da ES: è il movim. non iva);
200400040428  "   * se richiesta p.n.collegata, memorizzo i riferimenti reg.
200500040428  "   * dell'ultima reg. confermata prima di passare al riepilogo
200600040428  "  C                   eval      PncNrasreg = NrAsReg039
200700040428D1596C                   eval      PncSys     = Sys039
200800970128     C                   ENDIF
200900970115     C*
201000970122     C* memorizzo  se è stata richiesta almeno una pn collegata per uscire
201100970122     c* dal ciclo per farla
201200970123     c                   if        ChiestaCol = *off
201300970123     C                   EVAL      ChiestaCol  = Pncoll039
201400970116     C                   EVAL      tabcoll004 =tabcoll039
201500970128     c                   endif
201600950822     C*
201700950822     C* memorizzo che è stata inserita una registrazione
201800040309     C                   EVAL      WOprGlobal = *on
201900040309     C*
202000040309     C* se non è gestita l'autofattura  e non siamo in visualizzazione
202100040309     C                   IF        AFtAcq039 <> *on and
202200040309     c                             AftAcq004 <>  *on or opz004 = '05'
202300040309     C* eseguo le operazioni di fine registrazione
202400040309     C                   EXSR      FineReg
202500040309     C* se è gestita l'autofattura
202600040309     C                   ELSE
202700040309     C* chiamo il pgm di gestione autofattura
202800040309     C                   MOVE      '036'         $Gest
202900040309     C                   ENDIF
203000040309     C*
203100040309     C                   ENDSR
203200080916D2369C************************************************************
203300080916     C* Modifica contropartite pn collegata
203400080916D2369C************************************************************
203500080916     C     ChgCpuPnColl  BEGSR
203600080916     C*
203700080916     C                   CallB     'NDC019R'
203800080916     C                   PARM                    KPJBA
203900080916     C                   PARM                    NDC019DS
204000080916     C*
204100080916D2369C                   ENDSR
204200040309     C************************************************************
204300040309     C* Chiamata pgm chiusura alla fine di ogni opzione se non
204400970120     c* chiamato il riepilogo (viene esclusa la generazione di autofattura
204500970120     c* perchè se la deve fare passa per la normale GEST039)
204600970120     C************************************************************
204700970120     C     Mini039       BEGSR
204800040108     c*
204900040108D1521C* Se stampa autofattura  non eseguita, la eseguo
205000040108  "  C                   IF        WAftSta = *on
205100040108  "  C                   Movel     NDC168DS      KPJBU
205200040108  "  C                   Call      'NDC168R'
205300040108  "  c                   Parm                    KPJBA
205400040108  "  C                   Movel     KPJBU         NDC168DS
205500040108  "  c                   eval      WAftSta = *off
205600040108D1521C                   Endif
205700970120     c*
205800970120     c                   eval      PassaRiep = *on
205900080916     C*
206000970120     C*
206100970120     C* se sono in visualizzazione deve reperire i bottoni della
206200970120     C* registrazione perché non è stato fatto prima e adesso servono
206300970120     C                   IF        Opz004 = '05'
206400970120     C                   EXSR      RepBotReg
206500970120     C                   ENDIF
206600080916     c*
206700080916D2369C* se era stato chiesto di modificare la contropartita utente
206800080916  "  C* ed è presente prima nota collegata, eseguo la modifica
206900080917  "  C* dopo che è stato dato il commit dalla routine CallConfer
207000080917  "  c                   if        ChgCpu019 <> 'N' and
207100080917  "  c                             ChgCpu019 <> *blanks
207200080917  "  c                             and gestitaCol = *on
207300080917  "  c                             and OPZ004 = '02'
207400080917  "  c                   exsr      ChgCpuPNColl
207500080917D2369c                   endif
207600970120     C*
207700970120     C                   RESET                   NDC039DS
207800970120     C* il tipo di opzione è ininfluente
207900970120     C                   MOVEL     Opz004        OPZ039
208000970120     C*
208100970120     C* passaggio parametri input
208200970120     C*
208300970120     C                   Eval      Ctb039     =  Ctb004
208400970120     C                   Eval      Unita039   =  Unita004
208500970120     C                   Eval      SOCIETA039 =  XscSoc
208600970120     C                   Eval      DOCIVA039  =  DocIva004
208700970120     C                   Eval      Sys039     =  RegSys
208800970120     C                   Eval      NrAsReg039 =  RegNrAsReg
208900970120     C                   Eval      DIVISA039  = RegDivisa
209000970120     C                   Eval      CAUSALE039 = RegCausTes
209100970120     C                   Eval      DTDOC039   = RegDtDoc
209200970120     C                   Eval      NRDOC039   = RegNrDoc
209300970120     C                   Eval      SERDOC039  = RegSerDoc
209400970120     C                   Eval      DTREG039   = RegDtReg
209500970120     C                   Eval      NRREG039   = RegNrReg
209600970120     C                   Eval      SERREG039  = RegSerReg
209700970121     C                   eval      snatura039 = clifor004
209800970120     C*
209900970120     C* se documento IVA
210000970120     C                   IF        DocIva004  = '1'
210100970120     C                   Eval      SNATURA039 = CliFor015
210200970120     C                   Eval      SOGGETT039 = Ksc015
210300970120     C                   Eval      KCC039     = Kcc015
210400970120     C                   Eval      AftAcq039  = AftAcq015
210500970120     C                   Eval      DTPAR039   = DtPar015
210600970120     C                   Eval      NRPAR039   = NrPar015
210700970120     C                   Eval      SERPAR039  = SerPar015
210800970120     C                   Eval      IMPORTO039 = Importo015
210900970120     C                   Eval      ImportD039 = ImportD015
211000970120     C                   Eval      CAUSD039   = CausD015
211100970120     C                   Eval      SOGGD039   = SoggD015
211200970120     C                   Eval      FlgIntr039 = Intra015
211300970120     C                   Eval      TipoReg039 = TpReg015
211400970120     C                   Eval      Stato039 = Stato015
211500970120     C                   Eval      PartIVA039 = PartIVA015
211600970120     C*
211700970122     C* la chiamata alle ritenute d'acconto non ci può essere
211800970120     C* pulisco i suoi dati
211900970120     C                   CLEAR                   FlgPerc039
212000970120     C                   EVAL      DA039      = *blank
212100970120     C                   EVAL      Imponib039 = 0
212200970120     C                   EVAL      IVA039     = 0
212300970120     C                   EVAL      SNS039     = 0
212400981015     C                   EVAL      ImponD039  = 0
212500981015     C                   EVAL      IVAD039    = 0
212600981015     C                   EVAL      SNSD039    = 0
212700970120     C*
212800970120     C                   Else
212900970120     C                   Eval      CAUSALE039 = RegCausTes
213000970120     C                   Eval      CAUSD039   = CausTeD010
213100970120     C                   Eval      DTDOC039   = RegDtDoc
213200970120     C                   Eval      NRDOC039   = RegNrDoc
213300970120     C                   Eval      SERDOC039  = RegSerDoc
213400970120     C                   Eval      DTREG039   = RegDtReg
213500970120     C                   Eval      SERREG039  = RegSerReg
213600970120     C                   Endif
213700970120     C                   Eval      FlgRegDS = REGFlgReg
213800970120     C                   Eval      Cespite039 = FlgReg02
213900970120     C* se sono in immissione passo anche il codice default
214000970120     C                   IF        Opz004 = '01'
214100110301D2670X**rireperisco i default di causale perchè potrebbero essersi
214200110301  "  X**sporcati (vedi autofattura) da RegCausTes
214300110303  "  X*** ex D2612       exsr      RepDftCau
214400110303  "  X*** ex D2612       if        esito=*off
214500110303  "  X*** ex D2612       EVAL      RepDft039 = OP_RepDft
214600110303  "  X*** ex D2612       else
214700110303  "  X*** ex D2612       clear                   RepDft039
214800110303D2670X*** ex D2612       endif
214900100722D2612X****************   EVAL      RepDft039 = WRepDft
215000110301D2670C                   EVAL      RepDft039 = WRepDft
215100970120     C                   EVAL      UteUlMo039 = REGUteUlMo
215200970120     C                   ELSE
215300970120     C                   EVAL      RepDft039 = *blank
215400970120     C                   EVAL      UteUlMo039 = *blank
215500970120     C                   ENDIF
215600970120     C*
215700970123     C                   Eval      PnColl039  = GestitaCol
215800970120     C                   Eval      PGM039     = *blank
215900040401D1573C                   MOVEL     TabPNot004    TabPNot039
216000041118C1801C                   Eval      %Subst(Flags039:2:1)=
216100041118C1801C                             %Subst(Flags004:2:1)
216200110301D2670X**Pulisco i default di causale per farli reperire al pgm
216300110301  "  X**riepilogo in quanto potrebbero essersi sporcati
216400110303  "  X**(caso dell'autofattura) ex D2612
216500110303D2670X***  ex D2612      clear                   DftInput
216600970120     C* passo tutti quei campi che possono servire al pgm
216700970120     C                   CALL      'NDC039R'
216800970120     C                   PARM                    KPJBA
216900970120     C                   PARM                    NDC039DS
217000100722D2612x****************** PARM                    ANDFT
217100110303D2670C                   PARM                    ANDFT
217200110303D2670x**** ex D2612      PARM                    DftInput
217300020214C1530C                   PARM                    KeyLgn
217400030124D1270C                   PARM                    DesLgn
217501120224D2722X** Ex D6700        PARM                    Opz039Pnc
217502120224D2670C                   PARM                    Opz039Pnc
217600970120     C*
217700080325D2366x*** ex D2227       clear                   TabPNot004
217800970120     C* la registrazione in questione è scritta !
217900970128     c*
218000040107     C*
218100970128     c                   if        Pncoll039 = *on
218200970128     c* Salvo nrasreg della registrazione generante
218300040428D1596X****************** eval      PncNrasreg = Regnrasreg
218400040428D1596X****************** eval      PncSys     = Regsys
218500040428D1596 *
218600040428  "   * Se dal riepilogo (ndc039r) sono andato a manutenere una reg
218700040428  "   * (<> da quella appena confermata)
218800040428  "   * (ES: _Interrogazioni + _Riconciliazioni + 11=movimenti
218900040428  "   *      + 22=Partite su un movimento non iva )
219000040428  "   * la registr. in 'linea' in questo punto del pgm
219100040428  "   * (identificata con £NDREG variabile globale)
219200040428  "   * è l'ultima aggiornata (quindi da ES: è il movim. non iva);
219300040428  "   * se richiesta p.n.collegata, memorizzo i riferimenti reg.
219400040428  "   * dell'ultima reg. confermata prima di passare al riepilogo
219500040428  "  C                   eval      PncNrasreg = NrAsReg039
219600040428D1596C                   eval      PncSys     = Sys039
219700970128     C                   ENDIF
219800970128     c*
219900970120     C* memorizzo  se è stata richiesta una PN collegata
220000970124     c                   if        ChiestaCol = *off
220100970124     C                   EVAL      ChiestaCol  = Pncoll039
220200970124     C                   EVAL      tabcoll004 =tabcoll039
220300970124     C                   ENDIF
220400970120     C*
220500970120     C* memorizzo che è stata inserita una registrazione
220600970120     C                   EVAL      WOprGlobal = *on
220700970120     C*
220800970120     C                   ENDSR
220900951011     C************************************************************
221000951011     C* Chiamata pgm generazione autofattura
221100951011     C************************************************************
221200951011     C     Gest036       BEGSR
221300970128     c*
221400970128     C* salvo la registrazione (che potrebbe sporcarsi visto che questo pgm
221500970128     C* crea una o più registrazioni)
221600970128     C                   EVAL      WNDREG = £NDREG
221700951011     C*
221800951011     C* questo pgm non gestisce video ed esegue una elaborazione automatica
221900951011     C* per cui non entra nella catena dei pgm (sarà il pgm che lo ha
222000951011     C* richiesto a rifarlo se serve una nuova elaborazione)
222100951011     C*
222200951011     C                   RESET                   NDC036DS
222300970415     c                   if        aftacq015 <> *on and AftAcq004 = *on
222400961202     c                   eval      DltAft036 = *on
222500970415     C                   ELSE
222600970415     c                   eval      DltAft036 = *off
222700970415     c                   endif
222800970123     C*
222900970123     c* pulisco campo della causale della fattura generante l'autofattura
223000970123     c                   move      *off          AftAcq004
223100951011     C*
223200951011     C                   MOVE      Kcc015        Kcc036
223300951011     C                   MOVE      Ksc015        Ksc036
223400951011     C                   MOVE      Segno015      Segno036
223500960514     C                   MOVE      DTPAR015      DtPar036
223600960514     C                   MOVE      NRPAR015      NrPar036
223700960514     C                   MOVE      SERPAR015     SerPar036
223800951013     C                   MOVE      CausAFt015    CausAFt036
223900951127     C                   MOVE      *ON           Commit036
224000951113     C* Salvataggio per stampa autofattura
224100951113     C                   MOVE      AftSta015     WAftSta
224200951113     C                   MOVE      RegSys        SysVCEE168
224300951113     C                   MOVE      RegNrAsReg    RegVCEE168
224400951011     C*
224500951011     C                   MOVEL     NDC036DS      KPJBU
224600951012     C                   CALL      'NDC036R'
224700961202     c                   PARM                    KPJBA
224800951011     C                   MOVEL     KPJBU         NDC036DS
224900951113     C* Salvataggio per stampa autofattura
225000951113     C                   MOVE      RegSys        SysACEE168
225100951113     C                   MOVE      RegNrAsReg    RegACEE168
225200970128     c*
225300970128     c*
225400951011     C* decide chi chiamare dopo
225500951011     C                   EXSR      Decide036
225600951011     C*
225700951011     C                   ENDSR
225800970124     C*
225900951011     C************************************************************
226000951011     C* Decide chi chiamare dopo
226100951011     C************************************************************
226200951011     C     Decide036     BEGSR
226300951011     C*
226400951011     C* se il pgm si è chiuso in errore
226500951011     C     Esito036      IFEQ      *on
226600951012     C* avverto che sto chiamando per un'autofattura in errore
226700951016     C                   MOVE      *on           ErrAutoFat
226800951012     C* avverto che, comunque, c'è stato un aggiornamento
226900951012     C                   EVAL      WOprSingle = *on
227000951012     C* faccio vedere la registrazione errata partendo dalla testata
227100951012     C                   MOVE      '015'         $Gest
227200951011     C* per cui faccio le seguenti operazioni :
227300951011     C*--- pulisco la schiera dei pgm da richiamare
227400951011     C                   CLEAR                   $Catena
227500951011     C                   CLEAR                   X
227600951011     C*--- richiedo l'inizializzazione del 1° pgm che sarà chiamato
227700951011     C                   MOVE      *on           $Inzxxx
227800951011     C*--- inizializzo le variabili di appoggio
227900951011     C                   MOVEL     *blank        RateDDS
228000951011     C                   RESET                   DtOpe
228100951011     C                   RESET                   DtVal
228200951016     C                   MOVE      *off          ErrGenRate
228300970124     c                   move      *off          PassaRiep
228400951011     C*--- pulisco la chiave univoca
228500951011     C                   CLEAR                   Sys004
228600951011     C                   CLEAR                   NrAsReg004
228700110301D2670C                   move      *on           PassaRiep037
228800951011     C*
228900951011     C* se il pgm si è chiuso correttamente
229000951011     C                   ELSE
229100110301D2670C                   move      *off          PassaRiep037
229200951011     C* eseguo i passi previsti per la fine registrazione
229300110303C2162X**riimposto la registrazione dopo l'autfattura per il riepilogo
229400110303C2162X***                EVAL      £NDREG = WNDREG
229500110301D2670C                   EVAL      £NDREG = WNDREG
229600110223     C*
229700951011     C                   EXSR      FineReg
229800951011     C                   ENDIF
229900951011     C*
230000951011     C                   ENDSR
230100970124     C************************************************************
230200970124     C* Gestione fine programma
230300970124     C************************************************************
230400970124     C     GestFine      BEGSR
230500970124     c*
230600970124     c* se non sono passata per il riepilogo
230700970124     c                   If        PassaRiep <>*on   and
230800970124     c* e l'opzione non è visualizzazione o annullamento
230900970124     c                             Opz004 <>'05' and Opz004 <>'04' and
231000970124     c* e non digitato f12 o f3
231100970124     c                             Ret004 <> '1' and ret004 <> '2' and
231200970124     c* e non ci sono errori
231300060202D1938c                             Err004 = *off
231400060202D1938x* e non siamo nei modelli di prima nota
231500060202D1938X*****                        and Regctb <> 'MD'
231600970124     C                   exsr      Mini039
231700970124     c                   eval      PassaRiep = *on
231800970124     c                   endif
231900091201D2565x* se opzione cancellazione, ed esistevano ratei, emetto wdw di
232000091201  "  x* avvertimento
232100091201  "  x* ex c1871         if        opz004='04'
232200091201  "  x*                            and Esiste326=*on
232300091201  "  x* e non digitato f12 o f3
232400091201  "  x*                            and Ret004 <> '1' and ret004 <> '2'
232500091201  "  x* e non ci sono errori
232600091201  "  x*                            and Err004 = *off
232700091201  "  x*                  exsr      WdwAvvRat
232800091201D2565x*                  endif
232900970124     c*
233000110301D2670C                   if         (AFtAcq039 <> *off  or
233100110301  "  C                              AftAcq004 <> *off)
233200110304  "  c* se non già emesso per l'autofattura perchè in caso di errore
233300110304  "  c* viene già emesso
233400110301  "  c                             and PassaRiep037 = *off
233500110301  "  c                   exsr      Call037
233600110301D2670c                   endif
233700110301     c*
233800970124     c* se chiesta prima nota collegata, continuo nel ciclo
233900970124     c                   if        Chiestacol =  *on
234000970124     c                   eval      $gest = '009'
234100970124     C                   ELSE
234200020507     C*
234300030224C1627X* la decatalogazione è affidata al modulo ndmvc116
234400030224C1627X* perciò le specifiche che seguono non sono più valide
234500030224C1627X**ex c1530 *se Laguna è attivo
234600030224  "  X**ex c1530 *       IF        xscLaguna = *on
234700030224  "  X**ex c1530 *se la registrazione è di tipo IVA
234800030224  "  X**ex c1530 *                 and DocIVA004 = '1'
234900030224  "  X**ex c1605 *se la registrazione è Definitiva
235000030224  "  X**ex c1605 *                 and TpRegz004 = 'D'
235100030224  "  X**ex c1605 *se la sottonatura è fornitore
235200030224  "  X**ex c1605 *                 and OPESTp = 'F'
235300030224  "  X**ex d1166 *è stata effettuata una conferma su questa registraz
235400030224  "  X**ex d1166 *                 and WOprSingle = '1'
235500030224  "  X**ex c1530 *e l'opzione è cancellazione
235600030224  "  X**ex c1530 *                 and ( Opz004 = '04')
235700030224  "  X**ex c1605 *se il parametro modulo LAGUNA c'è
235800030224  "  X**ex c1605 *                 and NoParmLGN = *off
235900030224  "  X**ex c1605 *se è prevista la decatalogazione automatica da pn
236000030224  "  X**ex c1605 *                 and DecFatC062 = *on
236100030224  "  X**ex c1530 **** se la causale prevede la catalogazione
236200030224  "  X**ex c1530 ******* funzione non ancora attivata
236300030224  "  X**ex c1530 *chiamo il modulo che cataloga (opz immiss/copia) o
236400030224  "  X**ex c1530 *decataloga il documento dalla fattura o dalla nota
236500030224  "  X**ex c1530 *       CALLB     'NDC516R'
236600030224  "  X**ex c1530 *       PARM                    kpjba
236700030224  "  X**ex c1530 *       PARM                    Opz004
236800030224  "  X**ex c1530 *       PARM                    PartIVA015
236900030224  "  X**ex c1530 *       PARM                    SoggD015
237000030224  "  X**ex c1530 *       PARM                    Ksc015
237100030224  "  X**ex c1530 *       ENDIF
237200030224C1627X**ex c1530 *
237300970127     C* se provengo dai programmi di prima nota clienti e fornitori da menù
237400970127     C                   IF        (Pgm004   = 'NDC005R' or
237500970127     C                              Pgm004   = 'NDC006R') and
237600970124     C* e non è stata chiesta una pn collegata
237700970124     c                             ChiestaCol <> *on               and
237800970128     c* non è stato digitato F3 O F12
237900970128     c                             Retxxx <> '1'  and Retxxx <> '2'
238000020507     C*
238100970124     C* resetto la variabile che mi dice che per la singola registrazione
238200970124     C* c'è stato un aggiornamento
238300970124     C                   EVAL      WOprSingle = *off
238400970124     C* ritorno al primo pgm della catena
238500970124     C                   MOVE      $Catena(1)    $Gest
238600970124     C* ripartendo da capo
238700970124     C*--- pulisco la schiera dei pgm da richiamare
238800970124     C                   CLEAR                   $Catena
238900970124     C                   CLEAR                   X
239000970124     C*--- richiedo l'inizializzazione del 1° pgm che sarà chiamato
239100970124     C                   MOVE      *on           $Inzxxx
239200970124     C*--- inizializzo le variabili di appoggio
239300970124     C                   MOVEL     *blank        RateDDS
239400970124     C                   RESET                   DtOpe
239500970124     C                   RESET                   DtVal
239600970124     C                   MOVE      *off          ErrAutoFat
239700970124     C                   MOVE      *off          ErrGenRate
239800970124     c                   move      *off          PassaRiep
239900970124     c                   move      *off          ChiestaCol
240000970127     c                   move      SavGestita    GestitaCol
240100970127     c* riimposto i parametri della chiamata iniziale
240200970127     c                   select
240300970127     c                   when      pgm004  = 'NDC005R'
240400970127     C                   EVAL      DocIVA004 = '1'
240500970127     C                   EVAL      TpRegz004 = 'D'
240600970127     C                   EVAL      CliFor004 = 'C'
240700970127     C                   EVAL      SovrEse004 = *off
240800970127     c                   EVAL      Takeout004 = *off
240900970127     C* gli dico di gestire p.n.collegata
241000970127     C                   EVAL      Pncoll004  = SavGestita
241100970127     c                   when      pgm004  = 'NDC006R'
241200970127     C                   EVAL      DocIVA004 = '1'
241300970127     C                   EVAL      TpRegz004 = 'D'
241400970127     C                   EVAL      CliFor004 = 'F'
241500970127     C                   EVAL      SovrEse004 = *off
241600970127     c                   EVAL      Takeout004 = *off
241700970127     C                   ENDSL
241800970124     C*--- pulisco la chiave univoca
241900970124     C                   CLEAR                   Sys004
242000970124     C                   CLEAR                   NrAsReg004
242100970127     c                   move      '015'         $gest
242200970124     C*--- predispongo i moduli per iniziare un'immissione
242300970124     C                   EXSR      CallNuova
242400970124     c                   else
242500970124     c* esco da programma
242600970124     c                   move      *ON           $FINE
242700970124     C                   Endif
242800970124     C                   Endif
242900970124     c*
243000970124     C                   endsr
243100110301D2670C************************************************************
243200110301  "  C* Gestione fine programma
243300110301  "  C************************************************************
243400110301D2670C     Call037       BEGSR
243500110301     C*
243600110301     C                   move      *on           PassaRiep037      1
243700110301     C*
243800110301     C                   reset                   ndc037ds
243900110301     C* il tipo di opzione è ininfluente
244000110301     C                   MOVEL     opz004        OPZ037
244100110301     C*
244200110301     C                   eval      nrasreg037 =  RegACEE168
244300110301     C                   eval      Sys037     =  SysACEE168
244400110301     C*
244500110301     C* passo tutti quei campi che possono servire al pgm
244600110301     C                   MOVEL     NDC037DS      KPJBU
244700110301     C                   CALL      'NDC037R'
244800110301     C                   PARM                    KPJBA
244900110301     C                   PARM                    autds
245000110301     C                   MOVEL     KPJBU         NDC037DS
245100110301     C*
245200110301D2670C                   endsr
245300970124     C************************************************************
245400970124     C* Chiamata pgm generazione prima nota collegata
245500970128     c* Questa chiamata NON si poteva fare dal riepilogo perchè
245600970128     c* se per caso viene generata l'autofattura, deve essere prima
245700970128     c* scritta quella e poi la prima nota collegata, e se per caso
245800970128     c* c'è un errore, viene prima emessa l'autofattura errata e poi
245900970128     c* la prima nota collegata
246000970124     C************************************************************
246100970124     C     Gest009       BEGSR
246200970124     c*
246300970124     c* Pulisco tutto
246400970124     C* per cui faccio le seguenti operazioni :
246500970124     C*--- pulisco la schiera dei pgm da richiamare
246600970124     C                   CLEAR                   $Catena
246700970124     C                   CLEAR                   X
246800970124     C*--- richiedo l'inizializzazione del 1° pgm che sarà chiamato
246900970124     C                   MOVE      *on           $Inzxxx
247000970124     C*--- inizializzo le variabili di appoggio
247100970124     C                   MOVEL     *blank        RateDDS
247200970124     C                   RESET                   DtOpe
247300970124     C                   RESET                   DtVal
247400970124     C                   MOVE      *off          ErrGenRate
247500970124     c                   move      *off          PassaRiep
247600970124     c*
247700970128     C* genero prima nota collegata da registrazione generante
247800970124     C                   RESET                   NDC009DS
247900970128     c                   move      PncSys        Sys009
248000970128     c                   move      PncNrasReg    NrasREg009
248100970124     c                   move      Tabcoll004    TabColl009
248200970124     C                   Call      'NDC009R'
248300970124     C                   parm                    KPJBA
248400040309     C                   parm                    NDC009DS
248500040309     C* imposto i parametri
248600040309     c* nr assoluto registrazione collegata
248700040309     c                   eval      Sys004 = sys009
248800040309     c                   eval      NrasReg004 = Nrasreg009
248900040309     c* se l'ho scritta, chiamo in immission con dati da reperire
249000040309     c                   if        opr009     = *on
249100040309     C                   MOVEL     '01'          OPZ004
249200040309     C                   MOVE      *on           takeout004
249300040309     c                   eval      Woprsingle = *on
249400040309     C                   ELSE
249500040309     c                   eval      Woprsingle = *off
249600040309     c* altrimenti va in manutenzione o annullamento
249700040309     c* imposto £ndreg in manutenzione o annullamento
249800040309     C                   CALLB     'NDMVC100'
249900040309     C                   PARM                    Sys004
250000040309     C                   PARM                    NrAsReg004
250100040309     C                   PARM                    *omit
250200040309     C                   PARM                    *omit
250300040309     C                   PARM                    Esito
250400040309     C                   PARM                    *omit
250500040309     C* se  provengo dal decisore stesso, ho l'opzione opz039 impostata
250600040309     c* dopo la chiamata al riepilogo, altrimenti mi arriva già imposta
250700040309     c* ta in opz004
250800040309     c                   if        pgm004 <> 'NDC039R'
250900110303D2670x****               if        opz039 =  '02'
251000110303D2670c                   if        opz039Pnc =  '02'
251100040309     C                   MOVEL     '02'          OPZ004
251200040309     c                   else
251300040309     C                   MOVEL     '04'          OPZ004
251400040309     c                   endif
251500040309     C                   endif
251600040309     c                   move      *off          takeout004
251700040309     C                   ENDIF
251800040309     C* passo i valori che possono generare delle descriminanti per le
251900970124     C* decisioni del "decisore"
252000970124     C                   MOVEL     '0'           DocIVA004
252100970124     C                   MOVEL     *on           SovrEse004
252200970124     c                   move      *off          Pncoll004
252300970124     c                   move      *off          GestitaCol
252400970124     c                   move      *off          ChiestaCol
252500970124     c* pulisco altrimemti fa una autofattura
252600970124     c                   move      *off          AftAcq004
252700970124     c                   move      *off          AftAcq015
252800970124     c* torno a riemettere la testata
252900970124     c                   eval      $Gest = '010'
253000110304     c*
253100110304D2670c* pulisco campo dell'opzione con cui è stata chiesta la
253200110304  "  c* prima nota collegata in caso di esistenza, cioè opzione
253300110304  "  c* modifica o annullamento
253400110304D2670c                   clear                   Opz039Pnc
253500970124     c*
253600970124     C                   ENDSR
253700970124     C*
253800951011     C************************************************************
253900951011     C* Reperimento totali imponibile e spese n.s per Percipiente
254000951011     C************************************************************
254100951011     C     RepTotPerc    BEGSR
254200951011     C* se percipiente
254300951220     c                   eval      Codoper = '1'
254400951011     c                   Exsr      GetIva
254500951011     C                   Clear                   SNS039
254600951011     C                   Clear                   Imponib039
254700981015     C                   Clear                   SNSD039
254800981015     C                   Clear                   ImponD039
254900951011     C     CodOper       DOWNE     '9'
255000951011     C* Calcolo totale imponibile e spese non asoggettate
255100951011     C                   if        IvaAliq = 0
255200951011     C                   Exsr      ChainALR
255300951011     C                   if        ALRProPra = '1'
255400951011     C                   Eval      Wdare  = Ivadare * Ivaimponib
255500951011     C                   Eval      Wavere = Ivaavere * Ivaimponib
255600951011     C                   Eval      SNS039 = SNS039 + Wdare - Wavere
255700981015     C                   Eval      WdareD = Ivadare * IvaimponD
255800981015     C                   Eval      WavereD= Ivaavere * IvaimponD
255900981015     C                   Eval      SNSD039 = SNSD039 + WdareD- WavereD
256000951011
256100951011     C                   Else
256200951011     C                   Eval      wdare = Ivadare * Ivaimponib
256300951011     C                   Eval      wavere = Ivaavere * Ivaimponib
256400951011     C                   Eval      Imponib039= Imponib039 + Wdare - Wavere
256500981015     C                   Eval      wdareD= Ivadare * IvaimponD
256600981015     C                   Eval      wavereD= Ivaavere * IvaimponD
256700981015     C                   Eval      ImponD039 = ImponD039  + WdareD- WavereD
256800951011     C                   EndIf
256900951011
257000951011     C                   else
257100951011     C                   eval      wdare = Ivadare * Ivaimponib
257200951011     C                   eval      wavere = Ivaavere * Ivaimponib
257300951011     C                   eval      Imponib039= imponib039 + Wdare - Wavere
257400981015     C                   eval      wdareD= Ivadare * IvaimponD
257500981015     C                   eval      wavereD= Ivaavere * IvaimponD
257600981015     C                   eval      ImponD039 = imponD039  + WdareD- WavereD
257700951011     C                   endif
257800951011
257900951011     c                   eval      Codoper    = '2'
258000951011     c                   exsr      getiva
258100951011     C                   ENDDO
258200951011     C* fine percipiente
258300951011     C                   EndSr
258400951011     C************************************************************
258500951011     C* Aggancio riga analr01l
258600951011     C************************************************************
258700951011     C     ChainALR      BEGSR
258800951011     C*
258900951011     C* Reperimento riga ANALR
259000951011     C                   Eval      ALRSocieta = IvaSocieta
259100951011     C                   Eval      ALRTpReg   = IvaTpReg
259200951011     C                   Eval      ALRLibro   = IvaLibro
259300951011     C                   Eval      ALRAliq    = IvaAliq
259400951011     C                   Eval      ALRRifIva  = IvaRifIva
259500951011     C                   If        OPENALR = *off
259600040309     C                   Open      ANALR01L
259700040309     c                   Eval      OpenALR = *on
259800040309     C                   Endif
259900040309     C     K05ALR01      Chain     ANALR01l                           21
260000040309     C*
260100040309     C                   ENDSR
260200040309     c*---------------------------------------------------------------
260300040309     c* Reperisce dati di IVA
260400040309     c*---------------------------------------------------------------
260500040309     c     GetIVA        begsr
260600040309
260700040309     c                   eval      LenOut = %size(NDIVA)
260800040309     C                   callb     'NDDRVIVA'
260900040309     C                   PARM                    Regsys
261000040309     C                   PARM                    RegNrasReg
261100040309     C                   PARM                    *omit
261200040309     c                   PARM                    *omit
261300040309     C                   PARM      *OFF          Lock              1
261400040309     C                   PARM      'NDIVA'       StrutturaO
261500040309     C                   PARM                    LenOut
261600040309     C                   PARM                    Codoper
261700040309     C                   PARM                    NDIVA
261800040309     c                   endsr
261900040309     C************************************************************
262000040309     C* Test aggiornamenti attivi per la singola registrazione
262100040309     C* in previsione di uscire dal pgm se non ne ho
262200040309     C************************************************************
262300040309     C     TstOpr        BEGSR
262400040309     C*
262500040309     C* se non ho aggiornamenti attivi per la singola registrazione
262600040309     C                   IF        WOprSingle = *off
262700040309     C* rilascio registrazione
262800040309     C                   CALLB     'NDMVC115'
262900950622     C* esco da pgm
263000970124     C                   MOVE      'FIN'         $GEST
263100950622     C* informo il chiamante che è stato premuto un tasto di funzione
263200950622     C* passandogli il valore della Retxxx ricevuta
263300950622     C                   MOVE      Retxxx        RET004
263400950622     C* altrimenti
263500950622     C                   ELSE
263600950622     C* chiedo conferma dell'annullamento aggiornamento
263700950622     C                   EXSR      MSGPGM
263800950622     C*
263900950622     C                   SELECT
264000950622     C* Annulla richiesta
264100950622     C     MsgRtn        WHENEQ    'N'
264200950622     C     MsgRtn        OREQ      'n'
264300950622     C* ritorno al pgm che ha dato il comando
264400950704     C                   MOVE      $Gest         $Catena(X)
264500950622     C* Conferma uscita
264600950622     C     MsgRtn        WHENEQ    'S'
264700950622     C     MsgRtn        OREQ      's'
264800950727     C* rilascio registrazione
264900950703     C                   CALLB     'NDMVC115'
265000950622     C* esco da pgm
265100970124     C                   MOVE      'FIN'         $GEST
265200950622     C* informo il chiamante che è stato premuto un tasto di funzione
265300950622     C* passandogli il valore della Retxxx ricevuta
265400950622     C                   MOVE      Retxxx        RET004
265500950622     C*
265600950622     C                   ENDSL
265700950622     C*
265800950622     C                   ENDIF
265900950622     C*
266000950622     C                   ENDSR
266100950616     C************************************************************
266200951011     C* Operazioni da eseguire a fine registrazione
266300950616     C************************************************************
266400951011     C     FineReg       BEGSR
266500951113     C* Se stampa autofattura  allora chiamo pgm aft
266600951113     C                   IF        WAftSta = *on
266700951113     C                   Movel     NDC168DS      KPJBU
266800951113     C                   Call      'NDC168R'
266900951113     c                   Parm                    KPJBA
267000951113     C                   Movel     KPJBU         NDC168DS
267100970303     c                   eval      WAftSta = *off
267200951113     C                   Endif
267300970120     c*
267400970120     c* se non sono passata per il riepilogo lo emetto sempre
267500970120     c* per tutte le opzioni fatta esclusione per l'annullamento e visuali
267600970120     c* zzazione
267700110303C2162X***                If        PassaRiep <>*on   and
267800110303D2670C                   If        PassaRiep <>*on   and
267900110303  "  X********* ex C2162 If        (PassaRiep <>*on    or
268000110303  "  X********* ex C2162            AFtAcq039 = *on   or
268100110303D2670X********* ex C2162            AftAcq004 = *on)  and
268200970124     c                             Opz004    <>'05' and opz004 <> '04' AND
268300970124     c* e non digitato f12 o f3
268400970124     c                             Ret004 <> '1' and ret004 <> '2' and
268500970124     c* e non ci sono errori
268600060202D1938c                             Err004 = *off
268700060202  "  X* e non siamo nei modelli di prima nota
268800060202D1938x***                          and Regctb <> 'MD'
268900110303D2670X*** ex C2162
269000110301  "  X* Se è prevista la registrazione di autofatturazione di acquisto
269100110301  "  X* la gestione Intracomunitaria parte da lì
269200110303  "  X*** ex C2162       IF        OpeIntra = *On and
269300110303  "  X*** ex C2162                 (Opz004 = '01' or Opz004 = '03')
269400110303  "  X*** ex C2162       EVAL      Intra015 = *On
269500110303  "  X*** ex C2162       ENDIF
269600110303D2670X*** ex C2162
269700970120     C                   exsr      Mini039
269800970120     c                   eval      PassaRiep = *on
269900970120     c                   endif
270000110303D2670X*** ex C2162
270100110303  "  X*** ex C2162 reimposto la registrazione dopo l'autofattura
270200110303  "  X*** ex D2612       IF        $Gest = '036'
270300110303  "  X*** ex C2162       EVAL      £NDREG = WNDREG
270400110303D2670X*** ex D2612       EndIf
270500951113     C*
270600970127     C* se provengo dai programmi di prima nota clienti e fornitori da menù
270700970127     C                   IF        (Pgm004   = 'NDC005R' or
270800970127     C                              Pgm004   = 'NDC006R') and
270900970127     C* e non è stata chiesta una pn collegata
271000970127     c                             ChiestaCol <> *on               and
271100970127     c* non è stato digitato F3
271200970127     c                             Retxxx <> '1'
271300110304     c*
271400110304D2670C* se cè ancora da emettere il riepilogo dell'autofattura lo emett
271500110304  "  C                   if         (AFtAcq039 <> *off  or
271600110304  "  C                              AftAcq004 <> *off)
271700110304  "  c* se non già emesso per l'autofattura perchè in caso di errore
271800110304  "  c* viene già emesso
271900110304  "  c                             and PassaRiep037 = *off
272000110304  "  c                   exsr      Call037
272100110304D2670c                   endif
272200970124     C* provengo dalla prima nota clienti o dalla prima nota fornitori
272300951011     C* resetto la variabile che mi dice che per la singola registrazione
272400951011     C* c'è stato un aggiornamento
272500951011     C                   EVAL      WOprSingle = *off
272600951011     C* ritorno al primo pgm della catena
272700951011     C                   MOVE      $Catena(1)    $Gest
272800951011     C* ripartendo da capo
272900951011     C*--- pulisco la schiera dei pgm da richiamare
273000951011     C                   CLEAR                   $Catena
273100951011     C                   CLEAR                   X
273200951011     C*--- richiedo l'inizializzazione del 1° pgm che sarà chiamato
273300951011     C                   MOVE      *on           $Inzxxx
273400951011     C*--- inizializzo le variabili di appoggio
273500951011     C                   MOVEL     *blank        RateDDS
273600951011     C                   RESET                   DtOpe
273700951011     C                   RESET                   DtVal
273800951016     C                   MOVE      *off          ErrAutoFat
273900951016     C                   MOVE      *off          ErrGenRate
274000970127     c* memorizzo che sono passata per il riepilogo
274100970127     c                   move      *off          PassaRiep
274200970127     c                   move      SavGestita    GestitaCol
274300970127     c* riimposto i parametri della chiamata iniziale
274400970127     c                   select
274500970127     c                   when      pgm004  = 'NDC005R'
274600970127     C                   EVAL      DocIVA004 = '1'
274700970127     C                   EVAL      TpRegz004 = 'D'
274800970127     C                   EVAL      CliFor004 = 'C'
274900970127     C                   EVAL      SovrEse004 = *off
275000970127     c                   EVAL      Takeout004 = *off
275100970127     C* gli dico di gestire p.n.collegata
275200970127     C                   EVAL      Pncoll004  = SavGestita
275300970127     c                   when      pgm004  = 'NDC006R'
275400970127     C                   EVAL      DocIVA004 = '1'
275500970127     C                   EVAL      TpRegz004 = 'D'
275600970127     C                   EVAL      CliFor004 = 'F'
275700970127     C                   EVAL      SovrEse004 = *off
275800970127     c                   EVAL      Takeout004 = *off
275900970127     C                   ENDSL
276000951011     C*--- pulisco la chiave univoca
276100951011     C                   CLEAR                   Sys004
276200951011     C                   CLEAR                   NrAsReg004
276300970127     c* riimposto la testata documenti iva
276400970127     c                   move      '015'         $gest
276500951019     C*--- predispongo i moduli per iniziare un'immissione
276600951019     C                   EXSR      CallNuova
276700951011     C* altrimenti
276800951011     C                   ELSE
276900951011     C* esco da pgm
277000970124     C                   MOVE      'FIN'         $GEST
277100951011     C                   ENDIF
277200951011     C*
277300951011     C                   ENDSR
277400951011     C************************************************************
277500951011     C* PGM PER GESTIONE MESSAGGI ERRORE
277600951011     C************************************************************
277700951011     C     MSGPGM        BEGSR
277800951011     C*
277900950615     C                   CLEAR                   XMSGDS
278000950615     C                   MOVE      'N'           MSGLCK
278100950615     C                   Z-ADD     1             MSGMSG
278200950710     C                   MOVEL     'SN'          MSGTPR
278300950615     C                   MOVEL     'S'           MSGRSP
278400950615     C                   MOVEL     'S'           MSGEMV
278500950615     C                   Z-ADD     10            MSGRGP
278600950615     C                   MOVEL     'N'           MSGCNL
278700950615     C                   MOVEL     'N'           MSGVID
278800950615     C                   MOVEL     'N'           MSGOPE
278900950615     C                   MOVEL     'N'           MSGSTP
279000950615     C                   CALL      'XMSGR'
279100950615     C                   PARM                    XMSGDS
279200950615     C*
279300950615     C                   ENDSR
279400950615     C************************************************************
279500950615     C* OPERAZIONI INIZIALI
279600950615     C************************************************************
279700950615     C     *INZSR        BEGSR
279800950615     C*
279900940127     C* Reperimento parametri
280000940127     C*
280100940117     C     *ENTRY        PLIST
280200940117     C                   PARM                    KPJBA
280300950410     C                   PARM                    DSAUT
280400951010     C*
280500951010     C* Esegue override share(*YES)
280600951010     C*
280700951010     C                   call      'NDC000C'
280800940127     C*
280900940117     C                   ENDSR
281000940223     C************************************************************
281100940223     C* INIZIALIZZAZIONE VARIABILI
281200940223     C************************************************************
281300940223     C     INZVAR        BEGSR
281400941215     C*
281500941215     C* Dati da PGM chiamante
281600941215     C*
281700950616     C                   MOVEL     KPJBU         C004DS
281800951128     C*
281900951128     C* Reperimento dati società
282000951128     C*
282100951128     C                   MOVEL     'SOC001'      TIPXSC
282200951128     C                   MOVEL     *BLANK        SOCXSC
282300951128     C                   EXSR      REPSOC
282400941201     C*
282500951010     C* Controlli dati società
282600941201     C*
282700951010     C     RTNXSC        IFNE      '1'
282800951010     C                   MOVEL     XSOCDS        SOC001
282900951010     C* se la società è fittizia è inutile l'elaborazione
283000040309b0278x***  XscScF        IFEQ      'S'
283100001010b0278C     XscScF        IFEQ      *on
283200951010     C                   MOVEL     '1'           Err004
283300951010     C                   EXSR      ENDPGM
283400951010     C                   ENDIF
283500951010     C                   ELSE
283600970124     C                   MOVE      *ON           $FINE
283700951010     C                   EXSR      ENDPGM
283800951010     C                   ENDIF
283900941201     C*
284000941124     C* Reperimento autorizzazioni
284100071211c2068c* se contabilità <> da CG e per documenti NON IVA reperisco
284200071211c2068c* di nuovo le autorizzazioni, perchè potrebbero essere differenti
284300941124     C*
284400951129     C                   IF        Parms = 1
284500951201     c                             or %addr(dsaut) = *null
284600071211C2068c                             or
284700071211c2068c                             (ctb004 <> 'CG' and dociva004='0')
284800950526     C                   EXSR      REPAUT
284900950622     C                   ELSE
285000950804     C                   MOVEL     DSAUT         AUTDS
285100950804     C                   MOVEL     XCADDS        CONPNODS
285200950620     C                   ENDIF
285300950615     C*
285400950615     C* Controllo autorizzazioni
285500950615     C*
285600950615     C                   EXSR      CtrlAut
285700021115C1605C*
285800021115  "  C* Reperimento parametri di gestione
285900030122C1605C*
286000030122D1270C* se Laguna è attivo
286100030122D1270C                   IF        xscLaguna = *on
286200030122C1605C                   MOVE      *off          NoParmLGN
286300021115  "  C                   MOVEL     *BLANK        XPARIC
286400021115  "  C                   MOVEL     XSCSOC        XPASOC
286500021115  "  C                   MOVEL     'LAGUNA'      XPAPAR
286600021115  "  C                   MOVEL     *BLANK        XPAOUT
286700021115  "  C                   EXSR      REPPAR
286800040205C1605C     XPAERR        IFEQ      '0'
286900040205C1722X*ex C1605********* MOVEL     XPAOUT        ANP062DS
287000040205C1722C                   MOVEL     XPAOUT        £ANP062DS
287100040205C1605C                   ELSE
287200021115  "  C                   MOVE      *on           NoParmLGN
287300021115C1605C                   ENDIF
287400030122D1270C                   ENDIF
287500950330     C*
287600090121C2115C*
287700090121  "  C* Reperimento parametri portafoglio passivo
287800090121  "  C*
287900090121  "  C                   MOVEL     *BLANK        XPARIC
288000090121  "  C                   MOVEL     XSCSOC        XPASOC
288100090121  "  C                   MOVEL     'NDCHH5  '    XPAPAR
288200090121  "  C                   MOVEL     *BLANK        XPAOUT
288300090121  "  C                   EXSR      REPPAR
288400090121  "  C     XPAERR        IFEQ      '0'
288500090121  "  C                   MOVEL     XPAOUT        NDCHH5DS
288600090121C2115C                   ENDIF
288700940223     C* Variabili per gestione videate
288800940223     C*
288900950615     C                   MOVE      *OFF          $Fine
289000950616     C                   CLEAR                   $Catena
289100950704     C                   CLEAR                   X
289200950622     C* appena entro nel decisore sicuramente il primo pgm chiamato sarà
289300950622     C* da inizializzare
289400950622     C                   MOVE      *on           $Inzxxx
289500081002D2369c* pulire qui ds ndc019ds da passare e ricevere a NDC015R
289600081002D2369c                   clear                   ndc019ds
289700950330     C*
289800950330     C* Variabili appoggio
289900950330     C*
290000950619     C                   MOVEL     *OFF          WOprSingle
290100950619     C                   MOVEL     *OFF          WOprGlobal
290200950913     C                   MOVEL     *blank        RateDDS
290300941104     C*
290400950727     C                   RESET                   DtOpe
290500950727     C                   RESET                   DtVal
290600961202     C                   MOVE      *off          AftAcq004
290700961202     C                   MOVE      *off          ErrAutoFat
290800951016     C                   MOVE      *off          ErrGenRate
290900970123     C                   MOVE      *off          WAftSta
291000970123     C                   MOVE      *off          ChiestaCol        1
291100970124     C                   MOVE      *off          PassaRiep
291200970120     c* Salvo se gestire prima nota collegata
291300970123     c                   move      PnColl004     GestitaCol        1
291400970127     c                   move      GestitaCol    SavGestita        1
291500950727     C*
291600951010     C* se non è stato effettuato un caricamento esterno
291700951010     C                   IF        TakeOut004 = *off
291800951010     C*
291900951010     C* dall'opzione definisco quali sono le operazioni da fare subito
292000951010     C                   SELECT
292100951010     C*
292200951010     C* Immissione
292300951010     C     Opz004        WHENEQ    '01'
292400970127     c* se non provengo però dal riepilogo, perchè la callprendi va fatta
292500970127     c* con il numero assoluto della prima nota collegata
292600970128     c                   if        pgm004 <> 'NDC039R'
292700970127     C                   EXSR      CallNuova
292800970127     c                   endif
292900951010     C*
293000951010     C* Modifica
293100951010     C     Opz004        WHENEQ    '02'
293200951010     C* Annullamento
293300951010     C     Opz004        OREQ      '04'
293400951010     C* Visualizzazione
293500951010     C     Opz004        OREQ      '05'
293600970127     c* se non provengo però dal riepilogo, perchè la callprendi va fatta
293700970127     c* con il numero assoluto della prima nota collegata
293800970128     c                   if        pgm004 <> 'NDC039R'
293900951010     C                   EXSR      CallPrendi
294000970127     c                   endif
294100951010     C*
294200951010     C* Copia
294300951010     C     Opz004        WHENEQ    '03'
294400960729     C     Opz004        OREQ      '33'
294500960418     C                   EXSR      CallCopia
294600951010     C*
294700990809     C* Passaggio provvisorio/definitivo oppure provvisorio/gestionale
294800951013     C     Opz004        WHENEQ    '22'
294900990809C1218C     Opz004        OREQ      '23'
295000951013     C                   EXSR      CallP_D
295100951013     C* eseguito il passaggio, richiamo la registrazione in modifica
295200040309D0154x********           MOVE      '02'          Opz004
295300951010     C*
295400951010     C                   ENDSL
295500951010     C*
295600951025     C* Se è stato effettuato un caricamento esterno imposto WoprSingle =*on
295700951025     C                   ELSE
295800951025     C                   eval      WoprSingle = *on
295900951025     C*
296000951010     C                   ENDIF
296100951010     C*
296200951010     C* dai dati registrazione definisco quale pgm chiamare per primo
296300951010     C*  testata registrazioni non IVA
296400951011     C     DocIVA004     IFEQ      '0'
296500951010     C                   MOVE      '010'         $GEST
296600970121     C                   reset                   ndc015ds
296700951010     C*  testata registrazioni IVA
296800951010     C                   ELSE
296900970121     C                   reset                   ndc010ds
297000951010     C                   MOVE      '015'         $GEST
297100951010     C                   ENDIF
297200970124     c*
297300970127     C* Se provengo dall'opzione 21 della lista prima nota, devo andare
297400970127     c* subito nella routine della prima nota collegata
297500970128     c                   if        pgm004  = 'NDC039R'
297600970127     C                   MOVE      '009'         $gest
297700970128     c* imposto nrasreg
297800970128     c                   eval      PncNrasreg = nrasreg004
297900970128     c                   eval      PncSys     = sys004
298000970124     c                   endif
298100051010     c*
298200051010C1871C* In caso di annullamento, verifico presenza di ratei collegati
298300051010  "  c* per emettere window di avvertimento alla fine
298400051010  "  c                   eval      Esiste326=*off
298500051010  "  C                   if        Opz004='04'
298600091119C1871c                   exsr      CtrlRatei
298700091119D2565c                   if        Esiste326=*on
298800091119  "  c                   exsr      WdwAvvRatX
298900091119  "  C                   if        Msgrtn <>'S'
299000091119  "  C                   EXSR      endpgm
299100091119D2565c                   endif
299200091119c1871c                   endif
299300051010C1871c                   endif
299400940223     C*
299500070424     c*
299600940223     C                   ENDSR
299700051010c1871 ************************************************************
299800051010      * test esistenza ratei collegati
299900051010C1871 ************************************************************
300000051010     C     CtrlRatei     Begsr
300100051010     c*
300200051010     c                   reset                   ndc326ds
300300051010     c                   eval      Opz326='01'
300400051010     c                   eval      Societa326=xscsoc
300500051010     c                   eval      Ctb326 = Ctb004
300600051010     c                   eval      Sys326=sys004
300700051010     c                   eval      Nrasreg326=Nrasreg004
300800051010     c                   eval      kpjbu=ndc326ds
300900051010     c                   call      'NDC326R'
301000051010     C                   PARM                    kpjba
301100051010     c                   eval      ndc326ds=kpjbu
301200051010     c*
301300051010C1871c                   endsr
301400091201D2565X************************************************************
301500091201  "  X* wdw di avvertimento esistenza ratei   EX c1871
301600091201  "  X************************************************************
301700091201  "  X*    WdwAvvRat     Begsr
301800091201  "  X*
301900091201  "  X*                  clear                   xmsgds
302000091201  "  X*                  MOVE      'N'           MSGLCK
302100091201  "  X*                  Z-ADD     0             MSGMSG
302200091201  "  X*                  MOVEL     'S'           MSGEMV
302300091201  "  X*                  Z-ADD     10            MSGRGP
302400091201  "  X*                  MOVEL     'N'           MSGCNL
302500091201  "  X*                  MOVEL     'N'           MSGVID
302600091201  "  X*                  MOVEL     'N'           MSGOPE
302700091201  "  X*                  MOVEL     'N'           MSGSTP
302800091201  "  X*                  eval      XmsgCd1 = 'NDC0902'
302900091201  "  X*                  eval      XmsgCd2 = *blanks
303000091201  "  X*                  eval      varMsg1 = rateo326
303100091201  "  X*                  eval      xmsginz = *on
303200091201  "  X*
303300091201  "  X*                  CALL      'XMSGR'
303400091201  "  X*                  PARM                    XMSGDS
303500091201  "  X*                  PARM                    XMStxt          135
303600091201  "  X*                  PARM                    XMSGinz           1
303700091201  "  X*                  PARM                    XMSGCd1           7
303800091201  "  X*                  PARM                    XMSGCd2           7
303900091201  "  X*                  PARM                    XMSGCd3           7
304000091201  "  X*                  PARM                    XMSGSn1           7
304100091201  "  X*                  PARM                    XMSGCdTxT        63
304200091201  "  X*                  PARM                    MSGCdTit         14
304300091201  "  X*                  PARM                    MSGClPrtf         1
304400091201  "  X*                  PARM                    VarMsg1         100
304500091201  "  X*                  PARM                    VarMsg2         100
304600091201  "  X*                  PARM                    VarMsg3         100
304700091201  "  X*
304800091201D2565X*                  endsr
304900091119D2565 ************************************************************
305000091119      * wdw di avvertimento esistenza ratei chiede se proseguire
305100091119D2565 ************************************************************
305200091119     C     WdwAvvRatX    Begsr
305300091119     c*
305400091119     c                   clear                   xmsgds
305500091119     C                   MOVE      'N'           MSGLCK
305600091119     C                   Z-ADD     0             MSGMSG
305700091119     C                   MOVEL     'S'           MSGEMV
305800091119     C                   Z-ADD     10            MSGRGP
305900091119     C                   MOVEL     'N'           MSGCNL
306000091119     C                   MOVEL     'N'           MSGVID
306100091119     C                   MOVEL     'N'           MSGOPE
306200091119     C                   MOVEL     'N'           MSGSTP
306300091119     C                   MOVEL     'S'           MSGRSP
306400091119     C                   MOVEL     'SN'          MSGTPR
306500091119     c                   eval      XmsgCd1 = 'NDC1134'
306600091119     c                   eval      XmsgCd2 = 'COS1325'
306700091119     c                   eval      varMsg1 = rateo326
306800091119     c                   eval      xmsginz = *on
306900091119     C*
307000091119     C                   CALL      'XMSGR'
307100091119     C                   PARM                    XMSGDS
307200091119     C                   PARM                    XMStxt          135
307300091119     C                   PARM                    XMSGinz           1
307400091119     C                   PARM                    XMSGCd1           7
307500091119     C                   PARM                    XMSGCd2           7
307600091119     C                   PARM                    XMSGCd3           7
307700091119     C                   PARM                    XMSGSn1           7
307800091119     C                   PARM                    XMSGCdTxT        63
307900091119     C                   PARM                    MSGCdTit         14
308000091119     C                   PARM                    MSGClPrtf         1
308100091119     C                   PARM                    VarMsg1         100
308200091119     C                   PARM                    VarMsg2         100
308300091119     C                   PARM                    VarMsg3         100
308400091119     c*
308500091124D2565c                   endsr
308600941209     C************************************************************
308700941209     C* REPERIMENTO DATI SOCIETA'
308800941209     C************************************************************
308900941209     C     REPSOC        BEGSR
309000941209     C*
309100950522     C                   CALLB     'XSOC'
309200941209     C                   PARM                    TIPXSC            6
309300941209     C                   PARM                    SOCXSC            3
309400941209     C                   PARM                    CDSXSC            9 0
309500941209     C                   PARM                    MODXSC            3
309600941209     C                   PARM      *BLANKS       RTNXSC            1
309700941209     C                   PARM                    XSOCDS
309800941209     C                   PARM                    KPJBA
309900941209     C*
310000941209     C                   ENDSR
310100021115C1605C************************************************************
310200021115  "  C* REPERIMENTO PARAMETRI GESTIONE
310300021115  "  C************************************************************
310400021115C1605C     REPPAR        BEGSR
310500021115     C*
310600021115     C                   CALLB     'XPAR'
310700021115     C                   PARM                    XPASOC            3
310800021115     C                   PARM                    XPAPAR            8
310900021115     C                   PARM                    XPAOUT
311000021115     C                   PARM                    XPAERR            1
311100021115     C                   PARM                    XPARIC            1
311200021115     C*
311300021115C1605C                   ENDSR
311400941124     C************************************************************
311500941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
311600941124     C************************************************************
311700941124     C     REPAUT        BEGSR
311800941124     C*
311900941124     C                   CLEAR                   AUTDS
312000941124     C*
312100941124     C                   MOVEL     XSCSOC        XCASOC
312200950905     C                   MOVEL     *blank        XCAUNI
312300941124     C                   Z-ADD     0             XCAGRP
312400941124     C                   MOVEL     KNMUS         XCAPRF
312500950525     C                   MOVEL     'CONPNO'      XCAFNC
312600950525     C                   MOVEL     *ZERO         XCAVFU
312700071213c2068c* devo passare la contabilità solo se 'CG' per tutti i movimenti
312800071213  "  c* per tutte le altre contabilità solo per i movimenti NON IVA
312900071213  "  c                   if        Ctb004   ='CG' or
313000071213  "  c                             (CTB004 <> 'CG'and DocIva004='0')
313100071213  "  c                   move      Ctb004        XcaVfu
313200071213  "  c                   endif
313300071211  "  x*****************  MOVE      'CK'          XCATCK
313400071211c2068c                   MOVE      'CC'          XCATCK
313500950525     C                   MOVE      '1'           XCANAU
313600941124     C*
313700950522     C                   CALLB     'XCHKAUT'
313800941124     C                   PARM                    AUTDS
313900941124     C*
314000941124     C* errori/non abilitazione
314100941124     C     XCARTN        IFNE      0
314200941124     C                   EXSR      ENDPGM
314300941124     C                   ELSE
314400950525     C                   MOVEL     XCADDS        CONPNODS
314500941124     C                   ENDIF
314600950615     C*
314700950615     C                   ENDSR
314800950615     C************************************************************
314900950615     C* Controllo autorizzazione
315000950615     C************************************************************
315100950615     C     CtrlAut       BEGSR
315200950616     C*
315300960112     C* se sono in immissione o copia, siccome non ho autorizzazioni per
315400960112     C* livello/utente/gruppo/lista di gruppi lo controllo direttamente;
315500960112     C* nel caso di passaggio da provv. a def. devo ctrl sia l'autorizzaz.
315600960112     C* a immettere che quella a modificare
315700950616     C*
315800950727     C                   IF        Opz004 = '01' or
315900960112     C                             Opz004 = '03' or
316000960729     C                             Opz004 = '33' or
316100040309C1218x******************           Opz004 = '22'
316200040309C1218C                             Opz004 = '22' or
316300990809C1218C                             Opz004 = '23'
316400950616     C*
316500950616     C                   EVAL      XARAbil = *on
316600950616     C*
316700950616     C* testo le autorizzazioni per il tipo documento (IVA - non IVA)
316800950616     C                   SELECT
316900950616     C*
317000950616     C* documenti non IVA
317100950616     C                   WHEN      DocIVA004 = '0'     and
317200950616     C                             ImmNon = *off
317300950616     C*
317400950616     C                   EVAL      XARAbil = *off
317500950616     C*
317600950616     C* documenti IVA
317700950616     C                   WHEN      DocIVA004 = '1'      and
317800950616     C                             ImmIVA = *off
317900950616     C*
318000950616     C                   EVAL      XARAbil = *off
318100950616     C*
318200950616     C                   ENDSL
318300950616     C*
318400031118C1701 *
318500031118  "   * Se richiesto passaggio a definitivo
318600031118  "   * e si è autorizzati al tipo documento (IVA - non IVA),
318700031118  "   * e
318800031118  "   * se tipo reg.=provvisoria e si è autorizzati a passare in
318900031118  "   * definitivo solo le reg. gestionali
319000031118  "   * oppure
319100031118  "   * se tipo reg.=gestionale e si è autorizzati a passare in
319200031118  "   * definitivo solo le reg. provvisorie
319300031118  "   * memorizzo la mancata autorizzazione all'opzione
319400031118  "  C                   if        Opz004 = '22'
319500031118  "  C                             and
319600031118  "  C                             XARAbil = *on
319700031118  "  C                             and
319800031209  "  C                             ((TpRegZ004='P' and PassaggD='2')
319900031118  "  C                             or
320000031209  "  C                             (TpRegZ004='G' and PassaggD='1'))
320100031118  "  C                   eval      XARAbil = *off
320200031118  "  C                   endif
320300031118C1701 *
320400960112     C                   ENDIF
320500960112     C*
320600960112     C* se non sono in immissione né in copia né in passaggio provv./def.
320700960112     C                   IF        Opz004 <> '01' and
320800960112     C                             Opz004 <> '03' and
320900960729     C                             Opz004 <> '33' and
321000040309C1218x******************           Opz004 <> '22' or
321100040309C1218C                             Opz004 <> '22' and
321200990809  "  C*  né in passaggio provv./gestionale
321300990809C1218C                             Opz004 <> '23' or
321400960112     C* oppure sono in passaggio provv./def. e sono abilitato all'immissione
321500960112     C                             Opz004 =  '22' and
321600960112     C                             XARAbil = *on
321700990809C1218C* oppure sono in passaggio provv./ges. e sono abilitato all'immissione
321800990809  "  C                             or Opz004 =  '23' and
321900990809C1218C                             XARAbil = *on
322000950615     C*
322100950619     C* richiamo XAutReg a seconda del documento e dell'opzione
322200960112     C* passando i campi di autorizzazione relativi alla opzione richiesta
322300950619     C                   EXSR      CallXAR
322400950619     C*
322500950619     C* se non sono abilitato
322600950619     C                   IF        XARAbil = *off   and
322700950619     C* e l'opzione è la modifica
322800970124     C                             Opz004 = '02'
322900950619     C* riprovo a chiamare XAutReg cambiando l'opzione in visualizzazione
323000950619     C                   EVAL      Opz004 = '05'
323100950619     C                   EXSR      CallXAR
323200941124     C*
323300950616     C                   ENDIF
323400950619     C*
323500950619     C                   ENDIF
323600950616     C*
323700950616     C* se non sono abilitato
323800950616     C                   IF        XARAbil = *off
323900950616     C* avverto il chiamante dell'errore
324000000822     C                   MOVE      '2'           Err004
324100000821C1410 * e 'comunico' al chiamante il tipo di autorizzazione mancante
324200000822C1410C                   Eval      %Subst(Flags004:1:1)  = 'A'
324300950616     C* esco da pgm
324400950616     C                   EXSR      EndPgm
324500950616     C                   ENDIF
324600950619     C*
324700950619     C                   ENDSR
324800950619     C************************************************************
324900950619     C* Chiamata a XAUTREG
325000950619     C************************************************************
325100950619     C     CallXAR       BEGSR
325200950619     C*
325300950619     C* valorizzo le autorizzazioni per il tipo documento (IVA - non IVA)
325400950619     C                   SELECT
325500950619     C*
325600950619     C* documenti non IVA
325700950619     C                   WHEN      DocIVA004 = '0'
325800950619     C                   MOVE      LvRNon        XARAutLivR
325900950619     C* reperisco le autorizzazioni per l'opzione richiesta
326000950619     C                   SELECT
326100950619     C*  opzione = modifica
326200951013     C                   WHEN      Opz004 = '02'            or
326300951013     C*            oppure passaggio provvisorio definitivo
326400970127     C                             Opz004 = '22'
326500990809C1218C                             or Opz004 = '23'
326600950619     C                   EVAL      XARAutTot = ManNonT
326700950619     C                   EVAL      XARAutGru = ManNonG
326800950619     C                   EVAL      XARAutPrf = ManNonP
326900950619     C                   EVAL      XARAutLis = ManNonL
327000950619     C*  opzione = cancella
327100950619     C                   WHEN      Opz004 = '04'
327200950619     C                   EVAL      XARAutTot = AnnNonT
327300950619     C                   EVAL      XARAutGru = AnnNonG
327400950619     C                   EVAL      XARAutPrf = AnnNonP
327500950619     C                   EVAL      XARAutLis = AnnNonL
327600950619     C*  opzione = visualizza
327700950619     C                   WHEN      Opz004 = '05'
327800950619     C                   EVAL      XARAutTot = VisNonT
327900950619     C                   EVAL      XARAutGru = VisNonG
328000950619     C                   EVAL      XARAutPrf = VisNonP
328100950619     C                   EVAL      XARAutLis = VisNonL
328200950619     C                   ENDSL
328300950619     C*
328400950619     C* documenti IVA
328500950619     C                   WHEN      DocIVA004 = '1'
328600950619     C                   MOVE      LvRIVA        XARAutLivR
328700950619     C* reperisco le autorizzazioni per l'opzione richiesta
328800950619     C                   SELECT
328900950619     C*  opzione = modifica
329000960112     C                   WHEN      Opz004 = '02'     or
329100960112     C*            oppure passaggio provvisorio definitivo
329200970127     C                             Opz004 = '22'
329300990809C1218C                             or Opz004 = '23'
329400950619     C                   EVAL      XARAutTot = ManIVAT
329500950619     C                   EVAL      XARAutGru = ManIVAG
329600950619     C                   EVAL      XARAutPrf = ManIVAP
329700950619     C                   EVAL      XARAutLis = ManIVAL
329800950619     C*  opzione = cancella
329900950619     C                   WHEN      Opz004 = '04'
330000950619     C                   EVAL      XARAutTot = AnnIVAT
330100950619     C                   EVAL      XARAutGru = AnnIVAG
330200950619     C                   EVAL      XARAutPrf = AnnIVAP
330300950619     C                   EVAL      XARAutLis = AnnIVAL
330400950619     C*  opzione = visualizza
330500950619     C                   WHEN      Opz004 = '05'
330600950619     C                   EVAL      XARAutTot = VisIVAT
330700950619     C                   EVAL      XARAutGru = VisIVAG
330800950619     C                   EVAL      XARAutPrf = VisIVAP
330900950619     C                   EVAL      XARAutLis = VisNonL
331000950619     C                   ENDSL
331100950619     C*
331200950619     C                   ENDSL
331300950619     C*
331400950619     C                   EVAL      XARSocieta = XscSoc
331500950619     C                   EVAL      XARAmbito = '1'
331600950619     C                   EVAL      XARUtenteR = XscCUt
331700950619     C                   MOVE      XscGrp        XARGruppoR
331800950619     C                   EVAL      XARUtenteP = UteUlMo004
331900950619     C                   EVAL      XARLivREnt = LivRise004
332000950619     C                   CALL      'XAUTREG'
332100950619     C                   PARM                    XAutRegDS
332200950616     C*
332300941124     C                   ENDSR
332400951010     C************************************************************
332500951010     C* Chiamata modulo che inizializza una nuova registrazione
332600951010     C************************************************************
332700951010     C     CallNuova     BEGSR
332800951010     C*
332900951010     C                   CALLB     'NDMVC101'
333000951010     C                   PARM                    Esito
333100951010     C                   PARM                    Sys004
333200951010     C                   PARM                    NrAsReg004
333300951010     C                   PARM                    *omit
333400951010     C*
333500980119     C*
333600980119     C* se la funzione è andata in errore
333700980119     C                   IF        Esito = *on
333800980119     C* avverto l'utente
333900980119     C*
334000980119     C* reperisco i msg da passare a XMSGR
334100980119     C     1             DO        2             x
334200980119     C                   MOVEL     MsgId(x)      IDMSG
334300980119     C                   CALLB     'XRTVM'
334400980119     C                   PARM                    IDMSG            27
334500980119     C                   PARM                    TXTMSG          100
334600980119     C                   PARM                    ERRMSG            1
334700980119     C     ERRMSG        IFEQ      *ON
334800980119     C                   MOVEL     *ALL' '       TxtMsg
334900980119     C                   ENDIF
335000980119     C                   IF        x = 1
335100980119     C                   MOVEL     TXTMSG        MsgMS1
335200980119     C                   ELSE
335300980119     C                   MOVEL     TxtMsg        MsgMS2
335400980119     C                   ENDIF
335500980119     C                   ENDDO
335600980119     C*
335700980119     C                   MOVEL     *blank        MSGMS3
335800980119     C                   Z-ADD     0             MSGMSG
335900980119     C                   MOVEL     'N'           MSGRSP
336000980119     C                   MOVEL     *blank        MSGTPR
336100980119     C                   MOVEL     'S'           MSGEMV
336200980119     C                   Z-ADD     10            MSGRGP
336300980119     C                   MOVEL     'N'           MSGLCK
336400980119     C                   MOVEA     STATUS        STS
336500980119     C                   MOVEL     'N'           MSGCNL
336600980119     C                   MOVEL     'N'           MSGVID
336700980119     C                   MOVEL     'N'           MSGOPE
336800980119     C                   MOVEL     'N'           MSGSTP
336900980119     C                   CALL      'XMSGR'
337000980119     C                   PARM                    XMSGDS
337100980119     C                   PARM                    XMSGTXT         135
337200980119     C* ed esco dal programma
337300980119     C                   EVAL      $Fine  = *On
337400980119     C                   EVAL      Err004 = '1'
337500980119     C                   EXSR      EndPgm
337600980119     C                   ENDIF
337700980119     C*
337800980119     C*
337900951010     C                   ENDSR
338000951010     C************************************************************
338100951010     C* Chiamata al modulo di reperimento registrazione + allocaz.
338200951010     C************************************************************
338300951010     C     CallPrendi    BEGSR
338400951010     C*
338500951010     C                   CALLB     'NDMVC100'
338600951010     C                   PARM                    Sys004
338700951010     C                   PARM                    NrAsReg004
338800951010     C                   PARM                    *omit
338900951010     C                   PARM                    *omit
339000951010     C                   PARM                    Esito
339100951010     C                   PARM                    *omit
339200951010     C*
339300951010     C* se il rcd da gestire non viene trovato
339400951010     C* ritorno al pgm guida avvertendolo dell'errore
339500951010     C                   IF        Esito = *on
339600951010     C                   MOVE      *ON           $FINE
339700951010     C                   MOVE      '1'           ERR004
339800951010     C                   EXSR      EndPgm
339900951010     C                   ENDIF
340000951010     C*
340100951010     C* se la registrazione è collegata ad un'altra
340200951010     C                   IF        RegNrAsCol > 0      and
340300970124     C* e non siamo in visualizzazione
340400970124     C                             Opz004 <> '05'      and
340500970124     C* e non è una prima nota collegata
340600970124     C                             RegFunGen <> 'A'
340700951010     C* non posso manutenzionarla quindi
340800951010     C* ritorno al pgm guida avvertendolo dell'errore
340900951010     C                   MOVE      *ON           $FINE
341000951010     C                   MOVE      '3'           ERR004
341100970116     C                   EXSR      EndPgm
341200951010     C                   ENDIF
341300951010     C*
341400961202     c*  salvo se la registrazione in modifica aveva autofattura
341500961202     c* in modo da cancellarla se non l'avrà più
341600961202     c*
341700961202     c                   eval      aftacq004 = opeaftacq
341800961202     c*
341900951010     C                   ENDSR
342000951010     C************************************************************
342100951010     C* Chiamata al modulo di copia registrazione
342200951010     C************************************************************
342300951010     C     CallCopia     BEGSR
342400960729     C*
342500960729     C                   IF        Opz004 = '33'
342600960729     C                   MOVEL     '2'           ModoCopia
342700960729     C                   MOVEL     '03'          Opz004
342800960729     C                   ELSE
342900960729     C                   MOVEL     '0'           ModoCopia
343000960729     C                   ENDIF
343100960729     C*
343200960729     C                   Z-ADD     0             NewSys
343300960729     C                   Z-ADD     0             NewNrAsReg
343400951010     C*
343500951010     C                   CALLB     'NDMVC117'
343600951010     C                   PARM                    Sys004
343700951010     C                   PARM                    NrAsReg004
343800960729     C                   PARM                    NewSys
343900960729     C                   PARM                    NewNrAsReg
344000951010     C                   PARM                    *omit
344100951010     C                   PARM                    *omit
344200951010     C                   PARM                    Esito
344300951010     C                   PARM                    *omit
344400960729     C                   PARM                    ModoCopia
344500951010     C*
344600951010     C* se il rcd da copiare non viene trovato
344700951010     C* ritorno al pgm guida avvertendolo dell'errore
344800951010     C                   IF        Esito = *on
344900951010     C                   MOVE      *ON           $FINE
345000951010     C                   MOVE      '1'           ERR004
345100951010     C                   EXSR      EndPgm
345200951010     C                   ENDIF
345300951010     C*
345400951010     C                   ENDSR
345500951013     C************************************************************
345600951013     C* Chiamata al modulo di passaggio provvisorio a definitivo
345700951013     C************************************************************
345800951013     C     CallP_D       BEGSR
345900951013     C*
346000990809C1218C* a seconda del tipo di opzione decido il tipo registrazione
346100990809  "  C* a cui passare
346200990809  "  C                   SELECT
346300990809  "  C                   WHEN      Opz004 = '22'
346400990809  "  C                   MOVE      'D'           TpRegz004
346500990809  "  C                   WHEN      Opz004 = '23'
346600990809  "  C                   MOVE      'G'           TpRegz004
346700990809  "  C                   ENDSL
346800990809C1218C*
346900951013     C                   CALLB     'NDMVC119'
347000951013     C                   PARM                    Sys004
347100951013     C                   PARM                    NrAsReg004
347200951013     C                   PARM      '01'          Operazione
347300951013     C                   PARM      '0'           Gesmsg
347400951013     C                   PARM                    Esito
347500040309C1218x****************** PARM      'D'           TpRegz004
347600990809C1218C                   PARM                    TpRegz004
347700951013     C                   PARM                    *omit
347800951013     C                   PARM                    *omit
347900951013     C                   PARM                    *omit
348000951013     C                   PARM                    *omit
348100960312     C*
348200960312     C* se il rcd da gestire non viene trovato
348300960312     C* ritorno al pgm guida avvertendolo dell'errore
348400960312     C                   IF        Esito = *on
348500960312     C                   MOVE      *ON           $FINE
348600960312     C                   MOVE      '1'           ERR004
348700960312     C                   EXSR      EndPgm
348800960312     C                   ENDIF
348900951013     C*
349000951013     C                   ENDSR
349100940128     C************************************************************
349200950615     C* Definizione Klist
349300940128     C************************************************************
349400950615     C     DefKlist      BEGSR
349500940128     C*
349600940223     C* klist
349700940223     C*
349800951011     C* Klist ANALR01l
349900951011     C     K05ALR01      Klist
350000951011     C                   Kfld                    ALRSocieta
350100951011     C                   Kfld                    ALRTpReg
350200951011     C                   Kfld                    ALRLibro
350300951011     C                   Kfld                    ALRAliq
350400951011     C                   Kfld                    ALRRifIva
350500940127     C*
350600940117     C                   ENDSR
350700110303D2670x************************************************************
350800110303  "  x* Reperimento default causale  EX D2612
350900110303  "  x************************************************************
351000110303  "  x*    RepDftCau     BEGSR
351100110303  "  x*
351200110303  "  x*  reperisco dati causale
351300110303  "  x*                  eval      LenOut = %Size(AnopePnC)
351400110303  "  x*                  callb     'NDMVC003'
351500110303  "  x*                  parm                    XScSoc
351600110303  "  x*                  parm                    RegCausTes
351700110303  "  x*                  parm      *loval        $DataIso
351800110303  "  x*                  parm      '1'           Uso               1
351900110303  "  x*                  parm      *off          GesMsg
352000110303  "  x*                  parm                    Esito
352100110303  "  x*                  parm      'ANOPE'       StrutturaO
352200110303  "  x*                  parm                    AnopePnC
352300110303  "  x*                  parm                    LenOut
352400110303  "  x*                  parm      *on           Refresh
352500110303  "  x*
352600110303D2670x*                  ENDSR
352700980109**
352800980109PROMSG    *LIBL     COS1030
352900980109PROMSG    *LIBL     COS1031
