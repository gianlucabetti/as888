000100111024     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200111024     H*PARMS CVTOPT(*NONE)
000300111024     H DECEDIT('0,') DATEDIT(*DMY.)
000400111024      *
000500130925      * Estrazione Spesometro 2013
000600130925      * ---------------------------------------
000700111024      *
000800111024      * Il programma verifica le registrazione contabili di tipo IVA
000900130924      * del periodo ricevuto e le scrive sul file NDSda00F.
001000111024      *
001100111024      * ---------------------------------------------------------------
001200111024      *
001300111024     F*------------
001400111024     F* Spesometro
001500131021     FNDSda00F  O    E           K DISK    USROPN
001600131021     F****************                     Commit
001700111111     F*------------
001800111024     F* Spesometro
001900130930     F* key: SOCIETA/SYS/NRASREG/ANNO
002000131021     FNDSda02L  UF   E           K DISK    RENAME(NDSda000:NDSda02) USROPN
002100131021     F************                         USROPN Commit
002200131219     F*------------
002300131219C2231F* Testata elaborazione
002400131219C2231FNDStt01L  IF   E           K DISK    RENAME(NDSTT000:NDSTT01)
002500130930     F*------------
002600130930     F* Spesometro: messaggi estrazione
002700130930     FNDSMS01L  UF A E           K DISK
002800131030C2228F*------------
002900131030 "   F* Spesometro: trattamenti speciali
003000131030 "   FNDSMT01L  IF   E           K DISK    Rename(NDSMT000:NDSMT01)
003100131030 "   F*------------
003200131030 "   F* Spesometro: trattamenti speciali
003300131030C2228FNDSMT02L  IF   E           K DISK    Rename(NDSMT000:NDSMT02)
003400111024     F*------------
003500111024     F* Testata registrazioni
003600111024     F* key: SYS/NRASREG
003700111024     FNDREG01L  IF   E           K DISK    Rename(NDREG000:NDREG01)
003800111024     F*------------
003900111024     F* Testata registrazioni
004000111024     F* key: SOCIETA/DTREG
004100111024     FNDREG02L  IF   E           K DISK    Rename(NDREG000:NDREG02)
004200111024     F*------------
004300111024     F* Movimenti IVA
004400111024     F* key: SYS/NRASREG/NRRIGAM/NRRIGA
004500111024     FNDIVA01L  IF   E           K DISK
004600111024     F*------------
004700111024     F* Contabilità generale
004800111024     F* key: SYS/NRASREG/NRRIGAM
004900111024     FNDMOV01L  IF   E           K DISK
005000111024     F*------------
005100111024     F* Analitica
005200111024     F* key: SYS/NRASREG/NRRIGAM/DIMEN/NRRIGA
005300111024     FNDMOA01L  IF   E           K DISK
005400111024     F*------------
005500111024     F* Particolarità di partita
005600111024     F* key: SOC/CTB/KCC/KSC/DTPAR/NRPAR/SERPAR
005700111024     FNDPPA01L  IF   E           K DISK
005800111024     F*------------
005900111024     F* Partite e scadenze
006000111024     F* key: SYS/NRASREG/NRRIGAM/NRRIGA
006100111024     FNDPAS01L  IF   E           K DISK
006200111024     F*------------
006300111024     F* Libri IVA
006400111024     F* key: SOC/TPREG/LIBRO
006500111024     FANDLI01L  IF   E           K DISK
006600111024     F*------------
006700111024     F* Riferimenti IVA
006800111024     F* key: SOC/TPREG/ALIQ/RIFIVA
006900111024     FANRIV01L  IF   E           K DISK
007000111024     F*-------------
007100111024     F* Associazioni Libri/Riferimenti IVA
007200111024     FANALR01L  IF   E           K DISK
007300111024     F*------------
007400111024     F* Estensione nominativi partite
007500111024     F* key: SYS/NRASINT
007600111024     FNDCPA01L  IF   E           K DISK
007700111024     F*----------
007800111026     F* liste segnalazioni
007900130925     FNDCP87P1  O    E             Printer OflInd(*In37)
008000130925     FNDCP87P2  O    E             Printer OflInd(*In38)
008100111024     D*-------------
008200111024     D* Passaggio Parametri
008300111024     D KPJBA         E DS
008400131001     D* Parametri di lancio
008500131001     D NDCP87DS      E DS                  Inz
008600111024     D*-------------
008700111024     D* Flag di errore di esecuzione
008800111024     D pErrEse         S              1A
008900111024     D*-------------
009000111024     D* Reperimento nome PGM
009100111024     D STATUS         SDS           333
009200111024     D  DSPGM            *PROC
009300111024     D  PARMS            *PARMS
009400111026     D*-------------
009500111026     D* Parametri per pgm controllo comunicazione Black List
009600111026     D NDCP91DS      E DS                  Inz
009700111026     D*-------------
009800111026     D* Parametri per pgm controllo comunicazione Intrastat
009900111026     D NDCP92DS      E DS                  Inz
010000111024     D*-------------
010100111024     D* Dati di ambiente ottenuti da XSOC
010200111024     D SOC001        E DS                  ExtName(XSOC001DS)
010300111024     D*-------------
010400111024     D* DS Interna per dati di output di XSOC
010500111024     D XSOCDS          DS          1000
010600111024     D*-------------
010700111024     D* Richiamo a XATB
010800111024     D XATBDS        E DS                  Inz
010900111025     D*-------------
011000111025     D* Parametri per XPAR
011100111025     D  XPaSoc         S              3
011200111025     D  XPaPar         S              8
011300111025     D  XPaOut         S           2000
011400111025     D  XPaErr         S              1
011500111025     D  XPaRic         S              1
011600111025     D*-------------
011700130925     D* parametro modulo PJSPES13
011800130925     D ANP098DS      E DS
011900111110     D*-------------
012000131016     D* tabella CP3 - Causali speciali
012100131016     D AngCP3DS      E DS                  Inz
012200111024     D*-------------
012300111024     D* tabella CP1 - Causali escluse
012400111024     D AngCP1DS      E DS                  Inz
012500111024     D*-------------
012600111024     D* tabella G12 - Stati
012700111024     D AngG12DS      E DS                  Inz
012800111026     D*-------------
012900111026     D* tabella G12 - Natura giuridica
013000111026     D AngG16DS      E DS                  Inz
013100111024     D*-------------
013200111024     D* Dati del soggetto
013300111024     D DVASOG        E DS
013400111024     D*-------------
013500111024     D* Anagrafica soggetto: estensione
013600111024     D ANSO1         E DS                  ExtName(ANSO100F)
013700111024     D*---------
013800111024     D* Variazioni estremi fiscali
013900111024     D ANCPI         E DS                  ExtName(ANCPI00F)
014000111024     D*---------
014100111024     D* Estensione nominativi partite
014200111024     D NDCPA         E DS                  ExtName(NDCPA00F)
014300111024     D*---------
014400111024     D* Dati Spesometro
014500130925     D NDSda         E DS                  ExtName(NDSda00F)
014600111024     D*--------
014700111024     D* Definizioni parametri driver soggetti
014800111024     D DvaRic          S              1
014900111024     D DvaSocieta      S              3
015000111024     D DvaUnita        S              8
015100111024     D DvaStrutt       S             10
015200111024     D DvaDtRif        S               D
015300111024     D DvaLenOut       S              9B 0
015400111024     D DvaSnatura      S              1
015500111024     D DvaCodice       S              8
015600111024     D DvaLineav       S              3
015700111024     D DvaFiliale      S              3  0
015800111024     D DvaSogg         S              8
015900111024     D DvaTpInd        S              2
016000111024     D DvaCdInd        S              3
016100111024     D DvaOutput       S           4000
016200111024     D DvaErrore       S              1
016300111024     D*-------------
016400111024     D* Driver estremi fiscali (ANCPI00F)
016500111024     D DVCRic          S              1
016600111024     D DVCSocieta      S              3
016700111024     D DVCUnita        S              8
016800111024     D DVCStrutt       S             10
016900111024     D DVCDtRif        S               D
017000111024     D DVCLenOut       S              9B 0
017100111024     D DVCSogg         S              8
017200111024     D DVCErrore       S              1
017300111024     D DVCOutput       S           4000
017400111024     D*-------------
017500111024     D* Dati del conto
017600111024     D ND002DS       E DS
017700111024     D*-------------
017800111024     D* Anagrafica causali operative
017900111024     D AnOpe         E DS                  ExtName(ANOPE00F)
018000111024     D*-------------
018100111024     D* Definizioni parametri standard chiamate moduli
018200111024     D Refresh         S              1A
018300111024     D GesMsg          S              1A
018400111024     D Esito           S              1A
018500111024     D StrutturaO      S             10A
018600111024     D LenOut          S              9B 0
018700111024     D Uso             S              1A
018800111024     D Data            S               D
018900111024     D*-------------
019000111024     D* Flag di fine lettura su NDREG00F
019100111024     D $EofReg         S              1A
019200111024     D* Flag di fine lettura su NDIVA00F
019300111024     D $EofIva         S              1A
019400111024     D*-------------
019500111024     D* Flag di registrazione da elaborare
019600111024     D $RegOk          S              1A
019700111024     D*-------------
019800130925     D* Flag di registrazione già presente su NDSda
019900130925     D $EsisteSda      S              1A
020000111024     D*-------------
020100111024     D* Flag di registrazione con almeno un record di NDIVA
020200111024     D $EsisteIva      S              1A
020300111026     D*-------------
020400131003     D* Flag di errore / esclusione per scrittura NDsda00F
020500130925     D $Errore         S                   Like(SdaErr)
020600131003     D $Esclus         S                   Like(SdaEsc)
020700111108     D*-------------
020800111108     D* Flag di errore che pregiudica il calcolo degli importi
020900111108     D $ErrImp         S              1A
021000111108     D* Flag di errore che indica che i dati sono incompleti
021100111108     D $ErrInc         S              1A
021200111024     D*-------------
021300130925     D* Dati vari per scrittura NDSda00F
021400131010     D $NrAsInt        S                   Like(SdaNumInt)
021500131002     D $Quadro         S                   Like(SdaQuadro)
021600131009     D $TipOpe         S                   Like(SdaTipOpe)
021700131009     D $DtOpe          S                   Like(Sdadtope )
021800131009     D $NrOpe          S                   Like(SdaNRope )
021900111025     D*-------------
022000111025     D* Dati Iva di testata
022100130925     D $TpReg          S                   Like(SdaTpReg)
022200130925     D $Libro          S                   Like(SdaLibro)
022300111026     D*-------------
022400111026     D* Flag che indica la registrazione è una nota di variazione
022500111026     D* principale del cliente / fornitore
022600111026     D $Nota           S              1A
022700131104C2228x*$SegnoN         S              1S 0 Inz(0)
022800131003     D*-------------
022900131003     D* Flag che indica la registrazione è un doc riepilogativo
023000131016     D $DocRie         S              1A
023100131016     D* Flag che indica la registrazione è un reverse charge
023200131016     D $RevChar        S              1A
023300111031     D*-------------
023400111031     D* Flag che indica la registrazione è una autofattura
023500111031     D $Autofat        S              1A
023600111024     D*-------------
023700111024     D* Flag che indica che è stata reperita la partita del movimento
023800111024     D* principale del cliente / fornitore
023900111024     D $Partita        S              1A
024000111024     D*-------------
024100111026     D* Soggetto del conto del movimento cliente / fornitore
024200111024     D $CFSogg         S                   Like(Sogg002)
024300111026     D* Tipo soggetto del conto del movimento cliente / fornitore
024400111026     D $CFCliFor       S                   Like(SNatura002)
024500111024     D* Conto del movimento cliente / fornitore
024600111024     D $CFKcc          S                   Like(MovKcc)
024700111024     D $CFKsc          S                   Like(MovKsc)
024800111024     D* Segni dare / avere del movimento cliente / fornitore
024900111024     D $CFDare         S                   Like(MovDare)
025000111024     D $CFAvere        S                   Like(MovAvere)
025100111024     D* Importo in m.c. del movimento cliente / fornitore
025200111024     D $CFImporto      S                   Like(MovImporto)
025300111104     D* Flag di forzatura sul conto cliente / fornitore
025400111104     D $CFModPag       S              1A
025500111104     D $CFGesSog       S              1A
025600131008     D* Flag di accorpamento forzato sul conto
025700131008     D $CFAccFor       S              1A
025800111024     D*-------------
025900111024     D* Segno per calcoli
026000111024     D $Segno          S              1S 0
026100111024     D*-------------
026200111024     D* Tipologia di dati del soggetto:
026300111024     D* '0' = nessun dato reperito
026400111024     D* '1' = dati reperiti da NDCPA00F (soggetto estemporaneo)
026500111024     D* '2' = dati reperiti da ANCPI00F (variazione fiscale)
026600111024     D* '3' = dati reperiti da DVASOG (dati anagrafici)
026700131001     D $TipSog         S              1A
026800111024     D*-------------
026900111024     D* Dati del soggetto per controlli
027000111024     D $Stato          S                   Like(DvsStato)
027100111110     D $StaNa          S                   Like(DvsStato)
027200111026     D $NatGiur        S                   Like(DvsNatGiur)
027300111026     D*-------------
027400111026     D* Flag di soggetto persona fisica
027500111026     D $PFisica        S              1A
027600111024     D*-------------
027700111024     D* Flag di soggetto residente
027800111024     D $Residente      S              1A
027900111024     D*-------------
028000131001     D* Dati anagrafici per scrittura NDSda
028100130925     D $CdFisc         S                   Like(SdaCdFisc )
028200130925     D $PartIva        S                   Like(SdaPartIva)
028300130925     D $Cognome        S                   Like(SdaCognome)
028400130925     D $Nome           S                   Like(SdaNome   )
028500130925     D $DtNasc         S                   Like(SdaDtNasc )
028600130925     D $ComNa          S                   Like(SdaComNa  )
028700130925     D $PrvNa          S                   Like(SdaPrvNa  )
028800131003     D $StaEsD         S                   Like(SdaStaEsDo)
028900130925     D $RagSoc         S                   Like(SdaRagSoc )
029000131003     D $LocEst         S                   Like(SdaLocEsSL)
029100131003     D $StaEsS         S                   Like(SdaStaEsSL)
029200131003     D $IndEst         S                   Like(SdaIndEsSL)
029300111024     D*-------------
029400131001     D* Importi per scrittura NDSda00F
029500130925     D $Imponib        S                   Like(SdaImponib)
029600130925     D $Imposta        S                   Like(SdaImposta)
029700130925     D $Importo        S                   Like(SdaImporto)
029800131121C2231D $ImponibP       S                   Like(SdaImponib)
029900131121 "   D $ImpostaP       S                   Like(SdaImposta)
030000131121 "   D $ImportoP       S                   Like(SdaImporto)
030100131121 "   D $ImponibN       S                   Like(SdaImponib)
030200131121 "   D $ImpostaN       S                   Like(SdaImposta)
030300131121C2231D $ImportoN       S                   Like(SdaImporto)
030400111108     D*-------------
030500131030C2228D* Flag trattamenti speciali
030600131030 "   D $TraSpe         S              1A
030700131030 "   D $TraSpe31       S              1A
030800131030 "   D* Codice per verifica
030900131030C2228D $Codice         S                   Like(SmtCodice)
031000131030     D*-------------
031100111108     D* costante per schiere
031200111108     D cMsgMax         C                   Const(50)
031300111108     D* schiera per scrittura NDSMS00F (errori)
031400111108     D SkTipMsgE       S                   Like(SmsTipMsg) Dim(cMsgMax)
031500111108     D SkIntMsgE       S                   Like(SmsIntMsg) Dim(cMsgMax)
031600111108     D SkCodMsgE       S                   Like(SmsCodMsg) Dim(cMsgMax)
031700111108     D* contatore riservato a schiera messaggi di errore
031800111108     D $MsgE           S              3P 0
031900111108     D* schiera per scrittura NDSMS00F (esclusione)
032000111108     D SkTipMsgX       S                   Like(SmsTipMsg) Dim(cMsgMax)
032100111108     D SkIntMsgX       S                   Like(SmsIntMsg) Dim(cMsgMax)
032200111108     D SkCodMsgX       S                   Like(SmsCodMsg) Dim(cMsgMax)
032300111108     D* contatore riservato a schiera messaggi di esclusione
032400111108     D $MsgX           S              3P 0
032500111027     D*-------------
032600111027     D* campi per scrittura NDSMS00F
032700111027     D $TipMsg         S                   Like(SmsTipMsg)
032800111027     D $IntMsg         S                   Like(SmsIntMsg)
032900111027     D $CodMsg         S                   Like(SmsCodMsg)
033000111027     D $TxtMsg         S                   Like(SmsTxtMsg)
033100111111     D*-------------
033200111111     D* estremi registrazioni per scrittura NDSMS00F
033300130925     D $Sys            S                   Like(SdaSys)
033400130925     D $NrAsReg        S                   Like(SdaNrAsReg)
033500111024     D*-------------
033600111025     D* flag che indica che il riferimento Iva è escluso
033700111025     D $RifEsc         S              1A
033800111024     D* Variabili per analisi su NDIVA
033900111108     D* flag che indica se c'è almeno un riferimento IVA escluso
034000111108     D $ErrIva         S              1A
034100111024     D* flag che indica se c'è almeno un riferimento IVA escluso
034200111024     D $EscAlmUno      S              1A
034300111024     D* flag che indica se c'è almeno un riferimento IVA non escluso
034400111024     D $NoEAlmUno      S              1A
034500111024     D*-------------
034600111024     D* dati per routine di controllo del segno
034700111027     D $Dare           S                   Like(IvaDare)
034800111027     D $Avere          S                   Like(IvaAvere)
034900111031     D*-------------
035000131219C2231x** date inizio e fine per lettura registrazioni
035100131219 "   x** $DtRegD         S               D
035200131219C2231x** $DtRegA         S               D
035300111108     D*-------------
035400111108     D* indici generici
035500111108     D $I              S              5S 0
035600111025     D*-------------
035700111025     D* Dati per reperimento tramite driver
035800111025     D WSocieta        S                   Like(RegSocieta)
035900111025     D WKcc            S                   Like(MovKcc)
036000111025     D WKsc            S                   Like(MovKsc)
036100111025     D WCausale        S                   Like(RegCausTes)
036200111024     D*-------------
036300111024     D* variabili di lavoro
036400111027     D WImporto        S                   Like(MovImporto)
036500111110     D WStato          S                   Like(DvsStato)
036600111031     D WDataIni        S             10A   Inz('xxxx-01-01')
036700111031     D WDataFin        S             10A   Inz('xxxx-12-31')
036800111024     D*-------------
036900111027     D* data del giorno
037000111024     D UDateISO        S               D
037100111024     D*-------------
037200111024     D* variabili di lavoro
037300111024     D WIdMsg          S                   Like(IdMsg)
037400111024     D                                     Inz('PROMSG    *LIBL     xxxxxxx')
037500131008     D*-------------
037600131008     D* Controlli validità legende
037700131008     D X07ValDS      E DS
037800111024     C*----------------------------------------------------*
037900111024     C*                MAIN LINE PROGRAM
038000111024     C*----------------------------------------------------*
038100111024     C*
038200111024     C* operazioni iniziali
038300111024     C                   ExSr      InzVar
038400111024     C*
038500111024     C* elaborazione
038600111024     C                   ExSr      Elabora
038700111024     C*
038800111024     C* fine programma
038900111024     C                   ExSr      EndPgm
039000111024     C*
039100111024      /EJECT
039200111024     C************************************************************
039300111024     C* Fine Programma
039400111024     C************************************************************
039500111024     C     ENDPGM        BEGSR
039600111024     C*
039700130925     C                   MOVEL     NDCP87DS      KPJBU
039800111024     C*
039900111024     C                   SetOn                                            LR
040000111024     C                   Return
040100111024     C*
040200111024     C                   ENDSR
040300111024      /EJECT
040400131004     C************************************************************
040500131004     C* Elaborazione principale
040600131004     C************************************************************
040700131004     C     Elabora       BEGSR
040800131004     C*
040900111031     C* imposto la data registrazione iniziale e finale
041000131219C2231x***                Movel     AnnoP87       WDataIni
041100131219 "   x***                Movel     WDataIni      $DtRegD
041200131219 "   x***                Movel     AnnoP87       WDataFin
041300131219C2231x***                Movel     WDataFin      $DtRegA
041400111024     C*
041500111024     C* leggo tutte le registrazioni del periodo indicato
041600130925     C                   Eval      RegSocieta = SocietaP87
041700131219C2231x****               Eval      RegDtReg   = $DtRegD
041800131219C2231C                   Eval      RegDtReg   = STTDTESTRI
041900111024     C     K02Reg02      Setll     NDREG02L
042000111024     C     RegSocieta    ReadE     NDREG02L                               21
042100111024     C                   Movel     *In21         $EofReg
042200111024     C                   DoW       $EofReg = *Off And
042300131219C2231x****                           RegDtReg <= $DtRegA
042400131219C2231C                               RegDtReg <= STTDTESTRF
042500111024     C*
042600111024     C* inizializzo variabili per analisi registrazione
042700111024     C                   ExSr      InzReg
042800111024     C*
042900111024     C* verifico se la registrazione è da elaborare
043000111024     C                   ExSr      SelReg
043100111024     C*
043200111024     C* elaborazione registrazione
043300111024     C                   If        $RegOk = *on
043400111024     C                   ExSr      ElaReg
043500111024     C                   EndIf
043600111024     C*
043700111024     C* leggo prossima registrazione
043800111024     C     RegSocieta    ReadE     NDREG02L                               21
043900111024     C                   Movel     *In21         $EofReg
044000111024     C                   EndDo
044100111024     C*
044200111024     C                   ENDSR
044300111024      /EJECT
044400111024     C************************************************************
044500111024     C* Inizializzazione registrazione
044600111024     C************************************************************
044700111024     C     InzReg        BEGSR
044800111111     C*
044900111111     C* memorizzo estremi della registrazione per messaggi
045000111111     C                   Eval      $Sys = RegSys
045100111111     C                   Eval      $NrAsReg = RegNrAsReg
045200111024     C*
045300131003     C* flag che indica che la registrazione è già presente su NDSda
045400131001     C                   Move      *Off          $EsisteSda
045500111024     C*
045600111024     C* flag che indica che la registrazione ha almeno una riga di IVA
045700111024     C                   Move      *Off          $EsisteIva
045800111024     C*
045900111024     C* flag di errore
046000111024     C                   Move      *Off          $Errore
046100111024     C                   Move      *Off          $Esclus
046200111108     C                   Move      *Off          $ErrImp
046300111108     C                   Move      *Off          $ErrInc
046400111024     C*
046500111024     C* campi vari
046600131003     C                   Clear                   $NrAsInt
046700111024     C                   Clear                   $TipOpe
046800111024     C                   Move      *Off          $Partita
046900111025     C                   Move      *Off          $Nota
047000131104C2228x**                 Eval      $SegnoN = 0
047100111031     C                   Move      *Off          $Autofat
047200131016     C                   Move      *Off          $RevChar
047300131016     C                   Move      *Off          $DocRie
047400111025     C*
047500111025     C* campi relativi alla testata Iva
047600111025     C                   Clear                   $TpReg
047700111025     C                   Clear                   $Libro
047800111024     C*
047900111104     C* campi relativi al movimento cliente / fornitore e al conto
048000111024     C                   Clear                   $CFSogg
048100111026     C                   Clear                   $CFCliFor
048200111024     C                   Clear                   $CFKcc
048300111024     C                   Clear                   $CFKsc
048400111024     C                   Clear                   $CFDare
048500111024     C                   Clear                   $CFAvere
048600111024     C                   Clear                   $CFImporto
048700111104     C                   Clear                   $CFGesSog
048800131008     C                   Move      *Off          $CFAccFor
048900111024     C*
049000111024     C* dati del soggetto
049100111024     C                   Clear                   $TipSog
049200111024     C                   Clear                   $Stato
049300111110     C                   Clear                   $StaNa
049400111026     C                   Clear                   $NatGiur
049500111026     C                   Clear                   $Residente
049600111026     C                   Clear                   $PFisica
049700111024     C*
049800111024     C* dati anagrafici del soggetto
049900111024     C                   Clear                   $CdFisc
050000111024     C                   Clear                   $PartIva
050100111024     C                   Clear                   $Cognome
050200111024     C                   Clear                   $Nome
050300111024     C                   Clear                   $DtNasc
050400111024     C                   Clear                   $ComNa
050500111024     C                   Clear                   $PrvNa
050600111024     C                   Clear                   $StaEsD
050700111024     C                   Clear                   $RagSoc
050800111024     C                   Clear                   $LocEst
050900111024     C                   Clear                   $StaEsS
051000111024     C                   Clear                   $IndEst
051100111024     C*
051200111024     C* importi
051300111024     C                   Z-Add     0             $Imponib
051400111024     C                   Z-Add     0             $Imposta
051500111103     C                   Z-Add     0             $Importo
051600131121C2231C                   Z-Add     0             $ImponibP
051700131121 "   C                   Z-Add     0             $ImpostaP
051800131121 "   C                   Z-Add     0             $ImportoP
051900131121 "   C                   Z-Add     0             $ImponibN
052000131121 "   C                   Z-Add     0             $ImpostaN
052100131121C2231C                   Z-Add     0             $ImportoN
052200111108     C*
052300111108     C* schiere per memorizzare messaggi di errore
052400111108     C                   Z-Add     0             $MsgE
052500111108     C                   Clear                   SkTipMsgE
052600111108     C                   Clear                   SkIntMsgE
052700111108     C                   Clear                   SkCodMsgE
052800111108     C*
052900111108     C* schiere per memorizzare messaggi di esclusione
053000111108     C                   Z-Add     0             $MsgX
053100111108     C                   Clear                   SkTipMsgX
053200111108     C                   Clear                   SkIntMsgX
053300111108     C                   Clear                   SkCodMsgX
053400111024     C*
053500111024     C                   ENDSR
053600111024      /EJECT
053700111024     C************************************************************
053800111024     C* Selezione registrazione
053900111024     C************************************************************
054000111024     C     SelReg        BEGSR
054100111024     C*
054200111024     C* per default la registrazione è da elaborare
054300111024     C                   Eval      $RegOk = *On
054400111024     C*
054500111024     C                   Select
054600111024     C*
054700111024     C* escludo registrazioni annullate
054800111024     C                   When      RegAnn = '1'
054900111024     C                   Eval      $RegOk = *Off
055000111024     C*
055100111024     C* escludo registrazioni con contabilità diversa da generale
055200111024     C                   When      RegCtb <> 'CG'
055300111024     C                   Eval      $RegOk = *Off
055400111024     C*
055500111024     C* escludo registrazioni non definitive
055600111024     C                   When      RegTpRegZ <> 'D'
055700111024     C                   Eval      $RegOk = *Off
055800111024     C*
055900111024     C                   Other
056000111024     C*
056100111024     C* escludo registrazioni già estratte
056200130925     C                   ExSr      CtrEsiSda
056300130925     C                   If        $EsisteSda = *On
056400111024     C                   Eval      $RegOk = *Off
056500111024     C                   Else
056600111024     C*
056700111024     C* escludo registrazioni prive di record di NDIVA
056800111024     C                   ExSr      CtrEsiIva
056900111024     C                   If        $EsisteIva = *Off
057000111024     C                   Eval      $RegOk = *Off
057100111024     C                   EndIf
057200111024     C                   EndIf
057300111024     C                   EndSl
057400111024     C*
057500111024     C                   ENDSR
057600111024      /EJECT
057700111024     C************************************************************
057800130925     C* Controllo esistenza su NDSda00F
057900111024     C************************************************************
058000130925     C     CtrEsiSda     BEGSR
058100111024     C*
058200111024     C* verifico se la registrazione esiste già: questo può succedere
058300111024     C* se sono stati inseriti / modificati manualmente dei record
058400111024     C* e nel lancio dell'estrazione è stato chiesto di non cancellare
058500111024     C* questi record
058600130925     C                   Eval      SdaSocieta = SocietaP87
058700130925     C                   Eval      SdaSys     = RegSys
058800130925     C                   Eval      SdaNrAsReg = RegNrAsReg
058900130925     C                   Eval      SdaAnno    = AnnoP87
059000130925     C     K04Sda02      Setll     NDSda02                                21
059100130925     C                   Move      *In21         $EsisteSda
059200111024     C*
059300111024     C                   ENDSR
059400111024      /EJECT
059500111024     C************************************************************
059600111024     C* Controllo esistenza su NDIVA00F
059700111024     C************************************************************
059800111024     C     CtrEsiIva     BEGSR
059900111024     C*
060000111024     C* verifico se la registrazione ha almeno un movimento Iva
060100130925     C                   Eval      IvaSys     = RegSys
060200111024     C                   Eval      IvaNrAsReg = RegNrAsReg
060300111024     C     K02Iva01      Setll     NDIVA01L                               21
060400111024     C                   Move      *In21         $EsisteIva
060500111024     C*
060600111024     C                   ENDSR
060700111024      /EJECT
060800111024     C************************************************************
060900111024     C* Elaborazione registrazione
061000111024     C************************************************************
061100111024     C     ElaReg        BEGSR
061200111024     C*
061300111024     C* controllo la registrazione
061400111024     C                   ExSr      CtrReg
061500111025     C*
061600111026     C* se dopo le precedenti verifiche, la registrazione risulta
061700111026     C* ancora come "da elaborare", proseguo
061800111026     C                   If        $RegOk = *On
061900111024     C*
062000111027     C* leggo NDIVA
062100111024     C                   ExSr      RedIva
062200111025     C*
062300111026     C* se dopo le precedenti verifiche, la registrazione risulta
062400111026     C* ancora come "da elaborare", proseguo
062500111026     C                   If        $RegOk = *On
062600111024     C*
062700111027     C* eseguo controlli finali sulla registrazione
062800111024     C                   ExSr      CtrFin
062900131016      *
063000131016      * Controllo che il quadro sia valorizzato
063100131016     C                   ExSr      CtrQuadro
063200131016     C*
063300131016     C* scrivo archivi di output
063400131016     C                   ExSr      WrtOut
063500111108     C*
063600111108     C* scarico messaggi di errore memorizzati
063700111108     C                   ExSr      ScaSms
063800111024     C*
063900111024     C                   EndIf
064000111025     C                   EndIf
064100111024     C*
064200111024     C                   ENDSR
064300111024      /EJECT
064400111024     C************************************************************
064500131001     C* Reperimento numero assoluto per NDSda00F
064600111024     C************************************************************
064700111024     C     RepNum        BEGSR
064800111024     C*
064900111024     C                   Move      UDateIso      XNmDat
065000111024     C                   Move      UDateIso      XNmDag
065100111024     C*
065200111024     C                   CallB     'XNMR'
065300111024     C                   Parm      '2'           XNmRic            1
065400130925     C                   Parm      SocietaP87    XNmSoc            3
065500111024     C                   Parm      *Blank        XNmUni            8
065600111024     C                   Parm      *Blank        XNmCtb            2
065700111024     C                   Parm      *Blank        XNmKey            8
065800130926     C                   Parm      'S132'        XNmSer            4
065900111024     C                   Parm      *Off          XNmCmt            1
066000111024     C                   Parm                    XNmDat           10
066100111024     C                   Parm      *Off          XNmIVA            1
066200111024     C                   Parm      *Off          XNmReg            1
066300111024     C                   Parm                    XNmErr            1
066400111024     C                   Parm                    XNmNum            9 0
066500111024     C                   Parm                    XNmAut            1
066600111024     C                   Parm                    XNmDag           10
066700111024     C*
066800111024     C* se il numeratore va in errore, si tratta di un errore bloccante
066900111024     C                   If        XNmErr <> '0'
067000111031     C                   Eval      pBlkCodMsg = 'PRO3321'
067100111031     C                   ExSr      GesErrBlk
067200111024     C                   Else
067300131003     C                   Eval      $NrAsInt = XNmNum
067400111024     C                   EndIf
067500111024     C*
067600111024     C                   ENDSR
067700111024     C/EJECT
067800111024     C************************************************************
067900111024     C* Controllo della registrazione
068000111024     C************************************************************
068100111025     C     CtrReg        BEGSR
068200111025     C*
068300111025     C* metto in linea il movimento del cliente / fornitore
068400111025     C                   ExSr      RepMovCF
068500111025     C*
068600111025     C* se registrazione è ancora da elaborare, controllo causale
068700111025     C                   If        $RegOk = *On
068800111025     C                   ExSr      CtrCausale
068900111025     C                   EndIf
069000111025     C*
069100131104     C* se registrazione è ancora da elaborare, controllo soggetto
069200131104     C                   If        $RegOk = *On
069300131104     C                   ExSr      CtrSog
069400131104     C                   EndIf
069500111025     C*
069600111025     C* se registrazione è ancora da elaborare,
069700111024     C* controllo presenza registrazione in altre comunicazioni
069800111025     C                   If        $RegOk = *On
069900111025     C                   ExSr      CtrComunic
070000111025     C                   EndIf
070100111025     C*
070200111025     C                   ENDSR
070300111025      /EJECT
070400111024     C************************************************************
070500111024     C* Controllo causale
070600111024     C************************************************************
070700111024     C     CtrCausale    BEGSR
070800111024     C*
070900111024     C* controllo presenza causale in tabella CP1
071000111024     C                   ExSr      RepTabCP1
071100111024     C                   If        XTbErr = '0'
071200111024     C* Causale esclusa dalla comunicazione
071300111024     C                   Eval      $TipMsg = '2'
071400111108     C                   Eval      $IntMsg = '002'
071500111024     C                   Eval      $CodMsg = 'NDC1016'
071600111109     C                   ExSr      MemSms
071700111024     C                   EndIf
071800111024     C*
071900111024     C* reperimento dei dati di causale
072000111024     C                   Eval      WCausale = RegCausTes
072100111024     C                   ExSr      CallMvc003
072200111108     C*
072300111108     C                   If        Esito = *On
072400111024     C* Causale di testata inesistente
072500111024     C                   Eval      $TipMsg = '1'
072600111108     C                   Eval      $IntMsg = '003'
072700111024     C                   Eval      $CodMsg = 'MVC0001'
072800111109     C                   ExSr      MemSms
072900111108     C* questo errore pregiudica il calcolo degli importi
073000111108     C                   Eval      $ErrImp = *On
073100111108     C                   Else
073200111025     C*
073300111025     C* Verifico se la fattura è una Nota di variazione
073400111026     C                   If        OpeTpDocI = '1' Or
073500111026     C                             OpeTpDocI = '4' Or
073600111026     C                             OpeTpDocI = '7'
073700111025     C                   Eval      $Nota = *On
073800111025     C                   EndIf
073900131010     C*
074000131104C2228x**Se è una nota ne imposto il segno
074100131104  "  x**                 If        $nota   = *on
074200131104  "  x**                 Eval      $SegnoN = 1
074300131104  "  x**in base al tipo operazione Cliente='1'
074400131104  "  x**se è addebito imposto il segno negativo
074500131104  "  x**altrimenti è accredito
074600131104  "  x**                 If        ($TipOpe = '1'
074700131104  "  x**                           and $CFDare = 1   )
074800131104  "  x**                           OR
074900131104  "  x**                           ($TipOpe = '2'
075000131104  "  x**                            and $CFavere = 1 )
075100131104  "  x**                 Eval      $SegnoN = -1
075200131104  "  x**                 Endif
075300131104C2228x**                 Endif
075400111025     C*
075500111025     C* verifico se autofattura
075600111025     C                   If        OpeTpDocI = '8'
075700131019     C*********          Eval      $Autofat = *On
075800131016     C* Non la escludo
075900131016     C*** Registrazione di autofattura
076000131016     C***                Eval      $TipMsg = '2'
076100131016     C***                Eval      $IntMsg = '017'
076200131016     C***                Eval      $CodMsg = 'NDC1000'
076300131016     C***                ExSr      memsms
076400111025     C                   EndIf
076500131016     C*
076600131016     C* Controllo se è una causale particolare
076700131016     C* Potrei averla definita qui come causale di autofattura
076800131016     C* o come documento riepilogativo
076900131016     C                   Exsr      RepTabCp3
077000111108     C*
077100111108     C                   EndIf
077200111024     C*
077300111024     C                   ENDSR
077400111024      /EJECT
077500111024     C************************************************************
077600111024     C* Controllo soggetto
077700111024     C************************************************************
077800111024     C     CtrSog        BEGSR
077900111024     C*
078000111024     C* se reperimento del conto riuscito, proseguo...
078100111024     C                   If        $CFKcc <> *Blanks And
078200111024     C                             $CFKsc <> *Blanks
078300111024     C*
078400111024     C* metto in linea i dati di partita del movimento principale,
078500111024     C* necessario per successivi controlli
078600111024     C                   ExSr      RepPartita
078700111024     C*
078800111024     C* reperisco i dati del conto cliente / fornitore,
078900111024     C                   ExSr      RepContoCF
079000111024     C*
079100111024     C* se reperimento del codice soggetto riuscito, proseguo...
079200111024     C                   If        $CfSogg <> *Blanks
079300111024     C* reperisco i dati del soggetto
079400111024     C                   ExSr      RepSog
079500111024     C*
079600111024     C* se reperimento dello stato riuscito, proseguo...
079700111024     C                   If        $Stato <> *Blanks
079800111024     C* reperisco i dati dello stato del soggetto
079900111024     C                   ExSr      RepStato
080000111024     C                   EndIf
080100111024     C                   EndIf
080200111024     C                   EndIf
080300111024     C*
080400111024     C                   ENDSR
080500111024      /EJECT
080600111025     C************************************************************
080700111025     C* Controllo presenza registrazioni in altre comunicazioni
080800111025     C************************************************************
080900111025     C     CtrComunic    BEGSR
081000111025     C*
081100111025     C* il controllo viene eseguito solo per i soggetti NON residenti
081200111025     C                   If        $Residente = *Off
081300111025     C*
081400111025     C* controllo se registrazione già comunicata in Black List
081500111025     C                   ExSr      CtrBlack
081600111025     C*
081700111025     C* controllo se registrazione già comunicata in Intrastat
081800111025     C                   ExSr      CtrIntra
081900131218      *
082000131218C2233 * Se tipo elaborazione = spesometro
082100131219  "  C                   If        TpElaP87 = '1'
082200131219  "   * controllo se acquisto di San Marino
082300131218  "   * e nel caso lo escludo
082400131219  "  C                   ExSr      CtrAcqSM
082500131218C2233C                   Endif
082600111025     C*
082700111025     C                   EndIf
082800111025     C*
082900111025     C                   ENDSR
083000111025      /EJECT
083100111026     C************************************************************
083200111026     C* Controllo se registrazione già comunicata in Black List
083300111026     C************************************************************
083400111026     C     CtrBlack      BEGSR
083500111026     C*
083600111026     C                   Clear                   NDCP91DS
083700111026     C*
083800130925     C                   Eval      SocietaP91 = SocietaP87
083900130925     C                   Eval      AnnoP91 = AnnoP87
084000111026     C*
084100111026     C                   Eval      SysP91 = RegSys
084200111026     C                   Eval      NrAsRegP91 = RegNrAsReg
084300111026     C*
084400111026     C                   Eval      ComunicP91 = *Off
084500111026     C                   Eval      ErrP91 = *Off
084600111026     C*
084700111026     C                   Movel     NDCP91DS      Kpjbu
084800111026     C                   Call      'NDCP91R'                            21
084900111026     C                   Parm                    Kpjba
085000111026     C                   Movel     Kpjbu         NDCP91DS
085100111026     C*
085200111026     C                   Select
085300111026     C* Errore durante il controllo sulla Black List
085400111026     C                   When      ErrP91 <> '0'
085500111026     C                   Eval      $TipMsg = '1'
085600111026     C                   Eval      $IntMsg = '012'
085700111026     C                   Eval      $CodMsg = 'NDC1198'
085800111109     C                   ExSr      MemSms
085900111026     C* Operazione già comunicata tramite Black List
086000111026     C                   When      ComunicP91 = *On
086100111026     C                   Eval      $TipMsg = '2'
086200111026     C                   Eval      $IntMsg = '013'
086300111026     C                   Eval      $CodMsg = 'NDC1188'
086400111109     C                   ExSr      MemSms
086500111026     C                   EndSl
086600111026     C*
086700111026     C                   ENDSR
086800111026      /EJECT
086900111026     C************************************************************
087000111026     C* Controllo se registrazione già comunicata in Intra
087100111026     C************************************************************
087200111026     C     CtrIntra      BEGSR
087300111026     C*
087400111026     C                   Clear                   NDCP92DS
087500111026     C*
087600130925     C                   Eval      SocietaP92 = SocietaP87
087700130925     C                   Eval      AnnoP92 = AnnoP87
087800111026     C*
087900111026     C                   Eval      DtDocP92 = RegDtDoc
088000111026     C                   Eval      NrDocP92 = RegNrDoc
088100111026     C                   Eval      SerDocP92 = RegSerDoc
088200111026     C*
088300111026     C                   Eval      TipOpeP92 = $TipOpe
088400111026     C                   Eval      PartIvaP92 = $PartIva
088500111026     C*
088600111026     C                   Eval      ComunicP92 = *Off
088700111026     C                   Eval      ErrP92 = *Off
088800111026     C*
088900111026     C                   Movel     NDCP92DS      Kpjbu
089000111026     C                   Call      'NDCP92R'                            21
089100111026     C                   Parm                    Kpjba
089200111026     C                   Movel     Kpjbu         NDCP92DS
089300111026     C*
089400111026     C                   Select
089500111026     C* Errore durante il controllo su Intrastat
089600111026     C                   When      ErrP92 <> '0'
089700111026     C                   Eval      $TipMsg = '1'
089800111026     C                   Eval      $IntMsg = '014'
089900111026     C                   Eval      $CodMsg = 'NDC1199'
090000111109     C                   ExSr      MemSms
090100111026     C* Operazione già comunicata tramite Intrastat
090200111026     C                   When      ComunicP92 = *On
090300111026     C                   Eval      $TipMsg = '2'
090400111026     C                   Eval      $IntMsg = '015'
090500111026     C                   Eval      $CodMsg = 'NDC1189'
090600111109     C                   ExSr      MemSms
090700111026     C                   EndSl
090800111026     C*
090900111026     C                   ENDSR
091000131219C2233C************************************************************
091100131126 "   C* Controllo se registrazione di acq San Marino
091200131126 "   C************************************************************
091300131219C2233C     CtrAcqSM      BEGSR
091400131126     C*
091500131126     C* Se è un acquisto
091600131126     C                   If        $TipOpe  = '2'
091700131126     C                             and $Stato <> *Blanks
091800131126     C                             and CodUicG12 = 37
091900131126     C                   Eval      $TipMsg = '2'
092000131126     C                   Eval      $IntMsg = '015'
092100131126     C                   Eval      $CodMsg = 'NDC1231'
092200131126     C                   ExSr      MemSms
092300131126     C                   Endif
092400131126      *
092500131219C2233C                   ENDSR
092600111026      /EJECT
092700111025     C************************************************************
092800111025     C* Lettura NDIVA00F
092900111025     C************************************************************
093000111025     C     RedIva        BEGSR
093100111108     C*
093200111108     C* flag di errore che invalida lettura righe Iva
093300111108     C                   Movel     *Off          $ErrIva
093400111025     C*
093500111025     C* flag di controllo per presenza dei riferimenti di esclusione
093600111025     C                   Movel     *Off          $EscAlmUno
093700111025     C                   Movel     *Off          $NoEAlmUno
093800111025     C*
093900111025     C* importi in m.c. divisi per tipo operazione iva
094000111025     C                   Clear                   $Imponib
094100111025     C                   Clear                   $Imposta
094200131121C2231C                   Clear                   $ImponibP
094300131121 "   C                   Clear                   $ImpostaP
094400131121 "   C                   Clear                   $ImponibN
094500131121C2231C                   Clear                   $ImpostaN
094600111025     C*
094700111025     C* leggo tutte le righe di Iva della registrazione
094800111025     C                   Eval      IvaSys = RegSys
094900111025     C                   Eval      IvaNrAsReg = RegNrAsReg
095000111025     C     K02Iva01      Setll     NDIVA01L
095100111025     C     K02Iva01      ReadE     NDIVA01L                               21
095200111025     C                   Movel     *In21         $EofIva
095300111025     C*
095400111025     C* se prima lettura riuscita (dovrebbe, visto che era già stata
095500111025     C* verificata l'esistenza di almeno una riga di Iva)
095600111025     C                   If        *In21 = *Off
095700111025     C*
095800111025     C* memorizzo dati Iva "di testata"
095900111025     C                   Eval      $TpReg = IvaTpReg
096000111025     C                   Eval      $Libro = IvaLibro
096100111025     C*
096200111025     C* reperisco i dati anagrafici del libro, facendolo un'unica volta
096300111025     C* fuori dal ciclo visto che il dato si ripete su tutte le righe
096400111025     C                   ExSr      RepLibro
096500111025     C                   EndIf
096600111025     C*
096700111025     C* ciclo su tutte le righe di Iva
096800111025     C                   DoW       $EofIva = *Off And
096900111108     C                               $ErrIva = *Off And
097000111025     C                               $RegOk = *On
097100111025     C*
097200111025     C* elaboro la riga di Iva
097300111025     C                   ExSr      ElaIva
097400111025     C*
097500111025     C* leggo prossima riga di Iva
097600111025     C     K02Iva01      ReadE     NDIVA01L                               21
097700111025     C                   Movel     *In21         $EofIva
097800111025     C                   EndDo
097900111108     C*
098000111108     C* se registrazione non risulta da scartare, eseguo controlli
098100111108     C* finali
098200111108     C                   If        $RegOk = *On
098300111031     C*
098400111031     C* terminato il ciclo di lettura di NDIVA, verifico se escludere
098500111031     C* la registrazione in base ai riferimenti Iva esclusi e a quanto
098600111031     C* richiesto nell'apposito campo di parametro modulo
098700111108     C                   If        $ErrIva = *Off
098800111031     C                   ExSr      CtrEscReg
098900111108     C* se rilevato errore grave bloccante durante lettura di Iva,
099000111108     C* si tratta di un errore che impedisce il calcolo degli importi
099100111108     C                   Else
099200111108     C                   Eval      $ErrImp = *On
099300111108     C                   EndIf
099400111025     C*
099500111108     C                   EndIf
099600111108     C*
099700111025     C                   ENDSR
099800111025      /EJECT
099900111025     C************************************************************
100000111025     C* Elaborazione riga Iva
100100111025     C************************************************************
100200111025     C     ElaIva        BEGSR
100300111025     C*
100400111025     C* inizializzo il flag che indica che il riferimento di riga
100500111025     C* è escluso dallo Spesometro
100600111025     C                   Eval      $RifEsc = *Off
100700111025     C*
100800111025     C* gestione del segno
100900111025     C                   Eval      $Dare = IvaDare
101000111025     C                   Eval      $Avere = IvaAvere
101100111025     C                   ExSr      GesSegno
101200111025     C*
101300111025     C* lettura riferimento Iva
101400111025     C                   Eval      RIvSocieta = IvaSocieta
101500130930     C                   Eval      RIvTpReg   = IvaTpReg
101600130930     C                   Eval      RIvAliq    = IvaAliq
101700130930     C                   Eval      RIvRifIva  = IvaRifIva
101800111025     C     K04RIv01      Chain     ANRIV01L                           21
101900130930     C                   If        *In21      = *On
102000130930     C                   Eval      $ErrIva    = *On
102100111025     C* Riferimento IVA inesistente in anagrafica.
102200111025     C                   Eval      $TipMsg = '1'
102300111026     C                   Eval      $IntMsg = '018'
102400111025     C                   Eval      $CodMsg = 'NDC1010'
102500111109     C                   ExSr      MemSms
102600111025     C                   Else
102700111025     C*
102800111025     C* lettura associazione riferimento IVA - Libro
102900111025     C                   Eval      ALRSocieta = IvaSocieta
103000111025     C                   Eval      ALRTpReg = IvaTpReg
103100111025     C                   Eval      ALRLibro = IvaLibro
103200111025     C                   Eval      ALRAliq = IvaAliq
103300111025     C                   Eval      ALRRifIva = IvaRifIva
103400111028     C     K05ALR01      Chain     ANALR01L                           21
103500111025     C                   If        *In21 = *On
103600111108     C                   Eval      $ErrIva = *On
103700111025     C* Associazione libro-riferimento IVA inesistente
103800111025     C                   Eval      $TipMsg = '1'
103900111026     C                   Eval      $IntMsg = '019'
104000111025     C                   Eval      $CodMsg = 'NDC0997'
104100111109     C                   ExSr      MemSms
104200111025     C                   Else
104300131030     C*
104400131030C2228C* reperimento dei trattamenti speciali a livello di riga Iva
104500131030C2228C                   ExSr      RepTraIva
104600130930      *
104700130930      * scarto tipo registro corrispettivo, sempre
104800130930     C                   If        IvaTpReg = '3'
104900131030C2228C                             and $TraSpe31 = *Off
105000130930     C                   ExSr      ScartaReg
105100130930     C                   Else
105200111025     C*
105300111025     C* memorizzo flag di esclusione della riga
105400111031     C* e imposto i flag di "almeno un escluso/non escluso trovato"
105500111025     C                   If        ALRElenchi = '01'
105600111025     C                   Eval      $RifEsc = '1'
105700111031     C                   Movel     *On           $EscAlmUno
105800111025     C                   Else
105900111025     C                   Eval      $RifEsc = '0'
106000111031     C                   Movel     *On           $NoEAlmUno
106100111025     C                   EndIf
106200111025     C*
106300111025     C* verifico coerenza tra conto C/F e conto di contropartita
106400111025     C                   If        $CFKcc <> IvaKccCP Or
106500111025     C                             $CFKsc <> IvaKscCP
106600111025     C* La contropartita del movimento IVA non corrisponde al
106700111025     C* cliente/fornitore della registrazione.
106800111025     C                   Eval      $TipMsg = '3'
106900111026     C                   Eval      $IntMsg = '020'
107000111025     C                   Eval      $CodMsg = 'NDC1007'
107100111109     C                   ExSr      MemSms
107200111025     C                   EndIf
107300111025     C*
107400111025     C* se riferimento non escluso oppure
107500111025     C* riferimento escluso ma comunque da considerare nell'importo,
107600111025     C* allora incremento totali da comunicare
107700111025     C                   If        $RifEsc <> '1' Or
107800131001     C                             ($RifEsc = '1' And ComImp098 = '2')
107900131126C2231C* Se nota, splitto in base al segno
108000131126 "   C                   If        $Nota  = *on
108100131126 "   C                   If        $Segno = 1
108200131121 "   C                   Eval      $ImponibP= $ImponibP+ (IvaImponib * $Segno)
108300131121 "   C                   Eval      $ImpostaP= $ImpostaP+ (IvaImporto * $Segno)
108400131121 "   C                   Else
108500131121 "   C                   Eval      $ImponibN= $ImponibN+ (IvaImponib * $Segno)
108600131121 "   C                   Eval      $ImpostaN= $ImpostaN+ (IvaImporto * $Segno)
108700131126 "   C                   Endif
108800131126C2231C                   Else
108900131126     C                   Eval      $Imponib = $Imponib + (IvaImponib * $Segno)
109000131126     C                   Eval      $Imposta = $Imposta + (IvaImporto * $Segno)
109100131126C2231C                   Endif
109200111025     C                   EndIf
109300111025     C*
109400111025     C                   EndIf
109500111025     C                   EndIf
109600111025     C                   EndIf
109700111025     C*
109800111025     C                   ENDSR
109900131030      /EJECT
110000131030C2228C************************************************************
110100131030 "   C* Reperimento trattamenti speciali a livello di riga Iva
110200131030 "   C************************************************************
110300131030C2228C     RepTraIva     BEGSR
110400131030     C*
110500131030     C* verifico presenza trattamento speciale:
110600131030     C* "31" - Corrispettivo da includere
110700131030     C                   Eval      $Codice = '31'
110800131030     C                   ExSr      ChkTraIva
110900131030     C                   Eval      $TraSpe31 = $TraSpe
111000131030     C*
111100131030C2228C                   ENDSR
111200111025      /EJECT
111300131030C2228C************************************************************
111400131030 "   C* Verifica trattamento speciale a livello di riga Iva
111500131030 "   C************************************************************
111600131030C2228C     ChkTraIva     BEGSR
111700131030     C*
111800131030     C* per default non c'è trattamento speciale
111900131030     C                   Eval      $TraSpe = *Off
112000131030     C*
112100131030     C                   Eval      SmtSocieta = RegSocieta
112200131030     C                   Eval      SmtCodice = $Codice
112300131030     C                   Eval      SmtTpDato = '2'
112400131030     C     K03Smt01      Setll     NDSmt01L                               21
112500131030     C*
112600131030     C* se esiste almeno una combinazione per il trattamento cercato,
112700131030     C* verifico se tale combinazione riguarda il movimento in esame
112800131030     C                   If        *In21 = *On
112900131030     C*
113000131030     C* tentativo n.1: accesso per chiave completa
113100131030     C                   Eval      SmtSocieta = RegSocieta
113200131030     C                   Eval      SmtTpReg = $TpReg
113300131030     C                   Eval      SmtLibro = $Libro
113400131030     C                   Eval      SmtAliq = IvaAliq
113500131030     C                   Eval      SmtRifIva = IvaRifIva
113600131030     C                   Eval      SmtCausale = RegCausTes
113700131030     C                   Eval      SmtCodice = $Codice
113800131030     C     K07Smt02      Chain     NDSmt02L                           21
113900131030   b2C                   If        *In21 = *Off
114000131030     C                   Eval      $TraSpe = *On
114100131030   x2C                   Else
114200131030     C*
114300131030     C* tentativo n.2: accesso per chiave completa senza causale
114400131030     C                   Eval      SmtSocieta = RegSocieta
114500131030     C                   Eval      SmtTpReg = $TpReg
114600131030     C                   Eval      SmtLibro = $Libro
114700131030     C                   Eval      SmtAliq = IvaAliq
114800131030     C                   Eval      SmtRifIva = IvaRifIva
114900131030     C                   Eval      SmtCausale = *Blanks
115000131030     C                   Eval      SmtCodice = $Codice
115100131030     C     K07Smt02      Chain     NDSmt02L                           21
115200131030   b3C                   If        *In21 = *Off
115300131030     C                   Eval      $TraSpe = *On
115400131030   x3C                   Else
115500131030     C*
115600131030     C* tentativo n.2 bis: accesso per chiave completa senza Rif. Iva
115700131030     C                   Eval      SmtSocieta = RegSocieta
115800131030     C                   Eval      SmtTpReg = $TpReg
115900131030     C                   Eval      SmtLibro = $Libro
116000131030     C                   Eval      SmtAliq = 0
116100131030     C                   Eval      SmtRifIva = *Blanks
116200131030     C                   Eval      SmtCausale = RegCausTes
116300131030     C                   Eval      SmtCodice = $Codice
116400131030     C     K07Smt02      Chain     NDSmt02L                           21
116500131030     C                   If        *In21 = *Off
116600131030     C                   Eval      $TraSpe = *On
116700131030     C                   Else
116800131030     C*
116900131030     C* tentativo n.3: solo tipo registro e libro
117000131030     C                   Eval      SmtSocieta = RegSocieta
117100131030     C                   Eval      SmtTpReg = $TpReg
117200131030     C                   Eval      SmtLibro = $Libro
117300131030     C                   Eval      SmtAliq = 0
117400131030     C                   Eval      SmtRifIva = *Blanks
117500131030     C                   Eval      SmtCausale = *Blanks
117600131030     C                   Eval      SmtCodice = $Codice
117700131030     C     K07Smt02      Chain     NDSmt02L                           21
117800131030   b4C                   If        *In21 = *Off
117900131030     C                   Eval      $TraSpe = *On
118000131030   x4C                   Else
118100131030     C*
118200131030     C* tentativo n.4: accesso per aliquota / riferimento Iva
118300131030     C                   Eval      SmtSocieta = RegSocieta
118400131030     C                   Eval      SmtTpReg = *Blanks
118500131030     C                   Eval      SmtLibro = *Blanks
118600131030     C                   Eval      SmtAliq = IvaAliq
118700131030     C                   Eval      SmtRifIva = IvaRifIva
118800131030     C                   Eval      SmtCausale = *Blanks
118900131030     C                   Eval      SmtCodice = $Codice
119000131030     C     K07Smt02      Chain     NDSmt02L                           21
119100131030   b5C                   If        *In21 = *Off
119200131030     C                   Eval      $TraSpe = *On
119300131030   x5C                   Else
119400131030     C*
119500131030     C* tentativo n.5: solo causale
119600131030     C                   Eval      SmtSocieta = RegSocieta
119700131030     C                   Eval      SmtTpReg = *Blanks
119800131030     C                   Eval      SmtLibro = *Blanks
119900131030     C                   Eval      SmtAliq = 0
120000131030     C                   Eval      SmtRifIva = *Blanks
120100131030     C                   Eval      SmtCausale = RegCausTes
120200131030     C                   Eval      SmtCodice = $Codice
120300131030     C     K07Smt02      Chain     NDSmt02L                           21
120400131030   b6C                   If        *In21 = *Off
120500131030     C                   Eval      $TraSpe = *On
120600131030   e6C                   EndIf
120700131030     C*
120800131030   e5C                   EndIf
120900131030   e4C                   EndIf
121000131030     C                   EndIf
121100131030   e3C                   EndIf
121200131030   e2C                   EndIf
121300131030   e1C                   EndIf
121400131030     C*
121500131030C2228C                   ENDSR
121600131030      /EJECT
121700111025     C************************************************************
121800111025     C* Gestione del segno
121900111025     C************************************************************
122000111025     C     GesSegno      BEGSR
122100111025     C*
122200111025     C                   Select
122300111025     C*
122400111025     C* operazioni attive (clienti):
122500111025     C* se segno del movimento è Avere sommo, altrimenti detraggo
122600111025     C                   When      $TipOpe = '1'
122700111025     C                   If        $Avere = 1
122800111025     C                   Eval      $Segno = 1
122900111025     C                   Else
123000111025     C                   Eval      $Segno = -1
123100111025     C                   EndIf
123200111025     C*
123300111025     C* operazioni passive (fornitori):
123400111025     C* se segno del movimento è Dare sommo, altrimenti detraggo
123500111025     C                   When      $TipOpe = '2'
123600111025     C                   If        $Dare = 1
123700111025     C                   Eval      $Segno = 1
123800111025     C                   Else
123900111025     C                   Eval      $Segno = -1
124000111025     C                   EndIf
124100111025     C*
124200111025     C                   EndSl
124300111025     C*
124400111025     C                   ENDSR
124500111025      /EJECT
124600111031     C************************************************************
124700111031     C* Controllo esclusione registrazione in base a riferimenti Iva
124800111031     C************************************************************
124900111031     C     CtrEscReg     BEGSR
125000111031     C*
125100111031     C                   Select
125200111031     C*
125300111031     C* se non c'è almeno un riferimento non escluso, devo escludere
125400111031     C* la registrazione
125500111031     C                   When      $NoEAlmUno = *Off
125600111031     C* Tutte le righe di Iva hanno un riferimento escluso dallo
125700111031     C* Spesometro
125800111031     C                   Eval      $TipMsg = '2'
125900111031     C                   Eval      $IntMsg = '026'
126000111031     C                   Eval      $CodMsg = 'NDC1194'
126100111109     C                   ExSr      MemSms
126200111031     C*
126300111031     C* se c'è almeno un riferimento escluso, escludo l'intera
126400111031     C* registrazione se così richiesto in parametro modulo
126500111031     C                   When      $EscAlmUno = *On And
126600131001     C                              EscReg098 = '2'
126700111031     C* Alcune righe di Iva hanno un riferimento escluso dallo
126800111031     C* Spesometro
126900111031     C                   Eval      $TipMsg = '2'
127000111031     C                   Eval      $IntMsg = '027'
127100111031     C                   Eval      $CodMsg = 'NDC1195'
127200111109     C                   ExSr      MemSms
127300111031     C                   EndSl
127400111031     C*
127500111031     C                   ENDSR
127600111031      /EJECT
127700111024     C************************************************************
127800111024     C* Controlli finali sulla registrazione
127900111024     C************************************************************
128000111024     C     CtrFin        BEGSR
128100131002     C*
128200131002     C* impostazione del quadro
128300130930     C                   ExSr      SetQuadro
128400111026     C*
128500111026     C* impostazione del numero e data operazione
128600131008     C                   ExSr      SetDatOpe
128700111027     C*
128800111027     C* impostazione dati vari
128900111027     C                   ExSr      SetDatVar
129000111108     C*
129100111108     C* se riscontrato errore che impedisce un corretto calcolo
129200111108     C* degli importi, azzero i campi per impedire che venga
129300111108     C* memorizzato un valore non corretto
129400111108     C                   If        $ErrImp = *On
129500111108     C                   ExSr      PulImporti
129600111108     C                   EndIf
129700111109     C*
129800131016     C* pulizia dati non significativi in base al quadro, oppure
129900131016     C* se quadro non impostato, a caratteristiche del soggetto
130000111026     C                   ExSr      PulDatRec
130100111024     C*
130200111026     C* verifica dati obbligatori in base al tipo record
130300111026     C                   If        $Errore = *Off
130400131121C2231C* Totalizzo imponib+imposta per controlli
130500131126 "   C                   Eval      $Importo  = $Imponib  + $Imposta
130600131126 "   C                   Eval      $ImportoP = $ImponibP + $ImpostaP
130700131121C2231C                   Eval      $ImportoN = $ImponibN + $ImpostaN
130800131121     C*
130900111026     C                   ExSr      ChkDatRec
131000111026     C                   EndIf
131100111024     C*
131200111024     C                   ENDSR
131300111024      /EJECT
131400111027     C************************************************************
131500130930     C* Impostazione del quadro
131600111027     C************************************************************
131700130930     C     SetQuadro     BEGSR
131800111027     C*
131900131002     C* Note e fatture vengono estratte separatemente
132000131016     C* Residenti (stato del soggetto = IT)
132100131010     C*   Fatture vengono estratte nei quadri FE e FR
132200131010     C*   Note vengono estratte nei quadri NE e NR
132300131010     C*        con  gli importi segnati:
132400131031     C*     positivi    quando accrescono il valore della fattura
132500131031     C*     negativi    quando lo diminuiscono
132600131016     C* NON Residenti (stato del soggetto <> IT)
132700131016     C*   Fatture vengono estratte nei quadri FN(emesse) e SE(ricevute)
132800131031     C*   Note non devono essere estratte, non hanno quadro
132900131031     C*        e vengono escluse
133000131031     C*
133100131001     C                   Eval      $Quadro = *blank
133200131010     C*
133300111027     C                   Select
133400111027     C*
133500131002     C* quadro FE: fatture emesse
133600130930     C* operazioni con soggetti residenti
133700131001     C* tipo operazione Cliente='1'
133800131001     C                   When      $Nota = *Off
133900131001     C                             and $Residente = *On
134000131001     C                             and $TipOpe     = '1'
134100131002     C                   Eval      $Quadro = 'FE'
134200131001      *
134300131002     C* quadro FR: fatture ricevute
134400131001     C* operazioni con soggetti residenti
134500131001     C* tipo operazione Fornitore='2'
134600131001     C                   When      $Nota = *Off
134700131001     C                             and $Residente = *On
134800131001     C                             and $TipOpe     = '2'
134900131002     C                   Eval      $Quadro = 'FR'
135000111027     C*
135100131001     C* quadro FN: operazioni attive con sogg. non residenti
135200131003     C* tipo operazione Cliente='1'
135300131001     C                   When      $Nota = *Off
135400131001     C                             and $Residente = *Off
135500131001     C                             and $TipOpe     = '1'
135600130930     C                   Eval      $Quadro = 'FN'
135700131015     C*
135800131015     C* quadro SE: operazioni passive con sogg. non residenti
135900131015     C* tipo operazione Fornitore ='2'
136000131015     C                   When      $Nota = *Off
136100131015     C                             and $Residente = *Off
136200131015     C                             and $TipOpe     = '2'
136300131015     C                   Eval      $Quadro = 'SE'
136400111027     C*
136500131010     C* quadro NE note emesse
136600130930     C* note di variazione
136700131001     C* in base al tipo operazione Cliente='1'
136800131001     C                   When      $Nota = *On
136900131001     C                             and $Residente = *On
137000131001     C                             and $TipOpe     = '1'
137100131010     C                   Eval      $Quadro = 'NE'
137200131001     C*
137300131010     C* quadro NR    note ricevute
137400131001     C* note di variazione
137500131001     C* in base al tipo operazione Fornitore= '2'
137600131001     C                   When      $Nota = *On
137700131001     C                             and $Residente = *On
137800131002     C                             and $TipOpe    = '2'
137900131010     C                   Eval      $Quadro = 'NR'
138000131031     C*
138100131031C2228C* quadro ' '   note estere
138200131031 "   C                   When      $Nota = *On
138300131031 "   C                             and $Residente = *Off
138400131031 "   C* Nota residente estero, non viene trasmessa
138500131031 "   C                   Eval      $TipMsg = '2'
138600131031 "   C                   Eval      $IntMsg = '002'
138700131031 "   C                   Eval      $CodMsg = 'NDC1229'
138800131031 "   C                   ExSr      MemSms
138900131031C2228C*
139000111027     C                   EndSl
139100111027     C*
139200111027     C                   ENDSR
139300111027      /EJECT
139400131008     C************************************************************
139500131008     C* Impostazione dei dati dell'operazione
139600131008     C************************************************************
139700131008     C     SetDatOpe     BEGSR
139800131008     C*
139900131008     C                   Eval      $DtOpe = RegDtReg
140000131008     C                   Eval      $NrOpe = %Char(RegNrDoc)
140100131008     C*
140200131008     C*
140300131008     C                   ENDSR
140400131008      /EJECT
140500111027     C************************************************************
140600111027     C* Impostazione dati vari
140700111027     C************************************************************
140800111027     C     SetDatVar     BEGSR
140900111110     C*
141000111110     C* se soggetto non residente e persona fisica, devo reperire i
141100111110     C* dati del suo stato di nascita
141200111110     C                   If        $Residente = *Off And
141300131002     C                             $PFisica   = *On
141400131002     C                   Eval      WStato     = $StaNa
141500111110     C                   ExSr      RepTabG12
141600111110     C* se soggetto è nato all'estero...
141700120418     C                   If        NazG12 <> 'IT'
141800111110     C* ...devo valorizzare il campo del comune di nascita con la
141900111110     C* descrizione dello stato e la provincia di nascita con 'EE'
142000111110     C                   Eval      $ComNa = DesG12
142100111110     C                   Eval      $PrvNa = 'EE'
142200111110     C                   EndIf
142300111110     C                   EndIf
142400111027     C*
142500111027     C                   ENDSR
142600111027      /EJECT
142700111108     C************************************************************
142800111108     C* Pulizia importi
142900111108     C************************************************************
143000111108     C     PulImporti    BEGSR
143100111108     C*
143200131121     C                   Eval      $Imponib  = 0
143300131121     C                   Eval      $Imposta  = 0
143400131121C2231C                   Eval      $ImponibP = 0
143500131121 "   C                   Eval      $ImpostaP = 0
143600131121 "   C                   Eval      $ImponibN = 0
143700131121C2231C                   Eval      $ImpostaN = 0
143800111108     C*
143900111108     C                   ENDSR
144000111108      /EJECT
144100111026     C************************************************************
144200111026     C* Eliminazione campi da non valorizzare per il tipo record
144300111026     C************************************************************
144400111026     C     PulDatRec     BEGSR
144500111028     C*
144600111026     C                   Select
144700111026     C*
144800131002     C* Quadro = FE
144900131002     C                   When      $Quadro = 'FE'
145000131019      * Piva, cdfisc e doc riep sono in alternativa
145100131019      * Preval doc riep, visto che è forzato da tabella CP3
145200131019     C                   If        $DocRie  = *on
145300131019     C                   Eval      $PartIva = *Blanks
145400131019     C                   Eval      $CdFisc  = *Blanks
145500131002     C                   EndIf
145600131019     C                   If        $PartIva <> *Blanks
145700131019     C                   Eval      $CdFisc  = *Blanks
145800131019     C                   EndIf
145900131019      * Un doc riepilogativo non può essere autofattura
146000131016     C                   If        $DocRie = *on
146100131016     C                   Eval      $AutoFat= *off
146200131016     C                   EndIf
146300111026     C*
146400131002     C* Quadro = FR
146500131002     C                   When      $Quadro = 'FR'
146600131002     C                   Eval      $CdFisc = *Blanks
146700131019      * Piva e doc riep sono in alternativa
146800131019      * Preval doc riep, visto che è forzato da tabella CP3
146900131019     C                   If        $DocRie  = *on
147000131019     C                   Eval      $PartIva = *Blanks
147100131019     C                   EndIf
147200131107C2230x**Un doc riepilogativo o in reverse charge non può essere autofattura
147300131107  "  x**                 If        $RevChar= *on
147400131107  "  x**                           or $DocRie = *on
147500131107  "  x**                 Eval      $AutoFat= *off
147600131107  "  x**                 EndIf
147700131107  "   * Se autofattura non può essere reverse charge
147800131107  "  C                   If        $AutoFat= *on
147900131107  "  C                   Eval      $RevChar= *off
148000131107C2230C                   EndIf
148100111026     C*
148200131002     C* Quadro = NE
148300131002     C                   When      $Quadro = 'NE'
148400131002     C                   If        $PartIva <> *Blanks
148500131002     C                   Eval      $CdFisc = *Blanks
148600131002     C                   EndIf
148700131002     C*
148800131002     C* Quadro = NR
148900131002     C                   When      $Quadro = 'NR'
149000111026     C                   Eval      $CdFisc = *Blanks
149100111026     C*
149200131019     C* Quadro = FN o SE
149300131002     C                   When      $Quadro = 'FN'
149400131016     C                             or $Quadro = 'SE'
149500111026     C*
149600131002     C                   Eval      $CdFisc  = *Blanks
149700131002     C                   Eval      $PartIva =  *Blanks
149800131016     C                   Eval      $DocRie = *off
149900131016     C                   Eval      $RevChar= *off
150000131016     C                   Eval      $AutoFat= *off
150100131003     C* dati per non residenti se persona giuridica
150200131003     C                   Select
150300131003     C                   When      $PFisica = *Off
150400131003     C                   Clear                   $Cognome
150500131003     C                   Clear                   $Nome
150600131003     C                   Clear                   $DtNasc
150700131003     C                   Clear                   $ComNa
150800131003     C                   Clear                   $PrvNa
150900131003     C                   Clear                   $StaEsD
151000131003     C* dati per non residenti se persona fisica
151100131003     C                   When      $PFisica = *On
151200131003     C                   Clear                   $RagSoc
151300131003     C                   Clear                   $LocEst
151400131003     C                   Clear                   $StaEsS
151500131003     C                   Clear                   $IndEst
151600131003     C                   EndSl
151700111026     C*
151800111026     C                   EndSl
151900111026     C*
152000131016     C* Per tutti ad eccezione di FN e SE
152100131002     C* o comunque se quadro non impostato e soggetto residente
152200131016     C                   If        ($Quadro <>'FN' and $Quadro <>'SE')
152300131016     C                             or  ($Quadro = ' ' And $Residente = *On)
152400111026     C* dati per non residenti
152500111026     C                   Clear                   $Cognome
152600111026     C                   Clear                   $Nome
152700111026     C                   Clear                   $DtNasc
152800111026     C                   Clear                   $ComNa
152900111026     C                   Clear                   $PrvNa
153000111026     C                   Clear                   $StaEsD
153100111026     C                   Clear                   $RagSoc
153200111026     C                   Clear                   $LocEst
153300111026     C                   Clear                   $StaEsS
153400111026     C                   Clear                   $IndEst
153500111026     C                   EndIf
153600111026     C*
153700111026     C*
153800111026     C                   ENDSR
153900111026      /EJECT
154000111026     C************************************************************
154100111026     C* Verifica dati obbligatori in base al tipo record
154200111026     C************************************************************
154300111026     C     ChkDatRec     BEGSR
154400131121      *
154500111108     C* se manca un dato obbligatorio tra quelli previsti per il
154600111108     C* tipo record, si tratta di un errore
154700111026     C*
154800111026     C                   Select
154900111026     C*
155000131002     C* Quadro = FE
155100131003     C                   When      $Quadro = 'FE'
155200131016     C                   If        ($CdFisc=*Blanks and $PartIva=*blank
155300131016     C                              and $docrie =*off  )
155400131003     C                             or (RegDtreg = *Loval and RegDtdoc=*loval)
155500131003     C                             or RegNrDoc  = 0
155600131126C2231C                             or $Importo  = 0
155700131121C2231x********                     or ($Imponib = 0 and $Imposta = 0 )
155800111108     C                   Eval      $ErrInc = *On
155900111026     C                   EndIf
156000111026     C*
156100131002     C* Quadro = FR
156200131003     C                   When      $Quadro  = 'FR'
156300131016     C                   If        ($PartIva = *blank and $docrie =*off  )
156400131016     C                             or ($PartIva<>*blank and RegDtReg=*Loval)
156500131016     C                             or ($DocRie = *on and RegDtReg=*Loval
156600131016     C                                 and RegDtdoc=*loval   )
156700131126C2231C                             or $Importo  = 0
156800131121C2231x********                     or ($Imponib = 0 and $Imposta = 0 )
156900111108     C                   Eval      $ErrInc = *On
157000111026     C                   EndIf
157100111026     C*
157200131002     C* Quadro = 'NE'
157300131002     C                   When      $Quadro = 'NE'
157400131003     C                   If        ($CdFisc=*Blanks and $PartIva=*blank)
157500131003     C                             or (RegDtreg = *Loval and RegDtdoc=*loval)
157600131003     C                             or RegNrDoc = 0
157700131121C2231C                             or ($ImportoP = 0 and $ImportoN = 0)
157800131121C2231x********                     or ($Imponib = 0 and $Imposta = 0 )
157900111108     C                   Eval      $ErrInc = *On
158000111026     C                   EndIf
158100111026     C*
158200131002     C* Quadro = 'NR'
158300131002     C                   When      $Quadro = 'NR'
158400131003     C                   If        $PartIva=*blank
158500131003     C                             or RegDtReg  = *Loval
158600131121C2231C                             or ($ImportoP = 0 and $ImportoN = 0)
158700131121C2231x********                     or ($Imponib = 0 and $Imposta = 0 )
158800111108     C                   Eval      $ErrInc = *On
158900111026     C                   EndIf
159000111026     C*
159100131002     C* Quadro = 'FN'
159200131002     C                   When      $Quadro = 'FN'
159300131016     C                             or $Quadro = 'SE'
159400131019     C                   If        ($Quadro = 'FN' and RegDtreg = *Loval
159500131019     C                               and RegDtdoc=*loval)
159600131019     C                             or ($Quadro='SE' and RegDtReg=*loval)
159700131003     C                             or ($Cognome = *Blanks And $RagSoc = *Blanks)
159800131126C2231C                             or $Importo = 0
159900131121C2231x********                     or ($Imponib = 0 and $Imposta = 0 )
160000111108     C                   Eval      $ErrInc = *On
160100111026     C                   EndIf
160200131003     C* se valorizzato cognome, altri campi vanno valorizzati
160300131003     C                   Select
160400131003     C                   When      $Cognome <> *Blanks
160500131003     C                   If        $Nome = *Blanks Or
160600131003     C                             $DtNasc = *Loval Or
160700131003     C                             $ComNa = *Blanks Or
160800131003     C                             $PrvNa = *Blanks Or
160900131003     C                             $StaEsD = 0
161000131003     C                   Eval      $ErrInc = *On
161100131003     C                   EndIf
161200131003     C* se valorizzata ragione sociale, altri campi vanno valorizzati
161300131003     C                   When      $RagSoc <> *Blanks
161400131003     C                   If        $StaEsS = 0
161500131003     C                   Eval      $ErrInc = *On
161600131003     C                   EndIf
161700131003     C                   Endsl
161800111026     C*
161900111026     C                   EndSl
162000111027     C*
162100111026     C*
162200111026     C* se alla fine dei controlli è emerso un errore, segnalo che
162300111026     C* i dati del record sono incompleti
162400111108     C                   If        $ErrInc = *On
162500111026     C* Dati incompleti
162600111026     C                   Eval      $TipMsg = '1'
162700111108     C                   Eval      $IntMsg = '030'
162800111026     C                   Eval      $CodMsg = 'PRO0371'
162900111109     C                   ExSr      MemSms
163000111026     C                   EndIf
163100111026     C*
163200111026     C                   ENDSR
163300111026      /EJECT
163400111111     C************************************************************
163500111111     C* Controllo valorizzazione tipo record
163600111111     C************************************************************
163700131002     C     CtrQuadro     BEGSR
163800111111     C*
163900111111     C* eseguo controllo di sicurezza per assicurarmi che il record,
164000131031     C* esca in errore se non ha quadro
164100111111     C*
164200111111     C                   If        $Errore = *Off And
164300131002     C                              $Quadro = ' '
164400111111     C                   Eval      $Errore = *On
164500131016     C* non è stato possibile determinare il quadro
164600111111     C                   Eval      $TipMsg = '1'
164700111111     C                   Eval      $IntMsg = '029'
164800131002     C                   Eval      $CodMsg = 'NDC1226'
164900131031C2228C                   ExSr      MemSms
165000131031C2228x*******            ExSr      GesSms
165100111111     C                   EndIf
165200111111     C*
165300111111     C                   ENDSR
165400111111      /EJECT
165500111024     C************************************************************
165600111025     C* Reperisco i dati di tabella CP1
165700111024     C************************************************************
165800111024     C     RepTabCP1     BEGSR
165900111024     C*
166000111024     C                   Clear                   ANGCP1DS
166100111024     C*
166200111024     C                   Clear                   XATBDS
166300111024     C                   Move      '1'           XTBRIC
166400130925     C                   Move      SocietaP87    XTBAZI
166500111024     C                   Move      *ZERO         XTBKEY
166600111024     C                   Move      'CP1'         XTBCOD
166700111024     C                   Move      RegCausTes    XTBKEY
166800111024     C                   Callb     'XATB'
166900111024     C                   Parm                    XATBDS
167000111024     C*
167100111024     C                   If        XTbErr = '0'
167200111024     C                   Movel     XTBUNI        AngCP1DS
167300111024     C                   Else
167400111024     C                   Clear                   ANGCP1DS
167500111024     C                   EndIf
167600111024     C*
167700111024     C                   ENDSR
167800111024     C/EJECT
167900111110     C************************************************************
168000131016     C* Reperisco i dati di tabella CP3
168100111110     C************************************************************
168200131016     C     RepTabCP3     BEGSR
168300111110     C*
168400131016     C                   Clear                   ANGCP3DS
168500111110     C*
168600111110     C                   Clear                   XATBDS
168700111110     C                   Move      '1'           XTBRIC
168800130925     C                   Move      SocietaP87    XTBAZI
168900111110     C                   Move      *ZERO         XTBKEY
169000131016     C                   Move      'CP3'         XTBCOD
169100131016     C                   Move      RegCausTes    XTBKEY
169200111110     C                   Callb     'XATB'
169300111110     C                   Parm                    XATBDS
169400111110     C*
169500111110     C                   If        XTbErr = '0'
169600131016     C                   Movel     XTBUNI        AngCP3DS
169700131018      * Se la causale è definita di autofattura ok
169800131018      * altrimenti comanda la tabella CP3
169900131018     C                   If        $Autofat = *off
170000131018     C                   Eval      $Autofat = AUTOFCP3
170100131018     C                   EndIf
170200131016      * Se documento riepilogativo  DocRieCp3=*on
170300131016     C                   Eval      $DocRie  = DOCRIECP3
170400111110     C                   EndIf
170500111110     C*
170600111110     C                   ENDSR
170700111110     C/EJECT
170800111024     C************************************************************
170900111024     C* Reperimento movimento cliente / fornitore
171000111024     C************************************************************
171100111024     C     RepMovCF      BEGSR
171200111024     C*
171300111024     C* per le registrazioni IVA, la riga cliente / fornitore si
171400111024     C* trova OBBLIGATORIAMENTE sulla riga con MOVNRRIGAM = 1
171500111024     C* NB: la registrazione è sicuramente IVA dato che esiste un
171600111024     C*     record di NDIVA
171700111024     C                   Eval      MovSys = RegSys
171800111024     C                   Eval      MovNrAsReg = RegNrAsReg
171900111024     C                   Eval      MovNrRigaM = 1
172000111024     C     K03Mov01      Chain     NDMOV01L                           21
172100111024     C*
172200111024     C                   If        *In21 = *On
172300111024     C*
172400111024     C* Non trovato movimento Cliente / Fornitore della registrazione
172500111024     C                   Eval      $TipMsg = '1'
172600111108     C                   Eval      $IntMsg = '001'
172700111024     C                   Eval      $CodMsg = 'MVC0049'
172800111109     C                   ExSr      MemSms
172900111108     C*
173000111108     C* questo errore pregiudica il calcolo degli importi
173100111108     C                   Eval      $ErrImp = *On
173200111024     C*
173300111024     C                   Else
173400111024     C*
173500111024     C* CRITERIO DI SELEZIONE:
173600111024     C* se nel lancio era richiesta una selezione per conto,
173700111024     C* scarto la registrazione qualora il criterio non sia rispettato
173800130925     C                   If        (KcCP87 <> *Blanks And
173900130925     C                              KcCP87 <> MovKcc) Or
174000130925     C                             (KsCP87 <> *Blanks And
174100130925     C                              KsCP87 <> MovKsc)
174200111024     C                   ExSr      ScartaReg
174300111024     C                   Else
174400111024     C*
174500111024     C* memorizzo gli stati Dare / Avere
174600111024     C                   Eval      $CFDare = MovDare
174700111024     C                   Eval      $CFAvere = MovAvere
174800111024     C* memorizzo il conto
174900111024     C                   Eval      $CFKcc = MovKcc
175000111024     C                   Eval      $CFKsc = MovKsc
175100111024     C* memorizzo l'importo
175200111024     C                   Eval      $CFImporto = MovImporto
175300111024     C*
175400111024     C                   EndIf
175500111024     C*
175600111024     C                   EndIf
175700111024     C*
175800111024     C                   ENDSR
175900111024     C/EJECT
176000111024     C************************************************************
176100111024     C* Reperimento partita del movimento principale
176200111024     C************************************************************
176300111024     C     RepPartita    BEGSR
176400111024     C*
176500111024     C* inizializzo il flag che mi dice che sono stati reperiti i
176600111024     C* dati di partita
176700111024     C                   Eval      $Partita = *Off
176800111024     C*
176900111024     C* metto in linea il record di partita del movimento C/F
177000111024     C                   Eval      PasSys = MovSys
177100111024     C                   Eval      PasNrAsReg = MovNrAsReg
177200111024     C                   Eval      PasNrRigaM = MovNrRigaM
177300111024     C     K03Pas01      Chain     NdPas01L                           21
177400111024     C                   If        *In21 = *Off
177500111024     C*
177600111024     C* metto in linea il record di particolarità di partita
177700111024     C                   Eval      PPaSocieta = PasSocieta
177800111024     C                   Eval      PPaCtb = PasCtb
177900111024     C                   Eval      PPaKcc = PasKcc
178000111024     C                   Eval      PPaKsc = PasKsc
178100111024     C                   Eval      PPaDtPar = PasDtPar
178200111024     C                   Eval      PPaNrPar = PasNrPar
178300111024     C                   Eval      PPaSerPar = PasSerPar
178400111024     C     K07PPa01      Chain     NdPPa01L                           21
178500111024     C                   If        *In21 = *Off
178600111024     C                   Eval      $Partita = *On
178700111024     C                   EndIf
178800111024     C*
178900111024     C                   EndIf
179000111024     C*
179100111024     C                   ENDSR
179200111024     C/EJECT
179300111024     C************************************************************
179400111024     C* Reperimento dati del conto Cliente / Fornitore
179500111024     C************************************************************
179600111024     C     RepContoCF    BEGSR
179700111024     C*
179800111024     C* reperisco i dati del conto del cliente / fornitore
179900111024     C                   Eval      WKcc = $CFKcc
180000111024     C                   Eval      WKsc = $CFKsc
180100111024     C                   ExSr      CallMvc002
180200111024     C*
180300111024     C                   If        Esito = *On
180400111024     C*
180500111024     C* Conto inesistente
180600111024     C                   Eval      $TipMsg = '1'
180700111024     C                   Eval      $IntMsg = '004'
180800111024     C                   Eval      $CodMsg = 'PRO0016'
180900111109     C                   ExSr      MemSms
181000111108     C*
181100111108     C* questo errore pregiudica il calcolo degli importi
181200111108     C                   Eval      $ErrImp = *On
181300111024     C*
181400111024     C                   Else
181500111024     C*
181600111024     C* CRITERIO DI SELEZIONE:
181700111024     C* se nel lancio era richiesta una selezione per natura o sogg.,
181800111024     C* scarto la registrazione qualora il criterio non sia rispettato
181900130925     C                   If        (CliForP87 <> *Blanks And
182000130925     C                              CliForP87 <> SNatura002) Or
182100130925     C                             (SoggP87 <> *Blanks And
182200130925     C                              SoggP87 <> Sogg002)
182300111024     C                   ExSr      ScartaReg
182400111024     C                   Else
182500111024     C*
182600111024     C* memorizzo il codice soggetto
182700111024     C                   Eval      $CFSogg = Sogg002
182800111024     C*
182900111024     C* Conto escluso
183000130930     C                   If        AlleIva002  = '01'
183100130930     C                   Eval      $TipMsg     = '2'
183200130930     C                   Eval      $IntMsg     = '005'
183300130930     C                   Eval      $CodMsg     = 'NDC1186'
183400111109     C                   ExSr      MemSms
183500111024     C                   EndIf
183600111024     C*
183700111024     C* verifica della sottonatura
183800111024     C                   Select
183900130930     C                   When      SNatura002  = 'C'
184000130930     C                   Eval      $CFCliFor   = 'C'
184100130930     C                   Eval      $TipOpe     = '1'
184200130930     C                   When      SNatura002  = 'F'
184300130930     C                   Eval      $CFCliFor   = 'F'
184400130930     C                   Eval      $TipOpe     = '2'
184500111024     C                   Other
184600111024     C* Sottonatura del conto deve essere 'C' o 'F'
184700130930     C                   Eval      $TipMsg     = '1'
184800130930     C                   Eval      $IntMsg     = '006'
184900130930     C                   Eval      $CodMsg     = 'NDC0023'
185000111109     C                   ExSr      MemSms
185100111108     C* questo errore pregiudica il calcolo degli importi
185200130930     C                   Eval      $ErrImp     = *On
185300111024     C                   EndSl
185400111026     C*
185500111026     C* in caso di nota di variazione, adesso che è stata reperita
185600111026     C* la natura del conto, sono in grado di impostare i flag
185700111026     C* di accredito / addebito in base al segno del movimento C/F
185800131003     C****               If        $Nota = *On
185900131003     C****               Select
186000131003     C****               When      (SNatura002 = 'C' And $CFDare = 1) Or
186100131003     C****                         (SNatura002 = 'F' And $CFDare = 0)
186200131003     C****               Eval      $VarImb = 'D'
186300131003     C****               Eval      $VarIva = 'D'
186400131003     C****               When      (SNatura002 = 'C' And $CFDare = 0) Or
186500131003     C****                         (SNatura002 = 'F' And $CFDare = 1)
186600131003     C****               Eval      $VarImb = 'C'
186700131003     C****               Eval      $VarIva = 'C'
186800131003     C****               EndSl
186900131003     C****               EndIf
187000111024     C                   EndIf
187100111024     C                   EndIf
187200111024     C*
187300111024     C                   ENDSR
187400111024     C/EJECT
187500111024     C************************************************************
187600111024     C* Reperimento dati del soggetto anagrafico
187700111024     C************************************************************
187800111024     C     RepSog        BEGSR
187900111024     C*
188000111024     C* inizializzo tutte le ds del soggetto
188100111024     C                   Clear                   DVASOG
188200111024     C                   Clear                   ANSO1
188300111024     C                   Clear                   ANCPI
188400111024     C                   Clear                   NDCPA
188500111024     C*
188600111024     C* se il soggetto è estemporaneo devo reperire i dati dalla
188700111024     C* estensione di partita NDCPA00F
188800111024     C                   Eval      $TipSog = '0'
188900111024     C                   If        Tecnico002 = '2'
189000111024     C                   ExSr      RepSogEst
189100111024     C                   Else
189200111024     C                   ExSr      RepSogAna
189300111024     C                   EndIf
189400111024     C*
189500111024     C* terminato il reperimento, memorizzo le variabili relativi
189600111024     C* ai dati del soggetto, che serviranno per i controlli
189700131003     C* successivi e per il caricamento su NDSda
189800111024     C                   Select
189900111024     C*
190000111024     C* dati reperiti da NDCPA00F (soggetto estemporaneo)
190100111024     C                   When      $TipSog = '1'
190200111024     C                   Eval      $Stato = CPaStato
190300111110     C                   Eval      $StaNa = CPaStaNa
190400111026     C                   Eval      $NatGiur = *Blanks
190500111024     C                   Eval      $RagSoc  = CPaDescr
190600111024     C                   Eval      $Cognome = CPaCognome
190700111024     C                   Eval      $Nome    = CPaNome
190800111024     C                   Eval      $DtNasc  = CPaDtNa
190900111024     C                   Eval      $ComNa   = CPaComNa
191000111024     C                   Eval      $PrvNa   = CPaPrvNa
191100111024     C                   Eval      $LocEst  = CPaLocalit
191200111024     C                   Eval      $IndEst  = CPaIndiriz
191300111024     C                   Eval      $PartIva = CPaPartIva
191400111024     C                   Eval      $CdFisc  = CPaCdFisc
191500111024     C*
191600111024     D* dati reperiti da ANCPI00F (variazione fiscale)
191700111024     D* NB: i dati che non esistono su ANCPI, vengono prelevati da
191800111024     D*     DVASOG (DVSSOG e ANSO1)
191900111024     C                   When      $TipSog = '2'
192000111024     C                   Eval      $Stato = CpiStato
192100111110     C                   Eval      $StaNa = So1StaNa
192200111026     C                   Eval      $NatGiur = CpiNatGiur
192300111024     C                   Eval      $RagSoc  = CpiDesSog1
192400111024     C                   Eval      $Cognome = So1Cognome
192500111024     C                   Eval      $Nome    = So1Nome
192600111024     C                   Eval      $DtNasc  = DvsDtNasc
192700111024     C                   Eval      $ComNa   = So1ComNa
192800111024     C                   Eval      $PrvNa   = So1PrvNa
192900111024     C                   Eval      $LocEst  = CpiLocalit
193000111024     C                   Eval      $IndEst  = CpiIndriz
193100111024     C                   Eval      $PartIva = CpiPartIva
193200111024     C                   Eval      $CdFisc  = CpiCdFisc
193300111024     C*
193400111024     C* dati reperiti da DVASOG (dati anagrafici)
193500111024     C                   When      $TipSog = '3'
193600111024     C                   Eval      $Stato = DvsStato
193700111110     C                   Eval      $StaNa = So1StaNa
193800111026     C                   Eval      $NatGiur = DvsNatGiur
193900111024     C                   Eval      $RagSoc  = DvsDes
194000111024     C                   Eval      $Cognome = So1Cognome
194100111024     C                   Eval      $Nome    = So1Nome
194200111024     C                   Eval      $DtNasc  = DvsDtNasc
194300111024     C                   Eval      $ComNa   = So1ComNa
194400111024     C                   Eval      $PrvNa   = So1PrvNa
194500111024     C                   Eval      $LocEst  = DvsLocalit
194600111024     C                   Eval      $IndEst  = DvsIndriz
194700111024     C                   Eval      $PartIva = DvsPartIva
194800111024     C                   Eval      $CdFisc  = DvsCdFisc
194900111024     C*
195000111024     C                   EndSl
195100111024     C*
195200111024     C                   ENDSR
195300111024     C/EJECT
195400111024     C************************************************************
195500111024     C* Reperimento dati del soggetto estemporaneo
195600111024     C************************************************************
195700111024     C     RepSogEst     BEGSR
195800111024     C*
195900111024     C* in caso di soggetto estemporaneo, i dati si trovano sulla
196000111024     C* estensione di partita: se quindi la partita non è stata
196100111026     C* trovata, si tratta di un errore
196200111024     C                   If        $Partita = *Off
196300111024     C* Partita non esistente
196400111024     C                   Eval      $TipMsg = '1'
196500111024     C                   Eval      $IntMsg = '007'
196600111024     C                   Eval      $CodMsg = 'NDC1009'
196700111109     C                   ExSr      MemSms
196800111024     C                   Else
196900111024     C*
197000111024     C* metto in linea il record di NDCPA00F con i dati
197100111024     C                   Eval      CpaSys = PPaSys
197200111024     C                   Eval      CpaNrAsInt = PPaNrAsInt
197300111024     C     K02Cpa01      Chain     NdCpa01L                           21
197400111024     C                   If        *In21 = *On
197500111024     C* Estensione nominativi partite inesistente
197600111024     C                   Eval      $TipMsg = '1'
197700111024     C                   Eval      $IntMsg = '007'
197800111024     C                   Eval      $CodMsg = 'NDC1009'
197900111109     C                   ExSr      MemSms
198000111024     C                   Else
198100111024     C* segnalo che sto usando i dati del soggetto estemporaneo
198200111024     C                   Eval      $TipSog = '1'
198300111024     C                   EndIf
198400111024     C*
198500111024     C                   EndIf
198600111024     C*
198700111024     C                   ENDSR
198800111024     C/EJECT
198900111024     C************************************************************
199000111024     C* Reperimento dati del soggetto anagrafico
199100111024     C************************************************************
199200111024     C     RepSogAna     BEGSR
199300111024     C*
199400111024     C* altrimenti i dati sono quelli anagrafici: prima provo a
199500111024     C* reperire una eventuale variazione fiscale
199600111024     C                   ExSr      CallDvaCpi
199700111024     C                   If        DVCErrore = *Off
199800111024     C                   Eval      %SubSt(ANCPI:1:DVCLenOut)
199900111024     C                              = %SubSt(DVCOutput:1:DVCLenOut)
200000111024     C* trovato: segnalo che uso i dati delle variazioni fiscali
200100111024     C                   Eval      $TipSog = '2'
200200111024     C                   Else
200300111024     C* non trovato: segnalo che uso i normali dati anagrafici
200400111024     C                   Eval      $TipSog = '3'
200500111024     C                   EndIf
200600111024     C*
200700111024     C* anche se userò i dati delle variazioni fiscali, devo prelevare
200800111024     C* anche i dati di DVASOG e di ANSO1 perchè solo lì si trovano
200900111024     C* alcuni dati (che non sono presenti sulle variazioni fiscali)
201000111024     C*
201100111024     C* dati di DVASOG (obbligatori)
201200111024     C                   Eval      DvaStrutt = 'DVASOG    '
201300111024     C                   Eval      DvaLenOut = %Size(DVASOG)
201400111024     C                   ExSr      CallDvaSog
201500111024     C                   If        DvaErrore   = *Off
201600111024     C                   Eval      %SubSt(DVASOG:1:DVALENOUT)
201700111024     C                              = %SubSt(DVAOUTPUT:1:DVALENOUT)
201800111024     C                   Else
201900111024     C* Codice soggetto inesistente
202000111024     C                   Eval      $TipMsg = '1'
202100111024     C                   Eval      $IntMsg = '008'
202200111024     C                   Eval      $CodMsg = 'PRO0053'
202300111109     C                   ExSr      MemSms
202400111024     C                   EndIf
202500111024     C*
202600111024     C* dati di ANSO1 (non necessariamente obbligatori)
202700111024     C                   Eval      DvaStrutt = 'ANSO1     '
202800111024     C                   Eval      DvaLenOut = %Size(ANSO1)
202900111024     C                   ExSr      CallDvaSog
203000111024     C                   If        DvaErrore   = *Off
203100111024     C                   Eval      %SubSt(ANSO1:1:DVALENOUT)
203200111024     C                              = %SubSt(DVAOUTPUT:1:DVALENOUT)
203300111024     C                   EndIf
203400111024     C*
203500111024     C                   ENDSR
203600111024     C/EJECT
203700111024     C************************************************************
203800111024     C* DATI ANAGRAFICI VARIAZIONE ESTREMI FISCALI
203900111024     C************************************************************
204000111024     C     CallDvaCpi    BEGSR
204100111024     C*
204200111024     C                   Movel     '2'           DVCRic
204300130925     C                   Movel     SocietaP87    DVCSocieta
204400111024     C                   Movel     *Blank        DVCUnita
204500111024     C                   Movel     'ANCPI'       DVCStrutt
204600111024     C                   Movel     RegDtReg      DVCDtRif
204700111024     C                   Movel     $CFSogg       DVCSogg
204800111024     C*
204900111024     C                   Eval      DVCLenOut = %Size(ANCPI)
205000111024     C*
205100111024     C                   CallB     'NDDVACPI'
205200111024     C                   Parm                    DVCRic
205300111024     C                   Parm                    DVCSocieta
205400111024     C                   Parm                    DVCUnita
205500111024     C                   Parm                    DVCStrutt
205600111024     C                   Parm                    DVCDtRif
205700111024     C                   Parm                    DVCLenOut
205800111024     C                   Parm                    DVCSogg
205900111024     C                   Parm      *Off          DVCErrore
206000111024     C                   Parm      *Blanks       DVCOutPut
206100111024     C*
206200111024     C                   ENDSR
206300111024     C/EJECT
206400111024     C******************************************************
206500111024     C* Reperisco i dati del soggetto
206600111024     C******************************************************
206700111024     C     CallDvaSog    BEGSR                                                  *
206800111024     C*
206900111024     C                   Movel     '1'           DvaRic
207000130925     C                   Movel     SocietaP87    DvaSocieta
207100111024     C                   Movel     *Blanks       DvaUnita
207200111024     C                   Movel     RegDtReg      DvaDtRif
207300111024     C                   Movel     $CFSogg       DvaSogg
207400111024     C                   Movel     *Blanks       DvaTpInd
207500111024     C                   Movel     *Blanks       DvaCdInd
207600111024     C*
207700111024     C                   CALLB     'NDDVASOG'
207800111024     C                   Parm                    DvaRic
207900111024     C                   Parm                    DvaSocieta
208000111024     C                   Parm                    DvaUnita
208100111024     C                   Parm                    DvaStrutt
208200111024     C                   Parm                    DvaDtRif
208300111024     C                   Parm                    DvaLenOut
208400111024     C                   Parm                    *Omit
208500111024     C                   Parm                    *Omit
208600111024     C                   Parm                    *Omit
208700111024     C                   Parm                    *Omit
208800111024     C                   Parm                    DvaSogg
208900111024     C                   Parm      *Blanks       DvaTpInd
209000111024     C                   Parm      *Blanks       DvaCdInd
209100111024     C                   Parm      *Off          DvaErrore
209200111024     C                   Parm      *Blanks       DvaOutput
209300111024     C*
209400111024     C                   ENDSR
209500111024     C/EJECT
209600111024     C************************************************************
209700111024     C* Reperisco i dati relativi allo stato del soggetto
209800111024     C************************************************************
209900111024     C     RepStato      BEGSR
210000111024     C*
210100111024     C* reperisco i dati di tabella G12 (Stati)
210200111110     C                   Eval      WStato = $Stato
210300111024     C                   ExSr      RepTabG12
210400131218     C*
210500131218C2233C* Estrazione San Marino:
210600131218 "   C* solo soggetti con codice stato = 37
210700131218 "   C                   If        TpElaP87 = '3'  And
210800140304 "   C*****                        CodUicG12 <> 0 and CodUicG12 <> 37
210900140304 "   C                             CodUicG12 <> 37
211000131218 "   C                   ExSr      ScartaReg
211100131218 "   C                   LeaveSR
211200131218C2233C                   Endif
211300111024     C*
211400111024     C* memorizzo i codici UIC dello stato nei campi che lo richiedono
211500111026     C                   If        CodUicG12 <> 0
211600131003     C                   Eval      $StaEsS = CodUicG12
211700131003     C                   Eval      $StaEsD = CodUicG12
211800111026     C                   EndIf
211900111024     C*
212000111024     C* verifico se stato è italiano, usando il codice CEE
212100111024     C                   If        NazG12 = 'IT'
212200111024     C                   Eval      $Residente = *On
212300111024     C                   Else
212400111024     C                   Eval      $Residente = *Off
212500111024     C                   EndIf
212600111024     C*
212700111024     C* se soggetto residente, verifico se il soggetto è privo di
212800111024     C* un identificativo fiscale
212900111024     C                   If        $Residente = *On
213000131104C2228x***                If        $CdFisc = *Blanks And
213100131104 "   x***                          $PartIva = *Blanks
213200131104 "   x**Soggetto residente privo di codice fiscale e partita Iva
213300131104 "   x***                Eval      $TipMsg = '1'
213400131104 "   x***                Eval      $IntMsg = '010'
213500131104 "   x***                Eval      $CodMsg = 'NDC1187'
213600131104 "   x***                ExSr      MemSms
213700131104C2228x***                EndIf
213800131016     C*
213900131016     C* se soggetto residente, in caso di autofattura
214000131107C2230C* ed è una vendita
214100131016     C* cdfisc o partita iva devono essere = alla società
214200131016     C                   If        $AutoFat = *on
214300131107C2230C                             and Snatura002 = 'C'
214400131016     C                   If        $CdFisc<>XSCCDF and $CdFisc<>*blank
214500131016     C                             and XSCCDF<>*blank
214600131016     C* Autofattura su soggetto con identificativi non coincidenti a quelli
214700131016     C* di società
214800131016     C                   Eval      $TipMsg = '1'
214900131016     C                   Eval      $IntMsg = '010'
215000131016     C                   Eval      $CodMsg = 'NDC1228'
215100131016     C                   ExSr      MemSms
215200131016     C                   EndIf
215300131016     C                   If        $PartIva<>XSCIva and $PartIva<>*blank
215400131016     C                             and XSCIva<>*blank
215500131016     C* Autofattura su soggetto con identificativi non coincidenti a quelli
215600131016     C* di società
215700131016     C                   Eval      $TipMsg = '1'
215800131016     C                   Eval      $IntMsg = '010'
215900131016     C                   Eval      $CodMsg = 'NDC1228'
216000131016     C                   ExSr      MemSms
216100131016     C                   EndIf
216200131016     C                   EndIf
216300111024     C                   EndIf
216400111026     C*
216500111026     C* se soggetto non residente, ho bisogno della natura giuridica
216600111026     C* del soggetto
216700111026     C                   If        $Residente = *Off
216800111026     C                   ExSr      RepTabG16
216900111026     C                   EndIf
217000111024     C*
217100111024     C                   ENDSR
217200111024     C/EJECT
217300111024     C************************************************************
217400111024     C* Reperisco i dati di tabella G12
217500111024     C************************************************************
217600111024     C     RepTabG12     BEGSR
217700111024     C*
217800111024     C                   Clear                   ANGG12DS
217900111024     C*
218000111024     C                   Clear                   XATBDS
218100111024     C                   Move      '1'           XTBRIC
218200130925     C                   Move      SocietaP87    XTBAZI
218300111024     C                   Move      *ZERO         XTBKEY
218400111024     C                   Move      'G12'         XTBCOD
218500111110     C                   Move      WStato        XTBKEY
218600111024     C                   Callb     'XATB'
218700111024     C                   Parm                    XATBDS
218800111024     C*
218900111024     C                   If        XTbErr = '0'
219000111024     C                   Movel     XTBUNI        AngG12DS
219100111024     C* CodiceUic è un campo numerico aggiunto in un momento successivo
219200111024     C* alla nascita della tabella: gestisco il caso che non sia mai
219300111024     C* stato valorizzato e che quindi sia rimasto blank su ATBUNI
219400111024     C                   If        %SubSt(AngG12DS: 50: 3) = *Blanks
219500111024     C                   Eval      %SubSt(AngG12DS: 50: 3) = *Zeros
219600111024     C                   Endif
219700111024     C                   Else
219800111024     C                   Clear                   ANGG12DS
219900111024     C* Codice stato inesistente su tab.G12
220000111024     C                   Eval      $TipMsg = '1'
220100111024     C                   Eval      $IntMsg = '009'
220200111024     C                   Eval      $CodMsg = 'PRO0142'
220300111109     C                   ExSr      MemSms
220400111024     C                   EndIf
220500111024     C*
220600111024     C                   ENDSR
220700111024     C/EJECT
220800111026     C************************************************************
220900111026     C* Reperisco i dati di tabella G16
221000111026     C************************************************************
221100111026     C     RepTabG16     BEGSR
221200111026     C*
221300111026     C                   Clear                   ANGG16DS
221400111026     C*
221500111026     C                   Clear                   XATBDS
221600111026     C                   Move      '1'           XTBRIC
221700130925     C                   Move      SocietaP87    XTBAZI
221800111026     C                   Move      *ZERO         XTBKEY
221900111026     C                   Move      'G16'         XTBCOD
222000111026     C                   Move      $NatGiur      XTBKEY
222100111026     C                   Callb     'XATB'
222200111026     C                   Parm                    XATBDS
222300111026     C*
222400111026     C                   If        XTbErr = '0'
222500111026     C                   Movel     XTBUNI        AngG16DS
222600111026     C                   If        TPNG16 = '1'
222700111026     C                   Eval      $PFisica = *On
222800111026     C                   Else
222900111026     C                   Eval      $PFisica = *Off
223000111026     C                   EndIf
223100111026     C                   Else
223200111026     C                   Clear                   ANGG16DS
223300111026     C* Errore nel reperimento della natura giuridica
223400111026     C                   Eval      $TipMsg = '1'
223500111026     C                   Eval      $IntMsg = '011'
223600111026     C                   Eval      $CodMsg = 'PRO0143'
223700111109     C                   ExSr      MemSms
223800111026     C                   EndIf
223900111026     C*
224000111026     C                   ENDSR
224100111026     C/EJECT
224200111025     C************************************************************
224300111025     C* Reperisco dati anagrafici del libro Iva
224400111025     C************************************************************
224500111025     C     RepLibro      BEGSR
224600111025     C*
224700111025     C                   Eval      DLiSocieta = IvaSocieta
224800111025     C                   Eval      DLiTpReg = IvaTpReg
224900111025     C                   Eval      DLiLibro = IvaLibro
225000111025     C     K03Dli01      Chain     ANDLI01L                           21
225100111025     C                   If        *In21 = *On
225200111025     C* Libro IVA inesistente in anagrafica
225300111025     C                   Eval      $TipMsg = '1'
225400111026     C                   Eval      $IntMsg = '016'
225500111025     C                   Eval      $CodMsg = 'NDC1011'
225600111109     C                   ExSr      MemSms
225700111025     C                   Else
225800111025     C*
225900111025     C* se fornitore e registro vendite e territorialità libro CEE,
226000131016     C* si tratta di un reverse charge (eventualmente gestita senza      !)
226100111025     C* automatismi di prima nota)                                     !!!)
226200131016     C****: eseguo il controllo se non è già noto che la registrazione !!!)
226300131016     C****  è una autofattura, altrimenti stamperei due volte la stessa!!!)
226400131016     C****  segnalazione                                               !!!)
226500131016     C****               If        $Autofat = *Off
226600111025     C                   If        SNatura002 = 'F' And
226700111025     C                             IvaTpReg = '2' And
226800111025     C                             DLiTerrito  = '1'
226900131016     C****               Eval      $Autofat = *On
227000131016     C                   Eval      $RevChar = *On
227100131016     C* Registrazione di reverse charge
227200131016     C* le escludo
227300111025     C                   Eval      $TipMsg = '2'
227400111026     C                   Eval      $IntMsg = '017'
227500131016     C                   Eval      $CodMsg = 'NDC1227'
227600111109     C                   ExSr      MemSms
227700111025     C                   EndIf
227800131016     C****               EndIf
227900111025     C*
228000111025     C* se libro Iva di sola liquidazione, scarto la registrazione
228100111025     C                   If        DLiSospens = '5'
228200111025     C                   ExSr      ScartaReg
228300111025     C                   EndIf
228400111025     C*
228500111025     C                   EndIf
228600111025     C*
228700111025     C                   ENDSR
228800111025     C/EJECT
228900111024     C************************************************************
229000111024     C* Chiamata NDMVC002
229100111024     C************************************************************
229200111024     C     CallMvc002    BEGSR
229300111024     C*
229400111024     C                   Clear                   Nd002Ds
229500111024     C*
229600111024     C                   Movel     RegDtReg      Data
229700111024     C                   Eval      LenOut = %Size(ND002DS)
229800111024     C                   CallB     'NDMVC002'
229900130925     C                   Parm      SocietaP87    WSocieta
230000111024     C                   Parm                    WKcc
230100111024     C                   Parm                    WKsc
230200111024     C                   Parm                    Data
230300111024     C                   Parm      *Off          GesMsg
230400111024     C                   Parm      *Off          Esito
230500111024     C                   Parm      'ND002DS'     StrutturaO
230600111024     C                   Parm                    Nd002Ds
230700111024     C                   Parm                    LenOut
230800111024     C*
230900111024     C                   ENDSR
231000111024     C/EJECT
231100111024     C************************************************************
231200111024     C* Chiamata NDMVC003
231300111024     C************************************************************
231400111024     C     CallMvc003    BEGSR
231500111024     C*
231600111024     C                   Clear                   Anope
231700111024     C*
231800111024     C                   Eval      LenOut = %Size(Anope)
231900111024     C                   Callb     'NDMVC003'
232000130925     C                   Parm      SocietaP87    WSocieta
232100111024     C                   Parm                    WCausale
232200111024     C                   Parm      *Loval        Data
232300111024     C                   Parm      '1'           Uso
232400111024     C                   Parm      *Off          GesMsg
232500111024     C                   Parm                    Esito
232600111024     C                   Parm      'ANOPE'       StrutturaO
232700111024     C                   Parm                    Anope
232800111024     C                   Parm                    LenOut
232900111024     C                   Parm      *Off          Refresh
233000111024     C*
233100111024     C                   ENDSR
233200111024     C/EJECT
233300111026     C************************************************************
233400111026     C* Scrittura archivi di output
233500111026     C************************************************************
233600111026     C     WrtOut        BEGSR
233700131016     C*
233800131126C2231C                   If        $Nota  = *on
233900131126  "   * Posso avere due record di sda per registrazione
234000131126  "   * scrivo sda con importo positivo
234100131121  "  C                   If        $ImportoP <> 0
234200131121  "  C                   Eval      $Imponib = $ImponibP
234300131126  "  C                   Eval      $Imposta = $ImpostaP
234400131126  "  C* scrivo sda
234500131126  "  C* reperisco numero interno  per NDSda
234600131126  "  C                   ExSr      RepNum
234700131126  "  C                   ExSr      WrtSda
234800131126  "  C                   Endif
234900131126  "   *
235000131126  "  C* scrivo sda con importo negativo
235100131121  "  C                   If        $ImportoN <> 0
235200131121  "  C                   Eval      $Imponib = $ImponibN
235300131121  "  C                   Eval      $Imposta = $ImpostaN
235400131121  "  C* scrivo sda con importo negativo
235500131121  "  C* reperisco numero interno  per NDSda
235600131121  "  C                   ExSr      RepNum
235700131121  "  C                   ExSr      WrtSda
235800131126  "  C                   Endif
235900131126C2231C                   Else
236000131126     C* scrivo sda
236100131126     C* reperisco numero interno  per NDSda
236200131126     C                   ExSr      RepNum
236300131126     C                   ExSr      WrtSda
236400131126C2231C                   Endif
236500131121      *
236600131016      * se ho scritto un  record in reverse charge
236700131016      * ed ho una collegata la aggiorno
236800131016     C                   If        SdaRevChar = *on
236900131016     C                             and REGNRASCOL <> 0
237000131016     C                   Eval      SdaSocieta = SocietaP87
237100131016     C                   Eval      SdaSys     = RegSys
237200131016     C                   Eval      SdaNrAsReg = RegNrAsCol
237300131016     C                   Eval      SdaAnno    = AnnoP87
237400131121C2231 * Potrei avere due ndsda
237500131121 "   C     K04Sda02      SetLL     NdSda02L
237600131121 "   C                   Do        *hival
237700131121 "   C     K04Sda02      ReadE     NdSda02L
237800131121 "   C                   If        %Eof(NdSda02L)
237900131121 "   C                   Leave
238000131121 "   C                   Endif
238100131121C2231x***  K04Sda02      Chain     NDSda02L
238200131121 "   x***                If        %Found(NdSda02L)
238300131121 "   x**Se autofattura non può essere reverse charge      C2230
238400131121 "   x***                          and SdaAutoFat= *off   C2230
238500131121C2231C                   If        SdaAutoFat= *off
238600131016     C                   Eval      SdaRevChar = *on
238700131107C2230x****               Eval      SdaAutofat = *off
238800131016     C                   Update    NdSda02
238900131016     C                   Endif
239000131121C2231C                   EndDO
239100131121      *
239200131016     C                   Endif
239300131016      *
239400131021     C*********          Commit
239500131021      *
239600111026     C                   ENDSR
239700111026     C/EJECT
239800111025     C************************************************************
239900130930     C* Scrittura NDSda00F
240000111025     C************************************************************
240100130930     C     WrtSda        BEGSR
240200111025     C*
240300130930     C                   Clear                   NDSda000
240400131003     C* tipo elaborazione=spesometro
240500131218C2231x****               Eval      SDATPELA   = '1'
240600131218C2231C                   Eval      SDATPELA   = TpElaP87
240700130930     C                   Eval      SdaSocieta = SocietaP87
240800130930     C                   Eval      SdaAnno    = AnnoP87
240900131218C2231C                   Eval      SdaTipPer  = TipPerP87
241000131218C2231C                   Move      PeriodoP87    SdaPeriodo
241100130930     C                   Eval      SdaNumEla  = NumElaP87
241200131003     C                   Eval      SdaNumInt  = $NrAsInt
241300111026     C*
241400131003     C                   Eval      SdaQuadro  = $Quadro
241500131008     C                   Eval      SdaDtOpe   = $DtOpe
241600131008     C                   Eval      SdaNrOpe   = $NrOpe
241700111025     C*
241800130930     C                   Eval      SdaSys     = RegSys
241900130930     C                   Eval      SdaNrAsReg = RegNrAsReg
242000111026     C*
242100130930     C                   Eval      SdaCtb     = RegCtb
242200130930     C                   Eval      SdaTpReg   = $TpReg
242300130930     C                   Eval      SdaLibro   = $Libro
242400130930     C                   Eval      SdaCausTes = RegCausTes
242500130930     C                   Eval      SdaDtReg   = RegDtReg
242600130930     C                   Eval      SdaNrReg   = RegNrReg
242700130930     C                   Eval      SdaSerReg  = RegSerReg
242800130930     C                   Eval      SdaDtDoc   = RegDtDoc
242900130930     C                   Eval      SdaNrDoc   = RegNrDoc
243000130930     C                   Eval      SdaSerDoc  = RegSerDoc
243100131003     C                   Eval      SDADTDCPAG = REGDTLIB2
243200111026     C*
243300130930     C                   Eval      SdaImpMov  = $CFImporto
243400130930     C                   Eval      SdaKcc     = $CFKcc
243500130930     C                   Eval      SdaKsc     = $CFKsc
243600130930     C                   Eval      SdaCliFor  = $CFCliFor
243700130930     C                   Eval      SdaSogg    = $CFSogg
243800111026     C*
243900130930     C                   Eval      SdaCdFisc  = $CdFisc
244000130930     C                   Eval      SdaPartIva = $PartIva
244100111026     C*
244200130930     C                   Eval      SdaTipOpe  = $TipOpe
244300111026     C*
244400131003     C                   Eval      SdaImponib = $Imponib
244500131003     C                   Eval      SdaImposta = $Imposta
244600131003     C                   Eval      SdaImporto = $Imponib + $Imposta
244700131121     C*
244800131104C2228x**Se è una fattura $SegnoN = 0
244900131104  "  x**Se è una nota  $SegnoN mi dice se è di addebito(=-1) o accredito(=1)
245000131104  "  x**Se è = -1  rendo negativi gli importi
245100131104  "  x**                 If        $SegnoN = -1
245200131104  "  x**                 Eval      SdaImponib = SdaImponib * $SegnoN
245300131104  "  x**                 Eval      SdaImposta = SdaImposta * $SegnoN
245400131104  "  x**                 Eval      SdaImporto = SdaImporto * $SegnoN
245500131104C2228x**                 Endif
245600111026     C*
245700130930     C                   Eval      SdaCognome = $Cognome
245800130930     C                   Eval      SdaNome    = $Nome
245900130930     C                   Eval      SdaDtNasc  = $DtNasc
246000130930     C                   Eval      SdaComNA   = $ComNA
246100130930     C                   Eval      SdaPrvNA   = $PrvNA
246200131003     C                   Eval      SdaStaEsDo = $StaEsD
246300130930     C                   Eval      SdaRagSoc  = $RagSoc
246400131003     C                   Eval      SdaLocEsSL = $LocEst
246500131003     C                   Eval      SdaStaEsSL = $StaEsS
246600131003     C                   Eval      SdaIndEsSL = $IndEst
246700111026     C*
246800130930     C                   Eval      SdaCorCas  = '0'
246900131016     C                   Eval      SdaDescr   = *blank
247000111027     C*
247100130930     C                   Eval      SdaAnn     = '0'
247200130930     C                   Eval      SdaMan     = '0'
247300130930     C                   Eval      SdaErr     = $Errore
247400130930     C                   Eval      SdaEsc     = $Esclus
247500111027     C*
247600130930     C                   Eval      SdaUteImm  = XScCUt
247700130930     C                   Eval      SdaDtImm   = UDateIso
247800130930     C                   Eval      SdaUteUlMo = XScCUt
247900130930     C                   Eval      SdaDtUlMo  = UDateIso
248000111109     C*
248100131003     C                   Eval      SdaAutofat = $Autofat
248200131016     C                   Eval      SdaRevChar = $RevChar
248300131016     C                   Eval      SdaDocRie  = $DocRie
248400111109     C*
248500111026     C*
248600130930     C                   Write     NDSda000
248700111025     C*
248800111025     C                   ENDSR
248900111025      /EJECT
249000111025     C************************************************************
249100111025     C* Scrittura NDSMS00F
249200111025     C************************************************************
249300111025     C     WrtSms        BEGSR
249400111025     C*
249500111025     C                   Clear                   NDSMS000
249600111025     C*
249700130925     C                   Eval      SmsSocieta = SocietaP87
249800130925     C                   Eval      SmsAnno = AnnoP87
249900111025     C                   Eval      SmsNrAsInt = $NrAsInt
250000111111     C                   Eval      SmsSys = $Sys
250100111111     C                   Eval      SmsNrAsReg = $NrAsReg
250200111025     C*
250300111025     C                   Eval      SmsTipMSG = $TipMsg
250400111025     C                   Eval      SmsIntMSG = $IntMsg
250500111025     C                   Eval      SmsCodMSG = $CodMsg
250600111025     C                   Eval      SmsTxtMSG = $TxtMsg
250700111025     C*
250800111025     C                   Write     NDSMS000
250900111025     C*
251000111025     C                   ENDSR
251100111025      /EJECT
251200111024     C************************************************************
251300111024     C* REPERIMENTO DATI SOCIETA'
251400111024     C************************************************************
251500111024     C     REPSOC        BEGSR
251600111024     C*
251700111024     C                   CallB     'XSOC'
251800111024     C                   Parm                    TipXSc            6
251900111024     C                   Parm                    SocXSc            3
252000111024     C                   Parm                    CdsXSc            9 0
252100111024     C                   Parm                    ModXSc            3
252200111024     C                   Parm      *Blanks       RtnXSc            1
252300111024     C                   Parm                    XSocDS
252400111024     C                   Parm                    KPJBA
252500111024     C*
252600111024     C                   ENDSR
252700111024     C/EJECT
252800111108     C************************************************************
252900111108     C* Memorizzazione segnalazione di errore
253000111108     C************************************************************
253100111109     C     MemSms        BEGSR
253200111108     C*
253300111108     C* esclusione
253400111108     C                   If        $TipMsg = '2'
253500111108     C*
253600111108     C* incremento il contatore
253700111108     C                   Add       1             $MsgX
253800111108     C*
253900111108     C* memorizzo i dati del messaggio
254000111108     C                   Eval      SkTipMsgX($MsgX) = $TipMsg
254100111108     C                   Eval      SkIntMsgX($MsgX) = $IntMsg
254200111108     C                   Eval      SkCodMsgX($MsgX) = $CodMsg
254300111109     C*
254400111109     C* accendo il flag di esclusione
254500111109     C                   Eval      $Esclus = *On
254600111108     C*
254700111108     C* errore o warning
254800111108     C                   Else
254900111108     C*
255000111108     C* incremento il contatore
255100111108     C                   Add       1             $MsgE
255200111108     C*
255300111108     C* memorizzo i dati del messaggio
255400111108     C                   Eval      SkTipMsgE($MsgE) = $TipMsg
255500111108     C                   Eval      SkIntMsgE($MsgE) = $IntMsg
255600111108     C                   Eval      SkCodMsgE($MsgE) = $CodMsg
255700111109     C*
255800111109     C* accendo il flag di errore
255900111109     C                   If        $TipMsg = '1'
256000111109     C                   Eval      $Errore = *On
256100111109     C                   EndIf
256200111108     C*
256300111108     C                   EndIf
256400111108     C*
256500111108     C                   ENDSR
256600111108     C/EJECT
256700111108     C************************************************************
256800111108     C* Scarico segnalazioni di errore
256900111108     C************************************************************
257000111108     C     ScaSMS        BEGSR
257100111108     C*
257200111108     C* solo adesso che ho terminato l'esame della registrazione,
257300111108     C* posso "scaricare" le segnalazioni di errore:
257400111108     C* 1) evito in questo modo di stampare messaggi di registrazioni
257500111108     C*    che durante l'elaborazione sono state scartate
257600111108     C* 2) posso stampare tra gli errori SOLO quelli relativi a
257700111108     C*    registrazioni NON escluse
257800111108     C*
257900111108     C* esclusione (da fare per primo)
258000111108     C                   For       $I = 1 To $MsgX
258100111108     C                   Eval      $TipMsg = SkTipMsgX($I)
258200111108     C                   Eval      $IntMsg = SkIntMsgX($I)
258300111108     C                   Eval      $CodMsg = SkCodMsgX($I)
258400111108     C                   ExSr      GesSms
258500111108     C                   EndFor
258600111108     C*
258700111108     C* errori e warning
258800111108     C                   For       $I = 1 To $MsgE
258900111108     C                   Eval      $TipMsg = SkTipMsgE($I)
259000111108     C                   Eval      $IntMsg = SkIntMsgE($I)
259100111108     C                   Eval      $CodMsg = SkCodMsgE($I)
259200111108     C                   ExSr      GesSms
259300111108     C                   EndFor
259400111108     C*
259500111108     C                   ENDSR
259600111108     C/EJECT
259700111024     C************************************************************
259800111024     C* Gestione segnalazioni
259900111024     C************************************************************
260000111108     C     GesSms        BEGSR
260100111024     C*
260200111024     C* reperimento testo messaggio
260300111031     C                   Move      $CodMsg       WIdMsg
260400111024     C                   ExSr      RepTxtMsg
260500111024     C                   If        ErrMsg = *Off
260600111024     C                   Eval      $TxtMsg = TxtMsg
260700111024     C                   Else
260800111024     C                   Eval      $TxtMsg = *Blanks
260900111024     C                   EndIf
261000111026     C*
261100111026     C* imposta flag e stampa errore / motivo di esclusione
261200111026     C                   Select
261300111026     C                   When      $TipMsg = '1'
261400111026     C                   Eval      $Errore = *On
261500111026     C                   ExSr      StampaErr1
261600111026     C                   When      $TipMsg = '2'
261700111026     C                   Eval      $Esclus = *On
261800111026     C                   ExSr      StampaErr2
261900111026     C                   When      $TipMsg = '3'
262000111108     C                   ExSr      StampaErr1
262100111026     C                   EndSl
262200111025     C*
262300111025     C* scrittura su NDSMS00F
262400111024     C                   ExSr      WrtSms
262500111024     C*
262600111024     C                   ENDSR
262700111024     C/EJECT
262800111031     C************************************************************
262900111031     C* Gestione errore bloccante
263000111031     C************************************************************
263100111031     C     GesErrBlk     BEGSR
263200111031     C*
263300111031     C* impostazione flag di errore esecuzione
263400111031     C                   Eval      pErrEse = *On
263500111031     C*
263600111031     C* richiamo programma di stampa
263700130925     C                   Call      'NDCP87R9'
263800111031     C                   Parm                    Kpjba
263900111031     C                   Parm                    pBlkCodMsg        7
264000111031     C*
264100111031     C* fine programma immediata
264200111031     C                   ExSr      EndPgm
264300111031     C*
264400111031     C                   ENDSR
264500111031     C/EJECT
264600111024     C************************************************************
264700111026     C* Stampa errori
264800111024     C************************************************************
264900111024     C     StampaErr1    BEGSR
265000111108     C*
265100111108     C* stampa gli errori solo se riferiti a una non esclusa
265200111108     C                   If        $Esclus = *Off
265300111024     C*
265400111024     C* stampa intestazione
265500111026     C                   If        *In37 = *On
265600130925     C                   Write     CP871TS
265700130925     C                   Write     CP871T1
265800111026     C                   SetOff                                       37
265900111024     C                   EndIf
266000111024     C*
266100111024     C* stampa errore
266200111024     C                   ExSr      RieR1
266300130925     C                   Write     CP871R1
266400111108     C*
266500111108     C                   EndIf
266600111024     C*
266700111024     C                   ENDSR
266800111024     C/EJECT
266900111024     C************************************************************
267000111026     C* Stampa motivi di esclusione
267100111024     C************************************************************
267200111024     C     StampaErr2    BEGSR
267300111024     C*
267400111024     C* stampa intestazione
267500111026     C                   If        *In38 = *On
267600130925     C                   Write     CP872TS
267700130925     C                   Write     CP872T1
267800111026     C                   SetOff                                       38
267900111024     C                   EndIf
268000111024     C*
268100111024     C* stampa errore
268200111024     C                   ExSr      RieR1
268300130925     C                   Write     CP872R1
268400111024     C*
268500111024     C                   ENDSR
268600111024     C/EJECT
268700111024     C************************************************************
268800111024     C* Riempimento riga di stampa errore
268900111024     C************************************************************
269000111024     C     RieR1         BEGSR
269100111024     C*
269200111024     C                   If        RegDtReg <> *Loval
269300111024     C     *JobRun       Move      RegDtReg      R1DtReg
269400111024     C                   Else
269500111024     C                   Z-Add     0             R1DtReg
269600111024     C                   EndIf
269700111024     C                   Eval      R1NrReg = RegNrReg
269800111024     C                   Eval      R1SerReg = RegSerReg
269900111024     C                   Eval      R1CausTes = RegCausTes
270000111024     C                   Eval      R1Kcc = $CFKcc
270100111024     C                   Eval      R1Ksc = $CFKsc
270200111024     C                   If        RegDtDoc <> *Loval
270300111024     C     *JobRun       Move      RegDtDoc      R1DtDoc
270400111024     C                   Else
270500111024     C                   Z-Add     0             R1DtDoc
270600111024     C                   EndIf
270700111024     C                   Eval      R1NrDoc = RegNrDoc
270800111024     C                   Eval      R1SerDoc = RegSerDoc
270900111024     C                   Eval      R1Importo = $CFImporto
271000111024     C*
271100111024     C                   Eval      R1Testo = $TxtMsg
271200111024     C*
271300111024     C                   ENDSR
271400111024     C/EJECT
271500111024     C************************************************************
271600111024     C* Reperimento testo messaggi
271700111024     C************************************************************
271800111024     C     RepTxtMsg     BEGSR
271900111024     C*
272000111024     C                   CallB     'XRTVM'
272100111024     C                   Parm      WIdMsg        IdMsg            27
272200111024     C                   Parm      *Blank        TxtMsg          100
272300111024     C                   Parm      *Off          ErrMsg            1
272400111024     C                   Parm                    *Omit                          centratura
272500111024     C                   Parm                    *Omit                          lun output.
272600111024     C                   Parm      *Blank        MsgDta          100
272700111024     C*
272800111024     C                   ENDSR
272900111024     C/EJECT
273000111024     C************************************************************
273100111024     C* Scarta registrazione
273200111024     C************************************************************
273300111024     C     ScartaReg     BEGSR
273400111024     C*
273500111024     C* in taluni casi può capitare che una registrazione venga
273600111024     C* scartata dopo essere stata parzialmente elaborata (di solito
273700111024     C* per mancata corrispondenza con i criteri di lancio)
273800111025     C*
273900111025     C* la registrazione non va elaborata
274000111025     C                   Eval      $RegOk = *Off
274100111024     C*
274200111024     C                   ENDSR
274300111024     C/EJECT
274400111024     C************************************************************
274500111024     C* OPERAZIONI INIZIALI
274600111024     C************************************************************
274700111024     C     *INZSR        BEGSR
274800111024     C*
274900111024     C* Reperimento parametri
275000111024     C*
275100111024     C     *Entry        PList
275200111024     C                   Parm                    Kpjba
275300111024     C                   Parm                    pErrEse
275400111026     C*
275500111026     C* accensione indicatore di overflow per stampa prima testata
275600111026     C                   SetOn                                        37
275700111026     C                   SetOn                                        38
275800111026     C                   SetOn                                        39
275900111024     C*
276000131021     C*
276100131021     C************       CALLB     'XSTRCMT'
276200131021     C                   OPEN      NDSDA00F
276300131021     C                   OPEN      NDSDA02L
276400131021     C*
276500111024     C                   ENDSR
276600111024      /EJECT
276700111024     C************************************************************
276800111024     C* INIZIALIZZAZIONE VARIABILI
276900111024     C************************************************************
277000111024     C     INZVAR        BEGSR
277100111024     C*
277200130925     C                   Movel     KPJBU         NDCP87DS
277300111024     C*
277400111024     C* data del giorno
277500111024     C     *DMY          Move      UDate         UDateISO
277600111024     C*
277700111024     C* inizializzo flag di errore
277800111024     C                   Movel     *Off          pErrEse
277900111024     C*
278000111024     C* reperimento dati società
278100111024     C                   Movel     'SOC001'      TIPXSC
278200130925     C                   Movel     SocietaP87    SOCXSC
278300111024     C                   ExSr      RepSoc
278400111024     C     RTNXSC        IfNE      '1'
278500111024     C                   Movel     XSOCDS        SOC001
278600111024     C                   Else
278700111031     C                   Eval      pBlkCodMsg = 'PRO0027'
278800111031     C                   ExSr      GesErrBlk
278900111024     C                   EndIf
279000111025     C*
279100111025     C* reperisco dati di parametro modulo PJSPESOM
279200131001     C                   Clear                   ANP098DS
279300111025     C                   CallB     'XPAR'
279400130925     C                   Parm      SocietaP87    XPaSoc
279500130925     C                   Parm      'PJSPES13'    XPaPAR
279600111025     C                   Parm      *Blanks       XPaOut
279700111025     C                   Parm      *Blanks       XPaErr
279800111025     C                   Parm      *Blanks       XPaRic
279900111025     C     XPaErr        IfEQ      *OFF
280000131001     C                   Movel     XPAOUT        ANP098DS
280100111025     C                   Else
280200131001     C* se non vengono reperiti i dati di parametro modulo PJSPES13,
280300111025     C* si tratta di un errore bloccante
280400111031     C                   Eval      pBlkCodMsg = 'PRO3320'
280500111031     C                   ExSr      GesErrBlk
280600111025     C                   EndIf
280700131219      *
280800131219C2231C* reperisco la testata elaborazione
280900131219 "   C                   Eval      STTSOCIETA = xscsoc
281000131219 "   C                   Eval      STTNUMELA  = NUMELAP87
281100131219 "   C     K02Stt01      Chain     Ndstt01L                           21
281200131219 "   C                   If        not %found(Ndstt01l)
281300131219 "   C                   Eval      pBlkCodMsg = 'PRO1660'
281400131219 "   C                   ExSr      GesErrBlk
281500131219C2231C                   Endif
281600111024     C*
281700111024     C* reperisco nome sistema informatico
281800111024     C                   CALL      'XNETA'                              21
281900111024     C                   PARM                    NOMSYS
282000111024     C*
282100111024     C                   MOVEL     KNSIF         NOMSIF
282200111024     C                   MOVEL     XSCDSI        NOMDIT
282300111024     C                   MOVEL     DSPGM         NOMPGM
282400111024     C*
282500111024     C                   ENDSR
282600111024      /EJECT
282700111024     C************************************************************
282800111024     C* Definizione KList
282900111024     C************************************************************
283000111024     C     DefKList      BEGSR
283100130930     C*
283200130930     C     K04Sda02      KList
283300130930     C                   KFld                    SdaSocieta
283400130930     C                   KFld                    SdaSys
283500130930     C                   KFld                    SdaNrAsReg
283600130930     C                   KFld                    SdaAnno
283700131219     C*
283800131219C2231C     K02Stt01      KList
283900131219 "   C                   KFld                    STTSOCIETA
284000131219C2231C                   KFld                    STTNUMEla
284100130930     C*
284200130930     C     K03Sms01      KList
284300130930     C                   KFld                    SmsSocieta
284400130930     C                   KFld                    SmsAnno
284500130930     C                   KFld                    SmsNrAsInt
284600130930     C*
284700131030C2228C* Trattamenti speciali
284800131030 "   C     K03Smt01      KList
284900131030 "   C                   KFld                    SmtSocieta
285000131030 "   C                   KFld                    SmtCodice
285100131030 "   C                   KFld                    SmtTpDato
285200131030 "   C*
285300131030 "   C     K07Smt02      KList
285400131030 "   C                   KFld                    SmtSocieta
285500131030 "   C                   KFld                    SmtTpReg
285600131030 "   C                   KFld                    SmtLibro
285700131030 "   C                   KFld                    SmtAliq
285800131030 "   C                   KFld                    SmtRifIva
285900131030 "   C                   KFld                    SmtCausale
286000131030C2228C                   KFld                    SmtCodice
286100131030     C*
286200111024     C*
286300111024     C     K02Reg01      KList
286400111024     C                   KFld                    RegSys
286500111024     C                   KFld                    RegNrAsReg
286600111024     C*
286700111024     C     K02Reg02      KList
286800111024     C                   KFld                    RegSocieta
286900111024     C                   KFld                    RegDtReg
287000111024     C*
287100111024     C     K02Iva01      KList
287200111024     C                   KFld                    IvaSys
287300111024     C                   KFld                    IvaNrAsReg
287400111024     C*
287500111024     C     K02Mov01      KList
287600111024     C                   KFld                    MovSys
287700111024     C                   KFld                    MovNrAsReg
287800111024     C*
287900111024     C     K03Mov01      KList
288000111024     C                   KFld                    MovSys
288100111024     C                   KFld                    MovNrAsReg
288200111024     C                   KFld                    MovNrRigaM
288300111024     C*
288400111024     C     K03MoA01      KList
288500111024     C                   KFld                    MoaSys
288600111024     C                   KFld                    MoaNrAsReg
288700111024     C                   KFld                    MoaNrRigaM
288800111024     C*
288900111024     C     K04MoA01      KList
289000111024     C                   KFld                    MoaSys
289100111024     C                   KFld                    MoaNrAsReg
289200111024     C                   KFld                    MoaNrRigaM
289300111024     C                   KFld                    MoaDimen
289400111024     C*
289500111024     C     K07PPa01      KList
289600111024     C                   KFld                    PPaSocieta
289700111024     C                   KFld                    PPaCtb
289800111024     C                   KFld                    PPaKcc
289900111024     C                   KFld                    PPaKsc
290000111024     C                   KFld                    PPaDtPar
290100111024     C                   KFld                    PPaNrPar
290200111024     C                   KFld                    PPaSerPar
290300111024     C*
290400111024     C     K03Pas01      KList
290500111024     C                   KFld                    PasSys
290600111024     C                   KFld                    PasNrAsReg
290700111024     C                   KFld                    PasNrRigaM
290800111024     C*
290900111024     C     K03DLi01      KList
291000111024     C                   KFld                    DLiSocieta
291100111024     C                   KFld                    DLiTpReg
291200111024     C                   KFld                    DLiLibro
291300111024     C*
291400111024     C     K04Riv01      KList
291500111024     C                   KFld                    RIvSocieta
291600111024     C                   KFld                    RIvTpReg
291700111024     C                   KFld                    RIvAliq
291800111024     C                   KFld                    RIvRifIva
291900111024     C*
292000111024     C     K05ALR01      KList
292100111024     C                   KFld                    ALRSocieta
292200111024     C                   KFld                    ALRTpReg
292300111024     C                   KFld                    ALRLibro
292400111024     C                   KFld                    ALRAliq
292500111024     C                   KFld                    ALRRifIva
292600111024     C*
292700111024     C     K02Cpa01      KList
292800111024     C                   KFld                    CpaSys
292900111024     C                   KFld                    CpaNrasInt
293000130930     ***
293100130930     ***   K04Rdc03      KList
293200130930     ***                 KFld                    RdcSocieta
293300130930     ***                 KFld                    RdcDtReg
293400130930     ***                 KFld                    RdcNrReg
293500130930     ***                 KFld                    RdcSerReg
293600111024     C*
293700111024     C                   ENDSR
293800111024      /EJECT
