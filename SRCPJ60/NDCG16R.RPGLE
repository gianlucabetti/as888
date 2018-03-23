000100980929     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200980929     H*PARMS ACTGRP(QILE) CVTOPT(*DATETIME)
000300090504>>>>>H*PARMS OPTIMIZE(*FULL)
000400950626     H DECEDIT('0,') DATEDIT(*DMY.)
000500001215     H*      -----------------------------------------------
000600001215     H*     PER COMPILARE QUESTO SORGENTE ESEGUIRE IL COMANDO
000700001215     H*    !!!!!!!!!!!!  CALL NDCG16COMP !!!!!!!!!!!!
000800001215     H*
000900960627     H*      Calcolo dati contabili per griglia
001000960307     H*      -----------------------------------------------
001100960919     H*  ATTENZIONE:
001200960919     H*  Tutti i file di società DEVONO essere in usropn
001300960919     H*  perchè il pgm può cambiare il sistema
001400960919     H*  informativo
001500960919     F*-------------------------------------------------------------------
001600001222     F*               MAPPA FLAG
001700001222     F* Ci sono tre tipi di calcolo:
001800001222     F*
001900001222     F* 1) NOTTURNO (ANA904R)
002000001222     F* FlgConto = sempre ad *on, cioè la lettura dello scadenzario
002100001222     F*            è per conto e non per partita
002200001222     F* FlgX20   = sempre ad *off, cioè NON lancia MAI il calcolo
002300001222     F*            dello scadenzario, ma usa quello calcolato prima
002400001222     F*            da ANA904R
002500001222     F* FlgQtemp = sempre ad *off. NON legge MAI i file dello scaden
002600001222     F*            zario in qtemp, MA SEMPRE quelli residenti.
002700001222     F*            (NDSCD01W e NDSCP01W)
002800001222     F*
002900001222     F* 2) BATCH (az. CG11)
003000001222     F* FlgConto = sempre ad *on, cioè la lettura dello scadenzario
003100001222     F*            è per conto e non per partita
003200001222     F* FlgX20   = sempre ad *on, chiama SEMPRE X20SCADR, in
003300001222     F*            quanto lancia il calcolo dello scadenzario in QTEMP
003400001222     F* FlgQtemp = sempre ad *on. NON legge MAI i file dello scaden
003500001222     F*            zario residenti, ma SEMPRE quelli in qtemp.
003600001222     F*            (NDSCD01L e NDSCP01L)
003700001222     F*
003800001222     F* 3) INTERATTIVO (az.CG10)
003900001222     F* SE NON SCELTI PARAMETRI PARTICOLARI SULLA SECONDA VIDEATA
004000001222     F* E' UGUALE AL CASO 2) BATCH
004100001222     F*
004200001222     F* SE SCELTI PARAMETRI PARTICOLARI
004300001222     F* FlgConto = sempre ad *off,cioè la lettura dello scadenzario
004400001222     F*            è per partita e non per conto
004500001222     F* FlgX20   = è ad *on se chiama il calcolo dello scadenzario, ad
004600001222     F*            *off se non serve in quanto la partita è chiusa.
004700001222     F* FlgQtemp = è ad *on se chiamato il calcolo dello scadenzario,
004800001222     F*            altrimenti ad *off, quindi si possono leggere per
004900001222     F*            una partita i file residenti (NDSCP01W o NDSCD01W)
005000001222     F*            per un'altra i file in qtemp.
005100001222     F*
005200951211     F*----------------------------------------------------*
005300960919     FNDPPA01L  IF   E           K DISK    usropn
005400960919    >FNDPPS01L  IF   E           K DISK    usropn
005500960307     F*------------
005600960628     FNDGVA01L  IF   E           K DISK
005700000915C1390FNDGVE01L  IF   E           K DISK
005800960919     FNDMIX01L  IF   E           K DISK    usropn
005900020917D1199FNDCSC01L  IF   E           K DISK
006000960123     D*-------------
006100960715     FNDSCD01L  IF   E           K DISK    usropn
006200960715     FNDSCP01L  IF   E           K DISK    usropn
006300001027D0747F* viste logiche che puntano sempre sui file residenti
006400001027D0747FNDSCD01W  IF   E           K DISK    usropn
006500001027D0747F                                     RENAME(NDSCD000:NDSCD1W)
006600001027D0747FNDSCP01W  IF   E           K DISK    usropn
006700001027D0747F                                     RENAME(NDSCP000:NDSCP1W)
006800960628     D*-------------
006900960802     FNDGCO01L  UF A E           K DISK    USROPN
007000960709    >F                                     RENAME(NDGCO000:NDGCO01)
007100960802     FNDGCO04L  UF   E           K DISK    USROPN
007200960709    >F                                     RENAME(NDGCO000:NDGCO04)
007300960628     D*-------------
007400960730     D*Schiere importi 8 periodi in lire e divisa virtuale e totale effettiv
007500960730     DVirIsca0         S             19  3 DIM(8)                               IMPORTI IN LIRE
007600960730     DTotIsca0         S             19  3 DIM(8)                               IMPORTI IN LIRE
007700960730     DVirIsca0D        S             19  3 DIM(8)                               IMPORTI IN LIRE
007800960730     DTotIsca0D        S             19  3 DIM(8)                               IMPORTI IN LIRE
007900960730     D*-------------
008000960307     D datmin          C                   const('0001-01-01')
008100960726     D datmax          C                   const('2039-12-31')
008200960730     D datmax1         C                   const('9999-12-31')
008300960131     D*-------------
008400950626     D KPJBA         E DS
008500960628     D*-------------
008600960628     D* Parametri gestione credito/debito
008700960628     D ANP016DS      E DS
008800960628     D*-------------
008900960628     D* DS gestione parametri modulo
009000960628     D XPAOUT          DS          2000
009100960923     D*-------------
009200960923     D* Reperimento Parametri Mix di Partita
009300960923     D Anp011DS      E DS
009400960923     D*-------------
009500960923     D* Tabella G86 per cod.entità
009600960923     D AngG86DS      E DS
009700960923     D*-------------
009800960923     D* Schiera di appoggio per tipi entità dal Mix di partita
009900960923     D TipEnt          S              3    Dim(8)Inz
010000950626     D*-------------
010100950626     D* Reperimento nome PGM
010200950626     D STATUS         SDS           333
010300950626     D  DSPGM            *PROC
010400950626     D*-------------
010500950626     D* Parametri ricevuti dal PGM
010600960708     D NDCG01DS      E DS
010700960130     D*-------------
010800950626     D* Dati per pgm XSOC
010900950626     D SOC001        E DS                  EXTNAME(XSOC001DS)
011000960926     D    FgO5                        1    overlay(XscFgo:5)                    Scadenz. retroattivo
011100981002     D    XScNrDecMC                  1S 0 Overlay(XScSkP:6)
011200950626     D*-------------
011300950626     D* DS Interna per dati utente
011400950626     D XSOCDS          DS          1000
011500960923     D*-------------
011600960923     D* Richiamo a XATB
011700960923     D XATBDS        E DS
011800960628     D*-------------
011900960628     D* DS esterna per dati clienti
012000960628     D DVACLI        E DS                  INZ
012100960628     D* Definizioni parametri driver anagrafici
012200960628     DDvaric           S              1
012300960628     DDvaSocieta       S              3
012400960628     DDvaUnita         S              8
012500960628     DDvaStrutt        S             10
012600960628     DDvaDtRif         S               D
012700960628     DDvaLenOut        S              9B 0
012800960628     DDvaSnatura       S              1
012900960628     DDvaCodice        S              8
013000960628     DDvaLineav        S              3
013100960628     DDvaFiliale       S              3  0
013200960628     DDvaSogg          S              8
013300960628     DDvaTpInd         S              2
013400960628     DDvaCdInd         S              3
013500960628     DDvaErrore        S              1
013600960628     DDvaOutput        S           4000
013700960923     D*-------------
013800981026     DAnrco          E DS                  EXTNAME(ANRCO00F)   inz
013900981026     D  RCODTANN     E                     inz('0001-01-01')
014000981026     D  RCODTIMM     E                     inz('0001-01-01')
014100981026     D  RCODTULMO    E                     inz('0001-01-01')
014200960923     D*-------------
014300960923     D* Definizioni parametri standard chiamate moduli
014400960923     D Esito           S              1
014500960923     D gesmsg          S              1
014600960923     D lenout          S              9B 0
014700960923     D Data            S             10
014800960923     D strutturaO      S             10
014900960923     D Operazione      S              2
015000960923     D LenIn           S              9B 0
015100960923     D refresh         S              1
015200960923     D uso             S              1
015300960923     DOutput           S           4000
015400960709     D*-------------
015500960709     D* Cambio
015600980929     D*X01CAMBIDS    E DS                                    inz
015700980929     D*X03MultcDS    E DS                                    inz
015800000915     D*-----------
015900000915C1390D* Schiere x pgm ANA960R
016000000915  "  D FIL960          S             10    DIM(10)
016100000915  "  D DAT960          S           4096    DIM(10)
016200000915  "  D*-------------
016300000915  "  D* DS esterna per dati PARTITA
016400000915C1390D NDPPA         E DS                  EXTNAME(ndppa00F)
016500960923     D*-------------
016600960923     D* Parametri per reperimento esercizio contabile
016700960923    >Dx10eseds       E DS
016800960923     D*-------------
016900960923     D* Parametri per reperimento esercizio statistico
017000960923    >Dx50eseds       E DS
017100960923     D*-------------
017200990122     DAnope          E DS                  EXTNAME(ANOPE00F) inz
017300990122     D  OPEDTANN     E                     inz('0001-01-01')
017400990122     D  OPEDTIMM     E                     inz('0001-01-01')
017500990122     D  OPEDTULMO    E                     inz('0001-01-01')
017600950626     D*-------------
017700960123     D* variabili
017800960123     D $Fine           S              1A
017900960123     D $RcdOK          S              1A
018000960729     D flgx20          S              1
018100001027D0747D flgQtemp        S              1
018200001107D0747D flgScadenz      S              1
018300001107D0747D flgEntita       S              1
018400001106D0747D flgOVER         S              1
018500960726     D flgconto        S              1
018600960628     D flgPPS          S              1
018700960722     D flgMIX          S              1
018800960628     D udateiso        S             10
018900960628     D DtRischio       S             10
019000960628     D DataIso1        S               D
019100960628     D DataIso2        S               D
019200960628     D Wdare           S             19  3
019300960628     D Wavere          S             19  3
019400960628     D Wsaldo          S             19  3
019500960726     D WdareD          S             19  3
019600960726     D WavereD         S             19  3
019700960730     D NrDecMc         S              1  0
019800960628     D Oggrisc         S                   like(OpeggRisc)
019900990607     D WrkPeriodo      S                   like(GcoPeriodo)
020000990607     D DataIso         S               D
020100020924d1199D Savkcc          S                   like(kccG01)
020200960926     D*
020300960926     D* Campi di salvataggio
020400960926     D SAVDTVAL        S                   like(MixdtvAL) inz
020500960926     D Savsocieta      S                   like(Gcosocieta)
020600960926     D Savdivisa       S                   like(PpaDivisa)
020700960926     D Savlineav       S                   like(Gcolineav)
020800960926     D Savfiliale      S                   like(Gcofiliale)
020900960923     D SAVSogg         S                   like(Gcosogg)
021000960923     D SAVkccestr      S                   like(Gcokccest)
021100960923     D SAVkscestr      S                   like(Gcokscest)
021200960926     D SAVAgente       S                   like(Gcoagente)
021300960926     D SAVkccage       S                   like(Gcokcc)
021400960926     D SAVSoggage      S                   like(Gcosogg)
021500960926     D SAVkccesta      S                   like(Gcokccest)
021600960926     D SAVkscesta      S                   like(Gcokscest)
021700990907     D SAVsaldnat      S                   like(RcosaldNat)
021800960926     D*
021900960926     D* Campi di lavoro
022000991201     d* Ds per salvataggio ds in entrata
022100991201     D ndcg01ds1       s                   like(ndcg01ds)
022200991201     D DtSca           s                   like(DtscaG01)
022300991201     D DtPerDa         s                   like(DtPerDaG01)
022400991201     D DtPerA          s                   like(DtPerAG01)
022500991201     D CtgAn1          s                   like(CtgAn1G01)
022600991201     D CtgAn2          s                   like(CtgAn2G01)
022700991201     D CtgAn3          s                   like(CtgAN3G01)
022800991201     D Tb8Per          s                   like(Tb8PerG01)
022900991201     D DtCam           s                   like(DtCaMG01)
023000991201     D TpCam           s                   like(TpCaMG01)
023100991201     D Movdef          s                   like(MovDefG01)
023200991201     D MovProv         s                   like(MovProvG01)
023300991201     D MovGes          s                   like(MovGesG01)
023400991201     D DtRegD          s                   like(DtRegDG01)
023500991201     D DtRegP          s                   like(DtRegPG01)
023600991201     D DtRegG          s                   like(DtRegGG01)
023700960926     D WrkImpRmc       S                   like(ScdImpRMc)
023800960926     D WrkImpRD        S                   like(ScdImpRD)
023900960926     D WrkGirisc       S                   like(Gvagirisc)
024000960926     D WrkGiinasd      S                   like(GvagiinasD)
024100960926     D WrkGiinasp      S                   like(GvagiinasP)
024200960926     D wrkdtcam        S                   like(dtcamg01)
024300960926     D Wrkimporto      S                   like(Gcoiesposd)
024400960926     D WrkimportS      S                   like(Gcoiesposd)
024500960926     D Wrksaldo        S                   like(Gcoiesposd)
024600960926     D wrkDtpar        S                   like(PPadtpar)
024700960926     D wrknrpar        S                   like(PPanrpar)
024800960926     D wrkserpar       S                   like(PPaserpar)
024900960926     D wrkkcc          S                   like(PPakcc)
025000960926     D wrkksc          S                   like(PPaksc)
025100960926     D WrKopen         S              1
025200070510D2189D WrkRepAgen      S                   like(GvaRepAgen)
025300001027D0747D WrKopenW        S              1
025400000915c1390D SiEsclus        S              1
025500960307     D*-------------
025600960307     D* Parametri per reperimento saldo per e/c a partita
025700960307     D X20SCADDS     E DS                  inz
025800090504>>>>>D  X20FLGTPSC                    1    overlay(x20flg:1)                    Scadenz. retroattivo
025900960307     D* DS parametro esteso se richiesto x20parscd='S'
026000960307     D X20SCADXDS    E DS                  inz
026100960729     D*-------------
026200960729     D* Parametri per calcolo 8 periodi in caso di mancata chiamata
026300960729     D* All'x20scad
026400960729     D X51prd08DS    E DS                  inz
026500960729    >D  x51errori    E                     inz('0')
026600960123     D*-------------
026700960718     D* Schiera per QCMDEXC
026800960715     D CMD             S             80    DIM(1) CTDATA PERRCD(1)              Costanti pgm
026900980929     D*-------------
027000980929     D* flag di NO controllo scostamento cambio/importo
027100980929     D NoCtrlScos      S              1A
027200980929    >D WImpInp         S                   Like(ImporD0202)
027300980929    >D WImpOut         S                   Like(ImporD0202)
027400000117C1309D ZDateISO        S               D
027500980929     D*-------------
027600980929     D* PROTOTIPAZIONE DELLE PROCEDURE
027700981103     D/COPY *LIBL/SRCCPY,PJX002PR
027800980929     D*-------------
027900951212      /EJECT
028000951212     C************************************************************
028100950626     C*                MAIN LINE PROGRAM
028200951212     C************************************************************
028300950626     C* Inizializzazione variabili
028400950626     C                   EXSR      INZVAR
028500960726     C*
028600960729     c* se presenti selezioni particolari, devo leggere PPA e chiamare
028700960729     C* x20scad per partita(se scadenzario è da calcolare)
028800960726     c* se non chieste selezioni particolari chiamo x20scad per il conto
028900960729     c* (e scadenzario è da calcolare)
029000960729     c* poi leggo PPA e file ndcs* per partita
029100960726     C*
029200960726     c                   select
029300960726     C                   when      flgconto = *off
029400960726     C                   eval      ppasocieta = SocietaG01
029500960726     C                   eval      ppactb     = ctbG01
029600960726     C                   eval      ppakcc     = KccG01
029700960726     C                   eval      ppaksc     = KscG01
029800960726     C                   eval      ppadtpar   = DtPerdaG01
029900960307     C                   EXSR      LeggoPPA
030000960910     C*
030100960910     c*  altrimenti posso chiamare X20scadr per conto per velocizzare i
030200960910     c*  calcoli
030300960726     C                   when      flgconto = *on
030400960726     c                   eval      wrkkcc =kccg01
030500960726     c                   eval      wrkksc = kscg01
030600960726     c                   eval      wrkdtpar = datmin
030700960726     c                   eval      wrknrpar = 0
030800960726     c                   eval      wrkserpar = *blanks
030900001027     c*
031000001027D0747C* Flag per sapere se utilizzare file residenti o in qtemp
031100001027D0747c                   eval      flgQTEMP=*off
031200001027     C*
031300000629d0652C* Chiamo routine per il calcolo dello scadenzario solo se NON SIAMO
031400000629 "   C* IN ELABORAZIONE NOTTURNA, in quanto in questo caso viene lanciato
031500000629 "   c* dal programma ANA904R
031600000629d0652c                   If        NotturnG01 <> *on
031700000629     c* se lo scadenzario è da calcolare da parametri di gestione
031800960726   1 C                   if        ClcScad016 = '2'  or
031900991201     c* o se sono stati modificati i dati rispetto all'ulimo calcolo eseg.
032000001107D0747c                             FlgScadenz= *on   or
032100961213     c* o se sono in batch (che è sempre per conto), ma non se è azione
032200961213     C* notturna, perchè in quel caso lo scadenzario è già stato calcolato
032300990906     c                             (interatg01 =*off and notturng01 <> *on)
032400960729     c                   eval      flgx20 = *on
032500001102d0747c                   eval      flgQTEMP=*on
032600960726     C                   exsr      callX20
032700960726   1ec                   endif
032800000629d0652c                   endif
032900960726     c*
033000001027     c* Apro i file o in qtemp se chiamato x20
033100001027D0747c                   if        flgQtemp=*on
033200960726     c                   if        wrkopen = ' '
033300960726     c                   open      ndscp01l
033400960726     c                   open      ndscd01l
033500960726     c                   eval      wrkopen = *on
033600960726     c                   endif
033700001027D0747c                   endif
033800001027     c*
033900001027D0747c* Apro i file residenti solo se necessario
034000001027D0747c                   if        flgQtemp<>*on
034100001027  "  c                   if        wrkopenW= ' '
034200001027  "  c                   open      ndscp01W
034300001027  "  c                   open      ndscd01W
034400001027  "  c                   eval      wrkopenW= *on
034500001027  "  c                   endif
034600001027D0747c                   endif
034700960726     c*
034800960729     c* Poi leggo ppa
034900960729     C                   eval      ppasocieta = SocietaG01
035000960729     C                   eval      ppactb     = ctbG01
035100960729     C                   eval      ppakcc     = kccg01
035200960729     C                   eval      ppaksc     = kscg01
035300960729     C                   eval      ppadtpar   = DtPerdaG01
035400960729     C                   EXSR      LeggoPPA
035500960726     c*
035600960726     c                   endsl
035700950626     C*
035800950626     C* Fine programma
035900950626     C                   EXSR      ENDPGM
036000950626     C*
036100960117      /EJECT
036200960117     C************************************************************
036300960726     C* Lettura NDPPA  (lettura per kcc/ksc)
036400960117     C************************************************************
036500960307     C     LeggoPPA      BEGSR
036600960123     C*
036700960926     c* Reperisco i dati del cliente da driver agrafica: AGENTE
036800960923     C* da ndmvc002 reperisco sogg, kccestr e kscestr
036900960731     C*
037000960926     C                   exsr      DatiConto
037100960731     C*
037200960729     c* Imposto inizializzazione il flag della routine X51PRD08
037300960729     C* ad *on la prima volta che viene chiamato per conto per pulire gli
037400960729     c* 8 Periodi, dopo la prima chiamata lo imposto ad *off
037500960729     c*
037600960729     C                   move      *on           x51iniz
037700961007     C* pulisco gli 8 periodi nel caso non richiami il programma
037800961007     c                   clear                   x51prd01
037900961007     c                   clear                   x51prd02
038000961007     c                   clear                   x51prd03
038100961007     c                   clear                   x51prd04
038200961007     c                   clear                   x51prd05
038300961007     c                   clear                   x51prd06
038400961007     c                   clear                   x51prd07
038500961007     c                   clear                   x51prd08
038600961007     c                   clear                   x51prd01D
038700961007     c                   clear                   x51prd02D
038800961007     c                   clear                   x51prd03D
038900961007     c                   clear                   x51prd04D
039000961007     c                   clear                   x51prd05D
039100961007     c                   clear                   x51prd06D
039200961007     c                   clear                   x51prd07D
039300961007     c                   clear                   x51prd08D
039400960729     C                   eval      x51Tabcf   = Tb8perG01
039500960729     C*
039600960729     C*posizionamento iniziale con 5 chiavi:
039700960729     C*soc
039800960729     C*ctb
039900960729     C*kcc
040000960729     C*ksc
040100960729     C*dtpar
040200960627     c*
040300960627     C     K05PPA01      SETLL     NDPPA01L
040400960627     C     K04PPA01      READE     NDPPA01L                               21
040500950626     C*
040600960117   1 C     *in21         DOWEQ     *off
040700960104     C*
040800960418   2 C                   DO
040900960418     C*
041000960627     C* se supera la data limite partita superiore
041100960708   3 C                   IF        PPADtPar > DtPerAG01
041200960627     C     *hival        setll     NDPPA01L
041300001103D0747c                   leave
041400960627   4eC                   ENDIF
041500960508     C*
041600960508     C* se unità diversa non considero il record
041700960708   3 C                   IF        unitaG01 <> *blanks and ppaunita <> unitaG01
041800960508     C                   leave
041900960508   3eC                   ENDIF
042000960514     C*
042100960514     C* se data partita o numero partita = 0 non considero il record
042200960514   3 C                   IF        ppadtpar = datmin or ppanrpar = 0
042300960514     C                   leave
042400960514   3eC                   ENDIF
042500960627     C*
042600960627     C* se divisa diversa da quella richiesta non considero il record
042700960729   3 C                   IF        (divisaG01 <> *blanks and
042800960729     c                             divselG01 = 'I' and ppadivisa <> divisaG01)
042900960627     C                             or
043000960729     c                             (divselG01 = 'O' and ppadivisa =  divisaG01)
043100960627     C                   leave
043200960627   3eC                   ENDIF
043300000915c1390c* se presente file esclusione partite sulla versione, richiama
043400000915  "  c* programma validazione regola
043500000915  "  c                   if        SiEsclus = *on
043600000915  "  c                   exsr      TestaEscl
043700000915  "  c* se esistono delle regole soddisfatte, il record (la partita) è
043800000915  "  c* da escludere
043900000915  "  c                   if        Rok960 = *on
044000000915  "  c                   leave
044100000915  "  c                   endif
044200000915c1390c                   endif
044300960627     C*
044400960731     C* Aggancio NDPPS per altri test se richiesti i dati in chiave o
044500000915     c* se chieste selezioni sui dati
044600960628     C                   eval      flgpps = *off
044700070510D2189X*************      if        kscageG01<> *blanks and RepAgen016 = '1'
044800070510D2189c                   if        kscageG01<> *blanks and WrkRepAgen = '1'
044900990906     c                             and snaturaG01 = 'C'
045000990906     C                             or  ctgan1g01 <> *blanks
045100960731     C                             or  ctgan2g01 <> *blanks
045200960731     C                             or  ctgan3g01 <> *blanks
045300070510D2189X*************                or (gvaAgente1 = *on and repagen016 ='1'
045400070510D2189C                             or (gvaAgente1 = *on and wrkRepAgen ='1'
045500990906     c                             and snaturaG01 = 'C')
045600960731     c                             or  GvaCtgAn11 = *on
045700960731     c                             or  GvaCtgAn21 = *on
045800960731     c                             or  GvaCtgAn31 = *on
045900960627     c                   eval      ppssys = ppasys
046000960627     c                   eval      ppsnrasint = ppanrasint
046100960627     c     k02pps01      CHAIN     NDPPS01L                           21
046200960627   3 C                   if        *in21 = *off
046300960628     C                   eval      flgpps = *on
046400960627     c*
046500960708   4 C                   IF        kscageG01 <> *blanks and ppSKSCAGE <>
046600070510D2189X***                          kscageG01 and repagen016 = '1'
046700070510D2189c                             kscageG01 and WrkRepAgen = '1'
046800960627     C                   leave
046900960627   4eC                   ENDIF
047000960708   4 C                   IF        CTGAN1G01 <> *blanks and PpsCtgana1<>
047100960723     c                             ctgan1G01
047200960627     C                   leave
047300960627   4eC                   ENDIF
047400960708   4 C                   IF        CTGAN2G01 <> *blanks and PpsCtgana2<>
047500960708     c                             ctgan2G01
047600960627     C                   leave
047700960627   4eC                   ENDIF
047800960708   4 C                   IF        CTGAN3G01 <> *blanks and PpsCtgana3<>
047900960708     c                             ctgan3G01
048000960627     C                   leave
048100960627   4eC                   ENDIF
048200960627     c*
048300960627   3eC                   ENDIF
048400960731  3bec                   ENDIF
048500960726     C*
048600960726     c                   eval      wrkkcc = ppakcc
048700960726     c                   eval      wrkksc = ppaksc
048800960726     c                   eval      wrkdtpar = ppadtpar
048900960726     c                   eval      wrknrpar = ppanrpar
049000960726     c                   eval      wrkserpar = ppaserpar
049100960117     C*
049200001106     C* Chiamo routine per ottenere il saldo solo se
049300001106     c* il saldo è da calcolare da parametri di gestione e se non l'ho
049400001106     c* già chiamato per il conto
049500001222D0802C* Chiamo routine per il calcolo dello scadenzario solo se NON SIAMO
049600001222 "   C* IN ELABORAZIONE NOTTURNA, in quanto in questo caso viene lanciato
049700001222 "   c* dal programma ANA904R
049800001222D0802c                   If        NotturnG01 <> *on
049900001222   2 c                   if        flgx20 <> *on
050000001106     C*
050100001106D0747C* Flag per sapere se utilizzare file residenti o in qtemp
050200001106D0747c                   eval      flgQTEMP=*off
050300001222   3 C                   if        ClcScad016 = '2'  or
050400001106D0747c* o se sono stati chiesti dei parametri particolari
050500001106  "  c**                           MovdefG01 <> *on          or
050600001106  "  c**                           MovprovG01<> *on          or
050700001106  "  c**                           MovgesG01 <> *on          or
050800001106  "  c**                           dtregdG01 <> udateiso     or
050900001106  "  c**                           (DtRegPg01 <> '2039-12-31' and
051000001106  "  c**                           DtRegPg01 <> '9999-12-31')  or
051100001106  "  c**                           DtRegGg01 <> udateiso
051200001106  "  c* o se sono stati modificati i dati rispetto all'ulimo calcolo eseg.
051300001107D0747c                             FlgScadenz= *on
051400960627     C* lo chiamo però solo se
051500960627     c* la partita NON è chiusa (se chiusa i dati sono a 0)
051600001222   4 C                   if        (Ppaparchiu <> '1'
051700001027     c                             or  ppadtchiu > dtregdG01)
051800960729     c                   eval      flgx20 = *on
051900001102D0747c                   eval      flgQtemp=*on
052000960726     C                   exsr      callX20
052100001222   5 C                   if        x20errori <>' '
052200960307     C                   leave
052300001222   5Ec                   endif
052400001222   4 c                   endif
052500001222   3Ec                   endif
052600001222   2Ec                   endif
052700001222D0802c                   endif
052800960716     c*
052900001027     c* Apro i file in qtemp se chiamato x20
053000001027D0747c                   if        flgQtemp=*on
053100001027     c                   if        wrkopen = ' '
053200001027     c                   open      ndscp01l
053300001027     c                   open      ndscd01l
053400001027     c                   eval      wrkopen = *on
053500001027     c                   endif
053600001027D0747c                   endif
053700001027     c*
053800001027D0747c* Apro i file residenti solo se necessario
053900001027D0747c                   if        flgQtemp<>*on
054000001027  "  c                   if        wrkopenW= ' '
054100001027  "  c                   open      ndscp01W
054200001027  "  c                   open      ndscd01W
054300001027  "  c                   eval      wrkopenW= *on
054400001027  "  c                   endif
054500001027D0747c                   endif
054600990114     C*
054700990114     c* Reperisco agente anagrafico utilizzando unità di partita e solo se
054800990114     C* in anagrafica società è stabilito che unità = linea di vendita
054900990114     C*
055000990114   1 C                   if        SnaturaG01 = 'C' and Fgo5 = *on
055100990114     C                   exsr      DatiConto1
055200990114     c                   endif
055300960307     c*
055400960627     C* se arrivo qui è ok e scrivo file NDGCO00F
055500960722     c* se chiesta almeno una entità, eseguo un loop di lettura
055600960722     c* su file NDMIX per impostare le chiavi e suddividere gli
055700960722     c* importi in base alle percentuali, altrimenti non la faccio
055800960722     c* ed eseguo subito la routine che scrive NDGCO
055900960722     C*
056000990906     c                   if        (SnaturaG01 = 'C' or SnaturaG01 ='F') and
056100990608     c                             (GvaEntit11 = *on or
056200960722     c                             GvaEntit21 = *on or
056300960722     c                             GvaEntit31 = *on or
056400960722     c                             GvaEntit41 = *on or
056500960722     c                             GvaEntit51 = *on or
056600960722     c                             GvaEntit61 = *on or
056700960722     c                             GvaEntit71 = *on or
056800990608     c                             GvaEntit81 = *on)
056900960722     C                   eval      flgMix = *On
057000960722     c                   exsr      LeggoMix
057100960722     C                   else
057200960722     C                   eval      flgMix = *Off
057300960725     c                   exsr      LeggoGco
057400960726     c                   exsr      EXECalcoli
057500960730     C                   move      *on           x51iniz
057600960725     c                   exsr      WriteGco
057700960722     c                   endif
057800960730     c*
057900960730     c* spengo flag di x20 solo se chiamata per partita (se chiamata per con
058000960730     c* deve rimanere acceso per non chiamarlo anche per partita
058100960730     c                   if        flgconto = *off
058200960730     c                   eval      flgx20 = *off
058300960730     c                   endif
058400960307     C*
058500960307   2eC                   ENDDO
058600960311     C*
058700960627     C     K04PPA01      READE     NDPPA01L                               21
058800960117     C*
058900960307   1eC                   ENDDO
059000960712      *
059100960123     C*
059200950626     C                   ENDSR
059300960307     C/EJECT
059400960307     C************************************************************
059500960307     C* RIEMPIMENTO DS PER saldo conto
059600960307     C************************************************************
059700960726     C     callX20       BEGSR
059800960729     c*
059900960307     C                   RESET                   x20scadds
060000960307     C                   RESET                   x20scadxds
060100960918     C                   eval      x20societa = societag01
060200960708     C                   eval      x20unita   = unitaG01
060300960708     C                   eval      x20ctb     = ctbG01
060400960708    >C                   eval      x20def     = movdefG01
060500960708    >C                   eval      x20prov    = movprovG01
060600960708    >C                   eval      x20gest    = movgesG01
060700960726    >C                   eval      x20kcc     = wrkkcc
060800960726    >C                   eval      x20ksc     = wrkksc
060900960710    >C                   eval      x20snatura = SnaturaG01
061000960726    >C                   eval      x20dtpar   = wrkdtpar
061100960726    >C                   eval      x20nrpar   = wrknrpar
061200960726    >C                   eval      x20serpar  = wrkserpar
061300960729     c                   if        divselG01 = 'I'
061400960729     C                   eval      x20divr    = divisaG01
061500960729     c                   endif
061600960708     C                   eval      x20dtregmd = DtregDG01
061700960708     C                   eval      x20dtregmp = DtregPG01
061800960708     C                   eval      x20dtregmg = DtregGG01
061900960307     C                   eval      x20parsca  = 'S'
062000960307     C                   eval      x20elasca  = 'R'
062100960708     C                   eval      x20DtSca   = DtScaG01
062200090504>>>>>C* Imposto scadenzario retroattivo.
062300090504>>>>>C                   if        dtregdg01 < udateiso
062400090504>>>>>C                             or dtscag01 < udateiso
062500090504>>>>>C                   eval      x20flgtpsc='1'
062600090504>>>>>c                   else
062700090504>>>>>C                   eval      x20flgtpsc='0'
062800090504>>>>>C                   endif
062900960726     C*
063000960702     c* se tabella non richiesta, viene utilizzata quella dei parametri di
063100960702     c* gestione
063200960708     C                   eval      x20Tabcf   = Tb8perG01
063300960729     C                   eval      x20ppa     = 'S'
063400960307     C                   eval      x20lrt     = 'R'
063500981222     C*
063600981222     C* Calcolo lo scadenzario tenendo conto del flag
063700981222     C* "Causale esclusa da scadenzario".
063800020917D1199C* Certo, sarebbe meglio reperirlo dal codice versione (se ci fosse)
063900020917  "  C* ma per adesso va bene così.
064000020917  "  c* invece che metterlo nel codice versione è stato deciso di repe-
064100020917  "  c* rirlo dai parametri di calcolo scadenzario del capoconto
064200020917  "  C* lettura per società/conto
064300020917  "  c* Se DATA LIMITE < OGGI, è tipo scadenzario RETROATTIVO, altrimenti
064400020917  "  C* è tipo scadenzario ATTUALE
064500020924  "  c                   if        wrkkcc<> Savkcc
064600020924  "  C                   EVAL      SavKcc= wrkkcc
064700020917  "  C                   eval      Cscsocieta = societag01
064800020917  "  C                   if        DTRegDG01< udateiso
064900020917  "  C                   eval      CscTpScad  ='1'
065000020917  "  c                   else
065100020917  "  C                   eval      CscTpScad  ='0'
065200020917  "  C                   endif
065300020917  "  C                   eval      CscKcc     = wrkkcc
065400020917  "  C     K03Csc01      CHAIN     NDCSC01L                           21
065500020924  "  c                   else
065600020924  "  c* va bene l'ultimo chain fatto
065700020924  "  c                   setoff                                       21
065800020924  "  c                   endif
065900020917  "  C                   if        *in21=*off
066000020917  "  C                             and CSCAnn <> 'A'
066100020917  "  C                   eval      x20cautec=cscesclcau
066200020917D1199C                   else
066300020917     C                   eval      x20cautec = 'E'
066400020917D1199c                   endif
066500960726     C*
066600960726     C* chiama X20( se interattivo, gli dico di creare i file in qtemp)
066700000119     c* DEVE LAVORARE SEMPRE IN QTEMP, ANCHE SE BATCH
066800000119  2  c*****              if        interatg01 = *on
066900960726     C                   eval      x20Write   = 'T'
067000960726     C                   eval      x20crtPF   = 'Y'
067100960726     C                   CALL      'X20SCADC'
067200960726     C                   PARM                    X20SCADDS
067300000119     c***                else
067400000119     C***                eval      x20Write   = 'R'
067500000119     C***                eval      x20crtPF   = ' '
067600000119     C***                CALL      'X20SCADR'
067700000119     C***                PARM                    X20SCADDS
067800000119   2eC***                endif
067900960729     C*
068000960307     C*
068100960307     C                   ENDSR
068200960726     c*
068300960726     C/EJECT
068400960726     C************************************************************
068500960726     C* chiude i file aperti da X20
068600960726     C************************************************************
068700960726     C     ChiudeX20     BEGSR
068800960726     c*
068900960726     C                   RESET                   x20scadds
069000960726     C                   RESET                   x20scadxds
069100960726     C                   eval      x20dtregmd = Datmin
069200960726     C                   eval      x20dtregmp = Datmin
069300960726     C                   eval      x20dtregmg = Datmin
069400960726     C                   eval      x20DtSca   = Datmin
069500960726     C                   eval      x20lrt     = 'L'
069600960726     C                   CALL      'X20SCADR'
069700960726     C                   PARM                    X20SCADDS
069800001027D0747c                   if        wrkopen = *on
069900960726     c                   close     ndscp01l
070000960726     c                   close     ndscd01l
070100001027     c                   clear                   wrkopen
070200001027D0747c                   endif
070300960726     c*
070400960726      * Cancellazione archivio NDSCP e ndscd in qtemp se creati
070500001107     c* il saldo è da calcolare da parametri di gestione solo se
070600001107     c* siamo in interattivo
070700001107     C                   if        ClcScad016 = '2'  or
070800960903     c* o se sono stati chiesti dei parametri particolari
070900001107D0747c**                           MovdefG01 <> *on          or
071000001107  "  c**                           MovprovG01<> *on          or
071100001107  "  c**                           MovgesG01 <> *on          or
071200001107  "  c**                           dtregdG01 <> udateiso     or
071300001107  "  c**                           (DtRegPg01 <> '2039-12-31' and
071400001107  "  c**                           DtRegPg01 <> '9999-12-31') or
071500001107  "  c**                           DtRegGg01 <> udateiso     or
071600001107D0747c                             FlgScadenz= *on
071700960726     C                   call      'QCMDEXC'                            25
071800960726     C                   parm                    cmd
071900960726     C                   parm      80            len              15 5
072000960726   1Ec                   endif
072100960726     C*
072200960726     C                   ENDSR
072300960722      /EJECT
072400960722     C************************************************************
072500960722     C* Lettura NDMIX se chiesta almeno una entità
072600960722     C* Leggo tutti i record con la stessa data di validità
072700960722     c* precedente alla data di registrazione definitiva immessa
072800960722     c* a video, eseguo i calcoli una sola volta e poi suddivido
072900960722     c* i dati ricavati per le percentuali del mix
073000960722     C* arrotondando sull'ultimo record
073100960722     C************************************************************
073200960722     C     LeggoMix      BEGSR
073300960723     C*
073400960723     c* Pulisco i campi che servono per fare l'arrotondamento
073500960723     c* in caso di spaccatura di MIX
073600960723     C                   IF        Flgmix = *on
073700960723     c* Virtuali (totali virtuali della partita, da pulire ad inizio di
073800960723     c* ogni partita)
073900960905     c                   clear                   VirIespos        19 3
074000960905     c                   clear                   VirIesposD       19 3
074100960905     c                   clear                   VirIrisc         19 3
074200960905     c                   clear                   VirIriscD        19 3
074300960723     c                   clear                   VirIsaldo        19 3
074400960723     c                   clear                   VirIsainv        19 3
074500960723     c                   clear                   VirIsaldoD       19 3
074600960723     c                   clear                   VirIsainvD       19 3
074700960723     c                   clear                   VirIeffar        19 3
074800960723     c                   clear                   VirIeffarD       19 3
074900960723     c                   clear                   VirItitar        19 3
075000960723     c                   clear                   VirItitarD       19 3
075100960723     c                   clear                   VirIeffas        19 3
075200960723     c                   clear                   VirIeffasD       19 3
075300960723     c                   clear                   VirIinasp        19 3
075400960723     c                   clear                   VirIinaspD       19 3
075500960723     c                   clear                   VirIinasd        19 3
075600960723     c                   clear                   VirIinasdD       19 3
075700960723     c                   clear                   VirIscato        19 3
075800960723     c                   clear                   VirIscatoD       19 3
075900960723     c                   clear                   VirIscare        19 3
076000960723     c                   clear                   VirIscareD       19 3
076100970404     c                   clear                   VirIscain        19 3
076200970404     c                   clear                   VirIscainD       19 3
076300960730     c                   clear                   VirIsca0
076400960730     c                   clear                   VirIsca0D
076500960723     c                   endif
076600960722     C*
076700960722     C* Per una partita leggo i suoi record di mix
076800960722     c* pulisco i campi
076900960722     c                   eval      mixsys = ppasys
077000960722     c                   eval      mixnrasint = ppanrasint
077100960722     c                   eval      mixdtval   = dtregdG01
077200990609     C     K03MIX01      SETGT     NDMIX01L
077300960730     c     k02mix01      READPE    NDMIX01L                               43
077400960730     C                   if        *in43 = *off
077500960722     c                   move      MIXDTVAL      SAVDTVAL
077600960722     c                   else
077700960722     c* SE non c'è neanche un record di mix, ed era stato chiesto, torno
077800960722     c* Alla situazione di nessun mix richiesto
077900960722     C                   eval      flgMix = *Off
078000960725     c                   exsr      LeggoGco
078100960726     c                   exsr      EXECalcoli
078200960730     C                   move      *on           x51iniz
078300960725     c                   exsr      WriteGco
078400960722     c                   endif
078500960722     c*
078600960730   1 C     *IN43         doweq     *off
078700960722     c     mixdtval      andeq     savdtval
078800960722     c*
078900960725     c                   exsr      LeggoGco
079000960726     c                   exsr      EXECalcoli
079100960730     C                   move      *on           x51iniz
079200960725     c                   exsr      WriteGco
079300960722     C*
079400960730     c     k02mix01      READPE    NDMIX01L                               43
079500960722     C                   ENDDO
079600960722     C*
079700960730     c* aggancio ultimo record di NDGCO scritto ed eseguo
079800960730     c* e quadratura se  presente almeno un mix
079900960730     c                   if        flgmix = *on
080000960725     c                   exsr      LeggoGco
080100960725     C                   EXSR      QuadraGco
080200960730     c                   exsr      WriteGco
080300960725     c                   endif
080400960722     C*
080500960722     C                   ENDSR
080600960722      /EJECT
080700960722      ************************************************************
080800960722      * Lettura file dei dati contabili
080900960722      ************************************************************
081000960725     C     LeggoGco      BEGSR
081100960722     c*
081200960722     C* composizione chiave secondo la versione da utilizzare
081300960722     C                   EXSR      ComponeKey
081400960722     c*
081500961216     c     K23Gco01      chain     NDgCo01L                           44
081600960730     C* Se trovato e se azione diversa, pulisco tutti i campi e aggiorno
081700960730     C* Se NON trovato                  pulisco tutti i campi e scrivo
081800960730     C* Se trovato e se azione uguale,  aggiorno
081900960722  1  c                   if        *in44 = *off and
082000960730     c                             knraz <> gconraz  or
082100960722     c                             *in44 = *on
082200960730     c                   exsr      PulizCampi
082300960722  1e c                   endif
082400960722     c*
082500960722     c                   endsr
082600960730      /EJECT
082700960730      ************************************************************
082800960730      * Pulizia campi che devono essere puliti
082900960730      ************************************************************
083000960730     C     PulizCampi    BEGSR
083100960730     c*
083200960730     c                   clear                   gcoann
083300960730     c                   clear                   gcoIespos
083400960730     c                   clear                   gcoIscato
083500960730     c                   clear                   gcoIscare
083600970408     c                   clear                   gcoIscain
083700960730     c                   clear                   gcoIrisc
083800960730     c                   clear                   gcoIeffas
083900960730     c                   clear                   gcoIinasp
084000960730     c                   clear                   gcoIinasd
084100960730     c                   clear                   gcoIeffar
084200960730     c                   clear                   gcoItitar
084300960730     c                   clear                   gcoIsaldo
084400960730     c                   clear                   gcoIsainv
084500960731     c                   clear                   gcoIdcnc
084600960730     c*
084700960730     c                   clear                   gcoIesposD
084800960730     c                   clear                   gcoIscatoD
084900960730     c                   clear                   gcoIscareD
085000970408     c                   clear                   gcoIscainD
085100960730     c                   clear                   gcoIriscD
085200960730     c                   clear                   gcoIeffasD
085300960730     c                   clear                   gcoIinaspD
085400960730     c                   clear                   gcoIinasdD
085500960730     c                   clear                   gcoIeffarD
085600960730     c                   clear                   gcoItitarD
085700960730     c                   clear                   gcoIsaldoD
085800960730     c                   clear                   gcoIsainvD
085900960730     C*
086000960730     c                   clear                   gcoIesposS
086100960730     c                   clear                   gcoIscatoS
086200960730     c                   clear                   gcoIscareS
086300970408     c                   clear                   gcoIscainS
086400960730     c                   clear                   gcoIriscS
086500960730     c                   clear                   gcoIeffasS
086600960730     c                   clear                   gcoIinaspS
086700960730     c                   clear                   gcoIinasdS
086800960730     c                   clear                   gcoIeffarS
086900960730     c                   clear                   gcoItitarS
087000960730     c                   clear                   gcoIsaldoS
087100960730     c                   clear                   gcoIsainvS
087200960730     c*
087300960730     c                   clear                   gcoIsca01
087400960730     c                   clear                   gcoIsca02
087500960730     c                   clear                   gcoIsca03
087600960730     c                   clear                   gcoIsca04
087700960730     c                   clear                   gcoIsca05
087800960730     c                   clear                   gcoIsca06
087900960730     c                   clear                   gcoIsca07
088000960730     c                   clear                   gcoIsca08
088100960730     c                   clear                   gcoIsca01D
088200960730     c                   clear                   gcoIsca02D
088300960730     c                   clear                   gcoIsca03D
088400960730     c                   clear                   gcoIsca04D
088500960730     c                   clear                   gcoIsca05D
088600960730     c                   clear                   gcoIsca06D
088700960730     c                   clear                   gcoIsca07D
088800960730     c                   clear                   gcoIsca08D
088900960730     c                   clear                   gcoIsca01S
089000960730     c                   clear                   gcoIsca02S
089100960730     c                   clear                   gcoIsca03S
089200960730     c                   clear                   gcoIsca04S
089300960730     c                   clear                   gcoIsca05S
089400960730     c                   clear                   gcoIsca06S
089500960730     c                   clear                   gcoIsca07S
089600960730     c                   clear                   gcoIsca08S
089700960730     c*
089800960923     c                   clear                   gcosogg
089900960923     c                   clear                   gcokccest
090000960923     c                   clear                   gcokscest
090100960923     c                   clear                   gcosoggage
090200960923     c                   clear                   gcokccesta
090300960923     c                   clear                   gcokscesta
090400960923     c                   clear                   gcokccage
090500960923     c                   clear                   gcoesercS
090600960923     c                   clear                   gcoeseprS
090700960923     c                   clear                   gcoperiodo
090800960923     c                   clear                   gcoesercC
090900960923     c                   clear                   gcoeseprC
091000960923     c                   clear                   gcolineav
091100960923     c                   clear                   gcofiliale
091200960923     c*
091300960730     C                   ENDSR
091400960730      /EJECT
091500960628      ************************************************************
091600960628      * Scrittura file dei dati contabili
091700960628      ************************************************************
091800960725     C     WriteGco      BEGSR
091900960628     c*
092000961212     C                   TIME                    ORA               6 0
092100960731     c*
092200960731     C* Imposto data / ora / utente immissione se record NON
092300960731     c* trovato o se trovato e azione DIVERSA
092400960731     c*
092500960731  1  c                   if        *in44 = *off and
092600960731     c                             knraz <> gconraz  or
092700960731     c                             *in44 = *on
092800960702     c                   eval      gcodtimm = udateiso
092900960702     c                   eval      gcouteimm = xsccut
093000960708     c                   eval      gcooraimm  = ora
093100960731     c                   endif
093200960731     c*
093300960731     c* in tutti i casi data, ora, utente modifica vanno messi sempre
093400960731     c*
093500960731     c                   eval      gconraz = knraz
093600960731     c                   eval      gcodtulmo = udateiso
093700960731     c                   eval      gcouteulmo = xsccut
093800960731     c                   eval      gcooraulmo = ora
093900960923     c* altri dati del conto
094000960923     c                   eval      gcosogg = savsogg
094100960923     c                   eval      gcokccest = savkccestr
094200960923     c                   eval      gcokscest = savkscestr
094300960926     C*
094400960926     c* dati dell'agente (li reperisco solo al cambio di agente)
094500960926     C*
094600961125     c                   if        GcoAgente =  *blanks
094700961125     c                   clear                   savkccage
094800961125     c                   clear                   savsoggage
094900961125     c                   clear                   savkccesta
095000961125     c                   clear                   savkscesta
095100961125     c                   endif
095200961125     c                   if        GcoAgente <> *blanks
095300960923     c                   exsr      DatiAgente
095400960923     c                   endif
095500960926     c                   eval      gcosoggage = savsoggage
095600960926     c                   eval      gcokccage  = savkccage
095700960926     c                   eval      gcokccesta = savkccesta
095800960926     c                   eval      gcokscesta = savkscesta
095900960926     C*
096000960923     c* dati esercizio contabile e statistico
096100960923     C                   eval      gcoesercC  = x10eserciz
096200960923     C                   eval      gcoeseprC  = x10eseprog
096300960923     C                   eval      gcoesercS  = x50eserciz
096400960923     C                   eval      gcoeseprS  = x50eseprog
096500990607     C*****              MOVE      x50statper    gcoperiodo
096600990607     C                   MOVE      WrkPeriodo    gcoperiodo
096700960926     C*
096800960923     c* dati linea di vendita e filiale
096900960926     c* Se nei parametri della società si stabilisce che ldv e Unita
097000960926     C* coincidono, metto nel campo lineav l'unità
097100960926     c                   if        Fgo5 = *on
097200960926     C                   eval      gcolineav  = Gcounita
097300960926     c                   else
097400960923     C                   eval      gcolineav  = savlineav
097500960926     c                   endif
097600960923     C                   eval      gcofiliale = savfiliale
097700960731     c*
097800960731     c                   if        *in44 = *on
097900960708     c                   write     NDgco01
098000960628     C                   ELSE
098100960708     c                   update    NDgco01
098200960628     C                   endif
098300960628     C*
098400960628     c                   ENDSR
098500960923      /EJECT
098600960923      ************************************************************
098700960923      * Dati dell'agente
098800960923      ************************************************************
098900960923     C     DatiAgente    BEGSR
099000960923     c*
099100960926     c                   clear                   SavKccAge
099200960926     c                   clear                   SavSoggage
099300960926     c                   clear                   SavKccEsta
099400960926     c                   clear                   SavKscEsta
099500960926     c*
099600960923     c* reperisco capoconto
099700960923     C                   CALLB     'XCAPCLIFOR'
099800960923     C                   PARM                    societag01
099900960923     C                   PARM      'F     '      xcapkcc           6
100000960923     C                   PARM                    GcoAgente
100100960923     C*
100200960926     c                   move      xcapkcc       SavKccAge
100300960923     C*
100400981026     C                   reset                   ANRCO
100500981026     C                   EVAL      LenOut = %Size(ANRCO)
100600960923     C                   eval      Data = Datmin
100700960923     C                   CallB     'NDMVC002'
100800960923     C                   PARM                    SocietaG01
100900960926     C                   PARM                    savkccage
101000960923     C                   PARM                    gcoagente
101100960923     C                   PARM                    Data
101200960923     C                   PARM      *OFF          GesMsg
101300960923     C                   PARM      *OFF          Esito
101400981026     C                   PARM      'ANRCO'       StrutturaO
101500981026     C                   PARM                    anrco
101600960923     C                   PARM                    LenOut
101700960923   B1C     Esito         IFEQ      *Off
101800981026     c                   eval      Savsoggage = rcosogg
101900981026     c                   eval      Savkccesta = rcokccestr
102000981026     c                   eval      Savkscesta = rcokscestr
102100960923     c                   endif
102200960923     c*
102300960923     c                   ENDSR
102400960708      /EJECT
102500960708      ************************************************************
102600960904      * legge tutti i record di NDGCO con azione differente
102700960904      * per la stessa sottonatura o capoconto
102800960708      ************************************************************
102900960708     C     FleggaGCO     BEGSR
103000960708     c*
103100010608D0946c                   move      SocietaG01    GcoSocieta
103200961216     c                   move      cdversG01     GcoCdVers
103300960904     c                   move      ctbg01        GcoCtb
103400960904     c                   move      snaturaG01    GcoSnatura
103500960904     c                   move      kccg01        GcoKcc
103600960924     c                   move      kscg01        GcoKsc
103700961216     c     K06gco04      setll     ndgco04l
103800960708     c                   DO        *hival
103900961216     c     k06gco04      reade     ndgco04l                               21
104000960708     c   21              leave
104100960709     c*
104200960708     c                   if        gconraz <> knraz
104300960708     c                   eval      gcoann = 'A'
104400960709     c                   update    ndgco04
104500960708     c                   endif
104600960709     c*
104700960709     C* se azione uguale eseguo setgt per saltare i record della elabora
104800960709     c* zione in corso che vanno bene
104900960709     c                   if        gconraz =  knraz
105000961216     C     K07gco04      setgt     ndgco04l
105100960709     c                   endif
105200960709     C*
105300960708     c                   enddo
105400960708     c*
105500960708     c                   endsr
105600960628      /EJECT
105700960628      ************************************************************
105800960628      * composizione chiave versione
105900960628      ************************************************************
106000960722     C     ComponeKey    BEGSR
106100960722     C*
106200960722     c* a seconda di quanto stabilito nel livello di sintesi dei dati
106300960722     C*
106400961216     c                   eval      GcoCdVers  = CdVersG01
106500961216     c*
106600960628     c                   if        GvaSociet1 = *on
106700960726     c                   eval      GCosocieta = SocietaG01
106800960628     c                   endif
106900960628     c                   if        GvaCtb1    = *on
107000960726     c                   eval      GcoCtb     = CtbG01
107100960628     c                   endif
107200960628     c                   if        GvaSnatur1 = *on
107300960710     c                   eval      Gcosnatura = SnaturaG01
107400960628     c                   endif
107500960628     c                   if        GvaKcc1    = *on
107600960726     c                   eval      GcoKcc     = WRKKcc
107700960628     c                   endif
107800960628     c                   if        Gvaksc1    = *on
107900960726     c                   eval      GcoKsc     = WRKKsc
108000960628     c                   endif
108100960628     c                   if        GvaUnita1  = *on
108200960731     c                   eval      GcoUnita   = Ppaunita
108300960628     c                   endif
108400960628     c                   if        GvaDivisa1 = *on
108500960729     c                   eval      GcoDivisa  = ppadivisa
108600960628     c                   endif
108700960722     c                   move      datmin        gcodtpar
108800960628     c                   if        GvaPartit1 = *on
108900960726     c                   eval      GcoDtPar   = PPadtPar
109000960726     c                   eval      GcoNrPar   = ppaNrPar
109100960726     c                   eval      GcoSerPar  = ppaSerPar
109200960628     c                   endif
109300960628     C*
109400960628     c*
109500960628     c* Dati da NDPPS se era stato trovato
109600070511D2189C                   clear                   GcoAgente
109700070511  "  C                   clear                   GcoCtgAn01
109800070511  "  C                   clear                   GcoCtgAn02
109900070511D2189C                   clear                   GcoCtgAn03
110000960628   2 c                   if        flgPPS = *on
110100070510D2189X***************    if        GvaAgente1 = *on and repagen016 = '1'
110200070510D2189c                   if        GvaAgente1 = *on and WrkRepAgen = '1'
110300960712     c                   eval      Gcoagente  = PpsKscAge
110400960628     c                   endif
110500960628     c                   if        GvaCtgAn11 = *on
110600960628     c                   eval      Gcoctgan01 = PpsCtgAna1
110700960628     c                   endif
110800960628     c                   if        GvaCtgAn21 = *on
110900960628     c                   eval      GcoCtgan02 = PpsCtgAna2
111000960628     c                   endif
111100960628     c                   if        GvaCtgAn31 = *on
111200960628     c                   eval      GcoCtgAn03 = PpsCtgAna3
111300960628     c                   endif
111400960628   2ec                   endif
111500960723     c*
111600960729     c* se agente da anagrafica
111700070510D2189X**************     if        GvaAgente1 = *on and repagen016 = '2'
111800070510D2189c                   if        GvaAgente1 = *on and WrkRepAgen = '2'
111900960723     C                   EVAL      GcoAgente = SavAgente
112000960729     c                   endif
112100960722     C*
112200960911     c* Dati da NDMIX o blank se non gestito MIX
112300960923     c* intanto salvo i valori delle entità LDV e fil
112400960923     C                   EXSR      DatiMIx
112500960628     C*
112600960628     c                   ENDSR
112700960923      /EJECT
112800960923      ************************************************************
112900960923      * Reperisco entità da mix
113000960923      ************************************************************
113100960923     C     DatiMix       BEGSR
113200960923     c*
113300960923     c                   clear                   savlineav
113400960923     c                   clear                   savfiliale
113500960925     c                   eval      GcoEntit1 = *blanks
113600960925     c                   eval      GcoEntit2 = *blanks
113700960925     c                   eval      GcoEntit3 = *blanks
113800960925     c                   eval      GcoEntit4 = *blanks
113900960925     c                   eval      GcoEntit5 = *blanks
114000960925     c                   eval      GcoEntit6 = *blanks
114100960925     c                   eval      GcoEntit7 = *blanks
114200960925     c                   eval      GcoEntit8 = *blanks
114300960925     c                   eval      GcoTprif1 = *blanks
114400960925     c                   eval      GcoTprif2 = *blanks
114500960925     c                   eval      GcoTprif3 = *blanks
114600960925     c                   eval      GcoTprif4 = *blanks
114700960925     c                   eval      GcoTprif5 = *blanks
114800960925     c                   eval      GcoTprif6 = *blanks
114900960925     c                   eval      GcoTprif7 = *blanks
115000960925     c                   eval      GcoTprif8 = *blanks
115100960923     C*
115200960926     c                   if        flgMIX = *on
115300960926     C*
115400960924     c                   if        GvaEntit11 = *on and mixentit1 <> *blanks
115500960923     c                   eval      GcoEntit1  = MixEntit1
115600960923     c                   eval      GcotpRif1  = TipEnt(1)
115700960923     C                   IF        tipent(1) = 'LDV'
115800960923     c                   movel     gcoentit1     savlineav
115900960923     c                   endif
116000960923     C                   IF        tipent(1) = 'FIL'
116100960923     c                   movel     gcoentit1     savfiliale
116200960923     c                   endif
116300960923     c                   endif
116400960923     c*
116500960924     c                   if        GvaEntit21 = *on and mixentit2 <> *blanks
116600960923     c                   eval      GcoEntit2  = MixEntit2
116700960923     c                   eval      GcotpRif2  = TipEnt(2)
116800960923     C                   IF        tipent(2) = 'LDV'
116900960923     c                   movel     gcoentit2     savlineav
117000960923     c                   endif
117100960923     C                   IF        tipent(2) = 'FIL'
117200960923     c                   movel     gcoentit2     savfiliale
117300960923     c                   endif
117400960923     c                   endif
117500960923     C*
117600960924     c                   if        GvaEntit31 = *on and mixentit3 <> *blanks
117700960923     c                   eval      GcoEntit3  = MixEntit3
117800960923     c                   eval      GcotpRif3  = TipEnt(3)
117900960923     C                   IF        tipent(3) = 'LDV'
118000960923     c                   movel     gcoentit3     savlineav
118100960923     c                   endif
118200960923     C                   IF        tipent(3) = 'FIL'
118300960923     c                   movel     gcoentit3     savfiliale
118400960923     c                   endif
118500960923     c                   endif
118600960923     C*
118700960924     c                   if        GvaEntit41 = *on and mixentit4 <> *blanks
118800960923     c                   eval      GcoEntit4  = MixEntit4
118900960923     c                   eval      GcotpRif4  = TipEnt(4)
119000960923     C                   IF        tipent(4) = 'LDV'
119100960923     c                   movel     gcoentit4     savlineav
119200960923     c                   endif
119300960923     C                   IF        tipent(4) = 'FIL'
119400960923     c                   movel     gcoentit4     savfiliale
119500960923     c                   endif
119600960923     c                   endif
119700960923     C*
119800960924     c                   if        GvaEntit51 = *on and mixentit5 <> *blanks
119900960923     c                   eval      GcoEntit5  = MixEntit5
120000960923     c                   eval      GcotpRif5  = TipEnt(5)
120100960923     C                   IF        tipent(5) = 'LDV'
120200960923     c                   movel     gcoentit5     savlineav
120300960923     c                   endif
120400960923     C                   IF        tipent(5) = 'FIL'
120500960923     c                   movel     gcoentit5     savfiliale
120600960923     c                   endif
120700960923     c                   endif
120800960923     c*
120900960924     c                   if        GvaEntit61 = *on and mixentit6 <> *blanks
121000960923     c                   eval      GcoEntit6  = MixEntit6
121100960923     c                   eval      GcotpRif6  = TipEnt(6)
121200960923     C                   IF        tipent(6) = 'LDV'
121300960923     c                   movel     gcoentit6     savlineav
121400960923     c                   endif
121500960923     C                   IF        tipent(6) = 'FIL'
121600960923     c                   movel     gcoentit6     savfiliale
121700960923     c                   endif
121800960923     c                   endif
121900960923     c*
122000960924     c                   if        GvaEntit71 = *on and mixentit7 <> *blanks
122100960923     c                   eval      GcoEntit7  = MixEntit7
122200960923     c                   eval      GcotpRif7  = TipEnt(7)
122300960923     C                   IF        tipent(7) = 'LDV'
122400960923     c                   movel     gcoentit7     savlineav
122500960923     c                   endif
122600960923     C                   IF        tipent(7) = 'FIL'
122700960923     c                   movel     gcoentit7     savfiliale
122800960923     c                   endif
122900960923     c                   endif
123000960923     c*
123100960924     c                   if        GvaEntit81 = *on and mixentit8 <> *blanks
123200960923     c                   eval      Gcoentit8  = MixEntit8
123300960923     c                   eval      GcotpRif8  = TipEnt(8)
123400960923     C                   IF        tipent(8) = 'LDV'
123500960923     c                   movel     gcoentit8     savlineav
123600960923     c                   endif
123700960923     C                   IF        tipent(8) = 'FIL'
123800960923     c                   movel     gcoentit8     savfiliale
123900960923     c                   endif
124000960923     c                   endif
124100960923     c*
124200960926     c                   endif
124300960923     c*
124400960923     C                   ENDSR
124500960923     c*
124600960731      /EJECT
124700960731      ************************************************************
124800960731      * Reperisco il fido e l'agente da anagrafica
124900960731      ************************************************************
125000960926     C     DatiConto     BEGSR
125100960731     c*
125200960731     c* Solo per clienti
125300960731     c                   clear                   savAgente
125400960923     c                   clear                   savsogg
125500960923     c                   clear                   savkccestr
125600960923     c                   clear                   savkscestr
125700990907     c                   clear                   savsaldnat
125800990114     c* Se su ansif unita <> da ldv, va bene passare unita di selezione
125900990114     c* al driver nddvasog prima di leggere PPA, nell'altro caso bisogna
126000990114     c* passare l'unità della partita valida , quindi chiamarlo durante
126100990114     c* la lettura di ppa
126200990114   1 C                   if        SnaturaG01 = 'C' and fgo5  <> *on
126300960731     C                   MOVE      '1'           DVARIC
126400960731     C                   MOVE      societaG01    DVASOCIETA
126500960731     C                   MOVE      UnitaG01      DVAUnita
126600960731     C                   MOVE      SnaturaG01    DVASNATURA
126700960731     C                   MOVE      PPAKSC        DVAcodice
126800960731     C                   MOVEL     'DVACLI'      DVAstrutt
126900960731     C                   EVAL      DVALenOut = %Size(DVACLI)
127000960731     C                   CALLB     'NDDVASOG'    PDVASOG
127100960731   2 C                   if        DVAerrore<>*ON
127200960731     C                   eval      %subst(DVACLI:1:DVALENOUT)
127300960731     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
127400960731     c                   eval      Savagente = DVCAGENTE1
127500960731   2ec                   endif
127600960731   1ec                   endif
127700960923     C*
127800960923     C* Dati del cliente o fornitore
127900981026     C                   reset                   ANRCO
128000981026     C                   EVAL      LenOut = %Size(ANRCO)
128100960923     C                   eval      Data = Datmin
128200960923     C                   CallB     'NDMVC002'
128300960923     C                   PARM                    SocietaG01
128400960923     C                   PARM                    kccg01
128500960923     C                   PARM                    kscg01
128600960923     C                   PARM                    Data
128700960923     C                   PARM      *OFF          GesMsg
128800960923     C                   PARM      *OFF          Esito
128900981026     C                   PARM      'ANRCO'       StrutturaO
129000981026     C                   PARM                    anrco
129100960923     C                   PARM                    LenOut
129200960923   B1C     Esito         IFEQ      *Off
129300981026     c                   eval      savsogg = rcosogg
129400981026     c                   eval      savkccestr = rcokccestr
129500981026     c                   eval      savkscestr = rcokscestr
129600990907     c                   eval      savsaldnat = rcosaldnat
129700960923     c                   endif
129800960731     C*
129900960731     C                   ENDSR
130000990114      /EJECT
130100990114      ************************************************************
130200990114      * Reperisco agente da unità di partita
130300990114      ************************************************************
130400990114     C     DatiConto1    BEGSR
130500990114     c*
130600990114     c                   clear                   savAgente
130700990114     c*
130800990114     C                   MOVE      '1'           DVARIC
130900990114     C                   MOVE      societaG01    DVASOCIETA
131000990114     C                   MOVE      PpaUnita      DVAUnita
131100990114     C                   MOVE      SnaturaG01    DVASNATURA
131200990114     C                   MOVE      PPAKSC        DVAcodice
131300990114     C                   MOVEL     'DVACLI'      DVAstrutt
131400990114     C                   EVAL      DVALenOut = %Size(DVACLI)
131500990114     C                   CALLB     'NDDVASOG'    PDVASOG
131600990114   2 C                   if        DVAerrore<>*ON
131700990114     C                   eval      %subst(DVACLI:1:DVALENOUT)
131800990114     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
131900990114     c                   eval      Savagente = DVCAGENTE1
132000990114     c                   endif
132100990114     c*
132200990114     c                   endsr
132300960628      /EJECT
132400960628      ************************************************************
132500960729      * esecuzione per dati contabili
132600960628      ************************************************************
132700960726     C     EXECalcoli    BEGSR
132800960729     c*
132900960729     c* Totali (TOTALI effettivi della partita se chiesta spaccatura in
133000960729     C* mix) (da pulire sempre perchè vengono ricalcolati)
133100960729     c*
133200960729     C                   IF        FLGMIX = *on
133300960905     c                   clear                   TotIespos        19 3
133400960905     c                   clear                   TotIrisc         19 3
133500960905     c                   clear                   TotIesposD       19 3
133600960905     c                   clear                   TotIriscD        19 3
133700960729     c                   clear                   TotIsaldo        19 3
133800960729     c                   clear                   TotIsainv        19 3
133900960729     c                   clear                   TotIsaldoD       19 3
134000960729     c                   clear                   TotIsainvD       19 3
134100960729     c                   clear                   TotIeffar        19 3
134200960729     c                   clear                   TotIeffarD       19 3
134300960729     c                   clear                   TotItitar        19 3
134400960729     c                   clear                   TotItitarD       19 3
134500960729     c                   clear                   TotIeffas        19 3
134600960729     c                   clear                   TotIeffasD       19 3
134700960729     c                   clear                   TotIinasp        19 3
134800960729     c                   clear                   TotIinaspD       19 3
134900960729     c                   clear                   TotIinasd        19 3
135000960729     c                   clear                   TotIinasdD       19 3
135100960729     c                   clear                   TotIscato        19 3
135200960729     c                   clear                   TotIscatoD       19 3
135300960729     c                   clear                   TotIscare        19 3
135400960729     c                   clear                   TotIscareD       19 3
135500970404     c                   clear                   TotIscain        19 3
135600970404     c                   clear                   TotIscainD       19 3
135700960730     c                   clear                   TotIsca0
135800960730     c                   clear                   TotIsca0d
135900960729     c                   endif
136000960725     c*
136100960726     c* Trovo saldo contabile
136200960725     c*
136300960726     c                   eval      ScPsocieta = SocietaG01
136400960726     c                   eval      ScPctb     = CTBG01
136500960920     c                   eval      ScPtpscad  = *off
136600960725     C                   eval      scpdett    = 'P'
136700960726     c                   eval      ScPkcc     = wrkkcc
136800960726     c                   eval      ScPksc     = wrkksc
136900960726     c                   eval      ScPdtpar   = WRKdtpar
137000960726     c                   eval      ScPnrpar   = WRKnrpar
137100960726     c                   eval      ScPserpar  = wrkserpar
137200090504>>>>>C* Imposto scadenzario retroattivo.
137300090504>>>>>C                   if        dtregdg01 < udateiso
137400090504>>>>>C                             or dtscag01 < udateiso
137500090504>>>>>c                   eval      Scptpscad  = *on
137600090504>>>>>C                   endif
137700960726     c*
137800960726     c* Eseguo chain per partita
137900001027D0747C                   IF        flgQtemp = *on
138000960920     c     K09SCP01      CHAIN     NDSCP01L                           21
138100001027D0747c                   else
138200001027 "   c     K09SCP01      CHAIN     NDSCP01W                           21
138300001027D0747c                   endif
138400961115     c                   clear                   WDare
138500961115     c                   clear                   WAvere
138600961115     c                   clear                   WSaldo
138700961115     c                   clear                   WDareD
138800961115     c                   clear                   WAvereD
138900960729   1 c                   if        *in21 = *off
139000960725     C* moneta di conto
139100960725     c*sommo i dare e gli avere nei campi di lavoro DARE e Avere per
139200960725     c* vedere se il segno della partita è positivo o negativo
139300960725     c                   add       scptDlD       WDare
139400960725     c                   add       scptDlP       WDare
139500960725     c                   add       scptDlG       WDare
139600960725     c                   add       scptAlD       WAvere
139700960725     c                   add       scptAlP       WAvere
139800960725     c                   add       scptAlG       WAvere
139900960726     C* divisa
140000960726     c                   add       scptDDD       WDareD
140100960726     c                   add       scptDDP       WDareD
140200960726     c                   add       scptDDG       WDareD
140300960726     c                   add       scptADD       WAvereD
140400960726     c                   add       scptADP       WAvereD
140500960726     c                   add       scptADG       WAvereD
140600960726     C*
140700960726   1eC                   endif
140800960726     c*
140900960729     c* TROVO SALDO
141000960726     c* mc
141100960725     c     WDare         sub       WAvere        wrksaldo
141200960725     C* se non chiesto mix
141300960725     c                   if        flgmix = *off
141400960725     c                   z-add     wrksaldo      Wsaldo
141500960725     c                   else
141600960725     c* calcolo importo in base alla percentuale del mix
141700960730     c                   z-add     wrksaldo      wrkimporto
141800960730     c                   exsr      CalcPercMc
141900960730     C                   eval      wsaldo = wrkimporto
142000960725     c                   endif
142100960725     c*
142200960725     c                   add       Wsaldo        GcoIsaldo
142300960725     c                   add       wsaldo        VirIsaldo
142400960725     C                   ADD       wrksaldo      TotIsaldo
142500960725     C*
142600960725     c*clienti
142700990907   1 c                   if        SnaturaG01 = 'C' or SavSaldNat = 'D'
142800960725     C                   if        Wdare         < Wavere
142900960725     c                   add       Wsaldo        GcoIsainv
143000960725     c                   add       wsaldo        VirIsainv
143100960725     C                   ADD       wrksaldo      TotIsainv
143200960725     c                   endif
143300960729   1Ec                   endif
143400960725     c*fornitori
143500990907   1 c                   if        SnaturaG01 = 'F'  or SavSaldNat = 'A'
143600960725     C                   if        WDare         > Wavere
143700960725     c                   add       Wsaldo        GcoIsainv
143800960725     c                   add       wsaldo        VirIsainv
143900960725     C                   ADD       wrksaldo      TotIsainv
144000960725     c                   endif
144100960729   1Ec                   endif
144200960726     c* divisa
144300960726     c     WDareD        sub       WAvereD       WRksaldo
144400960725     C* se non chiesto mix
144500960725     c                   if        flgmix = *off
144600960725     c                   z-add     wrksaldo      Wsaldo
144700960725     c                   else
144800960725     c* calcolo importo in base alla percentuale del mix
144900960730     C                   Z-add     wrksaldo      wrkimporto
145000960730     c                   exsr      CalcPercD
145100960730     C                   eval      wsaldo = Wrkimporto
145200960725     c                   endif
145300960725     c*
145400960725     c                   add       Wsaldo        GcoIsaldoD
145500960725     c                   add       Wsaldo        VirIsaldoD
145600960725     C                   ADD       wrksaldo      TotIsaldoD
145700960725     C*
145800960725     c*clienti
145900990907   1 c                   if        SnaturaG01 = 'C' or SavSaldNat = 'D'
146000960725     C                   if        Wdare         < Wavere
146100960725     c                   add       Wsaldo        GcoIsainvD
146200960725     c                   add       Wsaldo        VirIsainvD
146300960725     C                   ADD       wrksaldo      TotIsainvD
146400960725     c                   endif
146500960729   1ec                   endif
146600960725     c*fornitori
146700990907   1 c                   if        SnaturaG01 = 'F'  or SavSaldNat = 'A'
146800960725     C                   if        WDare         > Wavere
146900960725     c                   add       Wsaldo        GcoIsainvD
147000960725     c                   add       Wsaldo        VirIsainvD
147100960725     C                   ADD       wrksaldo      TotIsainvD
147200960725     c                   endif
147300960729   1ec                   endif
147400960726     C*
147500960726     c* SALDI in mc. simulata
147600960729   1 C                   IF        gcoIsainvD <> 0 and ppadivisa
147700960729     c                             <> xscdiv  and tpcamg01 <> *blanks
147800960726     C                   z-add     gcoIsainvD    wrkimporto
147900960726     c                   EXSR      RepMcSimul
148000960726     C                   z-add     wrkimporto    GcoIsainvS
148100960726     c                   else
148200960726     c                   z-add     GcoIsainv     GcoIsainvS
148300960726   2ec                   endif
148400960726     c*
148500960729   2 C                   IF        gcoIsaldoD <> 0 and ppadivisa <>
148600960729     c                             xscdiv  and tpcamg01 <> *blanks
148700960726     C                   z-add     gcoIsaldoD    wrkimporto
148800960726     c                   EXSR      RepMcSimul
148900960726     C                   z-add     wrkimporto    GcoIsaldoS
149000960726     c                   else
149100960726     c                   z-add     GcoIsaldo     GcoIsaldoS
149200960726   2ec                   endif
149300960723     c*
149400960628     c*
149500960729     c* Dettaglio PER PARTITA
149600960628     c*
149700960726     c                   eval      Scdsocieta = SocietaG01
149800960726     c                   eval      Scdctb     = ctbG01
149900090504>>>>>C* Imposto scadenzario retroattivo.
150000090504>>>>>C                   if        dtregdg01 < udateiso
150100090504>>>>>C                             or dtscag01 < udateiso
150200090504>>>>>c                   eval      Scdtpscad  = *on
150300090504>>>>>C                   else
150400960920     c                   eval      Scdtpscad  = *off
150500090504>>>>>C                   endif
150600090504>>>>>C*
150700960726     c                   eval      Scdkcc     = WRKkcc
150800960726     c                   eval      Scdksc     = wrkksc
150900960726     c                   eval      Scddtpar   = wrkdtpar
151000960726     c                   eval      Scdnrpar   = wrknrpar
151100960726     c                   eval      Scdserpar  = wrkserpar
151200001027D0747C                   If        flgQtemp = *on
151300960920     c     k08scd01      setll     NDSCD01L
151400001027D0747c                   else
151500001027  "  c     k08scd01      setll     NDSCD01W
151600001027D0747c                   endif
151700960726     c*
151800960702     c                   do        *HIVAL
151900001027D0747C                   If        flgQtemp = *on
152000960920     c     k08scd01      READE     NDSCD01L                               21
152100001027D0747c                   else
152200001031  "  c     k08scd01      READE     NDSCD01W                               21
152300001027D0747c                   endif
152400960716     C   21              leave
152500090504
152600090504>>>>>C* Per alcune versioni scarto gli effetti.
152700090504>>>>>C                   IF        ScdMdPag <> *BLANK
152800090504>>>>>C                             AND
152900090504>>>>>C                             YAmerighi = *ON
153000090504>>>>>C                   ITER
153100090504>>>>>C                   ENDIF
153200090504
153300960628     c* Calcolo data rischio
153400960628     c* Tolgo alla data di scadenza di riferimento i giorni di rischio
153500960628     c* della causale
153600960628     c                   clear                   OggRisc
153700960628     c                   if        ScdCauRisc <> *blanks
153800990122     C                   CLEAR                   ANOPE
153900990122     C                   eval      Data = Datmin
154000990122     C                   EVAL      LenOut = %Size(ANOPE)
154100990122     C                   CallB     'NDMVC003'
154200990122     C                   PARM                    SCDSocieta
154300990122     C                   PARM                    SCDCauRisc
154400990122     C                   PARM                    Data
154500990122     C                   PARM                    Uso
154600990122     C                   PARM      *OFF          GesMsg
154700990122     C                   PARM      *OFF          Esito
154800990122     C                   PARM      'ANOPE'       StrutturaO
154900990122     C                   PARM                    ANOPE
155000990122     C                   PARM                    LenOut
155100990122     C                   PARM      '0'           Refresh
155200990122     C                   IF        esito = *off
155300960628     c                   z-add     Opeggrisc     OggRisc
155400960628     C                   endif
155500960628     C                   endif
155600960628     c*
155700960708     c                   move      DtscaG01      DataIso1
155800960628     c     DataIso1      subdur    OGgRisc:*Days DataIso2
155900960628     c                   move      DataIso2      DtRischio
156000960905     C*
156100960905     c* Calcoli per campi di NDSCD
156200960905     C*
156300960905     C* se non chiesto mix
156400960905   1 c                   if        flgmix = *off
156500960905     C                   Z-add     ScdImpRmc     WrKImpRmc
156600960905     C                   z-add     ScdImpRD      WrkImpRD
156700960905   1xc                   else
156800960905     c* calcolo importo in base alla percentuale del mix
156900960905     c                   z-add     ScdImpRmc     Wrkimporto
157000960905     c                   exsr      CalcPercMc
157100960905     C                   z-add     wrkimporto    WrkImpRmc
157200960905     c                   z-add     ScdImpRd      Wrkimporto
157300960905     c                   exsr      CalcPercD
157400960905     C                   z-add     wrkimporto    WrkImpRD
157500960905  1e c                   endif
157600960905     C* importo simulato (nel campo wrkimportS dal campo WrKimpRD)
157700960905     C                   exsr      simula
157800960725     C*
157900960725     c* IMPORTO RESIDUO CAMBI
158000960726     C*
158100960729     C                   IF        ppadivisa <> xscdiv  and scdimprd
158200960726     c                             = 0
158300960905     C                   ADD       WrKImpRmc     GcoIdcnc
158400960726     c                   endif
158500960628     C*
158600960905     c* EFFETTI A RISCHIO + RISCHIO
158700960628     C*
158800960628     c                   if        ScdCauRisc <> *blanks and
158900960628     c                             ScdMdPag   <> *blanks and
159000961115     c                             Dtrischio < ScdDtSca  and
159100960708     c                             ScdDtSca <= DtScaG01
159200960905     c*
159300960905     C                   ADD       WrkImpRmc     GcoIeffaR
159400960905     C                   ADD       WrkImpRD      GcoIeffaRD
159500960905     c                   add       wrkimportS    GcoieffarS
159600960905     c*
159700960905     C                   ADD       WrkImpRmc     GcoIrisc
159800960905     C                   ADD       WrkImpRD      GcoIriscD
159900960905     c                   add       wrkimportS    GcoiriscS
160000960905     C*Se chiesto Mix, riempi campi per quadratura
160100960905   1 c                   if        flgmix = *on
160200960722     C                   ADD       ScdImpRmc     TotIeffaR
160300960722     C                   ADD       ScdImpRD      TotIeffaRD
160400960905     c                   add       WrkImpRmc     VirIeffaR
160500960905     c                   add       wrkimpRD      VirIeffaRD
160600960905     C*
160700960905     C                   ADD       ScdImpRmc     Totirisc
160800960905     C                   ADD       ScdImpRD      TotIriscD
160900960905     c                   add       WrkImpRmc     VirIrisc
161000960905     c                   add       wrkimpRD      VirIriscD
161100960729  1e c                   endif
161200960905     C* Somme importo rischio
161300960905     c                   exsr      SommaRisc
161400960729     c                   endif
161500960628     C*
161600960905     c* ALTRI TITOLI A RISCHIO + RISCHIO
161700960628     C*
161800960628     c                   if        ScdCauRisc <> *blanks and
161900980608     c                             ScdMdpag   =  *blanks and
162000960628     c                             Dtrischio < ScdDtSca  and
162100960708     c                             ScdDtSca <= DtScaG01
162200960905     C*
162300960905     C                   ADD       WrkImpRmc     GcoItitaR
162400960905     C                   ADD       WrkImpRD      GcoItitaRD
162500960905     c                   add       wrkimportS    GcoititarS
162600960905     c*
162700960905     C                   ADD       WrkImpRmc     GcoIrisc
162800960905     C                   ADD       WrkImpRD      GcoIriscD
162900960905     c                   add       wrkimportS    GcoiriscS
163000960905     C*Se chiesto Mix, riempi campi per quadratura
163100960905   1 c                   if        flgmix = *on
163200960722     C                   ADD       ScdImpRmc     TotItitaR
163300960722     C                   ADD       ScdImpRD      TotItitaRD
163400960905     C                   ADD       WrkimpRmc     VirItitaR
163500960905     C                   ADD       wrkimpRD      VirItitaRD
163600960905     C*
163700960905     C                   ADD       ScdImpRmc     TotIrisc
163800960905     C                   ADD       ScdImpRD      TotIriscD
163900960905     C                   ADD       WrkimpRmc     VirIrisc
164000960905     C                   ADD       wrkimpRD      VirIriscD
164100960722     c                   endif
164200960905     C* Somme importo rischio
164300960905     c                   exsr      SommaRisc
164400960628     c                   endif
164500960628     C*
164600960628     c* EFFETTI A SCADERE
164700960628     C*
164800960628     c                   if        ScdMdPag <> *blanks and
164900960708     c                             ScdDtSca > DtScaG01
165000960905     C                   ADD       WrkImpRmc     GcoIeffas
165100960905     C                   ADD       WrkImpRD      GcoIeffasD
165200960905     c                   add       wrkimportS    GcoieffasS
165300960905     C*Se chiesto Mix, riempi campi per quadratura
165400960905   1 c                   if        flgmix = *on
165500960722     C                   ADD       ScdImpRmc     TotIeffas
165600960722     C                   ADD       ScdImpRD      TotIeffasD
165700960905     C                   ADD       wrkimpRmc     VirIeffas
165800960905     C                   ADD       wrkimpRD      VirIeffasD
165900960722     c                   endif
166000960628     c                   endif
166100960628     c*
166200960722     c* INCASSI A SCADERE PROVVISORI
166300960709     c                   if        ScdTitoli = '1'     and
166400960709     c                             ScdDtSca > DtScaG01
166500960905     C                   ADD       WrkImpRmc     GcoIinasP
166600960905     C                   ADD       WrkImpRD      GcoIinaspD
166700960905     c                   add       wrkimportS    GcoiinaspS
166800960905     C*Se chiesto Mix, riempi campi per quadratura
166900960905   1 c                   if        flgmix = *on
167000960722     C                   ADD       ScdImpRmc     TotIinasP
167100960722     C                   ADD       ScdImpRD      TotIinaspD
167200960905     C                   ADD       wrkimpRmc     VirIinasP
167300960905     C                   ADD       wrkimpRD      VirIinaspD
167400960722     c                   endif
167500960905     c* se flg gest. imp. incassi provv è 1, lo devo sommare nel totale
167600960905     c* esposizione
167700960905     c                   if        wrkgiinasP = '1'
167800960905     c                   add       WrkImpRmc     GcoIespos
167900960905     c                   add       WRkImpRD      GcoIesposD
168000960905     c                   add       WrkimportS    GcoIesposS
168100960905     c                   endif
168200960905     c* se flg gest. imp. incassi provv è 2, lo devo sommare nel totale
168300960905     c* scadere
168400981221     c* e, di conseguenza, nell'esposizione
168500960905     c                   if        wrkgiinasP = '2'
168600960905     c                   add       WrkImpRmc     GcoIscare
168700960905     c                   add       WRkImpRD      GcoIscareD
168800960905     c                   add       WrkimportS    GcoIscareS
168900981221     c                   add       WrkImpRmc     GcoIespos
169000981221     c                   add       WRkImpRD      GcoIesposD
169100981221     c                   add       WrkimportS    GcoIesposS
169200960905     c                   endif
169300960709     c                   endif
169400960628     c*
169500960722     c* INCASSI A SCADERE DEFINITIVI
169600960722     c*
169700960722     c                   if        ScdTitoli = '2'     and
169800960722     c                             ScdDtSca > DtScaG01
169900960905     C                   ADD       WrkImpRmc     GcoIinasD
170000960905     C                   ADD       WrkImpRD      GcoIinasDD
170100960905     c                   add       wrkimportS    GcoiinasdS
170200960905     C*Se chiesto Mix, riempio campi per quadratura
170300960905   1 c                   if        flgmix = *on
170400960722     C                   ADD       ScdImpRmc     TotIinasD
170500960722     C                   ADD       ScdImpRD      TotIinasDD
170600960905     C                   ADD       wrkimpRmc     VirIinasD
170700960905     C                   ADD       wrkimpRD      VirIinasDD
170800960722     c                   endif
170900960905     c* se flg gest. imp. incassi defin è 1, lo devo sommare nel totale
171000960905     c* esposizione
171100960905     c                   if        wrkgiinasd = '1'
171200960905     c                   add       WrkImpRmc     GcoIespos
171300960905     c                   add       WrkImpRD      GcoIesposD
171400960905     c                   add       WrkImportS    GcoIesposS
171500960905     c                   endif
171600960905     c* se flg gest. imp. incassi defin è 2, lo devo sommare nel totale
171700960905     c* scadere
171800981221     c* e, di conseguenza, nell'esposizione
171900960905     c                   if        wrkgiinasd = '2'
172000960905     c                   add       WrkImpRmc     GcoIscare
172100960905     c                   add       WrkImpRD      GcoIscareD
172200960905     c                   add       WrkImportS    GcoIscareS
172300981221     c                   add       WrkImpRmc     GcoIespos
172400981221     c                   add       WrkImpRD      GcoIesposD
172500981221     c                   add       WrkImportS    GcoIesposS
172600960905     c                   endif
172700960722     c                   endif
172800960628     c*
172900961115     c* TOTALE SCADUTO
173000960628     c*
173100960725     C                   IF        ScdMdPag = *blanks and
173200960725     c                             ScdTitoli = *blanks and
173300961115     c                             ScdDtsca  <= DtRischio
173400960905     C                   ADD       WrkImpRmc     GcoIscato
173500960905     C                   ADD       WrkImpRD      GcoIscatoD
173600960905     c                   add       wrkimportS    GcoiscatoS
173700960905     C                   ADD       WrkImpRmc     GcoIespos
173800960905     C                   ADD       WrkImpRD      GcoIesposD
173900960905     c                   add       wrkimportS    GcoiesposS
174000960905     C*Se chiesto Mix, riempio campi per quadratura
174100960905   1 c                   if        flgmix = *on
174200960722     C                   ADD       ScdImpRmc     TotIscato
174300960722     C                   ADD       ScdImpRD      TotIscatoD
174400960905     C                   ADD       wrkimpRmc     VirIscato
174500960905     C                   ADD       wrkimpRD      VirIscatoD
174600960905     C                   ADD       ScdImpRmc     Totiespos
174700960905     C                   ADD       ScdImpRD      TotIesposD
174800960905     C                   ADD       wrkimpRmc     VirIespos
174900960905     C                   ADD       wrkimpRD      VirIesposD
175000960722     c                   endif
175100960628     c                   endif
175200970404     c*
175300970404     c* SCADUTO PER INSOLUTI
175400970404     c*
175500970408     c                   if        ScdCauS2   <> *blanks
175600990122     C                   CLEAR                   ANOPE
175700990122     C                   eval      Data = Datmin
175800990122     C                   EVAL      LenOut = %Size(ANOPE)
175900990122     C                   CallB     'NDMVC003'
176000990122     C                   PARM                    SCDSocieta
176100990122     C                   PARM                    SCDCauS2
176200990122     C                   PARM                    Data
176300990122     C                   PARM                    Uso
176400990122     C                   PARM      *OFF          GesMsg
176500990122     C                   PARM      *OFF          Esito
176600990122     C                   PARM      'ANOPE'       StrutturaO
176700990122     C                   PARM                    ANOPE
176800990122     C                   PARM                    LenOut
176900990122     C                   PARM      '0'           Refresh
177000990122     C                   IF        esito = *off
177100970404     C                   IF        ScdMdPag = *blanks and
177200970404     c                             ScdTitoli = *blanks and
177300970404     c                             ScdDtsca  <= DtRischio and
177400970404     c                             Opecalscad = '1'
177500970404     C                   ADD       WrkImpRmc     GcoIscain
177600970404     C                   ADD       WrkImpRD      GcoIscainD
177700970404     c                   add       wrkimportS    GcoiscainS
177800970404     C*Se chiesto Mix, riempio campi per quadratura
177900970404   1 c                   if        flgmix = *on
178000970404     C                   ADD       ScdImpRmc     TotIscain
178100970404     C                   ADD       ScdImpRD      TotIscainD
178200970404     C                   ADD       wrkimpRmc     VirIscain
178300970404     C                   ADD       wrkimpRD      VirIscainD
178400970404     c                   endif
178500970404     c                   endif
178600970408     c                   endif
178700970408     c                   endif
178800960628     C*
178900960905     c* TOTALE SCADERE (da sommare nell'esposizione)
179000960628     C*
179100960708     c     ScdDtsca      ifgt      DtScaG01
179200970402     c* questi non li devo sommare perchè li ho già sommati precedentemente
179300970402     c* se richiesto nei parametri moduli
179400970402     c     ScdTitoli     andne     '1'
179500970402     c     ScdTitoli     andne     '2'
179600960905     C                   ADD       WrkImpRmc     GcoIscare
179700960905     C                   ADD       WrkImpRD      GcoIscareD
179800960905     c                   add       wrkimportS    GcoiscareS
179900960905     C                   ADD       WrkImpRmc     GcoIespos
180000960905     C                   ADD       WrkImpRD      GcoIesposD
180100960905     c                   add       wrkimportS    GcoiesposS
180200960905     C*Se chiesto Mix, riempio campi per quadratura
180300960905   1 c                   if        flgmix = *on
180400960722     C                   ADD       ScdImpRmc     TotIscare
180500960722     C                   ADD       ScdImpRD      TotIscareD
180600960905     C                   ADD       wrkimpRmc     VirIscare
180700960905     C                   ADD       wrkimpRD      VirIscareD
180800960905     C                   ADD       ScdImpRmc     TotIespos
180900960905     C                   ADD       ScdImpRD      TotIesposD
181000960905     C                   ADD       wrkimpRmc     VirIespos
181100960905     C                   ADD       wrkimpRD      VirIesposD
181200960722     c                   endif
181300960628     c                   endif
181400960729     c*
181500960729     c* 8 PERIODI
181600960730     C* chiamo routine x51prd08r se NON chiamata X20SCAD per partita
181700960730     C* o se non chiamata per niente o se chiamata x20 per conto
181800960730     C                   IF        flgx20 = *off or
181900960730     C                             flgx20 = *on and flgconto = *on
182000960730     C                   IF        ScdMdPag = *blanks and
182100960730     c                             ScdTitoli = *blanks and
182200961115     c                             ScdDtsca  <= DtRischio  OR
182300961118     c                             ScdDtsca  > DtScaG01
182400981221     C* Sommo negli 8 periodi gli incassi a scedere P e D
182500981221     C* solo se nei parametri di calcolo è stato detto di sommarli
182600981221     C* nel totale scadere, così i dati sono omogenei.
182700981221     C     scdtitoli     ifeq      *blanks
182800981221     C     scdtitoli     oreq      '1'
182900981221     C     wrkgiinasp    andeq     '2'
183000981221     C     scdtitoli     oreq      '2'
183100981221     C     wrkgiinasd    andeq     '2'
183200960730     C* se non chiesto mix
183300960730   1 c                   if        flgmix = *off
183400960730     c                   eval      x51importo = ScdImpRmc
183500960730     c                   eval      x51importD = ScdImpRd
183600960730   1xc                   else
183700960730     c* calcolo importo in base alla percentuale del mix
183800960730     C                   z-add     ScdImpRmc     wrkimporto
183900960730     c                   exsr      CalcPercMc
184000960730     c                   z-add     wrkimporto    x51importo
184100960730     C                   z-add     ScdImpRd      wrkimporto
184200960730     c                   exsr      CalcPercD
184300960730     c                   z-add     wrkimporto    x51importd
184400960731  1e c                   endif
184500960729     C                   eval      x51dtscar = dtscaG01
184600960729     C                   eval      x51dtsca  = scddtsca
184700960729     C                   CALL      'X51PRD08R'
184800960729     c                   parm                    x51prd08ds
184900961119     C                   EXSR      AddPerX51
185000960730     c* sommo nei campi virtuali per quadratura
185100960730     C                   IF        X51NumPer <> 0 and flgmix = *on
185200960730     C                   z-add     x51NumPer     $x                3 0
185300960730     c                   add       x51importo    VirIsca0($X)
185400960730     c                   add       x51importD    VirIsca0d($X)
185500960730     C                   ADD       ScdImpRmc     TotIsca0($X)
185600960730     C                   ADD       ScdImpRD      TotIsca0D($X)
185700960730     c                   endif
185800960729     c                   eval      x51INIZ = *off
185900981221     C*
186000981221     C                   endif
186100981221     C*
186200960729     c                   endif
186300960729     c                   endif
186400960628     C*
186500960702   1eC                   ENDDO
186600961119     c*
186700961119     c* SE chiamato x20 e se chiamato per partita uso i campi di x20 per gli
186800961119     c* 8 periodi
186900961119     c                   if        flgconto = *off and flgx20 = *on
187000961119     C                   EXSR      AddPerX20
187100961119     c                   endif
187200961119     C* calcolo simulato 8 periodi
187300961119     c                   exsr      ExePeriodi
187400960628     C*
187500960628     C                   ENDSR
187600960905      /EJECT
187700960905      ************************************************************
187800960905      * Somme importo rischio
187900960905      ************************************************************
188000960905     C     SommaRisc     BEGSR
188100960905     c*
188200960905     c* se flg gestione importo rischio è 1, lo devo sommare nel totale
188300960905     c* esposizione
188400960905     c                   if        wrkgirisc = '1'
188500960905     c                   add       WrkIMPRMc     GcoIespos
188600960905     c                   add       WrkImpRD      GcoIesposD
188700960905     c                   add       WrkimportS    GcoIesposS
188800960905     c                   endif
188900960905     c*
189000960905     c* se flg gestione importo rischio è 3, lo devo sommare nel totale
189100960905     c* a scadere
189200981221     c* e, di conseguenza, nell'esposizione
189300960905     c                   if        wrkgirisc = '2'
189400960905     c                   add       WrkIMPRMc     GcoIscare
189500960905     c                   add       WrkImpRD      GcoIscareD
189600960905     c                   add       WrkimportS    GcoIscareS
189700981221     c                   add       WrkIMPRMc     GcoIespos
189800981221     c                   add       WrkImpRD      GcoIesposD
189900981221     c                   add       WrkimportS    GcoIesposS
190000960905     c                   endif
190100960905     c*
190200960905     c* se flg gestione importo rischio è 3, lo devo sommare nel totale
190300960905     c* scaduto
190400981221     c* e, di conseguenza, nell'esposizione
190500960905     c                   if        wrkgirisc = '3'
190600960905     c                   add       WrkIMPRMc     GcoIscato
190700960905     c                   add       WrkImpRD      GcoIscatoD
190800960905     c                   add       WrkimportS    GcoIscatoS
190900981221     c                   add       WrkIMPRMc     GcoIespos
191000981221     c                   add       WrkImpRD      GcoIesposD
191100981221     c                   add       WrkimportS    GcoIesposS
191200960905     c                   endif
191300960905     C*
191400960905     c                   ENDSR
191500960725      /EJECT
191600960725      ************************************************************
191700961119      * Calcoli degli 8 periodi se usato x20 per partita
191800960725      ************************************************************
191900961119     C     AddPerx20     BEGSR
192000960729     c*
192100960730     C* PERIODI
192200960730     c* SE chiamato x20 e se chiamato per partita uso i campi di x20 in add
192300960730     c                   add       x20prd01      GcoIsca01
192400960730     c                   add       x20prd02      GcoIsca02
192500960730     c                   add       x20prd03      GcoIsca03
192600960730     c                   add       x20prd04      GcoIsca04
192700960730     c                   add       x20prd05      GcoIsca05
192800960730     c                   add       x20prd06      GcoIsca06
192900960730     c                   add       x20prd07      GcoIsca07
193000960730     c                   add       x20prd08      GcoIsca08
193100960730     c                   add       x20prd01D     GcoIsca01D
193200960730     c                   add       x20prd02D     GcoIsca02D
193300960730     c                   add       x20prd03D     GcoIsca03D
193400960730     c                   add       x20prd04D     GcoIsca04D
193500960730     c                   add       x20prd05D     GcoIsca05D
193600960730     c                   add       x20prd06D     GcoIsca06D
193700960730     c                   add       x20prd07D     GcoIsca07D
193800960730     c                   add       x20prd08D     GcoIsca08D
193900960731     c                   z-add     x20prd01      wrkIsca01
194000960731     c                   z-add     x20prd02      wrkIsca02
194100960731     c                   z-add     x20prd03      wrkIsca03
194200960731     c                   z-add     x20prd04      wrkIsca04
194300960731     c                   z-add     x20prd05      wrkIsca05
194400960731     c                   z-add     x20prd06      wrkIsca06
194500960731     c                   z-add     x20prd07      wrkIsca07
194600960731     c                   z-add     x20prd08      wrkIsca08
194700960731     c                   z-add     x20prd01D     wrkIsca01D
194800960731     c                   z-add     x20prd02D     wrkIsca02D
194900960731     c                   z-add     x20prd03D     wrkIsca03D
195000960731     c                   z-add     x20prd04D     wrkIsca04D
195100960731     c                   z-add     x20prd05D     wrkIsca05D
195200960731     c                   z-add     x20prd06D     wrkIsca06D
195300960731     c                   z-add     x20prd07D     wrkIsca07D
195400960731     c                   z-add     x20prd08D     wrkIsca08D
195500960725     c*
195600960725     c                   endsr
195700960725      /EJECT
195800961119      ************************************************************
195900961119      * Calcoli degli 8 periodi da X51
196000961119      ************************************************************
196100961119     C     AddPerX51     BEGSR
196200961119     c*
196300961119     C* PERIODI
196400961119     c* uso quelli della x51prd08r per record di ndscd in add
196500961119     c* se x20 è stata chiamata per conto o non è stata chiamata perchè
196600961119     c* dato già calcolati
196700961119     c                   select
196800961119     c                   when      x51numper = 1
196900961119     c                   add       x51importo    GcoIsca01
197000961119     c                   add       x51importd    GcoIsca01d
197100961119     c                   when      x51numper = 2
197200961119     c                   add       x51importo    GcoIsca02
197300961119     c                   add       x51importd    GcoIsca02d
197400961119     c                   when      x51numper = 3
197500961119     c                   add       x51importo    GcoIsca03
197600961119     c                   add       x51importd    GcoIsca03d
197700961119     c                   when      x51numper = 4
197800961119     c                   add       x51importo    GcoIsca04
197900961119     c                   add       x51importd    GcoIsca04d
198000961119     c                   when      x51numper = 5
198100961119     c                   add       x51importo    GcoIsca05
198200961119     c                   add       x51importd    GcoIsca05d
198300961119     c                   when      x51numper = 6
198400961119     c                   add       x51importo    GcoIsca06
198500961119     c                   add       x51importd    GcoIsca06d
198600961119     c                   when      x51numper = 7
198700961119     c                   add       x51importo    GcoIsca07
198800961119     c                   add       x51importd    GcoIsca07d
198900961119     c                   when      x51numper = 8
199000961119     c                   add       x51importo    GcoIsca08
199100961119     c                   add       x51importd    GcoIsca08d
199200961119     c                   endsl
199300961119     c                   z-add     x51prd01      wrkIsca01        19 3
199400961119     c                   z-add     x51prd02      wrkIsca02        19 3
199500961119     c                   z-add     x51prd03      wrkIsca03        19 3
199600961119     c                   z-add     x51prd04      wrkIsca04        19 3
199700961119     c                   z-add     x51prd05      wrkIsca05        19 3
199800961119     c                   z-add     x51prd06      wrkIsca06        19 3
199900961119     c                   z-add     x51prd07      wrkIsca07        19 3
200000961119     c                   z-add     x51prd08      wrkIsca08        19 3
200100961119     c                   z-add     x51prd01D     wrkIsca01D       19 3
200200961119     c                   z-add     x51prd02D     wrkIsca02D       19 3
200300961119     c                   z-add     x51prd03D     wrkIsca03D       19 3
200400961119     c                   z-add     x51prd04D     wrkIsca04D       19 3
200500961119     c                   z-add     x51prd05D     wrkIsca05D       19 3
200600961119     c                   z-add     x51prd06D     wrkIsca06D       19 3
200700961119     c                   z-add     x51prd07D     wrkIsca07D       19 3
200800961119     c                   z-add     x51prd08D     wrkIsca08D       19 3
200900961119     C*
201000961119     c                   endsr
201100961119     c*
201200961119      /EJECT
201300961119      ************************************************************
201400961119      * Calcoli simulato degli 8 periodi
201500961119      ************************************************************
201600961119     C     ExePeriodi    BEGSR
201700961119     c*
201800961119     C* 8 PERIODI (simulati)
201900961119     C                   IF        WRKisca01D <> 0 and ppadivisa <> xscdiv
202000961119     c                             and tpcamg01 <> *blanks
202100961119     C                   z-add     WRKIsca01D    wrkimporto
202200961119     c                   EXSR      RepMcSimul
202300961119     C                   add       wrkimporto    GcoIsca01S
202400961119     c                   else
202500961119     c                   add       wrkisca01     Gcoisca01S
202600961119     c                   endif
202700961119     C*
202800961119     C                   IF        wrkisca02D <> 0 and ppadivisa <> xscdiv
202900961119     c                             and tpcamg01 <> *blanks
203000961119     C                   z-add     wrkIsca02D    wrkimporto
203100961119     c                   EXSR      RepMcSimul
203200961119     C                   add       wrkimporto    GcoIsca02S
203300961119     c                   else
203400961119     c                   add       wrkisca02     Gcoisca02S
203500961119     c                   endif
203600961119     C*
203700961119     C                   IF        wrkisca03D <> 0 and ppadivisa <> xscdiv
203800961119     c                             and tpcamg01 <> *blanks
203900961119     C                   z-add     wrkIsca03D    wrkimporto
204000961119     c                   EXSR      RepMcSimul
204100961119     C                   add       wrkimporto    GcoIsca03S
204200961119     c                   else
204300961119     c                   add       wrkisca03     Gcoisca03S
204400961119     c                   endif
204500961119     C*
204600961119     C                   IF        wrkisca04D <> 0 and ppadivisa <> xscdiv
204700961119     c                             and tpcamg01 <> *blanks
204800961119     C                   z-add     wrkIsca04D    wrkimporto
204900961119     c                   EXSR      RepMcSimul
205000961119     C                   add       wrkimporto    GcoIsca04S
205100961119     c                   else
205200961119     c                   add       wrkisca04     Gcoisca04S
205300961119     c                   endif
205400961119     C*
205500961119     C                   IF        wrkisca05D <> 0 and ppadivisa <> xscdiv
205600961119     c                             and tpcamg01 <> *blanks
205700961119     C                   z-add     wrkIsca05D    wrkimporto
205800961119     c                   EXSR      RepMcSimul
205900961119     C                   add       wrkimporto    GcoIsca05S
206000961119     c                   else
206100961119     c                   add       wrkisca05     Gcoisca05S
206200961119     c                   endif
206300961119     C*
206400961119     C                   IF        wrkisca06D <> 0 and ppadivisa <> xscdiv
206500961119     c                             and tpcamg01 <> *blanks
206600961119     C                   z-add     wrkIsca06D    wrkimporto
206700961119     c                   EXSR      RepMcSimul
206800961119     C                   add       wrkimporto    GcoIsca06S
206900961119     c                   else
207000961119     c                   add       wrkisca06     Gcoisca06S
207100961119     c                   endif
207200961119     C*
207300961119     C                   IF        wrkisca07D <> 0 and ppadivisa <> xscdiv
207400961119     c                             and tpcamg01 <> *blanks
207500961119     C                   z-add     wrkIsca07D    wrkimporto
207600961119     c                   EXSR      RepMcSimul
207700961119     C                   add       wrkimporto    GcoIsca07S
207800961119     c                   else
207900961119     c                   add       wrkisca07     Gcoisca07S
208000961119     c                   endif
208100961119     C*
208200961119     C                   IF        wrkisca08D <> 0 and ppadivisa <> xscdiv
208300961119     c                             and tpcamg01 <> *blanks
208400961119     C                   z-add     wrkIsca08D    wrkimporto
208500961119     c                   EXSR      RepMcSimul
208600961119     C                   add       wrkimporto    GcoIsca08S
208700961119     c                   else
208800961119     c                   add       wrkisca08     Gcoisca08S
208900961119     c                   endif
209000961119     c*
209100961119     c                   endsr
209200960725      ************************************************************
209300960725      * Calcolo simulato sui totali ricavati
209400960725      ************************************************************
209500960726     C     Simula        BEGSR
209600960726     C*
209700960905     C                   IF        Wrkimprd   <> 0 and ppadivisa <> xscdiv
209800960726     c                             and tpcamg01 <> *blanks
209900960905     C                   z-add     Wrkimprd      wrkimporto
210000960726     c                   EXSR      RepMcSimul
210100960729     C                   z-add     wrkimporto    wrkimportS
210200960726     c                   else
210300960905     c                   z-add     Wrkimprmc     wrkimportS
210400960726     c                   endif
210500960726     c*
210600960726     C                   ENDSR
210700960725     c*
210800960722      /EJECT
210900960722      ************************************************************
211000960722      * Quadratura NDGCO causa spaccatura per entità
211100960722      ************************************************************
211200960722     C     QuadraGco     BEGSR
211300960722     c*
211400960905     c     totiespos     sub       viriespos     wrkimporto
211500960905     c                   add       wrkimporto    gcoIespos
211600960905     c     totiesposD    sub       viriesposD    wrkimporto
211700960905     c                   add       wrkimporto    gcoIesposD
211800960905     c     totirisc      sub       viririsc      wrkimporto
211900960905     c                   add       wrkimporto    gcoIrisc
212000960905     c     totiriscD     sub       viririscD     wrkimporto
212100960905     c                   add       wrkimporto    gcoIriscD
212200960722     c     totisaldo     sub       virisaldo     wrkimporto
212300960722     c                   add       wrkimporto    gcoIsaldo
212400960722     c     totisainv     sub       virisainv     wrkimporto
212500960722     c                   add       wrkimporto    gcoIsainv
212600960722     c     totisaldoD    sub       virisaldoD    wrkimporto
212700960722     c                   add       wrkimporto    gcoIsaldoD
212800960722     c     totisainvD    sub       virisainvD    wrkimporto
212900960722     c                   add       wrkimporto    gcoIsainv
213000960722     c     totIeffar     sub       virieffar     wrkimporto
213100960722     c                   add       wrkimporto    gcoIeffar
213200960722     c     totIeffarD    sub       virieffarD    wrkimporto
213300960722     c                   add       wrkimporto    gcoIeffarD
213400960722     c     totItitar     sub       virItitar     wrkimporto
213500960722     c                   add       wrkimporto    gcoItitar
213600960722     c     totItitarD    sub       virItitarD    wrkimporto
213700960722     c                   add       wrkimporto    gcoItitarD
213800960722     c     totIeffaS     sub       virIeffas     wrkimporto
213900960722     c                   add       wrkimporto    gcoIeffas
214000960722     c     totIeffasD    sub       virIeffasD    wrkimporto
214100960722     c                   add       wrkimporto    gcoIeffasD
214200960722     c     totIinasp     sub       virIinasp     wrkimporto
214300960722     c                   add       wrkimporto    gcoIinasp
214400960722     c     totIinaspD    sub       virIinaspD    wrkimporto
214500960722     c                   add       wrkimporto    gcoIinaspD
214600960722     c     totIinasd     sub       virIinasd     wrkimporto
214700960722     c                   add       wrkimporto    gcoIinasd
214800960722     c     totIinasdD    sub       virIinasdD    wrkimporto
214900960722     c                   add       wrkimporto    gcoIinasdD
215000960722     c     totISCATO     sub       virIscato     wrkimporto
215100960722     c                   add       wrkimporto    gcoIscato
215200960722     c     totISCATOD    sub       virIscatoD    wrkimporto
215300960722     c                   add       wrkimporto    gcoIscatoD
215400960722     c     totISCAre     sub       virIscare     wrkimporto
215500960722     c                   add       wrkimporto    gcoIscare
215600960722     c     totISCAreD    sub       virIscared    wrkimporto
215700960722     c                   add       wrkimporto    gcoIscareD
215800960730     c* Quadratura 8 periodi
215900960730     c     1             do        8             $x
216000960730     c     totISCa0($X)  sub       virIsca0($X)  wrkimporto
216100960730     c                   select
216200960730     c                   when      $X =1
216300960730     c                   add       wrkimporto    gcoIsca01
216400960730     c                   when      $X =2
216500960730     c                   add       wrkimporto    gcoIsca02
216600960730     c                   when      $X =3
216700960730     c                   add       wrkimporto    gcoIsca03
216800960730     c                   when      $X =4
216900960730     c                   add       wrkimporto    gcoIsca04
217000960730     c                   when      $X =5
217100960730     c                   add       wrkimporto    gcoIsca05
217200960730     c                   when      $X =6
217300960730     c                   add       wrkimporto    gcoIsca06
217400960730     c                   when      $X =7
217500960730     c                   add       wrkimporto    gcoIsca07
217600960730     c                   when      $X =8
217700960730     c                   add       wrkimporto    gcoIsca08
217800960730     C                   endsl
217900960730     c     totISCa0D($X) sub       virIsca0D($X) wrkimporto
218000960730     c                   select
218100960730     c                   when      $X =1
218200960730     c                   add       wrkimporto    gcoIsca01D
218300960730     c                   when      $X =2
218400960730     c                   add       wrkimporto    gcoIsca02D
218500960730     c                   when      $X =3
218600960730     c                   add       wrkimporto    gcoIsca03D
218700960730     c                   when      $X =4
218800960730     c                   add       wrkimporto    gcoIsca04D
218900960730     c                   when      $X =5
219000960730     c                   add       wrkimporto    gcoIsca05D
219100960730     c                   when      $X =6
219200960730     c                   add       wrkimporto    gcoIsca06D
219300960730     c                   when      $X =7
219400960730     c                   add       wrkimporto    gcoIsca07D
219500960730     c                   when      $X =8
219600960730     c                   add       wrkimporto    gcoIsca08D
219700960730     C                   endsl
219800960730     c                   enddo
219900960722     c*
220000960722     c                   endsr
220100960722      /EJECT
220200960709      ************************************************************
220300960709      * Determino importi in moneta di conto simulata
220400960709      ************************************************************
220500960709     C     RepMcSimul    BEGSR
220600960709     C*
220700960709     C* richiama X01CAMBI per reperire il cambio alla data
220800960709     c*
220900980929     c****               exsr      callX01
221000980929     C*
221100960730     C* chiama X03MULTC per il controvalore
221200960730     C*
221300980929     c****               reset                   x03multcds
221400980929     C****               z-add     wrkimporto    x03importo
221500980929     C****               Eval      X03MODALIT =  '0'
221600980929     C****               Eval      X03NRDEC   =  X01NrDec
221700980929     C****               Eval      X03CAMBIO  =  x01cambio
221800980929     C****               Eval      X03BASECAM =  X01BaseStd
221900980929     C****               Eval      X03ARROTON =  '0'
222000980929     C*
222100980929     C****               Call      'X03MULTC'
222200980929     C****               PARM                    X03MultcDs
222300980929     C*
222400980929   5 C****               if        x03err=*off
222500980929     C****               z-add     x03controv    wrkimporto
222600980929   5-C****               endif
222700960716     C*
222800980929   2 c                   if        dtcamG01 <> datmin
222900980929     c                   eval      wrkdtcam = dtcamg01
223000980929     c                   else
223100980929     c                   eval      wrkdtcam = dtregdG01
223200980929   2eC                   endif
223300980929     C*
223400980929     C                   RESET                   PJX00202DS
223500980929     C                   MOVEL     PPADivisa     Divisa0202
223600980929     C                   MOVEL     XscDiv        DivMC0202
223700980929     C                   Z-ADD     wrkImporto    ImporD0202
223800980929     C                   Z-ADD     0             ImporM0202
223900980929     C                   Z-ADD     0             Cambio0202
224000980929     C                   MOVEL     wrkdtcam      DtRif0202
224100980929     C                   MOVEL     'H'           Arroto0202
224200980929     C* non richiedo il controllo scostamento cambio/importo
224300980929     C                   MOVEL     *on           NoCtrlScos
224400980929     C                   CALLP     X0202TestC(PJX00202DS :
224500980929     C                                        %size(PJX00202DS) :
224600980929     C                                        *off   : Esito   :
224700980929     C                                        NoCtrlScos : *omit)
224800980929     C                   If        Esito   = *off
224900980929     C                   Eval      wrkimporto   = ImporM0202
225000980929     C                   EndIf
225100980929     C*
225200960709     C                   ENDSR
225300960730      /EJECT
225400960730     C************************************************************
225500960730     C* Chiama X01Cambi
225600960730     C************************************************************
225700980929     C***  CallX01       BEGSR
225800960730     C*
225900960730     C* Se tipo cambio passato
226000960730     c* calcolo importo in m.c. simulato con data =  a quella
226100960730     c* passata, se non passata prendo la data limite registrazione defini
226200960730     C* tiva
226300960730     c*
226400980929   2 c****               if        dtcamG01 <> datmin
226500980929     c****               eval      wrkdtcam = dtcamg01
226600980929     c****               else
226700980929     c****               eval      wrkdtcam = dtregdG01
226800980929   2eC****               endif
226900960730     c*
227000960730     C*  Eseguo solo al cambio divisa
227100980929     C***                IF        ppadivisa <> savdivisa
227200980929     c***                move      ppadivisa     savdivisa
227300980929     C***
227400980929     C***                reset                   x01cambids
227500980929     C***                eval      x01societa=societag01
227600980929     C***                eval      x01divisa=ppadivisa
227700980929     C***                eval      x01dtrifdi=wrkdtcam
227800980929     C***                eval      x01divrif=xscdiv
227900980929     C***                eval      x01dtrifca=wrkdtcam
228000980929     C***                eval      x01tpcam=tpcamG01
228100980929     C***                eval      x01flgcam='3'
228200980929     C***                eval      x01dieerr=*off
228300980929     C***                eval      x01camerr=*off
228400980929     C*
228500980929     C***                call      'X01CAMBI'
228600980929     C***                parm                    x01cambids
228700980929     c*
228800980929     C***                ENDIF
228900980929     C***
229000980929     C***                ENDSR
229100960730      /EJECT
229200960730     C************************************************************
229300960730     C* Chiama X03mult per la percentuale, cosìarrotonda a seconda
229400960730     c* dei decimali (per divisa)
229500960730     C************************************************************
229600960730     C     CalcPercD     BEGSR
229700960730     c*
229800960730     C* richiama X01CAMBI per reperire i decimali della divisa
229900980929     C***                exsr      CallX01
230000960730     c*
230100960730     c* Moneta di conto
230200960730     c*
230300980929     C***                RESET                   X03MULTCDS
230400980929     C***                eval      X03Importo= WrkImporto
230500980929     C***                eval      X03Cambio = MixPerOpe/100
230600980929     C***                eval      X03Modalit= '0'
230700980929     C***                eval      X03Arroton= '0'
230800980929     C***                eval      X03NrDec  = x01NrDec
230900980929     C***                eval      X03BaseCam= x01BaseStd
231000980929     C***                call      'X03MULTC'
231100980929     C***                PARM                    X03MULTCDS
231200980929     C***                eval      WrkImporto= X03Controv
231300980929     C*
231400980929   2 c                   if        dtcamG01 <> datmin
231500980929     c                   eval      wrkdtcam = dtcamg01
231600980929     c                   else
231700980929     c                   eval      wrkdtcam = dtregdG01
231800980929   2eC                   endif
231900980929     C*
232000980929     C* reperimento dati divisa inserita
232100980929     C                   IF        X0201RpDVS(PPADivisa:wrkdtcam:
232200980929     C                                        ANDVS00F: %size(ANDVS00F):
232300980929     C                                        ANDVX00F: %size(ANDVX00F))
232400980929     C                             <> 0
232500980929     C* al momento non dovrebbe essere possibile un errore
232600980929   E2C                   ENDIF
232700980929     C                   Z-Add     WrkImporto    WImpInp
232800980929     C                   CallP     X0201MultC(WImpInp: MixPerOpe: 100:
232900980929     C                                        'H': '0': DVXNrDec : WImpOut)
233000980929     C                   eval      WrkImporto= WImpOut
233100980929     C*
233200960730     c                   ENDsr
233300960730      /EJECT
233400960730     C************************************************************
233500960730     C* Chiama X03mult per la percentuale, cosìarrotonda a seconda
233600960730     c* dei decimali (per moneta di conto)
233700960730     C************************************************************
233800960730     C     CalcPercMc    BEGSR
233900960730     c*
234000960730     c* Moneta di conto
234100980929     c*
234200980929     C***                RESET                   X03MULTCDS
234300980929     C***                eval      X03Importo= WrkImporto
234400980929     C***                eval      X03Cambio = MixPerOpe/100
234500980929     C***                eval      X03Modalit= '0'
234600980929     C***                eval      X03Arroton= '0'
234700980929     C***                eval      X03NrDec  = NrDecMc
234800980929     C***                eval      X03BaseCam= 1
234900980929     C***                call      'X03MULTC'
235000980929     C***                PARM                    X03MULTCDS
235100980929     C***                eval      WrkImporto= X03Controv
235200960730     C*
235300980929     C                   Z-Add     WrkImporto    WImpInp
235400980929     C                   CallP     X0201MultC(WImpInp: MixPerOpe: 100:
235500980929     C                                        'H': '0': XScNrDecMC: WImpOut)
235600980929     C                   eval      WrkImporto= WImpOut
235700980929     C*
235800960730     C                   ENDSR
235900960726      /EJECT
236000960726     C************************************************************
236100960726     C* FINE PROGRAMMA
236200960726     C************************************************************
236300960726     C     ENDPGM        BEGSR
236400960917     C*
236500960923     C                   IF        ERRG01 = ' '
236600960726     C*
236700960726     c* eseguo ciclo di lettura su ndgco per azione per flaggare con
236800960726     C* 'A' di annullato quei record non trovati per l'aggiornamento
236900960926     c                   exsr      fleggaGco
237000960726     C*
237100960726     C* chiamo routine per chiudere i file aperti da x20
237200960726     c* In batch si può evitare, ma in interattivo è da fare
237300960726     c* perchè è possibile leggere i file in qtemp o quelli residenti a
237400960726     c* seconda della richiesta dei parametri
237500960726     C                   IF        interatg01 = *on
237600960726     C                   EXSR      ChiudeX20
237700960726     c                   endif
237800960923     c*
237900960923     c                   endif
238000960923     C*
238100960802     C* Chiudo i file per lo stesso motivo di prima
238200960802     C                   IF        interatg01 = *on
238300960802     c                   close     ndgco01l
238400960802     c                   close     ndgco04l
238500960919     c                   close     NDGCO04L
238600960919     c                   close     NDPPA01L
238700960919     c                   close     NDPPS01L
238800960919     c                   close     NDMIX01L
238900960802     c                   eval      $openGco = *off
239000960802     c                   endif
239100960726     c*
239200960726     c* ripasso campi valorizzati
239300960726     c                   movel     ndcg01ds      kpjbu
239400960917     c*
239500960917     c* riimposto società originaria se
239600960917     C* società se diversa dalla impostata
239700960917     C* (serve in interattivo)
239800960917     c                   if        societaG01 <> savsocieta
239900960917     C                   MOVEL     'SOC001'      TIPXSC
240000960917     C                   MOVEL     Savsocieta    SOCXSC
240100960917     C                   EXSR      REPSOC
240200960917     C                   MOVEL     XSOCDS        SOC001
240300960917     c                   endif
240400090504
240500090504>>>>>C* Cancello l'area dati creata all'inizio.
240600090504>>>>>C                   IF        YAmerighi = *ON
240700090504>>>>>C                   EVAL      YCmd = 'DLTDTAARA'
240800090504>>>>>C                             + ' DTAARA(QTEMP/YOMEFAASC)'
240900090504>>>>>C                   CALL      'QCMDEXC'
241000090504>>>>>C                   PARM                    YCmd            256
241100090504>>>>>C                   PARM      256           YLen             15 5
241200090504>>>>>C                   ENDIF
241300090504
241400960726     C*
241500960918     C                   SETON                                        RT
241600960726     C                   RETURN
241700960726     C*
241800960726     C                   ENDSR
241900960130      /EJECT
242000960130      ************************************************************
242100960130      * Definizioni e inizializzazioni
242200960923      * vengono eseguiti in questa routine i reperimenti dei dati con
242300960923      * la società del JOB perchè non può più cambiare.
242400960130      ************************************************************
242500960130     C     *INZSR        BEGSR
242600000117C1309C     *DMY          Move      Udate         ZDateISO
242700000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
242800960130     C*
242900951214     C     *ENTRY        PLIST
243000951214     C                   PARM                    KPJBA
243100960628     c* UDATE
243200960628     C                   CALLb     'XDT4000'
243300960628     C                   PARM                    XDTAMG           10
243400000117C1309C*****              PARM      UDATE         XDTGMA            6 0
243500000117C1309C                   PARM      ZDate         XDTGMA            6 0
243600960628     C                   PARM      4             XDTSTA            1 0
243700960628     C                   MOVEL     XDTAMG        UDATEISO
243800960702     c*
243900960802     C*
244000960802     C* Imposto i flag di apertura ad off la prima volta che è stato chiamat
244100960802     c                   move      *off          $openGco          1
244200960919     C*
244300960923     c* questo movel serve al pgm se batch, che così non ripassa più di qua
244400960923     c* e reperisce i dati della società di elaborazione una sola volta
244500960919     C*
244600960919     C                   movel     kpjbu         NDCG01DS
244700960722     C*
244800960923     C* Reperimento dati società :in INTERATTIVO con quella cui sono
244900960923     C* associeta , in BATCH con quella che arriva nella DS (altrimenti
245000960923     c* prende quella di default del profilo) Lo faccio adesso così in
245100960923     c* batch non lo fa più.
245200960923     c*
245300960919     c                   if         interatg01 = *on
245400960919     C*
245500960722     C                   MOVEL     'SOC001'      TIPXSC
245600960722     C                   MOVEL     *blanks       SOCXSC
245700960722     C                   EXSR      REPSOC
245800960722     C     RTNXSC        IFNE      '1'
245900960722     C                   MOVEL     XSOCDS        SOC001
246000960917     c* Salvo società originaria
246100960917     c                   move      xscsoc        savsocieta
246200960722     C                   ELSE
246300970203     c                   eval      Errg01 = '1'
246400960722     C                   MOVE      *ON           $FINE
246500960722     C                   EXSR      ENDPGM
246600960722     C                   ENDIF
246700960919     C*
246800960919     C                   else
246900960919     C*
247000960919     C                   MOVEL     'SOC001'      TIPXSC
247100960919     C                   MOVEL     SocietaG01    SOCXSC
247200960919     C                   EXSR      REPSOC
247300960919     C     RTNXSC        IFNE      '1'
247400960919     C                   MOVEL     XSOCDS        SOC001
247500960919     c* Salvo società originaria (che in questo caso è identica)
247600960919     c                   move      xscsoc        savsocieta
247700960919     C                   ELSE
247800970203     c                   eval      Errg01 = '1'
247900960919     C                   MOVE      *ON           $FINE
248000960919     C                   EXSR      ENDPGM
248100960919     C                   ENDIF
248200960919     c*
248300960919     C                   endif
248400980929     c*
248500960923     C* Reperisco dati moneta di conto della società del job
248600960918     C* Moneta di conto
248700980929     C***                MOVE      XSCSOC        X01societa
248800980929     C***                MOVEL     xscdiv        X01divisa
248900980929     C***                MOVEL     '1'           X01flgcam
249000980929     c***                eval      x01dtrifca = dtregdG01
249100980929     C***                CALL      'X01CAMBI'
249200980929     C***                PARM                    X01CambiDs
249300980929     C***                Z-add     x01nrdec      NrDecMc
249400960923     c*
249500960923     C* Reperisco dati società (così se batch lo fa una sola volta)
249600960923     c                   EXSR      RepDatiSoc
249700970120     c*
249800970120     c* Reperisce con il codice versione i valori di alcuni campi
249900970120     c* e le regole per comporre la chiave
250000970120     c                   EXSR      RepCdVers
250100001102     C*
250200001102D0747C* Reperisco libreria di NDSCP01L
250300001102  "  c                   call      'X73RTVOBJD'
250400001102  "  C                   parm      'NDSCP01L'    Oggetto          10
250500001102  "  C                   parm      '*FILE'       Tipo              7
250600001102  "  C                   parm                    LibreriaP        10
250700001102  "  C                   parm                    Attributi        10
250800001102  "  C* Reperisco libreria di NDSCD01L
250900001102  "  c                   call      'X73RTVOBJD'
251000001102  "  C                   parm      'NDSCD01L'    Oggetto          10
251100001102  "  C                   parm      '*FILE'       Tipo              7
251200001102  "  C                   parm                    LibreriaD        10
251300040305D0747C                   parm                    Attributi        10
251400960918     C*
251500960918     C                   ENDSR
251600960726      /EJECT
251700960726      ************************************************************
251800960726      * INIZIALIZZAZIONE VARIABILI
251900960923      * viene subito eseguito il cambio lista di librerie se società
252000960923      * diversa, e adesso xscsoc e la societag01 sono uguali.
252100960726      ************************************************************
252200960726     C     INZVAR        BEGSR
252300960726     C*
252400960726     C                   movel     kpjbu         NDCG01DS
252500090504
252600090504>>>>> * Versioni di Amerighi.
252700090504>>>>>C                   MOVEL     *OFF          YAmerighi         1
252800090504>>>>>C     CdVersG01     IFEQ      'GENN'
252900090504>>>>>C     CdVersG01     OREQ      'FEBB'
253000090504>>>>>C     CdVersG01     OREQ      'MARZ'
253100090504>>>>>C     CdVersG01     OREQ      'APRI'
253200090504>>>>>C     CdVersG01     OREQ      'MAGG'
253300090504>>>>>C     CdVersG01     OREQ      'GIUG'
253400090504>>>>>C     CdVersG01     OREQ      'LUGL'
253500090504>>>>>C     CdVersG01     OREQ      'AGOS'
253600090504>>>>>C     CdVersG01     OREQ      'SETT'
253700090504>>>>>C     CdVersG01     OREQ      'OTTO'
253800090504>>>>>C     CdVersG01     OREQ      'NOVE'
253900090504>>>>>C     CdVersG01     OREQ      'DICE'
254000090504>>>>>C                   EVAL      YAmerighi = *ON
254100090504>>>>>C                   ENDIF
254200090504>>>>>C* Per alcune versioni scarto gli effetti
254300090504>>>>>C* creando un'area dati in QTEMP che modificherà
254400090504>>>>>C* il comportamento di X20SCADR.
254500090504>>>>>C                   IF        YAmerighi = *ON
254600090504>>>>>C                   EVAL      YCmd = 'CRTDTAARA'
254700090504>>>>>C                             + ' DTAARA(QTEMP/YOMEFAASC)'
254800090504>>>>>C                             + ' TYPE(*CHAR) LEN(1) VALUE(''1'')'
254900090504>>>>>C                             + ' TEXT(''Ometti effetti a scadere'')'
255000090504>>>>>C                   CALL      'QCMDEXC'
255100090504>>>>>C                   PARM                    YCmd            256
255200090504>>>>>C                   PARM      256           YLen             15 5
255300090504>>>>>C                   ENDIF
255400090504
255500050110D1721c* pulizia campi
255600050110  "  c                   clear                   wrkIsca01
255700050110  "  c                   clear                   wrkIsca02
255800050110  "  c                   clear                   wrkIsca03
255900050110  "  c                   clear                   wrkIsca04
256000050110  "  c                   clear                   wrkIsca05
256100050110  "  c                   clear                   wrkIsca06
256200050110  "  c                   clear                   wrkIsca07
256300050110  "  c                   clear                   wrkIsca08
256400050110  "  c                   clear                   wrkIsca01D
256500050110  "  c                   clear                   wrkIsca02D
256600050110  "  c                   clear                   wrkIsca03D
256700050110  "  c                   clear                   wrkIsca04D
256800050110  "  c                   clear                   wrkIsca05D
256900050110  "  c                   clear                   wrkIsca06D
257000050110  "  c                   clear                   wrkIsca07D
257100050110D1721c                   clear                   wrkIsca08D
257200960923     c*
257300960923     C* Reperimento dati società se diversa dalla impostata
257400960919     c*
257500960919     c                   if        societaG01 <> xscsoc or
257600960923     C                             xscsoc     = *blanks
257700960919     C                   MOVEL     'SOC001'      TIPXSC
257800960919     C                   MOVEL     SocietaG01    SOCXSC
257900960919     C                   EXSR      REPSOC
258000960919     C     RTNXSC        IFNE      '1'
258100960919     C                   MOVEL     XSOCDS        SOC001
258200960919     C                   ELSE
258300970203     c                   eval      Errg01 = '1'
258400960919     C                   MOVE      *ON           $FINE
258500960919     C                   EXSR      ENDPGM
258600960919     C                   ENDIF
258700960923     C* Reperisco dati nuova società
258800960923     c                   exsr      RepDatiSoc
258900960919     C                   ENDIF
259000960802     C*
259100960919     c* SE file chiusi, li apro nella società di elaborazione (dopo cambio
259200960919     c* sistema informativo)
259300960802     c                   if        $openGCO = *off
259400960802     c                   open      NDGCO01L
259500960802     c                   open      NDGCO04L
259600960919     c                   open      NDPPA01L
259700960919     c                   open      NDPPS01L
259800960919     c                   open      NDMIX01L
259900960910     c                   eval      $openGco = *on
260000960802     C                   endif
260100960923     c*
260200990608     C* Reperimento tipo entità dal Mix di partita solo per cli/for
260300001107D0747c* solo una volta in batch, in quanto non cambia
260400001107     c                   if        interatG01=*on or
260500001107     c                             interatG01=*off and flgEntita <> *on
260600990608     c                   if        SnaturaG01 = 'C' or SnaturaG01 = 'F'
260700960923     C                   EXSR      REPTipEnt
260800001107D0747c                   eval      flgEntita=*on
260900990608     c                   endif
261000001107D0747c                   endif
261100960726     C*
261200960726     C* IMPOSTAZIONI FLAG DI LAVORO
261300960726     C*
261400960726     C* Se non richiesti parametri particolari
261500001107     C* imposto flag per chiamare routine x20 per conto e non per partita
261600960729     C*
261700960726     c                   eval      flgConto = *oFF
261800960726     C                   if        dtperdaG01 = DatMIN and
261900960730     C                             (dtperaG01  = Datmax or
262000960730     C                             dtperaG01  = Datmax1) and
262100960726     C                             ctgan1g01 = *blanks and
262200960726     C                             ctgan2g01 = *blanks and
262300960726     C                             ctgan3g01 = *blanks and
262400960726     C                             kscageg01 = *blanks and
262500960729     C                             (divisaG01 = *blanks or divselg01 = 'I')
262600960726     c                   eval      flgConto = *on
262700960726     c                   endif
262800001107     c*
262900001107D0747c*  Imposto flag di calcolo scadenzario
263000001107  "  c*  se sono stati modificati i dati rispetto all'ulimo calcolo es eg.
263100001107  "  c*  imposto flag per non rifare gli stessi test 3 volte
263200001107  "  c                   eval      FlgScadenz= *oFF
263300001107  "  c*
263400001107  "  c*  serve  solo nel caso di calcolo interattivo, in quanto in batch
263500001107  "  c*  il calcolo dello scadenzario viene effettuato sempre
263600001107  "  c                   if        InteratG01=*on
263700010608  "  c* se modificati i parametri
263800001107  "  c                   if        DtScaG01  <> DtSca    or
263900001107  "  c                             DtPerDaG01<> DtPerDa  or
264000001107  "  c                             DtPerAg01 <> DtPerA   or
264100001107  "  c                             CtgAn1G01 <> *blanks      or
264200001107  "  c                             CtgAn2G01 <> *blanks      or
264300001107  "  c                             CtgAn3G01 <> *blanks      or
264400001107  "  c                             Tb8PerG01 <> tb8per       or
264500001107  "  c                             (DtCamG01  <> DtCam       and
264600001107  "  c                             TpCamG01  <> ' ')         or
264700001107  "  c                             TpCamG01  <> TpCam        or
264800001107  "  c                             MovdefG01 <> MovDef       or
264900001107  "  c                             MovprovG01<> MovProv      or
265000001107  "  c                             MovgesG01 <> MovGes       or
265100001107  "  c                             DtRegDg01 <> DtRegD       or
265200001107  "  c                             DtRegPg01 <> DtRegP       or
265300010608  "  c                             DtRegGg01 <> DtRegG       OR
265400010608d0747c* o se richiesto F5
265500010608D0946C                             TastoF5G01 = *on
265600070301D2133C                             OR ClcContG01= *on
265700010608d0747c                   eval      flgScadenz= *on
265800001107  "  c                   endif
265900001107D0747c                   endif
266000960726     c*
266100960726     c* Pulizia flag
266200960729     c                   eval      flgMix = *off
266300960726     c                   eval      flgPPS = *off
266400960729     c                   eval      flgx20 = *off
266500001106     c*
266600001106D0747c* Eseguo ovrdbf dei file dello scadenzario
266700001107  "  c* in interattivo sempre
266800001106  "  c                   if        interatG01= *on or
266900001107  "  c* in batch solo una volta (legge i file residenti se notturno,
267000001107  "  c* altrimenti i file in qtemp se da menu'
267100001106  "  c                             interatG01<>*on  and flgover<> *on
267200001106  "  c                   eval      CMD1='Ovrdbf file(NDSCP01W) tofile('+
267300001106  "  C                             %trim(LibreriaP) +'/NDSCP01L) ovrscope(*job)'
267400001106  "  C                   call      'QCMDEXC'                            25
267500001106  "  C                   parm                    cmd1             80
267600001106  "  C                   parm      80            len              15 5
267700001106  "  c                   eval      CMD1='Ovrdbf file(NDSCD01W) tofile('+
267800001106  "  C                             %trim(LibreriaD) +'/NDSCD01L) ovrscope(*job)'
267900001106  "  C                   call      'QCMDEXC'                            25
268000001106  "  C                   parm                    cmd1             80
268100001106  "  C                   parm      80            len              15 5
268200001107  "  c                   eval      flgOver = *on
268300001106  "  c                   endif
268400001027D0747c
268500960730     c                   endsr
268600960726     C*
268700960923     C*
268800960923     C/EJECT
268900960923     C************************************************************
269000960923     C* REPERIMENTO dati della società di elaborazione
269100960923     C************************************************************
269200960923     C     REPDatisoc    BEGSR
269300960923     c*
269400960923     C* trovo esercizio contabile dalla data limite registrazione definit.
269500960923     C                   move      societaG01    X10societa
269600960923     C                   move      ctbg01        X10ctb
269700960923     C                   move      dtRegdg01     x10data
269800960923     C                   call      'X10ESER'
269900960923     C                   parm                    x10eseds
270000960923     C*
270100960923     C* trovo esercizio statistico dalla data limite registrazione definit.
270200960923     C                   move      societaG01    X50societa
270300960923     C                   move      ctbg01        X50ctb
270400960923     C                   move      dtRegdg01     x50data
270500960923     C                   call      'X50ESER'
270600960923     C                   parm                    x50eseds
270700990607     c* Trovo periodo
270800990607     c                   exsr      CalcPeriod
270900960923     C*
271000960923     C* Reperimento parametri gestione modulo credito/debito
271100960923     C                   MOVEL     *BLANK        XPARIC
271200960923     C                   MOVEL     societaG01    XPASOC
271300960923     C                   MOVEL     'CREDDEB '    XPAPAR
271400960923     C                   MOVEL     *BLANK        XPAOUT
271500960923     C                   EXSR      REPPAR
271600960923     C     XPAERR        IFEQ      '0'
271700960923     C                   MOVEL     XPAOUT        ANP016DS
271800960923     C                   ELSE
271900960923     C                   SETON                                        LR
272000960923     C                   RETURN
272100960923     C                   ENDIF
272200960923     C*
272300960923     C* Reperimento parametri data area Mix di partita
272400960923     C*
272500960923     C                   MOVEL     *BLANK        XPARIC
272600960923     C                   MOVEL     SocietaG01    XPASOC
272700960923     C                   MOVEL     'MIXPART '    XPAPAR
272800960923     C                   MOVEL     *BLANK        XPAOUT
272900960923     C                   EXSR      REPPAR
273000960923     C     XPAERR        IFEQ      '0'
273100960923     C                   MOVEL     XPAOUT        ANP011DS
273200960923     C                   ELSE
273300960923     C                   SETON                                        LR
273400960923     C                   RETURN
273500960923     C                   ENDIF
273600960923     C*
273700960923     C                   ENDSR
273800990607      /EJECT
273900990607      ************************************************************
274000990607      * Calcolo periodo
274100990607      ************************************************************
274200990607     C     CalcPeriod    BEGSR
274300990607     c                   clear                   wrkperiodo
274400990607     c*
274500990607     c                   select
274600990607     C*mesi
274700990607     c                   when      X50StatPer = '1'
274800990607     c                   move      DtRegDg01     dataiso
274900990607     c                   extrct    dataiso:*M    wrkperiodo
275000990607     C*periodi mensili progressivi
275100990607     c                   when      X50StatPer = '2'
275200990607     c                   move      DtregDG01     dataiso
275300990607     c                   extrct    dataiso:*M    wrkmese           3 0
275400990607     c                   move      x50dtines     dataiso
275500990607     c                   extrct    dataiso:*M    wrkmeseda         3 0
275600990630     c                   if        wrkmeseda > wrkmese
275700990630     c                   add       12            wrkmese
275800990630     c                   endif
275900990607     c                   z-add     1             $x                3 0
276000990630     c                   do        24
276100990607     c                   if        wrkmese = wrkmeseda
276200990607     c                   z-add     $x            wrkperiodo
276300990607     c                   leave
276400990607     c                   else
276500990607     c                   add       1             wrkmeseda
276600990607     c                   add       1             $x
276700990607     c                   endif
276800990607     c                   enddo
276900990607     c
277000990607     C*giorni (trasformo data in giuliano)
277100990607     c                   when      X50StatPer = '3'
277200990607     c                   move      DtRegDG01     dataiso
277300990607     c     *jul          move      dataiso       wrkaaggg          5 0
277400990607     c                   move      wrkaaggg      wrkperiodo
277500990607     C*
277600990607     c                   endsl
277700990607     c*
277800990607     c                   ENDSR
277900960923     C/EJECT
278000960923     C************************************************************
278100960923     C* REPERIMENTO tipo entità dal mix di partita
278200960923     c* con la società di elaborazione
278300960923     C************************************************************
278400960923     C     REPTipEnt     BEGSR
278500960923     c                   clear                   TIPENT
278600960923     C
278700960923     C*Con il cod.dimensione  si ottengono da tab.G86 gli 8 cod.entità
278800960923     c* per clienti o fornitori
278900960923     C                   MOVE      xsccut        XTBUTE
279000960923     C                   MOVE      '1'           XTBRIC
279100960923     C                   MOVE      societag01    XTBAZI
279200960923     C                   MOVE      *ZERO         XTBKEY
279300960923     C                   MOVE      'G86'         XTBCOD
279400960923     c                   if        snaturag01 = 'C'
279500960923     C                   MOVE      Dimcli011     XTBKEY
279600960923     c                   else
279700960923     C                   MOVE      Dimfor011     XTBKEY
279800960923     c                   endif
279900960923     C
280000960923     C                   CALLB     'XATB'
280100960923     C                   PARM                    XATBDS
280200960923     C     XTBERR        IFEQ      '0'
280300960923     C                   MOVEL     XTBUNI        AngG86ds
280400960923   2-C
280500960923     C*Valorizzo la schiera per i cod.entità
280600960923     C                   MOVE      TE1G86        TipEnt(1)
280700960923     C                   MOVE      TE2G86        TipEnt(2)
280800960923     C                   MOVE      TE3G86        TipEnt(3)
280900960923     C                   MOVE      TE4G86        TipEnt(4)
281000960923     C                   MOVE      TE5G86        TipEnt(5)
281100960923     C                   MOVE      TE6G86        TipEnt(6)
281200960923     C                   MOVE      TE7G86        TipEnt(7)
281300960923     C                   MOVE      TE8G86        TipEnt(8)
281400960923     C                   Endif
281500960923     C
281600960923     C
281700960923     C                   ENDSR
281800960923     c/EJECT
281900960726     C************************************************************
282000960923     C* Reperisce codice versione
282100960726     C************************************************************
282200960726     C     RepCdVers     BEGSR
282300960726     C*
282400960926     C*Aggancio ndgva per sapere come devo poi comporre le chiavi del
282500960926     C*file di sintesi (se non esiste codice versione, errore ed esco
282600960726     C*
282700960726     c                   eval      GvaTpAcc = CDVersg01
282800960926     c     GvaTpAcc      chain     ndgva01L                           21
282900960726     c                   if        *in21 = *on
283000960726     c                   eval      errG01 = '3'
283100960726     c                   exsr      endpgm
283200991201     C                   else
283300991201     c* salvo la ds in entrata
283400991201     c                   movel     ndcg01ds      ndcg01ds1
283500991201     c* reperisco e salvo dati ultimo calcolo eseguito
283600991201     c                   if        GvaDatiUc <> *blanks
283700991201     c                   movel     GvaDatiUc     ndcg01ds
283800991201     c                   endif
283900991201     c* metto dati nei campi di confronto (o quelli dell'ultimo calcolo o
284000991201     c* quelli attuali)
284100991201     c                   eval      DtSca = DtScaG01
284200991201     c                   eval      DtPerda = DtPerDaG01
284300991201     c                   eval      DtPerA = DtPerAG01
284400991201     c                   eval      CtgAn1 = CtgAn1G01
284500991201     c                   eval      CtgAn2 = CtgAn2G01
284600991201     c                   eval      CtgAn3 = CtgAn3G01
284700991201     c                   eval      Tb8Per = Tb8PerG01
284800991201     c                   eval      DtCam  = DtCamG01
284900991201     c                   eval      TpCam  = TpCamG01
285000991201     c                   eval      MovDef = MovDefG01
285100991201     c                   eval      MovProv= MovProvG01
285200991201     c                   eval      MovGes = MovGesG01
285300991201     c                   eval      DtRegD = DtRegDG01
285400991201     c                   eval      DtRegP = DtRegPG01
285500991201     c                   eval      DtRegG = DtRegGG01
285600991201     c* rimetto ds originale
285700991201     c                   movel     ndcg01ds1     ndcg01ds
285800991201     c                   endif
285900960726     c*
286000960726     C* Memorizzo i 4 flag di tipo gestione importo,. che possono arrivare
286100960726     c* dal tipo versione o, se blank, dai parametri moduli
286200960726     c*
286300960726     c                   if        gvagirisc <> *blanks
286400960726     c                   eval      wrkgirisc = gvagirisc
286500960726     c                   else
286600960726     c                   eval      wrkgirisc = girisc016
286700960726     c                   endif
286800960726     c                   if        gvagiinasd<> *blanks
286900960726     c                   eval      wrkgiinasd= gvagiinasd
287000960726     c                   else
287100960726     c                   eval      wrkgiinasd= giinasd016
287200960726     c                   endif
287300960726     c                   if        gvagiinasP<> *blanks
287400960726     c                   eval      wrkgiinasP= gvagiinasP
287500960726     c                   else
287600960726     c                   eval      wrkgiinasP= giinasP016
287700960726     c                   endif
287800070510D2189c                   if        GvaRepAgen<>*blanks
287900070510  "  c                   eval      WrkRepAgen= GvaRepAgen
288000070510  "  c                   else
288100070510  "  c                   eval      WrkRepAgen= RepAgen016
288200070510D2189c                   endif
288300000915c1390c*
288400000915  "  c* aggancio esclusione partite
288500000915  "  c                   eval      SiEsclus = *off
288600000915  "  c     GvaTpAcc      chain     ndgvE01L                           21
288700000915  "  c                   if        *in21=*off
288800000915  "  c                   eval      SiEsclus = *on
288900000915  "  c                   endif
289000000915c1390c*
289100960726     C                   ENDSR
289200000915     C/EJECT
289300000915c1390C************************************************************
289400000915  "  C* Testa esclusione partite con validatore regola
289500000915  "  C************************************************************
289600000915  "  C     TestaEscl     BEGSR
289700000915  "  c*
289800000915  "  C                   movel     'NDPPA000'    fil960(1)
289900000915  "  C                   movel     NDPPA         dat960(1)
290000000915  "   *
290100000915  "  C                   move      SocietaG01    soc960
290200000915  "  C                   z-add     GveNumero     lan960
290300000915  "  C                   z-add     0             num960
290400000915  "  C                   move      *on           rok960
290500000915  "  C                   move      *off          err960
290600000915  "  C                   move      *on           ott960
290700000915  "   *
290800000915  "  C                   callB     'ANA960R'
290900000915  "  C                   parm                    soc960            3
291000000915  "  C                   parm                    lan960            9 0
291100000915  "  C                   parm                    fil960
291200000915  "  C                   parm                    dat960
291300000915  "  C                   parm                    num960            3 0
291400000915  "  C                   parm                    rok960            1
291500000915  "  C                   parm                    err960            1
291600000915  "  C                   parm                    ott960            1
291700000915  "  c*
291800000915c1390c                   endsr
291900951212      /EJECT
292000950626      ************************************************************
292100950626      * Reperimento dati società
292200950626      ************************************************************
292300950626     C     REPSOC        BEGSR
292400950626      *
292500950831     C                   CALLB     'XSOC'
292600950626     C                   PARM                    TIPXSC            6
292700950626     C                   PARM                    SOCXSC            3
292800950626     C                   PARM                    CDSXSC            9 0
292900950626     C                   PARM                    MODXSC            3
293000950626     C                   PARM      *BLANKS       RTNXSC            1
293100950626     C                   PARM                    XSOCDS
293200950626     C                   PARM                    KPJBA
293300950626      *
293400950626     C                   ENDSR
293500960628     C/EJECT
293600960628     C************************************************************
293700960628     C* REPERIMENTO PARAMETRI GESTIONE
293800960628     C************************************************************
293900960628     C     REPPAR        BEGSR
294000960628     C*
294100960923     C                   CALLB     'XPAR'
294200960628     C                   PARM                    XPASOC            3
294300960628     C                   PARM                    XPAPAR            8
294400960628     C                   PARM                    XPAOUT
294500960628     C                   PARM                    XPAERR            1
294600960628     C                   PARM                    XPARIC            1
294700960628     C*
294800960628     C                   ENDSR
294900960123     C/EJECT
295000960123     C************************************************************
295100960123     C* DEFINIZIONE KLIST
295200960123     C************************************************************
295300960123     C     DefKlist      BEGSR
295400960123     C*
295500960123     C* klist
295600960123     C*
295700960627     C* NDPPA01L
295800960627     C     K05PPA01      KLIST
295900960307     C                   KFLD                    PPASocieta
296000960307     C                   KFLD                    PPActb
296100960307     C                   KFLD                    PPakcc
296200960307     C                   KFLD                    PPaksc
296300960307     C                   KFLD                    PPaDtpar
296400960627     C     K04PPA01      KLIST
296500960307     C                   KFLD                    PPASocieta
296600960307     C                   KFLD                    PPActb
296700960307     C                   KFLD                    PPakcc
296800960307     C                   KFLD                    PPaksc
296900960627     C     K03PPA01      KLIST
297000960307     C                   KFLD                    PPASocieta
297100960307     C                   KFLD                    PPActb
297200960307     C                   KFLD                    PPakcc
297300960628     c*
297400960627    >C* ndpps01l
297500960627    >C     K02PPS01      KLIST
297600960627    >C                   KFLD                    PPSSYS
297700960627    >C                   KFLD                    PPSNRASINT
297800960628    >C* ndGCO01l
297900961216    >C     K23GCO01      KLIST
298000961216    >C                   KFLD                    GCOCdVers
298100960628    >C                   KFLD                    GCOSocieta
298200960628    >C                   KFLD                    GCOCTB
298300960628    >C                   KFLD                    GCOSnatura
298400960628    >C                   KFLD                    GCOKCC
298500960628    >C                   KFLD                    GCOKSC
298600960628    >C                   KFLD                    GCOUnita
298700960628    >C                   KFLD                    GCODivisa
298800960628    >C                   KFLD                    GCOAgente
298900960628    >C                   KFLD                    GCOEntit1
299000960628    >C                   KFLD                    GCOEntit2
299100960628    >C                   KFLD                    GCOEntit3
299200960628    >C                   KFLD                    GCOEntit4
299300960628    >C                   KFLD                    GCOEntit5
299400960628    >C                   KFLD                    GCOEntit6
299500960628    >C                   KFLD                    GCOEntit7
299600960628    >C                   KFLD                    GCOEntit8
299700960628    >C                   KFLD                    GCOCtgAn01
299800960628    >C                   KFLD                    GCOCtgAn02
299900960628    >C                   KFLD                    GCOCtgAn03
300000960628    >C                   KFLD                    GCODtPar
300100960628    >C                   KFLD                    GCONrPar
300200960628    >C                   KFLD                    GCOSerPar
300300960709    >C* ndGCO04l
300400961216    >C     K06GCO04      KLIST
300500961216    >C                   KFLD                    GCOCdVers
300600960904    >C                   KFLD                    GCOSocieta
300700960904    >C                   KFLD                    GCOCTB
300800960904    >C                   KFLD                    GCOSnatura
300900960904    >C                   KFLD                    GCOKCC
301000960924    >C                   KFLD                    GCOKsC
301100961216    >C     K07GCO04      KLIST
301200961216    >C                   KFLD                    GCOCdVers
301300960709    >C                   KFLD                    GCOSocieta
301400960904    >C                   KFLD                    GCOCTB
301500960904    >C                   KFLD                    GCOSnatura
301600960904    >C                   KFLD                    GCOKCC
301700960924    >C                   KFLD                    GCOKSC
301800960709    >C                   KFLD                    GCOnraz
301900960628    >C* ndmix01L
302000960628    >C     K03mix01      KLIST
302100960628    >C                   KFLD                    mixSYS
302200960628    >C                   KFLD                    mixNRASINT
302300960628    >C                   KFLD                    mixdtval
302400960718    >C     K02mix01      KLIST
302500960718    >C                   KFLD                    mixSYS
302600960718    >C                   KFLD                    mixNRASINT
302700960702    >C* ndscd01l
302800960920    >C     K08scd01      KLIST
302900960726    >C                   KFLD                    scdSocieta
303000960726    >C                   KFLD                    ScdCTB
303100960920    >C                   KFLD                    Scdtpscad
303200960726    >C                   KFLD                    ScdKCC
303300960726    >C                   KFLD                    ScdKSC
303400960702    >C                   KFLD                    ScdDtpar
303500960702    >C                   KFLD                    ScdNrpar
303600960702    >C                   KFLD                    ScdSerPar
303700960709    >C* ndscP01l
303800960920    >C     K09scP01      KLIST
303900960709    >C                   KFLD                    scPSocieta
304000960709    >C                   KFLD                    ScPCTB
304100960920    >C                   KFLD                    ScPtpscad
304200960709     C                   KFLD                    SCPDETT
304300960709    >C                   KFLD                    ScPKCC
304400960709    >C                   KFLD                    ScPKSC
304500960709    >C                   KFLD                    ScPDtpar
304600960709    >C                   KFLD                    ScPNrpar
304700960709    >C                   KFLD                    ScPSerPar
304800020917D1199c*
304900020917  "  c     K03Csc01      KLIST
305000020917  "  C                   KFLD                    CscSocieta
305100020917  "  C                   KFLD                    CscTpScad
305200020917D1199C                   KFLD                    CscKCC
305300960628     C*
305400960628     C* PLIST per NDVASOG
305500960628     C     PDVASOG       PLIST
305600960628     C                   PARM                    DVARIC
305700960628     C                   PARM                    DVASOCIETA
305800960628     C                   PARM                    DVAUNITA
305900960628     C                   PARM                    DVASTRUTT
306000960628     C                   PARM                    DVADTRIF
306100960628     C                   PARM                    DVALENOUT
306200960628     C* Se si accede con il codice cli/for impostare i campi seguenti
306300960628     C* se si accede con il soggetto ometterli e impostare gli altri
306400960628     C                   PARM                    DVASNATURA
306500960628     C                   PARM                    DVACODICE
306600960628     C                   PARM                    DVALINEAV
306700960628     C                   PARM                    DVAFILIALE
306800960628     C                   PARM                    *omit
306900960628     C                   PARM                    DVATPIND
307000960628     C                   PARM                    DVACDIND
307100960628     C                   PARM                    DVAERRORE
307200960628     C                   PARM                    DVAOUTPUT
307300960627     c*
307400960123     C                   ENDSR
307500960712     c/EJECT
307600960715**  CMD
307700960715DLTF FILE(QTEMP/NDSC*)
