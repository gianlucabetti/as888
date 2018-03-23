000100030129     H*PARMS OPTION(*NOXREF *NODEBUGIO)
000200030129     H*PARMS DFTACTGRP(*NO)
000300030129     H*PARMS BNDDIR(PJXBND PJCBND)
000400030129     H*PARMS ACTGRP(QILE)
000500030129     H*PARMS CVTOPT(*NONE)
000600030129      *****************************************************************
000700030129     H DECEDIT('0,') DATEDIT(*DMY.)
000800030129      *****************************************************************
000900030129      *
001000030129      *                    Comunicazione annuale iva
001100030129      *            ---------------------------------------------
001200030129      *****************************************************************
001300030129      *****************************************************************
001400030210      * (join tra ndiva e ndivc) Soc/TpReg/Libro/DtAnnot
001500030210     FNDIVA11J  IF   E           K DISK
001600050202C1814 * (join tra ndiva e ndivc) Soc/TpReg/Libro/DtReg
001700050202C1814FNDIVA08J  IF   E           K DISK
001701050928C1867 * (join tra ndiva e ndivc) Soc/TpReg/Libro/DtRif
001702050928C1867FNDIVA02J  IF   E           K DISK
001800030129      * movimenti
001900030129     FNDMOV01L  IF   E           K DISK
002000030129      * anagr.associazione libri/riferim-aliquote
002100030129     FANALR01L  IF   E           K DISK
002200030129      * anagr.libri
002300030129     FANDLI01L  IF   E           K DISK
002400040218C1723X* anagr.riferimenti/aliquote
002500040218  "  X*ANRIV03L  IF   E           K DISK
002600040218  "   * SOC/TPR/ALI/RIF
002700040218C1723FANRIV01L  IF   E           K DISK
002800030129      *
002900030129     FNDC181P   O    E             printer oflind(*in39) usropn
003000030129      *****************************************************************
003100030129      * Passaggio Parametri
003200030129     D KPJBA         E DS
003300030129      *-------------------------------------------------------*
003400030129      * Dati di ambiente ottenuti da XSOC
003500030129     D SOC001        E DS                  EXTNAME(XSOC001DS)
003600030129     D    XScDare                     1    overlay(XScFgo:7)
003700030129     D    XScAvere                    1    overlay(XScFgo:8)
003701050930C1867D    XscIvaRgm                   1    overlay(XscFgo:9)                    Scadenz. retroattivo
003800030129      *-------------
003900030129      * DS Interna per dati di output di XSOC
004000030129     D XSOCDS          DS          1000
004100030129      *-------------------------------------------------------*
004200030129      * Reperimento Autorizzazioni
004300030129     D*AUTDS         E DS                  EXTNAME(XCHKAUTDS)
004400030129      *-------------
004500030129      * Maschera dati autorizzazioni
004600030129     D*xxxxxxDS      E DS
004700030129      *-------------------------------------------------------*
004800030129      * Reperimento nome PGM
004900030129     D STATUS         SDS           333
005000030129     D  DSPGM            *Proc
005100030129     D  Utente               254    263
005200030129      *-------------------------------------------------------*
005300030129      * Parametri che ricevo dal lancio
005400030129     D NDC181Ds      E DS
005500030129     D  Err181       E                     INZ('0')
005600030129      *-------------------------------------------------------*
005700030129      * Definizioni controllo causale
005800030129     D ANOPEDS       E DS                  Inz ExtName(ANOPE00F)
005900030129     D Esito           S              1
006000030129     D GesMsg          S              1
006100030129     D LenOut          S              9B 0
006200030129     D StrutturaO      S             10
006300030129     D Refresh         S              1
006400030129     D Uso             S              1
006500030129     D Output          S           4000
006600030129     D Causale         S              4
006700030129      *-------------------------------------------------------*
006800030129      * causali incontrate nell'elaborazione (x ctr autofattura)
006900030129     D  SkCaus         s                   dim(100) like(IVACausTes)
007000030129     D  SkTpDocI       s                   dim(100) like(OPETpDocI)
007100030129     D  $c             s              3s 0
007200030129     D  $Autofatt      s              1    inz('0')
007300030129      *-------------------------------------------------------*
007400030129      * Variabili di appoggio
007500030129      *-------------------------------------------------------*
007600030129     D $LibroElab      s                   like(IVALibro   )
007700030129     D $EndRegist      s              1
007800030129     D $EndLibro       s              1
007900030129     D SAVTpReg        s                   like(IVATpReg   )
008000030129     D SAVLibro        s                   like(IVALibro   )
008100030129     D $LibroSosp      s              1
008200030129     D $MovOk          s              1
008300030129     D $OpenPrtF       s              1    inz('0')
008400030129      *--------------------------------------------------------
008500030129      * Variabili che memorizzano calcoli parziali, da sommare
008600030129      * in chiamate successive
008700030129     D $OpAtt          s                   like(OpAtt181   )
008800030129     D $OpAttNI        s                   like(OpAttNI181 )
008900030129     D $OpAttEs        s                   like(OpAttEs181 )
009000030129     D $OpAttCI        s                   like(OpAttCI181 )
009100030129     D $OpPas          s                   like(OpPas181   )
009200030129     D $OpPasNI        s                   like(OpPasNI181 )
009300030129     D $OpPasEs        s                   like(OpPasEs181 )
009400030129     D $OpPasAI        s                   like(OpPasAI181 )
009500030129     D $TotVen         s                   like(TotVen181  )
009600030129     D $TotAcq         s                   like(TotAcq181  )
009700040213C1723 *----------------------------------------------------*
009800040213  "   * Memorizza se reperite causali per beni e servizi
009900040213  "  D $RepCauBS       s              1a
010000040213  "   *---------
010100040213  "   * Memorizza in ANMPE00F/MPEPAR le causali che distinguono
010200040213  "   * le cessioni/acquisti di beni da quelle di servizi:
010300040213  "   * MPEPAR (256 A) memorizza nei primi 112 caratteri da dx
010400040213  "   * le 28 causali per i beni, e dalla 128° alla 240° posizione,
010500040213  "   * le 28 causali per i servizi.
010600040224  "   * P.S:
010700040224C1723 * in MPEPAR c'è ancora lo spazio per 4 causali sui beni e
010800040224C1727X*ex C1723**4 causali sui servizi
010900040224  "   * 3 causali sui servizi
011000040224  "   * gli ultimi 4 caratteri di MPEPAR sono utilizzati per
011100040224  "   * flag generici;
011200040224  "   * l'ultimo carattere è impiegato per indicare se le causali
011300040224  "   * devono essere considerate nel calcolo della CAIVA
011400040224  "   *---------
011500040224  "   * se causale ok, movimento iva considerato per cess/acq intrac:
011600040224  "   * Causale ok se:
011700040224  "   * reperita classificaz. causali beni e servizi  e
011800040224  "   *   le causali NON devono essere CONSIDerate nel calcolo CAIVA
011900040224  "   *   oppure
012000040224  "   *   le causali devono essere CONSIDerate nel calcolo CAIVA e
012100040224  "   *   la causale (di riga se esiste ed è classificata
012200040224  "   *   oppure di testata) è classificata come causale per beni
012300040224  "  D $CauOK          s              1
012400040224C1727 *---------
012500040224C1723D XMpeDS        E DS                  INZ
012600040213  "  D XMpeDS1       E DS                  INZ
012700040213  "   *---------
012800040213  "   *  Dimensione skiere per gestione causali in ANMPE00F/MPEPAR
012900040213  "  D $DimSkMpe       c                   const(32)
013000040213  "   *---------
013100040213  "   * Beni Causale
013200040213  "  D DSBeniCa        DS
013300040213  "  D  SKBeniCa                           dim($DimSkMpe) like(IVACausTes)
013400040213  "   *----------------
013500040213  "   * Servizi Causale
013600040213  "  D DSServCa        DS
013700040213  "  D  SKServCa                           dim($DimSkMpe) like(IVACausTes)
013800040213  "   *---------
013900040213  "   *  tipo causale: 'B'ene o 'S'ervizio
014000040213  "  D $CauBS          s              2
014100040213C1723 *----------------------------------------------------*
014200050202C1814D $DtOrd          s               D
014300050202C1814 *----------------------------------------------------*
014302050930C1867D* Controlli validità legende
014303050930C1867D X07DS         E DS                  EXTNAME(X07VALDS)
014400030129      ******************************************************
014500030129      *                MAIN LINE PROGRAM
014600030129      ******************************************************
014700030129      *
014800030129      * Inizializzazione variabili
014900030129     C                   exsr      InzVar
015000030129      *
015100030129      * Sono chiamato solo per calcolo
015200030129     C                   if        Stampa181 = *off
015300030129     C                   exsr      CalcCAI
015400030129      * Sono chiamato solo per stampa
015500030129     C                   else
015600030129     C                   exsr      StampaCAI
015700030129     C                   endif
015800030129      *
015900030129      * Fine programma
016000030129     C                   exsr      EndPGM
016100030129      *
016200030129      ************************************************************
016300030129      * Fine Programma
016400030129      ************************************************************
016500030129     C     EndPgm        Begsr
016600030129      *
016700030129      * dati per comunicazione annuale iva
016800030129      * ripasso i dati calcolati (utili se sono chiamto solo x il calcolo)
016900030129     C                   z-add     $OpAtt        OpAtt181
017000030129     C                   z-add     $OpAttNI      OpAttNI181
017100030129     C                   z-add     $OpAttEs      OpAttEs181
017200030129     C                   z-add     $OpAttCI      OpAttCI181
017300030129     C                   z-add     $OpPas        OpPas181
017400030129     C                   z-add     $OpPasNI      OpPasNI181
017500030129     C                   z-add     $OpPasEs      OpPasEs181
017600030129     C                   z-add     $OpPasAI      OpPasAI181
017700030129     C                   z-add     $TotVen       TotVen181
017800030129     C                   z-add     $TotAcq       TotAcq181
017900030129      *
018000030129     C                   movel     NDC181ds      Kpjbu
018100030129      *
018200030129      * Chiudo come mi è stato detto dal chiamante
018300030129     C                   if         End181 = 'R'
018400030129     C                   eval       *InRT = *On
018500030129     C                   else
018600030129     C                   eval       *InLR = *On
018700030129     C                   endif
018800030129      *
018900030129     C                   return
019000030129      *
019100030129     C     XEndPgm       Endsr
019200030129      ************************************************************
019300030129      * Calcolo dati per Comunicazine Annuale Iva
019400030129      ************************************************************
019500030129     C     CalcCAI       Begsr
019600030129      *
019700030129      * lettura iniziale
019800030129     C                   exsr      SetRedIniz
019900030129      *
020000030129      * ciclo sui libri del registro ----------------------------
020100030129     C                   dow       $EndRegist   = *off
020200030129     C                   exsr      SetRedLib
020300030129      *
020400050202C1814 *   imposto dta ordinamento in base al tipo data richiesto
020500050202C1814C                   exsr      RepDtOrd
020600030129      *
020700030129      *   ciclo sui movimenti iva libro,-------------------------
020800050202C1814X*** con data liquidazione entro il periodo lanciato
020802050928C1867 **ex C1814 *** con data reg/annotaz. entro il periodo lanciato
020803050928C1867 *   con data reg/annotaz./liq entro il periodo lanciato
021000030129     C                   dow       $EndLibro = *off     and
021100050202C1814X**************************** IVADtAnnot <= DtF181
021200050202C1814C                             $DtOrd     <= DtF181
021300030129      *
021400030129      *
021500030129      *      seleziono dati del movimento
021600030129     C                   exsr      SelMov
021700030129      *      se dati del movimento ok
021800030129      *      sviluppo calcolo in base al singolo rec di ndiva00f
021900030129     C                   if        $MovOk = *on
022000030129     C                   exsr      CalcCAI_1
022100030129     C                   endif
022200030129      *
022300050202C1814 *   LETTURA movimenti iva libro
022301050928C1867C                   exsr      Reade_K03Iva
022302050928  "   *
022303050928  "  X* Specifiche asteriscate ristrutturate in routine 'Reade_K03Iva'
022304050928  "  X**ex C1814 ******richiesta ordinamento per data Registrazione
022401050928  "  X**ex C1814 ******* if        TipDat2181 = 'G'
022402050928  "  X**ex C1814 * K03IVA08      reade     NDIVA08J                       21
022601050928  "  X**ex C1814 **richiesta ordinamento per data Annotazione (o altro)
022602050928  "  X**ex C1814 ******* else
022900050928  "  X*****K03IVA11 **** reade     NDIVA11J                               21
022901050928  "  X**ex C1814 ******* endif
023100050928C1867X****************** eval      $EndLibro = *in21
023200050202C1814 *
023300050202  "   * imposto dta ordinamento in base al tipo data richiesto
023400050202C1814C                   exsr      RepDtOrd
023500030129      *
023600030129     C                   enddo
023700030129      *
023800030129      *   recupero il libro successivo a quello appena elaborato
023900030129     C                   exsr      RepNextLib
024000030129     C                   enddo
024100030129      *
024200030129      *
024300030129     C     XCalcCAI      Endsr
024301050928C1867 ******************************************************
024302050928  "   * lettura NDIVA* con 3 kiavi: Soc/TpReg/Libro
024304050928  "   ******************************************************
024305050928  "  C     Reade_K03Iva  Begsr
024306050928  "   *
024307050928  "   *  -----------------------------------------------------
024308050928  "   * |      VALUE POSSIBILI di TIPDAT2181                  |
024309050928  "   * |-----------------------------------------------------|
024310050928  "   * |                REGIME IVA                |          |
024311050928  "   * |------------------------------------------|          |
024312050928  "   * |Trasportatori misti:| Non trasport. misti:| LF USATA |
024313050928  "   * |------------------------------------------|----------|
024314050928  "   * |F=Data liquidazione | F=Data liquidazione | NDIVA02J |
024315050928  "   * |G=Data registrazione|                     | NDIVA08J |
024316050928  "   * |R=Data annotazione  | R=Data registrazione| NDIVA11J |
024317050928  "   *  -----------------------------------------------------
024322050928  "   *
024323050928  "  C                   select
024324050928  "   *
024325050928  "   * --------------------------------------------------
024326050928  "   * richiesto calcolo per data REGISTRAZIONE
024328050928  "  C                   when      TipDat2181 = 'G'
024329050928  "   *   Soc/TpReg/Libro/DTREG  /NrReg/DtDoc/NrDoc
024330050928  "  C     K03IVA08      reade     NDIVA08J                               21
024331050928  "   *
024332050928  "   * --------------------------------------------------
024333050928  "   * richiesto calcolo per data ANNOTAZIONE o REGISTRAZIONE
024337050928  "  C                   when      TipDat2181 = 'R'
024338050928  "   *   Soc/TpReg/Libro/DTANNOT/NrReg/DtDoc/NrDoc
024339050928  "  C     K03IVA11      reade     NDIVA11J                               21
024340050928  "   *
024341050928  "   * --------------------------------------------------
024342050928  "   * richiesto calcolo per data LIQUIDAZIONE
024345050928  "  C                   when      TipDat2181 = 'F'
024346050928  "   *   Soc/TpReg/Libro/DTRIF  /NrReg/DtDoc/NrDoc
024347050928  "  C     K03IVA02      reade     NDIVA02J                               21
024348050928  "   *
024349050928  "   * --------------------------------------------------
024350050928  "   * se non è stata passato l'ordinamento
024351050928  "   * (non dovrebbe succedere), simulo fine file
024352050928  "  C                   other
024353050928  "  C                   eval      *in21 = *on
024354050928  "  C                   endsl
024355050928  "   *
024356050928  "  C                   eval      $EndLibro = *in21
024357050928  "   *
024358050928C1867C     XReade_K03Iva Endsr
024359050928C1867 ******************************************************
024360050928  "   * lettura NDIVA* con 2 kiavi: Soc/TpReg
024362050928  "   ******************************************************
024363050928  "  C     Reade_K02Iva  Begsr
024364050928  "   *
024365050928  "   *  -----------------------------------------------------
024366050928  "   * |      VALUE POSSIBILI di TIPDAT2181                  |
024367050928  "   * |-----------------------------------------------------|
024368050928  "   * |                REGIME IVA                |          |
024369050928  "   * |------------------------------------------|          |
024370050928  "   * |Trasportatori misti:| Non trasport. misti:| LF USATA |
024371050928  "   * |------------------------------------------|----------|
024372050928  "   * |F=Data liquidazione | F=Data liquidazione | NDIVA02J |
024373050928  "   * |G=Data registrazione|                     | NDIVA08J |
024374050928  "   * |R=Data annotazione  | R=Data registrazione| NDIVA11J |
024375050928  "   *  -----------------------------------------------------
024376050928  "   *
024377050928  "  C                   select
024378050928  "   *
024379050928  "   * --------------------------------------------------
024380050928  "   * richiesto calcolo per data REGISTRAZIONE
024381050928  "  C                   when      TipDat2181 = 'G'
024382050928  "   *   Soc/TpReg/Libro/DTREG  /NrReg/DtDoc/NrDoc
024383050928  "  C     K02IVA08      reade     NDIVA08J                               21
024385050928  "   *
024386050928  "   * --------------------------------------------------
024387050928  "   * richiesto calcolo per data ANNOTAZIONE o REGISTRAZIONE
024388050928  "  C                   when      TipDat2181 = 'R'
024389050928  "   *   Soc/TpReg/Libro/DTANNOT/NrReg/DtDoc/NrDoc
024390051006  "  C     K02IVA11      reade     NDIVA11J                               21
024392050928  "   *
024393050928  "   * --------------------------------------------------
024394050928  "   * richiesto calcolo per data LIQUIDAZIONE
024395050928  "  C                   when      TipDat2181 = 'F'
024396050928  "   *   Soc/TpReg/Libro/DTRIF  /NrReg/DtDoc/NrDoc
024397050928  "  C     K02IVA02      reade     NDIVA02J                               21
024398050928  "   *
024399050928  "   * --------------------------------------------------
024400050928  "   * se non è stata passato l'ordinamento
024401050928  "   * (non dovrebbe succedere), simulo fine file
024402050928  "  C                   other
024403050928  "  C                   eval      *in21 = *on
024404050928  "   *
024405050928  "  C                   endsl
024406050928  "   *
024407050928  "   *
024408050928  "  C                   eval      $EndRegist   = *in21
024409050928  "  C                   if        $EndRegist   = *off
024410050928  "  C                   eval      $LibroElab = IVALibro
024411050928  "  C                   endif
024412050928  "   *
024413050928C1867C     XReade_K02Iva Endsr
024414050202C1814 ******************************************************
024500050202  "   * imposto dta ordinamento in base al tipo data richiesto
024600050202  "   ******************************************************
024700050202  "  C     RepDtOrd      Begsr
024800050928C1814 *
024801050928C1867 *  -----------------------------------------------------
024802050928  "   * |      VALUE POSSIBILI di TIPDAT2181                  |
024803050928  "   * |-----------------------------------------------------|
024804050928  "   * |                REGIME IVA                |          |
024805050928  "   * |------------------------------------------|          |
024806050928  "   * |Trasportatori misti:| Non trasport. misti:| LF USATA |
024807050928  "   * |------------------------------------------|----------|
024808050928  "   * |F=Data liquidazione | F=Data liquidazione | NDIVA02J |
024809050928  "   * |G=Data registrazione|                     | NDIVA08J |
024810050928  "   * |R=Data annotazione  | R=Data registrazione| NDIVA11J |
024811050928C1867 *  -----------------------------------------------------
024900050928C1814C                   eval      $DtOrd = *hival
025000050202  "   *
025100050202  "  C                   if        $EndLibro  = *off
025200050928C1814 *
025201050928C1867C                   select
025202050928  "   *
025301050928  "   * ------------------------------------------------------
025302050930  "   * richiesto calcolo per data REGISTRAZIONE
025304050930  "  X**ex C1814 **richiesta ordinamento per data Registrazione
025400050930  "  X**ex C1814 ******* if        TipDat2181 = 'G'
025401050928C1867C                   when      TipDat2181 = 'G'
025500050928C1814C                   eval      $DtOrd = IVADtReg
025600050928C1867X**ex C1814 **richiesta ordinamento per data Annotazione (o altro)
025700050928  "  X**ex C1814 ******* else
025701050928  "   *
025702050928  "   * ------------------------------------------------------
025703050928  "   * richiesto calcolo per data ANNOTAZIONE o REGISTRAZIONE
025704050928C1867C                   when      TipDat2181 = 'R'
025800050928C1814C                   eval      $DtOrd = IVADtAnnot
025801050928C1867 *
025802050928  "   * ------------------------------------------------------
025803050928  "   * richiesto calcolo per data LIQUIDAZIONE
025804050928  "  C                   when      TipDat2181 = 'F'
025805050928  "  C                   eval      $DtOrd = IVADtRif
025806050928  "   *
025900050928  "  X**ex C1814 ******* endif
025901050928C1867C                   endsl
026000050928C1814 *
026100050202  "  C                   endif
026200050202  "   *
026300050202C1814C     XRepDtOrd     Endsr
026400030129      ******************************************************
026500030129      * Posizionamento e lettura iniziale
026600030129      ******************************************************
026700030129     C     SetRedIniz    Begsr
026800030129      *
026900030129     C                   eval      $EndRegist   = *off
027000030129     C                   eval      $EndLibro    = *off
027100030129      *
027200030129     C                   move      SoC181        IVASocieta
027300030129     C                   move      TpReg181      IVATpReg
027400030129      *
027500030129      * passato solo il registro e non il libro
027600030129      *  recupero il primo libro del registro
027700030129     C                   if        Libro181 = *blank
027800030129      *
027801050928C1867C                   select
027802050928  "   *
027803050928C1867 * --------------------------------------------------
027900050202C1814 * richiesto ordinamento per data Registrazione
028000050928C1867X**ex C1814 ******* if        TipDat2181 = 'G'
028001050928  "  C                   when      TipDat2181 = 'G'
028002050928C1867 *   Soc/TpReg/Libro/DTREG  /NrReg/DtDoc/NrDoc
028100050928C1814C     k02IVA08      setll     NDIVA08J
028101050928C1867 *
028200050928  "  X**ex C1814 ***K02IVA08      reade     NDIVA08J                      21
028300050928  "  X**ex C1814 **richiesto ordinamento per data Annotazione (o altro)
028400050928  "  X**ex C1814 ******* else
028401050928  "   * --------------------------------------------------
028402050928  "   * richiesto calcolo per data ANNOTAZIONE o REGISTRAZIONE
028403050928  "  C                   when      TipDat2181 = 'R'
028404050928C1867 *   Soc/TpReg/Libro/DTANNOT/NrReg/DtDoc/NrDoc
028500030210     C     k02IVA11      setll     NDIVA11J
028600050928C1867X**** K02IVA11      reade     NDIVA11J                               21
028601050928  "   *
028602050928  "   * --------------------------------------------------
028603050928  "   * richiesto calcolo per data LIQUIDAZIONE
028604050928  "  C                   when      TipDat2181 = 'F'
028606050928  "   *   Soc/TpReg/Libro/DTRIF  /NrReg/DtDoc/NrDoc
028607050928  "  C     k02IVA02      setll     NDIVA02J
028608050928  "   *
028609050928  "  X**ex C1814 ******* endif
028610050928C1867C                   endsl
028800050202C1814 *
028900050928C1867X****************** eval      $EndRegist   = *in21
029000050928  "  X****************** if        $EndRegist   = *off
029100050928  "  X****************** eval      $LibroElab = IVALibro
029200050928  "  X****************** endif
029201050928  "   *
029202050928  "   * lettura NDIVA* con 2 Key: Soc/TpReg
029203050928C1867C                   exsr      Reade_K02Iva
029300030129      *
029400030129      * passato registro + libro
029500030129      *  preparo il resto della k per la set/red del libro
029600030129     C                   else
029700030129     C                   move      Libro181      $LibroElab
029800030129      *
029900030129     C                   endif
030000030129      *
030100030129     C     XSetRedIni    Endsr
030200030129      ******************************************************
030300030129      * posizionamento e prima lettura sul libro
030400030129      ******************************************************
030500030129     C     SetRedLib     Begsr
030600030129      *
030700030129     C                   move      $LibroElab    IVALibro
030800050202C1814 *
030801050928C1867C                   select
030802050928  "   *
030803050928C1867 * --------------------------------------------------
030900050928C1814 * richiesto ordinamento per data Registrazione
031000050928C1867X**ex C1814 ******* if        TipDat2181 = 'G'
031001050928C1867C                   when      TipDat2181 = 'G'
031100050928C1814C                   move      DtI181        IVADtReg
031101050928C1867 *   Soc/TpReg/Libro/DTREG  /NrReg/DtDoc/NrDoc
031200050928C1814C     k04IVA08      setll     NDIVA08J
031201050928C1867 *
031300050928  "  X**ex C1814 ***K03IVA08      reade     NDIVA08J                      21
031400050928  "  X**ex C1814 **richiesto ordinamento per data Annotazione (o altro)
031500050928  "  X**ex C1814******** else
031501050928  "   * --------------------------------------------------
031502050928  "   * richiesto calcolo per data ANNOTAZIONE o REGISTRAZIONE
031503050928C1867C                   when      TipDat2181 = 'R'
031600050202     C                   move      DtI181        IVADtAnnot
031601050928C1867 *   Soc/TpReg/Libro/DTANNOT/NrReg/DtDoc/NrDoc
031700030210     C     k04IVA11      setll     NDIVA11J
031800050928C1867X*****K03IVA11      reade     NDIVA11J                               21
031801050928  "   *
031802050928  "   * --------------------------------------------------
031803050928  "   * richiesto calcolo per data LIQUIDAZIONE
031804050928  "  C                   when      TipDat2181 = 'F'
031805050928  "  C                   move      DtI181        IVADtRif
031806050928  "   *   Soc/TpReg/Libro/DTRIF  /NrReg/DtDoc/NrDoc
031807050928  "  C     k04IVA02      setll     NDIVA02J
031808050928  "   *
031900050928  "  X**ex C1814 ******* endif
031901050928C1867C                   endsl
032000050202C1814 *
032001050928C1867X****************** eval      $EndLibro = *in21
032200030129      *
032202050928C1867 * lettura NDIVA* con 3 Key: Soc/TpReg/Libro
032203050928  "  C                   exsr      Reade_K03Iva
032204050928C1867 *
032300030129     C     XSetRedLib    Endsr
032400030129      ******************************************************
032500030129      * recupero il libro successivo a quello appena elaborato
032600030129      ******************************************************
032700030129     C     RepNextLib    Begsr
032800030129      *
032900030129      * passato solo il registro e non il libro
033000030129      *  recupero il libro successivo a quello appena elaborato
033100030129     C                   if        Libro181 = *blank
033200030129      *
033300030129     C                   move      Soc181        IVASocieta
033400030129     C                   move      TpReg181      IVATpReg
033500030129     C                   move      $LibroElab    IVALibro
033600050202C1814 *
033601050928C1867C                   select
033602050928  "   *
033603050928C1867 * --------------------------------------------------
033700050928C1814 * richiesto ordinamento per data Registrazione
033800050928C1867X**ex C1814 ******* if        TipDat2181 = 'G'
033801050928  "  C                   when      TipDat2181 = 'G'
033802050928C1867 *   Soc/TpReg/Libro/DTREG  /NrReg/DtDoc/NrDoc
033900050928C1814C     k03IVA08      setgt     NDIVA08J
033901050928C1867 *
034000050928  "  X**ex C1814 ***K02IVA08      reade     NDIVA08J                      21
034100050928  "  X**ex C1814 **richiesto ordinamento per data Annotazione (o altro)
034101050928  "  X**ex C1814 ******* else
034102050928  "   * --------------------------------------------------
034103050928  "   * richiesto calcolo per data ANNOTAZIONE o REGISTRAZIONE
034104050928  "  C                   when      TipDat2181 = 'R'
034105050928C1867 *   Soc/TpReg/Libro/DTANNOT/NrReg/DtDoc/NrDoc
034300030210     C     k03IVA11      setgt     NDIVA11J
034400050928C1867X**** K02IVA11      reade     NDIVA11J                               21
034401050928  "   *
034402050928  "   * --------------------------------------------------
034403050928  "   * richiesto calcolo per data LIQUIDAZIONE
034404050928  "  C                   when      TipDat2181 = 'F'
034405050928  "   *   Soc/TpReg/Libro/DTRIF  /NrReg/DtDoc/NrDoc
034406050928  "  C     k03IVA02      setgt     NDIVA02J
034407050928  "   *
034408050928  "  X**ex C1814 ******* endif
034409050928C1867C                   endsl
034600050202C1814 *
034700050928C1867X****************** eval      $EndRegist   = *in21
034800050928  "  X****************** if        $EndRegist   = *off
034900050928  "  X****************** eval      $LibroElab = IVALibro
035000050928  "  X****************** endif
035001050928  "   *
035002050928  "   * lettura NDIVA* con 2 Key: Soc/TpReg
035003050928C1867C                   exsr      Reade_K02Iva
035100030129      *
035200030129      * passato registro + libro: non devo leggere altri libri
035300030129     C                   else
035400030129     C                   eval      $EndRegist   = *on
035500030129      *
035600030129     C                   endif
035700030129      *
035800030129     C     XRepNextLi    Endsr
035900030129      ******************************************************
036000030129      * Sviluppo calcolo in base al singolo rec di ndiva00f
036100030129      ******************************************************
036200030129     C     CalcCAI_1     Begsr
036300030129      *
036400030129      *
036500030129     C                   if        (IVATpReg='1' and IVAAvere=1)
036600030129     C                             or
036700030129     C                             ((IVATpReg='2' or IVATpReg='3')
036800030129     C                               and  (IVADare=1))
036900030129     C     IVAImporto    mult      -1            IVAImporto
037000030129     C     IVAImponib    mult      -1            IVAImponib
037100030129     C                   endif
037200030129      *
037300030129      * ---------------------------------------------------
037400030129      * Iva esigibile per il periodo(totven)/Iva che si detrae(totacq)
037500030129     C                   exsr      TotVen_Acq
037600030129      *
037700030129      * ---------------------------------------------------
037800030129     C                   move      IVASocieta    ALRSocieta
037900030129     C                   move      IVATpReg      ALRTpReg
038000030129     C                   move      IVALibro      ALRLibro
038100030129     C                   z-add     IVAAliq       ALRAliq
038200030129     C                   move      IVARifIva     ALRRifIva
038300030129     C     K05ALR01      chain     ANALR01L                           24
038400030129     C                   if        *in24 = *Off
038500040218C1723C                             and ALRAnn <> '1'
038600030129      *
038700030129      *  --------------------------------------------------
038800030129      *  Operazioni passive/attive (al netto dell'iva)
038900030129     C                   exsr      Att_Pas
039000030129      *
039100030129      *  --------------------------------------------------
039200030129      *  di cui acquisti/cessioni intracomunitari
039300030129     C                   exsr      AttCI_PasA
039400030129      *
039500030129      *  --------------------------------------------------
039600030129      *  di cui operazioni non imponibili ed esenti
039700030129     C                   exsr      AttEs_PasE
039800030129      *
039900030129     C                   endif
040000030129      *
040100030129     C     XCalcCAI_1    Endsr
040200030129      ******************************************************
040300030129      * Calcolo Iva esigibile per il periodo(totven)
040400030129      *         Iva che si detrae(totacq)
040500030129      ******************************************************
040600030129     C     TotVen_Acq    Begsr
040700030129      *
040800030129      *   Se non indetraibile
040900030129     C                   if        IVAIndetr <> *on
041000030129      *
041100030129      *   Reperisco impostazioni libro iva
041200030129     C                   exsr      RepDLI
041300030129      *
041400030129      *   Se libro sospeso/differito non lo considero nel totale
041500030129     C                   if        $LibroSosp   = *off
041600030129      *
041700030129     C                   select
041800030129      *    registro acquisti
041900030129     C                   when      IVATpReg = '1'
042000030129     C                   sub       IVAImporto    $TOTAcq
042100030129      *    registro vendite o corrispettivi
042200030129     C                   when      IVATpReg = '2' or IVATpReg='3'
042300030129     C                   add       IVAImporto    $TOTVen
042400030129     C                   endsl
042500030129      *
042600030129     C                   endif
042700030129      *
042800030129     C                   endif
042900030129      *
043000030129     C     XTotVen_Ac    Endsr
043100030129      ******************************************************
043200030129      * Calcolo Operazioni passive/attive (al netto dell'iva)
043300030129      ******************************************************
043400030129     C     Att_Pas       Begsr
043500030129      *
043600030129      * se è una operazione attiva/passiva
043700030129     C                   if        %subst(ALRAlleIva:1:1) = *On
043800030129      *
043900030129     C                   select
044000030129      *    registro acquisti
044100030129     C                   when      IVATpReg = '1'
044200030129     C                   sub       IVAImponib    $OpPas
044300030129      *    registro vendite
044400030129     C                   when      IVATpReg = '2'
044500030129     C                   exsr      RepCausale
044600030129     C                   if        $Autofatt = *off
044700030129     C                   add       IVAImponib    $OpAtt
044800030129     C                   endif
044900030129      *    registro corrispettivi
045000030129     C                   when      IVATpReg = '3'
045100030129     C                   add       IVAImponib    $OpAtt
045200030129      *
045300030129     C                   endsl
045400030129      *
045500030129     C                   endif
045600030129      *
045700030129     C     XAtt_Pas      Endsr
045800030129      ******************************************************
045900030129      * Calcolo di cui Cessioni/Acquisti intracomunitari
046000030129      ******************************************************
046100030129     C     AttCI_PasA    Begsr
046200030129      *
046300030129      * se è un 'di cui cessioni/acquisti intracomunitaria'
046400030129     C                   if        %subst(ALRAlleIva:2:1) = *On
046500040224C1723 *
046600040224C1727 * se causale ok, movimento iva considerato per cess/acq intrac:
046700040224  "   * Causale ok se:
046800040224  "   * reperita classificaz. causali beni e servizi  e
046900040224  "   *   le causali NON devono essere CONSIDerate nel calcolo CAIVA
047000040224  "   *   oppure
047100040224  "   *   le causali devono essere CONSIDerate nel calcolo CAIVA e
047200040224  "   *   la causale (di riga se esiste ed è classificata
047300040224  "   *   oppure di testata) è classificata come causale per beni
047400040224  "  X**se la causale (di riga se esiste ed è classificata
047500040224C1727X**oppure di testata) è classificata come causale per beni
047600040224C1723C                   exsr      RepTipoCauBS
047700040224C1727X*ex C1723 ******** if        $CauBS= 'B '
047800040224C1727C                   if        $CauOK= *on
047900040213C1723 *
048000030129      *
048100030129     C                   select
048200030129      *    registro acquisti
048300030129     C                   when      IVATpReg = '1'
048400030129     C                   sub       IVAImponib    $OpPasAI
048500030129      *    registro vendite
048600030129     C                   when      IVATpReg = '2'
048700030129     C                   exsr      RepCausale
048800030129     C                   if        $Autofatt = *off
048900030129     C                   add       IVAImponib    $OpAttCI
049000030129     C                   endif
049100030129      *    registro corrispettivi
049200030129     C                   when      IVATpReg = '3'
049300030129     C                   add       IVAImponib    $OpAttCI
049400030129      *
049500030129     C                   endsl
049600040213C1723 *
049700040213C1723C                   endif
049800030129      *
049900030129     C                   endif
050000030129      *
050100030129     C     XAttCI_Pas    Endsr
050200030129      ******************************************************
050300030129      * Calcolo di cui operazioni ESenti e non imponibili
050400030129      ******************************************************
050500030129     C     AttEs_PasE    Begsr
050600030129      *
050700030129      * se è un'operazione attiva/passiva e
050800030129     C                   if        %subst(ALRAlleIva:1:1) = *On and
050900030210      *
051000030210      * non è un 'di cui cessioni/acquisti intracomunitaria' e
051100030210      * (la normativa vuole che tra le operazioni non imponibili
051200030210      * o esenti non siano totalizzate le intracomunitarie)
051300040213C1723 * (solo le operazioni attive esenti comprendono le intracom.)
051400040213C1723X**************************** %subst(ALRAlleIva:2:1) <>*On and
051500030210      *
051600030210      * il movimento iva ha un codice di riferimento
051700030210     C                             IVARifIva <> *blank
051800030129      *
051900030129     C                   move      IVASocieta    RIVSocieta
052000040218C1723C                   move      IVATpReg      RIVTpReg
052100040218C1723C                   move      IVAAliq       RIVAliq
052200030129     C                   move      IVARifIva     RIVRifIva
052300040218C1723X*****K02RIV03      chain     ANRIV03L                           25
052400040218C1723C     K04RIV01      chain     ANRIV01L                           25
052500030129      *
052600030129      * codice riferimento esistente in anagrafica
052700030129     C                   if        *in25 = *Off
052800030129      *
052900030129      *  codice riferimento considerato esente
053000030129     C                   if        %subst(RIVFlg:3:1) = '1'
053100030129      *
053200030129     C                   select
053300030129      *    registro acquisti
053400030129     C                   when      IVATpReg = '1'
053500040213C1723C                             and %subst(ALRAlleIva:2:1) <>*On
053600030129     C                   sub       IVAImponib    $OpPasEs
053700030129      *    registro vendite
053800030129     C                   when      IVATpReg = '2'
053900030129     C                   exsr      RepCausale
054000030129     C                   if        $Autofatt = *off
054100030129     C                   add       IVAImponib    $OpAttEs
054200030129     C                   endif
054300030129      *    registro corrispettivi
054400030129     C                   when      IVATpReg = '3'
054500030129     C                   add       IVAImponib    $OpAttEs
054600030129      *
054700030129     C                   endsl
054800030129      *
054900030129      *  codice riferimento considerato non imponibile
055000030129     C                   else
055100040213C1723 *
055200040213  "  C                   if        %subst(RIVFlg:4:1) = '1'
055300040213C1723C                             and %subst(ALRAlleIva:2:1) <>*On
055400030129      *
055500030129     C                   select
055600030129      *    registro acquisti
055700030129     C                   when      IVATpReg = '1'
055800030129     C                   sub       IVAImponib    $OpPasNI
055900030129      *    registro vendite
056000030129     C                   when      IVATpReg = '2'
056100030129     C                   exsr      RepCausale
056200030129     C                   if        $Autofatt = *off
056300030129     C                   add       IVAImponib    $OpAttNI
056400030129     C                   endif
056500030129      *    registro corrispettivi
056600030129     C                   when      IVATpReg = '3'
056700030129     C                   add       IVAImponib    $OpAttNI
056800030129      *
056900030129     C                   endsl
057000030129      *
057100040213C1723C                   endif
057200040213     C                   endif
057300030129     C                   endif
057400030129     C                   endif
057500030129      *
057600030129     C     XAttEs_Pas    Endsr
057700040213C1723 ******************************************************
057800040213  "   * Reperisco tipo causale: se bene o servizio
057900040213  "   ******************************************************
058000040213  "  C     RepTipoCauBS  Begsr
058100040213  "   *
058200040224C1723C                   clear                   $CauBS
058300040224C1727C                   clear                   $CauOK
058400040224C1723 *
058500040213  "   *--------------------------------------------------------------
058600040213  "   * Reperimento causali classificate per distinguere beni e servizi
058700040213  "   *   Se non è stato ancora fatto
058800040213  "   *   (il pgm potrebbe essere chiamato + volte per fare i calcoli)
058900040213  "   *   reperisco il rec di ANMPE che memorizza le causali
059000040213  "   *   che distinguono cessione/acquisti beni e servizi
059100040213  "  C                   if        $RepCauBS = *off
059200040213  "  C                   exsr      RepCauBS
059300040213  "  C                   endif
059400040213  "   *
059500040213  "   *
059600040213  "   *--------------------------------------------------------------
059700040224C1723 * Reperimento se la reg. riguarda un bene o un servizio
059800040224C1727 * se reperita classificaz. causali beni e servizi
059900040224C1727 * e le causali devono essere CONSIDerate nel calcolo della CAIVA
060000040224C1723C                   if        $RepCauBS = *on
060100040224C1727C                             and %subst(SkServCa(32):4:1)=*on
060200040224C1723 *
060300040213  "   *
060400040213  "   *    _(A) Causale Riga iva______________________________________
060500040213  "   *   |   se   |esiste |classificata|calssificata | è Sommata tra |
060600040213  "   *   |        |       |nei Beni    |nei Servizi  | le Op.Intrac. |
060700040213  "   *   | -------|--------------------------------------------------|
060800040213  "   *   | caso(1)|  si   |   SI       |    no       |     SI        |
060900040213  "   *   | caso(2)|  si   |   no       |    si       |     no        |
061000040213  "   *   | caso(3)|  si   |   si       |    si       |     no        |
061100040213  "   *   | caso(4)|  si   |   no       |    no       | no (vai a (B))|
061200040213  "   *   | caso(5)|  no   |   /        |    /        | /  (vai a (B))|
061300040213  "   *   | ----------------------------------------------------------|
061400040213  "   *   |se si verificano i  casi (4) o (5)                         |
061500040213  "   *   |(la causale di riga non esiste oppure non è classificata): |
061600040213  "   *   |(B) Causale Testata----------------------------------------|
061700040213  "   *   |                |classificata|calssificata | è Sommata tra |
061800040213  "   *   |                |nei Beni    |nei Servizi  | le Op.Intrac. |
061900040213  "   *   |                |------------------------------------------|
062000040213  "   *   |                |   SI       |    no       |     SI        |
062100040213  "   *   |                |   no       |    si       |     no        |
062200040213  "   *   |                |   si       |    si       |     no        |
062300040213  "   *   |                |   no       |    no       |     no        |
062400040213  "   *   |___________________________________________________________|
062500040213  "   *
062600040213  "   *
062700040213  "   * Causale di RIGA
062800040213  "  C                   if        IVACausRig <> *blank
062900040213  "   *
063000040213  "  C                   eval      $c = 1
063100040213  "  C     IVACausRig    lookup    SkBeniCa($c)                           25
063200040213  "  C                   eval      $c = 1
063300040213  "  C     IVACausRig    lookup    SkServCa($c)                           26
063400040213  "  C                   select
063500040213  "   *  caus. di riga definita tra i beni ma non tra i servizi
063600040213  "  C                   when      *in25 = *on   and *in26 = *off
063700040213  "  C                   eval      $CauBS = 'B '
063800040213  "   *  caus. di riga definita non tra i beni ma tra i servizi
063900040213  "  C                   when      *in25 = *off  and *in26 = *on
064000040213  "  C                   eval      $CauBS = ' S'
064100040213  "   *  caus. di riga definita sia tra i beni che tra i servizi
064200040213  "  C                   when      *in25 = *on   and *in26 = *on
064300040213  "  C                   eval      $CauBS = 'BS'
064400040213  "  C                   endsl
064500040213  "  C                   endif
064600040213  "   *
064700040213  "   *
064800040213  "   * Causale di riga non esiste oppure non è classificata
064900040213  "   * causale di TESTATA
065000040213  "  C                   if        IVACausRig = *blank
065100040213  "  C                             or  $CauBS = *blank
065200040213  "   *
065300040213  "  C                   eval      $c = 1
065400040213  "  C     IVACausTes    lookup    SkBeniCa($c)                           25
065500040213  "  C                   eval      $c = 1
065600040213  "  C     IVACausTes    lookup    SkServCa($c)                           26
065700040213  "  C                   select
065800040213  "   *  caus. Testata definita tra i beni ma non tra i servizi
065900040213  "  C                   when      *in25 = *on   and *in26 = *off
066000040213  "  C                   eval      $CauBS = 'B '
066100040213  "   *  caus. Testata definita non tra i beni ma tra i servizi
066200040213  "  C                   when      *in25 = *off  and *in26 = *on
066300040213  "  C                   eval      $CauBS = ' S'
066400040213  "   *  caus. di riga definita sia tra i beni che tra i servizi
066500040213  "  C                   when      *in25 = *on   and *in26 = *on
066600040213  "  C                   eval      $CauBS = 'BS'
066700040213  "  C                   endsl
066800040224  "  C                   endif
066900040224  "   *
067000040224  "  C                   endif
067100040224C1723 *
067200040224C1727 *
067300040224  "   * Causale ok se:
067400040224  "   * reperita classificaz. causali beni e servizi  e
067500040224  "   *   le causali NON devono essere CONSIDerate nel calcolo CAIVA
067600040224  "   *   oppure
067700040224  "   *   le causali devono essere CONSIDerate nel calcolo CAIVA e
067800040224  "   *   la causale (di riga se esiste ed è classificata
067900040224  "   *   oppure di testata) è classificata come causale per beni
068000040224  "  C                   if        $RepCauBS = *on
068100040224  "  C                             and
068200040224  "  C                             ( %subst(SkServCa(32):4:1)=*off
068300040224  "  C                               or
068400040224  "  C                               (%subst(SkServCa(32):4:1)=*on
068500040224  "  C                               and $CauBS='B ')  )
068600040224  "  C                   eval      $CauOK = *on
068700040224  "  C                   endif
068800040224C1727 *
068900040213C1723C     XRepTipoCauBS Endsr
069000040213C1723 ******************************************************
069100040213  "   * Reperisco causali che distinguono beni e servizi
069200040213  "   ******************************************************
069300040213  "  C     RepCauBS      Begsr
069400040213  "   *
069500040213  "  C                   clear                   XMpeDS
069600040213  "  C                   eval      TpChiamata = '01'
069700040213  "  C                   eval      NChiavi = 5
069800040213  "  C                   eval      VLogica = 2
069900040213  "   *
070000040213  "  C                   clear                   XMpeDS1
070100040213  "  C                   movel     XScSoc        DSMpeSoc
070200040213  "  C                   movel     'NDC570R'     DSMpePgm
070300040213  "  C                   movel     *blank        DSMpePrf
070400040213  "   * modalità con cui il pgm è usato
070500040213  "   * MPETIP = 'PX'  pgm richiamato come lista (CMD4)
070600040213  "   * MPETIP = 'PV'  pgm richiamato da menù
070700040213  "   * MPETIP = 'PB'  x lancio Batch
070800040213  "  C                   movel     'PV'          DSMpeTip
070900040213  "   *
071000040213  "  C                   callb     'XMPE'
071100040213  "  C                   parm                    XMpeDS
071200040213  "  C                   parm                    XMpeDS1
071300040213  "   *
071400040213  "  C                   if        Risultato = '0'
071500040213  "  C                   movel     DSMpePar      DSBeniCa
071600040213  "  C                   move      DSMpePar      DSServCa
071700040213  "   *
071800040213  "  C                   eval      $RepCauBS = *on
071900040213  "  C                   endif
072000040213  "   *
072100040213C1723C     XRepCauBS     Endsr
072200030129      ******************************************************
072300030129      * Reperisco impostazioni libro iva
072400030129      ******************************************************
072500030129     C     RepDLI        Begsr
072600030129      *
072700030129      * reperisco dati del libro se non l'ho ancora fatto
072800030129     C                   if        IVATpReg <> SAVTpReg or
072900030129     C                             IVALibro <> SAVLibro
073000030129      *
073100030129     C                   eval      SAVTpReg = IVATpReg
073200030129     C                   eval      SAvLibro = IVALibro
073300030129     C                   eval      $LibroSosp   = *off
073400030129      *
073500030129     C                   eval      DLISocieta = IVASocieta
073600030129     C                   eval      DLITpReg = IVATpReg
073700030129     C                   eval      DLILibro = IVALibro
073800030129     C     K03DLI01      chain     ANDLI01L                           23
073900030129     C   23              clear                   DLISospens
074000030129      *
074100030129      * libro sospeso/differito
074200030129     C                   if        DLISospens='1' or DLISospens='4'
074201180105R275 C                             or  DLIsospens =  '9'
074300030129     C                   eval      $LibroSosp   = *on
074400030129     C                   endif
074500030129      *
074600030129     C                   endif
074700030129      *
074800030129     C     XRepDLI       Endsr
074900030129      ************************************************************
075000030129      * Reperisco dati causale di testata
075100030129      ************************************************************
075200030129     C     RepCausale    Begsr
075300030129      *
075400030129     C                   eval      $Autofatt = *off
075500030129      *
075600030129      * reperisco dati causale incontrata per la prima volta
075700030129     C                   eval      $c = 1
075800030129     C     IVACausTes    lookup    SkCaus($c)                             25
075900030129     C                   if        *in25 = *off
076000030129      *
076100030129     C                   clear                   ANOPEDS
076200030129     C                   eval      lenout = %size(ANOPEDS)
076300030129     C                   callb     'NDMVC003'
076400030129     C                   parm                    XScSoc
076500030129     C                   parm                    IVACausTes
076600030129     C                   parm                    *Omit
076700030129     C                   parm                    Uso
076800030129     C                   parm      *Off          GesMsg
076900030129     C                   parm      *Off          Esito
077000030129     C                   parm      'ANOPE'       StrutturaO
077100030129     C     ANOPEDS       parm                    OutPut
077200030129     C                   parm                    LenOut
077300030129     C                   parm      *off          Refresh
077400030129      *
077500030129     C     *blank        lookup    SkCaus($c)                             25
077600030129     C                   if        *in25 = *on
077700030129     C                   move      IVACausTes    SkCaus($c)
077800030129     C                   move      OPETpDocI     SkTpDocI($c)
077900030129     C                   endif
078000030129      *
078100030129     C                   endif
078200030129      *
078300030129      *
078400030129     C                   eval      $c = 1
078500030129     C     IVACausTes    lookup    SkCaus($c)                             25
078600030129     C                   if        *in25 = *on  and
078700030129     C                             SkTpDocI($c) = '8'
078800030129     C                   eval      $Autofatt = *on
078900030129     C                   endif
079000030129      *
079100030129     C     XRepCausal    Endsr
079200030129      ******************************************************
079300030129      * Seleziono dati del movimento
079400030129      ******************************************************
079500030129     C     SelMov        Begsr
079600030129      *
079700030129     C                   eval      $MovOk = *off
079800030129      *
079900030129      *  deve essere un movimento definitivo e con ctb = al lancio
080000030129     C                   move      IVASys        MOVSys
080100030129     C                   move      IVANrAsReg    MOVNrAsReg
080200030129     C                   move      IVANrRigaM    MOVNrRigaM
080300030129     C     K03MOV01      chain     NDMOV01L                           22
080400030129     C                   if        *in22 = *off   and
080500030129     C                             MOVTpRegZ = 'D' and
080600030129     C                             MOVCtb = Ctb181
080700030129     C                   eval      $MovOk = *on
080800030129     C                   endif
080900030129      *
081000030129     C     XSelMov       Endsr
081100030129      ************************************************************
081200030129      * Stampa dati x Comunicazione Annuale Iva
081300030129      ************************************************************
081400030129     C     StampaCAI     BEGSR
081500030129      *
081600030129     C                   if        $OpenPrtf = *off
081700030129     C                   open      NDC181P
081800030129     C                   eval      $OpenPrtf = *on
081900030129     C                   exsr      RieTesta
082000030129     C                   endif
082100030129      *
082200030129      * Overflow
082300030129     C                   if        *in39 = *on
082400030129     C                   write     C181Testa
082500030129     C                   eval      *in39 = *off
082600030129     C                   endif
082700030129      *
082800030129      * periodo
082900030129     C     *JobRun       move      DtI181        P4DataI
083000030129     C     *JobRun       move      DtF181        P4DataF
083001050930C1867 * tipo data scelta
083002050930  "  C                   move      *blank        P4TipDat2D
083003050930  "  C                   RESET                   X07DS
083004050930  "  C                   MOVE      '1'           X07RIC
083005050930  "  C                   MOVEL     'ANDIZ000'    X07TRC
083006050930  "  C                   if        XScIvaRgm = '2'
083007050930  "   *  F=Data liquidazione/G=Data registrazione/R=Data annotazione
083008050930  "  C                   MOVEL     'DSPTIPDAT3'  X07CAM
083009050930  "  C                   else
083010050930  "   *  F=Data liquidazione/R=Data registrazione
083011050930  "  C                   MOVEL     'DSPTIPDATI'  X07CAM
083012050930  "  C                   endif
083013050930  "  C                   MOVEL     TipDat2181    X07VAL
083014050930  "  C                   MOVE      *OFF          X07ALL
083015050930  "  C                   CALLb     'X07VALR'
083016050930  "  C                   PARM                    X07DS
083017050930  "  C     X07ERR        IFEQ      '0'
083018050930  "  C                   MOVEL     X07DES        P4TipDat2D
083021050930C1867C                   ENDIF
083100030129      *
083200030129      * importi e segno operazioni attive/passive
083300030129      * (segno: se l'importo = 0 evidenzio il segno naturale dell'operaz.)
083400030129     C                   if        $OpAtt     <  0
083500030129     C                   eval      P4OpAtt    = $OpAtt * -1
083600030129     C                   eval      P4OpAttS   = XScDare
083700030129     C                   else
083800030129     C                   eval      P4OpAtt    = $OpAtt
083900030129     C                   eval      P4OpAttS   = XScAvere
084000030129     C                   endif
084100030129     C                   if        $OpAttNI   <  0
084200030129     C                   eval      P4OpAttNI  = $OpAttNI * -1
084300030129     C                   eval      P4OpAttNIS = XScDare
084400030129     C                   else
084500030129     C                   eval      P4OpAttNI  = $OpAttNI
084600030129     C                   eval      P4OpAttNIS = XScAvere
084700030129     C                   endif
084800030129     C                   if        $OpAttEs   <  0
084900030129     C                   eval      P4OpAttEs  = $OpAttEs * -1
085000030129     C                   eval      P4OpAttEsS = XScDare
085100030129     C                   else
085200030129     C                   eval      P4OpAttEs  = $OpAttEs
085300030129     C                   eval      P4OpAttEsS = XScAvere
085400030129     C                   endif
085500030129     C                   if        $OpAttCI   <  0
085600030129     C                   eval      P4OpAttCI  = $OpAttCI * -1
085700030129     C                   eval      P4OpAttCIS = XScDare
085800030129     C                   else
085900030129     C                   eval      P4OpAttCI  = $OpAttCI
086000030129     C                   eval      P4OpAttCIS = XScAvere
086100030129     C                   endif
086200030129     C                   if        $TotVen    <  0
086300030129     C                   eval      P4TotVen   = $TotVen  * -1
086400030129     C                   eval      P4TotVenS  = XScDare
086500030129     C                   else
086600030129     C                   eval      P4TotVen   = $TotVen
086700030129     C                   eval      P4TotVenS  = XScAvere
086800030129     C                   endif
086900030129     C                   if        $OpPas    <=  0
087000030129     C                   eval      P4OpPas    = $OpPas   * -1
087100030129     C                   eval      P4OpPasS   = XScDare
087200030129     C                   else
087300030129     C                   eval      P4OpPas    = $OpPas
087400030129     C                   eval      P4OpPasS   = XScAvere
087500030129     C                   endif
087600030129     C                   if        $OpPasNI  <=  0
087700030129     C                   eval      P4OpPasNI  = $OpPasNI * -1
087800030129     C                   eval      P4OpPasNIS = XScDare
087900030129     C                   else
088000030129     C                   eval      P4OpPasNI  = $OpPasNI
088100030129     C                   eval      P4OpPasNIS = XScAvere
088200030129     C                   endif
088300030129     C                   if        $OpPasEs  <=  0
088400030129     C                   eval      P4OpPasEs  = $OpPasEs * -1
088500030129     C                   eval      P4OpPasEsS = XScDare
088600030129     C                   else
088700030129     C                   eval      P4OpPasEs  = $OpPasEs
088800030129     C                   eval      P4OpPasEsS = XScAvere
088900030129     C                   endif
089000030129     C                   if        $OpPasAI  <=  0
089100030129     C                   eval      P4OpPasAI  = $OpPasAI * -1
089200030129     C                   eval      P4OpPasAIS = XScDare
089300030129     C                   else
089400030129     C                   eval      P4OpPasAI  = $OpPasAI
089500030129     C                   eval      P4OpPasAIS = XScAvere
089600030129     C                   endif
089700030129     C                   if        $TotAcq   <=  0
089800030129     C                   eval      P4TotAcq   = $TotAcq  * -1
089900030129     C                   eval      P4TotAcqS  = XScDare
090000030129     C                   else
090100030129     C                   eval      P4TotAcq   = $TotAcq
090200030129     C                   eval      P4TotAcqS  = XScAvere
090300030129     C                   endif
090400030129      *
090500030129     C                   write     C181P4
090600030129      *
090700030129     C     XStampaCAI    Endsr
090800030129      ************************************************************
090900030129      * Riempio campi di testata
091000030129      ************************************************************
091100030129     C     RieTesta      Begsr
091200030129      *
091300030129     C                   movel     XScDsi        NomDit
091400030129     C                   call      'XNETA'                              29
091500030129     C                   parm                    NomSys
091600030129     C                   movel     KNSif         NomSif
091700030129     C                   movel     DsPgm         NomPgm
091800030129     C                   move      XScCdF        CodFis
091900030129      *
092000030129      * la I° volta deve essere stampata la testata
092100030129     C                   eval      *in39 = *on
092200030129      *
092300030129     C     XRieTesta     Endsr
092400030129      ************************************************************
092500030129      * INIZIALIZZAZIONE VARIABILI
092600030129      ************************************************************
092700030129     C     InzVar        BEGSR
092800030129      *
092900030129     C                   eval      NDC181DS = Kpjbu
093000030129      *
093100030129      * Variabili
093200030129     C                   clear                   SkCaus
093300030129     C                   clear                   SkTpDocI
093400030129      *
093500030129      * Reperimento autotizzazione
093600030129     C**                 exsr      RepAut
093700030129      *
093800030129     C     XInzVar       Endsr
093900030129      ************************************************************
094000030129      * OPERAZIONI INIZIALI
094100030129      ************************************************************
094200030129     C     *Inzsr        Begsr
094300030129      *
094400030129     C     *entry        plist
094500030129     C                   parm                    Kpjba
094600030129      *
094700030129     C                   eval      NDC181DS = Kpjbu
094800030129      *
094900030129      * Reperimento dati società
095000030129     C                   exsr      Inz_RepSoc
095100030129      *
095200030129      * Importi che devono rimanere valorizzate in chiamate successive
095300030129     C                   clear                   $OpAtt
095400030129     C                   clear                   $OpAttNI
095500030129     C                   clear                   $OpAttEs
095600030129     C                   clear                   $OpAttCI
095700030129     C                   clear                   $OpPas
095800030129     C                   clear                   $OpPasNI
095900030129     C                   clear                   $OpPasEs
096000030129     C                   clear                   $OpPasAI
096100030129     C                   clear                   $TotVen
096200030129     C                   clear                   $TotAcq
096300030129     C                   move      *off          $OpenPrtF
096400040213C1723C                   move      *off          $RepCauBS
096500030129      *
096600030129     C     XInzsr        Endsr
096700030129      ************************************************************
096800030129      * Gestione reperimento dati società
096900030129      ************************************************************
097000030129     C     Inz_RepSoc    Begsr
097100030129      *
097200030129     C                   movel     'SOC001'      TipXSc
097300030129     C                   movel     Soc181        SocXSc
097400030129     C                   exsr      RepSoc
097500030129     C                   if        RtnXsc <> '1'
097600030129     C                   movel     XSOCDS        SOC001
097700030129     C                   else
097800030129     C                   exsr      EndPgm
097900030129     C                   endif
098000030129      *
098100030129     C     XInz_RepSo    Endsr
098200030129      ************************************************************
098300030129      * Reperimento dati società
098400030129      ************************************************************
098500030129     C     RepSoc        Begsr
098600030129      *
098700030129     C                   callb     'XSOC'
098800030129     C                   parm                    TIPXSC            6
098900030129     C                   parm                    SOCXSC            3
099000030129     C                   parm                    CDSXSC            9 0
099100030129     C                   parm                    MODXSC            3
099200030129     C                   parm      *BLANKS       RTNXSC            1
099300030129     C                   parm                    XSOCDS
099400030129     C                   parm                    KPJBA
099500030129      *
099600030129     C     XRepSoc       Endsr
099700030129      ************************************************************
099800030129      * DEFINIZIONE CAMPI : KList,VARIABILI,CONTATORI,INDICI...
099900030129      ************************************************************
100000030129     C     DefCam        BegSr
100100030129      *
100200030129      * Plist
100300030129      *
100400030129      * KList
100500030129      *
100600030210      * NDIVA11J
100700030210     C     K02IVA11      klist
100800030129     C                   kfld                    IVASocieta
100900030129     C                   kfld                    IVATpReg
101000030210     C     K03IVA11      klist
101100030129     C                   kfld                    IVASocieta
101200030129     C                   kfld                    IVATpReg
101300030129     C                   kfld                    IVALibro
101400030210     C     K04IVA11      klist
101500030129     C                   kfld                    IVASocieta
101600030129     C                   kfld                    IVATpReg
101700030129     C                   kfld                    IVALibro
101800030210     C                   kfld                    IVADtAnnot
101900050202C1814 * NDIVA08J
102000050202  "  C     K02IVA08      klist
102100050202  "  C                   kfld                    IVASocieta
102200050202  "  C                   kfld                    IVATpReg
102300050202  "  C     K03IVA08      klist
102400050202  "  C                   kfld                    IVASocieta
102500050202  "  C                   kfld                    IVATpReg
102600050202  "  C                   kfld                    IVALibro
102700050202  "  C     K04IVA08      klist
102800050202  "  C                   kfld                    IVASocieta
102900050202  "  C                   kfld                    IVATpReg
103000050202  "  C                   kfld                    IVALibro
103100050202C1814C                   kfld                    IVADtReg
103101050928C1867 * NDIVA02J
103102050928  "  C     K02IVA02      klist
103103050928  "  C                   kfld                    IVASocieta
103104050928  "  C                   kfld                    IVATpReg
103105050928  "  C     K03IVA02      klist
103106050928  "  C                   kfld                    IVASocieta
103107050928  "  C                   kfld                    IVATpReg
103108050928  "  C                   kfld                    IVALibro
103109050928  "  C     K04IVA02      klist
103110050928  "  C                   kfld                    IVASocieta
103111050928  "  C                   kfld                    IVATpReg
103112050928  "  C                   kfld                    IVALibro
103113050928C1867C                   kfld                    IVADtRif
103200030129      *
103300030129      * NDMOV01L
103400030129     C     K03MOV01      klist
103500030129     C                   kfld                    MOVSys
103600030129     C                   kfld                    MOVNrAsReg
103700030129     C                   kfld                    MOVNrRigaM
103800030129      *
103900040218C1723X**ANRIV03L
104000040218C1723X**** K02RIV03      klist
104100040218C1723 * ANRIV01L
104200040218C1723C     K04RIV01      klist
104300030129     C                   kfld                    RIVSocieta
104400040218C1723C                   kfld                    RIVTpReg
104500040218C1723C                   kfld                    RIVAliq
104600030129     C                   kfld                    RIVRifIva
104700030129      *
104800030129      * ANALR01L
104900030129     C     K05ALR01      klist
105000030129     C                   kfld                    ALRSocieta
105100030129     C                   kfld                    ALRTpReg
105200030129     C                   kfld                    ALRLibro
105300030129     C                   kfld                    ALRAliq
105400030129     C                   kfld                    ALRRifIva
105500030129      *
105600030129      * ANDLI01L
105700030129     C     K03DLI01      klist
105800030129     C                   kfld                    DLISocieta
105900030129     C                   kfld                    DLITpReg
106000030129     C                   kfld                    DLILibro
106100030129      *
106200030129     C     XDefCam       EndSr
