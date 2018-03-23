000100100820     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(*CALLER)
000200100830     H*PARMS CVTOPT(*NONE)
000300100820     H DECEDIT('0,') DATEDIT(*DMY.)
000301100830     F*----------------------------------------------------*
000400100820      *
000500100831      *            Persona fisica: dati aggiuntivi
000600100820      *           ----------------------------------
000700100820      *
000900100820     F*----------------------------------------------------*
000901100830     F* estensione nominativi di partita (per conti
000902100830     F* estemporanei)
000903100830     FNDCPA01L  IF   E           K DISK
000904100830     F*------------
000905100830     F* anagrafica soggetti storicizzata
000906100830     FANCPI01L  IF   E           K DISK
000907100830     F*------------
000908100830     F* anagrafica soggetti standard
001000100820     FANSOG01L  IF   E           K DISK
001001100830     FANIND01L  IF   E           K DISK
001002160510R224 F*------------
001003160513  "  FNDPPA01L  IF   E           K DISK
001004160513  "  F                                     RENAME(NDPPA000:NDPPA01)
001005160513  "  F*------------
001006160513R224 FNDPPA04L  IF   E           K DISK
001100100820     F*------------
001200100820     FSDGSO301L UF A E           K DISK
001300100820     F*------------
001400100820     FSDGSD16D  CF   E             WORKSTN
001500100820     F                                     RENAME(SD16D1:FMTD1)
001501160510R224 F                                     RENAME(SD16D2:FMTD2)
001600100820     F                                     RENAME(SD16T1:FMTT1)
001700100820     F                                     RENAME(SD16Z1:FMTZ1)
001800100820     F                                     INFDS(DSFMT)
001900100820     D*----------------------------------------------------*
002000100820     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
002100100820     D $ID             S             27    DIM(5) ALT($OZ)
002200100820     D* SChiere per la gestione dei tasti di funzione
002300100820     D $FM             S             27    DIM(24)
002400100820     D $FX             S             50    DIM(24)
002500100820     D $FC             S              1    DIM(24)
002600100820     D $FR             S              1    DIM(24)
002700100820     D* Video D1
002800100820     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
002900100820     D $FX1            S             50    DIM(24) ALT($FM1)
003000100820     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
003100100820     D $FR1            S              1    DIM(24) ALT($FC1)
003200100820     D*-------------
003300100820     D* Passaggio Parametri
003400100820     D KPJBA         E DS
003401100830     D*-------------
003402100830     D* Parametri di ricezione
003403100830     D SDGSD16DS     E DS                  INZ
003404160513R224 D*-------------
003405160513 "   D* Ricerca capoconti
003406160513 "   D ANA200DS      E DS                  INZ
003407160513 "   D*-------------
003408160513 "   D* Ricerca sottoconti
003409160513 "   D ANA201DS      E DS                  INZ
003410160513 "   D*-------------
003411160513 "   D* Ricerca partite
003412160513 "   D NDC085DS      E DS                   inz
003413160513 "   D*-------------
003414160513 "   D* x ricerca su partite
003415160513 "   D $DSAUT          DS           512
003416160513 "   D*-------------
003417160513 "   D* Schiera partite in ricerca
003418160513 "   D SkPartite       DS                  Occurs(50)
003419160513 "   D  SkDtPar                            Like(PPADtPar)
003420160513 "   D  SkNrPar                            Like(PPANrPar)
003421160513 "   D  SkSerPar                           Like(PPASerPar)
003422160513 "   D  SkImportD                    19S 3
003423160513 "   D  SkDtSca                            like(dtpar085)
003424160513 "   D  SkSegnoD                      1
003425160513 "   D  SkImporto                    19S 3
003426160513 "   D  SkDivisa                           like(Divisa085)
003427160513 "   D*---------
003428160428 "   D* tabella destinazione dati aggiuntivi
003429160428 "   D SDGHC4DS      E DS                  INZ
003430160428 "   D*-------------
003431160428 "   D* Ricerca soggetto
003432160428 "   D ANA040DS      E DS                  INZ
003433160428 "   D*---------
003434160429 "   D* Ricerca dati fiscali storicizzati
003435160429 "   D ANA057DS      E DS                  INZ
003436160429 "   D*---------
003437160428 "   D* Dati soggetti
003438160428 "   D DVASog        E DS
003439160428 "   D*---------
003440160428 "   D* driver soggetti
003441160428 "   D DVARic          S              1
003442160428 "   D DVASocieta      S              3
003443160428 "   D DVAUnita        S              8
003444160428 "   D DVADtRif        S               D
003445160428 "   D DVAStrutt       S             10
003446160428 "   D DVALenOut       S              9B 0
003447160428 "   D DVASnatura      S              1
003448160428 "   D DVACodice       S              8
003449160428 "   D DVALineav       S              3
003450160428 "   D DVAFiliale      S              3  0
003451160428 "   D DVASogg         S              8
003452160428 "   D DVATpInd        S              2
003453160428 "   D DVACdInd        S              3
003454160428 "   D DVAOutput       S           4000
003455160428R224 D DVAErrore       S              1
003456170822R258 D*----------
003457170822 "   D* Passaggio Parametri al PGM ANA801R
003458170822 "   D A801DS        E DS                  EXTNAME(ANA801DS)
003459170822R258 D  ERR801       E                     INZ('0')
003500100820     D*-------------
003600100820     D* Reperimento Autorizzazioni
003700100820     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
003800100820     D*-------------
003900100820     D* Passaggio Autorizzazioni
004000100820     D DSAUT           DS           512
004001100830     D*-------------
004002100830     D* Machera autorizzazioni
004003100830     D ANGSOGDS      E DS                  INZ
004100100820     D*-------------
004200100830     D* Ricerca legende
004300100830     D ANA964DS      E DS                  INZ
004301100830     D*-------------
004400100820     D* Controlli validità legende
004500100830     D X07VALDS      E DS                  INZ
004600100820     D*-------------
004700100820     D* Reperimento nome PGM
004800100820     D STATUS         SDS           333
004900100820     D  DSPGM            *PROC
005000100820     D  PARMS            *PARMS
005400100820     D*-------------
005500100820     D* DS gestione gruppo  o azienda
005600100820     D XPAOUT          DS          2000
005700100820     D*-------------
005701100830     D* Dati di ambiente ottenuti da SDGSOCDR
005702100830     D SOC001        E DS                  EXTNAME(SDGSOCDS )
006200100820     D*-------------
006201100830     D* DS Interna per dati di output di SDGSOCDR
006400100820     D XSOCDS          DS          1000
006401100830     D*-------------
006402100830     D* Richiamo a SDGTABDR
006403100830     D SDGTABDS      E DS                  inz
006800100820     D*-------------
006802100830     D* Parametri per ricerca codice ANATB00F
006803100830     D XTabDS        E DS                  inz
007100100820     D*-------------
007200100831     D* provincie
007300100831     D ANGC17DS      E DS                                    INZ
007301100831     D*-------------
007302100831     D* Campo chiave per tabella C17
007303100831     D KeyC17          S              6A   inz
007304100831     D*-------------
007305100831     D* Stati
007306100831     D ANGG12DS      E DS                                    INZ
007400100820     D*-------------
007500100820     D DSFMT           DS
007600100820     D  $TASTO               369    369
007700100820     D  NRG                  370    370
007800100820     D  NCL                  371    371
007900100820     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
007901160513     D*-------------
007902160513     D* Controllo conti
007903160513     D ND002DS       E DS
007904160513     D*-------------
007905160513     D* Definizioni parametri standard chiamate moduli
007906160513     D$Esito           S              1
007907160513     D$GesMsg          S              1
007908160513     D$LenOut          S              9B 0
007909160513     D$Struttura       S             10
008000100820     D*-------------
008100100820     D* DS Esterna gestione messaggi per COMIT
008101100830     D SDGMSGDS      E DS                  INZ
008300100820     D  STS          E                     EXTFLD(MSGSTS)
008400100820     D                                     DIM(333)
008500100820     D  JBA          E                     EXTFLD(MSGJBA)
008600100820     D                                     DIM(502)
008700100820     D*-------------
008800100820     D* posizione Cursore
008900100820     D CURSOR          DS
009000100820     D  RIRI                   1      2B 0 INZ
009100100820     D  R$$                    2      2
009200100820     D  COCO                   3      4B 0 INZ
009300100820     D  C$$                    4      4
009400100820     D*-------------
009500100820     D* Tasti di funzione
009600100820     D F01             C                   CONST(X'31')
009700100820     D F02             C                   CONST(X'32')
009800100820     D F03             C                   CONST(X'33')
009900100820     D F04             C                   CONST(X'34')
010000100820     D F05             C                   CONST(X'35')
010100100820     D F06             C                   CONST(X'36')
010200100820     D F07             C                   CONST(X'37')
010300100820     D F08             C                   CONST(X'38')
010400100820     D F09             C                   CONST(X'39')
010500100820     D F10             C                   CONST(X'3A')
010600100820     D F11             C                   CONST(X'3B')
010700100820     D F12             C                   CONST(X'3C')
010800100820     D F13             C                   CONST(X'B1')
010900100820     D F14             C                   CONST(X'B2')
011000100820     D F15             C                   CONST(X'B3')
011100100820     D F16             C                   CONST(X'B4')
011200100820     D F17             C                   CONST(X'B5')
011300100820     D F18             C                   CONST(X'B6')
011400100820     D F19             C                   CONST(X'B7')
011500100820     D F20             C                   CONST(X'B8')
011600100820     D F21             C                   CONST(X'B9')
011700100820     D F22             C                   CONST(X'BA')
011800100820     D F23             C                   CONST(X'BB')
011900100820     D F24             C                   CONST(X'BC')
012000100820     D ENTER           C                   CONST(X'F1')
012100100820     D ROLDWN          C                   CONST(X'F4')
012200100820     D ROLLUP          C                   CONST(X'F5')
012300100820     D*-------------
012400100820     D* Sottotitolo di testata
012500100820     D SOTTIT          C                   CONST('PROMSG    *LIBL-
012600100820     D                                          COS0095')
012601160503R224 D*-------------
012602160503  "  D* Sottotitolo di testata
012603160503  "  D $Sottit         S             27A
012604160503  "  D* "Soggetti"
012605160503  "  D SOTTIT1         C                   CONST('PROMSG    *LIBL-
012606160503  "  D                                          COS2504')
012608160503  "  D* "Variazione estremi fiscali"
012609160503  "  D SOTTIT2         C                   CONST('PROMSG    *LIBL-
012610160503  "  D                                          COS0095')
012611160503  "  D* "Nominativi di partita"
012614160503  "  D SOTTIT3         C                   CONST('PROMSG    *LIBL-
012615160503R224 D                                          COS0440')
012800100830     D*-------------
012801100830     D* campi di lavoro
012804100831     D $FlgDef         S              1A
012805100903     D $RepSO3         S              1A
012806100831     D UDateISO        S               D
012807100830     D WDtNasc         S               D
012808100830     D WDtFiVl         S               D
013202100830     D $CdFisc         S                   Like(SOGCdFisc )
013203100830     D $PartIVA        S                   Like(SOGPartIVA)
013204100830     D $CogRag         S                   Like(SOGDes   )
013205100830     D $Nome           S                   Like(SOGDes  )
013206100901     D $Sex            S                   Like(SOGSex  )
013207100830     D $DtNasc         S                   Like(SOGDtNasc)
013208100830     D $LocNasc        S                   Like(INDLocalit)
013209100830     D $PrvNasc        S              2A
013210100830     D $Stato          S                   Like(INDStato)
013211100830     D $StaFed         S                   Like(SOGDes   )
013212100830     D $Localit        S                   Like(INDLocalit)
013213100830     D $IndirE         S                   Like(INDIndriz)
013214160428R224 D WSogg           S                   Like(DVASogg)
013215160513 "   D $FoundPPA04     S              1A
013216160510 "   D WSys            S                   Like(PPASys )
013217160513 "   D WNrAsInt        S                   Like(PPANrAsInt)
013220160513 "   D WData           S               D
013221160520 "   D WDtPar          S               D
013222160520 "   D*----------------------------------------------------*
013223160520 "   D* Controllo chiamata ricorsiva
013224160520R224 D X48ChPgmDS    E DS                  INZ
013225170822R258 D*------
013226170822 "   D $CAP            S                   Like(INDCAP   )
013227170822 "   D $Prov           S                   Like(INDProv  )
013228170822 "   D*------
013229170822 "   D WStato          S                    Like(INDStato )
013230170822 "   D WCAP            S                    Like(INDCAP )
013231170822 "   D WProv           S                    Like(INDProv)
013232170822R258 D WLocalit        S                    Like(INDLocalit)
015200100820     C*----------------------------------------------------*
015300100820     C*                MAIN LINE PROGRAM
015400100820     C*----------------------------------------------------*
015500100820     C*
015600100830     C                   EXSR      INZVAR
015700100820     C*
015800100820     C     $FINE         DOWEQ     *OFF
015900100820     C     $GEST         CASEQ     'D1'          GESD1
016000100820     C                   ENDCS
016100100820     C                   ENDDO
016200100820     C*
016300100830     C                   EXSR      ENDPGM
016400100820     C*
016500100820     C************************************************************
016600100820     C* RICERCHE
016700100820     C************************************************************
016800100830     C     ENDPGM        BEGSR
016900100820     C*
017000100820     C* se ho confermato l'uscita proseguo
017100100820     C     $FINE         IFEQ      *ON
017200100820     C*
017300100820     C* disallocazione rcd nel caso di modifica/annullamento
017400100820     C     OPZD16        IFEQ      '02'
017500100820     C     OPZD16        OREQ      '04'
017600100820     C                   UNLOCK    SDGSO301L                            22
017700100820     C                   EndIf
017800100820     C*
017900100820     C* passaggio dati al pgm Chiamante
018000100820     C*
018100100820     C                   MoveL     SDGSD16DS     KPJBU
018200100820     C                   MoveL     KPJBU         SDGSD16DS
018300100820     C*
018400100820     C                   SETON                                            RT
018500100820     C                   RETURN
018600100820     C*
018700100820     C                   EndIf
018800100820     C*
018900100830     C                   ENDSR
018901100830     C/EJECT
019000100820     C************************************************************
019100100820     C* RICERCHE
019200100820     C************************************************************
019300100830     C     SEARCH        BEGSR
019400100820     C*
019500100820     C* determino Riga/Colonna del Cursore
019600100820     C                   Move      NRG           R$$
019700100820     C                   Move      NCL           C$$
019800100820     C                   Z-ADD     RIRI          CURR
019900100820     C                   Z-ADD     COCO          CURC
020000100820     C*
020100100830     C* I campi che al momento dell'F4 risultano protetti non devono
020200100830     C* essere abilitati alla funzione di ricerca
020300100820     C*
020400100820     C                   SELECT
020401160428R224 C*
020402160429 "   C* FMTD1 - Soggetto
020403160428 "   C     H1NMFL        WHENEQ    'D1SOGG'
020404160520 "   C*
020405160520  "  C* ctr chiamata ricorsiva
020406160520  "  C                   reset                   X48ChPgmds
020407160520  "  C                   eval      X48Pgm      = 'ANA040R '
020408160520  "  C                   eval      Kpjbu       = X48ChPgmDS
020409160520  "  C                   call      'X48CHPGM'
020410160520  "  C                   parm                    Kpjba
020411160520  "  C                   movel     Kpjbu         x48ChPgmDS
020412160520  "  C                   if        x48errore   = *Off
020413160520 "   C*
020414160428 "   C                   EXSR      F4_Sogg
020415160520 "   C*
020416160520 "   C                   endif
020417160429 "   C*
020418160429 "   C* FMTD1 - Data fine validità
020419160429 "   C     H1NMFL        WHENEQ    'D1DTFIVL'
020420160520 "   C*
020421160520  "  C* ctr chiamata ricorsiva
020422160520  "  C                   reset                   X48ChPgmds
020423160520  "  C                   eval      X48Pgm      = 'ANA057R '
020424160520  "  C                   eval      Kpjbu       = X48ChPgmDS
020425160520  "  C                   call      'X48CHPGM'
020426160520  "  C                   parm                    Kpjba
020427160520  "  C                   movel     Kpjbu         x48ChPgmDS
020428160520  "  C                   if        x48errore   = *Off
020429160520 "   C*
020430160429 "   C                   EXSR      F4_DtFiVl
020431160520 "   C*
020432160520 "   C                   endif
020433160513 "   C*
020434160513 "   C* FMTD1 - Capoconto
020435160513 "   C     H1NMFL        WHENEQ    'D1KCC   '
020436160513 "   C                   EXSR      F4_Kcc
020437160513 "   C*
020438160513 "   C* FMTD1 - sottoconto
020439160513 "   C     H1NMFL        WHENEQ    'D1KSC   '
020440160513 "   C                   EXSR      F4_Ksc
020441160513 "   C*
020442160513 "   C* FMTD1- PARTITA
020443160513 "   C     H1NMFL        WHENEQ    'D1DTPAR'
020444160513 "   C     H1NMFL        OrEq      'D1NRPAR'
020445160513 "   C     H1NMFL        OrEq      'D1SERPAR'
020446160523 "   C*
020447160523  "  C* ctr chiamata ricorsiva
020448160523  "  C                   reset                   X48ChPgmds
020449160523  "  C                   eval      X48Pgm      = 'NDC085R '
020450160523  "  C                   eval      Kpjbu       = X48ChPgmDS
020451160523  "  C                   call      'X48CHPGM'
020452160523  "  C                   parm                    Kpjba
020453160523  "  C                   movel     Kpjbu         x48ChPgmDS
020454160523  "  C                   if        x48errore   = *Off
020456160513 "   C*
020457160513 "   C                   if        D1Kcc      <> *Blank and
020458160513 "   C                             D1Ksc      <> *Blank
020459160513 "   C                   EXSR      CtrConto
020460160513 "   C                   if        *In99       = *Off
020461160513 "   C                   EXSR      F4_Partita
020462160513 "   C                   endif
020463160513 "   C                   else
020464160513 "   C* Campo obbligatorio
020465160513 "   C                   SetOn                                        5899
020466160513 "   C                   endif
020467160523 "   C*
020468160523 "   C                   endif
020469160428 "   C*
020470160428 "   C* FMTD1 - Destinazione
020471160428 "   C     H1NMFL        WHENEQ    'D1DEST    '
020472160428R224 C                   EXSR      F4_Dest
020473170822R258 C*
020474170822 "   C*  Provincia
020475170822 "   C     H1NMFL        WHENEQ    'D1PROV   '
020476170822 "   C                   MoveL     D1Prov        WProv
020477170822 "   C                   MoveL     D1Stato       WStato
020478170822 "   C                   EXSR      F4_Prov
020479170822 "   C                   if        WProv      <> *Blanks
020480170822 "   C                   MoveL     WProv         D1Prov
020481170822 "   C                   endif
020482170822 "   C                   if        D1Stato     = *Blanks   and
020483170822 "   C                             WStato     <> *Blanks
020484170822 "   C                   MoveL     WStato        D1Stato
020485170822 "   C                   endif
020486170822 "   C*
020487170822 "   C*  CAP
020488170822 "   C     H1NMFL        WHENEQ    'D1CAP    '
020489170822 "   C                   MoveL     D1CAP         WCAP
020490170822 "   C                   MoveL     D1Stato       wStato
020491170822 "   C                   EXSR      F4_CAP
020492170822 "   C                   if        WCAP       <> *Blanks
020493170822 "   C                   MoveL     WCAP          D1CAP
020494170822 "   C                   MoveL     WStato        D1Stato
020495170822 "   C                   MoveL     WProv         D1Prov
020496170822 "   C                   MoveL     WLocalit      D1Localit
020497170822R258 C                   endif
020498100901     C*
020499100901     C     H1NMFL        WHENEQ    'D1SEX'
020500100901     C                   EXSR      F4_Sex
020501100820     C*
020600100830     C* provincia di nascita
020700100830     C     H1NMFL        WHENEQ    'D1PRVNASC'
020800100820     C                   RESET                   XTABDS
020900100820     C                   Move      '01'          XTAOPZ
021000100820     C                   Move      XSCSOC        XTAAZI
021100100820     C                   Move      XSCLIN        XTALIN
021200100820     C                   Z-ADD     CURR          XTARIG
021300100820     C                   Z-ADD     CURC          XTACOL
021301100831     C*
021302100901     C                   Movel     XScSta        KeyC17
021303100831     C                   Move      KeyC17        XTBKey
021304100831     C*
021400100820     C                   CLEAR                   KPJBU
021500100820     C                   MoveL     XTABDS        KPJBU
021501100831     C*
021600100820     C                   CALL      'ANGC17R'
021700100820     C                   PARM                    KPJBA
021701100831     C*
021800100820     C                   MoveL     KPJBU         XTABDS
022301100830     C*
022302100830     C                   if        XTaErr     <> '1'  and
022303100830     C                             XTaKey     <> *Blank
022304100830     C                   Move      XTaKey        D1PrvNasc
022305100830     C                   endif
023100100820     C*
023101100830     C*
023300100830     C     H1NMFL        WHENEQ    'D1STATO'
023400100820     C                   RESET                   XTABDS
023500100820     C                   Move      '01'          XTAOPZ
023600100820     C                   Move      XSCSOC        XTAAZI
023700100820     C                   Move      XSCLIN        XTALIN
023800100820     C                   Z-ADD     CURR          XTARIG
023900100820     C                   Z-ADD     CURC          XTACOL
024000100820     C                   CLEAR                   KPJBU
024100100820     C                   MoveL     XTABDS        KPJBU
024200100820     C                   CALL      'ANGG12R'
024300100820     C                   PARM                    KPJBA
024400100820     C                   MoveL     KPJBU         XTABDS
024401100830     C*
024402100830     C                   if        XTaErr     <> '1'  and
024403100830     C                             XTaKey     <> *Blank
024404160513R224 X****               Move      XTaKey        D1PrvNasc
024405160513R224 C                   Move      XTaKey        D1Stato
024406100830     C                   endif
027600100820     C*
027700100830     C* ricerca non abilitata in questa posizione
027800100820     C                   OTHER
028200100820     C                   SETON                                        9899
028300100820     C*
028400100820     C                   ENDSL
028500100820     C*
028600100820     C                   Move      *HIVAL        $LASTV
028601100830     C*
028602100830     C* imposto pos. del cursore
028603100830     C                   Z-ADD     CURR          H1RIGA
028604100830     C                   Z-ADD     CURC          H1COLO
028605100830     C*
028700100830     C                   ENDSR
028701100830     C/EJECT
028702160428R224 C************************************************************
028703160428 "   C* Ricerca soggetto
028704160428 "   C************************************************************
028705160428R224 C     F4_Sogg       BEGSR
028706160428     C*
028707160428     C                   RESET                   ANA040DS
028708160428     C*
028709160428     C                   MOVE      '01'          OPZ040
028710160428     C*
028712160429     C                   if        OpzD16      = '02'
028714160428     C                   MOVE      '05'          OPZ040
028715160428     C                   endif
028716160428     C*
028717160428     C                   CLEAR                   KPJBU
028718160428     C                   MOVEL     ANA040DS      KPJBU
028719160428     C                   CALL      'ANA040R'
028720160428     C                   PARM                    KPJBA
028721160428     C                   MOVEL     KPJBU         ANA040DS
028722160428     C*
028723160428     C                   if        Cod040     <> *Blank
028724160428     C                   MoveL     Cod040        D1Sogg
028725160428     C                   endif
028726160428     C*
028727160428R224 C                   ENDSR
028728160428R224 C/EJECT
028729160429R224 C************************************************************
028730160429 "   C* Ricerca dati fiscali storicizzati
028731160429 "   C************************************************************
028732160429R224 C     F4_DtFiVl     BEGSR
028733160429     C*
028734160429     C                   RESET                   ANA057DS
028735160429     C*
028736160429     C                   MOVE      '01'          OPZ057
028737160429     C*
028739160429     C                   if        OpzD16      = '02'
028741160429     C                   MOVE      '05'          OPZ057
028742160429     C                   endif
028743160429     C*
028744160429     C                   Eval      Sog057      = D1Sogg
028745160429     C                   if        D1DtFiVl   <> *Zero
028746160429     C     *Jobrun       move      D1DtFiVl      DtF057
028747160429     C                   endif
028748160429     C*
028749160429     C                   CLEAR                   KPJBU
028750160429     C                   MOVEL     ANA057DS      KPJBU
028751160429     C                   CALL      'ANA057R'
028752160429     C                   PARM                    KPJBA
028753160429     C                   MOVEL     KPJBU         ANA057DS
028754160429     C*
028755160429     C                   if        DtF057     <> *Loval
028756160429     C     *JobRun       Move      DtF057        D1DtFiVl
028757160429     C                   endif
028758160429     C*
028759160429R224 C                   ENDSR
028760160429R224 C/EJECT
028761160513R224 C************************************************************
028762160513 "   C* Ricerca capoconto
028763160513 "   C************************************************************
028764160513R224 C     F4_Kcc        BEGSR
028765160513     C*
028766160513     C                   RESET                   ANA200DS
028767160513     C*
028768160513     C                   MOVEL     '01'          OPZ200
028769160513     C                   MOVE      D1KCC         KCC200
028770160513     C*
028771160513     C                   MOVEL     ANA200DS      KPJBU
028772160513     C*
028773160513     C                   CALL      'ANA200R'
028774160513     C                   PARM                    KPJBA
028775160513     C*
028776160513     C                   MOVEL     KPJBU         ANA200DS
028777160513     C*
028778160513     C     KCC200        IFNE      *BLANK
028779160513     C                   MOVEL     KCC200        D1KCC
028780160513     C                   ENDIF
028781160513     C*
028782160513     C* imposto pos. del cursore
028783160513     C                   Z-ADD     CURR          H1RIGA
028784160513     C                   Z-ADD     CURC          H1COLO
028785160513     C*
028786160513R224 C                   ENDSR
028787160513R224 C/EJECT
028788160513R224 C************************************************************
028789160513 "   C* Ricerca sottoconto
028790160513 "   C************************************************************
028791160513R224 C     F4_Ksc        BEGSR
028792160513     C*
028793160513     C                   RESET                   ANA201DS
028794160513     C*
028795160513     C                   MOVE      XSCSOC        SOC201
028796160513     C                   MOVE      D1KCC         KCC201
028797160513     C                   MOVE      D1KsC         KSC201
028798160513     C*
028799160513     C                   MOVEL     ANA201DS      KPJBU
028800160513     C                   CALL      'ANA201R'
028801160513     C                   PARM                    KPJBA
028802160513     C                   MOVEL     KPJBU         ANA201DS
028803160513     C*
028804160513     C     KSC201        IFNE      *BLANK
028805160513     C                   MOVEL     KCC201        D1KCC
028806160513     C                   MOVEL     KSC201        D1KSC
028807160513     C                   ENDIF
028808160513     C*
028809160513     C* imposto pos. del cursore
028810160513     C                   Z-ADD     CURR          H1RIGA
028811160513     C                   Z-ADD     CURC          H1COLO
028812160513     C*
028813160513R224 C                   ENDSR
028814160513R224 C/EJECT
028815160513R224 C************************************************************
028816160513 "   C* GESTIONE F4 su partita
028817160513 "   C************************************************************
028818160513R224 C     F4_Partita    BEGSR
028819160513     C*
028820160513     C                   Clear                   NDC085DS
028821160513     C*
028822160513     C                   MOVEL     '01'          OPZ085
028823160513     C                   MOVE      CtbD16        Ctb085
028824160513     C                   MOVE      *Blank        Unita085
028825160513     C*
028826160513     C                   if        D1Kcc      <> 'C'     and
028827160513     C                             D1Kcc      <> 'F'
028828160513     C*
028829160513     C                   CALLB     'XALLINEA'
028830160513     C                   PARM                    D1Kcc
028831160513     C                   PARM      6             LEN               5 0
028832160513     C                   PARM                    XSCINP
028833160513     C                   endif
028834160513     C*
028835160513     C                   CALLB     'XALLINEA'
028836160513     C                   PARM                    D1Ksc
028837160513     C                   PARM      8             LEN               5 0
028838160513     C                   PARM                    XSCINP
028839160513     C*
028840160513     C* reperisco capoconto se cliente/fornitore
028841160513     C                   if        D1Kcc       = 'C       '     or
028842160513     C                             D1Kcc       = 'F       '
028843160513     C                   CALLB     'XCAPCLIFOR'
028844160513     C                   PARM      XScSoc        XCAPSOCIET        3
028845160513     C     D1Kcc         PARM      D1Kcc         XCAPKCC           6
028846160513     C     D1Ksc         PARM      D1Ksc         XCAPKSC           8
028847160513     C                   endif
028848160513     C*
028849160513     C                   MOVE      D1Kcc         Kcc085
028850160513     C                   MOVE      D1Ksc         Ksc085
028851160513     C* ..Mod.interr.E/C: 1=x partita
028852160513     C                   Eval      Modalit085 =  '1'
028853160513     C* ..Movimenti Defin./Provvis./Gestionali = Si
028854160513     C                   Eval      MovDef085  =  *On
028855160513     C                   Eval      MovProv085 =  *On
028856160513     C                   Eval      MovGes085  =  *On
028857160513     C* ..Ordine: Ascendente per scegliere prima le partite + vecchie
028858160513     C                   Eval      Ord085     =  'A'
028859160513     C* ..Visualizzo le partite in divisa effettiva
028860160513     C                   Eval      DivVis085  =  '1'
028861160513     C* ..Includo la divisa della proposta
028862160513     C                   Eval      DivSel085  =  'I'
028863160513     C**********         Eval      Divisa085  =  D1Divisa
028864160513     C* ..Interrogazione multipla: No
028865160513     C                   Eval      EstMul085  =  *Off
028866160513     C* ..Situaz. partite: 1=Tutte le partite
028867160513     C                   Eval      SitPar085  =  '1'
028868160513     C* ..Ripresa Saldo: No
028869160513     C                   Eval      RipSald085 =  *Off
028870160513     C* ..Selezione multipla : Si
028871160513     C                   Eval      Multip085  =  *On
028872160513     C*
028873160513     C* ..Pulisco la ds multipla prima di chiamare
028874160513     C                   Do        50            $X                4 0
028875160513     C     $x            Occur     SkPartite
028876160513     C                   CLEAR                   SkPartite
028877160513     C                   EndDo
028878160513     C*
028879160513     C                   CLEAR                   $DSAUT
028880160513     C                   MOVEL     NDC085DS      KPJBU
028881160513     C*
028882160513     C                   CALL      'NDC085R'
028883160513     C                   PARM                    KPJBA
028884160513     C                   PARM                    $DSAUT
028885160513     C                   PARM                    SkPartite
028886160513     C*
028887160513     C                   MOVEL     KPJBU         NDC085DS
028888160513     C*
028889160513     C* gestione ritorno dall'F4 su partita
028890160513     C                   EXSR      RTNF4Par
028891160513     C*
028892160513R224 C                   ENDSR
028893160513R224 C/EJECT
028894160513R224 C************************************************************
028895160513 "   C* Ritorno dall'F4 su partite
028896160513 "   C************************************************************
028897160513R224 C     RTNF4Par      BEGSR
028898160513     C*
028899160513     C* Ciclo per trovare almeno una partita...
028900160513     C                   Do        1             $X
028901160513     C     $X            Occur     SkPartite
028902160513     C*
028903160513     C* Se c'è almeno una partita...
028904160513     C     SkNrPar       IfNe      0
028905160513     C*
028906160513     C* ..valorizzo rif. partita
028907160513     C     SkDtPar       IfNe      *Loval
028908160513     C     *JobRun       Move      SkDtPar       D1DtPar
028909160513     C                   EndIf
028910160513     C                   Eval      D1NrPar    =  SkNrPar
028911160513     C                   Eval      D1SerPar   =  SkSerPar
028912160513     C*
028913160513     C                   Leave
028914160513     C                   EndIf
028915160513     C*
028916160513     C                   EndDo
028917160513     C*
028918160513R224 C                   ENDSR
028919160513R224 C/EJECT
028920160428R224 C************************************************************
028921160428 "   C* RICERCA Destinazione anagrafica
028922160428 "   C************************************************************
028923160428R224 C     F4_Dest       BEGSR
028924160428     C*
028925160428     C                   RESET                   XTABDS
028926160428     C*
028927160428     C                   MOVE      '01'          XTAOPZ
028928160428     C                   MOVE      XSCSOC        XTAAZI
028929160428     C                   MOVE      XSCLIN        XTALIN
028930160428     C                   Z-ADD     CURR          XTARIG
028931160428     C                   Z-ADD     CURC          XTACOL
028932160428     C                   CLEAR                   KPJBU
028933160428     C                   MOVEL     XTABDS        KPJBU
028934160428     C                   CALL      'SDGHC4R'
028935160428     C                   PARM                    KPJBA
028936160428     C                   MOVEL     KPJBU         XTABDS
028937160428     C*
028938160428     C                   if        XTaErr     <> '1'  and
028939160428     C                             XTaKey     <> *Blank
028940160428     C                   Move      XTaKey        D1Dest
028941160428     C                   endif
028942160428     C*
028943160428R224 C                   ENDSR
028944160428R224 C/EJECT
028945100901     C************************************************************
028946100901     C* Ricerca sesso
028947100901     C************************************************************
028948100901     C     F4_Sex        BEGSR
028949100901     C*
028950100901     C                   Reset                   ANA964DS
028951100901     C*
028952100901     C                   MoveL     '01'          Opz964
028953100901     C                   MoveL     'ANSOG000'    Trc964
028954100901     C                   MoveL     'SOGSEX   '   Cam964
028955100901     C*
028956100901     C                   MoveL     ANA964DS      KPJBU
028957100901     C                   Call      'ANA964R'
028958100901     C                   Parm                    KPJBA
028959100901     C                   MoveL     KPJBU         ANA964DS
028960100901     C*
028961100901     C                   if        Ret964     <> '2'
028962100901     C                   MoveL     Val964        D1Sex
028963100901     C                   endif
028964100901     C*
028965100901     C                   ENDSR
028966100901      /EJECT
028967170822R258 C************************************************************
028968170822 "   C* Ricerca codice provincia
028969170822 "   C************************************************************
028970170822R258 C     F4_Prov       BEGSR
028971170822     C*
028972170822     C                   Reset                   XTabDS
028973170822     C*
028974170822     C                   Clear                   Campo6            6
028975170822     C*
028976170822     C                   Move      '01'          XTaOpz
028977170822     C                   Move      XScSoc        XTaAzi
028978170822     C                   Move      XScLin        XTaLin
028979170822     C                   Z-add     Curr          XTaRig
028980170822     C                   Z-add     Curc          XTaCol
028981170822     C                   Move      *Blank        XTaKey
028982170822     C                   Movel     WStato        Campo6
028983170822     C                   Move      Campo6        XtaKey
028984170822     C*
028985170822     C                   Clear                   KPJBU
028986170822     C                   MoveL     XTabDS        KPJBU
028987170822     C*
028988170822     C                   CALL      'ANGC17R'
028989170822     C                   PARM                    KPJBA
028990170822     C                   MoveL     KPJBU         XTabDS
028991170822     C*
028992170822     C                   if        XTaErr     <> '1'      and
028993170822     C                             XTaKey     <> *Blank
028994170822     C                   Move      XTaKey        Campo6
028995170822     C                   Move      Campo6        WProv
028996170822     C                   Movel     Campo6        WStato
028997170822     C                   endif
028998170822     C*
028999170822R258 C                   ENDSR
029000170822R258  /EJECT
029001170822R258 C************************************************************
029002170822 "   C* Ricerca codice CAP
029003170822 "   C************************************************************
029004170822R258 C     F4_CAP        BEGSR
029005170822     C*
029006170822     C                   Clear                   WCAP
029007170822     C*
029008170822     C                   Reset                   A801DS
029009170822     C                   Move      '01'          OPZ801
029010170822     C                   Move      XscSoc        Societa801
029011170822     C                   Movel     WStato        Stato801
029012170822     C                   Movel     WCap          Cap801
029013170822     C                   Z-Add     CURR          XTARIG
029014170822     C                   Z-Add     CURC          XTACOL
029015170822     C                   Clear                   KPJBU
029016170822     C                   Movel     A801DS        KPJBU
029017170822     C                   Call      'ANA801R'
029018170822     C                   Parm                    KPJBA
029019170822     C                   Movel     KPJBU         A801DS
029020170822     C*
029021170822     C     ERR801        IfNe      '1'
029022170822     C                   MoveL     CAP801        WCAP
029023170822     C                   Else
029024170822     C                   MoveL     *BLANK        WCAP
029025170822     C                   EndIf
029026170822     C*
029027170822     C                   if        WCAP       <> *Blanks
029028170822     C                   Eval      WStato      = Stato801
029029170822     C                   Eval      WProv       = Prov801
029030170822     C                   Eval      WLocalit    = Localit801
029031170822     C                   endif
029032170822     C*
029033170822R258 C                   ENDSR
029034170822R258  /EJECT
029035100820     C************************************************************
029036100820     C* GESTIONE EMISSIONE TASTI FUNZIONE
029037100820     C************************************************************
029100100830     C     GESPIE        BEGSR
029200100820     C*
029300100820     C                   SELECT
029400100820     C*
029500100820     C     $GEST         WHENEQ    'D1'
029600100820     C     $ULKD1        IFEQ      0
029700100820     C* riempimento opzioni
029800100830     C                   EXSR      F24D1
029900100820     C                   ELSE
030000100820     C                   WRITE     FMTZ1
030100100820     C                   EndIf
030200100820     C                   ENDSL
030300100820     C*
030400100830     C                   ENDSR
030401100830     C/EJECT
030500100820     C************************************************************
030600100820     C* GESTIONE VIDEO RECORD D1
030700100820     C************************************************************
030800100830     C     GESD1         BEGSR
030900100820     C* inizializzazione videata
031000100820     C     $INZD1        IFEQ      *ON
031100100830     C                   EXSR      INZD1
031200100820     C                   Move      *OFF          $INZD1
031300100820     C                   EndIf
031400100820     C* emissione piede videata se proveniente da altra
031500100820     C* salvataggio valore formato di provenienza
031600100820     C     $LASTV        IFNE      'D1'
031700100820     C                   WRITE     FMTT1
031800100830     C                   EXSR      GESPIE
031900100820     C                   Move      $LASTV        $LASTG
032000100820     C                   Move      'D1'          $LASTV
032100100820     C                   EndIf
032200100820     C*
032300100820     C* se esistono errori sulla videata
032400100820     C* emetto la write del formato a indiCatori spenti per vedere
032500100820     C* le eventuali deCodifiChe
032600100820     C     *IN99         IFEQ      *ON
032700100820     C     $GEST         ANDEQ     'D1'
032800100820     C                   MoveA     *IN           WIN
032900100820     C                   Move      *ALL'0'       IN5098           49
033000100820     C                   MoveA     IN5098        *IN(50)
033100160510R224 X****               WRITE     FMTD1
033101160510R224 C                   EXSR      WriteD1
033200100820     C                   MoveA     WIN           *IN
033300100820     C                   EndIf
033400100820     C*
033500100820     C                   SELECT
033600100820     C* Annullamento
033700100820     C     OPZD16        WHENEQ    '04'
033800100820     C* Visualizzazione
033900100820     C     OPZD16        OREQ      '05'
033901160510R224 X****               WRITE     FMTD1
033902160510R224 C                   EXSR      WriteD1
034100100820     C                   EXFMT     PROTECT
034200100820     C* Immissione/ModifiCa/Copia
034300100820     C                   OTHER
034400160510R224 X****               EXFMT     FMTD1
034401160510R224 C                   EXSR      EXFMTD1
034500100820     C                   ENDSL
034600100820     C* Controllo abilitazione tasti funzione
034700100820     C*
034800100830     C                   EXSR      CTRKD1
034900100820     C*
035000100820     C     *IN99         IFEQ      *OFF
035100100820     C*
035200100820     C                   SELECT
035300100820     C* F2=DeCodifiCa
035400100820     C     $TASTO        WHENEQ    F02
035500100830     C                   EXSR      F02D1
035600100820     C* F3=Fine
035700100820     C     $TASTO        WHENEQ    F03
035800100830     C                   EXSR      F03D1
035900100820     C* F4=RiCerCa
036000100820     C     $TASTO        WHENEQ    F04
036100100830     C                   EXSR      SEARCH
036200100820     C* F5=Refresh
036300100820     C     $TASTO        WHENEQ    F05
036400100830     C                   EXSR      F05D1
036500100820     C* F8=SuCCessivo
036600100820     C     $TASTO        WHENEQ    F08
036700100830     C                   EXSR      F08D1
036800100820     C* F12=Ritorno
036900100820     C     $TASTO        WHENEQ    F12
037000100830     C                   EXSR      F12D1
037001160429R224 X**** F16=Cancellazione
037002160429 "   X**** $TASTO        WHENEQ    F16
037003160429R224 X****               EXSR      F16D1
037100100820     C* F24=Altri tasti
037200100820     C     $TASTO        WHENEQ    F24
037300100830     C                   EXSR      F24D1
037400100820     C*
037500100820     C                   OTHER
037600100830     C                   EXSR      CTRD1
037700100820     C*
037800100820     C     OPZD16        IFNE      '05'
037900100820     C     $TASTO        IFEQ      F06
038000100820     C     *IN99         ANDEQ     *OFF
038100100820     C* eseguo aggiornamento
038200100830     C                   EXSR      AGGANA
038300100820     C* eseguo operazioni del dopo-aggiornamento
038400100820     C     *IN99         IFEQ      *OFF
038500100830     C                   EXSR      GESAGG
038600100820     C                   EndIf
038700100820     C                   EndIf
038800100820     C                   EndIf
038900100820     C*
039000100820     C                   ENDSL
039100100820     C*
039200100820     C                   EndIf
039300100820     C*
039400100830     C                   ENDSR
039401100830     C/EJECT
039500100820     C************************************************************
039600100820     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
039700100820     C************************************************************
039800100830     C     CTRKD1        BEGSR
039900100820     C*
040000100820     C                   SETOFF                                       99
040100100820     C                   MoveA     $FC1          $FC
040200100830     C                   EXSR      CTRKEY
040300100820     C     *IN99         IFEQ      *ON
040400100820     C                   SETON                                        97
040500100820     C                   EndIf
040600100820     C*
040700100830     C                   ENDSR
040701100830     C/EJECT
040800100820     C************************************************************
040900100820     C* INIZIALIZZAZIONE VIDEATA DATI
041000100820     C************************************************************
041100100830     C     INZD1         BEGSR
041200100820     C*
041300100820     C                   CLEAR                   FMTD1
041301160510R224 C                   CLEAR                   FMTD2
041400100820     C*
041500100820     C                   Z-ADD     1             $PIED1
041501100831     C*
041502100831     C                   Eval      SO3Sogg     = SoggD16
041503100831     C                   Eval      SO3DtFiVl   = DtFiVlD16
041504100831     C                   Eval      SO3Sys      = SysD16
041505100831     C                   Eval      SO3NrAsInt  = NrAsIntD16
041506160428R224 C                   Eval      SO3Dest     = DestD16
041600100820     C*
041601160505R224 C* se il soggetto è ricevuto, lo proteggo
041602160505     C                   if        SoggD16    <> *Blank
041603100831     C                   Eval      *In11       = *On
041604100831     C                   endif
041610100831     C*
041611160505R224 C* se la data fine validità è ricevuta, la proteggo
041612160505     C                   if        DtFiVlD16  <> *Loval
041615100831     C                   Eval      *In12       = *On
041616100831     C                   endif
041617160513R224 C*
041618160513 "   C* se chiamata da anagrafica dati fiscali storicizzati o da menu e dati
041619160513 "   C* anagrafici del soggetto, mostro
041620160513 "   C* la data di fine validità
041621160513 "   C                   if        TpCallD16   = '2'         or
041622160513 "   C                             (TpCallD16   = ' '   and
041623160513 "   C                              TpDatid16   = '1'      )
041624160513 "   C                   Eval      *In14       = *On
041625160513R224 C                   endif
041626100831     C*
041700100820     C     OPZD16        IFNE      '01'
042000100820     C*
042100100820     C     *IN22         DOUEQ     *OFF
042200100820     C     MSGRTN        ORNE      'R'
042300100820     C* se ModifiCa/Annullamento, il rCd Che leggo viene bloCCato
042400100820     C     OPZD16        IFEQ      '02'
042500100820     C     OPZD16        OREQ      '04'
042600160428R224 X**** K04SO301      CHAIN     SDGSO301L                          2122
042601160428R224 C     K05SO301      CHAIN     SDGSO301L                          2122
042700100820     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
042800100820     C                   ELSE
042900160428R224 X**** K04SO301      CHAIN(N)  SDGSO301L                          2122
042901160428R224 C     K05SO301      CHAIN(N)  SDGSO301L                          2122
042902100830     C                   ENDIF
043100100820     C*
043200100820     C* Gestione RCD allocato
043300100820     C     *IN22         IFEQ      *ON
043400100820     C                   MoveL     'SDGSO300F'   MSGFIL
043500100830     C                   EXSR      MSGPGM
043600100830     C                   ENDIF
043700100820     C                   ENDDO
043800100820     C*
043900100830     C* se il rcd da gestire non viene trovato
044000100820     C* oppure è alloCato da altro pgm
044100100820     C* ritorno al pgm guida avvertendolo dell'errore
044200100820     C     *IN21         IFEQ      *ON
044300100820     C     *IN22         OREQ      *ON
044400100820     C                   Move      *ON           $FINE
044500100820     C                   Move      '1'           ERRD16
044600100820     C*
044700100820     C* altrimenti
044800100820     C                   ELSE
044900100830     C* alloco in modo esclusivo i rcd
045000100830     C*****OPZD16        IFEQ      '02'
045100100830     C*****OPZD16        OREQ      '04'
045200100830     C********           UPDATE    SDGSO3000
045300100830     C********++         EndIf
045301160510R224 C* se dati da particolarità di partita, recupero i dati
045302160510 "   C* della partita
045303160510 "   C                   if        TpDatiD16   = '2'
045304160510 "   C                   Eval      WSys        = SysD16
045305160510 "   C                   Eval      WNrAsInt    = NrAsIntD16
045306160510 "   C                   EXSR      ChainPPA
045307160510R224 C                   endif
045400100820     C* riempio la videata
045500100830     C                   EXSR      RIED1
045600100830     C* richiamo routine dei ctrl per decodificarla
045700100830     C                   EXSR      CTRD1
045800100820     C*
045900100820     C                   Move      *ALL'0'       IN5098
046000100820     C                   MoveA     IN5098        *IN(50)
046100100820     C* definizione indiCatori su video
046200100820     C                   SETOFF                                       02
046300100820     C                   SETOFF                                       04
046400100820     C                   SELECT
046500100820     C     OPZD16        WHENEQ    '02'
046600100820     C                   SETON                                        02        PR Campi key
046700100820     C     OPZD16        WHENEQ    '04'
046800100820     C                   SETON                                        02        PR Campi key
046900100820     C     OPZD16        WHENEQ    '05'
047000100820     C                   ENDSL
047100100820     C*
047200100830     C                   ENDIF
047300100820     C*
047400100820     C                   ELSE
047500100820     C*
047600100830     C* riempio con i default di immissione
047601100830     C                   IF        $FlgDef = *on
047602160523R224 C                             and  (SoggD16 <> *Blank   or
047603160523R224 C                                   NrAsIntD16 <> *Zero   )
047700100830     C                   EXSR      RIEDEF
047701100830     C                   ENDIF
047800100820     C*
047900100830     C* imposto i campi chiave ricevuti anche se si è in immissione
048000100830     C* perchè potrebbero essere stati passati dei default di chiave
048500100830     C                   Eval      D1Sogg      = SoggD16
048501100830     C                   if        DtFiVlD16  <> *Loval
048502100830     C     *JobRun       Move      DtFiVlD16     D1DtFiVl
048503100830     C                   endif
048504160510R224 X****               Eval      H1Sys       = SysD16
048505160510R224 X****               Eval      H1NrAsInt   = NrAsIntD16
048506160510R224 C                   Eval      D1Sys       = SysD16
048507160510 "   C                   Eval      D1NrAsInt   = NrAsIntD16
048508160510 "   C                   Eval      D1Dest      = DestD16
048509160510 "   C                   Eval      D1Kcc       = KccD16
048510160510 "   C                   Eval      D1Ksc       = KscD16
048511160510 "   C                   if        DtParD16   <> *Loval
048512160510 "   C     *JobRun       Move      DtParD16      D1DtPar
048513160510 "   C                   endif
048514160510 "   C                   Eval      D1NrPar     = NrParD16
048515160513R224 C                   Eval      D1SerPar    = SerParD16
048600100820     C*
048700100830     C                   ENDIF
048800100820     C*
048900100830     C                   ENDSR
048901100830     C/EJECT
048902160510R224 C************************************************************
048903160510 "   C* Reperimento dati da particolarità di partita
048904160510 "   C************************************************************
048905160510R224 C     ChainPPA      BEGSR
048906160510     C*
048907160513     C                   Eval      $FoundPPA04 = *Off
048908160510     C*
048909160510     C                   Eval      PPASys      = WSys
048910160510     C                   Eval      PPANrAsInt  = WNrAsInt
048911160510     C     K02PPA04      CHAIN     NDPPA04L                           21
048912160510     C                   if        *In21       = *Off
048913160513     C                   Eval      $FoundPPA04 = *On
048914160510     C                   endif
048915160510     C*
048916160510R224 C                   ENDSR
048917160510R224 C/EJECT
049000100820     C************************************************************
049100100820     C* RIEMPIMENTO VIDEATA  D1
049200100820     C************************************************************
049300100830     C     RIED1         BEGSR
049400100820     C*
049500100830     C* impostare i campi della videata dai campi del file
049600100820     C*
049601100830     C                   Eval      D1Sogg      = SO3Sogg
049602100830     C                   if        SO3DtFiVl  <> *Loval
049700100830     C     *JobRun       Move      SO3DtFiVl     D1DtFiVl
049701100830     C                   endif
049702160510R224 X****               Eval      H1Sys       = SO3Sys
049703160510R224 X****               Eval      H1NrAsInt   = SO3NrAsInt
049704160510R224 C                   Eval      D1Sys       = SO3Sys
049705160510 "   C                   Eval      D1NrAsInt   = SO3NrAsInt
049706160510 "   C*
049707160513 "   C                   if        $FoundPPA04 = *On
049708160510 "   C                   Eval      D1Kcc       = PPAKcc
049709160510 "   C                   Eval      D1Ksc       = PPAKsc
049710160510 "   C                   if        PPADtPar   <> *Loval
049711160510 "   C     *JobRun       Move      PPADtPar      D1DtPar
049712160510 "   C                   endif
049713160510 "   C                   Eval      D1NrPar     = PPANrPar
049714160510 "   C                   Eval      D1SerPar    = PPASerPar
049716160510 "   C                   endif
049717160510 "   C*
049718160428R224 C                   Eval      D1Dest      = SO3Dest
050300100820     C*
050301100830     C                   Eval      D1CdFisc    = SO3CdFisc
050302100830     C                   Eval      D1PartIVA   = SO3PartIVA
050303100830     C                   Eval      D1CogRag    = SO3CogRag
050304100830     C                   Eval      D1Nome      = SO3Nome
050305100901     C                   Eval      D1Sex       = SO3Sex
050306100831     C*
050307100830     C                   if        SO3DtNasc  <> *Loval
050308100831     C                   Select
050309100831     C* formato mm/gg/aaaa
050310100831     C                   When      $DatFmt     = '*MDY'
050311100831     C     *USA          Move      SO3DtNasc     D1DtNasc
050312100831     C* formato aa/mm/gg
050313100831     C                   When      $DatFmt     = '*YMD'
050314100831     C     *ISO          Move      SO3DtNasc     D1DtNasc
050316100831     C* formato gg/mm/aaaa
050317100831     C                   When      $DatFmt     = '*DMY'
050318100831     C     *EUR          Move      SO3DtNasc     D1DtNasc
050320100831     C                   endSl
050321100831     C                   endif
050322100831     C*
050323100830     C                   Eval      D1LocNasc   = SO3LocNasc
050324100830     C                   Eval      D1PrvNasc   = SO3PrvNasc
050325100830     C                   Eval      D1Localit   = SO3Localit
050326100830     C                   Eval      D1Stato     = SO3Stato
050327100830     C                   Eval      D1StaFed    = SO3StaFed
050328100830     C                   Eval      D1IndirE    = SO3IndirE
050329170822R258 C                   Eval      D1CAP       = SO3CAP
050330170822R258 C                   Eval      D1Prov      = SO3Prov
052200100820     C*
052300100830     C                   ENDSR
052400100820     C/EJECT
052500100820     C************************************************************
052600100820     C* RIEMPIMENTO CAMPI DAI DEFAULT
052700100820     C************************************************************
052800100830     C     RIEDEF        BEGSR
052900100820     C*
053000100830     C* riempire i campi con i default recuperati dal codice di
053100100820     C* riferimento
053101100830     C                   Eval      D1PartIVA   = $PartIVA
053102100830     C                   Eval      D1CdFisc    = $CdFisc
053103100830     C                   Eval      D1CogRag    = $CogRag
053104100830     C                   Eval      D1Nome      = $Nome
053105100901     C                   Eval      D1Sex       = $Sex
053106100831     C*
053107100830     C                   if        $DtNasc    <> *Loval
053109100831     C                   Select
053110100831     C* formato mm/gg/aaaa
053111100831     C                   When      $DatFmt     = '*MDY'
053112100831     C     *USA          Move      $DtNasc       D1DtNasc
053113100831     C* formato aa/mm/gg
053114100831     C                   When      $DatFmt     = '*YMD'
053115100831     C     *ISO          Move      $DtNasc       D1DtNasc
053116100831     C* formato gg/mm/aaaa
053117100831     C                   When      $DatFmt     = '*DMY'
053118100831     C     *EUR          Move      $DtNasc       D1DtNasc
053119100831     C                   endSl
053120100831     C                   endif
053121100831     C*
053122100830     C                   Eval      D1LocNasc   = $LocNasc
053123100830     C                   Eval      D1PrvNasc   = $PrvNasc
053124100830     C                   Eval      D1Stato     = $Stato
053125100830     C                   Eval      D1StaFed    = $StaFed
053126100830     C                   Eval      D1Localit   = $Localit
053127100830     C                   Eval      D1IndirE    = $IndirE
053128170822R258 C                   Eval      D1CAP       = $CAP
053129170822R258 C                   Eval      D1Prov      = $Prov
055000100820     C*
055100100830     C                   ENDSR
055101100830     C/EJECT
055200100820     C************************************************************
055300100820     C* GESTIONE F02 VIDEO D1
055400100820     C************************************************************
055500100830     C     F02D1         BEGSR
055600100820     C*
055700100820     C* per deCodifiCare la videata riChiamo la routine dei Ctrl
055800100820     C* e poi disattivo gli eventuali messaggi di errore
055900100830     C                   EXSR      CTRD1
056000100820     C*
056100100820     C                   Move      *ALL'0'       IN5098
056200100820     C                   MoveA     IN5098        *IN(50)
056300100820     C*
056400100830     C                   ENDSR
056401100830     C/EJECT
056500100820     C************************************************************
056600100820     C* GESTIONE F03 VIDEO D1
056700100820     C************************************************************
056800100830     C     F03D1         BEGSR
056900100820     C*
057000100820     C                   Move      *ON           $FINE
057100100820     C                   Move      '1'           RETD16
057200100820     C* fine programma
057300100830     C                   EXSR      ENDPGM
057400100820     C*
057500100830     C                   ENDSR
057501100830     C/EJECT
057502160510R224 C************************************************************
057503160510 "   C* Scrittura videata D1/D2
057504160510 "   C************************************************************
057505160510R224 C     WriteD1       BEGSR
057506160510     C*
057507160510     C                   Select
057508160510     C                   When      TpDatiD16   = '1'
057509160510     C                   WRITE     FMTD1
057510160510     C*
057511160510     C*
057512160510     C                   When      TpDatiD16   = '2'
057514160510     C                   WRITE     FMTD2
057515160510     C*
057516160510     C                   endSl
057517160510     C*
057518160510R224 C                   ENDSR
057519160510R224 C/EJECT
057520160510R224 C************************************************************
057521160510 "   C* Emissione videata D1/D2
057522160510 "   C************************************************************
057523160510R224 C     ExfmtD1       BEGSR
057524160510     C*
057525160510     C                   Select
057526160510     C                   When      TpDatiD16   = '1'
057527160510     C                   EXFMT     FMTD1
057528160510     C*
057529160510     C*
057530160510     C                   When      TpDatiD16   = '2'
057531160510     C                   EXFMT     FMTD2
057532160510     C*
057533160510     C                   endSl
057534160510     C*
057535160510R224 C                   ENDSR
057536160510R224 C/EJECT
057600100820     C************************************************************
057700100820     C* GESTIONE F05 VIDEO D1
057800100820     C************************************************************
057900100830     C     F05D1         BEGSR
058000100820     C*
058100100820     C                   Move      *ON           $INZD1
058200100820     C*
058300100830     C                   ENDSR
058301100830     C/EJECT
058400100820     C************************************************************
058500100820     C* GESTIONE F08 VIDEO D1
058600100820     C************************************************************
058700100830     C     F08D1         BEGSR
058800100820     C*
058900100820     C                   Move      *ON           $FINE
059000100820     C                   Move      '0'           RETD16
059100100820     C*
059200100830     C                   ENDSR
059201100830     C/EJECT
059300100820     C************************************************************
059400100820     C* GESTIONE F12 VIDEO D1
059500100820     C************************************************************
059600100830     C     F12D1         BEGSR
059700100820     C*
059800100820     C                   Move      *ON           $FINE
059900100820     C                   Move      '2'           RETD16
060000100820     C*
060100100830     C                   ENDSR
060101100830     C/EJECT
060102160429R224 X************************************************************
060103160429 "   X**** GESTIONE F16 VIDEO D1
060104160429 "   X************************************************************
060105160429 "   X**** F16D1         BEGSR
060106160429 "   X****
060107160429 "   X****               DELETE    SDGSO3000
060108160429 "   X****               EXSR      GESAGG
060109160429 "   X****
060110160429 "   X****               ENDSR
060111160429R224 X****/EJECT
060200100820     C************************************************************
060300100820     C* GESTIONE F24 VIDEO D1
060400100820     C************************************************************
060500100830     C     F24D1         BEGSR
060600100820     C*
060700100820     C                   MoveA     $FX1          $FX
060800100820     C                   MoveA     $FC1          $FC
060900100820     C                   MoveA     $FR1          $FR
061000100820     C                   Z-ADD     $ULKD1        $ULK
061100100830     C                   EXSR      RIEKEY
061200100820     C                   Z-ADD     $ULK          $ULKD1            3 0
061300100820     C                   MoveA     $FC           $FC1
061400100820     C                   MoveL     $KEY1         Z1KE1
061500100820     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
061600100820     C                   MoveL     $KEY2         Z1KE2
061700100820     C                   WRITE     FMTZ1
061800100820     C*
061900100830     C                   ENDSR
061901100830     C/EJECT
062000100820     C************************************************************
062100100820     C* CONTROLLO VIDEATA
062200100820     C************************************************************
062300100830     C     CTRD1         BEGSR
062400100820     C*
062500100820     C                   SETOFF                                       99
062600100820     C* non imposto pos. del Cursore
062700100820     C                   Z-ADD     0             H1RIGA
062800100820     C                   Z-ADD     0             H1COLO
062900100820     C*
063000100820     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
063100100820     C*
063200100820     C     OPZD16        IFNE      '04'
063300100830     C*
063301160506R224 C* se chiamata da anagrafica soggetti, mostro il codice soggetto
063302160506 "   C                   if        TpCallD16   = '1'         or
063303160506 "   C                             (TpCallD16   = ' '   and
063304160506 "   C                              TpDatid16   = '1'      )
063306160506 "   C                   EXSR      CtrSogg
063307160506 "   C                   endif
063308160506 "   C                   if        TpCallD16   = '2'         or
063309160506 "   C                             (TpCallD16   = ' '   and
063310160506R224 C                              TpDatid16   = '1'      )
063311100830     C                   EXSR      CtrDtFiVl
063312160506R224 C                   endif
063313160510 "   C                   if        TpCallD16   = '3'         or
063314160510 "   C                             (TpCallD16   = ' '   and
063315160513 "   C                              TpDatid16   = '2'      )
063316160510 "   C                   EXSR      CtrConto
063317160510 "   C                   EXSR      CtrPartita
063318160513 "   C                   EXSR      CtrSys_NrAsInt
063319160510 "   C                   endif
063320160510 "   C*
063321160506 "   C                   EXSR      CtrDest
063322160428 "   C*
063323160429 "   C* reperisco i default se manca tutto
063324160428 "   C                   if        D1Sogg     <> *Blank  and
063325160428 "   C                             D1CogRag    = *Blank  and
063326160428 "   C                             D1Nome      = *Blank  and
063327160428 "   C                             D1Sex       = *Blank  and
063328160428 "   C                             D1DtNasc    = *Zero   and
063329160428 "   C                             D1LocNasc   = *Blank  and
063330160428 "   C                             D1PrvNasc   = *Blank  and
063331160428 "   C                             D1Localit   = *Blank  and
063332160428 "   C                             D1Stato     = *Blank  and
063333170822R224 C                             D1StaFed    = *Blank  and
063334170822R258 C                             D1Prov      = *Blank  and
063335170822R258 C                             D1CAP       = *Blank  and
063336170822R224 C                             D1IndirE    = *Blank
063339160428 "   C                   EXSR      RepDef2
063340160428 "   C                   if        $FlgDef = *on
063341160428 "   C                   EXSR      RIEDEF
063342160428 "   C                   endif
063343160428 "   C                   endif
063344160428R224 C*
063345100830     C                   EXSR      CtrCogRag
063346100831     C                   EXSR      CtrNome
063347100901     C                   EXSR      CtrSex
063348100830     C                   EXSR      CtrDtNasc
063349100830     C                   EXSR      CtrLocNasc
063350100830     C                   EXSR      CtrPrvNasc
063351100830     C                   EXSR      CtrLocalit
063352100830     C                   EXSR      CtrStato
063353100830     C                   EXSR      CtrStaFed
063354100830     C                   EXSR      CtrIndirE
065000100820     C*
071500100820     C*
071600100820     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
071700100820     C     OPZD16        IFEQ      '01'
071800100820     C     OPZD16        OREQ      '03'
071900100820     C* SOLO SE LA DATA FINE VALIDITA' E' CORRETTA
072000100820     C     *IN69         IFEQ      *OFF
072100100820     C     *IN70         ANDEQ     *OFF
072200100830     C                   Eval      SO3Sogg     = D1Sogg
072201100830     C                   Eval      SO3DtFiVl   = WDtFiVl
072202160510R224 X****               Eval      SO3Sys      = H1Sys
072203160510R224 X****               Eval      SO3NrAsInt  = H1NrAsInt
072204160510R224 C                   Eval      SO3Sys      = D1Sys
072205160510 "   C                   Eval      SO3NrAsInt  = D1NrAsInt
072206160428R224 C                   Eval      SO3Dest     = D1Dest
072400160428R224 X**** K04SO301      SETLL     SDGSO301L                              21
072401160428R224 C     K05SO301      SETLL     SDGSO301L                              21
072500100820     C     *IN21         IFEQ      '1'
072600100820     C                   SETON                                        5999
072700100830     C                   ENDIF
072800100830     C                   ENDIF
072900100830     C                   ENDIF
073000100820     C*
073100100830     C                   ENDIF
073200100820     C*
073300100830     C                   ENDSR
073301100830     C/EJECT
073302160428R224 C************************************************************
073303160428 "   C* Controllo soggetto
073304160428 "   C************************************************************
073305160428R224 C     CtrSogg       BEGSR
073306160428     C*
073307160428     C                   Clear                   D1SoggD
073311160428     C*
073312160428     C                   CALLB     'XALLINEA'
073313160428     C                   PARM                    D1Sogg
073314160428     C                   PARM      8             LEN               5 0
073315160428     C                   PARM                    XSCINP
073316160428     C*
073317160428     C                   Eval      WSogg       = D1Sogg
073318160428     C                   MoveL     'DVASOG    '  DVAStrutt
073319160428     C                   Eval      DVALENOUT   = %size(DvaSog)
073320160428     C*
073321160428     C                   EXSR      CallDvaSog
073322160428     C*
073323160428     C                   if        DVAErrore  <> '1'
073324160428     C                   Eval      %subst(DVASOG:1:DVALENOUT)
073325160428     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
073326160428     C*
073327160428     C                   MoveL     DVSDesFil     D1SoggD
073328160428     C                   else
073330160505     C                   Seton                                        6799
073331160428     C                   endif
073334160428     C*
073335160428R224 C                   ENDSR
073336160428R224 C/EJECT
073337160428R224 C************************************************************
073338160428 "   C* Chiamata al driver dei soggetti
073339160428 "   C************************************************************
073340160428R224 C     CallDvaSog    BEGSR
073341160428     C*
073342160428     C                   CALLB     'NDDVASOG'
073343160428     C                   PARM      '1'           DVARic
073344160428     C                   PARM      XScSoc        DVASocieta
073345160428     C                   PARM      *Blank        DVAUnita
073346160428     C                   PARM                    DVAStrutt
073347160428     C                   PARM                    DVADtRif
073348160428     C                   PARM                    DVALenOut
073349160428     C                   PARM      *Blank        DVASNatura
073350160428     C                   PARM      *Blank        DVACodice
073351160428     C                   PARM      *Blank        DVALineaV
073352160428     C                   PARM      *Zero         DVAFiliale
073353160428     C                   PARM      WSogg         DVASogg
073354160428     C                   PARM      *Blank        DVATpInd
073355160428     C                   PARM      *Blank        DVACdInd
073356160428     C                   PARM      *Off          DVAErrore
073357160428     C                   PARM                    DVAOutPut
073358160428     C*
073359160428R224 C                   ENDSR
073360160428R224 C/EJECT
073804100830     C*****************************************************
073805100830     C* Data fine validità
073806100830     C*****************************************************
073807100830     C     CtrDtFiVl     BEGSR
073808100830     C*
073809100830     C                   Clear                   WDtFiVl
073810100830     C*
073811100830     C                   if        D1DtFiVl   <> 0
073815100831     C     *DMY          TEST(D)                 D1DtFiVl               22
073816100831     C*
073817100831     C* data corretta
073818100831     C                   if        *In22       = *Off
073819100831     C     *JobRun       Move      D1DtFiVl      WDtFiVl
073820160505R224 C*
073821160505 "   C* verifico che esista il dato fiscale storicizzato
073822160505 "   C                   Eval      CPISogg     = D1Sogg
073823160505 "   C                   Eval      CPIDtFiVl   = WDtFiVl
073824160505 "   C     K02CPI01      SETLL     ANCPI01L                               21
073825160505 "   C                   if        *IN21      <> '1'
073826160505 "   C                   SETON                                        5499
073827160505R224 C                   ENDIF
073828100831     C                   endif
073829100831     C*
073830100830     C                   endif
073842100830     C*
073843100830     C                   ENDSR
073844100830     C/EJECT
073845160513R224 C************************************************************
073846160513 "   C* Controllo conto
073847160513 "   C************************************************************
073848160513R224 C     CtrConto      BEGSR
073849160513     C*
073850160513     C                   Clear                   D1KscD
073851160513     C*
073852160513     C                   if        D1Kcc      <> 'C'     and
073853160513     C                             D1Kcc      <> 'F'
073854160513     C                   CALLB     'XALLINEA'
073855160513     C                   PARM                    D1Kcc
073856160513     C                   PARM      6             LEN               5 0
073857160513     C                   PARM                    XSCINP
073858160513     C                   endif
073859160513     C*
073860160513     C                   CALLB     'XALLINEA'
073861160513     C                   PARM                    D1Ksc
073862160513     C                   PARM      8             LEN               5 0
073863160513     C                   PARM                    XSCINP
073864160513     C*
073865160513     C* reperisco capoconto se cliente/fornitore
073866160513     C                   if        D1Kcc       = 'C       '     or
073867160513     C                             D1Kcc       = 'F       '
073868160513     C                   CALLB     'XCAPCLIFOR'
073869160513     C                   PARM      XScSoc        XCAPSOCIET        3
073870160513     C     D1Kcc         PARM      D1Kcc         XCAPKCC           6
073871160513     C     D1Ksc         PARM      D1Ksc         XCAPKSC           8
073872160513     C                   endif
073873160513     C*
073875160513     C* Richiamo modulo controllo conto
073876160513     C                   EXSR      CALLMVC002
073877160513     C                   if        $Esito      = *On
073878160513     C                   SETON                                        7099
073879160513     C                   else
073880160513     C                   MoveL     DesBrev002    D1KscD
073881160513     C*
073882160513     C* Controllo se conto gestito a partite
073883160513     C                   if        TpGest002   = '0'
073884160513     C                   SetOn                                        6199
073885160513     C                   endif
073886160513     C*
073887160513     C                   endif
073890160513     C*
073891160513R224 C                   ENDSR
073892160513R224 C/EJECT
073893160513R224 C************************************************************
073894160513 "   C* Controllo conto
073895160513 "   C************************************************************
073896160513R224 C     CallMvc002    BEGSR
073897160513     C*
073898160513     C                   Clear                   ND002DS
073899160513     C                   EVAL      $LenOut = %Size(ND002DS)
073900160513     C                   CallB     'NDMVC002'
073901160513     C                   PARM                    xscsoc
073902160513     C                   PARM                    D1Kcc
073903160513     C                   PARM                    D1Ksc
073904160513     C                   PARM                    WData
073905160513     C                   PARM      *OFF          $GesMsg
073906160513     C                   PARM      *OFF          $Esito
073907160513     C                   PARM      'ND002DS'     $Struttura
073908160513     C                   PARM                    ND002DS
073909160513     C                   PARM                    $LenOut
073910160513     C*
073911160513R224 C                   ENDSR
073912160513R224 C/EJECT
073913160513R224 C************************************************************
073914160513 "   C* Controllo partita
073915160513 "   C************************************************************
073916160513R224 C     CtrPartita    BEGSR
073917160513     C*
073918160513     C                   Eval      WDtPar      = *Loval
073919160513     C*
073920160513     C                   if        D1DtPar    <> 0
073921160513     C     *JobRun       TEST(D)                 D1DtPar                21
073922160513     C                   if        *In21       = *On
073923160513     C                   SetOn                                        6299
073924160513     C                   else
073925160513     C     *JobRun       Move      D1DtPar       WDtPar
073926160513     C                   endif
073927160513     C                   endif
073928160513     C*
073929160513R224 C                   ENDSR
073930160513R224 C/EJECT
073931160428R224 C************************************************************
073932160513 "   C* Recupero il numero assoluto interno della partita
073933160428 "   C************************************************************
073934160513R224 C     CtrSys_NrAsIntBEGSR
073935160428     C*
073936160513     C                   Eval      PPASocieta  = XScSoc
073937160513     C                   Eval      PPACtb      = CtbD16
073938160513     C                   Eval      PPAKcc      = D1Kcc
073939160513     C                   Eval      PPAKsc      = D1Ksc
073940160513     C                   Eval      PPAKsc      = D1Ksc
073941160513     C                   Eval      PPADtPar    = WDtPar
073942160513     C                   Eval      PPANrPar    = D1NrPar
073943160513     C                   Eval      PPASerPar   = D1SerPar
073944160513     C     K07PPA01      CHAIN     NDPPA01L                           21
073945160513     C                   if        *In21       = *Off
073946160513     C                   Eval      D1Sys       = PPASys
073947160513     C                   Eval      D1NrAsInt   = PPANrAsInt
073948160513     C                   else
073949160513     C                   SetOn                                        6399
073950160513     C                   Eval      D1Sys       = 0
073951160513     C                   Eval      D1NrAsInt   = 0
073952160513     C                   endif
073953160513     C*
073954160513R224 C                   ENDSR
073955160513R224 C/EJECT
073956160513R224 C************************************************************
073957160513 "   C* Controllo destinazione
073958160513 "   C************************************************************
073959160513R224 C     CtrDest       BEGSR
073960160513     C*
073961160428     C                   Clear                   D1DestD
073962160428     C*
073963160428     C                   Clear                   SDGHC4DS
073964160428     C*
073965160428     C                   Clear                   SDGTABDS
073966160428     C*
073967160428     C                   if        D1Dest     <> *Blank
073968160428     C*
073969160428     C                   Eval      XTbRic      = '1'
073970160428     C                   Eval      XTbAzi      = XScSoc
073971160428     C                   Eval      XTbKey      = *Zero
073972160428     C                   Eval      XTbCod      = 'HC4'
073973160428     C                   Move      D1Dest        XTbKey
073974160428     C*
073975160428     C                   CALL      'SDGTABDR'
073976160428     C                   PARM                    SDGTABDS
073977160428     C*
073978160428     C                   if        XTbErr     <> '0'
073979160428     C                   SetOn                                        5299
073980160428     C*
073981160428     C                   else
073982160428     C                   MoveL     XTbUni        SDGHC4DS
073983160428     C                   MoveL     DesHC4        D1DestD
073984160428     C*
073985160428     C                   endif
073986160428     C*
073987160428     C                   else
073988160523     C* "Generica"
073989160428     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS1529'
073990160428     C                   EXSR      PrendiMsg
073991160428     C                   if        ERRMSG     <> *ON
073992160428     C                   MoveL     TXTMSG        D1DestD
073993160428     C                   else
073994160428     C                   MoveL     *ALL'?'       D1DestD
073995160428     C                   endif
073996160428     C*
073997160428     C                   endif
073998160428     C*
073999160428R224 C                   ENDSR
074000160428R224 C/EJECT
074001160428R224 C************************************************************
074002160428 "   C* Reperimento testo messaggi
074003160428 "   C************************************************************
074004160428R224 C     PrendiMsg     BEGSR
074005160428     C*
074006160428     C                   CALLB     'XRTVM'
074007160428     C                   PARM                    IDMSG            27
074008160428     C                   PARM                    TXTMSG          100
074009160428     C                   PARM                    ERRMSG            1
074010160428     C*
074011160428R224 C                   ENDSR
074012160428R224 C/EJECT
074013160428R224 C************************************************************
074014160428 "   C* REPERIMENTO CAMPI DI DEFAULT da campi a video
074015160428 "   C************************************************************
074016160428R224 C     REPDEF2       BEGSR
074017160428     C*
074018160428     C                   Select
074019160428     C*----------------------------------
074020160428     C* da ESTENSIONE NOMINATIVI DI PARTITA
074021160428???? C                   When      SysD16     <> 0          or
074022160428???? C                             NrAsIntD16 <> 0
074023160428     C                   Z-Add     SysD16        CPASys
074024160428     C                   Z-Add     NrAsIntD16    CPAnrAsInt
074025160428     C     K02CPA01      CHAIN     NDCPA01L                           21
074026160428     C                   if        *In21       = *Off
074027160428     C                   EVAL      $FlgDef = *on
074028160428     C*
074029160428     C                   Eval      $PartIVA    = CPAPartIVA
074030160428     C                   Eval      $CdFisc     = CPACdFisc
074031160428     C                   Eval      $CogRag     = CPADescr
074032160428     C                   Eval      $Nome       = *Blank
074033160428     C                   Eval      $Sex        = *Blank
074034160428     C                   Eval      $DtNasc     = *Loval
074035160428     C                   Eval      $LocNasc    = *Blank
074036160428     C                   Eval      $PrvNasc    = *Blank
074037160428     C                   Eval      $Stato      = CPAStato
074038160428     C                   Eval      $StaFed     = *Blank
074039160428     C                   Eval      $Localit    = CPALocalit
074040160428     C                   Eval      $IndirE     = CPAIndiriz
074041170822R258 C                   Eval      $CAP        = CPACAP
074042170822R258 C                   Eval      $Prov       = CPAProv
074043160428     C                   endif
074044160428     C*
074045160428     C*----------------------------------
074046160428     C* da STORICIZZAZIONE DATI FISCALI
074047160428     C                   When      D1DtFiVl   <> *Zero
074048160428     C*
074049160428     C* se inserimento,  verifico se esistono i dati aggiuntivi
074050160428     C*    collegati al soggetto
074051160428     C                   EVAL      $RepSO3     = *Off
074052160428     C                   if        OPZD16     =  '01'
074053160428     C                   MoveL     D1Sogg        SO3Sogg
074054160428     C                   MoveL     *Loval        SO3DtFiVl
074055160520     C                   Z-Add     D1Sys         SO3Sys
074056160520     C                   Z-Add     D1NrAsInt     SO3NrAsInt
074057160520     C                   MoveL     D1Dest        SO3Dest
074058160520     C     K05SO301      CHAIN(N)  SDGSO301L                          21
074059160428     C                   if        *In21       = *Off
074060160428     C                   EVAL      $RepSO3     = *On
074061160428     C*
074062160428     C                   Eval      $Nome       = SO3Nome
074063160428     C                   Eval      $Sex        = SO3Sex
074064160428     C                   Eval      $DtNasc     = SO3DtNasc
074065160428     C                   Eval      $LocNasc    = SO3LocNasc
074066160428     C                   Eval      $PrvNasc    = SO3PrvNasc
074067160428     C                   endif
074068160428     C                   endif
074069160428     C*
074070160428     C                   MoveL     D1Sogg        CPISogg
074071160428     C     *JobRun       Move      D1DtFiVl      CPIDtFiVl
074072160428     C     K02CPI01      CHAIN     ANCPI01L                           21
074073160428     C                   if        *In21       = *Off
074074160428     C                   EVAL      $FlgDef = *on
074075160428     C*
074076160428     C                   Eval      $PartIVA    = CPIPartIVA
074077160428     C                   Eval      $CdFisc     = CPICdFisc
074078160428     C                   Eval      $CogRag     = CPIDesSog1
074079160428     C*
074080160428     C                   if        $RepSO3    <> *On
074081160428     C                   Eval      $Nome       = *Blank
074082160428     C                   Eval      $Sex        = *Blank
074083160428     C                   Eval      $DtNasc     = *Loval
074084160428     C                   Eval      $LocNasc    = *Blank
074085160428     C                   Eval      $PrvNasc    = *Blank
074086160428     C*
074087160428     C     D1Sogg        CHAIN     ANSOG01L                           21
074088160428     C                   if        *In21       = *Off
074089160428     C                   Eval      $Sex        = SOGSex
074090160428     C                   Eval      $DtNasc     = SOGDtNasc
074091160428     C                   endif
074092160428     C*
074093160428     C                   endif
074094160428     C*
074095160428     C                   Eval      $Stato      = CPIStato
074096160428     C                   Eval      $StaFed     = *Blank
074097160428     C                   Eval      $Localit    = CPILocalit
074098160428     C                   Eval      $IndirE     = CPIIndriz
074099170822R258 C                   Eval      $CAP        = CPICAP
074100170822R258 C                   Eval      $Prov       = CPIProv
074101160428     C                   endif
074102160428     C*
074103160428     C                   Other
074104160428     C*----------------------------------
074105160428     C* da ANAGRAFICA SOGGETTI STANDARD
074106160428     C     D1Sogg        CHAIN     ANSOG01L                           21
074107160428     C                   if        *In21       = *Off
074108160428     C     D1Sogg        CHAIN     ANIND01L                           21
074109160428     C                   if        *In21       = *Off
074110160428     C                   EVAL      $FlgDef = *on
074111160428     C*
074112160428     C                   Eval      $PartIVA    = SOGPartIVA
074113160428     C                   Eval      $CdFisc     = SOGCdFisc
074114160428     C                   Eval      $CogRag     = SOGDes
074115160428     C                   Eval      $Nome       = *Blank
074116160428     C                   Eval      $Sex        = SOGSex
074117160428     C                   Eval      $DtNasc     = SOGDtNasc
074118160428     C                   Eval      $LocNasc    = *Blank
074119160428     C                   Eval      $PrvNasc    = *Blank
074120160428     C                   Eval      $Stato      = INDStato
074121160428     C                   Eval      $StaFed     = *Blank
074122160428     C                   Eval      $Localit    = INDLocalit
074123160428     C                   Eval      $IndirE     = INDIndriz
074124170822R258 C                   Eval      $CAP        = INDCAP
074125170822R258 C                   Eval      $Prov       = INDProv
074126160428     C*
074127160428     C                   endif
074128160428     C                   endif
074129160428     C*
074130160428     C                   endSl
074131160428     C*
074132160428R224 C                   ENDSR
074133160428R224 C/EJECT
074134100830     C*****************************************************
074135100830     C* Cognome/ragione sociale
074136100830     C*****************************************************
074137100830     C     CtrCogRag     BEGSR
074138100830     C*
074139100830     C                   if        D1CogRag    = *Blank
074140100830     C                   Seton                                        5199
074141100830     C                   endif
074142100830     C*
074143100830     C                   ENDSR
074144100830     C/EJECT
074145100831     C*****************************************************
074146100831     C* Nome
074147100831     C*****************************************************
074148100831     C     CtrNome       BEGSR
074149100831     C*
074150100831     C                   if        D1Nome    = *Blank
074151100901     C************       Seton                                        5299
074152100831     C                   endif
074153100831     C*
074154100831     C                   ENDSR
074155100831     C/EJECT
074156100901     C*****************************************************
074157100901     C* Sesso
074158100901     C*****************************************************
074159100901     C     CtrSex        BEGSR
074160100901     C*
074161100901     C                   Clear                   D1SexD
074162100901     C                   Clear                   X07ValDs
074163100901     C*
074164100901     C                   if        D1Sex      <> *Blank
074165100901     C*
074166100901     C                   Move      '1'           X07Ric
074167100901     C                   Movel     'ANSOG000'    X07TRc
074168100901     C                   Movel     'SOGSEX    '  X07Cam
074169100901     C                   Movel     D1Sex         X07VAL
074170100901     C*
074171100901     C                   Callb     'X07VALR'
074172100901     C                   Parm                    X07ValDs
074173100901     C*
074174100901     C     X07ERR        IFEQ      *Off
074175100901     C                   MOVEL     X07DES        D1SexD
074176100901     C                   else
074177100901     C                   SetOn                                        6099
074178100901     C                   endif
074179100901     C*
074180100901     C                   endif
074181100901     C*
074182100901     C                   ENDSR
074183100901     C/EJECT
074184100830     C*****************************************************
074185100830     C* Data di nascita
074186100830     C*****************************************************
074187100830     C     CtrDtNasc     BEGSR
074188100830     C*
074189100830     C                   Clear                   WDtNasc
074190100830     C*
074191100830     C                   if        D1DtNasc   <> 0
074192100831     C*
074193100830     C                   Select
074194100830     C* formato mm/gg/aaaa
074195100830     C                   When      $DatFmt     = '*MDY'
074196100831     C     *USA          TEST(D)                 D1DtNasc               22
074197100830     C* formato aa/mm/gg
074198100830     C                   When      $DatFmt     = '*YMD'
074199100831     C     *ISO          TEST(D)                 D1DtNasc               22
074200100830     C* formato gg/mm/aaaa
074201100830     C                   When      $DatFmt     = '*DMY'
074202100831     C     *EUR          TEST(D)                 D1DtNasc               22
074203100830     C                   endSl
074204100830     C* data corretta
074205100831     C                   if        *In22       = *Off
074206100830     C                   Select
074207100830     C* formato mm/gg/aaaa
074208100830     C                   When      $DatFmt     = '*MDY'
074209100830     C     *USA          Move      D1DtNasc      WDtNasc
074210100830     C* formato aa/mm/gg
074211100830     C                   When      $DatFmt     = '*YMD'
074212100830     C     *ISO          Move      D1DtNasc      WDtNasc
074213100830     C* formato gg/mm/aaaa
074214100830     C                   When      $DatFmt     = '*DMY'
074215100830     C     *EUR          Move      D1DtNasc      WDtNasc
074216100830     C                   endSl
074217100830     C*
074218100830     C                   else
074219100830     C*
074220100831     C                   SetOn                                        5399
074221100830     C                   endif
074222100830     C*
074223100831     C                   else
074224100831     C*************      SetOn                                        5399
074225100830     C                   endif
074226100830     C*
074227100830     C                   ENDSR
074228100830     C/EJECT
074229100830     C*****************************************************
074230100830     C* Località di  nascita
074231100830     C*****************************************************
074232100830     C     CtrLocNasc    BEGSR
074233100830     C*
074234100831     C                   if        D1LocNasc   = *Blank
074235100831     C************       Seton                                        5499
074236100830     C                   endif
074237100830     C*
074238100830     C                   ENDSR
074239100830     C/EJECT
074240100830     C*****************************************************
074241100830     C* Provincia di nascita
074242100830     C*****************************************************
074243100830     C     CtrPrvNasc    BEGSR
074244100831     C*
074245100831     C                   if        D1PrvNasc  <> *Blank
074246100831     C*
074247100831     C                   Reset                   SDGTABDS
074248100831     C                   Reset                   ANGC17DS
074249100831     C*
074250100831     C                   Move      '1'           XTBRic
074251100831     C                   Move      XSCSOC        XTBAzi
074252100831     C                   Move      *Zero         XTBKey
074253100831     C                   Move      'C17'         XTBCod
074254100831     C                   Move      D1PrvNasc     KeyC17
074255100901     C                   Movel     XScSta        KeyC17
074256100831     C                   Move      KeyC17        XTBKey
074257100831     C*
074258100831     C                   CALL      'SDGTABDR'
074259100831     C                   PARM                    SDGTABDS
074260100831     C*
074261100831     C                   if        XTbErr     <> '0'
074262100831     C                   Seton                                        5599
074263100831     C                   endif
074264100831     C                   endif
074265100830     C*
074266100830     C                   ENDSR
074267100830     C/EJECT
074268100830     C*****************************************************
074269100830     C* Località di residenza
074270100830     C*****************************************************
074271100830     C     CtrLocalit    BEGSR
074272100830     C*
074273100831     C                   if        D1Localit = *Blank
074274100831     C************       Seton                                        5699
074275100831     C                   endif
074276100830     C*
074277100830     C                   ENDSR
074278100830     C/EJECT
074279100830     C*****************************************************
074280100830     C* Stato in tabella G12
074281100830     C*****************************************************
074282100830     C     CtrStato      BEGSR
074283100830     C*
074284100830     C                   Reset                   SDGTABDS
074300100830     C                   Reset                   ANGG12DS
074301100830     C*
074400100830     C                   Move      '1'           XTBRic
074500100830     C                   Move      XSCSOC        XTBAzi
074600100830     C                   Move      *Zero         XTBKey
074700100830     C                   Move      'G12'         XTBCod
074800100830     C                   Move      D1Stato       XTBKey
074801100830     C*
074900100830     C                   CALL      'SDGTABDR'
075000100830     C                   PARM                    SDGTABDS
075100100820     C*
075200100830     C                   if        XTbErr     <> '0'
075300100831     C                   Seton                                        5799
075400100820     C                   endif
075800100820     C*
075900100830     C                   ENDSR
075901100830     C/EJECT
076000100820     C************************************************************
076100100830     C* Stato federale, provincia o contea
076200100820     C************************************************************
076300100830     C     CtrStaFed     BEGSR
076400100820     C*
076402100831     C                   if        D1StaFed  = *Blank
076403100831     C**********         Seton                                        5899
076404100831     C                   endif
076405100830     C*
076406100830     C                   ENDSR
076407100830     C/EJECT
076408100830     C************************************************************
076409100830     C* Indirizzo estero
076410100830     C************************************************************
076411100830     C     CtrIndirE     BEGSR
076412100830     C*
076414100831     C                   if        D1IndirE  = *Blank
076415100831     C***********        Seton                                        5999
076416100831     C                   endif
076417100830     C*
076418100830     C                   ENDSR
076419100830     C/EJECT
076420100830     C************************************************************
076421100830     C* AGGIORNAMENTO
076422100830     C************************************************************
076423100830     C     AggAna        BEGSR
077400100820     C*
077500100820     C                   SELECT
077600100820     C     OPZD16        WHENEQ    '01'
077700100820     C     OPZD16        OREQ      '03'
077800100830     C                   EXSR      RIEANA
077900100820     C                   WRITE     SDGSO3000                            22
078000100830     C* se rcd non scrivibile attivo errore
078100100820     C     *IN22         IFEQ      *ON
078200100820     C                   SETON                                        9599
078300100830     C                   ENDIF
078301100830     C*
078400100820     C     OPZD16        WHENEQ    '02'
078500100830     C                   EXSR      RIEANA
078600100820     C                   UPDATE    SDGSO3000
078601100830     C*
078700100820     C     OPZD16        WHENEQ    '04'
078800100820     C                   DELETE    SDGSO3000
078900100820     C                   ENDSL
079000100820     C*
079100100830     C* richiama pgm User Exit
079200100820     C*
079300100820     C     USRERR        IFEQ      *BLANK
079400100830     C* imposta Chiave univoca  per user exit
079500100820     C                   Move      XSCSOC        XUKSOC
079600100820     C                   Move      XSCDUN        XUKDUN
079700100820     C                   MoveL     NOMPGM        XUKPGM
079800100830     C                   MoveL     SO3Sogg       XUKFL1
079801100830     C                   MoveL     SO3DtFiVl     XUKFL2
079900100830     C                   MoveL     SO3Sys        XUKFL3
080000100830     C                   MoveL     SO3NrAsInt    XUKFL4
080100100820     C                   MoveL     XUKDS         KPJBU
080200100820     C     NOMPGM        CAT       'U':0         USREXT           10
080300100820     C                   CALL      USREXT                               21
080400100820     C                   PARM                    KPJBA
080500100820     C     *IN21         IFEQ      *ON
080600100820     C                   Move      *ON           USRERR            1
080700100830     C                   ENDIF
080800100830     C                   ENDIF
080900100820     C*
081000100830     C                   ENDSR
081001100830     C/EJECT
081100100820     C************************************************************
081200100820     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
081300100820     C************************************************************
081400100830     C     RIEANA        BEGSR
081500100820     C*
081602100831     C                   if        OPZD16      = '01'   or
081603100831     C                             OPZD16      = '03'
081604100831     C*
081605100831     C                   CLEAR                   SDGSO3000
081611100831     C*
081612100831     C* imposto la data e l'utente di immissione
081613100831     C                   Eval      SO3DtImm    = UDateIso
081614100831     C                   Eval      SO3UteImm   = XScCUt
081615100831     C*
081619100831     C                   endif
081620100831     C*
081621100831     C* imposto la data e l'utente di ultima modifica
081622100831     C                   Eval      SO3DtUlMo   = UDateIso
081623100831     C                   Eval      SO3UteUlMo  = XScCUt
081700100820     C*
081800100830     C* imposto i campi del file da quelli video
081900100830     C                   Eval      SO3Sogg     = D1Sogg
082000100830     C                   Eval      SO3DtFiVl   = WDtFiVl
082002160510R224 X****               Eval      SO3Sys      = H1Sys
082003160510R224 X****               Eval      SO3NrAsInt  = H1NrAsInt
082004160510R224 C                   Eval      SO3Sys      = D1Sys
082005160510R224 C                   Eval      SO3NrAsInt  = D1NrAsInt
082100100830     C                   Eval      SO3PartIVA  = D1PartIVA
082200100830     C                   Eval      SO3CdFisc   = D1CdFisc
082300100830     C                   Eval      SO3CogRag   = D1CogRag
082400100830     C                   Eval      SO3Nome     = D1Nome
082401100901     C                   Eval      SO3Sex      = D1Sex
082500100830     C                   Eval      SO3LocNasc  = D1LocNasc
082600100830     C                   Eval      SO3DtNasc   = WDtNasc
082700100830     C                   Eval      SO3PrvNasc  = D1PrvNasc
082800100830     C                   Eval      SO3Localit  = D1Localit
082900100830     C                   Eval      SO3Stato    = D1Stato
083000100830     C                   Eval      SO3StaFed   = D1StaFed
083100100830     C                   Eval      SO3Indire   = D1Indire
083101160428R221 C                   Eval      SO3Dest     = D1Dest
083102170822R258 C                   Eval      SO3CAP      = D1CAP
083103170822R258 C                   Eval      SO3Prov     = D1Prov
083200100820     C*
083300100830     C                   ENDSR
083301100830     C/EJECT
083400100820     C************************************************************
083500100820     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
083600100820     C************************************************************
083700100830     C     GESAGG        BEGSR
083800100820     C*
083900100830     C* segnalo al pgm chiamante l'avvenuta conferma
084000100820     C                   Move      *ON           OPRD16
084100100830     C* segnalo al pgm chiamante l'aver premuto l'F6
084200100820     C                   Move      '0'           RETD16
084300100820     C*
084400100820     C                   SELECT
084500100820     C*
084600100830     C* nel caso di immissione
084700100820     C     OPZD16        WHENEQ    '01'
084800100820     C* ripristino la videata iniziale
084900100820     C                   Move      *ON           $FINE
085000100820     C                   Move      *ON           $INZD1
085100100820     C                   Move      'D1'          $GEST
085200100820     C*
085300100830     C* nel caso di modifiCa
085400100820     C     OPZD16        WHENEQ    '02'
085500100830     C* ripasso i campi che devono essere reimpostati sul sfl
085503160520R224 C                   MoveL     SO3CogRag     CogRagD16
085504160520R224 C                   MoveL     SO3Nome       NomeD16
085600100830     C* e ritorno al pgm chiamante
085700100820     C                   Move      *ON           $FINE
085800100820     C*
085900100820     C* per tutte le altre opzioni
086000100820     C                   OTHER
086100100830     C* ritorno al pgm chiamante
086200100820     C                   Move      *ON           $FINE
086300100820     C                   ENDSL
086400100820     C*
086500100830     C                   ENDSR
086501100830     C/EJECT
086600100820     C************************************************************
086700100820     C* CONTROLLO ABILITAZIONE TASTI
086800100820     C************************************************************
086900100830     C     CTRKEY        BEGSR
087000100820     C*
087100100820     C                   SELECT
087200100820     C     $TASTO        WHENEQ    F01
087300100820     C     $FC(1)        IFEQ      '0'
087400100820     C                   SETON                                        99
087500100820     C                   EndIf
087600100820     C     $TASTO        WHENEQ    F02
087700100820     C     $FC(2)        IFEQ      '0'
087800100820     C                   SETON                                        99
087900100820     C                   EndIf
088000100820     C     $TASTO        WHENEQ    F03
088100100820     C     $FC(3)        IFEQ      '0'
088200100820     C                   SETON                                        99
088300100820     C                   EndIf
088400100820     C     $TASTO        WHENEQ    F04
088500100820     C     $FC(4)        IFEQ      '0'
088600100820     C                   SETON                                        99
088700100820     C                   EndIf
088800100820     C     $TASTO        WHENEQ    F05
088900100820     C     $FC(5)        IFEQ      '0'
089000100820     C                   SETON                                        99
089100100820     C                   EndIf
089200100820     C     $TASTO        WHENEQ    F06
089300100820     C     $FC(6)        IFEQ      '0'
089400100820     C                   SETON                                        99
089500100820     C                   EndIf
089600100820     C     $TASTO        WHENEQ    F07
089700100820     C     $FC(7)        IFEQ      '0'
089800100820     C                   SETON                                        99
089900100820     C                   EndIf
090000100820     C     $TASTO        WHENEQ    F08
090100100820     C     $FC(8)        IFEQ      '0'
090200100820     C                   SETON                                        99
090300100820     C                   EndIf
090400100820     C     $TASTO        WHENEQ    F09
090500100820     C     $FC(09)       IFEQ      '0'
090600100820     C                   SETON                                        99
090700100820     C                   EndIf
090800100820     C     $TASTO        WHENEQ    F10
090900100820     C     $FC(10)       IFEQ      '0'
091000100820     C                   SETON                                        99
091100100820     C                   EndIf
091200100820     C     $TASTO        WHENEQ    F11
091300100820     C     $FC(11)       IFEQ      '0'
091400100820     C                   SETON                                        99
091500100820     C                   EndIf
091600100820     C     $TASTO        WHENEQ    F12
091700100820     C     $FC(12)       IFEQ      '0'
091800100820     C                   SETON                                        99
091900100820     C                   EndIf
092000100820     C     $TASTO        WHENEQ    F13
092100100820     C     $FC(13)       IFEQ      '0'
092200100820     C                   SETON                                        99
092300100820     C                   EndIf
092400100820     C     $TASTO        WHENEQ    F14
092500100820     C     $FC(14)       IFEQ      '0'
092600100820     C                   SETON                                        99
092700100820     C                   EndIf
092800100820     C     $TASTO        WHENEQ    F15
092900100820     C     $FC(15)       IFEQ      '0'
093000100820     C                   SETON                                        99
093100100820     C                   EndIf
093200100820     C     $TASTO        WHENEQ    F16
093300100820     C     $FC(16)       IFEQ      '0'
093400100820     C                   SETON                                        99
093500100820     C                   EndIf
093600100820     C     $TASTO        WHENEQ    F17
093700100820     C     $FC(17)       IFEQ      '0'
093800100820     C                   SETON                                        99
093900100820     C                   EndIf
094000100820     C     $TASTO        WHENEQ    F18
094100100820     C     $FC(18)       IFEQ      '0'
094200100820     C                   SETON                                        99
094300100820     C                   EndIf
094400100820     C     $TASTO        WHENEQ    F19
094500100820     C     $FC(19)       IFEQ      '0'
094600100820     C                   SETON                                        99
094700100820     C                   EndIf
094800100820     C     $TASTO        WHENEQ    F20
094900100820     C     $FC(20)       IFEQ      '0'
095000100820     C                   SETON                                        99
095100100820     C                   EndIf
095200100820     C     $TASTO        WHENEQ    F21
095300100820     C     $FC(21)       IFEQ      '0'
095400100820     C                   SETON                                        99
095500100820     C                   EndIf
095600100820     C     $TASTO        WHENEQ    F22
095700100820     C     $FC(22)       IFEQ      '0'
095800100820     C                   SETON                                        99
095900100820     C                   EndIf
096000100820     C     $TASTO        WHENEQ    F23
096100100820     C     $FC(23)       IFEQ      '0'
096200100820     C                   SETON                                        99
096300100820     C                   EndIf
096400100820     C     $TASTO        WHENEQ    F24
096500100820     C     $FC(24)       IFEQ      '0'
096600100820     C                   SETON                                        99
096700100820     C                   EndIf
096800100820     C                   ENDSL
096900100820     C*
097000100830     C                   ENDSR
097001100830     C/EJECT
097100100820     C******************************************************
097200100820     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
097300100820     C******************************************************
097400100830     C     MSGPGM        BEGSR
097401100830     C*
097402100830     C                   Reset                   SDGMSGDS
097500100820     C*
097600100820     C                   Z-ADD     2             MSGMSG
097700100820     C                   MoveL     'S'           MSGRSP
097800100820     C                   MoveL     'CR'          MSGTPR
097900100820     C                   MoveL     'S'           MSGEMV
098000100820     C                   Z-ADD     10            MSGRGP
098100100820     C                   MoveL     'S'           MSGLCK
098200100820     C                   MoveA     STATUS        STS
098300100820     C                   MoveL     'N'           MSGCNL
098400100820     C                   MoveL     'N'           MSGVID
098500100820     C                   MoveL     'N'           MSGOPE
098600100820     C                   MoveL     'N'           MSGSTP
098601100830     C*
098602100830     C                   Call      'SDGMSGR'
098603100830     C                   parm                    SDGMSGDS
098900100820     C*
099000100830     C                   ENDSR
099001100830     C/EJECT
099100100820     C************************************************************
099200100820     C* OPERAZIONI INIZIALI
099300100820     C************************************************************
099400100830     C     *INZSR        BEGSR
099500100820     C*
099600100831     C     *DMY          Move      Udate         UDateISO
099701100830     C*
099702100830     C* reperisco DATFMT del job
099703100830     C                   CallB     'XDATFMT'
099704100830     C                   Parm                    $DatFmt           4
099800100820     C*
099900100820     C* Reperimento parametri
100000100820     C     *ENTRY        PLIST
100100100820     C                   PARM                    KPJBA
100200100820     C                   PARM                    DSAUT
100300100830     C*
100400100820     C     PARMS         IFLT      2
100500100820     C                   Move      *BLANK        AUTDS
100600100820     C                   ELSE
100700100820     C                   MoveL     DSAUT         AUTDS
100800100820     C                   EndIf
100900100820     C*
101000100820     C* Reperimento tasti di funzione
101100100820     C*
101200100830     C                   EXSR      REPKEY
101300100820     C*
101400100830     C                   ENDSR
101401100830     C/EJECT
101500100820     C************************************************************
101600100820     C* REPERIMENTO TASTI DI FUNZIONE
101700100820     C************************************************************
101800100830     C     REPKEY        BEGSR
101900100820     C                   MoveA     $FM1          $FM
102000100820     C                   MoveA     $FC1          $FC
102100100830     C                   EXSR      REPKKK
102200100820     C                   MoveA     $FX           $FX1
102300100820     C*
102400100830     C                   ENDSR
102401100830     C/EJECT
102500100820     C************************************************************
102600100820     C* REPERIMENTO TASTI DI FUNZIONE
102700100820     C************************************************************
102800100830     C     REPKKK        BEGSR
102900100820     C*
103000100820     C                   CLEAR                   $FX
103100100820     C     1             DO        24            X
103200100820     C*
103300100820     C     $FM(X)        IFNE      *BLANK
103400100820     C     $FC(X)        ANDNE     '0'
103500100820     C*
103600100820     C                   MoveL     $FM(X)        IDMSG
103700100820     C                   CALLB     'XRTVM'
103800100820     C                   PARM                    IDMSG            27
103900100820     C                   PARM                    TXTMSG          100
104000100820     C                   PARM                    ERRMSG            1
104100100820     C*
104200100820     C     ERRMSG        IFNE      *ON
104300100820     C                   MoveL     TXTMSG        $FX(X)
104400100820     C                   ELSE
104500100820     C                   MoveL     *ALL' '       $FX(X)
104600100820     C                   EndIf
104700100820     C*
104800100820     C                   EndIf
104900100820     C*
105000100820     C                   ENDDO
105100100820     C*
105200100830     C                   ENDSR
105201100830     C/EJECT
105300100820     C************************************************************
105400100820     C* INIZIALIZZAZIONE VARIABILI
105500100820     C************************************************************
105600100830     C     INZVAR        BEGSR
105900100820     C*
106000100820     C                   MoveL     KPJBU         SDGSD16DS
106100100820     C*
106200100830     C* Reperimento dati società
106400100820     C                   MoveL     'SOC001'      TIPXSC
106500100820     C                   MoveL     *BLANK        SOCXSC
106600100830     C                   EXSR      REPSOC
106700100820     C     RTNXSC        IFNE      '1'
106800100820     C                   MoveL     XSOCDS        SOC001
106900100820     C                   ELSE
107000100820     C                   RETURN
107100100830     C                   ENDIF
107101100831     C*
107102160429R224 X**** verifico esistenza dei dati aggiuntivi
107103160429R224 X****               EXSR      ChkSDGSO3
107200100820     C*
107300100820     C* Reperimento autorizzazioni
107400100820     C     AUTDS         IFEQ      *BLANK
107500100830     C                   EXSR      RepAut
107600100820     C                   ELSE
107700100830     C                   MoveL     XCADDS        ANGSOGDS
107701100830     C                   ENDIF
107900100820     C*
108000100830     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
108100100820     C* le autorizzazione
108300100830     C                   EXSR      TSTAUT
109500100820     C*
109600100820     C* Definizione dei tasti funzione utilizzabili a pgm
109800100830     C                   EXSR      DEFOPZ
109900100820     C*
110000100820     C* Variabili per gestione videate
110200100820     C                   Move      'D1'          $GEST                          fmt in gestione
110300100820     C                   Move      *OFF          $FINE                          fine pgm
110301100830     C                   MOVE      *OFF          $FlgDef                        fine pgm
110400100820     C                   Move      *ON           $INZD1                         inizializzione D1
110500100820     C                   Move      *BLANK        $LASTG                         fmt di provenienza
110600100820     C                   Move      *BLANK        $LASTV                         ultima emissione
110700100820     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
110800100820     C*
110900100820     C* Variabili appoggio
111100100820     C                   Z-ADD     0             CURR
111200100820     C                   Z-ADD     0             CURC
111511100831     C*
111600100830     C* Valorizzazione campi univoci testate
112000100820     C                   CLEAR                   FMTT1
112100100820     C                   MoveL     KNSIF         NOMSIF
112200100820     C                   MoveL     XSCDSI        NOMDIT
112201100831     C                   MOVEL     KCDAZ         NOMAZN
112300100830     C                   MoveL     DSPGM         NOMPGM           10
112301160503R224 C*
112302160503  "  C* Recupero il titolo con il tipo di comunicazione
112303160503  "  C                   MOVEL     *BLANK        T1TIT
112304160503  "  C*
112305160503  "  C                   Select
112306160516  "  C*
112307160516  "  C* da anagrafica soggetti
112308160503  "  C                   When      TpCallD16   = '1'
112309160503  "  C                   Eval      $Sottit     = Sottit1
112310160516  "  C*
112311160516  "  C* da anagrafica variazioni fiscali
112312160503  "  C                   When      TpCallD16   = '2'
112313160503  "  C                   Eval      $Sottit     = Sottit2
112314160516  "  C*
112315160516  "  C* da estensione nominativi di partita
112316160503  "  C                   When      TpCallD16   = '3'
112317160503  "  C                   Eval      $Sottit     = Sottit3
112318160516  "  C*
112319160516  "  C* da menu, dati anagrafici del soggetto + storicizzati
112320160516  "  C                   When      TpCallD16   = ' '      and
112321160516  "  C                             TpDatiD16   = '1'
112322160516  "  C                   Eval      $Sottit     = Sottit1
112323160516  "  C*
112324160516  "  C* da menu, estensione nominativi di partita
112325160516  "  C                   When      TpCallD16   = ' '      and
112326160516  "  C                             TpDatiD16   = '2'
112327160516  "  C                   Eval      $Sottit     = Sottit3
112328160516  "  C*
112329160516  "  C                   endSl
112330160516  "  C*
112331160503  "  C     $Sottit       IFNE      *BLANK
112332160503  "  C                   MOVEL     $Sottit       IDMSG
112333160503  "  C                   CALLB     'XRTVM'
112334160503  "  C                   PARM                    IDMSG            27
112335160503  "  C                   PARM                    TXTMSG          100
112336160503  "  C                   PARM                    ERRMSG            1
112337160503  "  C                   PARM      *ON           CTRMSG            1            centratura
112338160503  "  C                   PARM      30            LENMSG            3 0          lun output.
112339160503  "  C     ERRMSG        IFNE      *ON
112340160503  "  C                   MOVEL     TXTMSG        T1TIT
112341160503  "  C                   ELSE
112342160503  "  C                   MOVEL     *ALL'?'       T1TIT
112343160503  "  C                   ENDIF
112344160503  "  C                   ENDIF
112345160503R224 C*
112400100830     C* recupero il tipo operazione
112500100820     C                   Z-ADD     1             X
112600100820     C     OPZD16        LOOKUP    $OZ(X)                                 21
112700100820     C     *IN21         IFEQ      *ON
112800100820     C                   MoveL     $ID(X)        IDMSG
112900100820     C                   CALLB     'XRTVM'
113000100820     C                   PARM                    IDMSG            27
113100100820     C                   PARM                    TXTMSG          100
113200100820     C                   PARM                    ERRMSG            1
113300100830     C                   PARM      *Off          CTRMSG            1            Centratura
113400100820     C                   PARM      30            LENMSG            3 0          lun output.
113500100820     C     ERRMSG        IFNE      *ON
113600100820     C                   MoveL     TXTMSG        T1OPE
113700100820     C                   ELSE
113800100820     C                   MoveL     *ALL'?'       T1OPE
113900100830     C                   ENDIF
113901100830     C                   ENDIF
114001100830     C*
114100100830     C* Reperisco e memorizzo default in caso di immissione
114400100831     C*****OPZD16        IFEQ      '01'
114900100830     C                   EXSR      REPDEF
115100100831     C*****              ENDIF
115200100820     C*
115800100830     C* Inizializzazione/Decodifica prima videata
116000100830     C                   EXSR      INZD1
116100100820     C                   Move      *OFF          $INZD1
116101160523R224 C*
116102160523 "   C* se dati anagrafici del soggetto e/o storicizzati
116103160523 "   C                   if        TpDatiD16   = '1'
116104160523 "   C                   EXSR      CtrSogg
116105160523R224 C                   endif
116200100820     C*
116300100830     C                   ENDSR
116301100830     C/EJECT
116808100830     C************************************************************
116809100830     C* REPERIMENTO DATI SOCIETA'
116810100830     C************************************************************
116811100830     C     REPSOC        BEGSR
116812100830     C*
116900100830     C                   CALL      'SDGSOCDR'
117000100820     C                   PARM                    TIPXSC            6
117100100820     C                   PARM                    SOCXSC            3
117200100820     C                   PARM                    CDSXSC            9 0
117300100820     C                   PARM                    MODXSC            3
117400100820     C                   PARM      *BLANKS       RTNXSC            1
117500100820     C                   PARM                    XSOCDS
117600100820     C                   PARM                    KPJBA
117700100820     C*
117800100830     C                   ENDSR
117801100830     C/EJECT
117802160429R224 X************************************************************
117803160429 "   X* Verifico esistenza record dei dati aggiuntivi
117804160429 "   X************************************************************
117805160429 "   X**** ChkSDGSO3     BEGSR
117806160429 "   X****
117807160429 "   X**** verifico se il record esiste già,
117808160429 "   X**** se non esiste
117809160429 "   X**** e sono in modifica cambio in inserimento, se tuttavia
117810160429 "   X**** manca l'autorizzazione all'inserimento, segnalo errore
117811160429 "   X**** Se il record esiste e ho l'autorizzazione, entro in
117812160429 "   X**** modifica, se manca l'autorizz. entro in visualizzaz.
117813160429 "   X**** Se se manca l'autorizzazone a inserire, modificare o
117814160429 "   X**** visualzizzare, non deve essere presente l'opz. 22.
117815160429 "   X**** Se manca l'autorizzazione a cancellare, non vedo il
117816160429 "   X**** tasto F16=Cancellazione
117817160429 "   X****
117818160429 "   X****               Eval      SO3Sogg     = SoggD16
117819160429 "   X****               Eval      SO3DtFiVl   = DtFiVlD16
117820160429 "   X****               Eval      SO3Sys      = SysD16
117821160429 "   X****               Eval      SO3NrAsInt  = NrAsIntD16
117824160429 "   X**** K04SO301      CHAIN(N)  SDGSO301L                          2122
117826160429 "   X****               if        *In21       = *On
117827160429 "   X****               if        OPZD16      = '02'
117828160429 "   X****               Eval      OPZD16      = '01'
117829160429 "   X****               endif
117830160429 "   X****               endif
117831160429 "   X****
117832160429 "   X****               ENDSR
117833160429R224 X****/EJECT
117900100820     C************************************************************
118000100820     C* REPERIMENTO AUTORIZZAZIONI UTENTE
118100100820     C************************************************************
118200100830     C     REPAUT        BEGSR
118300100820     C*
118400100820     C                   CLEAR                   AUTDS
118500100820     C*
118600100820     C                   MoveL     XSCSOC        XCASOC                         SOCIETA
118700100820     C                   MoveL     XSCDUN        XCAUNI                         UNITA
118800100820     C                   Z-ADD     0             XCAGRP                         GRUPPO
118900100820     C                   MoveL     KNMUS         XCAPRF                         PROFILO
119000100820     C                   MoveL     'ANGSOG'      XCAFNC                         FUNZIONE
119100100820     C                   MoveL     *ZERO         XCAVFU                         VARIABILE
119200100820     C                   Move      'CK'          XCATCK                         TIPO CTRL
119300100820     C                   Move      '1'           XCANAU                         RICH.XNOAUT
119400100820     C*
119500100820     C                   CALL      'XCHKAUT'
119600100820     C                   PARM                    AUTDS
119700100820     C*
119800100820     C* errori/non abilitazione
119900100820     C     XCARTN        IFNE      0
120000100820     C                   Move      '2'           ErrD16
120100100820     C                   Move      *ON           $FINE
120200100830     C                   EXSR      ENDPGM
120300100820     C                   ELSE
120400100830     C                   MoveL     XCADDS        ANGSOGDS
120500100830     C                   ENDIF
120600100820     C*
120700100830     C                   ENDSR
120701100830     C/EJECT
120800100820     C************************************************************
120900100820     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
121000100820     C************************************************************
121100100830     C     TSTAUT        BEGSR
121200100820     C*
121300100820     C                   SELECT
121400100820     C*
121500100830     C* se richiamato in immissione
121600100820     C     OPZD16        WHENEQ    '01'
121700100820     C* ma l'utente non è abilitato
121800100820     C     IMMSOG        ANDEQ     *OFF
121900100830     C* esco da pgm avvertendolo
122000100820     C                   Move      '2'           ERRD16
122100100820     C                   Move      *ON           $FINE
122200100830     C                   EXSR      ENDPGM
122300100820     C*
122400100830     C* se richiamato in modifica
122500100820     C     OPZD16        WHENEQ    '02'
122600100820     C* ma l'utente non è abilitato
122700100820     C     MANSOG        ANDEQ     *OFF
122800100820     C* testo se l'utente è abilitato almeno alla visualizzazione
122900100820     C     VISSOG        IFEQ      *ON
123000100820     C                   Move      '05'          OPZD16
123100100820     C                   ELSE
123200100830     C* altrimenti esco da pgm avvertendolo
123300100820     C                   Move      '2'           ERRD16
123400100820     C                   Move      *ON           $FINE
123500100830     C                   EXSR      ENDPGM
123600100830     C                   ENDIF
123700100820     C*
123800100830     C* se richiamato in copia
123900100820     C     OPZD16        WHENEQ    '03'
124000100820     C* ma l'utente non è abilitato
124100100820     C     IMMSOG        ANDEQ     *OFF
124200100830     C* esco da pgm avvertendolo
124300100820     C                   Move      '2'           ERRD16
124400100820     C                   Move      *ON           $FINE
124500100830     C                   EXSR      ENDPGM
124600100820     C*
124700100830     C* se richiamato in annullamento
124800100820     C     OPZD16        WHENEQ    '04'
124900100820     C* ma l'utente non è abilitato
125000100820     C     ANNSOG        ANDEQ     *OFF
125100100830     C* esco da pgm avvertendolo
125200100820     C                   Move      '2'           ERRD16
125300100820     C                   Move      *ON           $FINE
125400100830     C                   EXSR      ENDPGM
125500100820     C*
125600100830     C* se richiamato in visualizzazione
125700100820     C     OPZD16        WHENEQ    '05'
125800100820     C* ma l'utente non è abilitato
125900100820     C     VISSOG        ANDEQ     *OFF
126000100830     C* esco da pgm avvertendolo
126100100820     C                   Move      '2'           ERRD16
126200100820     C                   Move      *ON           $FINE
126300100830     C                   EXSR      ENDPGM
126400100820     C*
126500100820     C                   ENDSL
126600100820     C*
126700100830     C                   ENDSR
126701100830     C/EJECT
128529100831     C************************************************************
128530100831     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
128531100831     C************************************************************
128532100831     C     DEFOPZ        BEGSR
128533100831     C*
128600100831     C* Questa routine deve valorizzare la schiera $FC1/$FR1
128700100831     C* con S o N a seconda della utilizzabilità o meno
128800100831     C* del tasto funzione correlato
128900100820     C*
129000100831     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
129100100831     C* di compilazione
129200100820     C                   RESET                   $FC1
129300100820     C                   RESET                   $FR1
129400100820     C*
129500100831     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
129600100831     C* invalido alcune opzioni
129700100820     C                   SELECT
129701100831     C* Se pgm richiamato in insedrimento disattivo :
129702100831     C     OPZD16        WHENEQ    '01'
129712160429R224 X**** F16=Cancellazione
129713160429 "   X****               Z-ADD     16            X
129714160429R224 X****               Move      '0'           $FC1(X)
129715100831     C*
129800100831     C* Se pgm richiamato in interrogazione disattivo :
129900100820     C     OPZD16        WHENEQ    '05'
130000100831     C* F4=Ricerca
130100100820     C                   Z-ADD     4             X
130200100820     C                   Move      '0'           $FC1(X)
130300100820     C* F5=Ripristino
130400100820     C                   Z-ADD     5             X
130500100820     C                   Move      '0'           $FC1(X)
130600100820     C* F6=Conferma
130700100820     C                   Z-ADD     06            X
130800100820     C                   Move      '0'           $FC1(X)
130801160429R224 X**** F16=Cancellazione
130802160429 "   X****               Z-ADD     16            X
130803160429R224 X****               Move      '0'           $FC1(X)
130804100831     C*
130900100831     C* Se pgm richiamato in annullamento disattivo :
131000100820     C     OPZD16        WHENEQ    '04'
131100100831     C* F4=Ricerca
131200100820     C                   Z-ADD     4             X
131300100820     C                   Move      '0'           $FC1(X)
131400100820     C* F5=Ripristino
131500100820     C                   Z-ADD     5             X
131600100820     C                   Move      '0'           $FC1(X)
131700100820     C*
131800100820     C                   OTHER
131900100820     C*
132000100820     C                   ENDSL
132001100831     C*
132002100831     C*
132003160429R224 X**** se cancellazione non abilitata, disabilito il
132004160429 "   X**** tasto funzione F16
132005160429 "   X****               if        ANNSOG      = *OFF
132006160429 "   X**** F16=Cancellazione
132007160429 "   X****               Z-ADD     16            X
132008160429 "   X****               Move      '0'           $FC1(X)
132009160429R224 X****               endif
132100100820     C*
132200100830     C                   ENDSR
132201100830     C/EJECT
132300100820     C************************************************************
132400100820     C* REPERIMENTO CAMPI DI DEFAULT
132500100820     C************************************************************
132600100830     C     REPDEF        BEGSR
132702100830     C*
132703100830     C                   Select
132704100831     C*----------------------------------
132705100831     C* da ESTENSIONE NOMINATIVI DI PARTITA
132706100831     C                   When      SysD16     <> 0          or
132707100830     C                             NrAsIntD16 <> 0
132708100830     C                   Z-Add     SysD16        CPASys
132709100830     C                   Z-Add     NrAsIntD16    CPAnrAsInt
132710100830     C     K02CPA01      CHAIN     NDCPA01L                           21
132711100830     C                   if        *In21       = *Off
132712100830     C                   EVAL      $FlgDef = *on
132713100830     C*
132714100830     C                   Eval      $PartIVA    = CPAPartIVA
132715100830     C                   Eval      $CdFisc     = CPACdFisc
132716100830     C                   Eval      $CogRag     = CPADescr
132717100830     C                   Eval      $Nome       = *Blank
132718100901     C                   Eval      $Sex        = *Blank
132719100830     C                   Eval      $DtNasc     = *Loval
132720100830     C                   Eval      $LocNasc    = *Blank
132721100830     C                   Eval      $PrvNasc    = *Blank
132722100830     C                   Eval      $Stato      = CPAStato
132723100830     C                   Eval      $StaFed     = *Blank
132724100830     C                   Eval      $Localit    = CPALocalit
132725100830     C                   Eval      $IndirE     = CPAIndiriz
132726170822R258 C                   Eval      $CAP        = CPACAP
132727170822R258 C                   Eval      $Prov       = CPAProv
132728100830     C                   endif
132729100830     C*
132730100831     C*----------------------------------
132731100831     C* da STORICIZZAZIONE DATI FISCALI
132732100830     C                   When      DtFiVlD16  <> *Loval
132733100831     C*
132734100831     C* se inserimento,  verifico se esistono i dati aggiuntivi
132735100831     C*    collegati al soggetto
132736100903     C                   EVAL      $RepSO3     = *Off
132737100903     C                   if        OPZD16     =  '01'
132738100903     C                   MoveL     SoggD16       SO3Sogg
132739100903     C                   MoveL     *Loval        SO3DtFiVl
132740100903     C                   Z-Add     0             SO3Sys
132741100903     C                   Z-Add     0             SO3nrAsInt
132742160428R224 C                   MoveL     DestD16       SO3Dest
132743160428R224 X**** K04SO301      CHAIN(N)  SDGSO301L                          21
132744160428R224 C     K05SO301      CHAIN(N)  SDGSO301L                          21
132745100903     C                   if        *In21       = *Off
132746100903     C                   EVAL      $RepSO3     = *On
132747100903     C*
132748100903     C                   Eval      $Nome       = SO3Nome
132749100903     C                   Eval      $Sex        = SO3Sex
132750100903     C                   Eval      $DtNasc     = SO3DtNasc
132752100903     C                   Eval      $LocNasc    = SO3LocNasc
132753100903     C                   Eval      $PrvNasc    = SO3PrvNasc
132758100903     C                   endif
132760100903     C                   endif
132761100831     C*
132763100830     C                   MoveL     SoggD16       CPISogg
132764100830     C                   MoveL     DtFiVlD16     CPIDtFiVl
132765100830     C     K02CPI01      CHAIN     ANCPI01L                           21
132766100830     C                   if        *In21       = *Off
132767100830     C                   EVAL      $FlgDef = *on
132768100830     C*
132769100830     C                   Eval      $PartIVA    = CPIPartIVA
132770100830     C                   Eval      $CdFisc     = CPICdFisc
132771100830     C                   Eval      $CogRag     = CPIDesSog1
132774100831     C*
132775100903     C                   if        $RepSO3    <> *On
132776100903     C                   Eval      $Nome       = *Blank
132777100901     C                   Eval      $Sex        = *Blank
132778100831     C                   Eval      $DtNasc     = *Loval
132779100903     C                   Eval      $LocNasc    = *Blank
132780100903     C                   Eval      $PrvNasc    = *Blank
132781100903     C*
132782100831     C     SoggD16       CHAIN     ANSOG01L                           21
132783100831     C                   if        *In21       = *Off
132784100901     C                   Eval      $Sex        = SOGSex
132785100831     C                   Eval      $DtNasc     = SOGDtNasc
132786100831     C                   endif
132787100831     C*
132789100903     C                   endif
132790100903     C*
132793100830     C                   Eval      $Stato      = CPIStato
132794100830     C                   Eval      $StaFed     = *Blank
132795100830     C                   Eval      $Localit    = CPILocalit
132796100830     C                   Eval      $IndirE     = CPIIndriz
132797170822R258 C                   Eval      $CAP        = CPICAP
132798170822R258 C                   Eval      $Prov       = CPIProv
132799100830     C                   endif
132800100831     C*
132801100830     C                   Other
132802100831     C*----------------------------------
132803100831     C* da ANAGRAFICA SOGGETTI STANDARD
132804100830     C     SoggD16       CHAIN     ANSOG01L                           21
132805100830     C                   if        *In21       = *Off
132806100830     C     SoggD16       CHAIN     ANIND01L                           21
132807100830     C                   if        *In21       = *Off
132808100830     C                   EVAL      $FlgDef = *on
132809100830     C*
132810100830     C                   Eval      $PartIVA    = SOGPartIVA
132811100830     C                   Eval      $CdFisc     = SOGCdFisc
132812100830     C                   Eval      $CogRag     = SOGDes
132813100830     C                   Eval      $Nome       = *Blank
132814100901     C                   Eval      $Sex        = SOGSex
132815100830     C                   Eval      $DtNasc     = SOGDtNasc
132816100830     C                   Eval      $LocNasc    = *Blank
132817100830     C                   Eval      $PrvNasc    = *Blank
132818100830     C                   Eval      $Stato      = INDStato
132819100830     C                   Eval      $StaFed     = *Blank
132820100830     C                   Eval      $Localit    = INDLocalit
132821100830     C                   Eval      $IndirE     = INDIndriz
132822170822R258 C                   Eval      $CAP        = INDCAP
132823170822R258 C                   Eval      $Prov       = INDProv
132824100830     C*
132825100830     C                   endif
132826100830     C                   endif
132827100830     C*
132828100830     C                   endSl
132829100830     C*
132830100830     C                   ENDSR
132831100830     C/EJECT
132900100820     C************************************************************
133000100820     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
133100100820     C************************************************************
133200100830     C     DEFCAM        BEGSR
133300100820     C*
133400100820     C* klist
133500100820     C*
133600100820     C* SDGSO301L
133700100830     C     K04SO301      KLIST
133800100830     C                   KFLD                    SO3Sogg
133801100830     C                   KFLD                    SO3DtFiVL
133802100830     C                   KFLD                    SO3Sys
133803100830     C                   KFLD                    SO3NrAsint
133804160428R224 C*
133805160428 "   C     K05SO301      KLIST
133806160428 "   C                   KFLD                    SO3Sogg
133807160428 "   C                   KFLD                    SO3DtFiVL
133808160428 "   C                   KFLD                    SO3Sys
133809160428 "   C                   KFLD                    SO3NrAsInt
133810160428R224 C                   KFLD                    SO3Dest
133820100830     C*
133821100830     C* ANCPI01L
133822100830     C     K02CPI01      KLIST
133823100831     C                   KFLD                    CPISogg
133824100831     C                   KFLD                    CPIDtFiVL
133825100830     C*
133826100830     C* NDCPA01L
133827100830     C     K02CPA01      KLIST
133828100831     C                   KFLD                    CPASys
133829100831     C                   KFLD                    CPANrAsint
133830160510R224 C*
133831160510 "   C* NDPPA04L
133832160510 "   C     K02PPA04      KLIST
133833160510 "   C                   KFLD                    PPASys
133834160513 "   C                   KFLD                    PPANrAsInt
133835160513 "   C* NDPPA01L
133836160513 "   C     K07PPA01      KLIST
133837160513 "   C                   KFLD                    PPASocieta
133838160513 "   C                   KFLD                    PPACTB
133839160513 "   C                   KFLD                    PPAKCC
133840160513 "   C                   KFLD                    PPAKSC
133841160513 "   C                   KFLD                    PPADTPAR
133842160513 "   C                   KFLD                    PPANRPAR
133843160513R224 C                   KFLD                    PPASERPAR
134000100820     C*
134100100820     C* Variabili per gestione videate
134300100820     C                   Move      *BLANK        $GEST             2            fmt in gestione
134400100820     C                   Move      *BLANK        $FINE             1            fine pgm
134500100820     C                   Move      *BLANK        $INZD1            1            inizializzione D1
134600100820     C                   Move      *BLANK        $LASTG            2            fmt di provenienza
134700100820     C                   Move      *BLANK        $LASTV            2            ultima emissione
134800100820     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
135600100820     C*
135700100820     C                   Z-ADD     0             CURR              2 0
135800100820     C                   Z-ADD     0             CURC              2 0
135900100820     C                   Move      *ZEROS        WIN              99
136000100820     C*
136100100820     C* IndiCi
136200100820     C                   Z-ADD     0             X                 3 0
136300100820     C                   Z-ADD     0             Y                 3 0
136400100820     C*
136500100830     C                   ENDSR
136501100830     C/EJECT
136600100820     C************************************************************
136700100820     C* RIEMPIMENTO TASTI FUNZIONE
136800100820     C************************************************************
136900100830     C     RIEKEY        BEGSR
137000100820     C*
137100100820     C* Input :
137200100820     C* - $FX = sChiera Con tutti i testi disponibili
137300100820     C* - $FC = sChiere Con i flag di validità assoCiati ai testi
137400100820     C* - $FR = sChiere Con i flag di REVERSE IMAGE
137500100820     C* - $ULK = ultimo testo CariCato
137600100820     C* - $L01 = Lunghezza prima riga
137700100820     C* - $L02 = Lunghezza seConda riga
137800100820     C* Output :
137900100820     C* - $ULK
138000100820     C* - $KEY1= 1° riga di tasti funzione
138100100820     C* - $KEY2= 2° riga di tasti funzione
138200100820     C*
138300100820     C                   CALLB     'XKEYMSG'
138400100820     C* input
138500100820     C                   PARM                    $FX
138600100820     C                   PARM                    $FC
138700100820     C                   PARM                    $FR
138800100820     C                   PARM                    $ULK              3 0
138900100820     C* output
139000100820     C                   PARM                    $KEY1            79
139100100820     C                   PARM                    $KEY2            79
139200100820     C* input
139300100820     C                   PARM      79            $L01              3 0
139400100820     C                   PARM      79            $L02              3 0
139500100820     C*
139600100820     C                   PARM                    $ALLFUNCT       480
139700100820     C*
139800100830     C                   ENDSR
139801100830     C/EJECT
139900100820     C************************************************************
139901160429R224 C* tolta abilitazione al tasto F16=Annullamento
140000100820**
14000110083001SDGMSG    *LIBL     SDC0002
14020010082002PROMSG    *LIBL     COS0002
14030010082003PROMSG    *LIBL     COS0003
14040010082004PROMSG    *LIBL     COS0004
14050010082005PROMSG    *LIBL     COS0005
140600100820** TASTI DI FUNZIONE  D1
140700100820PROMSG    *LIBL     KEY0001  01
140800100820PROMSG    *LIBL     KEY0002  02
140900100820PROMSG    *LIBL     KEY0003  03
141000100820PROMSG    *LIBL     KEY0004  04
141100100820PROMSG    *LIBL     KEY0005  05
141200100820PROMSG    *LIBL     KEY0006  06
141300100820PROMSG    *LIBL     KEY0007  07
141400100820PROMSG    *LIBL     KEY0008  08
141500100820PROMSG    *LIBL     KEY0009  09
141600100820PROMSG    *LIBL     KEY0010  10
141700100820PROMSG    *LIBL     KEY0011  11
141800100820PROMSG    *LIBL     KEY0012  12
141900100820PROMSG    *LIBL     KEY0013  13
142000100820PROMSG    *LIBL     KEY0014  14
142100100820PROMSG    *LIBL     KEY0015  15
142200100831SDGMSG    *LIBL     SDK0009  16
142300100820PROMSG    *LIBL     KEY0017  17
142400100820PROMSG    *LIBL     KEY0018  18
142500100820PROMSG    *LIBL     KEY0019  19
142600100820PROMSG    *LIBL     KEY0020  20
142700100820PROMSG    *LIBL     KEY0021  21
142800100820PROMSG    *LIBL     KEY0022  22
142900100820PROMSG    *LIBL     KEY0023  23
143000100820PROMSG    *LIBL     KEY0024  24
143100100820**  ABILITAZIONE D1
1432001008200N  01
1433001008200N  02
1434001008201N  03
1435001008201N  04
1436001008201N  05
1437001008201N  06
1438001008200N  07
1439001008201N  08
1440001008200N  09
1441001008200N  10
1442001008200N  11
1443001008201N  12
1444001008200N  13
1445001008200N  14
1446001008200N  15
1447001604290N  16                  ## R224
1448001008200N  17
1449001008200N  18
1450001008200N  19
1451001008200N  20
1452001008200N  21
1453001008200N  22
1454001008200N  23   Impostare a '1' se si utilizzano opzioni
1455001008201N  24   LasCiare sempre impostata a '1' il valore della riga 24
