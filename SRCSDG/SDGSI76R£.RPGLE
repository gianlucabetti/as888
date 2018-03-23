000100130909     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200130909     H*actgrp(QILE) PARMS CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500170803      *                Spesometro trimestrale XML
000600130909      *                 Dati di dettaglio - manutenzione
000700000000      *           ---------------------------------------
000800000000      *
000900170803R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
001000170803      *
001100000000     F*----------------------------------------------------*
001200170804     F* Spesometro - Movimenti estratti
001300170804     FSDGCST00F UF A E             DISK
001400170804     F*------------
001500130909     F* Spesometro - Movimenti estratti
001600170803     F* Key: Soc/TpCom/AA/TT/MM/CF/PartIVA/CdFisc    UNIQUE
001700170804     FSDGCST05L IF   E           K DISK
001800170804     F                                     RENAME(SDGCST000:SDGCST05)
001900000000     F*------------
002000130919     F* Spesometro - Movimenti estratti
002100170803     F* Key: TpCom/TpRec/Sys/NrAsReg/NrRigaM/NrRig   UNIQUE
002200170803     FSDGCST01L IF   E           K DISK
002300170803     F                                     RENAME(SDGCST000:SDGCST01)
002400170807     F*------------
002500170807     F* Spesometro - Catalogo file
002600170807     F* Key: Soc/IdFile
002700170807     FSDGCSF04L IF   E           K DISK
002800000000     F*-------------
002900000000     FANRIV01L  IF   E           K DISK
003000130909     F*-------------
003100130909     F* libri
003200130909     FANDLI01L  IF   E           K DISK
003300130909     F*------------
003400170803     FSDGSI76D  CF   E             WORKSTN
003500170803     F                                     RENAME(SI76D1:FMTD1)
003600170803     F                                     RENAME(SI76T1:FMTT1)
003700170803     F                                     RENAME(SI76Z1:FMTZ1)
003800000000     F* eventuali schermi supplementari
003900170803     F                                     RENAME(SI76D2:FMTD2)
004000170803     F                                     RENAME(SI76D3:FMTD3)
004100000000     F                                     INFDS(DSFMT)
004200000000     F*------------
004300000000     D*----------------------------------------------------*
004400000000     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004500000000     D $ID             S             27    DIM(5) ALT($OZ)
004600000000     D* Schiere per la gestione dei tasti di funzione
004700000000     D $FM             S             27    DIM(24)
004800000000     D $FX             S             50    DIM(24)
004900000000     D $FC             S              1    DIM(24)
005000000000     D $FR             S              1    DIM(24)
005100000000     D* Video D1
005200000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005300000000     D $FX1            S             50    DIM(24) ALT($FM1)
005400000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005500000000     D $FR1            S              1    DIM(24) ALT($FC1)
005600000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005700000000     D* funzione personalizzati
005800000000     D $PGF1           S             10    DIM(25)
005900000000     D*----------------------------------------------------*
006000000000     D $D1L01          S              3  0
006100000000     D $D1L02          S              3  0
006200000000     D*-------------
006300000000     D* Passaggio Parametri
006400130909     D KPJBA         E DS
006500000000     D*-------------
006600000000     D* DS Esterna gestione messaggi
006700000000     D SDGMSGDS      E DS
006800000000     D  STS          E                     EXTFLD(MSGSTS)
006900000000     D                                     DIM(333)
007000000000     D  JBA          E                     EXTFLD(MSGJBA)
007100000000     D                                     DIM(502)
007200000000     D*-------------
007300000000     D* Reperimento Autorizzazioni
007400000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
007500000000     D*-------------
007600000000     D* Maschera dati autorizzazioni
007700000000     D CONCIVDS      E DS
007800000000     D*-------------
007900000000     D* Reperimento nome PGM
008000000000     D STATUS         SDS           333
008100000000     D  DSPGM            *PROC
008200130909     D  PARMS            *PARMS
008300000000     D*-------------
008400000000     D* Dati di ambiente ottenuti da SDGSOCDR
008500000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
008600000000     D*-------------
008700000000     D* DS Interna per dati di output di SDGSOCDR
008800000000     D XSOCDS          DS          1000
008900000000     D*-------------
009000000000     D* Passaggio Parametri al PGM di manutenzione
009100170803     D SDGSI76DS     E DS
009200170807     D*-------------
009300170807     D* Passaggio Parametri al PGM di estrazione dati
009400170807     D SDGSI82DS     E DS
009500170807     D  ERRI82       E                     INZ('0')
009600130925     D*-------------
009700130925     D* Passaggio Parametri al PGM capoconti
009800130925     D ANA200DS      E DS
009900000000     D  CMT200       E                     INZ('0')
010000000000     D  RET200       E                     INZ('0')
010100000000     D  OPR200       E                     INZ('0')
010200000000     D  ERR200       E                     INZ('0')
010300000000     D  FSC200       E                     INZ('S')
010400000000     D*-------------
010500000000     D* Ricerca sottoconti
010600000000     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
010700000000     D*-------------
010800000000     D* Passaggio Parametri al PGM clienti
010900000000     D ANA300DS      E DS
011000000000     D  CMT300       E                     INZ('0')
011100000000     D  RET300       E                     INZ('0')
011200000000     D  OPR300       E                     INZ('0')
011300000000     D  ERR300       E                     INZ('0')
011400000000     D  FSC300       E                     INZ('S')
011500000000     D*-------------
011600000000     D* Passaggio Parametri al PGM fornitori
011700000000     D ANA400DS      E DS
011800000000     D  CMT400       E                     INZ('0')
011900000000     D  RET400       E                     INZ('0')
012000000000     D  OPR400       E                     INZ('0')
012100000000     D  ERR400       E                     INZ('0')
012200000000     D  FSC400       E                     INZ('S')
012300000000     D*-------------
012400000000     D* Controllo conti
012500000000     D ND002DS       E DS                  INZ
012600000000     D*-------------
012700000000     D* Definizioni parametri standard chiamate moduli
012800000000     D Esito           S              1
012900000000     D GesMsg          S              1
013000000000     D LenOut          S              9B 0
013100000000     D Data            S             10
013200000000     D StrutturaO      S             10
013300000000     D*-------------
013400000000     D* Parametri per ricerca codice ANATB00F
013500000000     D XTABDS        E DS                  inz
013600000000     D*-------------
013700000000     D* Richiamo a SDGTABDR
013800000000     D SDGTABDS      E DS                  inz
013900000000     D*-------------
014000000000     D* Definizioni controllo causale
014100000000     D Anope00f      e ds                  inz
014200000000     D Refresh         S              1
014300000000     D Uso             S              1
014400000000     D Output          S           4000
014500000000     D Causale         S              4
014600000000     D*-------------
014700000000     D* Ricerca causale
014800000000     D ANA070DS      E DS                  INZ
014900000000     D  RET070       E                     INZ('0')
015000000000     D  OPR070       E                     INZ('0')
015100000000     D  CMT070       E                     INZ('0')
015200000000     D  ERR070       E                     INZ('0')
015300000000     D  FSC070       E                     INZ('S')
015400000000     D*-------------
015500170803     D* Parametri modulo SDSPETRIM
015600170803     D SDGPA12DS     E DS
015700000000     D*-------------
015800000000     D* Ricerca riferimenti e aliquote
015900130909     D ANA064DS      E DS                  INZ
016000000000     D  Ret064       E                     Inz('0')
016100000000     D  Opr064       E                     Inz('0')
016200000000     D  Err064       E                     Inz('0')
016300000000     D*-------------
016400000000     D* ricerca soggetti
016500000000     D ANA040DS      E DS                  INZ
016600000000     D*-------------
016700000000     D* DS esterna per dati soggetti
016800000000     D DVASOG        E DS                  INZ
016900000000     D*-------------
017000000000     D* Definizioni parametri driver anagrafici
017100000000     D Dvaric          S              1
017200000000     D DvaSocieta      S              3
017300000000     D DvaUnita        S              8
017400000000     D DvaStrutt       S             10
017500000000     D DvaDtRif        S               D
017600000000     D DvaLenOut       S              9B 0
017700000000     D DvaSnatura      S              1
017800000000     D DvaCodice       S              8
017900000000     D DvaLineav       S              3
018000000000     D DvaFiliale      S              3  0
018100000000     D DvaSogg         S              8
018200000000     D DvaTpInd        S              2
018300000000     D DvaCdInd        S              3
018400000000     D DvaErrore       S              1
018500000000     D DvaOutput       S           4000
018600000000     D*-------------
018700000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
018800000000     D* alla funzione personalizzata
018900170803     D $SaveDS         S                   like(SDGSI76DS)
019000000000     D*-------------
019100000000     D* Ds per ricerca legende
019200130909     D ANA964DS      E DS                  INZ
019300000000     D*-------------
019400000000     D* Controlli validità per valore di una legenda
019500000000     D X07VALDS      E DS                  Inz
019600000000     D*-------------
019700000000     D DSFMT           DS
019800000000     D  $TASTO               369    369
019900000000     D  NRG                  370    370
020000000000     D  NCL                  371    371
020100000000     D* riga cursore window
020200000000     D  NRgWin               382    382
020300000000     D* colonna cursore window
020400000000     D  NClWin               383    383
020500000000     D*-------------
020600000000     D* DS per UserExit
020700000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
020800000000     D*-------------
020900000000     D* posizione cursore
021000000000     D CURSOR          DS
021100000000     D  RIRI                   1      2B 0 INZ
021200000000     D  R$$                    2      2
021300000000     D  COCO                   3      4B 0 INZ
021400000000     D  C$$                    4      4
021500000000     D*-------------
021600000000     D* Costanti
021700000000     D*-------------
021800000000     D* Tasti di funzione
021900000000     D F01             C                   CONST(X'31')
022000000000     D F02             C                   CONST(X'32')
022100000000     D F03             C                   CONST(X'33')
022200000000     D F04             C                   CONST(X'34')
022300000000     D F05             C                   CONST(X'35')
022400000000     D F06             C                   CONST(X'36')
022500000000     D F07             C                   CONST(X'37')
022600000000     D F08             C                   CONST(X'38')
022700000000     D F09             C                   CONST(X'39')
022800000000     D F10             C                   CONST(X'3A')
022900000000     D F11             C                   CONST(X'3B')
023000000000     D F12             C                   CONST(X'3C')
023100000000     D F13             C                   CONST(X'B1')
023200000000     D F14             C                   CONST(X'B2')
023300000000     D F15             C                   CONST(X'B3')
023400000000     D F16             C                   CONST(X'B4')
023500000000     D F17             C                   CONST(X'B5')
023600000000     D F18             C                   CONST(X'B6')
023700000000     D F19             C                   CONST(X'B7')
023800000000     D F20             C                   CONST(X'B8')
023900000000     D F21             C                   CONST(X'B9')
024000000000     D F22             C                   CONST(X'BA')
024100000000     D F23             C                   CONST(X'BB')
024200000000     D F24             C                   CONST(X'BC')
024300000000     D ENTER           C                   CONST(X'F1')
024400000000     D ROLDWN          C                   CONST(X'F4')
024500000000     D ROLLUP          C                   CONST(X'F5')
024600000000     D*-------------
024700000000     D* Sottotitolo di testata
024800170803     D SOTTIT          C                   CONST('PROMSG    *LIBL-
024900170803     D                                          COS2504')
025000170803     D*-------------
025100170803     D* Sottotitolo di testata
025200170803     D* "Dettaglio"
025300170803     D SOTTITT1        C                   CONST('PROMSG    *LIBL-
025400170803     D                                          COS0648')
025500000000     D*-------------
025600000000     D* Variabili per gestione videate
025700000000     D $Gest           S              2A
025800000000     D $Fine           S              1A
025900000000     D $InzD1          S              1A
026000000000     D $LastG          S              2A
026100000000     D $LastV          S              2A
026200000000     D $FlgDef         S              1A
026300000000     D*-------------
026400000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
026500000000     D CurR            S              3S 0
026600000000     D CurC            S              3S 0
026700000000     D Win             S             99A
026800000000     D*-------------
026900000000     D* Variabili appoggio specifiche della singola anagrafica
027000000000     D*  Indici
027100000000     D X               S              5S 0
027200000000     D Y               S              5S 0
027300000000     D* Indice della schiera tasti funzione
027400000000     D PosFun          S              2  0
027500170803     D*-------------
027600170803     D* Ricerca libri IVA
027700170803     D AnA062DS      E DS                  Inz
027800170803     D  Ret062       E                     Inz('0')
027900170803     D  Opr062       E                     Inz('0')
028000170803     D  Err062       E                     Inz('0')
028100170803     D*-------------
028200170803     D* Ricerca associazione libro-riferimento IVA
028300170803     D ANA068DS      E DS                  INZ
028400170803     D*-------------
028500170803     D* Campi di lavoro
028600170803     D WData           S               D
028700170803     D WIn16           S              1
028800130926     D WIn17           S              1
028900130923     D UDateISO        S               D
029000000000     D WSogg           S                   Like(DVSSogg)
029100000000     D WDtAlfa         S             10A
029200170804     D WDtDoc          S                   Like(CSTDtDoc  )
029300170804     D WDtReg          S                   Like(CSTDtReg  )
029400170804     D WDtpar          S                   Like(CSTDtPar  )
029500170804     D WDtAnnot        S                   Like(CSTDtAnnot)
029600170803     D WAliq           S                   Like(CSTAliq   )
029700170803     D WRifIVA         S                   Like(CSTRifIVA )
029800000000     D WRifIvaOk       S              1
029900170803     D WKcc            S                   Like(CSTKcc )
030000170803     D WKsc            S                   Like(CSTKsc )
030100000000     D*------
030200000000     D $Esito          S              1
030300000000     D $Prenot         S              1
030400170807     D*------
030500170807     D $Sys            S                    Like(CSTSys    )
030600170807     D $NrAsReg        S                    Like(CSTNrAsReg)
030700170807     D $NrRigaM        S                    Like(CSTNrRigaM)
030800170807     D $NrRiga         S                    Like(CSTNrRiga )
030900170810     D*-------
031000170810     D CallRicors      S              1A   INZ('0')
031100170810     D*-------------
031200170810     D* campo di lavoro per emissione window di avvertimento
031300170810     D WXMSGInz        S              1A   INZ('1')
031400000000     C*----------------------------------------------------*
031500000000     C*                MAIN LINE PROGRAM
031600000000     C*----------------------------------------------------*
031700000000     C*
031800000000     C* inizializzazione variabili
031900130909     C                   EXSR      INZVAR
032000000000     C*
032100130909     C     $FINE         DOWEQ     *Off
032200000000     C     $GEST         CASEQ     'D1'          GESD1
032300000000     C     $GEST         CASEQ     'D2'          GESD2
032400130923     C     $GEST         CASEQ     'D3'          GESD3
032500000000     C                   ENDCS
032600000000     C                   ENDDO
032700000000     C*
032800000000     C* fine programma
032900130909     C                   EXSR      ENDPGM
033000000000     C/EJECT
033100000000     C************************************************************
033200000000     C* Fine Programma
033300000000     C************************************************************
033400000000     C     ENDPGM        BEGSR
033500000000     C*
033600000000     C* se non esco per errore
033700170803    >C     ERRI76        IFEQ      '0'
033800000000     C*
033900000000     C* se il COMMIT non è demandato al pgm chiamante
034000170803    >C     CMTI76        IFEQ      '0'
034100000000     C*
034200000000     C* test se esistono transazioni attive ed eventualmente cosa fare
034300000000     C                   Z-ADD     0             EXECOD
034400130909     C                   CALL      'XRCMTI'
034500130909     C                   PARM                    EXECOD            1 0
034600000000     C*
034700000000     C                   SELECT
034800000000     C* Annulla richiesta
034900000000     C     EXECOD        WHENEQ    1
035000130909     C                   MoveL     *Off          $FINE
035100130909     C                   MoveL     *HIVAL        $LASTV
035200000000     C* Conferma uscita
035300000000     C     EXECOD        WHENEQ    2
035400000000     C*                  ROLBK
035500000000     C*
035600000000     C                   ENDSL
035700000000     C*
035800000000     C                   ENDIF
035900000000     C*
036000000000     C                   ENDIF
036100000000     C*
036200000000     C* se ho confermato l'uscita proseguo
036300000000     C     $FINE         IFEQ      *ON
036400000000     C*
036500000000     C* disallocazione rcd nel caso di modifica/annullamento
036600170803    >C     OPZI76        IFEQ      '02'
036700170803    >C     OPZI76        OREQ      '04'
036800170804    >C                   UNLOCK    SDGCST00F                            22
036900000000     C                   ENDIF
037000000000     C*
037100130909     C                   SetOn                                            RT
037200000000     C                   RETURN
037300000000     C*
037400000000     C                   ENDIF
037500000000     C*
037600000000     C                   ENDSR
037700000000     C/EJECT
037800000000     C************************************************************
037900000000     C* RICERCHE
038000000000     C************************************************************
038100000000     C     SEARCH        BEGSR
038200000000     C*
038300000000     C* determino Riga/Colonna del cursore
038400000000     C                   MOVE      NRG           R$$
038500000000     C                   MOVE      NCL           C$$
038600000000     C                   Z-ADD     RIRI          CURR
038700000000     C                   Z-ADD     COCO          CURC
038800000000     C*
038900000000     C                   SELECT
039000170807     C*
039100170807     C* tipo record
039200170807     C     H1NMFL        WHENEQ    'D1TPREC'
039300170807     C                   EXSR      F4_TpRec
039400170807     C*
039500170807     C* tipo documento
039600170807     C     H1NMFL        WHENEQ    'D1TPDOC  '
039700170807     C                   EXSR      F4_TpDoc
039800170807     C*
039900170807     C* natura operazione
040000170807     C     H1NMFL        WHENEQ    'D1NATOPE '
040100170807     C                   EXSR      F4_NatOpe
040200170807     C*
040300170807     C* motivo di esclusione
040400170807     C     H1NMFL        WHENEQ    'D1MOTESCL'
040500170807     C                   EXSR      F4_MotEscl
040600170807     C*
040700170807     C* esigibilità
040800170807     C     H1NMFL        WHENEQ    'D1ESIGIB '
040900170807     C                   EXSR      F4_Esigib
041000000000     C*
041100000000     C*  Soggetto
041200000000     C     H1NMFL        WHENEQ    'D1SOGG'
041300130909     C                   EXSR      F4_Sogg
041400130911     C*
041500130911     C*  Tipo natura giuridica
041600170804     C     H1NMFL        WHENEQ    'D1TPN1'
041700170804     C                   EXSR      F4_TPN1
041800000000     C*
041900000000     C* capoconto
042000000000     C     H1NMFL        WHENEQ    'D1KCC'
042100130909     C                   EXSR      F4_Kcc
042200000000     C*
042300000000     C* cliente
042400000000     C     H1NMFL        WHENEQ    'D1KSC'
042500130911    >C     H1CF          ANDEQ     'C'
042600130909     C                   EXSR      F4_Cliente
042700000000     C*
042800000000     C*  fornitore
042900000000     C     H1NMFL        WHENEQ    'D1KSC'
043000130911    >C     H1CF          ANDEQ     'F'
043100130909     C                   EXSR      F4_Fornitore
043200130909     C*
043300130909     C* sottoconto
043400130909     C     H1NMFL        WHENEQ    'D1KSC'
043500130909     C                   EXSR      F4_Ksc
043600000000     C*
043700000000     C* causale beni/servizi
043800000000     C     H1NMFL        WHENEQ    'D1CAUSALE'
043900130909     C                   EXSR      F4_Causale
044000000000     C*
044100000000     C* riferimenti IVA
044200000000     C     H1NMFL        WHENEQ    'D1ALIQ'
044300000000     C     H1NMFL        OREQ      'D1RIFIVA'
044400130909     C                   EXSR      F4_AliqRifIVA
044500130909     C*
044600130909     C* FMTD1 - Tipo registro IVA
044700130909     C     H1NMFL        WHENEQ    'D1TPREG'
044800130909     C                   EXSR      F4_TpReg
044900130909     C*
045000130909     C* FMTD1 - Libro  IVA
045100130909     C     H1NMFL        WHENEQ    'D1LIBRO'
045200130909     C                   EXSR      F4_Libro
045300000000     C*
045400130909     C* serie registrazione
045500000000     C     H1NMFL        WHENEQ    'D1SERREG'
045600130909     C                   EXSR      F4_Serie
045700130909     C*
045800130909     C* origine
045900130909     C     H1NMFL        WHENEQ    'D1ORIGINE'
046000130909     C                   EXSR      F4_Origine
046100000000     C*
046200000000     C* FMTD2 - Tipo annullamento
046300130909     C     H1NMFL        WHENEQ    'D1TPANN'
046400130909     C                   EXSR      F4_TpAnn
046500170807     C*
046600170807     C     H1NMFL        WHENEQ    'D1FILEO'
046700170807     C     H1NMFL        OREQ      'D1IDFILEO'
046800170807     C     H1NMFL        OREQ      'D1FILER'
046900170807     C     H1NMFL        OREQ      'D1IDFILER'
047000170807     C     H1NMFL        OREQ      'D1FILEA'
047100170807     C     H1NMFL        OREQ      'D1IDFILEA'
047200170810     C* controllo chiamata ricorsiva
047300170810     C                   Eval      $Pgm       = 'SDGSI82R'
047400170810     C                   EXSR      CHEKPGM
047500170810     C*
047600170810     C                   if        CallRicors <> *ON
047700170807     C                   EXSR      F4_File
047800170810     C                   endif
047900000000     C*
048000000000     C* ricerca non abilitata in questa posizione
048100000000     C                   OTHER
048200130909     C                   SetOn                                        9899
048300000000     C*
048400000000     C                   ENDSL
048500000000     C*
048600000000     C* imposto pos. del cursore
048700000000     C                   Z-ADD     CURR          H1RIGA
048800000000     C                   Z-ADD     CURC          H1COLO
048900000000     C*
049000000000     C                   MOVE      *HIVAL        $LASTV
049100000000     C*
049200000000     C                   ENDSR
049300000000     C/EJECT
049400170807     C************************************************************
049500170807     C* Ricerca tipo record
049600170807     C************************************************************
049700170807     C     F4_TpRec      BEGSR
049800170807     C*
049900170807     C                   Reset                   ANA964DS
050000170807     C*
050100170807     C                   MoveL     '01'          Opz964
050200170807     C                   MoveL     'SDGCST000'   Trc964
050300170807     C                   MoveL     'CSTTPREC  '  Cam964
050400170807     C*
050500170807     C                   MoveL     ANA964DS      KPJBU
050600170807     C                   CALL      'ANA964R'
050700170807     C                   PARM                    KPJBA
050800170807     C                   MoveL     KPJBU         ANA964DS
050900170807     C*
051000170807     C                   if        Ret964     <> '2'
051100170807     C                   MoveL     Val964        D1TpRec
051200170807     C                   endif
051300170807     C*
051400170807     C                   ENDSR
051500170807     C/EJECT
051600170807     C************************************************************
051700170807     C* Ricerca tipo documento
051800170807     C************************************************************
051900170807     C     F4_TpDoc      BEGSR
052000170807     C*
052100170807     C                   Reset                   ANA964DS
052200170807     C*
052300170807     C                   MoveL     '01'          Opz964
052400170807     C                   MoveL     'SDGCST000'   Trc964
052500170807     C                   MoveL     'CSTTPDOC  '  Cam964
052600170807     C*
052700170807     C                   MoveL     ANA964DS      KPJBU
052800170807     C                   CALL      'ANA964R'
052900170807     C                   PARM                    KPJBA
053000170807     C                   MoveL     KPJBU         ANA964DS
053100170807     C*
053200170807     C                   if        Ret964     <> '2'
053300170807     C                   MoveL     Val964        D1TpDoc
053400170807     C                   endif
053500170807     C*
053600170807     C                   ENDSR
053700170807     C/EJECT
053800170807     C************************************************************
053900170807     C* Ricerca natura dell'operazione
054000170807     C************************************************************
054100170807     C     F4_NatOpe     BEGSR
054200170807     C*
054300170807     C                   Reset                   ANA964DS
054400170807     C*
054500170807     C                   MoveL     '01'          Opz964
054600170807     C                   MoveL     'SDGCST000'   Trc964
054700170807     C                   MoveL     'CSTNATOPE '  Cam964
054800170807     C*
054900170807     C                   MoveL     ANA964DS      KPJBU
055000170807     C                   CALL      'ANA964R'
055100170807     C                   PARM                    KPJBA
055200170807     C                   MoveL     KPJBU         ANA964DS
055300170807     C*
055400170807     C                   if        Ret964     <> '2'
055500170807     C                   MoveL     Val964        D1NatOpe
055600170807     C                   endif
055700170807     C*
055800170807     C                   ENDSR
055900170807     C/EJECT
056000170807     C************************************************************
056100170807     C* Ricerca motivo esclusione (NATURA)
056200170807     C************************************************************
056300170807     C     F4_MotEscl    BEGSR
056400170807     C*
056500170807     C                   Reset                   ANA964DS
056600170807     C*
056700170807     C                   MoveL     '01'          Opz964
056800170807     C                   MoveL     'SDGCST000'   Trc964
056900170807     C                   MoveL     'CSTMOTESCL'  Cam964
057000170807     C                   MoveL     *On           All964
057100170807     C*
057200170807     C                   MoveL     ANA964DS      KPJBU
057300170807     C                   CALL      'ANA964R'
057400170807     C                   PARM                    KPJBA
057500170807     C                   MoveL     KPJBU         ANA964DS
057600170807     C*
057700170807     C                   if        Ret964     <> '2'
057800170807     C                   MoveL     Val964        D1MotEscl
057900170807     C                   endif
058000170807     C*
058100170807     C                   ENDSR
058200170807     C/EJECT
058300170807     C************************************************************
058400170807     C* Ricerca esibilità
058500170807     C************************************************************
058600170807     C     F4_Esigib     BEGSR
058700170807     C*
058800170807     C                   Reset                   ANA964DS
058900170807     C*
059000170807     C                   MoveL     '01'          Opz964
059100170807     C                   MoveL     'SDGCST000'   Trc964
059200170807     C                   MoveL     'CSTESIGIB '  Cam964
059300170807     C*
059400170807     C                   MoveL     ANA964DS      KPJBU
059500170807     C                   CALL      'ANA964R'
059600170807     C                   PARM                    KPJBA
059700170807     C                   MoveL     KPJBU         ANA964DS
059800170807     C*
059900170807     C                   if        Ret964     <> '2'
060000170807     C                   MoveL     Val964        D1Esigib
060100170807     C                   endif
060200170807     C*
060300170807     C                   ENDSR
060400170807     C/EJECT
060500000000     C************************************************************
060600000000     C* Ricerca codice soggetto
060700000000     C************************************************************
060800000000     C     F4_Sogg       BEGSR
060900000000     C*
061000130909     C                   Reset                   ANA040DS
061100000000     C*
061200130926     C                   MoveL     '01'          OPZ040
061300130909     C                   MoveL     D1Sogg        Cod040
061400130909     C                   MoveL     ANA040DS      KPJBU
061500130909     C                   CALL      'ANA040R'
061600130909     C                   PARM                    KPJBA
061700000000     C*
061800130909     C                   MoveL     KPJBU         ANA040DS
061900000000     C*
062000000000     C                   if        Ret040     <> '2'
062100130909     C                   MoveL     Cod040        D1Sogg
062200000000     C                   endif
062300000000     C*
062400000000     C                   ENDSR
062500000000     C/EJECT
062600130911     C************************************************************
062700130911     C* Ricerca tipo natura giuridica
062800130911     C************************************************************
062900170804     C     F4_TpN1       BEGSR
063000130911     C*
063100130911     C                   Reset                   ANA964DS
063200130911     C*
063300130911     C                   MoveL     '01'          Opz964
063400170803     C                   MoveL     'SDGCST000'   Trc964
063500170804     C                   MoveL     'CSTTPN1   '  Cam964
063600130911     C*
063700130911     C                   MoveL     ANA964DS      KPJBU
063800130911     C                   CALL      'ANA964R'
063900130911     C                   PARM                    KPJBA
064000130911     C                   MoveL     KPJBU         ANA964DS
064100130911     C*
064200130911     C                   if        Ret964     <> '2'
064300170804     C                   MoveL     Val964        D1TpN1
064400130911     C                   endif
064500130911     C*
064600130911     C                   ENDSR
064700130911     C/EJECT
064800000000     C************************************************************
064900000000     C* Ricerca capoconto
065000000000     C************************************************************
065100000000     C     F4_Kcc        BEGSR
065200000000     C*
065300130909     C                   Reset                   ANA200DS
065400000000     C*
065500170803     C                   if        OPZI76      = '05' or
065600170803     C                             OPZI76      = '04'
065700130909     C                   MoveL     '02'          OPZ200
065800000000     C                   else
065900130909     C                   MoveL     '01'          OPZ200
066000000000     C                   endif
066100130909     C                   MoveL     D1Kcc         KCC200
066200130909     C                   MoveL     ANA200DS      KPJBU
066300130909     C                   CALL      'ANA200R'                            21
066400130909     C                   PARM                    KPJBA
066500130909     C                   MoveL     KPJBU         ANA200DS
066600000000     C*
066700000000     C     KCC200        IFNE      *BLANK
066800000000     C                   MOVE      KCC200        D1Kcc
066900000000     C                   ENDIF
067000000000     C*
067100000000     C                   ENDSR
067200000000     C/EJECT
067300000000     C************************************************************
067400000000     C* Ricerca cliente
067500000000     C************************************************************
067600000000     C     F4_Cliente    BEGSR
067700000000     C*
067800130909    >C                   Reset                   ANA300DS
067900000000     C*
068000170803     C                   if        OPZI76      = '05' or
068100170803     C                             OPZI76      = '04'
068200130909    >C                   MoveL     '02'          OPZ300
068300000000     C                   else
068400130909    >C                   MoveL     '01'          OPZ300
068500000000     C                   endif
068600130909    >C                   MoveL     D1Ksc         KSC300
068700130909    >C                   MoveL     D1Sogg        SOG300
068800130909    >C                   MoveL     ANA300DS      KPJBU
068900000000     C*
069000130909    >C                   CALL      'ANA300R'                            21
069100130909     C                   PARM                    KPJBA
069200000000     C*
069300130909    >C                   MoveL     KPJBU         ANA300DS
069400000000     C*
0695000000002   >C     KSC300        IFNE      *BLANK
069600000000    >C                   MOVE      KCC300        D1Kcc
069700000000    >C                   MOVE      KSC300        D1Ksc
0698000000002-  >C                   ENDIF
069900000000     C*
070000000000     C                   ENDSR
070100000000     C/EJECT
070200000000     C************************************************************
070300000000     C* Ricerca fornitore
070400000000     C************************************************************
070500000000     C     F4_Fornitore  BEGSR
070600000000     C*
070700130909    >C                   Reset                   ANA400DS
070800000000     C*
070900170803     C                   if        OPZI76      = '05' or
071000170803     C                             OPZI76      = '04'
071100130909    >C                   MoveL     '02'          OPZ400
071200000000     C                   else
071300130909    >C                   MoveL     '01'          OPZ400
071400000000     C                   endif
071500130909    >C                   MoveL     D1Ksc         KSC400
071600130909    >C                   MoveL     D1Sogg        SOG400
071700000000     C*
071800130909    >C                   MoveL     ANA400DS      KPJBU
071900000000     C*
072000130909    >C                   CALL      'ANA400R'                            21
072100130909     C                   PARM                    KPJBA
072200000000     C*
072300130909    >C                   MoveL     KPJBU         ANA400DS
072400000000     C*
0725000000002   >C     KSC400        IFNE      *BLANK
072600000000    >C                   MOVE      KCC400        D1Kcc
072700000000    >C                   MOVE      KSC400        D1Ksc
0728000000002-  >C                   ENDIF
072900000000     C*
073000000000     C                   ENDSR
073100000000     C/EJECT
073200130909     C************************************************************
073300130909     C* Ricerca conto
073400130909     C************************************************************
073500130909     C     F4_Ksc        BEGSR
073600130909     C*
073700130909     C                   Reset                   A201DS
073800130909     C                   MOVE      XSCSOC        SOC201
073900130909     C                   MOVE      D1KCC         KCC201
074000130909     C                   MOVE      D1KsC         KSC201
074100170803     C                   if        OPZI76      = '05' or
074200170803     C                             OPZI76      = '04'
074300130909     C                   MoveL     '02'          OPZ201
074400130909     C                   else
074500130909     C                   MoveL     '01'          OPZ201
074600130909     C                   endif
074700130909     C                   MoveL     A201DS        KPJBU
074800130909     C                   CALL      'ANA201R'
074900130909     C                   PARM                    KPJBA
075000130909     C                   MoveL     KPJBU         A201DS
075100130909     C*
075200130909     C     KSC201        IFNE      *BLANK
075300130909     C                   MoveL     KCC201        D1KCC
075400130909     C                   MoveL     KSC201        D1KSC
075500130909     C                   ENDIF
075600130909     C*
075700130909     C                   ENDSR
075800130909     C/EJECT
075900000000     C************************************************************
076000000000     C* Ricerca codice causale beni/servizi
076100000000     C************************************************************
076200000000     C     F4_Causale    BEGSR
076300000000     C*
076400130909     C                   Reset                   ANA070DS
076500000000     C*
076600170803     C                   if        OPZI76      = '05' or
076700170803     C                             OPZI76      = '04'
076800000000     C                   Move      '02'          Opz070
076900000000     C                   else
077000000000     C                   Move      '01'          Opz070
077100000000     C                   endif
077200000000     C                   MOVE      '1'           Tpc070
077300130909     C                   CLEAR                   KPJBU
077400130909     C                   MoveL     ANA070DS      KPJBU
077500000000     C*
077600130909     C                   CALL      'ANA070R'
077700130909     C                   PARM                    KPJBA
077800000000     C*
077900130909     C                   MoveL     KPJBU         ANA070DS
078000000000     C*
078100000000     C     Cau070        IFNE      *BLANK
078200130909     C                   MoveL     Cau070        D1Causale
078300000000     C                   ENDIF
078400000000     C*
078500000000     C                   ENDSR
078600000000      /EJECT
078700000000     C************************************************************
078800000000     C* Ricerca aliquota/Riferimento IVA
078900000000     C************************************************************
079000000000     C     F4_AliqRifIVA BEGSR
079100000000     C*
079200130909     C                   Reset                   ANA064DS
079300000000     C*
079400170803     C                   if        OPZI76      = '05' or
079500170803     C                             OPZI76      = '04'
079600000000     C                   Move      '02'          OPZ064
079700000000     C                   else
079800000000     C                   Move      '01'          OPZ064
079900000000     C                   endif
080000000000     C************       Move      XscSoc        SOC064
080100000000     C*
080200000000     C                   Z-Add     D1Aliq        Ali064
080300130909     C                   MoveL     D1RifIVA      RfI064
080400000000     C*
080500170804     C                   if        H1CF        = 'F'
080600130909     C                   MoveL     '1'           TpR064
080700000000     C                   else
080800130909     C                   MoveL     '2'           TpR064
080900000000     C                   endif
081000000000     C*
081100130909     C                   MoveL     ANA064DS      KPJBU
081200000000     C*
081300130909     C                   CALL      'ANA064R'
081400130909     C                   PARM                    KPJBA
081500000000     C*
081600130909     C                   MoveL     KPJBU         ANA064DS
081700000000     C*
081800000000     C                   if        Ali064     <> 0           or
081900000000     C                             RfI064     <> *Blank
082000000000     C*
082100000000     C                   Z-Add     Ali064        D1Aliq
082200130909     C                   MoveL     RfI064        D1RifIVA
082300000000     C*
082400000000     C                   endif
082500000000     C*
082600000000     C                   ENDSR
082700000000      /EJECT
082800130909     C************************************************************
082900130909     C* Ricerca tipo registro IVA
083000130909     C************************************************************
083100130909     C     F4_TpReg      BEGSR
083200130909     C*
083300130909     C                   Reset                   ANA964DS
083400111010     C*
083500130909     C                   MoveL     '01'          Opz964
083600130909     C                   MoveL     'ANRIV000 '   Trc964
083700130909     C                   MoveL     'RIVTPREG'    Cam964
083800111010     C*
083900130909     C                   MoveL     ANA964DS      KPJBU
084000130909     C                   CALL      'ANA964R'
084100130909     C                   PARM                    KPJBA
084200130909     C                   MoveL     KPJBU         ANA964DS
084300111010     C*
084400111010     C                   if        Ret964     <> '2'
084500130909     C                   MoveL     Val964        D1TpReg
084600111010     C                   endif
084700111010     C*
084800130909     C                   ENDSR
084900130909     C/EJECT
085000130909     C************************************************************
085100130909     C* Ricerca libro  IVA
085200130909     C************************************************************
085300130909     C     F4_Libro      BEGSR
085400130909     C*
085500130909     C                   if        D1TpReg     = *Blank      or
085600130909     C                             (D1Aliq     = 0       and
085700130909     C                              D1RifIVA   = *Blank     )
085800130909     C                   EXSR      F4_LibroIVA
085900130909     C                   else
086000130909     C                   EXSR      F4_Assoc
086100130909     C                   endif
086200130909     C*
086300130909     C                   ENDSR
086400130909     C/EJECT
086500130909     C************************************************************
086600130909     C* Ricerca libro IVA
086700130909     C************************************************************
086800130909     C     F4_LibroIVA   BEGSR
086900111010     C*
087000130909     C                   Reset                   ANA062DS
087100111010     C*
087200111010     C                   Move      '01'          OPZ062
087300130909     C                   MoveL     D1TpReg       TpR062
087400130909     C                   MoveL     D1Libro       Lib062
087500111010     C*
087600130909     C                   MoveL     ANA062DS      KPJBU
087700111010     C*
087800130909     C                   CALL      'ANA062R'
087900130909     C                   PARM                    KPJBA
088000111010     C*
088100130909     C                   MoveL     KPJBU         ANA062DS
088200111010     C*
088300111010     C                   if        Ret062     <> '2'
088400130909     C                   MoveL     TpR062        D1TpReg
088500130909     C                   MoveL     Lib062        D1Libro
088600111010     C                   endif
088700111010     C*
088800130909     C                   ENDSR
088900130909     C/EJECT
089000130909     C************************************************************
089100130909     C* Ricerca associazione libro-riferimento IVA
089200130909     C************************************************************
089300130909     C     F4_Assoc      BEGSR
089400111010     C*
089500130909     C                   Reset                   ANA068DS
089600111010     C*
089700111010     C                   MOVE      '01'          OPZ068
089800111010     C                   MOVE      XscSoc        SOC068
089900111010     C                   MOVE      D1TpReg       TPR068
090000111010     C                   MOVE      D1Libro       LIB068
090100111010     C                   MOVE      D1Aliq        ALI068
090200111010     C                   MOVE      D1RifIVA      RIF068
090300130909     C                   MoveL     ANA068DS      KPJBU
090400111010     C*
090500130909     C                   CALL      'ANA068R'
090600130909     C                   PARM                    KPJBA
090700111010     C*
090800130909     C                   MoveL     KPJBU         ANA068DS
090900111010     C*
091000111010     C                   IF        Ret068     <> '2'
091100111010     C*
091200111010     C                   Z-Add     ALI068        D1Aliq
091300111010     C                   MOVE      RIF068        D1RifIVA
091400111010     C                   MOVE      TpR068        D1TpReg
091500111010     C                   MOVE      Lib068        D1Libro
091600111010     C*
091700111010     C                   ENDIF
091800111010     C*
091900111010     C                   ENDSR
092000111010     C/EJECT
092100130909     C************************************************************
092200130909     C* Serie numeratore
092300130909     C************************************************************
092400130909     C     F4_Serie      BEGSR
092500130909     C*
092600130909     C                   Reset                   XTabDs
092700130909     C*
092800170803     C                   if        OPZI76      = '05' or
092900170803     C                             OPZI76      = '04'
093000130909     C                   Move      '02'          XTaOpz
093100130909     C                   else
093200130909     C                   Move      '01'          XTaOpz
093300130909     C                   endif
093400130909     C                   Move      XScSoc        XTaAzi
093500130909     C                   Move      XSCLIN        XTaLin
093600130909     C                   Z-Add     CURR          XTARIG
093700130909     C                   Z-Add     CURC          XTACOL
093800130909     C*
093900130909     C                   CLEAR                   KPJBU
094000130909     C                   MoveL     XTabDs        KPJBU
094100130909     C*
094200130909     C                   CALL      'ANGG13R'
094300130909     C                   PARM                    KPJBA
094400130909     C*
094500130909     C                   MoveL     KPJBU         XTabDs
094600130909     C*
094700130909     C     XTaKey        IFNE      *Blank
094800130909     C*
094900130909     C                   Select
095000130909     C     H1NMFL        WHENEQ    'D1SERDOC'
095100130909     C                   Move      XTaKey        D1SerDoc
095200130909     C     H1NMFL        WHENEQ    'D1SERREG'
095300130909     C                   Move      XTaKey        D1SerReg
095400130909     C     H1NMFL        WHENEQ    'D1SERPAR'
095500130909     C                   Move      XTaKey        D1SerPar
095600130909     C                   endSl
095700130909     C*
095800130909     C                   ENDIF
095900130909     C*
096000130909     C                   ENDSR
096100130909     C/EJECT
096200130909     C************************************************************
096300130909     C* Origine
096400130909     C************************************************************
096500130909     C     F4_Origine    BEGSR
096600130909     C*
096700130909     C                   Reset                   ANA964DS
096800130909     C*
096900130909     C                   MoveL     '01'          Opz964
097000170803     C                   MoveL     'SDGCST000'   Trc964
097100170803     C                   MoveL     'CSTORIGINE'  Cam964
097200130909     C                   MoveL     *Off          All964
097300130909     C*
097400130909     C                   MoveL     ANA964DS      KPJBU
097500130909     C                   CALL      'ANA964R'
097600130909     C                   PARM                    KPJBA
097700130909     C                   MoveL     KPJBU         ANA964DS
097800130909     C*
097900130909     C                   if        Ret964     <> '2'
098000130909     C                   MoveL     Val964        D1Origine
098100130909     C                   endif
098200130909     C*
098300130909     C                   ENDSR
098400130909     C/EJECT
098500000000     C************************************************************
098600000000     C* Ricerca tipo annullamento
098700000000     C************************************************************
098800000000     C     F4_TpAnn      BEGSR
098900000000     C*
099000130909     C                   Reset                   ANA964DS
099100000000     C*
099200130909     C                   MoveL     '01'          Opz964
099300170803     C                   MoveL     'SDGCST000'   Trc964
099400170803     C                   MoveL     'CSTTPANN'    Cam964
099500000000     C*
099600130909     C                   MoveL     ANA964DS      KPJBU
099700130909     C                   CALL      'ANA964R'
099800130909     C                   PARM                    KPJBA
099900130909     C                   MoveL     KPJBU         ANA964DS
100000000000     C*
100100000000     C                   if        Ret964     <> '2'
100200130909     C                   MoveL     Val964        D1TpAnn
100300000000     C                   endif
100400000000     C*
100500000000     C                   ENDSR
100600000000     C/EJECT
100700170807     C************************************************************
100800170807     C* Ricerca file telematico
100900170807     C************************************************************
101000170807     C     F4_File       BEGSR
101100170807     C*
101200170807     C                   Reset                   SDGSI82DS
101300170807     C                   Move      '01'          OpzI82
101400170807     C                   Move      XScSoc        SocietaI82
101500170807     C                   Z-ADD     C1Anno        AnnoI82
101600170807     C                   Z-ADD     C1Trim        TrimI82
101700170807     C                   MoveL     H1CF          CFI82
101800170807     C*
101900170807     C                   Select
102000170807     C*
102100170807     C     H1NMFL        WHENEQ    'D1FILEO'
102200170807     C                   Move(P)   D1FileO       FileI82
102300170807     C     H1NMFL        WHENEQ    'D1IDFILEO'
102400170807     C                   Move(P)   D1IdFileO     IdFileI82
102500170807     C*
102600170807     C     H1NMFL        WHENEQ    'D1FILER'
102700170807     C                   MoveL(P)  D1FileR       FileI82
102800170807     C     H1NMFL        WHENEQ    'D1IDFILER'
102900170807     C                   MoveL(P)  D1IdFileR     IdFileI82
103000170807     C*
103100170807     C     H1NMFL        WHENEQ    'D1FILEA'
103200170807     C                   MoveL(P)  D1FileA       FileI82
103300170807     C     H1NMFL        WHENEQ    'D1IDFILEA'
103400170807     C                   MoveL(P)  D1IdFileA     IdFileI82
103500170807     C                   endSL
103600170807     C*
103700170807     C                   Clear                   KPJBU
103800170807     C                   Movel     SDGSI82DS     KPJBU
103900170807     C                   Call      'SDGSI82R'
104000170807     C                   Parm                    KPJBA
104100170807     C                   Movel     KPJBU         SDGSI82DS
104200170807     C*
104300170807     C                   Select
104400170807     C*
104500170807     C     H1NMFL        WHENEQ    'D1FILEO'
104600170807     C                   MoveL(P)  FileI82       D1FileO
104700170807     C     H1NMFL        WHENEQ    'D1IDFILEO'
104800170807     C                   MoveL(P)  IdFileI82     D1IdFileO
104900170807     C*
105000170807     C     H1NMFL        WHENEQ    'D1FILER'
105100170807     C                   MoveL(P)  FileI82       D1FileR
105200170807     C     H1NMFL        WHENEQ    'D1IDFILER'
105300170807     C                   MoveL(P)  IdFileI82     D1IdFileR
105400170807     C*
105500170807     C     H1NMFL        WHENEQ    'D1FILEA'
105600170807     C                   MoveL(P)  FileI82       D1FileA
105700170807     C     H1NMFL        WHENEQ    'D1IDFILEA'
105800170807     C                   MoveL(P)  IdFileI82     D1IdFileA
105900170807     C                   endSL
106000170807     C*
106100170807     C                   ENDSR
106200170807     C/EJECT
106300170810     C************************************************************
106400170810     C* Controllo chiamata ricorsiva
106500170810     C************************************************************
106600170810     C     CHEKPGM       BEGSR
106700170810     C*
106800170810     C                   Eval      CallRicors  = *Off
106900170810     C*
107000170810     C                   Clear                   FlgSts
107100170810     C                   CALL      'XCHKPGM'                            2121
107200170810     C                   Parm                    $PGM             10
107300170810     C                   Parm      '0'           FlgSts            1
107400170810     C*
107500170810     C                   if        *In21       = *Off  and
107600170810     C                             FlgSts      = *On
107700170810     C*
107800170810     C                   Eval      CallRicors  = *On
107900170810     C* Emetto avviso di pgm ripetuto
108000170810     C                   Clear                   SDGMSGDS
108100170810     C                   EXSR      REPMSG
108200170810     C                   EXSR      CallSDGMSGR
108300170810     C                   endif
108400170810     C*
108500170810     C                   ENDSR
108600170810     C/EJECT
108700170810     C************************************************************
108800170810     C* Reperimento testo messaggi
108900170810     C************************************************************
109000170810     C     RepMsg        BEGSR
109100170810     C*
109200170810     C* "Funzione non richiamabile"
109300170810     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0430'
109400170810     C                   EXSR      PrendiMsg
109500170810     C                   if        ERRMSG     <> *ON
109600170810     C                   MoveL     TXTMSG        MSGMS1
109700170810     C                   else
109800170810     C                   MoveL     *ALL'?'       MSGMS1
109900170810     C                   endif
110000170810     C*
110100170810     C                   Eval      MsgMs2      = $Pgm
110200170810     C*
110300170810     C* "Causa chiamata ricorsiva"
110400170810     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0449'
110500170810     C                   EXSR      PrendiMsg
110600170810     C                   if        ERRMSG     <> *ON
110700170810     C                   MoveL     TXTMSG        MSGMS3
110800170810     C                   else
110900170810     C                   MoveL     *ALL'?'       MSGMS3
111000170810     C                   endif
111100170810     C*
111200170810     C                   ENDSR
111300170810     C/EJECT
111400170810     C************************************************************
111500170810     C* Reperimento testo messaggi
111600170810     C************************************************************
111700170810     C     PrendiMsg     BEGSR
111800170810     C*
111900170810     C                   CALLB     'XRTVM'
112000170810     C                   PARM                    IDMSG            27
112100170810     C                   PARM                    TXTMSG          100
112200170810     C                   PARM                    ERRMSG            1
112300170810     C*
112400170810     C                   ENDSR
112500170810     C/EJECT
112600170810     C************************************************************
112700170810     C* Avvertimento chiamata ricorsiva
112800170810     C************************************************************
112900170810     C     CallSDGMSGR   BEGSR
113000170810     C*
113100170810     C                   MoveL     'N'           MSGRSP
113200170810     C                   MoveL     *Blank        MSGTPR
113300170810     C                   MoveL     'S'           MSGEMV
113400170810     C                   Z-ADD     10            MSGRGP
113500170810     C                   MoveL     'N'           MSGCNL
113600170810     C                   MOVEL     0             MSGMSG
113700170810     C                   MOVEA     STATUS        STS
113800170810     C                   MoveL     'N'           MSGVID
113900170810     C                   MoveL     'N'           MSGOPE
114000170810     C                   MoveL     'N'           MSGSTP
114100170810     C                   MoveL     'N'           MSGLCK
114200170810     C*
114300170810     C                   CALL      'SDGMSGR'
114400170810     C                   PARM                    SDGMSGDS
114500170810     C                   PARM      *Blank        XMSTxt          135
114600170810     C                   PARM      WXMsgInz      XMSGInz           1
114700170810     C*
114800170810     C                   Move      *Off          WXMsgInz
114900170810     C*
115000170810     C                   ENDSR
115100170810     C/EJECT
115200130909     C************************************************************
115300000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
115400000000     C************************************************************
115500000000     C     GESPIE        BEGSR
115600000000     C*
115700000000     C                   SELECT
115800000000     C*
115900000000     C     $GEST         WHENEQ    'D1'
116000000000     C     $ULKD1        IFEQ      0
116100000000     C* riempimento opzioni
116200130909     C                   EXSR      F24D1
116300000000     C                   ELSE
116400000000     C                   WRITE     FMTZ1
116500000000     C                   ENDIF
116600000000     C*
116700000000     C     $GEST         WHENEQ    'D2'
116800000000     C     $ULKD1        IFEQ      0
116900000000     C* riempimento opzioni
117000130909     C                   EXSR      F24D2
117100000000     C                   ELSE
117200000000     C                   WRITE     FMTZ1
117300000000     C                   ENDIF
117400000000     C*
117500000000     C     $GEST         WHENEQ    'D3'
117600000000     C     $ULKD1        IFEQ      0
117700000000     C* riempimento opzioni
117800130909     C                   EXSR      F24D3
117900000000     C                   ELSE
118000000000     C                   WRITE     FMTZ1
118100000000     C                   ENDIF
118200000000     C                   ENDSL
118300000000     C*
118400000000     C                   ENDSR
118500000000     C/EJECT
118600000000     C************************************************************
118700000000     C* Chiamata programma con tasto personalizzato
118800000000     C************************************************************
118900000000     C     FPerD1        BEGSR
119000000000     C*
119100000000     C* ATTENZIONE NON PUÒ FUNZIONARE:
119200170803     C* la DS SDGSI76DS è più lunga della KPJBU e la chiamata
119300000000     C* al pgm con tasto personalizzato passa i dati tramite
119400130909     C* la KPJBA
119500000000     C*
119600000000     C* salvo la DS di ingresso
119700170803    >C                   MoveL     SDGSI76DS     $SaveDS
119800000000     C*
119900170803    >C                   MOVE      xscsoc        SocietaI76
120000170803     C* passare nei campi appositi della DS SDGSI76DS il valore
120100000000     C* della chiave della VL univoca
120200170803     C                   Eval      CFI76       = H1CF
120300170804     C                   Eval      SoggInpI76  = D1Sogg
120400170804     C                   Eval      PartIvaI76  = C1PartIva
120500170804     C                   Eval      CdFiscI76   = C1CdFisc
120600170804     C                   Eval      CogRag1I76  = H1CogRag1
120700170804     C                   Eval      Stato1I76   = C1Stato1
120800000000     C*
120900170803    >C                   MoveL     SDGSI76DS     KPJBU
121000130909     C                   CALL      $PGF1(PosFun)                        22
121100130909     C                   PARM                    KPJBA
121200130909     C                   PARM                    NOMPGM
121300000000     C*
121400000000     C* ripristino la DS di ingresso
121500170803    >C                   MoveL     $SaveDS       SDGSI76DS
121600000000     C*
121700000000     C                   ENDSR
121800000000     C/EJECT
121900000000     C************************************************************
122000000000     C* GESTIONE VIDEO RECORD D1
122100000000     C************************************************************
122200000000     C     GESD1         BEGSR
122300000000     C* inizializzazione videata
122400000000   B1C     $INZD1        IFEQ      *ON
122500130909     C                   EXSR      INZD1
122600130909     C                   MOVE      *Off          $INZD1
122700000000   E1C                   ENDIF
122800000000     C* emissione piede videata se proveniente da altra
122900000000     C* salvataggio valore formato di provenienza
123000000000   B1C     $LASTV        IFNE      'D1'
123100000000     C                   WRITE     FMTT1
123200130909     C                   EXSR      GESPIE
123300000000     C                   MOVE      $LASTV        $LASTG
123400000000     C                   MOVE      'D1'          $LASTV
123500000000   E1C                   ENDIF
123600000000     C*
123700000000     C* se esistono errori sulla videata
123800000000     C* emetto la write del formato a indicatori spenti per vedere
123900000000     C* le eventuali decodifiche
124000130926   B1C                   if        *IN99       = *On      and
124100130926     C                             $GEST        = 'D1'
124200000000     C                   MOVEA     *IN           WIN
124300000000     C                   MOVE      *ALL'0'       IN5098           49
124400000000     C                   MOVEA     IN5098        *IN(50)
124500000000     C                   WRITE     FMTD1
124600000000     C                   MOVEA     WIN           *IN
124700000000   E1C                   ENDIF
124800000000     C* Inizializzazione indice tasti funzione personalizzati
124900130909     C                   Eval      PosFun = 25
125000000000     C*
125100000000   B1C                   SELECT
125200000000     C* Annullamento
125300170803    >C     OPZI76        WHENEQ    '04'
125400000000     C* Visualizzazione
125500170803    >C     OPZI76        OREQ      '05'
125600000000     C                   WRITE     FMTD1
125700000000     C                   EXFMT     PROTECT
125800000000     C* Immissione/Modifica/Copia
125900000000   X1C                   OTHER
126000000000     C                   EXFMT     FMTD1
126100000000   E1C                   ENDSL
126200000000     C*
126300000000     C* azzero la posizione del cursore
126400000000     C                   Z-ADD     0             H1RIGA
126500000000     C                   Z-ADD     0             H1COLO
126600000000     C*
126700000000     C* controllo abilitazione tasti funzione
126800130909     C                   EXSR      CTRKD1
126900000000     C*
127000130909   B1C     *IN99         IFEQ      *Off
127100000000     C*
127200000000   B2C                   SELECT
127300000000     C* ROLLUP
127400000000    >C     $TASTO        WHENEQ    ROLLUP
127500000000    >C                   MOVE      'D2'          $GEST
127600000000     C* F2=Decodifica
127700000000     C     $TASTO        WHENEQ    F02
127800130909     C                   EXSR      F02D1
127900000000     C* F3=Fine
128000000000     C     $TASTO        WHENEQ    F03
128100130909     C                   EXSR      F03D1
128200000000     C* F4=Ricerca
128300000000     C     $TASTO        WHENEQ    F04
128400130909     C                   EXSR      SEARCH
128500000000     C* F5=Refresh
128600000000     C     $TASTO        WHENEQ    F05
128700130909     C                   EXSR      F05D1
128800000000     C* F8=Successivo
128900000000     C     $TASTO        WHENEQ    F08
129000130909     C                   EXSR      F08D1
129100000000     C* F12=Ritorno
129200000000     C     $TASTO        WHENEQ    F12
129300130909     C                   EXSR      F12D1
129400000000     C* F24=Altri tasti
129500000000     C     $TASTO        WHENEQ    F24
129600130909     C                   EXSR      F24D1
129700000000     C* Personalizzata
129800170803     C* NB chiamata NON possibile perchè SDGSI76DS più lunga
129900130909     C* di KPJBU 256 caratteri                          a
130000000000     C**** $PGF1(PosFun) WHENNE    *BLANK
130100130909     C****               EXSR      FPerD1
130200000000     C*
130300000000   X2C                   OTHER
130400130909     C                   EXSR      CTRD1
130500000000     C* aggiungere le chiamate ai controlli delle videate supplementari
130600130909    >C     *IN99         IFEQ      *Off
130700130909    >C                   EXSR      CTRD2
130800130926   B1C                   if        *IN99       = *On
130900000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
131000000000    >C* indicatori uguali in videate differenti, genererei conflitti
131100000000    >C                   MOVE      *ALL'0'       IN5098           49
131200000000    >C                   MOVEA     IN5098        *IN(50)
131300130909    >C                   SetOn                                        9996                       96
131400000000    >C                   ENDIF
131500000000    >C                   ENDIF
131600000000     C* aggiungere le chiamate ai controlli delle videate supplementari
131700130909    >C     *IN99         IFEQ      *Off
131800130923    >C                   EXSR      CTRD3
131900130926   B1C                   if        *IN99       = *On
132000000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
132100000000    >C* indicatori uguali in videate differenti, genererei conflitti
132200000000    >C                   MOVE      *ALL'0'       IN5098           49
132300000000    >C                   MOVEA     IN5098        *IN(50)
132400130909    >C                   SetOn                                        9996                       96
132500000000    >C                   ENDIF
132600000000    >C                   ENDIF
132700000000     C*
132800000000   B3C     $TASTO        IFEQ      F06
132900130909     C     *IN99         ANDEQ     *Off
133000000000     C* eseguo aggiornamento
133100130909     C                   EXSR      AGGANA
133200000000     C* eseguo operazioni del dopo-aggiornamento
133300130909   B4C     *IN99         IFEQ      *Off
133400130909     C                   EXSR      GESAGG
133500000000   E4C                   ENDIF
133600000000   E3C                   ENDIF
133700000000     C*
133800000000   E2C                   ENDSL
133900000000     C*
134000000000   E1C                   ENDIF
134100000000     C*
134200000000     C                   ENDSR
134300000000     C/EJECT
134400000000     C************************************************************
134500000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
134600000000     C************************************************************
134700000000     C     CTRKD1        BEGSR
134800000000     C*
134900130909     C                   SetOff                                       99
135000000000     C                   MOVEA     $FC1          $FC
135100130909     C                   EXSR      CTRKEY
135200000000     C     *IN99         IFEQ      *ON
135300130909     C                   SetOn                                        97
135400000000     C                   ENDIF
135500000000     C*
135600000000     C                   ENDSR
135700000000     C/EJECT
135800000000     C************************************************************
135900000000     C* INIZIALIZZAZIONE VIDEATA DATI
136000000000     C************************************************************
136100000000     C     INZD1         BEGSR
136200000000     C*
136300000000     C                   CLEAR                   FMTD1
136400000000     C                   CLEAR                   FMTD2
136500000000     C                   CLEAR                   FMTD3
136600170807     C*
136700170807     C                   Clear                   $Sys
136800170807     C                   Clear                   $NrAsReg
136900170807     C                   Clear                   $NrRigaM
137000170807     C                   Clear                   $NrRiga
137100000000     C*
137200130924     C                   Eval      D1On1       = XScOn
137300130924     C                   Eval      D1On2       = XScOn
137400130909     C                   Eval      D1On3       = XScOn
137500130909     C                   Eval      D1On4       = XScOn
137600130909     C                   Eval      D1On5       = XScOn
137700130924     C                   Eval      D1Off1      = XScOff
137800130924     C                   Eval      D1Off2      = XScOff
137900130909     C                   Eval      D1Off3      = XScOff
138000130909     C                   Eval      D1Off4      = XScOff
138100130909     C                   Eval      D1Off5      = XScOff
138200000000     C*
138300170803     C                   Eval      C1Anno      = AnnoI76
138400170803     C                   Eval      C1Trim      = TrimI76
138500170803     C                   Eval      C1CogRag1   = CogRag1I76
138600170803     C                   Eval      C1Stato1    = Stato1I76
138700170803     C                   Eval      C1PartIVA   = PartIVAI76
138800170803     C                   Eval      C1CdFisc    = CdFiscI76
138900170803     C*
139000170803     C                   Eval      H1CF        = CFI76
139100170803     C                   Eval      H1CogRag1   = CogRag1I76
139200170803     C*
139300170803     C*
139400170804     C                   Eval      D1Sogg      = SoggInpI76
139500170804     C                   Eval      D1Causale   = CausInpI76
139600140401     C*
139700000000     C* per tutte le opzioni NON immissione
139800170803     C     OPZI76        IFNE      '01'
139900000000     C*
140000130909     C     *IN22         DOUEQ     *Off
140100000000     C     MSGRTN        ORNE      'R'
140200000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
140300170803     C     OPZI76        IFEQ      '02'
140400170803    >C     OPZI76        OREQ      '04'
140500170803     C     NrRcSTI76     CHAIN     SDGCST00F                          2122
140600000000     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
140700000000   X3C                   ELSE
140800170803     C     NrRcSTI76     CHAIN(N)  SDGCST00F                          2122
140900000000   E3C                   ENDIF
141000000000     C*
141100000000     C* Gestione RCD allocato
141200000000   B3C     *IN22         IFEQ      *ON
141300170803    >C                   MoveL     'SDGCST00F'   MSGFIL
141400130909     C                   EXSR      MSGPGM
141500000000   E3C                   ENDIF
141600000000   E2C                   ENDDO
141700000000     C*
141800000000     C* se il rcd da gestire non viene trovato
141900000000     C* oppure è allocato da altro pgm
142000000000     C* ritorno al pgm guida avvertendolo dell'errore
142100000000   B2C     *IN21         IFEQ      *ON
142200000000     C     *IN22         OREQ      *ON
142300000000     C                   MOVE      *ON           $FINE
142400170803    >C                   MOVE      '1'           ERRI76
142500000000     C*
142600000000     C* altrimenti
142700000000   X2C                   ELSE
142800000000     C* riempio la videata
142900130909     C                   EXSR      RIED1
143000000000    >C* Aggiungere le routine di riempimento dei record succcessivi
143100130909    >C                   EXSR      RIED2
143200000000    >C* Aggiungere le routine di riempimento dei record succcessivi
143300130909    >C                   EXSR      RIED3
143400000000     C* richiamo routine dei ctrl per decodificarla
143500130909     C                   EXSR      CTRD1
143600000000    >C* Aggiungere le routine di controllo dei record succcessivi
143700130909    >C                   EXSR      CTRD2
143800130923    >C                   EXSR      CTRD3
143900000000     C                   MOVE      *ALL'0'       IN5098
144000000000     C                   MOVEA     IN5098        *IN(50)
144100170807     C*
144200170807     C* definizione indicatori su video
144300170807     C                   SETOFF                                       02
144400170807   B3C                   SELECT
144500170807    >C     OPZI76        WHENEQ    '02'
144600170807     C                   SETON                                        02        PR campi key
144700170807    >C     OPZI76        WHENEQ    '05'
144800170807   E3C                   ENDSL
144900170807     C*
145000000000   E2C                   ENDIF
145100000000     C*
145200000000     C* per immissione
145300000000   X1C                   ELSE
145400000000     C*
145500000000     C* riempio con i default di immissione  (se reperiti)
145600000000     C                   IF        $FlgDef     = *on
145700130909     C                   EXSR      RIEDEF
145800000000     C                   ENDIF
145900000000     C*
146000000000     C* imposto i campi chiave ricevuti anche se si è in immissione
146100000000     C* perchè potrebbero essere stati passati dei default di chiave
146200000000     C*
146300000000     C* decodifico le costanti
146400130909     C                   EXSR      F02D1
146500000000   E1C                   ENDIF
146600110819     C*
146700170803     C     OPZI76        IFEQ      '01'
146800170803    >C     OPZI76        OREQ      '03'
146900130909     C                   Eval      D1Origine   = 'M'
147000110819     C*
147100170815fm5  C                   if        $NrAsReg    = 0
147200110819     C* Reperisco il numero assoluto della registrazione fittizia
147300130909     C                   EXSR      RepNrAss
147400110819     C                   endif
147500110819     C                   endif
147600170809     C*
147700170809     C* Visualizzo data annotazione solo se richiesto dal parametro modulo
147800170809     C                   If        CFI76 = 'C' and
147900170809     C                             DtRifCA12 = 'A'
148000170809     C                   Seton                                        12
148100170809     C                   Endif
148200000000     C*
148300000000     C* imposto quegli attributi della videata che sono modificabili
148400000000     C* al richiamo del determinato pgm
148500130909     C                   EXSR      ATRD1
148600000000     C*
148700000000     C                   ENDSR
148800000000     C/EJECT
148900000000     C************************************************************
149000000000     C* RIEMPIMENTO VIDEATA  D1
149100000000     C************************************************************
149200000000     C     RIED1         BEGSR
149300000000     C*
149400170803     C                   Eval      H1CF        = CSTCF
149500170807     C*
149600170807     C                   if        OPZI76      = '02'
149700170807     C                   Eval      $Sys        = CSTSys
149800170807     C                   Eval      $NrAsReg    = CSTNrAsReg
149900170807     C                   Eval      $NrRigaM    = CSTNrRigaM
150000170807     C                   Eval      $NrRiga     = CSTNrRiga
150100170807     C                   endif
150200130923     C*
150300170804     C                   Eval      C1PartIva   = CSTPartIva
150400170804     C                   Eval      C1CdFisc    = CSTCdFisc
150500170804     C                   Eval      C1CogRag1   = CSTCogRag1
150600170804     C                   Eval      H1CogRag1   = CSTCogRag1
150700170804     C                   Eval      C1Stato1    = CSTStato1
150800130923     C*
150900170807     C                   Eval      D1TpRec     = CSTTpRec
151000170807     C                   Eval      D1TpDoc     = CSTTpDoc
151100170804     C*
151200170804     C                   if        CSTIntra    = '1'
151300170804     C                   Eval      D1Intra     = XScOn
151400170804     C                   else
151500170804     C                   Eval      D1Intra     = XScOff
151600170804     C                   endif
151700130923     C*
151800170804     C                   Eval      D1MotEscl   = CSTMotEscl
151900130923     C*
152000170803     C                   if        OPZI76     <> '03'
152100170803     C                   Eval      D1ImpOpeO   = CSTImpOpeO
152200170803     C                   Eval      D1IvaOpeO   = CSTIvaOpeO
152300130925     C                   endif
152400170803     C                   Eval      D1ImpOpe    = CSTImpOpe
152500170804     C                   Eval      D1IvaOpe    = CSTIvaOpe
152600170804     C*
152700170804     C                   if        CSTDeducib  = '1'
152800170804     C                   Eval      D1Deducib   = XScOn
152900170804     C                   else
153000170804     C                   Eval      D1Deducib   = XScOff
153100170804     C                   endif
153200130923     C*
153300170803     C                   if        CSTAutofat  = '1'
153400130923     C                   Eval      D1Autofat   = XScOn
153500130923     C                   else
153600130923     C                   Eval      D1Autofat   = XScOff
153700130923     C                   endif
153800170804     C*
153900170810     C                   Eval      D1Esigib    = CstEsigib
154000130923     C*
154100170803     C                   Eval      D1NatOpe    = CSTNatOpe
154200130923     C*
154300170803     C                   if        CSTAnn      = '1'
154400130923     C                   Eval      D1Ann       = XScOn
154500130923     C                   else
154600130923     C                   Eval      D1Ann       = XScOff
154700130923     C                   endif
154800170803     C                   Eval      D1TpAnn     = CSTTpAnn
154900110902     C*
155000000000     C                   ENDSR
155100000000     C************************************************************
155200000000     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
155300000000     C************************************************************
155400000000     C     ATRD1         BEGSR
155500000000     C*
155600000000    >C* in questa routine bisogna attivare indicatori o campi programma
155700000000    >C* che gestiscono gli attributi di uno più campi di questa videata;
155800000000    >C* attenzione : non è questo il posto in cui eseguire controlli,
155900000000    >C*  dovrebbero già arrivare dei flag
156000000000     C*
156100130909     C                   SetOff                                       02
156200000000     C*
156300000000     C* in caso di manutenzione
156400170803    >C     OPZI76        IFEQ      '02'
156500000000     C* proteggo i campi chiave
156600130909     C                   SetOn                                        02        PR campi key
156700000000   E3C                   ENDIF
156800000000     C*
156900000000     C                   ENDSR
157000000000     C/EJECT
157100000000     C************************************************************
157200000000     C* RIEMPIMENTO CAMPI DAI DEFAULT
157300000000     C************************************************************
157400000000     C     RIEDEF        BEGSR
157500000000     C*
157600170807     C                   Eval      D1TpRec     = TpRecI76
157700170807     C                   Eval      D1TpDoc     = 'TD01'
157800170803     C                   Eval      D1Intra     = XScOff
157900170803     C                   Eval      D1NatOpe    = BeniSrvA12
158000170803     C                   Eval      D1Autofat   = XScOff
158100170803     C                   Eval      D1MotEscl   = *Blank
158200170803     C                   Eval      D1PercDet   = 0
158300170803     C                   Eval      D1Deducib   = XScOff
158400170804     C                   Eval      D1Esigib    = '1'
158500130909     C                   Eval      D1NoIva     = XScOff
158600130909     C                   Eval      D1Origine   = 'M'
158700170803     C*
158800130911     C                   Eval      D1TpAnn     = *Blank
158900130909     C                   Eval      D1Ann       = XScOff
159000000000     C*
159100130919     C* mi posiziono sul primo record inserito per recuperare
159200130919     C* il soggetto e il tipo natura giuridica digitati in
159300130919     C* inserimento dei dati anagrafici
159400170803     C                   EXSR      RepAnagraf
159500130919     C*
159600000000     C                   ENDSR
159700000000     C/EJECT
159800170803     C************************************************************
159900170803     C* Reperiti dati anagrafici a parità di partita IVA/cd fiscale
160000170803     C************************************************************
160100170803     C     RepAnagraf    BEGSR
160200170803     C*
160300170803     C                   Eval      CSTSocieta  = SocietaI76
160400170803     C                   Eval      CSTTpCom    = TpComI76
160500170803     C*
160600170803     C* i dati anagrafici devono gli stessi nel trimestre,
160700170803     C* indipendentemente dal tipo record
160800170807     C*****              Eval      CSTTpRec    = D1TpRec
160900170803     C*
161000170803     C                   Eval      CSTAnno     = AnnoI76
161100170803     C                   Eval      CSTTrim     = TrimI76
161200170803     C                   Eval      CSTMese     = MeseI76
161300170803     C                   Eval      CSTCF       = CFI76
161400170804     C                   Eval      CSTPartIVA  = C1PartIVA
161500170803     C                   Eval      CSTCdFisc   = C1CdFisc
161600170803     C*
161700170803     C     K08CST05      SETLL     SDGCST05L
161800170803     C     K08CST05      READE     SDGCST05L                              21
161900170803     C                   if        *In21       = *Off
162000170803     C                   Eval      D1TpN1      = CSTTpN1
162100170803     C                   Eval      D1Sogg      = CSTSogg
162200170803     C                   endif
162300170803     C*
162400170803     C                   ENDSR
162500170803     C/EJECT
162600000000     C************************************************************
162700000000     C* GESTIONE F02 VIDEO D1
162800000000     C************************************************************
162900000000     C     F02D1         BEGSR
163000000000     C*
163100000000     C* per decodificare la videata richiamo la routine dei ctrl
163200000000     C* e poi disattivo gli eventuali messaggi di errore
163300130909     C                   EXSR      CTRD1
163400000000     C*
163500000000     C                   MOVE      *ALL'0'       IN5098
163600000000     C                   MOVEA     IN5098        *IN(50)
163700000000     C*
163800000000     C                   ENDSR
163900000000     C/EJECT
164000000000     C************************************************************
164100000000     C* GESTIONE F03 VIDEO D1
164200000000     C************************************************************
164300000000     C     F03D1         BEGSR
164400000000     C*
164500000000     C                   MOVE      *ON           $FINE
164600170803    >C                   MOVE      '1'           RETI76
164700000000     C* fine programma
164800130909     C                   EXSR      ENDPGM
164900000000     C*
165000000000     C                   ENDSR
165100000000     C/EJECT
165200000000     C************************************************************
165300000000     C* GESTIONE F05 VIDEO D1
165400000000     C************************************************************
165500000000     C     F05D1         BEGSR
165600000000     C*
165700000000     C                   MOVE      *ON           $INZD1
165800000000     C*
165900000000     C                   ENDSR
166000000000     C/EJECT
166100000000     C************************************************************
166200000000     C* GESTIONE F08 VIDEO D1
166300000000     C************************************************************
166400000000     C     F08D1         BEGSR
166500000000     C*
166600000000     C                   MOVE      *ON           $FINE
166700170803    >C                   MOVE      '0'           RETI76
166800000000     C*
166900000000     C                   ENDSR
167000000000     C/EJECT
167100130923     C************************************************************
167200130923     C* GESTIONE F12 VIDEO D1
167300130923     C************************************************************
167400130923     C     F12D1         BEGSR
167500130923     C*
167600130923     C                   MOVE      *ON           $FINE
167700170803    >C                   MOVE      '2'           RETI76
167800130923     C*
167900130923     C                   ENDSR
168000130923     C/EJECT
168100000000     C************************************************************
168200000000     C* GESTIONE F24 VIDEO D1
168300000000     C************************************************************
168400000000     C     F24D1         BEGSR
168500000000     C*
168600000000     C                   MOVEA     $FX1          $FX
168700000000     C                   MOVEA     $FC1          $FC
168800000000     C                   MOVEA     $FR1          $FR
168900000000     C                   Z-ADD     $ULKD1        $ULK
169000000000     C                   Z-ADD     $D1L01        $L01
169100000000     C                   Z-ADD     $D1L02        $L02
169200130909     C                   EXSR      RIEKEY
169300000000     C                   Z-ADD     $ULK          $ULKD1            3 0
169400000000     C                   MOVEA     $FC           $FC1
169500130909     C                   MoveL     $KEY1         Z1KE1
169600130909     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
169700130909     C                   MoveL     $KEY2         Z1KE2
169800000000     C                   WRITE     FMTZ1
169900000000     C*
170000000000     C                   ENDSR
170100000000     C/EJECT
170200000000     C************************************************************
170300000000     C* CONTROLLO VIDEATA
170400000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
170500000000     C* 95 96 97 98
170600000000     C************************************************************
170700000000     C     CTRD1         BEGSR
170800000000     C*
170900130909     C                   SetOff                                       99
171000000000     C*
171100000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
171200000000     C*
171300170803     C     OPZI76        IFNE      '04'
171400130918     C*
171500130918     C                   EXSR      CtrCF
171600130918     C*
171700170807     C                   EXSR      CtrTpRec
171800170807     C                   EXSR      CtrTpDoc
171900170807     C                   EXSR      CtrIntra
172000170807     C                   EXSR      CtrNatOpe
172100170807     C                   EXSR      CtrAutofat
172200170807     C*
172300170807     C                   EXSR      CtrMotEscl
172400130918     C                   EXSR      CtrImporti
172500170807     C*
172600170807     C                   EXSR      CtrDeducib
172700170807     C                   EXSR      CtrEsigib
172800130918     C*
172900130918     C                   EXSR      CtrAnn
173000130918     C                   EXSR      CtrTpAnn
173100000000     C*
173200000000     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
173300170803     C     OPZI76        IFEQ      '01'
173400170803    >C     OPZI76        OREQ      '03'
173500130909     C                   EXSR      CtrExist1
173600000000     C                   ENDIF
173700000000     C*
173800000000     C                   ENDIF
173900000000     C*
174000000000     C                   ENDSR
174100000000     C/EJECT
174200000000     C***********************************************************
174300130911     C* Decofico  la sottonatura
174400000000     C***********************************************************
174500130911     C     CtrCF         BEGSR
174600000000     C*
174700170804     C                   Clear                   C1CFD
174800130911     C                   Clear                   X07ValDs
174900130911     C*
175000130911     C                   Move      '1'           X07Ric
175100130911     C                   Move      *On           X07All
175200170803     C                   MoveL     'SDGCST000'   X07TRc
175300170803     C                   MoveL     'CSTCF    '   X07Cam
175400130911     C                   MoveL     H1CF          X07VAL
175500130911     C*
175600130911     C                   CALLB     'X07VALR'
175700130911     C                   PARM                    X07ValDs
175800130911     C*
175900130911     C     X07ERR        IFEQ      *Off
176000170804     C                   MoveL     X07DES        C1CFD
176100130911     C                   endif
176200130911     C*
176300130911     C                   ENDSR
176400130911     C/EJECT
176500130918     C************************************************************
176600170807     C* Controllo tipo record
176700130918     C************************************************************
176800170807     C     CtrTpRec      BEGSR
176900130918     C*
177000170807     C                   Clear                   D1TpRecD
177100130918     C                   Clear                   X07ValDs
177200130918     C*
177300130918     C                   Move      '1'           X07Ric
177400130918     C                   Move      *On           X07All
177500170803     C                   MoveL     'SDGCST000'   X07TRc
177600170807     C                   MoveL     'CSTTPREC  '  X07Cam
177700170807     C                   MoveL     D1TpRec       X07VAL
177800130918     C*
177900130918     C                   CALLB     'X07VALR'
178000130918     C                   PARM                    X07ValDs
178100130918     C*
178200130918     C     X07ERR        IFEQ      *Off
178300170807     C                   MoveL     X07DES        D1TpRecD
178400130918     C                   else
178500170807     C                   SetOn                                        6299
178600130918     C                   endif
178700130918     C*
178800130918     C                   ENDSR
178900130918     C/EJECT
179000170807     C************************************************************
179100170807     C* Controllo tipo documento
179200170807     C************************************************************
179300170807     C     CtrTpDoc      BEGSR
179400170807     C*
179500170807     C                   Clear                   D1TpDocD
179600170807     C                   Clear                   X07ValDs
179700170807     C*
179800170807     C                   Move      '1'           X07Ric
179900170807     C                   Move      *On           X07All
180000170807     C                   MoveL     'SDGCST000'   X07TRc
180100170807     C                   MoveL     'CSTTPDOC  '  X07Cam
180200170807     C                   MoveL     D1TpDoc       X07VAL
180300170807     C*
180400170807     C                   CALLB     'X07VALR'
180500170807     C                   PARM                    X07ValDs
180600170807     C*
180700170807     C     X07ERR        IFEQ      *Off
180800170807     C                   MoveL     X07DES        D1TpDocD
180900170807     C                   else
181000170807     C                   SetOn                                        5099
181100170807     C                   endif
181200170807     C*
181300170807     C                   ENDSR
181400170807     C/EJECT
181500170807     C************************************************************
181600170807     C* Controllo operazione intracomunitaria
181700170807     C************************************************************
181800170807     C     CtrIntra      BEGSR
181900170807     C*
182000170807     C                   if        D1Intra    <> XScOn    and
182100170807     C                             D1Intra    <> XScOff
182200170807     C                   SetOn                                        5199
182300170807     C                   endif
182400170807     C*
182500170807     C                   ENDSR
182600170807     C/EJECT
182700170807     C************************************************************
182800170807     C* Controllo natura operazione
182900170807     C************************************************************
183000170807     C     CtrNatOpe     BEGSR
183100170807     C*
183200170807     C                   Clear                   D1NatOpeD
183300170807     C                   Clear                   X07ValDs
183400170807     C*
183500170807     C                   Move      '1'           X07Ric
183600170807     C                   Move      *On           X07All
183700170807     C                   MoveL     'SDGCST000'   X07TRc
183800170807     C                   MoveL     'CSTNATOPE '  X07Cam
183900170807     C                   MoveL     D1NatOpe      X07VAL
184000170807     C*
184100170807     C                   CALLB     'X07VALR'
184200170807     C                   PARM                    X07ValDs
184300170807     C*
184400170807     C     X07ERR        IFEQ      *Off
184500170807     C                   MoveL     X07DES        D1NatOpeD
184600170807     C                   else
184700170807     C                   SetOn                                        5299
184800170807     C                   endif
184900170807     C*
185000170807     C                   ENDSR
185100170807     C/EJECT
185200170807     C************************************************************
185300170807     C* Controllo autofattura
185400170807     C************************************************************
185500170807     C     CtrAutofat    BEGSR
185600170807     C*
185700170807     C                   if        D1Autofat  <> XScOn    and
185800170807     C                             D1Autofat  <> XScOff
185900170807     C                   SetOn                                        5899
186000170807     C                   endif
186100170807     C*
186200170807     C                   ENDSR
186300170807     C/EJECT
186400130918     C************************************************************
186500170804     C* Controllo motivo di esclusione
186600130918     C************************************************************
186700170804     C     CtrMotEscl    BEGSR
186800130918     C*
186900170804     C                   Clear                   D1MotEsclD
187000130918     C                   Clear                   X07ValDs
187100130918     C*
187200170804     C                   if        D1MotEscl  <> *Blank
187300130918     C                   Move      '1'           X07Ric
187400130918     C                   Move      *On           X07All
187500170803     C                   MoveL     'SDGCST000'   X07TRc
187600170804     C                   MoveL     'CSTMOTESCL'  X07Cam
187700170804     C                   MoveL     D1MotEscl     X07VAL
187800130918     C*
187900130918     C                   CALLB     'X07VALR'
188000130918     C                   PARM                    X07ValDs
188100130918     C*
188200130918     C     X07ERR        IFEQ      *Off
188300170804     C                   MoveL     X07DES        D1MotEsclD
188400130918     C                   else
188500130918     C                   SetOn                                        5399
188600130918     C                   endif
188700130918     C*
188800170804     C                   endif
188900170804     C*
189000130918     C                   ENDSR
189100130918     C/EJECT
189200130918     C************************************************************
189300130918     C* Controllo importi
189400130918     C************************************************************
189500130918     C     CtrImporti    BEGSR
189600130918     C*
189700130919     C* se gli importi dell'operazione NON
189800130930     C* sono valorizzati, imposto i valori di origine
189900130918     C                   if        D1ImpOpe    = 0         and
190000130918     C                             D1IVAOpe    = 0
190100130918     C                   Eval      D1ImpOpe    = D1ImpOpeO
190200130918     C                   Eval      D1IVAOpe    = D1IVAOpeO
190300130918     C                   endif
190400130918     C*
190500170807???? C* se acquisti intra, l'importo può essere sia positivo che negativo
190600170807???? C******             if        D1TpDoc      <> 'TD10'   and
190700170807???? C******                       D1TpDoc      <> 'TD11'
190800170807     C*
190900170807     C* se note di credito, l'importo deve essere negativo
191000170807     C                   if        D1TpDoc       = 'TD04'
191100170807     C*
191200170807     C* il segno deve essere negativo
191300170807     C                   if        D1ImpOpe      > 0      or
191400170807     C                             D1IVAOpe      > 0
191500170814fm4  X*****              SetOn                                        6199
191600170807     C                   endif
191700170807     C*
191800170807     C                   else
191900130930     C*
192000130927     C* il segno deve essere positivo
192100130930     C                   if        D1ImpOpe      < 0      or
192200130927     C                             D1IVAOpe      < 0
192300170814fm4  X*****              SetOn                                        5499
192400130918     C                   endif
192500130918     C*
192600130930     C                   endif
192700130925     C*
192800130925     C                   if        D1ImpOpe      = 0      and
192900130925     C                             D1IVAOpe      = 0
193000170807     C                   SetOn                                        5599
193100130925     C                   endif
193200130918     C*
193300130918     C                   ENDSR
193400130918     C/EJECT
193500130918     C************************************************************
193600170807     C* Controllo spese deducibili
193700130918     C************************************************************
193800170807     C     CtrDeducib    BEGSR
193900130918     C*
194000170807     C                   if        D1Deducib  <> XScOn    and
194100170807     C                             D1Deducib  <> XScOff
194200170807     C                   SetOn                                        5699
194300170807     C                   endif
194400170807     C*
194500170807     C* i valori sono in alternativa
194600170807     C                   if        D1Deducib   = XScOn    and
194700170807     C                             D1PercDet   > 0
194800170807     C                   SetOn                                        5799
194900170807     C                   endif
195000170807     C*
195100170807     C                   ENDSR
195200170807     C/EJECT
195300170807     C************************************************************
195400170807     C* Controllo esigibilità
195500170807     C************************************************************
195600170807     C     CtrEsigib     BEGSR
195700170807     C*
195800170807     C                   Clear                   D1EsigibD
195900170807     C                   Clear                   X07ValDs
196000170807     C*
196100170807     C                   Move      '1'           X07Ric
196200170807     C                   Move      *On           X07All
196300170807     C                   MoveL     'SDGCST000'   X07TRc
196400170807     C                   MoveL     'CSTESIGIB '  X07Cam
196500170807     C                   MoveL     D1Esigib      X07VAL
196600170807     C*
196700170807     C                   CALLB     'X07VALR'
196800170807     C                   PARM                    X07ValDs
196900170807     C*
197000170807     C     X07ERR        IFEQ      *Off
197100170807     C                   MoveL     X07DES        D1EsigibD
197200170807     C                   else
197300170810     C                   SetOn                                        6399
197400170807     C                   endif
197500170807     C*
197600170807     C                   ENDSR
197700170807     C/EJECT
197800130918     C************************************************************
197900130918     C* Controllo annullato
198000130918     C************************************************************
198100130918     C     CtrAnn        BEGSR
198200130918     C*
198300130918     C                   if        D1Ann      <> XScOn    and
198400130918     C                             D1Ann      <> XScOff
198500170807     C                   SetOn                                        5999
198600130918     C                   endif
198700130918     C*
198800130918     C                   ENDSR
198900130918     C/EJECT
199000130918     C***********************************************************
199100130918     C* Controllo tipo annullamento
199200130918     C***********************************************************
199300130918     C     CtrTpAnn      BEGSR
199400130918     C*
199500130918     C                   if        D1Ann      =  XScOn
199600130918     C                   Clear                   X07ValDs
199700130918     C                   Move      '1'           X07Ric
199800130918     C                   Move      *Off          X07All
199900170803     C                   MoveL     'SDGCST000'   X07TRc
200000170803     C                   MoveL     'CSTTPANN'    X07Cam
200100130918     C                   MoveL     D1TpAnn       X07VAL
200200130918     C                   CALLB     'X07VALR'
200300130918     C                   PARM                    X07ValDs
200400130918     C*
200500130918     C     X07ERR        IFEQ      *Off
200600130918     C                   MoveL     X07DES        D1TpAnnD
200700130918     C                   else
200800170807     C                   SetOn                                        6099
200900130918     C                   endif
201000130918     C                   else
201100130918     C                   Clear                   D1TpAnn
201200130918     C                   endif
201300130918     C*
201400130918     C                   ENDSR
201500130918     C/EJECT
201600130911     C***********************************************************
201700130911     C* Controllo esistenza record 1
201800130911     C***********************************************************
201900130911     C     CtrExist1     BEGSR
202000130911     C*
202100170807     C                   Eval      CSTTpCom    = TpComI76
202200170807     C                   Eval      CSTTpRec    = D1TpRec
202300170807     C                   Eval      CSTSys      = $Sys
202400170807     C                   Eval      CSTNrAsReg  = $NrAsReg
202500170807     C                   Eval      CSTNrRigaM  = $NrRigaM
202600170807     C                   Eval      CSTNrRiga   = $NrRiga
202700000000     C*
202800170803     C     K05CST01      SETLL     SDGCST01L                              21
202900000000     C     *IN21         IFEQ      '1'
203000130909     C                   SetOn                                        9099
203100000000     C                   ENDIF
203200000000     C*
203300000000     C                   ENDSR
203400000000     C/EJECT
203500130909     C************************************************************
203600130909     C* GESTIONE VIDEO RECORD  D2
203700130909     C************************************************************
203800130909     C     GESD2         BEGSR
203900130909     C* emissione piede videata se proveniente da altra
204000130909     C* salvataggio valore formato di provenienza
204100130909     C     $LASTV        IFNE      'D2'
204200130909     C                   WRITE     FMTT1
204300130909     C                   EXSR      GESPIE
204400130909     C                   MOVE      $LASTV        $LASTG
204500130909     C                   MOVE      'D2'          $LASTV
204600130909     C                   ENDIF
204700000000     C*
204800000000     C* se esistono errori sulla videata
204900000000     C* emetto la write del formato a indicatori spenti per vedere
205000000000     C* le eventuali decodifiche
205100130926   B1C                   if        (*IN99       = *On   or
205200131001   B1C                              *In16       = *On      )  and
205300130926     C                             $GEST        = 'D2'
205400000000     C                   MOVEA     *IN           WIN
205500000000     C                   MOVE      *ALL'0'       IN5098           49
205600000000     C                   MOVEA     IN5098        *IN(50)
205700130930     C                   Eval      WIn16        = *In16
205800130930     C                   Eval      *In16        = *Off
205900000000     C                   WRITE     FMTD2
206000000000     C                   MOVEA     WIN           *IN
206100130930     C                   Eval      *In16        = WIn16
206200000000   E1C                   ENDIF
206300000000     C* Inizializzazione indice tasti funzione personalizzati
206400130909     C                   Eval      PosFun = 25
206500000000     C*
206600000000   B1C                   SELECT
206700000000     C* Annullamento
206800170803    >C     OPZI76        WHENEQ    '04'
206900000000     C* Visualizzazione
207000170803    >C     OPZI76        OREQ      '05'
207100000000     C                   WRITE     FMTD2
207200000000     C                   EXFMT     PROTECT
207300000000     C* Immissione/Modifica/Copia
207400000000   X1C                   OTHER
207500000000     C                   EXFMT     FMTD2
207600000000   E1C                   ENDSL
207700000000     C*
207800000000     C* azzero la posizione del cursore
207900000000     C                   Z-ADD     0             H1RIGA
208000000000     C                   Z-ADD     0             H1COLO
208100000000     C*
208200000000     C* controllo abilitazione tasti funzione
208300130909     C                   EXSR      CTRKD1
208400000000     C*
208500130909   B1C     *IN99         IFEQ      *Off
208600000000     C*
208700000000   B2C                   SELECT
208800000000     C* ROLLUP
208900000000    >C     $TASTO        WHENEQ    ROLLUP
209000000000    >C                   MOVE      'D3'          $GEST
209100000000     C* ROLDOWN
209200000000     C     $TASTO        WHENEQ    ROLDWN
209300000000     C                   MOVE      'D1'          $GEST
209400000000     C* F3=Fine
209500000000     C     $TASTO        WHENEQ    F03
209600130909     C                   EXSR      F03D1
209700000000     C* F4=Ricerca
209800000000     C     $TASTO        WHENEQ    F04
209900130909     C                   EXSR      SEARCH
210000000000     C* F5=Refresh
210100000000     C     $TASTO        WHENEQ    F05
210200130909     C                   EXSR      F05D2
210300000000     C* F8=Successivo
210400000000     C     $TASTO        WHENEQ    F08
210500130909     C                   EXSR      F08D1
210600000000     C* F12=Ritorno
210700000000     C     $TASTO        WHENEQ    F12
210800130909     C                   EXSR      F12D1
210900000000     C* F24=Altri tasti
211000000000     C     $TASTO        WHENEQ    F24
211100130909     C                   EXSR      F24D2
211200000000     C* Personalizzata
211300170803     C* NB chiamata NON possibile perchè SDGSI76DS più lunga
211400130909     C* di KPJBU 256 caratteri                          a
211500000000     C**** $PGF1(PosFun) Whenne    *BLANK
211600130909     C****               EXSR      FperD1
211700000000     C*
211800000000   X2C                   OTHER
211900130909     C                   EXSR      CTRD2
212000130909   B3C     *IN99         IFEQ      *Off
212100130909     C                   EXSR      CTRD1
212200130926   B1C                   if        *IN99       = *On
212300000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
212400000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
212500000000     C* videate
212600000000     C                   MOVE      *ALL'0'       IN5098           49
212700000000     C                   MOVEA     IN5098        *IN(50)
212800130909     C                   SetOn                                        9996                       96
212900000000     C                   ENDIF
213000000000     C                   ENDIF
213100130909   B3C     *IN99         IFEQ      *Off
213200130923     C                   EXSR      CTRD3
213300130926   B1C                   if        *IN99       = *On
213400000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
213500000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
213600000000     C* videate
213700000000     C                   MOVE      *ALL'0'       IN5098           49
213800000000     C                   MOVEA     IN5098        *IN(50)
213900130909     C                   SetOn                                        9996                       96
214000000000     C                   ENDIF
214100000000     C                   ENDIF
214200000000     C*
214300000000     C*
214400000000   B3C     $TASTO        IFEQ      F06
214500130909     C     *IN99         ANDEQ     *Off
214600000000     C* eseguo aggiornamento
214700130909     C                   EXSR      AGGANA
214800000000     C* eseguo operazioni del dopo-aggiornamento
214900130909     C     *IN99         IFEQ      *Off
215000130909     C                   EXSR      GESAGG
215100130909     C                   ENDIF
215200130909     C                   ENDIF
215300130909     C*
215400130909     C                   ENDSL
215500130909     C*
215600130909     C                   ENDIF
215700130909     C*
215800130909     C                   ENDSR
215900130909     C/EJECT
216000130909     C************************************************************
216100130909     C* RIEMPIMENTO VIDEATA D2
216200130909     C************************************************************
216300130909     C     RIED2         BEGSR
216400130909     C*
216500170803     C                   if        CSTNoIva    = '1'
216600130909     C                   Eval      D1NoIva     = XScOn
216700130909     C                   else
216800130909     C                   Eval      D1NoIva     = XScOff
216900130909     C                   endif
217000130923     C*
217100170803     C                   Eval      D1Sogg      = CSTSogg
217200170804     C                   Eval      D1TpN1      = CSTTpN1
217300170803     C                   Eval      D1Kcc       = CSTKcc
217400170803     C                   Eval      D1Ksc       = CSTKsc
217500170803     C                   Eval      D1Causale   = CSTCausale
217600170803     C                   Eval      D1Aliq      = CSTAliq
217700170803     C                   Eval      D1RifIVA    = CSTRifIVA
217800170803     C                   Eval      D1TpReg     = CSTTpReg
217900170803     C                   Eval      D1Libro     = CSTLibro
218000130923     C*
218100170803     C                   MoveL     CSTDtDoc      WDtDoc
218200170803     C                   if        CSTDtDoc    = *Loval
218300130923     C                   Eval      D1DtDoc     = 0
218400130923     C                   else
218500170803     C     *JobRun       Move      CSTDtDoc      D1DtDoc
218600130923     C                   endif
218700130923     C*
218800170803     C                   Eval      D1NrDoc     = CSTNrDoc
218900170803     C                   Eval      D1SerDoc    = CSTSerDoc
219000130923     C*
219100170803     C                   MoveL     CSTDtReg      WDtReg
219200170803     C                   if        CSTDtReg    = *Loval
219300130923     C                   Eval      D1DtReg     = 0
219400130923     C                   else
219500170803     C     *JobRun       Move      CSTDtReg      D1DtReg
219600130923     C                   endif
219700130923     C*
219800170803     C                   Eval      D1NrReg     = CSTNrReg
219900170803     C                   Eval      D1SerReg    = CSTSerReg
220000130923     C*
220100170803     C                   MoveL     CSTDtPar      WDtPar
220200170803     C                   if        CSTDtPar    = *Loval
220300130923     C                   Eval      D1DtPar     = 0
220400130923     C                   else
220500170803     C     *JobRun       Move      CSTDtPar      D1DtPar
220600130923     C                   endif
220700130923     C*
220800170803     C                   Eval      D1NrPar     = CSTNrPar
220900170803     C                   Eval      D1SerPar    = CSTSerPar
221000130923     C*
221100170815fm5  C                   if        OpzI76     <> '03'
221200170803     C                   Eval      D1Sys       = CSTSys
221300170803     C                   Eval      D1NrAsReg   = CSTNrAsReg
221400170815fm5  C                   endif
221500110902     C*
221600170803     C                   Eval      D1Origine   = CSTOrigine
221700000000     C*
221800000000     C                   ENDSR
221900000000     C/EJECT
222000000000     C************************************************************
222100000000     C* GESTIONE F05 VIDEO D2
222200000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
222300000000     C* e il riposizionamento alla prima
222400000000     C************************************************************
222500000000    >C     F05D2         BEGSR
222600000000     C*
222700000000     C                   MOVE      *ON           $INZD1
222800000000     C                   MOVE      'D1'          $GEST
222900000000     C*
223000000000    >C                   ENDSR
223100000000     C************************************************************
223200000000     C* GESTIONE F24 VIDEO D2
223300000000     C************************************************************
223400000000    >C     F24D2         BEGSR
223500000000     C*
223600000000     C                   MOVEA     $FX1          $FX
223700000000     C                   MOVEA     $FC1          $FC
223800000000     C                   MOVEA     $FR1          $FR
223900000000     C                   Z-ADD     $ULKD1        $ULK
224000000000     C                   Z-ADD     $D1L01        $L01
224100000000     C                   Z-ADD     $D1L02        $L02
224200130909     C                   EXSR      RIEKEY
224300000000     C                   Z-ADD     $ULK          $ULKD1            3 0
224400000000     C                   MOVEA     $FC           $FC1
224500130909     C                   MoveL     $KEY1         Z1KE1
224600130909     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
224700130909     C                   MoveL     $KEY2         Z1KE2
224800000000     C                   WRITE     FMTZ1
224900000000     C*
225000000000    >C                   ENDSR
225100000000     C************************************************************
225200000000     C* CONTROLLO VIDEATA  D2
225300000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
225400000000     C* 95 96 97 98
225500000000     C************************************************************
225600000000    >C     CTRD2         BEGSR
225700000000     C*
225800130909     C                   SetOff                                       99
225900000000     C*
226000000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
226100000000     C*
226200170803    >C     OPZI76        IFNE      '04'
226300130918     C*
226400130918     C                   EXSR      CtrSogg
226500170804     C                   EXSR      CtrTpN1
226600130918     C                   EXSR      CtrConto
226700130918     C*
226800130918     C                   EXSR      CtrCausale
226900130918     C                   EXSR      CtrNoIVA
227000130918     C                   EXSR      CtrAliq
227100130918     C                   EXSR      CtrRifIVA
227200130918     C*
227300130918     C                   if        (D1Aliq     <> 0       and
227400130918     C                              D1RifIVA   <> *Blank      )
227500130918     C*
227600130918     C* PRO0187 I dati sono in alternativa
227700130918     C                   SetOn                                        5299
227800130918     C                   endif
227900130918     C*
228000130918     C                   EXSR      CtrTpReg
228100130918     C                   EXSR      CtrLibro
228200130918     C*
228300170803     C                   EXSR      CtrDoc
228400130924     C*
228500130918     C                   EXSR      CtrReg
228600130924     C*
228700170803     C                   EXSR      CtrPar
228800170804     C*
228900170804     C                   EXSR      CtrAnnot
229000170807     C*
229100170807     C                   EXSR      CtrRifObb
229200130918     C*
229300170803     C                   if        OPZI76      = '01'   or
229400170803     C                             OPZI76      = '03'
229500130918     C*
229600170815fm5  C                   if        $NrAsReg    = 0
229700130918     C* Reperisco il numero assoluto della registrazione fittizia
229800130918     C                   EXSR      RepNrAss
229900130918     C                   endif
230000130918     C*
230100130918     C                   endif
230200130918     C*
230300130918     C                   EXSR      CtrOrigine
230400130909     C*
230500130909     C                   ENDIF
230600130909     C*
230700130909     C                   ENDSR
230800130909     C/EJECT
230900130918     C***********************************************************
231000130918     C* Controllo codice soggetto
231100130918     C***********************************************************
231200130918     C     CtrSogg       BEGSR
231300130918     C*
231400130918     C                   Clear                   D1SoggD
231500130918     C                   if        D1Sogg     <> *Blank
231600130918     C*
231700130918     C     D1SOGG        IFNE      *BLANK
231800130918     C                   CALLb     'XALLINEA'
231900130918     C                   PARM                    D1SOGG
232000130918     C                   PARM      8             LEN               5 0
232100130918     C                   PARM                    XSCINP
232200130918     C                   ENDIF
232300130918     C*
232400130918     C                   MoveL     D1Sogg        WSogg
232500130918     C                   EXSR      CallDvaSog
232600130918     C*
232700130918     C                   if        DVAErrore  <> '1'
232800130918     C*
232900130918     C                   Eval      %subst(DVASOG:1:DVALENOUT)
233000130918     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
233100130918     C*
233200130918     C                   MoveL     DVSDes        D1SoggD
233300130918     C*
233400130918     C                   else
233500130918     C*
233600131001     C                   SetOn                                        16
233700130918     C*
233800130918     C                   endif
233900130918     C*
234000130918     C                   endif
234100130918     C*
234200130918     C                   ENDSR
234300130918     C/EJECT
234400170804     C************************************************************
234500170804     C* Chiamata al driver dei soggetti
234600170804     C************************************************************
234700170804     C     CALLDvaSog    BEGSR
234800170804     C*
234900170804     C                   Eval      DVALenOut = %Size(DVASOG)
235000170804     C*
235100170804     C                   CALLB     'NDDVASOG'
235200170804     C                   PARM      '1'           DVARIC
235300170804     C                   PARM      XScSoc        DVASOCIETA
235400170804     C                   PARM                    DVAUNITA
235500170804     C                   PARM      'DVASOG    '  DVASTRUTT
235600170804     C                   PARM                    DVADTRIF
235700170804     C                   PARM                    DVALENOUT
235800170804     C* Se si accede con il codice cli/for impostare i campi seguenti
235900170804     C* se si accede con il soggetto ometterli e impostare gli altri
236000170804     C*****              PARM                    DVASNATURA
236100170804     C*****              PARM                    DVACODICE
236200170804     C*****              PARM                    DVALINEAV
236300170804     C*****              PARM                    DVAFILIALE
236400170804     C                   PARM                    *omit
236500170804     C                   PARM                    *omit
236600170804     C                   PARM                    *omit
236700170804     C                   PARM                    *omit
236800170804     C                   PARM      WSogg         DVSSOGG
236900170804     C                   PARM      *Blank        DVSTPIND
237000170804     C                   PARM      *Blank        DVSCDIND
237100170804     C                   PARM                    DVAERRORE
237200170804     C                   PARM                    DVAOUTPUT
237300170804     C*
237400170804     C                   ENDSR
237500170804     C/EJECT
237600130918     C***********************************************************
237700130918     C* Decofico  la tipologia della sottonatura
237800130918     C***********************************************************
237900170804     C     CtrTpN1       BEGSR
238000130918     C*
238100170804     C                   Clear                   D1TpN1D
238200130918     C                   Clear                   X07ValDs
238300130918     C*
238400130918     C                   Move      '1'           X07Ric
238500130918     C                   Move      *Off          X07All
238600170803     C                   MoveL     'SDGCST000'   X07TRc
238700170804     C                   MoveL     'CSTTPN1  '   X07Cam
238800170804     C                   MoveL     D1TpN1        X07VAL
238900130918     C*
239000130918     C                   CALLB     'X07VALR'
239100130918     C                   PARM                    X07ValDs
239200130918     C*
239300130918     C     X07ERR        IFEQ      *Off
239400170804     C                   MoveL     X07DES        D1TpN1D
239500130918     C                   else
239600130918     C                   SetOn                                        5199
239700130918     C                   endif
239800130918     C*
239900130918     C                   ENDSR
240000130918     C/EJECT
240100130918     C************************************************************
240200130918     C* Controllo esistenza conto collegato a soggetto e con
240300130918     C* la sottonatura indicata
240400130918     C************************************************************
240500130918     C     CtrConto      BEGSR
240600130918     C*
240700130918     C                   Clear                   WKcc
240800130918     C                   Clear                   WKsc
240900130918     C*
241000130918     C                   Clear                   D1KSCD
241100130918     C*
241200130918     C     D1KCC         IFNE      'C'
241300130918     C     D1KCC         ANDNE     'F'
241400130918     C                   CALLB     'XALLINEA'
241500130918     C                   PARM                    D1KCC
241600130918     C                   PARM      6             LEN               5 0
241700130918     C                   PARM                    XSCINP
241800130918     C                   ENDIF
241900130918     C*
242000130918     C                   if        D1Ksc      <> *Blank
242100130918     C*
242200130918     C     D1KSC         IFNE      *Blanks
242300130918     C                   CALLB     'XALLINEA'
242400130918     C                   PARM                    D1KSC
242500130918     C                   PARM      8             LEN               5 0
242600130918     C                   PARM                    XSCINP
242700130918     C                   ENDIF
242800130918     C*
242900130918     C     D1KCC         IFEQ      'C       '
243000130918     C     D1KCC         OREQ      'F       '
243100130918     C                   CALLB     'XCAPCLIFOR'
243200130918     C                   PARM                    XScSoc
243300130918     C                   PARM      D1KCC         D1Kcc
243400130918     C                   PARM                    D1Ksc
243500130918   E1C                   ENDIF
243600130918     C*
243700130918     C                   Eval      WKsc        = D1Ksc
243800130918     C                   Eval      WKcc        = D1Kcc
243900130918     C                   EXSR      CALLMVC002
244000170807     C                   if        Esito      <> *On
244100170807     C                   if        SNatura002 <> 'C' and
244200170807     C                             SNatura002 <> 'F'
244300130918     C                   SetOn                                        5399
244400130918     C                   else
244500130918     C                   MoveL     DesBrev002    D1KSCD
244600130918     C                   MoveL     SNatura002    H1CF
244700130918     C*
244800130918     C                   if        Sogg002    <> D1Sogg   and
244900130918     C                             D1Sogg     <> *Blank
245000130918     C                   Eval      P1SDG0081   = D1Sogg
245100130918     C                   SetOn                                        5499
245200130918     C                   endif
245300130918     C                   endif
245400130918     C*
245500130918     C                   endif
245600130918     C*
245700130918     C                   endif
245800130918     C*
245900130918     C                   ENDSR
246000130918     C/EJECT
246100130918     C***********************************************************
246200130918     C* Driver per controllo conto
246300130918     C***********************************************************
246400130918     C     CALLMvc002    BEGSR
246500130918     C*
246600130918     C                   Clear                   ND002DS
246700130918     C*
246800130918     C                   Eval      LenOut = %Size(ND002DS)
246900130918     C*
247000130918     C                   CALLB     'NDMVC002'
247100130918     C                   PARM                    XScSoc
247200130918     C                   PARM                    WKCC
247300130918     C                   PARM                    WKSC
247400130918     C                   PARM                    Data
247500130918     C                   PARM      *Off          GesMsg
247600130918     C                   PARM      *Off          Esito
247700130918     C                   PARM      'ND002DS'     StrutturaO
247800130918     C                   PARM                    ND002DS
247900130918     C                   PARM                    LenOut
248000130918     C*
248100130918     C                   ENDSR
248200130918     C/EJECT
248300130918     C************************************************************
248400130918     C* Controllo esistenza causale HD2
248500130918     C************************************************************
248600130918     C     CtrCausale    BEGSR
248700130918     C*
248800130919     C                   Clear                   D1CausaleD
248900130918     C*
249000130918     C                   if        D1Causale  <> *Blank
249100170804     C                             and
249200170804     C                             D1DtReg    <> 0      and
249300170804     C                             WDtReg     <> *Loval
249400170804     C*
249500170804     C     WDtReg        AddDur    1:*D          WData
249600170804     C                   Move      WData         WDtAlfa
249700170804     C*
249800170804     C* la causale deve essere valida alla data di registrazione
249900170804     C* passo quindi data di registrazione + un giorno
250000170804     C
250100170804     C*
250200170804     C                   Clear                   ANOPE00F
250300130918     C                   Eval      Lenout = %Size(ANOPE00F)
250400130918     C                   CALLB     'NDMVC003'
250500130918     C                   PARM                    XScSoc
250600130918     C                   PARM                    D1Causale
250700170804     C                   PARM                    WDtAlfa
250800130918     C                   PARM                    Uso
250900130918     C                   PARM      *Off          GesMsg
251000130918     C                   PARM      *Off          Esito
251100130918     C                   PARM      'ANOPE     '  StrutturaO
251200130918     C                   PARM                    ANOPE00F
251300130918     C                   PARM                    LenOut
251400130918     C                   PARM      *Off          Refresh
251500130918     C*
251600130918     C                   if        Esito     = *on
251700130918     C                   SetOn                                        5599
251800130918     C                   else
251900130919     C*
252000130919     C                   Eval      D1CausaleD  = OPEDesBrev
252100130918     C*
252200130918     C* deve essere per registrazioni IVA
252300130918     C                   if        OPETpc    <> '1'
252400130918     C                   SetOn                                        5699
252500130918     C                   endif
252600130918     C                   endif
252700130918     C*
252800130918     C                   endif
252900130918     C*
253000130918     C                   ENDSR
253100130918     C/EJECT
253200130918     C************************************************************
253300130918     C* Controllo movimento senza NDIVA
253400130918     C************************************************************
253500130918     C     CtrNoIVA      BEGSR
253600130918     C*
253700130918     C                   if        D1NoIva    <> XScOn    and
253800130918     C                             D1NoIva    <> XScOff
253900130918     C                   SetOn                                        5899
254000130918     C                   endif
254100130918     C*
254200130918     C                   ENDSR
254300130918     C/EJECT
254400130918     C************************************************************
254500130918     C* Controllo aliquota
254600130918     C************************************************************
254700130918     C     CtrAliq       BEGSR
254800130918     C*
254900130918     C                   if        D1Aliq     <> 0
255000130918     C*
255100130918     C                   Eval      WAliq       = D1Aliq
255200130918     C                   Eval      WRifIVA     = D1RifIVA
255300130918     C*
255400130918     C                   EXSR      CtrANRIV
255500130918     C                   if        WRifIvaOk  <> *On
255600130918     C                   SetOn                                        5999
255700130918     C                   endif
255800130918     C*
255900130918     C                   endif
256000130918     C*
256100130918     C                   ENDSR
256200130918     C/EJECT
256300130918     C************************************************************
256400130918     C* Controllo riferimento IVA
256500130918     C************************************************************
256600130918     C     CtrRifIVA     BEGSR
256700130918     C*
256800130918     C                   if        D1RifIVA   <> *Blank
256900130918     C*
257000130918     C                   Eval      WAliq       = D1Aliq
257100130918     C                   Eval      WRifIVA     = D1RifIVA
257200130918     C*
257300130918     C                   EXSR      CtrANRIV
257400130918     C                   if        WRifIvaOk  <> *On
257500130918     C                   SetOn                                        6099
257600130918     C                   endif
257700130918     C*
257800130918     C                   endif
257900130918     C*
258000130918     C                   ENDSR
258100130918     C/EJECT
258200130918     C************************************************************
258300130918     C* Controllo aliquota e riferimento IVA
258400130918     C************************************************************
258500130918     C     CtrANRIV      BEGSR
258600130918     C*
258700130918     C                   Eval      WRifIvaOk   = *On
258800130918     C*
258900130918     C                   Eval      RIVSocieta  = XscSoc
259000170804     C                   if        H1CF        = 'F'
259100130918     C                   Eval      RIVTpReg    = '1'
259200130918     C                   else
259300130918     C                   Eval      RIVTpReg    = '2'
259400130918     C                   endif
259500130918     C                   Eval      RIVALiq     = WAliq
259600130918     C                   Eval      RIVRifIVA   = WRifIva
259700130918     C*
259800130918     C     K04RIV01      CHAIN     ANRIV01L                           21
259900130918     C                   if        *In21       = *On
260000130918     C                   Eval      WRifIvaOk   = *Off
260100130918     C                   endif
260200130918     C*
260300130918     C                   ENDSR
260400130918     C/EJECT
260500130918     C************************************************************
260600130918     C* Controllo tipo registro IVA
260700130918     C************************************************************
260800130918     C     CtrTpReg      BEGSR
260900130918     C*
261000130918     C                   Clear                   D1TpRegD
261100130918     C                   Clear                   X07ValDs
261200130918     C*
261300130918     C                   if        D1TpReg    <> *blank
261400130918     C                   Move      '1'           X07Ric
261500130918     C                   Move      *On           X07All
261600130918     C                   MoveL     'ANRIV000 '   X07TRc
261700130918     C                   MoveL     'RIVTPREG'    X07Cam
261800130918     C                   MoveL     D1TpReg       X07VAL
261900130918     C*
262000130918     C                   CALLB     'X07VALR'
262100130918     C                   PARM                    X07ValDs
262200130918     C*
262300130918     C     X07ERR        IFEQ      *Off
262400130918     C                   MoveL     X07DES        D1TpRegD
262500130918     C                   else
262600170804     C                   SetOn                                        6199
262700130918     C                   endif
262800130918     C*
262900130918     C                   endif
263000130918     C*
263100130918     C                   ENDSR
263200130918     C/EJECT
263300130918     C************************************************************
263400130918     C* CONTROLLO libro (esistente nel registro)
263500130918     C************************************************************
263600130918     C     CtrLibro      BEGSR
263700130918     C*
263800130918     C                   Eval      D1LibroD    = *Blank
263900130918     C*
264000130918     C                   if        D1Libro    <> *Blank
264100130918     C*
264200130918     C                   Eval      DLISocieta  = XScSoc
264300130918     C                   Eval      DLITpReg    = D1TpReg
264400130918     C                   Eval      DLILibro    = D1Libro
264500130918     C     K03DLI01      CHAIN     ANDLI01L                           21
264600130918     C*
264700130918     C                   if        *In21       = *On
264800130918     C                   SetOn                                        6399
264900130918     C                   else
265000130918     C                   Eval      D1LibroD    = DLIDESBREV
265100130918     C                   endif
265200130918     C*
265300130918     C                   endif
265400130918     C*
265500130918     C                   if        D1Libro      <> *Blanks  and
265600130918     C                             D1TpReg       = *Blanks
265700130918     C                   SetOn                                        6499
265800130918     C                   endif
265900130918     C*
266000130918     C                   ENDSR
266100130918     C/EJECT
266200170803     C************************************************************
266300170803     C* Controllo documento
266400170803     C************************************************************
266500170803     C     CtrDoc        BEGSR
266600170803     C*
266700130918     C                   Eval      WDtDoc      = *Loval
266800130918     C                   if        D1DtDoc    <> 0
266900130918     C     *JobRun       Test(d)                 D1DtDoc                21
267000130918     C*
267100130918     C                   if        *In21       = *Off
267200130918     C     *JobRun       Move      D1DtDoc       WDtDoc
267300130918     C*
267400130918     C                   else
267500130918     C                   SetOn                                        6699
267600130918     C                   endif
267700130918     C*
267800130918     C                   endif
267900130918     C*
268000130918     C                   ENDSR
268100130918     C/EJECT
268200130918     C************************************************************
268300130918     C* Controllo registrazione
268400130918     C************************************************************
268500130918     C     CtrReg        BEGSR
268600130918     C*
268700130918     C                   Eval      WDtReg      = *Loval
268800130918     C*
268900130918     C                   if        D1DtReg    <> 0
269000130918     C     *JobRun       Test(d)                 D1DtReg                21
269100130918     C*
269200130918     C                   if        *In21       = *Off
269300130918     C     *JobRun       Move      D1DtReg       WDtReg
269400130918     C*
269500130918     C                   else
269600130918     C                   SetOn                                        6999
269700130918     C                   endif
269800130918     C*
269900130918     C                   endif
270000130918     C*
270100130918     C                   if        D1SerReg   <> *Blank
270200130918     C                   Clear                   SDGTABDS
270300130918     C                   Move      '1'           XTbRic
270400130918     C                   Move      XScSoc        XTbAzi
270500130918     C                   Move      *Zero         XTbKey
270600130918     C                   Move      'G13'         XTbCod
270700130918     C                   Move      D1SerReg      XTbKey
270800130918     C                   CALL      'SDGTABDR'
270900130918     C                   PARM                    SDGTABDS
271000130918     C     XTbErr        IFEq      '1'
271100130918     C                   SetOn                                        7199
271200130918     C                   endif
271300130918     C                   endif
271400130918     C*
271500130918     C                   ENDSR
271600130918     C/EJECT
271700130918     C************************************************************
271800130918     C* Controllo partita
271900130918     C************************************************************
272000170804     C     CtrPar        BEGSR
272100130918     C*
272200130918     C                   Eval      WDtPar      = *Loval
272300130918     C*
272400130918     C                   if        D1DtPar    <> 0
272500130918     C     *JobRun       Test(d)                 D1DtPar                21
272600130918     C*
272700130918     C                   if        *In21       = *Off
272800130918     C     *JobRun       Move      D1DtPar       WDtPar
272900130918     C                   else
273000130918     C                   SetOn                                        7399
273100130918     C                   endif
273200130918     C*
273300130918     C                   endif
273400130918     C*
273500130918     C                   ENDSR
273600130918     C/EJECT
273700170804     C************************************************************
273800170804     C* Controllo data annotazione
273900170804     C************************************************************
274000170804     C     CtrAnnot      BEGSR
274100170804     C*
274200170804     C                   Eval      WDtAnnot    = *Loval
274300170804     C*
274400170804     C                   if        D1DtAnnot  <> 0
274500170804     C     *JobRun       Test(d)                 D1DtAnnot              21
274600170804     C*
274700170804     C                   if        *In21       = *Off
274800170804     C     *JobRun       Move      D1DtAnnot     WDtAnnot
274900170804     C                   else
275000170807     C                   SetOn                                        5799
275100170804     C                   endif
275200170804     C*
275300170804     C                   endif
275400170804     C*
275500170804     C                   ENDSR
275600170804     C/EJECT
275700170807     C************************************************************
275800170807     C* Controllo riferimenti obbligatori
275900170807     C************************************************************
276000170807     C     CtrRifObb     BEGSR
276100170807     C*
276200170807     C                   if        CFI76       = 'C'
276300170809     C*
276400170809     C* si comunicano sempre i dati del documento
276500170809     C*
276600170809     C                   if        D1DtDoc     = 0
276700170809     C                   SetOn                                        6599
276800170809     C                   endif
276900170809     C*
277000170809     C                   if        D1NrDoc     = 0
277100170809     C                   SetOn                                        6299
277200170809     C                   endif
277300170809     C*
277400170807     C*
277500170809     C                   Select
277600170807     C*
277700170807     C*----
277800170807     C* si comunicano i dati per Registrazione
277900170809     C                   When      DtRifCA12   = 'R'
278000170807     C*
278100170809     C                   if        D1DtReg     = 0
278200170809     C                   SetOn                                        6899
278300170809     C                   endif
278400170807     C*
278500170809     C***                if        D1NrReg     = 0
278600170809     C***                SetOn                                        7099
278700170809     C***                endif
278800170807     C*----
278900170807     C* si comunicano i dati per Annotazione (Regime IVA Trasportatori)
279000170809     C                   When      DtRifCA12   = 'A'
279100170807     C*
279200170807     C* Data annotazione obbligatoria
279300170809     C                   if        D1DtAnnot   = 0
279400170809     C                   SetOn                                        7299
279500170809     C                   endif
279600170807     C*
279700170807     C* Numero registrazione obbligatorio
279800170809     C***                if        D1NrReg     = 0
279900170809     C***                SetOn                                        7099
280000170809     C***                endif
280100170807     C*
280200170809     C                   endSl
280300170807     C*
280400170807     C                   endif
280500170807     C*
280600170807     C                   if        CFI76       = 'F'
280700170807     C*
280800170807     C                   if        D1DtDoc     = 0
280900170807     C                   SetOn                                        6599
281000170807     C                   endif
281100170807     C*
281200170807     C                   if        D1NrDoc     = 0
281300170807     C                   SetOn                                        6299
281400170807     C                   endif
281500170807     C*
281600170807     C                   if        D1DtReg     = 0
281700170807     C                   SetOn                                        6899
281800170807     C                   endif
281900170807     C*
282000170807     C                   endif
282100170807     C*
282200170807     C                   ENDSR
282300170807     C/EJECT
282400130918     C***********************************************************
282500130918     C* Reperisco il numero assoluto di registrazione
282600130918     C***********************************************************
282700130918     C     RepNrAss      BEGSR
282800130918     C*
282900130918     C                   CALLB     'NDMVC101'
283000130918     C                   PARM      *Off          $Esito
283100130918     C                   PARM      0             $Sys
283200130918     C                   PARM      0             $NrAsReg
283300130918     C                   PARM      '1'           $Prenot
283400130918     C*
283500130918     C                   If        $Esito     =  *On
283600170803    >C                   MOVE      '1'           ERRI76
283700130918     C                   MOVE      *on           $Fine
283800130918     C                   EXSR      ENDPGM
283900130918     C                   endif
284000130918     C*
284100130918     C                   Eval      $NrRigaM    = 1
284200130918     C                   Eval      $NrRiga     = 1
284300130918     C*
284400130918     C                   ENDSR
284500130918      /EJECT
284600130918     C************************************************************
284700130918     C* Controllo origine record
284800130918     C************************************************************
284900130918     C     CtrOrigine    BEGSR
285000130918     C*
285100130918     C                   Clear                   D1OrigineD
285200130918     C                   Clear                   X07ValDs
285300130918     C*
285400130918     C                   Move      '1'           X07Ric
285500130918     C                   Move      *On           X07All
285600170803     C                   MoveL     'SDGCST000'   X07TRc
285700170803     C                   MoveL     'CSTORIGINE'  X07Cam
285800130918     C                   MoveL     D1Origine     X07VAL
285900130918     C*
286000130918     C                   CALLB     'X07VALR'
286100130918     C                   PARM                    X07ValDs
286200130918     C*
286300130918     C     X07ERR        IFEQ      *Off
286400130918     C                   MoveL     X07DES        D1OrigineD
286500130918     C                   else
286600130918     C                   SetOn                                        7599
286700130918     C                   endif
286800130918     C*
286900130918     C                   ENDSR
287000130918     C/EJECT
287100000000     C************************************************************
287200000000     C* GESTIONE VIDEO RECORD  D3
287300000000     C************************************************************
287400000000    >C     GESD3         BEGSR
287500000000     C* emissione piede videata se proveniente da altra
287600000000     C* salvataggio valore formato di provenienza
287700000000   B1C     $LASTV        IFNE      'D3'
287800000000     C                   WRITE     FMTT1
287900130909     C                   EXSR      GESPIE
288000000000     C                   MOVE      $LASTV        $LASTG
288100000000     C                   MOVE      'D3'          $LASTV
288200000000   E1C                   ENDIF
288300000000     C*
288400000000     C* se esistono errori sulla videata
288500000000     C* emetto la write del formato a indicatori spenti per vedere
288600000000     C* le eventuali decodifiche
288700131001   B1C                   if        (*IN99       = *On   or
288800131001   B1C                              *In17       = *On      )  and
288900131001     C                             $GEST        = 'D3'
289000000000     C                   MOVEA     *IN           WIN
289100000000     C                   MOVE      *ALL'0'       IN5098           49
289200131001     C                   Eval      WIn17        = *In17
289300131001     C                   Eval      *In17        = *Off
289400000000     C                   MOVEA     IN5098        *IN(50)
289500000000     C                   WRITE     FMTD3
289600000000     C                   MOVEA     WIN           *IN
289700131001     C                   Eval      *In17        = WIn17
289800000000   E1C                   ENDIF
289900000000     C* Inizializzazione indice tasti funzione personalizzati
290000130909     C                   Eval      PosFun = 25
290100000000     C*
290200000000   B1C                   SELECT
290300000000     C* Annullamento
290400170803    >C     OPZI76        WHENEQ    '04'
290500000000     C* Visualizzazione
290600170803    >C     OPZI76        OREQ      '05'
290700130923     C*
290800130923     C                   WRITE     FMTD3
290900130923     C                   EXFMT     PROTECT
291000110901     C*
291100000000     C* Immissione/Modifica/Copia
291200000000   X1C                   OTHER
291300130923     C                   EXFMT     FMTD3
291400000000   E1C                   ENDSL
291500000000     C*
291600000000     C* azzero la posizione del cursore
291700000000     C                   Z-ADD     0             H1RIGA
291800000000     C                   Z-ADD     0             H1COLO
291900000000     C*
292000000000     C* controllo abilitazione tasti funzione
292100130909     C                   EXSR      CTRKD1
292200000000     C*
292300130909   B1C     *IN99         IFEQ      *Off
292400000000     C*
292500000000   B2C                   SELECT
292600000000     C* ROLDOWN
292700000000     C     $TASTO        WHENEQ    ROLDWN
292800000000     C                   MOVE      'D2'          $GEST
292900000000     C* F3=Fine
293000000000     C     $TASTO        WHENEQ    F03
293100130909     C                   EXSR      F03D1
293200000000     C* F4=Ricerca
293300000000     C     $TASTO        WHENEQ    F04
293400130909     C                   EXSR      SEARCH
293500000000     C* F5=Refresh
293600000000     C     $TASTO        WHENEQ    F05
293700130909     C                   EXSR      F05D3
293800000000     C* F8=Successivo
293900000000     C     $TASTO        WHENEQ    F08
294000130909     C                   EXSR      F08D1
294100000000     C* F12=Ritorno
294200000000     C     $TASTO        WHENEQ    F12
294300130909     C                   EXSR      F12D1
294400000000     C* F24=Altri tasti
294500000000     C     $TASTO        WHENEQ    F24
294600130909     C                   EXSR      F24D3
294700000000     C* Personalizzata
294800170803     C* NB chiamata NON possibile perchè SDGSI76DS più lunga
294900130909     C* di KPJBU 256 caratteri                          a
295000000000     C**** $PGF1(PosFun) Whenne    *BLANK
295100130909     C****               EXSR      FperD1
295200000000     C*
295300000000   X2C                   OTHER
295400130923     C                   EXSR      CTRD3
295500130909   B3C     *IN99         IFEQ      *Off
295600130909     C                   EXSR      CTRD2
295700130926   B1C                   if        *IN99       = *On
295800000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
295900000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
296000000000     C* videate
296100000000     C                   MOVE      *ALL'0'       IN5098           49
296200000000     C                   MOVEA     IN5098        *IN(50)
296300130909     C                   SetOn                                        9996                       96
296400000000     C                   ENDIF
296500000000     C                   ENDIF
296600130909   B3C     *IN99         IFEQ      *Off
296700130909     C                   EXSR      CTRD1
296800130926   B1C                   if        *IN99       = *On
296900000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
297000000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
297100000000     C* videate
297200000000     C                   MOVE      *ALL'0'       IN5098           49
297300000000     C                   MOVEA     IN5098        *IN(50)
297400130909     C                   SetOn                                        9996                       96
297500000000     C                   ENDIF
297600000000     C                   ENDIF
297700000000     C*
297800000000     C*
297900000000   B3C     $TASTO        IFEQ      F06
298000130909     C     *IN99         ANDEQ     *Off
298100000000     C* eseguo aggiornamento
298200130909     C                   EXSR      AGGANA
298300000000     C* eseguo operazioni del dopo-aggiornamento
298400130909   B4C     *IN99         IFEQ      *Off
298500130909     C                   EXSR      GESAGG
298600000000   E4C                   ENDIF
298700000000   E3C                   ENDIF
298800000000     C*
298900000000   E2C                   ENDSL
299000000000     C*
299100000000   E1C                   ENDIF
299200000000     C*
299300000000    >C                   ENDSR
299400000000     C/EJECT
299500000000     C************************************************************
299600000000     C* RIEMPIMENTO VIDEATA D3
299700000000     C************************************************************
299800000000    >C     RIED3         BEGSR
299900000000     C*
300000170803     C                   Eval      D1FileO     = CSTFileO
300100170803     C                   Eval      D1PosizO    = CSTPosizO
300200170803     C                   Eval      D1IdFileO   = CSTIdFileO
300300170803     C                   Eval      D1IdPosO    = CSTIdPosO
300400000000     C*
300500170803     C                   Eval      D1FileR     = CSTFileR
300600170803     C                   Eval      D1PosizR    = CSTPosizR
300700170803     C                   Eval      D1IdFileR   = CSTIdFileR
300800170803     C                   Eval      D1IdPosR    = CSTIdPosR
300900170803     C*
301000170803     C                   Eval      D1FileA     = CSTFileA
301100170803     C                   Eval      D1IdFileA   = CSTIdFileA
301200130924     C*
301300000000    >C                   ENDSR
301400000000     C/EJECT
301500000000     C************************************************************
301600000000     C* GESTIONE F05 VIDEO D3
301700000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
301800000000     C* e il riposizionamento alla prima
301900000000     C************************************************************
302000000000    >C     F05D3         BEGSR
302100000000     C*
302200000000     C                   MOVE      *ON           $INZD1
302300000000     C                   MOVE      'D1'          $GEST
302400000000     C*
302500000000    >C                   ENDSR
302600000000     C************************************************************
302700000000     C* GESTIONE F24 VIDEO D3
302800000000     C************************************************************
302900000000    >C     F24D3         BEGSR
303000000000     C*
303100000000     C                   MOVEA     $FX1          $FX
303200000000     C                   MOVEA     $FC1          $FC
303300000000     C                   MOVEA     $FR1          $FR
303400000000     C                   Z-ADD     $ULKD1        $ULK
303500000000     C                   Z-ADD     $D1L01        $L01
303600000000     C                   Z-ADD     $D1L02        $L02
303700130909     C                   EXSR      RIEKEY
303800000000     C                   Z-ADD     $ULK          $ULKD1            3 0
303900000000     C                   MOVEA     $FC           $FC1
304000130909     C                   MoveL     $KEY1         Z1KE1
304100130909     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
304200130909     C                   MoveL     $KEY2         Z1KE2
304300000000     C                   WRITE     FMTZ1
304400000000     C*
304500000000    >C                   ENDSR
304600000000     C************************************************************
304700000000     C* CONTROLLO VIDEATA  D3
304800000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
304900000000     C* 95 96 97 98
305000000000     C************************************************************
305100000000    >C     CTRD3         BEGSR
305200000000     C*
305300130909     C                   SetOff                                       99
305400000000     C*
305500000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
305600000000     C*
305700170803    >C     OPZI76        IFNE      '04'
305800130911     C*
305900170807     C                   EXSR      CtrFileO
306000170807     C                   EXSR      CtrFileR
306100170807     C                   EXSR      CtrFileA
306200000000     C*
306300000000     C                   ENDIF
306400000000     C*
306500000000     C                   ENDSR
306600000000     C/EJECT
306700130925     C************************************************************
306800170807     C* Controllo file Ordinario
306900130925     C************************************************************
307000170807     C     CtrFileO      BEGSR
307100130925     C*
307200170807     C* obbligatori se record di Rettifica o di Annullamento
307300170807     C                   if        D1TpRec     = 'R'    or
307400170807     C                             D1TpRec     = 'A'
307500170807     C*
307600170807     C                   if        D1IdFileO   = *Blanks or
307700170807     C                             D1IdPosO    = 0
307800170807     C                   SetOn                                        5099
307900170807     C                   endif
308000170807     C*
308100170807     C                   endif
308200170807     C*
308300170807     C* deve esistere l'Id file dell'Agenzia delle Entrate
308400170807     C                   if        D1IdFileO  <> *Blanks
308500170807     C*
308600170807     C                   Eval      CSFSocieta  = XScSoc
308700170817fm7  C                   Eval      CSFIdFile   = D1IdFileO
308800170807     C     K02CSF04      CHAIN     SDGCSF04L                          21
308900170807     C                   if        *In21       = *On
309000170807     C                   SetOn                                        5199
309100170807     C                   else
309200170817fm7  C*****              Eval      D1FileO     = CSFFile
309300170807     C                   endif
309400170807     C*
309500170807     C                   if        D1IdPosO    = 0
309600170807     C                   SetOn                                        5299
309700170807     C                   endif
309800170807     C*
309900170807     C                   endif
310000170807     C*
310100170807     C                   ENDSR
310200170807     C/EJECT
310300170807     C************************************************************
310400170807     C* Controllo file di Rettifica
310500170807     C************************************************************
310600170807     C     CtrFileR      BEGSR
310700170807     C*
310800170807     C* Non valorizzare se record Ordinario o di Annullamento
310900170807     C                   if        D1TpRec     = 'O'    or
311000170807     C                             D1TpRec     = 'A'
311100170807     C*
311200170807     C                   Eval      D1FileR     = *Blanks
311300170807     C                   Eval      D1PosizR    = 0
311400170807     C                   Eval      D1IdFileR   = *Blanks
311500170807     C                   Eval      D1IdPosR    = 0
311600170807     C*
311700170807     C                   endif
311800170807     C*
311900170807     C* deve esistere l'Id file dell'Agenzia delle Entrate
312000170807     C                   if        D1IdFileR  <> *Blanks
312100170807     C*
312200170807     C                   Eval      CSFSocieta  = XScSoc
312300170817fm7  C                   Eval      CSFIdFile   = D1IdFileR
312400170807     C     K02CSF04      CHAIN     SDGCSF04L                          21
312500170807     C                   if        *In21       = *On
312600170807     C                   SetOn                                        5399
312700170807     C                   else
312800170817fm7  C*****              Eval      D1FileR     = CSFFile
312900170807     C                   endif
313000170807     C*
313100170807     C                   if        D1IdPosR    = 0
313200170807     C                   SetOn                                        5499
313300170807     C                   endif
313400170807     C*
313500170807     C                   endif
313600170807     C*
313700170807     C                   ENDSR
313800170807     C/EJECT
313900170807     C************************************************************
314000170807     C* Controllo file di Annullamento
314100170807     C************************************************************
314200170807     C     CtrFileA      BEGSR
314300170807     C*
314400170807     C* deve esistere l'Id file dell'Agenzia delle Entrate
314500170807     C                   if        D1IdFileA  <> *Blanks
314600170807     C*
314700170807     C                   Eval      CSFSocieta  = XScSoc
314800170817fm7  C                   Eval      CSFIdFile   = D1IdFileA
314900170807     C     K02CSF04      CHAIN     SDGCSF04L                          21
315000170807     C                   if        *In21       = *On
315100170807     C                   SetOn                                        5599
315200170807     C                   else
315300170817fm7  C*****              Eval      D1FileA     = CSFFile
315400170807     C                   endif
315500170807     C*
315600170807     C                   endif
315700170807     C*
315800170807     C                   ENDSR
315900170807     C/EJECT
316000170807     C************************************************************
316100170807     C* AGGIORNAMENTO ANAGRAFICA
316200170807     C************************************************************
316300170807     C     AGGANA        BEGSR
316400170807     C*
316500000000     C                   SELECT
316600170803    >C     OPZI76        WHENEQ    '01'
316700170803    >C     OPZI76        OREQ      '03'
316800130909     C                   EXSR      RIEANA
316900170803    >C                   WRITE     SDGCST000                            22
317000000000     C* se rcd non scrivibile attivo errore
317100000000     C     *IN22         IFEQ      *ON
317200130909     C                   SetOn                                        9599
317300000000     C                   ENDIF
317400170803    >C     OPZI76        WHENEQ    '02'
317500130909     C                   EXSR      RIEANA
317600170803    >C                   UPDATE    SDGCST000                            22
317700170803    >C     OPZI76        WHENEQ    '04'
317800170803    >C                   DELETE    SDGCST000                            21
317900000000     C                   ENDSL
318000000000     C*
318100000000     C* richiama pgm User Exit
318200000000     C     USRERR        IFEQ      *BLANK
318300000000     C* passa opzione ricevuta a user exit
318400170803    >C                   MoveL     OPZI76        XUKOPZ
318500170803     C* imposta chiave univoca  per user exit
318600000000     C                   MOVE      XSCSOC        XUKSOC
318700000000     C                   MOVE      XSCDUN        XUKDUN
318800130909     C                   MoveL     DSPGM         XUKPGM
318900170803    >C                   MoveL     CSTTpCom      XUKFL1
319000170803    >C                   MoveL     CSTTpRec      XUKFL2
319100170803    >C                   MoveL     CSTSys        XUKFL3
319200170803    >C                   MoveL     CSTNrAsReg    XUKFL4
319300170803    >C                   MoveL     CSTNrRigaM    XUKFL5
319400170803    >C                   MoveL     CSTNrRiga     XUKFL6
319500000000    >C* ho fino a 6 campi chiave passabili
319600130909     C                   MoveL     XUKDS         KPJBU
319700000000     C     DSPGM         CAT       'U':0         USREXT           10
319800130909     C                   CALL      USREXT                               21
319900130909     C                   PARM                    KPJBA
320000000000     C     *IN21         IFEQ      *ON
320100000000     C                   MOVE      *ON           USRERR            1
320200000000     C                   ENDIF
320300000000     C                   ENDIF
320400000000     C*
320500000000     C                   ENDSR
320600000000     C/EJECT
320700000000     C************************************************************
320800000000     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
320900000000     C************************************************************
321000000000     C     RIEANA        BEGSR
321100170803     C*
321200170803     C                   if        OPZI76      = '01'   or
321300170803     C                             OPZI76      = '03'
321400170803     C*
321500170803     C* imposto la data e l'utente di immissione
321600170803     C                   Eval      CSTDtImm    = UDateIso
321700170803     C                   Eval      CSTUteImm   = XScCUt
321800170803     C*
321900170803     C                   Eval      CSTSYS      = $Sys
322000170804     C                   Eval      CSTNrAsReg  = $NrAsReg
322100170803     C                   Eval      CSTNrRigaM  = $NrRigaM
322200170803     C                   Eval      CSTNrRiga   = $NrRiga
322300170803     C                   endif
322400170803     C*
322500170803     C* imposto la data e l'utente di ultima modifica
322600170803     C                   Eval      CSTDtUlMo   = UDateIso
322700170803     C                   Eval      CSTUteUlMo  = XScCUt
322800130923     C*
322900170803     C                   if        D1Ann       = XScOn
323000170803     C                   Eval      CSTAnn      = *On
323100170803     C                   else
323200170803     C                   Eval      CSTAnn      = *Blank
323300170803     C                   endif
323400170803     C                   Eval      CSTTpAnn    = D1TpAnn
323500170803     C*
323600170803     C                   Eval      CSTOrigine  = 'M'
323700170803     C*
323800170803     C* imposto i campi del file da ricevuti dal chiamante
323900170803     C                   Eval      CSTSocieta  = XScSoc
324000170803     C                   Eval      CSTTpCom    = TpComI76
324100170807     C                   Eval      CSTTpRec    = D1TpRec
324200170803     C                   Eval      CSTAnno     = AnnoI76
324300170803     C                   Eval      CSTTrim     = TrimI76
324400170803     C                   Eval      CSTMese     = MeseI76
324500170803     C*
324600170803     C                   Eval      CSTPrgEstr  = 0
324700170803     C*
324800170803     C                   Eval      CSTCF       = CFI76
324900170803     C                   Eval      CSTSogg     = D1Sogg
325000170803     C                   Eval      CSTPartIva  = PartIvaI76
325100170803     C                   Eval      CSTCdFisc   = CdFiscI76
325200170803     C                   Eval      CSTStato1   = Stato1I76
325300170803     C                   Eval      CSTIdPaes1  = IdPaes1I76
325400170803     C                   Eval      CSTPIVA1    = PIVA1I76
325500170803     C                   Eval      CSTCdFisc1  = CdFisc1I76
325600170803     C                   Eval      CSTCogRag1  = CogRag1I76
325700170803     C                   Eval      CSTTpN1     = TpN1I76
325800170803     C                   Eval      CSTDenom1   = Denom1I76
325900170803     C                   Eval      CSTCognom1  = Cognom1I76
326000170803     C                   Eval      CSTNome1    = Nome1I76
326100130923     C*
326200170803     C                   Eval      CSTIndir2   = Indir2I76
326300170803     C                   Eval      CSTNrCiv2   = NrCiv2I76
326400170803     C                   Eval      CSTCAP2     = CAP2I76
326500170803     C                   Eval      CSTLocali2  = Locali2I76
326600170803     C                   Eval      CSTProv2    = Prov2I76
326700170803     C                   Eval      CSTStato2   = Stato2I76
326800170803     C                   Eval      CSTIdPaes2  = IdPaes2I76
326900170803     C*
327000170803     C                   Eval      CSTIndir3   = Indir3I76
327100170803     C                   Eval      CSTNrCiv3   = NrCiv3I76
327200170803     C                   Eval      CSTCAP3     = CAP3I76
327300170803     C                   Eval      CSTLocali3  = Locali3I76
327400170803     C                   Eval      CSTProv3    = Prov3I76
327500170803     C                   Eval      CSTStato3   = Stato3I76
327600170803     C                   Eval      CSTIdPaes3  = IdPaes3I76
327700170803     C*
327800170803     C                   Eval      CSTStato4   = Stato4I76
327900170803     C                   Eval      CSTIdPaes4  = IdPaes4I76
328000170803     C                   Eval      CSTPIVA4    = PIVA4I76
328100170803     C                   Eval      CSTCogRag4  = CogRag4I76
328200170803     C                   Eval      CSTTpN4     = TpN4I76
328300170803     C                   Eval      CSTDenom4   = Denom4I76
328400170803     C                   Eval      CSTCognom4  = Cognom4I76
328500170803     C                   Eval      CSTNome4    = Nome4I76
328600000000     C*
328700130923     C*
328800170803     C                   Eval      CSTKcc      = D1Kcc
328900170803     C                   Eval      CSTKsc      = D1Ksc
329000170803     C*
329100170803     C                   Eval      CSTCausale  = D1Causale
329200170803     C                   Eval      CSTAliq     = D1Aliq
329300170803     C                   Eval      CSTRifIVA   = D1RifIVA
329400170803     C*
329500170803     C                   Eval      CSTTpReg    = D1TpReg
329600170803     C                   Eval      CSTLibro    = D1Libro
329700170803     C*
329800170803     C                   Eval      CSTDtDoc    = WDtDoc
329900170803     C                   Eval      CSTNrDoc    = D1NrDoc
330000170803     C                   Eval      CSTSerDoc   = D1SerDoc
330100170803     C*
330200170803     C                   Eval      CSTDtReg    = WDtReg
330300170803     C                   Eval      CSTNrReg    = D1NrReg
330400170803     C                   Eval      CSTSerReg   = D1SerReg
330500170803     C*
330600170803     C                   Eval      CSTDtPar    = WDtPar
330700170803     C                   Eval      CSTNrPar    = D1NrPar
330800170803     C                   Eval      CSTSerPar   = D1SerPar
330900170803     C*
331000170803     C                   Eval      CSTDtAnnot  = WDtAnnot
331100170803     C*
331200170803     C                   Eval      CSTTpDoc    = D1TpDoc
331300170803     C                   Eval      CSTNatOpe   = D1NatOpe
331400170803     C*
331500170803     C                   if        D1NoIva     = XScOn
331600170803     C                   Eval      CSTNoIva    = *On
331700170803     C                   else
331800170803     C                   Eval      CSTNoIva    = *Off
331900170803     C                   endif
332000130923     C*
332100170804     C                   if        D1MotEscl   = 'N5'
332200170803     C                   Eval      CSTMargine  = *On
332300130923     C                   else
332400170803     C                   Eval      CSTMargine  = *Off
332500130923     C                   endif
332600170803     C*
332700170804     C                   if        D1MotEscl   = 'N6'
332800170803     C                   Eval      CSTReverse  = *On
332900130923     C                   else
333000170803     C                   Eval      CSTReverse  = *Off
333100130923     C                   endif
333200170803     C*
333300130923     C                   if        D1Autofat   = XScOn
333400170803     C                   Eval      CSTAutofat  = *On
333500130923     C                   else
333600170803     C                   Eval      CSTAutofat  = *Off
333700130923     C                   endif
333800130923     C*
333900170816fm6  C                   if        D1Intra     = XScOn
334000170816 "   C                   Eval      CSTIntra    = *On
334100170816 "   C                   else
334200170816 "   C                   Eval      CSTIntra    = *Off
334300170816fm6  C                   endif
334400130923     C*
334500170803     C                   Eval      CSTImpOpe   = D1ImpOpe
334600170803     C                   Eval      CSTIvaOpe   = D1IvaOpe
334700130923     C*
334800170803     C                   Eval      CSTImpOpeO  = D1ImpOpeO
334900170803     C                   Eval      CSTIvaOpeO  = D1IvaOpeO
335000170803     C*
335100170803     C                   Eval      CSTMotEscl  = D1MotEscl
335200170803     C                   Eval      CSTPercDet  = D1PercDet
335300170803     C                   if        D1Deducib   = XScOn
335400170803     C                   Eval      CSTDeducib  = *On
335500170803     C                   else
335600170803     C                   Eval      CSTDeducib  = *Off
335700170803     C                   endif
335800170803     C                   Eval      CSTEsigib   = D1Esigib
335900130923     C*
336000170803     C                   Eval      CSTFileO    = D1FileO
336100170803     C                   Eval      CSTPosizO   = D1PosizO
336200170803     C                   Eval      CSTIdFileO  = D1IdFileO
336300170803     C                   Eval      CSTIdPosO   = D1IdPosO
336400170803     C*
336500170803     C                   Eval      CSTFileR    = D1FileR
336600170803     C                   Eval      CSTPosizR   = D1PosizR
336700170803     C                   Eval      CSTIdFileR  = D1IdFileR
336800170803     C                   Eval      CSTIdPosR   = D1IdPosR
336900170803     C*
337000170803     C                   Eval      CSTFileA    = D1FileA
337100170803     C                   Eval      CSTIdFileA  = D1IdFileA
337200000000     C*
337300000000     C                   ENDSR
337400000000     C/EJECT
337500000000     C************************************************************
337600000000     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
337700000000     C************************************************************
337800000000     C     GESAGG        BEGSR
337900000000     C*
338000000000     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
338100170803    >C     CMTI76        IFEQ      '0'
338200000000     C********           COMMIT
338300000000     C                   ENDIF
338400000000     C*
338500000000     C* segnalo al pgm chiamante l'avvenuta conferma
338600170803    >C                   MOVE      *ON           OPRI76
338700000000     C* segnalo al pgm chiamante l'aver premuto l'F6
338800170803    >C                   MOVE      '0'           RETI76
338900000000     C*
339000000000     C                   SELECT
339100000000     C*
339200000000     C* nel caso di immissione
339300170803    >C     OPZI76        WHENEQ    '01'
339400000000     C* ripristino la videata iniziale
339500000000     C                   MOVE      *ON           $INZD1
339600000000     C                   MOVE      'D1'          $GEST
339700000000     C*
339800000000     C* nel caso di copia
339900170803    >C     OPZI76        WHENEQ    '03'
340000000000     C* ritorno al pgm chiamante
340100000000     C                   MOVE      *ON           $FINE
340200000000     C*
340300000000     C* nel caso di modifica
340400170803    >C     OPZI76        WHENEQ    '02'
340500000000     C* ripasso i campi che devono essere reimpostati sul sfl
340600170807     C                   Eval      TpRecI76    = D1TpRec
340700170803     C                   Eval      KccI76      = D1Kcc
340800170803     C                   Eval      KscI76      = D1Ksc
340900170803     C                   Eval      DtDocI76    = WDtDoc
341000170803     C                   Eval      NrDocI76    = D1NrDoc
341100170803     C                   Eval      SerDocI76   = D1SerDoc
341200170803     C                   Eval      DtRegI76    = WDtReg
341300170803     C                   Eval      NrRegI76    = D1NrReg
341400170803     C                   Eval      SerRegI76   = D1SerReg
341500170804     C                   Eval      DtAnnotI76  = WDtAnnot
341600170803     C                   Eval      OrigineI76  = D1Origine
341700170803     C                   Eval      ImpOpeI76   = D1ImpOpe
341800170803     C                   Eval      IVAOpeI76   = D1IVAOpe
341900170803     C                   Eval      CausaleI76  = D1Causale
342000170803     C                   Eval      AliqI76     = D1Aliq
342100170803     C                   Eval      RifIVAI76   = D1RifIVA
342200170804     C                   Eval      MotEsclI76  = D1MotEscl
342300170803     C                   Eval      TpDocI76    = D1TpDoc
342400170804     C                   Eval      NatOpeI76   = D1NatOpe
342500170804     C                   Eval      EsigibI76   = D1Esigib
342600170804     C                   if        D1Ann       = XScOn
342700170804     C                   Eval      AnnI76      = *On
342800170804     C                   else
342900170804     C                   Eval      AnnI76      = *Blank
343000170804     C                   endif
343100000000     C* e ritorno al pgm chiamante
343200000000     C                   MOVE      *ON           $FINE
343300000000     C*
343400000000     C* per tutte le altre opzioni
343500000000     C                   OTHER
343600000000     C* ritorno al pgm chiamante
343700000000     C                   MOVE      *ON           $FINE
343800000000     C                   ENDSL
343900000000     C*
344000000000     C                   ENDSR
344100000000     C/EJECT
344200000000     C************************************************************
344300000000     C* CONTROLLO ABILITAZIONE TASTI
344400000000     C************************************************************
344500000000     C     CTRKEY        BEGSR
344600000000     C*
344700000000     C                   SELECT
344800000000     C     $TASTO        WHENEQ    F01
344900000000     C                   Z-ADD     01            PosFun
345000000000     C     $FC(1)        IFEQ      '0'
345100130909     C                   SetOn                                        99
345200000000     C                   ENDIF
345300000000     C     $TASTO        WHENEQ    F02
345400000000     C                   Z-ADD     02            PosFun
345500000000     C     $FC(2)        IFEQ      '0'
345600130909     C                   SetOn                                        99
345700000000     C                   ENDIF
345800000000     C     $TASTO        WHENEQ    F03
345900000000     C                   Z-ADD     03            PosFun
346000000000     C     $FC(3)        IFEQ      '0'
346100130909     C                   SetOn                                        99
346200000000     C                   ENDIF
346300000000     C     $TASTO        WHENEQ    F04
346400000000     C                   Z-ADD     04            PosFun
346500000000     C     $FC(4)        IFEQ      '0'
346600130909     C                   SetOn                                        99
346700000000     C                   ENDIF
346800000000     C     $TASTO        WHENEQ    F05
346900000000     C                   Z-ADD     05            PosFun
347000000000     C     $FC(5)        IFEQ      '0'
347100130909     C                   SetOn                                        99
347200000000     C                   ENDIF
347300000000     C     $TASTO        WHENEQ    F06
347400000000     C                   Z-ADD     06            PosFun
347500000000     C     $FC(6)        IFEQ      '0'
347600130909     C                   SetOn                                        99
347700000000     C                   ENDIF
347800000000     C     $TASTO        WHENEQ    F07
347900000000     C                   Z-ADD     07            PosFun
348000000000     C     $FC(7)        IFEQ      '0'
348100130909     C                   SetOn                                        99
348200000000     C                   ENDIF
348300000000     C     $TASTO        WHENEQ    F08
348400000000     C                   Z-ADD     08            PosFun
348500000000     C     $FC(8)        IFEQ      '0'
348600130909     C                   SetOn                                        99
348700000000     C                   ENDIF
348800000000     C     $TASTO        WHENEQ    F09
348900000000     C                   Z-ADD     09            PosFun
349000000000     C     $FC(09)       IFEQ      '0'
349100130909     C                   SetOn                                        99
349200000000     C                   ENDIF
349300000000     C     $TASTO        WHENEQ    F10
349400000000     C                   Z-ADD     10            PosFun
349500000000     C     $FC(10)       IFEQ      '0'
349600130909     C                   SetOn                                        99
349700000000     C                   ENDIF
349800000000     C     $TASTO        WHENEQ    F11
349900000000     C                   Z-ADD     11            PosFun
350000000000     C     $FC(11)       IFEQ      '0'
350100130909     C                   SetOn                                        99
350200000000     C                   ENDIF
350300000000     C     $TASTO        WHENEQ    F12
350400000000     C                   Z-ADD     12            PosFun
350500000000     C     $FC(12)       IFEQ      '0'
350600130909     C                   SetOn                                        99
350700000000     C                   ENDIF
350800000000     C     $TASTO        WHENEQ    F13
350900000000     C                   Z-ADD     13            PosFun
351000000000     C     $FC(13)       IFEQ      '0'
351100130909     C                   SetOn                                        99
351200000000     C                   ENDIF
351300000000     C     $TASTO        WHENEQ    F14
351400000000     C                   Z-ADD     14            PosFun
351500000000     C     $FC(14)       IFEQ      '0'
351600130909     C                   SetOn                                        99
351700000000     C                   ENDIF
351800000000     C     $TASTO        WHENEQ    F15
351900000000     C                   Z-ADD     15            PosFun
352000000000     C     $FC(15)       IFEQ      '0'
352100130909     C                   SetOn                                        99
352200000000     C                   ENDIF
352300000000     C     $TASTO        WHENEQ    F16
352400000000     C                   Z-ADD     16            PosFun
352500000000     C     $FC(16)       IFEQ      '0'
352600130909     C                   SetOn                                        99
352700000000     C                   ENDIF
352800000000     C     $TASTO        WHENEQ    F17
352900000000     C                   Z-ADD     17            PosFun
353000000000     C     $FC(17)       IFEQ      '0'
353100130909     C                   SetOn                                        99
353200000000     C                   ENDIF
353300000000     C     $TASTO        WHENEQ    F18
353400000000     C                   Z-ADD     18            PosFun
353500000000     C     $FC(18)       IFEQ      '0'
353600130909     C                   SetOn                                        99
353700000000     C                   ENDIF
353800000000     C     $TASTO        WHENEQ    F19
353900000000     C                   Z-ADD     19            PosFun
354000000000     C     $FC(19)       IFEQ      '0'
354100130909     C                   SetOn                                        99
354200000000     C                   ENDIF
354300000000     C     $TASTO        WHENEQ    F20
354400000000     C                   Z-ADD     20            PosFun
354500000000     C     $FC(20)       IFEQ      '0'
354600130909     C                   SetOn                                        99
354700000000     C                   ENDIF
354800000000     C     $TASTO        WHENEQ    F21
354900000000     C                   Z-ADD     21            PosFun
355000000000     C     $FC(21)       IFEQ      '0'
355100130909     C                   SetOn                                        99
355200000000     C                   ENDIF
355300000000     C     $TASTO        WHENEQ    F22
355400000000     C                   Z-ADD     22            PosFun
355500000000     C     $FC(22)       IFEQ      '0'
355600130909     C                   SetOn                                        99
355700000000     C                   ENDIF
355800000000     C     $TASTO        WHENEQ    F23
355900000000     C                   Z-ADD     23            PosFun
356000000000     C     $FC(23)       IFEQ      '0'
356100130909     C                   SetOn                                        99
356200000000     C                   ENDIF
356300000000     C     $TASTO        WHENEQ    F24
356400000000     C                   Z-ADD     24            PosFun
356500000000     C     $FC(24)       IFEQ      '0'
356600130909     C                   SetOn                                        99
356700000000     C                   ENDIF
356800000000     C                   ENDSL
356900000000     C*
357000000000     C                   ENDSR
357100000000     C/EJECT
357200000000     C******************************************************
357300000000     C* PGM PER GESTIONE MESSAGGI ERRORE
357400000000     C******************************************************
357500000000     C     MSGPGM        BEGSR
357600000000     C*
357700000000     C                   Z-ADD     2             MSGMSG
357800130909     C                   MoveL     'S'           MSGRSP
357900130909     C                   MoveL     'CR'          MSGTPR
358000130909     C                   MoveL     'S'           MSGEMV
358100000000     C                   Z-ADD     10            MSGRGP
358200130909     C                   MoveL     'S'           MSGLCK
358300000000     C                   MOVEA     STATUS        STS
358400130909     C                   MoveL     'N'           MSGCNL
358500130909     C                   MoveL     'N'           MSGVID
358600130909     C                   MoveL     'N'           MSGOPE
358700130909     C                   MoveL     'N'           MSGSTP
358800130909     C                   CALL      'SDGMSGR'
358900130909     C                   PARM                    SDGMSGDS
359000000000     C*
359100000000     C                   ENDSR
359200000000     C/EJECT
359300000000     C************************************************************
359400000000     C* OPERAZIONI INIZIALI
359500000000     C************************************************************
359600000000     C     *INZSR        BEGSR
359700000000     C*
359800000000     C* Reperimento parametri
359900000000     C*
360000000000     C     *ENTRY        PLIST
360100130909     C                   PARM                    KPJBA
360200170803     C                   PARM                    SDGSI76DS
360300000000     C*
360400000000     C* Reperimento dati società
360500000000     C*
360600130909     C                   MoveL     'SOC001'      TIPXSC
360700130909     C                   MoveL     *BLANK        SOCXSC
360800130909     C                   EXSR      REPSOC
360900000000     C*
361000000000     C* Controlli dati società
361100000000     C*
361200000000     C     RTNXSC        IFNE      '1'
361300130909     C                   MoveL     XSOCDS        SOC001
361400000000     C                   ELSE
361500170803    >C                   MOVE      '1'           ERRI76
361600000000     C                   MOVE      *on           $Fine
361700130909     C                   EXSR      EndPgm
361800000000     C                   ENDIF
361900000000     C*
362000000000     C                   MOVE      *BLANK        AUTDS
362100000000     C*
362200000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
362300130909     C                   Eval      $D1L01 = %Size(Z1Ke1)
362400130909     C                   Eval      $D1L02 = %Size(Z1Ke2)
362500000000     C*
362600000000     C* Reperimento tasti di funzione
362700000000     C*
362800130909     C                   EXSR      REPKEY
362900000000     C*
363000000000     C* Richiamo routine per aggiunta tasti funzione personalizzati
363100130909     C                   CALLB     'X46USREXT'
363200130909     C                   PARM                    *omit
363300130909     C                   PARM                    *omit
363400130909     C                   PARM                    *omit
363500130909     C                   PARM                    *omit
363600130909     C                   PARM                    *omit
363700130909     C                   PARM                    $FX1
363800130909     C                   PARM                    $FC1
363900130909     C                   PARM                    $PGF1
364000130909     C                   PARM                    DsPgm
364100130909     C                   PARM                    xsccut
364200130909     C                   PARM                    xscgrp
364300000000     C*
364400000000     C     *DMY          Move      UDate         UDateISO
364500130912     C*
364600130912     C* reperisco DATFMT del job
364700130912     C                   CallB     'XDATFMT'
364800130912     C                   Parm                    $DatFmt           4
364900000000     C*
365000000000     C                   ENDSR
365100000000     C/EJECT
365200000000     C************************************************************
365300000000     C* REPERIMENTO TASTI DI FUNZIONE
365400000000     C************************************************************
365500000000     C     REPKEY        BEGSR
365600000000     C*
365700000000     C                   MOVEA     $FM1          $FM
365800000000     C                   MOVEA     $FC1          $FC
365900130909     C                   EXSR      REPKKK
366000000000     C                   MOVEA     $FX           $FX1
366100000000     C*
366200000000     C                   ENDSR
366300000000     C/EJECT
366400000000     C************************************************************
366500000000     C* REPERIMENTO TASTI DI FUNZIONE
366600000000     C************************************************************
366700000000     C     REPKKK        BEGSR
366800000000     C*
366900000000     C                   CLEAR                   $FX
367000000000     C     1             DO        24            X
367100000000     C*
367200000000     C     $FM(X)        IFNE      *BLANK
367300000000     C     $FC(X)        ANDNE     '0'
367400000000     C*
367500130909     C                   MoveL     $FM(X)        IDMSG
367600130909     C                   CALLB     'XRTVM'
367700130909     C                   PARM                    IDMSG            27
367800130909     C                   PARM                    TXTMSG          100
367900130909     C                   PARM                    ERRMSG            1
368000000000     C*
368100000000     C     ERRMSG        IFNE      *ON
368200130909     C                   MoveL     TXTMSG        $FX(X)
368300000000     C                   ELSE
368400130909     C                   MoveL     *ALL' '       $FX(X)
368500000000     C                   ENDIF
368600000000     C*
368700000000     C                   ENDIF
368800000000     C*
368900000000     C                   ENDDO
369000000000     C*
369100000000     C                   ENDSR
369200000000     C/EJECT
369300000000     C************************************************************
369400000000     C* INIZIALIZZAZIONE VARIABILI
369500000000     C************************************************************
369600000000     C     INZVAR        BEGSR
369700000000     C*
369800000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
369900000000     C* reperimento dati società
370000170803    >C                   If        SocietaI76 <> xscsoc    and
370100170803    >C                             OpzI76 <> *blank
370200130909     C                   MoveL     'SOC001'      TIPXSC
370300170803    >C                   MoveL     SocietaI76    SOCXSC
370400130909     C                   EXSR      REPSOC
370500000000     C     RTNXSC        IFNE      '1'
370600130909     C                   MoveL     XSOCDS        SOC001
370700000000     C                   ELSE
370800170803    >C                   MOVE      '1'           ERRI76
370900000000     C                   MOVE      *on           $Fine
371000130909     C                   EXSR      EndPgm
371100000000     C                   ENDIF
371200000000     C                   endif
371300000000     C*
371400000000     C* Reperimento autorizzazioni
371500000000     C*
371600000000     C* le autorizzazioni potrebbero essere già state passate,
371700000000     C* nel qual caso non vanno reperite
371800000000    >C     AUTDS         IFEQ      *BLANK
371900130909    >C                   EXSR      REPAUT
372000000000    >C                   ELSE
372100130909    >C                   MoveL     XCADDS        CONCIVDS
372200000000    >C                   ENDIF
372300000000     C*
372400000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
372500000000     C* le autorizzazione
372600000000     C*
372700130909    >C                   EXSR      TSTAUT
372800000000     C*
372900170803     C* Reperisco i dati di setup del modulo
373000170803     C                   EXSR      RepSDSPETRI
373100000000     C*
373200000000     C* Definizione dei tasti funzione utilizzabili a pgm
373300000000     C*
373400130909     C                   EXSR      DEFOPZ
373500000000     C*
373600000000     C* Variabili per gestione videate
373700000000     C*
373800000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
373900130909     C                   MOVE      *Off          $FINE                          fine pgm
374000130909     C                   MOVE      *Off          $FlgDef                        fine pgm
374100000000     C                   MOVE      *ON           $INZD1                         inizializzione D1
374200000000     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
374300000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
374400000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
374500000000     C* azzero la posizione del cursore
374600000000     C                   Z-ADD     0             H1RIGA
374700000000     C                   Z-ADD     0             H1COLO
374800000000     C*
374900000000     C* Variabili appoggio
375000000000     C*
375100000000     C                   Z-ADD     0             CURR
375200000000     C                   Z-ADD     0             CURC
375300000000     C*
375400000000     C* Valorizzazione campi univoci testate
375500000000     C*
375600000000     C                   CLEAR                   FMTT1
375700130909     C                   MoveL     KNSIF         NOMSIF
375800130909     C                   MoveL     XSCDSI        NOMDIT
375900130909     C                   MoveL     DSPGM         NOMPGM           10
376000130909     C                   MoveL     KCDAZ         NOMAZN
376100000000     C* recupero il sottotitolo (se c'è)
376200130909     C                   MoveL     *BLANK        T1TIT
376300000000     C     SOTTIT        IFNE      *BLANK
376400130909     C                   MoveL     SOTTIT        IDMSG
376500130909     C                   CALLB     'XRTVM'
376600130909     C                   PARM                    IDMSG            27
376700130909     C                   PARM                    TXTMSG          100
376800130909     C                   PARM                    ERRMSG            1
376900130909     C                   PARM      *ON           CTRMSG            1            centratura
377000130909     C                   PARM      30            LENMSG            3 0          lun output.
377100000000     C     ERRMSG        IFNE      *ON
377200130909     C                   MoveL     TXTMSG        T1TIT
377300000000     C                   ELSE
377400130909     C                   MoveL     *ALL'?'       T1TIT
377500000000     C                   ENDIF
377600000000     C                   ENDIF
377700170803     C*
377800170803     C* Recupero il titolo con il tipo di comunicazione
377900170803     C                   MOVEL     *BLANK        T1TIT
378000170803     C*
378100170803     C                   Select
378200170803     C*
378300170803     C* Comunicazione Spesometro trimestrale
378400170803     C                   When      TpComI76   = 'T'
378500170803     C                   if        SOTTITT1   <> *BLANK
378600170803     C                   MOVEL     SOTTITT1      IDMSG
378700170803     C                   endif
378800170803     C*
378900170803     C                   endsl
379000170803     C*
379100170803     C                   CALLB     'XRTVM'
379200170803     C                   PARM                    IDMSG            27
379300170803     C                   PARM                    TXTMSG          100
379400170803     C                   PARM                    ERRMSG            1
379500170803     C                   PARM      *ON           CTRMSG            1            centratura
379600170803     C                   PARM      30            LENMSG            3 0          lun output.
379700170803     C     ERRMSG        IFNE      *ON
379800170803     C                   MOVEL     TXTMSG        T1TIT
379900170803     C                   ELSE
380000170803     C                   MOVEL     *ALL'?'       T1TIT
380100170803     C                   ENDIF
380200170803     C* recupero il tipo operazione
380300170803     C                   Z-ADD     1             X
380400170803     C     OPZI76        LOOKUP    $OZ(X)                                 21
380500000000     C     *IN21         IFEQ      *ON
380600130909     C                   MoveL     $ID(X)        IDMSG
380700130909     C                   CALLB     'XRTVM'
380800130909     C                   PARM                    IDMSG            27
380900130909     C                   PARM                    TXTMSG          100
381000130909     C                   PARM                    ERRMSG            1
381100130909     C                   PARM      *Off          CTRMSG            1            centratura
381200130909     C                   PARM      15            LENMSG            3 0          lun output.
381300000000     C     ERRMSG        IFNE      *ON
381400130909     C                   MoveL     TXTMSG        T1OPE
381500000000     C                   ELSE
381600130909     C                   MoveL     *ALL'?'       T1OPE
381700000000     C                   ENDIF
381800000000     C                   ENDIF
381900000000     C*
382000000000     C* Inizializzazione/Decodifica prima videata
382100130909     C                   EXSR      REPDEF
382200130909     C                   EXSR      INZD1
382300130909     C                   EXSR      F02D1
382400130909     C                   MOVE      *Off          $INZD1
382500000000     C*
382600000000     C                   ENDSR
382700000000     C/EJECT
382800000000     C************************************************************
382900000000     C* REPERIMENTO DATI SOCIETA'
383000000000     C************************************************************
383100000000     C     REPSOC        BEGSR
383200000000     C*
383300130909     C                   CALL      'SDGSOCDR'
383400130909     C                   PARM                    TIPXSC            6
383500130909     C                   PARM                    SOCXSC            3
383600130909     C                   PARM                    CDSXSC            9 0
383700130909     C                   PARM                    MODXSC            3
383800130909     C                   PARM      *BLANKS       RTNXSC            1
383900130909     C                   PARM                    XSOCDS
384000130909     C                   PARM                    KPJBA
384100000000     C*
384200000000     C                   ENDSR
384300000000     C/EJECT
384400000000     C************************************************************
384500000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
384600000000     C************************************************************
384700000000     C     REPAUT        BEGSR
384800000000     C*
384900000000     C                   CLEAR                   AUTDS
385000000000     C                   Clear                   ConCivDs
385100000000     C*
385200130909     C                   MoveL     XSCSOC        XCASOC                         SOCIETA
385300130909     C                   MoveL     XSCDUN        XCAUNI                         UNITA
385400000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
385500130909     C                   MoveL     KNMUS         XCAPRF                         PROFILO
385600130909    >C                   MoveL     'CONCIV'      XCAFNC                         FUNZIONE
385700130909    >C                   MoveL     *ZERO         XCAVFU                         VARIABILE
385800000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
385900000000    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
386000000000     C*
386100130909     C                   CALLB     'XCHKAUT'
386200130909     C                   PARM                    AUTDS
386300000000     C*
386400000000     C* errori/non abilitazione
386500000000     C     XCARTN        IFNE      0
386600170803    >C                   MOVE      '2'           ERRI76
386700000000     C                   MOVE      *ON           $FINE
386800130909     C                   EXSR      ENDPGM
386900000000     C                   ELSE
387000130909    >C                   MoveL     XCADDS        CONCIVDS
387100000000     C                   ENDIF
387200000000     C*
387300000000     C                   ENDSR
387400000000     C/EJECT
387500000000     C************************************************************
387600000000     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
387700000000     C************************************************************
387800000000     C     TSTAUT        BEGSR
387900000000     C*
388000000000     C                   SELECT
388100000000     C*
388200000000     C* se richiamato in immissione
388300170803    >C     OPZI76        WHENEQ    '01'
388400000000     C* ma l'utente non è abilitato
388500130909    >C     IMMALL        IFEQ      *Off
388600000000     C* esco da pgm avvertendolo
388700170803    >C                   MOVE      '2'           ERRI76
388800000000     C                   MOVE      *ON           $FINE
388900130909     C                   EXSR      ENDPGM
389000000000     C                   ENDIF
389100000000     C*
389200000000     C* se richiamato in modifica
389300170803    >C     OPZI76        WHENEQ    '02'
389400000000     C* ma l'utente non è abilitato
389500130909    >C     MANALL        IFEQ      *Off
389600000000     C* testo se l'utente è abilitato almeno alla visualizzazione
389700000000    >C     VISALL        IFEQ      *ON
389800170803    >C                   MOVE      '05'          OPZI76
389900000000     C                   ELSE
390000000000     C* altrimenti esco da pgm avvertendolo
390100170803    >C                   MOVE      '2'           ERRI76
390200000000     C                   MOVE      *ON           $FINE
390300130909     C                   EXSR      ENDPGM
390400000000     C                   ENDIF
390500000000     C                   ENDIF
390600000000     C*
390700000000     C* se richiamato in copia
390800170803    >C     OPZI76        WHENEQ    '03'
390900000000     C* ma l'utente non è abilitato
391000130909    >C     IMMALL        IFEQ      *Off
391100000000     C* esco da pgm avvertendolo
391200170803    >C                   MOVE      '2'           ERRI76
391300000000     C                   MOVE      *ON           $FINE
391400130909     C                   EXSR      ENDPGM
391500000000     C                   ENDIF
391600000000     C*
391700000000     C* se richiamato in annullamento
391800170803    >C     OPZI76        WHENEQ    '04'
391900000000     C* ma l'utente non è abilitato
392000130909    >C     ANNALL        IFEQ      *Off
392100000000     C* esco da pgm avvertendolo
392200170803    >C                   MOVE      '2'           ERRI76
392300000000     C                   MOVE      *ON           $FINE
392400130909     C                   EXSR      ENDPGM
392500000000     C                   ENDIF
392600000000     C*
392700000000     C* se richiamato in visualizzazione
392800170803    >C     OPZI76        WHENEQ    '05'
392900000000     C* ma l'utente non è abilitato
393000130909    >C     VISALL        IFEQ      *Off
393100000000     C* esco da pgm avvertendolo
393200170803    >C                   MOVE      '2'           ERRI76
393300000000     C                   MOVE      *ON           $FINE
393400130909     C                   EXSR      ENDPGM
393500000000     C                   ENDIF
393600000000     C*
393700000000     C                   ENDSL
393800000000     C*
393900000000     C                   ENDSR
394000000000     C/EJECT
394100130918     C************************************************************
394200170803     C* Reperimento parametri di setup per Spesometro trimestrale
394300130918     C************************************************************
394400170803     C     RepSDSPETRI   BEGSR
394500000000     C*
394600170803     C                   Clear                   SDGPA12DS
394700000000     C*
394800130909     C                   CALLB     'XPAR'
394900130909     C                   PARM                    XScSoc
395000170803     C                   PARM      'SDSPETRI'    XPaPar            8
395100130909     C                   PARM                    XPaOut         2000
395200130909     C                   PARM      *Off          XPaErr            1
395300130909     C                   PARM      ' '           XPaRic            1
395400000000     C*
395500000000     C                   if        XPaErr      = *On
395600130909     C                   EXSR      ENDPGM
395700000000     C*
395800000000     C                   else
395900170803     C                   MoveL     XPaOut        SDGPA12DS
396000000000     C*
396100170803     C                   if        AttCSTA12   <> *On
396200130909     C                   EXSR      ENDPGM
396300000000     C                   endif
396400000000     C*
396500000000     C                   endif
396600000000     C*
396700000000     C                   ENDSR
396800000000     C/EJECT
396900000000     C************************************************************
397000000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
397100000000     C************************************************************
397200000000     C     DEFOPZ        BEGSR
397300000000     C*
397400000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1
397500000000     C* con S o N a seconda della utilizzabilità o meno
397600000000     C* del tasto funzione correlato
397700000000     C*
397800000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
397900000000     C* di compilazione
398000130909     C                   Reset                   $FC1
398100130909     C                   Reset                   $FR1
398200000000     C*
398300000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
398400000000     C* invalido alcune opzioni
398500000000     C                   SELECT
398600000000     C* Se pgm richiamato in interrogazione disattivo :
398700170803    >C     OPZI76        WHENEQ    '05'
398800000000     C* F2=Decodifica
398900000000     C                   Z-ADD     2             X
399000000000     C                   MOVE      '0'           $FC1(X)
399100000000     C* F4=Ricerca
399200000000     C                   Z-ADD     4             X
399300000000     C                   MOVE      '0'           $FC1(X)
399400000000     C* F5=Ripristino
399500000000     C                   Z-ADD     5             X
399600000000     C                   MOVE      '0'           $FC1(X)
399700000000     C* F6=Conferma
399800000000     C                   Z-ADD     06            X
399900000000     C                   MOVE      '0'           $FC1(X)
400000000000     C* Se pgm richiamato in annullamento disattivo :
400100170803    >C     OPZI76        WHENEQ    '04'
400200000000     C* F2=Decodifica
400300000000     C                   Z-ADD     2             X
400400000000     C                   MOVE      '0'           $FC1(X)
400500000000     C* F4=Ricerca
400600000000     C                   Z-ADD     4             X
400700000000     C                   MOVE      '0'           $FC1(X)
400800000000     C* F5=Ripristino
400900000000     C                   Z-ADD     5             X
401000000000     C                   MOVE      '0'           $FC1(X)
401100000000     C*
401200000000     C                   OTHER
401300000000     C*
401400000000     C                   ENDSL
401500000000     C*
401600000000     C                   ENDSR
401700000000     C/EJECT
401800000000     C************************************************************
401900000000     C* REPERIMENTO CAMPI DI DEFAULT
402000000000     C************************************************************
402100000000     C     REPDEF        BEGSR
402200000000     C*
402300130909     C                   Eval      $FlgDef     = *On
402400000000     C*
402500000000     C                   ENDSR
402600000000     C************************************************************
402700111010     C* DEFINIZIONE  LIST
402800000000     C************************************************************
402900130919     C     DefKlist      BEGSR
403000000000     C*
403100000000     C* klist
403200000000     C*
403300170803     C* SDGCST01L
403400170803     C     K05CST01      KLIST
403500170803     C                   KFLD                    CSTTpCom
403600170803     C                   KFLD                    CSTTpRec
403700170803     C                   KFLD                    CSTSys
403800170803     C                   KFLD                    CSTNrAsReg
403900170803     C                   KFLD                    CSTNrRigaM
404000170803     C                   KFLD                    CSTNrRiga
404100130919     C*
404200170807     C* SDGCST05L
404300170803     C     K08CST05      KLIST
404400170803     C                   KFLD                    CSTSocieta
404500170803     C                   KFLD                    CSTTpCom
404600170803     C                   KFLD                    CSTAnno
404700170803     C                   KFLD                    CSTTrim
404800170803     C                   KFLD                    CSTMese
404900170803     C                   KFLD                    CSTCF
405000170803     C                   KFLD                    CSTPartIVA
405100170803     C                   KFLD                    CSTCdFisc
405200170807     C*
405300170807     C* SDGCSF04L
405400170807     C     K02CSF04      KLIST
405500170807     C                   KFLD                    CSFSocieta
405600170807     C                   KFLD                    CSFIdFile
405700000000     C*
405800000000     C* ANRIV01L
405900000000     C     K04RIV01      KLIST
406000000000     C                   KFLD                    RIVSocieta
406100000000     C                   KFLD                    RIVTpReg
406200000000     C                   KFLD                    RIVAliq
406300000000     C                   KFLD                    RIVRifIVA
406400130909     C*
406500130909     C* ANDLI01L
406600130909     C     K03DLI01      klist
406700130909     C                   kfld                    DLISocieta
406800130909     C                   kfld                    DLITpReg
406900130909     C                   kfld                    DLILibro
407000000000     C*
407100000000     C                   ENDSR
407200000000     C/EJECT
407300000000     C************************************************************
407400000000     C* RIEMPIMENTO TASTI FUNZIONE
407500000000     C************************************************************
407600000000     C     RIEKEY        BEGSR
407700000000     C*
407800000000     C* Input :
407900000000     C* - $FX = schiera con tutti i testi disponibili
408000000000     C* - $FC = schiere con i flag di validità associati ai testi
408100000000     C* - $FR = schiere con i flag di REVERSE IMAGE
408200000000     C* - $ULK = ultimo testo caricato
408300000000     C* - $L01 = Lunghezza prima riga
408400000000     C* - $L02 = Lunghezza seconda riga
408500000000     C* Output :
408600000000     C* - $ULK
408700000000     C* - $KEY1= 1° riga di tasti funzione
408800000000     C* - $KEY2= 2° riga di tasti funzione
408900000000     C*
409000130909     C                   CALLB     'XKEYMSG'
409100000000     C* input
409200130909     C                   PARM                    $FX
409300130909     C                   PARM                    $FC
409400130909     C                   PARM                    $FR
409500130909     C                   PARM                    $ULK              3 0
409600000000     C* output
409700130909     C                   PARM                    $KEY1            79
409800130909     C                   PARM                    $KEY2            79
409900000000     C* input
410000130909     C                   PARM                    $L01              3 0
410100130909     C                   PARM                    $L02              3 0
410200000000     C*
410300130909     C                   PARM                    $ALLFUNCT       480
410400000000     C*
410500000000     C                   ENDSR
410600000000** $OZ / $ID
41070000000001SDGMSG    *LIBL     SDC0002
41080000000002PROMSG    *LIBL     COS0002
41090000000003PROMSG    *LIBL     COS0003
41100000000004PROMSG    *LIBL     COS0004
41110000000005PROMSG    *LIBL     COS0005
411200000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
411300000000PROMSG    *LIBL     KEY0001  01
411400000000PROMSG    *LIBL     KEY0002  02
411500000000PROMSG    *LIBL     KEY0003  03
411600000000PROMSG    *LIBL     KEY0004  04
411700000000PROMSG    *LIBL     KEY0005  05
411800000000PROMSG    *LIBL     KEY0006  06
411900000000PROMSG    *LIBL     KEY0007  07
412000000000PROMSG    *LIBL     KEY0008  08
412100000000PROMSG    *LIBL     KEY0009  09
412200000000PROMSG    *LIBL     KEY0010  10
412300130923SDGMSG    *LIBL     SDK0018  11
412400000000PROMSG    *LIBL     KEY0012  12
412500000000PROMSG    *LIBL     KEY0013  13
412600000000PROMSG    *LIBL     KEY0014  14
412700000000PROMSG    *LIBL     KEY0015  15
412800000000PROMSG    *LIBL     KEY0016  16
412900000000PROMSG    *LIBL     KEY0017  17
413000000000PROMSG    *LIBL     KEY0018  18
413100000000PROMSG    *LIBL     KEY0019  19
413200000000PROMSG    *LIBL     KEY0020  20
413300000000PROMSG    *LIBL     KEY0021  21
413400000000PROMSG    *LIBL     KEY0022  22
413500000000PROMSG    *LIBL     KEY0023  23
413600000000PROMSG    *LIBL     KEY0024  24
413700000000**  ABILITAZIONE D1 : $FC1 / $FR1
4138000000000N  01
4139000000000N  02
4140000000001N  03
4141000000001N  04
4142000000001N  05
4143000000001N  06
4144000000000N  07
4145000000001N  08
4146000000000N  09
4147000000000N  10
4148001708040N  11
4149000000001N  12
4150000000000N  13
4151000000000N  14
4152000000000N  15
4153000000000N  16
4154000000000N  17
4155000000000N  18
4156000000000N  19
4157000000000N  20
4158000000000N  21
4159000000000N  22
4160000000000N  23   Impostare a '1' se si utilizzano opzioni
4161000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
