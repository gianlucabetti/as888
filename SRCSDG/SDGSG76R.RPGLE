000100130909     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200130909     H*actgrp(QILE) PARMS CVTOPT(*NONE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500130909      *                Spesometro clienti/fornitori
000600130909      *                 Dati di dettaglio - manutenzione
000700000000      *           ---------------------------------------
000800000000      *
000900000000     F*----------------------------------------------------*
001000130909     F* Spesometro - Movimenti estratti
001100130909     F* Key: TpCom/Anno/Mese/Sys/NrAsReg/NrRigaM/NrRig   UNIQUE
001200130909     FSDGSCF11L UF A E           K DISK
001300000000     F*------------
001400130919     F* Spesometro - Movimenti estratti
001500130919     F* Key: Soc/TpCom/Anno/Sogg/Dati di raggruppam.
001600130919     FSDGSCF01L IF   E           K DISK
001700130919     F                                     RENAME(SDGSCF000:SDGSCF01)
001800130919     F*------------
001900130909     FSDGSPR03L IF   E           K DISK
002000000000     F*-------------
002100000000     FANRIV01L  IF   E           K DISK
002200130909     F*-------------
002300130909     F* libri
002400130909     FANDLI01L  IF   E           K DISK
002500130909     F*------------
002600130909     FSDGSG76D  CF   E             WORKSTN
002700130909     F                                     RENAME(SG76D1:FMTD1)
002800130909     F                                     RENAME(SG76T1:FMTT1)
002900130909     F                                     RENAME(SG76Z1:FMTZ1)
003000000000     F* eventuali schermi supplementari
003100130909     F                                     RENAME(SG76D2:FMTD2)
003200130909     F                                     RENAME(SG76D3:FMTD3)
003300000000     F                                     INFDS(DSFMT)
003400000000     F*------------
003500000000     D*----------------------------------------------------*
003600000000     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003700000000     D $ID             S             27    DIM(5) ALT($OZ)
003800000000     D* Schiere per la gestione dei tasti di funzione
003900000000     D $FM             S             27    DIM(24)
004000000000     D $FX             S             50    DIM(24)
004100000000     D $FC             S              1    DIM(24)
004200000000     D $FR             S              1    DIM(24)
004300000000     D* Video D1
004400000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004500000000     D $FX1            S             50    DIM(24) ALT($FM1)
004600000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
004700000000     D $FR1            S              1    DIM(24) ALT($FC1)
004800000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
004900000000     D* funzione personalizzati
005000000000     D $PGF1           S             10    DIM(25)
005100000000     D*----------------------------------------------------*
005200000000     D $D1L01          S              3  0
005300000000     D $D1L02          S              3  0
005400000000     D*-------------
005500000000     D* Passaggio Parametri
005600130909     D KPJBA         E DS
005700000000     D*-------------
005800000000     D* DS Esterna gestione messaggi
005900000000     D SDGMSGDS      E DS
006000000000     D  STS          E                     EXTFLD(MSGSTS)
006100000000     D                                     DIM(333)
006200000000     D  JBA          E                     EXTFLD(MSGJBA)
006300000000     D                                     DIM(502)
006400000000     D*-------------
006500000000     D* Reperimento Autorizzazioni
006600000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006700000000     D*-------------
006800000000     D* Maschera dati autorizzazioni
006900000000     D CONCIVDS      E DS
007000000000     D*-------------
007100000000     D* Reperimento nome PGM
007200000000     D STATUS         SDS           333
007300000000     D  DSPGM            *PROC
007400130909     D  PARMS            *PARMS
007500000000     D*-------------
007600000000     D* Dati di ambiente ottenuti da SDGSOCDR
007700000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
007800000000     D*-------------
007900000000     D* DS Interna per dati di output di SDGSOCDR
008000000000     D XSOCDS          DS          1000
008100000000     D*-------------
008200000000     D* Passaggio Parametri al PGM di manutenzione
008300130909     D SDGSG76DS     E DS
008400000000     D*-------------
008500130925     D* Passaggio Parametri al PGM conta righe quadro
008600130925     D SDGSG78DS     E DS                  INZ
008700130925     D*-------------
008800130925     D* Passaggio Parametri al PGM capoconti
008900130925     D ANA200DS      E DS
009000000000     D  CMT200       E                     INZ('0')
009100000000     D  RET200       E                     INZ('0')
009200000000     D  OPR200       E                     INZ('0')
009300000000     D  ERR200       E                     INZ('0')
009400000000     D  FSC200       E                     INZ('S')
009500000000     D*-------------
009600000000     D* Ricerca sottoconti
009700000000     D A201DS        E DS                  EXTNAME(ANA201DS) INZ
009800000000     D*-------------
009900000000     D* Passaggio Parametri al PGM clienti
010000000000     D ANA300DS      E DS
010100000000     D  CMT300       E                     INZ('0')
010200000000     D  RET300       E                     INZ('0')
010300000000     D  OPR300       E                     INZ('0')
010400000000     D  ERR300       E                     INZ('0')
010500000000     D  FSC300       E                     INZ('S')
010600000000     D*-------------
010700000000     D* Passaggio Parametri al PGM fornitori
010800000000     D ANA400DS      E DS
010900000000     D  CMT400       E                     INZ('0')
011000000000     D  RET400       E                     INZ('0')
011100000000     D  OPR400       E                     INZ('0')
011200000000     D  ERR400       E                     INZ('0')
011300000000     D  FSC400       E                     INZ('S')
011400130911     D*-------------
011500130918     D WStato          S                   Like(XScSta )
011600130918     D $CodUIC         S                   Like(CODUICG12)
011700130918     D $UICSoc         S                   Like(CODUICG12)
011800130918     D $UICKsc         S                   Like(CODUICG12)
011900130918     D $NazSoc         S                   Like(NazG12 )
012000130918     D $NazKsc         S                   Like(NazG12 )
012100130918     D $NazISO         S                   Like(NazG12 )
012200000000     D*-------------
012300000000     D* Controllo conti
012400000000     D ND002DS       E DS                  INZ
012500000000     D*-------------
012600000000     D* Definizioni parametri standard chiamate moduli
012700000000     D Esito           S              1
012800000000     D GesMsg          S              1
012900000000     D LenOut          S              9B 0
013000000000     D Data            S             10
013100000000     D StrutturaO      S             10
013200000000     D*-------------
013300000000     D* Parametri per ricerca codice ANATB00F
013400000000     D XTABDS        E DS                  inz
013500000000     D*-------------
013600000000     D* Richiamo a SDGTABDR
013700000000     D SDGTABDS      E DS                  inz
013800000000     D*-------------
013900000000     D* Ricerca codici Stati
014000000000     D ANGG12DS      E DS
014100130923     D*-------------
014200130923     D* Ricerca codici province italiane
014300130923     D ANGC17DS      E DS
014400130923     D*-------------
014500130923     D* Campo chiave per tabella C17
014600130923     D KeyC17          S              6A   inz
014700000000     D*-------------
014800000000     D* Definizioni controllo causale
014900000000     D Anope00f      e ds                  inz
015000000000     D Refresh         S              1
015100000000     D Uso             S              1
015200000000     D Output          S           4000
015300000000     D Causale         S              4
015400000000     D*-------------
015500000000     D* Ricerca causale
015600000000     D ANA070DS      E DS                  INZ
015700000000     D  RET070       E                     INZ('0')
015800000000     D  OPR070       E                     INZ('0')
015900000000     D  CMT070       E                     INZ('0')
016000000000     D  ERR070       E                     INZ('0')
016100000000     D  FSC070       E                     INZ('S')
016200000000     D*-------------
016300130909     D* Parametri modulo SDGSPECF
016400130909     D SDGPA08DS     E DS
016401151203R195 D*-------------
016402151203 "   D* Parametri modulo SDBLACL2
016403151203R195 D SDGPA11DS     E DS
016500000000     D*-------------
016600000000     D* Ricerca riferimenti e aliquote
016700130909     D ANA064DS      E DS                  INZ
016800000000     D  Ret064       E                     Inz('0')
016900000000     D  Opr064       E                     Inz('0')
017000000000     D  Err064       E                     Inz('0')
017100000000     D*-------------
017200000000     D* ricerca soggetti
017300000000     D ANA040DS      E DS                  INZ
017400000000     D*-------------
017500000000     D* DS esterna per dati soggetti
017600000000     D DVASOG        E DS                  INZ
017700000000     D*-------------
017800000000     D* Definizioni parametri driver anagrafici
017900000000     D Dvaric          S              1
018000000000     D DvaSocieta      S              3
018100000000     D DvaUnita        S              8
018200000000     D DvaStrutt       S             10
018300000000     D DvaDtRif        S               D
018400000000     D DvaLenOut       S              9B 0
018500000000     D DvaSnatura      S              1
018600000000     D DvaCodice       S              8
018700000000     D DvaLineav       S              3
018800000000     D DvaFiliale      S              3  0
018900000000     D DvaSogg         S              8
019000000000     D DvaTpInd        S              2
019100000000     D DvaCdInd        S              3
019200000000     D DvaErrore       S              1
019300000000     D DvaOutput       S           4000
019400000000     D*-------------
019500000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
019600000000     D* alla funzione personalizzata
019700130909     D $SaveDS         S                   like(SDGSG76DS)
019800000000     D*-------------
019900000000     D* Ds per ricerca legende
020000130909     D ANA964DS      E DS                  INZ
020100000000     D*-------------
020200000000     D* Controlli validità per valore di una legenda
020300000000     D X07VALDS      E DS                  Inz
020400000000     D*-------------
020500000000     D DSFMT           DS
020600000000     D  $TASTO               369    369
020700000000     D  NRG                  370    370
020800000000     D  NCL                  371    371
020900000000     D* riga cursore window
021000000000     D  NRgWin               382    382
021100000000     D* colonna cursore window
021200000000     D  NClWin               383    383
021300000000     D*-------------
021400000000     D* DS per UserExit
021500000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
021600000000     D*-------------
021700000000     D* posizione cursore
021800000000     D CURSOR          DS
021900000000     D  RIRI                   1      2B 0 INZ
022000000000     D  R$$                    2      2
022100000000     D  COCO                   3      4B 0 INZ
022200000000     D  C$$                    4      4
022300000000     D*-------------
022400000000     D* Costanti
022500000000     D*-------------
022600000000     D* Tasti di funzione
022700000000     D F01             C                   CONST(X'31')
022800000000     D F02             C                   CONST(X'32')
022900000000     D F03             C                   CONST(X'33')
023000000000     D F04             C                   CONST(X'34')
023100000000     D F05             C                   CONST(X'35')
023200000000     D F06             C                   CONST(X'36')
023300000000     D F07             C                   CONST(X'37')
023400000000     D F08             C                   CONST(X'38')
023500000000     D F09             C                   CONST(X'39')
023600000000     D F10             C                   CONST(X'3A')
023700000000     D F11             C                   CONST(X'3B')
023800000000     D F12             C                   CONST(X'3C')
023900000000     D F13             C                   CONST(X'B1')
024000000000     D F14             C                   CONST(X'B2')
024100000000     D F15             C                   CONST(X'B3')
024200000000     D F16             C                   CONST(X'B4')
024300000000     D F17             C                   CONST(X'B5')
024400000000     D F18             C                   CONST(X'B6')
024500000000     D F19             C                   CONST(X'B7')
024600000000     D F20             C                   CONST(X'B8')
024700000000     D F21             C                   CONST(X'B9')
024800000000     D F22             C                   CONST(X'BA')
024900000000     D F23             C                   CONST(X'BB')
025000000000     D F24             C                   CONST(X'BC')
025100000000     D ENTER           C                   CONST(X'F1')
025200000000     D ROLDWN          C                   CONST(X'F4')
025300000000     D ROLLUP          C                   CONST(X'F5')
025400000000     D*-------------
025500000000     D* Sottotitolo di testata
025600000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
025700000000     D                                          COS0648')
025701151201R195 D*-------------
025702151201  "  D* Sottotitolo di testata
025703151201  "  D* "Dettaglio Spesometro"
025704151201  "  D SOTTITT1        C                   CONST('SDGMSG    *LIBL-
025705151201  "  D                                          SDC0096')
025706151201  "  D* Sottotitolo di testata Black List
025707151201  "  D* "Dettaglio Aquisti da RSM"
025709151201  "  D SOTTITT2        C                   CONST('SDGMSG    *LIBL-
025710151201  "  D                                          SDC0097')
025711151201  "  D* Sottotitolo di testata Black List
025712151201  "  D* "Soggetti Black List"
025713151201  "  D* "Dettaglio Black List"
025714151201  "  D SOTTITT3        C                   CONST('SDGMSG    *LIBL-
025715151201R195 D                                          SDC0098')
025800000000     D*-------------
025900000000     D* Variabili per gestione videate
026000000000     D $Gest           S              2A
026100000000     D $Fine           S              1A
026200000000     D $InzD1          S              1A
026300000000     D $LastG          S              2A
026400000000     D $LastV          S              2A
026500000000     D $FlgDef         S              1A
026600000000     D*-------------
026700000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
026800000000     D CurR            S              3S 0
026900000000     D CurC            S              3S 0
027000000000     D Win             S             99A
027100000000     D*-------------
027200000000     D* Variabili appoggio specifiche della singola anagrafica
027300000000     D*  Indici
027400000000     D X               S              5S 0
027500000000     D Y               S              5S 0
027600000000     D* Indice della schiera tasti funzione
027700000000     D PosFun          S              2  0
027800111010R067 D*-------------
027900111010 "   D* Ricerca libri IVA
028000130909 "   D AnA062DS      E DS                  Inz
028100111010 "   D  Ret062       E                     Inz('0')
028200111010 "   D  Opr062       E                     Inz('0')
028300111010 "   D  Err062       E                     Inz('0')
028400111010 "   D*-------------
028500111010 "   D* Ricerca associazione libro-riferimento IVA
028600130909R067 D ANA068DS      E DS                  INZ
028700000000     D*-------------
028800000000     D* Campi di lavoro
028900131031R140 D WData           S               D
029000130930     D WIn16           S              1
029100130926     D WIn17           S              1
029200130923     D UDateISO        S               D
029300000000     D WDtNasc         S               D
029400130909     D WDtNasR         S               D
029500000000     D WSogg           S                   Like(DVSSogg)
029600000000     D WAnnoAlfa       S              4A
029700000000     D WDtAlfa         S             10A
029800000000     D WAnnoDoc        S              4S 0
029900000000     D WDtDoc          S               D
030000000000     D WAnnoReg        S              4S 0
030100000000     D WDtReg          S               D
030200130909     D WDtRegR         S               D
030300000000     D WDtpar          S               D
030400130909     D WAliq           S                   Like(SCFAliq   )
030500130909     D WRifIVA         S                   Like(SCFRifIVA )
030600000000     D WRifIvaOk       S              1
030700130909     D WKcc            S                   Like(SCFKcc )
030800130909     D WKsc            S                   Like(SCFKsc )
030900000000     D WDtOri          S               D
031000000000     D WDtDf           S               D
031100000000     D WDtRf           S               D
031200000000     D WDtPf           S               D
031300000000     D WDtOpe          S               D
031301160104R195 D WFormato        S                    Like(SCFFormato)
031400000000     D*------
031500000000     D $Esito          S              1
031600000000     D $Prenot         S              1
031700130909     D $TpCom          S                    Like(SCFTpCom  )
031800130909     D $Anno           S                    Like(SCFAnno   )
031900130909     D $Mese           S                    Like(SCFMese   )
032000130909     D $Sys            S                    Like(SCFSys    )
032100130909     D $NrAsReg        S                    Like(SCFNrAsReg)
032200130909     D $NrRigaM        S                    Like(SCFNrRigaM)
032300130909     D $NrRiga         S                    Like(SCFNrRiga )
032400000000     C*----------------------------------------------------*
032500000000     C*                MAIN LINE PROGRAM
032600000000     C*----------------------------------------------------*
032700000000     C*
032800000000     C* inizializzazione variabili
032900130909     C                   EXSR      INZVAR
033000000000     C*
033100130909     C     $FINE         DOWEQ     *Off
033200000000     C     $GEST         CASEQ     'D1'          GESD1
033300000000     C     $GEST         CASEQ     'D2'          GESD2
033400130923     C     $GEST         CASEQ     'D3'          GESD3
033500000000     C                   ENDCS
033600000000     C                   ENDDO
033700000000     C*
033800000000     C* fine programma
033900130909     C                   EXSR      ENDPGM
034000000000     C/EJECT
034100000000     C************************************************************
034200000000     C* Fine Programma
034300000000     C************************************************************
034400000000     C     ENDPGM        BEGSR
034500000000     C*
034600000000     C* se non esco per errore
034700130909    >C     ERRG76        IFEQ      '0'
034800000000     C*
034900000000     C* se il COMMIT non è demandato al pgm chiamante
035000130909    >C     CMTG76        IFEQ      '0'
035100000000     C*
035200000000     C* test se esistono transazioni attive ed eventualmente cosa fare
035300000000     C                   Z-ADD     0             EXECOD
035400130909     C                   CALL      'XRCMTI'
035500130909     C                   PARM                    EXECOD            1 0
035600000000     C*
035700000000     C                   SELECT
035800000000     C* Annulla richiesta
035900000000     C     EXECOD        WHENEQ    1
036000130909     C                   MoveL     *Off          $FINE
036100130909     C                   MoveL     *HIVAL        $LASTV
036200000000     C* Conferma uscita
036300000000     C     EXECOD        WHENEQ    2
036400000000     C*                  ROLBK
036500000000     C*
036600000000     C                   ENDSL
036700000000     C*
036800000000     C                   ENDIF
036900000000     C*
037000000000     C                   ENDIF
037100000000     C*
037200000000     C* se ho confermato l'uscita proseguo
037300000000     C     $FINE         IFEQ      *ON
037400000000     C*
037500000000     C* disallocazione rcd nel caso di modifica/annullamento
037600130909    >C     OPZG76        IFEQ      '02'
037700130909    >C     OPZG76        OREQ      '04'
037800130909    >C                   UNLOCK    SDGSCF11L                            22
037900000000     C                   ENDIF
038000000000     C*
038100130909     C                   SetOn                                            RT
038200000000     C                   RETURN
038300000000     C*
038400000000     C                   ENDIF
038500000000     C*
038600000000     C                   ENDSR
038700000000     C/EJECT
038800000000     C************************************************************
038900000000     C* RICERCHE
039000000000     C************************************************************
039100000000     C     SEARCH        BEGSR
039200000000     C*
039300000000     C* determino Riga/Colonna del cursore
039400000000     C                   MOVE      NRG           R$$
039500000000     C                   MOVE      NCL           C$$
039600000000     C                   Z-ADD     RIRI          CURR
039700000000     C                   Z-ADD     COCO          CURC
039800000000     C*
039900000000     C                   SELECT
040000000000     C*
040100000000     C*  Soggetto
040200000000     C     H1NMFL        WHENEQ    'D1SOGG'
040300130909     C                   EXSR      F4_Sogg
040400130911     C*
040500130911     C*  Tipo natura giuridica
040600130911     C     H1NMFL        WHENEQ    'D1TPN '
040700130911     C     H1NMFL        OREQ      'D1TPNR   '
040800130911     C                   EXSR      F4_TPN
040900000000     C*
041000000000     C*  Codice IVA
041100000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
041200130909     C     H1NMFL        OREQ      'D1PIVAR  '
041300130909     C                   EXSR      F4_PartIva
041400000000     C*
041500000000     C*  Codice fiscale
041600000000     C     H1NMFL        WHENEQ    'D1CDFISC'
041700130909     C     H1NMFL        OREQ      'D1CDFISCR'
041800130909     C                   EXSR      F4_CdFisc
041900000000     C*
042000000000     C*  Stato
042100000000     C     H1NMFL        WHENEQ    'D1STATO'
042200130909     C     H1NMFL        OREQ      'D1STATOR'
042300130909     C                   EXSR      F4_Stato
042400000000     C*
042500000000     C* capoconto
042600000000     C     H1NMFL        WHENEQ    'D1KCC'
042700130909     C                   EXSR      F4_Kcc
042800000000     C*
042900000000     C* cliente
043000000000     C     H1NMFL        WHENEQ    'D1KSC'
043100130911    >C     H1CF          ANDEQ     'C'
043200130909     C                   EXSR      F4_Cliente
043300000000     C*
043400000000     C*  fornitore
043500000000     C     H1NMFL        WHENEQ    'D1KSC'
043600130911    >C     H1CF          ANDEQ     'F'
043700130909     C                   EXSR      F4_Fornitore
043800130909     C*
043900130909     C* sottoconto
044000130909     C     H1NMFL        WHENEQ    'D1KSC'
044100130909     C                   EXSR      F4_Ksc
044200000000     C*
044300000000     C* causale beni/servizi
044400000000     C     H1NMFL        WHENEQ    'D1CAUSALE'
044500130909     C                   EXSR      F4_Causale
044600000000     C*
044700000000     C* riferimenti IVA
044800000000     C     H1NMFL        WHENEQ    'D1ALIQ'
044900000000     C     H1NMFL        OREQ      'D1RIFIVA'
045000130909     C                   EXSR      F4_AliqRifIVA
045100130909     C*
045200130909     C* FMTD1 - Tipo registro IVA
045300130909     C     H1NMFL        WHENEQ    'D1TPREG'
045400130909     C                   EXSR      F4_TpReg
045500130909     C*
045600130909     C* FMTD1 - Libro  IVA
045700130909     C     H1NMFL        WHENEQ    'D1LIBRO'
045800130909     C                   EXSR      F4_Libro
045900000000     C*
046000130909     C* serie registrazione
046100000000     C     H1NMFL        WHENEQ    'D1SERREG'
046200130909     C                   EXSR      F4_Serie
046300130909     C*
046400130909     C* tipo documento
046500130909     C     H1NMFL        WHENEQ    'D1TPDOC  '
046600130909     C                   EXSR      F4_TpDoc
046700130909     C*
046800130909     C* FMTD2 - Variazione della nota
046900130909     C     H1NMFL        WHENEQ    'D1VARNOTA'
047000130909     C     D1TpDoc       ANDEQ     'N'
047100130909     C                   EXSR      F4_VarNota
047200000000     C*
047300130909     C* natura operazione
047400130909     C     H1NMFL        WHENEQ    'D1NATOPE '
047500130909     C     H1NMFL        OREQ      'D1NATOPER'
047600130909     C                   EXSR      F4_NatOpe
047700130926     C*
047800130926     C* noleggio/leasing
047900130926     C     H1NMFL        WHENEQ    'D1LEASING'
048000130926     C     H1NMFL        OREQ      'D1LEASINR'
048100130926     C                   EXSR      F4_Leasing
048200130909     C*
048300130909     C* tipo operazione
048400130909     C     H1NMFL        WHENEQ    'D1TPOPE '
048500130925     C     H1NMFL        OREQ      'D1TPOPER '
048600130909     C                   EXSR      F4_TpOpe
048700130909     C*
048800130909     C* tipo importo
048900130909     C     H1NMFL        WHENEQ    'D1TPIMP '
049000130909     C                   EXSR      F4_TpImp
049100130909     C*
049200130909     C* origine
049300130909     C     H1NMFL        WHENEQ    'D1ORIGINE'
049400130909     C                   EXSR      F4_Origine
049500000000     C*
049600000000     C* FMTD2 - Tipo annullamento
049700130909     C     H1NMFL        WHENEQ    'D1TPANN'
049800130909     C                   EXSR      F4_TpAnn
049900000000     C*
050000130909     C* quadro di presentazione
050100130909     C     H1NMFL        WHENEQ    'D1QUADRO'
050200130909     C                   EXSR      F4_Quadro
050300000000     C*
050400000000     C* ricerca non abilitata in questa posizione
050500000000     C                   OTHER
050600130909     C                   SetOn                                        9899
050700000000     C*
050800000000     C                   ENDSL
050900000000     C*
051000000000     C* imposto pos. del cursore
051100000000     C                   Z-ADD     CURR          H1RIGA
051200000000     C                   Z-ADD     CURC          H1COLO
051300000000     C*
051400000000     C                   MOVE      *HIVAL        $LASTV
051500000000     C*
051600000000     C                   ENDSR
051700000000     C/EJECT
051800000000     C************************************************************
051900000000     C* Ricerca codice soggetto
052000000000     C************************************************************
052100000000     C     F4_Sogg       BEGSR
052200000000     C*
052300130909     C                   Reset                   ANA040DS
052400000000     C*
052500130926     C                   MoveL     '01'          OPZ040
052600130909     C                   MoveL     D1Sogg        Cod040
052700130909     C                   MoveL     ANA040DS      KPJBU
052800130909     C                   CALL      'ANA040R'
052900130909     C                   PARM                    KPJBA
053000000000     C*
053100130909     C                   MoveL     KPJBU         ANA040DS
053200000000     C*
053300000000     C                   if        Ret040     <> '2'
053400130909     C                   MoveL     Cod040        D1Sogg
053500000000     C                   endif
053600000000     C*
053700000000     C                   ENDSR
053800000000     C/EJECT
053900130911     C************************************************************
054000130911     C* Ricerca tipo natura giuridica
054100130911     C************************************************************
054200130911     C     F4_TpN        BEGSR
054300130911     C*
054400130911     C                   Reset                   ANA964DS
054500130911     C*
054600130911     C                   MoveL     '01'          Opz964
054700130911     C                   MoveL     'SDGSCF000'   Trc964
054800130911     C                   MoveL     'SCFTPN    '  Cam964
054900130911     C*
055000130911     C                   MoveL     ANA964DS      KPJBU
055100130911     C                   CALL      'ANA964R'
055200130911     C                   PARM                    KPJBA
055300130911     C                   MoveL     KPJBU         ANA964DS
055400130911     C*
055500130911     C                   if        Ret964     <> '2'
055600130911     C                   Select
055700130911     C     H1NMFL        WHENEQ    'D1TPN    '
055800130911     C                   MoveL     Val964        D1TpN
055900130911     C     H1NMFL        WHENEQ    'D1TPNR   '
056000130911     C                   MoveL     Val964        D1TpNR
056100130911     C                   endSl
056200130911     C                   endif
056300130911     C*
056400130911     C                   ENDSR
056500130911     C/EJECT
056600000000     C************************************************************
056700000000     C* Ricerca codice IVA
056800000000     C************************************************************
056900000000     C     F4_PartIva    BEGSR
057000000000     C*
057100130909     C                   Reset                   ANA040DS
057200000000     C*
057300130909     C                   MoveL     '05'          OPZ040
057400130909     C                   Select
057500130909     C     H1NMFL        WHENEQ    'D1PARTIVA'
057600130909     C                   MoveL     D1PartIva     IVA040
057700130909     C     H1NMFL        WHENEQ    'D1PIVAR  '
057800130909     C                   MoveL     D1PIVAR       IVA040
057900130909     C                   endSl
058000130909     C                   MoveL     ANA040DS      KPJBU
058100130909     C                   CALL      'ANA040R'
058200130909     C                   PARM                    KPJBA
058300000000     C*
058400130909     C                   MoveL     KPJBU         ANA040DS
058500000000     C*
058600000000     C                   if        Ret040     <> '2'
058700000000     C*
058800130909     C                   MoveL     Cod040        WSogg
058900130909     C                   EXSR      CALLDvaSog
059000000000     C*
059100000000     C                   if        DVAErrore  <> '1'
059200000000     C*
059300130909     C                   Eval      %subst(DVASOG:1:DVALENOUT)
059400000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
059500000000     C*
059600130909     C                   Select
059700130909     C     H1NMFL        WHENEQ    'D1PARTIVA'
059800130909     C                   MoveL     DVSPartIVA    D1PartIva
059900130909     C     H1NMFL        WHENEQ    'D1PIVAR  '
060000130909     C                   MoveL     DVSPartIVA    D1PIVAR
060100130909     C                   endSl
060200000000     C*
060300000000     C                   endif
060400000000     C*
060500000000     C                   endif
060600000000     C*
060700000000     C                   ENDSR
060800000000      /EJECT
060900000000     C************************************************************
061000000000     C* Chiamata al driver dei soggetti
061100000000     C************************************************************
061200130909     C     CALLDvaSog    BEGSR
061300000000     C*
061400130909     C                   Eval      DVALenOut = %Size(DVASOG)
061500000000     C*
061600130909     C                   CALLB     'NDDVASOG'
061700130909     C                   PARM      '1'           DVARIC
061800130909     C                   PARM      XScSoc        DVASOCIETA
061900130909     C                   PARM                    DVAUNITA
062000130909     C                   PARM      'DVASOG    '  DVASTRUTT
062100130909     C                   PARM                    DVADTRIF
062200130909     C                   PARM                    DVALENOUT
062300000000     C* Se si accede con il codice cli/for impostare i campi seguenti
062400000000     C* se si accede con il soggetto ometterli e impostare gli altri
062500130909     C*****              PARM                    DVASNATURA
062600130909     C*****              PARM                    DVACODICE
062700130909     C*****              PARM                    DVALINEAV
062800130909     C*****              PARM                    DVAFILIALE
062900130909     C                   PARM                    *omit
063000130909     C                   PARM                    *omit
063100130909     C                   PARM                    *omit
063200130909     C                   PARM                    *omit
063300130909     C                   PARM      WSogg         DVSSOGG
063400130909     C                   PARM      *Blank        DVSTPIND
063500130909     C                   PARM      *Blank        DVSCDIND
063600130909     C                   PARM                    DVAERRORE
063700130909     C                   PARM                    DVAOUTPUT
063800000000     C*
063900000000     C                   ENDSR
064000000000     C/EJECT
064100000000     C************************************************************
064200000000     C* Ricerca codice fiscale
064300000000     C************************************************************
064400000000     C     F4_CdFisc     BEGSR
064500000000     C*
064600130909     C                   Reset                   ANA040DS
064700000000     C*
064800130909     C                   MoveL     '05'          OPZ040
064900130909     C                   Select
065000130909     C     H1NMFL        WHENEQ    'D1CDFISC'
065100130909     C                   MoveL     D1CdFisc      CdF040
065200130909     C     H1NMFL        WHENEQ    'D1CDFISCR'
065300130909     C                   MoveL     D1CdFiscR     CdF040
065400130909     C                   endSl
065500130909     C                   MoveL     ANA040DS      KPJBU
065600130909     C                   CALL      'ANA040R'
065700130909     C                   PARM                    KPJBA
065800000000     C*
065900130909     C                   MoveL     KPJBU         ANA040DS
066000000000     C*
066100000000     C                   if        Ret040     <> '2'
066200000000     C*
066300130909     C                   MoveL     Cod040        WSogg
066400130909     C                   EXSR      CALLDvaSog
066500000000     C*
066600000000     C                   if        DVAErrore  <> '1'
066700000000     C*
066800130909     C                   Eval      %subst(DVASOG:1:DVALENOUT)
066900000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
067000000000     C*
067100130909     C                   Select
067200130909     C     H1NMFL        WHENEQ    'D1CDFISC'
067300130909     C                   MoveL     DVSCdFisc     D1CdFisc
067400130909     C     H1NMFL        WHENEQ    'D1CDFISCR'
067500130909     C                   MoveL     DVSCdFisc     D1CdFiscR
067600130909     C                   endSl
067700000000     C*
067800000000     C                   endif
067900000000     C*
068000000000     C                   endif
068100000000     C*
068200000000     C                   ENDSR
068300000000     C/EJECT
068400000000     C************************************************************
068500000000     C* Ricerca codice Stato del conto
068600000000     C************************************************************
068700000000     C     F4_Stato      BEGSR
068800000000     C*
068900130909     C                   Reset                   XTabDS
069000000000     C*
069100000000     C                   Move      '05'          XTaOpz
069200000000     C                   Move      XScSoc        XTaAzi
069300000000     C                   Move      XScLin        XTaLin
069400000000     C                   Z-add     Curr          XTaRig
069500000000     C                   Z-add     Curc          XTaCol
069600000000     C                   Move      *Blank        XTaKey
069700000000     C*
069800130909     C                   Clear                   KPJBU
069900130909     C                   MoveL     XTabDS        KPJBU
070000000000     C*
070100130909     C                   CALL      'ANGG12R'
070200130909     C                   PARM                    KPJBA
070300130909     C                   MoveL     KPJBU         XTabDS
070400000000     C*
070500000000     C                   if        XTaErr     <> '1'      and
070600000000     C                             XTaKey     <> *Blank
070700130909     C                   Select
070800130909     C     H1NMFL        WHENEQ    'D1STATO'
070900130909     C                   Move      XTaKey        D1Stato
071000130909     C     H1NMFL        WHENEQ    'D1STATOR'
071100130909     C                   Move      XTaKey        D1StatoR
071200130909     C                   endSl
071300000000     C                   endif
071400000000     C*
071500000000     C                   ENDSR
071600000000      /EJECT
071700000000     C************************************************************
071800000000     C* Ricerca capoconto
071900000000     C************************************************************
072000000000     C     F4_Kcc        BEGSR
072100000000     C*
072200130909     C                   Reset                   ANA200DS
072300000000     C*
072400130909     C                   if        OPZG76      = '05' or
072500130909     C                             OPZG76      = '04'
072600130909     C                   MoveL     '02'          OPZ200
072700000000     C                   else
072800130909     C                   MoveL     '01'          OPZ200
072900000000     C                   endif
073000130909     C                   MoveL     D1Kcc         KCC200
073100130909     C                   MoveL     ANA200DS      KPJBU
073200130909     C                   CALL      'ANA200R'                            21
073300130909     C                   PARM                    KPJBA
073400130909     C                   MoveL     KPJBU         ANA200DS
073500000000     C*
073600000000     C     KCC200        IFNE      *BLANK
073700000000     C                   MOVE      KCC200        D1Kcc
073800000000     C                   ENDIF
073900000000     C*
074000000000     C                   ENDSR
074100000000     C/EJECT
074200000000     C************************************************************
074300000000     C* Ricerca cliente
074400000000     C************************************************************
074500000000     C     F4_Cliente    BEGSR
074600000000     C*
074700130909    >C                   Reset                   ANA300DS
074800000000     C*
074900130909     C                   if        OPZG76      = '05' or
075000130909     C                             OPZG76      = '04'
075100130909    >C                   MoveL     '02'          OPZ300
075200000000     C                   else
075300130909    >C                   MoveL     '01'          OPZ300
075400000000     C                   endif
075500130909    >C                   MoveL     D1Ksc         KSC300
075600130909    >C                   MoveL     D1Sogg        SOG300
075700130909    >C                   MoveL     ANA300DS      KPJBU
075800000000     C*
075900130909    >C                   CALL      'ANA300R'                            21
076000130909     C                   PARM                    KPJBA
076100000000     C*
076200130909    >C                   MoveL     KPJBU         ANA300DS
076300000000     C*
0764000000002   >C     KSC300        IFNE      *BLANK
076500000000    >C                   MOVE      KCC300        D1Kcc
076600000000    >C                   MOVE      KSC300        D1Ksc
0767000000002-  >C                   ENDIF
076800000000     C*
076900000000     C                   ENDSR
077000000000     C/EJECT
077100000000     C************************************************************
077200000000     C* Ricerca fornitore
077300000000     C************************************************************
077400000000     C     F4_Fornitore  BEGSR
077500000000     C*
077600130909    >C                   Reset                   ANA400DS
077700000000     C*
077800130909     C                   if        OPZG76      = '05' or
077900130909     C                             OPZG76      = '04'
078000130909    >C                   MoveL     '02'          OPZ400
078100000000     C                   else
078200130909    >C                   MoveL     '01'          OPZ400
078300000000     C                   endif
078400130909    >C                   MoveL     D1Ksc         KSC400
078500130909    >C                   MoveL     D1Sogg        SOG400
078600000000     C*
078700130909    >C                   MoveL     ANA400DS      KPJBU
078800000000     C*
078900130909    >C                   CALL      'ANA400R'                            21
079000130909     C                   PARM                    KPJBA
079100000000     C*
079200130909    >C                   MoveL     KPJBU         ANA400DS
079300000000     C*
0794000000002   >C     KSC400        IFNE      *BLANK
079500000000    >C                   MOVE      KCC400        D1Kcc
079600000000    >C                   MOVE      KSC400        D1Ksc
0797000000002-  >C                   ENDIF
079800000000     C*
079900000000     C                   ENDSR
080000000000     C/EJECT
080100130909     C************************************************************
080200130909     C* Ricerca conto
080300130909     C************************************************************
080400130909     C     F4_Ksc        BEGSR
080500130909     C*
080600130909     C                   Reset                   A201DS
080700130909     C                   MOVE      XSCSOC        SOC201
080800130909     C                   MOVE      D1KCC         KCC201
080900130909     C                   MOVE      D1KsC         KSC201
081000130909     C                   if        OPZG76      = '05' or
081100130909     C                             OPZG76      = '04'
081200130909     C                   MoveL     '02'          OPZ201
081300130909     C                   else
081400130909     C                   MoveL     '01'          OPZ201
081500130909     C                   endif
081600130909     C                   MoveL     A201DS        KPJBU
081700130909     C                   CALL      'ANA201R'
081800130909     C                   PARM                    KPJBA
081900130909     C                   MoveL     KPJBU         A201DS
082000130909     C*
082100130909     C     KSC201        IFNE      *BLANK
082200130909     C                   MoveL     KCC201        D1KCC
082300130909     C                   MoveL     KSC201        D1KSC
082400130909     C                   ENDIF
082500130909     C*
082600130909     C                   ENDSR
082700130909     C/EJECT
082800000000     C************************************************************
082900000000     C* Ricerca codice causale beni/servizi
083000000000     C************************************************************
083100000000     C     F4_Causale    BEGSR
083200000000     C*
083300130909     C                   Reset                   ANA070DS
083400000000     C*
083500130909     C                   if        OPZG76      = '05' or
083600130909     C                             OPZG76      = '04'
083700000000     C                   Move      '02'          Opz070
083800000000     C                   else
083900000000     C                   Move      '01'          Opz070
084000000000     C                   endif
084100000000     C                   MOVE      '1'           Tpc070
084200130909     C                   CLEAR                   KPJBU
084300130909     C                   MoveL     ANA070DS      KPJBU
084400000000     C*
084500130909     C                   CALL      'ANA070R'
084600130909     C                   PARM                    KPJBA
084700000000     C*
084800130909     C                   MoveL     KPJBU         ANA070DS
084900000000     C*
085000000000     C     Cau070        IFNE      *BLANK
085100130909     C                   MoveL     Cau070        D1Causale
085200000000     C                   ENDIF
085300000000     C*
085400000000     C                   ENDSR
085500000000      /EJECT
085600000000     C************************************************************
085700000000     C* Ricerca aliquota/Riferimento IVA
085800000000     C************************************************************
085900000000     C     F4_AliqRifIVA BEGSR
086000000000     C*
086100130909     C                   Reset                   ANA064DS
086200000000     C*
086300130909     C                   if        OPZG76      = '05' or
086400130909     C                             OPZG76      = '04'
086500000000     C                   Move      '02'          OPZ064
086600000000     C                   else
086700000000     C                   Move      '01'          OPZ064
086800000000     C                   endif
086900000000     C************       Move      XscSoc        SOC064
087000000000     C*
087100000000     C                   Z-Add     D1Aliq        Ali064
087200130909     C                   MoveL     D1RifIVA      RfI064
087300000000     C*
087400130919     C                   if        D1TpOpe     = 'P'
087500130909     C                   MoveL     '1'           TpR064
087600000000     C                   else
087700130909     C                   MoveL     '2'           TpR064
087800000000     C                   endif
087900000000     C*
088000130909     C                   MoveL     ANA064DS      KPJBU
088100000000     C*
088200130909     C                   CALL      'ANA064R'
088300130909     C                   PARM                    KPJBA
088400000000     C*
088500130909     C                   MoveL     KPJBU         ANA064DS
088600000000     C*
088700000000     C                   if        Ali064     <> 0           or
088800000000     C                             RfI064     <> *Blank
088900000000     C*
089000000000     C                   Z-Add     Ali064        D1Aliq
089100130909     C                   MoveL     RfI064        D1RifIVA
089200000000     C*
089300000000     C                   endif
089400000000     C*
089500000000     C                   ENDSR
089600000000      /EJECT
089700130909     C************************************************************
089800130909     C* Ricerca tipo registro IVA
089900130909     C************************************************************
090000130909     C     F4_TpReg      BEGSR
090100130909     C*
090200130909     C                   Reset                   ANA964DS
090300111010     C*
090400130909     C                   MoveL     '01'          Opz964
090500130909     C                   MoveL     'ANRIV000 '   Trc964
090600130909     C                   MoveL     'RIVTPREG'    Cam964
090700111010     C*
090800130909     C                   MoveL     ANA964DS      KPJBU
090900130909     C                   CALL      'ANA964R'
091000130909     C                   PARM                    KPJBA
091100130909     C                   MoveL     KPJBU         ANA964DS
091200111010     C*
091300111010     C                   if        Ret964     <> '2'
091400130909     C                   MoveL     Val964        D1TpReg
091500111010     C                   endif
091600111010     C*
091700130909     C                   ENDSR
091800130909     C/EJECT
091900130909     C************************************************************
092000130909     C* Ricerca libro  IVA
092100130909     C************************************************************
092200130909     C     F4_Libro      BEGSR
092300130909     C*
092400130909     C                   if        D1TpReg     = *Blank      or
092500130909     C                             (D1Aliq     = 0       and
092600130909     C                              D1RifIVA   = *Blank     )
092700130909     C                   EXSR      F4_LibroIVA
092800130909     C                   else
092900130909     C                   EXSR      F4_Assoc
093000130909     C                   endif
093100130909     C*
093200130909     C                   ENDSR
093300130909     C/EJECT
093400130909     C************************************************************
093500130909     C* Ricerca libro IVA
093600130909     C************************************************************
093700130909     C     F4_LibroIVA   BEGSR
093800111010     C*
093900130909     C                   Reset                   ANA062DS
094000111010     C*
094100111010     C                   Move      '01'          OPZ062
094200130909     C                   MoveL     D1TpReg       TpR062
094300130909     C                   MoveL     D1Libro       Lib062
094400111010     C*
094500130909     C                   MoveL     ANA062DS      KPJBU
094600111010     C*
094700130909     C                   CALL      'ANA062R'
094800130909     C                   PARM                    KPJBA
094900111010     C*
095000130909     C                   MoveL     KPJBU         ANA062DS
095100111010     C*
095200111010     C                   if        Ret062     <> '2'
095300130909     C                   MoveL     TpR062        D1TpReg
095400130909     C                   MoveL     Lib062        D1Libro
095500111010     C                   endif
095600111010     C*
095700130909     C                   ENDSR
095800130909     C/EJECT
095900130909     C************************************************************
096000130909     C* Ricerca associazione libro-riferimento IVA
096100130909     C************************************************************
096200130909     C     F4_Assoc      BEGSR
096300111010     C*
096400130909     C                   Reset                   ANA068DS
096500111010     C*
096600111010     C                   MOVE      '01'          OPZ068
096700111010     C                   MOVE      XscSoc        SOC068
096800111010     C                   MOVE      D1TpReg       TPR068
096900111010     C                   MOVE      D1Libro       LIB068
097000111010     C                   MOVE      D1Aliq        ALI068
097100111010     C                   MOVE      D1RifIVA      RIF068
097200130909     C                   MoveL     ANA068DS      KPJBU
097300111010     C*
097400130909     C                   CALL      'ANA068R'
097500130909     C                   PARM                    KPJBA
097600111010     C*
097700130909     C                   MoveL     KPJBU         ANA068DS
097800111010     C*
097900111010     C                   IF        Ret068     <> '2'
098000111010     C*
098100111010     C                   Z-Add     ALI068        D1Aliq
098200111010     C                   MOVE      RIF068        D1RifIVA
098300111010     C                   MOVE      TpR068        D1TpReg
098400111010     C                   MOVE      Lib068        D1Libro
098500111010     C*
098600111010     C                   ENDIF
098700111010     C*
098800111010     C                   ENDSR
098900111010     C/EJECT
099000130909     C************************************************************
099100130909     C* Serie numeratore
099200130909     C************************************************************
099300130909     C     F4_Serie      BEGSR
099400130909     C*
099500130909     C                   Reset                   XTabDs
099600130909     C*
099700130909     C                   if        OPZG76      = '05' or
099800130909     C                             OPZG76      = '04'
099900130909     C                   Move      '02'          XTaOpz
100000130909     C                   else
100100130909     C                   Move      '01'          XTaOpz
100200130909     C                   endif
100300130909     C                   Move      XScSoc        XTaAzi
100400130909     C                   Move      XSCLIN        XTaLin
100500130909     C                   Z-Add     CURR          XTARIG
100600130909     C                   Z-Add     CURC          XTACOL
100700130909     C*
100800130909     C                   CLEAR                   KPJBU
100900130909     C                   MoveL     XTabDs        KPJBU
101000130909     C*
101100130909     C                   CALL      'ANGG13R'
101200130909     C                   PARM                    KPJBA
101300130909     C*
101400130909     C                   MoveL     KPJBU         XTabDs
101500130909     C*
101600130909     C     XTaKey        IFNE      *Blank
101700130909     C*
101800130909     C                   Select
101900130909     C     H1NMFL        WHENEQ    'D1SERDOC'
102000130909     C                   Move      XTaKey        D1SerDoc
102100130909     C     H1NMFL        WHENEQ    'D1SERREG'
102200130909     C                   Move      XTaKey        D1SerReg
102300130909     C     H1NMFL        WHENEQ    'D1SERPAR'
102400130909     C                   Move      XTaKey        D1SerPar
102500130909     C                   endSl
102600130909     C*
102700130909     C                   ENDIF
102800130909     C*
102900130909     C                   ENDSR
103000130909     C/EJECT
103100130909     C************************************************************
103200130909     C* Ricerca tipo documento
103300130909     C************************************************************
103400130909     C     F4_TpDoc      BEGSR
103500130909     C*
103600130909     C                   Reset                   ANA964DS
103700130909     C*
103800130909     C                   MoveL     '01'          Opz964
103900130909     C                   MoveL     'SDGSCF000'   Trc964
104000130909     C                   MoveL     'SCFTPDOC  '  Cam964
104100130909     C*
104200130909     C                   MoveL     ANA964DS      KPJBU
104300130909     C                   CALL      'ANA964R'
104400130909     C                   PARM                    KPJBA
104500130909     C                   MoveL     KPJBU         ANA964DS
104600130909     C*
104700130909     C                   if        Ret964     <> '2'
104800130909     C                   MoveL     Val964        D1TpDoc
104900130909     C                   endif
105000130909     C*
105100130909     C                   ENDSR
105200130909     C/EJECT
105300130909     C************************************************************
105400130909     C* Ricerca variazione della nota
105500130909     C************************************************************
105600130909     C     F4_VarNota    BEGSR
105700130909     C*
105800130909     C                   Reset                   ANA964DS
105900130909     C*
106000130909     C                   MoveL     '01'          Opz964
106100130909     C                   MoveL     'SDGSCF000'   Trc964
106200130909     C                   MoveL     'SCFVARNOTA'  Cam964
106300130909     C*
106400130909     C                   MoveL     ANA964DS      KPJBU
106500130909     C                   CALL      'ANA964R'
106600130909     C                   PARM                    KPJBA
106700130909     C                   MoveL     KPJBU         ANA964DS
106800130909     C*
106900130909     C                   if        Ret964     <> '2'
107000130909     C                   MoveL     Val964        D1VarNota
107100130909     C                   endif
107200130909     C*
107300130909     C                   ENDSR
107400130909     C/EJECT
107500130909     C************************************************************
107600130909     C* Ricerca natura dell'operazione
107700130909     C************************************************************
107800130909     C     F4_NatOpe     BEGSR
107900130909     C*
108000130909     C                   Reset                   ANA964DS
108100130909     C*
108200130909     C                   MoveL     '01'          Opz964
108300130909     C                   MoveL     'SDGSCF000'   Trc964
108301160208R195 C                   Select
108302160208R195 C     H1NMFL        WHENEQ    'D1NATOPE '
108400130909     C                   MoveL     'SCFNATOPE '  Cam964
108401160208R195 C     H1NMFL        WHENEQ    'D1NATOPER'
108402160208 "   C                   MoveL     'SCFNATOPER'  Cam964
108403160208 "   C                   MoveL     '1'           All964
108404160208R195 C                   endSl
108500130909     C*
108600130909     C                   MoveL     ANA964DS      KPJBU
108700130909     C                   CALL      'ANA964R'
108800130909     C                   PARM                    KPJBA
108900130909     C                   MoveL     KPJBU         ANA964DS
109000130909     C*
109100130909     C                   if        Ret964     <> '2'
109200130909     C                   Select
109300130909     C     H1NMFL        WHENEQ    'D1NATOPE '
109400130909     C                   MoveL     Val964        D1NatOpe
109500130909     C     H1NMFL        WHENEQ    'D1NATOPER'
109600130909     C                   MoveL     Val964        D1NatOpeR
109700130909     C                   endSl
109800130909     C                   endif
109900130909     C*
110000130909     C                   ENDSR
110100130909     C/EJECT
110200130926     C************************************************************
110300130926     C* Ricerca noleggio/leasing
110400130926     C************************************************************
110500130926     C     F4_Leasing    BEGSR
110600130926     C*
110700130926     C                   Reset                   ANA964DS
110800130926     C*
110900130926     C                   MoveL     '01'          Opz964
111000130926     C                   MoveL     'SDGSCF000'   Trc964
111100130926     C                   Select
111200130926     C     H1NMFL        WHENEQ    'D1LEASING'
111300130926     C                   MoveL     'SCFLEASING'  Cam964
111400130926     C     H1NMFL        WHENEQ    'D1LEASINR'
111500130926     C                   MoveL     'SCFLEASINR'  Cam964
111600130926     C                   endSl
111700131015R135 C                   MoveL     *On           All964
111800130926     C*
111900130926     C                   MoveL     ANA964DS      KPJBU
112000130926     C                   CALL      'ANA964R'
112100130926     C                   PARM                    KPJBA
112200130926     C                   MoveL     KPJBU         ANA964DS
112300130926     C*
112400130926     C                   if        Ret964     <> '2'
112500130926     C                   Select
112600130926     C     H1NMFL        WHENEQ    'D1LEASING'
112700130926     C                   MoveL     Val964        D1Leasing
112800130926     C     H1NMFL        WHENEQ    'D1LEASINR'
112900130926     C                   MoveL     Val964        D1LeasinR
113000130926     C                   endSl
113100130926     C                   endif
113200130926     C*
113300130926     C                   ENDSR
113400130926     C/EJECT
113500130909     C************************************************************
113600130909     C* Ricerca tipo operazione
113700130909     C************************************************************
113800130909     C     F4_TpOpe      BEGSR
113900130909     C*
114000130909     C                   Reset                   ANA964DS
114100130909     C*
114200130909     C                   MoveL     '01'          Opz964
114300130909     C                   MoveL     'SDGSCF000'   Trc964
114301160208R195 C                   Select
114302160208R195 C     H1NMFL        WHENEQ    'D1TPOPE  '
114400130909     C                   MoveL     'SCFTPOPE  '  Cam964
114401160208R195 C     H1NMFL        WHENEQ    'D1TPOPER '
114402160208 "   C                   MoveL     'SCFTPOPER '  Cam964
114403160208 "   C                   MoveL     '1'           All964
114404160208R195 C                   endSl
114500130909     C*
114600130909     C                   MoveL     ANA964DS      KPJBU
114700130909     C                   CALL      'ANA964R'
114800130909     C                   PARM                    KPJBA
114900130909     C                   MoveL     KPJBU         ANA964DS
115000130909     C*
115100130909     C                   if        Ret964     <> '2'
115200130925     C                   Select
115300130925     C     H1NMFL        WHENEQ    'D1TPOPE  '
115400130925     C                   MoveL     Val964        D1TpOpe
115500130925     C     H1NMFL        WHENEQ    'D1TPOPER '
115600130925     C                   MoveL     Val964        D1TpOpeR
115700130925     C                   endSl
115800130909     C                   endif
115900130909     C*
116000130909     C                   ENDSR
116100130909     C/EJECT
116200130909     C************************************************************
116300130909     C* Ricerca tipo importo
116400130909     C************************************************************
116500130909     C     F4_TpImp      BEGSR
116600130909     C*
116700130909     C                   Reset                   ANA964DS
116800130909     C*
116900130909     C                   MoveL     '01'          Opz964
117000130909     C                   MoveL     'SDGSCF000'   Trc964
117100130909     C                   MoveL     'SCFTPIMP  '  Cam964
117101160404R2213C* se Black List, il tipo importo è fondamentale
117102160404 "   C* altrimenti non serve
117103160404 "   C                   if        TpComG76    = 'B'
117104160404 "   C                   Move      *Off          All964
117105160404R2213C                   else
117200130918     C                   MoveL     *On           All964
117201160404R2213C                   endif
117300130909     C*
117400130909     C                   MoveL     ANA964DS      KPJBU
117500130909     C                   CALL      'ANA964R'
117600130909     C                   PARM                    KPJBA
117700130909     C                   MoveL     KPJBU         ANA964DS
117800130909     C*
117900130909     C                   if        Ret964     <> '2'
118000130909     C                   MoveL     Val964        D1TpImp
118100130909     C                   endif
118200130909     C*
118300130909     C                   ENDSR
118400130909     C/EJECT
118500130909     C************************************************************
118600130909     C* Origine
118700130909     C************************************************************
118800130909     C     F4_Origine    BEGSR
118900130909     C*
119000130909     C                   Reset                   ANA964DS
119100130909     C*
119200130909     C                   MoveL     '01'          Opz964
119300130909     C                   MoveL     'SDGSCF000'   Trc964
119400130909     C                   MoveL     'SCFORIGINE'  Cam964
119500130909     C                   MoveL     *Off          All964
119600130909     C*
119700130909     C                   MoveL     ANA964DS      KPJBU
119800130909     C                   CALL      'ANA964R'
119900130909     C                   PARM                    KPJBA
120000130909     C                   MoveL     KPJBU         ANA964DS
120100130909     C*
120200130909     C                   if        Ret964     <> '2'
120300130909     C                   MoveL     Val964        D1Origine
120400130909     C                   endif
120500130909     C*
120600130909     C                   ENDSR
120700130909     C/EJECT
120800000000     C************************************************************
120900000000     C* Ricerca tipo annullamento
121000000000     C************************************************************
121100000000     C     F4_TpAnn      BEGSR
121200000000     C*
121300130909     C                   Reset                   ANA964DS
121400000000     C*
121500130909     C                   MoveL     '01'          Opz964
121600130909     C                   MoveL     'SDGSCF000'   Trc964
121700130909     C                   MoveL     'SCFTPANN'    Cam964
121800000000     C*
121900130909     C                   MoveL     ANA964DS      KPJBU
122000130909     C                   CALL      'ANA964R'
122100130909     C                   PARM                    KPJBA
122200130909     C                   MoveL     KPJBU         ANA964DS
122300000000     C*
122400000000     C                   if        Ret964     <> '2'
122500130909     C                   MoveL     Val964        D1TpAnn
122600000000     C                   endif
122700000000     C*
122800000000     C                   ENDSR
122900000000     C/EJECT
123000130909     C************************************************************
123100130909     C* Ricerca quadro di presentazione
123200130909     C************************************************************
123300130909     C     F4_Quadro     BEGSR
123400130909     C*
123500130909     C                   Reset                   ANA964DS
123600130909     C*
123700130909     C                   MoveL     '01'          Opz964
123701160105R195 C*
123702160105 "   C                   Select
123703160105 "   C*
123704160105 "   C* Spesometro
123705160105R195 C                   When      TpComG76    = 'E'
123800130909     C                   MoveL     'SDGSCF000'   Trc964
123900130909     C                   MoveL     'SCFQUADRO'   Cam964
123901160105R195 C*
123902160105 "   C* Acquisti da San Marino
123903160105 "   C                   When      TpComG76    = 'S'
123904160105 "   C                   MoveL     'ANDIZ000 '   Trc964
123905160105 "   C                   MoveL     'SDGQUADRO1'  Cam964
123906160105 "   C*
123907160105 "   C* Black List
123908160105 "   C                   When      TpComG76    = 'B'
123909160105 "   C                   MoveL     'ANDIZ000 '   Trc964
123910160105 "   C                   MoveL     'SDGQUADRO2'  Cam964
123911160105 "   C                   endSl
123912160105 "   C*
123913160105R195 C                   MoveL     *On           All964
124000130909     C*
124100130909     C                   MoveL     ANA964DS      KPJBU
124200130909     C                   CALL      'ANA964R'
124300130909     C                   PARM                    KPJBA
124400130909     C                   MoveL     KPJBU         ANA964DS
124500130909     C*
124600130909     C                   if        Ret964     <> '2'
124700130909     C                   MoveL     Val964        D1Quadro
124800130909     C                   endif
124900130909     C*
125000130909     C                   ENDSR
125100130909     C/EJECT
125200130909     C************************************************************
125300000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
125400000000     C************************************************************
125500000000     C     GESPIE        BEGSR
125600000000     C*
125700000000     C                   SELECT
125800000000     C*
125900000000     C     $GEST         WHENEQ    'D1'
126000000000     C     $ULKD1        IFEQ      0
126100000000     C* riempimento opzioni
126200130909     C                   EXSR      F24D1
126300000000     C                   ELSE
126400000000     C                   WRITE     FMTZ1
126500000000     C                   ENDIF
126600000000     C*
126700000000     C     $GEST         WHENEQ    'D2'
126800000000     C     $ULKD1        IFEQ      0
126900000000     C* riempimento opzioni
127000130909     C                   EXSR      F24D2
127100000000     C                   ELSE
127200000000     C                   WRITE     FMTZ1
127300000000     C                   ENDIF
127400000000     C*
127500000000     C     $GEST         WHENEQ    'D3'
127600000000     C     $ULKD1        IFEQ      0
127700000000     C* riempimento opzioni
127800130909     C                   EXSR      F24D3
127900000000     C                   ELSE
128000000000     C                   WRITE     FMTZ1
128100000000     C                   ENDIF
128200000000     C                   ENDSL
128300000000     C*
128400000000     C                   ENDSR
128500000000     C/EJECT
128600000000     C************************************************************
128700000000     C* Chiamata programma con tasto personalizzato
128800000000     C************************************************************
128900000000     C     FPerD1        BEGSR
129000000000     C*
129100000000     C* ATTENZIONE NON PUÒ FUNZIONARE:
129200130909     C* la DS SDGSG76DS è più lunga della KPJBU e la chiamata
129300000000     C* al pgm con tasto personalizzato passa i dati tramite
129400130909     C* la KPJBA
129500000000     C*
129600000000     C* salvo la DS di ingresso
129700130909    >C                   MoveL     SDGSG76DS     $SaveDS
129800000000     C*
129900130909    >C                   MOVE      xscsoc        SocietaG76
130000130909     C* passare nei campi appositi della DS SDGSG76DS il valore
130100000000     C* della chiave della VL univoca
130200130911     C                   Eval      CFG76       = H1CF
130300130909     C                   Eval      SoggG76     = D1Sogg
130400130909     C                   Eval      PartIvaG76  = D1PartIva
130500130909     C                   Eval      CdFiscG76   = D1CdFisc
130600160404R2215X****               Eval      CogRagG76   = D1CogRag
130601160404R2215C                   Eval      CogRagG76   = H1CogRag
130700130909     C                   Eval      StatoG76    = D1Stato
130800000000     C*
130900130909    >C                   MoveL     SDGSG76DS     KPJBU
131000130909     C                   CALL      $PGF1(PosFun)                        22
131100130909     C                   PARM                    KPJBA
131200130909     C                   PARM                    NOMPGM
131300000000     C*
131400000000     C* ripristino la DS di ingresso
131500130909    >C                   MoveL     $SaveDS       SDGSG76DS
131600000000     C*
131700000000     C                   ENDSR
131800000000     C/EJECT
131900000000     C************************************************************
132000000000     C* GESTIONE VIDEO RECORD D1
132100000000     C************************************************************
132200000000     C     GESD1         BEGSR
132300000000     C* inizializzazione videata
132400000000   B1C     $INZD1        IFEQ      *ON
132500130909     C                   EXSR      INZD1
132600130909     C                   MOVE      *Off          $INZD1
132700000000   E1C                   ENDIF
132800000000     C* emissione piede videata se proveniente da altra
132900000000     C* salvataggio valore formato di provenienza
133000000000   B1C     $LASTV        IFNE      'D1'
133100000000     C                   WRITE     FMTT1
133200130909     C                   EXSR      GESPIE
133300000000     C                   MOVE      $LASTV        $LASTG
133400000000     C                   MOVE      'D1'          $LASTV
133500000000   E1C                   ENDIF
133600000000     C*
133700000000     C* se esistono errori sulla videata
133800000000     C* emetto la write del formato a indicatori spenti per vedere
133900000000     C* le eventuali decodifiche
134000130926   B1C                   if        *IN99       = *On      and
134100130926     C                             $GEST        = 'D1'
134200000000     C                   MOVEA     *IN           WIN
134300000000     C                   MOVE      *ALL'0'       IN5098           49
134400000000     C                   MOVEA     IN5098        *IN(50)
134500000000     C                   WRITE     FMTD1
134600000000     C                   MOVEA     WIN           *IN
134700000000   E1C                   ENDIF
134800000000     C* Inizializzazione indice tasti funzione personalizzati
134900130909     C                   Eval      PosFun = 25
135000000000     C*
135100000000   B1C                   SELECT
135200000000     C* Annullamento
135300130909    >C     OPZG76        WHENEQ    '04'
135400000000     C* Visualizzazione
135500130909    >C     OPZG76        OREQ      '05'
135600000000     C                   WRITE     FMTD1
135700000000     C                   EXFMT     PROTECT
135800000000     C* Immissione/Modifica/Copia
135900000000   X1C                   OTHER
136000000000     C                   EXFMT     FMTD1
136100000000   E1C                   ENDSL
136200000000     C*
136300000000     C* azzero la posizione del cursore
136400000000     C                   Z-ADD     0             H1RIGA
136500000000     C                   Z-ADD     0             H1COLO
136600000000     C*
136700000000     C* controllo abilitazione tasti funzione
136800130909     C                   EXSR      CTRKD1
136900000000     C*
137000130909   B1C     *IN99         IFEQ      *Off
137100000000     C*
137200000000   B2C                   SELECT
137300000000     C* ROLLUP
137400000000    >C     $TASTO        WHENEQ    ROLLUP
137500000000    >C                   MOVE      'D2'          $GEST
137600000000     C* F2=Decodifica
137700000000     C     $TASTO        WHENEQ    F02
137800130909     C                   EXSR      F02D1
137900000000     C* F3=Fine
138000000000     C     $TASTO        WHENEQ    F03
138100130909     C                   EXSR      F03D1
138200000000     C* F4=Ricerca
138300000000     C     $TASTO        WHENEQ    F04
138400130909     C                   EXSR      SEARCH
138500000000     C* F5=Refresh
138600000000     C     $TASTO        WHENEQ    F05
138700130909     C                   EXSR      F05D1
138800000000     C* F8=Successivo
138900000000     C     $TASTO        WHENEQ    F08
139000130909     C                   EXSR      F08D1
139100130923     C* F11=Forzatura
139200130923     C     $TASTO        WHENEQ    F11
139300130923     C                   EXSR      F11D1
139400000000     C* F12=Ritorno
139500000000     C     $TASTO        WHENEQ    F12
139600130909     C                   EXSR      F12D1
139700000000     C* F24=Altri tasti
139800000000     C     $TASTO        WHENEQ    F24
139900130909     C                   EXSR      F24D1
140000000000     C* Personalizzata
140100130909     C* NB chiamata NON possibile perchè SDGSG76DS più lunga
140200130909     C* di KPJBU 256 caratteri                          a
140300000000     C**** $PGF1(PosFun) WHENNE    *BLANK
140400130909     C****               EXSR      FPerD1
140500000000     C*
140600000000   X2C                   OTHER
140700130909     C                   EXSR      CTRD1
140800000000     C* aggiungere le chiamate ai controlli delle videate supplementari
140900130909    >C     *IN99         IFEQ      *Off
141000130909    >C                   EXSR      CTRD2
141100130926   B1C                   if        *IN99       = *On
141200000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
141300000000    >C* indicatori uguali in videate differenti, genererei conflitti
141400000000    >C                   MOVE      *ALL'0'       IN5098           49
141500000000    >C                   MOVEA     IN5098        *IN(50)
141600130909    >C                   SetOn                                        9996                       96
141700000000    >C                   ENDIF
141800000000    >C                   ENDIF
141900000000     C* aggiungere le chiamate ai controlli delle videate supplementari
142000130909    >C     *IN99         IFEQ      *Off
142100130923    >C                   EXSR      CTRD3
142200130926   B1C                   if        *IN99       = *On
142300000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
142400000000    >C* indicatori uguali in videate differenti, genererei conflitti
142500000000    >C                   MOVE      *ALL'0'       IN5098           49
142600000000    >C                   MOVEA     IN5098        *IN(50)
142700130909    >C                   SetOn                                        9996                       96
142800000000    >C                   ENDIF
142900000000    >C                   ENDIF
143000000000     C*
143100000000   B3C     $TASTO        IFEQ      F06
143200130909     C     *IN99         ANDEQ     *Off
143300000000     C* eseguo aggiornamento
143400130909     C                   EXSR      AGGANA
143500000000     C* eseguo operazioni del dopo-aggiornamento
143600130909   B4C     *IN99         IFEQ      *Off
143700130909     C                   EXSR      GESAGG
143800000000   E4C                   ENDIF
143900000000   E3C                   ENDIF
144000000000     C*
144100000000   E2C                   ENDSL
144200000000     C*
144300000000   E1C                   ENDIF
144400000000     C*
144500000000     C                   ENDSR
144600000000     C/EJECT
144700000000     C************************************************************
144800000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
144900000000     C************************************************************
145000000000     C     CTRKD1        BEGSR
145100000000     C*
145200130909     C                   SetOff                                       99
145300000000     C                   MOVEA     $FC1          $FC
145400130909     C                   EXSR      CTRKEY
145500000000     C     *IN99         IFEQ      *ON
145600130909     C                   SetOn                                        97
145700000000     C                   ENDIF
145800000000     C*
145900000000     C                   ENDSR
146000000000     C/EJECT
146100000000     C************************************************************
146200000000     C* INIZIALIZZAZIONE VIDEATA DATI
146300000000     C************************************************************
146400000000     C     INZD1         BEGSR
146500000000     C*
146600000000     C                   CLEAR                   FMTD1
146700000000     C                   CLEAR                   FMTD2
146800000000     C                   CLEAR                   FMTD3
146900110822     C*
147000130924     C*----------------------------------------------------
147100130924     C* dati di raggruppamento
147200130924     C*
147300151203R195 X**** mostro gli estremi della registrazione se comunicazione
147400151203 "   X**** a dettaglio, i numeri delle operazioni aggregate se
147500151203R195 X**** aggregata
147600131017R136 X****               if        FormatoA08  = 'D'
147700151203R195 X****               Eval      *In13       = *On
147800131017R136 X****               else
147900151203R195 X****               Eval      *In14       = *On
148000131017R136 X****               endif
148100151203R195 X*ex R135
148200151203 "   X*ex R135 mostro comunque gli estremi di registrazione se documenti di
148300151203 "   X*ex R135 riepilogo comunicati in maniera dettagliata
148400151203 "   X*ex R135           if        DocRieA08   = 'D'
148500151203 "   X*ex R135           Eval      *In13       = *On
148600151203R195 X*ex R135           endif
148601151217R195 C*
148602151217 "   C                   if        TpComG76 <> 'B'
148603151217 "   C                   Eval      *in13       = *On
148604151217 "   C                   Eval      *in14       = *On
148605160201 "   C                   Endif
148606160201 "   C*
148607160201 "   C                   Select
148608160201 "   C                   When      TpComG76  = 'E'      or
148609160201 "   C                             TpComG76  = 'S'
148610160201 "   C                   Eval      *in15       = *On
148611160201 "   C                   When      TpComG76  = 'B'
148612160201 "   C                   Eval      *in18       = *On
148613160201R195 C                   endSl
148700130924     C*----------------------------------------------------
148800130924     C*
148900130909     C                   Clear                   $TpCom
149000130909     C                   Clear                   $Anno
149100130909     C                   Clear                   $Mese
149200111012     C                   Clear                   $Sys
149300000000     C                   Clear                   $NrAsReg
149400110826     C                   Clear                   $NrRigaM
149500110826     C                   Clear                   $NrRiga
149600000000     C*
149700130924     C                   Eval      D1On1       = XScOn
149800130924     C                   Eval      D1On2       = XScOn
149900130909     C                   Eval      D1On3       = XScOn
150000130909     C                   Eval      D1On4       = XScOn
150100130909     C                   Eval      D1On5       = XScOn
150200130924     C                   Eval      D1Off1      = XScOff
150300130924     C                   Eval      D1Off2      = XScOff
150400130909     C                   Eval      D1Off3      = XScOff
150500130909     C                   Eval      D1Off4      = XScOff
150600130909     C                   Eval      D1Off5      = XScOff
150700000000     C*
150800130909     C                   Eval      D1Anno      = AnnoG76
150900130911     C                   Eval      H1CF        = CFG76
151000130911     C                   Eval      D1TPN       = TPNG76
151100130909     C                   Eval      D1Sogg      = SoggG76
151200131206R141 C                   Eval      D1Causale   = CausaleG76
151300130909     C                   Eval      D1PartIva   = PartIvaG76
151400130909     C                   Eval      D1CdFisc    = CdFiscG76
151500130909     C                   Eval      D1CogRag    = CogRagG76
151501160404R2215C                   Eval      H1CogRag    = CogRagG76
151600130909     C                   Eval      H1Nome      = NomeG76
151700130909     C                   Eval      H1DtNasc    = DtNascG76
151800130909     C                   Eval      H1LocNasc   = LocNascG76
151900130909     C                   Eval      H1PrvNasc   = PrvNascG76
152000130909     C                   Eval      D1Stato     = StatoG76
152100130924     C                   Eval      D1CodUIC    = CodUICG76
152200130909     C                   Eval      H1Localit   = LocalitG76
152300130909     C                   Eval      H1IndirE    = IndirEG76
152400140401R133 C                   Select
152401151201R195 C* Acquisti da San Marino
152500151201R133 C                   When      TpComg76    = 'S'
152600140401 "   C                   Eval      *In11       = *On
152700140401 "   C                   Eval      D1Mese      = MeseG76
152800140401 "   C                   Other
152900140401 "   C                   Eval      *In11       = *Off
153000140401R133 C                   endSl
153100140401     C*
153200000000     C* per tutte le opzioni NON immissione
153300130909     C     OPZG76        IFNE      '01'
153400140401R133 C***                Eval      SCFTpCom    = 'E'
153500140401R133 C                   Eval      SCFTpCom    = TpComG76
153600130909     C                   Eval      SCFAnno     = AnnoG76
153700130909     C                   Eval      SCFMese     = MeseG76
153800130909     C                   Eval      SCFSys      = SysG76
153900130909     C                   Eval      SCFNrAsReg  = NrAsRegG76
154000130909     C                   Eval      SCFNrRigaM  = NrRigaMG76
154100130909     C                   Eval      SCFNrRiga   = NrRigaG76
154200000000     C*
154300130909     C     *IN22         DOUEQ     *Off
154400000000     C     MSGRTN        ORNE      'R'
154500000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
154600130909     C     OPZG76        IFEQ      '02'
154700130909    >C     OPZG76        OREQ      '04'
154800130919     C     K07SCF11      CHAIN     SDGSCF11L                          2122
154900000000     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
155000000000   X3C                   ELSE
155100130919     C     K07SCF11      CHAIN(N)  SDGSCF11L                          2122
155200000000   E3C                   ENDIF
155300000000     C*
155400000000     C* Gestione RCD allocato
155500000000   B3C     *IN22         IFEQ      *ON
155600130909    >C                   MoveL     'SDGSCF11L'   MSGFIL
155700130909     C                   EXSR      MSGPGM
155800000000   E3C                   ENDIF
155900000000   E2C                   ENDDO
156000000000     C*
156100000000     C* se il rcd da gestire non viene trovato
156200000000     C* oppure è allocato da altro pgm
156300000000     C* ritorno al pgm guida avvertendolo dell'errore
156400000000   B2C     *IN21         IFEQ      *ON
156500000000     C     *IN22         OREQ      *ON
156600000000     C                   MOVE      *ON           $FINE
156700130909    >C                   MOVE      '1'           ERRG76
156800000000     C*
156900000000     C* altrimenti
157000000000   X2C                   ELSE
157100000000     C* riempio la videata
157200130909     C                   EXSR      RIED1
157300000000    >C* Aggiungere le routine di riempimento dei record succcessivi
157400130909    >C                   EXSR      RIED2
157500000000    >C* Aggiungere le routine di riempimento dei record succcessivi
157600130909    >C                   EXSR      RIED3
157700000000     C* richiamo routine dei ctrl per decodificarla
157800130909     C                   EXSR      CTRD1
157900000000    >C* Aggiungere le routine di controllo dei record succcessivi
158000130909    >C                   EXSR      CTRD2
158100130923    >C                   EXSR      CTRD3
158200000000     C                   MOVE      *ALL'0'       IN5098
158300000000     C                   MOVEA     IN5098        *IN(50)
158400000000   E2C                   ENDIF
158500000000     C*
158600000000     C* per immissione
158700000000   X1C                   ELSE
158800000000     C*
158900000000     C* riempio con i default di immissione  (se reperiti)
159000000000     C                   IF        $FlgDef     = *on
159100130909     C                   EXSR      RIEDEF
159200000000     C                   ENDIF
159300000000     C*
159400000000     C* imposto i campi chiave ricevuti anche se si è in immissione
159500000000     C* perchè potrebbero essere stati passati dei default di chiave
159600000000     C*
159700000000     C* decodifico le costanti
159800130909     C                   EXSR      F02D1
159900000000   E1C                   ENDIF
160000110819     C*
160100130909     C     OPZG76        IFEQ      '01'
160200130909    >C     OPZG76        OREQ      '03'
160300130909     C                   Eval      D1Origine   = 'M'
160400110819     C*
160500110819     C                   if        $NrAsReg    = 0
160600110819     C* Reperisco il numero assoluto della registrazione fittizia
160700130909     C                   EXSR      RepNrAss
160800110819     C                   endif
160900110819     C                   endif
161000000000     C*
161100000000     C* imposto quegli attributi della videata che sono modificabili
161200000000     C* al richiamo del determinato pgm
161300130909     C                   EXSR      ATRD1
161400000000     C*
161500000000     C                   ENDSR
161600000000     C/EJECT
161700000000     C************************************************************
161800000000     C* RIEMPIMENTO VIDEATA  D1
161900000000     C************************************************************
162000000000     C     RIED1         BEGSR
162100000000     C*
162200000000     C* impostare i campi della videata dai campi del file
162300130909     C                   if        OPZG76      = '02'
162400130909     C                   Eval      $TpCom      = SCFTpCom
162500130909     C                   Eval      $Anno       = SCFAnno
162600130909     C                   Eval      $Mese       = SCFMese
162700130909     C                   Eval      $Sys        = SCFSys
162800130909     C                   Eval      $NrAsReg    = SCFNrAsReg
162900130909     C                   Eval      $NrRigaM    = SCFNrRigaM
163000130909     C                   Eval      $NrRiga     = SCFNrRiga
163100000000     C                   endif
163200000000     C*
163300130911     C                   Eval      H1CF        = SCFCF
163400130923     C*
163500130923     C                   Eval      D1PartIva   = SCFPartIva
163600130923     C                   Eval      D1CdFisc    = SCFCdFisc
163700130923     C                   Eval      D1CogRag    = SCFCogRag
163701160404R2215C                   Eval      H1CogRag    = SCFCogRag
163800130923     C                   Eval      D1Stato     = SCFStato
163900130923     C*
164000130923     C                   Eval      D1TpDoc     = SCFTpDoc
164100130923     C*
164200130923     C                   Eval      D1VarNota   = SCFVarNota
164300130923     C                   Eval      D1TpOpe     = SCFTpOpe
164400130923     C                   Eval      D1TpImp     = SCFTpImp
164500130923     C*
164600130925     C                   if        OPZG76     <> '03'
164700130923     C                   Eval      D1ImpOpeO   = SCFImpOpeO
164800130923     C                   Eval      D1IvaOpeO   = SCFIvaOpeO
164900130925     C                   endif
165000130923     C                   Eval      D1ImpOpe    = SCFImpOpe
165100130923     C                   Eval      D1IvaOpe    = SCFIvaOpe
165101160201R195 C                   Eval      D1ImpSogl   = SCFImpSogl
165102160201R195 C                   Eval      D1IvaSogl   = SCFIvaSogl
165200130923     C*
165300130923     C                   if        SCFMargine  = '1'
165400130923     C                   Eval      D1Margine   = XScOn
165500130923     C                   else
165600130923     C                   Eval      D1Margine   = XScOff
165700130923     C                   endif
165800130923     C*
165900130923     C                   if        SCFReverse  = '1'
166000130923     C                   Eval      D1Reverse   = XScOn
166100130923     C                   else
166200130923     C                   Eval      D1Reverse   = XScOff
166300130923     C                   endif
166400130923     C*
166500130923     C                   if        SCFAutofat  = '1'
166600130923     C                   Eval      D1Autofat   = XScOn
166700130923     C                   else
166800130923     C                   Eval      D1Autofat   = XScOff
166900130923     C                   endif
167000130923     C*
167100130926     C                   Eval      D1Leasing   = SCFLeasing
167200130923     C*
167300130923     C                   Eval      D1NatOpe    = SCFNatOpe
167400130923     C*
167500130923     C                   if        SCFAnn      = '1'
167600130923     C                   Eval      D1Ann       = XScOn
167700130923     C                   else
167800130923     C                   Eval      D1Ann       = XScOff
167900130923     C                   endif
168000130923     C                   Eval      D1TpAnn     = SCFTpAnn
168100110902     C*
168200000000     C                   ENDSR
168300000000     C************************************************************
168400000000     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
168500000000     C************************************************************
168600000000     C     ATRD1         BEGSR
168700000000     C*
168800000000    >C* in questa routine bisogna attivare indicatori o campi programma
168900000000    >C* che gestiscono gli attributi di uno più campi di questa videata;
169000000000    >C* attenzione : non è questo il posto in cui eseguire controlli,
169100000000    >C*  dovrebbero già arrivare dei flag
169200000000     C*
169300130909     C                   SetOff                                       02
169400000000     C*
169500000000     C* in caso di manutenzione
169600130909    >C     OPZG76        IFEQ      '02'
169700000000     C* proteggo i campi chiave
169800130909     C                   SetOn                                        02        PR campi key
169900000000   E3C                   ENDIF
170000000000     C*
170100000000     C                   ENDSR
170200000000     C/EJECT
170300000000     C************************************************************
170400000000     C* RIEMPIMENTO CAMPI DAI DEFAULT
170500000000     C************************************************************
170600000000     C     RIEDEF        BEGSR
170700000000     C*
170800130924     C                   if        QuadroG76   = 'SA'    or
170900130924     C                             QuadroG76   = 'DF'    or
171000131017R136 C                             QuadroG76   = 'TU'    or
171100130924     C                             QuadroG76   = *Blank
171200130918     C                   Eval      D1TpDoc     = 'S'
171300130924     C                   endif
171400130918     C                   Eval      D1TpOpe     = 'A'
171500130909     C                   Eval      D1NoIva     = XScOff
171600130909     C                   Eval      D1Origine   = 'M'
171700130911     C                   Eval      D1Margine   = XScOff
171800130911     C                   Eval      D1Reverse   = XScOff
171900130911     C                   Eval      D1Autofat   = XScOff
172000130926     C                   Eval      D1Leasing   = '0'
172001151204R195 C* Spesometro o Acquisti da San Marino
172002151211 "   C                   if        TpComG76    = 'E'     or
172003151211R195 C                             TpComG76    = 'S'
172100130911     C                   Eval      D1NatOpe    = BeniSrvA08
172101151204R195 C                   endif
172102151204 "   C*
172103151204 "   C* Black List
172104151211 "   C                   if        TpComG76    = 'B'
172105151211 "   C                   Eval      D1Natope    = BeniSrvA11
172106151204 "   C                   endif
172107151204R195 C*
172200130911     C                   Eval      D1TpAnn     = *Blank
172300130909     C                   Eval      D1Ann       = XScOff
172400130924     C                   Eval      D1Quadro    = QuadroG76
172401151204R195 C* se Spesometro
172402151211R195 C                   if        TpComG76    = 'E'
172500130927     C                   if        D1Quadro    = *Blank  and
172600130927     C                             D1TpDoc     = 'S'     and
172700130927     C                             FormatoA08  = 'A'
172800131017R136 C                   if        D1CdFisc   <> *Blank
172900130927     C                   Eval      D1Quadro    = 'SA'
173000131017R136 C                   else
173100131017 "   C                   Eval      D1Quadro    = 'TU'
173200131017R136 C                   endif
173300130927     C                   endif
173400130927     C                   if        D1Quadro    = *Blank  and
173500130927     C                             D1TpDoc     = 'S'     and
173600130927     C                             FormatoA08  = 'D'
173700131017R136 C                   if        D1CdFisc   <> *Blank
173800130927     C                   Eval      D1Quadro    = 'DF'
173900131017R136 C                   else
174000131017 "   C                   Eval      D1Quadro    = 'TU'
174100131017R136 C                   endif
174200130927     C                   endif
174201151207R195 C                   endif
174202151207R195 C*
174203151201R195 C* Acquisti da San Marino
174300140401R133 C                   if        TpComG76   =  'S'    and
174400140401R133 C                             OpzG76     =  '01'
174500140401R133 C                   Eval      D1Quadro   = 'SE'
174600140401R133 C                   endif
174700000000     C*
174800130919     C* mi posiziono sul primo record inserito per recuperare
174900130919     C* il soggetto e il tipo natura giuridica digitati in
175000130919     C* inserimento dei dati anagrafici
175100130919     C                   Eval      SCFSOCIETA  = SOCIETAG76
175200140401R133 C****               Eval      SCFTPCOM    = 'E'
175300140401R133 C                   Eval      SCFTPCOM    = TPCOMG76
175400130919     C                   Eval      SCFANNO     = ANNOG76
175500130919     C                   Eval      SCFMESE     = MESEG76
175600130919     C                   Eval      SCFCOGRAG   = COGRAGG76
175700130919     C                   Eval      SCFNOME     = NOMEG76
175800130919     C                   Eval      SCFDTNASC   = DTNASCG76
175900130919     C                   Eval      SCFLOCNASC  = LOCNASCG76
176000130919     C                   Eval      SCFPRVNASC  = PRVNASCG76
176100130919     C                   Eval      SCFSTATO    = STATOG76
176200130919     C                   Eval      SCFCODUIC   = CODUICG76
176300130919     C                   Eval      SCFLOCALIT  = LOCALITG76
176400130919     C                   Eval      SCFINDIRE   = INDIREG76
176500130919     C                   Eval      SCFPARTIVA  = PARTIVAG76
176600130919     C                   Eval      SCFCDFISC   = CDFISCG76
176700130919     C*
176800130919     C     K15SCF01      SETLL     SDGSCF01L
176900130919     C     K15SCF01      READE     SDGSCF01L                              21
177000130919     C                   if        *In21       = *Off
177100130919     C                   Eval      D1TpN       = SCFTpN
177200130919     C                   Eval      D1Sogg      = SCFSogg
177300130919     C                   endif
177400130919     C*
177500000000     C                   ENDSR
177600000000     C/EJECT
177700000000     C************************************************************
177800000000     C* GESTIONE F02 VIDEO D1
177900000000     C************************************************************
178000000000     C     F02D1         BEGSR
178100000000     C*
178200000000     C* per decodificare la videata richiamo la routine dei ctrl
178300000000     C* e poi disattivo gli eventuali messaggi di errore
178400130909     C                   EXSR      CTRD1
178500000000     C*
178600000000     C                   MOVE      *ALL'0'       IN5098
178700000000     C                   MOVEA     IN5098        *IN(50)
178800000000     C*
178900000000     C                   ENDSR
179000000000     C/EJECT
179100000000     C************************************************************
179200000000     C* GESTIONE F03 VIDEO D1
179300000000     C************************************************************
179400000000     C     F03D1         BEGSR
179500000000     C*
179600000000     C                   MOVE      *ON           $FINE
179700130909    >C                   MOVE      '1'           RETG76
179800000000     C* fine programma
179900130909     C                   EXSR      ENDPGM
180000000000     C*
180100000000     C                   ENDSR
180200000000     C/EJECT
180300000000     C************************************************************
180400000000     C* GESTIONE F05 VIDEO D1
180500000000     C************************************************************
180600000000     C     F05D1         BEGSR
180700000000     C*
180800000000     C                   MOVE      *ON           $INZD1
180900000000     C*
181000000000     C                   ENDSR
181100000000     C/EJECT
181200000000     C************************************************************
181300000000     C* GESTIONE F08 VIDEO D1
181400000000     C************************************************************
181500000000     C     F08D1         BEGSR
181600000000     C*
181700000000     C                   MOVE      *ON           $FINE
181800130909    >C                   MOVE      '0'           RETG76
181900000000     C*
182000000000     C                   ENDSR
182100000000     C/EJECT
182200000000     C************************************************************
182300130923     C* GESTIONE F11 VIDEO D1
182400000000     C************************************************************
182500130923     C     F11D1         BEGSR
182600000000     C*
182700130923     C* deduco i dati di raggruppamento
182800130923     C                   EXSR      RepG12Ksc
182801160104R195 C                   Select
182802160104 "   C* Spesometro
182803160104R195 C                   When      TpComG76    = 'E'
182900130923     C                   EXSR      DefQuadri
182901160104R195 C*
182902160104 "   C* Acquisti da San Marino
182903160104 "   C                   When      TpComG76    = 'S'
182904160104 "   C                   EXSR      DefQuadroSE
182905160104 "   C*
182906160104 "   C* Black List
182907160104 "   C                   When      TpComG76    = 'B'
182908160104 "   C                   EXSR      DefQuadroBL
182909160104R195 C                   endSl
183000000000     C*
183100000000     C                   ENDSR
183200000000     C/EJECT
183300130923     C************************************************************
183400130923     C* GESTIONE F12 VIDEO D1
183500130923     C************************************************************
183600130923     C     F12D1         BEGSR
183700130923     C*
183800130923     C                   MOVE      *ON           $FINE
183900130923    >C                   MOVE      '2'           RETG76
184000130923     C*
184100130923     C                   ENDSR
184200130923     C/EJECT
184300000000     C************************************************************
184400000000     C* GESTIONE F24 VIDEO D1
184500000000     C************************************************************
184600000000     C     F24D1         BEGSR
184700000000     C*
184800000000     C                   MOVEA     $FX1          $FX
184900000000     C                   MOVEA     $FC1          $FC
185000000000     C                   MOVEA     $FR1          $FR
185100000000     C                   Z-ADD     $ULKD1        $ULK
185200000000     C                   Z-ADD     $D1L01        $L01
185300000000     C                   Z-ADD     $D1L02        $L02
185400130909     C                   EXSR      RIEKEY
185500000000     C                   Z-ADD     $ULK          $ULKD1            3 0
185600000000     C                   MOVEA     $FC           $FC1
185700130909     C                   MoveL     $KEY1         Z1KE1
185800130909     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
185900130909     C                   MoveL     $KEY2         Z1KE2
186000000000     C                   WRITE     FMTZ1
186100000000     C*
186200000000     C                   ENDSR
186300000000     C/EJECT
186400000000     C************************************************************
186500000000     C* CONTROLLO VIDEATA
186600000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
186700000000     C* 95 96 97 98
186800000000     C************************************************************
186900000000     C     CTRD1         BEGSR
187000000000     C*
187100130909     C                   SetOff                                       99
187200000000     C*
187300000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
187400000000     C*
187500130909     C     OPZG76        IFNE      '04'
187600130918     C*
187700130918     C                   EXSR      CtrCF
187800130918     C*
187900130918     C                   EXSR      CtrTpDoc
188000130918     C                   EXSR      CtrVarNota
188100130918     C                   EXSR      CtrTpOpe
188200130918     C                   EXSR      CtrTpImp
188300130918     C*
188400130918     C                   EXSR      CtrImporti
188500130918     C*
188600130918     C                   EXSR      CtrMargine
188700130918     C                   EXSR      CtrReverse
188800130918     C                   EXSR      CtrAutofat
188900130918     C                   EXSR      CtrLeasing
189000130918     C*
189100130918     C                   EXSR      CtrNatOpe
189200130918     C*
189300130918     C                   EXSR      CtrAnn
189400130918     C                   EXSR      CtrTpAnn
189500000000     C*
189600000000     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
189700130909     C     OPZG76        IFEQ      '01'
189800130909    >C     OPZG76        OREQ      '03'
189900130909     C                   EXSR      CtrExist1
190000000000     C                   ENDIF
190100000000     C*
190200000000     C                   ENDIF
190300000000     C*
190400000000     C                   ENDSR
190500000000     C/EJECT
190600000000     C***********************************************************
190700130911     C* Decofico  la sottonatura
190800000000     C***********************************************************
190900130911     C     CtrCF         BEGSR
191000000000     C*
191100130911     C                   Clear                   D1CFD
191200130911     C                   Clear                   X07ValDs
191300130911     C*
191400130911     C                   Move      '1'           X07Ric
191500130911     C                   Move      *On           X07All
191600130911     C                   MoveL     'SDGSCF000'   X07TRc
191700130911     C                   MoveL     'SCFCF    '   X07Cam
191800130911     C                   MoveL     H1CF          X07VAL
191900130911     C*
192000130911     C                   CALLB     'X07VALR'
192100130911     C                   PARM                    X07ValDs
192200130911     C*
192300130911     C     X07ERR        IFEQ      *Off
192400130911     C                   MoveL     X07DES        D1CFD
192500130911     C                   endif
192600130911     C*
192700130911     C                   ENDSR
192800130911     C/EJECT
192900130918     C************************************************************
193000130918     C* Controllo tipo documento
193100130918     C************************************************************
193200130918     C     CtrTpDoc      BEGSR
193300130918     C*
193400130918     C                   Clear                   D1TpDocD
193500130918     C                   Clear                   X07ValDs
193600130918     C*
193700130918     C                   Move      '1'           X07Ric
193800130918     C                   Move      *On           X07All
193900130918     C                   MoveL     'SDGSCF000'   X07TRc
194000130918     C                   MoveL     'SCFTPDOC  '  X07Cam
194100130918     C                   MoveL     D1TpDoc       X07VAL
194200130918     C*
194300130918     C                   CALLB     'X07VALR'
194400130918     C                   PARM                    X07ValDs
194500130918     C*
194600130918     C     X07ERR        IFEQ      *Off
194700130918     C                   MoveL     X07DES        D1TpDocD
194800130918     C                   else
194900130918     C                   SetOn                                        5099
195000130918     C                   endif
195100130918     C*
195200130918     C                   ENDSR
195300130918     C/EJECT
195400130918     C************************************************************
195500130918     C* Controllo variazione della nota
195600130918     C************************************************************
195700130918     C     CtrVarNota    BEGSR
195800130918     C*
195900130918     C                   Clear                   D1VarNotaD
196000130918     C                   Clear                   X07ValDs
196100130918     C*
196200130918     C                   if        D1TpDoc     = 'N'
196300130918     C                   Move      '1'           X07Ric
196400130918     C                   Move      *On           X07All
196500130918     C                   MoveL     'SDGSCF000'   X07TRc
196600130918     C                   MoveL     'SCFVARNOTA'  X07Cam
196700130918     C                   MoveL     D1VarNota     X07VAL
196800130918     C*
196900130918     C                   CALLB     'X07VALR'
197000130918     C                   PARM                    X07ValDs
197100130918     C*
197200130918     C     X07ERR        IFEQ      *Off
197300130918     C                   MoveL     X07DES        D1VarNotaD
197400130918     C                   else
197500130918     C                   SetOn                                        5199
197600130918     C                   endif
197700130918     C*
197800130918     C                   else
197900130918     C                   Eval      D1VarNota   = *Blank
198000130918     C*
198100130918     C                   endif
198200130918     C*
198300130918     C                   ENDSR
198400130918     C/EJECT
198500130918     C************************************************************
198600130918     C* Controllo tipo operazione
198700130918     C************************************************************
198800130918     C     CtrTpOpe      BEGSR
198900130918     C*
199000130918     C                   Clear                   D1TpOpeD
199100130918     C                   Clear                   X07ValDs
199200130918     C*
199300130918     C                   Move      '1'           X07Ric
199400130918     C                   Move      *On           X07All
199500130918     C                   MoveL     'SDGSCF000'   X07TRc
199600130918     C                   MoveL     'SCFTPOPE  '  X07Cam
199700130918     C                   MoveL     D1TpOpe       X07VAL
199800130918     C*
199900130918     C                   CALLB     'X07VALR'
200000130918     C                   PARM                    X07ValDs
200100130918     C*
200200130918     C     X07ERR        IFEQ      *Off
200300130918     C                   MoveL     X07DES        D1TpOpeD
200400130918     C                   else
200500130918     C                   SetOn                                        5299
200600130918     C                   endif
200700130918     C*
200800130918     C                   ENDSR
200900130918     C/EJECT
201000130918     C************************************************************
201100130918     C* Controllo tipo importo
201200130918     C************************************************************
201300130918     C     CtrTpImp      BEGSR
201400130918     C*
201500130918     C                   Clear                   D1TpImpD
201600130918     C                   Clear                   X07ValDs
201700130918     C*
201800130918     C                   Move      '1'           X07Ric
201801160329R2201C* se Black List, il tipo importo è fondamentale
201802160329 "   C* altrimenti non serve
201803160329 "   C                   if        TpComG76    = 'B'
201804160329 "   C                   Move      *Off          X07All
201805160329R2201C                   else
201900130918     C                   Move      *On           X07All
201901160329R2201C                   endif
202000130918     C                   MoveL     'SDGSCF000'   X07TRc
202100130918     C                   MoveL     'SCFTPIMP  '  X07Cam
202200130918     C                   MoveL     D1TpImp       X07VAL
202300130918     C*
202400130918     C                   CALLB     'X07VALR'
202500130918     C                   PARM                    X07ValDs
202600130918     C*
202700130918     C     X07ERR        IFEQ      *Off
202800130918     C                   MoveL     X07DES        D1TpImpD
202900130918     C                   else
203000130918     C                   SetOn                                        5399
203100130918     C                   endif
203200130918     C*
203300130918     C                   ENDSR
203400130918     C/EJECT
203500130918     C************************************************************
203600130918     C* Controllo importi
203700130918     C************************************************************
203800130918     C     CtrImporti    BEGSR
203900130918     C*
204000130919     C* se gli importi dell'operazione NON
204100130930     C* sono valorizzati, imposto i valori di origine
204200130918     C                   if        D1ImpOpe    = 0         and
204300130918     C                             D1IVAOpe    = 0
204400130918     C                   Eval      D1ImpOpe    = D1ImpOpeO
204500130918     C                   Eval      D1IVAOpe    = D1IVAOpeO
204600130918     C                   endif
204700130918     C*
204800130918     C                   if        D1TpDoc       = 'N'
204900130918     C*
205000130918     C* il segno dell'importo deve essere coerente con la variazione
205100130919     C* della nota e il tipo operazione
205200130927     C                   if        (D1TpOpe     = 'A'   and
205300130927     C                              D1VarNota   = 'D'      ) or
205400130927     C                             (D1TpOpe     = 'P'   and
205500130927     C                              D1VarNota   = 'C'      )
205600130930     C*
205700130927     C* il segno deve essere positivo
205800130930     C                   if        D1ImpOpe      < 0      or
205900130927     C                             D1IVAOpe      < 0
206000130927     C                   SetOn                                        5599
206100130918     C                   endif
206200130918     C*
206300130918     C                   else
206400130927     C* il segno deve essere negativo
206500130930     C                   if        D1ImpOpe      > 0      or
206600130927     C                             D1IVAOpe      > 0
206700130927     C                   SetOn                                        5499
206800130918     C                   endif
206900130918     C*
207000130918     C                   endif
207100130918     C*
207200130930     C                   endif
207300130925     C*
207400130925     C                   if        D1ImpOpe      = 0      and
207500130925     C                             D1IVAOpe      = 0
207600130925     C                   SetOn                                        5699
207700130925     C                   endif
207800130918     C*
207900130918     C                   ENDSR
208000130918     C/EJECT
208100130918     C************************************************************
208200130918     C* Controllo IVA non esposta (Regime del margine)
208300130918     C************************************************************
208400130918     C     CtrMargine    BEGSR
208500130918     C*
208600130918     C                   if        D1Margine  <> XScOn    and
208700130918     C                             D1Margine  <> XScOff
208800130918     C                   SetOn                                        5899
208900130918     C                   endif
209000130918     C*
209100130918     C                   ENDSR
209200130918     C/EJECT
209300130918     C************************************************************
209400130918     C* Controllo reverse charge
209500130918     C************************************************************
209600130918     C     CtrReverse    BEGSR
209700130918     C*
209800130918     C                   if        D1Reverse  <> XScOn    and
209900130918     C                             D1Reverse  <> XScOff
210000130918     C                   SetOn                                        5999
210100130918     C                   endif
210200130918     C*
210300130918     C                   ENDSR
210400130918     C/EJECT
210500130918     C************************************************************
210600130918     C* Controllo autofattura
210700130918     C************************************************************
210800130918     C     CtrAutofat    BEGSR
210900130918     C*
211000130918     C                   if        D1Autofat  <> XScOn    and
211100130918     C                             D1Autofat  <> XScOff
211200130918     C                   SetOn                                        6099
211300130918     C                   endif
211400130918     C*
211500130918     C                   ENDSR
211600130918     C/EJECT
211700130918     C************************************************************
211800130918     C* Controllo leasing
211900130918     C************************************************************
212000130918     C     CtrLeasing    BEGSR
212100130926     C*
212200130926     C                   Clear                   D1LeasingD
212300130926     C                   Clear                   X07ValDs
212400130926     C*
212500130926     C                   Move      '1'           X07Ric
212600130926     C                   Move      *On           X07All
212700130926     C                   MoveL     'SDGSCF000'   X07TRc
212800130926     C                   MoveL     'SCFLEASING'  X07Cam
212900130926     C                   MoveL     D1Leasing     X07VAL
213000130926     C*
213100130926     C                   CALLB     'X07VALR'
213200130926     C                   PARM                    X07ValDs
213300130926     C*
213400130926     C     X07ERR        IFEQ      *Off
213500130926     C                   MoveL     X07DES        D1LeasingD
213600130926     C                   else
213700130926     C                   SetOn                                        6199
213800130926     C                   endif
213900130918     C*
214000130918     C                   ENDSR
214100130918     C/EJECT
214200130918     C************************************************************
214300130918     C* Controllo natura operazione
214400130918     C************************************************************
214500130918     C     CtrNatOpe     BEGSR
214600130918     C*
214700130918     C                   Clear                   D1NatOpeD
214800130918     C                   Clear                   X07ValDs
214900130918     C*
215000130918     C                   Move      '1'           X07Ric
215100130918     C                   Move      *On           X07All
215200130918     C                   MoveL     'SDGSCF000'   X07TRc
215300130918     C                   MoveL     'SCFNATOPE '  X07Cam
215400130918     C                   MoveL     D1NatOpe      X07VAL
215500130918     C*
215600130918     C                   CALLB     'X07VALR'
215700130918     C                   PARM                    X07ValDs
215800130918     C*
215900130918     C     X07ERR        IFEQ      *Off
216000130918     C                   MoveL     X07DES        D1NatOpeD
216100130918     C                   else
216200130918     C                   SetOn                                        6299
216300130918     C                   endif
216400130918     C*
216500130918     C                   ENDSR
216600130918     C/EJECT
216700130918     C************************************************************
216800130918     C* Controllo annullato
216900130918     C************************************************************
217000130918     C     CtrAnn        BEGSR
217100130918     C*
217200130918     C                   if        D1Ann      <> XScOn    and
217300130918     C                             D1Ann      <> XScOff
217400130918     C                   SetOn                                        6399
217500130918     C                   endif
217600130918     C*
217700130918     C                   ENDSR
217800130918     C/EJECT
217900130918     C***********************************************************
218000130918     C* Controllo tipo annullamento
218100130918     C***********************************************************
218200130918     C     CtrTpAnn      BEGSR
218300130918     C*
218400130918     C                   if        D1Ann      =  XScOn
218500130918     C                   Clear                   X07ValDs
218600130918     C                   Move      '1'           X07Ric
218700130918     C                   Move      *Off          X07All
218800130918     C                   MoveL     'SDGSCF000'   X07TRc
218900130918     C                   MoveL     'SCFTPANN'    X07Cam
219000130918     C                   MoveL     D1TpAnn       X07VAL
219100130918     C                   CALLB     'X07VALR'
219200130918     C                   PARM                    X07ValDs
219300130918     C*
219400130918     C     X07ERR        IFEQ      *Off
219500130918     C                   MoveL     X07DES        D1TpAnnD
219600130918     C                   else
219700130918     C                   SetOn                                        6499
219800130918     C                   endif
219900130918     C                   else
220000130918     C                   Clear                   D1TpAnn
220100130918     C                   endif
220200130918     C*
220300130918     C                   ENDSR
220400130918     C/EJECT
220500130911     C***********************************************************
220600130911     C* Controllo esistenza record 1
220700130911     C***********************************************************
220800130911     C     CtrExist1     BEGSR
220900130911     C*
221000130909     C                   Eval      SCFTpCom    = $TpCom
221100130909     C                   Eval      SCFAnno     = $Anno
221200130909     C                   Eval      SCFMese     = $Mese
221300130909     C                   Eval      SCFSys      = $Sys
221400130909     C                   Eval      SCFNrAsReg  = $NrAsReg
221500130909     C                   Eval      SCFNrRigaM  = $NrRigaM
221600130909     C                   Eval      SCFNrRiga   = $NrRiga
221700000000     C*
221800130919     C     K07SCF11      SETLL     SDGSCF11L                              21
221900000000     C     *IN21         IFEQ      '1'
222000130909     C                   SetOn                                        9099
222100000000     C                   ENDIF
222200000000     C*
222300000000     C                   ENDSR
222400000000     C/EJECT
222500130909     C************************************************************
222600130909     C* GESTIONE VIDEO RECORD  D2
222700130909     C************************************************************
222800130909     C     GESD2         BEGSR
222900130909     C* emissione piede videata se proveniente da altra
223000130909     C* salvataggio valore formato di provenienza
223100130909     C     $LASTV        IFNE      'D2'
223200130909     C                   WRITE     FMTT1
223300130909     C                   EXSR      GESPIE
223400130909     C                   MOVE      $LASTV        $LASTG
223500130909     C                   MOVE      'D2'          $LASTV
223600130909     C                   ENDIF
223700000000     C*
223800000000     C* se esistono errori sulla videata
223900000000     C* emetto la write del formato a indicatori spenti per vedere
224000000000     C* le eventuali decodifiche
224100130926   B1C                   if        (*IN99       = *On   or
224200131001   B1C                              *In16       = *On      )  and
224300130926     C                             $GEST        = 'D2'
224400000000     C                   MOVEA     *IN           WIN
224500000000     C                   MOVE      *ALL'0'       IN5098           49
224600000000     C                   MOVEA     IN5098        *IN(50)
224700130930     C                   Eval      WIn16        = *In16
224800130930     C                   Eval      *In16        = *Off
224900000000     C                   WRITE     FMTD2
225000000000     C                   MOVEA     WIN           *IN
225100130930     C                   Eval      *In16        = WIn16
225200000000   E1C                   ENDIF
225300000000     C* Inizializzazione indice tasti funzione personalizzati
225400130909     C                   Eval      PosFun = 25
225500000000     C*
225600000000   B1C                   SELECT
225700000000     C* Annullamento
225800130909    >C     OPZG76        WHENEQ    '04'
225900000000     C* Visualizzazione
226000130909    >C     OPZG76        OREQ      '05'
226100000000     C                   WRITE     FMTD2
226200000000     C                   EXFMT     PROTECT
226300000000     C* Immissione/Modifica/Copia
226400000000   X1C                   OTHER
226500000000     C                   EXFMT     FMTD2
226600000000   E1C                   ENDSL
226700000000     C*
226800000000     C* azzero la posizione del cursore
226900000000     C                   Z-ADD     0             H1RIGA
227000000000     C                   Z-ADD     0             H1COLO
227100000000     C*
227200000000     C* controllo abilitazione tasti funzione
227300130909     C                   EXSR      CTRKD1
227400000000     C*
227500130909   B1C     *IN99         IFEQ      *Off
227600000000     C*
227700000000   B2C                   SELECT
227800000000     C* ROLLUP
227900000000    >C     $TASTO        WHENEQ    ROLLUP
228000000000    >C                   MOVE      'D3'          $GEST
228100000000     C* ROLDOWN
228200000000     C     $TASTO        WHENEQ    ROLDWN
228300000000     C                   MOVE      'D1'          $GEST
228400000000     C* F3=Fine
228500000000     C     $TASTO        WHENEQ    F03
228600130909     C                   EXSR      F03D1
228700000000     C* F4=Ricerca
228800000000     C     $TASTO        WHENEQ    F04
228900130909     C                   EXSR      SEARCH
229000000000     C* F5=Refresh
229100000000     C     $TASTO        WHENEQ    F05
229200130909     C                   EXSR      F05D2
229300000000     C* F8=Successivo
229400000000     C     $TASTO        WHENEQ    F08
229500130909     C                   EXSR      F08D1
229600130923     C* F11=Forzatura
229700130923     C     $TASTO        WHENEQ    F11
229800130923     C                   EXSR      F11D1
229900000000     C* F12=Ritorno
230000000000     C     $TASTO        WHENEQ    F12
230100130909     C                   EXSR      F12D1
230200000000     C* F24=Altri tasti
230300000000     C     $TASTO        WHENEQ    F24
230400130909     C                   EXSR      F24D2
230500000000     C* Personalizzata
230600130909     C* NB chiamata NON possibile perchè SDGSG76DS più lunga
230700130909     C* di KPJBU 256 caratteri                          a
230800000000     C**** $PGF1(PosFun) Whenne    *BLANK
230900130909     C****               EXSR      FperD1
231000000000     C*
231100000000   X2C                   OTHER
231200130909     C                   EXSR      CTRD2
231300130909   B3C     *IN99         IFEQ      *Off
231400130909     C                   EXSR      CTRD1
231500130926   B1C                   if        *IN99       = *On
231600000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
231700000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
231800000000     C* videate
231900000000     C                   MOVE      *ALL'0'       IN5098           49
232000000000     C                   MOVEA     IN5098        *IN(50)
232100130909     C                   SetOn                                        9996                       96
232200000000     C                   ENDIF
232300000000     C                   ENDIF
232400130909   B3C     *IN99         IFEQ      *Off
232500130923     C                   EXSR      CTRD3
232600130926   B1C                   if        *IN99       = *On
232700000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
232800000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
232900000000     C* videate
233000000000     C                   MOVE      *ALL'0'       IN5098           49
233100000000     C                   MOVEA     IN5098        *IN(50)
233200130909     C                   SetOn                                        9996                       96
233300000000     C                   ENDIF
233400000000     C                   ENDIF
233500000000     C*
233600000000     C*
233700000000   B3C     $TASTO        IFEQ      F06
233800130909     C     *IN99         ANDEQ     *Off
233900000000     C* eseguo aggiornamento
234000130909     C                   EXSR      AGGANA
234100000000     C* eseguo operazioni del dopo-aggiornamento
234200130909     C     *IN99         IFEQ      *Off
234300130909     C                   EXSR      GESAGG
234400130909     C                   ENDIF
234500130909     C                   ENDIF
234600130909     C*
234700130909     C                   ENDSL
234800130909     C*
234900130909     C                   ENDIF
235000130909     C*
235100130909     C                   ENDSR
235200130909     C/EJECT
235300130909     C************************************************************
235400130909     C* RIEMPIMENTO VIDEATA D2
235500130909     C************************************************************
235600130909     C     RIED2         BEGSR
235700130909     C*
235800130909     C                   if        SCFNoIva    = '1'
235900130909     C                   Eval      D1NoIva     = XScOn
236000130909     C                   else
236100130909     C                   Eval      D1NoIva     = XScOff
236200130909     C                   endif
236300130923     C*
236400130923     C                   Eval      D1Sogg      = SCFSogg
236500130923     C                   Eval      D1TpN       = SCFTpN
236600130923     C                   Eval      D1Kcc       = SCFKcc
236700130923     C                   Eval      D1Ksc       = SCFKsc
236800130923     C                   Eval      D1Causale   = SCFCausale
236900130923     C                   Eval      D1Aliq      = SCFAliq
237000130923     C                   Eval      D1RifIVA    = SCFRifIVA
237100130923     C                   Eval      D1TpReg     = SCFTpReg
237200130923     C                   Eval      D1Libro     = SCFLibro
237300130923     C*
237400130923     C                   MoveL     SCFDtDoc      WDtDoc
237500130923     C                   if        SCFDtDoc    = *Loval
237600130923     C                   Eval      D1DtDoc     = 0
237700130923     C                   else
237800130923     C     *JobRun       Move      SCFDtDoc      D1DtDoc
237900130923     C                   endif
238000130923     C*
238100130923     C                   Eval      D1NrDoc     = SCFNrDoc
238200130923     C                   Eval      D1SerDoc    = SCFSerDoc
238300130923     C*
238400130923     C                   MoveL     SCFDtReg      WDtReg
238500130923     C                   if        SCFDtReg    = *Loval
238600130923     C                   Eval      D1DtReg     = 0
238700130923     C                   else
238800130923     C     *JobRun       Move      SCFDtReg      D1DtReg
238900130923     C                   endif
239000130923     C*
239100130923     C                   Eval      D1NrReg     = SCFNrReg
239200130923     C                   Eval      D1SerReg    = SCFSerReg
239300130923     C*
239400130923     C                   MoveL     SCFDtPar      WDtPar
239500130923     C                   if        SCFDtPar    = *Loval
239600130923     C                   Eval      D1DtPar     = 0
239700130923     C                   else
239800130923     C     *JobRun       Move      SCFDtPar      D1DtPar
239900130923     C                   endif
240000130923     C*
240100130923     C                   Eval      D1NrPar     = SCFNrPar
240200130923     C                   Eval      D1SerPar    = SCFSerPar
240300130923     C*
240400130923     C                   Eval      D1Sys       = SCFSys
240500130923     C                   Eval      D1NrAsReg   = SCFNrAsReg
240600110902     C*
240700130909     C                   Eval      D1Origine   = SCFOrigine
240800000000     C*
240900000000     C                   ENDSR
241000000000     C/EJECT
241100000000     C************************************************************
241200000000     C* GESTIONE F05 VIDEO D2
241300000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
241400000000     C* e il riposizionamento alla prima
241500000000     C************************************************************
241600000000    >C     F05D2         BEGSR
241700000000     C*
241800000000     C                   MOVE      *ON           $INZD1
241900000000     C                   MOVE      'D1'          $GEST
242000000000     C*
242100000000    >C                   ENDSR
242200000000     C************************************************************
242300000000     C* GESTIONE F24 VIDEO D2
242400000000     C************************************************************
242500000000    >C     F24D2         BEGSR
242600000000     C*
242700000000     C                   MOVEA     $FX1          $FX
242800000000     C                   MOVEA     $FC1          $FC
242900000000     C                   MOVEA     $FR1          $FR
243000000000     C                   Z-ADD     $ULKD1        $ULK
243100000000     C                   Z-ADD     $D1L01        $L01
243200000000     C                   Z-ADD     $D1L02        $L02
243300130909     C                   EXSR      RIEKEY
243400000000     C                   Z-ADD     $ULK          $ULKD1            3 0
243500000000     C                   MOVEA     $FC           $FC1
243600130909     C                   MoveL     $KEY1         Z1KE1
243700130909     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
243800130909     C                   MoveL     $KEY2         Z1KE2
243900000000     C                   WRITE     FMTZ1
244000000000     C*
244100000000    >C                   ENDSR
244200000000     C************************************************************
244300000000     C* CONTROLLO VIDEATA  D2
244400000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
244500000000     C* 95 96 97 98
244600000000     C************************************************************
244700000000    >C     CTRD2         BEGSR
244800000000     C*
244900130909     C                   SetOff                                       99
245000000000     C*
245100000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
245200000000     C*
245300130909    >C     OPZG76        IFNE      '04'
245400130918     C*
245500130918     C                   EXSR      CtrSogg
245600130919     C                   Clear                   D1TpND
245700131001     C************       if        D1TpDoc    <> 'S'
245800130918     C                   EXSR      CtrTpN
245900131001     C************       endif
246000130918     C                   EXSR      CtrConto
246100130918     C*
246200130918     C                   EXSR      CtrCausale
246300130918     C                   EXSR      CtrNoIVA
246400130918     C                   EXSR      CtrAliq
246500130918     C                   EXSR      CtrRifIVA
246600130918     C*
246700130918     C                   if        (D1Aliq     <> 0       and
246800130918     C                              D1RifIVA   <> *Blank      )
246900130918     C*
247000130918     C* PRO0187 I dati sono in alternativa
247100130918     C                   SetOn                                        5299
247200130918     C                   endif
247300130918     C*
247400130918     C                   EXSR      CtrTpReg
247500130918     C                   EXSR      CtrLibro
247600130918     C*
247700130918     C                   EXSR      CtrDocumento
247800130924     C*
247900130918     C                   EXSR      CtrReg
248000130924     C*
248100130918     C                   EXSR      CtrPartita
248200130918     C*
248300130918     C                   if        OPZG76      = '01'   or
248400130918     C                             OPZG76      = '03'
248500130918     C*
248600130918     C                   if        $NrAsReg    = 0
248700130918     C* Reperisco il numero assoluto della registrazione fittizia
248800130918     C                   EXSR      RepNrAss
248900130918     C                   endif
249000130918     C*
249100130918     C                   endif
249200130918     C*
249300130918     C                   EXSR      CtrOrigine
249400130909     C*
249500130909     C                   ENDIF
249600130909     C*
249700130909     C                   ENDSR
249800130909     C/EJECT
249900130918     C***********************************************************
250000130918     C* Controllo codice soggetto
250100130918     C***********************************************************
250200130918     C     CtrSogg       BEGSR
250300130918     C*
250400130918     C                   Clear                   D1SoggD
250500130918     C                   if        D1Sogg     <> *Blank
250600130918     C*
250700130918     C     D1SOGG        IFNE      *BLANK
250800130918     C                   CALLb     'XALLINEA'
250900130918     C                   PARM                    D1SOGG
251000130918     C                   PARM      8             LEN               5 0
251100130918     C                   PARM                    XSCINP
251200130918     C                   ENDIF
251300130918     C*
251400130918     C                   MoveL     D1Sogg        WSogg
251500130918     C                   EXSR      CallDvaSog
251600130918     C*
251700130918     C                   if        DVAErrore  <> '1'
251800130918     C*
251900130918     C                   Eval      %subst(DVASOG:1:DVALENOUT)
252000130918     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
252100130918     C*
252200130918     C                   MoveL     DVSDes        D1SoggD
252300130918     C*
252400130918     C                   else
252500130918     C*
252600131001     C                   SetOn                                        16
252700130918     C*
252800130918     C                   endif
252900130918     C*
253000130918     C                   endif
253100130918     C*
253200130918     C                   ENDSR
253300130918     C/EJECT
253400130918     C***********************************************************
253500130918     C* Decofico  la tipologia della sottonatura
253600130918     C***********************************************************
253700130918     C     CtrTpN        BEGSR
253800130918     C*
253900130918     C                   Clear                   X07ValDs
254000130918     C*
254100130918     C                   Move      '1'           X07Ric
254200130918     C                   Move      *Off          X07All
254300130918     C                   MoveL     'SDGSCF000'   X07TRc
254400130918     C                   MoveL     'SCFTPN   '   X07Cam
254500130918     C                   MoveL     D1TpN         X07VAL
254600130918     C*
254700130918     C                   CALLB     'X07VALR'
254800130918     C                   PARM                    X07ValDs
254900130918     C*
255000130918     C     X07ERR        IFEQ      *Off
255100130918     C                   MoveL     X07DES        D1TpND
255200130918     C                   else
255300131018R136 C*
255400131018 "   C                   if        (D1TpDoc    <> 'R'      and
255500131018 "   C                              D1TpN       = *Blank      )
255600131018 "   C                             or
255700131018R136 C                             D1TpN      <> *Blank
255800130918     C                   SetOn                                        5199
255900131018R136 C                   endif
256000130918     C                   endif
256100130918     C*
256200130918     C                   ENDSR
256300130918     C/EJECT
256400130918     C************************************************************
256500130918     C* Controllo esistenza conto collegato a soggetto e con
256600130918     C* la sottonatura indicata
256700130918     C************************************************************
256800130918     C     CtrConto      BEGSR
256900130918     C*
257000130918     C                   Clear                   WKcc
257100130918     C                   Clear                   WKsc
257200130918     C*
257300130918     C                   Clear                   D1KSCD
257400130918     C*
257500130918     C     D1KCC         IFNE      'C'
257600130918     C     D1KCC         ANDNE     'F'
257700130918     C                   CALLB     'XALLINEA'
257800130918     C                   PARM                    D1KCC
257900130918     C                   PARM      6             LEN               5 0
258000130918     C                   PARM                    XSCINP
258100130918     C                   ENDIF
258200130918     C*
258300130918     C                   if        D1Ksc      <> *Blank
258400130918     C*
258500130918     C     D1KSC         IFNE      *Blanks
258600130918     C                   CALLB     'XALLINEA'
258700130918     C                   PARM                    D1KSC
258800130918     C                   PARM      8             LEN               5 0
258900130918     C                   PARM                    XSCINP
259000130918     C                   ENDIF
259100130918     C*
259200130918     C     D1KCC         IFEQ      'C       '
259300130918     C     D1KCC         OREQ      'F       '
259400130918     C                   CALLB     'XCAPCLIFOR'
259500130918     C                   PARM                    XScSoc
259600130918     C                   PARM      D1KCC         D1Kcc
259700130918     C                   PARM                    D1Ksc
259800130918   E1C                   ENDIF
259900130918     C*
260000130918     C                   Eval      WKsc        = D1Ksc
260100130918     C                   Eval      WKcc        = D1Kcc
260200130918     C                   EXSR      CALLMVC002
260300130918     C                   if        Esito       = *On
260400130918     C                   SetOn                                        5299
260500130918     C                   else
260600130918     C                   if        Snatura002 <> 'C' and
260700130918     C                             Snatura002 <> 'F'
260800130918     C                   SetOn                                        5399
260900130918     C                   else
261000130918     C                   MoveL     DesBrev002    D1KSCD
261100130918     C                   MoveL     SNatura002    H1CF
261200130918     C*
261300130918     C                   if        Sogg002    <> D1Sogg   and
261400130918     C                             D1Sogg     <> *Blank
261500130918     C                   Eval      P1SDG0081   = D1Sogg
261600130918     C                   SetOn                                        5499
261700130918     C                   endif
261800130918     C                   endif
261900130918     C*
262000130918     C                   endif
262100130918     C*
262200130918     C                   endif
262300130918     C*
262400130918     C                   ENDSR
262500130918     C/EJECT
262600130918     C***********************************************************
262700130918     C* Driver per controllo conto
262800130918     C***********************************************************
262900130918     C     CALLMvc002    BEGSR
263000130918     C*
263100130918     C                   Clear                   ND002DS
263200130918     C*
263300130918     C                   Eval      LenOut = %Size(ND002DS)
263400130918     C*
263500130918     C                   CALLB     'NDMVC002'
263600130918     C                   PARM                    XScSoc
263700130918     C                   PARM                    WKCC
263800130918     C                   PARM                    WKSC
263900130918     C                   PARM                    Data
264000130918     C                   PARM      *Off          GesMsg
264100130918     C                   PARM      *Off          Esito
264200130918     C                   PARM      'ND002DS'     StrutturaO
264300130918     C                   PARM                    ND002DS
264400130918     C                   PARM                    LenOut
264500130918     C*
264600130918     C                   ENDSR
264700130918     C/EJECT
264800130918     C************************************************************
264900130918     C* Controllo esistenza causale HD2
265000130918     C************************************************************
265100130918     C     CtrCausale    BEGSR
265200130918     C*
265300130919     C                   Clear                   D1CausaleD
265400130918     C*
265500130918     C                   if        D1Causale  <> *Blank
265600131031R140 C                             and
265700131031 "   C                             D1DtReg    <> 0      and
265800131031 "   C                             WDtReg     <> *Loval
265900131031 "   C*
266000131031 "   C     WDtReg        AddDur    1:*D          WData
266100131031 "   C                   Move      WData         WDtAlfa
266200131031 "   C*
266300131031 "   C* la causale deve essere valida alla data di registrazione
266400131031 "   C* passo quindi data di registrazione + un giorno
266500131031 "   C
266600131031R140 C*
266700130918     C                   Clear                   ANOPE00F
266800130918     C                   Eval      Lenout = %Size(ANOPE00F)
266900130918     C                   CALLB     'NDMVC003'
267000130918     C                   PARM                    XScSoc
267100130918     C                   PARM                    D1Causale
267200131031R140 X****               PARM                    *Omit
267300131031R140 C                   PARM                    WDtAlfa
267400130918     C                   PARM                    Uso
267500130918     C                   PARM      *Off          GesMsg
267600130918     C                   PARM      *Off          Esito
267700130918     C                   PARM      'ANOPE     '  StrutturaO
267800130918     C                   PARM                    ANOPE00F
267900130918     C                   PARM                    LenOut
268000130918     C                   PARM      *Off          Refresh
268100130918     C*
268200130918     C                   if        Esito     = *on
268300130918     C                   SetOn                                        5599
268400130918     C                   else
268500130919     C*
268600130919     C                   Eval      D1CausaleD  = OPEDesBrev
268700130918     C*
268800130918     C* deve essere per registrazioni IVA
268900130918     C                   if        OPETpc    <> '1'
269000130918     C                   SetOn                                        5699
269100160318R2191X***                else
269200160318R2191X*** controllo esclusioni
269300160318R2191X***                EXSR      CtrSDGSPR
269400130918     C                   endif
269500130918     C                   endif
269600130918     C*
269700130918     C                   endif
269800130918     C*
269900130918     C                   ENDSR
270000130918     C/EJECT
270100130918     C************************************************************
270200130918     C* Controllo esclusioni
270300130918     C************************************************************
270400130918     C     CtrSDGSPR     BEGSR
270500130918     C*
270600130918     C                   Eval      SPRSocieta = XscSoc
270700130918     C                   Eval      SPRTipReg  = '3'
270800130918     C                   Eval      SPRStpReg  = '1'
270900130918     C                   Eval      SPRCausale = D1Causale
271000130918     C     K04SPR03      chain     SDGSPR03L
271100130918     C                   if        %found(SDGSPR03L)
271200130918     C* causale esclusa da presentazione
271300130918     C                   SetOn                                        5799
271400130918     C                   endif
271500130918     C*
271600130918     C                   ENDSR
271700130918     C/EJECT
271800130918     C************************************************************
271900130918     C* Controllo movimento senza NDIVA
272000130918     C************************************************************
272100130918     C     CtrNoIVA      BEGSR
272200130918     C*
272300130918     C                   if        D1NoIva    <> XScOn    and
272400130918     C                             D1NoIva    <> XScOff
272500130918     C                   SetOn                                        5899
272600130918     C                   endif
272700130918     C*
272800130918     C                   ENDSR
272900130918     C/EJECT
273000130918     C************************************************************
273100130918     C* Controllo aliquota
273200130918     C************************************************************
273300130918     C     CtrAliq       BEGSR
273400130918     C*
273500130918     C                   if        D1Aliq     <> 0
273600130918     C*
273700130918     C                   Eval      WAliq       = D1Aliq
273800130918     C                   Eval      WRifIVA     = D1RifIVA
273900130918     C*
274000130918     C                   EXSR      CtrANRIV
274100130918     C                   if        WRifIvaOk  <> *On
274200130918     C                   SetOn                                        5999
274300130918     C                   endif
274400130918     C*
274500130918     C                   endif
274600130918     C*
274700130918     C                   ENDSR
274800130918     C/EJECT
274900130918     C************************************************************
275000130918     C* Controllo riferimento IVA
275100130918     C************************************************************
275200130918     C     CtrRifIVA     BEGSR
275300130918     C*
275400130918     C                   if        D1RifIVA   <> *Blank
275500130918     C*
275600130918     C                   Eval      WAliq       = D1Aliq
275700130918     C                   Eval      WRifIVA     = D1RifIVA
275800130918     C*
275900130918     C                   EXSR      CtrANRIV
276000130918     C                   if        WRifIvaOk  <> *On
276100130918     C                   SetOn                                        6099
276200130918     C                   endif
276300130918     C*
276400130918     C                   endif
276500130918     C*
276600130918     C                   ENDSR
276700130918     C/EJECT
276800130918     C************************************************************
276900130918     C* Controllo aliquota e riferimento IVA
277000130918     C************************************************************
277100130918     C     CtrANRIV      BEGSR
277200130918     C*
277300130918     C                   Eval      WRifIvaOk   = *On
277400130918     C*
277500130918     C                   Eval      RIVSocieta  = XscSoc
277600130919     C                   if        D1TpOpe     = 'P'
277700130918     C                   Eval      RIVTpReg    = '1'
277800130918     C                   else
277900130918     C                   Eval      RIVTpReg    = '2'
278000130918     C                   endif
278100130918     C                   Eval      RIVALiq     = WAliq
278200130918     C                   Eval      RIVRifIVA   = WRifIva
278300130918     C*
278400130918     C     K04RIV01      CHAIN     ANRIV01L                           21
278500130918     C                   if        *In21       = *On
278600130918     C                   Eval      WRifIvaOk   = *Off
278700130918     C                   endif
278800130918     C*
278900130918     C                   ENDSR
279000130918     C/EJECT
279100130918     C************************************************************
279200130918     C* Controllo tipo registro IVA
279300130918     C************************************************************
279400130918     C     CtrTpReg      BEGSR
279500130918     C*
279600130918     C                   Clear                   D1TpRegD
279700130918     C                   Clear                   X07ValDs
279800130918     C*
279900130918     C                   if        D1TpReg    <> *blank
280000130918     C                   Move      '1'           X07Ric
280100130918     C                   Move      *On           X07All
280200130918     C                   MoveL     'ANRIV000 '   X07TRc
280300130918     C                   MoveL     'RIVTPREG'    X07Cam
280400130918     C                   MoveL     D1TpReg       X07VAL
280500130918     C*
280600130918     C                   CALLB     'X07VALR'
280700130918     C                   PARM                    X07ValDs
280800130918     C*
280900130918     C     X07ERR        IFEQ      *Off
281000130918     C                   MoveL     X07DES        D1TpRegD
281100130918     C*
281200130919     C* controllo congruenza il tipo operazione
281300130919     C                   if        D1TpOpe     = 'A'     and
281400130918     C                             D1TpReg    <> *Blank  and
281500130918     C                             D1TpReg    <> '2'     and
281600130918     C                             D1TpReg    <> '3'
281700130918     C                   SetOn                                        6199
281800130918     C                   endif
281900130918     C*
282000130919     C                   if        D1TpOpe     = 'P'     and
282100130918     C                             D1TpReg    <> *Blank  and
282200130918     C                             D1TpReg    <> '1'
282300130918     C                   SetOn                                        6199
282400130918     C                   endif
282500130918     C*
282600130918     C                   else
282700130918     C                   SetOn                                        6299
282800130918     C                   endif
282900130918     C*
283000130918     C                   endif
283100130918     C*
283200130918     C                   ENDSR
283300130918     C/EJECT
283400130918     C************************************************************
283500130918     C* CONTROLLO libro (esistente nel registro)
283600130918     C************************************************************
283700130918     C     CtrLibro      BEGSR
283800130918     C*
283900130918     C                   Eval      D1LibroD    = *Blank
284000130918     C*
284100130918     C                   if        D1Libro    <> *Blank
284200130918     C*
284300130918     C                   Eval      DLISocieta  = XScSoc
284400130918     C                   Eval      DLITpReg    = D1TpReg
284500130918     C                   Eval      DLILibro    = D1Libro
284600130918     C     K03DLI01      CHAIN     ANDLI01L                           21
284700130918     C*
284800130918     C                   if        *In21       = *On
284900130918     C                   SetOn                                        6399
285000130918     C                   else
285100130918     C                   Eval      D1LibroD    = DLIDESBREV
285200130918     C                   endif
285300130918     C*
285400130918     C                   endif
285500130918     C*
285600130918     C                   if        D1Libro      <> *Blanks  and
285700130918     C                             D1TpReg       = *Blanks
285800130918     C                   SetOn                                        6499
285900130918     C                   endif
286000130918     C*
286100130918     C                   ENDSR
286200130918     C/EJECT
286300130918     C************************************************************
286400130918     C* Controllo documento
286500130918     C************************************************************
286600130918     C     CtrDocumento  BEGSR
286700130918     C*
286701151207R195 C* se Spesometro
286702151211R195 C                   if        TpComG76    = 'E'
286800131017R136 X****               if        FormatoA08  = 'D'   and
286900131017R136 X****                         D1TpDoc    <> 'S'
287000131017R136 C*
287100160310R195 X*ex R136           if        (FormatoA08  = 'D'   and
287200160310 "   X*ex R136                      D1TpDoc    <> 'S'       ) or
287300160310 "   X*ex R136                     (D1TpDoc     = 'S'   and
287400160310R195 X*ex R136                      D1CdFisc    = *Blank    )
287401160310R195 C*
287402160310 "   C* riporto la data doc. NON obbligatoria per il quadro TU
287403160310 "   C* rimarrà obbligatoria la registrazione per comunicare il
287404160310 "   C* quadro a dettaglio
287405160310 "   C                   if        FormatoA08  = 'D'   and
287406160310R195 C                             D1TpDoc    <> 'S'
287500130918     C* Riferimenti documento obbligatori
287600130918     C     D1DtDoc       IFEQ      0
287700130918     C                   SetOn                                        6599
287800130918     C                   ENDIF
287900130924     C                   endif
288000130918     C*
288001151207R195 C                   endif
288002151207R195 C*
288100130918     C                   Eval      WDtDoc      = *Loval
288200130918     C*
288300130918     C                   if        D1DtDoc    <> 0
288400130918     C     *JobRun       Test(d)                 D1DtDoc                21
288500130918     C*
288600130918     C                   if        *In21       = *Off
288700130918     C     *JobRun       Move      D1DtDoc       WDtDoc
288800130918     C*
288900130918     C                   else
289000130918     C                   SetOn                                        6699
289100130918     C                   endif
289200130918     C*
289300130918     C                   endif
289400130918     C*
289500130924     C*
289501151207R195 C* se Spesometro
289502151211R195 C                   if        TpComG76    = 'E'
289600131017R136 X****               if        FormatoA08  = 'D'   and
289700131017R136 X****                         D1TpDoc    <> 'S'
289800131017R136 C*
290201160310R195 X*ex R136           if        (FormatoA08  = 'D'   and
290202160310 "   X*ex R136                      D1TpDoc    <> 'S'       ) or
290203160310 "   X*ex R136                     (D1TpDoc     = 'S'   and
290204160310R195 X*ex R136                      D1CdFisc    = *Blank    )
290205160310R195 C*
290206160310 "   C* riporto il num. doc. NON obbligatorio per il quadro TU
290207160310 "   C* rimarrà obbligatoria la registrazione per comunicare il
290208160310 "   C* quadro a dettaglio
290209160310 "   C                   if        FormatoA08  = 'D'   and
290210160310R195 C                             D1TpDoc    <> 'S'
290300130918     C     D1NrDoc       IFEQ      0
290400130918     C                   SetOn                                        6799
290500130918     C                   ENDIF
290600130924     C                   endif
290601151207R195 C                   endif
290602151207R195 C*
290700130918     C*
290800130918     C                   ENDSR
290900130918     C/EJECT
291000130918     C************************************************************
291100130918     C* Controllo registrazione
291200130918     C************************************************************
291300130918     C     CtrReg        BEGSR
291400130918     C*
291500130918     C* Riferimenti registrazione obbligatori
291600130918     C     D1DtReg       IFEQ      0
291700130918     C                   SetOn                                        6899
291800130918     C                   ENDIF
291900130918     C*
292000130918     C                   Eval      WDtReg      = *Loval
292100130918     C*
292200130918     C                   if        D1DtReg    <> 0
292300130918     C     *JobRun       Test(d)                 D1DtReg                21
292400130918     C*
292500130918     C                   if        *In21       = *Off
292600130918     C     *JobRun       Move      D1DtReg       WDtReg
292700130918     C*
292800130918     C                   else
292900130918     C                   SetOn                                        6999
293000130918     C                   endif
293100130918     C*
293200130918     C                   endif
293300130918     C*
293400130918     C     D1NrReg       IFEQ      0
293500130918     C                   SetOn                                        7099
293600130918     C                   ENDIF
293700130918     C*
293800130918     C                   if        D1SerReg   <> *Blank
293900130918     C                   Clear                   SDGTABDS
294000130918     C                   Move      '1'           XTbRic
294100130918     C                   Move      XScSoc        XTbAzi
294200130918     C                   Move      *Zero         XTbKey
294300130918     C                   Move      'G13'         XTbCod
294400130918     C                   Move      D1SerReg      XTbKey
294500130918     C                   CALL      'SDGTABDR'
294600130918     C                   PARM                    SDGTABDS
294700130918     C     XTbErr        IFEq      '1'
294800130918     C                   SetOn                                        7199
294900130918     C                   endif
295000130918     C                   endif
295100130918     C*
295200130918     C                   ENDSR
295300130918     C/EJECT
295400130918     C************************************************************
295500130918     C* Controllo partita
295600130918     C************************************************************
295700130918     C     CtrPartita    BEGSR
295800130918     C*
295900130918     C                   Eval      WDtPar      = *Loval
296000130918     C*
296100130918     C                   if        D1DtPar    <> 0
296200130918     C     *JobRun       Test(d)                 D1DtPar                21
296300130918     C*
296400130918     C                   if        *In21       = *Off
296500130918     C     *JobRun       Move      D1DtPar       WDtPar
296600130918     C                   else
296700130918     C                   SetOn                                        7399
296800130918     C                   endif
296900130918     C*
297000130918     C                   endif
297100130918     C*
297200130918     C                   ENDSR
297300130918     C/EJECT
297400130918     C***********************************************************
297500130918     C* Reperisco il numero assoluto di registrazione
297600130918     C***********************************************************
297700130918     C     RepNrAss      BEGSR
297800130918     C*
297900130918     C                   CALLB     'NDMVC101'
298000130918     C                   PARM      *Off          $Esito
298100130918     C                   PARM      0             $Sys
298200130918     C                   PARM      0             $NrAsReg
298300130918     C                   PARM      '1'           $Prenot
298400130918     C*
298500130918     C                   If        $Esito     =  *On
298600130918    >C                   MOVE      '1'           ERRG76
298700130918     C                   MOVE      *on           $Fine
298800130918     C                   EXSR      ENDPGM
298900130918     C                   endif
299000130918     C*
299100130918     C                   Eval      $NrRigaM    = 1
299200130918     C                   Eval      $NrRiga     = 1
299300130918     C*
299400130918     C                   ENDSR
299500130918      /EJECT
299600130918     C************************************************************
299700130918     C* Controllo origine record
299800130918     C************************************************************
299900130918     C     CtrOrigine    BEGSR
300000130918     C*
300100130918     C                   Clear                   D1OrigineD
300200130918     C                   Clear                   X07ValDs
300300130918     C*
300400130918     C                   Move      '1'           X07Ric
300500130918     C                   Move      *On           X07All
300600130918     C                   MoveL     'SDGSCF000'   X07TRc
300700130918     C                   MoveL     'SCFORIGINE'  X07Cam
300800130918     C                   MoveL     D1Origine     X07VAL
300900130918     C*
301000130918     C                   CALLB     'X07VALR'
301100130918     C                   PARM                    X07ValDs
301200130918     C*
301300130918     C     X07ERR        IFEQ      *Off
301400130918     C                   MoveL     X07DES        D1OrigineD
301500130918     C                   else
301600130918     C                   SetOn                                        7599
301700130918     C                   endif
301800130918     C*
301900130918     C                   ENDSR
302000130918     C/EJECT
302100000000     C************************************************************
302200000000     C* GESTIONE VIDEO RECORD  D3
302300000000     C************************************************************
302400000000    >C     GESD3         BEGSR
302500000000     C* emissione piede videata se proveniente da altra
302600000000     C* salvataggio valore formato di provenienza
302700000000   B1C     $LASTV        IFNE      'D3'
302800000000     C                   WRITE     FMTT1
302900130909     C                   EXSR      GESPIE
303000000000     C                   MOVE      $LASTV        $LASTG
303100000000     C                   MOVE      'D3'          $LASTV
303200000000   E1C                   ENDIF
303300000000     C*
303400000000     C* se esistono errori sulla videata
303500000000     C* emetto la write del formato a indicatori spenti per vedere
303600000000     C* le eventuali decodifiche
303700131001   B1C                   if        (*IN99       = *On   or
303800131001   B1C                              *In17       = *On      )  and
303900131001     C                             $GEST        = 'D3'
304000000000     C                   MOVEA     *IN           WIN
304100000000     C                   MOVE      *ALL'0'       IN5098           49
304200131001     C                   Eval      WIn17        = *In17
304300131001     C                   Eval      *In17        = *Off
304400000000     C                   MOVEA     IN5098        *IN(50)
304500000000     C                   WRITE     FMTD3
304600000000     C                   MOVEA     WIN           *IN
304700131001     C                   Eval      *In17        = WIn17
304800000000   E1C                   ENDIF
304900000000     C* Inizializzazione indice tasti funzione personalizzati
305000130909     C                   Eval      PosFun = 25
305100000000     C*
305200000000   B1C                   SELECT
305300000000     C* Annullamento
305400130909    >C     OPZG76        WHENEQ    '04'
305500000000     C* Visualizzazione
305600130909    >C     OPZG76        OREQ      '05'
305700110901     C*
305800130909     C                   If        D1Origine   = 'M'
305900130909     C                   Eval      *In12       = *On
306000110901     C                   Else
306100130909     C                   Eval      *In12       = *Off
306200110901     C                   EndIf
306300130923     C*
306400130923     C                   WRITE     FMTD3
306500130923     C                   EXFMT     PROTECT
306600110901     C*
306700000000     C* Immissione/Modifica/Copia
306800000000   X1C                   OTHER
306900130923     C                   EXFMT     FMTD3
307000000000   E1C                   ENDSL
307100000000     C*
307200000000     C* azzero la posizione del cursore
307300000000     C                   Z-ADD     0             H1RIGA
307400000000     C                   Z-ADD     0             H1COLO
307500000000     C*
307600000000     C* controllo abilitazione tasti funzione
307700130909     C                   EXSR      CTRKD1
307800000000     C*
307900130909   B1C     *IN99         IFEQ      *Off
308000000000     C*
308100000000   B2C                   SELECT
308200000000     C* ROLDOWN
308300000000     C     $TASTO        WHENEQ    ROLDWN
308400000000     C                   MOVE      'D2'          $GEST
308500000000     C* F3=Fine
308600000000     C     $TASTO        WHENEQ    F03
308700130909     C                   EXSR      F03D1
308800000000     C* F4=Ricerca
308900000000     C     $TASTO        WHENEQ    F04
309000130909     C                   EXSR      SEARCH
309100000000     C* F5=Refresh
309200000000     C     $TASTO        WHENEQ    F05
309300130909     C                   EXSR      F05D3
309400000000     C* F8=Successivo
309500000000     C     $TASTO        WHENEQ    F08
309600130909     C                   EXSR      F08D1
309700130923     C* F11=Forzatura
309800130923     C     $TASTO        WHENEQ    F11
309900130923     C                   EXSR      F11D1
310000000000     C* F12=Ritorno
310100000000     C     $TASTO        WHENEQ    F12
310200130909     C                   EXSR      F12D1
310300000000     C* F24=Altri tasti
310400000000     C     $TASTO        WHENEQ    F24
310500130909     C                   EXSR      F24D3
310600000000     C* Personalizzata
310700130909     C* NB chiamata NON possibile perchè SDGSG76DS più lunga
310800130909     C* di KPJBU 256 caratteri                          a
310900000000     C**** $PGF1(PosFun) Whenne    *BLANK
311000130909     C****               EXSR      FperD1
311100000000     C*
311200000000   X2C                   OTHER
311300130923     C                   EXSR      CTRD3
311400130909   B3C     *IN99         IFEQ      *Off
311500130909     C                   EXSR      CTRD2
311600130926   B1C                   if        *IN99       = *On
311700000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
311800000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
311900000000     C* videate
312000000000     C                   MOVE      *ALL'0'       IN5098           49
312100000000     C                   MOVEA     IN5098        *IN(50)
312200130909     C                   SetOn                                        9996                       96
312300000000     C                   ENDIF
312400000000     C                   ENDIF
312500130909   B3C     *IN99         IFEQ      *Off
312600130909     C                   EXSR      CTRD1
312700130926   B1C                   if        *IN99       = *On
312800000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
312900000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
313000000000     C* videate
313100000000     C                   MOVE      *ALL'0'       IN5098           49
313200000000     C                   MOVEA     IN5098        *IN(50)
313300130909     C                   SetOn                                        9996                       96
313400000000     C                   ENDIF
313500000000     C                   ENDIF
313600000000     C*
313700000000     C*
313800000000   B3C     $TASTO        IFEQ      F06
313900130909     C     *IN99         ANDEQ     *Off
314000000000     C* eseguo aggiornamento
314100130909     C                   EXSR      AGGANA
314200000000     C* eseguo operazioni del dopo-aggiornamento
314300130909   B4C     *IN99         IFEQ      *Off
314400130909     C                   EXSR      GESAGG
314500000000   E4C                   ENDIF
314600000000   E3C                   ENDIF
314700000000     C*
314800000000   E2C                   ENDSL
314900000000     C*
315000000000   E1C                   ENDIF
315100000000     C*
315200000000    >C                   ENDSR
315300000000     C/EJECT
315400000000     C************************************************************
315500000000     C* RIEMPIMENTO VIDEATA D3
315600000000     C************************************************************
315700000000    >C     RIED3         BEGSR
315800000000     C*
315900130909     C                   Eval      D1Quadro    = SCFQuadro
316000130909     C                   Eval      D1CogRagR   = SCFCogRagR
316100130909     C                   Eval      D1NomeR     = SCFNomeR
316200000000     C*
316300130909     C                   MoveL     SCFDtNasR     WDtNasR
316400130909     C                   if        SCFDtNasR   = *Loval
316500130909     C                   Eval      D1DtNasR    = 0
316600000000     C                   else
316700130930     C*
316800130930     C                   Select
316900130930     C* formato mm/gg/aaaa
317000130930     C                   When      $DatFmt     = '*MDY'
317100130930     C     *USA          Move      SCFDtNasR     D1DtNasR
317200130930     C* formato aa/mm/gg
317300130930     C                   When      $DatFmt     = '*YMD'
317400130930     C     *ISO          Move      SCFDtNasR     D1DtNasR
317500130930     C* formato gg/mm/aaaa
317600130930     C                   When      $DatFmt     = '*DMY'
317700130930     C     *EUR          Move      SCFDtNasR     D1DtNasR
317800130930     C                   endSl
317900000000     C                   endif
318000130930     C*
318100130909     C                   Eval      D1LocNasR   = SCFLocNasR
318200130909     C                   Eval      D1PrvNasR   = SCFPrvNasR
318300130909     C                   Eval      D1StatoR    = SCFStatoR
318400130909     C                   Eval      D1CodUICR   = SCFCodUICR
318500130909     C                   Eval      D1LocaliR   = SCFLocaliR
318600130909     C                   Eval      D1IndirER   = SCFIndirER
318700130909     C                   Eval      D1TpNR      = SCFTpNR
318800130909     C                   Eval      D1PIVAR     = SCFPIVAR
318900130909     C                   Eval      D1CdFiscR   = SCFCdFiscR
319000130925     C*
319100130925     C                   Eval      D1TpOpeR    = SCFTpOpeR
319200130925     C                   Eval      D1NatOpeR   = SCFNatOpeR
319300130926     C                   Eval      D1LeasinR   = SCFLeasinR
319400130923     C*
319500130911     C                   if        SCFDtRegR   = *Hival       or
319600130911     C                             SCFDtRegR   = *Loval
319700130911     C                   Eval      D1DtRegR    = 0
319800130911     C                   else
319900130911     C     *JobRun       Move      SCFDtRegR     D1DtRegR
320000130911     C                   endif
320100130911     C                   Eval      D1NrRegR    = SCFNrRegR
320200130911     C                   Eval      D1SerRegR   = SCFSerRegR
320300000000     C*
320400130925     C                   Eval      D1NrOpe     = SCFNrOpe
320500130924     C*
320600000000    >C                   ENDSR
320700000000     C/EJECT
320800000000     C************************************************************
320900000000     C* GESTIONE F05 VIDEO D3
321000000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
321100000000     C* e il riposizionamento alla prima
321200000000     C************************************************************
321300000000    >C     F05D3         BEGSR
321400000000     C*
321500000000     C                   MOVE      *ON           $INZD1
321600000000     C                   MOVE      'D1'          $GEST
321700000000     C*
321800000000    >C                   ENDSR
321900000000     C************************************************************
322000000000     C* GESTIONE F24 VIDEO D3
322100000000     C************************************************************
322200000000    >C     F24D3         BEGSR
322300000000     C*
322400000000     C                   MOVEA     $FX1          $FX
322500000000     C                   MOVEA     $FC1          $FC
322600000000     C                   MOVEA     $FR1          $FR
322700000000     C                   Z-ADD     $ULKD1        $ULK
322800000000     C                   Z-ADD     $D1L01        $L01
322900000000     C                   Z-ADD     $D1L02        $L02
323000130909     C                   EXSR      RIEKEY
323100000000     C                   Z-ADD     $ULK          $ULKD1            3 0
323200000000     C                   MOVEA     $FC           $FC1
323300130909     C                   MoveL     $KEY1         Z1KE1
323400130909     C                   MoveL     $ALLFUNCT     H1ALLFUNCT
323500130909     C                   MoveL     $KEY2         Z1KE2
323600000000     C                   WRITE     FMTZ1
323700000000     C*
323800000000    >C                   ENDSR
323900000000     C************************************************************
324000000000     C* CONTROLLO VIDEATA  D3
324100000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
324200000000     C* 95 96 97 98
324300000000     C************************************************************
324400000000    >C     CTRD3         BEGSR
324500000000     C*
324600130909     C                   SetOff                                       99
324700000000     C*
324800000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
324900000000     C*
325000130909    >C     OPZG76        IFNE      '04'
325100130911     C*
325200130924     C                   EXSR      RepG12Ksc
325300130923     C                   EXSR      CtrQuadro
325400000000     C*
325500130924     C                   EXSR      CtrTpNR
325600130909     C                   EXSR      CtrCogRagR
325700130909     C                   EXSR      CtrNomeR
325800130909     C                   EXSR      CtrDtNasR
325900130909     C                   EXSR      CtrLocNasR
326000130909     C                   EXSR      CtrPrvNasR
326100130909     C                   EXSR      CtrStatoR
326200131112R141 C                   EXSR      CtrCodUICR
326300130909     C                   EXSR      CtrLocaliR
326400130909     C                   EXSR      CtrIndirER
326500130909     C                   EXSR      CtrPIVAR
326600130909     C                   EXSR      CtrCdFiscR
326700130924     C*
326800130925     C                   EXSR      CtrNatOpeR
326900130925     C                   EXSR      CtrTpOpeR
327000130909     C                   EXSR      CtrLeasinR
327100130924     C*
327200131017R136 C                   EXSR      CtrRegR
327201151207R195 C                   EXSR      CtrNrOpe
327300130925     C                   if        FormatoA08  = 'D'
327400131017R136 X****               EXSR      CtrRegR
327500130925     C                   else
327600151207R195 X****               EXSR      CtrNrOpe
327700130925     C                   endif
327800000000     C*
327900000000     C                   ENDIF
328000000000     C*
328100000000     C                   ENDSR
328200000000     C/EJECT
328300130918     C************************************************************
328400130918     C* Reperisco dati da G12 del conto
328500130918     C************************************************************
328600130918     C     RepG12Ksc     BEGSR
328700130918     C*
328800130919     C                   Eval      $NazKsc     = *Blank
328900130918     C*
329000130918     C                   Eval      WStato      = D1Stato
329100130918     C                   EXSR      RepTabG12
329200130918     C*
329300130930     C                   if        XTBErr      = '0'
329400130930     C*
329500130930     C                   Eval      $NazKsc     = $NazISO
329600130930     C                   if        $NazKsc     = *Blank
329700130930     C                   Eval      $NazKsc     = D1Stato
329800130930     C                   endif
329900130918     C                   endif
330000130918     C*
330100130930     C                   Eval      $UICKsc     = D1CodUIC
330200130918     C*
330300130918     C                   ENDSR
330400130918      /EJECT
330500130911     C************************************************************
330600130911     C* Definizione dati per Quadri
330700130911     C************************************************************
330800130911     C     DefQuadri     BEGSR
330900130911     C*
330901160104R195 C                   Eval      WFormato    = FormatoA08
330902160104R195 C*
331000130911     C                   Select
331100130911     C*
331200130911     C*--------------------
331300130911     C* Formato AGGREGATO
331400130911     C                   When      FormatoA08  = 'A'
331500130911     C                   Select
331600130911     C*-------
331700130911     C* se conto Nazionale e documento Fattura, Nota o Riepilogo
331800130918     C                   When      $UICKsc     = $UICSoc       and
331900130911     C                             (D1TpDoc     = 'F'       or
332000130911     C                              D1TpDoc     = 'N'       or
332100130911     C                              D1TpDoc     = 'R'         )
332101160104R141 C*
332102160104 "   C* in caso di Autofattura, deve finire in FA perchè andrà
332103160104 "   C* girata sull'azienda comunicante
332104160104 "   C                   When      ($UICKsc     = $UICSoc       and
332105160104 "   C                              (D1TpDoc     = 'F'       or
332106160104 "   C                               D1TpDoc     = 'N'       or
332107160104 "   C                               D1TpDoc     = 'R'         )   ) or
332108160104R141 C                             D1Autofat   = XScOn
332200130911     C                   Eval      D1Quadro    = 'FA'
332300130912     C                   Eval      H1SeqQua    =  01
332400130911     C*
332500130911     C*
332600130911     C                   Eval      D1COGRAGR   = *Blank
332700130911     C                   Eval      D1NOMER     = *Blank
332800130923     C                   Eval      D1DTNASR    = 0
332900130911     C                   Eval      D1LOCNASR   = *Blank
333000130911     C                   Eval      D1PRVNASR   = *Blank
333100130911     C                   Eval      D1STATOR    = *Blank
333200130918     C                   Eval      D1CODUICR   = 0
333300130911     C                   Eval      D1LOCALIR   = *Blank
333400130911     C                   Eval      D1INDIRER   = *Blank
333500130923     C                   Eval      D1TpNR      = *Blank
333600130911     C                   Eval      D1PIVAR     = D1PartIVA
333700130911     C                   Eval      D1CDFISCR   = D1CdFisc
333800130925     C                   Eval      D1TpOpeR    = *Blank
333900130925     C                   Eval      D1NatOpeR   = *Blank
334000131015R135 X****               if        D1Leasing  <> '0'
334100131015 "   X****               Eval      D1LeasinR   = '1'
334200131015 "   X****               else
334300131015 "   X****               Eval      D1LeasinR   = '0'
334400131015R135 X****               endif
334500131015R135 C                   Eval      D1LeasinR   = D1Leasing
334600131111R140 C                   if        D1LeasinR   = '0'
334700131111 "   C                   Eval      D1LeasinR   = *Blank
334800131111R140 C                   endif
334900130923     C                   Eval      D1DTREGR    = 0
335000130911     C                   Eval      D1NRREGR    = *Zero
335100130911     C                   Eval      D1SERREGR   = *Blank
335200131015R135 C* in caso di Doc.Riepilogo verifico l'effettiva modalità di
335300131015 "   C* aggregazione impostata sul parametro modulo
335400131015 "   C                   if        DocRieA08   = 'D'       and
335500131015 "   C                             D1TpDoc     = 'R'
335600131015 "   C                   Eval      D1DTREGR    = D1DTREG
335700131015 "   C                   Eval      D1NRREGR    = D1NRREG
335800131015 "   C                   Eval      D1SERREGR   = D1SERREG
335900131015R135 C                   endif
336000131015     C*
336100130911     C*-------
336200130911     C* se documento Senza Fattura
336300130911     C                   When      D1TpDoc     = 'S'
336400131017R136 C* controllo se documento legato al turismo (ovvero,
336500131017 "   C* codice fiscale non impostato)
336600131017R136 C                   if        D1CdFisc   <> *Blank
336700130911     C                   Eval      D1Quadro    = 'SA'
336800130912     C                   Eval      H1SeqQua    = 02
336900130911     C*
337000130911     C                   Eval      D1COGRAGR   = *Blank
337100130911     C                   Eval      D1NOMER     = *Blank
337200130923     C                   Eval      D1DTNASR    = 0
337300130911     C                   Eval      D1LOCNASR   = *Blank
337400130911     C                   Eval      D1PRVNASR   = *Blank
337500130911     C                   Eval      D1STATOR    = *Blank
337600130918     C                   Eval      D1CODUICR   = 0
337700130911     C                   Eval      D1LOCALIR   = *Blank
337800130911     C                   Eval      D1INDIRER   = *Blank
337900130923     C                   Eval      D1TpNR      = *Blank
338000130911     C                   Eval      D1PIVAR     = *Blank
338100130911     C                   Eval      D1CDFISCR   = D1CdFisc
338200130925     C                   Eval      D1TpOpeR    = *Blank
338300130925     C                   Eval      D1NatOpeR   = *Blank
338400131015R135 X****               if        D1Leasing  <> '0'
338500131015 "   X****               Eval      D1LeasinR   = '1'
338600131015 "   X****               else
338700131015 "   X****               Eval      D1LeasinR   = '0'
338800131015R135 X****               endif
338900131015R135 C                   Eval      D1LeasinR   = D1Leasing
339000131111R140 C                   if        D1LeasinR   = '0'
339100131111 "   C                   Eval      D1LeasinR   = *Blank
339200131111R140 C                   endif
339300130923     C                   Eval      D1DTREGR    = 0
339400130911     C                   Eval      D1NRREGR    = *Zero
339500130911     C                   Eval      D1SERREGR   = *Blank
339600131017R136 C*
339700131017 "   C* si tratta di operazioni legate al Turismo
339800131017 "   C                   else
339900131017 "   C                   Eval      D1Quadro    = 'TU'
340000131017 "   C                   Eval      H1SeqQua    = 11
340100160404R136 C*
340200160404R2215X*ex R136           Eval      D1COGRAGR   = D1COGRAG
340201160404R2215C                   Eval      D1COGRAGR   = H1COGRAG
340300160404R136 C                   if        D1TpN       = '1'
340400131017 "   C                   Eval      D1NOMER     = H1NOME
340500131017 "   C*
340600131017 "   C                   if        H1DtNasc    = *Loval
340700131017 "   C                   Eval      D1DtNasR    = 0
340800131017 "   C                   else
340900131017 "   C*
341000131017 "   C                   Select
341100131017 "   C* formato mm/gg/aaaa
341200131017 "   C                   When      $DatFmt     = '*MDY'
341300131017 "   C     *USA          Move      H1DtNasc      D1DtNasR
341400131017 "   C* formato aa/mm/gg
341500131017 "   C                   When      $DatFmt     = '*YMD'
341600131017 "   C     *ISO          Move      H1DtNasc      D1DtNasR
341700131017 "   C* formato gg/mm/aaaa
341800131017 "   C                   When      $DatFmt     = '*DMY'
341900131017 "   C     *EUR          Move      H1DtNasc      D1DtNasR
342000131017 "   C                   endSl
342100131017 "   C*
342200131017 "   C                   endif
342300131017 "   C*
342400131017 "   C                   Eval      D1LOCNASR   = H1LOCNASC
342500131017 "   C                   Eval      D1PRVNASR   = H1PRVNASC
342600131017 "   C                   Eval      D1STATOR    = D1STATO
342700131017 "   C                   Eval      D1CODUICR   = D1CODUIC
342800131017 "   C                   Eval      D1LOCALIR   = H1LOCALIT
342900131017 "   C                   Eval      D1INDIRER   = H1INDIRE
343000131017 "   C                   else
343100131017 "   C                   endif
343200131017 "   C                   Eval      D1TpNR      = D1TPN
343300131017 "   C                   Eval      D1PIVAR     = *Blank
343400131017 "   C                   Eval      D1CDFISCR   = *Blank
343500131017 "   C                   Eval      D1TpOpeR    = *Blank
343600131017 "   C                   Eval      D1NatOpeR   = *Blank
343700131017 "   C                   Eval      D1LeasinR   = *Blank
343800131017 "   C                   Eval      D1DTREGR    = D1DTREG
343900131017 "   C                   Eval      D1NRREGR    = D1NRREG
344000131017 "   C                   Eval      D1SERREGR   = D1SERREG
344100131017 "   C                   Eval      D1NrOpe     = 0
344200131017R136 C                   endif
344300130911     C*
344400130911     C                   other
344500130911     C*-------
344600130911     C* se conto non residente
344700130911     C                   Eval      D1Quadro    = 'BL'
344800130912     C                   Eval      H1SeqQua    = 03
344900130911     C*
345002160404R2215X****               Eval      D1COGRAGR   = D1COGRAG
345003160404R2215C                   Eval      D1COGRAGR   = H1COGRAG
345100131001     C                   if        D1TpN       = '1'
345200130911     C                   Eval      D1NOMER     = H1NOME
345300130912     C*
345400130912     C                   if        H1DtNasc    = *Loval
345500130912     C                   Eval      D1DtNasR    = 0
345600130912     C                   else
345700130912     C*
345800130912     C                   Select
345900130912     C* formato mm/gg/aaaa
346000130912     C                   When      $DatFmt     = '*MDY'
346100130912     C     *USA          Move      H1DtNasc      D1DtNasR
346200130912     C* formato aa/mm/gg
346300130912     C                   When      $DatFmt     = '*YMD'
346400130912     C     *ISO          Move      H1DtNasc      D1DtNasR
346500130912     C* formato gg/mm/aaaa
346600130912     C                   When      $DatFmt     = '*DMY'
346700130912     C     *EUR          Move      H1DtNasc      D1DtNasR
346800130912     C                   endSl
346900130912     C*
347000130912     C                   endif
347100130912     C*
347200130911     C                   Eval      D1LOCNASR   = H1LOCNASC
347300130911     C                   Eval      D1PRVNASR   = H1PRVNASC
347400131001     C                   Eval      D1STATOR    = D1STATO
347500131001     C                   Eval      D1CODUICR   = D1CODUIC
347600131001     C                   else
347700131001     C                   Eval      D1STATOR    = D1STATO
347800130924     C                   Eval      D1CODUICR   = D1CODUIC
347900130911     C                   Eval      D1LOCALIR   = H1LOCALIT
348000130911     C                   Eval      D1INDIRER   = H1INDIRE
348100131001     C                   endif
348200130923     C                   Eval      D1TpNR      = D1TpN
348300130911     C                   Eval      D1PIVAR     = D1PartIVA
348400130911     C                   Eval      D1CDFISCR   = D1CdFisc
348500130925     C                   if        D1TpOpe     = 'P'        and
348600130925     C                             D1NatOpe    = 'S'
348700130925     C                   Eval      D1TpOpeR    = 'P'
348800130925     C                   Eval      D1NatOpeR   = 'S'
348900130925     C                   else
349000130925     C                   Eval      D1TpOpeR    = *Blank
349100130925     C                   Eval      D1NatOpeR   = *Blank
349200130925     C                   endif
349300130923     C                   Eval      D1LeasinR   = *Blank
349400130923     C                   Eval      D1DTREGR    = 0
349500130911     C                   Eval      D1NRREGR    = *Zero
349600130911     C                   Eval      D1SERREGR   = *Blank
349700130911     C*
349800130911     C                   endSl
349900130911     C*
350000130911     C*
350100130911     C*--------------------
350200130911     C* Formato DETTAGLIO
350300130911     C                   When      FormatoA08  = 'D'
350400130911     C                   Select
350500130911     C*-------
350600130911     C* se conto Nazionale e documento Fattura o Riepilogo
350700160104R195 X****               When      $UICKsc     = $UICSoc       and
350800160104 "   X****                         (D1TpDoc     = 'F'       or
350900160104R195 X****                          D1TpDoc     = 'R'         )
350901160104R195 C*
350902160104 "   C* in caso di Autofattura, deve finire in FE perchè andrà
350903160104 "   C* girata sull'azienda comunicante
350904160104 "   C                   When      ($UICKsc     = $UICSoc       and
350905160104 "   C                              (D1TpDoc     = 'F'       or
350906160104 "   C                               D1TpDoc     = 'R'         )   ) or
350907160104 "   C                             (D1Autofat   = XScOn         and
350908160104 "   C                              (D1TpDoc     = 'F'       or
350909160104R195 C                               D1TpDoc     = 'R'         )   )
351000130911     C* se documenti Emessi
351100130911     C                   if        D1TpOpe      = 'A'
351200130911     C                   Eval      D1Quadro    = 'FE'
351300130912     C                   Eval      H1SeqQua    = 04
351400130911     C*
351500130911     C                   Eval      D1COGRAGR   = *Blank
351600130911     C                   Eval      D1NOMER     = *Blank
351700130923     C                   Eval      D1DTNASR    = 0
351800130911     C                   Eval      D1LOCNASR   = *Blank
351900130911     C                   Eval      D1PRVNASR   = *Blank
352000130911     C                   Eval      D1STATOR    = *Blank
352100130918     C                   Eval      D1CODUICR   = 0
352200130911     C                   Eval      D1LOCALIR   = *Blank
352300130911     C                   Eval      D1INDIRER   = *Blank
352400130923     C                   Eval      D1TpNR      = *Blank
352500130911     C                   Eval      D1PIVAR     = D1PartIVA
352600130911     C                   Eval      D1CDFISCR   = D1CdFisc
352700130925     C                   Eval      D1TpOpeR    = *Blank
352800130925     C                   Eval      D1NatOpeR   = *Blank
352900130923     C                   Eval      D1LeasinR   = D1Leasing
353000131111R140 C                   if        D1LeasinR   = '0'
353100131111 "   C                   Eval      D1LeasinR   = *Blank
353200131111R140 C                   endif
353300130911     C                   Eval      D1DTREGR    = D1DTREG
353400130911     C                   Eval      D1NRREGR    = D1NRREG
353500130911     C                   Eval      D1SERREGR   = D1SERREG
353600130925     C                   Eval      D1NrOpe     = 0
353700130911     C*
353800130911     C                   else
353900130911     C*
354000130911     C* se documenti Ricevuti
354100130911     C                   Eval      D1Quadro    = 'FR'
354200130912     C                   Eval      H1SeqQua    = 05
354300130911     C*
354400130911     C                   Eval      D1COGRAGR   = *Blank
354500130911     C                   Eval      D1NOMER     = *Blank
354600130923     C                   Eval      D1DTNASR    = 0
354700130911     C                   Eval      D1LOCNASR   = *Blank
354800130911     C                   Eval      D1PRVNASR   = *Blank
354900130911     C                   Eval      D1STATOR    = *Blank
355000130918     C                   Eval      D1CODUICR   = 0
355100130911     C                   Eval      D1LOCALIR   = *Blank
355200130911     C                   Eval      D1INDIRER   = *Blank
355300130923     C                   Eval      D1TpNR      = *Blank
355400130911     C                   Eval      D1PIVAR     = D1PartIVA
355500130911     C                   Eval      D1CDFISCR   = *Blank
355600130925     C                   Eval      D1TpOpeR    = *Blank
355700130925     C                   Eval      D1NatOpeR   = *Blank
355800130923     C                   Eval      D1LeasinR   = *Blank
355900130911     C                   Eval      D1DTREGR    = D1DTREG
356000130911     C                   Eval      D1NRREGR    = D1NRREG
356100130911     C                   Eval      D1SERREGR   = D1SERREG
356200130925     C                   Eval      D1NrOpe     = 0
356300130911     C*
356400130911     C                   endif
356500130911     C*
356600130911     C*-------
356700130911     C* se conto Nazionale e documento Nota
356800160104R195 X****               When      $UICKsc     = $UICSoc       and
356900160104R195 X****                         D1TpDoc     = 'N'
356901160104R195 C*
356902160104 "   C* in caso di Autofattura, deve finire in NE perchè andrà
356903160104 "   C* girata sull'azienda comunicante
356904160104 "   C                   When      ($UICKsc     = $UICSoc  and
356905160104 "   C                              D1TpDoc     = 'N'         ) or
356906160104 "   C                             (D1Autofat   = *On      and
356907160104R195 C                              D1TpDoc     = 'N'         )
357000130911     C* se documenti Emessi
357100130911     C                   if        D1TpOpe      = 'A'
357200130911     C                   Eval      D1Quadro    = 'NE'
357300130912     C                   Eval      H1SeqQua    = 06
357400130911     C*
357500130911     C                   Eval      D1COGRAGR   = *Blank
357600130911     C                   Eval      D1NOMER     = *Blank
357700130923     C                   Eval      D1DTNASR    = 0
357800130911     C                   Eval      D1LOCNASR   = *Blank
357900130911     C                   Eval      D1PRVNASR   = *Blank
358000130911     C                   Eval      D1STATOR    = *Blank
358100130918     C                   Eval      D1CODUICR   = 0
358200130911     C                   Eval      D1LOCALIR   = *Blank
358300130911     C                   Eval      D1INDIRER   = *Blank
358400130923     C                   Eval      D1TpNR      = *Blank
358500130911     C                   Eval      D1PIVAR     = D1PartIVA
358600130911     C                   Eval      D1CDFISCR   = D1CdFisc
358700130925     C                   Eval      D1TpOpeR    = *Blank
358800130925     C                   Eval      D1NatOpeR   = *Blank
358900130923     C                   Eval      D1LeasinR   = *Blank
359000130911     C                   Eval      D1DTREGR    = D1DTREG
359100130911     C                   Eval      D1NRREGR    = D1NRREG
359200130911     C                   Eval      D1SERREGR   = D1SERREG
359300130925     C                   Eval      D1NrOpe     = 0
359400130911     C*
359500130911     C                   else
359600130911     C*
359700130911     C* se documenti Ricevuti
359800130911     C                   Eval      D1Quadro    = 'NR'
359900130912     C                   Eval      H1SeqQua    = 07
360000130911     C*
360100130911     C                   Eval      D1COGRAGR   = *Blank
360200130911     C                   Eval      D1NOMER     = *Blank
360300130923     C                   Eval      D1DTNASR    = 0
360400130911     C                   Eval      D1LOCNASR   = *Blank
360500130911     C                   Eval      D1PRVNASR   = *Blank
360600130911     C                   Eval      D1STATOR    = *Blank
360700130918     C                   Eval      D1CODUICR   = 0
360800130911     C                   Eval      D1LOCALIR   = *Blank
360900130911     C                   Eval      D1INDIRER   = *Blank
361000130923     C                   Eval      D1TpNR      = *Blank
361100130911     C                   Eval      D1PIVAR     = D1PartIVA
361200130911     C                   Eval      D1CDFISCR   = *Blank
361300130925     C                   Eval      D1TpOpeR    = *Blank
361400130925     C                   Eval      D1NatOpeR   = *Blank
361500130923     C                   Eval      D1LeasinR   = *Blank
361600130911     C                   Eval      D1DTREGR    = D1DTREG
361700130911     C                   Eval      D1NRREGR    = D1NRREG
361800130911     C                   Eval      D1SERREGR   = D1SERREG
361900130925     C                   Eval      D1NrOpe     = 0
362000130911     C*
362100130911     C                   endif
362200130911     C*
362300130911     C*-------
362400130911     C* se documento Senza Fattura
362500130911     C                   When      D1TpDoc     = 'S'
362600131017R136 C* controllo se documento legato al turismo (ovvero,
362700131017 "   C* codice fiscale non impostato)
362800131017R136 C                   if        D1CdFisc   <> *Blank
362900130911     C                   Eval      D1Quadro    = 'DF'
363000130912     C                   Eval      H1SeqQua    = 08
363100130911     C*
363200130911     C                   Eval      D1COGRAGR   = *Blank
363300130911     C                   Eval      D1NOMER     = *Blank
363400130923     C                   Eval      D1DTNASR    = 0
363500130911     C                   Eval      D1LOCNASR   = *Blank
363600130911     C                   Eval      D1PRVNASR   = *Blank
363700130911     C                   Eval      D1STATOR    = *Blank
363800130918     C                   Eval      D1CODUICR   = 0
363900130911     C                   Eval      D1LOCALIR   = *Blank
364000130911     C                   Eval      D1INDIRER   = *Blank
364100130923     C                   Eval      D1TpNR      = *Blank
364200130911     C                   Eval      D1PIVAR     = *Blank
364300130911     C                   Eval      D1CDFISCR   = D1CdFisc
364400130925     C                   Eval      D1TpOpeR    = *Blank
364500130925     C                   Eval      D1NatOpeR   = *Blank
364600131015R135 X****               if        D1Leasing  <> '0'
364700131015 "   X****               Eval      D1LeasinR   = '1'
364800131015 "   X****               else
364900131015 "   X****               Eval      D1LeasinR   = '0'
365000131015R135 X****               endif
365100131015R135 C                   Eval      D1LeasinR   = D1Leasing
365200131111R140 C                   if        D1LeasinR   = '0'
365300131111 "   C                   Eval      D1LeasinR   = *Blank
365400131111R140 C                   endif
365500130911     C                   Eval      D1DTREGR    = D1DTREG
365600130911     C                   Eval      D1NRREGR    = D1NRREG
365700130911     C                   Eval      D1SERREGR   = D1SERREG
365800130925     C                   Eval      D1NrOpe     = 0
365900131017R136 C*
366000131017 "   C* si tratta di operazioni legate al Turismo
366100131017 "   C                   else
366200131017 "   C                   Eval      D1Quadro    = 'TU'
366300131017 "   C                   Eval      H1SeqQua    = 11
366400160404R136 C*
366501160404R2215X*ex R136           Eval      D1COGRAGR   = D1COGRAG
366502160404R2215C                   Eval      D1COGRAGR   = H1COGRAG
366600160404R136 C                   if        D1TpN       = '1'
366700131017 "   C                   Eval      D1NOMER     = H1NOME
366800131017 "   C*
366900131017 "   C                   if        H1DtNasc    = *Loval
367000131017 "   C                   Eval      D1DtNasR    = 0
367100131017 "   C                   else
367200131017 "   C*
367300131017 "   C                   Select
367400131017 "   C* formato mm/gg/aaaa
367500131017 "   C                   When      $DatFmt     = '*MDY'
367600131017 "   C     *USA          Move      H1DtNasc      D1DtNasR
367700131017 "   C* formato aa/mm/gg
367800131017 "   C                   When      $DatFmt     = '*YMD'
367900131017 "   C     *ISO          Move      H1DtNasc      D1DtNasR
368000131017 "   C* formato gg/mm/aaaa
368100131017 "   C                   When      $DatFmt     = '*DMY'
368200131017 "   C     *EUR          Move      H1DtNasc      D1DtNasR
368300131017 "   C                   endSl
368400131017 "   C*
368500131017 "   C                   endif
368600131017 "   C*
368700131017 "   C                   Eval      D1LOCNASR   = H1LOCNASC
368800131017 "   C                   Eval      D1PRVNASR   = H1PRVNASC
368900131017 "   C                   Eval      D1STATOR    = D1STATO
369000131017 "   C                   Eval      D1CODUICR   = D1CODUIC
369100131017 "   C                   Eval      D1LOCALIR   = H1LOCALIT
369200131017 "   C                   Eval      D1INDIRER   = H1INDIRE
369300131017 "   C                   endif
369400131017 "   C                   Eval      D1TpNR      = D1TPN
369500131017 "   C                   Eval      D1PIVAR     = *Blank
369600131017 "   C                   Eval      D1CDFISCR   = *Blank
369700131017 "   C                   Eval      D1TpOpeR    = *Blank
369800131017 "   C                   Eval      D1NatOpeR   = *Blank
369900131017 "   C                   Eval      D1LeasinR   = *Blank
370000131017 "   C                   Eval      D1DTREGR    = D1DTREG
370100131017 "   C                   Eval      D1NRREGR    = D1NRREG
370200131017 "   C                   Eval      D1SERREGR   = D1SERREG
370300131017 "   C                   Eval      D1NrOpe     = 0
370400131017R136 C                   endif
370500130911     C*
370600130911     C*
370700130911     C                   other
370800130911     C*-------
370900130911     C* se conto non residente
371000131015R135 X**** e Acquisti di Servizi
371100131015 "   X****               if        D1TpOpe     = 'P'    and
371200131015R135 X****                         D1NatOpe    = 'S'
371300131015R135 C* e Acquisti di Beni o di Servizi
371400131015R135 C                   if        D1TpOpe     = 'P'
371500130911     C                   Eval      D1Quadro    = 'SE'
371600130912     C                   Eval      H1SeqQua    = 10
371700130911     C*
371801160404R2215X****               Eval      D1COGRAGR   = D1COGRAG
371802160404R2215C                   Eval      D1COGRAGR   = H1COGRAG
371900131001     C                   if        D1TpN       = '1'
372000130911     C                   Eval      D1NOMER     = H1NOME
372100130912     C*
372200130912     C                   if        H1DtNasc    = *Loval
372300130912     C                   Eval      D1DtNasR    = 0
372400130912     C                   else
372500130912     C*
372600130912     C                   Select
372700130912     C* formato mm/gg/aaaa
372800130912     C                   When      $DatFmt     = '*MDY'
372900130912     C     *USA          Move      H1DtNasc      D1DtNasR
373000130912     C* formato aa/mm/gg
373100130912     C                   When      $DatFmt     = '*YMD'
373200130912     C     *ISO          Move      H1DtNasc      D1DtNasR
373300130912     C* formato gg/mm/aaaa
373400130912     C                   When      $DatFmt     = '*DMY'
373500130912     C     *EUR          Move      H1DtNasc      D1DtNasR
373600130912     C                   endSl
373700130912     C*
373800130912     C                   endif
373900130912     C*
374000130911     C                   Eval      D1LOCNASR   = H1LOCNASC
374100130911     C                   Eval      D1PRVNASR   = H1PRVNASC
374200130911     C                   Eval      D1STATOR    = D1STATO
374300130924     C                   Eval      D1CODUICR   = D1CODUIC
374400131001     C                   else
374500131001     C                   Eval      D1STATOR    = D1STATO
374600131001     C                   Eval      D1CODUICR   = D1CODUIC
374700130911     C                   Eval      D1LOCALIR   = H1LOCALIT
374800130911     C                   Eval      D1INDIRER   = H1INDIRE
374900131001     C                   endif
375000130923     C                   Eval      D1TpNR      = D1TPN
375100130911     C                   Eval      D1PIVAR     = D1PartIVA
375200130911     C                   Eval      D1CDFISCR   = D1CdFisc
375300160201R195 X****               Eval      D1TpOpeR    = *Blank
375301160201R195 C                   Eval      D1TpOpeR    = D1TpOpe
375400130925     C                   Eval      D1NatOpeR   = *Blank
375500130923     C                   Eval      D1LeasinR   = *Blank
375600130911     C                   Eval      D1DTREGR    = D1DTREG
375700130911     C                   Eval      D1NRREGR    = D1NRREG
375800130911     C                   Eval      D1SERREGR   = D1SERREG
375900130925     C                   Eval      D1NrOpe     = 0
376000130911     C*
376100130911     C                   else
376200130911     C*
376300130911     C* se conto non residente
376400131015R135 X**** e Vendite o Acquisti di Beni
376500131015R135 C* e Vendite di Beni o Servizi
376600130911     C                   Eval      D1Quadro    = 'FN'
376700130912     C                   Eval      H1SeqQua    = 09
376800130911     C*
376801160404R2215X****               Eval      D1COGRAGR   = D1COGRAG
376802160404R2215C                   Eval      D1COGRAGR   = H1COGRAG
377000131001     C                   if        D1TpN       = '1'
377100130911     C                   Eval      D1NOMER     = H1NOME
377200130912     C*
377300130912     C                   if        H1DtNasc    = *Loval
377400130912     C                   Eval      D1DtNasR    = 0
377500130912     C                   else
377600130912     C*
377700130912     C                   Select
377800130912     C* formato mm/gg/aaaa
377900130912     C                   When      $DatFmt     = '*MDY'
378000130912     C     *USA          Move      H1DtNasc      D1DtNasR
378100130912     C* formato aa/mm/gg
378200130912     C                   When      $DatFmt     = '*YMD'
378300130912     C     *ISO          Move      H1DtNasc      D1DtNasR
378400130912     C* formato gg/mm/aaaa
378500130912     C                   When      $DatFmt     = '*DMY'
378600130912     C     *EUR          Move      H1DtNasc      D1DtNasR
378700130912     C                   endSl
378800130912     C*
378900130912     C                   endif
379000130912     C*
379100130911     C                   Eval      D1LOCNASR   = H1LOCNASC
379200130911     C                   Eval      D1PRVNASR   = H1PRVNASC
379300130911     C                   Eval      D1STATOR    = D1STATO
379400130924     C                   Eval      D1CODUICR   = D1CODUIC
379500131001     C                   else
379600131001     C                   Eval      D1STATOR    = D1STATO
379700131001     C                   Eval      D1CODUICR   = D1CODUIC
379800160104R195 X****               endif
379900130911     C                   Eval      D1LOCALIR   = H1LOCALIT
380000130911     C                   Eval      D1INDIRER   = H1INDIRE
380001160104R195 C                   endif
380100130923     C                   Eval      D1TpNR      = D1TPN
380200130911     C                   Eval      D1PIVAR     = *Blank
380300130911     C                   Eval      D1CDFISCR   = *Blank
380400130925     C                   Eval      D1TpOpeR    = *Blank
380500130925     C                   Eval      D1NatOpeR   = *Blank
380600130923     C                   Eval      D1LeasinR   = D1Leasing
380700131111R140 C                   if        D1LeasinR   = '0'
380800131111 "   C                   Eval      D1LeasinR   = *Blank
380900131111R140 C                   endif
381000130911     C                   Eval      D1DTREGR    = D1DTREG
381100130911     C                   Eval      D1NRREGR    = D1NRREG
381200130911     C                   Eval      D1SERREGR   = D1SERREG
381300130925     C                   Eval      D1NrOpe     = 0
381400130911     C*
381500130911     C                   endif
381600130911     C*
381700130911     C                   endSl
381800130911     C*
381900130911     C                   endSl
382000130911     C*
382100130911     C                   ENDSR
382200130911     C/EJECT
382201160104R195 C************************************************************
382202160104 "   C* Definizione dati per Quadro SE per Aquisti da San Marino
382203160104 "   C************************************************************
382204160104R195 C     DefQuadroSE   BEGSR
382205160104     C*
382206160104     C                   Eval      WFormato    = 'D'
382208160104     C                   Eval      D1Quadro    = 'SE'
382209160104     C                   Eval      H1SeqQua    = 10
382210160104     C*
382211160404R2215X****               Eval      D1COGRAGR   = D1COGRAG
382212160404R2215C                   Eval      D1COGRAGR   = H1COGRAG
382214160104     C                   if        D1TpN       = '1'
382215160104     C                   Eval      D1NOMER     = H1NOME
382216160104     C*
382217160104     C                   if        H1DtNasc    = *Loval
382218160104     C                   Eval      D1DtNasR    = 0
382219160104     C                   else
382220160104     C*
382221160104     C                   Select
382222160104     C* formato mm/gg/aaaa
382223160104     C                   When      $DatFmt     = '*MDY'
382224160104     C     *USA          Move      H1DtNasc      D1DtNasR
382225160104     C* formato aa/mm/gg
382226160104     C                   When      $DatFmt     = '*YMD'
382227160104     C     *ISO          Move      H1DtNasc      D1DtNasR
382228160104     C* formato gg/mm/aaaa
382229160104     C                   When      $DatFmt     = '*DMY'
382230160104     C     *EUR          Move      H1DtNasc      D1DtNasR
382231160104     C                   endSl
382232160104     C*
382233160104     C                   endif
382234160104     C*
382235160104     C                   Eval      D1LOCNASR   = H1LOCNASC
382236160104     C                   Eval      D1PRVNASR   = H1PRVNASC
382237160104     C                   Eval      D1STATOR    = D1STATO
382238160104     C                   Eval      D1CODUICR   = D1CODUIC
382239160104     C                   else
382240160104     C                   Eval      D1STATOR    = D1STATO
382241160104     C                   Eval      D1CODUICR   = D1CODUIC
382242160104     C                   Eval      D1LOCALIR   = H1LOCALIT
382243160104     C                   Eval      D1INDIRER   = H1INDIRE
382244160104     C                   endif
382245160104     C                   Eval      D1TpNR      = D1TPN
382246160104     C                   Eval      D1PIVAR     = D1PartIVA
382247160104     C                   Eval      D1CDFISCR   = D1CdFisc
382248160201     C                   Eval      D1TpOpeR    = D1TpOpe
382249160104     C                   Eval      D1NatOpeR   = *Blank
382250160104     C                   Eval      D1LeasinR   = *Blank
382251160104     C                   Eval      D1DTREGR    = D1DTREG
382252160104     C                   Eval      D1NRREGR    = D1NRREG
382253160104     C                   Eval      D1SERREGR   = D1SERREG
382254160104     C                   Eval      D1NrOpe     = 0
382255160104     C*
382256160104R195 C                   ENDSR
382257160104R195 C/EJECT
382258160104R195 C************************************************************
382259160104 "   C* Definizione dati per Quadro BL in comunicaz. Black List
382260160104 "   C************************************************************
382261160104R195 C     DefQuadroBL   BEGSR
382262160104     C*
382263160104     C                   Eval      WFormato    = 'A'
382264160104     C                   Eval      D1Quadro    = 'BL'
382265160104     C                   Eval      H1SeqQua    = 03
382266160104     C*
382267160404R2215X****               Eval      D1COGRAGR   = D1COGRAG
382268160404R2215C                   Eval      D1COGRAGR   = H1COGRAG
382270160104     C                   if        D1TpN       = '1'
382271160104     C                   Eval      D1NOMER     = H1NOME
382272160104     C*
382273160104     C                   if        H1DtNasc    = *Loval
382274160104     C                   Eval      D1DtNasR    = 0
382275160104     C                   else
382276160104     C*
382277160104     C                   Select
382278160104     C* formato mm/gg/aaaa
382279160104     C                   When      $DatFmt     = '*MDY'
382280160104     C     *USA          Move      H1DtNasc      D1DtNasR
382281160104     C* formato aa/mm/gg
382282160104     C                   When      $DatFmt     = '*YMD'
382283160104     C     *ISO          Move      H1DtNasc      D1DtNasR
382284160104     C* formato gg/mm/aaaa
382285160104     C                   When      $DatFmt     = '*DMY'
382286160104     C     *EUR          Move      H1DtNasc      D1DtNasR
382287160104     C                   endSl
382288160104     C*
382289160104     C                   endif
382290160104     C*
382291160104     C                   Eval      D1LOCNASR   = H1LOCNASC
382292160104     C                   Eval      D1PRVNASR   = H1PRVNASC
382293160104     C                   Eval      D1STATOR    = D1STATO
382294160104     C                   Eval      D1CODUICR   = D1CODUIC
382295160104     C                   else
382296160104     C                   Eval      D1STATOR    = D1STATO
382297160104     C                   Eval      D1CODUICR   = D1CODUIC
382298160104     C                   Eval      D1LOCALIR   = H1LOCALIT
382299160104     C                   Eval      D1INDIRER   = H1INDIRE
382300160104     C                   endif
382301160104     C                   Eval      D1TpNR      = D1TpN
382302160104     C                   Eval      D1PIVAR     = D1PartIVA
382303160104     C                   Eval      D1CDFISCR   = D1CdFisc
382306160104     C                   Eval      D1TpOpeR    = *Blank
382307160104     C                   Eval      D1NatOpeR   = *Blank
382309160104     C                   Eval      D1LeasinR   = *Blank
382310160104     C                   Eval      D1DTREGR    = 0
382311160104     C                   Eval      D1NRREGR    = *Zero
382312160104     C                   Eval      D1SERREGR   = *Blank
382313160104     C*
382314160104R195 C                   ENDSR
382315160104R195 C/EJECT
382316130909     C************************************************************
382400130909     C* Controllo quadro di presentazione
382500130909     C************************************************************
382600130909     C     CtrQuadro     BEGSR
382700130909     C*
382800130909     C                   Clear                   D1QuadroD
382900130909     C                   Clear                   X07ValDs
383000130909     C*
383100130909     C                   Move      '1'           X07Ric
383200130909     C                   Move      *On           X07All
383201160105R195 C*
383202160105 "   C                   Select
383203160105 "   C*
383204160105 "   C* Spesometro
383205160105R195 C                   When      TpComG76    = 'E'
383300130909     C                   MoveL     'SDGSCF000'   X07TRc
383400130909     C                   MoveL     'SCFQUADRO '  X07Cam
383401160105R195 C*
383402160105 "   C* Acquisti da San Marino
383403160105 "   C                   When      TpComG76    = 'S'
383404160105 "   C                   MoveL     'ANDIZ000 '   X07TRc
383405160105 "   C                   MoveL     'SDGQUADRO1'  X07Cam
383406160105 "   C*
383407160105 "   C* Black List
383408160105 "   C                   When      TpComG76    = 'B'
383409160105 "   C                   MoveL     'ANDIZ000 '   X07TRc
383410160105 "   C                   MoveL     'SDGQUADRO2'  X07Cam
383411160105 "   C                   endSl
383412160105R195 C*
383500130909     C                   MoveL     D1Quadro      X07VAL
383600130924     C*
383700130909     C                   CALLB     'X07VALR'
383800130909     C                   PARM                    X07ValDs
383900140401     C*
384000130909     C     X07ERR        IFEQ      *Off
384100130909     C                   MoveL     X07DES        D1QuadroD
384200130909     C                   else
384300130923     C                   SetOn                                        5099
384400130909     C                   endif
384500130924     C*
384501160105R195 C                   Select
384502160105 "   C* /***************************************************/
384503160105 "   C*
384504160105 "   C* Spesometro
384505160105R195 C                   When      TpComG76    = 'E'
384506160209R195 C                   EXSR      CtrQuadri
384600130924     C*
384700160209R195 X**** controllo congruenza quadro e altri dati
384800160209 "   X****               Select
384900160209 "   X****
385000160209 "   X****--------------------
385100160209 "   X**** Formato AGGREGATO
385200160209 "   X****               When      FormatoA08  = 'A'
385300160209 "   X****               Select
385400160209 "   X****-------
385500160209 "   X**** se conto Nazionale e documento Fattura, Nota o Riepilogo
385600160209 "   X****               When      $UICKsc     = $UICSoc       and
385700160209 "   X****                         (D1TpDoc     = 'F'       or
385800160209 "   X****                          D1TpDoc     = 'N'       or
385900160209 "   X****                          D1TpDoc     = 'R'         )and
386000160209 "   X****                         D1Quadro    = 'FA'
386100160209 "   X****               Eval      H1SeqQua    =  01
386200160209 "   X****
386300160209 "   X****-------
386400160209 "   X**** se documento Senza Fattura
386500160209 "   X****               When      D1TpDoc     = 'S'      and
386600160209 "   X****                         D1Quadro    = 'SA'
386700160209 "   X****               Eval      H1SeqQua    =  02
386800160209 "   X*ex R136
386900160209 "   X*ex R136 ------
387000160209 "   X*ex R136 se documento Senza Fattura legato al Turismo
387100160209 "   X*ex R136           When      D1TpDoc     = 'S'      and
387200160209 "   X*ex R136                     D1Quadro    = 'TU'
387300160209 "   X****-------
387400160209 "   X**** se conto non residente
387500160209 "   X****               When      $UICKsc    <> $UICSoc       and
387600160209 "   X****                         D1Quadro    = 'BL'
387700160209 "   X****               Eval      H1SeqQua    =  03
387800160209 "   X****
387900160209 "   X* ex R133 se Acquisti da San Marino deve essere
388000160105 "   X* ex R133 analitica e quadro SE (anche se p.modulo dice aggregata)
388100160105 "   X* ex R133          When      $UICKsc    <> $UICSoc       and
388200160105 "   X* ex R133                    D1Quadro    = 'SE'
388300160105 "   X* ex R133          Eval      H1SeqQua    =  03
388400160209 "   X* ex R133
388500160209 "   X****               other
388600160209 "   X****               SetOn                                        5099
388700160209 "   X****
388800160209 "   X****               endSl
388900160209 "   X****
389000160209 "   X****
389100160209 "   X****--------------------
389200160209 "   X**** Formato DETTAGLIO
389300160209 "   X****               When      FormatoA08  = 'D'
389400160209 "   X****               Select
389500160209 "   X****-------
389600160209 "   X**** se conto Nazionale e documento Fattura o Riepilogo
389700160209 "   X****               When      ($UICKsc     = $UICSoc       and
389800160209 "   X****                          (D1TpDoc     = 'F'       or
389900160209 "   X****                           D1TpDoc     = 'R'         )   )
390000160209 "   X****                         AND
390100160209 "   X**** se documenti Emessi
390200160209 "   X****                         ((D1TpOpe      = 'A'          and
390300160209 "   X****                           D1Quadro    = 'FE'             )or
390400160209 "   X**** se documenti Ricevuti
390500160209 "   X****                          (D1TpOpe      = 'P'          and
390600160209 "   X****                           D1Quadro    = 'FR'             )  )
390700160209 "   X****
390800160209 "   X****-------
390900160209 "   X**** se conto Nazionale e documento Nota
391000160209 "   X****               When      ($UICKsc     = $UICSoc       and
391100160209 "   X****                          D1TpDoc     = 'N'              )
391200160209 "   X****                         AND
391300160209 "   X**** se documenti Emessi
391400160209 "   X****                         ((D1TpOpe      = 'A'          and
391500160209 "   X****                           D1Quadro    = 'NE'             )or
391600160209 "   X**** se documenti Ricevuti
391700160209 "   X****                          (D1TpOpe      = 'P'          and
391800160209 "   X****                           D1Quadro    = 'NR'             )  )
391900160209 "   X****
392000160209 "   X****-------
392100160209 "   X**** se documento Senza Fattura
392200160209 "   X****               When      D1TpDoc     = 'S'           and
392300160209 "   X****                         D1Quadro    = 'DF'
392400160209 "   X*ex R136
392500160209 "   X*ex R136 -------
392600160209 "   X*ex R136 se documento Senza Fattura legato al Turismo e persona fisica
392700160209 "   X*ex R136           When      D1TpDoc     = 'S'      and
392800160209 "   X*ex R136                     D1Quadro    = 'TU'     and
392900160209 "   X*ex R136                     D1TpN       = '1'
392901160209 "   X*ex R136                     D1TpNR      = '1'
393000160209 "   X****-------
393100160209 "   X**** se conto non residente
393200160209 "   X****               When      $UICKsc    <> $UICSoc
393300160209R195 X****                         AND
393400131016R135 X**** se Acquisti di Servizi
393500160209R195 X*ex R135 se Acquisto da non residente
393600160209R195 X****                         ((D1TpOpe     = 'P'           and
393700131016R135 X****                           D1NatOpe    = 'S'           and
393800160209R195 X****                           D1Quadro    = 'SE'             )or
393900160209 "   X**** se Vendite o Acquisti di Beni
394000160209 "   X****                           D1Quadro    = 'FN'                )
394100160209 "   X****
394200160209 "   X****               other
394300160209 "   X****               SetOn                                        5099
394400160209 "   X****
394500160209 "   X****               endSl
394600160209 "   X****
394700160209 "   X****               endSl
394800160209 "   X*ex R135
394900160209 "   X**** se Acquisto di Beni da non residente e non annullato,
395000160209 "   X**** segnalo errore
395100160209 "   X*ex R135           if        D1TpOpe     = 'P'           and
395200160209 "   X*ex R135                     D1NatOpe    = 'B'           and
395300160209 "   X*ex R135                     D1Quadro    = 'SE'          and
395400160209 "   X*ex R133                     TpComG76   <> 'S'           and
395500160209 "   X*ex R135                     D1Ann      <> XScOn
395600160209 "   X*ex R135           SetOn                                        7099
395700160209R195 X*ex R135           endif
395701160105R195 C*
395702160105 "   C* /***************************************************/
395703160105 "   C*
395704160105 "   C* Acquisti da San Marino
395705160105 "   C                   When      TpComG76    = 'S'
395706160209 "   C                   EXSR      CtrQuadroSE
395722160105 "   C*
395723160105 "   C* /***************************************************/
395724160105 "   C*
395725160105 "   C* Black List
395726160105 "   C                   When      TpComG76    = 'B'
395727160209 "   C                   EXSR      CtrQuadroBL
395738160105 "   C*
395739160105 "   C* /***************************************************/
395740160105 "   C*
395741160105R195 C                   endSl
395800131017R136 C*
395900131017 "   C                   Select
396000131017 "   C                   When      D1Quadro    = 'FA'
396100131017 "   C                   Eval      H1SeqQua    =  01
396200131017 "   C*
396300131017 "   C                   When      D1Quadro    = 'SA'
396400131017 "   C                   Eval      H1SeqQua    =  02
396500131017 "   C*
396600131211R136 C                   When      D1Quadro    = 'BL'
396700131211R141 X*ex R136           Eval      H1SeqQua    =  02
396800131211R141 C                   Eval      H1SeqQua    =  03
396900131211R136 C*
397000131017 "   C                   When      D1Quadro    = 'FE'
397100131017 "   C                   Eval      H1SeqQua    =  04
397200131017 "   C*
397300131017 "   C                   When      D1Quadro    = 'FR'
397400131017 "   C                   Eval      H1SeqQua    =  05
397500131017 "   C*
397600131017 "   C                   When      D1Quadro    = 'NE'
397700131017 "   C                   Eval      H1SeqQua    =  06
397800131017 "   C*
397900131017 "   C                   When      D1Quadro    = 'NR'
398000131017 "   C                   Eval      H1SeqQua    =  07
398100131017 "   C*
398200131017 "   C                   When      D1Quadro    = 'DF'
398300131017 "   C                   Eval      H1SeqQua    =  08
398400131017 "   C*
398500131017 "   C                   When      D1Quadro    = 'FN'
398600131017 "   C                   Eval      H1SeqQua    =  09
398700131017 "   C*
398800131017 "   C                   When      D1Quadro    = 'SE'
398900131017 "   C                   Eval      H1SeqQua    =  10
399000131017 "   C*
399100131017 "   C                   When      D1Quadro    = 'TU'
399200131017 "   C                   Eval      H1SeqQua    =  11
399300131017R136 C                   endSl
399400130924     C*
399500130909     C                   ENDSR
399600130909     C/EJECT
399601160209R195 C************************************************************
399602160209 "   C* Controllo quadri dello Spesometro
399603160209 "   C************************************************************
399604160209R195 C     CtrQuadri     BEGSR
399605160209     C*
399607160209     C* controllo congruenza quadro e altri dati
399608160209     C                   Select
399609160209     C*
399610160209     C*--------------------
399611160209     C* Formato AGGREGATO
399612160209     C                   When      FormatoA08  = 'A'
399613160209     C                   Select
399614160209     C*-------
399615160209     C* se conto Nazionale e documento Fattura, Nota o Riepilogo
399616160209     C                   When      $UICKsc     = $UICSoc       and
399617160209     C                             (D1TpDoc     = 'F'       or
399618160209     C                              D1TpDoc     = 'N'       or
399619160209     C                              D1TpDoc     = 'R'         )and
399620160209     C                             D1Quadro    = 'FA'
399621160209     C                   Eval      H1SeqQua    =  01
399622160209     C*
399623160209     C*-------
399624160209     C* se documento Senza Fattura
399625160209     C                   When      D1TpDoc     = 'S'      and
399626160209     C                             D1Quadro    = 'SA'
399627160209     C                   Eval      H1SeqQua    =  02
399628160209     C*
399629160209     C*-------
399630160209     C* se documento Senza Fattura legato al Turismo
399631160209     C                   When      D1TpDoc     = 'S'      and
399632160209     C                             D1Quadro    = 'TU'
399633160209     C*-------
399634160209     C* se conto non residente
399635160209     C                   When      $UICKsc    <> $UICSoc       and
399636160209     C                             D1Quadro    = 'BL'
399637160209     C                   Eval      H1SeqQua    =  03
399638160209     C*
399645160209     C                   other
399646160209     C                   SetOn                                        5099
399647160209     C*
399648160209     C                   endSl
399649160209     C*
399650160209     C*
399651160209     C*--------------------
399652160209     C* Formato DETTAGLIO
399653160209     C                   When      FormatoA08  = 'D'
399654160209     C                   Select
399655160209     C*-------
399656160209     C* se conto Nazionale e documento Fattura o Riepilogo
399657160209     C                   When      ($UICKsc     = $UICSoc       and
399658160209     C                              (D1TpDoc     = 'F'       or
399659160209     C                               D1TpDoc     = 'R'         )   )
399660160209     C                             AND
399661160209     C* se documenti Emessi
399663160209R195 C                             ((D1TpOpeR     = 'A'          and
399664160209     C                               D1Quadro    = 'FE'             )or
399665160209     C* se documenti Ricevuti
399667160209R195 C                              (D1TpOpeR     = 'P'          and
399668160209     C                               D1Quadro    = 'FR'             )  )
399669160209     C*
399670160209     C*-------
399671160209     C* se conto Nazionale e documento Nota
399673160209R195 C                   When      (D1CodUICR   = $UICSoc       and
399674160209     C                              D1TpDoc     = 'N'              )
399675160209     C                             AND
399676160209     C* se documenti Emessi
399678160209R195 C                             ((D1TpOpeR     = 'A'          and
399679160209     C                               D1Quadro    = 'NE'             )or
399680160209     C* se documenti Ricevuti
399682160209R195 C                              (D1TpOpeR     = 'P'          and
399683160209     C                               D1Quadro    = 'NR'             )  )
399684160209     C*
399685160209     C*-------
399686160209     C* se documento Senza Fattura
399687160209     C                   When      D1TpDoc     = 'S'           and
399688160209     C                             D1Quadro    = 'DF'
399689160209     C*
399690160209     C*-------
399691160209     C* se documento Senza Fattura legato al Turismo e persona fisica
399692160209     C                   When      D1TpDoc     = 'S'      and
399693160209     C                             D1Quadro    = 'TU'     and
399695160209     C                             D1TpNR      = '1'
399696160209     C*-------
399697160209     C* se conto non residente
399699160209R195 C                   When      D1CodUICR  <> $UICSoc
399700160209     C                             AND
399702160209     C* se Acquisto da non residente
399704160209R195 C                             ((D1TpOpeR    = 'P'           and
399706160209     C                               D1Quadro    = 'SE'             )or
399707160209     C* se Vendite o Acquisti di Beni
399708160209     C                               D1Quadro    = 'FN'                )
399709160209     C*
399710160209     C                   other
399711160209     C                   SetOn                                        5099
399712160209     C*
399713160209     C                   endSl
399714160209     C*
399715160209     C                   endSl
399716160209     C*
399717160209     C* se Acquisto di Beni da non residente e non annullato,
399718160209     C* segnalo errore
399721160209R195 C                   if        D1TpOpeR    = 'P'           and
399722160209R195 C                             D1NatOpeR   = 'B'           and
399723160209     C                             D1Quadro    = 'SE'          and
399725160209     C                             D1Ann      <> XScOn
399726160209     C                   SetOn                                        7099
399727160209     C                   endif
399728160209     C*
399729160209R195 C                   ENDSR
399730160209R195 C/EJECT
399731160209R195 C************************************************************
399732160209 "   C* Controllo quadro SE per gli Acquisti da San Marino
399733160209 "   C************************************************************
399734160209R195 C     CtrQuadroSE   BEGSR
399735160209     C*
399737160209     C                   Select
399738160209     C*-------
399739160209     C* se conto non residente
399740160209     C                   When      D1CodUICR  <> $UICSoc
399741160209     C                             AND
399742160209     C* se Acquisto da non residente
399743160209     C                             D1TpOpeR    = 'P'           and
399744160209     C                             D1Quadro    = 'SE'
399745160209     C*
399746160209     C                   other
399747160209     C                   SetOn                                        5099
399748160209     C*
399749160209     C                   endSl
399750160209     C*
399751160209     C                   Eval      D1NatOpeR   = *Blank
399752160209     C                   Eval      D1LeasinR   = *Blank
399753160209     C*
399754160209R195 C                   ENDSR
399755160209R195 C/EJECT
399756160209R195 C************************************************************
399757160209 "   C* Controllo quadro BL per Black List
399758160209 "   C************************************************************
399759160209R195 C     CtrQuadroBL   BEGSR
399760160209     C*
399761160209     C                   Select
399762160209     C*-------
399763160209     C* se conto non residente
399764160209     C                   When      D1CodUICR  <> $UICSoc       and
399765160209     C                             D1Quadro    = 'BL'
399766160209     C                   other
399767160209     C                   SetOn                                        5099
399768160209     C*
399769160209     C                   endSl
399770160209     C*
399771160209     C                   Eval      D1TpOpeR    = *Blank
399772160209     C                   Eval      D1NatOpeR   = *Blank
399773160209     C                   Eval      D1LeasinR   = *Blank
399774160209     C*
399775160209R195 C                   ENDSR
399776160209R195 C/EJECT
399777130924     C************************************************************
399800130924     C* Controllo tipo natura giuridica
399900130924     C************************************************************
400000130924     C     CtrTpNR       BEGSR
400100130924     C*
400200130924     C                   Clear                   D1TpNRD
400300130924     C*
400400130924     C                   Clear                   X07ValDs
400500130924     C*
400600130924     C                   if        D1TpNR     <> *Blank
400700130924     C*
400800130924     C                   Move      '1'           X07Ric
400900130924     C                   Move      *Off          X07All
401000130924     C                   Movel     'SDGSCF000'   X07TRc
401100130924     C                   Movel     'SCFTPN  '    X07Cam
401200130924     C                   Movel     D1TpNR        X07VAL
401300130924     C                   Callb     'X07VALR'
401400130924     C                   Parm                    X07ValDs
401500130924     C*
401600130924     C     X07ERR        IFEQ      *Off
401700130924     C                   MOVEL     X07DES        D1TpNRD
401800130924     C                   else
401900130924     C                   SetOn                                        5999
402000130924     C                   endif
402100130924     C*
402200130924     C                   endif
402300130924     C*
402400130924     C*
402500130924     C                   Select
402600130924     C*
402700130924     C                   When      D1Quadro    = 'BL'     or
402800130924     C                             D1Quadro    = 'FN'     or
402900131017R136 C                             D1Quadro    = 'TU'     or
403000130924     C                             D1Quadro    = 'SE'
403100130924     C*
403200130924     C                   if        D1TpNR      = *Blank
403300130924     C                   SetOn                                        5999
403400130924     C                   endif
403500130924     C*
403600130924     C                   Other
403700130924     C*
403800130924     C                   Eval      D1TpNR      = *Blank
403900130924     C*
404000130924     C                   endSl
404100130924     C*
404200130924     C                   ENDSR
404300130924     C/EJECT
404400130909     C************************************************************
404500130909     C* Controllo cognome/ragione sociale
404600130909     C************************************************************
404700130909     C     CtrCogRagR    BEGSR
404800130923     C*
404900130923     C     ''''          Scan      D1CogRagR                              22
405000130923     C     *IN22         IFEQ      *On
405100130923     C                   SetOn                                        5199
405200130923     C                   endif
405300130924     C*
405400130924     C                   Select
405500130924     C*
405600130924     C                   When      D1Quadro    = 'BL'     or
405700130924     C                             D1Quadro    = 'FN'     or
405800131017R136 C                             D1Quadro    = 'TU'     or
405900130924     C                             D1Quadro    = 'SE'
406000130924     C*
406100130924     C                   if        D1CogRagR   = *Blank
406200160211R195 X****               SetOn                                        5199
406201160211R195 C                   SetOn                                        8199
406300130924     C                   endif
406400130924     C*
406500130924     C                   Other
406600130924     C*
406700130924     C                   Eval      D1CogRagR   = *Blank
406800130924     C*
406900130924     C                   endSl
407000130909     C*
407100130909     C                   ENDSR
407200130909     C/EJECT
407300130909     C************************************************************
407400130909     C* Controllo nome
407500130909     C************************************************************
407600130909     C     CtrNomeR      BEGSR
407700130923     C*
407800130923     C     ''''          Scan      D1NomeR                                22
407900130923     C     *IN22         IFEQ      *On
408000130923     C                   SetOn                                        5299
408100130923     C                   endif
408200130924     C*
408300130924     C                   Select
408400130924     C*
408500130924     C                   When      (D1Quadro    = 'BL'     or
408600130924     C                              D1Quadro    = 'FN'     or
408700131018R136 X****                          D1Quadro    = 'SE'       ) and
408800131018R136 X****                         D1TpNR      = '1'
408900131018R136 C                              D1Quadro    = 'TU'     or
409000131018R136 C                              D1Quadro    = 'SE'       )
409100130924     C*
409200131018R136 C                   if        D1TpNR      = '1'
409300130924     C                   if        D1NomeR     = *Blank
409400160211R185 X****               SetOn                                        5299
409401160211R185 C                   SetOn                                        8299
409500130924     C                   endif
409600131018R136 C                   else
409700131018 "   C                   if        D1TpNR      = '2'       or
409800131018 "   C                             D1TpNR      = '3'
409900131018 "   C                   Eval      D1NomeR     = *Blank
410000131018 "   C                   endif
410100131018R136 C                   endif
410200130924     C*
410300130924     C                   Other
410400130924     C*
410500130924     C                   Eval      D1NomeR     = *Blank
410600130924     C*
410700130924     C                   endSl
410800130909     C*
410900130909     C                   ENDSR
411000130909     C/EJECT
411100130909     C************************************************************
411200130909     C* Controllo data di nascita
411300130909     C************************************************************
411400130909     C     CtrDtNasR     BEGSR
411500130923     C*
411600130923     C                   Clear                   WDtNasR
411700130923     C*
411800130923     C                   if        D1DtNasR   <> 0
411900130923     C                   Select
412000130923     C* formato mm/gg/aaaa
412100130923     C                   When      $DatFmt     = '*MDY'
412200130923     C     *USA          TEST(D)                 D1DtNasR               53
412300130923     C* formato aa/mm/gg
412400130923     C                   When      $DatFmt     = '*YMD'
412500130923     C     *ISO          TEST(D)                 D1DtNasR               53
412600130923     C* formato gg/mm/aaaa
412700130923     C                   When      $DatFmt     = '*DMY'
412800130923     C     *EUR          TEST(D)                 D1DtNasR               53
412900130923     C                   endSl
413000130923     C* data corretta
413100130923     C                   if        *In53       = *Off
413200130923     C                   Select
413300130923     C* formato mm/gg/aaaa
413400130923     C                   When      $DatFmt     = '*MDY'
413500130923     C     *USA          Move      D1DtNasR      WDtNasR
413600130923     C* formato aa/mm/gg
413700130923     C                   When      $DatFmt     = '*YMD'
413800130923     C     *ISO          Move      D1DtNasR      WDtNasR
413900130923     C* formato gg/mm/aaaa
414000130923     C                   When      $DatFmt     = '*DMY'
414100130923     C     *EUR          Move      D1DtNasR      WDtNasR
414200130923     C                   endSl
414300130923     C*
414400130923     C                   else
414500130923     C*
414600130923     C                   SetOn                                          99
414700130923     C                   endif
414800130923     C*
414900130923     C                   endif
415000130924     C*
415100130924     C*
415200130924     C                   Select
415300130924     C*
415400130924     C                   When      (D1Quadro    = 'BL'     or
415500130924     C                              D1Quadro    = 'FN'     or
415600131018R136 X****                          D1Quadro    = 'SE'       ) and
415700131018R136 X****                         D1TpNR      = '1'
415800131018R136 C                              D1Quadro    = 'TU'     or
415900131018R136 C                              D1Quadro    = 'SE'       )
416000130924     C*
416100131018R136 C                   if        D1TpNR      = '1'
416200130924     C                   if        D1DtNasR    = 0
416300130924     C                   SetOn                                        5399
416400130924     C                   endif
416500131018R136 C                   else
416600131018 "   C                   if        D1TpNR      = '2'       or
416700131018 "   C                             D1TpNR      = '3'
416800131018 "   C                   Eval      D1DtNasR    = 0
416900131018 "   C                   endif
417000131018R136 C                   endif
417100130924     C*
417200130924     C                   Other
417300130924     C*
417400130924     C                   Eval      D1DtNasR    = 0
417500130924     C*
417600130924     C                   endSl
417700130909     C*
417800130909     C                   ENDSR
417900130909     C/EJECT
418000130909     C************************************************************
418100130909     C* Controllo località di nascita
418200130909     C************************************************************
418300130909     C     CtrLocNasR    BEGSR
418400130909     C*
418500130923     C     ''''          Scan      D1LocNasR                              22
418600130923     C     *IN22         IFEQ      *On
418700130923     C                   SetOn                                        5499
418800130923     C                   endif
418900130924     C*
419000130924     C*
419100130924     C                   Select
419200130924     C*
419300130924     C                   When      (D1Quadro    = 'BL'     or
419400130924     C                              D1Quadro    = 'FN'     or
419500131018R136 X****                          D1Quadro    = 'SE'       ) and
419600131018R136 X****                         D1TpNR      = '1'
419700131018R136 C                              D1Quadro    = 'TU'     or
419800131018R136 C                              D1Quadro    = 'SE'       )
419900130924     C*
420000131018R136 C                   if        D1TpNR      = '1'
420100130924     C                   if        D1LocNasR   = *Blank
420200131211R141 X****               SetOn                                        5499
420300131211R141 C                   SetOn                                        7399
420400130924     C                   endif
420500131018R136 C                   else
420600131018 "   C                   if        D1TpNR      = '2'       or
420700131018 "   C                             D1TpNR      = '3'
420800131018 "   C                   Eval      D1LocNasR   = *Blank
420900131018 "   C                   endif
421000131018R136 C                   endif
421100130924     C*
421200130924     C                   Other
421300130924     C*
421400130924     C                   Eval      D1LocNasR   = *Blank
421500130924     C*
421600130924     C                   endSl
421700130909     C*
421800130909     C                   ENDSR
421900130909     C/EJECT
422000130909     C************************************************************
422100130909     C* Controllo provincia di nascita
422200130909     C************************************************************
422300130909     C     CtrPrvNasR    BEGSR
422400130923     C*
422500130923     C                   Clear                   D1PrvNasRD
422600130923     C*
422700130923     C                   Clear                   ANGC17DS
422800131001     C*
422900131001     C* non siamo nella situazione di provincia estera forzata per
423000131001     C* soggetti esteri che sono persona fisica
423100131001     C                   if        D1PrvNasR   = *Blank   and
423200131001     C                             D1TpNR      = '1'      and
423300131001     C                             D1CodUICR  <> 0        and
423400131001     C                             D1CodUICR  <> $UICSoc
423500131001     C                   Eval      D1PrvNasR   = 'EE'
423600131001     C                   endif
423700131018R136 C*
423800131018 "   C                   Select
423900131018 "   C*
424000131018 "   C                   When      (D1Quadro    = 'BL'     or
424100131018 "   C                              D1Quadro    = 'FN'     or
424200131018 "   C                              D1Quadro    = 'TU'     or
424300131018 "   C                              D1Quadro    = 'SE'       )
424400131018 "   C*
424500131018 "   C                   if        D1TpNR      = '1'
424600131018 "   C                   if        D1PrvNasR   = *Blank
424700131018 "   C                   SetOn                                        5599
424800131018 "   C                   endif
424900131018 "   C                   else
425000131018 "   C                   if        D1TpNR      = '2'       or
425100131018 "   C                             D1TpNR      = '3'
425200131018 "   C                   Eval      D1PrvNasR   = *Blank
425300131018 "   C                   endif
425400131018 "   C                   endif
425500131018 "   C*
425600131018 "   C                   Other
425700131018 "   C*
425800131018 "   C                   Eval      D1PrvNasR   = *Blank
425900131018 "   C*
426000131018R136 C                   endSl
426100130923     C*
426200130923     C                   if        D1PrvNasR  <> *Blank
426300130923     C*
426400130923     C                   RESET                   SDGTABDS
426500130923     C*
426600130923     C                   Eval      XTbRic      = '1'
426700130923     C                   Eval      XTbAzi      = XScSoc
426800130923     C                   Eval      XTbKey      = *Zero
426900130923     C                   Eval      XTbCod      = 'C17'
427000130923     C                   Move      D1PrvNasR     KeyC17
427100130923     C                   Movel     XScSta        KeyC17
427200130923     C                   Move      KeyC17        XTBKey
427300130923     C*
427400130923     C                   CALL      'SDGTABDR'
427500130923     C                   PARM                    SDGTABDS
427600130923     C*
427700130923     C                   if        XTbErr     <> '0'
427800131001     C* non siamo nella situazione di provincia estera forzata per
427900131001     C* soggetti esteri che sono persona fisica
428000131001     C                   if        not
428100131001     C                             (D1PrvNasR   = 'EE'    and
428200131001     C                              D1TpNR      = '1'     and
428300131001     C                              D1CodUICR  <> 0       and
428400131001     C                              D1CodUICR  <> $UICSoc    )
428500130923     C                   SetOn                                        5599
428600131001     C                   endif
428700130923     C*
428800130923     C                   else
428900130923     C                   MoveL     XTbUni        ANGC17DS
429000130923     C                   MoveL     DesC17        D1PrvNasRD
429100130923     C*
429200130923     C                   endif
429300130923     C*
429400130923     C                   endif
429500130924     C*
429600130924     C*
429700131018R136 X****               Select
429800131018 "   X****
429900131018 "   X****               When      (D1Quadro    = 'BL'     or
430000131018 "   X****                          D1Quadro    = 'FN'     or
430100131018 "   X****                          D1Quadro    = 'SE'       ) and
430200131018 "   X****                         D1TpNR      = '1'
430300131018 "   X****
430400131018 "   X****               if        D1PrvNasR   = *Blank
430500131018 "   X****               SetOn                                        5599
430600131018 "   X****               endif
430700131018 "   X****
430800131018 "   X****               Other
430900131018 "   X****
431000131018 "   X****               Eval      D1PrvNasR   = *Blank
431100131018 "   X****
431200131018R136 X****               endSl
431300130909     C*
431400130909     C                   ENDSR
431500130909     C/EJECT
431600130909     C************************************************************
431700130909     C* Controllo Stato
431800130909     C************************************************************
431900130909     C     CtrStatoR     BEGSR
432000130911     C*
432100130911     C                   Clear                   D1StatoRD
432200130923     C*
432300130911     C                   Clear                   AngG12DS
432400130911     C*
432500130923     C                   Clear                   SDGTABDS
432600130911     C*
432700130923     C                   if        D1StatoR   <> *Blank
432800130923     C*
432900130911     C                   Eval      XTbRic      = '1'
433000130911     C                   Eval      XTbAzi      = XScSoc
433100130911     C                   Eval      XTbKey      = *Zero
433200130911     C                   Eval      XTbCod      = 'G12'
433300130911     C                   Move      D1StatoR      XTbKey
433400130911     C*
433500130911     C                   CALL      'SDGTABDR'
433600130911     C                   PARM                    SDGTABDS
433700130911     C*
433800130911     C                   if        XTbErr     <> '0'
433900130930     C* AVVERTIMENTO
434000131001     C                   Eval      *In17       = *On
434100130911     C*
434200130911     C                   else
434300130911     C                   MoveL     XTbUni        AngG12DS
434400130911     C                   MoveL     DesG12        D1StatoRD
434500130923     C*
434600131001     C                   if        D1CodUICR   = 0
434700130923     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
434800130923     C                   Z-Add     0             D1CODUICR
434900130923     C                   else
435000130923     C                   Z-Add     CodUICG12     D1CodUICR
435100130923     C                   endif
435200130930     C                   endif
435300130911     C*
435400130911     C                   endif
435500130930     C*
435600131112R141 X****               EXSR      CtrCodUICR
435700130909     C*
435800130923     C                   endif
435900130923     C*
436000130909     C                   ENDSR
436100130909     C/EJECT
436200130930     C************************************************************
436300130930     C* Controllo codice UIC
436400130930     C************************************************************
436500130930     C     CtrCodUICR    BEGSR
436600130930     C*
436700130930     C                   Select
436800130930     C*
436900130930     C                   When      D1Quadro    = 'BL'     or
437000130930     C                             D1Quadro    = 'FN'     or
437100131017R136 C                             D1Quadro    = 'TU'     or
437200130930     C                             D1Quadro    = 'SE'
437300130930     C*
437400130930     C                   if        D1CodUICR   = 0
437500130930     C                   SetOn                                        5699
437600130930     C                   endif
437700130930     C*
437800130930     C                   Other
437900130930     C*
438000130930     C                   Eval      D1CodUICR   = 0
438100130930     C*
438200130930     C                   endSl
438300130930     C*
438400130930     C                   ENDSR
438500130930     C/EJECT
438600130909     C************************************************************
438700130909     C* Controllo località di residenza
438800130909     C************************************************************
438900130909     C     CtrLocaliR    BEGSR
439000130923     C*
439100130923     C     ''''          Scan      D1LocaliR                              22
439200130923     C     *IN22         IFEQ      *On
439300130923     C                   SetOn                                        5799
439400130923     C                   endif
439500130924     C*
439600130924     C                   Select
439700130924     C*
439800130924     C                   When      (D1Quadro    = 'BL'     or
439900130924     C                              D1Quadro    = 'FN'     or
440000130924     C                              D1Quadro    = 'SE'       ) and
440100130924     C                             (D1TpNR      = '2'      or
440200130924     C                              D1TpNR      = '3'        )
440300130924     C*
440400130924     C                   if        D1LocaliR   = *Blank
440500160211R195 X****               SetOn                                        5799
440501160211R195 C                   SetOn                                        8799
440600130924     C                   endif
440700131017R136 C*
440800131017 "   C                   When      D1Quadro     = 'TU'      and
440900131017 "   C                             D1TpNR       = '1'
441000131017 "   C*
441100160211R136 C                   if        D1LocaliR   = *Blank
441200160211R195 X*ex R136           SetOn                                        5799
441201160211R195 C                   SetOn                                        8799
441300131017R136 C                   endif
441400130924     C*
441500130924     C                   Other
441600130924     C*
441700130924     C                   Eval      D1LocaliR   = *Blank
441800130924     C*
441900130924     C                   endSl
442000130909     C*
442100130909     C                   ENDSR
442200130909     C/EJECT
442300130909     C************************************************************
442400130909     C* Controllo indirizzo estero
442500130909     C************************************************************
442600130909     C     CtrIndirER    BEGSR
442700130909     C*
442800130923     C     ''''          Scan      D1IndirER                              22
442900130923     C     *IN22         IFEQ      *On
443000130923     C                   SetOn                                        5899
443100130923     C                   endif
443200130924     C*
443300130924     C                   Select
443400130924     C*
443500130924     C                   When      (D1Quadro    = 'BL'     or
443600130924     C                              D1Quadro    = 'FN'     or
443700130924     C                              D1Quadro    = 'SE'       ) and
443800130924     C                             (D1TpNR      = '2'      or
443900130924     C                              D1TpNR      = '3'        )
444000130924     C*
444100130924     C                   if        D1IndireR   = *Blank
444200160211R195 X****               SetOn                                        5899
444201160211R195 C                   SetOn                                        8899
444300130924     C                   endif
444400131017R136 C*
444500131017 "   C                   When      D1Quadro     = 'TU'      and
444600131017 "   C                             D1TpNR       = '1'
444700131017 "   C*
444800160211R136 C                   if        D1IndireR   = *Blank
444900160211R195 X*ex R136           SetOn                                        5899
444901160211R195 C                   SetOn                                        8899
445000131017R136 C                   endif
445100130924     C*
445200130924     C                   Other
445300130924     C*
445400130924     C                   Eval      D1IndireR   = *Blank
445500130924     C*
445600130924     C                   endSl
445700130909     C*
445800130909     C                   ENDSR
445900130909     C/EJECT
446000130909     C************************************************************
446100130909     C* Controllo partita IVA
446200130909     C************************************************************
446300130909     C     CtrPIVAR      BEGSR
446400130909     C*
446500130924     C                   Select
446600130924     C*
446700130924     C                   When      D1Quadro    = 'FA'     or
446800130924     C                             D1Quadro    = 'FE'     or
446900130924     C                             D1Quadro    = 'NE'
447000130924     C*
447100130924     C                   if        D1PIVAR     = *Blank   and
447200130924     C                             D1CdFiscR   = *Blank
447300130924     C                   SetOn                                        6099
447400130924     C                   endif
447500130924     C*
447600130925     C*
447700130925     C                   When      D1Quadro    = 'BL'     or
447800130925     C                             D1Quadro    = 'SE'
447900130924     C*
448000130924     C                   When      D1Quadro    = 'FR'     or
448100130924     C                             D1Quadro    = 'NR'
448200130924     C*
448300130924     C                   if        D1PIVAR     = *Blank
448400130924     C                   SetOn                                        6099
448500130924     C                   endif
448600130924     C                   Eval      D1CdFiscR   = *Blank
448700130924     C*
448800130924     C*
448900130924     C                   Other
449000130924     C*
449100130924     C                   Eval      D1PIVAR     = *Blank
449200130924     C*
449300130924     C                   endSl
449400130909     C*
449500130909     C                   ENDSR
449600130909     C/EJECT
449700130909     C************************************************************
449800130909     C* Controllo codice fiscale
449900130909     C************************************************************
450000130909     C     CtrCdFiscR    BEGSR
450100130909     C*
450200130924     C                   Select
450300130924     C*
450400130924     C                   When      D1Quadro    = 'FA'     or
450500130924     C                             D1Quadro    = 'FE'     or
450600130924     C                             D1Quadro    = 'NE'
450700130924     C*
450800130924     C                   if        D1PIVAR     = *Blank   and
450900130924     C                             D1CdFiscR   = *Blank
451000130924     C                   SetOn                                        6199
451100130924     C                   endif
451200130925     C*
451300130925     C*
451400130925     C                   When      D1Quadro    = 'BL'     or
451500130925     C                             D1Quadro    = 'SE'
451600130924     C*
451700130924     C*
451800130924     C                   When      D1Quadro    = 'SA'     or
451900130924     C                             D1Quadro    = 'DF'
452000130924     C*
452100130924     C                   if        D1CdFiscR   = *Blank
452200130924     C                   SetOn                                        6199
452300130924     C                   endif
452400130924     C                   Eval      D1PIVAR     = *Blank
452500130924     C*
452600130924     C*
452700130924     C                   Other
452800130924     C*
452900130924     C                   Eval      D1CdFiscR   = *Blank
453000130924     C*
453100130924     C                   endSl
453200130909     C*
453300130909     C                   ENDSR
453400130909     C/EJECT
453500130925     C************************************************************
453600130925     C* Controllo tipo operazione
453700130925     C************************************************************
453800130925     C     CtrTpOpeR     BEGSR
453900130925     C*
454000130925     C                   Clear                   D1TpOpeRD
454100130925     C                   Clear                   X07ValDs
454200130925     C*
454300160208R195 X****               if        D1TpOpeR   <> *Blank
454400130925     C*
454500130925     C                   Move      '1'           X07Ric
454600130925     C                   Move      *On           X07All
454700130925     C                   MoveL     'SDGSCF000'   X07TRc
454800160208R195 X****               MoveL     'SCFTPOPE  '  X07Cam
454801160208R195 C                   MoveL     'SCFTPOPER '  X07Cam
454900130925     C                   MoveL     D1TpOpeR      X07VAL
455000130925     C*
455100130925     C                   CALLB     'X07VALR'
455200130925     C                   PARM                    X07ValDs
455300130925     C*
455400130925     C     X07ERR        IFEQ      *Off
455500130925     C                   MoveL     X07DES        D1TpOpeRD
455600130925     C                   else
455700130925     C                   SetOn                                        7299
455800130925     C                   endif
455900130925     C*
456000160208R195 X****               endif
456100130925     C*
456200130925     C                   ENDSR
456300130925     C/EJECT
456400130909     C************************************************************
456500130909     C* Controllo natura dell'operazione
456600130909     C************************************************************
456700130909     C     CtrNatOpeR    BEGSR
456800130911     C*
456900130911     C                   Clear                   D1NatOpeRD
457000130911     C                   Clear                   X07ValDs
457100130923     C*
457200160208R195 X****               if        D1NatOpeR  <> *Blank
457300130911     C*
457400130911     C                   Move      '1'           X07Ric
457500130911     C                   Move      *On           X07All
457600130911     C                   MoveL     'SDGSCF000'   X07TRc
457700160208R195 X****               MoveL     'SCFNATOPE '  X07Cam
457701160208R195 C                   MoveL     'SCFNATOPER'  X07Cam
457800130911     C                   MoveL     D1NatOpeR     X07VAL
457900130911     C*
458000130911     C                   CALLB     'X07VALR'
458100130911     C                   PARM                    X07ValDs
458200130911     C*
458300130911     C     X07ERR        IFEQ      *Off
458400130911     C                   MoveL     X07DES        D1NatOpeRD
458500130911     C                   else
458600130923     C                   SetOn                                        6299
458700130923     C                   endif
458800130923     C*
458900160208R195 X****               endif
459000130909     C*
459100130909     C                   ENDSR
459200130909     C/EJECT
459300130909     C************************************************************
459400130909     C* Controllo leasing
459500130909     C************************************************************
459600130909     C     CtrLeasinR    BEGSR
459700131015R135 C*
459800131015R135 C                   Clear                   D1LeasinRD
459900130923     C*
460000130924     C                   Select
460100130924     C*
460200131015R135 X****               When      D1Quadro    = 'FA'     or
460300131015 "   X****                         D1Quadro    = 'SA'     or
460400131015 "   X****                         D1Quadro    = 'DF'
460500131015 "   X****
460600131015 "   X****               if        D1LeasinR  <> '1'      and
460700131015 "   X****                         D1LeasinR  <> '0'
460800131015 "   X****               SetOn                                        6399
460900131015 "   X****               else
461000131015 "   X****               EXSR      RepLeasinRD
461100131015R135 X****               endif
461200130926     C*
461300130926     C*
461400130926     C                   When      D1Quadro    = 'FE'     or
461500131015R135 C                             D1Quadro    = 'FA'     or
461600131015 "   C                             D1Quadro    = 'SA'     or
461700131015R135 C                             D1Quadro    = 'DF'     or
461800130926     C                             D1Quadro    = 'FN'
461900130926     C*
462000131111R140 X****               if        D1LeasinR  <> '0'      and
462100131111R140 C                   if        D1LeasinR  <> *Blank   and
462200130926     C                             D1LeasinR  <> 'A'      and
462300130926     C                             D1LeasinR  <> 'B'      and
462400130926     C                             D1LeasinR  <> 'C'      and
462500130926     C                             D1LeasinR  <> 'D'      and
462600130926     C                             D1LeasinR  <> 'E'
462700130926     C                   SetOn                                        6399
462800130926     C                   else
462900130926     C                   EXSR      RepLeasinRD
463000130926     C                   endif
463100130924     C*
463200130924     C                   other
463300130924     C*
463400130926     C                   Eval      D1LeasinR   = *Blank
463500130926     C                   EXSR      RepLeasinRD
463600130926     C*
463700130926     C                   endSl
463800130926     C*
463900130926     C                   ENDSR
464000130926     C/EJECT
464100130926     C************************************************************
464200130926     C* Reperimento descrizione del leasing
464300130926     C************************************************************
464400130926     C     RepLeasinRD   BEGSR
464500130926     C*
464600130926     C                   Clear                   D1LeasinRD
464700130926     C                   Clear                   X07ValDs
464800130926     C*
464900130926     C                   Move      '1'           X07Ric
465000130926     C                   Move      *On           X07All
465100130926     C                   MoveL     'SDGSCF000'   X07TRc
465200130926     C                   MoveL     'SCFLEASINR'  X07Cam
465300130926     C                   MoveL     D1LeasinR     X07VAL
465400130926     C*
465500130926     C                   CALLB     'X07VALR'
465600130926     C                   PARM                    X07ValDs
465700130926     C*
465800130926     C     X07ERR        IFEQ      *Off
465900130926     C                   MoveL     X07DES        D1LeasinRD
466000130926     C                   endif
466100130909     C*
466200130909     C                   ENDSR
466300130909     C/EJECT
466400130911     C************************************************************
466500130911     C* Controllo registrazione
466600130911     C************************************************************
466700130911     C     CtrRegR       BEGSR
466800130911     C*
466900130911     C                   Eval      WDtRegR     = *Loval
467000130911     C*
467100130911     C                   if        D1DtRegR   <> 0
467200130911     C     *JobRun       Test(d)                 D1DtRegR               21
467300130911     C*
467400130911     C                   if        *In21       = *Off
467500130911     C     *JobRun       Move      D1DtRegR      WDtRegR
467600130911     C*
467700130911     C                   else
467800130923     C                   SetOn                                        6499
467900130923     C                   endif
468000130911     C*
468100130911     C                   endif
468200130924     C*
468300130924     C                   if        D1DtRegR    = 0          and
468400130924     C                             FormatoA08  = 'D'
468401151217R195 C                             and TpComG76    <> 'B'
468500130924     C                   SetOn                                        6499
468600130924     C                   endif
468700131017R136 C*
468800131017 "   C                   if        D1DtRegR    = 0          and
468900131017 "   C                             D1Quadro    = 'TU'
469000131017 "   C                   SetOn                                        6499
469100131018 "   C                   endif
469200131018 "   C*
469300131018 "   C                   if        D1DtRegR    = 0          and
469400131018 "   C                             D1TpDoc     = 'R'        and
469500151217R136 C                             DocRieA08   = 'D'
469501151217R195 C                             and TpComG76    <> 'B'
469600151217R136 C                   SetOn                                        6499
469700131018R136 C                   endif
469800130911     C*
469900130924     C*
470000130924     C                   if        D1NrRegR    = 0          and
470100130924     C                             FormatoA08  = 'D'
470101151217R195 C                             and TpComG76    <> 'B'
470200130924     C                   SetOn                                        6599
470300130924     C                   endif
470400131017R136 C*
470500131017 "   C                   if        D1NrRegR    = 0          and
470600131017 "   C                             D1Quadro    = 'TU'
470700131017 "   C                   SetOn                                        6599
470800131018 "   C                   endif
470900131018 "   C*
471000131018 "   C                   if        D1NrRegR    = 0          and
471100131018 "   C                             D1TpDoc     = 'R'        and
471200151217R136 C                             DocRieA08   = 'D'
471201151217R195 C                             and TpComG76    <> 'B'
471300151217R136 C                   SetOn                                        6599
471400131018R136 C                   endif
471500130911     C*
471600130911     C                   if        D1SerRegR  <> *Blank
471700130923     C*
471800130911     C                   Clear                   SDGTABDS
471900130911     C                   Move      '1'           XTbRic
472000130911     C                   Move      XScSoc        XTbAzi
472100130911     C                   Move      *Zero         XTbKey
472200130911     C                   Move      'G13'         XTbCod
472300130911     C                   Move      D1SerRegR     XTbKey
472400130911     C                   CALL      'SDGTABDR'
472500130911     C                   PARM                    SDGTABDS
472600130911     C     XTbErr        IFEq      '1'
472700130923     C                   SetOn                                        6699
472800130923     C                   endif
472900130923     C*
473000130911     C                   endif
473100130911     C*
473200130911     C                   ENDSR
473300130911     C/EJECT
473400130924     C************************************************************
473500130924     C* Controllo numero operazioni aggregate
473600130924     C************************************************************
473700130924     C     CtrNrOpe      BEGSR
473800130924     C*
473900130925     C* verifico quanti record esistono nel quadro
474000130925     C*************      EXSR      ContaRighe
474100130925     C* tolto conteggio numero righe per quadro, tutto è lasciato
474200130925     C* in mano all'utente
474300130925     C*
474400130925     C                   Select
474500130925     C*
474600130925     C                   When      NrRcdG78   > 0    and
474700130925     C                             NrOpeG78   = *On
474800130925     C* se già presente una riga con numero operazione,
474900130925     C* NON posso scrivere altre righe nello stesso quadro
475000130925     C                   SetOn                                        6899
475100130925     C*
475200130925     C*
475300130925     C                   When      NrRcdG78   > 0       and
475400130925     C                             D1NrOpe    > 0
475500130925     C* se già presente una riga e a video è impostato il numero
475600130925     C* operazione
475700130925     C* NON posso scrivere il record
475800130925     C                   SetOn                                        6999
475900130925     C*
476000130925     C                   Other
476100130925     C*
476200130925     C* posso inserire il record, con o senza il numero operazione
476300130925     C*
476400130925     C                   Select
476500130924     C*
476600130925     C* NON posso valorizzare alcun numero operazione
476700130925     C                   When      D1Quadro    = 'BL'      and
476800130925     C                             D1NrOpe     > 0
476900130925     C                   SetOn                                        7199
477000130924     C*
477100130924     C                   endSl
477200130925     C*
477300130925     C                   endSl
477400130924     C*
477500130924     C                   ENDSR
477600130924     C/EJECT
477700000000     C************************************************************
477800130925     C* Conta righe nel quadro
477900000000     C************************************************************
478000130925     C     ContaRighe    BEGSR
478100000000     C*
478200130925     C                   Clear                   SDGSG78DS
478300130925     C*
478400130925     C                   Eval      SOCIETAG78  = XScSoc
478500140401R133 C****               Eval      TPCOMG78    = 'E'
478600140401R133 C                   Eval      TPCOMG78    = TPCOMG76
478700130925     C                   Eval      ANNOG78     = D1Anno
478800140401R133 C****               Eval      MESEG78     = 0
478900140401R133 C                   Eval      MESEG78     = MESEG76
479000130925     C                   Eval      SEQQUAG78   = H1SEQQUA
479100130925     C                   Eval      QUADROG78   = D1QUADRO
479200130925     C                   Eval      COGRAGRG78  = D1COGRAGR
479300130925     C                   Eval      NOMERG78    = D1NOMER
479400130925     C                   Eval      DTNASRG78   = WDTNASR
479500130925     C                   Eval      LOCNASRG78  = D1LOCNASR
479600130925     C                   Eval      PRVNASRG78  = D1PRVNASR
479700130925     C                   Eval      STATORG78   = D1STATOR
479800130925     C                   Eval      CODUICRG78  = D1CODUICR
479900130925     C                   Eval      LOCALIRG78  = D1LOCALIR
480000130925     C                   Eval      INDIRERG78  = D1INDIRER
480100130925     C                   Eval      TPNRG78     = D1TPNR
480200130925     C                   Eval      PIVARG78    = D1PIVAR
480300130925     C                   Eval      CDFISCRG78  = D1CDFISCR
480400130925     C                   Eval      TpOpeRG78   = D1TpOpeR
480500130925     C                   Eval      NATOPERG78  = D1NatOpeR
480600130926     C                   Eval      NATOPERG78  = D1NatOpeR
480700130926     C                   Eval      LeasinRG78  = D1LeasinR
480800130925     C                   Eval      DTREGRG78   = *Loval
480900130925     C                   Eval      NRREGRG78   = 0
481000130925     C                   Eval      SERREGRG78  = *Blank
481100130925     C                   Eval      SYSG78      = $Sys
481200130925     C                   Eval      NRASREGG78  = $NrAsReg
481300130925     C                   Eval      NRRIGAMG78  = $NrRigaM
481400130925     C                   Eval      NRRIGAG78   = $NrRiga
481500130925     C*
481600130925     C                   CALL      'SDGSG78R'
481700130925     C                   PARM                    KPJBA
481800130925     C                   PARM                    SDGSG78DS
481900130925     C*
482000130925     C                   ENDSR
482100130925     C/EJECT
482200130925     C************************************************************
482300130925     C* AGGIORNAMENTO ANAGRAFICA
482400130925     C************************************************************
482500130925     C     AGGANA        BEGSR
482600130925     C*
482700000000     C                   SELECT
482800130909    >C     OPZG76        WHENEQ    '01'
482900130909    >C     OPZG76        OREQ      '03'
483000130909     C                   EXSR      RIEANA
483100130909    >C                   WRITE     SDGSCF000                            22
483200000000     C* se rcd non scrivibile attivo errore
483300000000     C     *IN22         IFEQ      *ON
483400130909     C                   SetOn                                        9599
483500000000     C                   ENDIF
483600130909    >C     OPZG76        WHENEQ    '02'
483700130909     C                   EXSR      RIEANA
483800130909    >C                   UPDATE    SDGSCF000                            22
483900130909    >C     OPZG76        WHENEQ    '04'
484000130909    >C                   DELETE    SDGSCF000                            21
484100000000     C                   ENDSL
484200000000     C*
484300000000     C* richiama pgm User Exit
484400000000     C     USRERR        IFEQ      *BLANK
484500000000     C* passa opzione ricevuta a user exit
484600130909    >C                   MoveL     OPZG76        XUKOPZ
484700000000???? C* imposta chiave univoca  per user exit
484800000000???? C* la chiave univoca non esiste perchè sto lavorando
484900000000???? C* con raggruppamento di dati
485000000000     C                   MOVE      XSCSOC        XUKSOC
485100000000     C                   MOVE      XSCDUN        XUKDUN
485200130909     C                   MoveL     DSPGM         XUKPGM
485300130909    >C                   MoveL     *Blank        XUKFL1
485400130909    >C                   MoveL     *Blank        XUKFL2
485500000000    >C* ho fino a 6 campi chiave passabili
485600130909     C                   MoveL     XUKDS         KPJBU
485700000000     C     DSPGM         CAT       'U':0         USREXT           10
485800130909     C                   CALL      USREXT                               21
485900130909     C                   PARM                    KPJBA
486000000000     C     *IN21         IFEQ      *ON
486100000000     C                   MOVE      *ON           USRERR            1
486200000000     C                   ENDIF
486300000000     C                   ENDIF
486400000000     C*
486500000000     C                   ENDSR
486600000000     C/EJECT
486700000000     C************************************************************
486800000000     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
486900000000     C************************************************************
487000000000     C     RIEANA        BEGSR
487100130923     C*
487200130923     C                   Eval      SCFOrigine  = 'M'
487300130923     C                   Eval      SCFPartIva  = PartIvaG76
487400130923     C                   Eval      SCFCdFisc   = CdFiscG76
487500130923     C                   Eval      SCFCogRag   = CogRagG76
487600130923     C                   Eval      SCFNome     = NomeG76
487700130923     C*
487800130923     C                   Eval      SCFDtNasc   = DtNascG76
487900130923     C                   Eval      SCFLocNasc  = LocNascG76
488000130923     C                   Eval      SCFPrvNasc  = PrvNascG76
488100130923     C                   Eval      SCFStato    = StatoG76
488200130923     C                   Eval      SCFCodUIC   = CodUICG76
488300130923     C                   Eval      SCFLocalit  = LocalitG76
488400130923     C                   Eval      SCFIndirE   = IndirEG76
488500000000     C*
488600130909     C                   if        OPZG76      = '01'   or
488700130909     C                             OPZG76      = '03'
488800000000     C*
488900130909     C                   Eval      SCFSYS      = $Sys
489000130909     C                   Eval      SCFNRASREG  = $NrAsReg
489100130909     C                   Eval      SCFNRRIGAM  = $NrRigaM
489200130909     C                   Eval      SCFNRRIGA   = $NrRiga
489300110902     C*
489400130909     C                   Eval      SCFIMPSOGL  = 0
489500130909     C                   Eval      SCFIVASOGL  = 0
489600000000     C*
489700000000     C* imposto la data e l'utente di immissione
489800130909     C                   Eval      SCFDtImm    = UDateIso
489900130909     C                   Eval      SCFUteImm   = XScCUt
490000000000     C*
490100140401R133 C****               Eval      SCFTpCom    = 'E'
490200140401R133 C                   Eval      SCFTpCom    = TpComG76
490300130909     C                   Eval      SCFAnno     = AnnoG76
490400140401R133 C****               Eval      SCFMese     = 0
490500140401R133 C                   Eval      SCFMese     = MeseG76
490600000000     C                   endif
490700000000     C*
490800000000     C* imposto la data e l'utente di ultima modifica
490900130909     C                   Eval      SCFDtUlMo   = UDateIso
491000130909     C                   Eval      SCFUteUlMo  = XScCUt
491100000000     C*
491200130909     C                   MoveL     XscSoc        SCFSocieta
491300000000     C*
491400000000     C* imposto i campi del file da ricevuti dal chiamante
491500130911     C                   Eval      SCFCF       = H1CF
491600130923     C*
491700130923     C                   Eval      SCFTpDoc    = D1TpDoc
491800130923     C                   Eval      SCFVarNota  = D1VarNota
491900130923     C*
492000130923     C                   if        D1Margine   = XScOn
492100130923     C                   Eval      SCFMargine  = *On
492200130923     C                   else
492300130923     C                   Eval      SCFMargine  = *Off
492400130923     C                   endif
492500130923     C                   if        D1Reverse   = XScOn
492600130923     C                   Eval      SCFReverse  = *On
492700130923     C                   else
492800130923     C                   Eval      SCFReverse  = *Off
492900130923     C                   endif
493000130923     C                   if        D1Autofat   = XScOn
493100130923     C                   Eval      SCFAutofat  = *On
493200130923     C                   else
493300130923     C                   Eval      SCFAutofat  = *Off
493400130923     C                   endif
493500130923     C*
493600130926     C                   Eval      SCFLeasing  = D1Leasing
493700130926     C                   Eval      SCFNatOpe   = D1NatOpe
493800130923     C                   Eval      SCFTpOpe    = D1TpOpe
493900130923     C                   Eval      SCFTpImp    = D1TpImp
494000130923     C*
494100130923     C                   Eval      SCFImpOpe   = D1ImpOpe
494200130923     C                   Eval      SCFIvaOpe   = D1IvaOpe
494300130923     C*
494400130923     C                   Eval      SCFImpOpeO  = D1ImpOpeO
494500130923     C                   Eval      SCFIvaOpeO  = D1IvaOpeO
494600130923     C                   Eval      SCFImpOpe   = D1ImpOpe
494700130923     C                   Eval      SCFIvaOpe   = D1IvaOpe
494800130923     C                   if        D1Ann       = XScOn
494900130923     C                   Eval      SCFAnn      = *On
495000130923     C                   else
495100130923     C                   Eval      SCFAnn      = *Blank
495200130923     C                   endif
495300130923     C                   Eval      SCFTpAnn    = D1TpAnn
495400130923     C*
495500130923     C* D2
495600130923     C                   Eval      SCFTpN      = D1TpN
495700130911     C                   Eval      SCFSogg     = D1Sogg
495800000000     C*
495900000000     C* imposto i campi del file da quelli video
496000130909     C                   Eval      SCFKcc      = D1Kcc
496100130909     C                   Eval      SCFKsc      = D1Ksc
496200130909     C                   Eval      SCFCausale  = D1Causale
496300130909     C                   Eval      SCFAliq     = D1Aliq
496400130909     C                   Eval      SCFRifIVA   = D1RifIVA
496500130909     C*
496600130909     C                   Eval      SCFTpReg    = D1TpReg
496700130909     C                   Eval      SCFLibro    = D1Libro
496800130909     C*
496900130909     C                   Eval      SCFDtDoc    = WDtDoc
497000130909     C                   Eval      SCFNrDoc    = D1NrDoc
497100130909     C                   Eval      SCFSerDoc   = D1SerDoc
497200000000     C*
497300130909     C                   Eval      SCFDtReg    = WDtReg
497400130909     C                   Eval      SCFNrReg    = D1NrReg
497500130909     C                   Eval      SCFSerReg   = D1SerReg
497600000000     C*
497700130909     C                   Eval      SCFDtPar    = WDtPar
497800130909     C                   Eval      SCFNrPar    = D1NrPar
497900130909     C                   Eval      SCFSerPar   = D1SerPar
498000110902     C                   if        D1NoIva     = XScOn
498100130909     C                   Eval      SCFNoIva    = *On
498200110902     C                   else
498300130909     C                   Eval      SCFNoIva    = *Off
498400110902     C                   endif
498500000000     C*
498600000000     C* D3
498700130911     C*
498800130911     C                   Eval      SCFFormato  = FORMATOA08
498900130909     C                   Eval      SCFQuadro   = D1Quadro
499000130925     C                   Eval      SCFSeqQua   = H1SeqQua
499100130909     C                   Eval      SCFCogRagR  = D1CogRagR
499200130909     C                   Eval      SCFNomeR    = D1NomeR
499300130909     C                   Eval      SCFDtNasR   = WDtNasR
499400130909     C                   Eval      SCFLocNasR  = D1LocNasR
499500130909     C                   Eval      SCFPrvNasR  = D1PrvNasR
499600130909     C                   Eval      SCFStatoR   = D1StatoR
499700130930     C                   Eval      SCFCodUICR  = D1CodUICR
499800130909     C                   Eval      SCFLocaliR  = D1LocaliR
499900130909     C                   Eval      SCFIndirER  = D1IndirER
500000130909     C                   Eval      SCFTpNR     = D1TpNR
500100130909     C                   Eval      SCFPIVAR    = D1PIVAR
500200130909     C                   Eval      SCFCdFiscR  = D1CdFiscR
500300130925     C*
500400130925     C                   Eval      SCFTpOpeR   = D1TpOpeR
500500130925     C                   Eval      SCFNatOpeR  = D1NatOpeR
500600130926     C                   Eval      SCFLeasinR  = D1LeasinR
500700130911     C*
500800131017R136 X****               if        FormatoA08  = 'A'
500900131017 "   X****               Eval      SCFDtRegR   = *Loval
501000131017 "   X****               Eval      SCFNrRegR   = 0
501100131018 "   X****               Eval      SCFSerRegR  = *Blank
501200131018 "   X****               Eval      SCFNrOpe    = D1NrOpe
501300131018 "   X****               else
501400131018 "   X****               Eval      SCFDtRegR   = WDtReg
501500131018 "   X****               Eval      SCFNrRegR   = D1NrReg
501600131018 "   X****               Eval      SCFSerRegR  = D1SerReg
501700131018 "   X****               Eval      SCFNrOpe    = 0
501800131017R136 X****               endif
501900131018R136 C*
502000131018 "   C                   Eval      SCFDtRegR   = WDtRegR
502100131018 "   C                   Eval      SCFNrRegR   = D1NrRegR
502200131018 "   C                   Eval      SCFSerRegR  = D1SerRegR
502300131018R136 C                   Eval      SCFNrOpe    = D1NrOpe
502400000000     C*
502500000000     C                   ENDSR
502600000000     C/EJECT
502700000000     C************************************************************
502800000000     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
502900000000     C************************************************************
503000000000     C     GESAGG        BEGSR
503100000000     C*
503200000000     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
503300130909    >C     CMTG76        IFEQ      '0'
503400000000     C********           COMMIT
503500000000     C                   ENDIF
503600000000     C*
503700000000     C* segnalo al pgm chiamante l'avvenuta conferma
503800130909    >C                   MOVE      *ON           OPRG76
503900000000     C* segnalo al pgm chiamante l'aver premuto l'F6
504000130909    >C                   MOVE      '0'           RETG76
504100000000     C*
504200000000     C                   SELECT
504300000000     C*
504400000000     C* nel caso di immissione
504500130909    >C     OPZG76        WHENEQ    '01'
504600000000     C* ripristino la videata iniziale
504700000000     C                   MOVE      *ON           $INZD1
504800000000     C                   MOVE      'D1'          $GEST
504900000000     C*
505000000000     C* nel caso di copia
505100130909    >C     OPZG76        WHENEQ    '03'
505200000000     C* ritorno al pgm chiamante
505300000000     C                   MOVE      *ON           $FINE
505400000000     C*
505500000000     C* nel caso di modifica
505600130909    >C     OPZG76        WHENEQ    '02'
505700000000     C* ripasso i campi che devono essere reimpostati sul sfl
505800130909     C                   Eval      KccG76      = D1Kcc
505900130909     C                   Eval      KscG76      = D1Ksc
506000130909     C                   Eval      DtDocG76    = WDtDoc
506100130909     C                   Eval      NrDocG76    = D1NrDoc
506200130909     C                   Eval      SerDocG76   = D1SerDoc
506300130909     C                   Eval      DtRegG76    = WDtReg
506400130909     C                   Eval      NrRegG76    = D1NrReg
506500130909     C                   Eval      SerRegG76   = D1SerReg
506600130909     C                   Eval      DtParG76    = WDtPar
506700130909     C                   Eval      OrigineG76  = D1Origine
506800130909     C                   if        D1Ann       = XScOn
506900130909     C                   Eval      AnnG76      = *On
507000000000     C                   else
507100130909     C                   Eval      AnnG76      = *Blank
507200000000     C                   endif
507300130918     C                   Eval      ImpOpeG76   = D1ImpOpe
507400130918     C                   Eval      IVAOpeG76   = D1IVAOpe
507500130918     C                   Eval      CausaleG76  = D1Causale
507600130909     C                   Eval      AliqG76     = D1Aliq
507700130909     C                   Eval      RifIVAG76   = D1RifIVA
507800130909     C                   Eval      TpDocG76    = D1TpDoc
507900130909     C                   Eval      TPNRG76     = D1TpNR
508000130909     C                   Eval      CogRagRG76  = D1CogRagR
508100130909     C                   Eval      NomeRG76    = D1NomeR
508200130909     C                   Eval      DtNasRG76   = WDtNasR
508300130909     C                   Eval      LocNasRG76  = D1LocNasR
508400130909     C                   Eval      PrvNasRG76  = D1PrvNasR
508500130909     C                   Eval      StatoRG76   = D1StatoR
508600130909     C                   Eval      CodUICRG76  = D1CodUICR
508700130909     C                   Eval      LocaliRG76  = D1LocaliR
508800130909     C                   Eval      IndirERG76  = D1IndirER
508900130909     C                   Eval      PIVARG76    = D1PIVAR
509000130909     C                   Eval      CdFiscRG76  = D1CdFiscR
509100130925     C                   Eval      TpOpeRG76   = D1TpOpeR
509200130925     C                   Eval      NatOpeRG76  = D1NatOpeR
509300130909     C                   Eval      LeasinRG76  = D1LeasinR
509400130909     C                   Eval      DtRegRG76   = WDtRegR
509500130909     C                   Eval      NrRegRG76   = D1NrRegR
509600130909     C                   Eval      SerRegRG76  = D1SerRegR
509700000000     C* e ritorno al pgm chiamante
509800000000     C                   MOVE      *ON           $FINE
509900000000     C*
510000000000     C* per tutte le altre opzioni
510100000000     C                   OTHER
510200000000     C* ritorno al pgm chiamante
510300000000     C                   MOVE      *ON           $FINE
510400000000     C                   ENDSL
510500000000     C*
510600000000     C                   ENDSR
510700000000     C/EJECT
510800000000     C************************************************************
510900000000     C* CONTROLLO ABILITAZIONE TASTI
511000000000     C************************************************************
511100000000     C     CTRKEY        BEGSR
511200000000     C*
511300000000     C                   SELECT
511400000000     C     $TASTO        WHENEQ    F01
511500000000     C                   Z-ADD     01            PosFun
511600000000     C     $FC(1)        IFEQ      '0'
511700130909     C                   SetOn                                        99
511800000000     C                   ENDIF
511900000000     C     $TASTO        WHENEQ    F02
512000000000     C                   Z-ADD     02            PosFun
512100000000     C     $FC(2)        IFEQ      '0'
512200130909     C                   SetOn                                        99
512300000000     C                   ENDIF
512400000000     C     $TASTO        WHENEQ    F03
512500000000     C                   Z-ADD     03            PosFun
512600000000     C     $FC(3)        IFEQ      '0'
512700130909     C                   SetOn                                        99
512800000000     C                   ENDIF
512900000000     C     $TASTO        WHENEQ    F04
513000000000     C                   Z-ADD     04            PosFun
513100000000     C     $FC(4)        IFEQ      '0'
513200130909     C                   SetOn                                        99
513300000000     C                   ENDIF
513400000000     C     $TASTO        WHENEQ    F05
513500000000     C                   Z-ADD     05            PosFun
513600000000     C     $FC(5)        IFEQ      '0'
513700130909     C                   SetOn                                        99
513800000000     C                   ENDIF
513900000000     C     $TASTO        WHENEQ    F06
514000000000     C                   Z-ADD     06            PosFun
514100000000     C     $FC(6)        IFEQ      '0'
514200130909     C                   SetOn                                        99
514300000000     C                   ENDIF
514400000000     C     $TASTO        WHENEQ    F07
514500000000     C                   Z-ADD     07            PosFun
514600000000     C     $FC(7)        IFEQ      '0'
514700130909     C                   SetOn                                        99
514800000000     C                   ENDIF
514900000000     C     $TASTO        WHENEQ    F08
515000000000     C                   Z-ADD     08            PosFun
515100000000     C     $FC(8)        IFEQ      '0'
515200130909     C                   SetOn                                        99
515300000000     C                   ENDIF
515400000000     C     $TASTO        WHENEQ    F09
515500000000     C                   Z-ADD     09            PosFun
515600000000     C     $FC(09)       IFEQ      '0'
515700130909     C                   SetOn                                        99
515800000000     C                   ENDIF
515900000000     C     $TASTO        WHENEQ    F10
516000000000     C                   Z-ADD     10            PosFun
516100000000     C     $FC(10)       IFEQ      '0'
516200130909     C                   SetOn                                        99
516300000000     C                   ENDIF
516400000000     C     $TASTO        WHENEQ    F11
516500000000     C                   Z-ADD     11            PosFun
516600000000     C     $FC(11)       IFEQ      '0'
516700130909     C                   SetOn                                        99
516800000000     C                   ENDIF
516900000000     C     $TASTO        WHENEQ    F12
517000000000     C                   Z-ADD     12            PosFun
517100000000     C     $FC(12)       IFEQ      '0'
517200130909     C                   SetOn                                        99
517300000000     C                   ENDIF
517400000000     C     $TASTO        WHENEQ    F13
517500000000     C                   Z-ADD     13            PosFun
517600000000     C     $FC(13)       IFEQ      '0'
517700130909     C                   SetOn                                        99
517800000000     C                   ENDIF
517900000000     C     $TASTO        WHENEQ    F14
518000000000     C                   Z-ADD     14            PosFun
518100000000     C     $FC(14)       IFEQ      '0'
518200130909     C                   SetOn                                        99
518300000000     C                   ENDIF
518400000000     C     $TASTO        WHENEQ    F15
518500000000     C                   Z-ADD     15            PosFun
518600000000     C     $FC(15)       IFEQ      '0'
518700130909     C                   SetOn                                        99
518800000000     C                   ENDIF
518900000000     C     $TASTO        WHENEQ    F16
519000000000     C                   Z-ADD     16            PosFun
519100000000     C     $FC(16)       IFEQ      '0'
519200130909     C                   SetOn                                        99
519300000000     C                   ENDIF
519400000000     C     $TASTO        WHENEQ    F17
519500000000     C                   Z-ADD     17            PosFun
519600000000     C     $FC(17)       IFEQ      '0'
519700130909     C                   SetOn                                        99
519800000000     C                   ENDIF
519900000000     C     $TASTO        WHENEQ    F18
520000000000     C                   Z-ADD     18            PosFun
520100000000     C     $FC(18)       IFEQ      '0'
520200130909     C                   SetOn                                        99
520300000000     C                   ENDIF
520400000000     C     $TASTO        WHENEQ    F19
520500000000     C                   Z-ADD     19            PosFun
520600000000     C     $FC(19)       IFEQ      '0'
520700130909     C                   SetOn                                        99
520800000000     C                   ENDIF
520900000000     C     $TASTO        WHENEQ    F20
521000000000     C                   Z-ADD     20            PosFun
521100000000     C     $FC(20)       IFEQ      '0'
521200130909     C                   SetOn                                        99
521300000000     C                   ENDIF
521400000000     C     $TASTO        WHENEQ    F21
521500000000     C                   Z-ADD     21            PosFun
521600000000     C     $FC(21)       IFEQ      '0'
521700130909     C                   SetOn                                        99
521800000000     C                   ENDIF
521900000000     C     $TASTO        WHENEQ    F22
522000000000     C                   Z-ADD     22            PosFun
522100000000     C     $FC(22)       IFEQ      '0'
522200130909     C                   SetOn                                        99
522300000000     C                   ENDIF
522400000000     C     $TASTO        WHENEQ    F23
522500000000     C                   Z-ADD     23            PosFun
522600000000     C     $FC(23)       IFEQ      '0'
522700130909     C                   SetOn                                        99
522800000000     C                   ENDIF
522900000000     C     $TASTO        WHENEQ    F24
523000000000     C                   Z-ADD     24            PosFun
523100000000     C     $FC(24)       IFEQ      '0'
523200130909     C                   SetOn                                        99
523300000000     C                   ENDIF
523400000000     C                   ENDSL
523500000000     C*
523600000000     C                   ENDSR
523700000000     C/EJECT
523800000000     C******************************************************
523900000000     C* PGM PER GESTIONE MESSAGGI ERRORE
524000000000     C******************************************************
524100000000     C     MSGPGM        BEGSR
524200000000     C*
524300000000     C                   Z-ADD     2             MSGMSG
524400130909     C                   MoveL     'S'           MSGRSP
524500130909     C                   MoveL     'CR'          MSGTPR
524600130909     C                   MoveL     'S'           MSGEMV
524700000000     C                   Z-ADD     10            MSGRGP
524800130909     C                   MoveL     'S'           MSGLCK
524900000000     C                   MOVEA     STATUS        STS
525000130909     C                   MoveL     'N'           MSGCNL
525100130909     C                   MoveL     'N'           MSGVID
525200130909     C                   MoveL     'N'           MSGOPE
525300130909     C                   MoveL     'N'           MSGSTP
525400130909     C                   CALL      'SDGMSGR'
525500130909     C                   PARM                    SDGMSGDS
525600000000     C*
525700000000     C                   ENDSR
525800000000     C/EJECT
525900000000     C************************************************************
526000000000     C* OPERAZIONI INIZIALI
526100000000     C************************************************************
526200000000     C     *INZSR        BEGSR
526300000000     C*
526400000000     C* Reperimento parametri
526500000000     C*
526600000000     C     *ENTRY        PLIST
526700130909     C                   PARM                    KPJBA
526800130909     C                   PARM                    SDGSG76DS
526900000000     C*
527000000000     C* Reperimento dati società
527100000000     C*
527200130909     C                   MoveL     'SOC001'      TIPXSC
527300130909     C                   MoveL     *BLANK        SOCXSC
527400130909     C                   EXSR      REPSOC
527500000000     C*
527600000000     C* Controlli dati società
527700000000     C*
527800000000     C     RTNXSC        IFNE      '1'
527900130909     C                   MoveL     XSOCDS        SOC001
528000000000     C                   ELSE
528100130909    >C                   MOVE      '1'           ERRG76
528200000000     C                   MOVE      *on           $Fine
528300130909     C                   EXSR      EndPgm
528400000000     C                   ENDIF
528500000000     C*
528600000000     C                   MOVE      *BLANK        AUTDS
528700000000     C*
528800000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
528900130909     C                   Eval      $D1L01 = %Size(Z1Ke1)
529000130909     C                   Eval      $D1L02 = %Size(Z1Ke2)
529100000000     C*
529200000000     C* Reperimento tasti di funzione
529300000000     C*
529400130909     C                   EXSR      REPKEY
529500000000     C*
529600000000     C* Richiamo routine per aggiunta tasti funzione personalizzati
529700130909     C                   CALLB     'X46USREXT'
529800130909     C                   PARM                    *omit
529900130909     C                   PARM                    *omit
530000130909     C                   PARM                    *omit
530100130909     C                   PARM                    *omit
530200130909     C                   PARM                    *omit
530300130909     C                   PARM                    $FX1
530400130909     C                   PARM                    $FC1
530500130909     C                   PARM                    $PGF1
530600130909     C                   PARM                    DsPgm
530700130909     C                   PARM                    xsccut
530800130909     C                   PARM                    xscgrp
530900000000     C*
531000000000     C     *DMY          Move      UDate         UDateISO
531100130912     C*
531200130912     C* reperisco DATFMT del job
531300130912     C                   CallB     'XDATFMT'
531400130912     C                   Parm                    $DatFmt           4
531500000000     C*
531600000000     C                   ENDSR
531700000000     C/EJECT
531800000000     C************************************************************
531900000000     C* REPERIMENTO TASTI DI FUNZIONE
532000000000     C************************************************************
532100000000     C     REPKEY        BEGSR
532200000000     C*
532300000000     C                   MOVEA     $FM1          $FM
532400000000     C                   MOVEA     $FC1          $FC
532500130909     C                   EXSR      REPKKK
532600000000     C                   MOVEA     $FX           $FX1
532700000000     C*
532800000000     C                   ENDSR
532900000000     C/EJECT
533000000000     C************************************************************
533100000000     C* REPERIMENTO TASTI DI FUNZIONE
533200000000     C************************************************************
533300000000     C     REPKKK        BEGSR
533400000000     C*
533500000000     C                   CLEAR                   $FX
533600000000     C     1             DO        24            X
533700000000     C*
533800000000     C     $FM(X)        IFNE      *BLANK
533900000000     C     $FC(X)        ANDNE     '0'
534000000000     C*
534100130909     C                   MoveL     $FM(X)        IDMSG
534200130909     C                   CALLB     'XRTVM'
534300130909     C                   PARM                    IDMSG            27
534400130909     C                   PARM                    TXTMSG          100
534500130909     C                   PARM                    ERRMSG            1
534600000000     C*
534700000000     C     ERRMSG        IFNE      *ON
534800130909     C                   MoveL     TXTMSG        $FX(X)
534900000000     C                   ELSE
535000130909     C                   MoveL     *ALL' '       $FX(X)
535100000000     C                   ENDIF
535200000000     C*
535300000000     C                   ENDIF
535400000000     C*
535500000000     C                   ENDDO
535600000000     C*
535700000000     C                   ENDSR
535800000000     C/EJECT
535900000000     C************************************************************
536000000000     C* INIZIALIZZAZIONE VARIABILI
536100000000     C************************************************************
536200000000     C     INZVAR        BEGSR
536300000000     C*
536400000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
536500000000     C* reperimento dati società
536600130909    >C                   If        SocietaG76 <> xscsoc    and
536700130909    >C                             OpzG76 <> *blank
536800130909     C                   MoveL     'SOC001'      TIPXSC
536900130909    >C                   MoveL     SocietaG76    SOCXSC
537000130909     C                   EXSR      REPSOC
537100000000     C     RTNXSC        IFNE      '1'
537200130909     C                   MoveL     XSOCDS        SOC001
537300000000     C                   ELSE
537400130909    >C                   MOVE      '1'           ERRG76
537500000000     C                   MOVE      *on           $Fine
537600130909     C                   EXSR      EndPgm
537700000000     C                   ENDIF
537800000000     C                   endif
537900000000     C*
538000000000     C* Reperimento autorizzazioni
538100000000     C*
538200000000     C* le autorizzazioni potrebbero essere già state passate,
538300000000     C* nel qual caso non vanno reperite
538400000000    >C     AUTDS         IFEQ      *BLANK
538500130909    >C                   EXSR      REPAUT
538600000000    >C                   ELSE
538700130909    >C                   MoveL     XCADDS        CONCIVDS
538800000000    >C                   ENDIF
538900000000     C*
539000000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
539100000000     C* le autorizzazione
539200000000     C*
539300130909    >C                   EXSR      TSTAUT
539400130911     C*
539500130918     C* Reperisco dati della società da tab. G12
539600130918     C                   EXSR      RepG12Soc
539700000000     C*
539800000000     C* Reperisco i dati di setup del modulo elenchi
539801160104R195 C*
539802160104 "   C                   Select
539803160104 "   C* se Spesometro o Acquisti da San marino
539804160104 "   C                   When      TpComG76    = 'E'     or
539805160104R195 C                             TpComG76    = 'S'
539900130918     C                   EXSR      RepSDGSPECF
539901160104R195 C* se Black List
539902160104 "   C                   When      TpComG76    = 'B'
539903160104 "   C                   EXSR      RepSDBLACK2
539904160104 "   C*
539905160104R195 C                   endSl
540000000000     C*
540100000000     C* Definizione dei tasti funzione utilizzabili a pgm
540200000000     C*
540300130909     C                   EXSR      DEFOPZ
540400000000     C*
540500000000     C* Variabili per gestione videate
540600000000     C*
540700000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
540800130909     C                   MOVE      *Off          $FINE                          fine pgm
540900130909     C                   MOVE      *Off          $FlgDef                        fine pgm
541000000000     C                   MOVE      *ON           $INZD1                         inizializzione D1
541100000000     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
541200000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
541300000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
541400000000     C* azzero la posizione del cursore
541500000000     C                   Z-ADD     0             H1RIGA
541600000000     C                   Z-ADD     0             H1COLO
541700000000     C*
541800000000     C* Variabili appoggio
541900000000     C*
542000000000     C                   Z-ADD     0             CURR
542100000000     C                   Z-ADD     0             CURC
542200000000     C*
542300000000     C* Valorizzazione campi univoci testate
542400000000     C*
542500000000     C                   CLEAR                   FMTT1
542600130909     C                   MoveL     KNSIF         NOMSIF
542700130909     C                   MoveL     XSCDSI        NOMDIT
542800130909     C                   MoveL     DSPGM         NOMPGM           10
542900130909     C                   MoveL     KCDAZ         NOMAZN
543000000000     C* recupero il sottotitolo (se c'è)
543100130909     C                   MoveL     *BLANK        T1TIT
543200000000     C     SOTTIT        IFNE      *BLANK
543300130909     C                   MoveL     SOTTIT        IDMSG
543400130909     C                   CALLB     'XRTVM'
543500130909     C                   PARM                    IDMSG            27
543600130909     C                   PARM                    TXTMSG          100
543700130909     C                   PARM                    ERRMSG            1
543800130909     C                   PARM      *ON           CTRMSG            1            centratura
543900130909     C                   PARM      30            LENMSG            3 0          lun output.
544000000000     C     ERRMSG        IFNE      *ON
544100130909     C                   MoveL     TXTMSG        T1TIT
544200000000     C                   ELSE
544300130909     C                   MoveL     *ALL'?'       T1TIT
544400000000     C                   ENDIF
544500000000     C                   ENDIF
544501151201R195 C*
544502151201  "  C* Recupero il titolo con il tipo di comunicazione
544503151201  "  C                   MOVEL     *BLANK        T1TIT
544504151201  "  C*
544505151201  "  C                   Select
544506151201  "  C*
544507151201  "  C* Comunicazione Spesometro
544508151201  "  C                   When      TpComG76   = 'E'
544509151201  "  C                   if        SOTTITT1   <> *BLANK
544510151201  "  C                   MOVEL     SOTTITT1      IDMSG
544511151201  "  C                   endif
544512151201  "  C*
544513151201  "  C* Comunicazione Acquisti da San Marino
544514151201  "  C                   When      TpComG76   = 'S'
544515151201  "  C                   if        SOTTITT2   <> *BLANK
544516151201  "  C                   MOVEL     SOTTITT2      IDMSG
544517151201  "  C                   endif
544518151201  "  C*
544519151201  "  C* Comunicazione Black list
544520151201  "  C                   When      TpComG76   = 'B'
544521151201  "  C                   if        SOTTITT3   <> *BLANK
544522151201  "  C                   MOVEL     SOTTITT3      IDMSG
544523151201  "  C                   endif
544524151201  "  C*
544525151201  "  C                   endsl
544526151201  "  C*
544527151201  "  C                   CALLB     'XRTVM'
544528151201  "  C                   PARM                    IDMSG            27
544529151201  "  C                   PARM                    TXTMSG          100
544530151201  "  C                   PARM                    ERRMSG            1
544531151201  "  C                   PARM      *ON           CTRMSG            1            centratura
544532151201  "  C                   PARM      30            LENMSG            3 0          lun output.
544533151201  "  C     ERRMSG        IFNE      *ON
544534151201  "  C                   MOVEL     TXTMSG        T1TIT
544535151201  "  C                   ELSE
544536151201  "  C                   MOVEL     *ALL'?'       T1TIT
544537151201R195 C                   ENDIF
544600000000     C* recupero il tipo operazione
544700000000     C                   Z-ADD     1             X
544800130909    >C     OPZG76        LOOKUP    $OZ(X)                                 21
544900000000     C     *IN21         IFEQ      *ON
545000130909     C                   MoveL     $ID(X)        IDMSG
545100130909     C                   CALLB     'XRTVM'
545200130909     C                   PARM                    IDMSG            27
545300130909     C                   PARM                    TXTMSG          100
545400130909     C                   PARM                    ERRMSG            1
545500130909     C                   PARM      *Off          CTRMSG            1            centratura
545600130909     C                   PARM      15            LENMSG            3 0          lun output.
545700000000     C     ERRMSG        IFNE      *ON
545800130909     C                   MoveL     TXTMSG        T1OPE
545900000000     C                   ELSE
546000130909     C                   MoveL     *ALL'?'       T1OPE
546100000000     C                   ENDIF
546200000000     C                   ENDIF
546300000000     C*
546400000000     C* Inizializzazione/Decodifica prima videata
546500130909     C                   EXSR      REPDEF
546600130909     C                   EXSR      INZD1
546700130909     C                   EXSR      F02D1
546800130909     C                   MOVE      *Off          $INZD1
546900000000     C*
547000000000     C                   ENDSR
547100000000     C/EJECT
547200000000     C************************************************************
547300000000     C* REPERIMENTO DATI SOCIETA'
547400000000     C************************************************************
547500000000     C     REPSOC        BEGSR
547600000000     C*
547700130909     C                   CALL      'SDGSOCDR'
547800130909     C                   PARM                    TIPXSC            6
547900130909     C                   PARM                    SOCXSC            3
548000130909     C                   PARM                    CDSXSC            9 0
548100130909     C                   PARM                    MODXSC            3
548200130909     C                   PARM      *BLANKS       RTNXSC            1
548300130909     C                   PARM                    XSOCDS
548400130909     C                   PARM                    KPJBA
548500000000     C*
548600000000     C                   ENDSR
548700000000     C/EJECT
548800000000     C************************************************************
548900000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
549000000000     C************************************************************
549100000000     C     REPAUT        BEGSR
549200000000     C*
549300000000     C                   CLEAR                   AUTDS
549400000000     C                   Clear                   ConCivDs
549500000000     C*
549600130909     C                   MoveL     XSCSOC        XCASOC                         SOCIETA
549700130909     C                   MoveL     XSCDUN        XCAUNI                         UNITA
549800000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
549900130909     C                   MoveL     KNMUS         XCAPRF                         PROFILO
550000130909    >C                   MoveL     'CONCIV'      XCAFNC                         FUNZIONE
550100130909    >C                   MoveL     *ZERO         XCAVFU                         VARIABILE
550200000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
550300000000    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
550400000000     C*
550500130909     C                   CALLB     'XCHKAUT'
550600130909     C                   PARM                    AUTDS
550700000000     C*
550800000000     C* errori/non abilitazione
550900000000     C     XCARTN        IFNE      0
551000130909    >C                   MOVE      '2'           ERRG76
551100000000     C                   MOVE      *ON           $FINE
551200130909     C                   EXSR      ENDPGM
551300000000     C                   ELSE
551400130909    >C                   MoveL     XCADDS        CONCIVDS
551500000000     C                   ENDIF
551600000000     C*
551700000000     C                   ENDSR
551800000000     C/EJECT
551900000000     C************************************************************
552000000000     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
552100000000     C************************************************************
552200000000     C     TSTAUT        BEGSR
552300000000     C*
552400000000     C                   SELECT
552500000000     C*
552600000000     C* se richiamato in immissione
552700130909    >C     OPZG76        WHENEQ    '01'
552800000000     C* ma l'utente non è abilitato
552900130909    >C     IMMALL        IFEQ      *Off
553000000000     C* esco da pgm avvertendolo
553100130909    >C                   MOVE      '2'           ERRG76
553200000000     C                   MOVE      *ON           $FINE
553300130909     C                   EXSR      ENDPGM
553400000000     C                   ENDIF
553500000000     C*
553600000000     C* se richiamato in modifica
553700130909    >C     OPZG76        WHENEQ    '02'
553800000000     C* ma l'utente non è abilitato
553900130909    >C     MANALL        IFEQ      *Off
554000000000     C* testo se l'utente è abilitato almeno alla visualizzazione
554100000000    >C     VISALL        IFEQ      *ON
554200130909    >C                   MOVE      '05'          OPZG76
554300000000     C                   ELSE
554400000000     C* altrimenti esco da pgm avvertendolo
554500130909    >C                   MOVE      '2'           ERRG76
554600000000     C                   MOVE      *ON           $FINE
554700130909     C                   EXSR      ENDPGM
554800000000     C                   ENDIF
554900000000     C                   ENDIF
555000000000     C*
555100000000     C* se richiamato in copia
555200130909    >C     OPZG76        WHENEQ    '03'
555300000000     C* ma l'utente non è abilitato
555400130909    >C     IMMALL        IFEQ      *Off
555500000000     C* esco da pgm avvertendolo
555600130909    >C                   MOVE      '2'           ERRG76
555700000000     C                   MOVE      *ON           $FINE
555800130909     C                   EXSR      ENDPGM
555900000000     C                   ENDIF
556000000000     C*
556100000000     C* se richiamato in annullamento
556200130909    >C     OPZG76        WHENEQ    '04'
556300000000     C* ma l'utente non è abilitato
556400130909    >C     ANNALL        IFEQ      *Off
556500000000     C* esco da pgm avvertendolo
556600130909    >C                   MOVE      '2'           ERRG76
556700000000     C                   MOVE      *ON           $FINE
556800130909     C                   EXSR      ENDPGM
556900000000     C                   ENDIF
557000000000     C*
557100000000     C* se richiamato in visualizzazione
557200130909    >C     OPZG76        WHENEQ    '05'
557300000000     C* ma l'utente non è abilitato
557400130909    >C     VISALL        IFEQ      *Off
557500000000     C* esco da pgm avvertendolo
557600130909    >C                   MOVE      '2'           ERRG76
557700000000     C                   MOVE      *ON           $FINE
557800130909     C                   EXSR      ENDPGM
557900000000     C                   ENDIF
558000000000     C*
558100000000     C                   ENDSL
558200000000     C*
558300000000     C                   ENDSR
558400000000     C/EJECT
558500130918     C************************************************************
558600130918     C* Reperisco dati da G12 della società
558700130918     C************************************************************
558800130918     C     RepG12Soc     BEGSR
558900130918     C*
559000130918     C                   Eval      $UICSoc     = 0
559100130918     C                   Eval      $NazSoc     = *Blank
559200130918     C*
559300130918     C                   Eval      WStato      = XScSta
559400130918     C                   EXSR      RepTabG12
559500130918     C*
559600130918     C                   if        XTBErr     <> '0'
559700130918     C                   Eval      ERRG76      = '1'
559800130918     C                   EXSR      ENDPGM
559900130918     C                   endif
560000130918     C*
560100130918     C                   if        $CodUIC     = 0
560200130918     C                   Eval      ERRG76      = '1'
560300130918     C                   EXSR      ENDPGM
560400130918     C                   endif
560500130918     C*
560600130918     C                   Eval      $UICSoc     = $CodUIC
560700130918     C*
560800130918     C                   Eval      $NazSoc     = $NazISO
560900130918     C                   if        $NazSoc     = *Blank
561000130918     C                   Eval      $NazSoc     = XScSta
561100130918     C                   endif
561200130918     C*
561300130918     C                   ENDSR
561400130918      /EJECT
561500130918     C************************************************************
561600130918     C* Reperisco dati tabella G12
561700130918     C************************************************************
561800130918     C     RepTabG12     BEGSR
561900130918     C*
562000130918     C                   Eval      $CodUIC     = 0
562100130918     C                   Eval      $NazISO     = *Blank
562200130918     C*
562300130918     C                   Clear                   ANGG12DS
562400130918     C                   Clear                   SDGTABDS
562500130918     C*
562600130918     C                   Eval      XTbRic      = '1'
562700130918     C                   Eval      XTbAzi      = XScSoc
562800130918     C                   Eval      XTbKey      = *Zero
562900130918     C                   Eval      XTbCod      = 'G12'
563000130918     C                   Move      WStato        XTbKey
563100130918     C*
563200130918     C                   CALL      'SDGTABDR'
563300130918     C                   Parm                    SDGTABDS
563400130918     C*
563500130918     C                   if        XTBErr      = '0'
563600130918     C                   Eval      ANGG12DS    = XTbUni
563700130918     C*
563800130918     C                   if        %SubSt(ANGG12DS:50:3)   = *Blank
563900130918     C                   Z-Add     0             $CodUIC
564000130918     C                   else
564100130918     C                   Z-Add     CodUICG12     $CodUIC
564200130918     C                   endif
564300130918     C*
564400130918     C                   Eval      $NazISO     = NazG12
564500130918     C*
564600130918     C                   endif
564700130918     C*
564800130918     C                   ENDSR
564900130918      /EJECT
565000130918     C************************************************************
565100130918     C* Reperimento parametri di setup per Spesometro cli/for
565200130918     C************************************************************
565300130918     C     RepSDGSPECF   BEGSR
565400000000     C*
565500130909     C                   Clear                   SDGPA08DS
565600000000     C*
565700130909     C                   CALLB     'XPAR'
565800130909     C                   PARM                    XScSoc
565900130909     C                   PARM      'SDGSPECF'    XPaPar            8
566000130909     C                   PARM                    XPaOut         2000
566100130909     C                   PARM      *Off          XPaErr            1
566200130909     C                   PARM      ' '           XPaRic            1
566300000000     C*
566400000000     C                   if        XPaErr      = *On
566500130909     C                   EXSR      ENDPGM
566600000000     C*
566700000000     C                   else
566800130909     C                   MoveL     XPaOut        SDGPA08DS
566900000000     C*
567000130909     C                   if        AttSCFA08   <> *On
567100130909     C                   EXSR      ENDPGM
567200000000     C                   endif
567300000000     C*
567400000000     C                   endif
567500000000     C*
567600000000     C                   ENDSR
567700000000     C/EJECT
567701160104R195 C************************************************************
567702160104 "   C* Reperimento parametri di setup per Black List
567703160104 "   C************************************************************
567704160104R195 C     RepSDBLACK2   BEGSR
567705160104     C*
567706160104     C                   Clear                   SDGPA11DS
567707160104     C*
567708160104     C                   CALLB     'XPAR'
567709160104     C                   Parm                    XScSoc
567710160104     C                   Parm      'SDBLACK2'    XPaPar            8
567711160104     C                   Parm                    XPaOut         2000
567712160104     C                   Parm      *OFF          XPaErr            1
567713160104     C                   Parm      ' '           XPaRic            1
567714160104     C*
567716160104     C                   if        XPaErr      = *On
567717160104     C                   EXSR      ENDPGM
567718160104     C*
567719160104     C                   else
567720160104     C                   MoveL     XPaOut        SDGPA11DS
567721160104     C*
567722160104     C                   if        AttBLA11    <> *On
567723160104     C                   EXSR      ENDPGM
567724160104     C                   endif
567725160104     C*
567726160104     C                   endif
567732160104     C*
567733160104R195 C                   ENDSR
567734160104R195 C/EJECT
567800000000     C************************************************************
567900000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
568000000000     C************************************************************
568100000000     C     DEFOPZ        BEGSR
568200000000     C*
568300000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1
568400000000     C* con S o N a seconda della utilizzabilità o meno
568500000000     C* del tasto funzione correlato
568600000000     C*
568700000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
568800000000     C* di compilazione
568900130909     C                   Reset                   $FC1
569000130909     C                   Reset                   $FR1
569100000000     C*
569200000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
569300000000     C* invalido alcune opzioni
569400000000     C                   SELECT
569500000000     C* Se pgm richiamato in interrogazione disattivo :
569600130909    >C     OPZG76        WHENEQ    '05'
569700000000     C* F2=Decodifica
569800000000     C                   Z-ADD     2             X
569900000000     C                   MOVE      '0'           $FC1(X)
570000000000     C* F4=Ricerca
570100000000     C                   Z-ADD     4             X
570200000000     C                   MOVE      '0'           $FC1(X)
570300000000     C* F5=Ripristino
570400000000     C                   Z-ADD     5             X
570500000000     C                   MOVE      '0'           $FC1(X)
570600000000     C* F6=Conferma
570700000000     C                   Z-ADD     06            X
570800000000     C                   MOVE      '0'           $FC1(X)
570900000000     C* Se pgm richiamato in annullamento disattivo :
571000130909    >C     OPZG76        WHENEQ    '04'
571100000000     C* F2=Decodifica
571200000000     C                   Z-ADD     2             X
571300000000     C                   MOVE      '0'           $FC1(X)
571400000000     C* F4=Ricerca
571500000000     C                   Z-ADD     4             X
571600000000     C                   MOVE      '0'           $FC1(X)
571700000000     C* F5=Ripristino
571800000000     C                   Z-ADD     5             X
571900000000     C                   MOVE      '0'           $FC1(X)
572000000000     C*
572100000000     C                   OTHER
572200000000     C*
572300000000     C                   ENDSL
572400000000     C*
572500000000     C                   ENDSR
572600000000     C/EJECT
572700000000     C************************************************************
572800000000     C* REPERIMENTO CAMPI DI DEFAULT
572900000000     C************************************************************
573000000000     C     REPDEF        BEGSR
573100000000     C*
573200130909     C                   Eval      $FlgDef     = *On
573300000000     C*
573400000000     C                   ENDSR
573500000000     C************************************************************
573600111010     C* DEFINIZIONE  LIST
573700000000     C************************************************************
573800130919     C     DefKlist      BEGSR
573900000000     C*
574000000000     C* klist
574100000000     C*
574200130909     C* SDGSCF11L
574300130919     C     K07SCF11      KLIST
574400130909     C                   KFLD                    SCFTpCom
574500130909     C                   KFLD                    SCFAnno
574600130909     C                   KFLD                    SCFMese
574700130909     C                   KFLD                    SCFSys
574800130909     C                   KFLD                    SCFNrAsReg
574900130909     C                   KFLD                    SCFNrRigaM
575000130909     C                   KFLD                    SCFNrRiga
575100130919     C*
575200130919     C* SDGSCF01L
575300130919     C     K15SCF01      KLIST
575400130919     C                   KFLD                    SCFSOCIETA
575500130919     C                   KFLD                    SCFTPCOM
575600130919     C                   KFLD                    SCFANNO
575700130919     C                   KFLD                    SCFMESE
575800130919     C                   KFLD                    SCFCOGRAG
575900130919     C                   KFLD                    SCFNOME
576000130919     C                   KFLD                    SCFDTNASC
576100130919     C                   KFLD                    SCFLOCNASC
576200130919     C                   KFLD                    SCFPRVNASC
576300130919     C                   KFLD                    SCFSTATO
576400130919     C                   KFLD                    SCFCODUIC
576500130919     C                   KFLD                    SCFLOCALIT
576600130919     C                   KFLD                    SCFINDIRE
576700130919     C                   KFLD                    SCFPARTIVA
576800130919     C                   KFLD                    SCFCDFISC
576900000000     C*
577000000000     C* ANRIV01L
577100000000     C     K04RIV01      KLIST
577200000000     C                   KFLD                    RIVSocieta
577300000000     C                   KFLD                    RIVTpReg
577400000000     C                   KFLD                    RIVAliq
577500000000     C                   KFLD                    RIVRifIVA
577600000000     C*
577700130909     C* SDGSPR03L
577800130909     C     K04SPR03      KLIST
577900130909     C                   KFLD                    SPRSOCIETA
578000130909     C                   KFLD                    SPRTIPREG
578100130909     C                   KFLD                    SPRSTPREG
578200130909     C                   KFLD                    SPRCAUSALE
578300130909     C*
578400130909     C* ANDLI01L
578500130909     C     K03DLI01      klist
578600130909     C                   kfld                    DLISocieta
578700130909     C                   kfld                    DLITpReg
578800130909     C                   kfld                    DLILibro
578900000000     C*
579000000000     C                   ENDSR
579100000000     C/EJECT
579200000000     C************************************************************
579300000000     C* RIEMPIMENTO TASTI FUNZIONE
579400000000     C************************************************************
579500000000     C     RIEKEY        BEGSR
579600000000     C*
579700000000     C* Input :
579800000000     C* - $FX = schiera con tutti i testi disponibili
579900000000     C* - $FC = schiere con i flag di validità associati ai testi
580000000000     C* - $FR = schiere con i flag di REVERSE IMAGE
580100000000     C* - $ULK = ultimo testo caricato
580200000000     C* - $L01 = Lunghezza prima riga
580300000000     C* - $L02 = Lunghezza seconda riga
580400000000     C* Output :
580500000000     C* - $ULK
580600000000     C* - $KEY1= 1° riga di tasti funzione
580700000000     C* - $KEY2= 2° riga di tasti funzione
580800000000     C*
580900130909     C                   CALLB     'XKEYMSG'
581000000000     C* input
581100130909     C                   PARM                    $FX
581200130909     C                   PARM                    $FC
581300130909     C                   PARM                    $FR
581400130909     C                   PARM                    $ULK              3 0
581500000000     C* output
581600130909     C                   PARM                    $KEY1            79
581700130909     C                   PARM                    $KEY2            79
581800000000     C* input
581900130909     C                   PARM                    $L01              3 0
582000130909     C                   PARM                    $L02              3 0
582100000000     C*
582200130909     C                   PARM                    $ALLFUNCT       480
582300000000     C*
582400000000     C                   ENDSR
582500000000** $OZ / $ID
58260000000001SDGMSG    *LIBL     SDC0002
58270000000002PROMSG    *LIBL     COS0002
58280000000003PROMSG    *LIBL     COS0003
58290000000004PROMSG    *LIBL     COS0004
58300000000005PROMSG    *LIBL     COS0005
583100000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
583200000000PROMSG    *LIBL     KEY0001  01
583300000000PROMSG    *LIBL     KEY0002  02
583400000000PROMSG    *LIBL     KEY0003  03
583500000000PROMSG    *LIBL     KEY0004  04
583600000000PROMSG    *LIBL     KEY0005  05
583700000000PROMSG    *LIBL     KEY0006  06
583800000000PROMSG    *LIBL     KEY0007  07
583900000000PROMSG    *LIBL     KEY0008  08
584000000000PROMSG    *LIBL     KEY0009  09
584100000000PROMSG    *LIBL     KEY0010  10
584200130923SDGMSG    *LIBL     SDK0018  11
584300000000PROMSG    *LIBL     KEY0012  12
584400000000PROMSG    *LIBL     KEY0013  13
584500000000PROMSG    *LIBL     KEY0014  14
584600000000PROMSG    *LIBL     KEY0015  15
584700000000PROMSG    *LIBL     KEY0016  16
584800000000PROMSG    *LIBL     KEY0017  17
584900000000PROMSG    *LIBL     KEY0018  18
585000000000PROMSG    *LIBL     KEY0019  19
585100000000PROMSG    *LIBL     KEY0020  20
585200000000PROMSG    *LIBL     KEY0021  21
585300000000PROMSG    *LIBL     KEY0022  22
585400000000PROMSG    *LIBL     KEY0023  23
585500000000PROMSG    *LIBL     KEY0024  24
585600000000**  ABILITAZIONE D1 : $FC1 / $FR1
5857000000000N  01
5858000000000N  02
5859000000001N  03
5860000000001N  04
5861000000001N  05
5862000000001N  06
5863000000000N  07
5864000000001N  08
5865000000000N  09
5866000000000N  10
5867001309231N  11
5868000000001N  12
5869000000000N  13
5870000000000N  14
5871000000000N  15
5872000000000N  16
5873000000000N  17
5874000000000N  18
5875000000000N  19
5876000000000N  20
5877000000000N  21
5878000000000N  22
5879000000000N  23   Impostare a '1' se si utilizzano opzioni
5880000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
