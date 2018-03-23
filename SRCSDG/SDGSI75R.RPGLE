000100170804     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND)
000200170804     H*PARMS CVTOPT(*NONE) ACTGRP(DETT)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000      *
000500000000      *           -----------------------------
000600170804      *                Spesometro trimestrale XML
000700170804      *                 Dati di dettaglio - guida
000800000000      *           -----------------------------
000900170804      *
001000170804R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
001100000000      *
001200000000     F*----------------------------------------------------------------
001300170804     F* Spesometro - Movimenti estratti
001400170808     FSDGCST00F UF   E             DISK    UsrOpn
001500170804     F*-------------
001600170804     F* Spesometro - Movimenti estratti
001700170804     F* Key: Soc/TpCom/TpRec/AA/TT/MM/Dati di raggruppam.     i
001800170804     FSDGCST02L IF   E           K DISK    Rename(SDGCST000:SDGCST002)
001900170804     F                                     INFDS(NRRDS)
002000110825     F*------------
002100170804     FSDGSI75D  CF   E             WORKSTN
002200170804     F                                     RENAME(SI75D1:FMTD1)
002300170804     F                                     RENAME(SI75D2:FMTD2)
002400170804     F                                     RENAME(SI75D3:FMTD3)
002500170804     F                                     RENAME(SI75S1:FMTS1)
002600170804     F                                     RENAME(SI75C1:FMTC1)
002700170804     F                                     RENAME(SI75T1:FMTT1)
002800170804     F                                     RENAME(SI75Z1:FMTZ1)
002900170804     F                                     RENAME(SI75Z2:FMTZ2)
003000170804     F                                     RENAME(SI75Z3:FMTZ3)
003100000000     F                                     SFILE(FMTS1:S1NRR)
003200000000     F                                     INFDS(DSFMT)
003300000000     D*----------------------------------------------------*
003400170804     D*Num.relativo record per SDGCST02L
003500170804     D NRRDS           DS           512
003600170804     D  NrRCST               397    400B 0
003700170804     D*-------------
003800000000     D* Schiere per la gestione delle opzioni
003900000000     D*
004000000000     D* dimensione della schiera $MS1, da non cambiare
004100000000     D $DIMOP          C                   CONST(30)
004200000000     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
004300000000     D $TX1            S             50    DIM($dimop) ALT($MS1)
004400000000     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004500000000     D $FL1            S              1    DIM($dimop) ALT($OP1)
004600000000     D* schiera contenente i nomi dei programma richiamati attraverso
004700000000     D* le opzioni personalizzate
004800000000     D $PGO1           S             10    DIM($dimop)
004900000000     D $TX             S             50    DIM(50)
005000000000     D $FL             S              1    DIM(50)
005100000000     D* Schiere generali per la gestione dei tasti di funzione
005200000000     D $FM             S             27    DIM(24)
005300000000     D $FX             S             50    DIM(24)
005400000000     D $FC             S              1    DIM(24)
005500000000     D $FR             S              1    DIM(24)
005600000000     D* Schiere per la gestione dei tasti di funzione
005700000000     D* Video D1
005800000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005900000000     D $FX1            S             50    DIM(24) ALT($FM1)
006000000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006100000000     D $FR1            S              1    DIM(24) ALT($FC1)
006200000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
006300000000     D* funzione personalizzata S1
006400000000     D $PGF2           S             10    DIM(25)
006500000000     D* Schiere per la gestione dei tasti di funzione
006600000000     D* Video S1
006700000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006800000000     D $FX2            S             50    DIM(24) ALT($FM2)
006900000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
007000000000     D $FR2            S              1    DIM(24) ALT($FC2)
007100000000     D* Schiere per la gestione dei tasti di funzione
007200000000     D* Video D2
007300000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
007400000000     D $FX3            S             50    DIM(24) ALT($FM3)
007500000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007600000000     D $FR3            S              1    DIM(24) ALT($FC3)
007700000000     D*
007800000000     D $D1L01          S              3  0
007900000000     D $D1L02          S              3  0
008000000000     D $S1L01          S              3  0
008100000000     D $S1L02          S              3  0
008200000000     D $C1Op1          S              3  0
008300000000     D $C1Op2          S              3  0
008400000000     D $D2L01          S              3  0
008500000000     D $D2L02          S              3  0
008600000000     D*
008700000000     D*----------------------------------------------------*
008800000000     D* Valori assunti
008900000000     D XMPEDS        E DS                  INZ
009000000000     D XMPEDS1       E DS                  INZ
009100000000     D DSMPE           DS                  INZ
009200000000     D  $LOP                   1      1  0
009300000000     D  $ORD                   2      3  0
009400000000     D*-------------
009500000000     D* Passaggio Parametri
009600000000     D KPJBA         E DS
009700000000     D*-------------
009800000000     D* Reperimento Autorizzazioni
009900000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010000000000     D*-------------
010100000000     D* Dati per pgm XNOAUT
010200000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
010300000000     D*-------------
010400000000     D* Passaggio Autorizzazioni
010500000000     D DSAUT           DS           512
010600000000     D*-------------
010700000000     D* Maschera dati autorizzazioni
010800000000     D CONCIVDS      E DS
010900000000     D*-------------
011000000000     D* Dati di ambiente ottenuti da SDGSOCDR
011100000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
011200000000     D*-------------
011300000000     D* DS Interna per dati di output di SDGSOCDR
011400000000     D XSOCDS          DS          1000
011500000000     D*-------------
011600000000     D* DS Esterna gestione messaggi
011700000000     D SDGMSGDS      E DS
011800000000     D  STS          E                     EXTFLD(MSGSTS)
011900000000     D                                     DIM(333)
012000000000     D  JBA          E                     EXTFLD(MSGJBA)
012100000000     D                                     DIM(502)
012200000000     D*-------------
012300000000     D* campi di lavoro per emissione window di avvertimento
012400000000     D WXMSGInz        S              1A   INZ('1')
012500000000     D*-------------
012600000000     D* Richiamo a SDGTABDR
012700000000     D SDGTABDS      E DS                  inz
012800000000     D*-------------
012900000000     D* Parametri per ricerca codice ANATB00F
013000000000     D XTABDS        E DS                  INZ
013100000000     D*-------------
013200170804     D* Ricerca codici Stati
013300170804     D ANGG12DS      E DS
013400170804     D* Ricerca codici ISO Stati
013500170804     D SDGHD4DS      E DS
013600170804     D*----------
013700170804     D WData           S               D
013800170804     D WAnno           S              4  0
013900170804     D WDtAlfa         S             10A
014000170804     D DataDs          DS
014100170804     D  AnnoDs                        4A
014200170804     D   Tratt1                       1A   Inz('-')
014300170804     D  MeseDs                        2A
014400170804     D   Tratt2                       1A   Inz('-')
014500170804     D  GiornoDs                      2A
014600000000     D*-------------
014700000000     D* Ricerca legende
014800170804     D ANA064DS      E DS                  INZ
014900170804     D*-------------
015000170804     D* Definizioni controllo causale
015100170804     D Anope00f      e ds                  inz
015200170804     D Refresh         S              1
015300170804     D Uso             S              1
015400170804     D Causale         S              4
015500170804     D*----------
015600170804     D* Ricerca causale
015700170804     D ANA070DS      E DS                  INZ
015800170804     D  RET070       E                     INZ('0')
015900170804     D  OPR070       E                     INZ('0')
016000170804     D  CMT070       E                     INZ('0')
016100170804     D  ERR070       E                     INZ('0')
016200170804     D  FSC070       E                     INZ('S')
016300000000     D*-------------
016400170804     D* Ricerca legende
016500170804     D ANA964DS      E DS                  INZ
016600170804     D*-------------
016700000000     D*Controlli validità per valore di una legenda
016800000000     D X07VALDS      E DS                  Inz
016900000000     D*-------------
017000000000     D* Reperimento parametri
017100000000     D XPaOut          DS          2000
017200000000     D*-------------
017300000000     D* Parametri in input
017400170804    >D SDGSI75DS     E DS
017500170807     D*-------------
017600170807     D* Parametri del PGM di manutenzione  dati anagrafici
017700170807     D SDGSI74DS     E DS                  inz
017800170807     D  RETI74       E                     INZ('0')
017900170807     D  OPRI74       E                     INZ('0')
018000170807     D  ERRI74       E                     INZ('0')
018100170807     D  CMTI74       E                     INZ('0')
018200000000     D*-------------
018300000000     D* Parametri del PGM di manutenzione  dati di dettaglio
018400170804     D SDGSI76DS     E DS                  inz
018500170804     D  RETI76       E                     INZ('0')
018600170804     D  OPRI76       E                     INZ('0')
018700170804     D  ERRI76       E                     INZ('0')
018800170804     D  CMTI76       E                     INZ('0')
018900000000     D*-------------
019000000000     D* Interrogazione conti
019100000000     D ANA399DS      E DS                  Inz
019200000000     D*
019300000000     D WA39Iniz        S                   Like(A39Iniz)
019400000000     D*-------------
019500000000     D* Paramentri richiamo pnota
019600000000     D NDC001DS      E DS                  INZ
019700000000     D  RET001       E                     INZ('0')
019800000000     D  OPR001       E                     INZ('0')
019900000000     D  ERR001       E                     INZ('0')
020000170804     D*-------------
020100170804     D* Ds per la chiave di lettura
020200170804     D DsCliFor        DS
020300170804     D  COGRAG1Ds                          Like(COGRAG1I75)
020400170804     D  CFDs                               Like(CFI75     )
020500170804     D  PARTIVADs                          Like(PARTIVAI75)
020600170804     D  CDFISCDs                           Like(CDFISCI75 )
020700170804     D  STATO1Ds                           Like(STATO1I75 )
020800170804     D  IDPAES1Ds                          Like(IDPAES1I75)
020900170804     D  PIVA1Ds                            Like(PIVA1I75  )
021000170804     D  CDFISC1Ds                          Like(CDFISC1I75)
021100170804     D  TPN1Ds                             Like(TPN1I75   )
021200170804     D  DENOM1Ds                           Like(DENOM1I75 )
021300170804     D  NOME1Ds                            Like(NOME1I75  )
021400170804     D  COGNOM1Ds                          Like(COGNOM1I75)
021500170804     D  INDIR2Ds                           Like(INDIR2I75 )
021600170804     D  NRCIV2Ds                           Like(NRCIV2I75 )
021700170804     D  CAP2Ds                             Like(CAP2I75   )
021800170804     D  LOCALI2Ds                          Like(LOCALI2I75)
021900170804     D  PROV2Ds                            Like(PROV2I75  )
022000170804     D  STATO2Ds                           Like(STATO2I75 )
022100170804     D  IDPAES2Ds                          Like(IDPAES2I75)
022200170804     D  INDIR3Ds                           Like(INDIR3I75 )
022300170804     D  NRCIV3Ds                           Like(NRCIV3I75 )
022400170804     D  CAP3Ds                             Like(CAP3I75   )
022500170804     D  LOCALI3Ds                          Like(LOCALI3I75)
022600170804     D  PROV3Ds                            Like(PROV3I75  )
022700170804     D  STATO3Ds                           Like(STATO3I75 )
022800170804     D  IDPAES3Ds                          Like(IDPAES3I75)
022900170804     D  STATO4Ds                           Like(STATO4I75 )
023000170804     D  IDPAES4Ds                          Like(IDPAES4I75)
023100170804     D  PIVA4Ds                            Like(PIVA4I75  )
023200170804     D  TPN4Ds                             Like(TPN4I75   )
023300170804     D  COGRAG4Ds                          Like(COGRAG4I75)
023400170804     D  DENOM4Ds                           Like(DENOM4I75 )
023500170804     D  NOME4Ds                            Like(NOME4I75  )
023600170804     D  COGNOM4Ds                          Like(COGNOM4I75)
023700000000     D*-------------
023800000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
023900000000     D* alla funzione personalizzata
024000170804    >D $SaveDS         S                   like(SDGSI75DS)
024100000000     D*-------------
024200000000     D DSFMT           DS           512
024300000000     D  $TASTO               369    369
024400000000     D  NRG                  370    370
024500000000     D  NCL                  371    371
024600000000      * riga cursore window
024700000000     D  NRiWin               382    382
024800000000      * colonna cursore window
024900000000     D  NCoWin               383    383
025000000000     D  SFLNRR               378    379B 0
025100000000     D*-------------
025200000000     D* posizione cursore
025300000000     D CURSOR          DS
025400000000     D  RIRI                   1      2B 0 INZ
025500000000     D  R$$                    2      2
025600000000     D  COCO                   3      4B 0 INZ
025700000000     D  C$$                    4      4
025800000000     D*-------------
025900000000     D* Costanti
026000000000     D* nr.di righe del sfl (SFLPAG)
026100000000    >D SFLPAG          C                   CONST(9999)
026200000000     D*-------------
026300000000     D* Tasti di funzione
026400000000     D F01             C                   CONST(X'31')
026500000000     D F02             C                   CONST(X'32')
026600000000     D F03             C                   CONST(X'33')
026700000000     D F04             C                   CONST(X'34')
026800000000     D F05             C                   CONST(X'35')
026900000000     D F06             C                   CONST(X'36')
027000000000     D F07             C                   CONST(X'37')
027100000000     D F08             C                   CONST(X'38')
027200000000     D F09             C                   CONST(X'39')
027300000000     D F10             C                   CONST(X'3A')
027400000000     D F11             C                   CONST(X'3B')
027500000000     D F12             C                   CONST(X'3C')
027600000000     D F13             C                   CONST(X'B1')
027700000000     D F14             C                   CONST(X'B2')
027800000000     D F15             C                   CONST(X'B3')
027900000000     D F16             C                   CONST(X'B4')
028000000000     D F17             C                   CONST(X'B5')
028100000000     D F18             C                   CONST(X'B6')
028200000000     D F19             C                   CONST(X'B7')
028300000000     D F20             C                   CONST(X'B8')
028400000000     D F21             C                   CONST(X'B9')
028500000000     D F22             C                   CONST(X'BA')
028600000000     D F23             C                   CONST(X'BB')
028700000000     D F24             C                   CONST(X'BC')
028800000000     D ENTER           C                   CONST(X'F1')
028900000000     D ROLDWN          C                   CONST(X'F4')
029000000000     D ROLLUP          C                   CONST(X'F5')
029100000000     D*-------------
029200000000     D* Attributi video
029300000000     D Normale         C                   Const(X'32')
029400000000     D Protetto        C                   Const(X'A0')
029500000000     D Annul           C                   CONST(X'A1')
029600000000     D*-------------
029700000000     D* Sottotitolo di testata
029800000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
029900000000     D                                          COS0648')
030000000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
030100000000     D                                          SDC0001')
030200170804     D*-------------
030300170804     D* Sottotitolo di testata
030400170804     D* "Dettaglio"
030500170804     D SOTTITT1        C                   CONST('PROMSG    *LIBL-
030600170804     D                                          COS0648')
030700000000     D*-------------
030800000000     D* Variabili per gestione videate
030900000000     D $Gest           S              2A
031000000000     D $Fine           S              1A
031100000000     D $InzD1          S              1A
031200000000     D $InzS1          S              1A
031300000000     D $InzD2          S              1A
031400000000     D $LastG          S              2A
031500000000     D $LastV          S              2A
031600000000     D $EndFile        S              1A
031700000000     D $Esci           S              1A
031800000000     D $RcdOK          S              1A
031900000000     D $Unique         S              1A
032000000000     D*-------------
032100000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
032200000000     D S1Nrr           S                   like(C1Rcd)
032300000000     D WSfl            S                   like(C1Nrr)
032400000000     D WMax            S                   like(C1Rcd)
032500000000     D WPag            S                   like(C1Rcd)
032600000000     D WInzS1          S                   like($InzS1)
032700170804     D WOpr            S                   like(OprI75)
032800000000     D CurR            S              3S 0
032900000000     D CurC            S              3S 0
033000000000     D Win             S             99A
033100000000     D W1Pos           S                   like(C1Pos)
033200000000     D WINKR           S              1A
033300000000     D WExf            S              1A
033400000000     D WSogg           S                   Like(DVSSogg)
033500170804     D WStatoHd4       S                   Like(CSTStato1)
033501170810     D SAVOpzI75       S                   like(OPZI75)
033600000000     D*-------------
033700000000     D* Indice della schiera tasti funzione
033800000000     D PosFun          S              2  0
033900000000     D*-------------
034000000000     D*  Indici
034100000000     D X               S              5S 0
034200000000     D Y               S              5S 0
034300000000     D* Indice della schiera per le opzioni personalizzate
034400000000     D PosOpz          S              2  0
034500000000     D* numero max opzioni
034600000000     D NumOpz          S              2  0 Inz($DIMOP)
034700000000     D* numero effettive opzioni
034800000000     D OpzLun          S              2  0
034900000000     D*-------------
035000000000     D* ricerca soggetti
035100000000     D ANA040DS      E DS                  INZ
035200170804     D*-------------
035300170804     D Esito           S              1
035400170804     D GesMsg          S              1
035500170804     D Output          S           4000
035600170804     D LenOut          S              9B 0
035700170804     D Struttura       S             10
035800000000     D*-------------
035900000000     D* Campi di lavoro
036000170804     D UDateISO        S               D
036100170804     D WKcc            S                   Like(CSTKcc )
036200170804     D WKsc            S                   Like(CSTKsc )
036300000000     D*-------------
036400000000     D* Reperimento nome PGM
036500000000     D STATUS         SDS           333
036600000000     D  DSPGM            *PROC
036700000000     D  PARMS            *PARMS
036800000000     D*-------------
036900000000     D* DS esterna per dati soggetti
037000000000     D DVASOG        E DS                  INZ
037100000000     D*-------------
037200000000     D* Definizioni parametri driver anagrafici
037300000000     D Dvaric          S              1
037400000000     D DvaSocieta      S              3
037500000000     D DvaUnita        S              8
037600000000     D DvaStrutt       S             10
037700000000     D DvaDtRif        S               D
037800000000     D DvaLenOut       S              9B 0
037900000000     D DvaSnatura      S              1
038000000000     D DvaCodice       S              8
038100000000     D DvaLineav       S              3
038200000000     D DvaFiliale      S              3  0
038300000000     D DvaSogg         S              8
038400000000     D DvaTpInd        S              2
038500000000     D DvaCdInd        S              3
038600000000     D DvaErrore       S              1
038700000000     D DvaOutput       S           4000
038800000000     C*----------------------------------------------------*
038900000000     C*                MAIN LINE PROGRAM
039000000000     C*----------------------------------------------------*
039100000000     C*
039200000000     C* inizializzazione variabili
039300000000     C                   EXSR      INZVAR
039400000000     C*
039500000000     C     $FINE         DOWEQ     *OFF
039600000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
039700000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
039800000000     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
039900000000     C                   ENDCS
040000000000     C                   ENDDO
040100000000     C* fine programma
040200000000     C                   EXSR      ENDPGM
040300000000     C*
040400000000     C/EJECT
040500000000     C************************************************************
040600000000     C* FINE PROGRAMMA
040700000000     C************************************************************
040800000000     C     ENDPGM        BEGSR
040900110825     C*
041000000000     C* passaggio dati al pgm chiamante
041100170804    >C                   MOVEL     WOPR          OPRI75                                     ne S1
041200000000     C*
041300000000     C* Se pgm Guida non è stato richiamato posso chiudere in LR
041400170804    >C     OPZI75        IFEQ      *BLANK
041500000000     C                   SETON                                            LR
041600000000     C* altrimenti chiudo in RT
041700000000     C                   ELSE
041800000000     C                   SETON                                            RT
041900000000     C                   ENDIF
042000000000     C                   RETURN
042100000000     C*
042200000000     C                   ENDSR
042300000000     C/EJECT
042400000000     C************************************************************
042500000000     C* RICERCHE
042600000000     C************************************************************
042700000000     C     SEARCH        BEGSR
042800000000     C*
042900000000     C* determino Riga/Colonna del cursore
043000000000     C                   MOVE      NRG           R$$
043100000000     C                   MOVE      NCL           C$$
043200000000     C                   Z-ADD     RIRI          CURR
043300000000     C                   Z-ADD     COCO          CURC
043400000000     C*
043500000000     C                   EXSR      CallAllinea
043600000000     C*
043700000000   B1C                   SELECT
043800000000     C*
043900000000     C*  Stato
044000170804     C     H1NMFL        WHENEQ    'D1STATO1'
044100170804     C                   EXSR      F4_Stato1
044200000000     C*
044300000000     C*  Codice IVA
044400000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
044500000000     C                   EXSR      F4_PartIva
044600000000     C*
044700000000     C*  Codice fiscale
044800000000     C     H1NMFL        WHENEQ    'D1CDFISC'
044900000000     C                   EXSR      F4_CdFisc
045000000000     C*
045100000000     C*  Soggetto
045200000000     C     H1NMFL        WHENEQ    'D1SOGG'
045300000000     C                   EXSR      F4_Sogg
045400170804     C*
045500170804     C*  Causale
045600170804     C     H1NMFL        WHENEQ    'D1CAUSALE'
045700170804     C                   EXSR      F4_Causale
045800170804     C*
045900170804     C*  Tipo record
046000170804     C     H1NMFL        WHENEQ    'D1TPREC  '
046100170804     C                   EXSR      F4_TpRec
046200000000     C*
046300000000     C* FMTD1 - Dati attivi/non attivi
046400000000     C     H1NMFL        WHENEQ    'D1ANN  '
046500000000     C                   EXSR      F4_Ann
046600000000     C*
046700000000     C* FMTD1 - Tipo annullamento
046800000000     C     H1NMFL        WHENEQ    'D1TPANN'
046900000000     C                   EXSR      F4_TpAnn
047000170804     C*
047100170804     C*  Trimestre
047200170804     C     H1NMFL        WHENEQ    'D1TRIM'
047300170804     C                   EXSR      F4_Trim
047400170804     C*
047500170804     C*  Natura soggetto
047600170804     C     H1NMFL        WHENEQ    'D1CF'
047700170804     C                   EXSR      F4_CF
047800000000     C*
047900000000     C* ricerca non abilitata in questa posizione
048000000000   X1C                   OTHER
048100000000     C                   SETON                                        9899
048200000000     C*
048300000000   E1C                   ENDSL
048400000000     C*
048500000000     C* imposto pos. del cursore
048600000000     C                   Z-ADD     CURR          H1RIGA
048700000000     C                   Z-ADD     CURC          H1COLO
048800000000     C*
048900000000     C                   MOVE      *HIVAL        $LASTV
049000000000     C*
049100000000     C                   ENDSR
049200000000     C/EJECT
049300000000     C************************************************************
049400000000     C* Richiamo programma per allineare i codici
049500000000     C************************************************************
049600000000     C     CallAllinea   BEGSR
049700000000     C*
049800000000     C     D1SOGG        IFNE      *BLANK
049900000000     C                   CALLb     'XALLINEA'
050000000000     C                   PARM                    D1SOGG
050100000000     C                   PARM      8             LEN               5 0
050200000000     C                   PARM                    XSCINP
050300000000     C                   ENDIF
050400000000     C*
050500000000     C                   ENDSR
050600000000      /EJECT
050700000000     C************************************************************
050800000000     C* Ricerca codice Stato del conto
050900000000     C************************************************************
051000170804     C     F4_Stato1     BEGSR
051100000000     C*
051200000000     C                   Reset                   XTabDS
051300000000     C*
051400000000     C                   Move      '01'          XTaOpz
051500000000     C                   Move      XScSoc        XTaAzi
051600000000     C                   Move      XScLin        XTaLin
051700000000     C                   Z-add     Curr          XTaRig
051800000000     C                   Z-add     Curc          XTaCol
051900000000     C                   Move      *Blank        XTaKey
052000000000     C*
052100000000     C                   Clear                   KPJBU
052200000000     C                   MoveL     XTabDS        KPJBU
052300000000     C*
052400170804     C                   CALL      'ANGG12R'
052500000000     C                   PARM                    KPJBA
052600000000     C                   MoveL     KPJBU         XTabDS
052700000000     C*
052800000000     C                   if        XTaErr     <> '1'  and
052900000000     C                             XTaKey     <> *Blank
053000170804     C                   Move      XTaKey        D1Stato1
053100000000     C                   endif
053200000000     C*
053300000000     C                   ENDSR
053400000000      /EJECT
053500170804     C************************************************************
053600170804     C* Ricerca Trimestre di competenza
053700170804     C************************************************************
053800170804     C     F4_Trim       BEGSR
053900170804     C*
054000170804     C                   Reset                   ANA964DS
054100170804     C*
054200170804     C                   MoveL     '01'          Opz964
054300170804     C                   MoveL     'SDGCST000'   Trc964
054400170804     C                   MoveL     'CSTTRIM  '   Cam964
054500170804     C                   MoveL     *On           All964
054600170804     C*
054700170804     C                   MoveL     ANA964DS      KPJBU
054800170804     C                   Call      'ANA964R'
054900170804     C                   Parm                    KPJBA
055000170804     C                   MoveL     KPJBU         ANA964DS
055100170804     C*
055200170804     C                   If        Ret964     <> '2'
055300170804     C                   MoveL     Val964        D1Trim
055400170804     C                   EndIf
055500170804     C*
055600170804     C                   EndSr
055700170804      /EJECT
055800170804     C************************************************************
055900170804     C* Ricerca natura del conto
056000170804     C************************************************************
056100170804     C     F4_CF         BEGSR
056200170804     C*
056300170804     C                   Reset                   ANA964DS
056400170804     C*
056500170804     C                   MoveL     '01'          Opz964
056600170804     C                   MoveL     'SDGCST000'   Trc964
056700170804     C                   MoveL     'CSTCF    '   Cam964
056800170804     C                   MoveL     *On           All964
056900170804     C*
057000170804     C                   MoveL     ANA964DS      KPJBU
057100170804     C                   CALL      'ANA964R'
057200170804     C                   PARM                    KPJBA
057300170804     C                   MoveL     KPJBU         ANA964DS
057400170804     C*
057500170804     C                   if        Ret964     <> '2'
057600170804     C                   MoveL     Val964        D1CF
057700170804     C                   endif
057800170804     C*
057900170804     C                   ENDSR
058000170804      /EJECT
058100000000     C************************************************************
058200000000     C* Ricerca codice IVA
058300000000     C************************************************************
058400000000     C     F4_PartIva    BEGSR
058500000000     C*
058600000000     C                   RESET                   ANA040DS
058700000000     C*
058800000000     C                   MOVEL     '01'          OPZ040
058900000000     C                   MoveL     D1PartIva     IVA040
059000000000     C                   MOVEL     ANA040DS      KPJBU
059100000000     C                   CALL      'ANA040R'
059200000000     C                   PARM                    KPJBA
059300000000     C*
059400000000     C                   MOVEL     KPJBU         ANA040DS
059500000000     C*
059600000000     C                   if        Ret040     <> '2'
059700000000     C*
059800000000     C                   MoveL     Cod040        WSogg
059900000000     C                   EXSR      CallDvaSog
060000000000     C*
060100000000     C                   if        DVAErrore  <> '1'
060200000000     C*
060300000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
060400000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
060500000000     C*
060600000000     C                   MoveL     DVSPartIVA    D1PartIva
060700000000     C*
060800000000     C                   endif
060900000000     C*
061000000000     C                   endif
061100000000     C*
061200000000     C                   ENDSR
061300000000      /EJECT
061400000000     C************************************************************
061500000000     C* Ricerca codice fiscale
061600000000     C************************************************************
061700000000     C     F4_CdFisc     BEGSR
061800000000     C*
061900000000     C                   RESET                   ANA040DS
062000000000     C*
062100000000     C                   MOVEL     '01'          OPZ040
062200000000     C                   MoveL     D1CdFisc      CdF040
062300000000     C                   MOVEL     ANA040DS      KPJBU
062400000000     C                   CALL      'ANA040R'
062500000000     C                   PARM                    KPJBA
062600000000     C*
062700000000     C                   MOVEL     KPJBU         ANA040DS
062800000000     C*
062900000000     C                   if        Ret040     <> '2'
063000000000     C*
063100000000     C                   MoveL     Cod040        WSogg
063200000000     C                   EXSR      CallDvaSog
063300000000     C*
063400000000     C                   if        DVAErrore  <> '1'
063500000000     C*
063600000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
063700000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
063800000000     C*
063900000000     C                   MoveL     DVSCdFisc     D1CdFisc                       *
064000000000     C*
064100000000     C                   endif
064200000000     C*
064300000000     C                   endif
064400000000     C*
064500000000     C                   ENDSR
064600000000     C/EJECT
064700000000     C************************************************************
064800000000     C* Ricerca codice soggetto
064900000000     C************************************************************
065000000000     C     F4_Sogg       BEGSR
065100000000     C*
065200000000     C                   RESET                   ANA040DS
065300000000     C*
065400000000     C                   MOVEL     '01'          OPZ040
065500000000     C                   MoveL     D1Sogg        Cod040
065600000000     C                   MOVEL     ANA040DS      KPJBU
065700000000     C                   CALL      'ANA040R'
065800000000     C                   PARM                    KPJBA
065900000000     C*
066000000000     C                   MOVEL     KPJBU         ANA040DS
066100000000     C*
066200000000     C                   if        Ret040     <> '2'
066300000000     C                   MoveL     Cod040        D1Sogg
066400000000     C                   endif
066500000000     C*
066600000000     C                   ENDSR
066700000000     C/EJECT
066800170804     C************************************************************
066900170804     C* Ricerca codice causale
067000170804     C************************************************************
067100170804     C     F4_Causale    BEGSR
067200170804     C*
067300170804     C                   Reset                   ANA070DS
067400170804     C*
067500170804     C                   Move      '01'          Opz070
067600170804     C                   MOVE      '1'           Tpc070
067700170804     C                   CLEAR                   KPJBU
067800170804     C                   MoveL     ANA070DS      KPJBU
067900170804     C*
068000170804     C                   CALL      'ANA070R'
068100170804     C                   PARM                    KPJBA
068200170804     C*
068300170804     C                   MoveL     KPJBU         ANA070DS
068400170804     C*
068500170804     C     Cau070        IFNE      *BLANK
068600170804     C                   MoveL     Cau070        D1Causale
068700170804     C                   ENDIF
068800170804     C*
068900170804     C                   ENDSR
069000170804      /EJECT
069100170804     C************************************************************
069200170804     C* Ricerca Tipo Record
069300170804     C************************************************************
069400170804     C     F4_TpRec      BEGSR
069500170804     C*
069600170804     C                   Reset                   ANA964DS
069700170804     C*
069800170804     C                   MoveL     '01'          Opz964
069900170804     C                   MoveL     'SDGCST000'   Trc964
070000170804     C                   MoveL     'CSTTPREC '   Cam964
070100170804     C                   MoveL     *On           All964
070200170804     C*
070300170804     C                   MoveL     ANA964DS      KPJBU
070400170804     C                   Call      'ANA964R'
070500170804     C                   Parm                    KPJBA
070600170804     C                   MoveL     KPJBU         ANA964DS
070700170804     C*
070800170804     C                   If        Ret964     <> '2'
070900170804     C                   MoveL     Val964        D1TpRec
071000170804     C                   EndIf
071100170804     C*
071200170804     C                   EndSr
071300170804      /EJECT
071400000000     C************************************************************
071500000000     C* Ricerca dati attivi/non attivi
071600000000     C************************************************************
071700000000     C     F4_Ann        BEGSR
071800000000     C*
071900000000     C                   reset                   ana964ds
072000000000     C*
072100000000     C                   movel     '01'          Opz964
072200000000     C                   movel     'ANDIZ000 '   Trc964
072300000000     C                   movel     'DSPATT   '   Cam964
072400000000     C*
072500000000     C                   movel     ana964ds      Kpjbu
072600000000     C                   call      'ANA964R'
072700000000     C                   parm                    Kpjba
072800000000     C                   movel     Kpjbu         Ana964ds
072900000000     C*
073000000000     C                   if        Ret964     <> '2'
073100000000     C                   movel     Val964        D1Ann
073200000000     C                   endif
073300000000     C*
073400000000     C                   ENDSR
073500000000     C/EJECT
073600000000     C************************************************************
073700000000     C* Ricerca tipo annullamento
073800000000     C************************************************************
073900000000     C     F4_TpAnn      BEGSR
074000000000     C*
074100000000     C                   reset                   ana964ds
074200000000     C*
074300000000     C                   movel     '01'          Opz964
074400170804     C                   movel     'SDGCST000'   Trc964
074500170804     C                   movel     'CSTTPANN'    Cam964
074600000000     C*
074700000000     C                   movel     ana964ds      Kpjbu
074800000000     C                   call      'ANA964R'
074900000000     C                   parm                    Kpjba
075000000000     C                   movel     Kpjbu         Ana964ds
075100000000     C*
075200000000     C                   if        Ret964     <> '2'
075300000000     C                   movel     Val964        D1TpAnn
075400000000     C                   endif
075500000000     C*
075600000000     C                   ENDSR
075700000000     C/EJECT
075800000000     C************************************************************
075900000000     C* RIEMPIMENTO OPZIONI
076000000000     C************************************************************
076100000000     C     RIEOPZ        BEGSR
076200000000     C*
076300000000     C                   CALLB     'X04OPMSG'
076400000000     C* input
076500000000     C                   PARM                    $TX
076600000000     C                   PARM                    $FL
076700000000     C                   PARM                    WCOZ              3 0
076800000000     C                   PARM                    WMXO              3 0
076900000000     C* output
077000000000     C                   PARM                    WOPZ1            76
077100000000     C                   PARM                    WOPZ2            76
077200000000     C                   PARM                    FF23              1
077300000000     C* input
077400000000     C                   PARM                    $L01              3 0
077500000000     C                   PARM                    $L02              3 0
077600000000     C*
077700000000     C                   PARM                    $ALLOPT         960
077800000000     C*
077900000000     C                   ENDSR
078000000000     C/EJECT
078100000000     C************************************************************
078200000000     C* RIEMPIMENTO TASTI FUNZIONE
078300000000     C************************************************************
078400000000     C     RIEKEY        BEGSR
078500000000     C*
078600000000     C                   CALLB     'XKEYMSG'
078700000000     C* input
078800000000     C                   PARM                    $FX
078900000000     C                   PARM                    $FC
079000000000     C                   PARM                    $FR
079100000000     C                   PARM                    $ULK              3 0
079200000000     C* output
079300000000     C                   PARM                    $KEY1            79
079400000000     C                   PARM                    $KEY2            79
079500000000     C* input
079600000000     C                   PARM                    $L01              3 0
079700000000     C                   PARM                    $L02              3 0
079800000000     C*
079900000000     C                   PARM                    $ALLFUNCT       480
080000000000     C*
080100000000     C                   ENDSR
080200000000     C/EJECT
080300000000     C************************************************************
080400000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
080500000000     C************************************************************
080600000000     C     GESPIE        BEGSR
080700000000     C*
080800000000     C                   SELECT
080900000000     C* Parzializzazioni
081000000000     C     $GEST         WHENEQ    'D1'
081100000000     C     $ULKD1        IFEQ      0
081200000000     C* riempimento opzioni
081300000000     C                   EXSR      F24D1
081400000000     C                   ELSE
081500000000     C                   WRITE     FMTZ1
081600000000     C                   ENDIF
081700000000     C* Lista
081800000000     C     $GEST         WHENEQ    'S1'
081900000000     C     $ULKS1        IFEQ      0
082000000000     C                   EXSR      F24S1
082100000000     C                   ELSE
082200000000     C                   WRITE     FMTZ2
082300000000     C                   ENDIF
082400000000     C* Modifica valori assunti
082500000000     C     $GEST         WHENEQ    'D2'
082600000000     C     $ULKD2        IFEQ      0
082700000000     C                   EXSR      F24D2
082800000000     C                   ELSE
082900000000     C                   WRITE     FMTZ3
083000000000     C                   ENDIF
083100000000     C                   ENDSL
083200000000     C*
083300000000     C                   ENDSR
083400000000     C/EJECT
083500000000     C************************************************************
083600000000     C* GESTIONE PARZIALIZZAZIONI
083700000000     C************************************************************
083800000000     C     GESD1         BEGSR
083900000000     C*
084000000000     C* inizializzazione videata
084100000000     C     $INZD1        IFEQ      *ON
084200000000     C                   EXSR      INZD1
084300000000     C                   MOVE      *OFF          $INZD1
084400000000     C                   ENDIF
084500000000     C* emissione piede videata se proveniente da altra
084600000000     C* salvataggio valore formato di provenienza
084700000000     C     $LASTV        IFNE      'D1'
084800000000     C*
084900000000     C* recupero il sottotitolo (se c'è)
085000000000     C                   Clear                   D1TIT
085100000000     C     SOTTITD1      IFNE      *BLANK
085200000000     C                   MOVEL     SOTTITD1      IDMSG
085300000000     C                   CALLB     'XRTVM'
085400000000     C                   PARM                    IDMSG            27
085500000000     C                   PARM                    TXTMSG          100
085600000000     C                   PARM                    ERRMSG            1
085700000000     C                   PARM      *Off          CTRMSG            1            centratura
085800000000     C                   PARM      15            LENMSG            3 0          lun output.
085900000000     C     ERRMSG        IFNE      *ON
086000000000     C                   MOVEL     TXTMSG        D1TIT
086100000000     C                   ELSE
086200000000     C                   MOVEL     *ALL'?'       D1TIT
086300000000     C                   ENDIF
086400000000     C                   ENDIF
086500000000     C*
086600000000     C* salvo stato F17 per sapere cosa fare in caso di F12
086700000000     C                   MOVE      *INKR         WINKR
086800000000     C                   WRITE     FMTT1
086900000000     C                   EXSR      GESPIE
087000000000     C                   MOVE      $LASTV        $LASTG
087100000000     C                   MOVE      'D1'          $LASTV
087200000000     C                   ENDIF
087300000000     C*
087400000000     C* se esistono errori sulla videata
087500000000     C* emetto la write del formato a indicatori spenti per vedere
087600000000     C* le eventuali decodifiche
087700000000     C     *IN99         IFEQ      *ON
087800000000     C                   MOVEA     *IN           WIN
087900000000     C                   MOVE      *ALL'0'       IN5098           49
088000000000     C                   MOVEA     IN5098        *IN(50)
088100000000     C                   WRITE     FMTD1
088200000000     C                   MOVEA     WIN           *IN
088300000000     C                   ENDIF
088400000000     C*
088500000000     C                   EXFMT     FMTD1
088600000000     C*
088700000000     C* azzero la posizione del cursore
088800000000     C                   Z-ADD     0             H1RIGA
088900000000     C                   Z-ADD     0             H1COLO
089000000000     C*
089100000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
089200000000     C* INPUT
089300000000     C* $FC1 = Abilitazioni videata D1
089400000000     C* OUTPUT
089500000000     C* *IN97 + *IN99 Errori
089600000000     C                   MOVEA     $FC1          $FC
089700000000     C                   EXSR      CTRKEY
089800000000     C*
089900000000     C     *IN99         IFEQ      *OFF
090000000000     C*
090100000000     C                   SELECT
090200000000     C* F2=Decodifica
090300000000     C     $TASTO        WHENEQ    F02
090400000000     C                   EXSR      F02D1
090500000000     C* F3=Fine
090600000000     C     $TASTO        WHENEQ    F03
090700000000     C                   EXSR      F03D1
090800000000     C* F4=Ricerca
090900000000     C     $TASTO        WHENEQ    F04
091000000000     C                   EXSR      SEARCH
091100000000     C* F5=Refresh
091200000000     C     $TASTO        WHENEQ    F05
091300000000     C                   EXSR      F05D1
091400000000     C* F8=Successivo
091500000000     C     $TASTO        WHENEQ    F08
091600000000     C                   EXSR      F08D1
091700000000     C* F10=Immissione
091800000000     C     $TASTO        WHENEQ    F10
091900000000     C                   EXSR      F10D1
092000000000     C* F12=Ritorno
092100000000     C     $TASTO        WHENEQ    F12
092200000000     C                   EXSR      F12D1
092300000000     C* F18=Modifica valori assunti
092400000000     C     $TASTO        WHENEQ    F18
092500000000     C                   EXSR      F18D1
092600000000     C* F24=Altri tasti
092700000000     C     $TASTO        WHENEQ    F24
092800000000     C                   EXSR      F24D1
092900000000     C*
093000000000     C                   OTHER
093100000000     C                   EXSR      CTRD1
093200000000     C     *IN99         IFEQ      *OFF
093300000000     C* definisco l'ordinamento
093400000000     C                   EXSR      DEFORD
093500000000     C* richiamo la lista
093600000000     C                   MOVE      'S1'          $GEST
093700000000     C                   MOVE      *ON           $INZS1
093800000000     C                   MOVE      *BLANK        W1POS
093900000000     C                   ENDIF
094000000000     C*
094100000000     C                   ENDSL
094200000000     C*
094300000000     C                   ENDIF
094400000000     C*
094500000000     C                   ENDSR
094600000000     C/EJECT
094700000000     C************************************************************
094800000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
094900000000     C************************************************************
095000000000     C     INZD1         BEGSR
095100000000     C*
095200000000     C                   CLEAR                   FMTD1
095300000000     C*
095400000000     C* Se pgm  richiamato con parametri forniti , li imposto nelle
095500000000     C*    parzializzazioni
095600000000     C                   SELECT
095700000000     C* il pgm è richiamato con chiave univoca per gestione POSTA
095800000000     C     $Unique       WHENEQ    *ON
095900000000     C* il pgm è richiamato e
096000000000     C* sono stati passati dei valori di parzializzazione ma non
096100000000     C* la chiave univoca
096200170804     C     OPZI75        WHENNE    *BLANK
096300000000     C* imposto i valori passati sui campi a video
096400170804     C                   MOVEL     AnnoI75       D1Anno
096500170804     C                   MOVEL     TrimI75       D1Trim
096600170804     C                   MOVEL     TpRecI75      D1TpRec
096700170804     C                   MOVEL     CFI75         D1CF
096800170804     C                   MOVEL     Stato1I75     D1Stato1
096900170804     C                   MOVEL     PartIvaI75    D1PartIva
097000170804     C                   MOVEL     CdFiscI75     D1CdFisc
097100170804     C                   MOVEL     SoggI75       D1Sogg
097200170804     C                   MOVEL     CausaleI75    D1Causale
097300170804     C                   MOVEL     AnnI75        D1Ann
097400170804     C                   MOVEL     TpAnnI75      D1TpAnn
097500000000     C* il pgm non è richiamato
097600170804     C     OPZI75        WHENEQ    *BLANK
097700000000     C                   Eval      D1Ann       = *Blank
097800130906     C                   ENDSL
097900130906     C*
098000130906     C                   ENDSR
098100000000      /EJECT
098200130906     C************************************************************
098300130906     C* GESTIONE F02 VIDEO D1
098400130906     C************************************************************
098500000000     C     F02D1         BEGSR
098600000000     C*
098700000000     C* per decodificare la videata richiamo la routine dei ctrl
098800000000     C* e poi disattivo gli eventuali messaggi di errore
098900000000     C                   EXSR      CTRD1
099000000000     C*
099100000000     C                   MOVE      *ALL'0'       IN5098
099200000000     C                   MOVEA     IN5098        *IN(50)
099300000000     C*
099400000000     C                   ENDSR
099500000000     C/EJECT
099600000000     C************************************************************
099700000000     C* GESTIONE F03 VIDEO D1
099800000000     C************************************************************
099900000000     C     F03D1         BEGSR
100000000000     C*
100100000000     C                   MOVE      *ON           $FINE
100200170804    >C                   MOVE      '1'           RETI75
100300000000     C* fine programma
100400000000     C                   EXSR      ENDPGM
100500000000     C*
100600000000     C                   ENDSR
100700000000     C/EJECT
100800000000     C************************************************************
100900000000     C* GESTIONE F05 VIDEO D1
101000000000     C************************************************************
101100000000     C     F05D1         BEGSR
101200000000     C*
101300000000     C                   MOVE      *ON           $INZD1
101400000000     C*
101500000000     C                   ENDSR
101600000000     C/EJECT
101700000000     C************************************************************
101800000000     C* GESTIONE F08 VIDEO D1
101900000000     C************************************************************
102000000000     C     F08D1         BEGSR
102100000000     C*
102200000000     C                   MOVE      *ON           $FINE
102300170804    >C                   MOVE      '0'           RETI75
102400000000     C*
102500000000     C                   ENDSR
102600000000     C/EJECT
102700000000     C************************************************************
102800000000     C* GESTIONE F10 VIDEO D1
102900000000     C************************************************************
103000000000     C     F10D1         BEGSR
103100000000     C*
103200170804    >C                   RESET                   SDGSI76DS
103300170804    >C                   MOVE      xscsoc        SocietaI76
103400170804    >C                   MOVEL     '01'          OPZI76
103500170804    >C     CMTI75        IFEQ      '1'
103600170804    >C                   MOVEL     '1'           CMTI76
103700000000     C                   ELSE
103800170804    >C                   MOVEL     '0'           CMTI76
103900000000     C                   ENDIF
104000170804     C*
104100170804     C                   Eval      AnnoI76     = D1Anno
104200170804     C                   Eval      TrimI76     = D1Trim
104300170804     C*
104400170804     C                   Eval      AnnI76      = D1Ann
104500170804     C                   Eval      TpAInpI76   = D1TpAnn
104600170804     C*
104700170804     C                   Eval      TpComI76    = TpComI75
104800170804     C                   Eval      TpRecI76    = D1TpRec
104900170804     C                   Eval      CfI76       = D1Cf
105000170804     C                   Eval      SoggInpI76  = D1Sogg
105100170804     C                   Eval      CausInpI76  = D1Causale
105200170804     C                   Eval      PartivaI76  = D1Partiva
105300170804     C                   Eval      CdFiscI76   = D1CdFisc
105400170804     C*
105500000000     C* passare anche le parzializzazioni ricevute relative alla chiave
105600000000     C* univoca :
105700000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
105800170804    >C                   IF        OPZI75 <> *Blank
105900170804     C                   Eval      Stato1I76   = Stato1I75
106000170804     C                   Eval      IdPaes1I76  = IdPaes1I75
106100170804     C                   Eval      PIVA1I76    = PIVA1I75
106200170804     C                   Eval      CdFisc1I76  = CdFisc1I75
106300170804     C                   Eval      CogRag1I76  = CogRag1I75
106400170804     C                   Eval      TpN1I76     = TpN1I75
106500170804     C                   Eval      Denom1I76   = Denom1I75
106600170804     C                   Eval      Cognom1I76  = Cognom1I75
106700170804     C                   Eval      Nome1I76    = Nome1I75
106800170804     C*
106900170804     C                   Eval      Indir2I76   = Indir2I75
107000170804     C                   Eval      NrCiv2I76   = NrCiv2I75
107100170804     C                   Eval      CAP2I76     = CAP2I75
107200170804     C                   Eval      Locali2I76  = Locali2I75
107300170804     C                   Eval      Prov2I76    = Prov2I75
107400170804     C                   Eval      Stato2I76   = Stato2I75
107500170804     C                   Eval      IdPaes2I76  = IdPaes2I75
107600170804     C*
107700170804     C                   Eval      Indir3I76   = Indir3I75
107800170804     C                   Eval      NrCiv3I76   = NrCiv3I75
107900170804     C                   Eval      CAP3I76     = CAP3I75
108000170804     C                   Eval      Locali3I76  = Locali3I75
108100170804     C                   Eval      Prov3I76    = Prov3I75
108200170804     C                   Eval      Stato3I76   = Stato3I75
108300170804     C                   Eval      IdPaes3I76  = IdPaes3I75
108400170804     C*
108500170804     C                   Eval      Stato4I76   = Stato4I75
108600170804     C                   Eval      IdPaes4I76  = IdPaes4I75
108700170804     C                   Eval      PIVA4I76    = PIVA4I75
108800170804     C                   Eval      CogRag4I76  = CogRag4I75
108900170804     C                   Eval      TpN4I76     = TpN4I75
109000170804     C                   Eval      Denom4I76   = Denom4I75
109100170804     C                   Eval      Cognom4I76  = Cognom4I75
109200170804     C                   Eval      Nome4I76    = Nome4I75
109300000000     C                   ENDIF
109400000000     C*
109500170804    >C                   CALL      'SDGSI76R'
109600000000     C                   PARM                    KPJBA
109700170804    >C                   PARM                    SDGSI76DS
109800000000     C* ritorno da PGM gestione
109900170804     C                   EXSR      RTNI76
110000000000     C*
110100000000     C                   MOVE      *HIVAL        $LASTV
110200000000     C*
110300000000     C                   ENDSR
110400000000     C/EJECT
110500000000     C************************************************************
110600000000     C* GESTIONE F12 VIDEO D1
110700000000     C************************************************************
110800000000     C     F12D1         BEGSR
110900000000     C*
111000000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
111100000000     C*  per F12 torno alla lista
111200000000     C     $LOP          IFEQ      2
111300000000     C     WINKR         OREQ      *ON
111400000000     C                   MOVE      'S1'          $GEST
111500000000     C*  altrimenti, per F12 torno a menu
111600000000     C                   ELSE
111700000000     C                   MOVE      *ON           $FINE
111800170804    >C                   MOVE      '2'           RETI75
111900000000     C                   ENDIF
112000000000     C*
112100000000     C                   ENDSR
112200000000     C/EJECT
112300000000     C************************************************************
112400000000     C* GESTIONE F18 VIDEO D1
112500000000     C************************************************************
112600000000     C     F18D1         BEGSR
112700000000     C*
112800000000     C                   MOVE      'D2'          $GEST
112900000000     C                   MOVEL     *ON           $INZD2
113000000000     C*
113100000000     C                   ENDSR
113200000000     C/EJECT
113300000000     C************************************************************
113400000000     C* GESTIONE F24 VIDEO D1
113500000000     C* input
113600000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
113700000000     C* $FX1 =Testi tasti funzione
113800000000     C* $FC1 = Abilitazioni
113900000000     C* $FR1 = Inversioni di fondo
114000000000     C************************************************************
114100000000     C     F24D1         BEGSR
114200000000     C*
114300000000     C                   MOVEA     $FX1          $FX
114400000000     C                   MOVEA     $FC1          $FC
114500000000     C                   MOVEA     $FR1          $FR
114600000000     C                   Z-ADD     $ULKD1        $ULK
114700000000     C                   Z-ADD     $D1L01        $L01
114800000000     C                   Z-ADD     $D1L02        $L02
114900000000     C                   EXSR      RIEKEY
115000000000     C                   Z-ADD     $ULK          $ULKD1            3 0
115100000000     C                   MOVEA     $FC           $FC1
115200000000     C                   MOVEL     $KEY1         Z1KE1
115300000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
115400000000     C                   MOVEL     $KEY2         Z1KE2
115500000000     C                   WRITE     FMTZ1
115600000000     C*
115700000000     C*
115800000000     C                   ENDSR
115900000000     C/EJECT
116000000000     C************************************************************
116100000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
116200000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
116300000000     C* 93 94 96 97 98
116400000000     C************************************************************
116500000000     C     CTRD1         BEGSR
116600000000     C*
116700000000     C                   SETOFF                                       99
116800000000     C*
116900000000     C                   EXSR      CtrAnno
117000000000     C*
117100000000     C                   EXSR      CtrStato
117200000000     C*
117300000000     C                   EXSR      CtrSogg
117400000000     C*
117500000000     C                   EXSR      CtrAnn
117600000000     C*
117700000000     C                   ENDSR
117800000000     C/EJECT
117900000000     C***********************************************************
118000000000     C* Controllo anno di competenza
118100000000     C***********************************************************
118200000000     C     CtrAnno       BEGSR
118300000000     C*
118400000000     C                   if        D1Anno = 0
118500000000     C                   SetOn                                        5399
118600000000     C                   endif
118700000000     C*
118800000000     C                   ENDSR
118900000000     C/EJECT
119000000000     C***********************************************************
119100000000     C* Controllo Stato
119200000000     C***********************************************************
119300000000     C     CtrStato      BEGSR
119400000000     C*
119500170804     C                   Clear                   D1Stato1D
119600170804     C                   Clear                   ANGG12DS
119700000000     C*
119800170804     C                   if        D1Stato1   <> *Blank
119900000000     C*
120000000000     C                   RESET                   SDGTABDS
120100000000     C*
120200000000     C                   Eval      XTbRic      = '1'
120300000000     C                   Eval      XTbAzi      = XScSoc
120400000000     C                   Eval      XTbKey      = *Zero
120500170804     C                   Eval      XTbCod      = 'G12'
120600170804     C                   Move      D1Stato1      XTbKey
120700000000     C*
120800000000     C                   CALL      'SDGTABDR'
120900000000     C                   PARM                    SDGTABDS
121000000000     C*
121100000000     C                   if        XTbErr     <> '0'
121200000000     C                   SetOn                                        6899
121300000000     C*
121400000000     C                   else
121500170804     C                   MoveL     XTbUni        ANGG12DS
121600170804     C                   MoveL     DesG12        D1Stato1D
121700170804     C*
121800170804     C* Reperisco stato ISO da tabella HD4
121900170804     C                   movel     D1Stato1      WStatoHd4
122000170804     C                   Clear                   D1IdPaes1
122100170804     C                   Exsr      RepHd4
122200170804     C                   Movel     CodIso2Hd4    D1IdPaes1
122300000000     C*
122400000000     C                   endif
122500000000     C*
122600000000     C                   endif
122700000000     C*
122800000000     C                   ENDSR
122900000000     C/EJECT
123000170804     C***********************************************************
123100170804     C* Reperisco stato iso da tabella HD4
123200170804     C***********************************************************
123300170804     C     RepHd4        BEGSR
123400170804     C*
123500170804     C                   Clear                   SDGHD4DS
123600170804     C*
123700170804     C                   RESET                   SDGTABDS
123800170804     C*
123900170804     C                   Eval      XTbRic      = '1'
124000170804     C                   Eval      XTbAzi      = XScSoc
124100170804     C                   Eval      XTbKey      = *Zero
124200170804     C                   Eval      XTbCod      = 'HD4'
124300170804     C                   Move      WstatoHd4     XTbKey
124400170804     C*
124500170804     C                   CALL      'SDGTABDR'
124600170804     C                   PARM                    SDGTABDS
124700170804     C*
124800170804     C                   if        XTbErr     =  '0'
124900170804     C                   MoveL     XTbUni        SDGHd4DS
125000170804     C                   endif
125100170804     C*
125200170804     C*
125300170804     C                   ENDSR
125400170804     C/EJECT
125500170804     C***********************************************************
125600170804     C* Controllo trimestre
125700170804     C***********************************************************
125800170804     C     CtrTrim       BEGSR
125900170804     C*
126000170804     C                   Clear                   X07ValDs
126100170804     C*
126200170804     C                   Move      '1'           X07Ric
126300170804     C                   Move      *On           X07All
126400170804     C                   Movel     'SDGCST000'   X07TRc
126500170804     C                   Movel     'CSTTRIM   '  X07Cam
126600170804     C                   Movel     D1Trim        X07VAL
126700170804     C*
126800170804     C                   Callb     'X07VALR'
126900170804     C                   Parm                    X07ValDs
127000170804     C*
127100170804     C     X07ERR        IFEQ      *Off
127200170804     C                   else
127300170804     C                   SetOn                                        7799
127400170804     C                   endif
127500170804     C*
127600170804     C                   ENDSR
127700170804     C/EJECT
127800170804     C***********************************************************
127900170804     C* Controllo tipo soggetto
128000170804     C***********************************************************
128100170804     C     CtrCF         BEGSR
128200170804     C*
128300170804     C                   Clear                   D1CFD
128400170804     C                   Clear                   C1CFD
128500170804     C*
128600170804     C                   Clear                   X07ValDs
128700170804     C*
128800170804     C                   Move      '1'           X07Ric
128900170804     C                   Move      *On           X07All
129000170804     C                   Movel     'SDGCST000'   X07TRc
129100170804     C                   Movel     'CSTCF    '   X07Cam
129200170804     C                   Movel     D1CF          X07VAL
129300170804     C*
129400170804     C                   Callb     'X07VALR'
129500170804     C                   Parm                    X07ValDs
129600170804     C*
129700170804     C     X07ERR        IFEQ      *Off
129800170804     C                   MOVEL     X07DES        D1CFD
129900170804     C                   MOVEL     X07DES        C1CFD
130000170804     C                   else
130100170804     C                   SetOn                                        5499
130200170804     C                   endif
130300170804     C*
130400170804     C                   ENDSR
130500170804     C/EJECT
130600000000     C***********************************************************
130700000000     C* Controllo codice soggetto
130800000000     C***********************************************************
130900000000     C     CtrSogg       BEGSR
131000000000     C*
131100000000     C                   Clear                   D1SoggD
131200000000     C                   if        D1Sogg     <> *Blank
131300000000     C*
131400000000     C     D1SOGG        IFNE      *BLANK
131500000000     C                   CALLb     'XALLINEA'
131600000000     C                   PARM                    D1SOGG
131700000000     C                   PARM      8             LEN               5 0
131800000000     C                   PARM                    XSCINP
131900000000     C                   ENDIF
132000000000     C*
132100000000     C                   MoveL     D1Sogg        WSogg
132200000000     C                   EXSR      CallDvaSog
132300000000     C*
132400000000     C                   if        DVAErrore  <> '1'
132500000000     C*
132600000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
132700000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
132800000000     C*
132900000000     C                   MoveL     DVSDes        D1SoggD
133000000000     C*
133100000000     C                   else
133200000000     C*
133300000000     C                   SetOn                                        5599
133400000000     C*
133500000000     C                   endif
133600000000     C*
133700000000     C                   endif
133800000000     C*
133900000000     C                   ENDSR
134000000000     C/EJECT
134100000000     C************************************************************
134200000000     C* Chiamata al driver dei soggetti
134300000000     C************************************************************
134400000000     C     CallDvaSog    BEGSR
134500000000     C*
134600000000     C                   EVAL      DVALenOut   = %Size(DVASOG)
134700000000     C*
134800000000     C                   CALLB     'NDDVASOG'
134900000000     C                   PARM      '1'           DVARIC
135000000000     C                   PARM      XSCSOC        DVASOCIETA
135100000000     C                   PARM                    DVAUNITA
135200000000     C                   PARM      'DVASOG    '  DVASTRUTT
135300000000     C                   PARM                    DVADTRIF
135400000000     C                   PARM                    DVALENOUT
135500000000     C* Se si accede con il codice cli/for impostare i campi seguenti
135600000000     C* se si accede con il soggetto ometterli e impostare gli altri
135700000000     C*                  PARM                    DVASNATURA
135800000000     C*                  PARM                    DVACODICE
135900000000     C*                  PARM                    DVALINEAV
136000000000     C*                  PARM                    DVAFILIALE
136100000000     C                   PARM                    *omit
136200000000     C                   PARM                    *omit
136300000000     C                   PARM                    *omit
136400000000     C                   PARM                    *omit
136500000000     C                   PARM      WSogg         DVSSOGG
136600000000     C                   PARM      *Blank        DVSTPIND
136700000000     C                   PARM      *Blank        DVSCDIND
136800000000     C                   PARM                    DVAERRORE
136900000000     C                   PARM                    DVAOUTPUT
137000000000     C*
137100000000     C                   ENDSR
137200000000     C/EJECT
137300170804     C************************************************************
137400170804     C* Controllo causale
137500170804     C************************************************************
137600170804     C     CtrCausale    BEGSR
137700170804     C*
137800170804     C                   Clear                   D1CausaleD
137900170804     C*
138000170804     C* la causale deve essere valida, controllo con data
138100170804     C* di 10 anni in meno rispetto all'1/01/Anno a video
138200170804     C                   Eval      WAnno       = D1Anno
138300170804     C                   MoveL     WAnno         AnnoDs
138400170804     C                   MoveL     '01'          MeseDs
138500170804     C                   MoveL     '01'          GiornoDs
138600170804     C                   Move      DataDs        WDtAlfa
138700170804     C                   Move      WDtAlfa       WData
138800170804     C     WData         SubDur    10:*Y         WData
138900170804     C                   Move      WData         WDtAlfa
139000170804     C*
139100170804     C                   if        D1Causale  <> *Blank
139200170804     C                   Clear                   ANOPE00F
139300170804     C                   Eval      Lenout = %Size(ANOPE00F)
139400170804     C                   CALLB     'NDMVC003'
139500170804     C                   PARM                    XScSoc
139600170804     C                   PARM                    D1Causale
139700170804     C                   PARM                    WDtAlfa
139800170804     C                   PARM                    Uso
139900170804     C                   PARM      *Off          GesMsg
140000170804     C                   PARM      *Off          Esito
140100170804     C                   PARM      'ANOPE     '  Struttura
140200170804     C                   PARM                    ANOPE00F
140300170804     C                   PARM                    LenOut
140400170804     C                   PARM      *Off          Refresh
140500170804     C*
140600170804     C                   if        Esito     = *on
140700170804     C                   SetOn                                        5199
140800170804     C                   else
140900170804     C*
141000170804     C                   Eval      D1CausaleD  = OPEDesBrev
141100170804     C*
141200170804     C                   endif
141300170804     C*
141400170804     C                   endif
141500170804     C*
141600170804     C                   ENDSR
141700170804     C/EJECT
141800170804     C***********************************************************
141900170804     C* Controllo tipo record
142000170804     C***********************************************************
142100170804     C     CtrTpRec      BEGSR
142200170804     C*
142300170804     C                   Clear                   D1TpRecD
142400170804     C*
142500170804     C                   Clear                   X07ValDs
142600170804     C*
142700170804     C                   Move      '1'           X07Ric
142800170804     C                   Move      *On           X07All
142900170804     C                   Movel     'SDGCST000'   X07TRc
143000170804     C                   Movel     'CSTTPREC '   X07Cam
143100170804     C                   Movel     D1TpRec       X07VAL
143200170804     C*
143300170804     C                   Callb     'X07VALR'
143400170804     C                   Parm                    X07ValDs
143500170804     C*
143600170804     C     X07ERR        IFEQ      *Off
143700170804     C                   MOVEL     X07DES        D1TpRecD
143800170804     C                   else
143900170804     C                   SetOn                                        5599
144000170804     C                   endif
144100170804     C*
144200170804     C                   ENDSR
144300170804     C/EJECT
144400000000     C***********************************************************
144500000000     C* Controllo dati annullati
144600000000     C***********************************************************
144700000000     C     CtrAnn        BEGSR
144800000000     C*
144900000000     C                   Clear                   D1AnnD
145000000000     C*
145100000000     C                   Clear                   X07ValDs
145200000000     C                   Move      '1'           X07Ric
145300000000     C                   Move      *Off          X07All
145400000000     C                   Movel     'ANDIZ000 '   X07TRc
145500000000     C                   Movel     'DSPATT   '   X07Cam
145600000000     C                   Movel     D1Ann         X07VAL
145700000000     C                   Callb     'X07VALR'
145800000000     C                   Parm                    X07ValDs
145900000000     C*
146000000000     C     X07ERR        IFEQ      *Off
146100000000     C                   MOVEL     X07DES        D1AnnD
146200000000     C                   else
146300000000     C                   SetOn                                        5699
146400000000     C                   endif
146500000000     C*
146600000000     C                   Clear                   D1TpAnnD
146700000000     C                   if        D1TpAnn    <>  *blank
146800000000     C*  solo se richiesti solo gli annullati oppure con annullati
146900000000     C                   if        D1Ann      =   '1'    or
147000000000     C                             D1Ann      =   '3'
147100000000     C                   exsr      CtrTpAnn
147200000000     C                   else
147300000000     C                   SetOn                                        5899
147400000000     C                   endif
147500000000     C                   endif
147600000000     C*
147700000000     C                   ENDSR
147800000000     C/EJECT
147900000000     C***********************************************************
148000000000     C* Controllo tipo annullamento
148100000000     C***********************************************************
148200000000     C     CtrTpAnn      BEGSR
148300000000     C*
148400000000     C                   Clear                   X07ValDs
148500000000     C                   Move      '1'           X07Ric
148600000000     C                   Move      *Off          X07All
148700170804     C                   Movel     'SDGCST000'   X07TRc
148800170804     C                   Movel     'CSTTPANN'    X07Cam
148900000000     C                   Movel     D1TpAnn       X07VAL
149000000000     C                   Callb     'X07VALR'
149100000000     C                   Parm                    X07ValDs
149200000000     C*
149300000000     C     X07ERR        IFEQ      *Off
149400000000     C                   MOVEL     X07DES        D1TpAnnD
149500000000     C                   else
149600000000     C                   SetOn                                        5799
149700000000     C                   endif
149800000000     C*
149900000000     C                   ENDSR
150000000000     C/EJECT
150100000000     C************************************************************
150200000000     C* DEFINIZIONE DELL'ORDINAMENTO
150300000000     C************************************************************
150400000000     C     DEFORD        BEGSR
150500000000     C*
150600000000     C* definisco l'ordinamento della lista a seconda dei valori
150700000000     C* impostati nel video di parzializzazione
150800000000     C                   Z-ADD     1             $ORD
150900000000     C*
151000000000     C                   SELECT
151100000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
151200000000     C* rcd con la chiave passata
151300000000     C     $Unique       WHENEQ    *ON
151400000000     C                   Z-ADD     99            $ORD
151500000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
151600000000     C* utilizzerò l'ordinamento precedentemente memorizzato
151700000000     C* (temporaneamente o definitivamente) nei valori assunti
151800000000     C                   OTHER
151900000000     C                   ENDSL
152000000000     C                   ENDSR
152100000000     C/EJECT
152200000000     C************************************************************
152300000000     C* Chiamata programma con tasto personalizzato
152400000000     C************************************************************
152500000000     C     FPerS1        BEGSR
152600000000     C*
152700000000     C*
152800000000     C* salvo la DS di ingresso
152900170804    >C                   MOVEL     SDGSI75DS     $SaveDS
153000000000     C*
153100170804    >C                   MOVEL     SDGSI75DS     KPJBU
153200000000     C                   CALL      $PGF2(PosFun)                        22
153300000000     C                   PARM                    KPJBA
153400000000     C*
153500000000     C* ripristino la DS di ingresso
153600170804    >C                   MOVEL     $SaveDS       SDGSI75DS
153700000000     C*
153800000000     C                   ENDSR
153900000000     C/EJECT
154000000000     C************************************************************
154100000000     C* GESTIONE LISTA
154200000000     C************************************************************
154300000000     C     GESS1         BEGSR
154400000000     C*
154500000000     C* mantengo la situazione 'aperta' o 'chiusa' del sfl drop
154600000000     C                   Eval      *In09     =   C1Mode
154700000000     C*
154800000000     C* inizializzazione videata
154900000000     C     $INZS1        IFEQ      *ON
155000000000     C                   EXSR      INZS1
155100000000     C                   MOVE      *OFF          $INZS1
155200000000     C                   ENDIF
155300000000     C* emissione piede videata se proveniente da altra
155400000000     C* salvataggio valore formato di provenienza
155500000000     C     $LASTV        IFNE      'S1'
155600000000     C                   WRITE     FMTT1
155700000000     C                   EXSR      GESPIE
155800000000     C                   MOVE      $LASTV        $LASTG
155900000000     C                   MOVE      'S1'          $LASTV
156000000000     C                   ENDIF
156100000000     C*
156200000000     C     WMAX          IFEQ      0
156300000000     C                   WRITE     FMTD3                                        NO RCD
156400000000     C                   ENDIF
156500000000     C* Inizializzazione indice tasti funzione personalizzati
156600000000     C                   Eval      PosFun = 25
156700000000     C*
156800000000     C                   EXFMT     FMTC1
156900000000     C*
157000000000     C* azzero la posizione del cursore
157100000000     C                   Z-ADD     0             H1RIGA
157200000000     C                   Z-ADD     0             H1COLO
157300000000     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
157400000000     C* di refresh dello sfondo
157500000000     C                   MOVE      '1'           WExf
157600000000     C*
157700000000     C* riemettere il nuovo sfondo del subfile
157800000000     C                   Eval      WA39Iniz    = '0'
157900000000     C*
158000000000     C     C1NRR         IFNE      0
158100000000     C                   Z-ADD     C1NRR         WSFL
158200000000     C                   ENDIF
158300000000     C                   IF        SFLNRR > 0
158400000000     C                   Z-ADD     SFLNRR        C1RCD
158500000000     C                   ENDIF
158600000000     C* inizializzazione indice opzioni personalizzate
158700000000     C                   Eval      PosOpz = 0
158800000000     C* CONTROLLO ABILITAZIONE TASTI
158900000000     C* INPUT
159000000000     C* $FC2 = Abilitazioni videata S1
159100000000     C* OUTPUT
159200000000     C* *IN97 + *IN99 Errori
159300000000     C                   MOVEA     $FC2          $FC
159400000000     C                   EXSR      CTRKEY
159500000000     C*
159600000000     C     *IN99         IFEQ      *OFF
159700000000     C*
159800000000     C                   SELECT
159900000000     C* F3=Fine
160000000000     C     $TASTO        WHENEQ    F03
160100000000     C                   EXSR      F03S1
160200130906     C* F5=Refresh
160300130906     C     $TASTO        WHENEQ    F05
160400130906     C                   EXSR      F05S1
160500000000     C* F8=Successivo
160600000000     C     $TASTO        WHENEQ    F08
160700000000     C                   EXSR      F08S1
160800000000     C* F10=Immissione
160900000000     C     $TASTO        WHENEQ    F10
161000000000     C                   EXSR      F10S1
161100000000     C* F12=Ritorno
161200000000     C     $TASTO        WHENEQ    F12
161300000000     C                   EXSR      F12S1
161400000000     C* F17=Parzializzazioni
161500000000     C     $TASTO        WHENEQ    F17
161600000000     C                   EXSR      F17S1
161700000000     C* F18=Modifica valori assunti
161800000000     C     $TASTO        WHENEQ    F18
161900000000     C                   EXSR      F18S1
162000000000     C* F23=Altre opzioni
162100000000     C     $TASTO        WHENEQ    F23
162200000000     C                   EXSR      F23S1
162300000000     C* F24=Altri tasti
162400000000     C     $TASTO        WHENEQ    F24
162500000000     C                   EXSR      F24S1
162600000000     C* ROLL-UP
162700000000     C     $TASTO        WHENEQ    ROLLUP
162800000000     C                   EXSR      ROLS1
162900000000     C* Personalizzata
163000000000     C     $PGF2(PosFun) Whenne    *BLANK
163100000000     C                   EXSR      FperS1
163200000000     C*
163300000000     C                   OTHER
163400000000     C* CONTROLLO DATI
163500000000     C                   EXSR      CTRC1
163600000000     C     *IN99         IFEQ      *OFF
163700000000     C                   EXSR      CTRS1
163800000000     C                   ENDIF
163900000000     C*
164000000000     C                   ENDSL
164100000000     C*
164200000000     C                   ENDIF
164300000000     C*
164400000000     C                   ENDSR
164500000000     C/EJECT
164600000000     C************************************************************
164700000000     C* Caricamento OPZIONI
164800000000     C************************************************************
164900000000    >C     CarOpz        BEGSR
165000000000     C*
165100000000     C* Reperimento tasti di funzione
165200000000     C                   EXSR      REPKEY
165300000000     C*
165400000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
165500000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
165600000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
165700000000     C     $ULKD1        IFEQ      0
165800000000     C                   EXSR      F24D1
165900000000     C                   ELSE
166000000000     C                   WRITE     FMTZ1
166100000000     C                   ENDIF
166200000000     C*
166300000000     C                   ENDSR
166400000000     C/EJECT
166500000000     C************************************************************
166600000000     C* INIZIALIZZAZIONE LISTA
166700000000     C************************************************************
166800000000    >C     INZS1         BEGSR
166900000000     C* pulizia SFL
167000000000     C                   SETOFF                                         3031
167100000000     C                   WRITE     FMTC1
167200000000     C                   SETON                                          31
167300000000     C*
167400000000     C* pulizia del sottotitolo
167500000000     C                   Clear                   D1TIT
167600000000     C*
167700170804     C                   Eval      C1Anno      = AnnoI75
167800170804     C                   Eval      C1Trim      = TrimI75
167900170804     C*
168000170804     C                   Eval      C1PartIva   = PartIvaI75
168100170804     C                   Eval      C1CdFisc    = CdFiscI75
168200170804     C                   Eval      C1CogRag1   = CogRag1I75
168300170804     C                   Eval      C1Stato1    = Stato1I75
168400170804     C                   EXSR      CtrCF
168500000000     C*
168600000000     C* riempimento opzioni
168700000000     C                   MOVEA     $TX1          $TX
168800000000     C                   MOVEA     $FL1          $FL
168900000000     C                   Z-ADD     0             WCOZ
169000000000     C                   Z-ADD     0             WMXO
169100000000     C*  equivale al num.di elementi della schiera $TX1
169200000000     C                   Z-add     OpzLun        WMXO
169300000000     C                   Z-ADD     $C1Op1        $L01
169400000000     C                   Z-ADD     $C1Op2        $L02
169500000000     C                   EXSR      RIEOPZ
169600000000     C                   Z-ADD     23            X
169700000000     C                   MOVE      FF23          $FC2(X)
169800000000     C                   MOVEL     WOPZ1         C1OPZ1
169900000000     C                   MOVEL     $ALLOPT       H1ALLOPT
170000000000     C                   MOVEL     WOPZ2         C1OPZ2
170100000000     C                   Z-ADD     WCOZ          C1COZ
170200000000     C*
170300000000     C* CARICAMENTO SFL A PAGINE
170400000000     C*
170500000000     C                   Z-ADD     0             S1NRR
170600000000     C                   Z-ADD     0             WMAX
170700170804     C*
170800000000     C* Posizionamento su file pilota
170900000000     C                   CLEAR                   C1POS
171000170804     C                   EXSR      SETANA
171100000000     C* ripristino i valori del posizionamento
171200000000     C                   MOVE      W1POS         C1POS
171300000000     C*
171400000000     C* Lettura file pilota
171500170804     C                   EXSR      REDANA
171600000000     C*
171700000000     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
171800000000     C                   IF        $LastV = 'D1'
171900000000     C                   Z-ADD     SFLPAG        WPAG
172000000000     C                   ENDIF
172100000000     C                   EXSR      ROLS1
172200000000     C* imposto il caricamento sfl del numero massimo di righe per pagina
172300000000     C                   Z-ADD     SFLPAG        WPAG
172400000000     C*
172500000000     C                   ENDSR
172600000000     C/EJECT
172700000000     C************************************************************
172800000000     C* CARICAMENTO PAGINA LISTA
172900000000     C************************************************************
173000000000     C     ROLS1         BEGSR
173100000000     C*
173200000000     C                   SETOFF                                       32
173300000000     C                   Z-ADD     0             Y
173400000000     C                   Z-ADD     WMAX          S1NRR
173500000000     C*
173600000000     C* Leggo dal file anagrafico per caricare la lista
173700000000     C*
173800000000     C     $EndFile      DOWEQ     *OFF
173900000000     C     Y             ANDLT     WPAG
174000000000     C*
174100000000     C                   EXSR      RIES1
174200000000     C*
174300000000     C                   ADD       1             S1NRR
174400000000     C                   ADD       1             Y
174500000000     C*
174600000000     C                   WRITE     FMTS1
174700000000     C*
174800000000     C                   EXSR      REDANA
174900000000     C*
175000000000     C                   ENDDO
175100000000     C*
175200000000     C                   Z-ADD     S1NRR         WMAX                 30
175300000000     C*
175400000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
175500000000     C*
175600000000     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
175700000000     C                   MVR                     RESTO             3 0
175800000000     C     PAGINE        MULT      SFLPAG        C1RCD
175900000000     C     RESTO         IFGT      0
176000000000     C                   ADD       1             C1RCD
176100000000     C                   ELSE
176200000000     C                   SUB       SFLPAG        C1RCD
176300000000     C                   ADD       1             C1RCD
176400000000     C                   ENDIF
176500000000     C*
176600000000     C                   ENDSR
176700000000     C/EJECT
176800170804     C************************************************************
176900170804     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
177000170804     C************************************************************
177100170804     C     SETANA        BEGSR
177200170804     C*
177300170804     C                   Clear                   DSCLIFOR
177400170804     C*
177500170804     C                   Eval      CSTSocieta   = SocietaI75
177600170804     C                   Eval      CSTTpCom     = TpComI75
177700170804     C                   Eval      CSTTpRec     = D1TpRec
177800170804     C                   Eval      CSTAnno      = D1Anno
177900170804     C                   Eval      CSTTrim      = D1Trim
178000170804     C                   Eval      CSTMese      = 0
178100170804     C*
178200170804     C                   Eval      CSTPartIVA   = D1Partiva
178300170804     C                   Eval      CSTCdFisc    = D1CdFisc
178400170804     C*
178500170804     C                   Eval      COGRAG1Ds = COGRAG1I75
178600170804     C                   Eval      CFDs      = CFI75
178700170804     C                   Eval      PartIVADs = PARTIVAI75
178800170804     C                   Eval      CdFiscDs  = CDFISCI75
178900170804     C                   Eval      STATO1Ds  = STATO1I75
179000170804     C                   Eval      IDPAES1Ds = IDPAES1I75
179100170804     C                   Eval      PIVA1Ds   = PIVA1I75
179200170804     C                   Eval      CDFISC1Ds = CDFISC1I75
179300170804     C                   Eval      TPN1Ds    = TPN1I75
179400170804     C                   Eval      DENOM1Ds  = DENOM1I75
179500170804     C                   Eval      NOME1Ds   = NOME1I75
179600170804     C                   Eval      COGNOM1Ds = COGNOM1I75
179700170804     C                   Eval      INDIR2Ds  = INDIR2I75
179800170804     C                   Eval      NRCIV2Ds  = NRCIV2I75
179900170804     C                   Eval      CAP2Ds    = CAP2I75
180000170804     C                   Eval      LOCALI2Ds = LOCALI2I75
180100170804     C                   Eval      PROV2Ds   = PROV2I75
180200170804     C                   Eval      STATO2Ds  = STATO2I75
180300170804     C                   Eval      IDPAES2Ds = IDPAES2I75
180400170804     C                   Eval      INDIR3Ds  = INDIR3I75
180500170804     C                   Eval      NRCIV3Ds  = NRCIV3I75
180600170804     C                   Eval      CAP3Ds    = CAP3I75
180700170804     C                   Eval      LOCALI3Ds = LOCALI3I75
180800170804     C                   Eval      PROV3Ds   = PROV3I75
180900170804     C                   Eval      STATO3Ds  = STATO3I75
181000170804     C                   Eval      IDPAES3Ds = IDPAES3I75
181100170804     C                   Eval      STATO4Ds  = STATO4I75
181200170804     C                   Eval      IDPAES4Ds = IDPAES4I75
181300170804     C                   Eval      PIVA4Ds   = PIVA4I75
181400170804     C                   Eval      TPN4Ds    = TPN4I75
181500170804     C                   Eval      COGRAG4Ds = COGRAG4I75
181600170804     C                   Eval      DENOM4Ds  = DENOM4I75
181700170804     C                   Eval      NOME4Ds   = NOME4I75
181800170804     C                   Eval      COGNOM4Ds = COGNOM4I75
181900170804     C*
182000170804     C                   Eval      CSTCliFor= DSCliFor
182100170804     C*
182200170804     C     K07CST02      Setll     SdgCST02l
182300170804     C*
182400170804     C                   ENDSR
182500170804     C/EJECT
182600170804     C************************************************************
182700170804     C* LETTURA RCD ARCHIVIO PILOTA
182800170804     C************************************************************
182900170804     C     REDANA        BEGSR
183000170804     C*
183100170804     C                   MOVEL     *OFF          $EndFile
183200170804     C                   MOVEL     *OFF          $RCDOK
183300170804     C*
183400170804     C     $EndFile      DOUEQ     *ON
183500170804     C     $RcdOK        OREQ      *ON
183600170804     C*
183700170804     C                   SELECT
183800170804     C*
183900170804     C     $ORD          WHENEQ    1
184000170804     C*
184100170804     C     K07CST02      READE     SDGCST02L                              21
184200170804     C*
184300170804     C                   EndSL
184400170804     C*
184500170804     C                   MOVE      *IN21         $EndFile
184600170804     C*
184700170804     C     $EndFile      IFEQ      *OFF
184800170804     C                   EXSR      SELANA
184900170804     C                   ENDIF
185000170804     C*
185100170804     C                   ENDDO
185200170804     C*
185300170804     C                   MOVE      $EndFile      *IN33                          SflEnd
185400170804     C*
185500170804     C                   ENDSR
185600170804     C/EJECT
185700000000     C************************************************************
185800000000     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
185900000000     C************************************************************
186000000000     C     CLCPAG        BEGSR
186100000000     C* Input :
186200000000     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
186300000000     C*          cursore
186400000000     C* - SFLPAG = numero rcd per pagina sfl
186500000000     C* Output :
186600000000     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
186700000000     C*
186800000000     C     WSFL          DIV       SFLPAG        PAGINE            4 0
186900000000     C                   MVR                     RESTO             3 0
187000000000     C     RESTO         IFGT      0
187100000000     C                   ADD       1             PAGINE
187200000000     C                   ENDIF
187300000000     C     PAGINE        MULT      SFLPAG        WPAG
187400000000     C*
187500000000     C                   ENDSR
187600000000     C/EJECT
187700000000     C************************************************************
187800000000     C* SELEZIONI SULLA RIGA LETTA
187900000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
188000000000     C* non rientra nei parametri di parzializzazione
188100000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
188200000000     C* superiore stabilito nei parametri
188300000000     C*
188400170808     C************************************************************
188500000000     C     SELANA        BEGSR
188600000000     C*
188700000000     C                   MOVEL     *ON           $RCDOK
188800000000     C                   MOVE      *hival        ALL9             15
188900170808     C*
189000170808     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
189100170808     C*
189200170808     C                   SELECT
189300170808     C*
189400170808     C                   When      D1Causale  <> *Blank   and
189500170808     C                             CSTCausale <> D1Causale
189600170808     C                   Eval      $RcdOk = *off
189700170808     C*
189800170808     C                   When      D1Sogg     <> *Blank   and
189900170808     C                             CSTSogg    <> D1Sogg
190000170808     C                   Eval      $RcdOk = *off
190001170810     C* Se richiamato dal catalogo
190002170810     C                   When      SavOpzI75 = '15' and
190003170810     C*
190004170810     C                             ((CstTpRec = 'O' and
190005170810     C                              (FileI75  <> CSTFileO or
190006170810     C                              IDFileI75 <> CSTIdFileO)) Or
190007170810     C*
190008170810     C                              (CstTpRec = 'R' and
190009170810     C                              (FileI75  <> CSTFileR or
190010170810     C                              IDFileI75 <> CSTIdFileR)) Or
190011170810     C*
190012170810     C                              (CstTpRec = 'A' and
190013170810     C                              (FileI75  <> CSTFileA or
190014170810     C                              IDFileI75 <> CSTIdFileA)))
190015170810     C*
190016170810     C                   Eval      $RcdOK = *OFF
190100170808     C*
190200170808     C                   ENDSL
190300170808     C*
190400170808     C*
190500170808     C                   if        $RcdOk = *On
190600170808     C*
190700170808     C                   SELECT
190800170808     C*
190900170808     C* Attivi + non attivi
191000170808     C                   When      D1Ann       = '1'
191100170808     C                   if        D1TpAnn     = *Blank       or
191200170808     C                             (D1TpAnn   <> *Blank   and
191300170808     C                              D1TpAnn    = CSTTpAnn    )
191400170808     C******--------------  OK
191500170808     C                   else
191600170808     C                   Eval      $RcdOk = *off
191700170808     C                   endif
191800170808     C*
191900170808     C* Attivi
192000170808     C                   When      D1Ann       = '2'
192100170808     C                   if        CSTAnn      = *Blank   and
192200170808     C                             CSTTpAnn    = *Blank
192300170808     C******--------------  OK
192400170808     C                   else
192500170808     C                   Eval      $RcdOk = *off
192600170808     C                   endif
192700170808     C*
192800170808     C* NON Attivi  (ANNULLATI)
192900170808     C                   When      D1Ann       = '3'
193000170808     C                   if        CSTAnn     <> *Blank           and
193100170808     C                             CSTTpAnn   <> *Blank           and
193200170808     C                             ((CSTTpAnn   = D1TpAnn  and
193300170808     C                               D1TpAnn   <> *Blank      )or
193400170808     C                               D1TpAnn    = *Blank         )
193500170808     C******--------------  OK
193600170808     C                   else
193700170808     C                   Eval      $RcdOk = *off
193800170808     C                   endif
193900170808     C*
194000170808     C                   ENDSL
194100170808     C*
194200170808     C                   endif
194300000000     C*
194400000000     C                   ENDSR
194500000000     C/EJECT
194600110825     C************************************************************
194700110825     C* RIEMPIMENTO RIGA LISTA
194800110825     C************************************************************
194900110825     C     RIES1         BEGSR
195000110825     C*
195100110825     C                   CLEAR                   FMTS1
195200110825     C*
195300110825     C* riempio i campi del sfl, compresi gli hidden con i valori
195400110825     C* del rcd letto
195500130919     C*
195600130919     C* se nota di variazione, sottolineo riferimenti documento
195700170804     C                   if        CSTTpDoc    = 'TD04'   or
195800170804     C                             CSTTpDoc    = 'TD05'   or
195900170804     C                             CSTImpOpe   < 0        or
196000170804     C                             CSTIVAOpe   < 0
196100130919     C                   Eval      *In12       = *On
196200130919     C                   endif
196300130919     C*
196400170804     C                   Eval      H1Sys       = CSTSys
196500170804     C                   Eval      H1NrAsReg   = CSTNrAsReg
196600170804     C                   Eval      H1NrRigaM   = CSTNrRigaM
196700170804     C                   Eval      H1NrRiga    = CSTNrRiga
196800170804     C*
196900170804     C                   Eval      H1NrRCST    = NrRCST
197000170804     C*
197100170804     C                   Eval      H1TpDoc     = CSTTpDoc
197200110825     C*
197300170804     C                   MoveL     CSTKcc        S1Kcc
197400170804     C                   MoveL     CSTKsc        S1Ksc
197500170804     C                   if        CSTDtDoc   <> *Loval
197600170804     C     *Jobrun       Move      CSTDtDoc      S1DtDoc
197700110825     C                   endif
197800170804     C                   Z-Add     CSTNrDoc      S1NrDoc
197900170804     C                   Z-Add     CSTNrDoc      H1NrDoc
198000170804     C                   MoveL     CSTSerDoc     H1SerDoc
198100170804     C                   if        CSTDtReg   <> *Loval
198200170804     C     *Jobrun       Move      CSTDtReg      S1DtReg
198300110825     C                   endif
198400170804     C                   Z-Add     CSTNrReg      S1NrReg
198500170804     C                   Z-Add     CSTNrReg      H1NrReg
198600170804     C                   MoveL     CSTSerReg     H1SerReg
198700170804     C                   MoveL     CSTOrigine    S1Origine
198800110825     C*
198900110825     C* azzero le memorizzazioni degli attributi non bottoni
199000110825     C                   MOVEL     *Blank        H1ANNA
199100170804     C                   if        CSTAnn      = '1'       or
199200170804     C                             CSTAnn      = '9'
199300110825     C                   MoveL     *ON           H1ANNA
199400110825     C                   endif
199500110825     C*
199600170804     C                   Z-Add     CSTImpOpe     S1ImpOpe
199700170804     C                   Z-Add     CSTIVAOpe     S1IVAOpe
199800170804     C                   MoveL     CSTCausale    S1Causale
199900170804     C                   Z-Add     CSTAliq       S1Aliq
200000170804     C                   MoveL     CSTRifIVA     S1RifIVA
200100110825     C*
200200110825     C* imposto quegli attributi del sfl che sono modificabili
200300110825     C* al richiamo del determinato pgm
200400110825     C                   EXSR      ATRS1
200500110825     C*
200600110825     C                   ENDSR
200700110825     C/EJECT
200800000000     C************************************************************
200900000000     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
201000000000     C************************************************************
201100000000     C     ATRS1         BEGSR
201200000000     C*
201300000000     C* in questa routine bisogna attivare indicatori o campi programma
201400000000     C* che gestiscono gli attributi di uno o più campi di questa videata;
201500000000     C* attenzione : non è questo il posto in cui eseguire controlli,
201600000000     C*  dovrebbero già arrivare dei flag
201700000000     C*
201800000000     C* azzero gli attributi non bottoni
201900000000     C                   MOVEL     X'A0'         S1ANNA
202000000000     C*
202100000000     C     H1ANNA        IFEQ      *ON
202200000000     C                   MOVEL     ANNUL         S1ANNA
202300000000     C                   ENDIF
202400130919     C*
202500130919     C* se nota di variazione, sottolineo riferimenti documento
202600170804     C                   if        H1TpDoc     = 'TD04'   or
202700170804     C                             H1TpDoc     = 'TD05'   or
202800170804     C                             S1ImpOpe    < 0        or
202900170804     C                             S1IVAOpe    < 0
203000130919     C                   Eval      *In12       = *On
203100130919     C                   else
203200130919     C                   Eval      *In12       = *Off
203300130919     C                   endif
203400130919     C*
203500000000     C*
203600000000     C                   ENDSR
203700000000     C/EJECT
203800000000     C************************************************************
203900000000     C* GESTIONE F08 VIDEO S1
204000000000     C************************************************************
204100000000     C     F08S1         BEGSR
204200000000     C*
204300000000     C                   MOVE      *ON           $FINE
204400170804    >C                   MOVE      '0'           RETI75
204500000000     C*
204600000000     C                   ENDSR
204700000000     C/EJECT
204800000000     C************************************************************
204900000000     C* GESTIONE F03 VIDEO S1
205000000000     C************************************************************
205100000000     C     F03S1         BEGSR
205200000000     C*
205300000000     C                   MOVE      *ON           $FINE
205400170804    >C                   MOVE      '1'           RETI75
205500000000     C* fine programma
205600000000     C                   EXSR      ENDPGM
205700000000     C*
205800000000     C                   ENDSR
205900000000     C/EJECT
206000130924     C************************************************************
206100130924     C* GESTIONE F05 VIDEO S1
206200130924     C************************************************************
206300130924     C     F05S1         BEGSR
206400130924     C*
206500130924     C                   MOVE      *ON           $INZS1
206600130924     C*
206700130924     C                   ENDSR
206800130924     C/EJECT
206900130924     C************************************************************
207000000000     C* GESTIONE F10 VIDEO S1
207100000000     C* AGGIUNTA RECORD
207200000000     C************************************************************
207300000000     C     F10S1         BEGSR
207400000000     C*
207500170804     C                   RESET                   SDGSI76DS
207600130924     C*
207700170804     C                   MOVE      xscsoc        SocietaI76
207800170804     C                   MOVEL     '01'          OPZI76
207900170804     C     CMTI75        IFEQ      '1'
208000170804     C                   MOVEL     '1'           CMTI76
208100000000     C                   ELSE
208200170804     C                   MOVEL     '0'           CMTI76
208300000000     C                   ENDIF
208400000000     C*
208500170804     C                   Eval      TpAInpI76   = D1TpAnn
208600170804     C                   Eval      CausInpI76  = D1Causale
208700170804     C                   Eval      SoggInpI76  = D1Sogg
208800170804     C*
208900170804     C                   Eval      AnnoI76     = C1Anno
209000170804     C                   Eval      TrimI76     = C1Trim
209100170804     C                   Eval      PartIvaI76  = C1PartIva
209200170804     C                   Eval      CdFiscI76   = C1CdFisc
209300170804     C                   Eval      CogRag1I76  = C1CogRag1
209400170804     C                   Eval      Stato1I76   = C1Stato1
209500170804     C                   EXSR      CtrCF
209600170804     C*
209700170804     C                   Eval      TpComI76    = TpComI75
209800170804     C                   Eval      TpRecI76    = D1TpRec
209900170804     C                   Eval      CfI76       = D1Cf
210000170804     C                   Eval      SoggInpI76  = D1Sogg
210100170804     C                   Eval      PartivaI76  = D1Partiva
210200170804     C                   Eval      CdFiscI76   = D1CdFisc
210300170804     C*
210400170804     C                   Eval      AnnI76      = D1Ann
210500170804     C                   Eval      TpAInpI76   = D1TpAnn
210600170804     C*
210700170804     C*
210800170804     C* passare anche le parzializzazioni ricevute relative alla chiave
210900170804     C* univoca :
211000170804     C*  se ce ne sono, potranno essere usate come dft d'immissione
211100170804    >C                   IF        OPZI75 <> *Blank
211200170804     C                   Eval      Stato1I76   = Stato1I75
211300170804     C                   Eval      IdPaes1I76  = IdPaes1I75
211400170804     C                   Eval      PIVA1I76    = PIVA1I75
211500170804     C                   Eval      CdFisc1I76  = CdFisc1I75
211600170804     C                   Eval      CogRag1I76  = CogRag1I75
211700170804     C                   Eval      TpN1I76     = TpN1I75
211800170804     C                   Eval      Denom1I76   = Denom1I75
211900170804     C                   Eval      Cognom1I76  = Cognom1I75
212000170804     C                   Eval      Nome1I76    = Nome1I75
212100170804     C*
212200170804     C                   Eval      Indir2I76   = Indir2I75
212300170804     C                   Eval      NrCiv2I76   = NrCiv2I75
212400170804     C                   Eval      CAP2I76     = CAP2I75
212500170804     C                   Eval      Locali2I76  = Locali2I75
212600170804     C                   Eval      Prov2I76    = Prov2I75
212700170804     C                   Eval      Stato2I76   = Stato2I75
212800170804     C                   Eval      IdPaes2I76  = IdPaes2I75
212900170804     C*
213000170804     C                   Eval      Indir3I76   = Indir3I75
213100170804     C                   Eval      NrCiv3I76   = NrCiv3I75
213200170804     C                   Eval      CAP3I76     = CAP3I75
213300170804     C                   Eval      Locali3I76  = Locali3I75
213400170804     C                   Eval      Prov3I76    = Prov3I75
213500170804     C                   Eval      Stato3I76   = Stato3I75
213600170804     C                   Eval      IdPaes3I76  = IdPaes3I75
213700170804     C*
213800170804     C                   Eval      Stato4I76   = Stato4I75
213900170804     C                   Eval      IdPaes4I76  = IdPaes4I75
214000170804     C                   Eval      PIVA4I76    = PIVA4I75
214100170804     C                   Eval      CogRag4I76  = CogRag4I75
214200170804     C                   Eval      TpN4I76     = TpN4I75
214300170804     C                   Eval      Denom4I76   = Denom4I75
214400170804     C                   Eval      Cognom4I76  = Cognom4I75
214500170804     C                   Eval      Nome4I76    = Nome4I75
214600170804     C                   ENDIF
214700000000     C*
214800170804    >C                   CALL      'SDGSI76R'
214900000000     C                   PARM                    KPJBA
215000170804    >C                   PARM                    SDGSI76DS
215100000000     C* ritorno da PGM gestione
215200170804    >C                   EXSR      RTNI76
215300000000     C     WINZS1        IFEQ      *ON
215400000000     C                   MOVE      *ON           $INZS1
215500000000     C* se esistevano già righe sul sfl
215600000000     C* calcolo pagina a cui deve posizionarsi
215700000000     C     WSFL          IFGT      0
215800000000     C                   EXSR      CLCPAG
215900000000     C* altrimenti carico solo la 1a pagina
216000000000     C                   ELSE
216100000000     C                   Z-ADD     SFLPAG        WPAG
216200000000     C                   ENDIF
216300000000     C                   ENDIF
216400000000     C*
216500000000     C                   MOVE      *HIVAL        $LASTV
216600000000     C*
216700000000     C                   ENDSR
216800000000     C/EJECT
216900000000     C************************************************************
217000000000     C* GESTIONE F12 VIDEO S1
217100000000     C************************************************************
217200000000     C     F12S1         BEGSR
217300000000     C*
217400000000     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
217500000000     C     $LOP          IFEQ      1
217600000000     C                   MOVE      'D1'          $GEST
217700000000     C*  se la 1a videata è questa, per F12 torno a menu
217800000000     C                   ELSE
217900000000     C                   MOVE      *ON           $FINE
218000170804    >C                   MOVE      '2'           RETI75
218100000000     C                   ENDIF
218200000000     C*
218300000000     C                   ENDSR
218400000000     C/EJECT
218500000000     C************************************************************
218600000000     C* GESTIONE F17 VIDEO S1
218700000000     C************************************************************
218800000000     C     F17S1         BEGSR
218900000000     C*
219000000000     C                   MOVE      'D1'          $GEST
219100000000     C* se il pgm è stato richiamato con chiave univoca
219200000000     C* e dopo è stata richiesta una parzializzazione
219300000000     C* tolgo la gestione della chiave univoca (azzere/abblenco il
219400000000     C* valore; propongo come ordinamento il 1°)
219500000000     C     $Unique       IFEQ      *ON
219600000000     C                   MOVE      *OFF          $Unique
219700000000     C                   ENDIF
219800000000     C     $ORD          IFEQ      99
219900000000     C                   Z-ADD     1             $ORD
220000000000     C                   MOVE      *ON           $INZD1
220100000000     C                   ENDIF
220200000000     C*
220300000000     C                   ENDSR
220400000000     C/EJECT
220500000000     C************************************************************
220600000000     C* GESTIONE F18 VIDEO S1
220700000000     C************************************************************
220800000000     C     F18S1         BEGSR
220900000000     C*
221000000000     C                   MOVE      'D2'          $GEST
221100000000     C                   MOVEL     *ON           $INZD2
221200000000     C     $ORD          IFEQ      99
221300000000     C                   Z-ADD     1             $ORD
221400000000     C                   ENDIF
221500000000     C*
221600000000     C                   ENDSR
221700000000     C/EJECT
221800000000     C************************************************************
221900000000     C* GESTIONE F23 VIDEO S1
222000000000     C************************************************************
222100000000     C     F23S1         BEGSR
222200000000     C*
222300000000     C                   Z-ADD     $C1Op1        $L01
222400000000     C                   Z-ADD     $C1Op2        $L02
222500000000     C                   EXSR      RIEOPZ
222600000000     C                   Z-ADD     23            X
222700000000     C                   MOVE      FF23          $FC2(X)
222800000000     C                   MOVEL     WOPZ1         C1OPZ1
222900000000     C                   MOVEL     $ALLOPT       H1ALLOPT
223000000000     C                   MOVEL     WOPZ2         C1OPZ2
223100000000     C                   Z-ADD     WCOZ          C1COZ
223200000000     C*
223300000000     C                   ENDSR
223400000000     C/EJECT
223500000000     C************************************************************
223600000000     C* GESTIONE F24 VIDEO S1
223700000000     C************************************************************
223800000000     C     F24S1         BEGSR
223900000000     C*
224000000000     C                   MOVEA     $FX2          $FX
224100000000     C                   MOVEA     $FC2          $FC
224200000000     C                   MOVEA     $FR2          $FR
224300000000     C                   Z-ADD     $ULKS1        $ULK
224400000000     C                   Z-ADD     $S1L01        $L01
224500000000     C                   Z-ADD     $S1L02        $L02
224600000000     C                   EXSR      RIEKEY
224700000000     C                   Z-ADD     $ULK          $ULKS1            3 0
224800000000     C                   MOVEA     $FC           $FC2
224900000000     C                   MOVEL     $KEY1         Z2KE1
225000000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
225100000000     C                   MOVEL     $KEY2         Z2KE2
225200000000     C                   WRITE     FMTZ2
225300000000     C*
225400000000     C                   ENDSR
225500000000     C/EJECT
225600000000     C************************************************************
225700000000     C* CONTROLLO TESTATA LISTA
225800000000     C************************************************************
225900000000     C     CTRC1         BEGSR
226000000000     C*
226100000000     C                   MOVE      *OFF          *IN99
226200000000     C*
226300000000     C     C1POS         IFNE      W1POS
226400000000     C                   EXSR      POSS1
226500000000     C                   ENDIF
226600000000     C*
226700000000     C                   ENDSR
226800000000     C/EJECT
226900000000     C************************************************************
227000000000     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
227100000000     C************************************************************
227200000000     C     POSS1         BEGSR
227300000000     C*
227400000000     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
227500000000     C*
227600000000   B1C     WMAX          IFGT      0
227700000000     C                   READC     FMTS1                                  21
227800000000   B2C     *IN21         DOWEQ     *OFF
227900000000     C     *IN99         ANDEQ     *OFF
228000000000     C*
228100000000   B3C     S1OPZ         IFNE      0
228200000000     C                   SETON                                        92  99
228300000000   E3C                   ENDIF
228400000000     C* se rilevata opzione attiva, attivo sflnxtchg
228500000000   B3C     *IN99         IFEQ      *ON
228600000000     C                   MOVE      *ON           *IN32
228700000000     C*
228800000000     C* imposto quegli attributi del sfl che sono modificabili
228900000000     C* al richiamo del determinato pgm
229000000000     C                   EXSR      ATRS1
229100000000     C*
229200000000     C                   UPDATE    FMTS1
229300000000   X3C                   ELSE
229400000000     C                   READC     FMTS1                                  21
229500000000   E3C                   ENDIF
229600000000     C*
229700000000   E2C                   ENDDO
229800000000     C*
229900000000   E1C                   ENDIF
230000000000     C*
230100000000     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
230200000000     C* AL VALORE DI POSIZIONAMENTO :
230300000000     C* se questo è maggiore del valore del rispettivo campo di
230400000000     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
230500000000     C* con i valori di parzializzazione.
230600000000     C*
230700000000   B1C     *IN99         IFEQ      *OFF
230800000000    >C                   MOVE      *BLANK        W1POS
230900000000     C*
231000000000     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
231100000000     C* è maggiore del relativo valore di parzializzazione, nel qual caso
231200000000     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
231300000000     C* richiedo comunque un ricaricamento del sfl che, se non si è
231400000000     C* verificata la condizione suddetta avverrà con posizionamento a
231500000000     C* partire dal valore di parzializzazione
231600000000     C*
231700000000   B2C                   SELECT
231800000000    >C* Attenzione : se un ordinamento ha un campo di posizionamento
231900000000    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
232000000000    >C* dei relativi campi
232100000000     C*
232200000000     C     $ORD          WHENEQ    1
232300000000     C*
232400000000     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
232500000000     C*
232600000000   E2C                   ENDSL
232700000000     C*
232800000000     C* richiedo l'inizializzazione sfl
232900000000     C                   MOVE      *ON           $INZS1
233000000000     C*
233100000000   E1C                   ENDIF
233200000000     C*
233300000000     C                   ENDSR
233400000000     C/EJECT
233500000000     C************************************************************
233600000000     C* CONTROLLO OPZIONI LISTA
233700000000     C************************************************************
233800000000     C     CTRS1         BEGSR
233900000000     C*
234000000000     C                   MOVEL     *OFF          $ESCI
234100000000     C                   SETOFF                                       99
234200000000     C                   Z-ADD     0             S1OPZ
234300000000     C*
234400000000     C* Leggo il sfl solo se ci sono rcd
234500000000   B1C     WMAX          IFGT      0
234600000000     C                   READC     FMTS1                                  21
234700000000     C*
234800000000     C* esce se fine sfl o errore che richiede l'uscita
234900000000   B2C     *IN21         DOWEQ     *OFF
235000000000     C     $ESCI         ANDEQ     *OFF
235100000000     C                   Z-ADD     S1NRR         C1RCD
235200000000     C* ctrl su riga
235300000000     C                   EXSR      RECS1
235400000000     C* gestione opzioni
235500000000   B3C     S1OPZ         IFNE      0
235600000000     C     *IN99         ANDEQ     *OFF
235700000000     C                   EXSR      OPZS1
235800000000   E3C                   ENDIF
235900000000     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
236000000000   B3C     *IN99         IFEQ      *ON
236100000000     C                   MOVE      *ON           *IN32
236200000000   E3C                   ENDIF
236300000000     C*
236400000000     C* imposto quegli attributi del sfl che sono modificabili
236500000000     C* al richiamo del determinato pgm
236600000000     C                   EXSR      ATRS1
236700000000     C*
236800000000     C                   Z-ADD     0             S1OPZ
236900000000     C*
237000000000     C                   UPDATE    FMTS1
237100000000     C                   MOVE      *OFF          *IN32
237200000000     C*
237300000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
237400000000   B3C     $ESCI         IFEQ      *OFF
237500000000     C                   READC     FMTS1                                  21
237600000000     C* a fine ciclo ripristino il flag richiesta iniz.sfl
237700000000   B4C     *IN21         IFEQ      *ON
237800000000     C                   MOVE      WINZS1        $INZS1
237900000000     C                   MOVE      *off          WINZS1
238000000000     C* se è avvenuta almeno una modifica
238100000000   B5C                   IF        $INZS1 = *on
238200000000     C* calcolo pagina a cui deve posizionarsi
238300000000     C     WSFL          IFGT      0
238400000000     C                   EXSR      CLCPAG
238500000000     C                   ELSE
238600000000     C                   Z-ADD     SFLPAG        WPAG
238700000000     C                   ENDIF
238800000000   E5C                   ENDIF
238900000000   E4C                   ENDIF
239000000000   E3C                   ENDIF
239100000000     C*
239200000000   E2C                   ENDDO
239300000000     C*
239400000000   E1C                   ENDIF
239500000000     C*
239600000000     C                   ENDSR
239700000000     C/EJECT
239800000000     C************************************************************
239900000000     C* CONTROLLO CAMPI I/O RIGA LISTA
240000000000     C************************************************************
240100000000     C     RECS1         BEGSR
240200000000     C*
240300000000     C* reset indicatori DSPATR
240400000000     C                   MOVE      *ALL'0'       IN4049           10
240500000000     C                   MOVEA     IN4049        *IN(40)
240600000000     C*
240700000000     C* Controllo se Utente Autorizzato alla scelta
240800000000     C     S1OPZ         IFNE      0
240900000000     C                   Z-ADD     1             X
241000000000     C     S1OPZ         LOOKUP    $OP1(X)                                21
241100000000     C* mi salvo l'indice dell' opzione
241200000000     C   21              Z-add     X             PosOpz            2 0
241300000000     C     *IN21         IFEQ      *OFF
241400000000     C     $FL1(X)       OREQ      'N'
241500000000     C                   MOVE      *ON           $ESCI
241600000000     C                   SETON                                        409199
241700000000     C                   ENDIF
241800000000     C                   ENDIF
241900000000     C*
242000000000     C*
242100000000     C                   ENDSR
242200000000     C/EJECT
242300000000     C************************************************************
242400000000     C* GESTIONE OPZIONI LISTA
242500000000     C************************************************************
242600000000     C     OPZS1         BEGSR
242700000000     C*
242800000000     C                   Z-ADD     RIRI          CURR
242900000000     C                   Z-ADD     COCO          CURC
243000000000     C*
243100000000     C                   SELECT
243200000000     C*
243300000000     C* se opz=Selezione
243400000000     C* passo la chiave al pgm chiamante
243500000000     C     S1OPZ         WHENEQ    1
243600170804     C                   MOVE      S1OPZ         OPZI75
243700170804     C* passare nei campi appositi della DS SDGSI75DS il valore
243800000000     C* della chiave della VL univoca (se c'è)
243900000000     C                   MOVE      *ON           $ESCI
244000000000     C                   MOVE      *ON           $FINE
244100000000     C*
244200000000     C                   MOVE      *HIVAL        $LASTV
244300000000     C*
244400000000     C* Interrogazioni conto
244500000000     C     S1OPZ         WHENEQ    15
244600000000     C                   EXSR      Opzs1_15
244700170807     C*
244800170807     C* Manutenzione dati anagrafici
244900170807     C     S1OPZ         WHENEQ    22
245000170807     C*
245100170807     C                   RESET                   SDGSI74DS
245200170807     C*
245201170810     C                   If        OpzI75 = '05'
245202170810     C                   MOVEL     '05'          OPZI74
245203170810     C                   Else
245300170808     C                   MOVEL     '02'          OPZI74
245301170810     C                   Endif
245400170807     C     CMTI75        IFEQ      '1'
245500170807     C                   MOVEL     '1'           CMTI74
245600170807     C                   ELSE
245700170807     C                   MOVEL     '0'           CMTI74
245800170807     C                   ENDIF
245900170807     C*
246000170807     C                   Move      XScSoc        SocietaI74
246100170807     C*
246200170807     C                   Eval      AnnoI74     = C1Anno
246300170807     C                   Eval      TrimI74     = C1Trim
246400170807     C*
246500170807     C                   Eval      TpComI74    = TpComI75
246600170807     C                   Eval      TpRecI74    = D1TpRec
246700170807     C                   Eval      NrRCSTI74   = H1NrRCST
246800170807     C*
246900170807     C                   CALL      'SDGSI74R'
247000170807     C                   PARM                    KPJBA
247100170807    >C                   PARM                    SDGSI74DS
247200170807     C*
247300170807     C* ritorno da PGM gestione
247400170807     C                   EXSR      RTNI74
247500170807     C                   MOVE      *HIVAL        $LASTV
247600000000     C*
247700000000     C* se opz=41=contabilità
247800000000     C     S1OPZ         WHENEQ    41
247900000000     C                   RESET                   NDC001DS
248000000000     C                   MOVEL     '02'          OPZ001
248100000000     C                   if        S1Origine   = 'A'
248200000000     C                   MOVEL     H1Sys         Uq1001
248300000000     C                   MOVEL     H1NrAsReg     Uq2001
248400000000     C*
248500000000     C                   MOVEL     H1Sys         SYS001
248600000000     C                   MOVEL     H1NrAsReg     NRASREG001
248700000000     C*
248800000000     C                   endif
248900000000     C*
249000000000     C                   if        S1DtReg    <> 0
249100000000     C     *Jobrun       Move      S1DtReg       DTREG1001
249200000000     C     *Jobrun       Move      S1DtReg       DTREG2001
249300000000     C                   endif
249400000000     C                   Z-Add     H1NrReg       NrReg001
249500000000     C                   MoveL     H1SerReg      SerReg001
249600000000     C*
249700000000     C                   if        S1DtDoc    <> 0
249800000000     C     *Jobrun       Move      S1DtDoc       DTDOC001
249900000000     C                   endif
250000000000     C                   Z-Add     H1NrDoc       NrDoc001
250100000000     C                   MoveL     H1SerDoc      SerDoc001
250200000000     C*
250300000000     C*
250400000000     C                   MoveL(P)  NDC001DS      KPJBU
250500000000     C                   CALL      'NDC001R'
250600000000     C                   PARM                    KPJBA
250700000000     C*
250800000000     C                   MoveL     KPJBU         NDC001DS
250900000000     C*
251000000000     C* ritorno da PGM gestione
251100000000     C                   EXSR      RTN001
251200000000     C                   MOVE      *HIVAL        $LASTV
251300000000     C*
251400000000     C* se opz=Annulla/Ripristina
251500000000     C     S1OPZ         WHENEQ    44
251600000000     C*
251700000000     C                   exsr      AnnullaRip
251800000000     C*
251900000000     C                   MOVE      *HIVAL        $LASTV
252000000000     C*
252100000000     C* se selezionata opzione personalizzata, richiamo il programma
252200000000     C* corrispondente
252300000000     C     $PGO1(PosOpz) Whenne    *Blank
252400170804    >C                   RESET                   SDGSI76DS
252500170804    >C                   MOVE      xscsoc        SocietaI76
252600170804    >C                   MOVEL     S1OPZ         OPZI76
252700170804    >C     CMTI75        IFEQ      '1'
252800170804    >C                   MOVEL     '1'           CMTI76
252900000000     C                   ELSE
253000170804    >C                   MOVEL     '0'           CMTI76
253100000000     C                   ENDIF
253200170804     C* passare nei campi appositi della DS SDGSI76DS il valore
253300000000     C* della chiave della VL univoca
253400000000     C*
253500170804    >C                   MOVEL     SDGSI76DS     KPJBU
253600000000     C                   CALL      $PGO1(PosOpz)                        22
253700000000     C                   PARM                    KPJBA
253800000000     C****               PARM                    NOMPGM
253900000000     C*
254000170804    >C                   MOVEL     KPJBU         SDGSI76DS
254100000000     C* ritorno da PGM
254200170804    >C                   EXSR      RTNI76
254300000000     C*
254400000000     C* se altre opzioni
254500000000     C* richiamo pgm gestione anagrafica
254600000000     C                   OTHER
254700000000     C*
254800170804     C                   RESET                   SDGSI76DS
254900000000     C*
255000170804     C                   MOVEL     S1OPZ         OPZI76
255100170804     C     CMTI75        IFEQ      '1'
255200170804     C                   MOVEL     '1'           CMTI76
255300000000     C                   ELSE
255400170804     C                   MOVEL     '0'           CMTI76
255500000000     C                   ENDIF
255600000000     C*
255700170804     C                   Move      XScSoc        SocietaI76
255800170804     C*
255900170804     C                   Eval      TpAInpI76   = D1TpAnn
256000170804     C                   Eval      CausInpI76  = D1Causale
256100170804     C                   Eval      SoggInpI76  = D1Sogg
256200170804     C*
256300170804     C                   Eval      AnnoI76     = C1Anno
256400170804     C                   Eval      TrimI76     = C1Trim
256500170804     C                   Eval      PartIvaI76  = C1PartIva
256600170804     C                   Eval      CdFiscI76   = C1CdFisc
256700170804     C                   Eval      CogRag1I76  = C1CogRag1
256800170804     C                   Eval      Stato1I76   = C1Stato1
256900170804     C*
257000170804     C                   Eval      TpComI76    = TpComI75
257100170804     C                   Eval      TpRecI76    = D1TpRec
257200170804     C                   Eval      CfI76       = D1Cf
257300170804     C                   Eval      SoggInpI76  = D1Sogg
257400170804     C                   Eval      PartivaI76  = D1Partiva
257500170804     C                   Eval      CdFiscI76   = D1CdFisc
257600170804     C*
257700170804     C                   Eval      AnnI76      = D1Ann
257800170804     C                   Eval      TpAInpI76   = D1TpAnn
257900170804     C*
258000170804     C*
258100170804     C                   if        OpzI75     <> *Blank
258200170804     C                   Eval      Stato1I76   = Stato1I75
258300170804     C                   Eval      IdPaes1I76  = IdPaes1I75
258400170804     C                   Eval      PIVA1I76    = PIVA1I75
258500170804     C                   Eval      CdFisc1I76  = CdFisc1I75
258600170804     C                   Eval      CogRag1I76  = CogRag1I75
258700170804     C                   Eval      TpN1I76     = TpN1I75
258800170804     C                   Eval      Denom1I76   = Denom1I75
258900170804     C                   Eval      Cognom1I76  = Cognom1I75
259000170804     C                   Eval      Nome1I76    = Nome1I75
259100170804     C*
259200170804     C                   Eval      Indir2I76   = Indir2I75
259300170804     C                   Eval      NrCiv2I76   = NrCiv2I75
259400170804     C                   Eval      CAP2I76     = CAP2I75
259500170804     C                   Eval      Locali2I76  = Locali2I75
259600170804     C                   Eval      Prov2I76    = Prov2I75
259700170804     C                   Eval      Stato2I76   = Stato2I75
259800170804     C                   Eval      IdPaes2I76  = IdPaes2I75
259900170804     C*
260000170804     C                   Eval      Indir3I76   = Indir3I75
260100170804     C                   Eval      NrCiv3I76   = NrCiv3I75
260200170804     C                   Eval      CAP3I76     = CAP3I75
260300170804     C                   Eval      Locali3I76  = Locali3I75
260400170804     C                   Eval      Prov3I76    = Prov3I75
260500170804     C                   Eval      Stato3I76   = Stato3I75
260600170804     C                   Eval      IdPaes3I76  = IdPaes3I75
260700170804     C*
260800170804     C                   Eval      Stato4I76   = Stato4I75
260900170804     C                   Eval      IdPaes4I76  = IdPaes4I75
261000170804     C                   Eval      PIVA4I76    = PIVA4I75
261100170804     C                   Eval      CogRag4I76  = CogRag4I75
261200170804     C                   Eval      TpN4I76     = TpN4I75
261300170804     C                   Eval      Denom4I76   = Denom4I75
261400170804     C                   Eval      Cognom4I76  = Cognom4I75
261500170804     C                   Eval      Nome4I76    = Nome4I75
261600170804     C                   endif
261700170804     C*
261800170804     C                   Eval      SysI76      = H1Sys
261900170804     C                   Eval      NrAsRegI76  = H1NrAsReg
262000170804     C                   Eval      NrRigaMI76  = H1NrRigaM
262100170804     C                   Eval      NrRigaI76   = H1NrRiga
262200170804     C                   Eval      NrRCSTI76   = H1NrRCST
262300000000     C*
262400170804     C                   CALL      'SDGSI76R'
262500000000     C                   PARM                    KPJBA
262600170804    >C                   PARM                    SDGSI76DS
262700000000     C*
262800000000     C* ritorno da PGM gestione
262900170804     C                   EXSR      RTNI76
263000000000     C*
263100000000     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
263200000000     C     *IN99         IFEQ      *ON
263300000000     C                   SETON                                        40
263400000000     C                   ENDIF
263500000000     C*
263600000000     C                   MOVE      *HIVAL        $LASTV
263700000000     C                   ENDSL
263800000000     C*
263900000000     C                   ENDSR
264000000000     C/EJECT
264100000000     C************************************************************
264200000000     C* Interrogazioni conto
264300000000     C************************************************************
264400000000     C     OpzS1_15      BEGSR
264500000000     C*
264600000000     C                   Reset                   ANA399DS
264700000000     C*
264800000000     C                   Eval      A39Societa  = XScSoc
264900000000     C                   Eval      A39Ctb      = 'CG'
265000000000     C                   Eval      A39Kcc      = S1Kcc
265100000000     C                   Eval      A39Ksc      = S1Ksc
265200000000     C                   Eval      A39Iniz     = WA39Iniz
265300000000     C*
265400000000     C                   MoveL     ANA399DS      KPJBU
265500000000     C*
265600000000     C                   CALL      'ANA399R'                            21
265700000000     C                   PARM                    KPJBA
265800000000     C*
265900000000     C                   MoveL     KPJBU         ANA399DS
266000000000     C*
266100000000     C* metto 1 in  wa39iniz per non far fare più la close
266200000000     C                   Eval      WA39Iniz    = '1'
266300000000     C*
266400000000     C                   ENDSR
266500000000     C/EJECT
266600000000     C************************************************************
266700000000     C* GESTIONE RITORNO DA PGM primanota generica
266800000000     C************************************************************
266900000000     C     RTN001        BEGSR
267000000000     C*
267100000000     C* modo di ritorno
267200000000     C*
267300000000     C                   SELECT
267400000000     C* 1 = F3
267500000000     C     RET001        WHENEQ    '1'
267600000000     C                   MOVE      *ON           $ESCI
267700000000     C                   MOVE      *ON           $FINE
267800170804     C                   MOVE      '1'           RetI75
267900000000     C* 2 = F12
268000000000     C     RET001        WHENEQ    '2'
268100000000     C     opr001        ANDNE     '1'
268200000000     C                   MOVE      *ON           $ESCI
268300000000     C*
268400000000     C                   ENDSL
268500000000     C*
268600000000     C* operazione eseguite dal pgm chiamato
268700000000     C*
268800000000     C                   SELECT
268900000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
269000000000     C     OPR001        WHENEQ    '1'
269100000000     C                   MOVE      OPR001        WOPR                           avverto pgm chiamant
269200000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
269300000000     C*
269400000000     C                   ENDSL
269500000000     C*
269600000000     C* funzione non eseguibile per errore :
269700000000     C*
269800000000     C                   SELECT
269900000000     C* 1 = funzione richiamata chiusa in errore
270000000000     C     ERR001        WHENEQ    '1'
270100000000     C                   MOVE      *ON           $ESCI
270200000000     C                   SETON                                        9399
270300000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
270400000000     C     ERR001        WHENEQ    '2'
270500000000     C                   MOVE      *ON           $ESCI
270600000000     C                   SETON                                        9499
270700000000     C*
270800000000     C                   ENDSL
270900000000     C*
271000000000     C                   ENDSR
271100170807     C/EJECT
271200170807     C************************************************************
271300170807     C* GESTIONE RITORNO DA PGM DI DATI ANGRAFICI
271400170807     C************************************************************
271500170807    >C     RTNI74        BEGSR
271600170807     C*
271700170807     C* modo di ritorno
271800170807     C*
271900170807     C                   SELECT
272000170807     C* questi modi di utilizzo dei valori di ritorno dal
272100170807     C* pgm di manutenzione rcd di anagrafica sono delle
272200170807     C* proposte, normalmente sempre valide, ma modificabili
272300170807     C* per situazioni particolari
272400170807     C* 1 = F3
272500170807    >C     RETI74        WHENEQ    '1'
272600170807     C                   MOVE      *ON           $ESCI                          sospese READC
272700170807     C                   MOVE      *ON           $FINE                          fine pgm
272800170807    >C                   MOVE      '1'           RETI75
272900170807     C* 2 = F12
273000170807    >C     RETI74        WHENEQ    '2'
273100170807     C* se il pgm di manutenzione ha prima una guida, è necessario anche
273200170807     c* questo test, altrimenti prevale il retI76 e il sfl non viene
273300170807     C* ricaricato
273400170807    >C     OPRI74        ANDNE     '1'
273500170807     C*
273600170807     C* se l'opzione con cui è stato chiamato non è la modifica
273700170807     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
273800170807     C                   MOVE      *ON           $ESCI                          sospese READC
273900170807     C*
274000170807     C                   ENDSL
274100170807     C*
274200170807     C* operazione eseguite dal pgm chiamato
274300170807     C*
274400170807     C                   SELECT
274500170807     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
274600170807    >C     OPRI74        WHENEQ    '1'
274700170807    >C                   MOVE      OPRI74        WOPR                           avverto pgm chiamant
274800170807     C*
274900170807     C* se l'opzione con cui è stato chiamato non è la modifica
275000170807     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
275100170808    >C     OPZI74        IFEQ      '02'
275200170807     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
275300170807     C* se l'opzione con cui è stato chiamato è la modifica
275400170807     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
275500170807     C                   ELSE
275600170807     C                   EXSR      AGGS1
275700170807     C                   ENDIF
275800170807     C*
275900170807     C                   ENDSL
276000170807     C*
276100170807     C* funzione non eseguibile per errore :
276200170807     C*
276300170807     C                   SELECT
276400170807     C* 1 = funzione richiamata chiusa in errore
276500170807    >C     ERRI74        WHENEQ    '1'
276600170807     C                   MOVE      *ON           $ESCI                          sospese READC
276700170807     C                   SETON                                        9399      emiss.SFLMSGID
276800170807     C* 2 = funzione richiamata senza la dovuta autorizzazione
276900170807    >C     ERRI74        WHENEQ    '2'
277000170807     C                   MOVE      *ON           $ESCI                          sospese READC
277100170807     C                   SETON                                        9499      emiss.SFLMSGID
277200170807     C*
277300170807     C                   ENDSL
277400170807     C*
277500170807     C                   ENDSR
277600000000     C/EJECT
277700000000     C************************************************************
277800000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
277900000000     C************************************************************
278000170804    >C     RTNI76        BEGSR
278100000000     C*
278200000000     C* modo di ritorno
278300000000     C*
278400000000     C                   SELECT
278500000000     C* questi modi di utilizzo dei valori di ritorno dal
278600000000     C* pgm di manutenzione rcd di anagrafica sono delle
278700000000     C* proposte, normalmente sempre valide, ma modificabili
278800000000     C* per situazioni particolari
278900000000     C* 1 = F3
279000170804    >C     RETI76        WHENEQ    '1'
279100000000     C                   MOVE      *ON           $ESCI                          sospese READC
279200000000     C                   MOVE      *ON           $FINE                          fine pgm
279300170804    >C                   MOVE      '1'           RETI75
279400000000     C* 2 = F12
279500170804    >C     RETI76        WHENEQ    '2'
279600000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
279700170804     c* questo test, altrimenti prevale il retI76 e il sfl non viene
279800000000     C* ricaricato
279900170804    >C     OPRI76        ANDNE     '1'
280000000000     C*
280100000000     C* se l'opzione con cui è stato chiamato non è la modifica
280200000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
280300000000     C                   MOVE      *ON           $ESCI                          sospese READC
280400000000     C*
280500000000     C                   ENDSL
280600000000     C*
280700000000     C* operazione eseguite dal pgm chiamato
280800000000     C*
280900000000     C                   SELECT
281000000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
281100170804    >C     OPRI76        WHENEQ    '1'
281200170804    >C                   MOVE      OPRI76        WOPR                           avverto pgm chiamant
281300000000     C*
281400000000     C* se l'opzione con cui è stato chiamato non è la modifica
281500000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
281600170804    >C     OPZI76        IFNE      '02'
281700000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
281800000000     C* se l'opzione con cui è stato chiamato è la modifica
281900000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
282000000000     C                   ELSE
282100000000     C                   EXSR      AGGS1
282200000000     C                   ENDIF
282300000000     C*
282400000000     C                   ENDSL
282500000000     C*
282600000000     C* funzione non eseguibile per errore :
282700000000     C*
282800000000     C                   SELECT
282900000000     C* 1 = funzione richiamata chiusa in errore
283000170804    >C     ERRI76        WHENEQ    '1'
283100000000     C                   MOVE      *ON           $ESCI                          sospese READC
283200000000     C                   SETON                                        9399      emiss.SFLMSGID
283300000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
283400170804    >C     ERRI76        WHENEQ    '2'
283500000000     C                   MOVE      *ON           $ESCI                          sospese READC
283600000000     C                   SETON                                        9499      emiss.SFLMSGID
283700000000     C*
283800000000     C                   ENDSL
283900000000     C*
284000000000     C                   ENDSR
284100000000     C/EJECT
284200000000     C************************************************************
284300000000     C* Annulla/Ripristina
284400000000     C************************************************************
284500000000     C     AnnullaRip    BEGSR
284501170808     C*
284502170808     C                   If        Not %Open(sdgcst00f)
284503170808     C                   Open      sdgcst00f
284504170808     C                   Endif
284600130906     C*
284700170804     C     H1NrRCST      CHAIN     SDGCST00F                          2122
284800130906     C                   if        *In21       = *Off
284900130906     C*
285000130906     C                   select
285100130906     C* Annulla
285200130906     C                   when      H1Anna      = *Blank
285300170804     C                   eval      CSTAnn      = '1'
285400170804     C                   Eval      CSTTpAnn    = '00'
285500130906     C* Ripristina
285600130906     C                   when      H1Anna      = '1'
285700170804     C                   eval      CSTAnn      = *Blank
285800170804     C                   Eval      CSTTpAnn    = *Blank
285900130906     C                   endsl
286000130906     C*
286100130906     C                   move      *on           WOPR
286200130906     C                   move      *on           WINZS1
286300130906     C*
286400170804     C                   Eval      CSTDtUlMo   = UDateIso
286500170804     C                   Eval      CSTUteUlMo  = XScCUt
286600170804     C                   Eval      CSTOrigine  = 'M'
286700130906     C*
286800170804     C                   UPDATE    SDGCST000
286900000000     C*
287000000000     C                   endif
287001170808     C*
287002170808     C                   If        %Open(sdgcst00f)
287003170808     C                   Unlock    sdgcst00f
287004170808     C                   Close     sdgcst00f
287005170808     C                   Endif
287100000000     C*
287200000000     C                   ENDSR
287300000000     C/EJECT
287400000000     C************************************************************
287500000000     C* AGGIORNAMENTO RIGA LISTA
287600000000     C************************************************************
287700000000     C     AGGS1         BEGSR
287800000000     C*
287900000000     C* riempio i campi del sfl che sono stati ripassati dal pgm
288000000000     C* di manutenzione dopo una opzione 02
288100130924     C*
288200170804     C                   Eval      H1TpDoc     = TpDocI76
288300170804     C*
288400130924     C* se nota di variazione, sottolineo riferimenti documento
288500170804     C                   if        TpDocI76    = 'TD04'   or
288600170804     C                             TpDocI76    = 'TD05'   or
288700170804     C                             ImpOpeI76   < 0        or
288800170804     C                             IVAOpeI76   < 0
288900130924     C                   Eval      *In12       = *On
289000130924     C                   else
289100130924     C                   Eval      *In12       = *Off
289200130924     C                   endif
289300130924     C*
289400130924     C*
289500170804     C                   Eval      S1Kcc       = KccI76
289600170804     C                   Eval      S1Ksc       = KscI76
289700170804     C                   if        DtDocI76   <> *Loval
289800170804     C     *JobRun       Move      DtDocI76      S1DtDoc
289900130924     C                   endif
290000170804     C                   Eval      H1NrDoc     = NrDocI76
290100170804     C                   Eval      H1SerDoc    = SerDocI76
290200170804     C                   if        DtRegI76   <> *Loval
290300170804     C     *JobRun       Move      DtRegI76      S1DtReg
290400130924     C                   endif
290500170804     C                   Eval      H1NrReg     = NrRegI76
290600170804     C                   Eval      H1SerReg    = SerRegI76
290700170804     C                   Eval      S1Origine   = OrigineI76
290800170804     C                   Eval      H1Anna      = AnnI76
290900170804     C                   Eval      S1ImpOpe    = ImpOpeI76
291000170804     C                   Eval      S1IVAOpe    = IVAOpeI76
291100170804     C                   Eval      S1Causale   = CausaleI76
291200170804     C                   Eval      S1Aliq      = AliqI76
291300170804     C                   Eval      S1RifIVA    = RifIVAI76
291400000000     C*
291500000000     C                   ENDSR
291600000000     C/EJECT
291700000000     C************************************************************
291800000000     C* GESTIONE VALORI ASSUNTI
291900000000     C************************************************************
292000000000     C     GESD2         BEGSR
292100000000     C*
292200000000   B1C     $INZD2        IFEQ      *ON
292300000000     C                   EXSR      INZD2
292400000000     C                   MOVE      *OFF          $INZD2
292500000000   E1C                   ENDIF
292600000000     C*
292700000000   B1C     $LASTV        IFNE      'D2'
292800000000     C                   EXSR      GESPIE
292900000000   B1C     $LASTV        IFNE      *HIVAL
293000000000     C                   MOVE      $LASTV        $LASTG
293100000000     C                   ENDIF
293200000000     C                   MOVE      'D2'          $LASTV
293300000000   E1C                   ENDIF
293400000000     C*
293500000000     C* se esistono errori sulla videata
293600000000     C* emetto la write del formato a indicatori spenti per vedere
293700000000     C* le eventuali decodifiche
293800000000   B1C     *IN99         IFEQ      *ON
293900000000     C                   MOVEA     *IN           WIN
294000000000     C                   MOVE      *ALL'0'       IN5098           49
294100000000     C                   MOVEA     IN5098        *IN(50)
294200000000     C                   WRITE     FMTD2
294300000000     C                   MOVEA     WIN           *IN
294400000000   E1C                   ENDIF
294500000000     C*
294600000000     C                   EXFMT     FMTD2
294700000000     C*
294800000000     C* azzero la posizione del cursore
294900000000     C                   Z-ADD     0             H1RIGA
295000000000     C                   Z-ADD     0             H1COLO
295100000000     C*
295200000000     C* CONTROLLO ABILITAZIONE TASTI
295300000000     C* INPUT
295400000000     C* $FC3 = Abilitazioni videata D2
295500000000     C* OUTPUT
295600000000     C* *IN97 + *IN99 Errori
295700000000     C                   MOVEA     $FC3          $FC
295800000000     C                   EXSR      CTRKEY
295900000000     C*
296000000000   B1C     *IN99         IFEQ      *OFF
296100000000     C*
296200000000   B2C                   SELECT
296300000000     C* F3=Fine
296400000000     C     $TASTO        WHENEQ    F03
296500000000     C                   EXSR      F03D2
296600000000     C* F12=Ritorno
296700000000     C     $TASTO        WHENEQ    F12
296800000000     C                   EXSR      F12D2
296900000000     C* F24=Altri tasti
297000000000     C     $TASTO        WHENEQ    F24
297100000000     C                   EXSR      F24D2
297200000000     C*
297300000000     C                   OTHER
297400000000     C                   EXSR      CTRD2
297500000000   B3C     *IN99         IFEQ      *OFF
297600000000     C                   MOVE      $LASTG        $GEST
297700000000   B4C                   SELECT
297800000000     C     $LASTG        WHENEQ    'S1'
297900000000     C* se è stato modificato l'ordinamento di default, ricarico la lista
298000000000   B5C     $ORD          IFNE      D2ORD
298100000000     C                   MOVE      *ON           $INZS1
298200000000     C                   MOVE      *BLANK        W1POS
298300000000   E5C                   ENDIF
298400000000     C     $LASTG        WHENEQ    'D1'
298500000000     C* per ora niente
298600000000   E4C                   ENDSL
298700000000     C* definisco i nuovi valori di default
298800000000     C                   Z-ADD     D2LOP         $LOP
298900000000     C                   Z-ADD     D2ORD         $ORD
299000000000     C*
299100000000     C* aggiorno valori assunti
299200000000     C* se richiesto il salvataggio
299300000000   B4C     D2BSV         IFEQ      1
299400000000     C                   EXSR      AGGMPE
299500000000   E4C                   ENDIF
299600000000     C*
299700000000   E3C                   ENDIF
299800000000     C*
299900000000   E2C                   ENDSL
300000000000     C*
300100000000   E1C                   ENDIF
300200000000     C*
300300000000     C                   ENDSR
300400000000     C/EJECT
300500000000     C************************************************************
300600000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
300700000000     C************************************************************
300800000000     C     INZD2         BEGSR
300900000000     C*
301000000000     C                   CLEAR                   FMTD2
301100000000     C                   EVAL      D2BSVA = Normale
301200000000     C* se sono entrato con chiave univoca
301300000000     C* impedisco il salvataggio valori assunti
301400000000     C     $Unique       IFEQ      *ON
301500000000     C                   EVAL      D2BSVA = Protetto
301600000000     C                   ENDIF
301700000000     C                   MOVEL     $LOP          D2LOP
301800000000     C                   MOVE      $ORD          D2ORD
301900000000     C*
302000000000     C                   ENDSR
302100000000     C/EJECT
302200000000     C************************************************************
302300000000     C* GESTIONE F03 VIDEO D2
302400000000     C************************************************************
302500000000     C     F03D2         BEGSR
302600000000     C*
302700000000     C                   MOVE      *ON           $FINE
302800000000     C* fine programma
302900000000     C                   EXSR      ENDPGM
303000000000     C*
303100000000     C                   ENDSR
303200000000     C/EJECT
303300000000     C************************************************************
303400000000     C* GESTIONE F12 VIDEO D2
303500000000     C************************************************************
303600000000     C     F12D2         BEGSR
303700000000     C*
303800000000     C                   MOVE      $LASTG        $GEST
303900000000     C*
304000000000     C                   ENDSR
304100000000     C/EJECT
304200000000     C************************************************************
304300000000     C* GESTIONE F24 VIDEO D2
304400000000     C************************************************************
304500000000     C     F24D2         BEGSR
304600000000     C*
304700000000     C                   MOVEA     $FX3          $FX
304800000000     C                   MOVEA     $FC3          $FC
304900000000     C                   MOVEA     $FR3          $FR
305000000000     C                   Z-ADD     $ULKD2        $ULK
305100000000     C                   Z-ADD     $D2L01        $L01
305200000000     C                   Z-ADD     $D2L02        $L02
305300000000     C                   EXSR      RIEKEY
305400000000     C                   Z-ADD     $ULK          $ULKD2            3 0
305500000000     C                   MOVEA     $FC           $FC3
305600000000     C                   MOVEL     $KEY1         Z3KE1
305700000000     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
305800000000     C                   WRITE     FMTZ3
305900000000     C*
306000000000     C                   ENDSR
306100000000     C/EJECT
306200000000     C************************************************************
306300000000     C* CONTROLLO VIDEATA VALORI ASSUNTI
306400000000     C************************************************************
306500000000     C     CTRD2         BEGSR
306600000000     C*
306700000000     C                   SETOFF                                       99
306800000000     C*
306900000000     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
307000000000     C     D2LOP         IFEQ      0
307100000000     C                   SETON                                          5099
307200000000     C                   ENDIF
307300000000     C*
307400000000     C* OBBLIGATORIO VALORE PER ORDINAMENTO
307500000000     C     D2ORD         IFEQ      0
307600000000     C                   SETON                                          5199
307700000000     C                   ENDIF
307800000000     C*
307900000000     C                   ENDSR
308000000000     C/EJECT
308100000000     C************************************************************
308200000000     C* CONTROLLO ABILITAZIONE TASTI
308300000000     C* INPUT
308400000000     C* $TASTO =Tasto funzione premuto
308500000000     C* $FC = Tasti abilitati
308600000000     C* OUTPUT
308700000000     C* IN99 = ERrore
308800000000     C************************************************************
308900000000     C     CTRKEY        BEGSR
309000000000     C*
309100000000     C                   SETOFF                                       99
309200000000     C*
309300000000     C                   SELECT
309400000000     C     $TASTO        WHENEQ    F01
309500000000     C                   z-add     01            posfun
309600000000     C     $FC(1)        IFEQ      '0'
309700000000     C                   SETON                                        9997
309800000000     C                   ENDIF
309900000000     C     $TASTO        WHENEQ    F02
310000000000     C                   z-add     02            posfun
310100000000     C     $FC(2)        IFEQ      '0'
310200000000     C                   SETON                                        9997
310300000000     C                   ENDIF
310400000000     C     $TASTO        WHENEQ    F03
310500000000     C                   z-add     03            posfun
310600000000     C     $FC(3)        IFEQ      '0'
310700000000     C                   SETON                                        9997
310800000000     C                   ENDIF
310900000000     C     $TASTO        WHENEQ    F04
311000000000     C                   z-add     04            posfun
311100000000     C     $FC(4)        IFEQ      '0'
311200000000     C                   SETON                                        9997
311300000000     C                   ENDIF
311400000000     C     $TASTO        WHENEQ    F05
311500000000     C                   z-add     05            posfun
311600000000     C     $FC(5)        IFEQ      '0'
311700000000     C                   SETON                                        9997
311800000000     C                   ENDIF
311900000000     C     $TASTO        WHENEQ    F06
312000000000     C                   z-add     06            posfun
312100000000     C     $FC(6)        IFEQ      '0'
312200000000     C                   SETON                                        9997
312300000000     C                   ENDIF
312400000000     C     $TASTO        WHENEQ    F07
312500000000     C                   z-add     07            posfun
312600000000     C     $FC(7)        IFEQ      '0'
312700000000     C                   SETON                                        9997
312800000000     C                   ENDIF
312900000000     C     $TASTO        WHENEQ    F08
313000000000     C                   z-add     08            posfun
313100000000     C     $FC(8)        IFEQ      '0'
313200000000     C                   SETON                                        9997
313300000000     C                   ENDIF
313400000000     C     $TASTO        WHENEQ    F09
313500000000     C                   z-add     09            posfun
313600000000     C     $FC(09)       IFEQ      '0'
313700000000     C                   SETON                                        9997
313800000000     C                   ENDIF
313900000000     C     $TASTO        WHENEQ    F10
314000000000     C                   z-add     10            posfun
314100000000     C     $FC(10)       IFEQ      '0'
314200000000     C                   SETON                                        9997
314300000000     C                   ENDIF
314400000000     C     $TASTO        WHENEQ    F11
314500000000     C                   z-add     11            posfun
314600000000     C     $FC(11)       IFEQ      '0'
314700000000     C                   SETON                                        9997
314800000000     C                   ENDIF
314900000000     C     $TASTO        WHENEQ    F12
315000000000     C                   z-add     12            posfun
315100000000     C     $FC(12)       IFEQ      '0'
315200000000     C                   SETON                                        9997
315300000000     C                   ENDIF
315400000000     C     $TASTO        WHENEQ    F13
315500000000     C                   z-add     13            posfun
315600000000     C     $FC(13)       IFEQ      '0'
315700000000     C                   SETON                                        9997
315800000000     C                   ENDIF
315900000000     C     $TASTO        WHENEQ    F14
316000000000     C                   z-add     14            posfun
316100000000     C     $FC(14)       IFEQ      '0'
316200000000     C                   SETON                                        9997
316300000000     C                   ENDIF
316400000000     C     $TASTO        WHENEQ    F15
316500000000     C                   z-add     15            posfun
316600000000     C     $FC(15)       IFEQ      '0'
316700000000     C                   SETON                                        9997
316800000000     C                   ENDIF
316900000000     C     $TASTO        WHENEQ    F16
317000000000     C                   z-add     16            posfun
317100000000     C     $FC(16)       IFEQ      '0'
317200000000     C                   SETON                                        9997
317300000000     C                   ENDIF
317400000000     C     $TASTO        WHENEQ    F17
317500000000     C                   z-add     17            posfun
317600000000     C     $FC(17)       IFEQ      '0'
317700000000     C                   SETON                                        9997
317800000000     C                   ENDIF
317900000000     C     $TASTO        WHENEQ    F18
318000000000     C                   z-add     18            posfun
318100000000     C     $FC(18)       IFEQ      '0'
318200000000     C                   SETON                                        9997
318300000000     C                   ENDIF
318400000000     C     $TASTO        WHENEQ    F19
318500000000     C                   z-add     19            posfun
318600000000     C     $FC(19)       IFEQ      '0'
318700000000     C                   SETON                                        9997
318800000000     C                   ENDIF
318900000000     C     $TASTO        WHENEQ    F20
319000000000     C                   z-add     20            posfun
319100000000     C     $FC(20)       IFEQ      '0'
319200000000     C                   SETON                                        9997
319300000000     C                   ENDIF
319400000000     C     $TASTO        WHENEQ    F21
319500000000     C                   z-add     21            posfun
319600000000     C     $FC(21)       IFEQ      '0'
319700000000     C                   SETON                                        9997
319800000000     C                   ENDIF
319900000000     C     $TASTO        WHENEQ    F22
320000000000     C                   z-add     22            posfun
320100000000     C     $FC(22)       IFEQ      '0'
320200000000     C                   SETON                                        9997
320300000000     C                   ENDIF
320400000000     C     $TASTO        WHENEQ    F23
320500000000     C                   z-add     23            posfun
320600000000     C     $FC(23)       IFEQ      '0'
320700000000     C                   SETON                                        9997
320800000000     C                   ENDIF
320900000000     C     $TASTO        WHENEQ    F24
321000000000     C                   z-add     24            posfun
321100000000     C     $FC(24)       IFEQ      '0'
321200000000     C                   SETON                                        9997
321300000000     C                   ENDIF
321400000000     C                   ENDSL
321500000000     C*
321600000000     C                   ENDSR
321700000000     C/EJECT
321800000000     C************************************************************
321900000000     C* AGGIORNAMENTO VALORI ASSUNTI
322000000000     C************************************************************
322100000000     C     AGGMPE        BEGSR
322200000000     C*
322300000000     C* preparo i parametri di chiamata al driver
322400000000     C                   CLEAR                   XMPEDS
322500000000     C                   EVAL      TpChiamata = '03'
322600000000     C                   EVAL      NChiavi = 5
322700000000     C                   EVAL      VLogica = 2
322800000000     C*
322900000000     C* preparo le chiavi per la chiamata al driver
323000000000     C                   CLEAR                   XMPEDS1
323100000000     C                   MOVEL     XSCSOC        DSMPESOC
323200000000     C                   MOVEL     DSPGM         DSMPEPGM
323300000000     C                   MOVEL     KNMUS         DSMPEPRF
323400000000     C*
323500000000     C* Si memorizzano i dati di default a seconda delle modalità con
323600000000     C* cui il pgm è usato
323700000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
323800000000     C* MPETIP = 'PV'  pgm richiamato da menù
323900000000     C* MPETIP = 'PB'  x lancio Batch
324000000000     C*
324100000000     C                   SELECT
324200170804    >C     OPZI75        WHENEQ    '01'
324300000000     C                   MOVEL     'PX'          DSMPETIP
324400000000     C                   OTHER
324500000000     C                   MOVEL     'PV'          DSMPETIP
324600000000     C                   ENDSL
324700000000     C* dati
324800000000     C                   MOVEL     DSMPE         DSMPEPAR
324900000000     C*
325000000000     C                   CALLB     'XMPE'
325100000000     C                   PARM                    XMPEDS
325200000000     C                   PARM                    XMPEDS1
325300000000     C*
325400000000     C                   ENDSR
325500000000     C/EJECT
325600000000     C************************************************************
325700000000     C* OPERAZIONI INIZIALI
325800000000     C************************************************************
325900000000     C     *INZSR        BEGSR
326000000000     C*
326100000000     C* Reperimento parametri
326200000000     C*
326300000000     C     *ENTRY        PLIST
326400000000     C                   PARM                    KPJBA
326500170804     C                   PARM                    SDGSI75DS
326609170810     C*
326700000000     C*
326800000000     C* Reperimento dati società
326900000000     C*
327000000000     C                   MOVEL     'SOC001'      TIPXSC
327100000000     C                   MOVEL     *BLANK        SOCXSC
327200000000     C                   EXSR      REPSOC
327300000000     C*
327400000000     C* Controlli dati società
327500000000     C*
327600000000     C     RTNXSC        IFNE      '1'
327700000000     C                   MOVEL     XSOCDS        SOC001
327800000000     C                   ELSE
327900000000     C                   MOVE      *ON           $FINE
328000170804    >C                   MOVE      '1'           ErrI75
328100000000     C                   EXSR      ENDPGM
328200000000     C                   ENDIF
328300000000     C*
328400000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
328500000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
328600000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
328700000000     C                   EVAL      $S1L01 = %Size(Z2Ke1)
328800000000     C                   EVAL      $S1L02 = %Size(Z2Ke2)
328900000000     C                   EVAL      $C1Op1 = %Size(C1Opz1)
329000000000     C                   EVAL      $C1Op2 = %Size(C1Opz2)
329100000000     C                   EVAL      $D2L01 = %Size(Z3Ke1)
329200000000     C                   EVAL      $D2L02 = 0
329300000000     C*
329400000000     C* Reperimento opzioni
329500000000     C*
329600000000     C                   EXSR      REPOPZ
329700000000     C*
329800000000     C* Reperimento tasti di funzione
329900000000     C*
330000000000     C                   EXSR      REPKEY
330100000000     C* Chiamata a routine per il reperimento delle opzioni
330200000000     C* personalizzate
330300000000    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
330400000000    >C* ripeterla passando le schiere dei tasti funzione
330500000000     C                   CAllb     'X46USREXT'
330600000000     C                   PARM                    $OP1
330700000000     C                   PARM                    $TX1
330800000000     C                   PARM                    $FL1
330900000000     C                   PARM                    $PGO1
331000000000     C                   PARM      $dimOp        NumOpz
331100000000     C                   PARM                    $FX2
331200000000     C                   PARM                    $FC2
331300000000     C                   PARM                    $PGF2
331400000000     C                   PARM                    DsPgm
331500000000     C                   PARM                    xsccut
331600000000     C                   PARM                    xscgrp
331700000000     C*
331800000000     C                   Z-add     1             OpzLun
331900000000     C     *blank        Lookup    $TX1(OpzLun)                           21
332000000000     C                   if        *in21 = *on
332100000000     C                   sub       1             OpzLun
332200000000     C                   else
332300000000     C                   Z-add     $DimOp        OpzLun
332400000000     C                   endif
332500170804     C*
332600170804     C     *DMY          Move      UDate         UDateISO
332700000000     C*
332800000000     C                   ENDSR
332900000000     C/EJECT
333000000000     C************************************************************
333100000000     C* INIZIALIZZAZIONE VARIABILI
333200000000     C************************************************************
333300000000     C     INZVAR        BEGSR
333400000000     C*
333401170810     C*
333402170810     C* Salvo se opzione '15' (visualizzazione da catalogo)
333403170810     C*
333404170810     C                   IF        OPZI75 = '15'
333405170810     C                   Movel     OPZI75        SAVOpzI75
333406170810     C                   Movel     '05'          OPZI75
333407170810     C                   ENDIF
333500000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
333600000000     C* reperimento dati società
333700170804    >C                   IF        SocietaI75 <> xscsoc  and
333800170804    >C                             OpzI75 <>*blank
333900000000     C                   MOVEL     'SOC001'      TIPXSC
334000170804    >C                   MOVEL     SocietaI75    SOCXSC
334100000000     C                   EXSR      REPSOC
334200000000     C     RTNXSC        IFNE      '1'
334300000000     C                   MOVEL     XSOCDS        SOC001
334400000000     C                   ELSE
334500000000     C                   MOVE      *ON           $FINE
334600170804    >C                   MOVE      '1'           ErrI75
334700000000     C                   EXSR      ENDPGM
334800000000     C                   ENDIF
334900000000     C                   ENDIF
335000110824     C*
335100000000     C* Reperimento parametro modulo
335200000000     C                   EXSR      REPPAR
335300000000     C*
335400000000     C* Reperimento autorizzazioni
335500000000     C*
335600000000     C* le autorizzazioni potrebbero essere già state passate,
335700000000     C* nel qual caso non vanno reperite
335800000000    >C                   EXSR      REPAUT
335900000000     C*
336000000000     C* visualizzo il sfl troncato
336100000000     C                   Eval      C1Mode      = *On
336200000000     C*
336300000000     C*
336400000000     C* Reperimento valori default utente
336500000000     C*
336600000000     C                   EXSR      REPDFT
336700000000     C*
336800000000     C* Definizione delle opzioni utilizzabili a pgm
336900000000     C*
337000000000     C                   EXSR      DEFOPZ
337100000000     C*
337200000000     C* Variabili per gestione videate
337300000000     C*
337400000000     C                   MOVE      *OFF          $FINE                          fine pgm
337500000000     C                   MOVE      *OFF          $INZD1                         inizializzione D1
337600000000     C                   MOVE      *OFF          $INZS1                         inizializzione S1
337700000000     C                   MOVE      *OFF          $INZD2                         inizializzione D2
337800000000     C                   MOVE      *OFF          $EndFile                       memo fine file
337900000000     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
338000000000     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
338100000000     C                   MOVE      *HIVAL        $LASTV
338200000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
338300000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
338400000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
338500000000     C* azzero la posizione del cursore
338600000000     C                   Z-ADD     0             H1RIGA
338700000000     C                   Z-ADD     0             H1COLO
338800000000     C*
338900000000     C                   MOVE      *ON           $INZD1
339000000000     C                   MOVE      *ON           $INZS1
339100000000     C*
339200000000     C* se pgm richiamato con chiave univoca impostata,
339300000000     C* forzo la emissione della lista e imposto l'ordinamento
339400000000    >C*  in questo esempio la chiave univoca per la posta è Codice
339500000000    >C*  ma qualora ci fosse più di un campo basterebbe testare quello più
339600000000    >C*  significativo (o meglio, quello che si sa essere obbligatorio
339700000000    >C*  ricevere pieno)
339800000000     C                   MOVE      *OFF          $Unique
339900000000     C*
340000000000     C* Variabili appoggio
340100000000     C*
340200000000     C                   Z-ADD     SFLPAG        WPAG
340300000000     C                   MOVEL     *OFF          WOPR
340400000000     C                   CLEAR                   W1POS
340500000000     C*
340600000000     C* definisco qual'è la prima videata
340700000000     C* (se la prima videata risulta la lista, devo comunque caricare
340800000000     C*  la videata di parzializzazione e gestire la definizione
340900000000     C*  dell'ordinamento, perchè, dai default oppure se sono stati
341000000000     C*  passati dei dati, può variare il caricamento)
341100000000     C                   SELECT
341200000000     C     $LOP          WHENEQ    1
341300000000     C                   MOVE      'D1'          $GEST
341400000000     C     $LOP          WHENEQ    2
341500000000     C                   MOVE      'S1'          $GEST
341600000000     C                   EXSR      INZD1
341700000000     C                   MOVE      *OFF          $INZD1
341800000000     C                   EXSR      F02D1
341900000000     C                   EXSR      DEFORD
342000000000     C                   ENDSL
342100000000     C*
342200000000     C* Valorizzazione campi univoci testate
342300000000     C*
342400000000     C                   CLEAR                   FMTT1
342500000000     C                   MOVEL     KNSIF         NOMSIF
342600000000     C                   MOVEL     XSCDSI        NOMDIT
342700000000     C                   MOVEL     KCDAZ         NOMAZN
342800000000     C****               MOVEL     DSPGM         NOMPGM
342900000000     C* recupero il sottotitolo (se c'è)
343000000000     C                   MOVEL     *BLANK        T1TIT
343100000000     C     SOTTIT        IFNE      *BLANK
343200000000     C                   MOVEL     SOTTIT        IDMSG
343300000000     C                   CALLB     'XRTVM'
343400000000     C                   PARM                    IDMSG            27
343500000000     C                   PARM                    TXTMSG          100
343600000000     C                   PARM                    ERRMSG            1
343700000000     C                   PARM      *ON           CTRMSG            1            centratura
343800000000     C                   PARM      30            LENMSG            3 0          lun output.
343900000000     C     ERRMSG        IFNE      *ON
344000000000     C                   MOVEL     TXTMSG        T1TIT
344100000000     C                   ELSE
344200000000     C                   MOVEL     *ALL'?'       T1TIT
344300000000     C                   ENDIF
344400000000     C                   ENDIF
344500170804     C*
344600170804     C                   MOVEL     *BLANK        T1TIT
344700170804     C                   if        SOTTITT1   <> *BLANK
344800170804     C                   MOVEL     SOTTITT1      IDMSG
344900170804     C                   endif
345000170804     C*
345100170804     C                   CALLB     'XRTVM'
345200170804     C                   PARM                    IDMSG            27
345300170804     C                   PARM                    TXTMSG          100
345400170804     C                   PARM                    ERRMSG            1
345500170804     C                   PARM      *ON           CTRMSG            1            centratura
345600170804     C                   PARM      30            LENMSG            3 0          lun output.
345700170804     C     ERRMSG        IFNE      *ON
345800170804     C                   MOVEL     TXTMSG        T1TIT
345900170804     C                   ELSE
346000170804     C                   MOVEL     *ALL'?'       T1TIT
346100170804     C                   ENDIF
346200000000     C*
346300000000     C                   ENDSR
346400000000     C/EJECT
346500000000     C************************************************************
346600000000     C* REPERIMENTO DATI SOCIETA'
346700000000     C************************************************************
346800000000     C     REPSOC        BEGSR
346900000000     C*
347000000000     C                   CALL      'SDGSOCDR'
347100000000     C                   PARM                    TIPXSC            6
347200000000     C                   PARM                    SOCXSC            3
347300000000     C                   PARM                    CDSXSC            9 0
347400000000     C                   PARM                    MODXSC            3
347500000000     C                   PARM      *BLANKS       RTNXSC            1
347600000000     C                   PARM                    XSOCDS
347700000000     C                   PARM                    KPJBA
347800000000     C*
347900000000     C                   ENDSR
348000000000     C/EJECT
348100000000     C************************************************************
348200000000     C* Rerimento parametro modulo
348300000000     C************************************************************
348400000000     C     REPPAR        BEGSR
348500000000     C*
348600000000     C*
348700000000     C                   ENDSR
348800000000     C************************************************************
348900000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
349000000000     C************************************************************
349100000000     C     REPAUT        BEGSR
349200000000     C*
349300000000     C                   CLEAR                   AUTDS
349400000000     C                   Clear                   ConCivDs
349500000000     C*
349600000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
349700000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
349800000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
349900000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
350000000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
350100000000     C                   MOVEL     *ZERO         XCAVFU
350200000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
350300000000     C                   MOVE      '0'           XCANAU                         RICH.XNOAUT
350400000000     C*
350500000000     C                   CALLB     'XCHKAUT'
350600000000     C                   PARM                    AUTDS
350700000000     C*
350800000000     C* errori/non abilitazione
350900000000     C     XCARTN        IFNE      0
351000000000     C*
351100000000     C                   ELSE
351200000000    >C                   MOVEL     XCADDS        CONCIVDS
351300000000     C                   ENDIF
351400000000     C*
351500000000     C                   ENDSR
351600000000     C/EJECT
351700000000     C************************************************************
351800000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
351900000000     C************************************************************
352000000000     C     REPDFT        BEGSR
352100000000     C*
352200000000     C* preparo i parametri di chiamata al driver
352300000000     C                   CLEAR                   XMPEDS
352400000000     C                   EVAL      TpChiamata = '01'
352500000000     C                   EVAL      NChiavi = 5
352600000000     C                   EVAL      VLogica = 2
352700000000     C*
352800000000     C* preparo le chiavi per la chiamata al driver
352900000000     C                   CLEAR                   XMPEDS1
353000000000     C                   MOVEL     XSCSOC        DSMPESOC
353100000000     C                   MOVEL     DSPGM         DSMPEPGM
353200000000     C                   MOVEL     KNMUS         DSMPEPRF
353300000000     C* Si reperiscono i dati di default a seconda delle modalità con
353400000000     C* cui il pgm è usato
353500000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
353600000000     C* MPETIP = 'PV'  pgm richiamato da menù
353700000000     C* MPETIP = 'PB'  x lancio Batch
353800000000     C                   SELECT
353900170804    >C     OPZI75        WHENEQ    '01'
354000000000     C                   MOVEL     'PX'          DSMPETIP
354100000000     C                   OTHER
354200000000     C                   MOVEL     'PV'          DSMPETIP
354300000000     C                   ENDSL
354400000000     C*
354500000000     C                   CALLB     'XMPE'
354600000000     C                   PARM                    XMPEDS
354700000000     C                   PARM                    XMPEDS1
354800000000     C*
354900000000     C     Risultato     IFEQ      '0'
355000000000     C                   MOVEL     DSMPEPAR      DSMPE
355100000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
355200000000     C* i suoi valori di default vengono decisi a pgm
355300000000     C                   ELSE
355400000000     C                   Z-ADD     2             $LOP
355500000000     C                   Z-ADD     1             $ORD
355600000000     C                   ENDIF
355700000000     C*
355800000000     C                   ENDSR
355900000000     C/EJECT
356000000000     C************************************************************
356100000000     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
356200000000     C************************************************************
356300000000     C     DEFOPZ        BEGSR
356400000000     C*
356500000000     C* Questa routine deve valorizzare la schiera $FL1
356600000000     C* con S o N a seconda della utilizzabilità o meno
356700000000     C* dell'opzione correlata
356800000000     C*
356900000000     C* Inizialmente si considerano tutte le opzioni come valide
357000000000     C     1             DO        $DIMOP        X
357100000000     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
357200000000     C                   If        $TX1(X)  <> *Blank
357300000000     C                   MOVE      'S'           $FL1(X)
357400000000     C                   Else
357500000000     C                   Leave
357600000000     C                   EndIf
357700000000     C                   ENDDO
357800000000     C*
357900000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
358000000000     C* con 1 o 0 a seconda della utilizzabilità o meno
358100000000     C* del tasto funzione correlato
358200000000     C*
358300000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
358400000000     C* di compilazione
358500000000     C                   RESET                   $FC1
358600000000     C                   RESET                   $FR1
358700000000     C                   RESET                   $FC2
358800000000     C                   RESET                   $FR2
358900000000     C                   RESET                   $FC3
359000000000     C                   RESET                   $FR3
359100000000     C*
359200000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
359300000000     C* invalido alcune opzioni
359400000000     C                   SELECT
359500000000     C* Se pgm richiamato in ricerca disattivo :
359600170804    >C     OPZI75        WHENEQ    '01'
359700000000     C* F3=Fine
359800000000     C                   Z-ADD     3             X
359900000000     C                   MOVE      '0'           $FC1(X)
360000000000     C                   MOVE      '0'           $FC2(X)
360100000000     C                   MOVE      '0'           $FC3(X)
360200000000     C*
360300000000     C*
360400000000     C* Se pgm richiamato in interrogazione disattivo :
360500170804    >C     OPZI75        WHENEQ    '05'
360600000000     C* F10=Immissione
360700000000     C                   Z-ADD     10            X
360800000000     C                   MOVE      '0'           $FC1(X)
360900000000     C                   MOVE      '0'           $FC2(X)
361000000000     C* 1=Selezione
361100000000     C                   Z-ADD     1             X
361200000000     C     01            LOOKUP    $OP1(X)                                21
361300000000     C     *IN21         IFEQ      *ON
361400000000     C                   MOVE      'N'           $FL1(X)
361500000000     C                   ENDIF
361600000000     C* 2=Modifica
361700000000     C                   Z-ADD     1             X
361800000000     C     02            LOOKUP    $OP1(X)                                21
361900000000     C     *IN21         IFEQ      *ON
362000000000     C                   MOVE      'N'           $FL1(X)
362100000000     C                   ENDIF
362200000000     C* 3=Copia
362300000000     C                   Z-ADD     1             X
362400000000     C     03            LOOKUP    $OP1(X)                                21
362500000000     C     *IN21         IFEQ      *ON
362600000000     C                   MOVE      'N'           $FL1(X)
362700000000     C                   ENDIF
362800000000     C* 4=Annullamento
362900000000     C                   Z-ADD     1             X
363000000000     C     04            LOOKUP    $OP1(X)                                21
363100000000     C     *IN21         IFEQ      *ON
363200000000     C                   MOVE      'N'           $FL1(X)
363300000000     C                   ENDIF
363301170810     C* 44=Annulla(ripristina)
363302170810     C                   Z-ADD     1             X
363303170810     C     44            LOOKUP    $OP1(X)                                21
363304170810     C     *IN21         IFEQ      *ON
363305170810     C                   MOVE      'N'           $FL1(X)
363306170810     C                   ENDIF
363400000000     C*
363500170804    >C     OPZI75        WHENEQ    *BLANKS
363600000000     C* Se pgm non richiamato, disattivo subito :
363700000000     C* F8=Selezione seguente
363800000000     C                   Z-ADD     8             X
363900000000     C                   MOVE      '0'           $FC1(X)
364000000000     C                   MOVE      '0'           $FC2(X)
364100000000     C* 1=Selezione
364200000000     C                   Z-ADD     1             X
364300000000     C     01            LOOKUP    $OP1(X)                                21
364400000000     C     *IN21         IFEQ      *ON
364500000000     C                   MOVE      'N'           $FL1(X)
364600000000     C                   ENDIF
364700000000     C*
364800000000     C* In tutti gli altri casi, disattivo subito :
364900000000     C                   OTHER
365000000000     C* 1=Selezione
365100000000     C                   Z-ADD     1             X
365200000000     C     01            LOOKUP    $OP1(X)                                21
365300000000     C     *IN21         IFEQ      *ON
365400000000     C                   MOVE      'N'           $FL1(X)
365500000000     C                   ENDIF
365600000000     C*
365700000000     C                   ENDSL
365800000000     C*
365900000000     C*
366000000000     C* poi, disattivo a seconda delle autorizzazioni :
366100000000     C                   IF        ImmAll <> *on
366200000000     C*
366300000000     C* F10=Immissione
366400000000     C                   Z-ADD     10            X
366500000000     C                   MOVE      '0'           $FC1(X)
366600000000     C                   MOVE      '0'           $FC2(X)
366700000000     C*
366800000000     C                   Z-ADD     1             X
366900000000     C     03            LOOKUP    $OP1(X)                                21
367000000000     C     *IN21         IFEQ      *ON
367100000000     C                   MOVE      'N'           $FL1(X)
367200000000     C                   ENDIF
367300000000     C*
367400000000     C                   ENDIF
367500000000     C*
367600000000     C                   IF        ManAll <> *on
367700000000     C                   Z-ADD     1             X
367800000000     C     02            LOOKUP    $OP1(X)                                21
367900000000     C     *IN21         IFEQ      *ON
368000000000     C                   MOVE      'N'           $FL1(X)
368100000000     C                   ENDIF
368200000000     C*
368300000000     C                   IF        VisAll <> *on
368400000000     C                   Z-ADD     1             X
368500000000     C     22            LOOKUP    $OP1(X)                                21
368600000000     C     *IN21         IFEQ      *ON
368700000000     C                   MOVE      'N'           $FL1(X)
368800000000     C                   ENDIF
368900000000     C                   endif
369000000000     C                   ENDIF
369100000000     C*
369200000000     C                   IF        AnnAll <> *on
369300000000     C                   Z-ADD     1             X
369400000000     C     04            LOOKUP    $OP1(X)                                21
369500000000     C     *IN21         IFEQ      *ON
369600000000     C                   MOVE      'N'           $FL1(X)
369700000000     C                   ENDIF
369800000000     C*
369900000000     C                   Z-ADD     1             X
370000000000     C     44            LOOKUP    $OP1(X)                                21
370100000000     C     *IN21         IFEQ      *ON
370200000000     C                   MOVE      'N'           $FL1(X)
370300000000     C                   ENDIF
370400000000     C                   ENDIF
370500000000     C*
370600000000     C                   IF        VisAll <> *on
370700000000     C                   Z-ADD     1             X
370800000000     C     05            LOOKUP    $OP1(X)                                21
370900000000     C     *IN21         IFEQ      *ON
371000000000     C                   MOVE      'N'           $FL1(X)
371100000000     C                   ENDIF
371200000000     C                   ENDIF
371300000000     C*
371400000000     C*
371500000000     C                   ENDSR
371600000000     C/EJECT
371700000000     C************************************************************
371800000000     C* REPERIMENTO OPZIONI
371900000000     C************************************************************
372000000000     C     REPOPZ        BEGSR
372100000000     C*
372200000000     C     1             DO        $DIMOP        X
372300000000     C*
372400000000     C     $MS1(X)       IFNE      *BLANK
372500000000     C*
372600000000     C                   MOVEL     $MS1(X)       IDMSG
372700000000     C                   CALLB     'XRTVM'
372800000000     C                   PARM                    IDMSG            27
372900000000     C                   PARM                    TXTMSG          100
373000000000     C                   PARM                    ERRMSG            1
373100000000     C*
373200000000     C     ERRMSG        IFNE      *ON
373300000000     C                   MOVEL     TXTMSG        $TX1(X)
373400000000     C                   ELSE
373500000000     C                   MOVEL     *ALL'?'       $TX1(X)
373600000000     C                   ENDIF
373700000000     C*
373800000000     C                   ENDIF
373900000000     C*
374000000000     C                   ENDDO
374100000000     C*
374200000000     C                   ENDSR
374300000000     C/EJECT
374400000000     C************************************************************
374500000000     C* REPERIMENTO TASTI DI FUNZIONE
374600000000     C************************************************************
374700000000     C     REPKEY        BEGSR
374800000000     C                   MOVEA     $FM1          $FM
374900000000     C                   MOVEA     $FC1          $FC
375000000000     C                   EXSR      REPKKK
375100000000     C                   MOVEA     $FX           $FX1
375200000000     C*
375300000000     C                   MOVEA     $FM2          $FM
375400000000     C                   MOVEA     $FC2          $FC
375500000000     C                   EXSR      REPKKK
375600000000     C                   MOVEA     $FX           $FX2
375700000000     C*
375800000000     C                   MOVEA     $FM3          $FM
375900000000     C                   MOVEA     $FC3          $FC
376000000000     C                   EXSR      REPKKK
376100000000     C                   MOVEA     $FX           $FX3
376200000000     C                   ENDSR
376300000000     C/EJECT
376400000000     C************************************************************
376500000000     C* REPERIMENTO TASTI DI FUNZIONE
376600000000     C************************************************************
376700000000     C     REPKKK        BEGSR
376800000000     C*
376900000000     C                   CLEAR                   $FX
377000000000     C*
377100000000     C     1             DO        24            X
377200000000     C*
377300000000     C     $FM(X)        IFNE      *BLANK
377400000000     C     $Fc(X)        andne     '0'
377500000000     C*
377600000000     C                   MOVEL     $FM(X)        IDMSG
377700000000     C                   CALLB     'XRTVM'
377800000000     C                   PARM                    IDMSG            27
377900000000     C                   PARM                    TXTMSG          100
378000000000     C                   PARM                    ERRMSG            1
378100000000     C*
378200000000     C     ERRMSG        IFNE      *ON
378300000000     C                   MOVEL     TXTMSG        $FX(X)
378400000000     C                   ELSE
378500000000     C                   MOVEL     *ALL' '       $FX(X)
378600000000     C                   ENDIF
378700000000     C*
378800000000     C                   ENDIF
378900000000     C*
379000000000     C                   ENDDO
379100000000     C*
379200000000     C                   ENDSR
379300000000     C/EJECT
379400000000     C************************************************************
379500000000     C* DEFINIZIONE KLIST
379600000000     C************************************************************
379700000000     C     DefKlist      BEGSR
379800000000     C*
379900000000     C* klist
380000170804     C*
380100170804     C     K07CST02      Klist
380200170804     C                   Kfld                    CSTSocieta
380300170804     C                   Kfld                    CSTTpCom
380400170804     C                   kfld                    CSTTprec
380500170804     C                   kfld                    CSTAnno
380600170804     C                   kfld                    CSTTrim
380700170804     C                   kfld                    CSTMese
380800170804     C                   kfld                    CSTCLIFOR
380900000000     C*
381000000000     C                   ENDSR
381100000000     C/EJECT
381200000000** $MS1 / $TX1
381300000000PROMSG    *LIBL     OPZ0001
381400000000PROMSG    *LIBL     OPZ0002
381500000000PROMSG    *LIBL     OPZ0003
381600000000PROMSG    *LIBL     OPZ0004
381700000000PROMSG    *LIBL     OPZ0005
381800000000PROMSG    *LIBL     OPZ0146
381900170807SDGMSG    *LIBL     SDO0055
382000000000PROMSG    *LIBL     OPZ0041
382100000000SDGMSG    *LIBL     SDO0024
382200000000** $OP1 / $FL1
38230000000001S      <Selezione
38240000000002S      <Modifica
38250000000003S      <Copia
38260000000004S      <Cancella
38270000000005S      <Visualizza
38280000000015S      <Interrogazioni conto
38290017080722S      <Dati anagrafici
38300000000041S      <Contabilità
38310000000044S      <Annulla/Ripristina
383200000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
383300000000PROMSG    *LIBL     KEY0001  01
383400000000PROMSG    *LIBL     KEY0002  02
383500000000PROMSG    *LIBL     KEY0003  03
383600000000PROMSG    *LIBL     KEY0004  04
383700000000PROMSG    *LIBL     KEY0005  05
383800000000PROMSG    *LIBL     KEY0006  06
383900000000PROMSG    *LIBL     KEY0007  07
384000000000PROMSG    *LIBL     KEY0008  08
384100000000PROMSG    *LIBL     KEY0009  09
384200000000PROMSG    *LIBL     KEY0010  10
384300000000PROMSG    *LIBL     KEY0011  11
384400000000PROMSG    *LIBL     KEY0012  12
384500000000PROMSG    *LIBL     KEY0013  13
384600000000PROMSG    *LIBL     KEY0014  14
384700000000SDGMSG    *LIBL     SDK0003  15
384800000000PROMSG    *LIBL     KEY0016  16
384900000000SDGMSG    *LIBL     SDK0001  17
385000000000PROMSG    *LIBL     KEY0018  18
385100000000PROMSG    *LIBL     KEY0019  19
385200000000PROMSG    *LIBL     KEY0020  20
385300000000PROMSG    *LIBL     KEY0021  21
385400000000PROMSG    *LIBL     KEY0022  22
385500000000PROMSG    *LIBL     KEY0023  23
385600000000PROMSG    *LIBL     KEY0024  24
385700000000**  ABILITAZIONE D1 : $FC1 / $FR1
3858000000000N  01
3859000000001N  02
3860000000001N  03
3861000000001N  04
3862000000000N  05
3863000000000N  06
3864000000000N  07
3865000000001N  08
3866000000000N  09
3867000000000N  10
3868000000000N  11
3869000000001N  12
3870000000000N  13
3871000000000N  14
3872000000000N  15
3873000000000N  16
3874000000000N  17
3875000000000N  18
3876000000000N  19
3877000000000N  20
3878000000000N  21
3879000000000N  22
3880000000000N  23   Impostare a '1' se si utilizzano opzioni
3881000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
388200000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
388300000000PROMSG    *LIBL     KEY0001  01
388400000000PROMSG    *LIBL     KEY0002  02
388500000000PROMSG    *LIBL     KEY0003  03
388600000000PROMSG    *LIBL     KEY0004  04
388700000000PROMSG    *LIBL     KEY0005  05
388800000000PROMSG    *LIBL     KEY0006  06
388900000000PROMSG    *LIBL     KEY0007  07
389000000000PROMSG    *LIBL     KEY0008  08
389100000000PROMSG    *LIBL     KEY0009  09
389200000000PROMSG    *LIBL     KEY0010  10
389300000000PROMSG    *LIBL     KEY0011  11
389400000000PROMSG    *LIBL     KEY0012  12
389500000000PROMSG    *LIBL     KEY0013  13
389600000000PROMSG    *LIBL     KEY0014  14
389700000000PROMSG    *LIBL     KEY0015  15
389800000000PROMSG    *LIBL     KEY0016  16
389900000000SDGMSG    *LIBL     SDK0001  17
390000000000PROMSG    *LIBL     KEY0018  18
390100000000PROMSG    *LIBL     KEY0019  19
390200000000PROMSG    *LIBL     KEY0020  20
390300000000PROMSG    *LIBL     KEY0021  21
390400000000PROMSG    *LIBL     KEY0022  22
390500000000PROMSG    *LIBL     KEY0023  23
390600000000PROMSG    *LIBL     KEY0024  24
390700000000**  ABILITAZIONE S1 : $FC2 / $FR2
3908000000000N  01
3909000000000N  02
3910000000001N  03
3911000000000N  04
3912001309240N  05
3913001309240N  06
3914000000000N  07
3915000000001N  08
3916000000001N  09
3917000000001N  10
3918000000000N  11
3919000000001N  12
3920000000000N  13
3921000000000N  14
3922000000000N  15
3923000000000N  16
3924000000000N  17
3925000000000N  18
3926000000000N  19
3927000000000N  20
3928000000000N  21
3929000000000N  22
3930000000001N  23   Impostare a '1' se si utilizzano opzioni
3931001202061N  24   Lasciare sempre impostata a '1' il valore della riga 24
393200000000** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
393300000000PROMSG    *LIBL     KEY0001  01
393400000000PROMSG    *LIBL     KEY0002  02
393500000000PROMSG    *LIBL     KEY0003  03
393600000000PROMSG    *LIBL     KEY0004  04
393700000000PROMSG    *LIBL     KEY0005  05
393800000000PROMSG    *LIBL     KEY0006  06
393900000000PROMSG    *LIBL     KEY0007  07
394000000000PROMSG    *LIBL     KEY0008  08
394100000000PROMSG    *LIBL     KEY0009  09
394200000000PROMSG    *LIBL     KEY0010  10
394300000000PROMSG    *LIBL     KEY0011  11
394400000000PROMSG    *LIBL     KEY0012  12
394500000000PROMSG    *LIBL     KEY0013  13
394600000000PROMSG    *LIBL     KEY0014  14
394700000000PROMSG    *LIBL     KEY0015  15
394800000000PROMSG    *LIBL     KEY0016  16
394900000000SDGMSG    *LIBL     SDK0001  17
395000000000PROMSG    *LIBL     KEY0018  18
395100000000PROMSG    *LIBL     KEY0019  19
395200000000PROMSG    *LIBL     KEY0020  20
395300000000PROMSG    *LIBL     KEY0021  21
395400000000PROMSG    *LIBL     KEY0022  22
395500000000PROMSG    *LIBL     KEY0023  23
395600000000PROMSG    *LIBL     KEY0024  24
395700000000**  ABILITAZIONE D3 : $FC3 / $FR3
3958000000000N  01
3959000000000N  02
3960000000001N  03
3961000000000N  04
3962000000000N  05
3963000000000N  06
3964000000000N  07
3965000000000N  08
3966000000000N  09
3967000000000N  10
3968000000000N  11
3969000000001N  12
3970000000000N  13
3971000000000N  14
3972000000000N  15
3973000000000N  16
3974000000000N  17
3975000000000N  18
3976000000000N  19
3977000000000N  20
3978000000000N  21
3979000000000N  22
3980000000000N  23   Impostare a '1' se si utilizzano opzioni
3981000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
