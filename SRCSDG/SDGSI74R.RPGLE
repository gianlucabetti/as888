000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND SDG007)
000200000000     H*PARMS CVTOPT(*NONE) ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*****************************************************************
000500000000      *
000600000000      *                   Spesometro trimestrale
000700000000      *                       Dati di testata
000800000000      *           ---------------------------------------
000900000000      *
001000000000     F*----------------------------------------------------*
001100000000     F* Spesometro - Movimenti estratti
001200000000     FSDGCST00F UF   E             DISK    USROPN
001300000000     F                                     COMMIT
001400000000     F* Spesometro - Movimenti estratti
001500000000     F* Key: Soc/TpCom/Anno/Trim/Mese/Tipo rec./dati anagrafici
001600000000     FSDGCST02L UF   E           K DISK    Rename(SDGCST000:SDGCST002)
001700000000     F                                     USROPN
001800000000     F                                     COMMIT
001900000000     F*------------
002000000000     F* Anagrafica codici avviamento postale
002100000000     FANCAP01L  IF   E           K DISK
002200000000     F*------------
002300000000     FSDGSI74D  CF   E             WORKSTN
002400000000     F                                     RENAME(SI74D1:FMTD1)
002500000000     F                                     RENAME(SI74T1:FMTT1)
002600000000     F                                     RENAME(SI74Z1:FMTZ1)
002700000000     F* eventuali schermi supplementari
002800000000     F                                     RENAME(SI74D2:FMTD2)
002900000000     F                                     RENAME(SI74D3:FMTD3)
003000000000     F                                     INFDS(DSFMT)
003100000000     F*------------
003200000000     D*----------------------------------------------------*
003300000000     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
003400000000     D $ID             S             27    DIM(5) ALT($OZ)
003500000000     D* Schiere per la gestione dei tasti di funzione
003600000000     D $FM             S             27    DIM(24)
003700000000     D $FX             S             50    DIM(24)
003800000000     D $FC             S              1    DIM(24)
003900000000     D $FR             S              1    DIM(24)
004000000000     D* Video D1
004100000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004200000000     D $FX1            S             50    DIM(24) ALT($FM1)
004300000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
004400000000     D $FR1            S              1    DIM(24) ALT($FC1)
004500000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
004600000000     D* funzione personalizzati
004700000000     D $PGF1           S             10    DIM(25)
004800000000     D*----------------------------------------------------*
004900000000     D $D1L01          S              3  0
005000000000     D $D1L02          S              3  0
005100000000     D*-------------
005200000000     D* Passaggio Parametri
005300000000     D KPJBA         E DS
005400000000     D SAVKPJBU        S                   Like(KPJBU) inz
005500000000     D*-------------
005600000000     D* DS Esterna gestione messaggi
005700000000     D SDGMSGDS      E DS
005800000000     D  STS          E                     EXTFLD(MSGSTS)
005900000000     D                                     DIM(333)
006000000000     D  JBA          E                     EXTFLD(MSGJBA)
006100000000     D                                     DIM(502)
006200000000     D*-------------
006300000000     D* Reperimento Autorizzazioni
006400000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
006500000000     D*-------------
006600000000     D* Maschera dati autorizzazioni
006700000000     D CONCIVDS      E DS
006800000000     D*-------------
006900000000     D* Reperimento nome PGM
007000000000     D STATUS         SDS           333
007100000000     D  DSPGM            *PROC
007200000000     D  PARMS            *PARMS
007300000000     D*-------------
007400000000     D* Dati di ambiente ottenuti da SDGSOCDR
007500000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
007600000000     D*-------------
007700000000     D* DS Interna per dati di output di SDGSOCDR
007800000000     D XSOCDS          DS          1000
007900000000     D*-------------
008000000000     D* Parametri modulo SDGSPETRI
008100000000     D SDGPA12DS     E DS
008200000000     D*-------------
008300000000     D* Passaggio Parametri al PGM di manutenzione
008400000000     D SDGSI74DS     E DS
008500000000     D*-------------
008600000000     D* Parametri del PGM di aggiornamento dei movimenti estratti
008700000000     D SDG00702DS    E DS                  INZ
008800000000     D* Parametri per recpero dei dati fiscali
008900000000     D SDG00701DS    E DS                  INZ
009000000000     D*
009100000000     D SAV00701DS      S                   Like(SDG00701DS) inz
009200000000     D*-------------
009300000000     D* Parametri per ANMPE
009400000000     D XMPEDS        E DS                  INZ
009500000000     D XMPEDS1       E DS                  INZ
009600000000     D*----------
009700000000     D* Passaggio Parametri al PGM ANA801R
009800000000     D A801DS        E DS                  EXTNAME(ANA801DS)
009900000000     D  ERR801       E                     INZ('0')
010000000000     D*-------------
010100000000     D* Parametri per ricerca codice ANATB00F
010200000000     D XTABDS        E DS                  inz
010300000000     D*-------------
010400000000     D* Richiamo a SDGTABDR
010500000000     D SDGTABDS      E DS                  inz
010600000000     D*-------------
010700000000     D* Ricerca codici Stato
010800000000     D ANGG12DS      E DS
010900000000     D*-------------
011000000000     D* Ricerca codici ISO
011100000000     D SDGHD4DS      E DS
011200000000     D*-------------
011300000000     D* Ricerca codici province italiane
011400000000     D ANGC17DS      E DS
011500000000     D*-------------
011600000000     D* Campo chiave per tabella C17
011700000000     D KeyC17          S              6A   inz
011800000000     D*-------------
011900000000     D* ricerca soggetti
012000000000     D ANA040DS      E DS                  INZ
012100000000     D*-------------
012200000000     D* DS esterna per dati soggetti
012300000000     D DVASOG        E DS                  INZ
012400000000     D*-------------
012500000000     D* Definizioni parametri driver anagrafici
012600000000     D Dvaric          S              1
012700000000     D DvaSocieta      S              3
012800000000     D DvaUnita        S              8
012900000000     D DvaStrutt       S             10
013000000000     D DvaDtRif        S               D
013100000000     D DvaLenOut       S              9B 0
013200000000     D DvaSnatura      S              1
013300000000     D DvaCodice       S              8
013400000000     D DvaLineav       S              3
013500000000     D DvaFiliale      S              3  0
013600000000     D DvaSogg         S              8
013700000000     D DvaTpInd        S              2
013800000000     D DvaCdInd        S              3
013900000000     D DvaErrore       S              1
014000000000     D DvaOutput       S           4000
014100000000     D*-------------
014200000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
014300000000     D* alla funzione personalizzata
014400000000     D $SaveDS         S                   like(SDGSI74DS)
014500000000     D*-------------
014600000000     D* Ds per ricerca legende
014700000000     D ANA964DS      E DS                  INZ
014800000000     D*-------------
014900000000     D* Controlli validità per valore di una legenda
015000000000     D X07VALDS      E DS                  Inz
015100000000     D*-------------
015200000000     D DSFMT           DS
015300000000     D  $TASTO               369    369
015400000000     D  NRG                  370    370
015500000000     D  NCL                  371    371
015600000000     D* riga cursore window
015700000000     D  NRgWin               382    382
015800000000     D* colonna cursore window
015900000000     D  NClWin               383    383
016000000000     D*-------------
016100000000     D* DS per UserExit
016200000000     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
016300000000     D*-------------
016400000000     D* posizione cursore
016500000000     D CURSOR          DS
016600000000     D  RIRI                   1      2B 0 INZ
016700000000     D  R$$                    2      2
016800000000     D  COCO                   3      4B 0 INZ
016900000000     D  C$$                    4      4
017000000000     D*-------------
017100000000     D* Costanti
017200000000     D*-------------
017300000000     D* Tasti di funzione
017400000000     D F01             C                   CONST(X'31')
017500000000     D F02             C                   CONST(X'32')
017600000000     D F03             C                   CONST(X'33')
017700000000     D F04             C                   CONST(X'34')
017800000000     D F05             C                   CONST(X'35')
017900000000     D F06             C                   CONST(X'36')
018000000000     D F07             C                   CONST(X'37')
018100000000     D F08             C                   CONST(X'38')
018200000000     D F09             C                   CONST(X'39')
018300000000     D F10             C                   CONST(X'3A')
018400000000     D F11             C                   CONST(X'3B')
018500000000     D F12             C                   CONST(X'3C')
018600000000     D F13             C                   CONST(X'B1')
018700000000     D F14             C                   CONST(X'B2')
018800000000     D F15             C                   CONST(X'B3')
018900000000     D F16             C                   CONST(X'B4')
019000000000     D F17             C                   CONST(X'B5')
019100000000     D F18             C                   CONST(X'B6')
019200000000     D F19             C                   CONST(X'B7')
019300000000     D F20             C                   CONST(X'B8')
019400000000     D F21             C                   CONST(X'B9')
019500000000     D F22             C                   CONST(X'BA')
019600000000     D F23             C                   CONST(X'BB')
019700000000     D F24             C                   CONST(X'BC')
019800000000     D ENTER           C                   CONST(X'F1')
019900000000     D ROLDWN          C                   CONST(X'F4')
020000000000     D ROLLUP          C                   CONST(X'F5')
020100000000     D*-------------
020200000000     D* Sottotitolo di testata
020300000000     D SOTTITSog       C                   CONST('PROMSG    *LIBL-
020400000000     D                                          COS2504')
020500000000     D SOTTITSoc       C                   CONST('SDGMSG    *LIBL-
020600000000     D                                          SDC0117')
020700000000     D $Sottit         S             27A
020800000000     D*-------------
020900000000     D* Variabili per gestione videate
021000000000     D $Gest           S              2A
021100000000     D $Fine           S              1A
021200000000     D $InzD1          S              1A
021300000000     D $LastG          S              2A
021400000000     D $LastV          S              2A
021500000000     D $FlgDef         S              1A
021600000000     D* -----------------
021700000000     D* Ds per la chiave di lettura
021800000000     D DsCliFor        DS
021900000000     D  CSTCOGRAG1
022000000000     D  CSTCF
022100000000     D  CSTPARTIVA
022200000000     D  CSTCDFISC
022300000000     D  CSTSTATO1
022400000000     D  CSTIDPAES1
022500000000     D  CSTPIVA1
022600000000     D  CSTCDFISC1
022700000000     D  CSTTPN1
022800000000     D  CSTDENOM1
022900000000     D  CSTNOME1
023000000000     D  CSTCOGNOM1
023100000000     D  CSTINDIR2
023200000000     D  CSTNRCIV2
023300000000     D  CSTCAP2
023400000000     D  CSTLOCALI2
023500000000     D  CSTPROV2
023600000000     D  CSTSTATO2
023700000000     D  CSTIDPAES2
023800000000     D  CSTINDIR3
023900000000     D  CSTNRCIV3
024000000000     D  CSTCAP3
024100000000     D  CSTLOCALI3
024200000000     D  CSTPROV3
024300000000     D  CSTSTATO3
024400000000     D  CSTIDPAES3
024500000000     D  CSTSTATO4
024600000000     D  CSTIDPAES4
024700000000     D  CSTPIVA4
024800000000     D  CSTTPN4
024900000000     D  CSTCOGRAG4
025000000000     D  CSTDENOM4
025100000000     D  CSTNOME4
025200000000     D  CSTCOGNOM4
025300000000     D* Ds key video
025400000000     D DsCliForV       DS
025500000000     D  H1COGRAG1
025600000000     D  D1CF
025700000000     D  D1PARTIVA
025800000000     D  D1CDFISC
025900000000     D  D1STATO1
026000000000     D  D1IDPAES1
026100000000     D  D1PIVA1
026200000000     D  D1CDFISC1
026300000000     D  D1TPN1
026400000000     D  D1DENOM1
026500000000     D  D1NOME1
026600000000     D  D1COGNOM1
026700000000     D  D1INDIR2
026800000000     D  D1NRCIV2
026900000000     D  D1CAP2
027000000000     D  D1LOCALI2
027100000000     D  D1PROV2
027200000000     D  D1STATO2
027300000000     D  D1IDPAES2
027400000000     D  D1INDIR3
027500000000     D  D1NRCIV3
027600000000     D  D1CAP3
027700000000     D  D1LOCALI3
027800000000     D  D1PROV3
027900000000     D  D1STATO3
028000000000     D  D1IDPAES3
028100000000     D  D1STATO4
028200000000     D  D1IDPAES4
028300000000     D  D1PIVA4
028400000000     D  D1TPN4
028500000000     D  H1COGRAG4
028600000000     D  D1DENOM4
028700000000     D  D1NOME4
028800000000     D  D1COGNOM4
028900000000     D*
029000000000     D  SavCliFor      S                   Like(dsclifor) inz
029100000000     D*
029200000000     D* DS DA ANMPE
029300000000     D DsFrontesp1     DS                  INZ
029400000000     D WStato1                             Like(CSTStato1 )
029500000000     D WIdPaes1                            Like(CSTIdPaes1)
029600000000     D WPIva1                              like(CstPiva1)
029700000000     D WCdFisc1                            like(CstCdFisc1)
029800000000     D WCogRag1                            like(CstCogRag1)
029900000000     D WTpN1                               like(CstTpN1   )
030000000000     D WDenom1                             like(CstDenom1 )
030100000000     D WCognom1                            like(CstCognom1)
030200000000     D WNome1                              like(CstNome1  )
030300000000     D DsFrontesp2     DS                  INZ
030400000000     D WINDIR2                             like(CstINDIR2 )
030500000000     D WNRCIV2                             like(CstNRCIV2 )
030600000000     D WCAP2                               like(CstCAP2   )
030700000000     D WLOCALI2                            like(CstLOCALI2)
030800000000     D WPROV2                              like(CstPROV2  )
030900000000     D WStato2                             Like(CSTStato2 )
031000000000     D WIDPAES2                            like(CstIDPAES2)
031100000000     D* Stabile Organizzazione
031200000000     D DsFrontesp3     DS                  INZ
031300000000     D WINDIR3                             like(CstINDIR3 )
031400000000     D WNRCIV3                             like(CstNRCIV3 )
031500000000     D WCAP3                               like(CstCAP3   )
031600000000     D WLOCALI3                            like(CstLOCALI3)
031700000000     D WPROV3                              like(CstPROV3  )
031800000000     D WStato3                             Like(CSTStato3 )
031900000000     D WIDPAES3                            like(CstIDPAES3)
032000000000     D* Rappresentante Fiscale
032100000000     D DsFrontesp4     DS                  INZ
032200000000     D WStato4                             Like(CSTStato4 )
032300000000     D WIDPAES4                            like(CstIDPAES4)
032400000000     D WPIVA4                              like(CstPIVA4  )
032500000000     D WCogRag4                            like(CstCogRag4)
032600000000     D WTpN4                               like(CstTpN4   )
032700000000     D WDenom4                             like(CstDenom4 )
032800000000     D WCognom4                            like(CstCognom4)
032900170905R2654X**** WNome4                              like(CstNome4  )
033000170904  "  D* Rappresentante Fiscale
033100170904  "  D DsFrontesp41    DS                  INZ
033200170905R2654D WNome4                              like(CstNome4  )
033300170911R266 D WCdFisc                             like(CstCdFisc )
033400170911R266 D WCarica                        2
033500000000     D*-------------
033600000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
033700000000     D CurR            S              3S 0
033800000000     D CurC            S              3S 0
033900000000     D Win             S             99A
034000000000     D*-------------
034100000000     D* Variabili appoggio specifiche della singola anagrafica
034200000000     D*  Indici
034300000000     D X               S              5S 0
034400000000     D Y               S              5S 0
034500000000     D* Indice della schiera tasti funzione
034600000000     D PosFun          S              2  0
034700000000     D*-------------
034800000000     D* Campi di lavoro
034900000000     D WIn16           S              1
035000000000     D WIn17           S              1
035100000000     D UDateISO        S               D
035200000000     D WDtNasc         S               D
035300000000     D WSogg           S                   Like(DVSSogg)
035400000000     D WAnnoAlfa       S              4A
035500000000     D WDtAlfa         S             10A
035600000000     D WData           S               D
035700000000     D*------
035800000000     D $Partiva        S                    Like(CstPartiva)
035900000000     D $Cdfisc         S                    Like(CstCdFisc)
036000000000     D $Stato          S                    Like(CstStato1)
036100000000     D $StatoD         S                    Like(D1Stato1D)
036200000000     D $Idpaese        S                    Like(CstIdPaes1)
036300000000     D $Tpn            S                    Like(CstTpn1)
036400000000     D $Tpnd           S                    Like(D1Tpn1D)
036500000000     D $Denom          S                    Like(D1Denom1)
036600000000     D $Nome           S                    Like(D1Nome1)
036700000000     D $Cognom         S                    Like(D1Cognom1)
036800000000     D $Cap            S                    Like(CstCap2)
036900000000     D $Prv            S                    Like(CstProv2)
037000000000     D $PrvD           S                    Like(D1Prov2D)
037100000000     D $Locali         S                    Like(CstLocali2)
037200000000     D $Indir          S                    Like(CstIndir2)
037300000000     D $ErrCtrl        S              1
037400000000     D $TipErr         S              1
037500000000     D $Errscan        S              1
037600000000     D $NrCar          S              9S 0
037700000000     D $CtrObbl        S              1
037800000000     D*---------
037900000000     D $Trovato        S              1
038000000000     D $Cont           S              1S 0
038100170905R2654D $DimSk          C                   Const(5)
038200000000     D*---------
038300000000     D DataDs          DS
038400000000     D  AnnoDs                        4A
038500000000     D   Tratt1                       1A   Inz('-')
038600000000     D  MeseDs                        2A
038700000000     D   Tratt2                       1A   Inz('-')
038800000000     D  GiornoDs                      2A
038900000000     D*----------
039000000000     D $Data10A1       S             10
039100000000     D $Data10A2       S             10
039200000000     D $DTrif          S                    Like(DtRif0701) inz
039300000000     D*------
039400000000     D $Esito          S              1
039500000000     D $Prenot         S              1
039600000000     D $Sys            S                    Like(CSTSys    )
039700000000     D $NrAsReg        S                    Like(CSTNrAsReg)
039800000000     D*-------------
039900000000     D WStato          S                   Like(XScSta )
040000000000     D $CodUIC         S                   Like(CODUICG12)
040100000000     D $UICSoc         S                   Like(CODUICG12)
040200000000     D $NazSoc         S                   Like(NazG12 )
040300000000     D $NazISO         S                   Like(NazG12 )
040400000000     D $FineG12        S              1    Inz(*Off)
040500000000     D*---------
040600000000     D* Lista caratteri da convertire e ammessi (Code Table Basic Latin) ASCII
040700000000     D CnvErr          C                   Const('ÁÃÉÍÑÓÚ[ÀÈÒÙÌ&')
040800000000     D CnvOk           C                   Const('AAEINOU AEOUIE')
040900000000     D CharOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
041000000000     D                                            abcdefghijklmnopqrstuvwxyz-
041100000000     D                                            01234567890/+-?:().,'''' ')
041200000000     D $FineScan       S              1    Inz(*Off)
041300000000     D $CampoScan      S            100A
041400000000     C*----------------------------------------------------*
041500000000     C*                MAIN LINE PROGRAM
041600000000     C*----------------------------------------------------*
041700000000     C*
041800000000     C* inizializzazione variabili
041900000000     C                   EXSR      INZVAR
042000000000     C*
042100000000     C     $FINE         DOWEQ     *OFF
042200000000     C     $GEST         CASEQ     'D1'          GESD1
042300000000     C     $GEST         CASEQ     'D2'          GESD2
042400000000     C     $GEST         CASEQ     'D3'          GESD3
042500000000     C                   ENDCS
042600000000     C                   ENDDO
042700000000     C*
042800000000     C* fine programma
042900000000     C                   EXSR      ENDPGM
043000000000     C/EJECT
043100000000     C************************************************************
043200000000     C* Fine Programma
043300000000     C************************************************************
043400000000     C     ENDPGM        BEGSR
043500000000     C*
043600000000     C* se non esco per errore
043700000000    >C     ErrI74        IFEQ      '0'
043800000000     C*
043900000000     C* se il COMMIT non è demandato al pgm chiamante
044000000000    >C     CMTI74        IFEQ      '0'
044100000000     C*
044200000000     C* test se esistono transazioni attive ed eventualmente cosa fare
044300000000     C                   Z-ADD     0             EXECOD
044400000000     C                   CALL      'XRCMTI'
044500000000     C                   PARM                    EXECOD            1 0
044600000000     C*
044700000000     C                   SELECT
044800000000     C* Annulla richiesta
044900000000     C     EXECOD        WHENEQ    1
045000000000     C                   MOVEL     *OFF          $FINE
045100000000     C                   MOVEL     *HIVAL        $LASTV
045200000000     C* Conferma uscita
045300000000     C     EXECOD        WHENEQ    2
045400000000     C*                  ROLBK
045500000000     C*
045600000000     C                   ENDSL
045700000000     C*
045800000000     C                   ENDIF
045900000000     C*
046000000000     C                   ENDIF
046100000000     C*
046200000000     C* se ho confermato l'uscita proseguo
046300000000     C     $FINE         IFEQ      *ON
046400000000     C*
046500000000     C* disallocazione rcd nel caso di modifica/annullamento
046600000000    >C     OPZI74        IFEQ      '02'
046700000000    >C     OPZI74        OREQ      '04'
046800000000     C*
046900000000     C* chiudo in LR per disallocare record
047000000000     C********           SETON                                            LR
047100000000     C                   Else
047200000000     C********           SETON                                            RT
047300000000     C                   ENDIF
047400000000     C*
047500000000     C* chiudo in LR per disallocare record
047600000000     C                   SETON                                            LR
047700000000     C                   RETURN
047800000000     C*
047900000000     C                   ENDIF
048000000000     C*
048100000000     C                   ENDSR
048200000000     C/EJECT
048300000000     C************************************************************
048400000000     C* RICERCHE
048500000000     C************************************************************
048600000000     C     SEARCH        BEGSR
048700000000     C*
048800000000     C* determino Riga/Colonna del cursore
048900000000     C                   MOVE      NRG           R$$
049000000000     C                   MOVE      NCL           C$$
049100000000     C                   Z-ADD     RIRI          CURR
049200000000     C                   Z-ADD     COCO          CURC
049300000000     C*
049400000000     C                   SELECT
049500000000     C*
049600000000     C*  Soggetto
049700000000     C     H1NMFL        WHENEQ    'D1SOGG'
049800000000     C                   EXSR      F4_Sogg
049900000000     C*
050000000000     C*  Natura soggetto
050100000000     C     H1NMFL        WHENEQ    'D1CF'
050200000000     C                   EXSR      F4_CF
050300000000     C*
050400000000     C*  Partita IVA
050500000000     C     H1NMFL        WHENEQ    'D1PARTIVA'
050600000000     C                   Movel     D1PartIva     $PartIva
050700000000     C                   EXSR      F4_PartIva
050800000000     C                   Movel     $Partiva      D1PartIva
050900000000     C*
051000000000     C*  Codice fiscale
051100000000     C     H1NMFL        WHENEQ    'D1CDFISC'
051200000000     C                   Movel     D1CdFisc      $CdFisc
051300000000     C                   EXSR      F4_CdFisc
051400000000     C                   Movel     $CdFisc       D1CdFisc
051500000000     C*
051600000000     C*  Stato1
051700000000     C     H1NMFL        WHENEQ    'D1STATO1 '
051800000000     C                   Movel     D1Stato1      $Stato
051900000000     C                   EXSR      F4_Stato
052000000000     C                   Movel     $Stato        D1Stato1
052100000000     C                   Movel     $IdPaese      D1IdPaes1
052200000000     C                   Movel     $Statod       D1Stato1D
052300000000     C*
052400000000     C*  Partita IVA 1
052500000000     C     H1NMFL        WHENEQ    'D1PIVA1  '
052600000000     C                   Movel     D1PIva1       $PartIva
052700000000     C                   EXSR      F4_PartIva
052800000000     C                   Movel     $PartIva      D1PIva1
052900000000     C*
053000000000     C*  Codice fiscale
053100000000     C     H1NMFL        WHENEQ    'D1CDFISC1'
053200000000     C                   Movel     D1CdFisc1     $CdFisc
053300000000     C                   EXSR      F4_CdFisc
053400000000     C                   Movel     $CdFisc       D1CdFisc1
053500000000     C*
053600000000     C*  Tipo natura giuridica
053700000000     C     H1NMFL        WHENEQ    'D1TPN1'
053800000000     C                   Movel     D1Tpn1        $Tpn
053900000000     C                   EXSR      F4_TPN
054000000000     C                   Movel     $Tpn          D1Tpn1
054100000000     C*
054200000000     C* SEDE
054300000000     C*
054400000000     C*  Stato 2
054500000000     C     H1NMFL        WHENEQ    'D1STATO2'
054600000000     C                   Movel     D1Stato2      $Stato
054700000000     C                   EXSR      F4_Stato
054800000000     C                   Movel     $Stato        D1Stato2
054900000000     C                   Movel     $IdPaese      D1IdPaes2
055000000000     C                   Movel     $Statod       D1Stato2D
055100000000     C*
055200000000     C*  provincia 2
055300000000     C     H1NMFL        WHENEQ    'D1PROV2  '
055400000000     C                   Movel     D1Prov2       $Prv
055500000000     C                   Movel     D1Stato2      $Stato
055600000000     C                   EXSR      F4_Prv
055700000000     C                   If        $Prv <> *blanks
055800000000     C                   Movel     $Prv          D1Prov2
055900000000     C                   Movel     $Stato        D1Stato2
056000000000     C                   Endif
056100000000     C*
056200000000     C*  Cap
056300000000     C     H1NMFL        WHENEQ    'D1CAP2   '
056400000000     C                   Movel     D1Cap2        $CAP
056500000000     C                   Movel     D1Stato2      $Stato
056600000000     C                   EXSR      F4_Cap
056700000000     C                   If        $Cap <> *blanks
056800000000     C                   Movel     $Cap          D1Cap2
056900000000     C                   Movel     $Stato        D1Stato2
057000000000     C                   Movel     $Prv          D1Prov2
057100000000     C                   Movel     $Locali       D1Locali2
057200000000     C                   Endif
057300000000     C*
057400000000     C* STABILE ORGANIZZAZIONE
057500000000     C*
057600000000     C*  Stato
057700000000     C     H1NMFL        WHENEQ    'D1STATO3'
057800000000     C                   Movel     D1Stato3      $Stato
057900000000     C                   EXSR      F4_Stato
058000000000     C                   Movel     $Stato        D1Stato3
058100000000     C                   Movel     $IDPaese      D1IdPaes3
058200000000     C                   Movel     $StatoD       D1Stato3D
058300000000     C*
058400000000     C*  Provincia
058500000000     C     H1NMFL        WHENEQ    'D1PROV3  '
058600000000     C                   Movel     D1Prov3       $PRV
058700000000     C                   Movel     D1Stato3      $Stato
058800000000     C                   EXSR      F4_Prv
058900000000     C                   If        $Prv <> *Blanks
059000000000     C                   Movel     $Prv          D1Prov3
059100000000     C                   Movel     $Stato        D1Stato3
059200000000     C                   Endif
059300000000     C*
059400000000     C*  Cap
059500000000     C     H1NMFL        WHENEQ    'D1CAP3   '
059600000000     C                   Movel     D1Cap3        $CAP
059700000000     C                   Movel     D1Stato3      $Stato
059800000000     C                   EXSR      F4_Cap
059900000000     C                   If        $Cap <> *blanks
060000000000     C                   Movel     $Cap          D1Cap3
060100000000     C                   Movel     $Stato        D1Stato3
060200000000     C                   Movel     $Prv          D1Prov3
060300000000     C                   Movel     $Locali       D1Locali3
060400000000     C                   Endif
060500000000     C*
060600000000     C* RAPPRESENTANTE FISCALE
060700000000     C*
060800000000     C*  Stato
060900000000     C     H1NMFL        WHENEQ    'D1STATO4'
061000000000     C                   Movel     D1Stato4      $Stato
061100000000     C                   EXSR      F4_Stato
061200000000     C                   Movel     $Stato        D1Stato4
061300000000     C                   Movel     $IDPaese      D1IdPaes4
061400000000     C                   Movel     $StatoD       D1Stato4D
061500000000     C*  Tipo natura giuridica
061600000000     C     H1NMFL        WHENEQ    'D1TPN4'
061700000000     C                   Movel     D1Tpn4        $Tpn
061800000000     C                   EXSR      F4_TPN
061900000000     C                   Movel     $Tpn          D1Tpn4
062000000000     C* ricerca non abilitata in questa posizione
062100000000     C                   OTHER
062200000000     C                   SETON                                        9899
062300000000     C*
062400000000     C                   ENDSL
062500000000     C*
062600000000     C* imposto pos. del cursore
062700000000     C                   Z-ADD     CURR          H1RIGA
062800000000     C                   Z-ADD     CURC          H1COLO
062900000000     C*
063000000000     C                   MOVE      *HIVAL        $LASTV
063100000000     C*
063200000000     C                   ENDSR
063300000000     C/EJECT
063400000000     C************************************************************
063500000000     C* Ricerca codice soggetto
063600000000     C************************************************************
063700000000     C     F4_Sogg       BEGSR
063800000000     C*
063900000000     C                   RESET                   ANA040DS
064000000000     C*
064100000000     C                   MOVEL     '01'          OPZ040
064200000000     C                   MoveL     D1Sogg        CoD040
064300000000     C                   MOVEL     ANA040DS      KPJBU
064400000000     C                   CALL      'ANA040R'
064500000000     C                   PARM                    KPJBA
064600000000     C*
064700000000     C                   MOVEL     KPJBU         ANA040DS
064800000000     C*
064900000000     C                   if        Ret040     <> '2'
065000000000     C                   MoveL     CoD040        D1Sogg
065100000000     C                   endif
065200000000     C*
065300000000     C                   ENDSR
065400000000     C/EJECT
065500000000     C************************************************************
065600000000     C* Ricerca natura del conto
065700000000     C************************************************************
065800000000     C     F4_CF         BEGSR
065900000000     C*
066000000000     C                   Reset                   ANA964DS
066100000000     C*
066200000000     C                   MoveL     '01'          Opz964
066300000000     C                   MoveL     'SDGCST000'   Trc964
066400000000     C                   MoveL     'CSTCF    '   Cam964
066500000000     C                   MoveL     *On           All964
066600000000     C*
066700000000     C                   MoveL     ANA964DS      KPJBU
066800000000     C                   CALL      'ANA964R'
066900000000     C                   PARM                    KPJBA
067000000000     C                   MoveL     KPJBU         ANA964DS
067100000000     C*
067200000000     C                   if        Ret964     <> '2'
067300000000     C                   MoveL     Val964        D1CF
067400000000     C                   endif
067500000000     C*
067600000000     C                   ENDSR
067700000000      /EJECT
067800000000     C************************************************************
067900000000     C* Ricerca tipo natura giuridica
068000000000     C************************************************************
068100000000     C     F4_TpN        BEGSR
068200000000     C*
068300000000     C                   Reset                   ANA964DS
068400000000     C*
068500000000     C                   MoveL     '01'          Opz964
068600000000     C                   MoveL     'SDGCST000'   Trc964
068700000000     C                   MoveL     'CSTTPN1   '  Cam964
068800000000     C*
068900000000     C                   MoveL     ANA964DS      KPJBU
069000000000     C                   CALL      'ANA964R'
069100000000     C                   PARM                    KPJBA
069200000000     C                   MoveL     KPJBU         ANA964DS
069300000000     C*
069400000000     C                   if        Ret964     <> '2'
069500000000     C                   MoveL     Val964        $Tpn
069600000000     C                   endif
069700000000     C*
069800000000     C                   ENDSR
069900000000     C/EJECT
070000000000     C************************************************************
070100000000     C* Ricerca codice IVA
070200000000     C************************************************************
070300000000     C     F4_PartIva    BEGSR
070400000000     C*
070500000000     C                   RESET                   ANA040DS
070600000000     C*
070700000000     C                   MOVEL     '01'          OPZ040
070800000000     C                   MoveL     $PartIva      IVA040
070900000000     C                   MoveL     D1Sogg        CoD040
071000000000     C                   MOVEL     ANA040DS      KPJBU
071100000000     C                   CALL      'ANA040R'
071200000000     C                   PARM                    KPJBA
071300000000     C*
071400000000     C                   MOVEL     KPJBU         ANA040DS
071500000000     C*
071600000000     C                   if        Ret040     <> '2'
071700000000     C*
071800000000     C                   MoveL     CoD040        WSogg
071900000000     C                   EXSR      CallDvaSog
072000000000     C*
072100000000     C                   if        DVAErrore  <> '1'
072200000000     C*
072300000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
072400000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
072500000000     C*
072600000000     C                   MoveL     DVSPartIVA    $PartIva
072700000000     C*
072800000000     C                   endif
072900000000     C*
073000000000     C                   endif
073100000000     C*
073200000000     C                   ENDSR
073300000000      /EJECT
073400000000     C************************************************************
073500000000     C* Ricerca codice fiscale
073600000000     C************************************************************
073700000000     C     F4_CdFisc     BEGSR
073800000000     C*
073900000000     C                   RESET                   ANA040DS
074000000000     C*
074100000000     C                   MOVEL     '01'          OPZ040
074200000000     C                   MoveL     D1Sogg        CoD040
074300000000     C                   MoveL     $CdFisc       CdF040
074400000000     C                   MOVEL     ANA040DS      KPJBU
074500000000     C                   CALL      'ANA040R'
074600000000     C                   PARM                    KPJBA
074700000000     C*
074800000000     C                   MOVEL     KPJBU         ANA040DS
074900000000     C*
075000000000     C                   if        Ret040     <> '2'
075100000000     C*
075200000000     C                   MoveL     CoD040        WSogg
075300000000     C                   EXSR      CallDvaSog
075400000000     C*
075500000000     C                   if        DVAErrore  <> '1'
075600000000     C*
075700000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
075800000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
075900000000     C*
076000000000     C                   MoveL     DVSCdFisc     $CdFisc
076100000000     C*
076200000000     C                   endif
076300000000     C*
076400000000     C                   endif
076500000000     C*
076600000000     C                   ENDSR
076700000000     C/EJECT
076800000000     C************************************************************
076900000000     C* Ricerca codice Stato
077000000000     C************************************************************
077100000000     C     F4_Stato      BEGSR
077200000000     C*
077300000000     C                   Reset                   XTabDS
077400000000     C*
077500000000     C                   Move      '01'          XTaOpz
077600000000     C                   Move      XScSoc        XTaAzi
077700000000     C                   Move      XScLin        XTaLin
077800000000     C                   Z-add     Curr          XTaRig
077900000000     C                   Z-add     Curc          XTaCol
078000000000     C                   Move      *Blank        XTaKey
078100000000     C*
078200000000     C                   Clear                   KPJBU
078300000000     C                   MoveL     XTabDS        KPJBU
078400000000     C*
078500000000     C                   CALL      'SDGHD4R'
078600000000     C                   PARM                    KPJBA
078700000000     C                   MoveL     KPJBU         XTabDS
078800000000     C*
078900000000     C                   if        XTaErr     <> '1'      and
079000000000     C                             XTaKey     <> *Blank
079100000000     C                   Move      XTaKey        $Stato
079200000000     C*
079300000000     C* Reperisco anche lo stato ISO
079400000000     C                   Clear                   $IdPaese
079500000000     C                   Clear                   $StatoD
079600000000     C*
079700000000     C                   Clear                   SDGHD4DS
079800000000     C                   Clear                   SDGTABDS
079900000000     C                   Eval      XTbRic      = '1'
080000000000     C                   Eval      XTbAzi      = XScSoc
080100000000     C                   Eval      XTbKey      = *Zero
080200000000     C                   Eval      XTbCod      = 'HD4'
080300000000     C                   Move      $Stato        XTbKey
080400000000     C*
080500000000     C                   CALL      'SDGTABDR'
080600000000     C                   PARM                    SDGTABDS
080700000000     C*
080800000000     C                   If        Xtberr  <> '1'
080900000000     C                   MoveL     XTbUni        SDGHD4DS
081000000000     C                   MoveL(P)  DesHd4        $Statod
081100000000     C                   MoveL     Codiso2Hd4    $IdPaese
081200000000     C                   Endif
081300000000     C                   endif
081400000000     C*
081500000000     C                   ENDSR
081600000000      /EJECT
081700000000     C************************************************************
081800000000     C* Ricerca codice provincia
081900000000     C************************************************************
082000000000     C     F4_Prv        BEGSR
082100000000     C*
082200000000     C                   Reset                   XTabDS
082300000000     C*
082400000000     C                   Clear                   Campo6            6
082500000000     C*
082600000000     C                   Move      '01'          XTaOpz
082700000000     C                   Move      XScSoc        XTaAzi
082800000000     C                   Move      XScLin        XTaLin
082900000000     C                   Z-add     Curr          XTaRig
083000000000     C                   Z-add     Curc          XTaCol
083100000000     C                   Move      *Blank        XTaKey
083200000000     C                   Movel     $Stato        Campo6
083300000000     C                   Move      Campo6        XtaKey
083400000000     C*
083500000000     C                   Clear                   KPJBU
083600000000     C                   MoveL     XTabDS        KPJBU
083700000000     C*
083800000000     C                   CALL      'ANGC17R'
083900000000     C                   PARM                    KPJBA
084000000000     C                   MoveL     KPJBU         XTabDS
084100000000     C*
084200000000     C                   if        XTaErr     <> '1'      and
084300000000     C                             XTaKey     <> *Blank
084400000000     C                   Move      XTaKey        Campo6
084500000000     C                   Move      Campo6        $Prv
084600000000     C                   Movel     Campo6        $Stato
084700000000     C                   endif
084800000000     C*
084900000000     C                   ENDSR
085000000000      /EJECT
085100000000     C************************************************************
085200000000     C* Ricerca codice cap
085300000000     C************************************************************
085400000000     C     F4_Cap        BEGSR
085500000000     C                   Clear                   $Cap
085600000000     C                   Reset                   A801DS
085700000000     C                   Move      '01'          OPZ801
085800000000     C                   Move      XscSoc        Societa801
085900000000     C                   Movel     $Stato        Stato801
086000000000     C                   Movel     $Cap          Cap801
086100000000     C                   Z-Add     CURR          XTARIG
086200000000     C                   Z-Add     CURC          XTACOL
086300000000     C                   Clear                   KPJBU
086400000000     C                   Movel     A801DS        KPJBU
086500000000     C                   Call      'ANA801R'
086600000000     C                   Parm                    KPJBA
086700000000     C                   Movel     KPJBU         A801DS
086800000000     C     ERR801        IfNe      '1'
086900000000     C                   Movel     CAP801        $Cap
087000000000     C                   Else
087100000000     C                   Movel     *BLANK        $Cap
087200000000     C                   EndIf
087300000000     C                   If        $Cap <> *blanks
087400000000     C                   Eval      $Stato =  Stato801
087500000000     C                   Eval      $Prv   =  Prov801
087600000000     C                   Eval      $Locali=  Localit801
087700000000     C                   EndIf
087800000000     C*
087900000000     C                   ENDSR
088000000000      /EJECT
088100000000     C************************************************************
088200000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
088300000000     C************************************************************
088400000000     C     GESPIE        BEGSR
088500000000     C*
088600000000     C                   SELECT
088700000000     C*
088800000000     C     $GEST         WHENEQ    'D1'
088900000000     C     $ULKD1        IFEQ      0
089000000000     C* riempimento opzioni
089100000000     C                   EXSR      F24D1
089200000000     C                   ELSE
089300000000     C                   WRITE     FMTZ1
089400000000     C                   ENDIF
089500000000     C*
089600000000     C     $GEST         WHENEQ    'D2'
089700000000     C     $ULKD1        IFEQ      0
089800000000     C* riempimento opzioni
089900000000     C                   EXSR      F24D2
090000000000     C                   ELSE
090100000000     C                   WRITE     FMTZ1
090200000000     C                   ENDIF
090300000000     C*
090400000000     C     $GEST         WHENEQ    'D3'
090500000000     C     $ULKD1        IFEQ      0
090600000000     C* riempimento opzioni
090700000000     C                   EXSR      F24D3
090800000000     C                   ELSE
090900000000     C                   WRITE     FMTZ1
091000000000     C                   ENDIF
091100000000     C                   ENDSL
091200000000     C*
091300000000     C                   ENDSR
091400000000     C/EJECT
091500000000     C************************************************************
091600000000     C* Chiamata programma con tasto personalizzato
091700000000     C************************************************************
091800000000     C     FPerD1        BEGSR
091900000000     C*
092000000000     C* salvo la DS di ingresso
092100000000    >C                   MOVEL     SDGSI74DS     $SaveDS
092200000000     C*
092300000000    >C                   MOVE      xscsoc        SocietaI74
092400000000     C* passare nei campi appositi della DS SDGSI74DS il valore
092500000000     C* della chiave della VL univoca
092600000000     C*
092700000000    >C***                MOVEL     SDGSI74DS     KPJBU
092800000000     C                   CALL      $PGF1(PosFun)                        22
092900000000     C                   PARM                    KPJBA
093000000000     C                   PARM                    sdgsi74ds
093100000000     C                   PARM                    NOMPGM
093200000000     C*
093300000000     C* ripristino la DS di ingresso
093400000000    >C                   MOVEL     $SaveDS       SDGSI74DS
093500000000     C*
093600000000     C                   ENDSR
093700000000     C/EJECT
093800000000     C************************************************************
093900000000     C* GESTIONE VIDEO RECORD D1
094000000000     C************************************************************
094100000000     C     GESD1         BEGSR
094200000000     C* inizializzazione videata
094300000000   B1C     $INZD1        IFEQ      *ON
094400000000     C                   EXSR      INZD1
094500000000     C                   MOVE      *OFF          $INZD1
094600000000   E1C                   ENDIF
094700000000     C* emissione piede videata se proveniente da altra
094800000000     C* salvataggio valore formato di provenienza
094900000000   B1C     $LASTV        IFNE      'D1'
095000000000     C                   WRITE     FMTT1
095100000000     C                   EXSR      GESPIE
095200000000     C                   MOVE      $LASTV        $LASTG
095300000000     C                   MOVE      'D1'          $LASTV
095400000000   E1C                   ENDIF
095500000000     C*
095600000000     C* se esistono errori sulla videata
095700000000     C* emetto la write del formato a indicatori spenti per vedere
095800000000     C* le eventuali decodifiche
095900000000   B1C                   if        (*IN99       = *On   or
096000000000   B1C                              *In16       = *On ) and
096100000000   B1C                              $Primavolta <>*On   and
096200000000     C                             $GEST        = 'D1'
096300000000     C                   MOVEA     *IN           WIN
096400000000     C                   MOVE      *ALL'0'       IN5098           49
096500000000     C                   MOVEA     IN5098        *IN(50)
096600000000     C                   MOVEA     WIN           *IN
096700000000     C                   Eval      WIn16        = *In16
096800000000     C                   Eval      *In16        = *Off
096900000000     C                   WRITE     FMTD1
097000000000     C                   MOVEA     WIN           *IN
097100000000     C                   Eval      *In16        = WIn16
097200000000   E1C                   ENDIF
097300000000   B1C                   if         $Primavolta = *On
097400000000     C                   MOVE      *ALL'0'       IN5098
097500000000     C                   MOVEA     IN5098        *IN(50)
097600000000     C                   endif
097700000000     C* Inizializzazione indice tasti funzione personalizzati
097800000000     C                   Eval      PosFun = 25
097900000000     C*
098000000000   B1C                   SELECT
098100000000     C* Annullamento
098200000000    >C     OPZI74        WHENEQ    '04'
098300000000     C* Visualizzazione
098400000000    >C     OPZI74        OREQ      '05'
098500000000     C                   WRITE     FMTD1
098600000000     C                   EXFMT     PROTECT
098700000000     C* Immissione/Modifica/Copia
098800000000   X1C                   OTHER
098900000000     C                   EXFMT     FMTD1
099000000000   E1C                   ENDSL
099100000000     C*
099200000000     C* azzero la posizione del cursore
099300000000     C                   Z-ADD     0             H1RIGA
099400000000     C                   Z-ADD     0             H1COLO
099500000000     C*
099600000000     C* controllo abilitazione tasti funzione
099700000000     C                   EXSR      CTRKD1
099800000000     C*
099900000000   B1C     *IN99         IFEQ      *OFF
100000000000     C*
100100000000   B2C                   SELECT
100200000000     C* ROLLUP
100300000000    >C     $TASTO        WHENEQ    ROLLUP
100400000000    >C                   MOVE      'D2'          $GEST
100500000000     C* F2=Decodifica
100600000000     C     $TASTO        WHENEQ    F02
100700000000     C                   EXSR      F02D1
100800000000     C* F3=Fine
100900000000     C     $TASTO        WHENEQ    F03
101000000000     C                   EXSR      F03D1
101100000000     C* F4=Ricerca
101200000000     C     $TASTO        WHENEQ    F04
101300000000     C                   EXSR      SEARCH
101400000000     C* F5=Refresh
101500000000     C     $TASTO        WHENEQ    F05
101600000000     C                   EXSR      F05D1
101700000000     C* F8=Successivo
101800000000     C     $TASTO        WHENEQ    F08
101900000000     C                   EXSR      F08D1
102000000000     C* F12=Ritorno
102100000000     C     $TASTO        WHENEQ    F12
102200000000     C                   EXSR      F12D1
102300000000     C* F24=Altri tasti
102400000000     C     $TASTO        WHENEQ    F24
102500000000     C                   EXSR      F24D1
102600000000     C* Personalizzata
102700000000     C* NB chiamata NON possibile perchè SDGSI74DS più lunga
102800000000     C* di KPJBU 256 caratteri                          a
102900000000     C**** $PGF1(PosFun) WHENNE    *BLANK
103000000000     C****               EXSR      FPerD1
103100000000     C*
103200000000   X2C                   OTHER
103300000000     C                   EXSR      CTRD1
103400000000     C* aggiungere le chiamate ai controlli delle videate supplementari
103500000000    >C     *IN99         IFEQ      *OFF
103600000000    >C                   EXSR      CTRD2
103700000000   B1C                   if        *IN99       = *On
103800000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
103900000000    >C* indicatori uguali in videate differenti, genererei conflitti
104000000000    >C                   MOVE      *ALL'0'       IN5098           49
104100000000    >C                   MOVEA     IN5098        *IN(50)
104200000000    >C                   SETON                                        9996                       96
104300000000    >C                   ENDIF
104400000000    >C                   ENDIF
104500000000     C* aggiungere le chiamate ai controlli delle videate supplementari
104600000000    >C     *IN99         IFEQ      *OFF
104700000000    >C                   EXSR      CTRD3
104800000000   B1C                   if        *IN99       = *On
104900000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
105000000000    >C* indicatori uguali in videate differenti, genererei conflitti
105100000000    >C                   MOVE      *ALL'0'       IN5098           49
105200000000    >C                   MOVEA     IN5098        *IN(50)
105300000000    >C                   SETON                                        9996                       96
105400000000    >C                   ENDIF
105500000000    >C                   ENDIF
105600000000     C* aggiungere le chiamate ai controlli delle videate supplementari
105700000000     C*
105800000000   B3C     $TASTO        IFEQ      F06
105900000000     C     *IN99         ANDEQ     *OFF
106000000000     C* eseguo aggiornamento
106100000000     C                   EXSR      AGGANA
106200000000     C* eseguo operazioni del dopo-aggiornamento
106300000000   B4C     *IN99         IFEQ      *OFF
106400000000     C                   EXSR      GESAGG
106500000000   E4C                   ENDIF
106600000000   E3C                   ENDIF
106700000000     C*
106800000000   E2C                   ENDSL
106900000000     C*
107000000000   E1C                   ENDIF
107100000000     C*
107200000000     C                   ENDSR
107300000000     C/EJECT
107400000000     C************************************************************
107500000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
107600000000     C************************************************************
107700000000     C     CTRKD1        BEGSR
107800000000     C*
107900000000     C                   SETOFF                                       99
108000000000     C                   MOVEA     $FC1          $FC
108100000000     C                   EXSR      CTRKEY
108200000000     C     *IN99         IFEQ      *ON
108300000000     C                   SETON                                        97
108400000000     C                   ENDIF
108500000000     C*
108600000000     C                   ENDSR
108700000000     C/EJECT
108800000000     C************************************************************
108900000000     C* INIZIALIZZAZIONE VIDEATA DATI
109000000000     C************************************************************
109100000000     C     INZD1         BEGSR
109200000000     C*
109300000000     C                   CLEAR                   FMTD1
109400000000    >C                   CLEAR                   FMTD2
109500000000    >C                   CLEAR                   FMTD3
109600000000     C*
109700000000     C                   Eval      D1Anno      = AnnoI74
109800000000     C                   Eval      D1Trim      = TrimI74
109900000000     C*
110000000000     C* Se opzione 20 reperisco i dati del frontespizio, se presenti
110100000000     C*
110200000000     C* per tutte le opzioni NON immissione
110300000000     C     OPZI74        IFNE      '01'
110400000000     C*
110500000000   B2C     *IN22         DOUEQ     *OFF
110600000000     C     MSGRTN        ORNE      'R'
110700000000     C* Se viene chiamato dal dettaglio, aggancio con il numero record
110800000000     C                   Select
110900000000     C*
111000000000     C                   When      NrrCstI74 <> 0
111100000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
111200000000   B>C     OPZI74        IFEQ      '02'
111300000000    >C     OPZI74        OREQ      '04'
111400000000     C     NRRCSTI74     Chain     SdGcst00f                          2122
111500000000     C                   Else
111600000000     C     NRRCSTI74     Chain(N)  SdGcst00f                          21
111700000000     C                   Endif
111800000000     C*
111900000000     C                   When      OPZI74 = '20'
112000000000     C*
112100000000     C                   EXSR      RepFronteSP
112200000000     C*
112300000000     C                   Other
112400000000     C                   Eval      CSTSocieta   = XScSoc
112500000000     C                   Eval      CSTTpCom     = TpcomI74
112600000000     C                   Eval      CSTTpRec     = TprecI74
112700000000     C                   Eval      CSTAnno      = AnnoI74
112800000000     C                   Eval      CSTTrim      = TrimI74
112900000000     C                   Eval      CSTMese      = MeseI74
113000000000     C*
113100000000     C                   Eval      CSTCOGRAG1 =  COGRAG1I74
113200000000     C                   Eval      CSTCF      =  CFI74
113300000000     C                   Eval      CSTPARTIVA =  PARTIVAI74
113400000000     C                   Eval      CSTCDFISC  =  CDFISCI74
113500000000     C                   Eval      CSTSTATO1  =  STATO1I74
113600000000     C                   Eval      CSTIDPAES1 =  IDPAES1I74
113700000000     C                   Eval      CSTPIVA1   =  PIVA1I74
113800000000     C                   Eval      CSTCDFISC1 =  CDFISC1I74
113900000000     C                   Eval      CSTTPN1    =  TPN1I74
114000000000     C                   Eval      CSTDENOM1  =  DENOM1I74
114100000000     C                   Eval      CSTNOME1   =  NOME1I74
114200000000     C                   Eval      CSTCOGNOM1 =  COGNOM1I74
114300000000     C                   Eval      CSTINDIR2  =  INDIR2I74
114400000000     C                   Eval      CSTNRCIV2  =  NRCIV2I74
114500000000     C                   Eval      CSTCAP2    =  CAP2I74
114600000000     C                   Eval      CSTLOCALI2 =  LOCALI2I74
114700000000     C                   Eval      CSTPROV2   =  PROV2I74
114800000000     C                   Eval      CSTSTATO2  =  STATO2I74
114900000000     C                   Eval      CSTIDPAES2 =  IDPAES2I74
115000000000     C                   Eval      CSTINDIR3  =  INDIR3I74
115100000000     C                   Eval      CSTNRCIV3  =  NRCIV3I74
115200000000     C                   Eval      CSTCAP3    =  CAP3I74
115300000000     C                   Eval      CSTLOCALI3 =  LOCALI3I74
115400000000     C                   Eval      CSTPROV3   =  PROV3I74
115500000000     C                   Eval      CSTSTATO3  =  STATO3I74
115600000000     C                   Eval      CSTIDPAES3 =  IDPAES3I74
115700000000     C                   Eval      CSTSTATO4  =  STATO4I74
115800000000     C                   Eval      CSTIDPAES4 =  IDPAES4I74
115900000000     C                   Eval      CSTPIVA4   =  PIVA4I74
116000000000     C                   Eval      CSTTPN4    =  TPN4I74
116100000000     C                   Eval      CSTCOGRAG4 =  COGRAG4I74
116200000000     C                   Eval      CSTDENOM4  =  DENOM4I74
116300000000     C                   Eval      CSTNOME4   =  NOME4I74
116400000000     C                   Eval      CSTCOGNOM4 =  COGNOM4I74
116500000000     C                   Movel     DsCliFor      CstCliFor
116600000000     C                   Movel     DsCliFor      SavCliFor
116700000000     C*
116800000000     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
116900000000   B>C     OPZI74        IFEQ      '02'
117000000000    >C     OPZI74        OREQ      '04'
117100000000     C*
117200000000     C     K07CST02      Chain     SDGCST02L                          2122
117300000000     C                   Else
117400000000     C     K07CST02      Chain(N)  SDGCST02L                          21
117500000000     C                   Endif
117600000000     C*
117700000000     C                   ENDSL
117800000000     C*
117900000000     C* Gestione RCD allocato
118000000000   B3C     *IN22         IFEQ      *ON
118100000000    >C                   MOVEL     'SDGCST00F'   MSGFIL
118200000000     C                   EXSR      MSGPGM
118300000000   E3C                   ENDIF
118400000000   E2C                   ENDDO
118500000000     C*
118600000000     C* se il rcd da gestire non viene trovato
118700000000     C* oppure è allocato da altro pgm
118800000000     C* ritorno al pgm guida avvertendolo dell'errore
118900000000   B2C     *IN21         IFEQ      *ON
119000000000     C     *IN22         OREQ      *ON
119100000000     C                   MOVE      *ON           $FINE
119200000000    >C                   MOVE      '1'           ERRI74
119300000000     C*
119400000000     C* altrimenti
119500000000   X2C                   ELSE
119600000000     C* riempio la videata
119700000000     C                   If        OPZI74 <> '20'
119800000000     C                   EXSR      RIED1
119900000000     C                   Endif
120000000000    >C* Aggiungere le routine di riempimento dei record succcessivi
120100000000     C* richiamo routine dei ctrl per decodificarla
120200000000     C                   EXSR      CTRD1
120300000000    >C* Aggiungere le routine di controllo dei record succcessivi
120400000000    >C                   EXSR      CTRD2
120500000000    >C                   EXSR      CTRD3
120600000000     C                   MOVE      *ALL'0'       IN5098
120700000000     C                   MOVEA     IN5098        *IN(50)
120800000000   E2C                   ENDIF
120900000000     C*
121000000000     C* per immissione
121100000000   X1C                   ELSE
121200000000     C*
121300000000     C* riempio con i default di immissione  (se reperiti)
121400000000     C                   IF        $FlgDef     = *on
121500000000     C                   EXSR      RIEDEF
121600000000     C                   ENDIF
121700000000     C*
121800000000     C* imposto i campi chiave ricevuti anche se si è in immissione
121900000000     C* perchè potrebbero essere stati passati dei default di chiave
122000000000     C                   Eval      D1Sogg      = SoggI74
122100000000     C                   Eval      D1Cf        = CFI74
122200000000     C                   Eval      D1PartIva   = PartIvaI74
122300000000     C                   Eval      D1CdFisc    = CdFiscI74
122400000000     C                   Eval      D1Tpn1      = Tpn1I74
122500000000     C                   Eval      D1Stato1    = Stato1I74
122600000000     C                   Eval      D1IdPaes1   = IdPaes1I74
122700000000     C                   Eval      D1PIva1     = PIva1I74
122800000000     C                   Eval      D1CdFisc1   = CdFisc1I74
122900000000     C                   Eval      H1CogRag1   = CogRag1I74
123000000000     C                   Eval      D1Cognom1   = Cognom1I74
123100000000     C                   Eval      D1Denom1    = Denom1I74
123200000000     C                   Eval      D1Nome1     = Nome1I74
123300000000     C                   Eval      D1Indir2    = Indir2I74
123400000000     C                   Eval      D1NrCiv2    = NrCiv2I74
123500000000     C                   Eval      D1Cap2      = Cap2I74
123600000000     C                   Eval      D1Locali2   = Locali2I74
123700000000     C                   Eval      D1Prov2     = Prov2I74
123800000000     C                   Eval      D1Stato2    = Stato2I74
123900000000     C                   Eval      D1IdPaes2   = IdPaes2I74
124000000000     C*
124100000000     C                   Eval      D1Indir3    = Indir3I74
124200000000     C                   Eval      D1NrCiv3    = NrCiv3I74
124300000000     C                   Eval      D1Cap2      = Cap3I74
124400000000     C                   Eval      D1Locali3   = Locali3I74
124500000000     C                   Eval      D1Prov3     = Prov3I74
124600000000     C                   Eval      D1Stato3    = Stato3I74
124700000000     C                   Eval      D1IdPaes3   = IdPaes3I74
124800000000     C*
124900000000     C                   Eval      D1Stato4    = Stato4I74
125000000000     C                   Eval      D1IdPaes4   = IdPaes4I74
125100000000     C                   Eval      D1Piva4     = Piva4I74
125200000000     C                   Eval      D1Tpn4      = Tpn4I74
125300000000     C                   Eval      H1CogRag4   = CogRag4I74
125400000000     C                   Eval      D1Cognom4   = Cognom4I74
125500000000     C                   Eval      D1Denom4    = Denom4I74
125600000000     C                   Eval      D1Nome4     = Nome4I74
125700000000     C*
125800000000     C* decodifico le costanti
125900000000     C                   EXSR      F02D1
126000000000   E1C                   ENDIF
126100000000     C*
126200000000     C* imposto quegli attributi della videata che sono modificabili
126300000000     C* al richiamo del determinato pgm
126400000000     C                   EXSR      ATRD1
126500000000     C*
126600000000     C                   ENDSR
126700000000     C/EJECT
126800000000     C************************************************************
126900000000     C* Reperisco i dati per il Frontespizio
127000000000     C******************************************************
127100000000     C     RepFrontesp   BEGSR
127200000000     C*
127300000000     C* preparo i parametri di chiamata al driver
127400000000     C                   Clear                   XMPEDS
127500000000     C                   EVAL      TpChiamata = '01'
127600000000     C                   EVAL      NChiavi = 5
127700000000     C                   EVAL      VLogica = 2
127800000000     C*
127900170905R2654X***                Do        4             $Cont
128000170905R2654C                   Do        $DimSk        $Cont
128100000000     C                   Clear                   XMPEDS1
128200000000     C                   Eval      DSMPEPGM = 'SDGSI74R' +
128300000000     C                                        %char($cont)
128400000000     C* preparo le chiavi per la chiamata al driver
128500000000     C                   MoveL     XSCSOC        DSMPESOC
128600000000     C                   MoveL     *Blanks       DSMPEPRF
128700000000     C* Si reperiscono i dati di default a seconda delle modalità con
128800000000     C* cui il pgm è usato
128900000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
129000000000     C* MPETIP = 'PV'  pgm richiamato da menù
129100000000     C* MPETIP = 'PB'  x lancio Batch
129200000000     C                   MoveL     'PV'          DSMPETIP
129300000000     C*
129400000000     C                   CALLB     'XMPE'
129500000000     C                   PARM                    XMPEDS
129600000000     C                   PARM                    XMPEDS1
129700000000     C*
129800000000     C     Risultato     Ifeq      '0'
129900000000     C*
130000000000     C                   select
130100000000     C                   When      $Cont   = 1
130200000000     C                   MoveL     DSMPEPAR      DsFrontesp1
130300000000     C                   When      $Cont   = 2
130400000000     C                   MoveL     DSMPEPAR      DsFrontesp2
130500000000     C                   When      $Cont   = 3
130600000000     C                   MoveL     DSMPEPAR      DsFrontesp3
130700000000     C                   When      $Cont   = 4
130800000000     C                   MoveL     DSMPEPAR      DsFrontesp4
130900170905R2654C                   When      $Cont   = 5
131000170905R2654C                   MoveL     DSMPEPAR      DsFrontesp41
131100000000     C                   EndSl
131200000000     C*
131300000000     C                   Endif
131400000000     C*
131500000000     C                   Enddo
131600000000     C* Reperisco dalla funzione anagrafica i dati del dichiarante
131700000000     C     Risultato     Ifeq      '0'
131800000000     C                   Else
131900000000     C                   EXSR      RepFisc_Soc
132000000000     C                   endif
132100000000     C                   Exsr      Ried_20
132200000000     C*
132300000000     C                   Endsr
132400000000     C/EJECT
132500000000     C************************************************************
132600000000     C* RIEMPIMENTO VIDEATA  D1
132700000000     C************************************************************
132800000000     C     RIED_20       BEGSR
132900000000     C*
133000000000     C* impostare i campi della videata dai campi del file
133100000000     C*
133200000000     C*********          Eval      D1Partiva = WPIVA1
133300000000     C********           Eval      D1Cdfisc  = WCdFisc1
133400170911R266 C                   Eval      D1Cdfisc  = WCdFisc
133500170911 "   C                   if        %subst(DsFrontesp41:81:2) = *blank
133600170911 "   C                   Eval      D1Carica  = 00
133700170911 "   C                   else
133800170911 "   C                   Eval      D1Carica  = %dec(WCarica:2:0)
133900170911R266 C                   Endif
134000000000     C*
134100000000     C                   IF        WIDPaes1 <> *BLANKS
134200000000     C******             MOVEL     WIDPaes1      $Idpaese
134300000000     C******             Exsr      RepStaHd4
134400000000     C******             Eval      D1Stato1  = $Stato
134500000000     C                   Endif
134600000000     C*
134700000000     C                   Eval      D1Stato1  = WStato1
134800000000     C                   Eval      D1IdPaes1 = WIdPaes1
134900000000     C                   Eval      D1PIVA1   = WPIVA1
135000000000     C                   Eval      D1CdFisc1 = WCdFisc1
135100000000     C                   Eval      H1COGRAG1 = WCOGRAG1
135200000000     C                   Eval      D1DENOM1  = WDENOM1
135300000000     C                   Eval      D1NOME1   = WNOME1
135400000000     C                   Eval      D1COGNOM1 = WCOGNOM1
135500000000     C*
135600000000     C                   If        D1Denom1 <> *Blanks
135700170905R2656C*                  Eval      D1TPN1 = '2'
135800170905R2656C                   Eval      D1TPN1 = WTpN1
135900000000     C                   Else
136000000000     C                   If        D1Nome1 <> *BLANKS
136100000000     C                   Eval      D1TPN1 = '1'
136200000000     C                   EndIF
136300000000     C                   EndIF
136400000000     C*
136500000000     C                   Eval      D1INDIR2  = WINDIR2
136600000000     C                   Eval      D1NRCIV2  = WNRCIV2
136700000000     C                   Eval      D1CAP2    = WCAP2
136800000000     C                   Eval      D1LOCALI2 = WLOCALI2
136900000000     C                   Eval      D1PROV2   = WPROV2
137000000000     C*
137100000000     C                   IF        WIDPaes2 <> *BLANKS
137200000000     C******             MOVEL     WIDPaes2      $Idpaese
137300000000     C******             Exsr      RepStaHd4
137400000000     C******             Eval      D1Stato2  = $Stato
137500000000     C                   Endif
137600000000     C*
137700000000     C                   Eval      D1Stato2  = WStato2
137800000000     C                   Eval      D1IDPAES2 = WIDPAES2
137900000000     C*
138000000000     C                   Eval      D1INDIR3  = WINDIR3
138100000000     C                   Eval      D1NRCIV3  = WNRCIV3
138200000000     C                   Eval      D1CAP3    = WCAP3
138300000000     C                   Eval      D1LOCALI3 = WLOCALI3
138400000000     C                   Eval      D1PROV3   = WPROV3
138500000000     C*
138600000000     C                   IF        WIDPaes3 <> *BLANKS
138700000000     C******             MOVEL     WIDPaes3      $Idpaese
138800000000     C******             Exsr      RepStaHd4
138900000000     C******             Eval      D1Stato3  = $Stato
139000000000     C                   Endif
139100000000     C*
139200000000     C                   Eval      D1Stato3  = WStato3
139300000000     C                   Eval      D1IDPAES3 = WIDPAES3
139400000000     C*
139500000000     C                   IF        WIDPaes4 <> *BLANKS
139600000000     C******             MOVEL     WIDPaes4      $Idpaese
139700000000     C******             Exsr      RepStaHd4
139800000000     C******             Eval      D1Stato4  = $Stato
139900000000     C                   Endif
140000000000     C*
140100000000     C                   Eval      D1Stato4  = WStato4
140200000000     C                   Eval      D1IdPaes4 = WIDPaes4
140300000000     C                   Eval      D1PIVA4   = WPIVA4
140400000000     C                   Eval      H1COGRAG4 = WCOGRAG4
140500000000     C                   Eval      D1DENOM4  = WDENOM4
140600000000     C                   Eval      D1NOME4   = WNOME4
140700000000     C                   Eval      D1COGNOM4 = WCOGNOM4
140800000000     C*
140900000000     C                   If        D1Denom4 <> *Blanks
141000170908R2657X****               Eval      D1TPN4 = '2'
141100170908R2657C                   Eval      D1TPN4 = WTpN4
141200000000     C                   Else
141300000000     C                   If        D1Nome4 <> *BLANKS
141400000000     C                   Eval      D1TPN4 = '1'
141500000000     C                   Endif
141600000000     C                   EndIF
141700000000     C*
141800000000     C*
141900000000     C                   ENDSR
142000000000     C/EJECT
142100000000     C************************************************************
142200000000     C* Reperimesto dello stato da tabella Hd4
142300000000     C************************************************************
142400000000     C     RepstaHd4     BEGSR
142500000000     C*
142600000000     C* Lettura tabella HD4
142700000000     C                   Clear                   SDGTABDS
142800000000     C*
142900000000     C                   MOVE      'HD4'         XTBCOD
143000000000     C                   MOVE      XScSoc        XTBAZI
143100000000     C                   MOVE      XScLin        XTBLIN
143200000000     C                   MOVE      '0'           XTBALC
143300000000     C                   MOVE      XScCut        XTBUTE
143400000000     C                   MOVE      '0'           XTBERR
143500000000     C*
143600000000     C* SETLL + READE
143700000000     C                   MOVE      '2'           XTBRIC
143800000000     C                   CALL      'SDGTABDR'
143900000000     C                   PARM                    SDGTABDS
144000000000     C*
144100000000     C                   DoW       XTBErr      = '0'
144200000000     C*
144300000000     C                   if        XTBErr      = '0'
144400000000     C                   MoveL     XtbUni        SDGHD4DS
144500000000     C*
144600000000     C                   if        CodISO2HD4  = $IdPaese
144700000000     C                   Move(P)   XtbKey        $Stato
144800000000     C                   Leave
144900000000     C                   endif
145000000000     C*
145100000000     C                   endif
145200000000     C* READE
145300000000     C                   Move      '3'           XTBRIC
145400000000     C                   MOVE      'HD4'         XTBCOD
145500000000     C                   MOVE      '0'           XTBERR
145600000000     C                   CALL      'SDGTABDR'
145700000000     C                   PARM                    SDGTABDS
145800000000     C*
145900000000     C                   endDo
146000000000     C*
146100000000     C                   ENDSR
146200000000     C/EJECT
146300000000     C************************************************************
146400000000     C* RIEMPIMENTO VIDEATA  D1
146500000000     C************************************************************
146600000000     C     RIED1         BEGSR
146700000000     C*
146800000000     C* impostare i campi della videata dai campi del file
146900000000     C*
147000000000     C                   Eval      D1CF      = CSTCF
147100000000     C                   Eval      D1SOGG    = CSTSOGG
147200000000     C                   Eval      D1PARTIVA = CSTPARTIVA
147300000000     C                   Eval      D1CDFISC  = CSTCDFISC
147400000000     C                   Eval      D1STATO1  = CSTSTATO1
147500000000     C                   Eval      D1IDPAES1 = CSTIDPAES1
147600000000     C                   Eval      D1PIVA1   = CSTPIVA1
147700000000     C                   Eval      D1CDFISC1 = CSTCDFISC1
147800000000     C                   Eval      D1TPN1    = CSTTPN1
147900000000     C                   Eval      D1DENOM1  = CSTDENOM1
148000000000     C                   Eval      D1COGNOM1 = CSTCOGNOM1
148100000000     C                   Eval      D1NOME1   = CSTNOME1
148200000000     C                   Eval      D1INDIR2  = CSTINDIR2
148300000000     C                   Eval      D1NRCIV2  = CSTNRCIV2
148400000000     C                   Eval      D1CAP2    = CSTCAP2
148500000000     C                   Eval      D1LOCALI2 = CSTLOCALI2
148600000000     C                   Eval      D1PROV2   = CSTPROV2
148700000000     C                   Eval      D1STATO2  = CSTSTATO2
148800000000     C                   Eval      D1IDPAES2 = CSTIDPAES2
148900000000     C                   Eval      D1INDIR3  = CSTINDIR3
149000000000     C                   Eval      D1NRCIV3  = CSTNRCIV3
149100000000     C                   Eval      D1CAP3    = CSTCAP3
149200000000     C                   Eval      D1LOCALI3 = CSTLOCALI3
149300000000     C                   Eval      D1PROV3   = CSTPROV3
149400000000     C                   Eval      D1STATO3  = CSTSTATO3
149500000000     C                   Eval      D1IDPAES3 = CSTIDPAES3
149600000000     C                   Eval      D1STATO4  = CSTSTATO4
149700000000     C                   Eval      D1IDPAES4 = CSTIDPAES4
149800000000     C                   Eval      D1PIVA4   = CSTPIVA4
149900000000     C                   Eval      D1TPN4    = CSTTPN4
150000000000     C                   Eval      D1DENOM4  = CSTDENOM4
150100000000     C                   Eval      D1NOME4   = CSTNOME4
150200000000     C                   Eval      D1COGNOM4 = CSTCOGNOM4
150300000000     C*
150400000000     C                   ENDSR
150500000000     C************************************************************
150600000000     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
150700000000     C************************************************************
150800000000     C     ATRD1         BEGSR
150900000000     C*
151000000000    >C* in questa routine bisogna attivare indicatori o campi programma
151100000000    >C* che gestiscono gli attributi di uno più campi di questa videata;
151200000000    >C* attenzione : non è questo il posto in cui eseguire controlli,
151300000000    >C*  dovrebbero già arrivare dei flag
151400000000     C*
151500000000     C                   SETOFF                                       02
151600000000     C*
151700000000     C* in caso di manutenzione
151800000000    >C     OPZI74        IFEQ      '02'
151900000000     C* proteggo i campi chiave
152000000000     C                   SETON                                        02        PR campi key
152100000000   E3C                   ENDIF
152200000000     C*
152300000000     C* in caso di insertimento o copia
152400000000     C* abilito i campi D1SOGG e D1Partiva e D1Cdfisc
152500000000    >C     OPZI74        IFEQ      '01'
152600000000    >C     OPZI74        orEQ      '03'
152700000000     C                   SETON                                        11        PR campi key
152800000000   E3C                   ENDIF
152900000000     C*
153000000000     C*
153100000000     C* in caso di chiamata dati società disabilito D1CF
153200000000     C*
153300000000    >C     OPZI74        IFEQ      '20'
153400000000     C                   SETON                                        12        PR campi key
153500000000   E3C                   ENDIF
153600000000     C                   ENDSR
153700000000     C/EJECT
153800000000     C************************************************************
153900000000     C* RIEMPIMENTO CAMPI DAI DEFAULT
154000000000     C************************************************************
154100000000     C     RIEDEF        BEGSR
154200000000     C*
154300000000     C*
154400000000     C                   ENDSR
154500000000     C/EJECT
154600000000     C************************************************************
154700000000     C* GESTIONE F02 VIDEO D1
154800000000     C************************************************************
154900000000     C     F02D1         BEGSR
155000000000     C*
155100000000     C* per decodificare la videata richiamo la routine dei ctrl
155200000000     C* e poi disattivo gli eventuali messaggi di errore
155300000000     C                   EXSR      CTRD1
155400000000     C*
155500000000     C                   MOVE      *ALL'0'       IN5098
155600000000     C                   MOVEA     IN5098        *IN(50)
155700000000     C*
155800000000     C                   ENDSR
155900000000     C/EJECT
156000000000     C************************************************************
156100000000     C* GESTIONE F03 VIDEO D1
156200000000     C************************************************************
156300000000     C     F03D1         BEGSR
156400000000     C*
156500000000     C                   MOVE      *ON           $FINE
156600000000    >C                   MOVE      '1'           RETI74
156700000000     C* fine programma
156800000000     C                   EXSR      ENDPGM
156900000000     C*
157000000000     C                   ENDSR
157100000000     C/EJECT
157200000000     C************************************************************
157300000000     C* GESTIONE F05 VIDEO D1
157400000000     C************************************************************
157500000000     C     F05D1         BEGSR
157600000000     C*
157700000000     C                   MOVE      *ON           $INZD1
157800000000     C*
157900000000     C                   ENDSR
158000000000     C/EJECT
158100000000     C************************************************************
158200000000     C* GESTIONE F08 VIDEO D1
158300000000     C************************************************************
158400000000     C     F08D1         BEGSR
158500000000     C*
158600000000     C                   MOVE      *ON           $FINE
158700000000    >C                   MOVE      '0'           RETI74
158800000000     C*
158900000000     C                   ENDSR
159000000000     C/EJECT
159100000000     C************************************************************
159200000000     C* GESTIONE F12 VIDEO D1
159300000000     C************************************************************
159400000000     C     F12D1         BEGSR
159500000000     C*
159600000000     C                   MOVE      *ON           $FINE
159700000000    >C                   MOVE      '2'           RETI74
159800000000     C*
159900000000     C                   ENDSR
160000000000     C/EJECT
160100000000     C************************************************************
160200000000     C* GESTIONE F24 VIDEO D1
160300000000     C************************************************************
160400000000     C     F24D1         BEGSR
160500000000     C*
160600000000     C                   MOVEA     $FX1          $FX
160700000000     C                   MOVEA     $FC1          $FC
160800000000     C                   MOVEA     $FR1          $FR
160900000000     C                   Z-ADD     $ULKD1        $ULK
161000000000     C                   Z-ADD     $D1L01        $L01
161100000000     C                   Z-ADD     $D1L02        $L02
161200000000     C                   EXSR      RIEKEY
161300000000     C                   Z-ADD     $ULK          $ULKD1            3 0
161400000000     C                   MOVEA     $FC           $FC1
161500000000     C                   MOVEL     $KEY1         Z1KE1
161600000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
161700000000     C                   MOVEL     $KEY2         Z1KE2
161800000000     C                   WRITE     FMTZ1
161900000000     C*
162000000000     C                   ENDSR
162100000000     C/EJECT
162200000000     C************************************************************
162300000000     C* CONTROLLO VIDEATA
162400000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
162500000000     C* 95 96 97 98
162600000000     C************************************************************
162700000000     C     CTRD1         BEGSR
162800000000     C*
162900000000     C                   SETOFF                                       99
163000000000     C                   Move      *Off          $PrimaVolta       1
163100000000     C*
163200000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
163300000000     C*
163400000000     C     OPZI74        IFNE      '04'
163500000000     C*
163600000000     C                   If        OPZI74      = '01'   or
163700000000     C                             OPZI74      = '03'
163800000000     C                   EXSR      CtrSogg
163900000000     C                   Endif
164000000000     C*
164100000000     C                   If        OPZI74     <> '20'
164200000000     C                   Exsr      CtrCF
164300000000     C                   Endif
164400000000     C*
164500000000     C* Salvo i campi che hanno controlli comuni
164600000000     C                   Movel     D1Stato1      $Stato
164700000000     C                   Movel     D1Tpn1        $Tpn
164800000000     C                   Movel     D1Denom1      $Denom
164900000000     C                   Movel     D1Nome1       $Nome
165000000000     C                   Movel     D1Cognom1     $Cognom
165100000000     C                   If        D1Denom1<> *blanks
165200000000     C                   Movel     D1Denom1      H1CogRag1
165300000000     C                   Else
165400000000     C                   Movel     D1Cognom1     H1CogRag1
165500000000     C                   Endif
165600000000     C                   Movel     D1Cognom1     $Cognom
165700000000     C*
165800000000     C* Tipo Natura giuridica
165900000000     C                   Movel     *On           $CtrObbl
166000000000     C                   EXSR      CtrTPN
166100000000     C                   Movel     $Tpnd         D1Tpn1d
166200000000     C                   If        $ErrCtrl = *on
166300000000     C                   Seton                                        5199
166400000000     C                   Endif
166500000000     C* Stato
166600000000     C                   if        D1IdPaes1   = *Blank   and
166700000000     C                             D1Stato1    = *Blank   and
166800000000     C                             OPZI74      = '20'
166900000000     C                   Eval      D1IdPaes1   = 'IT'
167000000000     C                   Eval      $IDPaese    = 'IT'
167100000000     C                   Exsr      RepStaHd4
167200000000     C                   Eval      D1Stato1  = $Stato
167300000000     C                   endif
167400000000     C***                EXSR      CtrStato1
167500000000     C                   EXSR      CtrStato
167600000000     C*
167700000000     C                   If        $ErrCtrl = *on
167800000000fm3  C                             Or
167900000000fm3  C                             D1Stato1 = *Blank
168000000000     C*
168100000000     C* Solo per il 2017 se lo stato è *blanks imposto lo stato iso = 'OO'
168200000000     C* e non emetto l'errore.
168300000000fm3  X*******            If        D1Stato1 =  'OO'    and
168400000000fm3  C                   If        D1Stato1 =  *Blank  and
168500000000     C                             D1Anno = 2017 and D1CF = 'F'
168600000000     C                   Movel     'OO'          $IDpaese
168700000000     C                   eval      $StatoD   = 'Valido per dogana 2017'
168800000000     C                   Else
168900000000     C*
169000000000     C                   If        $TipErr  = '1'
169100000000     C                   Seton                                        5299
169200000000     C                   Endif
169300000000     C*
169400000000     C                   If        $TipErr  = '2'
169500000000     C                   Seton                                        7299
169600000000     C                   Endif
169700000000     C*
169800000000     C                   Endif
169900000000     C                   Endif
170000000000     C*
170100000000     C                   Movel     $Statod       D1Stato1D
170200000000     C                   Movel     $IdPaese      D1IdPaes1
170300000000     C*
170400000000     C*
170500000000     C                   EXSR      CtrPIVA_CDF
170600000000     C* Denominazione
170700000000     C                   EXSR      CtrDenom
170800000000     C                   If        $ErrCtrl = *on
170900000000     C                   If        $TipErr  = '1'
171000000000     C                   Seton                                        5599
171100000000     C                   Else
171200000000     C                   Seton                                        7599
171300000000     C                   Endif
171400000000     C                   Endif
171500000000     C
171600000000     C* Cognome
171700000000     C                   EXSR      CtrCognome
171800000000     C                   If        $ErrCtrl = *on
171900000000     C                   If        $TipErr  = '1'
172000000000     C                   Seton                                        5699
172100000000     C                   Else
172200000000     C                   Seton                                        7699
172300000000     C                   Endif
172400000000     C                   Endif
172500000000     C* Nome
172600000000     C                   EXSR      CtrNome
172700000000     C                   If        $ErrCtrl = *on
172800000000     C                   If        $TipErr  = '1'
172900000000     C                   Seton                                        5799
173000000000     C                   Else
173100000000     C                   Seton                                        7799
173200000000     C                   Endif
173300000000     C                   Endif
173400000000     C*
173500000000     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
173600000000     C     OPZI74        IFEQ      '01'
173700000000    >C     OPZI74        OREQ      '03'
173800000000     C*
173900000000     C* imposto i campi chiave e verifico l'esistenza del record
174000000000     C*
174100000000     C                   Eval      CSTSocieta   = XScSoc
174200000000     C                   Eval      CSTTpCom     = TpcomI74
174300000000     C                   Eval      CSTTpRec     = TprecI74
174400000000     C                   Eval      CSTAnno      = AnnoI74
174500000000     C                   Eval      CSTTrim      = TrimI74
174600000000     C                   Eval      CSTMese      = MeseI74
174700000000     C*
174800000000     C                   Eval      CSTPartIVA   = D1Partiva
174900000000     C                   Eval      CSTCdFisc    = D1CdFisc
175000000000     C*
175100000000     C                   Eval      CSTStato1 = D1Stato1
175200000000     C                   Eval      CSTIdPaes1= D1IdPaes1
175300000000     C*
175400000000     C                   Movel     DsCliForV     DsCliFor
175500000000     C                   Movel     DsCliFor      CSTCliFor
175600000000     C     K07CST02      Setll     sdgcst02l                              21
175700000000     C* se lo trovo significa che esiste già
175800000000     C     *IN21         IFEQ      '1'
175900000000     C                   SETON                                        9099
176000000000     C                   ENDIF
176100000000     C*
176200000000     C                   ENDIF
176300000000     C*
176400000000     C                   ENDIF
176500000000     C*
176600000000     C                   ENDSR
176700000000     C/EJECT
176800000000     C***********************************************************
176900000000     C* Controllo Stato1
177000000000     C***********************************************************
177100000000     C     CtrStato1     BEGSR
177200000000     C*
177300000000     C                   Clear                   $ErrCtrl
177400000000     C                   Clear                   $TipErr
177500000000     C*
177600000000     C                   Clear                   $StatoD
177700000000     C                   Clear                   $IdPaese
177800000000     C*
177900000000     C                   Clear                   AngG12DS
178000000000     C*
178100000000     C                   Clear                   SDGTABDS
178200000000     C*
178300000000     C                   Eval      XTbRic      = '1'
178400000000     C                   Eval      XTbAzi      = XScSoc
178500000000     C                   Eval      XTbKey      = *Zero
178600000000     C                   Eval      XTbCod      = 'G12'
178700000000     C                   Move      $Stato        XTbKey
178800000000     C*
178900000000     C                   CALL      'SDGTABDR'
179000000000     C                   PARM                    SDGTABDS
179100000000     C*
179200000000     C                   if        XTbErr     <> '0'
179300000000     C*                  Eval      $Errctrl   = *on
179400000000     C*                  Eval      $TipErr    = '2'
179500000000     C                   endif
179600000000     C                   MoveL     XTbUni        AngG12DS
179700000000     C                   MoveL     DesG12        $StatoD
179800000000     C
179900000000     C* Reperisco il codice Iso da tabelle HD4
180000000000     C                   Clear                   SDGHD4DS
180100000000     C                   Clear                   SDGTABDS
180200000000     C                   Eval      XTbRic      = '1'
180300000000     C                   Eval      XTbAzi      = XScSoc
180400000000     C                   Eval      XTbKey      = *Zero
180500000000     C                   Eval      XTbCod      = 'HD4'
180600000000     C                   Move      $Stato        XTbKey
180700000000     C*
180800000000     C                   CALL      'SDGTABDR'
180900000000     C                   PARM                    SDGTABDS
181000000000     C                   if        XTbErr     <> '0'
181100000000     C                   Eval      $Errctrl   = *on
181200000000     C                   Eval      $TipErr    = '2'
181300000000     C                   else
181400000000     C                   MoveL     XTbUni        SDGHD4DS
181500000000     C                   MoveL     Codiso2Hd4    $IdPaese
181600000000     C                   MoveL     DesHD4        $StatoD
181700000000     C                   Endif
181800000000     C*
181900000000     C**                 Endif
182000000000     C*
182100000000     C                   ENDSR
182200000000     C/EJECT
182300000000     C***********************************************************
182400000000     C* Controllo codice soggetto
182500000000     C***********************************************************
182600000000     C     CtrSogg       BEGSR
182700000000     C*
182800000000     C                   Clear                   D1SoggD
182900000000     C*
183000000000     C     D1SOGG        IFNE      *BLANK
183100000000     C                   CALLb     'XALLINEA'
183200000000     C                   PARM                    D1SOGG
183300000000     C                   PARM      8             LEN               5 0
183400000000     C                   PARM                    XSCINP
183500000000     C                   ENDIF
183600000000     C*
183700000000     C                   MoveL     D1Sogg        WSogg
183800000000     C                   if        D1Sogg     <> *Blank
183900000000     C                   EXSR      CallDvaSog
184000000000     C*
184100000000     C                   if        DVAErrore  <> '1'
184200000000     C*
184300000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
184400000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
184500000000     C*
184600000000     C                   MoveL     DVSDes        D1SoggD
184700000000     C* Recupero i dati anagrafici solo se non è stato inserito alcun dato
184800000000     C                   If        D1Partiva  =  *blanks and
184900000000     C                             D1CdFisc   =  *blanks and
185000000000     C                             D1Stato1   =  *blanks and
185100000000     C                             D1IdPaes1  =  *blanks and
185200000000     C                             D1PIVA1    =  *blanks and
185300000000     C                             D1CdFisc   =  *blanks and
185400000000     C*
185500000000     C                             H1CogRag1  =  *blanks and
185600000000     C                             D1TpN1     =  *blanks and
185700000000     C                             D1Denom1   =  *blanks and
185800000000     C                             D1Cognom1  =  *blanks and
185900000000     C                             D1Nome1    =  *blanks and
186000000000     C*
186100000000     C                             D1Indir2   =  *blanks and
186200000000     C                             D1NrCiv2   =  *blanks and
186300000000     C                             D1CAP2     =  *blanks and
186400000000     C                             D1Locali2  =  *blanks and
186500000000     C                             D1Prov2    =  *blanks and
186600000000     C                             D1Stato2   =  *blanks and
186700000000     C                             D1IdPaes2  =  *blanks and
186800000000     C* STABILE ORGANIZZAZIONE
186900000000     C                             D1Indir3   =  *blanks and
187000000000     C                             D1NrCiv3   =  *blanks and
187100000000     C                             D1CAP3     =  *blanks and
187200000000     C                             D1Locali3  =  *blanks and
187300000000     C                             D1Prov3    =  *blanks and
187400000000     C                             D1Stato3   =  *blanks and
187500000000     C                             D1IdPaes3  =  *blanks and
187600000000     C*
187700000000     C* RAPPRESENTANTE FISCALE
187800000000     C                             D1Stato4   =  *blanks and
187900000000     C                             D1IdPaes4  =  *blanks and
188000000000     C                             D1PIVA4    =  *blanks and
188100000000     C*
188200000000     C                             H1CogRag4  =  *blanks and
188300000000     C                             D1TpN4     =  *blanks and
188400000000     C                             D1Denom4   =  *blanks and
188500000000     C                             D1Cognom4  =  *blanks and
188600000000     C                             D1Nome4    =  *blanks
188700000000     C*
188800000000     C                   Exsr      RepFisc
188900000000     C                   Eval      $PrimaVolta = *On
189000000000     C*
189100000000     C                   Endif
189200000000     C*
189300000000     C                   else
189400000000     C*
189500000000     C                   Eval      *In16       = *On
189600000000     C*
189700000000     C                   endif
189800000000     C*
189900000000     C                   endif
190000000000     C*
190100000000     C                   ENDSR
190200000000     C/EJECT
190300000000     C************************************************************
190400000000     C* Chiamata al driver dei soggetti
190500000000     C************************************************************
190600000000     C     CallDvaSog    BEGSR
190700000000     C*
190800000000     C                   EVAL      DVALenOut = %Size(DVASOG)
190900000000     C*
191000000000     C                   CALLB     'NDDVASOG'
191100000000     C                   PARM      '1'           DVARIC
191200000000     C                   PARM      XScSoc        DVASOCIETA
191300000000     C                   PARM                    DVAUNITA
191400000000     C                   PARM      'DVASOG    '  DVASTRUTT
191500000000     C                   PARM                    DVADTRIF
191600000000     C                   PARM                    DVALENOUT
191700000000     C* Se si accede con il codice cli/for impostare i campi seguenti
191800000000     C* se si accede con il soggetto ometterli e impostare gli altri
191900000000     C*****              PARM                    DVASNATURA
192000000000     C*****              PARM                    DVACODICE
192100000000     C*****              PARM                    DVALINEAV
192200000000     C*****              PARM                    DVAFILIALE
192300000000     C                   PARM                    *omit
192400000000     C                   PARM                    *omit
192500000000     C                   PARM                    *omit
192600000000     C                   PARM                    *omit
192700000000     C                   PARM      WSogg         DVSSOGG
192800000000     C                   PARM      *Blank        DVSTPIND
192900000000     C                   PARM      *Blank        DVSCDIND
193000000000     C                   PARM                    DVAERRORE
193100000000     C                   PARM                    DVAOUTPUT
193200000000     C*
193300000000     C                   ENDSR
193400000000     C/EJECT
193500000000     C***********************************************************
193600000000     C* Recupero  i dati anagrafici se immesso il soggetto
193700000000     C***********************************************************
193800000000     C     RepFisc       BEGSR
193900000000     C*
194000000000     C                   Clear                   SAV00701DS
194100000000     C* Dati del soggetto
194200000000     C                   Clear                   SDG00701DS
194300000000     C*
194400000000     C                   Eval      Opz0701     = '01'
194500000000     C                   Eval      Err0701     = '00'
194600000000     C                   Eval      Soc0701     = SocietaI74
194700000000     C                   Eval      TpCom0701   = TpComI74
194800000000     C                   Eval      Anno0701    = AnnoI74
194900000000     C                   Eval      Trim0701    = TrimI74
195000000000     C                   Eval      Mese0701    = MeseI74
195100000000     C                   Eval      Sogg0701    = D1Sogg
195200000000     C                   Eval      CF0701      = D1Cf
195300000000     C                   Eval      Tecnic0701  = *Off
195400000000     C                   Eval      TpN1Sg0701  = *Blank
195500000000     C                   Eval      DtRif0701   = *Loval
195600000000     C                   Eval      NrAsRe0701  = 0
195700000000     C*
195800000000     C* salvo la kpjbu per ripristinarla dopo il richiamo
195900000000     C                   MoveL     KPJBU         SavKPJBU
196000000000     C*
196100000000     C                   CALLB     'SDG00701R'
196200000000     C                   PARM                    KPJBA
196300000000     C                   PARM                    SDG00701DS
196400000000     C* Salvo i dati reperiti
196500000000     C                   MoveL     SDG00701ds    SAV00701ds
196600000000     C*
196700000000     C* Dati storicizzati (se presenti sostituisco)
196800000000     C                   Clear                   SDG00701DS
196900000000     C*
197000000000     C                   Exsr      RepPeriodo
197100000000     C*
197200000000     C                   Move      $Data10A2     $DtRif
197300000000     C*
197400000000     C                   Eval      Opz0701     = '02'
197500000000     C                   Eval      Err0701     = '00'
197600000000     C                   Eval      Soc0701     = SocietaI74
197700000000     C                   Eval      TpCom0701   = TpComI74
197800000000     C                   Eval      Anno0701    = AnnoI74
197900000000     C                   Eval      Trim0701    = TrimI74
198000000000     C                   Eval      Mese0701    = MeseI74
198100000000     C                   Eval      Sogg0701    = D1Sogg
198200000000     C                   Eval      CF0701      = D1Cf
198300000000     C                   Eval      Tecnic0701  = *Off
198400000000     C                   Eval      TpN1Sg0701  = *Blank
198500000000     C                   Eval      DtRif0701   = $DtRif
198600000000     C                   Eval      NrAsRe0701  = 0
198700000000     C*
198800000000     C* salvo la kpjbu per ripristinarla dopo il richiamo
198900000000     C                   MoveL     KPJBU         SavKPJBU
199000000000     C*
199100000000     C                   CALLB     'SDG00701R'
199200000000     C                   PARM                    KPJBA
199300000000     C                   PARM                    SDG00701DS
199400000000     C* se non trovati sostituisco con quelli del soggetto
199500000000     C                   if        Found0701  <> *On
199600000000     C                   MoveL     SAV00701ds    SDG00701ds
199700000000     C                   endif
199800000000     C* ripristino KPJBU
199900000000     C                   MoveL     SavKPJBU      KPJBU
200000000000     C*
200100000000     C* DATI FISCALI E SEDE
200200000000     C                   MoveL(P)  PartIV0701    D1Partiva
200300000000     C                   MoveL(P)  CdFisc0701    D1CdFisc
200400000000     C                   MoveL(P)  Stato10701    D1Stato1
200500000000     C                   MoveL(P)  IdPae10701    D1IdPaes1
200600000000     C                   MoveL(P)  PIVA10701     D1PIVA1
200700000000     C                   MoveL(P)  CdFis10701    D1CdFisc1
200800000000     C*
200900000000     C                   MoveL(P)  CogRa10701    H1CogRag1
201000000000     C                   MoveL(P)  TpN10701      D1TpN1
201100000000     C                   MoveL(P)  Denom10701    D1Denom1
201200000000     C                   MoveL(P)  Cogno10701    D1Cognom1
201300000000     C                   MoveL(P)  Nome10701     D1Nome1
201400000000     C*
201500000000     C                   MoveL(P)  Indir20701    D1Indir2
201600000000     C                   MoveL(P)  NrCiv20701    D1NrCiv2
201700000000     C                   MoveL(P)  CAP20701      D1CAP2
201800000000     C                   MoveL(P)  Local20701    D1Locali2
201900000000     C                   MoveL(P)  Prov20701     D1Prov2
202000000000     C                   MoveL(P)  Stato20701    D1Stato2
202100000000     C                   MoveL(P)  IdPae20701    D1IdPaes2
202200000000     C*
202300000000     C* STABILE ORGANIZZAZIONE
202400000000     C                   MoveL(P)  Indir30701    D1Indir3
202500000000     C                   MoveL(P)  NrCiv30701    D1NrCiv3
202600000000     C                   MoveL(P)  CAP30701      D1CAP3
202700000000     C                   MoveL(P)  Local30701    D1Locali3
202800000000     C                   MoveL(P)  Prov30701     D1Prov3
202900000000     C                   MoveL(P)  Stato30701    D1Stato3
203000000000     C                   MoveL(P)  IdPae30701    D1IdPaes3
203100000000     C*
203200000000     C* RAPPRESENTANTE FISCALE
203300000000     C                   MoveL(P)  Stato40701    D1Stato4
203400000000     C                   MoveL(P)  IdPae40701    D1IdPaes4
203500000000     C                   MoveL(P)  PIVA40701     D1PIVA4
203600000000     C*
203700000000     C                   MoveL(P)  CogRa40701    H1CogRag4
203800000000     C                   MoveL(P)  TpN40701      D1TpN4
203900000000     C                   MoveL(P)  Denom40701    D1Denom4
204000000000     C                   MoveL(P)  Cogno40701    D1Cognom4
204100000000     C                   MoveL(P)  Nome40701     D1Nome4
204200000000     C*
204300000000     C                   ENDSR
204400000000     C*
204500000000     C/EJECT
204600000000     C***********************************************************
204700000000     C* Recupero  i dati anagrafici della società
204800000000     C***********************************************************
204900000000     C     RepFisc_soc   BEGSR
205000000000     C*
205100000000     C* Dati del soggetto
205200000000     C                   Clear                   SDG00701DS
205300000000     C*
205400000000     C                   Eval      Opz0701     = '03'
205500000000     C                   Eval      Err0701     = '00'
205600000000     C                   Eval      Soc0701     = SocietaI74
205700000000     C                   Eval      TpCom0701   = TpComI74
205800000000     C                   Eval      Anno0701    = AnnoI74
205900000000     C                   Eval      Trim0701    = TrimI74
206000000000     C                   Eval      Mese0701    = MeseI74
206100000000     C                   Eval      Tecnic0701  = *Off
206200000000     C                   Eval      TpN1Sg0701  = *Blank
206300000000     C                   Eval      DtRif0701   = *Loval
206400000000     C                   Eval      NrAsRe0701  = 0
206500000000     C*
206600000000     C* salvo la kpjbu per ripristinarla dopo il richiamo
206700000000     C                   MoveL     KPJBU         SavKPJBU
206800000000     C*
206900000000     C                   CALLB     'SDG00701R'
207000000000     C                   PARM                    KPJBA
207100000000     C                   PARM                    SDG00701DS
207200000000     C*
207300000000     C                   Movel     savkpjbu      Kpjbu
207400000000     C*
207500000000     C* se non trovati sostituisco con quelli del soggetto
207600000000     C                   if        Found0701  =  *On
207700000000     C*
207800000000     C* DATI FISCALI E SEDE
207900000000     C******             MoveL(P)  PartIV0701    D1Partiva
208000000000     C******             MoveL(P)  CdFisc0701    D1CdFisc
208100000000     C                   MoveL(P)  Stato10701    WStato1
208200000000     C                   MoveL(P)  IdPae10701    WIdPaes1
208300000000     C                   if        Stato10701  = *Blank   and
208400000000     C                             IdPae10701  = 'IT'
208500000000     C                   Eval      $IDPaese    = 'IT'
208600000000     C                   Exsr      RepStaHd4
208700000000     C                   Eval      WStato1     = $Stato
208800000000     C                   endif
208900000000     C                   MoveL(P)  PIVA10701     WPIVA1
209000000000     C                   MoveL(P)  CdFisc0701    WCdFisc1
209100000000     C*
209200000000     C                   MoveL(P)  CogRa10701    WCogRag1
209300000000     C                   MoveL(P)  TpN10701      WTpN1
209400000000     C                   MoveL(P)  Denom10701    WDenom1
209500000000     C                   MoveL(P)  Cogno10701    WCognom1
209600000000     C                   MoveL(P)  Nome10701     WNome1
209700000000     C*
209800000000     C                   MoveL(P)  Indir20701    WIndir2
209900000000     C                   MoveL(P)  NrCiv20701    WNrCiv2
210000000000     C                   MoveL(P)  CAP20701      WCAP2
210100000000     C                   MoveL(P)  Local20701    WLocali2
210200000000     C                   MoveL(P)  Prov20701     WProv2
210300000000     C                   MoveL(P)  Stato20701    WStato2
210400000000     C                   MoveL(P)  IdPae20701    WIdPaes2
210500000000     C*
210600000000     C* STABILE ORGANIZZAZIONE
210700000000     C                   MoveL(P)  Indir30701    WIndir3
210800000000     C                   MoveL(P)  NrCiv30701    WNrCiv3
210900000000     C                   MoveL(P)  CAP30701      WCAP3
211000000000     C                   MoveL(P)  Local30701    WLocali3
211100000000     C                   MoveL(P)  Prov30701     WProv3
211200000000     C                   MoveL(P)  Stato30701    WStato3
211300000000     C                   MoveL(P)  IdPae30701    WIdPaes3
211400000000     C*
211500000000     C* RAPPRESENTANTE FISCALE
211600000000     C                   MoveL(P)  Stato40701    WStato4
211700000000     C                   MoveL(P)  IdPae40701    WIdPaes4
211800000000     C                   MoveL(P)  PIVA40701     WPIVA4
211900000000     C*
212000000000     C                   MoveL(P)  CogRa40701    WCogRag4
212100000000     C                   MoveL(P)  TpN40701      WTpN4
212200000000     C                   MoveL(P)  Denom40701    WDenom4
212300000000     C                   MoveL(P)  Cogno40701    WCognom4
212400000000     C                   MoveL(P)  Nome40701     WNome4
212500000000     C                   Endif
212600000000     C*
212700000000     C                   ENDSR
212800000000     C*
212900000000     C******************************************************
213000000000     C* Reperisco le date di inizio e fine periodo
213100000000     C******************************************************
213200000000     C     RepPeriodo    BEGSR
213300000000     C*
213400000000     C                   Reset                   DataDs
213500000000     C                   Clear                   $Data10A1
213600000000     C                   Clear                   $MeseI            2 0
213700000000     C                   Clear                   $MeseF            2 0
213800000000     C*
213900000000     C* recupero il mese di inizio trimestre e
214000000000     C*          il mese di fine   trimestre
214100000000     C                   Select
214200000000     C*
214300000000     C                   When      TrimI74     = 1
214400000000     C                   Eval      $MeseI      = 1
214500000000     C                   Eval      $MeseF      = 3
214600000000     C*
214700000000     C                   When      TrimI74     = 2
214800000000     C                   Eval      $MeseI      = 4
214900000000     C                   Eval      $MeseF      = 6
215000000000     C*
215100000000     C                   When      TrimI74     = 3
215200000000     C                   Eval      $MeseI      = 7
215300000000     C                   Eval      $MeseF      = 9
215400000000     C*
215500000000     C                   When      TrimI74     = 4
215600000000     C                   Eval      $MeseI      = 10
215700000000     C                   Eval      $MeseF      = 12
215800000000     C*
215900000000     C                   endSl
216000000000     C*
216100000000     C* Periodo Da
216200000000     C                   MoveL     AnnoI74       AnnoDs
216300000000     C                   MoveL     $MeseI        MeseDs
216400000000     C                   MoveL     '01'          GiornoDs
216500000000     C                   Move      DataDs        $Data10A1
216600000000     C*
216700000000     C* Periodo A
216800000000     C                   Clear                   WData
216900000000     C                   Reset                   DataDs
217000000000     C                   Clear                   $Data10A2
217100000000     C*
217200000000     C                   MoveL     AnnoI74       AnnoDs
217300000000     C                   MoveL     $MeseF        MeseDs
217400000000     C                   MoveL     '01'          GiornoDs
217500000000     C                   Move      DataDs        WData
217600000000     C*
217700000000     C* reperisco il fine mese
217800000000     C                   CALL      'XDATCD'
217900000000     C                   PARM      4             XCod              2 0
218000000000     C                   PARM                    WData
218100000000     C*
218200000000     C                   Move      WData         $Data10A2
218300000000     C*
218400000000     C                   ENDSR
218500000000     C/EJECT
218600000000     C/EJECT
218700000000     C***********************************************************
218800000000     C* Controllo cliente / fornitore
218900000000     C***********************************************************
219000000000     C     CtrCf         BEGSR
219100000000     C*
219200000000     C                   Clear                   D1Cfd
219300000000     C                   Clear                   X07ValDs
219400000000     C*
219500000000     C**                 If        D1Cf <> *blanks
219600000000     C                   Move      '1'           X07Ric
219700000000     C                   Move      *On           X07All
219800000000     C                   Movel     'SDGCST000'   X07TRc
219900000000     C                   Movel     'CSTCF    '   X07Cam
220000000000     C                   Movel     D1CF          X07VAL
220100000000     C*
220200000000     C                   Callb     'X07VALR'
220300000000     C                   Parm                    X07ValDs
220400000000     C*
220500000000     C     X07ERR        IFEQ      *Off
220600000000     C                   Movel     X07des        D1CfD
220700000000     C                   else
220800000000     C                   SetOn                                        5099
220900000000     C                   endif
221000000000     C**                 endif
221100000000     C*
221200000000     C                   Endsr
221300000000     C/EJECT
221400000000     C***********************************************************
221500000000     C* Controllo la tipologia della sottonatura
221600000000     C***********************************************************
221700000000     C     CtrTpN        BEGSR
221800000000     C*
221900000000     C                   Clear                   $Errctrl
222000000000     C                   Clear                   $TpnD
222100000000     C                   Clear                   X07ValDs
222200000000     C*
222300000000     C                   if        $Tpn       =  *Blank and
222400000000     C                             $CtrObbl = *on
222500000000     C                   Movel     *on           $ErrCtrl
222600000000     C                   Endif
222700000000     C*
222800000000     C                   if        $Tpn       =  *Blank and
222900000000     C                             ($Denom <>*blanks  or
223000000000     C                             $Nome  <>*blanks  or
223100000000     C                             $Cognom<>*blanks)
223200000000     C                   Movel     *on           $ErrCtrl
223300000000     C                   Endif
223400000000     C*
223500000000     C                   if        $Tpn       <> *Blank
223600000000     C                   Move      '1'           X07Ric
223700000000     C                   Move      *Off          X07All
223800000000     C                   MoveL     'SDGCST000'   X07TRc
223900000000     C                   MoveL     'CSTTPN1  '   X07Cam
224000000000     C                   MoveL     $Tpn          X07VAL
224100000000     C*
224200000000     C                   CALLB     'X07VALR'
224300000000     C                   PARM                    X07ValDs
224400000000     C*
224500000000     C     X07ERR        IFEQ      *Off
224600000000     C                   MoveL     X07DES        $Tpnd
224700000000     C                   else
224800000000     C                   Movel     *on           $ErrCtrl
224900000000     C                   endif
225000000000     C                   endif
225100000000     C*
225200000000     C                   ENDSR
225300000000     C/EJECT
225400000000     C***********************************************************
225500000000     C* Controllo ragione sociale (se Tpn <>'1' obbligatorio)
225600000000     C***********************************************************
225700000000     C     CtrDenom      BEGSR
225800000000     C*
225900000000     C                   Clear                   $ErrCtrl
226000000000     C                   Clear                   $TipErr
226100000000     C*
226200000000     C                   if        $Denom  = *Blank  and $Tpn  > '1'  or
226300000000     C                             $Denom  <>*Blank  and $Tpn =  '1'
226400000000     C                   Movel     '1'           $TipErr
226500000000     C                   else
226600000000     C*
226700000000     C                   If        $Denom <> *blanks
226800000000     C                   Movel     $Denom        $CampoScan
226900000000     C                   Exsr      ScanCharOk
227000000000     C                   If        $ErrScan = '1'
227100000000     C                   Movel     '2'           $TipErr
227200000000     C                   endif
227300000000     C                   endif
227400000000     C                   endif
227500000000     C*
227600000000     C                   IF        $TipErr <> *blanks
227700000000     C                   Movel     *on           $Errctrl
227800000000     C                   endif
227900000000     C*
228000000000     C                   ENDSR
228100000000     C/EJECT
228200000000     C***********************************************************
228300000000     C* Controllo Cognome
228400000000     C***********************************************************
228500000000     C     CtrCognome    BEGSR
228600000000     C*
228700000000     C                   Clear                   $ErrCtrl
228800000000     C                   Clear                   $TipErr
228900000000     C*
229000000000     C                   if        $Cognom = *Blank  and $Tpn =  '1'  or
229100000000     C                             $Cognom <>*Blank  and $Tpn >  '1'
229200000000     C                   Movel     '1'           $TipErr
229300000000     C                   else
229400000000     C*
229500000000     C                   If        $Cognom<> *blanks
229600000000     C                   Movel     $Cognom       $CampoScan
229700000000     C                   Exsr      ScanCharOk
229800000000     C                   If        $ErrScan = '1'
229900000000     C                   Movel     '2'           $TipErr
230000000000     C                   endif
230100000000     C                   endif
230200000000     C                   endif
230300000000     C*
230400000000     C                   IF        $TipErr <> *blanks
230500000000     C                   Movel     *on           $Errctrl
230600000000     C                   endif
230700000000     C*
230800000000     C                   ENDSR
230900000000     C/EJECT
231000000000     C***********************************************************
231100000000     C* Controllo nome
231200000000     C***********************************************************
231300000000     C     CtrNome       BEGSR
231400000000     C*
231500000000     C                   Clear                   $ErrCtrl
231600000000     C                   Clear                   $TipErr
231700000000     C*
231800000000     C                   if        $Nome   = *Blank  and $Tpn =  '1'  or
231900000000     C                             $Nome   <>*Blank  and  $Tpn >  '1'
232000000000     C                   Movel     '1'           $TipErr
232100000000     C                   else
232200000000     C*
232300000000     C                   If        $Nome  <> *blanks
232400000000     C                   Movel     $Nome         $CampoScan
232500000000     C                   Exsr      ScanCharOk
232600000000     C                   If        $ErrScan = '1'
232700000000     C                   Movel     '2'           $TipErr
232800000000     C                   endif
232900000000     C                   endif
233000000000     C                   endif
233100000000     C*
233200000000     C                   IF        $TipErr <> *blanks
233300000000     C                   Movel     *on           $Errctrl
233400000000     C                   endif
233500000000     C*
233600000000     C                   ENDSR
233700000000     C***********************************************************
233800000000     C* Controllo provincia
233900000000     C***********************************************************
234000000000     C     CtrPrv        BEGSR
234100000000     C*
234200000000     C                   Clear                   $ErrCtrl
234300000000     C                   Clear                   $TipErr
234400000000     C*
234500000000     C                   Clear                   $PrvD
234600000000     C*
234700000000     C                   Clear                   ANGC17DS
234800000000     C*
234900000000     C                   Select
235000000000     C*
235100000000     C                   When      $Prv       = *Blanks and
235200000000     C                             $CtrObbl = *on
235300000000     C                   Movel     *On           $ErrCtrl
235400000000     C                   Movel     '1'           $TipErr
235500000000     C*
235600000000     C                   When      $Prv       <> *Blanks
235700000000     C*
235800000000     C                   If        $Prv = 'DA' and $CtrObbl = *ON
235900000000     C                   MoveL(p)  'Dato Assente'$PrvD
236000000000     C                   Else
236100000000     C*
236200000000     C                   RESET                   SDGTABDS
236300000000     C*
236400000000     C                   Eval      XTbRic      = '1'
236500000000     C                   Eval      XTbAzi      = XScSoc
236600000000     C                   Eval      XTbKey      = *Zero
236700000000     C                   Eval      XTbCod      = 'C17'
236800000000     C                   Move      $Prv          KeyC17
236900000000     C                   Movel     $Stato        KeyC17
237000000000     C                   Move      KeyC17        XTBKey
237100000000     C*
237200000000     C                   CALL      'SDGTABDR'
237300000000     C                   PARM                    SDGTABDS
237400000000     C*
237500000000     C                   if        XTbErr     <> '0'
237600000000     C                   Movel     *on           $ErrCtrl
237700000000     C                   Movel     '2'           $TipErr
237800000000     C                   else
237900000000     C                   MoveL     XTbUni        ANGC17DS
238000000000     C                   MoveL     DesC17        $PrvD
238100000000     C                   endif
238200000000     C*
238300000000     C                   EndIf
238400000000     C*
238500000000     C                   EndSL
238600000000     C*
238700000000     C                   ENDSR
238800000000     C/EJECT
238900000000     C*****************************************************
239000000000     C* Controlli sul codice avviamento postale
239100000000     C*****************************************************
239200000000     C     CtrCAP        begsr
239300000000     C*
239400000000     C                   Clear                   $ErrCtrl
239500000000     C                   Clear                   $TipErr
239600000000     C*
239700000000     C* i controlli devono essere eseguiti con questo ordine, per cui
239800000000     C* al primo vado via
239900000000     C                   do
240000000000     C*
240100000000     C* Ctrl C.A.P., località, nazione
240200000000     C                   If        CecG12 = '2'
240300170831R2653C                             and $Stato   <> *blank
240400170828 "   C                             and $Cap     <> *blank
240500170831R2653C                             and $Locali  <> *blank
240600000000     C                   move      $Stato        CapStato
240700000000     C                   move      $CAP          CapCap
240800170831R2653X****               Move      $Locali       CapLocalit
240900170831R2653C                   Movel     $Locali       CapLocalit
241000000000     C     K03CAP01      Chain     Ancap01l                           21
241100000000     C                   if        *in21  = *on
241200000000     C* Località errata o C.A.P. non congruente con la località.
241300000000     C                   Movel     '1'           $TipErr
241400000000     c                   leave
241500000000     C* altrimenti, se il CAP è valido i dati reperiti sull'
241600000000     C* anagrafica ANCAP possono servire a valorizzare provincia
241700000000     C* e località
241800000000     C                   else
241900000000     C                   exsr      ValProv_Loc
242000000000     C                   endif
242100000000     C                   endif
242200000000     C*
242300000000     C* VALIDITA':
242400000000     c*   se CAP valorizzato
242500000000     C                   if        $CAP <> *blank
242600000000     C* e se su tabella stati è indicato
242700000000     C*   che DEVE essere eseguito il controllo del CAP
242800000000     C                             and CecG12 = '1'
242900170831R2653C                             and $Stato   <> *blank
243000000000     C* deve esistere su file codici avviamento
243100000000     C* postale...
243200000000     C                   move      $Stato        CAPStato
243300000000     C                   move      $CAP          CAPCap
243400000000     C     K02CAP01      chain     ANCAP01L                           21
243500000000     C* Codice di avviamento postale non trovato in anagrafica
243600000000fm8  C                   if        *in21  = *on
243700000000     C* "(F1) C.A.P. inesistente"
243800000000     C                   Movel     '2'           $TipErr
243900000000     c                   leave
244000000000     C* altrimenti, se il CAP è valido i dati reperiti sull'
244100000000     C* anagrafica ANCAP possono servire a valorizzare provincia
244200000000     C* e località
244300000000     C                   else
244400000000     C                   exsr      ValProv_Loc
244500000000     C                   endif
244600000000     C*
244700000000     C* NUMERO CARATTERI
244800000000     C* il numero di caratteri deve corrispondere a quello della tabell
244900000000     C     ' '           checkr    $CAP          $NrCar
245000000000     C                   if        $NrCar <> CAPG12 and CAPg12 <> 0
245100000000     c
245200000000     C* "C.A.P. ha un numero di caratteri diversi ..."
245300000000     C                   Movel     '3'           $TipErr
245400000000     c                   leave
245500000000     C                   endif
245600000000     C*
245700000000     C                   endif
245800000000     C*
245900000000     C                   enddo
246000000000     C*
246100000000     C                   If        $Tiperr <> *blanks
246200000000     C                   Movel     *on           $ErrCtrl
246300000000     C                   Endif
246400000000     C*
246500000000     C                   endsr
246600000000     C*****************************************************
246700000000     C* Valorizzazione provincia e località da anagrafica
246800000000     C* codici avviamento postale, se non già valorizzati
246900000000     C*****************************************************
247000000000     C     ValProv_Loc   begsr
247100000000     C*
247200000000     C* Provincia
247300000000     C* Si eseguono anche i controlli in modo da valorizzare
247400000000     C* anche la decodifica
247500000000     C                   if        $Prv       = *blank
247600000000     C                             and  CAPProv <> *blank
247700000000     C                   move      CAPProv       $Prv
247800000000     C                   endif
247900000000     C* Località
248000000000     C                   if        $Locali   = *blank
248100000000     C                             and  CAPLocalit <> *blank
248200000000     C                   move      CAPLocalit    $Locali
248300000000     C                   endif
248400000000     C*
248500000000     C                   endsr
248600000000     C************************************************************
248700000000     C* Scansione per caratteri non ammessi
248800000000     C************************************************************
248900000000     C     ScanCharOk    BEGSR
249000000000     C*
249100000000     C                   Clear                   $Errscan
249200000000     C*
249300000000     C* prima converte alcuni caratteri, sostituendoli con altri ammessi
249400000000     C     CnvErr:CnvOk  XLATE     $CampoScan    $CampoScan
249500000000     C*
249600000000     C* poi sostituisce con uno spazio i caratteri rimanenti non ammessi
249700000000     C                   Eval      $FineScan   = *Off
249800000000     C*
249900000000     C                   Clear                   PosIni            4 0
250000000000     C     CharOK        Check     $CampoScan    PosIni
250100000000     C*
250200000000     C                   if        PosIni <> 0
250300000000     C                   Eval      $ErrScan = *on
250400000000     C                   endif
250500000000     C*
250600000000     C                   ENDSR
250700000000     C/EJECT
250800000000     C************************************************************
250900000000     C* GESTIONE VIDEO RECORD  D2
251000000000     C************************************************************
251100000000    >C     GESD2         BEGSR
251200000000     C* emissione piede videata se proveniente da altra
251300000000     C* salvataggio valore formato di provenienza
251400000000   B1C     $LASTV        IFNE      'D2'
251500000000     C                   WRITE     FMTT1
251600000000     C                   EXSR      GESPIE
251700000000     C                   MOVE      $LASTV        $LASTG
251800000000     C                   MOVE      'D2'          $LASTV
251900000000   E1C                   ENDIF
252000000000     C*
252100000000     C* se esistono errori sulla videata
252200000000     C* emetto la write del formato a indicatori spenti per vedere
252300000000     C* le eventuali decodifiche
252400000000   B1C                   if         *IN99       = *On      and
252500000000     C                             $GEST        = 'D2'
252600000000     C                   MOVEA     *IN           WIN
252700000000     C                   MOVE      *ALL'0'       IN5098           49
252800000000     C                   MOVEA     IN5098        *IN(50)
252900000000     C                   WRITE     FMTD2
253000000000     C                   MOVEA     WIN           *IN
253100000000   E1C                   ENDIF
253200000000     C* Inizializzazione indice tasti funzione personalizzati
253300000000     C                   Eval      PosFun = 25
253400000000     C*
253500000000   B1C                   SELECT
253600000000     C* Annullamento
253700000000    >C     OPZI74        WHENEQ    '04'
253800000000     C* Visualizzazione
253900000000    >C     OPZI74        OREQ      '05'
254000000000     C                   WRITE     FMTD2
254100000000     C                   EXFMT     PROTECT
254200000000     C* Immissione/Modifica/Copia
254300000000   X1C                   OTHER
254400000000     C                   EXFMT     FMTD2
254500000000   E1C                   ENDSL
254600000000     C*
254700000000     C* azzero la posizione del cursore
254800000000     C                   Z-ADD     0             H1RIGA
254900000000     C                   Z-ADD     0             H1COLO
255000000000     C*
255100000000     C* controllo abilitazione tasti funzione
255200000000     C                   EXSR      CTRKD1
255300000000     C*
255400000000   B1C     *IN99         IFEQ      *OFF
255500000000     C*
255600000000   B2C                   SELECT
255700000000     C* ROLDOWN
255800000000     C     $TASTO        WHENEQ    ROLDWN
255900000000     C                   MOVE      'D1'          $GEST
256000000000     C* ROLLUP
256100000000     C     $TASTO        WHENEQ    ROLLUP
256200000000     C                   MOVE      'D3'          $GEST
256300000000     C* F3=Fine
256400000000     C     $TASTO        WHENEQ    F03
256500000000     C                   EXSR      F03D1
256600000000     C* F4=Ricerca
256700000000     C     $TASTO        WHENEQ    F04
256800000000     C                   EXSR      SEARCH
256900000000     C* F5=Refresh
257000000000     C     $TASTO        WHENEQ    F05
257100000000     C                   EXSR      F05D2
257200000000     C* F8=Successivo
257300000000     C     $TASTO        WHENEQ    F08
257400000000     C                   EXSR      F08D1
257500000000     C* F12=Ritorno
257600000000     C     $TASTO        WHENEQ    F12
257700000000     C                   EXSR      F12D1
257800000000     C* F24=Altri tasti
257900000000     C     $TASTO        WHENEQ    F24
258000000000     C                   EXSR      F24D2
258100000000     C* Personalizzata
258200000000     C* NB chiamata NON possibile perchè SDGSI74DS più lunga
258300000000     C* di KPJBU 256 caratteri                          a
258400000000     C**** $PGF1(PosFun) Whenne    *BLANK
258500000000     C****               EXSR      FperD1
258600000000     C*
258700000000   X2C                   OTHER
258800000000     C                   EXSR      CTRD2
258900000000   B3C     *IN99         IFEQ      *OFF
259000000000     C                   EXSR      CTRD1
259100000000   B1C                   if        *IN99       = *On
259200000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
259300000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
259400000000     C* videate
259500000000     C                   MOVE      *ALL'0'       IN5098           49
259600000000     C                   MOVEA     IN5098        *IN(50)
259700000000     C                   SETON                                        9996                       96
259800000000     C                   ENDIF
259900000000     C                   ENDIF
260000000000     C* aggiungere le chiamate ai controlli delle videate supplementari
260100000000    >C     *IN99         IFEQ      *OFF
260200000000    >C                   EXSR      CTRD3
260300000000   B1C                   if        *IN99       = *On
260400000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
260500000000    >C* indicatori uguali in videate differenti, genererei conflitti
260600000000    >C                   MOVE      *ALL'0'       IN5098           49
260700000000    >C                   MOVEA     IN5098        *IN(50)
260800000000    >C                   SETON                                        9996                       96
260900000000    >C                   ENDIF
261000000000    >C                   ENDIF
261100000000     C*
261200000000   B3C     $TASTO        IFEQ      F06
261300000000     C     *IN99         ANDEQ     *OFF
261400000000     C* eseguo aggiornamento
261500000000     C                   EXSR      AGGANA
261600000000     C* eseguo operazioni del dopo-aggiornamento
261700000000   B4C     *IN99         IFEQ      *OFF
261800000000     C                   EXSR      GESAGG
261900000000   E4C                   ENDIF
262000000000   E3C                   ENDIF
262100000000     C*
262200000000   E2C                   ENDSL
262300000000     C*
262400000000   E1C                   ENDIF
262500000000     C*
262600000000    >C                   ENDSR
262700000000     C/EJECT
262800000000     C************************************************************
262900000000     C* GESTIONE F05 VIDEO D2
263000000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
263100000000     C* e il riposizionamento alla prima
263200000000     C************************************************************
263300000000    >C     F05D2         BEGSR
263400000000     C*
263500000000     C                   MOVE      *ON           $INZD1
263600000000     C                   MOVE      'D1'          $GEST
263700000000     C*
263800000000    >C                   ENDSR
263900000000     C************************************************************
264000000000     C* GESTIONE F24 VIDEO D2
264100000000     C************************************************************
264200000000    >C     F24D2         BEGSR
264300000000     C*
264400000000     C                   MOVEA     $FX1          $FX
264500000000     C                   MOVEA     $FC1          $FC
264600000000     C                   MOVEA     $FR1          $FR
264700000000     C                   Z-ADD     $ULKD1        $ULK
264800000000     C                   Z-ADD     $D1L01        $L01
264900000000     C                   Z-ADD     $D1L02        $L02
265000000000     C                   EXSR      RIEKEY
265100000000     C                   Z-ADD     $ULK          $ULKD1            3 0
265200000000     C                   MOVEA     $FC           $FC1
265300000000     C                   MOVEL     $KEY1         Z1KE1
265400000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
265500000000     C                   MOVEL     $KEY2         Z1KE2
265600000000     C                   WRITE     FMTZ1
265700000000     C*
265800000000    >C                   ENDSR
265900000000     C/EJECT
266000000000     C************************************************************
266100000000     C* CONTROLLO VIDEATA  D2
266200000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
266300000000     C* 95 96 97 98
266400000000     C************************************************************
266500000000    >C     CTRD2         BEGSR
266600000000     C*
266700000000     C                   SETOFF                                       99
266800000000     C*
266900000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
267000000000     C*
267100000000    >C     OPZI74        IFNE      '04'
267200000000     C*
267300000000     C* Salvo i campi che hanno controlli comuni o che possono essere valorizzati
267400000000     C                   Eval      $Stato  = D1Stato2
267500000000     C                   Eval      $Cap    = D1Cap2
267600000000     C                   Eval      $Prv    = D1Prov2
267700000000     C                   Eval      $Locali = D1Locali2
267800000000     C*
267900000000     C* Stato
268000000000     C                   EXSR      CtrStato
268100000000     C*
268200000000     C                   If        $ErrCtrl = *on
268300000000     C*
268400000000     C                   If        $TipErr  = '1'
268500000000     C                   Seton                                        5999
268600000000     C                   Endif
268700000000     C*
268800000000     C                   If        $TipErr  = '2'
268900000000     C                   Seton                                        8299
269000000000     C                   Endif
269100000000     C*
269200000000     C                   Endif
269300000000     C*
269400000000     C                   Movel     $Statod       D1Stato2D
269500000000     C                   Movel     $IdPaese      D1IdPaes2
269600000000     C*
269700000000     C*
269800000000     C                   Exsr      CtrCap
269900000000     C*
270000000000     C                   If        $ErrCtrl = *on
270100000000     C*
270200000000     C                   If        $TipErr  = '1'
270300000000     C                   Seton                                        7399
270400000000     C                   Endif
270500000000     C*
270600000000     C                   If        $TipErr  = '2'
270700000000     C                   Seton                                        7499
270800000000     C                   Endif
270900000000     C*
271000000000     C                   If        $TipErr  = '3'
271100000000     C                   Seton                                        7899
271200000000     C                   Endif
271300000000     C*
271400000000     C                   Else
271500000000     C                   Movel     $Prv          D1Prov2
271600000000     C                   Movel     $Locali       D1Locali2
271700000000     C                   Endif
271800000000     C*
271900000000     C* Controllo provincia solo se IT
272000000000PP   C                   Movel     *off          $CtrObbl
272100000000     C                   If        $IdPaese = 'IT'
272200000000     C                   Movel     *on           $CtrObbl
272300000000     C                   Endif
272400000000     C*
272500000000     C                   EXSR      CtrPrv
272600000000     C*
272700000000     C                   If        $ErrCtrl = *on
272800000000     C                   IF        $Tiperr = '1'
272900000000     C                   Seton                                        9199
273000000000     C                   Else
273100000000     C                   Seton                                        5899
273200000000     C                   EndIF
273300000000     C                   Else
273400000000     C                   Movel     $Prvd         D1Prov2d
273500000000     C                   Endif
273600000000     C*
273700000000     C* Controllo località
273800000000     C                   Movel     *on           $CtrObbl
273900000000     C                   EXSR      CtrLocali
274000000000     C*
274100000000     C                   If        $ErrCtrl = *on
274200000000     C                   If        $Tiperr = '1'
274300000000     C                   Seton                                        8999
274400000000     C                   Else
274500000000     C                   Seton                                        6999
274600000000     C                   EndIF
274700000000     C                   EndIF
274800000000     C*
274900000000     C* Controllo indirizzo sede
275000000000     C                   Eval      $Indir = D1Indir2
275100000000     C                   Movel     *on           $CtrObbl
275200000000     C                   EXSR      CtrIndir
275300000000     C*
275400000000     C                   If        $ErrCtrl = *on
275500000000     C                   If        $Tiperr = '1'
275600000000     C                   Seton                                        8899
275700000000     C                   Else
275800000000     C                   Seton                                        6899
275900000000     C                   Endif
276000000000     C                   EndIF
276100000000     C*
276200000000     C* Controlli per stabile organizzazione
276300000000     C                   Eval      $Stato  = D1Stato3
276400000000     C                   Eval      $Cap    = D1Cap3
276500000000     C                   Eval      $Prv    = D1Prov3
276600000000     C                   Eval      $Locali = D1Locali3
276700000000     C                   Eval      $Indir = D1Indir3
276800000000     C*
276900000000     C* Stato
277000000000     C                   EXSR      CtrStato
277100000000     C*
277200000000     C                   If        $ErrCtrl = *on
277300000000     C*
277400000000     C                   If        $TipErr  = '1'
277500000000     C                   Seton                                        6199
277600000000     C                   Endif
277700000000     C*
277800000000     C                   If        $TipErr  = '2'
277900000000     C                   Seton                                        8399
278000000000     C                   Endif
278100000000     C*
278200000000     C                   Endif
278300000000     C*
278400000000     C                   Movel     $Statod       D1Stato3D
278500000000     C                   Movel     $IdPaese      D1IdPaes3
278600000000     C* Indirizzo
278700000000     C                   Movel     *off          $CtrObbl
278800000000     C                   EXSR      CtrIndir
278900000000     C*
279000000000     C                   If        $ErrCtrl = *on
279100000000     C                   Seton                                        7099
279200000000     C                   EndIF
279300000000     C*
279400000000     C                   ENDIF
279500000000     C*
279600000000     C                   EndSR
279700000000     C/EJECT
279800000000     C************************************************************
279900000000     C* GESTIONE VIDEO RECORD  D3
280000000000     C************************************************************
280100000000    >C     GESD3         BEGSR
280200000000     C* emissione piede videata se proveniente da altra
280300000000     C* salvataggio valore formato di provenienza
280400000000   B1C     $LASTV        IFNE      'D3'
280500000000     C                   WRITE     FMTT1
280600000000     C                   EXSR      GESPIE
280700000000     C                   MOVE      $LASTV        $LASTG
280800000000     C                   MOVE      'D3'          $LASTV
280900000000   E1C                   ENDIF
281000000000     C*
281100000000     C* se esistono errori sulla videata
281200000000     C* emetto la write del formato a indicatori spenti per vedere
281300000000     C* le eventuali decodifiche
281400000000   B1C                   if        *IN99       = *On   and
281500000000     C                             $GEST        = 'D3'
281600000000     C                   MOVEA     *IN           WIN
281700000000     C                   MOVE      *ALL'0'       IN5098           49
281800000000     C                   MOVEA     IN5098        *IN(50)
281900000000     C                   WRITE     FMTD3
282000000000     C                   MOVEA     WIN           *IN
282100000000   E1C                   ENDIF
282200000000     C* Inizializzazione indice tasti funzione personalizzati
282300000000     C                   Eval      PosFun = 25
282400000000     C*
282500000000   B1C                   SELECT
282600000000     C* Annullamento
282700000000    >C     OPZI74        WHENEQ    '04'
282800000000     C* Visualizzazione
282900000000    >C     OPZI74        OREQ      '05'
283000000000     C                   WRITE     FMTD3
283100000000     C                   EXFMT     PROTECT
283200000000     C* Immissione/Modifica/Copia
283300000000   X1C                   OTHER
283400000000     C                   EXFMT     FMTD3
283500000000   E1C                   ENDSL
283600000000     C*
283700000000     C* azzero la posizione del cursore
283800000000     C                   Z-ADD     0             H1RIGA
283900000000     C                   Z-ADD     0             H1COLO
284000000000     C*
284100000000     C* controllo abilitazione tasti funzione
284200000000     C                   EXSR      CTRKD1
284300000000     C*
284400000000   B1C     *IN99         IFEQ      *OFF
284500000000     C*
284600000000   B2C                   SELECT
284700000000     C* ROLDOWN
284800000000     C     $TASTO        WHENEQ    ROLDWN
284900000000     C                   MOVE      'D2'          $GEST
285000000000     C* F3=Fine
285100000000     C     $TASTO        WHENEQ    F03
285200000000     C                   EXSR      F03D1
285300000000     C* F4=Ricerca
285400000000     C     $TASTO        WHENEQ    F04
285500000000     C                   EXSR      SEARCH
285600000000     C* F5=Refresh
285700000000     C     $TASTO        WHENEQ    F05
285800000000     C                   EXSR      F05D3
285900000000     C* F8=Successivo
286000000000     C     $TASTO        WHENEQ    F08
286100000000     C                   EXSR      F08D1
286200000000     C* F12=Ritorno
286300000000     C     $TASTO        WHENEQ    F12
286400000000     C                   EXSR      F12D1
286500000000     C* F24=Altri tasti
286600000000     C     $TASTO        WHENEQ    F24
286700000000     C                   EXSR      F24D3
286800000000     C* Personalizzata
286900000000     C* NB chiamata NON possibile perchè SDGSI74DS più lunga
287000000000     C* di KPJBU 256 caratteri                          a
287100000000     C**** $PGF1(PosFun) Whenne    *BLANK
287200000000     C****               EXSR      FperD1
287300000000     C*
287400000000   X2C                   OTHER
287500000000     C                   EXSR      CTRD3
287600000000     C* aggiungere le chiamate ai controlli delle videate supplementari
287700000000    >C     *IN99         IFEQ      *OFF
287800000000    >C                   EXSR      CTRD2
287900000000   B1C                   if        *IN99       = *On
288000000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
288100000000    >C* indicatori uguali in videate differenti, genererei conflitti
288200000000    >C                   MOVE      *ALL'0'       IN5098           49
288300000000    >C                   MOVEA     IN5098        *IN(50)
288400000000    >C                   SETON                                        9996                       96
288500000000    >C                   ENDIF
288600000000    >C                   ENDIF
288700000000     C* aggiungere le chiamate ai controlli delle videate supplementari
288800000000    >C     *IN99         IFEQ      *OFF
288900000000    >C                   EXSR      CTRD1
289000000000   B1C                   if        *IN99       = *On
289100000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
289200000000    >C* indicatori uguali in videate differenti, genererei conflitti
289300000000    >C                   MOVE      *ALL'0'       IN5098           49
289400000000    >C                   MOVEA     IN5098        *IN(50)
289500000000    >C                   SETON                                        9996                       96
289600000000    >C                   ENDIF
289700000000    >C                   ENDIF
289800000000     C*
289900000000   B3C     $TASTO        IFEQ      F06
290000000000     C     *IN99         ANDEQ     *OFF
290100000000     C* eseguo aggiornamento
290200000000     C                   EXSR      AGGANA
290300000000     C* eseguo operazioni del dopo-aggiornamento
290400000000   B4C     *IN99         IFEQ      *OFF
290500000000     C                   EXSR      GESAGG
290600000000   E4C                   ENDIF
290700000000   E3C                   ENDIF
290800000000     C*
290900000000   E2C                   ENDSL
291000000000     C*
291100000000   E1C                   ENDIF
291200000000     C*
291300000000    >C                   ENDSR
291400000000     C/EJECT
291500000000     C************************************************************
291600000000     C* GESTIONE F05 VIDEO D3
291700000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
291800000000     C* e il riposizionamento alla prima
291900000000     C************************************************************
292000000000    >C     F05D3         BEGSR
292100000000     C*
292200000000     C                   MOVE      *ON           $INZD1
292300000000     C                   MOVE      'D1'          $GEST
292400000000     C*
292500000000    >C                   ENDSR
292600000000     C************************************************************
292700000000     C* GESTIONE F24 VIDEO D2
292800000000     C************************************************************
292900000000    >C     F24D3         BEGSR
293000000000     C*
293100000000     C                   MOVEA     $FX1          $FX
293200000000     C                   MOVEA     $FC1          $FC
293300000000     C                   MOVEA     $FR1          $FR
293400000000     C                   Z-ADD     $ULKD1        $ULK
293500000000     C                   Z-ADD     $D1L01        $L01
293600000000     C                   Z-ADD     $D1L02        $L02
293700000000     C                   EXSR      RIEKEY
293800000000     C                   Z-ADD     $ULK          $ULKD1            3 0
293900000000     C                   MOVEA     $FC           $FC1
294000000000     C                   MOVEL     $KEY1         Z1KE1
294100000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
294200000000     C                   MOVEL     $KEY2         Z1KE2
294300000000     C                   WRITE     FMTZ1
294400000000     C*
294500000000    >C                   ENDSR
294600000000     C/EJECT
294700000000     C************************************************************
294800000000     C* CONTROLLO VIDEATA  D3
294900000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
295000000000     C* 95 96 97 98
295100000000     C************************************************************
295200000000    >C     CTRD3         BEGSR
295300000000     C*
295400000000     C                   SETOFF                                       99
295500000000     C*
295600000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
295700000000     C*
295800000000    >C     OPZI74        IFNE      '04'
295900000000     C*
296000000000     C* Salvo i campi che hanno controlli comuni o che possono essere valorizzati
296100000000     C                   Eval      $Stato  = D1Stato3
296200000000     C                   Eval      $Cap    = D1Cap3
296300000000     C                   Eval      $Prv    = D1Prov3
296400000000     C                   Eval      $Locali = D1Locali3
296500000000     C*  CAP
296600000000     C                   Exsr      CtrCap
296700000000     C*
296800000000     C                   If        $ErrCtrl = *on
296900000000     C*
297000000000     C                   If        $TipErr  = '1'
297100000000     C                   Seton                                        7999
297200000000     C                   Endif
297300000000     C*
297400000000     C                   If        $TipErr  = '2'
297500000000     C                   Seton                                        8099
297600000000     C                   Endif
297700000000     C*
297800000000     C                   If        $TipErr  = '3'
297900000000     C                   Seton                                        8199
298000000000     C                   Endif
298100000000     C*
298200000000     C                   Else
298300000000     C                   Movel     $Prv          D1Prov3
298400000000     C                   Movel     $Locali       D1Locali3
298500000000     C                   Endif
298600000000     C*
298700000000     C* Controllo provincia
298800000000     C                   Movel     *off          $CtrObbl
298900000000     C                   EXSR      CtrPrv
299000000000     C*
299100000000     C                   If        $ErrCtrl = *on
299200000000     C                   Seton                                        6099
299300000000     C                   Else
299400000000     C                   Movel     $Prvd         D1Prov3d
299500000000     C                   Endif
299600000000     C*
299700000000     C* Controllo località
299800000000     C                   EXSR      CtrLocali
299900000000     C*
300000000000     C                   If        $ErrCtrl = *on
300100000000     C                   Seton                                        7199
300200000000     C                   EndIF
300300000000     C*
300400000000     C* Stato Rappresentante fiscale
300500000000     C                   Eval      $stato = d1stato4
300600000000     C*
300700000000     C                   EXSR      CtrStato
300800000000     C*
300900000000     C                   If        $ErrCtrl = *on
301000000000     C*
301100000000     C                   If        $TipErr  = '1'
301200000000     C                   Seton                                        6299
301300000000     C                   Endif
301400000000     C*
301500000000     C                   If        $TipErr  = '2'
301600000000     C                   Seton                                        8499
301700000000     C                   Endif
301800000000     C*
301900000000     C                   Endif
302000000000     C*
302100000000     C                   Movel     $Statod       D1Stato4D
302200000000     C                   Movel     $IdPaese      D1IdPaes4
302300000000     C*
302400000000     C* Salvo i campi che hanno controlli comuni
302500000000     C                   Movel     D1Denom4      $Denom
302600000000     C                   Movel     D1Nome4       $Nome
302700000000     C                   Movel     D1Cognom4     $Cognom
302800000000     C                   If        D1Denom4<> *blanks
302900000000     C                   Movel     D1Denom4      H1CogRag4
303000000000     C                   Else
303100000000     C                   Movel     D1Cognom4     H1CogRag4
303200000000     C                   Endif
303300000000     C                   Movel     D1Tpn4        $Tpn
303400000000     C*
303500000000     C* Tipo Natura giuridica
303600000000     C                   Movel     *Off          $CtrObbl
303700000000     C                   EXSR      CtrTPN
303800000000     C                   Movel     $Tpnd         D1Tpn4d
303900000000     C                   If        $ErrCtrl = *on
304000000000     C                   Seton                                        6499
304100000000     C                   Endif
304200000000     C*
304300000000     C* Denominazione
304400000000     C                   EXSR      CtrDenom
304500000000     C                   If        $ErrCtrl = *on
304600000000     C                   If        $TipErr  = '1'
304700000000     C                   Seton                                        6599
304800000000     C                   Else
304900000000     C                   Seton                                        8599
305000000000     C                   Endif
305100000000     C                   Endif
305200000000     C*
305300000000     C* Cognome
305400000000     C                   EXSR      CtrCognome
305500000000     C                   If        $ErrCtrl = *on
305600000000     C                   If        $TipErr  = '1'
305700000000     C                   Seton                                        6699
305800000000     C                   Else
305900000000     C                   Seton                                        8699
306000000000     C                   Endif
306100000000     C                   Endif
306200000000     C* Nome
306300000000     C                   EXSR      CtrNome
306400000000     C                   If        $ErrCtrl = *on
306500000000     C                   If        $TipErr  = '1'
306600000000     C                   Seton                                        7799
306700000000     C                   Else
306800000000     C                   Seton                                        8799
306900000000     C                   Endif
307000000000     C                   Endif
307100000000     C                   ENDIF
307200000000     C*
307300000000     C                   ENDSR
307400000000     C***********************************************************
307500000000     C     CtrPiva_CDF   BEGSR
307600000000     C*
307700000000     C                   If        D1Piva1 = *blanks  and
307800000000     C                             D1Cdfisc1 = *blanks
307900000000Fm3  C                   if        D1IdPaes1 = 'OO'
308000000000fm3  C                   Eval      D1Piva1   = '99999999999'
308100000000fm3  C                   else
308200000000     C                   Seton                                        535499
308300000000fm3  C                   Endif
308400000000     C                   Endif
308500000000     C*
308600000000     C                   ENDSR
308700000000     C/EJECT
308800000000     C***********************************************************
308900000000     C* Controllo Stato
309000000000     C***********************************************************
309100000000     C     CtrStato      BEGSR
309200000000     C*
309300000000     C                   Clear                   $ErrCtrl
309400000000     C                   Clear                   $TipErr
309500000000     C*
309600000000     C                   Clear                   $StatoD
309700000000     C                   Clear                   $IdPaese
309800000000     C*
309900000000     C                   Clear                   AngG12DS
310000000000     C*
310100000000     C                   Clear                   SDGTABDS
310200000000     C*
310300000000     C                   if        $Stato     <> *Blank
310400000000     C                   Eval      XTbRic      = '1'
310500000000     C                   Eval      XTbAzi      = XScSoc
310600000000     C                   Eval      XTbKey      = *Zero
310700000000     C                   Eval      XTbCod      = 'G12'
310800000000     C                   Move      $Stato        XTbKey
310900000000     C*
311000000000     C                   CALL      'SDGTABDR'
311100000000     C                   PARM                    SDGTABDS
311200000000     C*
311300000000     C                   if        XTbErr     <> '0'
311400000000     C*                  Eval      $Errctrl   = *on
311500000000     C*                  Eval      $TipErr    = '2'
311600000000     C                   endif
311700000000     C                   MoveL     XTbUni        AngG12DS
311800000000     C                   MoveL     DesG12        $StatoD
311900000000     C
312000000000     C* Reperisco il codice Iso da tabelle HD4
312100000000     C                   Clear                   SDGHD4DS
312200000000     C                   Clear                   SDGTABDS
312300000000     C                   Eval      XTbRic      = '1'
312400000000     C                   Eval      XTbAzi      = XScSoc
312500000000     C                   Eval      XTbKey      = *Zero
312600000000     C                   Eval      XTbCod      = 'HD4'
312700000000     C                   Move      $Stato        XTbKey
312800000000     C*
312900000000     C                   CALL      'SDGTABDR'
313000000000     C                   PARM                    SDGTABDS
313100000000     C                   if        XTbErr     <> '0'
313200000000     C                   Eval      $Errctrl   = *on
313300000000     C                   Eval      $TipErr    = '2'
313400000000     C                   else
313500000000     C                   MoveL     XTbUni        SDGHD4DS
313600000000     C                   MoveL     Codiso2Hd4    $IdPaese
313700000000     C                   MoveL     DesHd4        $StatoD
313800000000     C                   Endif
313900000000     C*
314000000000     C**                 Endif
314100000000     C*
314200000000     C                   Endif
314300000000     C*
314400000000     C                   ENDSR
314500000000     C/EJECT
314600000000     C************************************************************
314700000000     C* Controllo Località di residenza
314800000000     C************************************************************
314900000000     C     CtrLocali     BEGSR
315000000000     C*
315100000000     C                   Clear                   $ErrCtrl
315200000000     C                   Clear                   $TipErr
315300000000     C*
315400000000     C                   Select
315500000000     C*
315600000000     C                   When      $Locali    = *Blanks and
315700000000     C                             $CtrObbl = *on
315800000000     C                   Movel     *On           $ErrCtrl
315900000000     C                   Movel     '1'           $TipErr
316000000000     C*
316100000000     C                   When      $Locali    <> *Blanks
316200000000     C*
316300000000     C                   Movel     $Locali       $CampoScan
316400000000     C                   Exsr      ScanCharOk
316500000000     C                   If        $ErrScan = '1'
316600000000     C                   Movel     *on           $Errctrl
316700000000     C                   Movel     '2'           $TipErr
316800000000     C                   endif
316900000000     C*
317000000000     C                   EndSl
317100000000     C*
317200000000     C                   ENDSR
317300000000     C/EJECT
317400000000     C************************************************************
317500000000     C* Controllo indirizzo
317600000000     C************************************************************
317700000000     C     CtrIndir      BEGSR
317800000000     C*
317900000000     C                   Clear                   $ErrCtrl
318000000000     C                   Clear                   $TipErr
318100000000     C*
318200000000     C                   Select
318300000000     C*
318400000000     C                   When      $Indir     = *Blanks and
318500000000     C                             $CtrObbl = *on
318600000000     C                   Movel     *On           $ErrCtrl
318700000000     C                   Movel     '1'           $TipErr
318800000000     C*
318900000000     C                   When      $Indir     <> *Blanks
319000000000     C*
319100000000     C                   Movel     $Indir        $CampoScan
319200000000     C                   Exsr      ScanCharOk
319300000000     C                   If        $ErrScan = '1'
319400000000     C                   Movel     *on           $Errctrl
319500000000     C                   Movel     '2'           $TipErr
319600000000     C                   endif
319700000000     C*
319800000000     C                   EndSl
319900000000     C*
320000000000     C                   ENDSR
320100000000     C/EJECT
320200000000     C************************************************************
320300000000     C* AGGIORNAMENTO ANAGRAFICA
320400000000     C************************************************************
320500000000     C     AGGANA        BEGSR
320600000000     C*
320700000000     C                   SELECT
320800000000    >C     OPZI74        WHENEQ    '01'
320900000000    >C     OPZI74        OREQ      '03'
321000000000     C                   EXSR      AGGIORNA
321100000000    >C     OPZI74        WHENEQ    '02'
321200000000     C                   EXSR      AGGIORNA
321300000000    >C     OPZI74        WHENEQ    '04'
321400000000     C                   EXSR      AGGIORNA
321500000000    >C     OPZI74        WHENEQ    '20'
321600000000     C                   EXSR      AGGIOMPE
321700000000     C                   ENDSL
321800000000     C
321900000000     C* se rcd non scrivibile attivo errore
322000000000     C     *IN22         IFEQ      *ON
322100000000     C                   SETON                                        9599
322200000000     C                   ENDIF
322300000000     C*
322400000000     C* richiama pgm User Exit
322500000000     C     USRERR        IFEQ      *BLANK
322600000000     C* passa opzione ricevuta a user exit
322700000000    >C                   MOVEL     OPZI74        XUKOPZ
322800000000     C* imposta chiave univoca  per user exit
322900000000     C* la chiave univoca non esiste perchè sto lavorando
323000000000     C* con raggruppamento di dati
323100000000     C                   MOVE      XSCSOC        XUKSOC
323200000000     C                   MOVE      XSCDUN        XUKDUN
323300000000     C                   MOVEL     DSPGM         XUKPGM
323400000000    >C                   MOVEL     *Blank        XUKFL1
323500000000    >C                   MOVEL     *Blank        XUKFL2
323600000000    >C* ho fino a 6 campi chiave passabili
323700000000     C                   MOVEL     XUKDS         KPJBU
323800000000     C     DSPGM         CAT       'U':0         USREXT           10
323900000000     C                   CALL      USREXT                               21
324000000000     C                   PARM                    KPJBA
324100000000     C     *IN21         IFEQ      *ON
324200000000     C                   MOVE      *ON           USRERR            1
324300000000     C                   ENDIF
324400000000     C                   ENDIF
324500000000     C*
324600000000     C                   ENDSR
324700000000     C/EJECT
324800000000     C************************************************************
324900000000     C* VALORIZZA E CHIAMA FUNZIONE DI AGGIORNAMENTO
325000000000     C************************************************************
325100000000     C     AGGIORNA      BEGSR
325200000000     C*
325300000000     C* Prima di richiamare l'aggiornamento svincolo il record
325400000000     C                   If        NRRCSTI74 > 0
325500000000     C                   Unlock    SDGCST00F
325600000000     C                   Close     SDGCST00F
325700000000     C                   Else
325800000000     C                   Unlock    SDGCST02L
325900000000     C                   Close     SDGCST02L
326000000000     C                   Endif
326100000000     C*
326200000000     C                   CLEAR                   SDG00702DS
326300000000     C*
326400000000     C                   Eval      Opz0702 = OpzI74
326500000000     C                   Eval      Cmt0702 = CmtI74
326600000000     C*  Dati della chiave
326700000000     C                   Eval      Soc0702   =  XscSoc
326800000000     C                   Eval      TpCom0702 =  TpComI74
326900000000     C                   Eval      TpRec0702 =  TpRecI74
327000000000     C                   Eval      Anno0702  =  AnnoI74
327100000000     C                   Eval      Trim0702  =  TrimI74
327200000000     C                   Eval      Mese0702  =  MeseI74
327300000000     C*
327400000000     C                   Eval      CLIFOR0702=  SavCliFor
327500000000     C*
327600000000     C                   Eval      NrrCST0702=  NrrCSTI74
327700000000     C*
327800000000     C* Dati relativi al filtro
327900000000     C                   Eval      Causal0702=  CAUSALEI74
328000000000     C                   Eval      Ann0702   =  ANNI74
328100000000     C                   Eval      TPAnn0702 =  TPANNI74
328200000000     C*
328300000000     C* Dati Anagrafici da scrivere modificare
328400000000     C                   Eval      CF0702      = CfInpI74
328500000000     C                   Eval      Sogg0702    = D1SOGG
328600000000     C                   Eval      CF0702      = D1CF
328700000000     C                   Eval      Partiv0702  = D1Partiva
328800000000     C                   Eval      CdFisc0702  = D1CDFisc
328900000000     C                   Eval      Stato10702  = D1Stato1
329000000000     C                   Eval      IDPae10702  = D1IDPaes1
329100000000     C                   Eval      PIVA10702   = D1PIVA1
329200000000     C                   Eval      CDFis10702  = D1CDFisc1
329300000000     C                   Eval      Cogra10702  = H1Cograg1
329400000000     C                   Eval      TPN10702    = D1TPN1
329500000000     C                   Eval      Denom10702  = D1Denom1
329600000000     C                   Eval      Cogno10702  = D1Cognom1
329700000000     C                   Eval      Nome10702   = D1Nome1
329800000000     C                   Eval      Indir20702  = D1Indir2
329900000000     C                   Eval      NrCiv20702  = D1NrCiv2
330000000000     C                   Eval      CAP20702    = D1CAP2
330100000000     C                   Eval      Local20702  = D1Locali2
330200000000     C                   Eval      Prov20702   = D1Prov2
330300000000     C                   Eval      Stato20702  = D1Stato2
330400000000     C                   Eval      IDPae20702  = D1IDPaes2
330500000000     C                   Eval      Indir30702  = D1Indir3
330600000000     C                   Eval      NrCiv30702  = D1NrCiv3
330700000000     C                   Eval      CAP30702    = D1CAP3
330800000000     C                   Eval      Local30702  = D1Locali3
330900000000     C                   Eval      Prov30702   = D1Prov3
331000000000     C                   Eval      Stato30702  = D1Stato3
331100000000     C                   Eval      IDPae30702  = D1IDPaes3
331200000000     C                   Eval      Stato40702  = D1Stato4
331300000000     C                   Eval      IDPae40702  = D1IDPaes4
331400000000     C                   Eval      PIVA40702   = D1PIVA4
331500000000     C                   Eval      Cogra40702  = H1Cograg4
331600000000     C                   Eval      TPN40702    = D1TPN4
331700000000     C                   Eval      Denom40702  = D1Denom4
331800000000     C                   Eval      Cogno40702  = D1Cognom4
331900000000     C                   Eval      Nome40702   = D1Nome4
332000000000     C*
332100000000     C                   CALLB     'SDG00702R'
332200000000     C                   PARM                    KPJBA
332300000000     C                   PARM                    SDG00702DS
332400000000     C*
332500000000     C                   If        Err0702  = *on
332600000000     C                   Seton                                        22
332700000000     C                   Endif
332800000000     C*
332900000000     C                   ENDSR
333000000000     C/EJECT
333100000000     C************************************************************
333200000000     C* VALORIZZA ANMPE
333300000000     C************************************************************
333400000000     C     AGGIOMPE      BEGSR
333500000000     C*
333600170911R266 C                   Eval      WCdFisc  = D1CdFisc
333700170911R266 C                   Eval      WCarica  = %char(D1Carica)
333800000000     C*
333900170908R2657C                   Eval      WStato1  = D1Stato1
334000170908R2657C                   Eval      WTpN1    = D1TpN1
334100000000     C                   Eval      WIdPaes1 = D1IdPaes1
334200000000     C                   Eval      WPIVA1   = D1PIVA1
334300000000     C                   Eval      WCdFisc1 = D1CdFisc1
334400000000     C*
334500000000     C                   Eval      WDENOM1  = D1DENOM1
334600000000     C                   Eval      WCOGNOM1 = D1COGNOM1
334700000000     C                   Eval      WNOME1   = D1NOME1
334800000000     C                   Eval      WINDIR2  = D1INDIR2
334900000000     C                   Eval      WNRCIV2  = D1NRCIV2
335000000000     C                   Eval      WCAP2    = D1CAP2
335100000000     C                   Eval      WLOCALI2 = D1LOCALI2
335200000000     C                   Eval      WPROV2   = D1PROV2
335300170908R2657C                   Eval      WStato2  = D1Stato2
335400000000     C*
335500000000     C                   Eval      WIDPAES2 = D1IDPAES2
335600000000     C*
335700000000     C                   Eval      WINDIR3  = D1INDIR3
335800000000     C                   Eval      WNRCIV3  = D1NRCIV3
335900000000     C                   Eval      WCAP3    = D1CAP3
336000000000     C                   Eval      WLOCALI3 = D1LOCALI3
336100000000     C                   Eval      WPROV3   = D1PROV3
336200000000     C*
336300170908R2657C                   Eval      WStato3  = D1Stato3
336400000000     C                   Eval      WIDPAES3 = D1IDPAES3
336500000000     C*
336600170908R2657C                   Eval      WStato4  = D1Stato4
336700170908R2657C                   Eval      WTpN4    = D1TpN4
336800000000     C                   Eval      WIDPAES4 = D1IDPAES4
336900000000     C                   Eval      WPIVA4   = D1PIVA4
337000000000     C*
337100000000     C                   Eval      WDENOM4  = D1DENOM4
337200000000     C                   Eval      WNOME4   = D1NOME4
337300000000     C                   Eval      WCOGNOM4 = D1COGNOM4
337400000000     C* preparo i parametri di chiamata al driver
337500000000     C                   Clear                   XMPEDS
337600000000     C                   EVAL      TpChiamata = '03'
337700000000     C                   EVAL      NChiavi = 5
337800000000     C                   EVAL      VLogica = 2
337900000000     C*
338000170905R2654X***                Do        4             $Cont
338100170905R2654C                   Do        $DimSk        $Cont
338200000000     C                   Clear                   XMPEDS1
338300000000     C                   Eval      DSMPEPGM = 'SDGSI74R' +
338400000000     C                                        %char($cont)
338500000000     C* preparo le chiavi per la chiamata al driver
338600000000     C                   MoveL     XSCSOC        DSMPESOC
338700000000     C                   MoveL     *Blanks       DSMPEPRF
338800000000     C* Si reperiscono i dati di default a seconda delle modalità con
338900000000     C* cui il pgm è usato
339000000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
339100000000     C* MPETIP = 'PV'  pgm richiamato da menù
339200000000     C* MPETIP = 'PB'  x lancio Batch
339300000000     C                   MoveL     'PV'          DSMPETIP
339400000000     C*
339500000000     C                   select
339600000000     C                   When      $Cont   = 1
339700000000     C                   MoveL     DsFrontesp1   DSMPEPAR
339800000000     C                   When      $Cont   = 2
339900000000     C                   MoveL     DsFrontesp2   DSMPEPAR
340000000000     C                   When      $Cont   = 3
340100000000     C                   MoveL     DsFrontesp3   DSMPEPAR
340200000000     C                   When      $Cont   = 4
340300000000     C                   MoveL     DsFrontesp4   DSMPEPAR
340400170905R2654C                   When      $Cont   = 5
340500170905R2654C                   MoveL     DsFrontesp41  DSMPEPAR
340600000000     C                   EndSl
340700000000     C*
340800000000     C                   CALLB     'XMPE'
340900000000     C                   PARM                    XMPEDS
341000000000     C                   PARM                    XMPEDS1
341100000000     C*
341200000000     C                   Enddo
341300000000     C*
341400000000     C                   If        Risultato = '1'
341500000000     C                   Seton                                        22
341600000000     C                   Endif
341700000000     C*
341800000000     C                   ENDSR
341900000000     C/EJECT
342000000000     C************************************************************
342100000000     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
342200000000     C************************************************************
342300000000     C     GESAGG        BEGSR
342400000000     C*
342500000000     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
342600000000    >C     CMTI74        IFEQ      '0'
342700000000     C                   COMMIT
342800000000     C                   ENDIF
342900000000     C*
343000000000     C* segnalo al pgm chiamante l'avvenuta conferma
343100000000    >C                   MOVE      *ON           OPRI74
343200000000     C* segnalo al pgm chiamante l'aver premuto l'F6
343300000000    >C                   MOVE      '0'           RETI74
343400000000     C*
343500000000     C                   SELECT
343600000000     C*
343700000000     C* nel caso di immissione
343800000000    >C     OPZI74        WHENEQ    '01'
343900000000    >C     OPZI74        orEQ      '03'
344000000000     C* Ripasso i dati inseriti
344100000000     C                   Exsr      DATIRIP
344200000000     C* Ripasso la il numero di record appena scritto
344300000000     C                   EVAL      NRRCSTI74 = NRRCST0702
344400000000     C*
344500000000     C* ripristino la videata iniziale
344600000000     C                   MOVE      *ON           $INZD1
344700000000     C                   MOVE      'D1'          $GEST
344800000000     C*
344900000000     C* forzo il ritorno al pgm chiamante perchè devo chiamare
345000000000     C* i pgm di manutenzione del dettaglio
345100000000     C                   MOVE      *ON           $FINE
345200000000     C*
345300000000    >C                   MOVE      xscsoc        SocietaI74
345400000000     C* passare nei campi appositi della DS SDGSI74DS il valore
345500000000     C* della chiave della VL univoca
345600000000     C*
345700000000     C* nel caso di modifica
345800000000    >C     OPZI74        WHENEQ    '02'
345900000000     C* ripasso i campi che devono essere reimpostati sul sfl
346000000000     C*
346100000000     C* Ripasso i dati modificati
346200000000     C                   Exsr      DATIRIP
346300000000     C*
346400000000     C* e ritorno al pgm chiamante
346500000000     C                   MOVE      *ON           $FINE
346600000000     C*
346700000000     C* per tutte le altre opzioni
346800000000     C                   OTHER
346900000000     C* ritorno al pgm chiamante
347000000000     C                   MOVE      *ON           $FINE
347100000000     C                   ENDSL
347200000000     C*
347300000000     C                   ENDSR
347400000000     C/EJECT
347500000000     C************************************************************
347600000000     C* DATI DA RIPASSARE
347700000000     C************************************************************
347800000000     C     DATIRIP       BEGSR
347900000000     C*
348000000000     C* DATI FISCALI E SEDE
348100000000     C                   Eval      SoggI74    =  D1Sogg
348200000000     C                   Eval      PartIVAI74 =  D1Partiva
348300000000     C                   Eval      CdFiscI74  =  D1CdFisc
348400000000     C                   Eval      CfI74      =  D1CF
348500000000     C                   Eval      Stato1I74  =  D1Stato1
348600000000     C                   Eval      IdPaes1I74 =  D1IdPaes1
348700000000     C                   Eval      PIVA1I74   =  D1PIVA1
348800000000     C                   Eval      CdFisc1I74 =  D1CdFisc
348900000000     C*
349000000000     C                   Eval      CogRag1I74 =  H1CogRag1
349100000000     C                   Eval      TpN1I74    =  D1TpN1
349200000000     C                   Eval      Denom1I74  =  D1Denom1
349300000000     C                   Eval      Cognom1I74 =  D1Cognom1
349400000000     C                   Eval      Nome1I74   =  D1Nome1
349500000000     C*
349600000000     C                   Eval      Indir2I74  =  D1Indir2
349700000000     C                   Eval      NrCiv2I74  =  D1NrCiv2
349800000000     C                   Eval      CAP2I74    =  D1CAP2
349900000000     C                   Eval      Locali2I74 =  D1Locali2
350000000000     C                   Eval      Prov2I74   =  D1Prov2
350100000000     C                   Eval      Stato2I74  =  D1Stato2
350200000000     C                   Eval      IdPaes2I74 =  D1IdPaes2
350300000000     C*
350400000000     C* STABILE ORGANIZZAZIONE
350500000000     C                   Eval      Indir3I74  =  D1Indir3
350600000000     C                   Eval      NrCiv3I74  =  D1NrCiv3
350700000000     C                   Eval      CAP3I74    =  D1CAP3
350800000000     C                   Eval      Locali3I74 =  D1Locali3
350900000000     C                   Eval      Prov3I74   =  D1Prov3
351000000000     C                   Eval      Stato3I74  =  D1Stato3
351100000000     C                   Eval      IdPaes3I74 =  D1IdPaes3
351200000000     C*
351300000000     C* RAPPRESENTANTE FISCALE
351400000000     C                   Eval      Stato4I74  =  D1Stato4
351500000000     C                   Eval      IdPaes4I74 =  D1IdPaes4
351600000000     C                   Eval      PIVA4I74   =  D1PIVA4
351700000000     C*
351800000000     C                   Eval      CogRag4I74 =  H1CogRag4
351900000000     C                   Eval      TpN4I74    =  D1TpN4
352000000000     C                   Eval      Denom4I74  =  D1Denom4
352100000000     C                   Eval      Cognom4I74 =  D1Cognom4
352200000000     C                   Eval      Nome4I74   =  D1Nome4
352300000000     C*
352400000000     C                   ENDSR
352500000000     C/EJECT
352600000000     C************************************************************
352700000000     C* CONTROLLO ABILITAZIONE TASTI
352800000000     C************************************************************
352900000000     C     CTRKEY        BEGSR
353000000000     C*
353100000000     C                   SELECT
353200000000     C     $TASTO        WHENEQ    F01
353300000000     C                   Z-ADD     01            PosFun
353400000000     C     $FC(1)        IFEQ      '0'
353500000000     C                   SETON                                        99
353600000000     C                   ENDIF
353700000000     C     $TASTO        WHENEQ    F02
353800000000     C                   Z-ADD     02            PosFun
353900000000     C     $FC(2)        IFEQ      '0'
354000000000     C                   SETON                                        99
354100000000     C                   ENDIF
354200000000     C     $TASTO        WHENEQ    F03
354300000000     C                   Z-ADD     03            PosFun
354400000000     C     $FC(3)        IFEQ      '0'
354500000000     C                   SETON                                        99
354600000000     C                   ENDIF
354700000000     C     $TASTO        WHENEQ    F04
354800000000     C                   Z-ADD     04            PosFun
354900000000     C     $FC(4)        IFEQ      '0'
355000000000     C                   SETON                                        99
355100000000     C                   ENDIF
355200000000     C     $TASTO        WHENEQ    F05
355300000000     C                   Z-ADD     05            PosFun
355400000000     C     $FC(5)        IFEQ      '0'
355500000000     C                   SETON                                        99
355600000000     C                   ENDIF
355700000000     C     $TASTO        WHENEQ    F06
355800000000     C                   Z-ADD     06            PosFun
355900000000     C     $FC(6)        IFEQ      '0'
356000000000     C                   SETON                                        99
356100000000     C                   ENDIF
356200000000     C     $TASTO        WHENEQ    F07
356300000000     C                   Z-ADD     07            PosFun
356400000000     C     $FC(7)        IFEQ      '0'
356500000000     C                   SETON                                        99
356600000000     C                   ENDIF
356700000000     C     $TASTO        WHENEQ    F08
356800000000     C                   Z-ADD     08            PosFun
356900000000     C     $FC(8)        IFEQ      '0'
357000000000     C                   SETON                                        99
357100000000     C                   ENDIF
357200000000     C     $TASTO        WHENEQ    F09
357300000000     C                   Z-ADD     09            PosFun
357400000000     C     $FC(09)       IFEQ      '0'
357500000000     C                   SETON                                        99
357600000000     C                   ENDIF
357700000000     C     $TASTO        WHENEQ    F10
357800000000     C                   Z-ADD     10            PosFun
357900000000     C     $FC(10)       IFEQ      '0'
358000000000     C                   SETON                                        99
358100000000     C                   ENDIF
358200000000     C     $TASTO        WHENEQ    F11
358300000000     C                   Z-ADD     11            PosFun
358400000000     C     $FC(11)       IFEQ      '0'
358500000000     C                   SETON                                        99
358600000000     C                   ENDIF
358700000000     C     $TASTO        WHENEQ    F12
358800000000     C                   Z-ADD     12            PosFun
358900000000     C     $FC(12)       IFEQ      '0'
359000000000     C                   SETON                                        99
359100000000     C                   ENDIF
359200000000     C     $TASTO        WHENEQ    F13
359300000000     C                   Z-ADD     13            PosFun
359400000000     C     $FC(13)       IFEQ      '0'
359500000000     C                   SETON                                        99
359600000000     C                   ENDIF
359700000000     C     $TASTO        WHENEQ    F14
359800000000     C                   Z-ADD     14            PosFun
359900000000     C     $FC(14)       IFEQ      '0'
360000000000     C                   SETON                                        99
360100000000     C                   ENDIF
360200000000     C     $TASTO        WHENEQ    F15
360300000000     C                   Z-ADD     15            PosFun
360400000000     C     $FC(15)       IFEQ      '0'
360500000000     C                   SETON                                        99
360600000000     C                   ENDIF
360700000000     C     $TASTO        WHENEQ    F16
360800000000     C                   Z-ADD     16            PosFun
360900000000     C     $FC(16)       IFEQ      '0'
361000000000     C                   SETON                                        99
361100000000     C                   ENDIF
361200000000     C     $TASTO        WHENEQ    F17
361300000000     C                   Z-ADD     17            PosFun
361400000000     C     $FC(17)       IFEQ      '0'
361500000000     C                   SETON                                        99
361600000000     C                   ENDIF
361700000000     C     $TASTO        WHENEQ    F18
361800000000     C                   Z-ADD     18            PosFun
361900000000     C     $FC(18)       IFEQ      '0'
362000000000     C                   SETON                                        99
362100000000     C                   ENDIF
362200000000     C     $TASTO        WHENEQ    F19
362300000000     C                   Z-ADD     19            PosFun
362400000000     C     $FC(19)       IFEQ      '0'
362500000000     C                   SETON                                        99
362600000000     C                   ENDIF
362700000000     C     $TASTO        WHENEQ    F20
362800000000     C                   Z-ADD     20            PosFun
362900000000     C     $FC(20)       IFEQ      '0'
363000000000     C                   SETON                                        99
363100000000     C                   ENDIF
363200000000     C     $TASTO        WHENEQ    F21
363300000000     C                   Z-ADD     21            PosFun
363400000000     C     $FC(21)       IFEQ      '0'
363500000000     C                   SETON                                        99
363600000000     C                   ENDIF
363700000000     C     $TASTO        WHENEQ    F22
363800000000     C                   Z-ADD     22            PosFun
363900000000     C     $FC(22)       IFEQ      '0'
364000000000     C                   SETON                                        99
364100000000     C                   ENDIF
364200000000     C     $TASTO        WHENEQ    F23
364300000000     C                   Z-ADD     23            PosFun
364400000000     C     $FC(23)       IFEQ      '0'
364500000000     C                   SETON                                        99
364600000000     C                   ENDIF
364700000000     C     $TASTO        WHENEQ    F24
364800000000     C                   Z-ADD     24            PosFun
364900000000     C     $FC(24)       IFEQ      '0'
365000000000     C                   SETON                                        99
365100000000     C                   ENDIF
365200000000     C                   ENDSL
365300000000     C*
365400000000     C                   ENDSR
365500000000     C/EJECT
365600000000     C******************************************************
365700000000     C* PGM PER GESTIONE MESSAGGI ERRORE
365800000000     C******************************************************
365900000000     C     MSGPGM        BEGSR
366000000000     C*
366100000000     C                   Z-ADD     2             MSGMSG
366200000000     C                   MOVEL     'S'           MSGRSP
366300000000     C                   MOVEL     'CR'          MSGTPR
366400000000     C                   MOVEL     'S'           MSGEMV
366500000000     C                   Z-ADD     10            MSGRGP
366600000000     C                   MOVEL     'S'           MSGLCK
366700000000     C                   MOVEA     STATUS        STS
366800000000     C                   MOVEL     'N'           MSGCNL
366900000000     C                   MOVEL     'N'           MSGVID
367000000000     C                   MOVEL     'N'           MSGOPE
367100000000     C                   MOVEL     'N'           MSGSTP
367200000000     C                   CALL      'SDGMSGR'
367300000000     C                   PARM                    SDGMSGDS
367400000000     C*
367500000000     C                   ENDSR
367600000000     C/EJECT
367700000000     C************************************************************
367800000000     C* OPERAZIONI INIZIALI
367900000000     C************************************************************
368000000000     C     *INZSR        BEGSR
368100000000     C*
368200000000     C* Reperimento parametri
368300000000     C*
368400000000     C     *ENTRY        PLIST
368500000000     C                   PARM                    KPJBA
368600000000     C                   PARM                    SDGSI74DS
368700000000     C*
368800000000     C* Reperimento dati società
368900000000     C*
369000000000     C                   MOVEL     'SOC001'      TIPXSC
369100000000     C                   MOVEL     *BLANK        SOCXSC
369200000000     C                   EXSR      REPSOC
369300000000     C*
369400000000     C* Controlli dati società
369500000000     C*
369600000000     C     RTNXSC        IFNE      '1'
369700000000     C                   MOVEL     XSOCDS        SOC001
369800000000     C                   ELSE
369900000000    >C                   MOVE      '1'           ErrI74
370000000000     C                   MOVE      *on           $Fine
370100000000     C                   EXSR      EndPgm
370200000000     C                   ENDIF
370300000000     C*
370400000000     C                   MOVE      *BLANK        AUTDS
370500000000     C*
370600000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
370700000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
370800000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
370900000000     C*
371000000000     C* Reperimento tasti di funzione
371100000000     C*
371200000000     C                   EXSR      REPKEY
371300000000     C*
371400000000     C* Richiamo routine per aggiunta tasti funzione personalizzati
371500000000     C                   CAllb     'X46USREXT'
371600000000     C                   PARM                    *omit
371700000000     C                   PARM                    *omit
371800000000     C                   PARM                    *omit
371900000000     C                   PARM                    *omit
372000000000     C                   PARM                    *omit
372100000000     C                   PARM                    $FX1
372200000000     C                   PARM                    $FC1
372300000000     C                   PARM                    $PGF1
372400000000     C                   PARM                    DsPgm
372500000000     C                   PARM                    xsccut
372600000000     C                   PARM                    xscgrp
372700000000     C*
372800000000     C     *DMY          Move      UDate         UDateISO
372900000000     C*
373000000000     C* reperisco DATFMT del job
373100000000     C                   CallB     'XDATFMT'
373200000000     C                   Parm                    $DatFmt           4
373300000000     C*
373400000000     C                   ENDSR
373500000000     C/EJECT
373600000000     C************************************************************
373700000000     C* REPERIMENTO TASTI DI FUNZIONE
373800000000     C************************************************************
373900000000     C     REPKEY        BEGSR
374000000000     C*
374100000000     C                   MOVEA     $FM1          $FM
374200000000     C                   MOVEA     $FC1          $FC
374300000000     C                   EXSR      REPKKK
374400000000     C                   MOVEA     $FX           $FX1
374500000000     C*
374600000000     C                   ENDSR
374700000000     C/EJECT
374800000000     C************************************************************
374900000000     C* REPERIMENTO TASTI DI FUNZIONE
375000000000     C************************************************************
375100000000     C     REPKKK        BEGSR
375200000000     C*
375300000000     C                   CLEAR                   $FX
375400000000     C     1             DO        24            X
375500000000     C*
375600000000     C     $FM(X)        IFNE      *BLANK
375700000000     C     $FC(X)        ANDNE     '0'
375800000000     C*
375900000000     C                   MOVEL     $FM(X)        IDMSG
376000000000     C                   CALLB     'XRTVM'
376100000000     C                   PARM                    IDMSG            27
376200000000     C                   PARM                    TXTMSG          100
376300000000     C                   PARM                    ERRMSG            1
376400000000     C*
376500000000     C     ERRMSG        IFNE      *ON
376600000000     C                   MOVEL     TXTMSG        $FX(X)
376700000000     C                   ELSE
376800000000     C                   MOVEL     *ALL' '       $FX(X)
376900000000     C                   ENDIF
377000000000     C*
377100000000     C                   ENDIF
377200000000     C*
377300000000     C                   ENDDO
377400000000     C*
377500000000     C                   ENDSR
377600000000     C/EJECT
377700000000     C************************************************************
377800000000     C* INIZIALIZZAZIONE VARIABILI
377900000000     C************************************************************
378000000000     C     INZVAR        BEGSR
378100000000     C*
378200000000     C* Attiva Commitment control se necessario
378300000000     C                   IF        CMTI74      = '0'
378400000000    >C                   CALLB     'XSTRCMT'
378500000000     C                   IF        NRRCSTI74 > 0
378600000000    >C                   OPEN      SDGCST00F
378700000000     C                   ELSE
378800000000    >C                   OPEN      SDGCST02L
378900000000     C                   ENDIF
379000000000     C                   ENDIF
379100000000     C*
379200000000     C* Dati da PGM guida
379300000000    >C*****              MOVEL     KPJBU         SDGSI74DS
379400000000     C*
379500000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
379600000000     C* reperimento dati società
379700000000    >C                   If        SocietaI74 <> xscsoc    and
379800000000    >C                             OpzI74 <> *blank
379900000000     C                   MOVEL     'SOC001'      TIPXSC
380000000000    >C                   MOVEL     SocietaI74    SOCXSC
380100000000     C                   EXSR      REPSOC
380200000000     C     RTNXSC        IFNE      '1'
380300000000     C                   MOVEL     XSOCDS        SOC001
380400000000     C                   ELSE
380500000000    >C                   MOVE      '1'           ErrI74
380600000000     C                   MOVE      *on           $Fine
380700000000     C                   EXSR      EndPgm
380800000000     C                   ENDIF
380900000000     C                   endif
381000000000     C*
381100000000     C* Reperimento autorizzazioni
381200000000     C*
381300000000     C* le autorizzazioni potrebbero essere già state passate,
381400000000     C* nel qual caso non vanno reperite
381500000000    >C     AUTDS         IFEQ      *BLANK
381600000000    >C                   EXSR      REPAUT
381700000000    >C                   ELSE
381800000000    >C                   MOVEL     XCADDS        CONCIVDS
381900000000    >C                   ENDIF
382000000000     C*
382100000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
382200000000     C* le autorizzazione
382300000000     C*
382400000000    >C                   EXSR      TSTAUT
382500000000     C*
382600000000     C* Definizione dei tasti funzione utilizzabili a pgm
382700000000     C*
382800000000     C                   EXSR      DEFOPZ
382900000000     C*
383000000000     C* Variabili per gestione videate
383100000000     C*
383200000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
383300000000     C                   MOVE      *OFF          $FINE                          fine pgm
383400000000     C                   MOVE      *OFF          $FlgDef                        fine pgm
383500000000     C                   MOVE      *ON           $INZD1                         inizializzione D1
383600000000     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
383700000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
383800000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
383900000000     C* azzero la posizione del cursore
384000000000     C                   Z-ADD     0             H1RIGA
384100000000     C                   Z-ADD     0             H1COLO
384200000000     C*
384300000000     C* Variabili appoggio
384400000000     C*
384500000000     C                   Z-ADD     0             CURR
384600000000     C                   Z-ADD     0             CURC
384700000000     C*
384800000000     C* Valorizzazione campi univoci testate
384900000000     C*
385000000000     C                   CLEAR                   FMTT1
385100000000     C                   MOVEL     KNSIF         NOMSIF
385200000000     C                   MOVEL     XSCDSI        NOMDIT
385300000000     C                   MOVEL     DSPGM         NOMPGM           10
385400000000     C                   MOVEL     KCDAZ         NOMAZN
385500000000     C* recupero il sottotitolo (se c'è)
385600000000     C                   MOVEL     *BLANK        T1TIT
385700000000     C                   if        OpzI74      = '20'
385800000000     C                   Eval      $Sottit     = SottitSoc
385900000000     C                   else
386000000000     C                   Eval      $Sottit     = SottitSog
386100000000     C                   endif
386200000000     C     $Sottit       IFNE      *BLANK
386300000000     C                   MOVEL     $Sottit       IDMSG
386400000000     C                   CALLB     'XRTVM'
386500000000     C                   PARM                    IDMSG            27
386600000000     C                   PARM                    TXTMSG          100
386700000000     C                   PARM                    ERRMSG            1
386800000000     C                   PARM      *ON           CTRMSG            1            centratura
386900000000     C                   PARM      30            LENMSG            3 0          lun output.
387000000000     C     ERRMSG        IFNE      *ON
387100000000     C                   MOVEL     TXTMSG        T1TIT
387200000000     C                   ELSE
387300000000     C                   MOVEL     *ALL'?'       T1TIT
387400000000     C                   ENDIF
387500000000     C                   ENDIF
387600000000     C* recupero il tipo operazione
387700000000     C                   Z-ADD     1             X
387800000000    >C     OPZI74        LOOKUP    $OZ(X)                                 21
387900000000     C     *IN21         IFEQ      *ON
388000000000     C                   MOVEL     $ID(X)        IDMSG
388100000000     C                   CALLB     'XRTVM'
388200000000     C                   PARM                    IDMSG            27
388300000000     C                   PARM                    TXTMSG          100
388400000000     C                   PARM                    ERRMSG            1
388500000000     C                   PARM      *Off          CTRMSG            1            centratura
388600000000     C                   PARM      15            LENMSG            3 0          lun output.
388700000000     C     ERRMSG        IFNE      *ON
388800000000     C                   MOVEL     TXTMSG        T1OPE
388900000000     C                   ELSE
389000000000     C                   MOVEL     *ALL'?'       T1OPE
389100000000     C                   ENDIF
389200000000     C                   ENDIF
389300000000     C*
389400000000     C* Inizializzazione/Decodifica prima videata
389500000000     C                   EXSR      REPDEF
389600000000     C                   EXSR      INZD1
389700000000     C                   MOVE      *OFF          $INZD1
389800000000     C*
389900000000     C                   EXSR      RepSDGSPETRI
390000000000     C*
390100000000     C                   ENDSR
390200000000     C/EJECT
390300000000     C************************************************************
390400000000     C* REPERIMENTO DATI SOCIETA'
390500000000     C************************************************************
390600000000     C     REPSOC        BEGSR
390700000000     C*
390800000000     C                   CALL      'SDGSOCDR'
390900000000     C                   PARM                    TIPXSC            6
391000000000     C                   PARM                    SOCXSC            3
391100000000     C                   PARM                    CDSXSC            9 0
391200000000     C                   PARM                    MODXSC            3
391300000000     C                   PARM      *BLANKS       RTNXSC            1
391400000000     C                   PARM                    XSOCDS
391500000000     C                   PARM                    KPJBA
391600000000     C*
391700000000     C                   ENDSR
391800000000     C/EJECT
391900000000     C************************************************************
392000000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
392100000000     C************************************************************
392200000000     C     REPAUT        BEGSR
392300000000     C*
392400000000     C                   CLEAR                   AUTDS
392500000000     C                   Clear                   ConCivDs
392600000000     C*
392700000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
392800000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
392900000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
393000000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
393100000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
393200000000    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
393300000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
393400000000    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
393500000000     C*
393600000000     C                   CALLB     'XCHKAUT'
393700000000     C                   PARM                    AUTDS
393800000000     C*
393900000000     C* errori/non abilitazione
394000000000     C     XCARTN        IFNE      0
394100000000    >C                   MOVE      '2'           ErrI74
394200000000     C                   MOVE      *ON           $FINE
394300000000     C                   EXSR      ENDPGM
394400000000     C                   ELSE
394500000000    >C                   MOVEL     XCADDS        CONCIVDS
394600000000     C                   ENDIF
394700000000     C*
394800000000     C                   ENDSR
394900000000     C/EJECT
395000000000     C************************************************************
395100000000     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
395200000000     C************************************************************
395300000000     C     TSTAUT        BEGSR
395400000000     C*
395500000000     C                   SELECT
395600000000     C*
395700000000     C* se richiamato in immissione
395800000000    >C     OPZI74        WHENEQ    '01'
395900000000     C* ma l'utente non è abilitato
396000000000    >C     IMMALL        IFEQ      *off
396100000000     C* esco da pgm avvertendolo
396200000000    >C                   MOVE      '2'           ERRI74
396300000000     C                   MOVE      *ON           $FINE
396400000000     C                   EXSR      ENDPGM
396500000000     C                   ENDIF
396600000000     C*
396700000000     C* se richiamato in modifica
396800000000    >C     OPZI74        WHENEQ    '02'
396900000000     C* ma l'utente non è abilitato
397000000000    >C     MANALL        IFEQ      *OFF
397100000000     C* testo se l'utente è abilitato almeno alla visualizzazione
397200000000    >C     VISALL        IFEQ      *ON
397300000000    >C                   MOVE      '05'          OPZI74
397400000000     C                   ELSE
397500000000     C* altrimenti esco da pgm avvertendolo
397600000000    >C                   MOVE      '2'           ERRI74
397700000000     C                   MOVE      *ON           $FINE
397800000000     C                   EXSR      ENDPGM
397900000000     C                   ENDIF
398000000000     C                   ENDIF
398100000000     C*
398200000000     C* se richiamato in copia
398300000000    >C     OPZI74        WHENEQ    '03'
398400000000     C* ma l'utente non è abilitato
398500000000    >C     IMMALL        IFEQ      *OFF
398600000000     C* esco da pgm avvertendolo
398700000000    >C                   MOVE      '2'           ERRI74
398800000000     C                   MOVE      *ON           $FINE
398900000000     C                   EXSR      ENDPGM
399000000000     C                   ENDIF
399100000000     C*
399200000000     C* se richiamato in annullamento
399300000000    >C     OPZI74        WHENEQ    '04'
399400000000     C* ma l'utente non è abilitato
399500000000    >C     ANNALL        IFEQ      *OFF
399600000000     C* esco da pgm avvertendolo
399700000000    >C                   MOVE      '2'           ERRI74
399800000000     C                   MOVE      *ON           $FINE
399900000000     C                   EXSR      ENDPGM
400000000000     C                   ENDIF
400100000000     C*
400200000000     C* se richiamato in visualizzazione
400300000000    >C     OPZI74        WHENEQ    '05'
400400000000     C* ma l'utente non è abilitato
400500000000    >C     VISALL        IFEQ      *OFF
400600000000     C* esco da pgm avvertendolo
400700000000    >C                   MOVE      '2'           ERRI74
400800000000     C                   MOVE      *ON           $FINE
400900000000     C                   EXSR      ENDPGM
401000000000     C                   ENDIF
401100000000     C*
401200000000     C                   ENDSL
401300000000     C*
401400000000     C                   ENDSR
401500000000     C/EJECT
401600000000     C************************************************************
401700000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
401800000000     C************************************************************
401900000000     C     DEFOPZ        BEGSR
402000000000     C*
402100000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1
402200000000     C* con S o N a seconda della utilizzabilità o meno
402300000000     C* del tasto funzione correlato
402400000000     C*
402500000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
402600000000     C* di compilazione
402700000000     C                   RESET                   $FC1
402800000000     C                   RESET                   $FR1
402900000000     C*
403000000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
403100000000     C* invalido alcune opzioni
403200000000     C                   SELECT
403300000000     C* Se pgm richiamato in interrogazione disattivo :
403400000000    >C     OPZI74        WHENEQ    '05'
403500000000     C* F2=Decodifica
403600000000     C                   Z-ADD     2             X
403700000000     C                   MOVE      '0'           $FC1(X)
403800000000     C* F4=Ricerca
403900000000     C                   Z-ADD     4             X
404000000000     C                   MOVE      '0'           $FC1(X)
404100000000     C* F5=Ripristino
404200000000     C                   Z-ADD     5             X
404300000000     C                   MOVE      '0'           $FC1(X)
404400000000     C* F6=Conferma
404500000000     C                   Z-ADD     06            X
404600000000     C                   MOVE      '0'           $FC1(X)
404700000000     C* Se pgm richiamato in annullamento disattivo :
404800000000    >C     OPZI74        WHENEQ    '04'
404900000000     C* F2=Decodifica
405000000000     C                   Z-ADD     2             X
405100000000     C                   MOVE      '0'           $FC1(X)
405200000000     C* F4=Ricerca
405300000000     C                   Z-ADD     4             X
405400000000     C                   MOVE      '0'           $FC1(X)
405500000000     C* F5=Ripristino
405600000000     C                   Z-ADD     5             X
405700000000     C                   MOVE      '0'           $FC1(X)
405800000000     C*
405900000000     C                   OTHER
406000000000     C*
406100000000     C                   ENDSL
406200000000     C*
406300000000     C                   ENDSR
406400000000     C/EJECT
406500000000     C************************************************************
406600000000     C* REPERIMENTO CAMPI DI DEFAULT
406700000000     C************************************************************
406800000000     C     REPDEF        BEGSR
406900000000     C*
407000000000     C                   Eval      $FlgDef     = *On
407100000000     C*
407200000000     C                   ENDSR
407300000000      /EJECT
407400000000     C************************************************************
407500000000     C* Rerimento parametri di setup per spesometro trimestrale
407600000000     C************************************************************
407700000000     C     RepSDGSPETRI  BEGSR
407800000000     C*
407900000000     C                   Clear                   SDGPA12DS
408000000000     C*
408100000000     C                   CALLB     'XPAR'
408200000000     C                   Parm                    XScSoc
408300000000     C                   Parm      'SDSPETRI'    XPaPar            8
408400000000     C                   Parm                    XPaOut         2000
408500000000     C                   Parm      *OFF          XPaErr            1
408600000000     C                   Parm      ' '           XPaRic            1
408700000000     C*
408800000000     C                   if        XPaErr      = *On
408900000000     C                   EXSR      ENDPGM
409000000000     C                   else
409100000000     C                   MoveL     XPaOut        SDGPA12DS
409200000000     C                   endif
409300000000     C*
409400000000     C                   EndSr
409500000000     C/EJECT
409600000000     C************************************************************
409700000000     C* DEFINIZIONE KLIST
409800000000     C************************************************************
409900000000     C     DefKlist      BEGSR
410000000000     C*
410100000000     C* klist
410200000000     C*
410300000000     C*
410400000000     C* ANCAP01L
410500000000     C     K02CAP01      klist
410600000000     C                   kfld                    CAPStato
410700000000     C                   kfld                    CAPCAP
410800000000     C*
410900000000     C     K03CAP01      klist
411000000000     C                   kfld                    CAPStato
411100000000     C                   kfld                    CAPCAP
411200000000     C                   kfld                    CapLocalit
411300000000     C*
411400000000     C* SDGCST02L
411500000000     C     K07CST02      KLIST
411600000000     C                   KFLD                    CSTSocieta
411700000000     C                   KFLD                    CSTTpCom
411800000000     C                   KFLD                    CSTTpRec
411900000000     C                   KFLD                    CSTAnno
412000000000     C                   KFLD                    CSTTrim
412100000000     C                   KFLD                    CSTMese
412200000000     C                   KFLD                    CSTCliFor
412300000000     C*
412400000000     C                   ENDSR
412500000000     C/EJECT
412600000000     C************************************************************
412700000000     C* RIEMPIMENTO TASTI FUNZIONE
412800000000     C************************************************************
412900000000     C     RIEKEY        BEGSR
413000000000     C*
413100000000     C* Input :
413200000000     C* - $FX = schiera con tutti i testi disponibili
413300000000     C* - $FC = schiere con i flag di validità associati ai testi
413400000000     C* - $FR = schiere con i flag di REVERSE IMAGE
413500000000     C* - $ULK = ultimo testo caricato
413600000000     C* - $L01 = Lunghezza prima riga
413700000000     C* - $L02 = Lunghezza seconda riga
413800000000     C* Output :
413900000000     C* - $ULK
414000000000     C* - $KEY1= 1° riga di tasti funzione
414100000000     C* - $KEY2= 2° riga di tasti funzione
414200000000     C*
414300000000     C                   CALLB     'XKEYMSG'
414400000000     C* input
414500000000     C                   PARM                    $FX
414600000000     C                   PARM                    $FC
414700000000     C                   PARM                    $FR
414800000000     C                   PARM                    $ULK              3 0
414900000000     C* output
415000000000     C                   PARM                    $KEY1            79
415100000000     C                   PARM                    $KEY2            79
415200000000     C* input
415300000000     C                   PARM                    $L01              3 0
415400000000     C                   PARM                    $L02              3 0
415500000000     C*
415600000000     C                   PARM                    $ALLFUNCT       480
415700000000     C*
415800000000     C                   ENDSR
415900000000** $OZ / $ID
41600000000001SDGMSG    *LIBL     SDC0002
41610000000002PROMSG    *LIBL     COS0002
41620000000003PROMSG    *LIBL     COS0003
41630000000004PROMSG    *LIBL     COS0004
41640000000005PROMSG    *LIBL     COS0005
416500000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
416600000000PROMSG    *LIBL     KEY0001  01
416700000000PROMSG    *LIBL     KEY0002  02
416800000000PROMSG    *LIBL     KEY0003  03
416900000000PROMSG    *LIBL     KEY0004  04
417000000000PROMSG    *LIBL     KEY0005  05
417100000000PROMSG    *LIBL     KEY0006  06
417200000000PROMSG    *LIBL     KEY0007  07
417300000000PROMSG    *LIBL     KEY0008  08
417400000000PROMSG    *LIBL     KEY0009  09
417500000000PROMSG    *LIBL     KEY0010  10
417600000000PROMSG    *LIBL     KEY0011  11
417700000000PROMSG    *LIBL     KEY0012  12
417800000000PROMSG    *LIBL     KEY0013  13
417900000000PROMSG    *LIBL     KEY0014  14
418000000000PROMSG    *LIBL     KEY0015  15
418100000000PROMSG    *LIBL     KEY0016  16
418200000000PROMSG    *LIBL     KEY0017  17
418300000000PROMSG    *LIBL     KEY0018  18
418400000000PROMSG    *LIBL     KEY0019  19
418500000000PROMSG    *LIBL     KEY0020  20
418600000000PROMSG    *LIBL     KEY0021  21
418700000000PROMSG    *LIBL     KEY0022  22
418800000000PROMSG    *LIBL     KEY0023  23
418900000000PROMSG    *LIBL     KEY0024  24
419000000000**  ABILITAZIONE D1 : $FC1 / $FR1
4191000000000N  01
4192000000000N  02
4193000000001N  03
4194000000001N  04
4195000000001N  05
4196000000001N  06
4197000000000N  07
4198000000001N  08
4199000000000N  09
4200000000000N  10
4201000000000N  11
4202000000001N  12
4203000000000N  13
4204000000000N  14
4205000000000N  15
4206000000000N  16
4207000000000N  17
4208000000000N  18
4209000000000N  19
4210000000000N  20
4211000000000N  21
4212000000000N  22
4213000000000N  23   Impostare a '1' se si utilizzano opzioni
4214000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
