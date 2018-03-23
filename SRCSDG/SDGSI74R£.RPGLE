000100170804     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND SDG007)
000200000000     H*PARMS CVTOPT(*NONE) ACTGRP(QILE)
000300000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*****************************************************************
000500000000      *
000600170728      *                   Spesometro trimestrale
000700170728      *                       Dati di testata
000800000000      *           ---------------------------------------
000900000000      *
001000000000     F*----------------------------------------------------*
001100130906     F* Spesometro - Movimenti estratti
001200170808     FSDGCST00F UF   E             DISK    USROPN
001300170808     F                                     COMMIT
001400170804     F* Spesometro - Movimenti estratti
001500170728     F* Key: Soc/TpCom/Anno/Trim/Mese/Tipo rec./dati anagrafici
001600170808     FSDGCST02L UF   E           K DISK    Rename(SDGCST000:SDGCST002)
001700170807     F                                     USROPN
001800170807     F                                     COMMIT
001900170803     F*------------
002000170803     F* Anagrafica codici avviamento postale
002100170803     FANCAP01L  IF   E           K DISK
002200000000     F*------------
002300170727     FSDGSI74D  CF   E             WORKSTN
002400170727     F                                     RENAME(SI74D1:FMTD1)
002500170727     F                                     RENAME(SI74T1:FMTT1)
002600170727     F                                     RENAME(SI74Z1:FMTZ1)
002700000000     F* eventuali schermi supplementari
002800170727     F                                     RENAME(SI74D2:FMTD2)
002900170727     F                                     RENAME(SI74D3:FMTD3)
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
005400170804     D SAVKPJBU        S                   Like(KPJBU) inz
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
008000170727     D* Parametri modulo SDGSPETRI
008100170727     D SDGPA12DS     E DS
008200130906     D*-------------
008300000000     D* Passaggio Parametri al PGM di manutenzione
008400170727     D SDGSI74DS     E DS
008500130906     D*-------------
008600170727     D* Parametri del PGM di aggiornamento dei movimenti estratti
008700170727     D SDG00702DS    E DS                  INZ
008800170804     D* Parametri per recpero dei dati fiscali
008900170804     D SDG00701DS    E DS                  INZ
009000170807     D*
009100170807     D SAV00701DS      S                   Like(SDG00701DS) inz
009200170807     D*-------------
009300170807     D* Parametri per ANMPE
009400170807     D XMPEDS        E DS                  INZ
009500170807     D XMPEDS1       E DS                  INZ
009600130906     D*----------
009700170731     D* Passaggio Parametri al PGM ANA801R
009800170731     D A801DS        E DS                  EXTNAME(ANA801DS)
009900170731     D  ERR801       E                     INZ('0')
010000000000     D*-------------
010100000000     D* Parametri per ricerca codice ANATB00F
010200000000     D XTABDS        E DS                  inz
010300000000     D*-------------
010400000000     D* Richiamo a SDGTABDR
010500000000     D SDGTABDS      E DS                  inz
010600000000     D*-------------
010700000000     D* Ricerca codici Stato
010800000000     D ANGG12DS      E DS
010900170727     D*-------------
011000170727     D* Ricerca codici ISO
011100170727     D SDGHD4DS      E DS
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
014400170728     D $SaveDS         S                   like(SDGSI74DS)
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
020300170808     D SOTTITSog       C                   CONST('PROMSG    *LIBL-
020400170808     D                                          COS2504')
020500170808     D SOTTITSoc       C                   CONST('SDGMSG    *LIBL-
020600170808     D                                          SDC0117')
020700170808     D $Sottit         S             27A
020800000000     D*-------------
020900000000     D* Variabili per gestione videate
021000000000     D $Gest           S              2A
021100000000     D $Fine           S              1A
021200000000     D $InzD1          S              1A
021300000000     D $LastG          S              2A
021400000000     D $LastV          S              2A
021500000000     D $FlgDef         S              1A
021600170804     D* -----------------
021700170804     D* Ds per la chiave di lettura
021800170804     D DsCliFor        DS
021900170804     D  CSTCOGRAG1
022000170804     D  CSTCF
022100170804     D  CSTPARTIVA
022200170804     D  CSTCDFISC
022300170804     D  CSTSTATO1
022400170804     D  CSTIDPAES1
022500170804     D  CSTPIVA1
022600170804     D  CSTCDFISC1
022700170804     D  CSTTPN1
022800170804     D  CSTDENOM1
022900170804     D  CSTNOME1
023000170804     D  CSTCOGNOM1
023100170804     D  CSTINDIR2
023200170804     D  CSTNRCIV2
023300170804     D  CSTCAP2
023400170804     D  CSTLOCALI2
023500170804     D  CSTPROV2
023600170804     D  CSTSTATO2
023700170804     D  CSTIDPAES2
023800170804     D  CSTINDIR3
023900170804     D  CSTNRCIV3
024000170804     D  CSTCAP3
024100170804     D  CSTLOCALI3
024200170804     D  CSTPROV3
024300170804     D  CSTSTATO3
024400170804     D  CSTIDPAES3
024500170804     D  CSTSTATO4
024600170804     D  CSTIDPAES4
024700170804     D  CSTPIVA4
024800170804     D  CSTTPN4
024900170804     D  CSTCOGRAG4
025000170804     D  CSTDENOM4
025100170804     D  CSTNOME4
025200170804     D  CSTCOGNOM4
025300170804     D* Ds key video
025400170804     D DsCliForV       DS
025500170804     D  H1COGRAG1
025600170804     D  D1CF
025700170804     D  D1PARTIVA
025800170804     D  D1CDFISC
025900170804     D  D1STATO1
026000170804     D  D1IDPAES1
026100170804     D  D1PIVA1
026200170804     D  D1CDFISC1
026300170804     D  D1TPN1
026400170804     D  D1DENOM1
026500170804     D  D1NOME1
026600170804     D  D1COGNOM1
026700170804     D  D1INDIR2
026800170804     D  D1NRCIV2
026900170804     D  D1CAP2
027000170804     D  D1LOCALI2
027100170804     D  D1PROV2
027200170804     D  D1STATO2
027300170804     D  D1IDPAES2
027400170804     D  D1INDIR3
027500170804     D  D1NRCIV3
027600170804     D  D1CAP3
027700170804     D  D1LOCALI3
027800170804     D  D1PROV3
027900170804     D  D1STATO3
028000170804     D  D1IDPAES3
028100170804     D  D1STATO4
028200170804     D  D1IDPAES4
028300170804     D  D1PIVA4
028400170804     D  D1TPN4
028500170804     D  H1COGRAG4
028600170804     D  D1DENOM4
028700170804     D  D1NOME4
028800170804     D  D1COGNOM4
028900170807     D*
029000170804     D  SavCliFor      S                   Like(dsclifor) inz
029100170807     D*
029200170807     D* DS DA ANMPE
029300170807     D DsFrontesp1     DS                  INZ
029400170808     D WStato1                             Like(CSTStato1 )
029500170808     D WIdPaes1                            Like(CSTIdPaes1)
029600170807     D WPIva1                              like(CstPiva1)
029700170807     D WCdFisc1                            like(CstCdFisc1)
029800170808     D WCogRag1                            like(CstCogRag1)
029900170808     D WTpN1                               like(CstTpN1   )
030000170808     D WDenom1                             like(CstDenom1 )
030100170807     D WCognom1                            like(CstCognom1)
030200170807     D WNome1                              like(CstNome1  )
030300170807     D DsFrontesp2     DS                  INZ
030400170807     D WINDIR2                             like(CstINDIR2 )
030500170807     D WNRCIV2                             like(CstNRCIV2 )
030600170807     D WCAP2                               like(CstCAP2   )
030700170807     D WLOCALI2                            like(CstLOCALI2)
030800170807     D WPROV2                              like(CstPROV2  )
030900170808     D WStato2                             Like(CSTStato2 )
031000170807     D WIDPAES2                            like(CstIDPAES2)
031100170807     D* Stabile Organizzazione
031200170807     D DsFrontesp3     DS                  INZ
031300170807     D WINDIR3                             like(CstINDIR3 )
031400170807     D WNRCIV3                             like(CstNRCIV3 )
031500170807     D WCAP3                               like(CstCAP3   )
031600170807     D WLOCALI3                            like(CstLOCALI3)
031700170807     D WPROV3                              like(CstPROV3  )
031800170808     D WStato3                             Like(CSTStato3 )
031900170807     D WIDPAES3                            like(CstIDPAES3)
032000170807     D* Rappresentante Fiscale
032100170807     D DsFrontesp4     DS                  INZ
032200170808     D WStato4                             Like(CSTStato4 )
032300170807     D WIDPAES4                            like(CstIDPAES4)
032400170807     D WPIVA4                              like(CstPIVA4  )
032500170808     D WCogRag4                            like(CstCogRag4)
032600170808     D WTpN4                               like(CstTpN4   )
032700170807     D WDenom4                             like(CstDenom4 )
032800170807     D WCognom4                            like(CstCognom4)
032801170905R265 X**** WNome4                              like(CstNome4  )
032802170905  "  D* Rappresentante Fiscale
032803170905  "  D DsFrontesp41    DS                  INZ
032804170905R265 D WNome4                              like(CstNome4  )
033000000000     D*-------------
033100000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
033200000000     D CurR            S              3S 0
033300000000     D CurC            S              3S 0
033400000000     D Win             S             99A
033500000000     D*-------------
033600000000     D* Variabili appoggio specifiche della singola anagrafica
033700000000     D*  Indici
033800000000     D X               S              5S 0
033900000000     D Y               S              5S 0
034000000000     D* Indice della schiera tasti funzione
034100000000     D PosFun          S              2  0
034200000000     D*-------------
034300000000     D* Campi di lavoro
034400130930     D WIn16           S              1
034500130926     D WIn17           S              1
034600000000     D UDateISO        S               D
034700000000     D WDtNasc         S               D
034800000000     D WSogg           S                   Like(DVSSogg)
034900000000     D WAnnoAlfa       S              4A
035000000000     D WDtAlfa         S             10A
035100170807     D WData           S               D
035200170728     D*------
035300170728     D $Partiva        S                    Like(CstPartiva)
035400170728     D $Cdfisc         S                    Like(CstCdFisc)
035500170801     D $Stato          S                    Like(CstStato1)
035600170801     D $StatoD         S                    Like(D1Stato1D)
035700170801     D $Idpaese        S                    Like(CstIdPaes1)
035800170728     D $Tpn            S                    Like(CstTpn1)
035900170804     D $Tpnd           S                    Like(D1Tpn1D)
036000170801     D $Denom          S                    Like(D1Denom1)
036100170801     D $Nome           S                    Like(D1Nome1)
036200170801     D $Cognom         S                    Like(D1Cognom1)
036300170728     D $Cap            S                    Like(CstCap2)
036400170804     D $Prv            S                    Like(CstProv2)
036500170804     D $PrvD           S                    Like(D1Prov2D)
036600170731     D $Locali         S                    Like(CstLocali2)
036700170804     D $Indir          S                    Like(CstIndir2)
036800170801     D $ErrCtrl        S              1
036900170801     D $TipErr         S              1
037000170802     D $Errscan        S              1
037100170803     D $NrCar          S              9S 0
037200170807     D $CtrObbl        S              1
037300170807     D*---------
037400170807     D $Trovato        S              1
037500170807     D $Cont           S              1S 0
037501170905R265 D $DimSk          C                   Const(5)
037600170807     D*---------
037700170807     D DataDs          DS
037800170807     D  AnnoDs                        4A
037900170807     D   Tratt1                       1A   Inz('-')
038000170807     D  MeseDs                        2A
038100170807     D   Tratt2                       1A   Inz('-')
038200170807     D  GiornoDs                      2A
038300170807     D*----------
038400170807     D $Data10A1       S             10
038500170807     D $Data10A2       S             10
038600170807     D $DTrif          S                    Like(DtRif0701) inz
038700000000     D*------
038800000000     D $Esito          S              1
038900000000     D $Prenot         S              1
039000170728     D $Sys            S                    Like(CSTSys    )
039100170728     D $NrAsReg        S                    Like(CSTNrAsReg)
039200130918     D*-------------
039300130918     D WStato          S                   Like(XScSta )
039400130918     D $CodUIC         S                   Like(CODUICG12)
039500130918     D $UICSoc         S                   Like(CODUICG12)
039600130918     D $NazSoc         S                   Like(NazG12 )
039700130918     D $NazISO         S                   Like(NazG12 )
039800131001     D $FineG12        S              1    Inz(*Off)
039900170801     D*---------
040000170801     D* Lista caratteri da convertire e ammessi (Code Table Basic Latin) ASCII
040100170807     D CnvErr          C                   Const('ÁÃÉÍÑÓÚ[ÀÈÒÙÌ&')
040200170807     D CnvOk           C                   Const('AAEINOU AEOUIE')
040300170807     D CharOK          C                   const('ABCDEFGHIJKLMNOPQRSTUVWXYZ-
040400170807     D                                            abcdefghijklmnopqrstuvwxyz-
040500170807     D                                            01234567890/+-?:().,'''' ')
040600170801     D $FineScan       S              1    Inz(*Off)
040700170801     D $CampoScan      S            100A
040800000000     C*----------------------------------------------------*
040900000000     C*                MAIN LINE PROGRAM
041000000000     C*----------------------------------------------------*
041100000000     C*
041200000000     C* inizializzazione variabili
041300000000     C                   EXSR      INZVAR
041400000000     C*
041500000000     C     $FINE         DOWEQ     *OFF
041600000000     C     $GEST         CASEQ     'D1'          GESD1
041700000000     C     $GEST         CASEQ     'D2'          GESD2
041800170728     C     $GEST         CASEQ     'D3'          GESD3
041900000000     C                   ENDCS
042000000000     C                   ENDDO
042100000000     C*
042200000000     C* fine programma
042300000000     C                   EXSR      ENDPGM
042400000000     C/EJECT
042500000000     C************************************************************
042600000000     C* Fine Programma
042700000000     C************************************************************
042800000000     C     ENDPGM        BEGSR
042900000000     C*
043000000000     C* se non esco per errore
043100170728    >C     ErrI74        IFEQ      '0'
043200000000     C*
043300000000     C* se il COMMIT non è demandato al pgm chiamante
043400170728    >C     CMTI74        IFEQ      '0'
043500000000     C*
043600000000     C* test se esistono transazioni attive ed eventualmente cosa fare
043700000000     C                   Z-ADD     0             EXECOD
043800000000     C                   CALL      'XRCMTI'
043900000000     C                   PARM                    EXECOD            1 0
044000000000     C*
044100000000     C                   SELECT
044200000000     C* Annulla richiesta
044300000000     C     EXECOD        WHENEQ    1
044400000000     C                   MOVEL     *OFF          $FINE
044500000000     C                   MOVEL     *HIVAL        $LASTV
044600000000     C* Conferma uscita
044700000000     C     EXECOD        WHENEQ    2
044800000000     C*                  ROLBK
044900000000     C*
045000000000     C                   ENDSL
045100000000     C*
045200000000     C                   ENDIF
045300000000     C*
045400000000     C                   ENDIF
045500000000     C*
045600000000     C* se ho confermato l'uscita proseguo
045700000000     C     $FINE         IFEQ      *ON
045800000000     C*
045900000000     C* disallocazione rcd nel caso di modifica/annullamento
046000170728    >C     OPZI74        IFEQ      '02'
046100170728    >C     OPZI74        OREQ      '04'
046200000000     C*
046300000000     C* chiudo in LR per disallocare record
046400170809     C********           SETON                                            LR
046500000000     C                   Else
046600170809     C********           SETON                                            RT
046700000000     C                   ENDIF
046800000000     C*
046900170809     C* chiudo in LR per disallocare record
047000170809     C                   SETON                                            LR
047100000000     C                   RETURN
047200000000     C*
047300000000     C                   ENDIF
047400000000     C*
047500000000     C                   ENDSR
047600000000     C/EJECT
047700000000     C************************************************************
047800000000     C* RICERCHE
047900000000     C************************************************************
048000000000     C     SEARCH        BEGSR
048100000000     C*
048200000000     C* determino Riga/Colonna del cursore
048300000000     C                   MOVE      NRG           R$$
048400000000     C                   MOVE      NCL           C$$
048500000000     C                   Z-ADD     RIRI          CURR
048600000000     C                   Z-ADD     COCO          CURC
048700000000     C*
048800000000     C                   SELECT
048900000000     C*
049000000000     C*  Soggetto
049100000000     C     H1NMFL        WHENEQ    'D1SOGG'
049200000000     C                   EXSR      F4_Sogg
049300170728     C*
049400170728     C*  Natura soggetto
049500170728     C     H1NMFL        WHENEQ    'D1CF'
049600170728     C                   EXSR      F4_CF
049700170728     C*
049800170728     C*  Partita IVA
049900170728     C     H1NMFL        WHENEQ    'D1PARTIVA'
050000170728     C                   Movel     D1PartIva     $PartIva
050100170728     C                   EXSR      F4_PartIva
050200170728     C                   Movel     $Partiva      D1PartIva
050300170728     C*
050400170728     C*  Codice fiscale
050500170728     C     H1NMFL        WHENEQ    'D1CDFISC'
050600170728     C                   Movel     D1CdFisc      $CdFisc
050700170728     C                   EXSR      F4_CdFisc
050800170728     C                   Movel     $CdFisc       D1CdFisc
050900170728     C*
051000170801     C*  Stato1
051100170801     C     H1NMFL        WHENEQ    'D1STATO1 '
051200170801     C                   Movel     D1Stato1      $Stato
051300170801     C                   EXSR      F4_Stato
051400170801     C                   Movel     $Stato        D1Stato1
051500170807     C                   Movel     $IdPaese      D1IdPaes1
051600170807     C                   Movel     $Statod       D1Stato1D
051700170807     C*
051800170728     C*  Partita IVA 1
051900170728     C     H1NMFL        WHENEQ    'D1PIVA1  '
052000170728     C                   Movel     D1PIva1       $PartIva
052100170728     C                   EXSR      F4_PartIva
052200170728     C                   Movel     $PartIva      D1PIva1
052300170728     C*
052400170728     C*  Codice fiscale
052500170728     C     H1NMFL        WHENEQ    'D1CDFISC1'
052600170728     C                   Movel     D1CdFisc1     $CdFisc
052700170728     C                   EXSR      F4_CdFisc
052800170728     C                   Movel     $CdFisc       D1CdFisc1
052900130911     C*
053000130911     C*  Tipo natura giuridica
053100170728     C     H1NMFL        WHENEQ    'D1TPN1'
053200170728     C                   Movel     D1Tpn1        $Tpn
053300170728     C                   EXSR      F4_TPN
053400170804     C                   Movel     $Tpn          D1Tpn1
053500170801     C*
053600170801     C* SEDE
053700170801     C*
053800170801     C*  Stato 2
053900170728     C     H1NMFL        WHENEQ    'D1STATO2'
054000170728     C                   Movel     D1Stato2      $Stato
054100000000     C                   EXSR      F4_Stato
054200170728     C                   Movel     $Stato        D1Stato2
054300170807     C                   Movel     $IdPaese      D1IdPaes2
054400170807     C                   Movel     $Statod       D1Stato2D
054500000000     C*
054600170801     C*  provincia 2
054700170804     C     H1NMFL        WHENEQ    'D1PROV2  '
054800170804     C                   Movel     D1Prov2       $Prv
054900170728     C                   Movel     D1Stato2      $Stato
055000170728     C                   EXSR      F4_Prv
055100170801     C                   If        $Prv <> *blanks
055200170804     C                   Movel     $Prv          D1Prov2
055300170801     C                   Movel     $Stato        D1Stato2
055400170728     C                   Endif
055500170731     C*
055600170731     C*  Cap
055700170731     C     H1NMFL        WHENEQ    'D1CAP2   '
055800170731     C                   Movel     D1Cap2        $CAP
055900170731     C                   Movel     D1Stato2      $Stato
056000170731     C                   EXSR      F4_Cap
056100170731     C                   If        $Cap <> *blanks
056200170731     C                   Movel     $Cap          D1Cap2
056300170731     C                   Movel     $Stato        D1Stato2
056400170804     C                   Movel     $Prv          D1Prov2
056500170731     C                   Movel     $Locali       D1Locali2
056600170731     C                   Endif
056700000000     C*
056800170801     C* STABILE ORGANIZZAZIONE
056900170801     C*
057000170801     C*  Stato
057100170801     C     H1NMFL        WHENEQ    'D1STATO3'
057200170801     C                   Movel     D1Stato3      $Stato
057300170801     C                   EXSR      F4_Stato
057400170801     C                   Movel     $Stato        D1Stato3
057500170807     C                   Movel     $IDPaese      D1IdPaes3
057600170807     C                   Movel     $StatoD       D1Stato3D
057700170801     C*
057800170801     C*  Provincia
057900170804     C     H1NMFL        WHENEQ    'D1PROV3  '
058000170804     C                   Movel     D1Prov3       $PRV
058100170801     C                   Movel     D1Stato3      $Stato
058200170801     C                   EXSR      F4_Prv
058300170803     C                   If        $Prv <> *Blanks
058400170804     C                   Movel     $Prv          D1Prov3
058500170801     C                   Movel     $Stato        D1Stato3
058600170801     C                   Endif
058700170801     C*
058800170801     C*  Cap
058900170801     C     H1NMFL        WHENEQ    'D1CAP3   '
059000170801     C                   Movel     D1Cap3        $CAP
059100170801     C                   Movel     D1Stato3      $Stato
059200170801     C                   EXSR      F4_Cap
059300170801     C                   If        $Cap <> *blanks
059400170801     C                   Movel     $Cap          D1Cap3
059500170801     C                   Movel     $Stato        D1Stato3
059600170804     C                   Movel     $Prv          D1Prov3
059700170801     C                   Movel     $Locali       D1Locali3
059800170801     C                   Endif
059900170801     C*
060000170801     C* RAPPRESENTANTE FISCALE
060100170801     C*
060200170801     C*  Stato
060300170801     C     H1NMFL        WHENEQ    'D1STATO4'
060400170801     C                   Movel     D1Stato4      $Stato
060500170801     C                   EXSR      F4_Stato
060600170801     C                   Movel     $Stato        D1Stato4
060700170807     C                   Movel     $IDPaese      D1IdPaes4
060800170807     C                   Movel     $StatoD       D1Stato4D
060900170801     C*  Tipo natura giuridica
061000170801     C     H1NMFL        WHENEQ    'D1TPN4'
061100170801     C                   Movel     D1Tpn4        $Tpn
061200170801     C                   EXSR      F4_TPN
061300170804     C                   Movel     $Tpn          D1Tpn4
061400000000     C* ricerca non abilitata in questa posizione
061500000000     C                   OTHER
061600000000     C                   SETON                                        9899
061700000000     C*
061800000000     C                   ENDSL
061900000000     C*
062000000000     C* imposto pos. del cursore
062100000000     C                   Z-ADD     CURR          H1RIGA
062200000000     C                   Z-ADD     CURC          H1COLO
062300000000     C*
062400000000     C                   MOVE      *HIVAL        $LASTV
062500000000     C*
062600000000     C                   ENDSR
062700000000     C/EJECT
062800000000     C************************************************************
062900000000     C* Ricerca codice soggetto
063000000000     C************************************************************
063100000000     C     F4_Sogg       BEGSR
063200000000     C*
063300000000     C                   RESET                   ANA040DS
063400000000     C*
063500000000     C                   MOVEL     '01'          OPZ040
063600000000     C                   MoveL     D1Sogg        CoD040
063700000000     C                   MOVEL     ANA040DS      KPJBU
063800000000     C                   CALL      'ANA040R'
063900000000     C                   PARM                    KPJBA
064000000000     C*
064100000000     C                   MOVEL     KPJBU         ANA040DS
064200000000     C*
064300000000     C                   if        Ret040     <> '2'
064400000000     C                   MoveL     CoD040        D1Sogg
064500000000     C                   endif
064600000000     C*
064700000000     C                   ENDSR
064800000000     C/EJECT
064900170728     C************************************************************
065000170728     C* Ricerca natura del conto
065100170728     C************************************************************
065200170728     C     F4_CF         BEGSR
065300170728     C*
065400170728     C                   Reset                   ANA964DS
065500170728     C*
065600170728     C                   MoveL     '01'          Opz964
065700170728     C                   MoveL     'SDGCST000'   Trc964
065800170728     C                   MoveL     'CSTCF    '   Cam964
065900170728     C                   MoveL     *On           All964
066000170728     C*
066100170728     C                   MoveL     ANA964DS      KPJBU
066200170728     C                   CALL      'ANA964R'
066300170728     C                   PARM                    KPJBA
066400170728     C                   MoveL     KPJBU         ANA964DS
066500170728     C*
066600170728     C                   if        Ret964     <> '2'
066700170728     C                   MoveL     Val964        D1CF
066800170728     C                   endif
066900170728     C*
067000170728     C                   ENDSR
067100170728      /EJECT
067200130911     C************************************************************
067300130911     C* Ricerca tipo natura giuridica
067400130911     C************************************************************
067500170728     C     F4_TpN        BEGSR
067600130911     C*
067700130911     C                   Reset                   ANA964DS
067800130911     C*
067900130911     C                   MoveL     '01'          Opz964
068000170728     C                   MoveL     'SDGCST000'   Trc964
068100170728     C                   MoveL     'CSTTPN1   '  Cam964
068200130911     C*
068300130911     C                   MoveL     ANA964DS      KPJBU
068400130911     C                   CALL      'ANA964R'
068500130911     C                   PARM                    KPJBA
068600130911     C                   MoveL     KPJBU         ANA964DS
068700130911     C*
068800130911     C                   if        Ret964     <> '2'
068900170728     C                   MoveL     Val964        $Tpn
069000130911     C                   endif
069100130911     C*
069200130911     C                   ENDSR
069300130911     C/EJECT
069400000000     C************************************************************
069500000000     C* Ricerca codice IVA
069600000000     C************************************************************
069700000000     C     F4_PartIva    BEGSR
069800000000     C*
069900000000     C                   RESET                   ANA040DS
070000000000     C*
070100000000     C                   MOVEL     '01'          OPZ040
070200170728     C                   MoveL     $PartIva      IVA040
070300000000     C                   MoveL     D1Sogg        CoD040
070400000000     C                   MOVEL     ANA040DS      KPJBU
070500000000     C                   CALL      'ANA040R'
070600000000     C                   PARM                    KPJBA
070700000000     C*
070800000000     C                   MOVEL     KPJBU         ANA040DS
070900000000     C*
071000000000     C                   if        Ret040     <> '2'
071100000000     C*
071200000000     C                   MoveL     CoD040        WSogg
071300000000     C                   EXSR      CallDvaSog
071400000000     C*
071500000000     C                   if        DVAErrore  <> '1'
071600000000     C*
071700000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
071800000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
071900000000     C*
072000170728     C                   MoveL     DVSPartIVA    $PartIva
072100000000     C*
072200000000     C                   endif
072300000000     C*
072400000000     C                   endif
072500000000     C*
072600000000     C                   ENDSR
072700000000      /EJECT
072800000000     C************************************************************
072900000000     C* Ricerca codice fiscale
073000000000     C************************************************************
073100000000     C     F4_CdFisc     BEGSR
073200000000     C*
073300000000     C                   RESET                   ANA040DS
073400000000     C*
073500000000     C                   MOVEL     '01'          OPZ040
073600000000     C                   MoveL     D1Sogg        CoD040
073700170728     C                   MoveL     $CdFisc       CdF040
073800000000     C                   MOVEL     ANA040DS      KPJBU
073900000000     C                   CALL      'ANA040R'
074000000000     C                   PARM                    KPJBA
074100000000     C*
074200000000     C                   MOVEL     KPJBU         ANA040DS
074300000000     C*
074400000000     C                   if        Ret040     <> '2'
074500000000     C*
074600000000     C                   MoveL     CoD040        WSogg
074700000000     C                   EXSR      CallDvaSog
074800000000     C*
074900000000     C                   if        DVAErrore  <> '1'
075000000000     C*
075100000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
075200000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
075300000000     C*
075400170804     C                   MoveL     DVSCdFisc     $CdFisc
075500000000     C*
075600000000     C                   endif
075700000000     C*
075800000000     C                   endif
075900000000     C*
076000000000     C                   ENDSR
076100000000     C/EJECT
076200000000     C************************************************************
076300170801     C* Ricerca codice Stato
076400000000     C************************************************************
076500000000     C     F4_Stato      BEGSR
076600000000     C*
076700000000     C                   Reset                   XTabDS
076800000000     C*
076900000000     C                   Move      '01'          XTaOpz
077000000000     C                   Move      XScSoc        XTaAzi
077100000000     C                   Move      XScLin        XTaLin
077200000000     C                   Z-add     Curr          XTaRig
077300000000     C                   Z-add     Curc          XTaCol
077400000000     C                   Move      *Blank        XTaKey
077500000000     C*
077600000000     C                   Clear                   KPJBU
077700000000     C                   MoveL     XTabDS        KPJBU
077800000000     C*
077900170807     C                   CALL      'SDGHD4R'
078000000000     C                   PARM                    KPJBA
078100000000     C                   MoveL     KPJBU         XTabDS
078200000000     C*
078300000000     C                   if        XTaErr     <> '1'      and
078400000000     C                             XTaKey     <> *Blank
078500170728     C                   Move      XTaKey        $Stato
078600170807     C*
078700170807     C* Reperisco anche lo stato ISO
078800170807     C                   Clear                   $IdPaese
078900170807     C                   Clear                   $StatoD
079000170807     C*
079100170807     C                   Clear                   SDGHD4DS
079200170807     C                   Clear                   SDGTABDS
079300170807     C                   Eval      XTbRic      = '1'
079400170807     C                   Eval      XTbAzi      = XScSoc
079500170807     C                   Eval      XTbKey      = *Zero
079600170807     C                   Eval      XTbCod      = 'HD4'
079700170807     C                   Move      $Stato        XTbKey
079800170807     C*
079900170807     C                   CALL      'SDGTABDR'
080000170807     C                   PARM                    SDGTABDS
080100170807     C*
080200170807     C                   If        Xtberr  <> '1'
080300170807     C                   MoveL     XTbUni        SDGHD4DS
080400170807     C                   MoveL(P)  DesHd4        $Statod
080500170807     C                   MoveL     Codiso2Hd4    $IdPaese
080600170807     C                   Endif
080700170807     C                   endif
080800000000     C*
080900000000     C                   ENDSR
081000000000      /EJECT
081100000000     C************************************************************
081200170728     C* Ricerca codice provincia
081300000000     C************************************************************
081400170728     C     F4_Prv        BEGSR
081500000000     C*
081600000000     C                   Reset                   XTabDS
081700000000     C*
081800170728     C                   Clear                   Campo6            6
081900170728     C*
082000000000     C                   Move      '01'          XTaOpz
082100000000     C                   Move      XScSoc        XTaAzi
082200000000     C                   Move      XScLin        XTaLin
082300000000     C                   Z-add     Curr          XTaRig
082400000000     C                   Z-add     Curc          XTaCol
082500000000     C                   Move      *Blank        XTaKey
082600170728     C                   Movel     $Stato        Campo6
082700170728     C                   Move      Campo6        XtaKey
082800000000     C*
082900000000     C                   Clear                   KPJBU
083000000000     C                   MoveL     XTabDS        KPJBU
083100000000     C*
083200000000     C                   CALL      'ANGC17R'
083300000000     C                   PARM                    KPJBA
083400000000     C                   MoveL     KPJBU         XTabDS
083500000000     C*
083600000000     C                   if        XTaErr     <> '1'      and
083700000000     C                             XTaKey     <> *Blank
083800170728     C                   Move      XTaKey        Campo6
083900170728     C                   Move      Campo6        $Prv
084000170728     C                   Movel     Campo6        $Stato
084100000000     C                   endif
084200000000     C*
084300000000     C                   ENDSR
084400170731      /EJECT
084500170731     C************************************************************
084600170731     C* Ricerca codice cap
084700170731     C************************************************************
084800170731     C     F4_Cap        BEGSR
084900170731     C                   Clear                   $Cap
085000170731     C                   Reset                   A801DS
085100170731     C                   Move      '01'          OPZ801
085200170731     C                   Move      XscSoc        Societa801
085300170731     C                   Movel     $Stato        Stato801
085400170809     C                   Movel     $Cap          Cap801
085500170731     C                   Z-Add     CURR          XTARIG
085600170731     C                   Z-Add     CURC          XTACOL
085700170731     C                   Clear                   KPJBU
085800170731     C                   Movel     A801DS        KPJBU
085900170731     C                   Call      'ANA801R'
086000170731     C                   Parm                    KPJBA
086100170731     C                   Movel     KPJBU         A801DS
086200170731     C     ERR801        IfNe      '1'
086300170731     C                   Movel     CAP801        $Cap
086400170731     C                   Else
086500170731     C                   Movel     *BLANK        $Cap
086600170731     C                   EndIf
086700170731     C                   If        $Cap <> *blanks
086800170731     C                   Eval      $Stato =  Stato801
086900170731     C                   Eval      $Prv   =  Prov801
087000170731     C                   Eval      $Locali=  Localit801
087100170731     C                   EndIf
087200170731     C*
087300170731     C                   ENDSR
087400000000      /EJECT
087500000000     C************************************************************
087600000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
087700000000     C************************************************************
087800000000     C     GESPIE        BEGSR
087900000000     C*
088000000000     C                   SELECT
088100000000     C*
088200000000     C     $GEST         WHENEQ    'D1'
088300000000     C     $ULKD1        IFEQ      0
088400000000     C* riempimento opzioni
088500000000     C                   EXSR      F24D1
088600000000     C                   ELSE
088700000000     C                   WRITE     FMTZ1
088800000000     C                   ENDIF
088900000000     C*
089000000000     C     $GEST         WHENEQ    'D2'
089100000000     C     $ULKD1        IFEQ      0
089200000000     C* riempimento opzioni
089300000000     C                   EXSR      F24D2
089400000000     C                   ELSE
089500000000     C                   WRITE     FMTZ1
089600000000     C                   ENDIF
089700170807     C*
089800170807     C     $GEST         WHENEQ    'D3'
089900170807     C     $ULKD1        IFEQ      0
090000170807     C* riempimento opzioni
090100170807     C                   EXSR      F24D3
090200170807     C                   ELSE
090300170807     C                   WRITE     FMTZ1
090400170807     C                   ENDIF
090500000000     C                   ENDSL
090600000000     C*
090700000000     C                   ENDSR
090800000000     C/EJECT
090900000000     C************************************************************
091000000000     C* Chiamata programma con tasto personalizzato
091100000000     C************************************************************
091200000000     C     FPerD1        BEGSR
091300000000     C*
091400000000     C* salvo la DS di ingresso
091500170728    >C                   MOVEL     SDGSI74DS     $SaveDS
091600000000     C*
091700170728    >C                   MOVE      xscsoc        SocietaI74
091800170728     C* passare nei campi appositi della DS SDGSI74DS il valore
091900000000     C* della chiave della VL univoca
092000000000     C*
092100170805    >C***                MOVEL     SDGSI74DS     KPJBU
092200000000     C                   CALL      $PGF1(PosFun)                        22
092300000000     C                   PARM                    KPJBA
092400170805     C                   PARM                    sdgsi74ds
092500000000     C                   PARM                    NOMPGM
092600000000     C*
092700000000     C* ripristino la DS di ingresso
092800170728    >C                   MOVEL     $SaveDS       SDGSI74DS
092900000000     C*
093000000000     C                   ENDSR
093100000000     C/EJECT
093200000000     C************************************************************
093300000000     C* GESTIONE VIDEO RECORD D1
093400000000     C************************************************************
093500000000     C     GESD1         BEGSR
093600000000     C* inizializzazione videata
093700000000   B1C     $INZD1        IFEQ      *ON
093800000000     C                   EXSR      INZD1
093900000000     C                   MOVE      *OFF          $INZD1
094000000000   E1C                   ENDIF
094100000000     C* emissione piede videata se proveniente da altra
094200000000     C* salvataggio valore formato di provenienza
094300000000   B1C     $LASTV        IFNE      'D1'
094400000000     C                   WRITE     FMTT1
094500000000     C                   EXSR      GESPIE
094600000000     C                   MOVE      $LASTV        $LASTG
094700000000     C                   MOVE      'D1'          $LASTV
094800000000   E1C                   ENDIF
094900000000     C*
095000000000     C* se esistono errori sulla videata
095100000000     C* emetto la write del formato a indicatori spenti per vedere
095200000000     C* le eventuali decodifiche
095300130926   B1C                   if        (*IN99       = *On   or
095400170808   B1C                              *In16       = *On ) and
095500170808   B1C                              $Primavolta <>*On   and
095600130926     C                             $GEST        = 'D1'
095700000000     C                   MOVEA     *IN           WIN
095800000000     C                   MOVE      *ALL'0'       IN5098           49
095900130926     C                   MOVEA     IN5098        *IN(50)
096000130926     C                   MOVEA     WIN           *IN
096100130930     C                   Eval      WIn16        = *In16
096200130930     C                   Eval      *In16        = *Off
096300000000     C                   WRITE     FMTD1
096400000000     C                   MOVEA     WIN           *IN
096500130930     C                   Eval      *In16        = WIn16
096600000000   E1C                   ENDIF
096700170808   B1C                   if         $Primavolta = *On
096800170808     C                   MOVE      *ALL'0'       IN5098
096900170808     C                   MOVEA     IN5098        *IN(50)
097000170808     C                   endif
097100000000     C* Inizializzazione indice tasti funzione personalizzati
097200000000     C                   Eval      PosFun = 25
097300000000     C*
097400000000   B1C                   SELECT
097500000000     C* Annullamento
097600170804    >C     OPZI74        WHENEQ    '04'
097700000000     C* Visualizzazione
097800170804    >C     OPZI74        OREQ      '05'
097900000000     C                   WRITE     FMTD1
098000000000     C                   EXFMT     PROTECT
098100000000     C* Immissione/Modifica/Copia
098200000000   X1C                   OTHER
098300000000     C                   EXFMT     FMTD1
098400000000   E1C                   ENDSL
098500000000     C*
098600000000     C* azzero la posizione del cursore
098700000000     C                   Z-ADD     0             H1RIGA
098800000000     C                   Z-ADD     0             H1COLO
098900000000     C*
099000000000     C* controllo abilitazione tasti funzione
099100000000     C                   EXSR      CTRKD1
099200000000     C*
099300000000   B1C     *IN99         IFEQ      *OFF
099400000000     C*
099500000000   B2C                   SELECT
099600170803     C* ROLLUP
099700000000    >C     $TASTO        WHENEQ    ROLLUP
099800170803    >C                   MOVE      'D2'          $GEST
099900000000     C* F2=Decodifica
100000000000     C     $TASTO        WHENEQ    F02
100100000000     C                   EXSR      F02D1
100200000000     C* F3=Fine
100300000000     C     $TASTO        WHENEQ    F03
100400000000     C                   EXSR      F03D1
100500000000     C* F4=Ricerca
100600000000     C     $TASTO        WHENEQ    F04
100700000000     C                   EXSR      SEARCH
100800000000     C* F5=Refresh
100900000000     C     $TASTO        WHENEQ    F05
101000000000     C                   EXSR      F05D1
101100000000     C* F8=Successivo
101200000000     C     $TASTO        WHENEQ    F08
101300000000     C                   EXSR      F08D1
101400000000     C* F12=Ritorno
101500000000     C     $TASTO        WHENEQ    F12
101600000000     C                   EXSR      F12D1
101700000000     C* F24=Altri tasti
101800000000     C     $TASTO        WHENEQ    F24
101900000000     C                   EXSR      F24D1
102000000000     C* Personalizzata
102100170728     C* NB chiamata NON possibile perchè SDGSI74DS più lunga
102200000000     C* di KPJBU 256 caratteri                          a
102300000000     C**** $PGF1(PosFun) WHENNE    *BLANK
102400000000     C****               EXSR      FPerD1
102500000000     C*
102600000000   X2C                   OTHER
102700000000     C                   EXSR      CTRD1
102800000000     C* aggiungere le chiamate ai controlli delle videate supplementari
102900000000    >C     *IN99         IFEQ      *OFF
103000000000    >C                   EXSR      CTRD2
103100130926   B1C                   if        *IN99       = *On
103200000000    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
103300000000    >C* indicatori uguali in videate differenti, genererei conflitti
103400000000    >C                   MOVE      *ALL'0'       IN5098           49
103500000000    >C                   MOVEA     IN5098        *IN(50)
103600000000    >C                   SETON                                        9996                       96
103700000000    >C                   ENDIF
103800000000    >C                   ENDIF
103900170802     C* aggiungere le chiamate ai controlli delle videate supplementari
104000170802    >C     *IN99         IFEQ      *OFF
104100170802    >C                   EXSR      CTRD3
104200170802   B1C                   if        *IN99       = *On
104300170802    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
104400170802    >C* indicatori uguali in videate differenti, genererei conflitti
104500170802    >C                   MOVE      *ALL'0'       IN5098           49
104600170802    >C                   MOVEA     IN5098        *IN(50)
104700170802    >C                   SETON                                        9996                       96
104800170802    >C                   ENDIF
104900170802    >C                   ENDIF
105000170802     C* aggiungere le chiamate ai controlli delle videate supplementari
105100000000     C*
105200000000   B3C     $TASTO        IFEQ      F06
105300000000     C     *IN99         ANDEQ     *OFF
105400000000     C* eseguo aggiornamento
105500000000     C                   EXSR      AGGANA
105600000000     C* eseguo operazioni del dopo-aggiornamento
105700000000   B4C     *IN99         IFEQ      *OFF
105800000000     C                   EXSR      GESAGG
105900000000   E4C                   ENDIF
106000000000   E3C                   ENDIF
106100000000     C*
106200000000   E2C                   ENDSL
106300000000     C*
106400000000   E1C                   ENDIF
106500000000     C*
106600000000     C                   ENDSR
106700000000     C/EJECT
106800000000     C************************************************************
106900000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
107000000000     C************************************************************
107100000000     C     CTRKD1        BEGSR
107200000000     C*
107300000000     C                   SETOFF                                       99
107400000000     C                   MOVEA     $FC1          $FC
107500000000     C                   EXSR      CTRKEY
107600000000     C     *IN99         IFEQ      *ON
107700000000     C                   SETON                                        97
107800000000     C                   ENDIF
107900000000     C*
108000000000     C                   ENDSR
108100000000     C/EJECT
108200000000     C************************************************************
108300000000     C* INIZIALIZZAZIONE VIDEATA DATI
108400000000     C************************************************************
108500000000     C     INZD1         BEGSR
108600000000     C*
108700000000     C                   CLEAR                   FMTD1
108800000000    >C                   CLEAR                   FMTD2
108900170804    >C                   CLEAR                   FMTD3
109000000000     C*
109100170804     C                   Eval      D1Anno      = AnnoI74
109200170804     C                   Eval      D1Trim      = TrimI74
109300170807     C*
109400170807     C* Se opzione 20 reperisco i dati del frontespizio, se presenti
109500000000     C*
109600000000     C* per tutte le opzioni NON immissione
109700170804     C     OPZI74        IFNE      '01'
109800170804     C*
109900170804   B2C     *IN22         DOUEQ     *OFF
110000170804     C     MSGRTN        ORNE      'R'
110100170804     C* Se viene chiamato dal dettaglio, aggancio con il numero record
110200170804     C                   Select
110300170804     C*
110400170804     C                   When      NrrCstI74 <> 0
110500170804     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
110600170804   B>C     OPZI74        IFEQ      '02'
110700170804    >C     OPZI74        OREQ      '04'
110800170804     C     NRRCSTI74     Chain     SdGcst00f                          2122
110900170804     C                   Else
111000170808     C     NRRCSTI74     Chain(N)  SdGcst00f                          21
111100170804     C                   Endif
111200170807     C*
111300170807     C                   When      OPZI74 = '20'
111400170807     C*
111500170807     C                   EXSR      RepFronteSP
111600170804     C*
111700170804     C                   Other
111800170804     C                   Eval      CSTSocieta   = XScSoc
111900170804     C                   Eval      CSTTpCom     = TpcomI74
112000170804     C                   Eval      CSTTpRec     = TprecI74
112100170804     C                   Eval      CSTAnno      = AnnoI74
112200170804     C                   Eval      CSTTrim      = TrimI74
112300170804     C                   Eval      CSTMese      = MeseI74
112400000000     C*
112500170804     C                   Eval      CSTCOGRAG1 =  COGRAG1I74
112600170804     C                   Eval      CSTCF      =  CFI74
112700170804     C                   Eval      CSTPARTIVA =  PARTIVAI74
112800170804     C                   Eval      CSTCDFISC  =  CDFISCI74
112900170804     C                   Eval      CSTSTATO1  =  STATO1I74
113000170804     C                   Eval      CSTIDPAES1 =  IDPAES1I74
113100170804     C                   Eval      CSTPIVA1   =  PIVA1I74
113200170804     C                   Eval      CSTCDFISC1 =  CDFISC1I74
113300170804     C                   Eval      CSTTPN1    =  TPN1I74
113400170804     C                   Eval      CSTDENOM1  =  DENOM1I74
113500170804     C                   Eval      CSTNOME1   =  NOME1I74
113600170804     C                   Eval      CSTCOGNOM1 =  COGNOM1I74
113700170804     C                   Eval      CSTINDIR2  =  INDIR2I74
113800170804     C                   Eval      CSTNRCIV2  =  NRCIV2I74
113900170804     C                   Eval      CSTCAP2    =  CAP2I74
114000170804     C                   Eval      CSTLOCALI2 =  LOCALI2I74
114100170804     C                   Eval      CSTPROV2   =  PROV2I74
114200170804     C                   Eval      CSTSTATO2  =  STATO2I74
114300170804     C                   Eval      CSTIDPAES2 =  IDPAES2I74
114400170804     C                   Eval      CSTINDIR3  =  INDIR3I74
114500170804     C                   Eval      CSTNRCIV3  =  NRCIV3I74
114600170804     C                   Eval      CSTCAP3    =  CAP3I74
114700170804     C                   Eval      CSTLOCALI3 =  LOCALI3I74
114800170804     C                   Eval      CSTPROV3   =  PROV3I74
114900170804     C                   Eval      CSTSTATO3  =  STATO3I74
115000170804     C                   Eval      CSTIDPAES3 =  IDPAES3I74
115100170804     C                   Eval      CSTSTATO4  =  STATO4I74
115200170804     C                   Eval      CSTIDPAES4 =  IDPAES4I74
115300170804     C                   Eval      CSTPIVA4   =  PIVA4I74
115400170804     C                   Eval      CSTTPN4    =  TPN4I74
115500170804     C                   Eval      CSTCOGRAG4 =  COGRAG4I74
115600170804     C                   Eval      CSTDENOM4  =  DENOM4I74
115700170804     C                   Eval      CSTNOME4   =  NOME4I74
115800170804     C                   Eval      CSTCOGNOM4 =  COGNOM4I74
115900170804     C                   Movel     DsCliFor      CstCliFor
116000170804     C                   Movel     DsCliFor      SavCliFor
116100170804     C*
116200170804     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
116300170804   B>C     OPZI74        IFEQ      '02'
116400170804    >C     OPZI74        OREQ      '04'
116500170804     C*
116600170808     C     K07CST02      Chain     SDGCST02L                          2122
116700170804     C                   Else
116800170808     C     K07CST02      Chain(N)  SDGCST02L                          21
116900170804     C                   Endif
117000170804     C*
117100170804     C                   ENDSL
117200170804     C*
117300170804     C* Gestione RCD allocato
117400170804   B3C     *IN22         IFEQ      *ON
117500170804    >C                   MOVEL     'SDGCST00F'   MSGFIL
117600170804     C                   EXSR      MSGPGM
117700170804   E3C                   ENDIF
117800170804   E2C                   ENDDO
117900170804     C*
118000170804     C* se il rcd da gestire non viene trovato
118100170804     C* oppure è allocato da altro pgm
118200170804     C* ritorno al pgm guida avvertendolo dell'errore
118300170804   B2C     *IN21         IFEQ      *ON
118400170804     C     *IN22         OREQ      *ON
118500170804     C                   MOVE      *ON           $FINE
118600170804    >C                   MOVE      '1'           ERRI74
118700000000     C*
118800000000     C* altrimenti
118900000000   X2C                   ELSE
119000000000     C* riempio la videata
119100170807     C                   If        OPZI74 <> '20'
119200000000     C                   EXSR      RIED1
119300170807     C                   Endif
119400000000    >C* Aggiungere le routine di riempimento dei record succcessivi
119500000000     C* richiamo routine dei ctrl per decodificarla
119600000000     C                   EXSR      CTRD1
119700000000    >C* Aggiungere le routine di controllo dei record succcessivi
119800170802    >C                   EXSR      CTRD2
119900170802    >C                   EXSR      CTRD3
120000000000     C                   MOVE      *ALL'0'       IN5098
120100000000     C                   MOVEA     IN5098        *IN(50)
120200000000   E2C                   ENDIF
120300000000     C*
120400000000     C* per immissione
120500000000   X1C                   ELSE
120600000000     C*
120700000000     C* riempio con i default di immissione  (se reperiti)
120800000000     C                   IF        $FlgDef     = *on
120900000000     C                   EXSR      RIEDEF
121000000000     C                   ENDIF
121100000000     C*
121200000000     C* imposto i campi chiave ricevuti anche se si è in immissione
121300000000     C* perchè potrebbero essere stati passati dei default di chiave
121400170802     C                   Eval      D1Sogg      = SoggI74
121500170809     C                   Eval      D1Cf        = CFI74
121600170802     C                   Eval      D1PartIva   = PartIvaI74
121700170802     C                   Eval      D1CdFisc    = CdFiscI74
121800170802     C                   Eval      D1Tpn1      = Tpn1I74
121900170804     C                   Eval      D1Stato1    = Stato1I74
122000170802     C                   Eval      D1IdPaes1   = IdPaes1I74
122100170802     C                   Eval      D1PIva1     = PIva1I74
122200170802     C                   Eval      D1CdFisc1   = CdFisc1I74
122300170804     C                   Eval      H1CogRag1   = CogRag1I74
122400170804     C                   Eval      D1Cognom1   = Cognom1I74
122500170804     C                   Eval      D1Denom1    = Denom1I74
122600170802     C                   Eval      D1Nome1     = Nome1I74
122700170802     C                   Eval      D1Indir2    = Indir2I74
122800170802     C                   Eval      D1NrCiv2    = NrCiv2I74
122900170802     C                   Eval      D1Cap2      = Cap2I74
123000170804     C                   Eval      D1Locali2   = Locali2I74
123100170802     C                   Eval      D1Prov2     = Prov2I74
123200170802     C                   Eval      D1Stato2    = Stato2I74
123300170802     C                   Eval      D1IdPaes2   = IdPaes2I74
123400170802     C*
123500170802     C                   Eval      D1Indir3    = Indir3I74
123600170802     C                   Eval      D1NrCiv3    = NrCiv3I74
123700170802     C                   Eval      D1Cap2      = Cap3I74
123800170802     C                   Eval      D1Locali3   = Locali3I74
123900170802     C                   Eval      D1Prov3     = Prov3I74
124000170802     C                   Eval      D1Stato3    = Stato3I74
124100170802     C                   Eval      D1IdPaes3   = IdPaes3I74
124200170802     C*
124300170802     C                   Eval      D1Stato4    = Stato4I74
124400170802     C                   Eval      D1IdPaes4   = IdPaes4I74
124500170802     C                   Eval      D1Piva4     = Piva4I74
124600170802     C                   Eval      D1Tpn4      = Tpn4I74
124700170804     C                   Eval      H1CogRag4   = CogRag4I74
124800170804     C                   Eval      D1Cognom4   = Cognom4I74
124900170804     C                   Eval      D1Denom4    = Denom4I74
125000170802     C                   Eval      D1Nome4     = Nome4I74
125100000000     C*
125200000000     C* decodifico le costanti
125300000000     C                   EXSR      F02D1
125400000000   E1C                   ENDIF
125500000000     C*
125600000000     C* imposto quegli attributi della videata che sono modificabili
125700000000     C* al richiamo del determinato pgm
125800000000     C                   EXSR      ATRD1
125900000000     C*
126000000000     C                   ENDSR
126100000000     C/EJECT
126200170807     C************************************************************
126300170807     C* Reperisco i dati per il Frontespizio
126400170807     C******************************************************
126500170807     C     RepFrontesp   BEGSR
126600170807     C*
126700170807     C* preparo i parametri di chiamata al driver
126800170807     C                   Clear                   XMPEDS
126900170807     C                   EVAL      TpChiamata = '01'
127000170807     C                   EVAL      NChiavi = 5
127100170807     C                   EVAL      VLogica = 2
127200170807     C*
127201170905R265 X***                Do        4             $Cont
127202170905R265 C                   Do        $DimSk        $Cont
127400170807     C                   Clear                   XMPEDS1
127500170807     C                   Eval      DSMPEPGM = 'SDGSI74R' +
127600170807     C                                        %char($cont)
127700170807     C* preparo le chiavi per la chiamata al driver
127800170807     C                   MoveL     XSCSOC        DSMPESOC
127900170807     C                   MoveL     *Blanks       DSMPEPRF
128000170807     C* Si reperiscono i dati di default a seconda delle modalità con
128100170807     C* cui il pgm è usato
128200170807     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
128300170807     C* MPETIP = 'PV'  pgm richiamato da menù
128400170807     C* MPETIP = 'PB'  x lancio Batch
128500170807     C                   MoveL     'PV'          DSMPETIP
128600170807     C*
128700170807     C                   CALLB     'XMPE'
128800170807     C                   PARM                    XMPEDS
128900170807     C                   PARM                    XMPEDS1
129000170807     C*
129100170807     C     Risultato     Ifeq      '0'
129200170807     C*
129300170807     C                   select
129400170807     C                   When      $Cont   = 1
129500170807     C                   MoveL     DSMPEPAR      DsFrontesp1
129600170807     C                   When      $Cont   = 2
129700170807     C                   MoveL     DSMPEPAR      DsFrontesp2
129800170807     C                   When      $Cont   = 3
129900170807     C                   MoveL     DSMPEPAR      DsFrontesp3
130000170807     C                   When      $Cont   = 4
130100170807     C                   MoveL     DSMPEPAR      DsFrontesp4
130101170905R265 C                   When      $Cont   = 5
130102170905R265 C                   MoveL     DSMPEPAR      DsFrontesp41
130200170807     C                   EndSl
130300170807     C*
130400170807     C                   Endif
130500170807     C*
130600170807     C                   Enddo
130700170807     C* Reperisco dalla funzione anagrafica i dati del dichiarante
130800170807     C     Risultato     Ifeq      '0'
130900170807     C                   Else
131000170807     C                   EXSR      RepFisc_Soc
131100170807     C                   endif
131200170808     C                   Exsr      Ried_20
131300170807     C*
131400170807     C                   Endsr
131500170807     C/EJECT
131600170807     C************************************************************
131700170807     C* RIEMPIMENTO VIDEATA  D1
131800170807     C************************************************************
131900170807     C     RIED_20       BEGSR
132000170807     C*
132100170807     C* impostare i campi della videata dai campi del file
132200170807     C*
132300170808     C*********          Eval      D1Partiva = WPIVA1
132400170808     C********           Eval      D1Cdfisc  = WCdFisc1
132500170807     C*
132600170807     C                   IF        WIDPaes1 <> *BLANKS
132700170808     C******             MOVEL     WIDPaes1      $Idpaese
132800170808     C******             Exsr      RepStaHd4
132900170808     C******             Eval      D1Stato1  = $Stato
133000170807     C                   Endif
133100170807     C*
133200170808     C                   Eval      D1Stato1  = WStato1
133300170807     C                   Eval      D1IdPaes1 = WIdPaes1
133400170807     C                   Eval      D1PIVA1   = WPIVA1
133500170807     C                   Eval      D1CdFisc1 = WCdFisc1
133600170808     C                   Eval      H1COGRAG1 = WCOGRAG1
133700170808     C                   Eval      D1DENOM1  = WDENOM1
133800170808     C                   Eval      D1NOME1   = WNOME1
133900170808     C                   Eval      D1COGNOM1 = WCOGNOM1
134000170807     C*
134100170807     C                   If        D1Denom1 <> *Blanks
134200170807     C                   Eval      D1TPN1 = '2'
134300170807     C                   Else
134400170807     C                   If        D1Nome1 <> *BLANKS
134500170807     C                   Eval      D1TPN1 = '1'
134600170807     C                   EndIF
134700170807     C                   EndIF
134800170807     C*
134900170807     C                   Eval      D1INDIR2  = WINDIR2
135000170807     C                   Eval      D1NRCIV2  = WNRCIV2
135100170807     C                   Eval      D1CAP2    = WCAP2
135200170807     C                   Eval      D1LOCALI2 = WLOCALI2
135300170807     C                   Eval      D1PROV2   = WPROV2
135400170807     C*
135500170807     C                   IF        WIDPaes2 <> *BLANKS
135600170808     C******             MOVEL     WIDPaes2      $Idpaese
135700170808     C******             Exsr      RepStaHd4
135800170808     C******             Eval      D1Stato2  = $Stato
135900170807     C                   Endif
136000170807     C*
136100170808     C                   Eval      D1Stato2  = WStato2
136200170807     C                   Eval      D1IDPAES2 = WIDPAES2
136300170807     C*
136400170807     C                   Eval      D1INDIR3  = WINDIR3
136500170807     C                   Eval      D1NRCIV3  = WNRCIV3
136600170807     C                   Eval      D1CAP3    = WCAP3
136700170807     C                   Eval      D1LOCALI3 = WLOCALI3
136800170807     C                   Eval      D1PROV3   = WPROV3
136900170807     C*
137000170807     C                   IF        WIDPaes3 <> *BLANKS
137100170808     C******             MOVEL     WIDPaes3      $Idpaese
137200170808     C******             Exsr      RepStaHd4
137300170808     C******             Eval      D1Stato3  = $Stato
137400170807     C                   Endif
137500170807     C*
137600170808     C                   Eval      D1Stato3  = WStato3
137700170807     C                   Eval      D1IDPAES3 = WIDPAES3
137800170807     C*
137900170807     C                   IF        WIDPaes4 <> *BLANKS
138000170808     C******             MOVEL     WIDPaes4      $Idpaese
138100170808     C******             Exsr      RepStaHd4
138200170808     C******             Eval      D1Stato4  = $Stato
138300170807     C                   Endif
138400170807     C*
138500170808     C                   Eval      D1Stato4  = WStato4
138600170807     C                   Eval      D1IdPaes4 = WIDPaes4
138700170807     C                   Eval      D1PIVA4   = WPIVA4
138800170808     C                   Eval      H1COGRAG4 = WCOGRAG4
138900170808     C                   Eval      D1DENOM4  = WDENOM4
139000170808     C                   Eval      D1NOME4   = WNOME4
139100170808     C                   Eval      D1COGNOM4 = WCOGNOM4
139200170807     C*
139300170807     C                   If        D1Denom4 <> *Blanks
139400170807     C                   Eval      D1TPN4 = '2'
139500170807     C                   Else
139600170807     C                   If        D1Nome4 <> *BLANKS
139700170807     C                   Eval      D1TPN4 = '1'
139800170807     C                   Endif
139900170807     C                   EndIF
140000170807     C*
140100170807     C*
140200170807     C                   ENDSR
140300170807     C/EJECT
140400170807     C************************************************************
140500170807     C* Reperimesto dello stato da tabella Hd4
140600170807     C************************************************************
140700170807     C     RepstaHd4     BEGSR
140800170807     C*
140900170807     C* Lettura tabella HD4
141000170807     C                   Clear                   SDGTABDS
141100170807     C*
141200170807     C                   MOVE      'HD4'         XTBCOD
141300170807     C                   MOVE      XScSoc        XTBAZI
141400170807     C                   MOVE      XScLin        XTBLIN
141500170807     C                   MOVE      '0'           XTBALC
141600170807     C                   MOVE      XScCut        XTBUTE
141700170807     C                   MOVE      '0'           XTBERR
141800170807     C*
141900170807     C* SETLL + READE
142000170807     C                   MOVE      '2'           XTBRIC
142100170807     C                   CALL      'SDGTABDR'
142200170807     C                   PARM                    SDGTABDS
142300170807     C*
142400170807     C                   DoW       XTBErr      = '0'
142500170807     C*
142600170807     C                   if        XTBErr      = '0'
142700170807     C                   MoveL     XtbUni        SDGHD4DS
142800170807     C*
142900170807     C                   if        CodISO2HD4  = $IdPaese
143000170807     C                   Move(P)   XtbKey        $Stato
143100170807     C                   Leave
143200170807     C                   endif
143300170807     C*
143400170807     C                   endif
143500170807     C* READE
143600170807     C                   Move      '3'           XTBRIC
143700170807     C                   MOVE      'HD4'         XTBCOD
143800170807     C                   MOVE      '0'           XTBERR
143900170807     C                   CALL      'SDGTABDR'
144000170807     C                   PARM                    SDGTABDS
144100170807     C*
144200170807     C                   endDo
144300170807     C*
144400170807     C                   ENDSR
144500170807     C/EJECT
144600000000     C************************************************************
144700000000     C* RIEMPIMENTO VIDEATA  D1
144800000000     C************************************************************
144900000000     C     RIED1         BEGSR
145000000000     C*
145100000000     C* impostare i campi della videata dai campi del file
145200000000     C*
145300170807     C                   Eval      D1CF      = CSTCF
145400170807     C                   Eval      D1SOGG    = CSTSOGG
145500170804     C                   Eval      D1PARTIVA = CSTPARTIVA
145600170804     C                   Eval      D1CDFISC  = CSTCDFISC
145700170804     C                   Eval      D1STATO1  = CSTSTATO1
145800170804     C                   Eval      D1IDPAES1 = CSTIDPAES1
145900170804     C                   Eval      D1PIVA1   = CSTPIVA1
146000170804     C                   Eval      D1CDFISC1 = CSTCDFISC1
146100170804     C                   Eval      D1TPN1    = CSTTPN1
146200170804     C                   Eval      D1DENOM1  = CSTDENOM1
146300170804     C                   Eval      D1COGNOM1 = CSTCOGNOM1
146400170804     C                   Eval      D1NOME1   = CSTNOME1
146500170804     C                   Eval      D1INDIR2  = CSTINDIR2
146600170804     C                   Eval      D1NRCIV2  = CSTNRCIV2
146700170804     C                   Eval      D1CAP2    = CSTCAP2
146800170804     C                   Eval      D1LOCALI2 = CSTLOCALI2
146900170804     C                   Eval      D1PROV2   = CSTPROV2
147000170804     C                   Eval      D1STATO2  = CSTSTATO2
147100170804     C                   Eval      D1IDPAES2 = CSTIDPAES2
147200170804     C                   Eval      D1INDIR3  = CSTINDIR3
147300170804     C                   Eval      D1NRCIV3  = CSTNRCIV3
147400170804     C                   Eval      D1CAP3    = CSTCAP3
147500170804     C                   Eval      D1LOCALI3 = CSTLOCALI3
147600170804     C                   Eval      D1PROV3   = CSTPROV3
147700170804     C                   Eval      D1STATO3  = CSTSTATO3
147800170804     C                   Eval      D1IDPAES3 = CSTIDPAES3
147900170804     C                   Eval      D1STATO4  = CSTSTATO4
148000170804     C                   Eval      D1IDPAES4 = CSTIDPAES4
148100170804     C                   Eval      D1PIVA4   = CSTPIVA4
148200170804     C                   Eval      D1TPN4    = CSTTPN4
148300170804     C                   Eval      D1DENOM4  = CSTDENOM4
148400170804     C                   Eval      D1NOME4   = CSTNOME4
148500170804     C                   Eval      D1COGNOM4 = CSTCOGNOM4
148600170804     C*
148700170804     C                   ENDSR
148800000000     C************************************************************
148900000000     C* RIPRISTINA GLI ATTRIBUTI DELLA VIDEATA
149000000000     C************************************************************
149100000000     C     ATRD1         BEGSR
149200000000     C*
149300000000    >C* in questa routine bisogna attivare indicatori o campi programma
149400000000    >C* che gestiscono gli attributi di uno più campi di questa videata;
149500000000    >C* attenzione : non è questo il posto in cui eseguire controlli,
149600000000    >C*  dovrebbero già arrivare dei flag
149700000000     C*
149800000000     C                   SETOFF                                       02
149900000000     C*
150000000000     C* in caso di manutenzione
150100170801    >C     OPZI74        IFEQ      '02'
150200000000     C* proteggo i campi chiave
150300000000     C                   SETON                                        02        PR campi key
150400000000   E3C                   ENDIF
150500130911     C*
150600130911     C* in caso di insertimento o copia
150700170807     C* abilito i campi D1SOGG e D1Partiva e D1Cdfisc
150800170801    >C     OPZI74        IFEQ      '01'
150900170801    >C     OPZI74        orEQ      '03'
151000130911     C                   SETON                                        11        PR campi key
151100130911   E3C                   ENDIF
151200000000     C*
151300170807     C*
151400170807     C* in caso di chiamata dati società disabilito D1CF
151500170807     C*
151600170807    >C     OPZI74        IFEQ      '20'
151700170807     C                   SETON                                        12        PR campi key
151800170807   E3C                   ENDIF
151900000000     C                   ENDSR
152000000000     C/EJECT
152100000000     C************************************************************
152200000000     C* RIEMPIMENTO CAMPI DAI DEFAULT
152300000000     C************************************************************
152400000000     C     RIEDEF        BEGSR
152500000000     C*
152600000000     C*
152700000000     C                   ENDSR
152800000000     C/EJECT
152900000000     C************************************************************
153000000000     C* GESTIONE F02 VIDEO D1
153100000000     C************************************************************
153200000000     C     F02D1         BEGSR
153300000000     C*
153400000000     C* per decodificare la videata richiamo la routine dei ctrl
153500000000     C* e poi disattivo gli eventuali messaggi di errore
153600000000     C                   EXSR      CTRD1
153700000000     C*
153800000000     C                   MOVE      *ALL'0'       IN5098
153900000000     C                   MOVEA     IN5098        *IN(50)
154000000000     C*
154100000000     C                   ENDSR
154200000000     C/EJECT
154300000000     C************************************************************
154400000000     C* GESTIONE F03 VIDEO D1
154500000000     C************************************************************
154600000000     C     F03D1         BEGSR
154700000000     C*
154800000000     C                   MOVE      *ON           $FINE
154900170804    >C                   MOVE      '1'           RETI74
155000000000     C* fine programma
155100000000     C                   EXSR      ENDPGM
155200000000     C*
155300000000     C                   ENDSR
155400000000     C/EJECT
155500000000     C************************************************************
155600000000     C* GESTIONE F05 VIDEO D1
155700000000     C************************************************************
155800000000     C     F05D1         BEGSR
155900000000     C*
156000000000     C                   MOVE      *ON           $INZD1
156100000000     C*
156200000000     C                   ENDSR
156300000000     C/EJECT
156400000000     C************************************************************
156500000000     C* GESTIONE F08 VIDEO D1
156600000000     C************************************************************
156700000000     C     F08D1         BEGSR
156800000000     C*
156900000000     C                   MOVE      *ON           $FINE
157000170804    >C                   MOVE      '0'           RETI74
157100000000     C*
157200000000     C                   ENDSR
157300000000     C/EJECT
157400000000     C************************************************************
157500000000     C* GESTIONE F12 VIDEO D1
157600000000     C************************************************************
157700000000     C     F12D1         BEGSR
157800000000     C*
157900000000     C                   MOVE      *ON           $FINE
158000170804    >C                   MOVE      '2'           RETI74
158100000000     C*
158200000000     C                   ENDSR
158300000000     C/EJECT
158400000000     C************************************************************
158500000000     C* GESTIONE F24 VIDEO D1
158600000000     C************************************************************
158700000000     C     F24D1         BEGSR
158800000000     C*
158900000000     C                   MOVEA     $FX1          $FX
159000000000     C                   MOVEA     $FC1          $FC
159100000000     C                   MOVEA     $FR1          $FR
159200000000     C                   Z-ADD     $ULKD1        $ULK
159300000000     C                   Z-ADD     $D1L01        $L01
159400000000     C                   Z-ADD     $D1L02        $L02
159500000000     C                   EXSR      RIEKEY
159600000000     C                   Z-ADD     $ULK          $ULKD1            3 0
159700000000     C                   MOVEA     $FC           $FC1
159800000000     C                   MOVEL     $KEY1         Z1KE1
159900000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
160000000000     C                   MOVEL     $KEY2         Z1KE2
160100000000     C                   WRITE     FMTZ1
160200000000     C*
160300000000     C                   ENDSR
160400000000     C/EJECT
160500000000     C************************************************************
160600000000     C* CONTROLLO VIDEATA
160700000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
160800000000     C* 95 96 97 98
160900000000     C************************************************************
161000000000     C     CTRD1         BEGSR
161100000000     C*
161200000000     C                   SETOFF                                       99
161300170808     C                   Move      *Off          $PrimaVolta       1
161400000000     C*
161500000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
161600000000     C*
161700170804     C     OPZI74        IFNE      '04'
161800000000     C*
161900170807     C                   If        OPZI74      = '01'   or
162000170804     C                             OPZI74      = '03'
162100000000     C                   EXSR      CtrSogg
162200170807     C                   Endif
162300170807     C*
162400170807     C                   If        OPZI74     <> '20'
162500170807     C                   Exsr      CtrCF
162600170807     C                   Endif
162700170801     C*
162800170801     C* Salvo i campi che hanno controlli comuni
162900170801     C                   Movel     D1Stato1      $Stato
163000170801     C                   Movel     D1Tpn1        $Tpn
163100170801     C                   Movel     D1Denom1      $Denom
163200170801     C                   Movel     D1Nome1       $Nome
163300170801     C                   Movel     D1Cognom1     $Cognom
163400170804     C                   If        D1Denom1<> *blanks
163500170804     C                   Movel     D1Denom1      H1CogRag1
163600170804     C                   Else
163700170804     C                   Movel     D1Cognom1     H1CogRag1
163800170804     C                   Endif
163900170804     C                   Movel     D1Cognom1     $Cognom
164000170801     C*
164100170801     C* Tipo Natura giuridica
164200170807     C                   Movel     *On           $CtrObbl
164300170801     C                   EXSR      CtrTPN
164400170805     C                   Movel     $Tpnd         D1Tpn1d
164500170801     C                   If        $ErrCtrl = *on
164600170801     C                   Seton                                        5199
164700170801     C                   Endif
164800170801     C* Stato
164900170808     C                   if        D1IdPaes1   = *Blank   and
165000170808     C                             D1Stato1    = *Blank   and
165100170808     C                             OPZI74      = '20'
165200170808     C                   Eval      D1IdPaes1   = 'IT'
165300170808     C                   Eval      $IDPaese    = 'IT'
165400170808     C                   Exsr      RepStaHd4
165500170808     C                   Eval      D1Stato1  = $Stato
165600170808     C                   endif
165700170808     C***                EXSR      CtrStato1
165800170808     C                   EXSR      CtrStato
165900170801     C*
166000170801     C                   If        $ErrCtrl = *on
166100170811fm3  C                             Or
166200170811fm3  C                             D1Stato1 = *Blank
166300170810     C*
166400170810     C* Solo per il 2017 se lo stato è *blanks imposto lo stato iso = 'OO'
166500170810     C* e non emetto l'errore.
166600170811fm3  X*******            If        D1Stato1 =  'OO'    and
166700170811fm3  C                   If        D1Stato1 =  *Blank  and
166800170810     C                             D1Anno = 2017 and D1CF = 'F'
166900170810     C                   Movel     'OO'          $IDpaese
167000170810     C                   eval      $StatoD   = 'Valido per dogana 2017'
167100170810     C                   Else
167200170801     C*
167300170804     C                   If        $TipErr  = '1'
167400170801     C                   Seton                                        5299
167500170801     C                   Endif
167600170801     C*
167700170804     C                   If        $TipErr  = '2'
167800170801     C                   Seton                                        7299
167900170801     C                   Endif
168000170801     C*
168100170807     C                   Endif
168200170810     C                   Endif
168300170807     C*
168400170801     C                   Movel     $Statod       D1Stato1D
168500170803     C                   Movel     $IdPaese      D1IdPaes1
168600170801     C*
168700000000     C*
168800170807     C                   EXSR      CtrPIVA_CDF
168900170803     C* Denominazione
169000170801     C                   EXSR      CtrDenom
169100170803     C                   If        $ErrCtrl = *on
169200170804     C                   If        $TipErr  = '1'
169300170805     C                   Seton                                        5599
169400170803     C                   Else
169500170805     C                   Seton                                        7599
169600170803     C                   Endif
169700170803     C                   Endif
169800170803     C
169900170803     C* Cognome
170000170804     C                   EXSR      CtrCognome
170100170803     C                   If        $ErrCtrl = *on
170200170804     C                   If        $TipErr  = '1'
170300170805     C                   Seton                                        5699
170400170803     C                   Else
170500170805     C                   Seton                                        7699
170600170803     C                   Endif
170700170803     C                   Endif
170800170803     C* Nome
170900000000     C                   EXSR      CtrNome
171000170803     C                   If        $ErrCtrl = *on
171100170804     C                   If        $TipErr  = '1'
171200170805     C                   Seton                                        5799
171300170803     C                   Else
171400170805     C                   Seton                                        7799
171500170803     C                   Endif
171600170803     C                   Endif
171700000000     C*
171800000000     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
171900170804     C     OPZI74        IFEQ      '01'
172000170804    >C     OPZI74        OREQ      '03'
172100000000     C*
172200000000     C* imposto i campi chiave e verifico l'esistenza del record
172300170804     C*
172400170804     C                   Eval      CSTSocieta   = XScSoc
172500170804     C                   Eval      CSTTpCom     = TpcomI74
172600170804     C                   Eval      CSTTpRec     = TprecI74
172700170804     C                   Eval      CSTAnno      = AnnoI74
172800170804     C                   Eval      CSTTrim      = TrimI74
172900170804     C                   Eval      CSTMese      = MeseI74
173000170804     C*
173100170804     C                   Eval      CSTPartIVA   = D1Partiva
173200170804     C                   Eval      CSTCdFisc    = D1CdFisc
173300170804     C*
173400170804     C                   Eval      CSTStato1 = D1Stato1
173500170804     C                   Eval      CSTIdPaes1= D1IdPaes1
173600170804     C*
173700170804     C                   Movel     DsCliForV     DsCliFor
173800170804     C                   Movel     DsCliFor      CSTCliFor
173900170808     C     K07CST02      Setll     sdgcst02l                              21
174000000000     C* se lo trovo significa che esiste già
174100170804     C     *IN21         IFEQ      '1'
174200000000     C                   SETON                                        9099
174300000000     C                   ENDIF
174400000000     C*
174500000000     C                   ENDIF
174600000000     C*
174700170807     C                   ENDIF
174800170807     C*
174900000000     C                   ENDSR
175000000000     C/EJECT
175100170808     C***********************************************************
175200170808     C* Controllo Stato1
175300170808     C***********************************************************
175400170808     C     CtrStato1     BEGSR
175500170808     C*
175600170808     C                   Clear                   $ErrCtrl
175700170808     C                   Clear                   $TipErr
175800170808     C*
175900170808     C                   Clear                   $StatoD
176000170808     C                   Clear                   $IdPaese
176100170808     C*
176200170808     C                   Clear                   AngG12DS
176300170808     C*
176400170808     C                   Clear                   SDGTABDS
176500170808     C*
176600170808     C                   Eval      XTbRic      = '1'
176700170808     C                   Eval      XTbAzi      = XScSoc
176800170808     C                   Eval      XTbKey      = *Zero
176900170808     C                   Eval      XTbCod      = 'G12'
177000170808     C                   Move      $Stato        XTbKey
177100170808     C*
177200170808     C                   CALL      'SDGTABDR'
177300170808     C                   PARM                    SDGTABDS
177400170808     C*
177500170808     C                   if        XTbErr     <> '0'
177600170808     C*                  Eval      $Errctrl   = *on
177700170808     C*                  Eval      $TipErr    = '2'
177800170808     C                   endif
177900170808     C                   MoveL     XTbUni        AngG12DS
178000170808     C                   MoveL     DesG12        $StatoD
178100170808     C
178200170808     C* Reperisco il codice Iso da tabelle HD4
178300170808     C                   Clear                   SDGHD4DS
178400170808     C                   Clear                   SDGTABDS
178500170808     C                   Eval      XTbRic      = '1'
178600170808     C                   Eval      XTbAzi      = XScSoc
178700170808     C                   Eval      XTbKey      = *Zero
178800170808     C                   Eval      XTbCod      = 'HD4'
178900170808     C                   Move      $Stato        XTbKey
179000170808     C*
179100170808     C                   CALL      'SDGTABDR'
179200170808     C                   PARM                    SDGTABDS
179300170808     C                   if        XTbErr     <> '0'
179400170808     C                   Eval      $Errctrl   = *on
179500170808     C                   Eval      $TipErr    = '2'
179600170808     C                   else
179700170808     C                   MoveL     XTbUni        SDGHD4DS
179800170808     C                   MoveL     Codiso2Hd4    $IdPaese
179900170808     C                   MoveL     DesHD4        $StatoD
180000170808     C                   Endif
180100170808     C*
180200170808     C**                 Endif
180300170808     C*
180400170808     C                   ENDSR
180500170808     C/EJECT
180600130919     C***********************************************************
180700130919     C* Controllo codice soggetto
180800130919     C***********************************************************
180900130919     C     CtrSogg       BEGSR
181000130919     C*
181100130919     C                   Clear                   D1SoggD
181200130919     C*
181300130919     C     D1SOGG        IFNE      *BLANK
181400130919     C                   CALLb     'XALLINEA'
181500130919     C                   PARM                    D1SOGG
181600130919     C                   PARM      8             LEN               5 0
181700130919     C                   PARM                    XSCINP
181800130919     C                   ENDIF
181900130919     C*
182000130919     C                   MoveL     D1Sogg        WSogg
182100130919     C                   if        D1Sogg     <> *Blank
182200130919     C                   EXSR      CallDvaSog
182300130919     C*
182400130919     C                   if        DVAErrore  <> '1'
182500130919     C*
182600130919     C                   Eval      %subst(DVASOG:1:DVALENOUT)
182700130919     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
182800130919     C*
182900130919     C                   MoveL     DVSDes        D1SoggD
183000170804     C* Recupero i dati anagrafici solo se non è stato inserito alcun dato
183100170807     C                   If        D1Partiva  =  *blanks and
183200170807     C                             D1CdFisc   =  *blanks and
183300170807     C                             D1Stato1   =  *blanks and
183400170807     C                             D1IdPaes1  =  *blanks and
183500170807     C                             D1PIVA1    =  *blanks and
183600170807     C                             D1CdFisc   =  *blanks and
183700170804     C*
183800170807     C                             H1CogRag1  =  *blanks and
183900170807     C                             D1TpN1     =  *blanks and
184000170807     C                             D1Denom1   =  *blanks and
184100170807     C                             D1Cognom1  =  *blanks and
184200170807     C                             D1Nome1    =  *blanks and
184300170804     C*
184400170807     C                             D1Indir2   =  *blanks and
184500170807     C                             D1NrCiv2   =  *blanks and
184600170807     C                             D1CAP2     =  *blanks and
184700170807     C                             D1Locali2  =  *blanks and
184800170807     C                             D1Prov2    =  *blanks and
184900170807     C                             D1Stato2   =  *blanks and
185000170807     C                             D1IdPaes2  =  *blanks and
185100170804     C* STABILE ORGANIZZAZIONE
185200170807     C                             D1Indir3   =  *blanks and
185300170807     C                             D1NrCiv3   =  *blanks and
185400170807     C                             D1CAP3     =  *blanks and
185500170807     C                             D1Locali3  =  *blanks and
185600170807     C                             D1Prov3    =  *blanks and
185700170807     C                             D1Stato3   =  *blanks and
185800170807     C                             D1IdPaes3  =  *blanks and
185900170804     C*
186000170804     C* RAPPRESENTANTE FISCALE
186100170807     C                             D1Stato4   =  *blanks and
186200170807     C                             D1IdPaes4  =  *blanks and
186300170807     C                             D1PIVA4    =  *blanks and
186400170804     C*
186500170807     C                             H1CogRag4  =  *blanks and
186600170807     C                             D1TpN4     =  *blanks and
186700170807     C                             D1Denom4   =  *blanks and
186800170807     C                             D1Cognom4  =  *blanks and
186900170807     C                             D1Nome4    =  *blanks
187000170804     C*
187100170807     C                   Exsr      RepFisc
187200170808     C                   Eval      $PrimaVolta = *On
187300170804     C*
187400170804     C                   Endif
187500130919     C*
187600130919     C                   else
187700130919     C*
187800131001     C                   Eval      *In16       = *On
187900130919     C*
188000130919     C                   endif
188100130919     C*
188200130919     C                   endif
188300130919     C*
188400130919     C                   ENDSR
188500130919     C/EJECT
188600130919     C************************************************************
188700130919     C* Chiamata al driver dei soggetti
188800130919     C************************************************************
188900130919     C     CallDvaSog    BEGSR
189000130919     C*
189100130919     C                   EVAL      DVALenOut = %Size(DVASOG)
189200130919     C*
189300130919     C                   CALLB     'NDDVASOG'
189400130919     C                   PARM      '1'           DVARIC
189500130919     C                   PARM      XScSoc        DVASOCIETA
189600130919     C                   PARM                    DVAUNITA
189700130919     C                   PARM      'DVASOG    '  DVASTRUTT
189800130919     C                   PARM                    DVADTRIF
189900130919     C                   PARM                    DVALENOUT
190000130919     C* Se si accede con il codice cli/for impostare i campi seguenti
190100130919     C* se si accede con il soggetto ometterli e impostare gli altri
190200130919     C*****              PARM                    DVASNATURA
190300130919     C*****              PARM                    DVACODICE
190400130919     C*****              PARM                    DVALINEAV
190500130919     C*****              PARM                    DVAFILIALE
190600130919     C                   PARM                    *omit
190700130919     C                   PARM                    *omit
190800130919     C                   PARM                    *omit
190900130919     C                   PARM                    *omit
191000130919     C                   PARM      WSogg         DVSSOGG
191100130919     C                   PARM      *Blank        DVSTPIND
191200130919     C                   PARM      *Blank        DVSCDIND
191300130919     C                   PARM                    DVAERRORE
191400130919     C                   PARM                    DVAOUTPUT
191500130919     C*
191600130919     C                   ENDSR
191700170804     C/EJECT
191800170804     C***********************************************************
191900170804     C* Recupero  i dati anagrafici se immesso il soggetto
192000170804     C***********************************************************
192100170807     C     RepFisc       BEGSR
192200170807     C*
192300170807     C                   Clear                   SAV00701DS
192400170807     C* Dati del soggetto
192500170804     C                   Clear                   SDG00701DS
192600170804     C*
192700170804     C                   Eval      Opz0701     = '01'
192800170804     C                   Eval      Err0701     = '00'
192900170804     C                   Eval      Soc0701     = SocietaI74
193000170804     C                   Eval      TpCom0701   = TpComI74
193100170804     C                   Eval      Anno0701    = AnnoI74
193200170804     C                   Eval      Trim0701    = TrimI74
193300170804     C                   Eval      Mese0701    = MeseI74
193400170804     C                   Eval      Sogg0701    = D1Sogg
193500170804     C                   Eval      CF0701      = D1Cf
193600170804     C                   Eval      Tecnic0701  = *Off
193700170804     C                   Eval      TpN1Sg0701  = *Blank
193800170804     C                   Eval      DtRif0701   = *Loval
193900170804     C                   Eval      NrAsRe0701  = 0
194000170804     C*
194100170804     C* salvo la kpjbu per ripristinarla dopo il richiamo
194200170804     C                   MoveL     KPJBU         SavKPJBU
194300170804     C*
194400170804     C                   CALLB     'SDG00701R'
194500170804     C                   PARM                    KPJBA
194600170804     C                   PARM                    SDG00701DS
194700170807     C* Salvo i dati reperiti
194800170807     C                   MoveL     SDG00701ds    SAV00701ds
194900170807     C*
195000170807     C* Dati storicizzati (se presenti sostituisco)
195100170807     C                   Clear                   SDG00701DS
195200170807     C*
195300170807     C                   Exsr      RepPeriodo
195400170807     C*
195500170807     C                   Move      $Data10A2     $DtRif
195600170807     C*
195700170807     C                   Eval      Opz0701     = '02'
195800170807     C                   Eval      Err0701     = '00'
195900170807     C                   Eval      Soc0701     = SocietaI74
196000170807     C                   Eval      TpCom0701   = TpComI74
196100170807     C                   Eval      Anno0701    = AnnoI74
196200170807     C                   Eval      Trim0701    = TrimI74
196300170807     C                   Eval      Mese0701    = MeseI74
196400170807     C                   Eval      Sogg0701    = D1Sogg
196500170807     C                   Eval      CF0701      = D1Cf
196600170807     C                   Eval      Tecnic0701  = *Off
196700170807     C                   Eval      TpN1Sg0701  = *Blank
196800170807     C                   Eval      DtRif0701   = $DtRif
196900170807     C                   Eval      NrAsRe0701  = 0
197000170807     C*
197100170807     C* salvo la kpjbu per ripristinarla dopo il richiamo
197200170807     C                   MoveL     KPJBU         SavKPJBU
197300170807     C*
197400170807     C                   CALLB     'SDG00701R'
197500170807     C                   PARM                    KPJBA
197600170807     C                   PARM                    SDG00701DS
197700170807     C* se non trovati sostituisco con quelli del soggetto
197800170807     C                   if        Found0701  <> *On
197900170807     C                   MoveL     SAV00701ds    SDG00701ds
198000170807     C                   endif
198100170807     C* ripristino KPJBU
198200170807     C                   MoveL     SavKPJBU      KPJBU
198300170804     C*
198400170804     C* DATI FISCALI E SEDE
198500170804     C                   MoveL(P)  PartIV0701    D1Partiva
198600170804     C                   MoveL(P)  CdFisc0701    D1CdFisc
198700170804     C                   MoveL(P)  Stato10701    D1Stato1
198800170804     C                   MoveL(P)  IdPae10701    D1IdPaes1
198900170804     C                   MoveL(P)  PIVA10701     D1PIVA1
199000170809     C                   MoveL(P)  CdFis10701    D1CdFisc1
199100170804     C*
199200170804     C                   MoveL(P)  CogRa10701    H1CogRag1
199300170804     C                   MoveL(P)  TpN10701      D1TpN1
199400170804     C                   MoveL(P)  Denom10701    D1Denom1
199500170804     C                   MoveL(P)  Cogno10701    D1Cognom1
199600170804     C                   MoveL(P)  Nome10701     D1Nome1
199700170804     C*
199800170804     C                   MoveL(P)  Indir20701    D1Indir2
199900170804     C                   MoveL(P)  NrCiv20701    D1NrCiv2
200000170804     C                   MoveL(P)  CAP20701      D1CAP2
200100170804     C                   MoveL(P)  Local20701    D1Locali2
200200170804     C                   MoveL(P)  Prov20701     D1Prov2
200300170804     C                   MoveL(P)  Stato20701    D1Stato2
200400170804     C                   MoveL(P)  IdPae20701    D1IdPaes2
200500170804     C*
200600170804     C* STABILE ORGANIZZAZIONE
200700170804     C                   MoveL(P)  Indir30701    D1Indir3
200800170804     C                   MoveL(P)  NrCiv30701    D1NrCiv3
200900170804     C                   MoveL(P)  CAP30701      D1CAP3
201000170804     C                   MoveL(P)  Local30701    D1Locali3
201100170804     C                   MoveL(P)  Prov30701     D1Prov3
201200170804     C                   MoveL(P)  Stato30701    D1Stato3
201300170804     C                   MoveL(P)  IdPae30701    D1IdPaes3
201400170804     C*
201500170804     C* RAPPRESENTANTE FISCALE
201600170804     C                   MoveL(P)  Stato40701    D1Stato4
201700170804     C                   MoveL(P)  IdPae40701    D1IdPaes4
201800170804     C                   MoveL(P)  PIVA40701     D1PIVA4
201900170804     C*
202000170804     C                   MoveL(P)  CogRa40701    H1CogRag4
202100170804     C                   MoveL(P)  TpN40701      D1TpN4
202200170804     C                   MoveL(P)  Denom40701    D1Denom4
202300170804     C                   MoveL(P)  Cogno40701    D1Cognom4
202400170804     C                   MoveL(P)  Nome40701     D1Nome4
202500170804     C*
202600170804     C                   ENDSR
202700170804     C*
202800170807     C/EJECT
202900170807     C***********************************************************
203000170807     C* Recupero  i dati anagrafici della società
203100170807     C***********************************************************
203200170807     C     RepFisc_soc   BEGSR
203300170807     C*
203400170807     C* Dati del soggetto
203500170807     C                   Clear                   SDG00701DS
203600170807     C*
203700170807     C                   Eval      Opz0701     = '03'
203800170807     C                   Eval      Err0701     = '00'
203900170807     C                   Eval      Soc0701     = SocietaI74
204000170807     C                   Eval      TpCom0701   = TpComI74
204100170807     C                   Eval      Anno0701    = AnnoI74
204200170807     C                   Eval      Trim0701    = TrimI74
204300170807     C                   Eval      Mese0701    = MeseI74
204400170807     C                   Eval      Tecnic0701  = *Off
204500170807     C                   Eval      TpN1Sg0701  = *Blank
204600170807     C                   Eval      DtRif0701   = *Loval
204700170807     C                   Eval      NrAsRe0701  = 0
204800170807     C*
204900170807     C* salvo la kpjbu per ripristinarla dopo il richiamo
205000170807     C                   MoveL     KPJBU         SavKPJBU
205100170807     C*
205200170807     C                   CALLB     'SDG00701R'
205300170807     C                   PARM                    KPJBA
205400170807     C                   PARM                    SDG00701DS
205500170807     C*
205600170807     C                   Movel     savkpjbu      Kpjbu
205700170807     C*
205800170807     C* se non trovati sostituisco con quelli del soggetto
205900170807     C                   if        Found0701  =  *On
206000170807     C*
206100170807     C* DATI FISCALI E SEDE
206200170808     C******             MoveL(P)  PartIV0701    D1Partiva
206300170808     C******             MoveL(P)  CdFisc0701    D1CdFisc
206400170808     C                   MoveL(P)  Stato10701    WStato1
206500170808     C                   MoveL(P)  IdPae10701    WIdPaes1
206600170808     C                   if        Stato10701  = *Blank   and
206700170808     C                             IdPae10701  = 'IT'
206800170808     C                   Eval      $IDPaese    = 'IT'
206900170808     C                   Exsr      RepStaHd4
207000170808     C                   Eval      WStato1     = $Stato
207100170808     C                   endif
207200170808     C                   MoveL(P)  PIVA10701     WPIVA1
207300170808     C                   MoveL(P)  CdFisc0701    WCdFisc1
207400170807     C*
207500170808     C                   MoveL(P)  CogRa10701    WCogRag1
207600170808     C                   MoveL(P)  TpN10701      WTpN1
207700170808     C                   MoveL(P)  Denom10701    WDenom1
207800170808     C                   MoveL(P)  Cogno10701    WCognom1
207900170808     C                   MoveL(P)  Nome10701     WNome1
208000170807     C*
208100170808     C                   MoveL(P)  Indir20701    WIndir2
208200170808     C                   MoveL(P)  NrCiv20701    WNrCiv2
208300170808     C                   MoveL(P)  CAP20701      WCAP2
208400170808     C                   MoveL(P)  Local20701    WLocali2
208500170808     C                   MoveL(P)  Prov20701     WProv2
208600170808     C                   MoveL(P)  Stato20701    WStato2
208700170808     C                   MoveL(P)  IdPae20701    WIdPaes2
208800170807     C*
208900170807     C* STABILE ORGANIZZAZIONE
209000170808     C                   MoveL(P)  Indir30701    WIndir3
209100170808     C                   MoveL(P)  NrCiv30701    WNrCiv3
209200170808     C                   MoveL(P)  CAP30701      WCAP3
209300170808     C                   MoveL(P)  Local30701    WLocali3
209400170808     C                   MoveL(P)  Prov30701     WProv3
209500170808     C                   MoveL(P)  Stato30701    WStato3
209600170808     C                   MoveL(P)  IdPae30701    WIdPaes3
209700170807     C*
209800170807     C* RAPPRESENTANTE FISCALE
209900170808     C                   MoveL(P)  Stato40701    WStato4
210000170808     C                   MoveL(P)  IdPae40701    WIdPaes4
210100170808     C                   MoveL(P)  PIVA40701     WPIVA4
210200170807     C*
210300170808     C                   MoveL(P)  CogRa40701    WCogRag4
210400170808     C                   MoveL(P)  TpN40701      WTpN4
210500170808     C                   MoveL(P)  Denom40701    WDenom4
210600170808     C                   MoveL(P)  Cogno40701    WCognom4
210700170808     C                   MoveL(P)  Nome40701     WNome4
210800170807     C                   Endif
210900170807     C*
211000170807     C                   ENDSR
211100170807     C*
211200170807     C******************************************************
211300170807     C* Reperisco le date di inizio e fine periodo
211400170807     C******************************************************
211500170807     C     RepPeriodo    BEGSR
211600170807     C*
211700170807     C                   Reset                   DataDs
211800170807     C                   Clear                   $Data10A1
211900170807     C                   Clear                   $MeseI            2 0
212000170807     C                   Clear                   $MeseF            2 0
212100170807     C*
212200170807     C* recupero il mese di inizio trimestre e
212300170807     C*          il mese di fine   trimestre
212400170807     C                   Select
212500170807     C*
212600170807     C                   When      TrimI74     = 1
212700170807     C                   Eval      $MeseI      = 1
212800170807     C                   Eval      $MeseF      = 3
212900170807     C*
213000170807     C                   When      TrimI74     = 2
213100170807     C                   Eval      $MeseI      = 4
213200170807     C                   Eval      $MeseF      = 6
213300170807     C*
213400170807     C                   When      TrimI74     = 3
213500170807     C                   Eval      $MeseI      = 7
213600170807     C                   Eval      $MeseF      = 9
213700170807     C*
213800170807     C                   When      TrimI74     = 4
213900170807     C                   Eval      $MeseI      = 10
214000170807     C                   Eval      $MeseF      = 12
214100170807     C*
214200170807     C                   endSl
214300170807     C*
214400170807     C* Periodo Da
214500170807     C                   MoveL     AnnoI74       AnnoDs
214600170807     C                   MoveL     $MeseI        MeseDs
214700170807     C                   MoveL     '01'          GiornoDs
214800170807     C                   Move      DataDs        $Data10A1
214900170807     C*
215000170807     C* Periodo A
215100170807     C                   Clear                   WData
215200170807     C                   Reset                   DataDs
215300170807     C                   Clear                   $Data10A2
215400170807     C*
215500170807     C                   MoveL     AnnoI74       AnnoDs
215600170807     C                   MoveL     $MeseF        MeseDs
215700170807     C                   MoveL     '01'          GiornoDs
215800170807     C                   Move      DataDs        WData
215900170807     C*
216000170807     C* reperisco il fine mese
216100170807     C                   CALL      'XDATCD'
216200170807     C                   PARM      4             XCod              2 0
216300170807     C                   PARM                    WData
216400170807     C*
216500170807     C                   Move      WData         $Data10A2
216600170807     C*
216700170807     C                   ENDSR
216800170807     C/EJECT
216900170804     C/EJECT
217000170804     C***********************************************************
217100170804     C* Controllo cliente / fornitore
217200170804     C***********************************************************
217300170804     C     CtrCf         BEGSR
217400170804     C*
217500170807     C                   Clear                   D1Cfd
217600170804     C                   Clear                   X07ValDs
217700170804     C*
217800170807     C**                 If        D1Cf <> *blanks
217900170804     C                   Move      '1'           X07Ric
218000170804     C                   Move      *On           X07All
218100170804     C                   Movel     'SDGCST000'   X07TRc
218200170804     C                   Movel     'CSTCF    '   X07Cam
218300170804     C                   Movel     D1CF          X07VAL
218400170804     C*
218500170804     C                   Callb     'X07VALR'
218600170804     C                   Parm                    X07ValDs
218700170804     C*
218800170804     C     X07ERR        IFEQ      *Off
218900170807     C                   Movel     X07des        D1CfD
219000170804     C                   else
219100170807     C                   SetOn                                        5099
219200170804     C                   endif
219300170807     C**                 endif
219400170804     C*
219500170804     C                   Endsr
219600130919     C/EJECT
219700130919     C***********************************************************
219800130919     C* Controllo la tipologia della sottonatura
219900130919     C***********************************************************
220000130919     C     CtrTpN        BEGSR
220100130919     C*
220200170801     C                   Clear                   $Errctrl
220300170801     C                   Clear                   $TpnD
220400130919     C                   Clear                   X07ValDs
220500170807     C*
220600170807     C                   if        $Tpn       =  *Blank and
220700170807     C                             $CtrObbl = *on
220800170807     C                   Movel     *on           $ErrCtrl
220900170807     C                   Endif
221000130919     C*
221100170801     C                   if        $Tpn       =  *Blank and
221200170801     C                             ($Denom <>*blanks  or
221300170801     C                             $Nome  <>*blanks  or
221400170801     C                             $Cognom<>*blanks)
221500170801     C                   Movel     *on           $ErrCtrl
221600170801     C                   Endif
221700170801     C*
221800170801     C                   if        $Tpn       <> *Blank
221900130919     C                   Move      '1'           X07Ric
222000130919     C                   Move      *Off          X07All
222100170801     C                   MoveL     'SDGCST000'   X07TRc
222200170805     C                   MoveL     'CSTTPN1  '   X07Cam
222300170801     C                   MoveL     $Tpn          X07VAL
222400130919     C*
222500130919     C                   CALLB     'X07VALR'
222600130919     C                   PARM                    X07ValDs
222700130919     C*
222800130919     C     X07ERR        IFEQ      *Off
222900170801     C                   MoveL     X07DES        $Tpnd
223000130919     C                   else
223100170801     C                   Movel     *on           $ErrCtrl
223200130919     C                   endif
223300170804     C                   endif
223400130919     C*
223500130919     C                   ENDSR
223600130919     C/EJECT
223700130919     C***********************************************************
223800170801     C* Controllo ragione sociale (se Tpn <>'1' obbligatorio)
223900130919     C***********************************************************
224000170803     C     CtrDenom      BEGSR
224100170803     C*
224200170803     C                   Clear                   $ErrCtrl
224300170803     C                   Clear                   $TipErr
224400130919     C*
224500170805     C                   if        $Denom  = *Blank  and $Tpn  > '1'  or
224600170805     C                             $Denom  <>*Blank  and $Tpn =  '1'
224700170803     C                   Movel     '1'           $TipErr
224800170803     C                   else
224900130919     C*
225000170803     C                   If        $Denom <> *blanks
225100170803     C                   Movel     $Denom        $CampoScan
225200170803     C                   Exsr      ScanCharOk
225300170803     C                   If        $ErrScan = '1'
225400170803     C                   Movel     '2'           $TipErr
225500130919     C                   endif
225600170803     C                   endif
225700170803     C                   endif
225800170803     C*
225900170803     C                   IF        $TipErr <> *blanks
226000170803     C                   Movel     *on           $Errctrl
226100170803     C                   endif
226200130919     C*
226300130919     C                   ENDSR
226400170803     C/EJECT
226500170803     C***********************************************************
226600170803     C* Controllo Cognome
226700170803     C***********************************************************
226800170803     C     CtrCognome    BEGSR
226900170803     C*
227000170803     C                   Clear                   $ErrCtrl
227100170803     C                   Clear                   $TipErr
227200170803     C*
227300170803     C                   if        $Cognom = *Blank  and $Tpn =  '1'  or
227400170805     C                             $Cognom <>*Blank  and $Tpn >  '1'
227500170803     C                   Movel     '1'           $TipErr
227600170803     C                   else
227700170803     C*
227800170803     C                   If        $Cognom<> *blanks
227900170803     C                   Movel     $Cognom       $CampoScan
228000170803     C                   Exsr      ScanCharOk
228100170803     C                   If        $ErrScan = '1'
228200170803     C                   Movel     '2'           $TipErr
228300170803     C                   endif
228400170803     C                   endif
228500170803     C                   endif
228600170803     C*
228700170803     C                   IF        $TipErr <> *blanks
228800170803     C                   Movel     *on           $Errctrl
228900170803     C                   endif
229000170803     C*
229100170803     C                   ENDSR
229200130919     C/EJECT
229300130919     C***********************************************************
229400130919     C* Controllo nome
229500130919     C***********************************************************
229600130919     C     CtrNome       BEGSR
229700170803     C*
229800170803     C                   Clear                   $ErrCtrl
229900170803     C                   Clear                   $TipErr
230000170803     C*
230100170803     C                   if        $Nome   = *Blank  and $Tpn =  '1'  or
230200170805     C                             $Nome   <>*Blank  and  $Tpn >  '1'
230300170803     C                   Movel     '1'           $TipErr
230400170803     C                   else
230500170803     C*
230600170803     C                   If        $Nome  <> *blanks
230700170803     C                   Movel     $Nome         $CampoScan
230800170803     C                   Exsr      ScanCharOk
230900170803     C                   If        $ErrScan = '1'
231000170803     C                   Movel     '2'           $TipErr
231100170803     C                   endif
231200170803     C                   endif
231300170803     C                   endif
231400170803     C*
231500170803     C                   IF        $TipErr <> *blanks
231600170803     C                   Movel     *on           $Errctrl
231700170803     C                   endif
231800170803     C*
231900130919     C                   ENDSR
232000130919     C***********************************************************
232100170803     C* Controllo provincia
232200130919     C***********************************************************
232300170803     C     CtrPrv        BEGSR
232400130919     C*
232500170807     C                   Clear                   $ErrCtrl
232600170807     C                   Clear                   $TipErr
232700170803     C*
232800170803     C                   Clear                   $PrvD
232900130919     C*
233000130919     C                   Clear                   ANGC17DS
233100130919     C*
233200170807     C                   Select
233300170807     C*
233400170807     C                   When      $Prv       = *Blanks and
233500170807     C                             $CtrObbl = *on
233600170807     C                   Movel     *On           $ErrCtrl
233700170807     C                   Movel     '1'           $TipErr
233800170807     C*
233900170807     C                   When      $Prv       <> *Blanks
234000170807     C*
234100170807     C                   If        $Prv = 'DA' and $CtrObbl = *ON
234200170807     C                   MoveL(p)  'Dato Assente'$PrvD
234300170807     C                   Else
234400170807     C*
234500130919     C                   RESET                   SDGTABDS
234600130919     C*
234700130919     C                   Eval      XTbRic      = '1'
234800130919     C                   Eval      XTbAzi      = XScSoc
234900130919     C                   Eval      XTbKey      = *Zero
235000130919     C                   Eval      XTbCod      = 'C17'
235100170803     C                   Move      $Prv          KeyC17
235200170803     C                   Movel     $Stato        KeyC17
235300130919     C                   Move      KeyC17        XTBKey
235400130919     C*
235500130919     C                   CALL      'SDGTABDR'
235600130919     C                   PARM                    SDGTABDS
235700130919     C*
235800130919     C                   if        XTbErr     <> '0'
235900170803     C                   Movel     *on           $ErrCtrl
236000170807     C                   Movel     '2'           $TipErr
236100130919     C                   else
236200130919     C                   MoveL     XTbUni        ANGC17DS
236300170803     C                   MoveL     DesC17        $PrvD
236400170807     C                   endif
236500130919     C*
236600170807     C                   EndIf
236700130919     C*
236800170807     C                   EndSL
236900170807     C*
237000130919     C                   ENDSR
237100130919     C/EJECT
237200170803     C*****************************************************
237300170803     C* Controlli sul codice avviamento postale
237400170803     C*****************************************************
237500170803     C     CtrCAP        begsr
237600170803     C*
237700170803     C                   Clear                   $ErrCtrl
237800170803     C                   Clear                   $TipErr
237900170803     C*
238000170803     C* i controlli devono essere eseguiti con questo ordine, per cui
238100170803     C* al primo vado via
238200170803     C                   do
238300170803     C*
238400170803     C* Ctrl C.A.P., località, nazione
238500170803     C                   If        CecG12 = '2'
238501170905R2653C                             and $Stato   <> *blank
238502170905 "   C                             and $Cap     <> *blank
238503170905R2653C                             and $Locali  <> *blank
238600170803     C                   move      $Stato        CapStato
238700170803     C                   move      $CAP          CapCap
238701170905R2653X****               Move      $Locali       CapLocalit
238702170905R2653C                   Movel     $Locali       CapLocalit
238900170803     C     K03CAP01      Chain     Ancap01l                           21
239000170803     C                   if        *in21  = *on
239100170803     C* Località errata o C.A.P. non congruente con la località.
239200170803     C                   Movel     '1'           $TipErr
239300170803     c                   leave
239400170803     C* altrimenti, se il CAP è valido i dati reperiti sull'
239500170803     C* anagrafica ANCAP possono servire a valorizzare provincia
239600170803     C* e località
239700170803     C                   else
239800170803     C                   exsr      ValProv_Loc
239900170803     C                   endif
240000170803     C                   endif
240100170803     C*
240200170803     C* VALIDITA':
240300170803     c*   se CAP valorizzato
240400170803     C                   if        $CAP <> *blank
240500170803     C* e se su tabella stati è indicato
240600170803     C*   che DEVE essere eseguito il controllo del CAP
240700170803     C                             and CecG12 = '1'
240701170905R2653C                             and $Stato   <> *blank
240800170803     C* deve esistere su file codici avviamento
240900170803     C* postale...
241000170803     C                   move      $Stato        CAPStato
241100170803     C                   move      $CAP          CAPCap
241200170803     C     K02CAP01      chain     ANCAP01L                           21
241300170803     C* Codice di avviamento postale non trovato in anagrafica
241400170818fm8  C                   if        *in21  = *on
241500170803     C* "(F1) C.A.P. inesistente"
241600170803     C                   Movel     '2'           $TipErr
241700170803     c                   leave
241800170803     C* altrimenti, se il CAP è valido i dati reperiti sull'
241900170803     C* anagrafica ANCAP possono servire a valorizzare provincia
242000170803     C* e località
242100170803     C                   else
242200170803     C                   exsr      ValProv_Loc
242300170803     C                   endif
242400170803     C*
242500170803     C* NUMERO CARATTERI
242600170803     C* il numero di caratteri deve corrispondere a quello della tabell
242700170803     C     ' '           checkr    $CAP          $NrCar
242800170803     C                   if        $NrCar <> CAPG12 and CAPg12 <> 0
242900170803     c
243000170803     C* "C.A.P. ha un numero di caratteri diversi ..."
243100170803     C                   Movel     '3'           $TipErr
243200170803     c                   leave
243300170803     C                   endif
243400170803     C*
243500170803     C                   endif
243600170803     C*
243700170803     C                   enddo
243800170803     C*
243900170803     C                   If        $Tiperr <> *blanks
244000170803     C                   Movel     *on           $ErrCtrl
244100170803     C                   Endif
244200170803     C*
244300170803     C                   endsr
244400170803     C*****************************************************
244500170803     C* Valorizzazione provincia e località da anagrafica
244600170803     C* codici avviamento postale, se non già valorizzati
244700170803     C*****************************************************
244800170803     C     ValProv_Loc   begsr
244900170803     C*
245000170803     C* Provincia
245100170803     C* Si eseguono anche i controlli in modo da valorizzare
245200170803     C* anche la decodifica
245300170804     C                   if        $Prv       = *blank
245400170803     C                             and  CAPProv <> *blank
245500170804     C                   move      CAPProv       $Prv
245600170803     C                   endif
245700170803     C* Località
245800170804     C                   if        $Locali   = *blank
245900170803     C                             and  CAPLocalit <> *blank
246000170804     C                   move      CAPLocalit    $Locali
246100170803     C                   endif
246200170803     C*
246300170803     C                   endsr
246400170801     C************************************************************
246500170801     C* Scansione per caratteri non ammessi
246600170801     C************************************************************
246700170801     C     ScanCharOk    BEGSR
246800170802     C*
246900170802     C                   Clear                   $Errscan
247000170801     C*
247100170801     C* prima converte alcuni caratteri, sostituendoli con altri ammessi
247200170801     C     CnvErr:CnvOk  XLATE     $CampoScan    $CampoScan
247300170801     C*
247400170801     C* poi sostituisce con uno spazio i caratteri rimanenti non ammessi
247500170801     C                   Eval      $FineScan   = *Off
247600170801     C*
247700170801     C                   Clear                   PosIni            4 0
247800170801     C     CharOK        Check     $CampoScan    PosIni
247900170801     C*
248000170801     C                   if        PosIni <> 0
248100170802     C                   Eval      $ErrScan = *on
248200170802     C                   endif
248300170801     C*
248400170801     C                   ENDSR
248500170807     C/EJECT
248600000000     C************************************************************
248700000000     C* GESTIONE VIDEO RECORD  D2
248800000000     C************************************************************
248900000000    >C     GESD2         BEGSR
249000000000     C* emissione piede videata se proveniente da altra
249100000000     C* salvataggio valore formato di provenienza
249200000000   B1C     $LASTV        IFNE      'D2'
249300000000     C                   WRITE     FMTT1
249400000000     C                   EXSR      GESPIE
249500000000     C                   MOVE      $LASTV        $LASTG
249600000000     C                   MOVE      'D2'          $LASTV
249700000000   E1C                   ENDIF
249800000000     C*
249900000000     C* se esistono errori sulla videata
250000000000     C* emetto la write del formato a indicatori spenti per vedere
250100000000     C* le eventuali decodifiche
250200170804   B1C                   if         *IN99       = *On      and
250300130926     C                             $GEST        = 'D2'
250400000000     C                   MOVEA     *IN           WIN
250500000000     C                   MOVE      *ALL'0'       IN5098           49
250600000000     C                   MOVEA     IN5098        *IN(50)
250700000000     C                   WRITE     FMTD2
250800000000     C                   MOVEA     WIN           *IN
250900000000   E1C                   ENDIF
251000000000     C* Inizializzazione indice tasti funzione personalizzati
251100000000     C                   Eval      PosFun = 25
251200000000     C*
251300000000   B1C                   SELECT
251400000000     C* Annullamento
251500170804    >C     OPZI74        WHENEQ    '04'
251600000000     C* Visualizzazione
251700170804    >C     OPZI74        OREQ      '05'
251800000000     C                   WRITE     FMTD2
251900000000     C                   EXFMT     PROTECT
252000000000     C* Immissione/Modifica/Copia
252100000000   X1C                   OTHER
252200000000     C                   EXFMT     FMTD2
252300000000   E1C                   ENDSL
252400000000     C*
252500000000     C* azzero la posizione del cursore
252600000000     C                   Z-ADD     0             H1RIGA
252700000000     C                   Z-ADD     0             H1COLO
252800000000     C*
252900000000     C* controllo abilitazione tasti funzione
253000000000     C                   EXSR      CTRKD1
253100000000     C*
253200000000   B1C     *IN99         IFEQ      *OFF
253300000000     C*
253400000000   B2C                   SELECT
253500000000     C* ROLDOWN
253600000000     C     $TASTO        WHENEQ    ROLDWN
253700000000     C                   MOVE      'D1'          $GEST
253800170805     C* ROLLUP
253900170805     C     $TASTO        WHENEQ    ROLLUP
254000170805     C                   MOVE      'D3'          $GEST
254100000000     C* F3=Fine
254200000000     C     $TASTO        WHENEQ    F03
254300000000     C                   EXSR      F03D1
254400000000     C* F4=Ricerca
254500000000     C     $TASTO        WHENEQ    F04
254600000000     C                   EXSR      SEARCH
254700000000     C* F5=Refresh
254800000000     C     $TASTO        WHENEQ    F05
254900000000     C                   EXSR      F05D2
255000000000     C* F8=Successivo
255100000000     C     $TASTO        WHENEQ    F08
255200000000     C                   EXSR      F08D1
255300000000     C* F12=Ritorno
255400000000     C     $TASTO        WHENEQ    F12
255500000000     C                   EXSR      F12D1
255600000000     C* F24=Altri tasti
255700000000     C     $TASTO        WHENEQ    F24
255800000000     C                   EXSR      F24D2
255900000000     C* Personalizzata
256000170728     C* NB chiamata NON possibile perchè SDGSI74DS più lunga
256100000000     C* di KPJBU 256 caratteri                          a
256200000000     C**** $PGF1(PosFun) Whenne    *BLANK
256300000000     C****               EXSR      FperD1
256400000000     C*
256500000000   X2C                   OTHER
256600000000     C                   EXSR      CTRD2
256700000000   B3C     *IN99         IFEQ      *OFF
256800000000     C                   EXSR      CTRD1
256900130926   B1C                   if        *IN99       = *On
257000000000     C* Spengo gli indicatori di errore per il controllo su altri schermi
257100000000     C* per evitare il conflitto fra gli stessi indicatori usati in divers  e
257200000000     C* videate
257300000000     C                   MOVE      *ALL'0'       IN5098           49
257400000000     C                   MOVEA     IN5098        *IN(50)
257500000000     C                   SETON                                        9996                       96
257600000000     C                   ENDIF
257700000000     C                   ENDIF
257800170803     C* aggiungere le chiamate ai controlli delle videate supplementari
257900170803    >C     *IN99         IFEQ      *OFF
258000170803    >C                   EXSR      CTRD3
258100170803   B1C                   if        *IN99       = *On
258200170803    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
258300170803    >C* indicatori uguali in videate differenti, genererei conflitti
258400170803    >C                   MOVE      *ALL'0'       IN5098           49
258500170803    >C                   MOVEA     IN5098        *IN(50)
258600170803    >C                   SETON                                        9996                       96
258700170803    >C                   ENDIF
258800170803    >C                   ENDIF
258900000000     C*
259000000000   B3C     $TASTO        IFEQ      F06
259100000000     C     *IN99         ANDEQ     *OFF
259200000000     C* eseguo aggiornamento
259300000000     C                   EXSR      AGGANA
259400000000     C* eseguo operazioni del dopo-aggiornamento
259500000000   B4C     *IN99         IFEQ      *OFF
259600000000     C                   EXSR      GESAGG
259700000000   E4C                   ENDIF
259800000000   E3C                   ENDIF
259900000000     C*
260000000000   E2C                   ENDSL
260100000000     C*
260200000000   E1C                   ENDIF
260300000000     C*
260400000000    >C                   ENDSR
260500000000     C/EJECT
260600000000     C************************************************************
260700000000     C* GESTIONE F05 VIDEO D2
260800000000     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
260900000000     C* e il riposizionamento alla prima
261000000000     C************************************************************
261100000000    >C     F05D2         BEGSR
261200000000     C*
261300000000     C                   MOVE      *ON           $INZD1
261400000000     C                   MOVE      'D1'          $GEST
261500000000     C*
261600000000    >C                   ENDSR
261700000000     C************************************************************
261800000000     C* GESTIONE F24 VIDEO D2
261900000000     C************************************************************
262000000000    >C     F24D2         BEGSR
262100000000     C*
262200000000     C                   MOVEA     $FX1          $FX
262300000000     C                   MOVEA     $FC1          $FC
262400000000     C                   MOVEA     $FR1          $FR
262500000000     C                   Z-ADD     $ULKD1        $ULK
262600000000     C                   Z-ADD     $D1L01        $L01
262700000000     C                   Z-ADD     $D1L02        $L02
262800000000     C                   EXSR      RIEKEY
262900000000     C                   Z-ADD     $ULK          $ULKD1            3 0
263000000000     C                   MOVEA     $FC           $FC1
263100000000     C                   MOVEL     $KEY1         Z1KE1
263200000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
263300000000     C                   MOVEL     $KEY2         Z1KE2
263400000000     C                   WRITE     FMTZ1
263500000000     C*
263600000000    >C                   ENDSR
263700170803     C/EJECT
263800000000     C************************************************************
263900000000     C* CONTROLLO VIDEATA  D2
264000000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
264100000000     C* 95 96 97 98
264200000000     C************************************************************
264300000000    >C     CTRD2         BEGSR
264400000000     C*
264500000000     C                   SETOFF                                       99
264600000000     C*
264700000000     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
264800000000     C*
264900170804    >C     OPZI74        IFNE      '04'
265000170803     C*
265100170803     C* Salvo i campi che hanno controlli comuni o che possono essere valorizzati
265200170803     C                   Eval      $Stato  = D1Stato2
265300170803     C                   Eval      $Cap    = D1Cap2
265400170804     C                   Eval      $Prv    = D1Prov2
265500170803     C                   Eval      $Locali = D1Locali2
265600170803     C*
265700170803     C* Stato
265800170803     C                   EXSR      CtrStato
265900170803     C*
266000170803     C                   If        $ErrCtrl = *on
266100170803     C*
266200170804     C                   If        $TipErr  = '1'
266300170803     C                   Seton                                        5999
266400170803     C                   Endif
266500170803     C*
266600170804     C                   If        $TipErr  = '2'
266700170803     C                   Seton                                        8299
266800170803     C                   Endif
266900170803     C*
267000170807     C                   Endif
267100170807     C*
267200170803     C                   Movel     $Statod       D1Stato2D
267300170803     C                   Movel     $IdPaese      D1IdPaes2
267400170803     C*
267500000000     C*
267600170803     C                   Exsr      CtrCap
267700170803     C*
267800170803     C                   If        $ErrCtrl = *on
267900170803     C*
268000170803     C                   If        $TipErr  = '1'
268100170803     C                   Seton                                        7399
268200170803     C                   Endif
268300170803     C*
268400170803     C                   If        $TipErr  = '2'
268500170803     C                   Seton                                        7499
268600170803     C                   Endif
268700170803     C*
268800170803     C                   If        $TipErr  = '3'
268900170803     C                   Seton                                        7899
269000170803     C                   Endif
269100170803     C*
269200170803     C                   Else
269300170804     C                   Movel     $Prv          D1Prov2
269400170803     C                   Movel     $Locali       D1Locali2
269500170803     C                   Endif
269600170803     C*
269700170810     C* Controllo provincia solo se IT
269800170811PP   C                   Movel     *off          $CtrObbl
269900170810     C                   If        $IdPaese = 'IT'
270000170807     C                   Movel     *on           $CtrObbl
270100170810     C                   Endif
270200170810     C*
270300170803     C                   EXSR      CtrPrv
270400170803     C*
270500170803     C                   If        $ErrCtrl = *on
270600170807     C                   IF        $Tiperr = '1'
270700170807     C                   Seton                                        9199
270800170807     C                   Else
270900170807     C                   Seton                                        5899
271000170807     C                   EndIF
271100170803     C                   Else
271200170804     C                   Movel     $Prvd         D1Prov2d
271300170803     C                   Endif
271400000000     C*
271500170803     C* Controllo località
271600170807     C                   Movel     *on           $CtrObbl
271700170803     C                   EXSR      CtrLocali
271800170803     C*
271900170803     C                   If        $ErrCtrl = *on
272000170807     C                   If        $Tiperr = '1'
272100170807     C                   Seton                                        8999
272200170807     C                   Else
272300170807     C                   Seton                                        6999
272400170803     C                   EndIF
272500170807     C                   EndIF
272600000000     C*
272700170803     C* Controllo indirizzo sede
272800170803     C                   Eval      $Indir = D1Indir2
272900170807     C                   Movel     *on           $CtrObbl
273000170803     C                   EXSR      CtrIndir
273100170803     C*
273200170803     C                   If        $ErrCtrl = *on
273300170807     C                   If        $Tiperr = '1'
273400170807     C                   Seton                                        8899
273500170807     C                   Else
273600170803     C                   Seton                                        6899
273700170807     C                   Endif
273800170803     C                   EndIF
273900170803     C*
274000170807     C* Controlli per stabile organizzazione
274100170807     C                   Eval      $Stato  = D1Stato3
274200170807     C                   Eval      $Cap    = D1Cap3
274300170807     C                   Eval      $Prv    = D1Prov3
274400170807     C                   Eval      $Locali = D1Locali3
274500170803     C                   Eval      $Indir = D1Indir3
274600170807     C*
274700170807     C* Stato
274800170807     C                   EXSR      CtrStato
274900170807     C*
275000170807     C                   If        $ErrCtrl = *on
275100170807     C*
275200170807     C                   If        $TipErr  = '1'
275300170807     C                   Seton                                        6199
275400170807     C                   Endif
275500170807     C*
275600170807     C                   If        $TipErr  = '2'
275700170807     C                   Seton                                        8399
275800170807     C                   Endif
275900170807     C*
276000170807     C                   Endif
276100170807     C*
276200170807     C                   Movel     $Statod       D1Stato3D
276300170807     C                   Movel     $IdPaese      D1IdPaes3
276400170807     C* Indirizzo
276500170807     C                   Movel     *off          $CtrObbl
276600170803     C                   EXSR      CtrIndir
276700170803     C*
276800170803     C                   If        $ErrCtrl = *on
276900170803     C                   Seton                                        7099
277000170803     C                   EndIF
277100170807     C*
277200170807     C                   ENDIF
277300170807     C*
277400170807     C                   EndSR
277500000000     C/EJECT
277600170803     C************************************************************
277700170803     C* GESTIONE VIDEO RECORD  D3
277800170803     C************************************************************
277900170803    >C     GESD3         BEGSR
278000170803     C* emissione piede videata se proveniente da altra
278100170803     C* salvataggio valore formato di provenienza
278200170803   B1C     $LASTV        IFNE      'D3'
278300170803     C                   WRITE     FMTT1
278400170803     C                   EXSR      GESPIE
278500170803     C                   MOVE      $LASTV        $LASTG
278600170803     C                   MOVE      'D3'          $LASTV
278700170803   E1C                   ENDIF
278800170803     C*
278900170803     C* se esistono errori sulla videata
279000170803     C* emetto la write del formato a indicatori spenti per vedere
279100170803     C* le eventuali decodifiche
279200170804   B1C                   if        *IN99       = *On   and
279300170803     C                             $GEST        = 'D3'
279400170803     C                   MOVEA     *IN           WIN
279500170803     C                   MOVE      *ALL'0'       IN5098           49
279600170803     C                   MOVEA     IN5098        *IN(50)
279700170803     C                   WRITE     FMTD3
279800170803     C                   MOVEA     WIN           *IN
279900170803   E1C                   ENDIF
280000170803     C* Inizializzazione indice tasti funzione personalizzati
280100170803     C                   Eval      PosFun = 25
280200170803     C*
280300170803   B1C                   SELECT
280400170803     C* Annullamento
280500170803    >C     OPZI74        WHENEQ    '04'
280600170803     C* Visualizzazione
280700170803    >C     OPZI74        OREQ      '05'
280800170803     C                   WRITE     FMTD3
280900170803     C                   EXFMT     PROTECT
281000170803     C* Immissione/Modifica/Copia
281100170803   X1C                   OTHER
281200170803     C                   EXFMT     FMTD3
281300170803   E1C                   ENDSL
281400170803     C*
281500170803     C* azzero la posizione del cursore
281600170803     C                   Z-ADD     0             H1RIGA
281700170803     C                   Z-ADD     0             H1COLO
281800170803     C*
281900170803     C* controllo abilitazione tasti funzione
282000170803     C                   EXSR      CTRKD1
282100170803     C*
282200170803   B1C     *IN99         IFEQ      *OFF
282300170803     C*
282400170803   B2C                   SELECT
282500170803     C* ROLDOWN
282600170803     C     $TASTO        WHENEQ    ROLDWN
282700170803     C                   MOVE      'D2'          $GEST
282800170803     C* F3=Fine
282900170803     C     $TASTO        WHENEQ    F03
283000170803     C                   EXSR      F03D1
283100170803     C* F4=Ricerca
283200170803     C     $TASTO        WHENEQ    F04
283300170803     C                   EXSR      SEARCH
283400170803     C* F5=Refresh
283500170803     C     $TASTO        WHENEQ    F05
283600170803     C                   EXSR      F05D3
283700170803     C* F8=Successivo
283800170803     C     $TASTO        WHENEQ    F08
283900170803     C                   EXSR      F08D1
284000170803     C* F12=Ritorno
284100170803     C     $TASTO        WHENEQ    F12
284200170803     C                   EXSR      F12D1
284300170803     C* F24=Altri tasti
284400170803     C     $TASTO        WHENEQ    F24
284500170803     C                   EXSR      F24D3
284600170803     C* Personalizzata
284700170803     C* NB chiamata NON possibile perchè SDGSI74DS più lunga
284800170803     C* di KPJBU 256 caratteri                          a
284900170803     C**** $PGF1(PosFun) Whenne    *BLANK
285000170803     C****               EXSR      FperD1
285100170803     C*
285200170803   X2C                   OTHER
285300170803     C                   EXSR      CTRD3
285400170805     C* aggiungere le chiamate ai controlli delle videate supplementari
285500170805    >C     *IN99         IFEQ      *OFF
285600170807    >C                   EXSR      CTRD2
285700170805   B1C                   if        *IN99       = *On
285800170805    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
285900170805    >C* indicatori uguali in videate differenti, genererei conflitti
286000170805    >C                   MOVE      *ALL'0'       IN5098           49
286100170805    >C                   MOVEA     IN5098        *IN(50)
286200170805    >C                   SETON                                        9996                       96
286300170805    >C                   ENDIF
286400170805    >C                   ENDIF
286500170803     C* aggiungere le chiamate ai controlli delle videate supplementari
286600170803    >C     *IN99         IFEQ      *OFF
286700170807    >C                   EXSR      CTRD1
286800170803   B1C                   if        *IN99       = *On
286900170803    >C* Spengo gli indicatori di errore perchè, nel caso si siano usati
287000170803    >C* indicatori uguali in videate differenti, genererei conflitti
287100170803    >C                   MOVE      *ALL'0'       IN5098           49
287200170803    >C                   MOVEA     IN5098        *IN(50)
287300170803    >C                   SETON                                        9996                       96
287400170803    >C                   ENDIF
287500170803    >C                   ENDIF
287600170803     C*
287700170803   B3C     $TASTO        IFEQ      F06
287800170803     C     *IN99         ANDEQ     *OFF
287900170803     C* eseguo aggiornamento
288000170803     C                   EXSR      AGGANA
288100170803     C* eseguo operazioni del dopo-aggiornamento
288200170803   B4C     *IN99         IFEQ      *OFF
288300170803     C                   EXSR      GESAGG
288400170803   E4C                   ENDIF
288500170803   E3C                   ENDIF
288600170803     C*
288700170803   E2C                   ENDSL
288800170803     C*
288900170803   E1C                   ENDIF
289000170803     C*
289100170803    >C                   ENDSR
289200170803     C/EJECT
289300170803     C************************************************************
289400170803     C* GESTIONE F05 VIDEO D3
289500170803     C* il tasto F5 su qualsiasi videata provoca il refresh di tutte
289600170803     C* e il riposizionamento alla prima
289700170803     C************************************************************
289800170803    >C     F05D3         BEGSR
289900170803     C*
290000170803     C                   MOVE      *ON           $INZD1
290100170803     C                   MOVE      'D1'          $GEST
290200170803     C*
290300170803    >C                   ENDSR
290400170803     C************************************************************
290500170803     C* GESTIONE F24 VIDEO D2
290600170803     C************************************************************
290700170803    >C     F24D3         BEGSR
290800170803     C*
290900170803     C                   MOVEA     $FX1          $FX
291000170803     C                   MOVEA     $FC1          $FC
291100170803     C                   MOVEA     $FR1          $FR
291200170803     C                   Z-ADD     $ULKD1        $ULK
291300170803     C                   Z-ADD     $D1L01        $L01
291400170803     C                   Z-ADD     $D1L02        $L02
291500170803     C                   EXSR      RIEKEY
291600170803     C                   Z-ADD     $ULK          $ULKD1            3 0
291700170803     C                   MOVEA     $FC           $FC1
291800170803     C                   MOVEL     $KEY1         Z1KE1
291900170803     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
292000170803     C                   MOVEL     $KEY2         Z1KE2
292100170803     C                   WRITE     FMTZ1
292200170803     C*
292300170803    >C                   ENDSR
292400170803     C/EJECT
292500170803     C************************************************************
292600170803     C* CONTROLLO VIDEATA  D3
292700170803     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
292800170803     C* 95 96 97 98
292900170803     C************************************************************
293000170803    >C     CTRD3         BEGSR
293100170803     C*
293200170803     C                   SETOFF                                       99
293300170803     C*
293400170803     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
293500170803     C*
293600170804    >C     OPZI74        IFNE      '04'
293700170803     C*
293800170803     C* Salvo i campi che hanno controlli comuni o che possono essere valorizzati
293900170803     C                   Eval      $Stato  = D1Stato3
294000170803     C                   Eval      $Cap    = D1Cap3
294100170804     C                   Eval      $Prv    = D1Prov3
294200170803     C                   Eval      $Locali = D1Locali3
294300170803     C*  CAP
294400170803     C                   Exsr      CtrCap
294500170803     C*
294600170803     C                   If        $ErrCtrl = *on
294700170803     C*
294800170803     C                   If        $TipErr  = '1'
294900170803     C                   Seton                                        7999
295000170803     C                   Endif
295100170803     C*
295200170803     C                   If        $TipErr  = '2'
295300170803     C                   Seton                                        8099
295400170803     C                   Endif
295500170803     C*
295600170803     C                   If        $TipErr  = '3'
295700170803     C                   Seton                                        8199
295800170803     C                   Endif
295900170803     C*
296000170803     C                   Else
296100170804     C                   Movel     $Prv          D1Prov3
296200170803     C                   Movel     $Locali       D1Locali3
296300170803     C                   Endif
296400170803     C*
296500170803     C* Controllo provincia
296600170807     C                   Movel     *off          $CtrObbl
296700170803     C                   EXSR      CtrPrv
296800170803     C*
296900170803     C                   If        $ErrCtrl = *on
297000170803     C                   Seton                                        6099
297100170803     C                   Else
297200170804     C                   Movel     $Prvd         D1Prov3d
297300170803     C                   Endif
297400170803     C*
297500170803     C* Controllo località
297600170803     C                   EXSR      CtrLocali
297700170803     C*
297800170803     C                   If        $ErrCtrl = *on
297900170803     C                   Seton                                        7199
298000170803     C                   EndIF
298100170803     C*
298200170803     C* Stato Rappresentante fiscale
298300170803     C                   Eval      $stato = d1stato4
298400170803     C*
298500170803     C                   EXSR      CtrStato
298600170803     C*
298700170803     C                   If        $ErrCtrl = *on
298800170803     C*
298900170804     C                   If        $TipErr  = '1'
299000170803     C                   Seton                                        6299
299100170803     C                   Endif
299200170803     C*
299300170804     C                   If        $TipErr  = '2'
299400170803     C                   Seton                                        8499
299500170803     C                   Endif
299600170803     C*
299700170807     C                   Endif
299800170807     C*
299900170803     C                   Movel     $Statod       D1Stato4D
300000170803     C                   Movel     $IdPaese      D1IdPaes4
300100170803     C*
300200170803     C* Salvo i campi che hanno controlli comuni
300300170803     C                   Movel     D1Denom4      $Denom
300400170803     C                   Movel     D1Nome4       $Nome
300500170803     C                   Movel     D1Cognom4     $Cognom
300600170804     C                   If        D1Denom4<> *blanks
300700170804     C                   Movel     D1Denom4      H1CogRag4
300800170804     C                   Else
300900170804     C                   Movel     D1Cognom4     H1CogRag4
301000170804     C                   Endif
301100170803     C                   Movel     D1Tpn4        $Tpn
301200170803     C*
301300170803     C* Tipo Natura giuridica
301400170807     C                   Movel     *Off          $CtrObbl
301500170803     C                   EXSR      CtrTPN
301600170805     C                   Movel     $Tpnd         D1Tpn4d
301700170803     C                   If        $ErrCtrl = *on
301800170803     C                   Seton                                        6499
301900170803     C                   Endif
302000170803     C*
302100170803     C* Denominazione
302200170803     C                   EXSR      CtrDenom
302300170803     C                   If        $ErrCtrl = *on
302400170804     C                   If        $TipErr  = '1'
302500170805     C                   Seton                                        6599
302600170803     C                   Else
302700170805     C                   Seton                                        8599
302800170803     C                   Endif
302900170803     C                   Endif
303000170807     C*
303100170807     C* Cognome
303200170807     C                   EXSR      CtrCognome
303300170807     C                   If        $ErrCtrl = *on
303400170807     C                   If        $TipErr  = '1'
303500170807     C                   Seton                                        6699
303600170807     C                   Else
303700170807     C                   Seton                                        8699
303800170807     C                   Endif
303900170807     C                   Endif
304000170807     C* Nome
304100170807     C                   EXSR      CtrNome
304200170807     C                   If        $ErrCtrl = *on
304300170807     C                   If        $TipErr  = '1'
304400170807     C                   Seton                                        7799
304500170807     C                   Else
304600170807     C                   Seton                                        8799
304700170807     C                   Endif
304800170807     C                   Endif
304900170807     C                   ENDIF
305000170803     C*
305100170803     C                   ENDSR
305200170807     C***********************************************************
305300170807     C     CtrPiva_CDF   BEGSR
305400170807     C*
305500170807     C                   If        D1Piva1 = *blanks  and
305600170807     C                             D1Cdfisc1 = *blanks
305700170811Fm3  C                   if        D1IdPaes1 = 'OO'
305800170811fm3  C                   Eval      D1Piva1   = '99999999999'
305900170811fm3  C                   else
306000170807     C                   Seton                                        535499
306100170811fm3  C                   Endif
306200170807     C                   Endif
306300170807     C*
306400170807     C                   ENDSR
306500170803     C/EJECT
306600130919     C***********************************************************
306700130919     C* Controllo Stato
306800130919     C***********************************************************
306900130919     C     CtrStato      BEGSR
307000130919     C*
307100170801     C                   Clear                   $ErrCtrl
307200170807     C                   Clear                   $TipErr
307300170801     C*
307400170801     C                   Clear                   $StatoD
307500170807     C                   Clear                   $IdPaese
307600130919     C*
307700130919     C                   Clear                   AngG12DS
307800130919     C*
307900130923     C                   Clear                   SDGTABDS
308000130919     C*
308100170801     C                   if        $Stato     <> *Blank
308200130919     C                   Eval      XTbRic      = '1'
308300130919     C                   Eval      XTbAzi      = XScSoc
308400130919     C                   Eval      XTbKey      = *Zero
308500130919     C                   Eval      XTbCod      = 'G12'
308600170801     C                   Move      $Stato        XTbKey
308700130919     C*
308800130919     C                   CALL      'SDGTABDR'
308900130919     C                   PARM                    SDGTABDS
309000130919     C*
309100130919     C                   if        XTbErr     <> '0'
309200170807     C*                  Eval      $Errctrl   = *on
309300170807     C*                  Eval      $TipErr    = '2'
309400170807     C                   endif
309500130919     C                   MoveL     XTbUni        AngG12DS
309600170808     C                   MoveL     DesG12        $StatoD
309700170801     C
309800170801     C* Reperisco il codice Iso da tabelle HD4
309900170805     C                   Clear                   SDGHD4DS
310000170805     C                   Clear                   SDGTABDS
310100170801     C                   Eval      XTbRic      = '1'
310200170801     C                   Eval      XTbAzi      = XScSoc
310300170801     C                   Eval      XTbKey      = *Zero
310400170801     C                   Eval      XTbCod      = 'HD4'
310500170801     C                   Move      $Stato        XTbKey
310600170801     C*
310700170801     C                   CALL      'SDGTABDR'
310800170801     C                   PARM                    SDGTABDS
310900170801     C                   if        XTbErr     <> '0'
311000170803     C                   Eval      $Errctrl   = *on
311100170803     C                   Eval      $TipErr    = '2'
311200170801     C                   else
311300170801     C                   MoveL     XTbUni        SDGHD4DS
311400170801     C                   MoveL     Codiso2Hd4    $IdPaese
311500170808     C                   MoveL     DesHd4        $StatoD
311600170801     C                   Endif
311700130919     C*
311800170807     C**                 Endif
311900170801     C*
312000170801     C                   Endif
312100130930     C*
312200130930     C                   ENDSR
312300170803     C/EJECT
312400000000     C************************************************************
312500000000     C* Controllo Località di residenza
312600000000     C************************************************************
312700170803     C     CtrLocali     BEGSR
312800170803     C*
312900170803     C                   Clear                   $ErrCtrl
313000170807     C                   Clear                   $TipErr
313100170807     C*
313200170807     C                   Select
313300170807     C*
313400170807     C                   When      $Locali    = *Blanks and
313500170807     C                             $CtrObbl = *on
313600170807     C                   Movel     *On           $ErrCtrl
313700170807     C                   Movel     '1'           $TipErr
313800170807     C*
313900170807     C                   When      $Locali    <> *Blanks
314000170807     C*
314100170807     C                   Movel     $Locali       $CampoScan
314200170807     C                   Exsr      ScanCharOk
314300170807     C                   If        $ErrScan = '1'
314400170807     C                   Movel     *on           $Errctrl
314500170807     C                   Movel     '2'           $TipErr
314600170807     C                   endif
314700170807     C*
314800170807     C                   EndSl
314900000000     C*
315000000000     C                   ENDSR
315100000000     C/EJECT
315200000000     C************************************************************
315300170803     C* Controllo indirizzo
315400000000     C************************************************************
315500170803     C     CtrIndir      BEGSR
315600170803     C*
315700170803     C                   Clear                   $ErrCtrl
315800170807     C                   Clear                   $TipErr
315900170807     C*
316000170807     C                   Select
316100170807     C*
316200170807     C                   When      $Indir     = *Blanks and
316300170807     C                             $CtrObbl = *on
316400170807     C                   Movel     *On           $ErrCtrl
316500170807     C                   Movel     '1'           $TipErr
316600170807     C*
316700170807     C                   When      $Indir     <> *Blanks
316800170807     C*
316900170803     C                   Movel     $Indir        $CampoScan
317000170803     C                   Exsr      ScanCharOk
317100170805     C                   If        $ErrScan = '1'
317200170803     C                   Movel     *on           $Errctrl
317300170807     C                   Movel     '2'           $TipErr
317400170805     C                   endif
317500170807     C*
317600170807     C                   EndSl
317700000000     C*
317800000000     C                   ENDSR
317900000000     C/EJECT
318000000000     C************************************************************
318100000000     C* AGGIORNAMENTO ANAGRAFICA
318200000000     C************************************************************
318300000000     C     AGGANA        BEGSR
318400000000     C*
318500000000     C                   SELECT
318600170804    >C     OPZI74        WHENEQ    '01'
318700170804    >C     OPZI74        OREQ      '03'
318800170804     C                   EXSR      AGGIORNA
318900170804    >C     OPZI74        WHENEQ    '02'
319000170804     C                   EXSR      AGGIORNA
319100170804    >C     OPZI74        WHENEQ    '04'
319200170804     C                   EXSR      AGGIORNA
319300170807    >C     OPZI74        WHENEQ    '20'
319400170807     C                   EXSR      AGGIOMPE
319500000000     C                   ENDSL
319600170807     C
319700170807     C* se rcd non scrivibile attivo errore
319800170807     C     *IN22         IFEQ      *ON
319900170807     C                   SETON                                        9599
320000170807     C                   ENDIF
320100000000     C*
320200000000     C* richiama pgm User Exit
320300000000     C     USRERR        IFEQ      *BLANK
320400000000     C* passa opzione ricevuta a user exit
320500170804    >C                   MOVEL     OPZI74        XUKOPZ
320600170809     C* imposta chiave univoca  per user exit
320700170809     C* la chiave univoca non esiste perchè sto lavorando
320800170809     C* con raggruppamento di dati
320900000000     C                   MOVE      XSCSOC        XUKSOC
321000000000     C                   MOVE      XSCDUN        XUKDUN
321100000000     C                   MOVEL     DSPGM         XUKPGM
321200000000    >C                   MOVEL     *Blank        XUKFL1
321300000000    >C                   MOVEL     *Blank        XUKFL2
321400000000    >C* ho fino a 6 campi chiave passabili
321500000000     C                   MOVEL     XUKDS         KPJBU
321600000000     C     DSPGM         CAT       'U':0         USREXT           10
321700000000     C                   CALL      USREXT                               21
321800000000     C                   PARM                    KPJBA
321900000000     C     *IN21         IFEQ      *ON
322000000000     C                   MOVE      *ON           USRERR            1
322100000000     C                   ENDIF
322200000000     C                   ENDIF
322300000000     C*
322400000000     C                   ENDSR
322500000000     C/EJECT
322600000000     C************************************************************
322700170804     C* VALORIZZA E CHIAMA FUNZIONE DI AGGIORNAMENTO
322800000000     C************************************************************
322900170804     C     AGGIORNA      BEGSR
323000170804     C*
323100170804     C* Prima di richiamare l'aggiornamento svincolo il record
323200170804     C                   If        NRRCSTI74 > 0
323300170808     C                   Unlock    SDGCST00F
323400170807     C                   Close     SDGCST00F
323500170804     C                   Else
323600170808     C                   Unlock    SDGCST02L
323700170808     C                   Close     SDGCST02L
323800170804     C                   Endif
323900000000     C*
324000170804     C                   CLEAR                   SDG00702DS
324100000000     C*
324200170804     C                   Eval      Opz0702 = OpzI74
324300170804     C                   Eval      Cmt0702 = CmtI74
324400170804     C*  Dati della chiave
324500170809     C                   Eval      Soc0702   =  XscSoc
324600170809     C                   Eval      TpCom0702 =  TpComI74
324700170809     C                   Eval      TpRec0702 =  TpRecI74
324800170809     C                   Eval      Anno0702  =  AnnoI74
324900170809     C                   Eval      Trim0702  =  TrimI74
325000170809     C                   Eval      Mese0702  =  MeseI74
325100170804     C*
325200170804     C                   Eval      CLIFOR0702=  SavCliFor
325300170804     C*
325400170809     C                   Eval      NrrCST0702=  NrrCSTI74
325500170804     C*
325600170804     C* Dati relativi al filtro
325700170809     C                   Eval      Causal0702=  CAUSALEI74
325800170809     C                   Eval      Ann0702   =  ANNI74
325900170809     C                   Eval      TPAnn0702 =  TPANNI74
326000170804     C*
326100170804     C* Dati Anagrafici da scrivere modificare
326200170809     C                   Eval      CF0702      = CfInpI74
326300170809     C                   Eval      Sogg0702    = D1SOGG
326400170809     C                   Eval      CF0702      = D1CF
326500170809     C                   Eval      Partiv0702  = D1Partiva
326600170809     C                   Eval      CdFisc0702  = D1CDFisc
326700170809     C                   Eval      Stato10702  = D1Stato1
326800170809     C                   Eval      IDPae10702  = D1IDPaes1
326900170804     C                   Eval      PIVA10702   = D1PIVA1
327000170809     C                   Eval      CDFis10702  = D1CDFisc1
327100170809     C                   Eval      Cogra10702  = H1Cograg1
327200170804     C                   Eval      TPN10702    = D1TPN1
327300170809     C                   Eval      Denom10702  = D1Denom1
327400170809     C                   Eval      Cogno10702  = D1Cognom1
327500170809     C                   Eval      Nome10702   = D1Nome1
327600170809     C                   Eval      Indir20702  = D1Indir2
327700170809     C                   Eval      NrCiv20702  = D1NrCiv2
327800170804     C                   Eval      CAP20702    = D1CAP2
327900170809     C                   Eval      Local20702  = D1Locali2
328000170809     C                   Eval      Prov20702   = D1Prov2
328100170809     C                   Eval      Stato20702  = D1Stato2
328200170809     C                   Eval      IDPae20702  = D1IDPaes2
328300170809     C                   Eval      Indir30702  = D1Indir3
328400170809     C                   Eval      NrCiv30702  = D1NrCiv3
328500170804     C                   Eval      CAP30702    = D1CAP3
328600170809     C                   Eval      Local30702  = D1Locali3
328700170809     C                   Eval      Prov30702   = D1Prov3
328800170809     C                   Eval      Stato30702  = D1Stato3
328900170809     C                   Eval      IDPae30702  = D1IDPaes3
329000170809     C                   Eval      Stato40702  = D1Stato4
329100170809     C                   Eval      IDPae40702  = D1IDPaes4
329200170804     C                   Eval      PIVA40702   = D1PIVA4
329300170809     C                   Eval      Cogra40702  = H1Cograg4
329400170804     C                   Eval      TPN40702    = D1TPN4
329500170809     C                   Eval      Denom40702  = D1Denom4
329600170809     C                   Eval      Cogno40702  = D1Cognom4
329700170809     C                   Eval      Nome40702   = D1Nome4
329800170804     C*
329900170804     C                   CALLB     'SDG00702R'
330000170804     C                   PARM                    KPJBA
330100170804     C                   PARM                    SDG00702DS
330200170804     C*
330300170804     C                   If        Err0702  = *on
330400170804     C                   Seton                                        22
330500170804     C                   Endif
330600170804     C*
330700000000     C                   ENDSR
330800170807     C/EJECT
330900170807     C************************************************************
331000170807     C* VALORIZZA ANMPE
331100170807     C************************************************************
331200170807     C     AGGIOMPE      BEGSR
331300170807     C*
331400170807     C*
331500170807     C                   Eval      WIdPaes1 = D1IdPaes1
331600170807     C                   Eval      WPIVA1   = D1PIVA1
331700170807     C                   Eval      WCdFisc1 = D1CdFisc1
331800170807     C*
331900170807     C                   Eval      WDENOM1  = D1DENOM1
332000170807     C                   Eval      WCOGNOM1 = D1COGNOM1
332100170807     C                   Eval      WNOME1   = D1NOME1
332200170807     C                   Eval      WINDIR2  = D1INDIR2
332300170807     C                   Eval      WNRCIV2  = D1NRCIV2
332400170807     C                   Eval      WCAP2    = D1CAP2
332500170807     C                   Eval      WLOCALI2 = D1LOCALI2
332600170807     C                   Eval      WPROV2   = D1PROV2
332700170807     C*
332800170807     C                   Eval      WIDPAES2 = D1IDPAES2
332900170807     C*
333000170807     C                   Eval      WINDIR3  = D1INDIR3
333100170807     C                   Eval      WNRCIV3  = D1NRCIV3
333200170807     C                   Eval      WCAP3    = D1CAP3
333300170807     C                   Eval      WLOCALI3 = D1LOCALI3
333400170807     C                   Eval      WPROV3   = D1PROV3
333500170807     C*
333600170807     C                   Eval      WIDPAES3 = D1IDPAES3
333700170807     C*
333800170807     C                   Eval      WIDPAES4 = D1IDPAES4
333900170807     C                   Eval      WPIVA4   = D1PIVA4
334000170807     C*
334100170807     C                   Eval      WDENOM4  = D1DENOM4
334200170807     C                   Eval      WNOME4   = D1NOME4
334300170807     C                   Eval      WCOGNOM4 = D1COGNOM4
334400170807     C* preparo i parametri di chiamata al driver
334500170807     C                   Clear                   XMPEDS
334600170807     C                   EVAL      TpChiamata = '03'
334700170807     C                   EVAL      NChiavi = 5
334800170807     C                   EVAL      VLogica = 2
334900170807     C*
334901170905R265 X***                Do        4             $Cont
334902170905R265 C                   Do        $DimSk        $Cont
335100170807     C                   Clear                   XMPEDS1
335200170807     C                   Eval      DSMPEPGM = 'SDGSI74R' +
335300170807     C                                        %char($cont)
335400170807     C* preparo le chiavi per la chiamata al driver
335500170807     C                   MoveL     XSCSOC        DSMPESOC
335600170807     C                   MoveL     *Blanks       DSMPEPRF
335700170807     C* Si reperiscono i dati di default a seconda delle modalità con
335800170807     C* cui il pgm è usato
335900170807     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
336000170807     C* MPETIP = 'PV'  pgm richiamato da menù
336100170807     C* MPETIP = 'PB'  x lancio Batch
336200170807     C                   MoveL     'PV'          DSMPETIP
336300170807     C*
336400170807     C                   select
336500170807     C                   When      $Cont   = 1
336600170807     C                   MoveL     DsFrontesp1   DSMPEPAR
336700170807     C                   When      $Cont   = 2
336800170807     C                   MoveL     DsFrontesp2   DSMPEPAR
336900170807     C                   When      $Cont   = 3
337000170807     C                   MoveL     DsFrontesp3   DSMPEPAR
337100170807     C                   When      $Cont   = 4
337200170807     C                   MoveL     DsFrontesp4   DSMPEPAR
337201170905R265 C                   When      $Cont   = 5
337202170905R265 C                   MoveL     DsFrontesp41  DSMPEPAR
337300170807     C                   EndSl
337400170807     C*
337500170807     C                   CALLB     'XMPE'
337600170807     C                   PARM                    XMPEDS
337700170807     C                   PARM                    XMPEDS1
337800170807     C*
337900170807     C                   Enddo
338000170807     C*
338100170807     C                   If        Risultato = '1'
338200170807     C                   Seton                                        22
338300170807     C                   Endif
338400170807     C*
338500170807     C                   ENDSR
338600170804     C/EJECT
338700000000     C************************************************************
338800000000     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
338900000000     C************************************************************
339000000000     C     GESAGG        BEGSR
339100000000     C*
339200000000     C* eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
339300170804    >C     CMTI74        IFEQ      '0'
339400170804     C                   COMMIT
339500000000     C                   ENDIF
339600000000     C*
339700000000     C* segnalo al pgm chiamante l'avvenuta conferma
339800170804    >C                   MOVE      *ON           OPRI74
339900000000     C* segnalo al pgm chiamante l'aver premuto l'F6
340000170804    >C                   MOVE      '0'           RETI74
340100000000     C*
340200000000     C                   SELECT
340300000000     C*
340400000000     C* nel caso di immissione
340500170804    >C     OPZI74        WHENEQ    '01'
340600170804    >C     OPZI74        orEQ      '03'
340700170809     C* Ripasso i dati inseriti
340800170809     C                   Exsr      DATIRIP
340900170809     C* Ripasso la il numero di record appena scritto
341000170808     C                   EVAL      NRRCSTI74 = NRRCST0702
341100170808     C*
341200000000     C* ripristino la videata iniziale
341300170807     C                   MOVE      *ON           $INZD1
341400000000     C                   MOVE      'D1'          $GEST
341500000000     C*
341600000000     C* forzo il ritorno al pgm chiamante perchè devo chiamare
341700000000     C* i pgm di manutenzione del dettaglio
341800000000     C                   MOVE      *ON           $FINE
341900000000     C*
342000170804    >C                   MOVE      xscsoc        SocietaI74
342100170728     C* passare nei campi appositi della DS SDGSI74DS il valore
342200000000     C* della chiave della VL univoca
342300000000     C*
342400000000     C* nel caso di modifica
342500170804    >C     OPZI74        WHENEQ    '02'
342600000000     C* ripasso i campi che devono essere reimpostati sul sfl
342700170804     C*
342800170809     C* Ripasso i dati modificati
342900170809     C                   Exsr      DATIRIP
343000170804     C*
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
344200170809     C************************************************************
344300170809     C* DATI DA RIPASSARE
344400170809     C************************************************************
344500170809     C     DATIRIP       BEGSR
344600170809     C*
344700170809     C* DATI FISCALI E SEDE
344800170809     C                   Eval      SoggI74    =  D1Sogg
344900170809     C                   Eval      PartIVAI74 =  D1Partiva
345000170809     C                   Eval      CdFiscI74  =  D1CdFisc
345100170809     C                   Eval      CfI74      =  D1CF
345200170809     C                   Eval      Stato1I74  =  D1Stato1
345300170809     C                   Eval      IdPaes1I74 =  D1IdPaes1
345400170809     C                   Eval      PIVA1I74   =  D1PIVA1
345500170809     C                   Eval      CdFisc1I74 =  D1CdFisc
345600170809     C*
345700170809     C                   Eval      CogRag1I74 =  H1CogRag1
345800170809     C                   Eval      TpN1I74    =  D1TpN1
345900170809     C                   Eval      Denom1I74  =  D1Denom1
346000170809     C                   Eval      Cognom1I74 =  D1Cognom1
346100170809     C                   Eval      Nome1I74   =  D1Nome1
346200170809     C*
346300170809     C                   Eval      Indir2I74  =  D1Indir2
346400170809     C                   Eval      NrCiv2I74  =  D1NrCiv2
346500170809     C                   Eval      CAP2I74    =  D1CAP2
346600170809     C                   Eval      Locali2I74 =  D1Locali2
346700170809     C                   Eval      Prov2I74   =  D1Prov2
346800170809     C                   Eval      Stato2I74  =  D1Stato2
346900170809     C                   Eval      IdPaes2I74 =  D1IdPaes2
347000170809     C*
347100170809     C* STABILE ORGANIZZAZIONE
347200170809     C                   Eval      Indir3I74  =  D1Indir3
347300170809     C                   Eval      NrCiv3I74  =  D1NrCiv3
347400170809     C                   Eval      CAP3I74    =  D1CAP3
347500170809     C                   Eval      Locali3I74 =  D1Locali3
347600170809     C                   Eval      Prov3I74   =  D1Prov3
347700170809     C                   Eval      Stato3I74  =  D1Stato3
347800170809     C                   Eval      IdPaes3I74 =  D1IdPaes3
347900170809     C*
348000170809     C* RAPPRESENTANTE FISCALE
348100170809     C                   Eval      Stato4I74  =  D1Stato4
348200170809     C                   Eval      IdPaes4I74 =  D1IdPaes4
348300170809     C                   Eval      PIVA4I74   =  D1PIVA4
348400170809     C*
348500170809     C                   Eval      CogRag4I74 =  H1CogRag4
348600170809     C                   Eval      TpN4I74    =  D1TpN4
348700170809     C                   Eval      Denom4I74  =  D1Denom4
348800170809     C                   Eval      Cognom4I74 =  D1Cognom4
348900170809     C                   Eval      Nome4I74   =  D1Nome4
349000170809     C*
349100170809     C                   ENDSR
349200170809     C/EJECT
349300000000     C************************************************************
349400000000     C* CONTROLLO ABILITAZIONE TASTI
349500000000     C************************************************************
349600000000     C     CTRKEY        BEGSR
349700000000     C*
349800000000     C                   SELECT
349900000000     C     $TASTO        WHENEQ    F01
350000000000     C                   Z-ADD     01            PosFun
350100000000     C     $FC(1)        IFEQ      '0'
350200000000     C                   SETON                                        99
350300000000     C                   ENDIF
350400000000     C     $TASTO        WHENEQ    F02
350500000000     C                   Z-ADD     02            PosFun
350600000000     C     $FC(2)        IFEQ      '0'
350700000000     C                   SETON                                        99
350800000000     C                   ENDIF
350900000000     C     $TASTO        WHENEQ    F03
351000000000     C                   Z-ADD     03            PosFun
351100000000     C     $FC(3)        IFEQ      '0'
351200000000     C                   SETON                                        99
351300000000     C                   ENDIF
351400000000     C     $TASTO        WHENEQ    F04
351500000000     C                   Z-ADD     04            PosFun
351600000000     C     $FC(4)        IFEQ      '0'
351700000000     C                   SETON                                        99
351800000000     C                   ENDIF
351900000000     C     $TASTO        WHENEQ    F05
352000000000     C                   Z-ADD     05            PosFun
352100000000     C     $FC(5)        IFEQ      '0'
352200000000     C                   SETON                                        99
352300000000     C                   ENDIF
352400000000     C     $TASTO        WHENEQ    F06
352500000000     C                   Z-ADD     06            PosFun
352600000000     C     $FC(6)        IFEQ      '0'
352700000000     C                   SETON                                        99
352800000000     C                   ENDIF
352900000000     C     $TASTO        WHENEQ    F07
353000000000     C                   Z-ADD     07            PosFun
353100000000     C     $FC(7)        IFEQ      '0'
353200000000     C                   SETON                                        99
353300000000     C                   ENDIF
353400000000     C     $TASTO        WHENEQ    F08
353500000000     C                   Z-ADD     08            PosFun
353600000000     C     $FC(8)        IFEQ      '0'
353700000000     C                   SETON                                        99
353800000000     C                   ENDIF
353900000000     C     $TASTO        WHENEQ    F09
354000000000     C                   Z-ADD     09            PosFun
354100000000     C     $FC(09)       IFEQ      '0'
354200000000     C                   SETON                                        99
354300000000     C                   ENDIF
354400000000     C     $TASTO        WHENEQ    F10
354500000000     C                   Z-ADD     10            PosFun
354600000000     C     $FC(10)       IFEQ      '0'
354700000000     C                   SETON                                        99
354800000000     C                   ENDIF
354900000000     C     $TASTO        WHENEQ    F11
355000000000     C                   Z-ADD     11            PosFun
355100000000     C     $FC(11)       IFEQ      '0'
355200000000     C                   SETON                                        99
355300000000     C                   ENDIF
355400000000     C     $TASTO        WHENEQ    F12
355500000000     C                   Z-ADD     12            PosFun
355600000000     C     $FC(12)       IFEQ      '0'
355700000000     C                   SETON                                        99
355800000000     C                   ENDIF
355900000000     C     $TASTO        WHENEQ    F13
356000000000     C                   Z-ADD     13            PosFun
356100000000     C     $FC(13)       IFEQ      '0'
356200000000     C                   SETON                                        99
356300000000     C                   ENDIF
356400000000     C     $TASTO        WHENEQ    F14
356500000000     C                   Z-ADD     14            PosFun
356600000000     C     $FC(14)       IFEQ      '0'
356700000000     C                   SETON                                        99
356800000000     C                   ENDIF
356900000000     C     $TASTO        WHENEQ    F15
357000000000     C                   Z-ADD     15            PosFun
357100000000     C     $FC(15)       IFEQ      '0'
357200000000     C                   SETON                                        99
357300000000     C                   ENDIF
357400000000     C     $TASTO        WHENEQ    F16
357500000000     C                   Z-ADD     16            PosFun
357600000000     C     $FC(16)       IFEQ      '0'
357700000000     C                   SETON                                        99
357800000000     C                   ENDIF
357900000000     C     $TASTO        WHENEQ    F17
358000000000     C                   Z-ADD     17            PosFun
358100000000     C     $FC(17)       IFEQ      '0'
358200000000     C                   SETON                                        99
358300000000     C                   ENDIF
358400000000     C     $TASTO        WHENEQ    F18
358500000000     C                   Z-ADD     18            PosFun
358600000000     C     $FC(18)       IFEQ      '0'
358700000000     C                   SETON                                        99
358800000000     C                   ENDIF
358900000000     C     $TASTO        WHENEQ    F19
359000000000     C                   Z-ADD     19            PosFun
359100000000     C     $FC(19)       IFEQ      '0'
359200000000     C                   SETON                                        99
359300000000     C                   ENDIF
359400000000     C     $TASTO        WHENEQ    F20
359500000000     C                   Z-ADD     20            PosFun
359600000000     C     $FC(20)       IFEQ      '0'
359700000000     C                   SETON                                        99
359800000000     C                   ENDIF
359900000000     C     $TASTO        WHENEQ    F21
360000000000     C                   Z-ADD     21            PosFun
360100000000     C     $FC(21)       IFEQ      '0'
360200000000     C                   SETON                                        99
360300000000     C                   ENDIF
360400000000     C     $TASTO        WHENEQ    F22
360500000000     C                   Z-ADD     22            PosFun
360600000000     C     $FC(22)       IFEQ      '0'
360700000000     C                   SETON                                        99
360800000000     C                   ENDIF
360900000000     C     $TASTO        WHENEQ    F23
361000000000     C                   Z-ADD     23            PosFun
361100000000     C     $FC(23)       IFEQ      '0'
361200000000     C                   SETON                                        99
361300000000     C                   ENDIF
361400000000     C     $TASTO        WHENEQ    F24
361500000000     C                   Z-ADD     24            PosFun
361600000000     C     $FC(24)       IFEQ      '0'
361700000000     C                   SETON                                        99
361800000000     C                   ENDIF
361900000000     C                   ENDSL
362000000000     C*
362100000000     C                   ENDSR
362200000000     C/EJECT
362300000000     C******************************************************
362400000000     C* PGM PER GESTIONE MESSAGGI ERRORE
362500000000     C******************************************************
362600000000     C     MSGPGM        BEGSR
362700000000     C*
362800000000     C                   Z-ADD     2             MSGMSG
362900000000     C                   MOVEL     'S'           MSGRSP
363000000000     C                   MOVEL     'CR'          MSGTPR
363100000000     C                   MOVEL     'S'           MSGEMV
363200000000     C                   Z-ADD     10            MSGRGP
363300000000     C                   MOVEL     'S'           MSGLCK
363400000000     C                   MOVEA     STATUS        STS
363500000000     C                   MOVEL     'N'           MSGCNL
363600000000     C                   MOVEL     'N'           MSGVID
363700000000     C                   MOVEL     'N'           MSGOPE
363800000000     C                   MOVEL     'N'           MSGSTP
363900000000     C                   CALL      'SDGMSGR'
364000000000     C                   PARM                    SDGMSGDS
364100000000     C*
364200000000     C                   ENDSR
364300000000     C/EJECT
364400000000     C************************************************************
364500000000     C* OPERAZIONI INIZIALI
364600000000     C************************************************************
364700000000     C     *INZSR        BEGSR
364800000000     C*
364900000000     C* Reperimento parametri
365000000000     C*
365100000000     C     *ENTRY        PLIST
365200000000     C                   PARM                    KPJBA
365300170804     C                   PARM                    SDGSI74DS
365400000000     C*
365500000000     C* Reperimento dati società
365600000000     C*
365700000000     C                   MOVEL     'SOC001'      TIPXSC
365800000000     C                   MOVEL     *BLANK        SOCXSC
365900000000     C                   EXSR      REPSOC
366000000000     C*
366100000000     C* Controlli dati società
366200000000     C*
366300000000     C     RTNXSC        IFNE      '1'
366400000000     C                   MOVEL     XSOCDS        SOC001
366500000000     C                   ELSE
366600170804    >C                   MOVE      '1'           ErrI74
366700000000     C                   MOVE      *on           $Fine
366800000000     C                   EXSR      EndPgm
366900000000     C                   ENDIF
367000000000     C*
367100000000     C                   MOVE      *BLANK        AUTDS
367200000000     C*
367300000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
367400000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
367500000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
367600000000     C*
367700000000     C* Reperimento tasti di funzione
367800000000     C*
367900000000     C                   EXSR      REPKEY
368000000000     C*
368100000000     C* Richiamo routine per aggiunta tasti funzione personalizzati
368200000000     C                   CAllb     'X46USREXT'
368300000000     C                   PARM                    *omit
368400000000     C                   PARM                    *omit
368500000000     C                   PARM                    *omit
368600000000     C                   PARM                    *omit
368700000000     C                   PARM                    *omit
368800000000     C                   PARM                    $FX1
368900000000     C                   PARM                    $FC1
369000000000     C                   PARM                    $PGF1
369100000000     C                   PARM                    DsPgm
369200000000     C                   PARM                    xsccut
369300000000     C                   PARM                    xscgrp
369400000000     C*
369500000000     C     *DMY          Move      UDate         UDateISO
369600000000     C*
369700000000     C* reperisco DATFMT del job
369800000000     C                   CallB     'XDATFMT'
369900000000     C                   Parm                    $DatFmt           4
370000000000     C*
370100000000     C                   ENDSR
370200000000     C/EJECT
370300000000     C************************************************************
370400000000     C* REPERIMENTO TASTI DI FUNZIONE
370500000000     C************************************************************
370600000000     C     REPKEY        BEGSR
370700000000     C*
370800000000     C                   MOVEA     $FM1          $FM
370900000000     C                   MOVEA     $FC1          $FC
371000000000     C                   EXSR      REPKKK
371100000000     C                   MOVEA     $FX           $FX1
371200000000     C*
371300000000     C                   ENDSR
371400000000     C/EJECT
371500000000     C************************************************************
371600000000     C* REPERIMENTO TASTI DI FUNZIONE
371700000000     C************************************************************
371800000000     C     REPKKK        BEGSR
371900000000     C*
372000000000     C                   CLEAR                   $FX
372100000000     C     1             DO        24            X
372200000000     C*
372300000000     C     $FM(X)        IFNE      *BLANK
372400000000     C     $FC(X)        ANDNE     '0'
372500000000     C*
372600000000     C                   MOVEL     $FM(X)        IDMSG
372700000000     C                   CALLB     'XRTVM'
372800000000     C                   PARM                    IDMSG            27
372900000000     C                   PARM                    TXTMSG          100
373000000000     C                   PARM                    ERRMSG            1
373100000000     C*
373200000000     C     ERRMSG        IFNE      *ON
373300000000     C                   MOVEL     TXTMSG        $FX(X)
373400000000     C                   ELSE
373500000000     C                   MOVEL     *ALL' '       $FX(X)
373600000000     C                   ENDIF
373700000000     C*
373800000000     C                   ENDIF
373900000000     C*
374000000000     C                   ENDDO
374100000000     C*
374200000000     C                   ENDSR
374300000000     C/EJECT
374400000000     C************************************************************
374500000000     C* INIZIALIZZAZIONE VARIABILI
374600000000     C************************************************************
374700000000     C     INZVAR        BEGSR
374800170807     C*
374900170807     C* Attiva Commitment control se necessario
375000170807     C                   IF        CMTI74      = '0'
375100170807    >C                   CALLB     'XSTRCMT'
375200170808     C                   IF        NRRCSTI74 > 0
375300170808    >C                   OPEN      SDGCST00F
375400170808     C                   ELSE
375500170808    >C                   OPEN      SDGCST02L
375600170808     C                   ENDIF
375700170807     C                   ENDIF
375800000000     C*
375900000000     C* Dati da PGM guida
376000170805    >C*****              MOVEL     KPJBU         SDGSI74DS
376100000000     C*
376200000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
376300000000     C* reperimento dati società
376400170804    >C                   If        SocietaI74 <> xscsoc    and
376500170804    >C                             OpzI74 <> *blank
376600000000     C                   MOVEL     'SOC001'      TIPXSC
376700170804    >C                   MOVEL     SocietaI74    SOCXSC
376800000000     C                   EXSR      REPSOC
376900000000     C     RTNXSC        IFNE      '1'
377000000000     C                   MOVEL     XSOCDS        SOC001
377100000000     C                   ELSE
377200170804    >C                   MOVE      '1'           ErrI74
377300000000     C                   MOVE      *on           $Fine
377400000000     C                   EXSR      EndPgm
377500000000     C                   ENDIF
377600000000     C                   endif
377700000000     C*
377800000000     C* Reperimento autorizzazioni
377900000000     C*
378000000000     C* le autorizzazioni potrebbero essere già state passate,
378100000000     C* nel qual caso non vanno reperite
378200000000    >C     AUTDS         IFEQ      *BLANK
378300000000    >C                   EXSR      REPAUT
378400000000    >C                   ELSE
378500000000    >C                   MOVEL     XCADDS        CONCIVDS
378600000000    >C                   ENDIF
378700000000     C*
378800000000     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
378900000000     C* le autorizzazione
379000000000     C*
379100000000    >C                   EXSR      TSTAUT
379200000000     C*
379300000000     C* Definizione dei tasti funzione utilizzabili a pgm
379400000000     C*
379500000000     C                   EXSR      DEFOPZ
379600000000     C*
379700000000     C* Variabili per gestione videate
379800000000     C*
379900000000     C                   MOVE      'D1'          $GEST                          fmt in gestione
380000000000     C                   MOVE      *OFF          $FINE                          fine pgm
380100000000     C                   MOVE      *OFF          $FlgDef                        fine pgm
380200000000     C                   MOVE      *ON           $INZD1                         inizializzione D1
380300000000     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
380400000000     C                   MOVE      *BLANK        $LASTV                         ultima emissione
380500000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
380600000000     C* azzero la posizione del cursore
380700000000     C                   Z-ADD     0             H1RIGA
380800000000     C                   Z-ADD     0             H1COLO
380900000000     C*
381000000000     C* Variabili appoggio
381100000000     C*
381200000000     C                   Z-ADD     0             CURR
381300000000     C                   Z-ADD     0             CURC
381400000000     C*
381500000000     C* Valorizzazione campi univoci testate
381600000000     C*
381700000000     C                   CLEAR                   FMTT1
381800000000     C                   MOVEL     KNSIF         NOMSIF
381900000000     C                   MOVEL     XSCDSI        NOMDIT
382000000000     C                   MOVEL     DSPGM         NOMPGM           10
382100000000     C                   MOVEL     KCDAZ         NOMAZN
382200000000     C* recupero il sottotitolo (se c'è)
382300000000     C                   MOVEL     *BLANK        T1TIT
382400170808     C                   if        OpzI74      = '20'
382500170808     C                   Eval      $Sottit     = SottitSoc
382600170808     C                   else
382700170808     C                   Eval      $Sottit     = SottitSog
382800170808     C                   endif
382900170808     C     $Sottit       IFNE      *BLANK
383000170808     C                   MOVEL     $Sottit       IDMSG
383100000000     C                   CALLB     'XRTVM'
383200000000     C                   PARM                    IDMSG            27
383300000000     C                   PARM                    TXTMSG          100
383400000000     C                   PARM                    ERRMSG            1
383500000000     C                   PARM      *ON           CTRMSG            1            centratura
383600000000     C                   PARM      30            LENMSG            3 0          lun output.
383700000000     C     ERRMSG        IFNE      *ON
383800000000     C                   MOVEL     TXTMSG        T1TIT
383900000000     C                   ELSE
384000000000     C                   MOVEL     *ALL'?'       T1TIT
384100000000     C                   ENDIF
384200000000     C                   ENDIF
384300000000     C* recupero il tipo operazione
384400000000     C                   Z-ADD     1             X
384500170804    >C     OPZI74        LOOKUP    $OZ(X)                                 21
384600000000     C     *IN21         IFEQ      *ON
384700000000     C                   MOVEL     $ID(X)        IDMSG
384800000000     C                   CALLB     'XRTVM'
384900000000     C                   PARM                    IDMSG            27
385000000000     C                   PARM                    TXTMSG          100
385100000000     C                   PARM                    ERRMSG            1
385200000000     C                   PARM      *Off          CTRMSG            1            centratura
385300000000     C                   PARM      15            LENMSG            3 0          lun output.
385400000000     C     ERRMSG        IFNE      *ON
385500000000     C                   MOVEL     TXTMSG        T1OPE
385600000000     C                   ELSE
385700000000     C                   MOVEL     *ALL'?'       T1OPE
385800000000     C                   ENDIF
385900000000     C                   ENDIF
386000000000     C*
386100000000     C* Inizializzazione/Decodifica prima videata
386200000000     C                   EXSR      REPDEF
386300000000     C                   EXSR      INZD1
386400000000     C                   MOVE      *OFF          $INZD1
386500000000     C*
386600170804     C                   EXSR      RepSDGSPETRI
386700130918     C*
386800000000     C                   ENDSR
386900000000     C/EJECT
387000000000     C************************************************************
387100000000     C* REPERIMENTO DATI SOCIETA'
387200000000     C************************************************************
387300000000     C     REPSOC        BEGSR
387400000000     C*
387500000000     C                   CALL      'SDGSOCDR'
387600000000     C                   PARM                    TIPXSC            6
387700000000     C                   PARM                    SOCXSC            3
387800000000     C                   PARM                    CDSXSC            9 0
387900000000     C                   PARM                    MODXSC            3
388000000000     C                   PARM      *BLANKS       RTNXSC            1
388100000000     C                   PARM                    XSOCDS
388200000000     C                   PARM                    KPJBA
388300000000     C*
388400000000     C                   ENDSR
388500000000     C/EJECT
388600000000     C************************************************************
388700000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
388800000000     C************************************************************
388900000000     C     REPAUT        BEGSR
389000000000     C*
389100000000     C                   CLEAR                   AUTDS
389200000000     C                   Clear                   ConCivDs
389300000000     C*
389400000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
389500000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
389600000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
389700000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
389800000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
389900000000    >C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
390000000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
390100000000    >C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
390200000000     C*
390300000000     C                   CALLB     'XCHKAUT'
390400000000     C                   PARM                    AUTDS
390500000000     C*
390600000000     C* errori/non abilitazione
390700000000     C     XCARTN        IFNE      0
390800170804    >C                   MOVE      '2'           ErrI74
390900000000     C                   MOVE      *ON           $FINE
391000000000     C                   EXSR      ENDPGM
391100000000     C                   ELSE
391200000000    >C                   MOVEL     XCADDS        CONCIVDS
391300000000     C                   ENDIF
391400000000     C*
391500000000     C                   ENDSR
391600000000     C/EJECT
391700000000     C************************************************************
391800000000     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
391900000000     C************************************************************
392000000000     C     TSTAUT        BEGSR
392100000000     C*
392200000000     C                   SELECT
392300000000     C*
392400000000     C* se richiamato in immissione
392500170804    >C     OPZI74        WHENEQ    '01'
392600000000     C* ma l'utente non è abilitato
392700000000    >C     IMMALL        IFEQ      *off
392800000000     C* esco da pgm avvertendolo
392900170804    >C                   MOVE      '2'           ERRI74
393000000000     C                   MOVE      *ON           $FINE
393100000000     C                   EXSR      ENDPGM
393200000000     C                   ENDIF
393300000000     C*
393400000000     C* se richiamato in modifica
393500170804    >C     OPZI74        WHENEQ    '02'
393600000000     C* ma l'utente non è abilitato
393700000000    >C     MANALL        IFEQ      *OFF
393800000000     C* testo se l'utente è abilitato almeno alla visualizzazione
393900000000    >C     VISALL        IFEQ      *ON
394000170804    >C                   MOVE      '05'          OPZI74
394100000000     C                   ELSE
394200000000     C* altrimenti esco da pgm avvertendolo
394300170804    >C                   MOVE      '2'           ERRI74
394400000000     C                   MOVE      *ON           $FINE
394500000000     C                   EXSR      ENDPGM
394600000000     C                   ENDIF
394700000000     C                   ENDIF
394800000000     C*
394900000000     C* se richiamato in copia
395000170804    >C     OPZI74        WHENEQ    '03'
395100000000     C* ma l'utente non è abilitato
395200000000    >C     IMMALL        IFEQ      *OFF
395300000000     C* esco da pgm avvertendolo
395400170804    >C                   MOVE      '2'           ERRI74
395500000000     C                   MOVE      *ON           $FINE
395600000000     C                   EXSR      ENDPGM
395700000000     C                   ENDIF
395800000000     C*
395900000000     C* se richiamato in annullamento
396000170804    >C     OPZI74        WHENEQ    '04'
396100000000     C* ma l'utente non è abilitato
396200000000    >C     ANNALL        IFEQ      *OFF
396300000000     C* esco da pgm avvertendolo
396400170804    >C                   MOVE      '2'           ERRI74
396500000000     C                   MOVE      *ON           $FINE
396600000000     C                   EXSR      ENDPGM
396700000000     C                   ENDIF
396800000000     C*
396900000000     C* se richiamato in visualizzazione
397000170804    >C     OPZI74        WHENEQ    '05'
397100000000     C* ma l'utente non è abilitato
397200000000    >C     VISALL        IFEQ      *OFF
397300000000     C* esco da pgm avvertendolo
397400170804    >C                   MOVE      '2'           ERRI74
397500000000     C                   MOVE      *ON           $FINE
397600000000     C                   EXSR      ENDPGM
397700000000     C                   ENDIF
397800000000     C*
397900000000     C                   ENDSL
398000000000     C*
398100000000     C                   ENDSR
398200000000     C/EJECT
398300000000     C************************************************************
398400000000     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
398500000000     C************************************************************
398600000000     C     DEFOPZ        BEGSR
398700000000     C*
398800000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1
398900000000     C* con S o N a seconda della utilizzabilità o meno
399000000000     C* del tasto funzione correlato
399100000000     C*
399200000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
399300000000     C* di compilazione
399400000000     C                   RESET                   $FC1
399500000000     C                   RESET                   $FR1
399600000000     C*
399700000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
399800000000     C* invalido alcune opzioni
399900000000     C                   SELECT
400000000000     C* Se pgm richiamato in interrogazione disattivo :
400100170804    >C     OPZI74        WHENEQ    '05'
400200000000     C* F2=Decodifica
400300000000     C                   Z-ADD     2             X
400400000000     C                   MOVE      '0'           $FC1(X)
400500000000     C* F4=Ricerca
400600000000     C                   Z-ADD     4             X
400700000000     C                   MOVE      '0'           $FC1(X)
400800000000     C* F5=Ripristino
400900000000     C                   Z-ADD     5             X
401000000000     C                   MOVE      '0'           $FC1(X)
401100000000     C* F6=Conferma
401200000000     C                   Z-ADD     06            X
401300000000     C                   MOVE      '0'           $FC1(X)
401400000000     C* Se pgm richiamato in annullamento disattivo :
401500170804    >C     OPZI74        WHENEQ    '04'
401600000000     C* F2=Decodifica
401700000000     C                   Z-ADD     2             X
401800000000     C                   MOVE      '0'           $FC1(X)
401900000000     C* F4=Ricerca
402000000000     C                   Z-ADD     4             X
402100000000     C                   MOVE      '0'           $FC1(X)
402200000000     C* F5=Ripristino
402300000000     C                   Z-ADD     5             X
402400000000     C                   MOVE      '0'           $FC1(X)
402500000000     C*
402600000000     C                   OTHER
402700000000     C*
402800000000     C                   ENDSL
402900000000     C*
403000000000     C                   ENDSR
403100000000     C/EJECT
403200000000     C************************************************************
403300000000     C* REPERIMENTO CAMPI DI DEFAULT
403400000000     C************************************************************
403500000000     C     REPDEF        BEGSR
403600000000     C*
403700000000     C                   Eval      $FlgDef     = *On
403800000000     C*
403900000000     C                   ENDSR
404000130918      /EJECT
404100130918     C************************************************************
404200170804     C* Rerimento parametri di setup per spesometro trimestrale
404300130918     C************************************************************
404400170804     C     RepSDGSPETRI  BEGSR
404500120215     C*
404600170804     C                   Clear                   SDGPA12DS
404700120215     C*
404800120215     C                   CALLB     'XPAR'
404900120215     C                   Parm                    XScSoc
405000170805     C                   Parm      'SDSPETRI'    XPaPar            8
405100120215     C                   Parm                    XPaOut         2000
405200120215     C                   Parm      *OFF          XPaErr            1
405300120215     C                   Parm      ' '           XPaRic            1
405400120215     C*
405500120215     C                   if        XPaErr      = *On
405600120215     C                   EXSR      ENDPGM
405700120215     C                   else
405800170804     C                   MoveL     XPaOut        SDGPA12DS
405900120215     C                   endif
406000120215     C*
406100120215     C                   EndSr
406200130918     C/EJECT
406300000000     C************************************************************
406400000000     C* DEFINIZIONE KLIST
406500000000     C************************************************************
406600000000     C     DefKlist      BEGSR
406700000000     C*
406800000000     C* klist
406900000000     C*
407000170803     C*
407100170803     C* ANCAP01L
407200170803     C     K02CAP01      klist
407300170803     C                   kfld                    CAPStato
407400170803     C                   kfld                    CAPCAP
407500170803     C*
407600170804     C     K03CAP01      klist
407700170804     C                   kfld                    CAPStato
407800170804     C                   kfld                    CAPCAP
407900170804     C                   kfld                    CapLocalit
408000170804     C*
408100170808     C* SDGCST02L
408200170808     C     K07CST02      KLIST
408300170804     C                   KFLD                    CSTSocieta
408400170804     C                   KFLD                    CSTTpCom
408500170804     C                   KFLD                    CSTTpRec
408600170804     C                   KFLD                    CSTAnno
408700170804     C                   KFLD                    CSTTrim
408800170804     C                   KFLD                    CSTMese
408900170804     C                   KFLD                    CSTCliFor
409000170803     C*
409100000000     C                   ENDSR
409200000000     C/EJECT
409300000000     C************************************************************
409400000000     C* RIEMPIMENTO TASTI FUNZIONE
409500000000     C************************************************************
409600000000     C     RIEKEY        BEGSR
409700000000     C*
409800000000     C* Input :
409900000000     C* - $FX = schiera con tutti i testi disponibili
410000000000     C* - $FC = schiere con i flag di validità associati ai testi
410100000000     C* - $FR = schiere con i flag di REVERSE IMAGE
410200000000     C* - $ULK = ultimo testo caricato
410300000000     C* - $L01 = Lunghezza prima riga
410400000000     C* - $L02 = Lunghezza seconda riga
410500000000     C* Output :
410600000000     C* - $ULK
410700000000     C* - $KEY1= 1° riga di tasti funzione
410800000000     C* - $KEY2= 2° riga di tasti funzione
410900000000     C*
411000000000     C                   CALLB     'XKEYMSG'
411100000000     C* input
411200000000     C                   PARM                    $FX
411300000000     C                   PARM                    $FC
411400000000     C                   PARM                    $FR
411500000000     C                   PARM                    $ULK              3 0
411600000000     C* output
411700000000     C                   PARM                    $KEY1            79
411800000000     C                   PARM                    $KEY2            79
411900000000     C* input
412000000000     C                   PARM                    $L01              3 0
412100000000     C                   PARM                    $L02              3 0
412200000000     C*
412300000000     C                   PARM                    $ALLFUNCT       480
412400000000     C*
412500000000     C                   ENDSR
412600000000** $OZ / $ID
41270000000001SDGMSG    *LIBL     SDC0002
41280000000002PROMSG    *LIBL     COS0002
41290000000003PROMSG    *LIBL     COS0003
41300000000004PROMSG    *LIBL     COS0004
41310000000005PROMSG    *LIBL     COS0005
413200000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
413300000000PROMSG    *LIBL     KEY0001  01
413400000000PROMSG    *LIBL     KEY0002  02
413500000000PROMSG    *LIBL     KEY0003  03
413600000000PROMSG    *LIBL     KEY0004  04
413700000000PROMSG    *LIBL     KEY0005  05
413800000000PROMSG    *LIBL     KEY0006  06
413900000000PROMSG    *LIBL     KEY0007  07
414000000000PROMSG    *LIBL     KEY0008  08
414100000000PROMSG    *LIBL     KEY0009  09
414200000000PROMSG    *LIBL     KEY0010  10
414300000000PROMSG    *LIBL     KEY0011  11
414400000000PROMSG    *LIBL     KEY0012  12
414500000000PROMSG    *LIBL     KEY0013  13
414600000000PROMSG    *LIBL     KEY0014  14
414700000000PROMSG    *LIBL     KEY0015  15
414800000000PROMSG    *LIBL     KEY0016  16
414900000000PROMSG    *LIBL     KEY0017  17
415000000000PROMSG    *LIBL     KEY0018  18
415100000000PROMSG    *LIBL     KEY0019  19
415200000000PROMSG    *LIBL     KEY0020  20
415300000000PROMSG    *LIBL     KEY0021  21
415400000000PROMSG    *LIBL     KEY0022  22
415500000000PROMSG    *LIBL     KEY0023  23
415600000000PROMSG    *LIBL     KEY0024  24
415700000000**  ABILITAZIONE D1 : $FC1 / $FR1
4158000000000N  01
4159000000000N  02
4160000000001N  03
4161000000001N  04
4162000000001N  05
4163000000001N  06
4164000000000N  07
4165000000001N  08
4166000000000N  09
4167000000000N  10
4168000000000N  11
4169000000001N  12
4170000000000N  13
4171000000000N  14
4172000000000N  15
4173000000000N  16
4174000000000N  17
4175000000000N  18
4176000000000N  19
4177000000000N  20
4178000000000N  21
4179000000000N  22
4180000000000N  23   Impostare a '1' se si utilizzano opzioni
4181000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
