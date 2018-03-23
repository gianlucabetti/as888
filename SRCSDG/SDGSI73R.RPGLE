000100170727     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND SDG007)
000200170705     H*PARMS CVTOPT(*NONE) ACTGRP(SOGG)
000300170705      *****************************************************************
000400170705     H DECEDIT('0,') DATEDIT(*DMY.)
000500170705      *****************************************************************
000600170705      *
000700170705      *                Spesometro trimestrale XML
000800170705      *                    Dati di testata
000900170705      *           -------------------------------------
001000170705      *
001100170705R258  * Questo pgm è stato creato con il task R258 Comunicaz. Spesometro Trimestrale
001200170705      *
001300170705      *****************************************************************
001400170705      *------------
001500170720     F* V/Logica per dati anagrafici
001600170808    >FSDGCST02L IF   E           K DISK
001700170808    >F                                     RENAME(SDGCST000:SDGCST002)
001800170705     FSDGSI73D  CF   E             WORKSTN
001900170705     F                                     RENAME(SI73D1:FMTD1)
002000170705     F                                     RENAME(SI73D2:FMTD2)
002100170705     F                                     RENAME(SI73D3:FMTD3)
002200170705     F                                     RENAME(SI73S1:FMTS1)
002300170705     F                                     RENAME(SI73C1:FMTC1)
002400170705     F                                     RENAME(SI73T1:FMTT1)
002500170705     F                                     RENAME(SI73Z1:FMTZ1)
002600170705     F                                     RENAME(SI73Z2:FMTZ2)
002700170705     F                                     RENAME(SI73Z3:FMTZ3)
002800000000     F                                     SFILE(FMTS1:S1NRR)
002900000000     F                                     INFDS(DSFMT)
003000000000     D*----------------------------------------------------*
003100000000     D* Prototipazione driver contatti
003200000000     D/COPY *LIBL/SRCCPY,PJDR12PR
003300000000     D*----------------------------------------------------*
003400000000     D* Schiere per la gestione delle opzioni
003500000000     D*
003600000000     D* dimensione della schiera $MS1, da non cambiare
003700000000     D $DIMOP          C                   CONST(30)
003800000000     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
003900000000     D $TX1            S             50    DIM($dimop) ALT($MS1)
004000000000     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004100000000     D $FL1            S              1    DIM($dimop) ALT($OP1)
004200000000     D* schiera contenente i nomi dei programma richiamati attraverso
004300000000     D* le opzioni personalizzate
004400000000     D $PGO1           S             10    DIM($dimop)
004500000000     D $TX             S             50    DIM(50)
004600000000     D $FL             S              1    DIM(50)
004700000000     D* Schiere generali per la gestione dei tasti di funzione
004800000000     D $FM             S             27    DIM(24)
004900000000     D $FX             S             50    DIM(24)
005000000000     D $FC             S              1    DIM(24)
005100000000     D $FR             S              1    DIM(24)
005200000000     D* Schiere per la gestione dei tasti di funzione
005300000000     D* Video D1
005400000000     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005500000000     D $FX1            S             50    DIM(24) ALT($FM1)
005600000000     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005700000000     D $FR1            S              1    DIM(24) ALT($FC1)
005800000000     D* Schiera contente i nomi dei programmi da richiamare con i tasti
005900000000     D* funzione personalizzata S1
006000000000     D $PGF2           S             10    DIM(25)
006100000000     D* Schiere per la gestione dei tasti di funzione
006200000000     D* Video S1
006300000000     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
006400000000     D $FX2            S             50    DIM(24) ALT($FM2)
006500000000     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
006600000000     D $FR2            S              1    DIM(24) ALT($FC2)
006700000000     D* Schiere per la gestione dei tasti di funzione
006800000000     D* Video D2
006900000000     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
007000000000     D $FX3            S             50    DIM(24) ALT($FM3)
007100000000     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
007200000000     D $FR3            S              1    DIM(24) ALT($FC3)
007300000000     D*
007400000000     D $D1L01          S              3  0
007500000000     D $D1L02          S              3  0
007600000000     D $S1L01          S              3  0
007700000000     D $S1L02          S              3  0
007800000000     D $C1Op1          S              3  0
007900000000     D $C1Op2          S              3  0
008000000000     D $D2L01          S              3  0
008100000000     D $D2L02          S              3  0
008200000000     D*
008300000000     D*----------------------------------------------------*
008400000000     D* Valori assunti
008500000000     D XMPEDS        E DS                  INZ
008600000000     D XMPEDS1       E DS                  INZ
008700000000     D DSMPE           DS                  INZ
008800000000     D  $LOP                   1      1  0
008900000000     D  $ORD                   2      3  0
009000000000     D*-------------
009100000000     D* Passaggio Parametri
009200000000     D KPJBA         E DS
009300110823     D*-------------
009400110823     D* Salvataggio Kpjbu per richiamo altri pgm
009500110823     D SavKPJBU        S            256
009600170705     D*-------------
009700170705     D* Parametri modulo SDSPETRI
009800170705     D SDGPA12DS     E DS
009900000000     D*-------------
010000000000     D* Reperimento Autorizzazioni
010100000000     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
010200000000     D*-------------
010300000000     D* Dati per pgm XNOAUT
010400000000     D XNOADS        E DS                  EXTNAME(XNOAUTDS)
010500000000     D*-------------
010600000000     D* Passaggio Autorizzazioni
010700000000     D DSAUT           DS           512
010800000000     D*-------------
010900000000     D* Maschera dati autorizzazioni
011000000000     D CONCIVDS      E DS
011100000000     D*-------------
011200000000     D* Dati di ambiente ottenuti da SDGSOCDR
011300000000     D SOC001        E DS                  EXTNAME(SDGSOCDS )
011400000000     D    SIFSogg                     8    overlay(XScFGO:31)
011500000000     D*-------------
011600000000     D* DS Interna per dati di output di SDGSOCDR
011700000000     D XSOCDS          DS          1000
011800000000     D*-------------
011900000000     D* DS Esterna gestione messaggi
012000000000     D SDGMSGDS      E DS                  INZ
012100000000     D  STS          E                     EXTFLD(MSGSTS)
012200000000     D                                     DIM(333)
012300000000     D  JBA          E                     EXTFLD(MSGJBA)
012400000000     D                                     DIM(502)
012500000000     D*-------------
012600000000     D* campi di lavoro per emissione window di avvertimento
012700000000     D WXMSGInz        S              1A   INZ('1')
012800000000     D*-------------
012900000000     D* Richiamo a SDGTABDR
013000000000     D SDGTABDS      E DS                  inz
013100000000     D*-------------
013200000000     D* Parametri per ricerca codice ANATB00F
013300000000     D XTABDS        E DS                  INZ
013400000000     D*-------------
013500000000     D* Ricerca codici Stati
013600000000     D ANGG12DS      E DS
013700170705     D* Ricerca codici ISO Stati
013800170727     D SDGHD4DS      E DS
013900170705     D*-------------
014000170705     D* Definizioni controllo causale
014100170705     D Anope00f      e ds                  inz
014200170705     D Refresh         S              1
014300170705     D Uso             S              1
014400170705     D Causale         S              4
014500170705     D*-------------
014600170705     D* Ricerca causale
014700170705     D ANA070DS      E DS                  INZ
014800170705     D  RET070       E                     INZ('0')
014900170705     D  OPR070       E                     INZ('0')
015000170705     D  CMT070       E                     INZ('0')
015100170705     D  ERR070       E                     INZ('0')
015200170705     D  FSC070       E                     INZ('S')
015300170705     D*----------
015400170705     D WData           S               D
015500170705     D WAnno           S              4  0
015600170705     D WDtAlfa         S             10A
015700170705     D DataDs          DS
015800170705     D  AnnoDs                        4A
015900170705     D   Tratt1                       1A   Inz('-')
016000170705     D  MeseDs                        2A
016100170705     D   Tratt2                       1A   Inz('-')
016200170705     D  GiornoDs                      2A
016300000000     D*-------------
016400000000     D* Ricerca legende
016500000000     D ANA964DS      E DS                  INZ
016600000000     D*-------------
016700000000     D*Controlli validità per valore di una legenda
016800000000     D X07VALDS      E DS                  Inz
016900000000     D*-------------
017000000000     D* DS per controllo conto
017100000000     D ND002DS       E DS                  INZ
017200000000     D*-------------
017300000000     D* Reperimento parametri
017400000000     D XPaOut          DS          2000
017500000000     D*-------------
017600000000     D* Parametri in input
017700170705    >D SDGSI73DS     E DS                  inz
017800170726     D*-------------
017900170726     D* Aggiornamento da testata
018000170726    >D SDG00702ds    E DS                  inz
018100170727     D CMT0702       E                     INZ('0')
018200000000     D*-------------
018300130912     D* manutenzione  dati di testata
018400170705     D SDGSI74DS     E DS                  inz
018500170705     D  RETI74       E                     INZ('0')
018600170705     D  OPRI74       E                     INZ('0')
018700170705     D  ERRI74       E                     INZ('0')
018800170705     D  CMTI74       E                     INZ('0')
018900000000     D*-------------
019000130912     D* lista dettaglio
019100170705     D SDGSI75DS     E DS                  inz
019200170705     D  RETI75       E                     INZ('0')
019300170705     D  OPRI75       E                     INZ('0')
019400170705     D  ERRI75       E                     INZ('0')
019500170705     D  CMTI75       E                     INZ('0')
019600000000     D*-------------
019700000000     D* manutenzione dettaglio
019800170705    >D SDGSI76DS     E DS                  inz
019900170705    >D  RETI76       E                     INZ('0')
020000170705    >D  OPRI76       E                     INZ('0')
020100170705    >D  ERRI76       E                     INZ('0')
020200170705    >D  CMTI76       E                     INZ('0')
020300000000     D*-------------
020400000000     D* campo di salvataggio per la DS d'ingresso durante la chiamata
020500000000     D* alla funzione personalizzata
020600170712    >D $SaveDS         S                   like(SDGSI73DS)
020700000000     D*-------------
020800000000     D DSFMT           DS           512
020900000000     D  $TASTO               369    369
021000000000     D  NRG                  370    370
021100000000     D  NCL                  371    371
021200000000      * riga cursore window
021300000000     D  NRiWin               382    382
021400000000      * colonna cursore window
021500000000     D  NCoWin               383    383
021600000000     D  SFLNRR               378    379B 0
021700000000     D*-------------
021800000000     D* posizione cursore
021900000000     D CURSOR          DS
022000000000     D  RIRI                   1      2B 0 INZ
022100000000     D  R$$                    2      2
022200000000     D  COCO                   3      4B 0 INZ
022300000000     D  C$$                    4      4
022400000000     D*-------------
022500000000     D* Costanti
022600000000     D* nr.di righe del sfl (SFLPAG)
022700170805     D SFLPAG          C                   CONST(6)
022800000000     D*-------------
022900000000     D* Tasti di funzione
023000000000     D F01             C                   CONST(X'31')
023100000000     D F02             C                   CONST(X'32')
023200000000     D F03             C                   CONST(X'33')
023300000000     D F04             C                   CONST(X'34')
023400000000     D F05             C                   CONST(X'35')
023500000000     D F06             C                   CONST(X'36')
023600000000     D F07             C                   CONST(X'37')
023700000000     D F08             C                   CONST(X'38')
023800000000     D F09             C                   CONST(X'39')
023900000000     D F10             C                   CONST(X'3A')
024000000000     D F11             C                   CONST(X'3B')
024100000000     D F12             C                   CONST(X'3C')
024200000000     D F13             C                   CONST(X'B1')
024300000000     D F14             C                   CONST(X'B2')
024400000000     D F15             C                   CONST(X'B3')
024500000000     D F16             C                   CONST(X'B4')
024600000000     D F17             C                   CONST(X'B5')
024700000000     D F18             C                   CONST(X'B6')
024800000000     D F19             C                   CONST(X'B7')
024900000000     D F20             C                   CONST(X'B8')
025000000000     D F21             C                   CONST(X'B9')
025100000000     D F22             C                   CONST(X'BA')
025200000000     D F23             C                   CONST(X'BB')
025300000000     D F24             C                   CONST(X'BC')
025400000000     D ENTER           C                   CONST(X'F1')
025500000000     D ROLDWN          C                   CONST(X'F4')
025600000000     D ROLLUP          C                   CONST(X'F5')
025700000000     D*-------------
025800000000     D* Attributi video
025900000000     D Normale         C                   Const(X'32')
026000000000     D Protetto        C                   Const(X'A0')
026100170725     D RI              C                   CONST(X'A1')
026200000000     D*-------------
026300000000     D* Sottotitolo di testata
026400000000     D SOTTIT          C                   CONST('PROMSG    *LIBL-
026500000000     D                                          COS2504')
026600000000     D SOTTITD1        C                   CONST('SDGMSG    *LIBL-
026700170705     D                                          SDC0001')
026800000000     D*-------------
026900000000     D* Variabili per gestione videate
027000000000     D $Gest           S              2A
027100000000     D $Fine           S              1A
027200000000     D $InzD1          S              1A
027300000000     D $InzS1          S              1A
027400000000     D $InzD2          S              1A
027500000000     D $LastG          S              2A
027600000000     D $LastV          S              2A
027700000000     D $EndFile        S              1A
027800000000     D $Esci           S              1A
027900000000     D $RcdOK          S              1A
028000170727     D $SavRcdOk       S              1A
028100000000     D $Unique         S              1A
028200170727     D $Rott01         S              1A
028300000000     D*-------------
028400000000     D* Variabili appoggio sempre presenti in tutte le anagrafiche
028500000000     D S1Nrr           S                   like(C1Rcd)
028600170807     D WNRR            S                   like(S1NRR)
028700000000     D WSfl            S                   like(C1Nrr)
028800000000     D WMax            S                   like(C1Rcd)
028900000000     D WPag            S                   like(C1Rcd)
029000000000     D WInzS1          S                   like($InzS1)
029100170712     D WOpr            S                   like(OprI73)
029200000000     D CurR            S              3S 0
029300000000     D CurC            S              3S 0
029400000000     D Win             S             99A
029500000000     D W1Pos           S                   like(C1Pos)
029600000000     D WINKR           S              1A
029700000000     D WExf            S              1A
029800000000     D WSogg           S                   Like(DVSSogg)
029900130828     D WFineSCF        S              1A
030000170731     D WStatoHd4       S                   Like(CSTStato1)
030100000000     D*-------------
030200000000     D* Indice della schiera tasti funzione
030300000000     D PosFun          S              2  0
030400170808     D*-------------
030500170803     D* Ds campi di salvataggio per caricare a rottira di dati anagrafici
030600170803     D SAVCLIFOR       DS
030700170803     D  SAVCOGRAG1                         Like(CSTCOGRAG1)
030800170803     D  SAVCF                              Like(CSTCF     )
030900170803     D  SAVPARTIVA                         Like(CSTPARTIVA)
031000170803     D  SAVCDFISC                          Like(CSTCDFISC )
031100170803     D  SAVSTATO1                          Like(CSTSTATO1 )
031200170803     D  SAVIDPAES1                         Like(CSTIDPAES1)
031300170803     D  SAVPIVA1                           Like(CSTPIVA1  )
031400170803     D  SAVCDFISC1                         Like(CSTCDFISC1)
031500170803     D  SAVTPN1                            Like(CSTTPN1   )
031600170803     D  SAVDENOM1                          Like(CSTDENOM1 )
031700170803     D  SAVNOME1                           Like(CSTNOME1  )
031800170803     D  SAVCOGNOM1                         Like(CSTCOGNOM1)
031900170803     D  SAVINDIR2                          Like(CSTINDIR2 )
032000170803     D  SAVNRCIV2                          Like(CSTNRCIV2 )
032100170803     D  SAVCAP2                            Like(CSTCAP2   )
032200170803     D  SAVLOCALI2                         Like(CSTLOCALI2)
032300170803     D  SAVPROV2                           Like(CSTPROV2  )
032400170803     D  SAVSTATO2                          Like(CSTSTATO2 )
032500170803     D  SAVIDPAES2                         Like(CSTIDPAES2)
032600170803     D  SAVINDIR3                          Like(CSTINDIR3 )
032700170803     D  SAVNRCIV3                          Like(CSTNRCIV3 )
032800170803     D  SAVCAP3                            Like(CSTCAP3   )
032900170803     D  SAVLOCALI3                         Like(CSTLOCALI3)
033000170803     D  SAVPROV3                           Like(CSTPROV3  )
033100170803     D  SAVSTATO3                          Like(CSTSTATO3 )
033200170803     D  SAVIDPAES3                         Like(CSTIDPAES3)
033300170803     D  SAVSTATO4                          Like(CSTSTATO4 )
033400170803     D  SAVIDPAES4                         Like(CSTIDPAES4)
033500170803     D  SAVPIVA4                           Like(CSTPIVA4  )
033600170803     D  SAVTPN4                            Like(CSTTPN4   )
033700170803     D  SAVCOGRAG4                         Like(CSTCOGRAG4)
033800170803     D  SAVDENOM4                          Like(CSTDENOM4 )
033900170803     D  SAVNOME4                           Like(CSTNOME4  )
034000170803     D  SAVCOGNOM4                         Like(CSTCOGNOM4)
034100170803     D*
034200170727     D SAVANNA         S                   like(CSTANN)
034300170727     D SAVNOANN        S                   like(CSTANN)
034400170727     D SAVCLIA         S                   like(CSTCF)
034500170727     D SAVFORA         S                   like(CSTCF)
034600170727     D $Root01         S              1A
034700170727     D $Primavolta     S              1A
034800170810     D SAVOpzI73       S                   like(OPZI73)
034900170728     D*
035000170728     D* Ds per la chiave di lettura
035100170728     D DsCliFor        DS
035200170728     D  CSTCOGRAG1
035300170803     D  CSTCF
035400170728     D  CSTPARTIVA
035500170728     D  CSTCDFISC
035600170731     D  CSTSTATO1
035700170728     D  CSTIDPAES1
035800170728     D  CSTPIVA1
035900170728     D  CSTCDFISC1
036000170728     D  CSTTPN1
036100170803     D  CSTDENOM1
036200170728     D  CSTNOME1
036300170803     D  CSTCOGNOM1
036400170728     D  CSTINDIR2
036500170728     D  CSTNRCIV2
036600170728     D  CSTCAP2
036700170728     D  CSTLOCALI2
036800170728     D  CSTPROV2
036900170728     D  CSTSTATO2
037000170728     D  CSTIDPAES2
037100170728     D  CSTINDIR3
037200170728     D  CSTNRCIV3
037300170728     D  CSTCAP3
037400170728     D  CSTLOCALI3
037500170728     D  CSTPROV3
037600170728     D  CSTSTATO3
037700170728     D  CSTIDPAES3
037800170731     D  CSTSTATO4
037900170728     D  CSTIDPAES4
038000170728     D  CSTPIVA4
038100170728     D  CSTTPN4
038200170728     D  CSTCOGRAG4
038300170803     D  CSTDENOM4
038400170728     D  CSTNOME4
038500170803     D  CSTCOGNOM4
038600170728     D* Ds per aggiornare
038700170728     D DsCliForA       DS
038800170803     D  H1COGRAG1
038900170803     D  H1CF
039000170728     D  S1PARTIVA
039100170728     D  S1CDFISC
039200170731     D  H1STATO1
039300170728     D  H1IDPAES1
039400170728     D  H1PIVA1
039500170728     D  H1CDFISC1
039600170728     D  H1TPN1
039700170803     D  H1DENOM1
039800170728     D  H1NOME1
039900170803     D  H1COGNOM1
040000170728     D  H1INDIR2
040100170728     D  H1NRCIV2
040200170728     D  H1CAP2
040300170728     D  H1LOCALI2
040400170728     D  H1PROV2
040500170728     D  H1STATO2
040600170728     D  H1IDPAES2
040700170728     D  H1INDIR3
040800170728     D  H1NRCIV3
040900170728     D  H1CAP3
041000170728     D  H1LOCALI3
041100170728     D  H1PROV3
041200170728     D  H1STATO3
041300170728     D  H1IDPAES3
041400170731     D  H1STATO4
041500170728     D  H1IDPAES4
041600170728     D  H1PIVA4
041700170728     D  H1TPN4
041800170728     D  H1COGRAG4
041900170803     D  H1DENOM4
042000170728     D  H1NOME4
042100170803     D  H1COGNOM4
042200000000     D*-------------
042300000000     D*  Indici
042400000000     D X               S              5S 0
042500000000     D Y               S              5S 0
042600000000     D* Indice della schiera per le opzioni personalizzate
042700000000     D PosOpz          S              2  0
042800000000     D* numero max opzioni
042900000000     D NumOpz          S              2  0 Inz($DIMOP)
043000000000     D* numero effettive opzioni
043100000000     D OpzLun          S              2  0
043200000000     D*-------------
043300000000     D* ricerca soggetti
043400130829     D ANA040DS      E DS                  INZ
043500000000     D*-------------
043600000000     D* Campi di lavoro
043700000000     D $P1PRO2184      S             10A
043800000000     D*-------------
043900000000     D* Reperimento nome PGM
044000000000     D STATUS         SDS           333
044100000000     D  DSPGM            *PROC
044200000000     D  PARMS            *PARMS
044300000000     D*-------------
044400000000     D* DS esterna per dati soggetti
044500000000     D DVASOG        E DS                  INZ
044600000000     D*-------------
044700000000     D* Definizioni parametri driver anagrafici
044800000000     D Dvaric          S              1
044900000000     D DvaSocieta      S              3
045000000000     D DvaUnita        S              8
045100000000     D DvaStrutt       S             10
045200000000     D DvaDtRif        S               D
045300000000     D DvaLenOut       S              9B 0
045400000000     D DvaSnatura      S              1
045500000000     D DvaCodice       S              8
045600000000     D DvaLineav       S              3
045700000000     D DvaFiliale      S              3  0
045800000000     D DvaSogg         S              8
045900000000     D DvaTpInd        S              2
046000000000     D DvaCdInd        S              3
046100000000     D DvaErrore       S              1
046200000000     D DvaOutput       S           4000
046300000000     D*-------------
046400000000     D* parametri per chiamata al driver dei contatti
046500000000     D $Errore         S              1
046600000000     D $Opz            S              1
046700000000     D $Len            S              9B 0
046800000000     D $Refresh        S              1
046900000000     D Esito           S              1
047000000000     D GesMsg          S              1
047100000000     D Output          S           4000
047200000000     D LenOut          S              9B 0
047300000000     D Struttura       S             10
047400130930     D*-------------
047500130930     D* Campi di lavoro
047600130930     D WIn17           S              1
047700170718     D $Continua       S              1A
047800170718     D $MsgCd1         S                    Like(XMSGCd1)
047900000000     C*----------------------------------------------------*
048000000000     C*                MAIN LINE PROGRAM
048100000000     C*----------------------------------------------------*
048200000000     C*
048300000000     C* inizializzazione variabili
048400000000     C                   EXSR      INZVAR
048500000000     C*
048600000000     C     $FINE         DOWEQ     *OFF
048700000000     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
048800000000     C     $GEST         CASEQ     'S1'          GESS1                          Lista
048900000000     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
049000000000     C                   ENDCS
049100000000     C                   ENDDO
049200000000     C* fine programma
049300000000     C                   EXSR      ENDPGM
049400000000     C*
049500000000     C/EJECT
049600000000     C************************************************************
049700000000     C* FINE PROGRAMMA
049800000000     C************************************************************
049900000000     C     ENDPGM        BEGSR
050000000000     C*
050100000000     C* passaggio dati al pgm chiamante
050200170712    >C                   MOVEL     WOPR          OPRI73                                     ne S1
050300170712    >C                   MOVEL     SDGSI73DS     KPJBU
050400000000     C*
050500000000     C* Se pgm Guida non è stato richiamato posso chiudere in LR
050600170712    >C     OPZI73        IFEQ      *BLANK
050700000000     C                   SETON                                            LR
050800000000     C* altrimenti chiudo in RT
050900000000     C                   ELSE
051000000000     C                   SETON                                            RT
051100000000     C                   ENDIF
051200000000     C                   RETURN
051300000000     C*
051400000000     C                   ENDSR
051500000000     C/EJECT
051600000000     C************************************************************
051700000000     C* RICERCHE
051800000000     C************************************************************
051900000000     C     SEARCH        BEGSR
052000000000     C*
052100000000     C* determino Riga/Colonna del cursore
052200000000     C                   MOVE      NRG           R$$
052300000000     C                   MOVE      NCL           C$$
052400000000     C                   Z-ADD     RIRI          CURR
052500000000     C                   Z-ADD     COCO          CURC
052600000000     C*
052700000000     C                   EXSR      CallAllinea
052800000000     C*
052900000000   B1C                   SELECT
053000000000     C*
053100000000     C*  Stato
053200170731     C     H1NMFL        WHENEQ    'D1STATO1'
053300000000     C                   EXSR      F4_Stato
053400170718     C*
053500170718     C*  Trimestre
053600170718     C     H1NMFL        WHENEQ    'D1TRIM'
053700170718     C                   EXSR      F4_Trim
053800170711     C*
053900130911     C*  Natura soggetto
054000130911     C     H1NMFL        WHENEQ    'D1CF'
054100130911     C                   EXSR      F4_CF
054200000000     C*
054300000000     C*  Codice IVA
054400170718     C     H1NMFL        WHENEQ    'D1PARTIVA'
054500170718     C                   EXSR      F4_Partiva
054600000000     C*
054700000000     C*  Codice fiscale
054800170718     C     H1NMFL        WHENEQ    'D1CDFISC'
054900170718     C                   EXSR      F4_CdFisc
055000000000     C*
055100000000     C*  Soggetto
055200000000     C     H1NMFL        WHENEQ    'D1SOGG'
055300000000     C                   EXSR      F4_Sogg
055400170718     C*
055500170718     C*  Causale
055600170718     C     H1NMFL        WHENEQ    'D1CAUSALE'
055700170718     C                   EXSR      F4_Causale
055800170718     C*
055900170718     C*  Tipo record
056000170718     C     H1NMFL        WHENEQ    'D1TPREC  '
056100170718     C                   EXSR      F4_TpRec
056200000000     C*
056300000000     C* FMTD1 - Dati attivi/non attivi
056400000000     C     H1NMFL        WHENEQ    'D1ANN  '
056500000000     C                   EXSR      F4_Ann
056600000000     C*
056700000000     C* FMTD1 - Tipo annullamento
056800000000     C     H1NMFL        WHENEQ    'D1TPANN'
056900000000     C                   EXSR      F4_TpAnn
057000000000     C*
057100000000     C* ricerca non abilitata in questa posizione
057200000000   X1C                   OTHER
057300000000     C                   SETON                                        9899
057400000000     C*
057500000000   E1C                   ENDSL
057600000000     C*
057700000000     C* imposto pos. del cursore
057800000000     C                   Z-ADD     CURR          H1RIGA
057900000000     C                   Z-ADD     CURC          H1COLO
058000000000     C*
058100000000     C                   MOVE      *HIVAL        $LASTV
058200000000     C*
058300000000     C                   ENDSR
058400000000     C/EJECT
058500170712     C************************************************************
058600000000     C* Richiamo programma per allineare i codici
058700000000     C************************************************************
058800000000     C     CallAllinea   BEGSR
058900000000     C*
059000000000     C     D1SOGG        IFNE      *BLANK
059100000000     C                   CALLb     'XALLINEA'
059200000000     C                   PARM                    D1SOGG
059300000000     C                   PARM      8             LEN               5 0
059400000000     C                   PARM                    XSCINP
059500000000     C                   ENDIF
059600000000     C*
059700000000     C                   ENDSR
059800170712      /EJECT
059900000000     C************************************************************
060000000000     C* Ricerca codice Stato del conto
060100000000     C************************************************************
060200000000     C     F4_Stato      BEGSR
060300000000     C*
060400000000     C                   Reset                   XTabDS
060500000000     C*
060600000000     C                   Move      '01'          XTaOpz
060700000000     C                   Move      XScSoc        XTaAzi
060800000000     C                   Move      XScLin        XTaLin
060900000000     C                   Z-add     Curr          XTaRig
061000000000     C                   Z-add     Curc          XTaCol
061100000000     C                   Move      *Blank        XTaKey
061200000000     C*
061300000000     C                   Clear                   KPJBU
061400000000     C                   MoveL     XTabDS        KPJBU
061500000000     C*
061600000000     C                   CALL      'ANGG12R'
061700000000     C                   PARM                    KPJBA
061800000000     C                   MoveL     KPJBU         XTabDS
061900000000     C*
062000000000     C                   if        XTaErr     <> '1'  and
062100000000     C                             XTaKey     <> *Blank
062200170731     C                   Move      XTaKey        D1Stato1
062300000000     C                   endif
062400000000     C*
062500000000     C                   ENDSR
062600000000      /EJECT
062700170718     C************************************************************
062800170711     C* Ricerca Trimestre di competenza
062900170718     C************************************************************
063000170711     C     F4_Trim       BEGSR
063100140327     C*
063200140327     C                   Reset                   ANA964DS
063300140327     C*
063400140327     C                   MoveL     '01'          Opz964
063500170727     C                   MoveL     'SDGCST000'   Trc964
063600170711     C                   MoveL     'CSTTRIM  '   Cam964
063700140327     C                   MoveL     *On           All964
063800140327     C*
063900140327     C                   MoveL     ANA964DS      KPJBU
064000140327     C                   Call      'ANA964R'
064100140327     C                   Parm                    KPJBA
064200140327     C                   MoveL     KPJBU         ANA964DS
064300140327     C*
064400140327     C                   If        Ret964     <> '2'
064500170711     C                   MoveL     Val964        D1Trim
064600140327     C                   EndIf
064700140327     C*
064800140327     C                   EndSr
064900170718      /EJECT
065000130911     C************************************************************
065100111003     C* Ricerca natura del conto
065200130911     C************************************************************
065300111003     C     F4_CF         BEGSR
065400111003     C*
065500111003     C                   Reset                   ANA964DS
065600111003     C*
065700111003     C                   MoveL     '01'          Opz964
065800170712     C                   MoveL     'SDGCST000'   Trc964
065900170712     C                   MoveL     'CSTCF    '   Cam964
066000111003     C                   MoveL     *On           All964
066100111003     C*
066200111003     C                   MoveL     ANA964DS      KPJBU
066300111003     C                   CALL      'ANA964R'
066400111003     C                   PARM                    KPJBA
066500111003     C                   MoveL     KPJBU         ANA964DS
066600111003     C*
066700111003     C                   if        Ret964     <> '2'
066800111003     C                   MoveL     Val964        D1CF
066900111003     C                   endif
067000111003     C*
067100111003     C                   ENDSR
067200130911      /EJECT
067300000000     C************************************************************
067400000000     C* Ricerca partita IVA
067500000000     C************************************************************
067600170718     C     F4_Partiva    BEGSR
067700000000     C*
067800130829     C                   RESET                   ANA040DS
067900000000     C*
068000130829     C                   MOVEL     '01'          OPZ040
068100170718     C                   MoveL     D1PartIVA     IVA040
068200170811     C                   MoveL     D1Sogg        CoD040
068300130829     C                   MOVEL     ANA040DS      KPJBU
068400130829     C                   CALL      'ANA040R'
068500000000     C                   PARM                    KPJBA
068600000000     C*
068700130829     C                   MOVEL     KPJBU         ANA040DS
068800000000     C*
068900130829     C                   if        Ret040     <> '2'
069000000000     C*
069100130829     C                   MoveL     CoD040        WSogg
069200000000     C                   EXSR      CallDvaSog
069300000000     C*
069400000000     C                   if        DVAErrore  <> '1'
069500000000     C*
069600000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
069700000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
069800000000     C*
069900170811     C                   MoveL     DVSPartIVA    D1Partiva
070000000000     C*
070100000000     C                   endif
070200000000     C*
070300000000     C                   endif
070400000000     C*
070500000000     C                   ENDSR
070600000000      /EJECT
070700000000     C************************************************************
070800000000     C* Ricerca codice fiscale
070900000000     C************************************************************
071000170718     C     F4_CdFisc     BEGSR
071100000000     C*
071200130829     C                   RESET                   ANA040DS
071300000000     C*
071400130829     C                   MOVEL     '01'          OPZ040
071500170718     C                   MoveL     D1CdFisc      CdF040
071600130829     C                   MoveL     D1Sogg        CoD040
071700130829     C                   MOVEL     ANA040DS      KPJBU
071800130829     C                   CALL      'ANA040R'
071900000000     C                   PARM                    KPJBA
072000000000     C*
072100130829     C                   MOVEL     KPJBU         ANA040DS
072200000000     C*
072300130829     C                   if        Ret040     <> '2'
072400000000     C*
072500130829     C                   MoveL     CoD040        WSogg
072600000000     C                   EXSR      CallDvaSog
072700000000     C*
072800000000     C                   if        DVAErrore  <> '1'
072900000000     C*
073000000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
073100000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
073200000000     C*
073300170718     C                   MoveL     DVSCdFisc     D1CdFisc                       *
073400000000     C*
073500000000     C                   endif
073600000000     C*
073700000000     C                   endif
073800000000     C*
073900000000     C                   ENDSR
074000000000     C/EJECT
074100000000     C************************************************************
074200000000     C* Ricerca codice soggetto
074300000000     C************************************************************
074400000000     C     F4_Sogg       BEGSR
074500000000     C*
074600130829     C                   RESET                   ANA040DS
074700000000     C*
074800130829     C                   MOVEL     '01'          OPZ040
074900130829     C                   MoveL     D1Sogg        CoD040
075000130829     C                   MOVEL     ANA040DS      KPJBU
075100130829     C                   CALL      'ANA040R'
075200000000     C                   PARM                    KPJBA
075300000000     C*
075400130829     C                   MOVEL     KPJBU         ANA040DS
075500000000     C*
075600130829     C                   if        Ret040     <> '2'
075700130829     C                   MoveL     CoD040        D1Sogg
075800000000     C                   endif
075900000000     C*
076000000000     C                   ENDSR
076100000000     C/EJECT
076200170718     C************************************************************
076300170718     C* Ricerca codice causale
076400170718     C************************************************************
076500170718     C     F4_Causale    BEGSR
076600131206     C*
076700131206     C                   Reset                   ANA070DS
076800131206     C*
076900131206     C                   Move      '01'          Opz070
077000131206     C                   MOVE      '1'           Tpc070
077100131206     C                   CLEAR                   KPJBU
077200131206     C                   MoveL     ANA070DS      KPJBU
077300131206     C*
077400131206     C                   CALL      'ANA070R'
077500131206     C                   PARM                    KPJBA
077600131206     C*
077700131206     C                   MoveL     KPJBU         ANA070DS
077800131206     C*
077900131206     C     Cau070        IFNE      *BLANK
078000131206     C                   MoveL     Cau070        D1Causale
078100131206     C                   ENDIF
078200131206     C*
078300170718     C                   ENDSR
078400170711      /EJECT
078500170718     C************************************************************
078600170718     C* Ricerca Tipo Record
078700170718     C************************************************************
078800170711     C     F4_TpRec      BEGSR
078900170711     C*
079000170711     C                   Reset                   ANA964DS
079100170711     C*
079200170711     C                   MoveL     '01'          Opz964
079300170712     C                   MoveL     'SDGCST000'   Trc964
079400170711     C                   MoveL     'CSTTPREC '   Cam964
079500170711     C                   MoveL     *On           All964
079600170711     C*
079700170711     C                   MoveL     ANA964DS      KPJBU
079800170711     C                   Call      'ANA964R'
079900170711     C                   Parm                    KPJBA
080000170711     C                   MoveL     KPJBU         ANA964DS
080100170711     C*
080200170711     C                   If        Ret964     <> '2'
080300170711     C                   MoveL     Val964        D1TpRec
080400170711     C                   EndIf
080500170711     C*
080600170711     C                   EndSr
080700170718      /EJECT
080800000000     C************************************************************
080900000000     C* Ricerca dati attivi/non attivi
081000000000     C************************************************************
081100000000     C     F4_Ann        BEGSR
081200000000     C*
081300000000     C                   reset                   ana964ds
081400000000     C*
081500000000     C                   movel     '01'          Opz964
081600000000     C                   movel     'ANDIZ000 '   Trc964
081700000000     C                   movel     'DSPATT   '   Cam964
081800000000     C*
081900000000     C                   movel     ana964ds      Kpjbu
082000000000     C                   call      'ANA964R'
082100000000     C                   parm                    Kpjba
082200000000     C                   movel     Kpjbu         Ana964ds
082300000000     C*
082400000000     C                   if        Ret964     <> '2'
082500000000     C                   movel     Val964        D1Ann
082600000000     C                   endif
082700000000     C*
082800000000     C                   ENDSR
082900000000     C/EJECT
083000000000     C************************************************************
083100000000     C* Ricerca tipo annullamento
083200000000     C************************************************************
083300000000     C     F4_TpAnn      BEGSR
083400000000     C*
083500000000     C                   reset                   ana964ds
083600000000     C*
083700000000     C                   movel     '01'          Opz964
083800170712     C                   movel     'SDGCST000'   Trc964
083900170712     C                   movel     'CSTTPANN'    Cam964
084000000000     C*
084100000000     C                   movel     ana964ds      Kpjbu
084200000000     C                   call      'ANA964R'
084300000000     C                   parm                    Kpjba
084400000000     C                   movel     Kpjbu         Ana964ds
084500000000     C*
084600000000     C                   if        Ret964     <> '2'
084700000000     C                   movel     Val964        D1TpAnn
084800000000     C                   endif
084900000000     C*
085000000000     C                   ENDSR
085100170711     C/EJECT
085200000000     C************************************************************
085300000000     C* RIEMPIMENTO OPZIONI
085400000000     C************************************************************
085500000000     C     RIEOPZ        BEGSR
085600000000     C*
085700000000     C                   CALLB     'X04OPMSG'
085800000000     C* input
085900000000     C                   PARM                    $TX
086000000000     C                   PARM                    $FL
086100000000     C                   PARM                    WCOZ              3 0
086200000000     C                   PARM                    WMXO              3 0
086300000000     C* output
086400000000     C                   PARM                    WOPZ1            76
086500000000     C                   PARM                    WOPZ2            76
086600000000     C                   PARM                    FF23              1
086700000000     C* input
086800000000     C                   PARM                    $L01              3 0
086900000000     C                   PARM                    $L02              3 0
087000000000     C*
087100000000     C                   PARM                    $ALLOPT         960
087200000000     C*
087300000000     C                   ENDSR
087400000000     C/EJECT
087500000000     C************************************************************
087600000000     C* RIEMPIMENTO TASTI FUNZIONE
087700000000     C************************************************************
087800000000     C     RIEKEY        BEGSR
087900000000     C*
088000000000     C                   CALLB     'XKEYMSG'
088100000000     C* input
088200000000     C                   PARM                    $FX
088300000000     C                   PARM                    $FC
088400000000     C                   PARM                    $FR
088500000000     C                   PARM                    $ULK              3 0
088600000000     C* output
088700000000     C                   PARM                    $KEY1            79
088800000000     C                   PARM                    $KEY2            79
088900000000     C* input
089000000000     C                   PARM                    $L01              3 0
089100000000     C                   PARM                    $L02              3 0
089200000000     C*
089300000000     C                   PARM                    $ALLFUNCT       480
089400000000     C*
089500000000     C                   ENDSR
089600000000     C/EJECT
089700000000     C************************************************************
089800000000     C* GESTIONE EMISSIONE TASTI FUNZIONE
089900000000     C************************************************************
090000000000     C     GESPIE        BEGSR
090100000000     C*
090200000000     C                   SELECT
090300000000     C* Parzializzazioni
090400000000     C     $GEST         WHENEQ    'D1'
090500000000     C     $ULKD1        IFEQ      0
090600000000     C* riempimento opzioni
090700000000     C                   EXSR      F24D1
090800000000     C                   ELSE
090900000000     C                   WRITE     FMTZ1
091000000000     C                   ENDIF
091100000000     C* Lista
091200000000     C     $GEST         WHENEQ    'S1'
091300000000     C     $ULKS1        IFEQ      0
091400000000     C                   EXSR      F24S1
091500000000     C                   ELSE
091600000000     C                   WRITE     FMTZ2
091700000000     C                   ENDIF
091800000000     C* Modifica valori assunti
091900000000     C     $GEST         WHENEQ    'D2'
092000000000     C     $ULKD2        IFEQ      0
092100000000     C                   EXSR      F24D2
092200000000     C                   ELSE
092300000000     C                   WRITE     FMTZ3
092400000000     C                   ENDIF
092500000000     C                   ENDSL
092600000000     C*
092700000000     C                   ENDSR
092800000000     C/EJECT
092900000000     C************************************************************
093000000000     C* GESTIONE PARZIALIZZAZIONI
093100000000     C************************************************************
093200000000     C     GESD1         BEGSR
093300000000     C*
093400000000     C* inizializzazione videata
093500000000     C     $INZD1        IFEQ      *ON
093600000000     C                   EXSR      INZD1
093700000000     C                   MOVE      *OFF          $INZD1
093800000000     C                   ENDIF
093900000000     C* emissione piede videata se proveniente da altra
094000000000     C* salvataggio valore formato di provenienza
094100000000     C     $LASTV        IFNE      'D1'
094200000000     C*
094300000000     C* recupero il sottotitolo (se c'è)
094400000000     C                   Clear                   D1TIT
094500000000     C     SOTTITD1      IFNE      *BLANK
094600000000     C                   MOVEL     SOTTITD1      IDMSG
094700000000     C                   CALLB     'XRTVM'
094800000000     C                   PARM                    IDMSG            27
094900000000     C                   PARM                    TXTMSG          100
095000000000     C                   PARM                    ERRMSG            1
095100000000     C                   PARM      *Off          CTRMSG            1            centratura
095200000000     C                   PARM      15            LENMSG            3 0          lun output.
095300000000     C     ERRMSG        IFNE      *ON
095400000000     C                   MOVEL     TXTMSG        D1TIT
095500000000     C                   ELSE
095600000000     C                   MOVEL     *ALL'?'       D1TIT
095700000000     C                   ENDIF
095800000000     C                   ENDIF
095900000000     C*
096000000000     C* salvo stato F17 per sapere cosa fare in caso di F12
096100000000     C                   MOVE      *INKR         WINKR
096200000000     C                   WRITE     FMTT1
096300000000     C                   EXSR      GESPIE
096400000000     C                   MOVE      $LASTV        $LASTG
096500000000     C                   MOVE      'D1'          $LASTV
096600000000     C                   ENDIF
096700000000     C*
096800000000     C* se esistono errori sulla videata
096900000000     C* emetto la write del formato a indicatori spenti per vedere
097000000000     C* le eventuali decodifiche
097100130930   B1C                   if        (*IN99       = *On   or
097200130930   B1C                              *In17       = *On      )
097300000000     C                   MOVEA     *IN           WIN
097400000000     C                   MOVE      *ALL'0'       IN5098           49
097500000000     C                   MOVEA     IN5098        *IN(50)
097600130930     C                   Eval      WIn17        = *In17
097700130930     C                   Eval      *In17        = *Off
097800000000     C                   WRITE     FMTD1
097900000000     C                   MOVEA     WIN           *IN
098000130930     C                   Eval      *In17        = WIn17
098100000000     C                   ENDIF
098200000000     C*
098300000000     C                   EXFMT     FMTD1
098400000000     C*
098500000000     C* azzero la posizione del cursore
098600000000     C                   Z-ADD     0             H1RIGA
098700000000     C                   Z-ADD     0             H1COLO
098800000000     C*
098900000000     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
099000000000     C* INPUT
099100000000     C* $FC1 = Abilitazioni videata D1
099200000000     C* OUTPUT
099300000000     C* *IN97 + *IN99 Errori
099400000000     C                   MOVEA     $FC1          $FC
099500000000     C                   EXSR      CTRKEY
099600000000     C*
099700000000     C     *IN99         IFEQ      *OFF
099800000000     C*
099900000000     C                   SELECT
100000000000     C* F2=Decodifica
100100000000     C     $TASTO        WHENEQ    F02
100200000000     C                   EXSR      F02D1
100300000000     C* F3=Fine
100400000000     C     $TASTO        WHENEQ    F03
100500000000     C                   EXSR      F03D1
100600000000     C* F4=Ricerca
100700000000     C     $TASTO        WHENEQ    F04
100800000000     C                   EXSR      SEARCH
100900000000     C* F5=Refresh
101000000000     C     $TASTO        WHENEQ    F05
101100000000     C                   EXSR      F05D1
101200000000     C* F8=Successivo
101300000000     C     $TASTO        WHENEQ    F08
101400000000     C                   EXSR      F08D1
101500000000     C* F10=Immissione
101600000000     C     $TASTO        WHENEQ    F10
101700000000     C                   EXSR      F10D1
101800000000     C* F12=Ritorno
101900000000     C     $TASTO        WHENEQ    F12
102000000000     C                   EXSR      F12D1
102100000000     C* F18=Modifica valori assunti
102200000000     C     $TASTO        WHENEQ    F18
102300000000     C                   EXSR      F18D1
102400000000     C* F24=Altri tasti
102500000000     C     $TASTO        WHENEQ    F24
102600000000     C                   EXSR      F24D1
102700000000     C*
102800000000     C                   OTHER
102900000000     C                   EXSR      CTRD1
103000000000     C     *IN99         IFEQ      *OFF
103100000000     C* definisco l'ordinamento
103200000000     C                   EXSR      DEFORD
103300000000     C* richiamo la lista
103400000000     C                   MOVE      'S1'          $GEST
103500000000     C                   MOVE      *ON           $INZS1
103600000000     C                   MOVE      *BLANK        W1POS
103700000000     C                   ENDIF
103800000000     C*
103900000000     C                   ENDSL
104000000000     C*
104100000000     C                   ENDIF
104200000000     C*
104300000000     C                   ENDSR
104400000000     C/EJECT
104500000000     C************************************************************
104600000000     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
104700000000     C************************************************************
104800000000     C     INZD1         BEGSR
104900000000     C*
105000000000     C                   CLEAR                   FMTD1
105100140327     C*
105200000000     C* Se pgm  richiamato con parametri forniti , li imposto nelle
105300000000     C*    parzializzazioni
105400000000     C                   SELECT
105500000000     C* il pgm è richiamato con chiave univoca per gestione POSTA
105600000000     C     $Unique       WHENEQ    *ON
105700000000     C* il pgm è richiamato e
105800000000     C* sono stati passati dei valori di parzializzazione ma non
105900000000     C* la chiave univoca
106000170712     C     OPZI73        WHENNE    *BLANK
106100000000     C* imposto i valori passati sui campi a video
106200170712     C                   MOVEL     AnnoI73       D1Anno
106300170804     C                   MOVEL     TrimI73       D1Trim
106400170810     C*                  MOVEL     MeseI73       D1Mese
106500170731     C                   MOVEL     Stato1I73     D1Stato1
106600170719     C                   MOVEL     PartivaI73    D1Partiva
106700170718     C                   MOVEL     CdFiscI73     D1CdFisc
106800170712     C                   MOVEL     SoggI73       D1Sogg
106900170713     C                   MOVEL     CausaleI73    D1Causale
107000170712     C                   MOVEL     AnnI73        D1Ann
107100170712     C                   MOVEL     CFI73         D1CF
107200170712     C                   MOVEL     TpRecI73      D1TpRec
107300000000     C* il pgm non è richiamato
107400170712     C     OPZI73        WHENEQ    *BLANK
107500170713     C                   Eval      D1Anno      = *Year
107600170727     C                   Eval      D1Ann       = '1'
107700170727     C                   Eval      D1Trim      = 0
107800170727     C                   Eval      D1TpRec     = 'O'
107900170712     C                   ENDSL
108000170713      *
108100170712     C                   ENDSR
108200000000      /EJECT
108300000000      ************************************************************
108400000000      * GESTIONE F02 VIDEO D1
108500000000      ************************************************************
108600000000     C     F02D1         BEGSR
108700000000     C*
108800000000     C* per decodificare la videata richiamo la routine dei ctrl
108900000000     C* e poi disattivo gli eventuali messaggi di errore
109000000000     C                   EXSR      CTRD1
109100000000     C*
109200000000     C                   MOVE      *ALL'0'       IN5098
109300000000     C                   MOVEA     IN5098        *IN(50)
109400000000     C*
109500000000     C                   ENDSR
109600170712      /EJECT
109700000000     C************************************************************
109800000000     C* GESTIONE F03 VIDEO D1
109900000000     C************************************************************
110000000000     C     F03D1         BEGSR
110100000000     C*
110200000000     C                   MOVE      *ON           $FINE
110300170712    >C                   MOVE      '1'           RETI73
110400000000     C* fine programma
110500000000     C                   EXSR      ENDPGM
110600000000     C*
110700000000     C                   ENDSR
110800000000     C/EJECT
110900000000     C************************************************************
111000000000     C* GESTIONE F05 VIDEO D1
111100000000     C************************************************************
111200000000     C     F05D1         BEGSR
111300000000     C*
111400000000     C                   MOVE      *ON           $INZD1
111500000000     C*
111600000000     C                   ENDSR
111700000000     C/EJECT
111800000000     C************************************************************
111900000000     C* GESTIONE F08 VIDEO D1
112000000000     C************************************************************
112100000000     C     F08D1         BEGSR
112200000000     C*
112300000000     C                   MOVE      *ON           $FINE
112400170712    >C                   MOVE      '0'           RETI73
112500000000     C*
112600000000     C                   ENDSR
112700000000     C/EJECT
112800000000     C************************************************************
112900000000     C* GESTIONE F10 VIDEO D1
113000000000     C************************************************************
113100000000     C     F10D1         BEGSR
113200000000     C*
113300170712    >C                   RESET                   SDGSI74DS
113400170712    >C                   MOVE      xscsoc        SocietaI74
113500170712    >C                   MOVEL     '01'          OPZI74
113600170712    >C     CMTI73        IFEQ      '1'
113700170712    >C                   MOVEL     '1'           CMTI74
113800000000     C                   ELSE
113900170712    >C                   MOVEL     '0'           CMTI74
114000000000     C                   ENDIF
114100170803     C*
114200170712     C                   Eval      AnnoI74     = D1Anno
114300170712     C                   Eval      TrimI74     = D1Trim
114400170713     C*
114500170713     C                   Eval      SoggI74     = D1Sogg
114600170712     C                   Eval      TpAnnI74    = D1TpAnn
114700170803     C                   Eval      TpComI74    = 'T'
114800170713     C                   Eval      TpRecI74    = D1TpRec
114900170803     C                   Eval      CfI74       = D1Cf
115000170803     C                   Eval      PartivaI74  = D1Partiva
115100170803     C                   Eval      CdFiscI74   = D1CdFisc
115200000000     C* passare anche le parzializzazioni ricevute relative alla chiave
115300000000     C* univoca :
115400000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
115500170712    >C                   IF        OPZI73 <> *Blank
115600000000     C                   ENDIF
115700000000     C*
115800170712    >C                   CALL      'SDGSI74R'
115900000000     C                   PARM                    KPJBA
116000170712    >C                   PARM                    SDGSI74DS
116100000000     C* ritorno da PGM gestione
116200170712     C                   EXSR      RTNI74
116300000000     C*
116400000000     C                   MOVE      *HIVAL        $LASTV
116500000000     C*
116600000000     C                   ENDSR
116700000000     C/EJECT
116800000000     C************************************************************
116900000000     C* GESTIONE F12 VIDEO D1
117000000000     C************************************************************
117100000000     C     F12D1         BEGSR
117200000000     C*
117300000000     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
117400000000     C*  per F12 torno alla lista
117500000000     C     $LOP          IFEQ      2
117600000000     C     WINKR         OREQ      *ON
117700000000     C                   MOVE      'S1'          $GEST
117800000000     C*  altrimenti, per F12 torno a menu
117900000000     C                   ELSE
118000000000     C                   MOVE      *ON           $FINE
118100170713    >C                   MOVE      '2'           RETI73
118200000000     C                   ENDIF
118300000000     C*
118400000000     C                   ENDSR
118500000000     C/EJECT
118600000000     C************************************************************
118700000000     C* GESTIONE F18 VIDEO D1
118800000000     C************************************************************
118900000000     C     F18D1         BEGSR
119000000000     C*
119100000000     C                   MOVE      'D2'          $GEST
119200000000     C                   MOVEL     *ON           $INZD2
119300000000     C*
119400000000     C                   ENDSR
119500000000     C/EJECT
119600000000     C************************************************************
119700000000     C* GESTIONE F24 VIDEO D1
119800000000     C* input
119900000000     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
120000000000     C* $FX1 =Testi tasti funzione
120100000000     C* $FC1 = Abilitazioni
120200000000     C* $FR1 = Inversioni di fondo
120300000000     C************************************************************
120400000000     C     F24D1         BEGSR
120500000000     C*
120600000000     C                   MOVEA     $FX1          $FX
120700000000     C                   MOVEA     $FC1          $FC
120800000000     C                   MOVEA     $FR1          $FR
120900000000     C                   Z-ADD     $ULKD1        $ULK
121000000000     C                   Z-ADD     $D1L01        $L01
121100000000     C                   Z-ADD     $D1L02        $L02
121200000000     C                   EXSR      RIEKEY
121300000000     C                   Z-ADD     $ULK          $ULKD1            3 0
121400000000     C                   MOVEA     $FC           $FC1
121500000000     C                   MOVEL     $KEY1         Z1KE1
121600000000     C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
121700000000     C                   MOVEL     $KEY2         Z1KE2
121800000000     C                   WRITE     FMTZ1
121900000000     C*
122000000000     C*
122100000000     C                   ENDSR
122200000000     C/EJECT
122300000000     C************************************************************
122400000000     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
122500000000     C* non usare i seguenti indicatori (perchè relativi a D1MSG):
122600000000     C* 93 94 96 97 98
122700000000     C************************************************************
122800000000     C     CTRD1         BEGSR
122900000000     C*
123000000000     C                   SETOFF                                       99
123100000000     C*
123200000000     C                   EXSR      CtrAnno
123300170713     C*
123400170713     C                   EXSR      CtrTrim
123500130911     C*
123600130911     C                   EXSR      CtrCF
123700000000     C*
123800170808     C                   EXSR      CtrStato1
123900000000     C*
124000000000     C                   EXSR      CtrSogg
124100170713     C*
124200170808     C                   if        D1Anno    > 10
124300170713     C                   EXSR      CtrCausale
124400170808     C                   endif
124500000000     C*
124600000000     C                   EXSR      CtrAnn
124700130829     C*
124800170712     C                   EXSR      CtrTpRec
124900170712     C*
125000000000     C                   ENDSR
125100000000     C/EJECT
125200000000     C***********************************************************
125300000000     C* Controllo anno di competenza
125400000000     C***********************************************************
125500000000     C     CtrAnno       BEGSR
125600000000     C*
125700000000     C                   if        D1Anno = 0
125800170808     C                   Eval      D1Anno      = *Year
125900170808     C**********         SetOn                                        5399
126000000000     C                   endif
126100000000     C*
126200000000     C                   ENDSR
126300000000     C/EJECT
126400000000     C***********************************************************
126500000000     C* Controllo Stato
126600000000     C***********************************************************
126700170808     C     CtrStato1     BEGSR
126800000000     C*
126900170731     C                   Clear                   D1Stato1D
127000170808     C                   Clear                   D1IdPaes1
127100170808     C*
127200000000     C                   Clear                   AngG12DS
127300000000     C*
127400170731     C                   if        D1Stato1   <> *Blank
127500000000     C*
127600000000     C                   RESET                   SDGTABDS
127700000000     C*
127800000000     C                   Eval      XTbRic      = '1'
127900000000     C                   Eval      XTbAzi      = XScSoc
128000000000     C                   Eval      XTbKey      = *Zero
128100000000     C                   Eval      XTbCod      = 'G12'
128200170731     C                   Move      D1Stato1      XTbKey
128300000000     C*
128400000000     C                   CALL      'SDGTABDR'
128500000000     C                   PARM                    SDGTABDS
128600000000     C*
128700000000     C                   if        XTbErr     <> '0'
128800000000     C                   SetOn                                        5299
128900000000     C*
129000000000     C                   else
129100000000     C                   MoveL     XTbUni        AngG12DS
129200170731     C                   MoveL     DesG12        D1Stato1D
129300170718     C* Reperisco stato ISO da tabella HD4
129400170731     C                   movel     D1Stato1      WStatoHd4
129500170731     C                   Clear                   D1IdPaes1
129600170718     C                   Exsr      RepHd4
129700170731     C                   Movel     CodIso2Hd4    D1IdPaes1
129800170718     C*
129900170718     C                   endif
130000000000     C*
130100000000     C                   endif
130200000000     C*
130300000000     C                   ENDSR
130400170718     C/EJECT
130500170718     C***********************************************************
130600170718     C* Reperisco stato iso da tabella HD4
130700170718     C***********************************************************
130800170718     C     RepHd4        BEGSR
130900170718     C*
131000170718     C                   Clear                   SDGHD4DS
131100170718     C*
131200170718     C                   RESET                   SDGTABDS
131300170718     C*
131400170718     C                   Eval      XTbRic      = '1'
131500170718     C                   Eval      XTbAzi      = XScSoc
131600170718     C                   Eval      XTbKey      = *Zero
131700170727     C                   Eval      XTbCod      = 'HD4'
131800170727     C                   Move      WstatoHd4     XTbKey
131900170718     C*
132000170718     C                   CALL      'SDGTABDR'
132100170718     C                   PARM                    SDGTABDS
132200170718     C*
132300170718     C                   if        XTbErr     =  '0'
132400170718     C                   MoveL     XTbUni        SDGHd4DS
132500170718     C                   endif
132600170718     C*
132700170718     C*
132800170718     C                   ENDSR
132900170718     C/EJECT
133000170713     C***********************************************************
133100170712     C* Controllo trimestre
133200170713     C***********************************************************
133300170727     C     CtrTrim       BEGSR
133400140327     C*
133500140327     C                   Clear                   X07ValDs
133600140327     C*
133700140327     C                   Move      '1'           X07Ric
133800140327     C                   Move      *On           X07All
133900170712     C                   Movel     'SDGCST000'   X07TRc
134000170712     C                   Movel     'CSTTRIM   '  X07Cam
134100170712     C                   Movel     D1Trim        X07VAL
134200140327     C*
134300140327     C                   Callb     'X07VALR'
134400140327     C                   Parm                    X07ValDs
134500140327     C*
134600140327     C     X07ERR        IFEQ      *Off
134700140327     C                   else
134800140327     C                   SetOn                                        7799
134900140327     C                   endif
135000140327     C*
135100140327     C                   ENDSR
135200170713     C/EJECT
135300130911     C***********************************************************
135400111003     C* Controllo tipo soggetto
135500130911     C***********************************************************
135600111003     C     CtrCF         BEGSR
135700111003     C*
135800111003     C                   Clear                   D1CFD
135900111003     C*
136000111003     C                   Clear                   X07ValDs
136100111003     C*
136200170724     C                   If        D1Cf <> *blanks
136300111003     C                   Move      '1'           X07Ric
136400111003     C                   Move      *On           X07All
136500170712     C                   Movel     'SDGCST000'   X07TRc
136600170712     C                   Movel     'CSTCF    '   X07Cam
136700111003     C                   Movel     D1CF          X07VAL
136800111003     C*
136900111003     C                   Callb     'X07VALR'
137000111003     C                   Parm                    X07ValDs
137100111003     C*
137200111003     C     X07ERR        IFEQ      *Off
137300130911     C                   MOVEL     X07DES        D1CFD
137400111003     C                   else
137500111003     C                   SetOn                                        5499
137600111003     C                   endif
137700170724     C                   endif
137800111003     C*
137900111003     C                   ENDSR
138000130911     C/EJECT
138100000000     C***********************************************************
138200000000     C* Controllo codice soggetto
138300000000     C***********************************************************
138400000000     C     CtrSogg       BEGSR
138500000000     C*
138600000000     C                   Clear                   D1SoggD
138700000000     C                   if        D1Sogg     <> *Blank
138800000000     C*
138900000000     C     D1SOGG        IFNE      *BLANK
139000000000     C                   CALLb     'XALLINEA'
139100000000     C                   PARM                    D1SOGG
139200000000     C                   PARM      8             LEN               5 0
139300000000     C                   PARM                    XSCINP
139400000000     C                   ENDIF
139500000000     C*
139600000000     C                   MoveL     D1Sogg        WSogg
139700000000     C                   EXSR      CallDvaSog
139800000000     C*
139900000000     C                   if        DVAErrore  <> '1'
140000000000     C*
140100000000     C                   Eval      %subst(DVASOG:1:DVALENOUT)
140200000000     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
140300000000     C*
140400000000     C                   MoveL     DVSDes        D1SoggD
140500000000     C*
140600000000     C                   else
140700000000     C*
140800130930     C                   Eval      *In17       = *On
140900000000     C*
141000000000     C                   endif
141100000000     C*
141200000000     C                   endif
141300000000     C*
141400000000     C                   ENDSR
141500000000     C/EJECT
141600170713     C************************************************************
141700170718     C* Controllo causale
141800170713     C************************************************************
141900170713     C     CtrCausale    BEGSR
142000131206     C*
142100131206     C                   Clear                   D1CausaleD
142200131206     C*
142300131206     C* la causale deve essere valida, controllo con data
142400131206     C* di 10 anni in meno rispetto all'1/01/Anno a video
142500131206     C                   Eval      WAnno       = D1Anno
142600131206     C                   MoveL     WAnno         AnnoDs
142700131206     C                   MoveL     '01'          MeseDs
142800131206     C                   MoveL     '01'          GiornoDs
142900131206     C                   Move      DataDs        WDtAlfa
143000131206     C                   Move      WDtAlfa       WData
143100131206     C     WData         SubDur    10:*Y         WData
143200131206     C                   Move      WData         WDtAlfa
143300131206     C*
143400131206     C                   if        D1Causale  <> *Blank
143500131206     C                   Clear                   ANOPE00F
143600131206     C                   Eval      Lenout = %Size(ANOPE00F)
143700131206     C                   CALLB     'NDMVC003'
143800131206     C                   PARM                    XScSoc
143900131206     C                   PARM                    D1Causale
144000131206     C                   PARM                    WDtAlfa
144100131206     C                   PARM                    Uso
144200131206     C                   PARM      *Off          GesMsg
144300131206     C                   PARM      *Off          Esito
144400131206     C                   PARM      'ANOPE     '  Struttura
144500131206     C                   PARM                    ANOPE00F
144600131206     C                   PARM                    LenOut
144700131206     C                   PARM      *Off          Refresh
144800131206     C*
144900131206     C                   if        Esito     = *on
145000131206     C                   SetOn                                        5199
145100131206     C                   else
145200131206     C*
145300131206     C                   Eval      D1CausaleD  = OPEDesBrev
145400131206     C*
145500131206     C                   endif
145600131206     C*
145700131206     C                   endif
145800131206     C*
145900170713     C                   ENDSR
146000170713     C/EJECT
146100130919     C***********************************************************
146200170718     C* Controllo tipo record
146300130919     C***********************************************************
146400170718     C     CtrTpRec      BEGSR
146500130919     C*
146600170718     C                   Clear                   D1TpRecD
146700130919     C*
146800130919     C                   Clear                   X07ValDs
146900130919     C*
147000130919     C                   Move      '1'           X07Ric
147100130919     C                   Move      *On           X07All
147200170712     C                   Movel     'SDGCST000'   X07TRc
147300170718     C                   Movel     'CSTTPREC '   X07Cam
147400170718     C                   Movel     D1TpRec       X07VAL
147500130919     C*
147600130919     C                   Callb     'X07VALR'
147700130919     C                   Parm                    X07ValDs
147800130919     C*
147900130919     C     X07ERR        IFEQ      *Off
148000170718     C                   MOVEL     X07DES        D1TpRecD
148100130919     C                   else
148200130919     C                   SetOn                                        5599
148300130919     C                   endif
148400130919     C*
148500130919     C                   ENDSR
148600130919     C/EJECT
148700000000     C***********************************************************
148800000000     C* Controllo dati annullati
148900000000     C***********************************************************
149000000000     C     CtrAnn        BEGSR
149100000000     C*
149200000000     C                   Clear                   D1AnnD
149300000000     C*
149400000000     C                   Clear                   X07ValDs
149500000000     C                   Move      '1'           X07Ric
149600000000     C                   Move      *Off          X07All
149700000000     C                   Movel     'ANDIZ000 '   X07TRc
149800000000     C                   Movel     'DSPATT   '   X07Cam
149900000000     C                   Movel     D1Ann         X07VAL
150000000000     C                   Callb     'X07VALR'
150100000000     C                   Parm                    X07ValDs
150200000000     C*
150300000000     C     X07ERR        IFEQ      *Off
150400000000     C                   MOVEL     X07DES        D1AnnD
150500000000     C                   else
150600000000     C                   SetOn                                        5699
150700000000     C                   endif
150800000000     C*
150900000000     C                   Clear                   D1TpAnnD
151000000000     C                   if        D1TpAnn    <>  *blank
151100000000     C*  solo se richiesti solo gli annullati oppure con annullati
151200000000     C                   if        D1Ann      =   '1'    or
151300000000     C                             D1Ann      =   '3'
151400000000     C                   exsr      CtrTpAnn
151500000000     C                   else
151600000000     C                   SetOn                                        5899
151700000000     C                   endif
151800000000     C                   endif
151900000000     C*
152000000000     C                   ENDSR
152100000000     C/EJECT
152200000000     C***********************************************************
152300000000     C* Controllo tipo annullamento
152400000000     C***********************************************************
152500000000     C     CtrTpAnn      BEGSR
152600000000     C*
152700000000     C                   Clear                   X07ValDs
152800000000     C                   Move      '1'           X07Ric
152900000000     C                   Move      *Off          X07All
153000170712     C                   Movel     'SDGCST000'   X07TRc
153100170807     C                   Movel     'CSTTPANN'    X07Cam
153200000000     C                   Movel     D1TpAnn       X07VAL
153300000000     C                   Callb     'X07VALR'
153400000000     C                   Parm                    X07ValDs
153500000000     C*
153600000000     C     X07ERR        IFEQ      *Off
153700000000     C                   MOVEL     X07DES        D1TpAnnD
153800000000     C                   else
153900000000     C                   SetOn                                        5799
154000000000     C                   endif
154100000000     C*
154200000000     C                   ENDSR
154300111011     C/EJECT
154400000000     C************************************************************
154500000000     C* Chiamata al driver dei soggetti
154600000000     C************************************************************
154700000000     C     CallDvaSog    BEGSR
154800000000     C*
154900000000     C                   EVAL      DVALenOut   = %Size(DVASOG)
155000000000     C*
155100000000     C                   CALLB     'NDDVASOG'
155200000000     C                   PARM      '1'           DVARIC
155300000000     C                   PARM      XSCSOC        DVASOCIETA
155400000000     C                   PARM                    DVAUNITA
155500000000     C                   PARM      'DVASOG    '  DVASTRUTT
155600000000     C                   PARM                    DVADTRIF
155700000000     C                   PARM                    DVALENOUT
155800000000     C* Se si accede con il codice cli/for impostare i campi seguenti
155900000000     C* se si accede con il soggetto ometterli e impostare gli altri
156000000000     C*                  PARM                    DVASNATURA
156100000000     C*                  PARM                    DVACODICE
156200000000     C*                  PARM                    DVALINEAV
156300000000     C*                  PARM                    DVAFILIALE
156400000000     C                   PARM                    *omit
156500000000     C                   PARM                    *omit
156600000000     C                   PARM                    *omit
156700000000     C                   PARM                    *omit
156800000000     C                   PARM      WSogg         DVSSOGG
156900000000     C                   PARM      *Blank        DVSTPIND
157000000000     C                   PARM      *Blank        DVSCDIND
157100000000     C                   PARM                    DVAERRORE
157200000000     C                   PARM                    DVAOUTPUT
157300000000     C*
157400000000     C                   ENDSR
157500000000     C/EJECT
157600000000     C************************************************************
157700000000     C* DEFINIZIONE DELL'ORDINAMENTO
157800000000     C************************************************************
157900000000     C     DEFORD        BEGSR
158000000000     C*
158100000000     C* definisco l'ordinamento della lista a seconda dei valori
158200000000     C* impostati nel video di parzializzazione
158300000000     C                   Z-ADD     1             $ORD
158400000000     C*
158500000000     C                   SELECT
158600000000     C* imposto l'ordinamento 99 per ottenere in selezione solo il
158700000000     C* rcd con la chiave passata
158800000000     C     $Unique       WHENEQ    *ON
158900000000     C                   Z-ADD     99            $ORD
159000000000     C* nel caso non sia stato digitato alcun valore in parzializzazione
159100000000     C* utilizzerò l'ordinamento precedentemente memorizzato
159200000000     C* (temporaneamente o definitivamente) nei valori assunti
159300000000     C                   OTHER
159400000000     C                   ENDSL
159500000000     C                   ENDSR
159600000000     C/EJECT
159700000000     C************************************************************
159800000000     C* Chiamata programma con tasto personalizzato
159900000000     C************************************************************
160000000000     C     FPerS1        BEGSR
160100000000     C*
160200000000     C* salvo la DS di ingresso
160300170712    >C                   MOVEL     SDGSI73DS     $SaveDS
160400000000     C*
160500170712    >C                   MOVEL     SDGSI73DS     KPJBU
160600000000     C                   CALL      $PGF2(PosFun)                        22
160700000000     C                   PARM                    KPJBA
160800000000     C*
160900000000     C* ripristino la DS di ingresso
161000170712    >C                   MOVEL     $SaveDS       SDGSI73DS
161100000000     C*
161200000000     C                   ENDSR
161300000000     C/EJECT
161400000000     C************************************************************
161500000000     C* GESTIONE LISTA
161600000000     C************************************************************
161700000000     C     GESS1         BEGSR
161800000000     C*
161900000000     C* inizializzazione videata
162000000000     C     $INZS1        IFEQ      *ON
162100000000     C                   EXSR      INZS1
162200000000     C                   MOVE      *OFF          $INZS1
162300000000     C                   ENDIF
162400000000     C* emissione piede videata se proveniente da altra
162500000000     C* salvataggio valore formato di provenienza
162600000000     C     $LASTV        IFNE      'S1'
162700000000     C                   WRITE     FMTT1
162800000000     C                   EXSR      GESPIE
162900000000     C                   MOVE      $LASTV        $LASTG
163000000000     C                   MOVE      'S1'          $LASTV
163100000000     C                   ENDIF
163200000000     C*
163300000000     C     WMAX          IFEQ      0
163400000000     C                   WRITE     FMTD3                                        NO RCD
163500000000     C                   ENDIF
163600000000     C* Inizializzazione indice tasti funzione personalizzati
163700000000     C                   Eval      PosFun = 25
163800000000     C*
163900000000     C                   EXFMT     FMTC1
164000000000     C*
164100000000     C* azzero la posizione del cursore
164200000000     C                   Z-ADD     0             H1RIGA
164300000000     C                   Z-ADD     0             H1COLO
164400000000     C* Serve per avvertire i pgm di sola finestra di eseguire le operazioni
164500000000     C* di refresh dello sfondo
164600000000     C                   MOVE      '1'           WExf
164700000000     C*
164800000000     C     C1NRR         IFNE      0
164900000000     C                   Z-ADD     C1NRR         WSFL
165000000000     C                   ENDIF
165100000000     C                   IF        SFLNRR > 0
165200000000     C                   Z-ADD     SFLNRR        C1RCD
165300000000     C                   ENDIF
165400000000     C* inizializzazione indice opzioni personalizzate
165500000000     C                   Eval      PosOpz = 0
165600000000     C* CONTROLLO ABILITAZIONE TASTI
165700000000     C* INPUT
165800000000     C* $FC2 = Abilitazioni videata S1
165900000000     C* OUTPUT
166000000000     C* *IN97 + *IN99 Errori
166100000000     C                   MOVEA     $FC2          $FC
166200000000     C                   EXSR      CTRKEY
166300000000     C*
166400000000     C     *IN99         IFEQ      *OFF
166500000000     C*
166600000000     C                   SELECT
166700000000     C* F3=Fine
166800000000     C     $TASTO        WHENEQ    F03
166900000000     C                   EXSR      F03S1
167000000000     C* F8=Successivo
167100000000     C     $TASTO        WHENEQ    F08
167200000000     C                   EXSR      F08S1
167300000000     C* F10=Immissione
167400000000     C     $TASTO        WHENEQ    F10
167500000000     C                   EXSR      F10S1
167600000000     C* F12=Ritorno
167700000000     C     $TASTO        WHENEQ    F12
167800000000     C                   EXSR      F12S1
167900000000     C* F17=Parzializzazioni
168000000000     C     $TASTO        WHENEQ    F17
168100000000     C                   EXSR      F17S1
168200000000     C* F18=Modifica valori assunti
168300000000     C     $TASTO        WHENEQ    F18
168400000000     C                   EXSR      F18S1
168500000000     C* F23=Altre opzioni
168600000000     C     $TASTO        WHENEQ    F23
168700000000     C                   EXSR      F23S1
168800000000     C* F24=Altri tasti
168900000000     C     $TASTO        WHENEQ    F24
169000000000     C                   EXSR      F24S1
169100000000     C* ROLL-UP
169200000000     C     $TASTO        WHENEQ    ROLLUP
169300000000     C                   EXSR      ROLS1
169400000000     C* Personalizzata
169500000000     C     $PGF2(PosFun) Whenne    *BLANK
169600000000     C                   EXSR      FperS1
169700000000     C*
169800000000     C                   OTHER
169900000000     C* CONTROLLO DATI
170000000000     C                   EXSR      CTRC1
170100000000     C     *IN99         IFEQ      *OFF
170200000000     C                   EXSR      CTRS1
170300000000     C                   ENDIF
170400000000     C*
170500000000     C                   ENDSL
170600000000     C*
170700000000     C                   ENDIF
170800000000     C*
170900000000     C                   ENDSR
171000000000     C/EJECT
171100000000     C************************************************************
171200000000     C* Caricamento OPZIONI
171300000000     C************************************************************
171400000000    >C     CarOpz        BEGSR
171500000000     C*
171600000000     C* Reperimento tasti di funzione
171700000000     C                   EXSR      REPKEY
171800000000     C*
171900000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
172000000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
172100000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
172200000000     C     $ULKD1        IFEQ      0
172300000000     C                   EXSR      F24D1
172400000000     C                   ELSE
172500000000     C                   WRITE     FMTZ1
172600000000     C                   ENDIF
172700000000     C*
172800000000     C                   ENDSR
172900000000     C/EJECT
173000000000     C************************************************************
173100000000     C* INIZIALIZZAZIONE LISTA
173200000000     C************************************************************
173300000000    >C     INZS1         BEGSR
173400000000     C* pulizia SFL
173500000000     C                   SETOFF                                         3031
173600000000     C                   WRITE     FMTC1
173700000000     C                   SETON                                          31
173800000000     C*
173900000000     C* pulizia del sottotitolo
174000000000     C                   Clear                   D1TIT
174100000000     C*
174200000000     C                   eval      C1Anno  = D1Anno
174300170712     C                   Eval      C1Trim  = D1Trim
174400000000     C*
174500000000     C* riempimento opzioni
174600000000     C                   MOVEA     $TX1          $TX
174700000000     C                   MOVEA     $FL1          $FL
174800000000     C                   Z-ADD     0             WCOZ
174900000000     C                   Z-ADD     0             WMXO
175000000000     C*  equivale al num.di elementi della schiera $TX1
175100000000     C                   Z-add     OpzLun        WMXO
175200000000     C                   Z-ADD     $C1Op1        $L01
175300000000     C                   Z-ADD     $C1Op2        $L02
175400000000     C                   EXSR      RIEOPZ
175500000000     C                   Z-ADD     23            X
175600000000     C                   MOVE      FF23          $FC2(X)
175700000000     C                   MOVEL     WOPZ1         C1OPZ1
175800000000     C                   MOVEL     $ALLOPT       H1ALLOPT
175900000000     C                   MOVEL     WOPZ2         C1OPZ2
176000000000     C                   Z-ADD     WCOZ          C1COZ
176100000000     C*
176200000000     C* CARICAMENTO SFL A PAGINE
176300000000     C*
176400000000     C                   Z-ADD     0             S1NRR
176500000000     C                   Z-ADD     0             WMAX
176600000000     C*
176700000000     C* Posizionamento su file pilota
176800000000     C                   CLEAR                   C1POS
176900000000     C                   EXSR      SETANA
177000000000     C* ripristino i valori del posizionamento
177100000000     C                   MOVE      W1POS         C1POS
177200000000     C*
177300000000     C* Lettura file pilota
177400000000     C                   EXSR      REDANA
177500000000     C*
177600000000     C* se arrivo dalla parzializzazione devo caricare solo la prima pagina
177700000000     C                   IF        $LastV = 'D1'
177800000000     C                   Z-ADD     SFLPAG        WPAG
177900000000     C                   ENDIF
178000000000     C                   EXSR      ROLS1
178100000000     C* imposto il caricamento sfl del numero massimo di righe per pagina
178200000000     C                   Z-ADD     SFLPAG        WPAG
178300000000     C*
178400000000     C                   ENDSR
178500000000     C/EJECT
178600000000     C************************************************************
178700000000     C* CARICAMENTO PAGINA LISTA
178800000000     C************************************************************
178900000000     C     ROLS1         BEGSR
179000000000     C*
179100000000     C                   SETOFF                                       32
179200000000     C                   Z-ADD     0             Y
179300000000     C                   Z-ADD     WMAX          S1NRR
179400000000     C*
179500000000     C* Leggo dal file anagrafico per caricare la lista
179600000000     C*
179700000000     C     $EndFile      DOWEQ     *OFF
179800000000     C     Y             ANDLT     WPAG
179900000000     C*
180000000000     C                   EXSR      RIES1
180100170727     C*
180200170727     C                   Exsr      SAVRott01
180300170727     C*
180400170809     C                   If        $RcdOk = *on
180500170808     C                   Exsr      SAVFlag01
180600170809     C                   Endif
180700000000     C*
180800000000     C                   ADD       1             S1NRR
180900000000     C                   ADD       1             Y
181000000000     C*
181100000000     C                   WRITE     FMTS1
181200000000     C*
181300000000     C                   EXSR      REDANA
181400000000     C*
181500000000     C                   ENDDO
181600000000     C*
181700170807     C****               Z-ADD     S1NRR         WMAX                 30
181800000000     C*
181900000000     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
182000000000     C*
182100170807     C**** S1NRR         DIV       SFLPAG        PAGINE            4 0
182200170807     C***                MVR                     RESTO             3 0
182300170807     C**** PAGINE        MULT      SFLPAG        C1RCD
182400170807     C***  RESTO         IFGT      0
182500170807     C***                ADD       1             C1RCD
182600170807     C***                ELSE
182700170807     C***                SUB       SFLPAG        C1RCD
182800170807     C***                ADD       1             C1RCD
182900170807     C****               ENDIF
183000170807     C
183100170804     C     $EndFile      Ifeq      *on
183200170808     C     $RcdOk        Ifeq      *on
183300170804     C*
183400170804     C                   EXSR      RIES1
183500170804     C*
183600170804     C                   ADD       1             S1NRR
183700170804     C                   ADD       1             Y
183800170804     C*
183900170804     C                   WRITE     FMTS1
184000170804     C
184100170804     C                   ENDIF
184200170804     C                   Endif
184300170807     C*
184400170807     C                   Z-ADD     S1NRR         WMAX                 30
184500170807     C*
184600170807     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
184700170807     C*
184800170807     C                   IF        S1NRR > 1
184900170807     C     S1NRR         SUB       1             WNRR
185000170807     C                   ELSE
185100170807     C                   Z-ADD     S1NRR         WNRR
185200170807     C                   ENDIF
185300170807     C     WNRR          DIV       SFLPAG        PAGINE            4 0
185400170807     C                   MVR                     RESTO             3 0
185500170807     C     PAGINE        MULT      SFLPAG        C1RCD
185600170807     C     RESTO         IFGT      0
185700170807     C                   ADD       1             C1RCD
185800170807     C                   ELSE
185900170807     C                   SUB       SFLPAG        C1RCD
186000170807     C                   ADD       1             C1RCD
186100170807     C                   ENDIF
186200170807     C
186300000000     C*
186400000000     C                   ENDSR
186500000000     C/EJECT
186600000000     C************************************************************
186700000000     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
186800000000     C************************************************************
186900000000     C     SETANA        BEGSR
187000170727     C*
187100170728     C                   Clear                   DSCLIFOR
187200170721     C*
187300170721     C                   Eval      CSTSocieta   = XScSoc
187400170727     C                   Eval      CSTTpCom     = 'T'
187500170721     C                   Eval      CSTTpRec     = D1TpRec
187600170721     C                   Eval      CSTAnno      = D1Anno
187700170721     C                   Eval      CSTTrim      = D1Trim
187800170721     C                   Eval      CSTMese      = 0
187900000000     C*
188000170727     C                   Movel     *on           $PrimaVolta
188100170808     C                   Movel     *off          $RcdOk
188200170808     C                   Movel     *off          $SavRcdOk
188300000000     C*
188400000000     C                   SELECT
188500000000     C*
188600000000     C* Gestione posizionamento
188700000000     C* Se il campo W1POS è > *blank imposto il posizionamento con
188800000000     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
188900000000     C*
189000000000    >C* Ord.1 = chiave ordinamento 1
189100000000     C     $ORD          WHENEQ    1
189200000000     C     W1POS         ANDNE     *BLANK
189300170726    >C                   MOVEL     W1POS         CSTCOGRAG1
189400000000     C* Ord.99= Chiave univoca per la POSTA
189500000000     C* (quando viene gestita la chiave univoca NON deve essere gestito
189600000000     C*  il posizionamento C1POS/W1POS)
189700000000     C     $ORD          WHENEQ    99
189800000000     C                   ENDSL
189900000000     C*
190000170728     C                   Eval      CSTCliFor= DSCliFor
190100170728     C*
190200170808     C     K07CST02      Setll     SdgCST02l
190300170721     C*
190400000000     C                   ENDSR
190500000000     C/EJECT
190600000000     C************************************************************
190700000000     C* LETTURA RCD ARCHIVIO PILOTA
190800000000     C************************************************************
190900000000     C     REDANA        BEGSR
191000000000     C*
191100000000     C                   MOVEL     *OFF          $EndFile
191200170808     C***                MOVEL     *OFF          $RCDOK
191300170725     C                   MOVEL     *OFF          $ROTT01
191400000000     C*
191500000000     C     $EndFile      DOUEQ     *ON
191600170727     C***  $RcdOK        OREQ      *ON
191700170725     C     $Rott01       OREQ      *ON
191800170721     C*
191900170721     C                   SELECT
192000170721     C*
192100170721     C     $ORD          WHENEQ    1
192200000000     C*
192300170808     C     K06CST02      READE     SDGCST02L                              21
192400170721     C*
192500170721     C                   EndSL
192600170721     C*
192700170721     C                   MOVE      *IN21         $EndFile
192800000000     C*
192900000000     C     $EndFile      IFEQ      *OFF
193000170808     C*
193100170808     C                   If        $PrimaVolta = *on
193200170808     C*
193300170808     C                   Exsr      SavRott01
193400170808     C*
193500170808     C                   Clear                   $primavolta
193600170808     C*
193700170808     C                   Endif
193800170808     C*
193900170808     C* Verifico la rottura
194000170808     C                   Exsr      Rott01
194100170808     C*
194200170808     C                   EXSR      SELANA
194300170725     C*
194400170810     C                   If        $SavRcdOk=*on
194500170727     C* salvo i flag dell'elenco (devo farlo a dettaglio)
194600170808     C                   If        $Rott01 <> *on
194700170727     C                   Exsr      Savflag01
194800170808     C                   EndIf
194900170808     C                   EndIf
195000170725     C*
195100170725     C                   ENDIF
195200000000     C*
195300000000     C                   ENDDO
195400000000     C*
195500170804     C                   MOVE      $EndFile      *IN33                          SflEnd
195600000000     C*
195700000000     C                   ENDSR
195800170725     C/EJECT
195900170725     C************************************************************
196000170725     C* Salvo i flag dell'elenco da scrivere a rottura
196100170725     C************************************************************
196200170725     C     SavFlag01     BEGSR
196300170725     C*
196400170725     C                   If        CstCf = 'C'
196500170725     C                   Movel     *on           SavCliA
196600170725     C                   Endif
196700170725     C*
196800170725     C                   If        CstCf = 'F'
196900170725     C                   Movel     *on           SavForA
197000170725     C                   Endif
197100170725     C*
197200170725     C                   If        CstAnn= *on
197300170725     C                   Movel     *on           SavAnna
197400170725     C                   Endif
197500170725     C                   If        CstAnn<>*on
197600170725     C                   Movel     *on           SavNoAnn
197700170725     C                   Endif
197800170725     C*
197900170725     C                   ENDSR
198000170725     C/EJECT
198100170725     C************************************************************
198200170725     C* Verifico la rottura
198300170725     C************************************************************
198400170725     C     ROTT01        BEGSR
198500170725     C*
198600170803     C                   IF        SAVCLIFOR  <> DSCLIFOR
198700170808     C                   If        $RcdOk = *on
198800170725     C                   Movel     *on           $Rott01
198900170808     C                   Else
199000170808     C                   Exsr      savrott01
199100170808     C                   Endif
199200170808     C                   Endif
199300170725     C*
199400170725     C                   ENDSR
199500000000     C/EJECT
199600170725     C************************************************************
199700170725     C* Salvataggio campi di rottura
199800170725     C************************************************************
199900170725     C     SavRott01     BEGSR
200000170725     C*
200100170803     C                   Eval      SAVCliFor  = DSCliFor
200200170725     C                   Clear                   SAVANNA
200300170725     C                   Clear                   SAVNOANN
200400170725     C                   Clear                   SAVCLIA
200500170725     C                   Clear                   SAVFORA
200600170808     C                   Movel     $SavRcdOk     $RCDOK
200700170725     C*
200800170725     C                   ENDSR
200900170725     C/EJECT
201000000000     C************************************************************
201100000000     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
201200000000     C************************************************************
201300000000     C     CLCPAG        BEGSR
201400000000     C* Input :
201500000000     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
201600000000     C*          cursore
201700000000     C* - SFLPAG = numero rcd per pagina sfl
201800000000     C* Output :
201900000000     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
202000000000     C*
202100000000     C     WSFL          DIV       SFLPAG        PAGINE            4 0
202200000000     C                   MVR                     RESTO             3 0
202300000000     C     RESTO         IFGT      0
202400000000     C                   ADD       1             PAGINE
202500000000     C                   ENDIF
202600000000     C     PAGINE        MULT      SFLPAG        WPAG
202700000000     C*
202800000000     C                   ENDSR
202900000000     C/EJECT
203000000000     C************************************************************
203100000000     C* SELEZIONI SULLA RIGA LETTA
203200000000     C* impostare a *OFF il campo $RCDOK per scartare un record che
203300000000     C* non rientra nei parametri di parzializzazione
203400000000     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
203500000000     C* superiore stabilito nei parametri
203600000000     C*
203700000000     C************************************************************
203800000000     C     SELANA        BEGSR
203900000000     C*
204000170721     C                   MOVE      *hival        ALL9             75
204100170808     C*
204200170808     C                   Movel     *On           $SavRcdOk
204300170808     C*
204400170808     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
204500000000     C*
204600000000     C                   SELECT
204700170808     C*
204800170808     C                   When      D1CF       <> *Blank   and
204900170808     C                             CSTCF      <> D1CF
205000170808     C                   Eval      $SavRcdOK = *OFF
205100170808     C*
205200170808     C                   When      D1Stato1   <> *Blank   and
205300170808     C                             CSTStato1  <> D1Stato1
205400170808     C                   Eval      $SavRcdOK = *OFF
205500170808     C*
205600170808     C                   When      D1PartIVA  <> *Blank   and
205700170808     C                             CSTPartIVA <> D1PartIVA
205800170808     C                   Eval      $SavRcdOK = *OFF
205900170808     C*
206000170808     C                   When      D1CdFisc   <> *Blank   and
206100170808     C                             CSTCdFisc  <> D1CdFisc
206200170808     C                   Eval      $SavRcdOK = *OFF
206300170808     C*
206400170808     C                   When      D1Causale  <> *Blank   and
206500170808     C                             CSTCausale <> D1Causale
206600170808     C                   Eval      $SavRcdOK = *OFF
206700170808     C*
206800170810     C                   When      D1Sogg     <> *Blank   and
206900170808     C                             CSTSogg    <> D1Sogg
207000170808     C                   Eval      $SavRcdOK = *OFF
207100170810     C*
207200170810     C* Se richiamato dal catalogo
207300170810     C                   When      SavOpzI73 = '15' and
207400170810     C*
207500170810     C                             ((CstTpRec = 'O' and
207600170810     C                              (FileI73  <> CSTFileO or
207700170810     C                              IDFileI73 <> CSTIDFileO)) Or
207800170810     C*
207900170810     C                              (CstTpRec = 'R' and
208000170810     C                              (FileI73  <> CSTFileR or
208100170810     C                              IDFileI73 <> CSTIDFileR)) Or
208200170810     C*
208300170810     C                              (CstTpRec = 'A' and
208400170810     C                              (FileI73  <> CSTFileA or
208500170810     C                              IDFileI73 <> CSTIDFileA)))
208600170810     C*
208700170810     C                   Eval      $SavRcdOK = *OFF
208800170808     C*
208900170808     C                   ENDSL
209000170808     C*
209100170808     C                   if        $SavRcdOk = *On
209200170808     C*
209300170808     C                   SELECT
209400170808     C*
209500170808     C* Attivi + non attivi
209600170808     C                   When      D1Ann       = '1'
209700170808     C                   if        D1TpAnn     = *Blank       or
209800170808     C                             (D1TpAnn   <> *Blank   and
209900170808     C                              D1TpAnn    = CSTTpAnn    )
210000170808     C******--------------  OK
210100170808     C                   else
210200170808     C                   Eval      $SavRcdOk = *off
210300170808     C                   endif
210400170808     C*
210500170808     C* Attivi
210600170808     C                   When      D1Ann       = '2'
210700170808     C                   if        CSTAnn      = *Blank   and
210800170808     C                             CSTTpAnn    = *Blank
210900170808     C******--------------  OK
211000170808     C                   else
211100170808     C                   Eval      $SavRcdOk = *off
211200170808     C                   endif
211300170808     C*
211400170808     C* NON Attivi  (ANNULLATI)
211500170808     C                   When      D1Ann       = '3'
211600170808     C                   if        CSTAnn     <> *Blank           and
211700170808     C                             CSTTpAnn   <> *Blank           and
211800170808     C                             ((CSTTpAnn   = D1TpAnn  and
211900170808     C                               D1TpAnn   <> *Blank      )or
212000170808     C                               D1TpAnn    = *Blank         )
212100170808     C******--------------  OK
212200170808     C                   else
212300170808     C                   Eval      $SavRcdOk = *off
212400170808     C                   endif
212500170808     C*
212600170808     C                   ENDSL
212700000000     C*
212800170808     C                   endif
212900170808     C* Salvare se c'è almeno un record valido
213000170808     C                   IF        $SavRcdOk = *On
213100170808     C                   Eval      $RcdOk = $SavRcdOk
213200170808     C                   Endif
213300000000     C*
213400000000     C                   ENDSR
213500000000     C/EJECT
213600000000     C************************************************************
213700000000     C* RIEMPIMENTO RIGA LISTA
213800000000     C************************************************************
213900000000     C     RIES1         BEGSR
214000000000     C*
214100000000     C                   CLEAR                   FMTS1
214200000000     C*
214300000000     C* riempio i campi del sfl, compresi gli hidden con i valori
214400000000     C* del rcd letto
214500170804     C                   MoveL     SAVCf         H1Cf
214600170725     C                   MoveL     SAVPartiva    S1Partiva
214700170725     C                   MoveL     SAVCdFisc     S1CdFisc
214800170725     C                   MoveL     SAVCogRag1    S1CogRag1
214900170731     C                   MoveL     SAVStato1     H1Stato1
215000170725     C                   MoveL     SAVIDpaes1    H1IdPaes1
215100170725     C                   Movel     SAVPiva1      H1Piva1
215200170725     C                   Movel     SAVCdFisc1    H1CdFisc1
215300170728     C                   Movel     SAVTpn1       H1Tpn1
215400170728     C                   MoveL     SAVCogRag1    H1CogRag1
215500170803     C                   MoveL     SAVDenom1     H1Denom1
215600170803     C                   MoveL     SAVNome1      H1Nome1
215700170803     C                   MoveL     SAVCognom1    H1Cognom1
215800170725     C                   Movel     SAVIndir2     H1Indir2
215900170725     C                   Movel     SAVNrCiv2     H1NrCiv2
216000170725     C                   Movel     SAVCap2       H1Cap2
216100170725     C                   Movel     SAVLocali2    H1Locali2
216200170725     C                   Movel     SAVProv2      H1Prov2
216300170725     C                   Movel     SAVStato2     H1Stato2
216400170725     C                   Movel     SAVIdPaes2    H1IdPaes2
216500170725     C                   Movel     SAVIndir3     H1Indir3
216600170725     C                   Movel     SAVNrciv3     H1NrCiv3
216700170725     C                   Movel     SAVCap3       H1Cap3
216800170725     C                   Movel     SAVLocali3    H1Locali3
216900170725     C                   Movel     SAVProv3      H1Prov3
217000170727     C                   Movel     SAVStato3     H1Stato3
217100170731     C                   Movel     SAVIdPaes3    H1IdPaes3
217200170731     C                   Movel     SAVStato4     H1Stato4
217300170725     C                   Movel     SAVIdPaes4    H1IdPaes4
217400170725     C                   Movel     SAVPiva4      H1PIva4
217500170728     C                   Movel     SAVTpn4       H1Tpn4
217600170725     C                   Movel     SAVCograg4    H1Cograg4
217700170803     C                   Movel     SAVDenom4     H1Denom4
217800170725     C                   Movel     SAVNome4      H1Nome4
217900170803     C                   Movel     SAVCognom4    H1Cognom4
218000000000     C*
218100000000     C* azzero le memorizzazioni degli attributi non bottoni
218200170725     C                   MOVEL     SAVAnna       H1ANNA
218300170725     C                   MOVEL     SAVClia       H1CLIA
218400170725     C                   MOVEL     SAVFORa       H1FORA
218500170725     C*
218600170725     C                   If        SAVNOANN = *on and
218700170725     C                             SAVANNA  = *on
218800170725     C                   Movel     '*'           S1ANN
218900170725     C                   Endif
219000000000     C* imposto quegli attributi del sfl che sono modificabili
219100000000     C* al richiamo del determinato pgm
219200000000     C                   EXSR      ATRS1
219300000000     C*
219400000000     C                   ENDSR
219500000000     C/EJECT
219600000000     C************************************************************
219700000000     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
219800000000     C************************************************************
219900000000     C     ATRS1         BEGSR
220000000000     C*
220100000000     C* in questa routine bisogna attivare indicatori o campi programma
220200000000     C* che gestiscono gli attributi di uno o più campi di questa videata;
220300000000     C* attenzione : non è questo il posto in cui eseguire controlli,
220400000000     C*  dovrebbero già arrivare dei flag
220500000000     C*
220600000000     C* azzero gli attributi non bottoni
220700000000     C                   MOVEL     X'A0'         S1ANNA
220800000000     C*
220900000000     C     H1ANNA        IFEQ      *ON
221000170725     C                   MOVEL     RI            S1ANNA
221100000000     C                   ENDIF
221200170725     C*
221300170725     C     H1CLIA        IFEQ      *ON
221400170725     C                   MOVEL     RI            S1CLIA
221500170725     C                   ENDIF
221600170725     C*
221700170725     C     H1FORA        IFEQ      *ON
221800170725     C                   MOVEL     RI            S1FORA
221900170725     C                   ENDIF
222000000000     C*
222100000000     C                   ENDSR
222200000000     C/EJECT
222300000000     C************************************************************
222400000000     C* GESTIONE F08 VIDEO S1
222500000000     C************************************************************
222600000000     C     F08S1         BEGSR
222700000000     C*
222800000000     C                   MOVE      *ON           $FINE
222900170719    >C                   MOVE      '0'           RETI73
223000000000     C*
223100000000     C                   ENDSR
223200000000     C/EJECT
223300000000     C************************************************************
223400000000     C* GESTIONE F03 VIDEO S1
223500000000     C************************************************************
223600000000     C     F03S1         BEGSR
223700000000     C*
223800000000     C                   MOVE      *ON           $FINE
223900170719    >C                   MOVE      '1'           RETI73
224000000000     C* fine programma
224100000000     C                   EXSR      ENDPGM
224200000000     C*
224300000000     C                   ENDSR
224400000000     C/EJECT
224500000000     C************************************************************
224600000000     C* GESTIONE F10 VIDEO S1
224700000000     C* AGGIUNTA RECORD
224800000000     C************************************************************
224900000000     C     F10S1         BEGSR
225000000000     C*
225100170712     C                   RESET                   SDGSI74DS
225200170718     C                   MOVE      xscsoc        SocietaI74
225300170718     C                   MOVEL     '01'          OPZI74
225400170718     C     CMTI73        IFEQ      '1'
225500170718     C                   MOVEL     '1'           CMTI74
225600000000     C                   ELSE
225700170718     C                   MOVEL     '0'           CMTI74
225800000000     C                   ENDIF
225900170718     C                   Eval      AnnoI74     = D1Anno
226000170807     C                   Eval      TrimI74     = D1Trim
226100170718     C                   Eval      TpComI74     = 'T'
226200170718     C                   Eval      TpRecI74    = D1TpRec
226300170718     C*
226400170719     C                   Eval      SoggI74     = D1Sogg
226500170719     C                   Eval      AnnI74      = D1Ann
226600170712     C                   Eval      TpAnnI74    = D1TpAnn
226700000000     C* passare anche le parzializzazioni ricevute relative alla chiave
226800000000     C* univoca :
226900000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
227000170712    >C                   IF        OPZI73 <> *Blank
227100000000     C                   ENDIF
227200170712    >C                   CALL      'SDGSI74R'
227300000000     C                   PARM                    KPJBA
227400170712    >C                   PARM                    SDGSI74DS
227500000000     C* ritorno da PGM gestione
227600170718    >C                   EXSR      RTNI74
227700000000     C     WINZS1        IFEQ      *ON
227800000000     C                   MOVE      *ON           $INZS1
227900000000     C* se esistevano già righe sul sfl
228000000000     C* calcolo pagina a cui deve posizionarsi
228100000000     C     WSFL          IFGT      0
228200000000     C                   EXSR      CLCPAG
228300000000     C* altrimenti carico solo la 1a pagina
228400000000     C                   ELSE
228500000000     C                   Z-ADD     SFLPAG        WPAG
228600000000     C                   ENDIF
228700000000     C                   ENDIF
228800000000     C*
228900000000     C                   MOVE      *HIVAL        $LASTV
229000000000     C*
229100000000     C                   ENDSR
229200000000     C/EJECT
229300000000     C************************************************************
229400000000     C* GESTIONE F12 VIDEO S1
229500000000     C************************************************************
229600000000     C     F12S1         BEGSR
229700000000     C*
229800000000     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
229900000000     C     $LOP          IFEQ      1
230000000000     C                   MOVE      'D1'          $GEST
230100000000     C*  se la 1a videata è questa, per F12 torno a menu
230200000000     C                   ELSE
230300000000     C                   MOVE      *ON           $FINE
230400170718    >C                   MOVE      '2'           RETI73
230500000000     C                   ENDIF
230600000000     C*
230700000000     C                   ENDSR
230800000000     C/EJECT
230900000000     C************************************************************
231000000000     C* GESTIONE F17 VIDEO S1
231100000000     C************************************************************
231200000000     C     F17S1         BEGSR
231300000000     C*
231400000000     C                   MOVE      'D1'          $GEST
231500000000     C* se il pgm è stato richiamato con chiave univoca
231600000000     C* e dopo è stata richiesta una parzializzazione
231700000000     C* tolgo la gestione della chiave univoca (azzere/abblenco il
231800000000     C* valore; propongo come ordinamento il 1°)
231900000000     C     $Unique       IFEQ      *ON
232000000000     C                   MOVE      *OFF          $Unique
232100000000     C                   ENDIF
232200000000     C     $ORD          IFEQ      99
232300000000     C                   Z-ADD     1             $ORD
232400000000     C                   MOVE      *ON           $INZD1
232500000000     C                   ENDIF
232600000000     C*
232700000000     C                   ENDSR
232800000000     C/EJECT
232900000000     C************************************************************
233000000000     C* GESTIONE F18 VIDEO S1
233100000000     C************************************************************
233200000000     C     F18S1         BEGSR
233300000000     C*
233400000000     C                   MOVE      'D2'          $GEST
233500000000     C                   MOVEL     *ON           $INZD2
233600000000     C     $ORD          IFEQ      99
233700000000     C                   Z-ADD     1             $ORD
233800000000     C                   ENDIF
233900000000     C*
234000000000     C                   ENDSR
234100000000     C/EJECT
234200000000     C************************************************************
234300000000     C* GESTIONE F23 VIDEO S1
234400000000     C************************************************************
234500000000     C     F23S1         BEGSR
234600000000     C*
234700000000     C                   Z-ADD     $C1Op1        $L01
234800000000     C                   Z-ADD     $C1Op2        $L02
234900000000     C                   EXSR      RIEOPZ
235000000000     C                   Z-ADD     23            X
235100000000     C                   MOVE      FF23          $FC2(X)
235200000000     C                   MOVEL     WOPZ1         C1OPZ1
235300000000     C                   MOVEL     $ALLOPT       H1ALLOPT
235400000000     C                   MOVEL     WOPZ2         C1OPZ2
235500000000     C                   Z-ADD     WCOZ          C1COZ
235600000000     C*
235700000000     C                   ENDSR
235800000000     C/EJECT
235900000000     C************************************************************
236000000000     C* GESTIONE F24 VIDEO S1
236100000000     C************************************************************
236200000000     C     F24S1         BEGSR
236300000000     C*
236400000000     C                   MOVEA     $FX2          $FX
236500000000     C                   MOVEA     $FC2          $FC
236600000000     C                   MOVEA     $FR2          $FR
236700000000     C                   Z-ADD     $ULKS1        $ULK
236800000000     C                   Z-ADD     $S1L01        $L01
236900000000     C                   Z-ADD     $S1L02        $L02
237000000000     C                   EXSR      RIEKEY
237100000000     C                   Z-ADD     $ULK          $ULKS1            3 0
237200000000     C                   MOVEA     $FC           $FC2
237300000000     C                   MOVEL     $KEY1         Z2KE1
237400000000     C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
237500000000     C                   MOVEL     $KEY2         Z2KE2
237600000000     C                   WRITE     FMTZ2
237700000000     C*
237800000000     C                   ENDSR
237900000000     C/EJECT
238000000000     C************************************************************
238100000000     C* CONTROLLO TESTATA LISTA
238200000000     C************************************************************
238300000000     C     CTRC1         BEGSR
238400000000     C*
238500000000     C                   MOVE      *OFF          *IN99
238600000000     C*
238700000000     C     C1POS         IFNE      W1POS
238800000000     C                   EXSR      POSS1
238900000000     C                   ENDIF
239000000000     C*
239100000000     C                   ENDSR
239200000000     C/EJECT
239300000000     C************************************************************
239400000000     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
239500000000     C************************************************************
239600000000     C     POSS1         BEGSR
239700000000     C*
239800000000     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
239900000000     C*
240000000000   B1C     WMAX          IFGT      0
240100000000     C                   READC     FMTS1                                  21
240200000000   B2C     *IN21         DOWEQ     *OFF
240300000000     C     *IN99         ANDEQ     *OFF
240400000000     C*
240500000000   B3C     S1OPZ         IFNE      0
240600000000     C                   SETON                                        92  99
240700000000   E3C                   ENDIF
240800000000     C* se rilevata opzione attiva, attivo sflnxtchg
240900000000   B3C     *IN99         IFEQ      *ON
241000000000     C                   MOVE      *ON           *IN32
241100000000     C*
241200000000     C* imposto quegli attributi del sfl che sono modificabili
241300000000     C* al richiamo del determinato pgm
241400000000     C                   EXSR      ATRS1
241500000000     C*
241600000000     C                   UPDATE    FMTS1
241700000000   X3C                   ELSE
241800000000     C                   READC     FMTS1                                  21
241900000000   E3C                   ENDIF
242000000000     C*
242100000000   E2C                   ENDDO
242200000000     C*
242300000000   E1C                   ENDIF
242400000000     C*
242500000000     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
242600000000     C* AL VALORE DI POSIZIONAMENTO :
242700000000     C* se questo è maggiore del valore del rispettivo campo di
242800000000     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
242900000000     C* con i valori di parzializzazione.
243000000000     C*
243100000000   B1C     *IN99         IFEQ      *OFF
243200000000    >C                   MOVE      *BLANK        W1POS
243300000000     C*
243400000000     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
243500000000     C* è maggiore del relativo valore di parzializzazione, nel qual caso
243600000000     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
243700000000     C* richiedo comunque un ricaricamento del sfl che, se non si è
243800000000     C* verificata la condizione suddetta avverrà con posizionamento a
243900000000     C* partire dal valore di parzializzazione
244000000000     C*
244100000000   B2C                   SELECT
244200000000    >C* Attenzione : se un ordinamento ha un campo di posizionamento
244300000000    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
244400000000    >C* dei relativi campi
244500000000     C*
244600000000     C     $ORD          WHENEQ    1
244700170801    >C     C1POS         ANDGT     W1Pos
244800170801    >C                   MOVEL     C1POS         W1POS
244900000000     C*
245000000000     C* per l'ordinamento 99 = POSTA non viene gestito il posizionamento
245100000000     C*
245200000000   E2C                   ENDSL
245300000000     C*
245400000000     C* richiedo l'inizializzazione sfl
245500000000     C                   MOVE      *ON           $INZS1
245600000000     C*
245700000000   E1C                   ENDIF
245800000000     C*
245900000000     C                   ENDSR
246000000000     C/EJECT
246100000000     C************************************************************
246200000000     C* CONTROLLO OPZIONI LISTA
246300000000     C************************************************************
246400000000     C     CTRS1         BEGSR
246500000000     C*
246600000000     C                   MOVEL     *OFF          $ESCI
246700000000     C                   SETOFF                                       99
246800000000     C                   Z-ADD     0             S1OPZ
246900000000     C*
247000000000     C* Leggo il sfl solo se ci sono rcd
247100000000   B1C     WMAX          IFGT      0
247200000000     C                   READC     FMTS1                                  21
247300000000     C*
247400000000     C* esce se fine sfl o errore che richiede l'uscita
247500000000   B2C     *IN21         DOWEQ     *OFF
247600000000     C     $ESCI         ANDEQ     *OFF
247700000000     C                   Z-ADD     S1NRR         C1RCD
247800000000     C* ctrl su riga
247900000000     C                   EXSR      RECS1
248000000000     C* gestione opzioni
248100000000   B3C     S1OPZ         IFNE      0
248200000000     C     *IN99         ANDEQ     *OFF
248300000000     C                   EXSR      OPZS1
248400000000   E3C                   ENDIF
248500000000     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
248600000000   B3C     *IN99         IFEQ      *ON
248700000000     C                   MOVE      *ON           *IN32
248800000000   E3C                   ENDIF
248900000000     C*
249000000000     C* imposto quegli attributi del sfl che sono modificabili
249100000000     C* al richiamo del determinato pgm
249200000000     C                   EXSR      ATRS1
249300000000     C*
249400000000     C                   Z-ADD     0             S1OPZ
249500000000     C*
249600000000     C                   UPDATE    FMTS1
249700000000     C                   MOVE      *OFF          *IN32
249800000000     C*
249900000000     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
250000000000   B3C     $ESCI         IFEQ      *OFF
250100000000     C                   READC     FMTS1                                  21
250200000000     C* a fine ciclo ripristino il flag richiesta iniz.sfl
250300000000   B4C     *IN21         IFEQ      *ON
250400000000     C                   MOVE      WINZS1        $INZS1
250500000000     C                   MOVE      *off          WINZS1
250600000000     C* se è avvenuta almeno una modifica
250700000000   B5C                   IF        $INZS1 = *on
250800000000     C* calcolo pagina a cui deve posizionarsi
250900000000     C     WSFL          IFGT      0
251000000000     C                   EXSR      CLCPAG
251100000000     C                   ELSE
251200000000     C                   Z-ADD     SFLPAG        WPAG
251300000000     C                   ENDIF
251400000000   E5C                   ENDIF
251500000000   E4C                   ENDIF
251600000000   E3C                   ENDIF
251700000000     C*
251800000000   E2C                   ENDDO
251900000000     C*
252000000000   E1C                   ENDIF
252100000000     C*
252200000000     C                   ENDSR
252300000000     C/EJECT
252400000000     C************************************************************
252500000000     C* CONTROLLO CAMPI I/O RIGA LISTA
252600000000     C************************************************************
252700000000     C     RECS1         BEGSR
252800000000     C*
252900000000     C* reset indicatori DSPATR
253000000000     C                   MOVE      *ALL'0'       IN4049           10
253100000000     C                   MOVEA     IN4049        *IN(40)
253200000000     C*
253300000000     C* Controllo se Utente Autorizzato alla scelta
253400000000     C     S1OPZ         IFNE      0
253500000000     C                   Z-ADD     1             X
253600000000     C     S1OPZ         LOOKUP    $OP1(X)                                21
253700000000     C* mi salvo l'indice dell' opzione
253800000000     C   21              Z-add     X             PosOpz            2 0
253900000000     C     *IN21         IFEQ      *OFF
254000000000     C     $FL1(X)       OREQ      'N'
254100000000     C                   MOVE      *ON           $ESCI
254200000000     C                   SETON                                        409199
254300000000     C                   ENDIF
254400000000     C                   ENDIF
254500000000     C*
254600000000     C*
254700000000     C                   ENDSR
254800000000     C/EJECT
254900000000     C************************************************************
255000000000     C* GESTIONE OPZIONI LISTA
255100000000     C************************************************************
255200000000     C     OPZS1         BEGSR
255300000000     C*
255400000000     C                   Z-ADD     RIRI          CURR
255500000000     C                   Z-ADD     COCO          CURC
255600000000     C*
255700000000     C                   SELECT
255800000000     C*
255900000000     C* se opz=Selezione
256000000000     C* passo la chiave al pgm chiamante
256100000000     C     S1OPZ         WHENEQ    1
256200170718    >C                   MOVE      S1OPZ         OPZI73
256300170712     C* passare nei campi appositi della DS SDGSI73DS il valore
256400000000     C* della chiave della VL univoca (se c'è)
256500000000     C                   MOVE      *ON           $ESCI
256600000000     C                   MOVE      *ON           $FINE
256700000000     C*
256800000000     C                   MOVE      *HIVAL        $LASTV
256900000000     C*
257000000000     C* se opz=Dettaglio
257100000000     C     S1OPZ         WHENEQ    11
257200000000     C                   EXSR      CallListaDett
257300000000     C*
257400000000     C* se opz=Annulla/Ripristina
257500000000     C     S1OPZ         WHENEQ    44
257600130829     C                   EXSR      AnnRipr
257700000000     C*
257800000000     C                   MOVE      *HIVAL        $LASTV
257900000000     C*
258000000000     C* se selezionata opzione personalizzata, richiamo il programma
258100000000     C* corrispondente
258200000000     C     $PGO1(PosOpz) Whenne    *Blank
258300170712    >C                   RESET                   SDGSI74DS
258400170718    >C                   MOVE      xscsoc        SocietaI74
258500170718    >C                   MOVEL     S1OPZ         OPZI74
258600170718    >C     CMTI73        IFEQ      '1'
258700170718    >C                   MOVEL     '1'           CMTI74
258800000000     C                   ELSE
258900170718    >C                   MOVEL     '0'           CMTI74
259000000000     C                   ENDIF
259100170712     C* passare nei campi appositi della DS SDGSI74DS il valore
259200000000     C* della chiave della VL univoca
259300000000     C*
259400170805    >C**                 MOVEL     SDGSI74DS     KPJBU
259500000000     C                   CALL      $PGO1(PosOpz)                        22
259600000000     C                   PARM                    KPJBA
259700170805     C                   PARM                    SDGSI74DS
259800000000     C****               PARM                    NOMPGM
259900000000     C*
260000170805    >C****               MOVEL     KPJBU         SDGSI74DS
260100000000     C* ritorno da PGM
260200170718    >C                   EXSR      RTNI74
260300000000     C*
260400000000     C* se altre opzioni
260500000000     C* richiamo pgm gestione anagrafica
260600000000     C                   OTHER
260700000000     C*
260800170712     C                   RESET                   SDGSI74DS
260900170805     C                   Move      XscSoc        SocietaI74
261000000000     C*
261100170718     C                   MOVEL     S1OPZ         OPZI74
261200170718     C     CMTI73        IFEQ      '1'
261300170718     C                   MOVEL     '1'           CMTI74
261400000000     C                   ELSE
261500170718     C                   MOVEL     '0'           CMTI74
261600000000     C                   ENDIF
261700170719     C*
261800170719     C                   Eval      AnnoI74     = D1Anno
261900170805     C                   Eval      TrimI74     = D1Trim
262000170719     C*
262100170719     C                   Eval      TpComI74     = 'T'
262200170805     C                   Eval      TprecI74    = D1TpRec
262300170719     C*
262400170719     C                   Eval      AnnI74      = D1Ann
262500170719     C                   Eval      TpAnnI74    = D1TpAnn
262600170719     C                   Eval      CausaleI74  = D1Causale
262700170809     C                   Eval      CFInpI74    = H1CF
262800170805     C                   Eval      CFI74       = H1CF
262900170719     C                   Eval      PartIvaI74  = S1Partiva
263000170719     C                   Eval      CdFiscI74   = S1CdFisc
263100170719     C                   Eval      CogRag1I74  = S1CogRag1
263200170726     C                   Eval      CogRag1I74  = H1CogRag1
263300170805     C                   Eval      Tpn1I74     = H1TPN1
263400170719     C                   Eval      Nome1I74    = H1Nome1
263500170805     C                   Eval      Cognom1I74  = H1Cognom1
263600170805     C                   Eval      Denom1I74   = H1Denom1
263700170731     C                   Eval      Stato1I74   = H1Stato1
263800170719     C                   Eval      IdPaes1I74  = H1IdPaes1
263900170719     C                   Eval      Piva1I74    = H1Piva1
264000170719     C                   Eval      CdFisc1I74  = H1CdFisc1
264100170719     C                   Eval      Indir2I74   = H1Indir2
264200170719     C                   Eval      NrCiv2I74   = H1NrCiv2
264300170719     C                   Eval      Cap2I74     = H1Cap2
264400170727     C                   Eval      Locali2I74  = H1Locali2
264500170719     C                   Eval      Prov2I74    = H1Prov2
264600170719     C                   Eval      Stato2I74   = H1Stato2
264700170719     C                   Eval      IdPaes2I74  = H1IdPaes2
264800170719     C                   Eval      Indir3I74   = H1Indir3
264900170719     C                   Eval      Nrciv3I74   = H1NrCiv3
265000170719     C                   Eval      Cap3I74     = H1Cap3
265100170719     C                   Eval      Locali3I74  = H1Locali3
265200170719     C                   Eval      Prov3I74    = H1Prov3
265300170719     C                   Eval      Stato3I74   = H1Stato3
265400170731     C                   Eval      IdPaes3I74  = H1IdPaes3
265500170727     C*
265600170731     C                   Eval      Stato4I74   = H1Stato4
265700170719     C                   Eval      IdPaes4I74  = H1IdPaes4
265800170719     C                   Eval      Piva4I74    = H1PIva4
265900170719     C                   Eval      Cograg4I74  = H1Cograg4
266000170805     C                   Eval      Tpn4I74     = H1Tpn4
266100170805     C                   Eval      Denom4I74   = H1Denom4
266200170805     C                   Eval      Cognom4I74  = H1Cognom4
266300170719     C                   Eval      Nome4I74    = H1Nome4
266400000000     C*
266500170712     C                   CALL      'SDGSI74R'
266600000000     C                   PARM                    KPJBA
266700170712    >C                   PARM                    SDGSI74DS
266800000000     C*
266900000000     C*
267000000000     C* ritorno da PGM gestione
267100170719     C                   EXSR      RTNI74
267200000000     C*
267300000000     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
267400000000     C     *IN99         IFEQ      *ON
267500000000     C                   SETON                                        40
267600000000     C                   ENDIF
267700000000     C*
267800000000     C                   MOVE      *HIVAL        $LASTV
267900000000     C                   ENDSL
268000000000     C*
268100000000     C                   ENDSR
268200000000     C/EJECT
268300000000     C************************************************************
268400000000     C* Chiamata a pgm lista dettaglio
268500000000     C************************************************************
268600000000     C     CallListaDett BEGSR
268700000000     C*
268800170712    >C                   RESET                   SDGSI75DS
268900000000     C*
269000170810     C                   IF        OpzI73 = '05'
269100170810    >C                   Movel     SavOpzI73     OPZI75
269200170810     C                   ELse
269300170810    >C                   Movel     '11'          OPZI75
269400170810     C                   Endif
269500170810    >C                   Move      xscsoc        SocietaI75
269600170718     C                   Eval      AnnoI75     = D1Anno
269700170803     C                   Eval      TrimI75     = D1Trim
269800170803     C                   Eval      MeseI75     = 0
269900170808     C                   Eval      TpComI75    = 'T'
270000170803     C                   Eval      TpRecI75    = D1Tprec
270100170718     C*
270200170718     C                   Eval      AnnI75      = D1Ann
270300170718     C                   Eval      TpAnnI75    = D1TpAnn
270400170718     C                   Eval      TprecI75    = D1TpRec
270500170718     C                   Eval      SoggI75     = D1Sogg
270600170718     C                   Eval      CausaleI75  = D1Causale
270700170803     C*
270800170803     C                   Eval      CFI75       = H1CF
270900170718     C                   Eval      PartIvaI75  = S1Partiva
271000170718     C                   Eval      CdFiscI75   = S1CdFisc
271100170803     C                   Eval      Stato1I75   = H1Stato1
271200170803     C                   Eval      IdPaes1I75  = H1IdPaes1
271300170804     C                   Eval      CogRag1I75  = H1CogRag1
271400170803     C                   Eval      Piva1I75    = H1Piva1
271500170803     C                   Eval      CdFisc1I75  = H1CdFisc1
271600170718     C                   Eval      Nome1I75    = H1Nome1
271700170803     C                   Eval      TPN1I75     = H1Tpn1
271800170803     C                   Eval      Denom1I75   = H1Denom1
271900170803     C                   Eval      Cognom1I75  = H1Cognom1
272000170803     C                   Eval      Nome1I75    = H1Nome1
272100170803     C*
272200170718     C                   Eval      Indir2I75   = H1Indir2
272300170718     C                   Eval      NrCiv2I75   = H1NrCiv2
272400170718     C                   Eval      Cap2I75     = H1Cap2
272500170727     C                   Eval      Locali2I75  = H1Locali2
272600170718     C                   Eval      Prov2I75    = H1Prov2
272700170718     C                   Eval      Stato2I75   = H1Stato2
272800170718     C                   Eval      IdPaes2I75  = H1IdPaes2
272900170803     C*
273000170718     C                   Eval      Indir3I75   = H1Indir3
273100170718     C                   Eval      Nrciv3I75   = H1NrCiv3
273200170718     C                   Eval      Cap3I75     = H1Cap3
273300170718     C                   Eval      Locali3I75  = H1Locali3
273400170718     C                   Eval      Prov3I75    = H1Prov3
273500170718     C                   Eval      Stato3I75   = H1Stato3
273600170731     C                   Eval      IdPaes3I75  = H1IdPaes3
273700170803     C*
273800170731     C                   Eval      Stato4I75   = H1Stato4
273900170718     C                   Eval      IdPaes4I75  = H1IdPaes4
274000170718     C                   Eval      Piva4I75    = H1PIva4
274100170803     C                   Eval      Tpn4I75     = H1Tpn4
274200170719     C                   Eval      Cograg4I75  = H1Cograg4
274300170804     C                   Eval      Denom4I75   = H1Denom4
274400170803     C                   Eval      Cognom4I75  = H1Cognom4
274500170718     C                   Eval      Nome4I75    = H1Nome4
274600170810     C*
274700170810     C                   EVAL      FileI75    = FileI73
274800170810     C                   EVAL      IdFileI75  = IDFileI73
274900170718     C*
275000170712     C                   CALL      'SDGSI75R'
275100000000     C                   PARM                    KPJBA
275200170712     C                   PARM                    SDGSI75DS
275300000000     C*
275400170719     C                   EXSR      RTNI75
275500000000     C*
275600000000     C                   ENDSR
275700000000     C/EJECT
275800000000     C************************************************************
275900000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
276000000000     C************************************************************
276100170719    >C     RTNI74        BEGSR
276200000000     C*
276300000000     C* modo di ritorno
276400000000     C*
276500000000     C                   SELECT
276600000000     C* questi modi di utilizzo dei valori di ritorno dal
276700000000     C* pgm di manutenzione rcd di anagrafica sono delle
276800000000     C* proposte, normalmente sempre valide, ma modificabili
276900000000     C* per situazioni particolari
277000000000     C* 1 = F3
277100170719    >C     RETI74        WHENEQ    '1'
277200000000     C                   MOVE      *ON           $ESCI                          sospese READC
277300000000     C                   MOVE      *ON           $FINE                          fine pgm
277400170719    >C                   MOVE      '1'           RETI73
277500000000     C* 2 = F12
277600170719    >C     RETI74        WHENEQ    '2'
277700000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
277800170719     c* questo test, altrimenti prevale il retI74 e il sfl non viene
277900000000     C* ricaricato
278000170719    >C     OPRI74        ANDNE     '1'
278100000000     C*
278200000000     C* se l'opzione con cui è stato chiamato non è la modifica
278300000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
278400170719    >C     OPZI74        IFNE      '02'
278500000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
278600000000     C                   else
278700000000     C                   MOVE      *ON           $ESCI                          sospese READC
278800000000     C                   endif
278900000000     C*
279000000000     C                   ENDSL
279100000000     C*
279200000000     C* operazione eseguite dal pgm chiamato
279300000000     C*
279400000000     C                   SELECT
279500000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
279600170719    >C     OPRI74        WHENEQ    '1'
279700170719    >C                   MOVE      OPRI74        WOPR                           avverto pgm chiamant
279800000000     C*
279900170719     C                   if        OpzI74      = '01'  or
280000170719     C                             OpzI74      = '03'
280100000000     C                   EXSR      CallDett
280200000000     C                   endif
280300000000     C*
280400000000     C* se l'opzione con cui è stato chiamato non è la modifica
280500000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
280600170719    >C     OPZI74        IFNE      '02'
280700000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
280800000000     C* se l'opzione con cui è stato chiamato è la modifica
280900000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
281000000000     C                   ELSE
281100000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
281200000000     C                   ENDIF
281300000000     C*
281400000000     C                   ENDSL
281500000000     C*
281600000000     C* funzione non eseguibile per errore :
281700000000     C*
281800000000     C                   SELECT
281900000000     C* 1 = funzione richiamata chiusa in errore
282000170719    >C     ERRI74        WHENEQ    '1'
282100000000     C                   MOVE      *ON           $ESCI                          sospese READC
282200000000     C                   SETON                                        9399      emiss.SFLMSGID
282300000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
282400170719    >C     ERRI74        WHENEQ    '2'
282500000000     C                   MOVE      *ON           $ESCI                          sospese READC
282600000000     C                   SETON                                        9499      emiss.SFLMSGID
282700000000     C*
282800000000     C                   ENDSL
282900000000     C*
283000170719     C                   if        (OPZI74    = '01' and
283100170719    >C                              OPRI74    = '1'      )  or
283200170719     C                             (OPZI74    = '03' and
283300170719    >C                              OPRI74    = '1'      )
283400000000     C                   EXSR      CallListaDett2
283500000000     C                   endif
283600000000     C*
283700000000     C                   ENDSR
283800170805     C/EJECT
283900000000     C************************************************************
284000000000     C* Chiamata a pgm gestione dati di dettaglio
284100000000     C************************************************************
284200000000     C     CallDett      BEGSR
284300000000     C*
284400170712     C                   RESET                   SDGSI76DS
284500000000     C*
284600170719     C                   MOVE      xscsoc        SocietaI76
284700000000     C*
284800170719     C                   MOVEL     '02'          OPZI76
284900000000     C*
285000170719     C     CMTI73        IFEQ      '1'
285100170719     C                   MOVEL     '1'           CMTI76
285200000000     C                   ELSE
285300170719     C                   MOVEL     '0'           CMTI76
285400000000     C                   ENDIF
285500170719     C*
285600170719     C                   Eval      AnnoI76     = AnnoI74
285700170809     C                   Eval      TrimI76     = TrimI74
285800170809     C                   Eval      MeseI76     = MeseI74
285900170719     C                   Eval      TpComI76    = TpComI74
286000170809     C                   Eval      TprecI76    = TprecI74
286100170719     C*
286200170809     C                   Eval      AnnInpI76   = AnnI74
286300170809     C                   Eval      TpAInpI76   = TpAnnI74
286400170809     C                   Eval      CausInpI76  = CausaleI74
286500170809     C                   Eval      SogginpI76  = SoggI74
286600170809     C*
286700170809     C                   Eval      CfI76       = CfI74
286800170719     C                   Eval      PartIvaI76  = PartIvaI74
286900170803     C                   Eval      CdFiscI76   = CdFiscI74
287000170803     C                   Eval      Stato1I76   = Stato1I74
287100170803     C                   Eval      IdPaes1I76  = IdPaes1I74
287200170803     C                   Eval      Tpn1I76     = Tpn1I74
287300170803     C                   Eval      Piva1I76    = Piva1I74
287400170803     C                   Eval      CdFisc1I76  = CdFisc1I74
287500170719     C                   Eval      CogRag1I76  = CogRag1I74
287600170803     C                   Eval      Denom1I76   = Denom1I74
287700170719     C                   Eval      Nome1I76    = Nome1I74
287800170803     C                   Eval      Cognom1I76  = Cognom1I74
287900170809     C*
288000170719     C                   Eval      Indir2I76   = Indir2I74
288100170719     C                   Eval      NrCiv2I76   = NrCiv2I74
288200170719     C                   Eval      Cap2I76     = Cap2I74
288300170719     C                   Eval      Locali2I76  = Locali2I74
288400170719     C                   Eval      Prov2I76    = Prov2I74
288500170719     C                   Eval      Stato2I76   = Stato2I74
288600170719     C                   Eval      IdPaes2I76  = IdPaes2I74
288700170809     C*
288800170719     C                   Eval      Indir3I76   = Indir3I74
288900170719     C                   Eval      Nrciv3I76   = Nrciv3I74
289000170719     C                   Eval      Cap3I76     = Cap3I74
289100170719     C                   Eval      Locali3I76  = Locali3I74
289200170719     C                   Eval      Prov3I76    = Prov3I74
289300170719     C                   Eval      Stato3I76   = Stato3I74
289400170731     C                   Eval      IdPaes3I76  = IdPaes3I74
289500170809     C*
289600170731     C                   Eval      Stato4I76   = Stato4I74
289700170719     C                   Eval      IdPaes4I76  = IdPaes4I74
289800170719     C                   Eval      Piva4I76    = Piva4I74
289900170719     C                   Eval      Cograg4I76  = Cograg4I74
290000170719     C                   Eval      Nome4I76    = Nome4I74
290100170719     C*
290200170803     C                   Eval      NrrCSTI76   = NrrCSTI74
290300000000     C*
290400000000     C* passare anche le parzializzazioni ricevute relative alla chiave
290500000000     C* univoca :
290600000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
290700170719     C                   IF        OPZI73 <> *Blank
290800000000     C                   ENDIF
290900000000     C*
291000170712     C                   CALL      'SDGSI76R'
291100000000     C                   PARM                    KPJBA
291200170712     C                   PARM                    SDGSI76DS
291300000000     C*
291400000000     C* ritorno da PGM gestione
291500170719     C                   EXSR      RTNI76
291600000000     C*
291700000000     C                   ENDSR
291800000000     C/EJECT
291900000000     C************************************************************
292000000000     C* RITORNO DA PGM DI GESTIONE dati di dettaglio
292100000000     C************************************************************
292200170719    >C     RTNI76        BEGSR
292300000000     C*
292400000000     C* modo di ritorno
292500000000     C*
292600000000     C                   SELECT
292700000000     C* questi modi di utilizzo dei valori di ritorno dal
292800000000     C* pgm di manutenzione rcd di anagrafica sono delle
292900000000     C* proposte, normalmente sempre valide, ma modificabili
293000000000     C* per situazioni particolari
293100000000     C* 1 = F3
293200170719    >C     RETI76        WHENEQ    '1'
293300000000     C                   MOVE      *ON           $ESCI                          sospese READC
293400000000     C                   MOVE      *ON           $FINE                          fine pgm
293500170719    >C                   MOVE      '1'           RETI73
293600000000     C* 2 = F12
293700170719    >C     RETI76        WHENEQ    '2'
293800000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
293900170719     c* questo test, altrimenti prevale il retI74 e il sfl non viene
294000000000     C* ricaricato
294100170719    >C     OPRI76        ANDNE     '1'
294200000000     C                   MOVE      *ON           $ESCI                          sospese READC
294300170719    >C                   MOVE      '2'           RETI73
294400000000     C*
294500000000     C                   ENDSL
294600000000     C*
294700000000     C* operazione eseguite dal pgm chiamato
294800000000     C*
294900000000     C                   SELECT
295000000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
295100170719    >C     OPRI76        WHENEQ    '1'
295200170719    >C                   MOVE      OPRI76        WOPR                           avverto pgm chiamant
295300000000     C*
295400000000     C* se l'opzione con cui è stato chiamato non è la modifica
295500000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
295600130828    >C**** OPZG76        IFNE      '02'
295700000000     C****               MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
295800000000     C* se l'opzione con cui è stato chiamato è la modifica
295900000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
296000000000     C****               ELSE
296100000000     C****               EXSR      AGGS1
296200000000     C****               ENDIF
296300000000     C*
296400000000     C                   ENDSL
296500000000     C*
296600000000     C* funzione non eseguibile per errore :
296700000000     C*
296800000000     C                   SELECT
296900000000     C* 1 = funzione richiamata chiusa in errore
297000170719    >C     ERRI76        WHENEQ    '1'
297100000000     C                   MOVE      *ON           $ESCI                          sospese READC
297200000000     C                   SETON                                        9399      emiss.SFLMSGID
297300000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
297400170719    >C     ERRI76        WHENEQ    '2'
297500000000     C                   MOVE      *ON           $ESCI                          sospese READC
297600000000     C                   SETON                                        9499      emiss.SFLMSGID
297700000000     C*
297800000000     C                   ENDSL
297900000000     C*
298000000000     C                   ENDSR
298100000000     C/EJECT
298200000000     C************************************************************
298300000000     C* Chiamata a pgm gestione dati di dettaglio
298400000000     C************************************************************
298500000000     C     CallListaDett2BEGSR
298600000000     C*
298700170712     C                   RESET                   SDGSI75DS
298800000000     C*
298900170719     C                   MOVE      xscsoc        SocietaI75
299000000000     C*
299100170719     C                   MOVEL     '11'          OPZI75
299200000000     C*
299300170719     C     CMTI73        IFEQ      '1'
299400170719     C                   MOVEL     '1'           CMTI75
299500000000     C                   ELSE
299600170719     C                   MOVEL     '0'           CMTI75
299700000000     C                   ENDIF
299800110824     C*
299900170809     C                   Eval      AnnoI75     = D1Anno
300000170809     C                   Eval      TrimI75     = D1Trim
300100170803     C                   Eval      MeseI75     = 0
300200170808     C                   Eval      TpComI75    = 'T'
300300170809     C                   Eval      TpRecI75    = D1TPRec
300400170803     C*
300500170803     C                   Eval      AnnI75      = D1Ann
300600170804     C                   Eval      TpAnnI75    = D1TpAnn
300700170803     C                   Eval      CausaleI75  = D1Causale
300800170803     C*
300900170809     C                   Eval      SoggI75     = SoggI74
301000170809     C                   Eval      CFI75       = CFI74
301100170809     C                   Eval      PartIvaI75  = PartIvaI74
301200170809     C                   Eval      CdFiscI75   = CdFiscI74
301300170809     C                   Eval      Stato1I75   = Stato1I74
301400170809     C                   Eval      IdPaes1I75  = IdPaes1I74
301500170809     C                   Eval      Piva1I75    = Piva1I74
301600170809     C                   Eval      CdFisc1I75  = CdFisc1I74
301700170809     C                   Eval      TPN1I75     = TPN1I74
301800170809     C                   Eval      Cograg1I75  = Cograg1I74
301900170809     C                   Eval      Denom1I75   = Denom1I74
302000170809     C                   Eval      Cognom1I75  = Cognom1I74
302100170809     C                   Eval      Nome1I75    = Nome1I74
302200170809     C*
302300170809     C                   Eval      Indir2I75   = Indir2I74
302400170809     C                   Eval      NrCiv2I75   = NrCiv2I74
302500170809     C                   Eval      Cap2I75     = Cap2I74
302600170809     C                   Eval      Locali2I75  = Locali2I74
302700170809     C                   Eval      Prov2I75    = Prov2I74
302800170809     C                   Eval      Stato2I75   = Stato2I74
302900170809     C                   Eval      IdPaes2I75  = IdPaes2I74
303000170809     C*
303100170809     C                   Eval      Indir3I75   = Indir3I74
303200170809     C                   Eval      Nrciv3I75   = Nrciv3I74
303300170809     C                   Eval      Cap3I75     = Cap3I74
303400170809     C                   Eval      Locali3I75  = Locali3I74
303500170809     C                   Eval      Prov3I75    = Prov3I74
303600170809     C                   Eval      Stato3I75   = Stato3I74
303700170809     C                   Eval      IdPaes3I75  = IdPaes3I74
303800170803     C*
303900170809     C                   Eval      Stato4I75   = Stato4I74
304000170809     C                   Eval      IdPaes4I75  = IdPaes4I74
304100170809     C                   Eval      Piva4I75    = Piva4I74
304200170809     C                   Eval      Tpn4I75     = Tpn4I74
304300170809     C                   Eval      Cograg4I75  = Cograg4I74
304400170809     C                   Eval      Denom4I75   = Denom4I74
304500170809     C                   Eval      Cognom4I75  = Cognom4I74
304600170809     C                   Eval      Nome4I75    = Nome4I74
304700170809     C*
304800170809     C* passare anche le parzializzazioni ricevutazioni ricealla chiave
304900000000     C* univoca :
305000000000     C*  se ce ne sono, potranno essere usate come dft d'immissione
305100170719     C                   IF        OPZI73 <> *Blank
305200000000     C                   ENDIF
305300000000     C*
305400170712     C                   CALL      'SDGSI75R'
305500000000     C                   PARM                    KPJBA
305600170712     C                   PARM                    SDGSI75DS
305700000000     C*
305800000000     C* ritorno da PGM gestione
305900170719     C                   EXSR      RTNI75
306000000000     C*
306100000000     C                   ENDSR
306200000000     C/EJECT
306300000000     C************************************************************
306400000000     C* GESTIONE RITORNO DA PGM DI GESTIONE
306500000000     C************************************************************
306600170719    >C     RTNI75        BEGSR
306700000000     C*
306800000000     C* modo di ritorno
306900000000     C*
307000000000     C                   SELECT
307100000000     C* questi modi di utilizzo dei valori di ritorno dal
307200000000     C* pgm di manutenzione rcd di anagrafica sono delle
307300000000     C* proposte, normalmente sempre valide, ma modificabili
307400000000     C* per situazioni particolari
307500000000     C* 1 = F3
307600170719    >C     RETI75        WHENEQ    '1'
307700000000     C                   MOVE      *ON           $ESCI                          sospese READC
307800000000     C                   MOVE      *ON           $FINE                          fine pgm
307900170719    >C                   MOVE      '1'           RETI73
308000000000     C
308100000000     C* 2 = F12
308200170719    >C     RETI75        WHENEQ    '2'
308300000000     C* se il pgm di manutenzione ha prima una guida, è necessario anche
308400170719     c* questo test, altrimenti prevale il retI74 e il sfl non viene
308500000000     C* ricaricato
308600170719    >C     OPRI75        ANDNE     '1'
308700000000     C                   MOVE      *ON           $ESCI                          sospese READC
308800000000     C*
308900000000     C* se l'opzione con cui è stato chiamato non è la modifica
309000000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
309100170719    >C     OPZI75        IFNE      '02'
309200000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
309300000000     C                   else
309400000000     C                   MOVE      *ON           $ESCI                          sospese READC
309500000000     C                   endif
309600000000     C*
309700000000     C                   ENDSL
309800000000     C*
309900000000     C* operazione eseguite dal pgm chiamato
310000000000     C*
310100000000     C                   SELECT
310200000000     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
310300170719    >C     OPRI75        WHENEQ    '1'
310400170719    >C                   MOVE      OPRI75        WOPR                           avverto pgm chiamant
310500000000     C*
310600000000     C* se l'opzione con cui è stato chiamato non è la modifica
310700000000     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
310800170719    >C     OPZI75        IFNE      '02'
310900000000     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
311000000000     C* se l'opzione con cui è stato chiamato è la modifica
311100000000     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
311200000000     C                   ELSE
311300000000     C                   EXSR      AGGS1
311400000000     C                   ENDIF
311500000000     C*
311600000000     C                   ENDSL
311700000000     C*
311800000000     C* funzione non eseguibile per errore :
311900000000     C*
312000000000     C                   SELECT
312100000000     C* 1 = funzione richiamata chiusa in errore
312200170719    >C     ERRI75        WHENEQ    '1'
312300000000     C                   MOVE      *ON           $ESCI                          sospese READC
312400000000     C                   SETON                                        9399      emiss.SFLMSGID
312500000000     C* 2 = funzione richiamata senza la dovuta autorizzazione
312600170719    >C     ERRI75        WHENEQ    '2'
312700000000     C                   MOVE      *ON           $ESCI                          sospese READC
312800000000     C                   SETON                                        9499      emiss.SFLMSGID
312900000000     C*
313000000000     C                   ENDSL
313100000000     C*
313200000000     C                   ENDSR
313300000000     C/EJECT
313400000000     C************************************************************
313500000000     C* Annulla/Ripristina
313600000000     C************************************************************
313700130829     C     AnnRipr       BEGSR
313800000000     C*
313900170719     C                   RESET                   SDG00702DS
314000130829     C*
314100170719     C                   Eval      Soc0702      = XScSoc
314200110824     C*
314300110824     C* passo eventuali parzializzazioni sugli annullati
314400170719     C                   Move      D1Ann         Ann0702
314500170719     C                   Move      D1TpAnn       TpAnn0702
314600170719     C*
314700170719     C                   Eval      Trim0702     = D1Trim
314800170719     C                   Eval      TpCom0702    = 'T'
314900130829     C*
315000170719     C                   Eval      Anno0702     = D1Anno
315100170726     C                   Eval      Mese0702     = 0
315200170726     C                   Move      D1Tprec       Tprec0702
315300130829     C*
315400130829     C* applico eventuale filtro
315500170719     C*
315600170719     C                   Eval      Causal0702  = D1Causale
315700170719     C*
315800170719     C                   Eval      CF0702      = D1CF
315900170719     C*
316000170719     C                   Eval      Sogg0702    = D1Sogg
316100170719     C*
316200170726     C* Compongo il campo chiave
316300170726     C*
316400170728     C                   Eval      CliFor0702= DsCliForA
316500130912     C*
316600000000     C* passo lo stato attuale della testata
316700170719     C                   Eval      AnnAtt0702   = H1Anna
316800170810     C* passo l'opzione
316900170727     C                   MOVE      '44'          OPZ0702
317000000000     C*
317100170719     C                   CALLB     'SDG00702R'
317200000000     C                   PARM                    KPJBA
317300170719     C                   PARM                    SDG00702DS
317400000000     C*
317500000000     C                   move      *on           WOPR
317600000000     C                   move      *on           WINZS1
317700000000     C*
317800000000     C*
317900000000     C                   ENDSR
318000000000     C/EJECT
318100000000     C************************************************************
318200000000     C* AGGIORNAMENTO RIGA LISTA
318300000000     C************************************************************
318400000000     C     AGGS1         BEGSR
318500000000     C*
318600000000     C* riempio i campi del sfl che sono stati ripassati dal pgm
318700000000     C* di manutenzione dopo una opzione 02
318800000000     C*
318900170718     C                   Eval      S1Partiva   = PartivaI74
319000170718     C                   Eval      S1CdFisc    = CdFiscI74
319100170805     C                   Eval      H1Cf        = CFI74
319200170805     C                   If        CFI74 = 'C'
319300170805     C                   Movel     *on           H1Clia
319400170805     C                   Endif
319500170805     C                   If        CFI74 = 'F'
319600170805     C                   Movel     *on           H1Fora
319700170805     C                   Endif
319800170805     C*
319900170805     C                   Eval      H1Tpn1      = Tpn1I74
320000170727     C                   Movel(p)  CogRag1I74    S1Cograg1
320100170727     C                   Eval      H1CogRag1   = CogRag1I74
320200170805     C                   Eval      H1Denom1    = Denom1I74
320300170727     C                   Eval      H1Nome1     = Nome1I74
320400170805     C                   Eval      H1Cognom1   = Cognom1I74
320500170731     C                   Eval      H1Stato1    = Stato1I74
320600170727     C                   Eval      H1IdPaes1   = IdPaes1I74
320700170727     C                   Eval      H1Piva1     = Piva1I74
320800170727     C                   Eval      H1CdFisc1   = CdFisc1I74
320900170727     C                   Eval      H1Nome1     = Nome1I74
321000170727     C                   Eval      H1Indir2    = Indir2I74
321100170727     C                   Eval      H1NrCiv2    = NrCiv2I74
321200170727     C                   Eval      H1Cap2      = Cap2I74
321300170727     C                   Eval      H1Locali2   = Locali2I74
321400170727     C                   Eval      H1Prov2     = Prov2I74
321500170727     C                   Eval      H1Stato2    = Stato2I74
321600170727     C                   Eval      H1IdPaes2   = IdPaes2I74
321700170727     C                   Eval      H1Indir3    = Indir3I74
321800170727     C                   Eval      H1NrCiv3    = Nrciv3I74
321900170727     C                   Eval      H1Cap3      = Cap3I74
322000170727     C*
322100170727     C                   Eval      H1Locali3   = Locali3I74
322200170727     C                   Eval      H1Prov3     = Prov3I74
322300170727     C                   Eval      H1Stato3    = Stato3I74
322400170731     C                   Eval      H1IdPaes3   = IdPaes3I74
322500170731     C                   Eval      H1Stato4    = Stato4I74
322600170727     C                   Eval      H1IdPaes4   = IdPaes4I74
322700170727     C                   Eval      H1PIva4     = Piva4I74
322800170805     C                   Eval      H1Tpn4      = Tpn4I74
322900170727     C                   Eval      H1Cograg4   = Cograg4I74
323000170805     C                   Eval      H1Denom4    = Denom4I74
323100170805     C                   Eval      H1Nome4     = Nome4I74
323200170805     C                   Eval      H1Cognom4   = Cognom4I74
323300000000     C*
323400000000     C                   ENDSR
323500000000     C/EJECT
323600000000     C************************************************************
323700000000     C* GESTIONE VALORI ASSUNTI
323800000000     C************************************************************
323900000000     C     GESD2         BEGSR
324000000000     C*
324100000000   B1C     $INZD2        IFEQ      *ON
324200000000     C                   EXSR      INZD2
324300000000     C                   MOVE      *OFF          $INZD2
324400000000   E1C                   ENDIF
324500000000     C*
324600000000   B1C     $LASTV        IFNE      'D2'
324700000000     C                   EXSR      GESPIE
324800000000   B1C     $LASTV        IFNE      *HIVAL
324900000000     C                   MOVE      $LASTV        $LASTG
325000000000     C                   ENDIF
325100000000     C                   MOVE      'D2'          $LASTV
325200000000   E1C                   ENDIF
325300000000     C*
325400000000     C* se esistono errori sulla videata
325500000000     C* emetto la write del formato a indicatori spenti per vedere
325600000000     C* le eventuali decodifiche
325700000000   B1C     *IN99         IFEQ      *ON
325800000000     C                   MOVEA     *IN           WIN
325900000000     C                   MOVE      *ALL'0'       IN5098           49
326000000000     C                   MOVEA     IN5098        *IN(50)
326100000000     C                   WRITE     FMTD2
326200000000     C                   MOVEA     WIN           *IN
326300000000   E1C                   ENDIF
326400000000     C*
326500000000     C                   EXFMT     FMTD2
326600000000     C*
326700000000     C* azzero la posizione del cursore
326800000000     C                   Z-ADD     0             H1RIGA
326900000000     C                   Z-ADD     0             H1COLO
327000000000     C*
327100000000     C* CONTROLLO ABILITAZIONE TASTI
327200000000     C* INPUT
327300000000     C* $FC3 = Abilitazioni videata D2
327400000000     C* OUTPUT
327500000000     C* *IN97 + *IN99 Errori
327600000000     C                   MOVEA     $FC3          $FC
327700000000     C                   EXSR      CTRKEY
327800000000     C*
327900000000   B1C     *IN99         IFEQ      *OFF
328000000000     C*
328100000000   B2C                   SELECT
328200000000     C* F3=Fine
328300000000     C     $TASTO        WHENEQ    F03
328400000000     C                   EXSR      F03D2
328500000000     C* F12=Ritorno
328600000000     C     $TASTO        WHENEQ    F12
328700000000     C                   EXSR      F12D2
328800000000     C* F24=Altri tasti
328900000000     C     $TASTO        WHENEQ    F24
329000000000     C                   EXSR      F24D2
329100000000     C*
329200000000     C                   OTHER
329300000000     C                   EXSR      CTRD2
329400000000   B3C     *IN99         IFEQ      *OFF
329500000000     C                   MOVE      $LASTG        $GEST
329600000000   B4C                   SELECT
329700000000     C     $LASTG        WHENEQ    'S1'
329800000000     C* se è stato modificato l'ordinamento di default, ricarico la lista
329900000000   B5C     $ORD          IFNE      D2ORD
330000000000     C                   MOVE      *ON           $INZS1
330100000000     C                   MOVE      *BLANK        W1POS
330200000000   E5C                   ENDIF
330300000000     C     $LASTG        WHENEQ    'D1'
330400000000     C* per ora niente
330500000000   E4C                   ENDSL
330600000000     C* definisco i nuovi valori di default
330700000000     C                   Z-ADD     D2LOP         $LOP
330800000000     C                   Z-ADD     D2ORD         $ORD
330900000000     C*
331000000000     C* aggiorno valori assunti
331100000000     C* se richiesto il salvataggio
331200000000   B4C     D2BSV         IFEQ      1
331300000000     C                   EXSR      AGGMPE
331400000000   E4C                   ENDIF
331500000000     C*
331600000000   E3C                   ENDIF
331700000000     C*
331800000000   E2C                   ENDSL
331900000000     C*
332000000000   E1C                   ENDIF
332100000000     C*
332200000000     C                   ENDSR
332300000000     C/EJECT
332400000000     C************************************************************
332500000000     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
332600000000     C************************************************************
332700000000     C     INZD2         BEGSR
332800000000     C*
332900000000     C                   CLEAR                   FMTD2
333000000000     C                   EVAL      D2BSVA = Normale
333100000000     C* se sono entrato con chiave univoca
333200000000     C* impedisco il salvataggio valori assunti
333300000000     C     $Unique       IFEQ      *ON
333400000000     C                   EVAL      D2BSVA = Protetto
333500000000     C                   ENDIF
333600000000     C                   MOVEL     $LOP          D2LOP
333700000000     C                   MOVE      $ORD          D2ORD
333800000000     C*
333900000000     C                   ENDSR
334000000000     C/EJECT
334100000000     C************************************************************
334200000000     C* GESTIONE F03 VIDEO D2
334300000000     C************************************************************
334400000000     C     F03D2         BEGSR
334500000000     C*
334600000000     C                   MOVE      *ON           $FINE
334700000000     C* fine programma
334800000000     C                   EXSR      ENDPGM
334900000000     C*
335000000000     C                   ENDSR
335100000000     C/EJECT
335200000000     C************************************************************
335300000000     C* GESTIONE F12 VIDEO D2
335400000000     C************************************************************
335500000000     C     F12D2         BEGSR
335600000000     C*
335700000000     C                   MOVE      $LASTG        $GEST
335800000000     C*
335900000000     C                   ENDSR
336000000000     C/EJECT
336100000000     C************************************************************
336200000000     C* GESTIONE F24 VIDEO D2
336300000000     C************************************************************
336400000000     C     F24D2         BEGSR
336500000000     C*
336600000000     C                   MOVEA     $FX3          $FX
336700000000     C                   MOVEA     $FC3          $FC
336800000000     C                   MOVEA     $FR3          $FR
336900000000     C                   Z-ADD     $ULKD2        $ULK
337000000000     C                   Z-ADD     $D2L01        $L01
337100000000     C                   Z-ADD     $D2L02        $L02
337200000000     C                   EXSR      RIEKEY
337300000000     C                   Z-ADD     $ULK          $ULKD2            3 0
337400000000     C                   MOVEA     $FC           $FC3
337500000000     C                   MOVEL     $KEY1         Z3KE1
337600000000     C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
337700000000     C                   WRITE     FMTZ3
337800000000     C*
337900000000     C                   ENDSR
338000000000     C/EJECT
338100000000     C************************************************************
338200000000     C* CONTROLLO VIDEATA VALORI ASSUNTI
338300000000     C************************************************************
338400000000     C     CTRD2         BEGSR
338500000000     C*
338600000000     C                   SETOFF                                       99
338700000000     C*
338800000000     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
338900000000     C     D2LOP         IFEQ      0
339000000000     C                   SETON                                          5099
339100000000     C                   ENDIF
339200000000     C*
339300000000     C* OBBLIGATORIO VALORE PER ORDINAMENTO
339400000000     C     D2ORD         IFEQ      0
339500000000     C                   SETON                                          5199
339600000000     C                   ENDIF
339700000000     C*
339800000000     C                   ENDSR
339900000000     C/EJECT
340000000000     C************************************************************
340100000000     C* CONTROLLO ABILITAZIONE TASTI
340200000000     C* INPUT
340300000000     C* $TASTO =Tasto funzione premuto
340400000000     C* $FC = Tasti abilitati
340500000000     C* OUTPUT
340600000000     C* IN99 = ERrore
340700000000     C************************************************************
340800000000     C     CTRKEY        BEGSR
340900000000     C*
341000000000     C                   SETOFF                                       99
341100000000     C*
341200000000     C                   SELECT
341300000000     C     $TASTO        WHENEQ    F01
341400000000     C                   z-add     01            posfun
341500000000     C     $FC(1)        IFEQ      '0'
341600000000     C                   SETON                                        9997
341700000000     C                   ENDIF
341800000000     C     $TASTO        WHENEQ    F02
341900000000     C                   z-add     02            posfun
342000000000     C     $FC(2)        IFEQ      '0'
342100000000     C                   SETON                                        9997
342200000000     C                   ENDIF
342300000000     C     $TASTO        WHENEQ    F03
342400000000     C                   z-add     03            posfun
342500000000     C     $FC(3)        IFEQ      '0'
342600000000     C                   SETON                                        9997
342700000000     C                   ENDIF
342800000000     C     $TASTO        WHENEQ    F04
342900000000     C                   z-add     04            posfun
343000000000     C     $FC(4)        IFEQ      '0'
343100000000     C                   SETON                                        9997
343200000000     C                   ENDIF
343300000000     C     $TASTO        WHENEQ    F05
343400000000     C                   z-add     05            posfun
343500000000     C     $FC(5)        IFEQ      '0'
343600000000     C                   SETON                                        9997
343700000000     C                   ENDIF
343800000000     C     $TASTO        WHENEQ    F06
343900000000     C                   z-add     06            posfun
344000000000     C     $FC(6)        IFEQ      '0'
344100000000     C                   SETON                                        9997
344200000000     C                   ENDIF
344300000000     C     $TASTO        WHENEQ    F07
344400000000     C                   z-add     07            posfun
344500000000     C     $FC(7)        IFEQ      '0'
344600000000     C                   SETON                                        9997
344700000000     C                   ENDIF
344800000000     C     $TASTO        WHENEQ    F08
344900000000     C                   z-add     08            posfun
345000000000     C     $FC(8)        IFEQ      '0'
345100000000     C                   SETON                                        9997
345200000000     C                   ENDIF
345300000000     C     $TASTO        WHENEQ    F09
345400000000     C                   z-add     09            posfun
345500000000     C     $FC(09)       IFEQ      '0'
345600000000     C                   SETON                                        9997
345700000000     C                   ENDIF
345800000000     C     $TASTO        WHENEQ    F10
345900000000     C                   z-add     10            posfun
346000000000     C     $FC(10)       IFEQ      '0'
346100000000     C                   SETON                                        9997
346200000000     C                   ENDIF
346300000000     C     $TASTO        WHENEQ    F11
346400000000     C                   z-add     11            posfun
346500000000     C     $FC(11)       IFEQ      '0'
346600000000     C                   SETON                                        9997
346700000000     C                   ENDIF
346800000000     C     $TASTO        WHENEQ    F12
346900000000     C                   z-add     12            posfun
347000000000     C     $FC(12)       IFEQ      '0'
347100000000     C                   SETON                                        9997
347200000000     C                   ENDIF
347300000000     C     $TASTO        WHENEQ    F13
347400000000     C                   z-add     13            posfun
347500000000     C     $FC(13)       IFEQ      '0'
347600000000     C                   SETON                                        9997
347700000000     C                   ENDIF
347800000000     C     $TASTO        WHENEQ    F14
347900000000     C                   z-add     14            posfun
348000000000     C     $FC(14)       IFEQ      '0'
348100000000     C                   SETON                                        9997
348200000000     C                   ENDIF
348300000000     C     $TASTO        WHENEQ    F15
348400000000     C                   z-add     15            posfun
348500000000     C     $FC(15)       IFEQ      '0'
348600000000     C                   SETON                                        9997
348700000000     C                   ENDIF
348800000000     C     $TASTO        WHENEQ    F16
348900000000     C                   z-add     16            posfun
349000000000     C     $FC(16)       IFEQ      '0'
349100000000     C                   SETON                                        9997
349200000000     C                   ENDIF
349300000000     C     $TASTO        WHENEQ    F17
349400000000     C                   z-add     17            posfun
349500000000     C     $FC(17)       IFEQ      '0'
349600000000     C                   SETON                                        9997
349700000000     C                   ENDIF
349800000000     C     $TASTO        WHENEQ    F18
349900000000     C                   z-add     18            posfun
350000000000     C     $FC(18)       IFEQ      '0'
350100000000     C                   SETON                                        9997
350200000000     C                   ENDIF
350300000000     C     $TASTO        WHENEQ    F19
350400000000     C                   z-add     19            posfun
350500000000     C     $FC(19)       IFEQ      '0'
350600000000     C                   SETON                                        9997
350700000000     C                   ENDIF
350800000000     C     $TASTO        WHENEQ    F20
350900000000     C                   z-add     20            posfun
351000000000     C     $FC(20)       IFEQ      '0'
351100000000     C                   SETON                                        9997
351200000000     C                   ENDIF
351300000000     C     $TASTO        WHENEQ    F21
351400000000     C                   z-add     21            posfun
351500000000     C     $FC(21)       IFEQ      '0'
351600000000     C                   SETON                                        9997
351700000000     C                   ENDIF
351800000000     C     $TASTO        WHENEQ    F22
351900000000     C                   z-add     22            posfun
352000000000     C     $FC(22)       IFEQ      '0'
352100000000     C                   SETON                                        9997
352200000000     C                   ENDIF
352300000000     C     $TASTO        WHENEQ    F23
352400000000     C                   z-add     23            posfun
352500000000     C     $FC(23)       IFEQ      '0'
352600000000     C                   SETON                                        9997
352700000000     C                   ENDIF
352800000000     C     $TASTO        WHENEQ    F24
352900000000     C                   z-add     24            posfun
353000000000     C     $FC(24)       IFEQ      '0'
353100000000     C                   SETON                                        9997
353200000000     C                   ENDIF
353300000000     C                   ENDSL
353400000000     C*
353500000000     C                   ENDSR
353600000000     C/EJECT
353700000000     C************************************************************
353800000000     C* AGGIORNAMENTO VALORI ASSUNTI
353900000000     C************************************************************
354000000000     C     AGGMPE        BEGSR
354100000000     C*
354200000000     C* preparo i parametri di chiamata al driver
354300000000     C                   CLEAR                   XMPEDS
354400000000     C                   EVAL      TpChiamata = '03'
354500000000     C                   EVAL      NChiavi = 5
354600000000     C                   EVAL      VLogica = 2
354700000000     C*
354800000000     C* preparo le chiavi per la chiamata al driver
354900000000     C                   CLEAR                   XMPEDS1
355000000000     C                   MOVEL     XSCSOC        DSMPESOC
355100000000     C                   MOVEL     DSPGM         DSMPEPGM
355200000000     C                   MOVEL     KNMUS         DSMPEPRF
355300000000     C*
355400000000     C* Si memorizzano i dati di default a seconda delle modalità con
355500000000     C* cui il pgm è usato
355600000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
355700000000     C* MPETIP = 'PV'  pgm richiamato da menù
355800000000     C* MPETIP = 'PB'  x lancio Batch
355900000000     C*
356000000000     C                   SELECT
356100170719    >C     OPZI73        WHENEQ    '01'
356200000000     C                   MOVEL     'PX'          DSMPETIP
356300000000     C                   OTHER
356400000000     C                   MOVEL     'PV'          DSMPETIP
356500000000     C                   ENDSL
356600000000     C* dati
356700000000     C                   MOVEL     DSMPE         DSMPEPAR
356800000000     C*
356900000000     C                   CALLB     'XMPE'
357000000000     C                   PARM                    XMPEDS
357100000000     C                   PARM                    XMPEDS1
357200000000     C*
357300000000     C                   ENDSR
357400000000     C/EJECT
357500000000     C************************************************************
357600000000     C* OPERAZIONI INIZIALI
357700000000     C************************************************************
357800000000     C     *INZSR        BEGSR
357900000000     C*
358000000000     C* Reperimento parametri
358100000000     C*
358200000000     C     *ENTRY        PLIST
358300000000     C                   PARM                    KPJBA
358400000000     C                   PARM                    DSAUT
358500000000     C*
358600000000     C* testo se sono stati passati i dati di autorizzazione
358700000000     C*
358800000000     C     PARMS         IFLT      2
358900000000     C                   MOVE      *BLANK        AUTDS
359000000000     C                   ELSE
359100000000     C                   MOVEL     DSAUT         AUTDS
359200000000     C                   ENDIF
359300000000     C*
359400000000     C* Reperimento dati società
359500000000     C*
359600000000     C                   MOVEL     'SOC001'      TIPXSC
359700000000     C                   MOVEL     *BLANK        SOCXSC
359800000000     C                   EXSR      REPSOC
359900000000     C*
360000000000     C* Controlli dati società
360100000000     C*
360200000000     C     RTNXSC        IFNE      '1'
360300000000     C                   MOVEL     XSOCDS        SOC001
360400000000     C                   ELSE
360500000000     C                   MOVE      *ON           $FINE
360600170718    >C                   MOVE      '1'           ErrI73
360700000000     C                   EXSR      ENDPGM
360800000000     C                   ENDIF
360900000000     C*
361000000000     C* reperisco le dimensioni delle variabili che conterranno i tasti
361100000000     C                   EVAL      $D1L01 = %Size(Z1Ke1)
361200000000     C                   EVAL      $D1L02 = %Size(Z1Ke2)
361300000000     C                   EVAL      $S1L01 = %Size(Z2Ke1)
361400000000     C                   EVAL      $S1L02 = %Size(Z2Ke2)
361500000000     C                   EVAL      $C1Op1 = %Size(C1Opz1)
361600000000     C                   EVAL      $C1Op2 = %Size(C1Opz2)
361700000000     C                   EVAL      $D2L01 = %Size(Z3Ke1)
361800000000     C                   EVAL      $D2L02 = 0
361900000000     C*
362000000000     C* Reperimento opzioni
362100000000     C*
362200000000     C                   EXSR      REPOPZ
362300000000     C*
362400000000     C* Reperimento tasti di funzione
362500000000     C*
362600000000     C                   EXSR      REPKEY
362700000000     C* Chiamata a routine per il reperimento delle opzioni
362800000000     C* personalizzate
362900000000    >C* se si vogliono abilitare i tasti personalizzati in ogni videata
363000000000    >C* ripeterla passando le schiere dei tasti funzione
363100000000     C                   CAllb     'X46USREXT'
363200000000     C                   PARM                    $OP1
363300000000     C                   PARM                    $TX1
363400000000     C                   PARM                    $FL1
363500000000     C                   PARM                    $PGO1
363600000000     C                   PARM      $dimOp        NumOpz
363700000000     C                   PARM                    $FX2
363800000000     C                   PARM                    $FC2
363900000000     C                   PARM                    $PGF2
364000000000     C                   PARM                    DsPgm
364100000000     C                   PARM                    xsccut
364200000000     C                   PARM                    xscgrp
364300000000     C*
364400000000     C                   Z-add     1             OpzLun
364500000000     C     *blank        Lookup    $TX1(OpzLun)                           21
364600000000     C                   if        *in21 = *on
364700000000     C                   sub       1             OpzLun
364800000000     C                   else
364900000000     C                   Z-add     $DimOp        OpzLun
365000000000     C                   endif
365100000000     C*
365200000000     C                   ENDSR
365300000000     C/EJECT
365400000000     C************************************************************
365500000000     C* INIZIALIZZAZIONE VARIABILI
365600000000     C************************************************************
365700000000     C     INZVAR        BEGSR
365800000000     C*
365900000000     C* Dati da PGM chiamante
366000000000     C*
366100170712    >C                   MOVEL     KPJBU         SDGSI73DS
366200170810     C*
366300170810     C* Salvo se presente l'opzione Passata
366400170810     C                   Movel     OPZI73        SAVOpzI73
366500170810     C*
366600170810     C                   IF        OPZI73 = '15'
366700170810     C                   Movel     '05'          OPZI73
366800170810     C                   ENDIF
366900000000     C*
367000000000     C* Se società diversa da quella impostata nella *inzsr, rieseguo
367100000000     C* reperimento dati società
367200170719    >C                   IF        SocietaI73 <> xscsoc  and
367300170719    >C                             OpzI73 <>*blank
367400000000     C                   MOVEL     'SOC001'      TIPXSC
367500170719    >C                   MOVEL     SocietaI73    SOCXSC
367600000000     C                   EXSR      REPSOC
367700000000     C     RTNXSC        IFNE      '1'
367800000000     C                   MOVEL     XSOCDS        SOC001
367900000000     C                   ELSE
368000000000     C                   MOVE      *ON           $FINE
368100170719    >C                   MOVE      '1'           ErrI73
368200000000     C                   EXSR      ENDPGM
368300000000     C                   ENDIF
368400000000     C                   ENDIF
368500000000     C*
368600170809     C*
368700170705     C* Reperisco i dati di setup del modulo Spesometro Trimestrale
368800170705     C                   EXSR      RepSDSPETRI
368900000000     C*
369000000000     C* Reperimento autorizzazioni
369100000000     C*
369200000000     C* le autorizzazioni potrebbero essere già state passate,
369300000000     C* nel qual caso non vanno reperite
369400000000    >C                   EXSR      REPAUT
369500000000     C*
369600000000     C*
369700000000     C* Reperimento valori default utente
369800000000     C*
369900000000     C                   EXSR      REPDFT
370000000000     C*
370100000000     C* Definizione delle opzioni utilizzabili a pgm
370200000000     C*
370300000000     C                   EXSR      DEFOPZ
370400000000     C*
370500000000     C* Variabili per gestione videate
370600000000     C*
370700000000     C                   MOVE      *OFF          $FINE                          fine pgm
370800000000     C                   MOVE      *OFF          $INZD1                         inizializzione D1
370900000000     C                   MOVE      *OFF          $INZS1                         inizializzione S1
371000000000     C                   MOVE      *OFF          $INZD2                         inizializzione D2
371100000000     C                   MOVE      *OFF          $EndFile                       memo fine file
371200000000     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
371300000000     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
371400000000     C                   MOVE      *HIVAL        $LASTV
371500000000     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
371600000000     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
371700000000     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
371800000000     C* azzero la posizione del cursore
371900000000     C                   Z-ADD     0             H1RIGA
372000000000     C                   Z-ADD     0             H1COLO
372100000000     C*
372200000000     C                   MOVE      *ON           $INZD1
372300000000     C                   MOVE      *ON           $INZS1
372400000000     C*
372500000000     C* se pgm richiamato con chiave univoca impostata,
372600000000     C* forzo la emissione della lista e imposto l'ordinamento
372700000000    >C*  in questo esempio la chiave univoca per la posta è Codice
372800000000    >C*  ma qualora ci fosse più di un campo basterebbe testare quello più
372900000000    >C*  significativo (o meglio, quello che si sa essere obbligatorio
373000000000    >C*  ricevere pieno)
373100000000     C                   MOVE      *OFF          $Unique
373200000000     C*
373300000000     C* Variabili appoggio
373400000000     C*
373500000000     C                   Z-ADD     SFLPAG        WPAG
373600000000     C                   MOVEL     *OFF          WOPR
373700000000     C                   CLEAR                   W1POS
373800000000     C*
373900000000     C* definisco qual'è la prima videata
374000000000     C* (se la prima videata risulta la lista, devo comunque caricare
374100000000     C*  la videata di parzializzazione e gestire la definizione
374200000000     C*  dell'ordinamento, perchè, dai default oppure se sono stati
374300000000     C*  passati dei dati, può variare il caricamento)
374400000000     C                   SELECT
374500000000     C     $LOP          WHENEQ    1
374600000000     C                   MOVE      'D1'          $GEST
374700000000     C     $LOP          WHENEQ    2
374800000000     C                   MOVE      'S1'          $GEST
374900000000     C                   EXSR      INZD1
375000000000     C                   MOVE      *OFF          $INZD1
375100000000     C                   EXSR      DEFORD
375200000000     C                   ENDSL
375300000000     C*
375400000000     C* Valorizzazione campi univoci testate
375500000000     C*
375600000000     C                   CLEAR                   FMTT1
375700000000     C                   MOVEL     KNSIF         NOMSIF
375800000000     C                   MOVEL     XSCDSI        NOMDIT
375900000000     C                   MOVEL     KCDAZ         NOMAZN
376000000000     C****               MOVEL     DSPGM         NOMPGM
376100000000     C* recupero il sottotitolo (se c'è)
376200000000     C                   MOVEL     *BLANK        T1TIT
376300140401     C*
376400140401     C     SOTTIT        IFNE      *BLANK
376500140401     C                   MOVEL     SOTTIT        IDMSG
376600000000     C                   CALLB     'XRTVM'
376700000000     C                   PARM                    IDMSG            27
376800000000     C                   PARM                    TXTMSG          100
376900000000     C                   PARM                    ERRMSG            1
377000000000     C                   PARM      *ON           CTRMSG            1            centratura
377100000000     C                   PARM      30            LENMSG            3 0          lun output.
377200000000     C     ERRMSG        IFNE      *ON
377300000000     C                   MOVEL     TXTMSG        T1TIT
377400000000     C                   ELSE
377500000000     C                   MOVEL     *ALL'?'       T1TIT
377600000000     C                   ENDIF
377700140401     C                   ENDIF
377800000000     C*
377900000000     C                   ENDSR
378000000000     C/EJECT
378100000000     C************************************************************
378200000000     C* REPERIMENTO DATI SOCIETA'
378300000000     C************************************************************
378400000000     C     REPSOC        BEGSR
378500000000     C*
378600000000     C                   CALL      'SDGSOCDR'
378700000000     C                   PARM                    TIPXSC            6
378800000000     C                   PARM                    SOCXSC            3
378900000000     C                   PARM                    CDSXSC            9 0
379000000000     C                   PARM                    MODXSC            3
379100000000     C                   PARM      *BLANKS       RTNXSC            1
379200000000     C                   PARM                    XSOCDS
379300000000     C                   PARM                    KPJBA
379400000000     C*
379500000000     C                   ENDSR
379600000000     C/EJECT
379700000000     C************************************************************
379800170705     C* Rerimento parametri di setup per Spesometro Trimestrale
379900000000     C************************************************************
380000170705     C     RepSDSPETRI   BEGSR
380100000000     C*
380200170705     C                   Clear                   SDGPA12DS
380300000000     C*
380400000000     C                   CALLB     'XPAR'
380500000000     C                   Parm                    XScSoc
380600170705     C                   Parm      'SDSPETRI'    XPaPar            8
380700000000     C                   Parm                    XPaOut         2000
380800000000     C                   Parm      *OFF          XPaErr            1
380900000000     C                   Parm      ' '           XPaRic            1
381000000000     C*
381100000000     C                   if        XPaErr      = *On
381200000000     C                   EXSR      MsgErr2
381300000000     C                   Eval      $Fine       = *On
381400000000     C                   EXSR      ENDPGM
381500000000     C*
381600000000     C                   else
381700170705     C                   MoveL     XPaOut        SDGPA12DS
381800000000     C*
381900170719     C                   if        AttCSTA12   <> *On
382000000000     C                   EXSR      ENDPGM
382100000000     C                   endif
382200000000     C*
382300000000     C                   endif
382400000000     C*
382500000000     C                   ENDSR
382600000000     C/EJECT
382700000000     C************************************************************
382800000000     C* Messaggio errore 2
382900000000     C************************************************************
383000000000     C     MsgErr2       BEGSR
383100000000     C*
383200000000     C                   Reset                   SDGMSGDS
383300000000     C*
383400000000     C                   MoveL     'S'           MSGEMV
383500000000     C                   Z-Add     10            MSGRGP
383600000000     C                   MoveL     'N'           MSGRSP
383700000000     C                   MoveL     '  '          MSGTPR
383800000000     C*
383900000000     C                   MoveL     XPaPar        $P1PRO2184
384000000000     C*
384100000000     C                   call      'SDGMSGR'
384200000000     C                   parm                    SDGMSGDS
384300000000     C                   parm                    XMsgTxt         135
384400000000     C*
384500000000     C* Modo di inizializzazione della window:
384600000000     C* ('1'->Close + Open  -->I° volta dopo l'emissione del video princip.)
384700000000     C* ('0'->rimane aperto -->tutte le emissioni successive se consecutive,
384800000000     C*                       per mantenere il sottofondo del video princip.)
384900000000     C                   parm      '1'           XMsgInz           1
385000000000     C*
385100000000     C* PRO2184  - Fallito reperimento recupero parametro modulo &1
385200000000     C                   parm      'PRO2184'     XMsgCd1           7
385300000000     C                   parm                    XMsgCd2           7
385400000000     C                   parm                    XMsgCd3           7
385500000000     C                   parm                    XMsgSn1           7
385600000000     C                   parm                    XMsgCdTxT        63
385700000000     C                   parm                    MsgCdTit         14
385800000000     C                   parm                    MsgClPrtf         1
385900000000     C                   parm      $P1PRO2184    VarMsg1         100
386000000000     C                   parm                    VarMsg2         100
386100000000     C                   parm                    VarMsg3         100
386200000000     C*
386300000000     C                   ENDSR
386400000000     C/EJECT
386500170809     C************************************************************
386600170809     C* Reperimento parametri di setup per Spesometro trimestrale
386700170809     C************************************************************
386800170809     C     RepSPETRI     BEGSR
386900160321     C*
387000170719     C                   Clear                   SDGPA12DS
387100160321     C*
387200160321     C                   CALLB     'XPAR'
387300160321     C                   Parm                    XScSoc
387400170719     C                   Parm      'SDSPETRI'    XPaPar            8
387500160321     C                   Parm                    XPaOut         2000
387600160321     C                   Parm      *OFF          XPaErr            1
387700160321     C                   Parm      ' '           XPaRic            1
387800160321     C*
387900160321     C                   if        XPaErr      = *On
388000160321     C                   EXSR      MsgErr2
388100160321     C                   Eval      $Fine       = *On
388200160321     C                   EXSR      ENDPGM
388300160321     C*
388400160321     C                   else
388500170719     C                   MoveL     XPaOut        SDGPA12DS
388600160321     C*
388700170719     C                   if        AttCSTA12   <> *On
388800160321     C                   EXSR      ENDPGM
388900160321     C                   endif
389000160321     C*
389100160321     C                   endif
389200160321     C*
389300170809     C                   ENDSR
389400170809     C/EJECT
389500000000     C************************************************************
389600000000     C* REPERIMENTO AUTORIZZAZIONI UTENTE
389700000000     C************************************************************
389800000000     C     REPAUT        BEGSR
389900000000     C*
390000000000     C                   CLEAR                   AUTDS
390100000000     C                   Clear                   ConCivDs
390200000000     C*
390300000000     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
390400000000     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
390500000000     C                   Z-ADD     0             XCAGRP                         GRUPPO
390600000000     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
390700000000    >C                   MOVEL     'CONCIV'      XCAFNC                         FUNZIONE
390800000000     C                   MOVEL     *ZERO         XCAVFU
390900000000    >C                   MOVE      'CK'          XCATCK                         TIPO CTRL
391000000000     C                   MOVE      '0'           XCANAU                         RICH.XNOAUT
391100000000     C*
391200000000     C                   CALLB     'XCHKAUT'
391300000000     C                   PARM                    AUTDS
391400000000     C*
391500000000     C* errori/non abilitazione
391600000000     C     XCARTN        IFNE      0
391700000000     C*
391800000000     C                   ELSE
391900000000    >C                   MOVEL     XCADDS        CONCIVDS
392000000000     C                   ENDIF
392100000000     C*
392200000000     C                   ENDSR
392300000000     C/EJECT
392400000000     C************************************************************
392500000000     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
392600000000     C************************************************************
392700000000     C     REPDFT        BEGSR
392800000000     C*
392900000000     C* preparo i parametri di chiamata al driver
393000000000     C                   CLEAR                   XMPEDS
393100000000     C                   EVAL      TpChiamata = '01'
393200000000     C                   EVAL      NChiavi = 5
393300000000     C                   EVAL      VLogica = 2
393400000000     C*
393500000000     C* preparo le chiavi per la chiamata al driver
393600000000     C                   CLEAR                   XMPEDS1
393700000000     C                   MOVEL     XSCSOC        DSMPESOC
393800000000     C                   MOVEL     DSPGM         DSMPEPGM
393900000000     C                   MOVEL     KNMUS         DSMPEPRF
394000000000     C* Si reperiscono i dati di default a seconda delle modalità con
394100000000     C* cui il pgm è usato
394200000000     C* MPETIP = 'PX'  pgm richiamato come lista (CMD4)
394300000000     C* MPETIP = 'PV'  pgm richiamato da menù
394400000000     C* MPETIP = 'PB'  x lancio Batch
394500000000     C                   SELECT
394600170719    >C     OPZI73        WHENEQ    '01'
394700000000     C                   MOVEL     'PX'          DSMPETIP
394800000000     C                   OTHER
394900000000     C                   MOVEL     'PV'          DSMPETIP
395000000000     C                   ENDSL
395100000000     C*
395200000000     C                   CALLB     'XMPE'
395300000000     C                   PARM                    XMPEDS
395400000000     C                   PARM                    XMPEDS1
395500000000     C*
395600000000     C     Risultato     IFEQ      '0'
395700000000     C                   MOVEL     DSMPEPAR      DSMPE
395800000000     C* nel caso sia la prima volta che l'utente apre questa anagrafica
395900000000     C* i suoi valori di default vengono decisi a pgm
396000000000     C                   ELSE
396100000000     C                   Z-ADD     1             $LOP
396200000000     C                   Z-ADD     1             $ORD
396300000000     C                   ENDIF
396400170810     C*
396500170810    >C     OPZI73        Ifeq      '05'
396600170810     C                   Movel     '2'           $lop
396700170810     C                   Endif
396800000000     C*
396900000000     C                   ENDSR
397000000000     C/EJECT
397100000000     C************************************************************
397200000000     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
397300000000     C************************************************************
397400000000     C     DEFOPZ        BEGSR
397500000000     C*
397600000000     C* Questa routine deve valorizzare la schiera $FL1
397700000000     C* con S o N a seconda della utilizzabilità o meno
397800000000     C* dell'opzione correlata
397900000000     C*
398000000000     C* Inizialmente si considerano tutte le opzioni come valide
398100000000     C     1             DO        $DIMOP        X
398200000000     C* Imposta tutti i flg delle opzioni non a *blank, a 'S'
398300000000     C                   If        $TX1(X)  <> *Blank
398400000000     C                   MOVE      'S'           $FL1(X)
398500000000     C                   Else
398600000000     C                   Leave
398700000000     C                   EndIf
398800000000     C                   ENDDO
398900000000     C*
399000000000     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
399100000000     C* con 1 o 0 a seconda della utilizzabilità o meno
399200000000     C* del tasto funzione correlato
399300000000     C*
399400000000     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
399500000000     C* di compilazione
399600000000     C                   RESET                   $FC1
399700000000     C                   RESET                   $FR1
399800000000     C                   RESET                   $FC2
399900000000     C                   RESET                   $FR2
400000000000     C                   RESET                   $FC3
400100000000     C                   RESET                   $FR3
400200000000     C*
400300000000     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
400400000000     C* invalido alcune opzioni
400500000000     C                   SELECT
400600000000     C* Se pgm richiamato in ricerca disattivo :
400700170719    >C     OPZI73        WHENEQ    '01'
400800000000     C* F3=Fine
400900000000     C                   Z-ADD     3             X
401000000000     C                   MOVE      '0'           $FC1(X)
401100000000     C                   MOVE      '0'           $FC2(X)
401200000000     C                   MOVE      '0'           $FC3(X)
401300000000     C*
401400000000     C*
401500000000     C* Se pgm richiamato in interrogazione disattivo :
401600170719    >C     OPZI73        WHENEQ    '05'
401700000000     C* F10=Immissione
401800000000     C                   Z-ADD     10            X
401900000000     C                   MOVE      '0'           $FC1(X)
402000000000     C                   MOVE      '0'           $FC2(X)
402100170810     C* F17=Filtro
402200170810     C                   Z-ADD     17            X
402300170810     C                   MOVE      '0'           $FC1(X)
402400170810     C                   MOVE      '0'           $FC2(X)
402500000000     C* 1=Selezione
402600000000     C                   Z-ADD     1             X
402700000000     C     01            LOOKUP    $OP1(X)                                21
402800000000     C     *IN21         IFEQ      *ON
402900000000     C                   MOVE      'N'           $FL1(X)
403000000000     C                   ENDIF
403100000000     C* 2=Modifica
403200000000     C                   Z-ADD     1             X
403300000000     C     02            LOOKUP    $OP1(X)                                21
403400000000     C     *IN21         IFEQ      *ON
403500000000     C                   MOVE      'N'           $FL1(X)
403600000000     C                   ENDIF
403700000000     C* 3=Copia
403800000000     C                   Z-ADD     1             X
403900000000     C     03            LOOKUP    $OP1(X)                                21
404000000000     C     *IN21         IFEQ      *ON
404100000000     C                   MOVE      'N'           $FL1(X)
404200000000     C                   ENDIF
404300000000     C* 4=Annullamento
404400000000     C                   Z-ADD     1             X
404500000000     C     04            LOOKUP    $OP1(X)                                21
404600000000     C     *IN21         IFEQ      *ON
404700000000     C                   MOVE      'N'           $FL1(X)
404800000000     C                   ENDIF
404900170810     C* 44=Aggiornamento
405000170810     C                   Z-ADD     1             X
405100170810     C     44            LOOKUP    $OP1(X)                                21
405200170810     C     *IN21         IFEQ      *ON
405300170810     C                   MOVE      'N'           $FL1(X)
405400170810     C                   ENDIF
405500000000     C*
405600170719    >C     OPZI73        WHENEQ    *BLANKS
405700000000     C* Se pgm non richiamato, disattivo subito :
405800000000     C* F8=Selezione seguente
405900000000     C                   Z-ADD     8             X
406000000000     C                   MOVE      '0'           $FC1(X)
406100000000     C                   MOVE      '0'           $FC2(X)
406200000000     C* 1=Selezione
406300000000     C                   Z-ADD     1             X
406400000000     C     01            LOOKUP    $OP1(X)                                21
406500000000     C     *IN21         IFEQ      *ON
406600000000     C                   MOVE      'N'           $FL1(X)
406700000000     C                   ENDIF
406800000000     C*
406900000000     C* In tutti gli altri casi, disattivo subito :
407000000000     C                   OTHER
407100000000     C* 1=Selezione
407200000000     C                   Z-ADD     1             X
407300000000     C     01            LOOKUP    $OP1(X)                                21
407400000000     C     *IN21         IFEQ      *ON
407500000000     C                   MOVE      'N'           $FL1(X)
407600000000     C                   ENDIF
407700000000     C*
407800000000     C                   ENDSL
407900000000     C*
408000000000     C*
408100000000     C* poi, disattivo a seconda delle autorizzazioni :
408200000000     C                   IF        ImmAll <> *on
408300000000     C*
408400000000     C* F10=Immissione
408500000000     C                   Z-ADD     10            X
408600000000     C                   MOVE      '0'           $FC1(X)
408700000000     C                   MOVE      '0'           $FC2(X)
408800000000     C*
408900000000     C                   ENDIF
409000000000     C*
409100000000     C                   IF        ManAll <> *on
409200000000     C                   Z-ADD     1             X
409300000000     C     02            LOOKUP    $OP1(X)                                21
409400000000     C     *IN21         IFEQ      *ON
409500000000     C                   MOVE      'N'           $FL1(X)
409600000000     C                   ENDIF
409700000000     C                   ENDIF
409800000000     C*
409900000000     C                   IF        AnnAll <> *on
410000000000     C                   Z-ADD     1             X
410100000000     C     44            LOOKUP    $OP1(X)                                21
410200000000     C     *IN21         IFEQ      *ON
410300000000     C                   MOVE      'N'           $FL1(X)
410400000000     C                   ENDIF
410500000000     C                   ENDIF
410600000000     C*
410700000000     C                   IF        VisAll <> *on
410800000000     C*
410900000000     C                   Z-ADD     1             X
411000000000     C     05            LOOKUP    $OP1(X)                                21
411100000000     C     *IN21         IFEQ      *ON
411200000000     C                   MOVE      'N'           $FL1(X)
411300000000     C                   ENDIF
411400000000     C*
411500000000     C                   Z-ADD     1             X
411600000000     C     20            LOOKUP    $OP1(X)                                21
411700000000     C     *IN21         IFEQ      *ON
411800000000     C                   MOVE      'N'           $FL1(X)
411900000000     C                   ENDIF
412000000000     C*
412100000000     C                   ENDIF
412200000000     C*
412300000000     C*
412400000000     C                   ENDSR
412500000000     C/EJECT
412600000000     C************************************************************
412700000000     C* REPERIMENTO OPZIONI
412800000000     C************************************************************
412900000000     C     REPOPZ        BEGSR
413000000000     C*
413100000000     C     1             DO        $DIMOP        X
413200000000     C*
413300000000     C     $MS1(X)       IFNE      *BLANK
413400000000     C*
413500000000     C                   MOVEL     $MS1(X)       IDMSG
413600000000     C                   CALLB     'XRTVM'
413700000000     C                   PARM                    IDMSG            27
413800000000     C                   PARM                    TXTMSG          100
413900000000     C                   PARM                    ERRMSG            1
414000000000     C*
414100000000     C     ERRMSG        IFNE      *ON
414200000000     C                   MOVEL     TXTMSG        $TX1(X)
414300000000     C                   ELSE
414400000000     C                   MOVEL     *ALL'?'       $TX1(X)
414500000000     C                   ENDIF
414600000000     C*
414700000000     C                   ENDIF
414800000000     C*
414900000000     C                   ENDDO
415000000000     C*
415100000000     C                   ENDSR
415200000000     C/EJECT
415300000000     C************************************************************
415400000000     C* REPERIMENTO TASTI DI FUNZIONE
415500000000     C************************************************************
415600000000     C     REPKEY        BEGSR
415700000000     C                   MOVEA     $FM1          $FM
415800000000     C                   MOVEA     $FC1          $FC
415900000000     C                   EXSR      REPKKK
416000000000     C                   MOVEA     $FX           $FX1
416100000000     C*
416200000000     C                   MOVEA     $FM2          $FM
416300000000     C                   MOVEA     $FC2          $FC
416400000000     C                   EXSR      REPKKK
416500000000     C                   MOVEA     $FX           $FX2
416600000000     C*
416700000000     C                   MOVEA     $FM3          $FM
416800000000     C                   MOVEA     $FC3          $FC
416900000000     C                   EXSR      REPKKK
417000000000     C                   MOVEA     $FX           $FX3
417100000000     C                   ENDSR
417200000000     C/EJECT
417300000000     C************************************************************
417400000000     C* REPERIMENTO TASTI DI FUNZIONE
417500000000     C************************************************************
417600000000     C     REPKKK        BEGSR
417700000000     C*
417800000000     C                   CLEAR                   $FX
417900000000     C*
418000000000     C     1             DO        24            X
418100000000     C*
418200000000     C     $FM(X)        IFNE      *BLANK
418300000000     C     $Fc(X)        andne     '0'
418400000000     C*
418500000000     C                   MOVEL     $FM(X)        IDMSG
418600000000     C                   CALLB     'XRTVM'
418700000000     C                   PARM                    IDMSG            27
418800000000     C                   PARM                    TXTMSG          100
418900000000     C                   PARM                    ERRMSG            1
419000000000     C*
419100000000     C     ERRMSG        IFNE      *ON
419200000000     C                   MOVEL     TXTMSG        $FX(X)
419300000000     C                   ELSE
419400000000     C                   MOVEL     *ALL' '       $FX(X)
419500000000     C                   ENDIF
419600000000     C*
419700000000     C                   ENDIF
419800000000     C*
419900000000     C                   ENDDO
420000000000     C*
420100000000     C                   ENDSR
420200000000     C/EJECT
420300000000     C************************************************************
420400000000     C* DEFINIZIONE KLIST
420500000000     C************************************************************
420600000000     C     DefKlist      BEGSR
420700000000     C*
420800000000     C* klist
420900000000     C*
421000170808     C* SDGCST02L
421100170721     C*
421200170808     C     K07CST02      Klist
421300170721     C                   Kfld                    CSTSocieta
421400170721     C                   Kfld                    CSTTpCom
421500170804     C                   kfld                    CSTTprec
421600170721     C                   kfld                    CSTAnno
421700170721     C                   kfld                    CSTTrim
421800170721     C                   kfld                    CSTMese
421900170721     C                   kfld                    CSTCLIFOR
422000170721     C*
422100170808     C     K06CST02      Klist
422200170721     C                   Kfld                    CSTSocieta
422300170721     C                   Kfld                    CSTTpCom
422400170804     C                   kfld                    CSTTprec
422500170721     C                   kfld                    CSTAnno
422600170721     C                   kfld                    CSTTrim
422700170721     C                   kfld                    CSTMese
422800000000     C                   ENDSR
422900000000     C/EJECT
423000170719** $MS1 / $TX1
423100000000PROMSG    *LIBL     OPZ0001
423200000000SDGMSG    *LIBL     SDO0021
423300130828PROMSG    *LIBL     OPZ0005
423400000000PROMSG    *LIBL     OPZ0285
423500000000SDGMSG    *LIBL     SDO0024
423600000000** $OP1 / $FL1
42370000000001S      <Selezione
42380000000002S      <Dati anagrafica
42390013082805S      <Visualizzazione
42400000000011S      <Dettaglio
42410000000044S      <Annulla/Ripristina
424200000000** TASTI DI FUNZIONE  D1 : $FM1 / $FX1
424300000000PROMSG    *LIBL     KEY0001  01
424400000000PROMSG    *LIBL     KEY0002  02
424500000000PROMSG    *LIBL     KEY0003  03
424600000000PROMSG    *LIBL     KEY0004  04
424700000000PROMSG    *LIBL     KEY0005  05
424800000000PROMSG    *LIBL     KEY0006  06
424900000000PROMSG    *LIBL     KEY0007  07
425000000000PROMSG    *LIBL     KEY0008  08
425100000000PROMSG    *LIBL     KEY0009  09
425200000000PROMSG    *LIBL     KEY0010  10
425300151203PROMSG    *LIBL     KEY0011  11
425400000000PROMSG    *LIBL     KEY0012  12
425500000000PROMSG    *LIBL     KEY0013  13
425600000000PROMSG    *LIBL     KEY0014  14
425700000000SDGMSG    *LIBL     SDK0003  15
425800000000PROMSG    *LIBL     KEY0016  16
425900000000SDGMSG    *LIBL     SDK0001  17
426000000000PROMSG    *LIBL     KEY0018  18
426100000000PROMSG    *LIBL     KEY0019  19
426200151203PROMSG    *LIBL     KEY0020  20
426300000000PROMSG    *LIBL     KEY0021  21
426400000000PROMSG    *LIBL     KEY0022  22
426500000000PROMSG    *LIBL     KEY0023  23
426600000000PROMSG    *LIBL     KEY0024  24
426700000000**  ABILITAZIONE D1 : $FC1 / $FR1
4268000000000N  01
4269000000001N  02
4270000000001N  03
4271000000001N  04
4272000000000N  05
4273000000000N  06
4274000000000N  07
4275000000001N  08
4276000000000N  09
4277000000000N  10
4278001512030N  11
4279000000001N  12
4280000000000N  13
4281000000000N  14
4282000000000N  15
4283000000000N  16
4284000000000N  17
4285000000000N  18
4286000000000N  19
4287000000000N  20
4288000000000N  21
4289000000000N  22
4290000000000N  23   Impostare a '1' se si utilizzano opzioni
4291000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
429200000000** TASTI DI FUNZIONE  S1 : $FM2 / $FX2
429300000000PROMSG    *LIBL     KEY0001  01
429400000000PROMSG    *LIBL     KEY0002  02
429500000000PROMSG    *LIBL     KEY0003  03
429600000000PROMSG    *LIBL     KEY0004  04
429700110823SDGMSG    *LIBL     SDK0013  05
429800000000PROMSG    *LIBL     KEY0006  06
429900000000PROMSG    *LIBL     KEY0007  07
430000000000PROMSG    *LIBL     KEY0008  08
430100000000PROMSG    *LIBL     KEY0009  09
430200000000PROMSG    *LIBL     KEY0010  10
430300110826SDGMSG    *LIBL     SDK0014  11
430400000000PROMSG    *LIBL     KEY0012  12
430500000000PROMSG    *LIBL     KEY0013  13
430600000000PROMSG    *LIBL     KEY0014  14
430700000000PROMSG    *LIBL     KEY0015  15
430800000000PROMSG    *LIBL     KEY0016  16
430900000000SDGMSG    *LIBL     SDK0001  17
431000000000PROMSG    *LIBL     KEY0018  18
431100000000PROMSG    *LIBL     KEY0019  19
431200170719SDGMSG    *LIBL     KEY0020  20
431300000000PROMSG    *LIBL     KEY0021  21
431400000000PROMSG    *LIBL     KEY0022  22
431500000000PROMSG    *LIBL     KEY0023  23
431600000000PROMSG    *LIBL     KEY0024  24
431700000000**  ABILITAZIONE S1 : $FC2 / $FR2
4318000000000N  01
4319000000000N  02
4320000000001N  03
4321000000000N  04
4322001309120N  05
4323000000000N  06
4324000000000N  07
4325000000001N  08
4326000000000N  09
4327000000001N  10
4328001309110N  11
4329000000001N  12
4330000000000N  13
4331000000000N  14
4332000000000N  15
4333000000000N  16
4334000000001N  17
4335000000000N  18
4336000000000N  19
4337001708051N  20
4338000000000N  21
4339000000000N  22
4340000000001N  23   Impostare a '1' se si utilizzano opzioni
4341000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
434200000000** TASTI DI FUNZIONE  D3 : $FM3 / $FX3
434300000000PROMSG    *LIBL     KEY0001  01
434400000000PROMSG    *LIBL     KEY0002  02
434500000000PROMSG    *LIBL     KEY0003  03
434600000000PROMSG    *LIBL     KEY0004  04
434700000000PROMSG    *LIBL     KEY0005  05
434800000000PROMSG    *LIBL     KEY0006  06
434900000000PROMSG    *LIBL     KEY0007  07
435000000000PROMSG    *LIBL     KEY0008  08
435100000000PROMSG    *LIBL     KEY0009  09
435200000000PROMSG    *LIBL     KEY0010  10
435300000000PROMSG    *LIBL     KEY0011  11
435400000000PROMSG    *LIBL     KEY0012  12
435500000000PROMSG    *LIBL     KEY0013  13
435600000000PROMSG    *LIBL     KEY0014  14
435700000000PROMSG    *LIBL     KEY0015  15
435800000000PROMSG    *LIBL     KEY0016  16
435900000000SDGMSG    *LIBL     SDK0001  17
436000000000PROMSG    *LIBL     KEY0018  18
436100000000PROMSG    *LIBL     KEY0019  19
436200000000PROMSG    *LIBL     KEY0020  20
436300000000PROMSG    *LIBL     KEY0021  21
436400000000PROMSG    *LIBL     KEY0022  22
436500000000PROMSG    *LIBL     KEY0023  23
436600000000PROMSG    *LIBL     KEY0024  24
436700000000**  ABILITAZIONE D3 : $FC3 / $FR3
4368000000000N  01
4369000000000N  02
4370000000001N  03
4371000000000N  04
4372000000000N  05
4373000000000N  06
4374000000000N  07
4375000000000N  08
4376000000000N  09
4377000000000N  10
4378000000000N  11
4379000000001N  12
4380000000000N  13
4381000000000N  14
4382000000000N  15
4383000000000N  16
4384000000000N  17
4385000000000N  18
4386000000000N  19
4387000000000N  20
4388000000000N  21
4389000000000N  22
4390000000000N  23   Impostare a '1' se si utilizzano opzioni
4391000000001N  24   Lasciare sempre impostata a '1' il valore della riga 24
