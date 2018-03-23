000100981214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981214     H*PARMS BNDDIR(PJXBND PJCBND)
000300980930     H*PARMS CVTOPT(*DATETIME)
000400980930     H DECEDIT('0,') DATEDIT(*DMY.)
000500980930      *
000600980930    > *            Estratto conto contabile - SITUAZIONE PARTITE
000700980930      *                                       dettaglio e totale
000800980930      *           ----------------------------------------------
000900980930      *
001000980930     F*----------------------------------------------------*
001100981002     F* Indicatori che condizionano campi a video:
001200981002     F* 10 on = riga di partita
001300981002     F* 11 Unita
001400981002     F* 12 Percipiente
001500981002     F* 13 Agente e solleciti (solo clienti)
001600981002     F* 14 Bonifici           (solo fornitori)
001700981002     F**15 sottolinea saldo solo se uguale all'importo originario
001800981002     F**16 sottolinea la data partita se ci sono commenti
001900981002     F* 17 fa vedere anche il saldo in divisa se chiesto includi una divisa
002000981002     F**18 fa vedere il saldo in alta intensità se ci sono movimenti provvis
002100981006     F* 19 esistenza commenti
002200980930    >F*----------------------------------------------------------------
002300980930    >F* Viste logiche utilizzate per i diversi ordinamenti
002400980930    >F*----------------------------------------------------------------
002500980930     F*------------
002600980930    >FNDPPA01L  IF   E           K DISK
002700980930    >F                                     RENAME(NDPPA000:NDPPA01)
002800980930    >FNDPPA03L  IF   E           K DISK
002900980930    >F                                     RENAME(NDPPA000:NDPPA03)
003000980930    >FNDPPS01L  IF   E           K DISK
003100980930    >F                                     RENAME(NDPPS000:NDPPS01)
003200980930    >FNDPAS02J  IF   E           K DISK
003300010119     F*
003400031119D1498x***NDREG01L  IF   E           K DISK    ex c1460
003500980930     F*------------
003600980930    >FNDC199D   CF   E             WORKSTN
003700980930     F                                     SFILE(C199S1:S1NRR)
003800980930     F                                     INFDS(DSFMT)
003900980930     D*----------------------------------------------------*
004000980930     D* Schiere per la gestione delle opzioni
004100980930     D*
004200980930     D* dimensione della schiera $MS1
004300010119c1460D $DIMOP          C                   CONST(30)
004400980930     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
004500980930     D $TX1            S             50    DIM($dimop) ALT($MS1)
004600980930     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
004700980930     D $FL1            S              1    DIM($dimop) ALT($OP1)
004800980930     D $TX             S             50    DIM(50)
004900980930     D $FL             S              1    DIM(50)
005000980930     D* Schiere generali per la gestione dei tasti di funzione
005100980930     D $FM             S             27    DIM(24)
005200980930     D $FX             S             50    DIM(24)
005300980930     D $FC             S              1    DIM(24)
005400980930     D $FR             S              1    DIM(24)
005500980930     D* Schiere per la gestione dei tasti di funzione
005600980930     D* Video S1
005700980930     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005800980930     D $FX1            S             50    DIM(24) ALT($FM1)
005900980930     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006000980930     D $FR1            S              1    DIM(24) ALT($FC1)
006100051216     D* titolo stampa e/c
006200980930     D $TIT            S             27    DIM(3) CTDATA PERRCD(1)              Titolo
006300980930     D*
006400051216     D* Schiera costanti
006500051216C1892D $Cost           S             27    DIM(2) CTDATA PERRCD(1)
006600051216     D*
006700981002     D* sottotitolo
006800981002     D SOTTIT          C                   CONST('PROMSG    *LIBL-
006900981002     D                                          COS0414')
007000980930     D/EJECT
007100020528     D*-------------
007200020528c1567D* stampa griglia
007300020528  "  D NDCG01DS      E DS                  INZ
007400020528c1567D NDCG12DS      E DS                  INZ
007500031119     D*-------------
007600031120D1498D* Parametri interrogazione ordini
007700031119D1498D ANA396DS      E DS                  inz
007800980930     D*-------------
007900980930     D* Interrogazione conti
008000980930     D ANA399DS      E DS                  inz
008100981009     D*-------------
008200981009     D* Dati anagrafici del conto
008300981009     D ANA397DS      E DS                  inz
008400981006     D*-------------
008500981006     D* Parametri del PGM di test esistenza commenti ANA724R
008600981006     D A724DS        E DS                  EXTNAME(ANA724DS) INZ
008700981006     D  EXI724       E                     INZ('0')
008800981006     D  RIS724       E                     INZ('0')
008900010305a0871x**CMT724       E                     INZ('0')
009000980930     D*-------------
009100980930     D* Parametri del PGM di gestione commenti ANA725R
009200980930     D ANA725DS      E DS                  INZ
009300980930     D  RET725       E                     INZ('0')
009400980930     D  OPR725       E                     INZ('0')
009500980930     D  ERR725       E                     INZ('0')
009600980930     D*-------------
009700980930     D* Stampa e/C clienti
009800980930    >D NDC141DS      E DS                  inz
009900980930     D*-------------
010000980930     D* Stampa e/C fornitori
010100980930    >D NDC142DS      E DS                  inz
010200980930     D*-------------
010300980930     D* Stampa e/C altri conti
010400980930    >D NDC143DS      E DS                  inz
010500980930     D*-------------
010600980930     D* Passaggio Parametri
010700980930     D KPJBA         E DS
010800980930     D*-------------
010900980930     D* Reperimento Autorizzazioni
011000980930     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
011100980930     D*-------------
011200980930     D* Passaggio Autorizzazioni
011300980930     D DSAUT           DS           512
011400980930     D*-------------
011500980930     D* Maschera dati autorizzazioni
011600980930    >D CONECCDS      E DS
011700980930     D*-------------
011800980930     D* Dati di ambiente ottenuti da XSOC
011900980930    >D SOC001        E DS                  EXTNAME(XSOC001DS)
012000980930     D    FgO1                        1    overlay(XscFgO)
012100980930     D    XscDare                     1    overlay(xscFgo:7)                    Scadenz. retroattivo
012200980930     D    XscAvere                    1    overlay(xscFgo:8)                    Scadenz. retroattivo
012300010119C1460D    XscGest                     1    overlay(xscSkp:3)
012400030206D1270D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
012500980930     D*-------------
012600980930     D* Dati di ambiente ricevuti dal chiamante
012700980930    >D SOC00x          S                   like(SOC001)
012800980930     D*-------------
012900980930     D* DS Interna per dati di output di XSOC
013000980930     D XSOCDS          DS          1000
013100980930     D*-------------
013200980930     D* Controlli validità legende
013300980930     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
013400980930     D*-------------
013500980930     D* Parametri per PGM X01CAMBI
013600980930     D X01CambiDs    E DS                  inz
013700980930     D*-------------
013800980930     D* Parametri per reperimento saldo
013900980930     D X20SCADDS     E DS                  INZ
014000980930     D* DS parametro esteso se richiesto x20parscd='S'
014100980930     D X20SCADXDS    E DS                  inz
014200980930     D*-------------
014300980930     D* Routine editazione di un numero
014400980930     D $SrcVar         s             30S 0
014500980930     D $SrcVarPr       s              9B 0 inz(30)
014600980930     D $SrcVarDe       s              9B 0
014700980930     D $EdtCode        s              1    inz('1')
014800980930     D $LenFld         s              3S 0
014900980930     D $RcvrVar        s             50
015000980930     D $Err            s              1
015100980930     D*-------------
015200980930     D* Parametri in ricezione
015300980930    >D NDC199DS      E DS
015400981001     D*-------------
015500981001     DAnope          E DS                  EXTNAME(ANOPE00F) inz
015600981001     D  OPEDTANN     E                     inz('0001-01-01')
015700981001     D  OPEDTIMM     E                     inz('0001-01-01')
015800981001     D  OPEDTULMO    E                     inz('0001-01-01')
015900980930     D*-------------
016000980930     D* Chiamata ricorsiva
016100980930    >D X48CHPGMDS    E DS                  INZ
016200980930     D*-------------
016300980930     D* Parametri del PGM di manutenzione NDC060R particolarità partite
016400980930    >D NDC060DS      E DS                  inz
016500980930    >D  RET060       E                     INZ('0')
016600980930    >D  OPR060       E                     INZ('0')
016700980930    >D  ERR060       E                     INZ('0')
016800980930    >D  DTCHIU060    E                     INZ('0001-01-01')
016900981002     D*-------------
017000981002     D* Parametri del PGM descrizioni aggiuntive
017100981002     D NDC027DS      E DS                  INZ
017200981002     D  RET027       E                     INZ('0')
017300981002     D  OPR027       E                     INZ('0')
017400981002     D  ERR027       E                     INZ('0')
017500981002     D*-------------
017600981002      * Parametri per call pgm visualizzazione partite
017700981002     D NDC073DS      E DS                  INZ
017800981002     D  OPR073       E                     INZ('0')
017900981002     D  RET073       E                     INZ('0')
018000981002     D  ERR073       E                     INZ('0')
018100981002     D  Exf073       E                     INZ('0')
018200981002     D*-------------
018300981002     D* Parametri per gestione p.nota interattiva
018400981002     D NDC001DS      E DS                  INZ
018500981002     D  RET001       E                     INZ('0')
018600981002     D  OPR001       E                     INZ('0')
018700981002     D  ERR001       E                     INZ('0')
018800981002     D  DTREG1001    E                     inz('0001-01-01')
018900981002     D  DTREG2001    E                     inz('0001-01-01')
019000981002     D  DTLGIO1001   E                     inz('0001-01-01')
019100981002     D  DTLGIO2001   E                     inz('0001-01-01')
019200981002     D  DTULMO1001   E                     inz('0001-01-01')
019300981002     D  DTULMO2001   E                     inz('0001-01-01')
019400981002     D  DTDOC001     E                     inz('0001-01-01')
019500030206D1270D*-------------
019600030206  "  D* dati catalogazione Laguna
019700030206D1270D ANLGN00F      E DS
019800981002     D*-------------
019900981002      * Parametri per call diretta finestra analitica
020000981002     D NDC083DS      E DS                  INZ
020100981002     D  RET083       E                     INZ('0')
020200981002     D  ERR083       E                     INZ('0')
020300981002     D  Exf083       E                     INZ('0')
020400051109     D*-------------
020500051109c1881 * Parametri per call diretta finestra tesoreria
020600051109  "  D NDC084DS      E DS                  INZ
020700051109  "  D  RET084       E                     INZ('0')
020800051109  "  D  ERR084       E                     INZ('0')
020900051109c1881D  exf084       E                     INZ('0')
021000981002     D*-------------
021100981002     D* Parametri del PGM di effetti passivi NDCH01R
021200981002    >D NDCH01ds      E DS                  inz
021300981002    >D RETH01        E                     INZ('0')
021400981002    >D OPRH01        E                     INZ('0')
021500981002    >D ERRH01        E                     INZ('0')
021600981002    >D DTDISTH01     E                     INZ('0001-01-01')
021700981002     D*-------------
021800981002     D* Parametri del PGM di effetti attivii NDCE01r
021900981002    >D NDCE01ds      E DS                  inz
022000981002    >D RETe01        E                     INZ('0')
022100981002    >D OPRe01        E                     INZ('0')
022200981002    >D ERRe01        E                     INZ('0')
022300980930     D*-------------
022400980930     D* Parametri del PGM di manutenzione NDC065R mix
022500981112    >D NDC065DS      E DS                  inz
022600980930    >D  RET065       E                     INZ('0')
022700980930    >D  OPR065       E                     INZ('0')
022800980930    >D  ERR065       E                     INZ('0')
022900980930     D*-------------
023000980930     D* Parametri del PGM dei solleciti NDCL29R
023100980930    >D NDCL29DS      E DS                  inz
023200980930     D*-------------
023300980930     D* Parametri del PGM di ritenute NDCR05R
023400980930    >D NDCR05DS      E DS                  inz
023500980930    >D  RETR05       E                     INZ('0')
023600980930    >D  OPRR05       E                     INZ('0')
023700980930    >D  ERRR05       E                     INZ('0')
023800980930     D*-------------
023900980930     D* Parametri del PGM di ritenute NDCH65R1
024000981112    >D*NDCH65DS      E DS                  inz
024100981112    >D**RETH65       E                     INZ('0')
024200981112    >D**OPRH65       E                     INZ('0')
024300981112    >D**ERRH65       E                     INZ('0')
024400981112     D*-------------
024500981112     D* Parametri del PGM di bonifici NDCB50R
024600981112     D NDCB50DS      E DS                  inz
024700981112     D  RETB50       E                     INZ('0')
024800981112     D  OPRB50       E                     INZ('0')
024900981112     D  ERRB50       E                     INZ('0')
025000981130     D  DTPARB50     E                     INZ('0001-01-01')
025100070621c2034D  DTDIST1B50   E                     INZ('0001-01-01')
025200070621c2034D  DTDIST2B50   E                     INZ('0001-01-01')
025300980930     D*-------------
025400980930     D* Parametri videata riepilogo per divisa
025500980930     D Ndc293Ds      E DS                  Inz
025600980930     D  OPR293       E                     INZ('0')
025700980930     D  RET293       E                     INZ('0')
025800980930     D  ERR293       E                     INZ('0')
025900980930     D*-------------
026000980930     D* Ds multipla riepilogo per divisa
026100980930     D Ndc291Ds1     E DS                  Inz Occurs(50)
026200980930     D*-------------
026300980930     D* Parametri programma riepilogo per divisa
026400980930     D Ndc291Ds      E DS                  Inz
026500980930     D  Esito291     E                     INZ('0')
026600980930     D*-------------
026700980930     D* Controllo conti
026800980930     D ND002DS       E DS                  inz
026900980930     D*-------------
027000980930     D* Definizioni parametri standard chiamate moduli
027100980930     D Esito           S              1
027200980930     D gesmsg          S              1
027300980930     D lenout          S              9B 0
027400980930     D Data            S             10
027500980930     D strutturaO      S             10
027600980930     D Operazione      S              2
027700980930     D LenIn           S              9B 0
027800980930     D refresh         S              1
027900980930     D uso             S              1
028000980930     DOutput           S           4000
028100980930     D*-------------
028200980930     D DSFMT           DS           512
028300980930     D  $TASTO               369    369
028400980930     D  NRG                  370    370
028500980930     D  NCL                  371    371
028600980930     D  SFLNRR               378    379B 0
028700980930     D*-------------
028800980930     D* posizione cursore
028900980930     D CURSOR          DS
029000980930     D  RIRI                   1      2B 0 INZ
029100980930     D  R$$                    2      2
029200980930     D  COCO                   3      4B 0 INZ
029300980930     D  C$$                    4      4
029400981002     D* Ds Riga  partita o movimento
029500980930     D S1RIGATOT       DS
029600981009     D  DsGGPar                1      2
029700981009     D  DsBarra1               3      3
029800981009     D  DsMMPar                4      5
029900981009     D  DsBarra2               6      6
030000981009     D  DsAAPar                7      8
030100981009     D  DsNrPar               10     18
030200981009     D  DsSerPar              20     23
030300981009     D  DsDivisa              25     28
030400981009     D  DsTpRegz               1      1
030500981009     D  DsCaus                 3      6
030600981009     D  DsCausD                8     29
030700981130     D*-------------
030800981130      * Variabile di appoggio per 'lavorare con date Iso
030900981130     D                 DS
031000981130     D  $DtPar60               1      6  0
031100981130     D  $GGPar                 1      2
031200981130     D  $MMPar                 3      4
031300981130     D  $AAPar                 5      6
031400981130     D $DataISO        S               D
031500980930     D*-------------
031600980930     D* Reperimento nome PGM
031700980930     D STATUS         SDS           333
031800980930     D  DSPGM            *PROC
031900980930     D  PARMS            *PARMS
032000980930     D*-------------
032100980930     D* Attributi video
032200980930     D* (definisco come costanti qugli attributi video che potrei
032300980930     D*  testare e quindi preferisco assegnargli un valore "parlante")
032400980930     D*  bottone presente
032500980930     D PRESENTE        C                   CONST(X'A1')
032600980930     D*-------------
032700980930     D* Tasti di funzione
032800980930     D F01             C                   CONST(X'31')
032900980930     D F02             C                   CONST(X'32')
033000980930     D F03             C                   CONST(X'33')
033100980930     D F04             C                   CONST(X'34')
033200980930     D F05             C                   CONST(X'35')
033300980930     D F06             C                   CONST(X'36')
033400980930     D F07             C                   CONST(X'37')
033500980930     D F08             C                   CONST(X'38')
033600980930     D F09             C                   CONST(X'39')
033700980930     D F10             C                   CONST(X'3A')
033800980930     D F11             C                   CONST(X'3B')
033900980930     D F12             C                   CONST(X'3C')
034000980930     D F13             C                   CONST(X'B1')
034100980930     D F14             C                   CONST(X'B2')
034200980930     D F15             C                   CONST(X'B3')
034300980930     D F16             C                   CONST(X'B4')
034400980930     D F17             C                   CONST(X'B5')
034500980930     D F18             C                   CONST(X'B6')
034600980930     D F19             C                   CONST(X'B7')
034700980930     D F20             C                   CONST(X'B8')
034800980930     D F21             C                   CONST(X'B9')
034900980930     D F22             C                   CONST(X'BA')
035000980930     D F23             C                   CONST(X'BB')
035100980930     D F24             C                   CONST(X'BC')
035200980930     D ENTER           C                   CONST(X'F1')
035300980930     D ROLDWN          C                   CONST(X'F4')
035400980930     D ROLLUP          C                   CONST(X'F5')
035500980930     D*-------------
035600980930     D DATMIN          C                   CONST('0001-01-01')
035700980930     D DATMAX          C                   CONST('9999-12-31')
035800980930     D*-------------
035900980930     D* Variabili per gestione videate
036000980930     D $Gest           S              2A
036100980930     D $Fine           S              1A
036200980930     D $InzD1          S              1A
036300980930     D $InzS1          S              1A
036400980930     D $LastG          S              2A
036500980930     D $LastV          S              2A
036600980930     D $EndFile        S              1A
036700980930     D $EndFile1       S              1A
036800980930     D $Esci           S              1A
036900980930     D $RcdOK          S              1A
037000980930     D $RcdOK1         S              1A
037100980930     D $carpag         S              1A
037200980930     D $ord            S              1S 0
037300980930     D sflpag          S              4S 0
037400991014     D AlmenoUno       S              1A
037500051109c1881D Wdw084wrk       S              1
037600980930     D*-------------
037700980930     D* Variabili appoggio sempre presenti in tutte le anagrafiche
037800980930     D S1Nrr           S                   like(C1Rcd)
037900980930     D SFLNRRSAV       S                   like(SFLNRR)
038000980930     D WSfl            S                   like(C1Nrr)
038100980930     D WMax            S                   like(C1Rcd)
038200980930     D WPag            S                   like(C1Rcd)
038300980930     D WInzS1          S                   like($InzS1)
038400980930    >D WOpr            S                   like(Opr199)
038500980930     D CurR            S              2S 0
038600980930     D CurC            S              2S 0
038700980930     D Win             S             99A
038800980930     D Wdate           S             10
038900980930    >D WDivisa         S                   like(X01Divisa)
039000980930    >D SavDivisa       S                   like(X01Divisa)
039100980930    >D SavDivVis       S                   like(divvis199)
039200981001    >D SavCausRig      S                   like(Opecausale)
039300980930    >D Wl29iniz        S                   like(l29iniz)
039400981002    >D Winz073         S                   like(exf073)
039500980930    >D WImporto        S                   like(saldo199)
039600980930    >D NrDec           S              3S 0
039700980930    >D NrDecd          S              3S 0
039800980930    >D WNrDec          S              3S 0
039900980930     D Wsegno          S              1A
040000980930    >D WSaldo          S                   like(Saldo199)
040100981002    >D Wkcc            S                   like(c1kcc)
040200981002    >D Wksc            S                   like(c1ksc)
040300980930     D wopzs1          S              1
040400980930     D wopzs2          S              1
040500000118C1309D ZDateISO        S               D
040600980930     d* Totali dare avere in moneta di conto del cliente
040700980930    >D WSaldoTotD      S                   like(Saldo199)
040800980930    >D WSaldoTotA      S                   like(Saldo199)
040900980930     d* Totali dare avere in divisa del cliente
041000980930    >D WSaldoToDD      S                   like(Saldo199)
041100980930    >D WSaldoToAD      S                   like(Saldo199)
041200980930     d* Totali dare avere in moneta di conto di partita
041300980930    >D WTotDare        S                   like(saldo199)
041400980930    >D WTotAvere       S                   like(Saldo199)
041500980930     d* Totali dare avere in divisa di partita
041600980930    >D WTotDareD       S                   like(saldo199)
041700980930    >D WTotAvereD      S                   like(Saldo199)
041800030206D1270D*-------------
041900030206  "   * Parametri driver Laguna
042000030206  "  D Opzione         S              2A
042100030206  "  D Errore          S              1A
042200030206  "  D Vista           S              2S 0
042300030206  "  D Dati            S           4000A
042400030206  "  D Lunghezza       S              9B 0
042500030206  "  D Commit          S              1A
042600030206  "  D NrChiavi        S              2S 0
042700030206  "  D Alloca          S              1A
042800030206  "  D DataISO         S               D
042900030206D1270D*
043000980930     D*-------------
043100980930     D*  Indici
043200980930     D X               S              3S 0
043300980930     D Y               S              3S 0
043400030206D1270D*-------------
043500030206  "  D* Chiamata a Laguna
043600071019D1270D Cmd670          S             15
043700071019C2055x*Parm670         S             44    dim(30)   ex D1270
043800071019C2055D Parm670         S             44    dim(70)
043900071019D1270D Err670          S              1
044000030206D1270D Ret670          S             32A
044100980930     D*-------------
044200980930     D* Richiamo a XATB
044300980930     D XATBDS        E DS
044400980930     D*-------------
044500980930     D* Tabella blocco amministrativo
044600980930     D TAB0AB        E DS                  EXTNAME(ANG0ABDS)
044700980930     C*----------------------------------------------------*
044800980930     C*                MAIN LINE PROGRAM
044900980930     C*----------------------------------------------------*
045000980930     C*
045100980930     C* inizializzazione variabili
045200980930     C                   EXSR      INZVAR
045300980930     C*
045400980930     C     $FINE         DOWEQ     *OFF
045500980930     C     $GEST         CASEQ     'S1'          GESS1
045600980930     C                   ENDCS
045700980930     C                   ENDDO
045800980930     C* fine programma
045900980930     C                   EXSR      ENDPGM
046000980930     C*
046100980930     C/EJECT
046200980930     C************************************************************
046300980930     C* FINE PROGRAMMA
046400980930     C************************************************************
046500980930     C     ENDPGM        BEGSR
046600980930     c* Per far riemettere la testata
046700980930     C                   MOVE      *HIVAL        $LASTV
046800980930     c                   move      wopr          opr199
046900980930     C*
047000980930     C* Se pgm Guida non è stato richiamato posso chiudere in LR
047100980930    >C     OPZ199        IFEQ      *BLANK
047200980930     C                   SETON                                            LR
047300980930     C* altrimenti chiudo in RT
047400980930     C                   ELSE
047500980930     C                   SETON                                            RT
047600980930     C                   ENDIF
047700980930     C                   RETURN
047800980930     C*
047900980930     C                   ENDSR
048000980930     C/EJECT
048100980930     C************************************************************
048200980930     C* RICERCHE
048300980930     C************************************************************
048400980930     C     SEARCH        BEGSR
048500980930     C*
048600980930     C* determino Riga/Colonna del cursore
048700980930     C                   MOVE      NRG           R$$
048800980930     C                   MOVE      NCL           C$$
048900980930     C                   Z-ADD     RIRI          CURR
049000980930     C                   Z-ADD     COCO          CURC
049100980930     C*
049200980930   B1C                   SELECT
049300980930     C*
049400980930    >C*--------------------------------------------------------------
049500980930    >C* inserire le routine di ricerca
049600980930    >C*--------------------------------------------------------------
049700980930    >C* FMTD1 -
049800980930    >C     1             WHENEQ    1
049900980930     C*
050000980930     C* ricerca non abilitata in questa posizione
050100980930   X1C                   OTHER
050200980930     C                   SETON                                        9899
050300980930     C*
050400980930   E1C                   ENDSL
050500980930     C*
050600980930     C* imposto pos. del cursore
050700980930     C                   Z-ADD     CURR          H1RIGA
050800980930     C                   Z-ADD     CURC          H1COLO
050900980930     C*
051000980930     C                   MOVE      *HIVAL        $LASTV
051100980930     C*
051200980930     C                   ENDSR
051300980930     C/EJECT
051400980930     C************************************************************
051500980930     C* RIEMPIMENTO OPZIONI
051600980930     C************************************************************
051700980930     C     RIEOPZ        BEGSR
051800980930     C*
051900980930     C* Input :
052000980930     C* - $TX = schiera con tutti i testi disponibili
052100980930     C* - $FL = schiere con i flag di validità associati ai testi
052200980930     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
052300980930     C*          opzione usato
052400980930     C* - WMXO = variabile contenente il numero massimo di testi
052500980930     C*          opzione usabili
052600980930     C* - $L01 = Lunghezza prima riga
052700980930     C* - $L02 = Lunghezza seconda riga
052800980930     C* Output :
052900980930     C* - WOPZ1 = variabile contenente la prima riga opzioni
053000980930     C* - WOPZ2 = variabile contenente la seconda riga opzioni
053100980930     C* - FF23  = flag che indica se c'è bisogno del tasto F23
053200980930     C*
053300980930     C                   CALLB     'X04OPMSG'
053400980930     C* input
053500980930     C                   PARM                    $TX
053600980930     C                   PARM                    $FL
053700980930     C                   PARM                    WCOZ              3 0
053800980930     C                   PARM                    WMXO              3 0
053900980930     C* output
054000980930     C                   PARM                    WOPZ1            76
054100980930     C                   PARM                    WOPZ2            76
054200980930     C                   PARM                    FF23              1
054300980930     C* input
054400980930    >C                   PARM                    $L01              3 0
054500980930    >C                   PARM                    $L02              3 0
054600980930     C*
054700060224C1913C                   PARM                    $ALLOPT         960
054800060224C1913C*
054900980930     C                   ENDSR
055000980930     C/EJECT
055100980930     C************************************************************
055200980930     C* RIEMPIMENTO TASTI FUNZIONE
055300980930     C************************************************************
055400980930     C     RIEKEY        BEGSR
055500980930     C*
055600980930     C* Input :
055700980930     C* - $FX = schiera con tutti i testi disponibili
055800980930     C* - $FC = schiere con i flag di validità associati ai testi
055900980930     C* - $FR = schiere con i flag di REVERSE IMAGE
056000980930     C* - $ULK = ultimo testo caricato
056100980930     C* - $L01 = Lunghezza prima riga
056200980930     C* - $L02 = Lunghezza seconda riga
056300980930     C* Output :
056400980930     C* - $ULK
056500980930     C* - $KEY1= 1° riga di tasti funzione
056600980930     C* - $KEY2= 2° riga di tasti funzione
056700980930     C*
056800980930     C                   CALLB     'XKEYMSG'
056900980930     C* input
057000980930     C                   PARM                    $FX
057100980930     C                   PARM                    $FC
057200980930     C                   PARM                    $FR
057300980930     C                   PARM                    $ULK              3 0
057400980930     C* output
057500980930     C                   PARM                    $KEY1            79
057600980930     C                   PARM                    $KEY2            79
057700980930     C* input
057800980930    >C                   PARM                    $L01              3 0
057900980930    >C                   PARM                    $L02              3 0
058000980930     C*
058100060224C1913C                   PARM                    $ALLFUNCT       480
058200060224C1913C*
058300980930     C                   ENDSR
058400980930     C/EJECT
058500980930     C************************************************************
058600980930     C* GESTIONE EMISSIONE TASTI FUNZIONE
058700980930     C************************************************************
058800980930     C     GESPIE        BEGSR
058900980930     C*
059000980930     C                   SELECT
059100980930     C* Lista
059200980930     C     $GEST         WHENEQ    'S1'
059300980930     C     $ULKS1        IFEQ      0
059400980930     C*
059500980930     C* Call routine editazione x importo in moneta di conto
059600980930     C* da fare solo una volta poichè si fa quando il caricamento pagina
059700980930     C* non è presente ( se c'era il caricamento a pagina, avevo già reperit
059800980930     C* o questi dati nella routine INZVAR)
059900980930     C                   IF        $carpag = *off
060000980930     C* totale moneta di conto
060100980930     C                   Z-ADD     WSaldoTotd    WtotDare
060200980930     C                   Z-ADD     WSaldoTotA    WTotAvere
060300980930     C                   exsr      RepSegno
060400980930     C* importo
060500980930     C                   Z-add     Wsaldo        WImporto
060600980930     C                   Z-add     NrDec         WNrDec
060700980930     C                   Eval      $LenFld = %Size(Z1Saldo)
060800980930     C* allineo dare a sinistra
060900980930     C                   if        Wsegno = xscdare
061000980930     C                   SUB       5             $lenfld
061100980930     C                   endif
061200980930     C                   EXSR      Edit
061300980930     C                   Eval      Z1Saldo = $RcvrVar
061400980930     C                   eval      z1segno = Wsegno
061500980930     c*
061600980930     C* totale in divisa
061700980930     C                   SETOFF                                       17
061800980930     C                   if        divisa199 <> *blanks and divsel199 = 'I'
061900980930     c                             and divisa199 <> xscdiv
062000980930     C                   seton                                        17
062100980930     c                   eval      wdivisa = divisa199
062200980930     c                   exsr      repdivisa
062300980930     c                   eval      nrdecD = x01nrdec
062400980930     C                   Z-ADD     WSaldoToDD    WtotDareD
062500980930     C                   Z-ADD     WSaldoToAD    WTotAvereD
062600980930     C                   exsr      RepSegnoD
062700980930     C* importo
062800980930     C                   Z-add     Wsaldo        WImporto
062900980930     C                   Z-add     NrDecD        WNrDec
063000980930     C                   Eval      $LenFld = %Size(Z1Saldo)
063100980930     C* allineo dare a sinistra
063200980930     C                   if        Wsegno = xscDare
063300980930     C                   SUB       5             $lenfld
063400980930     C                   endif
063500980930     C                   EXSR      Edit
063600980930     C                   Eval      Z1SaldoDiv = $RcvrVar
063700980930     C                   eval      z1segnoDiv = Wsegno
063800980930     c                   eval      z1divisa = DIVISA199
063900980930     C                   endif
064000980930     C                   endif
064100980930     c*
064200980930     C                   EXSR      F24S1
064300980930     C                   ELSE
064400980930     C                   WRITE     C199Z1
064500980930     C                   ENDIF
064600980930     C                   ENDSL
064700980930     C*
064800980930     C                   ENDSR
064900980930     C/EJECT
065000980930     C************************************************************
065100980930     C* Repsegno Reperisce segno moneta di conto
065200980930     C************************************************************
065300980930     C     RepSegno      BEGSR
065400980930     C*
065500980930     C                   Select
065600980930     C                   when      Wtotdare   > WtotAvere
065700980930     C                   eval      WSegno   = xscdare
065800980930     C                   eval      WSaldo   = Wtotdare   - Wtotavere
065900980930     C*
066000980930     C                   when      Wtotdare   < Wtotavere
066100980930     C                   eval      WSegno   = xscavere
066200980930     C                   eval      WSaldo   = Wtotavere  - Wtotdare
066300980930     C*
066400980930     C                   when      Wtotdare   = Wtotavere
066500980930     C                   eval      WSegno   = ' '
066600980930     C                   eval      WSaldo   = 0
066700980930     C                   endsl
066800980930     C*
066900980930     C                   ENDSR
067000980930     C*
067100980930     C/EJECT
067200980930     C************************************************************
067300980930     C* Repsegno Reperisce segno divisa
067400980930     C************************************************************
067500980930     C     RepSegnod     BEGSR
067600980930     C*
067700980930     C                   Select
067800980930     C                   when      WtotdareD  > WtotAvereD
067900980930     C                   eval      WSegno   = xscdare
068000980930     C                   eval      WSaldo   = WtotdareD  - WtotavereD
068100980930     C*
068200980930     C                   when      WtotdareD  < WtotavereD
068300980930     C                   eval      WSegno   = xscavere
068400980930     C                   eval      WSaldo   = WtotavereD - WtotdareD
068500980930     C*
068600980930     C                   when      WtotdareD  = WtotavereD
068700980930     C                   eval      WSegno   = ' '
068800980930     C                   eval      WSaldo   = 0
068900980930     C                   endsl
069000980930     C*
069100980930     C                   ENDSR
069200980930     C/EJECT
069300980930     C************************************************************
069400980930     C* GESTIONE LISTA
069500980930     C************************************************************
069600980930     C     GESS1         BEGSR
069700980930     C* inizializzazione videata
069800980930     C     $INZS1        IFEQ      *ON
069900980930     C                   EXSR      INZS1
070000980930     C                   MOVE      *OFF          $INZS1
070100980930     C                   ENDIF
070200980930     C* emissione piede videata se proveniente da altra
070300980930     C* salvataggio valore formato di provenienza
070400980930     C     $LASTV        IFNE      'S1'
070500980930     C                   WRITE     C199T1
070600980930     C                   EXSR      GESPIE
070700980930     C                   MOVE      $LASTV        $LASTG
070800980930     C                   MOVE      'S1'          $LASTV
070900980930     C                   ENDIF
071000980930     C*
071100980930     C     WMAX          IFEQ      0
071200980930     C                   WRITE     C199D1
071300980930     C                   ENDIF
071400980930     C*
071500981002     C                   Eval      *in09=c1mode
071600980930     C                   EXFMT     C199C1
071700980930     C     C1NRR         IFNE      0
071800980930     C                   Z-ADD     C1NRR         WSFL
071900980930     C                   ENDIF
072000980930     C*
072100980930     C     SFLNRR        IFNE      0
072200980930     C                   Z-ADD     SFLNRR        C1RCD
072300980930     C                   Z-ADD     SFLNRR        SFLNRRSAV
072400980930     C                   ELSE
072500980930     C                   Z-ADD     SFLNRRSAV     C1RCD
072600980930     C                   ENDIF
072700980930     C* faccio eseguire al pgm di sola finestra la close e la open per far
072800980930     c* riemettere il nuovo sfondo del subfile
072900980930     C                   eval      wl29iniz   = '0'
073000981002     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
073100981002     C                   MOVE      '1'           Winz073
073200051110c1881C                   MOVE      '1'           Wdw084wrk
073300980930     C*
073400980930     C* controllo abilitazione tasti funzione
073500980930     C                   EXSR      CTRKS1
073600980930     C*
073700980930     C     *IN99         IFEQ      *OFF
073800980930     C*
073900980930     C                   SELECT
074000981009     C* F2=Dati conto
074100981009     C     $TASTO        WHENEQ    F02
074200981009     C                   EXSR      F02S1
074300980930     C* F3=Fine
074400980930     C     $TASTO        WHENEQ    F03
074500980930     C                   EXSR      F03S1
074600980930     C* F8=Selezione seguente
074700980930     C     $TASTO        WHENEQ    F08
074800980930     C                   EXSR      F08S1
074900980930     C* F11=Cambio moneta
075000980930     C     $TASTO        WHENEQ    F11
075100980930     C                   EXSR      F11S1
075200980930     C* F12=Ritorno
075300980930     C     $TASTO        WHENEQ    F12
075400980930     C                   EXSR      F12S1
075500980930     C* F15=Interrogazioni
075600980930     C     $TASTO        WHENEQ    F15
075700980930     C                   EXSR      F15S1
075800980930     C* F16=Riepilogo per divisa
075900980930     C     $TASTO        WHENEQ    F16
076000980930     C                   EXSR      F16S1
076100980930     C* F20=Stampa e/c
076200980930     C     $TASTO        WHENEQ    F20
076300980930     C                   EXSR      F20S1
076400020528c1567C* F21=Stampa griglia
076500020528  "  C     $TASTO        WHENEQ    F21
076600020528c1567C                   EXSR      F21S1
076700980930     C* F23=Altre opzioni
076800980930     C     $TASTO        WHENEQ    F23
076900980930     C                   EXSR      F23S1
077000980930     C* F24=Altri tasti
077100980930     C     $TASTO        WHENEQ    F24
077200980930     C                   EXSR      F24S1
077300980930     C* ROLL-UP (se gestito a pagina)
077400980930     C     $TASTO        WHENEQ    ROLLUP
077500980930     C     $CARPAG       ANDEQ     *ON
077600980930     C                   EXSR      ROLS1
077700980930     C*
077800980930     C                   OTHER
077900980930     C* CONTROLLO DATI
078000980930     C                   EXSR      CTRC1
078100980930     C     *IN99         IFEQ      *OFF
078200980930     c* controllo che non vengano scelte opzioni miste di selezione se
078300980930     C* scelta opzione multipla
078400980930     c                   if        multip199 = *on
078500980930     C                   EXSR      CTRopz
078600980930     C                   endif
078700980930     C     *IN99         IFEQ      *OFF
078800980930     C                   EXSR      CTRS1
078900980930     C                   ENDIF
079000980930     C                   ENDIF
079100980930     C*
079200980930     C                   ENDSL
079300980930     C*
079400980930     C                   ENDIF
079500980930     C*
079600980930     C                   ENDSR
079700980930     C/EJECT
079800980930     C************************************************************
079900980930     C* CONTROLLI PER opzioni miste se scelta selezione multipla
080000980930     C************************************************************
080100980930     C     CTROPZ        BEGSR
080200980930     C*
080300980930     C                   SETOFF                                       99
080400980930     c                   clear                   wopzs1
080500980930     c                   clear                   wopzs2
080600980930     C*
080700980930   B1C     WMAX          IFGT      0
080800980930     C                   READC     C199S1                                 21
080900980930   B2C     *IN21         DOWEQ     *OFF
081000980930     C*
081100980930   B3C     S1OPZ         IFEQ      1
081200980930     C                   eval      wopzs1 = *on
081300980930   E3C                   ENDIF
081400980930   B3C     S1OPZ         IFNE      1
081500980930   B3C     S1OPZ         andne     0
081600980930     C                   eval      wopzs2 = *on
081700980930   E3C                   ENDIF
081800980930     c*
081900980930     C                   MOVE      *ON           *IN32
082000980930     C* imposto quegli attributi del sfl che sono modificabili
082100980930     C* al richiamo del determinato pgm
082200980930     C                   EXSR      ATRS1
082300980930     C                   UPDATE    C199S1
082400980930     C                   MOVE      *Off          *IN32
082500980930     c*
082600980930     C                   READC     C199S1                                 21
082700980930     C*
082800980930   E2C                   ENDDO
082900980930     C*
083000980930     C* se rilevate opzioni miste , attivo sflnxtchg
083100980930     C                   if        wopzs1 = *on and
083200980930     C                             wopzs2 = *on
083300980930     C                   seton                                        9993
083400980930     C                   endif
083500980930     C*
083600980930   E1C                   ENDIF
083700980930     C*
083800980930     C                   ENDSR
083900980930     C/EJECT
084000980930     C************************************************************
084100980930     C* INIZIALIZZAZIONE LISTA
084200980930     C************************************************************
084300980930     C     INZS1         BEGSR
084400980930     C* pulizia SFL
084500980930     C                   SETOFF                                         3031
084600980930     C                   WRITE     C199C1
084700980930     C                   SETON                                          31
084800980930     C*
084900980930     C* riempimento opzioni
085000980930     C                   MOVEA     $TX1          $TX
085100980930     C                   MOVEA     $FL1          $FL
085200980930     C                   Z-ADD     0             WCOZ
085300980930     C*  equivale al num.di elementi della schiera $TX1
085400980930     C                   Z-ADD     $DIMOP        WMXO
085500980930    >C                   Z-ADD     76            $L01
085600980930    >C                   Z-ADD     76            $L02
085700980930     C                   EXSR      RIEOPZ
085800980930     C                   Z-ADD     23            X
085900980930     C                   MOVE      FF23          $FC1(X)
086000980930     C                   MOVEL     WOPZ1         C1OPZ1
086100060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
086200980930     C                   MOVEL     WOPZ2         C1OPZ2
086300980930     C                   Z-ADD     WCOZ          C1COZ
086400051215C1892C* accendo indicatore 18 per differenti costanti dei titoli
086500051215  "  c                   if        dataSf199=*blanks
086600051215  "  c                   setoff                                       18
086700051215  "  c                   else
086800051215  "  c                   seton                                        18
086900051215c1892c                   endif
087000980930     C* imposta control
087100980930     C* reperisco capoconto se cliente/fornitore
087200980930   B1C     kcc199        IFEQ      'C       '
087300980930     C     kcc199        OREQ      'F       '
087400980930     C                   CALLB     'XCAPCLIFOR'
087500980930     C                   PARM                    Xscsoc
087600980930     C                   PARM                    kcc199
087700980930     C                   PARM                    ksc199
087800980930   E1C                   ENDIF
087900980930     C                   EVAL      C1KCC = kcc199
088000980930     C                   EVAL      C1KsC = ksc199
088100981002     C                   EVAL      Wkcc  = kcc199
088200981002     C                   EVAL      Wksc  = ksc199
088300980930     c* Reperisco dati del conto sempre
088400980930     c                   exsr      callmvc002
088500981002     c                   if         esito = *off
088600981002     c                   eval       descr199   = descr002
088700981002     c                   eval       desagg199  = desagg002
088800981002     c                   eval       percipi199 = percipi002
088900981002     c                   eval       snatura199 = snatura002
089000981002     c                   eval       analit199  = analit002
089100981002     c                   endif
089200980930     c* decodifico blocco amministrativo
089300981002     c                   clear                   c1bkammd
089400980930     c                   if        Bkamm002 <> *blanks
089500980930     C                   MOVE      '1'           XTBRIC
089600980930     C                   MOVE      XSCSOC        XTBAZI
089700980930     C                   MOVE      *ZERO         XTBKEY
089800980930     C                   MOVE      '0AB'         XTBCOD
089900980930     C                   MOVE      BkAmm002      XTBKEY
090000980930     C                   CALLb     'XATB'
090100980930     C                   PARM                    XATBDS
090200980930     C     XTBERR        IFEQ      *Off
090300980930     C                   MOVEL     XTBUNI        TAB0AB
090400981002     C                   MOVEL     DE20AB        C1BkAmmD
090500980930     c                   endif
090600980930     c                   endif
090700980930     C                   EVAL      C1Descr = DESCR199
090800980930     C                   EVAL      C1Desagg = DESAGG199
090900980930     C* Attivo o no scritta percipiente
091000980930     C                   IF        percipi199 = *on
091100980930     c                   eval      *in12 = *on
091200980930     C                   else
091300980930     c                   eval      *in12 = *off
091400980930     C                   endif
091500981006     C* Controlla esistenza commenti
091600981006     c                   exsr      TstTcm
091700981006     C* Attivo o no scritta commenti
091800981006     C                   IF        exi724     = *on
091900981006     c                   eval      *in19 = *on
092000981006     C                   else
092100981006     c                   eval      *in19 = *off
092200981006     C                   endif
092300980930     C*
092400980930     C* CARICAMENTO SFL A PAGINE o intero
092500980930     C*
092600980930     C                   Z-ADD     0             S1NRR
092700980930     C                   Z-ADD     0             WMAX
092800980930     C*
092900980930     C* Posizionamento su file pilota
093000980930     C                   EXSR      SETANA
093100980930     C*
093200980930     C* Lettura file pilota
093300980930     C                   EXSR      REDANA
093400981001     C* Posizionamento su NDpas per la partita letta
093500981001     C     $EndFile      IFEQ      *OFF
093600981001     c     $rcdok        andeq     *on
093700981001     C                   EXSR      SETPAS
093800981001     c                   endif
093900980930     C*
094000980930     C* Carico 1a pagina o tutto
094100980930     C                   Z-ADD     SFLPAG        WPAG
094200980930     C                   EXSR      ROLS1
094300980930     C*
094400980930     C                   ENDSR
094500980930     C/EJECT
094600980930     C*----------------------------------------------------------
094700980930     C* Chiamata NDMVC002
094800980930     C*----------------------------------------------------------
094900980930     C     CallMvc002    BEGSR
095000980930     C*
095100980930     C* Controllo conto
095200980930     C                   Clear                   ND002DS
095300980930     C                   EVAL      LenOut = %Size(ND002DS)
095400980930     C                   eval      Data = Datmin
095500980930     C                   CallB     'NDMVC002'
095600980930     C                   PARM                    xscsoc
095700981002     C                   PARM                    Wkcc
095800981002     C                   PARM                    Wksc
095900980930     C                   PARM                    Data
096000980930     C                   PARM      *OFF          GesMsg
096100980930     C                   PARM      *OFF          Esito
096200980930     C                   PARM      'ND002DS'     StrutturaO
096300980930     C                   PARM                    ND002DS
096400980930     C                   PARM                    LenOut
096500980930     C*
096600980930     C*
096700980930     C                   ENDSR
096800980930     C/EJECT
096900980930     C************************************************************
097000980930     C* CARICAMENTO PAGINA LISTA
097100980930     C************************************************************
097200980930     C     ROLS1         BEGSR
097300980930     C*
097400980930     C                   SETOFF                                       32
097500980930     C                   Z-ADD     0             Y
097600980930     C                   Z-ADD     WMAX          S1NRR
097700980930     C*
097800981001     C* Leggo dal file Ndppa00f per caricare la lista
097900981001  1  C     $EndFile      DOWEQ     *OFF
098000981001     c     Y             andlt     Wpag
098100981001     c*
098200981001     c* Ciclo lettura righe di NDpas
098300981001  2  C     $EndFile1     DOWEQ     *OFF
098400981001     c     Y             andlt     Wpag
098500980930     C                   EXSR      REDPAS
098600981001     C* Scrive fino a fine lista
098700981001  3  c                   if        $rcdok1 =  *on  and
098800981001     c                             $endfile1 =*off
098900980930     c                   exsr      ries1A
099000991014     c* memorizzo che c'è almeno un movimento
099100991014     c                   eval      AlmenoUno = *on
099200981001     C                   ADD       1             S1NRR
099300981001     C                   ADD       1             Y
099400981001     C                   WRITE     C199S1
099500981001  3e c                   endif
099600981001     c*
099700981001  2e c                   ENDDO
099800981001     c*
099900981001     c* scrivo riga partita solo se ci sta nella pagina e leggo altra part.
100000991014     c* e se c'è almeno un movimento
100100991014     c                   if        Y  < wpag
100200991014     c                   if        Almenouno = *on
100300980930     C                   EXSR      RIES1
100400980930     C                   ADD       1             S1NRR
100500980930     C                   ADD       1             Y
100600980930     C                   WRITE     C199S1
100700991014     c                   endif
100800980930     C                   EXSR      REDANA
100900981001     C* Posizionamento su NDpas per la partita letta
101000981001     C     $EndFile      IFEQ      *OFF
101100981001     c     $rcdok        andeq     *on
101200981001     C                   EXSR      SETPAS
101300981001     c                   endif
101400981001     c                   endif
101500981001     c*
101600981001  1e C                   ENDDO
101700981001     C*
101800981001     c                   exsr      FinePag
101900981001     C*
102000981001     C                   ENDSR
102100980930     C*
102200981001     C/EJECT
102300981001     C************************************************************
102400981001     C* Operazioni da eseguire se arrivati a fine lista
102500981001     C************************************************************
102600981001     C     FinePag       BEGSR
102700981001     C*
102800980930     C                   Z-ADD     S1NRR         WMAX                 30
102900980930     C*
103000980930     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
103100980930     C*
103200980930     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
103300011204D1023X***                MVR                     RESTO             3 0
103400011204D1023C                   MVR                     RESTO             4 0
103500980930     C     PAGINE        MULT      SFLPAG        C1RCD
103600980930     C     RESTO         IFGT      0
103700980930     C                   ADD       1             C1RCD
103800980930     C                   ELSE
103900980930     C                   SUB       SFLPAG        C1RCD
104000980930     C                   ADD       1             C1RCD
104100980930     C                   ENDIF
104200980930     C*
104300980930     C                   ENDSR
104400980930     C/EJECT
104500980930     C************************************************************
104600980930     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
104700980930     C************************************************************
104800980930     C     SETANA        BEGSR
104900980930     C*
105000980930     C                   MOVE      XSCSOC        PPASOCIETA
105100980930     C                   MOVE      CTB199        PPActb
105200980930     C                   MOVE      KCC199        PPAKCC
105300980930     C                   MOVE      KSC199        PPAKSC
105400980930     C*
105500980930     C                   SELECT
105600980930     C*
105700980930     C* Gestione posizionamento
105800980930     C* Se il campo W1POS è > *blank imposto il posizionamento con
105900980930     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
106000980930     C*
106100980930    >C* Ord.1 = chiave ordinamento 1 (discendente)
106200980930     C     $ORD          WHENEQ    1
106300980930     C*
106400980930   B2C     Dtpera199     IFEQ      datmin
106500980930    >C                   MOVE      DATMAX        ppadtpar
106600980930     C                   ELSE
106700980930    >C                   MOVE      DTPERa199     ppadtpar
106800980930   E2C                   ENDIF
106900980930     C*
107000980930     C* Se chieste partite solo aperte e se la data limite registrazione
107100980930     C* movimenti definitivi è > o uguale a oggi uso la vista logica che
107200980930     c* omette le partite chiuse
107300980930     C                   if        sitpar199 <> '1' and dtregd199 >= wdate
107400980930    >C     K05PPA01      SETGT     NDPPA03
107500980930     C                   else
107600980930    >C     K05PPA01      SETGT     NDPPA01
107700980930     C                   endif
107800980930     c*
107900980930    >C* Ord.2 = chiave ordinamento 2 (ascendente)
108000980930     C     $ORD          WHENEQ    2
108100980930   B2C     Dtperda199    IFEQ      datmin
108200980930    >C                   MOVE      DATMIN        ppadtpar
108300980930     C                   ELSE
108400980930    >C                   MOVE      DTPERDA199    ppadtpar
108500980930   E1C                   ENDIF
108600980930     C* Se chieste partite solo aperte e se data limite registrazioni
108700980930     C* definitive è >= oggi, uso la vista logica che omette partite chiuse
108800980930     C                   if        sitpar199 <> '1' and dtregd199 >= wdate
108900980930    >C     K05PPA01      SETLL     NDPPA03
109000980930     C                   else
109100980930    >C     K05PPA01      SETLL     NDPPA01
109200980930     C                   endif
109300980930     c*
109400980930     C                   ENDSL
109500980930     C*
109600980930     C                   ENDSR
109700980930     C/EJECT
109800980930     C************************************************************
109900980930     C* POSIZIONAMENTO RCD MOvimenti
110000980930     C************************************************************
110100980930     C     SETPAS        BEGSR
110200981001     C*
110300991014     c                   clear                   AlmenoUno
110400981001     C                   MOVE      *OFF          $EndFile1
110500981001     C*
110600980930     C                   MOVE      XSCSOC        PASSOCIETA
110700980930     C                   MOVE      CTB199        PASctb
110800980930     C                   MOVE      KCC199        PASKCC
110900980930     C                   MOVE      KSC199        PASKSC
111000980930     C                   MOVE      ppadtpar      PASDTPAR
111100980930     C                   MOVE      ppanrpar      PASNRPAR
111200980930     C                   MOVE      ppaserpar     PASSERPAR
111300980930     C*
111400980930    >C     K07PAS02      SETLL     NDPAS02J
111500980930     C*
111600980930     c                   ENDSR
111700980930     C/EJECT
111800980930     C************************************************************
111900980930     C* LETTURA RCD ARCHIVIO PILOTA
112000980930     C************************************************************
112100980930     C     REDANA        BEGSR
112200980930     C*
112300980930     C                   MOVEL     *OFF          $EndFile
112400980930     C                   MOVEL     *OFF          $RCDOK
112500980930     C*
112600980930     C     $EndFile      DOUEQ     *ON
112700980930     C     $RCDOK        OREQ      *ON
112800980930     C*
112900980930     C                   SELECT
113000980930     C* Discendente
113100980930     C     $ORD          WHENEQ    1
113200980930     C* Solo aperte
113300980930     C                   if        sitpar199 <> '1' and dtregd199 >= wdate
113400980930    >C     K04PPA01      READPE    NDPPA03                                21
113500980930     C                   else
113600980930    >C     K04PPA01      READPE    NDPPA01                                21
113700980930     C                   endif
113800980930     C* Ascendente
113900980930     C     $ORD          WHENEQ    2
114000980930     C* Solo aperte
114100980930     C                   if        sitpar199 <> '1' and dtregd199 >= wdate
114200980930    >C     K04PPA01      READE     NDPPA03                                21
114300980930     C                   else
114400980930    >C     K04PPA01      READE     NDPPA01                                21
114500980930     C                   endif
114600980930     C                   ENDSL
114700980930     C*
114800980930     C                   MOVE      *IN21         $EndFile
114900980930     C*
115000980930     C     $EndFile      IFEQ      *OFF
115100980930     C                   EXSR      SELANA
115200980930     C                   ENDIF
115300980930     C*
115400980930     C                   ENDDO
115500980930     C*
115600980930     C                   MOVE      *IN21         *IN33
115700980930     C*
115800980930     C                   ENDSR
115900980930     C/EJECT
116000980930     C************************************************************
116100980930     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
116200980930     C************************************************************
116300980930     C     CLCPAG        BEGSR
116400980930     C* Input :
116500980930     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
116600980930     C*          cursore
116700980930     C* - SFLPAG = numero rcd per pagina sfl
116800980930     C* Output :
116900980930     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
117000980930     C*
117100980930     C     WSFL          DIV       SFLPAG        PAGINE            4 0
117200011204D1023X***                MVR                     RESTO             3 0
117300011204D1023C                   MVR                     RESTO
117400980930     C     RESTO         IFGT      0
117500980930     C                   ADD       1             PAGINE
117600980930     C                   ENDIF
117700980930     C     PAGINE        MULT      SFLPAG        WPAG
117800980930     C*
117900980930     C                   ENDSR
118000980930     C/EJECT
118100980930     C************************************************************
118200980930     C* SELEZIONI SULLA RIGA LETTA
118300980930     C* impostare a *OFF il campo $RCDOK per scartare un record che
118400980930     C* non rientra nei parametri di parzializzazione
118500980930     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
118600980930     C* superiore stabilito nei parametri
118700980930     C*
118800980930     C************************************************************
118900980930     C     SELANA        BEGSR
119000980930     C*
119100980930     C                   MOVEL     *ON           $RCDOK
119200980930A1211X*                  MOVEL     *ALL'9'       ALL9             15
119300980930A1211C                   MOVEL     *hival        ALL9             15
119400980930     C*
119500980930     C* Se cliente/fornitore (sottonatura = C o F) leggo file estensione
119600980930     C                   IF        snatura199 = 'C' or snatura199 = 'F'
119700980930     C     K02PPA01      CHAIN     NDPPS01                            23
119800980930     C                   ENDIF
119900980930     C*
120000980930     C                   SELECT
120100980930     C*
120200980930     C* SELEZIONI RISPETTO AI VALORI RCD
120300980930     C*
120400980930     C* Scartare i record che non rispondono alle selezioni effettuate
120500980930     C* esempio : esclusione degli annullati.
120600980930     c*
120700980930     C* Unità diversa
120800980930     C
120900980930     C                   When      Unita199 <> *blanks and
121000980930     C                             PPAUnita <> Unita199
121100980930     C                   move      *off          $RCDok
121200980930     c*
121300980930     C* Periodo
121400980930     C                   When      PPaDtpar <  DtperDa199 or
121500980930     C                             PPaDtpar >  DtperA199
121600980930     C                   move      *off          $RCDok
121700980930     c*
121800980930     C* Divisa selezionata (includi)
121900980930     C                   When      DivSel199 = 'I' and divisa199 <> *blanks
122000980930     C                             and PPaDivisa <>  divisa199
122100980930     C                   move      *off          $RCDok
122200980930     c*
122300980930     C* Divisa selezionata (ometti)
122400980930     C                   When      DivSel199 = 'O' and divisa199 <> *blanks
122500980930     C                             and PPaDivisa =  divisa199
122600980930     C                   move      *off          $RCDok
122700980930     c*
122800980930     C* Richieste parzializzazioni su pps, ma pps non trovato
122900980930     C                   When      (CtgAn1199 <> *blanks or
123000980930     C                             CtgAn2199 <> *blanks or
123100980930     C                             CtgAn3199 <> *blanks or
123200980930     C                             kscage199 <> *blanks )
123300980930     C                              and *in23 =*on
123400980930     C                   move      *off          $RCDok
123500980930     c*
123600980930     C* Categorie di partita
123700980930     C* di pps
123800980930     C                   When      CtgAn1199 <> *blanks and
123900980930     C                             PpsCTgana1 <> CtgAn1199
124000980930     C                   move      *off          $RCDok
124100980930     c*
124200980930     C                   When      CtgAn2199 <> *blanks and
124300980930     C                             PpsCTgana2 <> CtgAn2199
124400980930     C                   move      *off          $RCDok
124500980930     C*
124600980930     C                   When      CtgAn3199 <> *blanks and
124700980930     C                             PpsCTgana3 <> CtgAn3199
124800980930     C                   move      *off          $RCDok
124900980930     c*
125000980930     C* Agente
125100980930     C                   When      KscAge199 <> *blanks and
125200980930     C                             PpsKscAge <> KscAge199
125300980930     C                   move      *off          $RCDok
125400980930     c*
125500980930     C* Blocco
125600980930     C                   When      Bkamm199  <> *blanks
125700980930     c*
125800980930     C* tutte le bloccate (devono esistere su ndpps)
125900980930     C                   if        Bkamm199  = '* '
126000980930     C                   if        (PpsBkamm  = *blanks and *in23 = *off)
126100980930     C                             or *in23 = *on
126200980930     C                   move      *off          $RCDok
126300980930     C                   endif
126400980930     C                   endif
126500980930     C* solo le non bloccate (tutte le aperte, anche quelle non trovate
126600980930     C* su ndpps)
126700980930     C                   if        Bkamm199  = '**' and
126800980930     C                             ppsBkamm  <> *blanks and *in23 = *off
126900980930     C                   move      *off          $RCDok
127000980930     C                   endif
127100980930     C* Codice blocco preciso (deve esistere su ndpps)
127200980930     C                   if        Bkamm199 <> '* ' and
127300980930     C                             bkamm199 <> '**'
127400980930     C                   if        (ppsBkamm  <> bkamm199 and *in23 = *off)
127500980930     C                             or *in23 = *on
127600980930     C                   move      *off          $RCDok
127700980930     C                   endif
127800980930     C                   endif
127900980930     C*
128000980930     C* Factor
128100980930     C                   When      KccFact199 <> *Blank and
128200980930     C                             KscFact199 <> *Blank
128300980930     C                   If        ((PPSKccFact <> KccFact199 or
128400980930     C                               PPSKscFact <> KscFact199) and
128500980930     C                              *In23 = *Off)
128600980930     C                             or *In23 = *On
128700980930     C                   Move      *Off          $RcdOK
128800980930     C                   EndIf
128900980930     C*
129000980930     C                   ENDSL
129100991129D0329C*
129200991129  "  C* scarto il rcd se
129300991129  "  C*  la partita è chiusa e
129400991129  "  C                   IF        PPAParChiu = *on        and
129500991129  "  C*  la data di chiusura è < o = alla data limite registrazione
129600991129  "  C*   definitiva e
129700991129  "  C                             PPADtChiu <= DtRegD199  and
129800991129  "  C*  non sono state chieste tutte le partite
129900991129  "  C                             SitPar199 <> '1'
130000991129  "  C                   MOVE      *Off          $RcdOK
130100991129D0329C                   ENDIF
130200980930     C*
130300980930     C*  chiamo routine  per altre selezioni
130400980930     C*  se selezioni precedentI  sono ok e se la partita non è chiusa
130500980930     C*  perchè in questo caso non mi servono i dati di x20 perchè sono
130600980930     C*  tutti a 0
130700980930     C                   clear                   WtotDare
130800980930     C                   clear                   WtotAvere
130900980930     C                   clear                   WtotDared
131000980930     C                   clear                   WtotAvereD
131100980930     c*
131200981020     c* se record ok e partita non chiusa
131300980930     C                   if        $rcdok = *on and (ppaparchiu <> '1'
131400981028     c                             or  ppadtchiu > dtregd199)
131500980930     C                   exsr      riex20scad
131600991129D0329C* in pratica qui una partita chiusa arriva solo se richieste
131700991129  "  C* tutte le partite, in quanto il test che la partita sia aperta o
131800991129  "  C* almeno con data chiusura successiva alla data limite registraz.
131900991129D0329C* definitiva è già stato fatto come elemento di selezione
132000980930     C                   CALL      'X20SCADR'
132100980930     C                   PARM                    X20SCADDS
132200980930     C                   PARM                    X20SCADXDS
132300980930     C* Se tutto ok
132400980930     C                   if        x20errori = ' ' and
132500980930     c                             x20nrmov <> 0
132600980930     C* calcolo totale dare e avere sia in moneta di conto che divisa della
132700980930     C* partita
132800980930     C* mc
132900980930     C                   add       x20tdld       WtotDare
133000980930     C                   add       x20tdlp       WtotDare
133100980930     C                   add       x20tdlg       WtotDare
133200980930     C                   add       x20tald       WtotAvere
133300980930     C                   add       x20talp       WtotAvere
133400980930     C                   add       x20talg       WtotAvere
133500980930     C* divisa
133600980930     C                   add       x20tddd       WtotDareD
133700980930     C                   add       x20tddp       WtotDared
133800980930     C                   add       x20tddg       WtotDareD
133900980930     C                   add       x20tadd       WtotAvereD
134000980930     C                   add       x20tadp       WtotAvereD
134100980930     C                   add       x20tadg       WtotAvereD
134200980930     C* Controlli situazione partita
134300980930     C                   EXSR      CtrSitPar
134400980930     C                   else
134500980930     C                   eval      $rcdok = *off
134600980930     C                   endif
134700980930     C                   endif
134800980930     C*
134900980930     C                   ENDSR
135000980930     C/EJECT
135100980930     C************************************************************
135200980930     C* controlli situazione partita dopo chiamata a X20SCADR
135300980930     C************************************************************
135400980930     C     CtrSitPar     BEGSR
135500980930     C*
135600980930     C* testo flag situazione partita richiesto sui risultati avuti da
135700980930     C* x20scadr
135800980930     C                   select
135900980930     C* Aperte/ a rischio
136000980930     C                   When      Sitpar199  = '0'
136100980930     C* Sono aperte se dare <> da avere  in moneta di conto def e gestionale
136200980930     C* Sono aperte se dare <> da avere  in divisa          def e gestionale
136300980930     C* se dare provvisorio <> 0 o avere provvisorio <> 0 in moneta di conto
136400980930     C* se dare provvisorio <> 0 o avere provvisorio <> 0 in divisa
136500980930     C* ese uno dei campi scaduto, a scadere, a rischio,
136600980930     C* portafoglio, é <> 0
136700980930     C                   if        x20tdld <> x20tald or
136800980930     C                             x20tdlg <> x20talg or
136900980930     C                             x20tddd <> x20tadd or
137000980930     C                             x20tddg <> x20tadg or
137100980930     C                             x20tdlp <> 0       or
137200980930     C                             x20talp <> 0       or
137300980930     C                             x20tddp <> 0       or
137400980930     C                             x20tadp <> 0       or
137500980930     C                             X20LScadu <> 0 or x20lascad <> 0 or
137600980930     C                             X20Risc <> 0 or X20Portaf <> 0
137700980930     C                   move      *on           $RCDok
137800980930     C                   else
137900980930     C                   move      *off          $RCDok
138000980930     C                   endif
138100980930     c* Aperte dare (testo solo moneta di conto)
138200980930     C* sono aperte in dare se dare <> da avere e dare > avere
138300980930     C                   When      Sitpar199  = '2'
138400980930     C                   if        WtotDare <> WtotAvere and
138500980930     C                             WtotDare >  WtotAvere
138600980930     C                   move      *on           $RCDok
138700980930     C                   else
138800980930     C                   move      *off          $RCDok
138900980930     C                   endif
139000980930     c* Aperte avere (testo solo moneta di conto)
139100980930     C* sono aperte in avere se dare <> da avere e dare < avere
139200980930     C                   When      Sitpar199  = '3'
139300980930     C                   if        WtotDare <> WtotAvere and
139400980930     C                             WtotDare <  WtotAvere
139500980930     C                   move      *on           $RCDok
139600980930     C                   else
139700980930     C                   move      *off          $RCDok
139800980930     C                   endif
139900980930     c* Aperte dare o avere (come rischiesta 0 senza però verifica rischio)
140000980930     C                   When      Sitpar199  = '4'
140100980930     C                   if        WtotDare <> WtotAvere or
140200980930     C                             WtotDareD <> WtotAvered
140300980930     C                   move      *on           $RCDok
140400980930     C                   else
140500980930     C                   move      *off          $RCDok
140600980930     C                   endif
140700980930     C*
140800980930     C                   endsl
140900980930     c*
141000991129D0329C************* spostato nella routine SELANA
141100991129  "  C**Testo se partita effettivamente aperta(se richiesto) nel caso in
141200991129  "  C**cui la data limite registrazioni definitive sia inferiore a udate
141300991129  "  C*****              if        $rcdok = *on  and dtregd199 < wdate and
141400991129  "  c*****                        sitpar199 <> '1'
141500991129  "  c**è*chiusa, quindi non valida, se la data chiusura partita è inferiore
141600991129  "  C**o*uguale alla data limite registrazioni
141700991129  "  c*****              if        ppadtchiu <= dtregd199 and ppaparchiu = *on
141800991129  "  C*****              move      *off          $RCDok
141900991129  "  c*****              endif
142000991129  "  c*****              endif
142100991129D0329C*************
142200980930     c*
142300980930     C                   ENDSR
142400980930     C/EJECT
142500980930     C************************************************************
142600981002     C* RIEMPIMENTO RIGA partita
142700980930     C************************************************************
142800980930     C     RIES1         BEGSR
142900980930     C*
143000980930     C                   CLEAR                   C199S1
143100981001     C*
143200981002     C                   EVAL      h1tiporiga = 'P'
143300981001     c                   move      '/'           DsBarra1
143400981001     c                   move      '/'           DsBarra2
143500980930     C*
143600981002     C* Dati partita
143700980930     C                   MOVE      PPASYS        H1SYS
143800980930     C                   MOVE      PPANRASINT    H1NRASINT
143900010119c1460C                   z-add     PPANRASGen    H1NRASGen
144000980930     C                   CALLb     'XDT4000'
144100980930     C                   PARM      PPADTPAR      XDTAMG           10
144200980930     C                   PARM                    XDTGMA            6 0
144300980930     C                   PARM      2             XDTSTA            1 0
144400981001     C                   PARM                    XDTANN            4 0
144500981001     C                   PARM                    XDTMES            2 0
144600981001     C                   PARM                    XDTGIO            2 0
144700981001     C                   MOVE      XDTGio        DsGGpar
144800981001     C                   MOVE      XDTMes        DsMMpar
144900981001     C                   MOVE      XDTANN        DsAApar
145000990608     C                   eval      h1dtpar = XdtGma
145100981007     c* tolgo gli zeri non significativi
145200981007     C                   z-add     PpaNrPar      $SrcVar
145300981007     C                   Eval      $SrcVarDe = 0
145400981007     C                   Eval      $EdtCode  = 'Z'
145500981007     C                   Eval      $LenFld   = 9
145600981007     C                   CallB     'X23EDITR'
145700981007     C                   Parm                    $SrcVar
145800981007     C                   Parm                    $SrcVarPr
145900981007     C                   Parm                    $SrcVarDe
146000981007     C                   Parm                    $EdtCode
146100981007     C                   Parm                    $LenFld
146200981007     C                   Parm      *blanks       $RcvrVar
146300981007     C                   Parm                    $Err
146400981007     C                   eval      dsnrpar = $RcvrVar
146500990608     C                   eval      h1nrpar = Ppanrpar
146600990608     C                   eval      h1serpar= Ppaserpar
146700991014     C                   eval      dsserpar= Ppaserpar
146800981002     C*
146900981002     C                   eval      s1unita = ppaunita
147000980930     c* divise di partita
147100981009     C                   EVAL      H1Divisa1  = ppadivisa
147200981009     C                   EVAL      H1Divisa2  = xscdiv
147300981002     C*
147400981002     C* Campi da NDPPS
147500981002     C                   if        *in23 = *off
147600981002     C                   CALLb     'XDT4000'
147700981002     C                   PARM      PPSDTDoc      XDTAMG
147800981002     C     S1DTDoc       PARM                    XDTGMA
147900981002     C                   PARM      2             XDTSTA
148000981002     C                   EVAL      S1nrDOC = PPSnrDOC
148100981002     C                   EVAL      S1serdoc = PPSserDOC
148200981023     c* agente
148300981023     c                   if        ppskscage <> *blanks
148400981023     C                   CALLB     'XCAPCLIFOR'
148500981023     C                   PARM                    Xscsoc
148600981023     C                   PARM      'F'           wkcc
148700981023     C                   PARM                    ppskscage
148800981023     C                   eval      s1kscage = ppskscage
148900981023     C                   eval      s1kccage = wkcc
149000981023     C* decodifica agente
149100981023     C                   EVAL      Wksc  = s1kscage
149200981023     C                   EXSR      CALLMVC002
149300981023     c                   if        esito = *off
149400981023     C                   eval      s1aged   = desbrev002
149500981023  1  c                   endif
149600981023  1  c                   endif
149700981002     c                   endif
149800980930     C*
149900980930     C* Campi da X20SCADR se lo avevo chiamato
150000980930     C* + calcolo totale saldo se lo deve fare
150100980930     c*
150200980930     c                   IF        ppaparchiu <> '1' or
150300981028     c                             ppadtchiu >  dtregd199
150400980930     c*
150500980930     C                   exsr      calcsaldo
150600980930     c                   endif
150700980930     C*
150800980930     c* Campi variabili a seguito richiesta moneta di conto o divisa
150900980930     C                   SELECT
151000980930     C* divisa
151100980930     C                   when      SavDivVis = '1'
151200981009     C                   EVAL      dsdivisa   = H1divisa1
151300981009     C                   EVAL      s1divisa2  = H1divisa2
151400980930     C                   eval      s1saldo1   = H1saldod
151500980930     C                   eval      s1segno1   = H1segnod
151600980930     C                   eval      s1saldo2   = H1saldom
151700980930     C                   eval      s1segno2   = H1segnom
151800980930     C* moneta di conto
151900980930     C                   when      SavDivVis = '0'
152000981009     C                   EVAL      dsDivisa   = h1divisa2
152100981009     C                   EVAL      s1divisa2  = H1divisa1
152200980930     C                   eval      s1saldo1   = H1saldom
152300980930     C                   eval      s1segno1   = H1segnom
152400980930     C                   eval      s1saldo2   = H1saldod
152500980930     C                   eval      s1segno2   = H1segnod
152600980930     C                   endsl
152700980930     C*
152800980930     C* imposto quegli attributi del sfl che sono modificabili
152900980930     C* al richiamo del determinato pgm
153000980930     C                   EXSR      ATRS1
153100980930     C*
153200980930     C                   ENDSR
153300980930     C/EJECT
153400980930     C************************************************************
153500980930     C* Lettura righe della partita
153600980930     C************************************************************
153700980930     C     REDPAS        BEGSR
153800980930     c*
153900980930     C                   MOVEL     *OFF          $RCDOK1
154000981001     C                   MOVEL     *OFF          $EndFile1
154100980930     C*
154200980930     C     $EndFile1     DOUEQ     *ON
154300980930     C     $RCDOK1       OREQ      *ON
154400980930     C*
154500980930    >C     K07PAS02      READE     NDPAS02J                               21
154600980930     C*
154700980930     C                   MOVE      *IN21         $EndFile1
154800980930     C*
154900980930     C     $EndFile1     IFEQ      *OFF
155000980930     C                   EXSR      SELANA1
155100980930     C                   ENDIF
155200980930     C*
155300980930     C                   ENDDO
155400980930     C*
155500980930     c                   endsr
155600980930     C/EJECT
155700980930     C************************************************************
155800980930     C* SELEZIONI SULLA RIGA LETTA di ndpas
155900980930     C* impostare a *OFF il campo $RCDOK per scartare un record che
156000980930     C* non rientra nei parametri di parzializzazione
156100980930     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
156200980930     C* superiore stabilito nei parametri
156300980930     C*
156400980930     C************************************************************
156500980930     C     SELANA1       BEGSR
156600980930     C*
156700980930     C                   MOVEL     *ON           $RCDOK1
156800980930A1211X*                  MOVEL     *ALL'9'       ALL9             15
156900980930A1211C                   MOVEL     *hival        ALL9             15
157000980930     C*
157100980930     C* SELEZIONI RISPETTO AI VALORI RCD
157200980930     C*
157300980930     C* Scartare i record che non rispondono alle selezioni effettuate
157400980930     C* esempio : esclusione degli annullati.
157500980930     C                   SELECT
157600980930     C* Provvis, definit, gestionali
157700980930     C*
157800980930     C                   WHEN      (Movdef199 = *off   and
157900980930     C                             pastpregz = 'D') or
158000980930     C                             (movprov199 = *off   and
158100980930     C                             pastpregz = 'P') or
158200980930     C                             (movges199  = *off   and
158300980930     C                             pastpregz = 'G')
158400980930     C                   eval      $rcdok1= *off
158500980930     C* date limite registrazione P, G, D
158600981001     C                   WHEN      (Movdef199 =*on and pasdtreg > dtregD199 and
158700980930     C                              Pastpregz = 'D') or
158800981001     C                             (Movprov199 =*on and pasdtreg > dtregP199 and
158900980930     C                              Pastpregz = 'P') or
159000981001     C                             (Movges199 =*on and pasdtreg > dtregG199  and
159100980930     C                              Pastpregz = 'G')
159200980930     C                   eval      $rcdok1= *off
159300980930     C*
159400980930     C                   ENDSL
159500980930     C*
159600980930     C                   ENDSR
159700980930     C/EJECT
159800980930     C************************************************************
159900981002     C* RIEMPIMENTO RIGA movimenti
160000980930     C************************************************************
160100980930     C     RIES1A        BEGSR
160200980930     C*
160300980930     C                   CLEAR                   C199S1
160400981002     C                   EVAL      h1tiporiga = 'M'
160500981002     C                   eval      h1sys   =     passys
160600981002     C                   eval      h1nrasreg =   pasnrasreg
160700981002     C                   eval      h1nrrigam =   movnrrigam
160800981002     C                   MOVE      pasnrriga     H1NRrigaP
160900981002     c
161000981001     c* data libro giornale
161100981001     C                   CALLb     'XDT4000'
161200090504>>>>>C                   PARM      PasDtReg      XDTAMG
161300981001     C     S1DTlgio      PARM                    XDTGMA
161400981001     C                   PARM      2             XDTSTA
161500981001     c* Causale riga
161600981001     C* decodifica causale (se diversa dalla precedente)
161700981001     C                   eval      DsCaus    = pascausrig
161800981002   1 C                   if        pascausrig <> Savcausrig
161900981001     C                   eval      savcausrig = pascausrig
162000981001     C                   EXSR      CALLMVC003
162100981002   2 C                   IF        Esito = *off
162200981001     C                   EVAL      DsCausD    = OPEdesbrev
162300981002   2 C                   ENDIF
162400981001     C                   else
162500981001     C                   EVAL      DsCausD    = OPEdesbrev
162600981002   1 C                   endif
162700981001     c* registrazione
162800981002     C                   CALLb     'XDT4000'
162900981002     C                   PARM      pasDTreg      XDTAMG
163000981002     C     S1DTreg       PARM                    XDTGMA
163100981002     C                   PARM      2             XDTSTA
163200981002     C                   MOVE      RegNRreg      S1NRreg
163300981002     C                   eval      s1serreg = regserreg
163400981001     c*Segnalo solo le registrazioni non definitive, per queste lascio blank
163500981002   1 C                   IF        Pastpregz <> 'D'
163600981009     C                   eval      dstpregz  = pastpregz
163700981002   1 C                   endif
163800981002     c* documento
163900981002     C                   CALLb     'XDT4000'
164000981002     C                   PARM      MovDTdoc      XDTAMG
164100981002     C     S1DTdoc       PARM                    XDTGMA
164200981002     C                   PARM      2             XDTSTA
164300981002     C                   eval      s1nrdoc = movnrdoc
164400981002     C                   eval      s1serdoc = movserdoc
164500981001     c* scadenza
164600981001     C                   CALLb     'XDT4000'
164700981001     C                   PARM      pasDTsca      XDTAMG
164800051215c1892C     H1DTsca       PARM                    XDTGMA
164900051215c1892x***  S1DTsca       PARM                    XDTGMA
165000981001     C                   PARM      2             XDTSTA
165100981001     c*
165200981001     C* segnalo che esistono altri movimenti  di ndmov
165300981002   1 C                   if        movimporto <> pasimporto
165400981001     C                   EVAL      S1MOVPAS = '>'
165500981002   1 C                   endif
165600981002     c* Importo coge
165700981002     C                   eval      s1impcoged = movimportd
165800981001     c* documento
165900981001     C                   CALLb     'XDT4000'
166000981001     C                   PARM      MovDTdoc      XDTAMG
166100981001     C     S1DTdoc       PARM                    XDTGMA
166200981001     C                   PARM      2             XDTSTA
166300981001     C                   eval      s1nrdoc = movnrdoc
166400981002     c*
166500981002     C* Ci può essere o mdpag o mdrd
166600981002  1  C                   IF        PASmdpag <> ' '
166700981002     C                   eval      s1mdpag = PASMdpag
166800981002  1  C                   endif
166900981002  1  C                   IF        PASmdrd  <> ' '
167000981002     C                   eval      s1mdpag = PASMdrd
167100981002  1  C                   endif
167200981002     C*
167300981002     C                   eval      s1divcoge = movdivisa
167400981002     C                   eval      s1Cambio   = movcambio
167500981002     C                   CALLb     'XDT4000'
167600981002     C                   PARM      pasDTflu      XDTAMG
167700051215C1892X***  S1DTflu       PARM                    XDTGMA
167800051215C1892C     H1DTflu       PARM                    XDTGMA
167900981002     C                   PARM      2             XDTSTA
168000981002     C                   CALLb     'XDT4000'
168100981002     C                   PARM      movdtcoco     XDTAMG
168200981002     C     S1DTcoco      PARM                    XDTGMA
168300981002     C                   PARM      2             XDTSTA
168400981002     C                   eval      s1unita = pasunita
168500981002     C                   eval      s1kcccpu = movkcccpu
168600981002     C                   eval      s1ksccpu = movksccpu
168700051216     c*
168800051215C1892C* Rimpio i campi S1DtSca e S1dtFlu a seconda della richiesta di
168900051215  "  c*visualizzazione
169000051215  "  c                   if        dataSf199=*blanks
169100051215  "  c                   eval      S1dtSca=H1Dtsca
169200051216  "  c                   eval      IdMsg= $Cost(1)
169300051216  "  C                   EXSR      RtvMsg
169400051215  "  c                   eval      S1dtFlu=H1DtFlu
169500051215  "  c                   else
169600051215  "  c                   eval      S1dtSca=H1DtFlu
169700051216  "  c                   eval      IdMsg= $Cost(2)
169800051216  "  C                   EXSR      RtvMsg
169900060419C1892c                   eval      S1dtFlu=H1DtSca
170000060419D1984c**se non c'è la data flusso , ci metto la data scadenza
170100060419  "  c                   if        S1Dtsca=0
170200060419  "  c                   eval      S1dtSca=H1DtSca
170300060419D1984c                   endif
170400051215C1892c                   endif
170500981002     C* decodifica contropartita
170600981002  1  c                   if        s1kcccpu <> *blanks and s1ksccpu <>
170700981002     c                             *blanks
170800981002     C                   EVAL      Wkcc  = s1kcccpu
170900981002     C                   EVAL      Wksc  = s1ksccpu
171000981002     C                   EXSR      CALLMVC002
171100981002     c                   if        esito = *off
171200981002     C                   eval      s1ksccpud= desbrev002
171300981002  1  c                   endif
171400981002  1  c                   endif
171500981002     C                   eval      s1desbrev= movdesbrev
171600981002     C*
171700981002     C* causale testata(trovo decodifica solo se diversa da causale riga)
171800981002     C                   eval      s1caustes= movcaustes
171900981002  1  C                   if        movcaustes <> Savcausrig
172000981002     C                   eval      savcausrig = movcaustes
172100981002     C                   EXSR      CALLMVC003
172200981002  2  C                   IF        Esito = *off
172300981002     C                   EVAL      s1CaustesD = OPEdesbrev
172400981002     C                   EVAL      portafh    = OPEportaf
172500031120D1498C                   EVAL      tpdocih    = OPETpDoci
172600981002  2  C                   ENDIF
172700981002     C                   else
172800981002     C                   EVAL      S1CausTesD = OPEdesbrev
172900981002     C                   EVAL      portafh    = OPEportaf
173000031120D1498C                   EVAL      tpdocih    = OPETpDoci
173100981002  1  C                   endif
173200981002     C*
173300981002     C                   eval      s1classe = regclasse
173400981002     C                   eval      s1utente = reguteulmo
173500981009     c                   eval      h1divisa1  = pasdivisa
173600981009     c                   eval      h1divisa2  = xscdiv
173700981001     C*
173800981001     C* Calcola importi tramite routine edit
173900981002     C                   exsr      CalcImport
174000981001     C*
174100981001     c* Imposta importi a seguito richiesta moneta di conto o divisa
174200981001     C                   SELECT
174300981001     C* divisa
174400981001     C                   when      SavDivVis = '1'
174500981001     C                   eval      s1saldo1   = H1saldoD
174600981001     C                   eval      s1segno1   = H1segnod
174700981001     C                   eval      s1saldo2   = H1saldoM
174800981001     C                   eval      s1segno2   = H1segnoM
174900981001     C* moneta di conto
175000981001     C                   when      SavDivVis = '0'
175100981001     C                   eval      s1saldo1   = H1saldoM
175200981001     C                   eval      s1segno1   = H1segnoM
175300981001     C                   eval      s1saldo2   = H1saldoD
175400981001     C                   eval      s1segno2   = H1segnoD
175500981001     C                   endsl
175600981001     C*
175700981001     C* se le divise sono uguali, imposto il valore del campo in drop
175800981001     C* uguale a quello della prima riga, così almeno sono allineati
175900981001     C                   IF        pasdivisa = xscdiv
176000981001     C                   eval      s1saldo2  = s1saldo1
176100981001     C                   endif
176200030206D1270C*
176300030206  "  C* Testo esistenza documenti catalogati su Laguna (se questo
176400030206  "  C*  attivo)
176500030206  "  C                   IF        xscLaguna = *on
176600030206  "  C                   EXSR      DriverLGN
176700030206  "  C*
176800030206  "  C                   EVAL      H1ClassLg = *blanks
176900030206  "  C                   EVAL      H1IdLgn = 0
177000030206  "  C                   IF        Errore = *off
177100030206  "  C                   EVAL      ANLGN00F = Dati
177200030206  "  C* salvo la chiave univoca per richiamare Laguna
177300030206  "  C                   EVAL      H1ClassLg = LGNClassLg
177400030206  "  C                   EVAL      H1IdLgn   = LGNIdLgn
177500030206  "  C                   ENDIF
177600030206  "  C*
177700030206D1270C                   ENDIF
177800981002     C*
177900981002     C* imposto quegli attributi del sfl che sono modificabili
178000981002     C* al richiamo del determinato pgm
178100981002     C                   EXSR      ATRS1
178200980930     C*
178300980930     c                   endsr
178400051216c1892C************************************************************
178500051216     C* Chiamata a xrtvm
178600051216c1892C************************************************************
178700051216     C     RtvMsg        BEGSR
178800051216     C*
178900051216     C                   CALLB     'XRTVM'
179000051216     C                   PARM                    IDMSG            27
179100051216     C                   PARM                    TXTMSG          100
179200051216     C                   PARM                    ERRMSGRTV         1
179300051216     C                   PARM      *OFF          CTRMSG            1            centratura
179400051216     C                   PARM                    LENMSG            3 0          lun output.
179500051216     C     ERRMSGRTV     IFNE      *ON
179600051216     C                   MOVEL     TxTMsg        S1CostData                     *
179700051216     C                   ENDIF
179800051216     C*
179900051216C1892C                   ENDSR
180000030206D1270C************************************************************
180100030206  "  C* reperimento dati sui documenti catalogati in Laguna
180200030206  "  C************************************************************
180300030206D1270C     DriverLGN     BEGSR
180400030206     C*
180500030206     C                   Clear                   Dati
180600030206     C                   Clear                   ANLGN00F
180700030206     C*
180800030206     C* se la sottonatura è cliente
180900030206     C                   IF        SNatura199 = 'C'
181000030206     C* imposto le chiavi per fatture attive
181100030206     C                   EVAL      LGNClassPJ = '030'
181200030206     C                   EVAL      LGNClassLg = 'DOC'
181300030206     C                   EVAL      LGNKey101 = xscsoc
181400030206     C                   MOVEL     '0001-01-01'  LGNKey102
181500030206     C* uso i dati di partita perchè quelli di documento non sono
181600030206     C* affidabili in questo pgm (potrei aver registrato prima la
181700030206     C*  nota che la fattura)
181800030206     C                   IF        S1DtDoc <> 0
181900030206     C     *jobrun       MOVEL     S1DtDoc       DataISO
182000030206     C                   MOVEL     DataISO       LGNKey102
182100030206     C                   ENDIF
182200030206     C                   MOVEL     S1NrDoc       LGNKey103
182300030206     C                   EVAL      LGNKey104 = S1SerDoc
182400030206     C                   EVAL      NrChiavi = 6
182500030206     C                   ENDIF
182600030206     C*
182700030206     C* se la sottonatura è fornitore
182800030206     C                   IF        SNatura199 = 'F'
182900030206     C* imposto le chiavi per fatture passive
183000030206     C                   EVAL      LGNClassPJ = '020'
183100030206     C                   EVAL      LGNClassLg = 'DOC'
183200030206     C                   EVAL      LGNKey101 = xscsoc
183300030206     C* non avendo una causale di riferimento non posso capire se si
183400030206     C* tratta di una partita legata ad una fattura o ad una nota
183500030206     C* anticipata, per cui prima testo come se fosse una fattura
183600030206     C                   EVAL      LGNKey102 = 'F'
183700030206     C                   MOVEL     '0001-01-01'  LGNKey103
183800030206     C* uso i dati di partita perchè quelli di documento non sono
183900030206     C* affidabili in questo pgm (potrei aver registrato prima la
184000030206     C*  nota che la fattura)
184100030206     C                   IF        S1DtDoc <> 0
184200030206     C     *jobrun       MOVEL     S1DtDoc       DataISO
184300030206     C                   MOVEL     DataISO       LGNKey103
184400030206     C                   MOVEL     S1NrDoc       LGNKey104
184500030206     C                   EVAL      LGNKey105 = S1SerDoc
184600030206     C                   ENDIF
184700030206     C                   EVAL      LGNKey106 = C1Ksc
184800030206     C                   EVAL      NrChiavi = 8
184900030206     C                   ENDIF
185000030206     C*
185100030206     C                   MOVEL     ANLGN00F      Dati
185200030206     C                   eval      Lunghezza = %size(ANLGN00F)
185300030206     C                   eval      Errore    = *off
185400030206     C*
185500030206     C                   CALLB     'PJDR1701R'
185600030206     C                   parm      '01'          Opzione                        1
185700030206     C                   parm                    Errore                         2
185800030206     C                   parm      2             Vista                          5
185900030206     C                   parm                    Dati                           3
186000030206     C                   parm                    Lunghezza                      4
186100030206     C                   parm      *off          Commit                         6
186200030206     C                   parm                    NrChiavi                       7
186300030206     C                   parm      *off          Alloca                         8
186400030206     C                   parm                    *omit                          9
186500030206     C*
186600030206     C* se ho cercata per una fattura passiva
186700030206     C                   IF        SNatura199 = 'F'
186800030206     C* e non ho trovato nulla
186900030206     C                             and Errore = *on
187000030206     C* cerco chiedendomi se ci sono documenti legati ad una nota
187100030206     C                   EVAL      LGNKey102 = 'N'
187200030206     C                   MOVEL     ANLGN00F      Dati
187300030206     C                   eval      Errore    = *off
187400030206     C                   CALLB     'PJDR1701R'
187500030206     C                   parm      '01'          Opzione                        1
187600030206     C                   parm                    Errore                         2
187700030206     C                   parm      2             Vista                          5
187800030206     C                   parm                    Dati                           3
187900030206     C                   parm                    Lunghezza                      4
188000030206     C                   parm      *off          Commit                         6
188100030206     C                   parm                    NrChiavi                       7
188200030206     C                   parm      *off          Alloca                         8
188300030206     C                   parm                    *omit                          9
188400030206     C                   ENDIF
188500030206     C*
188600030206D1270C                   ENDSR
188700981001     C/EJECT
188800981001     C*----------------------------------------------------------
188900981002     C* Imposta importi riga movimento
189000981001     C*----------------------------------------------------------
189100981002     C     CalcImport    BEGSR
189200981001     C*
189300981001     C* Importo in divisa
189400981001     C* Segno
189500981001     C                   if        pasdare = 1
189600981001     C                   eval      H1segnoD= XscDare
189700981001     C                   else
189800981001     C                   eval      H1segnoD= XscAvere
189900981001     C                   endif
190000981001     C* reperisco decimali se divisa diversa dalla precedente letta
190100981001     C                   if        Pasdivisa <> Savdivisa
190200981001     C                   eval      SavDivisa = PasDivisa
190300981001     C                   Eval      WDivisa =  PasDivisa
190400981001     C                   Exsr      RepDivisa
190500981001     C                   Eval      NrDecD = X01NrDec
190600981001     C                   endif
190700981001     C*
190800981001     C                   EVAL      Wimporto = Pasimportd
190900981001     C                   Z-add     NrDecD        WNrDec
191000981001     C                   Eval      $LenFld = %Size(H1saldoD)
191100981001     C*
191200981001     C* Imposto la dimensione del campo video in modo da allineare
191300981001     C* valute gestite con un numero diverso di decimali
191400981001     C                   Select
191500981001     C                   When      WNrDec = 2
191600981001     C                   Eval      $LenFld = %Size(h1saldoD)   - 1
191700981001     C                   When      WNrDec = 1
191800981001     C                   Eval      $LenFld = %Size(h1saldoD)   - 2
191900981001     C                   When      WNrDec = 0
192000981001     C                   Eval      $LenFld = %Size(h1saldoD)   - 4
192100981001     C                   Other
192200981001     C                   Eval      $LenFld = %Size(h1saldod)
192300981001     C                   Endsl
192400981001     C* allineo dare a sinistra
192500981001     C                   if        H1SegnoD = XscDare
192600981001     C                   SUB       5             $lenfld
192700981001     C                   Endif
192800981001     C                   EXSR      Edit
192900981001     C                   Eval      h1saldoD   = $RcvrVar
193000981001     C*
193100981001     C* moneta di conto
193200981001     C* Segno
193300981001     C                   if        pasdare = 1
193400981001     C                   eval      H1segnoM= XscDare
193500981001     C                   else
193600981001     C                   eval      H1segnoM= XscAvere
193700981001     C                   endif
193800981001     C* Importo
193900981001     C                   Z-add     Pasimporto    WImporto
194000981001     C                   Z-add     NrDec         WNrDec
194100981001     C                   Eval      $LenFld = %Size(h1SaldoM)
194200981001     C* allineo dare a sinistra
194300981001     C                   if        h1segnoM = XscDare
194400981001     C                   SUB       5             $lenfld
194500981001     C                   Endif
194600981001     C                   EXSR      Edit
194700981001     C                   Eval      h1SaldoM   = $RcvrVar
194800981001     C*
194900981001     C                   ENDSR
195000981001     C/EJECT
195100981001     C*----------------------------------------------------------
195200981001     C* Chiamata NDMVC003
195300981001     C*----------------------------------------------------------
195400981001     C     CallMvc003    BEGSR
195500981001     C*
195600981001     C* Controllo causale
195700981001     C                   CLEAR                   ANOPE
195800981001     C                   eval      Data = Datmin
195900981001     C                   EVAL      LenOut = %Size(ANOPE)
196000981001     C                   CallB     'NDMVC003'
196100981001     C                   PARM                    xscsoc
196200981002     C                   PARM                    SavCausrig
196300981001     C                   PARM                    Data
196400981001     C                   PARM                    Uso
196500981001     C                   PARM      *OFF          GesMsg
196600981001     C                   PARM      *OFF          Esito
196700981001     C                   PARM      'ANOPE'       StrutturaO
196800981001     C                   PARM                    ANOPE
196900981001     C                   PARM                    LenOut
197000990121     C                   PARM      '0'           Refresh
197100981001     C*
197200981001     C                   ENDSR
197300980930     C/EJECT
197400980930     C************************************************************
197500980930     C* RIEMPIMENTO DS PER saldo conto
197600980930     C************************************************************
197700980930     C     RIEx20scad    BEGSR
197800980930     C*
197900980930     C                   RESET                   x20scadds
198000980930     C                   RESET                   x20scadxds
198100980930     C                   eval      x20societa = xscsoc
198200980930     C                   eval      x20unita   = unita199
198300980930     C                   eval      x20ctb     = ctb199
198400980930     C                   eval      x20def = movdef199
198500980930     C                   eval      x20prov = movprov199
198600980930     C                   eval      x20gest = movges199
198700980930     C                   eval      x20dtpar   = PPADTPAR
198800980930     C                   eval      x20nrpar   = PPANRPAR
198900980930     C                   eval      x20serpar  = PPASerpar
199000980930     C                   eval      x20dtregmd = dtregd199
199100980930     C                   eval      x20dtregmp = dtregp199
199200980930     C                   eval      x20dtregmg = dtregg199
199300980930     C                   eval      x20kcc     = kcc199
199400980930     C                   eval      x20ksc     = ksc199
199500980930     C                   IF        divsel199 = 'I'
199600980930     C                   eval      x20divr    = divisa199
199700980930     C                   endif
199800980930     C                   eval      x20parsca  = 'S'
199900980930     C                   eval      x20elasca  = 'R'
200000980930     C                   eval      x20DtSca   = dtregd199
200100980930     C                   eval      x20ppa     = 'N'
200200980930     C                   eval      x20lrt     = 'R'
200300980930     C                   eval      x20snatura = snatura199
200400980930     C*
200500980930     C                   ENDSR
200600980930     C/EJECT
200700980930     C************************************************************
200800980930     C* Calcolo saldo per partita e totale se lo deve fare
200900980930     C************************************************************
201000980930     C     CalcSaldo     BEGSR
201100980930     C*
201200980930     C* addiziono per totale CONTO se lo deve fare (è sempre in moneta
201300980930     C* di conto)
201400980930     C                   if        $carpag = *off
201500980930     C                   add       WtotDare      WsaldototD
201600980930     C                   add       WtotAvere     WsaldototA
201700980930     C                   add       WtotDareD     WsaldotoDD
201800980930     C                   add       WtotAvereD    WsaldotoAD
201900980930     C                   endif
202000980930     C*
202100980930     C* Dati in MONETA DI CONTO
202200980930     C*
202300980930     C* Segno
202400980930     C                   exsr      RepSegno
202500980930     C                   eval      h1segnom  = Wsegno
202600980930     C* Importo
202700980930     C                   Z-add     Wsaldo        WImporto
202800980930     C                   Z-add     NrDec         WNrDec
202900980930     C                   Eval      $LenFld = %Size(h1SaldoM)
203000980930     C* allineo dare a sinistra
203100980930     C                   if        h1segnoM = xscdare
203200980930     C                   SUB       5             $lenfld
203300980930     C                   Endif
203400980930     C                   EXSR      Edit
203500980930     C                   Eval      h1Saldom = $RcvrVar
203600980930     C*
203700980930     C* Dati in DIVISA
203800980930     C* segno
203900980930     C                   exsr      RepSegnoD
204000980930     C                   eval      H1SegnoD  = Wsegno
204100980930     C*
204200980930     C* Importo in divisa
204300980930     C* reperisco decimali se divisa diversa dalla precedente letta
204400980930     C                   if        Ppadivisa <> Savdivisa
204500980930     C                   eval      SavDivisa = PPaDivisa
204600980930     C                   Eval      WDivisa =  PpaDivisa
204700980930     C                   Exsr      RepDivisa
204800980930     C                   Eval      NrDecD = X01NrDec
204900980930     C                   endif
205000980930     C*
205100980930     C* importo
205200980930     C                   EVAL      Wimporto = Wsaldo
205300980930     C                   Z-add     NrDecD        WNrDec
205400980930     C                   Eval      $LenFld = %Size(h1SaldoD)
205500980930     C*
205600980930     C* Imposto la dimensione del campo video in modo da allineare
205700980930     C* valute gestite con un numero diverso di decimali
205800980930     C                   Select
205900980930     C                   When      WNrDec = 2
206000980930     C                   Eval      $LenFld = %Size(h1SaldoD) - 1
206100980930     C                   When      WNrDec = 1
206200980930     C                   Eval      $LenFld = %Size(h1Saldod) - 2
206300980930     C                   When      WNrDec = 0
206400980930     C                   Eval      $LenFld = %Size(h1Saldod) - 4
206500980930     C                   Other
206600980930     C                   Eval      $LenFld = %Size(h1Saldod)
206700980930     C                   Endsl
206800980930     C* allineo dare a sinistra
206900980930     C                   if        H1SegnoD = xscdare
207000980930     C                   SUB       5             $lenfld
207100980930     C                   Endif
207200980930     C                   EXSR      Edit
207300980930     C                   Eval      h1Saldod  = $RcvrVar
207400980930     C*
207500980930     C                   ENDSR
207600980930     C/EJECT
207700980930     C************************************************************
207800980930     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
207900980930     C************************************************************
208000980930     C     ATRS1         BEGSR
208100980930     C*
208200981002    >C* impostare le condizioni per ripristinare gli attributi
208300981002    >C* della riga del sfl (se ce ne sono)
208400981002      * riga di partita o meno
208500981002     C                   if        H1Tiporiga ='P'
208600981002     C                   eval      *in10=*on
208700981002     C                   else
208800981002     C                   eval      *in10=*off
208900981002     C                   endif
209000981002      * attiva o no unità
209100981002     C                   if        fgo1=*on
209200981002     C                   eval      *in11=*on
209300981002     C                   else
209400981002     C                   eval      *in11=*off
209500981002     C                   endif
209600981002      * attiva agente e solleciti solo se cliente
209700981002     C                   if        snatura199 = 'C'
209800981002     C                   eval      *in13=*on
209900981002     C                   else
210000981002     C                   eval      *in13=*off
210100981002     C                   endif
210200981002      * attiva bonifici solo per fornitore
210300981002     C                   if        snatura199 = 'F'
210400981002     C                   eval      *in14=*on
210500981002     C                   else
210600981002     C                   eval      *in14=*off
210700981002     C                   endif
210800030206D1270C*
210900030206  "   * se c'è valorizzata l'Id di Laguna
211000030206  "  C                   EVAL      *in42=*off
211100030206  "  C                   IF        H1IdLgn <> 0
211200030206  "  C* metto in RI il numero documento per segnalare che esiste un
211300030206  "  C* documento catalogato per questa registrazione (o una fattura
211400030206  "  C*  o una nota)
211500030206  "  C                   EVAL      *in42=*on
211600030206D1270C                   endif
211700980930     C*
211800980930     C                   ENDSR
211900980930     C/EJECT
212000980930     C************************************************************
212100980930     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
212200980930     C* INPUT
212300980930     C* $FC1 = Abilitazioni videata S1
212400980930     C* OUTPUT
212500980930     C* *IN97 *IN99 Errori
212600980930     C************************************************************
212700980930     C     CTRKS1        BEGSR
212800980930     C*
212900980930     C                   SETOFF                                       99
213000980930     C                   MOVEA     $FC1          $FC
213100980930     C                   EXSR      CTRKEY
213200980930     C     *IN99         IFEQ      *ON
213300980930     C                   SETON                                        97
213400980930     C                   ENDIF
213500980930     C*
213600980930     C                   ENDSR
213700981009     C/EJECT
213800981009     C************************************************************
213900981009     C* GESTIONE F02 VIDEO S1  dati anagrafici conto
214000981009     C************************************************************
214100981009     C     F02S1         BEGSR
214200981009     C*
214300981009     C                   reset                   ana397ds
214400981009     C                   eval      societa397 = XSCSOC
214500981009     C                   eval      unita397 = unita199
214600981009     C                   eval      kcc397 = kcc199
214700981009     C                   eval      ksc397 = KSC199
214800981009     C                   eval      wdw397 = *on
214900981009     C                   movel     ana397ds      kpjbu
215000981009     C                   CALL      'ANA397R'                            21
215100981009     C                   PARM                    KPJBA
215200981009     c                   movel     kpjbu         ana397ds
215300981009     C*
215400981009     C                   ENDSR
215500980930     C/EJECT
215600980930     C************************************************************
215700980930     C* GESTIONE F08 VIDEO S1
215800980930     C************************************************************
215900980930     C     F08S1         BEGSR
216000980930     C*
216100980930     C                   MOVE      *ON           $FINE
216200980930    >C                   MOVE      '0'           RET199
216300980930     C*
216400980930     C                   ENDSR
216500980930     C/EJECT
216600980930     C************************************************************
216700980930     C* GESTIONE F03 VIDEO S1
216800980930     C************************************************************
216900980930     C     F03S1         BEGSR
217000980930     C*
217100980930     C                   MOVE      *ON           $FINE
217200980930    >C                   MOVE      '1'           RET199
217300980930     C* fine programma
217400980930     C                   EXSR      ENDPGM
217500980930     C*
217600980930     C                   ENDSR
217700980930     C/EJECT
217800980930     C************************************************************
217900980930     C* GESTIONE F11 VIDEO S1 - cambio moneta
218000980930     C************************************************************
218100980930     C     F11S1         BEGSR
218200980930     C*
218300980930     C* Eseguo solo se ci sono record nel sfl
218400980930     C                   IF        Wmax <> 0
218500980930     C* Scambio richiesta
218600980930     C                   select
218700980930     C                   when      Savdivvis = '0'
218800980930     C                   eval      Savdivvis = '1'
218900980930     C*
219000980930     C                   when      Savdivvis = '1'
219100980930     C                   eval      Savdivvis = '0'
219200980930     C                   endsl
219300980930     C*
219400980930     C                   DO        *hival        $X                4 0
219500980930     C     $X            chain     c199s1                             21
219600980930     C   21              LEAVE
219700980930     C                   select
219800980930     C                   when      Savdivvis = '0'
219900980930     C                   eval      s1saldo1 = h1saldom
220000980930     C                   eval      s1saldo2 = h1saldod
220100981009     c                   if        h1tiporiga = 'P'
220200981009     C                   EVAL      dsDiVisa   = h1divisa2
220300981009     C                   EVAL      s1DiVisa2  = h1divisa1
220400981009     c                   endif
220500980930     c*
220600980930     C                   when      Savdivvis = '1'
220700980930     C                   eval      s1saldo1 = h1saldod
220800980930     C                   eval      s1saldo2 = h1saldom
220900981009     c                   if        h1tiporiga = 'P'
221000981009     C                   EVAL      dsDiVisa   = h1divisa1
221100981009     C                   EVAL      s1DiVisa2  = h1divisa2
221200981009     c                   endif
221300980930     C                   endsl
221400981002     C*
221500981002     C* se le divise sono uguali, imposto il valore del campo in drop
221600981002     C* uguale a quello della prima riga, così almeno sono allineati
221700981002     C                   IF        h1divisa1 = h1divisa2
221800981002     C                   eval      s1saldo2 = s1saldo1
221900981002     C                   endif
222000980930     c*sflnxtchg
222100980930     c                   if        s1opz <> 0
222200980930     c                   seton                                        32
222300980930     c                   else
222400980930     c                   setoff                                       32
222500980930     c                   endif
222600980930     C* imposto quegli attributi del sfl che sono modificabili
222700980930     C* al richiamo del determinato pgm
222800980930     C                   EXSR      ATRS1
222900980930     C*  spengo r.i. per opzione
223000980930     c                   setoff                                       40
223100980930     C                   UPDATE    C199S1
223200980930     C*
223300980930     C                   ENDDO
223400980930     C*
223500980930     C                   ENDIF
223600980930     C                   ENDSR
223700980930     C/EJECT
223800980930     C************************************************************
223900980930     C* GESTIONE F12 VIDEO S1
224000980930     C************************************************************
224100980930     C     F12S1         BEGSR
224200980930     C*
224300980930     C                   MOVE      *ON           $FINE
224400980930    >C                   MOVE      '2'           RET199
224500980930     C*
224600980930     C                   ENDSR
224700980930     C/EJECT
224800980930     C************************************************************
224900980930     C* GESTIONE F15 VIDEO S1
225000980930     C************************************************************
225100980930     C     F15S1         BEGSR
225200980930     C*
225300980930     C                   reset                   ana399ds
225400980930     C                   eval      A39societa = XSCSOC
225500980930     C                   eval      A39unita = unita199
225600980930     C                   eval      A39ctb = ctb199
225700980930     C                   eval      A39kcc = kcc199
225800980930     C                   eval      A39ksc = KSC199
225900980930     C                   eval      A39SNATURA = snatura199
226000980930     C                   movel     ana399ds      kpjbu
226100980930     C                   CALL      'ANA399R'                            21
226200980930     C                   PARM                    KPJBA
226300981007     C     A39ERRORI     IFne      '1'
226400981007     c* se modificata esistenza commenti riemetto la testata
226500981007     c                   exsr      TstTcm
226600981007     c                   select
226700981007     C                   when      exi724     = *on and *in19 = *off
226800981007     c                   eval      *in19 = *on
226900981007     c                   write     c199t1
227000981007     C                   EXSR      GESPIE
227100981007     C                   when      exi724     =*off and *in19 = *on
227200981007     c                   eval      *in19 = *off
227300981007     c                   write     c199t1
227400981007     C                   EXSR      GESPIE
227500981007     C                   endsl
227600981007     c                   endif
227700980930     C*
227800980930     C                   ENDSR
227900980930     C/EJECT
228000980930     C************************************************************
228100980930     C* GESTIONE F16 VIDEO S1         Riepilogo per divisa
228200980930     C************************************************************
228300980930     C     F16S1         BEGSR
228400980930     C*
228500980930     C* Calcolo saldi per divisa
228600980930     C                   Reset                   Ndc291Ds
228700980930     C                   MOVE      XscSoc        Societa291
228800980930     C                   MOVE      Ctb199        Ctb291
228900980930     C                   MOVE      Unita199      Unita291
229000980930     C                   MOVE      Kcc199        Kcc291
229100980930     C                   MOVE      Ksc199        Ksc291
229200980930     C                   Move      Divisa199     DivisaS291
229300980930     C                   Move      DivSel199     DivSel291
229400980930     c
229500980930     C                   move      DtPerDa199    DtInPe291
229600980930     C                   move      DtPerA199     DtFiPe291
229700980930     C                   move      MovDef199     MovDef291
229800980930     C                   move      MovProv199    MovPro291
229900980930     C                   move      MovGes199     MovGes291
230000980930     C                   move      SitPar199     SitPar291
230100980930     C                   move      DtRegD199     DtRegD291
230200980930     C                   move      DtRegP199     DtRegP291
230300980930     C                   move      DtRegG199     DtRegG291
230400980930     C                   move      CtgAn1199     CtgAn1291
230500980930     C                   move      CtgAn2199     CtgAn2291
230600980930     C                   move      CtgAn3199     CtgAn3291
230700980930     C                   move      KscAge199     KscAge291
230800980930     C                   move      BkAmm199      BkAmm291
230900980930     C
231000981002     C                   Eval      Kpjbu = Ndc291Ds
231100980930     C                   CALL      'NDC291R'                            21
231200980930     C                   PARM                    KPJBA
231300980930     C                   PARM                    Ndc291Ds1
231400980930     C*
231500981002     c                   Movel     Kpjbu         Ndc291Ds
231600980930     C     Esito291      Ifeq      *on
231700980930     C* Generazione saldi terminata in errore
231800980930     C                   SETON                                        9099
231900980930     C                   Else
232000980930     C* Visualizzazione saldi in divisa
232100980930     C                   Reset                   Ndc293Ds
232200980930     C                   EVAL      Societa293 = Xscsoc
232300980930     C                   EVAL      Kcc293     = kcc199
232400980930     C                   EVAL      Ksc293     = ksc199
232500980930     C                   Movel     Descr199      DesBrev293
232600980930     C                   Eval      Kpjbu = Ndc293Ds
232700980930     C                   CALL      'NDC293R'                            21
232800980930     C                   PARM                    KPJBA
232900980930     C                   PARM                    SOC001
233000980930     C                   PARM                    Ndc291Ds1
233100980930     C* Testo uscita
233200980930     c                   Movel     Kpjbu         Ndc293Ds
233300980930     C* Se F3 vado a fine programma
233400980930     C     Ret293        Ifeq      '1'
233500980930     C                   MOVE      *ON           $FINE
233600980930    >C                   MOVE      '1'           RET199
233700980930     C                   EXSR      ENDPGM
233800980930     C                   endif
233900980930     C                   endif
234000980930     C*
234100980930     C                   ENDSR
234200980930     C/EJECT
234300980930     C************************************************************
234400980930     C* GESTIONE F20 VIDEO S1 - Stampa E/C
234500980930     C************************************************************
234600980930     C     F20S1         BEGSR
234700980930     C* Reperimento titolo stampa
234800980930     C                   EXSR      REPTIT
234900980930     C                   SELECT
235000980930     C* clienti
235100980930     c                   WHEN      snatura199 = 'C'
235200980930     C                   RESET                   NDC141DS
235300980930     C                   MOVE      xscsoc        soc141
235400980930     C                   MOVE      ctb199        ctb141
235500980930     C                   MOVE      Unita199      uni141
235600980930     C                   MOVE      kcc199        KCC141
235700980930     C                   MOVE      ksc199        KSC141
235800980930     C                   move      movdef199     def141
235900980930     C                   move      movprov199    pro141
236000980930     C                   move      movges199     ges141
236100980930     C                   move      dtperda199    dpd141
236200980930     C                   move      dtpera199     dpa141
236300030312d1303C                   move      divsel199     income141
236400980930     C                   MOVE      divisa199     div141
236500980930     C                   move      sitpar199     sip141
236600980930     C                   move      dtregd199     dmd141
236700980930     C                   move      dtregp199     dmp141
236800980930     C                   move      dtregg199     dmg141
236900980930     C                   MOVE      ctgan1199     ct1141
237000980930     C                   MOVE      ctgan2199     ct2141
237100980930     C                   MOVE      ctgan3199     ct3141
237200980930     C                   MOVE      kscage199     arc141
237300980930     C                   MOVE      bkamm199      bka141
237400980930     C                   MOVE      kccfact199    kcf141
237500980930     C                   MOVE      kscfact199    ksf141
237600980930     c                   eval      lr141 = *on
237700010409c1481C                   MOVE      stpcom199     stpcom141
237800060418D1983c                   move      DataSF199     DataSF141
237900980930     C                   MOVEL     NDC141DS      KPJBU
238000980930     C                   CALL      'NDC146R1'
238100980930     C                   PARM                    KPJBA
238200980930     C                   PARM                    TIT              50
238300980930     C* fornitori
238400980930     c                   WHEN      snatura199 = 'F'
238500980930     C                   RESET                   NDC142DS
238600980930     C                   MOVE      xscsoc        soc142
238700980930     C                   MOVE      ctb199        ctb142
238800980930     C                   MOVE      Unita199      uni142
238900980930     C                   MOVE      kcc199        KCC142
239000980930     C                   MOVE      ksc199        KSC142
239100980930     C                   move      movdef199     def142
239200980930     C                   move      movprov199    pro142
239300980930     C                   move      movges199     ges142
239400980930     C                   move      dtperda199    dpd142
239500980930     C                   move      dtpera199     dpa142
239600030312D1303C                   move      divsel199     income142
239700980930     C                   MOVE      divisa199     div142
239800980930     C                   move      sitpar199     sip142
239900980930     C                   move      dtregd199     dmd142
240000980930     C                   move      dtregp199     dmp142
240100980930     C                   move      dtregg199     dmg142
240200980930     C                   MOVE      ctgan1199     ct1142
240300980930     C                   MOVE      ctgan2199     ct2142
240400980930     C                   MOVE      ctgan3199     ct3142
240500980930     C                   MOVE      bkamm199      bka142
240600980930     C                   MOVE      kccfact199    kcf142
240700980930     C                   MOVE      kscfact199    ksf142
240800980930     c                   eval      lr142 = *on
240900010409c1481C                   MOVE      stpcom199     stpcom142
241000980930     C                   MOVEL     NDC142DS      KPJBU
241100980930     C                   CALL      'NDC147R1'
241200980930     C                   PARM                    KPJBA
241300980930     C                   PARM                    TIT              50
241400980930     C*
241500980930     C                   OTHER
241600980930     C*altri conti
241700980930     C                   RESET                   NDC143DS
241800980930     C                   MOVE      xscsoc        soc143
241900980930     C                   MOVE      ctb199        ctb143
242000980930     C                   MOVE      Unita199      uni143
242100980930     C                   MOVE      kcc199        KCC143
242200980930     C                   MOVE      ksc199        KSC143
242300980930     C                   move      movdef199     def143
242400980930     C                   move      movprov199    pro143
242500980930     C                   move      movges199     ges143
242600980930     C                   move      dtperda199    dpd143
242700980930     C                   move      dtpera199     dpa143
242800030312D1303C                   move      divsel199     income143
242900980930     C                   MOVE      divisa199     div143
243000980930     C                   move      sitpar199     sip143
243100980930     C                   move      dtregd199     dmd143
243200980930     C                   move      dtregp199     dmp143
243300980930     C                   move      dtregg199     dmg143
243400980930     c                   eval      lr143 = *on
243500010410c1481C                   MOVE      stpcom199     stpcom143
243600980930     C                   MOVEL     NDC143DS      KPJBU
243700980930     C                   CALL      'NDC148R1'
243800980930     C                   PARM                    KPJBA
243900980930     C                   PARM                    TIT              50
244000980930     c*
244100980930     C                   ENDSL
244200980930     C*
244300980930     c                   endsr
244400020528c1567C************************************************************
244500020528     C* stampa griglia
244600020528     C************************************************************
244700020528     C     F21S1         BEGSR
244800020528     C*
244900020528     C                   RESET                   NDCG12DS
245000020528     C                   RESET                   NDCG01DS
245100020528     c* dico di non inizializzare i parametri di calcolo della griglia in
245200020528     c* quanto glieli passo
245300020528     c                   eval      inzg12=*off
245400020528     C                   MOVE      xscsoc        societaG01
245500020528     C                   MOVE      ctb199        ctbG01
245600020528     C                   MOVE      Unita199      unitaG01
245700020528     C                   MOVE      Snatura199    SnaturaG01
245800020528     C                   MOVE      kcc199        KCCG01
245900020528     C                   MOVE      ksc199        KSCg01
246000020528     C                   move      movdef199     movdefg01
246100020528     C                   move      movprov199    movprovg01
246200020528     C                   move      movges199     movgesg01
246300020528     C                   move      dtperda199    DtPerDag01
246400020528     C                   move      dtpera199     DtPerAG01
246500020528     C                   MOVE      divisa199     DivisaG01
246600020528     C                   MOVE      divsel199     divselG01
246700020528     C                   move      dtregd199     DtREgDG01
246800020528     C                   move      dtregd199     DtScaG01
246900020528     C                   move      dtregp199     dtregPG01
247000020528     C                   move      dtregg199     DtRegGg01
247100020528     C                   MOVE      ctgan1199     ctgan1g01
247200020528     C                   MOVE      ctgan2199     ctgan2g01
247300020528     C                   MOVE      ctgan3199     ctgan3g01
247400020528     C                   MOVE      kscage199     kscageg01
247500020528     C                   movel(P)  ndcg12ds      kpjbu
247600020528     C                   CALL      'NDCG12R'
247700020528     C                   PARM                    KPJBA
247800020528     C                   PARM                    ndcg01ds
247900020528     C                   movel     kpjbu         ndcg12ds
248000020528     C*
248100020528     C*  errore
248200020528     C     Errg12        IFEQ      '1'
248300020528     C                   EXSR      endpgm
248400020528     C                   endif
248500020528     C*
248600020528c1567c                   ENDSR
248700980930     C/EJECT
248800980930     C************************************************************
248900980930     C* REPERIMENTO TITOLO stampa partite
249000980930     C************************************************************
249100980930     C     REPTIT        BEGSR
249200980930     C*
249300980930     C                   MOVE      *BLANKS       TIT
249400980930     C*
249500980930     c                   select
249600980930     c                   when      snatura199 = 'C'
249700980930     C                   MOVEL     $TIT(1)       IDMSG
249800980930     c                   when      snatura199 = 'F'
249900980930     C                   MOVEL     $TIT(2)       IDMSG
250000980930     c                   other
250100980930     C                   MOVEL     $TIT(3)       IDMSG
250200980930     c                   endsl
250300980930     C                   CALLB     'XRTVM'
250400980930     C                   PARM                    IDMSG            27
250500980930     C                   PARM                    TXTMSG          100
250600980930     C                   PARM                    ERRMSG            1
250700980930     C                   PARM      *ON           CTRMSG            1            centratura
250800980930     C                   PARM      50            LENMSG            3 0          lun output.
250900980930     C*
251000980930     C     ERRMSG        IFNE      *ON
251100980930     C                   MOVEL     TXTMSG        TIT
251200980930     C                   ELSE
251300980930     C                   MOVEL     *ALL'?'       TIT
251400980930     C                   ENDIF
251500980930     C*
251600980930     C                   ENDSR
251700980930     C/EJECT
251800980930     C************************************************************
251900980930     C* GESTIONE F23 VIDEO S1
252000980930     C************************************************************
252100980930     C     F23S1         BEGSR
252200980930     C*
252300980930    >C                   Z-ADD     76            $L01
252400980930    >C                   Z-ADD     76            $L02
252500980930     C                   EXSR      RIEOPZ
252600980930     C                   Z-ADD     23            X
252700980930     C                   MOVE      FF23          $FC1(X)
252800980930     C                   MOVEL     WOPZ1         C1OPZ1
252900060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
253000980930     C                   MOVEL     WOPZ2         C1OPZ2
253100980930     C                   Z-ADD     WCOZ          C1COZ
253200980930     C*
253300980930     C                   ENDSR
253400980930     C/EJECT
253500980930     C************************************************************
253600980930     C* GESTIONE F24 VIDEO S1
253700980930     C************************************************************
253800980930     C     F24S1         BEGSR
253900980930     C*
254000980930     C                   MOVEA     $FX1          $FX
254100980930     C                   MOVEA     $FC1          $FC
254200980930     C                   MOVEA     $FR1          $FR
254300980930     C                   Z-ADD     $ULKS1        $ULK
254400980930    >C                   Z-ADD     79            $L01
254500980930    >C                   Z-ADD     79            $L02
254600980930     C                   EXSR      RIEKEY
254700980930     C                   Z-ADD     $ULK          $ULKS1            3 0
254800980930     C                   MOVEA     $FC           $FC1
254900980930     C                   MOVEL     $KEY1         Z1KE1
255000060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
255100980930     C                   MOVEL     $KEY2         Z1KE2
255200980930     C                   WRITE     C199Z1
255300980930     C*
255400980930     C                   ENDSR
255500980930     C/EJECT
255600980930     C************************************************************
255700980930     C* CONTROLLO TESTATA LISTA
255800980930     C************************************************************
255900980930     C     CTRC1         BEGSR
256000980930     C*
256100980930     C                   MOVE      *OFF          *IN99
256200980930     C*
256300980930    >C* aggiungere in OR altri campi posizionamento se ci sono
256400981002     C***                EXSR      POSS1
256500980930     C*
256600980930     C                   ENDSR
256700980930     C/EJECT
256800980930     C************************************************************
256900980930     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
257000980930     C************************************************************
257100981002     C*    POSS1         BEGSR
257200980930     C*
257300980930     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
257400980930     C*
257500981002   B1C*    WMAX          IFGT      0
257600981002     C*                  READC     C199S1                                 21
257700981002   B2C*    *IN21         DOWEQ     *OFF
257800981002     C*    *IN99         ANDEQ     *OFF
257900981002     C*
258000981002   B3C*    S1OPZ         IFNE      0
258100981002     C*                  SETON                                        51  99
258200981002   E3C*                  ENDIF
258300981002     C* se rilevata opzione attiva, attivo sflnxtchg
258400981002   B3C*    *IN99         IFEQ      *ON
258500981002     C*                  MOVE      *ON           *IN32
258600980930     C*
258700980930     C* imposto quegli attributi del sfl che sono modificabili
258800980930     C* al richiamo del determinato pgm
258900981002     C*                  EXSR      ATRS1
259000981002     C*
259100981002     C*                  UPDATE    C199S1
259200981002   X3C*                  ELSE
259300981002     C*                  READC     C199S1                                 21
259400981002   E3C*                  ENDIF
259500981002     C*
259600981002   E2C*                  ENDDO
259700981002     C*
259800981002   E1C*                  ENDIF
259900981002     C*
260000980930     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
260100980930     C* AL VALORE DI POSIZIONAMENTO :
260200980930     C* se questo è maggiore del valore del rispettivo campo di
260300980930     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
260400980930     C* con i valori di parzializzazione.
260500980930     C*
260600981002   B1C*    *IN99         IFEQ      *OFF
260700980930     C*
260800980930     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
260900980930     C* è maggiore del relativo valore di parzializzazione, nel qual caso
261000980930     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
261100980930     C* richiedo comunque un ricaricamento del sfl che, se non si è
261200980930     C* verificata la condizione suddetta avverrà con posizionamento a
261300980930     C* partire dal valore di parzializzazione
261400980930     C*
261500981002   B2C*                  SELECT
261600980930    >C* Attenzione : se un ordinamento ha un campo di posizionamento
261700980930    >C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
261800980930    >C* dei relativi campi
261900980930     C*
262000981002     C*    $ORD          WHENEQ    1
262100981002     C*
262200981002     C*    $ORD          WHENEQ    2
262300981002     C*
262400981002   E2C*                  ENDSL
262500981002     C*
262600981002     C* richiedo l'inizializzazione sfl
262700981002     C*                  MOVE      *ON           *IN99
262800981002     C*                  MOVE      *ON           $INZS1
262900981002     C*
263000981002   E1C*                  ENDIF
263100981002     C*
263200981002     C*                  ENDSR
263300980930     C/EJECT
263400980930     C************************************************************
263500980930     C* CONTROLLO OPZIONI LISTA
263600980930     C************************************************************
263700980930     C     CTRS1         BEGSR
263800980930     C*
263900980930     C                   MOVEL     *OFF          $ESCI
264000980930     C                   SETOFF                                       99
264100980930     C                   Z-ADD     0             S1OPZ
264200980930     C*
264300980930     C* Leggo il sfl solo se ci sono rcd
264400980930   B1C     WMAX          IFGT      0
264500980930     C                   READC     C199S1                                 21
264600980930     C*
264700980930     C* esce se fine sfl o errore che richiede l'uscita
264800980930   B2C     *IN21         DOWEQ     *OFF
264900980930     C     $ESCI         ANDEQ     *OFF
265000980930     C                   Z-ADD     S1NRR         C1RCD
265100980930     C* ctrl su riga
265200980930     C                   EXSR      RECS1
265300980930     C* gestione opzioni
265400980930   B3C     S1OPZ         IFNE      0
265500980930     C     *IN99         ANDEQ     *OFF
265600980930     C                   EXSR      OPZS1
265700980930   E3C                   ENDIF
265800980930     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
265900980930   B3C     *IN99         IFEQ      *ON
266000980930     C                   MOVE      *ON           *IN32
266100980930   E3C                   ENDIF
266200980930     C*
266300980930     C* imposto quegli attributi del sfl che sono modificabili
266400980930     C* al richiamo del determinato pgm
266500980930     C                   EXSR      ATRS1
266600980930     C*
266700980930     C                   Z-ADD     0             S1OPZ
266800980930     C*
266900980930     C                   UPDATE    C199S1
267000980930     C                   MOVE      *OFF          *IN32
267100980930     C*
267200980930     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
267300980930   B3C     $ESCI         IFEQ      *OFF
267400980930     C                   READC     C199S1                                 21
267500980930     C* a fine ciclo ripristino il flag richiesta iniz.sfl
267600980930   B4C     *IN21         IFEQ      *ON
267700980930     C                   MOVE      WINZS1        $INZS1
267800980930     C* calcolo pagina a cui deve posizionarsi
267900980930     C     WSFL          IFGT      0
268000980930     C                   EXSR      CLCPAG
268100980930     C                   ELSE
268200980930     C                   Z-ADD     SFLPAG        WPAG
268300980930     C                   ENDIF
268400980930   E4C                   ENDIF
268500980930   E4C                   ENDIF
268600980930     C*
268700980930   E2C                   ENDDO
268800980930     C*
268900980930   E1C                   ENDIF
269000980930     C*
269100980930     C                   ENDSR
269200980930     C/EJECT
269300980930     C************************************************************
269400980930     C* CONTROLLO CAMPI I/O RIGA LISTA
269500980930     C************************************************************
269600980930     C     RECS1         BEGSR
269700980930     C*
269800980930     C* reset indicatori DSPATR
269900980930     C                   MOVE      *ALL'0'       IN4049           10
270000980930     C                   MOVEA     IN4049        *IN(40)
270100980930     C*
270200980930     C* Controllo se Utente Autorizzato alla scelta
270300980930     C     S1OPZ         IFNE      0
270400980930     C                   Z-ADD     1             X
270500980930     C     S1OPZ         LOOKUP    $OP1(X)                                21
270600980930     C     *IN21         IFEQ      *OFF
270700980930     C     $FL1(X)       OREQ      'N'
270800980930     C                   MOVE      *ON           $ESCI
270900980930     C                   SETON                                        504099
271000980930     C                   ENDIF
271100980930     C                   ENDIF
271200981002     C*
271300981002     C* Su riga movimento non sono valide le opzioni di partita
271400981002     c*
271500981002     C     H1tiporiga    ifeq      'M'
271600981002     C     *in99         andeq     *off
271700981002     C     S1OPZ         IFEQ      14
271800981002     C     S1OPZ         oreq      20
271900981002     C     S1OPZ         oreq      32
272000981002     C     S1OPZ         oreq      40
272100051109C1881X***  S1OPZ         oreq      23
272200051109C1881C     S1OPZ         oreq      31
272300981002     C     S1OPZ         oreq      42
272400031120D1498X**   S1OPZ         oreq      60    Ex C1460
272500010119     C                   MOVE      *ON           $ESCI
272600981002     C                   SETON                                        534099
272700981002     C                   ENDIF
272800981002     C                   ENDIF
272900981002     C*
273000981002     C* Su riga di partita non sono valide le opzioni di movimento
273100981002     c*
273200981002     C     H1tiporiga    ifeq      'P'
273300981002     C     *in99         andeq     *off
273400981002     C     S1OPZ         IFEQ      21
273500981002     C     S1OPZ         oreq      22
273600051109C1881C     S1OPZ         oreq      23
273700981009     C     S1OPZ         oreq      41
273800981002     C     S1OPZ         oreq      43
273900981002     C     S1OPZ         oreq      44
274000981002     C     S1OPZ         oreq      45
274100981002     C                   MOVE      *ON           $ESCI
274200981002     C                   SETON                                        544099
274300981002     C                   ENDIF
274400981002     C                   ENDIF
274500981002     C*
274600981002     C* Se richiesto portafoglio, la causale deve essere di tipo emissione porta
274700981002     c* per i clienti
274800981002     C     S1OPZ         IFEQ      43
274900981002     C     *in99         andeq     *off
275000981002     C     PORTAFH       andne     '1'
275100981002     C     snatura199    andeq     'C'
275200981002     C                   MOVE      *ON           $ESCI
275300981002     C                   SETON                                        524099
275400981002     C                   ENDIF
275500981002     C*
275600981002     C* Se richiesto portafoglio, la causale non deve essere = a blank      orta
275700981002     c* per i fornitori
275800981002     C     S1OPZ         IFEQ      43
275900981002     C     *in99         andeq     *off
276000981002     C     PORTAFH       andeq     ' '
276100981002     C     snatura199    andeq     'F'
276200981002     C                   MOVE      *ON           $ESCI
276300981002     C                   SETON                                        524099
276400981002     C                   ENDIF
276500031120     C*
276600031120D1498C* Se richiesta opzione 60=ordini, la causale deve essere di fatt.one porta
276700031120  "  C     S1OPZ         IFEQ      60
276800031120  "  C     H1tiporiga    andeq     'M'
276900031120  "  C     *in99         andeq     *off
277000031120  "  C     tpdociH       andne     '0'
277100031120  "  C     tpdociH       andne     '3'
277200031120  "  C     tpdociH       andne     '8'
277300031120  "  C                   MOVE      *ON           $ESCI
277400031120  "  C                   SETON                                        554099
277500031120D1498C                   ENDIF
277600030206D1270C*
277700030206  "  C* controllo che l'opzione 80=visione dei documenti su Laguna sia
277800030206  "  C* data solo sulle registrazioni che ne hanno
277900030206  "  C     S1OPZ         IFEQ      80
278000030206  "  C     H1IdLgn       ANDEQ     0
278100030206  "  C                   SETON                                        504099
278200030206  "  C                   ENDIF
278300030206D1270C*
278400980930     C*
278500980930     C                   ENDSR
278600980930     C/EJECT
278700980930     C************************************************************
278800980930     C* GESTIONE OPZIONI LISTA
278900980930     C************************************************************
279000980930     C     OPZS1         BEGSR
279100980930     C*
279200980930     C                   Z-ADD     RIRI          CURR
279300980930     C                   Z-ADD     COCO          CURC
279400980930     C*
279500980930     C                   SELECT
279600980930     C*
279700980930     C* se opz=Selezione
279800980930     C* passo la chiave al pgm chiamante
279900980930     C     S1OPZ         WHENEQ    1
280000980930     C*
280100980930     C                   MOVE      *HIVAL        $LASTV
280200980930     C*
280300980930     C
280400981002     C* OPZIONI DI PARTITA
280500980930     C* se opz=Commenti
280600980930     C* richiamo il pgm di gestione
280700980930     C     S1OPZ         WHENEQ    14
280800980930     C                   RESET                   ANA725DS
280900980930     C                   MOVE      '02'          OPZ725
281000980930    >C                   MOVE      'PPA'         TPA725
281100980930     C* inserire chiavi commenti
281200980930    >C                   MOVEL     CTB199        KY1725
281300980930    >C                   MOVEL     C1KCC         KY2725
281400980930    >C                   MOVE      C1KSC         KY2725
281500990608    >C                   MOVEL     H1DTPAR       KY3725
281600990608    >C                   MOVEL     H1nrpar       KY4725
281700990608    >C                   MOVE      H1serpar      KY4725
281800980930     C                   MOVE      *BLANK        KPJBU
281900980930     C                   MOVEL     ANA725DS      KPJBU
282000980930     C                   CALL      'ANA725R'
282100980930     C                   PARM                    KPJBA
282200980930     C                   MOVEL     KPJBU         ANA725DS
282300980930     C*
282400980930     C* ritorno da PGM commenti
282500980930     C                   EXSR      RTN725
282600980930     C*
282700980930     C                   MOVE      *HIVAL        $LASTV
282800980930     C*
282900980930     C* se opz=Particolarità partite
283000980930     C* richiamo il pgm di gestione
283100980930     C     S1OPZ         WHENEQ    20
283200980930     c                   reset                   x48chPgmds
283300980930     C                   eval      x48pgm = 'NDC060R'
283400980930     c                   eval      kpjbu = x48chpgmds
283500980930     c                   call      'X48CHPGM'
283600980930     c                   parm                    kpjba
283700980930     c                   movel     kpjbu         x48chpgmds
283800980930     c                   if        x48errore = *off
283900980930     C                   RESET                   NDC060DS
284000980930     c*
284100980930     C                   MOVE      '02'          OPZ060
284200980930     C                   MOVE      Unita199      unita060
284300980930     C                   MOVE      Ctb199        ctb060
284400980930     C                   MOVE      C1KCC         KCC060
284500980930     C                   MOVE      C1KSC         KSC060
284600980930     C                   CALLb     'XDT4000'
284700980930     C     dtpar060      PARM                    XDTAMG
284800990608     C                   PARM      H1dtpar       XDTGMA
284900980930     C                   PARM      4             XDTSTA
285000990608     C                   move      H1nrpar       nrpar060
285100990608     C                   move      H1serpar      serpar060
285200980930     C*imposto chiave unique per la lista
285300980930    >C                   MOVEL     ctb060        UQ1060
285400980930    >C                   MOVEL     kcc060        UQ2060
285500980930    >C                   MOVEL     ksc060        UQ3060
285600980930     C                   move      dtpar060      UQ4060
285700980930     C                   move      nrpar060      UQ5060
285800980930     C                   move      serpar060     UQ6060
285900980930     C                   MOVE      *BLANK        KPJBU
286000980930     C                   MOVEL     NDC060DS      KPJBU
286100980930     C                   CALL      'NDC060R'
286200980930     C                   PARM                    KPJBA
286300980930     C                   MOVEL     KPJBU         NDC060DS
286400980930     C*
286500980930     C* ritorno da PGM commenti
286600980930     C                   EXSR      RTN060
286700980930     C                   endif
286800980930     C*
286900980930     C                   MOVE      *HIVAL        $LASTV
287000980930     C*
287100980930     C* se opz=MIX
287200980930     C* richiamo il pgm di gestione
287300980930     C     S1OPZ         WHENEQ    32
287400980930     c                   reset                   x48chPgmds
287500980930     C                   eval      x48pgm = 'NDC065R'
287600980930     c                   eval      kpjbu = x48chpgmds
287700980930     c                   call      'X48CHPGM'
287800980930     c                   parm                    kpjba
287900980930     c                   movel     kpjbu         x48chpgmds
288000980930     c                   if        x48errore = *off
288100980930     C                   RESET                   NDC065DS
288200980930     C                   MOVE      '02'          OPZ065
288300980930     C                   MOVE      Unita199      unita065
288400980930     C                   MOVE      Ctb199        ctb065
288500980930     C                   MOVE      C1KCC         KCC065
288600980930     C                   MOVE      C1KSC         KSC065
288700980930     C                   CALLb     'XDT4000'
288800980930     C     dtpar065      PARM                    XDTAMG
288900990608     C                   PARM      H1dtpar       XDTGMA
289000980930     C                   PARM      4             XDTSTA
289100990608     C                   move      H1nrpar       nrpar065
289200990608     C                   move      H1serpar      serpar065
289300980930     C*imposto chiave unique per la lista
289400980930    >C                   MOVEL     ctb065        UQ1065
289500980930    >C                   MOVEL     kcc065        UQ2065
289600980930    >C                   MOVEL     ksc065        UQ3065
289700980930     C                   move      dtpar065      UQ4065
289800980930     C                   move      nrpar065      UQ5065
289900980930     C                   move      serpar065     UQ6065
290000980930     C                   MOVE      *BLANK        KPJBU
290100980930     C                   MOVEL     NDC065DS      KPJBU
290200980930     C                   CALL      'NDC065R'
290300980930     C                   PARM                    KPJBA
290400980930     C                   MOVEL     KPJBU         NDC065DS
290500980930     C*
290600980930     C* ritorno da PGM commenti
290700980930     C                   EXSR      RTN065
290800980930     c                   endif
290900980930     C*
291000980930     C                   MOVE      *HIVAL        $LASTV
291100980930     C*
291200980930     C* se opz=Ritenute
291300980930     C* richiamo il pgm di gestione
291400980930     C     S1OPZ         WHENEQ    40
291500980930     C                   RESET                   NDCR05DS
291600980930     C                   eval      opzr05     = '02'
291700980930     C                   eval      kccR05     = kcc199
291800980930     C                   eval      kscR05     = ksc199
291900980930     C                   CALLb     'XDT4000'
292000980930     C     ddcr05        PARM                    XDTAMG
292100980930     C                   PARM      s1dtdoc       XDTGMA
292200980930     C                   PARM      4             XDTSTA
292300980930     C                   move      s1nrdoc       ndcr05
292400981002     C                   move      s1serdoc      serdocr05
292500980930     C                   clear                   kpjbu
292600980930     C                   movel     ndcR05ds      kpjbu
292700980930     C                   CALL      'NDCR05R'
292800980930     C                   PARM                    KPJBA
292900980930     C                   movel     kpjbu         ndcR05ds
293000980930     C*
293100980930     c* F12
293200980930    >C     RETR05        ifeq      '2'
293300980930     C                   MOVE      *ON           $ESCI
293400980930     C                   endif
293500980930     C*  F3
293600980930    >C     RETR05        IFEQ      '1'
293700980930     C                   MOVE      *ON           $ESCI
293800980930     C                   MOVE      *ON           $FINE
293900980930     C                   MOVE      '1'           RET199
294000980930     C                   endif
294100980930     C*
294200980930     C                   MOVE      *HIVAL        $LASTV
294300980930     C*
294400980930     C*
294500980930     C* se opz=Bonifici
294600980930     C* richiamo il pgm di gestione
294700051109C1881X***  S1OPZ         WHENEQ    23
294800051109C1881C     S1OPZ         WHENEQ    31
294900981112      * chiamo NDCB50R al posto del vecchio NDCH65R1(appena asteriscato)
295000981112     c                   reset                   x48chPgmds
295100981112     C                   eval      x48pgm = 'NDCB50R'
295200981112     c                   eval      kpjbu = x48chpgmds
295300981112     c                   call      'X48CHPGM'
295400981112     c                   parm                    kpjba
295500981112     c                   movel     kpjbu         x48chpgmds
295600981112     c                   if        x48errore = *off
295700981112     C                   RESET                   NDCB50DS
295800981112     C                   Eval      OpzB50      = '05'
295900981230     C                   Eval      PgmCallB50  = DsPgm
296000981230     C                   Eval      SocietaB50  = XscSoc
296100981230     C                   Eval      UnitaB50    = Unita199
296200981230     C                   Eval      CtbB50      = Ctb199
296300981112     C                   Eval      KccB50      = Kcc199
296400981112     C                   Eval      KscB50      = Ksc199
296500981130     C                   Move      *Loval        $DataISO
296600981130     C                   Eval      $GGPar      = DsGGPar
296700981130     C                   Eval      $MMPar      = DsMMPar
296800981130     C                   Eval      $AAPar      = DsAAPar
296900981130     C     $Dtpar60      IfNe      0
297000000118C1309C     *JobRun       Move      $DtPar60      $DataISO
297100981130     C                   Move      $DataISO      DtParB50
297200981130     C                   EndIf
297300981130     C                   Move      DsNrPar       NrParB50
297400981130     C                   Eval      SerParB50   = DsSerPar
297500981230      *
297600981230     C                   Eval      Kpjbu = NDCB50DS
297700981112     C                   CALL      'NDCB50R'
297800981112     C                   PARM                    KPJBA
297900981112     C                   Movel     Kpjbu         NDCB50DS
298000981112     C
298100981112      * F12
298200981112    >C     RETB50        ifeq      '2'
298300981112     C                   MOVE      *ON           $ESCI
298400981112     C                   endif
298500981112      *  F3
298600981112    >C     RETB50        IFEQ      '1'
298700981112     C                   MOVE      *ON           $ESCI
298800981112     C                   MOVE      *ON           $FINE
298900981112     C                   MOVE      '1'           RET199
299000981112     C                   endif
299100981112     C                   endif
299200981112     C
299300981112     C                   MOVE      *HIVAL        $LASTV
299400981112      *
299500980930     C* se opz=Solleciti
299600980930     C* richiamo il pgm di gestione
299700980930     C     S1OPZ         WHENEQ    42
299800980930     c                   reset                   x48chPgmds
299900980930     C                   eval      x48pgm = 'NDCL29R'
300000980930     c                   eval      kpjbu = x48chpgmds
300100980930     c                   call      'X48CHPGM'
300200980930     c                   parm                    kpjba
300300980930     c                   movel     kpjbu         x48chpgmds
300400980930     c                   if        x48errore = *off
300500980930     C                   RESET                   NDCL29DS
300600980930     C                   eval      L29societa = xscsoc
300700000419D0536C*****              eval      L29unita   = unita199
300800000419D0536C* i programmi dei solleciti devono ricervere per forza l'unità
300900000419d0536C* quindi non è possibile passare quella della parzializzazione
301000000419D0536C                   eval      L29unita   = S1Unita
301100980930     C                   eval      L29ctb     = ctb199
301200980930     C                   eval      L29kcc     = kcc199
301300980930     C                   eval      L29ksc     = ksc199
301400980930     C                   eval      l29INIZ    = Wl29INIZ
301500980930     C                   clear                   kpjbu
301600980930     C                   movel     ndcl29ds      kpjbu
301700980930     C                   CALL      'NDCL29R'
301800980930     C                   PARM                    KPJBA
301900980930     C                   movel     kpjbu         ndcl29ds
302000980930     C*
302100980930     C* metto 1 in  wl29iniz per non far fare più la close
302200980930     C                   eval      wL29iniz = '1'
302300980930     c* F12
302400980930    >C     L29RET        ifeq      '2'
302500980930     C                   MOVE      *ON           $ESCI
302600980930     C                   endif
302700980930     C                   endif
302800980930     C*
302900980930     C                   MOVE      *HIVAL        $LASTV
303000981002     C*
303100981002     C* OPZIONI DI RIGA
303200981002     C* se opz=21 richiamo analitica
303300981002     C     S1OPZ         WHENEQ    21
303400981002     C*
303500981002     C                   RESET                   NDC083DS
303600981002     C                   MOVEL     H1Sys         Sys083
303700981002     C                   MOVEL     H1NrAsReg     NrAsReg083
303800981002     C                   MOVEL     H1NrRigaM     NrRigaM083
303900981002     C                   MOVE      '02'          Opz083
304000981002     C*
304100981002     C                   movel     NDC083DS      KPJBU
304200981002     C                   CALL      'NDC083R'
304300981002     C                   PARM                    Kpjba
304400981002     C                   Movel     kpjbu         NDC083DS
304500981002     C*
304600981002     C                   Exsr      Rtn083
304700981002     C*
304800981002     C* se opz=22 richiamo visualizzazione partite
304900981002     C* passo la chiave al pgm chiamante
305000981002     C     S1OPZ         WHENEQ    22
305100981002     C*
305200981002     C                   RESET                   NDC073DS
305300981002     c                   move      xscsoc        societa073
305400981002     C                   MOVEL     H1Sys         Sys073
305500981002     C                   MOVEL     H1NrAsReg     NrAsReg073
305600981002     C                   MOVEL     H1NrRigaM     NrRigaM073
305700981002     C                   MOVEL     H1NrRigaP     NrRigaP073
305800981002     C                   movel     descr199      kcckscd073
305900981002     c                   eval      snatura073 = snatura199
306000981002     C                   move      winz073       exf073
306100981002     C                   MOVE      '05'          Opz073
306200981002     C*
306300981002     C                   movel     NDC073DS      KPJBU
306400981002     C                   CALL      'NDC073R'
306500981002     C                   PARM                    Kpjba
306600981002     C                   Movel     kpjbu         NDC073DS
306700981002     C*
306800981002     C                   Exsr      Rtn073
306900051109c1881c* se opz=tesoreria
307000051109  "  C     S1OPZ         WHENEQ    23
307100051109  "  C*
307200051109  "  C                   RESET                   NDC084DS
307300051109  "  C                   MOVEL     H1Sys         Sys084
307400051109  "  C                   z-add     H1NrAsReg     NrAsReg084
307500051109  "  C                   z-add     H1NrRigaM     NrRigaM084
307600051109  "  C                   MOVE      '02'          Opz084
307700051109  "  C                   move      wdw084wrk     exf084
307800051109  "  C*
307900051109  "  C                   movel     NDC084DS      KPJBU
308000051109  "  C                   CALL      'NDC084R'
308100051109  "  C                   PARM                    Kpjba
308200051109  "  C                   Movel     kpjbu         NDC084DS
308300051109  "  C*
308400051109c1881C                   Exsr      Rtn084
308500981002     C*
308600981002     C* se opz=43 chiamo portafoglio attivo se cliente o passivo se fornitor
308700981002     C     S1OPZ         WHENEQ    43
308800981002     C* portafoglio attivo
308900981002     c                   select
309000981002     c                   when      snatura199 = 'C'
309100981002     c                   reset                   x48chPgmds
309200981002     C                   eval      x48pgm = 'NDCE01R'
309300981002     c                   eval      kpjbu = x48chpgmds
309400981002     c                   call      'X48CHPGM'
309500981002     c                   parm                    kpjba
309600981002     c                   movel     kpjbu         x48chpgmds
309700981002     c                   if        x48errore = *off
309800981002     C                   RESET                   NDCE01DS
309900981002     C                   eval      opze01    = '02'
310000981002     C                   eval      societae01= xscsoc
310100981002     C                   eval      ctbe01    = ctb199
310200981002     C                   eval      nreffE01  = s1nrreg
310300981002     C                   movel     NDCE01DS      Kpjbu
310400981002     C                   CALL      'NDCE01R'
310500981002     C                   PARM                    KPJBA
310600981002     C                   movel     kpjbu         NDce01ds
310700981002     C*
310800981002     C* ritorno da PGM gestione
310900981002     C                   EXSR      RTNCE01
311000981002     C                   endif
311100981002     c* Portafoglio passivo
311200981002     c                   when      snatura199 = 'F'
311300981002     c                   reset                   x48chPgmds
311400981002     C                   eval      x48pgm = 'NDCH01R'
311500981002     c                   eval      kpjbu = x48chpgmds
311600981002     c                   call      'X48CHPGM'
311700981002     c                   parm                    kpjba
311800981002     c                   movel     kpjbu         x48chpgmds
311900981002     c                   if        x48errore = *off
312000981002     C                   RESET                   NDCH01DS
312100981002     C                   eval      opzH01    = '02'
312200981002     C                   eval      societaH01= xscsoc
312300981002     C                   eval      ctbH01    = ctb199
312400981002     C                   eval      nreffH01  = s1nrreg
312500981002     C                   movel     NDCH01DS      Kpjbu
312600981002     C                   CALL      'NDCH01R'
312700981002     C                   PARM                    KPJBA
312800981002     C                   movel     kpjbu         NDcH01ds
312900981002     C* ritorno da PGM gestione
313000981002     C                   EXSR      RTNCH01
313100981002     C                   endif
313200981002     C                   endsl
313300981002     C*
313400981002     C* se opz=44 o 45 descrizioni aggiuntive
313500981002     C     S1OPZ         WHENEQ    44
313600981002     C     S1OPZ         oreq      45
313700981002     C                   RESET                   NDC027DS
313800981002     C*
313900981002     C                   MOVEL     '02'          OPZ027
314000981002     C* passare nei campi appositi della DS NDC027DS il valore
314100981002     C* della chiave della VL univoca
314200981002     C                   MOVEL     H1sys         Sys027
314300981002     C                   MOVEL     h1nrasreg     NrAsReg027
314400981002     c                   if        S1opz = 45
314500981002     C                   MOVEL     h1nrrigam     NrRigaM027
314600981002     C                   MOVEL     'M'           TipDes027
314700981002     C                   else
314800981002     C                   MOVEL     'T'           TipDes027
314900981002     C                   endif
315000981002     C* Faccio eseguire il commit al pgm chiamato
315100981002     C                   MOVE      '0'           Cmt027
315200981002     C                   MOVEL     *on           Inz027
315300981002     C*
315400981002     C* passo tutti quei campi che possono servire al pgm
315500981002     C                   MOVEL     NDC027DS      KPJBU
315600981002     C                   CALL      'NDC027R'
315700981002     C                   PARM                    KPJBA
315800981002     C                   MOVEL     KPJBU         NDC027DS
315900981002     C* ritorno da PGM gestione
316000981002     C                   EXSR      RTN027
316100981002     C*
316200981002     C* Richiamo prima nota
316300981009     C     S1OPZ         WHENEQ    41
316400981002     c*controllo chiamata ricorsiva p.nota
316500981002     c                   reset                   x48chPgmds
316600981002     C                   eval      x48pgm = 'NDC001R'
316700981002     c                   eval      kpjbu = x48chpgmds
316800981002     c                   call      'X48CHPGM'
316900981002     c                   parm                    kpjba
317000981002     c                   movel     kpjbu         x48chpgmds
317100981002     c                   if        x48errore = *off
317200981002      *
317300981002     C                   RESET                   NDC001DS
317400981002     C                   MOVEL     '02'          OPZ001
317500981002     C* passare nei campi appositi della DS C004DS il valore
317600981002     C* della chiave della VL univoca
317700981002     C                   MOVEL     H1Sys         UQ1001
317800981002     C                   MOVEL     H1NrAsReg     UQ2001
317900981002     C*
318000981002     C                   movel     NDC001DS      KPJBU
318100981002     C                   CALL      'NDC001R'
318200981002     C                   PARM                    Kpjba
318300981002     C                   movel     KPJBU         ndc001ds
318400981002     C* ritorno da PGM gestione
318500981002     C                   EXSR      RTN001
318600981002     c                   endif
318700031119c1460C* se opz=ordini
318800010119  "  C* richiamo il pgm di interfaccia richiamo ordini cli/for
318900031119C1460C     S1OPZ         WHENEQ    60
319000031119D1498X*                  eval      Societa    = xscsoc
319100031119  "  X*                  eval      CliFor     = Snatura199
319200031119  "  X*                  eval      Codice     = ksc199
319300031119  "  X* se clienti
319400031119  "  X*                  if        snatura199 = 'C' or
319500031119  "  X* o se fornitori di Tile
319600031119  "  X*                            (snatura199 = 'F' and xscgest = '2')
319700031119  "  X*                  move      '5'           Modalita
319800031119  "  X* passo dati documento
319900031119  "  X*                  CALLb     'XDT4000'
320000031119  "  X*    Data          PARM                    XDTAMG
320100031119  "  X*                  PARM      s1dtdoc       XDTGMA
320200031119  "  X*                  PARM      4             XDTSTA
320300031119  "  X*                  move      s1nrdoc       Numero
320400031119  "  X*                  move      s1serdoc      Serie
320500031119  "  X* se fornitori distribuzione
320600031119  "  X* bisogna recuperare  il numero/data registrazione
320700031119  "  X* della fattura che è il numero protocollo e passare quello
320800031119  "  X*                  else
320900031119  "  X*                  move      '0'           Modalita
321000031119  "  X*                  eval      RegSys = H1sys
321100031119  "  X*                  eval      RegNrasReg = H1NrasGen
321200031119  "  X*    k02Reg01      chain     NdREg01l                           21
321300031119  "  X*                  if        *in21 = *off
321400031119  "  X*                  eval      Serie = RegSerReg
321500031119  "  X*                  eval      Numero = RegNrReg
321600031119  "  X*                  eval      Data   = RegDtReg
321700031119  "  X*                  endif
321800031119  "  X*                  endif
321900031119  "  X* codice di fatturazione
322000031119  "  X*                  move      *on           CodFatt
322100031119  "  X*                  CALL      'ANA396R'
322200031119  "  X*                  PARM                    KPJBA
322300031119  "  X*                  PARM                    Societa           3
322400031119  "  X*                  PARM                    Codice            8
322500031119  "  X*                  PARM                    CliFor            1
322600031119  "  X*                  PARM                    CodFatt           1
322700031119  "  X*                  PARM                    Modalita          1
322800031119  "  X*                  PARM                    Data             10
322900031119  "  X*                  PARM                    Numero            9 0
323000031119  "  X*                  PARM                    Serie             4
323100031119  "  X*                  PARM                    Ritorno           1
323200031119  "  c*
323300031119  "  C                   RESET                   ana396ds
323400031119  "  c                   eval      Societa396 = xscsoc
323500031119  "  c                   eval      kcc396     = kcc199
323600031119  "  c                   eval      ksc396     = ksc199
323700031119  "  c                   eval      sys396     = h1sys
323800031120  "  C* Se tipo riga partita
323900031120  "  c                   if        h1tiporiga = 'P'
324000031120  "  c                   eval      modalit396 = '2'
324100031119  "  c                   eval      nrasint396 = h1nrasint
324200031120  "  c                   else
324300031120  "  C* Se tipo riga movimento
324400031120  "  c                   eval      modalit396 = '3'
324500031120  "  c                   eval      nrasreg396 = h1nrasreg
324600031120  "  c                   eval      nrrigam396 = h1nrrigam
324700031120  "  c                   endif
324800031119  "  c                   eval      codFatt396 = *on
324900031119  "  c                   eval      kpjbu=ana396ds
325000031119  "  c                   CALL      'ANA396R'
325100031119  "  C                   parm                    kpjba
325200031119  "  c                   eval      ana396ds=kpjbu
325300031119  "  c* F12
325400031119  "  C     Ritorno396    ifeq      '2'
325500031119D1498X***  Ritorno       ifeq      '2'
325600031119C1460C                   MOVE      *ON           $ESCI
325700010119  "  C                   endif
325800031119C1460C*  F3
325900031119D1498X***  Ritorno       IFEQ      '1'
326000031119D1498C     Ritorno396    IFEQ      '1'
326100031119C1460C                   MOVE      *ON           $ESCI
326200010119  "  C                   MOVE      *ON           $FINE
326300010119  "  C                   MOVE      '1'           RET199
326400010119  "  C                   endif
326500010119  "  C*
326600010119C1460C                   MOVE      *HIVAL        $LASTV
326700030206D1270C*
326800030206  "  C* Richiamo Laguna per visualizzazione documento
326900030206  "  C     S1OPZ         WHENEQ    80
327000030206  "  C                   MOVEL     H1ClassLg     Parm670(1)
327100030206  "  C                   MOVEL     H1IdLgn       Parm670(2)
327200071019D1270C                   MOVEL     knsif         Parm670(3)
327300071019C2055x*** ex D1270       CALL      'ANA670R'
327400071019C2055C                   CALL      'PJXX038R'
327500071019D1270C                   PARM                    xscsoc
327600030206  "  C                   PARM      'VIEW'        Cmd670
327700030206  "  C                   PARM                    Parm670
327800030206  "  C                   PARM                    Err670
327900030206  "  C                   PARM                    Ret670
328000030206  "  C* ritorno da PGM gestione
328100030206  "  C                   EXSR      RTN670
328200030206D1270C*
328300980930     C*
328400980930     C* se altre opzioni
328500980930     C* richiamo pgm gestione anagrafica
328600980930     C* anche per opzione blocco/sblocco, dove eseguirò solo aggiornamento
328700980930     C* archivio
328800980930     C                   OTHER
328900980930     C*
329000980930     C                   MOVE      *HIVAL        $LASTV
329100980930     C*
329200980930     C*
329300980930     C                   ENDSL
329400980930     C*
329500980930     C                   ENDSR
329600980930     C/EJECT
329700980930     C************************************************************
329800980930     C* GESTIONE RITORNO DA PGM DI particolarità partita
329900980930     C************************************************************
330000980930    >C     RTN060        BEGSR
330100980930     C*
330200980930     C* modo di ritorno
330300980930     C*
330400980930     C                   SELECT
330500980930     C* questi modi di utilizzo dei valori di ritorno dal
330600980930     C* pgm di manutenzione rcd di anagrafica sono delle
330700980930     C* proposte, normalmente sempre valide, ma modificabili
330800980930     C* per situazioni particolari
330900980930     C* 1 = F3
331000980930    >C     RET060        WHENEQ    '1'
331100980930     C                   MOVE      *ON           $ESCI
331200980930     C                   MOVE      *ON           $FINE
331300980930     C                   MOVE      '1'           RET199
331400980930     C* 2 = F12 (ma non c'è un aggiornamento)
331500980930    >C     RET060        WHENEQ    '2'
331600980930     C     OPR060        ANDNE     '1'
331700980930     C                   MOVE      *ON           $ESCI
331800980930     C*
331900980930     C                   ENDSL
332000980930     C*
332100980930     C* operazione eseguite dal pgm chiamato
332200980930     C*
332300980930    >C                   MOVE      OPR060        WOPR
332400980930     C                   SELECT
332500980930     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
332600980930    >C     OPR060        WHENEQ    '1'
332700980930     C* Se cliente/ fornitore ( sottonatura = 'C o F') leggo file estensione
332800980930     C* se non cliente o fornitore non interessa
332900980930     C                   IF        snatura199 = 'C' or snatura199 = 'F'
333000980930     C                   eval      ppasys = h1sys
333100980930     C                   eval      ppanrasint = h1nrasint
333200980930     C     K02PPA01      CHAIN     NDPPS01                            23
333300980930     C                   if        *in23 = *off
333400980930     C                   exsr      aggs1
333500980930     C                   endif
333600980930     C                   ENDIF
333700980930     C*
333800980930     C                   ENDSL
333900980930     C*
334000980930     C* funzione non eseguibile per errore :
334100980930     C*
334200980930     C                   SELECT
334300980930     C* 1 = funzione richiamata chiusa in errore
334400980930    >C     ERR060        WHENEQ    '1'
334500980930     C                   MOVE      *ON           $ESCI
334600980930     C                   SETON                                        9599
334700980930     C* 2 = funzione richiamata senza la dovuta autorizzazione
334800980930    >C     ERR060        WHENEQ    '2'
334900980930     C                   MOVE      *ON           $ESCI
335000980930     C                   SETON                                        9499
335100980930     C*
335200980930     C                   ENDSL
335300980930     C*
335400980930     C                   ENDSR
335500980930     C/EJECT
335600980930     C************************************************************
335700980930     C* GESTIONE RITORNO DA PGM DI GESTIONE MIX DI PARTITA
335800980930     C************************************************************
335900980930    >C     RTN065        BEGSR
336000980930     C*
336100980930     C* modo di ritorno
336200980930     C*
336300980930     C                   SELECT
336400980930     C* questi modi di utilizzo dei valori di ritorno dal
336500980930     C* pgm di manutenzione rcd di anagrafica sono delle
336600980930     C* proposte, normalmente sempre valide, ma modificabili
336700980930     C* per situazioni particolari
336800980930     C* 1 = F3
336900980930    >C     RET065        WHENEQ    '1'
337000980930     C                   MOVE      *ON           $ESCI
337100980930     C                   MOVE      *ON           $FINE
337200980930     C                   MOVE      '1'           RET199
337300980930     C* 2 = F12
337400980930    >C     RET065        WHENEQ    '2'
337500980930     C     OPR065        ANDNE     '1'
337600980930     C                   MOVE      *ON           $ESCI
337700980930     C*
337800980930     C                   ENDSL
337900980930     C*
338000980930     C* operazione eseguite dal pgm chiamato
338100980930     C*
338200980930    >C                   MOVE      OPR065        WOPR
338300980930     C                   SELECT
338400980930     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
338500980930    >C     OPR065        WHENEQ    '1'
338600980930     C* non viene eseguito il ricaricamento apposta perchè tanto il saldo
338700980930     c* non viene ricalcolato apposta per rendere il programma più veloce
338800980930     C*******            MOVE      *ON           WINZS1
338900980930     C*
339000980930     C                   ENDSL
339100980930     C*
339200980930     C* funzione non eseguibile per errore :
339300980930     C*
339400980930     C                   SELECT
339500980930     C* 1 = funzione richiamata chiusa in errore
339600980930    >C     ERR065        WHENEQ    '1'
339700980930     C                   MOVE      *ON           $ESCI
339800980930     C                   SETON                                        9599
339900980930     C* 2 = funzione richiamata senza la dovuta autorizzazione
340000980930    >C     ERR065        WHENEQ    '2'
340100980930     C                   MOVE      *ON           $ESCI
340200980930     C                   SETON                                        9499
340300980930     C*
340400980930     C                   ENDSL
340500980930     C*
340600980930     C                   ENDSR
340700980930     C/EJECT
340800980930     C************************************************************
340900980930     C* AGGIORNAMENTO RIGA LISTA
341000980930     C************************************************************
341100980930     C     AGGS1         BEGSR
341200980930     C*
341300980930     C* riempio i campi del sfl che sono stati ripassati dal pgm
341400980930     C* di manutenzione dopo una opzione 02
341500980930     C                   CALLB     'XDT4000'
341600980930     C                   PARM      PPSDTDoc      XDTAMG
341700980930     C     S1DTDoc       PARM                    XDTGMA
341800980930     C                   PARM      2             XDTSTA
341900980930     C                   EVAL      S1nrDOC = PPSnrDOC
342000980930     C**                 EVAL      S1serdoc = PPSserDOC
342100980930     C*
342200980930     C                   ENDSR
342300980930     C/EJECT
342400980930     C************************************************************
342500980930     C* GESTIONE RITORNO DA PGM DEI COMMENTI
342600980930     C************************************************************
342700980930     C     RTN725        BEGSR
342800980930     C*
342900980930     C* modo di ritorno
343000980930     C*
343100980930     C                   SELECT
343200980930     C* questi modi di utilizzo dei valori di ritorno dal
343300980930     C* pgm di manutenzione rcd di anagrafica sono delle
343400980930     C* proposte, normalmente sempre valide, ma modificabili
343500980930     C* per situazioni particolari
343600980930     C* 1 = F3
343700980930     C     RET725        WHENEQ    '1'
343800980930     C                   MOVE      *ON           $ESCI
343900980930     C                   MOVE      *ON           $FINE
344000980930     C                   MOVE      '1'           RET199
344100980930     C* 2 = F12
344200980930     C     RET725        WHENEQ    '2'
344300980930     C     OPR725        ANDNE     '1'
344400980930     C                   MOVE      *ON           $ESCI
344500980930     C*
344600980930     C                   ENDSL
344700980930     C*
344800980930     C* operazione eseguite dal pgm chiamato
344900980930     C*
345000980930     C                   SELECT
345100980930     C* 1 = eseguita scrittura o modifica commento
345200980930     C     OPR725        WHENEQ    '1'
345300980930     C*
345400980930     C                   ENDSL
345500980930     C*
345600980930     C* funzione non eseguibile per errore :
345700980930     C*
345800980930     C                   SELECT
345900980930     C* 1 = funzione richiamata chiusa in errore
346000980930     C     ERR725        WHENEQ    '1'
346100980930     C                   MOVE      *ON           $ESCI
346200980930     C                   SETON                                        9599
346300980930     C* 2 = funzione richiamata senza la dovuta autorizzazione
346400980930     C     ERR725        WHENEQ    '2'
346500980930     C                   MOVE      *ON           $ESCI
346600980930     C                   SETON                                        9499
346700980930    >C*  eventualmente gestire altri codici di errore
346800980930     C*
346900980930     C                   ENDSL
347000980930     C*
347100980930     C                   ENDSR
347200981002     C/EJECT
347300981002     C************************************************************
347400981002     C* GESTIONE RITORNO DA PGM analitica
347500981002     C************************************************************
347600981002     C     RTN083        BEGSR
347700981002     C*
347800981002     C* modo di ritorno
347900981002     C*
348000981002   B1C                   SELECT
348100981002     C* 1 = F3
348200981002     C     RET083        WHENEQ    '1'
348300981002     C                   MOVE      *ON           $ESCI
348400981002     C                   MOVE      *ON           $FINE
348500981002     C                   MOVE      '1'           RET199
348600981002     C* 2 = F12
348700981002     C     RET083        WHENEQ    '2'
348800981002     c     opr083        andne     '1'
348900981002     C                   MOVE      *ON           $ESCI
349000981002     C*
349100981002   E1C                   ENDSL
349200981002     C*
349300981002     C* operazione eseguite dal pgm chiamato
349400981002     C*
349500981002    >C                   MOVE      OPR083        WOPR
349600981002     C                   SELECT
349700981002     C* 1 = eseguito aggiornamento non faccio niente
349800981002    >C     OPR083        WHENEQ    '1'
349900981002     C                   move      *ON           winzs1
350000981002     C*
350100981002     C                   ENDSL
350200981002     C*
350300981002     C* funzione non eseguibile per errore :
350400981002     C*
350500981002     C                   SELECT
350600981002     C* 1 = funzione richiamata chiusa in errore
350700981002    >C     ERR083        WHENEQ    '1'
350800981002     C                   MOVE      *ON           $ESCI
350900981002     C                   SETON                                        9599
351000981002     C* 2 = funzione richiamata senza la dovuta autorizzazione
351100981002    >C     ERR083        WHENEQ    '2'
351200981002     C                   MOVE      *ON           $ESCI
351300981002     C                   SETON                                        9499
351400981002     C*
351500981002     C                   ENDSL
351600981002     C*
351700981002     C                   ENDSR
351800051109c1881C************************************************************
351900051109     C* GESTIONE RITORNO DA PGM tesoreria
352000051109c1881C************************************************************
352100051109     C     RTN084        BEGSR
352200051109     C*
352300051109     C* modo di ritorno
352400051109     C*
352500051109   B1C                   SELECT
352600051109     C* 1 = F3
352700051109     C     RET084        WHENEQ    '1'
352800051109     C                   MOVE      *ON           $ESCI
352900051109     C                   MOVE      *ON           $FINE
353000051109     C                   MOVE      '1'           RET199
353100051109     C* 2 = F12
353200051109     C     RET084        WHENEQ    '2'
353300051109     c     opr084        andne     *on
353400051109     C                   MOVE      *ON           $ESCI
353500051109     C*
353600051109   E1C                   ENDSL
353700051109     C*
353800051109     C* operazione eseguite dal pgm chiamato
353900051109     C*
354000051109    >C                   MOVE      OPR084        WOPR
354100051109     C                   SELECT
354200051109     C* 1 = eseguito aggiornamento
354300051109    >C     OPR084        WHENEQ    '1'
354400051109     C                   move      *ON           winzs1
354500051109     C*
354600051109     C                   ENDSL
354700051109     C*
354800051109     C* funzione non eseguibile per errore :
354900051109     C*
355000051109     C                   SELECT
355100051109     C* 1 = funzione richiamata chiusa in errore
355200051109    >C     ERR084        WHENEQ    '1'
355300051109     C                   MOVE      *ON           $ESCI
355400051109     C                   SETON                                        9599
355500051109     C* 2 = funzione richiamata senza la dovuta autorizzazione
355600051109    >C     ERR084        WHENEQ    '2'
355700051109     C                   MOVE      *ON           $ESCI
355800051109     C                   SETON                                        9499
355900051109     C*
356000051109     C                   ENDSL
356100051109     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
356200051109     C* di sola finestra
356300051109     C                   MOVEL     '0'           wdw084wrk
356400051109     C*
356500051109c1881C                   ENDSR
356600051109     C/EJECT
356700981002     C************************************************************
356800981002     C* GESTIONE RITORNO DA PGM PARTITE
356900981002     C************************************************************
357000981002     C     RTN073        BEGSR
357100981002     C*
357200981002     C* modo di ritorno
357300981002     C*
357400981002   B1C                   SELECT
357500981002     C* 1 = F3
357600981002     C     RET073        WHENEQ    '1'
357700981002     C                   MOVE      *ON           $ESCI
357800981002     C                   MOVE      *ON           $FINE
357900981002     C                   MOVE      '1'           RET199
358000981002     C* 2 = F12
358100981002     C     RET073        WHENEQ    '2'
358200981002     c     opr073        andne     '1'
358300981002     C                   MOVE      *ON           $ESCI
358400981002     C*
358500981002   E1C                   ENDSL
358600981002     C*
358700981002     C* operazione eseguite dal pgm chiamato
358800981002     C*
358900981002    >C                   MOVE      OPR073        WOPR
359000981002     C                   SELECT
359100981002     C* 1 = eseguito aggiornamento non faccio niente
359200981002    >C     OPR073        WHENEQ    '1'
359300981002     C                   move      *ON           winzs1
359400981002     C*
359500981002     C                   ENDSL
359600981002     C*
359700981002     C* funzione non eseguibile per errore :
359800981002     C*
359900981002     C                   SELECT
360000981002     C* 1 = funzione richiamata chiusa in errore
360100981002    >C     ERR073        WHENEQ    '1'
360200981002     C                   MOVE      *ON           $ESCI
360300981002     C                   SETON                                        9599
360400981002     C* 2 = funzione richiamata senza la dovuta autorizzazione
360500981002    >C     ERR073        WHENEQ    '2'
360600981002     C                   MOVE      *ON           $ESCI
360700981002     C                   SETON                                        9499
360800981002     C*
360900981002     C                   ENDSL
361000981002     C* disattivo il flag di avvenuta EXFMT dopo il richiamo a pgm
361100981002     C* di sola finestra
361200981002     C                   MOVEL     '0'           winz073
361300981002     C*
361400981002     C                   ENDSR
361500981002     C/EJECT
361600981002     C************************************************************
361700981002     C* GESTIONE RITORNO DA PGM PORTAFOGLIO ATTIVO
361800981002     C************************************************************
361900981002     C     RTNCE01       BEGSR
362000981002     C*
362100981002     C* modo di ritorno
362200981002     C*
362300981002   B1C                   SELECT
362400981002     C* 1 = F3
362500981002     C     RETE01        WHENEQ    '1'
362600981002     C                   MOVE      *ON           $ESCI
362700981002     C                   MOVE      *ON           $FINE
362800981002     C                   MOVE      '1'           RET199
362900981002     C* 2 = F12
363000981002     C     RETE01        WHENEQ    '2'
363100981002     C     oprE01        ANDNE     '1'
363200981002     C                   MOVE      *ON           $ESCI
363300981002     C*
363400981002   E1C                   ENDSL
363500981002     C*
363600981002     C* operazione eseguite dal pgm chiamato
363700981002     C*
363800981002    >C                   MOVE      OPRE01        WOPR
363900981002     C                   SELECT
364000981002     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
364100981002     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
364200981002    >C     OPRE01        WHENEQ    '1'
364300981002     C                   move      *ON           winzs1
364400981002     C*
364500981002     C                   ENDSL
364600981002     C*
364700981002     C* funzione non eseguibile per errore :
364800981002     C*
364900981002     C                   SELECT
365000981002     C* 1 = funzione richiamata chiusa in errore
365100981002    >C     ERRE01        WHENEQ    '1'
365200981002     C                   MOVE      *ON           $ESCI
365300981002     C                   SETON                                        9599
365400981002     C*
365500981002     C                   ENDSL
365600981002     C*
365700981002     C                   ENDSR
365800981002     C/EJECT
365900981002     C************************************************************
366000981002     C* GESTIONE RITORNO DA PGM PORTAFOGLIO PASSIVO
366100981002     C************************************************************
366200981002     C     RTNCH01       BEGSR
366300981002     C*
366400981002     C* modo di ritorno
366500981002     C*
366600981002   B1C                   SELECT
366700981002     C* 1 = F3
366800981002     C     RETH01        WHENEQ    '1'
366900981002     C                   MOVE      *ON           $ESCI
367000981002     C                   MOVE      *ON           $FINE
367100981002     C                   MOVE      '1'           RET199
367200981002     C* 2 = F12
367300981002     C     RETH01        WHENEQ    '2'
367400981002     C     oprH01        ANDNE     '1'
367500981002     C                   MOVE      *ON           $ESCI
367600981002     C*
367700981002   E1C                   ENDSL
367800981002     C*
367900981002     C* operazione eseguite dal pgm chiamato
368000981002     C*
368100981002    >C                   MOVE      OPRh01        WOPR
368200981002     C                   SELECT
368300981002     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
368400981002     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
368500981002    >C     OPRh01        WHENEQ    '1'
368600981002     C                   move      *ON           winzs1
368700981002     C*
368800981002     C                   ENDSL
368900981002     C*
369000981002     C* funzione non eseguibile per errore :
369100981002     C*
369200981002     C                   SELECT
369300981002     C* 1 = funzione richiamata chiusa in errore
369400981002    >C     ERRH01        WHENEQ    '1'
369500981002     C                   MOVE      *ON           $ESCI
369600981002     C                   SETON                                        9599
369700981002     C*
369800981002     C                   ENDSL
369900981002     C*
370000981002     C                   ENDSR
370100981002     C/EJECT
370200981002     C************************************************************
370300981002     C* GESTIONE RITORNO DA PGM DESCRIZIONI AGGIUNTIVE
370400981002     C************************************************************
370500981002     C     RTN027        BEGSR
370600981002     C*
370700981002     C* modo di ritorno
370800981002     C*
370900981002   B1C                   SELECT
371000981002     C* 1 = F3
371100981002     C     RET027        WHENEQ    '1'
371200981002     C                   MOVE      *ON           $ESCI
371300981002     C                   MOVE      *ON           $FINE
371400981002     C                   MOVE      '1'           RET199
371500981002     C* 2 = F12
371600981002     C     RET027        WHENEQ    '2'
371700981002     C     opr027        andne     '1'
371800981002     C                   MOVE      *ON           $ESCI
371900981002     C*
372000981002   E1C                   ENDSL
372100981002     C*
372200981002     C* operazione eseguite dal pgm chiamato
372300981002     C*
372400981002    >C                   MOVE      OPR027        WOPR
372500981002     C                   SELECT
372600981002     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
372700981002     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
372800981002    >C     OPR027        WHENEQ    '1'
372900981002     C* verifico se esistono descrizioni aggiuntive(riga)
373000981002     C* non lo faccio perchè non verranno quasi mai modificate
373100981002     C*****              EXSR      TestDesAGG
373200981002     C*
373300981002     C                   ENDSL
373400981002     C*
373500981002     C* funzione non eseguibile per errore :
373600981002     C*
373700981002     C                   SELECT
373800981002     C* 1 = funzione richiamata chiusa in errore
373900981002    >C     ERR027        WHENEQ    '1'
374000981002     C                   MOVE      *ON           $ESCI
374100981002     C                   SETON                                        9599
374200981002     C* 2 = funzione richiamata senza la dovuta autorizzazione
374300981002    >C     ERR027        WHENEQ    '2'
374400981002     C                   MOVE      *ON           $ESCI
374500981002     C                   SETON                                        9499
374600981002     C*
374700981002     C                   ENDSL
374800981002     C*
374900981002     C                   ENDSR
375000981002     C/EJECT
375100981002     C************************************************************
375200981002     C* GESTIONE RITORNO DA PGM primanota generica
375300981002     C************************************************************
375400981002     C     RTN001        BEGSR
375500981002     C*
375600981002     C* modo di ritorno
375700981002     C*
375800981002   B1C                   SELECT
375900981002     C* 1 = F3
376000981002     C     RET001        WHENEQ    '1'
376100981002     C                   MOVE      *ON           $ESCI
376200981002     C                   MOVE      *ON           $FINE
376300981002     C                   MOVE      '1'           RET199
376400981002     C* 2 = F12
376500981002     C     RET001        WHENEQ    '2'
376600981002     C     opr001        ANDNE     '1'
376700981002     C                   MOVE      *ON           $ESCI
376800981002     C*
376900981002   E1C                   ENDSL
377000981002     C*
377100981002     C* operazione eseguite dal pgm chiamato
377200981002     C*
377300981002    >C                   MOVE      OPR001        WOPR
377400981002     C                   SELECT
377500981002     C* 1 = eseguito aggiornamento (richiesto cariamento subfile
377600981002     C* testo esistenza descrizioni aggiuntive e modifico campo memorizzaz
377700981002    >C     OPR001        WHENEQ    '1'
377800981002     C                   move      *ON           winzs1
377900981002     C*
378000981002     C                   ENDSL
378100981002     C*
378200981002     C* funzione non eseguibile per errore :
378300981002     C*
378400981002     C                   SELECT
378500981002     C* 1 = funzione richiamata chiusa in errore
378600981002    >C     ERR001        WHENEQ    '1'
378700981002     C                   MOVE      *ON           $ESCI
378800981002     C                   SETON                                        9599
378900981002     C* 2 = funzione richiamata senza la dovuta autorizzazione
379000981002    >C     ERR001        WHENEQ    '2'
379100981002     C                   MOVE      *ON           $ESCI
379200981002     C                   SETON                                        9499
379300981002     C* 3 = registrazione non modificabile
379400981002     C     ERR001        WHENEQ    '3'
379500981002     C                   MOVE      *ON           $ESCI
379600981002     C                   SETON                                        9399
379700981002     C*
379800981002     C                   ENDSL
379900981002     C*
380000981002     C*
380100981002     C                   ENDSR
380200030206D1270C************************************************************
380300030206  "  C* GESTIONE RITORNO DA PGM visualizzazione documenti da Laguna
380400030206  "  C************************************************************
380500030206D1270C     RTN670        BEGSR
380600030206     C*
380700030206     C* modo di ritorno
380800030206     C*
380900030206     C* al ritorno da questa opzione non continuo il ciclo
381000030206     C                   MOVE      *ON           $ESCI
381100030206     C*
381200030206D1270C                   ENDSR
381300980930     C/EJECT
381400980930     C************************************************************
381500980930     C* CONTROLLO ABILITAZIONE TASTI
381600980930     C* INPUT
381700980930     C* $TASTO =Tasto funzione premuto
381800980930     C* $FC = Tasti abilitati
381900980930     C* OUTPUT
382000980930     C* IN99 = ERrore
382100980930     C************************************************************
382200980930     C     CTRKEY        BEGSR
382300980930     C*
382400980930     C                   SELECT
382500980930     C     $TASTO        WHENEQ    F01
382600980930     C     $FC(1)        IFEQ      '0'
382700980930     C                   SETON                                        99
382800980930     C                   ENDIF
382900980930     C     $TASTO        WHENEQ    F02
383000980930     C     $FC(2)        IFEQ      '0'
383100980930     C                   SETON                                        99
383200980930     C                   ENDIF
383300980930     C     $TASTO        WHENEQ    F03
383400980930     C     $FC(3)        IFEQ      '0'
383500980930     C                   SETON                                        99
383600980930     C                   ENDIF
383700980930     C     $TASTO        WHENEQ    F04
383800980930     C     $FC(4)        IFEQ      '0'
383900980930     C                   SETON                                        99
384000980930     C                   ENDIF
384100980930     C     $TASTO        WHENEQ    F05
384200980930     C     $FC(5)        IFEQ      '0'
384300980930     C                   SETON                                        99
384400980930     C                   ENDIF
384500980930     C     $TASTO        WHENEQ    F06
384600980930     C     $FC(6)        IFEQ      '0'
384700980930     C                   SETON                                        99
384800980930     C                   ENDIF
384900980930     C     $TASTO        WHENEQ    F07
385000980930     C     $FC(7)        IFEQ      '0'
385100980930     C                   SETON                                        99
385200980930     C                   ENDIF
385300980930     C     $TASTO        WHENEQ    F08
385400980930     C     $FC(8)        IFEQ      '0'
385500980930     C                   SETON                                        99
385600980930     C                   ENDIF
385700980930     C     $TASTO        WHENEQ    F09
385800980930     C     $FC(09)       IFEQ      '0'
385900980930     C                   SETON                                        99
386000980930     C                   ENDIF
386100980930     C     $TASTO        WHENEQ    F10
386200980930     C     $FC(10)       IFEQ      '0'
386300980930     C                   SETON                                        99
386400980930     C                   ENDIF
386500980930     C     $TASTO        WHENEQ    F11
386600980930     C     $FC(11)       IFEQ      '0'
386700980930     C                   SETON                                        99
386800980930     C                   ENDIF
386900980930     C     $TASTO        WHENEQ    F12
387000980930     C     $FC(12)       IFEQ      '0'
387100980930     C                   SETON                                        99
387200980930     C                   ENDIF
387300980930     C     $TASTO        WHENEQ    F13
387400980930     C     $FC(13)       IFEQ      '0'
387500980930     C                   SETON                                        99
387600980930     C                   ENDIF
387700980930     C     $TASTO        WHENEQ    F14
387800980930     C     $FC(14)       IFEQ      '0'
387900980930     C                   SETON                                        99
388000980930     C                   ENDIF
388100980930     C     $TASTO        WHENEQ    F15
388200980930     C     $FC(15)       IFEQ      '0'
388300980930     C                   SETON                                        99
388400980930     C                   ENDIF
388500980930     C     $TASTO        WHENEQ    F16
388600980930     C     $FC(16)       IFEQ      '0'
388700980930     C                   SETON                                        99
388800980930     C                   ENDIF
388900980930     C     $TASTO        WHENEQ    F17
389000980930     C     $FC(17)       IFEQ      '0'
389100980930     C                   SETON                                        99
389200980930     C                   ENDIF
389300980930     C     $TASTO        WHENEQ    F18
389400980930     C     $FC(18)       IFEQ      '0'
389500980930     C                   SETON                                        99
389600980930     C                   ENDIF
389700980930     C     $TASTO        WHENEQ    F19
389800980930     C     $FC(19)       IFEQ      '0'
389900980930     C                   SETON                                        99
390000980930     C                   ENDIF
390100980930     C     $TASTO        WHENEQ    F20
390200980930     C     $FC(20)       IFEQ      '0'
390300980930     C                   SETON                                        99
390400980930     C                   ENDIF
390500980930     C     $TASTO        WHENEQ    F21
390600980930     C     $FC(21)       IFEQ      '0'
390700980930     C                   SETON                                        99
390800980930     C                   ENDIF
390900980930     C     $TASTO        WHENEQ    F22
391000980930     C     $FC(22)       IFEQ      '0'
391100980930     C                   SETON                                        99
391200980930     C                   ENDIF
391300980930     C     $TASTO        WHENEQ    F23
391400980930     C     $FC(23)       IFEQ      '0'
391500980930     C                   SETON                                        99
391600980930     C                   ENDIF
391700980930     C     $TASTO        WHENEQ    F24
391800980930     C     $FC(24)       IFEQ      '0'
391900980930     C                   SETON                                        99
392000980930     C                   ENDIF
392100980930     C                   ENDSL
392200980930     C*
392300980930     C                   ENDSR
392400980930     C/EJECT
392500980930     C************************************************************
392600980930     C* OPERAZIONI INIZIALI
392700980930     C************************************************************
392800980930     C     *INZSR        BEGSR
392900000118C1309C     *DMY          Move      Udate         ZDateISO
393000000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
393100980930     C*
393200980930     C* Reperimento parametri
393300980930     C*
393400980930     C     *ENTRY        PLIST
393500980930     C                   PARM                    KPJBA
393600980930     c                   PARM                    ndc199ds
393700980930     C                   PARM                    SOC00x
393800980930     C                   PARM                    DSAUT
393900980930     C*
394000980930     C* testo se sono stati passati i dati di società
394100980930     C*  il caso che non siano arrivati i dati di società è quando il
394200980930     C*  chiamante richiede dati presenti su un altro SistemaInformativo,
394300980930     C*  in questo caso siamo in un nuovo GrpJob e il pgm si aprirà sempre
394400980930     C*  passando dall'*inzsr
394500980930     C*
394600980930     C                   IF        Parms < 3       or
394700980930     C                             %addr(SOC001) = *null
394800980930     C                   MOVE      *BLANK        SOC001
394900980930     C                   ELSE
395000980930     C                   MOVEL     SOC00x        SOC001
395100980930     C                   ENDIF
395200980930     C*
395300980930     C* Reperimento dati società
395400980930     C* potrebbero essere già stati passati,
395500980930     C* nel qual caso non vanno reperiti
395600980930    >C     SOC001        IFEQ      *BLANK
395700980930    >C                   MOVEL     'SOC001'      TIPXSC
395800980930     C                   MOVEL     Societa199    SOCXSC
395900980930     C                   EXSR      REPSOC
396000980930     C* Controlli dati società
396100980930     C     RTNXSC        IFNE      '1'
396200980930     C                   MOVEL     XSOCDS        SOC001
396300980930     C                   ELSE
396400980930    >C                   MOVE      '1'           Err199
396500980930     C                   MOVE      *on           $Fine
396600980930     C                   EXSR      EndPgm
396700980930     C                   ENDIF
396800980930     C                   ENDIF
396900980930     C*
397000980930     C* testo se sono stati passati i dati di autorizzazione
397100980930     C*
397200980930     C                   IF        Parms < 4       or
397300980930     C                             %addr(AUTDS) = *null
397400980930     C                   MOVE      *BLANK        AUTDS
397500980930     C                   ELSE
397600980930     C                   MOVEL     DSAUT         AUTDS
397700980930     C                   ENDIF
397800980930     C*
397900980930     C* Reperimento autorizzazioni
398000980930     C* le autorizzazioni potrebbero essere già state passate,
398100980930     C* nel qual caso non vanno reperite
398200980930    >C     AUTDS         IFEQ      *BLANK
398300980930    >C                   EXSR      REPAUT
398400980930     c                   else
398500980930     c                   movel     xcadds        coneccds
398600980930     C                   ENDIF
398700980930     C*
398800980930     C* Reperimento opzioni
398900980930     C*
399000980930     C                   EXSR      REPOPZ
399100980930     C*
399200980930     C* Reperimento tasti di funzione
399300980930     C*
399400980930     C                   EXSR      REPKEY
399500980930     C* Udate in iso
399600980930     C                   CALLb     'XDT4000'
399700980930     C     WDATE         PARM                    XDTAMG
399800000118C1309C*****              PARM      UDATE         XDTGMA
399900000118C1309C                   PARM      ZDate         XDTGMA
400000980930     C                   PARM      4             XDTSTA
400100980930     C*
400200980930     C                   ENDSR
400300980930     C/EJECT
400400980930     C************************************************************
400500980930     C* INIZIALIZZAZIONE VARIABILI
400600980930     C************************************************************
400700980930     C     INZVAR        BEGSR
400800980930     C*
400900980930     C* Definizione delle opzioni utilizzabili a pgm
401000980930     C*
401100980930     C                   EXSR      DEFOPZ
401200980930     C*
401300980930     C* Variabili per gestione videate
401400980930     C*
401500980930     C                   MOVE      *OFF          $FINE
401600980930     C                   MOVE      *OFF          $INZS1
401700980930     C                   MOVE      *OFF          $EndFile
401800981001     C                   MOVE      *OFF          $EndFile1
401900980930     C                   MOVE      *OFF          $ESCI
402000980930     C                   MOVE      *OFF          $RCDOK
402100980930     C                   MOVE      *HIVAL        $LASTV
402200980930     C                   Z-ADD     0             $ULKS1            3 0
402300980930     C                   MOVE      *ON           $INZS1
402400980930     C                   MOVE      'S1'          $GEST
402500980930     C                   CLEAR                   WOPR
402600980930     C                   CLEAR                   Wsaldototd
402700980930     C                   CLEAR                   Wsaldotota
402800980930     C                   CLEAR                   WsaldotoDD
402900980930     C                   CLEAR                   WsaldotoAD
403000980930     c*
403100980930     C*
403200980930     C* Valorizzazione campi univoci testate
403300980930     C*
403400980930     C                   CLEAR                   C199T1
403500980930     C                   MOVEL     KNSIF         NOMSIF
403600980930     C                   MOVEL     XSCDSI        NOMDIT
403700980930     C                   MOVEL     DSPGM         NOMPGM
403800981002     C* visualizzo il sfl troncato
403900981002     C                   Eval      c1mode=*on
404000981002     C* Reperimento sottotitolo
404100981002     C                   MOVEL     *BLANK        T1TIT
404200981002     C     SOTTIT        IFNE      *BLANK
404300981002     C                   MOVEL     SOTTIT        IDMSG
404400981002     C                   CALLB     'XRTVM'
404500981002     C                   PARM                    IDMSG            27
404600981002     C                   PARM                    TXTMSG          100
404700981002     C                   PARM                    ERRMSG            1
404800981002     C                   PARM      *ON           CTRMSG            1
404900981002     C                   PARM      30            LENMSG            3 0
405000981002     C     ERRMSG        IFNE      *ON
405100981002     C                   MOVEL     TXTMSG        T1TIT
405200981002     C                   ELSE
405300981002     C                   MOVEL     *ALL'?'       T1TIT
405400981002     C                   ENDIF
405500981002     C                   ENDIF
405600980930     C*
405700980930     C* il sottotitollo è la decodifica della situazione partite
405800980930     C* Controllo situazione partite
405900981002     C*                  MOVE      *BLANK        T1tit
406000981002     C*                  RESET                   X07DS
406100981002     C*                  MOVE      '1'           X07RIC
406200981002     C*                  MOVEL     'ANDIZ000'    X07TRC
406300981002     C*                  MOVEL     'DSPSITPAR'   X07CAM
406400981002     C*                  MOVEL     sitpar199     X07VAL
406500981002     C*                  MOVE      *OFF          X07ALL
406600981002     C*                  CALLB     'X07VALR'
406700981002     C*                  PARM                    X07DS
406800981002   B3C*    X07ERR        IFEQ      '0'
406900981002     C*                  MOVEL     x07des        t1tit
407000981002     C*                  MOVEl     t1tit         £STR            132
407100981002     C*                  MOVE      30            £LSTRC            3 0
407200980930      * Routine di centratura
407300980930      *
407400981002     C*    ' '           CHECKR    £STR          £LEN              3 0
407500981002     C*    £LSTRC        SUB       £LEN          £LEN
407600981002     C*                  DIV       2             £LEN
407700981002     C*    ' '           CAT(P)    £STR:£LEN     £STRC           132
407800981002     C*                  MOVEL     £strc         t1tit
407900980930      *
408000981002   X3C*                  Endif
408100980930     C*
408200980930     C* Reperimento dati moneta di conto
408300980930     C                   Eval      WDivisa =  XscDiv
408400980930     C                   Exsr      RepDivisa
408500980930     C                   Eval      NrDec   = X01NrDec
408600980930     C*
408700980930     C* Definisco ordinamento lettura
408800980930     C*
408900980930     C                   EXSR      DEFORD
409000980930     C*
409100980930     C*
409200980930     C* Definisco caricamento subfile
409300980930     C* Se il saldo è già stato calcolato, il caricamento è a pagina e
409400980930     c* ho già anche i flag di presenza a totale di conto
409500980930     C                   if        calcsal199 = *on
409600980930     C                   EVAL      $carpag = *on
409700981009     C                   EVAL      SFLPAG = 11
409800980930      *
409900980930      * Call routine editazione x importo in moneta di conto
410000980930     C                   Z-add     Saldo199      WImporto
410100980930     C                   Z-add     NrDec         WNrDec
410200980930     C                   Eval      $LenFld = %Size(Z1Saldo)
410300980930     c* Dare allineato a sinistra
410400980930     C                   if        Segno199 = xscdare
410500980930     C                   sub       5             $lenFld
410600980930     C                   endif
410700980930     C                   EXSR      Edit
410800980930     C                   Eval      Z1Saldo = $RcvrVar
410900980930     C                   eval      z1segno = segno199
411000980930      *
411100980930      * Call routine editazione x importo in divisa
411200980930     c                   setoff                                       17
411300980930     c                   if        divisa199 <> *blanks and divsel199 = 'I'
411400980930     c                             and divisa199 <> xscdiv
411500980930     c                   seton                                        17
411600980930     C                   EVAL      Wdivisa = divisa199
411700980930     c                   exsr      Repdivisa
411800980930     C                   Eval      NrDecD = X01NrDec
411900980930     C                   Z-add     SaldoD199     WImporto
412000980930     C                   Z-add     NrDecD        WNrDec
412100980930     C                   Eval      $LenFld = %Size(Z1SaldoDiv)
412200980930     c* Dare allineato a sinistra
412300980930     C                   if        Segno199 = xscdare
412400980930     C                   sub       5             $lenFld
412500980930     C                   endif
412600980930     C                   EXSR      Edit
412700980930     C                   Eval      Z1SaldoDiv= $RcvrVar
412800980930     C                   Eval      Z1SegnoDiv= SegnoD199
412900980930     C                   eval      z1divisa = Divisa199
413000980930     c                   endif
413100980930     C*
413200980930     C                   else
413300980930     C*
413400980930     c                   eval      $carpag = *off
413500980930     C                   EVAL      SFLPAG = 9999
413600980930     C                   eval      z1saldo = *blanks
413700980930     C                   eval      z1segno = ' '
413800980930     C                   eval      z1saldodiv = *blanks
413900980930     C                   eval      z1segnodiv = ' '
414000980930     C                   endif
414100980930     C*
414200980930     C* Variabili appoggio
414300980930     C*
414400980930     C                   Z-ADD     SFLPAG        WPAG
414500980930     C* Salvo divisa in entrata
414600980930     C                   EVAL      SavDivVis =   DivVis199
414700980930     C*
414800980930     C                   ENDSR
414900981006     C/EJECT
415000981006     C************************************************************
415100981006     C* TESTo esistenza commenti per il conto
415200981006     C************************************************************
415300981006     C     TSTTCM        BEGSR
415400981006     C*
415500981006     C                   RESET                   A724ds
415600981006     C                   MOVEL     '01'          OPZ724
415700981006     C                   MOVEL     xscsoc        SOC724
415800981006     C                   MOVEL     *blanks       UNI724
415900981006     C                   SELECT
416000981007     C     Snatura199    wheneq    'C'
416100981006     C                   MOVEL     '700'         TPA724
416200981006     C                   MOVEL     c1KSC         KY1724
416300981006     C                   MOVEL     '000'         KY3724
416400981007     C     SNATURA199    wheneq    'F'
416500981006     C                   MOVEL     '001'         TPA724
416600981006     C                   MOVEL     c1KSC         KY1724
416700981006     C                   OTHER
416800981006     C                   MOVEL     'PDC'         TPA724
416900981006     C                   MOVEL     c1KCC         KY1724
417000981006     C                   MOVEL     c1KSC         KY2724
417100981006     C                   endsl
417200981006     C                   MOVEL     Wdate         DTS724
417300981006     C                   MOVEL     A724ds        KPJBU
417400981006     C                   CALL      'ANA724R'
417500981006     C                   PARM                    KPJBA
417600981006     C                   MOVEL     KPJBU         a724ds
417700981006     C*
417800981006     C                   ENDSR
417900980930     C/EJECT
418000980930     C************************************************************
418100980930     C* Reperisco dati divisa
418200980930     C************************************************************
418300980930     C     RepDivisa     BEGSR
418400980930     C*
418500980930     C* Divisa
418600980930     C                   reset                   X01CambiDs
418700980930     C                   MOVE      xscsoc        X01societa
418800980930     C                   MOVEL     WDivisa       X01divisa
418900980930     C                   MOVEL     '1'           X01flgcam
419000980930     C                   MOVE      Wdate         x01dtRifCa
419100980930     C                   CALL      'X01CAMBI'
419200980930     C                   PARM                    X01CambiDs
419300980930     C*
419400980930     C                   ENDSR
419500980930     c/EJECT
419600980930     C************************************************************
419700980930     C* Editazione di un numero
419800980930     C************************************************************
419900980930     C     Edit          Begsr
420000980930      *
420100980930     C                   Select
420200980930     C                   When      WNrDec = 0
420300980930     C                   Z-add     WImporto      $SrcVar
420400980930     C                   When      WNrDec = 1
420500980930     C     WImporto      Mult      10            $SrcVar
420600980930     C                   When      WNrDec = 2
420700980930     C     WImporto      Mult      100           $SrcVar
420800980930     C                   Other
420900980930     C     WImporto      Mult      1000          $SrcVar
421000980930     C                   Endsl
421100980930     C*
421200980930     C                   Eval      $SrcVarDe = WNrDec
421300980930     C*
421400981007     C                   Eval      $EdtCode  = '1'
421500980930     C                   CallB     'X23EDITR'
421600980930     C                   Parm                    $SrcVar
421700980930     C                   Parm                    $SrcVarPr
421800980930     C                   Parm                    $SrcVarDe
421900980930     C                   Parm                    $EdtCode
422000980930     C                   Parm                    $LenFld
422100980930     C                   Parm      *blanks       $RcvrVar
422200980930     C                   Parm                    $Err
422300980930      *
422400980930     C                   ENDSR
422500980930     C*
422600980930     C/EJECT
422700980930     C************************************************************
422800980930     C* DEFINIZIONE DELL'ORDINAMENTO
422900980930     C************************************************************
423000980930     C     DEFORD        BEGSR
423100980930     C*
423200980930     C* definisco l'ordinamento della lista a seconda dei valori
423300980930     C* impostati nel video di parzializzazione
423400980930     C*
423500980930     C                   SELECT
423600980930    >C     ORD199        WHENEQ    'D'
423700980930     C                   Z-ADD     1             $ORD
423800980930    >C     ORD199        WHENeq    'A'
423900980930     C                   Z-ADD     2             $ORD
424000980930     C                   ENDSL
424100980930     C*
424200980930     C                   ENDSR
424300980930     C/EJECT
424400980930     C************************************************************
424500980930     C* REPERIMENTO DATI SOCIETA'
424600980930     C************************************************************
424700980930     C     REPSOC        BEGSR
424800980930     C*
424900980930     C                   CALLB     'XSOC'
425000980930     C                   PARM                    TIPXSC            6
425100980930     C                   PARM                    SOCXSC            3
425200980930     C                   PARM                    CDSXSC            9 0
425300980930     C                   PARM                    MODXSC            3
425400980930     C                   PARM      *BLANKS       RTNXSC            1
425500980930     C                   PARM                    XSOCDS
425600980930     C                   PARM                    KPJBA
425700980930     C*
425800980930     C                   ENDSR
425900980930     C/EJECT
426000980930     C************************************************************
426100980930     C* REPERIMENTO AUTORIZZAZIONI UTENTE
426200980930     C************************************************************
426300980930     C     REPAUT        BEGSR
426400980930     C*
426500980930     C                   CLEAR                   AUTDS
426600980930     C*
426700980930     C                   MOVEL     XSCSOC        XCASOC
426800980930     C                   MOVEL     XSCDUN        XCAUNI
426900980930     C                   Z-ADD     0             XCAGRP
427000980930     C                   MOVEL     KNMUS         XCAPRF
427100980930    >C                   MOVEL     'CONECC'      XCAFNC
427200980930    >C                   MOVEL     *ZERO         XCAVFU
427300980930    >C                   MOVE      'CK'          XCATCK
427400980930    >C                   MOVE      '1'           XCANAU
427500980930     C*
427600980930     C                   CALLB     'XCHKAUT'
427700980930     C                   PARM                    AUTDS
427800980930     C*
427900980930     C* errori/non abilitazione
428000980930     C     XCARTN        IFNE      0
428100980930     C                   EXSR      ENDPGM
428200980930     C                   ELSE
428300980930    >C                   MOVEL     XCADDS        CONECCDS
428400980930     C                   ENDIF
428500980930     C*
428600980930     C                   ENDSR
428700980930     C/EJECT
428800980930     C************************************************************
428900980930     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
429000980930     C************************************************************
429100980930     C     DEFOPZ        BEGSR
429200980930     C*
429300980930     C* Questa routine deve valorizzare la schiera $FL1
429400980930     C* con S o N a seconda della utilizzabilità o meno
429500980930     C* dell'opzione correlata
429600980930     C*
429700980930     C* Inizialmente si considerano tutte le opzioni come valide
429800980930     C     1             DO        $DIMOP        X
429900980930     C                   MOVE      'S'           $FL1(X)
430000980930     C                   ENDDO
430100980930     C*
430200980930     C* Questa routine deve valorizzare la schiera $FC1/$FR1
430300980930     C* con 1 o 0 a seconda della utilizzabilità o meno
430400980930     C* del tasto funzione correlato
430500980930     C*
430600980930     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
430700980930     C* di compilazione
430800980930     C                   RESET                   $FC1
430900980930     C                   RESET                   $FR1
431000980930     C*
431100980930     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
431200980930     C* invalido alcune opzioni
431300980930     C                   SELECT
431400980930     C* Se pgm richiamato in ricerca disattivo :
431500980930    >C     OPZ199        WHENEQ    '01'
431600980930     C* F3=Fine
431700980930     C                   Z-ADD     3             X
431800980930     C                   MOVE      '0'           $FC1(X)
431900980930     C*
432000980930     C* Se pgm richiamato in gestione       disattivo :
432100980930    >C     OPZ199        WHENEQ    '02'
432200980930     C* 1=Selezione
432300980930     C                   Z-ADD     1             X
432400980930     C     01            LOOKUP    $OP1(X)                                21
432500980930     C     *IN21         IFEQ      *ON
432600980930     C                   MOVE      'N'           $FL1(X)
432700980930     C                   ENDIF
432800980930     C*
432900980930     C* Se pgm richiamato in interrogazione disattivo :
433000980930    >C     OPZ199        WHENEQ    '05'
433100980930     C* 1=Selezione
433200980930     C                   Z-ADD     1             X
433300980930     C     01            LOOKUP    $OP1(X)                                21
433400980930     C     *IN21         IFEQ      *ON
433500980930     C                   MOVE      'N'           $FL1(X)
433600980930     C                   ENDIF
433700980930     C*
433800980930     C* In tutti gli altri casi, disattivo subito :
433900980930     C                   OTHER
434000980930     C*
434100980930     C                   ENDSL
434200980930     c*
434300980930     C* Se non cliente disattivo opzione solleciti
434400980930     C* 42=solleciti
434500980930     C                   if        snatura199 <> 'C'
434600980930     C                   Z-ADD     1             X
434700980930     C     42            LOOKUP    $OP1(X)                                21
434800980930     C     *IN21         IFEQ      *ON
434900980930     C                   MOVE      'N'           $FL1(X)
435000980930     C                   ENDIF
435100980930     C                   endif
435200980930     c*
435300980930     C* Se non percipiente disattivo ritenute
435400980930     C* 40=ritenute
435500980930     C                   if        percipi199 <> *on
435600980930     C                   Z-ADD     1             X
435700980930     C     40            LOOKUP    $OP1(X)                                21
435800980930     C     *IN21         IFEQ      *ON
435900980930     C                   MOVE      'N'           $FL1(X)
436000980930     C                   ENDIF
436100980930     C                   endif
436200980930     c*
436300980930     C* Se non fornitore disattivo opzione bonifici
436400051109     C* 31=bonifici
436500980930     C                   if        snatura199 <> 'F'
436600980930     C                   Z-ADD     1             X
436700051109C1881X***  23            LOOKUP    $OP1(X)                                21
436800051109C1881C     31            LOOKUP    $OP1(X)                                21
436900980930     C     *IN21         IFEQ      *ON
437000980930     C                   MOVE      'N'           $FL1(X)
437100980930     C                   ENDIF
437200980930     C                   endif
437300980930     C* Se non fornitore e neanche cliente disattivo opzione mix
437400980930     C* 32=mix
437500980930     C                   if        snatura199 <> 'F' and snatura199 <> 'C'
437600980930     C                   Z-ADD     1             X
437700980930     C     32            LOOKUP    $OP1(X)                                21
437800980930     C     *IN21         IFEQ      *ON
437900980930     C                   MOVE      'N'           $FL1(X)
438000980930     C                   ENDIF
438100980930     C                   endif
438200981002     c*
438300981002     C* Se non cliente/fornitore disattivo  portafoglio
438400981002     C* 43=portafoglio
438500981002     C                   if        snatura199 <> 'C' and snatura199 <> 'F'
438600981002     C                   Z-ADD     1             X
438700981002     C     43            LOOKUP    $OP1(X)                                21
438800981002     C     *IN21         IFEQ      *ON
438900981002     C                   MOVE      'N'           $FL1(X)
439000981002     C                   ENDIF
439100031120D1498X* e opzione 60=Ordini
439200031120 "   X*                  Z-ADD     1             X
439300031120 "   X*    60            LOOKUP    $OP1(X)                                21
439400031120 "   X*    *IN21         IFEQ      *ON
439500031120 "   X*                  MOVE      'N'           $FL1(X)
439600031120D1498X*                  ENDIF
439700981002     C                   endif
439800031120     c*
439900031120D1498C* Se non cliente/fornitore e neanche conto di costo o ricavo disa
440000031120  "  C* ttivo 60=ordini
440100031120  "  C                   if        snatura199 <> 'C' and snatura199 <> 'F'
440200031120  "  c                             and natura199 <>'3'and natura199 <>'4'
440300031120  "  C                   Z-ADD     1             X
440400031120  "  C     60            LOOKUP    $OP1(X)                                21
440500031120  "  C     *IN21         IFEQ      *ON
440600031120  "  C                   MOVE      'N'           $FL1(X)
440700031120  "  C                   ENDIF
440800031120D1498C                   endif
440900981002     c*
441000981002     C* Se conto non possiede analitica disattivo opzione
441100981002     c* e anche se da autorizzazioni ho e/c limitato per partite
441200981002     C* 21=analitica
441300981002     C                   if        analit199  =  '0' or
441400981002     C                             (Eccpar = '1' )
441500981002     C                   Z-ADD     1             X
441600981002     C     21            LOOKUP    $OP1(X)                                21
441700981002     C     *IN21         IFEQ      *ON
441800981002     C                   MOVE      'N'           $FL1(X)
441900981002     C                   ENDIF
442000981002     C                   endif
442100051109     C*
442200051109C1881C* Se non conto banca, disattivo opzione 23=tesoreria
442300051109  "  C                   if        snatura199 <> '2'
442400051109  "  C                   Z-ADD     1             X
442500051109  "  C     23            LOOKUP    $OP1(X)                                21
442600051109  "  C     *IN21         IFEQ      *ON
442700051109  "  C                   MOVE      'N'           $FL1(X)
442800051109  "  C                   ENDIF
442900051109C1881C                   endif
443000981002     C*
443100031120D1498X* se non presente nostro gestionale, disattivo opzione ordini
443200031120 "   X* Per ora è presente solo la visualizzazione ordini'distribuzione e
443300031120 "   X* tile                                                            e
443400031120 "   X*                  IF        xscGest <>'1' and xscgest <> '2'
443500031120 "   X*                  Z-ADD     1             X
443600031120 "   X*    60            LOOKUP    $OP1(X)                                21
443700031120 "   X*    *IN21         IFEQ      *ON
443800031120 "   X*                  MOVE      'N'           $FL1(X)
443900031120 "   X*                  ENDIF
444000031120D1498X*                  ENDIF
444100030206D1270C*
444200030206  "  C* Se Laguna non è presente o non è attivo per la società
444300030206  "  C                   IF        XscLaguna <> '1'
444400030206  "  C* oppure la sottonatura del conto non è né cliente né fornitore
444500030206  "  C                             or (SNatura199 <> 'C' and
444600030206  "  C                                 SNatura199 <> 'F')
444700030206  "  C* disattivo l'opzione 80
444800030206  "  C                   Z-ADD     1             X
444900030206  "  C     80            LOOKUP    $OP1(X)                                21
445000030206  "  C     *IN21         IFEQ      *ON
445100030206  "  C                   MOVE      'N'           $FL1(X)
445200030206  "  C                   ENDIF
445300030206D1270C                   ENDIF
445400980930     C*
445500980930     C                   ENDSR
445600980930     C/EJECT
445700980930     C************************************************************
445800980930     C* REPERIMENTO OPZIONI
445900980930     C************************************************************
446000980930     C     REPOPZ        BEGSR
446100980930     C*
446200980930     C     1             DO        $DIMOP        X
446300980930     C*
446400980930     C     $MS1(X)       IFNE      *BLANK
446500980930     C*
446600980930     C                   MOVEL     $MS1(X)       IDMSG
446700980930     C                   CALLB     'XRTVM'
446800980930     C                   PARM                    IDMSG            27
446900980930     C                   PARM                    TXTMSG          100
447000980930     C                   PARM                    ERRMSG            1
447100980930     C*
447200980930     C     ERRMSG        IFNE      *ON
447300980930     C                   MOVEL     TXTMSG        $TX1(X)
447400980930     C                   ELSE
447500980930     C                   MOVEL     *ALL'?'       $TX1(X)
447600980930     C                   ENDIF
447700980930     C*
447800980930     C                   ENDIF
447900980930     C*
448000980930     C                   ENDDO
448100980930     C*
448200980930     C                   ENDSR
448300980930     C/EJECT
448400980930     C************************************************************
448500980930     C* REPERIMENTO TASTI DI FUNZIONE
448600980930     C************************************************************
448700980930     C     REPKEY        BEGSR
448800980930     C                   MOVEA     $FM1          $FM
448900980930     C                   MOVEA     $Fc1          $Fc
449000980930     C                   EXSR      REPKKK
449100980930     C                   MOVEA     $FX           $FX1
449200980930     C*
449300980930     C                   ENDSR
449400980930     C/EJECT
449500980930     C************************************************************
449600980930     C* REPERIMENTO TASTI DI FUNZIONE
449700980930     C************************************************************
449800980930     C     REPKKK        BEGSR
449900980930     C*
450000980930     C                   CLEAR                   $FX
450100980930     C     1             DO        24            X
450200980930     C*
450300980930     C     $FM(X)        IFNE      *BLANK
450400980930     C     $Fc(X)        andne     '0'
450500980930     C*
450600980930     C                   MOVEL     $FM(X)        IDMSG
450700980930     C                   CALLB     'XRTVM'
450800980930     C                   PARM                    IDMSG            27
450900980930     C                   PARM                    TXTMSG          100
451000980930     C                   PARM                    ERRMSG            1
451100980930     C*
451200980930     C     ERRMSG        IFNE      *ON
451300980930     C                   MOVEL     TXTMSG        $FX(X)
451400980930     C                   ELSE
451500980930     C                   MOVEL     *ALL' '       $FX(X)
451600980930     C                   ENDIF
451700980930     C*
451800980930     C                   ENDIF
451900980930     C*
452000980930     C                   ENDDO
452100980930     C*
452200980930     C                   ENDSR
452300980930     C/EJECT
452400980930     C************************************************************
452500980930     C* DEFINIZIONE KLIST
452600980930     C************************************************************
452700980930     C     DefKlist      BEGSR
452800980930     C*
452900980930     C* klist
453000980930     C*
453100980930    >C* ndppa01L
453200980930    >C     K04PPA01      KLIST
453300980930    >C                   KFLD                    PPASOCIETA
453400980930    >C                   KFLD                    PPActb
453500980930    >C                   KFLD                    PPAKCC
453600980930    >C                   KFLD                    PPAKSC
453700980930     C*
453800980930    >C     K05PPA01      KLIST
453900980930    >C                   KFLD                    PPASOCIETA
454000980930    >C                   KFLD                    PPActb
454100980930    >C                   KFLD                    PPAKCC
454200980930    >C                   KFLD                    PPAKSC
454300980930    >C                   KFLD                    PPADTPAR
454400980930    >C* x file correlati
454500980930    >C     K02PPA01      KLIST
454600980930    >C                   KFLD                    PPASYS
454700980930    >C                   KFLD                    PPANRASINT
454800980930    >C* ndpas02j
454900980930    >C     K07PAS02      KLIST
455000980930    >C                   KFLD                    PASSOCIETA
455100980930    >C                   KFLD                    PASctb
455200980930    >C                   KFLD                    PASKCC
455300980930    >C                   KFLD                    PASKSC
455400980930    >C                   KFLD                    PASDTPAR
455500980930    >C                   KFLD                    PASNRPAR
455600980930    >C                   KFLD                    PASSERPAR
455700031119D1498X* NdReg01l  EX C1460
455800031119  " >X*    K02Reg01      KLIST
455900031119  " >X*                  KFLD                    RegSys
456000031119D1498X*                  KFLD                    RegNrasReg
456100980930     C*
456200980930     C                   ENDSR
456300980930     C/EJECT
456400051109     C*C1881: aggiunta opzione 23=tesoreria e modificata opzione 23=
456500051109     c*       bonifici in 31=bonifici
456600980930**
456700980930PROMSG    *LIBL     OPZ0001
456800980930PROMSG    *LIBL     OPZ0014
456900980930PROMSG    *LIBL     OPZ0139
457000051110PROMSG    *LIBL     OPZ0107
457100051110PROMSG    *LIBL     OPZ0108
457200051110PROMSG    *LIBL     OPZ0109                                   c1881
457300051109PROMSG    *LIBL     OPZ0560                                   c1881
457400980930PROMSG    *LIBL     OPZ0111
457500980930PROMSG    *LIBL     OPZ0140
457600051109PROMSG    *LIBL     OPZ0041
457700051220PROMSG    *LIBL     OPZ0142
457800981002PROMSG    *LIBL     OPZ0143
457900981002PROMSG    *LIBL     OPZ0144
458000981002PROMSG    *LIBL     OPZ0145
458100010119PROMSG    *LIBL     OPZ0407
458200030206PROMSG    *LIBL     OPZ0451             ### D1270 ###
458300980930**
45840098093001S      <Selezione
45850098093014S      <Gestione commenti
45860098093020S      <Particolarità
45870005111021S      <Analitica
45880005111022S      <Partite
45890005111023S      <Tesoreria                                               C1881
45900005110931S      <Bonifici                                                c1881
45910098093032S      <Mix
45920098093040S      <Ritenute
45930005122041S      <Contabilità
45940098093042S      <Solleciti
45950098100243S      <Portafoglio
45960098100244S      <Descriz. registrazione
45970098100245S      <Descriz. movimento
45980001011960S      <Ordini      C1460
45990003020680S      <Laguna                        ### D1270 ###
460000980930** TASTI DI FUNZIONE  S1
460100980930PROMSG    *LIBL     KEY0001  01
460200080128PROMSG    *LIBL     KEY0331  02             C2074
460300980930PROMSG    *LIBL     KEY0003  03
460400980930PROMSG    *LIBL     KEY0004  04
460500980930PROMSG    *LIBL     KEY0005  05
460600980930PROMSG    *LIBL     KEY0006  06
460700980930PROMSG    *LIBL     KEY0007  07
460800980930PROMSG    *LIBL     KEY0008  08
460900980930PROMSG    *LIBL     KEY0009  09
461000980930PROMSG    *LIBL     KEY0010  10
461100980930PROMSG    *LIBL     KEY0031  11
461200980930PROMSG    *LIBL     KEY0012  12
461300980930PROMSG    *LIBL     KEY0061  13
461400980930PROMSG    *LIBL     KEY0014  14
461500980930PROMSG    *LIBL     KEY0053  15
461600080128PROMSG    *LIBL     KEY0332  16                c2074
461700980930PROMSG    *LIBL     KEY0017  17
461800980930PROMSG    *LIBL     KEY0018  18
461900980930PROMSG    *LIBL     KEY0019  19
462000980930PROMSG    *LIBL     KEY0116  20
462100020527PROMSG    *LIBL     KEY0246  21
462200980930PROMSG    *LIBL     KEY0022  22
462300980930PROMSG    *LIBL     KEY0023  23
462400980930PROMSG    *LIBL     KEY0024  24
462500980930**  ABILITAZIONE S1
4626009809300N  01
4627009810091N  02
4628009809301N  03
4629009809300N  04
4630009809300N  05
4631009809300N  06
4632009809300N  07
4633009810020N  08
4634009809301N  09
4635009809300N  10
4636009809301N  11
4637009809301N  12
4638009810020N  13
4639009809300N  14
4640009809301N  15
4641009809301N  16
4642009809300N  17
4643009809300N  18
4644009809300N  19
4645009809301N  20
4646000205271N  21
4647009809300N  22
4648009809301N  23   Impostare a '1' se si utilizzano opzioni
4649009809301N  24   Lasciare sempre impostata a '1' il valore della riga 24
465000051216** $tit
465100980930PROMSG    *LIBL     COS0141
465200980930PROMSG    *LIBL     COS0142
465300980930PROMSG    *LIBL     COS0143
465400051216** $Cost
465500051216PROMSG    *LIBL     COS2601                                  Dt.flusso
465600051216PROMSG    *LIBL     COS2602                                  Dt.scaden.
