000100160913>>>>>H DFTACTGRP(*NO) ACTGRP('QILE')
000200160913>>>>>H BNDDIR('PJXBND':'PJCBND')
000300160913>>>>>H CVTOPT(*DATETIME)
000400940211     H DECEDIT('0,') DATEDIT(*DMY.)
000500941115      *
000600951108    > *            Estratto conto contabile - SITUAZIONE PARTITE
000700951108      *           ----------------------------------------------
000800940224      *
000900940128     F*----------------------------------------------------*
001000951113     F* Questo pgm può essere richiamato in 2 modi:
001100951115     F* 02 = Gestione
001200951113     F* 01 = Ricerca
001300960328     F*
001400951115     F* Indicatori che condizionano campi a video:
001500070607C2028F* 08 Impegni
001600100420c2156F* 10 Mette il conto in rosso se ci sono attiv. rec.credito aperte
001700951115     F* 11 Unita
001800951115     F* 12 Percipiente
001900951123     F* 13 Agente e solleciti (solo clienti)
002000951123     F* 14 Bonifici           (solo fornitori)
002100960315     F* 15 sottolinea saldo solo se uguale all'importo originario
002200960328     F* 16 sottolinea la data partita se ci sono commenti
002300960328     F* 17 fa vedere anche il saldo in divisa se chiesto includi una divisa
002400960328     F* 18 fa vedere il saldo in alta intensità se ci sono movimenti provvis
002500040308     F* 19 esistenza commenti
002600100521c2156F* 20 Mette la costante'impegni'rossa lampeggiante se ci sono impe
002700100521C2156F*    gni scaduti
002800040308     F*----------------------------------------------------------------
002900040308     F* Viste logiche utilizzate per i diversi ordinamenti
003000040308     F*----------------------------------------------------------------
003100040308     F*------------
003200040308     FNDPPA01L  IF   E           K DISK
003300040308     F                                     RENAME(NDPPA000:NDPPA01)
003400040308     FNDPPA03L  IF   E           K DISK
003500040308     F                                     RENAME(NDPPA000:NDPPA03)
003600040308     FNDPPS01L  IF   E           K DISK
003700040308     F                                     RENAME(NDPPS000:NDPPS01)
003800040308     FNDEPA01L  IF   E           K DISK
003900040308     F                                     RENAME(NDEPA000:NDEPA01)
004000040308     FNDMRA04L  IF   E           K DISK
004100040308     F                                     RENAME(NDMRA000:NDMRA04)
004200040308     FNDPSC01L  IF   E           K DISK
004300040308     F                                     RENAME(NDPSC000:NDPSC01)
004400031118D1498x***NDREG01L  IF   E           K DISK    ex c1460
004401110414C2185FNDCPA01L  IF   E           K DISK
004500940211     F*------------
004600040308     FNDC086D   CF   E             WORKSTN
004700951108     F                                     SFILE(C086S1:S1NRR)
004800940201     F                                     INFDS(DSFMT)
004900940128     D*----------------------------------------------------*
005000940302     D* Schiere per la gestione delle opzioni
005100940302     D*
005200950614     D* dimensione della schiera $MS1
005300020118C1213x******* $DIMOP          C                   CONST(10)
005400990803C1213D $DIMOP          C                   CONST(30)
005500950524     D $MS1            S             27    DIM($dimop) CTDATA PERRCD(1)
005600950524     D $TX1            S             50    DIM($dimop) ALT($MS1)
005700950524     D $OP1            S              2  0 DIM($dimop) CTDATA PERRCD(1)
005800950524     D $FL1            S              1    DIM($dimop) ALT($OP1)
005900941214     D $TX             S             50    DIM(50)
006000941214     D $FL             S              1    DIM(50)
006100940909     D* Schiere generali per la gestione dei tasti di funzione
006200940506     D $FM             S             27    DIM(24)
006300940506     D $FX             S             50    DIM(24)
006400940506     D $FC             S              1    DIM(24)
006500940506     D $FR             S              1    DIM(24)
006600940909     D* Schiere per la gestione dei tasti di funzione
006700951108     D* Video S1
006800940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006900940506     D $FX1            S             50    DIM(24) ALT($FM1)
007000940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
007100940506     D $FR1            S              1    DIM(24) ALT($FC1)
007200960109     d* titolo stampa e/c
007300960109     D $TIT            S             27    DIM(3) CTDATA PERRCD(1)              Titolo
007400011213D1016D* schiera blocchi rate
007500011213D1016D skratebl        S              2    DIM(15)                              Titolo
007600940211     D/EJECT
007700950524     D*-------------
007800950704     D* Parametri del PGM di test esistenza commenti ANA724R
007900950704     D A724DS        E DS                  EXTNAME(ANA724DS) INZ
008000950704     D  EXI724       E                     INZ('0')
008100950704     D  RIS724       E                     INZ('0')
008200010305a0871x**CMT724       E                     INZ('0')
008300981007     D*-------------
008400981007     D* Dati anagrafici del conto
008500981007     D ANA397DS      E DS                  inz
008600020527     D*-------------
008700020527c1567D* stampa griglia
008800020527  "  D NDCG01DS      E DS                  INZ
008900020527c1567D NDCG12DS      E DS                  INZ
009000031119     D*-------------
009100031120D1498D* Parametri interrogazione ordini
009200031119D1498D ANA396DS      E DS                  INZ
009300951109     D*-------------
009400951109     D* Interrogazione conti
009500951206     D ANA399DS      E DS                  inz
009600950704     D*-------------
009700950524     D* Parametri del PGM di gestione commenti ANA725R
009800950707     D ANA725DS      E DS                  INZ
009900950301     D  RET725       E                     INZ('0')
010000950301     D  OPR725       E                     INZ('0')
010100950301     D  ERR725       E                     INZ('0')
010200960109     D*-------------
010300960109     D* Stampa e/C clienti
010400040308     D NDC141DS      E DS                  inz
010500960111     D*-------------
010600960111     D* Stampa e/C fornitori
010700040308     D NDC142DS      E DS                  inz
010800960111     D*-------------
010900960111     D* Stampa e/C altri conti
011000040308     D NDC143DS      E DS                  inz
011100950524     D*-------------
011200990803C1213D* Gestione contenzioso clienti
011300990804  "  D NDCL70DS      E DS                  inz
011400990803  "  D*-------------
011500990803  "  D* autorizzazioni per contenzioso
011600990803  "  D ConPPADS      E DS                  inz
011700990803C1213D*-------------
011800010911C1511 *-------------
011900010911  "  D SavAutDS        s                   like(AutDS) INZ
012000010911  "   * Autorizzazione alla conversione partita
012100010911  "  D EurCnvDS      E DS                  INZ
012200010911  "   *-------------
012300010911  "  D* Paramentri per conversione divisa di partita in m.c.
012400010806  "  D NDC506DS      E DS                  INZ
012500010806  "  D  OPR506       E                     INZ('0')
012600010806  "  D  ERR506       E                     INZ('0')
012700010907C1511D*-------------
012800940211     D* Passaggio Parametri
012900940211     D KPJBA         E DS
013000940211     D*-------------
013100940225     D* Reperimento Autorizzazioni
013200941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
013300941124     D*-------------
013400950410     D* Passaggio Autorizzazioni
013500950410     D DSAUT           DS           512
013600950524     D*-------------
013700950524     D* Maschera dati autorizzazioni
013800040308     D CONECCDS      E DS
013900941209     D*-------------
014000941209     D* Dati di ambiente ottenuti da XSOC
014100040308     D SOC001        E DS                  EXTNAME(XSOC001DS)
014200950906     D    FgO1                        1    overlay(XscFgO)
014300961009     D    XscDare                     1    overlay(xscFgo:7)                    Scadenz. retroattivo
014400961009     D    XscAvere                    1    overlay(xscFgo:8)                    Scadenz. retroattivo
014500010119C1460D    XscGest                     1    overlay(xscSkp:3)
014600020118C1530D    XscLaguna                   1    overlay(xscSkp:8)                    Scadenz. retroattivo
014700960709     D*-------------
014800960709     D* Dati di ambiente ricevuti dal chiamante
014900040308     D SOC00x          S                   like(SOC001)
015000960709     D*-------------
015100960709     D* DS Interna per dati di output di XSOC
015200960709     D XSOCDS          DS          1000
015300951113     D*-------------
015400951113     D* Controlli validità legende
015500951113     D X07DS         E DS                  EXTNAME(X07VALDS) INZ
015600951109     D*-------------
015700951109     D* Parametri per PGM X01CAMBI
015800951109     D X01CambiDs    E DS                  inz
015900951109     D*-------------
016000951109     D* Parametri per reperimento saldo
016100951109     D X20SCADDS     E DS                  INZ
016200951109     D* DS parametro esteso se richiesto x20parscd='S'
016300951206     D X20SCADXDS    E DS                  inz
016400951206     D*-------------
016500951109     D* Routine editazione di un numero
016600951110     D $SrcVar         s             30S 0
016700951110     D $SrcVarPr       s              9B 0 inz(30)
016800951109     D $SrcVarDe       s              9B 0
016900951117     D $EdtCode        s              1    inz('1')
017000951109     D $LenFld         s              3S 0
017100951109     D $RcvrVar        s             50
017200951109     D $Err            s              1
017300940325     D*-------------
017400940325     D* Parametri in ricezione
017500040308     D NDC086DS      E DS
017600960318     D*schiera partite in ricerca
017700960318     D SkPartite       DS                  occurs(50)
017800960318     D  SkDtPar                            like(dtpar086)
017900960318     D  SkNrPar                            like(nrpar086)
018000960318     D  SkSerPar                           like(Serpar086)
018100960318     D  SkImportD                    19S 3
018200960319     D  SkDtSca                            like(dtpar086)
018300990115     D  SkSegnoD                      1
018400070202C1973D  SkImporto                    19S 3
018500070202C1973D  SkDivisa                           like(Divisa086)
018600961121     D*-------------
018700961121     D* Chiamata ricorsiva
018800040308     D X48CHPGMDS    E DS                  INZ
018900940325     D*-------------
019000951108     D* Parametri del PGM di dettaglio partita NDC087R
019100040308     D NDC087DS      E DS                  inz
019200040308     D  RET087       E                     INZ('0')
019300040308     D  OPR087       E                     INZ('0')
019400040308     D  ERR087       E                     INZ('0')
019500040308     D*-------------
019600040308     D* Parametri del PGM di manutenzione NDC060R particolarità partite
019700040308     D NDC060DS      E DS                  inz
019800040308     D  RET060       E                     INZ('0')
019900040308     D  OPR060       E                     INZ('0')
020000040308     D  ERR060       E                     INZ('0')
020100040308     D  DTCHIU060    E                     INZ('0001-01-01')
020200040308     D*-------------
020300040308     D* Parametri del PGM di manutenzione NDC063R scadenze/rate
020400040308     D NDC063DS      E DS                  inz
020500040308     D  RET063       E                     INZ('0')
020600040308     D  OPR063       E                     INZ('0')
020700040308     D  ERR063       E                     INZ('0')
020800950707     D*-------------
020900040308     D* Parametri del PGM di manutenzione NDC065R mix
021000040308     D NDC065DS      E DS                  inz
021100040308     D  RET065       E                     INZ('0')
021200040308     D  OPR065       E                     INZ('0')
021300040308     D  ERR065       E                     INZ('0')
021400040308     D*-------------
021500040308     D* Parametri del PGM di dettaglio/totale partita NDC199R
021600040308     D NDC199DS      E DS                  inz
021700040308     D  RET199       E                     INZ('0')
021800040308     D  OPR199       E                     INZ('0')
021900040308     D  ERR199       E                     INZ('0')
022000040308     D*-------------
022100040308     D* Parametri del PGM dei solleciti NDCL29R
022200040308     D NDCL29DS      E DS                  inz
022300040308     D*-------------
022400040308     D* Parametri del PGM di ritenute NDCR05R
022500040308     D NDCR05DS      E DS                  inz
022600040308     D  RETR05       E                     INZ('0')
022700040308     D  OPRR05       E                     INZ('0')
022800040308     D  ERRR05       E                     INZ('0')
022900040308     D*-------------
023000040308     D* Parametri del PGM di ritenute NDCH65R1
023100040308     D*NDCH65DS      E DS                  inz
023200040308     D**RETH65       E                     INZ('0')
023300040308     D**OPRH65       E                     INZ('0')
023400040308     D**ERRH65       E                     INZ('0')
023500040308     D*-------------
023600040308     D* Parametri del PGM di bonifici NDCB50R
023700040308     D NDCB50DS      E DS                  inz
023800040308     D  RETB50       E                     INZ('0')
023900040308     D  OPRB50       E                     INZ('0')
024000040308     D  ERRB50       E                     INZ('0')
024100040308     D  DTPARB50     E                     INZ('0001-01-01')
024200070621c2034D  DTDIST1B50   E                     INZ('0001-01-01')
024300070621c2034D  DTDIST2B50   E                     INZ('0001-01-01')
024400040308     D*-------------
024500971027     D* Parametri videata riepilogo per divisa
024600971029     D Ndc293Ds      E DS                  Inz
024700971029     D  OPR293       E                     INZ('0')
024800971029     D  RET293       E                     INZ('0')
024900971029     D  ERR293       E                     INZ('0')
025000971027     D*-------------
025100971027     D* Ds multipla riepilogo per divisa
025200971029     D Ndc291Ds1     E DS                  Inz Occurs(50)
025300971027     D*-------------
025400971027     D* Parametri programma riepilogo per divisa
025500971029     D Ndc291Ds      E DS                  Inz
025600971029     D  Esito291     E                     INZ('0')
025700960122     D*-------------
025800960122     D* Controllo conti
025900960122     D ND002DS       E DS                  inz
026000070607c2028D*-------------
026100070607  "  D* Parametri del PGM di reperimento informazioni recupero credito
026200070607c2028D NDCL99DS      E DS                  inz
026300090318c2125D  DTRIFL99     E                     INZ('0001-01-01')
026400090216c2119D*-------------
026500090216  "  D* Parametri del PGM di gestione giroconto a contenzioso
026600090216c2119D NDCL28DS      E DS                  inz
026700020118C1530D*-------------
026800020118  "  D* dati catalogazione Laguna
026900020118C1530D ANLGN00F      E DS
027000960122     D*-------------
027100960122     D* Definizioni parametri standard chiamate moduli
027200960122     D Esito           S              1
027300960122     D gesmsg          S              1
027400960122     D lenout          S              9B 0
027500960122     D Data            S             10
027600960122     D strutturaO      S             10
027700960122     D Operazione      S              2
027800960122     D LenIn           S              9B 0
027900960122     D refresh         S              1
028000960122     D uso             S              1
028100960122     DOutput           S           4000
028200940211     D*-------------
028300940211     D DSFMT           DS           512
028400940506     D  $TASTO               369    369
028500940211     D  NRG                  370    370
028600940211     D  NCL                  371    371
028700940211     D  SFLNRR               378    379B 0
028800940211     D*-------------
028900940211     D* posizione cursore
029000940211     D CURSOR          DS
029100940211     D  RIRI                   1      2B 0 INZ
029200940211     D  R$$                    2      2
029300940211     D  COCO                   3      4B 0 INZ
029400940211     D  C$$                    4      4
029500940127     D*-------------
029600940127     D* Reperimento nome PGM
029700940127     D STATUS         SDS           333
029800940127     D  DSPGM            *PROC
029900950411     D  PARMS            *PARMS
030000950704     D*-------------
030100950704     D* Attributi video
030200950704     D* (definisco come costanti qugli attributi video che potrei
030300950704     D*  testare e quindi preferisco assegnargli un valore "parlante")
030400950704     D*  bottone presente
030500060303     D PRESENTE        C                   CONST(X'A1')
030600060303D1948D PRESERED        C                   CONST(X'A9')
030700060303D1948D PRESEPNK        C                   CONST(X'B9')
030800940616     D*-------------
030900940506     D* Tasti di funzione
031000940506     D F01             C                   CONST(X'31')
031100940506     D F02             C                   CONST(X'32')
031200940506     D F03             C                   CONST(X'33')
031300940506     D F04             C                   CONST(X'34')
031400940506     D F05             C                   CONST(X'35')
031500940506     D F06             C                   CONST(X'36')
031600940506     D F07             C                   CONST(X'37')
031700940506     D F08             C                   CONST(X'38')
031800940506     D F09             C                   CONST(X'39')
031900940506     D F10             C                   CONST(X'3A')
032000940506     D F11             C                   CONST(X'3B')
032100940506     D F12             C                   CONST(X'3C')
032200940506     D F13             C                   CONST(X'B1')
032300940506     D F14             C                   CONST(X'B2')
032400940506     D F15             C                   CONST(X'B3')
032500940506     D F16             C                   CONST(X'B4')
032600940506     D F17             C                   CONST(X'B5')
032700940506     D F18             C                   CONST(X'B6')
032800940506     D F19             C                   CONST(X'B7')
032900940506     D F20             C                   CONST(X'B8')
033000940506     D F21             C                   CONST(X'B9')
033100940506     D F22             C                   CONST(X'BA')
033200940506     D F23             C                   CONST(X'BB')
033300940506     D F24             C                   CONST(X'BC')
033400940506     D ENTER           C                   CONST(X'F1')
033500940506     D ROLDWN          C                   CONST(X'F4')
033600940506     D ROLLUP          C                   CONST(X'F5')
033700951109     D*-------------
033800951109     D DATMIN          C                   CONST('0001-01-01')
033900951109     D DATMAX          C                   CONST('9999-12-31')
034000950524     D*-------------
034100950606     D* Variabili per gestione videate
034200950606     D $Gest           S              2A
034300950606     D $Fine           S              1A
034400950606     D $InzD1          S              1A
034500950606     D $InzS1          S              1A
034600950606     D $LastG          S              2A
034700950606     D $LastV          S              2A
034800950606     D $EndFile        S              1A
034900950606     D $Esci           S              1A
035000950606     D $RcdOK          S              1A
035100951108     D $carpag         S              1A
035200951108     D $ord            S              1S 0
035300951109     D sflpag          S              4S 0
035400950606     D*-------------
035500950606     D* Variabili appoggio sempre presenti in tutte le anagrafiche
035600950606     D S1Nrr           S                   like(C1Rcd)
035700951109     D SFLNRRSAV       S                   like(SFLNRR)
035800950606     D WSfl            S                   like(C1Nrr)
035900950606     D WMax            S                   like(C1Rcd)
036000950606     D WPag            S                   like(C1Rcd)
036100950606     D WInzS1          S                   like($InzS1)
036200040308     D WOpr            S                   like(Opr086)
036300040308     D CurR            S              2S 0
036400040308     D CurC            S              2S 0
036500040308     D Win             S             99A
036600040308     D W1Pos           S                   like(C1Pos)
036700040308     D Wdate           S             10
036800040308     D WDivisa         S                   like(X01Divisa)
036900040308     D SavDivisa       S                   like(X01Divisa)
037000040308     D SavDivVis       S                   like(divvis086)
037100040308     D Wl29iniz        S                   like(l29iniz)
037200040308     D WImporto        S                   like(saldo086)
037300040308     D NrDec           S              3S 0
037400040308     D NrDecd          S              3S 0
037500040308     D WNrDec          S              3S 0
037600040308     D Wsegno          S              1A
037700040308     D WSaldo          S                   like(Saldo086)
037800040308     D wopzs1          S              1
037900960319     D wopzs2          S              1
038000960307     d* Totali dare avere in moneta di conto del cliente
038100040308     D WSaldoTotD      S                   like(Saldo086)
038200040308     D WSaldoTotA      S                   like(Saldo086)
038300040308     d* Totali dare avere in divisa del cliente
038400040308     D WSaldoToDD      S                   like(Saldo086)
038500040308     D WSaldoToAD      S                   like(Saldo086)
038600040308     d* Totali dare avere in moneta di conto di partita
038700040308     D WTotDare        S                   like(saldo086)
038800040308     D WTotAvere       S                   like(Saldo086)
038900040308     d* Totali dare avere in divisa di partita
039000040308     D WTotDareD       S                   like(saldo086)
039100040308     D WTotAvereD      S                   like(Saldo086)
039200020118C1530D*-------------
039300020118  "   * Parametri driver Laguna
039400020118  "  D Opzione         S              2A
039500020118  "  D Errore          S              1A
039600020118  "  D Vista           S              2S 0
039700020118  "  D Dati            S           4000A
039800020118  "  D Lunghezza       S              9B 0
039900020118  "  D Commit          S              1A
040000020118  "  D NrChiavi        S              2S 0
040100020118  "  D Alloca          S              1A
040200020118  "  D DataISO         S               D
040300020118C1530D*
040400950606     D*-------------
040500950606     D*  Indici
040600950606     D X               S              3S 0
040700950606     D Y               S              3S 0
040800020118C1530D*-------------
040900020118  "  D* Chiamata a Laguna
041000021120C1530D Cmd670          S             15
041100021120C1605x*Parm670         S             30    dim(30)
041200071023C2055x*Parm670         S             44    dim(30)  ex C1605
041300071023C2055D Parm670         S             44    dim(70)
041400021120C1530D Err670          S              1
041500020118C1530D Ret670          S             32A
041600970401     D*-------------
041700970401     D* Richiamo a XATB
041800970401     D XATBDS        E DS
041900970401     D*-------------
042000970401     D* Tabella blocco amministrativo
042100970401     D TAB0AB        E DS                  EXTNAME(ANG0ABDS)
042200981130      *-------------
042300981130      * Variabile di appoggio
042400981130     D $DataISO        S               D
042500981130     D $Data60         S              6  0
042600000118C1309D ZDateISO        S               D
042700010907C1511D*-------------
042800010828  "  D* DS esterna gestione messaggi
042900010828  "  D XMSGDS        E DS
043000010828  "  D  XSTS         E                     EXTFLD(MSGSTS)
043100010828  "  D                                     DIM(333)                                           bili
043200010828  "  D  XJBA         E                     EXTFLD(MSGJBA)
043300010828  "  D                                     DIM(502)                                           bili
043400010828  "  D*-------------
043500010912  "  D* campi per emissione wdw di richiesta per continuare o meno
043600010912  "  D* le operazioni di conversione 46
043700010828  "  D OpenWdw46       S              1A   Inz(*Off)
043800010911  "  D*-------------
043900010911  "  D* indicatore se devo elaborare o meno la conversione partita
044000010911  "  D $Elabora46      S              1A   Inz(*Off)
044100010828  "  D*-------------
044200010828  "  D* campi di lavoro per emissione window di avvertimento
044300010828  "  D WXMSGInz        S              1A   INZ('1')
044400010910C1511D EmissWdw        S              1A   INZ('0')
044500011127D1016D WrkBkAmm        S                   like(pscbkamm)
044600031120d1498D WrkNatura       S                   like(natura002)
044700030619D1378 *-------------
044800030619  "   * segnala se siamo sotto web facing
044900030620  "   * web facing non riesce a convertire i campi messi in RI
045000030620  "   * mediante l'uso di una variabile nella parola kiave DSPATR
045100030620  "   * quindi se siamo sotto web facing metto oltre al segnalino in RI
045200030620  "   * metto un *
045300030620D1378D WebFacingOn     s              1s 0 inz
045301110414C2185D* Parametri del PGM di manutenzione NDC066R1 est. nominativi
045302110414  " >D NDC066DS1     E DS                  inz
045303110414  " >D  RET0661      E                     INZ('0')
045304110414  " >D  OPR0661      E                     INZ('0')
045305110414C2185D  ERR0661      E                     INZ('0')
045306110414     D*-------------
045307160913>>>>>D yEsito          S             10I 0
045400940127     C*----------------------------------------------------*
045500940127     C*                MAIN LINE PROGRAM
045600940127     C*----------------------------------------------------*
045700940127     C*
045800940223     C* inizializzazione variabili
045900940223     C                   EXSR      INZVAR
046000940223     C*
046100940223     C     $FINE         DOWEQ     *OFF
046200940131     C     $GEST         CASEQ     'S1'          GESS1
046300941027     C                   ENDCS
046400941027     C                   ENDDO
046500940325     C* fine programma
046600940325     C                   EXSR      ENDPGM
046700940225     C*
046800940325     C/EJECT
046900940325     C************************************************************
047000940325     C* FINE PROGRAMMA
047100940325     C************************************************************
047200940325     C     ENDPGM        BEGSR
047300951122     c* Per far riemettere la testata
047400951122     C                   MOVE      *HIVAL        $LASTV
047500970624     c                   move      wopr          opr086
047600940325     C*
047700940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
047800040308     C     OPZ086        IFEQ      *BLANK
047900940325     C                   SETON                                            LR
048000940325     C* altrimenti chiudo in RT
048100940325     C                   ELSE
048200940325     C                   SETON                                            RT
048300940325     C                   ENDIF
048400940616     C                   RETURN
048500940325     C*
048600940325     C                   ENDSR
048700940224     C/EJECT
048800940201     C************************************************************
048900940201     C* RICERCHE
049000940201     C************************************************************
049100940127     C     SEARCH        BEGSR
049200940127     C*
049300940127     C* determino Riga/Colonna del cursore
049400940127     C                   MOVE      NRG           R$$
049500940127     C                   MOVE      NCL           C$$
049600940127     C                   Z-ADD     RIRI          CURR
049700940127     C                   Z-ADD     COCO          CURC
049800950704     C*
049900040308     C                   SELECT
050000040308     C*
050100040308     C*--------------------------------------------------------------
050200040308     C* inserire le routine di ricerca
050300040308     C*--------------------------------------------------------------
050400040308     C* FMTD1 -
050500040308     C     1             WHENEQ    1
050600040308     C*
050700040308     C* ricerca non abilitata in questa posizione
050800040308     C                   OTHER
050900040308     C                   SETON                                        9899
051000040308     C*
051100040308     C                   ENDSL
051200950612     C*
051300950612     C* imposto pos. del cursore
051400950612     C                   Z-ADD     CURR          H1RIGA
051500950612     C                   Z-ADD     CURC          H1COLO
051600940127     C*
051700940506     C                   MOVE      *HIVAL        $LASTV
051800950612     C*
051900940127     C                   ENDSR
052000940131     C/EJECT
052100940224     C************************************************************
052200940131     C* RIEMPIMENTO OPZIONI
052300940224     C************************************************************
052400940131     C     RIEOPZ        BEGSR
052500940223     C*
052600940223     C* Input :
052700940223     C* - $TX = schiera con tutti i testi disponibili
052800940223     C* - $FL = schiere con i flag di validità associati ai testi
052900940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
053000940223     C*          opzione usato
053100940223     C* - WMXO = variabile contenente il numero massimo di testi
053200940223     C*          opzione usabili
053300940721     C* - $L01 = Lunghezza prima riga
053400940721     C* - $L02 = Lunghezza seconda riga
053500940223     C* Output :
053600940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
053700940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
053800941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
053900940223     C*
054000950522     C                   CALLB     'X04OPMSG'
054100940223     C* input
054200940223     C                   PARM                    $TX
054300940223     C                   PARM                    $FL
054400940225     C                   PARM                    WCOZ              3 0
054500940223     C                   PARM                    WMXO              3 0
054600940223     C* output
054700940223     C                   PARM                    WOPZ1            76
054800940223     C                   PARM                    WOPZ2            76
054900941214     C                   PARM                    FF23              1
055000950301     C* input
055100040308     C                   PARM                    $L01              3 0
055200040308     C                   PARM                    $L02              3 0
055300940223     C*
055400060224C1913C                   PARM                    $ALLOPT         960
055500060224C1913C*
055600940131     C                   ENDSR
055700940506     C/EJECT
055800940506     C************************************************************
055900940506     C* RIEMPIMENTO TASTI FUNZIONE
056000940506     C************************************************************
056100940506     C     RIEKEY        BEGSR
056200940506     C*
056300940506     C* Input :
056400940506     C* - $FX = schiera con tutti i testi disponibili
056500940506     C* - $FC = schiere con i flag di validità associati ai testi
056600940506     C* - $FR = schiere con i flag di REVERSE IMAGE
056700940506     C* - $ULK = ultimo testo caricato
056800940721     C* - $L01 = Lunghezza prima riga
056900940721     C* - $L02 = Lunghezza seconda riga
057000940506     C* Output :
057100940506     C* - $ULK
057200940609     C* - $KEY1= 1° riga di tasti funzione
057300940609     C* - $KEY2= 2° riga di tasti funzione
057400940506     C*
057500950522     C                   CALLB     'XKEYMSG'
057600940506     C* input
057700940506     C                   PARM                    $FX
057800940506     C                   PARM                    $FC
057900940506     C                   PARM                    $FR
058000940506     C                   PARM                    $ULK              3 0
058100940506     C* output
058200940506     C                   PARM                    $KEY1            79
058300940506     C                   PARM                    $KEY2            79
058400950301     C* input
058500940721    >C                   PARM                    $L01              3 0
058600940721    >C                   PARM                    $L02              3 0
058700940506     C*
058800060224C1913C                   PARM                    $ALLFUNCT       480
058900060224C1913C*
059000940506     C                   ENDSR
059100940224     C/EJECT
059200940224     C************************************************************
059300940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
059400940224     C************************************************************
059500940224     C     GESPIE        BEGSR
059600940224     C*
059700940224     C                   SELECT
059800940506     C* Lista
059900940506     C     $GEST         WHENEQ    'S1'
060000940506     C     $ULKS1        IFEQ      0
060100951110     C*
060200951110     C* Call routine editazione x importo in moneta di conto
060300951110     C* da fare solo una volta poichè si fa quando il caricamento pagina
060400951113     C* non è presente ( se c'era il caricamento a pagina, avevo già reperit
060500951113     C* o questi dati nella routine INZVAR)
060600951110     C                   IF        $carpag = *off
060700960307     C* totale moneta di conto
060800951113     C                   Z-ADD     WSaldoTotd    WtotDare
060900951113     C                   Z-ADD     WSaldoTotA    WTotAvere
061000951110     C                   exsr      RepSegno
061100951110     C* importo
061200951110     C                   Z-add     Wsaldo        WImporto
061300951110     C                   Z-add     NrDec         WNrDec
061400951110     C                   Eval      $LenFld = %Size(Z1Saldo)
061500951110     C* allineo dare a sinistra
061600961009     C                   if        Wsegno = xscdare
061700951110     C                   SUB       5             $lenfld
061800951110     C                   endif
061900951110     C                   EXSR      Edit
062000951110     C                   Eval      Z1Saldo = $RcvrVar
062100951110     C                   eval      z1segno = Wsegno
062200960307     c*
062300960307     C* totale in divisa
062400960307     C                   SETOFF                                       17
062500960307     C                   if        divisa086 <> *blanks and divsel086 = 'I'
062600960307     c                             and divisa086 <> xscdiv
062700960307     C                   seton                                        17
062800960307     c                   eval      wdivisa = divisa086
062900960307     c                   exsr      repdivisa
063000960307     c                   eval      nrdecD = x01nrdec
063100960307     C                   Z-ADD     WSaldoToDD    WtotDareD
063200960307     C                   Z-ADD     WSaldoToAD    WTotAvereD
063300960307     C                   exsr      RepSegnoD
063400960307     C* importo
063500960307     C                   Z-add     Wsaldo        WImporto
063600960307     C                   Z-add     NrDecD        WNrDec
063700960307     C                   Eval      $LenFld = %Size(Z1Saldo)
063800960307     C* allineo dare a sinistra
063900961009     C                   if        Wsegno = xscDare
064000960307     C                   SUB       5             $lenfld
064100960307     C                   endif
064200960307     C                   EXSR      Edit
064300960307     C                   Eval      Z1SaldoDiv = $RcvrVar
064400970421     C                   eval      z1segnoDiv = Wsegno
064500960307     c                   eval      z1divisa = DIVISA086
064600951110     C                   endif
064700960307     C                   endif
064800951110     c*
064900940506     C                   EXSR      F24S1
065000940506     C                   ELSE
065100951108     C                   WRITE     C086Z1
065200940506     C                   ENDIF
065300940506     C                   ENDSL
065400940506     C*
065500940224     C                   ENDSR
065600951110     C/EJECT
065700951110     C************************************************************
065800951113     C* Repsegno Reperisce segno moneta di conto
065900951110     C************************************************************
066000951110     C     RepSegno      BEGSR
066100951110     C*
066200951110     C                   Select
066300951113     C                   when      Wtotdare   > WtotAvere
066400961009     C                   eval      WSegno   = xscdare
066500951113     C                   eval      WSaldo   = Wtotdare   - Wtotavere
066600951110     C*
066700951113     C                   when      Wtotdare   < Wtotavere
066800961009     C                   eval      WSegno   = xscavere
066900951113     C                   eval      WSaldo   = Wtotavere  - Wtotdare
067000951110     C*
067100951113     C                   when      Wtotdare   = Wtotavere
067200951110     C                   eval      WSegno   = ' '
067300951110     C                   eval      WSaldo   = 0
067400951110     C                   endsl
067500951113     C*
067600951113     C                   ENDSR
067700951110     C*
067800951113     C/EJECT
067900951113     C************************************************************
068000951113     C* Repsegno Reperisce segno divisa
068100951113     C************************************************************
068200951113     C     RepSegnod     BEGSR
068300951113     C*
068400951113     C                   Select
068500951113     C                   when      WtotdareD  > WtotAvereD
068600961009     C                   eval      WSegno   = xscdare
068700951113     C                   eval      WSaldo   = WtotdareD  - WtotavereD
068800951113     C*
068900951113     C                   when      WtotdareD  < WtotavereD
069000961009     C                   eval      WSegno   = xscavere
069100951113     C                   eval      WSaldo   = WtotavereD - WtotdareD
069200951113     C*
069300951113     C                   when      WtotdareD  = WtotavereD
069400951113     C                   eval      WSegno   = ' '
069500951113     C                   eval      WSaldo   = 0
069600951113     C                   endsl
069700951113     C*
069800951110     C                   ENDSR
069900940224     C/EJECT
070000940127     C************************************************************
070100940131     C* GESTIONE LISTA
070200940127     C************************************************************
070300940127     C     GESS1         BEGSR
070400940223     C* inizializzazione videata
070500940223     C     $INZS1        IFEQ      *ON
070600940127     C                   EXSR      INZS1
070700940223     C                   MOVE      *OFF          $INZS1
070800940127     C                   ENDIF
070900940223     C* emissione piede videata se proveniente da altra
071000940223     C* salvataggio valore formato di provenienza
071100940202     C     $LASTV        IFNE      'S1'
071200951109     C                   WRITE     C086T1
071300940224     C                   EXSR      GESPIE
071400940202     C                   MOVE      $LASTV        $LASTG
071500940202     C                   MOVE      'S1'          $LASTV
071600941027     C                   ENDIF
071700940223     C*
071800940223     C     WMAX          IFEQ      0
071900951108     C                   WRITE     C086D1
072000940211     C                   ENDIF
072100951109     C                   Eval      *in09=c1mode
072200090216c2119c* apertura window
072300090216c2119C                   Eval      wrkwdwl28=*on
072400940127     C*
072500951108     C                   EXFMT     C086C1
072600940204     C     C1NRR         IFNE      0
072700940204     C                   Z-ADD     C1NRR         WSFL
072800940204     C                   ENDIF
072900951109     C*
073000951109     C     SFLNRR        IFNE      0
073100951109     C                   Z-ADD     SFLNRR        C1RCD
073200951109     C                   Z-ADD     SFLNRR        SFLNRRSAV
073300951109     C                   ELSE
073400951109     C                   Z-ADD     SFLNRRSAV     C1RCD
073500951109     C                   ENDIF
073600951124     C* faccio eseguire al pgm di sola finestra la close e la open per far
073700951124     c* riemettere il nuovo sfondo del subfile
073800951124     C                   eval      wl29iniz   = '0'
073900940506     C*
074000940506     C* controllo abilitazione tasti funzione
074100940506     C                   EXSR      CTRKS1
074200941205     C*
074300940506     C     *IN99         IFEQ      *OFF
074400940127     C*
074500941027     C                   SELECT
074600981007     C* F2=Dati conto
074700981007     C     $TASTO        WHENEQ    F02
074800981007     C                   EXSR      F02S1
074900940127     C* F3=Fine
075000940506     C     $TASTO        WHENEQ    F03
075100940309     C                   EXSR      F03S1
075200981001     C* F7=Richiamo pgm visualizzazione movimenti e partite
075300981001     C     $TASTO        WHENEQ    F07
075400981001     C                   EXSR      F07S1
075500951120     C* F8=Selezione seguente
075600951120     C     $TASTO        WHENEQ    F08
075700951120     C                   EXSR      F08S1
075800951108     C* F11=Cambio moneta
075900951108     C     $TASTO        WHENEQ    F11
076000951108     C                   EXSR      F11S1
076100940131     C* F12=Ritorno
076200940506     C     $TASTO        WHENEQ    F12
076300940309     C                   EXSR      F12S1
076400960314     C* F13=selezione multipla
076500960314     C     $TASTO        WHENEQ    F13
076600960314     C                   EXSR      F13S1
076700990803C1215 *
076800990803  "   * F14=Commenti (sul conto)
076900990803  "  C     $TASTO        WHENEQ    F14
077000990803  "  C                   EXSR      F14S1
077100990803C1215 *
077200951108     C* F15=Interrogazioni
077300951108     C     $TASTO        WHENEQ    F15
077400951108     C                   EXSR      F15S1
077500971027     C* F16=Riepilogo per divisa
077600971027     C     $TASTO        WHENEQ    F16
077700971027     C                   EXSR      F16S1
077800960109     C* F20=Stampa e/c
077900960109     C     $TASTO        WHENEQ    F20
078000960109     C                   EXSR      F20S1
078100020527c1567C* F21=Stampa griglia
078200020527  "  C     $TASTO        WHENEQ    F21
078300020527c1567C                   EXSR      F21S1
078400940224     C* F23=Altre opzioni
078500940506     C     $TASTO        WHENEQ    F23
078600940309     C                   EXSR      F23S1
078700940224     C* F24=Altri tasti
078800940506     C     $TASTO        WHENEQ    F24
078900940309     C                   EXSR      F24S1
079000951108     C* ROLL-UP (se gestito a pagina)
079100940506     C     $TASTO        WHENEQ    ROLLUP
079200951108     C     $CARPAG       ANDEQ     *ON
079300940127     C                   EXSR      ROLS1
079400940127     C*
079500941027     C                   OTHER
079600940127     C* CONTROLLO DATI
079700940131     C                   EXSR      CTRC1
079800940201     C     *IN99         IFEQ      *OFF
079900960319     c* controllo che non vengano scelte opzioni miste di selezione se
080000960319     C* scelta opzione multipla
080100960319     c                   if        multip086 = *on
080200960319     C                   EXSR      CTRopz
080300960319     C                   endif
080400960319     C     *IN99         IFEQ      *OFF
080500940131     C                   EXSR      CTRS1
080600941027     C                   ENDIF
080700960319     C                   ENDIF
080800941205     C*
080900941027     C                   ENDSL
081000940127     C*
081100940506     C                   ENDIF
081200941205     C*
081300940127     C                   ENDSR
081400960319     C/EJECT
081500990803C1215C************************************************************
081600990803  "  C* GESTIONE F14 VIDEO S1
081700990803  "  C************************************************************
081800990803  "  C     F14S1         BEGSR
081900990803  "  C*
082000990803  "  C* richiamo il pgm di gestione
082100990803  "  C                   RESET                   ANA725DS
082200990803  "  C                   MOVE      '02'          OPZ725
082300990803  "  C*
082400990803  "  C* inserire chiavi commenti
082500990803  "  C                   SELECT
082600990803  "  C     Snatura086    wheneq    'C'
082700990803  "  C                   MOVEL     '700'         TPA725
082800990803  "  C                   MOVEL     C1Ksc         KY1725
082900990803  "  C                   MOVEL     '000'         KY3725
083000990803  "  C     SNATURA086    wheneq    'F'
083100990803  "  C                   MOVEL     '001'         TPA725
083200990803  "  C                   MOVEL     C1Ksc         KY1725
083300990803  "  C                   OTHER
083400990803  "  C                   MOVEL     'PDC'         TPA725
083500990803  "  C                   MOVEL     C1Kcc         KY1725
083600990803  "  C                   MOVEL     C1Ksc         KY2725
083700990803  "  C                   endsl
083800990803  "  C*
083900990803  "  C                   MOVE      *BLANK        KPJBU
084000990803  "  C                   MOVEL     ANA725DS      KPJBU
084100990803  "  C*
084200990803  "  C                   CALL      'ANA725R'
084300990803  "  C                   PARM                    KPJBA
084400990803  "  C                   MOVEL     KPJBU         ANA725DS
084500990803  "  C*
084600990803  "  C* ritorno da PGM commenti
084700990803  "  C                   EXSR      RTN725
084800990803  "  C*
084900990803  "  C                   ENDSR
085000990803C1215C/EJECT
085100960319     C************************************************************
085200960319     C* CONTROLLI PER opzioni miste se scelta selezione multipla
085300960319     C************************************************************
085400960319     C     CTROPZ        BEGSR
085500960319     C*
085600960319     C                   SETOFF                                       99
085700960319     c                   clear                   wopzs1
085800960319     c                   clear                   wopzs2
085900960319     C*
086000040308     C     WMAX          IFGT      0
086100040308     C                   READC     C086S1                                 21
086200040308     C     *IN21         DOWEQ     *OFF
086300040308     C*
086400040308     C     S1OPZ         IFEQ      1
086500040308     C                   eval      wopzs1 = *on
086600040308     C                   ENDIF
086700040308     C     S1OPZ         IFNE      1
086800040308     C     S1OPZ         andne     0
086900040308     C                   eval      wopzs2 = *on
087000040308     C                   ENDIF
087100040308     c*
087200040308     C                   MOVE      *ON           *IN32
087300040308     C* imposto quegli attributi del sfl che sono modificabili
087400040308     C* al richiamo del determinato pgm
087500040308     C                   EXSR      ATRS1
087600040308     C                   UPDATE    C086S1
087700960319     C                   MOVE      *Off          *IN32
087800040308     c*
087900040308     C                   READC     C086S1                                 21
088000040308     C*
088100040308     C                   ENDDO
088200040308     C*
088300040308     C* se rilevate opzioni miste , attivo sflnxtchg
088400040308     C                   if        wopzs1 = *on and
088500040308     C                             wopzs2 = *on
088600040308     C                   seton                                        9993
088700040308     C                   endif
088800040308     C*
088900040308     C                   ENDIF
089000040308     C*
089100040308     C                   ENDSR
089200040308     C/EJECT
089300040308     C************************************************************
089400040308     C* INIZIALIZZAZIONE LISTA
089500940127     C************************************************************
089600940127     C     INZS1         BEGSR
089700940302     C* pulizia SFL
089800940128     C                   SETOFF                                         3031
089900951108     C                   WRITE     C086C1
090000940128     C                   SETON                                          31
090100940127     C*
090200940128     C* riempimento opzioni
090300940223     C                   MOVEA     $TX1          $TX
090400940223     C                   MOVEA     $FL1          $FL
090500940309     C                   Z-ADD     0             WCOZ
090600940225     C*  equivale al num.di elementi della schiera $TX1
090700940509     C                   Z-ADD     $DIMOP        WMXO
090800941220    >C                   Z-ADD     76            $L01
090900941220    >C                   Z-ADD     76            $L02
091000940128     C                   EXSR      RIEOPZ
091100941214     C                   Z-ADD     23            X
091200040308     C                   MOVE      FF23          $FC1(X)
091300040308     C                   MOVEL     WOPZ1         C1OPZ1
091400060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
091500040308     C                   MOVEL     WOPZ2         C1OPZ2
091600040308     C                   Z-ADD     WCOZ          C1COZ
091700040308     C* imposta control
091800040308     C* reperisco capoconto se cliente/fornitore
091900040308     C     kcc086        IFEQ      'C       '
092000040308     C     kcc086        OREQ      'F       '
092100040308     C                   CALLB     'XCAPCLIFOR'
092200040308     C                   PARM                    Xscsoc
092300040308     C                   PARM                    kcc086
092400040308     C                   PARM                    ksc086
092500040308     C                   ENDIF
092600040308     C                   EVAL      C1KCC = kcc086
092700040308     C                   EVAL      C1KsC = ksc086
092800040308     c* Reperisco tutti i dati del conto
092900040308     c                   exsr      callmvc002
093000970401     c* decodifico blocco amministrativo
093100970401     c                   clear                   c1bkammd
093200981007     c                   if        Bkamm002 <> *blanks and esito =*off
093300970401     C                   MOVE      '1'           XTBRIC
093400970401     C                   MOVE      XSCSOC        XTBAZI
093500970401     C                   MOVE      *ZERO         XTBKEY
093600970401     C                   MOVE      '0AB'         XTBCOD
093700970401     C                   MOVE      BkAmm002      XTBKEY
093800970401     C                   CALLb     'XATB'
093900970401     C                   PARM                    XATBDS
094000970401     C     XTBERR        IFEQ      *Off
094100970401     C                   MOVEL     XTBUNI        TAB0AB
094200970401     C                   MOVEL     DE20AB        C1BkAmmD
094300970401     c                   endif
094400970401     c                   endif
094500951108     C                   EVAL      C1Descr = DESCR086
094600951108     C                   EVAL      C1Desagg = DESAGG086
094700951108     C* Attivo o no scritta percipiente
094800951108     C                   IF        percipi086 = *on
094900951108     c                   eval      *in12 = *on
095000951108     C                   else
095100951108     c                   eval      *in12 = *off
095200951108     C                   endif
095300981006     C* Controlla esistenza commenti
095400981006     c                   exsr      TstTcm
095500981006     C* Attivo o no scritta commenti
095600981006     C                   IF        exi724     = *on
095700981006     c                   eval      *in19 = *on
095800981006     C                   else
095900981006     c                   eval      *in19 = *off
096000981006     C                   endif
096100100521c2028C* Controlla esistenza impegni
096200070607  "  c                   exsr      TstRecCred
096300070607  "  C* Attivo o no scritta impegni
096400100521c2028c                   IF        ImpegniL99 = *on
096500100521c2156c                             or ImpegniL99='2'
096600100521c2028c                   eval      *in08 = *on
096700100521c2156c* se impegni scaduti attivo indicatore 20
096800100521  "  c                   if        ImpegniL99='2'
096900100521  "  c                   eval      *in20 = *on
097000100521  "  c                   else
097100100521  "  c                   eval      *in20 = *off
097200100521C2156c                   endif
097300100521c2028C                   else
097400070607  "  c                   eval      *in08 = *off
097500070607c2028C                   endif
097600100421c2156C* Attivo o no indicatore 10 per mettere in rosso il conto
097700100420  "  c* se ci sono delle attività
097800100420  "  C                   IF        AttivitL99 = *on
097900100420  "  c                   eval      *in10 = *on
098000100420  "  C                   else
098100100420  "  c                   eval      *in10 = *off
098200100420c2156C                   endif
098300940128     C*
098400951108     C* CARICAMENTO SFL A PAGINE o intero
098500940128     C*
098600940201     C                   Z-ADD     0             S1NRR
098700940128     C                   Z-ADD     0             WMAX
098800940224     C*
098900940224     C* Posizionamento su file pilota
099000940302     C                   CLEAR                   C1POS
099100941214     C                   EXSR      SETANA
099200940302     C* ripristino i valori del posizionamento
099300940302     C                   MOVE      W1POS         C1POS
099400940131     C*
099500940201     C* Lettura file pilota
099600941214     C                   EXSR      REDANA
099700940131     C*
099800951108     C* Carico 1a pagina o tutto
099900050608     C                   Z-ADD     SFLPAG        WPAG
100000940127     C                   EXSR      ROLS1
100100050608D1830C                   Z-ADD     SFLPAG        WPAG
100200940127     C*
100300940127     C                   ENDSR
100400960122     C/EJECT
100500960122     C*----------------------------------------------------------
100600960122     C* Chiamata NDMVC002
100700960122     C*----------------------------------------------------------
100800960122     C     CallMvc002    BEGSR
100900960122     C*
101000960122     C* Controllo conto
101100960122     C                   Clear                   ND002DS
101200960122     C                   EVAL      LenOut = %Size(ND002DS)
101300960122     C                   eval      Data = Datmin
101400960122     C                   CallB     'NDMVC002'
101500960122     C                   PARM                    xscsoc
101600960122     C                   PARM                    kcc086
101700960122     C                   PARM                    ksc086
101800960122     C                   PARM                    Data
101900960122     C                   PARM      *OFF          GesMsg
102000960122     C                   PARM      *OFF          Esito
102100960122     C                   PARM      'ND002DS'     StrutturaO
102200960122     C                   PARM                    ND002DS
102300960122     C                   PARM                    LenOut
102400960122     C*
102500960122     c                   if         esito = *off
102600960122     c                   eval       descr086   = descr002
102700960122     c                   eval       desagg086  = desagg002
102800960122     c                   eval       percipi086 = percipi002
102900960122     c                   eval       snatura086 = snatura002
103000031120D1498c                   eval       wrknatura  =  natura002
103100960122     c                   eval       analit086  = analit002
103200960122     c                   endif
103300960122     C*
103400960122     C                   ENDSR
103500940127     C/EJECT
103600940127     C************************************************************
103700940131     C* CARICAMENTO PAGINA LISTA
103800940127     C************************************************************
103900940127     C     ROLS1         BEGSR
104000940127     C*
104100940128     C                   SETOFF                                       32
104200940223     C                   Z-ADD     0             Y
104300940127     C                   Z-ADD     WMAX          S1NRR
104400940127     C*
104500940127     C* Leggo dal file anagrafico per caricare la lista
104600940127     C*
104700950606     C     $EndFile      DOWEQ     *OFF
104800940201     C     Y             ANDLT     WPAG
104900940127     C*
105000940131     C                   EXSR      RIES1
105100940127     C*
105200940127     C                   ADD       1             S1NRR
105300940127     C                   ADD       1             Y
105400940127     C*
105500951108     C                   WRITE     C086S1
105600940131     C*
105700941214     C                   EXSR      REDANA
105800940128     C*
105900941027     C                   ENDDO
106000940127     C*
106100940223     C                   Z-ADD     S1NRR         WMAX                 30
106200940127     C*
106300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
106400940127     C*
106500940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
106600011204D1023X***                MVR                     RESTO             3 0
106700011204D1023C                   MVR                     RESTO             4 0
106800940131     C     PAGINE        MULT      SFLPAG        C1RCD
106900941027     C     RESTO         IFGT      0
107000940127     C                   ADD       1             C1RCD
107100941027     C                   ELSE
107200940407     C                   SUB       SFLPAG        C1RCD
107300940407     C                   ADD       1             C1RCD
107400941027     C                   ENDIF
107500940128     C*
107600940127     C                   ENDSR
107700940224     C/EJECT
107800940224     C************************************************************
107900940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
108000940224     C************************************************************
108100941214     C     SETANA        BEGSR
108200940224     C*
108300950704     C                   MOVE      XSCSOC        PPASOCIETA
108400951108     C                   MOVE      CTB086        PPActb
108500951108     C                   MOVE      KCC086        PPAKCC
108600951108     C                   MOVE      KSC086        PPAKSC
108700950704     C*
108800940224     C                   SELECT
108900040308     C*
109000040308     C* Gestione posizionamento
109100040308     C* Se il campo W1POS è > *blank imposto il posizionamento con
109200040308     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
109300040308     C*
109400040308     C* Ord.1 = chiave ordinamento 1 (discendente)
109500040308     C     $ORD          WHENEQ    1
109600040308     C*
109700040308     C     W1POS         IFNE      0
109800040308     C                   CALLb     'XDT4000'
109900040308     C                   PARM                    XDTAMG           10
110000040308     C                   PARM      w1pos         XDTGMA            6 0
110100040308     C                   PARM      4             XDTSTA            1 0
110200040308     C                   MOVE      xdtamg        ppadtpar
110300040308     C                   ELSE
110400040308     C     Dtpera086     IFEQ      datmin
110500040308     C                   MOVE      DATMAX        ppadtpar
110600040308     C                   ELSE
110700040308     C                   MOVE      DTPERa086     ppadtpar
110800040308     C                   ENDIF
110900040308     C                   ENDIF
111000040308     C*
111100040308     C* Se chieste partite solo aperte e se la data limite registrazione
111200040308     C* movimenti definitivi è > o uguale a oggi uso la vista logica che
111300040308     c* omette le partite chiuse
111400040308     C                   if        sitpar086 <> '1' and dtregd086 >= wdate
111500040308     C     K05PPA01      SETGT     NDPPA03
111600040308     C                   else
111700040308     C     K05PPA01      SETGT     NDPPA01
111800040308     C                   endif
111900040308     c*
112000040308     C* Ord.2 = chiave ordinamento 2 (ascendente)
112100040308     C     $ORD          WHENEQ    2
112200040308     C     W1POS         IFNE      0
112300040308     C                   CALLb     'XDT4000'
112400040308     C                   PARM                    XDTAMG
112500040308     C                   PARM      w1pos         XDTGMA
112600040308     C                   PARM      4             XDTSTA
112700040308     C                   MOVE      xdtamg        ppadtpar
112800040308     C                   ELSE
112900040308     C     Dtperda086    IFEQ      datmin
113000040308     C                   MOVE      DATMIN        ppadtpar
113100040308     C                   ELSE
113200040308     C                   MOVE      DTPERDA086    ppadtpar
113300040308     C                   ENDIF
113400040308     C                   ENDIF
113500040308     C* Se chieste partite solo aperte e se data limite registrazioni
113600040308     C* definitive è >= oggi, uso la vista logica che omette partite chiuse
113700040308     C                   if        sitpar086 <> '1' and dtregd086 >= wdate
113800040308     C     K05PPA01      SETLL     NDPPA03
113900040308     C                   else
114000040308     C     K05PPA01      SETLL     NDPPA01
114100040308     C                   endif
114200040308     c*
114300040308     C                   ENDSL
114400040308     C*
114500040308     C                   ENDSR
114600040308     C/EJECT
114700040308     C************************************************************
114800040308     C* LETTURA RCD ARCHIVIO PILOTA
114900040308     C************************************************************
115000040308     C     REDANA        BEGSR
115100040308     C*
115200040308     C                   MOVEL     *OFF          $EndFile
115300040308     C                   MOVEL     *OFF          $RCDOK
115400040308     C*
115500040308     C     $EndFile      DOUEQ     *ON
115600040308     C     $RCDOK        OREQ      *ON
115700040308     C*
115800040308     C                   SELECT
115900040308     C* Discendente
116000040308     C     $ORD          WHENEQ    1
116100040308     C* Solo aperte
116200040308     C                   if        sitpar086 <> '1' and dtregd086 >= wdate
116300040308     C     K04PPA01      READPE    NDPPA03                                21
116400040308     C                   else
116500040308     C     K04PPA01      READPE    NDPPA01                                21
116600040308     C                   endif
116700040308     C* Ascendente
116800040308     C     $ORD          WHENEQ    2
116900040308     C* Solo aperte
117000040308     C                   if        sitpar086 <> '1' and dtregd086 >= wdate
117100040308     C     K04PPA01      READE     NDPPA03                                21
117200040308     C                   else
117300040308     C     K04PPA01      READE     NDPPA01                                21
117400951113     C                   endif
117500941027     C                   ENDSL
117600940131     C*
117700950606     C                   MOVE      *IN21         $EndFile
117800940131     C*
117900950606     C     $EndFile      IFEQ      *OFF
118000941214     C                   EXSR      SELANA
118100941027     C                   ENDIF
118200940131     C*
118300941027     C                   ENDDO
118400940131     C*
118500940131     C                   MOVE      *IN21         *IN33
118600940131     C*
118700940131     C                   ENDSR
118800940224     C/EJECT
118900940224     C************************************************************
119000940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
119100940224     C************************************************************
119200940224     C     CLCPAG        BEGSR
119300940224     C* Input :
119400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
119500940224     C*          cursore
119600940224     C* - SFLPAG = numero rcd per pagina sfl
119700940224     C* Output :
119800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
119900940224     C*
120000940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
120100011204D1023X***                MVR                     RESTO             3 0
120200011204D1023C                   MVR                     RESTO
120300940224     C     RESTO         IFGT      0
120400940224     C                   ADD       1             PAGINE
120500940224     C                   ENDIF
120600940224     C     PAGINE        MULT      SFLPAG        WPAG
120700940224     C*
120800940224     C                   ENDSR
120900940131     C/EJECT
121000940131     C************************************************************
121100940131     C* SELEZIONI SULLA RIGA LETTA
121200940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
121300940314     C* non rientra nei parametri di parzializzazione
121400940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
121500940314     C* superiore stabilito nei parametri
121600940314     C*
121700940131     C************************************************************
121800941214     C     SELANA        BEGSR
121900940131     C*
122000940202     C                   MOVEL     *ON           $RCDOK
122100070205C1973X****               MOVEL     *ALL'9'       ALL9             15
122200070205C1973C                   MOVEL     *HIVAL        ALL9             15
122300951109     C*
122400980227     C* Se cliente/fornitore (sottonatura = C o F) leggo file estensione
122500011128D1016c                   clear                   WrkBkAmm
122600011213D1016c                   clear                   skratebl
122700951109     C                   IF        snatura086 = 'C' or snatura086 = 'F'
122800951109     C     K02PPA01      CHAIN     NDPPS01                            23
122900011128D1016C                   if        *in23=*off
123000011128  "  c                   eval      WRkBkamm = PpsBkAmm
123100011128  "  C                   endif
123200011128  "  c* Controllo se esistono delle rate bloccate solo se non esistono
123300070605D1016c* già dei blocchi su ndpps e solo per fornitori
123400070605C2028X***                if        wrkbkamm = *blank and snatura086='F'
123500070605C2028C                   if        wrkbkamm = *blank
123600070605D1016c                   exsr      CtrRateBl
123700011128D1016c                   endif
123800951109     C                   ENDIF
123900950314     C*
124000951110     C                   SELECT
124100940314     C*
124200940314     C* SELEZIONI RISPETTO AI VALORI RCD
124300940314     C*
124400950314     C* Scartare i record che non rispondono alle selezioni effettuate
124500950301     C* esempio : esclusione degli annullati.
124600950906     c*
124700950906     C* Unità diversa
124800951110     C
124900951110     C                   When      Unita086 <> *blanks and
125000951109     C                             PPAUnita <> Unita086
125100950906     C                   move      *off          $RCDok
125200951109     c*
125300951109     C* Periodo
125400951113     C                   When      PPaDtpar <  DtperDa086 or
125500951109     C                             PPaDtpar >  DtperA086
125600951109     C                   move      *off          $RCDok
125700951109     c*
125800951109     C* Divisa selezionata (includi)
125900951109     C                   When      DivSel086 = 'I' and divisa086 <> *blanks
126000951109     C                             and PPaDivisa <>  divisa086
126100951109     C                   move      *off          $RCDok
126200951109     c*
126300951109     C* Divisa selezionata (ometti)
126400951109     C                   When      DivSel086 = 'O' and divisa086 <> *blanks
126500951109     C                             and PPaDivisa =  divisa086
126600951109     C                   move      *off          $RCDok
126700951109     c*
126800960104     C* Richieste parzializzazioni su pps, ma pps non trovato
126900960104     C                   When      (CtgAn1086 <> *blanks or
127000960104     C                             CtgAn2086 <> *blanks or
127100960104     C                             CtgAn3086 <> *blanks or
127200110415     C                             kscage086 <> *blanks )
127500960104     C                              and *in23 =*on
127600960104     C                   move      *off          $RCDok
127700960104     c*
127800951109     C* Categorie di partita
127900951109     C* di pps
128000951109     C                   When      CtgAn1086 <> *blanks and
128100960104     C                             PpsCTgana1 <> CtgAn1086
128200951109     C                   move      *off          $RCDok
128300960104     c*
128400951109     C                   When      CtgAn2086 <> *blanks and
128500960104     C                             PpsCTgana2 <> CtgAn2086
128600951109     C                   move      *off          $RCDok
128700960104     C*
128800951109     C                   When      CtgAn3086 <> *blanks and
128900960104     C                             PpsCTgana3 <> CtgAn3086
129000951109     C                   move      *off          $RCDok
129100951109     c*
129200951109     C* Agente
129300960104     C                   When      KscAge086 <> *blanks and
129400951109     C                             PpsKscAge <> KscAge086
129500951109     C                   move      *off          $RCDok
129600951109     c*
129700951109     C* Blocco
129800960104     C                   When      Bkamm086  <> *blanks
129900960104     c*
130000011213     C* tutte le bloccate (devono esistere su ndpps o su ndpsc)
130100960104     C                   if        Bkamm086  = '* '
130200011213D1016X*************      if        (PpsBkamm  = *blanks and *in23 = *off)
130300011213  "  X*************                or *in23 = *on
130400011128D1016C                   if        wrkbkamm  =  *blanks
130500951109     C                   move      *off          $RCDok
130600960104     C                   endif
130700960104     C                   endif
130800960104     C* solo le non bloccate (tutte le aperte, anche quelle non trovate
130900011213     C* su ndpps o ndpsc)
131000960104     C                   if        Bkamm086  = '**' and
131100011213D1016x**************               ppsBkamm  <> *blanks and *in23 = *off
131200011128D1016c                             wrkbkamm  <> *blanks
131300951109     C                   move      *off          $RCDok
131400960104     C                   endif
131500011213     C* Codice blocco preciso (deve esistere su ndpps o su ndpsc)
131600960104     C                   if        Bkamm086 <> '* ' and
131700960104     C                             bkamm086 <> '**'
131800011213d1016c     bkamm086      lookup    skratebl                               10
131900011213  "  x***************    if        (ppsBkamm  <> bkamm086 and *in23 = *off)
132000011213  "  x***************              or *in23 = *on
132100011213  "  c                   if        wrkbkamm  <>  Bkamm086 and *in10
132200011213d1016c                             = *off
132300951121     C                   move      *off          $RCDok
132400960104     C                   endif
132500960104     C                   endif
132600980227     C*
132700980227     C* Factor
132800980227     C                   When      KccFact086 <> *Blank and
132900980227     C                             KscFact086 <> *Blank
133000980227     C                   If        ((PPSKccFact <> KccFact086 or
133100980227     C                               PPSKscFact <> KscFact086) and
133200980227     C                              *In23 = *Off)
133300980227     C                             or *In23 = *On
133400980227     C                   Move      *Off          $RcdOK
133500980227     C                   EndIf
133600980227     C*
133700951109     C                   ENDSL
134300991126D0329C*
134400991126  "  C* scarto il rcd se
134500991126  "  C*  la partita è chiusa e
134600991126  "  C                   IF        PPAParChiu = *on        and
134700991126  "  C*  la data di chiusura è < o = alla data limite registrazione
134800991126  "  C*   definitiva e
134900991126  "  C                             PPADtChiu <= DtRegD086  and
135000991126  "  C*  non sono state chieste tutte le partite
135100991126  "  C                             SitPar086 <> '1'
135200991126  "  C                   MOVE      *Off          $RcdOK
135300991126D0329C                   ENDIF
135400951113     C*
135500951113     C*  chiamo routine  per altre selezioni
135600951113     C*  se selezioni precedentI  sono ok e se la partita non è chiusa
135700951113     C*  perchè in questo caso non mi servono i dati di x20 perchè sono
135800951113     C*  tutti a 0
135900960201     C                   clear                   WtotDare
136000960201     C                   clear                   WtotAvere
136100960201     C                   clear                   WtotDared
136200960201     C                   clear                   WtotAvereD
136300981020     c* se record ok e partita non chiusa
136400960516     C                   if        $rcdok = *on and (ppaparchiu <> '1'
136500981028     c                             or  ppadtchiu > dtregd086)
136600991129D0329C* in pratica qui una partita chiusa arriva solo se richieste
136700991129  "  C* tutte le partite, in quanto il test che la partita sia aperta o
136800991129  "  C* almeno con data chiusura successiva alla data limite registraz.
136900991129D0329C* definitiva è già stato fatto come elemento di selezione
137000951113     C                   exsr      riex20scad
137100951113     C                   CALL      'X20SCADR'
137200951113     C                   PARM                    X20SCADDS
137300951113     C                   PARM                    X20SCADXDS
137400951113     C* Se tutto ok
137500960409     C                   if        x20errori = ' ' and
137600960409     c                             x20nrmov <> 0
137700951113     C* calcolo totale dare e avere sia in moneta di conto che divisa della
137800951113     C* partita
137900951113     C* mc
138000951113     C                   add       x20tdld       WtotDare
138100951113     C                   add       x20tdlp       WtotDare
138200951113     C                   add       x20tdlg       WtotDare
138300951113     C                   add       x20tald       WtotAvere
138400951113     C                   add       x20talp       WtotAvere
138500951113     C                   add       x20talg       WtotAvere
138600951113     C* divisa
138700951113     C                   add       x20tddd       WtotDareD
138800951113     C                   add       x20tddp       WtotDared
138900951113     C                   add       x20tddg       WtotDareD
139000951113     C                   add       x20tadd       WtotAvereD
139100951113     C                   add       x20tadp       WtotAvereD
139200951113     C                   add       x20tadg       WtotAvereD
139300951113     C* Controlli situazione partita
139400951113     C                   EXSR      CtrSitPar
139500951116     C                   else
139600951116     C                   eval      $rcdok = *off
139700951113     C                   endif
139800951113     C                   endif
139900951109     C*
140000940202     C                   ENDSR
140100011127     C/EJECT
140200011127D1016C************************************************************
140300011127  "  C* controllo se esistono delle rate bloccate
140400011127  "  C************************************************************
140500011127  "  C     CtrRateBl     BEGSR
140600011213  "  C*
140700011213  "  c                   clear                   skratebl
140800011127  "  C*
140900011127  "  C     K02Ppa01      Setll     Ndpsc01l
141000011127  "  c                   do        *hival
141100011128  "  C     K02Ppa01      reade     Ndpsc01l                               24
141200011128  "  c   24              leave
141300011127  "  c                   if        pscbkamm <> *blanks
141400011213  "  c                   eval      wrkbkamm = '+'
141500011213  "  c                   z-add     1             $x
141600011213  "  c     '  '          lookup    skratebl($X)                           10
141700011213  "  c   10              move      pscbkamm      skratebl($X)
141800011127  "  c                   endif
141900011127  "  c                   enddo
142000011127  "  C*
142100011127D1016C                   ENDSR
142200951113     C/EJECT
142300951113     C************************************************************
142400951113     C* controlli situazione partita dopo chiamata a X20SCADR
142500951113     C************************************************************
142600951113     C     CtrSitPar     BEGSR
142700951113     C*
142800951113     C* testo flag situazione partita richiesto sui risultati avuti da
142900951113     C* x20scadr
143000951113     C                   select
143100951113     C* Aperte/ a rischio
143200951113     C                   When      Sitpar086  = '0'
143300960103     C* Sono aperte se dare <> da avere  in moneta di conto def e gestionale
143400960314     C* Sono aperte se dare <> da avere  in divisa          def e gestionale
143500960314     C* se dare provvisorio <> 0 o avere provvisorio <> 0 in moneta di conto
143600960314     C* se dare provvisorio <> 0 o avere provvisorio <> 0 in divisa
143700960103     C* ese uno dei campi scaduto, a scadere, a rischio,
143800960103     C* portafoglio, é <> 0
143900960103     C                   if        x20tdld <> x20tald or
144000960103     C                             x20tdlg <> x20talg or
144100960314     C                             x20tddd <> x20tadd or
144200960314     C                             x20tddg <> x20tadg or
144300960103     C                             x20tdlp <> 0       or
144400960103     C                             x20talp <> 0       or
144500960314     C                             x20tddp <> 0       or
144600960314     C                             x20tadp <> 0       or
144700960102     C                             X20LScadu <> 0 or x20lascad <> 0 or
144800960102     C                             X20Risc <> 0 or X20Portaf <> 0
144900951113     C                   move      *on           $RCDok
145000951113     C                   else
145100951113     C                   move      *off          $RCDok
145200951113     C                   endif
145300951113     c* Aperte dare (testo solo moneta di conto)
145400951113     C* sono aperte in dare se dare <> da avere e dare > avere
145500951113     C                   When      Sitpar086  = '2'
145600951113     C                   if        WtotDare <> WtotAvere and
145700951113     C                             WtotDare >  WtotAvere
145800951113     C                   move      *on           $RCDok
145900951113     C                   else
146000951113     C                   move      *off          $RCDok
146100951113     C                   endif
146200951113     c* Aperte avere (testo solo moneta di conto)
146300951113     C* sono aperte in avere se dare <> da avere e dare < avere
146400951113     C                   When      Sitpar086  = '3'
146500951113     C                   if        WtotDare <> WtotAvere and
146600951113     C                             WtotDare <  WtotAvere
146700951113     C                   move      *on           $RCDok
146800951113     C                   else
146900951113     C                   move      *off          $RCDok
147000951113     C                   endif
147100951113     c* Aperte dare o avere (come rischiesta 0 senza però verifica rischio)
147200951113     C                   When      Sitpar086  = '4'
147300951113     C                   if        WtotDare <> WtotAvere or
147400951113     C                             WtotDareD <> WtotAvered
147500951113     C                   move      *on           $RCDok
147600951113     C                   else
147700951113     C                   move      *off          $RCDok
147800951113     C                   endif
147900951113     C*
148000951113     C                   endsl
148100951206     c*
148200040308D0329X************* spostato nella routine SELANA
148300040308  "  X**Testo se partita effettivamente aperta(se richiesto) nel caso in
148400040308  "  X**cui la data limite registrazioni definitive sia inferiore a udate
148500040308  "  X*****              if        $rcdok = *on  and dtregd086 < wdate and
148600040308  "  X*****                        sitpar086 <> '1'
148700040308  "  X**è*chiusa, quindi non valida, se la data chiusura partita è inferiore
148800040308  "  X**o*uguale alla data limite registrazioni
148900040308  "  X*****              if        ppadtchiu <= dtregd086 and ppaparchiu = *on
149000040308  "  X*****              move      *off          $RCDok
149100040308  "  X*****              endif
149200040308  "  X*****              endif
149300040308D0329X*************
149400951113     c*
149500951113     C                   ENDSR
149600940131     C/EJECT
149700940131     C************************************************************
149800940131     C* RIEMPIMENTO RIGA LISTA
149900940131     C************************************************************
150000940131     C     RIES1         BEGSR
150100940131     C*
150200951109     C                   CLEAR                   C086S1
150300950704     C* azzero le memorizzazioni degli attributi non bottoni
150400960328     C                   MOVEL     *ZERO         provvisorh
150500951109     C                   MOVEL     *ZERO         commentih
150600951109     C                   MOVEL     *ZERO         bonificih
150700951109     C                   MOVEL     *ZERO         sollecitih
150800951109     C                   MOVEL     *ZERO         scadutoh
150900951109     C                   MOVEL     *ZERO         ascadereh
151000951109     C                   MOVEL     *ZERO         rischioh
151100951109     C                   MOVEL     *ZERO         portafoglh
151200951109     C                   MOVEL     *ZERO         insrith
151300951109     C*
151400951109     C* Campi da NDppa
151500010119c1460C                   z-add     PPANRASGen    H1NRASGen
151600951109     C                   MOVE      PPASYS        H1SYS
151700951109     C                   MOVE      PPANRASINT    H1NRASINT
151800951109     C                   CALLb     'XDT4000'
151900951109     C                   PARM      PPADTPAR      XDTAMG
152000951109     C     S1DTPAR       PARM                    XDTGMA
152100951109     C                   PARM      2             XDTSTA
152200951109     C                   MOVE      PPANRPAR      S1NRPAR
152300951109     C                   MOVE      PPASERPAR     S1SERPAR
152400951113     C                   EVAL      S1UNITA = ppaunita
152500951109     c* divise fisse
152600951110     C                   EVAL      S1Divisa   = ppadivisa
152700951109     C                   EVAL      S1Monconto = xscdiv
152800951109     C*
152900951109     C* Campi da NDPPS
153000960104     C                   if        *in23 = *off
153100951109     C                   CALLb     'XDT4000'
153200951109     C                   PARM      PPSDTDoc      XDTAMG
153300951109     C     S1DTDoc       PARM                    XDTGMA
153400951109     C                   PARM      2             XDTSTA
153500951109     C                   EVAL      S1nrDOC = PPSnrDOC
153600951109     C                   EVAL      S1serdoc = PPSserDOC
153700951109     C                   EVAL      S1kscage = ppsKscAGE
153800011128D1016X***                EVAL      S1bkamm  = PPSbkamm
153900011128D1016C                   EVAL      S1bkamm  = WRKbkamm
154000951109     C                   EVAL      S1ctgana1= PPSctgana1
154100951109     C                   EVAL      S1ctgana2= PPSctgana2
154200951109     C                   EVAL      S1ctgana3= PPSctgana3
154300951109     C                   endif
154400981006     c*
154500011213D1016x* se partita non bloccata e solo per fornitori
154600011213  "  x* leggo file SCADENZE/ratei (non serve + perchè è in selana)
154700011213  "  x*                  if        s1bkamm = *blanks
154800011213  "  x*                            and snatura086 = 'F'
154900011213  "  x*    K02PPA01      SETLL     NDPSC01                                22
155000011213  "  x*                  do        *hival
155100011213  "  x*    K02PPA01      reade     NDPSC01                                22
155200011213  "  x*  22              leave
155300011213  "  x*                  if        pscBkamm <> *blanks
155400011213  "  x*                  eval      s1bkamm = '+'
155500011213  "  x*                  leave
155600011213  "  x*                  endif
155700011213  "  x*                  enddo
155800011213D1016x*                  endif
155900951113     C*
156000960409     C* Campi da X20SCADR se lo avevo chiamato
156100951113     C* + calcolo totale saldo se lo deve fare
156200960409     c*
156300981020     c                   IF         ppaparchiu <> '1' or
156400981028     c                             ppadtchiu >  dtregd086
156500960409     c*
156600951109     C                   exsr      calcsaldo
156700951109     C* altri campi moneta di conto
156800951109     C                   Z-ADD     X20lscadu     S1ScadutoM
156900951109     C                   Z-ADD     X20lAscad     S1AscaderM
157000951109     C                   Z-ADD     X20risc       S1RischioM
157100951109     C* altri campi in divisa
157200951109     C                   Z-ADD     X20dscadu     S1ScadutoD
157300951109     C                   Z-ADD     X20dascad     S1AscaderD
157400951109     C                   Z-ADD     X20drisc      S1RischioD
157500990617     C* importo originario (memorizzo se esistono movimenti di 1 solo segno)
157600990617     c* solo dare
157700990617     c                   if        (x20tddd <>0 or x20tddp <>0 or x20tddg <>0)
157800990617     c                                          and(x20tadd = 0 and
157900990617     c                                              x20tadp = 0 and
158000990617     C                                              x20tadg = 0 ) or
158100990617     c                             (x20tadd <>0 or x20tadp <>0 or x20tadg <>0)
158200990617     c                                          and(x20tddd = 0 and
158300990617     c                                              x20tddp = 0 and
158400990617     C                                              x20tddg = 0 )
158500960315     C                   select
158600960315     c                   when      snatura086 = 'C'
158700960315     C                   if        x20ivadare <> 0
158800960315     C                   z-ADD     X20ivadare    s1imporig
158900990617     c                   eval      SegnoOrig = 'D'
159000960315     c                   else
159100960315     C                   z-ADD     X20ivaaver    s1imporig
159200990617     c                   eval      SegnoOrig = 'A'
159300960315     c                   endif
159400960315     c                   when      snatura086 = 'F'
159500960315     C                   if        x20ivaaver <> 0
159600960315     C                   z-ADD     X20ivaaver    s1imporig
159700990617     c                   eval      SegnoOrig = 'A'
159800960315     c                   else
159900960315     C                   z-ADD     X20ivadare    s1imporig
160000990617     c                   eval      SegnoOrig = 'D'
160100960315     c                   endif
160200960315     c                   endsl
160300990617     c                   endif
160400951113     C* reperisco flag vari per presenza di riga partita
160500951110     C* scaduto
160600951110     C                   IF        X20Lscadu <> 0 or
160700951110     C                             X20Dscadu <> 0
160800951110     C                   eval      ScadutoH = *on
160900951109     C                   endif
161000951110     C* a scadere
161100951110     C                   IF        X20Lascad <> 0 or
161200951110     C                             X20dascad <> 0
161300951110     C                   eval      Ascadereh = *on
161400951110     C                   endif
161500951110     C* rischio
161600951110     C                   IF        X20risc   <> 0 or
161700951110     C                             X20Drisc  <> 0
161800951110     C                   eval      RischioH = *on
161900951110     C                   endif
162000951110     C* portafoglio
162100951110     C                   IF        X20portaf <> 0 or
162200951110     C                             X20Dportaf<> 0
162300951110     C                   eval      portafoglh = *on
162400951110     C                   endif
162500951110     C* insoluti/ritenute
162600960507     c* clienti
162700960507     c                   if        snatura086 = 'C'
162800951110     C                   IF        X20Scains <> 0 or
162900951110     C                             X20Dscains <> 0
163000951110     C                   eval      insritH = *on
163100951110     C                   endif
163200960507     c                   endif
163300960507     c* fornitore percipiente
163400960507     c                   if        snatura086 = 'F' and percipi086 = *on
163500960507     C                   eval      mrasocieta  = xscsoc
163600960507     C                   eval      mrakcc  = kcc086
163700960507     C                   eval      mraksc  = ksc086
163800960507     C                   CALLb     'XDT4000'
163900960507     C     mradrp        PARM                    XDTAMG
164000960507     C                   PARM      s1dtpar       XDTGMA
164100960507     C                   PARM      4             XDTSTA
164200960507     C                   eval      mranrp  = s1nrpar
164300960507     C                   eval      mraserpar = s1serpar
164400960507     C     K06mra04      CHAIN     NDmra04L                           21
164500960507     C                   if        *in21 = *off
164600960507     C                   eval      insritH = *on
164700060314D1955C                   MOVEL     PRESERED      zinsrita
164800060314D1955C                   MOVe      *hival        $lastv
164900960507     C                   endif
165000960507     C                   endif
165100960507     c
165200951205     C* bonifici (da flag su file NDPPS)
165300960104     C                   if        *in23 = *off
165400951205     C                   if        ppsboninc = *on
165500951205     C                   eval      bonificiH = *on
165600951205     C                   endif
165700951205     C                   endif
165800960328     c*
165900960328     C* memorizzo se presenti movimenti provvisori
166000960328     C                   IF        X20tdlp   <> 0 or
166100960328     C                             X20talp   <> 0 or
166200960328     C                             X20tddp   <> 0 or
166300960328     C                             X20tadp   <> 0
166400960328     C                   eval      provvisorH = *on
166500960328     C                   endif
166600951205     C
166700951123     C* reperisco se solleciti presenti (deve essere presente nel file e non
166800951205     C* annullato). Questo controllo si fa solo se lo scadutoin M.c. è <> 0!
166900951123     C                   if        scadutoh = *on
167000951123     C                   eval      epacsi  = xscsoc
167100951123     C                   eval      epakcc  = kcc086
167200951123     C                   eval      epaksc  = ksc086
167300951123     C                   CALLb     'XDT4000'
167400951123     C     epadrp        PARM                    XDTAMG
167500951123     C                   PARM      s1dtpar       XDTGMA
167600951123     C                   PARM      4             XDTSTA
167700951123     C                   eval      epanrp  = s1nrpar
167800951123     C                   eval      epaserpar = s1serpar
167900951123     C     K06EPA01      CHAIN     NDEPA01L                           21
168000951123     C                   if        *in21 = *off and epaann = *blanks
168100951123     C                   eval      sollecitiH = *on
168200951123     C                   endif
168300951123     C                   endif
168400951123     c*
168500951113     C* Flag totali da reperire se il caricamento non è a pagina
168600951113     C* ( se a pagina, li ho reperiti nella routine inzvar)
168700951113     C                   IF        $carpag = *off
168800951113     C                   if        scadutoH = *ON
168900060303D1948C***                MOVEL     PRESente      zscadutoa
169000060303D1948C                   MOVEL     PRESERED      zscadutoa
169100030922D1452X** ex D1378 ****** if        WebFacingOn = 1
169200030922D1452X** ex D1378 ****** move      '*'           Z1Scaduto
169300030922D1452X** ex D1378 ****** endif
169400951113     C                   endif
169500951113     C                   if        ascadereH = *ON
169600951113     C                   MOVEL     PRESente      zascaderea
169700030922D1452X** ex D1378 ****** if        WebFacingOn = 1
169800030922D1452X** ex D1378 ****** move      '*'           Z1AScadere
169900030922D1452X** ex D1378 ****** endif
170000951113     C                   endif
170100951113     C                   if        rischioH = *ON
170200060303D1948C***                MOVEL     PRESente      zrischioa
170300060303D1948C                   MOVEL     PRESePNK      zrischioa
170400030922D1452X** ex D1378 ****** if        WebFacingOn = 1
170500030922D1452X** ex D1378 ****** move      '*'           Z1Rischio
170600030922D1452X** ex D1378 ****** endif
170700951113     C                   endif
170800951113     C                   if        portafoglH = *ON
170900951113     C                   MOVEL     PRESente      zportafoga
171000030922D1452X** ex D1378 ****** if        WebFacingOn = 1
171100030922D1452X** ex D1378 ****** move      '*'           Z1Portaf
171200030922D1452X** ex D1378 ****** endif
171300951113     C                   endif
171400951113     C                   if        insritH = *ON
171500060303D1948C****               MOVEL     PRESente      zinsrita
171600060303D1948C                   MOVEL     PRESERED      zinsrita
171700030922D1452X** ex D1378 ****** if        WebFacingOn = 1
171800030922D1452X** ex D1378 ****** move      '*'           Z1InsRit
171900030922D1452X** ex D1378 ****** endif
172000951113     C                   endif
172100951113     C                   endif
172200960409     C*
172300960409     C                   ENDIF
172400951110     C*
172500951110     c* Campi variabili a seguito richiesta moneta di conto o divisa
172600951110     C                   SELECT
172700951110     C* divisa
172800951110     C                   when      SavDivVis = '1'
172900951110     C                   EVAL      S1DiVisa1  = ppadivisa
173000951110     C                   eval      s1saldo1   = H1saldod
173100951113     C                   eval      s1segno1   = H1segnod
173200951110     C                   EVAL      S1DiVisa2  = xscdiv
173300951110     C                   eval      s1saldo2   = H1saldom
173400951113     C                   eval      s1segno2   = H1segnom
173500951110     C* moneta di conto
173600951110     C                   when      SavDivVis = '0'
173700951110     C                   EVAL      S1Divisa1  = xscdiv
173800951110     C                   eval      s1saldo1   = H1saldom
173900951113     C                   eval      s1segno1   = H1segnom
174000951110     C                   EVAL      S1Divisa2  = ppadivisa
174100951110     C                   eval      s1saldo2   = H1saldod
174200951113     C                   eval      s1segno2   = H1segnod
174300951110     C                   endsl
174400951115     C*
174500951115     C* se le divise sono uguali, imposto il valore del campo in drop
174600951115     C* uguale a quello della prima riga, così almeno sono allineati
174700951115     C                   IF        ppadivisa = xscdiv
174800951115     C                   eval      s1saldo2 = s1saldo1
174900951115     C                   endif
175000090216     c*
175100090216C2119c* reperimento contenzioso della partita
175200090216C2119C                   EXSR      RepContenz
175300950704     C*
175400950704     C* reperisco se commento presente
175500950704     C                   MOVE      *BLANK        EXI724
175600950704     C                   RESET                   A724DS
175700950704     C                   MOVEL     '01'          OPZ724
175800950704     C                   MOVEL     XscSOC        SOC724
175900950704     C                   MOVEL     *BLANK        UNI724
176000951120     C                   MOVEL     'PPA'         TPA724
176100951109     C                   MOVEL     ctb086        KY1724
176200950907     C                   MOVEL     C1KCC         KY2724
176300950907     C                   MOVE      C1KSC         KY2724
176400040308     C                   MOVEL     S1DTPAR       KY3724
176500040308     C                   MOVEL     s1nrpar       KY4724
176600040308     C                   MOVE      s1serpar      KY4724
176700951109     C                   MOVEL     WDATE         DTS724
176800950704     C                   MOVEL     A724DS        KPJBU
176900950704     C                   CALL      'ANA724R'
177000950704     C                   PARM                    KPJBA
177100950704     C                   MOVEL     KPJBU         A724DS
177200950704     C* memorizzo lo stato del  bottone TCM
177300950704     C                   MOVE      EXI724        COMMENTIH
177400020118C1530C*
177500020118  "  C* Testo esistenza documenti catalogati su Laguna (se questo
177600020118  "  C*  attivo)
177700020118  "  C                   IF        xscLaguna = *on
177800020118  "  C                   EXSR      DriverLGN
177900020118  "  C*
178000020118  "  C                   EVAL      H1ClassLg = *blanks
178100020118  "  C                   EVAL      H1IdLgn = 0
178200020118  "  C                   IF        Errore = *off
178300020118  "  C                   EVAL      ANLGN00F = Dati
178400020118  "  C* salvo la chiave univoca per richiamare Laguna
178500020118  "  C                   EVAL      H1ClassLg = LGNClassLg
178600020118  "  C                   EVAL      H1IdLgn   = LGNIdLgn
178700020118  "  C                   ENDIF
178800020118  "  C*
178900020118C1530C                   ENDIF
179000950301     C*
179100950301     C* imposto quegli attributi del sfl che sono modificabili
179200950301     C* al richiamo del determinato pgm
179300950301     C                   EXSR      ATRS1
179400940131     C*
179500940131     C                   ENDSR
179600090216C2119C************************************************************
179700090216  "  C* REPERIMENTO Contenzioso x ogni partita della lettera
179800090216C2119C************************************************************
179900090216     C     RepContenz    BEGSR
180000090216     C*
180100090216     c                   reset                   ndcl99ds
180200090216     c*
180300090216     c                   eval      SocietaL99 =  XSCSOC
180400090216     c                   eval      Ctbl99     = 'CG'
180500090216     c                   eval      kccl99     =  kcc086
180600090216     c                   eval      KscL99     =  KSC086
180700090216     c     *jobrun       move      s1dtpar       $dataiso
180800090216     c                   move      $dataiso      dtparl99
180900090216     c                   eval      NrparL99=s1nrpar
181000090216     c                   eval      serparl99=s1serpar
181100090216     C                   eval      Contenzl99=*on
181200090216     c                   eval      kpjbu=NDCL99DS
181300090216     C                   CALL      'NDCL99R'
181400090216     C                   parm                    kpjba
181500090216     c                   movel     kpjbu         NDCL99DS
181600090216     c
181700090216     C* memorizzo la presenza  contenzioso per partita
181800090216     c                   if        Contenzl99=*on
181900090216     C                   MOVE      ContenzL99    H1contenz
182000090216     c                   endif
182100090216     C*
182200090216C2119C                   ENDSR
182300020118C1530C************************************************************
182400020118  "  C* reperimento dati sui documenti catalogati in Laguna
182500020118  "  C************************************************************
182600020118C1530C     DriverLGN     BEGSR
182700020118     C*
182800020118     C                   Clear                   Dati
182900020118     C                   Clear                   ANLGN00F
183000020118     C*
183100020118     C* se la sottonatura è cliente
183200020118     C                   IF        SNatura086 = 'C'
183300020118     C* imposto le chiavi per fatture attive
183400020118     C                   EVAL      LGNClassPJ = '030'
183500020118     C                   EVAL      LGNClassLg = 'DOC'
183600020118     C                   EVAL      LGNKey101 = xscsoc
183601160913>>>>>C                   EVAL      lgnKey102 = ctb086
183602160913>>>>>C                   EVAL      lgnKey103 = kcc086
183603160913>>>>>C                   EVAL      lgnKey104 = ksc086
183700160913>>>>>C                   MOVEL     '0001-01-01'  LGNKey105
183800020118     C* uso i dati di partita perchè quelli di documento non sono
183900020118     C* affidabili in questo pgm (potrei aver registrato prima la
184000020118     C*  nota che la fattura)
184100020118     C                   IF        S1DtPar <> 0
184200020118     C     *jobrun       MOVEL     S1DtPar       DataISO
184300160913>>>>>C                   MOVEL     DataISO       LGNKey105
184400020118     C                   ENDIF
184500160913>>>>>C                   MOVEL     S1NrPar       LGNKey106
184600160913>>>>>C                   EVAL      LGNKey107 = S1SerPar
184700160913>>>>>C                   EVAL      NrChiavi = 7
184800020118     C                   ENDIF
184900020118     C*
185000020118     C* se la sottonatura è fornitore
185100020118     C                   IF        SNatura086 = 'F'
185200021126     C* imposto le chiavi per fatture passive
185300020118     C                   EVAL      LGNClassPJ = '020'
185400020118     C                   EVAL      LGNClassLg = 'DOC'
185500020118     C                   EVAL      LGNKey101 = xscsoc
185501160913>>>>>C                   EVAL      lgnKey102 = ctb086
185502160913>>>>>C                   EVAL      lgnKey103 = kcc086
185503160913>>>>>C                   EVAL      lgnKey104 = ksc086
185600021126C1605C* non avendo una causale di riferimento non posso capire se si
185700021126C1605C* tratta di una partita legata ad una fattura o ad una nota
185800021126C1605C* anticipata, per cui prima testo come se fosse una fattura
185900160913>>>>>C*                  EVAL      LGNKey102 = 'F'
186000160913>>>>>C                   MOVEL     '0001-01-01'  LGNKey105
186100020118     C* uso i dati di partita perchè quelli di documento non sono
186200020118     C* affidabili in questo pgm (potrei aver registrato prima la
186300020118     C*  nota che la fattura)
186400020118     C                   IF        S1DtPar <> 0
186500020118     C     *jobrun       MOVEL     S1DtPar       DataISO
186600160913>>>>>C                   MOVEL     DataISO       LGNKey105
186700160913>>>>>C                   MOVEL     S1NrPar       LGNKey106
186800160913>>>>>C                   EVAL      LGNKey107 = S1SerPar
186900020118     C                   ENDIF
187000160913>>>>>C*                  EVAL      LGNKey106 = C1Ksc
187100160913>>>>>C                   EVAL      NrChiavi = 7
187200020118     C                   ENDIF
187300020118     C*
187400020118     C                   MOVEL     ANLGN00F      Dati
187500020118     C                   eval      Lunghezza = %size(ANLGN00F)
187600020118     C                   eval      Errore    = *off
187700020118     C*
187800020118     C                   CALLB     'PJDR1701R'
187900020118     C                   parm      '01'          Opzione                        1
188000020118     C                   parm                    Errore                         2
188100020118     C                   parm      2             Vista                          5
188200020118     C                   parm                    Dati                           3
188300020118     C                   parm                    Lunghezza                      4
188400020118     C                   parm      *off          Commit                         6
188500020118     C                   parm                    NrChiavi                       7
188600020118     C                   parm      *off          Alloca                         8
188700020118     C                   parm                    *omit                          9
188701160914>>>>>C*
188702160914>>>>>C                   LEAVESR
188800021126C1605C*
188900021126  "  C* se ho cercata per una fattura passiva
189000021126  "  C                   IF        SNatura086 = 'F'
189100021126  "  C* e non ho trovato nulla
189200021126  "  C                             and Errore = *on
189300021126  "  C* cerco chiedendomi se ci sono documenti legati ad una nota
189400021126  "  C                   EVAL      LGNKey102 = 'N'
189500021126  "  C                   MOVEL     ANLGN00F      Dati
189600021126  "  C                   eval      Errore    = *off
189700021126  "  C                   CALLB     'PJDR1701R'
189800021126  "  C                   parm      '01'          Opzione                        1
189900021126  "  C                   parm                    Errore                         2
190000021126  "  C                   parm      2             Vista                          5
190100021126  "  C                   parm                    Dati                           3
190200021126  "  C                   parm                    Lunghezza                      4
190300021126  "  C                   parm      *off          Commit                         6
190400021126  "  C                   parm                    NrChiavi                       7
190500021126  "  C                   parm      *off          Alloca                         8
190600021126  "  C                   parm                    *omit                          9
190700021126  "  C                   ENDIF
190800021126C1605C*
190900020118     C*
191000020118C1530C                   ENDSR
191100951109     C/EJECT
191200951109     C************************************************************
191300951109     C* RIEMPIMENTO DS PER saldo conto
191400951109     C************************************************************
191500951109     C     RIEx20scad    BEGSR
191600951109     C*
191700951109     C                   RESET                   x20scadds
191800951109     C                   RESET                   x20scadxds
191900951109     C                   eval      x20societa = xscsoc
192000000419     C                   eval      x20unita   = unita086
192100951109     C                   eval      x20ctb     = ctb086
192200951116     C                   eval      x20def = movdef086
192300951116     C                   eval      x20prov = movprov086
192400951116     C                   eval      x20gest = movges086
192500951109     C                   eval      x20dtpar   = PPADTPAR
192600951109     C                   eval      x20nrpar   = PPANRPAR
192700951109     C                   eval      x20serpar  = PPASerpar
192800951109     C                   eval      x20dtregmd = dtregd086
192900951109     C                   eval      x20dtregmp = dtregp086
193000951109     C                   eval      x20dtregmg = dtregg086
193100951109     C                   eval      x20kcc     = kcc086
193200951109     C                   eval      x20ksc     = ksc086
193300951116     C                   IF        divsel086 = 'I'
193400951109     C                   eval      x20divr    = divisa086
193500951109     C                   endif
193600951109     C                   eval      x20parsca  = 'S'
193700951109     C                   eval      x20elasca  = 'R'
193800951206     C                   eval      x20DtSca   = dtregd086
193900951109     C                   eval      x20ppa     = 'N'
194000951109     C                   eval      x20lrt     = 'R'
194100960513     C                   eval      x20snatura = snatura086
194200951109     C*
194300951109     C                   ENDSR
194400951109     C/EJECT
194500951109     C************************************************************
194600951109     C* Calcolo saldo per partita e totale se lo deve fare
194700951109     C************************************************************
194800951109     C     CalcSaldo     BEGSR
194900951113     C*
195000951113     C* addiziono per totale CONTO se lo deve fare (è sempre in moneta
195100951113     C* di conto)
195200951113     C                   if        $carpag = *off
195300951113     C                   add       WtotDare      WsaldototD
195400951113     C                   add       WtotAvere     WsaldototA
195500960307     C                   add       WtotDareD     WsaldotoDD
195600960307     C                   add       WtotAvereD    WsaldotoAD
195700951113     C                   endif
195800951113     C*
195900951113     C* Dati in MONETA DI CONTO
196000951109     C*
196100951110     C* Segno
196200951113     C                   exsr      RepSegno
196300951113     C                   eval      h1segnom  = Wsegno
196400951110     C* Importo
196500951109     C                   Z-add     Wsaldo        WImporto
196600951110     C                   Z-add     NrDec         WNrDec
196700951110     C                   Eval      $LenFld = %Size(h1SaldoM)
196800070202C1973c* memorizza saldo in moneta di conto
196900070202C1973c                   eval      h1saldomc  = wsaldo
197000951110     C* allineo dare a sinistra
197100961009     C                   if        h1segnoM = xscdare
197200951110     C                   SUB       5             $lenfld
197300951110     C                   Endif
197400951109     C                   EXSR      Edit
197500951110     C                   Eval      h1Saldom = $RcvrVar
197600951109     C*
197700951113     C* Dati in DIVISA
197800951110     C* segno
197900951113     C                   exsr      RepSegnoD
198000951113     C                   eval      H1SegnoD  = Wsegno
198100951109     C*
198200951110     C* Importo in divisa
198300951110     C* reperisco decimali se divisa diversa dalla precedente letta
198400951113     C                   if        Ppadivisa <> Savdivisa
198500951113     C                   eval      SavDivisa = PPaDivisa
198600951113     C                   Eval      WDivisa =  PpaDivisa
198700951113     C                   Exsr      RepDivisa
198800951113     C                   Eval      NrDecD = X01NrDec
198900951113     C                   endif
199000951110     C*
199100951110     C* importo
199200951109     C                   EVAL      Wimporto = Wsaldo
199300951110     C                   Z-add     NrDecD        WNrDec
199400951110     C                   Eval      $LenFld = %Size(h1SaldoD)
199500960315     c* memorizza saldo indivisa
199600960315     c                   eval      h1saldodiv = wsaldo
199700951110     C*
199800951110     C* Imposto la dimensione del campo video in modo da allineare
199900951110     C* valute gestite con un numero diverso di decimali
200000951109     C                   Select
200100951110     C                   When      WNrDec = 2
200200951110     C                   Eval      $LenFld = %Size(h1SaldoD) - 1
200300951110     C                   When      WNrDec = 1
200400951110     C                   Eval      $LenFld = %Size(h1Saldod) - 2
200500951110     C                   When      WNrDec = 0
200600951110     C                   Eval      $LenFld = %Size(h1Saldod) - 4
200700951109     C                   Other
200800951110     C                   Eval      $LenFld = %Size(h1Saldod)
200900951109     C                   Endsl
201000951110     C* allineo dare a sinistra
201100961009     C                   if        H1SegnoD = xscdare
201200951110     C                   SUB       5             $lenfld
201300951110     C                   Endif
201400951109     C                   EXSR      Edit
201500951110     C                   Eval      h1Saldod  = $RcvrVar
201600951109     C*
201700951109     C                   ENDSR
201800950301     C/EJECT
201900950301     C************************************************************
202000950301     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
202100950301     C************************************************************
202200950301     C     ATRS1         BEGSR
202300950301     C*
202400950704     C* azzero gli attributi non bottoni
202500951109     C                   MOVEL     X'A0'         SCADUTOA
202600951109     C                   MOVEL     X'A0'         ASCADEREA
202700951109     C                   MOVEL     X'A0'         RISCHIOA
202800951109     C                   MOVEL     X'A0'         PORTAFOGLA
202900951109     C                   MOVEL     X'A0'         INSRITA
203000960329     C***                MOVEL     X'A0'         COMMENTIA
203100951109     C                   MOVEL     X'A0'         BONIFICIA
203200951109     C                   MOVEL     X'A0'         SOLLECITIA
203300950704     C*
203400950704     C* li valorizzo a seconda dello stato ricevuto o testato
203500950704     C*
203600950704     C     COMMENTIH     IFEQ      *on
203700960329     C***                MOVEL     PRESente      COMMENTIA
203800950704     C                   ENDIF
203900951109     C     SCADUTOH      IFEQ      *on
204000060303D1948C***                MOVEL     PRESente      SCADUTOA
204100060303D1948C                   MOVEL     PRESERED      SCADUTOA
204200030922D1452X** ex D1378 ****** if        WebFacingOn = 1
204300030922D1452X** ex D1378 ****** move      '*'           S1Scaduto
204400030922D1452X** ex D1378 ****** endif
204500951109     C                   ENDIF
204600951109     C     ASCADEREH     IFEQ      *on
204700951109     C                   MOVEL     PRESente      ASCADEREA
204800030922D1452X** ex D1378 ****** if        WebFacingOn = 1
204900030922D1452X** ex D1378 ****** move      '*'           S1AScadere
205000030922D1452X** ex D1378 ****** endif
205100951109     C                   ENDIF
205200951109     C     RISCHIOH      IFEQ      *on
205300060303D1948C***                MOVEL     PRESente      RISCHIOA
205400060303D1948C                   MOVEL     PRESePNK      RISCHIOA
205500030922D1452X** ex D1378 ****** if        WebFacingOn = 1
205600030922D1452X** ex D1378 ****** move      '*'           S1Rischio
205700030922D1452X** ex D1378 ****** endif
205800951109     C                   ENDIF
205900951109     C     PORTAFOGLH    IFEQ      *on
206000951109     C                   MOVEL     PRESente      PORTAFOGLA
206100030922D1452X** ex D1378 ****** if        WebFacingOn = 1
206200030922D1452X** ex D1378 ****** move      '*'           S1Portafog
206300030922D1452X** ex D1378 ****** endif
206400951109     C                   ENDIF
206500951109     C     INSRITH       IFEQ      *on
206600060303D1948C****               MOVEL     PRESente      INSRITA
206700060303D1948C                   MOVEL     PRESERED      INSRITA
206800030922D1452X** ex D1378 ****** if        WebFacingOn = 1
206900030922D1452X** ex D1378 ****** move      '*'           S1InsRit
207000030922D1452X** ex D1378 ****** endif
207100951109     C                   ENDIF
207200951109     C     BONIFICIH     IFEQ      *on
207300951109     C                   MOVEL     PRESente      BONIFICIA
207400030922D1452X** ex D1378 ****** if        WebFacingOn = 1
207500030922D1452X** ex D1378 ****** move      '*'           S1Bonifici
207600030922D1452X** ex D1378 ****** endif
207700951109     C                   ENDIF
207800951109     C     SOLLECITIH    IFEQ      *on
207900951109     C                   MOVEL     PRESente      SOLLECITIA
208000030922D1452X** ex D1378 ****** if        WebFacingOn = 1
208100030922D1452X** ex D1378 ****** move      '*'           S1Sollecit
208200030922D1452X** ex D1378 ****** endif
208300951109     C                   ENDIF
208400950704    >C* impostare le condizioni per ripristinare gli attributi
208500950704    >C* della riga del sfl (se ce ne sono)
208600951113      * attiva o no unità
208700951113     C                   if        fgo1=*on
208800951113     C                   eval      *in11=*on
208900951113     C                   else
209000951113     C                   eval      *in11=*off
209100951113     C                   endif
209200951115      * attiva agente e solleciti solo se cliente
209300951115     C                   if        snatura086 = 'C'
209400951115     C                   eval      *in13=*on
209500951115     C                   else
209600951115     C                   eval      *in13=*off
209700951115     C                   endif
209800951123      * attiva bonifici solo per fornitore
209900951123     C                   if        snatura086 = 'F'
210000951123     C                   eval      *in14=*on
210100951123     C                   else
210200951123     C                   eval      *in14=*off
210300951123     C                   endif
210400960315      * sottolinea saldo1 solo se uguale all'importo originario e se è in divisa
210500960315     C                   if        s1imporig  = h1saldodiv and
210600990617     c                             s1imporig <> 0 and h1segnoD= SegnoOrig
210700960315     C                   eval      *in15=*on
210800960315     C                   else
210900960315     C                   eval      *in15=*off
211000960315     C                   endif
211100960328     c*
211200960328     c* sottolinea la data partita se ci sono commenti
211300960328     C     COMMENTIH     IFEQ      *on
211400960328     C                   eval      *in16=*on
211500960328     C                   else
211600960328     C                   eval      *in16=*off
211700960328     C                   endif
211800960328     c*
211900960328     c* metto il saldo ad alta intesità se ci sono movimenti provvisori
212000960328     C     PROVVISORH    IFEQ      *on
212100960328     C                   eval      *in18=*on
212200960328     C                   else
212300960328     C                   eval      *in18=*off
212400960328     C                   endif
212500020118C1530C*
212600020118  "   * se c'è valorizzata l'Id di Laguna
212700020118  "  C                   EVAL      *in41=*off
212800020118  "  C                   IF        H1IdLgn <> 0
212900020118  "  C* metto in RI il numero documento per segnalare che esiste un
213000020118  "  C* documento catalogato per questa registrazione (o una fattura
213100020118  "  C*  o una nota)
213200020118  "  C                   EVAL      *in41=*on
213300020118C1530C                   endif
213400950301     C*
213500950301     C                   ENDSR
213600941205     C/EJECT
213700941205     C************************************************************
213800941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
213900941205     C* INPUT
214000951109     C* $FC1 = Abilitazioni videata S1
214100941205     C* OUTPUT
214200941205     C* *IN97 *IN99 Errori
214300941205     C************************************************************
214400941205     C     CTRKS1        BEGSR
214500941205     C*
214600941205     C                   SETOFF                                       99
214700951109     C                   MOVEA     $FC1          $FC
214800941205     C                   EXSR      CTRKEY
214900941205     C     *IN99         IFEQ      *ON
215000941205     C                   SETON                                        97
215100941205     C                   ENDIF
215200941205     C*
215300941205     C                   ENDSR
215400981007     C/EJECT
215500981007     C************************************************************
215600981007     C* GESTIONE F02 VIDEO S1  dati anagrafici conto
215700981007     C************************************************************
215800981007     C     F02S1         BEGSR
215900981007     C*
216000981007     C                   reset                   ana397ds
216100981007     C                   eval      societa397 = XSCSOC
216200981007     C                   eval      unita397 = unita086
216300981007     C                   eval      kcc397 = kcc086
216400981007     C                   eval      ksc397 = KSC086
216500981007     C                   eval      wdw397 = *on
216600981007     C                   movel     ana397ds      kpjbu
216700981007     C                   CALL      'ANA397R'                            21
216800981007     C                   PARM                    KPJBA
216900981009     c                   movel     kpjbu         ana397ds
217000981007     C*
217100990714C1202C* Controlla esistenza commenti
217200990714  "  c                   exsr      TstTcm
217300990714  "  C* Attivo o no scritta commenti
217400990714  "  C                   IF        exi724     = *on
217500990714  "  c                   eval      *in19 = *on
217600990714  "  C                   else
217700990714  "  c                   eval      *in19 = *off
217800990714C1202C                   endif
217900981007     C                   ENDSR
218000941206     C/EJECT
218100941206     C************************************************************
218200941206     C* GESTIONE F08 VIDEO S1
218300941206     C************************************************************
218400941206     C     F08S1         BEGSR
218500941206     C*
218600941206     C                   MOVE      *ON           $FINE
218700951108    >C                   MOVE      '0'           RET086
218800941206     C*
218900941206     C                   ENDSR
219000940309     C/EJECT
219100940309     C************************************************************
219200940309     C* GESTIONE F03 VIDEO S1
219300940309     C************************************************************
219400940309     C     F03S1         BEGSR
219500940309     C*
219600940309     C                   MOVE      *ON           $FINE
219700951108    >C                   MOVE      '1'           RET086
219800940325     C* fine programma
219900940325     C                   EXSR      ENDPGM
220000940309     C*
220100940309     C                   ENDSR
220200951109     C/EJECT
220300980930     C************************************************************
220400981001     C* GESTIONE F07 VIDEO S1
220500980930     C************************************************************
220600981001     C     F07S1         BEGSR
220700980930     C                   RESET                   NDC199DS
220800980930     c*
220900980930     C* Se richiamato in ricerca, passo stessa opzione
221000981002     C                   MOVE      '02'          OPZ199
221100980930     c* Campi provenienti dal ndc086ds
221200980930     C                   move      ctb086        ctb199
221300981001     C                   move      unita086      unita199
221400980930     C                   move      kcc086        kcc199
221500980930     C                   move      ksc086        ksc199
221600980930     C                   move      movdef086     movdef199
221700980930     C                   move      movprov086    movprov199
221800980930     C                   move      movges086     movges199
221900981001     C                   move      dtperda086    dtperda199
222000981001     C                   move      dtpera086     dtpera199
222100981001     c                   move      ord086        ord199
222200981001     C                   move      divvis086     divvis199
222300981001     C                   move      divsel086     divsel199
222400981001     C                   move      divisa086     divisa199
222500981001     C                   move      sitpar086     sitpar199
222600980930     C                   move      dtregd086     dtregd199
222700980930     C                   move      dtregp086     dtregp199
222800980930     C                   move      dtregg086     dtregg199
222900981001     C                   move      ctgan1086     ctgan1199
223000981001     C                   move      ctgan2086     ctgan2199
223100981001     C                   move      ctgan3086     ctgan3199
223200981001     C                   move      kscage086     kscage199
223300981001     C                   move      bkamm086      bkamm199
223400981001     C                   move      kccfact086    kccfact199
223500981001     C                   move      kscfact086    kscfact199
223600980930     C                   eval      DESCR199   = DESCR086
223700980930     C                   eval      DESAGG199  = desagg086
223800980930     C                   eval      PERCIPI199 = percipi086
223900980930     C                   eval      SNATURA199 = snatura086
224000980930     C                   eval      analit199  = analit086
224100981001     C                   eval      calcsal199 = calcsal086
224200981001     C                   eval      saldo199   = saldo086
224300981001     C                   eval      segno199   = segno086
224400981001     C                   eval      scaduto199 = scaduto086
224500981001     C                   eval      ascader199 = ascader086
224600981001     C                   eval      rischio199 = rischio086
224700981001     C                   eval      portafo199 = portafo086
224800981001     C                   eval      insrit199  = insrit086
224900981001     C                   eval      saldod199  = saldod086
225000981001     C                   eval      segnod199  = segnod086
225100980930     c                   move      multip086     multip199
225200010409c1481C                   eval      stpcom199  = stpcom086
225300031120D1498C                   eval      Natura199  = wrknatura
225400051215C1892c                   move      DataSF086     DataSF199
225700980930     C                   CALL      'NDC199R'
225800980930     C                   PARM                    KPJBA
225900980930     c                   PARM                    ndc199ds
226000980930     C                   PARM                    SOC001
226100980930     C                   PARM                    AUTDS
226200980930     C*
226300980930     C* ritorno da PGM movimenti
226400981002     C                   EXSR      RTN199
226500980930     C*
226600980930     C*
226700980930     C                   ENDSR
226800951109     C************************************************************
226900951109     C* GESTIONE F11 VIDEO S1 - cambio moneta
227000951109     C************************************************************
227100951109     C     F11S1         BEGSR
227200951113     C*
227300951113     C* Eseguo solo se ci sono record nel sfl
227400951113     C                   IF        Wmax <> 0
227500951110     C* Scambio richiesta
227600951110     C                   select
227700951110     C                   when      Savdivvis = '0'
227800951110     C                   eval      Savdivvis = '1'
227900951110     C*
228000951110     C                   when      Savdivvis = '1'
228100951110     C                   eval      Savdivvis = '0'
228200951110     C                   endsl
228300951110     C*
228400951110     C                   DO        *hival        $X                4 0
228500951110     C     $X            chain     c086s1                             21
228600951110     C   21              LEAVE
228700951110     C                   select
228800951110     C                   when      Savdivvis = '0'
228900951110     C                   eval      s1saldo1 = h1saldom
229000951110     C                   EVAL      S1DiVisa1  = S1monconto
229100020424D1126C                   EVAL      S1segno1   = h1segnoM
229200951110     C                   eval      s1saldo2 = h1saldod
229300951110     C                   EVAL      S1DiVisa2  = S1divisa
229400020424D1126C                   EVAL      S1segno2   = h1segnoD
229500951110     c*
229600951110     C                   when      Savdivvis = '1'
229700951110     C                   eval      s1saldo1 = h1saldod
229800951110     C                   EVAL      S1DiVisa1  = S1divisa
229900020424D1126C                   EVAL      S1segno1   = h1segnoD
230000951110     C                   eval      s1saldo2 = h1saldom
230100951110     C                   EVAL      S1DiVisa2  = S1monconto
230200020424D1126C                   EVAL      S1segno2   = h1segnoM
230300951110     C                   endsl
230400951115     C*
230500951115     C* se le divise sono uguali, imposto il valore del campo in drop
230600951115     C* uguale a quello della prima riga, così almeno sono allineati
230700951115     C                   IF        s1divisa1 = s1divisa2
230800951115     C                   eval      s1saldo2 = s1saldo1
230900951115     C                   endif
231000960627     c*sflnxtchg
231100960626     c                   if        s1opz <> 0
231200960626     c                   seton                                        32
231300960627     c                   else
231400960627     c                   setoff                                       32
231500960626     c                   endif
231600951113     C* imposto quegli attributi del sfl che sono modificabili
231700951113     C* al richiamo del determinato pgm
231800951113     C                   EXSR      ATRS1
231900960627     C*  spengo r.i. per opzione
232000960627     c                   setoff                                       40
232100951110     C                   UPDATE    C086S1
232200951110     C*
232300951110     C                   ENDDO
232400951109     C*
232500951113     C                   ENDIF
232600951109     C                   ENDSR
232700940309     C/EJECT
232800940309     C************************************************************
232900940309     C* GESTIONE F12 VIDEO S1
233000940309     C************************************************************
233100940309     C     F12S1         BEGSR
233200940309     C*
233300940309     C                   MOVE      *ON           $FINE
233400951108    >C                   MOVE      '2'           RET086
233500940309     C*
233600940309     C                   ENDSR
233700960314     C/EJECT
233800960314     C************************************************************
233900960314     C* GESTIONE F13 VIDEO S1  mette '11' su tutte le partite
234000960314     C************************************************************
234100960314     C     F13S1         BEGSR
234200970219     C*
234300970219     c                   z-add     0             Wc1rcd
234400970219     c* Vedo se immessa una opzione o niente
234500040308     C                   IF        Wmax <> 0
234600970219     C*
234700970219     C* Dopo metto opzione 11 su tutti i record del subfile se non c'è
234800970219     c* nessuna opzione, altrimenti metto quella immessa
234900970219     c                   readC     c086s1                                 21
235000970219     c* se non trova niente
235100040308     c                   if        *in21 = *on
235200970219     c                   z-add     1             $Y                3 0
235300970219     C                   z-add     11            WOPZ              2 0
235400970219     c                   else
235500040308     c     s1opz         doweq     0
235600970219     c                   readC     c086s1                                 21
235700970219     c   21              leave
235800040308     c                   enddo
235900040308     c                   if        s1opz = 0
236000970219     c                   z-add     1             $Y                3 0
236100970219     C                   z-add     11            WOPZ              2 0
236200970219     c                   z-add     1             Wc1rcd            3 0
236300970219     c                   else
236400970219     c                   z-add     s1nrr         $Y
236500970219     C                   z-add     S1opz         WOPZ              2 0
236600970219     c                   z-add     s1nrr         Wc1rcd
236700040308     c                   endif
236800040308     c                   endif
236900040308     c                   endif
237000960315     c*
237100970219     C* devo ricaricare tutto il sfl se il caricamento era a pagina
237200960315     c                   if        $carpag = *on
237300960315     c                   eval      $carpag = *off
237400960315     C                   EVAL      SFLPAG = 9999
237500960315     C                   EVAL      WPAG   = 9999
237600960315     C                   eval      z1saldo = *blanks
237700960315     C                   eval      z1segno = ' '
237800970422     C                   eval      z1saldodiv = *blanks
237900970422     C                   eval      z1segnodiv = ' '
238000960315     c                   exsr      inzs1
238100960315     c                   endif
238200970219     c*
238300970219     C* Dopo metto opzione 11 su tutti i record del subfile se non c'è
238400970219     c* nessuna opzione, altrimenti metto quella immessa
238500040308     C                   IF        Wmax <> 0
238600970219     c*
238700040308     C     $Y            DO        *hival        $X                4 0
238800960315     C     $X            chain     c086s1                             21
238900960315     C   21              LEAVE
239000970219     C                   z-add     wopz          s1opz
239100960315     C                   EXSR      ATRS1
239200960315     C                   SETON                                        32
239300960315     C                   UPDATE    C086S1
239400040308     C                   ENDDO
239500970219     c*
239600040308     C                   ENDIF
239700970219     c*
239800970219     c                   if        Wc1rcd <>0
239900970219     c                   z-add     wc1rcd        c1rcd
240000970219     c                   endif
240100970219     C*
240200960314     C                   ENDSR
240300951109     C/EJECT
240400951109     C************************************************************
240500951109     C* GESTIONE F15 VIDEO S1
240600951109     C************************************************************
240700951109     C     F15S1         BEGSR
240800951109     C*
240900951109     C                   reset                   ana399ds
241000951109     C                   eval      A39societa = XSCSOC
241100951109     C                   eval      A39unita = unita086
241200951109     C                   eval      A39ctb = ctb086
241300951109     C                   eval      A39kcc = kcc086
241400951109     C                   eval      A39ksc = KSC086
241500951109     C                   eval      A39SNATURA = snatura086
241600951109     C                   movel     ana399ds      kpjbu
241700951109     C                   CALL      'ANA399R'                            21
241800951109     C                   PARM                    KPJBA
241900981007     C     A39ERRORI     IFne      '1'
242000981007     c* se modificata esistenza commenti riemetto la testata
242100981007     c                   exsr      TstTcm
242200981007     c                   select
242300981007     C                   when      exi724     = *on and *in19 = *off
242400981007     c                   eval      *in19 = *on
242500981007     c                   write     c086t1
242600981007     C                   EXSR      GESPIE
242700981007     C                   when      exi724     =*off and *in19 = *on
242800981007     c                   eval      *in19 = *off
242900981007     c                   write     c086t1
243000981007     C                   EXSR      GESPIE
243100981007     C                   endsl
243200981007     c                   endif
243300951109     C*
243400951109     C                   ENDSR
243500960109     C/EJECT
243600971027     C************************************************************
243700971027     C* GESTIONE F16 VIDEO S1         Riepilogo per divisa
243800971027     C************************************************************
243900971027     C     F16S1         BEGSR
244000971027     C*
244100971027     C* Calcolo saldi per divisa
244200971029     C                   Reset                   Ndc291Ds
244300971029     C                   MOVE      XscSoc        Societa291
244400971029     C                   MOVE      Ctb086        Ctb291
244500971029     C                   MOVE      Unita086      Unita291
244600971029     C                   MOVE      Kcc086        Kcc291
244700971029     C                   MOVE      Ksc086        Ksc291
244800971030     C                   Move      Divisa086     DivisaS291
244900971030     C                   Move      DivSel086     DivSel291
245000971027     c
245100971029     C                   move      DtPerDa086    DtInPe291
245200971029     C                   move      DtPerA086     DtFiPe291
245300971029     C                   move      MovDef086     MovDef291
245400971029     C                   move      MovProv086    MovPro291
245500971029     C                   move      MovGes086     MovGes291
245600971029     C                   move      SitPar086     SitPar291
245700971029     C                   move      DtRegD086     DtRegD291
245800971029     C                   move      DtRegP086     DtRegP291
245900971029     C                   move      DtRegG086     DtRegG291
246000971029     C                   move      CtgAn1086     CtgAn1291
246100971029     C                   move      CtgAn2086     CtgAn2291
246200971029     C                   move      CtgAn3086     CtgAn3291
246300971029     C                   move      KscAge086     KscAge291
246400971029     C                   move      BkAmm086      BkAmm291
246500080129c2074C                   move      kccfact086    kccfact291
246600080129c2074C                   move      kscfact086    kscfact291
246700080129c2074C                   move      Snatura086    Snatura291
247000971027     C
247100971029     C                   Eval      Kpjbu = Ndc291Ds
247200971029     C                   CALL      'NDC291R'                            21
247300971027     C                   PARM                    KPJBA
247400971029     C                   PARM                    Ndc291Ds1
247500971027     C*
247600971029     c                   Movel     Kpjbu         Ndc291Ds
247700971029     C     Esito291      Ifeq      *on
247800971030     C* Generazione saldi terminata in errore
247900971030     C                   SETON                                        9099
248000971029     C                   Else
248100971029     C* Visualizzazione saldi in divisa
248200971029     C                   Reset                   Ndc293Ds
248300971029     C                   EVAL      Societa293 = Xscsoc
248400971029     C                   EVAL      Kcc293     = kcc086
248500971029     C                   EVAL      Ksc293     = ksc086
248600971103     C                   Movel     Descr086      DesBrev293
248700971029     C                   Eval      Kpjbu = Ndc293Ds
248800971029     C                   CALL      'NDC293R'                            21
248900971027     C                   PARM                    KPJBA
249000971103     C                   PARM                    SOC001
249100971029     C                   PARM                    Ndc291Ds1
249200971030     C* Testo uscita
249300971029     c                   Movel     Kpjbu         Ndc293Ds
249400971027     C* Se F3 vado a fine programma
249500971029     C     Ret293        Ifeq      '1'
249600971027     C                   MOVE      *ON           $FINE
249700971027    >C                   MOVE      '1'           RET086
249800971027     C                   EXSR      ENDPGM
249900971027     C                   endif
250000971027     C                   endif
250100971027     C*
250200971027     C                   ENDSR
250300971027     C/EJECT
250400960109     C************************************************************
250500960109     C* GESTIONE F20 VIDEO S1 - Stampa E/C
250600960109     C************************************************************
250700960109     C     F20S1         BEGSR
250800960109     C* Reperimento titolo stampa
250900960109     C                   EXSR      REPTIT
251000960111     C                   SELECT
251100960111     C* clienti
251200960111     c                   WHEN      snatura086 = 'C'
251300960109     C                   RESET                   NDC141DS
251400960109     C                   MOVE      xscsoc        soc141
251500960109     C                   MOVE      ctb086        ctb141
251600960109     C                   MOVE      Unita086      uni141
251700960109     C                   MOVE      kcc086        KCC141
251800960109     C                   MOVE      ksc086        KSC141
251900960109     C                   move      movdef086     def141
252000960109     C                   move      movprov086    pro141
252100960109     C                   move      movges086     ges141
252200960109     C                   move      dtperda086    dpd141
252300960109     C                   move      dtpera086     dpa141
252400030312D1303C                   move      divsel086     income141
252500960109     C                   MOVE      divisa086     div141
252600960109     C                   move      sitpar086     sip141
252700960109     C                   move      dtregd086     dmd141
252800960109     C                   move      dtregp086     dmp141
252900960109     C                   move      dtregg086     dmg141
253000960109     C                   MOVE      ctgan1086     ct1141
253100960109     C                   MOVE      ctgan2086     ct2141
253200960109     C                   MOVE      ctgan3086     ct3141
253300960109     C                   MOVE      kscage086     arc141
253400980227     C                   MOVE      bkamm086      bka141
253500980227     C                   MOVE      kccfact086    kcf141
253600980227     C                   MOVE      kscfact086    ksf141
253700960111     c                   eval      lr141 = *on
253800010409c1481C                   MOVE      stpcom086     stpcom141
253900051216c1892C                   MOVE      DataSF086     DataSf141
254200960109     C                   MOVEL     NDC141DS      KPJBU
254300960109     C                   CALL      'NDC146R1'
254400960109     C                   PARM                    KPJBA
254500960109     C                   PARM                    TIT              50
254600960111     C* fornitori
254700960111     c                   WHEN      snatura086 = 'F'
254800960111     C                   RESET                   NDC142DS
254900960111     C                   MOVE      xscsoc        soc142
255000960111     C                   MOVE      ctb086        ctb142
255100960111     C                   MOVE      Unita086      uni142
255200960111     C                   MOVE      kcc086        KCC142
255300960111     C                   MOVE      ksc086        KSC142
255400960111     C                   move      movdef086     def142
255500960111     C                   move      movprov086    pro142
255600960111     C                   move      movges086     ges142
255700960111     C                   move      dtperda086    dpd142
255800960111     C                   move      dtpera086     dpa142
255900030312D1303C                   move      divsel086     income142
256000960111     C                   MOVE      divisa086     div142
256100960111     C                   move      sitpar086     sip142
256200960111     C                   move      dtregd086     dmd142
256300960111     C                   move      dtregp086     dmp142
256400960111     C                   move      dtregg086     dmg142
256500960111     C                   MOVE      ctgan1086     ct1142
256600960111     C                   MOVE      ctgan2086     ct2142
256700960111     C                   MOVE      ctgan3086     ct3142
256800960111     C                   MOVE      bkamm086      bka142
256900980227     C                   MOVE      kccfact086    kcf142
257000980227     C                   MOVE      kscfact086    ksf142
257100960111     c                   eval      lr142 = *on
257200010409c1481C                   MOVE      stpcom086     stpcom142
257300051216c1892C                   MOVE      DataSF086     DataSf142
257600960111     C                   MOVEL     NDC142DS      KPJBU
257700960111     C                   CALL      'NDC147R1'
257800960111     C                   PARM                    KPJBA
257900960111     C                   PARM                    TIT              50
258000960111     C*
258100960111     C                   OTHER
258200960111     C*altri conti
258300960111     C                   RESET                   NDC143DS
258400960111     C                   MOVE      xscsoc        soc143
258500960111     C                   MOVE      ctb086        ctb143
258600960111     C                   MOVE      Unita086      uni143
258700960111     C                   MOVE      kcc086        KCC143
258800960111     C                   MOVE      ksc086        KSC143
258900960111     C                   move      movdef086     def143
259000960111     C                   move      movprov086    pro143
259100960111     C                   move      movges086     ges143
259200960111     C                   move      dtperda086    dpd143
259300960111     C                   move      dtpera086     dpa143
259400030312D1303C                   move      divsel086     income143
259500960111     C                   MOVE      divisa086     div143
259600960111     C                   move      sitpar086     sip143
259700960111     C                   move      dtregd086     dmd143
259800960111     C                   move      dtregp086     dmp143
259900960111     C                   move      dtregg086     dmg143
260000960111     c                   eval      lr143 = *on
260100010410c1481C                   MOVE      stpcom086     stpcom143
260200960111     C                   MOVEL     NDC143DS      KPJBU
260300960111     C                   CALL      'NDC148R1'
260400960111     C                   PARM                    KPJBA
260500960111     C                   PARM                    TIT              50
260600960111     c*
260700960111     C                   ENDSL
260800960109     C*
260900960109     c                   endsr
261000020527c1567C************************************************************
261100020527     C* stampa griglia
261200020527     C************************************************************
261300020527     C     F21S1         BEGSR
261400020527     C*
261500020527     C                   RESET                   NDCG12DS
261600020527     C                   RESET                   NDCG01DS
261700020527     c* dico di non inizializzare i parametri di calcolo della griglia in
261800020527     c* quanto glieli passo
261900020527     c                   eval      inzg12=*off
262000020527     C                   MOVE      xscsoc        societaG01
262100020527     C                   MOVE      ctb086        ctbG01
262200020527     C                   MOVE      Unita086      unitaG01
262300020528     C                   MOVE      Snatura086    SnaturaG01
262400020527     C                   MOVE      kcc086        KCCG01
262500020527     C                   MOVE      ksc086        KSCg01
262600020527     C                   move      movdef086     movdefg01
262700020527     C                   move      movprov086    movprovg01
262800020527     C                   move      movges086     movgesg01
262900020527     C                   move      dtperda086    DtPerDag01
263000020527     C                   move      dtpera086     DtPerAG01
263100020527     C                   MOVE      divisa086     DivisaG01
263200020528     C                   MOVE      divsel086     divselG01
263300020527     C                   move      dtregd086     DtREgDG01
263400020528     C                   move      dtregd086     DtScaG01
263500020527     C                   move      dtregp086     dtregPG01
263600020527     C                   move      dtregg086     DtRegGg01
263700020527     C                   MOVE      ctgan1086     ctgan1g01
263800020527     C                   MOVE      ctgan2086     ctgan2g01
263900020527     C                   MOVE      ctgan3086     ctgan3g01
264000020527     C                   MOVE      kscage086     kscageg01
264100020527     C                   movel(P)  ndcg12ds      kpjbu
264200020527     C                   CALL      'NDCG12R'
264300020527     C                   PARM                    KPJBA
264400020527     C                   PARM                    ndcg01ds
264500020527     C                   movel     kpjbu         ndcg12ds
264600020527     C*
264700020527     C*  errore
264800020527     C     Errg12        IFEQ      '1'
264900020527     C                   EXSR      endpgm
265000020527     C                   endif
265100020527     C*
265200020527c1567c                   ENDSR
265300960109     C/EJECT
265400960109     C************************************************************
265500960109     C* REPERIMENTO TITOLO stampa partite
265600960109     C************************************************************
265700960109     C     REPTIT        BEGSR
265800960109     C*
265900960109     C                   MOVE      *BLANKS       TIT
266000960109     C*
266100960111     c                   select
266200960111     c                   when      snatura086 = 'C'
266300960109     C                   MOVEL     $TIT(1)       IDMSG
266400960111     c                   when      snatura086 = 'F'
266500960111     C                   MOVEL     $TIT(2)       IDMSG
266600960111     c                   other
266700960111     C                   MOVEL     $TIT(3)       IDMSG
266800960111     c                   endsl
266900960109     C                   CALLB     'XRTVM'
267000960109     C                   PARM                    IDMSG            27
267100960109     C                   PARM                    TXTMSG          100
267200960109     C                   PARM                    ERRMSG            1
267300960109     C                   PARM      *ON           CTRMSG            1            centratura
267400960109     C                   PARM      50            LENMSG            3 0          lun output.
267500960109     C*
267600960109     C     ERRMSG        IFNE      *ON
267700960109     C                   MOVEL     TXTMSG        TIT
267800960109     C                   ELSE
267900960109     C                   MOVEL     *ALL'?'       TIT
268000960109     C                   ENDIF
268100960109     C*
268200960109     C                   ENDSR
268300940309     C/EJECT
268400940309     C************************************************************
268500940309     C* GESTIONE F23 VIDEO S1
268600940309     C************************************************************
268700940309     C     F23S1         BEGSR
268800940309     C*
268900040308     C                   Z-ADD     76            $L01
269000040308     C                   Z-ADD     76            $L02
269100940309     C                   EXSR      RIEOPZ
269200941214     C                   Z-ADD     23            X
269300951109     C                   MOVE      FF23          $FC1(X)
269400940608     C                   MOVEL     WOPZ1         C1OPZ1
269500060224C1913C                   MOVEL     $ALLOPT       H1ALLOPT
269600951109     C                   MOVEL     WOPZ2         C1OPZ2
269700940309     C                   Z-ADD     WCOZ          C1COZ
269800940309     C*
269900940309     C                   ENDSR
270000940309     C/EJECT
270100940309     C************************************************************
270200940309     C* GESTIONE F24 VIDEO S1
270300940309     C************************************************************
270400940309     C     F24S1         BEGSR
270500940506     C*
270600951109     C                   MOVEA     $FX1          $FX
270700951109     C                   MOVEA     $FC1          $FC
270800951109     C                   MOVEA     $FR1          $FR
270900940506     C                   Z-ADD     $ULKS1        $ULK
271000040308     C                   Z-ADD     79            $L01
271100040308     C                   Z-ADD     79            $L02
271200940506     C                   EXSR      RIEKEY
271300940506     C                   Z-ADD     $ULK          $ULKS1            3 0
271400951109     C                   MOVEA     $FC           $FC1
271500951109     C                   MOVEL     $KEY1         Z1KE1
271600060224C1913C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
271700951109     C                   MOVEL     $KEY2         Z1KE2
271800951109     C                   WRITE     C086Z1
271900940506     C*
272000940309     C                   ENDSR
272100940128     C/EJECT
272200940128     C************************************************************
272300940131     C* CONTROLLO TESTATA LISTA
272400940128     C************************************************************
272500940131     C     CTRC1         BEGSR
272600940128     C*
272700940201     C                   MOVE      *OFF          *IN99
272800951109     C*
272900951109     C                   if        c1pos <> 0
273000951109     C                   CALLb     'XDT4000'
273100951109     C                   PARM                    XDTAMG           10
273200951109     C                   PARM      c1pos         XDTGMA            6 0
273300951109     C                   PARM      3             XDTSTA            1 0
273400951109     C     XDTSTA        IFLT      0
273500951109     C                   SETON                                        6099
273600951109     C                   endif
273700951109     C                   endif
273800940131     C*
273900940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
274000950707     C* L'INIZIALIZZAZIONE LISTA   (lo attivo anche se w1pos = c1pos, per ri
274100950707     C* partire dal primo numero partita
274200951109     C                   IF        *in99 = *off
274300950707     C     C1POS         IFNE      W1POS
274400040308     C* aggiungere in OR altri campi posizionamento se ci sono
274500940202     C                   EXSR      POSS1
274600950707     C                   ENDIF
274700951109     C                   Endif
274800940202     C*
274900940202     C                   ENDSR
275000940202     C/EJECT
275100940202     C************************************************************
275200940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
275300940202     C************************************************************
275400940202     C     POSS1         BEGSR
275500940202     C*
275600940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
275700940202     C*
275800040308     C     WMAX          IFGT      0
275900040308     C                   READC     C086S1                                 21
276000040308     C     *IN21         DOWEQ     *OFF
276100040308     C     *IN99         ANDEQ     *OFF
276200040308     C*
276300040308     C     S1OPZ         IFNE      0
276400040308     C                   SETON                                        51  99
276500040308     C                   ENDIF
276600040308     C* se rilevata opzione attiva, attivo sflnxtchg
276700040308     C     *IN99         IFEQ      *ON
276800040308     C                   MOVE      *ON           *IN32
276900040308     C*
277000040308     C* imposto quegli attributi del sfl che sono modificabili
277100040308     C* al richiamo del determinato pgm
277200040308     C                   EXSR      ATRS1
277300040308     C*
277400040308     C                   UPDATE    C086S1
277500040308     C                   ELSE
277600040308     C                   READC     C086S1                                 21
277700040308     C                   ENDIF
277800040308     C*
277900040308     C                   ENDDO
278000040308     C*
278100040308     C                   ENDIF
278200040308     C*
278300040308     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
278400040308     C* AL VALORE DI POSIZIONAMENTO :
278500040308     C* se questo è maggiore del valore del rispettivo campo di
278600040308     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
278700040308     C* con i valori di parzializzazione.
278800040308     C*
278900040308     C     *IN99         IFEQ      *OFF
279000040308     C                   MOVE      *ZEROS        W1POS
279100040308     C*
279200950525     C* per ogni ordinamento devo testare se il campo dell'inizio elenco
279300950525     C* è maggiore del relativo valore di parzializzazione, nel qual caso
279400950525     C* imposto la variabile del posizionamento (W1Pos) al nuovo valore;
279500950525     C* richiedo comunque un ricaricamento del sfl che, se non si è
279600950525     C* verificata la condizione suddetta avverrà con posizionamento a
279700950525     C* partire dal valore di parzializzazione
279800950525     C*
279900040308     C                   SELECT
280000040308     C* Attenzione : se un ordinamento ha un campo di posizionamento
280100040308     C* specifico bisogna sostituire la gestione W1Pos/C1Pos con quella
280200040308     C* dei relativi campi
280300040308     C*
280400040308     C     $ORD          WHENEQ    1
280500040308     C                   MOVEL     C1POS         W1POS
280600040308     C*
280700040308     C     $ORD          WHENEQ    2
280800040308     C                   MOVE      C1POS         W1POS
280900950525     C*
281000040308     C                   ENDSL
281100040308     C*
281200040308     C* richiedo l'inizializzazione sfl
281300040308     C                   MOVE      *ON           *IN99
281400040308     C                   MOVE      *ON           $INZS1
281500040308     C*
281600040308     C                   ENDIF
281700940202     C*
281800940131     C                   ENDSR
281900940131     C/EJECT
282000940131     C************************************************************
282100940131     C* CONTROLLO OPZIONI LISTA
282200940131     C************************************************************
282300940131     C     CTRS1         BEGSR
282400940131     C*
282500940202     C                   MOVEL     *OFF          $ESCI
282600940201     C                   SETOFF                                       99
282700940131     C                   Z-ADD     0             S1OPZ
282800940131     C*
282900940127     C* Leggo il sfl solo se ci sono rcd
283000040308     C     WMAX          IFGT      0
283100951109     C                   READC     C086S1                                 21
283200010907C1511C*
283300010828  "  C* imposto che in questo ciclo di lettura devo ancora aprire
283400010828  "  C* la finestra di conferma
283500010907C1511C                   Eval      OpenWdw46 =   *Off
283600940127     C*
283700940131     C* esce se fine sfl o errore che richiede l'uscita
283800040308     C     *IN21         DOWEQ     *OFF
283900040308     C     $ESCI         ANDEQ     *OFF
284000040308     C                   Z-ADD     S1NRR         C1RCD
284100040308     C* ctrl su riga
284200040308     C                   EXSR      RECS1
284300040308     C* gestione opzioni
284400040308     C     S1OPZ         IFNE      0
284500040308     C     *IN99         ANDEQ     *OFF
284600040308     C                   EXSR      OPZS1
284700040308     C                   ENDIF
284800040308     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
284900040308     C     *IN99         IFEQ      *ON
285000040308     C                   MOVE      *ON           *IN32
285100040308     C                   ENDIF
285200040308     C*
285300040308     C* imposto quegli attributi del sfl che sono modificabili
285400040308     C* al richiamo del determinato pgm
285500040308     C                   EXSR      ATRS1
285600040308     C*
285700040308     C                   Z-ADD     0             S1OPZ
285800040308     C*
285900040308     C                   UPDATE    C086S1
286000040308     C                   MOVE      *OFF          *IN32
286100040308     C*
286200040308     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
286300040308     C     $ESCI         IFEQ      *OFF
286400951109     C                   READC     C086S1                                 21
286500940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
286600040308     C     *IN21         IFEQ      *ON
286700040308     C                   MOVE      WINZS1        $INZS1
286800050608D1830C                   MOVE      *off          WINZS1
286900050608  "  C* se è avvenuta almeno una modifica
287000050608D1830C                   IF        $INZS1 = *on
287100040308     C* calcolo pagina a cui deve posizionarsi
287200040308     C     WSFL          IFGT      0
287300040308     C                   EXSR      CLCPAG
287400040308     C                   ELSE
287500040308     C                   Z-ADD     SFLPAG        WPAG
287600040308     C                   ENDIF
287700040308     C                   ENDIF
287800040308     C                   ENDIF
287900050608D1830C                   ENDIF
288000040308     C*
288100040308     C                   ENDDO
288200040308     C*
288300040308     C                   ENDIF
288400040308     C*
288500040308     C                   ENDSR
288600040308     C/EJECT
288700940127     C************************************************************
288800940131     C* CONTROLLO CAMPI I/O RIGA LISTA
288900940127     C************************************************************
289000940131     C     RECS1         BEGSR
289100940131     C*
289200940201     C* reset indicatori DSPATR
289300940201     C                   MOVE      *ALL'0'       IN4049           10
289400940201     C                   MOVEA     IN4049        *IN(40)
289500940201     C*
289600940131     C* Controllo se Utente Autorizzato alla scelta
289700940202     C     S1OPZ         IFNE      0
289800940131     C                   Z-ADD     1             X
289900940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
290000940211     C     *IN21         IFEQ      *OFF
290100940223     C     $FL1(X)       OREQ      'N'
290200940131     C                   MOVE      *ON           $ESCI
290300940201     C                   SETON                                        504099
290400940131     C                   ENDIF
290500940202     C                   ENDIF
290600950706     C*
290700990804C1213C* Controllo se Utente Autorizzato al contenzioso
290800990804  "  C* deve esserlo per l'immissione o per la manutenzione o per la
290900990804  "  C* visualizzazione
291000990907  "  C                   If        S1Opz = 50
291100990812  "  C* perchè l'opzione sia ammissibile, la divisa della partita deve
291200990812  "  C* essere la m.c.
291300990812  "  C                   If        S1Divisa <> XscDiv
291400990812  "  C                   SETON                                        534099
291500070709C1213C                   EndIf
291600070709C2023C* perchè l'opzione sia ammissibile, la partita deve essere in
291700070709  "  C* dare
291800070709  "  C                   If        S1Segno1 = 'A'
291900070709  "  C                   SETON                                        554099
292000070709C2023C                   EndIf
292100070709C1213C                   EndIf
292200020118C1213C*
292300020118C1530C*
292400020118  "  C* controllo che l'opzione 80=visione dei documenti su Laguna sia
292500020118  "  C* data solo sulle registrazioni che ne hanno
292600020118  "  C     S1OPZ         IFEQ      80
292700020118  "  C     H1IdLgn       ANDEQ     0
292800020118  "  C                   SETON                                        504099
292900020118  "  C                   ENDIF
293000021216C1530C*
293100940131     C                   ENDSR
293200941118     C/EJECT
293300941118     C************************************************************
293400941118     C* GESTIONE OPZIONI LISTA
293500941118     C************************************************************
293600941118     C     OPZS1         BEGSR
293700950301     C*
293800950301     C                   Z-ADD     RIRI          CURR
293900950301     C                   Z-ADD     COCO          CURC
294000941118     C*
294100010911C1511 *
294200010911  "  C* memorizzo che devo riemettere la wdw alla prossima eventuale
294300010911  "  C* richiesta di 46 Coversione divisa di partita in m.c.
294400010911  "  C* in quanto è stata richiesta  un'altra opzione
294500010911  "  C                   if        S1Opz <> 46
294600010911  "  C                   Eval      OpenWdw46    = *Off
294700010911  "  C                   endif
294800010911C1511 *
294900941027     C                   SELECT
295000940225     C*
295100950620     C* se opz=Selezione
295200950620     C* passo la chiave al pgm chiamante
295300940225     C     S1OPZ         WHENEQ    1
295400960327     C     S1OPZ         oreq      10
295500960123     C* passare nei campi appositi della DS NDC086DS il valore
295600950301     C* della chiave della VL univoca (se c'è)
295700040308     C                   MOVE      C1KCC         KCC086
295800040308     C                   MOVE      C1KSC         KSC086
295900951109     C                   CALLb     'XDT4000'
296000951109     C     dtpar086      PARM                    XDTAMG
296100951109     C                   PARM      s1dtpar       XDTGMA
296200951109     C                   PARM      4             XDTSTA
296300951108     C                   MOVE      S1NRPAR       NRPAR086
296400951108     C                   MOVE      S1SERPAR      SERPAR086
296500960123     c                   eval      ret086 = '3'
296600960123     c                   eval      dtsca086 = datmin
296700960318     C*
296800960318     C* Imposto la ds multipla se richiesto
296900960318     C                   If        multip086 = *on  and parms >=5
297000960318     C                   ADD       1             IndPar
297100960318     c                   if        indPar<=50
297200960318     c     indpar        occur     sKpartite
297300960318     c                   z-add     s1nrpar       SKNRpar
297400960318     c                   move      s1serpar      SKSerPar
297500960318     C                   CALLb     'XDT4000'
297600960318     C     skdtpar       PARM                    XDTAMG
297700960318     C                   PARM      s1dtpar       XDTGMA
297800960318     C                   PARM      4             XDTSTA
297900960327     c* se scelta con importo
298000960327     C                   if        S1OPZ = 10
298100960318     c                   z-ADD     H1saldodiv    skImportD
298200070202C1973c                   z-ADD     H1saldomc     skImporto
298300990115     c                   move      h1segnoD      sksegnoD
298400070202C1973c                   move      S1Divisa1     skDivisa
298500960327     c                   endif
298600960319     c                   move      datmin        skdtSca
298700960318     c                   endif
298800960318     C                   MOVE      *ON           $FINE
298900960318     c                   else
299000960318     C                   MOVE      *ON           $ESCI
299100960318     C                   MOVE      *ON           $FINE
299200960318     c                   endif
299300940909     C*
299400940909     C                   MOVE      *HIVAL        $LASTV
299500951114     C*
299600951114     C* se opz=Movimenti
299700951114     C* richiamo il pgm di gestione dettaglio partita
299800951114     C     S1OPZ         WHENEQ    11
299900951114     C                   RESET                   NDC087DS
300000951114     c*
300100960123     C* Se richiamato in ricerca, passo stessa opzione
300200960123     C                   MOVE      OPZ086        OPZ087
300300951124     c* Campi provenienti dal ndc086ds
300400951124     C                   move      ctb086        ctb087
300500951124     C                   move      s1unita       unita087
300600951124     C                   move      kcc086        kcc087
300700951124     C                   move      ksc086        ksc087
300800951124     C                   move      movdef086     movdef087
300900951124     C                   move      movprov086    movprov087
301000951124     C                   move      movges086     movges087
301100951124     C                   move      dtregd086     dtregd087
301200951124     C                   move      dtregp086     dtregp087
301300951124     C                   move      dtregg086     dtregg087
301400951124     C                   move      divvis086     divvis087
301500951124     C                   eval      DESCR087   = DESCR086
301600951124     C                   eval      DESAGG087  = desagg086
301700951124     C                   eval      PERCIPI087 = percipi086
301800951124     C                   eval      SNATURA087 = snatura086
301900951124     C                   eval      analit087  = analit086
302000951124     C* Campi specifici della scelta
302100951114     C                   CALLb     'XDT4000'
302200951114     C     dtpar087      PARM                    XDTAMG
302300951114     C                   PARM      s1dtpar       XDTGMA
302400951114     C                   PARM      4             XDTSTA
302500951114     C                   move      s1nrpar       nrpar087
302600951114     C                   move      s1serpar      serpar087
302700951114     C                   move      scadutoH      Scaduto087
302800951114     C                   move      AscadereH     Ascader087
302900951114     C                   move      Rischioh      rischio087
303000951114     C                   move      POrtafoglh    Portafo087
303100011213D1016x***                if        s1bkamm <> '+'
303200951114     C                   move      s1bkamm       bkamm087
303300011213d1016x***                endif
303400951114     C                   move      insrith       InsRit087
303500951114     C                   move      commentiH     comment087
303600951114     C                   IF        Bonificih = *ON  or sollecitiH = *ON
303700951115     C                   eval      bonsol087 = *on
303800951114     C                   Endif
303900951115     C* passo i campi saldo1 e 2 invece dei campi nascosti perchè questi
304000951115     C* del subfile sono allineati fra loro se la divisa è uguale
304100951115     C                   IF        Savdivvis = '1'
304200951115     C                   move      s1saldo1      saldod087
304300951115     C                   move      s1saldo2      saldoM087
304400951115     C                   else
304500951115     C                   move      s1saldo2      saldod087
304600951115     C                   move      s1saldo1      saldoM087
304700951115     C                   endif
304800951114     C                   move      h1segnod      segnod087
304900951114     C                   move      h1segnoM      segnoM087
305000951114     C                   move      s1divisa      divisa087
305100951114     C                   move      s1monconto    moncon087
305200951114     C                   move      s1kscage      kscage087
305300960319     c                   move      multip086     multip087
305400051215C1892c                   move      DataSF086     DataSF087
305500061123C1979c                   z-add     h1sys         Sys087
305600061123C1979c                   z-add     h1nrasint     NrasInt087
305700951114     C                   CALL      'NDC087R'
305800951114     C                   PARM                    KPJBA
305900951114     c                   PARM                    ndc087ds
306000951114     C                   PARM                    SOC001
306100951114     C                   PARM                    AUTDS
306200960319     C                   PARM                    skpartite
306300951114     C*
306400951114     C* ritorno da PGM movimenti
306500951114     C                   EXSR      RTN087
306600951114     C
306700940831     C*
306800950620     C* se opz=Commenti
306900950620     C* richiamo il pgm di gestione
307000940831     C     S1OPZ         WHENEQ    14
307100950606     C                   RESET                   ANA725DS
307200941214     C                   MOVE      '02'          OPZ725
307300040308     C                   MOVE      'PPA'         TPA725
307400040308     C* inserire chiavi commenti
307500040308     C                   MOVEL     CTB086        KY1725
307600040308     C                   MOVEL     C1KCC         KY2725
307700040308     C                   MOVE      C1KSC         KY2725
307800040308     C                   MOVEL     S1DTPAR       KY3725
307900040308     C                   MOVEL     s1nrpar       KY4725
308000040308     C                   MOVE      s1serpar      KY4725
308100040308     C                   MOVE      *BLANK        KPJBU
308200040308     C                   MOVEL     ANA725DS      KPJBU
308300040308     C                   CALL      'ANA725R'
308400040308     C                   PARM                    KPJBA
308500040308     C                   MOVEL     KPJBU         ANA725DS
308600040308     C*
308700040308     C* ritorno da PGM commenti
308800040308     C                   EXSR      RTN725
308900040308     C*
309000940831     C                   MOVE      *HIVAL        $LASTV
309100951109     C*
309200951109     C* se opz=Particolarità partite
309300951109     C* richiamo il pgm di gestione
309400951109     C     S1OPZ         WHENEQ    20
309500961121     c                   reset                   x48chPgmds
309600961121     C                   eval      x48pgm = 'NDC060R'
309700961121     c                   eval      kpjbu = x48chpgmds
309800961121     c                   call      'X48CHPGM'
309900961121     c                   parm                    kpjba
310000961121     c                   movel     kpjbu         x48chpgmds
310100961121     c                   if        x48errore = *off
310200951109     C                   RESET                   NDC060DS
310300951113     c*
310400951114     C                   MOVE      '02'          OPZ060
310500951109     C                   MOVE      Unita086      unita060
310600951109     C                   MOVE      Ctb086        ctb060
310700951109     C                   MOVE      C1KCC         KCC060
310800951109     C                   MOVE      C1KSC         KSC060
310900951109     C                   CALLb     'XDT4000'
311000951109     C     dtpar060      PARM                    XDTAMG
311100951109     C                   PARM      s1dtpar       XDTGMA
311200951109     C                   PARM      4             XDTSTA
311300951109     C                   move      s1nrpar       nrpar060
311400951109     C                   move      s1serpar      serpar060
311500960313     C*imposto chiave unique per la lista
311600040308     C                   MOVEL     ctb060        UQ1060
311700040308     C                   MOVEL     kcc060        UQ2060
311800040308     C                   MOVEL     ksc060        UQ3060
311900960313     C                   move      dtpar060      UQ4060
312000960313     C                   move      nrpar060      UQ5060
312100960313     C                   move      serpar060     UQ6060
312200951109     C                   MOVE      *BLANK        KPJBU
312300951109     C                   MOVEL     NDC060DS      KPJBU
312400951109     C                   CALL      'NDC060R'
312500951109     C                   PARM                    KPJBA
312600951109     C                   MOVEL     KPJBU         NDC060DS
312700951109     C*
312800951109     C* ritorno da PGM commenti
312900951109     C                   EXSR      RTN060
313000961121     C                   endif
313100951109     C*
313200951109     C                   MOVE      *HIVAL        $LASTV
313300950707     C*
313400950707     C* se opz=MIX
313500950707     C* richiamo il pgm di gestione
313600950707     C     S1OPZ         WHENEQ    32
313700961121     c                   reset                   x48chPgmds
313800961121     C                   eval      x48pgm = 'NDC065R'
313900961121     c                   eval      kpjbu = x48chpgmds
314000961121     c                   call      'X48CHPGM'
314100961121     c                   parm                    kpjba
314200961121     c                   movel     kpjbu         x48chpgmds
314300961121     c                   if        x48errore = *off
314400950707     C                   RESET                   NDC065DS
314500950707     C                   MOVE      '02'          OPZ065
314600951109     C                   MOVE      Unita086      unita065
314700951109     C                   MOVE      Ctb086        ctb065
314800950707     C                   MOVE      C1KCC         KCC065
314900950707     C                   MOVE      C1KSC         KSC065
315000951109     C                   CALLb     'XDT4000'
315100951109     C     dtpar065      PARM                    XDTAMG
315200951109     C                   PARM      s1dtpar       XDTGMA
315300951109     C                   PARM      4             XDTSTA
315400950707     C                   move      s1nrpar       nrpar065
315500950707     C                   move      s1serpar      serpar065
315600950718     C*imposto chiave unique per la lista
315700040308     C                   MOVEL     ctb065        UQ1065
315800040308     C                   MOVEL     kcc065        UQ2065
315900040308     C                   MOVEL     ksc065        UQ3065
316000950906     C                   move      dtpar065      UQ4065
316100950906     C                   move      nrpar065      UQ5065
316200950906     C                   move      serpar065     UQ6065
316300950707     C                   MOVE      *BLANK        KPJBU
316400950707     C                   MOVEL     NDC065DS      KPJBU
316500950707     C                   CALL      'NDC065R'
316600950707     C                   PARM                    KPJBA
316700950707     C                   MOVEL     KPJBU         NDC065DS
316800950707     C*
316900950707     C* ritorno da PGM commenti
317000950707     C                   EXSR      RTN065
317100961121     c                   endif
317200950707     C*
317300950707     C                   MOVE      *HIVAL        $LASTV
317400981006     C*
317500981006     C* se opz=Rate/scadenze
317600981006     C* richiamo il pgm di gestione
317700981006     C     S1OPZ         WHENEQ    33
317800981006     c                   reset                   x48chPgmds
317900981006     C                   eval      x48pgm = 'NDC063R'
318000981006     c                   eval      kpjbu = x48chpgmds
318100981006     c                   call      'X48CHPGM'
318200981006     c                   parm                    kpjba
318300981006     c                   movel     kpjbu         x48chpgmds
318400981006     c                   if        x48errore = *off
318500981006     C                   RESET                   NDC063DS
318600040308     C     OPZ086        IFEQ      '04'
318700040308     C     OPZ086        OREQ      '05'
318800040308     C* oppure se l'utente è abilitato per la funzione specifica di
318900040308     C* questa anagrafica solo alla visualizzazione
319000981006     C                   MOVE      '05'          OPZ063
319100981006     C                   ELSE
319200981006     C                   MOVE      '02'          OPZ063
319300981006     C                   ENDIF
319400981006     C                   MOVE      Ctb086        ctb063
319500981006     C                   MOVE      C1KCC         KCC063
319600981006     C                   MOVE      C1KSC         KSC063
319700981006     C                   CALLb     'XDT4000'
319800981006     C     dtpar063      PARM                    XDTAMG
319900981006     C                   PARM      s1dtpar       XDTGMA
320000981006     C                   PARM      4             XDTSTA
320100981006     C                   move      s1nrpar       nrpar063
320200981006     C                   move      s1serpar      serpar063
320300981006     C                   MOVE      *BLANK        KPJBU
320400981006     C                   MOVEL     NDC063DS      KPJBU
320500981006     C                   CALL      'NDC063R'
320600981006     C                   PARM                    KPJBA
320700981006     C                   MOVEL     KPJBU         NDC063DS
320800981006     C*
320900981006     C* ritorno da PGM commenti
321000981006     C                   EXSR      RTN063
321100981006     c                   endif
321200981006     C*
321300981006     C                   MOVE      *HIVAL        $LASTV
321301110414C2185C* se opz=Estensione nominativi partite
321302110414  "  C* richiamo il pgm di gestione
321303110414  "  C     S1OPZ         WHENEQ    34
321304110414  "  C* Controllo se ci sono record su NDCPA00F
321305110414  "  C                   MOVE      H1SYS         PPASYS
321306110414  "  C                   MOVE      H1NRASINT     PPANRASINT
321307110414  "  C     K02ppa01      SETLL     NDCpa01L
321309110414  "  C                   RESET                   NDC066DS1
321317110414  "  C* Immissione
321319110414  "  c                   if        not %equal(ndCpa01L)
321320110414  "  C                   MOVE      '01'          OPZ0661
321321110414  "  C                   Else
321322110414  "  c* modifica o altro
321324110414  "  C     OPZ086        IfEq      *BLANK
321325110414  "  C                   MOVE      '02'          OPZ0661
321326110414  "  C                   Else
321327110414  "  C                   MOVE      OPZ086        OPZ0661
321328110414  "  C                   EndIf
321329110414  "  C                   EndIf
321330110414  "  C*
321331110414  "  C                   MOVE      C1KCC         KCC0661
321332110414  "  C                   MOVE      C1KSC         KSC0661
321333110414  "  C                   MOVEL     C1DESCR       DESBRE0661
321334110414  "  C     S1DtPar       IFGT      0
321336110414  "  c     *jobrun       move      s1dtpar       $dataiso
321337110414  "  c                   move      $dataiso      dtpar0661
321338110414  "  C                   ELSE
321339110414  "  C                   MOVE      *loval        DtPar0661
321340110414  "  C                   ENDIF
321341110414  "  C                   move      s1nrpar       nrpar0661
321342110414  "  C                   move      s1serpar      serpar0661
321343110414  "  C*imposto chiave unique per la lista
321344110414  "  C                   MOVE      H1SYS         SYS0661
321345110414  "  C                   MOVE      H1NRASINT     NRASIN0661
321346110414  "  C*
321347110414  "  C                   MOVE      *BLANK        KPJBU
321348110414  "  C                   MOVEL     NDC066DS1     KPJBU
321349110414  "  C                   CALL      'NDC066R1'
321350110414  "  C                   PARM                    KPJBA
321351110414  "  C                   MOVEL     KPJBU         NDC066DS1
321352110414  "  C*
321353110414  "  C* ritorno da PGM
321354110414  "  C                   EXSR      RTN0661
321355110414  "  C*
321356110414C2185C                   MOVE      *HIVAL        $LASTV
321400951204     C*
321500951204     C* se opz=Ritenute
321600951204     C* richiamo il pgm di gestione
321700951204     C     S1OPZ         WHENEQ    40
321800951204     C                   RESET                   NDCR05DS
321900951204     C                   eval      opzr05     = '02'
322000951204     C                   eval      kccR05     = kcc086
322100951204     C                   eval      kscR05     = ksc086
322200951204     C                   CALLb     'XDT4000'
322300951204     C     ddcr05        PARM                    XDTAMG
322400951204     C                   PARM      s1dtdoc       XDTGMA
322500951204     C                   PARM      4             XDTSTA
322600951204     C                   move      s1nrdoc       ndcr05
322700951204     C                   move      s1serdoc      serdocr05
322800951204     C                   clear                   kpjbu
322900951204     C                   movel     ndcR05ds      kpjbu
323000951204     C                   CALL      'NDCR05R'
323100951204     C                   PARM                    KPJBA
323200951204     C                   movel     kpjbu         ndcR05ds
323300951204     C*
323400951204     c* F12
323500040308     C     RETR05        ifeq      '2'
323600951204     C                   MOVE      *ON           $ESCI
323700951204     C                   endif
323800951204     C*  F3
323900040308     C     RETR05        IFEQ      '1'
324000951204     C                   MOVE      *ON           $ESCI
324100951204     C                   MOVE      *ON           $FINE
324200951204     C                   MOVE      '1'           RET086
324300951204     C                   endif
324400951204     C*
324500951204     C                   MOVE      *HIVAL        $LASTV
324600951123     C*
324700951204     C*
324800951204     C* se opz=Bonifici
324900951204     C* richiamo il pgm di gestione
325000951204     C     S1OPZ         WHENEQ    41
325100981112      * NDCH65R1 è stato sostituito con il nuovo NDCB50R...
325200981112      * ...perciò asterisco le specifiche che seguono
325300010112      * chiamo NDCB50R al posto del vecchio NDCH65R1                   )
325400981112     c                   reset                   x48chPgmds
325500981112     C                   eval      x48pgm = 'NDCB50R'
325600981112     c                   eval      kpjbu = x48chpgmds
325700981112     c                   call      'X48CHPGM'
325800981112     c                   parm                    kpjba
325900981112     c                   movel     kpjbu         x48chpgmds
326000981112     c                   if        x48errore = *off
326100981112     c                   RESET                   NDCB50DS
326200981230     C                   Eval      OpzB50      = '05'
326300981230     c                   eval      PgmCallB50  = DsPgm
326400981230     c                   eval      SocietaB50  = Societa086
326500981230     c                   eval      UnitaB50    = Unita086
326600981112     C                   Eval      CtbB50      = Ctb086
326700981112     C                   Eval      KccB50      = Kcc086
326800981112     C                   Eval      KscB50      = Ksc086
326900981130     C                   Move      *Loval        DtParB50
327000981130     C                   Move      *Loval        $DataISO
327100981130     C                   Eval      $Data60     = 0
327200981130     C     S1DtPar       IfNe      0
327300981130     C                   Eval      $Data60     = S1DtPar
327400040308C1309X******Dmy          Move      $Data60       $DataISO
327500000118C1309C     *JobRun       Move      $Data60       $DataISO
327600981130     C                   Move      $DataISO      DtParB50
327700981130     C                   EndIf
327800981130     C                   Eval      NrParB50    = S1NrPar
327900981130     C                   Eval      SerParB50   = S1SerPar
328000981112     C                   Clear                   Kpjbu
328100981112     C                   Movel     NDCB50DS      Kpjbu
328200981112     C                   CALL      'NDCB50R'
328300981112     C                   PARM                    KPJBA
328400981112     C                   Movel     Kpjbu         NDCB50DS
328500981112     c* F12
328600040308     C     RETB50        ifeq      '2'
328700981112     C                   MOVE      *ON           $ESCI
328800981112     C                   endif
328900981112     C*  F3
329000040308     C     RETB50        IFEQ      '1'
329100981112     C                   MOVE      *ON           $ESCI
329200981112     C                   MOVE      *ON           $FINE
329300981112     C                   MOVE      '1'           RET086
329400981112     C                   endif
329500981112     C                   endif
329600981112      *
329700981112     C                   MOVE      *HIVAL        $LASTV
329800981112      *
329900951123     C* se opz=Solleciti
330000951123     C* richiamo il pgm di gestione
330100951123     C     S1OPZ         WHENEQ    42
330200961121     c                   reset                   x48chPgmds
330300961121     C                   eval      x48pgm = 'NDCL29R'
330400961121     c                   eval      kpjbu = x48chpgmds
330500961121     c                   call      'X48CHPGM'
330600961121     c                   parm                    kpjba
330700961121     c                   movel     kpjbu         x48chpgmds
330800961121     c                   if        x48errore = *off
330900951123     C                   RESET                   NDCL29DS
331000951123     C                   eval      L29societa = xscsoc
331100040308D0536X*****              eval      L29unita   = unita086
331200000419D0536C* i programmi dei solleciti devono ricervere per forza l'unità
331300000419d0536C* quindi non è possibile passare quella della parzializzazione
331400000419D0536C                   eval      L29unita   = S1Unita
331500951123     C                   eval      L29ctb     = ctb086
331600951123     C                   eval      L29kcc     = kcc086
331700951123     C                   eval      L29ksc     = ksc086
331800951124     C                   eval      l29INIZ    = Wl29INIZ
331900951123     C                   clear                   kpjbu
332000951123     C                   movel     ndcl29ds      kpjbu
332100951123     C                   CALL      'NDCL29R'
332200951123     C                   PARM                    KPJBA
332300951124     C                   movel     kpjbu         ndcl29ds
332400951124     C*
332500951124     C* metto 1 in  wl29iniz per non far fare più la close
332600951124     C                   eval      wL29iniz = '1'
332700951124     c* F12
332800951124    >C     L29RET        ifeq      '2'
332900951124     C                   MOVE      *ON           $ESCI
333000951124     C                   endif
333100961121     C                   endif
333200070612C2028C     L29OPR        ifeq      '1'
333300070612  "  C                   MOVE      *ON           $INZS1
333400070615  "  C                   MOVE      *ON           $ESCI
333500070612C2028C                   endif
333600951123     C*
333700951123     C                   MOVE      *HIVAL        $LASTV
333800010911C1511C*
333900010911  "  C* se opz=46 Coversione divisa di partita in m.c.
334000010911  "  C     S1OPZ         WHENEQ    46
334100010911C1511C                   exsr      Opz46
334200940225     C*
334300090216C2119C* se opz=Contenzioso
334400090216  "  C     S1OPZ         WHENEQ    50
334500090216  "  c* e se il cliente NON è già a contenzioso
334600090216  "  c     ContenzCli    andne     *on
334700090216  "  c* OPPURE
334800090216  "  C     S1OPZ         oreq      50
334900090216  "  c* e se il cliente E'già a contenzioso
335000090216  "  c     ContenzCli    andeq     *on
335100090216  "  c* la partita NON è a contenzioso
335200090216  "  c     H1contenz     andne     *on
335300090216  "  c* e il suo kcc è <> da  quello a contenzioso
335400090216  "  c* per cui  chiamo NDCL28R per effettuare giroconto partita
335500090216  "  c     Contenzkcc    andne     kcc086
335600090216  "  c*
335700090216  "  c                   exsr      opz50_28
335800090216C2119C*
335900990803C1213C* se opz=Contenzioso
336000090216c1213C     S1OPZ         WHENEQ    50
336100090216c2119c* e il  cliente è già a contenzioso
336200090216c2119c     ContenzCli    andeq     *on
336300090216c1213c*
336400990803  "  C                   RESET                   NDCL70DS
336500990803  "  c* Campi provenienti dal ndc086ds
336600990803  "  C                   Move      XscSoc        SocietaL70
336700990804  "  C                   Move      Unita086      UnitaL70
336800090216c1213C                   Move      Ctb086        CtbL70
336900090216c2119x****************** Move      Kcc086        KccL70
337000090216c2119c* visto che è a contenzioso, passo al pgm kcc contenzioso
337100090216c2119C                   Move      ContenzKcc    KccL70
337200090216c1213C                   Move      Ksc086        KscL70
337300990803  "  C                   Eval      DescrL70   = Descr086
337400990803  "  C                   Eval      DesAggL70  = DesAgg086
337500990803  "  C* Campi specifici della scelta
337600990803  "  C     *JobRun       Move      S1DtPar       $DataISO
337700990803  "  C                   Move      $DataISo      DtParL70
337800990803  "  C                   Z-Add     S1NrPar       NrParL70
337900990803  "  C                   Move      S1SerPar      SerParL70
338000990803  "  c                   If        S1BkAmm <> '+'
338100990803  "  C                   Move      S1BkAmm       BkAmmL70
338200990803  "  c                   EndIf
338300990803  "  C                   Z-Add     H1SaldoDiv    SaldoL70
338400990803  "  C                   Move      S1Segno1      SegnoL70
338500990803  "  C                   Move      S1Divisa1     DivisaL70
338600030416C1213C                   Move      s1kscage      AgenteL70
338700030416D1326x*****              Move      PortafoglH    PortafL70
338800030416D1326c                   Move      InsRitH       PortafL70
338900030416C1213C                   MoveL     NdcL70Ds      Kpjbu
339000990803  "  C                   CALL      'NDCL70R'
339100990803  "  C                   PARM                    KPJBA
339200990803  "  C                   PARM                    SOC001
339300990803  "  C                   PARM                    AUTDS
339400990803  "  C*
339500990810  "  C                   MoveL     Kpjbu         NdcL70DS
339600990803  "  C* ritorno da PGM contenzioso
339700990803  "  C                   EXSR      RTNL70
339800990803C1213C*
339900010112     C*
340000031119C1460C* se opz=ordini
340100010112  "  C* richiamo il pgm di interfaccia richiamo ordini cli/for
340200031119C1460C     S1OPZ         WHENEQ    60
340300031119D1498X*                  eval      Societa    = xscsoc
340400031119  "  X*                  eval      CliFor     = Snatura086
340500031119  "  X*                  eval      Codice     = ksc086
340600031119  "  X* se clienti
340700031119  "  X*                  if        snatura086 = 'C' or
340800031119  "  X* o se fornitori di Tile
340900031119  "  X*                            (snatura086 = 'F' and xscgest = '2')
341000031119  "  X*                  move      '5'           Modalita
341100031119  "  X* passo dati documento
341200031119  "  X*                  CALLb     'XDT4000'
341300031119  "  X*    Data          PARM                    XDTAMG
341400031119  "  X*                  PARM      s1dtdoc       XDTGMA
341500031119  "  X*                  PARM      4             XDTSTA
341600031119  "  X*                  move      s1nrdoc       Numero
341700031119  "  X*                  move      s1serdoc      Serie
341800031119  "  X*                  else
341900031119  "  X*                  move      '0'           Modalita
342000031119  "  X* se fornitori distribuzione
342100031119  "  x*                  eval      RegSys = H1sys
342200031118  "  x*                  eval      RegNrasReg = H1NrasGen
342300031118  "  x*    k02Reg01      chain     NdREg01l                           21
342400031118  "  X*                  if        *in21 = *off
342500031118  "  x*                  eval      Serie = RegSerReg
342600031118  "  x*                  eval      Numero = RegNrReg
342700031118  "  x*                  eval      Data   = RegDtReg
342800031118  "  x*                  endif
342900031119  "  X*                  endif
343000031119  "  X* codice di fatturazione
343100031119  "  X*                  move      *on           CodFatt
343200031119  "  X*
343300031119  "  X*                  CALL      'ANA396R'
343400031119  "  X*                  PARM                    KPJBA
343500031119  "  X*                  PARM                    Societa           3
343600031119  "  X*                  PARM                    Codice            8
343700031119  "  X*                  PARM                    CliFor            1
343800031119  "  X*                  PARM                    CodFatt           1
343900031119  "  X*                  PARM                    Modalita          1
344000031119  "  X*                  PARM                    Data             10
344100031119  "  X*                  PARM                    Numero            9 0
344200031119  "  X*                  PARM                    Serie             4
344300031119  "  X*                  PARM                    Ritorno           1
344400031119  "  c*
344500031119  "  C                   RESET                   ana396ds
344600031119  "  c                   eval      Societa396 = xscsoc
344700031119  "  c                   eval      kcc396     = kcc086
344800031119  "  c                   eval      ksc396     = ksc086
344900031119  "  c                   eval      sys396     = h1sys
345000031119  "  c                   eval      nrasint396 = h1nrasint
345100040308  "  c                   eval      CodFatt396=*on
345200040308  "  c                   eval      Modalit396='2'
345300031119  "  c                   eval      kpjbu=ana396ds
345400031119  "  c                   CALL      'ANA396R'
345500031119  "  C                   parm                    kpjba
345600031119  "  c                   eval      ana396ds=kpjbu
345700010112  "  c* F12
345800031119D1498C     Ritorno396    ifeq      '2'
345900031119C1460X***  Ritorno       ifeq      '2'
346000010112  "  C                   MOVE      *ON           $ESCI
346100010112  "  C                   endif
346200031119  "  C*  F3
346300031119C1460X***  Ritorno       IFEQ      '1'
346400031119D1498C     Ritorno396    IFEQ      '1'
346500031119C1460C                   MOVE      *ON           $ESCI
346600010112  "  C                   MOVE      *ON           $FINE
346700010112  "  C                   MOVE      '1'           RET086
346800010112  "  C                   endif
346900010112  "  C*
347000010119C1460C                   MOVE      *HIVAL        $LASTV
347100020118C1530C*
347200020118  "  C* Richiamo Laguna per visualizzazione documento
347300020118  "  C     S1OPZ         WHENEQ    80
347400020118  "  C                   MOVEL     H1ClassLg     Parm670(1)
347500021120C1530C                   MOVEL     H1IdLgn       Parm670(2)
347600021120C1605C                   MOVEL     knsif         Parm670(3)
347700071023C2055x***                CALL      'ANA670R'
347800071023C2055C                   CALL      'PJXX038R'
347900071023C1530C                   PARM                    xscsoc
348000020118  "  C                   PARM      'VIEW'        Cmd670
348100020118  "  C                   PARM                    Parm670
348200020118  "  C                   PARM                    Err670
348300020118  "  C                   PARM                    Ret670
348400020118  "  C* ritorno da PGM gestione
348500020118  "  C                   EXSR      RTN670
348600020118C1530C*
348700950620     C* se altre opzioni
348800950620     C* richiamo pgm gestione anagrafica
348900950907     C* anche per opzione blocco/sblocco, dove eseguirò solo aggiornamento
349000950907     C* archivio
349100941027     C                   OTHER
349200940131     C*
349300940506     C                   MOVE      *HIVAL        $LASTV
349400950907     C*
349500950907     C*
349600941027     C                   ENDSL
349700940225     C*
349800940131     C                   ENDSR
349900940223     C/EJECT
350000090216c2119C************************************************************
350100090216  "  C* opzione 50 con chiamata a NDCL28R per creazione posizione
350200090216  "  c* contabile o giroconto partita
350300090216  "  C************************************************************
350400090216c2119C     Opz50_28      BEGSR
350500090216     c*
350600090216     C                   RESET                   NDCL28DS
350700090216     C                   Move      XscSoc        SocietaL28
350800090216     c                   move      s1unita       UnitaL28
350900090216     c                   move      '02'          opzl28
351000090216     C                   Move      c1kcc         KccL28
351100090216     C                   Move      Ksc086        KscL28
351200090216     C     *JOBRUN       MOVE      S1dtpar       $dataiso
351300090216     c                   move      $dataiso      dtparl28
351400090216     c                   z-add     s1nrpar       nrparl28
351500090216     c                   move      s1serpar      serparL28
351600090216     c                   z-add     h1saldomc     importoL28
351700090216     c                   move      wrkwdwl28     wdwl28
351800090216     C                   MoveL     NdcL28Ds      Kpjbu
351900090216     C                   CALL      'NDCL28R'
352000090216     C                   PARM                    KPJBA
352100090216     C                   PARM                    SOC001
352200090216     C                   PARM                    AUTDS
352300090216     C*
352400090216     C                   MoveL     Kpjbu         NdcL28DS
352500090216     C* ritorno da PGM contenzioso
352600090216     C                   EXSR      RTNL28
352700090216     c*
352800090216c2119C                   ENDSR
352900090216C2119C************************************************************
353000090216  "  C* GESTIONE RITORNO DA PGM Creaz. conto e giroconto partita
353100090216  "  C************************************************************
353200090216  "  C     RTNL28        BEGSR
353300090216  "  C*
353400090216  "  C* modo di ritorno
353500090216  "  C*
353600090216  "  C                   SELECT
353700090216  "  C* questi modi di utilizzo dei valori di ritorno dal
353800090216  "  C* pgm di manutenzione rcd di anagrafica sono delle
353900090216  "  C* proposte, normalmente sempre valide, ma modificabili
354000090216  "  C* per situazioni particolari
354100090216  "  C* 1 = F3
354200090216  "  C     RETL28        WHENEQ    '1'
354300090216  "  C                   MOVE      *ON           $ESCI
354400090216  "  C                   MOVE      *ON           $FINE
354500090216  "  C                   MOVE      '1'           RET086
354600090216  "  C* 2 = F12
354700090216  "  C     RETL28        WHENEQ    '2'
354800090216  "  C     OPRL28        ANDNE     '1'
354900090216  "  C                   MOVE      *ON           $ESCI
355000090216  "  C*
355100090216  "  C                   ENDSL
355200090216  "  C*
355300090216  "  C* ricarico subfile se eseguito aggiornamento
355400090216  "  C     OPRL28        ifeq      '1'
355500090216  "  C                   MOVE      oprl28        Wopr
355600090216  "  c                   move      *on           $inzs1
355700090216  "  c                   move      *on           Winzs1
355800090216  "  c* richiamo immisione in contenzioso (con vecchio o nuovo kcc)
355900090216  "  C                   RESET                   NDCL70DS
356000090216  "  C                   Move      XscSoc        SocietaL70
356100090216  "  C                   Move      Unita086      UnitaL70
356200090216  "  C                   Move      ctb086        CtbL70
356300090216  "  C                   Move      KccL28        KccL70
356400090216  "  C                   Move      Ksc086        KscL70
356500090216  "  C                   Eval      DescrL70   = Descr086
356600090216  "  C                   Eval      DesAggL70  = DesAgg086
356700090216  "  C* Campi specifici della scelta
356800090216  "  C     *JobRun       Move      S1DtPar       $dataiso
356900090216  "  C                   Move      $DataISo      DtParL70
357000090216  "  C                   Z-Add     S1NrPar       NrParL70
357100090216  "  C                   Move      S1SerPar      SerParL70
357200090216  "  c                   If        S1BkAmm <> '+'
357300090216  "  C                   Move      S1BkAmm       BkAmmL70
357400090216  "  c                   EndIf
357500090216  "  C                   Z-Add     H1SaldoDiv    SaldoL70
357600090216  "  C                   Move      S1Segno1      SegnoL70
357700090216  "  C                   Move      S1Divisa1     DivisaL70
357800090216  "  C                   Move      s1kscage      AgenteL70
357900090216  "  c                   Move      InsRitH       PortafL70
358000090216  "  c* se il capoconto è diverso da quello ricevuto, significa che
358100090216  "  c* è stata girocontata la partita, quindi non ammessi tasti F12/F3
358200090216  "  c                   if        Kccl70<>kcc086
358300090216  "  c                   eval      NoRetL70 = *on
358400090216  "  C                   endif
358500090216  "  C                   MoveL     NdcL70Ds      Kpjbu
358600090216  "  C                   CALL      'NDCL70R'
358700090216  "  C                   PARM                    KPJBA
358800090216  "  C                   PARM                    SOC001
358900090216  "  C                   PARM                    AUTDS
359000090216  "  C                   MoveL     Kpjbu         NdcL70DS
359100090216  "  C* ritorno da PGM contenzioso
359200090216  "  C                   EXSR      RTNL70
359300090216  "  c                   endif
359400090216  "  C*
359500090216  "  C* funzione non eseguibile per errore :
359600090216  "  C*
359700090216  "  C                   SELECT
359800090216  "  C* 1 = funzione richiamata chiusa in errore
359900090216  "  C     ERRL28        WHENEQ    '1'
360000090216  "  C                   MOVE      *ON           $ESCI
360100090216  "  C                   SETON                                        9599
360200090216  "  C* 2 = funzione richiamata senza la dovuta autorizzazione
360300090216  "  C     ERRL28        WHENEQ    '2'
360400090216  "  C                   MOVE      *ON           $ESCI
360500090216  "  C                   SETON                                        9499
360600090216  "  C*
360700090216  "  C                   ENDSL
360800090216  "  C*
360900090216  "  c                   move      *off          wrkwdwl28         1
361000090216  "  C*
361100090216  "  C                   ENDSR
361200090216C2119C/EJECT
361300010911C1511 ************************************************************
361400010911  "   * opz=46 Coversione divisa di partita in m.c.
361500010911  "   ************************************************************
361600010911  "  C     Opz46         Begsr
361700010911  "   *
361800010911  "   * stabilisco se devo lanciare l'elaborazione dalla prima
361900010911  "   * volta che emetto la finestrina
362000010911  "  C                   if        OpenWdw46   = *Off
362100010911  "  C                   eval      $Elabora46 = *Off
362200010911  "  C* gestione emissione finestra
362300010911  "  C                   EXSR      GestWdw
362400010911  "  C                   endif
362500010911  "  C*
362600010911  "  C                   if        $Elabora46 = *On
362700010911  "  C*
362800010911  "  C                   RESET                   NDC506DS
362900010911  "  C                   Move      XScSoc        Societa506
363000010911  "  C                   Move      Ctb086        Ctb506
363100010911  "  C                   Move      Kcc086        Kcc506
363200010911  "  C                   Move      Ksc086        Ksc506
363300010911  "  C     *JobRun       Move      S1DtPar       $DataISO
363400010911  "  C                   Move      $DataISo      DtPar506
363500010911  "  C                   Z-Add     S1NrPar       NrPar506
363600010911  "  C                   Move      S1SerPar      SerPar506
363700010911  "  C*
363800010911  "  C                   MOVEL     NDC506DS      KPJBU
363900010911  "  C                   CALL      'NDC506R'
364000010911  "  C                   PARM                    KPJBA
364100010911  "  C*
364200010911  "  C                   MOVEL     KPJBU         NDC506DS
364300010911  "  C*
364400010911  "  C* ritorno da PGM gestione
364500010911  "  C                   EXSR      RTN506
364600010911  "  C*
364700010911  "  C                   endif
364800010911  "  C*
364900010911  "  C     XOpz46        Endsr
365000010911  "  C************************************************************
365100010911  "  C* Emetto la finestra per avvertire che sono state richieste
365200010911  "  C* delle operazioni di conversione, e quindi si chiede di
365300010911  "  C* confermare o meno le operazioni
365400010911  "  C************************************************************
365500010911  "  C     GestWdw       BEGSR
365600010911  "  C*
365700010911  "  C* "Sono state richieste delle conversioni di divisa di partita
365800010911  "  C*  in EURO."
365900010911  "  C                   MoveL     'NDC0639'     WMSGCd1           7
366000010911  "  C                   EXSR      MSGPGM
366100010911  "  C*
366200010911  "  C*-----
366300010911  "  C* "Continua confermando l'elaborazione"
366400010911  "  C                   if        MsgRtn    =   '1'
366500010911  "  C                   eval      $Elabora46 = *On
366600010911  "  C                   endif
366700010911  "  C*
366800010911  "  C* segnalo che in questo ciclo ho aperto la window
366900010911  "  C                   Eval      OpenWdw46   = *On
367000010911  "  C*
367100010911  "  C                   ENDSR
367200010911  "  C/EJECT
367300010911  "  C******************************************************
367400010911  "  C* PGM PER GESTIONE MESSAGGI ERRORE
367500010911  "  C******************************************************
367600010911  "  C     MSGPGM        BEGSR
367700010911  "  C*
367800010911  "  C                   Clear                   XMSGDS
367900010911  "  C*
368000010911  "  C                   MOVEL     'S'           MSGRSP
368100010911  "  C                   MOVEL     '12'          MSGTPR
368200010911  "  C                   MOVEL     'S'           MSGEMV
368300010911  "  C                   Z-ADD     10            MSGRGP
368400010911  "  C                   MOVEL     'N'           MSGCNL
368500010911  "  C                   MOVEL     0             MSGMSG
368600010911  "  C                   MOVEA     STATUS        XSTS
368700010911  "  C                   MOVEL     'N'           MSGVID
368800010911  "  C                   MOVEL     'N'           MSGOPE
368900010911  "  C                   MOVEL     'N'           MSGSTP
369000010911  "  C                   MOVEL     'N'           MSGLCK
369100010911  "  C*
369200010911  "  C* reperisco i testi delle possibili risposte
369300010911  "  C                   Clear                   XMsgTxt
369400010911  "  C                   EXSR      RepRispost
369500010911  "  C*
369600010911  "  C                   CALL      'XMSGR'
369700010911  "  C                   PARM                    XMSGDS
369800010911  "  C                   PARM                    XMSGTXT         135
369900010911  "  C                   PARM      WXMsgInz      XMSGInz           1
370000010911  "  C*
370100010911  "  C                   PARM      WMSGCd1       XMSGCd1           7
370200010911  "  C*
370300010911  "  C                   PARM      *Blank        XMSGCd2           7
370400010911  "  C*
370500010911  "  C                   PARM      *Blank        XMSGCd3           7
370600010911  "  C*
370700010911  "  C                   Move      *Off          WXMsgInz
370800010911  "  C*
370900010911  "  C                   ENDSR
371000010911  "  C/EJECT
371100010911  "  C******************************************************
371200010911  "  C* Reperimento testi delle possibili risposte
371300010911  "  C******************************************************
371400010911  "  C     RepRispost    BEGSR
371500010911  "  C*
371600010911  "  C                   EVAL      IDMSG = 'PROMSG    *LIBL     xxxxxxx'
371700010911  "  C* Continua
371800010911  "  C                   Move      'COS1025'     IdMsg
371900010911  "  C                   CallB     'XRTVM'
372000010911  "  C                   Parm                    IdMsg
372100010911  "  C                   Parm      *Blank        TxTMsg
372200010911  "  C                   Parm                    ErrMsg
372300010911  "  C                   if        ErrMsg      = *On
372400010911  "  C                   MoveL     *All'?'       TxtMsg
372500010911  "  C                   endif
372600010911  "  C*
372700010911  "  C                   Eval      %SubSt(XMsgTxt:1:15)  = TxtMsg
372800010911  "  C*
372900010911  "  C* Ritorno
373000010911  "  C                   Move      'COS1026'     IdMsg
373100010911  "  C                   CallB     'XRTVM'
373200010911  "  C                   Parm                    IdMsg
373300010911  "  C                   Parm      *Blank        TxTMsg
373400010911  "  C                   Parm                    ErrMsg
373500010911  "  C                   if        ErrMsg      = *On
373600010911  "  C                   MoveL     *All'?'       TxtMsg
373700010911  "  C                   endif
373800010911  "  C*
373900010911  "  C                   Eval      %SubSt(XMsgTxt:16:15)  = TxtMsg
374000010911  "  C*
374100010911  "  C                   ENDSR
374200010911  "  C/EJECT
374300010911  "  C************************************************************
374400010911  "  C* Conversione divisa di partita in m.c.
374500010911  "  C************************************************************
374600010911  "  C     RTN506        BEGSR
374700010911  "  C*
374800010911  "  C* modo di ritorno
374900010911  "  C*
375000010911  "  C                   Move      OPR506        WOpr
375100010911  "  C*
375200010911  "  C* ricarico subfile se eseguito aggiornamento
375300010911  "  C     OPR506        ifeq      '1'
375400010911  "  C                   MOVE      *ON           WINZS1
375500010911  "  C                   endif
375600010911  "  c*
375700010911  "  C* funzione non eseguibile per errore :
375800010911  "  C*
375900010911  "  C                   SELECT
376000010911  "  C* 1 = funzione richiamata chiusa in errore
376100010911  "  C     ERR506        WHENEQ    '1'
376200010911  "  C                   MOVE      *ON           $ESCI
376300010911  "  C                   SETON                                        544099
376400010911  "  C* 2 = funzione richiamata senza la dovuta autorizzazione
376500010911  "  C     ERR506        WHENEQ    '2'
376600010911  "  C                   MOVE      *ON           $ESCI
376700010911  "  C                   SETON                                        944099
376800010911  "  C*
376900010911  "  C                   ENDSL
377000010911  "  C*
377100010911  "  C                   ENDSR
377200010911C1511C/EJECT
377300990803C1213C************************************************************
377400990803  "  C* GESTIONE RITORNO DA PGM CONTENZIOSO
377500990803  "  C************************************************************
377600990803  "  C     RTNL70        BEGSR
377700990803  "  C*
377800990803  "  C* modo di ritorno
377900990803  "  C*
378000990803  "  C                   SELECT
378100990803  "  C* questi modi di utilizzo dei valori di ritorno dal
378200990803  "  C* pgm di manutenzione rcd di anagrafica sono delle
378300990803  "  C* proposte, normalmente sempre valide, ma modificabili
378400990803  "  C* per situazioni particolari
378500990803  "  C* 1 = F3
378600990803  "  C     RETL70        WHENEQ    '1'
378700990803  "  C                   MOVE      *ON           $ESCI
378800990803  "  C                   MOVE      *ON           $FINE
378900990803  "  C                   MOVE      '1'           RET086
379000990803  "  C* 2 = F12
379100990803  "  C     RETL70        WHENEQ    '2'
379200990803  "  C     OPRL70        ANDNE     '1'
379300990803  "  C                   MOVE      *ON           $ESCI
379400990803  "  C*
379500990803  "  C                   ENDSL
379600070713c1213C*
379700070713c2023C* ricarico subfile se eseguito aggiornamento blocco
379800070713  "  C     OPRL70        ifeq      '1'
379900070713  "  C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
380000070713  "  C                   eval      ppasys = h1sys
380100070713  "  C                   eval      ppanrasint = h1nrasint
380200070713  "  C     K02PPA01      CHAIN     NDPPS01                            23
380300070713  "  C                   if        *in23 = *off
380400070713  "  C                   exsr      aggs1
380500070713  "  C                   endif
380600070713c2023C                   endif
380700070713c1213C*
380800990803  "  C* funzione non eseguibile per errore :
380900990803  "  C*
381000990803  "  C                   SELECT
381100990803  "  C* 1 = funzione richiamata chiusa in errore
381200990803  "  C     ERRL70        WHENEQ    '1'
381300990803  "  C                   MOVE      *ON           $ESCI
381400990803  "  C                   SETON                                        9599
381500990803  "  C* 2 = funzione richiamata senza la dovuta autorizzazione
381600990803  "  C     ERRL70        WHENEQ    '2'
381700990803  "  C                   MOVE      *ON           $ESCI
381800990803  "  C                   SETON                                        9499
381900990803  "  C*
382000990803  "  C                   ENDSL
382100990803  "  C*
382200990803  "  C                   ENDSR
382300990803C1213C/EJECT
382301110414C2185C************************************************************
382302110414  "  C* GESTIONE RITORNO DA PGM DI GESTIONE Estensione nominativi
382303110414  "  C************************************************************
382304110414C2185C     RTN0661       BEGSR
382305110414     C*
382306110414     C* modo di ritorno
382307110414     C*
382308110414     C                   SELECT
382309110414     C* questi modi di utilizzo dei valori di ritorno dal
382310110414     C* pgm di manutenzione rcd di anagrafica sono delle
382311110414     C* proposte, normalmente sempre valide, ma modificabili
382312110414     C* per situazioni particolari
382313110414     C* 1 = F3
382314110414    >C     RET0661       WHENEQ    '1'
382315110414     C                   MOVE      *ON           $ESCI
382316110414     C                   MOVE      *ON           $FINE
382317110414     C                   MOVE      '1'           RET086
382318110414     C* 2 = F12
382319110414    >C     RET0661       WHENEQ    '2'
382320110414     C                   MOVE      *ON           $ESCI
382321110414     C*
382322110414     C                   ENDSL
382323110414     C*
382324110414     C* operazione eseguite dal pgm chiamato
382325110414     C*
382326110414    >C                   MOVE      OPR0661       WOPR
382352110414     C*
382353110414     C* funzione non eseguibile per errore :
382354110414     C*
382355110414     C                   SELECT
382356110414     C* 1 = funzione richiamata chiusa in errore
382357110414    >C     ERR0661       WHENEQ    '1'
382358110414     C                   MOVE      *ON           $ESCI
382359110414     C                   SETON                                        9599
382360110414     C* 2 = funzione richiamata senza la dovuta autorizzazione
382361110414    >C     ERR0661       WHENEQ    '2'
382362110414     C                   MOVE      *ON           $ESCI
382363110414     C                   SETON                                        9499
382364110414     C*
382365110414     C                   ENDSL
382366110414     C*
382367110414     C                   ENDSR
382400940223     C************************************************************
382500951109     C* GESTIONE RITORNO DA PGM movimenti
382600940223     C************************************************************
382700951109    >C     RTN087        BEGSR
382800940223     C*
382900940223     C* modo di ritorno
383000940223     C*
383100941027     C                   SELECT
383200950301     C* questi modi di utilizzo dei valori di ritorno dal
383300950301     C* pgm di manutenzione rcd di anagrafica sono delle
383400950301     C* proposte, normalmente sempre valide, ma modificabili
383500950301     C* per situazioni particolari
383600940223     C* 1 = F3
383700951109    >C     RET087        WHENEQ    '1'
383800940224     C                   MOVE      *ON           $ESCI
383900940223     C                   MOVE      *ON           $FINE
384000951108     C                   MOVE      '1'           RET086
384100940223     C* 2 = F12
384200951109    >C     RET087        WHENEQ    '2'
384300970624     C     OPR087        ANDNE     '1'
384400940223     C                   MOVE      *ON           $ESCI
384500960123     C*
384600960123     C* 3 = Effettuata una selezione, per cui vado a fine pgm
384700960123    >C     RET087        WHENEQ    '3'
384800960123     C                   MOVE      *ON           $ESCI
384900960123     C                   MOVE      *ON           $FINE
385000960123     C                   MOVE      '3'           RET086
385100960123     c* Ripasso nella ds ndc086ds la partita selezionata
385200960123     c                   eval      dtpar086 = dtpar087
385300960123     c                   eval      nrpar086 = nrpar087
385400960123     c                   eval      serpar086 = serpar087
385500960123     c                   eval      dtsca086  = dtsca087
385600940223     C*
385700941027     C                   ENDSL
385800940223     C*
385900940223     C* operazione eseguite dal pgm chiamato
386000940223     C*
386100951109    >C                   MOVE      OPR087        WOPR
386200941027     C                   SELECT
386300940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
386400951109    >C     OPR087        WHENEQ    '1'
386500970627     C* non viene eseguito il ricaricamento apposta perchè tanto il saldo
386600970627     c* non viene ricalcolato apposta per rendere il programma più veloce
386700970627     C*******            MOVE      *ON           WINZS1
386800940223     C*
386900941027     C                   ENDSL
387000940223     C*
387100940223     C* funzione non eseguibile per errore :
387200940223     C*
387300941027     C                   SELECT
387400940223     C* 1 = funzione richiamata chiusa in errore
387500951109    >C     ERR087        WHENEQ    '1'
387600940223     C                   MOVE      *ON           $ESCI
387700951109     C                   SETON                                        9599
387800950301     C* 2 = funzione richiamata senza la dovuta autorizzazione
387900951109    >C     ERR087        WHENEQ    '2'
388000950301     C                   MOVE      *ON           $ESCI
388100951109     C                   SETON                                        9499
388200941027     C*
388300941027     C                   ENDSL
388400940223     C*
388500940223     C                   ENDSR
388600981002     C/EJECT
388700981002     C************************************************************
388800981002     C* GESTIONE RITORNO DA PGM partite/movimenti
388900981002     C************************************************************
389000981002    >C     RTN199        BEGSR
389100981002     C*
389200981002     C* modo di ritorno
389300981002     C*
389400981002     C                   SELECT
389500981002     C* questi modi di utilizzo dei valori di ritorno dal
389600981002     C* pgm di manutenzione rcd di anagrafica sono delle
389700981002     C* proposte, normalmente sempre valide, ma modificabili
389800981002     C* per situazioni particolari
389900981002     C* 1 = F3
390000981002    >C     RET199        WHENEQ    '1'
390100981002     C                   MOVE      *ON           $ESCI
390200981002     C                   MOVE      *ON           $FINE
390300981002     C                   MOVE      '1'           RET086
390400981002     C* 2 = F12
390500981002    >C     RET199        WHENEQ    '2'
390600981002     C     OPR199        ANDNE     '1'
390700981002     C                   MOVE      *ON           $ESCI
390800981002     C*
390900981002     C* 3 = Effettuata una selezione, per cui vado a fine pgm
391000981002    >C     RET199        WHENEQ    '3'
391100981002     C                   MOVE      *ON           $ESCI
391200981002     C                   MOVE      *ON           $FINE
391300981002     C                   MOVE      '3'           RET086
391400981002     c* Ripasso nella ds ndc086ds la partita selezionata
391500981002     c                   eval      dtpar086 = dtpar199
391600981002     c                   eval      nrpar086 = nrpar199
391700981002     c                   eval      serpar086 = serpar199
391800981002     c                   eval      dtsca086  = dtsca199
391900981002     C*
392000981002     C                   ENDSL
392100981002     C*
392200981002     C* operazione eseguite dal pgm chiamato
392300981002     C*
392400981002    >C                   MOVE      OPR199        WOPR
392500981002     C                   SELECT
392600981002     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
392700981002    >C     OPR199        WHENEQ    '1'
392800981002     C* non viene eseguito il ricaricamento apposta perchè tanto il saldo
392900981002     c* non viene ricalcolato apposta per rendere il programma più veloce
393000981002     C*******            MOVE      *ON           WINZS1
393100981002     C*
393200981002     C                   ENDSL
393300981002     C*
393400981002     C* funzione non eseguibile per errore :
393500981002     C*
393600981002     C                   SELECT
393700981002     C* 1 = funzione richiamata chiusa in errore
393800981002    >C     ERR199        WHENEQ    '1'
393900981002     C                   MOVE      *ON           $ESCI
394000981002     C                   SETON                                        9599
394100981002     C* 2 = funzione richiamata senza la dovuta autorizzazione
394200981002    >C     ERR199        WHENEQ    '2'
394300981002     C                   MOVE      *ON           $ESCI
394400981002     C                   SETON                                        9499
394500981002     C*
394600981002     C                   ENDSL
394700981002     C*
394800981002     C                   ENDSR
394900951109     C/EJECT
395000951109     C************************************************************
395100951109     C* GESTIONE RITORNO DA PGM DI particolarità partita
395200951109     C************************************************************
395300951109    >C     RTN060        BEGSR
395400951109     C*
395500951109     C* modo di ritorno
395600951109     C*
395700951109     C                   SELECT
395800951109     C* questi modi di utilizzo dei valori di ritorno dal
395900951109     C* pgm di manutenzione rcd di anagrafica sono delle
396000951109     C* proposte, normalmente sempre valide, ma modificabili
396100951109     C* per situazioni particolari
396200951109     C* 1 = F3
396300951109    >C     RET060        WHENEQ    '1'
396400951109     C                   MOVE      *ON           $ESCI
396500951109     C                   MOVE      *ON           $FINE
396600951109     C                   MOVE      '1'           RET086
396700951114     C* 2 = F12 (ma non c'è un aggiornamento)
396800951109    >C     RET060        WHENEQ    '2'
396900970624     C     OPR060        ANDNE     '1'
397000951109     C                   MOVE      *ON           $ESCI
397100951109     C*
397200951109     C                   ENDSL
397300951109     C*
397400951109     C* operazione eseguite dal pgm chiamato
397500951109     C*
397600951109    >C                   MOVE      OPR060        WOPR
397700951109     C                   SELECT
397800951115     C* 1 = eseguito aggiornamento (richiesto aggiornamento subfile)
397900951109    >C     OPR060        WHENEQ    '1'
398000951115     C* Se cliente/ fornitore ( sottonatura = 'C o F') leggo file estensione
398100951115     C* se non cliente o fornitore non interessa
398200951115     C                   IF        snatura086 = 'C' or snatura086 = 'F'
398300951115     C                   eval      ppasys = h1sys
398400951115     C                   eval      ppanrasint = h1nrasint
398500951115     C     K02PPA01      CHAIN     NDPPS01                            23
398600951115     C                   if        *in23 = *off
398700951115     C                   exsr      aggs1
398800951115     C                   endif
398900951115     C                   ENDIF
399000951109     C*
399100951109     C                   ENDSL
399200951109     C*
399300951109     C* funzione non eseguibile per errore :
399400951109     C*
399500951109     C                   SELECT
399600951109     C* 1 = funzione richiamata chiusa in errore
399700951109    >C     ERR060        WHENEQ    '1'
399800951109     C                   MOVE      *ON           $ESCI
399900951109     C                   SETON                                        9599
400000951109     C* 2 = funzione richiamata senza la dovuta autorizzazione
400100951109    >C     ERR060        WHENEQ    '2'
400200951109     C                   MOVE      *ON           $ESCI
400300951109     C                   SETON                                        9499
400400951109     C*
400500951109     C                   ENDSL
400600951109     C*
400700951109     C                   ENDSR
400800981006     C/EJECT
400900981006     C************************************************************
401000981006     C* GESTIONE RITORNO DA PGM DI GESTIONE SCADENZE/rate
401100981006     C************************************************************
401200981006    >C     RTN063        BEGSR
401300981006     C*
401400981006     C* modo di ritorno
401500981006     C*
401600981006     C                   SELECT
401700981006     C* questi modi di utilizzo dei valori di ritorno dal
401800981006     C* pgm di manutenzione rcd di anagrafica sono delle
401900981006     C* proposte, normalmente sempre valide, ma modificabili
402000981006     C* per situazioni particolari
402100981006     C* 1 = F3
402200981006    >C     RET063        WHENEQ    '1'
402300981006     C                   MOVE      *ON           $ESCI
402400981006     C                   MOVE      *ON           $FINE
402500981006     C                   MOVE      '1'           RET086
402600981006     C* 2 = F12
402700981006    >C     RET063        WHENEQ    '2'
402800070612c2028C     OPR063        ANDNE     '1'
402900981006     C                   MOVE      *ON           $ESCI
403000981006     C*
403100981006     C                   ENDSL
403200981006     C*
403300981006     C* operazione eseguite dal pgm chiamato
403400981006     C*
403500981006    >C                   MOVE      OPR063        WOPR
403600981006     C                   SELECT
403700981006     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
403800981006    >C     OPR063        WHENEQ    '1'
403900981006     C*
404000981006     C* se l'opzione con cui è stato chiamato non è la modifica
404100981006     C* memorizzo di ripristinare il sfl alla fine del ciclo di opzioni
404200981006    >C     OPZ063        IFNE      '02'
404300981006     C                   MOVE      *ON           WINZS1
404400981006     C* se l'opzione con cui è stato chiamato è la modifica
404500981006     C* aggiorno subito la riga di sfl e non richiedo il ripristino sfl
404600981006     C                   ELSE
404700011213D1016C                   IF        snatura086 = 'C' or snatura086 = 'F'
404800011213  "  C                   eval      ppasys = h1sys
404900011213  "  C                   eval      ppanrasint = h1nrasint
405000011213  "  C     K02PPA01      CHAIN     NDPPS01                            23
405100011213  "  C                   if        *in23 = *off
405200011213  "  C                   exsr      aggs1
405300011213  "  C                   endif
405400011213d1016C                   ENDIF
405500011213d1016x****               EXSR      AGGS1
405600981006     C                   ENDIF
405700981006     C*
405800981006     C                   ENDSL
405900981006     C*
406000981006     C* funzione non eseguibile per errore :
406100981006     C*
406200981006     C                   SELECT
406300981006     C* 1 = funzione richiamata chiusa in errore
406400981006    >C     ERR063        WHENEQ    '1'
406500981006     C                   MOVE      *ON           $ESCI
406600981006     C                   SETON                                        5299
406700981006     C* 2 = funzione richiamata senza la dovuta autorizzazione
406800981006    >C     ERR063        WHENEQ    '2'
406900981006     C                   MOVE      *ON           $ESCI
407000981006     C                   SETON                                        5499
407100981006     C*
407200981006     C                   ENDSL
407300981006     C*
407400981006     C                   ENDSR
407500950707     C/EJECT
407600950707     C************************************************************
407700950707     C* GESTIONE RITORNO DA PGM DI GESTIONE MIX DI PARTITA
407800950707     C************************************************************
407900950707    >C     RTN065        BEGSR
408000950707     C*
408100950707     C* modo di ritorno
408200950707     C*
408300950707     C                   SELECT
408400950707     C* questi modi di utilizzo dei valori di ritorno dal
408500950707     C* pgm di manutenzione rcd di anagrafica sono delle
408600950707     C* proposte, normalmente sempre valide, ma modificabili
408700950707     C* per situazioni particolari
408800950707     C* 1 = F3
408900950707    >C     RET065        WHENEQ    '1'
409000950707     C                   MOVE      *ON           $ESCI
409100950707     C                   MOVE      *ON           $FINE
409200951108     C                   MOVE      '1'           RET086
409300950707     C* 2 = F12
409400950707    >C     RET065        WHENEQ    '2'
409500970624     C     OPR065        ANDNE     '1'
409600950707     C                   MOVE      *ON           $ESCI
409700950707     C*
409800950707     C                   ENDSL
409900950707     C*
410000950707     C* operazione eseguite dal pgm chiamato
410100950707     C*
410200950707    >C                   MOVE      OPR065        WOPR
410300951109     C                   SELECT
410400951109     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
410500951109    >C     OPR065        WHENEQ    '1'
410600970627     C* non viene eseguito il ricaricamento apposta perchè tanto il saldo
410700970627     c* non viene ricalcolato apposta per rendere il programma più veloce
410800970627     C*******            MOVE      *ON           WINZS1
410900950707     C*
411000950707     C                   ENDSL
411100950707     C*
411200950707     C* funzione non eseguibile per errore :
411300950707     C*
411400950707     C                   SELECT
411500950707     C* 1 = funzione richiamata chiusa in errore
411600950707    >C     ERR065        WHENEQ    '1'
411700950707     C                   MOVE      *ON           $ESCI
411800951109     C                   SETON                                        9599
411900950707     C* 2 = funzione richiamata senza la dovuta autorizzazione
412000950707    >C     ERR065        WHENEQ    '2'
412100950707     C                   MOVE      *ON           $ESCI
412200951109     C                   SETON                                        9499
412300950707     C*
412400950707     C                   ENDSL
412500950707     C*
412600950707     C                   ENDSR
412700950301     C/EJECT
412800950301     C************************************************************
412900950301     C* AGGIORNAMENTO RIGA LISTA
413000950301     C************************************************************
413100950301     C     AGGS1         BEGSR
413200950301     C*
413300950301     C* riempio i campi del sfl che sono stati ripassati dal pgm
413400950301     C* di manutenzione dopo una opzione 02
413500951115     C                   CALLB     'XDT4000'
413600951115     C                   PARM      PPSDTDoc      XDTAMG
413700951115     C     S1DTDoc       PARM                    XDTGMA
413800951115     C                   PARM      2             XDTSTA
413900951115     C                   EVAL      S1nrDOC = PPSnrDOC
414000951115     C                   EVAL      S1serdoc = PPSserDOC
414100951115     C                   EVAL      S1kscage = ppsKscAGE
414200011213     c                   EVAL      S1bkamm  = PPSbkamm
414300070612     c* se partita non bloccata
414400011213     c* leggo file SCADENZE/ratei
414500011213     c                   if        s1bkamm = *blanks
414600070612C2028X***************************  and snatura086 = 'F'
414700011213     c     K02PPA01      SETLL     NDPSC01                                22
414800011213     c                   do        *hival
414900011213     c     K02PPA01      reade     NDPSC01                                22
415000011213     c   22              leave
415100011213     c                   if        pscBkamm <> *blanks
415200011213     c                   eval      s1bkamm = '+'
415300011213     c                   leave
415400011213     c                   endif
415500011213     c                   enddo
415600011213     c                   endif
415700951115     C                   EVAL      S1ctgana1= PPSctgana1
415800951115     C                   EVAL      S1ctgana2= PPSctgana2
415900951115     C                   EVAL      S1ctgana3= PPSctgana3
416000981006     c                   eval      s1unita = unita060
416100950301     C*
416200950301     C                   ENDSR
416300941124     C/EJECT
416400941124     C************************************************************
416500941124     C* GESTIONE RITORNO DA PGM DEI COMMENTI
416600941124     C************************************************************
416700941124     C     RTN725        BEGSR
416800941124     C*
416900941124     C* modo di ritorno
417000941124     C*
417100941124     C                   SELECT
417200950301     C* questi modi di utilizzo dei valori di ritorno dal
417300950301     C* pgm di manutenzione rcd di anagrafica sono delle
417400950301     C* proposte, normalmente sempre valide, ma modificabili
417500950301     C* per situazioni particolari
417600941124     C* 1 = F3
417700941124     C     RET725        WHENEQ    '1'
417800941124     C                   MOVE      *ON           $ESCI
417900941124     C                   MOVE      *ON           $FINE
418000951114     C                   MOVE      '1'           RET086
418100941124     C* 2 = F12
418200941124     C     RET725        WHENEQ    '2'
418300970624     C     OPR725        ANDNE     '1'
418400941124     C                   MOVE      *ON           $ESCI
418500941124     C*
418600941124     C                   ENDSL
418700941124     C*
418800941124     C* operazione eseguite dal pgm chiamato
418900941124     C*
419000941124     C                   SELECT
419100941124     C* 1 = eseguita scrittura o modifica commento
419200941124     C     OPR725        WHENEQ    '1'
419300990803C1215 *
419400990803  "   *Testo esistenza commenti partita solo se sono arrivato dopo
419500990803  "   *solo se sono qui dopo aver fatto Opz 14=Commenti (sulla partita)
419600990803C1215C                   If        TPA725      = 'PPA'
419700990803     C*
419800950704     C* reperisco se commento presente
419900950704     C                   MOVE      *BLANK        EXI724
420000950704     C                   RESET                   A724DS
420100950704     C                   MOVEL     '01'          OPZ724
420200950704     C                   MOVEL     XscSOC        SOC724
420300950704     C                   MOVEL     *BLANK        UNI724
420400951120     C                   MOVEL     'PPA'         TPA724
420500951109     C                   MOVEL     Ctb086        KY1724
420600950907     C                   MOVEL     C1KCC         KY2724
420700950907     C                   MOVE      C1KSC         KY2724
420800040308     C                   MOVEL     S1DTPAR       KY3724
420900040308     C                   MOVEL     s1nrpar       KY4724
421000040308     C                   MOVE      s1serpar      KY4724
421100951109     C                   MOVEL     WDATE         DTS724
421200950704     C                   MOVEL     A724DS        KPJBU
421300950704     C                   CALL      'ANA724R'
421400950704     C                   PARM                    KPJBA
421500950704     C                   MOVEL     KPJBU         A724DS
421600950704     C* memorizzo lo stato del  bottone TCM
421700950704     C                   MOVE      EXI724        COMMENTIH
421800990803C1215 *
421900990803  "   *Testo esistenza commenti dopo F14=Commenti (sul conto)
422000990803  "  C                   Else
422100990803  "  C                   EXSR      TstTcm
422200990803  "  C*  Attivo o no scritta commenti per il conto
422300990803  "  C                   IF        Exi724     = *On
422400990803  "  C                   Eval      *In19      = *On
422500990803  "  C                   Else
422600990803  "  C                   Eval      *In19      = *Off
422700990803  "  C                   EndIf
422800990803  "  C*
422900990803C1215C                   EndIf
423000941124     C                   ENDSL
423100941124     C*
423200941124     C* funzione non eseguibile per errore :
423300941124     C*
423400941124     C                   SELECT
423500941124     C* 1 = funzione richiamata chiusa in errore
423600941124     C     ERR725        WHENEQ    '1'
423700941124     C                   MOVE      *ON           $ESCI
423800951109     C                   SETON                                        9599
423900950331     C* 2 = funzione richiamata senza la dovuta autorizzazione
424000950331     C     ERR725        WHENEQ    '2'
424100950331     C                   MOVE      *ON           $ESCI
424200951109     C                   SETON                                        9499
424300950704    >C*  eventualmente gestire altri codici di errore
424400941124     C*
424500941124     C                   ENDSL
424600941124     C*
424700941124     C                   ENDSR
424800020118C1530C************************************************************
424900020118  "  C* GESTIONE RITORNO DA PGM visualizzazione documenti da Laguna
425000020118  "  C************************************************************
425100020118C1530C     RTN670        BEGSR
425200020118     C*
425300020118     C* modo di ritorno
425400020118     C*
425500020118     C* al ritorno da questa opzione non continuo il ciclo
425600020118     C                   MOVE      *ON           $ESCI
425700020118     C*
425800020118C1530C                   ENDSR
425900941205     C/EJECT
426000941205     C************************************************************
426100941205     C* CONTROLLO ABILITAZIONE TASTI
426200941205     C* INPUT
426300941205     C* $TASTO =Tasto funzione premuto
426400941205     C* $FC = Tasti abilitati
426500941205     C* OUTPUT
426600941205     C* IN99 = ERrore
426700941205     C************************************************************
426800941205     C     CTRKEY        BEGSR
426900941205     C*
427000941205     C                   SELECT
427100941205     C     $TASTO        WHENEQ    F01
427200941205     C     $FC(1)        IFEQ      '0'
427300941205     C                   SETON                                        99
427400941205     C                   ENDIF
427500941205     C     $TASTO        WHENEQ    F02
427600941205     C     $FC(2)        IFEQ      '0'
427700941205     C                   SETON                                        99
427800941205     C                   ENDIF
427900941205     C     $TASTO        WHENEQ    F03
428000941205     C     $FC(3)        IFEQ      '0'
428100941205     C                   SETON                                        99
428200941205     C                   ENDIF
428300941205     C     $TASTO        WHENEQ    F04
428400941205     C     $FC(4)        IFEQ      '0'
428500941205     C                   SETON                                        99
428600941205     C                   ENDIF
428700941205     C     $TASTO        WHENEQ    F05
428800941205     C     $FC(5)        IFEQ      '0'
428900941205     C                   SETON                                        99
429000941205     C                   ENDIF
429100941205     C     $TASTO        WHENEQ    F06
429200941205     C     $FC(6)        IFEQ      '0'
429300941205     C                   SETON                                        99
429400941205     C                   ENDIF
429500941205     C     $TASTO        WHENEQ    F07
429600941205     C     $FC(7)        IFEQ      '0'
429700991014     C                   SETON                                        99
429800941205     C                   ENDIF
429900941205     C     $TASTO        WHENEQ    F08
430000941205     C     $FC(8)        IFEQ      '0'
430100941205     C                   SETON                                        99
430200941205     C                   ENDIF
430300941205     C     $TASTO        WHENEQ    F09
430400941205     C     $FC(09)       IFEQ      '0'
430500941205     C                   SETON                                        99
430600941205     C                   ENDIF
430700941205     C     $TASTO        WHENEQ    F10
430800941205     C     $FC(10)       IFEQ      '0'
430900941205     C                   SETON                                        99
431000941205     C                   ENDIF
431100941205     C     $TASTO        WHENEQ    F11
431200941205     C     $FC(11)       IFEQ      '0'
431300941205     C                   SETON                                        99
431400941205     C                   ENDIF
431500941205     C     $TASTO        WHENEQ    F12
431600941205     C     $FC(12)       IFEQ      '0'
431700941205     C                   SETON                                        99
431800941205     C                   ENDIF
431900941205     C     $TASTO        WHENEQ    F13
432000941205     C     $FC(13)       IFEQ      '0'
432100941205     C                   SETON                                        99
432200941205     C                   ENDIF
432300941205     C     $TASTO        WHENEQ    F14
432400941205     C     $FC(14)       IFEQ      '0'
432500941205     C                   SETON                                        99
432600941205     C                   ENDIF
432700941205     C     $TASTO        WHENEQ    F15
432800941205     C     $FC(15)       IFEQ      '0'
432900941205     C                   SETON                                        99
433000941205     C                   ENDIF
433100941205     C     $TASTO        WHENEQ    F16
433200941205     C     $FC(16)       IFEQ      '0'
433300941205     C                   SETON                                        99
433400941205     C                   ENDIF
433500941205     C     $TASTO        WHENEQ    F17
433600941205     C     $FC(17)       IFEQ      '0'
433700941205     C                   SETON                                        99
433800941205     C                   ENDIF
433900941205     C     $TASTO        WHENEQ    F18
434000941205     C     $FC(18)       IFEQ      '0'
434100941205     C                   SETON                                        99
434200941205     C                   ENDIF
434300941205     C     $TASTO        WHENEQ    F19
434400941205     C     $FC(19)       IFEQ      '0'
434500941205     C                   SETON                                        99
434600941205     C                   ENDIF
434700941205     C     $TASTO        WHENEQ    F20
434800941205     C     $FC(20)       IFEQ      '0'
434900941205     C                   SETON                                        99
435000941205     C                   ENDIF
435100941205     C     $TASTO        WHENEQ    F21
435200941205     C     $FC(21)       IFEQ      '0'
435300941205     C                   SETON                                        99
435400941205     C                   ENDIF
435500941205     C     $TASTO        WHENEQ    F22
435600941205     C     $FC(22)       IFEQ      '0'
435700941205     C                   SETON                                        99
435800941205     C                   ENDIF
435900941205     C     $TASTO        WHENEQ    F23
436000941205     C     $FC(23)       IFEQ      '0'
436100941205     C                   SETON                                        99
436200941205     C                   ENDIF
436300941205     C     $TASTO        WHENEQ    F24
436400941205     C     $FC(24)       IFEQ      '0'
436500941205     C                   SETON                                        99
436600941205     C                   ENDIF
436700941205     C                   ENDSL
436800941205     C*
436900941205     C                   ENDSR
437000940223     C/EJECT
437100940131     C************************************************************
437200940131     C* OPERAZIONI INIZIALI
437300940131     C************************************************************
437400940131     C     *INZSR        BEGSR
437500000118C1309C     *DMY          Move      Udate         ZDateISO
437600000118C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
437700960709     C*
437800960709     C* testo se sono stati passati i dati di autorizzazione
437900960709     C*
438000960710     C                   IF        Parms < 4       or
438100960710     C                             %addr(AUTDS) = *null
438200950410     C                   MOVE      *BLANK        AUTDS
438300950410     C                   ELSE
438400950410     C                   MOVEL     DSAUT         AUTDS
438500950410     C                   ENDIF
438600951108     C*
438700951108     C* Reperimento autorizzazioni
438800951108     C* le autorizzazioni potrebbero essere già state passate,
438900951108     C* nel qual caso non vanno reperite
439000951108    >C     AUTDS         IFEQ      *BLANK
439100951108    >C                   EXSR      REPAUT
439200960930     c                   else
439300960930     c                   movel     xcadds        coneccds
439400951108     C                   ENDIF
439500940225     C*
439600940223     C* Reperimento opzioni
439700940127     C*
439800940128     C                   EXSR      REPOPZ
439900950406     C*
440000940506     C* Reperimento tasti di funzione
440100940506     C*
440200940506     C                   EXSR      REPKEY
440300951109     C* Udate in iso
440400951109     C                   CALLb     'XDT4000'
440500951109     C     WDATE         PARM                    XDTAMG
440600040308C1309X*****              PARM      UDATE         XDTGMA
440700000118C1309C                   PARM      ZDate         XDTGMA
440800951109     C                   PARM      4             XDTSTA
440900030619D1378 *
441000030619  "   * Controllo se siamo sotto Web Facing
441100030620  "  C                   z-add     0             WebFacingOn
441200030619  "  C                   call      'XWEBFACING'                         26
441300030620  "  C                   parm                    WebFacingOn
441400030619  "  C   26              z-add     0             WebFacingOn
441500030619D1378 *
441600940127     C*
441700940117     C                   ENDSR
441800940223     C/EJECT
441900940223     C************************************************************
442000940223     C* INIZIALIZZAZIONE VARIABILI
442100940223     C************************************************************
442200940223     C     INZVAR        BEGSR
442300981110     C*
442400981110     C* Reperimento parametri
442500981110     C*
442600981110     C     *ENTRY        PLIST
442700981110     C                   PARM                    KPJBA
442800981110     c                   PARM                    ndc086ds
442900981110     C                   PARM                    SOC00x
443000981110     C                   PARM                    DSAUT
443100981110     C                   PARM                    skpartite
443101160913>>>>> // Ulteriori controlli autorizzazioni.
443102160913>>>>>C                   CALL      'YDC086R'
443103160913>>>>>C                   PARM                    kpjba
443104160913>>>>>c                   PARM                    ndc086ds
443105160913>>>>>C                   PARM                    soc00x
443106160913>>>>>C                   PARM                    dsAut
443107160913>>>>>C                   PARM                    skPartite
443108160913>>>>>C                   PARM      0             yEsito
443109160913>>>>>C                   IF        yEsito < *ZERO
443110160913>>>>>C                   RETURN
443111160913>>>>>C                   ENDIF
443200981110     C*
443300981110     C* testo se sono stati passati i dati di società
443400981110     C*  il caso che non siano arrivati i dati di società è quando il
443500981110     C*  chiamante richiede dati presenti su un altro SistemaInformativo,
443600981110     C*  in questo caso siamo in un nuovo GrpJob e il pgm si aprirà sempre
443700981110     C*  passando dall'*inzsr (si ma se cambia solo la società e NON il
443800981110     C*  sistema informativo non passa di qui e rimane la società vecchia)
443900981111     c*  vedi caso da estratto conto MULTIPLO.!!!
444000981110     C                   IF        Parms < 3       or
444100981110     C                             %addr(SOC001) = *null
444200981110     C                   MOVE      *BLANK        SOC001
444300981110     C                   ELSE
444400981110     C                   MOVEL     SOC00x        SOC001
444500981110     C                   ENDIF
444600981110     C*
444700981110     C* Reperimento dati società
444800981110     C* potrebbero essere già stati passati,
444900981110     C* nel qual caso non vanno reperiti
445000040308     C     SOC001        IFEQ      *BLANK
445100040308     C                   MOVEL     'SOC001'      TIPXSC
445200981110     C                   MOVEL     Societa086    SOCXSC
445300981110     C                   EXSR      REPSOC
445400981110     C* Controlli dati società
445500981110     C     RTNXSC        IFNE      '1'
445600981110     C                   MOVEL     XSOCDS        SOC001
445700981110     C                   ELSE
445800040308     C                   MOVE      '1'           Err086
445900981110     C                   MOVE      *on           $Fine
446000981110     C                   EXSR      EndPgm
446100981110     C                   ENDIF
446200981110     C                   ENDIF
446300940506     C*
446400990803C1213C* reperisco autorizzazioni per contenzioso
446500990803  "  C                   EXSR      REPAUTLeg
446600990803C1213 *
446700070621c2034c* Reperisco tutti i dati del conto
446800070621c2034c                   exsr      callmvc002
446900940506     C* Definizione delle opzioni utilizzabili a pgm
447000940506     C*
447100940506     C                   EXSR      DEFOPZ
447200950330     C*
447300940223     C* Variabili per gestione videate
447400940223     C*
447500940223     C                   MOVE      *OFF          $FINE
447600940223     C                   MOVE      *OFF          $INZS1
447700950606     C                   MOVE      *OFF          $EndFile
447800940223     C                   MOVE      *OFF          $ESCI
447900940223     C                   MOVE      *OFF          $RCDOK
448000940509     C                   MOVE      *HIVAL        $LASTV
448100940506     C                   Z-ADD     0             $ULKS1            3 0
448200940318     C                   MOVE      *ON           $INZS1
448300951109     C                   MOVE      'S1'          $GEST
448400970624     C                   CLEAR                   WOPR
448500951110     C                   CLEAR                   Wsaldototd
448600951110     C                   CLEAR                   Wsaldotota
448700960307     C                   CLEAR                   WsaldotoDD
448800960307     C                   CLEAR                   WsaldotoAD
448900960318     c*
449000960318     c* pulisco indice e schiera partite
449100960318     c                   if        parms >=5 and multip086 = *on
449200960318     c                   z-ADD     0             IndPar            3 0
449300960318     c                   do        50            $X
449400960318     c     $x            occur     skpartite
449500960318     C                   CLEAR                   SkPartite
449600960318     C                   enddo
449700960318     C                   endif
449800951122     C*
449900951122     C* Valorizzazione campi univoci testate
450000951122     C*
450100951122     C                   CLEAR                   C086T1
450200951122     C                   MOVEL     KNSIF         NOMSIF
450300951122     C                   MOVEL     XSCDSI        NOMDIT
450400951122     C                   MOVEL     DSPGM         NOMPGM
450500951109     C*
450600951109     C* visualizzo il sfl troncato
450700951109     C                   Eval      c1mode=*on
450800951113     C*
450900951113     C* il sottotitollo è la decodifica della situazione partite
451000951113     C* Controllo situazione partite
451100951113     C                   MOVE      *BLANK        T1tit
451200951113     C                   RESET                   X07DS
451300951113     C                   MOVE      '1'           X07RIC
451400951113     C                   MOVEL     'ANDIZ000'    X07TRC
451500951113     C                   MOVEL     'DSPSITPAR'   X07CAM
451600951113     C                   MOVEL     sitpar086     X07VAL
451700951113     C                   MOVE      *OFF          X07ALL
451800960924     C                   CALLB     'X07VALR'
451900951113     C                   PARM                    X07DS
452000040308     C     X07ERR        IFEQ      '0'
452100951113     C                   MOVEL     x07des        t1tit
452200951114     C                   MOVEl     t1tit         £STR            132
452300951114     C                   MOVE      30            £LSTRC            3 0
452400951114      * Routine di centratura
452500951114      *
452600951114     C     ' '           CHECKR    £STR          £LEN              3 0
452700951114     C     £LSTRC        SUB       £LEN          £LEN
452800951114     C                   DIV       2             £LEN
452900951114     C     ' '           CAT(P)    £STR:£LEN     £STRC           132
453000951114     C                   MOVEL     £strc         t1tit
453100951114      *
453200040308     C                   Endif
453300951109     C*
453400951109     C* Reperimento dati moneta di conto
453500951109     C                   Eval      WDivisa =  XscDiv
453600951109     C                   Exsr      RepDivisa
453700970521     C                   Eval      NrDec   = X01NrDec
453800951108     C*
453900951108     C* Definisco ordinamento lettura
454000951108     C*
454100950410     C                   EXSR      DEFORD
454200951108     C*
454300951110     C* Azzero attributi bottoni a livello di conto
454400951110     C*
454500951110     C                   MOVEL     X'A0'         zscadutoa
454600951110     C                   MOVEL     X'A0'         zascaderea
454700951110     C                   MOVEL     X'A0'         zrischioa
454800951110     C                   MOVEL     X'A0'         zportafoga
454900951110     C                   MOVEL     X'A0'         zinsrita
455000951110     C*
455100951113     C* Definisco caricamento subfile
455200951113     C* Se il saldo è già stato calcolato, il caricamento è a pagina e
455300951113     c* ho già anche i flag di presenza a totale di conto
455400951108     C                   if        calcsal086 = *on
455500951108     C                   EVAL      $carpag = *on
455600951108     C                   EVAL      SFLPAG = 8
455700951109      *
455800951109      * Call routine editazione x importo in moneta di conto
455900951109     C                   Z-add     Saldo086      WImporto
456000951110     C                   Z-add     NrDec         WNrDec
456100951109     C                   Eval      $LenFld = %Size(Z1Saldo)
456200951110     c* Dare allineato a sinistra
456300961009     C                   if        Segno086 = xscdare
456400951110     C                   sub       5             $lenFld
456500951110     C                   endif
456600951109     C                   EXSR      Edit
456700951109     C                   Eval      Z1Saldo = $RcvrVar
456800951109     C                   eval      z1segno = segno086
456900960307      *
457000960307      * Call routine editazione x importo in divisa
457100960307     c                   setoff                                       17
457200960307     c                   if        divisa086 <> *blanks and divsel086 = 'I'
457300960307     c                             and divisa086 <> xscdiv
457400960307     c                   seton                                        17
457500960307     C                   EVAL      Wdivisa = divisa086
457600960307     c                   exsr      Repdivisa
457700960307     C                   Eval      NrDecD = X01NrDec
457800960307     C                   Z-add     SaldoD086     WImporto
457900960307     C                   Z-add     NrDecD        WNrDec
458000960307     C                   Eval      $LenFld = %Size(Z1SaldoDiv)
458100960307     c* Dare allineato a sinistra
458200961009     C                   if        Segno086 = xscdare
458300960307     C                   sub       5             $lenFld
458400960307     C                   endif
458500960307     C                   EXSR      Edit
458600960307     C                   Eval      Z1SaldoDiv= $RcvrVar
458700970421     C                   Eval      Z1SegnoDiv= SegnoD086
458800960307     C                   eval      z1divisa = Divisa086
458900960307     c                   endif
459000960307     C*
459100951110     C     scaduto086    IFEQ      *on
459200060303D1948C****               MOVEL     PRESente      zscadutoa
459300060303D1948C                   MOVEL     PRESERED      zscadutoa
459400030922D1452X** ex D1378 ****** if        WebFacingOn = 1
459500030922D1452X** ex D1378 ****** move      '*'           Z1Scaduto
459600030922D1452X** ex D1378 ****** endif
459700951110     C                   ENDIF
459800951110     C     ascader086    IFEQ      *on
459900951110     C                   MOVEL     PRESente      zascaderea
460000030922D1452X** ex D1378 ****** if        WebFacingOn = 1
460100030922D1452X** ex D1378 ****** move      '*'           Z1AScadere
460200030922D1452X** ex D1378 ****** endif
460300951110     C                   ENDIF
460400951110     C     rischio086    IFEQ      *on
460500060303D1948C***                MOVEL     PRESente      zrischioa
460600060303D1948C                   MOVEL     PRESePNK      zrischioa
460700030922D1452X** ex D1378 ****** if        WebFacingOn = 1
460800030922D1452X** ex D1378 ****** move      '*'           Z1Rischio
460900030922D1452X** ex D1378 ****** endif
461000951110     C                   ENDIF
461100951110     C     portafo086    IFEQ      *on
461200951110     C                   MOVEL     PRESente      zportafoga
461300030922D1452X** ex D1378 ****** if        WebFacingOn = 1
461400030922D1452X** ex D1378 ****** move      '*'           Z1Portaf
461500030922D1452X** ex D1378 ****** endif
461600951110     C                   ENDIF
461700951110     C     insrit086     IFEQ      *on
461800060303D1948C***                MOVEL     PRESente      zinsrita
461900060303D1948C                   MOVEL     PRESERED      zinsrita
462000030922D1452X** ex D1378 ****** if        WebFacingOn = 1
462100030922D1452X** ex D1378 ****** move      '*'           Z1InsRit
462200030922D1452X** ex D1378 ****** endif
462300951110     C                   ENDIF
462400951109     C*
462500951108     C                   else
462600951109     C*
462700951108     c                   eval      $carpag = *off
462800951108     C                   EVAL      SFLPAG = 9999
462900951109     C                   eval      z1saldo = *blanks
463000951109     C                   eval      z1segno = ' '
463100970422     C                   eval      z1saldodiv = *blanks
463200970422     C                   eval      z1segnodiv = ' '
463300951108     C                   endif
463400951108     C*
463500951108     C* Variabili appoggio
463600951108     C*
463700951108     C                   Z-ADD     SFLPAG        WPAG
463800951108     C                   CLEAR                   W1POS
463900951110     C* Salvo divisa in entrata
464000951110     C                   EVAL      SavDivVis =   DivVis086
464100940223     C*
464200940223     C                   ENDSR
464300981006     C/EJECT
464400981006     C************************************************************
464500981006     C* TESTo esistenza commenti per il conto
464600981006     C************************************************************
464700981006     C     TSTTCM        BEGSR
464800981006     C*
464900981006     C                   RESET                   A724ds
465000981006     C                   MOVEL     '01'          OPZ724
465100981006     C                   MOVEL     xscsoc        SOC724
465200981006     C                   MOVEL     *blanks       UNI724
465300981006     C                   SELECT
465400981007     C     Snatura086    wheneq    'C'
465500981006     C                   MOVEL     '700'         TPA724
465600981006     C                   MOVEL     c1KSC         KY1724
465700981006     C                   MOVEL     '000'         KY3724
465800981007     C     SNATURA086    wheneq    'F'
465900981006     C                   MOVEL     '001'         TPA724
466000981006     C                   MOVEL     c1KSC         KY1724
466100981006     C                   OTHER
466200981006     C                   MOVEL     'PDC'         TPA724
466300981006     C                   MOVEL     c1KCC         KY1724
466400981006     C                   MOVEL     c1KSC         KY2724
466500981006     C                   endsl
466600981006     C                   MOVEL     Wdate         DTS724
466700981006     C                   MOVEL     A724ds        KPJBU
466800981006     C                   CALL      'ANA724R'
466900981006     C                   PARM                    KPJBA
467000981006     C                   MOVEL     KPJBU         a724ds
467100981006     C*
467200981006     C                   ENDSR
467300951109     C/EJECT
467400070607c2028C************************************************************
467500070607     C* TESTa esistenza impegni per il conto
467600070607c2028C************************************************************
467700070607     C     TstRecCred    BEGSR
467800070607     c*
467900090216c2119C                   move      *off          ContenzCli
468000090216c2119c                   clear                   contenzkcc
468100090216     c*
468200070607     c                   reset                   NDCL99DS
468300070607     c                   eval      societal99=xscsoc
468400070608     c                   eval      unitaL99  =unita086
468500070607     c                   eval      ctbl99=ctb086
468600070607     c                   eval      kccl99=kcc086
468700070607     c                   eval      kscl99=ksc086
468800070608     c                   eval      SolleciL99=*off
468900070607     c                   eval      ImpegniL99=*on
469000090216c2119x***                eval      ContenzL99=*off
469100090216c2119c                   eval      ContenzL99=*on
469200100420c2156c                   eval      AttivitL99=*on
469300070608     c                   eval      RatingL99 =*off
469400070607     c                   eval      BlocchiL99=*off
469500070607     c                   eval      UltimoSL99=*off
469600070607     c                   eval      kpjbu=ndcl99ds
469700070607     C*
469800070607     c                   CALL      'NDCL99R'
469900070607     C                   parm                    kpjba
470000070607     c                   eval      ndcl99ds = kpjbu
470100090216     c*
470200090216c2119C* memorizzo la presenza  contenzioso per cliente e il suo kcc contenz
470300090216  "  c                   if        Contenzl99=*on
470400090216  "  C                   MOVE      ContenzL99    ContenzCli        1
470500090216  "  C                   move      kccl99        ContenzKcc        6
470600090216c2119c                   endif
470700070607     C*
470800070607c2028C                   ENDSR
470900951109     C************************************************************
471000951109     C* Reperisco dati divisa
471100951109     C************************************************************
471200951109     C     RepDivisa     BEGSR
471300951109     C*
471400951109     C* Divisa
471500951109     C                   reset                   X01CambiDs
471600951109     C                   MOVE      xscsoc        X01societa
471700951109     C                   MOVEL     WDivisa       X01divisa
471800951109     C                   MOVEL     '1'           X01flgcam
471900951109     C                   MOVE      Wdate         x01dtRifCa
472000951109     C                   CALL      'X01CAMBI'
472100951109     C                   PARM                    X01CambiDs
472200951109     C*
472300951109     C                   ENDSR
472400951109     c/EJECT
472500951109     C************************************************************
472600951109     C* Editazione di un numero
472700951109     C************************************************************
472800951109     C     Edit          Begsr
472900951109      *
473000951109     C                   Select
473100951110     C                   When      WNrDec = 0
473200951110     C                   Z-add     WImporto      $SrcVar
473300951110     C                   When      WNrDec = 1
473400951110     C     WImporto      Mult      10            $SrcVar
473500951110     C                   When      WNrDec = 2
473600951110     C     WImporto      Mult      100           $SrcVar
473700951109     C                   Other
473800951110     C     WImporto      Mult      1000          $SrcVar
473900951109     C                   Endsl
474000951109     C*
474100951110     C                   Eval      $SrcVarDe = WNrDec
474200951109     C*
474300951109     C                   CallB     'X23EDITR'
474400951109     C                   Parm                    $SrcVar
474500951109     C                   Parm                    $SrcVarPr
474600951109     C                   Parm                    $SrcVarDe
474700951109     C                   Parm                    $EdtCode
474800951109     C                   Parm                    $LenFld
474900951109     C                   Parm      *blanks       $RcvrVar
475000951109     C                   Parm                    $Err
475100951109      *
475200951109     C                   ENDSR
475300951109     C*
475400951108     C/EJECT
475500951108     C************************************************************
475600951108     C* DEFINIZIONE DELL'ORDINAMENTO
475700951108     C************************************************************
475800951108     C     DEFORD        BEGSR
475900951108     C*
476000951108     C* definisco l'ordinamento della lista a seconda dei valori
476100951108     C* impostati nel video di parzializzazione
476200951108     C*
476300951108     C                   SELECT
476400951108    >C     ORD086        WHENEQ    'D'
476500951108     C                   Z-ADD     1             $ORD
476600951108    >C     ORD086        WHENeq    'A'
476700951108     C                   Z-ADD     2             $ORD
476800951108     C                   ENDSL
476900951108     C*
477000951108     C                   ENDSR
477100960709     C/EJECT
477200960709     C************************************************************
477300960709     C* REPERIMENTO DATI SOCIETA'
477400960709     C************************************************************
477500960709     C     REPSOC        BEGSR
477600960709     C*
477700960709     C                   CALLB     'XSOC'
477800960709     C                   PARM                    TIPXSC            6
477900960709     C                   PARM                    SOCXSC            3
478000960709     C                   PARM                    CDSXSC            9 0
478100960709     C                   PARM                    MODXSC            3
478200960709     C                   PARM      *BLANKS       RTNXSC            1
478300960709     C                   PARM                    XSOCDS
478400960709     C                   PARM                    KPJBA
478500960709     C*
478600960709     C                   ENDSR
478700941124     C/EJECT
478800941124     C************************************************************
478900941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
479000941124     C************************************************************
479100941124     C     REPAUT        BEGSR
479200941124     C*
479300941124     C                   CLEAR                   AUTDS
479400941124     C*
479500941124     C                   MOVEL     XSCSOC        XCASOC
479600941124     C                   MOVEL     XSCDUN        XCAUNI
479700941124     C                   Z-ADD     0             XCAGRP
479800941124     C                   MOVEL     KNMUS         XCAPRF
479900951114    >C                   MOVEL     'CONECC'      XCAFNC
480000941124    >C                   MOVEL     *ZERO         XCAVFU
480100010912    >C                   MOVE      'CK'          XCATCK
480200941124    >C                   MOVE      '1'           XCANAU
480300941124     C*
480400950522     C                   CALLB     'XCHKAUT'
480500941124     C                   PARM                    AUTDS
480600941124     C*
480700941124     C* errori/non abilitazione
480800941124     C     XCARTN        IFNE      0
480900941124     C                   EXSR      ENDPGM
481000941124     C                   ELSE
481100951108    >C                   MOVEL     XCADDS        CONECCDS
481200941124     C                   ENDIF
481300941124     C*
481400941124     C                   ENDSR
481500940223     C/EJECT
481600990803C1213C************************************************************
481700990803  "  C* REPERIMENTO AUTORIZZAZIONI UTENTE per Contenzioso legale
481800990803  "  C************************************************************
481900990803  "  C     RepAutLeg     BEGSR
482000990803  "  C*
482100990803  "  C                   CLEAR                   AUTDS
482200990803  "  C*
482300990803  "  C                   MOVEL     XSCSOC        XCASOC
482400990803  "  C                   MOVEL     XSCDUN        XCAUNI
482500990803  "  C                   Z-ADD     0             XCAGRP
482600990803  "  C                   MOVEL     KNMUS         XCAPRF
482700990803  "  C                   MOVEL     'CONPPA'      XCAFNC
482800990803  "  C                   MOVEL     *ZERO         XCAVFU
482900990803  "  C                   MOVE      'CK'          XCATCK
483000990803  "  C                   MOVE      '1'           XCANAU
483100990803  "  C*
483200990803  "  C                   CALLB     'XCHKAUT'
483300990803  "  C                   PARM                    AUTDS
483400990803  "  C*
483500990803  "  C* errori/non abilitazione
483600990803  "  C     XCARTN        IFNE      0
483700990803  "  C                   EXSR      ENDPGM
483800990803  "  C                   ELSE
483900990803  "  C                   MOVEL     XCADDS        CONPPADS
484000990803  "  C                   ENDIF
484100990803  "  C*
484200990803  "  C                   ENDSR
484300990803C1213C/EJECT
484400940131     C************************************************************
484500940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
484600940131     C************************************************************
484700940225     C     DEFOPZ        BEGSR
484800940131     C*
484900940225     C* Questa routine deve valorizzare la schiera $FL1
485000940225     C* con S o N a seconda della utilizzabilità o meno
485100940225     C* dell'opzione correlata
485200940225     C*
485300950301     C* Inizialmente si considerano tutte le opzioni come valide
485400940317     C     1             DO        $DIMOP        X
485500020321C1530C* Imposta tutti i flg, delle opzioni non a *blank, a 'S'
485600020321C1530C                   If        $TX1(X)  <> *Blank
485700940225     C                   MOVE      'S'           $FL1(X)
485800020321C1530C                   Else
485900020321  "  C                   Leave
486000020321C1530C                   EndIf
486100941027     C                   ENDDO
486200940715     C*
486300951109     C* Questa routine deve valorizzare la schiera $FC1/$FR1
486400941018     C* con 1 o 0 a seconda della utilizzabilità o meno
486500940715     C* del tasto funzione correlato
486600940715     C*
486700940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
486800940715     C* di compilazione
486900940715     C                   RESET                   $FC1
487000940715     C                   RESET                   $FR1
487100940225     C*
487200940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
487300940225     C* invalido alcune opzioni
487400940225     C                   SELECT
487500940225     C* Se pgm richiamato in ricerca disattivo :
487600951108    >C     OPZ086        WHENEQ    '01'
487700940603     C* F3=Fine
487800940603     C                   Z-ADD     3             X
487900941216     C                   MOVE      '0'           $FC1(X)
488000950116     C*
488100951114     C* Se pgm richiamato in gestione       disattivo :
488200951114    >C     OPZ086        WHENEQ    '02'
488300940225     C* 1=Selezione
488400940225     C                   Z-ADD     1             X
488500940225     C     01            LOOKUP    $OP1(X)                                21
488600940225     C     *IN21         IFEQ      *ON
488700940225     C                   MOVE      'N'           $FL1(X)
488800940225     C                   ENDIF
488900960327     c* 10=scelta con importo
489000960327     C                   Z-ADD     1             X
489100960327     C     10            LOOKUP    $OP1(X)                                21
489200960327     C     *IN21         IFEQ      *ON
489300960327     C                   MOVE      'N'           $FL1(X)
489400960327     C                   ENDIF
489500940225     C*
489600980616     C* Se pgm richiamato in interrogazione disattivo :
489700980616    >C     OPZ086        WHENEQ    '05'
489800980616     C* 1=Selezione
489900980616     C                   Z-ADD     1             X
490000980616     C     01            LOOKUP    $OP1(X)                                21
490100980616     C     *IN21         IFEQ      *ON
490200980616     C                   MOVE      'N'           $FL1(X)
490300980616     C                   ENDIF
490400990115     C* 10=Selezione con importi
490500990115     C                   Z-ADD     1             X
490600990115     C     10            LOOKUP    $OP1(X)                                21
490700990115     C     *IN21         IFEQ      *ON
490800990115     C                   MOVE      'N'           $FL1(X)
490900990115     C                   ENDIF
491000980616     C*
491100950301     C* In tutti gli altri casi, disattivo subito :
491200941228     C                   OTHER
491300941228     C*
491400940225     C                   ENDSL
491500960327     c*
491600960327     c* disattivo scelta con importo se non richiamato in scelta multipla
491700960327     c* 10=Scelta con importo
491800960327     C                   IF        multip086 <>*on
491900960327     C                   Z-ADD     1             X
492000960327     C     10            LOOKUP    $OP1(X)                                21
492100960327     C     *IN21         IFEQ      *ON
492200960327     C                   MOVE      'N'           $FL1(X)
492300960327     C                   ENDIF
492400960327     C                   ENDIF
492500951115     c*
492600951115     C* Se non cliente disattivo opzione solleciti
492700951115     C* 42=solleciti
492800951115     C                   if        snatura086 <> 'C'
492900951115     C                   Z-ADD     1             X
493000951115     C     42            LOOKUP    $OP1(X)                                21
493100951115     C     *IN21         IFEQ      *ON
493200951115     C                   MOVE      'N'           $FL1(X)
493300951115     C                   ENDIF
493400951115     C                   endif
493500951115     c*
493600951115     C* Se non percipiente disattivo ritenute
493700951115     C* 40=ritenute
493800951115     C                   if        percipi086 <> *on
493900951115     C                   Z-ADD     1             X
494000951115     C     40            LOOKUP    $OP1(X)                                21
494100951115     C     *IN21         IFEQ      *ON
494200951115     C                   MOVE      'N'           $FL1(X)
494300951115     C                   ENDIF
494400951115     C                   endif
494500951123     c*
494600070621c2034x* Se non fornitore disattivo opzione bonifici
494700070621  "  x* 41=bonifici
494800070621  "  C* Se non gestito a soggetti disattivo opzione bonifici(bonifici
494900070621  "  c* spot)
495000070621  "  C* 41=bonifici
495100070621  "  x*********          if        snatura086 <> 'F'
495200070621c2034c                   if        kccsogg002 <> *on
495300951123     C                   Z-ADD     1             X
495400951123     C     41            LOOKUP    $OP1(X)                                21
495500951123     C     *IN21         IFEQ      *ON
495600951123     C                   MOVE      'N'           $FL1(X)
495700951123     C                   ENDIF
495800951123     C                   endif
495900951123     C* Se non fornitore e neanche cliente disattivo opzione mix
496000951123     C* 32=mix
496100951123     C                   if        snatura086 <> 'F' and snatura086 <> 'C'
496200951123     C                   Z-ADD     1             X
496300951123     C     32            LOOKUP    $OP1(X)                                21
496400951123     C     *IN21         IFEQ      *ON
496500951123     C                   MOVE      'N'           $FL1(X)
496600951123     C                   ENDIF
496700981006     c* e opzione 33=rate
496800981006     C                   Z-ADD     1             X
496900981006     C     33            LOOKUP    $OP1(X)                                21
497000981006     C     *IN21         IFEQ      *ON
497100981006     C                   MOVE      'N'           $FL1(X)
497200981006     C                   ENDIF
497300010119C1460c* e opzione 60=Ordini
497400010112 "   C                   Z-ADD     1             X
497500010115 "   C     60            LOOKUP    $OP1(X)                                21
497600010112 "   C     *IN21         IFEQ      *ON
497700010112 "   C                   MOVE      'N'           $FL1(X)
497800010119C1460C                   ENDIF
497900951123     C                   endif
498000010911C1511 *
498100010911  "   * disabilito il 46=Conversione divisa di partita
498200010911  "   * se non c'è l'abilitazione in EURCNV
498300010911  "  C                   exsr      CtrlAutCnv
498400010911  "  C                   if        AbiDivPa <> *On
498500010911  "   * 46=Conversione divisa di partita
498600010911  "  C                   Z-ADD     1             X
498700010911  "  C     46            LOOKUP    $OP1(X)                                21
498800010911  "  C     *IN21         IFEQ      *ON
498900010911  "  C                   MOVE      'N'           $FL1(X)
499000010911  "  C                   ENDIF
499100010911  "  C                   endif
499200010911C1511 *
499300990803C1213 *
499400990803  "  C* 50=Contenzioso
499500990803  "  C* Se non cliente disattivo opzione
499600990803  "  C                   If        SNatura086 <> 'C'
499700990803  "  C                   Z-ADD     1             X
499800990803  "  C     50            LOOKUP    $OP1(X)                                21
499900990803  "  C     *IN21         IFEQ      *ON
500000990803  "  C                   MOVE      'N'           $FL1(X)
500100990803  "  C                   ENDIF
500200990803  "  C                   EndIf
500300990803  "  C* Se contabilità non CG disattivo opzione
500400990803  "  C                   If        Ctb086 <> 'CG'
500500990803  "  C                   Z-ADD     1             X
500600990803  "  C     50            LOOKUP    $OP1(X)                                21
500700990803  "  C     *IN21         IFEQ      *ON
500800990803  "  C                   MOVE      'N'           $FL1(X)
500900990803  "  C                   ENDIF
501000990803  "  C                   EndIf
501100990907  "  C* Se utente non autorizzato disattivo opzione
501200990907  "  C                   If        ImmLeg <> *On
501300990907  "  C                   Z-ADD     1             X
501400990907  "  C     50            LOOKUP    $OP1(X)                                21
501500990907  "  C     *IN21         IFEQ      *ON
501600990907  "  C                   MOVE      'N'           $FL1(X)
501700990907  "  C                   ENDIF
501800990907  "  C                   EndIf
501900990803C1213C*
502000031120D1498X* se non presente nostro gestionale, disattivo opzione ordini
502100031120 "   X* Per ora è presente solo la visualizzazione ordini'distribuzione
502200031120 "   X*                  IF        xscGest <>'1' and xscgest <> '2'
502300031120 "   X*                  Z-ADD     1             X
502400031120 "   X*    60            LOOKUP    $OP1(X)                                21
502500031120 "   X*    *IN21         IFEQ      *ON
502600031120 "   X*                  MOVE      'N'           $FL1(X)
502700031120 "   X*                  ENDIF
502800031120D1498X*                  ENDIF
502900020118C1530C*
503000020118  "  C* Se Laguna non è presente o non è attivo per la società
503100020118  "  C                   IF        XscLaguna <> '1'
503200020118  "  C* oppure la sottonatura del conto non è né cliente né fornitore
503300020118  "  C                             or (SNatura086 <> 'C' and
503400020118  "  C                                 SNatura086 <> 'F')
503500020118  "  C* disattivo l'opzione 80
503600020118  "  C                   Z-ADD     1             X
503700020118  "  C     80            LOOKUP    $OP1(X)                                21
503800020118  "  C     *IN21         IFEQ      *ON
503900020118  "  C                   MOVE      'N'           $FL1(X)
504000020118  "  C                   ENDIF
504100020118C1530C                   ENDIF
504101110414c2185C* 34=Estensione nominativi partite
504102110414  "  C                   if        tecnico002 <>'2'
504103110414  "  C                   Z-ADD     1             X
504104110414  "  C     34            LOOKUP    $OP1(X)                                21
504105110414  "  C     *IN21         IFEQ      *ON
504106110414  "  C                   MOVE      'N'           $FL1(X)
504107110414  "  C                   ENDIF
504108110414C2185C                   ENDIF
504200940225     C*
504300940225     C                   ENDSR
504400940225     C/EJECT
504500010911C1511 ************************************************************
504600010911  "   * Controllo autorizzazione alla conversione euro partita
504700010911  "   ************************************************************
504800010911  "  C     CtrlAutCnv    Begsr
504900010912  "   *
505000010912  "  C                   movel     AutDS         SavAUTDS
505100010912  "  C                   clear                   AutDS
505200010912  "  C                   clear                   EurCnvDS
505300010911  "   *
505400010911  "  C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
505500010911  "  C                   MOVEL     XSCDUN        XCAUNI                         UNITA
505600010911  "  C                   Z-ADD     0             XCAGRP                         GRUPPO
505700010911  "  C                   MOVEL     KNMUS         XCAPRF                         PROFILO
505800010912  "  C                   MOVEL     'EURCNV'      XCAFNC                         FUNZIONE
505900010911  "  C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
506000010911  "  C                   MOVE      'CK'          XCATCK                         TIPO CTRL
506100010912  "   * se l'abilitazione non esiste in tab.G23, oppure non esiste
506200010912  "   * per nessun profilo,  xchkaut non mi deve bloccare
506300010912  "   * con la window e non farmi entrare nell'E/C
506400010912  "  C**************     MOVE      '1'           XCANAU                         RICH.XNOAUT
506500010911  "  C                   CALLB     'XCHKAUT'
506600010911  "  C                   PARM                    AUTDS
506700010911  "   * errori/non abilitazione
506800010911  "  C                   if        XCaRtn = 0
506900010911  "  C                   MOVEL     XCADDS        EURCNVDS
507000010911  "  C                   endif
507100010911  "   *
507200010911  "  C                   movel     SavAutDS      AUTDS
507300010911  "   *
507400010911C1511C     XCtrlAutCn    Endsr
507500940225     C************************************************************
507600940225     C* REPERIMENTO OPZIONI
507700940225     C************************************************************
507800940225     C     REPOPZ        BEGSR
507900940225     C*
508000940314     C     1             DO        $DIMOP        X
508100940128     C*
508200940223     C     $MS1(X)       IFNE      *BLANK
508300940128     C*
508400940223     C                   MOVEL     $MS1(X)       IDMSG
508500950522     C                   CALLB     'XRTVM'
508600940201     C                   PARM                    IDMSG            27
508700940201     C                   PARM                    TXTMSG          100
508800940201     C                   PARM                    ERRMSG            1
508900940128     C*
509000940128     C     ERRMSG        IFNE      *ON
509100940223     C                   MOVEL     TXTMSG        $TX1(X)
509200940128     C                   ELSE
509300940223     C                   MOVEL     *ALL'?'       $TX1(X)
509400941027     C                   ENDIF
509500940128     C*
509600941027     C                   ENDIF
509700940128     C*
509800941027     C                   ENDDO
509900940128     C*
510000940128     C                   ENDSR
510100940506     C/EJECT
510200940506     C************************************************************
510300940506     C* REPERIMENTO TASTI DI FUNZIONE
510400940506     C************************************************************
510500940506     C     REPKEY        BEGSR
510600940506     C                   MOVEA     $FM1          $FM
510700950906     C                   MOVEA     $Fc1          $Fc
510800940506     C                   EXSR      REPKKK
510900940506     C                   MOVEA     $FX           $FX1
511000940919     C*
511100940919     C                   ENDSR
511200940506     C/EJECT
511300940506     C************************************************************
511400940506     C* REPERIMENTO TASTI DI FUNZIONE
511500940506     C************************************************************
511600940506     C     REPKKK        BEGSR
511700940506     C*
511800940506     C                   CLEAR                   $FX
511900940506     C     1             DO        24            X
512000940506     C*
512100940506     C     $FM(X)        IFNE      *BLANK
512200950906     C     $Fc(X)        andne     '0'
512300940506     C*
512400940506     C                   MOVEL     $FM(X)        IDMSG
512500950522     C                   CALLB     'XRTVM'
512600940506     C                   PARM                    IDMSG            27
512700940506     C                   PARM                    TXTMSG          100
512800940506     C                   PARM                    ERRMSG            1
512900940506     C*
513000940506     C     ERRMSG        IFNE      *ON
513100940506     C                   MOVEL     TXTMSG        $FX(X)
513200940506     C                   ELSE
513300940506     C                   MOVEL     *ALL' '       $FX(X)
513400941027     C                   ENDIF
513500940506     C*
513600941027     C                   ENDIF
513700940506     C*
513800941027     C                   ENDDO
513900940506     C*
514000940506     C                   ENDSR
514100940223     C/EJECT
514200940128     C************************************************************
514300950606     C* DEFINIZIONE KLIST
514400940128     C************************************************************
514500950606     C     DefKlist      BEGSR
514600940128     C*
514700940223     C* klist
514800940223     C*
514900040308     C* ndppa01L
515000040308     C     K04PPA01      KLIST
515100040308     C                   KFLD                    PPASOCIETA
515200040308     C                   KFLD                    PPActb
515300040308     C                   KFLD                    PPAKCC
515400040308     C                   KFLD                    PPAKSC
515500040308     C*
515600040308     C     K05PPA01      KLIST
515700040308     C                   KFLD                    PPASOCIETA
515800040308     C                   KFLD                    PPActb
515900040308     C                   KFLD                    PPAKCC
516000040308     C                   KFLD                    PPAKSC
516100040308     C                   KFLD                    PPADTPAR
516200040308     C* x file correlati
516300040308     C     K02PPA01      KLIST
516400040308     C                   KFLD                    PPASYS
516500040308     C                   KFLD                    PPANRASINT
516600040308     C* Ndepa01l
516700040308     C     K06EPA01      KLIST
516800040308     C                   KFLD                    EPACSI
516900040308     C                   KFLD                    EPAKCC
517000040308     C                   KFLD                    EPAKSC
517100040308     C                   KFLD                    EPADRP
517200040308     C                   KFLD                    EPANRP
517300040308     C                   KFLD                    EPASERPAR
517400040308     C* Ndmra04l
517500040308     C     K06mra04      KLIST
517600040308     C                   KFLD                    mrasocieta
517700040308     C                   KFLD                    mraKCC
517800040308     C                   KFLD                    mraKSC
517900040308     C                   KFLD                    mraDRP
518000040308     C                   KFLD                    mranrp
518100040308     C                   KFLD                    mraSERPAR
518200031118D1498X* NdReg01l  EX C1460
518300040308  "  X*    K02Reg01      KLIST
518400040308  "  X*                  KFLD                    RegSys
518500031118D1498X*                  KFLD                    RegNrasReg
518600950704     C*
518700940117     C                   ENDSR
518800940223     C/EJECT
518900010806C1511C*
519000010806C1511C* aggiunto msg OPZ0434  "46=Conversione divisa di partita"
519100941214**
519200940718PROMSG    *LIBL     OPZ0001
519300960327PROMSG    *LIBL     OPZ0168
519400951108PROMSG    *LIBL     OPZ0138
519500951108PROMSG    *LIBL     OPZ0014
519600951108PROMSG    *LIBL     OPZ0139
519700951108PROMSG    *LIBL     OPZ0111
519800981006PROMSG    *LIBL     OPZ0112
519801110414PROMSG    *LIBL     OPZ0322            ### C2185 ###
519900951108PROMSG    *LIBL     OPZ0140
520000951108PROMSG    *LIBL     OPZ0141
520100951108PROMSG    *LIBL     OPZ0142
520200990803PROMSG    *LIBL     OPZ0340
520300010112PROMSG    *LIBL     OPZ0407
520400020118PROMSG    *LIBL     OPZ0451
520500070614PROMSG    *LIBL     OPZ0434            ### C1511 ###
520600940201**
52070094022501S      <Selezione
52080096032710S      <Scelta con importo
52090095110811S      <Movimenti
52100094083114S      <Gestione commenti
52110095110820S      <Particolarità
52120095070432S      <Mix
52130098100633S      <Rate
52130111041434S      <Estensione nominativi         ### C2185 ###
52140095110840S      <Ritenute
52150095110841S      <Bonifici
52160095110842S      <Solleciti
52170001091150S      <Contenzioso                   ### C1213 ###
52180001091160S      <Ordini                        ### C1460 ###
52190002011880S      <Laguna                        ### C1530 ###
52200007061446S      <Conversione divisa di partita ### C1511 ###
522100940916** TASTI DI FUNZIONE  S1
522200940916PROMSG    *LIBL     KEY0001  01
522300080128PROMSG    *LIBL     KEY0331  02                       C2074
522400940916PROMSG    *LIBL     KEY0003  03
522500940916PROMSG    *LIBL     KEY0004  04
522600940916PROMSG    *LIBL     KEY0005  05
522700940916PROMSG    *LIBL     KEY0006  06
522800981001PROMSG    *LIBL     KEY0150  07
522900940916PROMSG    *LIBL     KEY0008  08
523000940916PROMSG    *LIBL     KEY0009  09
523100940916PROMSG    *LIBL     KEY0010  10
523200951109PROMSG    *LIBL     KEY0031  11
523300940916PROMSG    *LIBL     KEY0012  12
523400960314PROMSG    *LIBL     KEY0061  13
523500990803PROMSG    *LIBL     KEY0137  14
523600951109PROMSG    *LIBL     KEY0053  15
523700080128PROMSG    *LIBL     KEY0332  16                       c2074
523800940916PROMSG    *LIBL     KEY0017  17
523900940916PROMSG    *LIBL     KEY0018  18
524000940916PROMSG    *LIBL     KEY0019  19
524100960109PROMSG    *LIBL     KEY0116  20
524200020527PROMSG    *LIBL     KEY0246  21
524300940916PROMSG    *LIBL     KEY0022  22
524400940916PROMSG    *LIBL     KEY0023  23
524500940916PROMSG    *LIBL     KEY0024  24
524600940916**  ABILITAZIONE S1
5247009409160N  01
5248009810071N  02
5249009409161N  03
5250009409160N  04
5251009409160N  05
5252009409160N  06
5253009910141N  07
5254009511201N  08
5255009511081N  09
5256009507040N  10
5257009511081N  11
5258009409161N  12
5259009603141N  13
5260009908031N  14
5261009511081N  15
5262009710271N  16
5263009511080N  17
5264009511090N  18
5265009409160N  19
5266009601091N  20
5267000205271N  21
5268009409160N  22
5269009509131N  23   Impostare a '1' se si utilizzano opzioni
5270009509131N  24   Lasciare sempre impostata a '1' il valore della riga 24
527100960109**
527200960109PROMSG    *LIBL     COS0141
527300960111PROMSG    *LIBL     COS0142
527400960111PROMSG    *LIBL     COS0143
