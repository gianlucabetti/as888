000100981127     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200950913     H* CVTOPT(*DATETIME)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940117      *
000500950301      *            ANAGRAFICA PERCIPIENTI
000600950301      *           -----------------------
000700940223      *
000800940223      *
000900940223      * DOCUMENTI MICROANALISI :
001000940223      *
001100940223      * NOTE TECNICHE :
001200001122B0298 *  - questo pgm e altri (204, 205, 206) avrebbero chiamato
001300001122  "   *    XCHKAUT con ANGPDC ma, dalla notte dei tempi, ricevevano o
001400001122  "   *    ANGCLI o ANGFOR dentro DSAUT. Essendo DSAUT diverso da
001500001122  "   *    blank, XCHKAUT non veniva mai chiamato mentre venivano usati
001600001122  "   *    i dati ricevuti che, per simmetria, si adattavano nei campi
001700001122  "   *    di ANGPDC. Per questo i Ns clienti hanno sempre creduto che
001800001122  "   *    i dati contabili usassero le stesse autorizzazioni di
001900001122  "   *    clienti e fornitori.
002000001122  "   *    Col task B0244, che ha tolto dal chiamante ANA330R il
002100001122  "   *    parametro DSAUT nelle chiamate ai suddetti pgm, è partita
002200001122  "   *    la gestione delle autorizzazioni con ANGPDC: sollevando un
002300001122  "   *    putiferio tra chi aveva suddiviso nettamente il piano dei
002400001122  "   *    conti dai dati gestionali.
002500001122  "   *    La nuova scelta è stata di implementare le autorizzazioni
002600001122  "   *    ANGCLI e FOR con flags specifici per i dati contabili.
002700001122  "   *    Siccome questa modifica verrà fatta anche come PTF veloce
002800001122  "   *    per il rel.3.2, per evitare che si debbano impostare
002900001122  "   *    obbligatoriamente i nuovi flags e per evitare il secondo
003000001122  "   *    sconvolgimento senza documentazione, viene testato se il
003100001122  "   *    flag è blank (in pratica non impostato), nel qual caso viene
003200001122B0298 *    usato il corrispondente per i dati gestionali.
003300940223      *
003400940223      *
003500940223      * PERSONALIZZAZIONI
003600940223      *
003700981126      * C0766 : Gestione dei campi aggiuntivi:
003800981116      *         D2PINPS Codice posizione INPS
003900981116      *         D2SAT   Sequenza Altri Tributi
004000940223      *
004100940223      *
004200940223      *               MAPPA INDICATORI STD
004300940223      *
004400940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
004500950216      *  03           COPIA
004600941201      *  04           PROTECT TUTTI CAMPI CHIAVE TRANNE DATA ANN
004700941201      *               SE OPZ=ANNULLAMENTO OPPURE RCD GIA' ANNULLATO
004800950213      *  08           GESTIONE CAPOFILA O SOCIETA' GESTITA A SOCIETA'
004900950214      *  11           GESTIONE CAPOCONTI
005000950214      *  12           GESTIONE SOTTOCONTI
005100950214      *  13           GESTIONE CLIENTI
005200950214      *  14           GESTIONE FORNITORI
005300940223      *  21           GENERICO OPERAZIONI I/O
005400940223      *  22           GENERICO ERRORE OPERAZIONI I/O
005500940223      *  30           SFLDSP
005600940223      * N31           SFLCLR
005700940128      *  31           SFLDSPCTL
005800940128      *  32           SFLNXTCHG
005900940128      *  33           SFLEND
006000940128      *  39           OF PRTF
006100940315      *  40 <---> 49  DSPATR ERRORI SU SFL
006200950913      *  Specificare l'uso dei singoli indicatori
006300940315      *  50 <---> 98  ERRORI SU VIDEO
006400950913      *  Specificare l'uso dei singoli indicatori
006500940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
006600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
006700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
006800940128      *  99           INDIC. GENERALE DI ERRORE
006900950914      *----------------------------------------------------*
007000980714A0428F***ANAPE01L  UF A E           K DISK    COMMIT
007100980714  "  F***                                     USROPN
007200980714A0428FANAPE01L  UF A E           K DISK
007300950913     FANPDC01L  IF   E           K DISK
007400951005     FANRCO01L  UF   E           K DISK
007500950913     FANIND01L  IF   E           K DISK
007600950913     FANSOG01L  IF   E           K DISK
007700950913     FANTRI01L  IF   E           K DISK
007800000512A0778X***AnTxt03L  IF   E           K DISK
007900950913      *------------
008000950228     FANA207D   CF   E             WORKSTN
008100950228     F                                     RENAME(A207T1:FMTT1)
008200950228     F                                     RENAME(A207D1:FMTD1)
008300950228     F                                     RENAME(A207Z1:FMTZ1)
008400950228     F                                     RENAME(A207D2:FMTD2)
008500950228     F                                     RENAME(A207Z2:FMTZ2)
008600940201     F                                     INFDS(DSFMT)
008700950913      *----------------------------------------------------*
008800950213     D $COS            S             27    DIM(1) CTDATA PERRCD(1)              Costanti pgm
008900950405     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
009000950405     D $ID             S             27    DIM(5) ALT($OZ)
009100950913      * Schiere per la gestione dei tasti di funzione
009200940506     D $FM             S             27    DIM(24)
009300940506     D $FX             S             50    DIM(24)
009400940506     D $FC             S              1    DIM(24)
009500940506     D $FR             S              1    DIM(24)
009600950913      * Video D1/D2
009700940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
009800940506     D $FX1            S             50    DIM(24) ALT($FM1)
009900940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
010000940506     D $FR1            S              1    DIM(24) ALT($FC1)
010100950913      *-------------
010200950913      * Passaggio Parametri
010300940127     D KPJBA         E DS
010400970807     D*-------------
010500970807     D* Ricerca tipo testo
010600970807     D Ana720Ds      E DS
010700970807    >D  Ret720       E                     Inz('0')
010800970807    >D  Opr720       E                     Inz('0')
010900070119    >D  Err720       E                     Inz('0')
011000070119C2002D*-------------
011100070122  "  D* situazione vincoli ritenute d'acconto
011200070122  "  D XCHKMRADS     E DS                  INZ
011300070129C2002D*-------------
011400000512A0778D*-------------
011500000518  "  D* Variabili per chiamata a X72TXT
011600000518  "  D XEsito          S              1A
011700000518  "  D XOpe            S              1A
011800000518  "  D XSocieta        S              3A
011900000518  "  D XTpTesto        S              2A
012000000518  "  D XUnita          S              8A
012100000518  "  D XLingua         S              1A
012200000523  "  D XClaUte         S              2A
012300000523  "  D XPosRig         S              1A
012400000518  "  D XNrRiga         S              9P 0
012500000518  "  D XRibat          S              1A
012600000518  "  D XTesRig         S             70A
012700000518  "  D XNrChiavi       S              1  0
012800000518A0778D XUsoUnita       S              1A
012900970807     D*-------------
013000970807     D* Passaggio Parametri
013100950913     D a095ds        E DS                  EXTNAME(ANA095DS)
013200970807     D*-------------
013300970807     D* DS Esterna gestione messaggi per COMIT
013400940318     D XMSGDS        E DS
013500940318     D  STS          E                     EXTFLD(MSGSTS)
013600940721     D                                     DIM(333)
013700940318     D  JBA          E                     EXTFLD(MSGJBA)
013800940721     D                                     DIM(502)
013900970807     D*-------------
014000970807     D* Reperimento Autorizzazioni
014100941214     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
014200950913      *-------------
014300950913      * Maschera dati autorizzazioni
014400950213     D GPDCDS        E DS                  EXTNAME(ANGPDCDS)
014500001122B0298D ANGFORDS      E DS
014600950913      *-------------
014700950913      * Ricerca altri indirizzi
014800950331     D A045DS        E DS                  EXTNAME(ANA045DS) INZ
014900950227     D  RET045       E                     INZ('0')
015000950227     D  OPR045       E                     INZ('0')
015100950227     D  ERR045       E                     INZ('0')
015200941202     D*-------------
015300950913      * Ricezione Parametri
015400950228     D A207DS        E DS                  EXTNAME(ANA207DS)
015500950913      *-------------
015600950913      *DS per passaggio dati pgm User Exit
015700950331     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) INZ
015800950913      *-------------
015900950913      * Richiamo a XATB
016000950130     D XATBDS        E DS                  INZ
016100950913      *-------------
016200950913      * Parametri per ricerca codice ANATB00F
016300941215     D XTABDS        E DS                  INZ
016400950301     D*-------------
016500950301     D* DS esterna province
016600950301     D TABC17        E DS                  EXTNAME(ANGC17DS) INZ
016700950727     D* DS esterna PER TABELLA NATURA GIURIDICA
016800950727     D TABG16        E DS                  EXTNAME(ANGG16DS) INZ
016900950301     D*-------------
017000950301     D* DS esterna stato
017100950301     D TABG12        E DS                  EXTNAME(ANGG12DS) INZ
017200981125C0766D*-------------
017300981125C0766D* DS esterna sequenza altri tributi
017400981125C0766D TAB0CS        E DS                  EXTNAME(ANG0CSDS) INZ
017500941223     D*-------------
017600940201     D DSFMT           DS
017700940506     D  $TASTO               369    369
017800940201     D  NRG                  370    370
017900940201     D  NCL                  371    371
018000940201     D*-------------
018100950301     D*DS per tributo
018200950301     D                 DS
018300950301     D  WTRB                   1      4
018400950301     D  WVRT                   5      6
018500950301     D  WWTRB                  1      6
018600950301     D*DS per provincia
018700950301     D                 DS
018800950301     D  WSTA                   1      4
018900950301     D  WPRV                   5      6
019000950301     D  WWKEY                  1      6
019100940201     D* posizione cursore
019200940201     D CURSOR          DS
019300940223     D  RIRI                   1      2B 0 INZ
019400940201     D  R$$                    2      2
019500940223     D  COCO                   3      4B 0 INZ
019600940201     D  C$$                    4      4
019700940207     D*-------------
019800940207     D* Reperimento nome PGM
019900940207     D STATUS         SDS           333
020000940207     D  DSPGM            *PROC
020100950103     D  PARMS            *PARMS
020200950220     D*-------------
020300950220     D*  Ds per output DVFRN formato ANFRN+ANSOG+ANIND
020400001010A0840X*DVOFRN        E DS                  EXTNAME(DVOFRNDS) INZ
020500001010A0840D DVOFRN        E DS          1000    EXTNAME(DVOFRNDS) INZ
020600950220     D* Anagrafica fornitori
020700950220     D  FRNSOC       E                     EXTFLD(FRNSOCIETA)
020800950220     D  FRNUNT       E                     EXTFLD(FRNUNITA)
020900950220     D  FRNKCC       E                     EXTFLD(FRNKCC)
021000950220     D  FRNKSC       E                     EXTFLD(FRNKSC)
021100950220     D  FRNFIL       E                     EXTFLD(FRNFILIALE)
021200950220     D  FRNSOG       E                     EXTFLD(FRNSOGG)
021300950220     D  FRNTII       E                     EXTFLD(FRNTPIND)
021400950220     D  FRNCOI       E                     EXTFLD(FRNCDIND)
021500950220     D  FRNDUM       E                     EXTFLD(FRNDTULMO)
021600950220     D  FRNUTE       E                     EXTFLD(FRNUTENTE)
021700950220     D  FRNDIR       E                     EXTFLD(FRNDTINRPF)
021800950220     D  FRNDFR       E                     EXTFLD(FRNDTFIRPF)
021900950220     D  FRNDES       E                     EXTFLD(FRNDESBREV)
022000950220     D  FRNGPM       E                     EXTFLD(FRNGRUMERC)
022100950220     D  FRNSGM       E                     EXTFLD(FRNSETMERC)
022200950220     D  FRNCFA       E                     EXTFLD(FRNFORALT)
022300950220     D  FRNLIS       E                     EXTFLD(FRNLISTINO)
022400950220     D  FRNVAL       E                     EXTFLD(FRNDIVISA)
022500950220     D  FRNCDD       E                     EXTFLD(FRNCONDCON)
022600950220     D  FRNQFO       E                     EXTFLD(FRNQUALFOR)
022700950220     D  FRNVAF       E                     EXTFLD(FRNVOLAFF)
022800950220     D  FRNCIV       E                     EXTFLD(FRNCDIVA)
022900950220     D  FRNCOB       E                     EXTFLD(FRNCOLLOBB)
023000950220     D  FRNTCL       E                     EXTFLD(FRNTPCOLL)
023100950220     D  FRNFRL       E                     EXTFLD(FRNFORLIST)
023200950220     D  FRNFOF       E                     EXTFLD(FRNFORFATT)
023300950220     D  FRNPOT       E                     EXTFLD(FRNFORPOT)
023400950220     D  FRNTRP       E                     EXTFLD(FRNREPPRZL)
023500950220     D  FRNFFB       E                     EXTFLD(FRNFORBLO)
023600950220     D  FRNLSF       E                     EXTFLD(FRNLISFIL)
023700950220     D  FRNCPA       E                     EXTFLD(FRNABILPRI)
023800950220     D  FRNMDC       E                     EXTFLD(FRNMDCONS)
023900950220     D  FRNMAG       E                     EXTFLD(FRNMAG)
024000950220     D  FRNCLI       E                     EXTFLD(FRNCLICORR)
024100950220     D  FRNFRP       E                     EXTFLD(FRNFORPREC)
024200950220     D  FRNSC1       E                     EXTFLD(FRNSCOPAG)
024300950220     D  FRNSC2       E                     EXTFLD(FRNSCOPAG2)
024400950220     D  FRNIP1       E                     EXTFLD(FRNGGINPA1)
024500950220     D  FRNIP2       E                     EXTFLD(FRNGGINPA2)
024600950220     D  FRNIP3       E                     EXTFLD(FRNGGINPA3)
024700950220     D  FRNTRA       E                     EXTFLD(FRNSESSTRA)
024800950220     D  FRNRIC       E                     EXTFLD(FRNSESSRIC)
024900950220     D  FRNCDP       E                     EXTFLD(FRNCONDPAG)
025000950220     D* Anagrafica soggetti
025100950220     D  FOGCOD       E                     EXTFLD(SOGSOGG)
025200950220     D  FOGDAT       E                     EXTFLD(SOGDTENTR)
025300950220     D  FOGDAU       E                     EXTFLD(SOGDTULMO)
025400950220     D  FOGDAI       E                     EXTFLD(SOGDTINVL)
025500950220     D  FOGDAF       E                     EXTFLD(SOGDTFIVL)
025600950220     D  FOGDAN       E                     EXTFLD(SOGDTNASC)
025700950220     D  FOGDES       E                     EXTFLD(SOGDES)
025800950220     D  FOGDE1       E                     EXTFLD(SOGDESAGG)
025900950220     D  FOGCDF       E                     EXTFLD(SOGCDFISC)
026000950220     D  FOGIVA       E                     EXTFLD(SOGPARTIVA)
026100950220     D  FOGFPI       E                     EXTFLD(SOGFRCPIVA)
026200950220     D  FOGCIA       E                     EXTFLD(SOGISCCIAA)
026300950220     D  FOGSES       E                     EXTFLD(SOGSEX)
026400950220     D  FOGNAT       E                     EXTFLD(SOGNATGIUR)
026500950220     D  FOGCOP       E                     EXTFLD(SOGCODPREC)
026600950220     D  FOGABI       E                     EXTFLD(SOGABI)
026700950220     D  FOGCLG       E                     EXTFLD(SOGCLFSOG)
026800950220     D* Anagrafica indirizzi
026900950220     D  FNDCOD       E                     EXTFLD(INDSOGG)
027000950220     D  FNDTII       E                     EXTFLD(INDTPIND)
027100950220     D  FNDCOI       E                     EXTFLD(INDCDIND)
027200950220     D  FNDDAU       E                     EXTFLD(INDDTULMO)
027300950220     D  FNDDAI       E                     EXTFLD(INDDTINVL)
027400950220     D  FNDDAF       E                     EXTFLD(INDDTFIVL)
027500950220     D  FNDDES       E                     EXTFLD(INDDESFIL)
027600950220     D  FNDDE1       E                     EXTFLD(INDDESAGG)
027700950220     D  FNDVIA       E                     EXTFLD(INDINDRIZ)
027800950220     D  FNDCIT       E                     EXTFLD(INDLOCALIT)
027900950220     D  FNDCAP       E                     EXTFLD(INDCAP)
028000950220     D  FNDPRV       E                     EXTFLD(INDPROV)
028100950220     D  FNDSTA       E                     EXTFLD(INDSTATO)
028200950220     D  FNDCAB       E                     EXTFLD(INDCAB)
028300950220     D  FNDLIN       E                     EXTFLD(INDLINGUA)
028400950220     D  FNDTLX       E                     EXTFLD(INDTELEX)
028500950220     D  FNDTEL       E                     EXTFLD(INDTELEFON)
028600950220     D  FNDTLF       E                     EXTFLD(INDTELEFAX)
028700950220     D*-------------
028800950220     D* Richiamo a DVFOR
028900950220     D DVFOR         E DS                  EXTNAME(DVFORDS) INZ
029000950220     D*-------------
029100950220     D* COSTANTI
029200950220     D IND             C                   CONST('  DVOINDDS')
029300950220     D ACO             C                   CONST('  DVOACODS')
029400950220     D CLPJ            C                   CONST('  DVOCLJDS')
029500950220     D FIL             C                   CONST('  DVOFILDS')
029600950220     D CLP             C                   CONST('  DVOCLPDS')
029700950220     D CLI             C                   CONST('  DVOCLIDS')
029800950220     D FIF             C                   CONST('  DVOFIFDS')
029900950220     D FOR             C                   CONST('  DVOFORDS')
030000950220     D FRN             C                   CONST('  DVOFRNDS')
030100950913      *-------------
030200950913      * Costanti
030300950913      * data a 0
030400941117     D $DT0            C                   CONST('0001-01-01')
030500950913      *-------------
030600950913      * Tasti di funzione
030700940506     D F01             C                   CONST(X'31')
030800940506     D F02             C                   CONST(X'32')
030900940506     D F03             C                   CONST(X'33')
031000940506     D F04             C                   CONST(X'34')
031100940506     D F05             C                   CONST(X'35')
031200940506     D F06             C                   CONST(X'36')
031300940506     D F07             C                   CONST(X'37')
031400940506     D F08             C                   CONST(X'38')
031500940506     D F09             C                   CONST(X'39')
031600940506     D F10             C                   CONST(X'3A')
031700940506     D F11             C                   CONST(X'3B')
031800940506     D F12             C                   CONST(X'3C')
031900940506     D F13             C                   CONST(X'B1')
032000940506     D F14             C                   CONST(X'B2')
032100940506     D F15             C                   CONST(X'B3')
032200940506     D F16             C                   CONST(X'B4')
032300940506     D F17             C                   CONST(X'B5')
032400940506     D F18             C                   CONST(X'B6')
032500940506     D F19             C                   CONST(X'B7')
032600940506     D F20             C                   CONST(X'B8')
032700940506     D F21             C                   CONST(X'B9')
032800940506     D F22             C                   CONST(X'BA')
032900940506     D F23             C                   CONST(X'BB')
033000940506     D F24             C                   CONST(X'BC')
033100940506     D ENTER           C                   CONST(X'F1')
033200940506     D ROLDWN          C                   CONST(X'F4')
033300940506     D ROLLUP          C                   CONST(X'F5')
033400950913      *-------------
033500950913      * Sottotitolo di testata
033600950913      *             'PROMSG    *LIBL-     C         SOTTIT
033700950913    > *             '     COSXXXX'
033800950427     D SOTTIT          C                   CONST('                    ')
033900950913      *-------------
034000950913      * Dati di ambiente ottenuti da XSOC
034100941201     D SOC001        E DS                  EXTNAME(XSOC001DS)
034200950913      *-------------
034300950913      * DS Interna per dati di output di XSOC
034400941110     D XSOCDS          DS          1000
034500950913      *-------------
034600950913      * Passaggio Autorizzazioni
034700950103     D DSAUT           DS           512
034800950913      *----------------------------------------------------*
034900950913      * Anagrafica percipienti
035000000114A0735D ZDateISO        S               D
035100001122B0298D* appoggio flags autorizzazioni comuni tra ANGCLI/FOR/PDC
035200001122  "  D AutImmis        S              1A
035300001122  "  D AutModif        S              1A
035400001122B0298D AutVisua        S              1A
035500120727A1357D* appoggio attiva/disattiva
035600120727A1357D F17SAV          S              1A   inz('0')
035700950228     IANAPE000
035800950228     I              APESOCIETA                  APESOC
035900950228     I              APEKCC                      APEKCC
036000950228     I              APEKSC                      APEKSC
036100950228     I              APETRIBUTO                  APETRB
036200950228     I              APEVARTRIB                  APEVRT
036300950228     I              APETPTESTO                  APETES
036400950301     I              APETPIND                    APETPI
036500950301     I              APECDIND                    APECDI
036600950228     I              APELOCALIT                  APELOC
036700950228     I              APEPROV                     APEPRV
036800950228     I              APESTATO                    APESTA
036900950228     I              APECDESMIN                  APECDE
037000950228     I              APECDFISC                   APECDF
037100950913      *
037200950714     I              APECOGNOME                  APECOG
037300950714     I              APENOME                     APENOM
037400950714     I              APEDTCONT                   APEDTC
037500950913      *-------------
037600950213     IANRCO000
037700950220     I              RCOSOCIETA                  RCOSOC
037800950220     I              RCOKCCSOGG                  RCOKSO
037900950220     I              RCOSOGG                     RCOSOG
038000950220     I              RCOTPIND                    RCOTPI
038100950220     I              RCOCDIND                    RCOCDI
038200950220     I              RCODTANN                    RCODTA
038300950220     I              RCOUTEIMM                   RCOUTE
038400950220     I              RCODTIMM                    RCODTI
038500950220     I              RCOUTEULMO                  RCOUTU
038600950220     I              RCODTULMO                   RCODTU
038700950220     I              RCOTPCONTO                  RCOTPC
038800950220     I              RCOMOVIMLE                  RCOMOV
038900950220     I              RCOTPGEST                   RCOTPG
039000950220     I              RCOSTORICO                  RCOSTO
039100950220     I              RCOSCADENZ                  RCOSCA
039200950220     I              RCORATE                     RCORAT
039300950220     I              RCONATURA                   RCONAT
039400950220     I              RCOSNATURA                  RCOSNA
039500950220     I              RCOAGENTE                   RCOAGE
039600950220     I              RCOPERCIPI                  RCOPER
039700950220     I              RCOTECNICO                  RCOTEC
039800950220     I              RCOSALDNAT                  RCOSAL
039900950220     I              RCORATEORI                  RCORRI
040000950220     I              RCORAPPTES                  RCORAP
040100950220     I              RCOITAEST                   RCOITE
040200950220     I              RCOTPCNTGR                  RCOTPN
040300950220     I              RCOSOCAVAL                  RCOSVA
040400950220     I              RCORGABCTB                  RCOCTB
040500950220     I              RCORGABENT                  RCOENT
040600950220     I              RCORGSQDIM                  RCODIM
040700950220     I              RCOALLEIVA                  RCOALL
040800950220     I              RCOOBBLMC                   RCOOMC
040900950220     I              RCOOBBLDIV                  RCOODV
041000950220     I              RCODIVISA                   RCODIV
041100950220     I              RCOPREPORT                  RCOPRE
041200950220     I              RCOTABPNOT                  RCOPNO
041300950220     I              RCOUNITA                    RCOUNI
041400950220     I              RCOKCCESTR                  RCOKCE
041500950220     I              RCOKSCESTR                  RCOKSE
041600950220     I              RCOBKAMM                    RCOBKA
041700950220     I              RCOKCCFACT                  RCOKCF
041800950220     I              RCOKSCFACT                  RCOKSF
041900950220     I              RCOCTGAN01                  RCOC01
042000950220     I              RCOCTGAN02                  RCOC02
042100950220     I              RCOCTGAN03                  RCOC03
042200950220     I              RCOCTGAN04                  RCOC04
042300950220     I              RCOCTGAN05                  RCOC05
042400950220     I              RCOCTGAN06                  RCOC06
042500950220     I              RCOCTGAN07                  RCOC07
042600950220     I              RCOCTGAN08                  RCOC08
042700950220     I              RCOCTGAN09                  RCOC09
042800950220     I              RCOCTGAN10                  RCOC10
042900950220     I              RCOCTGAN11                  RCOC11
043000950220     I              RCOCTGAN12                  RCOC12
043100950220     I              RCOCTGAN13                  RCOC13
043200950220     I              RCOCTGAN14                  RCOC14
043300950220     I              RCOCTGAN15                  RCOC15
043400950220     I              RCOCTGAN16                  RCOC16
043500950220     I              RCOCTGAN17                  RCOC17
043600950220     I              RCOCTGAN18                  RCOC18
043700950220     I              RCOCTGAN19                  RCOC19
043800950220     I              RCOCTGAN20                  RCOC20
043900950220     I              RCOCTGAN21                  RCOC21
044000950220     I              RCOCTGAN22                  RCOC22
044100950220     I              RCOCTGAN23                  RCOC23
044200950220     I              RCOCTGAN24                  RCOC24
044300950220     I              RCOCTGAN25                  RCOC25
044400950220     I              RCOCTGAN26                  RCOC26
044500950220     I              RCOCTGAN27                  RCOC27
044600950220     I              RCOCTGAN28                  RCOC28
044700950220     I              RCOCTGAN29                  RCOC29
044800950220     I              RCOCTGAN30                  RCOC30
044900950220     I              RCORIFANA1                  RCORA1
045000950220     I              RCOTPRIFA1                  RCOTR1
045100950220     I              RCORIFANA2                  RCORA2
045200950220     I              RCOTPRIFA2                  RCOTR2
045300950220     I              RCORIFANA3                  RCORA3
045400950220     I              RCOTPRIFA3                  RCOTR3
045500950220     I              RCORIFANA4                  RCORA4
045600950220     I              RCOTPRIFA4                  RCOTR4
045700950220     I              RCORIFANA5                  RCORA5
045800950220     I              RCOTPRIFA5                  RCOTR5
045900950220     I              RCORIFFRE1                  RCORF1
046000950220     I              RCORIFFRE2                  RCORF2
046100950220     I              RCORIFFRE3                  RCORF3
046200950220     I              RCOVOCE                     RCOVCE
046300950220     I              RCOANALIT                   RCOANL
046400950220     I              RCOOBBQTA                   RCOOBQ
046500950220     I              RCORGRIPVA                  RCORGI
046600950220     I              RCOPRECCOD                  RCOPRC
046700950220     I              RCOLIVRIS                   RCOLVR
046800950220     I              RCOSESSTRA                  RCOSST
046900950220     I              RCOSESSRIC                  RCOSSR
047000950220     I              RCOFLGOPE                   RCOFLO
047100950220     I              RCOFLGUTE                   RCOFLU
047200950913      *-------------
047300950913      *
047400950913      * Anagrafica piano dei conti
047500950220     IANPDC000
047600950220     I              PDCSOCIETA                  PDCSOC
047700950220     I              PDCDESCR                    PDCRSO
047800950220     I              PDCDESAGG                   PDCDEA
047900950220     I              PDCDESBREV                  PDCDES
048000950220     I              PDCCTGKCC                   PDCCTG
048100950220     I              PDCCTGFISC                  PDCCTF
048200950220     I              PDCSOTGRU                   PDCSTG
048300950220     I              PDCCOOBAN                   PDCCOB
048400950220     I              PDCRICLVEL                  PDCRCV
048500950220     I              PDCFLAGS                    PDCFLG
048600950913      * Anagrafica indirizzi
048700950227     IANIND000
048800950227     I              INDSOGG                     INDCOD
048900950227     I              INDTPIND                    INDTII
049000950227     I              INDCDIND                    INDCOI
049100950227     I              INDDTULMO                   INDDAU
049200950227     I              INDDTINVL                   INDDAI
049300950227     I              INDDTFIVL                   INDDAF
049400950227     I              INDDESFIL                   INDDES
049500950227     I              INDDESAGG                   INDDE1
049600950227     I              INDINDRIZ                   INDVIA
049700950227     I              INDLOCALIT                  INDCIT
049800950227     I              INDCAP                      INDCAP
049900950227     I              INDPROV                     INDPRV
050000950227     I              INDSTATO                    INDSTA
050100950227     I              INDCAB                      INDCAB
050200950227     I              INDLINGUA                   INDLIN
050300950227     I              INDTELEX                    INDTLX
050400950227     I              INDTELEFON                  INDTEL
050500950227     I              INDTELEFAX                  INDTLF
050600950913      * Anagrafica soggetti
050700950227     IANSOG000
050800950227     I              SOGSOGG                     SOGCOD
050900950227     I              SOGDTENTR                   SOGDAT
051000950227     I              SOGDTULMO                   SOGDAU
051100950227     I              SOGDTINVL                   SOGDAI
051200950227     I              SOGDTFIVL                   SOGDAF
051300950227     I              SOGDTNASC                   SOGDAN
051400950227     I              SOGDES                      SOGDES
051500950227     I              SOGDESAGG                   SOGDE1
051600950227     I              SOGCDFISC                   SOGCDF
051700950227     I              SOGPARTIVA                  SOGIVA
051800950227     I              SOGFRCPIVA                  SOGFPI
051900950227     I              SOGISCCIAA                  SOGCIA
052000950227     I              SOGSEX                      SOGSES
052100950227     I              SOGNATGIUR                  SOGNAT
052200950227     I              SOGCODPREC                  SOGCOP
052300950227     I              SOGABI                      SOGABI
052400950227     I              SOGCLFSOG                   SOGCLG
052500950913      *----------------------------------------------------*
052600950913      *                MAIN LINE PROGRAM
052700950913      *----------------------------------------------------*
052800950913      *
052900950913      * inizializzazione variabili
053000940223     C                   EXSR      INZVAR
053100950913      *
053200940223     C     $FINE         DOWEQ     *OFF
053300950213     C     $GEST         CASEQ     'D1'          GESD1
053400950301     C     $GEST         CASEQ     'D2'          GESD2
053500941027     C                   ENDCS
053600941027     C                   ENDDO
053700950913      *
053800950913      * fine programma
053900940325     C                   EXSR      ENDPGM
054000940325     C/EJECT
054100950913      ************************************************************
054200950913      * RICERCHE
054300950913      ************************************************************
054400940325     C     ENDPGM        BEGSR
054500980714A0428 *
054600980714  "   * se il COMMIT non è demandato al pgm chiamante
054700980714  "  C***  CMT207        IFEQ      '0'
054800980714  "   *
054900980714  "   * test se esistono transazioni attive ed eventualmente cosa fare
055000980714  "  C***                Z-ADD     0             EXECOD
055100980714  "  C***                CALL      'XRCMTI'
055200980714  "  C***                PARM                    EXECOD            1 0
055300980714  "   *
055400980714  "  C***                SELECT
055500980714  "   * Annulla richiesta
055600980714  "  C***  EXECOD        WHENEQ    1
055700980714  "  C***                MOVEL     *OFF          $FINE
055800980714  "  C***                MOVEL     *HIVAL        $LASTV
055900980714  "   * Conferma uscita
056000980714  "  C***  EXECOD        WHENEQ    2
056100980714  "  C***                ROLBK
056200980714  "   *
056300980714  "  C***                ENDSL
056400980714  "   *
056500980714A0428C***                ENDIF
056600980714      *
056700980714      * se ho confermato l'uscita proseguo
056800980714     C     $FINE         IFEQ      *ON
056900000515      *
057000000515      * disallocazione rcd nel caso di modifica/annullamento
057100000515     C     OPZ207        IFEQ      '02'
057200000515     C     OPZ207        OREQ      '04'
057300000515     C                   UNLOCK    ANAPE01L                             22
057400000515     C                   ENDIF
057500000515      *
057600950913      * passaggio dati al pgm chiamante
057700950228     C                   MOVEL     A207DS        KPJBU
057800950913      *
057900940325     C                   SETON                                            RT
058000940616     C                   RETURN
058100950913      *
058200941201     C                   ENDIF
058300950913      *
058400940325     C                   ENDSR
058500941201     C/EJECT
058600950913      ************************************************************
058700950913      * RICERCHE
058800950913      ************************************************************
058900941201     C     SEARCH        BEGSR
059000950913      *
059100950913      * determino Riga/Colonna del cursore
059200940127     C                   MOVE      NRG           R$$
059300940127     C                   MOVE      NCL           C$$
059400940127     C                   Z-ADD     RIRI          CURR
059500940127     C                   Z-ADD     COCO          CURC
059600950913      *
059700950913      * I campi che al momento dell'F4 risultano protetti non devono
059800950913      * essere abilitati alla funzione di ricerca (CASO DI ANNULLAMENTO)
059900950913      *
060000941027     C                   SELECT
060100950913      *
060200950913      * FMTD1  - TIPO INDIRIZZO
060300950227     C     H1NMFL        WHENEQ    'D1TPI'
060400950227     C                   RESET                   A045DS
060500950227     C                   MOVEL     '01'          OPZ045
060600950227     C                   MOVEL     D1SOG         COD045
060700950227     C                   MOVEL     A045DS        KPJBU
060800950227     C                   CALL      'ANA045R'
060900950227     C                   PARM                    KPJBA
061000950227     C                   MOVEL     KPJBU         A045DS
061100950227     C                   MOVEL     TII045        WWTII
061200950913      *
061300950227     C     WWTII         IFNE      *BLANK
061400950227     C                   MOVEL     WWTII         D1TPI
061500950227     C                   MOVEL     COI045        D1CDI
061600950227     C                   ENDIF
061700950913      *
061800950913      * FMTD1  - TRIBUTO
061900950301     C     H1NMFL        WHENEQ    'D1TRB'
062000950913    >C                   RESET                   A095ds
062100950913    >C                   MOVE      '01'          OPZ095
062200950913    >C                   CLEAR                   KPJBU
062300950913    >C                   MOVEL     A095DS        KPJBU
062400950913    >C                   CALL      'ANA095R'
062500950913    >C                   PARM                    KPJBA
062600950913    >C                   MOVEL     KPJBU         A095DS
062700950913    >C     TRIbuto095    IFNE      *BLANK
062800950913    >C                   MOVEL     TRIbuto095    D1TRB
062900950913    >C                   MOVE      triprog095    D1VRT
063000950913    >C                   MOVE      *blank        D1trbd
063100950913    >C                   ENDIF
063200970807     C*
063300950913      * FMTD2  - PROVINCIA
063400950301     C     H1NMFL        WHENEQ    'D2PRV'
063500950301     C                   RESET                   XTABDS
063600950301     C                   MOVE      *BLANK        WWKEY
063700950301     C                   MOVE      D2STA         WSTA
063800950301     C                   MOVE      WWKEY         XTAKEY
063900950301     C                   MOVE      '01'          XTAOPZ
064000950301     C                   MOVE      XSCSOC        XTAAZI
064100950301     C                   MOVE      XSCLIN        XTALIN
064200950301     C                   Z-ADD     CURR          XTARIG
064300950301     C                   Z-ADD     CURC          XTACOL
064400950301     C                   CLEAR                   KPJBU
064500950301     C                   MOVEL     XTABDS        KPJBU
064600950301     C                   CALL      'ANGC17R'
064700950301     C                   PARM                    KPJBA
064800950301     C                   MOVEL     KPJBU         XTABDS
064900950301     C     XTAERR        IFNE      '1'
065000950301     C                   MOVE      XTAKEY        WWKEY
065100950301     C                   ELSE
065200950301     C                   MOVE      *BLANK        WWKEY
065300950301     C                   ENDIF
065400950913      *
065500950301     C     WWKEY         IFNE      *BLANK
065600950301     C                   MOVEL     WSTA          D2STA
065700950301     C                   MOVEL     WPRV          D2PRV
065800950301     C                   ENDIF
065900970807      *
066000950913      * FMTD2  - STATO
066100950301     C     H1NMFL        WHENEQ    'D2STA'
066200950301     C                   RESET                   XTABDS
066300950301     C                   MOVE      *BLANK        WWSTA
066400950301     C                   MOVE      WWSTA         XTAKEY
066500950301     C                   MOVE      '01'          XTAOPZ
066600950301     C                   MOVE      XSCSOC        XTAAZI
066700950301     C                   MOVE      XSCLIN        XTALIN
066800950301     C                   Z-ADD     CURR          XTARIG
066900950301     C                   Z-ADD     CURC          XTACOL
067000950301     C                   CLEAR                   KPJBU
067100950301     C                   MOVEL     XTABDS        KPJBU
067200950301     C                   CALL      'ANGG12R'
067300950301     C                   PARM                    KPJBA
067400950301     C                   MOVEL     KPJBU         XTABDS
067500950301     C     XTAERR        IFNE      '1'
067600950301     C                   MOVE      XTAKEY        WWSTA
067700950301     C                   ELSE
067800950301     C                   MOVE      *BLANK        WWSTA
067900950301     C                   ENDIF
068000950913      *
068100950301     C     WWSTA         IFNE      *BLANK
068200950301     C                   MOVEL     WWSTA         D2STA
068300950301     C                   ENDIF
068400120504C2205 *
068500120504  "   * FMTD2  - CODICE MINISTERIALE
068600120504  "  C     H1NMFL        WHENEQ    'D2CDE'
068700120504  "  C                   MOVE      *BLANK        WWSTA
068800120504  "  C                   If        D2Sta <> ' '
068900120504  "  C                   Movel(p)  D2Sta         WWSTA
069000120504  "  C                   Endif
069100120504  "  C                   RESET                   XTABDS
069200120504  "  C                   MOVE      WWSTA         XTAKEY
069300120504  "  C                   MOVE      '01'          XTAOPZ
069400120504  "  C                   MOVE      XSCSOC        XTAAZI
069500120504  "  C                   MOVE      XSCLIN        XTALIN
069600120504  "  C                   Z-ADD     CURR          XTARIG
069700120504  "  C                   Z-ADD     CURC          XTACOL
069800120504  "  C                   CLEAR                   KPJBU
069900120504  "  C                   MOVEL     XTABDS        KPJBU
070000120504  "  C                   CALL      'ANGG12R'
070100120504  "  C                   PARM                    KPJBA
070200120504  "  C                   MOVEL     KPJBU         XTABDS
070300120504  "  C     XTAERR        IFNE      '1'
070400120504  "  C                   MOVE      XTAKEY        WWSTA
070500120504  "  C                   ELSE
070600120504  "  C                   MOVE      *BLANK        WWSTA
070700120504  "  C                   ENDIF
070800120504  "   *
070900120504  "  C     WWSTA         IFNE      *BLANK
071000120504  "  C                   MOVE      XCACUT        XTBUTE
071100120504  "  C                   MOVE      '1'           XTBRIC
071200120504  "  C                   MOVE      XSCSOC        XTBAZI
071300120504  "  C                   MOVE      *ZERO         XTBKEY
071400120504  "  C                   MOVE      'G12'         XTBCOD
071500120504  "  C                   MOVE      WWSTA         XTBKEY
071600120504  "  C                   CALLb     'XATB'
071700120504  "  C                   PARM                    XATBDS
071800120504  "  C     XTBERR        IFNE      '1'
071900120504  "  C                   MOVEL     XTBUNI        TABG12
072000120504  "  C                   IF        %Subst(TABG12:50:3) >= '000'
072100120504  "  C                   Move      CodUicG12     D2CDE
072200120504  "  C                   EndIF
072300120504  "  C                   ENDIF
072400120504C2205C                   ENDIF
072500000515C0766 *
072600981116 "    * FMTD2  - Sequenza altri tributi
072700981116 "   C     H1NMFL        WHENEQ    'D2SAT'
072800981116 "   C                   RESET                   XTABDS
072900981116 "   C                   MOVE      *blank        XTAKEY
073000981116 "   C                   MOVE      '01'          XTAOPZ
073100981116 "   C                   MOVE      XSCSOC        XTAAZI
073200981116 "   C                   MOVE      XSCLIN        XTALIN
073300981116 "   C                   Z-ADD     CURR          XTARIG
073400981116 "   C                   Z-ADD     CURC          XTACOL
073500981116 "   C                   CLEAR                   KPJBU
073600981116 "   C                   MOVEL     XTABDS        KPJBU
073700981116 "   C                   CALL      'ANG0CSR'
073800981116 "   C                   PARM                    KPJBA
073900981116 "   C                   MOVEL     KPJBU         XTABDS
074000981116 "   C     XTAERR        IFNE      '1'
074100000515 "   C     XTAKEY        andne     *blank
074200981116 "   C                   MOVE      XTAKEY        D2SAT
074300981125 "   C*                  ELSE
074400981125 "   C*                  MOVE      *BLANK        D2SAT
074500981125C0766C                   ENDIF
074600970807      *
074700970807      * FMTD1  - Tipo testo
074800970807     C     H1NMFL        WHENEQ    'D1TES'
074900970807     C                   Reset                   Ana720DS
075000970807     C                   MOVEL     '01'          OPZ720
075100970807     C                   Clear                   Kpjbu
075200970807     C                   MOVEL     Ana720Ds      KPJBU
075300970807     C                   CALL      'ANA720R'
075400970807     C                   PARM                    KPJBA
075500970807     C                   MOVEL     KPJBU         Ana720Ds
075600970807      *
075700970807     C     TpTxt720      IFNE      *BLANK
075800970807     C                   MOVEL     TpTxt720      D1Tes
075900970807     C                   ENDIF
076000950913      *
076100950913      * ricerca non abilitata in questa posizione
076200941027     C                   OTHER
076300940926     C                   SETON                                        9899
076400950913      *
076500941027     C                   ENDSL
076600970807      * imposto pos. del cursore
076700970807     C                   Z-ADD     CURR          H1RIGA
076800970807     C                   Z-ADD     CURC          H1COLO
076900950913      *
077000940506     C                   MOVE      *HIVAL        $LASTV
077100940127     C                   ENDSR
077200940225     C/EJECT
077300950913      ************************************************************
077400950913      * GESTIONE EMISSIONE TASTI FUNZIONE
077500950913      ************************************************************
077600940225     C     GESPIE        BEGSR
077700950913      *
077800940225     C                   SELECT
077900950913      *
078000940506     C     $GEST         WHENEQ    'D1'
078100940506     C     $ULKD1        IFEQ      0
078200950913      * riempimento tasti
078300940506     C                   EXSR      F24D1
078400940506     C                   ELSE
078500940506     C                   WRITE     FMTZ1
078600940506     C                   ENDIF
078700950913      *
078800950301     C     $GEST         WHENEQ    'D2'
078900950301     C     $ULKD2        IFEQ      0
079000950913      * riempimento tasti
079100950301     C                   EXSR      F24D1
079200950301     C                   ELSE
079300950301     C                   WRITE     FMTZ1
079400950301     C                   ENDIF
079500950913      *
079600941221     C                   ENDSL
079700950913      *
079800940225     C                   ENDSR
079900940131     C/EJECT
080000950913      ************************************************************
080100950913      * GESTIONE VIDEO RECORD D1
080200950913      ************************************************************
080300950220     C     GESD1         BEGSR
080400950913      * inizializzazione videata
080500950220   B1C     $INZD1        IFEQ      *ON
080600950220     C                   EXSR      INZD1
080700950220     C                   MOVE      *OFF          $INZD1
080800950220   E1C                   ENDIF
080900950913      * emissione piede videata se proveniente da altra
081000950913      * salvataggio valore formato di provenienza
081100950220   B1C     $LASTV        IFNE      'D1'
081200950220     C                   WRITE     FMTT1
081300950220     C                   EXSR      GESPIE
081400950220     C                   MOVE      $LASTV        $LASTG
081500950220     C                   MOVE      'D1'          $LASTV
081600950220   E1C                   ENDIF
081700950913      *
081800950913      * se esistono errori sulla videata
081900950913      * emetto la write del formato a indicatori spenti per vedere
082000950913      * le eventuali decodifiche
082100950220   B1C     *IN99         IFEQ      *ON
082200950220     C     $GEST         ANDEQ     'D1'
082300950220     C                   MOVEA     *IN           WIN
082400950220     C                   MOVE      *ALL'0'       IN5098           49
082500950220     C                   MOVEA     IN5098        *IN(50)
082600950220     C                   WRITE     FMTD1
082700950220     C                   MOVEA     WIN           *IN
082800950220   E1C                   ENDIF
082900950913      *
083000950220   B1C                   SELECT
083100950913      * Visualizzazione
083200950301     C     OPZ207        WHENEQ    '05'
083300950220     C                   WRITE     FMTD1
083400950220     C                   EXFMT     PROTECT
083500950913      * Immissione/Modifica/Copia
083600950220   X1C                   OTHER
083700950220     C                   EXFMT     FMTD1
083800950220   E1C                   ENDSL
083900950913      * controllo abilitazione tasti funzione
084000950913      *
084100950220     C                   EXSR      CTRKD1
084200950913      *
084300950220   B1C     *IN99         IFEQ      *OFF
084400950913      *
084500950220   B2C                   SELECT
084600950913      * ROLLUP
084700950220     C     $TASTO        WHENEQ    ROLLUP
084800950220     C                   MOVE      'D2'          $GEST
084900950913      * F2=Decodifica
085000950220     C     $TASTO        WHENEQ    F02
085100950220     C                   EXSR      F02D1
085200950913      * F3=Fine
085300950220     C     $TASTO        WHENEQ    F03
085400950220     C                   EXSR      F03D1
085500950913      * F4=Ricerca
085600950220     C     $TASTO        WHENEQ    F04
085700950220     C                   EXSR      SEARCH
085800950913      * F5=Refresh
085900950220     C     $TASTO        WHENEQ    F05
086000950220     C                   EXSR      F05D1
086100950913      * F8=Successivo
086200950220     C     $TASTO        WHENEQ    F08
086300950220     C                   EXSR      F08D1
086400950913      * F12=Ritorno
086500950220     C     $TASTO        WHENEQ    F12
086600950220     C                   EXSR      F12D1
086700120727A1357 * F17=Attiva/disattiva
086800120727  "  C     $TASTO        WHENEQ    F17
086900120727A1357C                   EXSR      F17D1
087000950913      * F24=Altri tasti
087100950220     C     $TASTO        WHENEQ    F24
087200950220     C                   EXSR      F24D1
087300950913      *
087400950220   X2C                   OTHER
087500950220     C                   EXSR      CTRD1
087600950913      * aggiungere le chiamate ai controlli delle videate supplementari
087700950301     C     *IN99         IFEQ      *OFF
087800950301     C                   EXSR      CTRD2
087900950301     C     *IN99         IFEQ      *ON
088000950301     C                   SETON                                            96
088100950301     C                   ENDIF
088200950301     C                   ENDIF
088300950913      *
088400951006   B3C     $TASTO        IFEQ      F16
088500951006     C     *IN99         ANDEQ     *OFF
088600951006   B3C     $TASTO        OREQ      F06
088700950220     C     *IN99         ANDEQ     *OFF
088800950913      * eseguo aggiornamento
088900950301     C                   EXSR      AGGAPE
089000950913      * eseguo operazioni del dopo-aggiornamento
089100950220   B4C     *IN99         IFEQ      *OFF
089200950220     C                   EXSR      GESAGG
089300950220   E4C                   ENDIF
089400950220   E3C                   ENDIF
089500950913      *
089600950220   E2C                   ENDSL
089700950913      *
089800950220   E1C                   ENDIF
089900950913      *
090000950220     C                   ENDSR
090100940506     C/EJECT
090200950913      ************************************************************
090300950913      * GESTIONE VIDEO RECORD  D2
090400950913      ************************************************************
090500950301     C     GESD2         BEGSR
090600950913      * emissione piede videata se proveniente da altra
090700950913      * salvataggio valore formato di provenienza
090800950301   B1C     $LASTV        IFNE      'D2'
090900950301     C                   WRITE     FMTT1
091000950301     C                   EXSR      GESPIE
091100950301     C                   MOVE      $LASTV        $LASTG
091200950301     C                   MOVE      'D2'          $LASTV
091300950301   E1C                   ENDIF
091400950913      *
091500950913      * se esistono errori sulla videata
091600950913      * emetto la write del formato a indicatori spenti per vedere
091700950913      * le eventuali decodifiche
091800950301   B1C     *IN99         IFEQ      *ON
091900950301     C     $GEST         ANDEQ     'D2'
092000950301     C                   MOVEA     *IN           WIN
092100950301     C                   MOVE      *ALL'0'       IN5098           49
092200950301     C                   MOVEA     IN5098        *IN(50)
092300950301     C                   WRITE     FMTD2
092400950301     C                   MOVEA     WIN           *IN
092500950301   E1C                   ENDIF
092600950913      *
092700950301   B1C                   SELECT
092800950913      * Annullamento
092900950301     C     OPZ207        WHENEQ    '04'
093000950913      * Visualizzazione
093100950301     C     OPZ207        OREQ      '05'
093200950301     C                   WRITE     FMTD2
093300950301     C                   EXFMT     PROTECT
093400950913      * Immissione/Modifica/Copia
093500950301   X1C                   OTHER
093600950301     C                   EXFMT     FMTD2
093700950301   E1C                   ENDSL
093800950913      * controllo abilitazione tasti funzione
093900950913      *
094000950301     C                   EXSR      CTRKD1
094100950913      *
094200950301   B1C     *IN99         IFEQ      *OFF
094300950913      *
094400950301   B2C                   SELECT
094500950913      * ROLDOWN
094600950301     C     $TASTO        WHENEQ    ROLDWN
094700950301     C                   MOVE      'D1'          $GEST
094800950913      * F3=Fine
094900950301     C     $TASTO        WHENEQ    F03
095000950301     C                   EXSR      F03D1
095100950913      * F4=Ricerca
095200950301     C     $TASTO        WHENEQ    F04
095300950301     C                   EXSR      SEARCH
095400950913      * F5=Refresh
095500950301     C     $TASTO        WHENEQ    F05
095600950301     C                   EXSR      F05D1
095700950913      * F8=Successivo
095800950301     C     $TASTO        WHENEQ    F08
095900950301     C                   EXSR      F08D1
096000950913      * F12=Ritorno
096100950301     C     $TASTO        WHENEQ    F12
096200950301     C                   EXSR      F12D1
096300120727A1357 * F17=Attiva/disattiva
096400120727  "  C     $TASTO        WHENEQ    F17
096500120727A1357C                   EXSR      F17D1
096600950913      * F24=Altri tasti
096700950301     C     $TASTO        WHENEQ    F24
096800950301     C                   EXSR      F24D1
096900950913      *
097000950301   X2C                   OTHER
097100950301     C                   EXSR      CTRD2
097200950301   B3C     *IN99         IFEQ      *OFF
097300950301     C                   EXSR      CTRD1
097400950301   B4C     *IN99         IFEQ      *ON
097500950301     C                   SETON                                            96
097600950301   E4C                   ENDIF
097700950301   E3C                   ENDIF
097800950913      *
097900960313   B3C     $TASTO        IFEQ      F16
098000960313     C     *IN99         ANDEQ     *OFF
098100960313   B3C     $TASTO        oreq      F06
098200950301     C     *IN99         ANDEQ     *OFF
098300950913      * eseguo aggiornamento
098400950301     C                   EXSR      AGGAPE
098500950913      * eseguo operazioni del dopo-aggiornamento
098600950301   B4C     *IN99         IFEQ      *OFF
098700950301     C                   EXSR      GESAGG
098800950301   E4C                   ENDIF
098900950301   E3C                   ENDIF
099000950913      *
099100950301   E2C                   ENDSL
099200950913      *
099300950301   E1C                   ENDIF
099400950913      *
099500950301     C                   ENDSR
099600950301     C/EJECT
099700950913      ************************************************************
099800950913      * CONTROLLO VIDEATA D2
099900950913      ************************************************************
100000950301     C     CTRD2         BEGSR
100100950913      *
100200950301     C                   SETOFF                                       99
100300950913      * non imposto pos. del cursore
100400950301     C                   Z-ADD     0             H1RIGA
100500950301     C                   Z-ADD     0             H1COLO
100600950913      *
100700950913      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
100800950913      *
100900950301   B1C     OPZ207        IFNE      '04'
101000950301     C     *IN04         ANDNE     *ON
101100950913      *
101200950913      * Controllo  codice provincia
101300950303     C                   CLEAR                   D2PRVD
101400950714   B2C     D2PRV         IFNE      *BLANKS
101500950301     C                   MOVE      XCACUT        XTBUTE
101600950301     C                   MOVE      '1'           XTBRIC
101700950301     C                   MOVE      XSCSOC        XTBAZI
101800950301     C                   MOVE      *ZERO         XTBKEY
101900950301     C                   MOVE      'C17'         XTBCOD
102000950302     C                   MOVE      D2STA         WSTA
102100950301     C                   MOVE      D2PRV         WPRV
102200950301     C                   MOVE      WWKEY         XTBKEY
102300961003     C                   CALLb     'XATB'
102400950301     C                   PARM                    XATBDS
102500950714   B3C     XTBERR        IFEQ      '1'
102600950302     C                   SETON                                          5499
102700950714   X3C                   ELSE
102800950301     C                   MOVEL     XTBUNI        TABC17
102900950301     C                   MOVEL     DESC17        D2PRVD
103000950714   E3C                   ENDIF
103100950714   E2C                   ENDIF
103200950913      *
103300950913      * Controllo  codice stato
103400950303     C                   CLEAR                   D2STAD
103500950714   B2C     D2STA         IFNE      *BLANKS
103600950301     C                   MOVE      XCACUT        XTBUTE
103700950301     C                   MOVE      '1'           XTBRIC
103800950301     C                   MOVE      XSCSOC        XTBAZI
103900950301     C                   MOVE      *ZERO         XTBKEY
104000950301     C                   MOVE      'G12'         XTBCOD
104100950301     C                   MOVE      D2STA         XTBKEY
104200961003     C                   CALLb     'XATB'
104300950301     C                   PARM                    XATBDS
104400950714   B3C     XTBERR        IFEQ      '1'
104500950301     C                   SETON                                          5399
104600950714   X3C                   ELSE
104700950301     C                   MOVEL     XTBUNI        TABG12
104800950301     C                   MOVEL     DESG12        D2STAD
104900120504C2205C                   IF        %Subst(TABG12:50:3) >= '000'
105000120504  "  C                   IF        CodUicG12 <> 0
105100120504  "  C                   Move      CodUicG12     D2CDE
105200120504  "  C                   EndIf
105300120504C2205C                   EndIF
105400950714   E3C                   ENDIF
105500950714   E2C                   ENDIF
105600950913      *
105700950913      * Controllo  cognome/nome
105800950727     C                   MOVE      '1'           XTBRIC
105900950727     C                   MOVE      XSCSOC        XTBAZI
106000950727     C                   MOVE      *ZERO         XTBKEY
106100950727     C                   MOVE      'G16'         XTBCOD
106200950727     C                   MOVE      SOGNAT        XTBKEY
106300961003     C                   CALLb     'XATB'
106400950727     C                   PARM                    XATBDS
106500950727     C     XTBERR        IFEQ      '0'
106600950727     C                   MOVEL     XTBUNI        TABG16
106700950727     C     TPNG16        IFEQ      '1'
106800950913      * cognome/nome obbligatorio
106900950727     C     D2COG         IFEQ      *BLANK
107000950727     C     D2NOM         OREQ      *BLANK
107100950727     C                   SETON                                          5299
107200950727     C                   ENDIF
107300950727     C                   ELSE
107400950913      * cognome/nome non ammesso
107500950727     C     D2COG         IFNE      *BLANK
107600950727     C     D2NOM         ORNE      *BLANK
107700950727     C                   SETON                                          5699
107800950727     C                   ENDIF
107900950727     C                   ENDIF
108000950727     C                   ELSE
108100950727     C                   SETON                                          6099
108200950727     C                   ENDIF
108300981125C0766 *
108400981125C0766 * Controllo sequenza altri tributi
108500981116 "   c                   clear                   tab0CS
108600981116 "   C     D2SAT         IFNE      *BLANKS
108700981116 "   C                   MOVE      XCACUT        XTBUTE
108800981116 "   C                   MOVE      '1'           XTBRIC
108900981116 "   C                   MOVE      XSCSOC        XTBAZI
109000981116 "   C                   MOVE      *ZERO         XTBKEY
109100981116 "   C                   MOVE      '0CS'         XTBCOD
109200981116 "   C                   MOVE      D2SAT         XTBKEY
109300981116 "   C                   CALLb     'XATB'
109400981116 "   C                   PARM                    XATBDS
109500981116 "   C     XTBERR        IFEQ      '1'
109600981116 "   C                   SETON                                          6399
109700981116 "   C                   ELSE
109800981116 "   C                   MOVEL     XTBUNI        TAB0CS
109900981116 "   C                   ENDIF
110000981116 "   C                   ENDIF
110100981125C0766C                   MOVEL     des0CS        d2satd
110200950913      * Controllo data
110300950913      *--------------------------------------------------------
110400950913      * conversione da GMA a ISO  e controllo validità
110500950913      *--------------------------------------------------------
110600990705b0054C                   If        d2dtc   <>    *zeros
110700961003     C                   CALLb     'XDT4000'
110800950714     C                   PARM                    XDTAMG           10
110900950714     C                   PARM      D2DTC         XDTGMA            6 0
111000950714     C                   PARM      3             XDTSTA            1 0
111100950714     C     XDTSTA        IFNE      0
111200950714     C                   SETON                                        5899
111300950714     C                   ELSE
111400950714     C                   ENDIF
111500990705b0054C                   else
111600000114A0735C******ex-b0054     Eval      D2dtc  =  Udate
111700000114A0735C                   Eval      D2dtc  =  Zdate
111800000114b0054C                   EndIf
111900950913      *
112000950913      *
112100950301   E1C                   ENDIF
112200950301      *
112300950301     C                   ENDSR
112400950301     C/EJECT
112500950913      ************************************************************
112600950913      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
112700950913      ************************************************************
112800940506     C     CTRKD1        BEGSR
112900940506     C                   SETOFF                                       99
113000940506     C                   MOVEA     $FC1          $FC
113100940506     C                   EXSR      CTRKEY
113200940506     C     *IN99         IFEQ      *ON
113300940506     C                   SETON                                        97
113400940506     C                   ENDIF
113500940506     C                   ENDSR
113600950213     C/EJECT
113700940117      ************************************************************
113800940207      * INIZIALIZZAZIONE VIDEATA DATI
113900940117      ************************************************************
114000940127     C     INZD1         BEGSR
114100950913      *
114200940131     C                   CLEAR                   FMTD1
114300950301     C                   CLEAR                   FMTD2
114400950913      *
114500940225     C                   Z-ADD     1             $PIED1
114600950913      *
114700001201B0298C* devo rieseguire il test sulla modalità perchè la CLEAR sul
114800001201  "  C* formato video ha spento anche gli indicatori, usati per
114900001201  "  C* nascondere/proteggere dei campi
115000001201  "  x* Imposto subito indicatori a seconda della modalità in cui mi
115100001201  "  x* trovo
115200001201B0298x* solo capoconti
115300001201     C*
115400001201     C                   SETOFF                                       111213
115500001201     C                   SETOFF                                       14
115600001201     C     MOD207        IFEQ      '1'
115700001201     C                   SETON                                        11
115800001201     C                   ENDIF
115900001201     C     MOD207        IFEQ      '2'
116000001201     C                   SETON                                        12
116100001201     C                   ENDIF
116200001201     C     MOD207        IFEQ      '3'
116300001201     C                   SETON                                        13
116400001201     C                   ENDIF
116500001201     C     MOD207        IFEQ      '4'
116600001201     C                   SETON                                        14
116700001201     C                   ENDIF
116800950913      *
116900950302     C                   MOVE      KCC207        D1KCC
117000950302    >C   12              MOVEL     KSC207        D1KSC
117100950302    >C   13              MOVEL     KSC207        D1CLI
117200950302    >C   14              MOVEL     KSC207        D1FOR
117300950913      *
117400950913      * decodifica capoconto
117500950302    >C                   MOVEL     WWWSOC        PDCSOC
117600950302    >C                   MOVEL     KCC207        PDCKCC
117700950302     C                   MOVE      *BLANKS       PDCKSC
117800950302    >C     K3PDC1        CHAIN     ANPDC000                           21
117900950302     C     *IN21         IFEQ      *OFF
118000950302     C                   MOVEL     PDCDES        D1KCCD
118100950302     C                   ENDIF
118200950913      *
118300950913      * Cerco decodifiche sottoconti
118400950913      *
118500950913      * Sottoconto
118600950302    >C     *IN12         IFEQ      *ON
118700950302    >C                   MOVEL     WWWSOC        PDCSOC
118800950302    >C                   MOVEL     KCC207        PDCKCC
118900950302    >C                   MOVEL     KSC207        PDCKSC
119000950302    >C     K3PDC1        CHAIN     ANPDC000                           21
119100950302     C     *IN21         IFEQ      *OFF
119200950302     C                   MOVEL     PDCDES        D1KSCD
119300950302     C                   ENDIF
119400950302     C                   ENDIF
119500950913      * fornitori
119600950302    >C     *IN14         IFEQ      *ON
119700950302     C                   RESET                   DVFOR
119800950302     C                   MOVEL     WWWSOC        DFRSOC
119900950302     C                   MOVEL     *BLANK        DFRUNI
120000950302     C                   MOVEL     KSC207        DFRFOR
120100950302     C                   MOVEL     *ZERO         DFRFIL
120200950302     C                   Z-ADD     1             DFRNRK
120300950302     C                   MOVEL     '1'           DFRRIC
120400950302     C                   MOVEL     FRN           DFRSET
120500950302     C                   MOVEL     '1'           DFRVIS
120600950302     C                   CALL      'DVFOR'
120700950302     C                   PARM                    DVFOR
120800950302     C                   PARM                    DVOFRN
120900950913      *
121000950302     C     DFRERR        IFEQ      '0'
121100950302     C                   MOVEL     FRNDES        D1KSCD
121200950302     C                   ENDIF
121300950302     C                   ENDIF
121400950913      *
121500950913      * aggancio anrco per altri dati
121600950302    >C                   MOVEL     WWWSOC        RCOSOC
121700950302    >C                   MOVEL     KCC207        RCOKCC
121800950302    >C                   MOVEL     KSC207        RCOKSC
121900951005     C                   SETOFF                                       2122
122000951005     C* gestione record allocato
122100951005   B2C     *IN22         DOUEQ     *OFF
122200951005     C     MSGRTN        ORNE      'R'
122300951005      *
122400951005    >C     K3RCO1        CHAIN(N)  ANRCO000                           2122
122500951005      *
122600951005      * Gestione RCD allocato
122700951005   B3C     *IN22         IFEQ      *ON
122800951005    >C                   MOVEL     'ANRCO00F'    MSGFIL
122900951005     C                   Z-ADD     2             MSGMSG
123000951005     C                   MOVEL     'CR'          MSGTPR
123100951005     C                   EXSR      MSGPGM
123200951005   E3C                   ENDIF
123300951005   E2C                   ENDDO
123400951005     C*
123500951005     C* se record è allocato da altro pgm
123600951005     C* ritorno al pgm guida avvertendolo dell'errore
123700951005     C     *IN22         IFEQ      *ON
123800951005     C                   MOVE      *ON           $FINE
123900951005     C                   MOVE      '1'           ERR207
124000951005     C                   EXSR      ENDPGM
124100951005     C*
124200951005     C* altrimenti
124300951005   X2C                   ELSE
124400950302     C     *IN21         IFEQ      *OFF
124500950302     C                   MOVE      RCOSOG        D1SOG
124600950302     C                   MOVE      RCOTPI        D1TPI
124700950302     C                   MOVE      RCOCDI        D1CDI
124800950913      *
124900950913      * decodifica codice indirizzo di ANRCO
125000950302     C                   CLEAR                   D1DES1
125100950302     C                   CLEAR                   D1VIA1
125200950302     C                   CLEAR                   D1CIT1
125300950302     C                   CLEAR                   D1CAP1
125400950302     C                   CLEAR                   D1PRV1
125500950302     C                   CLEAR                   D1STA1
125600950302     C     D1TPI         IFNE      *BLANK
125700950302     C     D1CDI         ORNE      *BLANKS
125800950302     C                   MOVE      D1SOG         INDCOD
125900950302     C                   MOVE      D1TPI         INDTII
126000950302     C                   MOVE      D1CDI         INDCOI
126100950302     C     K3IND1        CHAIN     ANIND01L                           21
126200950302     C     *IN21         IFEQ      *OFF
126300950302     C                   MOVEL     INDDES        D1DES1
126400950302     C                   MOVEL     INDVIA        D1VIA1
126500950302     C                   MOVEL     INDCIT        D1CIT1
126600950302     C                   MOVEL     INDCAP        D1CAP1
126700950302     C                   MOVEL     INDPRV        D1PRV1
126800950302     C                   MOVEL     INDSTA        D1STA1
126900950302     C                   ENDIF
127000950302     C                   ENDIF
127100950302     C                   ENDIF
127200951005     C                   ENDIF
127300950913      *
127400950913      * Decodifica soggetto
127500950302     C                   CLEAR                   D1SOGD
127600950302     C                   CLEAR                   D1SOGA
127700950913     C     D1SOG         CHAIN     ANSOG01L                           21
127800950302     C     *IN21         IFEQ      *ON
127900950302     C                   SETON                                        9957
128000950302     C                   ELSE
128100950302     C                   MOVEL     SOGDES        D1SOGD
128200950302     C                   MOVEL     SOGDE1        D1SOGA
128300950302     C                   ENDIF
128400950913      *
128500950913      * Decodifica indirizzo principale
128600950302     C                   CLEAR                   D1DES
128700950302     C                   CLEAR                   D1VIA
128800950302     C                   CLEAR                   D1CIT
128900950302     C                   CLEAR                   D1CAP
129000950302     C                   CLEAR                   D1PRV
129100950302     C                   CLEAR                   D1STA
129200950302     C                   MOVE      D1SOG         INDCOD
129300950302     C                   MOVE      *BLANKS       INDTII
129400950302     C                   MOVE      *BLANKS       INDCOI
129500950302     C     K3IND1        CHAIN     ANIND01L                           21
129600950302     C     *IN21         IFEQ      *OFF
129700950302     C                   MOVEL     INDDES        D1DES
129800950302     C                   MOVEL     INDVIA        D1VIA
129900950302     C                   MOVEL     INDCIT        D1CIT
130000950302     C                   MOVEL     INDCAP        D1CAP
130100950302     C                   MOVEL     INDPRV        D1PRV
130200950302     C                   MOVEL     INDSTA        D1STA
130300950302     C                   ENDIF
130400950913      *
130500950913      * valorizzo la chiave univoca per reperire i dati
130600950913      * se non immissione
130700950301   B1C     OPZ207        IFNE      '01'
130800950913      *
130900950913      * impostare i campi chiave ricevuti
131000950301     C                   MOVEL     WWWSOC        APESOC
131100950301     C                   MOVEL     KCC207        APEKCC
131200950301     C                   MOVEL     KSC207        APEKSC
131300950913      *
131400950220   B2C     *IN22         DOUEQ     *OFF
131500950220     C     MSGRTN        ORNE      'R'
131600950913      *
131700950301   B3C     OPZ207        IFEQ      '02'
131800950301    >C     OPZ207        OREQ      '04'
131900951005    >C     K3RCO1        CHAIN     ANAPE000                           2122
132000950913      * se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
132100950213   X3C                   ELSE
132200951005    >C     K3RCO1        CHAIN(N)  ANAPE000                           2122
132300950213   E3C                   ENDIF
132400950913      *
132500950913      * Gestione RCD allocato
132600950213   B3C     *IN22         IFEQ      *ON
132700950301    >C                   MOVEL     'ANAPE00F'    MSGFIL
132800950213     C                   Z-ADD     2             MSGMSG
132900950213     C                   MOVEL     'CR'          MSGTPR
133000950213     C                   EXSR      MSGPGM
133100950213   E3C                   ENDIF
133200941201   E2C                   ENDDO
133300950913      *
133400950913      * se il rcd da gestire non viene trovato
133500950913      * oppure è allocato da altro pgm
133600950913      * ritorno al pgm guida avvertendolo dell'errore
133700941201   B2C     *IN21         IFEQ      *ON
133800940318     C     *IN22         OREQ      *ON
133900940224     C                   MOVE      *ON           $FINE
134000950301     C                   MOVE      '1'           ERR207
134100950913      *
134200950913      * altrimenti
134300941201   X2C                   ELSE
134400950913      * alloco in modo esclusivo il rcd
134500950301   B3C     OPZ207        IFEQ      '02'
134600950301     C     OPZ207        OREQ      '04'
134700951005     C                   UPDATE    ANAPE000
134800941205   E3C                   ENDIF
134900950913      * riempio le videate
135000950213     C                   EXSR      RIED1
135100950913      * richiamo routine dei ctrl per decodificarla
135200940614     C                   EXSR      CTRD1
135300950301     C                   EXSR      CTRD2
135400940224     C                   MOVE      *ALL'0'       IN5098
135500940224     C                   MOVEA     IN5098        *IN(50)
135600950913      * definizione indicatori su video
135700940224     C                   SETOFF                                       02
135800941201     C                   SETOFF                                       04
135900941201   B3C                   SELECT
136000950301     C     OPZ207        WHENEQ    '02'
136100940224     C                   SETON                                        02        PR campi key
136200950301     C     OPZ207        WHENEQ    '04'
136300941201     C                   SETON                                        04        PR tutti fld
136400950301     C     OPZ207        WHENEQ    '05'
136500941201   E3C                   ENDSL
136600950913      *
136700941201   E2C                   ENDIF
136800950913      *
136900950913      * per inserimento valorizzo i campi con i default
137000941214   X1C                   ELSE
137100941214   E1C                   ENDIF
137200950913      *
137300940117     C                   ENDSR
137400950220     C/EJECT
137500950913      ************************************************************
137600950913      * RIEMPIMENTO VIDEATA  D1
137700950913      ************************************************************
137800940207     C     RIED1         BEGSR
137900950913      *
138000950301     C                   MOVE      APETPI        D1TPI
138100950301     C                   MOVE      APECDI        D1CDI
138200950301     C                   MOVE      APETRB        D1TRB
138300950301     C                   MOVE      APEVRT        D1VRT
138400950301     C                   MOVE      APETES        D1TES
138500950301     C                   MOVE      APELOC        D2LOC
138600950301     C                   MOVE      APEPRV        D2PRV
138700950301     C                   MOVE      APESTA        D2STA
138800950301     C                   MOVE      APECDE        D2CDE
138900950301     C                   MOVE      APECDF        D2CDF
139000981125C0766C                   MOVE      APEPINPS      D2PINPS
139100981222C0766C                   MOVEL     APETINPS      D2SAT
139200950913      *
139300950913     C                   move      apedtc        xdtamg
139400950714     C                   MOVE      APECOG        D2COG
139500950714     C                   MOVE      APENOM        D2NOM
139600961003     C                   CALLb     'XDT4000'
139700950913     C                   PARM                    XDTAMG           10
139800950714     C                   PARM                    XDTGMA            6 0
139900950714     C                   PARM      1             XDTSTA            1 0
140000950714     C     XDTSTA        IFNE      0
140100950714     C                   ELSE
140200950714     C                   MOVE      XDTGMA        D2DTC
140300950714     C                   ENDIF
140400120705A1357 *
140500120731  "  C                   If        APEDTFine <> *loval
140600120705  "  C                   move      APEDTFine     XDTAMG
140700120705  "  C                   CALLb     'XDT4000'
140800120705  "  C                   PARM                    XDTAMG           10
140900120705  "  C                   PARM                    XDTGMA            6 0
141000120705  "  C                   PARM      1             XDTSTA            1 0
141100120705  "  C     XDTSTA        IFNE      0
141200120705  "  C                   ELSE
141300120705  "  C                   MOVE      XDTGMA        D2DTFine
141400120705  "  C                   ENDIF
141500120705A1357C                   ENDIF
141600950913      *
141700940207     C                   ENDSR
141800940916     C/EJECT
141900950913      ************************************************************
142000950913      * GESTIONE F02 VIDEO D1
142100950913      ************************************************************
142200940916     C     F02D1         BEGSR
142300950913      *
142400950913      * per decodificare la videata richiamo la routine dei ctrl
142500950913      * e poi disattivo gli eventuali messaggi di errore
142600940916     C                   EXSR      CTRD1
142700950913      *
142800940916     C                   MOVE      *ALL'0'       IN5098
142900940916     C                   MOVEA     IN5098        *IN(50)
143000950913      *
143100940916     C                   ENDSR
143200940309     C/EJECT
143300950913      ************************************************************
143400950913      * GESTIONE F03 VIDEO D1
143500950913      ************************************************************
143600940309     C     F03D1         BEGSR
143700950913      *
143800940309     C                   MOVE      *ON           $FINE
143900950301    >C                   MOVE      '1'           RET207
144000950913      * fine programma
144100940325     C                   EXSR      ENDPGM
144200950913      *
144300940325     C                   ENDSR
144400940309     C/EJECT
144500950913      ************************************************************
144600950913      * GESTIONE F05 VIDEO D1
144700950913      ************************************************************
144800940309     C     F05D1         BEGSR
144900950913      *
145000940309     C                   MOVE      *ON           $INZD1
145100950220     C                   MOVE      'D1'          $GEST
145200950913      *
145300940309     C                   ENDSR
145400940309     C/EJECT
145500950913      ************************************************************
145600950913      * GESTIONE F08 VIDEO D1
145700950913      ************************************************************
145800940309     C     F08D1         BEGSR
145900950913      *
146000940309     C                   MOVE      *ON           $FINE
146100950301     C                   MOVE      '0'           RET207
146200950913      *
146300940309     C                   ENDSR
146400940309     C/EJECT
146500950913      ************************************************************
146600950913      * GESTIONE F12 VIDEO D1
146700950913      ************************************************************
146800940309     C     F12D1         BEGSR
146900950913      *
147000940309     C                   MOVE      *ON           $FINE
147100950301     C                   MOVE      '2'           RET207
147200950913      *
147300940309     C                   ENDSR
147400940309     C/EJECT
147500120727A1357 ************************************************************
147600120727  "   * GESTIONE F17 VIDEO D1
147700120727  "   ************************************************************
147800120727A1357C     F17D1         BEGSR
147900120727      *
148000120727     C                   Movel     *on           F17SAV
148100120727     C                   If        D2DtFine =  0
148200120727     C                   Eval      D2DtFine = Zdate
148300120727     C                   Else
148400120727     C                   Eval      D2DtFine = 0
148500120727     C                   EndIf
148600120727      *
148700120727A1357C                   ENDSR
148800120727A1357C/EJECT
148900950913      ************************************************************
149000950913      * GESTIONE F24 VIDEO D1
149100950913      ************************************************************
149200940309     C     F24D1         BEGSR
149300950913      *
149400940506     C                   MOVEA     $FX1          $FX
149500940506     C                   MOVEA     $FC1          $FC
149600940506     C                   MOVEA     $FR1          $FR
149700950220     C                   Z-ADD     $ULKD2        $ULK
149800940506     C                   EXSR      RIEKEY
149900950220     C                   Z-ADD     $ULK          $ULKD1            3 0
150000940720     C                   MOVEA     $FC           $FC1
150100940506     C                   MOVEL     $KEY1         Z1KE1
150200060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
150300940506     C                   MOVEL     $KEY2         Z1KE2
150400940506     C                   WRITE     FMTZ1
150500950913      *
150600940309     C                   ENDSR
150700950220     C/EJECT
150800950913      ************************************************************
150900950913      * CONTROLLO VIDEATA  D1
151000950913      ************************************************************
151100950220     C     CTRD1         BEGSR
151200950913      *
151300940510     C                   SETOFF                                       99
151400950913      * non imposto pos. del cursore
151500950213     C                   Z-ADD     0             H1RIGA
151600950213     C                   Z-ADD     0             H1COLO
151700950913      *
151800950913      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
151900950913      *
152000950301   B1C     OPZ207        IFNE      '04'
152100950213     C     *IN04         ANDNE     *ON
152200950913      *
152300000515A0778 * La lettura di ANTXT viene sostituita con la chiamata al driver
152400000518A0778 * X72TXT
152500970807     C                   Clear                   D1TesD
152600970806     C                   if        D1Tes  <>     *Blank
152700000512A0778X***                Eval      TxTSocieta  = XScSoc
152800000512  "  X***                Eval      TxTTpTesto  = D1Tes
152900000512  "  X***  K02Txt03      Chain     AnTxt03l                           21
153000000512  "  X***                if        *In21       = *On
153100000512  "   *
153200000518  "  C                   eval      XSocieta = XScSoc
153300000518  "  C                   eval      XUnita   = *blank
153400000518  "  C                   eval      XTpTesto = D1Tes
153500000518  "  C                   eval      XLingua  = *blank
153600000523  "  C                   eval      XClaUte  = *blank
153700000518  "  C                   eval      XPosRig = 'A'
153800000518  "  C                   eval      XNrRiga = *zeros
153900000518  "  C                   eval      XRibat = *blanks
154000000518  "  C                   eval      XTesRig = *blanks
154100000518  "  C                   eval      XNrChiavi = 3
154200000518  "  C                   eval      XUsoUnita = '1'
154300000512  "   *
154400000518  "  C                   call      'X72TXT'
154500000518  "  C                   parm      '0'           XEsito
154600000519  "  C                   parm      '6'           XOpe
154700000518  "  C                   parm                    XSocieta
154800000518  "  C                   parm                    XTpTesto
154900000518  "  C                   parm                    XUnita
155000000518  "  C                   parm                    XLingua
155100000523  "  C                   parm                    XClaUte
155200000523  "  C                   parm                    XPosRig
155300000518  "  C                   parm                    XNrRiga
155400000518  "  C                   parm                    XRibat
155500000518  "  C                   parm                    XTesRig
155600000518  "  C                   parm                    XNrChiavi
155700000518  "  C                   parm                    XUsoUnita
155800000512  "   *
155900000518A0778C                   if        XEsito <> *Off
156000970807     C                   Seton                                        5099
156100970807     C                   else
156200000512A0778X***                eval      D1tesD      = TxtTesRig
156300000518A0778C                   eval      D1tesD      = XTesRig
156400970806     C                   endif
156500970806     C                   endif
156600950913      * CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
156700950301   B2C     OPZ207        IFEQ      '01'
156800950301     C     OPZ207        OREQ      '03'
156900950913      *
157000950302     C                   MOVE      WWWSOC        APESOC
157100950302     C                   MOVEL     KCC207        APEKCC
157200950302     C                   MOVE      KSC207        APEKSC
157300950302     C     K3APE1        SETLL     ANAPE000                               21
157400950220     C     *IN21         IFEQ      '1'
157500950220     C                   SETON                                        5999
157600950220     C                   ENDIF
157700950220   E2C                   ENDIF
157800950913      *
157900950913      * Controllo soggetto
158000950913     C     D1SOG         CHAIN     ANSOG01L                           21
158100950303     C     *IN21         IFEQ      *ON
158200950303     C                   SETON                                        9957
158300950303     C                   ENDIF
158400950913      *
158500950913      * Controllo esistenza codice indirizzo digitato
158600950227     C                   CLEAR                   D1DES1
158700950227     C                   CLEAR                   D1VIA1
158800950227     C                   CLEAR                   D1CIT1
158900950227     C                   CLEAR                   D1CAP1
159000950227     C                   CLEAR                   D1PRV1
159100950227     C                   CLEAR                   D1STA1
159200950227     C     D1TPI         IFNE      *BLANK
159300950227     C     D1CDI         ORNE      *BLANKS
159400950227     C                   MOVE      D1SOG         INDCOD
159500950227     C                   MOVE      D1TPI         INDTII
159600950227     C                   MOVE      D1CDI         INDCOI
159700950227     C     K3IND1        CHAIN     ANIND01L                           21
159800950227     C     *IN21         IFEQ      *ON
159900950227     C                   SETON                                        5599
160000950227     C                   ELSE
160100950227     C                   MOVEL     INDDES        D1DES1
160200950227     C                   MOVEL     INDVIA        D1VIA1
160300950227     C                   MOVEL     INDCIT        D1CIT1
160400950227     C                   MOVEL     INDCAP        D1CAP1
160500950227     C                   MOVEL     INDPRV        D1PRV1
160600950227     C                   MOVEL     INDSTA        D1STA1
160700950227     C                   ENDIF
160800950227     C                   ENDIF
160900950913      *
161000950913      * Controllo  codice tributo
161100950301     C                   MOVE      *BLANKS       D1TRBD
161200950913     C                   eval      tritributo = d1trb
161300950913     C                   eval      tritriprog = d1vrt
161400950913     C     k03tri01      chain     antri01l                           21
161500950913     C     *in21         IFEQ      '0'
161600950913     C                   MOVEL     tridescr      D1TRBD
161700950913     C                   else
161800950913     C                   seton                                        5199
161900950913     C                   ENDIF
162000950213   E1C                   ENDIF
162100070119C2002C*
162200070119  "  C                   if        *In99      = *Off
162300070122  "  C                   EXSR      CtrVincoli
162400070119  "  C                   endif
162500070119C2002C*
162600940510     C                   ENDSR
162700940224     C/EJECT
162800070119C2002C************************************************************
162900070122  "  C* Controllo i vincoli sul tributo
163000070119  "  C************************************************************
163100070122C2002C     CtrVincoli    BEGSR
163200070119     C*
163300070119     C                   Clear                   XCHKMRADS
163400070119     C*
163500070119     C                   Eval      XCMSocieta  = XScSoc
163600070129     C* controllo specifici dati passati
163700070119     C                   Eval      XCMMODO     = '3'
163800070119     C                   Eval      XCMKCC      = D1Kcc
163900070119     C                   Eval      XCMKSC      = Ksc207
164000070119     C                   Eval      XCMTributo  = D1Trb
164100070119     C                   Eval      XCMVarTrib  = D1Vrt
164200070119     C                   Eval      XCMDATA     = ZDateIso
164300070119     C*
164400070119     C                   MoveL     XCHKMRADS     KPJBU
164500070119     C                   CALL      'XCHKMRA'
164600070119     C                   PARM                    KPJBA
164700070119     C*
164800070119     C                   MoveL     KPJBU         XCHKMRADS
164900070119     C*
165000070119     C                   if        XCMErrore   = *On
165100070119     C*
165200070119     C                   Select
165300070129     C*
165400070119     C*---------------
165500070119     C* tributo con vincolo 1 e persona fisica con almeno 35 anni
165600070119     C* oppure
165700070119     C* tributo con vincolo 1 e persona non fisica
165800070129     C* oppure
165900070129     C* data di nascita non impostata
166000070123     C                   When      XCMTpErr    = '014'  or
166100070129     C                             XCMTpErr    = '019'  or
166200070129     C                             XCMTpErr    = '013'
166300070129     C                   SetOn                                        6199
166400070119     C*
166500070119     C*---------------
166600070119     C* tributo con vincolo 2 e persona fisica con meno di 35 anni
166700070119     C* oppure
166800070119     C* tributo con vincolo 2 e persona non fisica
166900070129     C* oppure
167000070129     C* data di nascita non impostata
167100070123     C                   When      XCMTpErr    = '016'  or
167200070129     C                             XCMTpErr    = '021'  or
167300070129     C                             XCMTpErr    = '017'
167400070129     C                   SetOn                                        6499
167500070119     C*
167600070119     C                   other
167700070122     C* si sono verificati errori in fase di reperimento dati per
167800070122     C* controllo vincoli su tributo
167900070129     C                   SetOn                                        6299
168000070119     C                   endsl
168100070119     C*
168200070119     C                   endif
168300070119     C*
168400070119C2002C                   ENDSR
168500070119C2002C/EJECT
168600950913      ************************************************************
168700950913      * AGGIORNAMENTO ANAGRAFICA PERCIPIENTI
168800950913      ************************************************************
168900950301     C     AGGAPE        BEGSR
169000951006     C*
169100951006     C                   IF        $TASTO=F16
169200120727     C                   Eval      OPZ207 = '04'
169300951006     C                   Endif
169400950913      * se l'opzione non è di scrittura
169500950913      * devo rileggere il rcd in questione perchè per allocarlo in
169600950913      * modo esclusivo ho già fatto l'UPDATE
169700950301     C     OPZ207        IFNE      '01'
169800950301     C     OPZ207        ANDNE     '03'
169900950301     C                   MOVEL     WWWSOC        APESOC
170000950301     C                   MOVEL     KCC207        APEKCC
170100950301     C                   MOVEL     KSC207        APEKSC
170200950301     C     K3APE1        CHAIN     ANAPE000                           21
170300941205     C                   ENDIF
170400950913      *
170500940207     C                   SELECT
170600950913      *
170700950301     C     OPZ207        WHENEQ    '01'
170800950301     C     OPZ207        OREQ      '03'
170900950913      * dopo riempio campi da video
171000950301     C                   EXSR      RIEAPE
171100951005     C                   EXSR      FLGPERC
171200950301     C                   WRITE     ANAPE000                             22
171300950913      * se rcd non scrivibile attivo errore
171400940318     C     *IN22         IFEQ      *ON
171500941201     C                   SETON                                        9599
171600941027     C                   ENDIF
171700950913      *
171800951006     C     OPZ207        WHENEQ    '02'
171900951006     C                   EXSR      RIEAPE
172000120727A1357C                   If        F17Sav = *on
172100120727  "  C                   EXSR      FLGPERC
172200120727A1357C                   Endif
172300951006     C                   UPDATE    ANAPE000
172400951006     C     OPZ207        WHENEQ    '04'
172500951006     C                   EXSR      FLGPERC
172600951006     C                   DELETE    ANAPE000
172700950913      *
172800940207     C                   ENDSL
172900950323      *
173000950323      * richiama pgm User Exit
173100950323      *
173200950323     C     USRERR        IFEQ      *BLANK
173300950323     C     *IN99         ANDEQ     *OFF
173400950323      * imposta chiave univoca  per user exit
173500950323     C                   MOVE      XSCSOC        XUKSOC
173600950323     C                   MOVE      XSCDUN        XUKDUN
173700950323     C                   MOVEL     NOMPGM        XUKPGM
173800950323    >C                   MOVEL     APESOC        XUKFL1
173900950323    >C                   MOVEL     APEKCC        XUKFL2
174000950323    >C                   MOVEL     APEKSC        XUKFL3
174100950323    >C                   MOVEL     XUKDS         KPJBU
174200950323     C     NOMPGM        CAT       'U':0         USREXT           10
174300950323     C                   CALL      USREXT                               21
174400950323     C                   PARM                    KPJBA
174500950323     C     *IN21         IFEQ      *ON
174600950323     C                   MOVE      *ON           USRERR            1
174700950323     C                   ENDIF
174800950323     C                   ENDIF
174900950913      *
175000940203     C                   ENDSR
175100940224     C/EJECT
175200950913      ************************************************************
175300950913      * RIEMPIMENTO FILE IN AGGIORNAMENTO
175400950913      ************************************************************
175500950301     C     RIEAPE        BEGSR
175600950913      *
175700950913      * impostare i campi del file
175800950913      * ANAPE
175900950301     C                   MOVE      WWWSOC        APESOC
176000950301     C                   MOVEL     KCC207        APEKCC
176100950301     C                   MOVEL     KSC207        APEKSC
176200950301     C                   MOVE      D1TPI         APETPI
176300950301     C                   MOVE      D1CDI         APECDI
176400950301     C                   MOVE      D1TRB         APETRB
176500950301     C                   MOVE      D1VRT         APEVRT
176600950301     C                   MOVE      D1TES         APETES
176700950301     C                   MOVE      D2LOC         APELOC
176800950301     C                   MOVE      D2PRV         APEPRV
176900950301     C                   MOVE      D2STA         APESTA
177000950301     C                   MOVE      D2CDE         APECDE
177100950301     C                   MOVE      D2CDF         APECDF
177200981125C0766C                   MOVE      D2PINPS       APEPINPS
177300981222C0766C                   MOVEL     D2SAT         APETINPS
177400950913      *
177500950714     C                   MOVE      D2COG         APECOG
177600950714     C                   MOVE      D2NOM         APENOM
177700961003     C                   CALLb     'XDT4000'
177800950714     C                   PARM                    XDTAMG           10
177900950714     C                   PARM      D2DTC         XDTGMA            6 0
178000950714     C                   PARM      3             XDTSTA            1 0
178100950714     C     XDTSTA        IFNE      0
178200950714     C                   ELSE
178300950714     C                   MOVE      XDTAMG        APEDTC
178400950714     C                   ENDIF
178500120705A1357C*
178600120705  "  C                   If        D2DtFine <> *zeros
178700120705  "  C                   CALLb     'XDT4000'
178800120705  "  C                   PARM                    XDTAMG           10
178900120705  "  C                   PARM      D2DTFine      XDTGMA            6 0
179000120705  "  C                   PARM      3             XDTSTA            1 0
179100120705  "  C     XDTSTA        IFNE      0
179200120705  "  C                   ELSE
179300120705  "  C                   MOVE      XDTAMG        APEDTFINE
179400120705  "  C                   ENDIF
179500120705  "  C                   ELSE
179600120705  "  C                   MOVE      *loval        APEDTFINE
179700120705A1357C                   ENDIF
179800950913      *
179900940207     C                   ENDSR
180000951005     C/EJECT
180100951005      ************************************************************
180200951005      * Attivo percipiente
180300951005      ************************************************************
180400951005     C     FLGPERC       BEGSR
180500951005      *
180600951005      * aggancio anrco per altri dati
180700951005    >C                   MOVEL     WWWSOC        RCOSOC
180800951005    >C                   MOVEL     KCC207        RCOKCC
180900951005    >C                   MOVEL     KSC207        RCOKSC
181000951005     C                   SETOFF                                       2122
181100951005     C* gestione record allocato
181200951005   B2C     *IN22         DOUEQ     *OFF
181300951005     C     MSGRTN        ORNE      'R'
181400951005      *
181500951005    >C     K3RCO1        CHAIN     ANRCO000                           2122
181600951005      *
181700951005      * Gestione RCD allocato
181800951005   B3C     *IN22         IFEQ      *ON
181900951005    >C                   MOVEL     'ANRCO00F'    MSGFIL
182000951005     C                   Z-ADD     2             MSGMSG
182100951005     C                   MOVEL     'CR'          MSGTPR
182200951005     C                   EXSR      MSGPGM
182300951005   E3C                   ENDIF
182400951005   E2C                   ENDDO
182500951005     C*
182600951005     C* se record è allocato da altro pgm
182700951005     C* ritorno al pgm guida avvertendolo dell'errore
182800951005     C     *IN22         IFEQ      *ON
182900951005     C                   MOVE      *ON           $FINE
183000951005     C                   MOVE      '1'           ERR207
183100951005     C                   EXSR      ENDPGM
183200951005     C*
183300951005     C* altrimenti
183400951005   X2C                   ELSE
183500951005     C     *IN21         IFEQ      *OFF
183600951006     c                   Select
183700951006     C                   When      OPZ207 = '01'
183800951005     C                   MOVEL     '1'           RCOPER
183900951006     C                   When      OPZ207 = '04'
184000951006     C                   MOVEL     '0'           RCOPER
184100120727A1357C                   When      OPZ207 = '02'
184200120727  "  C                             AND D2DTFINE = 0
184300120727  "  C                   MOVEL     '1'           RCOPER
184400120727  "  C                   When      OPZ207 = '02'
184500120727  "  C                             AND D2DTFINE = ZDATE
184600120727A1357C                   MOVEL     '0'           RCOPER
184700951006     C                   EndSl
184800951006     C                   UPDATE    ANRCO000
184900951006     C                   ENDIF
185000951005     C                   ENDIF
185100951005      *
185200951005     C                   ENDSR
185300940224     C/EJECT
185400950913      ************************************************************
185500950913      * GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
185600950913      ************************************************************
185700940224     C     GESAGG        BEGSR
185800980714A0428 *
185900980714  "   * eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
186000980714  "  C***  CMT207        IFEQ      '0'
186100980714  "  C***                COMMIT
186200980714A0428C***                ENDIF
186300950913      *
186400950913      * segnalo al pgm chiamante l'avvenuta conferma
186500950301     C                   MOVE      *ON           OPR207
186600950913      * segnalo al pgm chiamante l'aver premuto l'F6
186700950301     C                   MOVE      '0'           RET207
186800950913      *
186900950302     C                   MOVE      *ON           $FINE
187000950913      *
187100940224     C                   ENDSR
187200940318     C/EJECT
187300950913      ******************************************************
187400950913      * PGM PER GESTIONE MESSAGGI ERRORE                   *
187500950913      ******************************************************
187600940318     C     MSGPGM        BEGSR
187700950913      *
187800950913      * Repesisco messaggio doi errore se msgmsg = 0
187900950213     C     MSGMSG        IFEQ      0
188000950213     C                   MOVEL     $COS(1)       IDMSG
188100961003     C                   CALLb     'XRTVM'
188200950213     C                   PARM                    IDMSG            27
188300950213     C                   PARM                    TXTMSG          100
188400950213     C                   PARM                    ERRMSG            1
188500950213     C     ERRMSG        IFNE      *ON
188600950213     C                   MOVEL     TXTMSG        MSG              70
188700950213     C                   ELSE
188800950213     C                   MOVEL     *ALL'?'       MSG
188900950213     C                   END
189000950213     C                   MOVEL     MSG           MSGMS1
189100950213     C                   ELSE
189200950213     C                   MOVE      *BLANKS       MSGMS1
189300950213     C                   ENDIF
189400950913      *
189500950213     C                   MOVEL     'S'           MSGRSP
189600950213     C                   MOVEL     'S'           MSGEMV
189700950213     C                   Z-ADD     10            MSGRGP
189800950213     C                   MOVEL     'S'           MSGLCK
189900950213     C                   MOVEA     STATUS        STS
190000950213     C                   MOVEL     'N'           MSGCNL
190100950213     C                   MOVEL     'N'           MSGVID
190200950213     C                   MOVEL     'N'           MSGOPE
190300950213     C                   MOVEL     'N'           MSGSTP
190400950213     C                   CALL      'XMSGR'
190500950213     C                   PARM                    XMSGDS
190600950913      *
190700940318     C                   ENDSR
190800940207     C/EJECT
190900950913      ************************************************************
191000950913      * OPERAZIONI INIZIALI
191100950913      ************************************************************
191200940131     C     *INZSR        BEGSR
191300000114A0735C     *DMY          Move      Udate         ZDateISO
191400000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
191500001122A0428x*
191600001122  "  x* Eseguo strcmtctl
191700001122  "  x***                CALLb     'XSTRCMT'
191800001122A0428x***                OPEN      ANAPE01L
191900950913      *
192000950913      * Reperimento parametri
192100950913      *
192200940117     C     *ENTRY        PLIST
192300940117     C                   PARM                    KPJBA
192400950103     C                   PARM                    DSAUT
192500950913      *
192600001122B0298C* siccome le autorizzazioni reperibili sono 2 e non una (ANGFOR,
192700001122  "  C*  e ANGPDC), sposto questo test nella InzVar in modo che venga
192800001122  "  C* fatto sempre - i chiamanti relativi a clienti e fornitori
192900001122  "  C* non passano mai il dato
193000001122  "  x**** PARMS         IFLT      2
193100001122  "  x****               MOVE      *BLANK        AUTDS
193200001122  "  x****               ELSE
193300001122  "  x****               MOVEL     DSAUT         AUTDS
193400001122B0298C****               ENDIF
193500950913      *
193600950913      *
193700950913      * Reperimento tasti di funzione
193800950913      *
193900940506     C                   EXSR      REPKEY
194000950913      *
194100940223     C                   ENDSR
194200940223     C/EJECT
194300950913      ************************************************************
194400950913      * REPERIMENTO TASTI DI FUNZIONE
194500950913      ************************************************************
194600940506     C     REPKEY        BEGSR
194700940506     C                   MOVEA     $FM1          $FM
194800950906     C                   MOVEA     $FC1          $FC
194900940506     C                   EXSR      REPKKK
195000940506     C                   MOVEA     $FX           $FX1
195100950913      *
195200940614     C                   ENDSR
195300940506     C/EJECT
195400950913      ************************************************************
195500950913      * REPERIMENTO TASTI DI FUNZIONE
195600950913      ************************************************************
195700940506     C     REPKKK        BEGSR
195800950913      *
195900940506     C                   CLEAR                   $FX
196000940506     C     1             DO        24            X
196100950913      *
196200940506     C     $FM(X)        IFNE      *BLANK
196300950906     C     $FC(X)        ANDNE     '0'
196400950913      *
196500940506     C                   MOVEL     $FM(X)        IDMSG
196600961003     C                   CALLb     'XRTVM'
196700940506     C                   PARM                    IDMSG            27
196800940506     C                   PARM                    TXTMSG          100
196900940506     C                   PARM                    ERRMSG            1
197000950913      *
197100940506     C     ERRMSG        IFNE      *ON
197200940506     C                   MOVEL     TXTMSG        $FX(X)
197300940506     C                   ELSE
197400940506     C                   MOVEL     *ALL' '       $FX(X)
197500941027     C                   ENDIF
197600950913      *
197700941027     C                   ENDIF
197800950913      *
197900941027     C                   ENDDO
198000950913      *
198100940506     C                   ENDSR
198200941110     C/EJECT
198300950913      ************************************************************
198400950913      * Reperimento dati società
198500950913      ************************************************************
198600941110     C     REPSOC        BEGSR
198700950913      *
198800961003     C                   CALLb     'XSOC'
198900941110     C                   PARM                    TIPXSC            6
199000941110     C                   PARM                    SOCXSC            3
199100941110     C                   PARM                    CDSXSC            9 0
199200941110     C                   PARM                    MODXSC            3
199300941110     C                   PARM      *BLANKS       RTNXSC            1
199400941110     C                   PARM                    XSOCDS
199500941110     C                   PARM                    KPJBA
199600950913      *
199700941110     C                   ENDSR
199800940509     C/EJECT
199900950913      ************************************************************
200000950913      * INIZIALIZZAZIONE VARIABILI
200100950913      ************************************************************
200200940223     C     INZVAR        BEGSR
200300950913      *
200400950913      * Dati da PGM guida
200500950913      *
200600950228     C                   MOVEL     KPJBU         A207DS
200700001122B0298C*
200800001122  "  C* Imposto subito indicatori a seconda della modalità in cui mi
200900001122  "  C* trovo
201000001122  "  C                   SETOFF                                       111213
201100001122  "  C                   SETOFF                                       14
201200001122  "   *  11           GESTIONE CAPOCONTI
201300001122  "  C     Mod207        IFEQ      '1'
201400001122  "  C                   SETON                                        11
201500001122  "  C                   ENDIF
201600001122  "   *  12           GESTIONE SOTTOCONTI
201700001122  "  C     Mod207        IFEQ      '2'
201800001122  "  C                   SETON                                        12
201900001122  "  C                   ENDIF
202000001122  "   *  13           GESTIONE CLIENTI
202100001122  "  C     Mod207        IFEQ      '3'
202200001122  "  C                   SETON                                        13
202300001122  "  C                   ENDIF
202400001122  "   *  14           GESTIONE FORNITORI
202500001122  "  C     Mod207        IFEQ      '4'
202600001122  "  C                   SETON                                        14
202700001122B0298C                   ENDIF
202800950913      *
202900950913      * Reperimento dati società
203000950913      *
203100941201     C                   MOVEL     'SOC001'      TIPXSC
203200941115     C                   MOVEL     *BLANK        SOCXSC
203300941115     C                   EXSR      REPSOC
203400941115     C     RTNXSC        IFNE      '1'
203500941115     C                   MOVEL     XSOCDS        SOC001
203600941115     C                   ELSE
203700941115     C                   RETURN
203800941115     C                   ENDIF
203900950913      *
204000950913      * gestione società di riferimento
204100950301     C     FSC207        IFEQ      'S'
204200950303     C     FSC207        OREQ      ' '
204300950301     C     OPZ207        OREQ      *BLANK
204400950110     C                   MOVE      XSCSOC        WWWSOC
204500950913      * gestione simulata da capofila
204600950110     C                   ELSE
204700950110     C                   MOVE      XSCCPA        WWWSOC
204800950110     C                   ENDIF
204900950913      *
205000950913      * Reperimento autorizzazioni
205100950913      *
205200001122B0298C* siccome le autorizzazioni reperibili sono 2 e non una (ANGFOR,
205300001122  "  C*  e ANGPDC), sposto questo test nella InzVar in modo che venga
205400001122  "  C* fatto sempre - i chiamanti relativi a clienti e fornitori
205500001122  "  C* non passano mai il dato
205600001122  "  C     PARMS         IFLT      2
205700001122  "  C                   MOVE      *BLANK        AUTDS
205800001122  "  C                   ELSE
205900001122  "  C                   MOVEL     DSAUT         AUTDS
206000001122B0298C                   ENDIF
206100950913      * le autorizzazioni potrebbero essere già state passate,
206200950913      * nel qual caso non vanno reperite
206300950103     C     AUTDS         IFEQ      *BLANK
206400950103     C                   EXSR      REPAUT
206500001122B0298C*
206600001122  "  C* siccome l'autorizzazione può essere passata dal chiamante solo
206700001122  "  C* nel caso gestisca i conti diversi da fornitori (che
206800001122  "  C*  invece non la passano mai) posso tranquillamente spostare i
206900001122B0298C* dati nella DS del PdC
207000950103     C                   ELSE
207100950213     C                   MOVEL     XCADDS        GPDCDS
207200001122B0298C* valorizzo i campi comuni per i flag di immissione/modifica/
207300001122  "  C* visualizzazione
207400001122  "  C                   EVAL      AutImmis = ImmPdC
207500001122  "  C                   EVAL      AutModif = ManPdC
207600001122B0298C                   EVAL      AutVisua = VisPdC
207700950103     C                   ENDIF
207800950913      *
207900950913      * Ctrl se l'opzione con cui è stato richiamato è congruente con
208000950913      * le autorizzazione
208100950913      *
208200950103     C                   EXSR      TSTAUT
208300950913      *
208400950913      * Ctrl sul capoconto e sul codice fornitore ricevuto
208500950913      *
208600950301     C                   EXSR      TSTKCC
208700950913      *
208800950913      * Variabili per gestione videate
208900950913      *
209000950913      * Solo in immissione  emetto videata'D1' negli altri
209100950913      * casi non serve (questo perchè i flag devono essere desunti
209200950913      * dalla categoria(nel caso di immissione capoconti) o dal capocon
209300950913      * to (nel caso di immissione sottoconti)
209400940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
209500940223     C                   MOVE      *OFF          $FINE                          fine pgm
209600940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
209700950213     C                   MOVE      *ON           $INZD2                         inizializzione D1
209800940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
209900940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
210000940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
210100950214     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
210200950220     C                   Z-ADD     0             $ULKD3            3 0          piede da emett. D1
210300950913      *
210400950913      * Definizione dei tasti funzione utilizzabili a pgm
210500950913      *
210600950213     C                   EXSR      DEFOPZ
210700950913      *
210800950913      * Variabili appoggio
210900950913      *
211000940224     C                   Z-ADD     0             CURR
211100940224     C                   Z-ADD     0             CURC
211200961003     C                   CALLb     'XDT4000'
211300941201     C                   PARM                    XDTAMG           10
211400000114A0735C*****              PARM      UDATE         XDTGMA            6 0
211500000114A0735C                   PARM      ZDate         XDTGMA            6 0
211600941201     C                   PARM      4             XDTSTA            1 0
211700941201     C                   MOVEL     XDTAMG        WDATE
211800941201     C                   MOVE      XSCON         SI
211900941201     C                   MOVE      XSCOFF        NO
212000950913      * inizializzo tutte le date in AA/MM/GG con la data a 0
212100950913      * richiamo XCHKAUT per reperire il cod.utente interno (XCACUT)
212200941214     C                   CLEAR                   AUTDS
212300950110     C                   MOVEL     WWWSOC        XCASOC                         SOCIETA
212400941214     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
212500941214     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
212600961003     C                   CALLb     'XCHKAUT'
212700941214     C                   PARM                    AUTDS
212800950913      *
212900950913      * Valorizzazione campi univoci testate
213000950913      *
213100940207     C                   CLEAR                   FMTT1
213200940207     C                   MOVEL     KNSIF         NOMSIF
213300941110     C                   MOVEL     XSCDSI        NOMDIT
213400941027     C                   MOVEL     DSPGM         NOMPGM
213500950913      * recupero il sottotitolo (se c'è)
213600950427     C                   MOVEL     *BLANK        T1TIT
213700950427     C     SOTTIT        IFNE      *BLANK
213800950427     C                   MOVEL     SOTTIT        IDMSG
213900961003     C                   CALLb     'XRTVM'
214000950427     C                   PARM                    IDMSG            27
214100950427     C                   PARM                    TXTMSG          100
214200950427     C                   PARM                    ERRMSG            1
214300950427     C                   PARM      *ON           CTRMSG            1            centratura
214400950427     C                   PARM      30            LENMSG            3 0          lun output.
214500950427     C     ERRMSG        IFNE      *ON
214600950427     C                   MOVEL     TXTMSG        T1TIT
214700950427     C                   ELSE
214800950427     C                   MOVEL     *ALL'?'       T1TIT
214900950427     C                   ENDIF
215000950427     C                   ENDIF
215100950913      * recupero il tipo operazione
215200940224     C                   Z-ADD     1             X
215300950301     C     OPZ207        LOOKUP    $OZ(X)                                 21
215400940224     C     *IN21         IFEQ      *ON
215500950405     C                   MOVEL     $ID(X)        IDMSG
215600961003     C                   CALLb     'XRTVM'
215700950405     C                   PARM                    IDMSG            27
215800950405     C                   PARM                    TXTMSG          100
215900950405     C                   PARM                    ERRMSG            1
216000950405     C                   PARM      *ON           CTRMSG            1            centratura
216100950405     C                   PARM      30            LENMSG            3 0          lun output.
216200950405     C     ERRMSG        IFNE      *ON
216300950405     C                   MOVEL     TXTMSG        T1OPE
216400950405     C                   ELSE
216500950405     C                   MOVEL     *ALL'?'       T1OPE
216600950405     C                   ENDIF
216700950405     C                   ENDIF
216800950913      *
216900950913      * Inizializzazione/Decodifica prima videata
217000950913      *
217100940224     C                   EXSR      INZD1
217200940224     C                   MOVE      *OFF          $INZD1
217300950913      *
217400940117     C                   ENDSR
217500950301     C/EJECT
217600950913      ************************************************************
217700950913      * TEST CAPOCONTO
217800950913      ************************************************************
217900950913      * il fornitore deve essere attivo contabilmente
218000950913      * Il capoconto deve essere gestito a soggetti
218100950913      *
218200950301     C     TSTKCC        BEGSR
218300950913      *
218400951006      * Controlli contabilità
218500950301    >C                   MOVEL     WWWSOC        RCOSOC
218600950301    >C                   MOVEL     KCC207        RCOKCC
218700950914    >C                   MOVEL     KSC207        RCOKSC
218800951005     C* gestione record allocato
218900951005     C                   SETOFF                                       2122
219000951005   B2C     *IN22         DOUEQ     *OFF
219100951005     C     MSGRTN        ORNE      'R'
219200951005      *
219300951005    >C     K3RCO1        CHAIN(N)  ANRCO000                           2122
219400951005      *
219500951005      * Gestione RCD allocato
219600951005   B3C     *IN22         IFEQ      *ON
219700951005    >C                   MOVEL     'ANRCO00F'    MSGFIL
219800951005     C                   Z-ADD     2             MSGMSG
219900951005     C                   MOVEL     'CR'          MSGTPR
220000951005     C                   EXSR      MSGPGM
220100951005   E3C                   ENDIF
220200951005   E2C                   ENDDO
220300951006     C* se  allocato da altro pgm
220400951005     C* ritorno al pgm guida avvertendolo dell'errore
220500951005     C     *IN22         IFEQ      *ON
220600951005     C                   MOVE      *ON           $FINE
220700951005     C                   MOVE      '1'           ERR207
220800951005     C                   EXSR      ENDPGM
220900951005     C*
221000951005     C* altrimenti
221100951005   X2C                   ELSE
221200951005     C* Record non trovato
221300951005     C* il fornitore deve essere attivo contabilmente
221400951005     C     *IN21         IFEQ      *ON
221500951005     C     MOD207        ANDEQ     '4'
221600951005     C                   MOVE      *ON           $FINE
221700951005     C                   MOVE      '3'           ERR207
221800951005     C                   EXSR      ENDPGM
221900951005     c                   endif
222000951006     C* Record trovato
222100950301     C     *IN21         IFEQ      *OFF
222200951006     C*
222300951006     C* deve essere gestito a soggetti
222400951006     C     RCOKSO        IFNE      *ON
222500950301     C                   MOVE      '4'           ERR207                         no soggetti
222600950301     C                   MOVE      *ON           $FINE
222700950301     C                   EXSR      ENDPGM
222800950301     C                   ENDIF
222900951005     C*
223000951005     C* PER fornitori la
223100951005      * Sottonatura deve essere 'F'
223200951005     C     MOD207        IFEQ      '4'
223300951005     C     RCOSNA        ANDNE     'F'
223400951005     C                   MOVE      '5'           ERR207                         sottonat. F
223500951005     C                   MOVE      *ON           $FINE
223600951005     C                   EXSR      ENDPGM
223700951005     C                   ENDIF
223800951006     C*
223900951006     C                   ENDIF
224000951005     C*
224100951005     C                   ENDIF
224200950913      *
224300950301     C                   ENDSR
224400950301     C/EJECT
224500950913      ************************************************************
224600950913      * TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
224700950913      ************************************************************
224800950103     C     TSTAUT        BEGSR
224900950913      *
225000950103     C                   SELECT
225100950913      *
225200950913      * se richiamato in immissione
225300950301     C     OPZ207        WHENEQ    '01'
225400950301     C     OPZ207        OREQ      '03'
225500950913      * ma l'utente non è abilitato
225600001122B0298x**** IMMPDC        IFEQ      *OFF
225700001122B0298C     AutImmis      IFEQ      *OFF
225800950913      * esco da pgm avvertendolo
225900950301     C                   MOVE      '2'           ERR207
226000950104     C                   MOVE      *ON           $FINE
226100950103     C                   EXSR      ENDPGM
226200950216     C                   ENDIF
226300950913      *
226400950913      * se richiamato in modifica
226500950301     C     OPZ207        WHENEQ    '02'
226600950913      * ma l'utente non è abilitato
226700001122B0298x**** MANPDC        ANDEQ     *OFF
226800001122B0298C     AutModif      ANDEQ     *OFF
226900950913      * testo se l'utente è abilitato almeno alla visualizzazione
227000001122B0298x**** VISPDC        IFEQ      *ON
227100001122B0298C     AutVisua      IFEQ      *ON
227200950301     C                   MOVE      '05'          OPZ207
227300950103     C                   ELSE
227400950913      * altrimenti esco da pgm avvertendolo
227500950301     C                   MOVE      '2'           ERR207
227600950104     C                   MOVE      *ON           $FINE
227700950103     C                   EXSR      ENDPGM
227800950103     C                   ENDIF
227900950913      *
228000950913      * se richiamato in copia
228100950301     C     OPZ207        WHENEQ    '03'
228200950913      * ma l'utente non è abilitato
228300001122B0298x**** IMMPDC        ANDEQ     *OFF
228400001122B0298C     AutImmis      ANDEQ     *OFF
228500950913      * esco da pgm avvertendolo
228600950301     C                   MOVE      '2'           ERR207
228700950104     C                   MOVE      *ON           $FINE
228800950103     C                   EXSR      ENDPGM
228900950913      *
229000950913      * se richiamato in annullamento
229100950301     C     OPZ207        WHENEQ    '04'
229200950913      * ma l'utente non è abilitato
229300950213     C     ANNPDC        ANDEQ     *OFF
229400950913      * esco da pgm avvertendolo
229500950301     C                   MOVE      '2'           ERR207
229600950104     C                   MOVE      *ON           $FINE
229700950103     C                   EXSR      ENDPGM
229800950913      *
229900950913      * se richiamato in visualizzazione
230000950301     C     OPZ207        WHENEQ    '05'
230100950913      * ma l'utente non è abilitato
230200001122B0298x**** VISPDC        ANDEQ     *OFF
230300001122B0298C     AutVisua      ANDEQ     *OFF
230400950913      * esco da pgm avvertendolo
230500950301     C                   MOVE      '2'           ERR207
230600950104     C                   MOVE      *ON           $FINE
230700950103     C                   EXSR      ENDPGM
230800950913      *
230900950103     C                   ENDSL
231000950913      *
231100950103     C                   ENDSR
231200950103     C/EJECT
231300950913      ************************************************************
231400950913      * REPERIMENTO AUTORIZZAZIONI UTENTE
231500950913      ************************************************************
231600950103     C     REPAUT        BEGSR
231700950913      *
231800950103     C                   CLEAR                   AUTDS
231900950913      *
232000950123     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
232100950103     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
232200950103     C                   Z-ADD     0             XCAGRP                         GRUPPO
232300950103     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
232400001122B0298C* a seconda della sottonatura del conto (cliento o fornitore o
232500001122  "  C*  altro conto) uso un'autorizzazione diversa
232600001122  "  C                   SELECT
232700001122  "  C* non dovrebbe mai accadere che un percipiente sia un cliente!
232800001122  "  C***                WHEN      *in13 = *on
232900001122  "  C***                MOVEL     'ANGCLI'      XCAFNC                         FUNZIONE
233000001122  "  C                   WHEN      *in14 = *on
233100001122  "  C                   MOVEL     'ANGFOR'      XCAFNC                         FUNZIONE
233200001122B0298C                   OTHER
233300950213     C                   MOVEL     'ANGPDC'      XCAFNC                         FUNZIONE
233400001122B0298C                   ENDSL
233500950103     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
233600950103     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
233700950103     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
233800950913      *
233900961003     C                   CALLb     'XCHKAUT'
234000950103     C                   PARM                    AUTDS
234100950913      *
234200950913      * errori/non abilitazione
234300950103     C     XCARTN        IFNE      0
234400950227     C                   MOVE      *ON           $FINE
234500950103     C                   EXSR      ENDPGM
234600950103     C                   ELSE
234700001122B0298C* a seconda della sottonatura del conto (cliento o fornitore o
234800001122  "  C*  altro conto) uso un'autorizzazione diversa
234900001122  "  C                   SELECT
235000001122  "  C                   WHEN      *in14 = *on
235100001122  "  C                   MOVEL     XCADDS        ANGFORDS
235200001122  "  C* valorizzo i campi comuni per i flag di immissione/modifica/
235300001122  "  C* visualizzazione
235400001122  "  C                   EVAL      AutImmis = ImmCtbFor
235500001122  "  C* nel caso il dato fosse blank
235600001122  "  C                   IF        ImmCtbFor = *blank
235700001122  "  C* uso il flag dei dati gestionali
235800001122  "  C                   EVAL      AutImmis = ImmFor
235900001122  "  C                   ENDIF
236000001122  "  C                   EVAL      AutModif = ManCtbFor
236100001122  "  C* nel caso il dato fosse blank
236200001122  "  C                   IF        ManCtbFor = *blank
236300001122  "  C* uso il flag dei dati gestionali
236400001122  "  C                   EVAL      AutModif = ManFor
236500001122  "  C                   ENDIF
236600001122  "  C                   EVAL      AutVisua = VisCtbFor
236700001122  "  C* nel caso il dato fosse blank
236800001122  "  C                   IF        VisCtbFor = *blank
236900001122  "  C* uso il flag dei dati gestionali
237000001122  "  C                   EVAL      AutVisua = VisFor
237100001122  "  C                   ENDIF
237200001122B0298C                   OTHER
237300950213     C                   MOVEL     XCADDS        GPDCDS
237400001122B0298C* valorizzo i campi comuni per i flag di immissione/modifica/
237500001122  "  C* visualizzazione
237600001122  "  C                   EVAL      AutImmis = ImmPdC
237700001122  "  C                   EVAL      AutModif = ManPdC
237800001122  "  C                   EVAL      AutVisua = VisPdC
237900001122B0298C                   ENDSL
238000950103     C                   ENDIF
238100950913      *
238200950103     C                   ENDSR
238300950301     C/EJECT
238400950913      ************************************************************
238500950913      * DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
238600950913      ************************************************************
238700940715     C     DEFOPZ        BEGSR
238800950913      *
238900950913      * Questa routine deve valorizzare la schiera $FC1/$FR1
239000950913      * con S o N a seconda della utilizzabilità o meno
239100950913      * del tasto funzione correlato
239200950913      *
239300950913      * Inizialmente ripristino le abilitazioni come da schiera a tempo
239400950913      * di compilazione
239500940715     C                   RESET                   $FC1
239600940715     C                   RESET                   $FR1
239700950913      *
239800950913      * Dopodichè, a seconda del richiamo e delle autorizzazioni
239900950913      * invalido alcune opzioni
240000940715     C                   SELECT
240100950913      * Se pgm richiamato in interrogazione disattivo :
240200951006     C     OPZ207        WHENEQ    '01'
240300951006      * F16=Annullamento
240400951006     C                   Z-ADD     16            X
240500951006     C                   MOVE      '0'           $FC1(X)
240600120727A1357 * F17=Disattiva/attiva
240700120727  "  C                   Z-ADD     17            X
240800120727A1357C                   MOVE      '0'           $FC1(X)
240900951006
241000951006     C     OPZ207        WHENEQ    '03'
241100951006      * F16=Annullamento
241200951006     C                   Z-ADD     16            X
241300951006     C                   MOVE      '0'           $FC1(X)
241400120727A1357 * F17=Disattiva/attiva
241500120727  "  C                   Z-ADD     17            X
241600120727A1357C                   MOVE      '0'           $FC1(X)
241700951006
241800950301    >C     OPZ207        WHENEQ    '05'
241900950913      * F4=Ricerca
242000940715     C                   Z-ADD     4             X
242100940720     C                   MOVE      '0'           $FC1(X)
242200950913      * F5=Ripristino
242300940715     C                   Z-ADD     5             X
242400940720     C                   MOVE      '0'           $FC1(X)
242500950913      * F6=Conferma
242600940715     C                   Z-ADD     06            X
242700940720     C                   MOVE      '0'           $FC1(X)
242800951006      * F16=Annullamento
242900951006     C                   Z-ADD     16            X
243000951006     C                   MOVE      '0'           $FC1(X)
243100120727A1357 * F17=Disattiva/Attiva
243200120727  "  C                   Z-ADD     17            X
243300120727A1357C                   MOVE      '0'           $FC1(X)
243400950913      * Se pgm richiamato in annullamento disattivo :
243500950913      * o ripristino annullamento
243600950301     C     OPZ207        WHENEQ    '04'
243700950222     C     *IN04         OREQ      *ON
243800950913      * F4=Ricerca
243900940715     C                   Z-ADD     4             X
244000940720     C                   MOVE      '0'           $FC1(X)
244100950913      * F5=Ripristino
244200940715     C                   Z-ADD     5             X
244300940720     C                   MOVE      '0'           $FC1(X)
244400951006      * F16=Annullamento
244500951006     C                   Z-ADD     16            X
244600951006     C                   MOVE      '0'           $FC1(X)
244700120727A1357 * F17=Disattiva/Attiva
244800120727  "  C                   Z-ADD     17            X
244900120727A1357C                   MOVE      '0'           $FC1(X)
245000950913      *
245100940715     C                   OTHER
245200950913      *
245300940715     C                   ENDSL
245400950913      *
245500940715     C                   ENDSR
245600950913      ************************************************************
245700950913      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
245800950913      ************************************************************
245900941214     C     DEFCAM        BEGSR
246000950913      *
246100950913      * klist
246200950913      *
246300950913    > * ANAPE01L
246400950301    >C     K3APE1        KLIST
246500950301    >C                   KFLD                    APESOC
246600950301    >C                   KFLD                    APEKCC
246700950301    >C                   KFLD                    APEKSC
246800950913      * ANRCO01L
246900950213     C     K3RCO1        KLIST
247000950213     C                   KFLD                    RCOSOC
247100950213     C                   KFLD                    RCOKCC
247200950213     C                   KFLD                    RCOKSC
247300950913      *
247400950913    > * ANPDC01L
247500950227    >C     K3PDC1        KLIST
247600950220    >C                   KFLD                    PDCSOC
247700950220    >C                   KFLD                    PDCKCC
247800950227    >C                   KFLD                    PDCKSC
247900950913    > * ANIND01L
248000950227    >C     K3IND1        KLIST
248100950227    >C                   KFLD                    INDCOD
248200950227    >C                   KFLD                    INDTII
248300950227    >C                   KFLD                    INDCOI
248400950913      * ANtri01L
248500950913     C     K03tri01      KLIST
248600950913     C                   KFLD                    xscsoc
248700950913     C                   KFLD                    tritributo
248800950913     C                   KFLD                    tritriprog
248900000512A0778X* AnTxt03L
249000000512  "  X***  K02Txt03      KLIST
249100000512  "  X***                Kfld                    TxtSocieta
249200000512A0778X***                Kfld                    TxtTpTesto
249300950913      *
249400950913      * Variabili per gestione videate
249500950913      *
249600940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
249700940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
249800940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
249900950213     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
250000940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
250100940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
250200940225     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
250300950913      *
250400950913      * Variabili appoggio
250500950913      *
250600950110     C     *LIKE         DEFINE    XSCSOC        WWWSOC
250700950301     C     *LIKE         DEFINE    D2STA         WWSTA
250800950227     C     *LIKE         DEFINE    D1TPI         WWTII
250900950221     C     *LIKE         DEFINE    RCODTA        WDATE
251000940223     C                   Z-ADD     0             CURR              2 0
251100940223     C                   Z-ADD     0             CURC              2 0
251200940223     C                   MOVE      *ZEROS        WIN              99
251300941201     C                   MOVE      *BLANK        SI                1
251400941201     C                   MOVE      *BLANK        NO                1
251500950913      *
251600950913      * Indici
251700950913      *
251800940223     C                   Z-ADD     0             X                 3 0
251900940223     C                   Z-ADD     0             Y                 3 0
252000950913      *
252100940117     C                   ENDSR
252200940509     C/EJECT
252300950913      ************************************************************
252400950913      * CONTROLLO ABILITAZIONE TASTI
252500950913      ************************************************************
252600940509     C     CTRKEY        BEGSR
252700940509     C                   SELECT
252800940509     C     $TASTO        WHENEQ    F01
252900940720     C     $FC(1)        IFEQ      '0'
253000940509     C                   SETON                                        99
253100940509     C                   ENDIF
253200940509     C     $TASTO        WHENEQ    F02
253300940720     C     $FC(2)        IFEQ      '0'
253400940509     C                   SETON                                        99
253500940509     C                   ENDIF
253600940509     C     $TASTO        WHENEQ    F03
253700940720     C     $FC(3)        IFEQ      '0'
253800940509     C                   SETON                                        99
253900940509     C                   ENDIF
254000940509     C     $TASTO        WHENEQ    F04
254100940720     C     $FC(4)        IFEQ      '0'
254200940509     C                   SETON                                        99
254300940509     C                   ENDIF
254400940509     C     $TASTO        WHENEQ    F05
254500940720     C     $FC(5)        IFEQ      '0'
254600940509     C                   SETON                                        99
254700940509     C                   ENDIF
254800940509     C     $TASTO        WHENEQ    F06
254900940720     C     $FC(6)        IFEQ      '0'
255000940509     C                   SETON                                        99
255100940509     C                   ENDIF
255200940509     C     $TASTO        WHENEQ    F07
255300940720     C     $FC(7)        IFEQ      '0'
255400940509     C                   SETON                                        99
255500940509     C                   ENDIF
255600940509     C     $TASTO        WHENEQ    F08
255700940720     C     $FC(8)        IFEQ      '0'
255800940509     C                   SETON                                        99
255900940509     C                   ENDIF
256000940509     C     $TASTO        WHENEQ    F09
256100940720     C     $FC(09)       IFEQ      '0'
256200940509     C                   SETON                                        99
256300940509     C                   ENDIF
256400940509     C     $TASTO        WHENEQ    F10
256500940720     C     $FC(10)       IFEQ      '0'
256600940509     C                   SETON                                        99
256700940509     C                   ENDIF
256800940509     C     $TASTO        WHENEQ    F11
256900940720     C     $FC(11)       IFEQ      '0'
257000940509     C                   SETON                                        99
257100940509     C                   ENDIF
257200940509     C     $TASTO        WHENEQ    F12
257300940720     C     $FC(12)       IFEQ      '0'
257400940509     C                   SETON                                        99
257500940509     C                   ENDIF
257600940509     C     $TASTO        WHENEQ    F13
257700940720     C     $FC(13)       IFEQ      '0'
257800940509     C                   SETON                                        99
257900940509     C                   ENDIF
258000940509     C     $TASTO        WHENEQ    F14
258100940720     C     $FC(14)       IFEQ      '0'
258200940509     C                   SETON                                        99
258300940509     C                   ENDIF
258400940509     C     $TASTO        WHENEQ    F15
258500940720     C     $FC(15)       IFEQ      '0'
258600940509     C                   SETON                                        99
258700940509     C                   ENDIF
258800940509     C     $TASTO        WHENEQ    F16
258900940720     C     $FC(16)       IFEQ      '0'
259000940509     C                   SETON                                        99
259100940509     C                   ENDIF
259200940509     C     $TASTO        WHENEQ    F17
259300940720     C     $FC(17)       IFEQ      '0'
259400940509     C                   SETON                                        99
259500940509     C                   ENDIF
259600940509     C     $TASTO        WHENEQ    F18
259700940720     C     $FC(18)       IFEQ      '0'
259800940509     C                   SETON                                        99
259900940509     C                   ENDIF
260000940509     C     $TASTO        WHENEQ    F19
260100940720     C     $FC(19)       IFEQ      '0'
260200940509     C                   SETON                                        99
260300940509     C                   ENDIF
260400940509     C     $TASTO        WHENEQ    F20
260500940720     C     $FC(20)       IFEQ      '0'
260600940509     C                   SETON                                        99
260700940509     C                   ENDIF
260800940509     C     $TASTO        WHENEQ    F21
260900940720     C     $FC(21)       IFEQ      '0'
261000940509     C                   SETON                                        99
261100940509     C                   ENDIF
261200940509     C     $TASTO        WHENEQ    F22
261300940720     C     $FC(22)       IFEQ      '0'
261400940509     C                   SETON                                        99
261500940509     C                   ENDIF
261600940509     C     $TASTO        WHENEQ    F23
261700940720     C     $FC(23)       IFEQ      '0'
261800940509     C                   SETON                                        99
261900940509     C                   ENDIF
262000940509     C     $TASTO        WHENEQ    F24
262100940720     C     $FC(24)       IFEQ      '0'
262200940509     C                   SETON                                        99
262300940509     C                   ENDIF
262400940509     C                   ENDSL
262500940509     C                   ENDSR
262600940509     C/EJECT
262700950913      ************************************************************
262800950913      * RIEMPIMENTO TASTI FUNZIONE
262900950913      ************************************************************
263000940509     C     RIEKEY        BEGSR
263100950913      *
263200950913      * Input :
263300950913      * - $FX = schiera con tutti i testi disponibili
263400950913      * - $FC = schiere con i flag di validità associati ai testi
263500950913      * - $FR = schiere con i flag di REVERSE IMAGE
263600950913      * - $ULK = ultimo testo caricato
263700950913      * - $L01 = Lunghezza prima riga
263800950913      * - $L02 = Lunghezza seconda riga
263900950913      * Output :
264000950913      * - $ULK
264100950913      * - $KEY1= 1° riga di tasti funzione
264200950913      * - $KEY2= 2° riga di tasti funzione
264300950913      *
264400961003     C                   CALLb     'XKEYMSG'
264500950913      * input
264600940509     C                   PARM                    $FX
264700940509     C                   PARM                    $FC
264800940509     C                   PARM                    $FR
264900940509     C                   PARM                    $ULK              3 0
265000950913      * output
265100940509     C                   PARM                    $KEY1            79
265200940509     C                   PARM                    $KEY2            79
265300950913      * input
265400941201     C                   PARM      79            $L01              3 0
265500941201     C                   PARM      79            $L02              3 0
265600950913      *
265700060224A1148C                   PARM                    $ALLFUNCT       480
265800060224A1148C*
265900940509     C                   ENDSR
266000950906**  Costanti da file messaggi
266100950213PROMSG    *LIBL     COS0049    record non trovato
266200940128**
26630095040501PROMSG    *LIBL     COS0001    immissione
26640095040502PROMSG    *LIBL     COS0002    modifica
26650095040503PROMSG    *LIBL     COS0003    copia
26660095040504PROMSG    *LIBL     COS0004    annullamento
26670095040505PROMSG    *LIBL     COS0005    visualizzazione
266800940509** TASTI DI FUNZIONE  D1
266900940509PROMSG    *LIBL     KEY0001  01
267000940509PROMSG    *LIBL     KEY0002  02
267100940509PROMSG    *LIBL     KEY0003  03
267200940509PROMSG    *LIBL     KEY0004  04
267300940509PROMSG    *LIBL     KEY0005  05
267400940509PROMSG    *LIBL     KEY0006  06
267500940509PROMSG    *LIBL     KEY0007  07
267600940509PROMSG    *LIBL     KEY0008  08
267700940509PROMSG    *LIBL     KEY0009  09
267800940509PROMSG    *LIBL     KEY0010  10
267900940509PROMSG    *LIBL     KEY0011  11
268000940509PROMSG    *LIBL     KEY0012  12
268100940509PROMSG    *LIBL     KEY0013  13
268200940509PROMSG    *LIBL     KEY0014  14
268300940509PROMSG    *LIBL     KEY0015  15
268400940509PROMSG    *LIBL     KEY0016  16
268500120727PROMSG    *LIBL     KEY0636  17                        A1357
268600120727PROMSG    *LIBL     KEY0018  18                                                 '
268700940509PROMSG    *LIBL     KEY0019  19
268800940509PROMSG    *LIBL     KEY0020  20
268900940509PROMSG    *LIBL     KEY0021  21
269000940509PROMSG    *LIBL     KEY0022  22
269100940509PROMSG    *LIBL     KEY0023  23
269200940509PROMSG    *LIBL     KEY0024  24
269300940509**  ABILITAZIONE D1
2694009407200N  01
2695009407200N  02
2696009407201N  03
2697009407201N  04
2698009407201N  05
2699009502201N  06
2700009407200N  07
2701009407201N  08
2702009407200N  09
2703009411160N  10
2704009407200N  11
2705009407201N  12
2706009407200N  13
2707009407200N  14
2708009407200N  15
2709009510061N  16
2710001207271N  17                                                 A1357
2711009407200N  18
2712009407200N  19
2713009407200N  20
2714009407200N  21
2715009407200N  22
2716009509060N  23   Impostare a '1' se si utilizzano opzioni
2717009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
