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
010900970807    >D  Err720       E                     Inz('0')
011000070129C2002D*-------------
011100070129  "  D* situazione vincoli ritenute d'acconto
011200070129  "  D XCHKMRADS     E DS                  INZ
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
017401160729R235 D* Storico percipienti a regime agevolato
017402160729  "  D SDGSK11DS     E DS                  INZ
017403160729  "  D  RETK11       E                     INZ('0')
017404160729  "  D  OPRK11       E                     INZ('0')
017405160729  "  D  ERRK11       E                     INZ('0')
017406160729  "  D  CMTK11       E                     INZ('0')
017407160729R235 D*-------------
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
035500950228     IANAPE000
035600950228     I              APESOCIETA                  APESOC
035700950228     I              APEKCC                      APEKCC
035800950228     I              APEKSC                      APEKSC
035900950228     I              APETRIBUTO                  APETRB
036000950228     I              APEVARTRIB                  APEVRT
036100950228     I              APETPTESTO                  APETES
036200950301     I              APETPIND                    APETPI
036300950301     I              APECDIND                    APECDI
036400950228     I              APELOCALIT                  APELOC
036500950228     I              APEPROV                     APEPRV
036600950228     I              APESTATO                    APESTA
036700950228     I              APECDESMIN                  APECDE
036800950228     I              APECDFISC                   APECDF
036900950913      *
037000950714     I              APECOGNOME                  APECOG
037100950714     I              APENOME                     APENOM
037200950714     I              APEDTCONT                   APEDTC
037300950913      *-------------
037400950213     IANRCO000
037500950220     I              RCOSOCIETA                  RCOSOC
037600950220     I              RCOKCCSOGG                  RCOKSO
037700950220     I              RCOSOGG                     RCOSOG
037800950220     I              RCOTPIND                    RCOTPI
037900950220     I              RCOCDIND                    RCOCDI
038000950220     I              RCODTANN                    RCODTA
038100950220     I              RCOUTEIMM                   RCOUTE
038200950220     I              RCODTIMM                    RCODTI
038300950220     I              RCOUTEULMO                  RCOUTU
038400950220     I              RCODTULMO                   RCODTU
038500950220     I              RCOTPCONTO                  RCOTPC
038600950220     I              RCOMOVIMLE                  RCOMOV
038700950220     I              RCOTPGEST                   RCOTPG
038800950220     I              RCOSTORICO                  RCOSTO
038900950220     I              RCOSCADENZ                  RCOSCA
039000950220     I              RCORATE                     RCORAT
039100950220     I              RCONATURA                   RCONAT
039200950220     I              RCOSNATURA                  RCOSNA
039300950220     I              RCOAGENTE                   RCOAGE
039400950220     I              RCOPERCIPI                  RCOPER
039500950220     I              RCOTECNICO                  RCOTEC
039600950220     I              RCOSALDNAT                  RCOSAL
039700950220     I              RCORATEORI                  RCORRI
039800950220     I              RCORAPPTES                  RCORAP
039900950220     I              RCOITAEST                   RCOITE
040000950220     I              RCOTPCNTGR                  RCOTPN
040100950220     I              RCOSOCAVAL                  RCOSVA
040200950220     I              RCORGABCTB                  RCOCTB
040300950220     I              RCORGABENT                  RCOENT
040400950220     I              RCORGSQDIM                  RCODIM
040500950220     I              RCOALLEIVA                  RCOALL
040600950220     I              RCOOBBLMC                   RCOOMC
040700950220     I              RCOOBBLDIV                  RCOODV
040800950220     I              RCODIVISA                   RCODIV
040900950220     I              RCOPREPORT                  RCOPRE
041000950220     I              RCOTABPNOT                  RCOPNO
041100950220     I              RCOUNITA                    RCOUNI
041200950220     I              RCOKCCESTR                  RCOKCE
041300950220     I              RCOKSCESTR                  RCOKSE
041400950220     I              RCOBKAMM                    RCOBKA
041500950220     I              RCOKCCFACT                  RCOKCF
041600950220     I              RCOKSCFACT                  RCOKSF
041700950220     I              RCOCTGAN01                  RCOC01
041800950220     I              RCOCTGAN02                  RCOC02
041900950220     I              RCOCTGAN03                  RCOC03
042000950220     I              RCOCTGAN04                  RCOC04
042100950220     I              RCOCTGAN05                  RCOC05
042200950220     I              RCOCTGAN06                  RCOC06
042300950220     I              RCOCTGAN07                  RCOC07
042400950220     I              RCOCTGAN08                  RCOC08
042500950220     I              RCOCTGAN09                  RCOC09
042600950220     I              RCOCTGAN10                  RCOC10
042700950220     I              RCOCTGAN11                  RCOC11
042800950220     I              RCOCTGAN12                  RCOC12
042900950220     I              RCOCTGAN13                  RCOC13
043000950220     I              RCOCTGAN14                  RCOC14
043100950220     I              RCOCTGAN15                  RCOC15
043200950220     I              RCOCTGAN16                  RCOC16
043300950220     I              RCOCTGAN17                  RCOC17
043400950220     I              RCOCTGAN18                  RCOC18
043500950220     I              RCOCTGAN19                  RCOC19
043600950220     I              RCOCTGAN20                  RCOC20
043700950220     I              RCOCTGAN21                  RCOC21
043800950220     I              RCOCTGAN22                  RCOC22
043900950220     I              RCOCTGAN23                  RCOC23
044000950220     I              RCOCTGAN24                  RCOC24
044100950220     I              RCOCTGAN25                  RCOC25
044200950220     I              RCOCTGAN26                  RCOC26
044300950220     I              RCOCTGAN27                  RCOC27
044400950220     I              RCOCTGAN28                  RCOC28
044500950220     I              RCOCTGAN29                  RCOC29
044600950220     I              RCOCTGAN30                  RCOC30
044700950220     I              RCORIFANA1                  RCORA1
044800950220     I              RCOTPRIFA1                  RCOTR1
044900950220     I              RCORIFANA2                  RCORA2
045000950220     I              RCOTPRIFA2                  RCOTR2
045100950220     I              RCORIFANA3                  RCORA3
045200950220     I              RCOTPRIFA3                  RCOTR3
045300950220     I              RCORIFANA4                  RCORA4
045400950220     I              RCOTPRIFA4                  RCOTR4
045500950220     I              RCORIFANA5                  RCORA5
045600950220     I              RCOTPRIFA5                  RCOTR5
045700950220     I              RCORIFFRE1                  RCORF1
045800950220     I              RCORIFFRE2                  RCORF2
045900950220     I              RCORIFFRE3                  RCORF3
046000950220     I              RCOVOCE                     RCOVCE
046100950220     I              RCOANALIT                   RCOANL
046200950220     I              RCOOBBQTA                   RCOOBQ
046300950220     I              RCORGRIPVA                  RCORGI
046400950220     I              RCOPRECCOD                  RCOPRC
046500950220     I              RCOLIVRIS                   RCOLVR
046600950220     I              RCOSESSTRA                  RCOSST
046700950220     I              RCOSESSRIC                  RCOSSR
046800950220     I              RCOFLGOPE                   RCOFLO
046900950220     I              RCOFLGUTE                   RCOFLU
047000950913      *-------------
047100950913      *
047200950913      * Anagrafica piano dei conti
047300950220     IANPDC000
047400950220     I              PDCSOCIETA                  PDCSOC
047500950220     I              PDCDESCR                    PDCRSO
047600950220     I              PDCDESAGG                   PDCDEA
047700950220     I              PDCDESBREV                  PDCDES
047800950220     I              PDCCTGKCC                   PDCCTG
047900950220     I              PDCCTGFISC                  PDCCTF
048000950220     I              PDCSOTGRU                   PDCSTG
048100950220     I              PDCCOOBAN                   PDCCOB
048200950220     I              PDCRICLVEL                  PDCRCV
048300950220     I              PDCFLAGS                    PDCFLG
048400950913      * Anagrafica indirizzi
048500950227     IANIND000
048600950227     I              INDSOGG                     INDCOD
048700950227     I              INDTPIND                    INDTII
048800950227     I              INDCDIND                    INDCOI
048900950227     I              INDDTULMO                   INDDAU
049000950227     I              INDDTINVL                   INDDAI
049100950227     I              INDDTFIVL                   INDDAF
049200950227     I              INDDESFIL                   INDDES
049300950227     I              INDDESAGG                   INDDE1
049400950227     I              INDINDRIZ                   INDVIA
049500950227     I              INDLOCALIT                  INDCIT
049600950227     I              INDCAP                      INDCAP
049700950227     I              INDPROV                     INDPRV
049800950227     I              INDSTATO                    INDSTA
049900950227     I              INDCAB                      INDCAB
050000950227     I              INDLINGUA                   INDLIN
050100950227     I              INDTELEX                    INDTLX
050200950227     I              INDTELEFON                  INDTEL
050300950227     I              INDTELEFAX                  INDTLF
050400950913      * Anagrafica soggetti
050500950227     IANSOG000
050600950227     I              SOGSOGG                     SOGCOD
050700950227     I              SOGDTENTR                   SOGDAT
050800950227     I              SOGDTULMO                   SOGDAU
050900950227     I              SOGDTINVL                   SOGDAI
051000950227     I              SOGDTFIVL                   SOGDAF
051100950227     I              SOGDTNASC                   SOGDAN
051200950227     I              SOGDES                      SOGDES
051300950227     I              SOGDESAGG                   SOGDE1
051400950227     I              SOGCDFISC                   SOGCDF
051500950227     I              SOGPARTIVA                  SOGIVA
051600950227     I              SOGFRCPIVA                  SOGFPI
051700950227     I              SOGISCCIAA                  SOGCIA
051800950227     I              SOGSEX                      SOGSES
051900950227     I              SOGNATGIUR                  SOGNAT
052000950227     I              SOGCODPREC                  SOGCOP
052100950227     I              SOGABI                      SOGABI
052200950227     I              SOGCLFSOG                   SOGCLG
052300950913      *----------------------------------------------------*
052400950913      *                MAIN LINE PROGRAM
052500950913      *----------------------------------------------------*
052600950913      *
052700950913      * inizializzazione variabili
052800940223     C                   EXSR      INZVAR
052900950913      *
053000940223     C     $FINE         DOWEQ     *OFF
053100950213     C     $GEST         CASEQ     'D1'          GESD1
053200950301     C     $GEST         CASEQ     'D2'          GESD2
053300941027     C                   ENDCS
053400941027     C                   ENDDO
053500950913      *
053600950913      * fine programma
053700940325     C                   EXSR      ENDPGM
053800940325     C/EJECT
053900950913      ************************************************************
054000950913      * RICERCHE
054100950913      ************************************************************
054200940325     C     ENDPGM        BEGSR
054300980714A0428 *
054400980714  "   * se il COMMIT non è demandato al pgm chiamante
054500980714  "  C***  CMT207        IFEQ      '0'
054600980714  "   *
054700980714  "   * test se esistono transazioni attive ed eventualmente cosa fare
054800980714  "  C***                Z-ADD     0             EXECOD
054900980714  "  C***                CALL      'XRCMTI'
055000980714  "  C***                PARM                    EXECOD            1 0
055100980714  "   *
055200980714  "  C***                SELECT
055300980714  "   * Annulla richiesta
055400980714  "  C***  EXECOD        WHENEQ    1
055500980714  "  C***                MOVEL     *OFF          $FINE
055600980714  "  C***                MOVEL     *HIVAL        $LASTV
055700980714  "   * Conferma uscita
055800980714  "  C***  EXECOD        WHENEQ    2
055900980714  "  C***                ROLBK
056000980714  "   *
056100980714  "  C***                ENDSL
056200980714  "   *
056300980714A0428C***                ENDIF
056400980714      *
056500980714      * se ho confermato l'uscita proseguo
056600980714     C     $FINE         IFEQ      *ON
056700000515      *
056800000515      * disallocazione rcd nel caso di modifica/annullamento
056900000515     C     OPZ207        IFEQ      '02'
057000000515     C     OPZ207        OREQ      '04'
057100000515     C                   UNLOCK    ANAPE01L                             22
057200000515     C                   ENDIF
057300000515      *
057400950913      * passaggio dati al pgm chiamante
057500950228     C                   MOVEL     A207DS        KPJBU
057600950913      *
057700940325     C                   SETON                                            RT
057800940616     C                   RETURN
057900950913      *
058000941201     C                   ENDIF
058100950913      *
058200940325     C                   ENDSR
058300941201     C/EJECT
058400950913      ************************************************************
058500950913      * RICERCHE
058600950913      ************************************************************
058700941201     C     SEARCH        BEGSR
058800950913      *
058900950913      * determino Riga/Colonna del cursore
059000940127     C                   MOVE      NRG           R$$
059100940127     C                   MOVE      NCL           C$$
059200940127     C                   Z-ADD     RIRI          CURR
059300940127     C                   Z-ADD     COCO          CURC
059400950913      *
059500950913      * I campi che al momento dell'F4 risultano protetti non devono
059600950913      * essere abilitati alla funzione di ricerca (CASO DI ANNULLAMENTO)
059700950913      *
059800941027     C                   SELECT
059900950913      *
060000950913      * FMTD1  - TIPO INDIRIZZO
060100950227     C     H1NMFL        WHENEQ    'D1TPI'
060200950227     C                   RESET                   A045DS
060300950227     C                   MOVEL     '01'          OPZ045
060400950227     C                   MOVEL     D1SOG         COD045
060500950227     C                   MOVEL     A045DS        KPJBU
060600950227     C                   CALL      'ANA045R'
060700950227     C                   PARM                    KPJBA
060800950227     C                   MOVEL     KPJBU         A045DS
060900950227     C                   MOVEL     TII045        WWTII
061000950913      *
061100950227     C     WWTII         IFNE      *BLANK
061200950227     C                   MOVEL     WWTII         D1TPI
061300950227     C                   MOVEL     COI045        D1CDI
061400950227     C                   ENDIF
061500950913      *
061600950913      * FMTD1  - TRIBUTO
061700950301     C     H1NMFL        WHENEQ    'D1TRB'
061800950913    >C                   RESET                   A095ds
061900950913    >C                   MOVE      '01'          OPZ095
062000950913    >C                   CLEAR                   KPJBU
062100950913    >C                   MOVEL     A095DS        KPJBU
062200950913    >C                   CALL      'ANA095R'
062300950913    >C                   PARM                    KPJBA
062400950913    >C                   MOVEL     KPJBU         A095DS
062500950913    >C     TRIbuto095    IFNE      *BLANK
062600950913    >C                   MOVEL     TRIbuto095    D1TRB
062700950913    >C                   MOVE      triprog095    D1VRT
062800950913    >C                   MOVE      *blank        D1trbd
062900950913    >C                   ENDIF
063000970807     C*
063100950913      * FMTD2  - PROVINCIA
063200950301     C     H1NMFL        WHENEQ    'D2PRV'
063300950301     C                   RESET                   XTABDS
063400950301     C                   MOVE      *BLANK        WWKEY
063500950301     C                   MOVE      D2STA         WSTA
063600950301     C                   MOVE      WWKEY         XTAKEY
063700950301     C                   MOVE      '01'          XTAOPZ
063800950301     C                   MOVE      XSCSOC        XTAAZI
063900950301     C                   MOVE      XSCLIN        XTALIN
064000950301     C                   Z-ADD     CURR          XTARIG
064100950301     C                   Z-ADD     CURC          XTACOL
064200950301     C                   CLEAR                   KPJBU
064300950301     C                   MOVEL     XTABDS        KPJBU
064400950301     C                   CALL      'ANGC17R'
064500950301     C                   PARM                    KPJBA
064600950301     C                   MOVEL     KPJBU         XTABDS
064700950301     C     XTAERR        IFNE      '1'
064800950301     C                   MOVE      XTAKEY        WWKEY
064900950301     C                   ELSE
065000950301     C                   MOVE      *BLANK        WWKEY
065100950301     C                   ENDIF
065200950913      *
065300950301     C     WWKEY         IFNE      *BLANK
065400950301     C                   MOVEL     WSTA          D2STA
065500950301     C                   MOVEL     WPRV          D2PRV
065600950301     C                   ENDIF
065700970807      *
065800950913      * FMTD2  - STATO
065900950301     C     H1NMFL        WHENEQ    'D2STA'
066000950301     C                   RESET                   XTABDS
066100950301     C                   MOVE      *BLANK        WWSTA
066200950301     C                   MOVE      WWSTA         XTAKEY
066300950301     C                   MOVE      '01'          XTAOPZ
066400950301     C                   MOVE      XSCSOC        XTAAZI
066500950301     C                   MOVE      XSCLIN        XTALIN
066600950301     C                   Z-ADD     CURR          XTARIG
066700950301     C                   Z-ADD     CURC          XTACOL
066800950301     C                   CLEAR                   KPJBU
066900950301     C                   MOVEL     XTABDS        KPJBU
067000950301     C                   CALL      'ANGG12R'
067100950301     C                   PARM                    KPJBA
067200950301     C                   MOVEL     KPJBU         XTABDS
067300950301     C     XTAERR        IFNE      '1'
067400950301     C                   MOVE      XTAKEY        WWSTA
067500950301     C                   ELSE
067600950301     C                   MOVE      *BLANK        WWSTA
067700950301     C                   ENDIF
067800950913      *
067900950301     C     WWSTA         IFNE      *BLANK
068000950301     C                   MOVEL     WWSTA         D2STA
068100950301     C                   ENDIF
068200000515C0766 *
068300981116 "    * FMTD2  - Sequenza altri tributi
068400981116 "   C     H1NMFL        WHENEQ    'D2SAT'
068500981116 "   C                   RESET                   XTABDS
068600981116 "   C                   MOVE      *blank        XTAKEY
068700981116 "   C                   MOVE      '01'          XTAOPZ
068800981116 "   C                   MOVE      XSCSOC        XTAAZI
068900981116 "   C                   MOVE      XSCLIN        XTALIN
069000981116 "   C                   Z-ADD     CURR          XTARIG
069100981116 "   C                   Z-ADD     CURC          XTACOL
069200981116 "   C                   CLEAR                   KPJBU
069300981116 "   C                   MOVEL     XTABDS        KPJBU
069400981116 "   C                   CALL      'ANG0CSR'
069500981116 "   C                   PARM                    KPJBA
069600981116 "   C                   MOVEL     KPJBU         XTABDS
069700981116 "   C     XTAERR        IFNE      '1'
069800000515 "   C     XTAKEY        andne     *blank
069900981116 "   C                   MOVE      XTAKEY        D2SAT
070000981125 "   C*                  ELSE
070100981125 "   C*                  MOVE      *BLANK        D2SAT
070200981125C0766C                   ENDIF
070300970807      *
070400970807      * FMTD1  - Tipo testo
070500970807     C     H1NMFL        WHENEQ    'D1TES'
070600970807     C                   Reset                   Ana720DS
070700970807     C                   MOVEL     '01'          OPZ720
070800970807     C                   Clear                   Kpjbu
070900970807     C                   MOVEL     Ana720Ds      KPJBU
071000970807     C                   CALL      'ANA720R'
071100970807     C                   PARM                    KPJBA
071200970807     C                   MOVEL     KPJBU         Ana720Ds
071300970807      *
071400970807     C     TpTxt720      IFNE      *BLANK
071500970807     C                   MOVEL     TpTxt720      D1Tes
071600970807     C                   ENDIF
071700950913      *
071800950913      * ricerca non abilitata in questa posizione
071900941027     C                   OTHER
072000940926     C                   SETON                                        9899
072100950913      *
072200941027     C                   ENDSL
072300970807      * imposto pos. del cursore
072400970807     C                   Z-ADD     CURR          H1RIGA
072500970807     C                   Z-ADD     CURC          H1COLO
072600950913      *
072700940506     C                   MOVE      *HIVAL        $LASTV
072800940127     C                   ENDSR
072900940225     C/EJECT
073000950913      ************************************************************
073100950913      * GESTIONE EMISSIONE TASTI FUNZIONE
073200950913      ************************************************************
073300940225     C     GESPIE        BEGSR
073400950913      *
073500940225     C                   SELECT
073600950913      *
073700940506     C     $GEST         WHENEQ    'D1'
073800940506     C     $ULKD1        IFEQ      0
073900950913      * riempimento tasti
074000940506     C                   EXSR      F24D1
074100940506     C                   ELSE
074200940506     C                   WRITE     FMTZ1
074300940506     C                   ENDIF
074400950913      *
074500950301     C     $GEST         WHENEQ    'D2'
074600950301     C     $ULKD2        IFEQ      0
074700950913      * riempimento tasti
074800950301     C                   EXSR      F24D1
074900950301     C                   ELSE
075000950301     C                   WRITE     FMTZ1
075100950301     C                   ENDIF
075200950913      *
075300941221     C                   ENDSL
075400950913      *
075500940225     C                   ENDSR
075600940131     C/EJECT
075700950913      ************************************************************
075800950913      * GESTIONE VIDEO RECORD D1
075900950913      ************************************************************
076000950220     C     GESD1         BEGSR
076100950913      * inizializzazione videata
076200950220   B1C     $INZD1        IFEQ      *ON
076300950220     C                   EXSR      INZD1
076400950220     C                   MOVE      *OFF          $INZD1
076500950220   E1C                   ENDIF
076600950913      * emissione piede videata se proveniente da altra
076700950913      * salvataggio valore formato di provenienza
076800950220   B1C     $LASTV        IFNE      'D1'
076900950220     C                   WRITE     FMTT1
077000950220     C                   EXSR      GESPIE
077100950220     C                   MOVE      $LASTV        $LASTG
077200950220     C                   MOVE      'D1'          $LASTV
077300950220   E1C                   ENDIF
077400950913      *
077500950913      * se esistono errori sulla videata
077600950913      * emetto la write del formato a indicatori spenti per vedere
077700950913      * le eventuali decodifiche
077800950220   B1C     *IN99         IFEQ      *ON
077900950220     C     $GEST         ANDEQ     'D1'
078000950220     C                   MOVEA     *IN           WIN
078100950220     C                   MOVE      *ALL'0'       IN5098           49
078200950220     C                   MOVEA     IN5098        *IN(50)
078300950220     C                   WRITE     FMTD1
078400950220     C                   MOVEA     WIN           *IN
078500950220   E1C                   ENDIF
078600950913      *
078700950220   B1C                   SELECT
078800950913      * Visualizzazione
078900950301     C     OPZ207        WHENEQ    '05'
079000950220     C                   WRITE     FMTD1
079100950220     C                   EXFMT     PROTECT
079200950913      * Immissione/Modifica/Copia
079300950220   X1C                   OTHER
079400950220     C                   EXFMT     FMTD1
079500950220   E1C                   ENDSL
079600950913      * controllo abilitazione tasti funzione
079700950913      *
079800950220     C                   EXSR      CTRKD1
079900950913      *
080000950220   B1C     *IN99         IFEQ      *OFF
080100950913      *
080200950220   B2C                   SELECT
080300950913      * ROLLUP
080400950220     C     $TASTO        WHENEQ    ROLLUP
080500950220     C                   MOVE      'D2'          $GEST
080600950913      * F2=Decodifica
080700950220     C     $TASTO        WHENEQ    F02
080800950220     C                   EXSR      F02D1
080900950913      * F3=Fine
081000950220     C     $TASTO        WHENEQ    F03
081100950220     C                   EXSR      F03D1
081200950913      * F4=Ricerca
081300950220     C     $TASTO        WHENEQ    F04
081400950220     C                   EXSR      SEARCH
081500950913      * F5=Refresh
081600950220     C     $TASTO        WHENEQ    F05
081700950220     C                   EXSR      F05D1
081701160729R235 C* F7=Regime agevolato
081702160729  "  C     $TASTO        WHENEQ    F07
081703160729R235 C                   EXSR      F07D1
081800950913      * F8=Successivo
081900950220     C     $TASTO        WHENEQ    F08
082000950220     C                   EXSR      F08D1
082100950913      * F12=Ritorno
082200950220     C     $TASTO        WHENEQ    F12
082300950220     C                   EXSR      F12D1
082400950913      * F24=Altri tasti
082500950220     C     $TASTO        WHENEQ    F24
082600950220     C                   EXSR      F24D1
082700950913      *
082800950220   X2C                   OTHER
082900950220     C                   EXSR      CTRD1
083000950913      * aggiungere le chiamate ai controlli delle videate supplementari
083100950301     C     *IN99         IFEQ      *OFF
083200950301     C                   EXSR      CTRD2
083300950301     C     *IN99         IFEQ      *ON
083400950301     C                   SETON                                            96
083500950301     C                   ENDIF
083600950301     C                   ENDIF
083700950913      *
083800951006   B3C     $TASTO        IFEQ      F16
083900951006     C     *IN99         ANDEQ     *OFF
084000951006   B3C     $TASTO        OREQ      F06
084100950220     C     *IN99         ANDEQ     *OFF
084101160829R235 C*
084102160829 "   C* se richiamo Fornitori, visualizzo
084103160829 "   C* utilizzo del codice percipiente
084104160829 "   C                   if        $TASTO      = F16   and
084105160829 "   C                             MOD207      = '4'
084106160829 "   C                   EXSR      CallSK20
084107160829 "   C* se uscita senza aver confermato la cancellazione,
084108160829 "   C* segnalo come errore
084109160829 "   C                   if        RETK20     <> '0'
084110160829 "   C                   Eval      *In99       = *On
084111160829 "   C                   endif
084112160829 "   C                   endif
084113160829 "   C*
084114160829R235 C     *IN99         IFEQ      *OFF
084200950913      * eseguo aggiornamento
084300950301     C                   EXSR      AGGAPE
084400950913      * eseguo operazioni del dopo-aggiornamento
084500950220   B4C     *IN99         IFEQ      *OFF
084600950220     C                   EXSR      GESAGG
084700950220   E4C                   ENDIF
084701160829R235 C*
084702160829 "   C                   ENDIF
084703160829R235 C*
084800950220   E3C                   ENDIF
084900950913      *
085000950220   E2C                   ENDSL
085100950913      *
085200950220   E1C                   ENDIF
085300950913      *
085400950220     C                   ENDSR
085500940506     C/EJECT
085600950913      ************************************************************
085700950913      * GESTIONE VIDEO RECORD  D2
085800950913      ************************************************************
085900950301     C     GESD2         BEGSR
086000950913      * emissione piede videata se proveniente da altra
086100950913      * salvataggio valore formato di provenienza
086200950301   B1C     $LASTV        IFNE      'D2'
086300950301     C                   WRITE     FMTT1
086400950301     C                   EXSR      GESPIE
086500950301     C                   MOVE      $LASTV        $LASTG
086600950301     C                   MOVE      'D2'          $LASTV
086700950301   E1C                   ENDIF
086800950913      *
086900950913      * se esistono errori sulla videata
087000950913      * emetto la write del formato a indicatori spenti per vedere
087100950913      * le eventuali decodifiche
087200950301   B1C     *IN99         IFEQ      *ON
087300950301     C     $GEST         ANDEQ     'D2'
087400950301     C                   MOVEA     *IN           WIN
087500950301     C                   MOVE      *ALL'0'       IN5098           49
087600950301     C                   MOVEA     IN5098        *IN(50)
087700950301     C                   WRITE     FMTD2
087800950301     C                   MOVEA     WIN           *IN
087900950301   E1C                   ENDIF
088000950913      *
088100950301   B1C                   SELECT
088200950913      * Annullamento
088300950301     C     OPZ207        WHENEQ    '04'
088400950913      * Visualizzazione
088500950301     C     OPZ207        OREQ      '05'
088600950301     C                   WRITE     FMTD2
088700950301     C                   EXFMT     PROTECT
088800950913      * Immissione/Modifica/Copia
088900950301   X1C                   OTHER
089000950301     C                   EXFMT     FMTD2
089100950301   E1C                   ENDSL
089200950913      * controllo abilitazione tasti funzione
089300950913      *
089400950301     C                   EXSR      CTRKD1
089500950913      *
089600950301   B1C     *IN99         IFEQ      *OFF
089700950913      *
089800950301   B2C                   SELECT
089900950913      * ROLDOWN
090000950301     C     $TASTO        WHENEQ    ROLDWN
090100950301     C                   MOVE      'D1'          $GEST
090200950913      * F3=Fine
090300950301     C     $TASTO        WHENEQ    F03
090400950301     C                   EXSR      F03D1
090500950913      * F4=Ricerca
090600950301     C     $TASTO        WHENEQ    F04
090700950301     C                   EXSR      SEARCH
090800950913      * F5=Refresh
090900950301     C     $TASTO        WHENEQ    F05
091000950301     C                   EXSR      F05D1
091001160729R235 C* F7=Regime agevolato
091002160729  "  C     $TASTO        WHENEQ    F07
091003160729R235 C                   EXSR      F07D1
091100950913      * F8=Successivo
091200950301     C     $TASTO        WHENEQ    F08
091300950301     C                   EXSR      F08D1
091400950913      * F12=Ritorno
091500950301     C     $TASTO        WHENEQ    F12
091600950301     C                   EXSR      F12D1
091700950913      * F24=Altri tasti
091800950301     C     $TASTO        WHENEQ    F24
091900950301     C                   EXSR      F24D1
092000950913      *
092100950301   X2C                   OTHER
092200950301     C                   EXSR      CTRD2
092300950301   B3C     *IN99         IFEQ      *OFF
092400950301     C                   EXSR      CTRD1
092500950301   B4C     *IN99         IFEQ      *ON
092600950301     C                   SETON                                            96
092700950301   E4C                   ENDIF
092800950301   E3C                   ENDIF
092900950913      *
093000960313   B3C     $TASTO        IFEQ      F16
093100960313     C     *IN99         ANDEQ     *OFF
093200960313   B3C     $TASTO        oreq      F06
093300950301     C     *IN99         ANDEQ     *OFF
093301160829R235 C*
093302160829 "   C* se richiamo Fornitori, visualizzo
093303160829 "   C* utilizzo del codice percipiente
093304160829 "   C                   if        $TASTO      = F16   and
093305160829 "   C                             MOD207      = '4'
093306160829 "   C                   EXSR      CallSK20
093307160829 "   C* se uscita senza aver confermato la cancellazione,
093308160829 "   C* segnalo come errore
093309160829 "   C                   if        RETK20     <> '0'
093310160829 "   C                   Eval      *In99       = *On
093311160829 "   C                   endif
093312160829 "   C                   endif
093313160829 "   C*
093314160829R235 C     *IN99         IFEQ      *OFF
093400950913      * eseguo aggiornamento
093500950301     C                   EXSR      AGGAPE
093600950913      * eseguo operazioni del dopo-aggiornamento
093700950301   B4C     *IN99         IFEQ      *OFF
093800950301     C                   EXSR      GESAGG
093900950301   E4C                   ENDIF
093901160825R235 C*
093902160825 "   C                   ENDIF
093903160825R235 C*
094000950301   E3C                   ENDIF
094100950913      *
094200950301   E2C                   ENDSL
094300950913      *
094400950301   E1C                   ENDIF
094500950913      *
094600950301     C                   ENDSR
094700950301     C/EJECT
094800950913      ************************************************************
094900950913      * CONTROLLO VIDEATA D2
095000950913      ************************************************************
095100950301     C     CTRD2         BEGSR
095200950913      *
095300950301     C                   SETOFF                                       99
095400950913      * non imposto pos. del cursore
095500950301     C                   Z-ADD     0             H1RIGA
095600950301     C                   Z-ADD     0             H1COLO
095700950913      *
095800950913      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
095900950913      *
096000950301   B1C     OPZ207        IFNE      '04'
096100950301     C     *IN04         ANDNE     *ON
096200950913      *
096300950913      * Controllo  codice provincia
096400950303     C                   CLEAR                   D2PRVD
096500950714   B2C     D2PRV         IFNE      *BLANKS
096600950301     C                   MOVE      XCACUT        XTBUTE
096700950301     C                   MOVE      '1'           XTBRIC
096800950301     C                   MOVE      XSCSOC        XTBAZI
096900950301     C                   MOVE      *ZERO         XTBKEY
097000950301     C                   MOVE      'C17'         XTBCOD
097100950302     C                   MOVE      D2STA         WSTA
097200950301     C                   MOVE      D2PRV         WPRV
097300950301     C                   MOVE      WWKEY         XTBKEY
097400961003     C                   CALLb     'XATB'
097500950301     C                   PARM                    XATBDS
097600950714   B3C     XTBERR        IFEQ      '1'
097700950302     C                   SETON                                          5499
097800950714   X3C                   ELSE
097900950301     C                   MOVEL     XTBUNI        TABC17
098000950301     C                   MOVEL     DESC17        D2PRVD
098100950714   E3C                   ENDIF
098200950714   E2C                   ENDIF
098300950913      *
098400950913      * Controllo  codice stato
098500950303     C                   CLEAR                   D2STAD
098600950714   B2C     D2STA         IFNE      *BLANKS
098700950301     C                   MOVE      XCACUT        XTBUTE
098800950301     C                   MOVE      '1'           XTBRIC
098900950301     C                   MOVE      XSCSOC        XTBAZI
099000950301     C                   MOVE      *ZERO         XTBKEY
099100950301     C                   MOVE      'G12'         XTBCOD
099200950301     C                   MOVE      D2STA         XTBKEY
099300961003     C                   CALLb     'XATB'
099400950301     C                   PARM                    XATBDS
099500950714   B3C     XTBERR        IFEQ      '1'
099600950301     C                   SETON                                          5399
099700950714   X3C                   ELSE
099800950301     C                   MOVEL     XTBUNI        TABG12
099900950301     C                   MOVEL     DESG12        D2STAD
100000950714   E3C                   ENDIF
100100950714   E2C                   ENDIF
100200950913      *
100300950913      * Controllo  cognome/nome
100400950727     C                   MOVE      '1'           XTBRIC
100500950727     C                   MOVE      XSCSOC        XTBAZI
100600950727     C                   MOVE      *ZERO         XTBKEY
100700950727     C                   MOVE      'G16'         XTBCOD
100800950727     C                   MOVE      SOGNAT        XTBKEY
100900961003     C                   CALLb     'XATB'
101000950727     C                   PARM                    XATBDS
101100950727     C     XTBERR        IFEQ      '0'
101200950727     C                   MOVEL     XTBUNI        TABG16
101300950727     C     TPNG16        IFEQ      '1'
101400950913      * cognome/nome obbligatorio
101500950727     C     D2COG         IFEQ      *BLANK
101600950727     C     D2NOM         OREQ      *BLANK
101700950727     C                   SETON                                          5299
101800950727     C                   ENDIF
101900950727     C                   ELSE
102000950913      * cognome/nome non ammesso
102100950727     C     D2COG         IFNE      *BLANK
102200950727     C     D2NOM         ORNE      *BLANK
102300950727     C                   SETON                                          5699
102400950727     C                   ENDIF
102500950727     C                   ENDIF
102600950727     C                   ELSE
102700950727     C                   SETON                                          6099
102800950727     C                   ENDIF
102900981125C0766 *
103000981125C0766 * Controllo sequenza altri tributi
103100981116 "   c                   clear                   tab0CS
103200981116 "   C     D2SAT         IFNE      *BLANKS
103300981116 "   C                   MOVE      XCACUT        XTBUTE
103400981116 "   C                   MOVE      '1'           XTBRIC
103500981116 "   C                   MOVE      XSCSOC        XTBAZI
103600981116 "   C                   MOVE      *ZERO         XTBKEY
103700981116 "   C                   MOVE      '0CS'         XTBCOD
103800981116 "   C                   MOVE      D2SAT         XTBKEY
103900981116 "   C                   CALLb     'XATB'
104000981116 "   C                   PARM                    XATBDS
104100981116 "   C     XTBERR        IFEQ      '1'
104200981116 "   C                   SETON                                          6399
104300981116 "   C                   ELSE
104400981116 "   C                   MOVEL     XTBUNI        TAB0CS
104500981116 "   C                   ENDIF
104600981116 "   C                   ENDIF
104700981125C0766C                   MOVEL     des0CS        d2satd
104800950913      * Controllo data
104900950913      *--------------------------------------------------------
105000950913      * conversione da GMA a ISO  e controllo validità
105100950913      *--------------------------------------------------------
105200990705b0054C                   If        d2dtc   <>    *zeros
105300961003     C                   CALLb     'XDT4000'
105400950714     C                   PARM                    XDTAMG           10
105500950714     C                   PARM      D2DTC         XDTGMA            6 0
105600950714     C                   PARM      3             XDTSTA            1 0
105700950714     C     XDTSTA        IFNE      0
105800950714     C                   SETON                                        5899
105900950714     C                   ELSE
106000950714     C                   ENDIF
106100990705b0054C                   else
106200000114A0735C******ex-b0054     Eval      D2dtc  =  Udate
106300000114A0735C                   Eval      D2dtc  =  Zdate
106400000114b0054C                   EndIf
106500950913      *
106600950913      *
106700950301   E1C                   ENDIF
106800950301      *
106900950301     C                   ENDSR
107000950301     C/EJECT
107100950913      ************************************************************
107200950913      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
107300950913      ************************************************************
107400940506     C     CTRKD1        BEGSR
107500940506     C                   SETOFF                                       99
107600940506     C                   MOVEA     $FC1          $FC
107700940506     C                   EXSR      CTRKEY
107800940506     C     *IN99         IFEQ      *ON
107900940506     C                   SETON                                        97
108000940506     C                   ENDIF
108100940506     C                   ENDSR
108200950213     C/EJECT
108300940117      ************************************************************
108400940207      * INIZIALIZZAZIONE VIDEATA DATI
108500940117      ************************************************************
108600940127     C     INZD1         BEGSR
108700950913      *
108800940131     C                   CLEAR                   FMTD1
108900950301     C                   CLEAR                   FMTD2
109000950913      *
109100940225     C                   Z-ADD     1             $PIED1
109200950913      *
109300001201B0298C* devo rieseguire il test sulla modalità perchè la CLEAR sul
109400001201  "  C* formato video ha spento anche gli indicatori, usati per
109500001201  "  C* nascondere/proteggere dei campi
109600001201  "  x* Imposto subito indicatori a seconda della modalità in cui mi
109700001201  "  x* trovo
109800001201B0298x* solo capoconti
109900001201     C*
110000001201     C                   SETOFF                                       111213
110100001201     C                   SETOFF                                       14
110200001201     C     MOD207        IFEQ      '1'
110300001201     C                   SETON                                        11
110400001201     C                   ENDIF
110500001201     C     MOD207        IFEQ      '2'
110600001201     C                   SETON                                        12
110700001201     C                   ENDIF
110800001201     C     MOD207        IFEQ      '3'
110900001201     C                   SETON                                        13
111000001201     C                   ENDIF
111100001201     C     MOD207        IFEQ      '4'
111200001201     C                   SETON                                        14
111300001201     C                   ENDIF
111400950913      *
111500950302     C                   MOVE      KCC207        D1KCC
111600950302    >C   12              MOVEL     KSC207        D1KSC
111700950302    >C   13              MOVEL     KSC207        D1CLI
111800950302    >C   14              MOVEL     KSC207        D1FOR
111900950913      *
112000950913      * decodifica capoconto
112100950302    >C                   MOVEL     WWWSOC        PDCSOC
112200950302    >C                   MOVEL     KCC207        PDCKCC
112300950302     C                   MOVE      *BLANKS       PDCKSC
112400950302    >C     K3PDC1        CHAIN     ANPDC000                           21
112500950302     C     *IN21         IFEQ      *OFF
112600950302     C                   MOVEL     PDCDES        D1KCCD
112700950302     C                   ENDIF
112800950913      *
112900950913      * Cerco decodifiche sottoconti
113000950913      *
113100950913      * Sottoconto
113200950302    >C     *IN12         IFEQ      *ON
113300950302    >C                   MOVEL     WWWSOC        PDCSOC
113400950302    >C                   MOVEL     KCC207        PDCKCC
113500950302    >C                   MOVEL     KSC207        PDCKSC
113600950302    >C     K3PDC1        CHAIN     ANPDC000                           21
113700950302     C     *IN21         IFEQ      *OFF
113800950302     C                   MOVEL     PDCDES        D1KSCD
113900950302     C                   ENDIF
114000950302     C                   ENDIF
114100950913      * fornitori
114200950302    >C     *IN14         IFEQ      *ON
114300950302     C                   RESET                   DVFOR
114400950302     C                   MOVEL     WWWSOC        DFRSOC
114500950302     C                   MOVEL     *BLANK        DFRUNI
114600950302     C                   MOVEL     KSC207        DFRFOR
114700950302     C                   MOVEL     *ZERO         DFRFIL
114800950302     C                   Z-ADD     1             DFRNRK
114900950302     C                   MOVEL     '1'           DFRRIC
115000950302     C                   MOVEL     FRN           DFRSET
115100950302     C                   MOVEL     '1'           DFRVIS
115200950302     C                   CALL      'DVFOR'
115300950302     C                   PARM                    DVFOR
115400950302     C                   PARM                    DVOFRN
115500950913      *
115600950302     C     DFRERR        IFEQ      '0'
115700950302     C                   MOVEL     FRNDES        D1KSCD
115800950302     C                   ENDIF
115900950302     C                   ENDIF
116000950913      *
116100950913      * aggancio anrco per altri dati
116200950302    >C                   MOVEL     WWWSOC        RCOSOC
116300950302    >C                   MOVEL     KCC207        RCOKCC
116400950302    >C                   MOVEL     KSC207        RCOKSC
116500951005     C                   SETOFF                                       2122
116600951005     C* gestione record allocato
116700951005   B2C     *IN22         DOUEQ     *OFF
116800951005     C     MSGRTN        ORNE      'R'
116900951005      *
117000951005    >C     K3RCO1        CHAIN(N)  ANRCO000                           2122
117100951005      *
117200951005      * Gestione RCD allocato
117300951005   B3C     *IN22         IFEQ      *ON
117400951005    >C                   MOVEL     'ANRCO00F'    MSGFIL
117500951005     C                   Z-ADD     2             MSGMSG
117600951005     C                   MOVEL     'CR'          MSGTPR
117700951005     C                   EXSR      MSGPGM
117800951005   E3C                   ENDIF
117900951005   E2C                   ENDDO
118000951005     C*
118100951005     C* se record è allocato da altro pgm
118200951005     C* ritorno al pgm guida avvertendolo dell'errore
118300951005     C     *IN22         IFEQ      *ON
118400951005     C                   MOVE      *ON           $FINE
118500951005     C                   MOVE      '1'           ERR207
118600951005     C                   EXSR      ENDPGM
118700951005     C*
118800951005     C* altrimenti
118900951005   X2C                   ELSE
119000950302     C     *IN21         IFEQ      *OFF
119100950302     C                   MOVE      RCOSOG        D1SOG
119200950302     C                   MOVE      RCOTPI        D1TPI
119300950302     C                   MOVE      RCOCDI        D1CDI
119400950913      *
119500950913      * decodifica codice indirizzo di ANRCO
119600950302     C                   CLEAR                   D1DES1
119700950302     C                   CLEAR                   D1VIA1
119800950302     C                   CLEAR                   D1CIT1
119900950302     C                   CLEAR                   D1CAP1
120000950302     C                   CLEAR                   D1PRV1
120100950302     C                   CLEAR                   D1STA1
120200950302     C     D1TPI         IFNE      *BLANK
120300950302     C     D1CDI         ORNE      *BLANKS
120400950302     C                   MOVE      D1SOG         INDCOD
120500950302     C                   MOVE      D1TPI         INDTII
120600950302     C                   MOVE      D1CDI         INDCOI
120700950302     C     K3IND1        CHAIN     ANIND01L                           21
120800950302     C     *IN21         IFEQ      *OFF
120900950302     C                   MOVEL     INDDES        D1DES1
121000950302     C                   MOVEL     INDVIA        D1VIA1
121100950302     C                   MOVEL     INDCIT        D1CIT1
121200950302     C                   MOVEL     INDCAP        D1CAP1
121300950302     C                   MOVEL     INDPRV        D1PRV1
121400950302     C                   MOVEL     INDSTA        D1STA1
121500950302     C                   ENDIF
121600950302     C                   ENDIF
121700950302     C                   ENDIF
121800951005     C                   ENDIF
121900950913      *
122000950913      * Decodifica soggetto
122100950302     C                   CLEAR                   D1SOGD
122200950302     C                   CLEAR                   D1SOGA
122300950913     C     D1SOG         CHAIN     ANSOG01L                           21
122400950302     C     *IN21         IFEQ      *ON
122500950302     C                   SETON                                        9957
122600950302     C                   ELSE
122700950302     C                   MOVEL     SOGDES        D1SOGD
122800950302     C                   MOVEL     SOGDE1        D1SOGA
122900950302     C                   ENDIF
123000950913      *
123100950913      * Decodifica indirizzo principale
123200950302     C                   CLEAR                   D1DES
123300950302     C                   CLEAR                   D1VIA
123400950302     C                   CLEAR                   D1CIT
123500950302     C                   CLEAR                   D1CAP
123600950302     C                   CLEAR                   D1PRV
123700950302     C                   CLEAR                   D1STA
123800950302     C                   MOVE      D1SOG         INDCOD
123900950302     C                   MOVE      *BLANKS       INDTII
124000950302     C                   MOVE      *BLANKS       INDCOI
124100950302     C     K3IND1        CHAIN     ANIND01L                           21
124200950302     C     *IN21         IFEQ      *OFF
124300950302     C                   MOVEL     INDDES        D1DES
124400950302     C                   MOVEL     INDVIA        D1VIA
124500950302     C                   MOVEL     INDCIT        D1CIT
124600950302     C                   MOVEL     INDCAP        D1CAP
124700950302     C                   MOVEL     INDPRV        D1PRV
124800950302     C                   MOVEL     INDSTA        D1STA
124900950302     C                   ENDIF
125000950913      *
125100950913      * valorizzo la chiave univoca per reperire i dati
125200950913      * se non immissione
125300950301   B1C     OPZ207        IFNE      '01'
125400950913      *
125500950913      * impostare i campi chiave ricevuti
125600950301     C                   MOVEL     WWWSOC        APESOC
125700950301     C                   MOVEL     KCC207        APEKCC
125800950301     C                   MOVEL     KSC207        APEKSC
125900950913      *
126000950220   B2C     *IN22         DOUEQ     *OFF
126100950220     C     MSGRTN        ORNE      'R'
126200950913      *
126300950301   B3C     OPZ207        IFEQ      '02'
126400950301    >C     OPZ207        OREQ      '04'
126500951005    >C     K3RCO1        CHAIN     ANAPE000                           2122
126600950913      * se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
126700950213   X3C                   ELSE
126800951005    >C     K3RCO1        CHAIN(N)  ANAPE000                           2122
126900950213   E3C                   ENDIF
127000950913      *
127100950913      * Gestione RCD allocato
127200950213   B3C     *IN22         IFEQ      *ON
127300950301    >C                   MOVEL     'ANAPE00F'    MSGFIL
127400950213     C                   Z-ADD     2             MSGMSG
127500950213     C                   MOVEL     'CR'          MSGTPR
127600950213     C                   EXSR      MSGPGM
127700950213   E3C                   ENDIF
127800941201   E2C                   ENDDO
127900950913      *
128000950913      * se il rcd da gestire non viene trovato
128100950913      * oppure è allocato da altro pgm
128200950913      * ritorno al pgm guida avvertendolo dell'errore
128300941201   B2C     *IN21         IFEQ      *ON
128400940318     C     *IN22         OREQ      *ON
128500940224     C                   MOVE      *ON           $FINE
128600950301     C                   MOVE      '1'           ERR207
128700950913      *
128800950913      * altrimenti
128900941201   X2C                   ELSE
129000950913      * alloco in modo esclusivo il rcd
129100950301   B3C     OPZ207        IFEQ      '02'
129200950301     C     OPZ207        OREQ      '04'
129300951005     C                   UPDATE    ANAPE000
129400941205   E3C                   ENDIF
129500950913      * riempio le videate
129600950213     C                   EXSR      RIED1
129700950913      * richiamo routine dei ctrl per decodificarla
129800940614     C                   EXSR      CTRD1
129900950301     C                   EXSR      CTRD2
130000940224     C                   MOVE      *ALL'0'       IN5098
130100940224     C                   MOVEA     IN5098        *IN(50)
130200950913      * definizione indicatori su video
130300940224     C                   SETOFF                                       02
130400941201     C                   SETOFF                                       04
130500941201   B3C                   SELECT
130600950301     C     OPZ207        WHENEQ    '02'
130700940224     C                   SETON                                        02        PR campi key
130800950301     C     OPZ207        WHENEQ    '04'
130900941201     C                   SETON                                        04        PR tutti fld
131000950301     C     OPZ207        WHENEQ    '05'
131100941201   E3C                   ENDSL
131200950913      *
131300941201   E2C                   ENDIF
131400950913      *
131500950913      * per inserimento valorizzo i campi con i default
131600941214   X1C                   ELSE
131700941214   E1C                   ENDIF
131800950913      *
131900940117     C                   ENDSR
132000950220     C/EJECT
132100950913      ************************************************************
132200950913      * RIEMPIMENTO VIDEATA  D1
132300950913      ************************************************************
132400940207     C     RIED1         BEGSR
132500950913      *
132600950301     C                   MOVE      APETPI        D1TPI
132700950301     C                   MOVE      APECDI        D1CDI
132800950301     C                   MOVE      APETRB        D1TRB
132900950301     C                   MOVE      APEVRT        D1VRT
133000950301     C                   MOVE      APETES        D1TES
133100950301     C                   MOVE      APELOC        D2LOC
133200950301     C                   MOVE      APEPRV        D2PRV
133300950301     C                   MOVE      APESTA        D2STA
133400950301     C                   MOVE      APECDE        D2CDE
133500950301     C                   MOVE      APECDF        D2CDF
133600981125C0766C                   MOVE      APEPINPS      D2PINPS
133700981222C0766C                   MOVEL     APETINPS      D2SAT
133800950913      *
133900950913     C                   move      apedtc        xdtamg
134000950714     C                   MOVE      APECOG        D2COG
134100950714     C                   MOVE      APENOM        D2NOM
134200961003     C                   CALLb     'XDT4000'
134300950913     C                   PARM                    XDTAMG           10
134400950714     C                   PARM                    XDTGMA            6 0
134500950714     C                   PARM      1             XDTSTA            1 0
134600950714     C     XDTSTA        IFNE      0
134700950714     C                   ELSE
134800950714     C                   MOVE      XDTGMA        D2DTC
134900950714     C                   ENDIF
135000950913      *
135100940207     C                   ENDSR
135200940916     C/EJECT
135300950913      ************************************************************
135400950913      * GESTIONE F02 VIDEO D1
135500950913      ************************************************************
135600940916     C     F02D1         BEGSR
135700950913      *
135800950913      * per decodificare la videata richiamo la routine dei ctrl
135900950913      * e poi disattivo gli eventuali messaggi di errore
136000940916     C                   EXSR      CTRD1
136100950913      *
136200940916     C                   MOVE      *ALL'0'       IN5098
136300940916     C                   MOVEA     IN5098        *IN(50)
136400950913      *
136500940916     C                   ENDSR
136600940309     C/EJECT
136700950913      ************************************************************
136800950913      * GESTIONE F03 VIDEO D1
136900950913      ************************************************************
137000940309     C     F03D1         BEGSR
137100950913      *
137200940309     C                   MOVE      *ON           $FINE
137300950301    >C                   MOVE      '1'           RET207
137400950913      * fine programma
137500940325     C                   EXSR      ENDPGM
137600950913      *
137700940325     C                   ENDSR
137800940309     C/EJECT
137900950913      ************************************************************
138000950913      * GESTIONE F05 VIDEO D1
138100950913      ************************************************************
138200940309     C     F05D1         BEGSR
138300950913      *
138400940309     C                   MOVE      *ON           $INZD1
138500950220     C                   MOVE      'D1'          $GEST
138600950913      *
138700940309     C                   ENDSR
138800940309     C/EJECT
138801160729R235 C************************************************************
138802160729  "  C* GESTIONE F07 VIDEO (D1-D2) - Regime agevolato
138803160729  "  C************************************************************
138804160729R235 C     F07D1         BEGSR
138805160729     C*
138806160729     C                   RESET                   SDGSK11DS
138808160729     C     AutModif      IFEQ      *OFF
138809170303R2525C     OPZ207        OReq      '05'
138810160729     C                   MOVE      '05'          OPZK11
138811160729     C                   ELSE
138812160729     C                   MOVE      '02'          OPZK11
138813160729     C                   ENDIF
138814160729     C     CMT207        IFEQ      '1'
138815160729     C                   MOVEL     '1'           CMTK11
138816160729     C                   ELSE
138817160729     C                   MOVEL     '0'           CMTK11
138818160729     C                   ENDIF
138819160729     C                   MoveL     D1Kcc         KCCK11
138820160729     C                   MoveL     D1For         KSCK11
138821160729     C                   Move      *Loval        DtValK11
138824160729     C*
138825160729     C                   MOVEL     SDGSK11DS     KPJBU
138826160729     C*
138827160729     C                   CALL      'SDGSK11R'
138828160729     C                   PARM                    KPJBA
138829160729     C                   PARM                    AUTDS
138830160729     C*
138831160729     C                   MOVEL     KPJBU         SDGSK11DS
138832160729     C*
138833160729     C* ritorno da PGM di variazioni
138834160729     C                   EXSR      RTNK11
138835160729     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
138836160729     C     *IN99         IFEQ      *ON
138837160729     C                   SETON                                        91
138838160729     C                   ENDIF
138839160729     C*
138840160729R235 C                   ENDSR
138841160729R235 C/EJECT
138842160729R235 C************************************************************
138843160729  "  C* GESTIONE RITORNO DA PGM regime agevolato
138844160729  "  C************************************************************
138845160729R235 C     RTNK11        BEGSR
138846160729     C*
138847160729     C* modo di ritorno
138848160729     C*
138849160729     C                   SELECT
138850160729     C* 1 = F3
138851160729     C     RETK11        WHENEQ    '1'
138852160729     C                   MOVE      *ON           $FINE                          fine pgm
138853160729     C*
138854160729     C                   ENDSL
138855160729     C* operazione eseguite dal pgm chiamato
138856160729     C*
138857160729     C                   SELECT
138858160729     C* 1 = eseguito aggiornamento
138859160729     C     OPRK11        WHENEQ    '1'
138860160729     C*
138861160729     C                   ENDSL
138862160729     C*
138863160729     C* funzione non eseguibile per errore :
138864160729     C*
138865160729     C                   SELECT
138866160729     C* 1 = funzione richiamata chiusa in errore
138867160729     C     ERRK11        WHENEQ    '1'
138868160729     C                   SETON                                        9199      emiss.SFLMSGID
138869160729     C* 2 = funzione richiamata senza la dovuta autorizzazione
138870160729     C     ERRK11        WHENEQ    '2'
138871160729     C                   SETON                                        9299      emiss.SFLMSGID
138872160729     C*
138873160729     C                   ENDSL
138874160729     C*
138875160729R235 C                   ENDSR
138876160729R235 C/EJECT
138900950913      ************************************************************
139000950913      * GESTIONE F08 VIDEO D1
139100950913      ************************************************************
139200940309     C     F08D1         BEGSR
139300950913      *
139400940309     C                   MOVE      *ON           $FINE
139500950301     C                   MOVE      '0'           RET207
139600950913      *
139700940309     C                   ENDSR
139800940309     C/EJECT
139900950913      ************************************************************
140000950913      * GESTIONE F12 VIDEO D1
140100950913      ************************************************************
140200940309     C     F12D1         BEGSR
140300950913      *
140400940309     C                   MOVE      *ON           $FINE
140500950301     C                   MOVE      '2'           RET207
140600950913      *
140700940309     C                   ENDSR
140800940309     C/EJECT
140900950913      ************************************************************
141000950913      * GESTIONE F24 VIDEO D1
141100950913      ************************************************************
141200940309     C     F24D1         BEGSR
141300950913      *
141400940506     C                   MOVEA     $FX1          $FX
141500940506     C                   MOVEA     $FC1          $FC
141600940506     C                   MOVEA     $FR1          $FR
141700950220     C                   Z-ADD     $ULKD2        $ULK
141800940506     C                   EXSR      RIEKEY
141900950220     C                   Z-ADD     $ULK          $ULKD1            3 0
142000940720     C                   MOVEA     $FC           $FC1
142100940506     C                   MOVEL     $KEY1         Z1KE1
142200060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
142300940506     C                   MOVEL     $KEY2         Z1KE2
142400940506     C                   WRITE     FMTZ1
142500950913      *
142600940309     C                   ENDSR
142700950220     C/EJECT
142800950913      ************************************************************
142900950913      * CONTROLLO VIDEATA  D1
143000950913      ************************************************************
143100950220     C     CTRD1         BEGSR
143200950913      *
143300940510     C                   SETOFF                                       99
143400950913      * non imposto pos. del cursore
143500950213     C                   Z-ADD     0             H1RIGA
143600950213     C                   Z-ADD     0             H1COLO
143700950913      *
143800950913      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
143900950913      *
144000950301   B1C     OPZ207        IFNE      '04'
144100950213     C     *IN04         ANDNE     *ON
144200950913      *
144300000515A0778 * La lettura di ANTXT viene sostituita con la chiamata al driver
144400000518A0778 * X72TXT
144500970807     C                   Clear                   D1TesD
144600970806     C                   if        D1Tes  <>     *Blank
144700000512A0778X***                Eval      TxTSocieta  = XScSoc
144800000512  "  X***                Eval      TxTTpTesto  = D1Tes
144900000512  "  X***  K02Txt03      Chain     AnTxt03l                           21
145000000512  "  X***                if        *In21       = *On
145100000512  "   *
145200000518  "  C                   eval      XSocieta = XScSoc
145300000518  "  C                   eval      XUnita   = *blank
145400000518  "  C                   eval      XTpTesto = D1Tes
145500000518  "  C                   eval      XLingua  = *blank
145600000523  "  C                   eval      XClaUte  = *blank
145700000518  "  C                   eval      XPosRig = 'A'
145800000518  "  C                   eval      XNrRiga = *zeros
145900000518  "  C                   eval      XRibat = *blanks
146000000518  "  C                   eval      XTesRig = *blanks
146100000518  "  C                   eval      XNrChiavi = 3
146200000518  "  C                   eval      XUsoUnita = '1'
146300000512  "   *
146400000518  "  C                   call      'X72TXT'
146500000518  "  C                   parm      '0'           XEsito
146600000519  "  C                   parm      '6'           XOpe
146700000518  "  C                   parm                    XSocieta
146800000518  "  C                   parm                    XTpTesto
146900000518  "  C                   parm                    XUnita
147000000518  "  C                   parm                    XLingua
147100000523  "  C                   parm                    XClaUte
147200000523  "  C                   parm                    XPosRig
147300000518  "  C                   parm                    XNrRiga
147400000518  "  C                   parm                    XRibat
147500000518  "  C                   parm                    XTesRig
147600000518  "  C                   parm                    XNrChiavi
147700000518  "  C                   parm                    XUsoUnita
147800000512  "   *
147900000518A0778C                   if        XEsito <> *Off
148000970807     C                   Seton                                        5099
148100970807     C                   else
148200000512A0778X***                eval      D1tesD      = TxtTesRig
148300000518A0778C                   eval      D1tesD      = XTesRig
148400970806     C                   endif
148500970806     C                   endif
148600950913      * CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
148700950301   B2C     OPZ207        IFEQ      '01'
148800950301     C     OPZ207        OREQ      '03'
148900950913      *
149000950302     C                   MOVE      WWWSOC        APESOC
149100950302     C                   MOVEL     KCC207        APEKCC
149200950302     C                   MOVE      KSC207        APEKSC
149300950302     C     K3APE1        SETLL     ANAPE000                               21
149400950220     C     *IN21         IFEQ      '1'
149500950220     C                   SETON                                        5999
149600950220     C                   ENDIF
149700950220   E2C                   ENDIF
149800950913      *
149900950913      * Controllo soggetto
150000950913     C     D1SOG         CHAIN     ANSOG01L                           21
150100950303     C     *IN21         IFEQ      *ON
150200950303     C                   SETON                                        9957
150300950303     C                   ENDIF
150400950913      *
150500950913      * Controllo esistenza codice indirizzo digitato
150600950227     C                   CLEAR                   D1DES1
150700950227     C                   CLEAR                   D1VIA1
150800950227     C                   CLEAR                   D1CIT1
150900950227     C                   CLEAR                   D1CAP1
151000950227     C                   CLEAR                   D1PRV1
151100950227     C                   CLEAR                   D1STA1
151200950227     C     D1TPI         IFNE      *BLANK
151300950227     C     D1CDI         ORNE      *BLANKS
151400950227     C                   MOVE      D1SOG         INDCOD
151500950227     C                   MOVE      D1TPI         INDTII
151600950227     C                   MOVE      D1CDI         INDCOI
151700950227     C     K3IND1        CHAIN     ANIND01L                           21
151800950227     C     *IN21         IFEQ      *ON
151900950227     C                   SETON                                        5599
152000950227     C                   ELSE
152100950227     C                   MOVEL     INDDES        D1DES1
152200950227     C                   MOVEL     INDVIA        D1VIA1
152300950227     C                   MOVEL     INDCIT        D1CIT1
152400950227     C                   MOVEL     INDCAP        D1CAP1
152500950227     C                   MOVEL     INDPRV        D1PRV1
152600950227     C                   MOVEL     INDSTA        D1STA1
152700950227     C                   ENDIF
152800950227     C                   ENDIF
152900950913      *
153000950913      * Controllo  codice tributo
153100950301     C                   MOVE      *BLANKS       D1TRBD
153200950913     C                   eval      tritributo = d1trb
153300950913     C                   eval      tritriprog = d1vrt
153400950913     C     k03tri01      chain     antri01l                           21
153500950913     C     *in21         IFEQ      '0'
153600950913     C                   MOVEL     tridescr      D1TRBD
153700950913     C                   else
153800950913     C                   seton                                        5199
153900950913     C                   ENDIF
154000950213   E1C                   ENDIF
154100070129C2002C*
154200070129  "  C                   if        *In99      = *Off
154300070129  "  C                   EXSR      CtrVincoli
154400070129  "  C                   endif
154500070129C2002C*
154600940510     C                   ENDSR
154700940224     C/EJECT
154800070129C2002C************************************************************
154900070129  "  C* Controllo i vincoli sul tributo
155000070129  "  C************************************************************
155100070129C2002C     CtrVincoli    BEGSR
155200070129     C*
155300070129     C                   Clear                   XCHKMRADS
155400070129     C*
155500070129     C                   Eval      XCMSocieta  = XScSoc
155600070129     C* controllo specifici dati passati
155700070129     C                   Eval      XCMMODO     = '3'
155800070129     C                   Eval      XCMKCC      = D1Kcc
155900070129     C                   Eval      XCMKSC      = Ksc207
156000070129     C                   Eval      XCMTributo  = D1Trb
156100070129     C                   Eval      XCMVarTrib  = D1Vrt
156200070129     C                   Eval      XCMDATA     = ZDateIso
156300070129     C*
156400070129     C                   MoveL     XCHKMRADS     KPJBU
156500070129     C                   CALL      'XCHKMRA'
156600070129     C                   PARM                    KPJBA
156700070129     C*
156800070129     C                   MoveL     KPJBU         XCHKMRADS
156900070129     C*
157000070129     C                   if        XCMErrore   = *On
157100070129     C*
157200070129     C                   Select
157300070129     C*
157400070129     C*---------------
157500070129     C* tributo con vincolo 1 e persona fisica con almeno 35 anni
157600070129     C* oppure
157700070129     C* tributo con vincolo 1 e persona non fisica
157800070129     C* oppure
157900070129     C* data di nascita non impostata
158000070129     C                   When      XCMTpErr    = '014'  or
158100070129     C                             XCMTpErr    = '019'  or
158200070129     C                             XCMTpErr    = '013'
158300070129     C                   SetOn                                        6199
158400070129     C*
158500070129     C*---------------
158600070129     C* tributo con vincolo 2 e persona fisica con meno di 35 anni
158700070129     C* oppure
158800070129     C* tributo con vincolo 2 e persona non fisica
158900070129     C* oppure
159000070129     C* data di nascita non impostata
159100070129     C                   When      XCMTpErr    = '016'  or
159200070129     C                             XCMTpErr    = '021'  or
159300070129     C                             XCMTpErr    = '017'
159400070129     C                   SetOn                                        6499
159500070129     C*
159600070129     C                   other
159700070129     C* si sono verificati errori in fase di reperimento dati per
159800070129     C* controllo vincoli su tributo
159900070129     C                   SetOn                                        6299
160000070129     C                   endsl
160100070129     C*
160200070129     C                   endif
160300070129     C*
160400070129C2002C                   ENDSR
160500070129C2002C/EJECT
160600950913      ************************************************************
160700950913      * AGGIORNAMENTO ANAGRAFICA PERCIPIENTI
160800950913      ************************************************************
160900950301     C     AGGAPE        BEGSR
161000951006     C*
161100951006     C                   IF        $TASTO=F16
161200951006     C                   Eval      OPZ207 = '04'
161300951006     C                   Endif
161400950913      * se l'opzione non è di scrittura
161500950913      * devo rileggere il rcd in questione perchè per allocarlo in
161600950913      * modo esclusivo ho già fatto l'UPDATE
161700950301     C     OPZ207        IFNE      '01'
161800950301     C     OPZ207        ANDNE     '03'
161900950301     C                   MOVEL     WWWSOC        APESOC
162000950301     C                   MOVEL     KCC207        APEKCC
162100950301     C                   MOVEL     KSC207        APEKSC
162200950301     C     K3APE1        CHAIN     ANAPE000                           21
162300941205     C                   ENDIF
162400950913      *
162500940207     C                   SELECT
162600950913      *
162700950301     C     OPZ207        WHENEQ    '01'
162800950301     C     OPZ207        OREQ      '03'
162900950913      * dopo riempio campi da video
163000950301     C                   EXSR      RIEAPE
163100951005     C                   EXSR      FLGPERC
163200950301     C                   WRITE     ANAPE000                             22
163300950913      * se rcd non scrivibile attivo errore
163400940318     C     *IN22         IFEQ      *ON
163500941201     C                   SETON                                        9599
163600941027     C                   ENDIF
163700950913      *
163800951006     C     OPZ207        WHENEQ    '02'
163900951006     C                   EXSR      RIEAPE
164000951006     C                   UPDATE    ANAPE000
164100951006     C     OPZ207        WHENEQ    '04'
164200951006     C                   EXSR      FLGPERC
164300951006     C                   DELETE    ANAPE000
164400950913      *
164500940207     C                   ENDSL
164600950323      *
164700950323      * richiama pgm User Exit
164800950323      *
164900950323     C     USRERR        IFEQ      *BLANK
165000950323     C     *IN99         ANDEQ     *OFF
165100950323      * imposta chiave univoca  per user exit
165200950323     C                   MOVE      XSCSOC        XUKSOC
165300950323     C                   MOVE      XSCDUN        XUKDUN
165400950323     C                   MOVEL     NOMPGM        XUKPGM
165500950323    >C                   MOVEL     APESOC        XUKFL1
165600950323    >C                   MOVEL     APEKCC        XUKFL2
165700950323    >C                   MOVEL     APEKSC        XUKFL3
165800950323    >C                   MOVEL     XUKDS         KPJBU
165900950323     C     NOMPGM        CAT       'U':0         USREXT           10
166000950323     C                   CALL      USREXT                               21
166100950323     C                   PARM                    KPJBA
166200950323     C     *IN21         IFEQ      *ON
166300950323     C                   MOVE      *ON           USRERR            1
166400950323     C                   ENDIF
166500950323     C                   ENDIF
166600950913      *
166700940203     C                   ENDSR
166800940224     C/EJECT
166900950913      ************************************************************
167000950913      * RIEMPIMENTO FILE IN AGGIORNAMENTO
167100950913      ************************************************************
167200950301     C     RIEAPE        BEGSR
167300950913      *
167400950913      * impostare i campi del file
167500950913      * ANAPE
167600950301     C                   MOVE      WWWSOC        APESOC
167700950301     C                   MOVEL     KCC207        APEKCC
167800950301     C                   MOVEL     KSC207        APEKSC
167900950301     C                   MOVE      D1TPI         APETPI
168000950301     C                   MOVE      D1CDI         APECDI
168100950301     C                   MOVE      D1TRB         APETRB
168200950301     C                   MOVE      D1VRT         APEVRT
168300950301     C                   MOVE      D1TES         APETES
168400950301     C                   MOVE      D2LOC         APELOC
168500950301     C                   MOVE      D2PRV         APEPRV
168600950301     C                   MOVE      D2STA         APESTA
168700950301     C                   MOVE      D2CDE         APECDE
168800950301     C                   MOVE      D2CDF         APECDF
168900981125C0766C                   MOVE      D2PINPS       APEPINPS
169000981222C0766C                   MOVEL     D2SAT         APETINPS
169100950913      *
169200950714     C                   MOVE      D2COG         APECOG
169300950714     C                   MOVE      D2NOM         APENOM
169400961003     C                   CALLb     'XDT4000'
169500950714     C                   PARM                    XDTAMG           10
169600950714     C                   PARM      D2DTC         XDTGMA            6 0
169700950714     C                   PARM      3             XDTSTA            1 0
169800950714     C     XDTSTA        IFNE      0
169900950714     C                   ELSE
170000950714     C                   MOVE      XDTAMG        APEDTC
170100950714     C                   ENDIF
170200950913      *
170300940207     C                   ENDSR
170400951005     C/EJECT
170500951005      ************************************************************
170600951005      * Attivo percipiente
170700951005      ************************************************************
170800951005     C     FLGPERC       BEGSR
170900951005      *
171000951005      * aggancio anrco per altri dati
171100951005    >C                   MOVEL     WWWSOC        RCOSOC
171200951005    >C                   MOVEL     KCC207        RCOKCC
171300951005    >C                   MOVEL     KSC207        RCOKSC
171400951005     C                   SETOFF                                       2122
171500951005     C* gestione record allocato
171600951005   B2C     *IN22         DOUEQ     *OFF
171700951005     C     MSGRTN        ORNE      'R'
171800951005      *
171900951005    >C     K3RCO1        CHAIN     ANRCO000                           2122
172000951005      *
172100951005      * Gestione RCD allocato
172200951005   B3C     *IN22         IFEQ      *ON
172300951005    >C                   MOVEL     'ANRCO00F'    MSGFIL
172400951005     C                   Z-ADD     2             MSGMSG
172500951005     C                   MOVEL     'CR'          MSGTPR
172600951005     C                   EXSR      MSGPGM
172700951005   E3C                   ENDIF
172800951005   E2C                   ENDDO
172900951005     C*
173000951005     C* se record è allocato da altro pgm
173100951005     C* ritorno al pgm guida avvertendolo dell'errore
173200951005     C     *IN22         IFEQ      *ON
173300951005     C                   MOVE      *ON           $FINE
173400951005     C                   MOVE      '1'           ERR207
173500951005     C                   EXSR      ENDPGM
173600951005     C*
173700951005     C* altrimenti
173800951005   X2C                   ELSE
173900951005     C     *IN21         IFEQ      *OFF
174000951006     c                   Select
174100951006     C                   When      OPZ207 = '01'
174200951005     C                   MOVEL     '1'           RCOPER
174300951006     C                   When      OPZ207 = '04'
174400951006     C                   MOVEL     '0'           RCOPER
174500951006     C                   EndSl
174600951006     C                   UPDATE    ANRCO000
174700951006     C                   ENDIF
174800951005     C                   ENDIF
174900951005      *
175000951005     C                   ENDSR
175100940224     C/EJECT
175200950913      ************************************************************
175300950913      * GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
175400950913      ************************************************************
175500940224     C     GESAGG        BEGSR
175600980714A0428 *
175700980714  "   * eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
175800980714  "  C***  CMT207        IFEQ      '0'
175900980714  "  C***                COMMIT
176000980714A0428C***                ENDIF
176100950913      *
176200950913      * segnalo al pgm chiamante l'avvenuta conferma
176300950301     C                   MOVE      *ON           OPR207
176400950913      * segnalo al pgm chiamante l'aver premuto l'F6
176500950301     C                   MOVE      '0'           RET207
176600950913      *
176700950302     C                   MOVE      *ON           $FINE
176800950913      *
176900940224     C                   ENDSR
177000940318     C/EJECT
177001160825R235 C************************************************************
177002160825 "   C* Chiamata per vedere percipiente dove è utilizzato
177003160825R235 C************************************************************
177004160825     C     CallSK20      BEGSR
177005160825     C*
177006160825     C                   CALL      'SDGSK20R'
177007160829     C                   PARM      XScSoc        SocK20            3
177008160829     C                   PARM      D1Kcc         KccK20            6
177009160829     C                   PARM      D1FOR         KscK20            8
177010160829     C                   PARM      D1KscD        KscDK20          20
177011160829     C                   PARM                    RetK20            1
177012160825     C*
177013160825R235 C                   ENDSR
177014160825R235 C/EJECT
177100950913      ******************************************************
177200950913      * PGM PER GESTIONE MESSAGGI ERRORE                   *
177300950913      ******************************************************
177400940318     C     MSGPGM        BEGSR
177500950913      *
177600950913      * Repesisco messaggio doi errore se msgmsg = 0
177700950213     C     MSGMSG        IFEQ      0
177800950213     C                   MOVEL     $COS(1)       IDMSG
177900961003     C                   CALLb     'XRTVM'
178000950213     C                   PARM                    IDMSG            27
178100950213     C                   PARM                    TXTMSG          100
178200950213     C                   PARM                    ERRMSG            1
178300950213     C     ERRMSG        IFNE      *ON
178400950213     C                   MOVEL     TXTMSG        MSG              70
178500950213     C                   ELSE
178600950213     C                   MOVEL     *ALL'?'       MSG
178700950213     C                   END
178800950213     C                   MOVEL     MSG           MSGMS1
178900950213     C                   ELSE
179000950213     C                   MOVE      *BLANKS       MSGMS1
179100950213     C                   ENDIF
179200950913      *
179300950213     C                   MOVEL     'S'           MSGRSP
179400950213     C                   MOVEL     'S'           MSGEMV
179500950213     C                   Z-ADD     10            MSGRGP
179600950213     C                   MOVEL     'S'           MSGLCK
179700950213     C                   MOVEA     STATUS        STS
179800950213     C                   MOVEL     'N'           MSGCNL
179900950213     C                   MOVEL     'N'           MSGVID
180000950213     C                   MOVEL     'N'           MSGOPE
180100950213     C                   MOVEL     'N'           MSGSTP
180200950213     C                   CALL      'XMSGR'
180300950213     C                   PARM                    XMSGDS
180400950913      *
180500940318     C                   ENDSR
180600940207     C/EJECT
180700950913      ************************************************************
180800950913      * OPERAZIONI INIZIALI
180900950913      ************************************************************
181000940131     C     *INZSR        BEGSR
181100000114A0735C     *DMY          Move      Udate         ZDateISO
181200000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
181300001122A0428x*
181400001122  "  x* Eseguo strcmtctl
181500001122  "  x***                CALLb     'XSTRCMT'
181600001122A0428x***                OPEN      ANAPE01L
181700950913      *
181800950913      * Reperimento parametri
181900950913      *
182000940117     C     *ENTRY        PLIST
182100940117     C                   PARM                    KPJBA
182200950103     C                   PARM                    DSAUT
182300950913      *
182400001122B0298C* siccome le autorizzazioni reperibili sono 2 e non una (ANGFOR,
182500001122  "  C*  e ANGPDC), sposto questo test nella InzVar in modo che venga
182600001122  "  C* fatto sempre - i chiamanti relativi a clienti e fornitori
182700001122  "  C* non passano mai il dato
182800001122  "  x**** PARMS         IFLT      2
182900001122  "  x****               MOVE      *BLANK        AUTDS
183000001122  "  x****               ELSE
183100001122  "  x****               MOVEL     DSAUT         AUTDS
183200001122B0298C****               ENDIF
183300950913      *
183400950913      *
183500950913      * Reperimento tasti di funzione
183600950913      *
183700940506     C                   EXSR      REPKEY
183800950913      *
183900940223     C                   ENDSR
184000940223     C/EJECT
184100950913      ************************************************************
184200950913      * REPERIMENTO TASTI DI FUNZIONE
184300950913      ************************************************************
184400940506     C     REPKEY        BEGSR
184500940506     C                   MOVEA     $FM1          $FM
184600950906     C                   MOVEA     $FC1          $FC
184700940506     C                   EXSR      REPKKK
184800940506     C                   MOVEA     $FX           $FX1
184900950913      *
185000940614     C                   ENDSR
185100940506     C/EJECT
185200950913      ************************************************************
185300950913      * REPERIMENTO TASTI DI FUNZIONE
185400950913      ************************************************************
185500940506     C     REPKKK        BEGSR
185600950913      *
185700940506     C                   CLEAR                   $FX
185800940506     C     1             DO        24            X
185900950913      *
186000940506     C     $FM(X)        IFNE      *BLANK
186100950906     C     $FC(X)        ANDNE     '0'
186200950913      *
186300940506     C                   MOVEL     $FM(X)        IDMSG
186400961003     C                   CALLb     'XRTVM'
186500940506     C                   PARM                    IDMSG            27
186600940506     C                   PARM                    TXTMSG          100
186700940506     C                   PARM                    ERRMSG            1
186800950913      *
186900940506     C     ERRMSG        IFNE      *ON
187000940506     C                   MOVEL     TXTMSG        $FX(X)
187100940506     C                   ELSE
187200940506     C                   MOVEL     *ALL' '       $FX(X)
187300941027     C                   ENDIF
187400950913      *
187500941027     C                   ENDIF
187600950913      *
187700941027     C                   ENDDO
187800950913      *
187900940506     C                   ENDSR
188000941110     C/EJECT
188100950913      ************************************************************
188200950913      * Reperimento dati società
188300950913      ************************************************************
188400941110     C     REPSOC        BEGSR
188500950913      *
188600961003     C                   CALLb     'XSOC'
188700941110     C                   PARM                    TIPXSC            6
188800941110     C                   PARM                    SOCXSC            3
188900941110     C                   PARM                    CDSXSC            9 0
189000941110     C                   PARM                    MODXSC            3
189100941110     C                   PARM      *BLANKS       RTNXSC            1
189200941110     C                   PARM                    XSOCDS
189300941110     C                   PARM                    KPJBA
189400950913      *
189500941110     C                   ENDSR
189600940509     C/EJECT
189700950913      ************************************************************
189800950913      * INIZIALIZZAZIONE VARIABILI
189900950913      ************************************************************
190000940223     C     INZVAR        BEGSR
190100950913      *
190200950913      * Dati da PGM guida
190300950913      *
190400950228     C                   MOVEL     KPJBU         A207DS
190500001122B0298C*
190600001122  "  C* Imposto subito indicatori a seconda della modalità in cui mi
190700001122  "  C* trovo
190800001122  "  C                   SETOFF                                       111213
190900001122  "  C                   SETOFF                                       14
191000001122  "   *  11           GESTIONE CAPOCONTI
191100001122  "  C     Mod207        IFEQ      '1'
191200001122  "  C                   SETON                                        11
191300001122  "  C                   ENDIF
191400001122  "   *  12           GESTIONE SOTTOCONTI
191500001122  "  C     Mod207        IFEQ      '2'
191600001122  "  C                   SETON                                        12
191700001122  "  C                   ENDIF
191800001122  "   *  13           GESTIONE CLIENTI
191900001122  "  C     Mod207        IFEQ      '3'
192000001122  "  C                   SETON                                        13
192100001122  "  C                   ENDIF
192200001122  "   *  14           GESTIONE FORNITORI
192300001122  "  C     Mod207        IFEQ      '4'
192400001122  "  C                   SETON                                        14
192500001122B0298C                   ENDIF
192600950913      *
192700950913      * Reperimento dati società
192800950913      *
192900941201     C                   MOVEL     'SOC001'      TIPXSC
193000941115     C                   MOVEL     *BLANK        SOCXSC
193100941115     C                   EXSR      REPSOC
193200941115     C     RTNXSC        IFNE      '1'
193300941115     C                   MOVEL     XSOCDS        SOC001
193400941115     C                   ELSE
193500941115     C                   RETURN
193600941115     C                   ENDIF
193700950913      *
193800950913      * gestione società di riferimento
193900950301     C     FSC207        IFEQ      'S'
194000950303     C     FSC207        OREQ      ' '
194100950301     C     OPZ207        OREQ      *BLANK
194200950110     C                   MOVE      XSCSOC        WWWSOC
194300950913      * gestione simulata da capofila
194400950110     C                   ELSE
194500950110     C                   MOVE      XSCCPA        WWWSOC
194600950110     C                   ENDIF
194700950913      *
194800950913      * Reperimento autorizzazioni
194900950913      *
195000001122B0298C* siccome le autorizzazioni reperibili sono 2 e non una (ANGFOR,
195100001122  "  C*  e ANGPDC), sposto questo test nella InzVar in modo che venga
195200001122  "  C* fatto sempre - i chiamanti relativi a clienti e fornitori
195300001122  "  C* non passano mai il dato
195400001122  "  C     PARMS         IFLT      2
195500001122  "  C                   MOVE      *BLANK        AUTDS
195600001122  "  C                   ELSE
195700001122  "  C                   MOVEL     DSAUT         AUTDS
195800001122B0298C                   ENDIF
195900950913      * le autorizzazioni potrebbero essere già state passate,
196000950913      * nel qual caso non vanno reperite
196100950103     C     AUTDS         IFEQ      *BLANK
196200950103     C                   EXSR      REPAUT
196300001122B0298C*
196400001122  "  C* siccome l'autorizzazione può essere passata dal chiamante solo
196500001122  "  C* nel caso gestisca i conti diversi da fornitori (che
196600001122  "  C*  invece non la passano mai) posso tranquillamente spostare i
196700001122B0298C* dati nella DS del PdC
196800950103     C                   ELSE
196900950213     C                   MOVEL     XCADDS        GPDCDS
197000001122B0298C* valorizzo i campi comuni per i flag di immissione/modifica/
197100001122  "  C* visualizzazione
197200001122  "  C                   EVAL      AutImmis = ImmPdC
197300001122  "  C                   EVAL      AutModif = ManPdC
197400001122B0298C                   EVAL      AutVisua = VisPdC
197500950103     C                   ENDIF
197600950913      *
197700950913      * Ctrl se l'opzione con cui è stato richiamato è congruente con
197800950913      * le autorizzazione
197900950913      *
198000950103     C                   EXSR      TSTAUT
198100950913      *
198200950913      * Ctrl sul capoconto e sul codice fornitore ricevuto
198300950913      *
198400950301     C                   EXSR      TSTKCC
198500950913      *
198600950913      * Variabili per gestione videate
198700950913      *
198800950913      * Solo in immissione  emetto videata'D1' negli altri
198900950913      * casi non serve (questo perchè i flag devono essere desunti
199000950913      * dalla categoria(nel caso di immissione capoconti) o dal capocon
199100950913      * to (nel caso di immissione sottoconti)
199200940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
199300940223     C                   MOVE      *OFF          $FINE                          fine pgm
199400940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
199500950213     C                   MOVE      *ON           $INZD2                         inizializzione D1
199600940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
199700940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
199800940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
199900950214     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
200000950220     C                   Z-ADD     0             $ULKD3            3 0          piede da emett. D1
200100950913      *
200200950913      * Definizione dei tasti funzione utilizzabili a pgm
200300950913      *
200400950213     C                   EXSR      DEFOPZ
200500950913      *
200600950913      * Variabili appoggio
200700950913      *
200800940224     C                   Z-ADD     0             CURR
200900940224     C                   Z-ADD     0             CURC
201000961003     C                   CALLb     'XDT4000'
201100941201     C                   PARM                    XDTAMG           10
201200000114A0735C*****              PARM      UDATE         XDTGMA            6 0
201300000114A0735C                   PARM      ZDate         XDTGMA            6 0
201400941201     C                   PARM      4             XDTSTA            1 0
201500941201     C                   MOVEL     XDTAMG        WDATE
201600941201     C                   MOVE      XSCON         SI
201700941201     C                   MOVE      XSCOFF        NO
201800950913      * inizializzo tutte le date in AA/MM/GG con la data a 0
201900950913      * richiamo XCHKAUT per reperire il cod.utente interno (XCACUT)
202000941214     C                   CLEAR                   AUTDS
202100950110     C                   MOVEL     WWWSOC        XCASOC                         SOCIETA
202200941214     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
202300941214     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
202400961003     C                   CALLb     'XCHKAUT'
202500941214     C                   PARM                    AUTDS
202600950913      *
202700950913      * Valorizzazione campi univoci testate
202800950913      *
202900940207     C                   CLEAR                   FMTT1
203000940207     C                   MOVEL     KNSIF         NOMSIF
203100941110     C                   MOVEL     XSCDSI        NOMDIT
203200941027     C                   MOVEL     DSPGM         NOMPGM
203300950913      * recupero il sottotitolo (se c'è)
203400950427     C                   MOVEL     *BLANK        T1TIT
203500950427     C     SOTTIT        IFNE      *BLANK
203600950427     C                   MOVEL     SOTTIT        IDMSG
203700961003     C                   CALLb     'XRTVM'
203800950427     C                   PARM                    IDMSG            27
203900950427     C                   PARM                    TXTMSG          100
204000950427     C                   PARM                    ERRMSG            1
204100950427     C                   PARM      *ON           CTRMSG            1            centratura
204200950427     C                   PARM      30            LENMSG            3 0          lun output.
204300950427     C     ERRMSG        IFNE      *ON
204400950427     C                   MOVEL     TXTMSG        T1TIT
204500950427     C                   ELSE
204600950427     C                   MOVEL     *ALL'?'       T1TIT
204700950427     C                   ENDIF
204800950427     C                   ENDIF
204900950913      * recupero il tipo operazione
205000940224     C                   Z-ADD     1             X
205100950301     C     OPZ207        LOOKUP    $OZ(X)                                 21
205200940224     C     *IN21         IFEQ      *ON
205300950405     C                   MOVEL     $ID(X)        IDMSG
205400961003     C                   CALLb     'XRTVM'
205500950405     C                   PARM                    IDMSG            27
205600950405     C                   PARM                    TXTMSG          100
205700950405     C                   PARM                    ERRMSG            1
205800950405     C                   PARM      *ON           CTRMSG            1            centratura
205900950405     C                   PARM      30            LENMSG            3 0          lun output.
206000950405     C     ERRMSG        IFNE      *ON
206100950405     C                   MOVEL     TXTMSG        T1OPE
206200950405     C                   ELSE
206300950405     C                   MOVEL     *ALL'?'       T1OPE
206400950405     C                   ENDIF
206500950405     C                   ENDIF
206600950913      *
206700950913      * Inizializzazione/Decodifica prima videata
206800950913      *
206900940224     C                   EXSR      INZD1
207000940224     C                   MOVE      *OFF          $INZD1
207100950913      *
207200940117     C                   ENDSR
207300950301     C/EJECT
207400950913      ************************************************************
207500950913      * TEST CAPOCONTO
207600950913      ************************************************************
207700950913      * il fornitore deve essere attivo contabilmente
207800950913      * Il capoconto deve essere gestito a soggetti
207900950913      *
208000950301     C     TSTKCC        BEGSR
208100950913      *
208200951006      * Controlli contabilità
208300950301    >C                   MOVEL     WWWSOC        RCOSOC
208400950301    >C                   MOVEL     KCC207        RCOKCC
208500950914    >C                   MOVEL     KSC207        RCOKSC
208600951005     C* gestione record allocato
208700951005     C                   SETOFF                                       2122
208800951005   B2C     *IN22         DOUEQ     *OFF
208900951005     C     MSGRTN        ORNE      'R'
209000951005      *
209100951005    >C     K3RCO1        CHAIN(N)  ANRCO000                           2122
209200951005      *
209300951005      * Gestione RCD allocato
209400951005   B3C     *IN22         IFEQ      *ON
209500951005    >C                   MOVEL     'ANRCO00F'    MSGFIL
209600951005     C                   Z-ADD     2             MSGMSG
209700951005     C                   MOVEL     'CR'          MSGTPR
209800951005     C                   EXSR      MSGPGM
209900951005   E3C                   ENDIF
210000951005   E2C                   ENDDO
210100951006     C* se  allocato da altro pgm
210200951005     C* ritorno al pgm guida avvertendolo dell'errore
210300951005     C     *IN22         IFEQ      *ON
210400951005     C                   MOVE      *ON           $FINE
210500951005     C                   MOVE      '1'           ERR207
210600951005     C                   EXSR      ENDPGM
210700951005     C*
210800951005     C* altrimenti
210900951005   X2C                   ELSE
211000951005     C* Record non trovato
211100951005     C* il fornitore deve essere attivo contabilmente
211200951005     C     *IN21         IFEQ      *ON
211300951005     C     MOD207        ANDEQ     '4'
211400951005     C                   MOVE      *ON           $FINE
211500951005     C                   MOVE      '3'           ERR207
211600951005     C                   EXSR      ENDPGM
211700951005     c                   endif
211800951006     C* Record trovato
211900950301     C     *IN21         IFEQ      *OFF
212000951006     C*
212100951006     C* deve essere gestito a soggetti
212200951006     C     RCOKSO        IFNE      *ON
212300950301     C                   MOVE      '4'           ERR207                         no soggetti
212400950301     C                   MOVE      *ON           $FINE
212500950301     C                   EXSR      ENDPGM
212600950301     C                   ENDIF
212700951005     C*
212800951005     C* PER fornitori la
212900951005      * Sottonatura deve essere 'F'
213000951005     C     MOD207        IFEQ      '4'
213100951005     C     RCOSNA        ANDNE     'F'
213200951005     C                   MOVE      '5'           ERR207                         sottonat. F
213300951005     C                   MOVE      *ON           $FINE
213400951005     C                   EXSR      ENDPGM
213500951005     C                   ENDIF
213600951006     C*
213700951006     C                   ENDIF
213800951005     C*
213900951005     C                   ENDIF
214000950913      *
214100950301     C                   ENDSR
214200950301     C/EJECT
214300950913      ************************************************************
214400950913      * TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
214500950913      ************************************************************
214600950103     C     TSTAUT        BEGSR
214700950913      *
214800950103     C                   SELECT
214900950913      *
215000950913      * se richiamato in immissione
215100950301     C     OPZ207        WHENEQ    '01'
215200950301     C     OPZ207        OREQ      '03'
215300950913      * ma l'utente non è abilitato
215400001122B0298x**** IMMPDC        IFEQ      *OFF
215500001122B0298C     AutImmis      IFEQ      *OFF
215600950913      * esco da pgm avvertendolo
215700950301     C                   MOVE      '2'           ERR207
215800950104     C                   MOVE      *ON           $FINE
215900950103     C                   EXSR      ENDPGM
216000950216     C                   ENDIF
216100950913      *
216200950913      * se richiamato in modifica
216300950301     C     OPZ207        WHENEQ    '02'
216400950913      * ma l'utente non è abilitato
216500001122B0298x**** MANPDC        ANDEQ     *OFF
216600001122B0298C     AutModif      ANDEQ     *OFF
216700950913      * testo se l'utente è abilitato almeno alla visualizzazione
216800001122B0298x**** VISPDC        IFEQ      *ON
216900001122B0298C     AutVisua      IFEQ      *ON
217000950301     C                   MOVE      '05'          OPZ207
217100950103     C                   ELSE
217200950913      * altrimenti esco da pgm avvertendolo
217300950301     C                   MOVE      '2'           ERR207
217400950104     C                   MOVE      *ON           $FINE
217500950103     C                   EXSR      ENDPGM
217600950103     C                   ENDIF
217700950913      *
217800950913      * se richiamato in copia
217900950301     C     OPZ207        WHENEQ    '03'
218000950913      * ma l'utente non è abilitato
218100001122B0298x**** IMMPDC        ANDEQ     *OFF
218200001122B0298C     AutImmis      ANDEQ     *OFF
218300950913      * esco da pgm avvertendolo
218400950301     C                   MOVE      '2'           ERR207
218500950104     C                   MOVE      *ON           $FINE
218600950103     C                   EXSR      ENDPGM
218700950913      *
218800950913      * se richiamato in annullamento
218900950301     C     OPZ207        WHENEQ    '04'
219000950913      * ma l'utente non è abilitato
219100950213     C     ANNPDC        ANDEQ     *OFF
219200950913      * esco da pgm avvertendolo
219300950301     C                   MOVE      '2'           ERR207
219400950104     C                   MOVE      *ON           $FINE
219500950103     C                   EXSR      ENDPGM
219600950913      *
219700950913      * se richiamato in visualizzazione
219800950301     C     OPZ207        WHENEQ    '05'
219900950913      * ma l'utente non è abilitato
220000001122B0298x**** VISPDC        ANDEQ     *OFF
220100001122B0298C     AutVisua      ANDEQ     *OFF
220200950913      * esco da pgm avvertendolo
220300950301     C                   MOVE      '2'           ERR207
220400950104     C                   MOVE      *ON           $FINE
220500950103     C                   EXSR      ENDPGM
220600950913      *
220700950103     C                   ENDSL
220800950913      *
220900950103     C                   ENDSR
221000950103     C/EJECT
221100950913      ************************************************************
221200950913      * REPERIMENTO AUTORIZZAZIONI UTENTE
221300950913      ************************************************************
221400950103     C     REPAUT        BEGSR
221500950913      *
221600950103     C                   CLEAR                   AUTDS
221700950913      *
221800950123     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
221900950103     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
222000950103     C                   Z-ADD     0             XCAGRP                         GRUPPO
222100950103     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
222200001122B0298C* a seconda della sottonatura del conto (cliento o fornitore o
222300001122  "  C*  altro conto) uso un'autorizzazione diversa
222400001122  "  C                   SELECT
222500001122  "  C* non dovrebbe mai accadere che un percipiente sia un cliente!
222600001122  "  C***                WHEN      *in13 = *on
222700001122  "  C***                MOVEL     'ANGCLI'      XCAFNC                         FUNZIONE
222800001122  "  C                   WHEN      *in14 = *on
222900001122  "  C                   MOVEL     'ANGFOR'      XCAFNC                         FUNZIONE
223000001122B0298C                   OTHER
223100950213     C                   MOVEL     'ANGPDC'      XCAFNC                         FUNZIONE
223200001122B0298C                   ENDSL
223300950103     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
223400950103     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
223500950103     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
223600950913      *
223700961003     C                   CALLb     'XCHKAUT'
223800950103     C                   PARM                    AUTDS
223900950913      *
224000950913      * errori/non abilitazione
224100950103     C     XCARTN        IFNE      0
224200950227     C                   MOVE      *ON           $FINE
224300950103     C                   EXSR      ENDPGM
224400950103     C                   ELSE
224500001122B0298C* a seconda della sottonatura del conto (cliento o fornitore o
224600001122  "  C*  altro conto) uso un'autorizzazione diversa
224700001122  "  C                   SELECT
224800001122  "  C                   WHEN      *in14 = *on
224900001122  "  C                   MOVEL     XCADDS        ANGFORDS
225000001122  "  C* valorizzo i campi comuni per i flag di immissione/modifica/
225100001122  "  C* visualizzazione
225200001122  "  C                   EVAL      AutImmis = ImmCtbFor
225300001122  "  C* nel caso il dato fosse blank
225400001122  "  C                   IF        ImmCtbFor = *blank
225500001122  "  C* uso il flag dei dati gestionali
225600001122  "  C                   EVAL      AutImmis = ImmFor
225700001122  "  C                   ENDIF
225800001122  "  C                   EVAL      AutModif = ManCtbFor
225900001122  "  C* nel caso il dato fosse blank
226000001122  "  C                   IF        ManCtbFor = *blank
226100001122  "  C* uso il flag dei dati gestionali
226200001122  "  C                   EVAL      AutModif = ManFor
226300001122  "  C                   ENDIF
226400001122  "  C                   EVAL      AutVisua = VisCtbFor
226500001122  "  C* nel caso il dato fosse blank
226600001122  "  C                   IF        VisCtbFor = *blank
226700001122  "  C* uso il flag dei dati gestionali
226800001122  "  C                   EVAL      AutVisua = VisFor
226900001122  "  C                   ENDIF
227000001122B0298C                   OTHER
227100950213     C                   MOVEL     XCADDS        GPDCDS
227200001122B0298C* valorizzo i campi comuni per i flag di immissione/modifica/
227300001122  "  C* visualizzazione
227400001122  "  C                   EVAL      AutImmis = ImmPdC
227500001122  "  C                   EVAL      AutModif = ManPdC
227600001122  "  C                   EVAL      AutVisua = VisPdC
227700001122B0298C                   ENDSL
227800950103     C                   ENDIF
227900950913      *
228000950103     C                   ENDSR
228100950301     C/EJECT
228200950913      ************************************************************
228300950913      * DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
228400950913      ************************************************************
228500940715     C     DEFOPZ        BEGSR
228600950913      *
228700950913      * Questa routine deve valorizzare la schiera $FC1/$FR1
228800950913      * con S o N a seconda della utilizzabilità o meno
228900950913      * del tasto funzione correlato
229000950913      *
229100950913      * Inizialmente ripristino le abilitazioni come da schiera a tempo
229200950913      * di compilazione
229300940715     C                   RESET                   $FC1
229400940715     C                   RESET                   $FR1
229500950913      *
229600950913      * Dopodichè, a seconda del richiamo e delle autorizzazioni
229700950913      * invalido alcune opzioni
229800940715     C                   SELECT
229900950913      * Se pgm richiamato in interrogazione disattivo :
230000951006     C     OPZ207        WHENEQ    '01'
230100951006      * F16=Annullamento
230200951006     C                   Z-ADD     16            X
230300951006     C                   MOVE      '0'           $FC1(X)
230400951006
230500951006     C     OPZ207        WHENEQ    '03'
230600951006      * F16=Annullamento
230700951006     C                   Z-ADD     16            X
230800951006     C                   MOVE      '0'           $FC1(X)
230900951006
231000950301    >C     OPZ207        WHENEQ    '05'
231100950913      * F4=Ricerca
231200940715     C                   Z-ADD     4             X
231300940720     C                   MOVE      '0'           $FC1(X)
231400950913      * F5=Ripristino
231500940715     C                   Z-ADD     5             X
231600940720     C                   MOVE      '0'           $FC1(X)
231700950913      * F6=Conferma
231800940715     C                   Z-ADD     06            X
231900940720     C                   MOVE      '0'           $FC1(X)
232000951006      * F16=Annullamento
232100951006     C                   Z-ADD     16            X
232200951006     C                   MOVE      '0'           $FC1(X)
232300950913      * Se pgm richiamato in annullamento disattivo :
232400950913      * o ripristino annullamento
232500950301     C     OPZ207        WHENEQ    '04'
232600950222     C     *IN04         OREQ      *ON
232700950913      * F4=Ricerca
232800940715     C                   Z-ADD     4             X
232900940720     C                   MOVE      '0'           $FC1(X)
233000950913      * F5=Ripristino
233100940715     C                   Z-ADD     5             X
233200940720     C                   MOVE      '0'           $FC1(X)
233300951006      * F16=Annullamento
233400951006     C                   Z-ADD     16            X
233500951006     C                   MOVE      '0'           $FC1(X)
233600950913      *
233700940715     C                   OTHER
233800950913      *
233900940715     C                   ENDSL
233901160729R235 C*
233902160729 "   C* se richiamo non Fornitori (?), disabilito il tasto F7=Regime agevolato
233903160729 "   C                   if        MOD207     <> '4'
233904160729 "    * F7 =Regime agevolato
233905160729 "   C                   Z-ADD     07            X
233906160729 "   C                   MOVE      '0'           $FC1(X)
233907160729R235 C                   endif
234000950913      *
234100940715     C                   ENDSR
234200950913      ************************************************************
234300950913      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
234400950913      ************************************************************
234500941214     C     DEFCAM        BEGSR
234600950913      *
234700950913      * klist
234800950913      *
234900950913    > * ANAPE01L
235000950301    >C     K3APE1        KLIST
235100950301    >C                   KFLD                    APESOC
235200950301    >C                   KFLD                    APEKCC
235300950301    >C                   KFLD                    APEKSC
235400950913      * ANRCO01L
235500950213     C     K3RCO1        KLIST
235600950213     C                   KFLD                    RCOSOC
235700950213     C                   KFLD                    RCOKCC
235800950213     C                   KFLD                    RCOKSC
235900950913      *
236000950913    > * ANPDC01L
236100950227    >C     K3PDC1        KLIST
236200950220    >C                   KFLD                    PDCSOC
236300950220    >C                   KFLD                    PDCKCC
236400950227    >C                   KFLD                    PDCKSC
236500950913    > * ANIND01L
236600950227    >C     K3IND1        KLIST
236700950227    >C                   KFLD                    INDCOD
236800950227    >C                   KFLD                    INDTII
236900950227    >C                   KFLD                    INDCOI
237000950913      * ANtri01L
237100950913     C     K03tri01      KLIST
237200950913     C                   KFLD                    xscsoc
237300950913     C                   KFLD                    tritributo
237400950913     C                   KFLD                    tritriprog
237500000512A0778X* AnTxt03L
237600000512  "  X***  K02Txt03      KLIST
237700000512  "  X***                Kfld                    TxtSocieta
237800000512A0778X***                Kfld                    TxtTpTesto
237900950913      *
238000950913      * Variabili per gestione videate
238100950913      *
238200940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
238300940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
238400940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
238500950213     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
238600940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
238700940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
238800940225     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
238900950913      *
239000950913      * Variabili appoggio
239100950913      *
239200950110     C     *LIKE         DEFINE    XSCSOC        WWWSOC
239300950301     C     *LIKE         DEFINE    D2STA         WWSTA
239400950227     C     *LIKE         DEFINE    D1TPI         WWTII
239500950221     C     *LIKE         DEFINE    RCODTA        WDATE
239600940223     C                   Z-ADD     0             CURR              2 0
239700940223     C                   Z-ADD     0             CURC              2 0
239800940223     C                   MOVE      *ZEROS        WIN              99
239900941201     C                   MOVE      *BLANK        SI                1
240000941201     C                   MOVE      *BLANK        NO                1
240100950913      *
240200950913      * Indici
240300950913      *
240400940223     C                   Z-ADD     0             X                 3 0
240500940223     C                   Z-ADD     0             Y                 3 0
240600950913      *
240700940117     C                   ENDSR
240800940509     C/EJECT
240900950913      ************************************************************
241000950913      * CONTROLLO ABILITAZIONE TASTI
241100950913      ************************************************************
241200940509     C     CTRKEY        BEGSR
241300940509     C                   SELECT
241400940509     C     $TASTO        WHENEQ    F01
241500940720     C     $FC(1)        IFEQ      '0'
241600940509     C                   SETON                                        99
241700940509     C                   ENDIF
241800940509     C     $TASTO        WHENEQ    F02
241900940720     C     $FC(2)        IFEQ      '0'
242000940509     C                   SETON                                        99
242100940509     C                   ENDIF
242200940509     C     $TASTO        WHENEQ    F03
242300940720     C     $FC(3)        IFEQ      '0'
242400940509     C                   SETON                                        99
242500940509     C                   ENDIF
242600940509     C     $TASTO        WHENEQ    F04
242700940720     C     $FC(4)        IFEQ      '0'
242800940509     C                   SETON                                        99
242900940509     C                   ENDIF
243000940509     C     $TASTO        WHENEQ    F05
243100940720     C     $FC(5)        IFEQ      '0'
243200940509     C                   SETON                                        99
243300940509     C                   ENDIF
243400940509     C     $TASTO        WHENEQ    F06
243500940720     C     $FC(6)        IFEQ      '0'
243600940509     C                   SETON                                        99
243700940509     C                   ENDIF
243800940509     C     $TASTO        WHENEQ    F07
243900940720     C     $FC(7)        IFEQ      '0'
244000940509     C                   SETON                                        99
244100940509     C                   ENDIF
244200940509     C     $TASTO        WHENEQ    F08
244300940720     C     $FC(8)        IFEQ      '0'
244400940509     C                   SETON                                        99
244500940509     C                   ENDIF
244600940509     C     $TASTO        WHENEQ    F09
244700940720     C     $FC(09)       IFEQ      '0'
244800940509     C                   SETON                                        99
244900940509     C                   ENDIF
245000940509     C     $TASTO        WHENEQ    F10
245100940720     C     $FC(10)       IFEQ      '0'
245200940509     C                   SETON                                        99
245300940509     C                   ENDIF
245400940509     C     $TASTO        WHENEQ    F11
245500940720     C     $FC(11)       IFEQ      '0'
245600940509     C                   SETON                                        99
245700940509     C                   ENDIF
245800940509     C     $TASTO        WHENEQ    F12
245900940720     C     $FC(12)       IFEQ      '0'
246000940509     C                   SETON                                        99
246100940509     C                   ENDIF
246200940509     C     $TASTO        WHENEQ    F13
246300940720     C     $FC(13)       IFEQ      '0'
246400940509     C                   SETON                                        99
246500940509     C                   ENDIF
246600940509     C     $TASTO        WHENEQ    F14
246700940720     C     $FC(14)       IFEQ      '0'
246800940509     C                   SETON                                        99
246900940509     C                   ENDIF
247000940509     C     $TASTO        WHENEQ    F15
247100940720     C     $FC(15)       IFEQ      '0'
247200940509     C                   SETON                                        99
247300940509     C                   ENDIF
247400940509     C     $TASTO        WHENEQ    F16
247500940720     C     $FC(16)       IFEQ      '0'
247600940509     C                   SETON                                        99
247700940509     C                   ENDIF
247800940509     C     $TASTO        WHENEQ    F17
247900940720     C     $FC(17)       IFEQ      '0'
248000940509     C                   SETON                                        99
248100940509     C                   ENDIF
248200940509     C     $TASTO        WHENEQ    F18
248300940720     C     $FC(18)       IFEQ      '0'
248400940509     C                   SETON                                        99
248500940509     C                   ENDIF
248600940509     C     $TASTO        WHENEQ    F19
248700940720     C     $FC(19)       IFEQ      '0'
248800940509     C                   SETON                                        99
248900940509     C                   ENDIF
249000940509     C     $TASTO        WHENEQ    F20
249100940720     C     $FC(20)       IFEQ      '0'
249200940509     C                   SETON                                        99
249300940509     C                   ENDIF
249400940509     C     $TASTO        WHENEQ    F21
249500940720     C     $FC(21)       IFEQ      '0'
249600940509     C                   SETON                                        99
249700940509     C                   ENDIF
249800940509     C     $TASTO        WHENEQ    F22
249900940720     C     $FC(22)       IFEQ      '0'
250000940509     C                   SETON                                        99
250100940509     C                   ENDIF
250200940509     C     $TASTO        WHENEQ    F23
250300940720     C     $FC(23)       IFEQ      '0'
250400940509     C                   SETON                                        99
250500940509     C                   ENDIF
250600940509     C     $TASTO        WHENEQ    F24
250700940720     C     $FC(24)       IFEQ      '0'
250800940509     C                   SETON                                        99
250900940509     C                   ENDIF
251000940509     C                   ENDSL
251100940509     C                   ENDSR
251200940509     C/EJECT
251300950913      ************************************************************
251400950913      * RIEMPIMENTO TASTI FUNZIONE
251500950913      ************************************************************
251600940509     C     RIEKEY        BEGSR
251700950913      *
251800950913      * Input :
251900950913      * - $FX = schiera con tutti i testi disponibili
252000950913      * - $FC = schiere con i flag di validità associati ai testi
252100950913      * - $FR = schiere con i flag di REVERSE IMAGE
252200950913      * - $ULK = ultimo testo caricato
252300950913      * - $L01 = Lunghezza prima riga
252400950913      * - $L02 = Lunghezza seconda riga
252500950913      * Output :
252600950913      * - $ULK
252700950913      * - $KEY1= 1° riga di tasti funzione
252800950913      * - $KEY2= 2° riga di tasti funzione
252900950913      *
253000961003     C                   CALLb     'XKEYMSG'
253100950913      * input
253200940509     C                   PARM                    $FX
253300940509     C                   PARM                    $FC
253400940509     C                   PARM                    $FR
253500940509     C                   PARM                    $ULK              3 0
253600950913      * output
253700940509     C                   PARM                    $KEY1            79
253800940509     C                   PARM                    $KEY2            79
253900950913      * input
254000941201     C                   PARM      79            $L01              3 0
254100941201     C                   PARM      79            $L02              3 0
254200950913      *
254300060224A1148C                   PARM                    $ALLFUNCT       480
254400060224A1148C*
254500940509     C                   ENDSR
254600950906**  Costanti da file messaggi
254700950213PROMSG    *LIBL     COS0049    record non trovato
254800940128**
25490095040501PROMSG    *LIBL     COS0001    immissione
25500095040502PROMSG    *LIBL     COS0002    modifica
25510095040503PROMSG    *LIBL     COS0003    copia
25520095040504PROMSG    *LIBL     COS0004    annullamento
25530095040505PROMSG    *LIBL     COS0005    visualizzazione
255400940509** TASTI DI FUNZIONE  D1
255500940509PROMSG    *LIBL     KEY0001  01
255600940509PROMSG    *LIBL     KEY0002  02
255700940509PROMSG    *LIBL     KEY0003  03
255800940509PROMSG    *LIBL     KEY0004  04
255900940509PROMSG    *LIBL     KEY0005  05
256000940509PROMSG    *LIBL     KEY0006  06
256001160729SDGMSG    *LIBL     SDK0025  07           ## R235 ##
256200940509PROMSG    *LIBL     KEY0008  08
256300940509PROMSG    *LIBL     KEY0009  09
256400940509PROMSG    *LIBL     KEY0010  10
256500940509PROMSG    *LIBL     KEY0011  11
256600940509PROMSG    *LIBL     KEY0012  12
256700940509PROMSG    *LIBL     KEY0013  13
256800940509PROMSG    *LIBL     KEY0014  14
256900940509PROMSG    *LIBL     KEY0015  15
257000940509PROMSG    *LIBL     KEY0016  16
257100940509PROMSG    *LIBL     KEY0017  17
257200940509PROMSG    *LIBL     KEY0018  18
257300940509PROMSG    *LIBL     KEY0019  19
257400940509PROMSG    *LIBL     KEY0020  20
257500940509PROMSG    *LIBL     KEY0021  21
257600940509PROMSG    *LIBL     KEY0022  22
257700940509PROMSG    *LIBL     KEY0023  23
257800940509PROMSG    *LIBL     KEY0024  24
257900940509**  ABILITAZIONE D1
2580009407200N  01
2581009407200N  02
2582009407201N  03
2583009407201N  04
2584009407201N  05
2585009502201N  06
2586001607291N  07               ## R235 ##
2587009407201N  08
2588009407200N  09
2589009411160N  10
2590009407200N  11
2591009407201N  12
2592009407200N  13
2593009407200N  14
2594009407200N  15
2595009510061N  16
2596009407200N  17
2597009407200N  18
2598009407200N  19
2599009407200N  20
2600009407200N  21
2601009407200N  22
2602009509060N  23   Impostare a '1' se si utilizzano opzioni
2603009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
