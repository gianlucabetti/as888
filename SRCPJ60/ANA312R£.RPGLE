000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200960416     H*PARMS CVTOPT(*DATETIME)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940117      *
000500950206      *            GESTIONE ANAGRAFICA DICHIARAZIONE D'INTENTI
000600950206      *           ---------------------------------------------
000700000519      *
000800940223      *
000900940223      * DOCUMENTI MICROANALISI :
001000940223      *
001100940223      * NOTE TECNICHE :
001200941214      *
001300940223      * PERSONALIZZAZIONI
001400940223      *
001500141217R160 F* Gestione flag dichiarazione estratta
001600940223      *
001700940223      *
001800940223      *               MAPPA INDICATORI STD
001900940223      *
002000940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002100970725      *  07           PROTECT CAMPO NR. ESTERNO PER CLIENTI
002200940223      *  21           GENERICO OPERAZIONI I/O
002300940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002400940223      *  30           SFLDSP
002500940223      * N31           SFLCLR
002600940128      *  31           SFLDSPCTL
002700940128      *  32           SFLNXTCHG
002800940128      *  33           SFLEND
002900940128      *  39           OF PRTF
003000940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003100940317      *  Specificare l'uso dei singoli indicatori
003200940315      *  50 <---> 98  ERRORI SU VIDEO
003300940317      *  Specificare l'uso dei singoli indicatori
003400940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003600940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003700940128      *  99           INDIC. GENERALE DI ERRORE
003800940128     F*----------------------------------------------------*
003900980223     FANDIC01L  IF   E           K DISK
004000970725     F                                     RENAME(ANDIC000:ANDIC01)
004100951004     F*
004200980223     FANDIC02L  UF A E           K DISK
004300970725     F                                     RENAME(ANDIC000:ANDIC02)
004400141217     F*
004500141217R160 FSdgDif01l UF A E           K DISK
004600000519A0778X*
004700000519A0778X***ANTXT01L  IF   E           K DISK
004800950914     F*
004900000518B0212FANA312P   O    E             PRINTER UsrOpn
005000000518B0212F*****ANA312P   O    E             PRINTER
005100951003     F*
005200941221     FANA312D   CF   E             WORKSTN
005300941221     F                                     RENAME(A312D1:FMTD1)
005400941221     F                                     RENAME(A312T1:FMTT1)
005500941221     F                                     RENAME(A312Z1:FMTZ1)
005600940201     F                                     INFDS(DSFMT)
005700940128     D*----------------------------------------------------*
005800950427     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005900950427     D $ID             S             27    DIM(5) ALT($OZ)
006000940506     D* Schiere per la gestione dei tasti di funzione
006100940506     D $FM             S             27    DIM(24)
006200940506     D $FX             S             50    DIM(24)
006300940506     D $FC             S              1    DIM(24)
006400940506     D $FR             S              1    DIM(24)
006500940506     D* Video D1
006600940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006700940506     D $FX1            S             50    DIM(24) ALT($FM1)
006800940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006900940506     D $FR1            S              1    DIM(24) ALT($FC1)
007000950914     D*-------------
007100940127     D* Passaggio Parametri
007200940127     D KPJBA         E DS
007300940318     D*-------------
007400940318     D* DS Esterna gestione messaggi per COMIT
007500940318     D XMSGDS        E DS
007600940318     D  STS          E                     EXTFLD(MSGSTS)
007700940721     D                                     DIM(333)
007800940318     D  JBA          E                     EXTFLD(MSGJBA)
007900940721     D                                     DIM(502)
008000941209     D*-------------
008100941209     D* Dati di ambiente ottenuti da XSOC
008200941209     D SOC001        E DS                  EXTNAME(XSOC001DS)
008300980116     D    XscGest                     1    overlay(xscSkp:3)
008400941209     D*-------------
008500941209     D* DS Interna per dati di output di XSOC
008600941209     D XSOCDS          DS          1000
008700940203     D*-------------
008800940201     D* Passaggio Parametri al PGM di manutenzione
008900941221     D A312DS        E DS                  EXTNAME(ANA312DS)
009000950914     D*-------------
009100970725     D* DS per passaggio dati pgm User Exit
009200950323     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
009300950104     D*-------------
009400950104     D* Reperimento Autorizzazioni
009500950104     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009600950914     D*-------------
009700970703     D* Maschera dati autorizzazioni per clienti
009800950104     D GCLIDS        E DS                  EXTNAME(ANGCLIDS)
009900950914     D*-------------
010000970725     D* Maschera dati autorizzazioni per fornitori
010100970725     D GFORDS        E DS                  EXTNAME(ANGFORDS)
010200970725     d  ImmFilF      E                     ExtFld(ImmFil)
010300970725     d  ManFilF      E                     ExtFld(ManFil)
010400970725     d  AnnFilF      E                     ExtFld(AnnFil)
010500970725     d  VisFilF      E                     ExtFld(VisFil)
010600970703     D*-------------
010700950104     D* Passaggio Autorizzazioni
010800950104     D DSAUT           DS           512
010900950914     D* Ricerca tipo testo
011000950914     D A720DS        E DS                  EXTNAME(ANA720DS) INZ
011100950914     D  UNI720       E                     EXTFLD(UNITA720)
011200950914     D  TPT720       E                     EXTFLD(TPTXT720)
011300950914     D  LIN720       E                     EXTFLD(LINGUA720)
011400950914     D  CLA720       E                     EXTFLD(CLAUTE720)
011500950914     D*-------------
011600951002     D* Operaz. importazione
011700951002     D TABG17        E DS                  EXTNAME(ANGG17DS)
011800951002     D*-------------
011900941215     D* Richiamo a XATB
012000941215     D XATBDS        E DS                  INZ
012100950914     D*-------------
012200941215     D* Richiamo a XTAB
012300941215     D XTABDS        E DS                  INZ
012400950914     D*-------------
012500950914     D* Richiamo a DVCLI
012600950914     D DVCLI         E DS                  EXTNAME(DVCLIDS) INZ
012700941216     D*-------------
012800950914     D CLI             C                   CONST('  DVOCLIDS')
012900950914     D*-------------
013000950914     D*  Ds per output DVCLI formato ANCLN+ANSOG+ANIND
013100950914     D DVOCLI        E DS                  EXTNAME(DVOCLIDS) INZ
013200950914     D*-------------
013300950914     D* Anagrafica clienti
013400950914     D  CLNSOC       E                     EXTFLD(CLNSOCIETA)
013500950914     D  CLNUNT       E                     EXTFLD(CLNUNITA)
013600950914     D  CLNKCC       E                     EXTFLD(CLNKCC)
013700950914     D  CLNKSC       E                     EXTFLD(CLNKSC)
013800950914     D  CLNSOG       E                     EXTFLD(CLNSOGG)
013900950914     D  CLNTII       E                     EXTFLD(CLNTPIND)
014000950914     D  CLNCOI       E                     EXTFLD(CLNCDIND)
014100950914     D  CLNDES       E                     EXTFLD(CLNDESBREV)
014200950914     D  CLNLDV       E                     EXTFLD(CLNLINEAV)
014300950914     D  CLNFIL       E                     EXTFLD(CLNFILIALE)
014400950914     D  CLNDAC       E                     EXTFLD(CLNDTINRPC)
014500950914     D  CLNUTE       E                     EXTFLD(CLNUTENTE)
014600950914     D  CLNDVA       E                     EXTFLD(CLNDTULMO)
014700950914     D  CLNCCP       E                     EXTFLD(CLNCLIPREC)
014800950914     D  CLNCFP       E                     EXTFLD(CLNFORCORR)
014900950914     D  CLNCCF       E                     EXTFLD(CLNCLIFATT)
015000950914     D  CLNFAP       E                     EXTFLD(CLNADDSPBA)
015100950914     D  CLNFSB       E                     EXTFLD(CLNSPEBOLL)
015200950914     D  CLNCOF       E                     EXTFLD(CLNNRCPFAT)
015300950914     D  CLNIMB       E                     EXTFLD(CLNIMBUSTA)
015400950914     D  CLNFID       E                     EXTFLD(CLNFIDOSTA)
015500950914     D  CLNFIA       E                     EXTFLD(CLNFIDOASS)
015600950914     D  CLNDID       E                     EXTFLD(CLNDTFIST)
015700950914     D  CLNDIA       E                     EXTFLD(CLNDTFIAS)
015800950914     D  CLNCAS       E                     EXTFLD(CLNCLIASSO)
015900950914     D  CLNGDR       E                     EXTFLD(CLNGGRCVAG)
016000950914     D  CLNFBC       E                     EXTFLD(CLNCTRCONT)
016100950914     D  CLNFPC       E                     EXTFLD(CLNCITPROV)
016200950914     D  CLNASN       E                     EXTFLD(CLNSTATIST)
016300950914     D  CLNDAN       E                     EXTFLD(CLNDTFIRPC)
016400950914     D  CLNCLV       E                     EXTFLD(CLNCLASABC)
016500950914     D  CLNSOL       E                     EXTFLD(CLNTPTESTO)
016600950914     D  CLNMAG       E                     EXTFLD(CLNMAG)
016700950914     D  CLNADO       E                     EXTFLD(CLNADDIVA)
016800950914     D  CLNFAT       E                     EXTFLD(CLNFREQFAT)
016900950914     D  CLNTFA       E                     EXTFLD(CLNRAGGBOL)
017000950914     D  CLNRAF       E                     EXTFLD(CLNRAGGART)
017100950914     D  CLNMSP       E                     EXTFLD(CLNMDSPED)
017200950914     D  CLNRES       E                     EXTFLD(CLNRESA)
017300950914     D  CLNSSF       E                     EXTFLD(CLNSTASCOF)
017400950914     D  CLNFDP       E                     EXTFLD(CLNDETPREZ)
017500950914     D  CLNDIR       E                     EXTFLD(CLNDTINRPL)
017600950914     D  CLNDFR       E                     EXTFLD(CLNDTFIRPL)
017700950914     D  CLNLIS       E                     EXTFLD(CLNLISTINO)
017800950914     D  CLNVAL       E                     EXTFLD(CLNDIVISA)
017900950914     D  CLNFUC       E                     EXTFLD(CLNCAPODIS)
018000950914     D  CLNFUN       E                     EXTFLD(CLNFUNZRIO)
018100950914     D  CLNAGE       E                     EXTFLD(CLNAGENTE1)
018200950914     D  CLNAG2       E                     EXTFLD(CLNAGENTE2)
018300950914     D  CLNAG3       E                     EXTFLD(CLNAGENTE3)
018400950914     D  CLNPSP       E                     EXTFLD(CLNPRIOEVA)
018500950914     D  CLNBGC       E                     EXTFLD(CLNBKGSCLI)
018600950914     D  CLNRSC       E                     EXTFLD(CLNRAGGSCO)
018700950914     D  CLNRAP       E                     EXTFLD(CLNRAGGPRV)
018800950914     D  CLNTPP       E                     EXTFLD(CLNLORDNET)
018900950914     D  CLNCDP       E                     EXTFLD(CLNCONDPAG)
019000950914     D  CLNSCP       E                     EXTFLD(CLNSCOCASS)
019100950914     D  CLNPA1       E                     EXTFLD(CLNPERCAD1)
019200950914     D  CLNPA2       E                     EXTFLD(CLNPERCAD2)
019300950914     D  CLNRCO       E                     EXTFLD(CLNCONFORD)
019400950914     D  CLNRGU       E                     EXTFLD(CLNRAGGVEN)
019500950914     D  CLNSPP       E                     EXTFLD(CLNSTAPREZ)
019600950914     D  CLNCIV       E                     EXTFLD(CLNCDIVA)
019700950914     D  CLNTIN       E                     EXTFLD(CLNTPINDCM)
019800950914     D  CLNSC1       E                     EXTFLD(CLNSCOPAG1)
019900950914     D  CLNSC2       E                     EXTFLD(CLNSCOPAG2)
020000950914     D  CLNIP1       E                     EXTFLD(CLNGGINPA1)
020100950914     D  CLNIP2       E                     EXTFLD(CLNGGINPA2)
020200950914     D  CLNIP3       E                     EXTFLD(CLNGGINPA3)
020300950914     D  CLNTRA       E                     EXTFLD(CLNSESSTRA)
020400950914     D  CLNRIC       E                     EXTFLD(CLNSESSRIC)
020500950914     D  CLNTIV       E                     EXTFLD(CLNTPPVEND)
020600950914     D  CLNTPC       E                     EXTFLD(CLNTPCAUZ)
020700950914     D  CLNTSA       E                     EXTFLD(CLNACCONTO)
020800950914     D  CLNCES       E                     EXTFLD(CLNCLICESS)
020900950914     D  CLNFS1       E                     EXTFLD(CLNCLISOLL)
021000950914     D  CLNFS2       E                     EXTFLD(CLNADDSOLL)
021100950914     D  CLNFS5       E                     EXTFLD(CLNCONTRAS)
021200950914     D  CLNFS6       E                     EXTFLD(CLNADDPROD)
021300950914     D  CLNRGC       E                     EXTFLD(CLNRAGGCON)
021400950914     D  CLNFS3       E                     EXTFLD(CLNFLG1)
021500950914     D  CLNFS4       E                     EXTFLD(CLNFLG2)
021600950914     D  CLNFL3       E                     EXTFLD(CLNFLG3)
021700950914     D  CLNFL4       E                     EXTFLD(CLNFLG4)
021800950914     D  CLNFL5       E                     EXTFLD(CLNFLG5)
021900950914     D  CLNFL6       E                     EXTFLD(CLNFLG6)
022000941216     D* Anagrafica soggetti
022100941216     D  SOGCOD       E                     EXTFLD(SOGSOGG)
022200941216     D  SOGDAT       E                     EXTFLD(SOGDTENTR)
022300941216     D  SOGDAU       E                     EXTFLD(SOGDTULMO)
022400941216     D  SOGDAI       E                     EXTFLD(SOGDTINVL)
022500941216     D  SOGDAF       E                     EXTFLD(SOGDTFIVL)
022600941216     D  SOGDAN       E                     EXTFLD(SOGDTNASC)
022700941216     D  SOGDES       E                     EXTFLD(SOGDES)
022800941216     D  SOGDE1       E                     EXTFLD(SOGDESAGG)
022900941216     D  SOGCDF       E                     EXTFLD(SOGCDFISC)
023000941216     D  SOGIVA       E                     EXTFLD(SOGPARTIVA)
023100941216     D  SOGFPI       E                     EXTFLD(SOGFRCPIVA)
023200941216     D  SOGCIA       E                     EXTFLD(SOGISCCIAA)
023300941216     D  SOGSES       E                     EXTFLD(SOGSEX)
023400941216     D  SOGNAT       E                     EXTFLD(SOGNATGIUR)
023500941216     D  SOGCOP       E                     EXTFLD(SOGCODPREC)
023600941216     D  SOGABI       E                     EXTFLD(SOGABI)
023700941216     D  SOGCLG       E                     EXTFLD(SOGCLFSOG)
023800950914     D* Anagrafica indirizzi
023900950914     D  INDCOD       E                     EXTFLD(INDSOGG)
024000950914     D  INDTII       E                     EXTFLD(INDTPIND)
024100950914     D  INDCOI       E                     EXTFLD(INDCDIND)
024200950914     D  INDDAU       E                     EXTFLD(INDDTULMO)
024300950914     D  INDDAI       E                     EXTFLD(INDDTINVL)
024400950914     D  INDDAF       E                     EXTFLD(INDDTFIVL)
024500950914     D  INDDES       E                     EXTFLD(INDDESFIL)
024600950914     D  INDDE1       E                     EXTFLD(INDDESAGG)
024700950914     D  INDVIA       E                     EXTFLD(INDINDRIZ)
024800950914     D  INDCIT       E                     EXTFLD(INDLOCALIT)
024900950914     D  INDCAP       E                     EXTFLD(INDCAP)
025000950914     D  INDPRV       E                     EXTFLD(INDPROV)
025100950914     D  INDSTA       E                     EXTFLD(INDSTATO)
025200950914     D  INDCAB       E                     EXTFLD(INDCAB)
025300950914     D  INDLIN       E                     EXTFLD(INDLINGUA)
025400950914     D  INDTLX       E                     EXTFLD(INDTELEX)
025500950914     D  INDTEL       E                     EXTFLD(INDTELEFON)
025600950914     D  INDTLF       E                     EXTFLD(INDTELEFAX)
025700950206     D*-------------
025800950914     D* Richiamo a DVFOR
025900950914     D DVFOR         E DS                  EXTNAME(DVFORDS) INZ
026000950914     D*-------------
026100950914     D FRN             C                   CONST('  DVOFRNDS')
026200950914     D*-------------
026300950914     D*  Ds per output DVFRN formato ANFRN+ANSOG+ANIND
026400001010A0840X*DVOFRN        E DS                  EXTNAME(DVOFRNDS) INZ
026500001010A0840D DVOFRN        E DS          1000    EXTNAME(DVOFRNDS) INZ
026600950914     D*-------------
026700950914     D* Anagrafica fornitori
026800950914     D  FRNSOC       E                     EXTFLD(FRNSOCIETA)
026900950914     D  FRNUNT       E                     EXTFLD(FRNUNITA)
027000950914     D  FRNKCC       E                     EXTFLD(FRNKCC)
027100950914     D  FRNKSC       E                     EXTFLD(FRNKSC)
027200950914     D  FRNFIL       E                     EXTFLD(FRNFILIALE)
027300950914     D  FRNSOG       E                     EXTFLD(FRNSOGG)
027400950914     D  FRNTII       E                     EXTFLD(FRNTPIND)
027500950914     D  FRNCOI       E                     EXTFLD(FRNCDIND)
027600950914     D  FRNDUM       E                     EXTFLD(FRNDTULMO)
027700950914     D  FRNUTE       E                     EXTFLD(FRNUTENTE)
027800950914     D  FRNDIR       E                     EXTFLD(FRNDTINRPF)
027900950914     D  FRNDFR       E                     EXTFLD(FRNDTFIRPF)
028000950914     D  FRNDES       E                     EXTFLD(FRNDESBREV)
028100950914     D  FRNGPM       E                     EXTFLD(FRNGRUMERC)
028200950914     D  FRNSGM       E                     EXTFLD(FRNSETMERC)
028300950914     D  FRNCFA       E                     EXTFLD(FRNFORALT)
028400950914     D  FRNLIS       E                     EXTFLD(FRNLISTINO)
028500950914     D  FRNVAL       E                     EXTFLD(FRNDIVISA)
028600950914     D  FRNCDD       E                     EXTFLD(FRNCONDCON)
028700950914     D  FRNQFO       E                     EXTFLD(FRNQUALFOR)
028800950914     D  FRNVAF       E                     EXTFLD(FRNVOLAFF)
028900950914     D  FRNCIV       E                     EXTFLD(FRNCDIVA)
029000950914     D  FRNCOB       E                     EXTFLD(FRNCOLLOBB)
029100950914     D  FRNTCL       E                     EXTFLD(FRNTPCOLL)
029200950914     D  FRNFRL       E                     EXTFLD(FRNFORLIST)
029300950914     D  FRNFOF       E                     EXTFLD(FRNFORFATT)
029400950914     D  FRNPOT       E                     EXTFLD(FRNFORPOT)
029500950914     D  FRNTRP       E                     EXTFLD(FRNREPPRZL)
029600950914     D  FRNFFB       E                     EXTFLD(FRNFORBLO)
029700950914     D  FRNLSF       E                     EXTFLD(FRNLISFIL)
029800950914     D  FRNCPA       E                     EXTFLD(FRNABILPRI)
029900950914     D  FRNMDC       E                     EXTFLD(FRNMDCONS)
030000950914     D  FRNMAG       E                     EXTFLD(FRNMAG)
030100950914     D  FRNCLI       E                     EXTFLD(FRNCLICORR)
030200950914     D  FRNFRP       E                     EXTFLD(FRNFORPREC)
030300950914     D  FRNSC1       E                     EXTFLD(FRNSCOPAG)
030400950914     D  FRNSC2       E                     EXTFLD(FRNSCOPAG2)
030500950914     D  FRNIP1       E                     EXTFLD(FRNGGINPA1)
030600950914     D  FRNIP2       E                     EXTFLD(FRNGGINPA2)
030700950914     D  FRNIP3       E                     EXTFLD(FRNGGINPA3)
030800950914     D  FRNTRA       E                     EXTFLD(FRNSESSTRA)
030900950914     D  FRNRIC       E                     EXTFLD(FRNSESSRIC)
031000950914     D  FRNCDP       E                     EXTFLD(FRNCONDPAG)
031100950914     D* Anagrafica soggetti
031200950914     D  FOGCOD       E                     EXTFLD(SOGSOGG)
031300950914     D  FOGDAT       E                     EXTFLD(SOGDTENTR)
031400950914     D  FOGDAU       E                     EXTFLD(SOGDTULMO)
031500950914     D  FOGDAI       E                     EXTFLD(SOGDTINVL)
031600950914     D  FOGDAF       E                     EXTFLD(SOGDTFIVL)
031700950914     D  FOGDAN       E                     EXTFLD(SOGDTNASC)
031800950914     D  FOGDES       E                     EXTFLD(SOGDES)
031900950914     D  FOGDE1       E                     EXTFLD(SOGDESAGG)
032000950914     D  FOGCDF       E                     EXTFLD(SOGCDFISC)
032100950914     D  FOGIVA       E                     EXTFLD(SOGPARTIVA)
032200950914     D  FOGFPI       E                     EXTFLD(SOGFRCPIVA)
032300950914     D  FOGCIA       E                     EXTFLD(SOGISCCIAA)
032400950914     D  FOGSES       E                     EXTFLD(SOGSEX)
032500950914     D  FOGNAT       E                     EXTFLD(SOGNATGIUR)
032600950914     D  FOGCOP       E                     EXTFLD(SOGCODPREC)
032700950914     D  FOGABI       E                     EXTFLD(SOGABI)
032800950914     D  FOGCLG       E                     EXTFLD(SOGCLFSOG)
032900950914     D* Anagrafica indirizzi
033000950914     D  FNDCOD       E                     EXTFLD(INDSOGG)
033100950914     D  FNDTII       E                     EXTFLD(INDTPIND)
033200950914     D  FNDCOI       E                     EXTFLD(INDCDIND)
033300950914     D  FNDDAU       E                     EXTFLD(INDDTULMO)
033400950914     D  FNDDAI       E                     EXTFLD(INDDTINVL)
033500950914     D  FNDDAF       E                     EXTFLD(INDDTFIVL)
033600950914     D  FNDDES       E                     EXTFLD(INDDESFIL)
033700950914     D  FNDDE1       E                     EXTFLD(INDDESAGG)
033800950914     D  FNDVIA       E                     EXTFLD(INDINDRIZ)
033900950914     D  FNDCIT       E                     EXTFLD(INDLOCALIT)
034000950914     D  FNDCAP       E                     EXTFLD(INDCAP)
034100950914     D  FNDPRV       E                     EXTFLD(INDPROV)
034200950914     D  FNDSTA       E                     EXTFLD(INDSTATO)
034300950914     D  FNDCAB       E                     EXTFLD(INDCAB)
034400950914     D  FNDLIN       E                     EXTFLD(INDLINGUA)
034500950914     D  FNDTLX       E                     EXTFLD(INDTELEX)
034600950914     D  FNDTEL       E                     EXTFLD(INDTELEFON)
034700950914     D  FNDTLF       E                     EXTFLD(INDTELEFAX)
034800950914     D*-------------
034900950206     D* DS esterna codice IVA
035000950206     D TABG48        E DS                  EXTNAME(ANGG48DS) INZ
035100950206     D*-------------
035200950206     D* Parametri
035300950206     D X06DS         E DS                  EXTNAME(X06G48DS)
035400950206     D  X06SOC       E                     EXTFLD(X06SOCIETA)
035500950206     D  X06CIV       E                     EXTFLD(X06CDIVA)
035600950206     D  X06DAT       E                     EXTFLD(X06DATA)
035700950206     D  X06LIN       E                     EXTFLD(X06LINGUA)
035800940201     D*-------------
035900940201     D DSFMT           DS
036000940506     D  $TASTO               369    369
036100940201     D  NRG                  370    370
036200940201     D  NCL                  371    371
036300940201     D*-------------
036400940201     D* posizione cursore
036500940201     D CURSOR          DS
036600940223     D  RIRI                   1      2B 0 INZ
036700940201     D  R$$                    2      2
036800940223     D  COCO                   3      4B 0 INZ
036900940201     D  C$$                    4      4
037000940207     D*-------------
037100940207     D* Reperimento nome PGM
037200940207     D STATUS         SDS           333
037300940207     D  DSPGM            *PROC
037400950104     D  PARMS            *PARMS
037500940225     D*-------------
037600940616     D* Costanti
037700941117     D* data a 0
037800941117     D $DT0            C                   CONST('0001-01-01')
037900940225     D*-------------
038000940506     D* Tasti di funzione
038100940506     D F01             C                   CONST(X'31')
038200940506     D F02             C                   CONST(X'32')
038300940506     D F03             C                   CONST(X'33')
038400940506     D F04             C                   CONST(X'34')
038500940506     D F05             C                   CONST(X'35')
038600940506     D F06             C                   CONST(X'36')
038700940506     D F07             C                   CONST(X'37')
038800940506     D F08             C                   CONST(X'38')
038900940506     D F09             C                   CONST(X'39')
039000940506     D F10             C                   CONST(X'3A')
039100940506     D F11             C                   CONST(X'3B')
039200940506     D F12             C                   CONST(X'3C')
039300940506     D F13             C                   CONST(X'B1')
039400940506     D F14             C                   CONST(X'B2')
039500940506     D F15             C                   CONST(X'B3')
039600940506     D F16             C                   CONST(X'B4')
039700940506     D F17             C                   CONST(X'B5')
039800940506     D F18             C                   CONST(X'B6')
039900940506     D F19             C                   CONST(X'B7')
040000940506     D F20             C                   CONST(X'B8')
040100940506     D F21             C                   CONST(X'B9')
040200940506     D F22             C                   CONST(X'BA')
040300940506     D F23             C                   CONST(X'BB')
040400940506     D F24             C                   CONST(X'BC')
040500940506     D ENTER           C                   CONST(X'F1')
040600940506     D ROLDWN          C                   CONST(X'F4')
040700940506     D ROLLUP          C                   CONST(X'F5')
040800950427     D*-------------
040900950427     D SOTTIT          C                   CONST('PROMSG    *LIBL-
041000950915     D                                          COSXXXX')
041100000114A0735D ZDateISO        S               D
041200041202B0762D DataIso         S               D
041300000519A0778D*-------------
041400000519  "  D* Variabili per chiamata a X72Txt
041500000519  "  D XEsito          S              1A
041600000519  "  D XOpe            S              1A
041700000519  "  D XSocieta        S              3A
041800000519  "  D XTpTesto        S              2A
041900000519  "  D XUnita          S              8A
042000000519  "  D XLingua         S              1A
042100000523  "  D XClaUte         S              2A
042200000519  "  D XPosRig         S              1A
042300000519  "  D XNrRiga         S              9P 0
042400000519  "  D XRibat          S              1A
042500000519  "  D XTesRig         S             70A
042600000519  "  D XNrChiavi       S              1  0
042700000519A0778D XUsoUnita       S              1A
042800081224D2461D*-------------
042900081224  "  D* DS esterna per dati fornitori
043000081224  "  D DVAFOR        E DS
043100081224  "  D* DS esterna per dati indirizzo
043200081224  "  D ANIND         E DS                  EXTNAME(ANIND00F) PREFIX($:1)
043300081224  "  D*-------------
043400081224  "  D* Definizioni parametri driver soggetti
043500081224  "  DdvaRic           S              1
043600081224  "  DdvaSocieta       S              3
043700081224  "  DdvaUnita         S              8
043800081224  "  DdvaDtRif         S               D
043900081224  "  DdvaStrutt        S             10
044000081224  "  DdvaLenOut        S              9B 0
044100081224  "  DdvaSnatura       S              1
044200081224  "  DdvaCodice        S              8
044300081224  "  DdvaLineav        S              3
044400081224  "  DdvaFiliale       S              3  0
044500081224  "  DdvaSogg          S              8
044600081224  "  DdvaTpInd         S              2
044700081224  "  DdvaCdInd         S              3
044800081224  "  DdvaOutput        S           4000
044900081224  "  DdvaErrore        S              1
045000081224  "  D WCap            S                   like(XScCap)
045100081224  "  D WLocali         S                   like(XScCit)
045200081224  "  D WProv           S                   like(XScPrv)
045300081224D2461D WCognNome       S                   like(RagSoc)
045400940128     I*----------------------------------------------------*
045500941221     I* Dichiarazione di intenti
045600970725     IANDIC01
045700970725     I              DICSOCIETA                  DICSOC
045800970725     I              DICUNITA                    DICUNI
045900970725     I              DICKSC                      DICKSC
046000970725     I              DICNRCLI                    DICNRC
046100970725     I              DICDTCLI                    DICDTC
046200970725     I              DICNRINTER                  DICNIN
046300970725     I              DICDTINTER                  DICDIN
046400970725     I              DICDTINDIC                  DICDTI
046500970725     I              DICDTFIDIC                  DICDTF
046600970725     I              DICIMPORTO                  DICIMP
046700970725     I              DICCDIVA                    DICIVA
046800970725     I              DICANNULLA                  DICANN
046900970725     I              DICTPTESTO                  DICTPT
047000970725     I              DICOPERA                    DICOPE
047100970725     I              DICANNO                     DICAAA
047200970725     I              DICDIVISA                   DICDIV
047300970725     I              DICDTREVOC                  DICDTR
047400970725     IANDIC02
047500951004     I              DICSOCIETA                  DICSOC
047600951004     I              DICUNITA                    DICUNI
047700951004     I              DICKSC                      DICKSC
047800951004     I              DICNRCLI                    DICNRC
047900951004     I              DICDTCLI                    DICDTC
048000951004     I              DICNRINTER                  DICNIN
048100951004     I              DICDTINTER                  DICDIN
048200951004     I              DICDTINDIC                  DICDTI
048300951004     I              DICDTFIDIC                  DICDTF
048400951004     I              DICIMPORTO                  DICIMP
048500951004     I              DICCDIVA                    DICIVA
048600951004     I              DICANNULLA                  DICANN
048700951004     I              DICTPTESTO                  DICTPT
048800951004     I              DICOPERA                    DICOPE
048900951004     I              DICANNO                     DICAAA
049000951004     I              DICDIVISA                   DICDIV
049100951004     I              DICDTREVOC                  DICDTR
049200950914     I*-------------
049300000519A0778X* Testi lettere
049400000519  "  X***ANTXT000
049500000519  "  X***           TXTSOCIETA                  TXTSOC
049600000519  "  X***           TXTUNITA                    TXTUNI
049700000519  "  X***           TXTTPTESTO                  TXTTPT
049800000519  "  X***           TXTLINGUA                   TXTLIN
049900000519  "  X***           TXTCLAUTE                   TXTCLA
050000000519  "  X***           TXTPOSRIG                   TXTPOS
050100000519  "  X***           TXTNRRIGA                   TXTNRR
050200000519  "  X***           TXTRIBAT                    TXTRIB
050300000519A0778X***           TXTTESRIG                   TXTTES
050400941209     I*-------------
050500940127     C*----------------------------------------------------*
050600940127     C*                MAIN LINE PROGRAM
050700940127     C*----------------------------------------------------*
050800940127     C*
050900940223     C* inizializzazione variabili
051000940223     C                   EXSR      INZVAR
051100940223     C*
051200940223     C     $FINE         DOWEQ     *OFF
051300940202     C     $GEST         CASEQ     'D1'          GESD1
051400941027     C                   ENDCS
051500941027     C                   ENDDO
051600940201     C*
051700940325     C* fine programma
051800940325     C                   EXSR      ENDPGM
051900940325     C/EJECT
052000940325     C************************************************************
052100950914     C* ENDPGM
052200940325     C************************************************************
052300940325     C     ENDPGM        BEGSR
052400941214     C*
052500941214     C* se ho confermato l'uscita proseguo
052600941214     C     $FINE         IFEQ      *ON
052700980223     C*
052800980223     C*** Faccio sempre ROLBK per il numeratore
052900980223     C**(se ho confermato l'inserimento precedente ho già fatto COMMIT)
053000980223     C                   ROLBK
053100940325     C*
053200940325     C* disallocazione rcd nel caso di modifica/annullamento
053300941221     C     OPZ312        IFEQ      '02'
053400941221     C     OPZ312        OREQ      '04'
053500970725     C                   UNLOCK    ANDIC02L                             22
053600141217R160 C                   UNLOCK    SdgDif01l                            22
053700941027     C                   ENDIF
053800940325     C*
053900940325     C* passaggio dati al pgm chiamante
054000941221     C                   MOVEL     A312DS        KPJBU
054100940325     C*
054200940325     C                   SETON                                            RT
054300940616     C                   RETURN
054400941214     C*
054500941214     C                   ENDIF
054600940325     C*
054700940325     C                   ENDSR
054800940325     C/EJECT
054900940201     C************************************************************
055000940201     C* RICERCHE
055100940201     C************************************************************
055200940127     C     SEARCH        BEGSR
055300940127     C*
055400940127     C* determino Riga/Colonna del cursore
055500940127     C                   MOVE      NRG           R$$
055600940127     C                   MOVE      NCL           C$$
055700940127     C                   Z-ADD     RIRI          CURR
055800940127     C                   Z-ADD     COCO          CURC
055900940127     C*
056000940224     C* I campi che al momento dell'F4 risultano protetti non devono
056100940224     C* essere abilitati alla funzione di ricerca
056200941027     C*
056300941027     C                   SELECT
056400941215     C*
056500941221     C* FMTD1  - IVA
056600941221     C     H1NMFL        WHENEQ    'D1IVA'
056700941215     C                   RESET                   XTABDS
056800950117     C                   MOVE      XSCSOC        XTAAZI
056900941215     C                   MOVE      XSCLIN        XTALIN
057000941221     C                   MOVE      *BLANK        WWIVA
057100941221     C                   MOVE      WWIVA         XTAKEY
057200941215     C                   MOVE      '01'          XTAOPZ
057300941215     C                   Z-ADD     CURR          XTARIG
057400941215     C                   Z-ADD     CURC          XTACOL
057500941215     C                   CLEAR                   KPJBU
057600941215     C                   MOVEL     XTABDS        KPJBU
057700020418B0435X****               IF        XscGest <> '2'                               Tile non installato
057800941221     C                   CALL      'ANGG48R'
057900941215     C                   PARM                    KPJBA
058000020418B0435X****               ELSE                                                   Tile installato
058100001027  "  X****               CALL      'SC$G48R'
058200001027  "  X****               PARM                    KPJBA
058300020418B0435X****               ENDIF
058400941215     C                   MOVEL     KPJBU         XTABDS
058500941215     C     XTAERR        IFNE      '1'
058600941221     C                   MOVE      XTAKEY        WWIVA
058700941215     C                   ELSE
058800941221     C                   MOVE      *BLANK        WWIVA
058900941215     C                   ENDIF
059000941221     C     WWIVA         IFNE      *BLANK
059100941221     C                   MOVEL     WWIVA         D1IVA
059200941215     C                   ENDIF
059300941215     C* imposto pos. del cursore
059400941215     C                   Z-ADD     CURR          H1RIGA
059500941215     C                   Z-ADD     CURC          H1COLO
059600950914     C*
059700950914     C* FMTD1  - TIPO TESTO
059800950914     C     H1NMFL        WHENEQ    'D1TPT'
059900951002     C     COF312        ANDEQ     'F'
060000950914     C                   RESET                   A720DS
060100950914     C                   MOVEL     '01'          OPZ720
060200000523A0778X***                MOVEL     XSCDUN        UNI720
060300950914     C                   MOVEL     XSCLIN        LIN720
060400950914     C                   MOVEL     A720DS        KPJBU
060500950914     C                   CALL      'ANA720R'
060600950914     C                   PARM                    KPJBA
060700950914     C                   MOVEL     KPJBU         A720DS
060800950914     C     TPT720        IFNE      *BLANK
060900950914     C                   MOVEL     TPT720        D1TPT
061000950914     C                   ENDIF
061100950914     C* imposto pos. del cursore
061200950914     C                   Z-ADD     CURR          H1RIGA
061300950914     C                   Z-ADD     CURC          H1COLO
061400951002     C*
061500951002     C* FMTD1  - OPERAZ. IMPORTAZIONE
061600951002     C     H1NMFL        WHENEQ    'D1OPI'
061700951002     C                   RESET                   XTABDS
061800951002     C                   MOVE      XSCSOC        XTAAZI
061900951002     C                   MOVE      XSCLIN        XTALIN
062000951002     C                   MOVE      *BLANK        WWOPI
062100951002     C                   MOVE      WWOPI         XTAKEY
062200951002     C                   MOVE      '01'          XTAOPZ
062300951002     C                   Z-ADD     CURR          XTARIG
062400951002     C                   Z-ADD     CURC          XTACOL
062500951002     C                   CLEAR                   KPJBU
062600951002     C                   MOVEL     XTABDS        KPJBU
062700951002     C                   CALL      'ANGG17R'
062800951002     C                   PARM                    KPJBA
062900951002     C                   MOVEL     KPJBU         XTABDS
063000951002     C     XTAERR        IFNE      '1'
063100951002     C                   MOVE      XTAKEY        WWOPI
063200951002     C                   ELSE
063300951002     C                   MOVE      *BLANK        WWOPI
063400951002     C                   ENDIF
063500951002     C     WWOPI         IFNE      *BLANK
063600951002     C                   MOVEL     WWOPI         D1OPI
063700951002     C                   ENDIF
063800951002     C* imposto pos. del cursore
063900951002     C                   Z-ADD     CURR          H1RIGA
064000951002     C                   Z-ADD     CURC          H1COLO
064100940223     C*
064200940223     C* ricerca non abilitata in questa posizione
064300941027     C                   OTHER
064400940926     C* imposto pos. del cursore
064500940926     C                   Z-ADD     CURR          H1RIGA
064600940926     C                   Z-ADD     CURC          H1COLO
064700940926     C                   SETON                                        9899
064800940127     C*
064900941027     C                   ENDSL
065000940127     C*
065100940506     C                   MOVE      *HIVAL        $LASTV
065200940127     C                   ENDSR
065300940225     C/EJECT
065400940225     C************************************************************
065500940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
065600940225     C************************************************************
065700940225     C     GESPIE        BEGSR
065800940225     C*
065900940225     C                   SELECT
066000940225     C*
066100940506     C     $GEST         WHENEQ    'D1'
066200940506     C     $ULKD1        IFEQ      0
066300940506     C* riempimento opzioni
066400940506     C                   EXSR      F24D1
066500940506     C                   ELSE
066600940506     C                   WRITE     FMTZ1
066700940506     C                   ENDIF
066800940225     C*
066900940225     C                   ENDSL
067000940225     C*
067100940225     C                   ENDSR
067200940131     C/EJECT
067300940131     C************************************************************
067400940510     C* GESTIONE VIDEO RECORD D1
067500940131     C************************************************************
067600940127     C     GESD1         BEGSR
067700940225     C* inizializzazione videata
067800941117     C     $INZD1        IFEQ      *ON
067900940127     C                   EXSR      INZD1
068000940223     C                   MOVE      *OFF          $INZD1
068100941117     C                   ENDIF
068200940225     C* emissione piede videata se proveniente da altra
068300940225     C* salvataggio valore formato di provenienza
068400941117     C     $LASTV        IFNE      'D1'
068500940207     C                   WRITE     FMTT1
068600940225     C                   EXSR      GESPIE
068700940202     C                   MOVE      $LASTV        $LASTG
068800940202     C                   MOVE      'D1'          $LASTV
068900941117     C                   ENDIF
069000940707     C*
069100940707     C* se esistono errori sulla videata
069200940707     C* emetto la write del formato a indicatori spenti per vedere
069300940707     C* le eventuali decodifiche
069400941117     C     *IN99         IFEQ      *ON
069500940707     C     $GEST         ANDEQ     'D1'
069600940707     C                   MOVEA     *IN           WIN
069700940707     C                   MOVE      *ALL'0'       IN5098           49
069800940707     C                   MOVEA     IN5098        *IN(50)
069900940707     C                   WRITE     FMTD1
070000940707     C                   MOVEA     WIN           *IN
070100941117     C                   ENDIF
070200940131     C*
070300941117     C                   SELECT
070400940203     C* Annullamento
070500941221     C     OPZ312        WHENEQ    '04'
070600940224     C* Visualizzazione
070700941221     C     OPZ312        OREQ      '05'
070800940203     C                   WRITE     FMTD1
070900940203     C                   EXFMT     PROTECT
071000940224     C* Immissione/Modifica/Copia
071100941117     C                   OTHER
071200940203     C                   EXFMT     FMTD1
071300941117     C                   ENDSL
071400940506     C* controllo abilitazione tasti funzione
071500940506     C*
071600940506     C                   EXSR      CTRKD1
071700940506     C*
071800941117     C     *IN99         IFEQ      *OFF
071900940131     C*
072000941117     C                   SELECT
072100940609     C* ROLLUP
072200941215     C*          $TASTO    WHEQ ROLLUP
072300941215     C*                    MOVE 'D2'      $GEST
072400940916     C* F2=Decodifica
072500940916     C     $TASTO        WHENEQ    F02
072600940916     C                   EXSR      F02D1
072700940131     C* F3=Fine
072800940506     C     $TASTO        WHENEQ    F03
072900940309     C                   EXSR      F03D1
073000940131     C* F4=Ricerca
073100940506     C     $TASTO        WHENEQ    F04
073200940127     C                   EXSR      SEARCH
073300940224     C* F5=Refresh
073400940506     C     $TASTO        WHENEQ    F05
073500940309     C                   EXSR      F05D1
073600940223     C* F8=Successivo
073700940506     C     $TASTO        WHENEQ    F08
073800940309     C                   EXSR      F08D1
073900940131     C* F12=Ritorno
074000940506     C     $TASTO        WHENEQ    F12
074100940309     C                   EXSR      F12D1
074200050801B0822C* F20=Annullamento revoca
074300050801  "  C     $TASTO        WHENEQ    F20
074400050801B0822C                   EXSR      F20D1
074500940315     C* F24=Altri tasti
074600940506     C     $TASTO        WHENEQ    F24
074700940315     C                   EXSR      F24D1
074800940131     C*
074900941117     C                   OTHER
075000940127     C                   EXSR      CTRD1
075100940203     C*
075200941117     C     $TASTO        IFEQ      F06
075300940207     C     *IN99         ANDEQ     *OFF
075400940224     C* eseguo aggiornamento
075500941214     C                   EXSR      AGGANA
075600940224     C* eseguo operazioni del dopo-aggiornamento
075700941117     C     *IN99         IFEQ      *OFF
075800940318     C                   EXSR      GESAGG
075900941117     C                   ENDIF
076000941117     C                   ENDIF
076100940203     C*
076200941117     C                   ENDSL
076300940224     C*
076400941117     C                   ENDIF
076500941027     C*
076600940117     C                   ENDSR
076700940506     C/EJECT
076800940506     C************************************************************
076900940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
077000940506     C************************************************************
077100940506     C     CTRKD1        BEGSR
077200941205     C*
077300940506     C                   SETOFF                                       99
077400940506     C                   MOVEA     $FC1          $FC
077500940506     C                   EXSR      CTRKEY
077600940506     C     *IN99         IFEQ      *ON
077700940506     C                   SETON                                        97
077800940506     C                   ENDIF
077900951012     C*
078000951012     C                   ENDSR
078100951012     C/EJECT
078200941209     C************************************************************
078300941209     C* INIZIALIZZAZIONE VIDEATA DATI
078400941209     C************************************************************
078500940127     C     INZD1         BEGSR
078600940131     C*
078700970210     C                   Z-ADD     0             DtcSAV
078800970210     C                   Z-ADD     0             NrcSAV
078900951004     C*
079000940131     C                   CLEAR                   FMTD1
079100950914     C*
079200950914     C                   MOVE      XSCON         D1ON1
079300950914     C                   MOVE      XSCOFF        D1OF1
079400950914     C                   MOVE      XSCOFF        D1OPE
079500950914     C*
079600950915     C     COF312        IFEQ      'F'
079700950915     C                   SETON                                        11
079800950915     C                   MOVE      XSCON         D1ON2
079900950915     C                   MOVE      XSCOFF        D1OF2
080000950915     C                   MOVE      XSCOFF        D1REV
080100141217R160 C                   MOVE      XSCON         D1ON3
080200141217 "   C                   MOVE      XSCOFF        D1OF3
080300150112R160 C                   MOVE      XSCOFF        D1Inviato
080400950915     C                   ENDIF
080500950915     C*
080600950914     C* Reperisco il soggetto passato dal chiamante
080700941222     C                   MOVEL     KSC312        D1KSC
080800941222     C*
080900941222     C                   MOVEL     *BLANK        D1KSCD
081000941222     C                   MOVEL     *BLANK        D1IND
081100941222     C                   MOVEL     *BLANK        D1CAP
081200941222     C                   MOVEL     *BLANK        D1LOC
081300941222     C                   MOVEL     *BLANK        D1PRV
081400941222     C                   MOVEL     *BLANK        D1IVAD
081500950914     C*
081600950914     C     COF312        IFEQ      'C'
081700950914     C*
081800950914     C* Decodifica cliente
081900941222     C     D1KSC         IFNE      *BLANK
082000941222     C                   RESET                   DVCLI
082100950110     C                   MOVEL     WWWSOC        DCLSOC
082200941222     C                   MOVEL     D1KSC         DCLCLI
082300941222     C                   MOVEL     *BLANK        DCLLDV
082400941222     C                   MOVEL     *ZERO         DCLFIL
082500941222     C                   Z-ADD     1             DCLNRK
082600941222     C                   MOVEL     '1'           DCLRIC
082700941222     C                   MOVEL     CLI           DCLSET
082800941222     C                   MOVEL     '1'           DCLVIS
082900941222     C                   CALL      'DVCLI'
083000941222     C                   PARM                    DVCLI
083100941222     C                   PARM                    DVOCLI
083200941222     C     DCLERR        IFEQ      '0'
083300950526     C                   MOVEL     INDDES        D1KSCD
083400941222     C                   MOVEL     INDVIA        D1IND
083500941222     C                   MOVEL     INDCAP        D1CAP
083600941222     C                   MOVEL     INDCIT        D1LOC
083700941222     C                   MOVEL     INDPRV        D1PRV
083800941222     C                   ENDIF
083900941222     C                   ENDIF
084000941222     C*
084100950914     C                   ELSE
084200950914     C*
084300950914     C     COF312        IFEQ      'F'
084400950914     C*
084500950914     C* Decodifca fornitore
084600950914     C     D1KSC         IFNE      *BLANK
084700950914     C                   RESET                   DVFOR
084800950914     C                   MOVEL     WWWSOC        DFRSOC
084900950914     C                   MOVEL     D1KSC         DFRFOR
085000951011     C                   MOVEL     *ZERO         DFRFIL
085100951011     C                   Z-ADD     1             DFRNRK
085200951011     C                   MOVEL     '1'           DFRRIC
085300951011     C                   MOVEL     FRN           DFRSET
085400951011     C                   MOVEL     '1'           DFRVIS
085500950914     C                   CALL      'DVFOR'
085600950914     C                   PARM                    DVFOR
085700950914     C                   PARM                    DVOFRN
085800950914     C     DFRERR        IFEQ      '0'
085900950914     C                   MOVEL     FNDDES        D1KSCD
086000950914     C                   MOVEL     FNDVIA        D1IND
086100950914     C                   MOVEL     FNDCAP        D1CAP
086200950914     C                   MOVEL     FNDCIT        D1LOC
086300950914     C                   MOVEL     FNDPRV        D1PRV
086400950914     C                   ENDIF
086500950914     C                   ENDIF
086600950914     C*
086700950914     C                   ENDIF
086800950914     C*
086900950914     C                   ENDIF
087000950914     C*
087100940225     C                   Z-ADD     1             $PIED1
087200940207     C*
087300940224     C* valorizzo la chiave univoca per reperire i dati
087400940224     C* se non immissione
087500941221     C     OPZ312        IFNE      '01'
087600940315     C*-------------------------------------------------------------
087700940315     C* impostare i campi chiave ricevuti
087800940315     C*-------------------------------------------------------------
087900950110     C                   MOVEL     WWWSOC        DICSOC
088000951002     C                   MOVEL     COF312        DICCF
088100970725     C                   MOVEL     NRI312        DICNIN
088200970725     C                   MOVEL     DTI312        DICDIN
088300940318     C*
088400141217R160 C                   MOVEL     WWWSOC        DifSocieta
088500141217 "   C                   MOVEL     COF312        DifCf
088600141217 "   C                   MOVEL     NRI312        DifNrInter
088700141217 "   C                   MOVEL     DTI312        DifDtInter
088800141217R160 C*
088900941201     C     *IN22         DOUEQ     *OFF
089000940318     C     MSGRTN        ORNE      'R'
089100940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
089200941221     C     OPZ312        IFEQ      '02'
089300941221     C     OPZ312        OREQ      '04'
089400970725     C     K4DIC2        CHAIN     ANDIC02                            2122
089500141217R160 C     K04Dif01      CHAIN     SdgDif01l                          2324
089600940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
089700941201     C                   ELSE
089800970725     C     K4DIC2        CHAIN(N)  ANDIC02                            2122
089900141217R160 C     K04Dif01      CHAIN(N)  SdgDif01l                          2324
090000941201     C                   ENDIF
090100940318     C*
090200940318     C* Gestione RCD allocato
090300941201     C     *IN22         IFEQ      *ON
090400941221     C                   MOVEL     'ANDIC00F'    MSGFIL
090500940318     C                   EXSR      MSGPGM
090600941201     C                   ENDIF
090700941201     C                   ENDDO
090800940224     C*
090900940224     C* se il rcd da gestire non viene trovato
091000940318     C* oppure è allocato da altro pgm
091100940224     C* ritorno al pgm guida avvertendolo dell'errore
091200941201     C     *IN21         IFEQ      *ON
091300940318     C     *IN22         OREQ      *ON
091400940224     C                   MOVE      *ON           $FINE
091500941221     C                   MOVE      '1'           ERR312
091600940224     C*
091700940224     C* altrimenti
091800941201     C                   ELSE
091900940224     C* riempio la videata
092000940224     C                   EXSR      RIED1
092100950206     C* Aggiungere le routine di riempimento dei record successivi
092200940224     C* richiamo routine dei ctrl per decodificarla
092300940614     C                   EXSR      CTRD1
092400950206     C* Aggiungere le routine di controllo dei record successivi
092500940224     C                   MOVE      *ALL'0'       IN5098
092600940224     C                   MOVEA     IN5098        *IN(50)
092700940224     C* definizione indicatori su video
092800970207     C                   SETOFF                                       0207
092900941201     C                   SELECT
093000941221     C     OPZ312        WHENEQ    '02'
093100940224     C                   SETON                                        02
093200970207     c* se cliente proteggo anche numero protocollo esterno
093300970207     c                   if        cof312 = 'C'
093400970207     c                   seton                                        07
093500970207     c                   endif
093600941221     C     OPZ312        WHENEQ    '05'
093700941201     C                   ENDSL
093800940224     C*
093900941201     C                   ENDIF
094000940223     C*
094100950914     C                   ELSE
094200950914     C* se immissione
094300950914     C                   MOVE      XSCDIV        D1MC
094400991217B0136 *Se NON è un fornitore propongo a video la data di oggi
094500991217B0136C                   If        Cof312     <> 'F'
094600000114A0735C*****              MOVE      UDATE         D1DIN
094700000114A0735C                   MOVE      ZDate         D1DIN
094800991217B0136C                   EndIf
094900991217  "   *
095000991217  "   *Reperisco il nr. protocollo interno  solo in CTRD1
095100991216  "   *(in base alla data protocollo interno impostata dall'utente)
095200991216B0136C*******************EXSR      REPNUM
095300950914     C*
095400941201     C                   ENDIF
095500050801B0822c* definizione tasti
095600050801B0822c                   EXSR      DEFOPZ
095700940224     C*
095800940117     C                   ENDSR
095900940207     C/EJECT
096000940207     C************************************************************
096100940510     C* RIEMPIMENTO VIDEATA  D1
096200940207     C************************************************************
096300940207     C     RIED1         BEGSR
096400940207     C*
096500941221     C                   MOVEL     DICKSC        D1KSC
096600941221     C                   MOVEL     DICNRC        D1NRC
096700951013     C* Se opzione Copia imposto oggi in Data protocollo interno
096800951013     C* e reperisco Nr. protocollo interno dal numeratore
096900951012     C     OPZ312        IFEQ      '03'
097000991217B0136 *Se NON è un fornitore propongo a video la data di oggi
097100991217B0136C                   If        Cof312     <> 'F'
097200000114A0735C*****              MOVE      UDATE         D1DIN
097300000114A0735C                   MOVE      ZDate         D1DIN
097400991217B0136C                   EndIf
097500991217  "   *Reperisco il nr. protocollo interno nel ctrD1
097600991216  "   *(in base alla data protocollo interno impostata dall'utente)
097700991216B0136C*******************EXSR      REPNUM
097800951012     C                   ELSE
097900960416     C                   CALLb     'XDT4000'
098000951012     C                   PARM      DICDIN        XDTAMG           10
098100951012     C                   PARM                    XDTGMA            6 0
098200951012     C                   PARM      2             XDTSTA            1 0
098300951012     C     XDTSTA        IFEQ      0
098400951012     C                   MOVE      XDTGMA        D1DIN
098500951012     C                   ENDIF
098600941221     C                   MOVEL     DICNIN        D1NIN
098700951012     C                   ENDIF
098800950130     C                   Z-ADD     DICIMP        D1IMP
098900950914     C                   MOVEL     DICIVA        D1IVA
099000950914     C     DICOPE        IFEQ      *ON
099100950914     C                   MOVE      XSCON         D1OPE
099200950914     C                   ELSE
099300950914     C                   MOVE      XSCOFF        D1OPE
099400950914     C                   ENDIF
099500950914     C                   Z-ADD     DICAAA        D1AAA
099600950914     C                   MOVEL     DICDIV        D1MC
099700950914     C     D1MC          IFEQ      *BLANK
099800950914     C                   MOVE      XSCDIV        D1MC
099900950914     C                   ENDIF
100000991228B0140C* se copia pulisco campi relativi alla revoca
100100991228  "  C     OPZ312        IFEQ      '03'
100200991228  "  C                   MOVE      XSCOFF        D1REV
100300991228  "  C                   MOVE      *BLANK        D1TPT
100400141217B0140C                   Z-ADD     0             D1DTR
100500150122R160 C                   Z-ADD     0             D1DTRICP
100600150112R160 C                   MOVE      XSCOFF        D1Inviato
100700150113R160 C                   MOVE      *BLANK        D1File
100800141217B0140C                   ELSE
100900991228B0140C                   If        CoF312 = 'F'
101000951002     C     DICREV        IFEQ      *ON
101100951002     C                   SETON                                        10
101200951002     C                   MOVE      XSCON         D1REV
101300951002     C                   ELSE
101400951002     C                   SETOFF                                       10
101500951002     C                   MOVE      XSCOFF        D1REV
101600951002     C                   ENDIF
101700150112R160 C                   if        DifInviato = *on
101800150112 "   C                   move      xscon         D1Inviato
101900141217 "   C                   else
102000150112 "   C                   move      xscoff        D1Inviato
102100150113 "   C                   endif
102200150113R160 C                   eval      D1File = DifFileTel
102300991228B0140C                   Else
102400141217B0140C                   Move      XSCOff        D1Rev
102500150112R160 C                   Move      XSCOff        D1Inviato
102600150113R160 C                   MOVE      *BLANK        D1File
102700991228B0140C                   EndIf
102800991228     C                   MOVEL     DICTPT        D1TPT
102900991228     C                   CALLb     'XDT4000'
103000991228     C                   PARM      DICDTR        XDTAMG           10
103100991228     C                   PARM                    XDTGMA            6 0
103200991228     C                   PARM      2             XDTSTA            1 0
103300991228     C     XDTSTA        IFEQ      0
103400991228     C                   MOVE      XDTGMA        D1DTR
103500991228     C                   ENDIF
103600991228B0140C                   ENDIF
103700951002     C                   MOVEL     DICOPI        D1OPI
103800960416     C                   CALLb     'XDT4000'
103900941221     C                   PARM      DICDTC        XDTAMG           10
104000941221     C                   PARM                    XDTGMA            6 0
104100941221     C                   PARM      2             XDTSTA            1 0
104200941221     C     XDTSTA        IFEQ      0
104300941221     C                   MOVE      XDTGMA        D1DTC
104400941221     C                   ENDIF
104500960416     C                   CALLb     'XDT4000'
104600941221     C                   PARM      DICDTI        XDTAMG           10
104700941221     C                   PARM                    XDTGMA            6 0
104800941221     C                   PARM      2             XDTSTA            1 0
104900941221     C     XDTSTA        IFEQ      0
105000941221     C                   MOVE      XDTGMA        D1DTI
105100941221     C                   ENDIF
105200960416     C                   CALLb     'XDT4000'
105300941221     C                   PARM      DICDTF        XDTAMG           10
105400941221     C                   PARM                    XDTGMA            6 0
105500941221     C                   PARM      2             XDTSTA            1 0
105600941221     C     XDTSTA        IFEQ      0
105700941221     C                   MOVE      XDTGMA        D1DTF
105800941221     C                   ENDIF
105900940207     C*
106000970210     C* SALVO DATA/NR. PROTOCOLLO esterno
106100970210     C                   MOVEL     D1Dtc         DtcSAV
106200970210     C                   MOVEL     D1Nrc         NrcSAV
106300951006     C* SALVO DATA INVIO LETTERA
106400040213     C                   MOVEL     DICDIT        DITSAV           10
106500040213     c*
106600040213D1540c                   if        cof312 ='C'
106700040213  "  C                   CALLb     'XDT4000'
106800040213  "  C                   PARM      DICDIT        XDTAMG           10
106900040213  "  C                   PARM                    XDTGMA            6 0
107000040213  "  C                   PARM      2             XDTSTA            1 0
107100040213  "  C     XDTSTA        IFEQ      0
107200040213  "  C                   MOVE      XDTGMA        D1DTLET
107300150122D1540C                   ENDIF
107400150122R160 C                   CALLb     'XDT4000'
107500150122  "  C                   PARM      DIFDTRICP     XDTAMG           10
107600150122  "  C                   PARM                    XDTGMA            6 0
107700150122  "  C                   PARM      2             XDTSTA            1 0
107800150122  "  C     XDTSTA        IFEQ      0
107900150122  "  C                   MOVE      XDTGMA        D1DTRICP
108000150122R160 C                   ENDIF
108100040213D1540C                   ENDIF
108200141217R160 C*
108300141217R160 C                   Move      DifDtInvio    H1DtInvio
108301160504R226 C*
108302160505  "  C                   Movel     DifProt       D1Prot
108303160505R226 C                   Movel     DifProtD      D1ProtD
108400951004     C*
108500940207     C                   ENDSR
108600940916     C/EJECT
108700940916     C************************************************************
108800940916     C* GESTIONE F02 VIDEO D1
108900940916     C************************************************************
109000940916     C     F02D1         BEGSR
109100940916     C*
109200940916     C* per decodificare la videata richiamo la routine dei ctrl
109300940916     C* e poi disattivo gli eventuali messaggi di errore
109400940916     C                   EXSR      CTRD1
109500940916     C*
109600940916     C                   MOVE      *ALL'0'       IN5098
109700940916     C                   MOVEA     IN5098        *IN(50)
109800940916     C*
109900940916     C                   ENDSR
110000940309     C/EJECT
110100940309     C************************************************************
110200940309     C* GESTIONE F03 VIDEO D1
110300940309     C************************************************************
110400940309     C     F03D1         BEGSR
110500940309     C*
110600940309     C                   MOVE      *ON           $FINE
110700941221     C                   MOVE      '1'           RET312
110800940325     C* fine programma
110900940325     C                   EXSR      ENDPGM
111000940325     C*
111100940325     C                   ENDSR
111200940309     C/EJECT
111300940309     C************************************************************
111400940309     C* GESTIONE F05 VIDEO D1
111500940309     C************************************************************
111600940309     C     F05D1         BEGSR
111700940309     C*
111800940309     C                   MOVE      *ON           $INZD1
111900940309     C*
112000940309     C                   ENDSR
112100940309     C/EJECT
112200940309     C************************************************************
112300940309     C* GESTIONE F08 VIDEO D1
112400940309     C************************************************************
112500940309     C     F08D1         BEGSR
112600940309     C*
112700940309     C                   MOVE      *ON           $FINE
112800941221     C                   MOVE      '0'           RET312
112900940309     C*
113000940309     C                   ENDSR
113100940309     C/EJECT
113200940309     C************************************************************
113300940309     C* GESTIONE F12 VIDEO D1
113400940309     C************************************************************
113500940309     C     F12D1         BEGSR
113600940309     C*
113700940309     C                   MOVE      *ON           $FINE
113800941221     C                   MOVE      '2'           RET312
113900940309     C*
114000940309     C                   ENDSR
114100940309     C/EJECT
114200050801B0822C************************************************************
114300050801  "  C* GESTIONE F20 VIDEO D1/D2 Annullamento revoca
114400050801B0822C************************************************************
114500050801     C     F20D1         BEGSR
114600050801     C*
114700050801   B1C     *IN22         DOUEQ     *OFF
114800050801     C     MSGRTN        ORNE      'R'
114900050801     C* blocco il record
115000050801     C     K4DIC2        CHAIN     ANDIC02                            2122
115100050801     C*
115200050801     C* Gestione RCD allocato
115300050801   B2C     *IN22         IFEQ      *ON
115400050801    >C                   MOVEL     'ANDIC00F'    MSGFIL
115500050801     C                   EXSR      MSGPGM
115600050801   E2C                   ENDIF
115700050801   E1C                   ENDDO
115800050801     C* se il rcd da gestire non viene trovato
115900050801     C* oppure è allocato da altro pgm
116000050801     C* ritorno al pgm guida avvertendolo dell'errore
116100050801   B1C     *IN21         IFEQ      *ON
116200050801     C     *IN22         OREQ      *ON
116300050801     C                   MOVE      *ON           $FINE
116400050801     C                   MOVE      '1'           ERR312
116500050801     C                   EXSR      ENDPGM
116600050801     c                   else
116700050801     c                   eval      DicRev     = *off
116800050801     c                   clear                   dictpt
116900050801     c                   move      '0001-01-01'  dicdtr
117000050801     c                   except    aggdic
117100050801     c                   endif
117200050801     c*
117300050801     C                   EXSR      GesAgg
117400050801     C*
117500050801     C                   ENDSR
117600050801     C/EJECT
117700940309     C************************************************************
117800940309     C* GESTIONE F24 VIDEO D1
117900940309     C************************************************************
118000940309     C     F24D1         BEGSR
118100940309     C*
118200940506     C                   MOVEA     $FX1          $FX
118300940506     C                   MOVEA     $FC1          $FC
118400940506     C                   MOVEA     $FR1          $FR
118500940506     C                   Z-ADD     $ULKD1        $ULK
118600940506     C                   EXSR      RIEKEY
118700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
118800940720     C                   MOVEA     $FC           $FC1
118900940506     C                   MOVEL     $KEY1         Z1KE1
119000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
119100940506     C                   MOVEL     $KEY2         Z1KE2
119200940506     C                   WRITE     FMTZ1
119300940506     C*
119400940309     C                   ENDSR
119500940207     C/EJECT
119600940131     C************************************************************
119700940207     C* CONTROLLO VIDEATA
119800940131     C************************************************************
119900940127     C     CTRD1         BEGSR
120000940131     C*
120100940127     C                   SETOFF                                       99
120200940127     C* non imposto pos. del cursore
120300940127     C                   Z-ADD     0             H1RIGA
120400940127     C                   Z-ADD     0             H1COLO
120500940223     C*
120600940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
120700940223     C*
120800941221     C     OPZ312        IFNE      '04'
120900940203     C*
121000970725     C* Nr. Protocollo ESTERNO è obbligatorio solo per i clienti
121100941221     C     D1NRC         IFEQ      *ZERO
121200970207     c     cof312        andeq     'C'
121300941221     C                   SETON                                        5099
121400941221     C                   ENDIF
121500970725     C*
121600991216B0136 **************************************************************
121700991216  "   *Sposto il ctl dopo il ctl dellla data/nr. protocollo interno
121800991216  "   *in tal modo posso reperire il numero qualore fosse = 0
121900991216  "   **************************************************************
122000991216  "  C**Nr. Protocollo INTERNO
122100991216  "  C*****D1NIN         IFEQ      *ZERO
122200991216  "  C****************** SETON                                        5399
122300991216B0136C****************** ENDIF
122400941221     C*
122500970725     C* Data Protocollo ESTERNO
122600970207     C                   MOVE      $DT0          DTCAMG
122700941221     C     D1DTC         IFNE      *ZERO
122800960416     C                   CALLb     'XDT4000'
122900941221     C                   PARM                    XDTAMG           10
123000941221     C                   PARM      D1DTC         XDTGMA            6 0
123100941221     C                   PARM      3             XDTSTA            1 0
123200941221     C     XDTSTA        IFNE      0
123300941222     C                   SETON                                        5299
123400941221     C                   ELSE
123500941221     C                   MOVE      XDTAMG        DTCAMG           10
123600941221     C                   ENDIF
123700941221     C                   ELSE
123800970725     C* (è obbligatoria solo per i clienti)
123900970207     c     cof312        ifeq      'C'
124000941222     C                   SETON                                        5199
124100970207     c                   endif
124200941221     C                   ENDIF
124300941221     C*
124400970725     C* Data Protocollo INTERNO
124500941221     C     D1DIN         IFNE      *ZERO
124600960416     C                   CALLb     'XDT4000'
124700941221     C                   PARM                    XDTAMG           10
124800941221     C                   PARM      D1DIN         XDTGMA            6 0
124900941221     C                   PARM      3             XDTSTA            1 0
125000941221     C     XDTSTA        IFNE      0
125100941222     C                   SETON                                        5599
125200941221     C                   ELSE
125300941221     C                   MOVE      XDTAMG        DINAMG           10
125400941221     C                   ENDIF
125500941221     C                   ELSE
125600941222     C                   SETON                                        5499
125700941221     C                   ENDIF
125800991216      *
125900991216B0136C* Nr. Protocollo INTERNO
126000040304B0688c                   if        opz312='01' or opz312='03'
126100040301B0688X**** D1NIN         IFEQ      *ZERO
126200040315B0136 *
126300991216  "   * Se la data Data/nr. protocollo interno è valorizzata
126400991216  "  C     DINAMG        IFNE      $DT0
126500991216  "   *  reperisco il nr. protocollo interno
126600991216  "  C                   EXSR      REPNUM
126700991216  "   *
126800991216  "   * Se la data Data/nr. protocollo interno NON è valorizzata
126900991216  "  C                   Else
127000991216  "   *  nr. protocollo interno obbligatorio
127100040315B0136C                   SETON                                        5399
127200040315B0688c                   ENDIF
127300040315B0136 *
127400991216B0136C                   EndIf
127500951004     C*
127600970725     C* CTRL UNIVOCITA' DATA/NR. PROTOCOLLO ESTERNO
127700970725     C* (Solo per i clienti !!!)
127800970725     C     cof312        IFEQ      'C'
127900970210     C     D1Dtc         IFNE      DtcSAV
128000970210     C     D1Nrc         ORNE      NrcSAV
128100970210     C                   MOVEL     WWWSOC        DICSOC
128200970210     C                   MOVEL     COF312        DICCF
128300970210     C                   CALLb     'XALLINEA'
128400970210     C                   PARM                    D1KSC
128500970210     C                   PARM      8             LEN               5 0
128600970210     C                   PARM                    XSCINP
128700970210     C                   MOVEL     D1KSC         DICKSC
128800970210     C                   MOVEL     DtcAMG        DICdtc
128900970210     C                   MOVEL     D1nrc         DICnrc
129000970210     C     K5DIC1        setll     ANDIC01l                               21
129100970725     C     *IN21         IFEQ      *ON
129200970725     C                   SETON                                        7599
129300970210     C                   ENDIF
129400970210     C                   ENDIF
129500970725     C                   ENDIF
129600950914     C*
129700950915     C* CTRL VALIDITA' DATA REVOCA
129800950915     C                   MOVEL     $DT0          DTRAMG
129900950914     C     D1DTR         IFNE      *ZERO
130000960416     C                   CALLb     'XDT4000'
130100950914     C                   PARM                    XDTAMG           10
130200950914     C                   PARM      D1DTR         XDTGMA            6 0
130300950914     C                   PARM      3             XDTSTA            1 0
130400950914     C     XDTSTA        IFNE      0
130500950914     C                   SETON                                        6599
130600950914     C                   ELSE
130700950914     C                   MOVE      XDTAMG        DTRAMG           10
130800950914     C                   ENDIF
130900950914     C                   ENDIF
131000040213     C*
131100040213D1540C* CTRL VALIDITA' Data ricevimento lettera solo per clienti
131200040213  "  C                   MOVEL     $DT0          $DTLAMG
131300040213  "  c                   if        Cof312='C'
131400040213  "  C     D1DtLet       IFNE      *ZERO
131500040213  "  C                   CALLb     'XDT4000'
131600040213  "  C                   PARM                    XDTAMG           10
131700040213  "  C                   PARM      D1DtLet       XDTGMA            6 0
131800040213  "  C                   PARM      3             XDTSTA            1 0
131900040213  "  C     XDTSTA        IFNE      0
132000040213  "  C                   SETON                                        5999
132100040213  "  C                   ELSE
132200040213  "  C                   MOVE      XDTAMG        $DTLAMG          10
132300040213  "  C                   ENDIF
132400040213  "  C                   ENDIF
132500040213D1540C                   endif
132600950914     C*
132700150122 R160C* CTRL VALIDITA' Data ricevuta presentaz. solo per clienti
132800150122  "  C                   MOVEL     $DT0          $DTRICPAMG
132900150122  "  c                   if        Cof312='C'
133000150122  "  C     D1DtRicP      IFNE      *ZERO
133100150122  "  C                   CALLb     'XDT4000'
133200150122  "  C                   PARM                    XDTAMG           10
133300150122  "  C                   PARM      D1DtRicP      XDTGMA            6 0
133400150122  "  C                   PARM      3             XDTSTA            1 0
133500150122  "  C     XDTSTA        IFNE      0
133600150122  "  C                   SETON                                        8299
133700150122  "  C                   ELSE
133800150122  "  C                   MOVE      XDTAMG        $DTRICPAMG       10
133900150122  "  C                   ENDIF
134000150122  "  C                   ENDIF
134100150122 R160C                   endif
134200950914     C* CTRL IVA
134300950914     C                   MOVEL     *BLANK        D1IVAD
134400950914     C     D1IVA         IFNE      *BLANK
134500950914     C                   CLEAR                   X06DS
134600950914     C                   MOVE      WWWSOC        X06SOC
134700950914     C                   MOVE      XSCLIN        X06LIN
134800950914     C                   MOVE      D1IVA         X06CIV
134900950914     C                   MOVE      WDATE         X06DAT
135000950914     C                   CALL      'X06G48'
135100950914     C                   PARM                    X06DS
135200950914     C     X06ERR        IFEQ      '0'
135300950914     C                   MOVEL     X06UNI        TABG48
135400950914     C                   MOVEL     DESG48        D1IVAD
135500950914     C                   ELSE
135600950914     C                   SETON                                        6199
135700950914     C                   ENDIF
135800950914     C                   ELSE
135900950914     C                   SETON                                        6099
136000950914     C                   ENDIF
136100951002     C*
136200951002     C* CTRL OPERAZ. IMPORTAZIONE
136300951002     C                   MOVEL     *BLANK        D1OPID
136400951002     C     D1OPI         IFNE      *BLANK
136500951002     C                   MOVE      XCACUT        XTBUTE
136600951002     C                   MOVE      '1'           XTBRIC
136700951002     C                   MOVE      XSCSOC        XTBAZI
136800951002     C                   MOVE      *ZERO         XTBKEY
136900951002     C                   MOVE      'G17'         XTBCOD
137000951002     C                   MOVE      D1OPI         XTBKEY
137100961004     C                   CALLb     'XATB'
137200951002     C                   PARM                    XATBDS
137300951002     C     XTBERR        IFEQ      *ON
137400951002     C                   SETON                                        7399
137500951002     C                   ELSE
137600951002     C                   MOVEL     XTBUNI        TABG17
137700951002     C                   MOVEL     DESG17        D1OPID
137800951002     C                   ENDIF
137900951002     C                   ELSE
138000951002     C                   SETON                                        7299
138100951002     C                   ENDIF
138200950914     C*
138300950915     C* CTRL VALIDITA' DATE INIZIO/FINE
138400950914     C                   MOVEL     $DT0          DTIAMG
138500950914     C                   MOVEL     $DT0          DTFAMG
138600950914     C     D1DTI         IFNE      *ZERO
138700960416     C                   CALLb     'XDT4000'
138800950914     C                   PARM                    XDTAMG           10
138900950914     C                   PARM      D1DTI         XDTGMA            6 0
139000950914     C                   PARM      3             XDTSTA            1 0
139100950914     C     XDTSTA        IFNE      0
139200950914     C                   SETON                                        5799
139300950914     C                   ELSE
139400950914     C                   MOVE      XDTAMG        DTIAMG           10
139500950914     C                   ENDIF
139600950914     C                   ENDIF
139700950914     C*
139800950914     C     D1DTF         IFNE      *ZERO
139900960416     C                   CALLb     'XDT4000'
140000950914     C                   PARM                    XDTAMG           10
140100950914     C                   PARM      D1DTF         XDTGMA            6 0
140200950914     C                   PARM      3             XDTSTA            1 0
140300950914     C     XDTSTA        IFNE      0
140400950914     C                   SETON                                        6499
140500950914     C                   ELSE
140600950914     C                   MOVE      XDTAMG        DTFAMG           10
140700950914     C                   ENDIF
140800950914     C                   ENDIF
140900950914     C*
141000950914     C     D1DTI         IFNE      0
141100950914     C     D1DTF         ANDEQ     0
141200950914     C     D1DTI         OREQ      0
141300950914     C     D1DTF         ANDNE     0
141400950914     C                   SETON                                        5699
141500950914     C                   ENDIF
141600950914     C*
141700950914     C     *IN56         IFEQ      *OFF
141800950914     C     *IN57         ANDEQ     *OFF
141900950914     C     *IN64         ANDEQ     *OFF
142000950914     C     DTIAMG        IFGT      DTFAMG
142100950914     C                   SETON                                        5899
142200950914     C                   ENDIF
142300950914     C                   ENDIF
142400941215     C*
142500950915     C     COF312        IFEQ      'F'
142600951004     C*
142700950915     C* CTRL FLAG STAMPA LETTERA REVOCA
142800950915     C     D1REV         IFNE      XSCON
142900950915     C     D1REV         ANDNE     XSCOFF
143000950915     C                   SETON                                        7199
143100950915     C                   ENDIF
143200141217R160 C*
143300141217 "   C* Flag dichiarazione estratta
143400150112 "   C     D1Inviato     IFNE      XSCON
143500150112 "   C     D1Inviato     ANDNE     XSCOFF
143600141217 "   C                   SETON                                        8499
143700141217R160 C                   ENDIF
143800950915     C*
143900950915     C* CTRL TIPO TESTO REVOCA
144000950914     C                   MOVE      *BLANK        D1TPTD
144100950914     C     D1TPT         IFNE      *BLANK
144200000519A0778X***                MOVE      WWWSOC        TXTSOC
144300000519  "  X***                MOVE      XSCDUN        TXTUNI
144400000519  "  X***                MOVE      D1TPT         TXTTPT
144500000519  "  X***                MOVE      XSCLIN        TXTLIN
144600000519  "  X***                MOVE      'A'           TXTPOS
144700000519  "  X***  K5TXT1        CHAIN     ANTXT01L                           21
144800000519  "  X***  *IN21         IFEQ      *OFF
144900000519  "  X***                MOVEL     TXTTES        D1TPTD
145000000519  "   *
145100000519  "  C                   eval      XEsito = *Off
145200000519  "  C                   eval      XOpe = '6'
145300000519  "  C                   eval      XSocieta = WWWSoc
145400000523  "  C                   eval      XUnita   = *blank
145500000519  "  C                   eval      XTpTesto = D1TpT
145600000519  "  C                   eval      XLingua  = XScLin
145700000523  "  C                   eval      XClaUte = *blanks
145800000519  "  C                   eval      XPosRig = 'A'
145900000519  "  C                   eval      XNrRiga = *zeros
146000000519  "  C                   eval      XRibat = *blanks
146100000519  "  C                   eval      XTesRig = *blanks
146200000519  "  C                   eval      XNrChiavi = 4
146300000519  "  C                   eval      XUsoUnita = '0'
146400000519  "   *
146500000519  "  C                   exsr      CallX72
146600000519  "   *
146700000519  "  C                   if        XEsito = *off
146800000519A0778C                   movel     XTesRig       D1TpTD
146900950914     C                   ELSE
147000950914     C                   SETON                                        6799
147100950914     C                   ENDIF
147200950914     C                   ENDIF
147300950915     C*
147400950915     C     D1REV         IFEQ      XSCON
147500950915     C     D1TPT         ANDEQ     *BLANK
147600950915     C                   SETON                                        6299
147700950915     C                   ENDIF
147800950915     C*
147900950915     C     D1REV         IFEQ      XSCON
148000950915     C     D1DTR         ANDEQ     0
148100950915     C                   SETON                                        6399
148200950915     C                   ENDIF
148300950915     C*
148400950915     C                   ENDIF
148500950914     C*
148600950915     C* CTRL FLAG OPERAZIONE SPECIFICATA
148700950914     C     D1OPE         IFNE      XSCON
148800950914     C     D1OPE         ANDNE     XSCOFF
148900950914     C                   SETON                                        6699
149000950914     C                   ENDIF
149100950914     C*
149200950914     C* CTRL ANNO/IMPORTO
149300150108R160FC                   if        D1OPE   <> XSCON
149400950914     C     D1AAA         IFNE      0
149500950914     C     D1IMP         ANDEQ     0
149600950914     C     D1AAA         OREQ      0
149700950914     C     D1IMP         ANDNE     0
149800950914     C                   SETON                                        6899
149900950914     C                   ENDIF
150000150108R160FC                   else
150100150108 "   C                   if        D1IMP    = 0
150200150108 "   C                   SETON                                        7799
150300150108 "   C                   endif
150400150108R160FC                   endif
150500950914     C*
150600950914     C* CTRL VALORI ALTERNATIVI
150700950914     C     D1OPE         IFEQ      XSCOFF
150800950914     C     D1AAA         ANDEQ     0
150900950914     C     D1DTI         ANDEQ     0
151000950914     C                   SETON                                        6999
151100950914     C                   ELSE
151200950914     C     D1OPE         IFEQ      XSCON
151300950914     C     D1AAA         ANDNE     0
151400950914     C                   SETON                                        7099
151500950914     C                   ELSE
151600950914     C     D1OPE         IFEQ      XSCON
151700950914     C     D1DTI         ANDNE     0
151800950914     C                   SETON                                        7099
151900950914     C                   ELSE
152000950914     C     D1AAA         IFNE      0
152100950914     C     D1DTI         ANDNE     0
152200950914     C                   SETON                                        7099
152300950914     C                   ENDIF
152400950914     C                   ENDIF
152500950914     C                   ENDIF
152600950914     C                   ENDIF
152700041202B0762c* controllo congruenze
152800041202  "  c* Anno data protocollo interno deve essere uguale ad anno valore
152900041202  "  c* dichiarazione se immesso
153000041202  "  c                   move      DinAmg        DataIso
153100041202  "  c                   extrct    DataIso:*Y    AnnoDic           4 0
153200041202  "  c                   if        D1AAA <> 0
153300041202  "  c                   if        AnnoDic <> D1AAA
153400041202  "  c                   seton                                        7499
153500041202  "  c                   endif
153600041202  "  c                   endif
153700041202  "  c* Anno data protocollo interno deve essere uguale ad anno periodo
153800041202  "  c* da .... a...  se immesso
153900041202  "  c                   if        D1Dti <> 0
154000041202  "  c                   move      DtiAmg        DataIso
154100041202  "  c                   extrct    DataIso:*Y    AnnoDTI           4 0
154200041202  "  c                   if        AnnoDic <> AnnoDti
154300041202  "  c                   seton                                        7699
154400041202  "  c                   endif
154500041202  "  c                   move      DtfAmg        DataIso
154600041202  "  c                   extrct    DataIso:*Y    AnnoDTF           4 0
154700041202  "  c                   if        AnnoDic <> AnnoDtF
154800041202  "  c                   seton                                        7699
154900041202  "  c                   endif
155000041202B0762c                   endif
155001160504R226 C* Se inseriti i dati del protocollo ricevuto, devono esserci entrambi
155002160505  "  C                   If        (D1Prot   <> 0 and
155003160505  "  C                              D1ProtD = 0)  or
155004160505  "  C                             (D1Prot   =  0 and
155005160505  "  C                              D1ProtD <>0)
155006160504  "  C                   Seton                                        7899
155007160504R226 C                   Endif
155100041202     c*
155200970210     C*
155300970210     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
155400970210     C     OPZ312        IFEQ      '01'
155500970210     C     OPZ312        OREQ      '03'
155600970210     C                   MOVEL     WWWSOC        DICSOC
155700970210     C                   MOVEL     COF312        DICCF
155800970725     C                   MOVEL     D1NIN         DICNIN
155900970725     C                   MOVEL     DINAMG        DICDIN
156000970725     C     K4DIC2        SETLL     ANDIC02                                21
156100970210     C     *IN21         IFEQ      '1'
156200970725     C                   SETON                                        9099
156300970210     C                   ENDIF
156400970210     C                   ENDIF
156500970210     C*
156600941027     C                   ENDIF
156700940127     C*
156800940117     C                   ENDSR
156900940224     C/EJECT
157000940224     C************************************************************
157100940315     C* AGGIORNAMENTO ANAGRAFICA
157200940224     C************************************************************
157300941214     C     AGGANA        BEGSR
157400940207     C*
157500940207     C                   SELECT
157600941221     C     OPZ312        WHENEQ    '01'
157700941221     C     OPZ312        OREQ      '03'
157800941214     C                   EXSR      RIEANA
157900970725     C                   WRITE     ANDIC02                              22
158000141217R160 C                   write     SdgDif000                            22
158100940318     C* se rcd non scrivibile attivo errore
158200940318     C     *IN22         IFEQ      *ON
158300941201     C                   SETON                                        9599
158400941027     C                   ENDIF
158500971002     C*
158600941221     C     OPZ312        WHENEQ    '02'
158700941214     C                   EXSR      RIEANA
158800970725     C                   UPDATE    ANDIC02
158900141217R160 C                   update    SdgDif000
159000971002     C*
159100941221     C     OPZ312        WHENEQ    '04'
159200970725     C                   DELETE    ANDIC02
159300141217R160 C                   delete    SdgDif000
159400940207     C                   ENDSL
159500950323      *
159600950323      * richiama pgm User Exit
159700950323      *
159800950323     C     USRERR        IFEQ      *BLANK
159900950323     C     *IN99         ANDEQ     *OFF
160000950323      * imposta chiave univoca  per user exit
160100950323     C                   MOVE      XSCSOC        XUKSOC
160200950323     C                   MOVE      XSCDUN        XUKDUN
160300950323     C                   MOVEL     NOMPGM        XUKPGM
160400950323     C                   MOVEL     DICSOC        XUKFL1
160500950323     C                   MOVEL     DICKSC        XUKFL2
160600950323     C                   MOVEL     DICNRC        XUKFL3
160700950323     C                   MOVEL     DICDTC        XUKFL4
160800950323     C                   MOVEL     XUKDS         KPJBU
160900950323     C     NOMPGM        CAT       'U':0         USREXT           10
161000950323     C                   CALL      USREXT                               21
161100950323     C                   PARM                    KPJBA
161200950323     C     *IN21         IFEQ      *ON
161300950323     C                   MOVE      *ON           USRERR            1
161400950323     C                   ENDIF
161500950323     C                   ENDIF
161600940203     C*
161700940203     C                   ENDSR
161800940224     C/EJECT
161900940224     C************************************************************
162000940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
162100940224     C************************************************************
162200941214     C     RIEANA        BEGSR
162300141217R160 C*
162400141217 "   C                   clear                   SdgDif000
162500141217 "   C                   movel     WWWSOC        DifSocieta
162600141217 "   C                   move      Cof312        DifCf
162700141217 "   C                   movel     D1NIN         DifNrInter
162800141217 "   C                   movel     DINAMG        DifDtInter
162900141217 "   C                   if        opz312='01' or opz312='03'
163000141217 "   C                   movel     $DT0          DifDtInvio
163100141217 "   C                   else
163200141217 "   C                   movel     H1DtInvio     DifDtInvio
163300150113 "   C                   movel     D1File        DifFileTel
163400141217R160 C                   endif
163500951006     C*
163600970725     C                   CLEAR                   ANDIC02
163700951006     C*
163800950110     C                   MOVEL     WWWSOC        DICSOC
163900941221     C                   MOVEL     D1KSC         DICKSC
164000941221     C                   MOVEL     D1NRC         DICNRC
164100941221     C                   MOVEL     DTCAMG        DICDTC
164200951012     C                   MOVEL     D1NIN         DICNIN
164300941221     C                   MOVEL     DINAMG        DICDIN
164400941221     C                   MOVEL     DTIAMG        DICDTI
164500941221     C                   MOVEL     DTFAMG        DICDTF
164600941221     C                   MOVEL     D1IVA         DICIVA
164700950130     C                   Z-ADD     D1IMP         DICIMP
164800950914     C     D1OPE         IFEQ      XSCON
164900950914     C                   MOVE      *ON           DICOPE
165000950914     C                   ELSE
165100950914     C                   MOVE      *OFF          DICOPE
165200950914     C                   ENDIF
165300950914     C                   Z-ADD     D1AAA         DICAAA
165400950914     C                   MOVE      D1MC          DICDIV
165500950914     C                   MOVE      COF312        DICCF
165600951002     C                   MOVE      D1OPI         DICOPI
165700040213D1540X***                MOVE      DITSAV        DICDIT
165800040213  "  c* fornitori
165900040213  "  C   11              MOVE      DITSAV        DICDIT
166000040213  "  c* clienti
166100040213D1540C  N11              move      $DTLAmg       DicDit
166200150126R160 X**N11              move      $DTRicPAmg    DifDtRicP
166300150126R163 C                   move      $DTRicPAmg    DifDtRicP
166400040213     C*
166500991228B0140C**** OPZ312        IFEQ      '03'
166600991228  "  C****               MOVE      $DT0          DICDTR
166700991228  "  C****               MOVE      *BLANK        DICTPT
166800991228  "  C****               MOVE      *OFF          DICREV
166900991228B0140C****               ELSE
167000951011     C                   MOVE      DTRAMG        DICDTR
167100951011     C                   MOVE      D1TPT         DICTPT
167200991228B0140C* se fornitori il flag di revoca è deciso a video
167300991228B0140C                   If        CoF312 = 'F'
167400951011     C     D1REV         IFEQ      XSCON
167500951011     C                   MOVE      *ON           DICREV
167600951011     C                   ELSE
167700951011     C                   MOVE      *OFF          DICREV
167800951011     C                   ENDIF
167900150112R160 C                   if        D1Inviato = XscOn
168000150112 "   C                   move      *on           DifInviato
168100141217 "   C                   else
168200150112 "   C                   move      *off          DifInviato
168300141217R160 C                   endif
168400991228B0140C* se clienti il flag di revoca è deciso in base alla data revoca
168500991228  "  C                   Else
168600991228  "  C                   If        DICDtR <> $DT0
168700991228  "  C                   Move      *On           DICRev
168800991228  "  C                   Else
168900991228  "  C                   Move      *Off          DICRev
169000991228  "  C                   EndIf
169100991228  "  C                   EndIf
169200991228B0140C****               ENDIF
169201160504R226 C*
169202160505  "  C                   Movel     D1Prot        DIFProt
169203160505R226 C                   Movel     D1ProtD       DIFProtD
169300940207     C*
169400940207     C                   ENDSR
169500940224     C/EJECT
169600951012     C************************************************************
169700951012     C* REPERIMENTO NR. PROTOCOLLO INTERNO
169800951012     C************************************************************
169900951012     C     REPNUM        BEGSR
170000951012     C*
170100040301B0688C     D1NIN         IFEQ      *ZERO
170200951012     C                   MOVE      '3'           XNMRIC
170300040301B0688C                   ELSE
170400040301  "  C                   move      '1'           Xnmric
170500040301B0688C                   ENDIF
170600951012     C                   MOVE      XSCSOC        XNMSOC
170700951012     C                   MOVE      *BLANKS       XNMUNI
170800951012     C                   MOVE      *BLANKS       XNMCTB
170900951012     C                   MOVE      *BLANKS       XNMKEY
171000951012     C     COF312        IFEQ      'C'
171100951012     C                   MOVE      'PCLI'        XNMSER
171200951012     C                   ELSE
171300951012     C                   MOVE      'PFOR'        XNMSER
171400951012     C                   ENDIF
171500960416     C                   MOVE      *ON           XNMCMT
171600951012     C                   MOVE      *OFF          XNMIVA
171700951012     C                   MOVE      *OFF          XNMREG
171800991216B0136C****************** MOVE      WDATE         XNMDAT
171900991216  "   *WDate è l'udate in formato 10 alfa
172000991216  "   *per reprire il numero, ho bisogno della data protocollo
172100991216  "   *a video trasformata in una data di 10 alfa
172200991216B0136C                   MOVE      DINAMG        XNMDAT
172300030704B0581c* stessa data per aggiornamento numeratore
172400030704B0581C                   MOVE      DINAMG        XNMDAG
172500961004     C                   CALLb     'XNMR'        PNMR
172600951012     C     XNMERR        IFEQ      *OFF
172700071205B1016c                   if        xnmric='3'
172800951012     C                   Z-ADD     XNMNUM        D1NIN
172900071205B1016c                   endif
173000960416     C                   ELSE
173100030704B0581c                   seton                                        99
173200030704  "  x***                MOVE      *ON           $FINE
173300030704B0581x***                EXSR      ENDPGM
173400951012     C                   END
173500951012     C*
173600951012     C                   ENDSR
173700951012     C/EJECT
173800940224     C************************************************************
173900940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
174000940224     C************************************************************
174100940224     C     GESAGG        BEGSR
174200980223     C*
174300980223     C**Eseguo COMMIT per "liberare" il numeratore
174400980223     C**(se sono in immissione o copia)
174500980223     C     OPZ312        IFEQ      '01'
174600980223     C     OPZ312        OREQ      '03'
174700980223     C                   COMMIT
174800980223     C                   ENDIF
174900940224     C*
175000940224     C* segnalo al pgm chiamante l'avvenuta conferma
175100941221     C                   MOVE      *ON           OPR312
175200940224     C* segnalo al pgm chiamante l'aver premuto l'F6
175300941221     C                   MOVE      '0'           RET312
175400940224     C*
175500951002     C* Stampa lettera revoca
175600951003     C     D1REV         IFEQ      XSCON
175700990507B0013C**** *IN10         ANDEQ     *OFF
175800951003     C                   EXSR      REVOCA
175900951002     C                   ENDIF
176000960416     C*
176100960416     C                   SELECT
176200960416     C*
176300960416     C* nel caso di immissione
176400960416     C     OPZ312        WHENEQ    '01'
176500960416     C* ripristino la videata iniziale
176600960416     C                   MOVE      *ON           $INZD1
176700960416     C                   MOVE      'D1'          $GEST
176800960416     C*
176900960416     C* per tutte le altre opzioni
177000960416     C                   OTHER
177100960416     C* ritorno al pgm chiamante
177200960416     C                   MOVE      *ON           $FINE
177300960416     C                   ENDSL
177400951002     C*
177500940224     C                   ENDSR
177600941205     C/EJECT
177700951003     C******************************************************
177800951003     C* STAMPA LETTERA REVOCA                              *
177900951003     C******************************************************
178000951003     C     REVOCA        BEGSR
178100951003     C*
178200000518B0212C                   Open      ANA312P
178300000518     C*
178400951003     C* Testata
178500951003     C                   MOVE      XSCCIT        CITTA
178600081224D2461C* la stampa lettera revoca è solo per fornitori, reperisco
178700081224  "  c* i dati dell'indirizzo lettera come fa il programma di stampa
178800081224  "  c* lettera NDC401R
178900081224D2461C                   EXSR      DATFOR
179000081224     C*
179100081224D2461x*                  MOVE      D1KSCD        RAGSOC
179200081224  "  x*                  MOVE      D1IND         INDIRI
179300081224  "  x*                  CLEAR                   CAPLOCPRV
179400081224  "  x*                  MOVEL     D1CAP         CAPLOCPRV
179500081224  "  x*                  CAT       D1LOC:1       CAPLOCPRV
179600081224D2461x*                  CAT       D1PRV:1       CAPLOCPRV
179700971204     C                   Z-ADD     D1DTR         DTREV
179800990507B0013C*****              Z-ADD     D1DTC         DTPROT
179900990507  "  C*****              Z-ADD     D1NRC         NRPROT
180000990507  "  C                   Z-ADD     D1DIN         DTPROT
180100990507B0013C                   Z-ADD     D1NIN         NRPROT
180200951003     C                   WRITE     TESTA
180300951003     C*
180400000519A0778X* Righe testo
180500000519  "  X***                MOVE      WWWSOC        TXTSOC
180600000519  "  X***                MOVE      XSCDUN        TXTUNI
180700000519  "  X***                MOVE      D1TPT         TXTTPT
180800000519  "  X***                MOVE      XSCLIN        TXTLIN
180900000519  "  X***                MOVE      'A'           TXTPOS
181000000519  "  X***  K5TXT1        SETGT     ANTXT01L
181100000519  "  X***  K4TXT1        READE     ANTXT01L                               21
181200000519  "  X***  *IN21         DOWEQ     *OFF
181300000519  "  X***                MOVEL     TXTTES        RIGA
181400000519  "  X***  TXTRIB        IFEQ      *ON
181500000519  "   *
181600000519  "   * SetGt su ANTXT01L con l'unità trovata nel record avente
181700000519  "   * tipo riga = 'A'
181800000519  "  C                   eval      XEsito = *Off
181900000519  "  C                   eval      XOpe = '5'
182000000519  "  C                   eval      XSocieta = WWWSoc
182100000523  "  C                   eval      XUnita = *blank
182200000519  "  C                   eval      XTpTesto = D1TpT
182300000519  "  C                   eval      XLingua = XScLin
182400000523  "  C                   eval      XClaUte = *blanks
182500000519  "  C                   eval      XPosRig = 'A'
182600000519  "  C                   eval      XNrRiga = *zeros
182700000519  "  C                   eval      XRibat = *blanks
182800000519  "  C                   eval      XTesRig = *blanks
182900000519  "  C                   eval      XNrChiavi = 5
183000000519  "  C                   eval      XUsoUnita = '0'
183100000519  "   *
183200000519  "  C                   exsr      CallX72
183300000519  "   *
183400000519  "  C                   dow       XEsito = *Off
183500000519  "   *
183600000519  "   * Reade su ANTXT01L per trovare i record successivi a quello con
183700000519  "   * PosRig = 'A'
183800000519  "  C                   eval      XEsito = *Off
183900000519  "  C                   eval      XOpe = '4'
184000000523  "  C                   eval      XClaUte = *blank
184100000523  "  C                   eval      XPosRig = *blank
184200000519  "  C                   eval      XNrRiga = *zeros
184300000519  "  C                   eval      XRibat = *blanks
184400000519  "  C                   eval      XTesRig = *blanks
184500000519  "  C                   eval      XNrChiavi = 4
184600000519  "  C                   eval      XUsoUnita = '0'
184700000519  "   *
184800000519  "  C                   exsr      CallX72
184900000519  "   *
185000000519  "  C                   if        XEsito = *Off
185100000519  "  C                   movel     XTesRig       Riga
185200000519A0778C     XRibat        ifeq      *on
185300951003     C                   SETON                                        80
185400951003     C                   ELSE
185500951003     C                   SETOFF                                       80
185600951003     C                   ENDIF
185700951003     C                   WRITE     TXTREV
185800000519A0778X***  K4TXT1        READE     ANTXT01L                               21
185900000519A0778C                   endif
186000951003     C                   ENDDO
186100951003     C*
186200000518B0212C                   Close     ANA312P
186300000518     C*
186400951003     C                   ENDSR
186500951003     C/EJECT
186600081224D2461C************************************************************
186700081224     C* DATI FORNITORE
186800081224D2461C************************************************************
186900081224     C     DATFOR        BEGSR
187000081224     C*
187100081224     C                   setoff                                       85
187200081224     C                   clear                   ragsoc
187300081224     C                   clear                   indiri
187400081224     C                   clear                   CapLocPrv
187500081224     C                   clear                   WCap
187600081224     C                   clear                   WLocali
187700081224     C                   clear                   WProv
187800081224     C                   clear                   dvfpartiva
187900081224     C                   clear                   dvfkcc
188000081224     C                   clear                   dvfksc
188100081224     C*
188200081224     C                   move      '1'           DVARIC
188300081224     C                   move      xscsoc        DVASOCIETA
188400081224     C                   move      *blank        DVASTRUTT
188500081224     C                   movel     'DVAFOR'      DVASTRUTT
188600081224     C                   eval      DVALENOUT = %Size(DVAFOR)
188700081224     C                   move      dicksc        DVACODICE
188800081224     C                   move      'F'           DVASNATURA
188900081224     C                   clear                   DVALINEAV
189000081224     C                   clear                   DVAFILIALE
189100081224     C                   clear                   DVATPIND
189200081224     C                   clear                   DVACDIND
189300081224     C*
189400081224     C                   CALLB     'NDDVASOG'    PDVASOG
189500081224     C*
189600081224   1bC     DVAERRORE     ifeq      *off
189700081224     C                   eval      %subst(DVAFOR:1:DVALENOUT)
189800081224     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
189900081224     C*
190000081224   2bC                   if        dvfkcca <> *blank and dvfksca <> *blank
190100081224     C* Percipiente
190200081224     C                   clear                   WCognNome
190300081224   3bC                   if        DvfCognome <> *blank
190400081224     C                   seton                                        85
190500081224     c*
190600081224     c                   movel     DvfCognome    WCognNome
190700081224     c     WCognNome     Cat       DvfNome:1     WCognNome
190800081224     c                   movel     WCognNome     RagSoc
190900081224     c*
191000081224   3xC                   else
191100081224     C                   Clear                   RagSoc
191200081224     C                   movel     dvfdes        ragsoc
191300081224   3eC                   endif
191400081224     C*
191500081224     C* Indirizzo percipiente diverso
191600081224   3bC                   if        dvftpinda<>*blank and dvfcdinda<>*blank
191700081224     C                   move      '1'           DVARIC
191800081224     C                   move      xscsoc        DVASOCIETA
191900081224     C                   move      *blank        DVASTRUTT
192000081224     C                   movel     'ANIND'       DVASTRUTT
192100081224     C                   eval      DVALENOUT = %Size(ANIND)
192200081224     C                   move      dicksc        DVACODICE
192300081224     C                   move      'F'           DVASNATURA
192400081224     C                   clear                   DVALINEAV
192500081224     C                   clear                   DVAFILIALE
192600081224     C                   move      dvftpinda     DVATPIND
192700081224     C                   move      dvfcdinda     DVACDIND
192800081224     C                   CALLB     'NDDVASOG'    PDVASOG
192900081224   4bC     DVAERRORE     ifeq      *off
193000081224     C                   eval      %subst(ANIND:1:DVALENOUT)
193100081224     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
193200081224     C                   movel     $ndindriz     indiri
193300081224     C                   movel     $ndCap        WCap
193400081224     C                   movel     $ndLocalit    WLocali
193500081224     C                   movel     $ndProv       WProv
193600081224   4eC                   endif
193700081224   3xC                   else
193800081224     C                   movel     dvfindriz     indiri
193900081224     C                   movel     dvfcap        WCap
194000081224     C                   movel     dvflocalit    WLocali
194100081224     C                   movel     dvfprov       WProv
194200081224   3eC                   endif
194300081224     C*
194400081224   2xC                   else
194500081224     C* Solo fornitore
194600081224     c                   Clear                   RagSoc
194700081224     C                   movel     dvfdes        ragsoc
194800081224     C                   movel     dvfindriz     indiri
194900081224     C                   movel     dvfcap        WCap
195000081224     C                   movel     dvflocalit    WLocali
195100081224     C                   movel     dvfprov       WProv
195200081224   2eC                   endif
195300081224     C*
195400081224     c                   clear                   WCap5             5
195500081224     c                   movel     WCap          WCap5
195600081224   2bC                   if        WCap5 <> *Blank and WCap5 <> *All'0'
195700081224     c                   movel     WCap          CapLocPrv
195800081224     c     CapLocPrv     Cat       WLocali:1     CapLocPrv
195900081224   2xc                   else
196000081224     c                   movel     WLocali       CapLocPrv
196100081224   2ec                   endif
196200081224   2bC                   if        CapLocPrv <> *Blank
196300081224     c     CapLocPrv     Cat       WProv:1       CapLocPrv
196400081224   2xc                   else
196500081224     c                   movel     WProv         CapLocPrv
196600081224   2ec                   endif
196700081224     C*
196800081224   1eC                   endif
196900081224     C*
197000081224     C*
197100081224D2461C                   ENDSR
197200000519A0778C************************************************************
197300000519  "  C* Chiamata al driver di lettura ANTXT00F
197400000519  "  C************************************************************
197500000519A0778C     CallX72       begsr
197600000519      *
197700000519     C                   call      'X72TXT'
197800000519     C                   parm                    XEsito
197900000519     C                   parm                    XOpe
198000000519     C                   parm                    XSocieta
198100000519     C                   parm                    XTpTesto
198200000519     C                   parm                    XUnita
198300000519     C                   parm                    XLingua
198400000523     C                   parm                    XClaUte
198500000519     C                   parm                    XPosRig
198600000519     C                   parm                    XNrRiga
198700000519     C                   parm                    XRibat
198800000519     C                   parm                    XTesRig
198900000519     C                   parm                    XNrChiavi
199000000519     C                   parm                    XUsoUnita
199100000519      *
199200000519A0778C                   endsr
199300941205     C************************************************************
199400941205     C* CONTROLLO ABILITAZIONE TASTI
199500941205     C************************************************************
199600941205     C     CTRKEY        BEGSR
199700941205     C*
199800941205     C                   SELECT
199900941205     C     $TASTO        WHENEQ    F01
200000941205     C     $FC(1)        IFEQ      '0'
200100941205     C                   SETON                                        99
200200941205     C                   ENDIF
200300941205     C     $TASTO        WHENEQ    F02
200400941205     C     $FC(2)        IFEQ      '0'
200500941205     C                   SETON                                        99
200600941205     C                   ENDIF
200700941205     C     $TASTO        WHENEQ    F03
200800941205     C     $FC(3)        IFEQ      '0'
200900941205     C                   SETON                                        99
201000941205     C                   ENDIF
201100941205     C     $TASTO        WHENEQ    F04
201200941205     C     $FC(4)        IFEQ      '0'
201300941205     C                   SETON                                        99
201400941205     C                   ENDIF
201500941205     C     $TASTO        WHENEQ    F05
201600941205     C     $FC(5)        IFEQ      '0'
201700941205     C                   SETON                                        99
201800941205     C                   ENDIF
201900941205     C     $TASTO        WHENEQ    F06
202000941205     C     $FC(6)        IFEQ      '0'
202100941205     C                   SETON                                        99
202200941205     C                   ENDIF
202300941205     C     $TASTO        WHENEQ    F07
202400941205     C     $FC(7)        IFEQ      '0'
202500941205     C                   SETON                                        99
202600941205     C                   ENDIF
202700941205     C     $TASTO        WHENEQ    F08
202800941205     C     $FC(8)        IFEQ      '0'
202900941205     C                   SETON                                        99
203000941205     C                   ENDIF
203100941205     C     $TASTO        WHENEQ    F09
203200941205     C     $FC(09)       IFEQ      '0'
203300941205     C                   SETON                                        99
203400941205     C                   ENDIF
203500941205     C     $TASTO        WHENEQ    F10
203600941205     C     $FC(10)       IFEQ      '0'
203700941205     C                   SETON                                        99
203800941205     C                   ENDIF
203900941205     C     $TASTO        WHENEQ    F11
204000941205     C     $FC(11)       IFEQ      '0'
204100941205     C                   SETON                                        99
204200941205     C                   ENDIF
204300941205     C     $TASTO        WHENEQ    F12
204400941205     C     $FC(12)       IFEQ      '0'
204500941205     C                   SETON                                        99
204600941205     C                   ENDIF
204700941205     C     $TASTO        WHENEQ    F13
204800941205     C     $FC(13)       IFEQ      '0'
204900941205     C                   SETON                                        99
205000941205     C                   ENDIF
205100941205     C     $TASTO        WHENEQ    F14
205200941205     C     $FC(14)       IFEQ      '0'
205300941205     C                   SETON                                        99
205400941205     C                   ENDIF
205500941205     C     $TASTO        WHENEQ    F15
205600941205     C     $FC(15)       IFEQ      '0'
205700941205     C                   SETON                                        99
205800941205     C                   ENDIF
205900941205     C     $TASTO        WHENEQ    F16
206000941205     C     $FC(16)       IFEQ      '0'
206100941205     C                   SETON                                        99
206200941205     C                   ENDIF
206300941205     C     $TASTO        WHENEQ    F17
206400941205     C     $FC(17)       IFEQ      '0'
206500941205     C                   SETON                                        99
206600941205     C                   ENDIF
206700941205     C     $TASTO        WHENEQ    F18
206800941205     C     $FC(18)       IFEQ      '0'
206900941205     C                   SETON                                        99
207000941205     C                   ENDIF
207100941205     C     $TASTO        WHENEQ    F19
207200941205     C     $FC(19)       IFEQ      '0'
207300941205     C                   SETON                                        99
207400941205     C                   ENDIF
207500941205     C     $TASTO        WHENEQ    F20
207600941205     C     $FC(20)       IFEQ      '0'
207700941205     C                   SETON                                        99
207800941205     C                   ENDIF
207900941205     C     $TASTO        WHENEQ    F21
208000941205     C     $FC(21)       IFEQ      '0'
208100941205     C                   SETON                                        99
208200941205     C                   ENDIF
208300941205     C     $TASTO        WHENEQ    F22
208400941205     C     $FC(22)       IFEQ      '0'
208500941205     C                   SETON                                        99
208600941205     C                   ENDIF
208700941205     C     $TASTO        WHENEQ    F23
208800941205     C     $FC(23)       IFEQ      '0'
208900941205     C                   SETON                                        99
209000941205     C                   ENDIF
209100941205     C     $TASTO        WHENEQ    F24
209200941205     C     $FC(24)       IFEQ      '0'
209300941205     C                   SETON                                        99
209400941205     C                   ENDIF
209500941205     C                   ENDSL
209600941205     C*
209700941205     C                   ENDSR
209800940318     C/EJECT
209900940318     C******************************************************
210000940322     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
210100940318     C******************************************************
210200940318     C     MSGPGM        BEGSR
210300940318     C*
210400940318     C                   Z-ADD     2             MSGMSG
210500940318     C                   MOVEL     'S'           MSGRSP
210600940318     C                   MOVEL     'CR'          MSGTPR
210700940318     C                   MOVEL     'S'           MSGEMV
210800940318     C                   Z-ADD     10            MSGRGP
210900940318     C                   MOVEL     'S'           MSGLCK
211000940318     C                   MOVEA     STATUS        STS
211100940318     C                   MOVEL     'N'           MSGCNL
211200940318     C                   MOVEL     'N'           MSGVID
211300940318     C                   MOVEL     'N'           MSGOPE
211400940318     C                   MOVEL     'N'           MSGSTP
211500960416     C                   CALL      'XMSGR'
211600940318     C                   PARM                    XMSGDS
211700940318     C*
211800940318     C                   ENDSR
211900940207     C/EJECT
212000940131     C************************************************************
212100940131     C* OPERAZIONI INIZIALI
212200940131     C************************************************************
212300940131     C     *INZSR        BEGSR
212400000114A0735C     *DMY          Move      Udate         ZDateISO
212500000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
212600940131     C*
212700940127     C* Reperimento parametri
212800940127     C*
212900940117     C     *ENTRY        PLIST
213000940117     C                   PARM                    KPJBA
213100950104     C                   PARM                    DSAUT
213200950104     C*
213300950104     C     PARMS         IFLT      2
213400950104     C                   MOVE      *BLANK        AUTDS
213500950104     C                   ELSE
213600950104     C                   MOVEL     DSAUT         AUTDS
213700950104     C                   ENDIF
213800940506     C*
213900940506     C* Reperimento tasti di funzione
214000940506     C*
214100940506     C                   EXSR      REPKEY
214200940506     C*
214300960416     C* STRCMTCTL per numeratore
214400960416     C                   CALLb     'XSTRCMT'
214500960416     C*
214600940223     C                   ENDSR
214700940223     C/EJECT
214800940506     C************************************************************
214900940506     C* REPERIMENTO TASTI DI FUNZIONE
215000940506     C************************************************************
215100940506     C     REPKEY        BEGSR
215200940506     C                   MOVEA     $FM1          $FM
215300950906     C                   MOVEA     $FC1          $FC
215400940506     C                   EXSR      REPKKK
215500940506     C                   MOVEA     $FX           $FX1
215600940614     C*
215700940614     C                   ENDSR
215800940506     C/EJECT
215900940506     C************************************************************
216000940506     C* REPERIMENTO TASTI DI FUNZIONE
216100940506     C************************************************************
216200940506     C     REPKKK        BEGSR
216300940506     C*
216400940506     C                   CLEAR                   $FX
216500940506     C     1             DO        24            X
216600940506     C*
216700940506     C     $FM(X)        IFNE      *BLANK
216800950906     C     $FC(X)        ANDNE     '0'
216900940506     C*
217000940506     C                   MOVEL     $FM(X)        IDMSG
217100960416     C                   CALLb     'XRTVM'
217200940506     C                   PARM                    IDMSG            27
217300940506     C                   PARM                    TXTMSG          100
217400940506     C                   PARM                    ERRMSG            1
217500940506     C*
217600940506     C     ERRMSG        IFNE      *ON
217700940506     C                   MOVEL     TXTMSG        $FX(X)
217800940506     C                   ELSE
217900940506     C                   MOVEL     *ALL' '       $FX(X)
218000941027     C                   ENDIF
218100940506     C*
218200941027     C                   ENDIF
218300940506     C*
218400941027     C                   ENDDO
218500940506     C*
218600940506     C                   ENDSR
218700940509     C/EJECT
218800940223     C************************************************************
218900940223     C* INIZIALIZZAZIONE VARIABILI
219000940223     C************************************************************
219100940223     C     INZVAR        BEGSR
219200950112     C*
219300950112     C* Dati da PGM guida
219400950112     C*
219500950112     C                   MOVEL     KPJBU         A312DS
219600941201     C*
219700941201     C* Reperimento dati società
219800941201     C*
219900941115     C                   MOVEL     'SOC001'      TIPXSC
220000941115     C                   MOVEL     *BLANK        SOCXSC
220100941115     C                   EXSR      REPSOC
220200941115     C     RTNXSC        IFNE      '1'
220300941115     C                   MOVEL     XSOCDS        SOC001
220400941115     C                   ELSE
220500941115     C                   RETURN
220600941115     C                   ENDIF
220700950110     C*
220800950110     C* gestione società di riferimento
220900950110     C     FSC312        IFEQ      'S'
221000950111     C     OPZ312        OREQ      *BLANK
221100950110     C                   MOVE      XSCSOC        WWWSOC
221200950110     C* gestione simulata da capofila
221300950110     C                   ELSE
221400950110     C                   MOVE      XSCCPA        WWWSOC
221500950110     C                   ENDIF
221600950104     C*
221700950104     C* le autorizzazioni potrebbero essere già state passate,
221800950104     C* nel qual caso non vanno reperite
221900950104     C     AUTDS         IFEQ      *BLANK
222000950104     C                   EXSR      REPAUT
222100950104     C                   ELSE
222200970703     c                   if        CoF312 = 'C'
222300950104     C                   MOVEL     XCADDS        GCLIDS
222400970703     c                   endif
222500970703     c                   if        CoF312 = 'F'
222600970725     C                   MOVEL     XCADDS        GFORDS
222700970703     c                   endif
222800950104     C                   ENDIF
222900950104     C*
223000950104     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
223100950104     C* le autorizzazione
223200950104     C*
223300950104     C                   EXSR      TSTAUT
223400940715     C*
223500050801B0822X* Definizione dei tasti funzione utilizzabili a pgm
223600050801  "  X* SPOSTATI DOPO IL CHAIN (per vedere se dichiaraz. revocata)
223700050801B0822X*****              EXSR      DEFOPZ
223800940128     C*
223900940223     C* Variabili per gestione videate
224000940128     C*
224100940223     C                   MOVE      'D1'          $GEST
224200940223     C                   MOVE      *OFF          $FINE
224300940223     C                   MOVE      *ON           $INZD1
224400940224     C                   MOVE      *BLANK        $LASTG
224500940224     C                   MOVE      *BLANK        $LASTV
224600940506     C                   Z-ADD     0             $ULKD1            3 0
224700940127     C*
224800940223     C* Variabili appoggio
224900940127     C*
225000940224     C                   Z-ADD     0             CURR
225100940224     C                   Z-ADD     0             CURC
225200941222     C                   MOVEL     $DT0          DTCAMG
225300941222     C                   MOVEL     $DT0          DINAMG
225400941222     C                   MOVEL     $DT0          DTIAMG
225500941222     C                   MOVEL     $DT0          DTFAMG
225600950915     C                   MOVEL     $DT0          DTRAMG
225700951006     C                   MOVEL     $DT0          DITSAV
225800960416     C                   CALLb     'XDT4000'
225900950206     C                   PARM                    XDTAMG           10
226000000114A0735C*****              PARM      UDATE         XDTGMA            6 0
226100000114A0735C                   PARM      ZDate         XDTGMA            6 0
226200950206     C                   PARM      4             XDTSTA            1 0
226300950206     C                   MOVEL     XDTAMG        WDATE
226400940207     C*
226500940207     C* Valorizzazione campi univoci testate
226600940207     C*
226700940207     C                   CLEAR                   FMTT1
226800940207     C                   MOVEL     KNSIF         NOMSIF
226900941110     C                   MOVEL     XSCDSI        NOMDIT
227000941027     C                   MOVEL     DSPGM         NOMPGM
227100950427     C* recupero il sottotitolo (se c'è)
227200950427     C                   MOVEL     *BLANK        T1TIT
227300950427     C     SOTTIT        IFNE      *BLANK
227400950915     C                   MOVEL     SOTTIT        WWSTIT           27
227500950915     C     COF312        IFEQ      'C'
227600950915     C                   MOVE      '0030'        WWSTIT
227700950915     C                   ELSE
227800950915     C     COF312        IFEQ      'F'
227900950915     C                   MOVE      '0140'        WWSTIT
228000950915     C                   ENDIF
228100950915     C                   ENDIF
228200950915     C                   MOVEL     WWSTIT        IDMSG
228300960416     C                   CALLb     'XRTVM'
228400950427     C                   PARM                    IDMSG            27
228500950427     C                   PARM                    TXTMSG          100
228600950427     C                   PARM                    ERRMSG            1
228700950427     C                   PARM      *ON           CTRMSG            1
228800950427     C                   PARM      30            LENMSG            3 0
228900950427     C     ERRMSG        IFNE      *ON
229000950427     C                   MOVEL     TXTMSG        T1TIT
229100950427     C                   ELSE
229200950427     C                   MOVEL     *ALL'?'       T1TIT
229300950427     C                   ENDIF
229400950427     C                   ENDIF
229500950427     C* recupero il tipo operazione
229600941216     C                   Z-ADD     1             X
229700941221     C     OPZ312        LOOKUP    $OZ(X)                                 21
229800941216     C     *IN21         IFEQ      *ON
229900950427     C                   MOVEL     $ID(X)        IDMSG
230000960416     C                   CALLb     'XRTVM'
230100950427     C                   PARM                    IDMSG            27
230200950427     C                   PARM                    TXTMSG          100
230300950427     C                   PARM                    ERRMSG            1
230400950427     C                   PARM      *ON           CTRMSG            1
230500950427     C                   PARM      30            LENMSG            3 0
230600950427     C     ERRMSG        IFNE      *ON
230700950427     C                   MOVEL     TXTMSG        T1OPE
230800950427     C                   ELSE
230900950427     C                   MOVEL     *ALL'?'       T1OPE
231000950427     C                   ENDIF
231100950427     C                   ENDIF
231200940224     C*
231300940224     C* Inizializzazione/Decodifica prima videata
231400940224     C*
231500940224     C                   EXSR      INZD1
231600940224     C                   MOVE      *OFF          $INZD1
231700940127     C*
231800940117     C                   ENDSR
231900950104     C/EJECT
232000950104     C************************************************************
232100950104     C* REPERIMENTO AUTORIZZAZIONI UTENTE
232200950104     C************************************************************
232300950104     C     REPAUT        BEGSR
232400950104     C*
232500950104     C                   CLEAR                   AUTDS
232600950104     C*
232700950123     C                   MOVEL     XSCSOC        XCASOC
232800950104     C                   MOVEL     XSCDUN        XCAUNI
232900950104     C                   Z-ADD     0             XCAGRP
233000950104     C                   MOVEL     KNMUS         XCAPRF
233100970703     c                   if        CoF312 = 'C'
233200970703     C                   MOVEL     'ANGCLI'      XCAFNC
233300970703     c                   endif
233400970703     c                   if        CoF312 = 'F'
233500970703     C                   MOVEL     'ANGFOR'      XCAFNC
233600970703     c                   endif
233700950104     C                   MOVEL     *ZERO         XCAVFU
233800950104     C                   MOVE      'CK'          XCATCK
233900950104     C                   MOVE      '1'           XCANAU
234000950104     C*
234100961004     C                   CALLb     'XCHKAUT'
234200950104     C                   PARM                    AUTDS
234300950104     C*
234400950104     C* errori/non abilitazione
234500950104     C     XCARTN        IFNE      0
234600980818     C                   MOVE      '2'           Err312
234700950227     C                   MOVE      *ON           $FINE
234800950104     C                   EXSR      ENDPGM
234900950104     C                   ELSE
235000970703     c                   if        CoF312 = 'C'
235100970703     C                   MOVEL     XCADDS        GCliDs
235200970703     c                   endif
235300970703     c                   if        CoF312 = 'F'
235400970703     C                   MOVEL     XCADDS        GForDs
235500970703     c                   endif
235600950104     C                   ENDIF
235700950104     C*
235800950104     C                   ENDSR
235900950104     C/EJECT
236000950104     C************************************************************
236100950104     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
236200950104     C************************************************************
236300950104     C     TSTAUT        BEGSR
236400950104     C*
236500970703     c* se cliente
236600970703     c                   if        CoF312 = 'C'
236700950104     C                   SELECT
236800950104     C*
236900950104     C* se richiamato in immissione
237000950104     C     OPZ312        WHENEQ    '01'
237100950104     C* ma l'utente non è abilitato
237200950104     C     IMMCLI        ANDEQ     *OFF
237300950104     C* esco da pgm avvertendolo
237400950104     C                   MOVE      '2'           ERR312
237500950104     C                   MOVE      *ON           $FINE
237600950104     C                   EXSR      ENDPGM
237700950104     C*
237800950104     C* se richiamato in modifica
237900950104     C     OPZ312        WHENEQ    '02'
238000950104     C* ma l'utente non è abilitato
238100950104     C     MANCLI        ANDEQ     *OFF
238200950104     C* testo se l'utente è abilitato almeno alla visualizzazione
238300950104     C     VISCLI        IFEQ      *ON
238400950104     C                   MOVE      '05'          OPZ312
238500950104     C                   ELSE
238600950104     C* altrimenti esco da pgm avvertendolo
238700950104     C                   MOVE      '2'           ERR312
238800950104     C                   MOVE      *ON           $FINE
238900950104     C                   EXSR      ENDPGM
239000950104     C                   ENDIF
239100950104     C*
239200950104     C* se richiamato in copia
239300950104     C     OPZ312        WHENEQ    '03'
239400950104     C* ma l'utente non è abilitato
239500950104     C     IMMCLI        ANDEQ     *OFF
239600950104     C* esco da pgm avvertendolo
239700950104     C                   MOVE      '2'           ERR312
239800950104     C                   MOVE      *ON           $FINE
239900950104     C                   EXSR      ENDPGM
240000950104     C*
240100950104     C* se richiamato in annullamento
240200950104     C     OPZ312        WHENEQ    '04'
240300950104     C* ma l'utente non è abilitato
240400950104     C     ANNCLI        ANDEQ     *OFF
240500950104     C* esco da pgm avvertendolo
240600950104     C                   MOVE      '2'           ERR312
240700950104     C                   MOVE      *ON           $FINE
240800950104     C                   EXSR      ENDPGM
240900950104     C*
241000950104     C* se richiamato in visualizzazione
241100950104     C     OPZ312        WHENEQ    '05'
241200950104     C* ma l'utente non è abilitato
241300950104     C     VISCLI        ANDEQ     *OFF
241400950104     C* esco da pgm avvertendolo
241500950104     C                   MOVE      '2'           ERR312
241600950104     C                   MOVE      *ON           $FINE
241700950104     C                   EXSR      ENDPGM
241800950104     C*
241900950104     C                   ENDSL
242000970703     c                   endif
242100970703     C*
242200970703     c* se fornitore
242300970703     c                   if        CoF312 = 'F'
242400970703     C                   SELECT
242500970703     C*
242600970703     C* se richiamato in immissione
242700970703     C     OPZ312        WHENEQ    '01'
242800970703     C* ma l'utente non è abilitato
242900970703     C     ImmFor        ANDEQ     *OFF
243000970703     C* esco da pgm avvertendolo
243100970703     C                   MOVE      '2'           ERR312
243200970703     C                   MOVE      *ON           $FINE
243300970703     C                   EXSR      ENDPGM
243400970703     C*
243500970703     C* se richiamato in modifica
243600970703     C     OPZ312        WHENEQ    '02'
243700970703     C* ma l'utente non è abilitato
243800970703     C     ManFor        ANDEQ     *OFF
243900970703     C* testo se l'utente è abilitato almeno alla visualizzazione
244000970703     C     VisFor        IFEQ      *ON
244100970703     C                   MOVE      '05'          OPZ312
244200970703     C                   ELSE
244300970703     C* altrimenti esco da pgm avvertendolo
244400970703     C                   MOVE      '2'           ERR312
244500970703     C                   MOVE      *ON           $FINE
244600970703     C                   EXSR      ENDPGM
244700970703     C                   ENDIF
244800970703     C*
244900970703     C* se richiamato in copia
245000970703     C     OPZ312        WHENEQ    '03'
245100970703     C* ma l'utente non è abilitato
245200970703     C     ImmFor        ANDEQ     *OFF
245300970703     C* esco da pgm avvertendolo
245400970703     C                   MOVE      '2'           ERR312
245500970703     C                   MOVE      *ON           $FINE
245600970703     C                   EXSR      ENDPGM
245700970703     C*
245800970703     C* se richiamato in annullamento
245900970703     C     OPZ312        WHENEQ    '04'
246000970703     C* ma l'utente non è abilitato
246100970703     C     AnnFor        ANDEQ     *OFF
246200970703     C* esco da pgm avvertendolo
246300970703     C                   MOVE      '2'           ERR312
246400970703     C                   MOVE      *ON           $FINE
246500970703     C                   EXSR      ENDPGM
246600970703     C*
246700970703     C* se richiamato in visualizzazione
246800970703     C     OPZ312        WHENEQ    '05'
246900970703     C* ma l'utente non è abilitato
247000970703     C     VisFor        ANDEQ     *OFF
247100970703     C* esco da pgm avvertendolo
247200970703     C                   MOVE      '2'           ERR312
247300970703     C                   MOVE      *ON           $FINE
247400970703     C                   EXSR      ENDPGM
247500970703     C*
247600970703     C                   ENDSL
247700970703     c                   endif
247800970703     C*
247900950104     C                   ENDSR
248000941209     C/EJECT
248100941209     C************************************************************
248200941209     C* REPERIMENTO DATI SOCIETA'
248300941209     C************************************************************
248400941209     C     REPSOC        BEGSR
248500941209     C*
248600960416     C                   CALLb     'XSOC'
248700941209     C                   PARM                    TIPXSC            6
248800941209     C                   PARM                    SOCXSC            3
248900941209     C                   PARM                    CDSXSC            9 0
249000941209     C                   PARM                    MODXSC            3
249100941209     C                   PARM      *BLANKS       RTNXSC            1
249200941209     C                   PARM                    XSOCDS
249300941209     C                   PARM                    KPJBA
249400941209     C*
249500941209     C                   ENDSR
249600940715     C/EJECT
249700940715     C************************************************************
249800940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
249900940715     C************************************************************
250000940715     C     DEFOPZ        BEGSR
250100940715     C*
250200940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
250300940715     C* con S o N a seconda della utilizzabilità o meno
250400940715     C* del tasto funzione correlato
250500940715     C*
250600940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
250700940715     C* di compilazione
250800940715     C                   RESET                   $FC1
250900940715     C                   RESET                   $FR1
251000940715     C*
251100940715     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
251200940715     C* invalido alcune opzioni
251300940715     C                   SELECT
251400940715     C* Se pgm richiamato in interrogazione disattivo :
251500941221     C     OPZ312        WHENEQ    '05'
251600940715     C* F4=Ricerca
251700940715     C                   Z-ADD     4             X
251800940720     C                   MOVE      '0'           $FC1(X)
251900940715     C* F5=Ripristino
252000940715     C                   Z-ADD     5             X
252100940720     C                   MOVE      '0'           $FC1(X)
252200940715     C* F6=Conferma
252300940715     C                   Z-ADD     06            X
252400940720     C                   MOVE      '0'           $FC1(X)
252500050801B0822C* F20=Annulla revoca
252600050801  "  C                   Z-ADD     20            X
252700050801B0822C                   MOVE      '0'           $FC1(X)
252800940715     C* Se pgm richiamato in annullamento disattivo :
252900941221     C     OPZ312        WHENEQ    '04'
253000940715     C* F4=Ricerca
253100940715     C                   Z-ADD     4             X
253200940720     C                   MOVE      '0'           $FC1(X)
253300940715     C* F5=Ripristino
253400940715     C                   Z-ADD     5             X
253500940720     C                   MOVE      '0'           $FC1(X)
253600050801B0822C* F20=Annulla revoca
253700050801  "  C                   Z-ADD     20            X
253800050802  "  C                   MOVE      '0'           $FC1(X)
253900050802  "  C* Se pgm richiamato in copia disattivo :
254000050802  "  C     OPZ312        WHENEQ    '03'
254100050802  "  C* F20=Annulla revoca
254200050802  "  C                   Z-ADD     20            X
254300050802B0822C                   MOVE      '0'           $FC1(X)
254400940715     C*
254500940715     C                   OTHER
254600940715     C*
254700940715     C                   ENDSL
254800940715     C*
254900940715     C* Disattivo a seconda delle autorizzazioni :
255000940715     C*********  AUTXXX    IFEQ 'N'
255100940715     C*********            Z-ADD1         X
255200940715     C*********  'XX'      LOKUP$OP1,X                   21
255300940715     C*********  *IN21     IFEQ *ON
255400940715     C*********            MOVE 'N'       $FL1,X
255500940715     C*********            ENDIF
255600940715     C*********            ENDIF
255700050801     C*
255800050801B0822c* se la dichiarazione NON è revocata, disattivo F20
255900050801  "  c* e lo disattivo sempre per i clienti, in quanto c'è solo la
256000050801  "  c* data che si può pulire a video
256100050801  "  C                   IF        DicRev     <>*on
256200050801  "  c                             or  cof312='C'
256300050801  "  C                   Z-ADD     20            X
256400050801  "  C                   MOVE      '0'           $FC1(X)
256500050801B0822C                   ENDIF
256600050801     C*
256700940715     C*
256800940715     C                   ENDSR
256900940128     C************************************************************
257000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
257100940223     C************************************************************
257200940223     C     DEFCAM        BEGSR
257300940223     C*
257400940223     C* klist
257500951004     C*
257600970725     C* ANDIC01L
257700970725     C     K5DIC1        KLIST
257800970725     C                   KFLD                    DICSOC
257900970725     C                   KFLD                    DICCF
258000970725     C                   KFLD                    DICKSC
258100970725     C                   KFLD                    DICNRC
258200970725     C                   KFLD                    DICDTC
258300951004     C* ANDIC02L
258400970725     C     K4DIC2        KLIST
258500951004     C                   KFLD                    DICSOC
258600951004     C                   KFLD                    DICCF
258700951004     C                   KFLD                    DICDIN
258800951004     C                   KFLD                    DICNIN
258900141217R160 C* SdgDif01l
259000141217 "   C     K04Dif01      KLIST
259100141217 "   C                   KFLD                    DifSocieta
259200141217 "   C                   KFLD                    DifCf
259300141217 "   C                   KFLD                    DifDtInter
259400141217R160 C                   KFLD                    DifNrInter
259500081224D2461C* PLIST per NDVASOG
259600081224  "  C     PDVASOG       PLIST
259700081224  "  C                   PARM                    DVARIC
259800081224  "  C                   PARM                    DVASOCIETA
259900081224  "  C                   PARM                    DVAUNITA
260000081224  "  C                   PARM                    DVASTRUTT
260100081224  "  C                   PARM                    DVADTRIF
260200081224  "  C                   PARM                    DVALENOUT
260300081224  "  C                   PARM                    DVASNATURA
260400081224  "  C                   PARM                    DVACODICE
260500081224  "  C                   PARM                    DVALINEAV
260600081224  "  C                   PARM                    DVAFILIALE
260700081224  "  C                   PARM                    *omit
260800081224  "  C                   PARM                    DVATPIND
260900081224  "  C                   PARM                    DVACDIND
261000081224  "  C                   PARM                    DVAERRORE
261100081224D2461C                   PARM                    DVAOUTPUT
261200950914     C*
261300000519A0778X* ANTXT01L
261400000519  "  X***  K5TXT1        KLIST
261500000519  "  X***                KFLD                    TXTSOC
261600000519  "  X***                KFLD                    TXTUNI
261700000519  "  X***                KFLD                    TXTTPT
261800000519  "  X***                KFLD                    TXTLIN
261900000519  "  X***                KFLD                    TXTPOS
262000000519  "  X*
262100000519  "  X***  K4TXT1        KLIST
262200000519  "  X***                KFLD                    TXTSOC
262300000519  "  X***                KFLD                    TXTUNI
262400000519  "  X***                KFLD                    TXTTPT
262500000519A0778X***                KFLD                    TXTLIN
262600951012     C*
262700951012     C* PLIST per XNUM
262800951012     C     PNMR          PLIST
262900951012     C                   PARM                    XNMRIC            1
263000951012     C                   PARM                    XNMSOC            3
263100951012     C                   PARM                    XNMUNI            8
263200951012     C                   PARM                    XNMCTB            2
263300951012     C                   PARM                    XNMKEY            8
263400951012     C                   PARM                    XNMSER            4
263500951012     C                   PARM                    XNMCMT            1
263600951012     C                   PARM                    XNMDAT           10
263700951012     C                   PARM                    XNMIVA            1
263800951012     C                   PARM                    XNMREG            1
263900951012     C                   PARM                    XNMERR            1
264000951012     C                   PARM                    XNMNUM            9 0
264100951012     C                   PARM                    XNMAUT            1
264200030704B0581C                   PARM                    XNMDAG           10
264300940223     C*
264400940223     C* Variabili per gestione videate
264500940223     C*
264600940223     C                   MOVE      *BLANK        $GEST             2
264700940223     C                   MOVE      *BLANK        $FINE             1
264800940223     C                   MOVE      *BLANK        $INZD1            1
264900940223     C                   MOVE      *BLANK        $LASTG            2
265000940223     C                   MOVE      *BLANK        $LASTV            2
265100940225     C                   Z-ADD     0             $PIED1            3 0
265200940223     C*
265300940223     C* Variabili appoggio
265400940223     C*
265500950206     C                   MOVE      *BLANK        WWIVA            13
265600951002     C                   MOVE      *BLANK        WWOPI             4
265700950110     C     *LIKE         DEFINE    XSCSOC        WWWSOC
265800940223     C                   Z-ADD     0             CURR              2 0
265900950206     C                   Z-ADD     0             CURC              2 0
266000940223     C                   MOVE      *ZEROS        WIN              99
266100940224     C                   MOVE      *BLANK        WTBERR            1
266200950206     C     *LIKE         DEFINE    X06DAT        WDATE
266300970210     C     *LIKE         DEFINE    D1Dtc         DtcSAV
266400970210     C     *LIKE         DEFINE    D1Nrc         NrcSAV
266500940223     C*
266600940223     C* Indici
266700940223     C*
266800940223     C                   Z-ADD     0             X                 3 0
266900940223     C                   Z-ADD     0             Y                 3 0
267000940127     C*
267100940117     C                   ENDSR
267200940509     C/EJECT
267300940509     C************************************************************
267400940509     C* RIEMPIMENTO TASTI FUNZIONE
267500940509     C************************************************************
267600940509     C     RIEKEY        BEGSR
267700940509     C*
267800940509     C* Input :
267900940509     C* - $FX = schiera con tutti i testi disponibili
268000940509     C* - $FC = schiere con i flag di validità associati ai testi
268100940509     C* - $FR = schiere con i flag di REVERSE IMAGE
268200940509     C* - $ULK = ultimo testo caricato
268300940722     C* - $L01 = Lunghezza prima riga
268400940722     C* - $L02 = Lunghezza seconda riga
268500940509     C* Output :
268600940509     C* - $ULK
268700940609     C* - $KEY1= 1° riga di tasti funzione
268800940609     C* - $KEY2= 2° riga di tasti funzione
268900940509     C*
269000960416     C                   CALLb     'XKEYMSG'
269100940509     C* input
269200940509     C                   PARM                    $FX
269300940509     C                   PARM                    $FC
269400940509     C                   PARM                    $FR
269500940509     C                   PARM                    $ULK              3 0
269600940509     C* output
269700940509     C                   PARM                    $KEY1            79
269800940509     C                   PARM                    $KEY2            79
269900940722     C* input
270000941116     C                   PARM      79            $L01              3 0
270100941116     C                   PARM      79            $L02              3 0
270200940509     C*
270300060224A1148C                   PARM                    $ALLFUNCT       480
270400060224A1148C*
270500940509     C                   ENDSR
270600950118     C*
270700050801B0822OANDIC02   E            AggDic
270800050801  "  O                       DicTpt
270900050801  "  O                       DicDtr
271000050801B0822O                       DicRev
271100950906**
27120095042701PROMSG    *LIBL     COS0001    immissione
27130095042702PROMSG    *LIBL     COS0002    modifica
27140095042703PROMSG    *LIBL     COS0003    copia
27150095042704PROMSG    *LIBL     COS0004    annullamento
27160095042705PROMSG    *LIBL     COS0005    visualizzazione
271700940509** TASTI DI FUNZIONE  D1
271800940509PROMSG    *LIBL     KEY0001  01
271900940509PROMSG    *LIBL     KEY0002  02
272000940509PROMSG    *LIBL     KEY0003  03
272100940509PROMSG    *LIBL     KEY0004  04
272200940509PROMSG    *LIBL     KEY0005  05
272300940509PROMSG    *LIBL     KEY0006  06
272400940509PROMSG    *LIBL     KEY0007  07
272500940509PROMSG    *LIBL     KEY0008  08
272600940509PROMSG    *LIBL     KEY0009  09
272700940509PROMSG    *LIBL     KEY0010  10
272800940509PROMSG    *LIBL     KEY0011  11
272900940509PROMSG    *LIBL     KEY0012  12
273000940509PROMSG    *LIBL     KEY0013  13
273100940509PROMSG    *LIBL     KEY0014  14
273200940509PROMSG    *LIBL     KEY0015  15
273300940509PROMSG    *LIBL     KEY0016  16
273400940509PROMSG    *LIBL     KEY0017  17
273500940509PROMSG    *LIBL     KEY0018  18
273600940509PROMSG    *LIBL     KEY0019  19
273700050801PROMSG    *LIBL     KEY0296  20                         B0822
273800940509PROMSG    *LIBL     KEY0021  21
273900940509PROMSG    *LIBL     KEY0022  22
274000940509PROMSG    *LIBL     KEY0023  23
274100940509PROMSG    *LIBL     KEY0024  24
274200940509**  ABILITAZIONE D1
2743009407200N  01
2744009407200N  02
2745009407201N  03
2746009407201N  04
2747009407201N  05
2748009407201N  06
2749009407200N  07
2750009407201N  08
2751009407200N  09
2752009411160N  10
2753009407200N  11
2754009407201N  12
2755009407200N  13
2756009407200N  14
2757009407200N  15
2758009407200N  16
2759009407200N  17
2760009407200N  18
2761009407200N  19
2762000508011S  20                                                  B0822
2763009407200N  21
2764009407200N  22
2765009509060N  23   Impostare a '1' se si utilizzano opzioni
2766009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
