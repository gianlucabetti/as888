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
001500940223      *
001600940223      *
001700940223      *               MAPPA INDICATORI STD
001800940223      *
001900940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002000970725      *  07           PROTECT CAMPO NR. ESTERNO PER CLIENTI
002100940223      *  21           GENERICO OPERAZIONI I/O
002200940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002300940223      *  30           SFLDSP
002400940223      * N31           SFLCLR
002500940128      *  31           SFLDSPCTL
002600940128      *  32           SFLNXTCHG
002700940128      *  33           SFLEND
002800940128      *  39           OF PRTF
002900940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003000940317      *  Specificare l'uso dei singoli indicatori
003100940315      *  50 <---> 98  ERRORI SU VIDEO
003200940317      *  Specificare l'uso dei singoli indicatori
003300940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003400940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003500940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003600940128      *  99           INDIC. GENERALE DI ERRORE
003700940128     F*----------------------------------------------------*
003800980223     FANDIC01L  IF   E           K DISK
003900970725     F                                     RENAME(ANDIC000:ANDIC01)
004000951004     F*
004100980223     FANDIC02L  UF A E           K DISK
004200970725     F                                     RENAME(ANDIC000:ANDIC02)
004300061013B0907FANDIC05L  IF   E           K DISK
004400061013B0907F                                     RENAME(ANDIC000:ANDIC05)
004500000519A0778X*
004600000519A0778X***ANTXT01L  IF   E           K DISK
004700950914     F*
004800000518B0212FANA312P   O    E             PRINTER UsrOpn
004900000518B0212F*****ANA312P   O    E             PRINTER
005000951003     F*
005100941221     FANA312D   CF   E             WORKSTN
005200941221     F                                     RENAME(A312D1:FMTD1)
005300941221     F                                     RENAME(A312T1:FMTT1)
005400941221     F                                     RENAME(A312Z1:FMTZ1)
005500940201     F                                     INFDS(DSFMT)
005600940128     D*----------------------------------------------------*
005700950427     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
005800950427     D $ID             S             27    DIM(5) ALT($OZ)
005900940506     D* Schiere per la gestione dei tasti di funzione
006000940506     D $FM             S             27    DIM(24)
006100940506     D $FX             S             50    DIM(24)
006200940506     D $FC             S              1    DIM(24)
006300940506     D $FR             S              1    DIM(24)
006400940506     D* Video D1
006500940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
006600940506     D $FX1            S             50    DIM(24) ALT($FM1)
006700940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
006800940506     D $FR1            S              1    DIM(24) ALT($FC1)
006900950914     D*-------------
007000940127     D* Passaggio Parametri
007100940127     D KPJBA         E DS
007200940318     D*-------------
007300940318     D* DS Esterna gestione messaggi per COMIT
007400940318     D XMSGDS        E DS
007500940318     D  STS          E                     EXTFLD(MSGSTS)
007600940721     D                                     DIM(333)
007700940318     D  JBA          E                     EXTFLD(MSGJBA)
007800940721     D                                     DIM(502)
007900941209     D*-------------
008000941209     D* Dati di ambiente ottenuti da XSOC
008100941209     D SOC001        E DS                  EXTNAME(XSOC001DS)
008200980116     D    XscGest                     1    overlay(xscSkp:3)
008300941209     D*-------------
008400941209     D* DS Interna per dati di output di XSOC
008500941209     D XSOCDS          DS          1000
008600940203     D*-------------
008700940201     D* Passaggio Parametri al PGM di manutenzione
008800941221     D A312DS        E DS                  EXTNAME(ANA312DS)
008900950914     D*-------------
009000970725     D* DS per passaggio dati pgm User Exit
009100950323     D XUKDS         E DS                  EXTNAME(XUSRKEYDS)
009200950104     D*-------------
009300950104     D* Reperimento Autorizzazioni
009400950104     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009500950914     D*-------------
009600970703     D* Maschera dati autorizzazioni per clienti
009700950104     D GCLIDS        E DS                  EXTNAME(ANGCLIDS)
009800950914     D*-------------
009900970725     D* Maschera dati autorizzazioni per fornitori
010000970725     D GFORDS        E DS                  EXTNAME(ANGFORDS)
010100970725     d  ImmFilF      E                     ExtFld(ImmFil)
010200970725     d  ManFilF      E                     ExtFld(ManFil)
010300970725     d  AnnFilF      E                     ExtFld(AnnFil)
010400970725     d  VisFilF      E                     ExtFld(VisFil)
010500970703     D*-------------
010600950104     D* Passaggio Autorizzazioni
010700950104     D DSAUT           DS           512
010800950914     D* Ricerca tipo testo
010900950914     D A720DS        E DS                  EXTNAME(ANA720DS) INZ
011000950914     D  UNI720       E                     EXTFLD(UNITA720)
011100950914     D  TPT720       E                     EXTFLD(TPTXT720)
011200950914     D  LIN720       E                     EXTFLD(LINGUA720)
011300950914     D  CLA720       E                     EXTFLD(CLAUTE720)
011400950914     D*-------------
011500951002     D* Operaz. importazione
011600951002     D TABG17        E DS                  EXTNAME(ANGG17DS)
011700951002     D*-------------
011800941215     D* Richiamo a XATB
011900941215     D XATBDS        E DS                  INZ
012000950914     D*-------------
012100941215     D* Richiamo a XTAB
012200941215     D XTABDS        E DS                  INZ
012300950914     D*-------------
012400950914     D* Richiamo a DVCLI
012500950914     D DVCLI         E DS                  EXTNAME(DVCLIDS) INZ
012600941216     D*-------------
012700950914     D CLI             C                   CONST('  DVOCLIDS')
012800950914     D*-------------
012900950914     D*  Ds per output DVCLI formato ANCLN+ANSOG+ANIND
013000950914     D DVOCLI        E DS                  EXTNAME(DVOCLIDS) INZ
013100950914     D*-------------
013200950914     D* Anagrafica clienti
013300950914     D  CLNSOC       E                     EXTFLD(CLNSOCIETA)
013400950914     D  CLNUNT       E                     EXTFLD(CLNUNITA)
013500950914     D  CLNKCC       E                     EXTFLD(CLNKCC)
013600950914     D  CLNKSC       E                     EXTFLD(CLNKSC)
013700950914     D  CLNSOG       E                     EXTFLD(CLNSOGG)
013800950914     D  CLNTII       E                     EXTFLD(CLNTPIND)
013900950914     D  CLNCOI       E                     EXTFLD(CLNCDIND)
014000950914     D  CLNDES       E                     EXTFLD(CLNDESBREV)
014100950914     D  CLNLDV       E                     EXTFLD(CLNLINEAV)
014200950914     D  CLNFIL       E                     EXTFLD(CLNFILIALE)
014300950914     D  CLNDAC       E                     EXTFLD(CLNDTINRPC)
014400950914     D  CLNUTE       E                     EXTFLD(CLNUTENTE)
014500950914     D  CLNDVA       E                     EXTFLD(CLNDTULMO)
014600950914     D  CLNCCP       E                     EXTFLD(CLNCLIPREC)
014700950914     D  CLNCFP       E                     EXTFLD(CLNFORCORR)
014800950914     D  CLNCCF       E                     EXTFLD(CLNCLIFATT)
014900950914     D  CLNFAP       E                     EXTFLD(CLNADDSPBA)
015000950914     D  CLNFSB       E                     EXTFLD(CLNSPEBOLL)
015100950914     D  CLNCOF       E                     EXTFLD(CLNNRCPFAT)
015200950914     D  CLNIMB       E                     EXTFLD(CLNIMBUSTA)
015300950914     D  CLNFID       E                     EXTFLD(CLNFIDOSTA)
015400950914     D  CLNFIA       E                     EXTFLD(CLNFIDOASS)
015500950914     D  CLNDID       E                     EXTFLD(CLNDTFIST)
015600950914     D  CLNDIA       E                     EXTFLD(CLNDTFIAS)
015700950914     D  CLNCAS       E                     EXTFLD(CLNCLIASSO)
015800950914     D  CLNGDR       E                     EXTFLD(CLNGGRCVAG)
015900950914     D  CLNFBC       E                     EXTFLD(CLNCTRCONT)
016000950914     D  CLNFPC       E                     EXTFLD(CLNCITPROV)
016100950914     D  CLNASN       E                     EXTFLD(CLNSTATIST)
016200950914     D  CLNDAN       E                     EXTFLD(CLNDTFIRPC)
016300950914     D  CLNCLV       E                     EXTFLD(CLNCLASABC)
016400950914     D  CLNSOL       E                     EXTFLD(CLNTPTESTO)
016500950914     D  CLNMAG       E                     EXTFLD(CLNMAG)
016600950914     D  CLNADO       E                     EXTFLD(CLNADDIVA)
016700950914     D  CLNFAT       E                     EXTFLD(CLNFREQFAT)
016800950914     D  CLNTFA       E                     EXTFLD(CLNRAGGBOL)
016900950914     D  CLNRAF       E                     EXTFLD(CLNRAGGART)
017000950914     D  CLNMSP       E                     EXTFLD(CLNMDSPED)
017100950914     D  CLNRES       E                     EXTFLD(CLNRESA)
017200950914     D  CLNSSF       E                     EXTFLD(CLNSTASCOF)
017300950914     D  CLNFDP       E                     EXTFLD(CLNDETPREZ)
017400950914     D  CLNDIR       E                     EXTFLD(CLNDTINRPL)
017500950914     D  CLNDFR       E                     EXTFLD(CLNDTFIRPL)
017600950914     D  CLNLIS       E                     EXTFLD(CLNLISTINO)
017700950914     D  CLNVAL       E                     EXTFLD(CLNDIVISA)
017800950914     D  CLNFUC       E                     EXTFLD(CLNCAPODIS)
017900950914     D  CLNFUN       E                     EXTFLD(CLNFUNZRIO)
018000950914     D  CLNAGE       E                     EXTFLD(CLNAGENTE1)
018100950914     D  CLNAG2       E                     EXTFLD(CLNAGENTE2)
018200950914     D  CLNAG3       E                     EXTFLD(CLNAGENTE3)
018300950914     D  CLNPSP       E                     EXTFLD(CLNPRIOEVA)
018400950914     D  CLNBGC       E                     EXTFLD(CLNBKGSCLI)
018500950914     D  CLNRSC       E                     EXTFLD(CLNRAGGSCO)
018600950914     D  CLNRAP       E                     EXTFLD(CLNRAGGPRV)
018700950914     D  CLNTPP       E                     EXTFLD(CLNLORDNET)
018800950914     D  CLNCDP       E                     EXTFLD(CLNCONDPAG)
018900950914     D  CLNSCP       E                     EXTFLD(CLNSCOCASS)
019000950914     D  CLNPA1       E                     EXTFLD(CLNPERCAD1)
019100950914     D  CLNPA2       E                     EXTFLD(CLNPERCAD2)
019200950914     D  CLNRCO       E                     EXTFLD(CLNCONFORD)
019300950914     D  CLNRGU       E                     EXTFLD(CLNRAGGVEN)
019400950914     D  CLNSPP       E                     EXTFLD(CLNSTAPREZ)
019500950914     D  CLNCIV       E                     EXTFLD(CLNCDIVA)
019600950914     D  CLNTIN       E                     EXTFLD(CLNTPINDCM)
019700950914     D  CLNSC1       E                     EXTFLD(CLNSCOPAG1)
019800950914     D  CLNSC2       E                     EXTFLD(CLNSCOPAG2)
019900950914     D  CLNIP1       E                     EXTFLD(CLNGGINPA1)
020000950914     D  CLNIP2       E                     EXTFLD(CLNGGINPA2)
020100950914     D  CLNIP3       E                     EXTFLD(CLNGGINPA3)
020200950914     D  CLNTRA       E                     EXTFLD(CLNSESSTRA)
020300950914     D  CLNRIC       E                     EXTFLD(CLNSESSRIC)
020400950914     D  CLNTIV       E                     EXTFLD(CLNTPPVEND)
020500950914     D  CLNTPC       E                     EXTFLD(CLNTPCAUZ)
020600950914     D  CLNTSA       E                     EXTFLD(CLNACCONTO)
020700950914     D  CLNCES       E                     EXTFLD(CLNCLICESS)
020800950914     D  CLNFS1       E                     EXTFLD(CLNCLISOLL)
020900950914     D  CLNFS2       E                     EXTFLD(CLNADDSOLL)
021000950914     D  CLNFS5       E                     EXTFLD(CLNCONTRAS)
021100950914     D  CLNFS6       E                     EXTFLD(CLNADDPROD)
021200950914     D  CLNRGC       E                     EXTFLD(CLNRAGGCON)
021300950914     D  CLNFS3       E                     EXTFLD(CLNFLG1)
021400950914     D  CLNFS4       E                     EXTFLD(CLNFLG2)
021500950914     D  CLNFL3       E                     EXTFLD(CLNFLG3)
021600950914     D  CLNFL4       E                     EXTFLD(CLNFLG4)
021700950914     D  CLNFL5       E                     EXTFLD(CLNFLG5)
021800950914     D  CLNFL6       E                     EXTFLD(CLNFLG6)
021900941216     D* Anagrafica soggetti
022000941216     D  SOGCOD       E                     EXTFLD(SOGSOGG)
022100941216     D  SOGDAT       E                     EXTFLD(SOGDTENTR)
022200941216     D  SOGDAU       E                     EXTFLD(SOGDTULMO)
022300941216     D  SOGDAI       E                     EXTFLD(SOGDTINVL)
022400941216     D  SOGDAF       E                     EXTFLD(SOGDTFIVL)
022500941216     D  SOGDAN       E                     EXTFLD(SOGDTNASC)
022600941216     D  SOGDES       E                     EXTFLD(SOGDES)
022700941216     D  SOGDE1       E                     EXTFLD(SOGDESAGG)
022800941216     D  SOGCDF       E                     EXTFLD(SOGCDFISC)
022900941216     D  SOGIVA       E                     EXTFLD(SOGPARTIVA)
023000941216     D  SOGFPI       E                     EXTFLD(SOGFRCPIVA)
023100941216     D  SOGCIA       E                     EXTFLD(SOGISCCIAA)
023200941216     D  SOGSES       E                     EXTFLD(SOGSEX)
023300941216     D  SOGNAT       E                     EXTFLD(SOGNATGIUR)
023400941216     D  SOGCOP       E                     EXTFLD(SOGCODPREC)
023500941216     D  SOGABI       E                     EXTFLD(SOGABI)
023600941216     D  SOGCLG       E                     EXTFLD(SOGCLFSOG)
023700950914     D* Anagrafica indirizzi
023800950914     D  INDCOD       E                     EXTFLD(INDSOGG)
023900950914     D  INDTII       E                     EXTFLD(INDTPIND)
024000950914     D  INDCOI       E                     EXTFLD(INDCDIND)
024100950914     D  INDDAU       E                     EXTFLD(INDDTULMO)
024200950914     D  INDDAI       E                     EXTFLD(INDDTINVL)
024300950914     D  INDDAF       E                     EXTFLD(INDDTFIVL)
024400950914     D  INDDES       E                     EXTFLD(INDDESFIL)
024500950914     D  INDDE1       E                     EXTFLD(INDDESAGG)
024600950914     D  INDVIA       E                     EXTFLD(INDINDRIZ)
024700950914     D  INDCIT       E                     EXTFLD(INDLOCALIT)
024800950914     D  INDCAP       E                     EXTFLD(INDCAP)
024900950914     D  INDPRV       E                     EXTFLD(INDPROV)
025000950914     D  INDSTA       E                     EXTFLD(INDSTATO)
025100950914     D  INDCAB       E                     EXTFLD(INDCAB)
025200950914     D  INDLIN       E                     EXTFLD(INDLINGUA)
025300950914     D  INDTLX       E                     EXTFLD(INDTELEX)
025400950914     D  INDTEL       E                     EXTFLD(INDTELEFON)
025500950914     D  INDTLF       E                     EXTFLD(INDTELEFAX)
025600950206     D*-------------
025700950914     D* Richiamo a DVFOR
025800950914     D DVFOR         E DS                  EXTNAME(DVFORDS) INZ
025900950914     D*-------------
026000950914     D FRN             C                   CONST('  DVOFRNDS')
026100950914     D*-------------
026200950914     D*  Ds per output DVFRN formato ANFRN+ANSOG+ANIND
026300001010A0840X*DVOFRN        E DS                  EXTNAME(DVOFRNDS) INZ
026400001010A0840D DVOFRN        E DS          1000    EXTNAME(DVOFRNDS) INZ
026500950914     D*-------------
026600950914     D* Anagrafica fornitori
026700950914     D  FRNSOC       E                     EXTFLD(FRNSOCIETA)
026800950914     D  FRNUNT       E                     EXTFLD(FRNUNITA)
026900950914     D  FRNKCC       E                     EXTFLD(FRNKCC)
027000950914     D  FRNKSC       E                     EXTFLD(FRNKSC)
027100950914     D  FRNFIL       E                     EXTFLD(FRNFILIALE)
027200950914     D  FRNSOG       E                     EXTFLD(FRNSOGG)
027300950914     D  FRNTII       E                     EXTFLD(FRNTPIND)
027400950914     D  FRNCOI       E                     EXTFLD(FRNCDIND)
027500950914     D  FRNDUM       E                     EXTFLD(FRNDTULMO)
027600950914     D  FRNUTE       E                     EXTFLD(FRNUTENTE)
027700950914     D  FRNDIR       E                     EXTFLD(FRNDTINRPF)
027800950914     D  FRNDFR       E                     EXTFLD(FRNDTFIRPF)
027900950914     D  FRNDES       E                     EXTFLD(FRNDESBREV)
028000950914     D  FRNGPM       E                     EXTFLD(FRNGRUMERC)
028100950914     D  FRNSGM       E                     EXTFLD(FRNSETMERC)
028200950914     D  FRNCFA       E                     EXTFLD(FRNFORALT)
028300950914     D  FRNLIS       E                     EXTFLD(FRNLISTINO)
028400950914     D  FRNVAL       E                     EXTFLD(FRNDIVISA)
028500950914     D  FRNCDD       E                     EXTFLD(FRNCONDCON)
028600950914     D  FRNQFO       E                     EXTFLD(FRNQUALFOR)
028700950914     D  FRNVAF       E                     EXTFLD(FRNVOLAFF)
028800950914     D  FRNCIV       E                     EXTFLD(FRNCDIVA)
028900950914     D  FRNCOB       E                     EXTFLD(FRNCOLLOBB)
029000950914     D  FRNTCL       E                     EXTFLD(FRNTPCOLL)
029100950914     D  FRNFRL       E                     EXTFLD(FRNFORLIST)
029200950914     D  FRNFOF       E                     EXTFLD(FRNFORFATT)
029300950914     D  FRNPOT       E                     EXTFLD(FRNFORPOT)
029400950914     D  FRNTRP       E                     EXTFLD(FRNREPPRZL)
029500950914     D  FRNFFB       E                     EXTFLD(FRNFORBLO)
029600950914     D  FRNLSF       E                     EXTFLD(FRNLISFIL)
029700950914     D  FRNCPA       E                     EXTFLD(FRNABILPRI)
029800950914     D  FRNMDC       E                     EXTFLD(FRNMDCONS)
029900950914     D  FRNMAG       E                     EXTFLD(FRNMAG)
030000950914     D  FRNCLI       E                     EXTFLD(FRNCLICORR)
030100950914     D  FRNFRP       E                     EXTFLD(FRNFORPREC)
030200950914     D  FRNSC1       E                     EXTFLD(FRNSCOPAG)
030300950914     D  FRNSC2       E                     EXTFLD(FRNSCOPAG2)
030400950914     D  FRNIP1       E                     EXTFLD(FRNGGINPA1)
030500950914     D  FRNIP2       E                     EXTFLD(FRNGGINPA2)
030600950914     D  FRNIP3       E                     EXTFLD(FRNGGINPA3)
030700950914     D  FRNTRA       E                     EXTFLD(FRNSESSTRA)
030800950914     D  FRNRIC       E                     EXTFLD(FRNSESSRIC)
030900950914     D  FRNCDP       E                     EXTFLD(FRNCONDPAG)
031000950914     D* Anagrafica soggetti
031100950914     D  FOGCOD       E                     EXTFLD(SOGSOGG)
031200950914     D  FOGDAT       E                     EXTFLD(SOGDTENTR)
031300950914     D  FOGDAU       E                     EXTFLD(SOGDTULMO)
031400950914     D  FOGDAI       E                     EXTFLD(SOGDTINVL)
031500950914     D  FOGDAF       E                     EXTFLD(SOGDTFIVL)
031600950914     D  FOGDAN       E                     EXTFLD(SOGDTNASC)
031700950914     D  FOGDES       E                     EXTFLD(SOGDES)
031800950914     D  FOGDE1       E                     EXTFLD(SOGDESAGG)
031900950914     D  FOGCDF       E                     EXTFLD(SOGCDFISC)
032000950914     D  FOGIVA       E                     EXTFLD(SOGPARTIVA)
032100950914     D  FOGFPI       E                     EXTFLD(SOGFRCPIVA)
032200950914     D  FOGCIA       E                     EXTFLD(SOGISCCIAA)
032300950914     D  FOGSES       E                     EXTFLD(SOGSEX)
032400950914     D  FOGNAT       E                     EXTFLD(SOGNATGIUR)
032500950914     D  FOGCOP       E                     EXTFLD(SOGCODPREC)
032600950914     D  FOGABI       E                     EXTFLD(SOGABI)
032700950914     D  FOGCLG       E                     EXTFLD(SOGCLFSOG)
032800950914     D* Anagrafica indirizzi
032900950914     D  FNDCOD       E                     EXTFLD(INDSOGG)
033000950914     D  FNDTII       E                     EXTFLD(INDTPIND)
033100950914     D  FNDCOI       E                     EXTFLD(INDCDIND)
033200950914     D  FNDDAU       E                     EXTFLD(INDDTULMO)
033300950914     D  FNDDAI       E                     EXTFLD(INDDTINVL)
033400950914     D  FNDDAF       E                     EXTFLD(INDDTFIVL)
033500950914     D  FNDDES       E                     EXTFLD(INDDESFIL)
033600950914     D  FNDDE1       E                     EXTFLD(INDDESAGG)
033700950914     D  FNDVIA       E                     EXTFLD(INDINDRIZ)
033800950914     D  FNDCIT       E                     EXTFLD(INDLOCALIT)
033900950914     D  FNDCAP       E                     EXTFLD(INDCAP)
034000950914     D  FNDPRV       E                     EXTFLD(INDPROV)
034100950914     D  FNDSTA       E                     EXTFLD(INDSTATO)
034200950914     D  FNDCAB       E                     EXTFLD(INDCAB)
034300950914     D  FNDLIN       E                     EXTFLD(INDLINGUA)
034400950914     D  FNDTLX       E                     EXTFLD(INDTELEX)
034500950914     D  FNDTEL       E                     EXTFLD(INDTELEFON)
034600950914     D  FNDTLF       E                     EXTFLD(INDTELEFAX)
034700950914     D*-------------
034800950206     D* DS esterna codice IVA
034900950206     D TABG48        E DS                  EXTNAME(ANGG48DS) INZ
035000950206     D*-------------
035100950206     D* Parametri
035200950206     D X06DS         E DS                  EXTNAME(X06G48DS)
035300950206     D  X06SOC       E                     EXTFLD(X06SOCIETA)
035400950206     D  X06CIV       E                     EXTFLD(X06CDIVA)
035500950206     D  X06DAT       E                     EXTFLD(X06DATA)
035600950206     D  X06LIN       E                     EXTFLD(X06LINGUA)
035700940201     D*-------------
035800940201     D DSFMT           DS
035900940506     D  $TASTO               369    369
036000940201     D  NRG                  370    370
036100940201     D  NCL                  371    371
036200940201     D*-------------
036300940201     D* posizione cursore
036400940201     D CURSOR          DS
036500940223     D  RIRI                   1      2B 0 INZ
036600940201     D  R$$                    2      2
036700940223     D  COCO                   3      4B 0 INZ
036800940201     D  C$$                    4      4
036900940207     D*-------------
037000940207     D* Reperimento nome PGM
037100940207     D STATUS         SDS           333
037200940207     D  DSPGM            *PROC
037300950104     D  PARMS            *PARMS
037400940225     D*-------------
037500940616     D* Costanti
037600941117     D* data a 0
037700941117     D $DT0            C                   CONST('0001-01-01')
037800940225     D*-------------
037900940506     D* Tasti di funzione
038000940506     D F01             C                   CONST(X'31')
038100940506     D F02             C                   CONST(X'32')
038200940506     D F03             C                   CONST(X'33')
038300940506     D F04             C                   CONST(X'34')
038400940506     D F05             C                   CONST(X'35')
038500940506     D F06             C                   CONST(X'36')
038600940506     D F07             C                   CONST(X'37')
038700940506     D F08             C                   CONST(X'38')
038800940506     D F09             C                   CONST(X'39')
038900940506     D F10             C                   CONST(X'3A')
039000940506     D F11             C                   CONST(X'3B')
039100940506     D F12             C                   CONST(X'3C')
039200940506     D F13             C                   CONST(X'B1')
039300940506     D F14             C                   CONST(X'B2')
039400940506     D F15             C                   CONST(X'B3')
039500940506     D F16             C                   CONST(X'B4')
039600940506     D F17             C                   CONST(X'B5')
039700940506     D F18             C                   CONST(X'B6')
039800940506     D F19             C                   CONST(X'B7')
039900940506     D F20             C                   CONST(X'B8')
040000940506     D F21             C                   CONST(X'B9')
040100940506     D F22             C                   CONST(X'BA')
040200940506     D F23             C                   CONST(X'BB')
040300940506     D F24             C                   CONST(X'BC')
040400940506     D ENTER           C                   CONST(X'F1')
040500940506     D ROLDWN          C                   CONST(X'F4')
040600940506     D ROLLUP          C                   CONST(X'F5')
040700950427     D*-------------
040800950427     D SOTTIT          C                   CONST('PROMSG    *LIBL-
040900950915     D                                          COSXXXX')
041000000114A0735D ZDateISO        S               D
041100041202B0762D DataIso         S               D
041200000519A0778D*-------------
041300000519  "  D* Variabili per chiamata a X72Txt
041400000519  "  D XEsito          S              1A
041500000519  "  D XOpe            S              1A
041600000519  "  D XSocieta        S              3A
041700000519  "  D XTpTesto        S              2A
041800000519  "  D XUnita          S              8A
041900000519  "  D XLingua         S              1A
042000000523  "  D XClaUte         S              2A
042100000519  "  D XPosRig         S              1A
042200000519  "  D XNrRiga         S              9P 0
042300000519  "  D XRibat          S              1A
042400000519  "  D XTesRig         S             70A
042500000519  "  D XNrChiavi       S              1  0
042600000519A0778D XUsoUnita       S              1A
042700081223D2461D*-------------
042800081223  "  D* DS esterna per dati fornitori
042900081223  "  D DVAFOR        E DS
043000081223  "  D* DS esterna per dati indirizzo
043100081223  "  D ANIND         E DS                  EXTNAME(ANIND00F) PREFIX($:1)
043200081223  "  D*-------------
043300081223  "  D* Definizioni parametri driver soggetti
043400081223  "  DdvaRic           S              1
043500081223  "  DdvaSocieta       S              3
043600081223  "  DdvaUnita         S              8
043700081223  "  DdvaDtRif         S               D
043800081223  "  DdvaStrutt        S             10
043900081223  "  DdvaLenOut        S              9B 0
044000081223  "  DdvaSnatura       S              1
044100081223  "  DdvaCodice        S              8
044200081223  "  DdvaLineav        S              3
044300081223  "  DdvaFiliale       S              3  0
044400081223  "  DdvaSogg          S              8
044500081223  "  DdvaTpInd         S              2
044600081223  "  DdvaCdInd         S              3
044700081223  "  DdvaOutput        S           4000
044800081223  "  DdvaErrore        S              1
044900081223  "  D WCap            S                   like(XScCap)
045000081223  "  D WLocali         S                   like(XScCit)
045100081223  "  D WProv           S                   like(XScPrv)
045200081223D2461D WCognNome       S                   like(RagSoc)
045300081223     D*-------------
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
049200061019C1937IANDIC05
049300061013  "  I              DICSOCIETA                  DICSOC
049400090224C1937I              DICCF                       DICCF
049500090224B1090x*********     DICKSC                      DICKSC
049600090224C1937I              DICANNOINT                  DICAIN
049700061013C1937I              DICNRINTER                  DICNIN
049800950914     I*-------------
049900000519A0778X* Testi lettere
050000000519  "  X***ANTXT000
050100000519  "  X***           TXTSOCIETA                  TXTSOC
050200000519  "  X***           TXTUNITA                    TXTUNI
050300000519  "  X***           TXTTPTESTO                  TXTTPT
050400000519  "  X***           TXTLINGUA                   TXTLIN
050500000519  "  X***           TXTCLAUTE                   TXTCLA
050600000519  "  X***           TXTPOSRIG                   TXTPOS
050700000519  "  X***           TXTNRRIGA                   TXTNRR
050800000519  "  X***           TXTRIBAT                    TXTRIB
050900000519A0778X***           TXTTESRIG                   TXTTES
051000941209     I*-------------
051100940127     C*----------------------------------------------------*
051200940127     C*                MAIN LINE PROGRAM
051300940127     C*----------------------------------------------------*
051400940127     C*
051500940223     C* inizializzazione variabili
051600940223     C                   EXSR      INZVAR
051700940223     C*
051800940223     C     $FINE         DOWEQ     *OFF
051900940202     C     $GEST         CASEQ     'D1'          GESD1
052000941027     C                   ENDCS
052100941027     C                   ENDDO
052200940201     C*
052300940325     C* fine programma
052400940325     C                   EXSR      ENDPGM
052500940325     C/EJECT
052600940325     C************************************************************
052700950914     C* ENDPGM
052800940325     C************************************************************
052900940325     C     ENDPGM        BEGSR
053000941214     C*
053100941214     C* se ho confermato l'uscita proseguo
053200941214     C     $FINE         IFEQ      *ON
053300980223     C*
053400980223     C*** Faccio sempre ROLBK per il numeratore
053500980223     C**(se ho confermato l'inserimento precedente ho già fatto COMMIT)
053600980223     C                   ROLBK
053700940325     C*
053800940325     C* disallocazione rcd nel caso di modifica/annullamento
053900941221     C     OPZ312        IFEQ      '02'
054000941221     C     OPZ312        OREQ      '04'
054100970725     C                   UNLOCK    ANDIC02L                             22
054200941027     C                   ENDIF
054300940325     C*
054400940325     C* passaggio dati al pgm chiamante
054500941221     C                   MOVEL     A312DS        KPJBU
054600940325     C*
054700940325     C                   SETON                                            RT
054800940616     C                   RETURN
054900941214     C*
055000941214     C                   ENDIF
055100940325     C*
055200940325     C                   ENDSR
055300940325     C/EJECT
055400940201     C************************************************************
055500940201     C* RICERCHE
055600940201     C************************************************************
055700940127     C     SEARCH        BEGSR
055800940127     C*
055900940127     C* determino Riga/Colonna del cursore
056000940127     C                   MOVE      NRG           R$$
056100940127     C                   MOVE      NCL           C$$
056200940127     C                   Z-ADD     RIRI          CURR
056300940127     C                   Z-ADD     COCO          CURC
056400940127     C*
056500940224     C* I campi che al momento dell'F4 risultano protetti non devono
056600940224     C* essere abilitati alla funzione di ricerca
056700941027     C*
056800941027     C                   SELECT
056900941215     C*
057000941221     C* FMTD1  - IVA
057100941221     C     H1NMFL        WHENEQ    'D1IVA'
057200941215     C                   RESET                   XTABDS
057300950117     C                   MOVE      XSCSOC        XTAAZI
057400941215     C                   MOVE      XSCLIN        XTALIN
057500941221     C                   MOVE      *BLANK        WWIVA
057600941221     C                   MOVE      WWIVA         XTAKEY
057700941215     C                   MOVE      '01'          XTAOPZ
057800941215     C                   Z-ADD     CURR          XTARIG
057900941215     C                   Z-ADD     CURC          XTACOL
058000941215     C                   CLEAR                   KPJBU
058100941215     C                   MOVEL     XTABDS        KPJBU
058200020418B0435X****               IF        XscGest <> '2'                               Tile non installato
058300941221     C                   CALL      'ANGG48R'
058400941215     C                   PARM                    KPJBA
058500020418B0435X****               ELSE                                                   Tile installato
058600001027  "  X****               CALL      'SC$G48R'
058700001027  "  X****               PARM                    KPJBA
058800020418B0435X****               ENDIF
058900941215     C                   MOVEL     KPJBU         XTABDS
059000941215     C     XTAERR        IFNE      '1'
059100941221     C                   MOVE      XTAKEY        WWIVA
059200941215     C                   ELSE
059300941221     C                   MOVE      *BLANK        WWIVA
059400941215     C                   ENDIF
059500941221     C     WWIVA         IFNE      *BLANK
059600941221     C                   MOVEL     WWIVA         D1IVA
059700941215     C                   ENDIF
059800941215     C* imposto pos. del cursore
059900941215     C                   Z-ADD     CURR          H1RIGA
060000941215     C                   Z-ADD     CURC          H1COLO
060100950914     C*
060200950914     C* FMTD1  - TIPO TESTO
060300950914     C     H1NMFL        WHENEQ    'D1TPT'
060400951002     C     COF312        ANDEQ     'F'
060500950914     C                   RESET                   A720DS
060600950914     C                   MOVEL     '01'          OPZ720
060700000523A0778X***                MOVEL     XSCDUN        UNI720
060800950914     C                   MOVEL     XSCLIN        LIN720
060900950914     C                   MOVEL     A720DS        KPJBU
061000950914     C                   CALL      'ANA720R'
061100950914     C                   PARM                    KPJBA
061200950914     C                   MOVEL     KPJBU         A720DS
061300950914     C     TPT720        IFNE      *BLANK
061400950914     C                   MOVEL     TPT720        D1TPT
061500950914     C                   ENDIF
061600950914     C* imposto pos. del cursore
061700950914     C                   Z-ADD     CURR          H1RIGA
061800950914     C                   Z-ADD     CURC          H1COLO
061900951002     C*
062000951002     C* FMTD1  - OPERAZ. IMPORTAZIONE
062100951002     C     H1NMFL        WHENEQ    'D1OPI'
062200951002     C                   RESET                   XTABDS
062300951002     C                   MOVE      XSCSOC        XTAAZI
062400951002     C                   MOVE      XSCLIN        XTALIN
062500951002     C                   MOVE      *BLANK        WWOPI
062600951002     C                   MOVE      WWOPI         XTAKEY
062700951002     C                   MOVE      '01'          XTAOPZ
062800951002     C                   Z-ADD     CURR          XTARIG
062900951002     C                   Z-ADD     CURC          XTACOL
063000951002     C                   CLEAR                   KPJBU
063100951002     C                   MOVEL     XTABDS        KPJBU
063200951002     C                   CALL      'ANGG17R'
063300951002     C                   PARM                    KPJBA
063400951002     C                   MOVEL     KPJBU         XTABDS
063500951002     C     XTAERR        IFNE      '1'
063600951002     C                   MOVE      XTAKEY        WWOPI
063700951002     C                   ELSE
063800951002     C                   MOVE      *BLANK        WWOPI
063900951002     C                   ENDIF
064000951002     C     WWOPI         IFNE      *BLANK
064100951002     C                   MOVEL     WWOPI         D1OPI
064200951002     C                   ENDIF
064300951002     C* imposto pos. del cursore
064400951002     C                   Z-ADD     CURR          H1RIGA
064500951002     C                   Z-ADD     CURC          H1COLO
064600940223     C*
064700940223     C* ricerca non abilitata in questa posizione
064800941027     C                   OTHER
064900940926     C* imposto pos. del cursore
065000940926     C                   Z-ADD     CURR          H1RIGA
065100940926     C                   Z-ADD     CURC          H1COLO
065200940926     C                   SETON                                        9899
065300940127     C*
065400941027     C                   ENDSL
065500940127     C*
065600940506     C                   MOVE      *HIVAL        $LASTV
065700940127     C                   ENDSR
065800940225     C/EJECT
065900940225     C************************************************************
066000940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
066100940225     C************************************************************
066200940225     C     GESPIE        BEGSR
066300940225     C*
066400940225     C                   SELECT
066500940225     C*
066600940506     C     $GEST         WHENEQ    'D1'
066700940506     C     $ULKD1        IFEQ      0
066800940506     C* riempimento opzioni
066900940506     C                   EXSR      F24D1
067000940506     C                   ELSE
067100940506     C                   WRITE     FMTZ1
067200940506     C                   ENDIF
067300940225     C*
067400940225     C                   ENDSL
067500940225     C*
067600940225     C                   ENDSR
067700940131     C/EJECT
067800940131     C************************************************************
067900940510     C* GESTIONE VIDEO RECORD D1
068000940131     C************************************************************
068100940127     C     GESD1         BEGSR
068200940225     C* inizializzazione videata
068300941117     C     $INZD1        IFEQ      *ON
068400940127     C                   EXSR      INZD1
068500940223     C                   MOVE      *OFF          $INZD1
068600941117     C                   ENDIF
068700940225     C* emissione piede videata se proveniente da altra
068800940225     C* salvataggio valore formato di provenienza
068900941117     C     $LASTV        IFNE      'D1'
069000940207     C                   WRITE     FMTT1
069100940225     C                   EXSR      GESPIE
069200940202     C                   MOVE      $LASTV        $LASTG
069300940202     C                   MOVE      'D1'          $LASTV
069400941117     C                   ENDIF
069500940707     C*
069600940707     C* se esistono errori sulla videata
069700940707     C* emetto la write del formato a indicatori spenti per vedere
069800940707     C* le eventuali decodifiche
069900941117     C     *IN99         IFEQ      *ON
070000940707     C     $GEST         ANDEQ     'D1'
070100940707     C                   MOVEA     *IN           WIN
070200940707     C                   MOVE      *ALL'0'       IN5098           49
070300940707     C                   MOVEA     IN5098        *IN(50)
070400940707     C                   WRITE     FMTD1
070500940707     C                   MOVEA     WIN           *IN
070600941117     C                   ENDIF
070700940131     C*
070800941117     C                   SELECT
070900940203     C* Annullamento
071000941221     C     OPZ312        WHENEQ    '04'
071100940224     C* Visualizzazione
071200941221     C     OPZ312        OREQ      '05'
071300940203     C                   WRITE     FMTD1
071400940203     C                   EXFMT     PROTECT
071500940224     C* Immissione/Modifica/Copia
071600941117     C                   OTHER
071700940203     C                   EXFMT     FMTD1
071800941117     C                   ENDSL
071900940506     C* controllo abilitazione tasti funzione
072000940506     C*
072100940506     C                   EXSR      CTRKD1
072200940506     C*
072300941117     C     *IN99         IFEQ      *OFF
072400940131     C*
072500941117     C                   SELECT
072600940609     C* ROLLUP
072700941215     C*          $TASTO    WHEQ ROLLUP
072800941215     C*                    MOVE 'D2'      $GEST
072900940916     C* F2=Decodifica
073000940916     C     $TASTO        WHENEQ    F02
073100940916     C                   EXSR      F02D1
073200940131     C* F3=Fine
073300940506     C     $TASTO        WHENEQ    F03
073400940309     C                   EXSR      F03D1
073500940131     C* F4=Ricerca
073600940506     C     $TASTO        WHENEQ    F04
073700940127     C                   EXSR      SEARCH
073800940224     C* F5=Refresh
073900940506     C     $TASTO        WHENEQ    F05
074000940309     C                   EXSR      F05D1
074100940223     C* F8=Successivo
074200940506     C     $TASTO        WHENEQ    F08
074300940309     C                   EXSR      F08D1
074400940131     C* F12=Ritorno
074500940506     C     $TASTO        WHENEQ    F12
074600940309     C                   EXSR      F12D1
074700050801B0822C* F20=Annullamento revoca
074800050801  "  C     $TASTO        WHENEQ    F20
074900050801B0822C                   EXSR      F20D1
075000940315     C* F24=Altri tasti
075100940506     C     $TASTO        WHENEQ    F24
075200940315     C                   EXSR      F24D1
075300940131     C*
075400941117     C                   OTHER
075500940127     C                   EXSR      CTRD1
075600061031B0907C* Controllo univocità numero nell'anno solare in caso di immiss.
075700061031  "  c* o copia
075800061031  "  c                   if        *in99=*off
075900061031  "  C                   if        D1NIn <> 0 and *in99=*off and
076000061031  "  c                             (Opz312='01' or Opz312='03')
076100061031  "  C                   exsr      CtrNum
076200061031  "  C                   endif
076300061031B0907C                   endif
076400940203     C*
076500941117     C     $TASTO        IFEQ      F06
076600940207     C     *IN99         ANDEQ     *OFF
076700940224     C* eseguo aggiornamento
076800941214     C                   EXSR      AGGANA
076900940224     C* eseguo operazioni del dopo-aggiornamento
077000941117     C     *IN99         IFEQ      *OFF
077100940318     C                   EXSR      GESAGG
077200941117     C                   ENDIF
077300941117     C                   ENDIF
077400940203     C*
077500941117     C                   ENDSL
077600940224     C*
077700941117     C                   ENDIF
077800941027     C*
077900940117     C                   ENDSR
078000940506     C/EJECT
078100940506     C************************************************************
078200940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
078300940506     C************************************************************
078400940506     C     CTRKD1        BEGSR
078500941205     C*
078600940506     C                   SETOFF                                       99
078700940506     C                   MOVEA     $FC1          $FC
078800940506     C                   EXSR      CTRKEY
078900940506     C     *IN99         IFEQ      *ON
079000940506     C                   SETON                                        97
079100940506     C                   ENDIF
079200951012     C*
079300951012     C                   ENDSR
079400951012     C/EJECT
079500941209     C************************************************************
079600941209     C* INIZIALIZZAZIONE VIDEATA DATI
079700941209     C************************************************************
079800940127     C     INZD1         BEGSR
079900940131     C*
080000970210     C                   Z-ADD     0             DtcSAV
080100970210     C                   Z-ADD     0             NrcSAV
080200951004     C*
080300940131     C                   CLEAR                   FMTD1
080400950914     C*
080500950914     C                   MOVE      XSCON         D1ON1
080600950914     C                   MOVE      XSCOFF        D1OF1
080700950914     C                   MOVE      XSCOFF        D1OPE
080800950914     C*
080900950915     C     COF312        IFEQ      'F'
081000950915     C                   SETON                                        11
081100950915     C                   MOVE      XSCON         D1ON2
081200950915     C                   MOVE      XSCOFF        D1OF2
081300950915     C                   MOVE      XSCOFF        D1REV
081400950915     C                   ENDIF
081500950915     C*
081600950914     C* Reperisco il soggetto passato dal chiamante
081700941222     C                   MOVEL     KSC312        D1KSC
081800941222     C*
081900941222     C                   MOVEL     *BLANK        D1KSCD
082000941222     C                   MOVEL     *BLANK        D1IND
082100941222     C                   MOVEL     *BLANK        D1CAP
082200941222     C                   MOVEL     *BLANK        D1LOC
082300941222     C                   MOVEL     *BLANK        D1PRV
082400941222     C                   MOVEL     *BLANK        D1IVAD
082500950914     C*
082600950914     C     COF312        IFEQ      'C'
082700950914     C*
082800950914     C* Decodifica cliente
082900941222     C     D1KSC         IFNE      *BLANK
083000941222     C                   RESET                   DVCLI
083100950110     C                   MOVEL     WWWSOC        DCLSOC
083200941222     C                   MOVEL     D1KSC         DCLCLI
083300941222     C                   MOVEL     *BLANK        DCLLDV
083400941222     C                   MOVEL     *ZERO         DCLFIL
083500941222     C                   Z-ADD     1             DCLNRK
083600941222     C                   MOVEL     '1'           DCLRIC
083700941222     C                   MOVEL     CLI           DCLSET
083800941222     C                   MOVEL     '1'           DCLVIS
083900941222     C                   CALL      'DVCLI'
084000941222     C                   PARM                    DVCLI
084100941222     C                   PARM                    DVOCLI
084200941222     C     DCLERR        IFEQ      '0'
084300950526     C                   MOVEL     INDDES        D1KSCD
084400941222     C                   MOVEL     INDVIA        D1IND
084500941222     C                   MOVEL     INDCAP        D1CAP
084600941222     C                   MOVEL     INDCIT        D1LOC
084700941222     C                   MOVEL     INDPRV        D1PRV
084800941222     C                   ENDIF
084900941222     C                   ENDIF
085000941222     C*
085100950914     C                   ELSE
085200950914     C*
085300950914     C     COF312        IFEQ      'F'
085400950914     C*
085500950914     C* Decodifca fornitore
085600950914     C     D1KSC         IFNE      *BLANK
085700950914     C                   RESET                   DVFOR
085800950914     C                   MOVEL     WWWSOC        DFRSOC
085900950914     C                   MOVEL     D1KSC         DFRFOR
086000951011     C                   MOVEL     *ZERO         DFRFIL
086100951011     C                   Z-ADD     1             DFRNRK
086200951011     C                   MOVEL     '1'           DFRRIC
086300951011     C                   MOVEL     FRN           DFRSET
086400951011     C                   MOVEL     '1'           DFRVIS
086500950914     C                   CALL      'DVFOR'
086600950914     C                   PARM                    DVFOR
086700950914     C                   PARM                    DVOFRN
086800950914     C     DFRERR        IFEQ      '0'
086900950914     C                   MOVEL     FNDDES        D1KSCD
087000950914     C                   MOVEL     FNDVIA        D1IND
087100950914     C                   MOVEL     FNDCAP        D1CAP
087200950914     C                   MOVEL     FNDCIT        D1LOC
087300950914     C                   MOVEL     FNDPRV        D1PRV
087400950914     C                   ENDIF
087500950914     C                   ENDIF
087600950914     C*
087700950914     C                   ENDIF
087800950914     C*
087900950914     C                   ENDIF
088000950914     C*
088100940225     C                   Z-ADD     1             $PIED1
088200940207     C*
088300940224     C* valorizzo la chiave univoca per reperire i dati
088400940224     C* se non immissione
088500941221     C     OPZ312        IFNE      '01'
088600940315     C*-------------------------------------------------------------
088700940315     C* impostare i campi chiave ricevuti
088800940315     C*-------------------------------------------------------------
088900950110     C                   MOVEL     WWWSOC        DICSOC
089000951002     C                   MOVEL     COF312        DICCF
089100970725     C                   MOVEL     NRI312        DICNIN
089200970725     C                   MOVEL     DTI312        DICDIN
089300940318     C*
089400941201     C     *IN22         DOUEQ     *OFF
089500940318     C     MSGRTN        ORNE      'R'
089600940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
089700941221     C     OPZ312        IFEQ      '02'
089800941221     C     OPZ312        OREQ      '04'
089900970725     C     K4DIC2        CHAIN     ANDIC02                            2122
090000940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
090100941201     C                   ELSE
090200970725     C     K4DIC2        CHAIN(N)  ANDIC02                            2122
090300941201     C                   ENDIF
090400940318     C*
090500940318     C* Gestione RCD allocato
090600941201     C     *IN22         IFEQ      *ON
090700941221     C                   MOVEL     'ANDIC00F'    MSGFIL
090800940318     C                   EXSR      MSGPGM
090900941201     C                   ENDIF
091000941201     C                   ENDDO
091100940224     C*
091200940224     C* se il rcd da gestire non viene trovato
091300940318     C* oppure è allocato da altro pgm
091400940224     C* ritorno al pgm guida avvertendolo dell'errore
091500941201     C     *IN21         IFEQ      *ON
091600940318     C     *IN22         OREQ      *ON
091700940224     C                   MOVE      *ON           $FINE
091800941221     C                   MOVE      '1'           ERR312
091900940224     C*
092000940224     C* altrimenti
092100941201     C                   ELSE
092200940224     C* riempio la videata
092300940224     C                   EXSR      RIED1
092400950206     C* Aggiungere le routine di riempimento dei record successivi
092500940224     C* richiamo routine dei ctrl per decodificarla
092600940614     C                   EXSR      CTRD1
092700950206     C* Aggiungere le routine di controllo dei record successivi
092800940224     C                   MOVE      *ALL'0'       IN5098
092900940224     C                   MOVEA     IN5098        *IN(50)
093000940224     C* definizione indicatori su video
093100970207     C                   SETOFF                                       0207
093200941201     C                   SELECT
093300941221     C     OPZ312        WHENEQ    '02'
093400940224     C                   SETON                                        02
093500970207     c* se cliente proteggo anche numero protocollo esterno
093600970207     c                   if        cof312 = 'C'
093700150121>>>>>c*                  seton                                        07
093800970207     c                   endif
093900941221     C     OPZ312        WHENEQ    '05'
094000941201     C                   ENDSL
094100940224     C*
094200941201     C                   ENDIF
094300940223     C*
094400950914     C                   ELSE
094500950914     C* se immissione
094600950914     C                   MOVE      XSCDIV        D1MC
094700991217B0136 *Se NON è un fornitore propongo a video la data di oggi
094800991217B0136C                   If        Cof312     <> 'F'
094900000114A0735C*****              MOVE      UDATE         D1DIN
095000000114A0735C                   MOVE      ZDate         D1DIN
095100991217B0136C                   EndIf
095200991217  "   *
095300991217  "   *Reperisco il nr. protocollo interno  solo in CTRD1
095400991216  "   *(in base alla data protocollo interno impostata dall'utente)
095500991216B0136C*******************EXSR      REPNUM
095600950914     C*
095700941201     C                   ENDIF
095800050801B0822c* definizione tasti
095900050801B0822c                   EXSR      DEFOPZ
096000940224     C*
096100940117     C                   ENDSR
096200940207     C/EJECT
096300940207     C************************************************************
096400940510     C* RIEMPIMENTO VIDEATA  D1
096500940207     C************************************************************
096600940207     C     RIED1         BEGSR
096700940207     C*
096800941221     C                   MOVEL     DICKSC        D1KSC
096900941221     C                   MOVEL     DICNRC        D1NRC
097000951013     C* Se opzione Copia imposto oggi in Data protocollo interno
097100951013     C* e reperisco Nr. protocollo interno dal numeratore
097200951012     C     OPZ312        IFEQ      '03'
097300991217B0136 *Se NON è un fornitore propongo a video la data di oggi
097400991217B0136C                   If        Cof312     <> 'F'
097500000114A0735C*****              MOVE      UDATE         D1DIN
097600000114A0735C                   MOVE      ZDate         D1DIN
097700991217B0136C                   EndIf
097800991217  "   *Reperisco il nr. protocollo interno nel ctrD1
097900991216  "   *(in base alla data protocollo interno impostata dall'utente)
098000991216B0136C*******************EXSR      REPNUM
098100951012     C                   ELSE
098200960416     C                   CALLb     'XDT4000'
098300951012     C                   PARM      DICDIN        XDTAMG           10
098400951012     C                   PARM                    XDTGMA            6 0
098500951012     C                   PARM      2             XDTSTA            1 0
098600951012     C     XDTSTA        IFEQ      0
098700951012     C                   MOVE      XDTGMA        D1DIN
098800951012     C                   ENDIF
098900941221     C                   MOVEL     DICNIN        D1NIN
099000951012     C                   ENDIF
099100950130     C                   Z-ADD     DICIMP        D1IMP
099200950914     C                   MOVEL     DICIVA        D1IVA
099300950914     C     DICOPE        IFEQ      *ON
099400950914     C                   MOVE      XSCON         D1OPE
099500950914     C                   ELSE
099600950914     C                   MOVE      XSCOFF        D1OPE
099700950914     C                   ENDIF
099800950914     C                   Z-ADD     DICAAA        D1AAA
099900950914     C                   MOVEL     DICDIV        D1MC
100000950914     C     D1MC          IFEQ      *BLANK
100100950914     C                   MOVE      XSCDIV        D1MC
100200950914     C                   ENDIF
100300991228B0140C* se copia pulisco campi relativi alla revoca
100400991228  "  C     OPZ312        IFEQ      '03'
100500991228  "  C                   MOVE      XSCOFF        D1REV
100600991228  "  C                   MOVE      *BLANK        D1TPT
100700991228  "  C                   Z-ADD     0             D1DTR
100800991228  "  C                   ELSE
100900991228B0140C                   If        CoF312 = 'F'
101000951002     C     DICREV        IFEQ      *ON
101100951002     C                   SETON                                        10
101200951002     C                   MOVE      XSCON         D1REV
101300951002     C                   ELSE
101400951002     C                   SETOFF                                       10
101500951002     C                   MOVE      XSCOFF        D1REV
101600951002     C                   ENDIF
101700991228B0140C                   Else
101800991228  "  C                   Move      XSCOff        D1Rev
101900991228B0140C                   EndIf
102000991228     C                   MOVEL     DICTPT        D1TPT
102100991228     C                   CALLb     'XDT4000'
102200991228     C                   PARM      DICDTR        XDTAMG           10
102300991228     C                   PARM                    XDTGMA            6 0
102400991228     C                   PARM      2             XDTSTA            1 0
102500991228     C     XDTSTA        IFEQ      0
102600991228     C                   MOVE      XDTGMA        D1DTR
102700991228     C                   ENDIF
102800991228B0140C                   ENDIF
102900951002     C                   MOVEL     DICOPI        D1OPI
103000960416     C                   CALLb     'XDT4000'
103100941221     C                   PARM      DICDTC        XDTAMG           10
103200941221     C                   PARM                    XDTGMA            6 0
103300941221     C                   PARM      2             XDTSTA            1 0
103400941221     C     XDTSTA        IFEQ      0
103500941221     C                   MOVE      XDTGMA        D1DTC
103600941221     C                   ENDIF
103700960416     C                   CALLb     'XDT4000'
103800941221     C                   PARM      DICDTI        XDTAMG           10
103900941221     C                   PARM                    XDTGMA            6 0
104000941221     C                   PARM      2             XDTSTA            1 0
104100941221     C     XDTSTA        IFEQ      0
104200941221     C                   MOVE      XDTGMA        D1DTI
104300941221     C                   ENDIF
104400960416     C                   CALLb     'XDT4000'
104500941221     C                   PARM      DICDTF        XDTAMG           10
104600941221     C                   PARM                    XDTGMA            6 0
104700941221     C                   PARM      2             XDTSTA            1 0
104800941221     C     XDTSTA        IFEQ      0
104900941221     C                   MOVE      XDTGMA        D1DTF
105000941221     C                   ENDIF
105100940207     C*
105200970210     C* SALVO DATA/NR. PROTOCOLLO esterno
105300970210     C                   MOVEL     D1Dtc         DtcSAV
105400970210     C                   MOVEL     D1Nrc         NrcSAV
105500951006     C* SALVO DATA INVIO LETTERA
105600040213     C                   MOVEL     DICDIT        DITSAV           10
105700040213     c*
105800040213D1540c                   if        cof312 ='C'
105900040213  "  C                   CALLb     'XDT4000'
106000040213  "  C                   PARM      DICDIT        XDTAMG           10
106100040213  "  C                   PARM                    XDTGMA            6 0
106200040213  "  C                   PARM      2             XDTSTA            1 0
106300040213  "  C     XDTSTA        IFEQ      0
106400040213  "  C                   MOVE      XDTGMA        D1DTLET
106500040213  "  C                   ENDIF
106600040213D1540C                   ENDIF
106700951004     C*
106800940207     C                   ENDSR
106900940916     C/EJECT
107000940916     C************************************************************
107100940916     C* GESTIONE F02 VIDEO D1
107200940916     C************************************************************
107300940916     C     F02D1         BEGSR
107400940916     C*
107500940916     C* per decodificare la videata richiamo la routine dei ctrl
107600940916     C* e poi disattivo gli eventuali messaggi di errore
107700940916     C                   EXSR      CTRD1
107800940916     C*
107900940916     C                   MOVE      *ALL'0'       IN5098
108000940916     C                   MOVEA     IN5098        *IN(50)
108100940916     C*
108200940916     C                   ENDSR
108300940309     C/EJECT
108400940309     C************************************************************
108500940309     C* GESTIONE F03 VIDEO D1
108600940309     C************************************************************
108700940309     C     F03D1         BEGSR
108800940309     C*
108900940309     C                   MOVE      *ON           $FINE
109000941221     C                   MOVE      '1'           RET312
109100940325     C* fine programma
109200940325     C                   EXSR      ENDPGM
109300940325     C*
109400940325     C                   ENDSR
109500940309     C/EJECT
109600940309     C************************************************************
109700940309     C* GESTIONE F05 VIDEO D1
109800940309     C************************************************************
109900940309     C     F05D1         BEGSR
110000940309     C*
110100940309     C                   MOVE      *ON           $INZD1
110200940309     C*
110300940309     C                   ENDSR
110400940309     C/EJECT
110500940309     C************************************************************
110600940309     C* GESTIONE F08 VIDEO D1
110700940309     C************************************************************
110800940309     C     F08D1         BEGSR
110900940309     C*
111000940309     C                   MOVE      *ON           $FINE
111100941221     C                   MOVE      '0'           RET312
111200940309     C*
111300940309     C                   ENDSR
111400940309     C/EJECT
111500940309     C************************************************************
111600940309     C* GESTIONE F12 VIDEO D1
111700940309     C************************************************************
111800940309     C     F12D1         BEGSR
111900940309     C*
112000940309     C                   MOVE      *ON           $FINE
112100941221     C                   MOVE      '2'           RET312
112200940309     C*
112300940309     C                   ENDSR
112400940309     C/EJECT
112500050801B0822C************************************************************
112600050801  "  C* GESTIONE F20 VIDEO D1/D2 Annullamento revoca
112700050801B0822C************************************************************
112800050801     C     F20D1         BEGSR
112900050801     C*
113000050801   B1C     *IN22         DOUEQ     *OFF
113100050801     C     MSGRTN        ORNE      'R'
113200050801     C* blocco il record
113300050801     C     K4DIC2        CHAIN     ANDIC02                            2122
113400050801     C*
113500050801     C* Gestione RCD allocato
113600050801   B2C     *IN22         IFEQ      *ON
113700050801    >C                   MOVEL     'ANDIC00F'    MSGFIL
113800050801     C                   EXSR      MSGPGM
113900050801   E2C                   ENDIF
114000050801   E1C                   ENDDO
114100050801     C* se il rcd da gestire non viene trovato
114200050801     C* oppure è allocato da altro pgm
114300050801     C* ritorno al pgm guida avvertendolo dell'errore
114400050801   B1C     *IN21         IFEQ      *ON
114500050801     C     *IN22         OREQ      *ON
114600050801     C                   MOVE      *ON           $FINE
114700050801     C                   MOVE      '1'           ERR312
114800050801     C                   EXSR      ENDPGM
114900050801     c                   else
115000050801     c                   eval      DicRev     = *off
115100050801     c                   clear                   dictpt
115200050801     c                   move      '0001-01-01'  dicdtr
115300050801     c                   except    aggdic
115400050801     c                   endif
115500050801     c*
115600050801     C                   EXSR      GesAgg
115700050801     C*
115800050801     C                   ENDSR
115900050801     C/EJECT
116000940309     C************************************************************
116100940309     C* GESTIONE F24 VIDEO D1
116200940309     C************************************************************
116300940309     C     F24D1         BEGSR
116400940309     C*
116500940506     C                   MOVEA     $FX1          $FX
116600940506     C                   MOVEA     $FC1          $FC
116700940506     C                   MOVEA     $FR1          $FR
116800940506     C                   Z-ADD     $ULKD1        $ULK
116900940506     C                   EXSR      RIEKEY
117000940506     C                   Z-ADD     $ULK          $ULKD1            3 0
117100940720     C                   MOVEA     $FC           $FC1
117200940506     C                   MOVEL     $KEY1         Z1KE1
117300060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
117400940506     C                   MOVEL     $KEY2         Z1KE2
117500940506     C                   WRITE     FMTZ1
117600940506     C*
117700940309     C                   ENDSR
117800940207     C/EJECT
117900940131     C************************************************************
118000940207     C* CONTROLLO VIDEATA
118100940131     C************************************************************
118200940127     C     CTRD1         BEGSR
118300940131     C*
118400940127     C                   SETOFF                                       99
118500940127     C* non imposto pos. del cursore
118600940127     C                   Z-ADD     0             H1RIGA
118700940127     C                   Z-ADD     0             H1COLO
118800940223     C*
118900940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
119000940223     C*
119100941221     C     OPZ312        IFNE      '04'
119200940203     C*
119300970725     C* Nr. Protocollo ESTERNO è obbligatorio solo per i clienti
119400941221     C     D1NRC         IFEQ      *ZERO
119500970207     c     cof312        andeq     'C'
119600941221     C                   SETON                                        5099
119700941221     C                   ENDIF
119800970725     C*
119900991216B0136 **************************************************************
120000991216  "   *Sposto il ctl dopo il ctl dellla data/nr. protocollo interno
120100991216  "   *in tal modo posso reperire il numero qualore fosse = 0
120200991216  "   **************************************************************
120300991216  "  C**Nr. Protocollo INTERNO
120400991216  "  C*****D1NIN         IFEQ      *ZERO
120500991216  "  C****************** SETON                                        5399
120600991216B0136C****************** ENDIF
120700941221     C*
120800970725     C* Data Protocollo ESTERNO
120900970207     C                   MOVE      $DT0          DTCAMG
121000941221     C     D1DTC         IFNE      *ZERO
121100960416     C                   CALLb     'XDT4000'
121200941221     C                   PARM                    XDTAMG           10
121300941221     C                   PARM      D1DTC         XDTGMA            6 0
121400941221     C                   PARM      3             XDTSTA            1 0
121500941221     C     XDTSTA        IFNE      0
121600941222     C                   SETON                                        5299
121700941221     C                   ELSE
121800941221     C                   MOVE      XDTAMG        DTCAMG           10
121900941221     C                   ENDIF
122000941221     C                   ELSE
122100970725     C* (è obbligatoria solo per i clienti)
122200970207     c     cof312        ifeq      'C'
122300941222     C                   SETON                                        5199
122400970207     c                   endif
122500941221     C                   ENDIF
122600941221     C*
122700970725     C* Data Protocollo INTERNO
122800941221     C     D1DIN         IFNE      *ZERO
122900960416     C                   CALLb     'XDT4000'
123000941221     C                   PARM                    XDTAMG           10
123100941221     C                   PARM      D1DIN         XDTGMA            6 0
123200941221     C                   PARM      3             XDTSTA            1 0
123300941221     C     XDTSTA        IFNE      0
123400941222     C                   SETON                                        5599
123500941221     C                   ELSE
123600941221     C                   MOVE      XDTAMG        DINAMG           10
123700941221     C                   ENDIF
123800941221     C                   ELSE
123900941222     C                   SETON                                        5499
124000941221     C                   ENDIF
124100991216      *
124200991216B0136C* Nr. Protocollo INTERNO
124300040304B0688c                   if        opz312='01' or opz312='03'
124400040301B0688X**** D1NIN         IFEQ      *ZERO
124500040315B0136 *
124600991216  "   * Se la data Data/nr. protocollo interno è valorizzata
124700991216  "  C     DINAMG        IFNE      $DT0
124800991216  "   *  reperisco il nr. protocollo interno
124900991216  "  C                   EXSR      REPNUM
125000991216  "   *
125100991216  "   * Se la data Data/nr. protocollo interno NON è valorizzata
125200991216  "  C                   Else
125300991216  "   *  nr. protocollo interno obbligatorio
125400040315B0136C                   SETON                                        5399
125500040315B0688c                   ENDIF
125600040315B0136 *
125700991216B0136C                   EndIf
125800951004     C*
125900970725     C* CTRL UNIVOCITA' DATA/NR. PROTOCOLLO ESTERNO
126000970725     C* (Solo per i clienti !!!)
126100970725     C     cof312        IFEQ      'C'
126200970210     C     D1Dtc         IFNE      DtcSAV
126300970210     C     D1Nrc         ORNE      NrcSAV
126400970210     C                   MOVEL     WWWSOC        DICSOC
126500970210     C                   MOVEL     COF312        DICCF
126600970210     C                   CALLb     'XALLINEA'
126700970210     C                   PARM                    D1KSC
126800970210     C                   PARM      8             LEN               5 0
126900970210     C                   PARM                    XSCINP
127000970210     C                   MOVEL     D1KSC         DICKSC
127100970210     C                   MOVEL     DtcAMG        DICdtc
127200970210     C                   MOVEL     D1nrc         DICnrc
127300970210     C     K5DIC1        setll     ANDIC01l                               21
127400970725     C     *IN21         IFEQ      *ON
127500970725     C                   SETON                                        7599
127600970210     C                   ENDIF
127700970210     C                   ENDIF
127800970725     C                   ENDIF
127900950914     C*
128000950915     C* CTRL VALIDITA' DATA REVOCA
128100950915     C                   MOVEL     $DT0          DTRAMG
128200950914     C     D1DTR         IFNE      *ZERO
128300960416     C                   CALLb     'XDT4000'
128400950914     C                   PARM                    XDTAMG           10
128500950914     C                   PARM      D1DTR         XDTGMA            6 0
128600950914     C                   PARM      3             XDTSTA            1 0
128700950914     C     XDTSTA        IFNE      0
128800950914     C                   SETON                                        6599
128900950914     C                   ELSE
129000950914     C                   MOVE      XDTAMG        DTRAMG           10
129100950914     C                   ENDIF
129200950914     C                   ENDIF
129300040213     C*
129400040213D1540C* CTRL VALIDITA' Data ricevimento lettera solo per clienti
129500040213  "  C                   MOVEL     $DT0          $DTLAMG
129600040213  "  c                   if        Cof312='C'
129700040213  "  C     D1DtLet       IFNE      *ZERO
129800040213  "  C                   CALLb     'XDT4000'
129900040213  "  C                   PARM                    XDTAMG           10
130000040213  "  C                   PARM      D1DtLet       XDTGMA            6 0
130100040213  "  C                   PARM      3             XDTSTA            1 0
130200040213  "  C     XDTSTA        IFNE      0
130300040213  "  C                   SETON                                        5999
130400040213  "  C                   ELSE
130500040213  "  C                   MOVE      XDTAMG        $DTLAMG          10
130600040213  "  C                   ENDIF
130700040213  "  C                   ENDIF
130800040213D1540C                   endif
130900950914     C*
131000950914     C* CTRL IVA
131100950914     C                   MOVEL     *BLANK        D1IVAD
131200950914     C     D1IVA         IFNE      *BLANK
131300950914     C                   CLEAR                   X06DS
131400950914     C                   MOVE      WWWSOC        X06SOC
131500950914     C                   MOVE      XSCLIN        X06LIN
131600950914     C                   MOVE      D1IVA         X06CIV
131700950914     C                   MOVE      WDATE         X06DAT
131800950914     C                   CALL      'X06G48'
131900950914     C                   PARM                    X06DS
132000950914     C     X06ERR        IFEQ      '0'
132100950914     C                   MOVEL     X06UNI        TABG48
132200950914     C                   MOVEL     DESG48        D1IVAD
132300950914     C                   ELSE
132400950914     C                   SETON                                        6199
132500950914     C                   ENDIF
132600950914     C                   ELSE
132700950914     C                   SETON                                        6099
132800950914     C                   ENDIF
132900951002     C*
133000951002     C* CTRL OPERAZ. IMPORTAZIONE
133100951002     C                   MOVEL     *BLANK        D1OPID
133200951002     C     D1OPI         IFNE      *BLANK
133300951002     C                   MOVE      XCACUT        XTBUTE
133400951002     C                   MOVE      '1'           XTBRIC
133500951002     C                   MOVE      XSCSOC        XTBAZI
133600951002     C                   MOVE      *ZERO         XTBKEY
133700951002     C                   MOVE      'G17'         XTBCOD
133800951002     C                   MOVE      D1OPI         XTBKEY
133900961004     C                   CALLb     'XATB'
134000951002     C                   PARM                    XATBDS
134100951002     C     XTBERR        IFEQ      *ON
134200951002     C                   SETON                                        7399
134300951002     C                   ELSE
134400951002     C                   MOVEL     XTBUNI        TABG17
134500951002     C                   MOVEL     DESG17        D1OPID
134600951002     C                   ENDIF
134700951002     C                   ELSE
134800951002     C                   SETON                                        7299
134900951002     C                   ENDIF
135000950914     C*
135100950915     C* CTRL VALIDITA' DATE INIZIO/FINE
135200950914     C                   MOVEL     $DT0          DTIAMG
135300950914     C                   MOVEL     $DT0          DTFAMG
135400950914     C     D1DTI         IFNE      *ZERO
135500960416     C                   CALLb     'XDT4000'
135600950914     C                   PARM                    XDTAMG           10
135700950914     C                   PARM      D1DTI         XDTGMA            6 0
135800950914     C                   PARM      3             XDTSTA            1 0
135900950914     C     XDTSTA        IFNE      0
136000950914     C                   SETON                                        5799
136100950914     C                   ELSE
136200950914     C                   MOVE      XDTAMG        DTIAMG           10
136300950914     C                   ENDIF
136400950914     C                   ENDIF
136500950914     C*
136600950914     C     D1DTF         IFNE      *ZERO
136700960416     C                   CALLb     'XDT4000'
136800950914     C                   PARM                    XDTAMG           10
136900950914     C                   PARM      D1DTF         XDTGMA            6 0
137000950914     C                   PARM      3             XDTSTA            1 0
137100950914     C     XDTSTA        IFNE      0
137200950914     C                   SETON                                        6499
137300950914     C                   ELSE
137400950914     C                   MOVE      XDTAMG        DTFAMG           10
137500950914     C                   ENDIF
137600950914     C                   ENDIF
137700950914     C*
137800950914     C     D1DTI         IFNE      0
137900950914     C     D1DTF         ANDEQ     0
138000950914     C     D1DTI         OREQ      0
138100950914     C     D1DTF         ANDNE     0
138200950914     C                   SETON                                        5699
138300950914     C                   ENDIF
138400950914     C*
138500950914     C     *IN56         IFEQ      *OFF
138600950914     C     *IN57         ANDEQ     *OFF
138700950914     C     *IN64         ANDEQ     *OFF
138800950914     C     DTIAMG        IFGT      DTFAMG
138900950914     C                   SETON                                        5899
139000950914     C                   ENDIF
139100950914     C                   ENDIF
139200941215     C*
139300950915     C     COF312        IFEQ      'F'
139400951004     C*
139500950915     C* CTRL FLAG STAMPA LETTERA REVOCA
139600950915     C     D1REV         IFNE      XSCON
139700950915     C     D1REV         ANDNE     XSCOFF
139800950915     C                   SETON                                        7199
139900950915     C                   ENDIF
140000950915     C*
140100950915     C* CTRL TIPO TESTO REVOCA
140200950914     C                   MOVE      *BLANK        D1TPTD
140300950914     C     D1TPT         IFNE      *BLANK
140400000519A0778X***                MOVE      WWWSOC        TXTSOC
140500000519  "  X***                MOVE      XSCDUN        TXTUNI
140600000519  "  X***                MOVE      D1TPT         TXTTPT
140700000519  "  X***                MOVE      XSCLIN        TXTLIN
140800000519  "  X***                MOVE      'A'           TXTPOS
140900000519  "  X***  K5TXT1        CHAIN     ANTXT01L                           21
141000000519  "  X***  *IN21         IFEQ      *OFF
141100000519  "  X***                MOVEL     TXTTES        D1TPTD
141200000519  "   *
141300000519  "  C                   eval      XEsito = *Off
141400000519  "  C                   eval      XOpe = '6'
141500000519  "  C                   eval      XSocieta = WWWSoc
141600000523  "  C                   eval      XUnita   = *blank
141700000519  "  C                   eval      XTpTesto = D1TpT
141800000519  "  C                   eval      XLingua  = XScLin
141900000523  "  C                   eval      XClaUte = *blanks
142000000519  "  C                   eval      XPosRig = 'A'
142100000519  "  C                   eval      XNrRiga = *zeros
142200000519  "  C                   eval      XRibat = *blanks
142300000519  "  C                   eval      XTesRig = *blanks
142400000519  "  C                   eval      XNrChiavi = 4
142500000519  "  C                   eval      XUsoUnita = '0'
142600000519  "   *
142700000519  "  C                   exsr      CallX72
142800000519  "   *
142900000519  "  C                   if        XEsito = *off
143000000519A0778C                   movel     XTesRig       D1TpTD
143100950914     C                   ELSE
143200950914     C                   SETON                                        6799
143300950914     C                   ENDIF
143400950914     C                   ENDIF
143500950915     C*
143600950915     C     D1REV         IFEQ      XSCON
143700950915     C     D1TPT         ANDEQ     *BLANK
143800950915     C                   SETON                                        6299
143900950915     C                   ENDIF
144000950915     C*
144100950915     C     D1REV         IFEQ      XSCON
144200950915     C     D1DTR         ANDEQ     0
144300950915     C                   SETON                                        6399
144400950915     C                   ENDIF
144500950915     C*
144600950915     C                   ENDIF
144700950914     C*
144800950915     C* CTRL FLAG OPERAZIONE SPECIFICATA
144900950914     C     D1OPE         IFNE      XSCON
145000950914     C     D1OPE         ANDNE     XSCOFF
145100950914     C                   SETON                                        6699
145200950914     C                   ENDIF
145300950914     C*
145400950914     C* CTRL ANNO/IMPORTO
145500950914     C     D1AAA         IFNE      0
145600950914     C     D1IMP         ANDEQ     0
145700950914     C     D1AAA         OREQ      0
145800950914     C     D1IMP         ANDNE     0
145900950914     C                   SETON                                        6899
146000950914     C                   ENDIF
146100950914     C*
146200950914     C* CTRL VALORI ALTERNATIVI
146300950914     C     D1OPE         IFEQ      XSCOFF
146400950914     C     D1AAA         ANDEQ     0
146500950914     C     D1DTI         ANDEQ     0
146600950914     C                   SETON                                        6999
146700950914     C                   ELSE
146800950914     C     D1OPE         IFEQ      XSCON
146900950914     C     D1AAA         ANDNE     0
147000950914     C                   SETON                                        7099
147100950914     C                   ELSE
147200950914     C     D1OPE         IFEQ      XSCON
147300950914     C     D1DTI         ANDNE     0
147400950914     C                   SETON                                        7099
147500950914     C                   ELSE
147600950914     C     D1AAA         IFNE      0
147700950914     C     D1DTI         ANDNE     0
147800950914     C                   SETON                                        7099
147900950914     C                   ENDIF
148000950914     C                   ENDIF
148100950914     C                   ENDIF
148200950914     C                   ENDIF
148300041202B0762c* controllo congruenze
148400041202  "  c* Anno data protocollo interno deve essere uguale ad anno valore
148500041202  "  c* dichiarazione se immesso
148600041202  "  c                   move      DinAmg        DataIso
148700041202  "  c                   extrct    DataIso:*Y    AnnoDic           4 0
148800041202  "  c                   if        D1AAA <> 0
148900041202  "  c                   if        AnnoDic <> D1AAA
149000041202  "  c                   seton                                        7499
149100041202  "  c                   endif
149200041202  "  c                   endif
149300041202  "  c* Anno data protocollo interno deve essere uguale ad anno periodo
149400041202  "  c* da .... a...  se immesso
149500041202  "  c                   if        D1Dti <> 0
149600041202  "  c                   move      DtiAmg        DataIso
149700041202  "  c                   extrct    DataIso:*Y    AnnoDTI           4 0
149800041202  "  c                   if        AnnoDic <> AnnoDti
149900041202  "  c                   seton                                        7699
150000041202  "  c                   endif
150100041202  "  c                   move      DtfAmg        DataIso
150200041202  "  c                   extrct    DataIso:*Y    AnnoDTF           4 0
150300041202  "  c                   if        AnnoDic <> AnnoDtF
150400041202  "  c                   seton                                        7699
150500041202  "  c                   endif
150600041202B0762c                   endif
150700061013     c*
150800041202     c*
150900970210     C*
151000970210     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
151100970210     C     OPZ312        IFEQ      '01'
151200970210     C     OPZ312        OREQ      '03'
151300970210     C                   MOVEL     WWWSOC        DICSOC
151400970210     C                   MOVEL     COF312        DICCF
151500970725     C                   MOVEL     D1NIN         DICNIN
151600970725     C                   MOVEL     DINAMG        DICDIN
151700970725     C     K4DIC2        SETLL     ANDIC02                                21
151800970210     C     *IN21         IFEQ      '1'
151900970725     C                   SETON                                        9099
152000970210     C                   ENDIF
152100970210     C                   ENDIF
152200970210     C*
152300941027     C                   ENDIF
152400940127     C*
152500940117     C                   ENDSR
152600940224     C/EJECT
152700940224     C************************************************************
152800940315     C* AGGIORNAMENTO ANAGRAFICA
152900940224     C************************************************************
153000941214     C     AGGANA        BEGSR
153100940207     C*
153200940207     C                   SELECT
153300941221     C     OPZ312        WHENEQ    '01'
153400941221     C     OPZ312        OREQ      '03'
153500941214     C                   EXSR      RIEANA
153600970725     C                   WRITE     ANDIC02                              22
153700940318     C* se rcd non scrivibile attivo errore
153800940318     C     *IN22         IFEQ      *ON
153900941201     C                   SETON                                        9599
154000941027     C                   ENDIF
154100971002     C*
154200941221     C     OPZ312        WHENEQ    '02'
154300941214     C                   EXSR      RIEANA
154400970725     C                   UPDATE    ANDIC02
154500971002     C*
154600941221     C     OPZ312        WHENEQ    '04'
154700970725     C                   DELETE    ANDIC02
154800940207     C                   ENDSL
154900950323      *
155000950323      * richiama pgm User Exit
155100950323      *
155200950323     C     USRERR        IFEQ      *BLANK
155300950323     C     *IN99         ANDEQ     *OFF
155400950323      * imposta chiave univoca  per user exit
155500950323     C                   MOVE      XSCSOC        XUKSOC
155600950323     C                   MOVE      XSCDUN        XUKDUN
155700950323     C                   MOVEL     NOMPGM        XUKPGM
155800950323     C                   MOVEL     DICSOC        XUKFL1
155900950323     C                   MOVEL     DICKSC        XUKFL2
156000950323     C                   MOVEL     DICNRC        XUKFL3
156100950323     C                   MOVEL     DICDTC        XUKFL4
156200950323     C                   MOVEL     XUKDS         KPJBU
156300950323     C     NOMPGM        CAT       'U':0         USREXT           10
156400950323     C                   CALL      USREXT                               21
156500950323     C                   PARM                    KPJBA
156600950323     C     *IN21         IFEQ      *ON
156700950323     C                   MOVE      *ON           USRERR            1
156800950323     C                   ENDIF
156900950323     C                   ENDIF
157000940203     C*
157100940203     C                   ENDSR
157200940224     C/EJECT
157300940224     C************************************************************
157400940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
157500940224     C************************************************************
157600941214     C     RIEANA        BEGSR
157700951006     C*
157800970725     C                   CLEAR                   ANDIC02
157900951006     C*
158000950110     C                   MOVEL     WWWSOC        DICSOC
158100941221     C                   MOVEL     D1KSC         DICKSC
158200941221     C                   MOVEL     D1NRC         DICNRC
158300941221     C                   MOVEL     DTCAMG        DICDTC
158400951012     C                   MOVEL     D1NIN         DICNIN
158500941221     C                   MOVEL     DINAMG        DICDIN
158600941221     C                   MOVEL     DTIAMG        DICDTI
158700941221     C                   MOVEL     DTFAMG        DICDTF
158800941221     C                   MOVEL     D1IVA         DICIVA
158900950130     C                   Z-ADD     D1IMP         DICIMP
159000950914     C     D1OPE         IFEQ      XSCON
159100950914     C                   MOVE      *ON           DICOPE
159200950914     C                   ELSE
159300950914     C                   MOVE      *OFF          DICOPE
159400950914     C                   ENDIF
159500950914     C                   Z-ADD     D1AAA         DICAAA
159600950914     C                   MOVE      D1MC          DICDIV
159700950914     C                   MOVE      COF312        DICCF
159800951002     C                   MOVE      D1OPI         DICOPI
159900040213D1540X***                MOVE      DITSAV        DICDIT
160000040213  "  c* fornitori
160100040213  "  C   11              MOVE      DITSAV        DICDIT
160200040213  "  c* clienti
160300040213D1540C  N11              move      $DTLAmg       DicDit
160400040213     C*
160500991228B0140C**** OPZ312        IFEQ      '03'
160600991228  "  C****               MOVE      $DT0          DICDTR
160700991228  "  C****               MOVE      *BLANK        DICTPT
160800991228  "  C****               MOVE      *OFF          DICREV
160900991228B0140C****               ELSE
161000951011     C                   MOVE      DTRAMG        DICDTR
161100951011     C                   MOVE      D1TPT         DICTPT
161200991228B0140C* se fornitori il flag di revoca è deciso a video
161300991228B0140C                   If        CoF312 = 'F'
161400951011     C     D1REV         IFEQ      XSCON
161500951011     C                   MOVE      *ON           DICREV
161600951011     C                   ELSE
161700951011     C                   MOVE      *OFF          DICREV
161800951011     C                   ENDIF
161900991228B0140C* se clienti il flag di revoca è deciso in base alla data revoca
162000991228  "  C                   Else
162100991228  "  C                   If        DICDtR <> $DT0
162200991228  "  C                   Move      *On           DICRev
162300991228  "  C                   Else
162400991228  "  C                   Move      *Off          DICRev
162500991228  "  C                   EndIf
162600991228  "  C                   EndIf
162700991228B0140C****               ENDIF
162800940207     C*
162900940207     C                   ENDSR
163000940224     C/EJECT
163100951012     C************************************************************
163200951012     C* REPERIMENTO NR. PROTOCOLLO INTERNO
163300951012     C************************************************************
163400951012     C     REPNUM        BEGSR
163500951012     C*
163600040301B0688C     D1NIN         IFEQ      *ZERO
163700951012     C                   MOVE      '3'           XNMRIC
163800040301B0688C                   ELSE
163900040301  "  C                   move      '1'           Xnmric
164000040301B0688C                   ENDIF
164100951012     C                   MOVE      XSCSOC        XNMSOC
164200951012     C                   MOVE      *BLANKS       XNMUNI
164300951012     C                   MOVE      *BLANKS       XNMCTB
164400951012     C                   MOVE      *BLANKS       XNMKEY
164500951012     C     COF312        IFEQ      'C'
164600951012     C                   MOVE      'PCLI'        XNMSER
164700951012     C                   ELSE
164800951012     C                   MOVE      'PFOR'        XNMSER
164900951012     C                   ENDIF
165000960416     C                   MOVE      *ON           XNMCMT
165100951012     C                   MOVE      *OFF          XNMIVA
165200951012     C                   MOVE      *OFF          XNMREG
165300991216B0136C****************** MOVE      WDATE         XNMDAT
165400991216  "   *WDate è l'udate in formato 10 alfa
165500991216  "   *per reprire il numero, ho bisogno della data protocollo
165600991216  "   *a video trasformata in una data di 10 alfa
165700991216B0136C                   MOVE      DINAMG        XNMDAT
165800030704B0581c* stessa data per aggiornamento numeratore
165900030704B0581C                   MOVE      DINAMG        XNMDAG
166000961004     C                   CALLb     'XNMR'        PNMR
166100951012     C     XNMERR        IFEQ      *OFF
166200061031B0907c                   if        xnmric='3'
166300951012     C                   Z-ADD     XNMNUM        D1NIN
166400061031B0907c                   endif
166500960416     C                   ELSE
166600030704B0581c                   seton                                        99
166700030704  "  x***                MOVE      *ON           $FINE
166800030704B0581x***                EXSR      ENDPGM
166900951012     C                   END
167000951012     C*
167100951012     C                   ENDSR
167200951012     C/EJECT
167300061013B0907C************************************************************
167400061013     C* Controllo numero univoco all'interno dell'anno solare
167500061013B0907C************************************************************
167600061013     C     Ctrnum        BEGSR
167700061013     C*
167800061013     C                   MOVEL     WWWSOC        DICSOC
167900061013     C                   MOVEL     COF312        DICCF
168000090224B1090x****************** MOVEL     KSC312        DICKSC
168100061013     C     *Jobrun       move      D1din         DataIso
168200061013     c                   extrct    DataIso:*Y    DicAIn
168300061013     c                   move      D1Nin         DicNIn
168400090224B1090x**** k05dic05      chain     Andic05l
168500090224B1090c     k04dic05      chain     Andic05l
168600061013     c                   if        %found(andic05l)
168700061013     c*
168800061013     C                   clear                   XMsgDS
168900061013     C                   clear                   XMsgTxt
169000061013     C                   movel     0             MsgMsg
169100061013     C                   movel     'N'           MsgRsp
169200061013     C                   movel     'S'           MsgEmv
169300061013     C                   z-add     10            MsgRgp
169400061013     C                   movel     'N'           MsgLck
169500061013     C                   movel     'N'           MsgCnl
169600061013     C                   movel     'N'           MsgVid
169700061013     C                   movel     'N'           MsgOpe
169800061013     C                   movel     'N'           MsgStp
169900061013      *
170000061013     C                   call      'XMSGR'
170100061013     C                   parm                    XMsgDS
170200061013     C                   parm                    XMsgTxt         135
170300061031     C                   parm      '1'           XMsgInz           1
170400061013     C                   parm      'COS2645'     XMsgCd1           7
170500061013     C                   parm                    XMsgCd2           7
170600061013     C                   parm                    XMsgCd3           7
170700061013     C                   parm                    XMsgSn1           7
170800061013     C                   parm                    XMsgCdTxT        63
170900061013     C                   parm                    MsgCdTit         14
171000061013     C                   parm                    MsgClPrtf         1
171100061013     C                   parm                    VarMsg1         100
171200061013     C                   parm                    VarMsg2         100
171300061013     C                   parm                    VarMsg3         100
171400061013     c                   endif
171500061013     c*
171600061013B0907C                   ENDSR
171700940224     C************************************************************
171800940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
171900940224     C************************************************************
172000940224     C     GESAGG        BEGSR
172100980223     C*
172200980223     C**Eseguo COMMIT per "liberare" il numeratore
172300980223     C**(se sono in immissione o copia)
172400980223     C     OPZ312        IFEQ      '01'
172500980223     C     OPZ312        OREQ      '03'
172600980223     C                   COMMIT
172700980223     C                   ENDIF
172800940224     C*
172900940224     C* segnalo al pgm chiamante l'avvenuta conferma
173000941221     C                   MOVE      *ON           OPR312
173100940224     C* segnalo al pgm chiamante l'aver premuto l'F6
173200941221     C                   MOVE      '0'           RET312
173300940224     C*
173400951002     C* Stampa lettera revoca
173500951003     C     D1REV         IFEQ      XSCON
173600990507B0013C**** *IN10         ANDEQ     *OFF
173700951003     C                   EXSR      REVOCA
173800951002     C                   ENDIF
173900960416     C*
174000960416     C                   SELECT
174100960416     C*
174200960416     C* nel caso di immissione
174300960416     C     OPZ312        WHENEQ    '01'
174400960416     C* ripristino la videata iniziale
174500960416     C                   MOVE      *ON           $INZD1
174600960416     C                   MOVE      'D1'          $GEST
174700960416     C*
174800960416     C* per tutte le altre opzioni
174900960416     C                   OTHER
175000960416     C* ritorno al pgm chiamante
175100960416     C                   MOVE      *ON           $FINE
175200960416     C                   ENDSL
175300951002     C*
175400940224     C                   ENDSR
175500941205     C/EJECT
175600951003     C******************************************************
175700951003     C* STAMPA LETTERA REVOCA                              *
175800951003     C******************************************************
175900951003     C     REVOCA        BEGSR
176000951003     C*
176100000518B0212C                   Open      ANA312P
176200000518     C*
176300951003     C* Testata
176400951003     C                   MOVE      XSCCIT        CITTA
176500081223     c*
176600081223D2461C* la stampa lettera revoca è solo per fornitori, reperisco
176700081223  "  c* i dati dell'indirizzo lettera come fa il programma di stampa
176800081223  "  c* lettera NDC401R
176900081223D2461C                   EXSR      DATFOR
177000081223     c*
177100081223D2461X*                  MOVE      D1KSCD        RAGSOC
177200081223  "  X*                  MOVE      D1IND         INDIRI
177300081223  "  X*                  CLEAR                   CAPLOCPRV
177400081223  "  X*                  MOVEL     D1CAP         CAPLOCPRV
177500081223  "  X*                  CAT       D1LOC:1       CAPLOCPRV
177600081223D2461X*                  CAT       D1PRV:1       CAPLOCPRV
177700971204     C                   Z-ADD     D1DTR         DTREV
177800990507B0013C*****              Z-ADD     D1DTC         DTPROT
177900990507  "  C*****              Z-ADD     D1NRC         NRPROT
178000990507  "  C                   Z-ADD     D1DIN         DTPROT
178100990507B0013C                   Z-ADD     D1NIN         NRPROT
178200951003     C                   WRITE     TESTA
178300951003     C*
178400000519A0778X* Righe testo
178500000519  "  X***                MOVE      WWWSOC        TXTSOC
178600000519  "  X***                MOVE      XSCDUN        TXTUNI
178700000519  "  X***                MOVE      D1TPT         TXTTPT
178800000519  "  X***                MOVE      XSCLIN        TXTLIN
178900000519  "  X***                MOVE      'A'           TXTPOS
179000000519  "  X***  K5TXT1        SETGT     ANTXT01L
179100000519  "  X***  K4TXT1        READE     ANTXT01L                               21
179200000519  "  X***  *IN21         DOWEQ     *OFF
179300000519  "  X***                MOVEL     TXTTES        RIGA
179400000519  "  X***  TXTRIB        IFEQ      *ON
179500000519  "   *
179600000519  "   * SetGt su ANTXT01L con l'unità trovata nel record avente
179700000519  "   * tipo riga = 'A'
179800000519  "  C                   eval      XEsito = *Off
179900000519  "  C                   eval      XOpe = '5'
180000000519  "  C                   eval      XSocieta = WWWSoc
180100000523  "  C                   eval      XUnita = *blank
180200000519  "  C                   eval      XTpTesto = D1TpT
180300000519  "  C                   eval      XLingua = XScLin
180400000523  "  C                   eval      XClaUte = *blanks
180500000519  "  C                   eval      XPosRig = 'A'
180600000519  "  C                   eval      XNrRiga = *zeros
180700000519  "  C                   eval      XRibat = *blanks
180800000519  "  C                   eval      XTesRig = *blanks
180900000519  "  C                   eval      XNrChiavi = 5
181000000519  "  C                   eval      XUsoUnita = '0'
181100000519  "   *
181200000519  "  C                   exsr      CallX72
181300000519  "   *
181400000519  "  C                   dow       XEsito = *Off
181500000519  "   *
181600000519  "   * Reade su ANTXT01L per trovare i record successivi a quello con
181700000519  "   * PosRig = 'A'
181800000519  "  C                   eval      XEsito = *Off
181900000519  "  C                   eval      XOpe = '4'
182000000523  "  C                   eval      XClaUte = *blank
182100000523  "  C                   eval      XPosRig = *blank
182200000519  "  C                   eval      XNrRiga = *zeros
182300000519  "  C                   eval      XRibat = *blanks
182400000519  "  C                   eval      XTesRig = *blanks
182500000519  "  C                   eval      XNrChiavi = 4
182600000519  "  C                   eval      XUsoUnita = '0'
182700000519  "   *
182800000519  "  C                   exsr      CallX72
182900000519  "   *
183000000519  "  C                   if        XEsito = *Off
183100000519  "  C                   movel     XTesRig       Riga
183200000519A0778C     XRibat        ifeq      *on
183300951003     C                   SETON                                        80
183400951003     C                   ELSE
183500951003     C                   SETOFF                                       80
183600951003     C                   ENDIF
183700951003     C                   WRITE     TXTREV
183800000519A0778X***  K4TXT1        READE     ANTXT01L                               21
183900000519A0778C                   endif
184000951003     C                   ENDDO
184100951003     C*
184200000518B0212C                   Close     ANA312P
184300000518     C*
184400951003     C                   ENDSR
184500951003     C/EJECT
184600081223D2461C************************************************************
184700081223     C* DATI FORNITORE
184800081223D2461C************************************************************
184900081223     C     DATFOR        BEGSR
185000081223     C*
185100081223     C                   setoff                                       85
185200081223     C                   clear                   ragsoc
185300081223     C                   clear                   indiri
185400081223     C                   clear                   CapLocPrv
185500081223     C                   clear                   WCap
185600081223     C                   clear                   WLocali
185700081223     C                   clear                   WProv
185800081223     C                   clear                   dvfpartiva
185900081223     C                   clear                   dvfkcc
186000081223     C                   clear                   dvfksc
186100081223     C*
186200081223     C                   move      '1'           DVARIC
186300081223     C                   move      xscsoc        DVASOCIETA
186400081223     C                   move      *blank        DVASTRUTT
186500081223     C                   movel     'DVAFOR'      DVASTRUTT
186600081223     C                   eval      DVALENOUT = %Size(DVAFOR)
186700081223     C                   move      dicksc        DVACODICE
186800081223     C                   move      'F'           DVASNATURA
186900081223     C                   clear                   DVALINEAV
187000081223     C                   clear                   DVAFILIALE
187100081223     C                   clear                   DVATPIND
187200081223     C                   clear                   DVACDIND
187300081223     C*
187400081223     C                   CALLB     'NDDVASOG'    PDVASOG
187500081223     C*
187600081223   1bC     DVAERRORE     ifeq      *off
187700081223     C                   eval      %subst(DVAFOR:1:DVALENOUT)
187800081223     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
187900081223     C*
188000081223   2bC                   if        dvfkcca <> *blank and dvfksca <> *blank
188100081223     C* Percipiente
188200081223     C                   clear                   WCognNome
188300081223   3bC                   if        DvfCognome <> *blank
188400081223     C                   seton                                        85
188500081223     c*
188600081223     c                   movel     DvfCognome    WCognNome
188700081223     c     WCognNome     Cat       DvfNome:1     WCognNome
188800081223     c                   movel     WCognNome     RagSoc
188900081223     c*
189000081223   3xC                   else
189100081223     C                   Clear                   RagSoc
189200081223     C                   movel     dvfdes        ragsoc
189300081223   3eC                   endif
189400081223     C*
189500081223     C* Indirizzo percipiente diverso
189600081223   3bC                   if        dvftpinda<>*blank and dvfcdinda<>*blank
189700081223     C                   move      '1'           DVARIC
189800081223     C                   move      xscsoc        DVASOCIETA
189900081223     C                   move      *blank        DVASTRUTT
190000081223     C                   movel     'ANIND'       DVASTRUTT
190100081223     C                   eval      DVALENOUT = %Size(ANIND)
190200081223     C                   move      dicksc        DVACODICE
190300081223     C                   move      'F'           DVASNATURA
190400081223     C                   clear                   DVALINEAV
190500081223     C                   clear                   DVAFILIALE
190600081223     C                   move      dvftpinda     DVATPIND
190700081223     C                   move      dvfcdinda     DVACDIND
190800081223     C                   CALLB     'NDDVASOG'    PDVASOG
190900081223   4bC     DVAERRORE     ifeq      *off
191000081223     C                   eval      %subst(ANIND:1:DVALENOUT)
191100081223     C                              = %subst(DVAOUTPUT:1:DVALENOUT)
191200081223     C                   movel     $ndindriz     indiri
191300081223     C                   movel     $ndCap        WCap
191400081223     C                   movel     $ndLocalit    WLocali
191500081223     C                   movel     $ndProv       WProv
191600081223   4eC                   endif
191700081223   3xC                   else
191800081223     C                   movel     dvfindriz     indiri
191900081223     C                   movel     dvfcap        WCap
192000081223     C                   movel     dvflocalit    WLocali
192100081223     C                   movel     dvfprov       WProv
192200081223   3eC                   endif
192300081223     C*
192400081223   2xC                   else
192500081223     C* Solo fornitore
192600081223     c                   Clear                   RagSoc
192700081223     C                   movel     dvfdes        ragsoc
192800081223     C                   movel     dvfindriz     indiri
192900081223     C                   movel     dvfcap        WCap
193000081223     C                   movel     dvflocalit    WLocali
193100081223     C                   movel     dvfprov       WProv
193200081223   2eC                   endif
193300081223     C*
193400081223     c                   clear                   WCap5             5
193500081223     c                   movel     WCap          WCap5
193600081223   2bC                   if        WCap5 <> *Blank and WCap5 <> *All'0'
193700081223     c                   movel     WCap          CapLocPrv
193800081223     c     CapLocPrv     Cat       WLocali:1     CapLocPrv
193900081223   2xc                   else
194000081223     c                   movel     WLocali       CapLocPrv
194100081223   2ec                   endif
194200081223   2bC                   if        CapLocPrv <> *Blank
194300081223     c     CapLocPrv     Cat       WProv:1       CapLocPrv
194400081223   2xc                   else
194500081223     c                   movel     WProv         CapLocPrv
194600081223   2ec                   endif
194700081223     C*
194800081223   1eC                   endif
194900081223     C*
195000081223     C*
195100081223D2461C                   ENDSR
195200000519A0778C************************************************************
195300000519  "  C* Chiamata al driver di lettura ANTXT00F
195400000519  "  C************************************************************
195500000519A0778C     CallX72       begsr
195600000519      *
195700000519     C                   call      'X72TXT'
195800000519     C                   parm                    XEsito
195900000519     C                   parm                    XOpe
196000000519     C                   parm                    XSocieta
196100000519     C                   parm                    XTpTesto
196200000519     C                   parm                    XUnita
196300000519     C                   parm                    XLingua
196400000523     C                   parm                    XClaUte
196500000519     C                   parm                    XPosRig
196600000519     C                   parm                    XNrRiga
196700000519     C                   parm                    XRibat
196800000519     C                   parm                    XTesRig
196900000519     C                   parm                    XNrChiavi
197000000519     C                   parm                    XUsoUnita
197100000519      *
197200000519A0778C                   endsr
197300941205     C************************************************************
197400941205     C* CONTROLLO ABILITAZIONE TASTI
197500941205     C************************************************************
197600941205     C     CTRKEY        BEGSR
197700941205     C*
197800941205     C                   SELECT
197900941205     C     $TASTO        WHENEQ    F01
198000941205     C     $FC(1)        IFEQ      '0'
198100941205     C                   SETON                                        99
198200941205     C                   ENDIF
198300941205     C     $TASTO        WHENEQ    F02
198400941205     C     $FC(2)        IFEQ      '0'
198500941205     C                   SETON                                        99
198600941205     C                   ENDIF
198700941205     C     $TASTO        WHENEQ    F03
198800941205     C     $FC(3)        IFEQ      '0'
198900941205     C                   SETON                                        99
199000941205     C                   ENDIF
199100941205     C     $TASTO        WHENEQ    F04
199200941205     C     $FC(4)        IFEQ      '0'
199300941205     C                   SETON                                        99
199400941205     C                   ENDIF
199500941205     C     $TASTO        WHENEQ    F05
199600941205     C     $FC(5)        IFEQ      '0'
199700941205     C                   SETON                                        99
199800941205     C                   ENDIF
199900941205     C     $TASTO        WHENEQ    F06
200000941205     C     $FC(6)        IFEQ      '0'
200100941205     C                   SETON                                        99
200200941205     C                   ENDIF
200300941205     C     $TASTO        WHENEQ    F07
200400941205     C     $FC(7)        IFEQ      '0'
200500941205     C                   SETON                                        99
200600941205     C                   ENDIF
200700941205     C     $TASTO        WHENEQ    F08
200800941205     C     $FC(8)        IFEQ      '0'
200900941205     C                   SETON                                        99
201000941205     C                   ENDIF
201100941205     C     $TASTO        WHENEQ    F09
201200941205     C     $FC(09)       IFEQ      '0'
201300941205     C                   SETON                                        99
201400941205     C                   ENDIF
201500941205     C     $TASTO        WHENEQ    F10
201600941205     C     $FC(10)       IFEQ      '0'
201700941205     C                   SETON                                        99
201800941205     C                   ENDIF
201900941205     C     $TASTO        WHENEQ    F11
202000941205     C     $FC(11)       IFEQ      '0'
202100941205     C                   SETON                                        99
202200941205     C                   ENDIF
202300941205     C     $TASTO        WHENEQ    F12
202400941205     C     $FC(12)       IFEQ      '0'
202500941205     C                   SETON                                        99
202600941205     C                   ENDIF
202700941205     C     $TASTO        WHENEQ    F13
202800941205     C     $FC(13)       IFEQ      '0'
202900941205     C                   SETON                                        99
203000941205     C                   ENDIF
203100941205     C     $TASTO        WHENEQ    F14
203200941205     C     $FC(14)       IFEQ      '0'
203300941205     C                   SETON                                        99
203400941205     C                   ENDIF
203500941205     C     $TASTO        WHENEQ    F15
203600941205     C     $FC(15)       IFEQ      '0'
203700941205     C                   SETON                                        99
203800941205     C                   ENDIF
203900941205     C     $TASTO        WHENEQ    F16
204000941205     C     $FC(16)       IFEQ      '0'
204100941205     C                   SETON                                        99
204200941205     C                   ENDIF
204300941205     C     $TASTO        WHENEQ    F17
204400941205     C     $FC(17)       IFEQ      '0'
204500941205     C                   SETON                                        99
204600941205     C                   ENDIF
204700941205     C     $TASTO        WHENEQ    F18
204800941205     C     $FC(18)       IFEQ      '0'
204900941205     C                   SETON                                        99
205000941205     C                   ENDIF
205100941205     C     $TASTO        WHENEQ    F19
205200941205     C     $FC(19)       IFEQ      '0'
205300941205     C                   SETON                                        99
205400941205     C                   ENDIF
205500941205     C     $TASTO        WHENEQ    F20
205600941205     C     $FC(20)       IFEQ      '0'
205700941205     C                   SETON                                        99
205800941205     C                   ENDIF
205900941205     C     $TASTO        WHENEQ    F21
206000941205     C     $FC(21)       IFEQ      '0'
206100941205     C                   SETON                                        99
206200941205     C                   ENDIF
206300941205     C     $TASTO        WHENEQ    F22
206400941205     C     $FC(22)       IFEQ      '0'
206500941205     C                   SETON                                        99
206600941205     C                   ENDIF
206700941205     C     $TASTO        WHENEQ    F23
206800941205     C     $FC(23)       IFEQ      '0'
206900941205     C                   SETON                                        99
207000941205     C                   ENDIF
207100941205     C     $TASTO        WHENEQ    F24
207200941205     C     $FC(24)       IFEQ      '0'
207300941205     C                   SETON                                        99
207400941205     C                   ENDIF
207500941205     C                   ENDSL
207600941205     C*
207700941205     C                   ENDSR
207800940318     C/EJECT
207900940318     C******************************************************
208000940322     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
208100940318     C******************************************************
208200940318     C     MSGPGM        BEGSR
208300940318     C*
208400940318     C                   Z-ADD     2             MSGMSG
208500940318     C                   MOVEL     'S'           MSGRSP
208600940318     C                   MOVEL     'CR'          MSGTPR
208700940318     C                   MOVEL     'S'           MSGEMV
208800940318     C                   Z-ADD     10            MSGRGP
208900940318     C                   MOVEL     'S'           MSGLCK
209000940318     C                   MOVEA     STATUS        STS
209100940318     C                   MOVEL     'N'           MSGCNL
209200940318     C                   MOVEL     'N'           MSGVID
209300940318     C                   MOVEL     'N'           MSGOPE
209400940318     C                   MOVEL     'N'           MSGSTP
209500960416     C                   CALL      'XMSGR'
209600940318     C                   PARM                    XMSGDS
209700940318     C*
209800940318     C                   ENDSR
209900940207     C/EJECT
210000940131     C************************************************************
210100940131     C* OPERAZIONI INIZIALI
210200940131     C************************************************************
210300940131     C     *INZSR        BEGSR
210400000114A0735C     *DMY          Move      Udate         ZDateISO
210500000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
210600940131     C*
210700940127     C* Reperimento parametri
210800940127     C*
210900940117     C     *ENTRY        PLIST
211000940117     C                   PARM                    KPJBA
211100950104     C                   PARM                    DSAUT
211200950104     C*
211300950104     C     PARMS         IFLT      2
211400950104     C                   MOVE      *BLANK        AUTDS
211500950104     C                   ELSE
211600950104     C                   MOVEL     DSAUT         AUTDS
211700950104     C                   ENDIF
211800940506     C*
211900940506     C* Reperimento tasti di funzione
212000940506     C*
212100940506     C                   EXSR      REPKEY
212200940506     C*
212300960416     C* STRCMTCTL per numeratore
212400960416     C                   CALLb     'XSTRCMT'
212500960416     C*
212600940223     C                   ENDSR
212700940223     C/EJECT
212800940506     C************************************************************
212900940506     C* REPERIMENTO TASTI DI FUNZIONE
213000940506     C************************************************************
213100940506     C     REPKEY        BEGSR
213200940506     C                   MOVEA     $FM1          $FM
213300950906     C                   MOVEA     $FC1          $FC
213400940506     C                   EXSR      REPKKK
213500940506     C                   MOVEA     $FX           $FX1
213600940614     C*
213700940614     C                   ENDSR
213800940506     C/EJECT
213900940506     C************************************************************
214000940506     C* REPERIMENTO TASTI DI FUNZIONE
214100940506     C************************************************************
214200940506     C     REPKKK        BEGSR
214300940506     C*
214400940506     C                   CLEAR                   $FX
214500940506     C     1             DO        24            X
214600940506     C*
214700940506     C     $FM(X)        IFNE      *BLANK
214800950906     C     $FC(X)        ANDNE     '0'
214900940506     C*
215000940506     C                   MOVEL     $FM(X)        IDMSG
215100960416     C                   CALLb     'XRTVM'
215200940506     C                   PARM                    IDMSG            27
215300940506     C                   PARM                    TXTMSG          100
215400940506     C                   PARM                    ERRMSG            1
215500940506     C*
215600940506     C     ERRMSG        IFNE      *ON
215700940506     C                   MOVEL     TXTMSG        $FX(X)
215800940506     C                   ELSE
215900940506     C                   MOVEL     *ALL' '       $FX(X)
216000941027     C                   ENDIF
216100940506     C*
216200941027     C                   ENDIF
216300940506     C*
216400941027     C                   ENDDO
216500940506     C*
216600940506     C                   ENDSR
216700940509     C/EJECT
216800940223     C************************************************************
216900940223     C* INIZIALIZZAZIONE VARIABILI
217000940223     C************************************************************
217100940223     C     INZVAR        BEGSR
217200950112     C*
217300950112     C* Dati da PGM guida
217400950112     C*
217500950112     C                   MOVEL     KPJBU         A312DS
217600941201     C*
217700941201     C* Reperimento dati società
217800941201     C*
217900941115     C                   MOVEL     'SOC001'      TIPXSC
218000941115     C                   MOVEL     *BLANK        SOCXSC
218100941115     C                   EXSR      REPSOC
218200941115     C     RTNXSC        IFNE      '1'
218300941115     C                   MOVEL     XSOCDS        SOC001
218400941115     C                   ELSE
218500941115     C                   RETURN
218600941115     C                   ENDIF
218700950110     C*
218800950110     C* gestione società di riferimento
218900950110     C     FSC312        IFEQ      'S'
219000950111     C     OPZ312        OREQ      *BLANK
219100950110     C                   MOVE      XSCSOC        WWWSOC
219200950110     C* gestione simulata da capofila
219300950110     C                   ELSE
219400950110     C                   MOVE      XSCCPA        WWWSOC
219500950110     C                   ENDIF
219600950104     C*
219700950104     C* le autorizzazioni potrebbero essere già state passate,
219800950104     C* nel qual caso non vanno reperite
219900950104     C     AUTDS         IFEQ      *BLANK
220000950104     C                   EXSR      REPAUT
220100950104     C                   ELSE
220200970703     c                   if        CoF312 = 'C'
220300950104     C                   MOVEL     XCADDS        GCLIDS
220400970703     c                   endif
220500970703     c                   if        CoF312 = 'F'
220600970725     C                   MOVEL     XCADDS        GFORDS
220700970703     c                   endif
220800950104     C                   ENDIF
220900950104     C*
221000950104     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
221100950104     C* le autorizzazione
221200950104     C*
221300950104     C                   EXSR      TSTAUT
221400940715     C*
221500050801B0822X* Definizione dei tasti funzione utilizzabili a pgm
221600050801  "  X* SPOSTATI DOPO IL CHAIN (per vedere se dichiaraz. revocata)
221700050801B0822X*****              EXSR      DEFOPZ
221800940128     C*
221900940223     C* Variabili per gestione videate
222000940128     C*
222100940223     C                   MOVE      'D1'          $GEST
222200940223     C                   MOVE      *OFF          $FINE
222300940223     C                   MOVE      *ON           $INZD1
222400940224     C                   MOVE      *BLANK        $LASTG
222500940224     C                   MOVE      *BLANK        $LASTV
222600940506     C                   Z-ADD     0             $ULKD1            3 0
222700940127     C*
222800940223     C* Variabili appoggio
222900940127     C*
223000940224     C                   Z-ADD     0             CURR
223100940224     C                   Z-ADD     0             CURC
223200941222     C                   MOVEL     $DT0          DTCAMG
223300941222     C                   MOVEL     $DT0          DINAMG
223400941222     C                   MOVEL     $DT0          DTIAMG
223500941222     C                   MOVEL     $DT0          DTFAMG
223600950915     C                   MOVEL     $DT0          DTRAMG
223700951006     C                   MOVEL     $DT0          DITSAV
223800960416     C                   CALLb     'XDT4000'
223900950206     C                   PARM                    XDTAMG           10
224000000114A0735C*****              PARM      UDATE         XDTGMA            6 0
224100000114A0735C                   PARM      ZDate         XDTGMA            6 0
224200950206     C                   PARM      4             XDTSTA            1 0
224300950206     C                   MOVEL     XDTAMG        WDATE
224400940207     C*
224500940207     C* Valorizzazione campi univoci testate
224600940207     C*
224700940207     C                   CLEAR                   FMTT1
224800940207     C                   MOVEL     KNSIF         NOMSIF
224900941110     C                   MOVEL     XSCDSI        NOMDIT
225000941027     C                   MOVEL     DSPGM         NOMPGM
225100950427     C* recupero il sottotitolo (se c'è)
225200950427     C                   MOVEL     *BLANK        T1TIT
225300950427     C     SOTTIT        IFNE      *BLANK
225400950915     C                   MOVEL     SOTTIT        WWSTIT           27
225500950915     C     COF312        IFEQ      'C'
225600950915     C                   MOVE      '0030'        WWSTIT
225700950915     C                   ELSE
225800950915     C     COF312        IFEQ      'F'
225900950915     C                   MOVE      '0140'        WWSTIT
226000950915     C                   ENDIF
226100950915     C                   ENDIF
226200950915     C                   MOVEL     WWSTIT        IDMSG
226300960416     C                   CALLb     'XRTVM'
226400950427     C                   PARM                    IDMSG            27
226500950427     C                   PARM                    TXTMSG          100
226600950427     C                   PARM                    ERRMSG            1
226700950427     C                   PARM      *ON           CTRMSG            1
226800950427     C                   PARM      30            LENMSG            3 0
226900950427     C     ERRMSG        IFNE      *ON
227000950427     C                   MOVEL     TXTMSG        T1TIT
227100950427     C                   ELSE
227200950427     C                   MOVEL     *ALL'?'       T1TIT
227300950427     C                   ENDIF
227400950427     C                   ENDIF
227500950427     C* recupero il tipo operazione
227600941216     C                   Z-ADD     1             X
227700941221     C     OPZ312        LOOKUP    $OZ(X)                                 21
227800941216     C     *IN21         IFEQ      *ON
227900950427     C                   MOVEL     $ID(X)        IDMSG
228000960416     C                   CALLb     'XRTVM'
228100950427     C                   PARM                    IDMSG            27
228200950427     C                   PARM                    TXTMSG          100
228300950427     C                   PARM                    ERRMSG            1
228400950427     C                   PARM      *ON           CTRMSG            1
228500950427     C                   PARM      30            LENMSG            3 0
228600950427     C     ERRMSG        IFNE      *ON
228700950427     C                   MOVEL     TXTMSG        T1OPE
228800950427     C                   ELSE
228900950427     C                   MOVEL     *ALL'?'       T1OPE
229000950427     C                   ENDIF
229100950427     C                   ENDIF
229200940224     C*
229300940224     C* Inizializzazione/Decodifica prima videata
229400940224     C*
229500940224     C                   EXSR      INZD1
229600940224     C                   MOVE      *OFF          $INZD1
229700940127     C*
229800940117     C                   ENDSR
229900950104     C/EJECT
230000950104     C************************************************************
230100950104     C* REPERIMENTO AUTORIZZAZIONI UTENTE
230200950104     C************************************************************
230300950104     C     REPAUT        BEGSR
230400950104     C*
230500950104     C                   CLEAR                   AUTDS
230600950104     C*
230700950123     C                   MOVEL     XSCSOC        XCASOC
230800950104     C                   MOVEL     XSCDUN        XCAUNI
230900950104     C                   Z-ADD     0             XCAGRP
231000950104     C                   MOVEL     KNMUS         XCAPRF
231100970703     c                   if        CoF312 = 'C'
231200970703     C                   MOVEL     'ANGCLI'      XCAFNC
231300970703     c                   endif
231400970703     c                   if        CoF312 = 'F'
231500970703     C                   MOVEL     'ANGFOR'      XCAFNC
231600970703     c                   endif
231700950104     C                   MOVEL     *ZERO         XCAVFU
231800950104     C                   MOVE      'CK'          XCATCK
231900950104     C                   MOVE      '1'           XCANAU
232000950104     C*
232100961004     C                   CALLb     'XCHKAUT'
232200950104     C                   PARM                    AUTDS
232300950104     C*
232400950104     C* errori/non abilitazione
232500950104     C     XCARTN        IFNE      0
232600980818     C                   MOVE      '2'           Err312
232700950227     C                   MOVE      *ON           $FINE
232800950104     C                   EXSR      ENDPGM
232900950104     C                   ELSE
233000970703     c                   if        CoF312 = 'C'
233100970703     C                   MOVEL     XCADDS        GCliDs
233200970703     c                   endif
233300970703     c                   if        CoF312 = 'F'
233400970703     C                   MOVEL     XCADDS        GForDs
233500970703     c                   endif
233600950104     C                   ENDIF
233700950104     C*
233800950104     C                   ENDSR
233900950104     C/EJECT
234000950104     C************************************************************
234100950104     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
234200950104     C************************************************************
234300950104     C     TSTAUT        BEGSR
234400950104     C*
234500970703     c* se cliente
234600970703     c                   if        CoF312 = 'C'
234700950104     C                   SELECT
234800950104     C*
234900950104     C* se richiamato in immissione
235000950104     C     OPZ312        WHENEQ    '01'
235100950104     C* ma l'utente non è abilitato
235200950104     C     IMMCLI        ANDEQ     *OFF
235300950104     C* esco da pgm avvertendolo
235400950104     C                   MOVE      '2'           ERR312
235500950104     C                   MOVE      *ON           $FINE
235600950104     C                   EXSR      ENDPGM
235700950104     C*
235800950104     C* se richiamato in modifica
235900950104     C     OPZ312        WHENEQ    '02'
236000950104     C* ma l'utente non è abilitato
236100950104     C     MANCLI        ANDEQ     *OFF
236200950104     C* testo se l'utente è abilitato almeno alla visualizzazione
236300950104     C     VISCLI        IFEQ      *ON
236400950104     C                   MOVE      '05'          OPZ312
236500950104     C                   ELSE
236600950104     C* altrimenti esco da pgm avvertendolo
236700950104     C                   MOVE      '2'           ERR312
236800950104     C                   MOVE      *ON           $FINE
236900950104     C                   EXSR      ENDPGM
237000950104     C                   ENDIF
237100950104     C*
237200950104     C* se richiamato in copia
237300950104     C     OPZ312        WHENEQ    '03'
237400950104     C* ma l'utente non è abilitato
237500950104     C     IMMCLI        ANDEQ     *OFF
237600950104     C* esco da pgm avvertendolo
237700950104     C                   MOVE      '2'           ERR312
237800950104     C                   MOVE      *ON           $FINE
237900950104     C                   EXSR      ENDPGM
238000950104     C*
238100950104     C* se richiamato in annullamento
238200950104     C     OPZ312        WHENEQ    '04'
238300950104     C* ma l'utente non è abilitato
238400950104     C     ANNCLI        ANDEQ     *OFF
238500950104     C* esco da pgm avvertendolo
238600950104     C                   MOVE      '2'           ERR312
238700950104     C                   MOVE      *ON           $FINE
238800950104     C                   EXSR      ENDPGM
238900950104     C*
239000950104     C* se richiamato in visualizzazione
239100950104     C     OPZ312        WHENEQ    '05'
239200950104     C* ma l'utente non è abilitato
239300950104     C     VISCLI        ANDEQ     *OFF
239400950104     C* esco da pgm avvertendolo
239500950104     C                   MOVE      '2'           ERR312
239600950104     C                   MOVE      *ON           $FINE
239700950104     C                   EXSR      ENDPGM
239800950104     C*
239900950104     C                   ENDSL
240000970703     c                   endif
240100970703     C*
240200970703     c* se fornitore
240300970703     c                   if        CoF312 = 'F'
240400970703     C                   SELECT
240500970703     C*
240600970703     C* se richiamato in immissione
240700970703     C     OPZ312        WHENEQ    '01'
240800970703     C* ma l'utente non è abilitato
240900970703     C     ImmFor        ANDEQ     *OFF
241000970703     C* esco da pgm avvertendolo
241100970703     C                   MOVE      '2'           ERR312
241200970703     C                   MOVE      *ON           $FINE
241300970703     C                   EXSR      ENDPGM
241400970703     C*
241500970703     C* se richiamato in modifica
241600970703     C     OPZ312        WHENEQ    '02'
241700970703     C* ma l'utente non è abilitato
241800970703     C     ManFor        ANDEQ     *OFF
241900970703     C* testo se l'utente è abilitato almeno alla visualizzazione
242000970703     C     VisFor        IFEQ      *ON
242100970703     C                   MOVE      '05'          OPZ312
242200970703     C                   ELSE
242300970703     C* altrimenti esco da pgm avvertendolo
242400970703     C                   MOVE      '2'           ERR312
242500970703     C                   MOVE      *ON           $FINE
242600970703     C                   EXSR      ENDPGM
242700970703     C                   ENDIF
242800970703     C*
242900970703     C* se richiamato in copia
243000970703     C     OPZ312        WHENEQ    '03'
243100970703     C* ma l'utente non è abilitato
243200970703     C     ImmFor        ANDEQ     *OFF
243300970703     C* esco da pgm avvertendolo
243400970703     C                   MOVE      '2'           ERR312
243500970703     C                   MOVE      *ON           $FINE
243600970703     C                   EXSR      ENDPGM
243700970703     C*
243800970703     C* se richiamato in annullamento
243900970703     C     OPZ312        WHENEQ    '04'
244000970703     C* ma l'utente non è abilitato
244100970703     C     AnnFor        ANDEQ     *OFF
244200970703     C* esco da pgm avvertendolo
244300970703     C                   MOVE      '2'           ERR312
244400970703     C                   MOVE      *ON           $FINE
244500970703     C                   EXSR      ENDPGM
244600970703     C*
244700970703     C* se richiamato in visualizzazione
244800970703     C     OPZ312        WHENEQ    '05'
244900970703     C* ma l'utente non è abilitato
245000970703     C     VisFor        ANDEQ     *OFF
245100970703     C* esco da pgm avvertendolo
245200970703     C                   MOVE      '2'           ERR312
245300970703     C                   MOVE      *ON           $FINE
245400970703     C                   EXSR      ENDPGM
245500970703     C*
245600970703     C                   ENDSL
245700970703     c                   endif
245800970703     C*
245900950104     C                   ENDSR
246000941209     C/EJECT
246100941209     C************************************************************
246200941209     C* REPERIMENTO DATI SOCIETA'
246300941209     C************************************************************
246400941209     C     REPSOC        BEGSR
246500941209     C*
246600960416     C                   CALLb     'XSOC'
246700941209     C                   PARM                    TIPXSC            6
246800941209     C                   PARM                    SOCXSC            3
246900941209     C                   PARM                    CDSXSC            9 0
247000941209     C                   PARM                    MODXSC            3
247100941209     C                   PARM      *BLANKS       RTNXSC            1
247200941209     C                   PARM                    XSOCDS
247300941209     C                   PARM                    KPJBA
247400941209     C*
247500941209     C                   ENDSR
247600940715     C/EJECT
247700940715     C************************************************************
247800940715     C* DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
247900940715     C************************************************************
248000940715     C     DEFOPZ        BEGSR
248100940715     C*
248200940722     C* Questa routine deve valorizzare la schiera $FC1/$FR1
248300940715     C* con S o N a seconda della utilizzabilità o meno
248400940715     C* del tasto funzione correlato
248500940715     C*
248600940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
248700940715     C* di compilazione
248800940715     C                   RESET                   $FC1
248900940715     C                   RESET                   $FR1
249000940715     C*
249100940715     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
249200940715     C* invalido alcune opzioni
249300940715     C                   SELECT
249400940715     C* Se pgm richiamato in interrogazione disattivo :
249500941221     C     OPZ312        WHENEQ    '05'
249600940715     C* F4=Ricerca
249700940715     C                   Z-ADD     4             X
249800940720     C                   MOVE      '0'           $FC1(X)
249900940715     C* F5=Ripristino
250000940715     C                   Z-ADD     5             X
250100940720     C                   MOVE      '0'           $FC1(X)
250200940715     C* F6=Conferma
250300940715     C                   Z-ADD     06            X
250400940720     C                   MOVE      '0'           $FC1(X)
250500050801B0822C* F20=Annulla revoca
250600050801  "  C                   Z-ADD     20            X
250700050801B0822C                   MOVE      '0'           $FC1(X)
250800940715     C* Se pgm richiamato in annullamento disattivo :
250900941221     C     OPZ312        WHENEQ    '04'
251000940715     C* F4=Ricerca
251100940715     C                   Z-ADD     4             X
251200940720     C                   MOVE      '0'           $FC1(X)
251300940715     C* F5=Ripristino
251400940715     C                   Z-ADD     5             X
251500940720     C                   MOVE      '0'           $FC1(X)
251600050801B0822C* F20=Annulla revoca
251700050801  "  C                   Z-ADD     20            X
251800050802  "  C                   MOVE      '0'           $FC1(X)
251900050802  "  C* Se pgm richiamato in copia disattivo :
252000050802  "  C     OPZ312        WHENEQ    '03'
252100050802  "  C* F20=Annulla revoca
252200050802  "  C                   Z-ADD     20            X
252300050802B0822C                   MOVE      '0'           $FC1(X)
252400940715     C*
252500940715     C                   OTHER
252600940715     C*
252700940715     C                   ENDSL
252800940715     C*
252900940715     C* Disattivo a seconda delle autorizzazioni :
253000940715     C*********  AUTXXX    IFEQ 'N'
253100940715     C*********            Z-ADD1         X
253200940715     C*********  'XX'      LOKUP$OP1,X                   21
253300940715     C*********  *IN21     IFEQ *ON
253400940715     C*********            MOVE 'N'       $FL1,X
253500940715     C*********            ENDIF
253600940715     C*********            ENDIF
253700050801     C*
253800050801B0822c* se la dichiarazione NON è revocata, disattivo F20
253900050801  "  c* e lo disattivo sempre per i clienti, in quanto c'è solo la
254000050801  "  c* data che si può pulire a video
254100050801  "  C                   IF        DicRev     <>*on
254200050801  "  c                             or  cof312='C'
254300050801  "  C                   Z-ADD     20            X
254400050801  "  C                   MOVE      '0'           $FC1(X)
254500050801B0822C                   ENDIF
254600050801     C*
254700940715     C*
254800940715     C                   ENDSR
254900940128     C************************************************************
255000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
255100940223     C************************************************************
255200940223     C     DEFCAM        BEGSR
255300940223     C*
255400940223     C* klist
255500951004     C*
255600970725     C* ANDIC01L
255700970725     C     K5DIC1        KLIST
255800970725     C                   KFLD                    DICSOC
255900970725     C                   KFLD                    DICCF
256000970725     C                   KFLD                    DICKSC
256100970725     C                   KFLD                    DICNRC
256200970725     C                   KFLD                    DICDTC
256300951004     C* ANDIC02L
256400970725     C     K4DIC2        KLIST
256500951004     C                   KFLD                    DICSOC
256600951004     C                   KFLD                    DICCF
256700951004     C                   KFLD                    DICDIN
256800951004     C                   KFLD                    DICNIN
256900061013B0907C* ANDIC05L
257000090224B1090x**** K05DIC05      KList
257100090224B1090c     K04DIC05      KList
257200090303B0907C                   KFld                    DicSoc
257300090224B0907c                   KFld                    DicCF
257400090224B1090x*****************  KFld                    DicKsc
257500090224B0907C                   KFld                    DicAIn
257600061013B0907C                   KFld                    DicNIn
257700081223     C*
257800081223D2461C* PLIST per NDVASOG
257900081223  "  C     PDVASOG       PLIST
258000081223  "  C                   PARM                    DVARIC
258100081223  "  C                   PARM                    DVASOCIETA
258200081223  "  C                   PARM                    DVAUNITA
258300081223  "  C                   PARM                    DVASTRUTT
258400081223  "  C                   PARM                    DVADTRIF
258500081223  "  C                   PARM                    DVALENOUT
258600081223  "  C                   PARM                    DVASNATURA
258700081223  "  C                   PARM                    DVACODICE
258800081223  "  C                   PARM                    DVALINEAV
258900081223  "  C                   PARM                    DVAFILIALE
259000081223  "  C                   PARM                    *omit
259100081223  "  C                   PARM                    DVATPIND
259200081223  "  C                   PARM                    DVACDIND
259300081223  "  C                   PARM                    DVAERRORE
259400081223D2461C                   PARM                    DVAOUTPUT
259500950914     C*
259600000519A0778X* ANTXT01L
259700000519  "  X***  K5TXT1        KLIST
259800000519  "  X***                KFLD                    TXTSOC
259900000519  "  X***                KFLD                    TXTUNI
260000000519  "  X***                KFLD                    TXTTPT
260100000519  "  X***                KFLD                    TXTLIN
260200000519  "  X***                KFLD                    TXTPOS
260300000519  "  X*
260400000519  "  X***  K4TXT1        KLIST
260500000519  "  X***                KFLD                    TXTSOC
260600000519  "  X***                KFLD                    TXTUNI
260700000519  "  X***                KFLD                    TXTTPT
260800000519A0778X***                KFLD                    TXTLIN
260900951012     C*
261000951012     C* PLIST per XNUM
261100951012     C     PNMR          PLIST
261200951012     C                   PARM                    XNMRIC            1
261300951012     C                   PARM                    XNMSOC            3
261400951012     C                   PARM                    XNMUNI            8
261500951012     C                   PARM                    XNMCTB            2
261600951012     C                   PARM                    XNMKEY            8
261700951012     C                   PARM                    XNMSER            4
261800951012     C                   PARM                    XNMCMT            1
261900951012     C                   PARM                    XNMDAT           10
262000951012     C                   PARM                    XNMIVA            1
262100951012     C                   PARM                    XNMREG            1
262200951012     C                   PARM                    XNMERR            1
262300951012     C                   PARM                    XNMNUM            9 0
262400951012     C                   PARM                    XNMAUT            1
262500030704B0581C                   PARM                    XNMDAG           10
262600940223     C*
262700940223     C* Variabili per gestione videate
262800940223     C*
262900940223     C                   MOVE      *BLANK        $GEST             2
263000940223     C                   MOVE      *BLANK        $FINE             1
263100940223     C                   MOVE      *BLANK        $INZD1            1
263200940223     C                   MOVE      *BLANK        $LASTG            2
263300940223     C                   MOVE      *BLANK        $LASTV            2
263400940225     C                   Z-ADD     0             $PIED1            3 0
263500940223     C*
263600940223     C* Variabili appoggio
263700940223     C*
263800950206     C                   MOVE      *BLANK        WWIVA            13
263900951002     C                   MOVE      *BLANK        WWOPI             4
264000950110     C     *LIKE         DEFINE    XSCSOC        WWWSOC
264100940223     C                   Z-ADD     0             CURR              2 0
264200950206     C                   Z-ADD     0             CURC              2 0
264300940223     C                   MOVE      *ZEROS        WIN              99
264400940224     C                   MOVE      *BLANK        WTBERR            1
264500950206     C     *LIKE         DEFINE    X06DAT        WDATE
264600970210     C     *LIKE         DEFINE    D1Dtc         DtcSAV
264700970210     C     *LIKE         DEFINE    D1Nrc         NrcSAV
264800940223     C*
264900940223     C* Indici
265000940223     C*
265100940223     C                   Z-ADD     0             X                 3 0
265200940223     C                   Z-ADD     0             Y                 3 0
265300940127     C*
265400940117     C                   ENDSR
265500940509     C/EJECT
265600940509     C************************************************************
265700940509     C* RIEMPIMENTO TASTI FUNZIONE
265800940509     C************************************************************
265900940509     C     RIEKEY        BEGSR
266000940509     C*
266100940509     C* Input :
266200940509     C* - $FX = schiera con tutti i testi disponibili
266300940509     C* - $FC = schiere con i flag di validità associati ai testi
266400940509     C* - $FR = schiere con i flag di REVERSE IMAGE
266500940509     C* - $ULK = ultimo testo caricato
266600940722     C* - $L01 = Lunghezza prima riga
266700940722     C* - $L02 = Lunghezza seconda riga
266800940509     C* Output :
266900940509     C* - $ULK
267000940609     C* - $KEY1= 1° riga di tasti funzione
267100940609     C* - $KEY2= 2° riga di tasti funzione
267200940509     C*
267300960416     C                   CALLb     'XKEYMSG'
267400940509     C* input
267500940509     C                   PARM                    $FX
267600940509     C                   PARM                    $FC
267700940509     C                   PARM                    $FR
267800940509     C                   PARM                    $ULK              3 0
267900940509     C* output
268000940509     C                   PARM                    $KEY1            79
268100940509     C                   PARM                    $KEY2            79
268200940722     C* input
268300941116     C                   PARM      79            $L01              3 0
268400941116     C                   PARM      79            $L02              3 0
268500940509     C*
268600060224A1148C                   PARM                    $ALLFUNCT       480
268700060224A1148C*
268800940509     C                   ENDSR
268900950118     C*
269000050801B0822OANDIC02   E            AggDic
269100050801  "  O                       DicTpt
269200050801  "  O                       DicDtr
269300050801B0822O                       DicRev
269400950906**
26950095042701PROMSG    *LIBL     COS0001    immissione
26960095042702PROMSG    *LIBL     COS0002    modifica
26970095042703PROMSG    *LIBL     COS0003    copia
26980095042704PROMSG    *LIBL     COS0004    annullamento
26990095042705PROMSG    *LIBL     COS0005    visualizzazione
270000940509** TASTI DI FUNZIONE  D1
270100940509PROMSG    *LIBL     KEY0001  01
270200940509PROMSG    *LIBL     KEY0002  02
270300940509PROMSG    *LIBL     KEY0003  03
270400940509PROMSG    *LIBL     KEY0004  04
270500940509PROMSG    *LIBL     KEY0005  05
270600940509PROMSG    *LIBL     KEY0006  06
270700940509PROMSG    *LIBL     KEY0007  07
270800940509PROMSG    *LIBL     KEY0008  08
270900940509PROMSG    *LIBL     KEY0009  09
271000940509PROMSG    *LIBL     KEY0010  10
271100940509PROMSG    *LIBL     KEY0011  11
271200940509PROMSG    *LIBL     KEY0012  12
271300940509PROMSG    *LIBL     KEY0013  13
271400940509PROMSG    *LIBL     KEY0014  14
271500940509PROMSG    *LIBL     KEY0015  15
271600940509PROMSG    *LIBL     KEY0016  16
271700940509PROMSG    *LIBL     KEY0017  17
271800940509PROMSG    *LIBL     KEY0018  18
271900940509PROMSG    *LIBL     KEY0019  19
272000050801PROMSG    *LIBL     KEY0296  20                         B0822
272100940509PROMSG    *LIBL     KEY0021  21
272200940509PROMSG    *LIBL     KEY0022  22
272300940509PROMSG    *LIBL     KEY0023  23
272400940509PROMSG    *LIBL     KEY0024  24
272500940509**  ABILITAZIONE D1
2726009407200N  01
2727009407200N  02
2728009407201N  03
2729009407201N  04
2730009407201N  05
2731009407201N  06
2732009407200N  07
2733009407201N  08
2734009407200N  09
2735009411160N  10
2736009407200N  11
2737009407201N  12
2738009407200N  13
2739009407200N  14
2740009407200N  15
2741009407200N  16
2742009407200N  17
2743009407200N  18
2744009407200N  19
2745000508011S  20                                                  B0822
2746009407200N  21
2747009407200N  22
2748009509060N  23   Impostare a '1' se si utilizzano opzioni
2749009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
