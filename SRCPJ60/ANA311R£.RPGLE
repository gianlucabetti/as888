000100981202     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200941115     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300950104      *--------INIZIO SPECIFICHE --------------------------*
000400940211     H DECEDIT('0,') DATEDIT(*DMY.)
000500941115      *
000600950113      *        ANAGRAFICA DICHIARAZIONE D'INTENTI
000700950113      *        ----------------------------------
000800940224      *
000900940224      *
001000940224      * DOCUMENTI MICROANALISI :
001100940224      *
001200940224      * NOTE TECNICHE :
001300940224      *
001400940224      *
001500940224      * PERSONALIZZAZIONI
001600940224      *
001700150113R160  * Aggiunta gestione estensione dich.intento SDGDIF00F
001800150113R160  * Aggiunta opzione 6=Stampa su modulo
001900940224      *
002000940224      *
002100940314      *               MAPPA INDICATORI
002200940224      *
002300940307      *  21           GENERICO OPERAZIONI I/O
002400940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002500940224      *  30           SFLDSP
002600940224      * N31           SFLCLR
002700940224      *  31           SFLDSPCTL
002800940224      *  32           SFLNXTCHG
002900940224      *  33           SFLEND
003000940224      *  39           OF PRTF
003100940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003200940317      *  Specificare l'uso dei singoli indicatori
003300940224      *  50 <---> 98  ERRORI SU VIDEO
003400940909      *  Specificare l'uso dei singoli indicatori
003500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003600940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003700940224      *  99           INDIC. GENERALE DI ERRORE
003800940128     F*----------------------------------------------------*
003900971029     FANDIC03L  IF   E           K DISK
004000971029     F                                     RENAME(ANDIC000:ANDIC03)
004100940211     F*------------
004200020718A0957FANDIC04L  IF   E           K DISK
004300020718A0957F                                     RENAME(ANDIC000:ANDIC04)
004400020718     F*------------
004500150113R160 FSdgDif01l IF   E           K DISK
004600150113     F*------------
004700940201     FANMPE02L  UF A E           K DISK
004800940211     F*------------
004900941221     FANA311D   CF   E             WORKSTN
005000941221     F                                     RENAME(A311D1:FMTD1)
005100941221     F                                     RENAME(A311D2:FMTD2)
005200941221     F                                     RENAME(A311D3:FMTD3)
005300941221     F                                     RENAME(A311S1:FMTS1)
005400941221     F                                     RENAME(A311C1:FMTC1)
005500941221     F                                     RENAME(A311T1:FMTT1)
005600941221     F                                     RENAME(A311Z1:FMTZ1)
005700941221     F                                     RENAME(A311Z2:FMTZ2)
005800941221     F                                     RENAME(A311Z3:FMTZ3)
005900940131     F                                     SFILE(FMTS1:S1NRR)
006000940201     F                                     INFDS(DSFMT)
006100940128     D*----------------------------------------------------*
006200940302     D* Schiere per la gestione delle opzioni
006300940302     D*
006400150113R160 D** $MS1            S             27    DIM(5) CTDATA PERRCD(1)
006500150113 "   D** $TX1            S             50    DIM(5) ALT($MS1)
006600150113 "   D** $OP1            S              2  0 DIM(5) CTDATA PERRCD(1)
006700150113 "   D** $FL1            S              1    DIM(5) ALT($OP1)
006800150113 "   D $MS1            S             27    DIM($Dimop) CTDATA PERRCD(1)
006900150113 "   D $TX1            S             50    DIM($Dimop) ALT($MS1)
007000150113 "   D $OP1            S              2  0 DIM($Dimop) CTDATA PERRCD(1)
007100150113R160 D $FL1            S              1    DIM($Dimop) ALT($OP1)
007200941214     D $TX             S             50    DIM(50)
007300941214     D $FL             S              1    DIM(50)
007400940909     D* Schiere generali per la gestione dei tasti di funzione
007500940506     D $FM             S             27    DIM(24)
007600940506     D $FX             S             50    DIM(24)
007700940506     D $FC             S              1    DIM(24)
007800940506     D $FR             S              1    DIM(24)
007900940909     D* Schiere per la gestione dei tasti di funzione
008000940506     D* Video D1
008100940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
008200940506     D $FX1            S             50    DIM(24) ALT($FM1)
008300940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
008400940506     D $FR1            S              1    DIM(24) ALT($FC1)
008500940506     D* Schiere per la gestione dei tasti di funzione
008600940506     D* Video S1
008700940506     D $FM2            S             27    DIM(24) CTDATA PERRCD(1)
008800940506     D $FX2            S             50    DIM(24) ALT($FM2)
008900940506     D $FC2            S              1    DIM(24) CTDATA PERRCD(1)
009000940506     D $FR2            S              1    DIM(24) ALT($FC2)
009100940916     D* Schiere per la gestione dei tasti di funzione
009200940916     D* Video D2
009300940916     D $FM3            S             27    DIM(24) CTDATA PERRCD(1)
009400940916     D $FX3            S             50    DIM(24) ALT($FM3)
009500940916     D $FC3            S              1    DIM(24) CTDATA PERRCD(1)
009600940916     D $FR3            S              1    DIM(24) ALT($FC3)
009700950914     D*-------------
009800941216     D* Richiamo a DVCLI
009900941216     D DVCLI         E DS                  EXTNAME(DVCLIDS) INZ
010000941216     D*-------------
010100941216     D CLI             C                   CONST('  DVOCLIDS')
010200950914     D*-------------
010300941216     D*  Ds per output DVCLI formato ANCLN+ANSOG+ANIND
010400941216     D DVOCLI        E DS                  EXTNAME(DVOCLIDS) INZ
010500950914     D*-------------
010600941216     D* Anagrafica clienti
010700941216     D  CLNSOC       E                     EXTFLD(CLNSOCIETA)
010800941216     D  CLNUNT       E                     EXTFLD(CLNUNITA)
010900941216     D  CLNKCC       E                     EXTFLD(CLNKCC)
011000941216     D  CLNKSC       E                     EXTFLD(CLNKSC)
011100941216     D  CLNSOG       E                     EXTFLD(CLNSOGG)
011200941216     D  CLNTII       E                     EXTFLD(CLNTPIND)
011300941216     D  CLNCOI       E                     EXTFLD(CLNCDIND)
011400941216     D  CLNDES       E                     EXTFLD(CLNDESBREV)
011500941216     D  CLNLDV       E                     EXTFLD(CLNLINEAV)
011600941216     D  CLNFIL       E                     EXTFLD(CLNFILIALE)
011700941216     D  CLNDAC       E                     EXTFLD(CLNDTINRPC)
011800941216     D  CLNUTE       E                     EXTFLD(CLNUTENTE)
011900941216     D  CLNDVA       E                     EXTFLD(CLNDTULMO)
012000941216     D  CLNCCP       E                     EXTFLD(CLNCLIPREC)
012100941216     D  CLNCFP       E                     EXTFLD(CLNFORCORR)
012200941216     D  CLNCCF       E                     EXTFLD(CLNCLIFATT)
012300941216     D  CLNFAP       E                     EXTFLD(CLNADDSPBA)
012400941216     D  CLNFSB       E                     EXTFLD(CLNSPEBOLL)
012500941216     D  CLNCOF       E                     EXTFLD(CLNNRCPFAT)
012600941216     D  CLNIMB       E                     EXTFLD(CLNIMBUSTA)
012700941216     D  CLNFID       E                     EXTFLD(CLNFIDOSTA)
012800941216     D  CLNFIA       E                     EXTFLD(CLNFIDOASS)
012900941216     D  CLNDID       E                     EXTFLD(CLNDTFIST)
013000941216     D  CLNDIA       E                     EXTFLD(CLNDTFIAS)
013100941216     D  CLNCAS       E                     EXTFLD(CLNCLIASSO)
013200941216     D  CLNGDR       E                     EXTFLD(CLNGGRCVAG)
013300941216     D  CLNFBC       E                     EXTFLD(CLNCTRCONT)
013400941216     D  CLNFPC       E                     EXTFLD(CLNCITPROV)
013500941216     D  CLNASN       E                     EXTFLD(CLNSTATIST)
013600941216     D  CLNDAN       E                     EXTFLD(CLNDTFIRPC)
013700941216     D  CLNCLV       E                     EXTFLD(CLNCLASABC)
013800941216     D  CLNSOL       E                     EXTFLD(CLNTPTESTO)
013900941216     D  CLNMAG       E                     EXTFLD(CLNMAG)
014000941216     D  CLNADO       E                     EXTFLD(CLNADDIVA)
014100941216     D  CLNFAT       E                     EXTFLD(CLNFREQFAT)
014200941216     D  CLNTFA       E                     EXTFLD(CLNRAGGBOL)
014300941216     D  CLNRAF       E                     EXTFLD(CLNRAGGART)
014400941216     D  CLNMSP       E                     EXTFLD(CLNMDSPED)
014500941216     D  CLNRES       E                     EXTFLD(CLNRESA)
014600941216     D  CLNSSF       E                     EXTFLD(CLNSTASCOF)
014700941216     D  CLNFDP       E                     EXTFLD(CLNDETPREZ)
014800941216     D  CLNDIR       E                     EXTFLD(CLNDTINRPL)
014900941216     D  CLNDFR       E                     EXTFLD(CLNDTFIRPL)
015000941216     D  CLNLIS       E                     EXTFLD(CLNLISTINO)
015100941216     D  CLNVAL       E                     EXTFLD(CLNDIVISA)
015200941216     D  CLNFUC       E                     EXTFLD(CLNCAPODIS)
015300941216     D  CLNFUN       E                     EXTFLD(CLNFUNZRIO)
015400941216     D  CLNAGE       E                     EXTFLD(CLNAGENTE1)
015500941216     D  CLNAG2       E                     EXTFLD(CLNAGENTE2)
015600941216     D  CLNAG3       E                     EXTFLD(CLNAGENTE3)
015700941216     D  CLNPSP       E                     EXTFLD(CLNPRIOEVA)
015800941216     D  CLNBGC       E                     EXTFLD(CLNBKGSCLI)
015900941216     D  CLNRSC       E                     EXTFLD(CLNRAGGSCO)
016000941216     D  CLNRAP       E                     EXTFLD(CLNRAGGPRV)
016100941216     D  CLNTPP       E                     EXTFLD(CLNLORDNET)
016200941216     D  CLNCDP       E                     EXTFLD(CLNCONDPAG)
016300941216     D  CLNSCP       E                     EXTFLD(CLNSCOCASS)
016400941216     D  CLNPA1       E                     EXTFLD(CLNPERCAD1)
016500941216     D  CLNPA2       E                     EXTFLD(CLNPERCAD2)
016600941216     D  CLNRCO       E                     EXTFLD(CLNCONFORD)
016700941216     D  CLNRGU       E                     EXTFLD(CLNRAGGVEN)
016800941216     D  CLNSPP       E                     EXTFLD(CLNSTAPREZ)
016900941216     D  CLNCIV       E                     EXTFLD(CLNCDIVA)
017000941216     D  CLNTIN       E                     EXTFLD(CLNTPINDCM)
017100941216     D  CLNSC1       E                     EXTFLD(CLNSCOPAG1)
017200941216     D  CLNSC2       E                     EXTFLD(CLNSCOPAG2)
017300941216     D  CLNIP1       E                     EXTFLD(CLNGGINPA1)
017400941216     D  CLNIP2       E                     EXTFLD(CLNGGINPA2)
017500941216     D  CLNIP3       E                     EXTFLD(CLNGGINPA3)
017600941216     D  CLNTRA       E                     EXTFLD(CLNSESSTRA)
017700941216     D  CLNRIC       E                     EXTFLD(CLNSESSRIC)
017800941216     D  CLNTIV       E                     EXTFLD(CLNTPPVEND)
017900941216     D  CLNTPC       E                     EXTFLD(CLNTPCAUZ)
018000941216     D  CLNTSA       E                     EXTFLD(CLNACCONTO)
018100941216     D  CLNCES       E                     EXTFLD(CLNCLICESS)
018200941216     D  CLNFS1       E                     EXTFLD(CLNCLISOLL)
018300941216     D  CLNFS2       E                     EXTFLD(CLNADDSOLL)
018400941216     D  CLNFS5       E                     EXTFLD(CLNCONTRAS)
018500941216     D  CLNFS6       E                     EXTFLD(CLNADDPROD)
018600941216     D  CLNRGC       E                     EXTFLD(CLNRAGGCON)
018700941216     D  CLNFS3       E                     EXTFLD(CLNFLG1)
018800941216     D  CLNFS4       E                     EXTFLD(CLNFLG2)
018900941216     D  CLNFL3       E                     EXTFLD(CLNFLG3)
019000941216     D  CLNFL4       E                     EXTFLD(CLNFLG4)
019100941216     D  CLNFL5       E                     EXTFLD(CLNFLG5)
019200941216     D  CLNFL6       E                     EXTFLD(CLNFLG6)
019300941216     D* Anagrafica soggetti
019400941216     D  SOGCOD       E                     EXTFLD(SOGSOGG)
019500941216     D  SOGDAT       E                     EXTFLD(SOGDTENTR)
019600941216     D  SOGDAU       E                     EXTFLD(SOGDTULMO)
019700941216     D  SOGDAI       E                     EXTFLD(SOGDTINVL)
019800941216     D  SOGDAF       E                     EXTFLD(SOGDTFIVL)
019900941216     D  SOGDAN       E                     EXTFLD(SOGDTNASC)
020000941216     D  SOGDES       E                     EXTFLD(SOGDES)
020100941216     D  SOGDE1       E                     EXTFLD(SOGDESAGG)
020200941216     D  SOGCDF       E                     EXTFLD(SOGCDFISC)
020300941216     D  SOGIVA       E                     EXTFLD(SOGPARTIVA)
020400941216     D  SOGFPI       E                     EXTFLD(SOGFRCPIVA)
020500941216     D  SOGCIA       E                     EXTFLD(SOGISCCIAA)
020600941216     D  SOGSES       E                     EXTFLD(SOGSEX)
020700941216     D  SOGNAT       E                     EXTFLD(SOGNATGIUR)
020800941216     D  SOGCOP       E                     EXTFLD(SOGCODPREC)
020900941216     D  SOGABI       E                     EXTFLD(SOGABI)
021000941216     D  SOGCLG       E                     EXTFLD(SOGCLFSOG)
021100941216     D* Anagrafica indirizzi
021200941216     D  INDCOD       E                     EXTFLD(INDSOGG)
021300941216     D  INDTII       E                     EXTFLD(INDTPIND)
021400941216     D  INDCOI       E                     EXTFLD(INDCDIND)
021500941216     D  INDDAU       E                     EXTFLD(INDDTULMO)
021600941216     D  INDDAI       E                     EXTFLD(INDDTINVL)
021700941216     D  INDDAF       E                     EXTFLD(INDDTFIVL)
021800941216     D  INDDES       E                     EXTFLD(INDDESFIL)
021900941216     D  INDDE1       E                     EXTFLD(INDDESAGG)
022000941216     D  INDVIA       E                     EXTFLD(INDINDRIZ)
022100941216     D  INDCIT       E                     EXTFLD(INDLOCALIT)
022200941216     D  INDCAP       E                     EXTFLD(INDCAP)
022300941216     D  INDPRV       E                     EXTFLD(INDPROV)
022400941216     D  INDSTA       E                     EXTFLD(INDSTATO)
022500941216     D  INDCAB       E                     EXTFLD(INDCAB)
022600941216     D  INDLIN       E                     EXTFLD(INDLINGUA)
022700941216     D  INDTLX       E                     EXTFLD(INDTELEX)
022800941216     D  INDTEL       E                     EXTFLD(INDTELEFON)
022900941216     D  INDTLF       E                     EXTFLD(INDTELEFAX)
023000950914     D*-------------
023100950914     D* Richiamo a DVFOR
023200950914     D DVFOR         E DS                  EXTNAME(DVFORDS) INZ
023300950914     D*-------------
023400950914     D FRN             C                   CONST('  DVOFRNDS')
023500950914     D*-------------
023600950914     D*  Ds per output DVFRN formato ANFRN+ANSOG+ANIND
023700001010A0840X*DVOFRN        E DS                  EXTNAME(DVOFRNDS) INZ
023800001010A0840D DVOFRN        E DS          1000    EXTNAME(DVOFRNDS) INZ
023900950914     D*-------------
024000950914     D* Anagrafica fornitori
024100950914     D  FRNSOC       E                     EXTFLD(FRNSOCIETA)
024200950914     D  FRNUNT       E                     EXTFLD(FRNUNITA)
024300950914     D  FRNKCC       E                     EXTFLD(FRNKCC)
024400950914     D  FRNKSC       E                     EXTFLD(FRNKSC)
024500950914     D  FRNFIL       E                     EXTFLD(FRNFILIALE)
024600950914     D  FRNSOG       E                     EXTFLD(FRNSOGG)
024700950914     D  FRNTII       E                     EXTFLD(FRNTPIND)
024800950914     D  FRNCOI       E                     EXTFLD(FRNCDIND)
024900950914     D  FRNDUM       E                     EXTFLD(FRNDTULMO)
025000950914     D  FRNUTE       E                     EXTFLD(FRNUTENTE)
025100950914     D  FRNDIR       E                     EXTFLD(FRNDTINRPF)
025200950914     D  FRNDFR       E                     EXTFLD(FRNDTFIRPF)
025300950914     D  FRNDES       E                     EXTFLD(FRNDESBREV)
025400950914     D  FRNGPM       E                     EXTFLD(FRNGRUMERC)
025500950914     D  FRNSGM       E                     EXTFLD(FRNSETMERC)
025600950914     D  FRNCFA       E                     EXTFLD(FRNFORALT)
025700950914     D  FRNLIS       E                     EXTFLD(FRNLISTINO)
025800950914     D  FRNVAL       E                     EXTFLD(FRNDIVISA)
025900950914     D  FRNCDD       E                     EXTFLD(FRNCONDCON)
026000950914     D  FRNQFO       E                     EXTFLD(FRNQUALFOR)
026100950914     D  FRNVAF       E                     EXTFLD(FRNVOLAFF)
026200950914     D  FRNCIV       E                     EXTFLD(FRNCDIVA)
026300950914     D  FRNCOB       E                     EXTFLD(FRNCOLLOBB)
026400950914     D  FRNTCL       E                     EXTFLD(FRNTPCOLL)
026500950914     D  FRNFRL       E                     EXTFLD(FRNFORLIST)
026600950914     D  FRNFOF       E                     EXTFLD(FRNFORFATT)
026700950914     D  FRNPOT       E                     EXTFLD(FRNFORPOT)
026800950914     D  FRNTRP       E                     EXTFLD(FRNREPPRZL)
026900950914     D  FRNFFB       E                     EXTFLD(FRNFORBLO)
027000950914     D  FRNLSF       E                     EXTFLD(FRNLISFIL)
027100950914     D  FRNCPA       E                     EXTFLD(FRNABILPRI)
027200950914     D  FRNMDC       E                     EXTFLD(FRNMDCONS)
027300950914     D  FRNMAG       E                     EXTFLD(FRNMAG)
027400950914     D  FRNCLI       E                     EXTFLD(FRNCLICORR)
027500950914     D  FRNFRP       E                     EXTFLD(FRNFORPREC)
027600950914     D  FRNSC1       E                     EXTFLD(FRNSCOPAG)
027700950914     D  FRNSC2       E                     EXTFLD(FRNSCOPAG2)
027800950914     D  FRNIP1       E                     EXTFLD(FRNGGINPA1)
027900950914     D  FRNIP2       E                     EXTFLD(FRNGGINPA2)
028000950914     D  FRNIP3       E                     EXTFLD(FRNGGINPA3)
028100950914     D  FRNTRA       E                     EXTFLD(FRNSESSTRA)
028200950914     D  FRNRIC       E                     EXTFLD(FRNSESSRIC)
028300950914     D  FRNCDP       E                     EXTFLD(FRNCONDPAG)
028400950914     D* Anagrafica soggetti
028500950914     D  FOGCOD       E                     EXTFLD(SOGSOGG)
028600950914     D  FOGDAT       E                     EXTFLD(SOGDTENTR)
028700950914     D  FOGDAU       E                     EXTFLD(SOGDTULMO)
028800950914     D  FOGDAI       E                     EXTFLD(SOGDTINVL)
028900950914     D  FOGDAF       E                     EXTFLD(SOGDTFIVL)
029000950914     D  FOGDAN       E                     EXTFLD(SOGDTNASC)
029100950914     D  FOGDES       E                     EXTFLD(SOGDES)
029200950914     D  FOGDE1       E                     EXTFLD(SOGDESAGG)
029300950914     D  FOGCDF       E                     EXTFLD(SOGCDFISC)
029400950914     D  FOGIVA       E                     EXTFLD(SOGPARTIVA)
029500950914     D  FOGFPI       E                     EXTFLD(SOGFRCPIVA)
029600950914     D  FOGCIA       E                     EXTFLD(SOGISCCIAA)
029700950914     D  FOGSES       E                     EXTFLD(SOGSEX)
029800950914     D  FOGNAT       E                     EXTFLD(SOGNATGIUR)
029900950914     D  FOGCOP       E                     EXTFLD(SOGCODPREC)
030000950914     D  FOGABI       E                     EXTFLD(SOGABI)
030100950914     D  FOGCLG       E                     EXTFLD(SOGCLFSOG)
030200950914     D* Anagrafica indirizzi
030300950914     D  FNDCOD       E                     EXTFLD(INDSOGG)
030400950914     D  FNDTII       E                     EXTFLD(INDTPIND)
030500950914     D  FNDCOI       E                     EXTFLD(INDCDIND)
030600950914     D  FNDDAU       E                     EXTFLD(INDDTULMO)
030700950914     D  FNDDAI       E                     EXTFLD(INDDTINVL)
030800950914     D  FNDDAF       E                     EXTFLD(INDDTFIVL)
030900950914     D  FNDDES       E                     EXTFLD(INDDESFIL)
031000950914     D  FNDDE1       E                     EXTFLD(INDDESAGG)
031100950914     D  FNDVIA       E                     EXTFLD(INDINDRIZ)
031200950914     D  FNDCIT       E                     EXTFLD(INDLOCALIT)
031300950914     D  FNDCAP       E                     EXTFLD(INDCAP)
031400950914     D  FNDPRV       E                     EXTFLD(INDPROV)
031500950914     D  FNDSTA       E                     EXTFLD(INDSTATO)
031600950914     D  FNDCAB       E                     EXTFLD(INDCAB)
031700950914     D  FNDLIN       E                     EXTFLD(INDLINGUA)
031800950914     D  FNDTLX       E                     EXTFLD(INDTELEX)
031900950914     D  FNDTEL       E                     EXTFLD(INDTELEFON)
032000950914     D  FNDTLF       E                     EXTFLD(INDTELEFAX)
032100941216     D*-------------
032200940207     D* Valori assunti
032300941111     D DSMPE           DS                  INZ
032400940207     D  $LOP                   1      1  0
032500940831     D  $ORD                   2      3  0
032600940909     D*-------------
032700941215     D*Definire campi chiave per la vista logica
032800940831     D* DS per chiave aggancio
032900940831     D DSKEY           DS
033000941215     D  KSCKEY                 1      8
033100941221     D  NRCKEY                 9     14  0
033200941221     D  DCLKEY                15     24
033300940909     D*-------------
033400941026     D* FUNZIONI ESCLUSIVE DI NOSTRADAMUS
033500010314a0871x*-------------
033600010314  "  x* Parametri del PGM di gestione commenti ANA725R
033700010314  "  x*A725DS        E DS                  EXTNAME(ANA725DS) INZ
033800010314  "  x* RET725       E                     INZ('0')
033900010314  "  x* OPR725       E                     INZ('0')
034000010314a0871x* ERR725       E                     INZ('0')
034100941215     D*-------------
034200941215     D* Parametri del PGM invio posta ANA727R
034300941215     D A727DS        E DS                  EXTNAME(ANA727DS) INZ
034400941215     D  RET727       E                     INZ('0')
034500941215     D  OPR727       E                     INZ('0')
034600941215     D  ERR727       E                     INZ('0')
034700940207     D*-------------
034800940211     D* Passaggio Parametri
034900940211     D KPJBA         E DS
035000940211     D*-------------
035100940225     D* Reperimento Autorizzazioni
035200941124     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
035300950104     D* Passaggio Autorizzazioni
035400950104     D DSAUT           DS           512
035500941124     D*-------------
035600941124     D* Maschera dati autorizzazioni
035700950104     D GCLIDS        E DS                  EXTNAME(ANGCLIDS)
035800970704     D*-------------
035900970704     D* Maschera dati autorizzazioni per clienti
036000970704     D GForDs        E DS                  EXTNAME(AngForDs)
036100970704     d ImmFilF       E                     ExtFld(ImmFil)
036200970704     d ManFilF       E                     ExtFld(ManFil)
036300970704     d AnnFilF       E                     ExtFld(AnnFil)
036400970704     d VisFilF       E                     ExtFld(VisFil)
036500970704     D*-------------
036600941209     D* Dati di ambiente ottenuti da XSOC
036700941209     D SOC001        E DS                  EXTNAME(XSOC001DS)
036800941209     D*-------------
036900941209     D* DS Interna per dati di output di XSOC
037000941209     D XSOCDS          DS          1000
037100940325     D*-------------
037200941215     D* Richiamo a XATB
037300941215     D XATBDS        E DS                  INZ
037400941215     D*-------------
037500941215     D* Parametri per ricerca codice ANATB00F
037600941215     D XTABDS        E DS                  INZ
037700941215     D* Ricerca cliente
037800941229     D A300DS        E DS                  EXTNAME(ANA300DS)
037900940325     D* Parametri in ricezione
038000941221     D A311DS        E DS                  EXTNAME(ANA311DS)
038100940325     D*-------------
038200941221     D* Parametri del PGM di manutenzione ANA312R
038300941221     D A312DS        E DS                  EXTNAME(ANA312DS) INZ
038400941221     D  RET312       E                     INZ('0')
038500941221     D  OPR312       E                     INZ('0')
038600941221     D  ERR312       E                     INZ('0')
038700980205     D  CMT312       E                     INZ('0')
038800150113R160 D*-------------
038900150113 "   D* Parametri del pgm di stampa modulo SDGSJ63R
039000150113 "   D SdgSJ63DS     E DS                  Inz
039100150113R160 D  ERRJ63       E                     INZ('0')
039200940211     D*-------------
039300940211     D DSFMT           DS           512
039400940506     D  $TASTO               369    369
039500940211     D  NRG                  370    370
039600940211     D  NCL                  371    371
039700940211     D  SFLNRR               378    379B 0
039800940211     D*-------------
039900940211     D* posizione cursore
040000940211     D CURSOR          DS
040100940211     D  RIRI                   1      2B 0 INZ
040200940211     D  R$$                    2      2
040300940211     D  COCO                   3      4B 0 INZ
040400940211     D  C$$                    4      4
040500940127     D*-------------
040600940127     D* Reperimento nome PGM
040700940127     D STATUS         SDS           333
040800940127     D  DSPGM            *PROC
040900950104     D  PARMS            *PARMS
041000940207     D*-------------
041100940616     D* Costanti
041200940616     D* nr.di righe del sfl (SFLPAG)
041300941228     D SFLPAG          C                   CONST(9)
041400940609     D* dimensione della schiera $MS1
041500150113R160 D** $DIMOP          C                   CONST(5)
041600150113R160 D $DIMOP          C                   CONST(6)
041700941117     D* data a 0
041800941117     D $DT0            C                   CONST('0001-01-01')
041900950113     D* data a *HIVAL
042000950113     D $DT999          C                   CONST('9999-12-31')
042100940616     D*-------------
042200940506     D* Tasti di funzione
042300940506     D F01             C                   CONST(X'31')
042400940506     D F02             C                   CONST(X'32')
042500940506     D F03             C                   CONST(X'33')
042600940506     D F04             C                   CONST(X'34')
042700940506     D F05             C                   CONST(X'35')
042800940506     D F06             C                   CONST(X'36')
042900940506     D F07             C                   CONST(X'37')
043000940506     D F08             C                   CONST(X'38')
043100940506     D F09             C                   CONST(X'39')
043200940506     D F10             C                   CONST(X'3A')
043300940506     D F11             C                   CONST(X'3B')
043400940506     D F12             C                   CONST(X'3C')
043500940506     D F13             C                   CONST(X'B1')
043600940506     D F14             C                   CONST(X'B2')
043700940506     D F15             C                   CONST(X'B3')
043800940506     D F16             C                   CONST(X'B4')
043900940506     D F17             C                   CONST(X'B5')
044000940506     D F18             C                   CONST(X'B6')
044100940506     D F19             C                   CONST(X'B7')
044200940506     D F20             C                   CONST(X'B8')
044300940506     D F21             C                   CONST(X'B9')
044400940506     D F22             C                   CONST(X'BA')
044500940506     D F23             C                   CONST(X'BB')
044600940506     D F24             C                   CONST(X'BC')
044700940506     D ENTER           C                   CONST(X'F1')
044800940506     D ROLDWN          C                   CONST(X'F4')
044900940506     D ROLLUP          C                   CONST(X'F5')
045000950427     D*-------------
045100950427     D SOTTIT          C                   CONST('PROMSG    *LIBL-
045200950915     D                                          COSXXXX')
045300150120R160  *--------------------------------------------------------------------
045400150120 "    *  - Codice: codice in cui eliminare gli zeri non significativi a sx
045500150120 "    *  - Len: lunghezza effettiva del codice
045600150120 "    *--------------------------------------------------------------------
045700150120 "   D SkCod           S              1A   DIM(6)
045800150120 "   D WCod            S              6A
045900150120R160 D WNrInter        S                   Like(NrInte1J63)
046000940302     I*
046100940211     I/EJECT
046200940128     I*----------------------------------------------------*
046300941221     I* Dichiarazione di intenti
046400971029     IANDIC03
046500941221     I              DICSOCIETA                  DICSOC
046600941221     I              DICUNITA                    DICUNI
046700941221     I              DICKSC                      DICKSC
046800941221     I              DICNRCLI                    DICNRC
046900941221     I              DICDTCLI                    DICDTC
047000941221     I              DICNRINTER                  DICNIN
047100941221     I              DICDTINTER                  DICDIN
047200941221     I              DICDTINDIC                  DICDTI
047300941221     I              DICDTFIDIC                  DICDTF
047400941221     I              DICIMPORTO                  DICIMP
047500941221     I              DICCDIVA                    DICIVA
047600941221     I              DICANNULLA                  DICANN
047700950914     I              DICTPTESTO                  DICTPT
047800950914     I              DICOPERA                    DICOPE
047900950914     I              DICANNO                     DICAAA
048000950914     I              DICDIVISA                   DICDIV
048100950914     I              DICDTREVOC                  DICDTR
048200020718A0957IANDIC04
048300020718  "  I              DICSOCIETA                  DICSOC
048400020718  "  I              DICUNITA                    DICUNI
048500020718  "  I              DICKSC                      DICKSC
048600020718  "  I              DICNRCLI                    DICNRC
048700020718  "  I              DICDTCLI                    DICDTC
048800020718  "  I              DICNRINTER                  DICNIN
048900020718  "  I              DICDTINTER                  DICDIN
049000020718  "  I              DICDTINDIC                  DICDTI
049100020718  "  I              DICDTFIDIC                  DICDTF
049200020718  "  I              DICIMPORTO                  DICIMP
049300020718  "  I              DICCDIVA                    DICIVA
049400020718  "  I              DICANNULLA                  DICANN
049500020718  "  I              DICTPTESTO                  DICTPT
049600020718  "  I              DICOPERA                    DICOPE
049700020718  "  I              DICANNO                     DICAAA
049800020718  "  I              DICDIVISA                   DICDIV
049900020718A0957I              DICDTREVOC                  DICDTR
050000940127     C*----------------------------------------------------*
050100940127     C*                MAIN LINE PROGRAM
050200940127     C*----------------------------------------------------*
050300940127     C*
050400940223     C* inizializzazione variabili
050500940223     C                   EXSR      INZVAR
050600940223     C*
050700940223     C     $FINE         DOWEQ     *OFF
050800940131     C     $GEST         CASEQ     'D1'          GESD1                          Parzializz.
050900940131     C     $GEST         CASEQ     'S1'          GESS1                          Lista
051000940131     C     $GEST         CASEQ     'D2'          GESD2                          Valori ass.
051100941027     C                   ENDCS
051200941027     C                   ENDDO
051300940325     C* fine programma
051400940325     C                   EXSR      ENDPGM
051500940225     C*
051600940325     C/EJECT
051700940325     C************************************************************
051800940325     C* FINE PROGRAMMA
051900940325     C************************************************************
052000940325     C     ENDPGM        BEGSR
052100940722     C*
052200940722     C* aggiorno valori assunti
052300941116     C* se NON sono entrato con chiave univoca salvo
052400941214     C     $UNIQ         IFEQ      *OFF
052500940722     C                   EXSR      AGGMPE
052600941013     C                   ENDIF
052700940325     C*
052800940325     C* passaggio dati al pgm chiamante
052900941221     C                   MOVEL     WOPR          OPR311                                     ne S1
053000941221     C                   MOVEL     A311DS        KPJBU
053100940325     C*
053200940325     C* Se pgm Guida non è stato richiamato posso chiudere in LR
053300941221     C     OPZ311        IFEQ      *BLANK
053400940325     C                   SETON                                            LR
053500940325     C* altrimenti chiudo in RT
053600940325     C                   ELSE
053700940325     C                   SETON                                            RT
053800940325     C                   ENDIF
053900940616     C                   RETURN
054000940325     C*
054100940325     C                   ENDSR
054200940224     C/EJECT
054300940201     C************************************************************
054400940201     C* RICERCHE
054500940201     C************************************************************
054600940127     C     SEARCH        BEGSR
054700940127     C*
054800940127     C* determino Riga/Colonna del cursore
054900940127     C                   MOVE      NRG           R$$
055000940127     C                   MOVE      NCL           C$$
055100940127     C                   Z-ADD     RIRI          CURR
055200940127     C                   Z-ADD     COCO          CURC
055300940127     C*
055400941118     C                   SELECT
055500940127     C*
055600941215     C*
055700941221     C* FMTD1  -
055800941221     C     H1NMFL        WHENEQ    'XXXXX'
055900941215     C* imposto pos. del cursore
056000941215     C                   Z-ADD     CURR          H1RIGA
056100941215     C                   Z-ADD     CURC          H1COLO
056200940223     C* ricerca non abilitata in questa posizione
056300941118     C                   OTHER
056400940929     C* imposto pos. del cursore
056500940929     C                   Z-ADD     CURR          H1RIGA
056600940929     C                   Z-ADD     CURC          H1COLO
056700940929     C                   SETON                                        9899
056800940127     C*
056900941118     C                   ENDSL
057000940127     C*
057100940506     C                   MOVE      *HIVAL        $LASTV
057200940127     C                   ENDSR
057300940131     C/EJECT
057400940224     C************************************************************
057500940131     C* RIEMPIMENTO OPZIONI
057600940224     C************************************************************
057700940131     C     RIEOPZ        BEGSR
057800940223     C*
057900940223     C* Input :
058000940223     C* - $TX = schiera con tutti i testi disponibili
058100940223     C* - $FL = schiere con i flag di validità associati ai testi
058200940225     C* - WCOZ = variabile contenente l'indice dell'ultimo testo
058300940223     C*          opzione usato
058400940223     C* - WMXO = variabile contenente il numero massimo di testi
058500940223     C*          opzione usabili
058600940721     C* - $L01 = Lunghezza prima riga
058700940721     C* - $L02 = Lunghezza seconda riga
058800940223     C* Output :
058900940223     C* - WOPZ1 = variabile contenente la prima riga opzioni
059000940223     C* - WOPZ2 = variabile contenente la seconda riga opzioni
059100941214     C* - FF23  = flag che indica se c'è bisogno del tasto F23
059200940223     C*
059300961004     C                   CALLb     'X04OPMSG'
059400940223     C* input
059500940223     C                   PARM                    $TX
059600940223     C                   PARM                    $FL
059700940225     C                   PARM                    WCOZ              3 0
059800940223     C                   PARM                    WMXO              3 0
059900940223     C* output
060000940223     C                   PARM                    WOPZ1            76
060100940223     C                   PARM                    WOPZ2            76
060200941214     C                   PARM                    FF23              1
060300940721     C* input
060400941220     C                   PARM      76            $L01              3 0
060500941220     C                   PARM      76            $L02              3 0
060600940223     C*
060700060224A1148C                   PARM                    $ALLOPT         960
060800060224A1148C*
060900940131     C                   ENDSR
061000940506     C/EJECT
061100940506     C************************************************************
061200940506     C* RIEMPIMENTO TASTI FUNZIONE
061300940506     C************************************************************
061400940506     C     RIEKEY        BEGSR
061500940506     C*
061600940506     C* Input :
061700940506     C* - $FX = schiera con tutti i testi disponibili
061800940506     C* - $FC = schiere con i flag di validità associati ai testi
061900940506     C* - $FR = schiere con i flag di REVERSE IMAGE
062000940506     C* - $ULK = ultimo testo caricato
062100940721     C* - $L01 = Lunghezza prima riga
062200940721     C* - $L02 = Lunghezza seconda riga
062300940506     C* Output :
062400940506     C* - $ULK
062500940609     C* - $KEY1= 1° riga di tasti funzione
062600940609     C* - $KEY2= 2° riga di tasti funzione
062700940506     C*
062800961004     C                   CALLb     'XKEYMSG'
062900940506     C* input
063000940506     C                   PARM                    $FX
063100940506     C                   PARM                    $FC
063200940506     C                   PARM                    $FR
063300940506     C                   PARM                    $ULK              3 0
063400940506     C* output
063500940506     C                   PARM                    $KEY1            79
063600940506     C                   PARM                    $KEY2            79
063700940721     C* input
063800940721     C                   PARM                    $L01              3 0
063900940721     C                   PARM                    $L02              3 0
064000940506     C*
064100060224A1148C                   PARM                    $ALLFUNCT       480
064200060224A1148C*
064300940506     C                   ENDSR
064400940224     C/EJECT
064500940224     C************************************************************
064600940224     C* GESTIONE EMISSIONE TASTI FUNZIONE
064700940224     C************************************************************
064800940224     C     GESPIE        BEGSR
064900940224     C*
065000940224     C                   SELECT
065100940224     C* Parzializzazioni
065200940224     C     $GEST         WHENEQ    'D1'
065300940506     C     $ULKD1        IFEQ      0
065400940506     C* riempimento opzioni
065500940506     C                   EXSR      F24D1
065600940506     C                   ELSE
065700940506     C                   WRITE     FMTZ1
065800940506     C                   ENDIF
065900940506     C* Lista
066000940506     C     $GEST         WHENEQ    'S1'
066100940506     C     $ULKS1        IFEQ      0
066200940506     C                   EXSR      F24S1
066300940506     C                   ELSE
066400940506     C                   WRITE     FMTZ2
066500940506     C                   ENDIF
066600940916     C* Modifica valori assunti
066700940916     C     $GEST         WHENEQ    'D2'
066800940916     C     $ULKD2        IFEQ      0
066900940916     C                   EXSR      F24D2
067000940916     C                   ELSE
067100940916     C                   WRITE     FMTZ3
067200940916     C                   ENDIF
067300940506     C                   ENDSL
067400940506     C*
067500940224     C                   ENDSR
067600940224     C/EJECT
067700940224     C************************************************************
067800940224     C* GESTIONE PARZIALIZZAZIONI
067900940224     C************************************************************
068000940224     C     GESD1         BEGSR
068100940224     C* inizializzazione videata
068200940223     C     $INZD1        IFEQ      *ON
068300940127     C                   EXSR      INZD1
068400940223     C                   MOVE      *OFF          $INZD1
068500941027     C                   ENDIF
068600940223     C* emissione piede videata se proveniente da altra
068700940223     C* salvataggio valore formato di provenienza
068800940202     C     $LASTV        IFNE      'D1'
068900940223     C* salvo stato F17 per sapere cosa fare in caso di F12
069000960415     C                   MOVE      *INKR         WINKR             1
069100940202     C                   WRITE     FMTT1
069200940224     C                   EXSR      GESPIE
069300940202     C                   MOVE      $LASTV        $LASTG
069400940202     C                   MOVE      'D1'          $LASTV
069500940707     C                   ENDIF
069600940707     C*
069700940707     C* se esistono errori sulla videata
069800940707     C* emetto la write del formato a indicatori spenti per vedere
069900940707     C* le eventuali decodifiche
070000940707     C     *IN99         IFEQ      *ON
070100940707     C                   MOVEA     *IN           WIN
070200940707     C                   MOVE      *ALL'0'       IN5098           49
070300940707     C                   MOVEA     IN5098        *IN(50)
070400940707     C                   WRITE     FMTD1
070500940707     C                   MOVEA     WIN           *IN
070600940707     C                   ENDIF
070700940131     C*
070800940131     C                   EXFMT     FMTD1
070900940506     C*
071000941205     C* controllo abilitazione tasti funzione
071100940506     C                   EXSR      CTRKD1
071200940506     C*
071300940506     C     *IN99         IFEQ      *OFF
071400940131     C*
071500940117     C                   SELECT
071600940916     C* F2=Decodifica
071700940916     C     $TASTO        WHENEQ    F02
071800940916     C                   EXSR      F02D1
071900940131     C* F3=Fine
072000940506     C     $TASTO        WHENEQ    F03
072100940307     C                   EXSR      F03D1
072200940131     C* F4=Ricerca
072300940506     C     $TASTO        WHENEQ    F04
072400940127     C                   EXSR      SEARCH
072500940916     C* F5=Refresh
072600940916     C     $TASTO        WHENEQ    F05
072700940916     C                   EXSR      F05D1
072800941206     C* F8=Successivo
072900941206     C     $TASTO        WHENEQ    F08
073000941206     C                   EXSR      F08D1
073100940131     C* F12=Ritorno
073200940506     C     $TASTO        WHENEQ    F12
073300940307     C                   EXSR      F12D1
073400940131     C* F18=Modifica valori assunti
073500940506     C     $TASTO        WHENEQ    F18
073600940307     C                   EXSR      F18D1
073700940224     C* F24=Altri tasti
073800940506     C     $TASTO        WHENEQ    F24
073900940309     C                   EXSR      F24D1
074000940131     C*
074100940117     C                   OTHER
074200940127     C                   EXSR      CTRD1
074300940127     C     *IN99         IFEQ      *OFF
074400940131     C* definisco l'ordinamento
074500940131     C                   SELECT
074600940314     C* << nel caso di più viste logiche e quindi più campi di
074700940314     C*    parzializzazione, qui vanno definiti i criteri per
074800940314     C*    determinare quale VL va letta, a seconda di quali campi
074900940314     C*    sono stati valorizzati >>
075000941215     C     D1KSC         WHENNE    *BLANK
075100940131     C                   Z-ADD     1             $ORD
075200941104     C* imposto l'ordinamento 99 per ottenere in selezione solo il
075300941104     C* rcd con la chiave passata
075400941214     C     $UNIQ         WHENEQ    *ON
075500940831     C                   Z-ADD     99            $ORD
075600940131     C* nel caso non sia stato digitato nulla uso l'ordinamento
075700940131     C* dei valori assunti
075800940131     C                   ENDSL
075900940127     C                   MOVE      'S1'          $GEST
076000940223     C                   MOVE      *ON           $INZS1
076100940302     C                   MOVE      *BLANK        W1POS
076200940211     C                   ENDIF
076300940224     C*
076400940117     C                   ENDSL
076500940224     C*
076600940506     C                   ENDIF
076700940117     C                   ENDSR
076800940224     C/EJECT
076900940224     C************************************************************
077000940224     C* INIZIALIZZAZIONE VIDEATA PARZIALIZZAZIONI
077100940224     C************************************************************
077200940127     C     INZD1         BEGSR
077300940318     C*
077400940318     C                   CLEAR                   FMTD1
077500941013     C* Se pgm  richiamato con parametri forniti , li imposto nelle
077600941013     C*    parzializzazioni
077700941104     C                   SELECT
077800941213     C* il pgm è richiamato con chiave univoca per gestione POSTA
077900941214     C     $UNIQ         WHENEQ    *ON
078000941221     C                   MOVE      UQ1311        D1KSC
078100951011     C                   MOVE      UQ3311        D1NRCH
078200951011     C                   MOVE      UQ4311        D1DTCH
078300941104     C* il pgm è richiamato e
078400941104     C* sono stati passati dei valori di parzializzazione ma non
078500941104     C* la chiave univoca
078600941221     C     OPZ311        WHENNE    *BLANK
078700941222     C                   MOVEL     KSC311        D1KSC
078800941104     C*--- impostare i valori passati sui campi a video
078900941104     C* il pgm non è richiamato
079000941221     C     OPZ311        WHENEQ    *BLANK
079100941104     C*--- imposto dei valori di default
079200941215     C*                    MOVE '????'    D1MOD
079300941115     C                   ENDSL
079400940224     C*
079500940117     C                   ENDSR
079600940506     C/EJECT
079700940506     C************************************************************
079800940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
079900940609     C* INPUT
080000941205     C* $FC1 = Abilitazioni videata D1
080100940509     C* OUTPUT
080200940509     C* *IN97 *IN99 Errori
080300940506     C************************************************************
080400940506     C     CTRKD1        BEGSR
080500941205     C*
080600940506     C                   SETOFF                                       99
080700940506     C                   MOVEA     $FC1          $FC
080800940506     C                   EXSR      CTRKEY
080900940506     C     *IN99         IFEQ      *ON
081000940506     C                   SETON                                        97
081100940506     C                   ENDIF
081200941205     C*
081300940506     C                   ENDSR
081400940916     C/EJECT
081500940916     C************************************************************
081600940916     C* GESTIONE F02 VIDEO D1
081700940916     C************************************************************
081800940916     C     F02D1         BEGSR
081900940916     C*
082000940916     C* per decodificare la videata richiamo la routine dei ctrl
082100940916     C* e poi disattivo gli eventuali messaggi di errore
082200940916     C                   EXSR      CTRD1
082300940916     C*
082400940916     C                   MOVE      *ALL'0'       IN5098
082500940916     C                   MOVEA     IN5098        *IN(50)
082600940916     C*
082700940916     C                   ENDSR
082800940224     C/EJECT
082900940131     C************************************************************
083000940307     C* GESTIONE F03 VIDEO D1
083100940131     C************************************************************
083200940307     C     F03D1         BEGSR
083300940131     C*
083400940307     C                   MOVE      *ON           $FINE
083500941221     C                   MOVE      '1'           RET311
083600940325     C* fine programma
083700940325     C                   EXSR      ENDPGM
083800940307     C*
083900940307     C                   ENDSR
084000940916     C/EJECT
084100940916     C************************************************************
084200940916     C* GESTIONE F05 VIDEO D1
084300940916     C************************************************************
084400940916     C     F05D1         BEGSR
084500940916     C*
084600940916     C                   MOVE      *ON           $INZD1
084700940916     C*
084800940916     C                   ENDSR
084900941206     C/EJECT
085000941206     C************************************************************
085100941206     C* GESTIONE F08 VIDEO D1
085200941206     C************************************************************
085300941206     C     F08D1         BEGSR
085400941206     C*
085500941206     C                   MOVE      *ON           $FINE
085600941221     C                   MOVE      '0'           RET311
085700941206     C*
085800941206     C                   ENDSR
085900940307     C/EJECT
086000940307     C************************************************************
086100940307     C* GESTIONE F12 VIDEO D1
086200940307     C************************************************************
086300940307     C     F12D1         BEGSR
086400940307     C*
086500940307     C*  se la 1a videata è la lista oppure sono arrivato qui con F17,
086600940307     C*  per F12 torno alla lista
086700940307     C     $LOP          IFEQ      2
086800940307     C     WINKR         OREQ      *ON
086900940307     C                   MOVE      'S1'          $GEST
087000940307     C*  altrimenti, per F12 torno a menu
087100940307     C                   ELSE
087200940307     C                   MOVE      *ON           $FINE
087300941221     C                   MOVE      '2'           RET311
087400941027     C                   ENDIF
087500940307     C*
087600940307     C                   ENDSR
087700940307     C/EJECT
087800940307     C************************************************************
087900940307     C* GESTIONE F18 VIDEO D1
088000940307     C************************************************************
088100940307     C     F18D1         BEGSR
088200940307     C*
088300940307     C                   MOVE      'D2'          $GEST
088400940307     C                   MOVEL     *ON           $INZD2
088500940307     C*
088600940307     C                   ENDSR
088700940307     C/EJECT
088800940307     C************************************************************
088900940307     C* GESTIONE F24 VIDEO D1
089000940609     C* input
089100940509     C* $ULKD1 = Ultimo tasto funzione emesso per video D1
089200940509     C* $FX1 =Testi tasti funzione
089300940509     C* $FC1 = Abilitazioni
089400940509     C* $FR1 = Inversioni di fondo
089500940307     C************************************************************
089600940307     C     F24D1         BEGSR
089700940506     C*
089800940506     C                   MOVEA     $FX1          $FX
089900940506     C                   MOVEA     $FC1          $FC
090000940506     C                   MOVEA     $FR1          $FR
090100940506     C                   Z-ADD     $ULKD1        $ULK
090200940929     C                   Z-ADD     79            $L01
090300940929     C                   Z-ADD     79            $L02
090400940506     C                   EXSR      RIEKEY
090500940506     C                   Z-ADD     $ULK          $ULKD1            3 0
090600940720     C                   MOVEA     $FC           $FC1
090700940506     C                   MOVEL     $KEY1         Z1KE1
090800060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
090900940506     C                   MOVEL     $KEY2         Z1KE2
091000940506     C                   WRITE     FMTZ1
091100940506     C*
091200940307     C*
091300940307     C                   ENDSR
091400940307     C/EJECT
091500940307     C************************************************************
091600940307     C* CONTROLLO VIDEATA PARZIALIZZAZIONI
091700940307     C************************************************************
091800940307     C     CTRD1         BEGSR
091900940307     C*
092000940127     C                   SETOFF                                       99
092100940127     C* non imposto pos. del cursore
092200940127     C                   Z-ADD     0             H1RIGA
092300940127     C                   Z-ADD     0             H1COLO
092400940131     C*
092500940314     C* << elencare gli eventuali controlli sui campi di
092600940314     C*    parzializzazione, ricordando che quasi sempre essi
092700940314     C*    non ne avranno perchè determinano solo il range di
092800940314     C*    scrittura della lista e quindi (nel caso di campi
092900940314     C*    alfanumerici) saranno trattati come chiavi parziali >>
093000940131     C*
093100940117     C                   ENDSR
093200940224     C/EJECT
093300940127     C************************************************************
093400940131     C* GESTIONE LISTA
093500940127     C************************************************************
093600940127     C     GESS1         BEGSR
093700940223     C* inizializzazione videata
093800940223     C     $INZS1        IFEQ      *ON
093900940127     C                   EXSR      INZS1
094000940223     C                   MOVE      *OFF          $INZS1
094100940127     C                   ENDIF
094200940223     C* emissione piede videata se proveniente da altra
094300940223     C* salvataggio valore formato di provenienza
094400940202     C     $LASTV        IFNE      'S1'
094500940202     C                   WRITE     FMTT1
094600940224     C                   EXSR      GESPIE
094700940202     C                   MOVE      $LASTV        $LASTG
094800940202     C                   MOVE      'S1'          $LASTV
094900941027     C                   ENDIF
095000940223     C*
095100940223     C     WMAX          IFEQ      0
095200940211     C                   WRITE     FMTD3                                        NO RCD
095300940211     C                   ENDIF
095400950109     C*Serve per emissione finestra pgm invio ANA727r (per open/close
095500940831     C*window)
095600940831     C                   MOVE      '1'           WIN727            1
095700940127     C*
095800940131     C                   EXFMT     FMTC1
095900940204     C     C1NRR         IFNE      0
096000940204     C                   Z-ADD     C1NRR         WSFL
096100940204     C                   ENDIF
096200940127     C                   Z-ADD     SFLNRR        C1RCD
096300940506     C*
096400940506     C* controllo abilitazione tasti funzione
096500940506     C                   EXSR      CTRKS1
096600941205     C*
096700940506     C     *IN99         IFEQ      *OFF
096800940127     C*
096900941027     C                   SELECT
097000940127     C* F3=Fine
097100940506     C     $TASTO        WHENEQ    F03
097200940309     C                   EXSR      F03S1
097300941206     C* F8=Successivo
097400941206     C     $TASTO        WHENEQ    F08
097500941206     C                   EXSR      F08S1
097600940131     C* F10=Immissione
097700940506     C     $TASTO        WHENEQ    F10
097800940309     C                   EXSR      F10S1
097900940131     C* F12=Ritorno
098000940506     C     $TASTO        WHENEQ    F12
098100940309     C                   EXSR      F12S1
098200940131     C* F17=Parzializzazioni
098300940506     C     $TASTO        WHENEQ    F17
098400940309     C                   EXSR      F17S1
098500940131     C* F18=Modifica valori assunti
098600940506     C     $TASTO        WHENEQ    F18
098700940309     C                   EXSR      F18S1
098800940224     C* F23=Altre opzioni
098900940506     C     $TASTO        WHENEQ    F23
099000940309     C                   EXSR      F23S1
099100940224     C* F24=Altri tasti
099200940506     C     $TASTO        WHENEQ    F24
099300940309     C                   EXSR      F24S1
099400940127     C* ROLL-UP
099500940506     C     $TASTO        WHENEQ    ROLLUP
099600940127     C                   EXSR      ROLS1
099700940127     C*
099800941027     C                   OTHER
099900940127     C* CONTROLLO DATI
100000940131     C                   EXSR      CTRC1
100100940201     C     *IN99         IFEQ      *OFF
100200940131     C                   EXSR      CTRS1
100300941027     C                   ENDIF
100400941205     C*
100500941027     C                   ENDSL
100600940127     C*
100700940506     C                   ENDIF
100800941205     C*
100900940127     C                   ENDSR
101000940224     C/EJECT
101100940127     C************************************************************
101200940131     C* INIZIALIZZAZIONE LISTA
101300940127     C************************************************************
101400940127     C     INZS1         BEGSR
101500940302     C* pulizia SFL
101600940128     C                   SETOFF                                         3031
101700940131     C                   WRITE     FMTC1
101800940128     C                   SETON                                          31
101900940127     C*
102000940128     C* riempimento opzioni
102100940223     C                   MOVEA     $TX1          $TX
102200940223     C                   MOVEA     $FL1          $FL
102300940309     C                   Z-ADD     0             WCOZ
102400940225     C*  equivale al num.di elementi della schiera $TX1
102500940509     C                   Z-ADD     $DIMOP        WMXO
102600940128     C                   EXSR      RIEOPZ
102700941214     C                   Z-ADD     23            X
102800941214     C                   MOVE      FF23          $FC2(X)
102900940608     C                   MOVEL     WOPZ1         C1OPZ1
103000060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
103100940608     C                   MOVEL     WOPZ2         C1OPZ2
103200940225     C                   Z-ADD     WCOZ          C1COZ
103300941221     C                   MOVEL     *BLANK        C1KSCD
103400941221     C                   MOVEL     *BLANK        C1IND
103500941221     C                   MOVEL     *BLANK        C1CAP
103600941221     C                   MOVEL     *BLANK        C1LOC
103700941221     C                   MOVEL     *BLANK        C1PRV
103800950914     C*
103900950914     C     COF311        IFEQ      'C'
104000950914     C* DECODICFICO CLIENTE
104100941222     C     D1KSC         IFNE      *BLANK
104200951011     C                   MOVEL     D1KSC         C1KSC
104300941221     C                   RESET                   DVCLI
104400950110     C                   MOVEL     WWWSOC        DCLSOC
104500941222     C                   MOVEL     D1KSC         DCLCLI
104600941221     C                   MOVEL     *BLANK        DCLLDV
104700941221     C                   MOVEL     *ZERO         DCLFIL
104800941221     C                   Z-ADD     1             DCLNRK
104900941221     C                   MOVEL     '1'           DCLRIC
105000941221     C                   MOVEL     CLI           DCLSET
105100941221     C                   MOVEL     '1'           DCLVIS
105200941221     C                   CALL      'DVCLI'
105300941221     C                   PARM                    DVCLI
105400941221     C                   PARM                    DVOCLI
105500941221     C     DCLERR        IFEQ      '0'
105600950526     C                   MOVEL     INDDES        C1KSCD
105700941221     C                   MOVEL     INDVIA        C1IND
105800941221     C                   MOVEL     INDCAP        C1CAP
105900941221     C                   MOVEL     INDCIT        C1LOC
106000941221     C                   MOVEL     INDPRV        C1PRV
106100941221     C                   ENDIF
106200941221     C                   ENDIF
106300950914     C                   ELSE
106400950914     C     COF311        IFEQ      'F'
106500951004     C                   SETON                                        11
106600950914     C* DECODICFICO FORNITORE
106700950914     C     D1KSC         IFNE      *BLANK
106800951011     C                   MOVEL     D1KSC         C1KSC
106900950914     C                   RESET                   DVFOR
107000950914     C                   MOVEL     WWWSOC        DFRSOC
107100950914     C                   MOVEL     D1KSC         DFRFOR
107200951011     C                   MOVEL     *ZERO         DFRFIL
107300951011     C                   Z-ADD     1             DFRNRK
107400951011     C                   MOVEL     '1'           DFRRIC
107500951011     C                   MOVEL     FRN           DFRSET
107600951011     C                   MOVEL     '1'           DFRVIS
107700950914     C                   CALL      'DVFOR'
107800950914     C                   PARM                    DVFOR
107900950914     C                   PARM                    DVOFRN
108000950914     C     DFRERR        IFEQ      '0'
108100950914     C                   MOVEL     FNDDES        C1KSCD
108200950914     C                   MOVEL     FNDVIA        C1IND
108300950914     C                   MOVEL     FNDCAP        C1CAP
108400950914     C                   MOVEL     FNDCIT        C1LOC
108500950914     C                   MOVEL     FNDPRV        C1PRV
108600950914     C                   ENDIF
108700950914     C                   ENDIF
108800950914     C                   ENDIF
108900950914     C                   ENDIF
109000940128     C*
109100940128     C* CARICAMENTO SFL A PAGINE
109200940128     C*
109300940201     C                   Z-ADD     0             S1NRR
109400940128     C                   Z-ADD     0             WMAX
109500940224     C*
109600940224     C* Posizionamento su file pilota
109700940302     C                   CLEAR                   C1POS
109800941214     C                   EXSR      SETANA
109900940302     C* ripristino i valori del posizionamento
110000940302     C                   MOVE      W1POS         C1POS
110100941215     C* aggiungere gestione altri campi posizionamento se ci sono
110200940131     C*
110300940201     C* Lettura file pilota
110400941214     C                   EXSR      REDANA
110500940131     C*
110600940131     C* Carico 1a pagina
110700940127     C                   EXSR      ROLS1
110800940201     C                   Z-ADD     SFLPAG        WPAG
110900940127     C*
111000940127     C                   ENDSR
111100940127     C/EJECT
111200940127     C************************************************************
111300940131     C* CARICAMENTO PAGINA LISTA
111400940127     C************************************************************
111500940127     C     ROLS1         BEGSR
111600940127     C*
111700940128     C                   SETOFF                                       32
111800940223     C                   Z-ADD     0             Y
111900940127     C                   Z-ADD     WMAX          S1NRR
112000940127     C*
112100940127     C* Leggo dal file anagrafico per caricare la lista
112200940127     C*
112300941027     C     $EFILE        DOWEQ     *OFF
112400940201     C     Y             ANDLT     WPAG
112500940127     C*
112600940131     C                   EXSR      RIES1
112700940127     C*
112800940127     C                   ADD       1             S1NRR
112900940127     C                   ADD       1             Y
113000940127     C*
113100940131     C                   WRITE     FMTS1
113200940131     C*
113300941214     C                   EXSR      REDANA
113400940128     C*
113500941027     C                   ENDDO
113600940127     C*
113700940223     C                   Z-ADD     S1NRR         WMAX                 30
113800940127     C*
113900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
114000940127     C*
114100940201     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
114200940127     C                   MVR                     RESTO             3 0
114300940131     C     PAGINE        MULT      SFLPAG        C1RCD
114400941027     C     RESTO         IFGT      0
114500940127     C                   ADD       1             C1RCD
114600941027     C                   ELSE
114700940407     C                   SUB       SFLPAG        C1RCD
114800940407     C                   ADD       1             C1RCD
114900941027     C                   ENDIF
115000940128     C*
115100940127     C                   ENDSR
115200940224     C/EJECT
115300940224     C************************************************************
115400940224     C* POSIZIONAMENTO RCD ARCHIVIO PILOTA
115500940224     C************************************************************
115600941214     C     SETANA        BEGSR
115700940224     C*
115800950110     C                   MOVEL     WWWSOC        DICSOC
115900951002     C                   MOVEL     COF311        DICCF
116000941222     C                   MOVEL     D1KSC         DICKSC
116100941222     C     $UNIQ         IFEQ      *OFF
116200020718A0957C     COF311        IFEQ      'F'
116300020718  "  C                   MOVEL     $DT999        DICDIN
116400020718A0957C                   ELSE
116500941222     C                   MOVEL     $DT0          DICDTC
116600020718A0957C                   ENDIF
116700941222     C                   ENDIF
116800940224     C                   SELECT
116900941214     C*
117000941214     C* Gestione posizionamento
117100941214     C* Se il campo W1POS è > *blank imposto il posizionamento con
117200941214     C* W1POS altrimenti lo imposto con il valore di parametrizzazione
117300941214     C*
117400940314     C* Ord.1 = chiave ordinamento 1
117500940224     C     $ORD          WHENEQ    1
117600941222     C     W1POS         IFNE      *ZERO
117700941222     C                   MOVEL     W1POS         DICNRC
117800940302     C                   ELSE
117900941222     C                   Z-ADD     *ZERO         DICNRC
118000941027     C                   ENDIF
118100020718A0957C     COF311        IFEQ      'F'
118200020718  "  C     K5DIC4        SETLL     ANDIC04
118300020718A0957C                   ELSE
118400971029     C     K5DIC3        SETLL     ANDIC03
118500020718A0957C                   ENDIF
118600940831     C* Ord.99= Chiave univoca
118700941104     C* (quando viene gestita la chiave univoca NON deve essere gestito
118800941104     C*  il posizionamento C1POS/W1POS)
118900940831     C     $ORD          WHENEQ    99
119000941222     C                   MOVEL     D1KSC         DICKSC
119100020718A0957C     COF311        IFEQ      'F'
119200020718  "  C     K5DIC4        SETLL     ANDIC04
119300020718A0957C                   ELSE
119400971029     C     K5DIC3        SETLL     ANDIC03
119500020718A0957C                   ENDIF
119600940224     C                   ENDSL
119700940224     C*
119800940224     C                   ENDSR
119900940128     C/EJECT
120000940128     C************************************************************
120100940131     C* LETTURA RCD ARCHIVIO PILOTA
120200940128     C************************************************************
120300941214     C     REDANA        BEGSR
120400940128     C*
120500940131     C                   MOVEL     *OFF          $EFILE
120600940131     C                   MOVEL     *OFF          $RCDOK
120700940131     C*
120800941027     C     $EFILE        DOUEQ     *ON
120900940131     C     $RCDOK        OREQ      *ON
121000940131     C*
121100941027     C                   SELECT
121200940131     C     $ORD          WHENEQ    1
121300020718A0957C     COF311        IFEQ      'F'
121400020718  "  C     WWWSOC        READE     ANDIC04                                21
121500020718A0957C                   ELSE
121600971029     C     WWWSOC        READE     ANDIC03                                21
121700020718A0957C                   ENDIF
121800940831     C     $ORD          WHENEQ    99
121900020718A0957C     COF311        IFEQ      'F'
122000020718  "  C                   READ      ANDIC04                                21
122100020718A0957C                   ELSE
122200971029     C                   READ      ANDIC03                                21
122300020718A0957C                   ENDIF
122400941027     C                   ENDSL
122500940131     C*
122600940131     C                   MOVE      *IN21         $EFILE
122700940131     C*
122800941027     C     $EFILE        IFEQ      *OFF
122900941214     C                   EXSR      SELANA
123000941027     C                   ENDIF
123100940131     C*
123200941027     C                   ENDDO
123300940131     C*
123400940131     C                   MOVE      *IN21         *IN33                          SflEnd
123500940131     C*
123600940131     C                   ENDSR
123700940224     C/EJECT
123800940224     C************************************************************
123900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
124000940224     C************************************************************
124100940224     C     CLCPAG        BEGSR
124200940224     C* Input :
124300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
124400940224     C*          cursore
124500940224     C* - SFLPAG = numero rcd per pagina sfl
124600940224     C* Output :
124700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
124800940224     C*
124900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
125000940224     C                   MVR                     RESTO             3 0
125100940224     C     RESTO         IFGT      0
125200940224     C                   ADD       1             PAGINE
125300940224     C                   ENDIF
125400940224     C     PAGINE        MULT      SFLPAG        WPAG
125500940224     C*
125600940224     C                   ENDSR
125700940131     C/EJECT
125800940131     C************************************************************
125900940131     C* SELEZIONI SULLA RIGA LETTA
126000940314     C* impostare a *OFF il campo $RCDOK per scartare un record che
126100940314     C* non rientra nei parametri di parzializzazione
126200940314     C* Eseguire una *HIVAL SETLL al raggiungimento di un limite
126300940314     C* superiore stabilito nei parametri
126400940314     C*
126500940131     C************************************************************
126600941214     C     SELANA        BEGSR
126700940131     C*
126800940202     C                   MOVEL     *ON           $RCDOK
126900940131     C                   MOVEL     *ALL'9'       ALL9             15
127000940314     C*
127100940314     C                   SELECT
127200940314     C*
127300940314     C* SELEZIONI RISPETTO AI VALORI RCD
127400940314     C*
127500940314     C* << elaborare eventuali selezioni che non riguardano le
127600940316     C*    parzializzazioni ma più propriamente i dati del rcd ;
127700940314     C*    esempio : esclusione degli annullati. >>
127800940314     C*
127900940314     C* SELEZIONI RISPETTO ALLE PARZIALIZZAZIONI
128000940314     C*
128100941215     C* Sotto conto non compreso nella scelta
128200961219     C     DICKSC        WHENNE    D1KSC
128300941215     C                   MOVEL     *OFF          $RCDOK
128400950113     C                   MOVE      *HIVAL        DICSOC
128500951002     C                   MOVE      *HIVAL        DICCF
128600950113     C                   MOVE      *HIVAL        DICKSC
128700020719     C*
128800020719A0957C     COF311        IFEQ      'F'
128900020719  "  C                   MOVE      *HIVAL        DICNIN
129000020719  "  C                   MOVE      $DT0          DICDIN
129100020719  "  C     K5DIC4        SETLL     ANDIC04
129200020719A0957C                   ELSE
129300950113     C                   MOVE      *HIVAL        DICNRC
129400950113     C                   MOVE      $DT999        DICDTC
129500971029     C     K5DIC3        SETLL     ANDIC03
129600020718A0957C                   ENDIF
129700940831     C*
129800941213     C* chiave diversa dall unique passata al pgm
129900941221     C     DICKSC        WHENNE    D1KSC
130000941213     C     $ORD          ANDEQ     99
130100940831     C                   MOVEL     *OFF          $RCDOK
130200950113     C                   MOVE      *HIVAL        DICSOC
130300951002     C                   MOVE      *HIVAL        DICCF
130400950113     C                   MOVE      *HIVAL        DICKSC
130500020719A0957C     COF311        IFEQ      'F'
130600020719  "  C                   MOVE      *HIVAL        DICNIN
130700020719  "  C                   MOVE      $DT0          DICDIN
130800020719  "  C     K5DIC4        SETLL     ANDIC04
130900020719A0957C                   ELSE
131000950113     C                   MOVE      *HIVAL        DICNRC
131100950113     C                   MOVE      $DT999        DICDTC
131200971029     C     K5DIC3        SETLL     ANDIC03
131300020718A0957C                   ENDIF
131400940316     C*
131500941215     C                   ENDSL
131600940202     C*
131700940202     C                   ENDSR
131800940131     C/EJECT
131900940131     C************************************************************
132000940131     C* RIEMPIMENTO RIGA LISTA
132100940131     C************************************************************
132200940131     C     RIES1         BEGSR
132300940131     C*
132400940131     C                   CLEAR                   FMTS1
132500940314     C* << riempio i campi del sfl, compresi gli hidden con i valori
132600940314     C*    del rcd letto >>
132700940131     C*
132800941221     C                   MOVEL     DICNRC        S1NRC
132900941221     C                   MOVEL     DICNIN        S1NIN
133000941221     C                   MOVEL     DICIVA        S1IVA
133100950130     C                   Z-ADD     DICIMP        S1IMP
133200941221     C* CTRL DATE
133300941221     C*
133400961004     C                   CALLb     'XDT4000'
133500941221     C                   PARM      DICDTC        XDTAMG           10
133600941221     C                   PARM                    XDTGMA            6 0
133700941221     C                   PARM      2             XDTSTA            1 0
133800941221     C     XDTSTA        IFEQ      0
133900941221     C                   MOVE      XDTGMA        S1DTC
134000941221     C                   ENDIF
134100941221     C*
134200961004     C                   CALLb     'XDT4000'
134300941221     C                   PARM      DICDTI        XDTAMG           10
134400941221     C                   PARM                    XDTGMA            6 0
134500941221     C                   PARM      2             XDTSTA            1 0
134600941221     C     XDTSTA        IFEQ      0
134700941221     C                   MOVE      XDTGMA        S1DTI
134800941221     C                   ENDIF
134900941221     C*
135000961004     C                   CALLb     'XDT4000'
135100941221     C                   PARM      DICDTF        XDTAMG           10
135200941221     C                   PARM                    XDTGMA            6 0
135300941221     C                   PARM      2             XDTSTA            1 0
135400941221     C     XDTSTA        IFEQ      0
135500941221     C                   MOVE      XDTGMA        S1DTF
135600941221     C                   ENDIF
135700941221     C*
135800961004     C                   CALLb     'XDT4000'
135900941221     C                   PARM      DICDIN        XDTAMG           10
136000941221     C                   PARM                    XDTGMA            6 0
136100941221     C                   PARM      2             XDTSTA            1 0
136200941221     C     XDTSTA        IFEQ      0
136300941221     C                   MOVE      XDTGMA        S1DIN
136400941221     C                   ENDIF
136500150113R160 C* gestione estensione dichiarazione d'intento
136600150113 "   C                   eval      DifSocieta = DicSoc
136700150113 "   C                   eval      DifCf      = DicCf
136800150113 "   C                   eval      DifNrInter = DicNin
136900150113 "   C                   eval      DifDtInter = DicDin
137000150113 "   C     K04Dif01      CHAIN     SdgDif01l
137100150113 "   C                   if        %found(SdgDif01l)
137200150113 "   C                   eval      H1File     = DifFileTel
137300150113R160 C                   endif
137400020701A0957C* della riga del sfl (se ce ne sono)
137500020701  "  C     DICDTR        IFNE      $DT0
137600020701  "  C                   EVAL      H1dtrevoc = *on
137700020701  "  C                   ELSE
137800020701  "  C                   EVAL      H1dtrevoc = *off
137900020701  "  C                   ENDIF
138000020701  "  C* imposto quegli attributi del sfl che sono modificabili
138100020701  "  C* al richiamo del determinato pgm
138200020701A0957C                   EXSR      ATRS1
138300941215     C*
138400940131     C                   ENDSR
138500941205     C/EJECT
138600941205     C************************************************************
138700941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA S1
138800941205     C* INPUT
138900941205     C* $FC2 = Abilitazioni videata S1
139000941205     C* OUTPUT
139100941205     C* *IN97 *IN99 Errori
139200941205     C************************************************************
139300941205     C     CTRKS1        BEGSR
139400941205     C*
139500941205     C                   SETOFF                                       99
139600941205     C                   MOVEA     $FC2          $FC
139700941205     C                   EXSR      CTRKEY
139800941205     C     *IN99         IFEQ      *ON
139900941205     C                   SETON                                        97
140000941205     C                   ENDIF
140100941205     C*
140200941205     C                   ENDSR
140300941206     C/EJECT
140400941206     C************************************************************
140500941206     C* GESTIONE F08 VIDEO S1
140600941206     C************************************************************
140700941206     C     F08S1         BEGSR
140800941206     C*
140900941206     C                   MOVE      *ON           $FINE
141000941221     C                   MOVE      '0'           RET311
141100941206     C*
141200941206     C                   ENDSR
141300940309     C/EJECT
141400940309     C************************************************************
141500940309     C* GESTIONE F03 VIDEO S1
141600940309     C************************************************************
141700940309     C     F03S1         BEGSR
141800940309     C*
141900940309     C                   MOVE      *ON           $FINE
142000941221     C                   MOVE      '1'           RET311
142100940325     C* fine programma
142200940325     C                   EXSR      ENDPGM
142300940309     C*
142400940309     C                   ENDSR
142500940309     C/EJECT
142600940309     C************************************************************
142700940309     C* GESTIONE F10 VIDEO S1
142800940609     C* AGGIUNTA RECORD
142900940309     C************************************************************
143000940309     C     F10S1         BEGSR
143100940309     C*
143200941221     C                   RESET                   A312DS
143300941221     C                   MOVEL     '01'          OPZ312
143400950113     C                   MOVEL     FSC311        FSC312
143500950914     C                   MOVEL     COF311        COF312
143600950113     C                   MOVEL     D1KSC         KSC312
143700980205     C     CMT311        IFEQ      '1'
143800980205     C                   MOVEL     '1'           CMT312
143900980205     C                   ELSE
144000980205     C                   MOVEL     '0'           CMT312
144100980205     C                   ENDIF
144200941221     C                   MOVEL     A312DS        KPJBU
144300941221     C                   CALL      'ANA312R'
144400940309     C                   PARM                    KPJBA
144500970704     c                   Parm                    AutDs
144600941221     C                   MOVEL     KPJBU         A312DS
144700940309     C* ritorno da PGM gestione
144800941221     C                   EXSR      RTN312
144900940309     C     WINZS1        IFEQ      *ON
145000940309     C                   MOVE      *ON           $INZS1
145100940309     C* se esistevano già righe sul sfl
145200940309     C* calcolo pagina a cui deve posizionarsi
145300940309     C     WSFL          IFGT      0
145400940309     C                   EXSR      CLCPAG
145500940309     C* altrimenti carico solo la 1a pagina
145600940309     C                   ELSE
145700940309     C                   Z-ADD     SFLPAG        WPAG
145800940309     C                   ENDIF
145900940309     C                   ENDIF
146000940309     C*
146100940627     C                   MOVE      *HIVAL        $LASTV
146200940627     C*
146300940309     C                   ENDSR
146400940309     C/EJECT
146500940309     C************************************************************
146600940309     C* GESTIONE F12 VIDEO S1
146700940309     C************************************************************
146800940309     C     F12S1         BEGSR
146900940309     C*
147000940309     C*  se la 1a videata sono le parzializzazioni, per F12 torno a queste
147100940309     C     $LOP          IFEQ      1
147200940309     C                   MOVE      'D1'          $GEST
147300940309     C*  se la 1a videata è questa, per F12 torno a menu
147400940309     C                   ELSE
147500940309     C                   MOVE      *ON           $FINE
147600941221     C                   MOVE      '2'           RET311
147700941027     C                   ENDIF
147800940309     C*
147900940309     C                   ENDSR
148000940309     C/EJECT
148100940309     C************************************************************
148200940309     C* GESTIONE F17 VIDEO S1
148300940309     C************************************************************
148400940309     C     F17S1         BEGSR
148500940309     C*
148600940309     C                   MOVE      'D1'          $GEST
148700941104     C* se il pgm è stato richiamato con chiave univoca
148800941104     C* e dopo è stata richiesta una parzializzazione
148900941104     C* tolgo la gestione della chiave univoca (azzere/abblenco il
149000941104     C* valore; propongo come ordinamento il 1°)
149100941213     C     $UNIQ         IFEQ      *ON
149200941221     C                   MOVE      *BLANK        UQ1311
149300941221     C                   MOVE      *BLANK        UQ2311
149400941221     C                   MOVE      *BLANK        UQ3311
149500951011     C                   MOVE      *BLANK        UQ3311
149600941027     C                   ENDIF
149700940831     C     $ORD          IFEQ      99
149800940831     C                   Z-ADD     1             $ORD
149900941104     C                   MOVE      *ON           $INZD1
150000941027     C                   ENDIF
150100940309     C*
150200940309     C                   ENDSR
150300940309     C/EJECT
150400940309     C************************************************************
150500940309     C* GESTIONE F18 VIDEO S1
150600940309     C************************************************************
150700940309     C     F18S1         BEGSR
150800940309     C*
150900940309     C                   MOVE      'D2'          $GEST
151000940309     C                   MOVEL     *ON           $INZD2
151100940831     C     $ORD          IFEQ      99
151200940831     C                   Z-ADD     1             $ORD
151300941027     C                   ENDIF
151400940309     C*
151500940309     C                   ENDSR
151600940309     C/EJECT
151700940309     C************************************************************
151800940309     C* GESTIONE F23 VIDEO S1
151900940309     C************************************************************
152000940309     C     F23S1         BEGSR
152100940309     C*
152200940309     C                   EXSR      RIEOPZ
152300941214     C                   Z-ADD     23            X
152400941214     C                   MOVE      FF23          $FC2(X)
152500940608     C                   MOVEL     WOPZ1         C1OPZ1
152600060224A1148C                   MOVEL     $ALLOPT       H1ALLOPT
152700940608     C                   MOVEL     WOPZ2         C1OPZ2
152800940309     C                   Z-ADD     WCOZ          C1COZ
152900940309     C*
153000940309     C                   ENDSR
153100940309     C/EJECT
153200940309     C************************************************************
153300940309     C* GESTIONE F24 VIDEO S1
153400940309     C************************************************************
153500940309     C     F24S1         BEGSR
153600940309     C*
153700940506     C*
153800940506     C                   MOVEA     $FX2          $FX
153900940506     C                   MOVEA     $FC2          $FC
154000940506     C                   MOVEA     $FR2          $FR
154100940506     C                   Z-ADD     $ULKS1        $ULK
154200940929     C                   Z-ADD     79            $L01
154300940929     C                   Z-ADD     79            $L02
154400940506     C                   EXSR      RIEKEY
154500940506     C                   Z-ADD     $ULK          $ULKS1            3 0
154600940720     C                   MOVEA     $FC           $FC2
154700940506     C                   MOVEL     $KEY1         Z2KE1
154800060224A1148C                   MOVEL     $ALLFUNCT     H2ALLFUNCT
154900940506     C                   MOVEL     $KEY2         Z2KE2
155000940506     C                   WRITE     FMTZ2
155100940506     C*
155200940309     C                   ENDSR
155300940128     C/EJECT
155400940128     C************************************************************
155500940131     C* CONTROLLO TESTATA LISTA
155600940128     C************************************************************
155700940131     C     CTRC1         BEGSR
155800940128     C*
155900940201     C                   MOVE      *OFF          *IN99
156000940131     C*
156100940202     C* SE RICHIESTO UN INIZIO ELENCO PARTICOLARE ATTIVO
156200940202     C* L'INIZIALIZZAZIONE LISTA
156300940202     C*
156400941214     C     C1POS         IFNE      W1POS
156500941215     C* aggiungere in OR altri campi posizionamento se ci sono
156600940202     C                   EXSR      POSS1
156700941027     C                   ENDIF
156800940202     C*
156900940202     C                   ENDSR
157000940202     C/EJECT
157100940202     C************************************************************
157200940202     C* CONTROLLI PER POSIZIONAMENTO SU LISTA
157300940202     C************************************************************
157400940202     C     POSS1         BEGSR
157500940202     C*
157600940202     C* CONTROLLO SE ESISTONO OPZIONI ATTIVE
157700940202     C*
157800941118     C     WMAX          IFGT      0
157900940202     C                   READC     FMTS1                                  21
158000941118     C     *IN21         DOWEQ     *OFF
158100940202     C     *IN99         ANDEQ     *OFF
158200940202     C*
158300941118     C     S1OPZ         IFNE      0
158400940202     C                   SETON                                        51  99
158500941118     C                   ENDIF
158600940202     C* se rilevata opzione attiva, attivo sflnxtchg
158700941118     C     *IN99         IFEQ      *ON
158800940202     C                   MOVE      *ON           *IN32
158900941118     C* imposta attributi della riga di sfl
159000941118     C                   EXSR      ATRS1
159100941118     C*
159200940202     C                   UPDATE    FMTS1
159300941118     C                   ELSE
159400940202     C                   READC     FMTS1                                  21
159500941118     C                   ENDIF
159600940202     C*
159700941118     C                   ENDDO
159800940202     C*
159900941118     C                   ENDIF
160000940202     C*
160100940221     C* SE CTRL OK, DETERMINO QUALE CARICAMENTO DEVO USARE RISPETTO
160200940221     C* AL VALORE DI POSIZIONAMENTO :
160300940221     C* se questo è maggiore del valore del rispettivo campo di
160400940221     C* parzializzazione lo uso, altrimenti eseguo un ricaricamento
160500940221     C* con i valori di parzializzazione.
160600940221     C*
160700941118     C     *IN99         IFEQ      *OFF
160800941222     C                   Z-ADD     *ZERO         W1POS
160900940202     C*
161000941118     C                   SELECT
161100940202     C*
161200940202     C     $ORD          WHENEQ    1
161300940202     C*
161400941222     C*          C1POS     IFGT XXXXX
161500941222     C                   Z-ADD     C1POS         W1POS
161600941215     C* sostituire C1POS/W1POS con altri campi posizionamento
161700941215     C* se il campo in parzializzazione ne richiede uno specifico
161800941222     C*                    ENDIF
161900940131     C*
162000940202     C                   MOVE      *ON           *IN99
162100940223     C                   MOVE      *ON           $INZS1
162200940831     C*
162300940831     C     $ORD          WHENEQ    99
162400940831     C*
162500940831     C                   MOVE      *ON           *IN99
162600940831     C                   MOVE      *ON           $INZS1
162700940831     C*
162800940202     C*
162900941118     C                   ENDSL
163000940202     C*
163100941118     C                   ENDIF
163200940202     C*
163300940131     C                   ENDSR
163400940131     C/EJECT
163500940131     C************************************************************
163600940131     C* CONTROLLO OPZIONI LISTA
163700940131     C************************************************************
163800940131     C     CTRS1         BEGSR
163900940131     C*
164000940202     C                   MOVEL     *OFF          $ESCI
164100940201     C                   SETOFF                                       99
164200940131     C                   Z-ADD     0             S1OPZ
164300940131     C*
164400940127     C* Leggo il sfl solo se ci sono rcd
164500941118     C     WMAX          IFGT      0
164600940131     C                   READC     FMTS1                                  21
164700940127     C*
164800940131     C* esce se fine sfl o errore che richiede l'uscita
164900941118     C     *IN21         DOWEQ     *OFF
165000940131     C     $ESCI         ANDEQ     *OFF
165100940201     C                   Z-ADD     S1NRR         C1RCD
165200940131     C* ctrl su riga
165300940131     C                   EXSR      RECS1
165400940131     C* gestione opzioni
165500941118     C     S1OPZ         IFNE      0
165600940201     C     *IN99         ANDEQ     *OFF
165700940131     C                   EXSR      OPZS1
165800941118     C                   ENDIF
165900940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
166000941118     C     *IN99         IFEQ      *ON
166100940131     C                   MOVE      *ON           *IN32
166200941118     C                   ENDIF
166300941118     C* imposta attributi della riga di sfl
166400941118     C                   EXSR      ATRS1
166500941118     C*
166600940131     C                   Z-ADD     0             S1OPZ
166700940223     C*
166800940127     C                   UPDATE    FMTS1
166900941125     C                   MOVE      *OFF          *IN32
167000940223     C*
167100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
167200941118     C     $ESCI         IFEQ      *OFF
167300940131     C                   READC     FMTS1                                  21
167400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
167500941118     C     *IN21         IFEQ      *ON
167600940201     C                   MOVE      WINZS1        $INZS1
167700940204     C* calcolo pagina a cui deve posizionarsi
167800940728     C     WSFL          IFGT      0
167900940728     C                   EXSR      CLCPAG
168000940728     C                   ELSE
168100940728     C                   Z-ADD     SFLPAG        WPAG
168200940728     C                   ENDIF
168300941118     C                   ENDIF
168400941118     C                   ENDIF
168500940131     C*
168600941118     C                   ENDDO
168700940127     C*
168800941118     C                   ENDIF
168900940131     C*
169000940127     C                   ENDSR
169100940127     C/EJECT
169200940127     C************************************************************
169300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
169400940127     C************************************************************
169500940131     C     RECS1         BEGSR
169600940131     C*
169700940201     C* reset indicatori DSPATR
169800940201     C                   MOVE      *ALL'0'       IN4049           10
169900940201     C                   MOVEA     IN4049        *IN(40)
170000940201     C*
170100940131     C* Controllo se Utente Autorizzato alla scelta
170200940202     C     S1OPZ         IFNE      0
170300940131     C                   Z-ADD     1             X
170400940223     C     S1OPZ         LOOKUP    $OP1(X)                                21
170500940211     C     *IN21         IFEQ      *OFF
170600940223     C     $FL1(X)       OREQ      'N'
170700940131     C                   MOVE      *ON           $ESCI
170800940201     C                   SETON                                        504099
170900940131     C                   ENDIF
171000940202     C                   ENDIF
171100940131     C*
171200940131     C                   ENDSR
171300940224     C/EJECT
171400940131     C************************************************************
171500941118     C* RIPRISTINA GLI ATTRIBUTI DELLA RIGA DEL SFL
171600940131     C************************************************************
171700941118     C     ATRS1         BEGSR
171800940201     C*
171900941215     C* impostare le condizioni per ripristinare gli attributi
172000941215     C* della riga del sfl (se ce ne sono)
172100020701A0957C*
172200020823  "  C* presenza data revoca
172300020701  "  C                   IF        H1Dtrevoc = *on
172400020701  "  C                   SETON                                        41
172500020701  "  C                   ELSE
172600020701  "  C                   SETOFF                                       41
172700020701A0957C                   ENDIF
172800941118     C*
172900941118     C                   ENDSR
173000941118     C/EJECT
173100941118     C************************************************************
173200941118     C* GESTIONE OPZIONI LISTA
173300941118     C************************************************************
173400941118     C     OPZS1         BEGSR
173500941118     C*
173600941027     C                   SELECT
173700940225     C*
173800940225     C* se opz=Selezione passo la chiave al pgm chiamante
173900940225     C     S1OPZ         WHENEQ    1
174000941221     C* << passare nei campi appositi della DS D311DS il valore
174100940314     C*    della chiave della VL univoca (se c'è) >>
174200941221     C                   MOVE      S1OPZ         OPZ311
174300950224     C                   MOVE      C1KSC         KSC311
174400950224     C                   MOVE      S1NRC         NRC311
174500941221     C*
174600961004     C                   CALLb     'XDT4000'
174700941221     C                   PARM                    XDTAMG           10
174800941221     C                   PARM      S1DTC         XDTGMA            6 0
174900941222     C                   PARM      4             XDTSTA            1 0
175000941221     C     XDTSTA        IFEQ      0
175100950224     C                   MOVE      XDTAMG        DTC311
175200941221     C                   ENDIF
175300941221     C*
175400940225     C                   MOVE      *ON           $ESCI
175500940225     C                   MOVE      *ON           $FINE
175600940909     C*
175700940909     C                   MOVE      *HIVAL        $LASTV
175800150113R160 C*
175900150113 "   C* se opz=Stampa
176000150113 "   C     S1OPZ         WHENEQ    6
176100150113 "   C                   EXSR      StampaModulo
176200150113R160 C                   MOVE      *HIVAL        $LASTV
176300150113     C*
176400940831     C* se opz=Invia scrive nella posta
176500940831     C     S1OPZ         WHENEQ    10
176600941215     C*                    RESETA727DS
176700941215     C*                    MOVE '02'      OPZ727
176800941215     C*                    MOVE WIN727    INZ727
176900941215     C*                    MOVELS1NRG     TXT727
177000941215     C*                    MOVE S1DRG     TXT727
177100941215     C*                    MOVEL'CNT'     TPA727
177200941215     C*                    MOVELH1PRG     PRGKEY
177300941215     C*                    MOVE H1RIG     RIGKEY
177400941215     C*                    MOVE DSKEY     KEY727
177500941215     C*                    MOVE *BLANKS   USO727
177600941215     C*                    MOVE 'E'       PGM727
177700941215     C*                    CLEARKPJBU
177800941215     C*                    MOVELA727DS    KPJBU
177900941215     C*                    CALL 'ANA727R'
178000941215     C*                    PARM           KPJBA
178100941215     C*                    MOVELKPJBU     A727DS
178200941215     C*                    MOVE ' '       WIN727
178300940909     C*
178400941215     C*                    MOVE *HIVAL    $LASTV
178500940831     C*
178600940831     C* se opz=Commenti richiamo pgm gestione commenti
178700940831     C     S1OPZ         WHENEQ    14
178800941215     C*                    RESETA725DS
178900941221     C*          OPZ311    IFEQ '04'
179000941221     C*          OPZ311    OREQ '05'
179100941215     C* oppure se l'utente è abilitato per la funzione specifica di
179200941215     C* questa anagrafica solo alla visualizzazione
179300941215     C*                    MOVE '05'      OPZ725
179400941215     C*                    ELSE
179500941215     C*                    MOVE '02'      OPZ725
179600941215     C*                    ENDIF
179700950116     C*                    MOVE FSC311    FSC725
179800950116     C*                    MOVE 'YYY'     TPA725
179900941117     C* inserire chiavi commenti
180000941215     C*                    MOVE XXXXX     KY1725
180100941215     C*                    MOVE XXXXX     KY2725
180200941215     C*                    MOVE XXXXX     KY3725
180300941215     C*                    MOVE XXXXX     KY4725
180400941215     C*                    MOVE *BLANK    KPJBU
180500941215     C*                    MOVELA725DS    KPJBU
180600941215     C*                    CALL 'ANA725R'
180700941215     C*                    PARM           KPJBA
180800941215     C*                    MOVELKPJBU     A725DS
180900941124     C*
181000941124     C* ritorno da PGM commenti
181100941215     C*                    EXSR RTN725
181200940831     C*
181300941215     C*                    MOVE *HIVAL    $LASTV
181400940225     C*
181500940225     C* se altre opzioni (o nessuna opzione) richiamo pgm gestione
181600941027     C                   OTHER
181700940225     C*
181800941221     C                   RESET                   A312DS
181900941221     C* << passare nei campi appositi della DS A312DS il valore
182000940314     C*    della chiave della VL univoca (se c'è) >>
182100941221     C                   MOVEL     S1OPZ         OPZ312
182200941221     C*          CMT311    IFEQ '1'
182300941221     C*                    MOVEL'1'       CMT312
182400941221     C*                    ELSE
182500941221     C*                    MOVEL'0'       CMT312
182600941221     C*                    ENDIF
182700980121     C     CMT311        IFEQ      '1'
182800980121     C                   MOVEL     '1'           CMT312
182900980121     C                   ELSE
183000980121     C                   MOVEL     '0'           CMT312
183100980121     C                   ENDIF
183200950113     C                   MOVEL     FSC311        FSC312
183300950914     C                   MOVEL     COF311        COF312
183400941221     C                   MOVEL     C1KSC         KSC312
183500970725     C                   MOVEL     S1NIN         NRI312
183600941221     C*
183700961004     C                   CALLb     'XDT4000'
183800941222     C                   PARM                    XDTAMG           10
183900970725     C                   PARM      S1DIN         XDTGMA            6 0
184000941222     C                   PARM      4             XDTSTA            1 0
184100941221     C     XDTSTA        IFEQ      0
184200970725     C                   MOVE      XDTAMG        DTI312
184300941221     C                   ENDIF
184400940314     C*
184500941221     C                   MOVEL     A312DS        KPJBU
184600941221     C                   CALL      'ANA312R'
184700940314     C                   PARM                    KPJBA
184800970704     c                   Parm                    AutDs
184900941221     C                   MOVEL     KPJBU         A312DS
185000940201     C*
185100940223     C* ritorno da PGM gestione
185200941221     C                   EXSR      RTN312
185300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
185400941027     C     *IN99         IFEQ      *ON
185500940223     C                   SETON                                        40
185600941027     C                   ENDIF
185700940131     C*
185800940506     C                   MOVE      *HIVAL        $LASTV
185900941027     C                   ENDSL
186000940225     C*
186100940131     C                   ENDSR
186200940223     C/EJECT
186300940223     C************************************************************
186400940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
186500940223     C************************************************************
186600941221     C     RTN312        BEGSR
186700940223     C*
186800940223     C* modo di ritorno
186900940223     C*
187000941027     C                   SELECT
187100940314     C* << questi modi di utilizzo dei valori di ritorno dal
187200940314     C*    pgm di manutenzione rcd di anagrafica sono delle
187300940314     C*    proposte, normalmente sempre valide, ma modificabili
187400940314     C*    per situazioni particolari >>
187500940223     C* 1 = F3
187600941221     C     RET312        WHENEQ    '1'
187700940224     C                   MOVE      *ON           $ESCI                          sospese READC
187800940223     C                   MOVE      *ON           $FINE                          fine pgm
187900941230     C                   MOVE      '1'           RET311
188000940223     C* 2 = F12
188100941221     C     RET312        WHENEQ    '2'
188200940223     C                   MOVE      *ON           $ESCI                          sospese READC
188300940223     C*
188400941027     C                   ENDSL
188500940223     C*
188600940223     C* operazione eseguite dal pgm chiamato
188700940223     C*
188800941027     C                   SELECT
188900940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
189000941221     C     OPR312        WHENEQ    '1'
189100940223     C                   MOVE      *ON           WINZS1                         iniz.sfl fine ciclo
189200941221     C                   MOVE      OPR312        WOPR                           avverto pgm chiamant
189300020701A0957C                   MOVE      *ON           $INZS1                         iniz.sfl fine ciclo
189400940223     C*
189500941027     C                   ENDSL
189600020701     C*
189700940223     C* funzione non eseguibile per errore :
189800940223     C*
189900941027     C                   SELECT
190000940223     C* 1 = funzione richiamata chiusa in errore
190100940316     C*  eventualmente gestire altri codici di errore
190200941221     C     ERR312        WHENEQ    '1'
190300940223     C                   MOVE      *ON           $ESCI                          sospese READC
190400940223     C                   SETON                                        5299      emiss.SFLMSGID
190500941027     C*
190600941027     C                   ENDSL
190700940223     C*
190800940223     C                   ENDSR
190900941124     C/EJECT
191000010314a0871x************************************************************
191100010314  "  x* GESTIONE RITORNO DA PGM DEI COMMENTI
191200010314  "  x************************************************************
191300010314  "  x*    RTN725        BEGSR
191400010314  "  x*
191500010314  "  x* modo di ritorno
191600010314  "  x*
191700010314  "  x*                  SELECT
191800010314  "  x* << questi modi di utilizzo dei valori di ritorno dal
191900010314  "  x*    pgm di manutenzione rcd di anagrafica sono delle
192000010314  "  x*    proposte, normalmente sempre valide, ma modificabili
192100010314  "  x*    per situazioni particolari >>
192200010314  "  x* 1 = F3
192300010314  "  x*    RET725        WHENEQ    '1'
192400010314  "  x*                  MOVE      *ON           $ESCI                          sospese READC
192500010314  "  x*                  MOVE      *ON           $FINE                          fine pgm
192600010314  "  x* 2 = F12
192700010314  "  x*    RET725        WHENEQ    '2'
192800010314  "  x*                  MOVE      *ON           $ESCI                          sospese READC
192900010314  "  x*
193000010314  "  x*                  ENDSL
193100010314  "  x*
193200010314  "  x* operazione eseguite dal pgm chiamato
193300010314  "  x*
193400010314  "  x*                  SELECT
193500010314  "  x* 1 = eseguita scrittura o modifica commento
193600010314  "  x*    OPR725        WHENEQ    '1'
193700010314  "  x* segnalare su sfl guida l'avvenuto aggiornamento                e ciclo
193800010314  "  x*
193900010314  "  x*                  ENDSL
194000010314  "  x*
194100010314  "  x* funzione non eseguibile per errore :
194200010314  "  x*
194300010314  "  x*                  SELECT
194400010314  "  x* 1 = funzione richiamata chiusa in errore
194500010314  "  x*  eventualmente gestire altri codici di errore
194600010314  "  x*    ERR725        WHENEQ    '1'
194700010314  "  x*                  MOVE      *ON           $ESCI                          sospese READC
194800010314  "  x*                  SETON                                        5299      emiss.SFLMSGID
194900010314  "  x*
195000010314  "  x*                  ENDSL
195100010314  "  x*
195200010314  "  x*                  ENDSR
195300010314a0871x*EJECT
195400150113R160 C************************************************************
195500150113 "   C* Stampa su modulo
195600150113R160 C************************************************************
195700150113     C     StampaModulo  BEGSR
195800150113     C*
195900150113     C                   reset                   SdgSj63Ds
196000150113     C*
196100150113     C                   eval      SocietaJ63  = XscSoc
196200150113     C                   eval      FileJ63     = H1File
196300150113     C                   eval      TipoStpJ63  = '1'
196400150120     C                   MoveL(P)  S1Nin         WCod
196500150120     C                   EXSR      TogliZero
196600150120     C                   Eval      NrInte1J63  = WNrInter
196700150120     C                   eval      NrInte2J63  = NrInte1J63
196800150113     C*
196900150113     C                   eval      Kpjbu       = SdgSj63Ds
197000150113     C                   call      'SDGSJ63R'
197100150113     C                   parm                    Kpjba
197200150113     C                   eval      SdgSj63Ds   = Kpjbu
197300150113     C*
197400150113     C                   exsr      RtnJ63
197500150113     C*
197600150113     C                   ENDSR
197700150113     C/EJECT
197800150113R160 C************************************************************
197900150113 "   C* GESTIONE RITORNO DA PGM DI GESTIONE
198000150113R160 C************************************************************
198100150113     C     RTNJ63        BEGSR
198200150113     C*
198300150113     C* modo di ritorno
198400150113     C*
198500150113     C                   SELECT
198600150113     C* questi modi di utilizzo dei valori di ritorno dal
198700150113     C* pgm di manutenzione rcd di anagrafica sono delle
198800150113     C* proposte, normalmente sempre valide, ma modificabili
198900150113     C* per situazioni particolari
199000150113     C* 1 = F3
199100150113     C     RETJ63        WHENEQ    '1'
199200150113     C                   MOVE      *ON           $ESCI                          sospese READC
199300150113     C                   MOVE      *ON           $FINE                          fine pgm
199400150113     C                   MOVE      '2'           RET311
199500150113     C* 2 = F12
199600150113     C     RETJ63        WHENEQ    '2'
199700150113     C                   MOVE      *ON           $ESCI                          sospese READC
199800150113     C*
199900150113     C                   ENDSL
200000150113     C*
200100150113     C* operazione eseguite dal pgm chiamato
200200150113     C*
200300150113     C* funzione non eseguibile per errore :
200400150113     C*
200500150113     C                   SELECT
200600150113     C* 1 = funzione richiamata chiusa in errore
200700150113     C     ERRJ63        WHENEQ    '1'
200800150113     C                   MOVE      *ON           $ESCI                          sospese READC
200900150113     C                   SETON                                        5299      emiss.SFLMSGID
201000150113     C*
201100150113     C                   ENDSL
201200150113     C*
201300150113R160 C                   ENDSR
201400150113R160 C/EJECT
201500150120R160 C************************************************************
201600150120 "   C* Tolgo gli zeri e metto *Blank
201700150120R160 C************************************************************
201800150120     C     TogliZero     BEGSR
201900150120     C*
202000150120     C                   If        WCod        > *Blank
202100150120     C*
202200150120     C* Sostituisco gli zeri a sx non significativi con dei blank
202300150120     C                   MoveA     WCod          SkCod
202400150120     C                   Do        6             $X                2 0
202500150120     C                   If        SkCod($X)='0'
202600150120     C                   Move      *Blank        SkCod($X)
202700150120     C                   Else
202800150120     C                   Leave
202900150120     C                   EndIf
203000150120     C                   EndDo
203100150120     C                   MoveA     SkCod         WCod
203200150120     C*
203300150120     C* Elimino i blank a dx
203400150120     C                   Move(P)   WCod          WNrInter
203500150120     C*
203600150120     C                   EndIf
203700150120R160 C*
203800150120 "   C                   ENDSR
203900150120R160 C/EJECT
204000940131     C************************************************************
204100940131     C* GESTIONE VALORI ASSUNTI
204200940131     C************************************************************
204300940131     C     GESD2         BEGSR
204400940131     C*
204500941118     C     $INZD2        IFEQ      *ON
204600940131     C                   EXSR      INZD2
204700940223     C                   MOVE      *OFF          $INZD2
204800941118     C                   ENDIF
204900940202     C*
205000941118     C     $LASTV        IFNE      'D2'
205100940923     C                   EXSR      GESPIE
205200940202     C                   MOVE      $LASTV        $LASTG
205300940202     C                   MOVE      'D2'          $LASTV
205400941118     C                   ENDIF
205500940131     C*
205600940707     C* se esistono errori sulla videata
205700940707     C* emetto la write del formato a indicatori spenti per vedere
205800940707     C* le eventuali decodifiche
205900941118     C     *IN99         IFEQ      *ON
206000940707     C                   MOVEA     *IN           WIN
206100940707     C                   MOVE      *ALL'0'       IN5098           49
206200940707     C                   MOVEA     IN5098        *IN(50)
206300940707     C                   WRITE     FMTD2
206400940707     C                   MOVEA     WIN           *IN
206500941118     C                   ENDIF
206600940707     C*
206700940131     C                   EXFMT     FMTD2
206800941205     C*
206900941205     C* controllo abilitazione tasti funzione
207000941205     C                   EXSR      CTRKD2
207100941205     C*
207200941205     C     *IN99         IFEQ      *OFF
207300941205     C*
207400941205     C                   SELECT
207500940131     C* F3=Fine
207600940506     C     $TASTO        WHENEQ    F03
207700940309     C                   EXSR      F03D2
207800940131     C* F12=Ritorno
207900940506     C     $TASTO        WHENEQ    F12
208000940309     C                   EXSR      F12D2
208100940916     C* F24=Altri tasti
208200940916     C     $TASTO        WHENEQ    F24
208300940916     C                   EXSR      F24D2
208400940131     C*
208500940131     C                   OTHER
208600941220     C                   EXSR      CTRD2
208700941205     C     *IN99         IFEQ      *OFF
208800940211     C                   MOVE      $LASTG        $GEST
208900941205     C                   SELECT
209000940202     C     $LASTG        WHENEQ    'S1'
209100940131     C* se è stato modificato l'ordinamento di default, ricarico la lista
209200941205     C     $ORD          IFNE      D2ORD
209300940223     C                   MOVE      *ON           $INZS1
209400941222     C                   Z-ADD     *ZERO         W1POS
209500941205     C                   ENDIF
209600940202     C     $LASTG        WHENEQ    'D1'
209700940318     C* per ora niente
209800941205     C                   ENDSL
209900940131     C* definisco i nuovi valori di default
210000940201     C                   Z-ADD     D2LOP         $LOP
210100940131     C                   Z-ADD     D2ORD         $ORD
210200940609     C* << elenco altri dati default >>
210300941205     C                   ENDIF
210400940131     C*
210500941205     C                   ENDSL
210600940131     C*
210700941205     C                   ENDIF
210800941205     C*
210900940131     C                   ENDSR
211000940223     C/EJECT
211100940131     C************************************************************
211200940211     C* INIZIALIZZAZIONE VIDEATA VALORI ASSUNTI
211300940131     C************************************************************
211400940201     C     INZD2         BEGSR
211500940131     C*
211600940131     C                   CLEAR                   FMTD2
211700940201     C                   MOVEL     $LOP          D2LOP
211800940831     C                   MOVE      $ORD          D2ORD
211900940314     C* << elenco altri dati default >>
212000940131     C*
212100940131     C                   ENDSR
212200941205     C/EJECT
212300941205     C************************************************************
212400941205     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D2
212500941205     C* INPUT
212600941205     C* $FC3 = Abilitazioni videata D2
212700941205     C* OUTPUT
212800941205     C* *IN97 *IN99 Errori
212900941205     C************************************************************
213000941205     C     CTRKD2        BEGSR
213100941205     C*
213200941205     C                   SETOFF                                       99
213300941205     C                   MOVEA     $FC3          $FC
213400941205     C                   EXSR      CTRKEY
213500941205     C     *IN99         IFEQ      *ON
213600941205     C                   SETON                                        97
213700941205     C                   ENDIF
213800941205     C*
213900941205     C                   ENDSR
214000940309     C/EJECT
214100940309     C************************************************************
214200940309     C* GESTIONE F03 VIDEO D2
214300940309     C************************************************************
214400940309     C     F03D2         BEGSR
214500940309     C*
214600940309     C                   MOVE      *ON           $FINE
214700940325     C* fine programma
214800940325     C                   EXSR      ENDPGM
214900940309     C*
215000940309     C                   ENDSR
215100940309     C/EJECT
215200940309     C************************************************************
215300940309     C* GESTIONE F12 VIDEO D2
215400940309     C************************************************************
215500940309     C     F12D2         BEGSR
215600940309     C*
215700940309     C                   MOVE      $LASTG        $GEST
215800940309     C*
215900940309     C                   ENDSR
216000940916     C/EJECT
216100940916     C************************************************************
216200940916     C* GESTIONE F24 VIDEO D2
216300940916     C************************************************************
216400940916     C     F24D2         BEGSR
216500940916     C*
216600940916     C                   MOVEA     $FX3          $FX
216700940916     C                   MOVEA     $FC3          $FC
216800940916     C                   MOVEA     $FR3          $FR
216900940916     C                   Z-ADD     $ULKD2        $ULK
217000940929     C                   Z-ADD     56            $L01
217100940929     C                   Z-ADD     56            $L02
217200940916     C                   EXSR      RIEKEY
217300940916     C                   Z-ADD     $ULK          $ULKD2            3 0
217400940916     C                   MOVEA     $FC           $FC3
217500940916     C                   MOVEL     $KEY1         Z3KE1
217600060224A1148C                   MOVEL     $ALLFUNCT     H3ALLFUNCT
217700940916     C                   MOVEL     $KEY2         Z3KE2
217800940916     C                   WRITE     FMTZ3
217900940916     C*
218000940916     C                   ENDSR
218100940223     C/EJECT
218200940131     C************************************************************
218300940131     C* CONTROLLO VIDEATA VALORI ASSUNTI
218400940131     C************************************************************
218500940131     C     CTRD2         BEGSR
218600940131     C*
218700940131     C                   SETOFF                                       99
218800940131     C* non imposto pos. del cursore
218900940131     C                   Z-ADD     0             H1RIGA
219000940131     C                   Z-ADD     0             H1COLO
219100940131     C*
219200940131     C* OBBLIGATORIO VALORE PER LISTA/PARZIALIZZAZIONI
219300940201     C     D2LOP         IFEQ      0
219400940131     C                   SETON                                          5099
219500941027     C                   ENDIF
219600940131     C*
219700940131     C* OBBLIGATORIO VALORE PER ORDINAMENTO
219800940131     C     D2ORD         IFEQ      0
219900940131     C                   SETON                                          5199
220000941027     C                   ENDIF
220100940314     C* << elenco controlli altri dati default >>
220200940131     C*
220300940131     C                   ENDSR
220400941205     C/EJECT
220500941205     C************************************************************
220600941205     C* CONTROLLO ABILITAZIONE TASTI
220700941205     C* INPUT
220800941205     C* $TASTO =Tasto funzione premuto
220900941205     C* $FC = Tasti abilitati
221000941205     C* OUTPUT
221100941205     C* IN99 = ERrore
221200941205     C************************************************************
221300941205     C     CTRKEY        BEGSR
221400941205     C*
221500941205     C                   SELECT
221600941205     C     $TASTO        WHENEQ    F01
221700941205     C     $FC(1)        IFEQ      '0'
221800941205     C                   SETON                                        99
221900941205     C                   ENDIF
222000941205     C     $TASTO        WHENEQ    F02
222100941205     C     $FC(2)        IFEQ      '0'
222200941205     C                   SETON                                        99
222300941205     C                   ENDIF
222400941205     C     $TASTO        WHENEQ    F03
222500941205     C     $FC(3)        IFEQ      '0'
222600941205     C                   SETON                                        99
222700941205     C                   ENDIF
222800941205     C     $TASTO        WHENEQ    F04
222900941205     C     $FC(4)        IFEQ      '0'
223000941205     C                   SETON                                        99
223100941205     C                   ENDIF
223200941205     C     $TASTO        WHENEQ    F05
223300941205     C     $FC(5)        IFEQ      '0'
223400941205     C                   SETON                                        99
223500941205     C                   ENDIF
223600941205     C     $TASTO        WHENEQ    F06
223700941205     C     $FC(6)        IFEQ      '0'
223800941205     C                   SETON                                        99
223900941205     C                   ENDIF
224000941205     C     $TASTO        WHENEQ    F07
224100941205     C     $FC(7)        IFEQ      '0'
224200941205     C                   SETON                                        99
224300941205     C                   ENDIF
224400941205     C     $TASTO        WHENEQ    F08
224500941205     C     $FC(8)        IFEQ      '0'
224600941205     C                   SETON                                        99
224700941205     C                   ENDIF
224800941205     C     $TASTO        WHENEQ    F09
224900941205     C     $FC(09)       IFEQ      '0'
225000941205     C                   SETON                                        99
225100941205     C                   ENDIF
225200941205     C     $TASTO        WHENEQ    F10
225300941205     C     $FC(10)       IFEQ      '0'
225400941205     C                   SETON                                        99
225500941205     C                   ENDIF
225600941205     C     $TASTO        WHENEQ    F11
225700941205     C     $FC(11)       IFEQ      '0'
225800941205     C                   SETON                                        99
225900941205     C                   ENDIF
226000941205     C     $TASTO        WHENEQ    F12
226100941205     C     $FC(12)       IFEQ      '0'
226200941205     C                   SETON                                        99
226300941205     C                   ENDIF
226400941205     C     $TASTO        WHENEQ    F13
226500941205     C     $FC(13)       IFEQ      '0'
226600941205     C                   SETON                                        99
226700941205     C                   ENDIF
226800941205     C     $TASTO        WHENEQ    F14
226900941205     C     $FC(14)       IFEQ      '0'
227000941205     C                   SETON                                        99
227100941205     C                   ENDIF
227200941205     C     $TASTO        WHENEQ    F15
227300941205     C     $FC(15)       IFEQ      '0'
227400941205     C                   SETON                                        99
227500941205     C                   ENDIF
227600941205     C     $TASTO        WHENEQ    F16
227700941205     C     $FC(16)       IFEQ      '0'
227800941205     C                   SETON                                        99
227900941205     C                   ENDIF
228000941205     C     $TASTO        WHENEQ    F17
228100941205     C     $FC(17)       IFEQ      '0'
228200941205     C                   SETON                                        99
228300941205     C                   ENDIF
228400941205     C     $TASTO        WHENEQ    F18
228500941205     C     $FC(18)       IFEQ      '0'
228600941205     C                   SETON                                        99
228700941205     C                   ENDIF
228800941205     C     $TASTO        WHENEQ    F19
228900941205     C     $FC(19)       IFEQ      '0'
229000941205     C                   SETON                                        99
229100941205     C                   ENDIF
229200941205     C     $TASTO        WHENEQ    F20
229300941205     C     $FC(20)       IFEQ      '0'
229400941205     C                   SETON                                        99
229500941205     C                   ENDIF
229600941205     C     $TASTO        WHENEQ    F21
229700941205     C     $FC(21)       IFEQ      '0'
229800941205     C                   SETON                                        99
229900941205     C                   ENDIF
230000941205     C     $TASTO        WHENEQ    F22
230100941205     C     $FC(22)       IFEQ      '0'
230200941205     C                   SETON                                        99
230300941205     C                   ENDIF
230400941205     C     $TASTO        WHENEQ    F23
230500941205     C     $FC(23)       IFEQ      '0'
230600941205     C                   SETON                                        99
230700941205     C                   ENDIF
230800941205     C     $TASTO        WHENEQ    F24
230900941205     C     $FC(24)       IFEQ      '0'
231000941205     C                   SETON                                        99
231100941205     C                   ENDIF
231200941205     C                   ENDSL
231300941205     C*
231400941205     C                   ENDSR
231500940223     C/EJECT
231600940201     C************************************************************
231700940201     C* AGGIORNAMENTO VALORI ASSUNTI
231800940201     C************************************************************
231900940201     C     AGGMPE        BEGSR
232000940201     C*
232100940201     C                   CLEAR                   ANMPE000
232200950110     C                   MOVEL     XSCSOC        MPESOC
232300941118     C                   MOVEL     DSPGM         MPEPGM
232400940201     C                   MOVEL     KNMUS         MPEPRF
232500941013     C*
232600941209     C*Si memorizzano i dati di default a seconda delle modalità con
232700941209     C*cui il pgm è usato
232800941209     C*MPETIP = 'PX'  pgm richiamato come lista (CMD4)
232900941209     C*MPETIP = 'PV'  pgm richiamato da menù
233000941209     C*MPETIP = 'PB'  x lancio Batch
233100941013     C                   SELECT
233200941221     C     OPZ311        WHENEQ    '01'
233300941013     C                   MOVEL     'PX'          MPETIP
233400941013     C                   OTHER
233500941013     C                   MOVEL     'PV'          MPETIP
233600941013     C                   ENDSL
233700941118     C     K4MPE2        CHAIN     ANMPE02L                           21
233800940201     C                   MOVEL     DSMPE         MPEPAR
233900940201     C     *IN21         IFEQ      *ON
234000940201     C                   WRITE     ANMPE000
234100940201     C                   ELSE
234200940201     C                   UPDATE    ANMPE000
234300941027     C                   ENDIF
234400940201     C*
234500940201     C                   ENDSR
234600940223     C/EJECT
234700940131     C************************************************************
234800940131     C* OPERAZIONI INIZIALI
234900940131     C************************************************************
235000940131     C     *INZSR        BEGSR
235100940131     C*
235200940127     C* Reperimento parametri
235300940127     C*
235400940117     C     *ENTRY        PLIST
235500940117     C                   PARM                    KPJBA
235600950104     C                   PARM                    DSAUT
235700950104     C*
235800950104     C     PARMS         IFLT      2
235900950104     C                   MOVE      *BLANK        AUTDS
236000950104     C                   ELSE
236100950104     C                   MOVEL     DSAUT         AUTDS
236200950104     C                   ENDIF
236300940225     C*
236400940223     C* Reperimento opzioni
236500940127     C*
236600940128     C                   EXSR      REPOPZ
236700940506     C* Reperimento tasti di funzione
236800940506     C*
236900940506     C                   EXSR      REPKEY
237000940506     C*
237100940127     C*
237200940117     C                   ENDSR
237300940223     C/EJECT
237400940223     C************************************************************
237500940223     C* INIZIALIZZAZIONE VARIABILI
237600940223     C************************************************************
237700940223     C     INZVAR        BEGSR
237800940729     C*
237900940729     C* Dati da PGM chiamante
238000940729     C*
238100941221     C                   MOVEL     KPJBU         A311DS
238200941215     C*
238300941215     C* Reperimento dati società
238400941215     C*
238500941215     C                   MOVEL     'SOC001'      TIPXSC
238600941215     C                   MOVEL     *BLANK        SOCXSC
238700941215     C                   EXSR      REPSOC
238800941215     C     RTNXSC        IFNE      '1'
238900941215     C                   MOVEL     XSOCDS        SOC001
239000941215     C                   ELSE
239100941215     C                   RETURN
239200941215     C                   ENDIF
239300950110     C*
239400950110     C* gestione società di riferimento
239500950110     C     FSC311        IFEQ      'S'
239600950111     C     OPZ311        OREQ      *BLANK
239700950110     C                   MOVE      XSCSOC        WWWSOC
239800950110     C* gestione simulata da capofila
239900950110     C                   ELSE
240000950110     C                   MOVE      XSCCPA        WWWSOC
240100950110     C                   ENDIF
240200941201     C*
240300941124     C* Reperimento autorizzazioni
240400941124     C*
240500941124     C* << nel caso l'anagrafica non gestisse le autorizzazioni
240600941124     C*    asteriscare la chiamata alla subroutine REPAUT >>
240700950104     C     AUTDS         IFEQ      *BLANK
240800950104     C                   EXSR      REPAUT
240900950104     C                   ELSE
241000970704     c                   if        CoF311 = 'C'
241100970704     C                   MOVEL     XCADDS        GCLIDS
241200970704     c                   endif
241300970704     c                   if        CoF311 = 'F'
241400970704     C                   MOVEL     XCADDS        GForDs
241500970704     c                   endif
241600950104     C                   ENDIF
241700970704     C*
241800970704     C* Ctrl se l'opzione con cui è stato richiamato è congruente con
241900970704     C* le autorizzazione
242000970704     C*
242100970704     C                   EXSR      TSTAUT
242200941026     C*
242300941026     C* Reperimento valori default utente
242400941026     C*
242500941026     C                   EXSR      REPDFT
242600940506     C*
242700940506     C* Definizione delle opzioni utilizzabili a pgm
242800940506     C*
242900940506     C                   EXSR      DEFOPZ
243000940223     C* Variabili per gestione videate
243100940223     C*
243200940223     C                   MOVE      *OFF          $FINE                          fine pgm
243300940223     C                   MOVE      *OFF          $INZD1                         inizializzione D1
243400940223     C                   MOVE      *OFF          $INZS1                         inizializzione S1
243500940223     C                   MOVE      *OFF          $INZD2                         inizializzione D2
243600940223     C                   MOVE      *OFF          $EFILE                         memo fine file
243700940223     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
243800940223     C                   MOVE      *OFF          $RCDOK                         rcd letto valido
243900940509     C                   MOVE      *HIVAL        $LASTV
244000940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
244100940506     C                   Z-ADD     0             $ULKS1            3 0          piede da emett. S1
244200950224     C                   Z-ADD     0             $ULKD2            3 0          piede da emett. D1
244300940223     C*
244400940318     C                   MOVE      *ON           $INZD1
244500940318     C                   MOVE      *ON           $INZS1
244600940831     C*
244700940831     C* se pgm richiamato con chiave univoca impostata,
244800940831     C* forzo la emissione della lista e imposto l'ordinamento
244900950224     C     UQ1311        IFNE      *BLANK
245000941213     C                   MOVE      *ON           $UNIQ
245100941213     C                   Z-ADD     2             $LOP
245200941013     C                   Z-ADD     99            $ORD
245300941213     C                   ELSE
245400941213     C                   MOVE      *OFF          $UNIQ
245500941213     C                   ENDIF
245600941104     C*
245700941104     C* definisco qual'è la prima videata
245800941104     C* (se è la lista devo comunque inizializzare la videata di
245900941104     C*  parzializzazione perchè il sfl viene caricato in base a quei
246000941104     C*  dati)
246100940223     C                   SELECT
246200940223     C     $LOP          WHENEQ    1
246300940223     C                   MOVE      'D1'          $GEST
246400940223     C     $LOP          WHENEQ    2
246500940223     C                   MOVE      'S1'          $GEST
246600941104     C                   EXSR      INZD1
246700941104     C                   MOVE      *OFF          $INZD1
246800941104     C                   ENDSL
246900940223     C*
247000940223     C* Variabili appoggio
247100940223     C*
247200940223     C                   Z-ADD     SFLPAG        WPAG
247300941124     C                   MOVEL     *OFF          WOPR                                       ne S1
247400940223     C*
247500940223     C* Valorizzazione campi univoci testate
247600940223     C*
247700940223     C                   CLEAR                   FMTT1
247800940223     C                   MOVEL     KNSIF         NOMSIF
247900941109     C                   MOVEL     XSCDSI        NOMDIT
248000941027     C                   MOVEL     DSPGM         NOMPGM
248100950427     C* recupero il sottotitolo (se c'è)
248200950427     C                   MOVEL     *BLANK        T1TIT
248300950427     C     SOTTIT        IFNE      *BLANK
248400950915     C                   MOVEL     SOTTIT        WWSTIT           27
248500950915     C     COF311        IFEQ      'C'
248600950915     C                   MOVE      '0030'        WWSTIT
248700950915     C                   ELSE
248800950915     C     COF311        IFEQ      'F'
248900950915     C                   MOVE      '0140'        WWSTIT
249000950915     C                   ENDIF
249100950915     C                   ENDIF
249200950915     C                   MOVEL     WWSTIT        IDMSG
249300961004     C                   CALLb     'XRTVM'
249400950427     C                   PARM                    IDMSG            27
249500950427     C                   PARM                    TXTMSG          100
249600950427     C                   PARM                    ERRMSG            1
249700950427     C                   PARM      *ON           CTRMSG            1            centratura
249800950427     C                   PARM      30            LENMSG            3 0          lun output.
249900950427     C     ERRMSG        IFNE      *ON
250000950427     C                   MOVEL     TXTMSG        T1TIT
250100950427     C                   ELSE
250200950427     C                   MOVEL     *ALL'?'       T1TIT
250300950427     C                   ENDIF
250400950427     C                   ENDIF
250500940223     C*
250600940223     C                   ENDSR
250700941209     C/EJECT
250800941209     C************************************************************
250900941209     C* REPERIMENTO DATI SOCIETA'
251000941209     C************************************************************
251100941209     C     REPSOC        BEGSR
251200941209     C*
251300961004     C                   CALLb     'XSOC'
251400941209     C                   PARM                    TIPXSC            6
251500941209     C                   PARM                    SOCXSC            3
251600941209     C                   PARM                    CDSXSC            9 0
251700941209     C                   PARM                    MODXSC            3
251800941209     C                   PARM      *BLANKS       RTNXSC            1
251900941209     C                   PARM                    XSOCDS
252000941209     C                   PARM                    KPJBA
252100941209     C*
252200941209     C                   ENDSR
252300941124     C/EJECT
252400941124     C************************************************************
252500941124     C* REPERIMENTO AUTORIZZAZIONI UTENTE
252600941124     C************************************************************
252700941124     C     REPAUT        BEGSR
252800941124     C*
252900941124     C                   CLEAR                   AUTDS
253000941124     C*
253100950123     C                   MOVEL     XSCSOC        XCASOC                         SOCIETA
253200941124     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
253300941124     C                   Z-ADD     0             XCAGRP                         GRUPPO
253400941124     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
253500970704     c                   if        CoF311 = 'C'
253600970704     C                   MOVEL     'ANGCLI'      XCAFNC                         FUNZIONE
253700970704     c                   endif
253800970704     c                   if        CoF311 = 'F'
253900970704     C                   MOVEL     'ANGFOR'      XCAFNC                         FUNZIONE
254000970704     c                   endif
254100941124     C                   MOVEL     *ZERO         XCAVFU                         VARIABILE
254200941124     C                   MOVE      'CK'          XCATCK                         TIPO CTRL
254300941124     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
254400941124     C*
254500961004     C                   CALLb     'XCHKAUT'
254600941124     C                   PARM                    AUTDS
254700941124     C*
254800941124     C* errori/non abilitazione
254900941124     C     XCARTN        IFNE      0
255000941124     C                   EXSR      ENDPGM
255100941124     C                   ELSE
255200970704     c                   if        CoF311 = 'C'
255300970704     C                   MOVEL     XCADDS        GCliDs
255400970704     c                   endif
255500970704     c                   if        CoF311 = 'F'
255600970704     C                   MOVEL     XCADDS        GForDs
255700970704     c                   endif
255800941124     C                   ENDIF
255900941124     C*
256000941124     C                   ENDSR
256100940223     C/EJECT
256200970704     C************************************************************
256300970704     C* TEST OPZIONE DI RICHIAMO IN BASE ALLE AUTORIZZAZIONI UTENTE
256400970704     C************************************************************
256500970704     C     TSTAUT        BEGSR
256600970704     C*
256700970704     c* se cliente
256800970704     c                   if        CoF311 = 'C'
256900970704     C                   SELECT
257000970704     C*
257100010205B0341X**se richiamato in immissione
257200010205  "  X***  OPZ311        WHENEQ    '01'
257300010202  "  X**ma l'utente non è abilitato
257400010202  "  X***  IMMCLI        ANDEQ     *OFF
257500010202  "  X**esco da pgm avvertendolo
257600010202  "  X***                MOVE      '2'           ERR311
257700010202  "  X***                MOVE      *ON           $FINE
257800010202B0341X***                EXSR      ENDPGM
257900970704     C*
258000970704     C* se richiamato in modifica
258100970704     C     OPZ311        WHENEQ    '02'
258200970704     C* ma l'utente non è abilitato
258300970704     C     MANCLI        ANDEQ     *OFF
258400970704     C* testo se l'utente è abilitato almeno alla visualizzazione
258500970704     C     VISCLI        IFEQ      *ON
258600970704     C                   MOVE      '05'          OPZ311
258700970704     C                   ELSE
258800970704     C* altrimenti esco da pgm avvertendolo
258900970704     C                   MOVE      '2'           ERR311
259000970704     C                   MOVE      *ON           $FINE
259100970704     C                   EXSR      ENDPGM
259200970704     C                   ENDIF
259300970704     C*
259400970704     C* se richiamato in copia
259500970704     C     OPZ311        WHENEQ    '03'
259600970704     C* ma l'utente non è abilitato
259700970704     C     IMMCLI        ANDEQ     *OFF
259800970704     C* esco da pgm avvertendolo
259900970704     C                   MOVE      '2'           ERR311
260000970704     C                   MOVE      *ON           $FINE
260100970704     C                   EXSR      ENDPGM
260200970704     C*
260300970704     C* se richiamato in annullamento
260400970704     C     OPZ311        WHENEQ    '04'
260500970704     C* ma l'utente non è abilitato
260600970704     C     ANNCLI        ANDEQ     *OFF
260700970704     C* esco da pgm avvertendolo
260800970704     C                   MOVE      '2'           ERR311
260900970704     C                   MOVE      *ON           $FINE
261000970704     C                   EXSR      ENDPGM
261100970704     C*
261200970704     C* se richiamato in visualizzazione
261300970704     C     OPZ312        WHENEQ    '05'
261400970704     C* ma l'utente non è abilitato
261500970704     C     VISCLI        ANDEQ     *OFF
261600970704     C* esco da pgm avvertendolo
261700970704     C                   MOVE      '2'           ERR312
261800970704     C                   MOVE      *ON           $FINE
261900970704     C                   EXSR      ENDPGM
262000970704     C*
262100970704     C                   ENDSL
262200970704     c                   endif
262300970704     C*
262400970704     C*
262500970704     c* se fornitore
262600970704     c                   if        CoF311 = 'F'
262700970704     C                   SELECT
262800970704     C*
262900970704     C* se richiamato in immissione
263000970704     C     OPZ311        WHENEQ    '01'
263100970704     C* ma l'utente non è abilitato
263200970704     C     ImmFor        ANDEQ     *OFF
263300970704     C* esco da pgm avvertendolo
263400970704     C                   MOVE      '2'           ERR311
263500970704     C                   MOVE      *ON           $FINE
263600970704     C                   EXSR      ENDPGM
263700970704     C*
263800970704     C* se richiamato in modifica
263900970704     C     OPZ311        WHENEQ    '02'
264000970704     C* ma l'utente non è abilitato
264100970704     C     ManFor        ANDEQ     *OFF
264200970704     C* testo se l'utente è abilitato almeno alla visualizzazione
264300970704     C     VisFor        IFEQ      *ON
264400970704     C                   MOVE      '05'          OPZ311
264500970704     C                   ELSE
264600970704     C* altrimenti esco da pgm avvertendolo
264700970704     C                   MOVE      '2'           ERR311
264800970704     C                   MOVE      *ON           $FINE
264900970704     C                   EXSR      ENDPGM
265000970704     C                   ENDIF
265100970704     C*
265200970704     C* se richiamato in copia
265300970704     C     OPZ311        WHENEQ    '03'
265400970704     C* ma l'utente non è abilitato
265500970704     C     ImmFor        ANDEQ     *OFF
265600970704     C* esco da pgm avvertendolo
265700970704     C                   MOVE      '2'           ERR311
265800970704     C                   MOVE      *ON           $FINE
265900970704     C                   EXSR      ENDPGM
266000970704     C*
266100970704     C* se richiamato in annullamento
266200970704     C     OPZ311        WHENEQ    '04'
266300970704     C* ma l'utente non è abilitato
266400970704     C     AnnFor        ANDEQ     *OFF
266500970704     C* esco da pgm avvertendolo
266600970704     C                   MOVE      '2'           ERR311
266700970704     C                   MOVE      *ON           $FINE
266800970704     C                   EXSR      ENDPGM
266900970704     C*
267000970704     C* se richiamato in visualizzazione
267100970704     C     OPZ311        WHENEQ    '05'
267200970704     C* ma l'utente non è abilitato
267300970704     C     VisFor        ANDEQ     *OFF
267400970704     C* esco da pgm avvertendolo
267500970704     C                   MOVE      '2'           ERR311
267600970704     C                   MOVE      *ON           $FINE
267700970704     C                   EXSR      ENDPGM
267800970704     C*
267900970704     C                   ENDSL
268000970704     c                   endif
268100970704     C*
268200970704     C                   ENDSR
268300970704     C/EJECT
268400940127     C************************************************************
268500940131     C* REPERIMENTO/DEFINIZIONE VALORI DEFAULT UTENTE
268600940127     C************************************************************
268700940131     C     REPDFT        BEGSR
268800940127     C*
268900950110     C                   MOVEL     WWWSOC        MPESOC
269000940201     C                   MOVEL     DSPGM         MPEPGM
269100940131     C                   MOVEL     KNMUS         MPEPRF
269200941027     C*Si reperiscono i dati di default a seconda delle modalità con
269300941027     C*cui il pgm è usato
269400941027     C*MPETIP = 'PX'  pgm richiamato come lista (CMD4)
269500941027     C*MPETIP = 'PV'  pgm richiamato da menù
269600941027     C*MPETIP = 'PB'  x lancio Batch
269700941013     C                   SELECT
269800941221     C     OPZ311        WHENEQ    '01'
269900941013     C                   MOVEL     'PX'          MPETIP
270000941013     C                   OTHER
270100941013     C                   MOVEL     'PV'          MPETIP
270200941013     C                   ENDSL
270300941118     C     K4MPE2        CHAIN(N)  ANMPE02L                           21
270400940131     C     *IN21         IFEQ      *OFF
270500940131     C                   MOVEL     MPEPAR        DSMPE
270600940131     C* nel caso sia la prima volta che l'utente apre questa anagrafica
270700940131     C* i suoi valori di default vengono decisi a pgm
270800940131     C                   ELSE
270900941221     C                   Z-ADD     2             $LOP
271000940131     C                   Z-ADD     1             $ORD
271100941011     C* << elenco altri dati default >>
271200941027     C                   ENDIF
271300940131     C*
271400940131     C                   ENDSR
271500940223     C/EJECT
271600940131     C************************************************************
271700940225     C* DEFINIZIONE OPZIONI UTILIZZABILI A PGM
271800940131     C************************************************************
271900940225     C     DEFOPZ        BEGSR
272000940131     C*
272100940225     C* Questa routine deve valorizzare la schiera $FL1
272200940225     C* con S o N a seconda della utilizzabilità o meno
272300940225     C* dell'opzione correlata
272400940225     C*
272500940225     C* Inizialmente si considerano tutte le opzioni come valide
272600940317     C     1             DO        $DIMOP        X
272700940225     C                   MOVE      'S'           $FL1(X)
272800941027     C                   ENDDO
272900940715     C*
273000940715     C* Questa routine deve valorizzare la schiera $FC1/$FR1/$FC2/$FR2
273100941018     C* con 1 o 0 a seconda della utilizzabilità o meno
273200940715     C* del tasto funzione correlato
273300940715     C*
273400940715     C* Inizialmente ripristino le abilitazioni come da schiera a tempo
273500940715     C* di compilazione
273600940715     C                   RESET                   $FC1
273700940715     C                   RESET                   $FR1
273800940715     C                   RESET                   $FC2
273900940715     C                   RESET                   $FR2
274000940919     C                   RESET                   $FC3
274100940919     C                   RESET                   $FR3
274200940225     C*
274300940225     C* Dopodichè, a seconda del richiamo e delle autorizzazioni
274400940225     C* invalido alcune opzioni
274500940225     C                   SELECT
274600940225     C* Se pgm richiamato in ricerca disattivo :
274700941221     C     OPZ311        WHENEQ    '01'
274800940307     C* F10=Immissione
274900940506     C                   Z-ADD     10            X
275000941018     C                   MOVE      '1'           $FC2(X)
275100940603     C* F3=Fine
275200940603     C                   Z-ADD     3             X
275300940720     C                   MOVE      '0'           $FC2(X)
275400940307     C* 2=Modifica
275500940307     C                   Z-ADD     1             X
275600940225     C     02            LOOKUP    $OP1(X)                                21
275700940225     C     *IN21         IFEQ      *ON
275800941216     C                   MOVE      'S'           $FL1(X)
275900940225     C                   ENDIF
276000940225     C* 3=Copia
276100940225     C                   Z-ADD     1             X
276200940225     C     03            LOOKUP    $OP1(X)                                21
276300940225     C     *IN21         IFEQ      *ON
276400941018     C                   MOVE      'S'           $FL1(X)
276500940225     C                   ENDIF
276600940225     C* 4=Annullamento
276700940225     C                   Z-ADD     1             X
276800940225     C     04            LOOKUP    $OP1(X)                                21
276900940225     C     *IN21         IFEQ      *ON
277000941018     C                   MOVE      'S'           $FL1(X)
277100940225     C                   ENDIF
277200940314     C* << aggiungere nollo stesso modo le altre opzioni da
277300940314     C*    disattivare (quelle specifiche per ogni pgm) >>
277400940225     C* Se pgm richiamato in interrogazione disattivo :
277500941221     C     OPZ311        WHENEQ    '05'
277600940307     C* F10=Immissione
277700940615     C                   Z-ADD     10            X
277800940720     C                   MOVE      '0'           $FC2(X)
277900940225     C* 1=Selezione
278000940225     C                   Z-ADD     1             X
278100940225     C     01            LOOKUP    $OP1(X)                                21
278200940225     C     *IN21         IFEQ      *ON
278300940225     C                   MOVE      'N'           $FL1(X)
278400940225     C                   ENDIF
278500940225     C* 2=Modifica
278600940225     C                   Z-ADD     1             X
278700940225     C     02            LOOKUP    $OP1(X)                                21
278800940225     C     *IN21         IFEQ      *ON
278900940225     C                   MOVE      'N'           $FL1(X)
279000940225     C                   ENDIF
279100940225     C* 3=Copia
279200940225     C                   Z-ADD     1             X
279300940225     C     03            LOOKUP    $OP1(X)                                21
279400940225     C     *IN21         IFEQ      *ON
279500940225     C                   MOVE      'N'           $FL1(X)
279600940225     C                   ENDIF
279700940225     C* 4=Annullamento
279800940225     C                   Z-ADD     1             X
279900940225     C     04            LOOKUP    $OP1(X)                                21
280000940225     C     *IN21         IFEQ      *ON
280100940225     C                   MOVE      'N'           $FL1(X)
280200940225     C                   ENDIF
280300941227     C*
280400941227     C* Se pgm non richiamato, disattivo subito :
280500941227     C     OPZ311        WHENEQ    *BLANK
280600941227     C* F8=Selezione seguente
280700941227     C                   Z-ADD     8             X
280800941227     C                   MOVE      '0'           $FC1(X)
280900941227     C                   MOVE      '0'           $FC2(X)
281000941227     C* 1=Selezione
281100941227     C                   Z-ADD     1             X
281200941227     C     01            LOOKUP    $OP1(X)                                21
281300941227     C     *IN21         IFEQ      *ON
281400941227     C                   MOVE      'N'           $FL1(X)
281500941227     C                   ENDIF
281600941227     C*
281700941227     C* In tutti gli altri casi, disattivo subito :
281800940225     C                   OTHER
281900940225     C* 1=Selezione
282000940225     C                   Z-ADD     1             X
282100940225     C     01            LOOKUP    $OP1(X)                                21
282200940225     C     *IN21         IFEQ      *ON
282300940225     C                   MOVE      'N'           $FL1(X)
282400940225     C                   ENDIF
282500940225     C*
282600940225     C                   ENDSL
282700941216     C*
282800150113R160 C* Disattivo l'opzione per i clienti
282900150113 "   C* 6=Stampa su modulo
283000150113 "   C                   IF        Cof311 = 'C'
283100150113 "   C                   Z-ADD     1             X
283200150113 "   C     06            LOOKUP    $OP1(X)                                21
283300150113 "   C     *IN21         IFEQ      *ON
283400150113 "   C                   MOVE      'N'           $FL1(X)
283500150113 "   C                   ENDIF
283600150113R160 C                   ENDIF
283700941118     C*
283800941118     C* poi, disattivo a seconda delle autorizzazioni :
283900950104     C     MANCLI        IFEQ      *OFF
284000950104     C                   Z-ADD     1             X
284100950104     C     02            LOOKUP    $OP1(X)                                21
284200950104     C     *IN21         IFEQ      *ON
284300950104     C                   MOVE      'N'           $FL1(X)
284400950104     C                   ENDIF
284500950104     C                   ENDIF
284600950104      *
284700950104     C     VISCLI        IFEQ      *OFF
284800950104     C                   Z-ADD     1             X
284900950104     C     05            LOOKUP    $OP1(X)                                21
285000950104     C     *IN21         IFEQ      *ON
285100950104     C                   MOVE      'N'           $FL1(X)
285200950104     C                   ENDIF
285300950104     C                   ENDIF
285400950104      *
285500950104     C     IMMCLI        IFEQ      *OFF
285600950104     C                   Z-ADD     1             X
285700950104     C     03            LOOKUP    $OP1(X)                                21
285800950104     C     *IN21         IFEQ      *ON
285900950104     C                   MOVE      'N'           $FL1(X)
286000950104     C                   ENDIF
286100950104     C                   ENDIF
286200940225     C*
286300950104     C     ANNCLI        IFEQ      *OFF
286400950104     C                   Z-ADD     1             X
286500950104     C     04            LOOKUP    $OP1(X)                                21
286600950104     C     *IN21         IFEQ      *ON
286700950104     C                   MOVE      'N'           $FL1(X)
286800950104     C                   ENDIF
286900950104     C                   ENDIF
287000950104     C*
287100950104     C     IMMCLI        IFEQ      *OFF
287200950104     C                   Z-ADD     10            X
287300950104     C                   MOVE      '1'           $FC2(X)
287400950104     C                   ENDIF
287500950104      *
287600940225     C                   ENDSR
287700940225     C/EJECT
287800940225     C************************************************************
287900940225     C* REPERIMENTO OPZIONI
288000940225     C************************************************************
288100940225     C     REPOPZ        BEGSR
288200940225     C*
288300940314     C     1             DO        $DIMOP        X
288400940128     C*
288500940223     C     $MS1(X)       IFNE      *BLANK
288600940128     C*
288700940223     C                   MOVEL     $MS1(X)       IDMSG
288800961004     C                   CALLb     'XRTVM'
288900940201     C                   PARM                    IDMSG            27
289000940201     C                   PARM                    TXTMSG          100
289100940201     C                   PARM                    ERRMSG            1
289200940128     C*
289300940128     C     ERRMSG        IFNE      *ON
289400940223     C                   MOVEL     TXTMSG        $TX1(X)
289500940128     C                   ELSE
289600940223     C                   MOVEL     *ALL'?'       $TX1(X)
289700941027     C                   ENDIF
289800940128     C*
289900941027     C                   ENDIF
290000940128     C*
290100941027     C                   ENDDO
290200940128     C*
290300940128     C                   ENDSR
290400940506     C/EJECT
290500940506     C************************************************************
290600940506     C* REPERIMENTO TASTI DI FUNZIONE
290700940506     C************************************************************
290800940506     C     REPKEY        BEGSR
290900940506     C                   MOVEA     $FM1          $FM
291000950906     C                   MOVEA     $FC1          $FC
291100940506     C                   EXSR      REPKKK
291200940506     C                   MOVEA     $FX           $FX1
291300940506     C*
291400940506     C                   MOVEA     $FM2          $FM
291500950906     C                   MOVEA     $FC2          $FC
291600940506     C                   EXSR      REPKKK
291700940506     C                   MOVEA     $FX           $FX2
291800940919     C*
291900940919     C                   MOVEA     $FM3          $FM
292000950906     C                   MOVEA     $FC3          $FC
292100940919     C                   EXSR      REPKKK
292200940919     C                   MOVEA     $FX           $FX3
292300940919     C                   ENDSR
292400940506     C/EJECT
292500940506     C************************************************************
292600940506     C* REPERIMENTO TASTI DI FUNZIONE
292700940506     C************************************************************
292800940506     C     REPKKK        BEGSR
292900940506     C*
293000940506     C                   CLEAR                   $FX
293100940506     C     1             DO        24            X
293200940506     C*
293300940506     C     $FM(X)        IFNE      *BLANK
293400950906     C     $FC(X)        ANDNE     '0'
293500940506     C*
293600940506     C                   MOVEL     $FM(X)        IDMSG
293700961004     C                   CALLb     'XRTVM'
293800940506     C                   PARM                    IDMSG            27
293900940506     C                   PARM                    TXTMSG          100
294000940506     C                   PARM                    ERRMSG            1
294100940506     C*
294200940506     C     ERRMSG        IFNE      *ON
294300940506     C                   MOVEL     TXTMSG        $FX(X)
294400940506     C                   ELSE
294500940506     C                   MOVEL     *ALL' '       $FX(X)
294600941027     C                   ENDIF
294700940506     C*
294800941027     C                   ENDIF
294900940506     C*
295000941027     C                   ENDDO
295100940506     C*
295200940506     C                   ENDSR
295300940223     C/EJECT
295400940128     C************************************************************
295500940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
295600940128     C************************************************************
295700940223     C     DEFCAM        BEGSR
295800940128     C*
295900940223     C* klist
296000940223     C*
296100971029     C* ANDIC03L
296200971029     C     K5DIC3        KLIST
296300941221     C                   KFLD                    DICSOC
296400951002     C                   KFLD                    DICCF
296500941221     C                   KFLD                    DICKSC
296600971029     C                   KFLD                    DICDTC
296700941221     C                   KFLD                    DICNRC
296800020718A0957C* ANDIC04L
296900020718  "  C     K5DIC4        KLIST
297000020718  "  C                   KFLD                    DICSOC
297100020718  "  C                   KFLD                    DICCF
297200020718  "  C                   KFLD                    DICKSC
297300020718  "  C                   KFLD                    DICDIN
297400020718A0957C                   KFLD                    DICNIN
297500940128     C* ANMPE02L
297600941118     C     K4MPE2        KLIST
297700941118     C                   KFLD                    MPESOC
297800941118     C                   KFLD                    MPEPGM
297900940128     C                   KFLD                    MPETIP
298000940128     C                   KFLD                    MPEPRF
298100150113R160 C* SdgDif01l
298200150113 "   C     K04Dif01      KLIST
298300150113 "   C                   KFLD                    DifSocieta
298400150113 "   C                   KFLD                    DifCf
298500150113 "   C                   KFLD                    DifDtInter
298600150113R160 C                   KFLD                    DifNrInter
298700940223     C*
298800940223     C* Variabili per gestione videate
298900940223     C*
299000940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
299100940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
299200940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
299300940223     C                   MOVE      *BLANK        $INZS1            1            inizializzione S1
299400940223     C                   MOVE      *BLANK        $INZD2            1            inizializzione D2
299500940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
299600940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
299700940223     C                   MOVE      *BLANK        $EFILE            1            memo fine file
299800940223     C                   MOVE      *BLANK        $ESCI             1            rich.uscita ciclo
299900940223     C                   MOVE      *BLANK        $RCDOK            1            rcd letto valido
300000940224     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
300100940224     C                   Z-ADD     0             $PIES1            3 0          piede da emett. S1
300200941213     C                   MOVE      *BLANK        $UNIQ             1            gest.POSTA
300300940223     C*
300400940314     C* Variabili appoggio sempre presenti in tutte le anagrafiche
300500940223     C*
300600940223     C     *LIKE         DEFINE    C1RCD         S1NRR
300700940223     C     *LIKE         DEFINE    C1NRR         WSFL
300800940223     C     *LIKE         DEFINE    C1RCD         WMAX
300900940223     C     *LIKE         DEFINE    C1RCD         WPAG
301000940223     C     *LIKE         DEFINE    $INZS1        WINZS1                                     ne S1
301100941221     C     *LIKE         DEFINE    OPR311        WOPR                                       ne S1
301200950110     C     *LIKE         DEFINE    XSCSOC        WWWSOC
301300940223     C                   MOVE      *BLANK        WWGRP             3
301400940223     C                   Z-ADD     0             CURR              2 0
301500940223     C                   Z-ADD     0             CURC              2 0
301600940223     C                   MOVE      *BLANK        WIN              99
301700940314     C     *LIKE         DEFINE    C1POS         W1POS
301800941215     C* aggiungere definizione altri campi posizionamento se ci sono
301900940314     C*
302000940314     C* Variabili appoggio specifiche della singola anagrafica
302100940314     C*
302200940314     C* campi per gestione F4=Ricerca
302300941215     C*          *LIKE     DEFN ANAxxx    WWxxx
302400940223     C* campi per parzializzazioni su rcd (solo alfanumerici)
302500941215     C*          *LIKE     DEFN ANAxxx    xxx999
302600940223     C*
302700940223     C* Indici
302800940223     C*
302900940223     C                   Z-ADD     0             X                 3 0
303000940223     C                   Z-ADD     0             Y                 3 0
303100940127     C*
303200940117     C                   ENDSR
303300940223     C/EJECT
303400950906**
303500940718PROMSG    *LIBL     OPZ0001
303600940225PROMSG    *LIBL     OPZ0002
303700941227PROMSG    *LIBL     OPZ0003
303800940131PROMSG    *LIBL     OPZ0004
303900940131PROMSG    *LIBL     OPZ0005
304000150113PROMSG    *LIBL     OPZ0449            ### R160 ###
304100940201**
30420094022501S      <Selezione
30430094121502N      <Modifica
30440094022503S      <Copia
30450094022504S      <Annullamento
30460094022505S      <Visualizzazione
30470015011306S      <Stampa su modulo              ### R160 ###
304800940506** TASTI DI FUNZIONE  D1
304900940506PROMSG    *LIBL     KEY0001  01
305000940506PROMSG    *LIBL     KEY0002  02
305100940506PROMSG    *LIBL     KEY0003  03
305200940506PROMSG    *LIBL     KEY0004  04
305300940506PROMSG    *LIBL     KEY0005  05
305400940506PROMSG    *LIBL     KEY0006  06
305500940506PROMSG    *LIBL     KEY0007  07
305600940506PROMSG    *LIBL     KEY0008  08
305700940506PROMSG    *LIBL     KEY0009  09
305800940506PROMSG    *LIBL     KEY0010  10
305900940506PROMSG    *LIBL     KEY0011  11
306000940506PROMSG    *LIBL     KEY0012  12
306100940506PROMSG    *LIBL     KEY0013  13
306200940506PROMSG    *LIBL     KEY0014  14
306300940506PROMSG    *LIBL     KEY0015  15
306400940506PROMSG    *LIBL     KEY0016  16
306500940506PROMSG    *LIBL     KEY0017  17
306600940506PROMSG    *LIBL     KEY0018  18
306700940506PROMSG    *LIBL     KEY0019  19
306800940506PROMSG    *LIBL     KEY0020  20
306900940506PROMSG    *LIBL     KEY0021  21
307000940506PROMSG    *LIBL     KEY0022  22
307100940506PROMSG    *LIBL     KEY0023  23
307200940506PROMSG    *LIBL     KEY0024  24
307300940506**  ABILITAZIONE D1
3074009407200N  01
3075009407200N  02
3076009407201N  03
3077009407201N  04
3078009407200N  05
3079009407200N  06
3080009407200N  07
3081009412061N  08
3082009407200N  09
3083009407200N  10
3084009407200N  11
3085009407201N  12
3086009407200N  13
3087009407200N  14
3088009407200N  15
3089009407200N  16
3090009407200N  17
3091009412210N  18
3092009407200N  19
3093009407200N  20
3094009407200N  21
3095009407200N  22
3096009509060N  23   Impostare a '1' se si utilizzano opzioni
3097009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
309800940916** TASTI DI FUNZIONE  S1
309900940916PROMSG    *LIBL     KEY0001  01
310000940916PROMSG    *LIBL     KEY0002  02
310100940916PROMSG    *LIBL     KEY0003  03
310200940916PROMSG    *LIBL     KEY0004  04
310300940916PROMSG    *LIBL     KEY0005  05
310400940916PROMSG    *LIBL     KEY0006  06
310500940916PROMSG    *LIBL     KEY0007  07
310600940916PROMSG    *LIBL     KEY0008  08
310700940916PROMSG    *LIBL     KEY0009  09
310800940916PROMSG    *LIBL     KEY0010  10
310900940916PROMSG    *LIBL     KEY0011  11
311000940916PROMSG    *LIBL     KEY0012  12
311100940916PROMSG    *LIBL     KEY0013  13
311200940916PROMSG    *LIBL     KEY0014  14
311300940916PROMSG    *LIBL     KEY0015  15
311400940916PROMSG    *LIBL     KEY0016  16
311500940916PROMSG    *LIBL     KEY0017  17
311600940916PROMSG    *LIBL     KEY0018  18
311700940916PROMSG    *LIBL     KEY0019  19
311800940916PROMSG    *LIBL     KEY0020  20
311900940916PROMSG    *LIBL     KEY0021  21
312000940916PROMSG    *LIBL     KEY0022  22
312100940916PROMSG    *LIBL     KEY0023  23
312200940916PROMSG    *LIBL     KEY0024  24
312300940916**  ABILITAZIONE S1
3124009409160N  01
3125009409160N  02
3126009409161N  03
3127009409160N  04
3128009409160N  05
3129009409160N  06
3130009409160N  07
3131009411241N  08
3132009409160N  09
3133009409161N  10
3134009409160N  11
3135009409161N  12
3136009409160N  13
3137009409160N  14
3138009409160N  15
3139009409160N  16
3140009412220N  17
3141009412210N  18
3142009409160N  19
3143009409160N  20
3144009409160N  21
3145009409160N  22
3146009509061N  23   Impostare a '1' se si utilizzano opzioni
3147009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
314800940916** TASTI DI FUNZIONE  D3
314900940506PROMSG    *LIBL     KEY0001  01
315000940506PROMSG    *LIBL     KEY0002  02
315100940506PROMSG    *LIBL     KEY0003  03
315200940506PROMSG    *LIBL     KEY0004  04
315300940506PROMSG    *LIBL     KEY0005  05
315400940506PROMSG    *LIBL     KEY0006  06
315500940506PROMSG    *LIBL     KEY0007  07
315600940506PROMSG    *LIBL     KEY0008  08
315700940506PROMSG    *LIBL     KEY0009  09
315800940506PROMSG    *LIBL     KEY0010  10
315900940506PROMSG    *LIBL     KEY0011  11
316000940506PROMSG    *LIBL     KEY0012  12
316100940506PROMSG    *LIBL     KEY0013  13
316200940506PROMSG    *LIBL     KEY0014  14
316300940506PROMSG    *LIBL     KEY0015  15
316400940506PROMSG    *LIBL     KEY0016  16
316500940506PROMSG    *LIBL     KEY0017  17
316600940506PROMSG    *LIBL     KEY0018  18
316700940506PROMSG    *LIBL     KEY0019  19
316800940506PROMSG    *LIBL     KEY0020  20
316900940506PROMSG    *LIBL     KEY0021  21
317000940506PROMSG    *LIBL     KEY0022  22
317100940506PROMSG    *LIBL     KEY0023  23
317200940506PROMSG    *LIBL     KEY0024  24
317300940916**  ABILITAZIONE D3
3174009407200N  01
3175009407200N  02
3176009407201N  03
3177009407200N  04
3178009407200N  05
3179009407200N  06
3180009407200N  07
3181009407200N  08
3182009412161N  09
3183009409160N  10
3184009407200N  11
3185009407201N  12
3186009407200N  13
3187009407200N  14
3188009407200N  15
3189009407200N  16
3190009409160N  17
3191009409160N  18
3192009407200N  19
3193009407200N  20
3194009407200N  21
3195009407200N  22
3196009509060N  23   Impostare a '1' se si utilizzano opzioni
3197009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
