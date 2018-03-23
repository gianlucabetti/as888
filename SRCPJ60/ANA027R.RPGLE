000100981202     H*PARMS DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200941115     H*PARMS OPTION(*NOXREF) CVTOPT(*DATETIME)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940117      *
000500950217      *            GESTIONE COORDINATE BANCARIE
000600941201      *           -----------------------------
000700080204D2341 *
000800080204  "   *****************************************************************
000900080204  "   *****************************************************************
001000080204  "   *
001100080204  "   * ATTENZIONE!        ATTENZIONE!
001200080204  "   *
001300080204  "   * questo programma è strettamente correlato al pgm ANAY15R Batch
001400080204  "   * input delle coordinate bancarie.
001500080204  "   * Tutte le volte che si modifica il pgm ANA027R bisogna valutare
001600080204  "   * se gli stessi controlli devono essere effettuati anche per
001700080204  "   * il pgm ANAY15R, e viceversa.
001800080204  "   *
001900080204  "   *****************************************************************
002000080204  "   *****************************************************************
002100080204D2341 *
002200950109      *----------------------------------------------------*
002300980223     FANCBA01L  UF A E           K DISK
002400980223     FANCBD01L  UF A E           K DISK
002500950112     FANRCO01L  IF   E           K DISK
002600950905     FANPDC01L  IF   E           K DISK
002700071029A1250FAnAcr01L  IF   E           K DISK
002800071127c2063FAnBan01L  IF   E           K DISK
002900950109      *------------
003000950112     FANA027D   CF   E             WORKSTN
003100950112     F                                     RENAME(A027D1:FMTD1)
003200950112     F                                     RENAME(A027T1:FMTT1)
003300950112     F                                     RENAME(A027Z1:FMTZ1)
003400030912B0611F                                     RENAME(A027D2:FMTD2)
003500030912B0611F                                     RENAME(A027D3:FMTD3)
003600950109      * eventuali schermi supplementari
003700940201     F                                     INFDS(DSFMT)
003800950109      *----------------------------------------------------*
003900950426     D $OZ             S              2    DIM(5) CTDATA PERRCD(1)
004000950426     D $ID             S             27    DIM(5) ALT($OZ)
004100950109      * Schiere per la gestione dei tasti di funzione
004200940506     D $FM             S             27    DIM(24)
004300940506     D $FX             S             50    DIM(24)
004400940506     D $FC             S              1    DIM(24)
004500940506     D $FR             S              1    DIM(24)
004600950109      * Video D1
004700940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
004800940506     D $FX1            S             50    DIM(24) ALT($FM1)
004900940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005000940506     D $FR1            S              1    DIM(24) ALT($FC1)
005100001010A0840X*DVOFRN        E DS                  EXTNAME(DVOFRNDS) INZ
005200001010A0840D DVOFRN        E DS          1000    EXTNAME(DVOFRNDS) INZ
005300950112     D* Anagrafica fornitori
005400950112     D  FRNSOC       E                     EXTFLD(FRNSOCIETA)
005500950112     D  FRNUNT       E                     EXTFLD(FRNUNITA)
005600950112     D  FRNKCC       E                     EXTFLD(FRNKCC)
005700950112     D  FRNKSC       E                     EXTFLD(FRNKSC)
005800950112     D  FRNFIL       E                     EXTFLD(FRNFILIALE)
005900950112     D  FRNSOG       E                     EXTFLD(FRNSOGG)
006000950112     D  FRNTII       E                     EXTFLD(FRNTPIND)
006100950112     D  FRNCOI       E                     EXTFLD(FRNCDIND)
006200950112     D  FRNDUM       E                     EXTFLD(FRNDTULMO)
006300950112     D  FRNUTE       E                     EXTFLD(FRNUTENTE)
006400950112     D  FRNDIR       E                     EXTFLD(FRNDTINRPF)
006500950119     D  FRNDFR       E                     EXTFLD(FRNDTFIRPF)
006600950112     D  FRNDES       E                     EXTFLD(FRNDESBREV)
006700950112     D  FRNGPM       E                     EXTFLD(FRNGRUMERC)
006800950112     D  FRNSGM       E                     EXTFLD(FRNSETMERC)
006900950112     D  FRNCFA       E                     EXTFLD(FRNFORALT)
007000950112     D  FRNLIS       E                     EXTFLD(FRNLISTINO)
007100950112     D  FRNVAL       E                     EXTFLD(FRNDIVISA)
007200950112     D  FRNCDD       E                     EXTFLD(FRNCONDCON)
007300950112     D  FRNQFO       E                     EXTFLD(FRNQUALFOR)
007400950112     D  FRNVAF       E                     EXTFLD(FRNVOLAFF)
007500950112     D  FRNCIV       E                     EXTFLD(FRNCDIVA)
007600950112     D  FRNCOB       E                     EXTFLD(FRNCOLLOBB)
007700950112     D  FRNTCL       E                     EXTFLD(FRNTPCOLL)
007800950112     D  FRNFRL       E                     EXTFLD(FRNFORLIST)
007900950112     D  FRNFOF       E                     EXTFLD(FRNFORFATT)
008000950112     D  FRNPOT       E                     EXTFLD(FRNFORPOT)
008100950112     D  FRNTRP       E                     EXTFLD(FRNREPPRZL)
008200950112     D  FRNFFB       E                     EXTFLD(FRNFORBLO)
008300950112     D  FRNLSF       E                     EXTFLD(FRNLISFIL)
008400950112     D  FRNCPA       E                     EXTFLD(FRNABILPRI)
008500950112     D  FRNMDC       E                     EXTFLD(FRNMDCONS)
008600950112     D  FRNMAG       E                     EXTFLD(FRNMAG)
008700950112     D  FRNCLI       E                     EXTFLD(FRNCLICORR)
008800950112     D  FRNFRP       E                     EXTFLD(FRNFORPREC)
008900950112     D  FRNSC1       E                     EXTFLD(FRNSCOPAG)
009000950112     D  FRNSC2       E                     EXTFLD(FRNSCOPAG2)
009100950112     D  FRNIP1       E                     EXTFLD(FRNGGINPA1)
009200950112     D  FRNIP2       E                     EXTFLD(FRNGGINPA2)
009300950112     D  FRNIP3       E                     EXTFLD(FRNGGINPA3)
009400950112     D  FRNTRA       E                     EXTFLD(FRNSESSTRA)
009500950112     D  FRNRIC       E                     EXTFLD(FRNSESSRIC)
009600950112     D* Anagrafica soggetti
009700950112     D  FOGCOD       E                     EXTFLD(SOGSOGG)
009800950112     D  FOGDAT       E                     EXTFLD(SOGDTENTR)
009900950112     D  FOGDAU       E                     EXTFLD(SOGDTULMO)
010000950112     D  FOGDAI       E                     EXTFLD(SOGDTINVL)
010100950112     D  FOGDAF       E                     EXTFLD(SOGDTFIVL)
010200950112     D  FOGDAN       E                     EXTFLD(SOGDTNASC)
010300950112     D  FOGDES       E                     EXTFLD(SOGDES)
010400950112     D  FOGDE1       E                     EXTFLD(SOGDESAGG)
010500950112     D  FOGCDF       E                     EXTFLD(SOGCDFISC)
010600950112     D  FOGIVA       E                     EXTFLD(SOGPARTIVA)
010700950112     D  FOGFPI       E                     EXTFLD(SOGFRCPIVA)
010800950112     D  FOGCIA       E                     EXTFLD(SOGISCCIAA)
010900950112     D  FOGSES       E                     EXTFLD(SOGSEX)
011000950112     D  FOGNAT       E                     EXTFLD(SOGNATGIUR)
011100950112     D  FOGCOP       E                     EXTFLD(SOGCODPREC)
011200950112     D  FOGABI       E                     EXTFLD(SOGABI)
011300950112     D  FOGCLG       E                     EXTFLD(SOGCLFSOG)
011400950112     D* Anagrafica indirizzi
011500950112     D  FNDCOD       E                     EXTFLD(INDSOGG)
011600950112     D  FNDTII       E                     EXTFLD(INDTPIND)
011700950112     D  FNDCOI       E                     EXTFLD(INDCDIND)
011800950112     D  FNDDAU       E                     EXTFLD(INDDTULMO)
011900950112     D  FNDDAI       E                     EXTFLD(INDDTINVL)
012000950112     D  FNDDAF       E                     EXTFLD(INDDTFIVL)
012100950112     D  FNDDES       E                     EXTFLD(INDDESFIL)
012200950112     D  FNDDE1       E                     EXTFLD(INDDESAGG)
012300950112     D  FNDVIA       E                     EXTFLD(INDINDRIZ)
012400950112     D  FNDCIT       E                     EXTFLD(INDLOCALIT)
012500950112     D  FNDCAP       E                     EXTFLD(INDCAP)
012600950112     D  FNDPRV       E                     EXTFLD(INDPROV)
012700950112     D  FNDSTA       E                     EXTFLD(INDSTATO)
012800950112     D  FNDCAB       E                     EXTFLD(INDCAB)
012900950112     D  FNDLIN       E                     EXTFLD(INDLINGUA)
013000950112     D  FNDTLX       E                     EXTFLD(INDTELEX)
013100950112     D  FNDTEL       E                     EXTFLD(INDTELEFON)
013200950112     D  FNDTLF       E                     EXTFLD(INDTELEFAX)
013300950113     D  FRNCDP       E                     EXTFLD(FRNCONDPAG)
013400950112     D*  Ds per output DVCLI formato ANCLN+ANSOG+ANIND
013500950112     D DVOCLI        E DS                  EXTNAME(DVOCLIDS) INZ
013600950112     D* Anagrafica clienti
013700950112     D  CLNSOC       E                     EXTFLD(CLNSOCIETA)
013800950112     D  CLNUNT       E                     EXTFLD(CLNUNITA)
013900950112     D  CLNKCC       E                     EXTFLD(CLNKCC)
014000950112     D  CLNKSC       E                     EXTFLD(CLNKSC)
014100950112     D  CLNSOG       E                     EXTFLD(CLNSOGG)
014200950112     D  CLNTII       E                     EXTFLD(CLNTPIND)
014300950112     D  CLNCOI       E                     EXTFLD(CLNCDIND)
014400950112     D  CLNDES       E                     EXTFLD(CLNDESBREV)
014500950112     D  CLNLDV       E                     EXTFLD(CLNLINEAV)
014600950112     D  CLNFIL       E                     EXTFLD(CLNFILIALE)
014700950112     D  CLNDAC       E                     EXTFLD(CLNDTINRPC)
014800950112     D  CLNUTE       E                     EXTFLD(CLNUTENTE)
014900950112     D  CLNDVA       E                     EXTFLD(CLNDTULMO)
015000950112     D  CLNCCP       E                     EXTFLD(CLNCLIPREC)
015100950112     D  CLNCFP       E                     EXTFLD(CLNFORCORR)
015200950112     D  CLNCCF       E                     EXTFLD(CLNCLIFATT)
015300950112     D  CLNFAP       E                     EXTFLD(CLNADDSPBA)
015400950112     D  CLNFSB       E                     EXTFLD(CLNSPEBOLL)
015500950112     D  CLNCOF       E                     EXTFLD(CLNNRCPFAT)
015600950112     D  CLNIMB       E                     EXTFLD(CLNIMBUSTA)
015700950112     D  CLNFID       E                     EXTFLD(CLNFIDOSTA)
015800950112     D  CLNFIA       E                     EXTFLD(CLNFIDOASS)
015900950112     D  CLNDID       E                     EXTFLD(CLNDTFIST)
016000950112     D  CLNDIA       E                     EXTFLD(CLNDTFIAS)
016100950112     D  CLNCAS       E                     EXTFLD(CLNCLIASSO)
016200950112     D  CLNGDR       E                     EXTFLD(CLNGGRCVAG)
016300950112     D  CLNFBC       E                     EXTFLD(CLNCTRCONT)
016400950112     D  CLNFPC       E                     EXTFLD(CLNCITPROV)
016500950112     D  CLNASN       E                     EXTFLD(CLNSTATIST)
016600950112     D  CLNDAN       E                     EXTFLD(CLNDTFIRPC)
016700950112     D  CLNCLV       E                     EXTFLD(CLNCLASABC)
016800950112     D  CLNSOL       E                     EXTFLD(CLNTPTESTO)
016900950112     D  CLNMAG       E                     EXTFLD(CLNMAG)
017000950112     D  CLNADO       E                     EXTFLD(CLNADDIVA)
017100950112     D  CLNFAT       E                     EXTFLD(CLNFREQFAT)
017200950112     D  CLNTFA       E                     EXTFLD(CLNRAGGBOL)
017300950112     D  CLNRAF       E                     EXTFLD(CLNRAGGART)
017400950112     D  CLNMSP       E                     EXTFLD(CLNMDSPED)
017500950112     D  CLNRES       E                     EXTFLD(CLNRESA)
017600950112     D  CLNSSF       E                     EXTFLD(CLNSTASCOF)
017700950112     D  CLNFDP       E                     EXTFLD(CLNDETPREZ)
017800950112     D  CLNDIR       E                     EXTFLD(CLNDTINRPL)
017900950112     D  CLNDFR       E                     EXTFLD(CLNDTFIRPL)
018000950112     D  CLNLIS       E                     EXTFLD(CLNLISTINO)
018100950112     D  CLNVAL       E                     EXTFLD(CLNDIVISA)
018200950112     D  CLNFUC       E                     EXTFLD(CLNCAPODIS)
018300950112     D  CLNFUN       E                     EXTFLD(CLNFUNZRIO)
018400950112     D  CLNAGE       E                     EXTFLD(CLNAGENTE1)
018500950112     D  CLNAG2       E                     EXTFLD(CLNAGENTE2)
018600950112     D  CLNAG3       E                     EXTFLD(CLNAGENTE3)
018700950112     D  CLNPSP       E                     EXTFLD(CLNPRIOEVA)
018800950112     D  CLNBGC       E                     EXTFLD(CLNBKGSCLI)
018900950112     D  CLNRSC       E                     EXTFLD(CLNRAGGSCO)
019000950112     D  CLNRAP       E                     EXTFLD(CLNRAGGPRV)
019100950112     D  CLNTPP       E                     EXTFLD(CLNLORDNET)
019200950112     D  CLNCDP       E                     EXTFLD(CLNCONDPAG)
019300950112     D  CLNSCP       E                     EXTFLD(CLNSCOCASS)
019400950112     D  CLNPA1       E                     EXTFLD(CLNPERCAD1)
019500950112     D  CLNPA2       E                     EXTFLD(CLNPERCAD2)
019600950112     D  CLNRCO       E                     EXTFLD(CLNCONFORD)
019700950112     D  CLNRGU       E                     EXTFLD(CLNRAGGVEN)
019800950112     D  CLNSPP       E                     EXTFLD(CLNSTAPREZ)
019900950112     D  CLNCIV       E                     EXTFLD(CLNCDIVA)
020000950112     D  CLNTIN       E                     EXTFLD(CLNTPINDCM)
020100950112     D  CLNSC1       E                     EXTFLD(CLNSCOPAG1)
020200950112     D  CLNSC2       E                     EXTFLD(CLNSCOPAG2)
020300950112     D  CLNIP1       E                     EXTFLD(CLNGGINPA1)
020400950112     D  CLNIP2       E                     EXTFLD(CLNGGINPA2)
020500950112     D  CLNIP3       E                     EXTFLD(CLNGGINPA3)
020600950112     D  CLNTRA       E                     EXTFLD(CLNSESSTRA)
020700950112     D  CLNRIC       E                     EXTFLD(CLNSESSRIC)
020800950112     D  CLNTIV       E                     EXTFLD(CLNTPPVEND)
020900950112     D  CLNTPC       E                     EXTFLD(CLNTPCAUZ)
021000950112     D  CLNTSA       E                     EXTFLD(CLNACCONTO)
021100950112     D  CLNCES       E                     EXTFLD(CLNCLICESS)
021200950112     D  CLNFS1       E                     EXTFLD(CLNCLISOLL)
021300950112     D  CLNFS2       E                     EXTFLD(CLNADDSOLL)
021400950112     D  CLNFS5       E                     EXTFLD(CLNCONTRAS)
021500950112     D  CLNFS6       E                     EXTFLD(CLNADDPROD)
021600950112     D  CLNRGC       E                     EXTFLD(CLNRAGGCON)
021700950112     D  CLNFS3       E                     EXTFLD(CLNFLG1)
021800950112     D  CLNFS4       E                     EXTFLD(CLNFLG2)
021900950112     D  CLNFL3       E                     EXTFLD(CLNFLG3)
022000950112     D  CLNFL4       E                     EXTFLD(CLNFLG4)
022100950112     D  CLNFL5       E                     EXTFLD(CLNFLG5)
022200950112     D  CLNFL6       E                     EXTFLD(CLNFLG6)
022300950112     D* Anagrafica soggetti
022400950112     D  COGCOD       E                     EXTFLD(SOGSOGG)
022500950112     D  COGDAT       E                     EXTFLD(SOGDTENTR)
022600950112     D  COGDAU       E                     EXTFLD(SOGDTULMO)
022700950112     D  COGDAI       E                     EXTFLD(SOGDTINVL)
022800950112     D  COGDAF       E                     EXTFLD(SOGDTFIVL)
022900950112     D  COGDAN       E                     EXTFLD(SOGDTNASC)
023000950112     D  COGDES       E                     EXTFLD(SOGDES)
023100950112     D  COGDE1       E                     EXTFLD(SOGDESAGG)
023200950112     D  COGCDF       E                     EXTFLD(SOGCDFISC)
023300950112     D  COGIVA       E                     EXTFLD(SOGPARTIVA)
023400950112     D  COGFPI       E                     EXTFLD(SOGFRCPIVA)
023500950112     D  COGCIA       E                     EXTFLD(SOGISCCIAA)
023600950112     D  COGSES       E                     EXTFLD(SOGSEX)
023700950112     D  COGNAT       E                     EXTFLD(SOGNATGIUR)
023800950112     D  COGCOP       E                     EXTFLD(SOGCODPREC)
023900950112     D  COGABI       E                     EXTFLD(SOGABI)
024000950112     D  COGCLG       E                     EXTFLD(SOGCLFSOG)
024100950112     D* Anagrafica indirizzi
024200950112     D  CNDCOD       E                     EXTFLD(INDSOGG)
024300950112     D  CNDTII       E                     EXTFLD(INDTPIND)
024400950112     D  CNDCOI       E                     EXTFLD(INDCDIND)
024500950112     D  CNDDAU       E                     EXTFLD(INDDTULMO)
024600950112     D  CNDDAI       E                     EXTFLD(INDDTINVL)
024700950112     D  CNDDAF       E                     EXTFLD(INDDTFIVL)
024800950112     D  CNDDES       E                     EXTFLD(INDDESFIL)
024900950112     D  CNDDE1       E                     EXTFLD(INDDESAGG)
025000950112     D  CNDVIA       E                     EXTFLD(INDINDRIZ)
025100950112     D  CNDCIT       E                     EXTFLD(INDLOCALIT)
025200950112     D  CNDCAP       E                     EXTFLD(INDCAP)
025300950112     D  CNDPRV       E                     EXTFLD(INDPROV)
025400950112     D  CNDSTA       E                     EXTFLD(INDSTATO)
025500950112     D  CNDCAB       E                     EXTFLD(INDCAB)
025600950112     D  CNDLIN       E                     EXTFLD(INDLINGUA)
025700950112     D  CNDTLX       E                     EXTFLD(INDTELEX)
025800950112     D  CNDTEL       E                     EXTFLD(INDTELEFON)
025900950112     D  CNDTLF       E                     EXTFLD(INDTELEFAX)
026000950112     D*  DS per input DVCLI
026100950112     D DVCLI         E DS                  EXTNAME(DVCLIDS) INZ
026200950112     D* DS per accesso a DVFOR
026300950112     D DVFOR         E DS                  EXTNAME(DVFORDS) INZ
026400950112     D/EJECT
026500950109      *----------------------------------------------------*
026600970228     D* Passaggio Parametri
026700940127     D KPJBA         E DS
026800970228     D*---------------
026900030916B0611D*Tabella stato
027000030917B0611D ANGG12DS      E DS
027100030916     D*---------------
027200970228     D*Controllo codice CLE
027300970228     D X31CLERIDS    E DS
027400970228     D*---------------
027500950111     D* Parametri per ricerca codice ANATB00F
027600950111     D XTABDS          DS                  INZ
027700950112      * '  ' = Gestione
027800950112      * '01' = Ricerca
027900950112      * '02' = Gestione
028000950112      * '05' = Visualizzazione
028100950112      * '11' = Abilitazione
028200950111     D  OPZTAB                 1      2
028300950112      * 0 = ritorno normale
028400950112      * 1 = F3
028500950112      * 2 = F12
028600950111     D  RETTAB                 3      3
028700950112      * operazione eseguite dal pgm chiamato :
028800950112      * 0 = nessuna operazione
028900950112      * 1 = eseguito aggiornamento
029000950111     D  OPRTAB                 4      4
029100950112      * funzione non eseguibile per errore :
029200950112      * 0 = nessun errore
029300950112      * 1 = funzione richiamata chiusa in errore
029400950112      * Aggiungere eventuali altri codici di errore
029500950111     D  ERRTAB                 5      5
029600950112      * Azienda
029700950111     D  AZITAB                 6      8
029800950112      * Lingua
029900950111     D  LINTAB                 9      9
030000950112      * Codice scelto
030100950111     D  KEYTAB                10     24
030200950112      * riga cursore chiamante
030300950111     D  RIGTAB                25     26  0
030400950112      * colonna cursore chiamante
030500950111     D  COLTAB                27     28  0
030600950109      *-------------
030700950109      * DS Esterna gestione messaggi per COMIT
030800940318     D XMSGDS        E DS
030900940318     D  STS          E                     EXTFLD(MSGSTS)
031000940721     D                                     DIM(333)
031100940318     D  JBA          E                     EXTFLD(MSGJBA)
031200940721     D                                     DIM(502)
031300071127C2063D*-------------
031400071127  "  D WXmsginz        S              1A
031500071127  "  D $kcc            S                   Like(BraKcc)
031600071127  "  D $ksc            S                   Like(BraKsc)
031700071127  "  D $Banca          S                   LIKE(BraBanca)
031800071127  "  D $Linea          S                   LIKE(BraLinea)
031900071127  "  D Operazione      S              2
032000071127  "  D*-------------
032100071127  "  D ANBRA         E DS                  EXTNAME(ANBRA00F)
032200071127  "  D*-------------
032300071127  "  D* Definizioni parametri standard chiamate moduli
032400071127  "  D $Esito          s              1
032500071127  "  D $GesMsg         s              1
032600071127  "  D $LenOut         s              9B 0
032700071127c2063D $Struttura      s             10
032800950109      *-------------
032900950109      * Dati di ambiente ottenuti da XSOC
033000950109     D SOC001        E DS                  EXTNAME(XSOC001DS)
033100970228     D    XscLegisl                   1    overlay(xscSKP:2)                    Scadenz. retroattivo
033200950109      *-------------
033300950109      * DS Interna per dati di output di XSOC
033400941209     D XSOCDS          DS          1000
033500950109      *-------------
033600950109      * Passaggio Parametri al PGM di manutenzione
033700950109     D A027DS        E DS                  EXTNAME(ANA027DS)
033800950426     D*-------------
033900950426     D* Passaggio Parametri al PGM ANA200R
034000950426     D A200DS        E DS                  EXTNAME(ANA200DS)
034100950426     D  CMT200       E                     INZ('0')
034200950426     D  RET200       E                     INZ('0')
034300950426     D  OPR200       E                     INZ('0')
034400950426     D  ERR200       E                     INZ('0')
034500950426     D  FSC200       E                     INZ('S')
034600950426     D*-------------
034700950426     D* Passaggio Parametri al PGM ANA250R
034800950426     D A250DS        E DS                  EXTNAME(ANA250DS)
034900950426     D  CMT250       E                     INZ('0')
035000950426     D  RET250       E                     INZ('0')
035100950426     D  OPR250       E                     INZ('0')
035200950426     D  ERR250       E                     INZ('0')
035300950426     D  FSC250       E                     INZ('S')
035400950426     D*-------------
035500950426     D* Passaggio Parametri al PGM ANA300R
035600950426    >D A300DS        E DS                  EXTNAME(ANA300DS)
035700950426     D  CMT300       E                     INZ('0')
035800950426    >D  RET300       E                     INZ('0')
035900950426    >D  OPR300       E                     INZ('0')
036000950426    >D  ERR300       E                     INZ('0')
036100950426     D  FSC300       E                     INZ('S')
036200950426     D*-------------
036300950426     D* Passaggio Parametri al PGM ANA400R
036400950426     D A400DS        E DS                  EXTNAME(ANA400DS)
036500950426     D  CMT400       E                     INZ('0')
036600950426     D  RET400       E                     INZ('0')
036700950426     D  OPR400       E                     INZ('0')
036800950426     D  ERR400       E                     INZ('0')
036900950426     D  FSC400       E                     INZ('S')
037000950426     D*-------------
037100950109      *-----------------------------------------------------------
037200950109      * inserire tutte le altre DS di passaggio parametri
037300950109      *-----------------------------------------------------------
037400950109      * Passaggio Parametri al PGM XATB
037500950109     D XATBDS        E DS                  INZ
037600980424     D X59ABIDS      E DS                  INZ
037700950109      *
037800940201     D DSFMT           DS
037900940506     D  $TASTO               369    369
038000940201     D  NRG                  370    370
038100940201     D  NCL                  371    371
038200980714     D*-------------
038300980714     D* DS per UserExit
038400980714     D XUKDS         E DS                  EXTNAME(XUSRKEYDS) inz
038500001201A0846D*-------------
038600001201 "   D* DS per coordinata IBAN
038700030820A0846D CBAIBANDS     E DS                  inz
038800030820B0584D*-------------
038900030820  "  D* DS per ridefinizione video campo coordinate BBAN
039000030820  "  D D1COORI         DS
039100030820  "  D  D1COORI1
039200030820  "  D  D1COORI2
039300030820  "  D  D1COORI3
039400030820  "  D  D1COORI4
039500030820  "  D  D1COORI5
039600030820  "  D  D1COORI6
039700040531B0584D  D1COORI7
039800040531B0722D  D1COORI8
039900040531B0584D*-------------
040000030820  "  D* campi di appoggio per IBAN
040100030820  "  D WCOORI          S                   like(D1COORI)
040200030820  "  D WCCB2           S              6
040300030820B0584D WCCB            S             12
040400100922C2169D*-------------
040500100922  "  D* Ricerca values
040600100922  "  D A964DS        E DS                  EXTNAME(ANA964DS) INZ
040700100922  "  D*-------------
040800100922  "  D* Controlli validità legende
040900100922  "  D X07DS         E DS                  EXTNAME(X07VALDS) INZ
041000100923  "  D*-------------
041100100923  "  D* Reperimento parametri
041200100923  "  D XPaOut          DS          2000
041300100923  "  D* Reperimento parametri BONIFICI
041400100923C2169D ANP039DS      E DS                  INZ
041500950109      *-------------
041600950109      * posizione cursore
041700940201     D CURSOR          DS
041800940223     D  RIRI                   1      2B 0 INZ
041900940201     D  R$$                    2      2
042000940223     D  COCO                   3      4B 0 INZ
042100940201     D  C$$                    4      4
042200940207     D*-------------
042300940207     D* Reperimento nome PGM
042400940207     D STATUS         SDS           333
042500940207     D  DSPGM            *PROC
042600940225     D*-------------
042700940616     D* Costanti
042800941117     D* data a 0
042900950105     D $DT0            C                   CONST('0001-01-01')
043000940225     D*-------------
043100940506     D* Tasti di funzione
043200940506     D F01             C                   CONST(X'31')
043300940506     D F02             C                   CONST(X'32')
043400940506     D F03             C                   CONST(X'33')
043500940506     D F04             C                   CONST(X'34')
043600940506     D F05             C                   CONST(X'35')
043700940506     D F06             C                   CONST(X'36')
043800940506     D F07             C                   CONST(X'37')
043900940506     D F08             C                   CONST(X'38')
044000940506     D F09             C                   CONST(X'39')
044100940506     D F10             C                   CONST(X'3A')
044200940506     D F11             C                   CONST(X'3B')
044300940506     D F12             C                   CONST(X'3C')
044400940506     D F13             C                   CONST(X'B1')
044500940506     D F14             C                   CONST(X'B2')
044600940506     D F15             C                   CONST(X'B3')
044700940506     D F16             C                   CONST(X'B4')
044800940506     D F17             C                   CONST(X'B5')
044900940506     D F18             C                   CONST(X'B6')
045000940506     D F19             C                   CONST(X'B7')
045100940506     D F20             C                   CONST(X'B8')
045200940506     D F21             C                   CONST(X'B9')
045300940506     D F22             C                   CONST(X'BA')
045400940506     D F23             C                   CONST(X'BB')
045500940506     D F24             C                   CONST(X'BC')
045600940506     D ENTER           C                   CONST(X'F1')
045700940506     D ROLDWN          C                   CONST(X'F4')
045800940506     D ROLLUP          C                   CONST(X'F5')
045900950426     D*-------------
046000950426     D* Sottotitolo di testata
046100950426     D SOTTIT          C                   CONST('                    ')
046200980424     D*-------------
046300980424     D* Passaggio Parametri al PGM ANA194R
046400980424    >D Ana194Ds      E DS                  Inz
046500980424    >D  Err194       E                     Inz('0')
046600980424     D*-------------
046700980424     D* Passaggio Parametri al PGM ANA196R
046800980424    >D Ana196Ds      E DS                  Inz
046900980424    >D  Err196       E                     Inz('0')
047000980424     D*-------------
047100950109      * Anagrafica coordinate bancarie cli/for/altri
047200000114A0735D ZDateISO        S               D
047300020506B0437D*-------------
047400020506  "  D* Costante per controllo "caratteri solo numerici"
047500020506  "  D Digits          C                   '1234567890'
047600020506B0437D*-------------
047700060413A1152 * Sportello descrittivo completo
047800060413  "   *---
047900060413  "   * DS per chiamare ana227r
048000060413  "  D Ana227ds      e ds
048100060413  "   *---
048200060413  "   * DS multipla sportello descrittivo completo
048300060412  "  D $DimDsm         c                   const(100)
048400060412  "  D $Dsm            s              3s 0
048500060412  "  D SportedDSM      ds                  occurs($DimDsm)
048600060413  "  D   DSMNrRiga                         like(CBDNrRiga )
048700060413  "  D   DSMSporteD                        like(CBDSporteD)
048800060414  "   *-------------
048900060414  "  D $DataISO        s               d
049000060412  "   *-------------
049100060413  "   * dati nei messaggi di errore in Xmsgr
049200060413  "  D $IdMsg          s              7a   inz
049300060413  "  D $File           s             10a   inz
049400060413A1152 *----------------------------------------------------*
049500080211D2341D* Parametri del PGM di manutenzione ANGG12R2
049600080211  "  D D002DS          DS
049700080211  "  D  OPZ002                 1      2
049800080211  "  D* 0 = ritorno normale
049900080211  "  D* 1 = F3
050000080211  "  D* 2 = F12
050100080211  "  D  RET002                 3      3
050200080211  "  D* operazione eseguite dal pgm chiamato :
050300080211  "  D* 0 = nessuna operazione
050400080211  "  D* 1 = eseguito aggiornamento
050500080211  "  D  OPR002                 4      4
050600080211  "  D* funzione non eseguibile per errore :
050700080211  "  D* 0 = nessun errore
050800080211  "  D* 1 = funzione richiamata chiusa in errore
050900080211  "  D* Aggiungere eventuali altri codici di errore
051000080211  "  D  ERR002                 5      5
051100080211  "  D* Azienda
051200080211  "  D  AZI002                 6      8
051300080211  "  D* Lingua
051400080211  "  D  LIN002                 9      9
051500080211  "  D* Codice scelto
051600080211  "  D  KEY002                10     24
051700080211  "  D*-------------
051800080211  "  D SavStaCAB       S                   Like(X59Stato)
051900080211  "  D SavNazCAB       S                   Like(NazG12  )
052000080211  "  D WStato          S                   Like(X59Stato)
052100080208  "  D $FoundSta       S              1
052200080208D2341D $FineG12        S              1
052300071029A1250 *-------------
052400071029  "  D $Stato          s                   like(D1Stato)
052500071029  "  D $check          s                   like(D1Check)
052600071029  "  D $Coori          s                   like(D1coori)
052700071029  "  D $err            s              1
052800071029  "  D $Opz            s              2
052900071029A1250 *****************************************************************
053000101105c2169 *-------------
053100101105  "  D D1CIN           DS
053200101105  "  D D1CCO                   1      1
053300101111  "  D D1CLE                   1      2
053400101111C2169D BYTE2                   2      2
053500950105     IANCBA000
053600950105     I              CBASOCIETA                  CBASOC
053700950105     I              CBAKCC                      CBAKCC
053800950105     I              CBAKSC                      CBAKSC
053900950105     I              CBAUTENTE                   CBACTM
054000950105     I              CBADTULMO                   CBADFM
054100950105     I              CBACCB                      CBACCB
054200950105     I              CBAABI                      CBAABI
054300950105     I              CBACAB                      CBACAB
054400950105     I              CBAENTITA                   CBAENT
054500950105     I              CBACLIFOR                   CBACOF
054600950105     I              CBATPAPPO                   CBATAP
054700950105     I              CBADTINVL                   CBADAL
054800950105     I              CBACDCTRL                   CBACCO
054900950105     I              CBAKCCNSBA                  CBAKCB
055000950105     I              CBAKSCNSBA                  CBAKSB
055100100922C2169I              CBAFLAG                     CBATPH
055200040220A1067I              CBASTATO                    CBASTA
055300950111     IANRCO000
055400950207     I              RCOSOCIETA                  RCOSOC
055500950207     I              RCOKCC                      RCOKCC
055600950207     I              RCOKSC                      RCOKSC
055700950207     I              RCOKCCSOGG                  RCOCSO
055800950207     I              RCOSOGG                     RCOSOG
055900950207     I              RCOTPIND                    RCOTII
056000950207     I              RCOCDIND                    RCOCOI
056100950207     I              RCODTANN                    RCODAL
056200950207     I              RCOUTEIMM                   RCOCTI
056300950207     I              RCODTIMM                    RCODFI
056400950207     I              RCOUTEULMO                  RCOCTM
056500950207     I              RCODTULMO                   RCODFM
056600950207     I              RCOTPCONTO                  RCOTIC
056700950207     I              RCOMOVIMLE                  RCOMOV
056800950207     I              RCOTPGEST                   RCOTGE
056900950207     I              RCOSTORICO                  RCOSTO
057000950207     I              RCOSCADENZ                  RCOSCA
057100950207     I              RCORATE                     RCORAT
057200950207     I              RCONATURA                   RCONAT
057300950207     I              RCOSNATURA                  RCOSNA
057400950207     I              RCOAGENTE                   RCOAGE
057500950207     I              RCOPERCIPI                  RCOFPR
057600950207     I              RCOTECNICO                  RCOTEC
057700950207     I              RCOSALDNAT                  RCOSAN
057800950207     I              RCORATEORI                  RCORER
057900950207     I              RCORAPPTES                  RCOTES
058000950207     I              RCOITAEST                   RCOCLA
058100950207     I              RCOTPCNTGR                  RCOESC
058200950207     I              RCOSOCAVAL                  RCOGRU
058300950207     I              RCORGABCTB                  RCORAC
058400950207     I              RCORGABENT                  RCORAE
058500950207     I              RCORGSQDIM                  RCORSD
058600950207     I              RCOALLEIVA                  RCOMEP
058700950207     I              RCOOBBLMC                   RCOLMC
058800950207     I              RCOOBBLDIV                  RCOLDV
058900950207     I              RCODIVISA                   RCODIV
059000950207     I              RCOPREPORT                  RCOCPP
059100950207     I              RCOTABPNOT                  RCOTPN
059200950207     I              RCOUNITA                    RCOUNT
059300950207     I              RCOKCCESTR                  RCOKCE
059400950207     I              RCOKSCESTR                  RCOKSE
059500950207     I              RCOBKAMM                    RCOABL
059600950207     I              RCOKCCFACT                  RCOKCF
059700950207     I              RCOKSCFACT                  RCOKSF
059800950207     I              RCOCTGAN01                  RCOA01
059900950207     I              RCOCTGAN02                  RCOA02
060000950207     I              RCOCTGAN03                  RCOA03
060100950207     I              RCOCTGAN04                  RCOA04
060200950207     I              RCOCTGAN05                  RCOA05
060300950207     I              RCOCTGAN06                  RCOA06
060400950207     I              RCOCTGAN07                  RCOA07
060500950207     I              RCOCTGAN08                  RCOA08
060600950207     I              RCOCTGAN09                  RCOA09
060700950207     I              RCOCTGAN10                  RCOA10
060800950207     I              RCOCTGAN11                  RCOA11
060900950207     I              RCOCTGAN12                  RCOA12
061000950207     I              RCOCTGAN13                  RCOA13
061100950207     I              RCOCTGAN14                  RCOA14
061200950207     I              RCOCTGAN15                  RCOA15
061300950207     I              RCOCTGAN16                  RCOA16
061400950207     I              RCOCTGAN17                  RCOA17
061500950207     I              RCOCTGAN18                  RCOA18
061600950207     I              RCOCTGAN19                  RCOA19
061700950207     I              RCOCTGAN20                  RCOA20
061800950207     I              RCOCTGAN21                  RCOA21
061900950207     I              RCOCTGAN22                  RCOA22
062000950207     I              RCOCTGAN23                  RCOA23
062100950207     I              RCOCTGAN24                  RCOA24
062200950207     I              RCOCTGAN25                  RCOA25
062300950207     I              RCOCTGAN26                  RCOA26
062400950207     I              RCOCTGAN27                  RCOA27
062500950207     I              RCOCTGAN28                  RCOA28
062600950207     I              RCOCTGAN29                  RCOA29
062700950207     I              RCOCTGAN30                  RCOA30
062800950207     I              RCORIFANA1                  RCORA1
062900950207     I              RCOTPRIFA1                  RCOTR1
063000950207     I              RCORIFANA2                  RCORA2
063100950207     I              RCOTPRIFA2                  RCOTR2
063200950207     I              RCORIFANA3                  RCORA3
063300950207     I              RCOTPRIFA3                  RCOTR3
063400950207     I              RCORIFANA4                  RCORA4
063500950207     I              RCOTPRIFA4                  RCOTR4
063600950207     I              RCORIFANA5                  RCORA5
063700950207     I              RCOTPRIFA5                  RCOTR5
063800950207     I              RCORIFFRE1                  RCORL1
063900950207     I              RCORIFFRE2                  RCORL2
064000950207     I              RCORIFFRE3                  RCORL3
064100950207     I              RCOANALIT                   RCOANL
064200950207     I              RCOOBBQTA                   RCOOQT
064300950207     I              RCORGRIPVA                  RCORRV
064400950207     I              RCOVOCE                     RCOVOC
064500950207     I              RCOPRECCOD                  RCOLIB
064600950207     I              RCOLIVRIS                   RCOFRI
064700950207     I              RCOSESSTRA                  RCOTRA
064800950207     I              RCOSESSRIC                  RCORIC
064900950207     I              RCOFLGOPE                   RCOOPZ
065000950207     I              RCOFLGUTE                   RCOFLA
065100950905      * Anagrafica piano dei conti
065200950905     IANPDC000
065300950905     I              PDCSOCIETA                  PDCSOC
065400950905     I              PDCDESCR                    PDCRSO
065500950905     I              PDCDESAGG                   PDCDEA
065600950905     I              PDCDESBREV                  PDCDES
065700950905     I              PDCCTGKCC                   PDCCTG
065800950905     I              PDCCTGFISC                  PDCCTF
065900950905     I              PDCSOTGRU                   PDCSTG
066000950905     I              PDCCOOBAN                   PDCCOB
066100950905     I              PDCRICLVEL                  PDCRCV
066200950905     I              PDCFLAGS                    PDCFLG
066300950109      * Anagrafica coordinate bancarie descrittive
066400950105     IANCBD000
066500950105     I              CBDSOCIETA                  CBDSOC
066600950105     I              CBDKCC                      CBDKCC
066700950105     I              CBDKSC                      CBDKSC
066800950109     I              CBDENTITA                   CBDENT
066900950105     I              CBDCLIFOR                   CBDCOF
067000950105     I              CBDTPAPPO                   CBDTAP
067100950105     I              CBDDTINVL                   CBDDAL
067200060411A1152X************* CBDISTITUT                  CBDIST
067300060411  "  X************* CBDAGENZIA                  CBDAGE
067400060411A1152X************* CBDLOCALIT                  CBDLOC
067500950109      *----------------------------------------------------*
067600950109      *                MAIN LINE PROGRAM
067700950109      *----------------------------------------------------*
067800950109      *
067900950109      * inizializzazione variabili
068000940223     C                   EXSR      INZVAR
068100950109      *
068200940223     C     $FINE         DOWEQ     *OFF
068300940202     C     $GEST         CASEQ     'D1'          GESD1
068400950105    > ****       $GEST     CASEQ'D2'      GESD2
068500941027     C                   ENDCS
068600941027     C                   ENDDO
068700950109      *
068800950109      * fine programma
068900940325     C                   EXSR      ENDPGM
069000940325     C/EJECT
069100950109      ************************************************************
069200950109      * RICERCHE
069300950109      ************************************************************
069400940325     C     ENDPGM        BEGSR
069500950109      *
069600950109      * se il COMMIT non è demandato al pgm chiamante
069700980223A0240C*
069800980223A024>C*    CMT027        IFEQ      '0'
069900980223A0240C*
070000980223A0240C* test se esistono transazioni attive ed eventualmente cosa fare
070100980223A0240C*                  Z-ADD     0             EXECOD
070200980223A0240C*                  CALL      'XRCMTI'
070300980223A0240C*                  PARM                    EXECOD            1 0
070400980223A0240C*
070500980223A0240C*                  SELECT
070600980223A0240C* Annulla richiesta
070700980223A0240C*    EXECOD        WHENEQ    1
070800980223A0240C*                  MOVEL     *OFF          $FINE
070900980223A0240C*                  MOVEL     *HIVAL        $LASTV
071000980223A0240C* Conferma uscita
071100980223A0240C*    EXECOD        WHENEQ    2
071200950109      ****************     ROLBK
071300980223A0240C*
071400980223A0240C*                  ENDSL
071500950109      *
071600980223A0240C*                  ENDIF
071700950109      *
071800950109      * se ho confermato l'uscita proseguo
071900941214     C     $FINE         IFEQ      *ON
072000950109      *
072100950109      * disallocazione rcd nel caso di modifica/annullamento
072200950105    >C     OPZ027        IFEQ      '02'
072300950105    >C     OPZ027        OREQ      '04'
072400970625    >C                   UNLOCK    ANCBA01L
072500970625    >C                   UNLOCK    ANCBD01L
072600941027     C                   ENDIF
072700950109      *
072800950109      * passaggio dati al pgm chiamante
072900950105    >C                   MOVEL     A027DS        KPJBU
073000950109      *
073100940325     C                   SETON                                            RT
073200940616     C                   RETURN
073300950109      *
073400941214     C                   ENDIF
073500950109      *
073600940325     C                   ENDSR
073700940325     C/EJECT
073800950109      ************************************************************
073900950109      * RICERCHE
074000950109      ************************************************************
074100940127     C     SEARCH        BEGSR
074200950109      *
074300950109      * determino Riga/Colonna del cursore
074400940127     C                   MOVE      NRG           R$$
074500940127     C                   MOVE      NCL           C$$
074600940127     C                   Z-ADD     RIRI          CURR
074700940127     C                   Z-ADD     COCO          CURC
074800950109      *
074900950109      * I campi che al momento dell'F4 risultano protetti non devono
075000950109      * essere abilitati alla funzione di ricerca
075100950109      *
075200941027     C                   SELECT
075300950426    > *
075400950426      * FMTD1  - CLIENTI
075500950426    >C     H1NMFL        WHENEQ    'D1COD'
075600950426    >C     D1COF         ANDEQ     'C'
075700950426    >C                   RESET                   A300DS
075800950426    >C                   MOVEL     '01'          OPZ300
075900950426    >C                   MOVEL     D1COD         KSC300
076000950426    >C                   MOVEL     A300DS        KPJBU
076100961212    >C                   CALL      'ANA300R'                            21
076200950426     C                   PARM                    KPJBA
076300950426    >C                   MOVEL     KPJBU         A300DS
0764009504262   >C     KSC300        IFNE      *BLANK
076500950426    >C                   MOVE      KSC300        D1COD
0766009504262-  >C                   ENDIF
076700950426      * imposto pos. del cursore
076800950426     C                   Z-ADD     CURR          H1RIGA
076900950426     C                   Z-ADD     CURC          H1COLO
077000950426      *
077100950426      * FMTD1  - FORNITORI
077200950426     C     H1NMFL        WHENEQ    'D1COD'
077300950426    >C     D1COF         ANDEQ     'F'
077400950426    >C                   RESET                   A400DS
077500950426    >C                   MOVEL     '01'          OPZ400
077600950426    >C                   MOVEL     D1COD         KSC400
077700950426    >C                   MOVEL     A400DS        KPJBU
077800961212    >C                   CALL      'ANA400R'                            21
077900950426     C                   PARM                    KPJBA
078000950426    >C                   MOVEL     KPJBU         A400DS
0781009504262   >C     KSC400        IFNE      *BLANK
078200950426    >C                   MOVE      KSC400        D1COD
0783009504262-  >C                   ENDIF
078400950426      * imposto pos. del cursore
078500950426     C                   Z-ADD     CURR          H1RIGA
078600950426     C                   Z-ADD     CURC          H1COLO
078700950426      *
078800950426      * FMTD1  - CAPOCONTI
078900950426     C     H1NMFL        WHENEQ    'D1KCC'
079000950426    >C                   RESET                   A200DS
079100950426    >C                   MOVEL     '01'          OPZ200
079200950426    >C                   MOVEL     D1KCC         KCC200
079300950426    >C                   MOVEL     A200DS        KPJBU
079400950426    >C                   CALL      'ANA200R'
079500950426     C                   PARM                    KPJBA
079600950426    >C                   MOVEL     KPJBU         A200DS
0797009504262   >C     KCC200        IFNE      *BLANK
079800950426    >C                   MOVE      KCC200        D1KCC
0799009504262-  >C                   ENDIF
080000950426      * imposto pos. del cursore
080100950426     C                   Z-ADD     CURR          H1RIGA
080200950426     C                   Z-ADD     CURC          H1COLO
080300950426      *
080400950426      * FMTD1  - SOTTOCONTI
080500950426     C     H1NMFL        WHENEQ    'D1KSC'
080600950426    >C                   RESET                   A250DS
080700950426    >C                   MOVEL     '01'          OPZ250
080800950426    >C                   MOVEL     D1KSC         KSC250
080900950426    >C                   MOVEL     A250DS        KPJBU
081000950426    >C                   CALL      'ANA250R'
081100950426     C                   PARM                    KPJBA
081200950426    >C                   MOVEL     KPJBU         A250DS
0813009504262   >C     KSC250        IFNE      *BLANK
081400950426    >C                   MOVE      KCC250        D1KCC
081500950426    >C                   MOVE      KSC250        D1KSC
0816009504262-  >C                   ENDIF
081700950426      * imposto pos. del cursore
081800950426     C                   Z-ADD     CURR          H1RIGA
081900950426     C                   Z-ADD     CURC          H1COLO
082000950109      *
082100950109      * FMTD1  - PACCHETTO
082200950110     C     H1NMFL        WHENEQ    'D1TAP'
082300950110     C                   MOVE      *BLANK        WWTAP
082400950110     C                   MOVE      WWTAP         KEYTAB
082500950110     C                   MOVE      '01'          OPZTAB
082600990810A0692C******             MOVE      XSCSOC        AZITAB
082700990810A0692C                   MOVE      WWWSOC        AZITAB
082800950110     C                   MOVE      XSCLIN        LINTAB
082900950110     C                   CLEAR                   KPJBU
083000950110     C                   MOVEL     XTABDS        KPJBU
083100950110     C                   CALL      'ANGG25R'
083200950110     C                   PARM                    KPJBA
083300950110     C                   MOVEL     KPJBU         XTABDS
083400950110     C                   MOVE      KEYTAB        WWTAP
083500950110      *
0836009501102   >C     WWTAP         IFNE      *BLANK
083700950110    >C                   MOVEL     WWTAP         D1TAP
083800950110    >C                   MOVEL     *BLANK        D1DTAP
0839009501102-  >C                   ENDIF
084000950109      * imposto pos. del cursore
084100980424     C                   Z-Add     CurR          H1Riga
084200980424     C                   Z-Add     CurC          H1Colo
084300980424     C* ABI
084400980424    >C     H1NMFL        WhenEq    'D1ABI'
084500980424    >C                   Reset                   Ana194Ds
084600980424     C                   Movel     '01'          Opz194
084700980424    >C                   Movel     D1ABI         ABI194
084800980424    >C                   Movel     WWWSOC        SOCIETA194
084900980424    >C                   Movel     Ana194Ds      KPJBU
085000980424    >C                   Call      'ANA194R'
085100980424     C                   Parm                    KPJBA
085200980424    >C                   Movel     KPJBU         Ana194Ds
0853009804242   >C                   If        Err194 <> '1'
0854009804242   >C                             and Abi194 <> *blank
085500980424    >C                   Move      ABI194        D1ABI
085600980424     C                   Move      *blanks       D1DIst
085700980424     C                   Move      *blanks       D1DAge
085800980424     C                   Move      *blanks       D1DLoc
0859009804242-  >C                   EndIf
086000950109      *
086100950111      * imposto pos. del cursore
086200980424     C                   Z-ADD     CURR          H1RIGA
086300950111     C                   Z-ADD     CURC          H1COLO
086400980424     C* CAB
086500980424    >C     H1NMFL        WhenEq    'D1CAB'
086600980424    >C                   Reset                   Ana196Ds
086700980424     C                   Movel     '01'          Opz196
086800021218     C                   Movel     D1ABI         ABI196
086900980424     C                   Movel     D1CAB         CAB196
087000980424    >C                   Movel     WWWSoc        SOCIETA196
087100980424    >C                   Movel     Ana196Ds      KPJBU
087200980424    >C                   Call      'ANA196R'
087300980424     C                   Parm                    KPJBA
087400980424    >C                   Movel     KPJBU         Ana196Ds
0875009804242   >C                   If        Err196 <> '1'
0876009804242   >C                             and CAB196 <> *blank
087700980424    >C                   Move      CAB196        D1CAB
087800021218b0511C                   Move      abi196        D1Abi
087900980424     C                   Move      *blanks       D1DIst
088000980424     C                   Move      *blanks       D1DAge
088100980424     C                   Move      *blanks       D1DLoc
088200980424     C                   If        ABI196 <> *Blank
088300980424     C                             and D1ABI = *Blank
088400980424     C                   Move      ABI196        D1ABI
088500980424     C                   EndIf
0886009804242-  >C                   EndIf
088700980424      *
088800980424      * imposto pos. del cursore
088900980424     C                   Z-ADD     CURR          H1RIGA
089000980424     C                   Z-ADD     CURC          H1COLO
089100950905      *
089200950905      * FMTD1  - CAPOCONTO BANCA CORRISPONDENTE
089300950905     C     H1NMFL        WHENEQ    'D1KCB'
089400950905    >C                   RESET                   A200DS
089500950905    >C                   MOVEL     '01'          OPZ200
089600950905    >C                   MOVEL     D1KCB         KCC200
089700950905     C                   MOVEL     '2'           SNA200
089800950905    >C                   MOVEL     A200DS        KPJBU
089900950905    >C                   CALL      'ANA200R'
090000950905     C                   PARM                    KPJBA
090100950905    >C                   MOVEL     KPJBU         A200DS
0902009509052   >C     KCC200        IFNE      *BLANK
090300950905    >C                   MOVE      KCC200        D1KCB
0904009509052-  >C                   ENDIF
090500950905      * imposto pos. del cursore
090600950905     C                   Z-ADD     CURR          H1RIGA
090700950905     C                   Z-ADD     CURC          H1COLO
090800950905      *
090900950905      * FMTD1  - SOTTOCONTO BANCA CORRISPONDENTE
091000950905     C     H1NMFL        WHENEQ    'D1KSB'
091100950905    >C                   RESET                   A250DS
091200950905    >C                   MOVEL     '01'          OPZ250
091300021122B0499C                   MOVEL     D1KCB         KCC250
091400950905    >C                   MOVEL     D1KSB         KSC250
091500950905     C                   MOVEL     '2'           SNA250
091600950905    >C                   MOVEL     A250DS        KPJBU
091700950905    >C                   CALL      'ANA250R'
091800950905     C                   PARM                    KPJBA
091900950905    >C                   MOVEL     KPJBU         A250DS
0920009509052   >C     KSC250        IFNE      *BLANK
092100950905    >C                   MOVE      KCC250        D1KCB
092200950905    >C                   MOVE      KSC250        D1KSB
0923009509052-  >C                   ENDIF
092400950905      * imposto pos. del cursore
092500950905     C                   Z-ADD     CURR          H1RIGA
092600950905     C                   Z-ADD     CURC          H1COLO
092700030917B0611 * FMTD1  - stato
092800030917 "   C     H1NMFL        WHENEQ    'D1STATO'
092900030917 "   C                   RESET                   XTABDS
093000030917 "   C                   if        Opz027 = '05'
093100030917 "   C                   move      '05'          OPZTAB
093200030917 "   C                   else
093300030917 "   C                   MOVE      '01'          OPZTAB
093400030917 "   C                   ENDIF
093500030917 "   C                   CLEAR                   KPJBU
093600030917 "   C                   MOVEL     XTABDS        KPJBU
093700030917 "   C                   CALL      'ANGG12R'
093800030917 "   C                   PARM                    KPJBA
093900030917 "   C                   MOVEL     KPJBU         XTABDS
094000030917 "    *
094100080208B0611C     RETTAB        IFEQ      *BLANK
094200080208D2341X*ex B0611          MOVE      KEYTAB        W4                4
094300080208  "  X*ex A1250 aggancio tabella stato perchè lo stato deve essere il campo
094400080208  "  X*ex A1250  della tabella nazg12 e non il campo chiave
094500080208  "  X*ex A1250          move      '1'           XTBRic
094600080208  "  X*ex A1250          move      XSCSOC        XTBAzi
094700080208  "  X*ex A1250          move      *Zero         XTBKey
094800080208  "  X*ex A1250          move      'G12'         XTBCod
094900080208  "  X*ex A1250          move      W4            xtbkey
095000080208  "  X*ex A1250          callb     'XATB'
095100080208  "  X*ex A1250          parm                    XATBDS
095200080208  "  X*ex A1250          if        XtbErr = '0'
095300080208D2341X*ex A1250          movel     XTBUni        ANGG12DS
095400080208A1250x****               MOVEL     W4            D1STATO
095500080208D2341C*
095600101105  "  C* reperisco la nazione CEE collegata al codice stato G12 scelto
095700080208  "  C                   Move      KEYTAB        WStato
095800080208  "  C                   EXSR      RepTabG12
095900080208D2341C                   if        XTbErr      = *Off
096000101105C2169X*******************MOVEL     NazG12        D1STATO
096100101105  "  c* non bisogna usare il campo nazG12 usato solo per nazioni cee
096200101105  "  c* ma codiso2G12
096300101105C2169C                   MOVEL     CodISO2G12    D1STATO
096400080208A1250c                   endif
096500080208D2341C*
096600071113B0611C                   ENDIF
096700030917 "    * imposto pos. del cursore
096800030917 "   C                   Z-ADD     CURR          H1RIGA
096900030917B0611C                   Z-ADD     CURC          H1COLO
097000130103B1206X* EX A1152 FMTD1  - sportello descrittivo
097100130103  "  X****   "      H1NMFL        WHENEQ    'D1SPORTED'
097200130103  "  X****   "                    exsr      F04_SporteD
097300130103  "  X****   "
097400130103B1206X* EX A1152
097500080211D2341C*
097600080211  "  C* FMTD1  - conto corrente
097700080211  "  C     H1NMFL        WHENEQ    'D1CCB'
097800080211  "  C                   EXSR      F04_CCB
097900080211D2341C*
098000100922C2169C* Tipologia flusso
098100100922  "  C     H1NMFL        WHENEQ    'D1TPHOMEB'
098200100922  "  C                   RESET                   A964DS
098300100922  "  C                   MOVEL     '01'          OPZ964
098400100922  "  C                   MOVEL     'ANDIZ000'    TRC964
098500100922  "  C                   MOVEL     'DSPTPHOMEB'  CAM964
098600100923  "  C                   MOVE      *ON           ALL964
098700100922  "  C                   MOVEL     A964DS        KPJBU
098800100922  "  C                   CALL      'ANA964R'
098900100922  "  C                   PARM                    KPJBA
099000100922  "  C                   MOVEL     KPJBU         A964DS
099100100922  "  C*
099200100924  "  c                   if        ret964 <> '2'
099300100922  "  C                   MOVEL     VAL964        D1TpHomeB
099400100922  "  C                   endif
099500100922  "  c*
099600100922  "   * imposto pos. del cursore
099700100922  "  C                   Z-ADD     CURR          H1RIGA
099800100922C2169C                   Z-ADD     CURC          H1COLO
099900950109      *
100000950109      * ricerca non abilitata in questa posizione
100100941027     C                   OTHER
100200950109      * imposto pos. del cursore
100300940926     C                   Z-ADD     CURR          H1RIGA
100400940926     C                   Z-ADD     CURC          H1COLO
100500940926     C                   SETON                                        9899
100600950109      *
100700941027     C                   ENDSL
100800950109      *
100900940506     C                   MOVE      *HIVAL        $LASTV
101000940127     C                   ENDSR
101100940225     C/EJECT
101200060412A1152 ************************************************************
101300060412  "   * ricerca sportello descrittivo
101400060412  "   ************************************************************
101500060412  "  C     F04_SporteD   Begsr
101600060412  "   *
101700060412  "   * ------------------------------------------------------
101800060412  "   * chiamo gestione sportello descrittivo
101900060412  "  C                   reset                   Ana227ds
102000060412  "   *
102100060412  "   *  opz
102200060412  "  C                   move      Opz027        Opz227
102300060412  "   *   se richiesta cancellazione ancbA, l'F4 sullo sport. descr
102400060412  "   *   consente la sola visualizzazione dello sportello stesso
102500060412  "  C                   if        Opz027 = '04'
102600060412  "  C                   move      '05'          Opz227
102700060412  "  C                   endif
102800060412  "   *   se richiesta la modifica di ancbA e non esiste ancbD, l'F4
102900060412  "   *   sullo sport. andrà in inserimento dello sportello stesso
103000060412  "  C                   if        Opz027 = '02' and ChnCbd = *off
103100060412  "  C                   move      '01'          Opz227
103200060412  "  C                   endif
103300060412  "   *  soc
103400060412  "  C                   movel     XScSoc        Societa227
103500060413  "   *  kiave:
103600060413  "   *  come kiave vengono passati sempre i dati presenti a video;
103700060413  "   *  (tali dati potrebbero essere diversi rispetto a quelli
103800060413  "   *   caricati inizialmente in ana027d)
103900060413  "   *  - se è la 1° chiamata, in ana227d vedrò i dati dell'ancbd
104000060413  "   *    iniziale di entrata in ana027d;
104100060413  "   *  - se non è la 1° chiamata, in ana227d vedrò i dati come
104200060413  "   *    li ho lasciati la volta precedente;
104300060413  "   *
104400060412  "  C                   movel     D1CoF         CliFor227
104500060412  "  C                   movel     D1Kcc         Kcc227
104600060412  "  C                   movel     D1Ksc         Ksc227
104700060413  "  C                   movel     D1Cod         Cod227
104800060412  "  C                   movel     D1CoFD        CliForD227
104900060412  "  C                   movel     D1Tap         TpAppo227
105000060412  "  C                   movel     D1DTap        TpAppoD227
105100060412  "   *
105200060412  "  C                   movel     Ana227ds      Kpjbu
105300060413  "  C                   call      'ANA227R'
105400060412  "  C                   parm                    Kpjba
105500060412  "  C                   parm                    SportedDSM
105600060412  "   *
105700060412  "  C                   movel     Kpjbu         Ana227ds
105800060412  "   *
105900060412  "   * ritorno da gestione sportello descrittivo
106000060412  "  C                   exsr      Rtn227
106100060412  "   *
106200060412  "  C                   Z-ADD     CURR          H1RIGA
106300060412  "  C                   Z-ADD     CURC          H1COLO
106400060412  "   *
106500060412  "  C     XF04_SporteD  Endsr
106600060412  "   ************************************************************
106700060412  "   * riempio ds multipla-sportello descrittivo direttamente
106800060412  "   * dai dati di ancbd01l
106900060412  "   ************************************************************
107000060412  "  C     RieSported    Begsr
107100060412  "   *
107200060412  "  C                   z-add     0             $Dsm
107300060412  "   *
107400060412  "   * leggo ancbd01l
107500060413  "  C                   movel     WWWSoc        CBDSoc
107600060413  "  C                   movel     Cof027        CBDCof
107700060413  "  C                   movel     Kcc027        CBDKcc
107800060413  "  C                   movel     Ksc027        CBDKsc
107900060413  "  C                   movel     Ent027        CBDEnt
108000060413  "  C                   movel     Tap027        CBDTap
108100060413  "  C                   movel     Dal027        CBDDal
108200060413  "  C                   z-add     1             CBDNrRiga
108300060412  "  C     K8CBD1        setll     ANCBD01L
108400060412  "  C                   exsr      ReadAncbd
108500060412  "   *
108600060412  "   * fino a quando ancbd01l c'è e non è allocato
108700060412  "   * e non ho riempito tutta la ds multipla
108800060412  "  C                   dow       *in24=*off  and *in23=*off
108900060412  "  C                             and $Dsm < $DimDsm
109000060412  "   *
109100060412  "   *  imposto ds multipla sportello descrittivo
109200060412  "  C                   add       1             $Dsm
109300060412  "  C     $Dsm          occur     SportedDSM
109400060412  "  C                   z-add     CBDNrRiga     DSMNrRiga
109500060412  "  C                   movel     CBDSporteD    DSMSporteD
109600060412  "   *
109700060412  "   *  leggo ancbd01l
109800060412  "  C                   exsr      ReadAncbd
109900060412  "  C                   enddo
110000060412  "   *
110100060412  "   *
110200060414  "   * se ancbd01l allocato, rinizializzo ds multipla
110300060412  "  C                   if        *in24 = *on
110400060412  "  C                   exsr      ClrSportedDsm
110500060412  "  C                   endif
110600060412  "   *
110700060412  "   *
110800060414  "  C     XRieSporteD   Endsr
110900060412  "   ************************************************************
111000060412  "   * inizializzazione ds multipla Sportello descrittivo
111100060412  "   ************************************************************
111200060412  "  C     ClrSportedDsm Begsr
111300060412  "   *
111400060412  "  C     1             do        $DimDsm       $Dsm
111500060412  "  C     $Dsm          occur     SportedDSM
111600060412  "  C                   clear                   SportedDSM
111700060412  "  C                   enddo
111800060412  "   *
111900060412  "  C     XClrSportedDsmEndsr
112000060412  "   ************************************************************
112100060412  "   * leggo ancbd01l per caricare SportedDsm per chiamare ana227r
112200060412  "   ************************************************************
112300060412  "  C     ReadAncbd     Begsr
112400060412  "   *
112500060412  "  C                   dou       *in24=*off or MsgRtn<>'R'
112600060412  "   *
112700060412  "   *   se Modifica/Annullamento, il rcd che leggo viene bloccato
112800060412  "  C                   if        Opz027='02' or Opz027='04'
112900060412  "  C     K7CBD1        reade     Ancbd01L                             2423
113000060412  "   *
113100060412  "   *   se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
113200060412  "  C                   else
113300060412  "  C     K7CBD1        reade(n)  Ancbd01L                             2423
113400060412  "   *
113500060412  "  C                   endif
113600060412  "   *
113700060412  "   *   Gestione RCD allocato
113800060412  "  C                   if        *in24 = *on
113900060412  "  C                   movel     'ANCBD00F'    MsgFil
114000060412  "  C                   exsr      MsgPgm
114100060412  "  C                   endif
114200060412  "   *
114300060412  "  C                   enddo
114400060412  "   *
114500060412  "  C     XReadAncbd    Endsr
114600060412  "   ************************************************************
114700060412  "   * Ritorno da gestione sportello descrittivo
114800060412  "   ************************************************************
114900060412  "  C     Rtn227        Begsr
115000060412  "   *
115100060412  "   * operazione eseguite dal pgm chiamato
115200060412  "   *
115300060412  "  C                   select
115400060412  "   *
115500060413  "   * 1 = eseguito aggiornamento
115600060412  "  C                   when      Opr227 = '1'
115700060412  "   *
115800060413  "   *  sport. descr può cambiare solo se non annullam o visual.
115900060412  "  C                   if        Opz027<>'04' and Opz227<>'05'
116000060412  "   *
116100060412  "   *  carico a video solo la 1° riga dello sportello descrittivo
116200060412  "   *  (se è vuota la 1° riga, è vuoto tutto lo sportello desc.)
116300060412  "  C     1             occur     SportedDSM
116400060413  "  C                   movel     DSMSported    D1SporteD
116500060413  "  C                   if        DSMSported <> *blank
116600060413  "  C                   move      ' ...+'       D1SporteD
116700060413  "  C                   endif
116800060413  "   *
116900060413  "  C                   endif
117000060413  "   *
117100060412  "  C                   endsl
117200060412  "   *
117300060412  "   * funzione non eseguibile per errore :
117400060412  "  C                   select
117500060412  "   *   1 = funzione richiamata chiusa in errore
117600060412  "  C                   when      Err227 = '1'
117700130416A1152 *   PRO0007-(F1) La funzione richiamata si è chiusa in errore
117800130416C2223x*******            eval      *in58 = *on
117900130416C2223C                   eval      *in93 = *on
118000130416A1152C                   eval      *in99 = *on
118100060412  "  C                   endsl
118200060412  "   *
118300060412A1152C     XRtn227       Endsr
118400080211D2341C************************************************************
118500080211  "  C* Ricerca tab. stato G12 per visualizzazione dati
118600080211  "  C************************************************************
118700080211D2341C     F04_CCB       BEGSR
118800080211     C*
118900080211     C* recupero del codice stato e della nazione da ABI/CAB
119000080211     C                   Clear                   SavStaCAB
119100080211     C                   Clear                   SavNazCAB
119200080211     C                   if        D1Stato     = *Blanks
119300080211     C                   if        D1ABI      <> *Blank  and
119400080211     C                             D1CAB      <> *Blank
119500080211     C                   EXSR      RepStaNaz
119600080211     C                   endif
119700080211     C                   endif
119800080211     C*
119900080211     C                   if        D1Stato    <> *Blanks  or
120000080211     C                             SavStaCAB  <> *Blanks
120100080211     C*
120200080211     C                   EXSR      CercaG12
120300080211     C                   if        $FoundSta   = *On
120400080211     C*
120500080211     C                   RESET                   D002DS
120600080211     C                   MOVEL     '02'          OPZ002
120700080211     C                   MOVE      *ZERO         KEY002
120800080211     C                   MOVE      WStato        KEY002
120900080211     C                   MOVE      XScLin        LIN002
121000080211     C                   MOVE      WWWSoc        AZI002
121100080211     C                   MOVE      *BLANKS       KPJBU
121200080211     C*
121300080211     C                   MOVEL     D002DS        KPJBU
121400080211     C*
121500080211     C                   CALL      'ANGG12R2'
121600080211     C                   PARM                    KPJBA
121700080211     C*
121800080211     C                   MOVEL     KPJBU         D002DS
121900080211     C*
122000080211     C                   endif
122100080211     C*
122200080211     C                   endif
122300080211     C*
122400080211D2341C                   ENDSR
122500080211D2341C/EJECT
122600080211D2341C************************************************************
122700080211  "  C* Cerca tab. stato G12
122800080211  "  C************************************************************
122900080211D2341C     CercaG12      BEGSR
123000080211     C*
123100080211     C                   Eval      $FoundSta   = *Off
123200080211     C*
123300080211     C* se presente utilizzo il codice stato impostato per IBAN
123400080211     C                   if        D1Stato    <> *Blank
123500080211     C*
123600080211     C* Devo ciclare su G12 per recuperare il primo codice tab. G12
123700080211     C* con codice UIC uguale a quello dello stato dell'IBAN
123800080211     C                   EXSR      CicloG12
123900080211     C                   if        $FoundSta   = *On
124000080211     C* utilizzo il codice stato tab. G12 recuperato
124100080211     C                   Move      XTbKey        WStato
124200080211     C                   endif
124300080211     C*
124400080211     C                   else
124500080211     C*
124600080211     C* altrimenti significa che è presente il codice stato di ABI/CAB
124700080211     C                   Eval      WStato      = SavStaCAB
124800080211     C                   Eval      $FoundSta   = *On
124900080211     C*
125000080211     C                   endif
125100080211     C*
125200080211     C*
125300080211     C                   if        $FoundSta   = *On
125400080211     C*
125500080211     C                   EXSR      RepTabG12
125600080211     C                   if        XTBErr      = *On
125700080211     C                   Eval      $FoundSta   = *Off
125800080211     C                   endif
125900080211     C*
126000080211     C                   endif
126100080211     C*
126200080211D2341C                   ENDSR
126300080211D2341C/EJECT
126400950109      ************************************************************
126500950109      * GESTIONE EMISSIONE TASTI FUNZIONE
126600950109      ************************************************************
126700940225     C     GESPIE        BEGSR
126800950109      *
126900940225     C                   SELECT
127000950109      *
127100940506     C     $GEST         WHENEQ    'D1'
127200940506     C     $ULKD1        IFEQ      0
127300950109      * riempimento opzioni
127400940506     C                   EXSR      F24D1
127500940506     C                   ELSE
127600940506     C                   WRITE     FMTZ1
127700940506     C                   ENDIF
127800950109      *
127900950109     C     $GEST         WHENEQ    'D2'
128000940926     C     $ULKD1        IFEQ      0
128100950109      * riempimento opzioni
128200950109      ***                  EXSR F24D2
128300950109      ****                 ELSE
128400950109      ***                  WRITEFMTZ1
128500940926     C                   ENDIF
128600940225     C                   ENDSL
128700950109      *
128800940225     C                   ENDSR
128900940131     C/EJECT
129000950109      ************************************************************
129100950109      * GESTIONE VIDEO RECORD D1
129200950109      ************************************************************
129300940127     C     GESD1         BEGSR
129400950109      * inizializzazione videata
129500941117   B1C     $INZD1        IFEQ      *ON
129600940127     C                   EXSR      INZD1
129700940223     C                   MOVE      *OFF          $INZD1
129800941117   E1C                   ENDIF
129900950109      * emissione piede videata se proveniente da altra
130000950109      * salvataggio valore formato di provenienza
130100941117   B1C     $LASTV        IFNE      'D1'
130200940207     C                   WRITE     FMTT1
130300940225     C                   EXSR      GESPIE
130400940202     C                   MOVE      $LASTV        $LASTG
130500940202     C                   MOVE      'D1'          $LASTV
130600941117   E1C                   ENDIF
130700950109      *
130800950109      * se esistono errori sulla videata
130900030820B0584 * o anche semplici messaggi di avvertimento
131000950109      * emetto la write del formato a indicatori spenti per vedere
131100950109      * le eventuali decodifiche
131200941117   B1C     *IN99         IFEQ      *ON
131300940707     C     $GEST         ANDEQ     'D1'
131400030820B0584C     *IN79         OREQ      *ON
131500030820B0584C     $GEST         ANDEQ     'D1'
131600070328B0953C     *IN61         OREQ      *ON
131700070328B0953C     $GEST         ANDEQ     'D1'
131800940707     C                   MOVEA     *IN           WIN
131900940707     C                   MOVE      *ALL'0'       IN5098           49
132000940707     C                   MOVEA     IN5098        *IN(50)
132100030912B0611C                   select
132200030912B0611c                   when      vis027=' '
132300940707     C                   WRITE     FMTD1
132400030912B0611c                   when      vis027='1'
132500030912  "  C                   WRITE     FMTD2
132600030912  "  c                   when      vis027='2'
132700030912  "  C                   WRITE     FMTD3
132800030912B0611C                   endsl
132900940707     C                   MOVEA     WIN           *IN
133000941117   E1C                   ENDIF
133100950109      *
133200941117   B1C                   SELECT
133300950109      * Annullamento
133400950105    >C     OPZ027        WHENEQ    '04'
133500950109      * Visualizzazione
133600950105    >C     OPZ027        OREQ      '05'
133700030912B0611C                   select
133800030912B0611c                   when      vis027=' '
133900030912     C                   WRITE     FMTD1
134000030912B0611c                   when      vis027='1'
134100030912  "  C                   WRITE     FMTD2
134200030912  "  c                   when      vis027='2'
134300030912  "  C                   WRITE     FMTD3
134400030912B0611C                   endsl
134500940203     C                   EXFMT     PROTECT
134600950109      * Immissione/Modifica/Copia
134700941117   X1C                   OTHER
134800071127c2063c                   eval      Wxmsginz='1'
134900030912B0611C                   select
135000030912B0611c                   when      vis027=' '
135100030912     C                   exfmt     FMTD1
135200030912B0611c                   when      vis027='1'
135300030912  "  C                   EXFMT     FMTD2
135400030912  "  c                   when      vis027='2'
135500030912  "  C                   EXFMT     FMTD3
135600030912B0611C                   endsl
135700941117   E1C                   ENDSL
135800950109      * controllo abilitazione tasti funzione
135900950109      *
136000940506     C                   EXSR      CTRKD1
136100950109      *
136200941117   B1C     *IN99         IFEQ      *OFF
136300950109      *
136400941117   B2C                   SELECT
136500950109      * ROLLUP
136600031002B0622x***  $TASTO        WHENEQ    ROLLUP
136700031002B0622x***                MOVE      'D2'          $GEST
136800950109      * F2=Decodifica
136900940916     C     $TASTO        WHENEQ    F02
137000940916     C                   EXSR      F02D1
137100950109      * F3=Fine
137200940506     C     $TASTO        WHENEQ    F03
137300940309     C                   EXSR      F03D1
137400950109      * F4=Ricerca
137500940506     C     $TASTO        WHENEQ    F04
137600940127     C                   EXSR      SEARCH
137700950109      * F5=Refresh
137800940506     C     $TASTO        WHENEQ    F05
137900940309     C                   EXSR      F05D1
138000130103B1206 * F7=Gestione sportello
138100130103  "  C     $TASTO        WHENEQ    F07
138200130103B1206C                   EXSR      F07D1
138300950109      * F8=Successivo
138400940506     C     $TASTO        WHENEQ    F08
138500940309     C                   EXSR      F08D1
138600950109      * F12=Ritorno
138700940506     C     $TASTO        WHENEQ    F12
138800940309     C                   EXSR      F12D1
138900950109      * F24=Altri tasti
139000940506     C     $TASTO        WHENEQ    F24
139100940315     C                   EXSR      F24D1
139200950109      *
139300941117   X2C                   OTHER
139400940127     C                   EXSR      CTRD1
139500950109      * aggiungere le chiamate ai controlli delle videate supplementari
139600950109    > *          *IN99     IFEQ *OFF
139700950109    > *                    EXSR CTRD2
139800950109    > *          *IN99     IFEQ *ON
139900950109    > *                    SETON                         96
140000950109    > *                    ENDIF
140100950109    > *                    ENDIF
140200950109      *
140300071029A1250c* ricalcolo coordinata IBAN
140400071029  "  c     $Tasto        IFEQ      F17
140500071029  "  C                   exsr      CalcIBAN
140600071029A1250C                   endif
140700071029     c*
140800941117   B3C     $TASTO        IFEQ      F06
140900940207     C     *IN99         ANDEQ     *OFF
141000030820B0584C     $TASTO        OREQ      F11
141100030820B0584C     *IN99         ANDEQ     *OFF
141200071127c2063c* segnalaz. coordinate tra conto e rapporto
141300071127  "  c                   exsr      SegnCoord
141400071127c2063c                   if        Msgrtn<>'N'
141500101102C2169c* segnalaz. se stato IBAN <> da stato società
141600101102  "  c                   exsr      SegnStato
141700101102C2169c                   if        Msgrtn<>'N'
141800950109      * eseguo aggiornamento
141900941214     C                   EXSR      AGGANA
142000950109      * eseguo operazioni del dopo-aggiornamento
142100941117   B4C     *IN99         IFEQ      *OFF
142200940318     C                   EXSR      GESAGG
142300941117   E4C                   ENDIF
142400101102C2169C                   endif
142500071127C2063C                   endif
142600941117   E3C                   ENDIF
142700950109      *
142800941117   E2C                   ENDSL
142900950109      *
143000941117   E1C                   ENDIF
143100950109      *
143200940117     C                   ENDSR
143300940506     C/EJECT
143400071127C2063 ************************************************************
143500071127      * Segnalazione se coordinate differenti
143600071127      ************************************************************
143700071127     C     SegnCoord     BEGSR
143800071129     c* non se cancellazione
143900071129     c                   if        Opz027 <> '04'
144000071127     c*
144100071127     C                   clear                   msgrtn
144200071127     c*
144300071127     c                   if        d1kcc <> *blanks and
144400071127     c                             d1ksc <> *blanks
144500071127     c*
144600071127     c* prima vedo se trattasi di rapporto bancario
144700071127     C                   Clear                   ANBRA
144800071127     C                   EVAL      $LenOut = %Size(ANBRA)
144900071127     C                   CallB     'NDDVABRA'
145000071127     C                   PARM      '01'          Operazione
145100071127     C                   PARM                    XSCSOC
145200071127     C                   PARM                    $Banca
145300071127     C                   PARM                    $Linea
145400071127     C                   PARM      d1kcc         $Kcc
145500071127     C                   PARM      d1ksc         $Ksc
145600071127     C                   PARM      'ANBRA'       $Struttura
145700071127     C                   PARM                    $LenOut
145800071127     C                   PARM      *OFF          $Esito
145900071127     C                   PARM                    ANBRA
146000071127     c                   if        $esito=*off
146100071127     c* reperisco abi
146200071127     c                   move      xscsoc        Bansocieta
146300071127     C                   movel     BraBanca      banbanca
146400071127     C     k02ban01      chain     anban01l                           21
146500071127     C* emetto segnalazione se
146600071127     c* ABI CAB o CCB del conto sono diversi da
146700071127     C* ABI CAB o CCB del rapporto bancario associato
146800071127     c                   if        BanAbi <> D1Abi or
146900071127     c                             BraCab <> D1Cab or
147000071127     c                             BraCcb <> D1Ccb
147100071127     c                   exsr      CallXmsgr
147200071127     C                   endif
147300071127     c*
147400071127     C                   endif
147500071127     c*
147600071127     C                   endif
147700071129     c*
147800071129     C                   endif
147900071127     c                   endsr
148000071127c2063C*----------------------------------------------------                     *
148100071127     C* messaggio di avvertimento di coordinate bancarie differenti             *
148200071127     C* nel conto associato al rapporto bancario
148300071127     C*----------------------------------------------------                     *
148400071127     C     CallXmsgr     BEGSR                                                  *
148500071127     c*
148600071127     c                   move      WxMsgInz      Xmsginz
148700071127     c                   clear                   XmsgDS
148800071127     C                   Z-ADD     0             MSGMSG
148900071127     C                   MOVEL     'S'           MSGRSP
149000071127     C                   MOVEL     'SN'          MSGTPR
149100071127     C                   MOVEL     'S'           MSGEMV
149200071127     C                   Z-ADD     10            MSGRGP
149300071127     C                   MOVEL     'N'           MSGLCK
149400071127     C                   MOVEL     'N'           MSGCNL
149500071127     C                   MOVEL     'N'           MSGVID
149600071127     C                   MOVEL     'N'           MSGOPE
149700071127     C                   MOVEL     'N'           MSGSTP
149800071127     C*
149900071127     C                   CALL      'XMSGR'
150000071127     C                   PARM                    XMSGDS
150100071127     C                   PARM                    XMStxt          135
150200071127     C                   PARM                    XMSGinz           1
150300071127     C                   PARM      'COS2677'     XMSGCd1           7
150400071127     C                   PARM                    XMSGCd2           7
150500071127     C                   PARM                    XMSGCd3           7
150600071127     C                   PARM                    XMSGSn1           7
150700071127     C                   PARM                    XMSGCdTxT        63
150800071127     C                   PARM                    MSGCdTit         14
150900071127     C                   PARM                    MSGClPrtf         1
151000071127     C                   PARM                    VarMsg1         100
151100071127     C                   PARM                    VarMsg2         100
151200071127     C                   PARM                    VarMsg3         100
151300071127     C                   EVAL      Wxmsginz='0'
151400071127     C*
151500071127c2063C                   ENDSR
151600101102C2169 ************************************************************
151700101102C2169 * Segnalazione se stato differente con tipo flusso 1
151800101102C2169 ************************************************************
151900101102C2169C     SegnStato     BEGSR
152000101102     C* non se cancellazione
152100101102     C                   if        Opz027 <> '04'
152200101102     C*
152300101102     C                   clear                   msgrtn
152400101102     C*
152500130416C2223X***                if        *in16 = *on and
152600130416 "   X***                          D1TPHomeB = '1'
152700130416C2223C                   IF        D1TPHomeB = '1'
152800101105     C* Reperisco lo stato
152900101105     C                   Eval      WStato      = XscSta
153000101105     C                   EXSR      RepTabG12
153100101105     C* emetto segnalazione se stato Iban e stato società diversi
153200101105     C                   if        D1Stato <> CodIso2G12
153300101102     C                   exsr      CallXmsgr2
153400101102     C                   endif
153500101102     C*
153600101102     C                   endif
153700101102     C*
153800101102     C                   endif
153900101102     C*
154000101102C2169c                   endsr
154100101102C2169C*----------------------------------------------------                     *
154200101102C2169C* messaggio di avvertimento se stato diverso da                           *
154300101102C2169C* stato società quando il tipo flusso = '1'
154400101102C2169C*----------------------------------------------------                     *
154500101102C2169C     CallXmsgr2    BEGSR                                                  *
154600101102     c*
154700101102     c                   move      WxMsgInz      Xmsginz
154800101102     c                   clear                   XmsgDS
154900101102     C                   Z-ADD     0             MSGMSG
155000101102     C                   MOVEL     'S'           MSGRSP
155100101102     C                   MOVEL     'SN'          MSGTPR
155200101102     C                   MOVEL     'S'           MSGEMV
155300101102     C                   Z-ADD     10            MSGRGP
155400101102     C                   MOVEL     'N'           MSGLCK
155500101102     C                   MOVEL     'N'           MSGCNL
155600101102     C                   MOVEL     'N'           MSGVID
155700101102     C                   MOVEL     'N'           MSGOPE
155800101102     C                   MOVEL     'N'           MSGSTP
155900101102     C*
156000101102     C                   CALL      'XMSGR'
156100101102     C                   PARM                    XMSGDS
156200101102     C                   PARM                    XMStxt          135
156300101102     C                   PARM                    XMSGinz           1
156400101102     C                   PARM      'COS2786'     XMSGCd1           7
156500101102     C                   PARM                    XMSGCd2           7
156600101105     C                   PARM      'COS2491'     XMSGCd3           7
156700101102     C                   PARM                    XMSGSn1           7
156800101102     C                   PARM                    XMSGCdTxT        63
156900101102     C                   PARM                    MSGCdTit         14
157000101102     C                   PARM                    MSGClPrtf         1
157100101102     C                   PARM                    VarMsg1         100
157200101102     C                   PARM                    VarMsg2         100
157300101102     C                   PARM                    VarMsg3         100
157400101102     C                   EVAL      Wxmsginz='0'
157500101102     C*
157600101102C2169C                   ENDSR
157700950109      ************************************************************
157800950109      * CONTROLLO ABILITAZIONE TASTI VIDEATA D1
157900950109      ************************************************************
158000940506     C     CTRKD1        BEGSR
158100950109      *
158200940506     C                   SETOFF                                       99
158300940506     C                   MOVEA     $FC1          $FC
158400940506     C                   EXSR      CTRKEY
158500940506     C     *IN99         IFEQ      *ON
158600940506     C                   SETON                                        97
158700940506     C                   ENDIF
158800950109      *
158900940506     C                   ENDSR
159000941209     C/EJECT
159100950109      ************************************************************
159200950109      * INIZIALIZZAZIONE VIDEATA DATI
159300950109      ************************************************************
159400940127     C     INZD1         BEGSR
159500950109      *
159600940131     C                   CLEAR                   FMTD1
159700030912B0611C                   CLEAR                   FMTD2
159800030912B0611C                   CLEAR                   FMTD3
159900060414A1152 *
160000060414  "   * inizializzo sportello descrittivo
160100060414  "  C                   exsr      ClrSportedDsm
160200060414  "   * flag che serve per sapere se cbd esiste
160300060414A1152C                   MOVE      *OFF          CHNCBD
160400950112      *
160500950112     C     COF027        IFNE      *BLANK
160600950112     C                   SETON                                        18
160700950112     C                   ELSE
160800970228     C                   SETOFF                                       18
160900950112     C                   ENDIF
161000970228     C*
161100101105C2169X**il CIN o CLE è sempre emesso di 2 byte. La lunghezza , come per
161200101105  "  X**il conto corrente è controllata dalla tabella G12
161300101105  "  X**   XscLegisl     IFeq      '1'
161400101105  "  X*Legislazione italiana: codice CIN
161500101105  "  X**                 SEToff                                       15
161600101105  "  X**                 Endif
161700101105  "  x**
161800101105  "  x**   XscLegisl     IFeq      '2'
161900101105  "  x*Legislazione francese: codice CLE
162000101105  "  x**                 SETon                                        15
162100101105C2169x**                 ENDIF
162200130416C2223X**Se nel parametro modulo bonifici è attiva la nuova  ex C2169
162300130416  "  X**modalità di estrazione, evidenzio il tipo flusso.      "
162400130416  "  X**                 If        ModEstr039 = '1'            "
162500130416  "  X**                 Seton                                 "      16
162600130416  "  X**                 Else                                  "
162700130416  "  X**                 Setoff                                "      16
162800130416C2223X**                 Endif                                 "
162900970228     C*
163000940225     C                   Z-ADD     1             $PIED1
163100950109      *
163200950105   B>C     OPZ027        IFNE      '01'
163300990810A0692C******             MOVEL     XSCSOC        CBASOC
163400990810A0692C                   MOVEL     WWWSOC        CBASOC
163500950105    >C                   MOVEL     COF027        CBACOF
163600950105    >C                   MOVEL     KCC027        CBAKCC
163700950105    >C                   MOVEL     KSC027        CBAKSC
163800950109    >C                   MOVEL     ENT027        CBAENT
163900950105    >C                   MOVEL     TAP027        CBATAP
164000950105    >C                   MOVEL     DAL027        CBADAL
164100100922C2169C                   MOVEL     TPH027        CBATPH
164200950109      *
164300941201   B2C     *IN22         DOUEQ     *OFF
164400940318     C     MSGRTN        ORNE      'R'
164500950109      * se Modifica/Annullamento, il rcd che leggo viene bloccato
164600950105   B>C     OPZ027        IFEQ      '02'
164700950105    >C     OPZ027        OREQ      '04'
164800950111    >C     K7CBA1        CHAIN     ANCBA000                           2122
164900950109      * se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
165000941201   X3C                   ELSE
165100950111    >C     K7CBA1        CHAIN(N)  ANCBA000                           2122
165200941201   E3C                   ENDIF
165300950109      *
165400950109      * Gestione RCD allocato
165500941201   B3C     *IN22         IFEQ      *ON
165600950110    >C                   MOVEL     'ANCBA00F'    MSGFIL
165700940318     C                   EXSR      MSGPGM
165800941201   E3C                   ENDIF
165900941201   E2C                   ENDDO
166000950105      *Nel caso abbia letto il record e non vi siano codice abi e cab
166100950105      *accedo al file delle coordinate bancarie descrittive per
166200950105      *reperire i dati
166300060414A1152X**se dopo una modifica in cui chncbd='1', vado in un immissione,
166400060414  "  X**chncbd='1' rimane sporco, quindi pulisco chncbd all'inizio rout
166500060414  "  X**flag che serve per sapere se cbd esiste
166600060414A1152X****************** MOVE      *OFF          CHNCBD
166700950105   B>C     *IN21         IFEQ      *OFF
166800950105   B>C     *IN22         ANDEQ     *OFF
166900950105   B>C     CBAABI        ANDEQ     *BLANK
167000950105   B>C     CBACAB        ANDEQ     *BLANK
167100950105   B2C     *IN22         DOUEQ     *OFF
167200950105     C     MSGRTN        ORNE      'R'
167300060413A1152C                   movel     CBASoc        CBDSoc
167400060413  "  C                   movel     CBACof        CBDCof
167500060413  "  C                   movel     CBAKcc        CBDKcc
167600060413  "  C                   movel     CBAKsc        CBDKsc
167700060413  "  C                   movel     CBAEnt        CBDEnt
167800060413  "  C                   movel     CBATap        CBDTap
167900060413A1152C                   movel     CBADal        CBDDal
168000950109      * se Modifica/Annullamento, il rcd che leggo viene bloccato
168100950105   B>C     OPZ027        IFEQ      '02'
168200950105    >C     OPZ027        OREQ      '04'
168300950105    >C     K7CBD1        CHAIN     ANCBD000                           2122
168400950109     C     *IN21         IFEQ      '0'
168500950109    >C                   MOVE      *ON           CHNCBD            1
168600950109    >C                   ENDIF
168700950109      * se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
168800950105   X3C                   ELSE
168900950105    >C     K7CBD1        CHAIN(N)  ANCBD000                           2122
169000950105   E3C                   ENDIF
169100950109      *
169200950109      * Gestione RCD allocato
169300950105   B3C     *IN22         IFEQ      *ON
169400950110    >C                   MOVEL     'ANCBD00F'    MSGFIL
169500950105     C                   EXSR      MSGPGM
169600950105   E3C                   ENDIF
169700950105   E2C                   ENDDO
169800060414A1152 *
169900060414  "   * imposto ds multipla sportello descrittivo se c'è ancbd
170000060414  "  C                   if        *in21 = *off
170100060414  "  C                   exsr      RieSported
170200060414  "  C                   endif
170300060414A1152 *
170400950105     C                   ENDIF
170500950109      *
170600950109      * se il rcd da gestire non viene trovato
170700950109      * oppure è allocato da altro pgm
170800950109      * ritorno al pgm guida avvertendolo dell'errore
170900941201   B2C     *IN21         IFEQ      *ON
171000940318     C     *IN22         OREQ      *ON
171100940224     C                   MOVE      *ON           $FINE
171200950105    >C                   MOVE      '1'           ERR027
171300950109      *
171400950109      * altrimenti
171500941201   X2C                   ELSE
171600950109      * riempio la videata
171700940224     C                   EXSR      RIED1
171800950109      * Aggiungere le routine di riempimento dei record succcessivi
171900950109    > **                   EXSR RIED2
172000950109      * richiamo routine dei ctrl per decodificarla
172100940614     C                   EXSR      CTRD1
172200950109      * Aggiungere le routine di controllo dei record succcessivi
172300950109    > ****                 EXSR CTRD2
172400940224     C                   MOVE      *ALL'0'       IN5098
172500940224     C                   MOVEA     IN5098        *IN(50)
172600950109      * definizione indicatori su video
172700940224     C                   SETOFF                                       02
172800941201   B3C                   SELECT
172900950105    >C     OPZ027        WHENEQ    '02'
173000940224     C                   SETON                                        02        PR campi key
173100941201   E3C                   ENDSL
173200950109      *
173300941201   E2C                   ENDIF
173400950109      *
173500950102   X1C                   ELSE
173600950109      *
173700950109      * riempio con i default di immissione
173800950109    > *----- immettere i default di immissione
173900950109      *
174000950109      * imposto i campi chiave ricevuti anche se si è in immissione
174100950109      * perchè potrebbero essere stati passati dei default di chiave
174200071123c2063c                   move      Abi027        D1Abi
174300071128  "  c                   move      Cab027        D1Cab
174400071128c2063c                   move      Ccb027        D1Ccb
174500950105    >C                   MOVEL     COF027        D1COF
174600950112    >C                   MOVEL     CFD027        D1COFD
174700950105    >C                   MOVEL     KCC027        D1KCC
174800950105    >C                   MOVEL     KSC027        D1KSC
174900950112    >C                   MOVEL     KSC027        D1COD
175000950105    >C                   MOVEL     ENT027        D1ENT
175100950105    >C                   MOVEL     TAP027        D1TAP
175200100922    >C                   MOVEL     TAP027        D1TAP
175300950112     C     DAL027        IFNE      *BLANK
175400950105     C                   MOVEL     DAL027        XDTAMG                         *
175500961003     C                   CALLb     'XDT4000'
175600950105     C                   PARM                    XDTAMG           10
175700950105     C                   PARM                    XDTGMA            6 0
175800950105     C                   PARM      2             XDTSTA            1 0
175900950105 4   C     XDTSTA        IFEQ      0                                            *
176000950105     C                   Z-ADD     XDTGMA        D1INV
176100950105     C                   ENDIF
176200950112     C                   ELSE
176300000114A0735C*****              Z-ADD     UDATE         D1INV
176400000114A0735C                   Z-ADD     ZDate         D1INV
176500950112     C                   ENDIF
176600100923C2169C                   MOVEL     TPH027        D1TPHomeB
176700950105      *
176800950105      *
176900950102   E1C                   ENDIF
177000950109      *
177100940117     C                   ENDSR
177200940207     C/EJECT
177300950109      ************************************************************
177400950109      * RIEMPIMENTO VIDEATA  D1
177500950109      ************************************************************
177600940207     C     RIED1         BEGSR
177700950109      *
177800950109    > *----------------------------------------------------------
177900950109    > * impostare i campi della videata
178000950109    > *----------------------------------------------------------
178100950105    >C                   MOVEL     CBACOF        D1COF
178200950105    >C                   MOVEL     CBAKCC        D1KCC
178300950109     C     CBACOF        IFNE      *BLANK
178400950109    >C                   MOVEL     CBAKSC        D1COD
178500950109    >C                   ELSE
178600950109    >C                   MOVEL     CBAKSC        D1KSC
178700950109    >C                   ENDIF
178800950105    >C                   MOVEL     CBAENT        D1ENT
178900950105    >C                   MOVEL     CBATAP        D1TAP
179000950105    >C                   MOVEL     CBACCB        D1CCB
179100101111C2169c                   if        BYTE2  = *blanks
179200950105    >C                   MOVEL     CBACCO        D1CCO
179300101111C2169c                   else
179400070322B0945C                   MOVEL     CBACIN        D1CLE
179500101111c2169c                   endif
179600950105    >C                   MOVEL     CBAKCB        D1KCB
179700950105    >C                   MOVEL     CBAKSB        D1KSB
179800950105    >C                   MOVEL     CBAABI        D1ABI
179900950105    >C                   MOVEL     CBACAB        D1CAB
180000950105     C     CBAABI        IFEQ      *BLANK
180100950105     C     CBACAB        ANDEQ     *BLANK
180200060405A1152X****************** MOVEL     CBDIST        D1IST
180300060405  "  X****************** MOVEL     CBDAGE        D1AGE
180400060405A1152X****************** MOVEL     CBDLOC        D1LOC
180500060414A1152C     1             occur     SportedDSM
180600060414  "  C                   movel     DSMSported    D1SporteD
180700060414  "  C                   if        DSMSported <> *blank
180800060414  "  C                   move      ' ...+'       D1SporteD
180900060414A1152C                   endif
181000950105     C                   ENDIF
181100950105     C                   MOVEL     CBADAL        XDTAMG                         *
181200961003     C                   CALLb     'XDT4000'
181300950105     C                   PARM                    XDTAMG           10
181400950105     C                   PARM                    XDTGMA            6 0
181500950105     C                   PARM      2             XDTSTA            1 0
181600950105 4   C     XDTSTA        IFEQ      0                                            *
181700950105     C                   Z-ADD     XDTGMA        D1INV
181800950105     C                   ENDIF
181900100922C2169C                   MOVEL     CBATPH        D1TPHomeB
182000950112      *Dal programma chiamante
182100950112     C                   MOVEL     CFD027        D1COFD                         *
182200001201A0846C                   movel     cbaiban       cbaibands
182300001201 "   c                   eval      d1stato = cbastato
182400001201 "   c                   eval      d1check = Cbacheck
182500001201a0846c                   eval      d1coori = CbaCoorI
182600020422A0930C                   Eval      D1BIC   = CBABic
182700950112      *
182800940207     C                   ENDSR
182900940916     C/EJECT
183000950109      ************************************************************
183100950109      * GESTIONE F02 VIDEO D1
183200950109      ************************************************************
183300940916     C     F02D1         BEGSR
183400950109      *
183500950109      * per decodificare la videata richiamo la routine dei ctrl
183600950109      * e poi disattivo gli eventuali messaggi di errore
183700940916     C                   EXSR      CTRD1
183800950109      *
183900940916     C                   MOVE      *ALL'0'       IN5098
184000940916     C                   MOVEA     IN5098        *IN(50)
184100950109      *
184200940916     C                   ENDSR
184300071029     C/EJECT
184400071029A1250 ************************************************************
184500071029      * Calcolo IBAN (si ricalcola il check (codice di controllo
184600071029      * della coordinata IBAN)a partire dalla coordinata BBAN e la
184700071029      * nazione della coordinata
184800071029A1250 ************************************************************
184900071029     C     CalcIBAN      BEGSR
185000071123     c* spengo tutti gli indicatori degli errori precedentemente
185100071123     c* trovati perchè deve prevalere l'errore di ricalcolo IBAN
185200071123     C                   MOVE      *ALL'0'       IN5098           49
185300071123     C                   MOVEA     IN5098        *IN(50)
185400071123     C*
185500071029     c                   clear                   $stato
185600071029     c                   clear                   $check
185700071029     c                   clear                   $coori
185800071029     c                   clear                   $err
185900071029     c                   clear                   $opz
186000071029     c*
186100071029     c* se c'è lo stato, prendo quello, altrimenti si ricava da
186200071029     c* ANACR01L
186300071029     C                   if        d1Stato  <> *blanks
186400071029     c                   eval      $Stato = D1Stato
186500071029     c                   else
186600071029     c                   eval      acrAbi=D1Abi
186700071029     c                   eval      acrCab=D1Cab
186800071029     c     K02Acr01      chain     Anacr01l
186900071029     c                   if        %found(anacr01L)
187000071113     c* aggancio tabella stato
187100080208D2341X****               move      '1'           XTBRic
187200080208  "  X****               move      XSCSOC        XTBAzi
187300080208  "  X****               move      *Zero         XTBKey
187400080208  "  X****               move      'G12'         XTBCod
187500080208  "  X****               move      AcrStato      XTBKey
187600080208  "  X****               callb     'XATB'
187700080208  "  X****               parm                    XATBDS
187800080208D2341X****               movel     XTBUni        ANGG12DS
187900080208D2341C*
188000080208  "  C                   Eval      WStato      = ACRStato
188100080208  "  C                   EXSR      RepTabG12
188200080208D2341C                   if        XTbErr      = *Off
188300101105C2169x****************** eval      $Stato = NazG12
188400101105  "  c* non bisogna usare il campo nazG12 usato solo per nazioni cee
188500101105  "  c* ma codiso2G12
188600101105C2169C                   eval      $Stato =CodISO2G12
188700080208D2341C                   endif
188800071029     c                   endif
188900071029     c                   endif
189000071127     c* si può calcolare solo c'è lo stato e la coordinata, il check
189100071127     c* viene calcolato
189200080118A1254C                   Eval      D1Coori = D1CCO + D1ABI + D1CAB + WCCB
189300071123     C                   if        $stato =  *blanks  or
189400071123     C                             D1coori=  *blanks
189500071123     C                   Seton                                        6099
189600071123     c                   else
189700071029     C                   Call      'XCHKIBAN'
189800071029     C                   Parm                    $Stato
189900071123     C                   Parm      D1Check       $Check
190000071029     C                   Parm      D1Coori       $Coori
190100071029     C                   Parm      *Off          $Err
190200071029     C                   Parm      '01'          $Opz
190300071029     c* $err='01': codice iban non valido
190400071029     C                   If        $Err='1'
190500071029     C                   Seton                                        8099
190600071029     c                   else
190700071029     c                   eval      d1check=$check
190800080211     c                   eval      d1stato=$stato
190900080118A1254c                   eval      d1cco=%subst($coori:1:1)
191000080118A1254C                   eval      %subst(D1Coori:1:1) = D1Cco
191100080211     C                   EndIf
191200071123     C                   EndIf
191300071029      *
191400071029A1250C                   ENDSR
191500940309     C/EJECT
191600950109      ************************************************************
191700950109      * GESTIONE F03 VIDEO D1
191800950109      ************************************************************
191900940309     C     F03D1         BEGSR
192000950109      *
192100940309     C                   MOVE      *ON           $FINE
192200950105    >C                   MOVE      '1'           RET027
192300950109      * fine programma
192400940325     C                   EXSR      ENDPGM
192500950109      *
192600940325     C                   ENDSR
192700940309     C/EJECT
192800950109      ************************************************************
192900950109      * GESTIONE F05 VIDEO D1
193000950109      ************************************************************
193100940309     C     F05D1         BEGSR
193200950109      *
193300940309     C                   MOVE      *ON           $INZD1
193400950109      *
193500940309     C                   ENDSR
193600940309     C/EJECT
193700130103B1206 ************************************************************
193800130103  "   * GESTIONE F07 VIDEO D1
193900130103  "   ************************************************************
194000130103B1206C     F07D1         BEGSR
194100130103      *
194200130103     C                   EXSR      F04_SporteD
194300130103      *
194400130103B1206C                   ENDSR
194500130103B1206C/EJECT
194600950109      ************************************************************
194700950109      * GESTIONE F08 VIDEO D1
194800950109      ************************************************************
194900940309     C     F08D1         BEGSR
195000950109      *
195100940309     C                   MOVE      *ON           $FINE
195200950105    >C                   MOVE      '0'           RET027
195300950109      *
195400940309     C                   ENDSR
195500940309     C/EJECT
195600950109      ************************************************************
195700950109      * GESTIONE F12 VIDEO D1
195800950109      ************************************************************
195900940309     C     F12D1         BEGSR
196000950109      *
196100940309     C                   MOVE      *ON           $FINE
196200950105    >C                   MOVE      '2'           RET027
196300950109      *
196400940309     C                   ENDSR
196500940309     C/EJECT
196600950109      ************************************************************
196700950109      * GESTIONE F24 VIDEO D1
196800950109      ************************************************************
196900940309     C     F24D1         BEGSR
197000950109      *
197100950109      *
197200940506     C                   MOVEA     $FX1          $FX
197300940506     C                   MOVEA     $FC1          $FC
197400940506     C                   MOVEA     $FR1          $FR
197500940506     C                   Z-ADD     $ULKD1        $ULK
197600940506     C                   EXSR      RIEKEY
197700940506     C                   Z-ADD     $ULK          $ULKD1            3 0
197800940720     C                   MOVEA     $FC           $FC1
197900940506     C                   MOVEL     $KEY1         Z1KE1
198000060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
198100940506     C                   MOVEL     $KEY2         Z1KE2
198200940506     C                   WRITE     FMTZ1
198300950109      *
198400940309     C                   ENDSR
198500940207     C/EJECT
198600950109      ************************************************************
198700950109      * CONTROLLO VIDEATA
198800950109      ************************************************************
198900940127     C     CTRD1         BEGSR
199000071123     C*
199100940127     C                   SETOFF                                       99
199200950109      * non imposto pos. del cursore
199300940127     C                   Z-ADD     0             H1RIGA
199400940127     C                   Z-ADD     0             H1COLO
199500950109      *
199600950109      * CTRL DA NON EFFETTUARE SE ANNULLAMENTO
199700950109      *
199800950105    >C     OPZ027        IFNE      '04'
199900040302B0689c* controlla data inizio validità
200000040302  "  c                   IF        D1INV <> 0
200100040302  "  C     *JobRun       Test(d)                 D1Inv                  21
200200040302  " >C     *In21         ifeq      *On
200300040302  " >C                   SetOn                                        8299
200400040302  "  c                   endif
200500040302B0689c                   endif
200600080205D2341C*
200700080208  "  C* recupero del codice stato e della nazione da ABI/CAB
200800080211  "  C                   Clear                   SavStaCAB
200900080211  "  C                   Clear                   SavNazCAB
201000080204  "  C                   if        D1ABI      <> *Blank  and
201100080204  "  C                             D1CAB      <> *Blank
201200080208  "  C                   EXSR      RepStaNaz
201300080204  "  C                   endif
201400080205D2341C*
201500030916     c*
201600030916B0611c* controllo caratteri c/C
201700080205B0611C* il numero di caratteri deve corrispondere a quello della tabell
201800080205D2341X*ex B0611          IF        d1ccB <> *blanks AND D1Stato <> *blanks
201900080205D2341C                   if        D1CCB      <> *Blanks       and
202000080211  "  C                             (D1Stato    <> *Blanks  or
202100080211D2341C                              SavStaCAB  <> *Blanks     )
202200080205B0611c                   exsr      CtrContoCC
202300030917B0611C                   endif
202400101105     C*
202500101105C2169c* controllo caratteri CIN
202600101105  "  C* il numero di caratteri deve corrispondere a quello della tabell
202700101105  "  c* G12 con lo stato dell'IBAN o lo stato dell'ABI/CAB
202800101105  "  C                   if        D1CIN      <> *Blanks       and
202900101105  "  C                             (D1Stato    <> *Blanks  or
203000101105  "  C                              SavStaCAB  <> *Blanks     )
203100101105  "  c                   exsr      CtrCarCIN
203200101105C2169C                   endif
203300030916     C*
203400071123A1250c* se non presente sportello descrittivo
203500071123A1250c                   IF        D1SporteD = *blanks
203600050428B0794C* se vuoti CIN/ABI/CAB/CCB e IBAN tutto inserito
203700050428  "  c* se stato =IT
203800050428  "  c* si possono ricavare cin/abi/cab/ccb da IBAN
203900050428  "  c                   if        D1Stato='IT'
204000050428  "  c                             and D1Check <> *blanks
204100050428  "  c                             and D1Coori <> *blanks
204200050428  "  c                             and D1Cco=*blanks
204300050428  "  c                             and D1ABI=*blanks
204400050428  "  c                             and D1Cab=*blanks
204500050428  "  c                             and D1Ccb=*blanks
204600050428  "  c* cin
204700050428  "  c                   eval      d1cco=%subst(d1coori1:1:1)
204800050428  "  c                   eval      d1abi=%subst(d1coori1:2:3) +
204900050428  "  c                                   %subst(d1coori2:1:2)
205000050428  "  c                   eval      d1cab=%subst(d1coori2:3:2) +
205100050428  "  c                                   %subst(d1coori3:1:3)
205200050428  "  c                   eval      d1cCb=%subst(d1coori3:4:1) +
205300050428  "  c                                   %subst(d1coori4:1:4) +
205400050428  "  c                                   %subst(d1coori5:1:4) +
205500050428  "  c                                   %subst(d1coori6:1:3)
205600050428B0794c                   endif
205700071123A1250c                   endif
205800950109      *
205900950905      * CI DEVONO ESSERE ALMENO
206000950905      * L'ABI/CAB O I DATI DESCRITTIVI O LA BANCA
206100060405A1152X**** D1IST         IFEQ      *BLANK
206200060405  "  X**** D1AGE         ANDEQ     *BLANK
206300060405A1152X**** D1LOC         ANDEQ     *BLANK
206400060405A1152C     D1SporteD     IFEQ      *BLANK
206500950111    >C     D1ABI         ANDEQ     *BLANK
206600950111    >C     D1CAB         ANDEQ     *BLANK
206700950905    >C     D1KCB         ANDEQ     *BLANK
206800950905    >C     D1KSB         ANDEQ     *BLANK
206900060405A1152 *  PRO0196 Inserire ABI/CAB oppure dati descrittivi oppure
207000060405A1152 *          banca corrispondente.
207100950111    >C                   SETON                                        5099
207200950111    >C                   ENDIF
207300030820B0584X**** EX A0846 ** Codice IBAN: o tutti o nessuno
207400020506B0437X**** ex A0846      if        D1check  < '00' and
207500020506  "  X**** ex A0846                (D1Stato <> *blanks or
207600020506  "  X**** ex A0846                D1CoorI <> *blanks)
207700020506  "  X**** ex A0846      seton                                        7899
207800020506B0437X**** ex A0846      endif
207900030820B0584X**** ex A0846      if        (D1Stato =  *blanks and
208000030820  "  X**** ex A0846                D1Check =  *blanks and
208100030820  "  X**** ex A0846                D1CoorI =  *blanks) or
208200030820  "  X**** ex A0846                D1Stato <> *blanks and
208300030820  "  X**** ex A0846                D1Check <> *blanks and
208400030820  "  X**** ex A0846                D1CoorI <> *blanks
208500030820  "  X**** ex A0846      else
208600030820  "  X**** ex A0846
208700030820  "  X**** ex A0846      seton                                        7799
208800030820  "  X**** ex A0846      endif
208900030820B0584X**** ex A0846
209000030820B0584 *
209100030820  "   * Codice IBAN: se inseriti i dati completi di CIN/ABI/CAB/CCB,
209200030820  "   *              compongo automaticamente il BBAN:
209300030820  "   *              - se non inserito, lo propongo a video
209400030820  "   *              - se già inserito, lo confronto con quello a video
209500030820  "   *                ed in caso di differenza emetto un avvertimento
209600030820  "  C                   Eval      WCCB = %Subst(D1CCB: 1: 12)
209700030820  "  C                   Eval      WCCB2 = %Subst(D1CCB: 13: 6)
209800030820  "  C                   If        D1CCO <> *Blanks And
209900030820  "  C                             D1ABI <> *Blanks And
210000030820  "  C                             D1CAB <> *Blanks And
210100070417B0584C                             WCCB <> *Blanks And WCCB2 = *Blanks
210200070412B0953c                             and opz027 <> '05'
210300070417B0584C                   Eval      WCoori = D1CCO + D1ABI + D1CAB + WCCB
210400030820  "  C                   If        D1Coori = *Blanks
210500070328B0584C                   Eval      D1Coori = WCoori
210600070328B0953c                   move      *on           IbanAutom
210700070328B0584C                   Else
210800030820  "  C                   If        WCoori <> D1Coori
210900070328B0584C                   SetOn                                        79
211000070328B0953c                   move      *off          IbanAutom         1
211100070328B0584C                   EndIf
211200030820  "  C                   EndIf
211300030820B0584C                   EndIf
211400020506B0437 ** Codice IBAN: il check deve essere numerico
211500020506  "  C     D1Check       IfNe      *Blanks
211600020506  "  C     Digits        Check     D1Check                                21
211700020506  "  C                   If        *In21 = *On
211800020506  "  C                   Seton                                        7899
211900020506  "  C                   EndIf
212000020506B0437C                   EndIf
212100030820B0584 ** Codice IBAN: se inserito stato o check, allora deve essere
212200030820  "   **              valorizzato o tutto o niente (mentre la terza
212300030820  "   **              parte, il BBAN, si può inserire da sola)
212400030820  "  C                   If        D1Stato <> *Blanks Or
212500030820  "  C                             D1Check <> *Blanks
212600030820  "  C                   If        (D1Stato = *Blanks Or D1Check =
212700030820  "  C                                *Blanks Or D1Coori = *Blanks) AND
212800030820  "  C                             (D1Stato <> *Blanks Or D1Check <>
212900030820  "  C                                *Blanks Or D1Coori <> *Blanks)
213000030820  "  C                   Seton                                        7799
213100030820  "  C                   EndIf
213200030820B0584C                   EndIf
213300030820B0584 ** Codice IBAN: se inserito tutto ne controllo la validità
213400030820  "  C                   If        D1Stato <> *Blanks And
213500030820  "  C                             D1Check <> *Blanks And
213600030820  "  C                             D1Coori <> *Blanks
213700030820  "  C                   Call      'XCHKIBAN'
213800030820  "  C                   Parm                    D1Stato
213900030820  "  C                   Parm                    D1Check
214000030820  "  C                   Parm                    D1Coori
214100030820  "  C                   Parm      *Off          XIbanErr          1
214200140217  "  C**                 If        XIbanErr <> *Off And
214300140217  "  C**                           $Tasto <> F11
214400140217  "  C                   If        XIbanErr <> *Off
214500030820  "  C                   Seton                                        8099
214600030820  "  C                   EndIf
214700030820B0584C                   EndIf
214800050428     c*
214900950112      **
215000950111    >C     D1KCB         IFEQ      *BLANK
215100950111    >C     D1KSB         ANDNE     *BLANK
215200950111    >C                   SETON                                        5299
215300950111    >C                   ENDIF
215400950112      **
215500950111    >C     D1KSB         IFEQ      *BLANK
215600950111    >C     D1KCB         ANDNE     *BLANK
215700950111    >C                   SETON                                        5399
215800950111    >C                   ENDIF
215900950112      **
216000950111    >C     D1ABI         IFEQ      *BLANK
216100950111    >C     D1CAB         ANDNE     *BLANK
216200950111    >C                   SETON                                        5599
216300950111    >C                   ENDIF
216400950112      **
216500950111    >C     D1CAB         IFEQ      *BLANK
216600950111    >C     D1ABI         ANDNE     *BLANK
216700950111    >C                   SETON                                        5699
216800060405     C                   ENDIF
216900060405     C*
217000060405A1152X**** D1IST         IFNE      *BLANK
217100060405  "  X**** D1LOC         ANDEQ     *BLANK
217200060405  "  X**** D1AGE         ORNE      *BLANK
217300060405  "  X**** D1LOC         ANDEQ     *BLANK
217400060405  "  X****               SETON                                        5899
217500060405  "  X****               ENDIF
217600060405  "  X****
217700060405  "  X**** D1LOC         IFNE      *BLANK
217800060405  "  X**** D1IST         ANDEQ     *BLANK
217900060405  "  X**** D1AGE         ORNE      *BLANK
218000060405  "  X**** D1IST         ANDEQ     *BLANK
218100060405  "  X****               SETON                                        6199
218200060405  "  X****               ENDIF
218300060405  "  X****
218400060405  "  X**** D1IST         IFNE      *BLANK
218500060405  "  X**** D1AGE         ANDEQ     *BLANK
218600060405  "  X**** D1LOC         ORNE      *BLANK
218700060405  "  X**** D1AGE         ANDEQ     *BLANK
218800060405  "  X****               SETON                                        6099
218900060405A1152X****               ENDIF
219000950905     C*
219100950905      * controllo i campi immessi per CLI/FOR
219200950905      * oppure KCC/KSC
219300950905     C                   EXSR      CTR2D1
219400950112      **
219500950112     C                   MOVEL     *BLANK        D1DAGE
219600950116     C                   MOVEL     *BLANK        D1DIST
219700950112     C                   MOVEL     *BLANK        D1DLOC
219800950112     C                   MOVEL     *BLANK        D1DCOM
219900980424    >C     D1ABI         IfNe      *Blank
220000980424    >C     D1CAB         AndNe     *Blank
220100980424    >C                   Move      D1ABI         X59ABI
220200980424    >C                   Move      D1CAB         X59CAB
220300990810A0692C********           Move      XscSoc        X59SOC
220400990810A0692C                   Move      WWWSoc        X59SOC
220500980424    >C                   Call      'X59ABI'
220600980424    >C                   Parm                    X59ABIDS
220700980424     C*
220800980424    >C                   If        X59Err = '1'
220900980424    >C                   Seton                                        5799
221000980424    >C                   Else
221100980424     C                   Movel     X59DesIst     D1DIST
221200980424     C                   Movel     X59DesAge     D1DAGE
221300980424     C                   Movel     X59Localit    D1DLOC
221400101105C2169C                   MOVE      X59Stato      D1Dloc
221500980424    >C                   EndIf
221600970228     C
221700101105C2169c*  questo controllo non va bene, non ha senso controllare
221800101105  "  c*  la legislazione della società di collegamento per
221900101105  "  c*  effettuare un controllo
222000101105  "  c*  sostituisco il test sulla legislazione francese con il test
222100101108  "  c*  sullo stato ISO della tabella G12 dello stato IBAN o ABI/CAB
222200101105  "  X*>>>>>>>Solo se legislazione francese<<<<<<<
222300101105  "  X*******************IF        XscLegisl = '2'
222400101105c2169C                   IF        CodIso2G12= 'FR'
222500970228     C*Controllo Abi/Cab/Conto per codice CLE
222600970228     C                   If        D1Ccb<>*blank
222700970312     C*Controllo/reperisco clerib
222800970228     C                   Exsr      CtrCle
222900970402     C                   Select
223000970402     C                   When      X31Err = '0'
223100970402     C*Controllo ok o non era stato passato:
223200970402     C*   restituisco Cle calcolato e conto allineato correttamente su 11
223300970402     C*   caratteri.
223400970402     C                   Eval      D1Cle  = x31Clo
223500970402     C                   Eval      D1Ccb  = x31Ccb
223600970402     C
223700970402     C                   When      X31Err = '6'
223800970312     C*AVVISO incongruenza Cle calcolato/Cle digitato
223900970228     C                   Seton                                        73
224000970402     C
224100970402     C                   When      X31Err = '2'
224200970402     C*Abi passato non corretto
224300970402     C                   Seton                                        7299
224400970402     C
224500970402     C                   When      X31Err = '3'
224600970402     C*Cab passato non corretto
224700970402     C                   Seton                                        7499
224800970402     C
224900970402     C                   When      X31Err = '4'
225000970402     C*Ccb passato non corretto
225100970402     C                   Seton                                        7599
225200970402     C
225300970402     C                   When      X31Err = '5'
225400970402     C*Cle passato non corretto
225500970402     C                   Seton                                        7699
225600970402     C
225700970402     C                   EndSl
225800970402     C
225900970228     C                   ENDIF
226000970228     C                   ENDIF
226100970228     C*
226200950111    >C                   ENDIF
226300950112      **
226400970228     C
226500950905     C                   MOVEL     *BLANK        D1KSBD
226600950111    >C     D1KSB         IFNE      *BLANK
226700950111    >C     D1KCB         ANDNE     *BLANK
226800950905     C*
226900961003     C                   CALLb     'XALLINEA'
227000950905     C                   PARM                    D1KCB
227100950905     C                   PARM      6             LEN               5 0
227200950905     C                   PARM                    XSCINP
227300950905     C*
227400961003     C                   CALLb     'XALLINEA'
227500950905     C                   PARM                    D1KSB
227600950905     C                   PARM      8             LEN               5 0
227700950905     C                   PARM                    XSCINP
227800950905     C*
227900950905    >C                   MOVE      D1KCB         RCOKCC
228000950905    >C                   MOVE      D1KSB         RCOKSC
228100950123    >C                   MOVE      WWWSOC        RCOSOC
228200950905    >C     K3RCO1        CHAIN     ANRCO01L                           21
228300950905    >C     *IN21         IFEQ      *ON
228400950905    >C     RCOSNA        ORNE      '2'
228500950111    >C                   SETON                                        5499
228600950905    >C                   ELSE
228700950905    >C                   MOVE      RCOKCC        PDCKCC
228800950905    >C                   MOVE      RCOKSC        PDCKSC
228900950905    >C                   MOVE      RCOSOC        PDCSOC
229000950905    >C     K3PDC1        CHAIN     ANPDC01L                           21
229100950905     C  N21              MOVEL     PDCDES        D1KSBD
229200950111    >C                   ENDIF
229300950111    >C                   ENDIF
229400950112      **
229500950111    >C     D1ABI         IFNE      *BLANK
229600950111    >C     D1CAB         ORNE      *BLANK
229700060405A1152X**** D1LOC         IFNE      *BLANK
229800060405  "  X**** D1IST         ORNE      *BLANK
229900060405A1152X**** D1AGE         ORNE      *BLANK
230000060411A1152C                   if        D1SporteD <> *blank
230100060405  "   *  PRO0195 Immettere i codici ABI e CAB oppure la descrizione
230200060405A1152 *          istituto, agenzia e località.
230300950111    >C                   SETON                                        6299
230400950111    >C                   ENDIF
230500950111    >C                   ENDIF
230600950112      **
230700950111    >C                   MOVEL     *BLANK        D1DTAP
230800950110    >C     D1TAP         IFNE      *BLANK
230900950110    >C                   MOVE      '1'           XTBRIC
231000950110    >C                   MOVE      XSCLIN        XTBLIN
231100950123    >C                   MOVE      WWWSOC        XTBAZI
231200950110    >C                   MOVE      *ZEROS        XTBKEY
231300950110    >C                   MOVE      D1TAP         XTBKEY
231400950110    >C                   MOVE      'G25'         XTBCOD
231500950110    >C                   EXSR      LETTAB
231600950110    >C     WTBERR        IFEQ      *OFF
231700950110    >C                   MOVEL     XTBUNI        D1DTAP
231800950911      * CTRL ESISTENZA DEL TIPO APPOGGIO STD
231900950911     C                   MOVEL     WWWSOC        CBASOC
232000950911     C                   MOVEL     D1COF         CBACOF
232100950911     C                   MOVEL     D1KCC         CBAKCC
232200950911     C     D1COF         IFEQ      *BLANK
232300950911     C                   MOVEL     D1KSC         CBAKSC
232400950911     C                   ELSE
232500950911     C                   MOVEL     D1COD         CBAKSC
232600950911     C                   ENDIF
232700950911     C                   MOVEL     D1ENT         CBAENT
232800950911     C                   MOVEL     *BLANK        CBATAP
232900961008     C**** K6CBA1        SETLL     ANCBA000                               21
233000961008     C                   Call      'ANA027R1'
233100961008     C                   Parm                    CbaSoc
233200961008     C                   Parm                    CbaCof
233300961008     C                   Parm                    CbaKcc
233400961008     C                   Parm                    CbaKsc
233500961008     C                   Parm                    CbaEnt
233600961008     C                   Parm                    CbaTap
233700961008     C                   Parm                    FlgRtn            1
233800010129B0337C                   Parm      '1'           TipCtr            1
233900961008     C**** *IN21         IFEQ      *OFF
234000961008     C     FlgRtn        IFEQ      *OFF
234100950911     C                   SETON                                        7199
234200950911     C                   ENDIF
234300950911     C*
234400950110    >C                   ELSE
234500950110    >C                   SETON                                        5199
234600950110    >C                   ENDIF
234700950110    >C                   ENDIF
234800100922C2169 * Tipologia flusso
234900130416C2223X*****              IF        *in16
235000130416C2169C                   MOVE      *BLANK        D1TpHomeBd
235100130417C2223 **Se bonifici per tipo flusso il campo è obbligatorio
235200130417 "    **per cui lo controllo
235300130417 "   C                   IF        D1TpHomeB <> *blank
235400130417C2223C                             or ModEstr039 = '1'
235500130416C2169C                   RESET                   X07DS
235600100922  "  C                   MOVE      '1'           X07RIC
235700100922  "  C                   MOVEL     'ANDIZ000'    X07TRC
235800100922  "  C                   MOVEL     'DSPTPHOMEB'  X07CAM
235900100922  "  C                   MOVEL     D1TpHomeB     X07VAL
236000100922  "  C                   MOVE      *OFF          X07ALL
236100100922  "  C                   CALLb     'X07VALR'
236200100922  "  C                   PARM                    X07DS
236300100922  "  C     X07ERR        IFEQ      '0'
236400100922  "  C                   MOVEL     X07DES        D1TpHomeBD
236500100922  "  C                   ELSE
236600100922  "  C                   SETON                                        8399
236700100923  "  C                   ENDIF                                                  *
236800130416C2169C                   ENDIF                                                  *
236900130416C2223X****               IF        *in16
237000130416C2169C                   IF        D1TpHomeB = '4' and
237100100923  "  C                             D1Sported = *blanks
237200100923  "  C                   Seton                                        8599
237300100923  "  C                   Endif
237400100923  "  C                   IF        (D1TpHomeB = '1' or
237500100923  "  C                             D1TpHomeB = '2') and
237600100923  "  C                             D1STATO = *blanks
237700100923  "  C                   Seton                                        8799
237800100923  "  C                   Endif
237900101102  "  C                   IF        D1TpHomeB = '3' and
238000101105  "  C                             D1Bic = *blanks
238100130416  "  C                   Seton                                        88
238200130416C2169C                   Endif
238300130416C2223X***                Endif
238400130416C2223C                   IF        D1Bic <> *blank
238500130416  "  C                             and D1STATO <> *blanks
238600130416  "  C                             and D1STATO <> %Subst(D1Bic: 5: 2)
238700130416  "  C                   Seton                                        5899
238800130416C2223C                   Endif
238900950109      *
239000950109      * CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
239100950112      *
239200950105     C     OPZ027        IFEQ      '01'
239300950105     C     OPZ027        OREQ      '03'
239400950109      *-------------------------------------------------------------
239500950109      * impostare i campi chiave e verificare l'esistenza del record
239600950109      *-------------------------------------------------------------
239700950123     C                   MOVEL     WWWSOC        CBASOC
239800950105     C                   MOVEL     D1COF         CBACOF
239900950113     C                   MOVEL     D1KCC         CBAKCC
240000950113     C     D1COF         IFEQ      *BLANK
240100950105     C                   MOVEL     D1KSC         CBAKSC
240200950113     C                   ELSE
240300950113     C                   MOVEL     D1COD         CBAKSC
240400950113     C                   ENDIF
240500950105     C                   MOVEL     D1ENT         CBAENT
240600950105     C                   MOVEL     D1TAP         CBATAP
240700950112      **
240800950105     C                   MOVEL     D1INV         XDTGMA                         *
240900961003     C                   CALLb     'XDT4000'
241000950105     C                   PARM                    XDTAMG           10
241100950105     C                   PARM                    XDTGMA            6 0
241200040302B0689x*****              PARM      4             XDTSTA            1 0
241300040302B0689c                   PARM      3             XDTSTA            1 0
241400950105 4   C     XDTSTA        IFEQ      0                                            *
241500950109     C                   MOVEL     XDTAMG        CBADAL
241600040305B0689c                   else
241700040305B0689c                   MOVE      '0001-01-01'  CBADAL
241800950105     C                   ENDIF
241900950105     C     K7CBA1        SETLL     ANCBA000                               21
242000940224     C     *IN21         IFEQ      '1'
242100060411A1152 * PRO0006 (F1) Codice già esistente per opzione di immissione o c
242200940223     C                   SETON                                        5999
242300950105     C                   ELSE
242400950105     C     D1ABI         IFEQ      *BLANK
242500950105     C     D1CAB         ANDEQ     *BLANK
242600950105     C     K7CBA1        SETLL     ANCBD000                               21
242700950105     C     *IN21         IFEQ      '1'
242800060405A1152 * PRO0006 (F1) Codice già esistente per opzione di immissione o c
242900950105     C                   SETON                                        5999
243000940223     C                   ENDIF
243100950105     C                   ENDIF
243200950105     C                   ENDIF
243300940224     C                   ENDIF
243400950112      **
243500941027     C                   ENDIF
243600950112      **
243700070328B0953C*
243800070328  "  C* se si è in modifica e la coordinata IBAN è solo
243900070328  "  C* un'informazione reperita (cioè, non è memorizzata sul file
244000100924B0953C* delle coordinate)lo segnalo con un avvertimento
244100100924C2169c                   if        *in99=*off
244200100924B0953C                   if        OPZ027      = '02'     and
244300070328  "  C                             IbanAutom   = *On      and
244400070328  "  c                             d1coori <> *blanks
244500070328  "  C                   seton                                        61
244600070328  "  C                   endif
244700071123B0953C*
244800100924c2169C                   endif
244900940117     C                   ENDSR
245000970228     C/EJECT
245100080204D2341C************************************************************
245200080208  "  C* Reperimento codice stato e nazione da ABI e CAB
245300080204  "  C************************************************************
245400080208D2341C     RepStaNaz     BEGSR
245500080204     C*
245600080204     C                   Reset                   X59AbiDs
245700080204     C*
245800080204     C                   Move      D1ABI         X59ABI
245900080204     C                   Move      D1CAB         X59CAB
246000080204     C                   Move      WWWSoc        X59SOC
246100080204     C*
246200080204     C                   CALL      'X59ABI'
246300080204     C                   PARM                    X59ABIDS
246400080204     C*
246500080204     C                   if        X59Err      = *On
246600080204     C*
246700080204     C* Codice ABI / CAB errato
246800080204     C                   Seton                                        5799
246900080204     C*
247000080204     C                   else
247100080207     C*
247200080211     C                   Eval      SavStaCAB   = X59Stato
247300080207     C*
247400080208     C* reperisco la decodifica UIC dello Stato
247500080208     C                   Eval      WStato      = X59Stato
247600080204     C                   EXSR      RepTabG12
247700080208     C*
247800080208     C                   if        XTBErr      = *On
247900080208     C* Codice Stato impostato per ABI/CAB inesistente in tab. G12
248000080208     C                   Seton                                        8499
248100080208     C*
248200080208     C                   else
248300080208     C*
248400101105c2169x****************   Eval      SavNazCAB   = NazG12
248500101105  "  c                   eval      SavNazCAB = Codiso2G12
248600101105  "  c* non bisogna usare il campo nazG12 usato solo per nazioni cee
248700101105c2169c* ma codiso2G12
248800080208     C*
248900080211     C                   if        SavNazCAB   = *Blank
249000080208     C* Nazione CEE non impostata in tab. G12 per il codice Stato
249100080208     C* presente in ABI/CAB: non è errore perchè non è obbligatoria
249200080208     C*
249300080208     C                   else
249400080208     C*
249500080211     C                   if        SavNazCAB  <> D1Stato   and
249600080208     C                             D1Stato    <> *Blank
249700080208     C*
249800080208     C* AVVERTIMENTO
249900080208     C* Codice Stato (YABStato) diverso dalla nazione CEE impostata
250000080208     C* per ABI/CAB
250100080208     C                   if        $Tasto     <> F11
250200080208     C                   Seton                                        8699
250300080208     C                   endif
250400080208     C*
250500080208     C                   else
250600080208     C*               OK
250700080208     C                   endif
250800080208     C                   endif
250900080208     C*
251000080208     C                   endif
251100080204     C*
251200080204     C                   endif
251300080204     C*
251400080204D2341C                   ENDSR
251500080204D2341C/EJECT
251600080204D2341C******************************************************
251700080208  "  C* Reperimento dati della tabella G12 - stati
251800080204  "  C******************************************************
251900080204D2341C     RepTabG12     BEGSR
252000080204     C*
252100080204     C                   Clear                   ANGG12DS
252200080204     C*
252300080204     C                   Clear                   XATBDS
252400080204     C                   Move      XSCCUT        XTBUTE
252500080204     C                   Move      '1'           XTBRIC
252600080204     C                   Move      WWWSoc        XTBAZI
252700080204     C                   MoVe      *ZERO         XTBKEY
252800080204     C                   Move      'G12'         XTBCOD
252900080208     C                   Move      WStato        XTBKEY
253000080204     C*
253100080208     C                   CALLB     'XATB'
253200080204     C                   PARM                    XATBDS
253300080208     C*
253400080208     C                   if        XTBErr      = *Off
253500080208     C*
253600080208     C* se non inserita lunghezza, metto 0 altrimenti si spacca
253700080208     C                   if        %SubSt(XTbUni:41:2) = *Blanks
253800080208     C                   Eval      %SubSt(XTbUni:41:2) = *Zeros
253900080208     C                   endif
254000080208     C*
254100080208     C                   MoveL     XTBUNI        ANGG12DS
254200080208     C*
254300101215c2169c* se stato italia, imposto CODISO2G12 con 'IT' per non fare
254400101215  "  c* modificare a tutti i clienti la tabella g12
254500101215  "  c                   if        %subst(Wstato:1:2) = 'IT'
254600101215  "  C                   eval      CodIso2G12='IT'
254700101215C2169C                   endif
254800080208     C                   endif
254900080204     C*
255000080204D2341C                   ENDSR
255100080204D2341 /EJECT
255200030917B0611C************************************************************
255300030917    >C* CONTROLLO codice cc
255400030917B0611C************************************************************
255500030917    >C     CTRContoCC    BEGSR
255600030917     c*
255700080208D2341X**** aggancio tabella stato
255800080208  "  X****               move      '1'           XTBRic
255900080208  "  X****               move      XSCSOC        XTBAzi
256000080208  "  X****               move      *Zero         XTBKey
256100080208  "  X****               move      'G12'         XTBCod
256200080208  "  X****               movel     d1Stato       wrkstato          4
256300080208  "  X****               move      wrkstato      XTBKey
256400080208  "  X****               callb     'XATB'
256500080208  "  X****               parm                    XATBDS
256600080208  "  X**** se trovato
256700080208D2341X****               if        XtbErr = '0'
256800040326B0699x***                movel     XTBUni        ANGG12DS
256900080208D2341X****
257000080208  "  X**** se non inserita lunghezza, metto 0 altrimenti si spacca
257100080208  "  X****               if        %subst(XTBUNI:41:2) = *blanks
257200080208  "  X****               eval      %subst(xtbuni:41:2) = *zeros
257300080208  "  X****               endif
257400080208  "  X****
257500080208D2341X*ex B0699          movel     XTBUni        ANGG12DS
257600080208D2341C*
257700080211  "  C                   EXSR      CercaG12
257800080208D2341C                   IF        $FoundSta   = *On
257900030917     c*
258000030917     c* Se presente la lunghezza in tabella
258100030917B2   c                   if        LungCcG12<>0
258200030917     c* la controllo a seconda dell'allineamento
258300030917s1   C                   Select
258400030917     c* A SINISTRA
258500030917w1   c                   When      alliCCg12 = *blanks or
258600030917     c                             alliCCg12 = '0'
258700030917     c* se la lunghezza supera tabella
258800030917b3   c                   if        lungccg12 <> 18
258900030917b3   c                   if        %subst(d1ccb:lungccg12+1:18-lungccg12)<> ' '
259000030917     c                   seton                                        8199
259100030917e3   c                   endif
259200030917e3   c                   endif
259300030917     c* se esiste un blank nella lunghezza stabilita, errore
259400030917b3   c                   DO        lungccg12     $lun              3 0
259500030917b4   c                   if        %subst(d1ccb:$lun:1)=*blanks
259600030917     c* è errore
259700030917     c                   seton                                        8199
259800030917     c                   leave
259900030917e4   C                   endif
260000030917e3   c                   enddo
260100030917     c*
260200030917     c* A DESTRA CON 0 o A DESTRA CON BLANKS
260300030917w2   c                   When      alliCCg12 = '1' OR
260400030917     c                             alliCCg12 = '2'
260500030917     c* se almeno uno da destra per la lunghezza stabilita è blank,allineo
260600030917     c* perchè significa che è stato appena inserito e deve essere alli
260700030917     c* neato
260800030917     c                   z-add     19            $lun
260900030917b3   c                   DO        lungccg12
261000030917     c                   sub       1             $lun
261100030917b4   c                   if        %subst(d1ccb:$lun:1)=*blanks
261200030917     C                   CALLb     'XALLINEA'
261300030917     C                   PARM                    D1CCB
261400030917     C                   PARM      18            LEN               5 0
261500030917     C                   PARM                    ALLICCG12
261600030917     c                   leave
261700030917e4   C                   endif
261800030917e3   c                   enddo
261900030917     c*
262000030917     c* se c'è ancora blank nella lunghezza stabilita a partire dal
262100030917     c* fondo a destra, errore
262200030917     c                   z-add     19            $lun
262300030917b3   c                   DO        lungccg12
262400030917     c                   sub       1             $lun
262500030917b4   c                   if        %subst(d1ccb:$lun:1)=*blanks
262600030917     c                   seton                                        8199
262700030917     c                   leave
262800030917e4   c                   endif
262900030917e3   c                   enddo
263000030917     c*
263100030917     c* poi pulisco i campi da sinistra a seconda della lunghezza
263200030917     C* MA SOLO SE SONO ZERI DERIVATI DALL'ALLINEAMENTO
263300030917     C                   eval      $lun = 18-lungCcG12
263400030917b3   c                   if        $lun<> 0
263500030917b4   c                   DO        $lun          $lun1             3 0
263600030917b5   c                   if        %subst(d1ccb:$lun1:1)='0'
263700030917     c                   eval      %subst(d1ccb:$lun1:1)=*blanks
263800030917e5   c                   endif
263900030917e4   c                   enddo
264000030917e3   c                   endif
264100030917     c*
264200030917     c* dopo la pulizia, se non ci sono dei blank da sinistra per la
264300030917     c* lunghezza rimanente, errore
264400030917     C                   eval      $lun = 18-lungCcG12
264500030917b3   c                   if        $lun<> 0
264600030917b4   c                   DO        $lun          $lun1             3 0
264700030917b5   c                   if        %subst(d1ccb:$lun1:1)<> *blanks
264800030917     c                   seton                                        8199
264900030917     c                   leave
265000030917e5   c                   endif
265100030917e4   c                   enddo
265200030917e3   c                   endif
265300030917     c*
265400030917s    c                   endsl
265500030917     C*
265600030917e2   C                   endif
265700030917     C*
265800030917e1   C                   endif
265900030917     C*
266000030917B0611C                   endsr
266100080207D2341C************************************************************
266200080207  "  C* Ciclo tabella G12 per recupero codice stato con codifica
266300080207  "  C* UIC indicata in D1STATO
266400080207  "  C************************************************************
266500080207D2341C     CicloG12      BEGSR
266600080207     C*
266700080207     C                   Clear                   ANGG12DS
266800080207     C*
266900080207     C                   Clear                   XATBDS
267000080207     C* posizionamento
267100080207     C                   Move      '9'           XTBRic
267200080207     C                   Move      WWWSOC        XTBAzi
267300080207     C*
267400080207     C* forzo blank perchè altrimenti non si posiziona bene
267500080207     C                   Move      *Blank        XTBKey
267600080207     C                   Move      'G12'         XTBCod
267700080207     C                   CALLB     'XATB'
267800080207     C                   PARM                    XATBDS
267900080207     C*
268000080207     C* se posizionamento ok
268100080207     C                   if        XTbErr      = '0'
268200080207     C* lettura
268300080207     C                   Move      '3'           XTBRic
268400080207     C                   CALLB     'XATB'
268500080207     C                   PARM                    XATBDS
268600080207     C                   Eval      $FineG12    = XTbErr
268700080207     C*
268800080207     C                   DoW       $FineG12    = *Off
268900080207     C*
269000080207     C* se non inserita lunghezza, metto 0 altrimenti si spacca
269100080211     C                   if        %SubSt(XTbUni:41:2) = *Blanks
269200080211     C                   Eval      %SubSt(XTbUni:41:2) = *Zeros
269300080207     C                   endif
269400080207     C*
269500080211     C                   MoveL     XTbUni        ANGG12DS
269600080207     C*
269700101105C2169X**************     if        NazG12      = D1Stato
269800101105  "  c* non bisogna usare il campo nazG12 usato solo per nazioni cee
269900101105  "  c* ma codiso2G12
270000101105C2169C                   if        CodIso2G12  = D1Stato
270100080207     C* trovato: esco dal ciclo
270200080208     C                   Eval      $FoundSta   = *On
270300080207     C                   Eval      $FineG12    = *On
270400080207     C                   else
270500080207     C* lettura
270600080207     C                   Move      '3'           XTBRic
270700080207     C                   CALLB     'XATB'
270800080207     C                   PARM                    XATBDS
270900080207     C                   parm                    XATBDS
271000080207     C                   Eval      $FineG12    = XTbErr
271100080207     C                   endif
271200080207     C*
271300080207     C                   endDo
271400080207     C*
271500080207     C                   endif
271600080207     C*
271700080207D2341C                   ENDSR
271800080207D2341 /EJECT
271900101105C2169C************************************************************
272000101105    >C* CONTROLLO lunghezza codice CIN
272100101105C2169C************************************************************
272200101105    >C     CTRCarCIN     BEGSR
272300101105     c*
272400101105     c* cerco la tabella che corrisponde alla nazione cee dello stato
272500101105     c* dell'IBAN se presente o allo stato delle coordinate bancarie
272600101105     c*
272700101105     C                   EXSR      CercaG12
272800101105     C                   IF        $FoundSta   = *On
272900101105     c*
273000101111     c* Può anche essere non presente la lunghezza PER ALCUNI STATI
273100101111     c*
273200101105     C* se lunghezza 1 deve essere messo allineato a sinistra
273300101105     c                   if        LungCinG12 =1
273400101111     c* O se stato iso ITALIA,  deve essere lungo 1
273500101111     C                             OR CodIso2G12= 'IT'
273600101105     c                   if        %subst(D1Cin:1:1) = *blanks
273700101105     c                   seton                                        8999
273800101105     c                   endif
273900101105     c                   if        %subst(D1Cin:2:1)<> *blanks
274000101105     c                   seton                                        8999
274100101105     c                   endif
274200101105     c                   endif
274300101105     C*
274400101105     C* lunghezza 2
274500101105     c                   if        LungCinG12 =2
274600101111     c                   if        %subst(D1Cin:1:1) = *blanks
274700101111     c                   seton                                        8999
274800101111     c                   endif
274900101111     c                   if        %subst(D1Cin:2:1) = *blanks
275000101111     c                   seton                                        8999
275100101111     c                   endif
275200101105     c                   endif
275300101111     c*
275400101105     c                   endif
275500101105     c*
275600101105c2169C                   endsr
275700970228    >C************************************************************
275800970228    >C* CONTROLLO codice CLE
275900970228    >C************************************************************
276000970228    >C     CTRCle        BEGSR
276100970228     C*
276200970228     C                   Reset                   X31CleRiDs
276300970228     C                   Eval      X31Abi = D1Abi
276400970228     C                   Eval      X31Cab = D1Cab
276500970228     C                   Eval      X31Ccb = D1Ccb
276600970228     C                   Eval      X31Cli = D1Cle
276700970228     C*
276800970228     C                   Call      'X31CLERIB'
276900970228     C                   Parm                    X31CleRiDs
277000970228     C
277100970228     C                   ENDsr
277200970228     C
277300950112     C/EJECT
277400950112    > ************************************************************
277500950112    > * CONTROLLO SU CAMPI CLI/FOR OPPURE KCC/KSC
277600950112    > ************************************************************
277700950112    >C     CTR2D1        BEGSR
277800950905     C*
277900950905     C                   MOVEL     *BLANK        D1COFD
278000950905     C*
278100950112     C                   SELECT
278200950905      * selezione per CLI/FOR
278300950112     C     *IN18         WHENEQ    *ON
278400950112      *Selezione per CLI/FOR oppure KCC/KSC
278500950112     C     D1COF         IFEQ      *BLANK
278600950112     C                   SETON                                          6499
278700950112     C                   ELSE
278800950112     C     D1COF         IFNE      'C'
278900950113     C     D1COF         ANDNE     'F'
279000950112     C                   SETON                                          6399
279100950112     C                   ENDIF
279200950112     C                   ENDIF
279300950112      *
279400950112     C     D1COD         IFEQ      *BLANK
279500950112     C                   SETON                                          6799
279600950112     C                   ELSE
279700950112     C     D1COF         IFEQ      'C'
279800950112     C     D1COF         OREQ      'F'
279900950112     C                   SELECT
280000950112     C     D1COF         WHENEQ    'C'
280100950112     C                   RESET                   DVCLI
280200950112     C                   MOVE      '1'           DCLRIC
280300950123     C                   MOVE      WWWSOC        DCLSOC
280400950112     C                   MOVE      XSCDUN        DCLUNI
280500950112     C                   MOVE      '1'           DCLVIS
280600950112     C                   MOVE      *OFF          DCLALC
280700950112     C                   MOVE      1             DCLNRK
280800950112     C                   CLEAR                   DCLDTA
280900950112     C                   MOVE      D1COD         DCLCLI
281000950112     C                   MOVE      'DVOCLIDS'    DCLSET
281100950112     C                   MOVE      *OFF          DCLERR
281200950112     C                   CALL      'DVCLI'
281300950112     C                   PARM                    DVCLI
281400950112     C                   PARM                    DVOCLI
281500950112      *
281600950112     C                   MOVEL     *BLANK        D1COFD
281700950112     C     DCLERR        IFEQ      *OFF
281800950112     C                   MOVEL     COGDES        D1COFD
281900950112     C                   ELSE
282000950112     C                   SETON                                        9965
282100950112     C                   ENDIF
282200950112      *
282300950112     C     D1COF         WHENEQ    'F'
282400950112     C                   MOVE      '1'           DFRRIC
282500950123     C                   MOVE      WWWSOC        DFRSOC
282600950112     C                   MOVE      XSCDUN        DFRUNI
282700950112     C                   MOVE      '1'           DFRVIS
282800950112     C                   MOVE      *OFF          DFRALC
282900950112     C                   MOVE      1             DFRNRK
283000950112     C                   CLEAR                   DFRDTA
283100950112AAA  C                   MOVE      D1COD         DFRFOR
283200950112     C                   MOVE      'DVOFRNDS'    DFRSET
283300950112     C                   MOVE      *OFF          DFRERR
283400950112     C                   CALL      'DVFOR'
283500950112     C                   PARM                    DVFOR
283600950112     C                   PARM                    DVOFRN
283700950112      *
283800950112     C                   MOVEL     *BLANK        D1COFD
283900950112     C     DFRERR        IFEQ      *OFF
284000950112     C                   MOVEL     FOGDES        D1COFD
284100950112     C                   ELSE
284200950112     C                   SETON                                        9966
284300950112     C                   ENDIF
284400950112     C                   ENDSL
284500950112     C                   ENDIF
284600950112     C                   ENDIF
284700950112      *
284800950905      * selezione per KCC/KSC
284900950905     C     *IN18         WHENEQ    *OFF
285000950112     C     D1KCC         IFEQ      *BLANK
285100950112     C                   SETON                                        9968
285200950112     C                   ENDIF
285300950112      *
285400950112     C     D1KSC         IFEQ      *BLANK
285500950112     C                   SETON                                        9969
285600950112     C                   ENDIF
285700950112      *
285800950112     C     D1KSC         IFNE      *BLANK
285900950112     C     D1KCC         ANDNE     *BLANK
286000950905     C*
286100961003     C                   CALLb     'XALLINEA'
286200950905     C                   PARM                    D1KCC
286300950905     C                   PARM      6             LEN               5 0
286400950905     C                   PARM                    XSCINP
286500950905     C*
286600961003     C                   CALLb     'XALLINEA'
286700950905     C                   PARM                    D1KSC
286800950905     C                   PARM      8             LEN               5 0
286900950905     C                   PARM                    XSCINP
287000950905      *
287100950112    >C                   MOVE      D1KCC         RCOKCC
287200950112    >C                   MOVE      D1KSC         RCOKSC
287300950123    >C                   MOVE      WWWSOC        RCOSOC
287400060427B0866X**** K3RCO1        SETLL     ANRCO01L                               21
287500060427B0866X**** *IN21         IFNE      *ON
287600060427B0866C     K3RCO1        CHAIN     ANRCO01L                           21
287700060427  "   * non può essere cli/for
287800060418  "  C                   if        *in21 = *on
287900060418  "  C                             or RCOSna = 'C'
288000060418  "  C                             or RCOSna = 'F'
288100060427B0866 * PRO0194-Conto errato o non valido
288200950112     C                   SETON                                          7099
288300950905    >C                   ELSE
288400950905    >C                   MOVE      RCOKCC        PDCKCC
288500950905    >C                   MOVE      RCOKSC        PDCKSC
288600950905    >C                   MOVE      RCOSOC        PDCSOC
288700950905    >C     K3PDC1        CHAIN     ANPDC01L                           21
288800950905     C  N21              MOVEL     PDCDES        D1COFD
288900950112     C                   ENDIF
289000950112     C                   ENDIF
289100950112      *
289200950112     C                   ENDSL
289300950112     C                   ENDSR
289400941215     C/EJECT
289500950109    > ************************************************************
289600950109    > * ACCESSO A TABELLE ANTAB00F
289700950109    > ************************************************************
289800950105    >C     LETTAB        BEGSR
289900950109    > *
290000950109    > * Input :
290100950109    > * - TABGRU/TABCOD = chiave univoca ANTAB00F
290200950109    > * Output :
290300950109    > * - WTBERR = flag segnalazione errore reperimento tabella
290400950109    > *
290500961003    >C                   CALLb     'XATB'
290600950105    >C                   PARM                    XATBDS
290700950109    > *
290800950105    >C                   MOVE      *OFF          WTBERR
290900950105    >C     XTBERR        IFEQ      *ON
291000950105    >C                   MOVE      *ON           WTBERR
291100950105    >C                   ENDIF
291200950109    > *
291300950105    >C                   ENDSR
291400940224     C/EJECT
291500950109      ************************************************************
291600950109    > * AGGIORNAMENTO ANAGRAFICA
291700950109      ************************************************************
291800941214     C     AGGANA        BEGSR
291900950109      *
292000940207     C                   SELECT
292100060413A1152 *
292200060413A1152 * INSERIMENTO/COPIA--------------------------------
292300950105    >C     OPZ027        WHENEQ    '01'
292400950105    >C     OPZ027        OREQ      '03'
292500060413A1152 *
292600060412A1152 * ANCBA ---
292700950105     C                   EXSR      RIECBA
292800950105    >C                   WRITE     ANCBA000                             22
292900060413A1152 *
293000060414  "   *     fallita scrittura ANCBA
293100060413  "  C                   if        *in22 = *on
293200060414  "   *       PRO1687-Fallita scrittura file
293300060413  "  C                   move      'PRO1687'     $IdMsg
293400060413  "  C                   move      'ANCBA00F'    $File
293500060413  "  C                   exsr      WinErr
293600060413  "  C                   eval      *in99 = *on
293700060414  "  C                   endif
293800060414  "   * ANCBD ---
293900060414  "  C                   exsr      AggAncbd
294000060414A1152 *
294100060414A1152X**** CBACAB ****** IFEQ      *BLANK
294200060414  "  X**** CBAABI ****** ANDEQ     *BLANK
294300060414  "  X**** *IN22 ******* ANDEQ     *OFF
294400060413  "  X****************** EXSR      RIECBD
294500060414  "  X****************** WRITE     ANCBD000                             22
294600060414  "  X****************** ENDIF
294700060414  "  X* se rcd non scrivibile attivo errore
294800060413  "  X**** *IN22 ******  IFEQ      *ON
294900060413  "  X****************** SETON                                        9599
295000060413A1152X****************** ENDIF
295100060414A1152 *
295200060412A1152 * MODIFICA-----------------------------------------
295300950105    >C     OPZ027        WHENEQ    '02'
295400060413A1152 *
295500060412A1152 * ANCBA ---
295600950105     C                   EXSR      RIECBA
295700060413A1152X****************** UPDATE    ANCBA000
295800060413A1152C                   UPDATE    ANCBA000                             25
295900060413  "   *
296000060414  "   *     dallito aggiornamento ANCBA
296100060413  "  C                   if        *in25 = *on
296200060414  "   *       PRO1278-(F1) L'operazione di aggiornamento è fallita
296300060413  "  C                   move      'PRO1278'     $IdMsg
296400060413  "  C                   move      'ANCBA00F'    $File
296500060413  "  C                   exsr      WinErr
296600060413  "  C                   eval      *in99 = *on
296700060414  "  C                   endif
296800060413  "   * ANCBD ---
296900060413  "  C                   exsr      AggAncbd
297000060413  "   *
297100060413A1152 *
297200060414A1152X**** CBACAB ****** IFEQ      *BLANK
297300060414  "  X**** CBAABI ****** ANDEQ     *BLANK
297400060412  "  X****************** EXSR      RIECBD
297500060414  "  X**** CHNCBD ****** IFEQ      *ON
297600060412  "  X****************** UPDATE    ANCBD000
297700060412  "  X****************** ELSE
297800060412  "  X****************** WRITE     ANCBD000
297900060412  "  X****************** ENDIF
298000060412  "  X****************** ELSE
298100060412  "  X*Se il record ANCBA00F era collegato ad un record di ANCBD00F
298200060412  "  X*che ora non serve piu' lo cancello
298300060414  "  X**** CHNCBD ****** IFEQ      *ON
298400060412  "  X****************** DELETE    ANCBD000
298500060412  "  X****************** ENDIF
298600060412A1152X****************** ENDIF
298700060413A1152 *
298800060412A1152 * CANCELLAZIONE------------------------------------
298900950105    >C     OPZ027        WHENEQ    '04'
299000060413A1152 *
299100060412A1152 * ANCBA ---
299200060413A1152X****************** DELETE    ANCBA000
299300060413A1152C                   DELETE    ANCBA000                             25
299400060414  "   *
299500060414  "   *     fallita cancellazione ANCBA
299600060413  "  C                   if        *in25 = *on
299700060414  "   *       PRO1342-Cancellazione fallita
299800060413  "  C                   move      'PRO1342'     $IdMsg
299900060413  "  C                   move      'ANCBA00F'    $File
300000060413  "  C                   exsr      WinErr
300100060413  "  C                   eval      *in99 = *on
300200060414  "  C                   endif
300300060413  "   * ANCBD ---
300400060413  "  C                   exsr      AggAncbd
300500060413A1152 *
300600060414A1152X**** CHNCBD ****** IFEQ      *ON
300700060412  "  X****************** DELETE    ANCBD000
300800060412A1152X****************** ENDIF
300900950109     C                   ENDSL
301000980714     C*
301100980714     C* richiama pgm User Exit
301200980714     C*
301300980714     C     USRERR        IFEQ      *BLANK
301400980714     C* imposta chiave univoca  per user exit
301500060413A0692X*****              MOVE      XSCSOC        XUKSOC
301600990810A0692C                   MOVE      WWWSOC        XUKSOC
301700980714     C                   MOVE      XSCDUN        XUKDUN
301800980714     C                   MOVEL     DSPGM         XUKPGM
301900980714     C                   MOVEL     CBACOF        XUKFL1
302000980714     C                   MOVEL     CBAKCC        XUKFL2
302100980714     C                   MOVEL     CBAKSC        XUKFL3
302200980714     C                   MOVEL     CBAENT        XUKFL4
302300980714     C                   MOVEL     CBATAP        XUKFL5
302400980714     C                   MOVEL     CBADAL        XUKFL6
302500980714     C                   MOVEL     XUKDS         KPJBU
302600980714     C     DSPGM         CAT       'U':0         USREXT           10
302700980714     C                   CALL      USREXT                               21
302800980714     C                   PARM                    KPJBA
302900980714     C     *IN21         IFEQ      *ON
303000980714     C                   MOVE      *ON           USRERR            1
303100980714     C                   ENDIF
303200980714     C                   ENDIF
303300980714     C*
303400950109      *
303500940203     C                   ENDSR
303600940224     C/EJECT
303700060412A1152 ************************************************************
303800060412  "   * Aggiornamento Ancbd00f
303900060412  "   ************************************************************
304000060412  "  C     AggAncbd      Begsr
304100060413  "   *
304200060413  "   * --------------------------------------------------------------
304300060413  "   * DISALLOCO ancbd:
304400060413  "   *   in Modifica/Cancellazione ho già letto ancbd allocandolo ma
304500060413  "   *   ora lo devo rileggere per aggiornarlo
304600060413  "  C                   if        Opz027='02'
304700060413  "  C                             or Opz027='04'
304800060413  "  C                   unlock    Ancbd01l
304900060413  "  C                   endif
305000060413  "   *
305100060413  "   * --------------------------------------------------------------
305200060414  "   * DELETE Ancbd (con kiave iniziale):
305300060414  "   *  Se non ci sono errori aggiornamento ancbA
305400060414  "   *  e esisteva già Ancbd (ChnCbd=*on se c'è ancbd in mod/canc)
305500060414  "  C                   if        *in99 = *off
305600060414  "  C                             and ChnCbd = *on
305700060413  "  C                   exsr      DelAncbd
305800060413  "  C                   endif
305900060413  "   *
306000060414  "   * ------------------------------------------------------------
306100060414  "   * WRITE  Ancbd
306200060414  "   *  Se non ci sono errori aggiornamento ancbA o cancellaz. ancbD
306300060414  "   *  e non siamo in cancellazione
306400060414  "   *  e Abi/Cab vuoti (abi/cab in alternativa a sportello descr.)
306500060414  "  C                   if        *in99 = *off
306600060414  "  C                             and Opz027 <> '04'
306700060414  "  C                             and CBAAbi = *blank
306800060414  "  C                             and CBACab = *blank
306900060412  "  C                   exsr      WrtAncbd
307000060413  "  C                   endif
307100060413  "   *
307200060413  "  C     XAggAncbd     Endsr
307300060413  "   ************************************************************
307400060413  "   * Cancellazione Ancbd00f
307500060413  "   ************************************************************
307600060413  "  C     DelAncbd      Begsr
307700060413  "   *
307800060413  "   * cancello ancbd con la kiave iniziale di caricamento ana027d
307900060413  "   *
308000060413  "   *  leggo ancbd01l
308100060413  "  C                   movel     CBASoc        CBDSoc
308200060413  "  C                   movel     Cof027        CBDCof
308300060413  "  C                   movel     Kcc027        CBDKcc
308400060413  "  C                   movel     Ksc027        CBDKsc
308500060413  "  C                   movel     Ent027        CBDEnt
308600060413  "  C                   movel     Tap027        CBDTap
308700060414  "   * in immissione la data arriva vuota
308800060414  "  C                   if        Dal027 <> *blank
308900060414  "  C                   move      Dal027        $DataIso
309000060414  "  C                   else
309100060414  "  C     *jobrun       move      D1Inv         $DataIso
309200060414  "  C                   endif
309300060414  "  C                   move      $DataIso      CBDDal
309400060413  "  C                   z-add     1             CBDNrRiga
309500060413  "  C     K8CBD1        setll     ANCBD01L
309600060413  "  C                   exsr      ReadAncbd
309700060413  "   *    rec allocato
309800060413  "  C                   if        *in24 = *on
309900060413  "   *    PRO1342-Cancellazione fallita
310000060413  "  C                   move      'PRO1342'     $IdMsg
310100060413  "  C                   move      'ANCBD00F'    $File
310200060413  "  C                   exsr      WinErr
310300060413  "  C                   eval      *in99 = *on
310400060413  "  C                   endif
310500060413  "   *
310600060413  "   *  fino a quando ancbd01l c'è e non è allocato
310700060413  "  C                   dow       *in24=*off
310800060413  "  C                             and *in23=*off
310900060413  "  C                             and *in99=*off
311000060413  "   *
311100060413  "   *   cancellazione
311200060413  "  C                   delete    ANCBD000                             25
311300060413  "   *
311400060413  "   *    cancellazione fallita
311500060413  "  C                   if        *in25 = *on
311600060413  "   *    PRO1342-Cancellazione fallita
311700060413  "  C                   move      'PRO1342'     $IdMsg
311800060413  "  C                   move      'ANCBD00F'    $File
311900060413  "  C                   exsr      WinErr
312000060413  "  C                   eval      *in99 = *on
312100060413  "  C                   endif
312200060413  "   *
312300060413  "   *  leggo ancbd01l
312400060413  "  C                   exsr      ReadAncbd
312500060413  "   *    rec allocato
312600060413  "  C                   if        *in24 = *on
312700060413  "   *    PRO1342-Cancellazione fallita
312800060413  "  C                   move      'PRO1342'     $IdMsg
312900060413  "  C                   move      'ANCBD00F'    $File
313000060413  "  C                   exsr      WinErr
313100060413  "  C                   eval      *in99 = *on
313200060413  "  C                   endif
313300060413  "   *
313400060413  "  C                   enddo
313500060413  "   *
313600060413  "  C     XDelAncbd     Endsr
313700060413  "   ************************************************************
313800060413  "   * Inserimento   Ancbd00f
313900060413  "   ************************************************************
314000060413  "  C     WrtAncbd      Begsr
314100060413  "   *
314200060414  "   *  valorizzo key sportello descrittivo
314300060413  "  C                   exsr      RieCBD
314400060414  "   *
314500060414  "   *  valorizzo sportello descrittivo...
314600060414  "   *
314700060414  "   *  ...vuoto se ds multipla sportello descr. non contiene dati
314800060414  "  C     1             occur     SportedDSM
314900060414  "  C                   if        DSMSporteD = *blank
315000060414  "   *
315100060414  "  C                   z-add     1             CBDNrRiga
315200060414  "  C                   movel     *blank        CBDSporteD
315300060414  "  C                   WRITE     ANCBD000                             25
315400060414  "   *
315500060414  "   *     fallita scrittura ancbd
315600060414  "  C                   if        *in25 = *on
315700060414  "   *     PRO1687-Fallita scrittura file
315800060414  "  C                   move      'PRO1687'     $IdMsg
315900060414  "  C                   move      'ANCBD00F'    $File
316000060414  "  C                   exsr      WinErr
316100060414  "  C                   eval      *in99 = *on
316200060414  "  C                   endif
316300060414  "   *
316400060414  "   *  ...da ds multipla sportello descr., se contiene dati
316500060414  "  C                   else
316600060414  "   *
316700060414  "  C     1             do        $DimDsm       $Dsm
316800060414  "  C     $Dsm          occur     SportedDSM
316900060414  "  C                   if        DSMSporteD <> *blank
317000060414  "   *
317100060413  "  C                   z-add     DSMNrRiga     CBDNrRiga
317200060413  "  C                   movel     DSMSporteD    CBDSporteD
317300060413  "  C                   WRITE     ANCBD000                             25
317400060413  "   *
317500060414  "   *     esco se fallita scrittura ancbd
317600060413  "  C                   if        *in25 = *on
317700060414  "   *       PRO1687-Fallita scrittura file
317800060413  "  C                   move      'PRO1687'     $IdMsg
317900060413  "  C                   move      'ANCBD00F'    $File
318000060413  "  C                   exsr      WinErr
318100060413  "  C                   eval      *in99 = *on
318200060414  "   *
318300060413  "  C                   leave
318400060413  "  C                   endif
318500060413  "   *
318600060414  "   *    esco se ds multipla sportello descr. non contiene più dati
318700060413  "  C                   else
318800060413  "  C                   leave
318900060413  "  C                   endif
319000060413  "   *
319100060413  "  C                   enddo
319200060413  "   *
319300060413  "  C                   endif
319400060413  "   *
319500060413  "  C     XWrtAncbd     Endsr
319600060413  "   ************************************************************
319700060413  "   * Window errore
319800060413  "   ************************************************************
319900060413  "  C     WinErr        Begsr
320000060413  "   *
320100060413  "  C                   clear                   XMsgDS
320200060413  "   *
320300060413  "   *   reperisco msg:
320400060413  "  C                   clear                   XMsgTxt
320500060413  "  C                   eval      IdMsg = 'PROMSG    *LIBL     xxxxxxx'
320600060413  "   *   PRO1687-Fallita scrittura file
320700060413  "   *   PRO1278-(F1) L'operazione di aggiornamento è fallita
320800060413  "   *   PRO1342-Cancellazione fallita
320900060413  "  C                   move      $IdMsg        IdMsg
321000060413  "  C                   callb     'XRTVM'
321100060413  "  C                   parm                    IdMsg            27
321200060413  "  C                   parm                    TxtMsg          100
321300060413  "  C                   parm                    ErrMsg            1
321400060413  "  C                   if        ErrMsg <> *on
321500060413  "  C                   movel     TxtMsg        MsgMs1
321600060413  "  C                   else
321700060413  "  C                   movel     *All'?'       MsgMs1
321800060413  "  C                   endif
321900060413  "   *   accodo il nome file la cui scrittura è fallita
322000060413  "  C                   cat       '(':1         MsgMs1
322100060413  "  C                   cat       $File:0       MsgMs1
322200060413  "  C                   cat       ')':1         MsgMs1
322300060413  "   *
322400060413  "   *   window d'avevrtimento bloccante
322500060413  "   *     nr messaggio precodificato
322600060413  "  C                   movel     0             MsgMsg
322700060413  "   *     esistenza risposta
322800060413  "  C                   movel     'N'           MsgRsp
322900060413  "   *     emissione a video
323000060413  "  C                   movel     'S'           MsgEmv
323100060413  "   *     riga da cui parte la finestra
323200060413  "  C                   z-add     10            MsgRgp
323300060413  "   *     gestione analisi del problema (F2)
323400060413  "  C                   movel     'N'           MsgLck
323500060413  "   *     non utilizzato
323600060413  "  C                   movel     'N'           MsgCnl
323700060413  "   *     mandare o meno alla coda del display/utente
323800060413  "  C                   movel     'N'           MsgVid
323900060413  "   *     mandare o meno all'operatore
324000060413  "  C                   movel     'N'           MsgOpe
324100060413  "   *     emissione in stampa
324200060413  "  C                   movel     'N'           MsgStp
324300060413  "   *
324400060413  "  C                   call      'XMSGR'
324500060413  "  C                   parm                    XMsgDS
324600060413  "  C                   parm                    XMsgTxt         135
324700060413  "  C                   parm                    XMsgInz           1
324800060413  "   *
324900060413A1152C     XWinErr       Endsr
325000950109      ************************************************************
325100950109      * RIEMPIMENTO FILE IN AGGIORNAMENTO
325200950109      ************************************************************
325300950105     C     RIECBA        BEGSR
325400950109      *
325500950105     C                   CLEAR                   ANCBA000
325600950109    > *-------------------------------------------------------------
325700950109    > * impostare i campi del file
325800950109    > *-------------------------------------------------------------
325900950123    >C                   MOVEL     WWWSOC        CBASOC
326000950105    >C                   MOVEL     D1COF         CBACOF
326100950105    >C                   MOVEL     D1KCC         CBAKCC
326200020506B0437X**** D1KSC         IFEQ      *BLANK
326300020506  "  X****               MOVEL     D1COD         CBAKSC
326400020506  "  X****               ELSE
326500020506  "  X****               MOVEL     D1KSC         CBAKSC
326600020506B0437X****               ENDIF
326700020506B0437C     D1COF         IFEQ      *BLANK
326800020506  "  C                   MOVEL     D1KSC         CBAKSC
326900020506  "  C                   ELSE
327000020506  "  C                   MOVEL     D1COD         CBAKSC
327100020506B0437C                   ENDIF
327200950105    >C                   MOVEL     D1ENT         CBAENT
327300950105    >C                   MOVEL     D1TAP         CBATAP
327400950105     C                   MOVEL     D1INV         XDTGMA                         *
327500961003     C                   CALLb     'XDT4000'
327600950105     C                   PARM                    XDTAMG           10
327700950105     C                   PARM                    XDTGMA            6 0
327800040302B0689x*****              PARM      4             XDTSTA            1 0
327900040302B0689c                   PARM      3             XDTSTA            1 0
328000950105 4   C     XDTSTA        IFEQ      0                                            *
328100950109     C                   MOVEL     XDTAMG        CBADAL
328200040302B0689c                   else
328300040302B0689c                   move      '0001-01-01'  Cbadal
328400950105     C                   ENDIF
328500950105     C                   MOVEL     D1CAB         CBACAB
328600950105     C                   MOVEL     D1ABI         CBAABI
328700950105     C                   MOVEL     D1CCB         CBACCB
328800101111C2169c                   if        BYTE2 = *blanks
328900950105     C                   MOVEL     D1CCO         CBACCO
329000101111C2169c                   else
329100070322B0945C                   MOVEL     D1CLE         CBACIN
329200101111c2169c                   endif
329300950105     C                   MOVEL     D1KCB         CBAKCB
329400950905     C                   MOVEL     D1KSB         CBAKSB
329500951106     C                   MOVEL     XSCCUT        CBACTM
329600000114A0735C*****              MOVE      UDATE         XDTGMA
329700000114A0735C                   MOVE      ZDate         XDTGMA
329800961003     C                   CALLb     'XDT4000'
329900950109     C                   PARM                    XDTAMG           10
330000950109     C                   PARM                    XDTGMA            6 0
330100950109     C                   PARM      4             XDTSTA            1 0
330200950109 4   C     XDTSTA        IFEQ      0                                            *
330300950109     C                   MOVEL     XDTAMG        CBADFM
330400950109     C                   ENDIF
330500001201a0846 *
330600001201 "   c                   eval      CBAStato = D1Stato
330700001201 "   c                   eval      CBAcheck = D1check
330800020422A0846c                   eval      CBACoorI = D1CoorI
330900020422A0930C                   Eval      CBABic   = D1Bic
331000001201A0846c                   EVAL      CbAIban = CBAIbanDS
331100100922C2169C*
331200100927C2169c                   Movel     D1TPHomeB     CBATpH
331300950109      *
331400940207     C                   ENDSR
331500950105     C/EJECT
331600950109      ************************************************************
331700950109      * RIEMPIMENTO FILE IN AGGIORNAMENTO
331800950109      ************************************************************
331900950109     C     RIECBD        BEGSR
332000950109      *
332100950109     C                   CLEAR                   ANCBD000
332200950109    > *-------------------------------------------------------------
332300950109    > * impostare i campi del file
332400950109    > *-------------------------------------------------------------
332500950123    >C                   MOVEL     WWWSOC        CBDSOC
332600950109    >C                   MOVEL     D1COF         CBDCOF
332700950109    >C                   MOVEL     D1KCC         CBDKCC
332800020506B0437X**** D1KSC         IFEQ      *BLANK
332900020506  "  X****               MOVEL     D1COD         CBDKSC
333000020506  "  X****               ELSE
333100020506  "  X****               MOVEL     D1KSC         CBDKSC
333200020506B0437X****               ENDIF
333300020506B0437C     D1COF         IFEQ      *BLANK
333400020506  "  C                   MOVEL     D1KSC         CBDKSC
333500020506  "  C                   ELSE
333600020506  "  C                   MOVEL     D1COD         CBDKSC
333700020506B0437C                   ENDIF
333800950109    >C                   MOVEL     D1ENT         CBDENT
333900950109    >C                   MOVEL     D1TAP         CBDTAP
334000950109     C                   MOVEL     D1INV         XDTGMA                         *
334100961003     C                   CALLb     'XDT4000'
334200950109     C                   PARM                    XDTAMG           10
334300950109     C                   PARM                    XDTGMA            6 0
334400040302B0689x*****              PARM      4             XDTSTA            1 0
334500040302B0689c                   PARM      3             XDTSTA            1 0
334600950109 4   C     XDTSTA        IFEQ      0                                            *
334700950109     C                   MOVEL     XDTAMG        CBDDAL
334800040326B0699c                   else
334900040326B0699c                   MOVE      '0001-01-01'  CBDDAL
335000950109     C                   ENDIF
335100060405A1152X****************** MOVEL     D1LOC         CBDLOC
335200060405A1152X****************** MOVEL     D1IST         CBDIST
335300060405A1152X****************** MOVEL     D1AGE         CBDAGE
335400950109      *
335500950109     C                   ENDSR
335600940224     C/EJECT
335700950109      ************************************************************
335800950109      * GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
335900950109      ************************************************************
336000940224     C     GESAGG        BEGSR
336100950109      *
336200950109      * eseguo COMMIT se non è richiesto che lo esegua il pgm chiamante
336300950109    > **+        CMT027    IFEQ '0'
336400950109      **                   COMIT
336500950109      **+                  ENDIF
336600950109      *
336700950109      * segnalo al pgm chiamante l'avvenuta conferma
336800950105    >C                   MOVE      *ON           OPR027
336900950109      * segnalo al pgm chiamante l'aver premuto l'F6
337000950105    >C                   MOVE      '0'           RET027
337100950109      *
337200941027     C                   SELECT
337300950109      * nel caso di immissione, ripristino la videata iniziale
337400950105    >C     OPZ027        WHENEQ    '01'
337500940224     C                   MOVE      *ON           $INZD1
337600941228     C                   MOVE      'D1'          $GEST
337700950109      * altrimenti ritorno al pgm chiamante
337800940224     C                   OTHER
337900940224     C                   MOVE      *ON           $FINE
338000941027     C                   ENDSL
338100071128     c*
338200071128C2063C* se richiamato da ANA088R (rapporti bancari) ha abi
338300071128  "  c* valorizzato, e in questo caso esce
338400071128  "  c                   if        Abi027 <> *blanks
338500071128  "  C                   MOVE      *ON           $FINE
338600071128c2063c                   endif
338700950109      *
338800940224     C                   ENDSR
338900941205     C/EJECT
339000950109      ************************************************************
339100950109      * CONTROLLO ABILITAZIONE TASTI
339200950109      ************************************************************
339300941205     C     CTRKEY        BEGSR
339400950109      *
339500941205     C                   SELECT
339600941205     C     $TASTO        WHENEQ    F01
339700941205     C     $FC(1)        IFEQ      '0'
339800941205     C                   SETON                                        99
339900941205     C                   ENDIF
340000941205     C     $TASTO        WHENEQ    F02
340100941205     C     $FC(2)        IFEQ      '0'
340200941205     C                   SETON                                        99
340300941205     C                   ENDIF
340400941205     C     $TASTO        WHENEQ    F03
340500941205     C     $FC(3)        IFEQ      '0'
340600941205     C                   SETON                                        99
340700941205     C                   ENDIF
340800941205     C     $TASTO        WHENEQ    F04
340900941205     C     $FC(4)        IFEQ      '0'
341000941205     C                   SETON                                        99
341100941205     C                   ENDIF
341200941205     C     $TASTO        WHENEQ    F05
341300941205     C     $FC(5)        IFEQ      '0'
341400941205     C                   SETON                                        99
341500941205     C                   ENDIF
341600941205     C     $TASTO        WHENEQ    F06
341700941205     C     $FC(6)        IFEQ      '0'
341800941205     C                   SETON                                        99
341900941205     C                   ENDIF
342000941205     C     $TASTO        WHENEQ    F07
342100941205     C     $FC(7)        IFEQ      '0'
342200941205     C                   SETON                                        99
342300941205     C                   ENDIF
342400941205     C     $TASTO        WHENEQ    F08
342500941205     C     $FC(8)        IFEQ      '0'
342600941205     C                   SETON                                        99
342700941205     C                   ENDIF
342800941205     C     $TASTO        WHENEQ    F09
342900941205     C     $FC(09)       IFEQ      '0'
343000941205     C                   SETON                                        99
343100941205     C                   ENDIF
343200941205     C     $TASTO        WHENEQ    F10
343300941205     C     $FC(10)       IFEQ      '0'
343400941205     C                   SETON                                        99
343500941205     C                   ENDIF
343600941205     C     $TASTO        WHENEQ    F11
343700941205     C     $FC(11)       IFEQ      '0'
343800941205     C                   SETON                                        99
343900941205     C                   ENDIF
344000941205     C     $TASTO        WHENEQ    F12
344100941205     C     $FC(12)       IFEQ      '0'
344200941205     C                   SETON                                        99
344300941205     C                   ENDIF
344400941205     C     $TASTO        WHENEQ    F13
344500941205     C     $FC(13)       IFEQ      '0'
344600941205     C                   SETON                                        99
344700941205     C                   ENDIF
344800941205     C     $TASTO        WHENEQ    F14
344900941205     C     $FC(14)       IFEQ      '0'
345000941205     C                   SETON                                        99
345100941205     C                   ENDIF
345200941205     C     $TASTO        WHENEQ    F15
345300941205     C     $FC(15)       IFEQ      '0'
345400941205     C                   SETON                                        99
345500941205     C                   ENDIF
345600941205     C     $TASTO        WHENEQ    F16
345700941205     C     $FC(16)       IFEQ      '0'
345800941205     C                   SETON                                        99
345900941205     C                   ENDIF
346000941205     C     $TASTO        WHENEQ    F17
346100941205     C     $FC(17)       IFEQ      '0'
346200941205     C                   SETON                                        99
346300941205     C                   ENDIF
346400941205     C     $TASTO        WHENEQ    F18
346500941205     C     $FC(18)       IFEQ      '0'
346600941205     C                   SETON                                        99
346700941205     C                   ENDIF
346800941205     C     $TASTO        WHENEQ    F19
346900941205     C     $FC(19)       IFEQ      '0'
347000941205     C                   SETON                                        99
347100941205     C                   ENDIF
347200941205     C     $TASTO        WHENEQ    F20
347300941205     C     $FC(20)       IFEQ      '0'
347400941205     C                   SETON                                        99
347500941205     C                   ENDIF
347600941205     C     $TASTO        WHENEQ    F21
347700941205     C     $FC(21)       IFEQ      '0'
347800941205     C                   SETON                                        99
347900941205     C                   ENDIF
348000941205     C     $TASTO        WHENEQ    F22
348100941205     C     $FC(22)       IFEQ      '0'
348200941205     C                   SETON                                        99
348300941205     C                   ENDIF
348400941205     C     $TASTO        WHENEQ    F23
348500941205     C     $FC(23)       IFEQ      '0'
348600941205     C                   SETON                                        99
348700941205     C                   ENDIF
348800941205     C     $TASTO        WHENEQ    F24
348900941205     C     $FC(24)       IFEQ      '0'
349000941205     C                   SETON                                        99
349100941205     C                   ENDIF
349200941205     C                   ENDSL
349300950109      *
349400941205     C                   ENDSR
349500940318     C/EJECT
349600950109      ******************************************************
349700950109      * PGM PER GESTIONE MESSAGGI ERRORE                   *
349800950109      ******************************************************
349900940318     C     MSGPGM        BEGSR
350000950109      *
350100940318     C                   Z-ADD     2             MSGMSG
350200940318     C                   MOVEL     'S'           MSGRSP
350300940318     C                   MOVEL     'CR'          MSGTPR
350400940318     C                   MOVEL     'S'           MSGEMV
350500940318     C                   Z-ADD     10            MSGRGP
350600940318     C                   MOVEL     'S'           MSGLCK
350700940318     C                   MOVEA     STATUS        STS
350800940318     C                   MOVEL     'N'           MSGCNL
350900940318     C                   MOVEL     'N'           MSGVID
351000940318     C                   MOVEL     'N'           MSGOPE
351100940318     C                   MOVEL     'N'           MSGSTP
351200940318     C                   CALL      'XMSGR'
351300940318     C                   PARM                    XMSGDS
351400950109      *
351500940318     C                   ENDSR
351600940207     C/EJECT
351700950109      ************************************************************
351800950109      * OPERAZIONI INIZIALI
351900950109      ************************************************************
352000940131     C     *INZSR        BEGSR
352100000114A0735C     *DMY          Move      Udate         ZDateISO
352200000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
352300950109      *
352400950109      * Reperimento parametri
352500950109      *
352600940117     C     *ENTRY        PLIST
352700940117     C                   PARM                    KPJBA
352800950109      *
352900950109      * Reperimento tasti di funzione
353000950109      *
353100940506     C                   EXSR      REPKEY
353200950109      *
353300940223     C                   ENDSR
353400940223     C/EJECT
353500950109      ************************************************************
353600950109      * REPERIMENTO TASTI DI FUNZIONE
353700950109      ************************************************************
353800940506     C     REPKEY        BEGSR
353900940506     C                   MOVEA     $FM1          $FM
354000950906     C                   MOVEA     $FC1          $FC
354100940506     C                   EXSR      REPKKK
354200940506     C                   MOVEA     $FX           $FX1
354300950109      *
354400940614     C                   ENDSR
354500940506     C/EJECT
354600950109      ************************************************************
354700950109      * REPERIMENTO TASTI DI FUNZIONE
354800950109      ************************************************************
354900940506     C     REPKKK        BEGSR
355000950109      *
355100940506     C                   CLEAR                   $FX
355200940506     C     1             DO        24            X
355300950109      *
355400940506     C     $FM(X)        IFNE      *BLANK
355500950906     C     $FC(X)        ANDNE     '0'
355600950109      *
355700940506     C                   MOVEL     $FM(X)        IDMSG
355800940506     C                   CALL      'XRTVM'
355900940506     C                   PARM                    IDMSG            27
356000940506     C                   PARM                    TXTMSG          100
356100940506     C                   PARM                    ERRMSG            1
356200950109      *
356300940506     C     ERRMSG        IFNE      *ON
356400940506     C                   MOVEL     TXTMSG        $FX(X)
356500940506     C                   ELSE
356600940506     C                   MOVEL     *ALL' '       $FX(X)
356700941027     C                   ENDIF
356800950109      *
356900941027     C                   ENDIF
357000950109      *
357100941027     C                   ENDDO
357200950109      *
357300940506     C                   ENDSR
357400940509     C/EJECT
357500950109      ************************************************************
357600950109      * INIZIALIZZAZIONE VARIABILI
357700950109      ************************************************************
357800940223     C     INZVAR        BEGSR
357900950109      *
358000950109      * Dati da PGM guida
358100950109      *
358200950105    >C                   MOVEL     KPJBU         A027DS
358300950109      *
358400950109      * Reperimento dati società
358500950109      *
358600941115    >C                   MOVEL     'SOC001'      TIPXSC
358700941115     C                   MOVEL     *BLANK        SOCXSC
358800941115     C                   EXSR      REPSOC
358900941115     C     RTNXSC        IFNE      '1'
359000941115     C                   MOVEL     XSOCDS        SOC001
359100941115     C                   ELSE
359200941115     C                   RETURN
359300941115     C                   ENDIF
359400950123     C*
359500950123     C* gestione società di riferimento
359600950123     C     FSC027        IFEQ      'S'
359700950217     C     FSC027        OREQ      ' '
359800950217     C     OPZ027        OREQ      *BLANK
359900950123     C                   MOVE      XSCSOC        WWWSOC
360000950123     C* gestione simulata da capofila
360100950123     C                   ELSE
360200950123     C                   MOVE      XSCCPA        WWWSOC
360300950123     C                   ENDIF
360400950109      *
360500950109      * Definizione dei tasti funzione utilizzabili a pgm
360600950109      *
360700940715     C                   EXSR      DEFOPZ
360800950109      *
360900950109      * Variabili per gestione videate
361000950109      *
361100940223     C                   MOVE      'D1'          $GEST                          fmt in gestione
361200940223     C                   MOVE      *OFF          $FINE                          fine pgm
361300940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
361400940224     C                   MOVE      *BLANK        $LASTG                         fmt di provenienza
361500940224     C                   MOVE      *BLANK        $LASTV                         ultima emissione
361600940506     C                   Z-ADD     0             $ULKD1            3 0          piede da emett. D1
361700070328B0953c                   move      *off          IbanAutom         1
361800950109      *
361900950109      * Variabili appoggio
362000950109      *
362100940224     C                   Z-ADD     0             CURR
362200940224     C                   Z-ADD     0             CURC
362300950109      *
362400950109      * Valorizzazione campi univoci testate
362500950109      *
362600940207     C                   CLEAR                   FMTT1
362700940207     C                   MOVEL     KNSIF         NOMSIF
362800941110     C                   MOVEL     XSCDSI        NOMDIT
362900941027     C                   MOVEL     DSPGM         NOMPGM
363000950426     C* recupero il sottotitolo (se c'è)
363100950426     C                   MOVEL     *BLANK        T1TIT
363200950426     C     SOTTIT        IFNE      *BLANK
363300950426     C                   MOVEL     SOTTIT        IDMSG
363400950426     C                   CALL      'XRTVM'
363500950426     C                   PARM                    IDMSG            27
363600950426     C                   PARM                    TXTMSG          100
363700950426     C                   PARM                    ERRMSG            1
363800950426     C                   PARM      *ON           CTRMSG            1            centratura
363900950426     C                   PARM      30            LENMSG            3 0          lun output.
364000950426     C     ERRMSG        IFNE      *ON
364100950426     C                   MOVEL     TXTMSG        T1TIT
364200950426     C                   ELSE
364300950426     C                   MOVEL     *ALL'?'       T1TIT
364400950426     C                   ENDIF
364500950426     C                   ENDIF
364600950426     C* recupero il tipo operazione
364700940224     C                   Z-ADD     1             X
364800950105    >C     OPZ027        LOOKUP    $OZ(X)                                 21
364900940224     C     *IN21         IFEQ      *ON
365000950426     C                   MOVEL     $ID(X)        IDMSG
365100950426     C                   CALL      'XRTVM'
365200950426     C                   PARM                    IDMSG            27
365300950426     C                   PARM                    TXTMSG          100
365400950426     C                   PARM                    ERRMSG            1
365500950426     C                   PARM      *ON           CTRMSG            1            centratura
365600950426     C                   PARM      30            LENMSG            3 0          lun output.
365700950426     C     ERRMSG        IFNE      *ON
365800950426     C                   MOVEL     TXTMSG        T1OPE
365900950426     C                   ELSE
366000950426     C                   MOVEL     *ALL'?'       T1OPE
366100950426     C                   ENDIF
366200950426     C                   ENDIF
366300100923C2169C* Reperisco il parametro modulo bonifici
366400100923C2169C                   EXSR      REPPAR
366500950109      *
366600950109      * Inizializzazione/Decodifica prima videata
366700950109      *
366800940224     C                   EXSR      INZD1
366900940224     C                   MOVE      *OFF          $INZD1
367000950109      *
367100940117     C                   ENDSR
367200941209     C/EJECT
367300950109      ************************************************************
367400950109      * REPERIMENTO DATI SOCIETA'
367500950109      ************************************************************
367600941209     C     REPSOC        BEGSR
367700950109      *
367800961003     C                   CALLb     'XSOC'
367900941209     C                   PARM                    TIPXSC            6
368000941209     C                   PARM                    SOCXSC            3
368100941209     C                   PARM                    CDSXSC            9 0
368200941209     C                   PARM                    MODXSC            3
368300941209     C                   PARM      *BLANKS       RTNXSC            1
368400941209     C                   PARM                    XSOCDS
368500941209     C                   PARM                    KPJBA
368600950109      *
368700941209     C                   ENDSR
368800940715     C/EJECT
368900100923C2169C************************************************************
369000100923  "  C* Rerimento parametro modulo
369100100923  "  C************************************************************
369200100923C2169C     REPPAR        BEGSR
369300100923      *
369400100923      * Reperimento parametro modulo BONIFICI
369500100923     C                   Clear                   Anp039DS
369600100923     C                   CALLB     'XPAR'
369700100923     C                   Parm                    XScSoc
369800100923     C                   Parm      'BONIFICI'    XPaPar            8
369900100923     C                   Parm                    XPaOut
370000100923     C                   Parm      *OFF          XPaErr            1
370100100923     C                   Parm      ' '           XPaRic            1
370200100923     C     XPaErr        IfEq      *Off
370300100923     C                   Movel     XPaOut        Anp039DS
370400100923     C                   EndIf
370500100923     C*
370600100923C2169C                   ENDSR
370700100923C2169C/EJECT
370800950109      ************************************************************
370900950109      * DEFINIZIONE TASTI FUNZIONE UTILIZZABILI A PGM
371000950109      ************************************************************
371100940715     C     DEFOPZ        BEGSR
371200950109      *
371300950109      * Questa routine deve valorizzare la schiera $FC1/$FR1
371400950109      * con S o N a seconda della utilizzabilità o meno
371500950109      * del tasto funzione correlato
371600950109      *
371700950109      * Inizialmente ripristino le abilitazioni come da schiera a tempo
371800950109      * di compilazione
371900940715     C                   RESET                   $FC1
372000940715     C                   RESET                   $FR1
372100950109      *
372200950109      * Dopodichè, a seconda del richiamo e delle autorizzazioni
372300950109      * invalido alcune opzioni
372400940715     C                   SELECT
372500950109      * Se pgm richiamato in interrogazione disattivo :
372600950105    >C     OPZ027        WHENEQ    '05'
372700950109      * F4=Ricerca
372800940715     C                   Z-ADD     4             X
372900940720     C                   MOVE      '0'           $FC1(X)
373000950109      * F5=Ripristino
373100940715     C                   Z-ADD     5             X
373200940720     C                   MOVE      '0'           $FC1(X)
373300950109      * F6=Conferma
373400940715     C                   Z-ADD     06            X
373500940720     C                   MOVE      '0'           $FC1(X)
373600030820B0584 * F11=Forzatura
373700030820  "  C                   Z-ADD     11            X
373800030820B0584C                   MOVE      '0'           $FC1(X)
373900950109      * Se pgm richiamato in annullamento disattivo :
374000950105    >C     OPZ027        WHENEQ    '04'
374100950109      * F4=Ricerca
374200940715     C                   Z-ADD     4             X
374300940720     C                   MOVE      '0'           $FC1(X)
374400950109      * F5=Ripristino
374500940715     C                   Z-ADD     5             X
374600940720     C                   MOVE      '0'           $FC1(X)
374700030820B0584 * F11=Forzatura
374800030820  "  C                   Z-ADD     11            X
374900030820B0584C                   MOVE      '0'           $FC1(X)
375000950109      *
375100940715     C                   OTHER
375200950109      *
375300940715     C                   ENDSL
375400950109      *
375500950109      * Disattivo a seconda delle autorizzazioni :
375600950109    > *********  AUTXXX    IFEQ 'N'
375700950109    > *********            Z-ADD1         X
375800950109    > *********  'XX'      LOKUP$OP1,X                   21
375900950109    > *********  *IN21     IFEQ *ON
376000950109    > *********            MOVE 'N'       $FL1,X
376100950109    > *********            ENDIF
376200950109    > *********            ENDIF
376300950109      *
376400940715     C                   ENDSR
376500950109      ************************************************************
376600950109      * DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
376700950109      ************************************************************
376800940223     C     DEFCAM        BEGSR
376900950109      *
377000950109      * klist
377100950109    > * ANCBA01L
377200950911    >C     K6CBA1        KLIST
377300950911    >C                   KFLD                    CBASOC
377400950911    >C                   KFLD                    CBACOF
377500950911    >C                   KFLD                    CBAKCC
377600950911    >C                   KFLD                    CBAKSC
377700950911    >C                   KFLD                    CBAENT
377800950911    >C                   KFLD                    CBATAP
377900950911    >C     K7CBA1        KLIST
378000950911    >C                   KFLD                    CBASOC
378100950911    >C                   KFLD                    CBACOF
378200950911    >C                   KFLD                    CBAKCC
378300950911    >C                   KFLD                    CBAKSC
378400950911    >C                   KFLD                    CBAENT
378500950911    >C                   KFLD                    CBATAP
378600950105    >C                   KFLD                    CBADAL
378700950111    > * ANRCO01L
378800950111    >C     K3RCO1        KLIST
378900950111    >C                   KFLD                    RCOSOC
379000950111    >C                   KFLD                    RCOKCC
379100950111    >C                   KFLD                    RCOKSC
379200950905    > * ANPDC01L
379300950905    >C     K3PDC1        KLIST
379400950905    >C                   KFLD                    PDCSOC
379500950905    >C                   KFLD                    PDCKCC
379600950905    >C                   KFLD                    PDCKSC
379700950109    > * ANCBD01L
379800950105    >C     K7CBD1        KLIST
379900060413A1152X****************** KFLD                    CBASOC
380000060413  "  X****************** KFLD                    CBACOF
380100060413  "  X****************** KFLD                    CBAKCC
380200060413  "  X****************** KFLD                    CBAKSC
380300060413  "  X****************** KFLD                    CBAENT
380400060413  "  X****************** KFLD                    CBATAP
380500060413A1152X****************** KFLD                    CBADAL
380600060413A1152C                   kfld                    CBDSoc
380700060413  "  C                   kfld                    CBDCof
380800060413  "  C                   kfld                    CBDKcc
380900060413  "  C                   kfld                    CBDKsc
381000060413  "  C                   kfld                    CBDEnt
381100060413  "  C                   kfld                    CBDTap
381200060413  "  C                   kfld                    CBDDal
381300060413  "  C     K8CBD1        KLIST
381400060413  "  C                   kfld                    CBDSoc
381500060413  "  C                   kfld                    CBDCof
381600060413  "  C                   kfld                    CBDKcc
381700060413  "  C                   kfld                    CBDKsc
381800060413  "  C                   kfld                    CBDEnt
381900060413  "  C                   kfld                    CBDTap
382000060413  "  C                   kfld                    CBDDal
382100060413  "  C                   kfld                    CBDNrRiga
382200060413A1152 *
382300071029A1250C     K02Acr01      KLIST
382400071029  "  C                   kfld                    AcrAbi
382500071029A1250C                   kfld                    AcrCab
382600071127c2063 * ANban01L
382700071127  "  C     K02ban01      KLIST
382800071127  "  C                   KFLD                    xscsoc
382900071127c2063C                   KFLD                    banbanca
383000950109      *
383100950109      * Variabili per gestione videate
383200950109      *
383300940223     C                   MOVE      *BLANK        $GEST             2            fmt in gestione
383400940223     C                   MOVE      *BLANK        $FINE             1            fine pgm
383500940223     C                   MOVE      *BLANK        $INZD1            1            inizializzione D1
383600940223     C                   MOVE      *BLANK        $LASTG            2            fmt di provenienza
383700940223     C                   MOVE      *BLANK        $LASTV            2            ultima emissione
383800940225     C                   Z-ADD     0             $PIED1            3 0          piede da emett. D1
383900950109      *
384000950109      * Variabili appoggio
384100950109      *
384200950110     C     *LIKE         DEFINE    CBATAP        WWTAP
384300950123     C     *LIKE         DEFINE    XSCSOC        WWWSOC
384400950110      *
384500940223     C                   Z-ADD     0             CURR              2 0
384600940223     C                   Z-ADD     0             CURC              2 0
384700940223     C                   MOVE      *ZEROS        WIN              99
384800940224     C                   MOVE      *BLANK        WTBERR            1
384900950109      *
385000950109      * Indici
385100950109      *
385200940223     C                   Z-ADD     0             X                 3 0
385300940223     C                   Z-ADD     0             Y                 3 0
385400950109      *
385500940117     C                   ENDSR
385600940509     C/EJECT
385700950109      ************************************************************
385800950109      * RIEMPIMENTO TASTI FUNZIONE
385900950109      ************************************************************
386000940509     C     RIEKEY        BEGSR
386100950109      *
386200950109      * Input :
386300950109      * - $FX = schiera con tutti i testi disponibili
386400950109      * - $FC = schiere con i flag di validità associati ai testi
386500950109      * - $FR = schiere con i flag di REVERSE IMAGE
386600950109      * - $ULK = ultimo testo caricato
386700950109      * - $L01 = Lunghezza prima riga
386800950109      * - $L02 = Lunghezza seconda riga
386900950109      * Output :
387000950109      * - $ULK
387100950109      * - $KEY1= 1° riga di tasti funzione
387200950109      * - $KEY2= 2° riga di tasti funzione
387300950109      *
387400961003     C                   CALLb     'XKEYMSG'
387500950109      * input
387600940509     C                   PARM                    $FX
387700940509     C                   PARM                    $FC
387800940509     C                   PARM                    $FR
387900940509     C                   PARM                    $ULK              3 0
388000950109      * output
388100940509     C                   PARM                    $KEY1            79
388200940509     C                   PARM                    $KEY2            79
388300950109    > * input
388400941116    >C                   PARM      79            $L01              3 0
388500941116    >C                   PARM      79            $L02              3 0
388600950109      *
388700060224A1148C                   PARM                    $ALLFUNCT       480
388800060224A1148C*
388900940509     C                   ENDSR
389000030820      * B0584
389100030820      * B0584 abilitato tasto F11 su D1
389200130103B1206 * B1206 abilitato tasto F7 su D1
389300950906**
38940095042601PROMSG    *LIBL     COS0001
38950095042602PROMSG    *LIBL     COS0002
38960095042603PROMSG    *LIBL     COS0003
38970095042604PROMSG    *LIBL     COS0004
38980095042605PROMSG    *LIBL     COS0005
389900940509** TASTI DI FUNZIONE  D1
390000940509PROMSG    *LIBL     KEY0001  01
390100940509PROMSG    *LIBL     KEY0002  02
390200940509PROMSG    *LIBL     KEY0003  03
390300940509PROMSG    *LIBL     KEY0004  04
390400940509PROMSG    *LIBL     KEY0005  05
390500940509PROMSG    *LIBL     KEY0006  06
390600130103PROMSG    *LIBL     KEY0638  07                              B1206
390700940509PROMSG    *LIBL     KEY0008  08
390800940509PROMSG    *LIBL     KEY0009  09
390900940509PROMSG    *LIBL     KEY0010  10
391000940509PROMSG    *LIBL     KEY0011  11
391100940509PROMSG    *LIBL     KEY0012  12
391200940509PROMSG    *LIBL     KEY0013  13
391300940509PROMSG    *LIBL     KEY0014  14
391400940509PROMSG    *LIBL     KEY0015  15
391500940509PROMSG    *LIBL     KEY0016  16
391600071029PROMSG    *LIBL     KEY0329  17                              A1250
391700940509PROMSG    *LIBL     KEY0018  18
391800940509PROMSG    *LIBL     KEY0019  19
391900071029PROMSG    *LIBL     KEY0020  20
392000940509PROMSG    *LIBL     KEY0021  21
392100940509PROMSG    *LIBL     KEY0022  22
392200940509PROMSG    *LIBL     KEY0023  23
392300940509PROMSG    *LIBL     KEY0024  24
392400940509**  ABILITAZIONE D1
3925009407200N  01
3926009407200N  02
3927009407201N  03
3928009407201N  04
3929009407201N  05
3930009407201N  06
3931001301031N  07                                                         B1206
3932009407201N  08
3933009407200N  09
3934009411160N  10
3935000308201N  11
3936009407201N  12
3937009407200N  13
3938009407200N  14
3939009407200N  15
3940009407200N  16
3941000710291N  17                                                         A1250
3942009407200N  18
3943009407200N  19
3944000710290N  20
3945009407200N  21
3946009407200N  22
3947009509060N  23   Impostare a '1' se si utilizzano opzioni
3948009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
