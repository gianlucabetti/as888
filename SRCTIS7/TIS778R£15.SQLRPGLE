000100000000     h*--------------------------------------------------------------------------------------------*
000200000000     h* Visualizza bolla da codice spedizione
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000      /TITLE Carica dati dettaglio spedizione
000500111108     H DFTACTGRP(*NO) ACTGRP(*CALLER) BNDDIR('TIS':'TIBS')
000600100513     hdatedit(*DMY) DECEDIT(*JOBRUN)
000700000000     f*--------------------------------------------------------------------------------------------*
000800000000     f* Data base
000900000000     f*--------------------------------------------------------------------------------------------*
001000000000     f*---
001100000000     f* Bolle di sede
001200000000     f*---
001300000000     ftitas30c  IF   E           K DISK    USROPN
001400000000     f*---
001500000000     f* Bolle di sede - Anagrafiche
001600000000     f*---
001700000000     ftitaa30c  IF   E           K DISK    USROPN
001800000000     f*---
001900000000     f* Bolle di sede - Riferimenti
002000000000     f*---
002100000000     ftita430c  IF   E           K DISK    USROPN
002200000000     f*---
002300000000     f* Eventi
002400000000     f*---
002500000000     ffnevb01l  IF   E           K DISK    USROPN
002600000000     f*---
002700000000     f* Legami bolla
002800000000     f*---
002900000000     ffnlbl01l  IF   E           K DISK    USROPN
003000000000     ffnlbl02l  IF   E           K DISK    USROPN
003100000000     f                                     RENAME(fnlbl000:fnlbl2)
003200130312     f*---
003300130312     f* bolle in arrivo e esiti PDA
003400130312     f*---
003500140226     ffnarb01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FNARB01L')
003600160126     ffnblp01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FNBLP01L')
003700160127     f                                     prefix(arb:3)
003800150403     ffiarp01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIARP01L')
003900140226     ffipct02l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIPCT02L')
004000140226     ffidst01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIDST01L')
004100140605     ffiar501l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIAR501L')
004200140606     f                                     RENAME(fiar5000:fiar5f)
004300140605     ffiar401l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIAR401L')
004400160314     ffiar901l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIAR901L')
004500000000     f*---
004600000000     f* Giacenze
004700000000     f*---
004800050309     ftigcp51l  IF   E           K DISK    USROPN
004900050309     ftignp01l  IF   E           K DISK    USROPN
005000000000     f*---
005100000000     f* Contrassegni
005200000000     f*---
005300000000     ftncsb03l  IF   E           K DISK    USROPN
005400000000     f*---
005500000000     f* Danni testata CA
005600000000     f*---
005700040906     ffndct01l  IF   E           K DISK    USROPN
005800030206     f*---
005900030206     f* Estensione testata bolle
006000030206     f*---
006100040531     ffiar531c  IF   E           K DISK    USROPN
006200000000     f*---
006300000000     f* Clienti abilitati
006400000000     f*---
006500000000     ftivss02l  IF   E           K DISK    USROPN
006600010716     f*---
006700010716     f* Estensione DPD
006800010716     f*---
006900010716     ftivtd01L  if   e           k disk    USROPN
007000000000     f*---
007100000000     f* Tabelle -NEW-
007200000000     f*---
007300000000     ftntbe01l  IF   E           K DISK    USROPN
007400000000     f*---
007500000000     f* Organigramma
007600000000     f*---
007700000000     fazorg01l  IF   E           K DISK    USROPN                               *organigramma
007800170301     f*---
007900170301     f* note tariffe
008000170301     f*---
008100170301     ftfntc01l  IF   E           K DISK    USROPN
008200051110
008300051110     ***********************************************************************************************
008400051110     **?
008500051110     **?Definizione procedure.
008600051110     **?
008700051110     ***********************************************************************************************
008800051110     D GetSpeChkCde    PR            10U 0
008900051110     D  Anno                          4
009000051110     D                                     VALUE
009100051110     D  IdSpedizione                 12
009200051110     D                                     VALUE
009300051110     D  ChkCode                      10U 0
009400051110     D  nEsito                        5I 0
009500051110     D tis7653r        PR
009600051110     D                                     EXTPGM('TIS7653R')
009700051110     D  ksu
009800051110     D                                     LIKE(kscI74)
009900051110     D  agc
010000051110     D                                     LIKE(gcpAgc)
010100051110     D  fgc
010200051110     D                                     LIKE(gcpFgc)
010300051110     D  ngc
010400051110     D                                     LIKE(gcpNgc)
010500051110     D  esito10                      10I 0
010600060612     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
010700060612     D  msgId                         7A   CONST
010800060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
010900060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
011000060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
011100060612     D                                     VARYING
011200060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
011300060612     D                                     VARYING
011400060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
011500060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
011600060621     D inzLingue       PR            10I 0
011700060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
011800060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
011900060621     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
012000060621     D cvtLinguaISO2ToTabel...
012100060621     D                 PR                  LIKE(linTabel)
012200060621     D  rqsISO2                            LIKE(linISO2)
012300060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
012400060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012500060621     D  rpyTabel                           LIKE(linTabel)
012600060621     D                                     OPTIONS(*NOPASS:*OMIT)
012700060621     D cvtLinguaISO2ToTntbe...
012800060621     D                 PR                  LIKE(linTntbe)
012900060621     D  rqsISO2                            LIKE(linISO2)
013000060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
013100060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
013200060621     D  rpyTntbe                           LIKE(linTntbe)
013300060621     D                                     OPTIONS(*NOPASS:*OMIT)
013400060621     D openTabel00f    PR
013500060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
013600060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
013700170315
013800060626     D chainTabel00f   PR           109A
013900060626     D  rqsOpCode                    10A   CONST
014000060626     D  rqsKut                        1P 0
014100060626     D  rqsCod                        2A   CONST
014200060626     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
014300060626     D  rqsLengthOfKey...
014400060626     D                               10I 0 CONST
014500060626     D  rqsFormat                    10A   CONST
014600060626     D  rpyOpCode                    10A
014700060626     D  rpyEsito                     10I 0
014800060626     D  rpyData                     109A   OPTIONS(*NOPASS:*OMIT)
014900170315     d
015000060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
015100060622     D  kpjba                              LIKEDS(kpjba)
015200060622     D  tibs02ds                           LIKEDS(tibs02ds)
015300170315     D
015400170315     D fnlgw00r        PR                  EXTPGM('FNLGW00R')
015500170315     D prmRqsOpCode...
015600170315     D                               10I 0 CONST
015700170315     D prmRpyOpCode...
015800170315     D                               10I 0
015900170315     D prmRpyIdMsg...
016000170315     D                               10I 0
016100170315     D prmRqsFormato...
016200170315     D                               10A   CONST
016300170315     D prmRqsData...
016400170315     D                            32767A   OPTIONS(*VARSIZE)
016500170315     D prmRqsDataSize...
016600170315     D                               10I 0 CONST
016700170315     D prmRpyFormato...
016800170315     D                               10A   CONST
016900170315     D prmRpyData...
017000170315     D                            32767A   OPTIONS(*VARSIZE)
017100170315     D prmRpyDataSize...
017200170315     D                               10I 0 CONST
017300170315     D prmRpyFormMsg...
017400170315     D                               10A   CONST OPTIONS(*NOPASS)
017500170315     D prmRpyMessage...
017600170315     D                            32767A   OPTIONS(*VARSIZE : *NOPASS)
017700170315     D prmRpyMsgSize...
017800170315     D                               10I 0 CONST OPTIONS(*NOPASS)
017900170315
018000170315     d
018100170315     d
018200090828     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
018300090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7702R
018400090828
018500051222     ***********************************************************************************************
018600051222     **?
018700051222     **?Definizione costanti.
018800051222     **?
018900051222     ***********************************************************************************************
019000101104     D ITALIA          C                   ' '
019100101104     D ESTERO          C                   'E'
019200101104     D EVENTO_ARRIVATA_IN_FILIALE...
019300101104     D                 C                   '703'
019400101104     D EVENTO_MESSA_IN_CONSEGNA...
019500101104     D                 C                   'MIC'
019600150612     D EVENTO_ARRIVATA_HUB...
019700150612     D                 C                   '712'
019800000000     d*--------------------------------------------------------------------------------------------*
019900000000     d* Data structure
020000000000     d*--------------------------------------------------------------------------------------------*
020100150403     d dvpooraris    e ds                  inz
020200150403     d fipctcetds    e ds
020300140605     d fipctcords    e ds
020400090914     D cndizion      E DS                  QUALIFIED BASED(nullPtr)
020500090914     D azlin00f      E DS                  BASED(nullPtr)
020600060622     D tabel00f      E DS                  INZ
020700060622     D kpjba         E DS                  INZ
020800140605     D trulorsds     E DS                  INZ
020900140605     D trulor2ds     E DS                  INZ
021000140605     D trulor3ds     E DS                  INZ
021100150819     D Diore01       E DS                  INZ
021200140606     d dar5emd       e ds                  inz prefix(§)
021300140605     D tnsd99ds      E DS                  INZ
021400160323     D fidn40ds      E DS                  INZ
021500160323     D tibs02ds      E DS                  INZ
021600060622     D  t02Mod       E                     INZ('C')                             Controllo
021700090828     D tis7700i0     E DS                  QUALIFIED
021800090828     D                                     INZ(*EXTDFT)
021900090828     D tis7700o0     E DS                  QUALIFIED
022000170314     D fnlgw00i0     E DS                  QUALIFIED
022100170314     D fnlgw00o0     E DS                  QUALIFIED
022200170315     D fnlgw00m0     E DS                  QUALIFIED
022300090828     d*---
022400000000     d* Ridenominazione formato record -TITAS-
022500000000     d*---
022600000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
022700000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
022800000000     d                                     PREFIX(s_)
022900140715     d titasdssavF   E DS                  EXTNAME(titas00f)                    *depisito titas
023000140715     d                                     PREFIX(f_)
023100080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
023200080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
023300080131     D                                     BASED(titas000FigliaPtr)
023400080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
023500080131     D                                     BASED(titas000FigliaPtr)
023600080131     d*---
023700000000     d* Variabili riferite al data base
023800000000     d*---
023900141126     d kasaasm         S                   LIKE(tasaas)                         *lettura titas30c
024000141126     d kaslnpm         S                   LIKE(taslnp)
024100141126     d kasnrsm         S                   LIKE(tasnrs)
024200141126     d kasnspm         S                   LIKE(tasnsp)
024300140715     d kasaasf         S                   LIKE(tasaas)                         *lettura titas30c
024400140715     d kaslnpf         S                   LIKE(taslnp)
024500140715     d kasnrsf         S                   LIKE(tasnrs)
024600140715     d kasnspf         S                   LIKE(tasnsp)
024700000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
024800000000     d kaslnp          S                   LIKE(taslnp)
024900000000     d kasnrs          S                   LIKE(tasnrs)
025000000000     d kasnsp          S                   LIKE(tasnsp)
025100000000     d kastbl          S                   LIKE(tastbl)
025200000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
025300000000     d kaalnp          S                   LIKE(taalnp)
025400000000     d kaanrs          S                   LIKE(taanrs)
025500000000     d kaansp          S                   LIKE(taansp)
025600000000     d kaatrc          S                   LIKE(taatrc)
025700000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
025800000000     d ka4lnp          S                   LIKE(ta4lnp)
025900000000     d ka4nrs          S                   LIKE(ta4nrs)
026000000000     d ka4nsp          S                   LIKE(ta4nsp)
026100000000     d ka4trc          S                   LIKE(ta4trc)
026200000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
026300000000     d kvblnp          S                   LIKE(evblnp)
026400000000     d kvbnrs          S                   LIKE(evbnrs)
026500000000     d kvbnsp          S                   LIKE(evbnsp)
026600000000     d kvbdev          S                   LIKE(evbdev)
026700000000     d kvbhev          S                   LIKE(evbhev)
026800000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
026900000000     d kcplnp          S                   LIKE(gcplnp)
027000000000     d kcpnrs          S                   LIKE(gcpnrs)
027100000000     d kcpnsp          S                   LIKE(gcpnsp)
027200000000     d kcpfrg          S                   LIKE(gcpfrg)
027300000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
027400000000     d knpfgc          S                   LIKE(gnpfgc)
027500000000     d knpngc          S                   LIKE(gnpngc)
027600000000     d knpfrg          S                   LIKE(gnpfrg)
027700000000     d knpfas          S                   LIKE(gnpfas)
027800000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
027900000000     d ksblnp          S                   LIKE(csblnp)
028000000000     d ksbnrs          S                   LIKE(csbnrs)
028100000000     d ksbnsp          S                   LIKE(csbnsp)
028200000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
028300000000     d kbllpn          S                   LIKE(lbllpn)
028400000000     d kblnrn          S                   LIKE(lblnrn)
028500000000     d kblnsn          S                   LIKE(lblnsn)
028600000000     d kblaap          S                   LIKE(lblaap)
028700000000     d kbllpp          S                   LIKE(lbllpp)
028800000000     d kblnrp          S                   LIKE(lblnrp)
028900000000     d kblnsp          S                   LIKE(lblnsp)
029000000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
029100000000     d kctlnp          S                   LIKE(dctlnp)
029200000000     d kctnrs          S                   LIKE(dctnrs)
029300000000     d kctnsp          S                   LIKE(dctnsp)
029400040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
029500030206     d kr5lnp          S                   LIKE(ar5lnp)
029600030206     d kr5nrs          S                   LIKE(ar5nrs)
029700030206     d kr5nsp          S                   LIKE(ar5nsp)
029800030206     d kr5trd          S                   LIKE(ar5trd)
029900000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
030000000000     d kssisv          S                   LIKE(vssisv)
030100010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
030200000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
030300000000     d kbeke1          S                   LIKE(tbeke1)
030400000000     d kbeke2          S                   LIKE(tbeke2)
030500000000     d kbelin          S                   LIKE(tbelin)
030600000000     d kbesif          S                   LIKE(tbesif)
030700000000     d waas            S                   LIKE(tasaas)                         *di lavoro
030800000000     d wlnp            S                   LIKE(taslnp)
030900000000     d wnrs            S                   LIKE(tasnrs)
031000000000     d wnsp            S                   LIKE(tasnsp)
031100000000     d wfca            S                   LIKE(tasfca)
031200000000     d wfgc            S                   LIKE(tasfgc)
031300000000     d wfpr            S                   LIKE(dctfpr)
031400000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
031500000000     d deplpn          S                   LIKE(lbllpn)
031600000000     d depnrn          S                   LIKE(lblnrn)
031700000000     d depnsn          S                   LIKE(lblnsn)
031800010716     d depute          S                   LIKE(vtdute)
031900010716     d deppwd          S                   LIKE(vtdpwd)
032000000000     d lcca            S                   LIKE(tascca)                         *di lavoro
032100000000     d lduc            S                   LIKE(tasduc)
032200000000     d llnp            S                   LIKE(taslnp)
032300000000     d ldti            S                   LIKE(tasdti)
032400000000     d lhti            S                   LIKE(tashti)
032500000000     d ldrt            S                   LIKE(tasdrt)
032600000000     d llna            S                   LIKE(taslna)
032700150617     d ltfa            S                   LIKE(tastfa)
032800000000     d ldcm            S                   LIKE(tasdcm)
032900000000     d lhmc            S                   LIKE(tashmc)
033000060621     d langTabel       S                   LIKE(tblkut)
033100060621     d langTntbe       S                   LIKE(tbelin)
033200140108     d wDev            S                   LIKE(evbdev)
033300140108     d wHev            S                   LIKE(evbhev)
033400150611     d wDEE            S                   LIKE(d98dee)
033500170314     d wbrtcode        s                   like(brtcodei74)
033600140616     d wParm           s            512a   inz
033700150403     D w_dallemsg      S              5a
033800150403     D w_allemsg       S              5a
033900150403     D w_dalle         S               t
034000150403     D w_alle          S               t
034100150403     D w_dalleoser     S               t
034200150403     D w_alleoser      S               t
034300000000     d*---
034400000000     d* Ds
034500000000     d*---
034600000000      * controllo data
034700000000     d wlbda8          DS                  INZ                                  *controlla data
034800000000     d  g08dat                        8  0                                       -data dritta
034900000000     d  g08inv                        8  0                                       -data invertita
035000000000     d  g08err                        1                                          -errore
035100000000     d  g08tgi                        5  0                                       -giorni fra date
035200000000      * scomposizione data generica
035300000000     d dsdat           DS                  INZ                                  *data
035400000000     d  dsann                         4  0                                       -anno
035500000000     d  dsmes                         2  0                                       -mese
035600000000     d  dsgio                         2  0                                       -giorno
035700000000      * scomposizione data generica (anno + mese/giorno)
035800000000     d                 DS                  INZ                                  *data aa + mm/gg
035900000000     d  dsannB                 1      4  0                                       -anno
036000000000     d  dsmgsB                 5      8  0                                       -mese/giorno
036100000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
036200000000      * composizione data generica editata
036300000000     d dsdate          DS                  INZ                                  *data
036400000000     d  dsgioe                        2  0                                       -giorno
036500000000     d  dsvd1                         1                                          -'/'
036600000000     d  dsmese                        2  0                                       -mese
036700000000     d  dsvd2                         1                                          -'/'
036800000000     d  dsanne                        4  0                                       -anno
036900000000      * scomposizione ora generica
037000000000     d dsora           DS                  INZ                                  *ora
037100000000     d  dshh                          2  0                                       -ore
037200000000     d  dsmm                          2  0                                       -minuti
037300000000      * composizione ora generica editata
037400000000     d dsorae          DS                  INZ                                  *ora
037500000000     d  dshhe                         2  0                                       -ore
037600000000     d  dsvo1                         1                                          -':'
037700000000     d  dsmme                         2  0                                       -minuti
037800030206      * lettura FIAR5 - record bancali
037900030206     d dar5ban       E DS
038000030206      * lettura FIAR5 - record supermercati
038100030206     d dar5scr       E DS
038200040330     d DAR5GEN       E DS
038300040330     D                                     INZ
038400040330     D                                     PREFIX(GEN)
038500000000      * tipi servizio
038600000000     d ds5e          E DS
038700000000      * disposizioni giacenza
038800000000     d ds2d          E DS
038900000000      * causale eventi
039000000000     d dice          E DS
039100000000      * causali chiusura CA
039200000000     d dcch          E DS
039300100119     d dI1a          E DS                  QUALIFIED
039400000000      * consegne anomale
039500000000     d ds7o          E DS
039600130312      * eventi
039700130312     d ds2a          E DS
039800160314     d
039900160314     d* cod bolla
040000160314     d ds3a          E DS
040100000000      * riferimenti
040200000000     d dta4a         E DS
040300070927     d dsbl4e        E DS
040400060627      * Estensione blp/arb tipo record "I"
040500060627     d dsbl4i        E DS
040600040913      * Addebito immagine POD
040700040913     d DLVA1         E DS
040800040913     D                                     INZ
040900000000      * lettura campo orgde3 di AZORG00F
041000000000     d og143         E DS
041100000000      * stati contrassegno
041200000000     d ds4s          E DS
041300141014      * particolarità consegna
041400141014     d ds7r          E DS
041500031217      * DS lettura campo TASFLO di TITAS
041600031217     d dtasflo       E DS
041700000000      * Ds ricerca cliente P.d.C.
041800000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
041900000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
042000000000     d indds         E DS                  EXTNAME(cnind00f) INZ
042100000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
042200000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
042300150304      * stato data prev cons
042400150304     d dspc          E DS
042500170301     d
042600170301     d dcli          E DS
042700150312     d
042800150312     d xgiolavds     e ds
042900000000      * reperimento clienti da cliente unificante
043000090903     d*tibs10ds      E DS
043100090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
043200000000      * scomposizione codice spedizione -DS Input-
043300000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
043400000000     d  dslnpi74                      3                                          -linea partenza
043500000000     d  dsnrsi74                      2                                          -serie
043600000000     d  dsnspi74                      7                                          -spedizione
043700000000      * composizione codice spedizione -bolle-
043800000000     d                 DS
043900000000     d  dstaslnp               1      3  0                                       -linea partenza
044000000000     d  dstasnrs               4      5  0                                       -serie
044100000000     d  dstasnsp               6     12  0                                       -spedizione
044200000000     d dstasspe                1     12                                         *ds cod spedizione
044300000000      * composizione data + ora evento
044400080131     d                 DS                  INZ
044500080131     d  evbdev                 1      8  0                                       -data evento
044600080131     d  evbhev                 9     12  0                                       -ora  evento
044700080131     d evbDevHev               1     12  0                                      *ds data+ora evento
044800130312      * composizione spedizione evento
044900130312     d                 DS                  INZ
045000130312     d  evbaas                 1      4  0                                       -data evento
045100130312     d  evblnp                 5      7  0                                       -ora  evento
045200130312     d  evbnrs                 8      9  0                                       -ora  evento
045300130312     d  evbnsp                10     16  0                                       -ora  evento
045400130312     d dsevbspe                1     16  0                                      *ds data+ora evento
045500060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
045600000000      * ordinamento schiere eventi
045700000000     d                 DS
045800000000     d  dsseql                 1      3  0                                       -sequenza legame
045900000000     d  dsseqe                 4      6  0                                       -sequenza evento
046000000000     d  dsdevhev               7     18  0                                       -data + ora
046100000000     d  dsi                   19     21  0                                       -indice
046200000000     d dssort                  1     21  0                                      *ds cod spedizione
046300000000     d* Input routine XALLINEA
046400000000     d walfa7          S              7                                         *alfanumerico 7
046500000000     d walfa7bis       S              7                                         *alfanumerico 7
046600000000     d wzeri           S              7                                         *alfa con zeri
046700000000     d*---
046800000000     d* Variabili
046900000000     d*---
047000170301     d k_ntcapl        s                   like(ntcapl)
047100170301     d k_ntcnk1        s                   like(ntcnk1)
047200170301     d k_ntcnk2        s                   like(ntcnk2)
047300170301     d k_ntctnt        s                   like(ntctnt)
047400101104     d i               S             10I 0                                      *indici
047500101104     d ii              S             10I 0
047600101104     d j               S             10I 0
047700101104     d jj              S             10I 0
047800101104     d z               S             10I 0
047900101104     d s               S             10I 0
048000101104     d k               S             10I 0
048100101104     d o               S             10I 0
048200101104     d tote            S             10I 0                                      *totale eventi memor
048300000000     d seql            S              5  0                                      *sequenza legami
048400000000     d n14             S             14  0                                      *Numerico 14
048500000000     d n8              S              8  0                                      *Numerico 8
048600000000     d n7              S              7  0                                      *Numerico 7
048700170310     d alfa4           S              4                                         *Numerico 4
048800010927     d n3              S              3  0                                      *Numerico 3
048900000000     d a1              S              1                                         *Alfanumerico 1
049000000000     d a3              S              3                                         *Alfanumerico 3
049100000000     d a5              S              5                                         *Alfanumerico 5
049200010717     d a8              S              8                                         *Alfanumerico 8
049300000000     d a15             S             15                                         *Alfanumerico 15
049400000000     d a200            S            200                                         *Alfanumerico 200
049500000000     d a200bis         S            200                                         *Alfanumerico 200
049600010927     d a230            S            230                                         *Alfanumerico 230
049700000000     d a400            S            400                                         *Alfanumerico 400
049800000000     d datcor          S              8  0                                      *data corrente
049900000000     d datpre          S              8  0                                      *data precedente
050000000000     d oracor          S              6  0                                      *ora corrente
050100000000     d anncor          S              4  0                                      *anno corrente
050200000000     d annpre          S              4  0                                      *anno precedente
050300140617     d wIdMsg          s              7a
050400140617     d wIdCampo        s             10a
050500000000     d werr            S              1                                         *errore generico
050600000000     d wdat            S              8  0                                      *data (aaaammgg)
050700000000     d wdate           S             10                                         *data (gg/mm/aaaa)
050800000000     d wora            S              4  0                                      *ora  (hhmm)
050900000000     d worae           S              5                                         *ora  (hh:mm)
051000000000     d worg            S              3  0                                      *filiale
051100000000     d wodes           S             30                                         *descrizione filiale
051200000000     d wofl1           S              1                                         *flag italia/estero
051300150617     d wtasfl1         S              1                                         *flag italia/estero
051400020702     d wodpd           S              3                                         *flag filiale DPD
051500000000     d wcev            S              3                                         *causale evento
051600030130     d wcdex           S             30                                         *descrizione bar/pt
051700000000     d wpgm            S             20                                         *programma
051800000000     d wftc            S              1                                         *cons.partic.
051900140714     d wricons         S              1                                         *wrk per riconsegna
052000140715     d keyforzata      S              1                                         *wrk per riconsegna
052100000000     d wdftc           S             50                                         *descriz.cons.partic
052200000000     d d1wdftc         S             50                                         *descriz.cons.part.1
052300000000     d d2wdftc         S             50                                         *descriz.cons.part.2
052400000000     d wgcx            S              2                                         *turno chiusura
052500000000     d wdgcx           S             20                                         *descriz.turno chius
052600000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
052700000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
052800000000     d we              S              3                                         *descrizione 'e'
052900000000     d bollaorig       S              1                                         *bolla originaria
053000150310     d PDAavv          S              1                                         *evento AVV a PDA
053100150421     d PDAgia          S              1                                         *evento GIA a PDA
053200000000     d sostrit         S              1                                         sostituisce Ritiro
053300000000     d sostcon         S              1                                         sostituisce Consegna
053400000000     d newevb          S              1                                         *nuovo evento
053500000000     d lavdevhev       S             12  0                                      *data + ora
053600040913     D WrkCurDate      S               D                                        Data corrente
053700060620     D wrkDtInvDisp    S              8
053800060620     D wrkLnaItaEst    S              1
053900060620     D rpyOpCode       S             10A
054000060620     D rpyEsito        S             10I 0
054100060621     D nullPtr         S               *
054200080131     D titas000FigliaPtr...
054300080131     D                 S               *   INZ(%ADDR(titas000Figlia))
054400010716     d*---
054500010716     d* Variabili di controlli
054600010716     d*---
054700010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
054800010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
054900000000     d*---
055000000000     d* Variabili di memorizzazione
055100000000     d*---
055200000000     d mhcre           S              5                                         *consegna richiesta
055300000000     d mdcre           S             10
055400000000     d mtcre           S             50
055500000000     d mftce           S            100                                         *consegna partic.
055600000000     d mgcxe           S            100                                         *turno chiusura
055700000000     d mffd            S            100                                         *fermo deposito
055800010927     d mmncB           S             35                                         *NON consegna LAV
055900010927     d mmncC           S             35
056000010927     d mmncG           S             35                                         *NON consegna GIAC
056100010927     d mmncH           S             35
056200010927     d mmncN           S             35                                         *NON consegna NOFATT
056300010927     d mmncO           S             35
056400050701     d mmnc8           S             35
056500050701     d mmnc9           S             35
056600030206     d mtel            S             20                                         *N° tel destinatario
056700030206     d mban            S             35                                         *bancali
056800030207     d mscr            S            500                                         *supermercati
056900030210     d msdace          S             10
057000030210     d msorce          S              5
057100030210     d msdcre          S             10
057200030210     d mshcre          S              5
057300030210     d mstcre          S             30
057400030210     d msnom           S             50
057500030210     d mscrT           S            500    DIM(5)
057600030211     d wmscrT          S           2504
057700000000     d*---
057800000000     d* Schiere
057900000000     d*---
058000000000      * taabella clienti del cliente unificante
058100000000      * tabella tipi servizio
058200000000     d stsp            S              1    DIM(20)                              *tipi servizio
058300000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
058400000000      * tabella consegne anomale
058500030130     d scca            S              1    DIM(30)                              *consegna anomala
058600000000      * tabella disposizioni giacenza
058700000000     d sdis            S              1    DIM(10)                              *codice
058800000000     d sddis           S             20    DIM(10)                              *descrizione italian
058900000000      * tabella filiali
059000000000     d sorg            S              3  0 DIM(300)                             *filiale
059100000000     d sodes           S             30    DIM(300)                             *descrizione filiale
059200000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
059300160406     d sodpd           S              3    DIM(300)                             ntw
059400000000      * eventi memorizzati
059500000000     d smi             S              3  0 DIM(200)                             *indice
059600000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
059700000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
059800000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
059900000000     d smdeve          S             10    DIM(200)                             *data evento editata
060000000000     d smheve          S              5    DIM(200)                             *ora  evento editata
060100000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
060200000000     d smdev           S             30    DIM(200)                             *descrizione evento
060300000000     d smcev           S              3    DIM(200)                             *causale evento
060400130313     d smspe           S             16  0 DIM(200)                             *causale evento
060500000000      * eventi memorizzati x ordinamento
060600000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
060700000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
060800000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
060900000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
061000000000     d spdeve          S             10    DIM(200)                             *data evento editata
061100000000     d spheve          S              5    DIM(200)                             *ora  evento editata
061200000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
061300000000     d spdev           S             30    DIM(200)                             *descrizione evento
061400000000     d spcev           S              3    DIM(200)                             *causale evento
061500130313     d spspe           S             16  0 DIM(200)                             *causale evento
061600000000      * note giacenze memorizzate
061700051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
061800051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
061900000000      * note memorizzate
062000000000     d smnot           S            100    DIM(10)
062100030206     d*---
062200030206     d* definizioni per passaggio parametri
062300030206     d*---
062400000000     d esito           s              1
062500000000      * DS contenente i dati di INPUT
062600170316     d tis778dsi     e DS                  INZ(*EXTDFT)
062700050104     D  cAASI74                       4
062800050104     D                                     OVERLAY(AASI74)
062900000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
063000140530     d tis778dso     e DS                  INZ(*EXTDFT)
063100000000     d dev                           10    DIM(50) OVERLAY(dato74)
063200000000     d hev                            5    DIM(50) OVERLAY(orao74)
063300000000     d fle                           30    DIM(50) OVERLAY(opeo74)
063400041007     d eve                           30    DIM(50) OVERLAY(eveo74)
063500041007     d cev                            3    DIM(50) OVERLAY(cevo74)
063600000000     d nota                         100    DIM(10) OVERLAY(notao74)
063700130313     d spe             S             16  0 DIM(50)
063800040906
063900140616     d tis778dsM     e DS                  INZ(*EXTDFT)
064000140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
064100140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
064200140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
064300140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
064400140530      *
064500040906     D FIDN12DS      E DS
064600040906     D                                     INZ
064700040906     D  I12FEL       E
064800040906     D                                     INZ('E')
064900040906     D  I12FAN       E
065000040906     D                                     INZ('E')
065100040906     D  O12KeyAry                    14
065200040906     D                                     DIM(20)
065300040906     D                                     OVERLAY(O12KEY)
065400040906
065500040906     D O12KeyDS        DS
065600040906     D                                     INZ
065700040906     D  O12KeyAAC                     4S 0
065800040906     D  O12KeyFil                     3S 0
065900040906     D  O12KeyNCA                     7S 0
066000040906
066100020702      * DS lettura tabella "NTW"
066200020702     d DNTW          E DS
066300130326     d Ddstflr       E DS
066400170314       // -?Controllo BRTCODE
066500170314     d TRUL28DS0     e ds                  INZ(*EXTDFT)
066600041223
066700041223     D ChkCode         S             10U 0
066800041223     D nEsito          S              5I 0
066900051110     D esito10         S             10I 0
067000170315     D prwRqsOpCode...
067100170315     D                 s             10I 0
067200170315     D prwRpyOpCode...
067300170315     D                 s             10I 0
067400170315     D prwRpyIdMsg...
067500170315     D                 s             10I 0
067600170315     D prwRqsFormato...
067700170315     D                 s             10A
067800170315     D prwRqsData...
067900170315     D                 s          32767A
068000170315     D prwRqsDataSize...
068100170315     D                 s             10I 0
068200170315     D prwRpyFormato...
068300170315     D                 s             10A
068400170315     D prwRpyData...
068500170315     D                 s          32767A
068600170315     D prwRpyDataSize...
068700170315     D                 s             10I 0
068800170315     D prwRpyFormMsg...
068900170315     D                 s             10A
069000170315     D prwRpyMessage...
069100170315     D                 s          32767A
069200170315     D prwRpyMsgSize...
069300170315     D                 s             10I 0
069400080131
069500100119     D*--------------------------------------------------
069600100119     D* Procedure name: GetDescrizioneIncassoContrassegno
069700100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
069800100119     D*                          o del contrassegno.
069900100121     D* Returns:        Descrizione modalità incasso contrassegno.
070000100119     D*--------------------------------------------------
070100100119     D GetDescrizioneIncassoContrassegno...
070200100119     D                 PR            35A
070300110304
070400110304     D*--------------------------------------------------
070500110304     D* Procedure name: IsContrassegnoCompensato
070600110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
070700110304     D*                          to con una fattura.
070800110304     D* Returns:        *ON = Compensato
070900110304     D*--------------------------------------------------
071000110304     D IsContrassegnoCompensato...
071100110304     D                 PR              N
071200120321
071300120321     D*--------------------------------------------------
071400120321     D* Procedure name: SetIdAssegnoMittente
071500120321     D* Purpose:        Imposta l'ID assegno mittente.
071600120321     D* Returns:        Esito.
071700120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
071800120321     D*                          segni mittente.
071900120321     D* Parameter:      priAbi => ABI assegno.
072000120321     D* Parameter:      priCab => CAB assegno.
072100120321     D*--------------------------------------------------
072200120321     D SetIdAssegnoMittente...
072300120321     D                 PR            10I 0
072400120321     D  priNumeroAssegno...
072500120321     D                                     LIKE(caNumO74)
072600120321     D  priAbi                             LIKE(caAbiO74)
072700120321     D  priCab                             LIKE(caCabO74)
072800130513
072900130513     D*--------------------------------------------------
073000130513     D* Procedure name: GetFilUrl
073100130513     D* Purpose:        Restituisce l'URL della filiale.
073200130513     D* Returns:        URL filiale.
073300130513     D* Parameter:      piFiliale => ID filiale
073400130513     D*--------------------------------------------------
073500130513     D GetFilUrl       PR           256A
073600130513     D  piFiliale                     3P 0 CONST
073700130513
073800130513
073900140527       //--------------------------------------------------------------
074000140527       //?Definizione prototipi.
074100140527       //--------------------------------------------------------------
074200140527      /copy gaitrasrc/srcprotopr,tis778R
074300140529      /copy gaitrasrc/srcconst,tis778R
074400170314      /copy gaitrasrc/srcprotopr,TRUL28R0
074500170315      /copy gaitrasrc/srcconst,fnlgw00r
074600140527       //--------------------------------------------------------------
074700140527       //?Definizione parametri programma.
074800140527       //--------------------------------------------------------------
074900140529     d tis778_GetBolla...
075000140527     d                 PI
075100140527     d prmRqsOpCode...
075200140527     d                               10I 0 CONST
075300140527     d prmRpyOpCode...
075400140527     d                               10I 0
075500140527     d prmRpyIdMsg...
075600140527     d                               10I 0
075700140527     d prmRqsFormato...
075800140527     d                               10A   CONST
075900140527     d prmRqsData...
076000140527     d                            32767A   OPTIONS(*VARSIZE)
076100140527     d prmRqsDataSize...
076200140527     d                               10I 0 CONST
076300140527     d prmRpyFormato...
076400140527     d                               10A   CONST
076500140527     d prmRpyData...
076600140527     d                            32767A   OPTIONS(*VARSIZE)
076700140527     d prmRpyDataSize...
076800140527     d                               10I 0 CONST
076900140527     d prmRpyFormMsg...
077000140527     d                               10A   CONST OPTIONS(*NOPASS)
077100140527     d prmRpyMessage...
077200140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
077300140527     d prmRpyMsgSize...
077400140527     d                               10I 0 CONST OPTIONS(*NOPASS)
077500080131
077600000000     i*--------------------------------------------------------------------------------------------*
077700000000     i* Input
077800000000     i*--------------------------------------------------------------------------------------------*
077900000000     i*---
078000000000     i* Indicatori lettura formati record -TITAS-
078100000000     i*---
078200000000     ititas000      01
078300000000     ititas010      02
078400000000     ititasp00      03
078500000000     c*--------------------------------------------------------------------------------------------*
078600000000     c* Main lines
078700000000     c*--------------------------------------------------------------------------------------------*
078800000000     c*
078900000000     c* operazioni iniziali -da fare sempre-
079000000000     c                   EXSR      rutinz
079100000000     c*
079200000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
079300000000if  1c                   IF        esito <> '1'                                 *no errori
079400100219     c*
079500100219     c                   MOVEL     dslnpi74      kaslnp
079600100219     c                   MOVEL     dsnrsi74      kasnrs
079700100219     c                   MOVEL     dsnspi74      kasnsp
079800100219     c*
079900111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
080000111103     c* restituisco errore.
080100111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
080200111103     c* e se non la trovo provo con l'anno precedente.
080300160318    1c                   IF        aasI74 <> *ZERO
080400100219     c                   Z-ADD     aasI74        kasaas
080500100219     c     keytas        SETLL     titas30c                               99
080600160318    2c                   IF        NOT *IN99
080700160318
080800160318     c     keytas        chain     fnblp01l
080900160318    3c                   if        %found(fnblp01l) and arbft1=' '
081000160318     c                   MOVEL     'P'           esito                          *bolla in part
081100160318   x3c                   else
081200111103     c                   EVAL      esito = '1'
081300160318    3c                   ENDIF
081400160318    2c                   ENDIF
081500160318
081600160318   x1c                   ELSE
081700000000     c                   Z-ADD     anncor        kasaas
081800000000     c     keytas        SETLL     titas30c                               99
081900000000if  2c                   IF        NOT *in99                                    *non trovato
082000160318
082100160318     c     keytas        chain     fnblp01l
082200160318    3c                   if        %found(fnblp01l) and arbft1=' '
082300160318     c                   MOVEL     'P'           esito                          *bolla in part
082400160318   x3c                   else
082500160318
082600160318     C* verifico se in partenza: se non è in partenza allora verifico anno precedente
082700000000     c                   Z-ADD     annpre        kasaas
082800000000     c     keytas        SETLL     titas30c                               99
082900160318if  4c                   IF        NOT *in99                                    *non trovato
083000000000     c                   MOVEL     '1'           esito                          *errore
083100160318e   4c                   ENDIF
083200160318e   3c                   ENDIF
083300000000e   2c                   ENDIF
083400160318    1c                   ENDIF
083500160127     c
083600160314    2c                   if        esito = '1'
083700140617     c                   eval      widmsg = 'TIS0564'
083800140616     c                   eval      widcampo = 'NSPEDIZI74'
083900140616     c                   clear                   wparm
084000140616     c                   exsr      messaggi
084100160314    2c                   endif
084200160318     c
084300141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
084400141126     c                   eval      kasaasm = kasaas
084500141126     c                   eval      kaslnpm = kaslnp
084600141126     c                   eval      kasnrsm = kasnrs
084700141126     c                   eval      kasnspm = kasnsp
084800141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
084900140715      * ultima figlia per forzare i dati in output
085000160318     c                   if        esito<>'P'
085100140714     c                   exsr      cercafiglia
085200160318     c                   else
085300160318     c                   exsr      cercamamma
085400160318     c                   endif
085500000000     c*
085600160127     c* Se bolla in FNBLP in partenza eseguo routine a parte
085700160314    2c                   if        esito='P'
085800160127     c
085900160127     c* imposta i campi della DS di Output dagli eventi
086000160127     c                   EXSR      impdsoevb
086100160127     c
086200160127     c* imposta i campi del C%Ass dal fiar9
086300160127     c                   EXSR      impdsoar9
086400160127     c
086500160127     c* impostazione campi in DS di output
086600160309     c                   exsr      impds_soloblp
086700160309     c                   exsr      impdsoarb
086800160309     c
086900160309     c* imposta i campi della DS di Output dalle note
087000160309     c                   EXSR      impdsonot
087100160314    2c                   endif
087200160127     c
087300000000     c* se trovata, legge la bolla
087400160127if  2c                   IF        esito <> '1'  and esito<>'P'
087500000000     C                   SETOFF                                       010203
087600140715     c     keytas        SETLL     titas30c
087700000000     c     keytas        READE     titas30c                               99    *no errore
087800000000if  3c                   IF        *in99                                        *non trovato
087900000000     c                   MOVEL     '1'           esito                          *errore
088000140617     c                   eval      widmsg = 'TIS0734'
088100140616     c                   eval      widcampo = 'NSPEDIZI74'
088200140616     c                   clear                   wparm
088300140616     c                   exsr      messaggi
088400000000x   3c                   ELSE                                                   *trovata
088500000000if  4c                   IF        *in01                                        *lettura titas000
088600000000     c                   EVAL      titasdssav = titasds
088700000000e   4c                   ENDIF
088800000000if  4c                   IF        *in02                                        *lettura titas010
088900000000     c                   EVAL      titasdssav = titasds
089000000000e   4c                   ENDIF
089100000000if  4c                   IF        *in03                                        *lettura titasp00
089200000000     c                   EVAL      titasdssav = titasds
089300000000e   4c                   ENDIF
089400141117      * se trovo la figlia ma ancora non è su titas
089500150617      * Se la bolla non è legata, nei campi della "figlia" ovvero F_  ci sono
089600150617      *  i campi della bolla originale passata
089700141117     c                   if        keyforzata <> *blank and
089800141117     c                             f_tasnsp = 0
089900141117     c                   eval      titasdssavf = titasdssav
090000141120     c                   else
090100141120     c                   clear                   keyforzata
090200141117     c                   endif
090300000000e   3c                   ENDIF
090400000000     c*
090500000000     c* controlla validità bolla
090600000000     c                   EXSR      chktas
090700000000     c*
090800030130     c* imposta i campi della DS di Output
090900000000if  3c                   IF        esito <> '1'                                 *no errore
091000140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
091100140714      * spedizione richiesta a video sempre
091200100225     c*
091300140716     c                   EVAL      dstaslnp = s_taslnp
091400140716     c                   EVAL      dstasnrs = s_tasnrs
091500140716     c                   EVAL      dstasnsp = s_tasnsp
091600140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
091700140723     c* chain ar5 con bolla richiesta a video
091800160127     c                   Z-ADD     s_tasaas      kr5aas
091900140723     c                   Z-ADD     s_taslnp      kr5lnp
092000140723     c                   Z-ADD     s_tasnrs      kr5nrs
092100140723     c                   Z-ADD     s_tasnsp      kr5nsp
092200100225     c                   MOVEL     'GEN'         kr5trd
092300100225     c     keyar5        CHAIN     fiar531c
092400100225     C                   IF        %FOUND
092500100225     C                   EVAL      DAR5GEN = AR5Uni
092600100225     C                   ELSE
092700100225     C                   RESET                   DAR5GEN
092800100225     C                   ENDIF
092900000000     c*
093000000000     c* imposta i campi della DS di Output dalle bolle
093100000000     c                   EXSR      impdsotas
093200000000     c*
093300000000     c* imposta i campi della DS di Output dagli eventi
093400000000     c                   EXSR      impdsoevb
093500000000     c*
093600000000     c* imposta i campi della DS di Output dal contrassegno
093700000000     c                   EXSR      impdsocsb
093800000000     c*
093900000000     c* imposta i campi della DS di Output dalla giacenza
094000000000     c                   EXSR      impdsogcp
094100000000     c*
094200000000     c* imposta i campi della DS di Output dalle note
094300000000     c                   EXSR      impdsonot
094400040330     c*
094500040330     c* imposta i campi della DS di Output del destinatario.
094600040330     c                   EXSR      ImpDSDest
094700010716     C*
094800160323     c* imposta i campi della DS di Output dalla firma DPD/EEX
094900010716     c                   EXSR      ImpdsoDPD
095000040913     C*
095100040913     c* imposta i campi della DS di Output di addebito immagine LDV.
095200040913     c                   EXSR      AddebitoLDV
095300000000e   3c                   ENDIF
095400000000e   2c                   ENDIF
095500140613e   1c                   ENDIF
095600160314     c
095700140919      *riempie campi con riferimenti alla consegna
095800160314      *  non per bolla ancora in partenza
095900160314     c                   if        esito <> '1'  and esito <>'P'
096000160314     c
096100140919     c                   if        f_tasdcm > 0
096200140919     c                   movel     f_tasdcm      wdat
096300140919     c                   exsr      edtdat
096400140919     c                   eval      dtconmeo74  = wdate
096500140919     c                   MOVEL     f_tashmc      wora
096600140919     c                   EXSR      edtora
096700140919     c                   eval      orconmeO74  =  worae
096800140919     c                   eval      flgbolcO74  =  'S'
096900140919     c                   eval      flgboldO74  =  'N'
097000141014      *firmatario distinta PDA
097100141014     c                   clear                   firmato74
097200141015     c                   if        gen§ar5pdaco <> 'NO'
097300141014     c                   exsr      repfirmat
097400141014     c                   else
097500141014     c                   if        f_tasgma <> *blank
097600141015     C                   EVAL      tblKey = f_tasgma
097700141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
097800141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
097900141014     C                             :rpyOpCode:rpyEsito)
098000141014     C                   IF        rpyOpCode = 'FOUND'
098100141014     c                   if        §7r2fi = 'S'
098200141014     c                   exsr      repfirmat
098300141014     c                   endif
098400141014     c                   endif
098500141014     c                   endif
098600141014     c                   endif
098700141014      *else consegna
098800140919     c                   else
098900140919     c                   eval      flgbolcO74  =  'N'
099000140919     c                   if        F_tasndc > 0 and f_tasddc > 0
099100141119     c                             and f_tasddc <= datcor
099200140919     c                   eval      flgboldo74 = 'S'
099300140919     c                   else
099400140919     c                   eval      flgboldO74  =  'N'
099500140919     c                   endif
099600140919     c                   endif
099700150617     c
099800140613if  3c                   IF        esito <> '1'                                 *no errore
099900150615     c***********        IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
100000150615     c************                 and cev(49) = ' ' and cev(50) = ' ' or
100100150615     c
100200150615     c                   if        f_tasdcm = 0 and keyforzata = *blank
100300130312     c                   exsr      srarb
100400140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
100500140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
100600140605     c                   exsr      impdsoarb
100700140604     c                   endif
100800140613     c                   endif
100900140613     c                   endif
101000141016     c                   endif
101100000000     c*
101200140530     C                   exsr      routend
101300141014      *__________________________________________________________________
101400141014     c     repfirmat     begsr
101500141014      *__________________________________________________________________
101600141014     c                   Z-ADD     f_tasaas      ka4aas
101700141014     c                   Z-ADD     f_taslnp      ka4lnp
101800141014     c                   Z-ADD     f_tasnrs      ka4nrs
101900141014     c                   Z-ADD     f_tasnsp      ka4nsp
102000141014     c                   MOVEL     '1'           ka4trc
102100141014     c     keyta4        CHAIN     tita430c                           99
102200141014if  1c                   IF        NOT *in99
102300141014     c                   MOVEL     ta4not        firmato74
102400141014e   1c                   ENDIF
102500141014     c                   endsr
102600141015      *__________________________________________________________________
102700141015     c     repfirmatARB  begsr
102800141015      *__________________________________________________________________
102900141015      *firmatario da arb
103000141015     c                   Z-ADD     arbaas        ka4aas
103100141015     c                   Z-ADD     arblnp        ka4lnp
103200141015     c                   Z-ADD     arbnrs        ka4nrs
103300141015     c                   Z-ADD     arbnsp        ka4nsp
103400141015     c                   MOVEL     '1'           ka4trc
103500141015     c     keyta4        CHAIN     fiar401l                           99
103600141015if  1c                   IF        NOT *in99
103700141015     c                   MOVEL     ar4not        firmato74
103800141015e   1c                   ENDIF
103900141015     c                   endsr
104000000000     c*--------------------------------------------------------------------------------------------*
104100000000     c* controlla validità bolla
104200000000     c*--------------------------------------------------------------------------------------------*
104300000000     c     chktas        BEGSR
104400000000     c*
104500000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
104600000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
104700000000     c                   Z-ADD     s_tasmgs      dsmgsB
104800000000if  1c                   IF        dsdatB < datpre
104900000000     c                   MOVEL     '1'           esito                          *errore
105000000000e   1c                   ENDIF
105100030130     c* se l'esito è positivo, salva alcuni dati della bolla
105200150617     c*********          EVAL      savtsp = f_tastsp
105300000000     c*
105400140718     C                   EVAL      wOrg = f_tasLna
105500051222     C                   EXSR      setLnaItaEst
105600051222     C
105700000000     c                   ENDSR
105800000000     c*--------------------------------------------------------------------------------------------*
105900000000     c* imposta i campi della DS di Output dalle bolle e files correlati
106000000000     c*--------------------------------------------------------------------------------------------*
106100140604     c     impdsotas     BEGSR
106200170301     c                   clear                   dcli
106300140604     c*
106400140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
106500140604     c                   EVAL      lnaDescO74 = sodes(i)
106600140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
106700150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
106800150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
106900150116     c                   MOVE      s_tasmgs      aasmgsO74
107000150116      *
107100140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
107200140718     c                   MOVEL     f_taslna      lineaaro74
107300140604     c*
107400140718     c                   MOVEL     f_tastbl      a1
107500140718     c                   MOVEL     f_tastbl      tpporto74
107600140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
107700140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
107800140604x   1c                   ELSE                                                   *Pagamento del
107900140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
108000140604e   1c                   ENDIF
108100140716     c                   MOVEL     f_tastsp      tpservio74
108200140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
108300140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
108400140604     C                             rpyOpCode:rpyEsito)
108500140604     C                   EVAL      corro74 = §5EDes
108600140604     c*
108700140716     c                   EVAL      destino74 = f_tasrsd
108800140716     c                   EVAL      desindo74 = f_tasind
108900140716     c                   EVAL      descapo74 = f_tascad
109000140716     c                   EVAL      desloco74 = f_taslod
109100140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
109200150326if  1c                   IF        f_tasnzd  = *blanks
109300150326     c                   EVAL      desnazo74 = f_tasprd
109400150326x   1c                   ELSE
109500150326     c***                EVAL      desnazo74 = f_tasnzd
109600150326     c***                EVAL      desnzdo74 = f_tasnzd
109700150326     c* 26/03/15: Attiviamo le specifiche per provincia export
109800150326     c                   EVAL      desnazo74 = f_tasprd
109900150326     c                   EVAL      desnzdo74 = f_tasnzd
110000150326e   1c                   ENDIF
110100140604     c*
110200140716     c                   EVAL      collio74  = f_tasncl
110300140716     c                   EVAL      pesoo74   = f_taspkf
110400140716     c                   EVAL      volumeo74 = f_tasvlf
110500160122     c***                EVAL      naturao74 = f_tasnas
110600140604     c*
110700140716     c                   EVAL      fatnumo74  = f_tasnft
110800140716     c                   MOVEL     f_tasdft      wdat
110900140604     c                   EXSR      edtdat
111000140604     c                   EVAL      fatdatao74 = wdate
111100140604     c*
111200140716     c                   EVAL      disnumo74  = f_tasndc
111300140716c    c                   MOVEL     f_tasddc      wdat
111400140604     c                   EXSR      edtdat
111500140604     c                   EVAL      disdatao74 = wdate
111600140604     **?Immagine lettera di vettura.
111700140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
111800140604     C                   IF        LDVO74 <> 'N'
111900140604     c                   CLEAR                   dtasflo
112000140716     c                   MOVEL     f_tasflo      dtasflo
112100140604     c                   IF        §floiml <> *BLANK
112200140604     c                   EVAL      ldvo74 = 'S'
112300140604     C                   EVAL      SpImLDVO74 =
112400140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
112500140716     C                             %EDITC(f_taslnp:'X') +
112600140716     C                             %EDITC(f_tasnrs:'X') +
112700140716     C                             %EDITC(f_tasnsp:'X')
112800140604     **?Reperisco il check code della spedizione POD
112900140604     C                   EVAL      PChkCdeO74 =
113000140604     C                             GetSpeChkCde(
113100140716     C                             %EDITC(f_tasaas:'X'):
113200140716     C                             %EDITC(f_taslnp:'X') +
113300140716     C                             %EDITC(f_tasnrs:'X') +
113400140716     C                             %EDITC(f_tasnsp:'X'):
113500140604     C                             ChkCode:nEsito)
113600140604     C                   ENDIF
113700140604     C                   ENDIF
113800140604     c*
113900170310     c                   MOVE      f_tasccm      alfa4
114000170301     c                   clear                   mito74
114100170310     c                   clear                   solomito74
114200170310if  1c                   IF        alfa4 <> '8888'                               *codificato
114300170301
114400170301     c* Verifico da tabella CLI quale ragione sociale mittente devo visualizzare
114500170301     c                   exsr      Tabcli
114600170301
114700170301x   1c                   ELSE                                                   *non codificato
114800170301     c                   Z-ADD     f_tasaas      kaaaas
114900170301     c                   Z-ADD     f_taslnp      kaalnp
115000170301     c                   Z-ADD     f_tasnrs      kaanrs
115100170301     c                   Z-ADD     f_tasnsp      kaansp
115200170301     c                   MOVEL     'M'           kaatrc
115300170301     c     keytaa        CHAIN     titaa30c                           99
115400170301if  2c                   IF        NOT *in99
115500170301     c                   EVAL      mito74    = taarsc
115600170301     c                   EVAL      mitindo74 = taaind
115700170301     c                   EVAL      mitcapo74 = taacap
115800170301     c                   EVAL      mitloco74 = taaloc
115900170301if  3c                   IF        taanaz = *blanks
116000170301     c                   EVAL      mitnazo74 = taaprv
116100170301x   3c                   ELSE
116200170301     c                   EVAL      mitnazo74 = taanaz
116300170301e   3c                   ENDIF
116400170301e   2c                   ENDIF
116500170301e   1c                   ENDIF
116600170301     c
116700140604     c* 2a ragione sociale destinatario.
116800140604     c                   CLEAR                   tita4000
116900140604     c                   CLEAR                   tita4p00
117000140716     c                   Z-ADD     f_tasaas      ka4aas
117100140716     c                   Z-ADD     f_taslnp      ka4lnp
117200140716     c                   Z-ADD     f_tasnrs      ka4nrs
117300140716     c                   Z-ADD     f_tasnsp      ka4nsp
117400140604     c                   MOVEL     'D'           ka4trc
117500140604     c     keyta4        CHAIN     tita430c                           99
117600140604if  1c                   IF        NOT *in99
117700140604     c                   MOVEL     ta4not        desti2O74
117800140604e   1c                   ENDIF
117900140604     c* rif partner estero
118000140604     c                   CLEAR                   tita4000
118100140604     c                   CLEAR                   tita4p00
118200140604     c                   CLEAR                   dsbl4e
118300140716     c                   Z-ADD     f_tasaas      ka4aas
118400140716     c                   Z-ADD     f_taslnp      ka4lnp
118500140716     c                   Z-ADD     f_tasnrs      ka4nrs
118600140716     c                   Z-ADD     f_tasnsp      ka4nsp
118700140604     c                   MOVEL     'E'           ka4trc
118800140604     c     keyta4        CHAIN     tita430c                           99
118900140604if  1c                   IF        NOT *in99
119000140604     c                   MOVEL     ta4not        dsbl4e
119100140604e   1c                   ENDIF
119200140604     c
119300140604     c                   CLEAR                   tita4000
119400140604     c                   CLEAR                   tita4p00
119500140604     c                   CLEAR                   dta4a
119600140716     c                   Z-ADD     f_tasaas      ka4aas
119700140716     c                   Z-ADD     f_taslnp      ka4lnp
119800140716     c                   Z-ADD     f_tasnrs      ka4nrs
119900140716     c                   Z-ADD     f_tasnsp      ka4nsp
120000140604     c                   MOVEL     'A'           ka4trc
120100140604     c     keyta4        CHAIN     tita430c                           99
120200140604if  1c                   IF        NOT *in99
120300140604     c                   MOVEL     ta4not        dta4a
120400140604e   1c                   ENDIF
120500140604     ** Riferimento mittente numerico (c'è sempre).
120600140716     C                   EVAL      rifero74 = f_tasrmn
120700140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
120800140604     C                   IF        §ta4arma <> *BLANK AND
120900140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
121000140604     C                   EVAL      rifero74a = §ta4arma
121100140604     C                   ENDIF
121200160122     c** Natura merce
121300160122     c                   EVAL      naturao74 = §ta4anas
121400140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
121500140604     C                   IF        §b4erp <> *BLANK AND
121600140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
121700140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
121800140604     C                   EVAL      rpto74 = §b4erp
121900140604     C                   ENDIF
122000140604
122100140604     **?Reperisco il check code della spedizione.
122200140604     C                   EVAL      SChkCdeO74 =
122300140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
122400140604     C                             NSpedizI74:ChkCode:nEsito)
122500140604
122600140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
122700140604     C                   IF        KSCI74 <> *BLANK
122800140604     c                   SETOFF                                           98
122900140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
123000140604     C                   RESET                   tis7700i0
123100141126     C                   EVAL      tis7700i0.aas = kasaasm
123200141126     C                   EVAL      tis7700i0.lnp = kaslnpm
123300141126     C                   EVAL      tis7700i0.nrs = kasnrsm
123400141126     C                   EVAL      tis7700i0.nsp = kasnspm
123500140604     C                   EVAL      tis7700i0.ksu = kscI74
123600140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
123700140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
123800140604     C                             : rpyEsito
123900140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
124000140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
124100140604     C                             )
124200140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
124300140604     C                             tis7700o0.bollValida = *ON)
124400140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
124500140604if  2c                   IF        NOT *in98                                      *non trovato
124600140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
124700140604     C                   reset                   TIS778DSO
124800160818     C******             EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
124900160818     c                   eval      esito='1'
125000160818     c                   eval      widmsg = 'TIS0849'
125100160818     c                   eval      widcampo = 'NSPEDIZI74'
125200160818     c                   clear                   wparm
125300160818     c                   exsr      messaggi
125400160818     c                   exsr      routend
125500160818     C******             RETURN
125600140604     C                   ENDIF
125700140604e   2c                   ENDIF
125800140604     C                   ENDIF
125900140604     c*
126000140604     ** Importo assicurato.
126100140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
126200140716     C                   EVAL      vasO74 = f_tasVas
126300141015     C                   EVAL      iasO74 = f_tasIas
126400140604     C                   ENDIF
126500140604     **
126600140604     c                   ENDSR
126700170301     c*--------------------------------------------------------------------------------------------*
126800170301     c* imposto campi del mittente da tabella CLI
126900170301     c*--------------------------------------------------------------------------------------------*
127000170301     c     tabcli        BEGSR
127100170310     c                   clear                   wusarmo           1
127200170301     C                   RESET                   tibs02ds
127300170301     C                   EVAL      t02Cod = 'CLI'
127400170301     C                   EVAL      t02Ke1 = %editc(ccmo74:'X')
127500170301     C                   EXSR      getTntbe00f
127600170310     c
127700170310     c* e mi hanno passarto una versione A o B --> significa che NON devo sostituire RSM
127800170313     c                   if        VERSIONI74<'C'
127900170310     c                   eval      t02err='E'
128000170310     c                   endif
128100170301
128200170301    0c                   IF        t02Err=' ' and t02atb=' '
128300170310    c
128400170310     c                   eval      solomito74=§CLINOIMTT
128500170310     c
128600170301     c* vedo se presente mittente originale
128700170301    1c                   if        §CLIMITOR='S'
128800170301     c* mittente originale da ARB
128900170301    2c                   if        wbol='ARB'
129000170301     c                   eval      mito74=arbrmo
129100170310
129200170310     c                   if        arbrmo<>*blanks
129300170310     c                   eval      wusarmo='S'
129400170310     c                   endif
129500170301   x2c                   else
129600170301     c                   Z-ADD     f_tasaas      kaaaas
129700170301     c                   Z-ADD     f_taslnp      kaalnp
129800170301     c                   Z-ADD     f_tasnrs      kaanrs
129900170301     c                   Z-ADD     f_tasnsp      kaansp
130000170301     c                   MOVEL     'O'           kaatrc
130100170301     c     keytaa        CHAIN     titaa30c
130200170301if  3c                   IF        %found(titaa30c)
130300170301     c                   eval      mito74=taarsc
130400170310     c                   eval      wusarmo='S'
130500170301    3c                   endif
130600170301    2c                   endif
130700170301    1c                   endif
130800170301
130900170301    1c                   if        mito74=*blanks and §CLITFNTC='S'
131000170301     c                   eval      k_NTCAPL =  'C'
131100170301     c                   eval      k_NTCNK1 =  '0151' + %editc(ccmo74:'X')
131200170301     c                   eval      k_NTCNK2 =  *BLANK
131300170301     c                   eval      k_NTCTNT =  'AM'
131400170301     c     ktfntc        chain     tfntc01l
131500170301    2c                   if        %found(tfntc01l)
131600170301     c                   eval      mito74=ntcrnt
131700170301    2c                   endif
131800170301    1c                   endif
131900170301    0c                   endif
132000170301     c
132100170301     c* questo pezzo lo faccio solo per TITAS
132200170310    1c                   if        wbol<>'ARB'
132300170301     c                   CLEAR                   bs69ds
132400170301     c                   CLEAR                   acods
132500170301     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
132600170301     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
132700170301     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
132800170301     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
132900170301     c                   PARM                    bs69ds
133000170301     c                   PARM                    acods
133100170301     c                   PARM                    indds
133200170301     c                   PARM                    clpds
133300170301     c                   PARM                    clsds
133400170310
133500170301if  2c                   IF        o69err <> '1'                                *no errore
133600170310
133700170310    1c                   if        mito74=*blanks
133800170301     c                   EVAL      mito74    = acorag
133900170310     c                   endif
134000170301     c                   EVAL      mitindo74 = indvia
134100170301     c                   EVAL      mitloco74 = indcit
134200170301     c                   MOVEL     indsta        a3
134300170301if  3c                   IF        a3 = *blanks
134400170301     c                   EVAL      mitnazo74 = indprv
134500170301x   3c                   ELSE
134600170301     c                   EVAL      mitnazo74 = a3
134700170301e   3c                   ENDIF
134800170301if  3c                   IF        indcae <> *blanks
134900170301     c                   EVAL      mitcapo74 = indcae
135000170301X   3c                   ELSE
135100170301     c                   MOVEL     indcap        a5
135200170301     c                   EVAL      mitcapo74 = a5
135300170301e   3c                   ENDIF
135400170301e   2c                   ENDIF
135500170301e   1c                   ENDIF
135600170310     c
135700170310     c* questo pezzo lo faccio solo per blp/arb
135800170310    1c                   if        mito74=*blanks and wbol= 'ARB'
135900170310     c                   EVAL      mito74    = arbrsm
136000170310     c                   endif
136100170310     c
136200170301     c                   ENDSR
136300000000     c*--------------------------------------------------------------------------------------------*
136400000000     c* imposta i campi della DS di Output dagli eventi
136500000000     c*--------------------------------------------------------------------------------------------*
136600000000     c     impdsoevb     BEGSR
136700000000     c*
136800000000     c* azzera le variabili di lavoro
136900000000     c                   CLEAR                   tote                           *contatore eventi
137000000000     c                   CLEAR                   seql                           *sequenza legami
137100000000     c                   CLEAR                   spsort                         *per ordinamento
137200000000     c                   CLEAR                   spseql                         *di memorizzazione
137300000000     c                   CLEAR                   spseqe
137400000000     c                   CLEAR                   spdevhev
137500000000     c                   CLEAR                   spdeve
137600000000     c                   CLEAR                   spheve
137700000000     c                   CLEAR                   spfle
137800000000     c                   CLEAR                   spdev
137900000000     c                   CLEAR                   spcev
138000130312     c                   CLEAR                   spspe
138100150618     c                   CLEAR                   ds7o
138200000000     c*
138300000000     c* imposta la sequenza legami per la bolla di Input
138400000000     c                   Z-ADD     1             seql                           *primo legaqme
138500000000     c*---
138600000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
138700000000     c*---
138800160127     c****
138900160127     c* Bolla presa in carico se ancora da partire
139000160420     c**                 if        esito='P'
139100160420     c**                 MOVEL     '700'         wcev                           *causale evento
139200160420     c**                 EXSR      deccevsta
139300160420if  4c**                 IF        werr <> '1'                                  *no errore
139400160420     c**                 ADD       1             tote
139500160420     c**                 MOVEL     wcdex         smdev(tote)
139600160420     c**                 exsr      evbinca
139700160420     c**                 endif
139800160420     c**                 endif
139900000000     c***
140000000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
140100000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
140200000000     c*   si trasforma nella consegna anomala della mamma
140300000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
140400000000     c***
140500000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
140600000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
140700160318     c
140800160318    0c                   if        esito<>'P'
140900000000     c                   EVAL      kblaan = s_tasaas
141000000000     c                   EVAL      kbllpn = s_taslnp
141100000000     c                   EVAL      kblnrn = s_tasnrs
141200000000     c                   EVAL      kblnsn = s_tasnsp
141300000000     c     keylbl        CHAIN     fnlbl01l                           99
141400000000if  1c                   IF        NOT *in99                                    *ha la mamma
141500000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
141600000000     c                   EVAL      kasaas = lblaap
141700000000     c                   EVAL      kaslnp = lbllpp
141800000000     c                   EVAL      kasnrs = lblnrp
141900000000     c                   EVAL      kasnsp = lblnsp
142000000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
142100000000if  2c                   IF        NOT *in99 AND
142200000000     c                             tascca <> *blanks                            *ha cons.anomala
142300060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
142400060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
142500060622     C                             :rpyEsito)
142600060622if  3c*                  IF        *in98                                        *trovata
142700060626     C                   IF        rpyOpCode = 'FOUND'
142800041222     **?Consegna anomala, POD non visualizzabile.
142900060622     C*                  IF        sapod(jj) = 'N'
143000060622     C                   IF        §7OPODImg = 'N'
143100041222     C                   EVAL      LDVO74 = 'N'
143200041222     C                   ENDIF
143300060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
143400060622     C                   IF        §7oinc <> *blanks                            *ok per internet
143500000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
143600041222e   3c                   ENDIF
143700000000e   3c                   ENDIF
143800000000e   2c                   ENDIF
143900000000e   1c                   ENDIF
144000160318e   0c                   ENDIF
144100000000     c*
144200030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
144300000000     c                   ADD       1             tote
144400060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
144500060622     c                   MOVEL     §7odei        smdev(tote)
144600030203x   3c                   ELSE                                                   *evento telef.P.O.
144700060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
144800030203e   3c                   ENDIF
144900030203e   2c                   ENDIF
145000030203     c*
145100030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
145200060622     c                   MOVEL     §7ocev        wcev                           *causale evento
145300000000     c                   EXSR      evbrit                                       *imposta evento
145400030203x   2c                   ELSE                                                   *NO sost.ritiro
145500160318     c
145600160408     c* per bolle in partenza devo impostare la data ritiro solo se spuntato in entrata
145700160408     c***                if        esito<>'P'
145800160413     c                   if        esito= 'P'
145900160413     c                   if        arbdse>0
146000160408     c                   eval      s_tasdrt=arbdrt
146100160413     c                   else
146200160413     c                   eval      s_tasdrt=00000000
146300160408     c                   endif
146400160413     c                   endif
146500160318     c
146600030203if  3c                   IF        s_tasdrt > *zeros
146700000000     c                   MOVEL     '701'         wcev                           *causale evento
146800000000     c                   EXSR      deccevsta
146900030203if  4c                   IF        werr <> '1'                                  *no errore
147000000000     c                   ADD       1             tote
147100030130     c                   MOVEL     wcdex         smdev(tote)
147200000000     c                   EXSR      evbrit                                       *imposta evento
147300030203e   4c                   ENDIF
147400030203e   3c                   ENDIF
147500030203e   2c                   ENDIF
147600160408e   2c***                ENDIF
147700140710     ** ID bolla madre.                                                          dubito figlia
147800160318     c                   if        esito='P'
147900160318     c                   EVAL      waas = arbaas
148000160318     c                   EVAL      wlnp = arblnp
148100160318     c                   EVAL      wnrs = arbnrs
148200160318     c                   EVAL      wnsp = arbnsp
148300160318     c                   else
148400160318     c
148500160318     c* questi dati per le bolle solo in partenza (non legate)
148600160318     c*  non li devo caricare
148700080131     c                   EVAL      waas = s_tasaas
148800080131     c                   EVAL      wlnp = s_taslnp
148900080131     c                   EVAL      wnrs = s_tasnrs
149000080131     c                   EVAL      wnsp = s_tasnsp
149100000000     c* Partenza merce
149200000000     c                   EVAL      lduc = s_tasduc                              *data partenza
149300000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
149400000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
149500000000     c                   EXSR      evbpar
149600000000     c* Arrivo merce
149700110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
149800110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
149900140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
150000140723     c                             gen§ar5ArrDt <> *ZERO
150100140723     c                   MONITOR
150200140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
150300140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
150400140723     c                   ON-ERROR
150500140723     c                   ENDMON
150600140723     c                   ENDIF
150700100225     c                   EVAL      llna = s_taslna                              *linea arrivo
150800150617     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
150900090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
151000090826     c                   EXSR      evbarr
151100000000     c*
151200000000     c* Consegna merce
151300000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
151400000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
151500000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
151600000000     c                   EVAL      llna = s_taslna                              *linea arrivo
151700150618     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
151800000000     c                   EXSR      evbcon
151900000000     c* Lettera anomalia
152000000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
152100000000     c                   EXSR      memdct
152200000000e   1c                   ENDIF
152300000000     c*---
152400000000     c* memorizza gli eventi dal file eventi della bolla di input
152500000000     c*---
152600160406     c* devo reimpostare la linea di arrivo della bolla richiesta, perchè potrei
152700150618     c*  aver chainato una bolla "mamma" ed aver sporcato il campo
152800150618     c                   eval      taslna=s_taslna
152900000000     c                   EXSR      memevb
153000000000     c*---
153100000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
153200000000     c*---
153300000000     c                   EXSR      elaevelbl
153400160318     c                   endif
153500000000     c*
153600000000     c* imposta contatore eventi da visualizzare nella DS di Output
153700000000     c                   EVAL      cnteveo74 = tote
153800000000     c*---
153900000000     c* ordina gli eventi per sequenza legami / Data+ora evento
154000000000     c*---
154100000000     c* compone la schiera da ordinare
154200000000do  1c     1             DO        200           j
154300000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
154400000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
154500000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
154600000000     c                   Z-ADD     smi(j)        dsi                             -indice
154700000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
154800000000e   1c                   ENDDO
154900000000     c*
155000000000     c* ordina la schiera composta
155100000000     c                   SORTA     spsort
155200000000     c*
155300000000     c* ricompone le schiere da quella ordinata
155400000000     c                   Z-ADD     *zeros        jj
155500000000do  1c     1             DO        200           j
155600000000if  2c                   IF        spsort(j) >*zeros
155700000000     c                   ADD       1             jj
155800000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
155900000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
156000000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
156100000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
156200000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
156300000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
156400000000     c                   MOVEL     smheve(dsi)   spheve(jj)
156500000000     c                   MOVEL     smfle(dsi)    spfle(jj)
156600000000     c                   MOVEL     smdev(dsi)    spdev(jj)
156700000000     c                   MOVEL     smcev(dsi)    spcev(jj)
156800130312     c                   MOVEL     smspe(dsi)    spspe(jj)
156900000000e   2c                   ENDIF
157000000000e   1c                   ENDDO
157100101104     **
157200101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
157300101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
157400101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
157500101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
157600101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
157700101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
157800101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
157900101104     ** Ricordati che l'ordine per data e ora è discendente.
158000101104     C                   EVAL      j = 0
158100101104     C                   DOU       j = 0
158200101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
158300101104     C                                        : spcev : j+1)
158400101104     C                   IF        j = 0
158500101104     C                   LEAVE
158600101104     C                   ENDIF
158700101104     C                   IF        j > 1 AND
158800101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
158900101104     C                   EVAL      dev(1) = spdeve(j-1)
159000101104     C                   EVAL      hev(1) = spheve(j-1)
159100101104     C                   EVAL      fle(1) = spfle(j-1)
159200101104     C                   EVAL      eve(1) = spdev(j-1)
159300101104     C                   EVAL      cev(1) = spcev(j-1)
159400130312     C                   EVAL      spe(1) = spspe(j-1)
159500101104     C                   EVAL      spdeve(j-1) = spdeve(j)
159600101104     C                   EVAL      spheve(j-1) = spheve(j)
159700101104     C                   EVAL      spfle(j-1) = spfle(j)
159800101104     C                   EVAL      spdev(j-1) = spdev(j)
159900101104     C                   EVAL      spcev(j-1) = spcev(j)
160000130312     C                   EVAL      spspe(j-1) = spspe(j)
160100101104     C                   EVAL      spdeve(j) = dev(1)
160200101104     C                   EVAL      spheve(j) = hev(1)
160300101104     C                   EVAL      spfle(j) = fle(1)
160400101104     C                   EVAL      spdev(j) = eve(1)
160500101104     C                   EVAL      spcev(j) = cev(1)
160600130312     C                   EVAL      spspe(j) = spe(1)
160700101104     C                   ENDIF
160800101104     C                   ENDDO
160900000000     c*
161000120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
161100120831     c                   CLEAR                   cntEveO74
161200000000do  1c     1             DO        50            j
161300120831     c                   IF        spcev(j) = *BLANK
161400120831     c                   LEAVE
161500120831     c                   ENDIF
161600000000     c                   EVAL      dev(j) = spdeve(j)
161700000000     c                   EVAL      hev(j) = spheve(j)
161800000000     c                   EVAL      fle(j) = spfle(j)
161900041007     c                   EVAL      eve(j) = spdev(j)
162000041007     c                   EVAL      cev(j) = spcev(j)
162100130312     c                   EVAL      spe(j) = spspe(j)
162200101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
162300100909     c                   CLEAR                   hev(j)
162400100909     c                   ENDIF
162500120831     c                   EVAL      cntEveO74 += 1
162600000000e   1c                   ENDDO
162700000000     c*
162800000000     c                   ENDSR
162900000000     c*--------------------------------------------------------------------------------------------*
163000000000     c* memorizza gli eventi dal file eventi della bolla di input
163100000000     c*--------------------------------------------------------------------------------------------*
163200000000     c     memevb        BEGSR
163300150617     c* il programma usa il campo TASLNA per attribuire gli eventi della bolla
163400150617     c*  usa il TITAS che ha letto per ultimo che può essere quello della bolla  mamma
163500150617     c*  o della bolla figlia
163600150617     c*  per sapere se la bolla è italia o estera non devo usare l'ultima figlia
163700150617     c*  ma lna della bolla che sta leggendo in questo momento
163800150617     c                   Z-ADD     tasLna        worg
163900150617     c                   exsr      decorg
164000150617     c                   eval      wtasfl1 =  wofl1
164100000000     c***
164200000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
164300000000     c* . se SI lo aggiorna solo nella sequenza legame
164400000000     c*   se NO lo inserisce
164500000000     c***
164600000000     c                   Z-ADD     waas          kvbaas
164700000000     c                   Z-ADD     wlnp          kvblnp
164800000000     c                   Z-ADD     wnrs          kvbnrs
164900000000     c                   Z-ADD     wnsp          kvbnsp
165000000000     c     keyevb        SETLL     fnevb01l
165100000000     c     keyevb        READE     fnevb01l                               99
165200000000do  1c                   DOW       NOT *in99
165300000000if  2c                   IF        evbatb = *blanks                             *valido
165400140108     c                   MOVEL     evbdev        wdev
165500140108     c                   MOVEL     evbhev        whev
165600080131if  3c                   IF        evbDev > *zeros
165700000000     c                   MOVEL     evbcev        wcev
165800000000     c                   EXSR      deccevsta
165900070109     c                   EXSR      eventoAnnulla
166000000000if  4c                   IF        werr <> '1'                                  *no errore
166100000000     c*
166200000000     c* controlla se evento già inserito
166300000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
166400000000     c                   MOVEL     'S'           newevb                         *nuovo evento
166500000000do  5c     1             DO        200           j
166600000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
166700080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
166800000000     c                   MOVEL     'N'           newevb                         *già inserito
166900000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
167000150618     C                   IF        wtasfl1 = ESTERO
167100150618     c                   Z-ADD     tasLna        worg
167200150618     c                   else
167300150618     c                   Z-ADD     evbfle        worg
167400150618     c                   endif
167500000000     c                   EXSR      decorg
167600000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
167700080201     c                   MOVEL     wDev          wdat
167800080201     c                   EXSR      edtdat
167900080201     c                   MOVEL     wdate         smdeve(j)
168000080201     c                   MOVEL     wHev          wora
168100080201     c                   EXSR      edtora
168200080201     c                   MOVEL     worae         smheve(j)
168300130313     c                   MOVEL     dsevbspe      smspe(j)
168400000000     c                   LEAVE                                                  *uscita ciclo
168500000000e   6c                   ENDIF
168600000000e   5c                   ENDDO
168700000000     c*
168800000000     c* incrementa contatore eventi
168900000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
169000000000     c                   ADD       1             tote
169100000000     c* indice schiera
169200000000     c                   Z-ADD     tote          smi(tote)
169300000000     c* sequenza legame
169400000000     c                   Z-ADD     seql          smseql(tote)
169500000000     c* sequenza evento
169600000000     c                   Z-ADD     020           smseqe(tote)
169700000000     c* descrizione
169800030130     c                   MOVEL     wcdex         smdev(tote)
169900000000     c* data
170000080201     c                   MOVEL     wDev          wdat
170100000000     c                   EXSR      edtdat
170200000000     c                   MOVEL     wdate         smdeve(tote)
170300000000     c* ora
170400080201     c                   MOVEL     wHev          wora
170500000000     c                   EXSR      edtora
170600000000     c                   MOVEL     worae         smheve(tote)
170700000000     c* filiale
170800150617     C***                IF        wrkLnaItaEst = ESTERO
170900150617     C                   IF        wtasfl1 = ESTERO
171000140722     c                   Z-ADD     tasLna        worg
171100140722     c                   else
171200140722     c                   Z-ADD     evbfle        worg
171300140722     c                   endif
171400140722     c                   EXSR      decorg
171500140722     c                   MOVEL     wodes         smfle(tote)
171600000000     c* causale
171700000000     c                   MOVEL     evbcev        smcev(tote)
171800130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
171900130312     c* bolla legata
172000130312     c                   MOVEL     dsevbspe      smspe(tote)
172100000000     c* data+ora evento
172200080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
172300000000e   5c                   ENDIF
172400000000e   4c                   ENDIF
172500000000e   3c                   ENDIF
172600000000e   2c                   ENDIF
172700010717     c     keyevb        READE     fnevb01l                               99
172800000000e   1c                   ENDDO
172900000000     c*
173000000000     c                   ENDSR
173100000000     c*--------------------------------------------------------------------------------------------*
173200000000     c* memorizza gli "eventi fissi"
173300000000     c*--------------------------------------------------------------------------------------------*
173400000000     c     memevefix     BEGSR
173500080131     ** ID bolla in canna.
173600080131     c                   EVAL      waas = tasaas
173700080131     c                   EVAL      wlnp = taslnp
173800080131     c                   EVAL      wnrs = tasnrs
173900080131     c                   EVAL      wnsp = tasnsp
174000000000     c*
174100000000     c* Partenza merce
174200000000     c                   EVAL      lduc = tasduc                                *data partenza
174300000000     c                   EVAL      llnp = taslnp                                *filiale partenza
174400000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
174500000000     c                   EXSR      evbpar
174600000000     c* Consegna merce
174700000000     c                   EVAL      lcca = tascca                                *consegna anomala
174800000000     c                   EVAL      ldcm = tasdcm                                *data consegna
174900000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
175000000000     c                   EVAL      llna = taslna                                *linea arrivo
175100150618     c                   EVAL      ltfa = tastfa                                *linea arrivo
175200000000     c                   EXSR      evbcon
175300000000     c*
175400000000     c                   ENDSR
175500000000     c*--------------------------------------------------------------------------------------------*
175600000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
175700000000     c*--------------------------------------------------------------------------------------------*
175800000000     c     elaevelbl     BEGSR
175900000000     c*
176000000000     c* azzera le variabili di lavoro
176100000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
176200000000     c                   EVAL      deplpn = *zeros
176300000000     c                   EVAL      depnrn = *zeros
176400000000     c                   EVAL      depnsn = *zeros
176500000000     c*
176600000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
176700000000     c                   EVAL      kbllpp =  s_taslnp
176800000000     c                   EVAL      kblnrp =  s_tasnrs
176900000000     c                   EVAL      kblnsp =  s_tasnsp
177000000000     c*
177100000000     c* ciclo fino a fine mamme
177200000000     c     ke2lbl        SETLL     fnlbl02l
177300000000     c     ke2lbl        READE     fnlbl02l                               99
177400000000do  1c                   DOW       NOT *in99
177500000000     c*
177600000000     c* ciclo fino a fine figlie della mamma
177700000000do  2c                   DOW       NOT *in99
177800000000     c*
177900000000     c* legge la bolla dal legame
178000000000     c                   EVAL      kasaas = lblaan                              *figlia
178100000000     c                   EVAL      kaslnp = lbllpn
178200000000     c                   EVAL      kasnrs = lblnrn
178300000000     c                   EVAL      kasnsp = lblnsn
178400000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
178500000000if  3c                   IF        NOT *in99 AND                                *esistente
178600000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
178700000000     c                             tastbl <> 'A3' AND
178800000000     c                             tastbl <> 'F3' AND
178900000000     c                             tastbl <> 'AP' AND
179000000000     c                             tastbl <> 'FC'
179100041222     **?Immagine lettera di vettura.
179200041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
179300041222     C                   IF        LDVO74 <> 'N'
179400040330     c                   MOVEL     tasflo        dtasflo
179500040330     c                   IF        §floiml <> *BLANK
179600050325     c                   EVAL      ldvo74 = 'S'
179700040330     C                   EVAL      SpImLDVO74 =
179800040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
179900040330     C                             %EDITC(taslnp:'X') +
180000040330     C                             %EDITC(tasnrs:'X') +
180100040330     C                             %EDITC(tasnsp:'X')
180200041223     **?Reperisco il check code della spedizione POD
180300041223     C                   EVAL      PChkCdeO74 =
180400041223     C                             GetSpeChkCde(
180500041223     C                             %EDITC(tasaas:'X'):
180600041223     C                             %EDITC(taslnp:'X') +
180700041223     C                             %EDITC(tasnrs:'X') +
180800041223     C                             %EDITC(tasnsp:'X'):
180900041223     C                             ChkCode:nEsito)
181000040330     C                   ENDIF
181100041222     C                   ENDIF
181200000000     c* incrementa la sequenza legami per le bolle legate
181300000000     c                   ADD       1             seql                           *legaqme successivo
181400000000     c*---
181500000000     c* memorizza gli "eventi fissi"
181600000000     c*---
181700000000     c                   EXSR      memevefix
181800000000     c*---
181900000000     c* memorizza gli eventi
182000000000     c*---
182100000000     c                   EVAL      waas = lblaan                                *figlia
182200000000     c                   EVAL      wlnp = lbllpn
182300000000     c                   EVAL      wnrs = lblnrn
182400000000     c                   EVAL      wnsp = lblnsn
182500000000     c                   EXSR      memevb
182600000000     c*
182700000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
182800000000     c                   EVAL      depaan = lblaan                              *ultima figlia
182900000000     c                   EVAL      deplpn = lbllpn
183000000000     c                   EVAL      depnrn = lblnrn
183100000000     c                   EVAL      depnsn = lblnsn
183200000000e   3c                   ENDIF
183300000000     c*
183400000000     c* lettura successiva legami
183500000000     c     ke2lbl        READE     fnlbl02l                               99
183600000000e   2c                   ENDDO                                                  *fine figlie mamma
183700000000     c*
183800000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
183900000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
184000000000     c                             kbllpp <> deplpn OR
184100000000     c                             kblnrp <> depnrn OR
184200000000     c                             kblnsp <> depnsn
184300000000     c* nuova chiave
184400000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
184500000000     c                   EVAL      kbllpp =  deplpn
184600000000     c                   EVAL      kblnrp =  depnrn
184700000000     c                   EVAL      kblnsp =  depnsn
184800000000     c* controlla figlie
184900000000     c     ke2lbl        SETLL     fnlbl02l
185000000000     c     ke2lbl        READE     fnlbl02l                               99
185100000000if  3c                   IF        *in99                                        *non ha figlie
185200000000     c                   LEAVE                                                  *esce dal ciclo
185300000000e   3c                   ENDIF
185400000000x   2c                   ELSE                                                   *sempre stessa mamma
185500000000     c                   LEAVE                                                  *esce dal ciclo
185600000000e   2c                   ENDIF
185700000000e   1c                   ENDDO                                                  *fine mamme
185800000000     c*
185900000000     c                   ENDSR
186000160127     c*--------------------------------------------------------------------------------------------*
186100160127     c* Imposta l'evento presa in carico se bolla in partenza
186200160127     c*--------------------------------------------------------------------------------------------*
186300160127     c     evbinca       BEGSR
186400160127     c*
186500160127     c* indice schiera
186600160127     c                   Z-ADD     tote          smi(tote)
186700160127     c* sequenza legame
186800160127     c                   Z-ADD     seql          smseql(tote)
186900160127     c* sequenza evento
187000160127     c                   Z-ADD     008           smseqe(tote)                   *primo evento
187100160127     c* data
187200160127if  1c                   IF        arbdim   > *zeros
187300160127     c                   MOVEL     arbdim        wdat
187400160127     c                   EXSR      edtdat
187500160127     c                   MOVEL     wdate         smdeve(tote)
187600160127e   1c                   ENDIF
187700160127     c* filiale
187800160127     c                   Z-ADD     arblnp        worg
187900160127     c                   EXSR      decorg
188000160127     c
188100160127     c                   MOVEL     wodes         smfle(tote)
188200160127     c* causale
188300160127     c                   MOVEL     wcev          smcev(tote)
188400160127     c* data+ora evento
188500160127     c                   MOVEL     wdat          smdevhev(tote)
188600160127     c                   MOVE      wora          smdevhev(tote)
188700160127     c*
188800160127     c                   ENDSR
188900000000     c*--------------------------------------------------------------------------------------------*
189000000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
189100000000     c*--------------------------------------------------------------------------------------------*
189200000000     c     evbrit        BEGSR
189300160318     c
189400000000     c*
189500000000     c* indice schiera
189600000000     c                   Z-ADD     tote          smi(tote)
189700000000     c* sequenza legame
189800000000     c                   Z-ADD     seql          smseql(tote)
189900000000     c* sequenza evento
190000000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
190100160407     c* data ritiro solo se spuntato
190200160318     c                   if        esito='P'
190300160407     c                   if        arbdse>0
190400160318if  1c                   IF        arbdrt > *zeros
190500160318     c                   MOVEL     arbdrt        wdat
190600160318     c                   EXSR      edtdat
190700160318     c                   MOVEL     wdate         smdeve(tote)
190800160318e   1c                   ENDIF
190900160318     c* ora  da FPP
191000160318if  2c                   IF        arbfpp = 'P'                                 *PM
191100160318     c                   MOVEL     1800          wora
191200160318x   2c                   ELSE                                                   *AM
191300160318     c                   MOVEL     1200          wora
191400160318e   1c                   ENDIF
191500160318     c                   EXSR      edtora
191600160318     c                   MOVEL     worae         smheve(tote)
191700160318     c* filiale
191800160318     c                   Z-ADD     arblnp        worg
191900160318     c                   EXSR      decorg
192000160407
192100160407     c                   endif
192200160407     c
192300160318     c
192400160318     c                   else
192500160318     c
192600000000if  1c                   IF        s_tasdrt > *zeros
192700000000     c                   MOVEL     s_tasdrt      wdat
192800000000     c                   EXSR      edtdat
192900000000     c                   MOVEL     wdate         smdeve(tote)
193000000000e   1c                   ENDIF
193100000000     c* ora
193200000000if  1c                   IF        s_tashrt > *zeros
193300000000     c                   MOVEL     s_tashrt      wora
193400000000x   1c                   ELSE
193500000000if  2c                   IF        s_tasfpp = 'P'                               *PM
193600000000     c                   MOVEL     1800          wora
193700000000x   2c                   ELSE                                                   *AM
193800000000     c                   MOVEL     1200          wora
193900000000e   2c                   ENDIF
194000000000e   1c                   ENDIF
194100000000     c                   EXSR      edtora
194200000000     c                   MOVEL     worae         smheve(tote)
194300000000     c* filiale
194400000000     c                   Z-ADD     s_taslnp      worg
194500000000     c                   EXSR      decorg
194600150618     c
194700150618     c* Se la bolla è import e devo emettere l'evento di telefonare
194800150618     c*  non posso indciare la filiale esterna, ma l'hub italiano
194900150618     c                   if        wofl1=ESTERO and sostrit='S' and
195000150618     c                             §7oinc='T'
195100150618     c                   z-add     s_tastfp      worg
195200150618     c                   EXSR      decorg
195300150618     c                   endif
195400160318     c                   endif
195500160318
195600000000     c                   MOVEL     wodes         smfle(tote)
195700000000     c* causale
195800000000     c                   MOVEL     wcev          smcev(tote)
195900000000     c* data+ora evento
196000000000     c                   MOVEL     wdat          smdevhev(tote)
196100000000     c                   MOVE      wora          smdevhev(tote)
196200000000     c*
196300000000     c                   ENDSR
196400000000     c*--------------------------------------------------------------------------------------------*
196500000000     c* Imposta l'evento Partenza
196600000000     c*--------------------------------------------------------------------------------------------*
196700000000     c     evbpar        BEGSR
196800000000     c*
196900000000     c* controlla SE e COME inserire l'evento
197000000000if  1c                   IF        lduc > *zeros
197100000000     c* imposta causale
197200000000     c                   MOVEL     '702'         wcev
197300000000     c                   EXSR      deccevsta
197400000000if  2c                   IF        werr <> '1'                                  *no errore
197500000000     c* imposta filiale
197600000000     c                   Z-ADD     llnp          worg
197700000000     c                   EXSR      decorg
197800000000     c* imposta data
197900101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
198000000000     c                   MOVEL     ldrt          wdat                           *data ritiro
198100000000x   3c                   ELSE                                                   partenza ITALIA
198200000000     c                   MOVEL     lduc          wdat                           *data partenza
198300000000e   3c                   ENDIF
198400000000     c                   EXSR      edtdat
198500000000     c* imposta ora
198600000000     c                   MOVEL     2100          wora                           *ora imposta
198700000000     c                   EXSR      edtora
198800000000     c*
198900000000     c* controlla se l'evento è già stato inserito
199000000000     c                   MOVEL     'S'           newevb                         *nuovo evento
199100000000do  3c     1             DO        200           j
199200000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
199300000000     c                   MOVE      wora          lavdevhev
199400000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
199500000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
199600000000     c                   MOVEL     'N'           newevb                         *già inserito
199700000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
199800000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
199900000000     c                   LEAVE                                                  *uscita ciclo
200000000000e   4c                   ENDIF
200100000000e   3c                   ENDDO
200200000000     c*
200300000000     c* incrementa contatore eventi
200400000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
200500000000     c                   ADD       1             tote
200600000000     c* indice schiera
200700000000     c                   Z-ADD     tote          smi(tote)
200800000000     c* sequenza legame
200900000000     c                   Z-ADD     seql          smseql(tote)
201000000000     c* sequenza evento
201100000000     c                   Z-ADD     020           smseqe(tote)
201200000000     c* descrizione
201300030130     c                   MOVEL     wcdex         smdev(tote)
201400000000     c* filiale -impostata sopra-
201500000000     c                   MOVEL     wodes         smfle(tote)
201600000000     c* data    -impostata sopra-
201700000000     c                   MOVEL     wdate         smdeve(tote)
201800000000     c* ora     -impostata sopra-
201900000000     c                   MOVEL     worae         smheve(tote)
202000000000     c* causale -impostata sopra-
202100000000     c                   MOVEL     wcev          smcev(tote)
202200000000     c* data+ora evento
202300000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
202400000000     c                   MOVE      wora          smdevhev(tote)
202500000000e   3c                   ENDIF
202600000000e   2c                   ENDIF
202700000000e   1c                   ENDIF
202800000000     c*
202900000000     c                   ENDSR
203000101129     c*------------------------------------------------------------------------*
203100101129     c*
203200101129     c* Imposta l'evento Arrivo
203300101129     c*
203400101129     c*------------------------------------------------------------------------*
203500000000     c     evbarr        BEGSR
203600101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
203700101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
203800101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
203900101129     ** sommato inutile.
204000150618     C                   IF        wrkLnaItaEst = ESTERO
204100150618     C                   LEAVESR
204200150618     C                   ENDIF
204300101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
204400101129     C                   IF        lhti = *ZERO
204500101129     C                   LEAVESR
204600101129     C                   ENDIF
204700000000     c*
204800000000     c* controlla SE e COME inserire l'evento
204900000000if  1c                   IF        ldti > *zeros OR
205000000000     c                             ldcm > *zeros
205100000000     c* imposta causale
205200101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
205300000000     c                   EXSR      deccevsta
205400000000if  2c                   IF        werr <> '1'                                  *no errore
205500000000     c* imposta filiale
205600150617     c                   Z-ADD     llna          worg
205700000000     c                   EXSR      decorg
205800000000     c* imposta data
205900000000if  3c                   IF        ldti > *zeros
206000000000     c                   MOVEL     ldti          wdat                           *data arrivo
206100000000x   3c                   ELSE
206200000000     c                   MOVEL     ldcm          wdat                           *data consegna
206300000000e   3c                   ENDIF
206400000000     c                   EXSR      edtdat
206500000000     c* imposta ora
206600000000if  3c                   IF        lhti > *zeros
206700000000     c                   MOVEL     lhti          wora                           *ora arrivo
206800000000x   3c                   ELSE
206900000000     c                   MOVEL     0600          wora                           *ora imposta
207000000000e   3c                   ENDIF
207100000000     c                   EXSR      edtora
207200000000     c*
207300000000     c* controlla se l'evento è già stato inserito
207400000000     c                   MOVEL     'S'           newevb                         *nuovo evento
207500000000do  3c     1             DO        200           j
207600000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
207700000000     c                   MOVE      wora          lavdevhev
207800000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
207900000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
208000000000     c                   MOVEL     'N'           newevb                         *già inserito
208100000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
208200000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
208300000000     c                   LEAVE                                                  *uscita ciclo
208400000000e   4c                   ENDIF
208500000000e   3c                   ENDDO
208600000000     c*
208700000000     c* incrementa contatore eventi
208800000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
208900000000     c                   ADD       1             tote
209000000000     c* indice schiera
209100000000     c                   Z-ADD     tote          smi(tote)
209200000000     c* sequenza legame
209300000000     c                   Z-ADD     seql          smseql(tote)
209400000000     c* sequenza evento
209500000000     c                   Z-ADD     020           smseqe(tote)
209600000000     c* descrizione
209700030130     c                   MOVEL     wcdex         smdev(tote)
209800000000     c* data    -impostata sopra-
209900000000     c                   MOVEL     wdate         smdeve(tote)
210000000000     c* ora     -impostata sopra-
210100000000     c                   MOVEL     worae         smheve(tote)
210200000000     c* filiale -impostata sopra-
210300000000     c                   MOVEL     wodes         smfle(tote)
210400000000     c* causale -impostata sopra-
210500000000     c                   MOVEL     wcev          smcev(tote)
210600000000     c* data+ora evento
210700000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
210800000000     c                   MOVE      wora          smdevhev(tote)
210900000000e   3c                   ENDIF
211000000000e   2c                   ENDIF
211100000000e   1c                   ENDIF
211200000000     c*
211300000000     c                   ENDSR
211400000000     c*--------------------------------------------------------------------------------------------*
211500000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
211600000000     c*--------------------------------------------------------------------------------------------*
211700140714     c     evbcon        BEGSR
211800000000     c*
211900000000     c* azzera variabili di lavoro
212000000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
212100000000     c****
212200000000     c* Consegna merce ->
212300000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
212400000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
212500000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
212600000000     c*   nè lo stato di consegna nè quello di consegna anomala.
212700000000     c****
212800000000     c* controlla SE e COME inserire l'evento
212900000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
213000060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
213100060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
213200060622     C                             :rpyEsito)
213300060622     C                   IF        rpyOpCode = 'FOUND'
213400041222     **?Consegna anomala, POD non visualizzabile.
213500060622     C                   IF        §7OPODImg = 'N'
213600041222     C                   EVAL      LDVO74 = 'N'
213700041222     C                   ENDIF
213800060622     c                   IF        §7oinc <> *blanks                            *ok per internet
213900060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
214000000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
214100000000x   3c                   ELSE                                                   *ha record FNEVB
214200000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
214300000000e   3c                   ENDIF
214400041222e   3c                   ENDIF
214500000000e   2c                   ENDIF
214600000000e   1c                   ENDIF
214700080131     c* imposta data
214800080131if  1c                   IF        ldcm > *zeros
214900080131     c                   MOVEL     ldcm          wdat
215000080131e   1c                   ENDIF
215100080131     c* imposta ora
215200080131if  1c                   IF        lhmc > *zeros
215300080131     c                   MOVEL     lhmc          wora
215400080131e   1c                   ENDIF
215500000000     c* imposta causale
215600000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
215700060622     c                   MOVEL     §7ocev        wcev                           *causale evento
215800080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
215900080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
216000080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
216100080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
216200080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
216300080131     c* ritiro merce) dalla bolla figlia.
216400080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
216500080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
216600080131     c                             AND s_tasNdc < *HIVAL
216700080131     c                   EVAL      kblAap = wAas
216800080131     c                   EVAL      kblLpp = wLnp
216900080131     c                   EVAL      kblNrp = wNrs
217000080131     c                   EVAL      kblNsp = wNsp
217100080131     c     ke2lbl        CHAIN     fnlbl02l
217200080131     c                   IF        %FOUND()
217300080131     c                   EVAL      kasAas = lblAan
217400080131     c                   EVAL      kasLnp = lblLpn
217500080131     c                   EVAL      kasNrs = lblNrn
217600080131     c                   EVAL      kasNsp = lblNsn
217700080131     C     keyTas        CHAIN     titas000      titas000Figlia
217800080131     C                   IF        %FOUND()
217900080131     C                   EVAL      wDat = titas000Figlia.tasDrt
218000080131     C                   EVAL      wOra = titas000Figlia.tasHrt
218100080131     C                   ELSE
218200080131     C     keyTas        CHAIN     titas010      titas010Figlia
218300080131     C                   IF        %FOUND()
218400080131     C                   EVAL      wDat = titas010Figlia.tasDrt
218500080131     C                   EVAL      wOra = titas010Figlia.tasHrt
218600080131     C                   ELSE
218700080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
218800080131     C                   IF        %FOUND()
218900080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
219000080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
219100080131     C                   ENDIF
219200080131     C                   ENDIF
219300080131     C                   ENDIF
219400080131     c                   ENDIF
219500080131     c                   ENDIF
219600000000x   1c                   ELSE                                                   *NO sost.Consegna
219700000000     c                   MOVEL     '704'         wcev                           *causale evento
219800000000e   1c                   ENDIF
219900080131     c* Editazione data e ora evento.
220000080131     c                   EXSR      edtdat
220100080131     c                   EXSR      edtora
220200000000     c* imposta filiale
220300000000     c                   Z-ADD     llna          worg
220400000000     c                   EXSR      decorg
220500150618     c* se si tratta di lna estera e la bolla ha consegna anomala
220600150618     c*  in cui mettere msg generico, allora devo sostituire la filiale
220700150618     c*  con il suo hub
220800150618     c                   if        wofl1=estero and lcca<>' ' and
220900150618     c                             §7oinc='T'
221000150618     c                   Z-ADD     ltfa          worg
221100150618     c                   EXSR      decorg
221200150618     c                   endif
221300150618     c
221400000000     c* controlla se l'evento è già stato inserito
221500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
221600000000do  1c     1             DO        200           j
221700000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
221800000000     c                   MOVE      wora          lavdevhev
221900000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
222000000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
222100000000     c                   MOVEL     'N'           newevb                         *già inserito
222200000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
222300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
222400000000     c                   LEAVE                                                  *uscita ciclo
222500000000e   2c                   ENDIF
222600000000e   1c                   ENDDO
222700000000     c*
222800000000     c* incrementa contatore eventi
222900030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
223000030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
223100000000     c                   ADD       1             tote                           incrementa contatore
223200060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
223300060622     c                   MOVEL     §7odei        smdev(tote)
223400030203x   4c                   ELSE                                                   *evento telef.P.O.
223500060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
223600030203e   4c                   ENDIF
223700060622     c                   MOVEL     §7ocev        wcev                           *causale evento
223800000000     c                   EXSR      evbcon2                                      *imposta evento
223900030203x   3c                   ELSE                                                   *NO sost.Consegna
224000000000     c*
224100030203if  4c                   IF        ldcm > *zeros
224200030203if  5c                   IF        sostcon <> ' '                               *considera consegna
224300000000     c                   EXSR      deccevsta
224400030203if  6c                   IF        werr <> '1'                                  *no errore
224500000000     c                   ADD       1             tote                           incrementa contatore
224600030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
224700000000     c                   EXSR      evbcon2                                      *imposta evento
224800030203e   6c                   ENDIF
224900030203e   5c                   ENDIF
225000030203e   4c                   ENDIF
225100030203e   3c                   ENDIF
225200000000     c*
225300030203e   1c                   ENDIF
225400030203     c*
225500000000     c                   ENDSR
225600000000     c*--------------------------------------------------------------------------------------------*
225700000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
225800000000     c*--------------------------------------------------------------------------------------------*
225900000000     c     evbcon2       BEGSR
226000000000     c*
226100000000     c* indice schiera
226200000000     c                   Z-ADD     tote          smi(tote)
226300000000     c* sequenza legame
226400000000     c                   Z-ADD     seql          smseql(tote)
226500000000     c* sequenza evento
226600000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
226700000000     c* data    -impostata sopra-
226800000000     c                   MOVEL     wdate         smdeve(tote)
226900000000     c* ora     -impostata sopra-
227000000000     c                   MOVEL     worae         smheve(tote)
227100000000     c* filiale -impostata sopra-
227200000000     c                   MOVEL     wodes         smfle(tote)
227300000000     c* causale -impostata sopra-
227400000000     c                   MOVEL     wcev          smcev(tote)
227500000000     c* data+ora evento
227600000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
227700000000     c                   MOVE      wora          smdevhev(tote)
227800000000     c*
227900000000     c                   ENDSR
228000000000     c*--------------------------------------------------------------------------------------------*
228100000000     c* memorizza i dati della lettera di anomalia
228200000000     c*--------------------------------------------------------------------------------------------*
228300000000     c     memdct        BEGSR
228400000000     c*
228500040906     C                   RESET                   FIDN12DS
228600040906     c                   Z-ADD     waas          I12AAS
228700040906     c                   Z-ADD     wlnp          I12LNP
228800040906     c                   Z-ADD     wnrs          I12NRS
228900040906     c                   Z-ADD     wnsp          I12NSP
229000040906     C                   CALL      'FIDN12R'
229100040906     C                   PARM                    FIDN12DS
229200000000     c* memorizza la prima lettera di anomalia valida
229300040906do  1c                   IF        O12Err = *BLANK
229400040906     C                   DO        O12NCA        K
229500040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
229600040906     C                   EVAL      DCTAAC = O12KeyAAC
229700040906     C                   EVAL      DCTFIL = O12KeyFil
229800040906     C                   EVAL      DCTNCA = O12KeyNCA
229900040906     c     keydct        CHAIN     fndct01l
230000040906do  1c                   IF        %FOUND
230100000000     c*
230200000000     c* controlla validità CA
230300000000     c                   MOVEL     '0'           werr                           *NO errore
230400000000     c                   EXSR      chkdct
230500000000if  2c                   IF        werr = '0'                                   *CA valida
230600000000     c*
230700000000     c* Imposta l'evento lettera di anomalia
230800000000     c                   EXSR      evbdct
230900000000     C                   LEAVE                                                  *uscita ciclo
231000000000e   2c                   ENDIF
231100000000     c*
231200040906e   1c                   ENDIF
231300040906e   1c                   ENDDO
231400040906e   1c                   ENDIF
231500000000     C*
231600000000     c                   ENDSR
231700000000     c*--------------------------------------------------------------------------------------------*
231800000000     c* controlla validità lettera di anomalia
231900000000     c*--------------------------------------------------------------------------------------------*
232000000000     c     chkdct        BEGSR
232100000000     c*
232200000000     c* reimposta variabili di lavoro
232300000000     c                   MOVEL     '0'           werr                           *NO errore
232400000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
232500000000     c*
232600000000     c* CA annullata, esclude
232700000000if  1c                   IF        dctatb <> ' '
232800000000     c                   MOVEL     '1'           werr                           *SI errore
232900000000e   1C                   ENDIF
233000000000     c*
233100000000     c* CA non confermata, esclude
233200000000if  1c                   IF        dctfpr = ' '
233300000000     c                   MOVEL     '1'           werr                           *SI errore
233400000000e   1C                   ENDIF
233500000000     c*
233600000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
233700000000if  1c                   IF        dctfpr = 'T' AND
233800000000     c                             dctfca >= 215 OR
233900000000     c                             dctfpr = 'P' AND
234000000000     c                             dctfca >= 400
234100000000x   1c                   ELSE
234200000000     c                   MOVEL     '1'           werr                           *SI errore
234300000000e   1C                   ENDIF
234400000000     c*
234500000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
234600000000if  1c                   IF        werr = '0'
234700000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
234800060622     C                   RESET                   tibs02ds
234900060622     C                   EVAL      t02Cod = 'CCH'
235000060622     C                   EVAL      t02Ke1 = dctcch
235100060622     C                   EXSR      getTntbe00f
235200060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
235300000000     c                   MOVEL     '1'           werr                           *SI errore
235400000000e   3C                   ENDIF
235500000000e   2C                   ENDIF
235600000000e   1C                   ENDIF
235700000000     c*
235800000000     c                   ENDSR
235900000000     c*--------------------------------------------------------------------------------------------*
236000000000     c* Imposta l'evento lettera di anomalia
236100000000     c*--------------------------------------------------------------------------------------------*
236200000000     c     evbdct        BEGSR
236300000000     C*
236400000000     c* imposta causale
236500000000if  1c                   IF        wfpr = 'T'
236600000000     c                   MOVEL     '709'         wcev                           *causale evento
236700000000x   1c                   ELSE
236800000000     c                   MOVEL     '710'         wcev                           *causale evento
236900000000e   1c                   ENDIF
237000000000     c                   EXSR      deccevsta                                    *decodifica evento
237100000000     c*
237200000000     c* imposta filiale
237300000000if  1c                   IF        werr <> '1'                                  *no errore
237400000000     c                   Z-ADD     dctfil        worg
237500000000     c                   EXSR      decorg
237600000000     c* imposta data
237700000000     c                   MOVEL     dctaac        wdat
237800000000     c                   MOVE      dctmgc        wdat
237900000000     c                   EXSR      edtdat
238000000000     c* imposta ora
238100000000     c                   MOVEL     1100          wora                           *FISSA
238200000000     c                   EXSR      edtora
238300000000     c*
238400000000     c* incrementa contatore eventi
238500000000     c                   ADD       1             tote
238600000000     c* indice schiera
238700000000     c                   Z-ADD     tote          smi(tote)
238800000000     c* sequenza legame
238900000000     c                   Z-ADD     seql          smseql(tote)
239000000000     c* sequenza evento
239100000000     c                   Z-ADD     999           smseqe(tote)
239200000000     c* descrizione -impostata sopra-
239300030130     c                   MOVEL     wcdex         smdev(tote)
239400000000     c* filiale -impostata sopra-
239500000000     c                   MOVEL     wodes         smfle(tote)
239600000000     c* data    -impostata sopra-
239700000000     c                   MOVEL     wdate         smdeve(tote)
239800000000     c* ora     -impostata sopra-
239900000000     c                   MOVEL     worae         smheve(tote)
240000000000     c* causale -impostata sopra-
240100000000     c                   MOVEL     wcev          smcev(tote)
240200000000     c* data+ora evento
240300000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
240400000000     c                   MOVE      wora          smdevhev(tote)
240500000000e   1c                   ENDIF
240600000000     c*
240700000000     c                   ENDSR
240800000000     c*--------------------------------------------------------------------------------------------*
240900000000     c* imposta i campi della DS di Output dal contrassegno
241000000000     c*--------------------------------------------------------------------------------------------*
241100000000     c     impdsocsb     BEGSR
241200000000     c*
241300000000     c* azzera le variabili di lavoro
241400000000     c*
241500000000     c****
241600000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
241700000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
241800000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
241900000000     c****
242000000000     c*
242100000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
242200090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
242300000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
242400000000x   1c                   ELSE                                                     *trovato
242500000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
242600000000e   1C                   ENDIF
242700000000     c*---
242800000000     c* memorizza il contrassegno della bolla di Input
242900000000     c*---
243000000000     c                   EVAL      waas = s_tasaas                              *bolla Input
243100000000     c                   EVAL      wlnp = s_taslnp
243200000000     c                   EVAL      wnrs = s_tasnrs
243300000000     c                   EVAL      wnsp = s_tasnsp
243400000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
243500000000     c                   EXSR      memcsb
243600000000     c*
243700000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
243800000000     c                   EXSR      elacsblbl
243900000000     c*
244000000000     c                   ENDSR
244100000000     c*--------------------------------------------------------------------------------------------*
244200000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
244300000000     c*--------------------------------------------------------------------------------------------*
244400000000     c     elacsblbl     BEGSR
244500000000     c*
244600000000     c* azzera le variabili di lavoro
244700000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
244800000000     c                   EVAL      deplpn = *zeros
244900000000     c                   EVAL      depnrn = *zeros
245000000000     c                   EVAL      depnsn = *zeros
245100000000     c*
245200000000     c* legge i legami delle figlie successive alla bolla di input
245300000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
245400000000     c                   EVAL      kbllpp =  s_taslnp
245500000000     c                   EVAL      kblnrp =  s_tasnrs
245600000000     c                   EVAL      kblnsp =  s_tasnsp
245700000000     c*
245800000000     c* ciclo fino a fine mamme
245900000000     c     ke2lbl        SETLL     fnlbl02l
246000000000     c     ke2lbl        READE     fnlbl02l                               99
246100000000do  1c                   DOW       NOT *in99
246200000000     c*
246300000000     c* ciclo fino a fine figlie della mamma
246400000000do  2c                   DOW       NOT *in99
246500000000     c*
246600000000     c* legge la bolla dal legame
246700000000     c                   EVAL      kasaas = lblaan                              *figlia
246800000000     c                   EVAL      kaslnp = lbllpn
246900000000     c                   EVAL      kasnrs = lblnrn
247000000000     c                   EVAL      kasnsp = lblnsn
247100000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
247200000000     c*---
247300000000     c* memorizza il contrassegno della bolla figlia legata
247400000000     c*---
247500000000if  4c                   IF        NOT *in99                                    *esistente
247600000000     c                   EVAL      waas = lblaan                                *figlia
247700000000     c                   EVAL      wlnp = lbllpn
247800000000     c                   EVAL      wnrs = lblnrn
247900000000     c                   EVAL      wnsp = lblnsn
248000000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
248100000000     c                   EXSR      memcsb
248200000000e   4c                   ENDIF
248300000000     c*
248400000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
248500000000     c                   EVAL      depaan = lblaan                              *ultima figlia
248600000000     c                   EVAL      deplpn = lbllpn
248700000000     c                   EVAL      depnrn = lblnrn
248800000000     c                   EVAL      depnsn = lblnsn
248900000000     c*
249000000000     c* lettura successiva legami
249100000000     c     ke2lbl        READE     fnlbl02l                               99
249200000000e   2c                   ENDDO                                                  *fine figlie mamma
249300000000     c*
249400000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
249500000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
249600000000     c                             kbllpp <> deplpn OR
249700000000     c                             kblnrp <> depnrn OR
249800000000     c                             kblnsp <> depnsn
249900000000     c* nuova chiave
250000000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
250100000000     c                   EVAL      kbllpp =  deplpn
250200000000     c                   EVAL      kblnrp =  depnrn
250300000000     c                   EVAL      kblnsp =  depnsn
250400000000     c* controlla figlie
250500000000     c     ke2lbl        SETLL     fnlbl02l
250600000000     c     ke2lbl        READE     fnlbl02l                               99
250700000000if  3c                   IF        *in99                                        *non ha figlie
250800000000     c                   LEAVE                                                  *esce dal ciclo
250900000000e   3c                   ENDIF
251000000000x   2c                   ELSE                                                   *sempre stessa mamma
251100000000     c                   LEAVE                                                  *esce dal ciclo
251200000000e   2c                   ENDIF
251300000000e   1c                   ENDDO                                                  *fine mamme
251400000000     c*
251500000000     c                   ENDSR
251600000000     c*--------------------------------------------------------------------------------------------*
251700000000     c* memorizza il contrassegno
251800000000     c*--------------------------------------------------------------------------------------------*
251900000000     c     memcsb        BEGSR
252000000000     c*
252100000000     c* legge il contrassegno, se avuto
252200000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
252300000000     c                   Z-ADD     waas          ksbaas
252400000000     c                   Z-ADD     wlnp          ksblnp
252500000000     c                   Z-ADD     wnrs          ksbnrs
252600000000     c                   Z-ADD     wnsp          ksbnsp
252700000000     c     keycsb        CHAIN     tncsb03l                           99
252800000000if  2c                   IF        NOT *in99                                    *esistente
252900000000     c* memorizza i dati
253000000000     c                   MOVEL     'S'           cao74                          *esiste C/A
253100000000     c*
253200000000     c                   Z-ADD     csbcas        caimpo74                       *importo
253300000000     c*
253400000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
253500010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
253600010820     c                             caimpo74 > *zeros
253700010820     c                   EVAL      cadivo74 = 'ITL'
253800010820e   3c                   ENDIF
253900160314
254000160314     ** Visualizzo la descrizione del tipo incasso
254100100121     C                   EVAL      cainco74 =
254200100121     C                                       GetDescrizioneIncassoContrassegno()
254300000000     c*
254400000000     c                   MOVEL     *blanks       castao74                       *stato
254500060622     C                   EVALR     tblKey = %CHAR(csbSta)
254600060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
254700060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
254800060622     C                             :rpyOpCode:rpyEsito)
254900060622     C                   IF        rpyOpCode = 'FOUND'
255000060622     c                   EVAL      castao74 = §4SDei
255100000000e   3c                   ENDIF
255200100119     c
255300000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
255400011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
255500060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
255600000000x   4c                   ELSE
255700000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
255800060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
255900000000x   5c                   ELSE
256000060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
256100000000e   5c                   ENDIF
256200000000e   4c                   ENDIF
256300000000e   3c                   ENDIF
256400000000     c*
256500000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
256600000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
256700000000if  4c                   IF        csbtpa = 'B'
256800060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
256900000000e   4c                   ENDIF
257000000000if  4c                   IF        csbtpa = 'C'
257100060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
257200000000e   4c                   ENDIF
257300000000x   3c                   ELSE                                                   *incasso Bartolini
257400000000if  4c                   IF        csbfpc = *blanks
257500110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
257600000000e   4c                   ENDIF
257700000000if  4c                   IF        csbfpc = '2'
257800060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
257900000000e   4c                   ENDIF
258000000000e   3c                   ENDIF
258100000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
258200000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
258300000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
258400000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
258500000000     c                             csbfpc = *blanks                              assegno di traenza
258600060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
258700000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
258800060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
258900000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
259000060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
259100000000e   4c                   ENDIF
259200000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
259300060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
259400060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
259500000000e   3c                   ENDIF
259600000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
259700000000     c                   Z-ADD     *zeros        caabio74                       *abi
259800000000     c                   Z-ADD     *zeros        cacabo74                       *cab
259900000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
260000000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
260100000000     c                   MOVEL     csbddp        wdat
260200000000     c                   EXSR      edtdat
260300000000     c                   EVAL      cadato74 = wdate
260400000000     C*
260500000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
260600000000     c                   Z-ADD     csbabi        caabio74
260700000000     c                   Z-ADD     csbcab        cacabo74
260800000000     c                   MOVEL     csbnra        canumo74
260900120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
261000120321     c                             : caCabO74)
261100000000x   4c                   ELSE                                                   *incasso Bartolini
261200000000     c                   MOVEL     csbndp        canumo74
261300000000e   4c                   ENDIF
261400000000e   3c                   ENDIF
261500110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
261600110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
261700110307     C                   IF        IsContrassegnoCompensato()
261800110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
261900110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
262000110304     C                   CLEAR                   caabio74
262100110304     C                   CLEAR                   cacabo74
262200110304     C                   CLEAR                   canumo74
262300110304     C                   ENDIF
262400110304     C*
262500000000e   2c                   ENDIF
262600000000e   1c                   ENDIF
262700000000     c*
262800000000     c                   ENDSR
262900000000     c*--------------------------------------------------------------------------------------------*
263000000000     c* imposta i campi della DS di Output dalla giacenza
263100000000     c*--------------------------------------------------------------------------------------------*
263200000000     c     impdsogcp     BEGSR
263300000000     c*
263400000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
263500000000     c                   EVAL      waas = s_tasaas                              *bolla Input
263600000000     c                   EVAL      wlnp = s_taslnp
263700000000     c                   EVAL      wnrs = s_tasnrs
263800000000     c                   EVAL      wnsp = s_tasnsp
263900000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
264000000000     c                   EXSR      memgcp
264100000000     c*
264200000000     c                   ENDSR
264300000000     c*--------------------------------------------------------------------------------------------*
264400000000     c* memorizza la giacenza
264500000000     c*--------------------------------------------------------------------------------------------*
264600000000     c     memgcp        BEGSR
264700000000     c*
264800000000     c* imposta le variabili di lavoro
264900000000     c                   CLEAR                   i                              *indice
265000000000     c                   CLEAR                   ii
265100000000     c                   CLEAR                   smdmc                          *note fase -20-
265200000000     c                   CLEAR                   smdmcR
265300000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
265400000000     c                   MOVEL     'N'           gco74                          *flag di controllo
265500000000     c                   MOVEL     'N'           gcfdio74
265600000000     c                   MOVEL     'N'           gcmito74
265700000000     c*
265800000000     c* legge la giacenza, se avuta
265900000000     c                   Z-ADD     waas          kcpaas
266000000000     c                   Z-ADD     wlnp          kcplnp
266100000000     c                   Z-ADD     wnrs          kcpnrs
266200000000     c                   Z-ADD     wnsp          kcpnsp
266300000000     c                   Z-ADD     *zeros        kcpfrg
266400050309     c     keygcp        CHAIN     tigcp51l                           99
266500050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
266600050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
266700000000if  2c                   IF        NOT *in99        AND                         *esistente
266800000000     c                             gcpatb = *blanks AND                         *no annullata
266900050415     c                             gcptcm <> 'Z'    AND
267000050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
267100050419     C
267200050419     C                   EVAL      gcRiapO74 = gcpRiap
267300050418     **?Descrizione della fase.
267400050418     C                   SELECT
267500050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
267600060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
267700050418     C                   WHEN      gcpFas = 20
267800060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
267900050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
268000060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
268100050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
268200060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
268300050418     C                   ENDSL
268400000000     c* memorizza i dati dalle NOTE GIACENZE
268500000000     c                   Z-ADD     *zeros        i
268600000000     c                   Z-ADD     *zeros        ii
268700000000     c                   Z-ADD     gcpagc        knpagc
268800000000     c                   Z-ADD     gcpfgc        knpfgc
268900000000     c                   Z-ADD     gcpngc        knpngc
269000000000     c                   Z-ADD     gcpfrg        knpfrg
269100050309     c     keygnp        SETLL     tignp01l
269200050309     c     keygnp        READE     tignp01l                               98
269300000000do  3c                   DOW       NOT *in98
269400050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
269500000000     c* fase -10-
269600000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
269700000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
269800000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
269900000000e   5c                   ENDIF
270000000000     c* fase -20-
270100000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
270200000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
270300051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
270400000000     c                   ADD       1             i
270500000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
270600000000e   6c                   ENDIF
270700000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
270800051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
270900050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
271000050405     c                   CLEAR                   ii
271100050405     c                   CLEAR                   smdmcR
271200050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
271300050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
271400050919     C                   IF        NOT %ERROR
271500140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
271600050919     C                   ENDIF
271700050405     C                   ELSE
271800000000     c                   ADD       1             ii
271900000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
272000050405     C                   ENDIF
272100000000e   6c                   ENDIF
272200000000e   5c                   ENDIF
272300000000e   4c                   ENDIF
272400050309     c     keygnp        READE     tignp01l                               98
272500000000e   3c                   ENDDO
272600000000     c*
272700000000     c* memorizza i dati dalle GIACENZE
272800000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
272900050404     c* Disposizione da immettere.
273000051110     c                   IF        gcpfas = 20
273100051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
273200051110     C                   EXSR      chkTivpi
273300051110e   3c                   ENDIF
273400000000     c*
273500000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
273600000000     c*
273700000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
273800000000     c*
273900060103     c                   Z-ADD     gcpagc        dsannB
274000060103     c                   Z-ADD     gcpmgc        dsmgsB
274100060103     c                   Z-ADD     dsdatB        wdat
274200000000if  3c                   IF        wdat > *zeros
274300000000     c                   EXSR      edtdat
274400000000     c                   EVAL      gcdago74 = wdate                             *data apertura
274500000000e   3C                   ENDIF
274600060104     c*
274700060104     c                   Z-ADD     gcpdur        wdat
274800060104     c                   IF        wdat > *zeros
274900060104     c                   EXSR      edtdat
275000060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
275100060104     C                   ENDIF
275200000000     c*
275300000000if  3c                   IF        gcpcmc <> *blanks
275400000000     c                   MOVEL     gcpcmc        wcev
275500000000     c                   EXSR      deccevgia
275600030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
275700000000e   3C                   ENDIF
275800000000     c*
275900000000     c                   MOVEL     *blanks       gcdcgo74
276000000000if  3c                   IF        gcpdcg > *zeros
276100000000     c                   Z-ADD     gcpdcg        wdat
276200000000     c                   EXSR      edtdat
276300000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
276400000000e   3c                   ENDIF
276500000000     c*---
276600000000     c* Disposizioni già immesse
276700000000     c*---
276800000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
276900000000     c*
277000000000if  4c                   IF        gcpddm > *zeros
277100000000     c                   Z-ADD     gcpddm        wdat
277200000000     c                   EXSR      edtdat
277300000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
277400000000e   4C                   ENDIF
277500000000     c*
277600000000if  4c                   IF        gcpdis <> *blanks
277700060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
277800060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
277900060626     C                             :rpyOpCode:rpyEsito)
278000060622     C                   EVAL      gcdiso74 = §2DDes
278100000000e   4C                   ENDIF
278200000000     c*
278300000000if  4c                   IF        gcpasg <> *blanks
278400000000if  5c                   IF        gcpasg = 'S'
278500060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
278600000000x   5c                   ELSE
278700060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
278800000000e   5C                   ENDIF
278900000000     c*
279000000000if  5c                   IF        gcppsg = 'M'
279100060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
279200000000e   5C                   ENDIF
279300000000if  5c                   IF        gcppsg = 'D'
279400060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
279500000000e   5C                   ENDIF
279600000000e   4C                   ENDIF
279700000000     c*
279800000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
279900000000     c                   MOVEL     gcpvcs        gcvcso74
280000000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
280100000000     c                   Z-ADD     gcpcas        gccaso74
280200000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
280300000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
280400000000e   5C                   ENDIF
280500000000e   4C                   ENDIF
280600050404     C                   IF        smdmc(1) <> *BLANK
280700050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
280800050404     C                             smdmc(3) + smdmc(4)
280900051014     C                   EVAL      gcdmc5O74 = smdmc(5)
281000050404     C                   ELSE
281100050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
281200050404     C                             smdmcr(3) + smdmcr(4)
281300051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
281400050404e   3C                   ENDIF
281500000000e   3C                   ENDIF
281600000000     c*---
281700000000     c* Disposizioni da immettere
281800000000     c*---
281900010719     c* se le disposizioni sono da immettere controlla che:
282000010719     c* - il cliente è il mittente della giacenza
282100000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
282200170315     C* CON KEY GIAC --> SEMPRE SI POSSONO IMMETTERE
282300170315     C                   SELECT
282400170315if  4C                   WHEN      keygiac <> *blanks
282500170315     c                   MOVEL     'S'           gcmito74                       SI POSSO IMMETTERE
282600170315     c
282700170315     C* CON BRTCODE  --> richiamo pgm fnlgw00r
282800170315if  4C                   WHEN      wbrtcode <> *blanks
282900170315     c                   exsr      chiamalgw
283000170315x   4c                   other
283100170315     c* per gli altri casi --> o loggato o anonimo ( gesrtito dal pgm seguente)
283200090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
283300090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
283400090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
283500090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
283600090914     C                             )
283700170315e   4C                   ENDsl
283800000000e   3C                   ENDIF
283900050404     C* Controllo esistenza storia giacenza.
284000050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
284100050404     C                   DO        *HIVAL
284200050404     C     K04GCP51      READE     TIGCP51L
284300050404     C                   IF        %EOF
284400050404     C                   LEAVE
284500050404     C                   ENDIF
284600050404     C                   IF        GCPATB = *BLANK
284700050404     C                   EVAL      StoGiaO74 = 'S'
284800050404     C                   LEAVE
284900050404     C                   ENDIF
285000050404     C                   ENDDO
285100050404     c*
285200000000e   2c                   ENDIF
285300000000     c*
285400000000     c                   ENDSR
285500170315     c*--------------------------------------------------------------------------------------------*
285600170315     c* richiamo pgm di verifica giacenze con collegamento con BRTCODE
285700170315     c*--------------------------------------------------------------------------------------------*
285800170315     c     Chiamalgw     BEGSR
285900170315
286000170315      /free
286100170315       reset  fnlgw00i0   ;
286200170315       reset  fnlgw00o0   ;
286300170315       fnlgw00i0.LINISO2=LANGI74    ;
286400170315       fnlgw00i0.tipoacc='B'         ;
286500170315       fnlgw00i0.brtcode=wbrtcode    ;
286600170315         prwrqsData = fnlgw00i0 ;
286700170315         prwrqsDatasize = %SIZE(fnlgw00i0) ;
286800170315         prwrpyDatasize = %SIZE(fnlgw00o0) ;
286900170315         prwrpyformato='         '  ;
287000170315         prwRpyFormMsg='FNLGW00M0'  ;
287100170316         prwrqsFormato='FNLGW00I0'  ;
287200170316         prwrpyFormato='FNLGW00O0'  ;
287300170315         prwRpyMsgSize  = %SIZE(fnlgw00m0);
287400170315         fnlgw00r(prwRqsOpCode : prwRpyOpCode : prwRpyIdMsg :
287500170315                prwRqsFormato : prwRqsData : prwRqsDataSize:
287600170315                prwRpyFormato : prwRpyData : prwRpyDataSize:
287700170315                prwRpyFormMsg : prwRpyMessage : prwRpyMsgSize)  ;
287800170315
287900170315         if   prwrpyOpCode = FNLGW00_RPYOPCODE_done   ;
288000170315         fnlgw00o0=prwrpydata  ;
288100170315         if fnlgw00o0.butgen=*on   ;
288200170315
288300170315         GCMITO74='S'  ;
288400170315
288500170315         endif    ;
288600170315         endif    ;
288700170315
288800170315      /end-free
288900170315
289000170315     c                   ENDSR
289100000000     c*--------------------------------------------------------------------------------------------*
289200000000     c* imposta i campi della DS di Output dalle note
289300000000     c*--------------------------------------------------------------------------------------------*
289400000000     c     impdsonot     BEGSR
289500000000     c*
289600000000     c* azzera le variabili di lavoro
289700000000     c                   CLEAR                   i
289800000000     c                   CLEAR                   mdcre                          consegna richiesta
289900000000     c                   CLEAR                   mhcre
290000030210     c                   CLEAR                   mtcre
290100000000     c                   CLEAR                   mftce                          consegna particolare
290200000000     c                   CLEAR                   wftc
290300000000     c                   CLEAR                   d1wdftc
290400000000     c                   CLEAR                   d2wdftc
290500000000     c                   CLEAR                   mgcxe                          turni chiusura
290600000000     c                   CLEAR                   wgcx
290700000000     c                   CLEAR                   d1wdgcx
290800000000     c                   CLEAR                   d2wdgcx
290900010927     c                   CLEAR                   mffd                           fermo deposito
291000010927     c                   CLEAR                   mmncB                          *motivi NON consegna
291100010927     c                   CLEAR                   mmncC
291200010927     c                   CLEAR                   mmncG
291300010927     c                   CLEAR                   mmncH
291400010927     c                   CLEAR                   mmncN
291500010927     c                   CLEAR                   mmncO
291600050701     c                   CLEAR                   mmnc8
291700050701     c                   CLEAR                   mmnc9
291800050701     c                   CLEAR                   a230
291900000000     c                   CLEAR                   we
292000030206     c                   CLEAR                   mtel                           n° tel destinatario
292100030206     c                   CLEAR                   mban                           bancali
292200030207     c                   CLEAR                   mscr                           supermercati
292300030210     c                   CLEAR                   msdace
292400030210     c                   CLEAR                   msorce
292500030210     c                   CLEAR                   msdcre
292600030210     c                   CLEAR                   mshcre
292700030210     c                   CLEAR                   mstcre
292800030210     c                   CLEAR                   msnom
292900030210     c                   CLEAR                   mscrT
293000030210     c                   CLEAR                   wmscrT
293100030210     c                   CLEAR                   s
293200030207     c
293300160127     c*imposta dati data consegna richiesta  DA BLP se bolla in partenza
293400160127     c                   if        esito='P'
293500160127     c                   eval      f_tasdcr=arbdcr
293600160127     c                   eval      f_tastcr=arbtcr
293700160127     c                   eval      f_tashcr=arbhcr
293800160127     c* imposto anche key spedizione
293900160309     c                   eval      f_tasaas=arbaas
294000160309     c                   eval      f_taslnp=arblnp
294100160309     c                   eval      f_tasnrs=arbnrs
294200160309     c                   eval      f_tasnsp=arbnsp
294300160127     c                   endif
294400160127     c
294500140716     c                   if        f_tasdcr > 0
294600140716     c                   MOVEL     f_tasdcr      wdat
294700140619     c                   EXSR      edtdat
294800140619     c                   eval      DTCONRCO74  =  wdate
294900140819if  3c                   IF        f_tastcr = ' '
295000140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
295100140819e   3c                   ENDIF
295200140819     c                   endif
295300140716     c                   if        f_tashcr > 0
295400140716     c                   MOVEL     f_tashcr      wora
295500140619     c                   EXSR      edtora
295600140619     c                   eval      ORACONRO74  =  worae
295700140619     c                   endif
295800140716     c                   eval      TPDATCRO74  =  f_tastcr
295900140619      *reperimento descrizione tipo consegna richiesta
296000140716if  3c                   IF        f_tastcr = 'P'
296100140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
296200140619e   3c                   ENDIF
296300140716if  3c                   IF        f_tastcr = 'D'
296400140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
296500140619e   3c                   ENDIF
296600000000     c*---
296700000000     c* memorizza le "note" che sono presenti nel record bolla
296800000000     c*---
296900000000     c* Consegna richiesta
297000140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
297100140619     c*m                           s_tashcr = *zeros
297200140619x   1c*m                 ELSE                                                   *SI cons.particolare
297300140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
297400140619     c*m                 MOVEL     s_tasdcr      wdat
297500140619     c*m                 EXSR      edtdat                                       *edita data
297600140619     c*m                 MOVEL     wdate         mdcre
297700140619e   2c*m                 ENDIF
297800140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
297900140619     c*m                 MOVEL     s_tashcr      wora
298000140619     c*m                 EXSR      edtora                                       *edita ora
298100140619     c*m                 MOVEL     worae         mhcre
298200140619e   2c*m                 ENDIF
298300140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
298400140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
298500140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
298600140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
298700140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
298800140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
298900140619e   3c*m                 ENDIF
299000140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
299100140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
299200140619e   3c*m                 ENDIF
299300140619if  3c*m                 IF        s_tastcr = ' '                               *il
299400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
299500140619e   3c*m                 ENDIF
299600140619e   2c*m                 ENDIF
299700140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
299800140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
299900140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
300000140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
300100140619e   3c*m                 ENDIF
300200140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
300300140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
300400140619e   3c*m                 ENDIF
300500140619if  3c*m                 IF        s_tastcr = ' '                               *il
300600140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
300700140619e   3c*m                 ENDIF
300800140619e   2c*m                 ENDIF
300900140619     c*m
301000140619if  2c*m                 IF        i < 10                                       *se c'è spazio
301100140619     c*m                 ADD       1             i                              *memorizza nota
301200140619     c*m                 EVAL      smnot(i) =
301300140619     c*m                           %TRIM(mtcre) +
301400140619     c*m                           ' '          +
301500140619     c*m                           %TRIM(mdcre) +
301600140619     c*m                           ' '          +
301700140619     c*m                           %TRIM(mhcre)
301800140619e   2c*m                 ENDIF
301900140619e   1c*m                 ENDIF
302000000000     c*
302100000000     c* Particolarità consegna -uno/due-
302200160127     c                   if        esito='P'
302300160309     c                   eval      f_tasftc = arbtc1
302400160314     c                   eval      f_tastc2 = arbtc2
302500160127     c                   endif
302600160127     c
302700140716if  1c                   IF        f_tasftc <> *blanks OR
302800140716     c                             f_tastc2 <> *blanks
302900140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
303000000000     c                   EXSR      decftc
303100000000     c                   MOVEL     wdftc         d1wdftc
303200140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
303300000000     c                   EXSR      decftc
303400000000     c                   MOVEL     wdftc         d2wdftc
303500000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
303600000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
303700060612     c                   EVAL      we = ' ' +
303800060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
303900000000x   2c                   ELSE
304000000000     c                   EVAL      we = *blanks
304100000000e   2c                   ENDIF
304200060612     c                   EVAL      mftce =
304300060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
304400060612     c                             %TRIM(d1wdftc)             +
304500060612     c                             we                         +
304600060612     c                             %TRIM(d2wdftc)
304700000000     c*
304800010927if  2c                   IF        i < 10                                       *se c'è spazio
304900000000     c                   ADD       1             i                              *memorizza nota
305000000000     c                   EVAL      smnot(i) = mftce
305100010927e   2c                   ENDIF
305200010927e   1c                   ENDIF
305300000000     c*
305400000000     c* Chiusura per turno
305500160127     c                   if        esito='P'
305600160309     c                   eval      f_tasgc1 = arbgc1
305700160309     c                   eval      f_tasgc1 = arbgc2
305800160127     c                   endif
305900160127     c
306000140716if  1c                   IF        f_tasgc1 <> *blanks OR
306100140716     c                             f_tasgc2 <> *blanks
306200140716if  2c                   IF        f_tasgc1 <> *blanks
306300140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
306400000000     c                   EXSR      decgcx
306500000000     c                   MOVEL     wdgcx         d1wdgcx
306600000000e   2c                   ENDIF
306700140716if  2c                   IF        f_tasgc2 <> *blanks
306800140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
306900000000     c                   EXSR      decgcx
307000000000     c                   MOVEL     wdgcx         d2wdgcx
307100000000e   2c                   ENDIF
307200000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
307300000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
307400060612     c                   EVAL      we = ' ' +
307500060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
307600000000x   2c                   ELSE
307700000000     c                   EVAL      we = *blanks
307800000000e   2c                   ENDIF
307900060612     c                   EVAL      mgcxe =
308000060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
308100060612     c                             %TRIM(d1wdgcx)       +
308200060612     c                             we                   +
308300060612     c                             %TRIM(d2wdgcx)
308400000000     c*
308500010927if  2c                   IF        i < 10                                       *se c'è spazio
308600000000     c                   ADD       1             i                              *memorizza nota
308700000000     c                   EVAL      smnot(i) = mgcxe
308800010927e   2c                   ENDIF
308900010927e   1c                   ENDIF
309000000000     c*
309100000000     c* Fermo deposito
309200160127     c                   if        esito='P'
309300160309     c                   eval      f_tasffd = arbffd
309400160127     c                   endif
309500160127
309600140716if  1c                   IF        f_tasffd = 'S'
309700060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
309800000000     c*
309900010927if  2c                   IF        i < 10                                       *se c'è spazio
310000010927     c                   ADD       1             i                              *memorizza nota
310100010927     c                   EVAL      smnot(i) = mffd
310200010927e   2c                   ENDIF
310300010927e   1c                   ENDIF
310400030206     c*---
310500030206     c* memorizza le "note" che NON sono presenti nel record bolla
310600030206     c* ma sul file estensione "anagrafiche"
310700030206     c*---
310800030206     c* N° telefono destinatario
310900160127     c                   if        esito='P'
311000160309     C                   EVAL      MitOrigO74 = arbrmo
311100160127     c                   else
311200140716     c                   Z-ADD     f_tasaas      kaaaas
311300140716     c                   Z-ADD     f_taslnp      kaalnp
311400140716     c                   Z-ADD     f_tasnrs      kaanrs
311500140716     c                   Z-ADD     f_tasnsp      kaansp
311600030206     c                   MOVEL     'O'           kaatrc
311700030206     c     keytaa        CHAIN     titaa30c                           98
311800030206if  1c                   IF        NOT *in98
311900040330     C                   EVAL      MitOrigO74 = TAARSC
312000030206e   1c                   ENDIF
312100160127e   1c                   ENDIF
312200170310     c*
312300170310     c* se ho già usato il mitt originale come RSM --> pulisco il campo
312400170310     c                   if        wusarmo='S'
312500170310     c                   clear                   MitOrigO74
312600170310     c                   endif
312700030206     c*---
312800030206     c* memorizza le "note" che NON sono presenti nel record bolla
312900030206     c* ma sul file estensione testata boolle
313000030206     c*---
313100030206     c* Bancali
313200140716     c                   Z-ADD     f_tasaas      kr5aas
313300140716     c                   Z-ADD     f_taslnp      kr5lnp
313400140716     c                   Z-ADD     f_tasnrs      kr5nrs
313500140716     c                   Z-ADD     f_tasnsp      kr5nsp
313600030206     c                   MOVEL     'BAN'         kr5trd
313700160127     c                   if        esito='P'
313800160127     c     keyar5        CHAIN     fiar501l                           98
313900160127     c                   else
314000040531     c     keyar5        CHAIN     fiar531c                           98
314100160127     c                   endif
314200160127     c
314300030206if  1c                   IF        NOT *in98
314400030206     c                   MOVEL     ar5uni        dar5ban
314500060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
314600060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
314700030206if  2c                   IF        §ar5rb1 > *zeros OR
314800030206     c                             §ar5rb2 > *zeros
314900060613     c                   EVAL      mban = %trim(mban) +
315000060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
315100060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
315200030206e   2c                   ENDIF
315300030206     c*
315400030206if  2c                   IF        i < 10                                       *se c'è spazio
315500030206     c                   ADD       1             i                              *memorizza nota
315600030206     c                   EVAL      smnot(i) = mban
315700030206e   2c                   ENDIF
315800030206e   1c                   ENDIF
315900040330
316000040330     **?Reperisco referente
316100030206     c* Note supermercati
316200140716if  1c     f_tasftc      IFEQ      'S'
316300140716     c     f_tastc2      OREQ      'S'
316400030210     c                   CLEAR                   s                              *indice
316500140716     c                   Z-ADD     f_tasaas      kr5aas
316600140716     c                   Z-ADD     f_taslnp      kr5lnp
316700140716     c                   Z-ADD     f_tasnrs      kr5nrs
316800140716     c                   Z-ADD     f_tasnsp      kr5nsp
316900030206     c                   MOVEL     'SCR'         kr5trd
317000160127     c                   if        esito='P'
317100160127     c     keyar5        SETLL     fiar501l
317200160127     c     keyar5        READE     fiar501l                               98
317300160127     c                   else
317400040531     c     keyar5        SETLL     fiar531c
317500040531     c     keyar5        READE     fiar531c                               98
317600160127     c                   endif
317700160127     c
317800030210do  2c                   DOW       NOT *in98
317900030206     c                   MOVEL     ar5uni        dar5scr
318000030207     c* data nota
318100030210if  3c                   IF        ar5dac > *zeros
318200030207     c                   MOVEL     ar5dac        wdat
318300030207     c                   EXSR      edtdat                                       *edita data
318400030210     c                   MOVEL     wdate         msdace
318500030210e   3c                   ENDIF
318600030207     c* ora nota
318700030210if  3c                   IF        ar5orc > *zeros
318800030207     c                   MOVEL     ar5orc        wora
318900030207     c                   EXSR      edtora                                       *edita ora
319000030210     c                   MOVEL     worae         msorce
319100030210e   3c                   ENDIF
319200030207     c* Consegna richiesta
319300140708if  3c                   IF        §ar5dcr = *zeros AND
319400140708     c                             §ar5hcr = *zeros
319500140708x   3c                   ELSE
319600140708if  4c                   IF        §ar5dcr > *zeros
319700140708     c                   MOVEL     §ar5dcr       wdat
319800140708     c                   EXSR      edtdat                                       *edita data
319900140708     c                   MOVEL     wdate         msdcre
320000140708e   4c                   ENDIF
320100140708if  4c                   IF        §ar5hcr > *zeros
320200140708     c                   MOVEL     §ar5hcr       wora
320300140708     c                   EXSR      edtora                                       *edita ora
320400140708     c                   MOVEL     worae         mshcre
320500140708e   4c                   ENDIF
320600140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
320700140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
320800140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
320900140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
321000140708if  5c                   IF        §ar5tcr = 'P'                                *prima
321100140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
321200140708e   5c                   ENDIF
321300140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
321400140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
321500140708e   5c                   ENDIF
321600140708if  5c                   IF        §ar5tcr = ' '                                *il
321700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
321800140708e   5c                   ENDIF
321900140708e   4c                   ENDIF
322000140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
322100140708     c                             §ar5dcr = *zeros                             *NO data richiesta
322200140708if  5c                   IF        §ar5tcr = 'P'                                *prima
322300140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
322400140708e   5c                   ENDIF
322500140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
322600140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
322700140708e   5c                   ENDIF
322800140708if  5c                   IF        §ar5tcr = ' '                                *il
322900140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
323000140708e   5c                   ENDIF
323100140708e   4c                   ENDIF
323200030210e   3c                   ENDIF
323300030210     c* Riferimento
323400030210if  3c                   IF        §ar5nom <>  *blanks
323500060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
323600060613     c                             + ' ' + §ar5nom
323700030210e   3c                   ENDIF
323800030210     c*
323900030210if  3c                   IF        s < 5                                        *MAX 5 note
324000030210     c                   ADD       1             s
324100030210     c                   EVAL      mscrT(s) = 'Il ' +
324200030210     c                             %trim(msdace) +                              *data nota
324300030210     c                             ' '    +
324400030210     c                             %trim(msorce) +                              *ora nota
324500030210     c                             ' '    +
324600140708     c                             %trim(mstcre) +                              *tipo consegna
324700140708     c                             ' '    +
324800140708     c                             %trim(msdcre) +                              *data consegna
324900140708     c                             ' '    +
325000140708     c                             %trim(mshcre) +                              *ora consegna
325100140708     c                             ' '    +
325200030210     c                             %trim(msnom)  +                              *parlato con
325300030210     c                             ' '    +
325400030210     c                             %trim(§ar5tel) +                             *telefono
325500030210     c                             ' '    +
325600030210     c                             %trim(§ar5mot)                               *motivo
325700030210e   2c                   ENDIF
325800030210     c*
325900160127     c                   if        esito='P'
326000160127     c     keyar5        READE     fiar501l                               98
326100160127     c                   else
326200160127     c     keyar5        READE     fiar531c                               98
326300160127e   1c                   ENDIF
326400030206e   2c                   ENDDO
326500030210e   1c                   ENDIF
326600030210     c* NOTA completa
326700030210if  1c                   IF        mscrT(1) <> *blanks
326800030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
326900030211     c                                      ' ' +
327000030211     C                                      %trim(mscrT(2)) +
327100030211     c                                      ' ' +
327200030211     C                                      %trim(mscrT(3)) +
327300030211     c                                      ' ' +
327400030211     C                                      %trim(mscrT(4)) +
327500030211     c                                      ' ' +
327600030211     C                                      %trim(mscrT(5))
327700030210     c                   Z-ADD     *zeros        s
327800030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
327900030210     c                   ADD       1             i
328000030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
328100030210     c                   EVAL      s = s + 100
328200030210e   2c                   ENDDO
328300030210     c*
328400030210e   1c                   ENDIF
328500010927     c*---
328600010927     c* memorizza le "note" che NON sono presenti nel record bolla
328700160127     c* ma sul file estensione
328800010927     c*---
328900160127     c
329000160127    0c                   if        esito<>'P'
329100010927     c                   CLEAR                   tita4000
329200010927     c                   CLEAR                   tita4p00
329300140716     c                   Z-ADD     f_tasaas      ka4aas
329400140716     c                   Z-ADD     f_taslnp      ka4lnp
329500140716     c                   Z-ADD     f_tasnrs      ka4nrs
329600140716     c                   Z-ADD     f_tasnsp      ka4nsp
329700160127     c
329800010927     c     ke2ta4        SETLL     tita430c
329900010927     c     ke2ta4        READE     tita430c                               98
330000010927do  1c                   DOW       NOT *in98
330100140708     c                   if        ta4trc = '8' OR ta4trc = '9'
330200050701     c*
330300050701if  3c                   IF        ta4trc = '8'
330400050701     c                   EVAL      mmnc8 = ta4not
330500050701e   3c                   ENDIF
330600050701if  3c                   IF        ta4trc = '9'
330700050701     c                   EVAL      mmnc9 = ta4not
330800050701e   3c                   ENDIF
330900050701     c*
331000010927e   2c                   ENDIF
331100010925     c*
331200010926     c     ke2ta4        READE     tita430c                               98
331300010926e   1c                   ENDDO
331400050701     C*
331500140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
331600050701     C*
331700050701     C                   IF        mmnc8 <> *BLANK
331800050701     C                   EVAL      a230 = %TRIML(mmnc8)
331900050701     C                   ENDIF
332000050701     C                   IF        mmnc9 <> *BLANK
332100050701     C                   IF        a230 <> *BLANK
332200050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
332300050701     C                   ELSE
332400050701     C                   EVAL      a230 = %TRIML(mmnc9)
332500050701     C                   ENDIF
332600050701     C                   ENDIF
332700010927     C*
332800050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
332900050701     C*
333000010927if  2c                   IF        i < 10                                       *se c'è spazio
333100010927     c                   ADD       1             i                              *memorizza nota
333200010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
333300010927e   2c                   ENDIF
333400010927     C*
333500010927if  2c                   IF        i < 10                                       *se c'è spazio
333600010927if  3c                   IF        n3 > 100
333700010927     c                   ADD       1             i                              *memorizza nota
333800010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
333900010927e   3c                   ENDIF
334000010927e   2c                   ENDIF
334100010927     C*
334200010927if  2c                   IF        i < 10                                       *se c'è spazio
334300010927if  3c                   IF        n3 > 200
334400010927     c                   ADD       1             i                              *memorizza nota
334500010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
334600010927e   3c                   ENDIF
334700010927e   2C                   ENDIF
334800010927e   1c                   ENDIF
334900160127e   0c                   ENDIF
335000010925     c*
335100000000     c* imposta contatore note da visualizzare nella DS di Output
335200000000     c                   EVAL      cntnoto74 = i
335300000000     c*
335400000000     c* imposta le note nella DS di Output
335500000000do  1c     1             DO        10            j
335600000000     c                   EVAL      nota(j) = smnot(j)
335700000000e   1c                   ENDDO
335800000000     c*
335900000000     c                   ENDSR
336000010716     c*--------------------------------------------------------------------------------------------*
336100010716     c* imposta i campi della DS di Output dalla firma DPD
336200010716     c*--------------------------------------------------------------------------------------------*
336300010716     c     impdsoDPD     BEGSR
336400010717     c*
336500010717     c* reimposta variabili di lavoro
336600010717     c                   RESET                   $AbFirma
336700010717     c                   CLEAR                   depute                         *deposito utente
336800010717     c                   CLEAR                   deppwd                         *deposito password
336900010717     c*---
337000010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
337100010717     c*---
337200010717     c                   Z-ADD     s_taslna      worg
337300010717     c                   EXSR      decorg
337400160406     c
337500020702     c                   IF        wodpd = 'DPD'
337600020702     c                   MOVEL     'S'           dpdo74
337700020702     c                   ELSE
337800020702     c                   MOVEL     *blanks       dpdo74
337900160406     c
338000160406     c* se si tratta di bolla EEX verifico se il partner è abilitato al link
338100160407     c                   IF        wodpd = 'EEX' and ksci74<>*blanks
338200160406     c                   clear                   fidn40ds
338300160406     c                   eval      i40fil=s_taslna
338400160406     c                   z-add     s_tasmgs      i40dsp
338500160406     c                   MOVEL     s_tasaas      i40dsp                          *data spedizione
338600160406     c                   call      'FIDN40R'
338700160406     c                   parm                    fidn40ds
338800160407
338900160406     c                   if        o40lnk='S'
339000160407     c* sommo i giorni alla data spedizione per sapere se ancora visibile
339100160407     c                   Clear                   xgiolavds
339200160407     c                   Eval      Ixglpa  = 'A'
339300160413     c                   Eval      IxglFil = s_taslna
339400160407     c                   Eval      Ixgldata = i40dsp
339500160407     c                   Eval      Ixgladd = 'S'
339600160407     c                   eval      ixglgg  = o40gio
339700160407     c                   Call      'XGIOLAV'
339800160407     c                   parm                    xgiolavds
339900160407     c                   if        oxgldata>=datcor
340000160406     c                   MOVEL     'E'           dpdo74
340100160407     c                   endif
340200160407     c
340300160406     c                   endif
340400160406     c                   endif
340500160406     c
340600020702     c                   ENDIF
340700010717     c*
340800010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
340900060627     c* Riferimento link Lettera di vettura
341000060627     c* Riferimento link Firma
341100010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
341200010717     c                   CLEAR                   tita4000
341300010717     c                   CLEAR                   tita4p00
341400060627     c                   CLEAR                   dsbl4i
341500010717     c                   Z-ADD     s_tasaas      ka4aas
341600010717     c                   Z-ADD     s_taslnp      ka4lnp
341700010717     c                   Z-ADD     s_tasnrs      ka4nrs
341800010717     c                   Z-ADD     s_tasnsp      ka4nsp
341900060627     c                   MOVEL     'I'           ka4trc
342000060627     c     keyta4        CHAIN     tita430c                           98
342100010717if  2c                   IF        NOT *in98
342200060627     c                   MOVEL     ta4not        dsbl4i
342300060627     c                   EVAL      DPDRmnO74 = §b4ipn
342400060627     c                   EVAL      DPDFnpO74 = §b4ipn
342500010717e   2c                   ENDIF
342600010717     c*
342700010717     c* cerca abilitazione firma DPD per questo segnacollo
342800010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
342900010717     c                   EXSR      RepAbilFirma
343000010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
343100010717     c*---
343200010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
343300010717     c*---
343400010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
343500010717     c* dati
343600010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
343700010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
343800010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
343900010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
344000010717     c*
344100010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
344200010717     c                   Z-ADD     s_tasaas      kvbaas
344300010717     c                   Z-ADD     s_taslnp      kvblnp
344400010717     c                   Z-ADD     s_tasnrs      kvbnrs
344500010717     c                   Z-ADD     s_tasnsp      kvbnsp
344600010717     c                   Z-ADD     99999999      kvbdev
344700010717     c                   Z-ADD     9999          kvbhev
344800010717     c     ke2evb        SETGT     fnevb01l
344900010717     c     keyevb        READPE    fnevb01l                               98
345000010717do  4c                   DOW       NOT *in98
345100101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
345200010720     c*
345300060629     c                   EVAL      devB01 = evbNot
345400060628     c                   IF        §NotDep = *BLANK
345500060628     c                   RESET                   DPDFirO74
345600060629     c                   LEAVE
345700060628     c                   ENDIF
345800060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
345900060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
346000080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
346100010720     c                   Z-ADD     *zeros        g08dat
346200010720     c                   MOVEL     '3'           g08err
346300010720     c                   CALL      'XSRDA8'
346400010720     c                   PARM                    wlbda8
346500010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
346600010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
346700010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
346800010720     c*
346900010717     c                   LEAVE                                                  *uscita ciclo
347000010717e   5c                   ENDIF
347100010717     c     keyevb        READPE    fnevb01l                               98
347200010717e   4c                   ENDDO
347300010717     c*
347400010717e   3c                   ENDIF
347500010717e   2c                   ENDIF
347600160323     c
347700160323   x1c                   else
347800010716e   1c                   ENDIF
347900010716     c*
348000010716     c                   ENDSR
348100160309     c*--------------------------------------------------------------------------------------------*
348200160309     c* impostazioni solo da FNBLP
348300160309     c*--------------------------------------------------------------------------------------------*
348400160309     c     impds_soloBLP BEGSR
348500160321     c
348600160321     c                   EVAL      dstaslnp = arblnp
348700160321     c                   EVAL      dstasnrs = arbnrs
348800160321     c                   EVAL      dstasnsp = arbnsp
348900160321     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
349000161117
349100161117     c                   MOVEL     arbaas        aasmgsO74                       *data spedizione
349200161117     c                   MOVE      arbmgs        aasmgsO74
349300161117     c
349400161117     c                   MOVEL     'N'           gco74                          *flag di controllo
349500161117     c                   MOVEL     'N'           gcfdio74
349600161117     c                   MOVEL     'N'           gcmito74
349700161117     c*
349800160314     C
349900160314     c                   Z-ADD     arblna        worg
350000160314     c                   exsr      setLnaItaEst
350100160314     C
350200160309     c* chain ar5 con bolla richiesta a video
350300160309     c                   Z-ADD     kasaas        kr5aas
350400160309     c                   Z-ADD     kaslnp        kr5lnp
350500160309     c                   Z-ADD     kasnrs        kr5nrs
350600160309     c                   Z-ADD     kasnsp        kr5nsp
350700160309     c                   MOVEL     'GEN'         kr5trd
350800160309     c     keyar5        CHAIN     fiar501l
350900160309     C                   IF        %FOUND
351000160309     C                   EVAL      DAR5GEN = AR5Uni
351100160309     C                   ELSE
351200160309     C                   RESET                   DAR5GEN
351300160309     C                   ENDIF
351400160309     C                   EVAL      REFCONSO74 = GEN§AR5REF
351500160309     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
351600160309     c*
351700160309     c* chain tipo bolla
351800160321     c                   eval      tblkey=arbcbo
351900160314     C                   EVAL      ds3A = chainTabel00f('CHAIN3':langTabel
352000160321     C                             :'3A':tblkey:%LEN(tblkey):'TBLUNI':
352100160314     C                             rpyOpCode:rpyEsito)
352200160309     c*
352300160309     c                   MOVEL     §3atb1        a1
352400160309     c                   MOVEL     §3atb1        tpporto74
352500160309if  1c                   IF        a1 = 'A'                                     *Pagamento del
352600160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
352700160309x   1c                   ELSE                                                   *Pagamento del
352800160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
352900160309e   1c                   ENDIF
353000160314     c
353100160314     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
353200160314     C                   IF        KSCI74 <> *BLANK
353300160314     c                   SETOFF                                           98
353400160314     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
353500160314     C                   RESET                   tis7700i0
353600160314     C                   EVAL      tis7700i0.aas = arbAas
353700160314     C                   EVAL      tis7700i0.lnp = arbLnp
353800160314     C                   EVAL      tis7700i0.nrs = arbNrs
353900160314     C                   EVAL      tis7700i0.nsp = arbNsp
354000160314     C                   EVAL      tis7700i0.tbl = §3atb1
354100160314     C                   EVAL      tis7700i0.ksu = kscI74
354200160314     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
354300160314     C***                CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
354400160314     C***                          : rpyEsito
354500160314     C***                          : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
354600160314     C***                          : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
354700160314     C***                          )
354800160314     C***                EVAL      *IN98 = (rpyEsito >= *ZERO AND
354900160314     C***                          tis7700o0.bollValida = *ON)
355000160314     **?Se non gli appartiene, prima di restituire errore controllo il check code.
355100160314if  2c                   IF        NOT *in98                                      *non trovato
355200160314     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
355300160314     C                   reset                   TIS778DSO
355400160818     C*****              EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
355500160818     c                   eval      esito='1'
355600160818     c                   eval      widmsg = 'TIS0849'
355700160818     c                   eval      widcampo = 'NSPEDIZI74'
355800160818     c                   clear                   wparm
355900160818     c                   exsr      messaggi
356000160818     c                   exsr      routend
356100160818     C******             RETURN
356200160314     C                   ENDIF
356300160314e   2c                   ENDIF
356400160314     C                   ENDIF
356500160314
356600160314     c
356700160309     c                   ENDSR
356800160314     c*--------------------------------------------------------------------------------------------*
356900160314     c* impostazioni del C/Assegno
357000160314     c*--------------------------------------------------------------------------------------------*
357100160314     c     ImpdsoAR9     BEGSR
357200160314     c                   Z-ADD     waas          ksbaas
357300160314     c                   Z-ADD     wlnp          ksblnp
357400160314     c                   Z-ADD     wnrs          ksbnrs
357500160314     c                   Z-ADD     wnsp          ksbnsp
357600160314     c     keycsb        CHAIN     fiar901l                           99
357700160314if  2c                   IF        NOT *in99                                    *esistente
357800160314     c* memorizza i dati
357900160314     c                   MOVEL     'S'           cao74                          *esiste C/A
358000160314     c*
358100160314     c                   Z-ADD     ar9cas        caimpo74                       *importo
358200160314     c*
358300160314     c                   EVAL      cadivo74 =  ar9vca                           *divisa
358400160314if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
358500160314     c                             caimpo74 > *zeros
358600160314     c                   EVAL      cadivo74 = 'ITL'
358700160314e   3c                   ENDIF
358800160314
358900160314     ** Visualizzo la descrizione del tipo incasso
359000160314     c**  devo impostare alcuni campi di CSB
359100160314     c                   eval      csbaas=ar9aas
359200160314     c                   eval      csblnp=ar9lnp
359300160314     c                   eval      csbnrs=ar9nrs
359400160314     c                   eval      csbnsp=ar9nsp
359500160314     c                   clear                   csbddc
359600160314     c                   clear                   csbtpa
359700160314     c                   clear                   csbtpi
359800160314     c
359900160314     C                   EVAL      cainco74 =
360000160314     C                                       GetDescrizioneIncassoContrassegno()
360100160314     c* stato C/Assegno : attivo
360200160314     c                   MOVEL     *blanks       castao74                       *stato
360300160314     C                   EVALR     tblKey = '0'
360400160314     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
360500160314     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
360600160314     C                             :rpyOpCode:rpyEsito)
360700160314     C                   IF        rpyOpCode = 'FOUND'
360800160314     c                   EVAL      castao74 = §4SDei
360900160314e   3c                   ENDIF
361000160314
361100160314if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
361200160314     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)     *NO distinta incasso
361300160314     c                   endif
361400160314
361500160314     c                   endif
361600160314     c
361700160314     c                   ENDSR
361800140604     c*--------------------------------------------------------------------------------------------*
361900140604     c* imposta i campi da FNARB se TASDCM = 0
362000140604     c*--------------------------------------------------------------------------------------------*
362100140604     c     impdsoARB     BEGSR
362200150403     c                   clear                   orarich           4 0
362300150611     c                   clear                   wdee
362400170301     c                   clear                   mito74
362500150410     c* memorizzo se utilizzo la data prevista consegna
362600150410     c                   clear                   usaprev           1
362700170301     c                   eval      wbol='ARB'
362800140604     c*
362900140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
363000140604     c                   EVAL      lnaDescO74 = sodes(i)
363100140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
363200140604     c*
363300150116      *eliminata forzatura resta impostato dalla mamma per composizione link
363400150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
363500150116     c*                  MOVE      arbmgs        aasmgsO74
363600140818      *riempimento da verificare
363700140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
363800140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
363900140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
364000140818
364100140818     c                   if        tpporto74 = 'A'
364200140818     c                   if        arbccm > 0
364300140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
364400140818     c                   else
364500140818     c                   movel     arblnp        ccmO74
364600140818     c                   move      8888          ccmO74
364700140818     c                   endif
364800140818     c                   else
364900140818     c                   z-add     arbksc        ccmO74
365000140818     c                   endif
365100140818      *
365200140604     c                   MOVEL     arblna        lineaaro74
365300140604     c                   MOVEL     arbtsp        tpservio74
365400140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
365500140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
365600140604     C                             rpyOpCode:rpyEsito)
365700140604     C                   EVAL      corro74 = §5EDes
365800140604     c*
365900140604     c                   EVAL      destino74 = arbrsd
366000140604     c                   EVAL      desindo74 = arbind
366100140604     c                   EVAL      descapo74 = arbcad
366200140604     c                   EVAL      desloco74 = arblod
366300140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
366400140604if  1c                   IF        arbnzd    = *blanks
366500140604     c                   EVAL      desnazo74 = arbprd
366600140604x   1c                   ELSE
366700150326     c**                 EVAL      desnazo74 = arbnzd
366800150326     c**                 EVAL      desnzdo74 = arbnzd
366900150326     c* 26/03/15: Attiviamo le specifiche per provincia export
367000150326     c                   EVAL      desnazo74 = arbprd
367100150326     c                   EVAL      desnzdo74 = arbnzd
367200150326e   1c                   ENDIF
367300140604     c*
367400140604     c                   EVAL      collio74  = arbncl
367500140604     c                   EVAL      pesoo74   = arbpkf
367600140604     c                   EVAL      volumeo74 = arbvlf
367700140604     c                   EVAL      naturao74 = arbnas
367800140604     c*
367900140604     c                   EVAL      disnumo74  = arbndc
368000140604c    c                   MOVEL     arbddc        wdat
368100140604     c                   EXSR      edtdat
368200140604     c                   EVAL      disdatao74 = wdate
368300140604     c*
368400170310     c                   MOVE      ccmo74        alfa4
368500170310if  1c                   IF        alfa4 <> '8888'                                *codificato
368600170310     c                   clear                   mito74
368700170310     c                   clear                   solomito74
368800170301     c                   exsr      tabcli
368900170310     c                   else
369000140604     c                   EVAL      mito74    = arbrsm
369100170310     c                   endif
369200170310
369300140604     c                   EVAL      mitindo74 = arbinm
369400140604     c                   EVAL      mitcapo74 = arbcam
369500140604     c                   EVAL      mitloco74 = arblom
369600140604      *specifiche da eliminare dopo la modifica in immissione bolle
369700140604if  3c                   IF        arbnzm = *blanks
369800140604     c                   EVAL      mitnazo74 = arbprm
369900140604x   3c                   ELSE
370000140604     c                   EVAL      mitnazo74 = arbnzm
370100140604e   3c                   ENDIF
370200140604     c* 2a ragione sociale destinatario.
370300140604     c                   Z-ADD     arbaas        ka4aas
370400140604     c                   Z-ADD     arblnp        ka4lnp
370500140604     c                   Z-ADD     arbnrs        ka4nrs
370600140604     c                   Z-ADD     arbnsp        ka4nsp
370700140604     c                   MOVEL     'D'           ka4trc
370800140604     c     keyta4        CHAIN     fiar401l                           99
370900140604if  1c                   IF        NOT *in99
371000140604     c                   MOVEL     ar4not        desti2O74
371100140604e   1c                   ENDIF
371200140604     c                   MOVEL     'E'           ka4trc
371300140604     c     keyta4        CHAIN     fiar401l                           99
371400140604if  1c                   IF        NOT *in99
371500140604     c                   MOVEL     ar4not        dsbl4e
371600140604e   1c                   ENDIF
371700140604     c
371800140604     ** Riferimento mittente numerico (c'è sempre).
371900140604     C                   EVAL      rifero74 = arbrmn
372000140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
372100140604     C                   IF        arbrma <> *blank and
372200140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
372300140604     C                   EVAL      rifero74a = arbrma
372400140604     C                   ENDIF
372500140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
372600140604     C                   IF        §b4erp <> *BLANK AND
372700140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
372800140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
372900140604     C                   EVAL      rpto74 = §b4erp
373000140604     C                   ENDIF
373100140604
373200140604     **?Reperisco il check code della spedizione.
373300140604     C                   EVAL      SChkCdeO74 =
373400140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
373500140604     C                             NSpedizI74:ChkCode:nEsito)
373600140604
373700140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
373800141126     C*m                 IF        KSCI74 <> *BLANK
373900141126     c*m                 SETOFF                                           98
374000140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
374100141126     C*m                 RESET                   tis7700i0
374200141126     C*m                 EVAL      tis7700i0.aas = arbAas
374300141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
374400141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
374500141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
374600141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
374700141126     C*m                 EVAL      tis7700i0.ksu = kscI74
374800141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
374900141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
375000141126     C*m                           : rpyEsito
375100141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
375200141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
375300141126     C*m                           )
375400141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
375500141126     C*m                           tis7700o0.bollValida = *ON)
375600140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
375700141126if  2c*m                 IF        NOT *in98                                      *non trovato
375800141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
375900141126     C*m                 reset                   TIS778DSO
376000141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
376100141126     C*m                 RETURN
376200141126     C*m                 ENDIF
376300141126e   2c*m                 ENDIF
376400141126     C*m                 ENDIF
376500140604     c*
376600140604     ** Importo assicurato.
376700140604     C                   IF        arbIas > 0
376800140604     C                   EVAL      vasO74 = arbVas
376900140604     C                   EVAL      iasO74 = arbIas
377000140604     C                   ENDIF
377100140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
377200140605     c                   if        arbdcm = 0 and wcev <> '704'
377300140617     c                   if        arbdcr > 0
377400140605     c                   MOVEL     arbdcr        wdat
377500140605     c                   EXSR      edtdat
377600140605     c                   eval      DTCONRCO74  =  wdate
377700140819if  3c                   IF        arbtcr = ' '
377800140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
377900140819e   3c                   ENDIF
378000140617     c                   endif
378100140617     c                   if        arbhcr > 0
378200140617     c                   MOVEL     arbhcr        wora
378300140617     c                   EXSR      edtora
378400140617     c                   eval      ORACONRO74  =  worae
378500140617     c                   endif
378600140606     c                   eval      TPDATCRO74  =  arbtcr
378700160407     c
378800140605      *reperimento descrizione tipo consegna richiesta
378900140617if  3c                   IF        arbtcr = 'P'
379000140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
379100140617e   3c                   ENDIF
379200140617if  3c                   IF        arbtcr = 'D'
379300140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
379400140617e   3c                   ENDIF
379500160407     c
379600160407     c
379700160407     c* Per  le bolle in partenza eseguo solo se presente la data entrata
379800160407    0c                   if        esito<>'P' or arbdse>0
379900160407     c
380000140605     C                   clear                   tnsd99ds
380100140605     C                   MOVEL     ' '           D98tla
380200160314     c                   if        esito='P'
380300160314     C                   MOVEL     'P'           D98tbo
380400160314     c                   else
380500140605     C                   MOVEL     'A'           D98tbo
380600160314     c                   endif
380700140605     C                   Z-ADD     arbaas        D98aas
380800140605     C                   Z-ADD     arblnp        D98lnp
380900140605     C                   Z-ADD     arbnrs        D98nrs
381000140605     C                   Z-ADD     arbnsp        D98nsp
381100140605     C*
381200140605     C                   call      'TNSD99R'
381300140605     C                   parm                    tnsd99ds
381400150304     c
381500140605     C*
381600140917      * chiamata a trulors per tempi standard
381700140917     c                   clear                   trulorsds
381800140917     c                   clear                   trulor2ds
381900140917     c                   clear                   trulor3ds
382000150611     c                   eval      wdee=d98dee
382100140917      * solo se questa data è valorizzata
382200150304    1c                   if        d98dee > 0
382300140912     c                   movel     d98dee        wdat
382400140605     c                   exsr      edtdat
382500140605     c                   eval      DTTEOCOO74  =  wdate
382600140819     c
382700140904      * se fermo deposito non richiamo pgm per reperimento orari
382800150304    2c                   if         arbffd = *blank
382900140605     c                   eval       IOREtser = 'C'
383000140605     c                   eval       IOREDDC=arbddc
383100140605     c                   eval       IOREnDC=arbndc
383200140609     c                   eval       IOREfil=arblna
383300140609     c                   eval       IOREdta=d98dce
383400140609     c                   eval       IOREcap=arbcad
383500140609     c                   eval       IOREloc=arblod
383600151019     c*****              eval       IOREtfp=arbtfp
383700151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
383800151019     c* del terminal di partenza originale
383900151019     c                   if         d98flp>*zeros
384000151019     c                   eval       IOREtfp=%INT(d98flp)
384100151019     c                   else
384200151019     c                   eval       IOREtfp=arbtfp
384300151019     c                   endif
384400140609     c                   eval       IOREtfa=arbtfa
384500140610     c                   eval       IOREdti=arbdti
384600140610     c                   eval       IOREhti=arbhti
384700140610     c                   eval       IOREdcr=arbdcr
384800140610     c                   eval       IOREhcr=arbhcr
384900140610     c                   eval       IOREtcr=arbtcr
385000140819     c                   eval       IOREtsp=arbtsp
385100140819     c                   eval       IOREnar=arbnzd
385200140819     c                   eval       IOREdei=d98dei
385300140819     c                   eval       IOREoei=d98oei
385400150819     c                   clear                   diore01
385500150819     c                   movel     d98ttc        §ioretrazC
385600150819     c                   movel     d98tcc        §ioreconsC
385700150819     c                   eval       IOREflo=diore01
385800150325     c* passo la località normalizzata
385900150325     c                   clear                   tita4000
386000150325     c                   clear                   tita4p00
386100150325     c                   Z-ADD     arbaas        ka4aas
386200150325     c                   Z-ADD     arblnp        ka4lnp
386300150325     c                   Z-ADD     arbnrs        ka4nrs
386400150326     c                   Z-ADD     arbnsp        ka4nsp
386500150325     c                   MOVEL     'X'           ka4trc
386600160314     c                   if        esito='P'
386700160314     c     keyta4        CHAIN     fiar401l                           99
386800160314if  1c                   IF        NOT *in99
386900160314     c                   MOVEL     ar4not        OOR3LOC_N
387000160314     c                   else
387100160314     c                   movel     arblod        OOR3LOC_N
387200160314e   1c                   ENDIF
387300160314     c
387400160314     c                   else
387500160314     c     keyta4        CHAIN     tita430c                           99
387600150325if  1c                   IF        NOT *in99
387700150325     c                   MOVEL     ta4not        OOR3LOC_N
387800150325     c                   else
387900150325     c                   movel     arblod        OOR3LOC_N
388000150325e   1c                   ENDIF
388100160314e   1c                   ENDIF
388200140605     c
388300160321     c                   if        esito<>'P'
388400150304    3c                   IF         (ARBfbc = *blanks and
388500140605     c                               ARBddc > 0 and
388600140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
388700140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
388800140605     c                   eval       IOREcons = 'S'
388900150304    3c                   endif
389000160321     c
389100140605     c* Orario presunto ritiro/consegna
389200140605     c                   move      arbndc        pctndc
389300140605     c                   move      arbifp        pctfgs
389400140605     c                   move      'COR'         ttrd
389500140605     c     kcet          setgt     fipct02l
389600140605     c     kcet          readpe    fipct02l
389700150304    3c                   if        not %eof(fipct02l)
389800140605     c                   movel     pctdati       fipctcords
389900140605     c                   eval      ioreorp=§PCTORASTI
390000140605     c                   endif
390100160321    3c                   endif
390200160321
390300140605     c                   call      'TRULORSR'
390400140605     C                   PARM                    kpjba
390500140605     C                   PARM                    trulorsds
390600140605     C                   PARM                    trulor2ds
390700140605     C                   PARM                    trulor3ds
390800150304    3c                   endif
390900140818     c                   clear                   ORSTINIO74
391000150304    3c                   if        OOR2STIS > 0
391100140617     c                   MOVEL     OOR2STIS      wora
391200140617     c                   EXSR      edtora
391300140617     c                   eval      ORSTINIO74  =  worae
391400150304    3c                   endif
391500140818     c                   clear                   ORSTFINO74
391600150304    3c                   if        OOR2STFS > 0
391700140617     c                   MOVEL     OOR2STFS      wora
391800140617     c                   EXSR      edtora
391900140617     c                   eval      ORSTFINO74  =  worae
392000150304    3c                   endif
392100140818     c                   clear                   ORMIINIO74
392200150304    3c                   if        OOR2MIIS > 0
392300140617     c                   MOVEL     OOR2MIIS      wora
392400140617     c                   EXSR      edtora
392500140617     c                   eval      ORMIINIO74  =  worae
392600150304    3c                   endif
392700140818     c                   clear                   ORMAFINO74
392800150304    3c                   if        OOR2MXFS > 0
392900140617     c                   MOVEL     OOR2MXFS      wora
393000140617     c                   EXSR      edtora
393100140617     c                   eval      ORMAFINO74  =  worae
393200150304    3c                   endif
393300150304    3c                   if        wricons = 'S'
393400140818     c                   clear                   ORRIDALO74
393500150304    4c                   if        OOR2RIDS > 0
393600140617     c                   MOVEL     OOR2RIDS      wora
393700140617     c                   EXSR      edtora
393800140617     c                   eval      ORRIDALO74  =  worae
393900150304    4c                   endif
394000140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
394100140818     c                   clear                   ORRIALLO74
394200150304    4c                   if        OOR2RIAS > 0
394300140617     c                   MOVEL     OOR2RIAS      wora
394400140617     c                   EXSR      edtora
394500140617     c                   eval      ORRIALLO74  =  worae
394600150304    4c                   endif
394700140617     c                   eval      FLRICONO74  =  OOR2FRIC
394800150304   x3c                   else
394900140711     c                   clear                   ORRIALLO74
395000140711     c                   clear                   ORRIDALO74
395100140711     c                   eval      FLRICONO74  =  'N'
395200150304    3c                   endif
395300140818     c                   clear                   ORSTICDO74
395400150304    3c                   if        OOR2PRESD > 0
395500140617     c                   MOVEL     OOR2PRESD     wora
395600140617     c                   EXSR      edtora
395700140617     c                   eval      ORSTICDO74  =  worae
395800150304    3c                   endif
395900140818     c                   clear                   ORSTICAO74
396000150304    3c                   if        OOR2PRESa > 0
396100140617     c                   MOVEL     OOR2PRESa     wora
396200140617     c                   EXSR      edtora
396300140617     c                   eval      ORSTICAO74  =  worae
396400150304    3c                   endif
396500140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
396600140605     c                   eval      FLLOCNOO74  =  OOR3NORM
396700140904      *end F.D.
396800150304    2c                   endif
396900140605      * RIFERIMENTI DESTINATARIO
397000140605     c                   MOVEL     'EMD'         kr5trd
397100140605     c     keyAR5        CHAIN     fiar501l
397200150304if  2c                   IF        %found(fiar501l)
397300140605     c                   MOVEL     ar5uni        dar5emd
397400140606     c                   eval      EMLDESTO74  =  §§AR5EML
397500140606     c                   eval      CELDESTO74  =  §§AR5TEL
397600150304e   2c                   ENDIF
397700140604     **
397800150304     c*
397900150304     c* Stato data prevista consegna
398000150305     c                   clear                   stat_n2o74
398100150305     c                   eval      STATUSo74   =  D98SPCDEE
398200150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
398300150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
398400150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
398500150312     c                   eval      statuso74='RIC'
398600150403     c* se presente FIARP --> visualizzo la data richiesta
398700150403     c     karp          chain     fiarp01l
398800150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
398900150611     c                   movel     arpdcr        wdee
399000150611     c                   movel     arpdcr        wdat
399100150403     c                   exsr      edtdat
399200150403     c                   eval      DTTEOCOO74  =  wdate
399300150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
399400150421    3C                   IF        ARPHCR>0
399500150403     c                   eval      orarich=arphcr
399600150421    3c                   endif
399700150421   x2c                   else
399800150403     c*  se presente DOPO IL imposto la data in D98dee
399900150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
400000150403     c                   eval      D98dee=arpdcr
400100150611     c                   eval      wdee=arpdcr
400200150421    3c                   endif
400300150403     c* PRIMA DEL come se non ci fosse...
400400150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
400500150421    3c                   if        d98dee>0
400600150312     c                   Clear                   xgiolavds
400700150312     c                   Eval      Ixglpa  = 'A'
400800150312     c                   Eval      IxglFil = arblna
400900150312     c                   Eval      Ixgldata = d98dee
401000150312     c                   Eval      Ixgladd = 'S'
401100150312     c                   Eval      Ixgllav = 'S'
401200150312     c                   eval      ixglgg  = 1
401300150312     c                   Call      'XGIOLAV'
401400150312     c                   parm                    xgiolavds
401500150611     c                   movel     oxgldata      wdee
401600150611     c                   movel     oxgldata      wdat
401700150312     c                   exsr      edtdat
401800150312     c                   eval      DTTEOCOO74  =  wdate
401900150421    3c                   endif
402000150421    2c                   endif
402100150421    1c                   endif
402200150421     c
402300150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
402400150421     c                   eval      statuso74='ATT'
402500150421     c                   endif
402600150421    1c                   if        PDAgia='S' and statuso74='DDC'
402700150421     c                   eval      statuso74='GIA'
402800150421     c                   endif
402900170320     c* Se lo stato è : in distinta e ho FIARP, aggiungo una scritta
403000170320     c                   if        statuso74='DDC'
403100170320     c     karp          chain     fiarp01l
403200170320    2c                   if        %found(fiarp01l) and arpdcr>0 and
403300170320     c                             arpdcr>datcor  and
403400170320     c                             (arptcr=' ' or  arptcr='D')
403500170320     c                   eval      statuso74='DDP'
403600170320     c                   endif
403700170320     c                   endif
403800170320     c
403900150401     c
404000150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
404100150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
404200150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
404300150401     c* La data contatto = data distinta + 1
404400150512     c**                 Clear                   xgiolavds
404500150512     c**                 Eval      Ixglpa  = 'A'
404600150512     c**                 Eval      IxglFil = arblna
404700150512     c**                 Eval      Ixgldata = arbddc
404800150512     c**                 Eval      Ixgladd = 'S'
404900150512     c**                 Eval      Ixgllav = 'S'
405000150512     c**                 eval      ixglgg  = 1
405100150512     c**                 Call      'XGIOLAV'
405200150512     c**                 parm                    xgiolavds
405300150512     c**                 movel     oxgldata      wdat
405400150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
405500150512     c**                 endif
405600150317     c
405700150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
405800150326     c*  lo status diventa RIC
405900150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
406000150317     c                   eval      statuso74='RIC'
406100150317     c                   endif
406200150310     c
406300150304     c* decodifica stato da mettere nei campi deco e note
406400150304     c                   clear                   dspc
406500150304     C                   RESET                   tibs02ds
406600150304     C                   EVAL      t02Cod = 'SPC'
406700150312     C                   EVAL      t02Ke1 = statuso74
406800150306     C                   EXSR      getTntbe00f
406900150306     c                   IF        t02Err <>'E'
407000150306     c
407100150305     c                   eval       stat_d1o74=§SPCDEC1
407200150305     c                   eval       stat_d2o74=§SPCDEC2
407300150304     c*
407400150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
407500150305     c                   eval       stat_n1o74=§SPCNOT1
407600150306     c                   eval       stat_n2o74=§SPCNOT2
407700150304     c                   else
407800150305     c                   clear                   stat_d1o74
407900150305     c                   clear                   stat_d2o74
408000150305     c                   clear                   stat_n1o74
408100150306     c                   clear                   stat_n2o74
408200150304     c                   endif
408300160407
408400160407     c                   endif
408500160407
408600150409     c                   else
408700150409     c* stato evento CONSEGNATO
408800150409     c                   eval      statuso74='CON'
408900150415     c                   eval      flgbolcO74  =  'S'
409000150415     c                   eval      flgboldO74  =  'N'
409100150409    1c                   endif
409200150304     c
409300150304     c
409400140919     ** riempimento per campi da arb con data consegna diversa da 0
409500140919     c                   if        arbdcm > 0
409600140919     c                   movel     arbdcm        wdat
409700140919     c                   exsr      edtdat
409800140919     c                   eval      dtconmeo74  = wdate
409900140919     c                   MOVEL     arbdcm        wora
410000140919     c                   EXSR      edtora
410100140919     c                   eval      orconmeO74  =  worae
410200140919     c                   eval      flgbolcO74  =  'S'
410300140919     c                   eval      flgboldO74  =  'N'
410400141015      *firmatario distinta PDA
410500141015     c                   clear                   firmato74
410600141015     c                   if        gen§ar5pdaco <> 'NO'
410700141015     c                   exsr      repfirmatARB
410800141015     c                   else
410900141015     c                   if        arbgma <> *blank
411000141015     C                   EVAL      tblKey = arbgma
411100141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
411200141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
411300141015     C                             :rpyOpCode:rpyEsito)
411400141015     C                   IF        rpyOpCode = 'FOUND'
411500141015     c                   if        §7r2fi = 'S'
411600141015     c                   exsr      repfirmatARB
411700141015     c                   endif
411800141015     c                   endif
411900141015     c                   endif
412000141015     c                   endif
412100140919     c                   else
412200150415     c* solo se non impostato da PdA lo stato CONSEGNA
412300150415     c                   if        statuso74<>'CON'
412400150415     c
412500140919     c                   eval      flgbolcO74  =  'N'
412600140919     c                   if        arbndc > 0 and arbddc > 0
412700150415     c                             and arbddc <= datcor and d98cons='S'
412800140919     c                   eval      flgboldo74 = 'S'
412900140919     c                   else
413000140919     c                   eval      flgboldO74  =  'N'
413100140919     c                   endif
413200150415     c                   endif
413300150415
413400140919     c                   endif
413500150415     c
413600150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
413700140919     c                   eval      orincono74 = orsticdo74
413800140919     c                   eval      orficono74 = ORSticaO74
413900140919     c                   else
414000140919     c                   eval      orincono74 = ORSTINIO74
414100140919     c                   eval      orficono74 = ORSTFINO74
414200140919     c                   endif
414300150326     c* note 1
414400150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
414500150309     c                   clear                   wparm
414600150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
414700150326     c                   if        D98DTCTD<>*blanks
414800150326     c                   movel     D98DTCTD      wdat
414900150326     c                   exsr      edtdat
415000150326     c                   else
415100150326     c                   eval      wdate=dtteocoo74
415200150410     c                   eval      usaprev='S'
415300150326     c                   endif
415400150326     c
415500150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
415600150309     c                   eval      stat_n1o74= rtvMsgLang(
415700150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
415800150309     c                   endif
415900150309     c* note 2
416000150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
416100150309     c                   clear                   wparm
416200150403     c* Se lo stato è PRG passo orario limite uscita aut
416300150403     c                   select
416400170320     c                   when      statuso74='PRG'
416500150326     c                   IF        d98picklim  >*zeros
416600150309
416700150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
416800150326     c                                   (%subst(d98picklim:3:2))
416900150309     c                   else
417000150309     c                   eval      wparm='09:00'
417100150403     c* reperisco orario di fine picking
417200150403     c* filiale / data
417300150403     C                   RESET                   tibs02ds
417400150403     C                   EVAL      t02Cod = 'OLP'
417500150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
417600150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
417700150403     C                   EXSR      getTntbe00f
417800150403     c* filiale
417900150403     c                   IF        t02Err = 'E'
418000150403     C                   RESET                   tibs02ds
418100150403     C                   EVAL      t02Cod = 'OLP'
418200150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
418300150403     C                   EXSR      getTntbe00f
418400150403     c                   endif
418500150403     c* Generico 000
418600150403     c                   IF        t02Err = 'E'
418700150403     C                   RESET                   tibs02ds
418800150403     C                   EVAL      t02Cod = 'OLP'
418900150403     C                   EVAL      t02Ke1 = '000'
419000150403     C                   EXSR      getTntbe00f
419100150403     c                   endif
419200150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
419300150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
419400150403     c                             %subst(t02uni:3:2)
419500150309     c                   endif
419600150403     c                   endif
419700150403     c                   eval      stat_n2o74= rtvMsgLang(
419800150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
419900170320     c
420000170320     c* Se lo stato è DDP passo la data cons richiesta di fiarp
420100170320    2c                   when      statuso74='DDP'
420200170320     c                   movel     arpdcr        wdat
420300170320     c                   exsr      edtdat
420400170320                         eval      wparm=wdate ;
420500170320     c                   eval      stat_n2o74= rtvMsgLang(
420600170320     c                             %subst(stat_n2o74:1:7):langi74:wparm)
420700150403     c                   other
420800150403     c* altri casi
420900150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
421000150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
421100150611     c                             arbtcr=' ' and arbdcr>0  and
421200150611     c                             arbdcr>=wdee
421300150410     c
421400150403     c                   eval      orarich=arbhcr
421500150410     c                   endif
421600150403     c
421700150403     c                   if        orarich>0
421800150403     c                   exsr      sr_dallealle
421900150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
422000150403     c                   eval      stat_n2o74= rtvMsgLang(
422100150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
422200150403     c
422300150403     c                   else
422400150403     c                   clear                   stat_n2o74
422500150403     c                   endif
422600150403     c
422700150403     c                   endsl
422800150403     c
422900150309     c                   endif
423000150410     c
423100150424     c* Se si tratta di fermo deposito pulisco sempre
423200150424     c                   if        usaprev<>'S' or arbffd='S'
423300150410     c                   clear                   DTTEOCOO74
423400150410     c                   clear                   ORINCONO74
423500150410     c                   clear                   ORFICONO74
423600150410     c                   endif
423700150309     c
423800170301     c                   clear                   wbol              3
423900140604     c                   ENDSR
424000150403      /free
424100150403       //--------------------------------------------------------------
424200150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
424300150403       //--------------------------------------------------------------
424400150403       BEGSR  sr_dallealle;
424500150403       clear  w_dallemsg ;
424600150403       clear  w_allemsg ;
424700150403
424800150403       // Reperimento dati tabella VPO - ORARISER
424900150403       if §VPORST_MW=0  ;
425000150403       clear tibs02ds;
425100150403       clear dvpooraris;
425200150403       t02mod='C';
425300150403       t02cod='VPO';
425400150403       t02ke1='ORARISER';
425500150403       tibs02r(kpjba:tibs02ds);
425600150403       if t02err=*blanks;
425700150403          dvpooraris=t02uni;
425800150403       endif;
425900150403       endif;
426000150403
426100150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
426200150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
426300150403       //   determino orari servizio comprensivi della tolleranza
426400150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
426500150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
426600150403
426700150403            if w_dalle<w_dalleoser;
426800150403               w_dalle=w_dalleoser;
426900150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
427000150403            endif;
427100150403
427200150403            if w_alle>w_alleoser;
427300150403               w_alle=w_alleoser;
427400150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
427500150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
427600150403               endif;
427700150403            endif;
427800150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
427900150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
428000150403
428100150403       endsr;
428200150403      /end-free
428300010717     c*--------------------------------------------------------------------------------------------*
428400010717     c* imposta i campi della DS di Output dagli eventi
428500010717     c*--------------------------------------------------------------------------------------------*
428600010717     c     RepAbilFirma  BEGSR
428700010717     c*
428800010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
428900010717     c* e recupera utente / password
429000010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
429100010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
429200010717     c     keyvtd        SETLL     tivtd01l
429300010717     c     keyvtd        READE     tivtd01l                               98
429400010717do  2c                   DOW       NOT *in98
429500010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
429600010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
429700010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
429800010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
429900010717     c                   EVAL      depute = vtdute                              *utente
430000010717     c                   EVAL      deppwd = vtdpwd                              *password
430100010717     c                   LEAVE                                                  *esce dal ciclo
430200010717e   3c                   ENDIF
430300010717     c     keyvtd        READE     tivtd01l                               98
430400010717e   2c                   ENDDO
430500010717e   1C                   ENDIF
430600010717     c*
430700010717     c                   ENDSR
430800000000     c*--------------------------------------------------------------------------------------------*
430900000000     c* edita data da aaaammgg in gg/mm/aaaa
431000000000     c*--------------------------------------------------------------------------------------------*
431100000000     c     edtdat        BEGSR
431200000000     c*
431300000000     c                   MOVEL     *blanks       wdate
431400000000     c*
431500000000     c                   MOVEL     wdat          dsdat
431600000000     c                   MOVEL     dsgio         dsgioe
431700140528     c                   MOVEL     '.'           dsvd1
431800000000     c                   MOVEL     dsmes         dsmese
431900140528     c                   MOVEL     '.'           dsvd2
432000000000     c                   MOVEL     dsann         dsanne
432100000000     c                   MOVEL     dsdate        wdate
432200000000     c*
432300000000     c                   ENDSR
432400000000     c*--------------------------------------------------------------------------------------------*
432500000000     c* edita ora da hhmm in hh:mm
432600000000     c*--------------------------------------------------------------------------------------------*
432700000000     c     edtora        BEGSR
432800000000     c*
432900000000     c                   MOVEL     *blanks       worae
433000000000     c*
433100000000     c                   MOVEL     wora          dsora
433200000000     c                   MOVEL     dshh          dshhe
433300140617     c                   MOVEL     '.'           dsvo1
433400000000     c                   MOVEL     dsmm          dsmme
433500000000     c                   MOVEL     dsorae        worae
433600000000     c*
433700000000     c                   ENDSR
433800000000     c*--------------------------------------------------------------------------------------------*
433900000000     c* decodifica punto operativo
434000000000     c*--------------------------------------------------------------------------------------------*
434100000000     c     decorg        BEGSR
434200000000     c*
434300000000     c                   MOVEL     *blanks       wodes                          *descrizione
434400000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
434500000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
434600000000     c*
434700000000     c                   SETOFF                                           98
434800041111     c                   Z-ADD     1             O
434900041111     c     worg          LOOKUP    sorg(O)                                98
435000000000if  1c                   IF        *in98                                        *trovata
435100121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
435200121001     c                             %EDITC(wOrg : 'X') + ')'
435300041111     c                   MOVEL     sofl1(O)      wofl1
435400041111     c                   MOVEL     sodpd(O)      wodpd
435500000000e   1c                   ENDIF
435600000000     c*
435700000000     c                   ENDSR
435800000000     c*--------------------------------------------------------------------------------------------*
435900000000     c* decodifica evento per STATI
436000000000     c*--------------------------------------------------------------------------------------------*
436100000000     c     deccevsta     BEGSR
436200000000     c*
436300000000     c                   MOVEL     '0'           werr                           *NO errore
436400050704     c*
436500050704     C                   IF        wcev = 'GEN'
436600050704     C                   EVAL      werr = *ON
436700050704     C                   CLEAR                   wcdex
436800050704     C                   LEAVESR
436900050704     C                   ENDIF
437000000000     c*
437100060622     C                   RESET                   tibs02ds
437200060622     C                   EVAL      t02Cod = 'ICE'
437300060622     C                   EVAL      t02Ke1 = wcev
437400060622     C                   EXSR      getTntbe00f
437500060622     C                   IF        t02Err <> 'E' AND
437600140528     C                             (§icesta = 'S'  OR
437700140528     C                               §icestaP = 'S' )
437800060622     c                   MOVEL     §icedei       wcdex
437900000000x   1c                   ELSE                                                   *non trovato
438000000000     c                   MOVEL     '1'           werr                           *SI errore
438100030130     c                   MOVEL     *blanks       wcdex
438200000000e   1c                   ENDIF
438300000000     c*
438400000000     c                   ENDSR
438500000000     c*--------------------------------------------------------------------------------------------*
438600000000     c* decodifica consegna particolare
438700000000     c*--------------------------------------------------------------------------------------------*
438800000000     c     decftc        BEGSR
438900000000     c*
439000000000     c                   MOVEL     *blanks       wdftc
439100000000     c*
439200000000if  1c                   IF        wftc = 'S'
439300060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
439400000000e   1c                   ENDIF
439500000000if  1c                   IF        wftc = 'P'
439600060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
439700000000e   1c                   ENDIF
439800000000if  1c                   IF        wftc = 'A'
439900060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
440000000000e   1c                   ENDIF
440100000000if  1c                   IF        wftc = 'F'
440200060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
440300000000e   1c                   ENDIF
440400000000     c*
440500000000     c                   ENDSR
440600000000     c*--------------------------------------------------------------------------------------------*
440700000000     c* decodifica turno di chiusura
440800000000     c*--------------------------------------------------------------------------------------------*
440900000000     c     decgcx        BEGSR
441000000000     c*
441100000000     c                   MOVEL     *blanks       wdgcx
441200000000     c* giorno
441300000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
441400000000if  1c                   IF        a1 = '1'
441500060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
441600000000e   1c                   ENDIF
441700000000if  1c                   IF        a1 = '2'
441800060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
441900000000e   1c                   ENDIF
442000000000if  1c                   IF        a1 = '3'
442100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
442200000000e   1c                   ENDIF
442300000000if  1c                   IF        a1 = '4'
442400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
442500000000e   1c                   ENDIF
442600000000if  1c                   IF        a1 = '5'
442700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
442800000000e   1c                   ENDIF
442900000000if  1c                   IF        a1 = '6'
443000060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
443100000000e   1c                   ENDIF
443200000000if  1c                   IF        a1 = '7'
443300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
443400000000e   1c                   ENDIF
443500000000if  1c                   IF        a1 = ' '
443600000000     c                   EVAL      wdgcx = *blanks
443700000000e   1c                   ENDIF
443800000000     c* am/pm
443900000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
444000000000if  1c                   IF        a1 = ' '
444100060612     c                   EVAL      wdgcx =
444200060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
444300000000e   1c                   ENDIF
444400000000if  1c                   IF        a1 = 'M'
444500060612     c                   EVAL      wdgcx =
444600060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
444700000000e   1c                   ENDIF
444800000000if  1c                   IF        a1 = 'P'
444900060612     c                   EVAL      wdgcx =
445000060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
445100000000e   1c                   ENDIF
445200000000     c*
445300000000     c                   ENDSR
445400000000     c*--------------------------------------------------------------------------------------------*
445500000000     c* decodifica evento per GIACENZE
445600000000     c*--------------------------------------------------------------------------------------------*
445700000000     c     deccevgia     BEGSR
445800000000     c*
445900000000     c                   MOVEL     '0'           werr                           *NO errore
446000000000     c*
446100060622     C                   RESET                   tibs02ds
446200060622     C                   EVAL      t02Cod = 'ICE'
446300060622     C                   EVAL      t02Ke1 = wcev
446400060622     C                   EXSR      getTntbe00f
446500060622     C                   IF        t02Err <> 'E' AND                            *trovata
446600140528     C                             (§icegia = 'S'  OR
446700140528     C                               §icegiap = 'S')
446800060622     c                   MOVEL     §icedei       wcdex
446900000000x   1c                   ELSE                                                   *non trovato
447000000000     c                   MOVEL     '1'           werr                           *SI errore
447100030130     c                   MOVEL     *blanks       wcdex
447200000000e   1c                   ENDIF
447300000000     c*
447400000000     c                   ENDSR
447500000000     c*--------------------------------------------------------------------------------------------*
447600000000     c* Controlla la DS dsi Input
447700000000     c*--------------------------------------------------------------------------------------------*
447800000000     c     chkdsinput    BEGSR
447900000000     c*
448000000000     c                   MOVEL     '0'           werr                           *NO errore
448100000000     c*
448200000000     c* dalla DS di Input scompone il codice spedizione
448300000000     c                   MOVEL     nspedizi74    dsspedizi74
448400000000     c*
448500000000     c* controlla che la lina di partenza sia TUTTA numerica
448600000000     c                   SETOFF                                       99
448700000000     c                   TESTN                   dslnpi74             99
448800000000if  1c                   IF        NOT *in99                                    *non è un numero
448900000000     c                   MOVEL     '1'           werr                           *SI errore
449000140617     c                   eval      widmsg = 'TIS0769'
449100140617     c                   eval      widcampo = '*LNP'
449200140617     c                   clear                   wparm
449300140617     c                   exsr      messaggi
449400000000x   1c                   ELSE                                                   *è un numero
449500000000     c                   MOVE      dslnpi74      a1
449600000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
449700000000     c                   MOVEL     '1'           werr                           *SI errore
449800140617     c                   eval      widmsg = 'TIS0769'
449900140617     c                   eval      widcampo = '*LNP'
450000140617     c                   clear                   wparm
450100140617     c                   exsr      messaggi
450200000000e   2c                   ENDIF
450300000000e   1c                   ENDIF
450400000000     c*
450500000000     c* controlla che la serie spedizione sia TUTTA numerica
450600000000     c                   SETOFF                                       99
450700000000     c                   TESTN                   dsnrsi74             99
450800000000if  1c                   IF        NOT *in99                                    *non è un numero
450900000000     c                   MOVEL     '1'           werr                           *SI errore
451000140617     c                   eval      widmsg = 'TIS0769'
451100140617     c                   eval      widcampo = '*NRS'
451200140617     c                   clear                   wparm
451300140617     c                   exsr      messaggi
451400000000x   1c                   ELSE                                                   *è un numero
451500000000     c                   MOVE      dsnrsi74      a1
451600000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
451700000000     c                   MOVEL     '1'           werr                           *SI errore
451800140617     c                   eval      widmsg = 'TIS0769'
451900140617     c                   eval      widcampo = '*NRS'
452000140617     c                   clear                   wparm
452100140617     c                   exsr      messaggi
452200000000e   2c                   ENDIF
452300000000e   1c                   ENDIF
452400000000     c*
452500000000     c* controlla che il numero spedizione sia TUTTO numerico
452600000000     c                   SETOFF                                       99
452700000000     c                   TESTN                   dsnspi74             99
452800000000if  1c                   IF        NOT *in99                                    *non è un numero
452900000000     c                   MOVEL     '1'           werr                           *SI errore
453000140617     c                   eval      widmsg = 'TIS0769'
453100140617     c                   eval      widcampo = '*NSP'
453200140617     c                   clear                   wparm
453300140617     c                   exsr      messaggi
453400000000x   1c                   ELSE                                                   *è un numero
453500000000     c                   MOVE      dsnspi74      a1
453600000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
453700000000     c                   MOVEL     '1'           werr                           *SI errore
453800140617     c                   eval      widmsg = 'TIS0769'
453900140617     c                   eval      widcampo = '*NSP'
454000140617     c                   clear                   wparm
454100140617     c                   exsr      messaggi
454200000000e   2c                   ENDIF
454300000000e   1c                   ENDIF
454400110104     c*
454500110104     c* controlla che il codice cliente sia TUTTO numerico
454600110104     c                   IF        kscI74 <> *BLANK
454700110104     c                   SETOFF                                       99
454800110104     c                   TESTN                   kscI74               99
454900110104     c                   IF        NOT *in99                                    *non è un numero
455000110104     c                   MOVEL     '1'           werr                           *SI errore
455100140617     c                   eval      widmsg = 'TIS0769'
455200140617     c                   eval      widcampo = 'KSCI74'
455300140617     c                   clear                   wparm
455400140617     c                   exsr      messaggi
455500110104     c                   ELSE                                                   *è un numero
455600110104     c                   MOVE      kscI74        a1
455700110104     c                   IF        a1 < '0'                                     *trovata una lettera
455800110104     c                   MOVEL     '1'           werr                           *SI errore
455900140617     c                   eval      widmsg = 'TIS0769'
456000140617     c                   eval      widcampo = 'KSCI74'
456100140617     c                   clear                   wparm
456200140617     c                   exsr      messaggi
456300110104     c                   ENDIF
456400110104     c                   ENDIF
456500110104     c                   ENDIF
456600110104     c*
456700110104     c* controlla che il codice mittente sia TUTTO numerico
456800110104     c                   IF        ccmI74 <> *BLANK
456900110104     c                   SETOFF                                       99
457000110104     c                   TESTN                   ccmI74               99
457100110104     c                   IF        NOT *in99                                    *non è un numero
457200110104     c                   MOVEL     '1'           werr                           *SI errore
457300140617     c                   eval      widmsg = 'TIS0769'
457400140617     c                   eval      widcampo = 'CCMI74'
457500140617     c                   clear                   wparm
457600140617     c                   exsr      messaggi
457700110104     c                   ELSE                                                   *è un numero
457800110104     c                   MOVE      ccmI74        a1
457900110104     c                   IF        a1 < '0'                                     *trovata una lettera
458000110104     c                   MOVEL     '1'           werr                           *SI errore
458100140617     c                   eval      widmsg = 'TIS0769'
458200140617     c                   eval      widcampo = 'CCMI74'
458300140617     c                   clear                   wparm
458400140617     c                   exsr      messaggi
458500110104     c                   ENDIF
458600110104     c                   ENDIF
458700110104     c                   ENDIF
458800050104     c*
458900050104     c* controlla anno spedizione
459000050104     c                   TESTN                   cAASI74              99
459100050104if  1c                   IF        NOT *in99                                    *non è un numero
459200050104     c                   EVAL      cAASI74 = *ZERO
459300050104x   1c                   ENDIF
459400100222     c*
459500100223     c* Estraggo l'anno dalla data spedizione.
459600100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
459700100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
459800100223     C                   IF        dspI74 <> *BLANK
459900100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
460000100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
460100100223     C                   MONITOR
460200100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
460300100223     C                   ON-ERROR
460400100223     C                   ENDMON
460500100222     C                   ENDIF
460600170314
460700170314     c*  BRTCODE
460800170314     c                   clear                   wbrtcode
460900170315     c                   if        versioni74>='D'
461000170314     c                   eval      wbrtcode=brtcodei74
461100170314     c                   if        wbrtcode<>*blanks
461200170314     c                   exsr      sr_trul28
461300170314     c                   endif
461400170315     c
461500170315     c                   endif
461600000000     c*
461700000000     c* se errore, esce dal programma
461800140610if  1c                   IF        werr = '1'                                   *SI errore
461900140610     c                   eval      esito = werr                                 *errore
462000140610e   1c                   ENDIF
462100000000     c*
462200000000     c                   ENDSR
462300170314      /FREE
462400170314       //--------------------------------------------------------------
462500170314       //?Controllo correttezza BRTCODE
462600170314       //--------------------------------------------------------------
462700170314       BEGSR  sr_trul28 ;
462800170314          clear trul28ds0;
462900170314          i280cod=wbrtcode;
463000170314          trul28r0(trul28ds0);
463100170314          if o280err='1';
463200170314             werr='1' ;
463300170314             wIdMsg   = 'TIS0737';
463400170314             widcampo = 'BRTCODEI74' ;
463500170314             wparm = wbrtcode ;
463600170314             exsr Messaggi;
463700170314          endif;
463800170314       ENDSR;
463900170314      /end-FREE
464000000000     c*--------------------------------------------------------------------------------------------*
464100000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
464200000000     c* Input:   Walfa7 = Alfanumerico di 7
464300000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
464400000000     c*--------------------------------------------------------------------------------------------*
464500000000     c     xallinea1     BEGSR
464600000000     c*
464700000000     c* reimposta variabli di lavoro
464800000000     c                   CLEAR                   i
464900000000     c                   CLEAR                   z
465000000000     c                   CLEAR                   walfa7bis
465100000000     c                   EVAL      wzeri = *ALL'0'
465200000000     c*
465300000000     c     ' '           CHECKR    walfa7:7      i
465400000000if  1c                   IF        i <> 0
465500000000     c     i             SUBST     walfa7        walfa7bis
465600000000e   1c                   ENDIF
465700000000if  1c                   IF        walfa7bis <> *blanks
465800000000     c     7             SUB       i             z
465900000000if  2c                   IF        z <> 0
466000000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
466100000000e   2c                   ENDIF
466200000000     c                   EVAL      z = z+ 1
466300000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
466400000000e   1c                   ENDIF
466500000000     c*
466600000000     c                   ENDSR
466700000000     c*--------------------------------------------------------------------------------------------*
466800000000     c* caricamento tabelle occorrenti
466900000000     c*--------------------------------------------------------------------------------------------*
467000000000     c     cartab        BEGSR
467100000000     c*---
467200000000     c* organigrama
467300000000     c*---
467400000000     c                   CLEAR                   i                              *indice
467500000000     C                   CLEAR                   sorg
467600000000     C                   CLEAR                   sodes
467700000000     C                   CLEAR                   sofl1
467800000000     C                   CLEAR                   sodpd
467900060623     C                   OPEN      tntbe01l
468000000000     c     *loval        SETLL     azorg01l
468100000000     c                   READ      azorg01l                               99
468200000000do  1c                   DOW       NOT *in99
468300000000     c                   ADD       1             i
468400000000     c                   Z-ADD     orgfil        sorg(i)
468500000000     c                   MOVEL     orgdes        sodes(i)
468600000000     c                   MOVEL     orgde3        og143
468700020702     C*
468800020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
468900020702     C                   CLEAR                   dntw
469000020702     C*
469100020702     C                   MOVEL(P)  'NTW'         kbecod
469200020702     C                   MOVEL(P)  §ogntw        kbeke1
469300020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
469400020702     c                   MOVEL     *blanks       kbelin                         *lingua
469500020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
469600020702     C     keytbe        CHAIN     tntbe01l
469700020702     C                   IF        %found(tntbe01l)
469800020702     C                   MOVEL     tbeuni        dntw
469900020702     C                   ENDIF
470000020702     C*
470100020702     C                   MOVEL     §ntwfie       sofl1(i)
470200160406     C*
470300020702     c                   MOVEL     §ogntw        sodpd(i)
470400160406     C*
470500000000     c                   READ      azorg01l                               99
470600000000e   1c                   ENDDO
470700060623     C                   CLOSE     tntbe01l
470800000000     c*---
470900000000     c                   ENDSR
471000000000     c*--------------------------------------------------------------------------------------------*
471100000000     c* operazioni iniziali -da fare sempre-
471200000000     c*--------------------------------------------------------------------------------------------*
471300000000     c     rutinz        BEGSR
471400000000     c*
471500000000     c* reperimento data corrente
471600000000     C                   TIME                    n14                            *ora (6)+ data (8)
471700000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
471800000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
471900000000     C                   Z-ADD     n8            g08dat
472000000000     C                   Z-ADD     *zeros        g08inv
472100000000     C                   MOVEL     '0'           g08err
472200000000     c                   CALL      'XSRDA8'
472300000000     c                   PARM                    wlbda8
472400000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
472500000000     C                   MOVEL     datcor        anncor                         *anno corrente
472600000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
472700000000     c*
472800000000     c* calcola data corrente - 1 anno
472900000000     c                   Z-ADD     datcor        datpre                         *data precedente
473000000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
473100000000     c*
473200010716     c* azzera la DS di Output e le variabili di lavoro / controllo
473300000000     c                   MOVEL     '0'           esito
473400140603     c                   reset                   tis778dso                      *ds Output
473500140603     c                   reset                   tis778dsm                      *ds Output
473600000000     c                   CLEAR                   smi                            *di memorizzazione
473700000000     c                   CLEAR                   smseql
473800000000     c                   CLEAR                   smseqe
473900000000     c                   CLEAR                   smdevhev
474000000000     c                   CLEAR                   smdeve
474100000000     c                   CLEAR                   smheve
474200000000     c                   CLEAR                   smfle
474300000000     c                   CLEAR                   smdev
474400000000     c                   CLEAR                   smcev
474500130312     c                   CLEAR                   smspe
474600000000     c                   CLEAR                   smdmc
474700000000     c                   CLEAR                   smdmcR
474800000000     c                   CLEAR                   tote                           *contatore eventi
474900000000     c                   CLEAR                   seql                           *sequenza legami
475000010716     c                   RESET                   $AbTD                          *di lavoro
475100010716     c                   RESET                   $AbFirma
475200150312     c                   clear                   pdaavv
475300150421     c                   clear                   pdagia
475400140603     c* parametri ingresso
475500140603     c                   exsr      Kontrolla
475600000000     c* controlla da DS in Input
475700000000     c                   EXSR      chkdsinput
475800000000     c*
475900000000     c* carica i clienti dettaglio del cliente unificante
476000000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
476100000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
476200000000     c     keyvss        SETLL     tivss02l
476300000000     c     keyvss        READE     tivss02l                               99
476400000000do  1c                   DOW       NOT *in99
476500000000if  2c                   IF        vssvat = *blanks                             *attivo
476600000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
476700000000     c                             datcor <= vssdsc
476800010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
476900010719     c* --> TD per vedere la firma DPD
477000010716     c                   RESET                   $AbTD                          *di lavoro
477100010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
477200010719     c     kssksu        SETLL     tivss02l
477300010719     c     kssksu        READE     tivss02l                               99
477400010719do  4c                   DOW       NOT *in99
477500010716if  5c                   IF        vssvat = *blanks                             *attivo
477600010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
477700010716     c                             datcor <= vssdsc
477800010719if  7c                   IF        vssisv = 'TD'
477900010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
478000010719e   7c                   ENDIF
478100010719e   6c                   ENDIF
478200010716e   5c                   ENDIF
478300010719     c     kssksu        READE     tivss02l                               99
478400010719e   4c                   ENDDO
478500010716     c*
478600000000     c                   LEAVE                                                  *uscita ciclo
478700000000e   3c                   ENDIF
478800000000e   2c                   ENDIF
478900000000     c     keyvss        READE     tivss02l                               99
479000010716e   1c                   ENDDO                                                  *fine cl.abil.TT
479100000000     c*
479200060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
479300060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
479400151216     c* Se lo strategi user number è = *blanks --> lo imposto a *zeros
479500151216     c                   monitor
479600151216     c                   if        RQSCIDI174>0
479700151216     c                   endif
479800151216     c
479900151216     c                   on-error
480000151216     c                   clear                   RQSCIDI174
480100151216     c                   endmon
480200060621     c*
480300000000     c                   ENDSR
480400040330
480500040330     ***********************************************************************************************
480600040330     **?
480700040330     **?Reperimento ulteriori dati destinatario.
480800040330     **?
480900040330     ***********************************************************************************************
481000040330     C     ImpDSDest     BEGSR
481100040330
481200140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
481300140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
481400100121     C                             ar5Trd <> 'GEN'
481500140716     c                   Z-ADD     f_tasaas      kr5aas
481600140716     c                   Z-ADD     f_taslnp      kr5lnp
481700140716     c                   Z-ADD     f_tasnrs      kr5nrs
481800140716     c                   Z-ADD     f_tasnsp      kr5nsp
481900040330     c                   MOVEL     'GEN'         kr5trd
482000040531     c     keyar5        CHAIN     fiar531c
482100040330     C                   IF        %FOUND
482200040330     C                   EVAL      DAR5GEN = AR5Uni
482300100121     C                   ELSE
482400100121     C                   RESET                   DAR5GEN
482500040330     C                   ENDIF
482600100121     C                   ENDIF
482700100121     C
482800100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
482900100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
483000040330
483100040330     C                   ENDSR
483200040913
483300040913     ***********************************************************************************************
483400040913     **?
483500040913     **?Determino se addebitare l'immagine LDV.
483600040913     **?
483700040913     ***********************************************************************************************
483800040913     C     AddebitoLDV   BEGSR
483900040913
484000040913     C                   IF        LDVO74 = 'S'
484100040913
484200040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
484300040913
484400040913     C                   ENDIF
484500040913
484600040913     C                   ENDSR
484700051110
484800051110     ***********************************************************************************************
484900051110     **?
485000051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
485100051110     **?
485200051110     ***********************************************************************************************
485300051110     C     chkTivpi      BEGSR
485400051110
485500051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
485600051110     C                             esito10)
485700051110     C                   IF        esito10 > 0 AND NOT %ERROR
485800051110     C                   EVAL      gcfdio74 = 'N'
485900051110     C                   ENDIF
486000051110
486100051110     C                   ENDSR
486200051222
486300051222     ***********************************************************************************************
486400051222     **
486500051222     ** Linea arrivo Italia o estera.
486600051222     **
486700051222     ***********************************************************************************************
486800051222     C     setLnaItaEst  BEGSR
486900051222
487000051222     C                   RESET                   wrkLnaItaEst
487100051222     C                   EXSR      decOrg
487200051222     C                   EVAL      wrkLnaItaEst = wofl1
487300051222
487400051222     C                   ENDSR
487500060622
487600060622     ***********************************************************************************************
487700150615     ** Reperisco informazioni stato bolla
487800060622     ***********************************************************************************************
487900130312     C     srarb         BEGSR
488000130312     c                   clear                   wcev
488100140714     c                   clear                   wricons
488200130312     c                   clear                   worg
488300130312     c                   clear                   wdat
488400130312     c                   clear                   wora
488500150615     c                   clear                   ds2a
488600130312     c                   eval      dsevbspe = spe(1)
488700130312     c     karb          chain     fnarb01l
488800130312     c                   if        not %error and
488900130312     c                             %found(fnarb01l)
489000130422     c                   Z-ADD     arblna        worg
489100130312     c                   exsr      setLnaItaEst
489200130312     c* no per estero
489300130312     C                   IF        wrkLnaItaEst = ESTERO
489400130312     c                   leavesr
489500130312     c                   end
489600150617     c
489700130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
489800130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
489900130312     C                   SELECT
490000141120     c                   WHEN      arbdcm <> 0
490100130312     c                   MOVEL     '704'         wcev                           *causale evento
490200130313     c                   move      arbdcm        WDAT
490300130313     c                   move      arbhmc        WORA
490400150617     c
490500130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
490600150615     c                   WHEN      ARBFDC = ' ' and arbndc>0
490700130326     c* solo se distinta NON in test
490800130326     c                   clear                   ddstflr
490900130326     c                   z-add     arbifp        dstfgs
491000130326     c                   z-add     arbndc        dstnfv
491100130326     c     kdst          chain     fidst01l
491200130326     c                   if        not %error and
491300130326     c                             %found(fidst01l)
491400130326     c                   eval      ddstflr = dstflr
491500130326     c                   end
491600130327     c                   if        §DSTTSTPDA = 'C' or
491700130327     c                             §DSTTSTPDA = 'E'
491800130326     c                   leavesr
491900130326     c                   end
492000130326     c*
492100150617     c* Cerco PRIMO evento da PdA
492200130313     c                   move      arbndc        pctndc
492300131105     c                   exsr      cercaceP
492400150617     c*
492500150617     c* Cerco ULTIMO evento da PdA
492600131105     c                   move      'CET'         ttrd
492700130312     c     kcet          setgt     fipct02l
492800130312     c     kcet          readpe    fipct02l
492900130312     c                   if        not %error and
493000130312     c                             not %eof(fipct02l)
493100130312     c                   movel     pctdati       fipctcetds
493200140926      * firmatario se c'è riempie il campo
493300140926     c                   if        §pctnomfir <> *blank
493400140926     c                   eval      firmato74  = §pctnomfir
493500140926     c                   endif
493600130312     c* consegnata
493700130312     c                   if        §pctcmc = ' '
493800130312     c                   MOVEL     '704'         wcev                           *causale evento
493900130312     c                   else
494000140711     c                   move      'S'           wricons
494100130312     c* mancata consegna
494200130313     c                   movel     §PCTcmc       wcev
494300130312     c                   clear                   ds2a
494400130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
494500130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
494600130312     C                             :rpyEsito)
494700130312     c                   if        §2aflg = '§' and §2acpt = 'S'
494800130312     c                   move      'T  '         wcev
494900130312     c                   endif
495000130312     c                   endif
495100130312     c* ??? £6
495200130312     c                   if        §pctdata > 0
495300130313     c                   move      §PCTDATA      WDAT
495400130312     c                   movel     §PCTORA       WORA
495500130312     c                   endif
495600130312     c                   end
495700130312     c                   endSL
495800130312     c                   end
495900130312     c* ho trovato un evento da inserire  al 1° posto
496000130312     c                   if        wcev <> ' '
496100130312     c                   EXSR      deccevsta
496200130312     c                   IF        werr <> '1'                                  *no errore
496300130312     c                   EXSR      decorg
496400130312     c                   EXSR      edtdat
496500130312     c                   EXSR      edtora
496600130313     c* libero il 1° elemnto di schiera
496700150617     c                   eval      j = 49
496800150617     c                   for       j = 49 downto 1
496900130312     c                   if        cev(j) <> ' '
497000130312     c                   eval      cev(j+1) = cev(j)
497100130312     c                   eval      dev(j+1) = dev(j)
497200130312     c                   eval      hev(j+1) = hev(j)
497300130312     c                   eval      fle(j+1) = fle(j)
497400130312     c                   eval      eve(j+1) = eve(j)
497500130312     c                   end
497600130313     c                   endfor
497700130313     c                   eval      dev(1) = wdate
497800130313     c                   eval      hev(1) = worae
497900130312     c                   eval      cev(1)= wcev
498000130313     c                   eval      fle(1) = wodes
498100130313     c                   eval      eve(1) = wcdex
498200130313     c                   EVAL      cntEveO74 += 1
498300150310     c*
498400150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
498500150310     c                   if        §2aftc='A'
498600150310     c                   eval      pdaavv='S'
498700150310     c                   endif
498800150421     c                   if        §2aftc='G'
498900150421     c                   eval      pdagia='S'
499000150421     c                   endif
499100150310     c
499200130312     c                   end
499300130312     c                   end
499400130312     C                   ENDSR
499500140714     **_________________________________________________________
499600140714     c     cercafiglia   begsr
499700140714     **_________________________________________________________
499800140715     c                   clear                   keyForzata
499900140714      * salva chiave originale per eventi
500000140715     c                   eval      kasaasf = kasaas
500100140715     c                   eval      kaslnpf = kaslnp
500200140715     c                   eval      kasnrsf = kasnrs
500300140715     c                   eval      kasnspf = kasnsp
500400140714     c                   do        *hival
500500140715     c     keytasf       chain     fnlbl02l
500600140714     c                   if        %found(fnlbl02l)
500700140715     c                   eval      kasaasf = LBLaan
500800140715     c                   eval      kaslnpf = LBLlpn
500900140715     c                   eval      kasnrsf = LBLnrn
501000140715     c                   eval      kasnspf = LBLnsn
501100140714     c                   move      'X'           keyForzata
501200140714     c                   iter
501300140714     c                   else
501400140714     c                   leave
501500140714     c                   endif
501600140714     c                   enddo
501700140715     c     keytasf       setll     titas30c                               99    *no errore
501800140715     c     keytasf       READE     titas30c                               99    *no errore
501900141117      *se non trova titas con figlia forza di nuovo la mamma
502000140715if  3c                   IF        *in99                                        *non trovato
502100141117     c                   clear                   titasdssavf
502200140715x   3c                   ELSE                                                   *trovata
502300140715if  4c                   IF        *in01                                        *lettura titas000
502400140715     c                   EVAL      titasdssavf = titasds
502500140715e   4c                   ENDIF
502600140715if  4c                   IF        *in02                                        *lettura titas010
502700140715     c                   EVAL      titasdssavf = titasds
502800140715e   4c                   ENDIF
502900140715if  4c                   IF        *in03                                        *lettura titasp00
503000140715     c                   EVAL      titasdssavf = titasds
503100140715e   4c                   ENDIF
503200140715e   3c                   ENDIF
503300140714     C                   ENDSR
503400160318     **_________________________________________________________
503500160318     c     cercamamma    begsr
503600160318     **_________________________________________________________
503700160318     c                   clear                   keyForzata
503800160318     c                   EVAL      kblaan = arbaas
503900160318     c                   EVAL      kbllpn = arblnp
504000160318     c                   EVAL      kblnrn = arbnrs
504100160318     c                   EVAL      kblnsn = arbnsp
504200160318     c     keylbl        CHAIN     fnlbl01l                           99
504300160318if  1c                   IF        NOT *in99                                    *ha la mamma
504400160318     c                   EVAL      kasaas = lblaap
504500160318     c                   EVAL      kaslnp = lbllpp
504600160318     c                   EVAL      kasnrs = lblnrp
504700160318     c                   EVAL      kasnsp = lblnsp
504800160318     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
504900160318if  3c                   IF        not *in99                                        *non trovato
505000160318     c                   move      'X'           keyForzata
505100160318     c                   eval      esito=' '
505200160408     c                   clear                   titasdssavf
505300160318     c                   endif
505400160318     c                   endif
505500160318     c
505600160318     c                   ENDSR
505700131105     **_________________________________________________________
505800131105     c     cercaCEP      begsr
505900131105     **_________________________________________________________
506000131105     c                   move      'CEP'         ttrd
506100131105     c     kcet          setgt     fipct02l
506200131105     c     kcet          readpe    fipct02l
506300131105     c                   if        not %error and
506400131105     c                             not %eof(fipct02l)
506500131105     c                   movel     pctdati       fipctcetds
506600131105     c* consegnata
506700131105     c                   if        §pctcmc = ' '
506800131105     c                   MOVEL     '704'         wcev                           *causale evento
506900131105     c                   else
507000131105     c* mancata consegna
507100131105     c                   movel     §PCTcmc       wcev
507200131105     c                   clear                   ds2a
507300131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
507400131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
507500131105     C                             :rpyEsito)
507600131105     c                   if        §2aflg = '§' and §2acpt = 'S'
507700131105     c                   move      'T  '         wcev
507800131105     c                   endif
507900131105     c                   endif
508000131105     c* ??? £6
508100131105     c                   if        §pctdata > 0
508200131105     c                   move      §PCTDATA      WDAT
508300131105     c                   movel     §PCTORA       WORA
508400131105     c                   endif
508500131105     c                   end
508600131105     c* ho trovato un evento da inserire  al 1° posto
508700131105     c                   if        wcev <> ' '
508800131105     c                   EXSR      deccevsta
508900131105     c                   IF        werr <> '1'                                  *no errore
509000131105     c                   EXSR      decorg
509100131105     c                   EXSR      edtdat
509200131105     c                   EXSR      edtora
509300131105     c* libero il 1° elemnto di schiera
509400150617     c                   eval      j = 49
509500150617     c                   for       j = 49 downto 1
509600131105     c                   if        cev(j) <> ' '
509700131105     c                   eval      cev(j+1) = cev(j)
509800131105     c                   eval      dev(j+1) = dev(j)
509900131105     c                   eval      hev(j+1) = hev(j)
510000131105     c                   eval      fle(j+1) = fle(j)
510100131105     c                   eval      eve(j+1) = eve(j)
510200131105     c                   end
510300131105     c                   endfor
510400131105     c                   eval      dev(1) = wdate
510500131105     c                   eval      hev(1) = worae
510600131105     c                   eval      cev(1)= wcev
510700131105     c                   eval      fle(1) = wodes
510800131105     c                   eval      eve(1) = wcdex
510900131105     c                   EVAL      cntEveO74 += 1
511000131105     c                   end
511100131105     c                   end
511200131105     c                   endsr
511300131105     ***********************************************************************************************
511400130312     **
511500130312     ** Reperisco tabella da TNTBE00F.
511600130312     **
511700130312     ***********************************************************************************************
511800130312     C     getTntbe00f   BEGSR
511900060622     C                   EVAL      t02Lin = langTntbe
512000060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
512100060622     C                   SELECT
512200060622     C                   WHEN      t02Cod = 'ICE'
512300060622     C                   EVAL      dice = t02Uni
512400060622     C                   WHEN      t02Cod = 'CCH'
512500060622     C                   EVAL      dcch = t02Uni
512600060622     C                   WHEN      t02Cod = 'NTW'
512700060622     C                   EVAL      dntw = t02Uni
512800100119     C                   WHEN      t02Cod = 'I1A'
512900100119     C                   EVAL      dI1a = t02Uni
513000150304     C                   WHEN      t02Cod = 'SPC'
513100150304     C                   EVAL      dSPC = t02Uni
513200170301     C                   WHEN      t02Cod = 'CLI'
513300170301     C                   EVAL      dcli = t02Uni
513400060622     C                   ENDSL
513500060622     C                   ENDSR
513600060608
513700140529      /free
513800140529       //--------------------------------------------------------------
513900140529       //?Controllo formale dei dati passati.
514000140529       //--------------------------------------------------------------
514100140603       BEGSR  Kontrolla;
514200140529
514300140529       //?OpCode
514400140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
514500160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
514600140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
514700160818           esito='1'    ;
514800140529           exsr RoutEnd;
514900140529         ENDIF;
515000140603           IF  prmRqsDataSize > 0;
515100140603           ELSE;
515200160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
515300140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
515400160818             esito='1'    ;
515500140603             exsr RoutEnd;
515600140603           ENDIF;
515700140529
515800140529       //?Formato e size RQS
515900140603           IF  prmRqsFormato = formatoi74;
516000140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
516100140529           ELSE;
516200160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
516300140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
516400160818             esito='1'    ;
516500140529             exsr RoutEnd;
516600140529           ENDIF;
516700140529         //?Formato e size RPY
516800140603           IF  prmRpyFormato = formatoo74;
516900140529           ELSE;
517000160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
517100140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
517200160818             esito='1'    ;
517300140529             exsr RoutEnd;
517400140529           ENDIF;
517500140529           IF  prmRpyDataSize > 0;
517600140529           ELSE;
517700160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
517800140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
517900160818             esito='1'    ;
518000140529             exsr RoutEnd;
518100140529           ENDIF;
518200140616       //?Formato e size MSG
518300140617         if %parms > 9 ;
518400140617          IF  prmRpyFormMsg = formato;
518500140617          ELSE;
518600160818           // prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
518700140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
518800160818           esito='1'    ;
518900140616           exsr RoutEnd;
519000140617          ENDIF;
519100140617          IF  prmRpyMsgSize > 0;
519200140617          ELSE;
519300160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
519400140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
519500160818           esito='1'    ;
519600140616           exsr RoutEnd;
519700140617          ENDIF;
519800140617         ENDIF;
519900140529
520000140529       ENDSR;
520100140529
520200140616       //--------------------------------------------------------------
520300140617       //?Routine per schierare i messaggi nella ds tis778dsm
520400140616       //--------------------------------------------------------------
520500140616       BEGSR  Messaggi;
520600140616
520700140616       //?Se ho già caricato 99 messaggi esco
520800140616         IF  nrmsg = 99;
520900160818           esito='1'  ;
521000140616           exsr RoutEnd;
521100140616           clear nrmsg;
521200140616         ENDIF;
521300140616
521400140616         nrmsg += 1;
521500140616         skIdMsg(nrmsg) = wIdMsg;
521600140616         skIdCampo(nrmsg) = wIdCampo;
521700140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
521800140616         IF  wParm <> *blanks;
521900140616           skTextMsg (nrmsg) =
522000140616           rtvMsgLang(wIdMsg:langi74:wParm);
522100140616         ELSE;
522200140616           skTextMsg (nrmsg)=
522300140616           rtvMsgLang(wIdMsg:langi74);
522400140616         ENDIF;
522500140616
522600140616       ENDSR;
522700140616
522800140529       //--------------------------------------------------------------
522900140529       BEGSR  RoutEnd;
523000140530
523100140530
523200160322        if  esito <> '0' and esito<>'P';
523300140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
523400140530             else;
523500140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
523600140530        endif ;
523700140529
523800140530       //?ritorno
523900140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
524000140617         if %parms > 9 ;
524100140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
524200140617         endif;
524300140529         return;
524400140529
524500140529       ENDSR;
524600140529      /end-free
524700000000     c*--------------------------------------------------------------------------------------------*
524800000000     c* operazioni iniziali
524900000000     c*--------------------------------------------------------------------------------------------*
525000000000     c     *inzsr        BEGSR
525100000000     c*
525200000000     c* ricevimento parametri
525300140603     c*m   *ENTRY        PLIST
525400140529     c*m                 PARM                    esito
525500140529     c*m                 PARM                    tis174dsi
525600140529     c*m                 PARM                    tis174dso
525700000000     c*
525800000000     c* chiavi di lettura
525900000000     c     keytas        KLIST                                                  titas30c
526000000000     c                   KFLD                    kasaas                         -anno spedizione
526100000000     c                   KFLD                    kaslnp                         -linea partenza
526200000000     c                   KFLD                    kasnrs                         -serie
526300000000     c                   KFLD                    kasnsp                         -spedizione
526400140715     c     keytasf       KLIST                                                  titas30c
526500140715     c                   KFLD                    kasaasf                        -anno spedizione
526600140715     c                   KFLD                    kaslnpf                        -linea partenza
526700140715     c                   KFLD                    kasnrsf                        -serie
526800140715     c                   KFLD                    kasnspf                        -spedizione
526900130312     c     karb          KLIST                                                  titas30c
527000140716     c                   KFLD                    f_tasaas                         -anno spedizione
527100140716     c                   KFLD                    f_taslnp                         -linea partenza
527200140716     c                   KFLD                    f_tasnrs                         -serie
527300140716     c                   KFLD                    f_tasnsp                         -spedizione
527400130326     C     Kdst          KLIST
527500130326     C                   KFLD                    dstnpg
527600130326     C                   KFLD                    dstnfv
527700130326     C                   KFLD                    dstfgs
527800130326     c                   z-add     4             dstnpg
527900130326     C     Kcet          KLIST
528000130326     C                   KFLD                    arbLNA
528100130326     C                   KFLD                    pctNDC
528200130326     C                   KFLD                    arbpdc
528300131105     C                   KFLD                    TTRD              3
528400130312     C                   KFLD                    arbAAS
528500130312     C                   KFLD                    arbLNP
528600130312     C                   KFLD                    arbNRS
528700130312     C                   KFLD                    arbNSP
528800150403     C     KARP          KLIST
528900150403     C                   KFLD                    arbAAS
529000150403     C                   KFLD                    arbLNP
529100150403     C                   KFLD                    arbNRS
529200150403     C                   KFLD                    arbNSP
529300000000     c     keytaa        KLIST                                                  titaa30c
529400000000     c                   KFLD                    kaaaas                         -anno spedizione
529500000000     c                   KFLD                    kaalnp                         -linea partenza
529600000000     c                   KFLD                    kaanrs                         -serie
529700000000     c                   KFLD                    kaansp                         -spedizione
529800000000     c                   KFLD                    kaatrc                         -tipo anagrafica
529900000000     c     keyta4        KLIST                                                  tita430c
530000000000     c                   KFLD                    ka4aas                         -anno spedizione
530100000000     c                   KFLD                    ka4lnp                         -linea partenza
530200000000     c                   KFLD                    ka4nrs                         -serie
530300000000     c                   KFLD                    ka4nsp                         -spedizione
530400000000     c                   KFLD                    ka4trc                         -tipo riferimento
530500010925     c     ke2ta4        KLIST                                                  tita430c
530600010925     c                   KFLD                    ka4aas                         -anno spedizione
530700010925     c                   KFLD                    ka4lnp                         -linea partenza
530800010925     c                   KFLD                    ka4nrs                         -serie
530900010925     c                   KFLD                    ka4nsp                         -spedizione
531000000000     c     keyevb        KLIST                                                  fnevb01l
531100000000     c                   KFLD                    kvbaas                         -anno spedizione
531200000000     c                   KFLD                    kvblnp                         -linea partenza
531300000000     c                   KFLD                    kvbnrs                         -serie
531400000000     c                   KFLD                    kvbnsp                         -spedizione
531500010717     c     ke2evb        KLIST                                                  fnevb01l
531600010717     c                   KFLD                    kvbaas                         -anno spedizione
531700010717     c                   KFLD                    kvblnp                         -linea partenza
531800010717     c                   KFLD                    kvbnrs                         -serie
531900010717     c                   KFLD                    kvbnsp                         -spedizione
532000010717     c                   KFLD                    kvbdev                         -data evento
532100010717     c                   KFLD                    kvbhev                         -ora  evento
532200000000     c     keylbl        KLIST                                                  fnlbl01l
532300000000     c                   KFLD                    kblaan                         -anno seguente
532400000000     c                   KFLD                    kbllpn                         -linea seguente
532500000000     c                   KFLD                    kblnrn                         -serie seguente
532600000000     c                   KFLD                    kblnsn                         -spedizione seguente
532700000000     c     ke2lbl        KLIST                                                  fnlbl02l
532800000000     c                   KFLD                    kblaap                         -anno precedente
532900000000     c                   KFLD                    kbllpp                         -linea precedente
533000000000     c                   KFLD                    kblnrp                         -serie precedente
533100000000     c                   KFLD                    kblnsp                         -spediz.precedente
533200000000     c     keytbe        KLIST                                                  *tntbe01l
533300000000     c                   KFLD                    kbecod                          -tabella
533400000000     c                   KFLD                    kbeke1                          -chiave uno
533500000000     c                   KFLD                    kbeke2                          -chiave due
533600000000     c                   KFLD                    kbelin                          -lingua
533700000000     c                   KFLD                    kbesif                          -s.informativo
533800000000     c     keyvss        KLIST                                                  *tivss02l
533900000000     c                   KFLD                    kssksu                          -cl.unificante
534000000000     c                   KFLD                    kssisv                          -tipo servizio
534100010717     c     keyvtd        KLIST                                                  *tivtd01l
534200010717     c                   KFLD                    ktdksu                          -cl.unificante
534300000000     c     keycsb        KLIST                                                  *tncsb03l
534400000000     c                   KFLD                    ksbaas                         -anno spedizione
534500000000     c                   KFLD                    ksblnp                         -linea partenza
534600000000     c                   KFLD                    ksbnrs                         -serie
534700000000     c                   KFLD                    ksbnsp                         -spedizione
534800040906     c     keydct        KLIST
534900040906     c                   KFLD                    DCTAAC
535000040906     c                   KFLD                    DCTFIL
535100040906     c                   KFLD                    DCTNCA
535200000000     c     keygcp        KLIST                                                  *figcp01l
535300000000     c                   KFLD                    kcpaas                         -anno spedizione
535400000000     c                   KFLD                    kcplnp                         -linea partenza
535500000000     c                   KFLD                    kcpnrs                         -serie
535600000000     c                   KFLD                    kcpnsp                         -spedizione
535700000000     c                   KFLD                    kcpfrg                         -prg aperture
535800050404     c     K04GCP51      KLIST
535900050404     c                   KFLD                    kcpaas                         -anno spedizione
536000050404     c                   KFLD                    kcplnp                         -linea partenza
536100050404     c                   KFLD                    kcpnrs                         -serie
536200050404     c                   KFLD                    kcpnsp                         -spedizione
536300000000     c     keygnp        KLIST                                                  *fignp01l
536400000000     c                   KFLD                    knpagc                         -anno apertura
536500000000     c                   KFLD                    knpfgc                         -filiale apertura
536600000000     c                   KFLD                    knpngc                         -numero giacenza
536700000000     c                   KFLD                    knpfrg                         -prg apertura
536800040531     c     keyar5        KLIST                                                  *fiar531c
536900030206     c                   KFLD                    kr5aas                         -anno spedizione
537000030206     c                   KFLD                    kr5lnp                         -linea partenza
537100030206     c                   KFLD                    kr5nrs                         -serie
537200030206     c                   KFLD                    kr5nsp                         -spedizione
537300030206     c                   KFLD                    kr5trd                         -tipo record
537400170301     c     ktfntc        KLIST                                                  titas30c
537500170301     c                   KFLD                    k_NTCAPL
537600170301     c                   KFLD                    k_NTCnk1
537700170301     c                   KFLD                    k_NTCnk2
537800170301     c                   KFLD                    k_NTCtnt
537900040907
538000000000     c* apre i files
538100000000     c                   OPEN      titas30c
538200000000     c                   OPEN      titaa30c
538300000000     c                   OPEN      tita430c
538400000000     c                   OPEN      fnevb01l
538500000000     c                   OPEN      tivss02l
538600010716     c                   OPEN      tivtd01l
538700000000     c                   OPEN      azorg01l
538800170301     c                   OPEN      tfntc01l
538900000000     c                   OPEN      fnlbl01l
539000000000     c                   OPEN      fnlbl02l
539100050309     c                   OPEN      tigcp51l
539200050309     c                   OPEN      tignp01l
539300000000     c                   OPEN      tncsb03l
539400040906     c                   OPEN      fndct01l
539500040531     c                   OPEN      fiar531c
539600130312     c                   OPEN      fnarb01l
539700160127     c                   OPEN      fnblp01l
539800160309     c                   OPEN      fiar901l
539900150403     c                   OPEN      fiarp01l
540000140606     c                   OPEN      fiar501l
540100140606     c                   OPEN      fiar401l
540200130326     c                   OPEN      fidst01l
540300130312     c                   OPEN      fipct02l
540400060621     C                   CALLP     openTabel00f()
540500000000     c*
540600000000     c* caricamento tabelle occorrenti
540700000000     c                   EXSR      cartab
540800060621     C                   CALLP     inzLingue()
540900000000     c*
541000000000     c                   ENDSR
541100070109
541200070109     ***********************************************************************************************
541300070109     **
541400070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
541500070109     **
541600070109     ***********************************************************************************************
541700070109     C     eventoAnnulla BEGSR
541800070109
541900070109     C                   IF        §iceEvAn <> *BLANK
542000070109     C                   FOR       j = 1 TO %ELEM(smCev)
542100070109     C                   IF        §iceEvAn = smCev(j) AND
542200070109     C                             evbDevHev = smDevHev(j)
542300070109     C                   CLEAR                   smCev(j)
542400130312     C                   CLEAR                   smspe(j)
542500070109     C                   CLEAR                   smDevHev(j)
542600070109     C                   CLEAR                   smi(j)
542700070109     C                   CLEAR                   smSeql(j)
542800070109     C                   CLEAR                   smSeqe(j)
542900070109     C                   CLEAR                   smDeve(j)
543000070109     C                   CLEAR                   smHeve(j)
543100070109     C                   CLEAR                   smFle(j)
543200070109     C                   CLEAR                   smDev(j)
543300070109     C                   LEAVE
543400070109     C                   ENDIF
543500070109     C                   ENDFOR
543600070109     C                   ENDIF
543700070109
543800070109     C                   ENDSR
543900080131
544000100119     P*--------------------------------------------------
544100100119     P* Procedure name: GetDescrizioneIncassoContrassegno
544200100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
544300100119     P*                          o del contrassegno.
544400100121     P* Returns:        Descrizione modalità incasso contrassegno
544500100119     P*--------------------------------------------------
544600100119     P GetDescrizioneIncassoContrassegno...
544700100119     P                 B
544800100119     D GetDescrizioneIncassoContrassegno...
544900100119     D                 PI            35A
545000100119
545100100121     C                   RESET                   tibs02ds
545200100121     C                   EVAL      t02Cod = 'I1A'
545300100121     C                   EVAL      t02Lin = langTntbe
545400100119     ** Il contrassegno è stato incassato
545500100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
545600100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
545700100121     C                   ELSE
545800100119     ** Il contrassegno non è stato incassato.
545900100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
546000100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
546100100119     C                             ar5Trd <> 'GEN'
546200100121     C                   EVAL      kr5Aas = csbAas
546300100121     C                   EVAL      kr5Lnp = csbLnp
546400100121     C                   EVAL      kr5Nrs = csbNrs
546500100121     C                   EVAL      kr5Nsp = csbNsp
546600100119     C                   EVAL      kr5Trd = 'GEN'
546700160314     c                   if        esito='P'
546800160314     C     keyAr5        CHAIN     fiar501l
546900160314     c                   else
547000160314     C     keyAr5        CHAIN     fiar531c
547100160314     c                   endif
547200160314     c
547300100119     C                   IF        %FOUND()
547400100119     C                   EVAL      dAr5Gen = ar5Uni
547500100119     C                   ELSE
547600100119     C                   RESET                   dAr5Gen
547700100119     C                   RETURN                  *BLANK
547800100119     C                   ENDIF
547900100119     C                   ENDIF
548000100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
548100100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
548200100119     C                   ELSE
548300100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
548400100119     C                   ENDIF
548500100121     C                   ENDIF
548600100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
548700100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
548800100119     C                   IF        t02Err = 'E'
548900100119     C                   RETURN                  *BLANK
549000100119     C                   ENDIF
549100100120     C                   EVAL      dI1a = t02Uni
549200140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
549300140417     C                   RETURN                  dI1a.descrizi2
549400140417     C                   ENDIF
549500100119     C                   RETURN                  dI1a.descrizion
549600100119
549700100119     P GetDescrizioneIncassoContrassegno...
549800100119     P                 E
549900100119
550000110304
550100110304     P*--------------------------------------------------
550200110304     P* Procedure name: IsContrassegnoCompensato
550300110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
550400110304     P*                          to con una fattura.
550500110304     P* Returns:        *ON = Compensato
550600110304     P*--------------------------------------------------
550700110304     P IsContrassegnoCompensato...
550800110304     P                 B
550900110304     D IsContrassegnoCompensato...
551000110304     D                 PI              N
551100110304
551200110304     D retField        S               N   INZ(*OFF)
551300110304
551400110307     ** Il contrassegno non è stato rimborsato.
551500110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
551600110307     C                   RETURN    retField
551700110307     C                   ENDIF
551800110307
551900110307     C/EXEC SQL
552000110307     C+ SELECT '1'
552100110307     C+ INTO :retField
552200110307     C+ FROM tncsv00f
552300110307     C+ WHERE csvAas = :csbAas
552400110307     C+   AND csvLnp = :csbLnp
552500110307     C+   AND csvNrs = :csbNrs
552600110307     C+   AND csvNsp = :csbNsp
552700110307     C+   AND csvCav = 'COMP'
552800110307     C+ FETCH FIRST ROW ONLY
552900110307     C/END-EXEC
553000110304
553100110307     C                   SELECT
553200110307     C                   WHEN      sqlCode < 0
553300110307     C                   DUMP(A)
553400110307     C                   ENDSL
553500110304
553600110307     C                   RETURN    retField
553700110304
553800110304     P IsContrassegnoCompensato...
553900110304     P                 E
554000110304
554100120321
554200120321     P*--------------------------------------------------
554300120321     P* Procedure name: SetIdAssegnoMittente
554400120321     P* Purpose:        Imposta l'ID assegno mittente.
554500120321     P* Returns:        Esito.
554600120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
554700120321     P*                          segni mittente.
554800120321     P* Parameter:      priAbi => ABI assegno.
554900120321     P* Parameter:      priCab => CAB assegno.
555000120321     P*--------------------------------------------------
555100120321     P SetIdAssegnoMittente...
555200120321     P                 B
555300120321     D SetIdAssegnoMittente...
555400120321     D                 PI            10I 0
555500120321     D  priNumeroAssegno...
555600120321     D                                     LIKE(caNumO74)
555700120321     D  priAbi                             LIKE(caAbiO74)
555800120321     D  priCab                             LIKE(caCabO74)
555900120321
556000120321     D retField        S             10I 0 STATIC
556100120321
556200120321      /FREE
556300120321
556400120321       CLEAR retField;
556500120321
556600120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
556700120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
556800120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
556900120321       // In questo caso restituisco l'ID del primo assegno.
557000120321
557100120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
557200120321         RETURN retField;
557300120321       ENDIF;
557400120321
557500120321       EXEC SQL
557600120321         SELECT csmNra, csmAbi, csmCab
557700120321           INTO :priNumeroAssegno, :priAbi, :priCab
557800120321           FROM tncsm00f
557900120321           WHERE csmKey = :priNumeroAssegno
558000120321           FETCH FIRST 1 ROW ONLY
558100120321       ;
558200120321
558300120321       SELECT;
558400120321         WHEN sqlCode = 100;
558500120321           retField = sqlCode;
558600120321         WHEN sqlCode < *ZERO;
558700120321           DUMP(A);
558800120321           retField = sqlCode;
558900120321       ENDSL;
559000120321
559100120321       RETURN retField;
559200120321
559300120321      /END-FREE
559400120321     P SetIdAssegnoMittente...
559500120321     P                 E
559600120321
559700130513
559800130513     P*--------------------------------------------------
559900130513     P* Procedure name: GetFilUrl
560000130513     P* Purpose:        Restituisce l'URL della filiale.
560100130513     P* Returns:        URL filiale.
560200130513     P* Parameter:      piFiliale => ID filiale
560300130513     P*--------------------------------------------------
560400130513     P GetFilUrl       B
560500130513     D GetFilUrl       PI           256A
560600130513     D  piFiliale                     3P 0 CONST
560700130513
560800130513     C                   IF        piFiliale = *ZERO
560900130513     C                   RETURN    *BLANK
561000130513     C                   ENDIF
561100130513     C                   RESET                   tibs02ds
561200130513     C                   EVAL      t02Cod = 'UFI'
561300130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
561400130513     C                   EVAL      t02Lin = langTntbe
561500130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
561600130513     C                   IF        t02Err = 'E'
561700130513     C                   RETURN    *BLANK
561800130513     C                   ENDIF
561900130513     C                   RETURN    t02Uni
562000130513
562100130513     P GetFilUrl       E
562200130513
