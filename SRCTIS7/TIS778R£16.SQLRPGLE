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
167300170626     c* Se presente l'aut nell'evento prendo la filiale AUT, altrimenti fle
167400170626     c                   if        %subst(evbnot:8:3)>'000'
167500170626     c                   eval      worg=%int(%subst(evbnot:8:3))
167600170626     c                   else
167700150618     c                   Z-ADD     evbfle        worg
167800150618     c                   endif
167900170626     c                   endif
168000170626
168100000000     c                   EXSR      decorg
168200000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
168300080201     c                   MOVEL     wDev          wdat
168400080201     c                   EXSR      edtdat
168500080201     c                   MOVEL     wdate         smdeve(j)
168600080201     c                   MOVEL     wHev          wora
168700080201     c                   EXSR      edtora
168800080201     c                   MOVEL     worae         smheve(j)
168900130313     c                   MOVEL     dsevbspe      smspe(j)
169000000000     c                   LEAVE                                                  *uscita ciclo
169100000000e   6c                   ENDIF
169200000000e   5c                   ENDDO
169300000000     c*
169400000000     c* incrementa contatore eventi
169500000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
169600000000     c                   ADD       1             tote
169700000000     c* indice schiera
169800000000     c                   Z-ADD     tote          smi(tote)
169900000000     c* sequenza legame
170000000000     c                   Z-ADD     seql          smseql(tote)
170100000000     c* sequenza evento
170200000000     c                   Z-ADD     020           smseqe(tote)
170300000000     c* descrizione
170400030130     c                   MOVEL     wcdex         smdev(tote)
170500000000     c* data
170600080201     c                   MOVEL     wDev          wdat
170700000000     c                   EXSR      edtdat
170800000000     c                   MOVEL     wdate         smdeve(tote)
170900000000     c* ora
171000080201     c                   MOVEL     wHev          wora
171100000000     c                   EXSR      edtora
171200000000     c                   MOVEL     worae         smheve(tote)
171300000000     c* filiale
171400150617     C***                IF        wrkLnaItaEst = ESTERO
171500150617     C                   IF        wtasfl1 = ESTERO
171600140722     c                   Z-ADD     tasLna        worg
171700140722     c                   else
171800170626     c* Se presente l'aut nell'evento prendo la filiale AUT, altrimenti fle
171900170626     c                   if        %subst(evbnot:8:3)>'000'
172000170626     c                   eval      worg=%int(%subst(evbnot:8:3))
172100170626     c                   else
172200140722     c                   Z-ADD     evbfle        worg
172300140722     c                   endif
172400170626     c                   endif
172500140722     c                   EXSR      decorg
172600140722     c                   MOVEL     wodes         smfle(tote)
172700000000     c* causale
172800000000     c                   MOVEL     evbcev        smcev(tote)
172900130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
173000130312     c* bolla legata
173100130312     c                   MOVEL     dsevbspe      smspe(tote)
173200000000     c* data+ora evento
173300080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
173400000000e   5c                   ENDIF
173500000000e   4c                   ENDIF
173600000000e   3c                   ENDIF
173700000000e   2c                   ENDIF
173800010717     c     keyevb        READE     fnevb01l                               99
173900000000e   1c                   ENDDO
174000000000     c*
174100000000     c                   ENDSR
174200000000     c*--------------------------------------------------------------------------------------------*
174300000000     c* memorizza gli "eventi fissi"
174400000000     c*--------------------------------------------------------------------------------------------*
174500000000     c     memevefix     BEGSR
174600080131     ** ID bolla in canna.
174700080131     c                   EVAL      waas = tasaas
174800080131     c                   EVAL      wlnp = taslnp
174900080131     c                   EVAL      wnrs = tasnrs
175000080131     c                   EVAL      wnsp = tasnsp
175100000000     c*
175200000000     c* Partenza merce
175300000000     c                   EVAL      lduc = tasduc                                *data partenza
175400000000     c                   EVAL      llnp = taslnp                                *filiale partenza
175500000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
175600000000     c                   EXSR      evbpar
175700000000     c* Consegna merce
175800000000     c                   EVAL      lcca = tascca                                *consegna anomala
175900000000     c                   EVAL      ldcm = tasdcm                                *data consegna
176000000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
176100000000     c                   EVAL      llna = taslna                                *linea arrivo
176200150618     c                   EVAL      ltfa = tastfa                                *linea arrivo
176300000000     c                   EXSR      evbcon
176400000000     c*
176500000000     c                   ENDSR
176600000000     c*--------------------------------------------------------------------------------------------*
176700000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
176800000000     c*--------------------------------------------------------------------------------------------*
176900000000     c     elaevelbl     BEGSR
177000000000     c*
177100000000     c* azzera le variabili di lavoro
177200000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
177300000000     c                   EVAL      deplpn = *zeros
177400000000     c                   EVAL      depnrn = *zeros
177500000000     c                   EVAL      depnsn = *zeros
177600000000     c*
177700000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
177800000000     c                   EVAL      kbllpp =  s_taslnp
177900000000     c                   EVAL      kblnrp =  s_tasnrs
178000000000     c                   EVAL      kblnsp =  s_tasnsp
178100000000     c*
178200000000     c* ciclo fino a fine mamme
178300000000     c     ke2lbl        SETLL     fnlbl02l
178400000000     c     ke2lbl        READE     fnlbl02l                               99
178500000000do  1c                   DOW       NOT *in99
178600000000     c*
178700000000     c* ciclo fino a fine figlie della mamma
178800000000do  2c                   DOW       NOT *in99
178900000000     c*
179000000000     c* legge la bolla dal legame
179100000000     c                   EVAL      kasaas = lblaan                              *figlia
179200000000     c                   EVAL      kaslnp = lbllpn
179300000000     c                   EVAL      kasnrs = lblnrn
179400000000     c                   EVAL      kasnsp = lblnsn
179500000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
179600000000if  3c                   IF        NOT *in99 AND                                *esistente
179700000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
179800000000     c                             tastbl <> 'A3' AND
179900000000     c                             tastbl <> 'F3' AND
180000000000     c                             tastbl <> 'AP' AND
180100000000     c                             tastbl <> 'FC'
180200041222     **?Immagine lettera di vettura.
180300041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
180400041222     C                   IF        LDVO74 <> 'N'
180500040330     c                   MOVEL     tasflo        dtasflo
180600040330     c                   IF        §floiml <> *BLANK
180700050325     c                   EVAL      ldvo74 = 'S'
180800040330     C                   EVAL      SpImLDVO74 =
180900040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
181000040330     C                             %EDITC(taslnp:'X') +
181100040330     C                             %EDITC(tasnrs:'X') +
181200040330     C                             %EDITC(tasnsp:'X')
181300041223     **?Reperisco il check code della spedizione POD
181400041223     C                   EVAL      PChkCdeO74 =
181500041223     C                             GetSpeChkCde(
181600041223     C                             %EDITC(tasaas:'X'):
181700041223     C                             %EDITC(taslnp:'X') +
181800041223     C                             %EDITC(tasnrs:'X') +
181900041223     C                             %EDITC(tasnsp:'X'):
182000041223     C                             ChkCode:nEsito)
182100040330     C                   ENDIF
182200041222     C                   ENDIF
182300000000     c* incrementa la sequenza legami per le bolle legate
182400000000     c                   ADD       1             seql                           *legaqme successivo
182500000000     c*---
182600000000     c* memorizza gli "eventi fissi"
182700000000     c*---
182800000000     c                   EXSR      memevefix
182900000000     c*---
183000000000     c* memorizza gli eventi
183100000000     c*---
183200000000     c                   EVAL      waas = lblaan                                *figlia
183300000000     c                   EVAL      wlnp = lbllpn
183400000000     c                   EVAL      wnrs = lblnrn
183500000000     c                   EVAL      wnsp = lblnsn
183600000000     c                   EXSR      memevb
183700000000     c*
183800000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
183900000000     c                   EVAL      depaan = lblaan                              *ultima figlia
184000000000     c                   EVAL      deplpn = lbllpn
184100000000     c                   EVAL      depnrn = lblnrn
184200000000     c                   EVAL      depnsn = lblnsn
184300000000e   3c                   ENDIF
184400000000     c*
184500000000     c* lettura successiva legami
184600000000     c     ke2lbl        READE     fnlbl02l                               99
184700000000e   2c                   ENDDO                                                  *fine figlie mamma
184800000000     c*
184900000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
185000000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
185100000000     c                             kbllpp <> deplpn OR
185200000000     c                             kblnrp <> depnrn OR
185300000000     c                             kblnsp <> depnsn
185400000000     c* nuova chiave
185500000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
185600000000     c                   EVAL      kbllpp =  deplpn
185700000000     c                   EVAL      kblnrp =  depnrn
185800000000     c                   EVAL      kblnsp =  depnsn
185900000000     c* controlla figlie
186000000000     c     ke2lbl        SETLL     fnlbl02l
186100000000     c     ke2lbl        READE     fnlbl02l                               99
186200000000if  3c                   IF        *in99                                        *non ha figlie
186300000000     c                   LEAVE                                                  *esce dal ciclo
186400000000e   3c                   ENDIF
186500000000x   2c                   ELSE                                                   *sempre stessa mamma
186600000000     c                   LEAVE                                                  *esce dal ciclo
186700000000e   2c                   ENDIF
186800000000e   1c                   ENDDO                                                  *fine mamme
186900000000     c*
187000000000     c                   ENDSR
187100160127     c*--------------------------------------------------------------------------------------------*
187200160127     c* Imposta l'evento presa in carico se bolla in partenza
187300160127     c*--------------------------------------------------------------------------------------------*
187400160127     c     evbinca       BEGSR
187500160127     c*
187600160127     c* indice schiera
187700160127     c                   Z-ADD     tote          smi(tote)
187800160127     c* sequenza legame
187900160127     c                   Z-ADD     seql          smseql(tote)
188000160127     c* sequenza evento
188100160127     c                   Z-ADD     008           smseqe(tote)                   *primo evento
188200160127     c* data
188300160127if  1c                   IF        arbdim   > *zeros
188400160127     c                   MOVEL     arbdim        wdat
188500160127     c                   EXSR      edtdat
188600160127     c                   MOVEL     wdate         smdeve(tote)
188700160127e   1c                   ENDIF
188800160127     c* filiale
188900160127     c                   Z-ADD     arblnp        worg
189000160127     c                   EXSR      decorg
189100160127     c
189200160127     c                   MOVEL     wodes         smfle(tote)
189300160127     c* causale
189400160127     c                   MOVEL     wcev          smcev(tote)
189500160127     c* data+ora evento
189600160127     c                   MOVEL     wdat          smdevhev(tote)
189700160127     c                   MOVE      wora          smdevhev(tote)
189800160127     c*
189900160127     c                   ENDSR
190000000000     c*--------------------------------------------------------------------------------------------*
190100000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
190200000000     c*--------------------------------------------------------------------------------------------*
190300000000     c     evbrit        BEGSR
190400160318     c
190500000000     c*
190600000000     c* indice schiera
190700000000     c                   Z-ADD     tote          smi(tote)
190800000000     c* sequenza legame
190900000000     c                   Z-ADD     seql          smseql(tote)
191000000000     c* sequenza evento
191100000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
191200160407     c* data ritiro solo se spuntato
191300160318     c                   if        esito='P'
191400160407     c                   if        arbdse>0
191500160318if  1c                   IF        arbdrt > *zeros
191600160318     c                   MOVEL     arbdrt        wdat
191700160318     c                   EXSR      edtdat
191800160318     c                   MOVEL     wdate         smdeve(tote)
191900160318e   1c                   ENDIF
192000160318     c* ora  da FPP
192100160318if  2c                   IF        arbfpp = 'P'                                 *PM
192200160318     c                   MOVEL     1800          wora
192300160318x   2c                   ELSE                                                   *AM
192400160318     c                   MOVEL     1200          wora
192500160318e   1c                   ENDIF
192600160318     c                   EXSR      edtora
192700160318     c                   MOVEL     worae         smheve(tote)
192800160318     c* filiale
192900160318     c                   Z-ADD     arblnp        worg
193000160318     c                   EXSR      decorg
193100160407
193200160407     c                   endif
193300160407     c
193400160318     c
193500160318     c                   else
193600160318     c
193700000000if  1c                   IF        s_tasdrt > *zeros
193800000000     c                   MOVEL     s_tasdrt      wdat
193900000000     c                   EXSR      edtdat
194000000000     c                   MOVEL     wdate         smdeve(tote)
194100000000e   1c                   ENDIF
194200000000     c* ora
194300000000if  1c                   IF        s_tashrt > *zeros
194400000000     c                   MOVEL     s_tashrt      wora
194500000000x   1c                   ELSE
194600000000if  2c                   IF        s_tasfpp = 'P'                               *PM
194700000000     c                   MOVEL     1800          wora
194800000000x   2c                   ELSE                                                   *AM
194900000000     c                   MOVEL     1200          wora
195000000000e   2c                   ENDIF
195100000000e   1c                   ENDIF
195200000000     c                   EXSR      edtora
195300000000     c                   MOVEL     worae         smheve(tote)
195400000000     c* filiale
195500000000     c                   Z-ADD     s_taslnp      worg
195600000000     c                   EXSR      decorg
195700150618     c
195800150618     c* Se la bolla è import e devo emettere l'evento di telefonare
195900150618     c*  non posso indciare la filiale esterna, ma l'hub italiano
196000150618     c                   if        wofl1=ESTERO and sostrit='S' and
196100150618     c                             §7oinc='T'
196200150618     c                   z-add     s_tastfp      worg
196300150618     c                   EXSR      decorg
196400150618     c                   endif
196500160318     c                   endif
196600160318
196700000000     c                   MOVEL     wodes         smfle(tote)
196800000000     c* causale
196900000000     c                   MOVEL     wcev          smcev(tote)
197000000000     c* data+ora evento
197100000000     c                   MOVEL     wdat          smdevhev(tote)
197200000000     c                   MOVE      wora          smdevhev(tote)
197300000000     c*
197400000000     c                   ENDSR
197500000000     c*--------------------------------------------------------------------------------------------*
197600000000     c* Imposta l'evento Partenza
197700000000     c*--------------------------------------------------------------------------------------------*
197800000000     c     evbpar        BEGSR
197900000000     c*
198000000000     c* controlla SE e COME inserire l'evento
198100000000if  1c                   IF        lduc > *zeros
198200000000     c* imposta causale
198300000000     c                   MOVEL     '702'         wcev
198400000000     c                   EXSR      deccevsta
198500000000if  2c                   IF        werr <> '1'                                  *no errore
198600000000     c* imposta filiale
198700000000     c                   Z-ADD     llnp          worg
198800000000     c                   EXSR      decorg
198900000000     c* imposta data
199000101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
199100000000     c                   MOVEL     ldrt          wdat                           *data ritiro
199200000000x   3c                   ELSE                                                   partenza ITALIA
199300000000     c                   MOVEL     lduc          wdat                           *data partenza
199400000000e   3c                   ENDIF
199500000000     c                   EXSR      edtdat
199600000000     c* imposta ora
199700000000     c                   MOVEL     2100          wora                           *ora imposta
199800000000     c                   EXSR      edtora
199900000000     c*
200000000000     c* controlla se l'evento è già stato inserito
200100000000     c                   MOVEL     'S'           newevb                         *nuovo evento
200200000000do  3c     1             DO        200           j
200300000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
200400000000     c                   MOVE      wora          lavdevhev
200500000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
200600000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
200700000000     c                   MOVEL     'N'           newevb                         *già inserito
200800000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
200900000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
201000000000     c                   LEAVE                                                  *uscita ciclo
201100000000e   4c                   ENDIF
201200000000e   3c                   ENDDO
201300000000     c*
201400000000     c* incrementa contatore eventi
201500000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
201600000000     c                   ADD       1             tote
201700000000     c* indice schiera
201800000000     c                   Z-ADD     tote          smi(tote)
201900000000     c* sequenza legame
202000000000     c                   Z-ADD     seql          smseql(tote)
202100000000     c* sequenza evento
202200000000     c                   Z-ADD     020           smseqe(tote)
202300000000     c* descrizione
202400030130     c                   MOVEL     wcdex         smdev(tote)
202500000000     c* filiale -impostata sopra-
202600000000     c                   MOVEL     wodes         smfle(tote)
202700000000     c* data    -impostata sopra-
202800000000     c                   MOVEL     wdate         smdeve(tote)
202900000000     c* ora     -impostata sopra-
203000000000     c                   MOVEL     worae         smheve(tote)
203100000000     c* causale -impostata sopra-
203200000000     c                   MOVEL     wcev          smcev(tote)
203300000000     c* data+ora evento
203400000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
203500000000     c                   MOVE      wora          smdevhev(tote)
203600000000e   3c                   ENDIF
203700000000e   2c                   ENDIF
203800000000e   1c                   ENDIF
203900000000     c*
204000000000     c                   ENDSR
204100101129     c*------------------------------------------------------------------------*
204200101129     c*
204300101129     c* Imposta l'evento Arrivo
204400101129     c*
204500101129     c*------------------------------------------------------------------------*
204600000000     c     evbarr        BEGSR
204700101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
204800101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
204900101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
205000101129     ** sommato inutile.
205100150618     C                   IF        wrkLnaItaEst = ESTERO
205200150618     C                   LEAVESR
205300150618     C                   ENDIF
205400101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
205500101129     C                   IF        lhti = *ZERO
205600101129     C                   LEAVESR
205700101129     C                   ENDIF
205800000000     c*
205900000000     c* controlla SE e COME inserire l'evento
206000000000if  1c                   IF        ldti > *zeros OR
206100000000     c                             ldcm > *zeros
206200000000     c* imposta causale
206300101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
206400000000     c                   EXSR      deccevsta
206500000000if  2c                   IF        werr <> '1'                                  *no errore
206600000000     c* imposta filiale
206700150617     c                   Z-ADD     llna          worg
206800000000     c                   EXSR      decorg
206900000000     c* imposta data
207000000000if  3c                   IF        ldti > *zeros
207100000000     c                   MOVEL     ldti          wdat                           *data arrivo
207200000000x   3c                   ELSE
207300000000     c                   MOVEL     ldcm          wdat                           *data consegna
207400000000e   3c                   ENDIF
207500000000     c                   EXSR      edtdat
207600000000     c* imposta ora
207700000000if  3c                   IF        lhti > *zeros
207800000000     c                   MOVEL     lhti          wora                           *ora arrivo
207900000000x   3c                   ELSE
208000000000     c                   MOVEL     0600          wora                           *ora imposta
208100000000e   3c                   ENDIF
208200000000     c                   EXSR      edtora
208300000000     c*
208400000000     c* controlla se l'evento è già stato inserito
208500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
208600000000do  3c     1             DO        200           j
208700000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
208800000000     c                   MOVE      wora          lavdevhev
208900000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
209000000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
209100000000     c                   MOVEL     'N'           newevb                         *già inserito
209200000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
209300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
209400000000     c                   LEAVE                                                  *uscita ciclo
209500000000e   4c                   ENDIF
209600000000e   3c                   ENDDO
209700000000     c*
209800000000     c* incrementa contatore eventi
209900000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
210000000000     c                   ADD       1             tote
210100000000     c* indice schiera
210200000000     c                   Z-ADD     tote          smi(tote)
210300000000     c* sequenza legame
210400000000     c                   Z-ADD     seql          smseql(tote)
210500000000     c* sequenza evento
210600000000     c                   Z-ADD     020           smseqe(tote)
210700000000     c* descrizione
210800030130     c                   MOVEL     wcdex         smdev(tote)
210900000000     c* data    -impostata sopra-
211000000000     c                   MOVEL     wdate         smdeve(tote)
211100000000     c* ora     -impostata sopra-
211200000000     c                   MOVEL     worae         smheve(tote)
211300000000     c* filiale -impostata sopra-
211400000000     c                   MOVEL     wodes         smfle(tote)
211500000000     c* causale -impostata sopra-
211600000000     c                   MOVEL     wcev          smcev(tote)
211700000000     c* data+ora evento
211800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
211900000000     c                   MOVE      wora          smdevhev(tote)
212000000000e   3c                   ENDIF
212100000000e   2c                   ENDIF
212200000000e   1c                   ENDIF
212300000000     c*
212400000000     c                   ENDSR
212500000000     c*--------------------------------------------------------------------------------------------*
212600000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
212700000000     c*--------------------------------------------------------------------------------------------*
212800140714     c     evbcon        BEGSR
212900000000     c*
213000000000     c* azzera variabili di lavoro
213100000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
213200000000     c****
213300000000     c* Consegna merce ->
213400000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
213500000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
213600000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
213700000000     c*   nè lo stato di consegna nè quello di consegna anomala.
213800000000     c****
213900000000     c* controlla SE e COME inserire l'evento
214000000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
214100060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
214200060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
214300060622     C                             :rpyEsito)
214400060622     C                   IF        rpyOpCode = 'FOUND'
214500041222     **?Consegna anomala, POD non visualizzabile.
214600060622     C                   IF        §7OPODImg = 'N'
214700041222     C                   EVAL      LDVO74 = 'N'
214800041222     C                   ENDIF
214900060622     c                   IF        §7oinc <> *blanks                            *ok per internet
215000060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
215100000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
215200000000x   3c                   ELSE                                                   *ha record FNEVB
215300000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
215400000000e   3c                   ENDIF
215500041222e   3c                   ENDIF
215600000000e   2c                   ENDIF
215700000000e   1c                   ENDIF
215800080131     c* imposta data
215900080131if  1c                   IF        ldcm > *zeros
216000080131     c                   MOVEL     ldcm          wdat
216100080131e   1c                   ENDIF
216200080131     c* imposta ora
216300080131if  1c                   IF        lhmc > *zeros
216400080131     c                   MOVEL     lhmc          wora
216500080131e   1c                   ENDIF
216600000000     c* imposta causale
216700000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
216800060622     c                   MOVEL     §7ocev        wcev                           *causale evento
216900080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
217000080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
217100080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
217200080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
217300080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
217400080131     c* ritiro merce) dalla bolla figlia.
217500080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
217600080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
217700080131     c                             AND s_tasNdc < *HIVAL
217800080131     c                   EVAL      kblAap = wAas
217900080131     c                   EVAL      kblLpp = wLnp
218000080131     c                   EVAL      kblNrp = wNrs
218100080131     c                   EVAL      kblNsp = wNsp
218200080131     c     ke2lbl        CHAIN     fnlbl02l
218300080131     c                   IF        %FOUND()
218400080131     c                   EVAL      kasAas = lblAan
218500080131     c                   EVAL      kasLnp = lblLpn
218600080131     c                   EVAL      kasNrs = lblNrn
218700080131     c                   EVAL      kasNsp = lblNsn
218800080131     C     keyTas        CHAIN     titas000      titas000Figlia
218900080131     C                   IF        %FOUND()
219000080131     C                   EVAL      wDat = titas000Figlia.tasDrt
219100080131     C                   EVAL      wOra = titas000Figlia.tasHrt
219200080131     C                   ELSE
219300080131     C     keyTas        CHAIN     titas010      titas010Figlia
219400080131     C                   IF        %FOUND()
219500080131     C                   EVAL      wDat = titas010Figlia.tasDrt
219600080131     C                   EVAL      wOra = titas010Figlia.tasHrt
219700080131     C                   ELSE
219800080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
219900080131     C                   IF        %FOUND()
220000080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
220100080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
220200080131     C                   ENDIF
220300080131     C                   ENDIF
220400080131     C                   ENDIF
220500080131     c                   ENDIF
220600080131     c                   ENDIF
220700000000x   1c                   ELSE                                                   *NO sost.Consegna
220800000000     c                   MOVEL     '704'         wcev                           *causale evento
220900000000e   1c                   ENDIF
221000080131     c* Editazione data e ora evento.
221100080131     c                   EXSR      edtdat
221200080131     c                   EXSR      edtora
221300000000     c* imposta filiale
221400000000     c                   Z-ADD     llna          worg
221500000000     c                   EXSR      decorg
221600150618     c* se si tratta di lna estera e la bolla ha consegna anomala
221700150618     c*  in cui mettere msg generico, allora devo sostituire la filiale
221800150618     c*  con il suo hub
221900150618     c                   if        wofl1=estero and lcca<>' ' and
222000150618     c                             §7oinc='T'
222100150618     c                   Z-ADD     ltfa          worg
222200150618     c                   EXSR      decorg
222300150618     c                   endif
222400150618     c
222500000000     c* controlla se l'evento è già stato inserito
222600000000     c                   MOVEL     'S'           newevb                         *nuovo evento
222700000000do  1c     1             DO        200           j
222800000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
222900000000     c                   MOVE      wora          lavdevhev
223000000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
223100000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
223200000000     c                   MOVEL     'N'           newevb                         *già inserito
223300000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
223400000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
223500000000     c                   LEAVE                                                  *uscita ciclo
223600000000e   2c                   ENDIF
223700000000e   1c                   ENDDO
223800000000     c*
223900000000     c* incrementa contatore eventi
224000030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
224100030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
224200000000     c                   ADD       1             tote                           incrementa contatore
224300060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
224400060622     c                   MOVEL     §7odei        smdev(tote)
224500030203x   4c                   ELSE                                                   *evento telef.P.O.
224600060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
224700030203e   4c                   ENDIF
224800060622     c                   MOVEL     §7ocev        wcev                           *causale evento
224900000000     c                   EXSR      evbcon2                                      *imposta evento
225000030203x   3c                   ELSE                                                   *NO sost.Consegna
225100000000     c*
225200030203if  4c                   IF        ldcm > *zeros
225300030203if  5c                   IF        sostcon <> ' '                               *considera consegna
225400000000     c                   EXSR      deccevsta
225500030203if  6c                   IF        werr <> '1'                                  *no errore
225600000000     c                   ADD       1             tote                           incrementa contatore
225700030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
225800000000     c                   EXSR      evbcon2                                      *imposta evento
225900030203e   6c                   ENDIF
226000030203e   5c                   ENDIF
226100030203e   4c                   ENDIF
226200030203e   3c                   ENDIF
226300000000     c*
226400030203e   1c                   ENDIF
226500030203     c*
226600000000     c                   ENDSR
226700000000     c*--------------------------------------------------------------------------------------------*
226800000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
226900000000     c*--------------------------------------------------------------------------------------------*
227000000000     c     evbcon2       BEGSR
227100000000     c*
227200000000     c* indice schiera
227300000000     c                   Z-ADD     tote          smi(tote)
227400000000     c* sequenza legame
227500000000     c                   Z-ADD     seql          smseql(tote)
227600000000     c* sequenza evento
227700000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
227800000000     c* data    -impostata sopra-
227900000000     c                   MOVEL     wdate         smdeve(tote)
228000000000     c* ora     -impostata sopra-
228100000000     c                   MOVEL     worae         smheve(tote)
228200000000     c* filiale -impostata sopra-
228300000000     c                   MOVEL     wodes         smfle(tote)
228400000000     c* causale -impostata sopra-
228500000000     c                   MOVEL     wcev          smcev(tote)
228600000000     c* data+ora evento
228700000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
228800000000     c                   MOVE      wora          smdevhev(tote)
228900000000     c*
229000000000     c                   ENDSR
229100000000     c*--------------------------------------------------------------------------------------------*
229200000000     c* memorizza i dati della lettera di anomalia
229300000000     c*--------------------------------------------------------------------------------------------*
229400000000     c     memdct        BEGSR
229500000000     c*
229600040906     C                   RESET                   FIDN12DS
229700040906     c                   Z-ADD     waas          I12AAS
229800040906     c                   Z-ADD     wlnp          I12LNP
229900040906     c                   Z-ADD     wnrs          I12NRS
230000040906     c                   Z-ADD     wnsp          I12NSP
230100040906     C                   CALL      'FIDN12R'
230200040906     C                   PARM                    FIDN12DS
230300000000     c* memorizza la prima lettera di anomalia valida
230400040906do  1c                   IF        O12Err = *BLANK
230500040906     C                   DO        O12NCA        K
230600040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
230700040906     C                   EVAL      DCTAAC = O12KeyAAC
230800040906     C                   EVAL      DCTFIL = O12KeyFil
230900040906     C                   EVAL      DCTNCA = O12KeyNCA
231000040906     c     keydct        CHAIN     fndct01l
231100040906do  1c                   IF        %FOUND
231200000000     c*
231300000000     c* controlla validità CA
231400000000     c                   MOVEL     '0'           werr                           *NO errore
231500000000     c                   EXSR      chkdct
231600000000if  2c                   IF        werr = '0'                                   *CA valida
231700000000     c*
231800000000     c* Imposta l'evento lettera di anomalia
231900000000     c                   EXSR      evbdct
232000000000     C                   LEAVE                                                  *uscita ciclo
232100000000e   2c                   ENDIF
232200000000     c*
232300040906e   1c                   ENDIF
232400040906e   1c                   ENDDO
232500040906e   1c                   ENDIF
232600000000     C*
232700000000     c                   ENDSR
232800000000     c*--------------------------------------------------------------------------------------------*
232900000000     c* controlla validità lettera di anomalia
233000000000     c*--------------------------------------------------------------------------------------------*
233100000000     c     chkdct        BEGSR
233200000000     c*
233300000000     c* reimposta variabili di lavoro
233400000000     c                   MOVEL     '0'           werr                           *NO errore
233500000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
233600000000     c*
233700000000     c* CA annullata, esclude
233800000000if  1c                   IF        dctatb <> ' '
233900000000     c                   MOVEL     '1'           werr                           *SI errore
234000000000e   1C                   ENDIF
234100000000     c*
234200000000     c* CA non confermata, esclude
234300000000if  1c                   IF        dctfpr = ' '
234400000000     c                   MOVEL     '1'           werr                           *SI errore
234500000000e   1C                   ENDIF
234600000000     c*
234700000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
234800000000if  1c                   IF        dctfpr = 'T' AND
234900000000     c                             dctfca >= 215 OR
235000000000     c                             dctfpr = 'P' AND
235100000000     c                             dctfca >= 400
235200000000x   1c                   ELSE
235300000000     c                   MOVEL     '1'           werr                           *SI errore
235400000000e   1C                   ENDIF
235500000000     c*
235600000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
235700000000if  1c                   IF        werr = '0'
235800000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
235900060622     C                   RESET                   tibs02ds
236000060622     C                   EVAL      t02Cod = 'CCH'
236100060622     C                   EVAL      t02Ke1 = dctcch
236200060622     C                   EXSR      getTntbe00f
236300060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
236400000000     c                   MOVEL     '1'           werr                           *SI errore
236500000000e   3C                   ENDIF
236600000000e   2C                   ENDIF
236700000000e   1C                   ENDIF
236800000000     c*
236900000000     c                   ENDSR
237000000000     c*--------------------------------------------------------------------------------------------*
237100000000     c* Imposta l'evento lettera di anomalia
237200000000     c*--------------------------------------------------------------------------------------------*
237300000000     c     evbdct        BEGSR
237400000000     C*
237500000000     c* imposta causale
237600000000if  1c                   IF        wfpr = 'T'
237700000000     c                   MOVEL     '709'         wcev                           *causale evento
237800000000x   1c                   ELSE
237900000000     c                   MOVEL     '710'         wcev                           *causale evento
238000000000e   1c                   ENDIF
238100000000     c                   EXSR      deccevsta                                    *decodifica evento
238200000000     c*
238300000000     c* imposta filiale
238400000000if  1c                   IF        werr <> '1'                                  *no errore
238500000000     c                   Z-ADD     dctfil        worg
238600000000     c                   EXSR      decorg
238700000000     c* imposta data
238800000000     c                   MOVEL     dctaac        wdat
238900000000     c                   MOVE      dctmgc        wdat
239000000000     c                   EXSR      edtdat
239100000000     c* imposta ora
239200000000     c                   MOVEL     1100          wora                           *FISSA
239300000000     c                   EXSR      edtora
239400000000     c*
239500000000     c* incrementa contatore eventi
239600000000     c                   ADD       1             tote
239700000000     c* indice schiera
239800000000     c                   Z-ADD     tote          smi(tote)
239900000000     c* sequenza legame
240000000000     c                   Z-ADD     seql          smseql(tote)
240100000000     c* sequenza evento
240200000000     c                   Z-ADD     999           smseqe(tote)
240300000000     c* descrizione -impostata sopra-
240400030130     c                   MOVEL     wcdex         smdev(tote)
240500000000     c* filiale -impostata sopra-
240600000000     c                   MOVEL     wodes         smfle(tote)
240700000000     c* data    -impostata sopra-
240800000000     c                   MOVEL     wdate         smdeve(tote)
240900000000     c* ora     -impostata sopra-
241000000000     c                   MOVEL     worae         smheve(tote)
241100000000     c* causale -impostata sopra-
241200000000     c                   MOVEL     wcev          smcev(tote)
241300000000     c* data+ora evento
241400000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
241500000000     c                   MOVE      wora          smdevhev(tote)
241600000000e   1c                   ENDIF
241700000000     c*
241800000000     c                   ENDSR
241900000000     c*--------------------------------------------------------------------------------------------*
242000000000     c* imposta i campi della DS di Output dal contrassegno
242100000000     c*--------------------------------------------------------------------------------------------*
242200000000     c     impdsocsb     BEGSR
242300000000     c*
242400000000     c* azzera le variabili di lavoro
242500000000     c*
242600000000     c****
242700000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
242800000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
242900000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
243000000000     c****
243100000000     c*
243200000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
243300090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
243400000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
243500000000x   1c                   ELSE                                                     *trovato
243600000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
243700000000e   1C                   ENDIF
243800000000     c*---
243900000000     c* memorizza il contrassegno della bolla di Input
244000000000     c*---
244100000000     c                   EVAL      waas = s_tasaas                              *bolla Input
244200000000     c                   EVAL      wlnp = s_taslnp
244300000000     c                   EVAL      wnrs = s_tasnrs
244400000000     c                   EVAL      wnsp = s_tasnsp
244500000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
244600000000     c                   EXSR      memcsb
244700000000     c*
244800000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
244900000000     c                   EXSR      elacsblbl
245000000000     c*
245100000000     c                   ENDSR
245200000000     c*--------------------------------------------------------------------------------------------*
245300000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
245400000000     c*--------------------------------------------------------------------------------------------*
245500000000     c     elacsblbl     BEGSR
245600000000     c*
245700000000     c* azzera le variabili di lavoro
245800000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
245900000000     c                   EVAL      deplpn = *zeros
246000000000     c                   EVAL      depnrn = *zeros
246100000000     c                   EVAL      depnsn = *zeros
246200000000     c*
246300000000     c* legge i legami delle figlie successive alla bolla di input
246400000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
246500000000     c                   EVAL      kbllpp =  s_taslnp
246600000000     c                   EVAL      kblnrp =  s_tasnrs
246700000000     c                   EVAL      kblnsp =  s_tasnsp
246800000000     c*
246900000000     c* ciclo fino a fine mamme
247000000000     c     ke2lbl        SETLL     fnlbl02l
247100000000     c     ke2lbl        READE     fnlbl02l                               99
247200000000do  1c                   DOW       NOT *in99
247300000000     c*
247400000000     c* ciclo fino a fine figlie della mamma
247500000000do  2c                   DOW       NOT *in99
247600000000     c*
247700000000     c* legge la bolla dal legame
247800000000     c                   EVAL      kasaas = lblaan                              *figlia
247900000000     c                   EVAL      kaslnp = lbllpn
248000000000     c                   EVAL      kasnrs = lblnrn
248100000000     c                   EVAL      kasnsp = lblnsn
248200000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
248300000000     c*---
248400000000     c* memorizza il contrassegno della bolla figlia legata
248500000000     c*---
248600000000if  4c                   IF        NOT *in99                                    *esistente
248700000000     c                   EVAL      waas = lblaan                                *figlia
248800000000     c                   EVAL      wlnp = lbllpn
248900000000     c                   EVAL      wnrs = lblnrn
249000000000     c                   EVAL      wnsp = lblnsn
249100000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
249200000000     c                   EXSR      memcsb
249300000000e   4c                   ENDIF
249400000000     c*
249500000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
249600000000     c                   EVAL      depaan = lblaan                              *ultima figlia
249700000000     c                   EVAL      deplpn = lbllpn
249800000000     c                   EVAL      depnrn = lblnrn
249900000000     c                   EVAL      depnsn = lblnsn
250000000000     c*
250100000000     c* lettura successiva legami
250200000000     c     ke2lbl        READE     fnlbl02l                               99
250300000000e   2c                   ENDDO                                                  *fine figlie mamma
250400000000     c*
250500000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
250600000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
250700000000     c                             kbllpp <> deplpn OR
250800000000     c                             kblnrp <> depnrn OR
250900000000     c                             kblnsp <> depnsn
251000000000     c* nuova chiave
251100000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
251200000000     c                   EVAL      kbllpp =  deplpn
251300000000     c                   EVAL      kblnrp =  depnrn
251400000000     c                   EVAL      kblnsp =  depnsn
251500000000     c* controlla figlie
251600000000     c     ke2lbl        SETLL     fnlbl02l
251700000000     c     ke2lbl        READE     fnlbl02l                               99
251800000000if  3c                   IF        *in99                                        *non ha figlie
251900000000     c                   LEAVE                                                  *esce dal ciclo
252000000000e   3c                   ENDIF
252100000000x   2c                   ELSE                                                   *sempre stessa mamma
252200000000     c                   LEAVE                                                  *esce dal ciclo
252300000000e   2c                   ENDIF
252400000000e   1c                   ENDDO                                                  *fine mamme
252500000000     c*
252600000000     c                   ENDSR
252700000000     c*--------------------------------------------------------------------------------------------*
252800000000     c* memorizza il contrassegno
252900000000     c*--------------------------------------------------------------------------------------------*
253000000000     c     memcsb        BEGSR
253100000000     c*
253200000000     c* legge il contrassegno, se avuto
253300000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
253400000000     c                   Z-ADD     waas          ksbaas
253500000000     c                   Z-ADD     wlnp          ksblnp
253600000000     c                   Z-ADD     wnrs          ksbnrs
253700000000     c                   Z-ADD     wnsp          ksbnsp
253800000000     c     keycsb        CHAIN     tncsb03l                           99
253900000000if  2c                   IF        NOT *in99                                    *esistente
254000000000     c* memorizza i dati
254100000000     c                   MOVEL     'S'           cao74                          *esiste C/A
254200000000     c*
254300000000     c                   Z-ADD     csbcas        caimpo74                       *importo
254400000000     c*
254500000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
254600010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
254700010820     c                             caimpo74 > *zeros
254800010820     c                   EVAL      cadivo74 = 'ITL'
254900010820e   3c                   ENDIF
255000160314
255100160314     ** Visualizzo la descrizione del tipo incasso
255200100121     C                   EVAL      cainco74 =
255300100121     C                                       GetDescrizioneIncassoContrassegno()
255400000000     c*
255500000000     c                   MOVEL     *blanks       castao74                       *stato
255600060622     C                   EVALR     tblKey = %CHAR(csbSta)
255700060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
255800060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
255900060622     C                             :rpyOpCode:rpyEsito)
256000060622     C                   IF        rpyOpCode = 'FOUND'
256100060622     c                   EVAL      castao74 = §4SDei
256200000000e   3c                   ENDIF
256300100119     c
256400000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
256500011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
256600060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
256700000000x   4c                   ELSE
256800000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
256900060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
257000000000x   5c                   ELSE
257100060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
257200000000e   5c                   ENDIF
257300000000e   4c                   ENDIF
257400000000e   3c                   ENDIF
257500000000     c*
257600000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
257700000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
257800000000if  4c                   IF        csbtpa = 'B'
257900060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
258000000000e   4c                   ENDIF
258100000000if  4c                   IF        csbtpa = 'C'
258200060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
258300000000e   4c                   ENDIF
258400000000x   3c                   ELSE                                                   *incasso Bartolini
258500000000if  4c                   IF        csbfpc = *blanks
258600110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
258700000000e   4c                   ENDIF
258800000000if  4c                   IF        csbfpc = '2'
258900060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
259000000000e   4c                   ENDIF
259100000000e   3c                   ENDIF
259200000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
259300000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
259400000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
259500000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
259600000000     c                             csbfpc = *blanks                              assegno di traenza
259700060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
259800000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
259900060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
260000000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
260100060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
260200000000e   4c                   ENDIF
260300000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
260400060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
260500060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
260600000000e   3c                   ENDIF
260700000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
260800000000     c                   Z-ADD     *zeros        caabio74                       *abi
260900000000     c                   Z-ADD     *zeros        cacabo74                       *cab
261000000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
261100000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
261200000000     c                   MOVEL     csbddp        wdat
261300000000     c                   EXSR      edtdat
261400000000     c                   EVAL      cadato74 = wdate
261500000000     C*
261600000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
261700000000     c                   Z-ADD     csbabi        caabio74
261800000000     c                   Z-ADD     csbcab        cacabo74
261900000000     c                   MOVEL     csbnra        canumo74
262000120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
262100120321     c                             : caCabO74)
262200000000x   4c                   ELSE                                                   *incasso Bartolini
262300000000     c                   MOVEL     csbndp        canumo74
262400000000e   4c                   ENDIF
262500000000e   3c                   ENDIF
262600110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
262700110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
262800110307     C                   IF        IsContrassegnoCompensato()
262900110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
263000110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
263100110304     C                   CLEAR                   caabio74
263200110304     C                   CLEAR                   cacabo74
263300110304     C                   CLEAR                   canumo74
263400110304     C                   ENDIF
263500110304     C*
263600000000e   2c                   ENDIF
263700000000e   1c                   ENDIF
263800000000     c*
263900000000     c                   ENDSR
264000000000     c*--------------------------------------------------------------------------------------------*
264100000000     c* imposta i campi della DS di Output dalla giacenza
264200000000     c*--------------------------------------------------------------------------------------------*
264300000000     c     impdsogcp     BEGSR
264400000000     c*
264500000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
264600000000     c                   EVAL      waas = s_tasaas                              *bolla Input
264700000000     c                   EVAL      wlnp = s_taslnp
264800000000     c                   EVAL      wnrs = s_tasnrs
264900000000     c                   EVAL      wnsp = s_tasnsp
265000000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
265100000000     c                   EXSR      memgcp
265200000000     c*
265300000000     c                   ENDSR
265400000000     c*--------------------------------------------------------------------------------------------*
265500000000     c* memorizza la giacenza
265600000000     c*--------------------------------------------------------------------------------------------*
265700000000     c     memgcp        BEGSR
265800000000     c*
265900000000     c* imposta le variabili di lavoro
266000000000     c                   CLEAR                   i                              *indice
266100000000     c                   CLEAR                   ii
266200000000     c                   CLEAR                   smdmc                          *note fase -20-
266300000000     c                   CLEAR                   smdmcR
266400000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
266500000000     c                   MOVEL     'N'           gco74                          *flag di controllo
266600000000     c                   MOVEL     'N'           gcfdio74
266700000000     c                   MOVEL     'N'           gcmito74
266800000000     c*
266900000000     c* legge la giacenza, se avuta
267000000000     c                   Z-ADD     waas          kcpaas
267100000000     c                   Z-ADD     wlnp          kcplnp
267200000000     c                   Z-ADD     wnrs          kcpnrs
267300000000     c                   Z-ADD     wnsp          kcpnsp
267400000000     c                   Z-ADD     *zeros        kcpfrg
267500050309     c     keygcp        CHAIN     tigcp51l                           99
267600050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
267700050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
267800000000if  2c                   IF        NOT *in99        AND                         *esistente
267900000000     c                             gcpatb = *blanks AND                         *no annullata
268000050415     c                             gcptcm <> 'Z'    AND
268100050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
268200050419     C
268300050419     C                   EVAL      gcRiapO74 = gcpRiap
268400050418     **?Descrizione della fase.
268500050418     C                   SELECT
268600050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
268700060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
268800050418     C                   WHEN      gcpFas = 20
268900060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
269000050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
269100060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
269200050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
269300060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
269400050418     C                   ENDSL
269500000000     c* memorizza i dati dalle NOTE GIACENZE
269600000000     c                   Z-ADD     *zeros        i
269700000000     c                   Z-ADD     *zeros        ii
269800000000     c                   Z-ADD     gcpagc        knpagc
269900000000     c                   Z-ADD     gcpfgc        knpfgc
270000000000     c                   Z-ADD     gcpngc        knpngc
270100000000     c                   Z-ADD     gcpfrg        knpfrg
270200050309     c     keygnp        SETLL     tignp01l
270300050309     c     keygnp        READE     tignp01l                               98
270400000000do  3c                   DOW       NOT *in98
270500050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
270600000000     c* fase -10-
270700000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
270800000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
270900000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
271000000000e   5c                   ENDIF
271100000000     c* fase -20-
271200000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
271300000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
271400051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
271500000000     c                   ADD       1             i
271600000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
271700000000e   6c                   ENDIF
271800000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
271900051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
272000050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
272100050405     c                   CLEAR                   ii
272200050405     c                   CLEAR                   smdmcR
272300050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
272400050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
272500050919     C                   IF        NOT %ERROR
272600140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
272700050919     C                   ENDIF
272800050405     C                   ELSE
272900000000     c                   ADD       1             ii
273000000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
273100050405     C                   ENDIF
273200000000e   6c                   ENDIF
273300000000e   5c                   ENDIF
273400000000e   4c                   ENDIF
273500050309     c     keygnp        READE     tignp01l                               98
273600000000e   3c                   ENDDO
273700000000     c*
273800000000     c* memorizza i dati dalle GIACENZE
273900000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
274000050404     c* Disposizione da immettere.
274100051110     c                   IF        gcpfas = 20
274200051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
274300051110     C                   EXSR      chkTivpi
274400051110e   3c                   ENDIF
274500000000     c*
274600000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
274700000000     c*
274800000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
274900000000     c*
275000060103     c                   Z-ADD     gcpagc        dsannB
275100060103     c                   Z-ADD     gcpmgc        dsmgsB
275200060103     c                   Z-ADD     dsdatB        wdat
275300000000if  3c                   IF        wdat > *zeros
275400000000     c                   EXSR      edtdat
275500000000     c                   EVAL      gcdago74 = wdate                             *data apertura
275600000000e   3C                   ENDIF
275700060104     c*
275800060104     c                   Z-ADD     gcpdur        wdat
275900060104     c                   IF        wdat > *zeros
276000060104     c                   EXSR      edtdat
276100060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
276200060104     C                   ENDIF
276300000000     c*
276400000000if  3c                   IF        gcpcmc <> *blanks
276500000000     c                   MOVEL     gcpcmc        wcev
276600000000     c                   EXSR      deccevgia
276700030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
276800000000e   3C                   ENDIF
276900000000     c*
277000000000     c                   MOVEL     *blanks       gcdcgo74
277100000000if  3c                   IF        gcpdcg > *zeros
277200000000     c                   Z-ADD     gcpdcg        wdat
277300000000     c                   EXSR      edtdat
277400000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
277500000000e   3c                   ENDIF
277600000000     c*---
277700000000     c* Disposizioni già immesse
277800000000     c*---
277900000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
278000000000     c*
278100000000if  4c                   IF        gcpddm > *zeros
278200000000     c                   Z-ADD     gcpddm        wdat
278300000000     c                   EXSR      edtdat
278400000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
278500000000e   4C                   ENDIF
278600000000     c*
278700000000if  4c                   IF        gcpdis <> *blanks
278800060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
278900060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
279000060626     C                             :rpyOpCode:rpyEsito)
279100060622     C                   EVAL      gcdiso74 = §2DDes
279200000000e   4C                   ENDIF
279300000000     c*
279400000000if  4c                   IF        gcpasg <> *blanks
279500000000if  5c                   IF        gcpasg = 'S'
279600060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
279700000000x   5c                   ELSE
279800060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
279900000000e   5C                   ENDIF
280000000000     c*
280100000000if  5c                   IF        gcppsg = 'M'
280200060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
280300000000e   5C                   ENDIF
280400000000if  5c                   IF        gcppsg = 'D'
280500060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
280600000000e   5C                   ENDIF
280700000000e   4C                   ENDIF
280800000000     c*
280900000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
281000000000     c                   MOVEL     gcpvcs        gcvcso74
281100000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
281200000000     c                   Z-ADD     gcpcas        gccaso74
281300000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
281400000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
281500000000e   5C                   ENDIF
281600000000e   4C                   ENDIF
281700050404     C                   IF        smdmc(1) <> *BLANK
281800050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
281900050404     C                             smdmc(3) + smdmc(4)
282000051014     C                   EVAL      gcdmc5O74 = smdmc(5)
282100050404     C                   ELSE
282200050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
282300050404     C                             smdmcr(3) + smdmcr(4)
282400051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
282500050404e   3C                   ENDIF
282600000000e   3C                   ENDIF
282700000000     c*---
282800000000     c* Disposizioni da immettere
282900000000     c*---
283000010719     c* se le disposizioni sono da immettere controlla che:
283100010719     c* - il cliente è il mittente della giacenza
283200000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
283300170315     C* CON KEY GIAC --> SEMPRE SI POSSONO IMMETTERE
283400170315     C                   SELECT
283500170315if  4C                   WHEN      keygiac <> *blanks
283600170315     c                   MOVEL     'S'           gcmito74                       SI POSSO IMMETTERE
283700170315     c
283800170315     C* CON BRTCODE  --> richiamo pgm fnlgw00r
283900170315if  4C                   WHEN      wbrtcode <> *blanks
284000170315     c                   exsr      chiamalgw
284100170315x   4c                   other
284200170315     c* per gli altri casi --> o loggato o anonimo ( gesrtito dal pgm seguente)
284300090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
284400090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
284500090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
284600090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
284700090914     C                             )
284800170315e   4C                   ENDsl
284900000000e   3C                   ENDIF
285000050404     C* Controllo esistenza storia giacenza.
285100050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
285200050404     C                   DO        *HIVAL
285300050404     C     K04GCP51      READE     TIGCP51L
285400050404     C                   IF        %EOF
285500050404     C                   LEAVE
285600050404     C                   ENDIF
285700050404     C                   IF        GCPATB = *BLANK
285800050404     C                   EVAL      StoGiaO74 = 'S'
285900050404     C                   LEAVE
286000050404     C                   ENDIF
286100050404     C                   ENDDO
286200050404     c*
286300000000e   2c                   ENDIF
286400000000     c*
286500000000     c                   ENDSR
286600170315     c*--------------------------------------------------------------------------------------------*
286700170315     c* richiamo pgm di verifica giacenze con collegamento con BRTCODE
286800170315     c*--------------------------------------------------------------------------------------------*
286900170315     c     Chiamalgw     BEGSR
287000170315
287100170315      /free
287200170315       reset  fnlgw00i0   ;
287300170315       reset  fnlgw00o0   ;
287400170315       fnlgw00i0.LINISO2=LANGI74    ;
287500170315       fnlgw00i0.tipoacc='B'         ;
287600170315       fnlgw00i0.brtcode=wbrtcode    ;
287700170315         prwrqsData = fnlgw00i0 ;
287800170315         prwrqsDatasize = %SIZE(fnlgw00i0) ;
287900170315         prwrpyDatasize = %SIZE(fnlgw00o0) ;
288000170315         prwrpyformato='         '  ;
288100170315         prwRpyFormMsg='FNLGW00M0'  ;
288200170316         prwrqsFormato='FNLGW00I0'  ;
288300170316         prwrpyFormato='FNLGW00O0'  ;
288400170315         prwRpyMsgSize  = %SIZE(fnlgw00m0);
288500170315         fnlgw00r(prwRqsOpCode : prwRpyOpCode : prwRpyIdMsg :
288600170315                prwRqsFormato : prwRqsData : prwRqsDataSize:
288700170315                prwRpyFormato : prwRpyData : prwRpyDataSize:
288800170315                prwRpyFormMsg : prwRpyMessage : prwRpyMsgSize)  ;
288900170315
289000170315         if   prwrpyOpCode = FNLGW00_RPYOPCODE_done   ;
289100170315         fnlgw00o0=prwrpydata  ;
289200170315         if fnlgw00o0.butgen=*on   ;
289300170315
289400170315         GCMITO74='S'  ;
289500170315
289600170315         endif    ;
289700170315         endif    ;
289800170315
289900170315      /end-free
290000170315
290100170315     c                   ENDSR
290200000000     c*--------------------------------------------------------------------------------------------*
290300000000     c* imposta i campi della DS di Output dalle note
290400000000     c*--------------------------------------------------------------------------------------------*
290500000000     c     impdsonot     BEGSR
290600000000     c*
290700000000     c* azzera le variabili di lavoro
290800000000     c                   CLEAR                   i
290900000000     c                   CLEAR                   mdcre                          consegna richiesta
291000000000     c                   CLEAR                   mhcre
291100030210     c                   CLEAR                   mtcre
291200000000     c                   CLEAR                   mftce                          consegna particolare
291300000000     c                   CLEAR                   wftc
291400000000     c                   CLEAR                   d1wdftc
291500000000     c                   CLEAR                   d2wdftc
291600000000     c                   CLEAR                   mgcxe                          turni chiusura
291700000000     c                   CLEAR                   wgcx
291800000000     c                   CLEAR                   d1wdgcx
291900000000     c                   CLEAR                   d2wdgcx
292000010927     c                   CLEAR                   mffd                           fermo deposito
292100010927     c                   CLEAR                   mmncB                          *motivi NON consegna
292200010927     c                   CLEAR                   mmncC
292300010927     c                   CLEAR                   mmncG
292400010927     c                   CLEAR                   mmncH
292500010927     c                   CLEAR                   mmncN
292600010927     c                   CLEAR                   mmncO
292700050701     c                   CLEAR                   mmnc8
292800050701     c                   CLEAR                   mmnc9
292900050701     c                   CLEAR                   a230
293000000000     c                   CLEAR                   we
293100030206     c                   CLEAR                   mtel                           n° tel destinatario
293200030206     c                   CLEAR                   mban                           bancali
293300030207     c                   CLEAR                   mscr                           supermercati
293400030210     c                   CLEAR                   msdace
293500030210     c                   CLEAR                   msorce
293600030210     c                   CLEAR                   msdcre
293700030210     c                   CLEAR                   mshcre
293800030210     c                   CLEAR                   mstcre
293900030210     c                   CLEAR                   msnom
294000030210     c                   CLEAR                   mscrT
294100030210     c                   CLEAR                   wmscrT
294200030210     c                   CLEAR                   s
294300030207     c
294400160127     c*imposta dati data consegna richiesta  DA BLP se bolla in partenza
294500160127     c                   if        esito='P'
294600160127     c                   eval      f_tasdcr=arbdcr
294700160127     c                   eval      f_tastcr=arbtcr
294800160127     c                   eval      f_tashcr=arbhcr
294900160127     c* imposto anche key spedizione
295000160309     c                   eval      f_tasaas=arbaas
295100160309     c                   eval      f_taslnp=arblnp
295200160309     c                   eval      f_tasnrs=arbnrs
295300160309     c                   eval      f_tasnsp=arbnsp
295400160127     c                   endif
295500160127     c
295600140716     c                   if        f_tasdcr > 0
295700140716     c                   MOVEL     f_tasdcr      wdat
295800140619     c                   EXSR      edtdat
295900140619     c                   eval      DTCONRCO74  =  wdate
296000140819if  3c                   IF        f_tastcr = ' '
296100140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
296200140819e   3c                   ENDIF
296300140819     c                   endif
296400140716     c                   if        f_tashcr > 0
296500140716     c                   MOVEL     f_tashcr      wora
296600140619     c                   EXSR      edtora
296700140619     c                   eval      ORACONRO74  =  worae
296800140619     c                   endif
296900140716     c                   eval      TPDATCRO74  =  f_tastcr
297000140619      *reperimento descrizione tipo consegna richiesta
297100140716if  3c                   IF        f_tastcr = 'P'
297200140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
297300140619e   3c                   ENDIF
297400140716if  3c                   IF        f_tastcr = 'D'
297500140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
297600140619e   3c                   ENDIF
297700000000     c*---
297800000000     c* memorizza le "note" che sono presenti nel record bolla
297900000000     c*---
298000000000     c* Consegna richiesta
298100140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
298200140619     c*m                           s_tashcr = *zeros
298300140619x   1c*m                 ELSE                                                   *SI cons.particolare
298400140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
298500140619     c*m                 MOVEL     s_tasdcr      wdat
298600140619     c*m                 EXSR      edtdat                                       *edita data
298700140619     c*m                 MOVEL     wdate         mdcre
298800140619e   2c*m                 ENDIF
298900140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
299000140619     c*m                 MOVEL     s_tashcr      wora
299100140619     c*m                 EXSR      edtora                                       *edita ora
299200140619     c*m                 MOVEL     worae         mhcre
299300140619e   2c*m                 ENDIF
299400140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
299500140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
299600140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
299700140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
299800140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
299900140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
300000140619e   3c*m                 ENDIF
300100140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
300200140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
300300140619e   3c*m                 ENDIF
300400140619if  3c*m                 IF        s_tastcr = ' '                               *il
300500140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
300600140619e   3c*m                 ENDIF
300700140619e   2c*m                 ENDIF
300800140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
300900140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
301000140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
301100140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
301200140619e   3c*m                 ENDIF
301300140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
301400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
301500140619e   3c*m                 ENDIF
301600140619if  3c*m                 IF        s_tastcr = ' '                               *il
301700140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
301800140619e   3c*m                 ENDIF
301900140619e   2c*m                 ENDIF
302000140619     c*m
302100140619if  2c*m                 IF        i < 10                                       *se c'è spazio
302200140619     c*m                 ADD       1             i                              *memorizza nota
302300140619     c*m                 EVAL      smnot(i) =
302400140619     c*m                           %TRIM(mtcre) +
302500140619     c*m                           ' '          +
302600140619     c*m                           %TRIM(mdcre) +
302700140619     c*m                           ' '          +
302800140619     c*m                           %TRIM(mhcre)
302900140619e   2c*m                 ENDIF
303000140619e   1c*m                 ENDIF
303100000000     c*
303200000000     c* Particolarità consegna -uno/due-
303300160127     c                   if        esito='P'
303400160309     c                   eval      f_tasftc = arbtc1
303500160314     c                   eval      f_tastc2 = arbtc2
303600160127     c                   endif
303700160127     c
303800140716if  1c                   IF        f_tasftc <> *blanks OR
303900140716     c                             f_tastc2 <> *blanks
304000140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
304100000000     c                   EXSR      decftc
304200000000     c                   MOVEL     wdftc         d1wdftc
304300140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
304400000000     c                   EXSR      decftc
304500000000     c                   MOVEL     wdftc         d2wdftc
304600000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
304700000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
304800060612     c                   EVAL      we = ' ' +
304900060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
305000000000x   2c                   ELSE
305100000000     c                   EVAL      we = *blanks
305200000000e   2c                   ENDIF
305300060612     c                   EVAL      mftce =
305400060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
305500060612     c                             %TRIM(d1wdftc)             +
305600060612     c                             we                         +
305700060612     c                             %TRIM(d2wdftc)
305800000000     c*
305900010927if  2c                   IF        i < 10                                       *se c'è spazio
306000000000     c                   ADD       1             i                              *memorizza nota
306100000000     c                   EVAL      smnot(i) = mftce
306200010927e   2c                   ENDIF
306300010927e   1c                   ENDIF
306400000000     c*
306500000000     c* Chiusura per turno
306600160127     c                   if        esito='P'
306700160309     c                   eval      f_tasgc1 = arbgc1
306800160309     c                   eval      f_tasgc1 = arbgc2
306900160127     c                   endif
307000160127     c
307100140716if  1c                   IF        f_tasgc1 <> *blanks OR
307200140716     c                             f_tasgc2 <> *blanks
307300140716if  2c                   IF        f_tasgc1 <> *blanks
307400140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
307500000000     c                   EXSR      decgcx
307600000000     c                   MOVEL     wdgcx         d1wdgcx
307700000000e   2c                   ENDIF
307800140716if  2c                   IF        f_tasgc2 <> *blanks
307900140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
308000000000     c                   EXSR      decgcx
308100000000     c                   MOVEL     wdgcx         d2wdgcx
308200000000e   2c                   ENDIF
308300000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
308400000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
308500060612     c                   EVAL      we = ' ' +
308600060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
308700000000x   2c                   ELSE
308800000000     c                   EVAL      we = *blanks
308900000000e   2c                   ENDIF
309000060612     c                   EVAL      mgcxe =
309100060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
309200060612     c                             %TRIM(d1wdgcx)       +
309300060612     c                             we                   +
309400060612     c                             %TRIM(d2wdgcx)
309500000000     c*
309600010927if  2c                   IF        i < 10                                       *se c'è spazio
309700000000     c                   ADD       1             i                              *memorizza nota
309800000000     c                   EVAL      smnot(i) = mgcxe
309900010927e   2c                   ENDIF
310000010927e   1c                   ENDIF
310100000000     c*
310200000000     c* Fermo deposito
310300160127     c                   if        esito='P'
310400160309     c                   eval      f_tasffd = arbffd
310500160127     c                   endif
310600160127
310700140716if  1c                   IF        f_tasffd = 'S'
310800060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
310900000000     c*
311000010927if  2c                   IF        i < 10                                       *se c'è spazio
311100010927     c                   ADD       1             i                              *memorizza nota
311200010927     c                   EVAL      smnot(i) = mffd
311300010927e   2c                   ENDIF
311400010927e   1c                   ENDIF
311500030206     c*---
311600030206     c* memorizza le "note" che NON sono presenti nel record bolla
311700030206     c* ma sul file estensione "anagrafiche"
311800030206     c*---
311900030206     c* N° telefono destinatario
312000160127     c                   if        esito='P'
312100160309     C                   EVAL      MitOrigO74 = arbrmo
312200160127     c                   else
312300140716     c                   Z-ADD     f_tasaas      kaaaas
312400140716     c                   Z-ADD     f_taslnp      kaalnp
312500140716     c                   Z-ADD     f_tasnrs      kaanrs
312600140716     c                   Z-ADD     f_tasnsp      kaansp
312700030206     c                   MOVEL     'O'           kaatrc
312800030206     c     keytaa        CHAIN     titaa30c                           98
312900030206if  1c                   IF        NOT *in98
313000040330     C                   EVAL      MitOrigO74 = TAARSC
313100030206e   1c                   ENDIF
313200160127e   1c                   ENDIF
313300170310     c*
313400170310     c* se ho già usato il mitt originale come RSM --> pulisco il campo
313500170310     c                   if        wusarmo='S'
313600170310     c                   clear                   MitOrigO74
313700170310     c                   endif
313800030206     c*---
313900030206     c* memorizza le "note" che NON sono presenti nel record bolla
314000030206     c* ma sul file estensione testata boolle
314100030206     c*---
314200030206     c* Bancali
314300140716     c                   Z-ADD     f_tasaas      kr5aas
314400140716     c                   Z-ADD     f_taslnp      kr5lnp
314500140716     c                   Z-ADD     f_tasnrs      kr5nrs
314600140716     c                   Z-ADD     f_tasnsp      kr5nsp
314700030206     c                   MOVEL     'BAN'         kr5trd
314800160127     c                   if        esito='P'
314900160127     c     keyar5        CHAIN     fiar501l                           98
315000160127     c                   else
315100040531     c     keyar5        CHAIN     fiar531c                           98
315200160127     c                   endif
315300160127     c
315400030206if  1c                   IF        NOT *in98
315500030206     c                   MOVEL     ar5uni        dar5ban
315600060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
315700060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
315800030206if  2c                   IF        §ar5rb1 > *zeros OR
315900030206     c                             §ar5rb2 > *zeros
316000060613     c                   EVAL      mban = %trim(mban) +
316100060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
316200060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
316300030206e   2c                   ENDIF
316400030206     c*
316500030206if  2c                   IF        i < 10                                       *se c'è spazio
316600030206     c                   ADD       1             i                              *memorizza nota
316700030206     c                   EVAL      smnot(i) = mban
316800030206e   2c                   ENDIF
316900030206e   1c                   ENDIF
317000040330
317100040330     **?Reperisco referente
317200030206     c* Note supermercati
317300140716if  1c     f_tasftc      IFEQ      'S'
317400140716     c     f_tastc2      OREQ      'S'
317500030210     c                   CLEAR                   s                              *indice
317600140716     c                   Z-ADD     f_tasaas      kr5aas
317700140716     c                   Z-ADD     f_taslnp      kr5lnp
317800140716     c                   Z-ADD     f_tasnrs      kr5nrs
317900140716     c                   Z-ADD     f_tasnsp      kr5nsp
318000030206     c                   MOVEL     'SCR'         kr5trd
318100160127     c                   if        esito='P'
318200160127     c     keyar5        SETLL     fiar501l
318300160127     c     keyar5        READE     fiar501l                               98
318400160127     c                   else
318500040531     c     keyar5        SETLL     fiar531c
318600040531     c     keyar5        READE     fiar531c                               98
318700160127     c                   endif
318800160127     c
318900030210do  2c                   DOW       NOT *in98
319000030206     c                   MOVEL     ar5uni        dar5scr
319100030207     c* data nota
319200030210if  3c                   IF        ar5dac > *zeros
319300030207     c                   MOVEL     ar5dac        wdat
319400030207     c                   EXSR      edtdat                                       *edita data
319500030210     c                   MOVEL     wdate         msdace
319600030210e   3c                   ENDIF
319700030207     c* ora nota
319800030210if  3c                   IF        ar5orc > *zeros
319900030207     c                   MOVEL     ar5orc        wora
320000030207     c                   EXSR      edtora                                       *edita ora
320100030210     c                   MOVEL     worae         msorce
320200030210e   3c                   ENDIF
320300030207     c* Consegna richiesta
320400140708if  3c                   IF        §ar5dcr = *zeros AND
320500140708     c                             §ar5hcr = *zeros
320600140708x   3c                   ELSE
320700140708if  4c                   IF        §ar5dcr > *zeros
320800140708     c                   MOVEL     §ar5dcr       wdat
320900140708     c                   EXSR      edtdat                                       *edita data
321000140708     c                   MOVEL     wdate         msdcre
321100140708e   4c                   ENDIF
321200140708if  4c                   IF        §ar5hcr > *zeros
321300140708     c                   MOVEL     §ar5hcr       wora
321400140708     c                   EXSR      edtora                                       *edita ora
321500140708     c                   MOVEL     worae         mshcre
321600140708e   4c                   ENDIF
321700140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
321800140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
321900140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
322000140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
322100140708if  5c                   IF        §ar5tcr = 'P'                                *prima
322200140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
322300140708e   5c                   ENDIF
322400140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
322500140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
322600140708e   5c                   ENDIF
322700140708if  5c                   IF        §ar5tcr = ' '                                *il
322800140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
322900140708e   5c                   ENDIF
323000140708e   4c                   ENDIF
323100140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
323200140708     c                             §ar5dcr = *zeros                             *NO data richiesta
323300140708if  5c                   IF        §ar5tcr = 'P'                                *prima
323400140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
323500140708e   5c                   ENDIF
323600140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
323700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
323800140708e   5c                   ENDIF
323900140708if  5c                   IF        §ar5tcr = ' '                                *il
324000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
324100140708e   5c                   ENDIF
324200140708e   4c                   ENDIF
324300030210e   3c                   ENDIF
324400030210     c* Riferimento
324500030210if  3c                   IF        §ar5nom <>  *blanks
324600060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
324700060613     c                             + ' ' + §ar5nom
324800030210e   3c                   ENDIF
324900030210     c*
325000030210if  3c                   IF        s < 5                                        *MAX 5 note
325100030210     c                   ADD       1             s
325200030210     c                   EVAL      mscrT(s) = 'Il ' +
325300030210     c                             %trim(msdace) +                              *data nota
325400030210     c                             ' '    +
325500030210     c                             %trim(msorce) +                              *ora nota
325600030210     c                             ' '    +
325700140708     c                             %trim(mstcre) +                              *tipo consegna
325800140708     c                             ' '    +
325900140708     c                             %trim(msdcre) +                              *data consegna
326000140708     c                             ' '    +
326100140708     c                             %trim(mshcre) +                              *ora consegna
326200140708     c                             ' '    +
326300030210     c                             %trim(msnom)  +                              *parlato con
326400030210     c                             ' '    +
326500030210     c                             %trim(§ar5tel) +                             *telefono
326600030210     c                             ' '    +
326700030210     c                             %trim(§ar5mot)                               *motivo
326800030210e   2c                   ENDIF
326900030210     c*
327000160127     c                   if        esito='P'
327100160127     c     keyar5        READE     fiar501l                               98
327200160127     c                   else
327300160127     c     keyar5        READE     fiar531c                               98
327400160127e   1c                   ENDIF
327500030206e   2c                   ENDDO
327600030210e   1c                   ENDIF
327700030210     c* NOTA completa
327800030210if  1c                   IF        mscrT(1) <> *blanks
327900030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
328000030211     c                                      ' ' +
328100030211     C                                      %trim(mscrT(2)) +
328200030211     c                                      ' ' +
328300030211     C                                      %trim(mscrT(3)) +
328400030211     c                                      ' ' +
328500030211     C                                      %trim(mscrT(4)) +
328600030211     c                                      ' ' +
328700030211     C                                      %trim(mscrT(5))
328800030210     c                   Z-ADD     *zeros        s
328900030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
329000030210     c                   ADD       1             i
329100030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
329200030210     c                   EVAL      s = s + 100
329300030210e   2c                   ENDDO
329400030210     c*
329500030210e   1c                   ENDIF
329600010927     c*---
329700010927     c* memorizza le "note" che NON sono presenti nel record bolla
329800160127     c* ma sul file estensione
329900010927     c*---
330000160127     c
330100160127    0c                   if        esito<>'P'
330200010927     c                   CLEAR                   tita4000
330300010927     c                   CLEAR                   tita4p00
330400140716     c                   Z-ADD     f_tasaas      ka4aas
330500140716     c                   Z-ADD     f_taslnp      ka4lnp
330600140716     c                   Z-ADD     f_tasnrs      ka4nrs
330700140716     c                   Z-ADD     f_tasnsp      ka4nsp
330800160127     c
330900010927     c     ke2ta4        SETLL     tita430c
331000010927     c     ke2ta4        READE     tita430c                               98
331100010927do  1c                   DOW       NOT *in98
331200140708     c                   if        ta4trc = '8' OR ta4trc = '9'
331300050701     c*
331400050701if  3c                   IF        ta4trc = '8'
331500050701     c                   EVAL      mmnc8 = ta4not
331600050701e   3c                   ENDIF
331700050701if  3c                   IF        ta4trc = '9'
331800050701     c                   EVAL      mmnc9 = ta4not
331900050701e   3c                   ENDIF
332000050701     c*
332100010927e   2c                   ENDIF
332200010925     c*
332300010926     c     ke2ta4        READE     tita430c                               98
332400010926e   1c                   ENDDO
332500050701     C*
332600140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
332700050701     C*
332800050701     C                   IF        mmnc8 <> *BLANK
332900050701     C                   EVAL      a230 = %TRIML(mmnc8)
333000050701     C                   ENDIF
333100050701     C                   IF        mmnc9 <> *BLANK
333200050701     C                   IF        a230 <> *BLANK
333300050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
333400050701     C                   ELSE
333500050701     C                   EVAL      a230 = %TRIML(mmnc9)
333600050701     C                   ENDIF
333700050701     C                   ENDIF
333800010927     C*
333900050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
334000050701     C*
334100010927if  2c                   IF        i < 10                                       *se c'è spazio
334200010927     c                   ADD       1             i                              *memorizza nota
334300010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
334400010927e   2c                   ENDIF
334500010927     C*
334600010927if  2c                   IF        i < 10                                       *se c'è spazio
334700010927if  3c                   IF        n3 > 100
334800010927     c                   ADD       1             i                              *memorizza nota
334900010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
335000010927e   3c                   ENDIF
335100010927e   2c                   ENDIF
335200010927     C*
335300010927if  2c                   IF        i < 10                                       *se c'è spazio
335400010927if  3c                   IF        n3 > 200
335500010927     c                   ADD       1             i                              *memorizza nota
335600010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
335700010927e   3c                   ENDIF
335800010927e   2C                   ENDIF
335900010927e   1c                   ENDIF
336000160127e   0c                   ENDIF
336100010925     c*
336200000000     c* imposta contatore note da visualizzare nella DS di Output
336300000000     c                   EVAL      cntnoto74 = i
336400000000     c*
336500000000     c* imposta le note nella DS di Output
336600000000do  1c     1             DO        10            j
336700000000     c                   EVAL      nota(j) = smnot(j)
336800000000e   1c                   ENDDO
336900000000     c*
337000000000     c                   ENDSR
337100010716     c*--------------------------------------------------------------------------------------------*
337200010716     c* imposta i campi della DS di Output dalla firma DPD
337300010716     c*--------------------------------------------------------------------------------------------*
337400010716     c     impdsoDPD     BEGSR
337500010717     c*
337600010717     c* reimposta variabili di lavoro
337700010717     c                   RESET                   $AbFirma
337800010717     c                   CLEAR                   depute                         *deposito utente
337900010717     c                   CLEAR                   deppwd                         *deposito password
338000010717     c*---
338100010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
338200010717     c*---
338300010717     c                   Z-ADD     s_taslna      worg
338400010717     c                   EXSR      decorg
338500160406     c
338600020702     c                   IF        wodpd = 'DPD'
338700020702     c                   MOVEL     'S'           dpdo74
338800020702     c                   ELSE
338900020702     c                   MOVEL     *blanks       dpdo74
339000160406     c
339100160406     c* se si tratta di bolla EEX verifico se il partner è abilitato al link
339200160407     c                   IF        wodpd = 'EEX' and ksci74<>*blanks
339300160406     c                   clear                   fidn40ds
339400160406     c                   eval      i40fil=s_taslna
339500160406     c                   z-add     s_tasmgs      i40dsp
339600160406     c                   MOVEL     s_tasaas      i40dsp                          *data spedizione
339700160406     c                   call      'FIDN40R'
339800160406     c                   parm                    fidn40ds
339900160407
340000160406     c                   if        o40lnk='S'
340100160407     c* sommo i giorni alla data spedizione per sapere se ancora visibile
340200160407     c                   Clear                   xgiolavds
340300160407     c                   Eval      Ixglpa  = 'A'
340400160413     c                   Eval      IxglFil = s_taslna
340500160407     c                   Eval      Ixgldata = i40dsp
340600160407     c                   Eval      Ixgladd = 'S'
340700160407     c                   eval      ixglgg  = o40gio
340800160407     c                   Call      'XGIOLAV'
340900160407     c                   parm                    xgiolavds
341000160407     c                   if        oxgldata>=datcor
341100160406     c                   MOVEL     'E'           dpdo74
341200160407     c                   endif
341300160407     c
341400160406     c                   endif
341500160406     c                   endif
341600160406     c
341700020702     c                   ENDIF
341800010717     c*
341900010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
342000060627     c* Riferimento link Lettera di vettura
342100060627     c* Riferimento link Firma
342200010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
342300010717     c                   CLEAR                   tita4000
342400010717     c                   CLEAR                   tita4p00
342500060627     c                   CLEAR                   dsbl4i
342600010717     c                   Z-ADD     s_tasaas      ka4aas
342700010717     c                   Z-ADD     s_taslnp      ka4lnp
342800010717     c                   Z-ADD     s_tasnrs      ka4nrs
342900010717     c                   Z-ADD     s_tasnsp      ka4nsp
343000060627     c                   MOVEL     'I'           ka4trc
343100060627     c     keyta4        CHAIN     tita430c                           98
343200010717if  2c                   IF        NOT *in98
343300060627     c                   MOVEL     ta4not        dsbl4i
343400060627     c                   EVAL      DPDRmnO74 = §b4ipn
343500060627     c                   EVAL      DPDFnpO74 = §b4ipn
343600010717e   2c                   ENDIF
343700010717     c*
343800010717     c* cerca abilitazione firma DPD per questo segnacollo
343900010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
344000010717     c                   EXSR      RepAbilFirma
344100010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
344200010717     c*---
344300010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
344400010717     c*---
344500010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
344600010717     c* dati
344700010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
344800010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
344900010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
345000010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
345100010717     c*
345200010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
345300010717     c                   Z-ADD     s_tasaas      kvbaas
345400010717     c                   Z-ADD     s_taslnp      kvblnp
345500010717     c                   Z-ADD     s_tasnrs      kvbnrs
345600010717     c                   Z-ADD     s_tasnsp      kvbnsp
345700010717     c                   Z-ADD     99999999      kvbdev
345800010717     c                   Z-ADD     9999          kvbhev
345900010717     c     ke2evb        SETGT     fnevb01l
346000010717     c     keyevb        READPE    fnevb01l                               98
346100010717do  4c                   DOW       NOT *in98
346200101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
346300010720     c*
346400060629     c                   EVAL      devB01 = evbNot
346500060628     c                   IF        §NotDep = *BLANK
346600060628     c                   RESET                   DPDFirO74
346700060629     c                   LEAVE
346800060628     c                   ENDIF
346900060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
347000060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
347100080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
347200010720     c                   Z-ADD     *zeros        g08dat
347300010720     c                   MOVEL     '3'           g08err
347400010720     c                   CALL      'XSRDA8'
347500010720     c                   PARM                    wlbda8
347600010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
347700010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
347800010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
347900010720     c*
348000010717     c                   LEAVE                                                  *uscita ciclo
348100010717e   5c                   ENDIF
348200010717     c     keyevb        READPE    fnevb01l                               98
348300010717e   4c                   ENDDO
348400010717     c*
348500010717e   3c                   ENDIF
348600010717e   2c                   ENDIF
348700160323     c
348800160323   x1c                   else
348900010716e   1c                   ENDIF
349000010716     c*
349100010716     c                   ENDSR
349200160309     c*--------------------------------------------------------------------------------------------*
349300160309     c* impostazioni solo da FNBLP
349400160309     c*--------------------------------------------------------------------------------------------*
349500160309     c     impds_soloBLP BEGSR
349600160321     c
349700160321     c                   EVAL      dstaslnp = arblnp
349800160321     c                   EVAL      dstasnrs = arbnrs
349900160321     c                   EVAL      dstasnsp = arbnsp
350000160321     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
350100161117
350200161117     c                   MOVEL     arbaas        aasmgsO74                       *data spedizione
350300161117     c                   MOVE      arbmgs        aasmgsO74
350400161117     c
350500161117     c                   MOVEL     'N'           gco74                          *flag di controllo
350600161117     c                   MOVEL     'N'           gcfdio74
350700161117     c                   MOVEL     'N'           gcmito74
350800161117     c*
350900160314     C
351000160314     c                   Z-ADD     arblna        worg
351100160314     c                   exsr      setLnaItaEst
351200160314     C
351300160309     c* chain ar5 con bolla richiesta a video
351400160309     c                   Z-ADD     kasaas        kr5aas
351500160309     c                   Z-ADD     kaslnp        kr5lnp
351600160309     c                   Z-ADD     kasnrs        kr5nrs
351700160309     c                   Z-ADD     kasnsp        kr5nsp
351800160309     c                   MOVEL     'GEN'         kr5trd
351900160309     c     keyar5        CHAIN     fiar501l
352000160309     C                   IF        %FOUND
352100160309     C                   EVAL      DAR5GEN = AR5Uni
352200160309     C                   ELSE
352300160309     C                   RESET                   DAR5GEN
352400160309     C                   ENDIF
352500160309     C                   EVAL      REFCONSO74 = GEN§AR5REF
352600160309     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
352700160309     c*
352800160309     c* chain tipo bolla
352900160321     c                   eval      tblkey=arbcbo
353000160314     C                   EVAL      ds3A = chainTabel00f('CHAIN3':langTabel
353100160321     C                             :'3A':tblkey:%LEN(tblkey):'TBLUNI':
353200160314     C                             rpyOpCode:rpyEsito)
353300160309     c*
353400160309     c                   MOVEL     §3atb1        a1
353500160309     c                   MOVEL     §3atb1        tpporto74
353600160309if  1c                   IF        a1 = 'A'                                     *Pagamento del
353700160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
353800160309x   1c                   ELSE                                                   *Pagamento del
353900160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
354000160309e   1c                   ENDIF
354100160314     c
354200160314     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
354300160314     C                   IF        KSCI74 <> *BLANK
354400160314     c                   SETOFF                                           98
354500160314     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
354600160314     C                   RESET                   tis7700i0
354700160314     C                   EVAL      tis7700i0.aas = arbAas
354800160314     C                   EVAL      tis7700i0.lnp = arbLnp
354900160314     C                   EVAL      tis7700i0.nrs = arbNrs
355000160314     C                   EVAL      tis7700i0.nsp = arbNsp
355100160314     C                   EVAL      tis7700i0.tbl = §3atb1
355200160314     C                   EVAL      tis7700i0.ksu = kscI74
355300160314     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
355400160314     C***                CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
355500160314     C***                          : rpyEsito
355600160314     C***                          : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
355700160314     C***                          : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
355800160314     C***                          )
355900160314     C***                EVAL      *IN98 = (rpyEsito >= *ZERO AND
356000160314     C***                          tis7700o0.bollValida = *ON)
356100160314     **?Se non gli appartiene, prima di restituire errore controllo il check code.
356200160314if  2c                   IF        NOT *in98                                      *non trovato
356300160314     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
356400160314     C                   reset                   TIS778DSO
356500160818     C*****              EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
356600160818     c                   eval      esito='1'
356700160818     c                   eval      widmsg = 'TIS0849'
356800160818     c                   eval      widcampo = 'NSPEDIZI74'
356900160818     c                   clear                   wparm
357000160818     c                   exsr      messaggi
357100160818     c                   exsr      routend
357200160818     C******             RETURN
357300160314     C                   ENDIF
357400160314e   2c                   ENDIF
357500160314     C                   ENDIF
357600160314
357700160314     c
357800160309     c                   ENDSR
357900160314     c*--------------------------------------------------------------------------------------------*
358000160314     c* impostazioni del C/Assegno
358100160314     c*--------------------------------------------------------------------------------------------*
358200160314     c     ImpdsoAR9     BEGSR
358300160314     c                   Z-ADD     waas          ksbaas
358400160314     c                   Z-ADD     wlnp          ksblnp
358500160314     c                   Z-ADD     wnrs          ksbnrs
358600160314     c                   Z-ADD     wnsp          ksbnsp
358700160314     c     keycsb        CHAIN     fiar901l                           99
358800160314if  2c                   IF        NOT *in99                                    *esistente
358900160314     c* memorizza i dati
359000160314     c                   MOVEL     'S'           cao74                          *esiste C/A
359100160314     c*
359200160314     c                   Z-ADD     ar9cas        caimpo74                       *importo
359300160314     c*
359400160314     c                   EVAL      cadivo74 =  ar9vca                           *divisa
359500160314if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
359600160314     c                             caimpo74 > *zeros
359700160314     c                   EVAL      cadivo74 = 'ITL'
359800160314e   3c                   ENDIF
359900160314
360000160314     ** Visualizzo la descrizione del tipo incasso
360100160314     c**  devo impostare alcuni campi di CSB
360200160314     c                   eval      csbaas=ar9aas
360300160314     c                   eval      csblnp=ar9lnp
360400160314     c                   eval      csbnrs=ar9nrs
360500160314     c                   eval      csbnsp=ar9nsp
360600160314     c                   clear                   csbddc
360700160314     c                   clear                   csbtpa
360800160314     c                   clear                   csbtpi
360900160314     c
361000160314     C                   EVAL      cainco74 =
361100160314     C                                       GetDescrizioneIncassoContrassegno()
361200160314     c* stato C/Assegno : attivo
361300160314     c                   MOVEL     *blanks       castao74                       *stato
361400160314     C                   EVALR     tblKey = '0'
361500160314     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
361600160314     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
361700160314     C                             :rpyOpCode:rpyEsito)
361800160314     C                   IF        rpyOpCode = 'FOUND'
361900160314     c                   EVAL      castao74 = §4SDei
362000160314e   3c                   ENDIF
362100160314
362200160314if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
362300160314     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)     *NO distinta incasso
362400160314     c                   endif
362500160314
362600160314     c                   endif
362700160314     c
362800160314     c                   ENDSR
362900140604     c*--------------------------------------------------------------------------------------------*
363000140604     c* imposta i campi da FNARB se TASDCM = 0
363100140604     c*--------------------------------------------------------------------------------------------*
363200140604     c     impdsoARB     BEGSR
363300150403     c                   clear                   orarich           4 0
363400150611     c                   clear                   wdee
363500170301     c                   clear                   mito74
363600150410     c* memorizzo se utilizzo la data prevista consegna
363700150410     c                   clear                   usaprev           1
363800170301     c                   eval      wbol='ARB'
363900140604     c*
364000140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
364100140604     c                   EVAL      lnaDescO74 = sodes(i)
364200140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
364300140604     c*
364400150116      *eliminata forzatura resta impostato dalla mamma per composizione link
364500150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
364600150116     c*                  MOVE      arbmgs        aasmgsO74
364700140818      *riempimento da verificare
364800140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
364900140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
365000140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
365100140818
365200140818     c                   if        tpporto74 = 'A'
365300140818     c                   if        arbccm > 0
365400140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
365500140818     c                   else
365600140818     c                   movel     arblnp        ccmO74
365700140818     c                   move      8888          ccmO74
365800140818     c                   endif
365900140818     c                   else
366000140818     c                   z-add     arbksc        ccmO74
366100140818     c                   endif
366200140818      *
366300140604     c                   MOVEL     arblna        lineaaro74
366400140604     c                   MOVEL     arbtsp        tpservio74
366500140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
366600140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
366700140604     C                             rpyOpCode:rpyEsito)
366800140604     C                   EVAL      corro74 = §5EDes
366900140604     c*
367000140604     c                   EVAL      destino74 = arbrsd
367100140604     c                   EVAL      desindo74 = arbind
367200140604     c                   EVAL      descapo74 = arbcad
367300140604     c                   EVAL      desloco74 = arblod
367400140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
367500140604if  1c                   IF        arbnzd    = *blanks
367600140604     c                   EVAL      desnazo74 = arbprd
367700140604x   1c                   ELSE
367800150326     c**                 EVAL      desnazo74 = arbnzd
367900150326     c**                 EVAL      desnzdo74 = arbnzd
368000150326     c* 26/03/15: Attiviamo le specifiche per provincia export
368100150326     c                   EVAL      desnazo74 = arbprd
368200150326     c                   EVAL      desnzdo74 = arbnzd
368300150326e   1c                   ENDIF
368400140604     c*
368500140604     c                   EVAL      collio74  = arbncl
368600140604     c                   EVAL      pesoo74   = arbpkf
368700140604     c                   EVAL      volumeo74 = arbvlf
368800140604     c                   EVAL      naturao74 = arbnas
368900140604     c*
369000140604     c                   EVAL      disnumo74  = arbndc
369100140604c    c                   MOVEL     arbddc        wdat
369200140604     c                   EXSR      edtdat
369300140604     c                   EVAL      disdatao74 = wdate
369400140604     c*
369500170310     c                   MOVE      ccmo74        alfa4
369600170310if  1c                   IF        alfa4 <> '8888'                                *codificato
369700170310     c                   clear                   mito74
369800170310     c                   clear                   solomito74
369900170301     c                   exsr      tabcli
370000170310     c                   else
370100140604     c                   EVAL      mito74    = arbrsm
370200170310     c                   endif
370300170310
370400140604     c                   EVAL      mitindo74 = arbinm
370500140604     c                   EVAL      mitcapo74 = arbcam
370600140604     c                   EVAL      mitloco74 = arblom
370700140604      *specifiche da eliminare dopo la modifica in immissione bolle
370800140604if  3c                   IF        arbnzm = *blanks
370900140604     c                   EVAL      mitnazo74 = arbprm
371000140604x   3c                   ELSE
371100140604     c                   EVAL      mitnazo74 = arbnzm
371200140604e   3c                   ENDIF
371300140604     c* 2a ragione sociale destinatario.
371400140604     c                   Z-ADD     arbaas        ka4aas
371500140604     c                   Z-ADD     arblnp        ka4lnp
371600140604     c                   Z-ADD     arbnrs        ka4nrs
371700140604     c                   Z-ADD     arbnsp        ka4nsp
371800140604     c                   MOVEL     'D'           ka4trc
371900140604     c     keyta4        CHAIN     fiar401l                           99
372000140604if  1c                   IF        NOT *in99
372100140604     c                   MOVEL     ar4not        desti2O74
372200140604e   1c                   ENDIF
372300140604     c                   MOVEL     'E'           ka4trc
372400140604     c     keyta4        CHAIN     fiar401l                           99
372500140604if  1c                   IF        NOT *in99
372600140604     c                   MOVEL     ar4not        dsbl4e
372700140604e   1c                   ENDIF
372800140604     c
372900140604     ** Riferimento mittente numerico (c'è sempre).
373000140604     C                   EVAL      rifero74 = arbrmn
373100140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
373200140604     C                   IF        arbrma <> *blank and
373300140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
373400140604     C                   EVAL      rifero74a = arbrma
373500140604     C                   ENDIF
373600140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
373700140604     C                   IF        §b4erp <> *BLANK AND
373800140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
373900140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
374000140604     C                   EVAL      rpto74 = §b4erp
374100140604     C                   ENDIF
374200140604
374300140604     **?Reperisco il check code della spedizione.
374400140604     C                   EVAL      SChkCdeO74 =
374500140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
374600140604     C                             NSpedizI74:ChkCode:nEsito)
374700140604
374800140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
374900141126     C*m                 IF        KSCI74 <> *BLANK
375000141126     c*m                 SETOFF                                           98
375100140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
375200141126     C*m                 RESET                   tis7700i0
375300141126     C*m                 EVAL      tis7700i0.aas = arbAas
375400141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
375500141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
375600141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
375700141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
375800141126     C*m                 EVAL      tis7700i0.ksu = kscI74
375900141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
376000141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
376100141126     C*m                           : rpyEsito
376200141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
376300141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
376400141126     C*m                           )
376500141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
376600141126     C*m                           tis7700o0.bollValida = *ON)
376700140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
376800141126if  2c*m                 IF        NOT *in98                                      *non trovato
376900141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
377000141126     C*m                 reset                   TIS778DSO
377100141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
377200141126     C*m                 RETURN
377300141126     C*m                 ENDIF
377400141126e   2c*m                 ENDIF
377500141126     C*m                 ENDIF
377600140604     c*
377700140604     ** Importo assicurato.
377800140604     C                   IF        arbIas > 0
377900140604     C                   EVAL      vasO74 = arbVas
378000140604     C                   EVAL      iasO74 = arbIas
378100140604     C                   ENDIF
378200140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
378300140605     c                   if        arbdcm = 0 and wcev <> '704'
378400140617     c                   if        arbdcr > 0
378500140605     c                   MOVEL     arbdcr        wdat
378600140605     c                   EXSR      edtdat
378700140605     c                   eval      DTCONRCO74  =  wdate
378800140819if  3c                   IF        arbtcr = ' '
378900140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
379000140819e   3c                   ENDIF
379100140617     c                   endif
379200140617     c                   if        arbhcr > 0
379300140617     c                   MOVEL     arbhcr        wora
379400140617     c                   EXSR      edtora
379500140617     c                   eval      ORACONRO74  =  worae
379600140617     c                   endif
379700140606     c                   eval      TPDATCRO74  =  arbtcr
379800160407     c
379900140605      *reperimento descrizione tipo consegna richiesta
380000140617if  3c                   IF        arbtcr = 'P'
380100140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
380200140617e   3c                   ENDIF
380300140617if  3c                   IF        arbtcr = 'D'
380400140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
380500140617e   3c                   ENDIF
380600160407     c
380700160407     c
380800160407     c* Per  le bolle in partenza eseguo solo se presente la data entrata
380900160407    0c                   if        esito<>'P' or arbdse>0
381000160407     c
381100140605     C                   clear                   tnsd99ds
381200140605     C                   MOVEL     ' '           D98tla
381300160314     c                   if        esito='P'
381400160314     C                   MOVEL     'P'           D98tbo
381500160314     c                   else
381600140605     C                   MOVEL     'A'           D98tbo
381700160314     c                   endif
381800140605     C                   Z-ADD     arbaas        D98aas
381900140605     C                   Z-ADD     arblnp        D98lnp
382000140605     C                   Z-ADD     arbnrs        D98nrs
382100140605     C                   Z-ADD     arbnsp        D98nsp
382200140605     C*
382300140605     C                   call      'TNSD99R'
382400140605     C                   parm                    tnsd99ds
382500150304     c
382600140605     C*
382700140917      * chiamata a trulors per tempi standard
382800140917     c                   clear                   trulorsds
382900140917     c                   clear                   trulor2ds
383000140917     c                   clear                   trulor3ds
383100150611     c                   eval      wdee=d98dee
383200140917      * solo se questa data è valorizzata
383300150304    1c                   if        d98dee > 0
383400140912     c                   movel     d98dee        wdat
383500140605     c                   exsr      edtdat
383600140605     c                   eval      DTTEOCOO74  =  wdate
383700140819     c
383800140904      * se fermo deposito non richiamo pgm per reperimento orari
383900150304    2c                   if         arbffd = *blank
384000140605     c                   eval       IOREtser = 'C'
384100140605     c                   eval       IOREDDC=arbddc
384200140605     c                   eval       IOREnDC=arbndc
384300140609     c                   eval       IOREfil=arblna
384400140609     c                   eval       IOREdta=d98dce
384500140609     c                   eval       IOREcap=arbcad
384600140609     c                   eval       IOREloc=arblod
384700151019     c*****              eval       IOREtfp=arbtfp
384800151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
384900151019     c* del terminal di partenza originale
385000151019     c                   if         d98flp>*zeros
385100151019     c                   eval       IOREtfp=%INT(d98flp)
385200151019     c                   else
385300151019     c                   eval       IOREtfp=arbtfp
385400151019     c                   endif
385500140609     c                   eval       IOREtfa=arbtfa
385600140610     c                   eval       IOREdti=arbdti
385700140610     c                   eval       IOREhti=arbhti
385800140610     c                   eval       IOREdcr=arbdcr
385900140610     c                   eval       IOREhcr=arbhcr
386000140610     c                   eval       IOREtcr=arbtcr
386100140819     c                   eval       IOREtsp=arbtsp
386200140819     c                   eval       IOREnar=arbnzd
386300140819     c                   eval       IOREdei=d98dei
386400140819     c                   eval       IOREoei=d98oei
386500150819     c                   clear                   diore01
386600150819     c                   movel     d98ttc        §ioretrazC
386700150819     c                   movel     d98tcc        §ioreconsC
386800150819     c                   eval       IOREflo=diore01
386900150325     c* passo la località normalizzata
387000150325     c                   clear                   tita4000
387100150325     c                   clear                   tita4p00
387200150325     c                   Z-ADD     arbaas        ka4aas
387300150325     c                   Z-ADD     arblnp        ka4lnp
387400150325     c                   Z-ADD     arbnrs        ka4nrs
387500150326     c                   Z-ADD     arbnsp        ka4nsp
387600150325     c                   MOVEL     'X'           ka4trc
387700160314     c                   if        esito='P'
387800160314     c     keyta4        CHAIN     fiar401l                           99
387900160314if  1c                   IF        NOT *in99
388000160314     c                   MOVEL     ar4not        OOR3LOC_N
388100160314     c                   else
388200160314     c                   movel     arblod        OOR3LOC_N
388300160314e   1c                   ENDIF
388400160314     c
388500160314     c                   else
388600160314     c     keyta4        CHAIN     tita430c                           99
388700150325if  1c                   IF        NOT *in99
388800150325     c                   MOVEL     ta4not        OOR3LOC_N
388900150325     c                   else
389000150325     c                   movel     arblod        OOR3LOC_N
389100150325e   1c                   ENDIF
389200160314e   1c                   ENDIF
389300140605     c
389400160321     c                   if        esito<>'P'
389500150304    3c                   IF         (ARBfbc = *blanks and
389600140605     c                               ARBddc > 0 and
389700140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
389800140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
389900140605     c                   eval       IOREcons = 'S'
390000150304    3c                   endif
390100160321     c
390200140605     c* Orario presunto ritiro/consegna
390300140605     c                   move      arbndc        pctndc
390400140605     c                   move      arbifp        pctfgs
390500140605     c                   move      'COR'         ttrd
390600140605     c     kcet          setgt     fipct02l
390700140605     c     kcet          readpe    fipct02l
390800150304    3c                   if        not %eof(fipct02l)
390900140605     c                   movel     pctdati       fipctcords
391000140605     c                   eval      ioreorp=§PCTORASTI
391100140605     c                   endif
391200160321    3c                   endif
391300160321
391400140605     c                   call      'TRULORSR'
391500140605     C                   PARM                    kpjba
391600140605     C                   PARM                    trulorsds
391700140605     C                   PARM                    trulor2ds
391800140605     C                   PARM                    trulor3ds
391900150304    3c                   endif
392000140818     c                   clear                   ORSTINIO74
392100150304    3c                   if        OOR2STIS > 0
392200140617     c                   MOVEL     OOR2STIS      wora
392300140617     c                   EXSR      edtora
392400140617     c                   eval      ORSTINIO74  =  worae
392500150304    3c                   endif
392600140818     c                   clear                   ORSTFINO74
392700150304    3c                   if        OOR2STFS > 0
392800140617     c                   MOVEL     OOR2STFS      wora
392900140617     c                   EXSR      edtora
393000140617     c                   eval      ORSTFINO74  =  worae
393100150304    3c                   endif
393200140818     c                   clear                   ORMIINIO74
393300150304    3c                   if        OOR2MIIS > 0
393400140617     c                   MOVEL     OOR2MIIS      wora
393500140617     c                   EXSR      edtora
393600140617     c                   eval      ORMIINIO74  =  worae
393700150304    3c                   endif
393800140818     c                   clear                   ORMAFINO74
393900150304    3c                   if        OOR2MXFS > 0
394000140617     c                   MOVEL     OOR2MXFS      wora
394100140617     c                   EXSR      edtora
394200140617     c                   eval      ORMAFINO74  =  worae
394300150304    3c                   endif
394400150304    3c                   if        wricons = 'S'
394500140818     c                   clear                   ORRIDALO74
394600150304    4c                   if        OOR2RIDS > 0
394700140617     c                   MOVEL     OOR2RIDS      wora
394800140617     c                   EXSR      edtora
394900140617     c                   eval      ORRIDALO74  =  worae
395000150304    4c                   endif
395100140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
395200140818     c                   clear                   ORRIALLO74
395300150304    4c                   if        OOR2RIAS > 0
395400140617     c                   MOVEL     OOR2RIAS      wora
395500140617     c                   EXSR      edtora
395600140617     c                   eval      ORRIALLO74  =  worae
395700150304    4c                   endif
395800140617     c                   eval      FLRICONO74  =  OOR2FRIC
395900150304   x3c                   else
396000140711     c                   clear                   ORRIALLO74
396100140711     c                   clear                   ORRIDALO74
396200140711     c                   eval      FLRICONO74  =  'N'
396300150304    3c                   endif
396400140818     c                   clear                   ORSTICDO74
396500150304    3c                   if        OOR2PRESD > 0
396600140617     c                   MOVEL     OOR2PRESD     wora
396700140617     c                   EXSR      edtora
396800140617     c                   eval      ORSTICDO74  =  worae
396900150304    3c                   endif
397000140818     c                   clear                   ORSTICAO74
397100150304    3c                   if        OOR2PRESa > 0
397200140617     c                   MOVEL     OOR2PRESa     wora
397300140617     c                   EXSR      edtora
397400140617     c                   eval      ORSTICAO74  =  worae
397500150304    3c                   endif
397600140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
397700140605     c                   eval      FLLOCNOO74  =  OOR3NORM
397800140904      *end F.D.
397900150304    2c                   endif
398000140605      * RIFERIMENTI DESTINATARIO
398100140605     c                   MOVEL     'EMD'         kr5trd
398200140605     c     keyAR5        CHAIN     fiar501l
398300150304if  2c                   IF        %found(fiar501l)
398400140605     c                   MOVEL     ar5uni        dar5emd
398500140606     c                   eval      EMLDESTO74  =  §§AR5EML
398600140606     c                   eval      CELDESTO74  =  §§AR5TEL
398700150304e   2c                   ENDIF
398800140604     **
398900150304     c*
399000150304     c* Stato data prevista consegna
399100150305     c                   clear                   stat_n2o74
399200150305     c                   eval      STATUSo74   =  D98SPCDEE
399300150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
399400150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
399500150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
399600150312     c                   eval      statuso74='RIC'
399700150403     c* se presente FIARP --> visualizzo la data richiesta
399800150403     c     karp          chain     fiarp01l
399900150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
400000150611     c                   movel     arpdcr        wdee
400100150611     c                   movel     arpdcr        wdat
400200150403     c                   exsr      edtdat
400300150403     c                   eval      DTTEOCOO74  =  wdate
400400150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
400500150421    3C                   IF        ARPHCR>0
400600150403     c                   eval      orarich=arphcr
400700150421    3c                   endif
400800150421   x2c                   else
400900150403     c*  se presente DOPO IL imposto la data in D98dee
401000150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
401100150403     c                   eval      D98dee=arpdcr
401200150611     c                   eval      wdee=arpdcr
401300150421    3c                   endif
401400150403     c* PRIMA DEL come se non ci fosse...
401500150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
401600150421    3c                   if        d98dee>0
401700150312     c                   Clear                   xgiolavds
401800150312     c                   Eval      Ixglpa  = 'A'
401900150312     c                   Eval      IxglFil = arblna
402000150312     c                   Eval      Ixgldata = d98dee
402100150312     c                   Eval      Ixgladd = 'S'
402200150312     c                   Eval      Ixgllav = 'S'
402300150312     c                   eval      ixglgg  = 1
402400150312     c                   Call      'XGIOLAV'
402500150312     c                   parm                    xgiolavds
402600150611     c                   movel     oxgldata      wdee
402700150611     c                   movel     oxgldata      wdat
402800150312     c                   exsr      edtdat
402900150312     c                   eval      DTTEOCOO74  =  wdate
403000150421    3c                   endif
403100150421    2c                   endif
403200150421    1c                   endif
403300150421     c
403400150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
403500150421     c                   eval      statuso74='ATT'
403600150421     c                   endif
403700150421    1c                   if        PDAgia='S' and statuso74='DDC'
403800150421     c                   eval      statuso74='GIA'
403900150421     c                   endif
404000170320     c* Se lo stato è : in distinta e ho FIARP, aggiungo una scritta
404100170320     c                   if        statuso74='DDC'
404200170320     c     karp          chain     fiarp01l
404300170320    2c                   if        %found(fiarp01l) and arpdcr>0 and
404400170320     c                             arpdcr>datcor  and
404500170320     c                             (arptcr=' ' or  arptcr='D')
404600170320     c                   eval      statuso74='DDP'
404700170320     c                   endif
404800170320     c                   endif
404900170320     c
405000150401     c
405100150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
405200150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
405300150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
405400150401     c* La data contatto = data distinta + 1
405500150512     c**                 Clear                   xgiolavds
405600150512     c**                 Eval      Ixglpa  = 'A'
405700150512     c**                 Eval      IxglFil = arblna
405800150512     c**                 Eval      Ixgldata = arbddc
405900150512     c**                 Eval      Ixgladd = 'S'
406000150512     c**                 Eval      Ixgllav = 'S'
406100150512     c**                 eval      ixglgg  = 1
406200150512     c**                 Call      'XGIOLAV'
406300150512     c**                 parm                    xgiolavds
406400150512     c**                 movel     oxgldata      wdat
406500150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
406600150512     c**                 endif
406700150317     c
406800150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
406900150326     c*  lo status diventa RIC
407000150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
407100150317     c                   eval      statuso74='RIC'
407200150317     c                   endif
407300150310     c
407400150304     c* decodifica stato da mettere nei campi deco e note
407500150304     c                   clear                   dspc
407600150304     C                   RESET                   tibs02ds
407700150304     C                   EVAL      t02Cod = 'SPC'
407800150312     C                   EVAL      t02Ke1 = statuso74
407900150306     C                   EXSR      getTntbe00f
408000150306     c                   IF        t02Err <>'E'
408100150306     c
408200150305     c                   eval       stat_d1o74=§SPCDEC1
408300150305     c                   eval       stat_d2o74=§SPCDEC2
408400150304     c*
408500150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
408600150305     c                   eval       stat_n1o74=§SPCNOT1
408700150306     c                   eval       stat_n2o74=§SPCNOT2
408800150304     c                   else
408900150305     c                   clear                   stat_d1o74
409000150305     c                   clear                   stat_d2o74
409100150305     c                   clear                   stat_n1o74
409200150306     c                   clear                   stat_n2o74
409300150304     c                   endif
409400160407
409500160407     c                   endif
409600160407
409700150409     c                   else
409800150409     c* stato evento CONSEGNATO
409900150409     c                   eval      statuso74='CON'
410000150415     c                   eval      flgbolcO74  =  'S'
410100150415     c                   eval      flgboldO74  =  'N'
410200150409    1c                   endif
410300150304     c
410400150304     c
410500140919     ** riempimento per campi da arb con data consegna diversa da 0
410600140919     c                   if        arbdcm > 0
410700140919     c                   movel     arbdcm        wdat
410800140919     c                   exsr      edtdat
410900140919     c                   eval      dtconmeo74  = wdate
411000140919     c                   MOVEL     arbdcm        wora
411100140919     c                   EXSR      edtora
411200140919     c                   eval      orconmeO74  =  worae
411300140919     c                   eval      flgbolcO74  =  'S'
411400140919     c                   eval      flgboldO74  =  'N'
411500141015      *firmatario distinta PDA
411600141015     c                   clear                   firmato74
411700141015     c                   if        gen§ar5pdaco <> 'NO'
411800141015     c                   exsr      repfirmatARB
411900141015     c                   else
412000141015     c                   if        arbgma <> *blank
412100141015     C                   EVAL      tblKey = arbgma
412200141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
412300141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
412400141015     C                             :rpyOpCode:rpyEsito)
412500141015     C                   IF        rpyOpCode = 'FOUND'
412600141015     c                   if        §7r2fi = 'S'
412700141015     c                   exsr      repfirmatARB
412800141015     c                   endif
412900141015     c                   endif
413000141015     c                   endif
413100141015     c                   endif
413200140919     c                   else
413300150415     c* solo se non impostato da PdA lo stato CONSEGNA
413400150415     c                   if        statuso74<>'CON'
413500150415     c
413600140919     c                   eval      flgbolcO74  =  'N'
413700140919     c                   if        arbndc > 0 and arbddc > 0
413800150415     c                             and arbddc <= datcor and d98cons='S'
413900140919     c                   eval      flgboldo74 = 'S'
414000140919     c                   else
414100140919     c                   eval      flgboldO74  =  'N'
414200140919     c                   endif
414300150415     c                   endif
414400150415
414500140919     c                   endif
414600150415     c
414700150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
414800140919     c                   eval      orincono74 = orsticdo74
414900140919     c                   eval      orficono74 = ORSticaO74
415000140919     c                   else
415100140919     c                   eval      orincono74 = ORSTINIO74
415200140919     c                   eval      orficono74 = ORSTFINO74
415300140919     c                   endif
415400150326     c* note 1
415500150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
415600150309     c                   clear                   wparm
415700150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
415800150326     c                   if        D98DTCTD<>*blanks
415900150326     c                   movel     D98DTCTD      wdat
416000150326     c                   exsr      edtdat
416100150326     c                   else
416200150326     c                   eval      wdate=dtteocoo74
416300150410     c                   eval      usaprev='S'
416400150326     c                   endif
416500150326     c
416600150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
416700150309     c                   eval      stat_n1o74= rtvMsgLang(
416800150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
416900150309     c                   endif
417000150309     c* note 2
417100150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
417200150309     c                   clear                   wparm
417300150403     c* Se lo stato è PRG passo orario limite uscita aut
417400150403     c                   select
417500170320     c                   when      statuso74='PRG'
417600150326     c                   IF        d98picklim  >*zeros
417700150309
417800150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
417900150326     c                                   (%subst(d98picklim:3:2))
418000150309     c                   else
418100150309     c                   eval      wparm='09:00'
418200150403     c* reperisco orario di fine picking
418300150403     c* filiale / data
418400150403     C                   RESET                   tibs02ds
418500150403     C                   EVAL      t02Cod = 'OLP'
418600150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
418700150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
418800150403     C                   EXSR      getTntbe00f
418900150403     c* filiale
419000150403     c                   IF        t02Err = 'E'
419100150403     C                   RESET                   tibs02ds
419200150403     C                   EVAL      t02Cod = 'OLP'
419300150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
419400150403     C                   EXSR      getTntbe00f
419500150403     c                   endif
419600150403     c* Generico 000
419700150403     c                   IF        t02Err = 'E'
419800150403     C                   RESET                   tibs02ds
419900150403     C                   EVAL      t02Cod = 'OLP'
420000150403     C                   EVAL      t02Ke1 = '000'
420100150403     C                   EXSR      getTntbe00f
420200150403     c                   endif
420300150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
420400150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
420500150403     c                             %subst(t02uni:3:2)
420600150309     c                   endif
420700150403     c                   endif
420800150403     c                   eval      stat_n2o74= rtvMsgLang(
420900150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
421000170320     c
421100170320     c* Se lo stato è DDP passo la data cons richiesta di fiarp
421200170320    2c                   when      statuso74='DDP'
421300170320     c                   movel     arpdcr        wdat
421400170320     c                   exsr      edtdat
421500170320                         eval      wparm=wdate ;
421600170320     c                   eval      stat_n2o74= rtvMsgLang(
421700170320     c                             %subst(stat_n2o74:1:7):langi74:wparm)
421800150403     c                   other
421900150403     c* altri casi
422000150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
422100150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
422200150611     c                             arbtcr=' ' and arbdcr>0  and
422300150611     c                             arbdcr>=wdee
422400150410     c
422500150403     c                   eval      orarich=arbhcr
422600150410     c                   endif
422700150403     c
422800150403     c                   if        orarich>0
422900150403     c                   exsr      sr_dallealle
423000150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
423100150403     c                   eval      stat_n2o74= rtvMsgLang(
423200150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
423300150403     c
423400150403     c                   else
423500150403     c                   clear                   stat_n2o74
423600150403     c                   endif
423700150403     c
423800150403     c                   endsl
423900150403     c
424000150309     c                   endif
424100150410     c
424200150424     c* Se si tratta di fermo deposito pulisco sempre
424300150424     c                   if        usaprev<>'S' or arbffd='S'
424400150410     c                   clear                   DTTEOCOO74
424500150410     c                   clear                   ORINCONO74
424600150410     c                   clear                   ORFICONO74
424700150410     c                   endif
424800150309     c
424900170301     c                   clear                   wbol              3
425000140604     c                   ENDSR
425100150403      /free
425200150403       //--------------------------------------------------------------
425300150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
425400150403       //--------------------------------------------------------------
425500150403       BEGSR  sr_dallealle;
425600150403       clear  w_dallemsg ;
425700150403       clear  w_allemsg ;
425800150403
425900150403       // Reperimento dati tabella VPO - ORARISER
426000150403       if §VPORST_MW=0  ;
426100150403       clear tibs02ds;
426200150403       clear dvpooraris;
426300150403       t02mod='C';
426400150403       t02cod='VPO';
426500150403       t02ke1='ORARISER';
426600150403       tibs02r(kpjba:tibs02ds);
426700150403       if t02err=*blanks;
426800150403          dvpooraris=t02uni;
426900150403       endif;
427000150403       endif;
427100150403
427200150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
427300150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
427400150403       //   determino orari servizio comprensivi della tolleranza
427500150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
427600150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
427700150403
427800150403            if w_dalle<w_dalleoser;
427900150403               w_dalle=w_dalleoser;
428000150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
428100150403            endif;
428200150403
428300150403            if w_alle>w_alleoser;
428400150403               w_alle=w_alleoser;
428500150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
428600150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
428700150403               endif;
428800150403            endif;
428900150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
429000150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
429100150403
429200150403       endsr;
429300150403      /end-free
429400010717     c*--------------------------------------------------------------------------------------------*
429500010717     c* imposta i campi della DS di Output dagli eventi
429600010717     c*--------------------------------------------------------------------------------------------*
429700010717     c     RepAbilFirma  BEGSR
429800010717     c*
429900010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
430000010717     c* e recupera utente / password
430100010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
430200010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
430300010717     c     keyvtd        SETLL     tivtd01l
430400010717     c     keyvtd        READE     tivtd01l                               98
430500010717do  2c                   DOW       NOT *in98
430600010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
430700010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
430800010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
430900010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
431000010717     c                   EVAL      depute = vtdute                              *utente
431100010717     c                   EVAL      deppwd = vtdpwd                              *password
431200010717     c                   LEAVE                                                  *esce dal ciclo
431300010717e   3c                   ENDIF
431400010717     c     keyvtd        READE     tivtd01l                               98
431500010717e   2c                   ENDDO
431600010717e   1C                   ENDIF
431700010717     c*
431800010717     c                   ENDSR
431900000000     c*--------------------------------------------------------------------------------------------*
432000000000     c* edita data da aaaammgg in gg/mm/aaaa
432100000000     c*--------------------------------------------------------------------------------------------*
432200000000     c     edtdat        BEGSR
432300000000     c*
432400000000     c                   MOVEL     *blanks       wdate
432500000000     c*
432600000000     c                   MOVEL     wdat          dsdat
432700000000     c                   MOVEL     dsgio         dsgioe
432800140528     c                   MOVEL     '.'           dsvd1
432900000000     c                   MOVEL     dsmes         dsmese
433000140528     c                   MOVEL     '.'           dsvd2
433100000000     c                   MOVEL     dsann         dsanne
433200000000     c                   MOVEL     dsdate        wdate
433300000000     c*
433400000000     c                   ENDSR
433500000000     c*--------------------------------------------------------------------------------------------*
433600000000     c* edita ora da hhmm in hh:mm
433700000000     c*--------------------------------------------------------------------------------------------*
433800000000     c     edtora        BEGSR
433900000000     c*
434000000000     c                   MOVEL     *blanks       worae
434100000000     c*
434200000000     c                   MOVEL     wora          dsora
434300000000     c                   MOVEL     dshh          dshhe
434400140617     c                   MOVEL     '.'           dsvo1
434500000000     c                   MOVEL     dsmm          dsmme
434600000000     c                   MOVEL     dsorae        worae
434700000000     c*
434800000000     c                   ENDSR
434900000000     c*--------------------------------------------------------------------------------------------*
435000000000     c* decodifica punto operativo
435100000000     c*--------------------------------------------------------------------------------------------*
435200000000     c     decorg        BEGSR
435300000000     c*
435400000000     c                   MOVEL     *blanks       wodes                          *descrizione
435500000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
435600000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
435700000000     c*
435800000000     c                   SETOFF                                           98
435900041111     c                   Z-ADD     1             O
436000041111     c     worg          LOOKUP    sorg(O)                                98
436100000000if  1c                   IF        *in98                                        *trovata
436200121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
436300121001     c                             %EDITC(wOrg : 'X') + ')'
436400041111     c                   MOVEL     sofl1(O)      wofl1
436500041111     c                   MOVEL     sodpd(O)      wodpd
436600000000e   1c                   ENDIF
436700000000     c*
436800000000     c                   ENDSR
436900000000     c*--------------------------------------------------------------------------------------------*
437000000000     c* decodifica evento per STATI
437100000000     c*--------------------------------------------------------------------------------------------*
437200000000     c     deccevsta     BEGSR
437300000000     c*
437400000000     c                   MOVEL     '0'           werr                           *NO errore
437500050704     c*
437600050704     C                   IF        wcev = 'GEN'
437700050704     C                   EVAL      werr = *ON
437800050704     C                   CLEAR                   wcdex
437900050704     C                   LEAVESR
438000050704     C                   ENDIF
438100000000     c*
438200060622     C                   RESET                   tibs02ds
438300060622     C                   EVAL      t02Cod = 'ICE'
438400060622     C                   EVAL      t02Ke1 = wcev
438500060622     C                   EXSR      getTntbe00f
438600060622     C                   IF        t02Err <> 'E' AND
438700140528     C                             (§icesta = 'S'  OR
438800140528     C                               §icestaP = 'S' )
438900060622     c                   MOVEL     §icedei       wcdex
439000000000x   1c                   ELSE                                                   *non trovato
439100000000     c                   MOVEL     '1'           werr                           *SI errore
439200030130     c                   MOVEL     *blanks       wcdex
439300000000e   1c                   ENDIF
439400000000     c*
439500000000     c                   ENDSR
439600000000     c*--------------------------------------------------------------------------------------------*
439700000000     c* decodifica consegna particolare
439800000000     c*--------------------------------------------------------------------------------------------*
439900000000     c     decftc        BEGSR
440000000000     c*
440100000000     c                   MOVEL     *blanks       wdftc
440200000000     c*
440300000000if  1c                   IF        wftc = 'S'
440400060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
440500000000e   1c                   ENDIF
440600000000if  1c                   IF        wftc = 'P'
440700060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
440800000000e   1c                   ENDIF
440900000000if  1c                   IF        wftc = 'A'
441000060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
441100000000e   1c                   ENDIF
441200000000if  1c                   IF        wftc = 'F'
441300060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
441400000000e   1c                   ENDIF
441500000000     c*
441600000000     c                   ENDSR
441700000000     c*--------------------------------------------------------------------------------------------*
441800000000     c* decodifica turno di chiusura
441900000000     c*--------------------------------------------------------------------------------------------*
442000000000     c     decgcx        BEGSR
442100000000     c*
442200000000     c                   MOVEL     *blanks       wdgcx
442300000000     c* giorno
442400000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
442500000000if  1c                   IF        a1 = '1'
442600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
442700000000e   1c                   ENDIF
442800000000if  1c                   IF        a1 = '2'
442900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
443000000000e   1c                   ENDIF
443100000000if  1c                   IF        a1 = '3'
443200060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
443300000000e   1c                   ENDIF
443400000000if  1c                   IF        a1 = '4'
443500060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
443600000000e   1c                   ENDIF
443700000000if  1c                   IF        a1 = '5'
443800060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
443900000000e   1c                   ENDIF
444000000000if  1c                   IF        a1 = '6'
444100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
444200000000e   1c                   ENDIF
444300000000if  1c                   IF        a1 = '7'
444400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
444500000000e   1c                   ENDIF
444600000000if  1c                   IF        a1 = ' '
444700000000     c                   EVAL      wdgcx = *blanks
444800000000e   1c                   ENDIF
444900000000     c* am/pm
445000000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
445100000000if  1c                   IF        a1 = ' '
445200060612     c                   EVAL      wdgcx =
445300060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
445400000000e   1c                   ENDIF
445500000000if  1c                   IF        a1 = 'M'
445600060612     c                   EVAL      wdgcx =
445700060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
445800000000e   1c                   ENDIF
445900000000if  1c                   IF        a1 = 'P'
446000060612     c                   EVAL      wdgcx =
446100060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
446200000000e   1c                   ENDIF
446300000000     c*
446400000000     c                   ENDSR
446500000000     c*--------------------------------------------------------------------------------------------*
446600000000     c* decodifica evento per GIACENZE
446700000000     c*--------------------------------------------------------------------------------------------*
446800000000     c     deccevgia     BEGSR
446900000000     c*
447000000000     c                   MOVEL     '0'           werr                           *NO errore
447100000000     c*
447200060622     C                   RESET                   tibs02ds
447300060622     C                   EVAL      t02Cod = 'ICE'
447400060622     C                   EVAL      t02Ke1 = wcev
447500060622     C                   EXSR      getTntbe00f
447600060622     C                   IF        t02Err <> 'E' AND                            *trovata
447700140528     C                             (§icegia = 'S'  OR
447800140528     C                               §icegiap = 'S')
447900060622     c                   MOVEL     §icedei       wcdex
448000000000x   1c                   ELSE                                                   *non trovato
448100000000     c                   MOVEL     '1'           werr                           *SI errore
448200030130     c                   MOVEL     *blanks       wcdex
448300000000e   1c                   ENDIF
448400000000     c*
448500000000     c                   ENDSR
448600000000     c*--------------------------------------------------------------------------------------------*
448700000000     c* Controlla la DS dsi Input
448800000000     c*--------------------------------------------------------------------------------------------*
448900000000     c     chkdsinput    BEGSR
449000000000     c*
449100000000     c                   MOVEL     '0'           werr                           *NO errore
449200000000     c*
449300000000     c* dalla DS di Input scompone il codice spedizione
449400000000     c                   MOVEL     nspedizi74    dsspedizi74
449500000000     c*
449600000000     c* controlla che la lina di partenza sia TUTTA numerica
449700000000     c                   SETOFF                                       99
449800000000     c                   TESTN                   dslnpi74             99
449900000000if  1c                   IF        NOT *in99                                    *non è un numero
450000000000     c                   MOVEL     '1'           werr                           *SI errore
450100140617     c                   eval      widmsg = 'TIS0769'
450200140617     c                   eval      widcampo = '*LNP'
450300140617     c                   clear                   wparm
450400140617     c                   exsr      messaggi
450500000000x   1c                   ELSE                                                   *è un numero
450600000000     c                   MOVE      dslnpi74      a1
450700000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
450800000000     c                   MOVEL     '1'           werr                           *SI errore
450900140617     c                   eval      widmsg = 'TIS0769'
451000140617     c                   eval      widcampo = '*LNP'
451100140617     c                   clear                   wparm
451200140617     c                   exsr      messaggi
451300000000e   2c                   ENDIF
451400000000e   1c                   ENDIF
451500000000     c*
451600000000     c* controlla che la serie spedizione sia TUTTA numerica
451700000000     c                   SETOFF                                       99
451800000000     c                   TESTN                   dsnrsi74             99
451900000000if  1c                   IF        NOT *in99                                    *non è un numero
452000000000     c                   MOVEL     '1'           werr                           *SI errore
452100140617     c                   eval      widmsg = 'TIS0769'
452200140617     c                   eval      widcampo = '*NRS'
452300140617     c                   clear                   wparm
452400140617     c                   exsr      messaggi
452500000000x   1c                   ELSE                                                   *è un numero
452600000000     c                   MOVE      dsnrsi74      a1
452700000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
452800000000     c                   MOVEL     '1'           werr                           *SI errore
452900140617     c                   eval      widmsg = 'TIS0769'
453000140617     c                   eval      widcampo = '*NRS'
453100140617     c                   clear                   wparm
453200140617     c                   exsr      messaggi
453300000000e   2c                   ENDIF
453400000000e   1c                   ENDIF
453500000000     c*
453600000000     c* controlla che il numero spedizione sia TUTTO numerico
453700000000     c                   SETOFF                                       99
453800000000     c                   TESTN                   dsnspi74             99
453900000000if  1c                   IF        NOT *in99                                    *non è un numero
454000000000     c                   MOVEL     '1'           werr                           *SI errore
454100140617     c                   eval      widmsg = 'TIS0769'
454200140617     c                   eval      widcampo = '*NSP'
454300140617     c                   clear                   wparm
454400140617     c                   exsr      messaggi
454500000000x   1c                   ELSE                                                   *è un numero
454600000000     c                   MOVE      dsnspi74      a1
454700000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
454800000000     c                   MOVEL     '1'           werr                           *SI errore
454900140617     c                   eval      widmsg = 'TIS0769'
455000140617     c                   eval      widcampo = '*NSP'
455100140617     c                   clear                   wparm
455200140617     c                   exsr      messaggi
455300000000e   2c                   ENDIF
455400000000e   1c                   ENDIF
455500110104     c*
455600110104     c* controlla che il codice cliente sia TUTTO numerico
455700110104     c                   IF        kscI74 <> *BLANK
455800110104     c                   SETOFF                                       99
455900110104     c                   TESTN                   kscI74               99
456000110104     c                   IF        NOT *in99                                    *non è un numero
456100110104     c                   MOVEL     '1'           werr                           *SI errore
456200140617     c                   eval      widmsg = 'TIS0769'
456300140617     c                   eval      widcampo = 'KSCI74'
456400140617     c                   clear                   wparm
456500140617     c                   exsr      messaggi
456600110104     c                   ELSE                                                   *è un numero
456700110104     c                   MOVE      kscI74        a1
456800110104     c                   IF        a1 < '0'                                     *trovata una lettera
456900110104     c                   MOVEL     '1'           werr                           *SI errore
457000140617     c                   eval      widmsg = 'TIS0769'
457100140617     c                   eval      widcampo = 'KSCI74'
457200140617     c                   clear                   wparm
457300140617     c                   exsr      messaggi
457400110104     c                   ENDIF
457500110104     c                   ENDIF
457600110104     c                   ENDIF
457700110104     c*
457800110104     c* controlla che il codice mittente sia TUTTO numerico
457900110104     c                   IF        ccmI74 <> *BLANK
458000110104     c                   SETOFF                                       99
458100110104     c                   TESTN                   ccmI74               99
458200110104     c                   IF        NOT *in99                                    *non è un numero
458300110104     c                   MOVEL     '1'           werr                           *SI errore
458400140617     c                   eval      widmsg = 'TIS0769'
458500140617     c                   eval      widcampo = 'CCMI74'
458600140617     c                   clear                   wparm
458700140617     c                   exsr      messaggi
458800110104     c                   ELSE                                                   *è un numero
458900110104     c                   MOVE      ccmI74        a1
459000110104     c                   IF        a1 < '0'                                     *trovata una lettera
459100110104     c                   MOVEL     '1'           werr                           *SI errore
459200140617     c                   eval      widmsg = 'TIS0769'
459300140617     c                   eval      widcampo = 'CCMI74'
459400140617     c                   clear                   wparm
459500140617     c                   exsr      messaggi
459600110104     c                   ENDIF
459700110104     c                   ENDIF
459800110104     c                   ENDIF
459900050104     c*
460000050104     c* controlla anno spedizione
460100050104     c                   TESTN                   cAASI74              99
460200050104if  1c                   IF        NOT *in99                                    *non è un numero
460300050104     c                   EVAL      cAASI74 = *ZERO
460400050104x   1c                   ENDIF
460500100222     c*
460600100223     c* Estraggo l'anno dalla data spedizione.
460700100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
460800100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
460900100223     C                   IF        dspI74 <> *BLANK
461000100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
461100100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
461200100223     C                   MONITOR
461300100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
461400100223     C                   ON-ERROR
461500100223     C                   ENDMON
461600100222     C                   ENDIF
461700170314
461800170314     c*  BRTCODE
461900170314     c                   clear                   wbrtcode
462000170315     c                   if        versioni74>='D'
462100170314     c                   eval      wbrtcode=brtcodei74
462200170314     c                   if        wbrtcode<>*blanks
462300170314     c                   exsr      sr_trul28
462400170314     c                   endif
462500170315     c
462600170315     c                   endif
462700000000     c*
462800000000     c* se errore, esce dal programma
462900140610if  1c                   IF        werr = '1'                                   *SI errore
463000140610     c                   eval      esito = werr                                 *errore
463100140610e   1c                   ENDIF
463200000000     c*
463300000000     c                   ENDSR
463400170314      /FREE
463500170314       //--------------------------------------------------------------
463600170314       //?Controllo correttezza BRTCODE
463700170314       //--------------------------------------------------------------
463800170314       BEGSR  sr_trul28 ;
463900170314          clear trul28ds0;
464000170314          i280cod=wbrtcode;
464100170314          trul28r0(trul28ds0);
464200170314          if o280err='1';
464300170314             werr='1' ;
464400170314             wIdMsg   = 'TIS0737';
464500170314             widcampo = 'BRTCODEI74' ;
464600170314             wparm = wbrtcode ;
464700170314             exsr Messaggi;
464800170314          endif;
464900170314       ENDSR;
465000170314      /end-FREE
465100000000     c*--------------------------------------------------------------------------------------------*
465200000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
465300000000     c* Input:   Walfa7 = Alfanumerico di 7
465400000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
465500000000     c*--------------------------------------------------------------------------------------------*
465600000000     c     xallinea1     BEGSR
465700000000     c*
465800000000     c* reimposta variabli di lavoro
465900000000     c                   CLEAR                   i
466000000000     c                   CLEAR                   z
466100000000     c                   CLEAR                   walfa7bis
466200000000     c                   EVAL      wzeri = *ALL'0'
466300000000     c*
466400000000     c     ' '           CHECKR    walfa7:7      i
466500000000if  1c                   IF        i <> 0
466600000000     c     i             SUBST     walfa7        walfa7bis
466700000000e   1c                   ENDIF
466800000000if  1c                   IF        walfa7bis <> *blanks
466900000000     c     7             SUB       i             z
467000000000if  2c                   IF        z <> 0
467100000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
467200000000e   2c                   ENDIF
467300000000     c                   EVAL      z = z+ 1
467400000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
467500000000e   1c                   ENDIF
467600000000     c*
467700000000     c                   ENDSR
467800000000     c*--------------------------------------------------------------------------------------------*
467900000000     c* caricamento tabelle occorrenti
468000000000     c*--------------------------------------------------------------------------------------------*
468100000000     c     cartab        BEGSR
468200000000     c*---
468300000000     c* organigrama
468400000000     c*---
468500000000     c                   CLEAR                   i                              *indice
468600000000     C                   CLEAR                   sorg
468700000000     C                   CLEAR                   sodes
468800000000     C                   CLEAR                   sofl1
468900000000     C                   CLEAR                   sodpd
469000060623     C                   OPEN      tntbe01l
469100000000     c     *loval        SETLL     azorg01l
469200000000     c                   READ      azorg01l                               99
469300000000do  1c                   DOW       NOT *in99
469400000000     c                   ADD       1             i
469500000000     c                   Z-ADD     orgfil        sorg(i)
469600000000     c                   MOVEL     orgdes        sodes(i)
469700000000     c                   MOVEL     orgde3        og143
469800020702     C*
469900020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
470000020702     C                   CLEAR                   dntw
470100020702     C*
470200020702     C                   MOVEL(P)  'NTW'         kbecod
470300020702     C                   MOVEL(P)  §ogntw        kbeke1
470400020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
470500020702     c                   MOVEL     *blanks       kbelin                         *lingua
470600020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
470700020702     C     keytbe        CHAIN     tntbe01l
470800020702     C                   IF        %found(tntbe01l)
470900020702     C                   MOVEL     tbeuni        dntw
471000020702     C                   ENDIF
471100020702     C*
471200020702     C                   MOVEL     §ntwfie       sofl1(i)
471300160406     C*
471400020702     c                   MOVEL     §ogntw        sodpd(i)
471500160406     C*
471600000000     c                   READ      azorg01l                               99
471700000000e   1c                   ENDDO
471800060623     C                   CLOSE     tntbe01l
471900000000     c*---
472000000000     c                   ENDSR
472100000000     c*--------------------------------------------------------------------------------------------*
472200000000     c* operazioni iniziali -da fare sempre-
472300000000     c*--------------------------------------------------------------------------------------------*
472400000000     c     rutinz        BEGSR
472500000000     c*
472600000000     c* reperimento data corrente
472700000000     C                   TIME                    n14                            *ora (6)+ data (8)
472800000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
472900000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
473000000000     C                   Z-ADD     n8            g08dat
473100000000     C                   Z-ADD     *zeros        g08inv
473200000000     C                   MOVEL     '0'           g08err
473300000000     c                   CALL      'XSRDA8'
473400000000     c                   PARM                    wlbda8
473500000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
473600000000     C                   MOVEL     datcor        anncor                         *anno corrente
473700000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
473800000000     c*
473900000000     c* calcola data corrente - 1 anno
474000000000     c                   Z-ADD     datcor        datpre                         *data precedente
474100000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
474200000000     c*
474300010716     c* azzera la DS di Output e le variabili di lavoro / controllo
474400000000     c                   MOVEL     '0'           esito
474500140603     c                   reset                   tis778dso                      *ds Output
474600140603     c                   reset                   tis778dsm                      *ds Output
474700000000     c                   CLEAR                   smi                            *di memorizzazione
474800000000     c                   CLEAR                   smseql
474900000000     c                   CLEAR                   smseqe
475000000000     c                   CLEAR                   smdevhev
475100000000     c                   CLEAR                   smdeve
475200000000     c                   CLEAR                   smheve
475300000000     c                   CLEAR                   smfle
475400000000     c                   CLEAR                   smdev
475500000000     c                   CLEAR                   smcev
475600130312     c                   CLEAR                   smspe
475700000000     c                   CLEAR                   smdmc
475800000000     c                   CLEAR                   smdmcR
475900000000     c                   CLEAR                   tote                           *contatore eventi
476000000000     c                   CLEAR                   seql                           *sequenza legami
476100010716     c                   RESET                   $AbTD                          *di lavoro
476200010716     c                   RESET                   $AbFirma
476300150312     c                   clear                   pdaavv
476400150421     c                   clear                   pdagia
476500140603     c* parametri ingresso
476600140603     c                   exsr      Kontrolla
476700000000     c* controlla da DS in Input
476800000000     c                   EXSR      chkdsinput
476900000000     c*
477000000000     c* carica i clienti dettaglio del cliente unificante
477100000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
477200000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
477300000000     c     keyvss        SETLL     tivss02l
477400000000     c     keyvss        READE     tivss02l                               99
477500000000do  1c                   DOW       NOT *in99
477600000000if  2c                   IF        vssvat = *blanks                             *attivo
477700000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
477800000000     c                             datcor <= vssdsc
477900010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
478000010719     c* --> TD per vedere la firma DPD
478100010716     c                   RESET                   $AbTD                          *di lavoro
478200010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
478300010719     c     kssksu        SETLL     tivss02l
478400010719     c     kssksu        READE     tivss02l                               99
478500010719do  4c                   DOW       NOT *in99
478600010716if  5c                   IF        vssvat = *blanks                             *attivo
478700010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
478800010716     c                             datcor <= vssdsc
478900010719if  7c                   IF        vssisv = 'TD'
479000010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
479100010719e   7c                   ENDIF
479200010719e   6c                   ENDIF
479300010716e   5c                   ENDIF
479400010719     c     kssksu        READE     tivss02l                               99
479500010719e   4c                   ENDDO
479600010716     c*
479700000000     c                   LEAVE                                                  *uscita ciclo
479800000000e   3c                   ENDIF
479900000000e   2c                   ENDIF
480000000000     c     keyvss        READE     tivss02l                               99
480100010716e   1c                   ENDDO                                                  *fine cl.abil.TT
480200000000     c*
480300060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
480400060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
480500151216     c* Se lo strategi user number è = *blanks --> lo imposto a *zeros
480600151216     c                   monitor
480700151216     c                   if        RQSCIDI174>0
480800151216     c                   endif
480900151216     c
481000151216     c                   on-error
481100151216     c                   clear                   RQSCIDI174
481200151216     c                   endmon
481300060621     c*
481400000000     c                   ENDSR
481500040330
481600040330     ***********************************************************************************************
481700040330     **?
481800040330     **?Reperimento ulteriori dati destinatario.
481900040330     **?
482000040330     ***********************************************************************************************
482100040330     C     ImpDSDest     BEGSR
482200040330
482300140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
482400140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
482500100121     C                             ar5Trd <> 'GEN'
482600140716     c                   Z-ADD     f_tasaas      kr5aas
482700140716     c                   Z-ADD     f_taslnp      kr5lnp
482800140716     c                   Z-ADD     f_tasnrs      kr5nrs
482900140716     c                   Z-ADD     f_tasnsp      kr5nsp
483000040330     c                   MOVEL     'GEN'         kr5trd
483100040531     c     keyar5        CHAIN     fiar531c
483200040330     C                   IF        %FOUND
483300040330     C                   EVAL      DAR5GEN = AR5Uni
483400100121     C                   ELSE
483500100121     C                   RESET                   DAR5GEN
483600040330     C                   ENDIF
483700100121     C                   ENDIF
483800100121     C
483900100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
484000100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
484100040330
484200040330     C                   ENDSR
484300040913
484400040913     ***********************************************************************************************
484500040913     **?
484600040913     **?Determino se addebitare l'immagine LDV.
484700040913     **?
484800040913     ***********************************************************************************************
484900040913     C     AddebitoLDV   BEGSR
485000040913
485100040913     C                   IF        LDVO74 = 'S'
485200040913
485300040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
485400040913
485500040913     C                   ENDIF
485600040913
485700040913     C                   ENDSR
485800051110
485900051110     ***********************************************************************************************
486000051110     **?
486100051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
486200051110     **?
486300051110     ***********************************************************************************************
486400051110     C     chkTivpi      BEGSR
486500051110
486600051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
486700051110     C                             esito10)
486800051110     C                   IF        esito10 > 0 AND NOT %ERROR
486900051110     C                   EVAL      gcfdio74 = 'N'
487000051110     C                   ENDIF
487100051110
487200051110     C                   ENDSR
487300051222
487400051222     ***********************************************************************************************
487500051222     **
487600051222     ** Linea arrivo Italia o estera.
487700051222     **
487800051222     ***********************************************************************************************
487900051222     C     setLnaItaEst  BEGSR
488000051222
488100051222     C                   RESET                   wrkLnaItaEst
488200051222     C                   EXSR      decOrg
488300051222     C                   EVAL      wrkLnaItaEst = wofl1
488400051222
488500051222     C                   ENDSR
488600060622
488700060622     ***********************************************************************************************
488800150615     ** Reperisco informazioni stato bolla
488900060622     ***********************************************************************************************
489000130312     C     srarb         BEGSR
489100130312     c                   clear                   wcev
489200140714     c                   clear                   wricons
489300130312     c                   clear                   worg
489400130312     c                   clear                   wdat
489500130312     c                   clear                   wora
489600150615     c                   clear                   ds2a
489700130312     c                   eval      dsevbspe = spe(1)
489800130312     c     karb          chain     fnarb01l
489900130312     c                   if        not %error and
490000130312     c                             %found(fnarb01l)
490100130422     c                   Z-ADD     arblna        worg
490200130312     c                   exsr      setLnaItaEst
490300130312     c* no per estero
490400130312     C                   IF        wrkLnaItaEst = ESTERO
490500130312     c                   leavesr
490600130312     c                   end
490700150617     c
490800130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
490900130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
491000130312     C                   SELECT
491100141120     c                   WHEN      arbdcm <> 0
491200130312     c                   MOVEL     '704'         wcev                           *causale evento
491300130313     c                   move      arbdcm        WDAT
491400130313     c                   move      arbhmc        WORA
491500150617     c
491600130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
491700150615     c                   WHEN      ARBFDC = ' ' and arbndc>0
491800130326     c* solo se distinta NON in test
491900130326     c                   clear                   ddstflr
492000130326     c                   z-add     arbifp        dstfgs
492100130326     c                   z-add     arbndc        dstnfv
492200130326     c     kdst          chain     fidst01l
492300130326     c                   if        not %error and
492400130326     c                             %found(fidst01l)
492500130326     c                   eval      ddstflr = dstflr
492600130326     c                   end
492700130327     c                   if        §DSTTSTPDA = 'C' or
492800130327     c                             §DSTTSTPDA = 'E'
492900130326     c                   leavesr
493000130326     c                   end
493100130326     c*
493200150617     c* Cerco PRIMO evento da PdA
493300130313     c                   move      arbndc        pctndc
493400131105     c                   exsr      cercaceP
493500150617     c*
493600150617     c* Cerco ULTIMO evento da PdA
493700131105     c                   move      'CET'         ttrd
493800130312     c     kcet          setgt     fipct02l
493900130312     c     kcet          readpe    fipct02l
494000130312     c                   if        not %error and
494100130312     c                             not %eof(fipct02l)
494200130312     c                   movel     pctdati       fipctcetds
494300140926      * firmatario se c'è riempie il campo
494400140926     c                   if        §pctnomfir <> *blank
494500140926     c                   eval      firmato74  = §pctnomfir
494600140926     c                   endif
494700130312     c* consegnata
494800130312     c                   if        §pctcmc = ' '
494900130312     c                   MOVEL     '704'         wcev                           *causale evento
495000130312     c                   else
495100140711     c                   move      'S'           wricons
495200130312     c* mancata consegna
495300130313     c                   movel     §PCTcmc       wcev
495400130312     c                   clear                   ds2a
495500130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
495600130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
495700130312     C                             :rpyEsito)
495800130312     c                   if        §2aflg = '§' and §2acpt = 'S'
495900130312     c                   move      'T  '         wcev
496000130312     c                   endif
496100130312     c                   endif
496200130312     c* ??? £6
496300130312     c                   if        §pctdata > 0
496400130313     c                   move      §PCTDATA      WDAT
496500130312     c                   movel     §PCTORA       WORA
496600130312     c                   endif
496700130312     c                   end
496800130312     c                   endSL
496900130312     c                   end
497000130312     c* ho trovato un evento da inserire  al 1° posto
497100130312     c                   if        wcev <> ' '
497200130312     c                   EXSR      deccevsta
497300130312     c                   IF        werr <> '1'                                  *no errore
497400130312     c                   EXSR      decorg
497500130312     c                   EXSR      edtdat
497600130312     c                   EXSR      edtora
497700130313     c* libero il 1° elemnto di schiera
497800150617     c                   eval      j = 49
497900150617     c                   for       j = 49 downto 1
498000130312     c                   if        cev(j) <> ' '
498100130312     c                   eval      cev(j+1) = cev(j)
498200130312     c                   eval      dev(j+1) = dev(j)
498300130312     c                   eval      hev(j+1) = hev(j)
498400130312     c                   eval      fle(j+1) = fle(j)
498500130312     c                   eval      eve(j+1) = eve(j)
498600130312     c                   end
498700130313     c                   endfor
498800130313     c                   eval      dev(1) = wdate
498900130313     c                   eval      hev(1) = worae
499000130312     c                   eval      cev(1)= wcev
499100130313     c                   eval      fle(1) = wodes
499200130313     c                   eval      eve(1) = wcdex
499300130313     c                   EVAL      cntEveO74 += 1
499400150310     c*
499500150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
499600150310     c                   if        §2aftc='A'
499700150310     c                   eval      pdaavv='S'
499800150310     c                   endif
499900150421     c                   if        §2aftc='G'
500000150421     c                   eval      pdagia='S'
500100150421     c                   endif
500200150310     c
500300130312     c                   end
500400130312     c                   end
500500130312     C                   ENDSR
500600140714     **_________________________________________________________
500700140714     c     cercafiglia   begsr
500800140714     **_________________________________________________________
500900140715     c                   clear                   keyForzata
501000140714      * salva chiave originale per eventi
501100140715     c                   eval      kasaasf = kasaas
501200140715     c                   eval      kaslnpf = kaslnp
501300140715     c                   eval      kasnrsf = kasnrs
501400140715     c                   eval      kasnspf = kasnsp
501500140714     c                   do        *hival
501600140715     c     keytasf       chain     fnlbl02l
501700140714     c                   if        %found(fnlbl02l)
501800140715     c                   eval      kasaasf = LBLaan
501900140715     c                   eval      kaslnpf = LBLlpn
502000140715     c                   eval      kasnrsf = LBLnrn
502100140715     c                   eval      kasnspf = LBLnsn
502200140714     c                   move      'X'           keyForzata
502300140714     c                   iter
502400140714     c                   else
502500140714     c                   leave
502600140714     c                   endif
502700140714     c                   enddo
502800140715     c     keytasf       setll     titas30c                               99    *no errore
502900140715     c     keytasf       READE     titas30c                               99    *no errore
503000141117      *se non trova titas con figlia forza di nuovo la mamma
503100140715if  3c                   IF        *in99                                        *non trovato
503200141117     c                   clear                   titasdssavf
503300140715x   3c                   ELSE                                                   *trovata
503400140715if  4c                   IF        *in01                                        *lettura titas000
503500140715     c                   EVAL      titasdssavf = titasds
503600140715e   4c                   ENDIF
503700140715if  4c                   IF        *in02                                        *lettura titas010
503800140715     c                   EVAL      titasdssavf = titasds
503900140715e   4c                   ENDIF
504000140715if  4c                   IF        *in03                                        *lettura titasp00
504100140715     c                   EVAL      titasdssavf = titasds
504200140715e   4c                   ENDIF
504300140715e   3c                   ENDIF
504400140714     C                   ENDSR
504500160318     **_________________________________________________________
504600160318     c     cercamamma    begsr
504700160318     **_________________________________________________________
504800160318     c                   clear                   keyForzata
504900160318     c                   EVAL      kblaan = arbaas
505000160318     c                   EVAL      kbllpn = arblnp
505100160318     c                   EVAL      kblnrn = arbnrs
505200160318     c                   EVAL      kblnsn = arbnsp
505300160318     c     keylbl        CHAIN     fnlbl01l                           99
505400160318if  1c                   IF        NOT *in99                                    *ha la mamma
505500160318     c                   EVAL      kasaas = lblaap
505600160318     c                   EVAL      kaslnp = lbllpp
505700160318     c                   EVAL      kasnrs = lblnrp
505800160318     c                   EVAL      kasnsp = lblnsp
505900160318     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
506000160318if  3c                   IF        not *in99                                        *non trovato
506100160318     c                   move      'X'           keyForzata
506200160318     c                   eval      esito=' '
506300160408     c                   clear                   titasdssavf
506400160318     c                   endif
506500160318     c                   endif
506600160318     c
506700160318     c                   ENDSR
506800131105     **_________________________________________________________
506900131105     c     cercaCEP      begsr
507000131105     **_________________________________________________________
507100131105     c                   move      'CEP'         ttrd
507200131105     c     kcet          setgt     fipct02l
507300131105     c     kcet          readpe    fipct02l
507400131105     c                   if        not %error and
507500131105     c                             not %eof(fipct02l)
507600131105     c                   movel     pctdati       fipctcetds
507700131105     c* consegnata
507800131105     c                   if        §pctcmc = ' '
507900131105     c                   MOVEL     '704'         wcev                           *causale evento
508000131105     c                   else
508100131105     c* mancata consegna
508200131105     c                   movel     §PCTcmc       wcev
508300131105     c                   clear                   ds2a
508400131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
508500131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
508600131105     C                             :rpyEsito)
508700131105     c                   if        §2aflg = '§' and §2acpt = 'S'
508800131105     c                   move      'T  '         wcev
508900131105     c                   endif
509000131105     c                   endif
509100131105     c* ??? £6
509200131105     c                   if        §pctdata > 0
509300131105     c                   move      §PCTDATA      WDAT
509400131105     c                   movel     §PCTORA       WORA
509500131105     c                   endif
509600131105     c                   end
509700131105     c* ho trovato un evento da inserire  al 1° posto
509800131105     c                   if        wcev <> ' '
509900131105     c                   EXSR      deccevsta
510000131105     c                   IF        werr <> '1'                                  *no errore
510100131105     c                   EXSR      decorg
510200131105     c                   EXSR      edtdat
510300131105     c                   EXSR      edtora
510400131105     c* libero il 1° elemnto di schiera
510500150617     c                   eval      j = 49
510600150617     c                   for       j = 49 downto 1
510700131105     c                   if        cev(j) <> ' '
510800131105     c                   eval      cev(j+1) = cev(j)
510900131105     c                   eval      dev(j+1) = dev(j)
511000131105     c                   eval      hev(j+1) = hev(j)
511100131105     c                   eval      fle(j+1) = fle(j)
511200131105     c                   eval      eve(j+1) = eve(j)
511300131105     c                   end
511400131105     c                   endfor
511500131105     c                   eval      dev(1) = wdate
511600131105     c                   eval      hev(1) = worae
511700131105     c                   eval      cev(1)= wcev
511800131105     c                   eval      fle(1) = wodes
511900131105     c                   eval      eve(1) = wcdex
512000131105     c                   EVAL      cntEveO74 += 1
512100131105     c                   end
512200131105     c                   end
512300131105     c                   endsr
512400131105     ***********************************************************************************************
512500130312     **
512600130312     ** Reperisco tabella da TNTBE00F.
512700130312     **
512800130312     ***********************************************************************************************
512900130312     C     getTntbe00f   BEGSR
513000060622     C                   EVAL      t02Lin = langTntbe
513100060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
513200060622     C                   SELECT
513300060622     C                   WHEN      t02Cod = 'ICE'
513400060622     C                   EVAL      dice = t02Uni
513500060622     C                   WHEN      t02Cod = 'CCH'
513600060622     C                   EVAL      dcch = t02Uni
513700060622     C                   WHEN      t02Cod = 'NTW'
513800060622     C                   EVAL      dntw = t02Uni
513900100119     C                   WHEN      t02Cod = 'I1A'
514000100119     C                   EVAL      dI1a = t02Uni
514100150304     C                   WHEN      t02Cod = 'SPC'
514200150304     C                   EVAL      dSPC = t02Uni
514300170301     C                   WHEN      t02Cod = 'CLI'
514400170301     C                   EVAL      dcli = t02Uni
514500060622     C                   ENDSL
514600060622     C                   ENDSR
514700060608
514800140529      /free
514900140529       //--------------------------------------------------------------
515000140529       //?Controllo formale dei dati passati.
515100140529       //--------------------------------------------------------------
515200140603       BEGSR  Kontrolla;
515300140529
515400140529       //?OpCode
515500140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
515600160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
515700140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
515800160818           esito='1'    ;
515900140529           exsr RoutEnd;
516000140529         ENDIF;
516100140603           IF  prmRqsDataSize > 0;
516200140603           ELSE;
516300160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
516400140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
516500160818             esito='1'    ;
516600140603             exsr RoutEnd;
516700140603           ENDIF;
516800140529
516900140529       //?Formato e size RQS
517000140603           IF  prmRqsFormato = formatoi74;
517100140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
517200140529           ELSE;
517300160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
517400140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
517500160818             esito='1'    ;
517600140529             exsr RoutEnd;
517700140529           ENDIF;
517800140529         //?Formato e size RPY
517900140603           IF  prmRpyFormato = formatoo74;
518000140529           ELSE;
518100160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
518200140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
518300160818             esito='1'    ;
518400140529             exsr RoutEnd;
518500140529           ENDIF;
518600140529           IF  prmRpyDataSize > 0;
518700140529           ELSE;
518800160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
518900140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
519000160818             esito='1'    ;
519100140529             exsr RoutEnd;
519200140529           ENDIF;
519300140616       //?Formato e size MSG
519400140617         if %parms > 9 ;
519500140617          IF  prmRpyFormMsg = formato;
519600140617          ELSE;
519700160818           // prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
519800140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
519900160818           esito='1'    ;
520000140616           exsr RoutEnd;
520100140617          ENDIF;
520200140617          IF  prmRpyMsgSize > 0;
520300140617          ELSE;
520400160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
520500140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
520600160818           esito='1'    ;
520700140616           exsr RoutEnd;
520800140617          ENDIF;
520900140617         ENDIF;
521000140529
521100140529       ENDSR;
521200140529
521300140616       //--------------------------------------------------------------
521400140617       //?Routine per schierare i messaggi nella ds tis778dsm
521500140616       //--------------------------------------------------------------
521600140616       BEGSR  Messaggi;
521700140616
521800140616       //?Se ho già caricato 99 messaggi esco
521900140616         IF  nrmsg = 99;
522000160818           esito='1'  ;
522100140616           exsr RoutEnd;
522200140616           clear nrmsg;
522300140616         ENDIF;
522400140616
522500140616         nrmsg += 1;
522600140616         skIdMsg(nrmsg) = wIdMsg;
522700140616         skIdCampo(nrmsg) = wIdCampo;
522800140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
522900140616         IF  wParm <> *blanks;
523000140616           skTextMsg (nrmsg) =
523100140616           rtvMsgLang(wIdMsg:langi74:wParm);
523200140616         ELSE;
523300140616           skTextMsg (nrmsg)=
523400140616           rtvMsgLang(wIdMsg:langi74);
523500140616         ENDIF;
523600140616
523700140616       ENDSR;
523800140616
523900140529       //--------------------------------------------------------------
524000140529       BEGSR  RoutEnd;
524100140530
524200140530
524300160322        if  esito <> '0' and esito<>'P';
524400140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
524500140530             else;
524600140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
524700140530        endif ;
524800140529
524900140530       //?ritorno
525000140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
525100140617         if %parms > 9 ;
525200140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
525300140617         endif;
525400140529         return;
525500140529
525600140529       ENDSR;
525700140529      /end-free
525800000000     c*--------------------------------------------------------------------------------------------*
525900000000     c* operazioni iniziali
526000000000     c*--------------------------------------------------------------------------------------------*
526100000000     c     *inzsr        BEGSR
526200000000     c*
526300000000     c* ricevimento parametri
526400140603     c*m   *ENTRY        PLIST
526500140529     c*m                 PARM                    esito
526600140529     c*m                 PARM                    tis174dsi
526700140529     c*m                 PARM                    tis174dso
526800000000     c*
526900000000     c* chiavi di lettura
527000000000     c     keytas        KLIST                                                  titas30c
527100000000     c                   KFLD                    kasaas                         -anno spedizione
527200000000     c                   KFLD                    kaslnp                         -linea partenza
527300000000     c                   KFLD                    kasnrs                         -serie
527400000000     c                   KFLD                    kasnsp                         -spedizione
527500140715     c     keytasf       KLIST                                                  titas30c
527600140715     c                   KFLD                    kasaasf                        -anno spedizione
527700140715     c                   KFLD                    kaslnpf                        -linea partenza
527800140715     c                   KFLD                    kasnrsf                        -serie
527900140715     c                   KFLD                    kasnspf                        -spedizione
528000130312     c     karb          KLIST                                                  titas30c
528100140716     c                   KFLD                    f_tasaas                         -anno spedizione
528200140716     c                   KFLD                    f_taslnp                         -linea partenza
528300140716     c                   KFLD                    f_tasnrs                         -serie
528400140716     c                   KFLD                    f_tasnsp                         -spedizione
528500130326     C     Kdst          KLIST
528600130326     C                   KFLD                    dstnpg
528700130326     C                   KFLD                    dstnfv
528800130326     C                   KFLD                    dstfgs
528900130326     c                   z-add     4             dstnpg
529000130326     C     Kcet          KLIST
529100130326     C                   KFLD                    arbLNA
529200130326     C                   KFLD                    pctNDC
529300130326     C                   KFLD                    arbpdc
529400131105     C                   KFLD                    TTRD              3
529500130312     C                   KFLD                    arbAAS
529600130312     C                   KFLD                    arbLNP
529700130312     C                   KFLD                    arbNRS
529800130312     C                   KFLD                    arbNSP
529900150403     C     KARP          KLIST
530000150403     C                   KFLD                    arbAAS
530100150403     C                   KFLD                    arbLNP
530200150403     C                   KFLD                    arbNRS
530300150403     C                   KFLD                    arbNSP
530400000000     c     keytaa        KLIST                                                  titaa30c
530500000000     c                   KFLD                    kaaaas                         -anno spedizione
530600000000     c                   KFLD                    kaalnp                         -linea partenza
530700000000     c                   KFLD                    kaanrs                         -serie
530800000000     c                   KFLD                    kaansp                         -spedizione
530900000000     c                   KFLD                    kaatrc                         -tipo anagrafica
531000000000     c     keyta4        KLIST                                                  tita430c
531100000000     c                   KFLD                    ka4aas                         -anno spedizione
531200000000     c                   KFLD                    ka4lnp                         -linea partenza
531300000000     c                   KFLD                    ka4nrs                         -serie
531400000000     c                   KFLD                    ka4nsp                         -spedizione
531500000000     c                   KFLD                    ka4trc                         -tipo riferimento
531600010925     c     ke2ta4        KLIST                                                  tita430c
531700010925     c                   KFLD                    ka4aas                         -anno spedizione
531800010925     c                   KFLD                    ka4lnp                         -linea partenza
531900010925     c                   KFLD                    ka4nrs                         -serie
532000010925     c                   KFLD                    ka4nsp                         -spedizione
532100000000     c     keyevb        KLIST                                                  fnevb01l
532200000000     c                   KFLD                    kvbaas                         -anno spedizione
532300000000     c                   KFLD                    kvblnp                         -linea partenza
532400000000     c                   KFLD                    kvbnrs                         -serie
532500000000     c                   KFLD                    kvbnsp                         -spedizione
532600010717     c     ke2evb        KLIST                                                  fnevb01l
532700010717     c                   KFLD                    kvbaas                         -anno spedizione
532800010717     c                   KFLD                    kvblnp                         -linea partenza
532900010717     c                   KFLD                    kvbnrs                         -serie
533000010717     c                   KFLD                    kvbnsp                         -spedizione
533100010717     c                   KFLD                    kvbdev                         -data evento
533200010717     c                   KFLD                    kvbhev                         -ora  evento
533300000000     c     keylbl        KLIST                                                  fnlbl01l
533400000000     c                   KFLD                    kblaan                         -anno seguente
533500000000     c                   KFLD                    kbllpn                         -linea seguente
533600000000     c                   KFLD                    kblnrn                         -serie seguente
533700000000     c                   KFLD                    kblnsn                         -spedizione seguente
533800000000     c     ke2lbl        KLIST                                                  fnlbl02l
533900000000     c                   KFLD                    kblaap                         -anno precedente
534000000000     c                   KFLD                    kbllpp                         -linea precedente
534100000000     c                   KFLD                    kblnrp                         -serie precedente
534200000000     c                   KFLD                    kblnsp                         -spediz.precedente
534300000000     c     keytbe        KLIST                                                  *tntbe01l
534400000000     c                   KFLD                    kbecod                          -tabella
534500000000     c                   KFLD                    kbeke1                          -chiave uno
534600000000     c                   KFLD                    kbeke2                          -chiave due
534700000000     c                   KFLD                    kbelin                          -lingua
534800000000     c                   KFLD                    kbesif                          -s.informativo
534900000000     c     keyvss        KLIST                                                  *tivss02l
535000000000     c                   KFLD                    kssksu                          -cl.unificante
535100000000     c                   KFLD                    kssisv                          -tipo servizio
535200010717     c     keyvtd        KLIST                                                  *tivtd01l
535300010717     c                   KFLD                    ktdksu                          -cl.unificante
535400000000     c     keycsb        KLIST                                                  *tncsb03l
535500000000     c                   KFLD                    ksbaas                         -anno spedizione
535600000000     c                   KFLD                    ksblnp                         -linea partenza
535700000000     c                   KFLD                    ksbnrs                         -serie
535800000000     c                   KFLD                    ksbnsp                         -spedizione
535900040906     c     keydct        KLIST
536000040906     c                   KFLD                    DCTAAC
536100040906     c                   KFLD                    DCTFIL
536200040906     c                   KFLD                    DCTNCA
536300000000     c     keygcp        KLIST                                                  *figcp01l
536400000000     c                   KFLD                    kcpaas                         -anno spedizione
536500000000     c                   KFLD                    kcplnp                         -linea partenza
536600000000     c                   KFLD                    kcpnrs                         -serie
536700000000     c                   KFLD                    kcpnsp                         -spedizione
536800000000     c                   KFLD                    kcpfrg                         -prg aperture
536900050404     c     K04GCP51      KLIST
537000050404     c                   KFLD                    kcpaas                         -anno spedizione
537100050404     c                   KFLD                    kcplnp                         -linea partenza
537200050404     c                   KFLD                    kcpnrs                         -serie
537300050404     c                   KFLD                    kcpnsp                         -spedizione
537400000000     c     keygnp        KLIST                                                  *fignp01l
537500000000     c                   KFLD                    knpagc                         -anno apertura
537600000000     c                   KFLD                    knpfgc                         -filiale apertura
537700000000     c                   KFLD                    knpngc                         -numero giacenza
537800000000     c                   KFLD                    knpfrg                         -prg apertura
537900040531     c     keyar5        KLIST                                                  *fiar531c
538000030206     c                   KFLD                    kr5aas                         -anno spedizione
538100030206     c                   KFLD                    kr5lnp                         -linea partenza
538200030206     c                   KFLD                    kr5nrs                         -serie
538300030206     c                   KFLD                    kr5nsp                         -spedizione
538400030206     c                   KFLD                    kr5trd                         -tipo record
538500170301     c     ktfntc        KLIST                                                  titas30c
538600170301     c                   KFLD                    k_NTCAPL
538700170301     c                   KFLD                    k_NTCnk1
538800170301     c                   KFLD                    k_NTCnk2
538900170301     c                   KFLD                    k_NTCtnt
539000040907
539100000000     c* apre i files
539200000000     c                   OPEN      titas30c
539300000000     c                   OPEN      titaa30c
539400000000     c                   OPEN      tita430c
539500000000     c                   OPEN      fnevb01l
539600000000     c                   OPEN      tivss02l
539700010716     c                   OPEN      tivtd01l
539800000000     c                   OPEN      azorg01l
539900170301     c                   OPEN      tfntc01l
540000000000     c                   OPEN      fnlbl01l
540100000000     c                   OPEN      fnlbl02l
540200050309     c                   OPEN      tigcp51l
540300050309     c                   OPEN      tignp01l
540400000000     c                   OPEN      tncsb03l
540500040906     c                   OPEN      fndct01l
540600040531     c                   OPEN      fiar531c
540700130312     c                   OPEN      fnarb01l
540800160127     c                   OPEN      fnblp01l
540900160309     c                   OPEN      fiar901l
541000150403     c                   OPEN      fiarp01l
541100140606     c                   OPEN      fiar501l
541200140606     c                   OPEN      fiar401l
541300130326     c                   OPEN      fidst01l
541400130312     c                   OPEN      fipct02l
541500060621     C                   CALLP     openTabel00f()
541600000000     c*
541700000000     c* caricamento tabelle occorrenti
541800000000     c                   EXSR      cartab
541900060621     C                   CALLP     inzLingue()
542000000000     c*
542100000000     c                   ENDSR
542200070109
542300070109     ***********************************************************************************************
542400070109     **
542500070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
542600070109     **
542700070109     ***********************************************************************************************
542800070109     C     eventoAnnulla BEGSR
542900070109
543000070109     C                   IF        §iceEvAn <> *BLANK
543100070109     C                   FOR       j = 1 TO %ELEM(smCev)
543200070109     C                   IF        §iceEvAn = smCev(j) AND
543300070109     C                             evbDevHev = smDevHev(j)
543400070109     C                   CLEAR                   smCev(j)
543500130312     C                   CLEAR                   smspe(j)
543600070109     C                   CLEAR                   smDevHev(j)
543700070109     C                   CLEAR                   smi(j)
543800070109     C                   CLEAR                   smSeql(j)
543900070109     C                   CLEAR                   smSeqe(j)
544000070109     C                   CLEAR                   smDeve(j)
544100070109     C                   CLEAR                   smHeve(j)
544200070109     C                   CLEAR                   smFle(j)
544300070109     C                   CLEAR                   smDev(j)
544400070109     C                   LEAVE
544500070109     C                   ENDIF
544600070109     C                   ENDFOR
544700070109     C                   ENDIF
544800070109
544900070109     C                   ENDSR
545000080131
545100100119     P*--------------------------------------------------
545200100119     P* Procedure name: GetDescrizioneIncassoContrassegno
545300100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
545400100119     P*                          o del contrassegno.
545500100121     P* Returns:        Descrizione modalità incasso contrassegno
545600100119     P*--------------------------------------------------
545700100119     P GetDescrizioneIncassoContrassegno...
545800100119     P                 B
545900100119     D GetDescrizioneIncassoContrassegno...
546000100119     D                 PI            35A
546100100119
546200100121     C                   RESET                   tibs02ds
546300100121     C                   EVAL      t02Cod = 'I1A'
546400100121     C                   EVAL      t02Lin = langTntbe
546500100119     ** Il contrassegno è stato incassato
546600100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
546700100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
546800100121     C                   ELSE
546900100119     ** Il contrassegno non è stato incassato.
547000100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
547100100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
547200100119     C                             ar5Trd <> 'GEN'
547300100121     C                   EVAL      kr5Aas = csbAas
547400100121     C                   EVAL      kr5Lnp = csbLnp
547500100121     C                   EVAL      kr5Nrs = csbNrs
547600100121     C                   EVAL      kr5Nsp = csbNsp
547700100119     C                   EVAL      kr5Trd = 'GEN'
547800160314     c                   if        esito='P'
547900160314     C     keyAr5        CHAIN     fiar501l
548000160314     c                   else
548100160314     C     keyAr5        CHAIN     fiar531c
548200160314     c                   endif
548300160314     c
548400100119     C                   IF        %FOUND()
548500100119     C                   EVAL      dAr5Gen = ar5Uni
548600100119     C                   ELSE
548700100119     C                   RESET                   dAr5Gen
548800100119     C                   RETURN                  *BLANK
548900100119     C                   ENDIF
549000100119     C                   ENDIF
549100100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
549200100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
549300100119     C                   ELSE
549400100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
549500100119     C                   ENDIF
549600100121     C                   ENDIF
549700100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
549800100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
549900100119     C                   IF        t02Err = 'E'
550000100119     C                   RETURN                  *BLANK
550100100119     C                   ENDIF
550200100120     C                   EVAL      dI1a = t02Uni
550300140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
550400140417     C                   RETURN                  dI1a.descrizi2
550500140417     C                   ENDIF
550600100119     C                   RETURN                  dI1a.descrizion
550700100119
550800100119     P GetDescrizioneIncassoContrassegno...
550900100119     P                 E
551000100119
551100110304
551200110304     P*--------------------------------------------------
551300110304     P* Procedure name: IsContrassegnoCompensato
551400110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
551500110304     P*                          to con una fattura.
551600110304     P* Returns:        *ON = Compensato
551700110304     P*--------------------------------------------------
551800110304     P IsContrassegnoCompensato...
551900110304     P                 B
552000110304     D IsContrassegnoCompensato...
552100110304     D                 PI              N
552200110304
552300110304     D retField        S               N   INZ(*OFF)
552400110304
552500110307     ** Il contrassegno non è stato rimborsato.
552600110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
552700110307     C                   RETURN    retField
552800110307     C                   ENDIF
552900110307
553000110307     C/EXEC SQL
553100110307     C+ SELECT '1'
553200110307     C+ INTO :retField
553300110307     C+ FROM tncsv00f
553400110307     C+ WHERE csvAas = :csbAas
553500110307     C+   AND csvLnp = :csbLnp
553600110307     C+   AND csvNrs = :csbNrs
553700110307     C+   AND csvNsp = :csbNsp
553800110307     C+   AND csvCav = 'COMP'
553900110307     C+ FETCH FIRST ROW ONLY
554000110307     C/END-EXEC
554100110304
554200110307     C                   SELECT
554300110307     C                   WHEN      sqlCode < 0
554400110307     C                   DUMP(A)
554500110307     C                   ENDSL
554600110304
554700110307     C                   RETURN    retField
554800110304
554900110304     P IsContrassegnoCompensato...
555000110304     P                 E
555100110304
555200120321
555300120321     P*--------------------------------------------------
555400120321     P* Procedure name: SetIdAssegnoMittente
555500120321     P* Purpose:        Imposta l'ID assegno mittente.
555600120321     P* Returns:        Esito.
555700120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
555800120321     P*                          segni mittente.
555900120321     P* Parameter:      priAbi => ABI assegno.
556000120321     P* Parameter:      priCab => CAB assegno.
556100120321     P*--------------------------------------------------
556200120321     P SetIdAssegnoMittente...
556300120321     P                 B
556400120321     D SetIdAssegnoMittente...
556500120321     D                 PI            10I 0
556600120321     D  priNumeroAssegno...
556700120321     D                                     LIKE(caNumO74)
556800120321     D  priAbi                             LIKE(caAbiO74)
556900120321     D  priCab                             LIKE(caCabO74)
557000120321
557100120321     D retField        S             10I 0 STATIC
557200120321
557300120321      /FREE
557400120321
557500120321       CLEAR retField;
557600120321
557700120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
557800120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
557900120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
558000120321       // In questo caso restituisco l'ID del primo assegno.
558100120321
558200120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
558300120321         RETURN retField;
558400120321       ENDIF;
558500120321
558600120321       EXEC SQL
558700120321         SELECT csmNra, csmAbi, csmCab
558800120321           INTO :priNumeroAssegno, :priAbi, :priCab
558900120321           FROM tncsm00f
559000120321           WHERE csmKey = :priNumeroAssegno
559100120321           FETCH FIRST 1 ROW ONLY
559200120321       ;
559300120321
559400120321       SELECT;
559500120321         WHEN sqlCode = 100;
559600120321           retField = sqlCode;
559700120321         WHEN sqlCode < *ZERO;
559800120321           DUMP(A);
559900120321           retField = sqlCode;
560000120321       ENDSL;
560100120321
560200120321       RETURN retField;
560300120321
560400120321      /END-FREE
560500120321     P SetIdAssegnoMittente...
560600120321     P                 E
560700120321
560800130513
560900130513     P*--------------------------------------------------
561000130513     P* Procedure name: GetFilUrl
561100130513     P* Purpose:        Restituisce l'URL della filiale.
561200130513     P* Returns:        URL filiale.
561300130513     P* Parameter:      piFiliale => ID filiale
561400130513     P*--------------------------------------------------
561500130513     P GetFilUrl       B
561600130513     D GetFilUrl       PI           256A
561700130513     D  piFiliale                     3P 0 CONST
561800130513
561900130513     C                   IF        piFiliale = *ZERO
562000130513     C                   RETURN    *BLANK
562100130513     C                   ENDIF
562200130513     C                   RESET                   tibs02ds
562300130513     C                   EVAL      t02Cod = 'UFI'
562400130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
562500130513     C                   EVAL      t02Lin = langTntbe
562600130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
562700130513     C                   IF        t02Err = 'E'
562800130513     C                   RETURN    *BLANK
562900130513     C                   ENDIF
563000130513     C                   RETURN    t02Uni
563100130513
563200130513     P GetFilUrl       E
563300130513
