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
034300171128       // -?Lunghezza del codice ricevuto in Input?
034400171128     d wLen            s              3  0 inz
034500000000     d*---
034600000000     d* Ds
034700000000     d*---
034800000000      * controllo data
034900000000     d wlbda8          DS                  INZ                                  *controlla data
035000000000     d  g08dat                        8  0                                       -data dritta
035100000000     d  g08inv                        8  0                                       -data invertita
035200000000     d  g08err                        1                                          -errore
035300000000     d  g08tgi                        5  0                                       -giorni fra date
035400000000      * scomposizione data generica
035500000000     d dsdat           DS                  INZ                                  *data
035600000000     d  dsann                         4  0                                       -anno
035700000000     d  dsmes                         2  0                                       -mese
035800000000     d  dsgio                         2  0                                       -giorno
035900000000      * scomposizione data generica (anno + mese/giorno)
036000000000     d                 DS                  INZ                                  *data aa + mm/gg
036100000000     d  dsannB                 1      4  0                                       -anno
036200000000     d  dsmgsB                 5      8  0                                       -mese/giorno
036300000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
036400000000      * composizione data generica editata
036500000000     d dsdate          DS                  INZ                                  *data
036600000000     d  dsgioe                        2  0                                       -giorno
036700000000     d  dsvd1                         1                                          -'/'
036800000000     d  dsmese                        2  0                                       -mese
036900000000     d  dsvd2                         1                                          -'/'
037000000000     d  dsanne                        4  0                                       -anno
037100000000      * scomposizione ora generica
037200000000     d dsora           DS                  INZ                                  *ora
037300000000     d  dshh                          2  0                                       -ore
037400000000     d  dsmm                          2  0                                       -minuti
037500000000      * composizione ora generica editata
037600000000     d dsorae          DS                  INZ                                  *ora
037700000000     d  dshhe                         2  0                                       -ore
037800000000     d  dsvo1                         1                                          -':'
037900000000     d  dsmme                         2  0                                       -minuti
038000030206      * lettura FIAR5 - record bancali
038100030206     d dar5ban       E DS
038200030206      * lettura FIAR5 - record supermercati
038300030206     d dar5scr       E DS
038400040330     d DAR5GEN       E DS
038500040330     D                                     INZ
038600040330     D                                     PREFIX(GEN)
038700000000      * tipi servizio
038800000000     d ds5e          E DS
038900000000      * disposizioni giacenza
039000000000     d ds2d          E DS
039100000000      * causale eventi
039200000000     d dice          E DS
039300000000      * causali chiusura CA
039400000000     d dcch          E DS
039500100119     d dI1a          E DS                  QUALIFIED
039600000000      * consegne anomale
039700000000     d ds7o          E DS
039800130312      * eventi
039900130312     d ds2a          E DS
040000160314     d
040100160314     d* cod bolla
040200160314     d ds3a          E DS
040300000000      * riferimenti
040400000000     d dta4a         E DS
040500070927     d dsbl4e        E DS
040600060627      * Estensione blp/arb tipo record "I"
040700060627     d dsbl4i        E DS
040800040913      * Addebito immagine POD
040900040913     d DLVA1         E DS
041000040913     D                                     INZ
041100000000      * lettura campo orgde3 di AZORG00F
041200000000     d og143         E DS
041300000000      * stati contrassegno
041400000000     d ds4s          E DS
041500141014      * particolarità consegna
041600141014     d ds7r          E DS
041700031217      * DS lettura campo TASFLO di TITAS
041800031217     d dtasflo       E DS
041900000000      * Ds ricerca cliente P.d.C.
042000000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
042100000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
042200000000     d indds         E DS                  EXTNAME(cnind00f) INZ
042300000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
042400000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
042500150304      * stato data prev cons
042600150304     d dspc          E DS
042700170301     d
042800170301     d dcli          E DS
042900150312     d
043000150312     d xgiolavds     e ds
043100000000      * reperimento clienti da cliente unificante
043200090903     d*tibs10ds      E DS
043300090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
043400000000      * scomposizione codice spedizione -DS Input-
043500000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
043600000000     d  dslnpi74                      3                                          -linea partenza
043700000000     d  dsnrsi74                      2                                          -serie
043800000000     d  dsnspi74                      7                                          -spedizione
043900000000      * composizione codice spedizione -bolle-
044000000000     d                 DS
044100000000     d  dstaslnp               1      3  0                                       -linea partenza
044200000000     d  dstasnrs               4      5  0                                       -serie
044300000000     d  dstasnsp               6     12  0                                       -spedizione
044400000000     d dstasspe                1     12                                         *ds cod spedizione
044500000000      * composizione data + ora evento
044600080131     d                 DS                  INZ
044700080131     d  evbdev                 1      8  0                                       -data evento
044800080131     d  evbhev                 9     12  0                                       -ora  evento
044900080131     d evbDevHev               1     12  0                                      *ds data+ora evento
045000130312      * composizione spedizione evento
045100130312     d                 DS                  INZ
045200130312     d  evbaas                 1      4  0                                       -data evento
045300130312     d  evblnp                 5      7  0                                       -ora  evento
045400130312     d  evbnrs                 8      9  0                                       -ora  evento
045500130312     d  evbnsp                10     16  0                                       -ora  evento
045600130312     d dsevbspe                1     16  0                                      *ds data+ora evento
045700060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
045800000000      * ordinamento schiere eventi
045900000000     d                 DS
046000000000     d  dsseql                 1      3  0                                       -sequenza legame
046100000000     d  dsseqe                 4      6  0                                       -sequenza evento
046200000000     d  dsdevhev               7     18  0                                       -data + ora
046300000000     d  dsi                   19     21  0                                       -indice
046400000000     d dssort                  1     21  0                                      *ds cod spedizione
046500000000     d* Input routine XALLINEA
046600000000     d walfa7          S              7                                         *alfanumerico 7
046700000000     d walfa7bis       S              7                                         *alfanumerico 7
046800000000     d wzeri           S              7                                         *alfa con zeri
046900000000     d*---
047000000000     d* Variabili
047100000000     d*---
047200170301     d k_ntcapl        s                   like(ntcapl)
047300170301     d k_ntcnk1        s                   like(ntcnk1)
047400170301     d k_ntcnk2        s                   like(ntcnk2)
047500170301     d k_ntctnt        s                   like(ntctnt)
047600101104     d i               S             10I 0                                      *indici
047700101104     d ii              S             10I 0
047800101104     d j               S             10I 0
047900101104     d jj              S             10I 0
048000101104     d z               S             10I 0
048100101104     d s               S             10I 0
048200101104     d k               S             10I 0
048300101104     d o               S             10I 0
048400101104     d tote            S             10I 0                                      *totale eventi memor
048500000000     d seql            S              5  0                                      *sequenza legami
048600000000     d n14             S             14  0                                      *Numerico 14
048700000000     d n8              S              8  0                                      *Numerico 8
048800000000     d n7              S              7  0                                      *Numerico 7
048900170310     d alfa4           S              4                                         *Numerico 4
049000010927     d n3              S              3  0                                      *Numerico 3
049100000000     d a1              S              1                                         *Alfanumerico 1
049200000000     d a3              S              3                                         *Alfanumerico 3
049300000000     d a5              S              5                                         *Alfanumerico 5
049400010717     d a8              S              8                                         *Alfanumerico 8
049500000000     d a15             S             15                                         *Alfanumerico 15
049600000000     d a200            S            200                                         *Alfanumerico 200
049700000000     d a200bis         S            200                                         *Alfanumerico 200
049800010927     d a230            S            230                                         *Alfanumerico 230
049900000000     d a400            S            400                                         *Alfanumerico 400
050000000000     d datcor          S              8  0                                      *data corrente
050100000000     d datpre          S              8  0                                      *data precedente
050200000000     d oracor          S              6  0                                      *ora corrente
050300000000     d anncor          S              4  0                                      *anno corrente
050400000000     d annpre          S              4  0                                      *anno precedente
050500140617     d wIdMsg          s              7a
050600140617     d wIdCampo        s             10a
050700000000     d werr            S              1                                         *errore generico
050800000000     d wdat            S              8  0                                      *data (aaaammgg)
050900000000     d wdate           S             10                                         *data (gg/mm/aaaa)
051000000000     d wora            S              4  0                                      *ora  (hhmm)
051100000000     d worae           S              5                                         *ora  (hh:mm)
051200000000     d worg            S              3  0                                      *filiale
051300000000     d wodes           S             30                                         *descrizione filiale
051400000000     d wofl1           S              1                                         *flag italia/estero
051500150617     d wtasfl1         S              1                                         *flag italia/estero
051600020702     d wodpd           S              3                                         *flag filiale DPD
051700000000     d wcev            S              3                                         *causale evento
051800030130     d wcdex           S             30                                         *descrizione bar/pt
051900000000     d wpgm            S             20                                         *programma
052000000000     d wftc            S              1                                         *cons.partic.
052100140714     d wricons         S              1                                         *wrk per riconsegna
052200140715     d keyforzata      S              1                                         *wrk per riconsegna
052300000000     d wdftc           S             50                                         *descriz.cons.partic
052400000000     d d1wdftc         S             50                                         *descriz.cons.part.1
052500000000     d d2wdftc         S             50                                         *descriz.cons.part.2
052600000000     d wgcx            S              2                                         *turno chiusura
052700000000     d wdgcx           S             20                                         *descriz.turno chius
052800000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
052900000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
053000000000     d we              S              3                                         *descrizione 'e'
053100000000     d bollaorig       S              1                                         *bolla originaria
053200150310     d PDAavv          S              1                                         *evento AVV a PDA
053300150421     d PDAgia          S              1                                         *evento GIA a PDA
053400000000     d sostrit         S              1                                         sostituisce Ritiro
053500000000     d sostcon         S              1                                         sostituisce Consegna
053600000000     d newevb          S              1                                         *nuovo evento
053700000000     d lavdevhev       S             12  0                                      *data + ora
053800040913     D WrkCurDate      S               D                                        Data corrente
053900060620     D wrkDtInvDisp    S              8
054000060620     D wrkLnaItaEst    S              1
054100060620     D rpyOpCode       S             10A
054200060620     D rpyEsito        S             10I 0
054300060621     D nullPtr         S               *
054400080131     D titas000FigliaPtr...
054500080131     D                 S               *   INZ(%ADDR(titas000Figlia))
054600010716     d*---
054700010716     d* Variabili di controlli
054800010716     d*---
054900010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
055000010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
055100000000     d*---
055200000000     d* Variabili di memorizzazione
055300000000     d*---
055400000000     d mhcre           S              5                                         *consegna richiesta
055500000000     d mdcre           S             10
055600000000     d mtcre           S             50
055700000000     d mftce           S            100                                         *consegna partic.
055800000000     d mgcxe           S            100                                         *turno chiusura
055900000000     d mffd            S            100                                         *fermo deposito
056000010927     d mmncB           S             35                                         *NON consegna LAV
056100010927     d mmncC           S             35
056200010927     d mmncG           S             35                                         *NON consegna GIAC
056300010927     d mmncH           S             35
056400010927     d mmncN           S             35                                         *NON consegna NOFATT
056500010927     d mmncO           S             35
056600050701     d mmnc8           S             35
056700050701     d mmnc9           S             35
056800030206     d mtel            S             20                                         *N° tel destinatario
056900030206     d mban            S             35                                         *bancali
057000030207     d mscr            S            500                                         *supermercati
057100030210     d msdace          S             10
057200030210     d msorce          S              5
057300030210     d msdcre          S             10
057400030210     d mshcre          S              5
057500030210     d mstcre          S             30
057600030210     d msnom           S             50
057700030210     d mscrT           S            500    DIM(5)
057800030211     d wmscrT          S           2504
057900000000     d*---
058000000000     d* Schiere
058100000000     d*---
058200000000      * taabella clienti del cliente unificante
058300000000      * tabella tipi servizio
058400000000     d stsp            S              1    DIM(20)                              *tipi servizio
058500000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
058600000000      * tabella consegne anomale
058700030130     d scca            S              1    DIM(30)                              *consegna anomala
058800000000      * tabella disposizioni giacenza
058900000000     d sdis            S              1    DIM(10)                              *codice
059000000000     d sddis           S             20    DIM(10)                              *descrizione italian
059100000000      * tabella filiali
059200000000     d sorg            S              3  0 DIM(300)                             *filiale
059300000000     d sodes           S             30    DIM(300)                             *descrizione filiale
059400000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
059500160406     d sodpd           S              3    DIM(300)                             ntw
059600000000      * eventi memorizzati
059700000000     d smi             S              3  0 DIM(200)                             *indice
059800000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
059900000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
060000000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
060100000000     d smdeve          S             10    DIM(200)                             *data evento editata
060200000000     d smheve          S              5    DIM(200)                             *ora  evento editata
060300000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
060400000000     d smdev           S             30    DIM(200)                             *descrizione evento
060500000000     d smcev           S              3    DIM(200)                             *causale evento
060600130313     d smspe           S             16  0 DIM(200)                             *causale evento
060700000000      * eventi memorizzati x ordinamento
060800000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
060900000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
061000000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
061100000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
061200000000     d spdeve          S             10    DIM(200)                             *data evento editata
061300000000     d spheve          S              5    DIM(200)                             *ora  evento editata
061400000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
061500000000     d spdev           S             30    DIM(200)                             *descrizione evento
061600000000     d spcev           S              3    DIM(200)                             *causale evento
061700130313     d spspe           S             16  0 DIM(200)                             *causale evento
061800000000      * note giacenze memorizzate
061900051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
062000051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
062100000000      * note memorizzate
062200000000     d smnot           S            100    DIM(10)
062300030206     d*---
062400030206     d* definizioni per passaggio parametri
062500030206     d*---
062600000000     d esito           s              1
062700000000      * DS contenente i dati di INPUT
062800170316     d tis778dsi     e DS                  INZ(*EXTDFT)
062900050104     D  cAASI74                       4
063000050104     D                                     OVERLAY(AASI74)
063100000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
063200140530     d tis778dso     e DS                  INZ(*EXTDFT)
063300000000     d dev                           10    DIM(50) OVERLAY(dato74)
063400000000     d hev                            5    DIM(50) OVERLAY(orao74)
063500000000     d fle                           30    DIM(50) OVERLAY(opeo74)
063600041007     d eve                           30    DIM(50) OVERLAY(eveo74)
063700041007     d cev                            3    DIM(50) OVERLAY(cevo74)
063800000000     d nota                         100    DIM(10) OVERLAY(notao74)
063900130313     d spe             S             16  0 DIM(50)
064000040906
064100140616     d tis778dsM     e DS                  INZ(*EXTDFT)
064200140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
064300140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
064400140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
064500140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
064600140530      *
064700040906     D FIDN12DS      E DS
064800040906     D                                     INZ
064900040906     D  I12FEL       E
065000040906     D                                     INZ('E')
065100040906     D  I12FAN       E
065200040906     D                                     INZ('E')
065300040906     D  O12KeyAry                    14
065400040906     D                                     DIM(20)
065500040906     D                                     OVERLAY(O12KEY)
065600040906
065700040906     D O12KeyDS        DS
065800040906     D                                     INZ
065900040906     D  O12KeyAAC                     4S 0
066000040906     D  O12KeyFil                     3S 0
066100040906     D  O12KeyNCA                     7S 0
066200040906
066300020702      * DS lettura tabella "NTW"
066400020702     d DNTW          E DS
066500130326     d Ddstflr       E DS
066600170314       // -?Controllo BRTCODE
066700170314     d TRUL28DS0     e ds                  INZ(*EXTDFT)
066800041223
066900041223     D ChkCode         S             10U 0
067000041223     D nEsito          S              5I 0
067100051110     D esito10         S             10I 0
067200170315     D prwRqsOpCode...
067300170315     D                 s             10I 0
067400170315     D prwRpyOpCode...
067500170315     D                 s             10I 0
067600170315     D prwRpyIdMsg...
067700170315     D                 s             10I 0
067800170315     D prwRqsFormato...
067900170315     D                 s             10A
068000170315     D prwRqsData...
068100170315     D                 s          32767A
068200170315     D prwRqsDataSize...
068300170315     D                 s             10I 0
068400170315     D prwRpyFormato...
068500170315     D                 s             10A
068600170315     D prwRpyData...
068700170315     D                 s          32767A
068800170315     D prwRpyDataSize...
068900170315     D                 s             10I 0
069000170315     D prwRpyFormMsg...
069100170315     D                 s             10A
069200170315     D prwRpyMessage...
069300170315     D                 s          32767A
069400170315     D prwRpyMsgSize...
069500170315     D                 s             10I 0
069600080131
069700100119     D*--------------------------------------------------
069800100119     D* Procedure name: GetDescrizioneIncassoContrassegno
069900100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
070000100119     D*                          o del contrassegno.
070100100121     D* Returns:        Descrizione modalità incasso contrassegno.
070200100119     D*--------------------------------------------------
070300100119     D GetDescrizioneIncassoContrassegno...
070400100119     D                 PR            35A
070500110304
070600110304     D*--------------------------------------------------
070700110304     D* Procedure name: IsContrassegnoCompensato
070800110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
070900110304     D*                          to con una fattura.
071000110304     D* Returns:        *ON = Compensato
071100110304     D*--------------------------------------------------
071200110304     D IsContrassegnoCompensato...
071300110304     D                 PR              N
071400120321
071500120321     D*--------------------------------------------------
071600120321     D* Procedure name: SetIdAssegnoMittente
071700120321     D* Purpose:        Imposta l'ID assegno mittente.
071800120321     D* Returns:        Esito.
071900120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
072000120321     D*                          segni mittente.
072100120321     D* Parameter:      priAbi => ABI assegno.
072200120321     D* Parameter:      priCab => CAB assegno.
072300120321     D*--------------------------------------------------
072400120321     D SetIdAssegnoMittente...
072500120321     D                 PR            10I 0
072600120321     D  priNumeroAssegno...
072700120321     D                                     LIKE(caNumO74)
072800120321     D  priAbi                             LIKE(caAbiO74)
072900120321     D  priCab                             LIKE(caCabO74)
073000130513
073100130513     D*--------------------------------------------------
073200130513     D* Procedure name: GetFilUrl
073300130513     D* Purpose:        Restituisce l'URL della filiale.
073400130513     D* Returns:        URL filiale.
073500130513     D* Parameter:      piFiliale => ID filiale
073600130513     D*--------------------------------------------------
073700130513     D GetFilUrl       PR           256A
073800130513     D  piFiliale                     3P 0 CONST
073900130513
074000130513
074100140527       //--------------------------------------------------------------
074200140527       //?Definizione prototipi.
074300140527       //--------------------------------------------------------------
074400140527      /copy gaitrasrc/srcprotopr,tis778R
074500140529      /copy gaitrasrc/srcconst,tis778R
074600170314      /copy gaitrasrc/srcprotopr,TRUL28R0
074700170315      /copy gaitrasrc/srcconst,fnlgw00r
074800140527       //--------------------------------------------------------------
074900140527       //?Definizione parametri programma.
075000140527       //--------------------------------------------------------------
075100140529     d tis778_GetBolla...
075200140527     d                 PI
075300140527     d prmRqsOpCode...
075400140527     d                               10I 0 CONST
075500140527     d prmRpyOpCode...
075600140527     d                               10I 0
075700140527     d prmRpyIdMsg...
075800140527     d                               10I 0
075900140527     d prmRqsFormato...
076000140527     d                               10A   CONST
076100140527     d prmRqsData...
076200140527     d                            32767A   OPTIONS(*VARSIZE)
076300140527     d prmRqsDataSize...
076400140527     d                               10I 0 CONST
076500140527     d prmRpyFormato...
076600140527     d                               10A   CONST
076700140527     d prmRpyData...
076800140527     d                            32767A   OPTIONS(*VARSIZE)
076900140527     d prmRpyDataSize...
077000140527     d                               10I 0 CONST
077100140527     d prmRpyFormMsg...
077200140527     d                               10A   CONST OPTIONS(*NOPASS)
077300140527     d prmRpyMessage...
077400140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
077500140527     d prmRpyMsgSize...
077600140527     d                               10I 0 CONST OPTIONS(*NOPASS)
077700080131
077800000000     i*--------------------------------------------------------------------------------------------*
077900000000     i* Input
078000000000     i*--------------------------------------------------------------------------------------------*
078100000000     i*---
078200000000     i* Indicatori lettura formati record -TITAS-
078300000000     i*---
078400000000     ititas000      01
078500000000     ititas010      02
078600000000     ititasp00      03
078700000000     c*--------------------------------------------------------------------------------------------*
078800000000     c* Main lines
078900000000     c*--------------------------------------------------------------------------------------------*
079000000000     c*
079100000000     c* operazioni iniziali -da fare sempre-
079200000000     c                   EXSR      rutinz
079300000000     c*
079400000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
079500000000if  1c                   IF        esito <> '1'                                 *no errori
079600100219     c*
079700100219     c                   MOVEL     dslnpi74      kaslnp
079800100219     c                   MOVEL     dsnrsi74      kasnrs
079900100219     c                   MOVEL     dsnspi74      kasnsp
080000100219     c*
080100111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
080200111103     c* restituisco errore.
080300111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
080400111103     c* e se non la trovo provo con l'anno precedente.
080500160318    1c                   IF        aasI74 <> *ZERO
080600100219     c                   Z-ADD     aasI74        kasaas
080700100219     c     keytas        SETLL     titas30c                               99
080800160318    2c                   IF        NOT *IN99
080900160318
081000160318     c     keytas        chain     fnblp01l
081100160318    3c                   if        %found(fnblp01l) and arbft1=' '
081200160318     c                   MOVEL     'P'           esito                          *bolla in part
081300160318   x3c                   else
081400111103     c                   EVAL      esito = '1'
081500160318    3c                   ENDIF
081600160318    2c                   ENDIF
081700160318
081800160318   x1c                   ELSE
081900000000     c                   Z-ADD     anncor        kasaas
082000000000     c     keytas        SETLL     titas30c                               99
082100000000if  2c                   IF        NOT *in99                                    *non trovato
082200160318
082300160318     c     keytas        chain     fnblp01l
082400160318    3c                   if        %found(fnblp01l) and arbft1=' '
082500160318     c                   MOVEL     'P'           esito                          *bolla in part
082600160318   x3c                   else
082700160318
082800160318     C* verifico se in partenza: se non è in partenza allora verifico anno precedente
082900000000     c                   Z-ADD     annpre        kasaas
083000000000     c     keytas        SETLL     titas30c                               99
083100160318if  4c                   IF        NOT *in99                                    *non trovato
083200000000     c                   MOVEL     '1'           esito                          *errore
083300160318e   4c                   ENDIF
083400160318e   3c                   ENDIF
083500000000e   2c                   ENDIF
083600160318    1c                   ENDIF
083700160127     c
083800160314    2c                   if        esito = '1'
083900140617     c                   eval      widmsg = 'TIS0564'
084000140616     c                   eval      widcampo = 'NSPEDIZI74'
084100140616     c                   clear                   wparm
084200140616     c                   exsr      messaggi
084300160314    2c                   endif
084400160318     c
084500141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
084600141126     c                   eval      kasaasm = kasaas
084700141126     c                   eval      kaslnpm = kaslnp
084800141126     c                   eval      kasnrsm = kasnrs
084900141126     c                   eval      kasnspm = kasnsp
085000141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
085100140715      * ultima figlia per forzare i dati in output
085200160318     c                   if        esito<>'P'
085300140714     c                   exsr      cercafiglia
085400160318     c                   else
085500160318     c                   exsr      cercamamma
085600160318     c                   endif
085700000000     c*
085800160127     c* Se bolla in FNBLP in partenza eseguo routine a parte
085900160314    2c                   if        esito='P'
086000160127     c
086100160127     c* imposta i campi della DS di Output dagli eventi
086200160127     c                   EXSR      impdsoevb
086300160127     c
086400160127     c* imposta i campi del C%Ass dal fiar9
086500160127     c                   EXSR      impdsoar9
086600160127     c
086700160127     c* impostazione campi in DS di output
086800160309     c                   exsr      impds_soloblp
086900160309     c                   exsr      impdsoarb
087000160309     c
087100160309     c* imposta i campi della DS di Output dalle note
087200160309     c                   EXSR      impdsonot
087300160314    2c                   endif
087400160127     c
087500000000     c* se trovata, legge la bolla
087600160127if  2c                   IF        esito <> '1'  and esito<>'P'
087700000000     C                   SETOFF                                       010203
087800140715     c     keytas        SETLL     titas30c
087900000000     c     keytas        READE     titas30c                               99    *no errore
088000000000if  3c                   IF        *in99                                        *non trovato
088100000000     c                   MOVEL     '1'           esito                          *errore
088200140617     c                   eval      widmsg = 'TIS0734'
088300140616     c                   eval      widcampo = 'NSPEDIZI74'
088400140616     c                   clear                   wparm
088500140616     c                   exsr      messaggi
088600000000x   3c                   ELSE                                                   *trovata
088700000000if  4c                   IF        *in01                                        *lettura titas000
088800000000     c                   EVAL      titasdssav = titasds
088900000000e   4c                   ENDIF
089000000000if  4c                   IF        *in02                                        *lettura titas010
089100000000     c                   EVAL      titasdssav = titasds
089200000000e   4c                   ENDIF
089300000000if  4c                   IF        *in03                                        *lettura titasp00
089400000000     c                   EVAL      titasdssav = titasds
089500000000e   4c                   ENDIF
089600141117      * se trovo la figlia ma ancora non è su titas
089700150617      * Se la bolla non è legata, nei campi della "figlia" ovvero F_  ci sono
089800150617      *  i campi della bolla originale passata
089900141117     c                   if        keyforzata <> *blank and
090000141117     c                             f_tasnsp = 0
090100141117     c                   eval      titasdssavf = titasdssav
090200141120     c                   else
090300141120     c                   clear                   keyforzata
090400141117     c                   endif
090500000000e   3c                   ENDIF
090600000000     c*
090700000000     c* controlla validità bolla
090800000000     c                   EXSR      chktas
090900000000     c*
091000030130     c* imposta i campi della DS di Output
091100000000if  3c                   IF        esito <> '1'                                 *no errore
091200140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
091300140714      * spedizione richiesta a video sempre
091400100225     c*
091500140716     c                   EVAL      dstaslnp = s_taslnp
091600140716     c                   EVAL      dstasnrs = s_tasnrs
091700140716     c                   EVAL      dstasnsp = s_tasnsp
091800140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
091900140723     c* chain ar5 con bolla richiesta a video
092000160127     c                   Z-ADD     s_tasaas      kr5aas
092100140723     c                   Z-ADD     s_taslnp      kr5lnp
092200140723     c                   Z-ADD     s_tasnrs      kr5nrs
092300140723     c                   Z-ADD     s_tasnsp      kr5nsp
092400100225     c                   MOVEL     'GEN'         kr5trd
092500100225     c     keyar5        CHAIN     fiar531c
092600100225     C                   IF        %FOUND
092700100225     C                   EVAL      DAR5GEN = AR5Uni
092800100225     C                   ELSE
092900100225     C                   RESET                   DAR5GEN
093000100225     C                   ENDIF
093100000000     c*
093200000000     c* imposta i campi della DS di Output dalle bolle
093300000000     c                   EXSR      impdsotas
093400000000     c*
093500000000     c* imposta i campi della DS di Output dagli eventi
093600000000     c                   EXSR      impdsoevb
093700000000     c*
093800000000     c* imposta i campi della DS di Output dal contrassegno
093900000000     c                   EXSR      impdsocsb
094000000000     c*
094100000000     c* imposta i campi della DS di Output dalla giacenza
094200000000     c                   EXSR      impdsogcp
094300000000     c*
094400000000     c* imposta i campi della DS di Output dalle note
094500000000     c                   EXSR      impdsonot
094600040330     c*
094700040330     c* imposta i campi della DS di Output del destinatario.
094800040330     c                   EXSR      ImpDSDest
094900010716     C*
095000160323     c* imposta i campi della DS di Output dalla firma DPD/EEX
095100010716     c                   EXSR      ImpdsoDPD
095200040913     C*
095300040913     c* imposta i campi della DS di Output di addebito immagine LDV.
095400040913     c                   EXSR      AddebitoLDV
095500000000e   3c                   ENDIF
095600000000e   2c                   ENDIF
095700140613e   1c                   ENDIF
095800160314     c
095900140919      *riempie campi con riferimenti alla consegna
096000160314      *  non per bolla ancora in partenza
096100160314     c                   if        esito <> '1'  and esito <>'P'
096200160314     c
096300140919     c                   if        f_tasdcm > 0
096400140919     c                   movel     f_tasdcm      wdat
096500140919     c                   exsr      edtdat
096600140919     c                   eval      dtconmeo74  = wdate
096700140919     c                   MOVEL     f_tashmc      wora
096800140919     c                   EXSR      edtora
096900140919     c                   eval      orconmeO74  =  worae
097000140919     c                   eval      flgbolcO74  =  'S'
097100140919     c                   eval      flgboldO74  =  'N'
097200141014      *firmatario distinta PDA
097300141014     c                   clear                   firmato74
097400141015     c                   if        gen§ar5pdaco <> 'NO'
097500141014     c                   exsr      repfirmat
097600141014     c                   else
097700141014     c                   if        f_tasgma <> *blank
097800141015     C                   EVAL      tblKey = f_tasgma
097900141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
098000141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
098100141014     C                             :rpyOpCode:rpyEsito)
098200141014     C                   IF        rpyOpCode = 'FOUND'
098300141014     c                   if        §7r2fi = 'S'
098400141014     c                   exsr      repfirmat
098500141014     c                   endif
098600141014     c                   endif
098700141014     c                   endif
098800141014     c                   endif
098900141014      *else consegna
099000140919     c                   else
099100140919     c                   eval      flgbolcO74  =  'N'
099200140919     c                   if        F_tasndc > 0 and f_tasddc > 0
099300141119     c                             and f_tasddc <= datcor
099400140919     c                   eval      flgboldo74 = 'S'
099500140919     c                   else
099600140919     c                   eval      flgboldO74  =  'N'
099700140919     c                   endif
099800140919     c                   endif
099900150617     c
100000140613if  3c                   IF        esito <> '1'                                 *no errore
100100150615     c***********        IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
100200150615     c************                 and cev(49) = ' ' and cev(50) = ' ' or
100300150615     c
100400150615     c                   if        f_tasdcm = 0 and keyforzata = *blank
100500130312     c                   exsr      srarb
100600140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
100700140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
100800140605     c                   exsr      impdsoarb
100900140604     c                   endif
101000140613     c                   endif
101100140613     c                   endif
101200141016     c                   endif
101300000000     c*
101400140530     C                   exsr      routend
101500141014      *__________________________________________________________________
101600141014     c     repfirmat     begsr
101700141014      *__________________________________________________________________
101800141014     c                   Z-ADD     f_tasaas      ka4aas
101900141014     c                   Z-ADD     f_taslnp      ka4lnp
102000141014     c                   Z-ADD     f_tasnrs      ka4nrs
102100141014     c                   Z-ADD     f_tasnsp      ka4nsp
102200141014     c                   MOVEL     '1'           ka4trc
102300141014     c     keyta4        CHAIN     tita430c                           99
102400141014if  1c                   IF        NOT *in99
102500141014     c                   MOVEL     ta4not        firmato74
102600141014e   1c                   ENDIF
102700141014     c                   endsr
102800141015      *__________________________________________________________________
102900141015     c     repfirmatARB  begsr
103000141015      *__________________________________________________________________
103100141015      *firmatario da arb
103200141015     c                   Z-ADD     arbaas        ka4aas
103300141015     c                   Z-ADD     arblnp        ka4lnp
103400141015     c                   Z-ADD     arbnrs        ka4nrs
103500141015     c                   Z-ADD     arbnsp        ka4nsp
103600141015     c                   MOVEL     '1'           ka4trc
103700141015     c     keyta4        CHAIN     fiar401l                           99
103800141015if  1c                   IF        NOT *in99
103900141015     c                   MOVEL     ar4not        firmato74
104000141015e   1c                   ENDIF
104100141015     c                   endsr
104200000000     c*--------------------------------------------------------------------------------------------*
104300000000     c* controlla validità bolla
104400000000     c*--------------------------------------------------------------------------------------------*
104500000000     c     chktas        BEGSR
104600000000     c*
104700000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
104800000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
104900000000     c                   Z-ADD     s_tasmgs      dsmgsB
105000000000if  1c                   IF        dsdatB < datpre
105100000000     c                   MOVEL     '1'           esito                          *errore
105200000000e   1c                   ENDIF
105300030130     c* se l'esito è positivo, salva alcuni dati della bolla
105400150617     c*********          EVAL      savtsp = f_tastsp
105500000000     c*
105600140718     C                   EVAL      wOrg = f_tasLna
105700051222     C                   EXSR      setLnaItaEst
105800051222     C
105900000000     c                   ENDSR
106000000000     c*--------------------------------------------------------------------------------------------*
106100000000     c* imposta i campi della DS di Output dalle bolle e files correlati
106200000000     c*--------------------------------------------------------------------------------------------*
106300140604     c     impdsotas     BEGSR
106400170301     c                   clear                   dcli
106500140604     c*
106600140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
106700140604     c                   EVAL      lnaDescO74 = sodes(i)
106800140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
106900150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
107000150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
107100150116     c                   MOVE      s_tasmgs      aasmgsO74
107200150116      *
107300140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
107400140718     c                   MOVEL     f_taslna      lineaaro74
107500140604     c*
107600140718     c                   MOVEL     f_tastbl      a1
107700140718     c                   MOVEL     f_tastbl      tpporto74
107800140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
107900140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
108000140604x   1c                   ELSE                                                   *Pagamento del
108100140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
108200140604e   1c                   ENDIF
108300140716     c                   MOVEL     f_tastsp      tpservio74
108400140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
108500140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
108600140604     C                             rpyOpCode:rpyEsito)
108700140604     C                   EVAL      corro74 = §5EDes
108800140604     c*
108900140716     c                   EVAL      destino74 = f_tasrsd
109000140716     c                   EVAL      desindo74 = f_tasind
109100140716     c                   EVAL      descapo74 = f_tascad
109200140716     c                   EVAL      desloco74 = f_taslod
109300140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
109400150326if  1c                   IF        f_tasnzd  = *blanks
109500150326     c                   EVAL      desnazo74 = f_tasprd
109600150326x   1c                   ELSE
109700150326     c***                EVAL      desnazo74 = f_tasnzd
109800150326     c***                EVAL      desnzdo74 = f_tasnzd
109900150326     c* 26/03/15: Attiviamo le specifiche per provincia export
110000150326     c                   EVAL      desnazo74 = f_tasprd
110100150326     c                   EVAL      desnzdo74 = f_tasnzd
110200150326e   1c                   ENDIF
110300140604     c*
110400140716     c                   EVAL      collio74  = f_tasncl
110500140716     c                   EVAL      pesoo74   = f_taspkf
110600140716     c                   EVAL      volumeo74 = f_tasvlf
110700160122     c***                EVAL      naturao74 = f_tasnas
110800140604     c*
110900140716     c                   EVAL      fatnumo74  = f_tasnft
111000140716     c                   MOVEL     f_tasdft      wdat
111100140604     c                   EXSR      edtdat
111200140604     c                   EVAL      fatdatao74 = wdate
111300140604     c*
111400140716     c                   EVAL      disnumo74  = f_tasndc
111500140716c    c                   MOVEL     f_tasddc      wdat
111600140604     c                   EXSR      edtdat
111700140604     c                   EVAL      disdatao74 = wdate
111800140604     **?Immagine lettera di vettura.
111900140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
112000140604     C                   IF        LDVO74 <> 'N'
112100140604     c                   CLEAR                   dtasflo
112200140716     c                   MOVEL     f_tasflo      dtasflo
112300140604     c                   IF        §floiml <> *BLANK
112400140604     c                   EVAL      ldvo74 = 'S'
112500140604     C                   EVAL      SpImLDVO74 =
112600140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
112700140716     C                             %EDITC(f_taslnp:'X') +
112800140716     C                             %EDITC(f_tasnrs:'X') +
112900140716     C                             %EDITC(f_tasnsp:'X')
113000140604     **?Reperisco il check code della spedizione POD
113100140604     C                   EVAL      PChkCdeO74 =
113200140604     C                             GetSpeChkCde(
113300140716     C                             %EDITC(f_tasaas:'X'):
113400140716     C                             %EDITC(f_taslnp:'X') +
113500140716     C                             %EDITC(f_tasnrs:'X') +
113600140716     C                             %EDITC(f_tasnsp:'X'):
113700140604     C                             ChkCode:nEsito)
113800140604     C                   ENDIF
113900140604     C                   ENDIF
114000140604     c*
114100170310     c                   MOVE      f_tasccm      alfa4
114200170301     c                   clear                   mito74
114300170310     c                   clear                   solomito74
114400170310if  1c                   IF        alfa4 <> '8888'                               *codificato
114500170301
114600170301     c* Verifico da tabella CLI quale ragione sociale mittente devo visualizzare
114700170301     c                   exsr      Tabcli
114800170301
114900170301x   1c                   ELSE                                                   *non codificato
115000170301     c                   Z-ADD     f_tasaas      kaaaas
115100170301     c                   Z-ADD     f_taslnp      kaalnp
115200170301     c                   Z-ADD     f_tasnrs      kaanrs
115300170301     c                   Z-ADD     f_tasnsp      kaansp
115400170301     c                   MOVEL     'M'           kaatrc
115500170301     c     keytaa        CHAIN     titaa30c                           99
115600170301if  2c                   IF        NOT *in99
115700170301     c                   EVAL      mito74    = taarsc
115800170301     c                   EVAL      mitindo74 = taaind
115900170301     c                   EVAL      mitcapo74 = taacap
116000170301     c                   EVAL      mitloco74 = taaloc
116100170301if  3c                   IF        taanaz = *blanks
116200170301     c                   EVAL      mitnazo74 = taaprv
116300170301x   3c                   ELSE
116400170301     c                   EVAL      mitnazo74 = taanaz
116500170301e   3c                   ENDIF
116600170301e   2c                   ENDIF
116700170301e   1c                   ENDIF
116800170301     c
116900140604     c* 2a ragione sociale destinatario.
117000140604     c                   CLEAR                   tita4000
117100140604     c                   CLEAR                   tita4p00
117200140716     c                   Z-ADD     f_tasaas      ka4aas
117300140716     c                   Z-ADD     f_taslnp      ka4lnp
117400140716     c                   Z-ADD     f_tasnrs      ka4nrs
117500140716     c                   Z-ADD     f_tasnsp      ka4nsp
117600140604     c                   MOVEL     'D'           ka4trc
117700140604     c     keyta4        CHAIN     tita430c                           99
117800140604if  1c                   IF        NOT *in99
117900140604     c                   MOVEL     ta4not        desti2O74
118000140604e   1c                   ENDIF
118100140604     c* rif partner estero
118200140604     c                   CLEAR                   tita4000
118300140604     c                   CLEAR                   tita4p00
118400140604     c                   CLEAR                   dsbl4e
118500140716     c                   Z-ADD     f_tasaas      ka4aas
118600140716     c                   Z-ADD     f_taslnp      ka4lnp
118700140716     c                   Z-ADD     f_tasnrs      ka4nrs
118800140716     c                   Z-ADD     f_tasnsp      ka4nsp
118900140604     c                   MOVEL     'E'           ka4trc
119000140604     c     keyta4        CHAIN     tita430c                           99
119100140604if  1c                   IF        NOT *in99
119200140604     c                   MOVEL     ta4not        dsbl4e
119300140604e   1c                   ENDIF
119400140604     c
119500140604     c                   CLEAR                   tita4000
119600140604     c                   CLEAR                   tita4p00
119700140604     c                   CLEAR                   dta4a
119800140716     c                   Z-ADD     f_tasaas      ka4aas
119900140716     c                   Z-ADD     f_taslnp      ka4lnp
120000140716     c                   Z-ADD     f_tasnrs      ka4nrs
120100140716     c                   Z-ADD     f_tasnsp      ka4nsp
120200140604     c                   MOVEL     'A'           ka4trc
120300140604     c     keyta4        CHAIN     tita430c                           99
120400140604if  1c                   IF        NOT *in99
120500140604     c                   MOVEL     ta4not        dta4a
120600140604e   1c                   ENDIF
120700140604     ** Riferimento mittente numerico (c'è sempre).
120800140716     C                   EVAL      rifero74 = f_tasrmn
120900140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
121000140604     C                   IF        §ta4arma <> *BLANK AND
121100140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
121200140604     C                   EVAL      rifero74a = §ta4arma
121300140604     C                   ENDIF
121400160122     c** Natura merce
121500160122     c                   EVAL      naturao74 = §ta4anas
121600140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
121700140604     C                   IF        §b4erp <> *BLANK AND
121800140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
121900140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
122000140604     C                   EVAL      rpto74 = §b4erp
122100140604     C                   ENDIF
122200140604
122300140604     **?Reperisco il check code della spedizione.
122400140604     C                   EVAL      SChkCdeO74 =
122500140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
122600140604     C                             NSpedizI74:ChkCode:nEsito)
122700140604
122800140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
122900140604     C                   IF        KSCI74 <> *BLANK
123000140604     c                   SETOFF                                           98
123100140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
123200140604     C                   RESET                   tis7700i0
123300141126     C                   EVAL      tis7700i0.aas = kasaasm
123400141126     C                   EVAL      tis7700i0.lnp = kaslnpm
123500141126     C                   EVAL      tis7700i0.nrs = kasnrsm
123600141126     C                   EVAL      tis7700i0.nsp = kasnspm
123700140604     C                   EVAL      tis7700i0.ksu = kscI74
123800140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
123900140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
124000140604     C                             : rpyEsito
124100140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
124200140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
124300140604     C                             )
124400140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
124500140604     C                             tis7700o0.bollValida = *ON)
124600140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
124700140604if  2c                   IF        NOT *in98                                      *non trovato
124800140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
124900140604     C                   reset                   TIS778DSO
125000160818     C******             EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
125100160818     c                   eval      esito='1'
125200160818     c                   eval      widmsg = 'TIS0849'
125300160818     c                   eval      widcampo = 'NSPEDIZI74'
125400160818     c                   clear                   wparm
125500160818     c                   exsr      messaggi
125600160818     c                   exsr      routend
125700160818     C******             RETURN
125800140604     C                   ENDIF
125900140604e   2c                   ENDIF
126000140604     C                   ENDIF
126100140604     c*
126200140604     ** Importo assicurato.
126300140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
126400140716     C                   EVAL      vasO74 = f_tasVas
126500141015     C                   EVAL      iasO74 = f_tasIas
126600140604     C                   ENDIF
126700140604     **
126800140604     c                   ENDSR
126900170301     c*--------------------------------------------------------------------------------------------*
127000170301     c* imposto campi del mittente da tabella CLI
127100170301     c*--------------------------------------------------------------------------------------------*
127200170301     c     tabcli        BEGSR
127300170310     c                   clear                   wusarmo           1
127400170301     C                   RESET                   tibs02ds
127500170301     C                   EVAL      t02Cod = 'CLI'
127600170301     C                   EVAL      t02Ke1 = %editc(ccmo74:'X')
127700170301     C                   EXSR      getTntbe00f
127800170310     c
127900170310     c* e mi hanno passarto una versione A o B --> significa che NON devo sostituire RSM
128000170313     c                   if        VERSIONI74<'C'
128100170310     c                   eval      t02err='E'
128200170310     c                   endif
128300170301
128400170301    0c                   IF        t02Err=' ' and t02atb=' '
128500170310    c
128600170310     c                   eval      solomito74=§CLINOIMTT
128700170310     c
128800170301     c* vedo se presente mittente originale
128900170301    1c                   if        §CLIMITOR='S'
129000170301     c* mittente originale da ARB
129100170301    2c                   if        wbol='ARB'
129200170301     c                   eval      mito74=arbrmo
129300170310
129400170310     c                   if        arbrmo<>*blanks
129500170310     c                   eval      wusarmo='S'
129600170310     c                   endif
129700170301   x2c                   else
129800170301     c                   Z-ADD     f_tasaas      kaaaas
129900170301     c                   Z-ADD     f_taslnp      kaalnp
130000170301     c                   Z-ADD     f_tasnrs      kaanrs
130100170301     c                   Z-ADD     f_tasnsp      kaansp
130200170301     c                   MOVEL     'O'           kaatrc
130300170301     c     keytaa        CHAIN     titaa30c
130400170301if  3c                   IF        %found(titaa30c)
130500170301     c                   eval      mito74=taarsc
130600170310     c                   eval      wusarmo='S'
130700170301    3c                   endif
130800170301    2c                   endif
130900170301    1c                   endif
131000170301
131100170301    1c                   if        mito74=*blanks and §CLITFNTC='S'
131200170301     c                   eval      k_NTCAPL =  'C'
131300170301     c                   eval      k_NTCNK1 =  '0151' + %editc(ccmo74:'X')
131400170301     c                   eval      k_NTCNK2 =  *BLANK
131500170301     c                   eval      k_NTCTNT =  'AM'
131600170301     c     ktfntc        chain     tfntc01l
131700170301    2c                   if        %found(tfntc01l)
131800170301     c                   eval      mito74=ntcrnt
131900170301    2c                   endif
132000170301    1c                   endif
132100170301    0c                   endif
132200170301     c
132300170301     c* questo pezzo lo faccio solo per TITAS
132400170310    1c                   if        wbol<>'ARB'
132500170301     c                   CLEAR                   bs69ds
132600170301     c                   CLEAR                   acods
132700170301     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
132800170301     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
132900170301     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
133000170301     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
133100170301     c                   PARM                    bs69ds
133200170301     c                   PARM                    acods
133300170301     c                   PARM                    indds
133400170301     c                   PARM                    clpds
133500170301     c                   PARM                    clsds
133600170310
133700170301if  2c                   IF        o69err <> '1'                                *no errore
133800170310
133900170310    1c                   if        mito74=*blanks
134000170301     c                   EVAL      mito74    = acorag
134100170310     c                   endif
134200170301     c                   EVAL      mitindo74 = indvia
134300170301     c                   EVAL      mitloco74 = indcit
134400170301     c                   MOVEL     indsta        a3
134500170301if  3c                   IF        a3 = *blanks
134600170301     c                   EVAL      mitnazo74 = indprv
134700170301x   3c                   ELSE
134800170301     c                   EVAL      mitnazo74 = a3
134900170301e   3c                   ENDIF
135000170301if  3c                   IF        indcae <> *blanks
135100170301     c                   EVAL      mitcapo74 = indcae
135200170301X   3c                   ELSE
135300170301     c                   MOVEL     indcap        a5
135400170301     c                   EVAL      mitcapo74 = a5
135500170301e   3c                   ENDIF
135600170301e   2c                   ENDIF
135700170301e   1c                   ENDIF
135800170310     c
135900170310     c* questo pezzo lo faccio solo per blp/arb
136000170310    1c                   if        mito74=*blanks and wbol= 'ARB'
136100170310     c                   EVAL      mito74    = arbrsm
136200170310     c                   endif
136300170310     c
136400170301     c                   ENDSR
136500000000     c*--------------------------------------------------------------------------------------------*
136600000000     c* imposta i campi della DS di Output dagli eventi
136700000000     c*--------------------------------------------------------------------------------------------*
136800000000     c     impdsoevb     BEGSR
136900000000     c*
137000000000     c* azzera le variabili di lavoro
137100000000     c                   CLEAR                   tote                           *contatore eventi
137200000000     c                   CLEAR                   seql                           *sequenza legami
137300000000     c                   CLEAR                   spsort                         *per ordinamento
137400000000     c                   CLEAR                   spseql                         *di memorizzazione
137500000000     c                   CLEAR                   spseqe
137600000000     c                   CLEAR                   spdevhev
137700000000     c                   CLEAR                   spdeve
137800000000     c                   CLEAR                   spheve
137900000000     c                   CLEAR                   spfle
138000000000     c                   CLEAR                   spdev
138100000000     c                   CLEAR                   spcev
138200130312     c                   CLEAR                   spspe
138300150618     c                   CLEAR                   ds7o
138400000000     c*
138500000000     c* imposta la sequenza legami per la bolla di Input
138600000000     c                   Z-ADD     1             seql                           *primo legaqme
138700000000     c*---
138800000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
138900000000     c*---
139000160127     c****
139100160127     c* Bolla presa in carico se ancora da partire
139200160420     c**                 if        esito='P'
139300160420     c**                 MOVEL     '700'         wcev                           *causale evento
139400160420     c**                 EXSR      deccevsta
139500160420if  4c**                 IF        werr <> '1'                                  *no errore
139600160420     c**                 ADD       1             tote
139700160420     c**                 MOVEL     wcdex         smdev(tote)
139800160420     c**                 exsr      evbinca
139900160420     c**                 endif
140000160420     c**                 endif
140100000000     c***
140200000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
140300000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
140400000000     c*   si trasforma nella consegna anomala della mamma
140500000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
140600000000     c***
140700000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
140800000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
140900160318     c
141000160318    0c                   if        esito<>'P'
141100000000     c                   EVAL      kblaan = s_tasaas
141200000000     c                   EVAL      kbllpn = s_taslnp
141300000000     c                   EVAL      kblnrn = s_tasnrs
141400000000     c                   EVAL      kblnsn = s_tasnsp
141500000000     c     keylbl        CHAIN     fnlbl01l                           99
141600000000if  1c                   IF        NOT *in99                                    *ha la mamma
141700000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
141800000000     c                   EVAL      kasaas = lblaap
141900000000     c                   EVAL      kaslnp = lbllpp
142000000000     c                   EVAL      kasnrs = lblnrp
142100000000     c                   EVAL      kasnsp = lblnsp
142200000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
142300000000if  2c                   IF        NOT *in99 AND
142400000000     c                             tascca <> *blanks                            *ha cons.anomala
142500060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
142600060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
142700060622     C                             :rpyEsito)
142800060622if  3c*                  IF        *in98                                        *trovata
142900060626     C                   IF        rpyOpCode = 'FOUND'
143000041222     **?Consegna anomala, POD non visualizzabile.
143100060622     C*                  IF        sapod(jj) = 'N'
143200060622     C                   IF        §7OPODImg = 'N'
143300041222     C                   EVAL      LDVO74 = 'N'
143400041222     C                   ENDIF
143500060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
143600060622     C                   IF        §7oinc <> *blanks                            *ok per internet
143700000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
143800041222e   3c                   ENDIF
143900000000e   3c                   ENDIF
144000000000e   2c                   ENDIF
144100000000e   1c                   ENDIF
144200160318e   0c                   ENDIF
144300000000     c*
144400030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
144500000000     c                   ADD       1             tote
144600060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
144700060622     c                   MOVEL     §7odei        smdev(tote)
144800030203x   3c                   ELSE                                                   *evento telef.P.O.
144900060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
145000030203e   3c                   ENDIF
145100030203e   2c                   ENDIF
145200030203     c*
145300030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
145400060622     c                   MOVEL     §7ocev        wcev                           *causale evento
145500000000     c                   EXSR      evbrit                                       *imposta evento
145600030203x   2c                   ELSE                                                   *NO sost.ritiro
145700160318     c
145800160408     c* per bolle in partenza devo impostare la data ritiro solo se spuntato in entrata
145900160408     c***                if        esito<>'P'
146000160413     c                   if        esito= 'P'
146100160413     c                   if        arbdse>0
146200160408     c                   eval      s_tasdrt=arbdrt
146300160413     c                   else
146400160413     c                   eval      s_tasdrt=00000000
146500160408     c                   endif
146600160413     c                   endif
146700160318     c
146800030203if  3c                   IF        s_tasdrt > *zeros
146900000000     c                   MOVEL     '701'         wcev                           *causale evento
147000000000     c                   EXSR      deccevsta
147100030203if  4c                   IF        werr <> '1'                                  *no errore
147200000000     c                   ADD       1             tote
147300030130     c                   MOVEL     wcdex         smdev(tote)
147400000000     c                   EXSR      evbrit                                       *imposta evento
147500030203e   4c                   ENDIF
147600030203e   3c                   ENDIF
147700030203e   2c                   ENDIF
147800160408e   2c***                ENDIF
147900140710     ** ID bolla madre.                                                          dubito figlia
148000160318     c                   if        esito='P'
148100160318     c                   EVAL      waas = arbaas
148200160318     c                   EVAL      wlnp = arblnp
148300160318     c                   EVAL      wnrs = arbnrs
148400160318     c                   EVAL      wnsp = arbnsp
148500160318     c                   else
148600160318     c
148700160318     c* questi dati per le bolle solo in partenza (non legate)
148800160318     c*  non li devo caricare
148900080131     c                   EVAL      waas = s_tasaas
149000080131     c                   EVAL      wlnp = s_taslnp
149100080131     c                   EVAL      wnrs = s_tasnrs
149200080131     c                   EVAL      wnsp = s_tasnsp
149300000000     c* Partenza merce
149400000000     c                   EVAL      lduc = s_tasduc                              *data partenza
149500000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
149600000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
149700000000     c                   EXSR      evbpar
149800000000     c* Arrivo merce
149900110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
150000110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
150100140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
150200140723     c                             gen§ar5ArrDt <> *ZERO
150300140723     c                   MONITOR
150400140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
150500140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
150600140723     c                   ON-ERROR
150700140723     c                   ENDMON
150800140723     c                   ENDIF
150900100225     c                   EVAL      llna = s_taslna                              *linea arrivo
151000150617     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
151100090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
151200090826     c                   EXSR      evbarr
151300000000     c*
151400000000     c* Consegna merce
151500000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
151600000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
151700000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
151800000000     c                   EVAL      llna = s_taslna                              *linea arrivo
151900150618     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
152000000000     c                   EXSR      evbcon
152100000000     c* Lettera anomalia
152200000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
152300000000     c                   EXSR      memdct
152400000000e   1c                   ENDIF
152500000000     c*---
152600000000     c* memorizza gli eventi dal file eventi della bolla di input
152700000000     c*---
152800160406     c* devo reimpostare la linea di arrivo della bolla richiesta, perchè potrei
152900150618     c*  aver chainato una bolla "mamma" ed aver sporcato il campo
153000150618     c                   eval      taslna=s_taslna
153100000000     c                   EXSR      memevb
153200000000     c*---
153300000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
153400000000     c*---
153500000000     c                   EXSR      elaevelbl
153600160318     c                   endif
153700000000     c*
153800000000     c* imposta contatore eventi da visualizzare nella DS di Output
153900000000     c                   EVAL      cnteveo74 = tote
154000000000     c*---
154100000000     c* ordina gli eventi per sequenza legami / Data+ora evento
154200000000     c*---
154300000000     c* compone la schiera da ordinare
154400000000do  1c     1             DO        200           j
154500000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
154600000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
154700000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
154800000000     c                   Z-ADD     smi(j)        dsi                             -indice
154900000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
155000000000e   1c                   ENDDO
155100000000     c*
155200000000     c* ordina la schiera composta
155300000000     c                   SORTA     spsort
155400000000     c*
155500000000     c* ricompone le schiere da quella ordinata
155600000000     c                   Z-ADD     *zeros        jj
155700000000do  1c     1             DO        200           j
155800000000if  2c                   IF        spsort(j) >*zeros
155900000000     c                   ADD       1             jj
156000000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
156100000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
156200000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
156300000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
156400000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
156500000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
156600000000     c                   MOVEL     smheve(dsi)   spheve(jj)
156700000000     c                   MOVEL     smfle(dsi)    spfle(jj)
156800000000     c                   MOVEL     smdev(dsi)    spdev(jj)
156900000000     c                   MOVEL     smcev(dsi)    spcev(jj)
157000130312     c                   MOVEL     smspe(dsi)    spspe(jj)
157100000000e   2c                   ENDIF
157200000000e   1c                   ENDDO
157300101104     **
157400101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
157500101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
157600101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
157700101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
157800101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
157900101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
158000101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
158100101104     ** Ricordati che l'ordine per data e ora è discendente.
158200101104     C                   EVAL      j = 0
158300101104     C                   DOU       j = 0
158400101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
158500101104     C                                        : spcev : j+1)
158600101104     C                   IF        j = 0
158700101104     C                   LEAVE
158800101104     C                   ENDIF
158900101104     C                   IF        j > 1 AND
159000101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
159100101104     C                   EVAL      dev(1) = spdeve(j-1)
159200101104     C                   EVAL      hev(1) = spheve(j-1)
159300101104     C                   EVAL      fle(1) = spfle(j-1)
159400101104     C                   EVAL      eve(1) = spdev(j-1)
159500101104     C                   EVAL      cev(1) = spcev(j-1)
159600130312     C                   EVAL      spe(1) = spspe(j-1)
159700101104     C                   EVAL      spdeve(j-1) = spdeve(j)
159800101104     C                   EVAL      spheve(j-1) = spheve(j)
159900101104     C                   EVAL      spfle(j-1) = spfle(j)
160000101104     C                   EVAL      spdev(j-1) = spdev(j)
160100101104     C                   EVAL      spcev(j-1) = spcev(j)
160200130312     C                   EVAL      spspe(j-1) = spspe(j)
160300101104     C                   EVAL      spdeve(j) = dev(1)
160400101104     C                   EVAL      spheve(j) = hev(1)
160500101104     C                   EVAL      spfle(j) = fle(1)
160600101104     C                   EVAL      spdev(j) = eve(1)
160700101104     C                   EVAL      spcev(j) = cev(1)
160800130312     C                   EVAL      spspe(j) = spe(1)
160900101104     C                   ENDIF
161000101104     C                   ENDDO
161100000000     c*
161200120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
161300120831     c                   CLEAR                   cntEveO74
161400000000do  1c     1             DO        50            j
161500120831     c                   IF        spcev(j) = *BLANK
161600120831     c                   LEAVE
161700120831     c                   ENDIF
161800000000     c                   EVAL      dev(j) = spdeve(j)
161900000000     c                   EVAL      hev(j) = spheve(j)
162000000000     c                   EVAL      fle(j) = spfle(j)
162100041007     c                   EVAL      eve(j) = spdev(j)
162200041007     c                   EVAL      cev(j) = spcev(j)
162300130312     c                   EVAL      spe(j) = spspe(j)
162400101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
162500100909     c                   CLEAR                   hev(j)
162600100909     c                   ENDIF
162700120831     c                   EVAL      cntEveO74 += 1
162800000000e   1c                   ENDDO
162900000000     c*
163000000000     c                   ENDSR
163100000000     c*--------------------------------------------------------------------------------------------*
163200000000     c* memorizza gli eventi dal file eventi della bolla di input
163300000000     c*--------------------------------------------------------------------------------------------*
163400000000     c     memevb        BEGSR
163500150617     c* il programma usa il campo TASLNA per attribuire gli eventi della bolla
163600150617     c*  usa il TITAS che ha letto per ultimo che può essere quello della bolla  mamma
163700150617     c*  o della bolla figlia
163800150617     c*  per sapere se la bolla è italia o estera non devo usare l'ultima figlia
163900150617     c*  ma lna della bolla che sta leggendo in questo momento
164000150617     c                   Z-ADD     tasLna        worg
164100150617     c                   exsr      decorg
164200150617     c                   eval      wtasfl1 =  wofl1
164300000000     c***
164400000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
164500000000     c* . se SI lo aggiorna solo nella sequenza legame
164600000000     c*   se NO lo inserisce
164700000000     c***
164800000000     c                   Z-ADD     waas          kvbaas
164900000000     c                   Z-ADD     wlnp          kvblnp
165000000000     c                   Z-ADD     wnrs          kvbnrs
165100000000     c                   Z-ADD     wnsp          kvbnsp
165200000000     c     keyevb        SETLL     fnevb01l
165300000000     c     keyevb        READE     fnevb01l                               99
165400000000do  1c                   DOW       NOT *in99
165500000000if  2c                   IF        evbatb = *blanks                             *valido
165600140108     c                   MOVEL     evbdev        wdev
165700140108     c                   MOVEL     evbhev        whev
165800080131if  3c                   IF        evbDev > *zeros
165900000000     c                   MOVEL     evbcev        wcev
166000000000     c                   EXSR      deccevsta
166100070109     c                   EXSR      eventoAnnulla
166200000000if  4c                   IF        werr <> '1'                                  *no errore
166300000000     c*
166400000000     c* controlla se evento già inserito
166500000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
166600000000     c                   MOVEL     'S'           newevb                         *nuovo evento
166700000000do  5c     1             DO        200           j
166800000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
166900080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
167000000000     c                   MOVEL     'N'           newevb                         *già inserito
167100000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
167200150618     C                   IF        wtasfl1 = ESTERO
167300150618     c                   Z-ADD     tasLna        worg
167400150618     c                   else
167500170626     c* Se presente l'aut nell'evento prendo la filiale AUT, altrimenti fle
167600170626     c                   if        %subst(evbnot:8:3)>'000'
167700170626     c                   eval      worg=%int(%subst(evbnot:8:3))
167800170626     c                   else
167900150618     c                   Z-ADD     evbfle        worg
168000150618     c                   endif
168100170626     c                   endif
168200170626
168300000000     c                   EXSR      decorg
168400000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
168500080201     c                   MOVEL     wDev          wdat
168600080201     c                   EXSR      edtdat
168700080201     c                   MOVEL     wdate         smdeve(j)
168800080201     c                   MOVEL     wHev          wora
168900080201     c                   EXSR      edtora
169000080201     c                   MOVEL     worae         smheve(j)
169100130313     c                   MOVEL     dsevbspe      smspe(j)
169200000000     c                   LEAVE                                                  *uscita ciclo
169300000000e   6c                   ENDIF
169400000000e   5c                   ENDDO
169500000000     c*
169600000000     c* incrementa contatore eventi
169700000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
169800000000     c                   ADD       1             tote
169900000000     c* indice schiera
170000000000     c                   Z-ADD     tote          smi(tote)
170100000000     c* sequenza legame
170200000000     c                   Z-ADD     seql          smseql(tote)
170300000000     c* sequenza evento
170400000000     c                   Z-ADD     020           smseqe(tote)
170500000000     c* descrizione
170600030130     c                   MOVEL     wcdex         smdev(tote)
170700000000     c* data
170800080201     c                   MOVEL     wDev          wdat
170900000000     c                   EXSR      edtdat
171000000000     c                   MOVEL     wdate         smdeve(tote)
171100000000     c* ora
171200080201     c                   MOVEL     wHev          wora
171300000000     c                   EXSR      edtora
171400000000     c                   MOVEL     worae         smheve(tote)
171500000000     c* filiale
171600150617     C***                IF        wrkLnaItaEst = ESTERO
171700150617     C                   IF        wtasfl1 = ESTERO
171800140722     c                   Z-ADD     tasLna        worg
171900140722     c                   else
172000170626     c* Se presente l'aut nell'evento prendo la filiale AUT, altrimenti fle
172100170626     c                   if        %subst(evbnot:8:3)>'000'
172200170626     c                   eval      worg=%int(%subst(evbnot:8:3))
172300170626     c                   else
172400140722     c                   Z-ADD     evbfle        worg
172500140722     c                   endif
172600170626     c                   endif
172700140722     c                   EXSR      decorg
172800140722     c                   MOVEL     wodes         smfle(tote)
172900000000     c* causale
173000000000     c                   MOVEL     evbcev        smcev(tote)
173100130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
173200130312     c* bolla legata
173300130312     c                   MOVEL     dsevbspe      smspe(tote)
173400000000     c* data+ora evento
173500080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
173600000000e   5c                   ENDIF
173700000000e   4c                   ENDIF
173800000000e   3c                   ENDIF
173900000000e   2c                   ENDIF
174000010717     c     keyevb        READE     fnevb01l                               99
174100000000e   1c                   ENDDO
174200000000     c*
174300000000     c                   ENDSR
174400000000     c*--------------------------------------------------------------------------------------------*
174500000000     c* memorizza gli "eventi fissi"
174600000000     c*--------------------------------------------------------------------------------------------*
174700000000     c     memevefix     BEGSR
174800080131     ** ID bolla in canna.
174900080131     c                   EVAL      waas = tasaas
175000080131     c                   EVAL      wlnp = taslnp
175100080131     c                   EVAL      wnrs = tasnrs
175200080131     c                   EVAL      wnsp = tasnsp
175300000000     c*
175400000000     c* Partenza merce
175500000000     c                   EVAL      lduc = tasduc                                *data partenza
175600000000     c                   EVAL      llnp = taslnp                                *filiale partenza
175700000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
175800000000     c                   EXSR      evbpar
175900000000     c* Consegna merce
176000000000     c                   EVAL      lcca = tascca                                *consegna anomala
176100000000     c                   EVAL      ldcm = tasdcm                                *data consegna
176200000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
176300000000     c                   EVAL      llna = taslna                                *linea arrivo
176400150618     c                   EVAL      ltfa = tastfa                                *linea arrivo
176500000000     c                   EXSR      evbcon
176600000000     c*
176700000000     c                   ENDSR
176800000000     c*--------------------------------------------------------------------------------------------*
176900000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
177000000000     c*--------------------------------------------------------------------------------------------*
177100000000     c     elaevelbl     BEGSR
177200000000     c*
177300000000     c* azzera le variabili di lavoro
177400000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
177500000000     c                   EVAL      deplpn = *zeros
177600000000     c                   EVAL      depnrn = *zeros
177700000000     c                   EVAL      depnsn = *zeros
177800000000     c*
177900000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
178000000000     c                   EVAL      kbllpp =  s_taslnp
178100000000     c                   EVAL      kblnrp =  s_tasnrs
178200000000     c                   EVAL      kblnsp =  s_tasnsp
178300000000     c*
178400000000     c* ciclo fino a fine mamme
178500000000     c     ke2lbl        SETLL     fnlbl02l
178600000000     c     ke2lbl        READE     fnlbl02l                               99
178700000000do  1c                   DOW       NOT *in99
178800000000     c*
178900000000     c* ciclo fino a fine figlie della mamma
179000000000do  2c                   DOW       NOT *in99
179100000000     c*
179200000000     c* legge la bolla dal legame
179300000000     c                   EVAL      kasaas = lblaan                              *figlia
179400000000     c                   EVAL      kaslnp = lbllpn
179500000000     c                   EVAL      kasnrs = lblnrn
179600000000     c                   EVAL      kasnsp = lblnsn
179700000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
179800000000if  3c                   IF        NOT *in99 AND                                *esistente
179900000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
180000000000     c                             tastbl <> 'A3' AND
180100000000     c                             tastbl <> 'F3' AND
180200000000     c                             tastbl <> 'AP' AND
180300000000     c                             tastbl <> 'FC'
180400041222     **?Immagine lettera di vettura.
180500041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
180600041222     C                   IF        LDVO74 <> 'N'
180700040330     c                   MOVEL     tasflo        dtasflo
180800040330     c                   IF        §floiml <> *BLANK
180900050325     c                   EVAL      ldvo74 = 'S'
181000040330     C                   EVAL      SpImLDVO74 =
181100040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
181200040330     C                             %EDITC(taslnp:'X') +
181300040330     C                             %EDITC(tasnrs:'X') +
181400040330     C                             %EDITC(tasnsp:'X')
181500041223     **?Reperisco il check code della spedizione POD
181600041223     C                   EVAL      PChkCdeO74 =
181700041223     C                             GetSpeChkCde(
181800041223     C                             %EDITC(tasaas:'X'):
181900041223     C                             %EDITC(taslnp:'X') +
182000041223     C                             %EDITC(tasnrs:'X') +
182100041223     C                             %EDITC(tasnsp:'X'):
182200041223     C                             ChkCode:nEsito)
182300040330     C                   ENDIF
182400041222     C                   ENDIF
182500000000     c* incrementa la sequenza legami per le bolle legate
182600000000     c                   ADD       1             seql                           *legaqme successivo
182700000000     c*---
182800000000     c* memorizza gli "eventi fissi"
182900000000     c*---
183000000000     c                   EXSR      memevefix
183100000000     c*---
183200000000     c* memorizza gli eventi
183300000000     c*---
183400000000     c                   EVAL      waas = lblaan                                *figlia
183500000000     c                   EVAL      wlnp = lbllpn
183600000000     c                   EVAL      wnrs = lblnrn
183700000000     c                   EVAL      wnsp = lblnsn
183800000000     c                   EXSR      memevb
183900000000     c*
184000000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
184100000000     c                   EVAL      depaan = lblaan                              *ultima figlia
184200000000     c                   EVAL      deplpn = lbllpn
184300000000     c                   EVAL      depnrn = lblnrn
184400000000     c                   EVAL      depnsn = lblnsn
184500000000e   3c                   ENDIF
184600000000     c*
184700000000     c* lettura successiva legami
184800000000     c     ke2lbl        READE     fnlbl02l                               99
184900000000e   2c                   ENDDO                                                  *fine figlie mamma
185000000000     c*
185100000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
185200000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
185300000000     c                             kbllpp <> deplpn OR
185400000000     c                             kblnrp <> depnrn OR
185500000000     c                             kblnsp <> depnsn
185600000000     c* nuova chiave
185700000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
185800000000     c                   EVAL      kbllpp =  deplpn
185900000000     c                   EVAL      kblnrp =  depnrn
186000000000     c                   EVAL      kblnsp =  depnsn
186100000000     c* controlla figlie
186200000000     c     ke2lbl        SETLL     fnlbl02l
186300000000     c     ke2lbl        READE     fnlbl02l                               99
186400000000if  3c                   IF        *in99                                        *non ha figlie
186500000000     c                   LEAVE                                                  *esce dal ciclo
186600000000e   3c                   ENDIF
186700000000x   2c                   ELSE                                                   *sempre stessa mamma
186800000000     c                   LEAVE                                                  *esce dal ciclo
186900000000e   2c                   ENDIF
187000000000e   1c                   ENDDO                                                  *fine mamme
187100000000     c*
187200000000     c                   ENDSR
187300160127     c*--------------------------------------------------------------------------------------------*
187400160127     c* Imposta l'evento presa in carico se bolla in partenza
187500160127     c*--------------------------------------------------------------------------------------------*
187600160127     c     evbinca       BEGSR
187700160127     c*
187800160127     c* indice schiera
187900160127     c                   Z-ADD     tote          smi(tote)
188000160127     c* sequenza legame
188100160127     c                   Z-ADD     seql          smseql(tote)
188200160127     c* sequenza evento
188300160127     c                   Z-ADD     008           smseqe(tote)                   *primo evento
188400160127     c* data
188500160127if  1c                   IF        arbdim   > *zeros
188600160127     c                   MOVEL     arbdim        wdat
188700160127     c                   EXSR      edtdat
188800160127     c                   MOVEL     wdate         smdeve(tote)
188900160127e   1c                   ENDIF
189000160127     c* filiale
189100160127     c                   Z-ADD     arblnp        worg
189200160127     c                   EXSR      decorg
189300160127     c
189400160127     c                   MOVEL     wodes         smfle(tote)
189500160127     c* causale
189600160127     c                   MOVEL     wcev          smcev(tote)
189700160127     c* data+ora evento
189800160127     c                   MOVEL     wdat          smdevhev(tote)
189900160127     c                   MOVE      wora          smdevhev(tote)
190000160127     c*
190100160127     c                   ENDSR
190200000000     c*--------------------------------------------------------------------------------------------*
190300000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
190400000000     c*--------------------------------------------------------------------------------------------*
190500000000     c     evbrit        BEGSR
190600160318     c
190700000000     c*
190800000000     c* indice schiera
190900000000     c                   Z-ADD     tote          smi(tote)
191000000000     c* sequenza legame
191100000000     c                   Z-ADD     seql          smseql(tote)
191200000000     c* sequenza evento
191300000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
191400160407     c* data ritiro solo se spuntato
191500160318     c                   if        esito='P'
191600160407     c                   if        arbdse>0
191700160318if  1c                   IF        arbdrt > *zeros
191800160318     c                   MOVEL     arbdrt        wdat
191900160318     c                   EXSR      edtdat
192000160318     c                   MOVEL     wdate         smdeve(tote)
192100160318e   1c                   ENDIF
192200160318     c* ora  da FPP
192300160318if  2c                   IF        arbfpp = 'P'                                 *PM
192400160318     c                   MOVEL     1800          wora
192500160318x   2c                   ELSE                                                   *AM
192600160318     c                   MOVEL     1200          wora
192700160318e   1c                   ENDIF
192800160318     c                   EXSR      edtora
192900160318     c                   MOVEL     worae         smheve(tote)
193000160318     c* filiale
193100160318     c                   Z-ADD     arblnp        worg
193200160318     c                   EXSR      decorg
193300160407
193400160407     c                   endif
193500160407     c
193600160318     c
193700160318     c                   else
193800160318     c
193900000000if  1c                   IF        s_tasdrt > *zeros
194000000000     c                   MOVEL     s_tasdrt      wdat
194100000000     c                   EXSR      edtdat
194200000000     c                   MOVEL     wdate         smdeve(tote)
194300000000e   1c                   ENDIF
194400000000     c* ora
194500000000if  1c                   IF        s_tashrt > *zeros
194600000000     c                   MOVEL     s_tashrt      wora
194700000000x   1c                   ELSE
194800000000if  2c                   IF        s_tasfpp = 'P'                               *PM
194900000000     c                   MOVEL     1800          wora
195000000000x   2c                   ELSE                                                   *AM
195100000000     c                   MOVEL     1200          wora
195200000000e   2c                   ENDIF
195300000000e   1c                   ENDIF
195400000000     c                   EXSR      edtora
195500000000     c                   MOVEL     worae         smheve(tote)
195600000000     c* filiale
195700000000     c                   Z-ADD     s_taslnp      worg
195800000000     c                   EXSR      decorg
195900150618     c
196000150618     c* Se la bolla è import e devo emettere l'evento di telefonare
196100150618     c*  non posso indciare la filiale esterna, ma l'hub italiano
196200150618     c                   if        wofl1=ESTERO and sostrit='S' and
196300150618     c                             §7oinc='T'
196400150618     c                   z-add     s_tastfp      worg
196500150618     c                   EXSR      decorg
196600150618     c                   endif
196700160318     c                   endif
196800160318
196900000000     c                   MOVEL     wodes         smfle(tote)
197000000000     c* causale
197100000000     c                   MOVEL     wcev          smcev(tote)
197200000000     c* data+ora evento
197300000000     c                   MOVEL     wdat          smdevhev(tote)
197400000000     c                   MOVE      wora          smdevhev(tote)
197500000000     c*
197600000000     c                   ENDSR
197700000000     c*--------------------------------------------------------------------------------------------*
197800000000     c* Imposta l'evento Partenza
197900000000     c*--------------------------------------------------------------------------------------------*
198000000000     c     evbpar        BEGSR
198100000000     c*
198200000000     c* controlla SE e COME inserire l'evento
198300000000if  1c                   IF        lduc > *zeros
198400000000     c* imposta causale
198500000000     c                   MOVEL     '702'         wcev
198600000000     c                   EXSR      deccevsta
198700000000if  2c                   IF        werr <> '1'                                  *no errore
198800000000     c* imposta filiale
198900000000     c                   Z-ADD     llnp          worg
199000000000     c                   EXSR      decorg
199100000000     c* imposta data
199200101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
199300000000     c                   MOVEL     ldrt          wdat                           *data ritiro
199400000000x   3c                   ELSE                                                   partenza ITALIA
199500000000     c                   MOVEL     lduc          wdat                           *data partenza
199600000000e   3c                   ENDIF
199700000000     c                   EXSR      edtdat
199800000000     c* imposta ora
199900000000     c                   MOVEL     2100          wora                           *ora imposta
200000000000     c                   EXSR      edtora
200100000000     c*
200200000000     c* controlla se l'evento è già stato inserito
200300000000     c                   MOVEL     'S'           newevb                         *nuovo evento
200400000000do  3c     1             DO        200           j
200500000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
200600000000     c                   MOVE      wora          lavdevhev
200700000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
200800000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
200900000000     c                   MOVEL     'N'           newevb                         *già inserito
201000000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
201100000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
201200000000     c                   LEAVE                                                  *uscita ciclo
201300000000e   4c                   ENDIF
201400000000e   3c                   ENDDO
201500000000     c*
201600000000     c* incrementa contatore eventi
201700000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
201800000000     c                   ADD       1             tote
201900000000     c* indice schiera
202000000000     c                   Z-ADD     tote          smi(tote)
202100000000     c* sequenza legame
202200000000     c                   Z-ADD     seql          smseql(tote)
202300000000     c* sequenza evento
202400000000     c                   Z-ADD     020           smseqe(tote)
202500000000     c* descrizione
202600030130     c                   MOVEL     wcdex         smdev(tote)
202700000000     c* filiale -impostata sopra-
202800000000     c                   MOVEL     wodes         smfle(tote)
202900000000     c* data    -impostata sopra-
203000000000     c                   MOVEL     wdate         smdeve(tote)
203100000000     c* ora     -impostata sopra-
203200000000     c                   MOVEL     worae         smheve(tote)
203300000000     c* causale -impostata sopra-
203400000000     c                   MOVEL     wcev          smcev(tote)
203500000000     c* data+ora evento
203600000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
203700000000     c                   MOVE      wora          smdevhev(tote)
203800000000e   3c                   ENDIF
203900000000e   2c                   ENDIF
204000000000e   1c                   ENDIF
204100000000     c*
204200000000     c                   ENDSR
204300101129     c*------------------------------------------------------------------------*
204400101129     c*
204500101129     c* Imposta l'evento Arrivo
204600101129     c*
204700101129     c*------------------------------------------------------------------------*
204800000000     c     evbarr        BEGSR
204900101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
205000101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
205100101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
205200101129     ** sommato inutile.
205300150618     C                   IF        wrkLnaItaEst = ESTERO
205400150618     C                   LEAVESR
205500150618     C                   ENDIF
205600101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
205700101129     C                   IF        lhti = *ZERO
205800101129     C                   LEAVESR
205900101129     C                   ENDIF
206000000000     c*
206100000000     c* controlla SE e COME inserire l'evento
206200000000if  1c                   IF        ldti > *zeros OR
206300000000     c                             ldcm > *zeros
206400000000     c* imposta causale
206500101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
206600000000     c                   EXSR      deccevsta
206700000000if  2c                   IF        werr <> '1'                                  *no errore
206800000000     c* imposta filiale
206900150617     c                   Z-ADD     llna          worg
207000000000     c                   EXSR      decorg
207100000000     c* imposta data
207200000000if  3c                   IF        ldti > *zeros
207300000000     c                   MOVEL     ldti          wdat                           *data arrivo
207400000000x   3c                   ELSE
207500000000     c                   MOVEL     ldcm          wdat                           *data consegna
207600000000e   3c                   ENDIF
207700000000     c                   EXSR      edtdat
207800000000     c* imposta ora
207900000000if  3c                   IF        lhti > *zeros
208000000000     c                   MOVEL     lhti          wora                           *ora arrivo
208100000000x   3c                   ELSE
208200000000     c                   MOVEL     0600          wora                           *ora imposta
208300000000e   3c                   ENDIF
208400000000     c                   EXSR      edtora
208500000000     c*
208600000000     c* controlla se l'evento è già stato inserito
208700000000     c                   MOVEL     'S'           newevb                         *nuovo evento
208800000000do  3c     1             DO        200           j
208900000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
209000000000     c                   MOVE      wora          lavdevhev
209100000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
209200000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
209300000000     c                   MOVEL     'N'           newevb                         *già inserito
209400000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
209500000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
209600000000     c                   LEAVE                                                  *uscita ciclo
209700000000e   4c                   ENDIF
209800000000e   3c                   ENDDO
209900000000     c*
210000000000     c* incrementa contatore eventi
210100000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
210200000000     c                   ADD       1             tote
210300000000     c* indice schiera
210400000000     c                   Z-ADD     tote          smi(tote)
210500000000     c* sequenza legame
210600000000     c                   Z-ADD     seql          smseql(tote)
210700000000     c* sequenza evento
210800000000     c                   Z-ADD     020           smseqe(tote)
210900000000     c* descrizione
211000030130     c                   MOVEL     wcdex         smdev(tote)
211100000000     c* data    -impostata sopra-
211200000000     c                   MOVEL     wdate         smdeve(tote)
211300000000     c* ora     -impostata sopra-
211400000000     c                   MOVEL     worae         smheve(tote)
211500000000     c* filiale -impostata sopra-
211600000000     c                   MOVEL     wodes         smfle(tote)
211700000000     c* causale -impostata sopra-
211800000000     c                   MOVEL     wcev          smcev(tote)
211900000000     c* data+ora evento
212000000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
212100000000     c                   MOVE      wora          smdevhev(tote)
212200000000e   3c                   ENDIF
212300000000e   2c                   ENDIF
212400000000e   1c                   ENDIF
212500000000     c*
212600000000     c                   ENDSR
212700000000     c*--------------------------------------------------------------------------------------------*
212800000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
212900000000     c*--------------------------------------------------------------------------------------------*
213000140714     c     evbcon        BEGSR
213100000000     c*
213200000000     c* azzera variabili di lavoro
213300000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
213400000000     c****
213500000000     c* Consegna merce ->
213600000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
213700000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
213800000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
213900000000     c*   nè lo stato di consegna nè quello di consegna anomala.
214000000000     c****
214100000000     c* controlla SE e COME inserire l'evento
214200000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
214300060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
214400060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
214500060622     C                             :rpyEsito)
214600060622     C                   IF        rpyOpCode = 'FOUND'
214700041222     **?Consegna anomala, POD non visualizzabile.
214800060622     C                   IF        §7OPODImg = 'N'
214900041222     C                   EVAL      LDVO74 = 'N'
215000041222     C                   ENDIF
215100060622     c                   IF        §7oinc <> *blanks                            *ok per internet
215200060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
215300000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
215400000000x   3c                   ELSE                                                   *ha record FNEVB
215500000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
215600000000e   3c                   ENDIF
215700041222e   3c                   ENDIF
215800000000e   2c                   ENDIF
215900000000e   1c                   ENDIF
216000080131     c* imposta data
216100080131if  1c                   IF        ldcm > *zeros
216200080131     c                   MOVEL     ldcm          wdat
216300080131e   1c                   ENDIF
216400080131     c* imposta ora
216500080131if  1c                   IF        lhmc > *zeros
216600080131     c                   MOVEL     lhmc          wora
216700080131e   1c                   ENDIF
216800000000     c* imposta causale
216900000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
217000060622     c                   MOVEL     §7ocev        wcev                           *causale evento
217100080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
217200080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
217300080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
217400080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
217500080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
217600080131     c* ritiro merce) dalla bolla figlia.
217700080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
217800080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
217900080131     c                             AND s_tasNdc < *HIVAL
218000080131     c                   EVAL      kblAap = wAas
218100080131     c                   EVAL      kblLpp = wLnp
218200080131     c                   EVAL      kblNrp = wNrs
218300080131     c                   EVAL      kblNsp = wNsp
218400080131     c     ke2lbl        CHAIN     fnlbl02l
218500080131     c                   IF        %FOUND()
218600080131     c                   EVAL      kasAas = lblAan
218700080131     c                   EVAL      kasLnp = lblLpn
218800080131     c                   EVAL      kasNrs = lblNrn
218900080131     c                   EVAL      kasNsp = lblNsn
219000080131     C     keyTas        CHAIN     titas000      titas000Figlia
219100080131     C                   IF        %FOUND()
219200080131     C                   EVAL      wDat = titas000Figlia.tasDrt
219300080131     C                   EVAL      wOra = titas000Figlia.tasHrt
219400080131     C                   ELSE
219500080131     C     keyTas        CHAIN     titas010      titas010Figlia
219600080131     C                   IF        %FOUND()
219700080131     C                   EVAL      wDat = titas010Figlia.tasDrt
219800080131     C                   EVAL      wOra = titas010Figlia.tasHrt
219900080131     C                   ELSE
220000080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
220100080131     C                   IF        %FOUND()
220200080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
220300080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
220400080131     C                   ENDIF
220500080131     C                   ENDIF
220600080131     C                   ENDIF
220700080131     c                   ENDIF
220800080131     c                   ENDIF
220900000000x   1c                   ELSE                                                   *NO sost.Consegna
221000000000     c                   MOVEL     '704'         wcev                           *causale evento
221100000000e   1c                   ENDIF
221200080131     c* Editazione data e ora evento.
221300080131     c                   EXSR      edtdat
221400080131     c                   EXSR      edtora
221500000000     c* imposta filiale
221600000000     c                   Z-ADD     llna          worg
221700000000     c                   EXSR      decorg
221800150618     c* se si tratta di lna estera e la bolla ha consegna anomala
221900150618     c*  in cui mettere msg generico, allora devo sostituire la filiale
222000150618     c*  con il suo hub
222100150618     c                   if        wofl1=estero and lcca<>' ' and
222200150618     c                             §7oinc='T'
222300150618     c                   Z-ADD     ltfa          worg
222400150618     c                   EXSR      decorg
222500150618     c                   endif
222600150618     c
222700000000     c* controlla se l'evento è già stato inserito
222800000000     c                   MOVEL     'S'           newevb                         *nuovo evento
222900000000do  1c     1             DO        200           j
223000000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
223100000000     c                   MOVE      wora          lavdevhev
223200000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
223300000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
223400000000     c                   MOVEL     'N'           newevb                         *già inserito
223500000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
223600000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
223700000000     c                   LEAVE                                                  *uscita ciclo
223800000000e   2c                   ENDIF
223900000000e   1c                   ENDDO
224000000000     c*
224100000000     c* incrementa contatore eventi
224200030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
224300030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
224400000000     c                   ADD       1             tote                           incrementa contatore
224500060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
224600060622     c                   MOVEL     §7odei        smdev(tote)
224700030203x   4c                   ELSE                                                   *evento telef.P.O.
224800060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
224900030203e   4c                   ENDIF
225000060622     c                   MOVEL     §7ocev        wcev                           *causale evento
225100000000     c                   EXSR      evbcon2                                      *imposta evento
225200030203x   3c                   ELSE                                                   *NO sost.Consegna
225300000000     c*
225400030203if  4c                   IF        ldcm > *zeros
225500030203if  5c                   IF        sostcon <> ' '                               *considera consegna
225600000000     c                   EXSR      deccevsta
225700030203if  6c                   IF        werr <> '1'                                  *no errore
225800000000     c                   ADD       1             tote                           incrementa contatore
225900030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
226000000000     c                   EXSR      evbcon2                                      *imposta evento
226100030203e   6c                   ENDIF
226200030203e   5c                   ENDIF
226300030203e   4c                   ENDIF
226400030203e   3c                   ENDIF
226500000000     c*
226600030203e   1c                   ENDIF
226700030203     c*
226800000000     c                   ENDSR
226900000000     c*--------------------------------------------------------------------------------------------*
227000000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
227100000000     c*--------------------------------------------------------------------------------------------*
227200000000     c     evbcon2       BEGSR
227300000000     c*
227400000000     c* indice schiera
227500000000     c                   Z-ADD     tote          smi(tote)
227600000000     c* sequenza legame
227700000000     c                   Z-ADD     seql          smseql(tote)
227800000000     c* sequenza evento
227900000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
228000000000     c* data    -impostata sopra-
228100000000     c                   MOVEL     wdate         smdeve(tote)
228200000000     c* ora     -impostata sopra-
228300000000     c                   MOVEL     worae         smheve(tote)
228400000000     c* filiale -impostata sopra-
228500000000     c                   MOVEL     wodes         smfle(tote)
228600000000     c* causale -impostata sopra-
228700000000     c                   MOVEL     wcev          smcev(tote)
228800000000     c* data+ora evento
228900000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
229000000000     c                   MOVE      wora          smdevhev(tote)
229100000000     c*
229200000000     c                   ENDSR
229300000000     c*--------------------------------------------------------------------------------------------*
229400000000     c* memorizza i dati della lettera di anomalia
229500000000     c*--------------------------------------------------------------------------------------------*
229600000000     c     memdct        BEGSR
229700000000     c*
229800040906     C                   RESET                   FIDN12DS
229900040906     c                   Z-ADD     waas          I12AAS
230000040906     c                   Z-ADD     wlnp          I12LNP
230100040906     c                   Z-ADD     wnrs          I12NRS
230200040906     c                   Z-ADD     wnsp          I12NSP
230300040906     C                   CALL      'FIDN12R'
230400040906     C                   PARM                    FIDN12DS
230500000000     c* memorizza la prima lettera di anomalia valida
230600040906do  1c                   IF        O12Err = *BLANK
230700040906     C                   DO        O12NCA        K
230800040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
230900040906     C                   EVAL      DCTAAC = O12KeyAAC
231000040906     C                   EVAL      DCTFIL = O12KeyFil
231100040906     C                   EVAL      DCTNCA = O12KeyNCA
231200040906     c     keydct        CHAIN     fndct01l
231300040906do  1c                   IF        %FOUND
231400000000     c*
231500000000     c* controlla validità CA
231600000000     c                   MOVEL     '0'           werr                           *NO errore
231700000000     c                   EXSR      chkdct
231800000000if  2c                   IF        werr = '0'                                   *CA valida
231900000000     c*
232000000000     c* Imposta l'evento lettera di anomalia
232100000000     c                   EXSR      evbdct
232200000000     C                   LEAVE                                                  *uscita ciclo
232300000000e   2c                   ENDIF
232400000000     c*
232500040906e   1c                   ENDIF
232600040906e   1c                   ENDDO
232700040906e   1c                   ENDIF
232800000000     C*
232900000000     c                   ENDSR
233000000000     c*--------------------------------------------------------------------------------------------*
233100000000     c* controlla validità lettera di anomalia
233200000000     c*--------------------------------------------------------------------------------------------*
233300000000     c     chkdct        BEGSR
233400000000     c*
233500000000     c* reimposta variabili di lavoro
233600000000     c                   MOVEL     '0'           werr                           *NO errore
233700000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
233800000000     c*
233900000000     c* CA annullata, esclude
234000000000if  1c                   IF        dctatb <> ' '
234100000000     c                   MOVEL     '1'           werr                           *SI errore
234200000000e   1C                   ENDIF
234300000000     c*
234400000000     c* CA non confermata, esclude
234500000000if  1c                   IF        dctfpr = ' '
234600000000     c                   MOVEL     '1'           werr                           *SI errore
234700000000e   1C                   ENDIF
234800000000     c*
234900000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
235000000000if  1c                   IF        dctfpr = 'T' AND
235100000000     c                             dctfca >= 215 OR
235200000000     c                             dctfpr = 'P' AND
235300000000     c                             dctfca >= 400
235400000000x   1c                   ELSE
235500000000     c                   MOVEL     '1'           werr                           *SI errore
235600000000e   1C                   ENDIF
235700000000     c*
235800000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
235900000000if  1c                   IF        werr = '0'
236000000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
236100060622     C                   RESET                   tibs02ds
236200060622     C                   EVAL      t02Cod = 'CCH'
236300060622     C                   EVAL      t02Ke1 = dctcch
236400060622     C                   EXSR      getTntbe00f
236500060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
236600000000     c                   MOVEL     '1'           werr                           *SI errore
236700000000e   3C                   ENDIF
236800000000e   2C                   ENDIF
236900000000e   1C                   ENDIF
237000000000     c*
237100000000     c                   ENDSR
237200000000     c*--------------------------------------------------------------------------------------------*
237300000000     c* Imposta l'evento lettera di anomalia
237400000000     c*--------------------------------------------------------------------------------------------*
237500000000     c     evbdct        BEGSR
237600000000     C*
237700000000     c* imposta causale
237800000000if  1c                   IF        wfpr = 'T'
237900000000     c                   MOVEL     '709'         wcev                           *causale evento
238000000000x   1c                   ELSE
238100000000     c                   MOVEL     '710'         wcev                           *causale evento
238200000000e   1c                   ENDIF
238300000000     c                   EXSR      deccevsta                                    *decodifica evento
238400000000     c*
238500000000     c* imposta filiale
238600000000if  1c                   IF        werr <> '1'                                  *no errore
238700000000     c                   Z-ADD     dctfil        worg
238800000000     c                   EXSR      decorg
238900000000     c* imposta data
239000000000     c                   MOVEL     dctaac        wdat
239100000000     c                   MOVE      dctmgc        wdat
239200000000     c                   EXSR      edtdat
239300000000     c* imposta ora
239400000000     c                   MOVEL     1100          wora                           *FISSA
239500000000     c                   EXSR      edtora
239600000000     c*
239700000000     c* incrementa contatore eventi
239800000000     c                   ADD       1             tote
239900000000     c* indice schiera
240000000000     c                   Z-ADD     tote          smi(tote)
240100000000     c* sequenza legame
240200000000     c                   Z-ADD     seql          smseql(tote)
240300000000     c* sequenza evento
240400000000     c                   Z-ADD     999           smseqe(tote)
240500000000     c* descrizione -impostata sopra-
240600030130     c                   MOVEL     wcdex         smdev(tote)
240700000000     c* filiale -impostata sopra-
240800000000     c                   MOVEL     wodes         smfle(tote)
240900000000     c* data    -impostata sopra-
241000000000     c                   MOVEL     wdate         smdeve(tote)
241100000000     c* ora     -impostata sopra-
241200000000     c                   MOVEL     worae         smheve(tote)
241300000000     c* causale -impostata sopra-
241400000000     c                   MOVEL     wcev          smcev(tote)
241500000000     c* data+ora evento
241600000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
241700000000     c                   MOVE      wora          smdevhev(tote)
241800000000e   1c                   ENDIF
241900000000     c*
242000000000     c                   ENDSR
242100000000     c*--------------------------------------------------------------------------------------------*
242200000000     c* imposta i campi della DS di Output dal contrassegno
242300000000     c*--------------------------------------------------------------------------------------------*
242400000000     c     impdsocsb     BEGSR
242500000000     c*
242600000000     c* azzera le variabili di lavoro
242700000000     c*
242800000000     c****
242900000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
243000000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
243100000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
243200000000     c****
243300000000     c*
243400000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
243500090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
243600000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
243700000000x   1c                   ELSE                                                     *trovato
243800000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
243900000000e   1C                   ENDIF
244000000000     c*---
244100000000     c* memorizza il contrassegno della bolla di Input
244200000000     c*---
244300000000     c                   EVAL      waas = s_tasaas                              *bolla Input
244400000000     c                   EVAL      wlnp = s_taslnp
244500000000     c                   EVAL      wnrs = s_tasnrs
244600000000     c                   EVAL      wnsp = s_tasnsp
244700000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
244800000000     c                   EXSR      memcsb
244900000000     c*
245000000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
245100000000     c                   EXSR      elacsblbl
245200000000     c*
245300000000     c                   ENDSR
245400000000     c*--------------------------------------------------------------------------------------------*
245500000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
245600000000     c*--------------------------------------------------------------------------------------------*
245700000000     c     elacsblbl     BEGSR
245800000000     c*
245900000000     c* azzera le variabili di lavoro
246000000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
246100000000     c                   EVAL      deplpn = *zeros
246200000000     c                   EVAL      depnrn = *zeros
246300000000     c                   EVAL      depnsn = *zeros
246400000000     c*
246500000000     c* legge i legami delle figlie successive alla bolla di input
246600000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
246700000000     c                   EVAL      kbllpp =  s_taslnp
246800000000     c                   EVAL      kblnrp =  s_tasnrs
246900000000     c                   EVAL      kblnsp =  s_tasnsp
247000000000     c*
247100000000     c* ciclo fino a fine mamme
247200000000     c     ke2lbl        SETLL     fnlbl02l
247300000000     c     ke2lbl        READE     fnlbl02l                               99
247400000000do  1c                   DOW       NOT *in99
247500000000     c*
247600000000     c* ciclo fino a fine figlie della mamma
247700000000do  2c                   DOW       NOT *in99
247800000000     c*
247900000000     c* legge la bolla dal legame
248000000000     c                   EVAL      kasaas = lblaan                              *figlia
248100000000     c                   EVAL      kaslnp = lbllpn
248200000000     c                   EVAL      kasnrs = lblnrn
248300000000     c                   EVAL      kasnsp = lblnsn
248400000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
248500000000     c*---
248600000000     c* memorizza il contrassegno della bolla figlia legata
248700000000     c*---
248800000000if  4c                   IF        NOT *in99                                    *esistente
248900000000     c                   EVAL      waas = lblaan                                *figlia
249000000000     c                   EVAL      wlnp = lbllpn
249100000000     c                   EVAL      wnrs = lblnrn
249200000000     c                   EVAL      wnsp = lblnsn
249300000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
249400000000     c                   EXSR      memcsb
249500000000e   4c                   ENDIF
249600000000     c*
249700000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
249800000000     c                   EVAL      depaan = lblaan                              *ultima figlia
249900000000     c                   EVAL      deplpn = lbllpn
250000000000     c                   EVAL      depnrn = lblnrn
250100000000     c                   EVAL      depnsn = lblnsn
250200000000     c*
250300000000     c* lettura successiva legami
250400000000     c     ke2lbl        READE     fnlbl02l                               99
250500000000e   2c                   ENDDO                                                  *fine figlie mamma
250600000000     c*
250700000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
250800000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
250900000000     c                             kbllpp <> deplpn OR
251000000000     c                             kblnrp <> depnrn OR
251100000000     c                             kblnsp <> depnsn
251200000000     c* nuova chiave
251300000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
251400000000     c                   EVAL      kbllpp =  deplpn
251500000000     c                   EVAL      kblnrp =  depnrn
251600000000     c                   EVAL      kblnsp =  depnsn
251700000000     c* controlla figlie
251800000000     c     ke2lbl        SETLL     fnlbl02l
251900000000     c     ke2lbl        READE     fnlbl02l                               99
252000000000if  3c                   IF        *in99                                        *non ha figlie
252100000000     c                   LEAVE                                                  *esce dal ciclo
252200000000e   3c                   ENDIF
252300000000x   2c                   ELSE                                                   *sempre stessa mamma
252400000000     c                   LEAVE                                                  *esce dal ciclo
252500000000e   2c                   ENDIF
252600000000e   1c                   ENDDO                                                  *fine mamme
252700000000     c*
252800000000     c                   ENDSR
252900000000     c*--------------------------------------------------------------------------------------------*
253000000000     c* memorizza il contrassegno
253100000000     c*--------------------------------------------------------------------------------------------*
253200000000     c     memcsb        BEGSR
253300000000     c*
253400000000     c* legge il contrassegno, se avuto
253500000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
253600000000     c                   Z-ADD     waas          ksbaas
253700000000     c                   Z-ADD     wlnp          ksblnp
253800000000     c                   Z-ADD     wnrs          ksbnrs
253900000000     c                   Z-ADD     wnsp          ksbnsp
254000000000     c     keycsb        CHAIN     tncsb03l                           99
254100000000if  2c                   IF        NOT *in99                                    *esistente
254200000000     c* memorizza i dati
254300000000     c                   MOVEL     'S'           cao74                          *esiste C/A
254400000000     c*
254500000000     c                   Z-ADD     csbcas        caimpo74                       *importo
254600000000     c*
254700000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
254800010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
254900010820     c                             caimpo74 > *zeros
255000010820     c                   EVAL      cadivo74 = 'ITL'
255100010820e   3c                   ENDIF
255200160314
255300160314     ** Visualizzo la descrizione del tipo incasso
255400100121     C                   EVAL      cainco74 =
255500100121     C                                       GetDescrizioneIncassoContrassegno()
255600000000     c*
255700000000     c                   MOVEL     *blanks       castao74                       *stato
255800060622     C                   EVALR     tblKey = %CHAR(csbSta)
255900060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
256000060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
256100060622     C                             :rpyOpCode:rpyEsito)
256200060622     C                   IF        rpyOpCode = 'FOUND'
256300060622     c                   EVAL      castao74 = §4SDei
256400000000e   3c                   ENDIF
256500100119     c
256600000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
256700011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
256800060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
256900000000x   4c                   ELSE
257000000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
257100060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
257200000000x   5c                   ELSE
257300060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
257400000000e   5c                   ENDIF
257500000000e   4c                   ENDIF
257600000000e   3c                   ENDIF
257700000000     c*
257800000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
257900000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
258000000000if  4c                   IF        csbtpa = 'B'
258100060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
258200000000e   4c                   ENDIF
258300000000if  4c                   IF        csbtpa = 'C'
258400060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
258500000000e   4c                   ENDIF
258600000000x   3c                   ELSE                                                   *incasso Bartolini
258700000000if  4c                   IF        csbfpc = *blanks
258800110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
258900000000e   4c                   ENDIF
259000000000if  4c                   IF        csbfpc = '2'
259100060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
259200000000e   4c                   ENDIF
259300000000e   3c                   ENDIF
259400000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
259500000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
259600000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
259700000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
259800000000     c                             csbfpc = *blanks                              assegno di traenza
259900060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
260000000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
260100060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
260200000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
260300060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
260400000000e   4c                   ENDIF
260500000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
260600060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
260700060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
260800000000e   3c                   ENDIF
260900000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
261000000000     c                   Z-ADD     *zeros        caabio74                       *abi
261100000000     c                   Z-ADD     *zeros        cacabo74                       *cab
261200000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
261300000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
261400000000     c                   MOVEL     csbddp        wdat
261500000000     c                   EXSR      edtdat
261600000000     c                   EVAL      cadato74 = wdate
261700000000     C*
261800000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
261900000000     c                   Z-ADD     csbabi        caabio74
262000000000     c                   Z-ADD     csbcab        cacabo74
262100000000     c                   MOVEL     csbnra        canumo74
262200120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
262300120321     c                             : caCabO74)
262400000000x   4c                   ELSE                                                   *incasso Bartolini
262500000000     c                   MOVEL     csbndp        canumo74
262600000000e   4c                   ENDIF
262700000000e   3c                   ENDIF
262800110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
262900110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
263000110307     C                   IF        IsContrassegnoCompensato()
263100110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
263200110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
263300110304     C                   CLEAR                   caabio74
263400110304     C                   CLEAR                   cacabo74
263500110304     C                   CLEAR                   canumo74
263600110304     C                   ENDIF
263700110304     C*
263800000000e   2c                   ENDIF
263900000000e   1c                   ENDIF
264000000000     c*
264100000000     c                   ENDSR
264200000000     c*--------------------------------------------------------------------------------------------*
264300000000     c* imposta i campi della DS di Output dalla giacenza
264400000000     c*--------------------------------------------------------------------------------------------*
264500000000     c     impdsogcp     BEGSR
264600000000     c*
264700000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
264800000000     c                   EVAL      waas = s_tasaas                              *bolla Input
264900000000     c                   EVAL      wlnp = s_taslnp
265000000000     c                   EVAL      wnrs = s_tasnrs
265100000000     c                   EVAL      wnsp = s_tasnsp
265200000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
265300000000     c                   EXSR      memgcp
265400000000     c*
265500000000     c                   ENDSR
265600000000     c*--------------------------------------------------------------------------------------------*
265700000000     c* memorizza la giacenza
265800000000     c*--------------------------------------------------------------------------------------------*
265900000000     c     memgcp        BEGSR
266000000000     c*
266100000000     c* imposta le variabili di lavoro
266200000000     c                   CLEAR                   i                              *indice
266300000000     c                   CLEAR                   ii
266400000000     c                   CLEAR                   smdmc                          *note fase -20-
266500000000     c                   CLEAR                   smdmcR
266600000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
266700000000     c                   MOVEL     'N'           gco74                          *flag di controllo
266800000000     c                   MOVEL     'N'           gcfdio74
266900000000     c                   MOVEL     'N'           gcmito74
267000000000     c*
267100000000     c* legge la giacenza, se avuta
267200000000     c                   Z-ADD     waas          kcpaas
267300000000     c                   Z-ADD     wlnp          kcplnp
267400000000     c                   Z-ADD     wnrs          kcpnrs
267500000000     c                   Z-ADD     wnsp          kcpnsp
267600000000     c                   Z-ADD     *zeros        kcpfrg
267700050309     c     keygcp        CHAIN     tigcp51l                           99
267800050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
267900050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
268000000000if  2c                   IF        NOT *in99        AND                         *esistente
268100000000     c                             gcpatb = *blanks AND                         *no annullata
268200050415     c                             gcptcm <> 'Z'    AND
268300050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
268400050419     C
268500050419     C                   EVAL      gcRiapO74 = gcpRiap
268600050418     **?Descrizione della fase.
268700050418     C                   SELECT
268800050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
268900060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
269000050418     C                   WHEN      gcpFas = 20
269100060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
269200050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
269300060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
269400050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
269500060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
269600050418     C                   ENDSL
269700000000     c* memorizza i dati dalle NOTE GIACENZE
269800000000     c                   Z-ADD     *zeros        i
269900000000     c                   Z-ADD     *zeros        ii
270000000000     c                   Z-ADD     gcpagc        knpagc
270100000000     c                   Z-ADD     gcpfgc        knpfgc
270200000000     c                   Z-ADD     gcpngc        knpngc
270300000000     c                   Z-ADD     gcpfrg        knpfrg
270400050309     c     keygnp        SETLL     tignp01l
270500050309     c     keygnp        READE     tignp01l                               98
270600000000do  3c                   DOW       NOT *in98
270700050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
270800000000     c* fase -10-
270900000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
271000000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
271100000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
271200000000e   5c                   ENDIF
271300000000     c* fase -20-
271400000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
271500000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
271600051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
271700000000     c                   ADD       1             i
271800000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
271900000000e   6c                   ENDIF
272000000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
272100051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
272200050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
272300050405     c                   CLEAR                   ii
272400050405     c                   CLEAR                   smdmcR
272500050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
272600050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
272700050919     C                   IF        NOT %ERROR
272800140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
272900050919     C                   ENDIF
273000050405     C                   ELSE
273100000000     c                   ADD       1             ii
273200000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
273300050405     C                   ENDIF
273400000000e   6c                   ENDIF
273500000000e   5c                   ENDIF
273600000000e   4c                   ENDIF
273700050309     c     keygnp        READE     tignp01l                               98
273800000000e   3c                   ENDDO
273900000000     c*
274000000000     c* memorizza i dati dalle GIACENZE
274100000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
274200050404     c* Disposizione da immettere.
274300051110     c                   IF        gcpfas = 20
274400051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
274500051110     C                   EXSR      chkTivpi
274600051110e   3c                   ENDIF
274700000000     c*
274800000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
274900000000     c*
275000000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
275100000000     c*
275200060103     c                   Z-ADD     gcpagc        dsannB
275300060103     c                   Z-ADD     gcpmgc        dsmgsB
275400060103     c                   Z-ADD     dsdatB        wdat
275500000000if  3c                   IF        wdat > *zeros
275600000000     c                   EXSR      edtdat
275700000000     c                   EVAL      gcdago74 = wdate                             *data apertura
275800000000e   3C                   ENDIF
275900060104     c*
276000060104     c                   Z-ADD     gcpdur        wdat
276100060104     c                   IF        wdat > *zeros
276200060104     c                   EXSR      edtdat
276300060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
276400060104     C                   ENDIF
276500000000     c*
276600000000if  3c                   IF        gcpcmc <> *blanks
276700000000     c                   MOVEL     gcpcmc        wcev
276800000000     c                   EXSR      deccevgia
276900030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
277000000000e   3C                   ENDIF
277100000000     c*
277200000000     c                   MOVEL     *blanks       gcdcgo74
277300000000if  3c                   IF        gcpdcg > *zeros
277400000000     c                   Z-ADD     gcpdcg        wdat
277500000000     c                   EXSR      edtdat
277600000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
277700000000e   3c                   ENDIF
277800000000     c*---
277900000000     c* Disposizioni già immesse
278000000000     c*---
278100000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
278200000000     c*
278300000000if  4c                   IF        gcpddm > *zeros
278400000000     c                   Z-ADD     gcpddm        wdat
278500000000     c                   EXSR      edtdat
278600000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
278700000000e   4C                   ENDIF
278800000000     c*
278900000000if  4c                   IF        gcpdis <> *blanks
279000060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
279100060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
279200060626     C                             :rpyOpCode:rpyEsito)
279300060622     C                   EVAL      gcdiso74 = §2DDes
279400000000e   4C                   ENDIF
279500000000     c*
279600000000if  4c                   IF        gcpasg <> *blanks
279700000000if  5c                   IF        gcpasg = 'S'
279800060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
279900000000x   5c                   ELSE
280000060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
280100000000e   5C                   ENDIF
280200000000     c*
280300000000if  5c                   IF        gcppsg = 'M'
280400060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
280500000000e   5C                   ENDIF
280600000000if  5c                   IF        gcppsg = 'D'
280700060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
280800000000e   5C                   ENDIF
280900000000e   4C                   ENDIF
281000000000     c*
281100000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
281200000000     c                   MOVEL     gcpvcs        gcvcso74
281300000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
281400000000     c                   Z-ADD     gcpcas        gccaso74
281500000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
281600000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
281700000000e   5C                   ENDIF
281800000000e   4C                   ENDIF
281900050404     C                   IF        smdmc(1) <> *BLANK
282000050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
282100050404     C                             smdmc(3) + smdmc(4)
282200051014     C                   EVAL      gcdmc5O74 = smdmc(5)
282300050404     C                   ELSE
282400050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
282500050404     C                             smdmcr(3) + smdmcr(4)
282600051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
282700050404e   3C                   ENDIF
282800000000e   3C                   ENDIF
282900000000     c*---
283000000000     c* Disposizioni da immettere
283100000000     c*---
283200010719     c* se le disposizioni sono da immettere controlla che:
283300010719     c* - il cliente è il mittente della giacenza
283400000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
283500170315     C* CON KEY GIAC --> SEMPRE SI POSSONO IMMETTERE
283600170315     C                   SELECT
283700170315if  4C                   WHEN      keygiac <> *blanks
283800170315     c                   MOVEL     'S'           gcmito74                       SI POSSO IMMETTERE
283900170315     c
284000170315     C* CON BRTCODE  --> richiamo pgm fnlgw00r
284100170315if  4C                   WHEN      wbrtcode <> *blanks
284200170315     c                   exsr      chiamalgw
284300170315x   4c                   other
284400170315     c* per gli altri casi --> o loggato o anonimo ( gesrtito dal pgm seguente)
284500090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
284600090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
284700090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
284800090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
284900090914     C                             )
285000170315e   4C                   ENDsl
285100000000e   3C                   ENDIF
285200050404     C* Controllo esistenza storia giacenza.
285300050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
285400050404     C                   DO        *HIVAL
285500050404     C     K04GCP51      READE     TIGCP51L
285600050404     C                   IF        %EOF
285700050404     C                   LEAVE
285800050404     C                   ENDIF
285900050404     C                   IF        GCPATB = *BLANK
286000050404     C                   EVAL      StoGiaO74 = 'S'
286100050404     C                   LEAVE
286200050404     C                   ENDIF
286300050404     C                   ENDDO
286400050404     c*
286500000000e   2c                   ENDIF
286600000000     c*
286700000000     c                   ENDSR
286800170315     c*--------------------------------------------------------------------------------------------*
286900170315     c* richiamo pgm di verifica giacenze con collegamento con BRTCODE
287000170315     c*--------------------------------------------------------------------------------------------*
287100170315     c     Chiamalgw     BEGSR
287200170315
287300170315      /free
287400170315       reset  fnlgw00i0   ;
287500170315       reset  fnlgw00o0   ;
287600170315       fnlgw00i0.LINISO2=LANGI74    ;
287700170315       fnlgw00i0.tipoacc='B'         ;
287800170315       fnlgw00i0.brtcode=wbrtcode    ;
287900170315         prwrqsData = fnlgw00i0 ;
288000170315         prwrqsDatasize = %SIZE(fnlgw00i0) ;
288100170315         prwrpyDatasize = %SIZE(fnlgw00o0) ;
288200170315         prwrpyformato='         '  ;
288300170315         prwRpyFormMsg='FNLGW00M0'  ;
288400170316         prwrqsFormato='FNLGW00I0'  ;
288500170316         prwrpyFormato='FNLGW00O0'  ;
288600170315         prwRpyMsgSize  = %SIZE(fnlgw00m0);
288700170315         fnlgw00r(prwRqsOpCode : prwRpyOpCode : prwRpyIdMsg :
288800170315                prwRqsFormato : prwRqsData : prwRqsDataSize:
288900170315                prwRpyFormato : prwRpyData : prwRpyDataSize:
289000170315                prwRpyFormMsg : prwRpyMessage : prwRpyMsgSize)  ;
289100170315
289200170315         if   prwrpyOpCode = FNLGW00_RPYOPCODE_done   ;
289300170315         fnlgw00o0=prwrpydata  ;
289400170315         if fnlgw00o0.butgen=*on   ;
289500170315
289600170315         GCMITO74='S'  ;
289700170315
289800170315         endif    ;
289900170315         endif    ;
290000170315
290100170315      /end-free
290200170315
290300170315     c                   ENDSR
290400000000     c*--------------------------------------------------------------------------------------------*
290500000000     c* imposta i campi della DS di Output dalle note
290600000000     c*--------------------------------------------------------------------------------------------*
290700000000     c     impdsonot     BEGSR
290800000000     c*
290900000000     c* azzera le variabili di lavoro
291000000000     c                   CLEAR                   i
291100000000     c                   CLEAR                   mdcre                          consegna richiesta
291200000000     c                   CLEAR                   mhcre
291300030210     c                   CLEAR                   mtcre
291400000000     c                   CLEAR                   mftce                          consegna particolare
291500000000     c                   CLEAR                   wftc
291600000000     c                   CLEAR                   d1wdftc
291700000000     c                   CLEAR                   d2wdftc
291800000000     c                   CLEAR                   mgcxe                          turni chiusura
291900000000     c                   CLEAR                   wgcx
292000000000     c                   CLEAR                   d1wdgcx
292100000000     c                   CLEAR                   d2wdgcx
292200010927     c                   CLEAR                   mffd                           fermo deposito
292300010927     c                   CLEAR                   mmncB                          *motivi NON consegna
292400010927     c                   CLEAR                   mmncC
292500010927     c                   CLEAR                   mmncG
292600010927     c                   CLEAR                   mmncH
292700010927     c                   CLEAR                   mmncN
292800010927     c                   CLEAR                   mmncO
292900050701     c                   CLEAR                   mmnc8
293000050701     c                   CLEAR                   mmnc9
293100050701     c                   CLEAR                   a230
293200000000     c                   CLEAR                   we
293300030206     c                   CLEAR                   mtel                           n° tel destinatario
293400030206     c                   CLEAR                   mban                           bancali
293500030207     c                   CLEAR                   mscr                           supermercati
293600030210     c                   CLEAR                   msdace
293700030210     c                   CLEAR                   msorce
293800030210     c                   CLEAR                   msdcre
293900030210     c                   CLEAR                   mshcre
294000030210     c                   CLEAR                   mstcre
294100030210     c                   CLEAR                   msnom
294200030210     c                   CLEAR                   mscrT
294300030210     c                   CLEAR                   wmscrT
294400030210     c                   CLEAR                   s
294500030207     c
294600160127     c*imposta dati data consegna richiesta  DA BLP se bolla in partenza
294700160127     c                   if        esito='P'
294800160127     c                   eval      f_tasdcr=arbdcr
294900160127     c                   eval      f_tastcr=arbtcr
295000160127     c                   eval      f_tashcr=arbhcr
295100160127     c* imposto anche key spedizione
295200160309     c                   eval      f_tasaas=arbaas
295300160309     c                   eval      f_taslnp=arblnp
295400160309     c                   eval      f_tasnrs=arbnrs
295500160309     c                   eval      f_tasnsp=arbnsp
295600160127     c                   endif
295700160127     c
295800140716     c                   if        f_tasdcr > 0
295900140716     c                   MOVEL     f_tasdcr      wdat
296000140619     c                   EXSR      edtdat
296100140619     c                   eval      DTCONRCO74  =  wdate
296200140819if  3c                   IF        f_tastcr = ' '
296300140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
296400140819e   3c                   ENDIF
296500140819     c                   endif
296600140716     c                   if        f_tashcr > 0
296700140716     c                   MOVEL     f_tashcr      wora
296800140619     c                   EXSR      edtora
296900140619     c                   eval      ORACONRO74  =  worae
297000140619     c                   endif
297100140716     c                   eval      TPDATCRO74  =  f_tastcr
297200140619      *reperimento descrizione tipo consegna richiesta
297300140716if  3c                   IF        f_tastcr = 'P'
297400140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
297500140619e   3c                   ENDIF
297600140716if  3c                   IF        f_tastcr = 'D'
297700140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
297800140619e   3c                   ENDIF
297900000000     c*---
298000000000     c* memorizza le "note" che sono presenti nel record bolla
298100000000     c*---
298200000000     c* Consegna richiesta
298300140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
298400140619     c*m                           s_tashcr = *zeros
298500140619x   1c*m                 ELSE                                                   *SI cons.particolare
298600140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
298700140619     c*m                 MOVEL     s_tasdcr      wdat
298800140619     c*m                 EXSR      edtdat                                       *edita data
298900140619     c*m                 MOVEL     wdate         mdcre
299000140619e   2c*m                 ENDIF
299100140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
299200140619     c*m                 MOVEL     s_tashcr      wora
299300140619     c*m                 EXSR      edtora                                       *edita ora
299400140619     c*m                 MOVEL     worae         mhcre
299500140619e   2c*m                 ENDIF
299600140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
299700140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
299800140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
299900140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
300000140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
300100140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
300200140619e   3c*m                 ENDIF
300300140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
300400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
300500140619e   3c*m                 ENDIF
300600140619if  3c*m                 IF        s_tastcr = ' '                               *il
300700140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
300800140619e   3c*m                 ENDIF
300900140619e   2c*m                 ENDIF
301000140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
301100140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
301200140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
301300140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
301400140619e   3c*m                 ENDIF
301500140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
301600140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
301700140619e   3c*m                 ENDIF
301800140619if  3c*m                 IF        s_tastcr = ' '                               *il
301900140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
302000140619e   3c*m                 ENDIF
302100140619e   2c*m                 ENDIF
302200140619     c*m
302300140619if  2c*m                 IF        i < 10                                       *se c'è spazio
302400140619     c*m                 ADD       1             i                              *memorizza nota
302500140619     c*m                 EVAL      smnot(i) =
302600140619     c*m                           %TRIM(mtcre) +
302700140619     c*m                           ' '          +
302800140619     c*m                           %TRIM(mdcre) +
302900140619     c*m                           ' '          +
303000140619     c*m                           %TRIM(mhcre)
303100140619e   2c*m                 ENDIF
303200140619e   1c*m                 ENDIF
303300000000     c*
303400000000     c* Particolarità consegna -uno/due-
303500160127     c                   if        esito='P'
303600160309     c                   eval      f_tasftc = arbtc1
303700160314     c                   eval      f_tastc2 = arbtc2
303800160127     c                   endif
303900160127     c
304000140716if  1c                   IF        f_tasftc <> *blanks OR
304100140716     c                             f_tastc2 <> *blanks
304200140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
304300000000     c                   EXSR      decftc
304400000000     c                   MOVEL     wdftc         d1wdftc
304500140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
304600000000     c                   EXSR      decftc
304700000000     c                   MOVEL     wdftc         d2wdftc
304800000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
304900000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
305000060612     c                   EVAL      we = ' ' +
305100060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
305200000000x   2c                   ELSE
305300000000     c                   EVAL      we = *blanks
305400000000e   2c                   ENDIF
305500060612     c                   EVAL      mftce =
305600060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
305700060612     c                             %TRIM(d1wdftc)             +
305800060612     c                             we                         +
305900060612     c                             %TRIM(d2wdftc)
306000000000     c*
306100010927if  2c                   IF        i < 10                                       *se c'è spazio
306200000000     c                   ADD       1             i                              *memorizza nota
306300000000     c                   EVAL      smnot(i) = mftce
306400010927e   2c                   ENDIF
306500010927e   1c                   ENDIF
306600000000     c*
306700000000     c* Chiusura per turno
306800160127     c                   if        esito='P'
306900160309     c                   eval      f_tasgc1 = arbgc1
307000160309     c                   eval      f_tasgc1 = arbgc2
307100160127     c                   endif
307200160127     c
307300140716if  1c                   IF        f_tasgc1 <> *blanks OR
307400140716     c                             f_tasgc2 <> *blanks
307500140716if  2c                   IF        f_tasgc1 <> *blanks
307600140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
307700000000     c                   EXSR      decgcx
307800000000     c                   MOVEL     wdgcx         d1wdgcx
307900000000e   2c                   ENDIF
308000140716if  2c                   IF        f_tasgc2 <> *blanks
308100140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
308200000000     c                   EXSR      decgcx
308300000000     c                   MOVEL     wdgcx         d2wdgcx
308400000000e   2c                   ENDIF
308500000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
308600000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
308700060612     c                   EVAL      we = ' ' +
308800060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
308900000000x   2c                   ELSE
309000000000     c                   EVAL      we = *blanks
309100000000e   2c                   ENDIF
309200060612     c                   EVAL      mgcxe =
309300060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
309400060612     c                             %TRIM(d1wdgcx)       +
309500060612     c                             we                   +
309600060612     c                             %TRIM(d2wdgcx)
309700000000     c*
309800010927if  2c                   IF        i < 10                                       *se c'è spazio
309900000000     c                   ADD       1             i                              *memorizza nota
310000000000     c                   EVAL      smnot(i) = mgcxe
310100010927e   2c                   ENDIF
310200010927e   1c                   ENDIF
310300000000     c*
310400000000     c* Fermo deposito
310500160127     c                   if        esito='P'
310600160309     c                   eval      f_tasffd = arbffd
310700160127     c                   endif
310800160127
310900140716if  1c                   IF        f_tasffd = 'S'
311000060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
311100000000     c*
311200010927if  2c                   IF        i < 10                                       *se c'è spazio
311300010927     c                   ADD       1             i                              *memorizza nota
311400010927     c                   EVAL      smnot(i) = mffd
311500010927e   2c                   ENDIF
311600010927e   1c                   ENDIF
311700030206     c*---
311800030206     c* memorizza le "note" che NON sono presenti nel record bolla
311900030206     c* ma sul file estensione "anagrafiche"
312000030206     c*---
312100030206     c* N° telefono destinatario
312200160127     c                   if        esito='P'
312300160309     C                   EVAL      MitOrigO74 = arbrmo
312400160127     c                   else
312500140716     c                   Z-ADD     f_tasaas      kaaaas
312600140716     c                   Z-ADD     f_taslnp      kaalnp
312700140716     c                   Z-ADD     f_tasnrs      kaanrs
312800140716     c                   Z-ADD     f_tasnsp      kaansp
312900030206     c                   MOVEL     'O'           kaatrc
313000030206     c     keytaa        CHAIN     titaa30c                           98
313100030206if  1c                   IF        NOT *in98
313200040330     C                   EVAL      MitOrigO74 = TAARSC
313300030206e   1c                   ENDIF
313400160127e   1c                   ENDIF
313500170310     c*
313600170310     c* se ho già usato il mitt originale come RSM --> pulisco il campo
313700170310     c                   if        wusarmo='S'
313800170310     c                   clear                   MitOrigO74
313900170310     c                   endif
314000030206     c*---
314100030206     c* memorizza le "note" che NON sono presenti nel record bolla
314200030206     c* ma sul file estensione testata boolle
314300030206     c*---
314400030206     c* Bancali
314500140716     c                   Z-ADD     f_tasaas      kr5aas
314600140716     c                   Z-ADD     f_taslnp      kr5lnp
314700140716     c                   Z-ADD     f_tasnrs      kr5nrs
314800140716     c                   Z-ADD     f_tasnsp      kr5nsp
314900030206     c                   MOVEL     'BAN'         kr5trd
315000160127     c                   if        esito='P'
315100160127     c     keyar5        CHAIN     fiar501l                           98
315200160127     c                   else
315300040531     c     keyar5        CHAIN     fiar531c                           98
315400160127     c                   endif
315500160127     c
315600030206if  1c                   IF        NOT *in98
315700030206     c                   MOVEL     ar5uni        dar5ban
315800060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
315900060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
316000030206if  2c                   IF        §ar5rb1 > *zeros OR
316100030206     c                             §ar5rb2 > *zeros
316200060613     c                   EVAL      mban = %trim(mban) +
316300060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
316400060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
316500030206e   2c                   ENDIF
316600030206     c*
316700030206if  2c                   IF        i < 10                                       *se c'è spazio
316800030206     c                   ADD       1             i                              *memorizza nota
316900030206     c                   EVAL      smnot(i) = mban
317000030206e   2c                   ENDIF
317100030206e   1c                   ENDIF
317200040330
317300040330     **?Reperisco referente
317400030206     c* Note supermercati
317500140716if  1c     f_tasftc      IFEQ      'S'
317600140716     c     f_tastc2      OREQ      'S'
317700030210     c                   CLEAR                   s                              *indice
317800140716     c                   Z-ADD     f_tasaas      kr5aas
317900140716     c                   Z-ADD     f_taslnp      kr5lnp
318000140716     c                   Z-ADD     f_tasnrs      kr5nrs
318100140716     c                   Z-ADD     f_tasnsp      kr5nsp
318200030206     c                   MOVEL     'SCR'         kr5trd
318300160127     c                   if        esito='P'
318400160127     c     keyar5        SETLL     fiar501l
318500160127     c     keyar5        READE     fiar501l                               98
318600160127     c                   else
318700040531     c     keyar5        SETLL     fiar531c
318800040531     c     keyar5        READE     fiar531c                               98
318900160127     c                   endif
319000160127     c
319100030210do  2c                   DOW       NOT *in98
319200030206     c                   MOVEL     ar5uni        dar5scr
319300030207     c* data nota
319400030210if  3c                   IF        ar5dac > *zeros
319500030207     c                   MOVEL     ar5dac        wdat
319600030207     c                   EXSR      edtdat                                       *edita data
319700030210     c                   MOVEL     wdate         msdace
319800030210e   3c                   ENDIF
319900030207     c* ora nota
320000030210if  3c                   IF        ar5orc > *zeros
320100030207     c                   MOVEL     ar5orc        wora
320200030207     c                   EXSR      edtora                                       *edita ora
320300030210     c                   MOVEL     worae         msorce
320400030210e   3c                   ENDIF
320500030207     c* Consegna richiesta
320600140708if  3c                   IF        §ar5dcr = *zeros AND
320700140708     c                             §ar5hcr = *zeros
320800140708x   3c                   ELSE
320900140708if  4c                   IF        §ar5dcr > *zeros
321000140708     c                   MOVEL     §ar5dcr       wdat
321100140708     c                   EXSR      edtdat                                       *edita data
321200140708     c                   MOVEL     wdate         msdcre
321300140708e   4c                   ENDIF
321400140708if  4c                   IF        §ar5hcr > *zeros
321500140708     c                   MOVEL     §ar5hcr       wora
321600140708     c                   EXSR      edtora                                       *edita ora
321700140708     c                   MOVEL     worae         mshcre
321800140708e   4c                   ENDIF
321900140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
322000140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
322100140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
322200140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
322300140708if  5c                   IF        §ar5tcr = 'P'                                *prima
322400140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
322500140708e   5c                   ENDIF
322600140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
322700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
322800140708e   5c                   ENDIF
322900140708if  5c                   IF        §ar5tcr = ' '                                *il
323000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
323100140708e   5c                   ENDIF
323200140708e   4c                   ENDIF
323300140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
323400140708     c                             §ar5dcr = *zeros                             *NO data richiesta
323500140708if  5c                   IF        §ar5tcr = 'P'                                *prima
323600140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
323700140708e   5c                   ENDIF
323800140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
323900140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
324000140708e   5c                   ENDIF
324100140708if  5c                   IF        §ar5tcr = ' '                                *il
324200140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
324300140708e   5c                   ENDIF
324400140708e   4c                   ENDIF
324500030210e   3c                   ENDIF
324600030210     c* Riferimento
324700030210if  3c                   IF        §ar5nom <>  *blanks
324800060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
324900060613     c                             + ' ' + §ar5nom
325000030210e   3c                   ENDIF
325100030210     c*
325200030210if  3c                   IF        s < 5                                        *MAX 5 note
325300030210     c                   ADD       1             s
325400030210     c                   EVAL      mscrT(s) = 'Il ' +
325500030210     c                             %trim(msdace) +                              *data nota
325600030210     c                             ' '    +
325700030210     c                             %trim(msorce) +                              *ora nota
325800030210     c                             ' '    +
325900140708     c                             %trim(mstcre) +                              *tipo consegna
326000140708     c                             ' '    +
326100140708     c                             %trim(msdcre) +                              *data consegna
326200140708     c                             ' '    +
326300140708     c                             %trim(mshcre) +                              *ora consegna
326400140708     c                             ' '    +
326500030210     c                             %trim(msnom)  +                              *parlato con
326600030210     c                             ' '    +
326700030210     c                             %trim(§ar5tel) +                             *telefono
326800030210     c                             ' '    +
326900030210     c                             %trim(§ar5mot)                               *motivo
327000030210e   2c                   ENDIF
327100030210     c*
327200160127     c                   if        esito='P'
327300160127     c     keyar5        READE     fiar501l                               98
327400160127     c                   else
327500160127     c     keyar5        READE     fiar531c                               98
327600160127e   1c                   ENDIF
327700030206e   2c                   ENDDO
327800030210e   1c                   ENDIF
327900030210     c* NOTA completa
328000030210if  1c                   IF        mscrT(1) <> *blanks
328100030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
328200030211     c                                      ' ' +
328300030211     C                                      %trim(mscrT(2)) +
328400030211     c                                      ' ' +
328500030211     C                                      %trim(mscrT(3)) +
328600030211     c                                      ' ' +
328700030211     C                                      %trim(mscrT(4)) +
328800030211     c                                      ' ' +
328900030211     C                                      %trim(mscrT(5))
329000030210     c                   Z-ADD     *zeros        s
329100030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
329200030210     c                   ADD       1             i
329300030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
329400030210     c                   EVAL      s = s + 100
329500030210e   2c                   ENDDO
329600030210     c*
329700030210e   1c                   ENDIF
329800010927     c*---
329900010927     c* memorizza le "note" che NON sono presenti nel record bolla
330000160127     c* ma sul file estensione
330100010927     c*---
330200160127     c
330300160127    0c                   if        esito<>'P'
330400010927     c                   CLEAR                   tita4000
330500010927     c                   CLEAR                   tita4p00
330600140716     c                   Z-ADD     f_tasaas      ka4aas
330700140716     c                   Z-ADD     f_taslnp      ka4lnp
330800140716     c                   Z-ADD     f_tasnrs      ka4nrs
330900140716     c                   Z-ADD     f_tasnsp      ka4nsp
331000160127     c
331100010927     c     ke2ta4        SETLL     tita430c
331200010927     c     ke2ta4        READE     tita430c                               98
331300010927do  1c                   DOW       NOT *in98
331400140708     c                   if        ta4trc = '8' OR ta4trc = '9'
331500050701     c*
331600050701if  3c                   IF        ta4trc = '8'
331700050701     c                   EVAL      mmnc8 = ta4not
331800050701e   3c                   ENDIF
331900050701if  3c                   IF        ta4trc = '9'
332000050701     c                   EVAL      mmnc9 = ta4not
332100050701e   3c                   ENDIF
332200050701     c*
332300010927e   2c                   ENDIF
332400010925     c*
332500010926     c     ke2ta4        READE     tita430c                               98
332600010926e   1c                   ENDDO
332700050701     C*
332800140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
332900050701     C*
333000050701     C                   IF        mmnc8 <> *BLANK
333100050701     C                   EVAL      a230 = %TRIML(mmnc8)
333200050701     C                   ENDIF
333300050701     C                   IF        mmnc9 <> *BLANK
333400050701     C                   IF        a230 <> *BLANK
333500050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
333600050701     C                   ELSE
333700050701     C                   EVAL      a230 = %TRIML(mmnc9)
333800050701     C                   ENDIF
333900050701     C                   ENDIF
334000010927     C*
334100050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
334200050701     C*
334300010927if  2c                   IF        i < 10                                       *se c'è spazio
334400010927     c                   ADD       1             i                              *memorizza nota
334500010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
334600010927e   2c                   ENDIF
334700010927     C*
334800010927if  2c                   IF        i < 10                                       *se c'è spazio
334900010927if  3c                   IF        n3 > 100
335000010927     c                   ADD       1             i                              *memorizza nota
335100010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
335200010927e   3c                   ENDIF
335300010927e   2c                   ENDIF
335400010927     C*
335500010927if  2c                   IF        i < 10                                       *se c'è spazio
335600010927if  3c                   IF        n3 > 200
335700010927     c                   ADD       1             i                              *memorizza nota
335800010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
335900010927e   3c                   ENDIF
336000010927e   2C                   ENDIF
336100010927e   1c                   ENDIF
336200160127e   0c                   ENDIF
336300010925     c*
336400000000     c* imposta contatore note da visualizzare nella DS di Output
336500000000     c                   EVAL      cntnoto74 = i
336600000000     c*
336700000000     c* imposta le note nella DS di Output
336800000000do  1c     1             DO        10            j
336900000000     c                   EVAL      nota(j) = smnot(j)
337000000000e   1c                   ENDDO
337100000000     c*
337200000000     c                   ENDSR
337300010716     c*--------------------------------------------------------------------------------------------*
337400010716     c* imposta i campi della DS di Output dalla firma DPD
337500010716     c*--------------------------------------------------------------------------------------------*
337600010716     c     impdsoDPD     BEGSR
337700010717     c*
337800010717     c* reimposta variabili di lavoro
337900010717     c                   RESET                   $AbFirma
338000010717     c                   CLEAR                   depute                         *deposito utente
338100010717     c                   CLEAR                   deppwd                         *deposito password
338200010717     c*---
338300010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
338400010717     c*---
338500010717     c                   Z-ADD     s_taslna      worg
338600010717     c                   EXSR      decorg
338700160406     c
338800020702     c                   IF        wodpd = 'DPD'
338900020702     c                   MOVEL     'S'           dpdo74
339000020702     c                   ELSE
339100020702     c                   MOVEL     *blanks       dpdo74
339200160406     c
339300160406     c* se si tratta di bolla EEX verifico se il partner è abilitato al link
339400160407     c                   IF        wodpd = 'EEX' and ksci74<>*blanks
339500160406     c                   clear                   fidn40ds
339600160406     c                   eval      i40fil=s_taslna
339700160406     c                   z-add     s_tasmgs      i40dsp
339800160406     c                   MOVEL     s_tasaas      i40dsp                          *data spedizione
339900160406     c                   call      'FIDN40R'
340000160406     c                   parm                    fidn40ds
340100160407
340200160406     c                   if        o40lnk='S'
340300160407     c* sommo i giorni alla data spedizione per sapere se ancora visibile
340400160407     c                   Clear                   xgiolavds
340500160407     c                   Eval      Ixglpa  = 'A'
340600160413     c                   Eval      IxglFil = s_taslna
340700160407     c                   Eval      Ixgldata = i40dsp
340800160407     c                   Eval      Ixgladd = 'S'
340900160407     c                   eval      ixglgg  = o40gio
341000160407     c                   Call      'XGIOLAV'
341100160407     c                   parm                    xgiolavds
341200160407     c                   if        oxgldata>=datcor
341300160406     c                   MOVEL     'E'           dpdo74
341400160407     c                   endif
341500160407     c
341600160406     c                   endif
341700160406     c                   endif
341800160406     c
341900020702     c                   ENDIF
342000010717     c*
342100010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
342200060627     c* Riferimento link Lettera di vettura
342300060627     c* Riferimento link Firma
342400010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
342500010717     c                   CLEAR                   tita4000
342600010717     c                   CLEAR                   tita4p00
342700060627     c                   CLEAR                   dsbl4i
342800010717     c                   Z-ADD     s_tasaas      ka4aas
342900010717     c                   Z-ADD     s_taslnp      ka4lnp
343000010717     c                   Z-ADD     s_tasnrs      ka4nrs
343100010717     c                   Z-ADD     s_tasnsp      ka4nsp
343200060627     c                   MOVEL     'I'           ka4trc
343300060627     c     keyta4        CHAIN     tita430c                           98
343400010717if  2c                   IF        NOT *in98
343500060627     c                   MOVEL     ta4not        dsbl4i
343600060627     c                   EVAL      DPDRmnO74 = §b4ipn
343700060627     c                   EVAL      DPDFnpO74 = §b4ipn
343800010717e   2c                   ENDIF
343900010717     c*
344000010717     c* cerca abilitazione firma DPD per questo segnacollo
344100010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
344200010717     c                   EXSR      RepAbilFirma
344300010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
344400010717     c*---
344500010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
344600010717     c*---
344700010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
344800010717     c* dati
344900010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
345000010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
345100010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
345200010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
345300010717     c*
345400010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
345500010717     c                   Z-ADD     s_tasaas      kvbaas
345600010717     c                   Z-ADD     s_taslnp      kvblnp
345700010717     c                   Z-ADD     s_tasnrs      kvbnrs
345800010717     c                   Z-ADD     s_tasnsp      kvbnsp
345900010717     c                   Z-ADD     99999999      kvbdev
346000010717     c                   Z-ADD     9999          kvbhev
346100010717     c     ke2evb        SETGT     fnevb01l
346200010717     c     keyevb        READPE    fnevb01l                               98
346300010717do  4c                   DOW       NOT *in98
346400101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
346500010720     c*
346600060629     c                   EVAL      devB01 = evbNot
346700060628     c                   IF        §NotDep = *BLANK
346800060628     c                   RESET                   DPDFirO74
346900060629     c                   LEAVE
347000060628     c                   ENDIF
347100060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
347200060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
347300080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
347400010720     c                   Z-ADD     *zeros        g08dat
347500010720     c                   MOVEL     '3'           g08err
347600010720     c                   CALL      'XSRDA8'
347700010720     c                   PARM                    wlbda8
347800010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
347900010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
348000010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
348100010720     c*
348200010717     c                   LEAVE                                                  *uscita ciclo
348300010717e   5c                   ENDIF
348400010717     c     keyevb        READPE    fnevb01l                               98
348500010717e   4c                   ENDDO
348600010717     c*
348700010717e   3c                   ENDIF
348800010717e   2c                   ENDIF
348900160323     c
349000160323   x1c                   else
349100010716e   1c                   ENDIF
349200010716     c*
349300010716     c                   ENDSR
349400160309     c*--------------------------------------------------------------------------------------------*
349500160309     c* impostazioni solo da FNBLP
349600160309     c*--------------------------------------------------------------------------------------------*
349700160309     c     impds_soloBLP BEGSR
349800160321     c
349900160321     c                   EVAL      dstaslnp = arblnp
350000160321     c                   EVAL      dstasnrs = arbnrs
350100160321     c                   EVAL      dstasnsp = arbnsp
350200160321     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
350300161117
350400161117     c                   MOVEL     arbaas        aasmgsO74                       *data spedizione
350500161117     c                   MOVE      arbmgs        aasmgsO74
350600161117     c
350700161117     c                   MOVEL     'N'           gco74                          *flag di controllo
350800161117     c                   MOVEL     'N'           gcfdio74
350900161117     c                   MOVEL     'N'           gcmito74
351000161117     c*
351100160314     C
351200160314     c                   Z-ADD     arblna        worg
351300160314     c                   exsr      setLnaItaEst
351400160314     C
351500160309     c* chain ar5 con bolla richiesta a video
351600160309     c                   Z-ADD     kasaas        kr5aas
351700160309     c                   Z-ADD     kaslnp        kr5lnp
351800160309     c                   Z-ADD     kasnrs        kr5nrs
351900160309     c                   Z-ADD     kasnsp        kr5nsp
352000160309     c                   MOVEL     'GEN'         kr5trd
352100160309     c     keyar5        CHAIN     fiar501l
352200160309     C                   IF        %FOUND
352300160309     C                   EVAL      DAR5GEN = AR5Uni
352400160309     C                   ELSE
352500160309     C                   RESET                   DAR5GEN
352600160309     C                   ENDIF
352700160309     C                   EVAL      REFCONSO74 = GEN§AR5REF
352800160309     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
352900160309     c*
353000160309     c* chain tipo bolla
353100160321     c                   eval      tblkey=arbcbo
353200160314     C                   EVAL      ds3A = chainTabel00f('CHAIN3':langTabel
353300160321     C                             :'3A':tblkey:%LEN(tblkey):'TBLUNI':
353400160314     C                             rpyOpCode:rpyEsito)
353500160309     c*
353600160309     c                   MOVEL     §3atb1        a1
353700160309     c                   MOVEL     §3atb1        tpporto74
353800160309if  1c                   IF        a1 = 'A'                                     *Pagamento del
353900160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
354000160309x   1c                   ELSE                                                   *Pagamento del
354100160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
354200160309e   1c                   ENDIF
354300160314     c
354400160314     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
354500160314     C                   IF        KSCI74 <> *BLANK
354600160314     c                   SETOFF                                           98
354700160314     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
354800160314     C                   RESET                   tis7700i0
354900160314     C                   EVAL      tis7700i0.aas = arbAas
355000160314     C                   EVAL      tis7700i0.lnp = arbLnp
355100160314     C                   EVAL      tis7700i0.nrs = arbNrs
355200160314     C                   EVAL      tis7700i0.nsp = arbNsp
355300160314     C                   EVAL      tis7700i0.tbl = §3atb1
355400160314     C                   EVAL      tis7700i0.ksu = kscI74
355500160314     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
355600160314     C***                CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
355700160314     C***                          : rpyEsito
355800160314     C***                          : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
355900160314     C***                          : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
356000160314     C***                          )
356100160314     C***                EVAL      *IN98 = (rpyEsito >= *ZERO AND
356200160314     C***                          tis7700o0.bollValida = *ON)
356300160314     **?Se non gli appartiene, prima di restituire errore controllo il check code.
356400160314if  2c                   IF        NOT *in98                                      *non trovato
356500160314     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
356600160314     C                   reset                   TIS778DSO
356700160818     C*****              EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
356800160818     c                   eval      esito='1'
356900160818     c                   eval      widmsg = 'TIS0849'
357000160818     c                   eval      widcampo = 'NSPEDIZI74'
357100160818     c                   clear                   wparm
357200160818     c                   exsr      messaggi
357300160818     c                   exsr      routend
357400160818     C******             RETURN
357500160314     C                   ENDIF
357600160314e   2c                   ENDIF
357700160314     C                   ENDIF
357800160314
357900160314     c
358000160309     c                   ENDSR
358100160314     c*--------------------------------------------------------------------------------------------*
358200160314     c* impostazioni del C/Assegno
358300160314     c*--------------------------------------------------------------------------------------------*
358400160314     c     ImpdsoAR9     BEGSR
358500160314     c                   Z-ADD     waas          ksbaas
358600160314     c                   Z-ADD     wlnp          ksblnp
358700160314     c                   Z-ADD     wnrs          ksbnrs
358800160314     c                   Z-ADD     wnsp          ksbnsp
358900160314     c     keycsb        CHAIN     fiar901l                           99
359000160314if  2c                   IF        NOT *in99                                    *esistente
359100160314     c* memorizza i dati
359200160314     c                   MOVEL     'S'           cao74                          *esiste C/A
359300160314     c*
359400160314     c                   Z-ADD     ar9cas        caimpo74                       *importo
359500160314     c*
359600160314     c                   EVAL      cadivo74 =  ar9vca                           *divisa
359700160314if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
359800160314     c                             caimpo74 > *zeros
359900160314     c                   EVAL      cadivo74 = 'ITL'
360000160314e   3c                   ENDIF
360100160314
360200160314     ** Visualizzo la descrizione del tipo incasso
360300160314     c**  devo impostare alcuni campi di CSB
360400160314     c                   eval      csbaas=ar9aas
360500160314     c                   eval      csblnp=ar9lnp
360600160314     c                   eval      csbnrs=ar9nrs
360700160314     c                   eval      csbnsp=ar9nsp
360800160314     c                   clear                   csbddc
360900160314     c                   clear                   csbtpa
361000160314     c                   clear                   csbtpi
361100160314     c
361200160314     C                   EVAL      cainco74 =
361300160314     C                                       GetDescrizioneIncassoContrassegno()
361400160314     c* stato C/Assegno : attivo
361500160314     c                   MOVEL     *blanks       castao74                       *stato
361600160314     C                   EVALR     tblKey = '0'
361700160314     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
361800160314     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
361900160314     C                             :rpyOpCode:rpyEsito)
362000160314     C                   IF        rpyOpCode = 'FOUND'
362100160314     c                   EVAL      castao74 = §4SDei
362200160314e   3c                   ENDIF
362300160314
362400160314if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
362500160314     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)     *NO distinta incasso
362600160314     c                   endif
362700160314
362800160314     c                   endif
362900160314     c
363000160314     c                   ENDSR
363100140604     c*--------------------------------------------------------------------------------------------*
363200140604     c* imposta i campi da FNARB se TASDCM = 0
363300140604     c*--------------------------------------------------------------------------------------------*
363400140604     c     impdsoARB     BEGSR
363500150403     c                   clear                   orarich           4 0
363600150611     c                   clear                   wdee
363700170301     c                   clear                   mito74
363800150410     c* memorizzo se utilizzo la data prevista consegna
363900150410     c                   clear                   usaprev           1
364000170301     c                   eval      wbol='ARB'
364100140604     c*
364200140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
364300140604     c                   EVAL      lnaDescO74 = sodes(i)
364400140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
364500140604     c*
364600150116      *eliminata forzatura resta impostato dalla mamma per composizione link
364700150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
364800150116     c*                  MOVE      arbmgs        aasmgsO74
364900140818      *riempimento da verificare
365000140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
365100140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
365200140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
365300140818
365400140818     c                   if        tpporto74 = 'A'
365500140818     c                   if        arbccm > 0
365600140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
365700140818     c                   else
365800140818     c                   movel     arblnp        ccmO74
365900140818     c                   move      8888          ccmO74
366000140818     c                   endif
366100140818     c                   else
366200140818     c                   z-add     arbksc        ccmO74
366300140818     c                   endif
366400140818      *
366500140604     c                   MOVEL     arblna        lineaaro74
366600140604     c                   MOVEL     arbtsp        tpservio74
366700140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
366800140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
366900140604     C                             rpyOpCode:rpyEsito)
367000140604     C                   EVAL      corro74 = §5EDes
367100140604     c*
367200140604     c                   EVAL      destino74 = arbrsd
367300140604     c                   EVAL      desindo74 = arbind
367400140604     c                   EVAL      descapo74 = arbcad
367500140604     c                   EVAL      desloco74 = arblod
367600140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
367700140604if  1c                   IF        arbnzd    = *blanks
367800140604     c                   EVAL      desnazo74 = arbprd
367900140604x   1c                   ELSE
368000150326     c**                 EVAL      desnazo74 = arbnzd
368100150326     c**                 EVAL      desnzdo74 = arbnzd
368200150326     c* 26/03/15: Attiviamo le specifiche per provincia export
368300150326     c                   EVAL      desnazo74 = arbprd
368400150326     c                   EVAL      desnzdo74 = arbnzd
368500150326e   1c                   ENDIF
368600140604     c*
368700140604     c                   EVAL      collio74  = arbncl
368800140604     c                   EVAL      pesoo74   = arbpkf
368900140604     c                   EVAL      volumeo74 = arbvlf
369000140604     c                   EVAL      naturao74 = arbnas
369100140604     c*
369200140604     c                   EVAL      disnumo74  = arbndc
369300140604c    c                   MOVEL     arbddc        wdat
369400140604     c                   EXSR      edtdat
369500140604     c                   EVAL      disdatao74 = wdate
369600140604     c*
369700170310     c                   MOVE      ccmo74        alfa4
369800170310if  1c                   IF        alfa4 <> '8888'                                *codificato
369900170310     c                   clear                   mito74
370000170310     c                   clear                   solomito74
370100170301     c                   exsr      tabcli
370200170310     c                   else
370300140604     c                   EVAL      mito74    = arbrsm
370400170310     c                   endif
370500170310
370600140604     c                   EVAL      mitindo74 = arbinm
370700140604     c                   EVAL      mitcapo74 = arbcam
370800140604     c                   EVAL      mitloco74 = arblom
370900140604      *specifiche da eliminare dopo la modifica in immissione bolle
371000140604if  3c                   IF        arbnzm = *blanks
371100140604     c                   EVAL      mitnazo74 = arbprm
371200140604x   3c                   ELSE
371300140604     c                   EVAL      mitnazo74 = arbnzm
371400140604e   3c                   ENDIF
371500140604     c* 2a ragione sociale destinatario.
371600140604     c                   Z-ADD     arbaas        ka4aas
371700140604     c                   Z-ADD     arblnp        ka4lnp
371800140604     c                   Z-ADD     arbnrs        ka4nrs
371900140604     c                   Z-ADD     arbnsp        ka4nsp
372000140604     c                   MOVEL     'D'           ka4trc
372100140604     c     keyta4        CHAIN     fiar401l                           99
372200140604if  1c                   IF        NOT *in99
372300140604     c                   MOVEL     ar4not        desti2O74
372400140604e   1c                   ENDIF
372500140604     c                   MOVEL     'E'           ka4trc
372600140604     c     keyta4        CHAIN     fiar401l                           99
372700140604if  1c                   IF        NOT *in99
372800140604     c                   MOVEL     ar4not        dsbl4e
372900140604e   1c                   ENDIF
373000140604     c
373100140604     ** Riferimento mittente numerico (c'è sempre).
373200140604     C                   EVAL      rifero74 = arbrmn
373300140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
373400140604     C                   IF        arbrma <> *blank and
373500140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
373600140604     C                   EVAL      rifero74a = arbrma
373700140604     C                   ENDIF
373800140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
373900140604     C                   IF        §b4erp <> *BLANK AND
374000140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
374100140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
374200140604     C                   EVAL      rpto74 = §b4erp
374300140604     C                   ENDIF
374400140604
374500140604     **?Reperisco il check code della spedizione.
374600140604     C                   EVAL      SChkCdeO74 =
374700140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
374800140604     C                             NSpedizI74:ChkCode:nEsito)
374900140604
375000140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
375100141126     C*m                 IF        KSCI74 <> *BLANK
375200141126     c*m                 SETOFF                                           98
375300140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
375400141126     C*m                 RESET                   tis7700i0
375500141126     C*m                 EVAL      tis7700i0.aas = arbAas
375600141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
375700141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
375800141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
375900141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
376000141126     C*m                 EVAL      tis7700i0.ksu = kscI74
376100141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
376200141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
376300141126     C*m                           : rpyEsito
376400141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
376500141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
376600141126     C*m                           )
376700141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
376800141126     C*m                           tis7700o0.bollValida = *ON)
376900140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
377000141126if  2c*m                 IF        NOT *in98                                      *non trovato
377100141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
377200141126     C*m                 reset                   TIS778DSO
377300141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
377400141126     C*m                 RETURN
377500141126     C*m                 ENDIF
377600141126e   2c*m                 ENDIF
377700141126     C*m                 ENDIF
377800140604     c*
377900140604     ** Importo assicurato.
378000140604     C                   IF        arbIas > 0
378100140604     C                   EVAL      vasO74 = arbVas
378200140604     C                   EVAL      iasO74 = arbIas
378300140604     C                   ENDIF
378400140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
378500140605     c                   if        arbdcm = 0 and wcev <> '704'
378600140617     c                   if        arbdcr > 0
378700140605     c                   MOVEL     arbdcr        wdat
378800140605     c                   EXSR      edtdat
378900140605     c                   eval      DTCONRCO74  =  wdate
379000140819if  3c                   IF        arbtcr = ' '
379100140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
379200140819e   3c                   ENDIF
379300140617     c                   endif
379400140617     c                   if        arbhcr > 0
379500140617     c                   MOVEL     arbhcr        wora
379600140617     c                   EXSR      edtora
379700140617     c                   eval      ORACONRO74  =  worae
379800140617     c                   endif
379900140606     c                   eval      TPDATCRO74  =  arbtcr
380000160407     c
380100140605      *reperimento descrizione tipo consegna richiesta
380200140617if  3c                   IF        arbtcr = 'P'
380300140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
380400140617e   3c                   ENDIF
380500140617if  3c                   IF        arbtcr = 'D'
380600140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
380700140617e   3c                   ENDIF
380800160407     c
380900160407     c
381000160407     c* Per  le bolle in partenza eseguo solo se presente la data entrata
381100160407    0c                   if        esito<>'P' or arbdse>0
381200160407     c
381300140605     C                   clear                   tnsd99ds
381400140605     C                   MOVEL     ' '           D98tla
381500160314     c                   if        esito='P'
381600160314     C                   MOVEL     'P'           D98tbo
381700160314     c                   else
381800140605     C                   MOVEL     'A'           D98tbo
381900160314     c                   endif
382000140605     C                   Z-ADD     arbaas        D98aas
382100140605     C                   Z-ADD     arblnp        D98lnp
382200140605     C                   Z-ADD     arbnrs        D98nrs
382300140605     C                   Z-ADD     arbnsp        D98nsp
382400140605     C*
382500140605     C                   call      'TNSD99R'
382600140605     C                   parm                    tnsd99ds
382700150304     c
382800140605     C*
382900140917      * chiamata a trulors per tempi standard
383000140917     c                   clear                   trulorsds
383100140917     c                   clear                   trulor2ds
383200140917     c                   clear                   trulor3ds
383300150611     c                   eval      wdee=d98dee
383400140917      * solo se questa data è valorizzata
383500150304    1c                   if        d98dee > 0
383600140912     c                   movel     d98dee        wdat
383700140605     c                   exsr      edtdat
383800140605     c                   eval      DTTEOCOO74  =  wdate
383900140819     c
384000140904      * se fermo deposito non richiamo pgm per reperimento orari
384100150304    2c                   if         arbffd = *blank
384200140605     c                   eval       IOREtser = 'C'
384300140605     c                   eval       IOREDDC=arbddc
384400140605     c                   eval       IOREnDC=arbndc
384500140609     c                   eval       IOREfil=arblna
384600140609     c                   eval       IOREdta=d98dce
384700140609     c                   eval       IOREcap=arbcad
384800140609     c                   eval       IOREloc=arblod
384900151019     c*****              eval       IOREtfp=arbtfp
385000151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
385100151019     c* del terminal di partenza originale
385200151019     c                   if         d98flp>*zeros
385300151019     c                   eval       IOREtfp=%INT(d98flp)
385400151019     c                   else
385500151019     c                   eval       IOREtfp=arbtfp
385600151019     c                   endif
385700140609     c                   eval       IOREtfa=arbtfa
385800140610     c                   eval       IOREdti=arbdti
385900140610     c                   eval       IOREhti=arbhti
386000140610     c                   eval       IOREdcr=arbdcr
386100140610     c                   eval       IOREhcr=arbhcr
386200140610     c                   eval       IOREtcr=arbtcr
386300140819     c                   eval       IOREtsp=arbtsp
386400140819     c                   eval       IOREnar=arbnzd
386500140819     c                   eval       IOREdei=d98dei
386600140819     c                   eval       IOREoei=d98oei
386700150819     c                   clear                   diore01
386800150819     c                   movel     d98ttc        §ioretrazC
386900150819     c                   movel     d98tcc        §ioreconsC
387000150819     c                   eval       IOREflo=diore01
387100150325     c* passo la località normalizzata
387200150325     c                   clear                   tita4000
387300150325     c                   clear                   tita4p00
387400150325     c                   Z-ADD     arbaas        ka4aas
387500150325     c                   Z-ADD     arblnp        ka4lnp
387600150325     c                   Z-ADD     arbnrs        ka4nrs
387700150326     c                   Z-ADD     arbnsp        ka4nsp
387800150325     c                   MOVEL     'X'           ka4trc
387900160314     c                   if        esito='P'
388000160314     c     keyta4        CHAIN     fiar401l                           99
388100160314if  1c                   IF        NOT *in99
388200160314     c                   MOVEL     ar4not        OOR3LOC_N
388300160314     c                   else
388400160314     c                   movel     arblod        OOR3LOC_N
388500160314e   1c                   ENDIF
388600160314     c
388700160314     c                   else
388800160314     c     keyta4        CHAIN     tita430c                           99
388900150325if  1c                   IF        NOT *in99
389000150325     c                   MOVEL     ta4not        OOR3LOC_N
389100150325     c                   else
389200150325     c                   movel     arblod        OOR3LOC_N
389300150325e   1c                   ENDIF
389400160314e   1c                   ENDIF
389500140605     c
389600160321     c                   if        esito<>'P'
389700150304    3c                   IF         (ARBfbc = *blanks and
389800140605     c                               ARBddc > 0 and
389900140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
390000140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
390100140605     c                   eval       IOREcons = 'S'
390200150304    3c                   endif
390300160321     c
390400140605     c* Orario presunto ritiro/consegna
390500140605     c                   move      arbndc        pctndc
390600140605     c                   move      arbifp        pctfgs
390700140605     c                   move      'COR'         ttrd
390800140605     c     kcet          setgt     fipct02l
390900140605     c     kcet          readpe    fipct02l
391000150304    3c                   if        not %eof(fipct02l)
391100140605     c                   movel     pctdati       fipctcords
391200140605     c                   eval      ioreorp=§PCTORASTI
391300140605     c                   endif
391400160321    3c                   endif
391500160321
391600140605     c                   call      'TRULORSR'
391700140605     C                   PARM                    kpjba
391800140605     C                   PARM                    trulorsds
391900140605     C                   PARM                    trulor2ds
392000140605     C                   PARM                    trulor3ds
392100150304    3c                   endif
392200140818     c                   clear                   ORSTINIO74
392300150304    3c                   if        OOR2STIS > 0
392400140617     c                   MOVEL     OOR2STIS      wora
392500140617     c                   EXSR      edtora
392600140617     c                   eval      ORSTINIO74  =  worae
392700150304    3c                   endif
392800140818     c                   clear                   ORSTFINO74
392900150304    3c                   if        OOR2STFS > 0
393000140617     c                   MOVEL     OOR2STFS      wora
393100140617     c                   EXSR      edtora
393200140617     c                   eval      ORSTFINO74  =  worae
393300150304    3c                   endif
393400140818     c                   clear                   ORMIINIO74
393500150304    3c                   if        OOR2MIIS > 0
393600140617     c                   MOVEL     OOR2MIIS      wora
393700140617     c                   EXSR      edtora
393800140617     c                   eval      ORMIINIO74  =  worae
393900150304    3c                   endif
394000140818     c                   clear                   ORMAFINO74
394100150304    3c                   if        OOR2MXFS > 0
394200140617     c                   MOVEL     OOR2MXFS      wora
394300140617     c                   EXSR      edtora
394400140617     c                   eval      ORMAFINO74  =  worae
394500150304    3c                   endif
394600150304    3c                   if        wricons = 'S'
394700140818     c                   clear                   ORRIDALO74
394800150304    4c                   if        OOR2RIDS > 0
394900140617     c                   MOVEL     OOR2RIDS      wora
395000140617     c                   EXSR      edtora
395100140617     c                   eval      ORRIDALO74  =  worae
395200150304    4c                   endif
395300140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
395400140818     c                   clear                   ORRIALLO74
395500150304    4c                   if        OOR2RIAS > 0
395600140617     c                   MOVEL     OOR2RIAS      wora
395700140617     c                   EXSR      edtora
395800140617     c                   eval      ORRIALLO74  =  worae
395900150304    4c                   endif
396000140617     c                   eval      FLRICONO74  =  OOR2FRIC
396100150304   x3c                   else
396200140711     c                   clear                   ORRIALLO74
396300140711     c                   clear                   ORRIDALO74
396400140711     c                   eval      FLRICONO74  =  'N'
396500150304    3c                   endif
396600140818     c                   clear                   ORSTICDO74
396700150304    3c                   if        OOR2PRESD > 0
396800140617     c                   MOVEL     OOR2PRESD     wora
396900140617     c                   EXSR      edtora
397000140617     c                   eval      ORSTICDO74  =  worae
397100150304    3c                   endif
397200140818     c                   clear                   ORSTICAO74
397300150304    3c                   if        OOR2PRESa > 0
397400140617     c                   MOVEL     OOR2PRESa     wora
397500140617     c                   EXSR      edtora
397600140617     c                   eval      ORSTICAO74  =  worae
397700150304    3c                   endif
397800140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
397900140605     c                   eval      FLLOCNOO74  =  OOR3NORM
398000140904      *end F.D.
398100150304    2c                   endif
398200140605      * RIFERIMENTI DESTINATARIO
398300140605     c                   MOVEL     'EMD'         kr5trd
398400140605     c     keyAR5        CHAIN     fiar501l
398500150304if  2c                   IF        %found(fiar501l)
398600140605     c                   MOVEL     ar5uni        dar5emd
398700140606     c                   eval      EMLDESTO74  =  §§AR5EML
398800140606     c                   eval      CELDESTO74  =  §§AR5TEL
398900150304e   2c                   ENDIF
399000140604     **
399100150304     c*
399200150304     c* Stato data prevista consegna
399300150305     c                   clear                   stat_n2o74
399400150305     c                   eval      STATUSo74   =  D98SPCDEE
399500150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
399600150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
399700150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
399800150312     c                   eval      statuso74='RIC'
399900150403     c* se presente FIARP --> visualizzo la data richiesta
400000150403     c     karp          chain     fiarp01l
400100150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
400200150611     c                   movel     arpdcr        wdee
400300150611     c                   movel     arpdcr        wdat
400400150403     c                   exsr      edtdat
400500150403     c                   eval      DTTEOCOO74  =  wdate
400600150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
400700150421    3C                   IF        ARPHCR>0
400800150403     c                   eval      orarich=arphcr
400900150421    3c                   endif
401000150421   x2c                   else
401100150403     c*  se presente DOPO IL imposto la data in D98dee
401200150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
401300150403     c                   eval      D98dee=arpdcr
401400150611     c                   eval      wdee=arpdcr
401500150421    3c                   endif
401600150403     c* PRIMA DEL come se non ci fosse...
401700150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
401800150421    3c                   if        d98dee>0
401900150312     c                   Clear                   xgiolavds
402000150312     c                   Eval      Ixglpa  = 'A'
402100150312     c                   Eval      IxglFil = arblna
402200150312     c                   Eval      Ixgldata = d98dee
402300150312     c                   Eval      Ixgladd = 'S'
402400150312     c                   Eval      Ixgllav = 'S'
402500150312     c                   eval      ixglgg  = 1
402600150312     c                   Call      'XGIOLAV'
402700150312     c                   parm                    xgiolavds
402800150611     c                   movel     oxgldata      wdee
402900150611     c                   movel     oxgldata      wdat
403000150312     c                   exsr      edtdat
403100150312     c                   eval      DTTEOCOO74  =  wdate
403200150421    3c                   endif
403300150421    2c                   endif
403400150421    1c                   endif
403500150421     c
403600150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
403700150421     c                   eval      statuso74='ATT'
403800150421     c                   endif
403900150421    1c                   if        PDAgia='S' and statuso74='DDC'
404000150421     c                   eval      statuso74='GIA'
404100150421     c                   endif
404200170320     c* Se lo stato è : in distinta e ho FIARP, aggiungo una scritta
404300170320     c                   if        statuso74='DDC'
404400170320     c     karp          chain     fiarp01l
404500170320    2c                   if        %found(fiarp01l) and arpdcr>0 and
404600170320     c                             arpdcr>datcor  and
404700170320     c                             (arptcr=' ' or  arptcr='D')
404800170320     c                   eval      statuso74='DDP'
404900170320     c                   endif
405000170320     c                   endif
405100170320     c
405200150401     c
405300150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
405400150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
405500150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
405600150401     c* La data contatto = data distinta + 1
405700150512     c**                 Clear                   xgiolavds
405800150512     c**                 Eval      Ixglpa  = 'A'
405900150512     c**                 Eval      IxglFil = arblna
406000150512     c**                 Eval      Ixgldata = arbddc
406100150512     c**                 Eval      Ixgladd = 'S'
406200150512     c**                 Eval      Ixgllav = 'S'
406300150512     c**                 eval      ixglgg  = 1
406400150512     c**                 Call      'XGIOLAV'
406500150512     c**                 parm                    xgiolavds
406600150512     c**                 movel     oxgldata      wdat
406700150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
406800150512     c**                 endif
406900150317     c
407000150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
407100150326     c*  lo status diventa RIC
407200150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
407300150317     c                   eval      statuso74='RIC'
407400150317     c                   endif
407500150310     c
407600150304     c* decodifica stato da mettere nei campi deco e note
407700150304     c                   clear                   dspc
407800150304     C                   RESET                   tibs02ds
407900150304     C                   EVAL      t02Cod = 'SPC'
408000150312     C                   EVAL      t02Ke1 = statuso74
408100150306     C                   EXSR      getTntbe00f
408200150306     c                   IF        t02Err <>'E'
408300150306     c
408400150305     c                   eval       stat_d1o74=§SPCDEC1
408500150305     c                   eval       stat_d2o74=§SPCDEC2
408600150304     c*
408700150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
408800150305     c                   eval       stat_n1o74=§SPCNOT1
408900150306     c                   eval       stat_n2o74=§SPCNOT2
409000150304     c                   else
409100150305     c                   clear                   stat_d1o74
409200150305     c                   clear                   stat_d2o74
409300150305     c                   clear                   stat_n1o74
409400150306     c                   clear                   stat_n2o74
409500150304     c                   endif
409600160407
409700160407     c                   endif
409800160407
409900150409     c                   else
410000150409     c* stato evento CONSEGNATO
410100150409     c                   eval      statuso74='CON'
410200150415     c                   eval      flgbolcO74  =  'S'
410300150415     c                   eval      flgboldO74  =  'N'
410400150409    1c                   endif
410500150304     c
410600150304     c
410700140919     ** riempimento per campi da arb con data consegna diversa da 0
410800140919     c                   if        arbdcm > 0
410900140919     c                   movel     arbdcm        wdat
411000140919     c                   exsr      edtdat
411100140919     c                   eval      dtconmeo74  = wdate
411200140919     c                   MOVEL     arbdcm        wora
411300140919     c                   EXSR      edtora
411400140919     c                   eval      orconmeO74  =  worae
411500140919     c                   eval      flgbolcO74  =  'S'
411600140919     c                   eval      flgboldO74  =  'N'
411700141015      *firmatario distinta PDA
411800141015     c                   clear                   firmato74
411900141015     c                   if        gen§ar5pdaco <> 'NO'
412000141015     c                   exsr      repfirmatARB
412100141015     c                   else
412200141015     c                   if        arbgma <> *blank
412300141015     C                   EVAL      tblKey = arbgma
412400141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
412500141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
412600141015     C                             :rpyOpCode:rpyEsito)
412700141015     C                   IF        rpyOpCode = 'FOUND'
412800141015     c                   if        §7r2fi = 'S'
412900141015     c                   exsr      repfirmatARB
413000141015     c                   endif
413100141015     c                   endif
413200141015     c                   endif
413300141015     c                   endif
413400140919     c                   else
413500150415     c* solo se non impostato da PdA lo stato CONSEGNA
413600150415     c                   if        statuso74<>'CON'
413700150415     c
413800140919     c                   eval      flgbolcO74  =  'N'
413900140919     c                   if        arbndc > 0 and arbddc > 0
414000150415     c                             and arbddc <= datcor and d98cons='S'
414100140919     c                   eval      flgboldo74 = 'S'
414200140919     c                   else
414300140919     c                   eval      flgboldO74  =  'N'
414400140919     c                   endif
414500150415     c                   endif
414600150415
414700140919     c                   endif
414800150415     c
414900150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
415000140919     c                   eval      orincono74 = orsticdo74
415100140919     c                   eval      orficono74 = ORSticaO74
415200140919     c                   else
415300140919     c                   eval      orincono74 = ORSTINIO74
415400140919     c                   eval      orficono74 = ORSTFINO74
415500140919     c                   endif
415600150326     c* note 1
415700150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
415800150309     c                   clear                   wparm
415900150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
416000150326     c                   if        D98DTCTD<>*blanks
416100150326     c                   movel     D98DTCTD      wdat
416200150326     c                   exsr      edtdat
416300150326     c                   else
416400150326     c                   eval      wdate=dtteocoo74
416500150410     c                   eval      usaprev='S'
416600150326     c                   endif
416700150326     c
416800150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
416900150309     c                   eval      stat_n1o74= rtvMsgLang(
417000150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
417100150309     c                   endif
417200150309     c* note 2
417300150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
417400150309     c                   clear                   wparm
417500150403     c* Se lo stato è PRG passo orario limite uscita aut
417600150403     c                   select
417700170320     c                   when      statuso74='PRG'
417800150326     c                   IF        d98picklim  >*zeros
417900150309
418000150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
418100150326     c                                   (%subst(d98picklim:3:2))
418200150309     c                   else
418300150309     c                   eval      wparm='09:00'
418400150403     c* reperisco orario di fine picking
418500150403     c* filiale / data
418600150403     C                   RESET                   tibs02ds
418700150403     C                   EVAL      t02Cod = 'OLP'
418800150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
418900150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
419000150403     C                   EXSR      getTntbe00f
419100150403     c* filiale
419200150403     c                   IF        t02Err = 'E'
419300150403     C                   RESET                   tibs02ds
419400150403     C                   EVAL      t02Cod = 'OLP'
419500150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
419600150403     C                   EXSR      getTntbe00f
419700150403     c                   endif
419800150403     c* Generico 000
419900150403     c                   IF        t02Err = 'E'
420000150403     C                   RESET                   tibs02ds
420100150403     C                   EVAL      t02Cod = 'OLP'
420200150403     C                   EVAL      t02Ke1 = '000'
420300150403     C                   EXSR      getTntbe00f
420400150403     c                   endif
420500150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
420600150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
420700150403     c                             %subst(t02uni:3:2)
420800150309     c                   endif
420900150403     c                   endif
421000150403     c                   eval      stat_n2o74= rtvMsgLang(
421100150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
421200170320     c
421300170320     c* Se lo stato è DDP passo la data cons richiesta di fiarp
421400170320    2c                   when      statuso74='DDP'
421500170320     c                   movel     arpdcr        wdat
421600170320     c                   exsr      edtdat
421700170320                         eval      wparm=wdate ;
421800170320     c                   eval      stat_n2o74= rtvMsgLang(
421900170320     c                             %subst(stat_n2o74:1:7):langi74:wparm)
422000150403     c                   other
422100150403     c* altri casi
422200150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
422300150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
422400150611     c                             arbtcr=' ' and arbdcr>0  and
422500150611     c                             arbdcr>=wdee
422600150410     c
422700150403     c                   eval      orarich=arbhcr
422800150410     c                   endif
422900150403     c
423000150403     c                   if        orarich>0
423100150403     c                   exsr      sr_dallealle
423200150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
423300150403     c                   eval      stat_n2o74= rtvMsgLang(
423400150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
423500150403     c
423600150403     c                   else
423700150403     c                   clear                   stat_n2o74
423800150403     c                   endif
423900150403     c
424000150403     c                   endsl
424100150403     c
424200150309     c                   endif
424300150410     c
424400150424     c* Se si tratta di fermo deposito pulisco sempre
424500150424     c                   if        usaprev<>'S' or arbffd='S'
424600150410     c                   clear                   DTTEOCOO74
424700150410     c                   clear                   ORINCONO74
424800150410     c                   clear                   ORFICONO74
424900150410     c                   endif
425000150309     c
425100170301     c                   clear                   wbol              3
425200140604     c                   ENDSR
425300150403      /free
425400150403       //--------------------------------------------------------------
425500150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
425600150403       //--------------------------------------------------------------
425700150403       BEGSR  sr_dallealle;
425800150403       clear  w_dallemsg ;
425900150403       clear  w_allemsg ;
426000150403
426100150403       // Reperimento dati tabella VPO - ORARISER
426200150403       if §VPORST_MW=0  ;
426300150403       clear tibs02ds;
426400150403       clear dvpooraris;
426500150403       t02mod='C';
426600150403       t02cod='VPO';
426700150403       t02ke1='ORARISER';
426800150403       tibs02r(kpjba:tibs02ds);
426900150403       if t02err=*blanks;
427000150403          dvpooraris=t02uni;
427100150403       endif;
427200150403       endif;
427300150403
427400150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
427500150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
427600150403       //   determino orari servizio comprensivi della tolleranza
427700150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
427800150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
427900150403
428000150403            if w_dalle<w_dalleoser;
428100150403               w_dalle=w_dalleoser;
428200150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
428300150403            endif;
428400150403
428500150403            if w_alle>w_alleoser;
428600150403               w_alle=w_alleoser;
428700150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
428800150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
428900150403               endif;
429000150403            endif;
429100150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
429200150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
429300150403
429400150403       endsr;
429500150403      /end-free
429600010717     c*--------------------------------------------------------------------------------------------*
429700010717     c* imposta i campi della DS di Output dagli eventi
429800010717     c*--------------------------------------------------------------------------------------------*
429900010717     c     RepAbilFirma  BEGSR
430000010717     c*
430100010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
430200010717     c* e recupera utente / password
430300010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
430400010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
430500010717     c     keyvtd        SETLL     tivtd01l
430600010717     c     keyvtd        READE     tivtd01l                               98
430700010717do  2c                   DOW       NOT *in98
430800010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
430900010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
431000010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
431100010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
431200010717     c                   EVAL      depute = vtdute                              *utente
431300010717     c                   EVAL      deppwd = vtdpwd                              *password
431400010717     c                   LEAVE                                                  *esce dal ciclo
431500010717e   3c                   ENDIF
431600010717     c     keyvtd        READE     tivtd01l                               98
431700010717e   2c                   ENDDO
431800010717e   1C                   ENDIF
431900010717     c*
432000010717     c                   ENDSR
432100000000     c*--------------------------------------------------------------------------------------------*
432200000000     c* edita data da aaaammgg in gg/mm/aaaa
432300000000     c*--------------------------------------------------------------------------------------------*
432400000000     c     edtdat        BEGSR
432500000000     c*
432600000000     c                   MOVEL     *blanks       wdate
432700000000     c*
432800000000     c                   MOVEL     wdat          dsdat
432900000000     c                   MOVEL     dsgio         dsgioe
433000140528     c                   MOVEL     '.'           dsvd1
433100000000     c                   MOVEL     dsmes         dsmese
433200140528     c                   MOVEL     '.'           dsvd2
433300000000     c                   MOVEL     dsann         dsanne
433400000000     c                   MOVEL     dsdate        wdate
433500000000     c*
433600000000     c                   ENDSR
433700000000     c*--------------------------------------------------------------------------------------------*
433800000000     c* edita ora da hhmm in hh:mm
433900000000     c*--------------------------------------------------------------------------------------------*
434000000000     c     edtora        BEGSR
434100000000     c*
434200000000     c                   MOVEL     *blanks       worae
434300000000     c*
434400000000     c                   MOVEL     wora          dsora
434500000000     c                   MOVEL     dshh          dshhe
434600140617     c                   MOVEL     '.'           dsvo1
434700000000     c                   MOVEL     dsmm          dsmme
434800000000     c                   MOVEL     dsorae        worae
434900000000     c*
435000000000     c                   ENDSR
435100000000     c*--------------------------------------------------------------------------------------------*
435200000000     c* decodifica punto operativo
435300000000     c*--------------------------------------------------------------------------------------------*
435400000000     c     decorg        BEGSR
435500000000     c*
435600000000     c                   MOVEL     *blanks       wodes                          *descrizione
435700000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
435800000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
435900000000     c*
436000000000     c                   SETOFF                                           98
436100041111     c                   Z-ADD     1             O
436200041111     c     worg          LOOKUP    sorg(O)                                98
436300000000if  1c                   IF        *in98                                        *trovata
436400121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
436500121001     c                             %EDITC(wOrg : 'X') + ')'
436600041111     c                   MOVEL     sofl1(O)      wofl1
436700041111     c                   MOVEL     sodpd(O)      wodpd
436800000000e   1c                   ENDIF
436900000000     c*
437000000000     c                   ENDSR
437100000000     c*--------------------------------------------------------------------------------------------*
437200000000     c* decodifica evento per STATI
437300000000     c*--------------------------------------------------------------------------------------------*
437400000000     c     deccevsta     BEGSR
437500000000     c*
437600000000     c                   MOVEL     '0'           werr                           *NO errore
437700050704     c*
437800050704     C                   IF        wcev = 'GEN'
437900050704     C                   EVAL      werr = *ON
438000050704     C                   CLEAR                   wcdex
438100050704     C                   LEAVESR
438200050704     C                   ENDIF
438300000000     c*
438400060622     C                   RESET                   tibs02ds
438500060622     C                   EVAL      t02Cod = 'ICE'
438600060622     C                   EVAL      t02Ke1 = wcev
438700060622     C                   EXSR      getTntbe00f
438800060622     C                   IF        t02Err <> 'E' AND
438900140528     C                             (§icesta = 'S'  OR
439000140528     C                               §icestaP = 'S' )
439100060622     c                   MOVEL     §icedei       wcdex
439200000000x   1c                   ELSE                                                   *non trovato
439300000000     c                   MOVEL     '1'           werr                           *SI errore
439400030130     c                   MOVEL     *blanks       wcdex
439500000000e   1c                   ENDIF
439600000000     c*
439700000000     c                   ENDSR
439800000000     c*--------------------------------------------------------------------------------------------*
439900000000     c* decodifica consegna particolare
440000000000     c*--------------------------------------------------------------------------------------------*
440100000000     c     decftc        BEGSR
440200000000     c*
440300000000     c                   MOVEL     *blanks       wdftc
440400000000     c*
440500000000if  1c                   IF        wftc = 'S'
440600060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
440700000000e   1c                   ENDIF
440800000000if  1c                   IF        wftc = 'P'
440900060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
441000000000e   1c                   ENDIF
441100000000if  1c                   IF        wftc = 'A'
441200060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
441300000000e   1c                   ENDIF
441400000000if  1c                   IF        wftc = 'F'
441500060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
441600000000e   1c                   ENDIF
441700000000     c*
441800000000     c                   ENDSR
441900000000     c*--------------------------------------------------------------------------------------------*
442000000000     c* decodifica turno di chiusura
442100000000     c*--------------------------------------------------------------------------------------------*
442200000000     c     decgcx        BEGSR
442300000000     c*
442400000000     c                   MOVEL     *blanks       wdgcx
442500000000     c* giorno
442600000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
442700000000if  1c                   IF        a1 = '1'
442800060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
442900000000e   1c                   ENDIF
443000000000if  1c                   IF        a1 = '2'
443100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
443200000000e   1c                   ENDIF
443300000000if  1c                   IF        a1 = '3'
443400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
443500000000e   1c                   ENDIF
443600000000if  1c                   IF        a1 = '4'
443700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
443800000000e   1c                   ENDIF
443900000000if  1c                   IF        a1 = '5'
444000060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
444100000000e   1c                   ENDIF
444200000000if  1c                   IF        a1 = '6'
444300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
444400000000e   1c                   ENDIF
444500000000if  1c                   IF        a1 = '7'
444600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
444700000000e   1c                   ENDIF
444800000000if  1c                   IF        a1 = ' '
444900000000     c                   EVAL      wdgcx = *blanks
445000000000e   1c                   ENDIF
445100000000     c* am/pm
445200000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
445300000000if  1c                   IF        a1 = ' '
445400060612     c                   EVAL      wdgcx =
445500060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
445600000000e   1c                   ENDIF
445700000000if  1c                   IF        a1 = 'M'
445800060612     c                   EVAL      wdgcx =
445900060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
446000000000e   1c                   ENDIF
446100000000if  1c                   IF        a1 = 'P'
446200060612     c                   EVAL      wdgcx =
446300060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
446400000000e   1c                   ENDIF
446500000000     c*
446600000000     c                   ENDSR
446700000000     c*--------------------------------------------------------------------------------------------*
446800000000     c* decodifica evento per GIACENZE
446900000000     c*--------------------------------------------------------------------------------------------*
447000000000     c     deccevgia     BEGSR
447100000000     c*
447200000000     c                   MOVEL     '0'           werr                           *NO errore
447300000000     c*
447400060622     C                   RESET                   tibs02ds
447500060622     C                   EVAL      t02Cod = 'ICE'
447600060622     C                   EVAL      t02Ke1 = wcev
447700060622     C                   EXSR      getTntbe00f
447800060622     C                   IF        t02Err <> 'E' AND                            *trovata
447900140528     C                             (§icegia = 'S'  OR
448000140528     C                               §icegiap = 'S')
448100060622     c                   MOVEL     §icedei       wcdex
448200000000x   1c                   ELSE                                                   *non trovato
448300000000     c                   MOVEL     '1'           werr                           *SI errore
448400030130     c                   MOVEL     *blanks       wcdex
448500000000e   1c                   ENDIF
448600000000     c*
448700000000     c                   ENDSR
448800000000     c*--------------------------------------------------------------------------------------------*
448900000000     c* Controlla la DS dsi Input
449000000000     c*--------------------------------------------------------------------------------------------*
449100000000     c     chkdsinput    BEGSR
449200000000     c*
449300000000     c                   MOVEL     '0'           werr                           *NO errore
449400000000     c*
449500000000     c* dalla DS di Input scompone il codice spedizione
449600000000     c                   MOVEL     nspedizi74    dsspedizi74
449700000000     c*
449800000000     c* controlla che la lina di partenza sia TUTTA numerica
449900000000     c                   SETOFF                                       99
450000000000     c                   TESTN                   dslnpi74             99
450100000000if  1c                   IF        NOT *in99                                    *non è un numero
450200000000     c                   MOVEL     '1'           werr                           *SI errore
450300140617     c                   eval      widmsg = 'TIS0769'
450400140617     c                   eval      widcampo = '*LNP'
450500140617     c                   clear                   wparm
450600140617     c                   exsr      messaggi
450700000000x   1c                   ELSE                                                   *è un numero
450800000000     c                   MOVE      dslnpi74      a1
450900000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
451000000000     c                   MOVEL     '1'           werr                           *SI errore
451100140617     c                   eval      widmsg = 'TIS0769'
451200140617     c                   eval      widcampo = '*LNP'
451300140617     c                   clear                   wparm
451400140617     c                   exsr      messaggi
451500000000e   2c                   ENDIF
451600000000e   1c                   ENDIF
451700000000     c*
451800000000     c* controlla che la serie spedizione sia TUTTA numerica
451900000000     c                   SETOFF                                       99
452000000000     c                   TESTN                   dsnrsi74             99
452100000000if  1c                   IF        NOT *in99                                    *non è un numero
452200000000     c                   MOVEL     '1'           werr                           *SI errore
452300140617     c                   eval      widmsg = 'TIS0769'
452400140617     c                   eval      widcampo = '*NRS'
452500140617     c                   clear                   wparm
452600140617     c                   exsr      messaggi
452700000000x   1c                   ELSE                                                   *è un numero
452800000000     c                   MOVE      dsnrsi74      a1
452900000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
453000000000     c                   MOVEL     '1'           werr                           *SI errore
453100140617     c                   eval      widmsg = 'TIS0769'
453200140617     c                   eval      widcampo = '*NRS'
453300140617     c                   clear                   wparm
453400140617     c                   exsr      messaggi
453500000000e   2c                   ENDIF
453600000000e   1c                   ENDIF
453700000000     c*
453800000000     c* controlla che il numero spedizione sia TUTTO numerico
453900000000     c                   SETOFF                                       99
454000000000     c                   TESTN                   dsnspi74             99
454100000000if  1c                   IF        NOT *in99                                    *non è un numero
454200000000     c                   MOVEL     '1'           werr                           *SI errore
454300140617     c                   eval      widmsg = 'TIS0769'
454400140617     c                   eval      widcampo = '*NSP'
454500140617     c                   clear                   wparm
454600140617     c                   exsr      messaggi
454700000000x   1c                   ELSE                                                   *è un numero
454800000000     c                   MOVE      dsnspi74      a1
454900000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
455000000000     c                   MOVEL     '1'           werr                           *SI errore
455100140617     c                   eval      widmsg = 'TIS0769'
455200140617     c                   eval      widcampo = '*NSP'
455300140617     c                   clear                   wparm
455400140617     c                   exsr      messaggi
455500000000e   2c                   ENDIF
455600000000e   1c                   ENDIF
455700110104     c*
455800110104     c* controlla che il codice cliente sia TUTTO numerico
455900110104     c                   IF        kscI74 <> *BLANK
456000110104     c                   SETOFF                                       99
456100110104     c                   TESTN                   kscI74               99
456200110104     c                   IF        NOT *in99                                    *non è un numero
456300110104     c                   MOVEL     '1'           werr                           *SI errore
456400140617     c                   eval      widmsg = 'TIS0769'
456500140617     c                   eval      widcampo = 'KSCI74'
456600140617     c                   clear                   wparm
456700140617     c                   exsr      messaggi
456800110104     c                   ELSE                                                   *è un numero
456900110104     c                   MOVE      kscI74        a1
457000110104     c                   IF        a1 < '0'                                     *trovata una lettera
457100110104     c                   MOVEL     '1'           werr                           *SI errore
457200140617     c                   eval      widmsg = 'TIS0769'
457300140617     c                   eval      widcampo = 'KSCI74'
457400140617     c                   clear                   wparm
457500140617     c                   exsr      messaggi
457600110104     c                   ENDIF
457700110104     c                   ENDIF
457800110104     c                   ENDIF
457900110104     c*
458000110104     c* controlla che il codice mittente sia TUTTO numerico
458100110104     c                   IF        ccmI74 <> *BLANK
458200110104     c                   SETOFF                                       99
458300110104     c                   TESTN                   ccmI74               99
458400110104     c                   IF        NOT *in99                                    *non è un numero
458500110104     c                   MOVEL     '1'           werr                           *SI errore
458600140617     c                   eval      widmsg = 'TIS0769'
458700140617     c                   eval      widcampo = 'CCMI74'
458800140617     c                   clear                   wparm
458900140617     c                   exsr      messaggi
459000110104     c                   ELSE                                                   *è un numero
459100110104     c                   MOVE      ccmI74        a1
459200110104     c                   IF        a1 < '0'                                     *trovata una lettera
459300110104     c                   MOVEL     '1'           werr                           *SI errore
459400140617     c                   eval      widmsg = 'TIS0769'
459500140617     c                   eval      widcampo = 'CCMI74'
459600140617     c                   clear                   wparm
459700140617     c                   exsr      messaggi
459800110104     c                   ENDIF
459900110104     c                   ENDIF
460000110104     c                   ENDIF
460100050104     c*
460200050104     c* controlla anno spedizione
460300050104     c                   TESTN                   cAASI74              99
460400050104if  1c                   IF        NOT *in99                                    *non è un numero
460500050104     c                   EVAL      cAASI74 = *ZERO
460600050104x   1c                   ENDIF
460700100222     c*
460800100223     c* Estraggo l'anno dalla data spedizione.
460900100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
461000100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
461100100223     C                   IF        dspI74 <> *BLANK
461200100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
461300100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
461400100223     C                   MONITOR
461500100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
461600100223     C                   ON-ERROR
461700100223     C                   ENDMON
461800100222     C                   ENDIF
461900170314
462000170314     c*  BRTCODE
462100170314     c                   clear                   wbrtcode
462200170315     c                   if        versioni74>='D'
462300170314     c                   eval      wbrtcode=brtcodei74
462400170314     c                   if        wbrtcode<>*blanks
462500171128     c* Errore se non è lungo 19
462600171128         // -?Calcolo del numero dei caratteri ricevuti?
462700171128         wLen = %len( %trimr(wbrtcode) );
462800171128         if wlen<19;
462900171128             werr='1' ;
463000171128             wIdMsg   = 'TIS0737';
463100171128             widcampo = 'BRTCODEI74' ;
463200171128             wparm = wbrtcode ;
463300171128             exsr Messaggi;
463400171128          endif;
463500171128
463600170314     c                   exsr      sr_trul28
463700170314     c                   endif
463800170315     c
463900170315     c                   endif
464000000000     c*
464100000000     c* se errore, esce dal programma
464200140610if  1c                   IF        werr = '1'                                   *SI errore
464300140610     c                   eval      esito = werr                                 *errore
464400140610e   1c                   ENDIF
464500000000     c*
464600000000     c                   ENDSR
464700170314      /FREE
464800170314       //--------------------------------------------------------------
464900170314       //?Controllo correttezza BRTCODE
465000170314       //--------------------------------------------------------------
465100170314       BEGSR  sr_trul28 ;
465200170314          clear trul28ds0;
465300170314          i280cod=wbrtcode;
465400170314          trul28r0(trul28ds0);
465500170314          if o280err='1';
465600170314             werr='1' ;
465700170314             wIdMsg   = 'TIS0737';
465800170314             widcampo = 'BRTCODEI74' ;
465900170314             wparm = wbrtcode ;
466000170314             exsr Messaggi;
466100170314          endif;
466200170314       ENDSR;
466300170314      /end-FREE
466400000000     c*--------------------------------------------------------------------------------------------*
466500000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
466600000000     c* Input:   Walfa7 = Alfanumerico di 7
466700000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
466800000000     c*--------------------------------------------------------------------------------------------*
466900000000     c     xallinea1     BEGSR
467000000000     c*
467100000000     c* reimposta variabli di lavoro
467200000000     c                   CLEAR                   i
467300000000     c                   CLEAR                   z
467400000000     c                   CLEAR                   walfa7bis
467500000000     c                   EVAL      wzeri = *ALL'0'
467600000000     c*
467700000000     c     ' '           CHECKR    walfa7:7      i
467800000000if  1c                   IF        i <> 0
467900000000     c     i             SUBST     walfa7        walfa7bis
468000000000e   1c                   ENDIF
468100000000if  1c                   IF        walfa7bis <> *blanks
468200000000     c     7             SUB       i             z
468300000000if  2c                   IF        z <> 0
468400000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
468500000000e   2c                   ENDIF
468600000000     c                   EVAL      z = z+ 1
468700000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
468800000000e   1c                   ENDIF
468900000000     c*
469000000000     c                   ENDSR
469100000000     c*--------------------------------------------------------------------------------------------*
469200000000     c* caricamento tabelle occorrenti
469300000000     c*--------------------------------------------------------------------------------------------*
469400000000     c     cartab        BEGSR
469500000000     c*---
469600000000     c* organigrama
469700000000     c*---
469800000000     c                   CLEAR                   i                              *indice
469900000000     C                   CLEAR                   sorg
470000000000     C                   CLEAR                   sodes
470100000000     C                   CLEAR                   sofl1
470200000000     C                   CLEAR                   sodpd
470300060623     C                   OPEN      tntbe01l
470400000000     c     *loval        SETLL     azorg01l
470500000000     c                   READ      azorg01l                               99
470600000000do  1c                   DOW       NOT *in99
470700000000     c                   ADD       1             i
470800000000     c                   Z-ADD     orgfil        sorg(i)
470900000000     c                   MOVEL     orgdes        sodes(i)
471000000000     c                   MOVEL     orgde3        og143
471100020702     C*
471200020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
471300020702     C                   CLEAR                   dntw
471400020702     C*
471500020702     C                   MOVEL(P)  'NTW'         kbecod
471600020702     C                   MOVEL(P)  §ogntw        kbeke1
471700020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
471800020702     c                   MOVEL     *blanks       kbelin                         *lingua
471900020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
472000020702     C     keytbe        CHAIN     tntbe01l
472100020702     C                   IF        %found(tntbe01l)
472200020702     C                   MOVEL     tbeuni        dntw
472300020702     C                   ENDIF
472400020702     C*
472500020702     C                   MOVEL     §ntwfie       sofl1(i)
472600160406     C*
472700020702     c                   MOVEL     §ogntw        sodpd(i)
472800160406     C*
472900000000     c                   READ      azorg01l                               99
473000000000e   1c                   ENDDO
473100060623     C                   CLOSE     tntbe01l
473200000000     c*---
473300000000     c                   ENDSR
473400000000     c*--------------------------------------------------------------------------------------------*
473500000000     c* operazioni iniziali -da fare sempre-
473600000000     c*--------------------------------------------------------------------------------------------*
473700000000     c     rutinz        BEGSR
473800000000     c*
473900000000     c* reperimento data corrente
474000000000     C                   TIME                    n14                            *ora (6)+ data (8)
474100000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
474200000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
474300000000     C                   Z-ADD     n8            g08dat
474400000000     C                   Z-ADD     *zeros        g08inv
474500000000     C                   MOVEL     '0'           g08err
474600000000     c                   CALL      'XSRDA8'
474700000000     c                   PARM                    wlbda8
474800000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
474900000000     C                   MOVEL     datcor        anncor                         *anno corrente
475000000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
475100000000     c*
475200000000     c* calcola data corrente - 1 anno
475300000000     c                   Z-ADD     datcor        datpre                         *data precedente
475400000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
475500000000     c*
475600010716     c* azzera la DS di Output e le variabili di lavoro / controllo
475700000000     c                   MOVEL     '0'           esito
475800140603     c                   reset                   tis778dso                      *ds Output
475900140603     c                   reset                   tis778dsm                      *ds Output
476000000000     c                   CLEAR                   smi                            *di memorizzazione
476100000000     c                   CLEAR                   smseql
476200000000     c                   CLEAR                   smseqe
476300000000     c                   CLEAR                   smdevhev
476400000000     c                   CLEAR                   smdeve
476500000000     c                   CLEAR                   smheve
476600000000     c                   CLEAR                   smfle
476700000000     c                   CLEAR                   smdev
476800000000     c                   CLEAR                   smcev
476900130312     c                   CLEAR                   smspe
477000000000     c                   CLEAR                   smdmc
477100000000     c                   CLEAR                   smdmcR
477200000000     c                   CLEAR                   tote                           *contatore eventi
477300000000     c                   CLEAR                   seql                           *sequenza legami
477400010716     c                   RESET                   $AbTD                          *di lavoro
477500010716     c                   RESET                   $AbFirma
477600150312     c                   clear                   pdaavv
477700150421     c                   clear                   pdagia
477800140603     c* parametri ingresso
477900140603     c                   exsr      Kontrolla
478000000000     c* controlla da DS in Input
478100000000     c                   EXSR      chkdsinput
478200000000     c*
478300000000     c* carica i clienti dettaglio del cliente unificante
478400000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
478500000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
478600000000     c     keyvss        SETLL     tivss02l
478700000000     c     keyvss        READE     tivss02l                               99
478800000000do  1c                   DOW       NOT *in99
478900000000if  2c                   IF        vssvat = *blanks                             *attivo
479000000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
479100000000     c                             datcor <= vssdsc
479200010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
479300010719     c* --> TD per vedere la firma DPD
479400010716     c                   RESET                   $AbTD                          *di lavoro
479500010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
479600010719     c     kssksu        SETLL     tivss02l
479700010719     c     kssksu        READE     tivss02l                               99
479800010719do  4c                   DOW       NOT *in99
479900010716if  5c                   IF        vssvat = *blanks                             *attivo
480000010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
480100010716     c                             datcor <= vssdsc
480200010719if  7c                   IF        vssisv = 'TD'
480300010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
480400010719e   7c                   ENDIF
480500010719e   6c                   ENDIF
480600010716e   5c                   ENDIF
480700010719     c     kssksu        READE     tivss02l                               99
480800010719e   4c                   ENDDO
480900010716     c*
481000000000     c                   LEAVE                                                  *uscita ciclo
481100000000e   3c                   ENDIF
481200000000e   2c                   ENDIF
481300000000     c     keyvss        READE     tivss02l                               99
481400010716e   1c                   ENDDO                                                  *fine cl.abil.TT
481500000000     c*
481600060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
481700060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
481800151216     c* Se lo strategi user number è = *blanks --> lo imposto a *zeros
481900151216     c                   monitor
482000151216     c                   if        RQSCIDI174>0
482100151216     c                   endif
482200151216     c
482300151216     c                   on-error
482400151216     c                   clear                   RQSCIDI174
482500151216     c                   endmon
482600060621     c*
482700000000     c                   ENDSR
482800040330
482900040330     ***********************************************************************************************
483000040330     **?
483100040330     **?Reperimento ulteriori dati destinatario.
483200040330     **?
483300040330     ***********************************************************************************************
483400040330     C     ImpDSDest     BEGSR
483500040330
483600140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
483700140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
483800100121     C                             ar5Trd <> 'GEN'
483900140716     c                   Z-ADD     f_tasaas      kr5aas
484000140716     c                   Z-ADD     f_taslnp      kr5lnp
484100140716     c                   Z-ADD     f_tasnrs      kr5nrs
484200140716     c                   Z-ADD     f_tasnsp      kr5nsp
484300040330     c                   MOVEL     'GEN'         kr5trd
484400040531     c     keyar5        CHAIN     fiar531c
484500040330     C                   IF        %FOUND
484600040330     C                   EVAL      DAR5GEN = AR5Uni
484700100121     C                   ELSE
484800100121     C                   RESET                   DAR5GEN
484900040330     C                   ENDIF
485000100121     C                   ENDIF
485100100121     C
485200100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
485300100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
485400040330
485500040330     C                   ENDSR
485600040913
485700040913     ***********************************************************************************************
485800040913     **?
485900040913     **?Determino se addebitare l'immagine LDV.
486000040913     **?
486100040913     ***********************************************************************************************
486200040913     C     AddebitoLDV   BEGSR
486300040913
486400040913     C                   IF        LDVO74 = 'S'
486500040913
486600040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
486700040913
486800040913     C                   ENDIF
486900040913
487000040913     C                   ENDSR
487100051110
487200051110     ***********************************************************************************************
487300051110     **?
487400051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
487500051110     **?
487600051110     ***********************************************************************************************
487700051110     C     chkTivpi      BEGSR
487800051110
487900051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
488000051110     C                             esito10)
488100051110     C                   IF        esito10 > 0 AND NOT %ERROR
488200051110     C                   EVAL      gcfdio74 = 'N'
488300051110     C                   ENDIF
488400051110
488500051110     C                   ENDSR
488600051222
488700051222     ***********************************************************************************************
488800051222     **
488900051222     ** Linea arrivo Italia o estera.
489000051222     **
489100051222     ***********************************************************************************************
489200051222     C     setLnaItaEst  BEGSR
489300051222
489400051222     C                   RESET                   wrkLnaItaEst
489500051222     C                   EXSR      decOrg
489600051222     C                   EVAL      wrkLnaItaEst = wofl1
489700051222
489800051222     C                   ENDSR
489900060622
490000060622     ***********************************************************************************************
490100150615     ** Reperisco informazioni stato bolla
490200060622     ***********************************************************************************************
490300130312     C     srarb         BEGSR
490400130312     c                   clear                   wcev
490500140714     c                   clear                   wricons
490600130312     c                   clear                   worg
490700130312     c                   clear                   wdat
490800130312     c                   clear                   wora
490900150615     c                   clear                   ds2a
491000130312     c                   eval      dsevbspe = spe(1)
491100130312     c     karb          chain     fnarb01l
491200130312     c                   if        not %error and
491300130312     c                             %found(fnarb01l)
491400130422     c                   Z-ADD     arblna        worg
491500130312     c                   exsr      setLnaItaEst
491600130312     c* no per estero
491700130312     C                   IF        wrkLnaItaEst = ESTERO
491800130312     c                   leavesr
491900130312     c                   end
492000150617     c
492100130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
492200130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
492300130312     C                   SELECT
492400141120     c                   WHEN      arbdcm <> 0
492500130312     c                   MOVEL     '704'         wcev                           *causale evento
492600130313     c                   move      arbdcm        WDAT
492700130313     c                   move      arbhmc        WORA
492800150617     c
492900130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
493000150615     c                   WHEN      ARBFDC = ' ' and arbndc>0
493100130326     c* solo se distinta NON in test
493200130326     c                   clear                   ddstflr
493300130326     c                   z-add     arbifp        dstfgs
493400130326     c                   z-add     arbndc        dstnfv
493500130326     c     kdst          chain     fidst01l
493600130326     c                   if        not %error and
493700130326     c                             %found(fidst01l)
493800130326     c                   eval      ddstflr = dstflr
493900130326     c                   end
494000130327     c                   if        §DSTTSTPDA = 'C' or
494100130327     c                             §DSTTSTPDA = 'E'
494200130326     c                   leavesr
494300130326     c                   end
494400130326     c*
494500150617     c* Cerco PRIMO evento da PdA
494600130313     c                   move      arbndc        pctndc
494700131105     c                   exsr      cercaceP
494800150617     c*
494900150617     c* Cerco ULTIMO evento da PdA
495000131105     c                   move      'CET'         ttrd
495100130312     c     kcet          setgt     fipct02l
495200130312     c     kcet          readpe    fipct02l
495300130312     c                   if        not %error and
495400130312     c                             not %eof(fipct02l)
495500130312     c                   movel     pctdati       fipctcetds
495600140926      * firmatario se c'è riempie il campo
495700140926     c                   if        §pctnomfir <> *blank
495800140926     c                   eval      firmato74  = §pctnomfir
495900140926     c                   endif
496000130312     c* consegnata
496100130312     c                   if        §pctcmc = ' '
496200130312     c                   MOVEL     '704'         wcev                           *causale evento
496300130312     c                   else
496400140711     c                   move      'S'           wricons
496500130312     c* mancata consegna
496600130313     c                   movel     §PCTcmc       wcev
496700130312     c                   clear                   ds2a
496800130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
496900130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
497000130312     C                             :rpyEsito)
497100130312     c                   if        §2aflg = '§' and §2acpt = 'S'
497200130312     c                   move      'T  '         wcev
497300130312     c                   endif
497400130312     c                   endif
497500130312     c* ??? £6
497600130312     c                   if        §pctdata > 0
497700130313     c                   move      §PCTDATA      WDAT
497800130312     c                   movel     §PCTORA       WORA
497900130312     c                   endif
498000130312     c                   end
498100130312     c                   endSL
498200130312     c                   end
498300130312     c* ho trovato un evento da inserire  al 1° posto
498400130312     c                   if        wcev <> ' '
498500130312     c                   EXSR      deccevsta
498600130312     c                   IF        werr <> '1'                                  *no errore
498700130312     c                   EXSR      decorg
498800130312     c                   EXSR      edtdat
498900130312     c                   EXSR      edtora
499000130313     c* libero il 1° elemnto di schiera
499100150617     c                   eval      j = 49
499200150617     c                   for       j = 49 downto 1
499300130312     c                   if        cev(j) <> ' '
499400130312     c                   eval      cev(j+1) = cev(j)
499500130312     c                   eval      dev(j+1) = dev(j)
499600130312     c                   eval      hev(j+1) = hev(j)
499700130312     c                   eval      fle(j+1) = fle(j)
499800130312     c                   eval      eve(j+1) = eve(j)
499900130312     c                   end
500000130313     c                   endfor
500100130313     c                   eval      dev(1) = wdate
500200130313     c                   eval      hev(1) = worae
500300130312     c                   eval      cev(1)= wcev
500400130313     c                   eval      fle(1) = wodes
500500130313     c                   eval      eve(1) = wcdex
500600130313     c                   EVAL      cntEveO74 += 1
500700150310     c*
500800150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
500900150310     c                   if        §2aftc='A'
501000150310     c                   eval      pdaavv='S'
501100150310     c                   endif
501200150421     c                   if        §2aftc='G'
501300150421     c                   eval      pdagia='S'
501400150421     c                   endif
501500150310     c
501600130312     c                   end
501700130312     c                   end
501800130312     C                   ENDSR
501900140714     **_________________________________________________________
502000140714     c     cercafiglia   begsr
502100140714     **_________________________________________________________
502200140715     c                   clear                   keyForzata
502300140714      * salva chiave originale per eventi
502400140715     c                   eval      kasaasf = kasaas
502500140715     c                   eval      kaslnpf = kaslnp
502600140715     c                   eval      kasnrsf = kasnrs
502700140715     c                   eval      kasnspf = kasnsp
502800140714     c                   do        *hival
502900140715     c     keytasf       chain     fnlbl02l
503000140714     c                   if        %found(fnlbl02l)
503100140715     c                   eval      kasaasf = LBLaan
503200140715     c                   eval      kaslnpf = LBLlpn
503300140715     c                   eval      kasnrsf = LBLnrn
503400140715     c                   eval      kasnspf = LBLnsn
503500140714     c                   move      'X'           keyForzata
503600140714     c                   iter
503700140714     c                   else
503800140714     c                   leave
503900140714     c                   endif
504000140714     c                   enddo
504100140715     c     keytasf       setll     titas30c                               99    *no errore
504200140715     c     keytasf       READE     titas30c                               99    *no errore
504300141117      *se non trova titas con figlia forza di nuovo la mamma
504400140715if  3c                   IF        *in99                                        *non trovato
504500141117     c                   clear                   titasdssavf
504600140715x   3c                   ELSE                                                   *trovata
504700140715if  4c                   IF        *in01                                        *lettura titas000
504800140715     c                   EVAL      titasdssavf = titasds
504900140715e   4c                   ENDIF
505000140715if  4c                   IF        *in02                                        *lettura titas010
505100140715     c                   EVAL      titasdssavf = titasds
505200140715e   4c                   ENDIF
505300140715if  4c                   IF        *in03                                        *lettura titasp00
505400140715     c                   EVAL      titasdssavf = titasds
505500140715e   4c                   ENDIF
505600140715e   3c                   ENDIF
505700140714     C                   ENDSR
505800160318     **_________________________________________________________
505900160318     c     cercamamma    begsr
506000160318     **_________________________________________________________
506100160318     c                   clear                   keyForzata
506200160318     c                   EVAL      kblaan = arbaas
506300160318     c                   EVAL      kbllpn = arblnp
506400160318     c                   EVAL      kblnrn = arbnrs
506500160318     c                   EVAL      kblnsn = arbnsp
506600160318     c     keylbl        CHAIN     fnlbl01l                           99
506700160318if  1c                   IF        NOT *in99                                    *ha la mamma
506800160318     c                   EVAL      kasaas = lblaap
506900160318     c                   EVAL      kaslnp = lbllpp
507000160318     c                   EVAL      kasnrs = lblnrp
507100160318     c                   EVAL      kasnsp = lblnsp
507200160318     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
507300160318if  3c                   IF        not *in99                                        *non trovato
507400160318     c                   move      'X'           keyForzata
507500160318     c                   eval      esito=' '
507600160408     c                   clear                   titasdssavf
507700160318     c                   endif
507800160318     c                   endif
507900160318     c
508000160318     c                   ENDSR
508100131105     **_________________________________________________________
508200131105     c     cercaCEP      begsr
508300131105     **_________________________________________________________
508400131105     c                   move      'CEP'         ttrd
508500131105     c     kcet          setgt     fipct02l
508600131105     c     kcet          readpe    fipct02l
508700131105     c                   if        not %error and
508800131105     c                             not %eof(fipct02l)
508900131105     c                   movel     pctdati       fipctcetds
509000131105     c* consegnata
509100131105     c                   if        §pctcmc = ' '
509200131105     c                   MOVEL     '704'         wcev                           *causale evento
509300131105     c                   else
509400131105     c* mancata consegna
509500131105     c                   movel     §PCTcmc       wcev
509600131105     c                   clear                   ds2a
509700131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
509800131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
509900131105     C                             :rpyEsito)
510000131105     c                   if        §2aflg = '§' and §2acpt = 'S'
510100131105     c                   move      'T  '         wcev
510200131105     c                   endif
510300131105     c                   endif
510400131105     c* ??? £6
510500131105     c                   if        §pctdata > 0
510600131105     c                   move      §PCTDATA      WDAT
510700131105     c                   movel     §PCTORA       WORA
510800131105     c                   endif
510900131105     c                   end
511000131105     c* ho trovato un evento da inserire  al 1° posto
511100131105     c                   if        wcev <> ' '
511200131105     c                   EXSR      deccevsta
511300131105     c                   IF        werr <> '1'                                  *no errore
511400131105     c                   EXSR      decorg
511500131105     c                   EXSR      edtdat
511600131105     c                   EXSR      edtora
511700131105     c* libero il 1° elemnto di schiera
511800150617     c                   eval      j = 49
511900150617     c                   for       j = 49 downto 1
512000131105     c                   if        cev(j) <> ' '
512100131105     c                   eval      cev(j+1) = cev(j)
512200131105     c                   eval      dev(j+1) = dev(j)
512300131105     c                   eval      hev(j+1) = hev(j)
512400131105     c                   eval      fle(j+1) = fle(j)
512500131105     c                   eval      eve(j+1) = eve(j)
512600131105     c                   end
512700131105     c                   endfor
512800131105     c                   eval      dev(1) = wdate
512900131105     c                   eval      hev(1) = worae
513000131105     c                   eval      cev(1)= wcev
513100131105     c                   eval      fle(1) = wodes
513200131105     c                   eval      eve(1) = wcdex
513300131105     c                   EVAL      cntEveO74 += 1
513400131105     c                   end
513500131105     c                   end
513600131105     c                   endsr
513700131105     ***********************************************************************************************
513800130312     **
513900130312     ** Reperisco tabella da TNTBE00F.
514000130312     **
514100130312     ***********************************************************************************************
514200130312     C     getTntbe00f   BEGSR
514300060622     C                   EVAL      t02Lin = langTntbe
514400060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
514500060622     C                   SELECT
514600060622     C                   WHEN      t02Cod = 'ICE'
514700060622     C                   EVAL      dice = t02Uni
514800060622     C                   WHEN      t02Cod = 'CCH'
514900060622     C                   EVAL      dcch = t02Uni
515000060622     C                   WHEN      t02Cod = 'NTW'
515100060622     C                   EVAL      dntw = t02Uni
515200100119     C                   WHEN      t02Cod = 'I1A'
515300100119     C                   EVAL      dI1a = t02Uni
515400150304     C                   WHEN      t02Cod = 'SPC'
515500150304     C                   EVAL      dSPC = t02Uni
515600170301     C                   WHEN      t02Cod = 'CLI'
515700170301     C                   EVAL      dcli = t02Uni
515800060622     C                   ENDSL
515900060622     C                   ENDSR
516000060608
516100140529      /free
516200140529       //--------------------------------------------------------------
516300140529       //?Controllo formale dei dati passati.
516400140529       //--------------------------------------------------------------
516500140603       BEGSR  Kontrolla;
516600140529
516700140529       //?OpCode
516800140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
516900160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
517000140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
517100160818           esito='1'    ;
517200140529           exsr RoutEnd;
517300140529         ENDIF;
517400140603           IF  prmRqsDataSize > 0;
517500140603           ELSE;
517600160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
517700140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
517800160818             esito='1'    ;
517900140603             exsr RoutEnd;
518000140603           ENDIF;
518100140529
518200140529       //?Formato e size RQS
518300140603           IF  prmRqsFormato = formatoi74;
518400140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
518500140529           ELSE;
518600160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
518700140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
518800160818             esito='1'    ;
518900140529             exsr RoutEnd;
519000140529           ENDIF;
519100140529         //?Formato e size RPY
519200140603           IF  prmRpyFormato = formatoo74;
519300140529           ELSE;
519400160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
519500140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
519600160818             esito='1'    ;
519700140529             exsr RoutEnd;
519800140529           ENDIF;
519900140529           IF  prmRpyDataSize > 0;
520000140529           ELSE;
520100160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
520200140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
520300160818             esito='1'    ;
520400140529             exsr RoutEnd;
520500140529           ENDIF;
520600140616       //?Formato e size MSG
520700140617         if %parms > 9 ;
520800140617          IF  prmRpyFormMsg = formato;
520900140617          ELSE;
521000160818           // prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
521100140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
521200160818           esito='1'    ;
521300140616           exsr RoutEnd;
521400140617          ENDIF;
521500140617          IF  prmRpyMsgSize > 0;
521600140617          ELSE;
521700160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
521800140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
521900160818           esito='1'    ;
522000140616           exsr RoutEnd;
522100140617          ENDIF;
522200140617         ENDIF;
522300140529
522400140529       ENDSR;
522500140529
522600140616       //--------------------------------------------------------------
522700140617       //?Routine per schierare i messaggi nella ds tis778dsm
522800140616       //--------------------------------------------------------------
522900140616       BEGSR  Messaggi;
523000140616
523100140616       //?Se ho già caricato 99 messaggi esco
523200140616         IF  nrmsg = 99;
523300160818           esito='1'  ;
523400140616           exsr RoutEnd;
523500140616           clear nrmsg;
523600140616         ENDIF;
523700140616
523800140616         nrmsg += 1;
523900140616         skIdMsg(nrmsg) = wIdMsg;
524000140616         skIdCampo(nrmsg) = wIdCampo;
524100140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
524200140616         IF  wParm <> *blanks;
524300140616           skTextMsg (nrmsg) =
524400140616           rtvMsgLang(wIdMsg:langi74:wParm);
524500140616         ELSE;
524600140616           skTextMsg (nrmsg)=
524700140616           rtvMsgLang(wIdMsg:langi74);
524800140616         ENDIF;
524900140616
525000140616       ENDSR;
525100140616
525200140529       //--------------------------------------------------------------
525300140529       BEGSR  RoutEnd;
525400140530
525500140530
525600160322        if  esito <> '0' and esito<>'P';
525700140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
525800140530             else;
525900140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
526000140530        endif ;
526100140529
526200140530       //?ritorno
526300140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
526400140617         if %parms > 9 ;
526500140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
526600140617         endif;
526700140529         return;
526800140529
526900140529       ENDSR;
527000140529      /end-free
527100000000     c*--------------------------------------------------------------------------------------------*
527200000000     c* operazioni iniziali
527300000000     c*--------------------------------------------------------------------------------------------*
527400000000     c     *inzsr        BEGSR
527500000000     c*
527600000000     c* ricevimento parametri
527700140603     c*m   *ENTRY        PLIST
527800140529     c*m                 PARM                    esito
527900140529     c*m                 PARM                    tis174dsi
528000140529     c*m                 PARM                    tis174dso
528100000000     c*
528200000000     c* chiavi di lettura
528300000000     c     keytas        KLIST                                                  titas30c
528400000000     c                   KFLD                    kasaas                         -anno spedizione
528500000000     c                   KFLD                    kaslnp                         -linea partenza
528600000000     c                   KFLD                    kasnrs                         -serie
528700000000     c                   KFLD                    kasnsp                         -spedizione
528800140715     c     keytasf       KLIST                                                  titas30c
528900140715     c                   KFLD                    kasaasf                        -anno spedizione
529000140715     c                   KFLD                    kaslnpf                        -linea partenza
529100140715     c                   KFLD                    kasnrsf                        -serie
529200140715     c                   KFLD                    kasnspf                        -spedizione
529300130312     c     karb          KLIST                                                  titas30c
529400140716     c                   KFLD                    f_tasaas                         -anno spedizione
529500140716     c                   KFLD                    f_taslnp                         -linea partenza
529600140716     c                   KFLD                    f_tasnrs                         -serie
529700140716     c                   KFLD                    f_tasnsp                         -spedizione
529800130326     C     Kdst          KLIST
529900130326     C                   KFLD                    dstnpg
530000130326     C                   KFLD                    dstnfv
530100130326     C                   KFLD                    dstfgs
530200130326     c                   z-add     4             dstnpg
530300130326     C     Kcet          KLIST
530400130326     C                   KFLD                    arbLNA
530500130326     C                   KFLD                    pctNDC
530600130326     C                   KFLD                    arbpdc
530700131105     C                   KFLD                    TTRD              3
530800130312     C                   KFLD                    arbAAS
530900130312     C                   KFLD                    arbLNP
531000130312     C                   KFLD                    arbNRS
531100130312     C                   KFLD                    arbNSP
531200150403     C     KARP          KLIST
531300150403     C                   KFLD                    arbAAS
531400150403     C                   KFLD                    arbLNP
531500150403     C                   KFLD                    arbNRS
531600150403     C                   KFLD                    arbNSP
531700000000     c     keytaa        KLIST                                                  titaa30c
531800000000     c                   KFLD                    kaaaas                         -anno spedizione
531900000000     c                   KFLD                    kaalnp                         -linea partenza
532000000000     c                   KFLD                    kaanrs                         -serie
532100000000     c                   KFLD                    kaansp                         -spedizione
532200000000     c                   KFLD                    kaatrc                         -tipo anagrafica
532300000000     c     keyta4        KLIST                                                  tita430c
532400000000     c                   KFLD                    ka4aas                         -anno spedizione
532500000000     c                   KFLD                    ka4lnp                         -linea partenza
532600000000     c                   KFLD                    ka4nrs                         -serie
532700000000     c                   KFLD                    ka4nsp                         -spedizione
532800000000     c                   KFLD                    ka4trc                         -tipo riferimento
532900010925     c     ke2ta4        KLIST                                                  tita430c
533000010925     c                   KFLD                    ka4aas                         -anno spedizione
533100010925     c                   KFLD                    ka4lnp                         -linea partenza
533200010925     c                   KFLD                    ka4nrs                         -serie
533300010925     c                   KFLD                    ka4nsp                         -spedizione
533400000000     c     keyevb        KLIST                                                  fnevb01l
533500000000     c                   KFLD                    kvbaas                         -anno spedizione
533600000000     c                   KFLD                    kvblnp                         -linea partenza
533700000000     c                   KFLD                    kvbnrs                         -serie
533800000000     c                   KFLD                    kvbnsp                         -spedizione
533900010717     c     ke2evb        KLIST                                                  fnevb01l
534000010717     c                   KFLD                    kvbaas                         -anno spedizione
534100010717     c                   KFLD                    kvblnp                         -linea partenza
534200010717     c                   KFLD                    kvbnrs                         -serie
534300010717     c                   KFLD                    kvbnsp                         -spedizione
534400010717     c                   KFLD                    kvbdev                         -data evento
534500010717     c                   KFLD                    kvbhev                         -ora  evento
534600000000     c     keylbl        KLIST                                                  fnlbl01l
534700000000     c                   KFLD                    kblaan                         -anno seguente
534800000000     c                   KFLD                    kbllpn                         -linea seguente
534900000000     c                   KFLD                    kblnrn                         -serie seguente
535000000000     c                   KFLD                    kblnsn                         -spedizione seguente
535100000000     c     ke2lbl        KLIST                                                  fnlbl02l
535200000000     c                   KFLD                    kblaap                         -anno precedente
535300000000     c                   KFLD                    kbllpp                         -linea precedente
535400000000     c                   KFLD                    kblnrp                         -serie precedente
535500000000     c                   KFLD                    kblnsp                         -spediz.precedente
535600000000     c     keytbe        KLIST                                                  *tntbe01l
535700000000     c                   KFLD                    kbecod                          -tabella
535800000000     c                   KFLD                    kbeke1                          -chiave uno
535900000000     c                   KFLD                    kbeke2                          -chiave due
536000000000     c                   KFLD                    kbelin                          -lingua
536100000000     c                   KFLD                    kbesif                          -s.informativo
536200000000     c     keyvss        KLIST                                                  *tivss02l
536300000000     c                   KFLD                    kssksu                          -cl.unificante
536400000000     c                   KFLD                    kssisv                          -tipo servizio
536500010717     c     keyvtd        KLIST                                                  *tivtd01l
536600010717     c                   KFLD                    ktdksu                          -cl.unificante
536700000000     c     keycsb        KLIST                                                  *tncsb03l
536800000000     c                   KFLD                    ksbaas                         -anno spedizione
536900000000     c                   KFLD                    ksblnp                         -linea partenza
537000000000     c                   KFLD                    ksbnrs                         -serie
537100000000     c                   KFLD                    ksbnsp                         -spedizione
537200040906     c     keydct        KLIST
537300040906     c                   KFLD                    DCTAAC
537400040906     c                   KFLD                    DCTFIL
537500040906     c                   KFLD                    DCTNCA
537600000000     c     keygcp        KLIST                                                  *figcp01l
537700000000     c                   KFLD                    kcpaas                         -anno spedizione
537800000000     c                   KFLD                    kcplnp                         -linea partenza
537900000000     c                   KFLD                    kcpnrs                         -serie
538000000000     c                   KFLD                    kcpnsp                         -spedizione
538100000000     c                   KFLD                    kcpfrg                         -prg aperture
538200050404     c     K04GCP51      KLIST
538300050404     c                   KFLD                    kcpaas                         -anno spedizione
538400050404     c                   KFLD                    kcplnp                         -linea partenza
538500050404     c                   KFLD                    kcpnrs                         -serie
538600050404     c                   KFLD                    kcpnsp                         -spedizione
538700000000     c     keygnp        KLIST                                                  *fignp01l
538800000000     c                   KFLD                    knpagc                         -anno apertura
538900000000     c                   KFLD                    knpfgc                         -filiale apertura
539000000000     c                   KFLD                    knpngc                         -numero giacenza
539100000000     c                   KFLD                    knpfrg                         -prg apertura
539200040531     c     keyar5        KLIST                                                  *fiar531c
539300030206     c                   KFLD                    kr5aas                         -anno spedizione
539400030206     c                   KFLD                    kr5lnp                         -linea partenza
539500030206     c                   KFLD                    kr5nrs                         -serie
539600030206     c                   KFLD                    kr5nsp                         -spedizione
539700030206     c                   KFLD                    kr5trd                         -tipo record
539800170301     c     ktfntc        KLIST                                                  titas30c
539900170301     c                   KFLD                    k_NTCAPL
540000170301     c                   KFLD                    k_NTCnk1
540100170301     c                   KFLD                    k_NTCnk2
540200170301     c                   KFLD                    k_NTCtnt
540300040907
540400000000     c* apre i files
540500000000     c                   OPEN      titas30c
540600000000     c                   OPEN      titaa30c
540700000000     c                   OPEN      tita430c
540800000000     c                   OPEN      fnevb01l
540900000000     c                   OPEN      tivss02l
541000010716     c                   OPEN      tivtd01l
541100000000     c                   OPEN      azorg01l
541200170301     c                   OPEN      tfntc01l
541300000000     c                   OPEN      fnlbl01l
541400000000     c                   OPEN      fnlbl02l
541500050309     c                   OPEN      tigcp51l
541600050309     c                   OPEN      tignp01l
541700000000     c                   OPEN      tncsb03l
541800040906     c                   OPEN      fndct01l
541900040531     c                   OPEN      fiar531c
542000130312     c                   OPEN      fnarb01l
542100160127     c                   OPEN      fnblp01l
542200160309     c                   OPEN      fiar901l
542300150403     c                   OPEN      fiarp01l
542400140606     c                   OPEN      fiar501l
542500140606     c                   OPEN      fiar401l
542600130326     c                   OPEN      fidst01l
542700130312     c                   OPEN      fipct02l
542800060621     C                   CALLP     openTabel00f()
542900000000     c*
543000000000     c* caricamento tabelle occorrenti
543100000000     c                   EXSR      cartab
543200060621     C                   CALLP     inzLingue()
543300000000     c*
543400000000     c                   ENDSR
543500070109
543600070109     ***********************************************************************************************
543700070109     **
543800070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
543900070109     **
544000070109     ***********************************************************************************************
544100070109     C     eventoAnnulla BEGSR
544200070109
544300070109     C                   IF        §iceEvAn <> *BLANK
544400070109     C                   FOR       j = 1 TO %ELEM(smCev)
544500070109     C                   IF        §iceEvAn = smCev(j) AND
544600070109     C                             evbDevHev = smDevHev(j)
544700070109     C                   CLEAR                   smCev(j)
544800130312     C                   CLEAR                   smspe(j)
544900070109     C                   CLEAR                   smDevHev(j)
545000070109     C                   CLEAR                   smi(j)
545100070109     C                   CLEAR                   smSeql(j)
545200070109     C                   CLEAR                   smSeqe(j)
545300070109     C                   CLEAR                   smDeve(j)
545400070109     C                   CLEAR                   smHeve(j)
545500070109     C                   CLEAR                   smFle(j)
545600070109     C                   CLEAR                   smDev(j)
545700070109     C                   LEAVE
545800070109     C                   ENDIF
545900070109     C                   ENDFOR
546000070109     C                   ENDIF
546100070109
546200070109     C                   ENDSR
546300080131
546400100119     P*--------------------------------------------------
546500100119     P* Procedure name: GetDescrizioneIncassoContrassegno
546600100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
546700100119     P*                          o del contrassegno.
546800100121     P* Returns:        Descrizione modalità incasso contrassegno
546900100119     P*--------------------------------------------------
547000100119     P GetDescrizioneIncassoContrassegno...
547100100119     P                 B
547200100119     D GetDescrizioneIncassoContrassegno...
547300100119     D                 PI            35A
547400100119
547500100121     C                   RESET                   tibs02ds
547600100121     C                   EVAL      t02Cod = 'I1A'
547700100121     C                   EVAL      t02Lin = langTntbe
547800100119     ** Il contrassegno è stato incassato
547900100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
548000100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
548100100121     C                   ELSE
548200100119     ** Il contrassegno non è stato incassato.
548300100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
548400100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
548500100119     C                             ar5Trd <> 'GEN'
548600100121     C                   EVAL      kr5Aas = csbAas
548700100121     C                   EVAL      kr5Lnp = csbLnp
548800100121     C                   EVAL      kr5Nrs = csbNrs
548900100121     C                   EVAL      kr5Nsp = csbNsp
549000100119     C                   EVAL      kr5Trd = 'GEN'
549100160314     c                   if        esito='P'
549200160314     C     keyAr5        CHAIN     fiar501l
549300160314     c                   else
549400160314     C     keyAr5        CHAIN     fiar531c
549500160314     c                   endif
549600160314     c
549700100119     C                   IF        %FOUND()
549800100119     C                   EVAL      dAr5Gen = ar5Uni
549900100119     C                   ELSE
550000100119     C                   RESET                   dAr5Gen
550100100119     C                   RETURN                  *BLANK
550200100119     C                   ENDIF
550300100119     C                   ENDIF
550400100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
550500100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
550600100119     C                   ELSE
550700100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
550800100119     C                   ENDIF
550900100121     C                   ENDIF
551000100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
551100100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
551200100119     C                   IF        t02Err = 'E'
551300100119     C                   RETURN                  *BLANK
551400100119     C                   ENDIF
551500100120     C                   EVAL      dI1a = t02Uni
551600140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
551700140417     C                   RETURN                  dI1a.descrizi2
551800140417     C                   ENDIF
551900100119     C                   RETURN                  dI1a.descrizion
552000100119
552100100119     P GetDescrizioneIncassoContrassegno...
552200100119     P                 E
552300100119
552400110304
552500110304     P*--------------------------------------------------
552600110304     P* Procedure name: IsContrassegnoCompensato
552700110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
552800110304     P*                          to con una fattura.
552900110304     P* Returns:        *ON = Compensato
553000110304     P*--------------------------------------------------
553100110304     P IsContrassegnoCompensato...
553200110304     P                 B
553300110304     D IsContrassegnoCompensato...
553400110304     D                 PI              N
553500110304
553600110304     D retField        S               N   INZ(*OFF)
553700110304
553800110307     ** Il contrassegno non è stato rimborsato.
553900110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
554000110307     C                   RETURN    retField
554100110307     C                   ENDIF
554200110307
554300110307     C/EXEC SQL
554400110307     C+ SELECT '1'
554500110307     C+ INTO :retField
554600110307     C+ FROM tncsv00f
554700110307     C+ WHERE csvAas = :csbAas
554800110307     C+   AND csvLnp = :csbLnp
554900110307     C+   AND csvNrs = :csbNrs
555000110307     C+   AND csvNsp = :csbNsp
555100110307     C+   AND csvCav = 'COMP'
555200110307     C+ FETCH FIRST ROW ONLY
555300110307     C/END-EXEC
555400110304
555500110307     C                   SELECT
555600110307     C                   WHEN      sqlCode < 0
555700110307     C                   DUMP(A)
555800110307     C                   ENDSL
555900110304
556000110307     C                   RETURN    retField
556100110304
556200110304     P IsContrassegnoCompensato...
556300110304     P                 E
556400110304
556500120321
556600120321     P*--------------------------------------------------
556700120321     P* Procedure name: SetIdAssegnoMittente
556800120321     P* Purpose:        Imposta l'ID assegno mittente.
556900120321     P* Returns:        Esito.
557000120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
557100120321     P*                          segni mittente.
557200120321     P* Parameter:      priAbi => ABI assegno.
557300120321     P* Parameter:      priCab => CAB assegno.
557400120321     P*--------------------------------------------------
557500120321     P SetIdAssegnoMittente...
557600120321     P                 B
557700120321     D SetIdAssegnoMittente...
557800120321     D                 PI            10I 0
557900120321     D  priNumeroAssegno...
558000120321     D                                     LIKE(caNumO74)
558100120321     D  priAbi                             LIKE(caAbiO74)
558200120321     D  priCab                             LIKE(caCabO74)
558300120321
558400120321     D retField        S             10I 0 STATIC
558500120321
558600120321      /FREE
558700120321
558800120321       CLEAR retField;
558900120321
559000120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
559100120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
559200120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
559300120321       // In questo caso restituisco l'ID del primo assegno.
559400120321
559500120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
559600120321         RETURN retField;
559700120321       ENDIF;
559800120321
559900120321       EXEC SQL
560000120321         SELECT csmNra, csmAbi, csmCab
560100120321           INTO :priNumeroAssegno, :priAbi, :priCab
560200120321           FROM tncsm00f
560300120321           WHERE csmKey = :priNumeroAssegno
560400120321           FETCH FIRST 1 ROW ONLY
560500120321       ;
560600120321
560700120321       SELECT;
560800120321         WHEN sqlCode = 100;
560900120321           retField = sqlCode;
561000120321         WHEN sqlCode < *ZERO;
561100120321           DUMP(A);
561200120321           retField = sqlCode;
561300120321       ENDSL;
561400120321
561500120321       RETURN retField;
561600120321
561700120321      /END-FREE
561800120321     P SetIdAssegnoMittente...
561900120321     P                 E
562000120321
562100130513
562200130513     P*--------------------------------------------------
562300130513     P* Procedure name: GetFilUrl
562400130513     P* Purpose:        Restituisce l'URL della filiale.
562500130513     P* Returns:        URL filiale.
562600130513     P* Parameter:      piFiliale => ID filiale
562700130513     P*--------------------------------------------------
562800130513     P GetFilUrl       B
562900130513     D GetFilUrl       PI           256A
563000130513     D  piFiliale                     3P 0 CONST
563100130513
563200130513     C                   IF        piFiliale = *ZERO
563300130513     C                   RETURN    *BLANK
563400130513     C                   ENDIF
563500130513     C                   RESET                   tibs02ds
563600130513     C                   EVAL      t02Cod = 'UFI'
563700130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
563800130513     C                   EVAL      t02Lin = langTntbe
563900130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
564000130513     C                   IF        t02Err = 'E'
564100130513     C                   RETURN    *BLANK
564200130513     C                   ENDIF
564300130513     C                   RETURN    t02Uni
564400130513
564500130513     P GetFilUrl       E
564600130513
