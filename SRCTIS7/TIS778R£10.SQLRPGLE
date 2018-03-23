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
007800051110
007900051110     ***********************************************************************************************
008000051110     **?
008100051110     **?Definizione procedure.
008200051110     **?
008300051110     ***********************************************************************************************
008400051110     D GetSpeChkCde    PR            10U 0
008500051110     D  Anno                          4
008600051110     D                                     VALUE
008700051110     D  IdSpedizione                 12
008800051110     D                                     VALUE
008900051110     D  ChkCode                      10U 0
009000051110     D  nEsito                        5I 0
009100051110     D tis7653r        PR
009200051110     D                                     EXTPGM('TIS7653R')
009300051110     D  ksu
009400051110     D                                     LIKE(kscI74)
009500051110     D  agc
009600051110     D                                     LIKE(gcpAgc)
009700051110     D  fgc
009800051110     D                                     LIKE(gcpFgc)
009900051110     D  ngc
010000051110     D                                     LIKE(gcpNgc)
010100051110     D  esito10                      10I 0
010200060612     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
010300060612     D  msgId                         7A   CONST
010400060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
010500060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
010600060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
010700060612     D                                     VARYING
010800060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
010900060612     D                                     VARYING
011000060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
011100060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
011200060621     D inzLingue       PR            10I 0
011300060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
011400060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
011500060621     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
011600060621     D cvtLinguaISO2ToTabel...
011700060621     D                 PR                  LIKE(linTabel)
011800060621     D  rqsISO2                            LIKE(linISO2)
011900060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
012000060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012100060621     D  rpyTabel                           LIKE(linTabel)
012200060621     D                                     OPTIONS(*NOPASS:*OMIT)
012300060621     D cvtLinguaISO2ToTntbe...
012400060621     D                 PR                  LIKE(linTntbe)
012500060621     D  rqsISO2                            LIKE(linISO2)
012600060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
012700060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012800060621     D  rpyTntbe                           LIKE(linTntbe)
012900060621     D                                     OPTIONS(*NOPASS:*OMIT)
013000060621     D openTabel00f    PR
013100060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
013200060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
013300060626     D chainTabel00f   PR           109A
013400060626     D  rqsOpCode                    10A   CONST
013500060626     D  rqsKut                        1P 0
013600060626     D  rqsCod                        2A   CONST
013700060626     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
013800060626     D  rqsLengthOfKey...
013900060626     D                               10I 0 CONST
014000060626     D  rqsFormat                    10A   CONST
014100060626     D  rpyOpCode                    10A
014200060626     D  rpyEsito                     10I 0
014300060626     D  rpyData                     109A   OPTIONS(*NOPASS:*OMIT)
014400060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
014500060622     D  kpjba                              LIKEDS(kpjba)
014600060622     D  tibs02ds                           LIKEDS(tibs02ds)
014700090828     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
014800090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7702R
014900090828
015000051222     ***********************************************************************************************
015100051222     **?
015200051222     **?Definizione costanti.
015300051222     **?
015400051222     ***********************************************************************************************
015500101104     D ITALIA          C                   ' '
015600101104     D ESTERO          C                   'E'
015700101104     D EVENTO_ARRIVATA_IN_FILIALE...
015800101104     D                 C                   '703'
015900101104     D EVENTO_MESSA_IN_CONSEGNA...
016000101104     D                 C                   'MIC'
016100150612     D EVENTO_ARRIVATA_HUB...
016200150612     D                 C                   '712'
016300000000     d*--------------------------------------------------------------------------------------------*
016400000000     d* Data structure
016500000000     d*--------------------------------------------------------------------------------------------*
016600150403     d dvpooraris    e ds                  inz
016700150403     d fipctcetds    e ds
016800140605     d fipctcords    e ds
016900090914     D cndizion      E DS                  QUALIFIED BASED(nullPtr)
017000090914     D azlin00f      E DS                  BASED(nullPtr)
017100060622     D tabel00f      E DS                  INZ
017200060622     D kpjba         E DS                  INZ
017300140605     D trulorsds     E DS                  INZ
017400140605     D trulor2ds     E DS                  INZ
017500140605     D trulor3ds     E DS                  INZ
017600150819     D Diore01       E DS                  INZ
017700140606     d dar5emd       e ds                  inz prefix(§)
017800140605     D tnsd99ds      E DS                  INZ
017900160323     D fidn40ds      E DS                  INZ
018000160323     D tibs02ds      E DS                  INZ
018100060622     D  t02Mod       E                     INZ('C')                             Controllo
018200090828     D tis7700i0     E DS                  QUALIFIED
018300090828     D                                     INZ(*EXTDFT)
018400090828     D tis7700o0     E DS                  QUALIFIED
018500090828     d*---
018600000000     d* Ridenominazione formato record -TITAS-
018700000000     d*---
018800000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
018900000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
019000000000     d                                     PREFIX(s_)
019100140715     d titasdssavF   E DS                  EXTNAME(titas00f)                    *depisito titas
019200140715     d                                     PREFIX(f_)
019300080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
019400080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
019500080131     D                                     BASED(titas000FigliaPtr)
019600080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
019700080131     D                                     BASED(titas000FigliaPtr)
019800080131     d*---
019900000000     d* Variabili riferite al data base
020000000000     d*---
020100141126     d kasaasm         S                   LIKE(tasaas)                         *lettura titas30c
020200141126     d kaslnpm         S                   LIKE(taslnp)
020300141126     d kasnrsm         S                   LIKE(tasnrs)
020400141126     d kasnspm         S                   LIKE(tasnsp)
020500140715     d kasaasf         S                   LIKE(tasaas)                         *lettura titas30c
020600140715     d kaslnpf         S                   LIKE(taslnp)
020700140715     d kasnrsf         S                   LIKE(tasnrs)
020800140715     d kasnspf         S                   LIKE(tasnsp)
020900000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
021000000000     d kaslnp          S                   LIKE(taslnp)
021100000000     d kasnrs          S                   LIKE(tasnrs)
021200000000     d kasnsp          S                   LIKE(tasnsp)
021300000000     d kastbl          S                   LIKE(tastbl)
021400000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
021500000000     d kaalnp          S                   LIKE(taalnp)
021600000000     d kaanrs          S                   LIKE(taanrs)
021700000000     d kaansp          S                   LIKE(taansp)
021800000000     d kaatrc          S                   LIKE(taatrc)
021900000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
022000000000     d ka4lnp          S                   LIKE(ta4lnp)
022100000000     d ka4nrs          S                   LIKE(ta4nrs)
022200000000     d ka4nsp          S                   LIKE(ta4nsp)
022300000000     d ka4trc          S                   LIKE(ta4trc)
022400000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
022500000000     d kvblnp          S                   LIKE(evblnp)
022600000000     d kvbnrs          S                   LIKE(evbnrs)
022700000000     d kvbnsp          S                   LIKE(evbnsp)
022800000000     d kvbdev          S                   LIKE(evbdev)
022900000000     d kvbhev          S                   LIKE(evbhev)
023000000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
023100000000     d kcplnp          S                   LIKE(gcplnp)
023200000000     d kcpnrs          S                   LIKE(gcpnrs)
023300000000     d kcpnsp          S                   LIKE(gcpnsp)
023400000000     d kcpfrg          S                   LIKE(gcpfrg)
023500000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
023600000000     d knpfgc          S                   LIKE(gnpfgc)
023700000000     d knpngc          S                   LIKE(gnpngc)
023800000000     d knpfrg          S                   LIKE(gnpfrg)
023900000000     d knpfas          S                   LIKE(gnpfas)
024000000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
024100000000     d ksblnp          S                   LIKE(csblnp)
024200000000     d ksbnrs          S                   LIKE(csbnrs)
024300000000     d ksbnsp          S                   LIKE(csbnsp)
024400000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
024500000000     d kbllpn          S                   LIKE(lbllpn)
024600000000     d kblnrn          S                   LIKE(lblnrn)
024700000000     d kblnsn          S                   LIKE(lblnsn)
024800000000     d kblaap          S                   LIKE(lblaap)
024900000000     d kbllpp          S                   LIKE(lbllpp)
025000000000     d kblnrp          S                   LIKE(lblnrp)
025100000000     d kblnsp          S                   LIKE(lblnsp)
025200000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
025300000000     d kctlnp          S                   LIKE(dctlnp)
025400000000     d kctnrs          S                   LIKE(dctnrs)
025500000000     d kctnsp          S                   LIKE(dctnsp)
025600040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
025700030206     d kr5lnp          S                   LIKE(ar5lnp)
025800030206     d kr5nrs          S                   LIKE(ar5nrs)
025900030206     d kr5nsp          S                   LIKE(ar5nsp)
026000030206     d kr5trd          S                   LIKE(ar5trd)
026100000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
026200000000     d kssisv          S                   LIKE(vssisv)
026300010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
026400000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
026500000000     d kbeke1          S                   LIKE(tbeke1)
026600000000     d kbeke2          S                   LIKE(tbeke2)
026700000000     d kbelin          S                   LIKE(tbelin)
026800000000     d kbesif          S                   LIKE(tbesif)
026900000000     d waas            S                   LIKE(tasaas)                         *di lavoro
027000000000     d wlnp            S                   LIKE(taslnp)
027100000000     d wnrs            S                   LIKE(tasnrs)
027200000000     d wnsp            S                   LIKE(tasnsp)
027300000000     d wfca            S                   LIKE(tasfca)
027400000000     d wfgc            S                   LIKE(tasfgc)
027500000000     d wfpr            S                   LIKE(dctfpr)
027600000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
027700000000     d deplpn          S                   LIKE(lbllpn)
027800000000     d depnrn          S                   LIKE(lblnrn)
027900000000     d depnsn          S                   LIKE(lblnsn)
028000010716     d depute          S                   LIKE(vtdute)
028100010716     d deppwd          S                   LIKE(vtdpwd)
028200000000     d lcca            S                   LIKE(tascca)                         *di lavoro
028300000000     d lduc            S                   LIKE(tasduc)
028400000000     d llnp            S                   LIKE(taslnp)
028500000000     d ldti            S                   LIKE(tasdti)
028600000000     d lhti            S                   LIKE(tashti)
028700000000     d ldrt            S                   LIKE(tasdrt)
028800000000     d llna            S                   LIKE(taslna)
028900150617     d ltfa            S                   LIKE(tastfa)
029000000000     d ldcm            S                   LIKE(tasdcm)
029100000000     d lhmc            S                   LIKE(tashmc)
029200060621     d langTabel       S                   LIKE(tblkut)
029300060621     d langTntbe       S                   LIKE(tbelin)
029400140108     d wDev            S                   LIKE(evbdev)
029500140108     d wHev            S                   LIKE(evbhev)
029600150611     d wDEE            S                   LIKE(d98dee)
029700140616     d wParm           s            512a   inz
029800150403     D w_dallemsg      S              5a
029900150403     D w_allemsg       S              5a
030000150403     D w_dalle         S               t
030100150403     D w_alle          S               t
030200150403     D w_dalleoser     S               t
030300150403     D w_alleoser      S               t
030400000000     d*---
030500000000     d* Ds
030600000000     d*---
030700000000      * controllo data
030800000000     d wlbda8          DS                  INZ                                  *controlla data
030900000000     d  g08dat                        8  0                                       -data dritta
031000000000     d  g08inv                        8  0                                       -data invertita
031100000000     d  g08err                        1                                          -errore
031200000000     d  g08tgi                        5  0                                       -giorni fra date
031300000000      * scomposizione data generica
031400000000     d dsdat           DS                  INZ                                  *data
031500000000     d  dsann                         4  0                                       -anno
031600000000     d  dsmes                         2  0                                       -mese
031700000000     d  dsgio                         2  0                                       -giorno
031800000000      * scomposizione data generica (anno + mese/giorno)
031900000000     d                 DS                  INZ                                  *data aa + mm/gg
032000000000     d  dsannB                 1      4  0                                       -anno
032100000000     d  dsmgsB                 5      8  0                                       -mese/giorno
032200000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
032300000000      * composizione data generica editata
032400000000     d dsdate          DS                  INZ                                  *data
032500000000     d  dsgioe                        2  0                                       -giorno
032600000000     d  dsvd1                         1                                          -'/'
032700000000     d  dsmese                        2  0                                       -mese
032800000000     d  dsvd2                         1                                          -'/'
032900000000     d  dsanne                        4  0                                       -anno
033000000000      * scomposizione ora generica
033100000000     d dsora           DS                  INZ                                  *ora
033200000000     d  dshh                          2  0                                       -ore
033300000000     d  dsmm                          2  0                                       -minuti
033400000000      * composizione ora generica editata
033500000000     d dsorae          DS                  INZ                                  *ora
033600000000     d  dshhe                         2  0                                       -ore
033700000000     d  dsvo1                         1                                          -':'
033800000000     d  dsmme                         2  0                                       -minuti
033900030206      * lettura FIAR5 - record bancali
034000030206     d dar5ban       E DS
034100030206      * lettura FIAR5 - record supermercati
034200030206     d dar5scr       E DS
034300040330     d DAR5GEN       E DS
034400040330     D                                     INZ
034500040330     D                                     PREFIX(GEN)
034600000000      * tipi servizio
034700000000     d ds5e          E DS
034800000000      * disposizioni giacenza
034900000000     d ds2d          E DS
035000000000      * causale eventi
035100000000     d dice          E DS
035200000000      * causali chiusura CA
035300000000     d dcch          E DS
035400100119     d dI1a          E DS                  QUALIFIED
035500000000      * consegne anomale
035600000000     d ds7o          E DS
035700130312      * eventi
035800130312     d ds2a          E DS
035900160314     d
036000160314     d* cod bolla
036100160314     d ds3a          E DS
036200000000      * riferimenti
036300000000     d dta4a         E DS
036400070927     d dsbl4e        E DS
036500060627      * Estensione blp/arb tipo record "I"
036600060627     d dsbl4i        E DS
036700040913      * Addebito immagine POD
036800040913     d DLVA1         E DS
036900040913     D                                     INZ
037000000000      * lettura campo orgde3 di AZORG00F
037100000000     d og143         E DS
037200000000      * stati contrassegno
037300000000     d ds4s          E DS
037400141014      * particolarità consegna
037500141014     d ds7r          E DS
037600031217      * DS lettura campo TASFLO di TITAS
037700031217     d dtasflo       E DS
037800000000      * Ds ricerca cliente P.d.C.
037900000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
038000000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
038100000000     d indds         E DS                  EXTNAME(cnind00f) INZ
038200000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
038300000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
038400150304      * stato data prev cons
038500150304     d dspc          E DS
038600150312     d
038700150312     d xgiolavds     e ds
038800000000      * reperimento clienti da cliente unificante
038900090903     d*tibs10ds      E DS
039000090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
039100000000      * scomposizione codice spedizione -DS Input-
039200000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
039300000000     d  dslnpi74                      3                                          -linea partenza
039400000000     d  dsnrsi74                      2                                          -serie
039500000000     d  dsnspi74                      7                                          -spedizione
039600000000      * composizione codice spedizione -bolle-
039700000000     d                 DS
039800000000     d  dstaslnp               1      3  0                                       -linea partenza
039900000000     d  dstasnrs               4      5  0                                       -serie
040000000000     d  dstasnsp               6     12  0                                       -spedizione
040100000000     d dstasspe                1     12                                         *ds cod spedizione
040200000000      * composizione data + ora evento
040300080131     d                 DS                  INZ
040400080131     d  evbdev                 1      8  0                                       -data evento
040500080131     d  evbhev                 9     12  0                                       -ora  evento
040600080131     d evbDevHev               1     12  0                                      *ds data+ora evento
040700130312      * composizione spedizione evento
040800130312     d                 DS                  INZ
040900130312     d  evbaas                 1      4  0                                       -data evento
041000130312     d  evblnp                 5      7  0                                       -ora  evento
041100130312     d  evbnrs                 8      9  0                                       -ora  evento
041200130312     d  evbnsp                10     16  0                                       -ora  evento
041300130312     d dsevbspe                1     16  0                                      *ds data+ora evento
041400060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
041500000000      * ordinamento schiere eventi
041600000000     d                 DS
041700000000     d  dsseql                 1      3  0                                       -sequenza legame
041800000000     d  dsseqe                 4      6  0                                       -sequenza evento
041900000000     d  dsdevhev               7     18  0                                       -data + ora
042000000000     d  dsi                   19     21  0                                       -indice
042100000000     d dssort                  1     21  0                                      *ds cod spedizione
042200000000     d* Input routine XALLINEA
042300000000     d walfa7          S              7                                         *alfanumerico 7
042400000000     d walfa7bis       S              7                                         *alfanumerico 7
042500000000     d wzeri           S              7                                         *alfa con zeri
042600000000     d*---
042700000000     d* Variabili
042800000000     d*---
042900101104     d i               S             10I 0                                      *indici
043000101104     d ii              S             10I 0
043100101104     d j               S             10I 0
043200101104     d jj              S             10I 0
043300101104     d z               S             10I 0
043400101104     d s               S             10I 0
043500101104     d k               S             10I 0
043600101104     d o               S             10I 0
043700101104     d tote            S             10I 0                                      *totale eventi memor
043800000000     d seql            S              5  0                                      *sequenza legami
043900000000     d n14             S             14  0                                      *Numerico 14
044000000000     d n8              S              8  0                                      *Numerico 8
044100000000     d n7              S              7  0                                      *Numerico 7
044200000000     d n4              S              4  0                                      *Numerico 4
044300010927     d n3              S              3  0                                      *Numerico 3
044400000000     d a1              S              1                                         *Alfanumerico 1
044500000000     d a3              S              3                                         *Alfanumerico 3
044600000000     d a5              S              5                                         *Alfanumerico 5
044700010717     d a8              S              8                                         *Alfanumerico 8
044800000000     d a15             S             15                                         *Alfanumerico 15
044900000000     d a200            S            200                                         *Alfanumerico 200
045000000000     d a200bis         S            200                                         *Alfanumerico 200
045100010927     d a230            S            230                                         *Alfanumerico 230
045200000000     d a400            S            400                                         *Alfanumerico 400
045300000000     d datcor          S              8  0                                      *data corrente
045400000000     d datpre          S              8  0                                      *data precedente
045500000000     d oracor          S              6  0                                      *ora corrente
045600000000     d anncor          S              4  0                                      *anno corrente
045700000000     d annpre          S              4  0                                      *anno precedente
045800140617     d wIdMsg          s              7a
045900140617     d wIdCampo        s             10a
046000000000     d werr            S              1                                         *errore generico
046100000000     d wdat            S              8  0                                      *data (aaaammgg)
046200000000     d wdate           S             10                                         *data (gg/mm/aaaa)
046300000000     d wora            S              4  0                                      *ora  (hhmm)
046400000000     d worae           S              5                                         *ora  (hh:mm)
046500000000     d worg            S              3  0                                      *filiale
046600000000     d wodes           S             30                                         *descrizione filiale
046700000000     d wofl1           S              1                                         *flag italia/estero
046800150617     d wtasfl1         S              1                                         *flag italia/estero
046900020702     d wodpd           S              3                                         *flag filiale DPD
047000000000     d wcev            S              3                                         *causale evento
047100030130     d wcdex           S             30                                         *descrizione bar/pt
047200000000     d wpgm            S             20                                         *programma
047300000000     d wftc            S              1                                         *cons.partic.
047400140714     d wricons         S              1                                         *wrk per riconsegna
047500140715     d keyforzata      S              1                                         *wrk per riconsegna
047600000000     d wdftc           S             50                                         *descriz.cons.partic
047700000000     d d1wdftc         S             50                                         *descriz.cons.part.1
047800000000     d d2wdftc         S             50                                         *descriz.cons.part.2
047900000000     d wgcx            S              2                                         *turno chiusura
048000000000     d wdgcx           S             20                                         *descriz.turno chius
048100000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
048200000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
048300000000     d we              S              3                                         *descrizione 'e'
048400000000     d bollaorig       S              1                                         *bolla originaria
048500150310     d PDAavv          S              1                                         *evento AVV a PDA
048600150421     d PDAgia          S              1                                         *evento GIA a PDA
048700000000     d sostrit         S              1                                         sostituisce Ritiro
048800000000     d sostcon         S              1                                         sostituisce Consegna
048900000000     d newevb          S              1                                         *nuovo evento
049000000000     d lavdevhev       S             12  0                                      *data + ora
049100040913     D WrkCurDate      S               D                                        Data corrente
049200060620     D wrkDtInvDisp    S              8
049300060620     D wrkLnaItaEst    S              1
049400060620     D rpyOpCode       S             10A
049500060620     D rpyEsito        S             10I 0
049600060621     D nullPtr         S               *
049700080131     D titas000FigliaPtr...
049800080131     D                 S               *   INZ(%ADDR(titas000Figlia))
049900010716     d*---
050000010716     d* Variabili di controlli
050100010716     d*---
050200010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
050300010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
050400000000     d*---
050500000000     d* Variabili di memorizzazione
050600000000     d*---
050700000000     d mhcre           S              5                                         *consegna richiesta
050800000000     d mdcre           S             10
050900000000     d mtcre           S             50
051000000000     d mftce           S            100                                         *consegna partic.
051100000000     d mgcxe           S            100                                         *turno chiusura
051200000000     d mffd            S            100                                         *fermo deposito
051300010927     d mmncB           S             35                                         *NON consegna LAV
051400010927     d mmncC           S             35
051500010927     d mmncG           S             35                                         *NON consegna GIAC
051600010927     d mmncH           S             35
051700010927     d mmncN           S             35                                         *NON consegna NOFATT
051800010927     d mmncO           S             35
051900050701     d mmnc8           S             35
052000050701     d mmnc9           S             35
052100030206     d mtel            S             20                                         *N° tel destinatario
052200030206     d mban            S             35                                         *bancali
052300030207     d mscr            S            500                                         *supermercati
052400030210     d msdace          S             10
052500030210     d msorce          S              5
052600030210     d msdcre          S             10
052700030210     d mshcre          S              5
052800030210     d mstcre          S             30
052900030210     d msnom           S             50
053000030210     d mscrT           S            500    DIM(5)
053100030211     d wmscrT          S           2504
053200000000     d*---
053300000000     d* Schiere
053400000000     d*---
053500000000      * taabella clienti del cliente unificante
053600000000      * tabella tipi servizio
053700000000     d stsp            S              1    DIM(20)                              *tipi servizio
053800000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
053900000000      * tabella consegne anomale
054000030130     d scca            S              1    DIM(30)                              *consegna anomala
054100000000      * tabella disposizioni giacenza
054200000000     d sdis            S              1    DIM(10)                              *codice
054300000000     d sddis           S             20    DIM(10)                              *descrizione italian
054400000000      * tabella filiali
054500000000     d sorg            S              3  0 DIM(300)                             *filiale
054600000000     d sodes           S             30    DIM(300)                             *descrizione filiale
054700000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
054800160406     d sodpd           S              3    DIM(300)                             ntw
054900000000      * eventi memorizzati
055000000000     d smi             S              3  0 DIM(200)                             *indice
055100000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
055200000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
055300000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
055400000000     d smdeve          S             10    DIM(200)                             *data evento editata
055500000000     d smheve          S              5    DIM(200)                             *ora  evento editata
055600000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
055700000000     d smdev           S             30    DIM(200)                             *descrizione evento
055800000000     d smcev           S              3    DIM(200)                             *causale evento
055900130313     d smspe           S             16  0 DIM(200)                             *causale evento
056000000000      * eventi memorizzati x ordinamento
056100000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
056200000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
056300000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
056400000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
056500000000     d spdeve          S             10    DIM(200)                             *data evento editata
056600000000     d spheve          S              5    DIM(200)                             *ora  evento editata
056700000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
056800000000     d spdev           S             30    DIM(200)                             *descrizione evento
056900000000     d spcev           S              3    DIM(200)                             *causale evento
057000130313     d spspe           S             16  0 DIM(200)                             *causale evento
057100000000      * note giacenze memorizzate
057200051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
057300051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
057400000000      * note memorizzate
057500000000     d smnot           S            100    DIM(10)
057600030206     d*---
057700030206     d* definizioni per passaggio parametri
057800030206     d*---
057900000000     d esito           s              1
058000000000      * DS contenente i dati di INPUT
058100140603     d tis778dsi     e DS                  INZ(*EXTDFT)
058200050104     D  cAASI74                       4
058300050104     D                                     OVERLAY(AASI74)
058400000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
058500140530     d tis778dso     e DS                  INZ(*EXTDFT)
058600000000     d dev                           10    DIM(50) OVERLAY(dato74)
058700000000     d hev                            5    DIM(50) OVERLAY(orao74)
058800000000     d fle                           30    DIM(50) OVERLAY(opeo74)
058900041007     d eve                           30    DIM(50) OVERLAY(eveo74)
059000041007     d cev                            3    DIM(50) OVERLAY(cevo74)
059100000000     d nota                         100    DIM(10) OVERLAY(notao74)
059200130313     d spe             S             16  0 DIM(50)
059300040906
059400140616     d tis778dsM     e DS                  INZ(*EXTDFT)
059500140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
059600140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
059700140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
059800140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
059900140530      *
060000040906     D FIDN12DS      E DS
060100040906     D                                     INZ
060200040906     D  I12FEL       E
060300040906     D                                     INZ('E')
060400040906     D  I12FAN       E
060500040906     D                                     INZ('E')
060600040906     D  O12KeyAry                    14
060700040906     D                                     DIM(20)
060800040906     D                                     OVERLAY(O12KEY)
060900040906
061000040906     D O12KeyDS        DS
061100040906     D                                     INZ
061200040906     D  O12KeyAAC                     4S 0
061300040906     D  O12KeyFil                     3S 0
061400040906     D  O12KeyNCA                     7S 0
061500040906
061600020702      * DS lettura tabella "NTW"
061700020702     d DNTW          E DS
061800130326     d Ddstflr       E DS
061900041223
062000041223     D ChkCode         S             10U 0
062100041223     D nEsito          S              5I 0
062200051110     D esito10         S             10I 0
062300080131
062400100119     D*--------------------------------------------------
062500100119     D* Procedure name: GetDescrizioneIncassoContrassegno
062600100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
062700100119     D*                          o del contrassegno.
062800100121     D* Returns:        Descrizione modalità incasso contrassegno.
062900100119     D*--------------------------------------------------
063000100119     D GetDescrizioneIncassoContrassegno...
063100100119     D                 PR            35A
063200110304
063300110304     D*--------------------------------------------------
063400110304     D* Procedure name: IsContrassegnoCompensato
063500110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
063600110304     D*                          to con una fattura.
063700110304     D* Returns:        *ON = Compensato
063800110304     D*--------------------------------------------------
063900110304     D IsContrassegnoCompensato...
064000110304     D                 PR              N
064100120321
064200120321     D*--------------------------------------------------
064300120321     D* Procedure name: SetIdAssegnoMittente
064400120321     D* Purpose:        Imposta l'ID assegno mittente.
064500120321     D* Returns:        Esito.
064600120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
064700120321     D*                          segni mittente.
064800120321     D* Parameter:      priAbi => ABI assegno.
064900120321     D* Parameter:      priCab => CAB assegno.
065000120321     D*--------------------------------------------------
065100120321     D SetIdAssegnoMittente...
065200120321     D                 PR            10I 0
065300120321     D  priNumeroAssegno...
065400120321     D                                     LIKE(caNumO74)
065500120321     D  priAbi                             LIKE(caAbiO74)
065600120321     D  priCab                             LIKE(caCabO74)
065700130513
065800130513     D*--------------------------------------------------
065900130513     D* Procedure name: GetFilUrl
066000130513     D* Purpose:        Restituisce l'URL della filiale.
066100130513     D* Returns:        URL filiale.
066200130513     D* Parameter:      piFiliale => ID filiale
066300130513     D*--------------------------------------------------
066400130513     D GetFilUrl       PR           256A
066500130513     D  piFiliale                     3P 0 CONST
066600130513
066700130513
066800140527       //--------------------------------------------------------------
066900140527       //?Definizione prototipi.
067000140527       //--------------------------------------------------------------
067100140527      /copy gaitrasrc/srcprotopr,tis778R
067200140529      /copy gaitrasrc/srcconst,tis778R
067300140527       //--------------------------------------------------------------
067400140527       //?Definizione parametri programma.
067500140527       //--------------------------------------------------------------
067600140529     d tis778_GetBolla...
067700140527     d                 PI
067800140527     d prmRqsOpCode...
067900140527     d                               10I 0 CONST
068000140527     d prmRpyOpCode...
068100140527     d                               10I 0
068200140527     d prmRpyIdMsg...
068300140527     d                               10I 0
068400140527     d prmRqsFormato...
068500140527     d                               10A   CONST
068600140527     d prmRqsData...
068700140527     d                            32767A   OPTIONS(*VARSIZE)
068800140527     d prmRqsDataSize...
068900140527     d                               10I 0 CONST
069000140527     d prmRpyFormato...
069100140527     d                               10A   CONST
069200140527     d prmRpyData...
069300140527     d                            32767A   OPTIONS(*VARSIZE)
069400140527     d prmRpyDataSize...
069500140527     d                               10I 0 CONST
069600140527     d prmRpyFormMsg...
069700140527     d                               10A   CONST OPTIONS(*NOPASS)
069800140527     d prmRpyMessage...
069900140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
070000140527     d prmRpyMsgSize...
070100140527     d                               10I 0 CONST OPTIONS(*NOPASS)
070200080131
070300000000     i*--------------------------------------------------------------------------------------------*
070400000000     i* Input
070500000000     i*--------------------------------------------------------------------------------------------*
070600000000     i*---
070700000000     i* Indicatori lettura formati record -TITAS-
070800000000     i*---
070900000000     ititas000      01
071000000000     ititas010      02
071100000000     ititasp00      03
071200000000     c*--------------------------------------------------------------------------------------------*
071300000000     c* Main lines
071400000000     c*--------------------------------------------------------------------------------------------*
071500000000     c*
071600000000     c* operazioni iniziali -da fare sempre-
071700000000     c                   EXSR      rutinz
071800000000     c*
071900000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
072000000000if  1c                   IF        esito <> '1'                                 *no errori
072100100219     c*
072200100219     c                   MOVEL     dslnpi74      kaslnp
072300100219     c                   MOVEL     dsnrsi74      kasnrs
072400100219     c                   MOVEL     dsnspi74      kasnsp
072500100219     c*
072600111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
072700111103     c* restituisco errore.
072800111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
072900111103     c* e se non la trovo provo con l'anno precedente.
073000160318    1c                   IF        aasI74 <> *ZERO
073100100219     c                   Z-ADD     aasI74        kasaas
073200100219     c     keytas        SETLL     titas30c                               99
073300160318    2c                   IF        NOT *IN99
073400160318
073500160318     c     keytas        chain     fnblp01l
073600160318    3c                   if        %found(fnblp01l) and arbft1=' '
073700160318     c                   MOVEL     'P'           esito                          *bolla in part
073800160318   x3c                   else
073900111103     c                   EVAL      esito = '1'
074000160318    3c                   ENDIF
074100160318    2c                   ENDIF
074200160318
074300160318   x1c                   ELSE
074400000000     c                   Z-ADD     anncor        kasaas
074500000000     c     keytas        SETLL     titas30c                               99
074600000000if  2c                   IF        NOT *in99                                    *non trovato
074700160318
074800160318     c     keytas        chain     fnblp01l
074900160318    3c                   if        %found(fnblp01l) and arbft1=' '
075000160318     c                   MOVEL     'P'           esito                          *bolla in part
075100160318   x3c                   else
075200160318
075300160318     C* verifico se in partenza: se non è in partenza allora verifico anno precedente
075400000000     c                   Z-ADD     annpre        kasaas
075500000000     c     keytas        SETLL     titas30c                               99
075600160318if  4c                   IF        NOT *in99                                    *non trovato
075700000000     c                   MOVEL     '1'           esito                          *errore
075800160318e   4c                   ENDIF
075900160318e   3c                   ENDIF
076000000000e   2c                   ENDIF
076100160318    1c                   ENDIF
076200160127     c
076300160314    2c                   if        esito = '1'
076400140617     c                   eval      widmsg = 'TIS0564'
076500140616     c                   eval      widcampo = 'NSPEDIZI74'
076600140616     c                   clear                   wparm
076700140616     c                   exsr      messaggi
076800160314    2c                   endif
076900160318     c
077000141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
077100141126     c                   eval      kasaasm = kasaas
077200141126     c                   eval      kaslnpm = kaslnp
077300141126     c                   eval      kasnrsm = kasnrs
077400141126     c                   eval      kasnspm = kasnsp
077500141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
077600140715      * ultima figlia per forzare i dati in output
077700160318     c                   if        esito<>'P'
077800140714     c                   exsr      cercafiglia
077900160318     c                   else
078000160318     c                   exsr      cercamamma
078100160318     c                   endif
078200000000     c*
078300160127     c* Se bolla in FNBLP in partenza eseguo routine a parte
078400160314    2c                   if        esito='P'
078500160127     c
078600160127     c* imposta i campi della DS di Output dagli eventi
078700160127     c                   EXSR      impdsoevb
078800160127     c
078900160127     c* imposta i campi del C%Ass dal fiar9
079000160127     c                   EXSR      impdsoar9
079100160127     c
079200160127     c* impostazione campi in DS di output
079300160309     c                   exsr      impds_soloblp
079400160309     c                   exsr      impdsoarb
079500160309     c
079600160309     c* imposta i campi della DS di Output dalle note
079700160309     c                   EXSR      impdsonot
079800160314    2c                   endif
079900160127     c
080000000000     c* se trovata, legge la bolla
080100160127if  2c                   IF        esito <> '1'  and esito<>'P'
080200000000     C                   SETOFF                                       010203
080300140715     c     keytas        SETLL     titas30c
080400000000     c     keytas        READE     titas30c                               99    *no errore
080500000000if  3c                   IF        *in99                                        *non trovato
080600000000     c                   MOVEL     '1'           esito                          *errore
080700140617     c                   eval      widmsg = 'TIS0734'
080800140616     c                   eval      widcampo = 'NSPEDIZI74'
080900140616     c                   clear                   wparm
081000140616     c                   exsr      messaggi
081100000000x   3c                   ELSE                                                   *trovata
081200000000if  4c                   IF        *in01                                        *lettura titas000
081300000000     c                   EVAL      titasdssav = titasds
081400000000e   4c                   ENDIF
081500000000if  4c                   IF        *in02                                        *lettura titas010
081600000000     c                   EVAL      titasdssav = titasds
081700000000e   4c                   ENDIF
081800000000if  4c                   IF        *in03                                        *lettura titasp00
081900000000     c                   EVAL      titasdssav = titasds
082000000000e   4c                   ENDIF
082100141117      * se trovo la figlia ma ancora non è su titas
082200150617      * Se la bolla non è legata, nei campi della "figlia" ovvero F_  ci sono
082300150617      *  i campi della bolla originale passata
082400141117     c                   if        keyforzata <> *blank and
082500141117     c                             f_tasnsp = 0
082600141117     c                   eval      titasdssavf = titasdssav
082700141120     c                   else
082800141120     c                   clear                   keyforzata
082900141117     c                   endif
083000000000e   3c                   ENDIF
083100000000     c*
083200000000     c* controlla validità bolla
083300000000     c                   EXSR      chktas
083400000000     c*
083500030130     c* imposta i campi della DS di Output
083600000000if  3c                   IF        esito <> '1'                                 *no errore
083700140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
083800140714      * spedizione richiesta a video sempre
083900100225     c*
084000140716     c                   EVAL      dstaslnp = s_taslnp
084100140716     c                   EVAL      dstasnrs = s_tasnrs
084200140716     c                   EVAL      dstasnsp = s_tasnsp
084300140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
084400140723     c* chain ar5 con bolla richiesta a video
084500160127     c                   Z-ADD     s_tasaas      kr5aas
084600140723     c                   Z-ADD     s_taslnp      kr5lnp
084700140723     c                   Z-ADD     s_tasnrs      kr5nrs
084800140723     c                   Z-ADD     s_tasnsp      kr5nsp
084900100225     c                   MOVEL     'GEN'         kr5trd
085000100225     c     keyar5        CHAIN     fiar531c
085100100225     C                   IF        %FOUND
085200100225     C                   EVAL      DAR5GEN = AR5Uni
085300100225     C                   ELSE
085400100225     C                   RESET                   DAR5GEN
085500100225     C                   ENDIF
085600000000     c*
085700000000     c* imposta i campi della DS di Output dalle bolle
085800000000     c                   EXSR      impdsotas
085900000000     c*
086000000000     c* imposta i campi della DS di Output dagli eventi
086100000000     c                   EXSR      impdsoevb
086200000000     c*
086300000000     c* imposta i campi della DS di Output dal contrassegno
086400000000     c                   EXSR      impdsocsb
086500000000     c*
086600000000     c* imposta i campi della DS di Output dalla giacenza
086700000000     c                   EXSR      impdsogcp
086800000000     c*
086900000000     c* imposta i campi della DS di Output dalle note
087000000000     c                   EXSR      impdsonot
087100040330     c*
087200040330     c* imposta i campi della DS di Output del destinatario.
087300040330     c                   EXSR      ImpDSDest
087400010716     C*
087500160323     c* imposta i campi della DS di Output dalla firma DPD/EEX
087600010716     c                   EXSR      ImpdsoDPD
087700040913     C*
087800040913     c* imposta i campi della DS di Output di addebito immagine LDV.
087900040913     c                   EXSR      AddebitoLDV
088000000000e   3c                   ENDIF
088100000000e   2c                   ENDIF
088200140613e   1c                   ENDIF
088300160314     c
088400140919      *riempie campi con riferimenti alla consegna
088500160314      *  non per bolla ancora in partenza
088600160314     c                   if        esito <> '1'  and esito <>'P'
088700160314     c
088800140919     c                   if        f_tasdcm > 0
088900140919     c                   movel     f_tasdcm      wdat
089000140919     c                   exsr      edtdat
089100140919     c                   eval      dtconmeo74  = wdate
089200140919     c                   MOVEL     f_tashmc      wora
089300140919     c                   EXSR      edtora
089400140919     c                   eval      orconmeO74  =  worae
089500140919     c                   eval      flgbolcO74  =  'S'
089600140919     c                   eval      flgboldO74  =  'N'
089700141014      *firmatario distinta PDA
089800141014     c                   clear                   firmato74
089900141015     c                   if        gen§ar5pdaco <> 'NO'
090000141014     c                   exsr      repfirmat
090100141014     c                   else
090200141014     c                   if        f_tasgma <> *blank
090300141015     C                   EVAL      tblKey = f_tasgma
090400141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
090500141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
090600141014     C                             :rpyOpCode:rpyEsito)
090700141014     C                   IF        rpyOpCode = 'FOUND'
090800141014     c                   if        §7r2fi = 'S'
090900141014     c                   exsr      repfirmat
091000141014     c                   endif
091100141014     c                   endif
091200141014     c                   endif
091300141014     c                   endif
091400141014      *else consegna
091500140919     c                   else
091600140919     c                   eval      flgbolcO74  =  'N'
091700140919     c                   if        F_tasndc > 0 and f_tasddc > 0
091800141119     c                             and f_tasddc <= datcor
091900140919     c                   eval      flgboldo74 = 'S'
092000140919     c                   else
092100140919     c                   eval      flgboldO74  =  'N'
092200140919     c                   endif
092300140919     c                   endif
092400150617     c
092500140613if  3c                   IF        esito <> '1'                                 *no errore
092600150615     c***********        IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
092700150615     c************                 and cev(49) = ' ' and cev(50) = ' ' or
092800150615     c
092900150615     c                   if        f_tasdcm = 0 and keyforzata = *blank
093000130312     c                   exsr      srarb
093100140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
093200140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
093300140605     c                   exsr      impdsoarb
093400140604     c                   endif
093500140613     c                   endif
093600140613     c                   endif
093700141016     c                   endif
093800000000     c*
093900140530     C                   exsr      routend
094000141014      *__________________________________________________________________
094100141014     c     repfirmat     begsr
094200141014      *__________________________________________________________________
094300141014     c                   Z-ADD     f_tasaas      ka4aas
094400141014     c                   Z-ADD     f_taslnp      ka4lnp
094500141014     c                   Z-ADD     f_tasnrs      ka4nrs
094600141014     c                   Z-ADD     f_tasnsp      ka4nsp
094700141014     c                   MOVEL     '1'           ka4trc
094800141014     c     keyta4        CHAIN     tita430c                           99
094900141014if  1c                   IF        NOT *in99
095000141014     c                   MOVEL     ta4not        firmato74
095100141014e   1c                   ENDIF
095200141014     c                   endsr
095300141015      *__________________________________________________________________
095400141015     c     repfirmatARB  begsr
095500141015      *__________________________________________________________________
095600141015      *firmatario da arb
095700141015     c                   Z-ADD     arbaas        ka4aas
095800141015     c                   Z-ADD     arblnp        ka4lnp
095900141015     c                   Z-ADD     arbnrs        ka4nrs
096000141015     c                   Z-ADD     arbnsp        ka4nsp
096100141015     c                   MOVEL     '1'           ka4trc
096200141015     c     keyta4        CHAIN     fiar401l                           99
096300141015if  1c                   IF        NOT *in99
096400141015     c                   MOVEL     ar4not        firmato74
096500141015e   1c                   ENDIF
096600141015     c                   endsr
096700000000     c*--------------------------------------------------------------------------------------------*
096800000000     c* controlla validità bolla
096900000000     c*--------------------------------------------------------------------------------------------*
097000000000     c     chktas        BEGSR
097100000000     c*
097200000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
097300000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
097400000000     c                   Z-ADD     s_tasmgs      dsmgsB
097500000000if  1c                   IF        dsdatB < datpre
097600000000     c                   MOVEL     '1'           esito                          *errore
097700000000e   1c                   ENDIF
097800030130     c* se l'esito è positivo, salva alcuni dati della bolla
097900150617     c*********          EVAL      savtsp = f_tastsp
098000000000     c*
098100140718     C                   EVAL      wOrg = f_tasLna
098200051222     C                   EXSR      setLnaItaEst
098300051222     C
098400000000     c                   ENDSR
098500000000     c*--------------------------------------------------------------------------------------------*
098600000000     c* imposta i campi della DS di Output dalle bolle e files correlati
098700000000     c*--------------------------------------------------------------------------------------------*
098800140604     c     impdsotas     BEGSR
098900140604     c*
099000140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
099100140604     c                   EVAL      lnaDescO74 = sodes(i)
099200140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
099300150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
099400150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
099500150116     c                   MOVE      s_tasmgs      aasmgsO74
099600150116      *
099700140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
099800140718     c                   MOVEL     f_taslna      lineaaro74
099900140604     c*
100000140718     c                   MOVEL     f_tastbl      a1
100100140718     c                   MOVEL     f_tastbl      tpporto74
100200140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
100300140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
100400140604x   1c                   ELSE                                                   *Pagamento del
100500140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
100600140604e   1c                   ENDIF
100700140716     c                   MOVEL     f_tastsp      tpservio74
100800140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
100900140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
101000140604     C                             rpyOpCode:rpyEsito)
101100140604     C                   EVAL      corro74 = §5EDes
101200140604     c*
101300140716     c                   EVAL      destino74 = f_tasrsd
101400140716     c                   EVAL      desindo74 = f_tasind
101500140716     c                   EVAL      descapo74 = f_tascad
101600140716     c                   EVAL      desloco74 = f_taslod
101700140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
101800150326if  1c                   IF        f_tasnzd  = *blanks
101900150326     c                   EVAL      desnazo74 = f_tasprd
102000150326x   1c                   ELSE
102100150326     c***                EVAL      desnazo74 = f_tasnzd
102200150326     c***                EVAL      desnzdo74 = f_tasnzd
102300150326     c* 26/03/15: Attiviamo le specifiche per provincia export
102400150326     c                   EVAL      desnazo74 = f_tasprd
102500150326     c                   EVAL      desnzdo74 = f_tasnzd
102600150326e   1c                   ENDIF
102700140604     c*
102800140716     c                   EVAL      collio74  = f_tasncl
102900140716     c                   EVAL      pesoo74   = f_taspkf
103000140716     c                   EVAL      volumeo74 = f_tasvlf
103100160122     c***                EVAL      naturao74 = f_tasnas
103200140604     c*
103300140716     c                   EVAL      fatnumo74  = f_tasnft
103400140716     c                   MOVEL     f_tasdft      wdat
103500140604     c                   EXSR      edtdat
103600140604     c                   EVAL      fatdatao74 = wdate
103700140604     c*
103800140716     c                   EVAL      disnumo74  = f_tasndc
103900140716c    c                   MOVEL     f_tasddc      wdat
104000140604     c                   EXSR      edtdat
104100140604     c                   EVAL      disdatao74 = wdate
104200140604     **?Immagine lettera di vettura.
104300140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
104400140604     C                   IF        LDVO74 <> 'N'
104500140604     c                   CLEAR                   dtasflo
104600140716     c                   MOVEL     f_tasflo      dtasflo
104700140604     c                   IF        §floiml <> *BLANK
104800140604     c                   EVAL      ldvo74 = 'S'
104900140604     C                   EVAL      SpImLDVO74 =
105000140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
105100140716     C                             %EDITC(f_taslnp:'X') +
105200140716     C                             %EDITC(f_tasnrs:'X') +
105300140716     C                             %EDITC(f_tasnsp:'X')
105400140604     **?Reperisco il check code della spedizione POD
105500140604     C                   EVAL      PChkCdeO74 =
105600140604     C                             GetSpeChkCde(
105700140716     C                             %EDITC(f_tasaas:'X'):
105800140716     C                             %EDITC(f_taslnp:'X') +
105900140716     C                             %EDITC(f_tasnrs:'X') +
106000140716     C                             %EDITC(f_tasnsp:'X'):
106100140604     C                             ChkCode:nEsito)
106200140604     C                   ENDIF
106300140604     C                   ENDIF
106400140604     c*
106500140716     c                   MOVE      f_tasccm      n4
106600140604if  1c                   IF        n4 <> 8888                                   *codificato
106700140604     c                   CLEAR                   bs69ds
106800140604     c                   CLEAR                   acods
106900140604     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
107000140716     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
107100140716     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
107200140604     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
107300140604     c                   PARM                    bs69ds
107400140604     c                   PARM                    acods
107500140604     c                   PARM                    indds
107600140604     c                   PARM                    clpds
107700140604     c                   PARM                    clsds
107800140604if  2c                   IF        o69err <> '1'                                *no errore
107900140604     c                   EVAL      mito74    = acorag
108000140604     c                   EVAL      mitindo74 = indvia
108100140604     c                   EVAL      mitloco74 = indcit
108200140604     c                   MOVEL     indsta        a3
108300140604if  3c                   IF        a3 = *blanks
108400140604     c                   EVAL      mitnazo74 = indprv
108500140604x   3c                   ELSE
108600140604     c                   EVAL      mitnazo74 = a3
108700140604e   3c                   ENDIF
108800140604if  3c                   IF        indcae <> *blanks
108900140604     c                   EVAL      mitcapo74 = indcae
109000140604X   3c                   ELSE
109100140604     c                   MOVEL     indcap        a5
109200140604     c                   EVAL      mitcapo74 = a5
109300140604e   3c                   ENDIF
109400140604e   2c                   ENDIF
109500140604x   1c                   ELSE                                                   *non codificato
109600140716     c                   Z-ADD     f_tasaas      kaaaas
109700140716     c                   Z-ADD     f_taslnp      kaalnp
109800140716     c                   Z-ADD     f_tasnrs      kaanrs
109900140716     c                   Z-ADD     f_tasnsp      kaansp
110000140604     c                   MOVEL     'M'           kaatrc
110100140604     c     keytaa        CHAIN     titaa30c                           99
110200140604if  2c                   IF        NOT *in99
110300140604     c                   EVAL      mito74    = taarsc
110400140604     c                   EVAL      mitindo74 = taaind
110500140604     c                   EVAL      mitcapo74 = taacap
110600140604     c                   EVAL      mitloco74 = taaloc
110700140604if  3c                   IF        taanaz = *blanks
110800140604     c                   EVAL      mitnazo74 = taaprv
110900140604x   3c                   ELSE
111000140604     c                   EVAL      mitnazo74 = taanaz
111100140604e   3c                   ENDIF
111200140604e   2c                   ENDIF
111300140604e   1c                   ENDIF
111400140604     c* 2a ragione sociale destinatario.
111500140604     c                   CLEAR                   tita4000
111600140604     c                   CLEAR                   tita4p00
111700140716     c                   Z-ADD     f_tasaas      ka4aas
111800140716     c                   Z-ADD     f_taslnp      ka4lnp
111900140716     c                   Z-ADD     f_tasnrs      ka4nrs
112000140716     c                   Z-ADD     f_tasnsp      ka4nsp
112100140604     c                   MOVEL     'D'           ka4trc
112200140604     c     keyta4        CHAIN     tita430c                           99
112300140604if  1c                   IF        NOT *in99
112400140604     c                   MOVEL     ta4not        desti2O74
112500140604e   1c                   ENDIF
112600140604     c* rif partner estero
112700140604     c                   CLEAR                   tita4000
112800140604     c                   CLEAR                   tita4p00
112900140604     c                   CLEAR                   dsbl4e
113000140716     c                   Z-ADD     f_tasaas      ka4aas
113100140716     c                   Z-ADD     f_taslnp      ka4lnp
113200140716     c                   Z-ADD     f_tasnrs      ka4nrs
113300140716     c                   Z-ADD     f_tasnsp      ka4nsp
113400140604     c                   MOVEL     'E'           ka4trc
113500140604     c     keyta4        CHAIN     tita430c                           99
113600140604if  1c                   IF        NOT *in99
113700140604     c                   MOVEL     ta4not        dsbl4e
113800140604e   1c                   ENDIF
113900140604     c
114000140604     c                   CLEAR                   tita4000
114100140604     c                   CLEAR                   tita4p00
114200140604     c                   CLEAR                   dta4a
114300140716     c                   Z-ADD     f_tasaas      ka4aas
114400140716     c                   Z-ADD     f_taslnp      ka4lnp
114500140716     c                   Z-ADD     f_tasnrs      ka4nrs
114600140716     c                   Z-ADD     f_tasnsp      ka4nsp
114700140604     c                   MOVEL     'A'           ka4trc
114800140604     c     keyta4        CHAIN     tita430c                           99
114900140604if  1c                   IF        NOT *in99
115000140604     c                   MOVEL     ta4not        dta4a
115100140604e   1c                   ENDIF
115200140604     ** Riferimento mittente numerico (c'è sempre).
115300140716     C                   EVAL      rifero74 = f_tasrmn
115400140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
115500140604     C                   IF        §ta4arma <> *BLANK AND
115600140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
115700140604     C                   EVAL      rifero74a = §ta4arma
115800140604     C                   ENDIF
115900160122     c** Natura merce
116000160122     c                   EVAL      naturao74 = §ta4anas
116100140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
116200140604     C                   IF        §b4erp <> *BLANK AND
116300140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
116400140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
116500140604     C                   EVAL      rpto74 = §b4erp
116600140604     C                   ENDIF
116700140604
116800140604     **?Reperisco il check code della spedizione.
116900140604     C                   EVAL      SChkCdeO74 =
117000140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
117100140604     C                             NSpedizI74:ChkCode:nEsito)
117200140604
117300140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
117400140604     C                   IF        KSCI74 <> *BLANK
117500140604     c                   SETOFF                                           98
117600140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
117700140604     C                   RESET                   tis7700i0
117800141126     C                   EVAL      tis7700i0.aas = kasaasm
117900141126     C                   EVAL      tis7700i0.lnp = kaslnpm
118000141126     C                   EVAL      tis7700i0.nrs = kasnrsm
118100141126     C                   EVAL      tis7700i0.nsp = kasnspm
118200140604     C                   EVAL      tis7700i0.ksu = kscI74
118300140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
118400140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
118500140604     C                             : rpyEsito
118600140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
118700140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
118800140604     C                             )
118900140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
119000140604     C                             tis7700o0.bollValida = *ON)
119100140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
119200140604if  2c                   IF        NOT *in98                                      *non trovato
119300140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
119400140604     C                   reset                   TIS778DSO
119500140604     C                   EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
119600140604     C                   RETURN
119700140604     C                   ENDIF
119800140604e   2c                   ENDIF
119900140604     C                   ENDIF
120000140604     c*
120100140604     ** Importo assicurato.
120200140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
120300140716     C                   EVAL      vasO74 = f_tasVas
120400141015     C                   EVAL      iasO74 = f_tasIas
120500140604     C                   ENDIF
120600140604     **
120700140604     c                   ENDSR
120800000000     c*--------------------------------------------------------------------------------------------*
120900000000     c* imposta i campi della DS di Output dagli eventi
121000000000     c*--------------------------------------------------------------------------------------------*
121100000000     c     impdsoevb     BEGSR
121200000000     c*
121300000000     c* azzera le variabili di lavoro
121400000000     c                   CLEAR                   tote                           *contatore eventi
121500000000     c                   CLEAR                   seql                           *sequenza legami
121600000000     c                   CLEAR                   spsort                         *per ordinamento
121700000000     c                   CLEAR                   spseql                         *di memorizzazione
121800000000     c                   CLEAR                   spseqe
121900000000     c                   CLEAR                   spdevhev
122000000000     c                   CLEAR                   spdeve
122100000000     c                   CLEAR                   spheve
122200000000     c                   CLEAR                   spfle
122300000000     c                   CLEAR                   spdev
122400000000     c                   CLEAR                   spcev
122500130312     c                   CLEAR                   spspe
122600150618     c                   CLEAR                   ds7o
122700000000     c*
122800000000     c* imposta la sequenza legami per la bolla di Input
122900000000     c                   Z-ADD     1             seql                           *primo legaqme
123000000000     c*---
123100000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
123200000000     c*---
123300160127     c****
123400160127     c* Bolla presa in carico se ancora da partire
123500160420     c**                 if        esito='P'
123600160420     c**                 MOVEL     '700'         wcev                           *causale evento
123700160420     c**                 EXSR      deccevsta
123800160420if  4c**                 IF        werr <> '1'                                  *no errore
123900160420     c**                 ADD       1             tote
124000160420     c**                 MOVEL     wcdex         smdev(tote)
124100160420     c**                 exsr      evbinca
124200160420     c**                 endif
124300160420     c**                 endif
124400000000     c***
124500000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
124600000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
124700000000     c*   si trasforma nella consegna anomala della mamma
124800000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
124900000000     c***
125000000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
125100000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
125200160318     c
125300160318    0c                   if        esito<>'P'
125400000000     c                   EVAL      kblaan = s_tasaas
125500000000     c                   EVAL      kbllpn = s_taslnp
125600000000     c                   EVAL      kblnrn = s_tasnrs
125700000000     c                   EVAL      kblnsn = s_tasnsp
125800000000     c     keylbl        CHAIN     fnlbl01l                           99
125900000000if  1c                   IF        NOT *in99                                    *ha la mamma
126000000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
126100000000     c                   EVAL      kasaas = lblaap
126200000000     c                   EVAL      kaslnp = lbllpp
126300000000     c                   EVAL      kasnrs = lblnrp
126400000000     c                   EVAL      kasnsp = lblnsp
126500000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
126600000000if  2c                   IF        NOT *in99 AND
126700000000     c                             tascca <> *blanks                            *ha cons.anomala
126800060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
126900060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
127000060622     C                             :rpyEsito)
127100060622if  3c*                  IF        *in98                                        *trovata
127200060626     C                   IF        rpyOpCode = 'FOUND'
127300041222     **?Consegna anomala, POD non visualizzabile.
127400060622     C*                  IF        sapod(jj) = 'N'
127500060622     C                   IF        §7OPODImg = 'N'
127600041222     C                   EVAL      LDVO74 = 'N'
127700041222     C                   ENDIF
127800060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
127900060622     C                   IF        §7oinc <> *blanks                            *ok per internet
128000000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
128100041222e   3c                   ENDIF
128200000000e   3c                   ENDIF
128300000000e   2c                   ENDIF
128400000000e   1c                   ENDIF
128500160318e   0c                   ENDIF
128600000000     c*
128700030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
128800000000     c                   ADD       1             tote
128900060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
129000060622     c                   MOVEL     §7odei        smdev(tote)
129100030203x   3c                   ELSE                                                   *evento telef.P.O.
129200060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
129300030203e   3c                   ENDIF
129400030203e   2c                   ENDIF
129500030203     c*
129600030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
129700060622     c                   MOVEL     §7ocev        wcev                           *causale evento
129800000000     c                   EXSR      evbrit                                       *imposta evento
129900030203x   2c                   ELSE                                                   *NO sost.ritiro
130000160318     c
130100160408     c* per bolle in partenza devo impostare la data ritiro solo se spuntato in entrata
130200160408     c***                if        esito<>'P'
130300160413     c                   if        esito= 'P'
130400160413     c                   if        arbdse>0
130500160408     c                   eval      s_tasdrt=arbdrt
130600160413     c                   else
130700160413     c                   eval      s_tasdrt=00000000
130800160408     c                   endif
130900160413     c                   endif
131000160318     c
131100030203if  3c                   IF        s_tasdrt > *zeros
131200000000     c                   MOVEL     '701'         wcev                           *causale evento
131300000000     c                   EXSR      deccevsta
131400030203if  4c                   IF        werr <> '1'                                  *no errore
131500000000     c                   ADD       1             tote
131600030130     c                   MOVEL     wcdex         smdev(tote)
131700000000     c                   EXSR      evbrit                                       *imposta evento
131800030203e   4c                   ENDIF
131900030203e   3c                   ENDIF
132000030203e   2c                   ENDIF
132100160408e   2c***                ENDIF
132200140710     ** ID bolla madre.                                                          dubito figlia
132300160318     c                   if        esito='P'
132400160318     c                   EVAL      waas = arbaas
132500160318     c                   EVAL      wlnp = arblnp
132600160318     c                   EVAL      wnrs = arbnrs
132700160318     c                   EVAL      wnsp = arbnsp
132800160318     c                   else
132900160318     c
133000160318     c* questi dati per le bolle solo in partenza (non legate)
133100160318     c*  non li devo caricare
133200080131     c                   EVAL      waas = s_tasaas
133300080131     c                   EVAL      wlnp = s_taslnp
133400080131     c                   EVAL      wnrs = s_tasnrs
133500080131     c                   EVAL      wnsp = s_tasnsp
133600000000     c* Partenza merce
133700000000     c                   EVAL      lduc = s_tasduc                              *data partenza
133800000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
133900000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
134000000000     c                   EXSR      evbpar
134100000000     c* Arrivo merce
134200110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
134300110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
134400140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
134500140723     c                             gen§ar5ArrDt <> *ZERO
134600140723     c                   MONITOR
134700140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
134800140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
134900140723     c                   ON-ERROR
135000140723     c                   ENDMON
135100140723     c                   ENDIF
135200100225     c                   EVAL      llna = s_taslna                              *linea arrivo
135300150617     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
135400090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
135500090826     c                   EXSR      evbarr
135600000000     c*
135700000000     c* Consegna merce
135800000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
135900000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
136000000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
136100000000     c                   EVAL      llna = s_taslna                              *linea arrivo
136200150618     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
136300000000     c                   EXSR      evbcon
136400000000     c* Lettera anomalia
136500000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
136600000000     c                   EXSR      memdct
136700000000e   1c                   ENDIF
136800000000     c*---
136900000000     c* memorizza gli eventi dal file eventi della bolla di input
137000000000     c*---
137100160406     c* devo reimpostare la linea di arrivo della bolla richiesta, perchè potrei
137200150618     c*  aver chainato una bolla "mamma" ed aver sporcato il campo
137300150618     c                   eval      taslna=s_taslna
137400000000     c                   EXSR      memevb
137500000000     c*---
137600000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
137700000000     c*---
137800000000     c                   EXSR      elaevelbl
137900160318     c                   endif
138000000000     c*
138100000000     c* imposta contatore eventi da visualizzare nella DS di Output
138200000000     c                   EVAL      cnteveo74 = tote
138300000000     c*---
138400000000     c* ordina gli eventi per sequenza legami / Data+ora evento
138500000000     c*---
138600000000     c* compone la schiera da ordinare
138700000000do  1c     1             DO        200           j
138800000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
138900000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
139000000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
139100000000     c                   Z-ADD     smi(j)        dsi                             -indice
139200000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
139300000000e   1c                   ENDDO
139400000000     c*
139500000000     c* ordina la schiera composta
139600000000     c                   SORTA     spsort
139700000000     c*
139800000000     c* ricompone le schiere da quella ordinata
139900000000     c                   Z-ADD     *zeros        jj
140000000000do  1c     1             DO        200           j
140100000000if  2c                   IF        spsort(j) >*zeros
140200000000     c                   ADD       1             jj
140300000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
140400000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
140500000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
140600000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
140700000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
140800000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
140900000000     c                   MOVEL     smheve(dsi)   spheve(jj)
141000000000     c                   MOVEL     smfle(dsi)    spfle(jj)
141100000000     c                   MOVEL     smdev(dsi)    spdev(jj)
141200000000     c                   MOVEL     smcev(dsi)    spcev(jj)
141300130312     c                   MOVEL     smspe(dsi)    spspe(jj)
141400000000e   2c                   ENDIF
141500000000e   1c                   ENDDO
141600101104     **
141700101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
141800101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
141900101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
142000101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
142100101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
142200101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
142300101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
142400101104     ** Ricordati che l'ordine per data e ora è discendente.
142500101104     C                   EVAL      j = 0
142600101104     C                   DOU       j = 0
142700101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
142800101104     C                                        : spcev : j+1)
142900101104     C                   IF        j = 0
143000101104     C                   LEAVE
143100101104     C                   ENDIF
143200101104     C                   IF        j > 1 AND
143300101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
143400101104     C                   EVAL      dev(1) = spdeve(j-1)
143500101104     C                   EVAL      hev(1) = spheve(j-1)
143600101104     C                   EVAL      fle(1) = spfle(j-1)
143700101104     C                   EVAL      eve(1) = spdev(j-1)
143800101104     C                   EVAL      cev(1) = spcev(j-1)
143900130312     C                   EVAL      spe(1) = spspe(j-1)
144000101104     C                   EVAL      spdeve(j-1) = spdeve(j)
144100101104     C                   EVAL      spheve(j-1) = spheve(j)
144200101104     C                   EVAL      spfle(j-1) = spfle(j)
144300101104     C                   EVAL      spdev(j-1) = spdev(j)
144400101104     C                   EVAL      spcev(j-1) = spcev(j)
144500130312     C                   EVAL      spspe(j-1) = spspe(j)
144600101104     C                   EVAL      spdeve(j) = dev(1)
144700101104     C                   EVAL      spheve(j) = hev(1)
144800101104     C                   EVAL      spfle(j) = fle(1)
144900101104     C                   EVAL      spdev(j) = eve(1)
145000101104     C                   EVAL      spcev(j) = cev(1)
145100130312     C                   EVAL      spspe(j) = spe(1)
145200101104     C                   ENDIF
145300101104     C                   ENDDO
145400000000     c*
145500120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
145600120831     c                   CLEAR                   cntEveO74
145700000000do  1c     1             DO        50            j
145800120831     c                   IF        spcev(j) = *BLANK
145900120831     c                   LEAVE
146000120831     c                   ENDIF
146100000000     c                   EVAL      dev(j) = spdeve(j)
146200000000     c                   EVAL      hev(j) = spheve(j)
146300000000     c                   EVAL      fle(j) = spfle(j)
146400041007     c                   EVAL      eve(j) = spdev(j)
146500041007     c                   EVAL      cev(j) = spcev(j)
146600130312     c                   EVAL      spe(j) = spspe(j)
146700101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
146800100909     c                   CLEAR                   hev(j)
146900100909     c                   ENDIF
147000120831     c                   EVAL      cntEveO74 += 1
147100000000e   1c                   ENDDO
147200000000     c*
147300000000     c                   ENDSR
147400000000     c*--------------------------------------------------------------------------------------------*
147500000000     c* memorizza gli eventi dal file eventi della bolla di input
147600000000     c*--------------------------------------------------------------------------------------------*
147700000000     c     memevb        BEGSR
147800150617     c* il programma usa il campo TASLNA per attribuire gli eventi della bolla
147900150617     c*  usa il TITAS che ha letto per ultimo che può essere quello della bolla  mamma
148000150617     c*  o della bolla figlia
148100150617     c*  per sapere se la bolla è italia o estera non devo usare l'ultima figlia
148200150617     c*  ma lna della bolla che sta leggendo in questo momento
148300150617     c                   Z-ADD     tasLna        worg
148400150617     c                   exsr      decorg
148500150617     c                   eval      wtasfl1 =  wofl1
148600000000     c***
148700000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
148800000000     c* . se SI lo aggiorna solo nella sequenza legame
148900000000     c*   se NO lo inserisce
149000000000     c***
149100000000     c                   Z-ADD     waas          kvbaas
149200000000     c                   Z-ADD     wlnp          kvblnp
149300000000     c                   Z-ADD     wnrs          kvbnrs
149400000000     c                   Z-ADD     wnsp          kvbnsp
149500000000     c     keyevb        SETLL     fnevb01l
149600000000     c     keyevb        READE     fnevb01l                               99
149700000000do  1c                   DOW       NOT *in99
149800000000if  2c                   IF        evbatb = *blanks                             *valido
149900140108     c                   MOVEL     evbdev        wdev
150000140108     c                   MOVEL     evbhev        whev
150100080131if  3c                   IF        evbDev > *zeros
150200000000     c                   MOVEL     evbcev        wcev
150300000000     c                   EXSR      deccevsta
150400070109     c                   EXSR      eventoAnnulla
150500000000if  4c                   IF        werr <> '1'                                  *no errore
150600000000     c*
150700000000     c* controlla se evento già inserito
150800000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
150900000000     c                   MOVEL     'S'           newevb                         *nuovo evento
151000000000do  5c     1             DO        200           j
151100000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
151200080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
151300000000     c                   MOVEL     'N'           newevb                         *già inserito
151400000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
151500150618     C                   IF        wtasfl1 = ESTERO
151600150618     c                   Z-ADD     tasLna        worg
151700150618     c                   else
151800150618     c                   Z-ADD     evbfle        worg
151900150618     c                   endif
152000000000     c                   EXSR      decorg
152100000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
152200080201     c                   MOVEL     wDev          wdat
152300080201     c                   EXSR      edtdat
152400080201     c                   MOVEL     wdate         smdeve(j)
152500080201     c                   MOVEL     wHev          wora
152600080201     c                   EXSR      edtora
152700080201     c                   MOVEL     worae         smheve(j)
152800130313     c                   MOVEL     dsevbspe      smspe(j)
152900000000     c                   LEAVE                                                  *uscita ciclo
153000000000e   6c                   ENDIF
153100000000e   5c                   ENDDO
153200000000     c*
153300000000     c* incrementa contatore eventi
153400000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
153500000000     c                   ADD       1             tote
153600000000     c* indice schiera
153700000000     c                   Z-ADD     tote          smi(tote)
153800000000     c* sequenza legame
153900000000     c                   Z-ADD     seql          smseql(tote)
154000000000     c* sequenza evento
154100000000     c                   Z-ADD     020           smseqe(tote)
154200000000     c* descrizione
154300030130     c                   MOVEL     wcdex         smdev(tote)
154400000000     c* data
154500080201     c                   MOVEL     wDev          wdat
154600000000     c                   EXSR      edtdat
154700000000     c                   MOVEL     wdate         smdeve(tote)
154800000000     c* ora
154900080201     c                   MOVEL     wHev          wora
155000000000     c                   EXSR      edtora
155100000000     c                   MOVEL     worae         smheve(tote)
155200000000     c* filiale
155300150617     C***                IF        wrkLnaItaEst = ESTERO
155400150617     C                   IF        wtasfl1 = ESTERO
155500140722     c                   Z-ADD     tasLna        worg
155600140722     c                   else
155700140722     c                   Z-ADD     evbfle        worg
155800140722     c                   endif
155900140722     c                   EXSR      decorg
156000140722     c                   MOVEL     wodes         smfle(tote)
156100000000     c* causale
156200000000     c                   MOVEL     evbcev        smcev(tote)
156300130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
156400130312     c* bolla legata
156500130312     c                   MOVEL     dsevbspe      smspe(tote)
156600000000     c* data+ora evento
156700080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
156800000000e   5c                   ENDIF
156900000000e   4c                   ENDIF
157000000000e   3c                   ENDIF
157100000000e   2c                   ENDIF
157200010717     c     keyevb        READE     fnevb01l                               99
157300000000e   1c                   ENDDO
157400000000     c*
157500000000     c                   ENDSR
157600000000     c*--------------------------------------------------------------------------------------------*
157700000000     c* memorizza gli "eventi fissi"
157800000000     c*--------------------------------------------------------------------------------------------*
157900000000     c     memevefix     BEGSR
158000080131     ** ID bolla in canna.
158100080131     c                   EVAL      waas = tasaas
158200080131     c                   EVAL      wlnp = taslnp
158300080131     c                   EVAL      wnrs = tasnrs
158400080131     c                   EVAL      wnsp = tasnsp
158500000000     c*
158600000000     c* Partenza merce
158700000000     c                   EVAL      lduc = tasduc                                *data partenza
158800000000     c                   EVAL      llnp = taslnp                                *filiale partenza
158900000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
159000000000     c                   EXSR      evbpar
159100000000     c* Consegna merce
159200000000     c                   EVAL      lcca = tascca                                *consegna anomala
159300000000     c                   EVAL      ldcm = tasdcm                                *data consegna
159400000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
159500000000     c                   EVAL      llna = taslna                                *linea arrivo
159600150618     c                   EVAL      ltfa = tastfa                                *linea arrivo
159700000000     c                   EXSR      evbcon
159800000000     c*
159900000000     c                   ENDSR
160000000000     c*--------------------------------------------------------------------------------------------*
160100000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
160200000000     c*--------------------------------------------------------------------------------------------*
160300000000     c     elaevelbl     BEGSR
160400000000     c*
160500000000     c* azzera le variabili di lavoro
160600000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
160700000000     c                   EVAL      deplpn = *zeros
160800000000     c                   EVAL      depnrn = *zeros
160900000000     c                   EVAL      depnsn = *zeros
161000000000     c*
161100000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
161200000000     c                   EVAL      kbllpp =  s_taslnp
161300000000     c                   EVAL      kblnrp =  s_tasnrs
161400000000     c                   EVAL      kblnsp =  s_tasnsp
161500000000     c*
161600000000     c* ciclo fino a fine mamme
161700000000     c     ke2lbl        SETLL     fnlbl02l
161800000000     c     ke2lbl        READE     fnlbl02l                               99
161900000000do  1c                   DOW       NOT *in99
162000000000     c*
162100000000     c* ciclo fino a fine figlie della mamma
162200000000do  2c                   DOW       NOT *in99
162300000000     c*
162400000000     c* legge la bolla dal legame
162500000000     c                   EVAL      kasaas = lblaan                              *figlia
162600000000     c                   EVAL      kaslnp = lbllpn
162700000000     c                   EVAL      kasnrs = lblnrn
162800000000     c                   EVAL      kasnsp = lblnsn
162900000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
163000000000if  3c                   IF        NOT *in99 AND                                *esistente
163100000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
163200000000     c                             tastbl <> 'A3' AND
163300000000     c                             tastbl <> 'F3' AND
163400000000     c                             tastbl <> 'AP' AND
163500000000     c                             tastbl <> 'FC'
163600041222     **?Immagine lettera di vettura.
163700041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
163800041222     C                   IF        LDVO74 <> 'N'
163900040330     c                   MOVEL     tasflo        dtasflo
164000040330     c                   IF        §floiml <> *BLANK
164100050325     c                   EVAL      ldvo74 = 'S'
164200040330     C                   EVAL      SpImLDVO74 =
164300040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
164400040330     C                             %EDITC(taslnp:'X') +
164500040330     C                             %EDITC(tasnrs:'X') +
164600040330     C                             %EDITC(tasnsp:'X')
164700041223     **?Reperisco il check code della spedizione POD
164800041223     C                   EVAL      PChkCdeO74 =
164900041223     C                             GetSpeChkCde(
165000041223     C                             %EDITC(tasaas:'X'):
165100041223     C                             %EDITC(taslnp:'X') +
165200041223     C                             %EDITC(tasnrs:'X') +
165300041223     C                             %EDITC(tasnsp:'X'):
165400041223     C                             ChkCode:nEsito)
165500040330     C                   ENDIF
165600041222     C                   ENDIF
165700000000     c* incrementa la sequenza legami per le bolle legate
165800000000     c                   ADD       1             seql                           *legaqme successivo
165900000000     c*---
166000000000     c* memorizza gli "eventi fissi"
166100000000     c*---
166200000000     c                   EXSR      memevefix
166300000000     c*---
166400000000     c* memorizza gli eventi
166500000000     c*---
166600000000     c                   EVAL      waas = lblaan                                *figlia
166700000000     c                   EVAL      wlnp = lbllpn
166800000000     c                   EVAL      wnrs = lblnrn
166900000000     c                   EVAL      wnsp = lblnsn
167000000000     c                   EXSR      memevb
167100000000     c*
167200000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
167300000000     c                   EVAL      depaan = lblaan                              *ultima figlia
167400000000     c                   EVAL      deplpn = lbllpn
167500000000     c                   EVAL      depnrn = lblnrn
167600000000     c                   EVAL      depnsn = lblnsn
167700000000e   3c                   ENDIF
167800000000     c*
167900000000     c* lettura successiva legami
168000000000     c     ke2lbl        READE     fnlbl02l                               99
168100000000e   2c                   ENDDO                                                  *fine figlie mamma
168200000000     c*
168300000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
168400000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
168500000000     c                             kbllpp <> deplpn OR
168600000000     c                             kblnrp <> depnrn OR
168700000000     c                             kblnsp <> depnsn
168800000000     c* nuova chiave
168900000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
169000000000     c                   EVAL      kbllpp =  deplpn
169100000000     c                   EVAL      kblnrp =  depnrn
169200000000     c                   EVAL      kblnsp =  depnsn
169300000000     c* controlla figlie
169400000000     c     ke2lbl        SETLL     fnlbl02l
169500000000     c     ke2lbl        READE     fnlbl02l                               99
169600000000if  3c                   IF        *in99                                        *non ha figlie
169700000000     c                   LEAVE                                                  *esce dal ciclo
169800000000e   3c                   ENDIF
169900000000x   2c                   ELSE                                                   *sempre stessa mamma
170000000000     c                   LEAVE                                                  *esce dal ciclo
170100000000e   2c                   ENDIF
170200000000e   1c                   ENDDO                                                  *fine mamme
170300000000     c*
170400000000     c                   ENDSR
170500160127     c*--------------------------------------------------------------------------------------------*
170600160127     c* Imposta l'evento presa in carico se bolla in partenza
170700160127     c*--------------------------------------------------------------------------------------------*
170800160127     c     evbinca       BEGSR
170900160127     c*
171000160127     c* indice schiera
171100160127     c                   Z-ADD     tote          smi(tote)
171200160127     c* sequenza legame
171300160127     c                   Z-ADD     seql          smseql(tote)
171400160127     c* sequenza evento
171500160127     c                   Z-ADD     008           smseqe(tote)                   *primo evento
171600160127     c* data
171700160127if  1c                   IF        arbdim   > *zeros
171800160127     c                   MOVEL     arbdim        wdat
171900160127     c                   EXSR      edtdat
172000160127     c                   MOVEL     wdate         smdeve(tote)
172100160127e   1c                   ENDIF
172200160127     c* filiale
172300160127     c                   Z-ADD     arblnp        worg
172400160127     c                   EXSR      decorg
172500160127     c
172600160127     c                   MOVEL     wodes         smfle(tote)
172700160127     c* causale
172800160127     c                   MOVEL     wcev          smcev(tote)
172900160127     c* data+ora evento
173000160127     c                   MOVEL     wdat          smdevhev(tote)
173100160127     c                   MOVE      wora          smdevhev(tote)
173200160127     c*
173300160127     c                   ENDSR
173400000000     c*--------------------------------------------------------------------------------------------*
173500000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
173600000000     c*--------------------------------------------------------------------------------------------*
173700000000     c     evbrit        BEGSR
173800160318     c
173900000000     c*
174000000000     c* indice schiera
174100000000     c                   Z-ADD     tote          smi(tote)
174200000000     c* sequenza legame
174300000000     c                   Z-ADD     seql          smseql(tote)
174400000000     c* sequenza evento
174500000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
174600160407     c* data ritiro solo se spuntato
174700160318     c                   if        esito='P'
174800160407     c                   if        arbdse>0
174900160318if  1c                   IF        arbdrt > *zeros
175000160318     c                   MOVEL     arbdrt        wdat
175100160318     c                   EXSR      edtdat
175200160318     c                   MOVEL     wdate         smdeve(tote)
175300160318e   1c                   ENDIF
175400160318     c* ora  da FPP
175500160318if  2c                   IF        arbfpp = 'P'                                 *PM
175600160318     c                   MOVEL     1800          wora
175700160318x   2c                   ELSE                                                   *AM
175800160318     c                   MOVEL     1200          wora
175900160318e   1c                   ENDIF
176000160318     c                   EXSR      edtora
176100160318     c                   MOVEL     worae         smheve(tote)
176200160318     c* filiale
176300160318     c                   Z-ADD     arblnp        worg
176400160318     c                   EXSR      decorg
176500160407
176600160407     c                   endif
176700160407     c
176800160318     c
176900160318     c                   else
177000160318     c
177100000000if  1c                   IF        s_tasdrt > *zeros
177200000000     c                   MOVEL     s_tasdrt      wdat
177300000000     c                   EXSR      edtdat
177400000000     c                   MOVEL     wdate         smdeve(tote)
177500000000e   1c                   ENDIF
177600000000     c* ora
177700000000if  1c                   IF        s_tashrt > *zeros
177800000000     c                   MOVEL     s_tashrt      wora
177900000000x   1c                   ELSE
178000000000if  2c                   IF        s_tasfpp = 'P'                               *PM
178100000000     c                   MOVEL     1800          wora
178200000000x   2c                   ELSE                                                   *AM
178300000000     c                   MOVEL     1200          wora
178400000000e   2c                   ENDIF
178500000000e   1c                   ENDIF
178600000000     c                   EXSR      edtora
178700000000     c                   MOVEL     worae         smheve(tote)
178800000000     c* filiale
178900000000     c                   Z-ADD     s_taslnp      worg
179000000000     c                   EXSR      decorg
179100150618     c
179200150618     c* Se la bolla è import e devo emettere l'evento di telefonare
179300150618     c*  non posso indciare la filiale esterna, ma l'hub italiano
179400150618     c                   if        wofl1=ESTERO and sostrit='S' and
179500150618     c                             §7oinc='T'
179600150618     c                   z-add     s_tastfp      worg
179700150618     c                   EXSR      decorg
179800150618     c                   endif
179900160318     c                   endif
180000160318
180100000000     c                   MOVEL     wodes         smfle(tote)
180200000000     c* causale
180300000000     c                   MOVEL     wcev          smcev(tote)
180400000000     c* data+ora evento
180500000000     c                   MOVEL     wdat          smdevhev(tote)
180600000000     c                   MOVE      wora          smdevhev(tote)
180700000000     c*
180800000000     c                   ENDSR
180900000000     c*--------------------------------------------------------------------------------------------*
181000000000     c* Imposta l'evento Partenza
181100000000     c*--------------------------------------------------------------------------------------------*
181200000000     c     evbpar        BEGSR
181300000000     c*
181400000000     c* controlla SE e COME inserire l'evento
181500000000if  1c                   IF        lduc > *zeros
181600000000     c* imposta causale
181700000000     c                   MOVEL     '702'         wcev
181800000000     c                   EXSR      deccevsta
181900000000if  2c                   IF        werr <> '1'                                  *no errore
182000000000     c* imposta filiale
182100000000     c                   Z-ADD     llnp          worg
182200000000     c                   EXSR      decorg
182300000000     c* imposta data
182400101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
182500000000     c                   MOVEL     ldrt          wdat                           *data ritiro
182600000000x   3c                   ELSE                                                   partenza ITALIA
182700000000     c                   MOVEL     lduc          wdat                           *data partenza
182800000000e   3c                   ENDIF
182900000000     c                   EXSR      edtdat
183000000000     c* imposta ora
183100000000     c                   MOVEL     2100          wora                           *ora imposta
183200000000     c                   EXSR      edtora
183300000000     c*
183400000000     c* controlla se l'evento è già stato inserito
183500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
183600000000do  3c     1             DO        200           j
183700000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
183800000000     c                   MOVE      wora          lavdevhev
183900000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
184000000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
184100000000     c                   MOVEL     'N'           newevb                         *già inserito
184200000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
184300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
184400000000     c                   LEAVE                                                  *uscita ciclo
184500000000e   4c                   ENDIF
184600000000e   3c                   ENDDO
184700000000     c*
184800000000     c* incrementa contatore eventi
184900000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
185000000000     c                   ADD       1             tote
185100000000     c* indice schiera
185200000000     c                   Z-ADD     tote          smi(tote)
185300000000     c* sequenza legame
185400000000     c                   Z-ADD     seql          smseql(tote)
185500000000     c* sequenza evento
185600000000     c                   Z-ADD     020           smseqe(tote)
185700000000     c* descrizione
185800030130     c                   MOVEL     wcdex         smdev(tote)
185900000000     c* filiale -impostata sopra-
186000000000     c                   MOVEL     wodes         smfle(tote)
186100000000     c* data    -impostata sopra-
186200000000     c                   MOVEL     wdate         smdeve(tote)
186300000000     c* ora     -impostata sopra-
186400000000     c                   MOVEL     worae         smheve(tote)
186500000000     c* causale -impostata sopra-
186600000000     c                   MOVEL     wcev          smcev(tote)
186700000000     c* data+ora evento
186800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
186900000000     c                   MOVE      wora          smdevhev(tote)
187000000000e   3c                   ENDIF
187100000000e   2c                   ENDIF
187200000000e   1c                   ENDIF
187300000000     c*
187400000000     c                   ENDSR
187500101129     c*------------------------------------------------------------------------*
187600101129     c*
187700101129     c* Imposta l'evento Arrivo
187800101129     c*
187900101129     c*------------------------------------------------------------------------*
188000000000     c     evbarr        BEGSR
188100101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
188200101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
188300101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
188400101129     ** sommato inutile.
188500150618     C                   IF        wrkLnaItaEst = ESTERO
188600150618     C                   LEAVESR
188700150618     C                   ENDIF
188800101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
188900101129     C                   IF        lhti = *ZERO
189000101129     C                   LEAVESR
189100101129     C                   ENDIF
189200000000     c*
189300000000     c* controlla SE e COME inserire l'evento
189400000000if  1c                   IF        ldti > *zeros OR
189500000000     c                             ldcm > *zeros
189600000000     c* imposta causale
189700101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
189800000000     c                   EXSR      deccevsta
189900000000if  2c                   IF        werr <> '1'                                  *no errore
190000000000     c* imposta filiale
190100150617     c                   Z-ADD     llna          worg
190200000000     c                   EXSR      decorg
190300000000     c* imposta data
190400000000if  3c                   IF        ldti > *zeros
190500000000     c                   MOVEL     ldti          wdat                           *data arrivo
190600000000x   3c                   ELSE
190700000000     c                   MOVEL     ldcm          wdat                           *data consegna
190800000000e   3c                   ENDIF
190900000000     c                   EXSR      edtdat
191000000000     c* imposta ora
191100000000if  3c                   IF        lhti > *zeros
191200000000     c                   MOVEL     lhti          wora                           *ora arrivo
191300000000x   3c                   ELSE
191400000000     c                   MOVEL     0600          wora                           *ora imposta
191500000000e   3c                   ENDIF
191600000000     c                   EXSR      edtora
191700000000     c*
191800000000     c* controlla se l'evento è già stato inserito
191900000000     c                   MOVEL     'S'           newevb                         *nuovo evento
192000000000do  3c     1             DO        200           j
192100000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
192200000000     c                   MOVE      wora          lavdevhev
192300000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
192400000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
192500000000     c                   MOVEL     'N'           newevb                         *già inserito
192600000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
192700000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
192800000000     c                   LEAVE                                                  *uscita ciclo
192900000000e   4c                   ENDIF
193000000000e   3c                   ENDDO
193100000000     c*
193200000000     c* incrementa contatore eventi
193300000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
193400000000     c                   ADD       1             tote
193500000000     c* indice schiera
193600000000     c                   Z-ADD     tote          smi(tote)
193700000000     c* sequenza legame
193800000000     c                   Z-ADD     seql          smseql(tote)
193900000000     c* sequenza evento
194000000000     c                   Z-ADD     020           smseqe(tote)
194100000000     c* descrizione
194200030130     c                   MOVEL     wcdex         smdev(tote)
194300000000     c* data    -impostata sopra-
194400000000     c                   MOVEL     wdate         smdeve(tote)
194500000000     c* ora     -impostata sopra-
194600000000     c                   MOVEL     worae         smheve(tote)
194700000000     c* filiale -impostata sopra-
194800000000     c                   MOVEL     wodes         smfle(tote)
194900000000     c* causale -impostata sopra-
195000000000     c                   MOVEL     wcev          smcev(tote)
195100000000     c* data+ora evento
195200000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
195300000000     c                   MOVE      wora          smdevhev(tote)
195400000000e   3c                   ENDIF
195500000000e   2c                   ENDIF
195600000000e   1c                   ENDIF
195700000000     c*
195800000000     c                   ENDSR
195900000000     c*--------------------------------------------------------------------------------------------*
196000000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
196100000000     c*--------------------------------------------------------------------------------------------*
196200140714     c     evbcon        BEGSR
196300000000     c*
196400000000     c* azzera variabili di lavoro
196500000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
196600000000     c****
196700000000     c* Consegna merce ->
196800000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
196900000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
197000000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
197100000000     c*   nè lo stato di consegna nè quello di consegna anomala.
197200000000     c****
197300000000     c* controlla SE e COME inserire l'evento
197400000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
197500060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
197600060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
197700060622     C                             :rpyEsito)
197800060622     C                   IF        rpyOpCode = 'FOUND'
197900041222     **?Consegna anomala, POD non visualizzabile.
198000060622     C                   IF        §7OPODImg = 'N'
198100041222     C                   EVAL      LDVO74 = 'N'
198200041222     C                   ENDIF
198300060622     c                   IF        §7oinc <> *blanks                            *ok per internet
198400060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
198500000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
198600000000x   3c                   ELSE                                                   *ha record FNEVB
198700000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
198800000000e   3c                   ENDIF
198900041222e   3c                   ENDIF
199000000000e   2c                   ENDIF
199100000000e   1c                   ENDIF
199200080131     c* imposta data
199300080131if  1c                   IF        ldcm > *zeros
199400080131     c                   MOVEL     ldcm          wdat
199500080131e   1c                   ENDIF
199600080131     c* imposta ora
199700080131if  1c                   IF        lhmc > *zeros
199800080131     c                   MOVEL     lhmc          wora
199900080131e   1c                   ENDIF
200000000000     c* imposta causale
200100000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
200200060622     c                   MOVEL     §7ocev        wcev                           *causale evento
200300080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
200400080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
200500080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
200600080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
200700080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
200800080131     c* ritiro merce) dalla bolla figlia.
200900080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
201000080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
201100080131     c                             AND s_tasNdc < *HIVAL
201200080131     c                   EVAL      kblAap = wAas
201300080131     c                   EVAL      kblLpp = wLnp
201400080131     c                   EVAL      kblNrp = wNrs
201500080131     c                   EVAL      kblNsp = wNsp
201600080131     c     ke2lbl        CHAIN     fnlbl02l
201700080131     c                   IF        %FOUND()
201800080131     c                   EVAL      kasAas = lblAan
201900080131     c                   EVAL      kasLnp = lblLpn
202000080131     c                   EVAL      kasNrs = lblNrn
202100080131     c                   EVAL      kasNsp = lblNsn
202200080131     C     keyTas        CHAIN     titas000      titas000Figlia
202300080131     C                   IF        %FOUND()
202400080131     C                   EVAL      wDat = titas000Figlia.tasDrt
202500080131     C                   EVAL      wOra = titas000Figlia.tasHrt
202600080131     C                   ELSE
202700080131     C     keyTas        CHAIN     titas010      titas010Figlia
202800080131     C                   IF        %FOUND()
202900080131     C                   EVAL      wDat = titas010Figlia.tasDrt
203000080131     C                   EVAL      wOra = titas010Figlia.tasHrt
203100080131     C                   ELSE
203200080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
203300080131     C                   IF        %FOUND()
203400080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
203500080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
203600080131     C                   ENDIF
203700080131     C                   ENDIF
203800080131     C                   ENDIF
203900080131     c                   ENDIF
204000080131     c                   ENDIF
204100000000x   1c                   ELSE                                                   *NO sost.Consegna
204200000000     c                   MOVEL     '704'         wcev                           *causale evento
204300000000e   1c                   ENDIF
204400080131     c* Editazione data e ora evento.
204500080131     c                   EXSR      edtdat
204600080131     c                   EXSR      edtora
204700000000     c* imposta filiale
204800000000     c                   Z-ADD     llna          worg
204900000000     c                   EXSR      decorg
205000150618     c* se si tratta di lna estera e la bolla ha consegna anomala
205100150618     c*  in cui mettere msg generico, allora devo sostituire la filiale
205200150618     c*  con il suo hub
205300150618     c                   if        wofl1=estero and lcca<>' ' and
205400150618     c                             §7oinc='T'
205500150618     c                   Z-ADD     ltfa          worg
205600150618     c                   EXSR      decorg
205700150618     c                   endif
205800150618     c
205900000000     c* controlla se l'evento è già stato inserito
206000000000     c                   MOVEL     'S'           newevb                         *nuovo evento
206100000000do  1c     1             DO        200           j
206200000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
206300000000     c                   MOVE      wora          lavdevhev
206400000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
206500000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
206600000000     c                   MOVEL     'N'           newevb                         *già inserito
206700000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
206800000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
206900000000     c                   LEAVE                                                  *uscita ciclo
207000000000e   2c                   ENDIF
207100000000e   1c                   ENDDO
207200000000     c*
207300000000     c* incrementa contatore eventi
207400030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
207500030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
207600000000     c                   ADD       1             tote                           incrementa contatore
207700060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
207800060622     c                   MOVEL     §7odei        smdev(tote)
207900030203x   4c                   ELSE                                                   *evento telef.P.O.
208000060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
208100030203e   4c                   ENDIF
208200060622     c                   MOVEL     §7ocev        wcev                           *causale evento
208300000000     c                   EXSR      evbcon2                                      *imposta evento
208400030203x   3c                   ELSE                                                   *NO sost.Consegna
208500000000     c*
208600030203if  4c                   IF        ldcm > *zeros
208700030203if  5c                   IF        sostcon <> ' '                               *considera consegna
208800000000     c                   EXSR      deccevsta
208900030203if  6c                   IF        werr <> '1'                                  *no errore
209000000000     c                   ADD       1             tote                           incrementa contatore
209100030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
209200000000     c                   EXSR      evbcon2                                      *imposta evento
209300030203e   6c                   ENDIF
209400030203e   5c                   ENDIF
209500030203e   4c                   ENDIF
209600030203e   3c                   ENDIF
209700000000     c*
209800030203e   1c                   ENDIF
209900030203     c*
210000000000     c                   ENDSR
210100000000     c*--------------------------------------------------------------------------------------------*
210200000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
210300000000     c*--------------------------------------------------------------------------------------------*
210400000000     c     evbcon2       BEGSR
210500000000     c*
210600000000     c* indice schiera
210700000000     c                   Z-ADD     tote          smi(tote)
210800000000     c* sequenza legame
210900000000     c                   Z-ADD     seql          smseql(tote)
211000000000     c* sequenza evento
211100000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
211200000000     c* data    -impostata sopra-
211300000000     c                   MOVEL     wdate         smdeve(tote)
211400000000     c* ora     -impostata sopra-
211500000000     c                   MOVEL     worae         smheve(tote)
211600000000     c* filiale -impostata sopra-
211700000000     c                   MOVEL     wodes         smfle(tote)
211800000000     c* causale -impostata sopra-
211900000000     c                   MOVEL     wcev          smcev(tote)
212000000000     c* data+ora evento
212100000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
212200000000     c                   MOVE      wora          smdevhev(tote)
212300000000     c*
212400000000     c                   ENDSR
212500000000     c*--------------------------------------------------------------------------------------------*
212600000000     c* memorizza i dati della lettera di anomalia
212700000000     c*--------------------------------------------------------------------------------------------*
212800000000     c     memdct        BEGSR
212900000000     c*
213000040906     C                   RESET                   FIDN12DS
213100040906     c                   Z-ADD     waas          I12AAS
213200040906     c                   Z-ADD     wlnp          I12LNP
213300040906     c                   Z-ADD     wnrs          I12NRS
213400040906     c                   Z-ADD     wnsp          I12NSP
213500040906     C                   CALL      'FIDN12R'
213600040906     C                   PARM                    FIDN12DS
213700000000     c* memorizza la prima lettera di anomalia valida
213800040906do  1c                   IF        O12Err = *BLANK
213900040906     C                   DO        O12NCA        K
214000040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
214100040906     C                   EVAL      DCTAAC = O12KeyAAC
214200040906     C                   EVAL      DCTFIL = O12KeyFil
214300040906     C                   EVAL      DCTNCA = O12KeyNCA
214400040906     c     keydct        CHAIN     fndct01l
214500040906do  1c                   IF        %FOUND
214600000000     c*
214700000000     c* controlla validità CA
214800000000     c                   MOVEL     '0'           werr                           *NO errore
214900000000     c                   EXSR      chkdct
215000000000if  2c                   IF        werr = '0'                                   *CA valida
215100000000     c*
215200000000     c* Imposta l'evento lettera di anomalia
215300000000     c                   EXSR      evbdct
215400000000     C                   LEAVE                                                  *uscita ciclo
215500000000e   2c                   ENDIF
215600000000     c*
215700040906e   1c                   ENDIF
215800040906e   1c                   ENDDO
215900040906e   1c                   ENDIF
216000000000     C*
216100000000     c                   ENDSR
216200000000     c*--------------------------------------------------------------------------------------------*
216300000000     c* controlla validità lettera di anomalia
216400000000     c*--------------------------------------------------------------------------------------------*
216500000000     c     chkdct        BEGSR
216600000000     c*
216700000000     c* reimposta variabili di lavoro
216800000000     c                   MOVEL     '0'           werr                           *NO errore
216900000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
217000000000     c*
217100000000     c* CA annullata, esclude
217200000000if  1c                   IF        dctatb <> ' '
217300000000     c                   MOVEL     '1'           werr                           *SI errore
217400000000e   1C                   ENDIF
217500000000     c*
217600000000     c* CA non confermata, esclude
217700000000if  1c                   IF        dctfpr = ' '
217800000000     c                   MOVEL     '1'           werr                           *SI errore
217900000000e   1C                   ENDIF
218000000000     c*
218100000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
218200000000if  1c                   IF        dctfpr = 'T' AND
218300000000     c                             dctfca >= 215 OR
218400000000     c                             dctfpr = 'P' AND
218500000000     c                             dctfca >= 400
218600000000x   1c                   ELSE
218700000000     c                   MOVEL     '1'           werr                           *SI errore
218800000000e   1C                   ENDIF
218900000000     c*
219000000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
219100000000if  1c                   IF        werr = '0'
219200000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
219300060622     C                   RESET                   tibs02ds
219400060622     C                   EVAL      t02Cod = 'CCH'
219500060622     C                   EVAL      t02Ke1 = dctcch
219600060622     C                   EXSR      getTntbe00f
219700060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
219800000000     c                   MOVEL     '1'           werr                           *SI errore
219900000000e   3C                   ENDIF
220000000000e   2C                   ENDIF
220100000000e   1C                   ENDIF
220200000000     c*
220300000000     c                   ENDSR
220400000000     c*--------------------------------------------------------------------------------------------*
220500000000     c* Imposta l'evento lettera di anomalia
220600000000     c*--------------------------------------------------------------------------------------------*
220700000000     c     evbdct        BEGSR
220800000000     C*
220900000000     c* imposta causale
221000000000if  1c                   IF        wfpr = 'T'
221100000000     c                   MOVEL     '709'         wcev                           *causale evento
221200000000x   1c                   ELSE
221300000000     c                   MOVEL     '710'         wcev                           *causale evento
221400000000e   1c                   ENDIF
221500000000     c                   EXSR      deccevsta                                    *decodifica evento
221600000000     c*
221700000000     c* imposta filiale
221800000000if  1c                   IF        werr <> '1'                                  *no errore
221900000000     c                   Z-ADD     dctfil        worg
222000000000     c                   EXSR      decorg
222100000000     c* imposta data
222200000000     c                   MOVEL     dctaac        wdat
222300000000     c                   MOVE      dctmgc        wdat
222400000000     c                   EXSR      edtdat
222500000000     c* imposta ora
222600000000     c                   MOVEL     1100          wora                           *FISSA
222700000000     c                   EXSR      edtora
222800000000     c*
222900000000     c* incrementa contatore eventi
223000000000     c                   ADD       1             tote
223100000000     c* indice schiera
223200000000     c                   Z-ADD     tote          smi(tote)
223300000000     c* sequenza legame
223400000000     c                   Z-ADD     seql          smseql(tote)
223500000000     c* sequenza evento
223600000000     c                   Z-ADD     999           smseqe(tote)
223700000000     c* descrizione -impostata sopra-
223800030130     c                   MOVEL     wcdex         smdev(tote)
223900000000     c* filiale -impostata sopra-
224000000000     c                   MOVEL     wodes         smfle(tote)
224100000000     c* data    -impostata sopra-
224200000000     c                   MOVEL     wdate         smdeve(tote)
224300000000     c* ora     -impostata sopra-
224400000000     c                   MOVEL     worae         smheve(tote)
224500000000     c* causale -impostata sopra-
224600000000     c                   MOVEL     wcev          smcev(tote)
224700000000     c* data+ora evento
224800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
224900000000     c                   MOVE      wora          smdevhev(tote)
225000000000e   1c                   ENDIF
225100000000     c*
225200000000     c                   ENDSR
225300000000     c*--------------------------------------------------------------------------------------------*
225400000000     c* imposta i campi della DS di Output dal contrassegno
225500000000     c*--------------------------------------------------------------------------------------------*
225600000000     c     impdsocsb     BEGSR
225700000000     c*
225800000000     c* azzera le variabili di lavoro
225900000000     c*
226000000000     c****
226100000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
226200000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
226300000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
226400000000     c****
226500000000     c*
226600000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
226700090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
226800000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
226900000000x   1c                   ELSE                                                     *trovato
227000000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
227100000000e   1C                   ENDIF
227200000000     c*---
227300000000     c* memorizza il contrassegno della bolla di Input
227400000000     c*---
227500000000     c                   EVAL      waas = s_tasaas                              *bolla Input
227600000000     c                   EVAL      wlnp = s_taslnp
227700000000     c                   EVAL      wnrs = s_tasnrs
227800000000     c                   EVAL      wnsp = s_tasnsp
227900000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
228000000000     c                   EXSR      memcsb
228100000000     c*
228200000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
228300000000     c                   EXSR      elacsblbl
228400000000     c*
228500000000     c                   ENDSR
228600000000     c*--------------------------------------------------------------------------------------------*
228700000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
228800000000     c*--------------------------------------------------------------------------------------------*
228900000000     c     elacsblbl     BEGSR
229000000000     c*
229100000000     c* azzera le variabili di lavoro
229200000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
229300000000     c                   EVAL      deplpn = *zeros
229400000000     c                   EVAL      depnrn = *zeros
229500000000     c                   EVAL      depnsn = *zeros
229600000000     c*
229700000000     c* legge i legami delle figlie successive alla bolla di input
229800000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
229900000000     c                   EVAL      kbllpp =  s_taslnp
230000000000     c                   EVAL      kblnrp =  s_tasnrs
230100000000     c                   EVAL      kblnsp =  s_tasnsp
230200000000     c*
230300000000     c* ciclo fino a fine mamme
230400000000     c     ke2lbl        SETLL     fnlbl02l
230500000000     c     ke2lbl        READE     fnlbl02l                               99
230600000000do  1c                   DOW       NOT *in99
230700000000     c*
230800000000     c* ciclo fino a fine figlie della mamma
230900000000do  2c                   DOW       NOT *in99
231000000000     c*
231100000000     c* legge la bolla dal legame
231200000000     c                   EVAL      kasaas = lblaan                              *figlia
231300000000     c                   EVAL      kaslnp = lbllpn
231400000000     c                   EVAL      kasnrs = lblnrn
231500000000     c                   EVAL      kasnsp = lblnsn
231600000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
231700000000     c*---
231800000000     c* memorizza il contrassegno della bolla figlia legata
231900000000     c*---
232000000000if  4c                   IF        NOT *in99                                    *esistente
232100000000     c                   EVAL      waas = lblaan                                *figlia
232200000000     c                   EVAL      wlnp = lbllpn
232300000000     c                   EVAL      wnrs = lblnrn
232400000000     c                   EVAL      wnsp = lblnsn
232500000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
232600000000     c                   EXSR      memcsb
232700000000e   4c                   ENDIF
232800000000     c*
232900000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
233000000000     c                   EVAL      depaan = lblaan                              *ultima figlia
233100000000     c                   EVAL      deplpn = lbllpn
233200000000     c                   EVAL      depnrn = lblnrn
233300000000     c                   EVAL      depnsn = lblnsn
233400000000     c*
233500000000     c* lettura successiva legami
233600000000     c     ke2lbl        READE     fnlbl02l                               99
233700000000e   2c                   ENDDO                                                  *fine figlie mamma
233800000000     c*
233900000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
234000000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
234100000000     c                             kbllpp <> deplpn OR
234200000000     c                             kblnrp <> depnrn OR
234300000000     c                             kblnsp <> depnsn
234400000000     c* nuova chiave
234500000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
234600000000     c                   EVAL      kbllpp =  deplpn
234700000000     c                   EVAL      kblnrp =  depnrn
234800000000     c                   EVAL      kblnsp =  depnsn
234900000000     c* controlla figlie
235000000000     c     ke2lbl        SETLL     fnlbl02l
235100000000     c     ke2lbl        READE     fnlbl02l                               99
235200000000if  3c                   IF        *in99                                        *non ha figlie
235300000000     c                   LEAVE                                                  *esce dal ciclo
235400000000e   3c                   ENDIF
235500000000x   2c                   ELSE                                                   *sempre stessa mamma
235600000000     c                   LEAVE                                                  *esce dal ciclo
235700000000e   2c                   ENDIF
235800000000e   1c                   ENDDO                                                  *fine mamme
235900000000     c*
236000000000     c                   ENDSR
236100000000     c*--------------------------------------------------------------------------------------------*
236200000000     c* memorizza il contrassegno
236300000000     c*--------------------------------------------------------------------------------------------*
236400000000     c     memcsb        BEGSR
236500000000     c*
236600000000     c* legge il contrassegno, se avuto
236700000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
236800000000     c                   Z-ADD     waas          ksbaas
236900000000     c                   Z-ADD     wlnp          ksblnp
237000000000     c                   Z-ADD     wnrs          ksbnrs
237100000000     c                   Z-ADD     wnsp          ksbnsp
237200000000     c     keycsb        CHAIN     tncsb03l                           99
237300000000if  2c                   IF        NOT *in99                                    *esistente
237400000000     c* memorizza i dati
237500000000     c                   MOVEL     'S'           cao74                          *esiste C/A
237600000000     c*
237700000000     c                   Z-ADD     csbcas        caimpo74                       *importo
237800000000     c*
237900000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
238000010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
238100010820     c                             caimpo74 > *zeros
238200010820     c                   EVAL      cadivo74 = 'ITL'
238300010820e   3c                   ENDIF
238400160314
238500160314     ** Visualizzo la descrizione del tipo incasso
238600100121     C                   EVAL      cainco74 =
238700100121     C                                       GetDescrizioneIncassoContrassegno()
238800000000     c*
238900000000     c                   MOVEL     *blanks       castao74                       *stato
239000060622     C                   EVALR     tblKey = %CHAR(csbSta)
239100060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
239200060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
239300060622     C                             :rpyOpCode:rpyEsito)
239400060622     C                   IF        rpyOpCode = 'FOUND'
239500060622     c                   EVAL      castao74 = §4SDei
239600000000e   3c                   ENDIF
239700100119     c
239800000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
239900011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
240000060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
240100000000x   4c                   ELSE
240200000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
240300060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
240400000000x   5c                   ELSE
240500060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
240600000000e   5c                   ENDIF
240700000000e   4c                   ENDIF
240800000000e   3c                   ENDIF
240900000000     c*
241000000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
241100000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
241200000000if  4c                   IF        csbtpa = 'B'
241300060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
241400000000e   4c                   ENDIF
241500000000if  4c                   IF        csbtpa = 'C'
241600060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
241700000000e   4c                   ENDIF
241800000000x   3c                   ELSE                                                   *incasso Bartolini
241900000000if  4c                   IF        csbfpc = *blanks
242000110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
242100000000e   4c                   ENDIF
242200000000if  4c                   IF        csbfpc = '2'
242300060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
242400000000e   4c                   ENDIF
242500000000e   3c                   ENDIF
242600000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
242700000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
242800000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
242900000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
243000000000     c                             csbfpc = *blanks                              assegno di traenza
243100060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
243200000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
243300060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
243400000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
243500060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
243600000000e   4c                   ENDIF
243700000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
243800060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
243900060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
244000000000e   3c                   ENDIF
244100000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
244200000000     c                   Z-ADD     *zeros        caabio74                       *abi
244300000000     c                   Z-ADD     *zeros        cacabo74                       *cab
244400000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
244500000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
244600000000     c                   MOVEL     csbddp        wdat
244700000000     c                   EXSR      edtdat
244800000000     c                   EVAL      cadato74 = wdate
244900000000     C*
245000000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
245100000000     c                   Z-ADD     csbabi        caabio74
245200000000     c                   Z-ADD     csbcab        cacabo74
245300000000     c                   MOVEL     csbnra        canumo74
245400120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
245500120321     c                             : caCabO74)
245600000000x   4c                   ELSE                                                   *incasso Bartolini
245700000000     c                   MOVEL     csbndp        canumo74
245800000000e   4c                   ENDIF
245900000000e   3c                   ENDIF
246000110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
246100110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
246200110307     C                   IF        IsContrassegnoCompensato()
246300110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
246400110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
246500110304     C                   CLEAR                   caabio74
246600110304     C                   CLEAR                   cacabo74
246700110304     C                   CLEAR                   canumo74
246800110304     C                   ENDIF
246900110304     C*
247000000000e   2c                   ENDIF
247100000000e   1c                   ENDIF
247200000000     c*
247300000000     c                   ENDSR
247400000000     c*--------------------------------------------------------------------------------------------*
247500000000     c* imposta i campi della DS di Output dalla giacenza
247600000000     c*--------------------------------------------------------------------------------------------*
247700000000     c     impdsogcp     BEGSR
247800000000     c*
247900000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
248000000000     c                   EVAL      waas = s_tasaas                              *bolla Input
248100000000     c                   EVAL      wlnp = s_taslnp
248200000000     c                   EVAL      wnrs = s_tasnrs
248300000000     c                   EVAL      wnsp = s_tasnsp
248400000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
248500000000     c                   EXSR      memgcp
248600000000     c*
248700000000     c                   ENDSR
248800000000     c*--------------------------------------------------------------------------------------------*
248900000000     c* memorizza la giacenza
249000000000     c*--------------------------------------------------------------------------------------------*
249100000000     c     memgcp        BEGSR
249200000000     c*
249300000000     c* imposta le variabili di lavoro
249400000000     c                   CLEAR                   i                              *indice
249500000000     c                   CLEAR                   ii
249600000000     c                   CLEAR                   smdmc                          *note fase -20-
249700000000     c                   CLEAR                   smdmcR
249800000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
249900000000     c                   MOVEL     'N'           gco74                          *flag di controllo
250000000000     c                   MOVEL     'N'           gcfdio74
250100000000     c                   MOVEL     'N'           gcmito74
250200000000     c*
250300000000     c* legge la giacenza, se avuta
250400000000     c                   Z-ADD     waas          kcpaas
250500000000     c                   Z-ADD     wlnp          kcplnp
250600000000     c                   Z-ADD     wnrs          kcpnrs
250700000000     c                   Z-ADD     wnsp          kcpnsp
250800000000     c                   Z-ADD     *zeros        kcpfrg
250900050309     c     keygcp        CHAIN     tigcp51l                           99
251000050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
251100050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
251200000000if  2c                   IF        NOT *in99        AND                         *esistente
251300000000     c                             gcpatb = *blanks AND                         *no annullata
251400050415     c                             gcptcm <> 'Z'    AND
251500050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
251600050419     C
251700050419     C                   EVAL      gcRiapO74 = gcpRiap
251800050418     **?Descrizione della fase.
251900050418     C                   SELECT
252000050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
252100060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
252200050418     C                   WHEN      gcpFas = 20
252300060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
252400050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
252500060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
252600050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
252700060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
252800050418     C                   ENDSL
252900000000     c* memorizza i dati dalle NOTE GIACENZE
253000000000     c                   Z-ADD     *zeros        i
253100000000     c                   Z-ADD     *zeros        ii
253200000000     c                   Z-ADD     gcpagc        knpagc
253300000000     c                   Z-ADD     gcpfgc        knpfgc
253400000000     c                   Z-ADD     gcpngc        knpngc
253500000000     c                   Z-ADD     gcpfrg        knpfrg
253600050309     c     keygnp        SETLL     tignp01l
253700050309     c     keygnp        READE     tignp01l                               98
253800000000do  3c                   DOW       NOT *in98
253900050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
254000000000     c* fase -10-
254100000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
254200000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
254300000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
254400000000e   5c                   ENDIF
254500000000     c* fase -20-
254600000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
254700000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
254800051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
254900000000     c                   ADD       1             i
255000000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
255100000000e   6c                   ENDIF
255200000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
255300051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
255400050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
255500050405     c                   CLEAR                   ii
255600050405     c                   CLEAR                   smdmcR
255700050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
255800050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
255900050919     C                   IF        NOT %ERROR
256000140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
256100050919     C                   ENDIF
256200050405     C                   ELSE
256300000000     c                   ADD       1             ii
256400000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
256500050405     C                   ENDIF
256600000000e   6c                   ENDIF
256700000000e   5c                   ENDIF
256800000000e   4c                   ENDIF
256900050309     c     keygnp        READE     tignp01l                               98
257000000000e   3c                   ENDDO
257100000000     c*
257200000000     c* memorizza i dati dalle GIACENZE
257300000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
257400050404     c* Disposizione da immettere.
257500051110     c                   IF        gcpfas = 20
257600051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
257700051110     C                   EXSR      chkTivpi
257800051110e   3c                   ENDIF
257900000000     c*
258000000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
258100000000     c*
258200000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
258300000000     c*
258400060103     c                   Z-ADD     gcpagc        dsannB
258500060103     c                   Z-ADD     gcpmgc        dsmgsB
258600060103     c                   Z-ADD     dsdatB        wdat
258700000000if  3c                   IF        wdat > *zeros
258800000000     c                   EXSR      edtdat
258900000000     c                   EVAL      gcdago74 = wdate                             *data apertura
259000000000e   3C                   ENDIF
259100060104     c*
259200060104     c                   Z-ADD     gcpdur        wdat
259300060104     c                   IF        wdat > *zeros
259400060104     c                   EXSR      edtdat
259500060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
259600060104     C                   ENDIF
259700000000     c*
259800000000if  3c                   IF        gcpcmc <> *blanks
259900000000     c                   MOVEL     gcpcmc        wcev
260000000000     c                   EXSR      deccevgia
260100030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
260200000000e   3C                   ENDIF
260300000000     c*
260400000000     c                   MOVEL     *blanks       gcdcgo74
260500000000if  3c                   IF        gcpdcg > *zeros
260600000000     c                   Z-ADD     gcpdcg        wdat
260700000000     c                   EXSR      edtdat
260800000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
260900000000e   3c                   ENDIF
261000000000     c*---
261100000000     c* Disposizioni già immesse
261200000000     c*---
261300000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
261400000000     c*
261500000000if  4c                   IF        gcpddm > *zeros
261600000000     c                   Z-ADD     gcpddm        wdat
261700000000     c                   EXSR      edtdat
261800000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
261900000000e   4C                   ENDIF
262000000000     c*
262100000000if  4c                   IF        gcpdis <> *blanks
262200060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
262300060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
262400060626     C                             :rpyOpCode:rpyEsito)
262500060622     C                   EVAL      gcdiso74 = §2DDes
262600000000e   4C                   ENDIF
262700000000     c*
262800000000if  4c                   IF        gcpasg <> *blanks
262900000000if  5c                   IF        gcpasg = 'S'
263000060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
263100000000x   5c                   ELSE
263200060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
263300000000e   5C                   ENDIF
263400000000     c*
263500000000if  5c                   IF        gcppsg = 'M'
263600060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
263700000000e   5C                   ENDIF
263800000000if  5c                   IF        gcppsg = 'D'
263900060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
264000000000e   5C                   ENDIF
264100000000e   4C                   ENDIF
264200000000     c*
264300000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
264400000000     c                   MOVEL     gcpvcs        gcvcso74
264500000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
264600000000     c                   Z-ADD     gcpcas        gccaso74
264700000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
264800000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
264900000000e   5C                   ENDIF
265000000000e   4C                   ENDIF
265100050404     C                   IF        smdmc(1) <> *BLANK
265200050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
265300050404     C                             smdmc(3) + smdmc(4)
265400051014     C                   EVAL      gcdmc5O74 = smdmc(5)
265500050404     C                   ELSE
265600050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
265700050404     C                             smdmcr(3) + smdmcr(4)
265800051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
265900050404e   3C                   ENDIF
266000000000e   3C                   ENDIF
266100000000     c*---
266200000000     c* Disposizioni da immettere
266300000000     c*---
266400010719     c* se le disposizioni sono da immettere controlla che:
266500010719     c* - il cliente è il mittente della giacenza
266600000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
266700040726if  4C                   IF        keygiac <> *blanks
266800040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
266900040726x   4c                   ELSE
267000090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
267100090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
267200090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
267300090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
267400090914     C                             )
267500040726e   4C                   ENDIF
267600000000e   3C                   ENDIF
267700050404     C* Controllo esistenza storia giacenza.
267800050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
267900050404     C                   DO        *HIVAL
268000050404     C     K04GCP51      READE     TIGCP51L
268100050404     C                   IF        %EOF
268200050404     C                   LEAVE
268300050404     C                   ENDIF
268400050404     C                   IF        GCPATB = *BLANK
268500050404     C                   EVAL      StoGiaO74 = 'S'
268600050404     C                   LEAVE
268700050404     C                   ENDIF
268800050404     C                   ENDDO
268900050404     c*
269000000000e   2c                   ENDIF
269100000000     c*
269200000000     c                   ENDSR
269300000000     c*--------------------------------------------------------------------------------------------*
269400000000     c* imposta i campi della DS di Output dalle note
269500000000     c*--------------------------------------------------------------------------------------------*
269600000000     c     impdsonot     BEGSR
269700000000     c*
269800000000     c* azzera le variabili di lavoro
269900000000     c                   CLEAR                   i
270000000000     c                   CLEAR                   mdcre                          consegna richiesta
270100000000     c                   CLEAR                   mhcre
270200030210     c                   CLEAR                   mtcre
270300000000     c                   CLEAR                   mftce                          consegna particolare
270400000000     c                   CLEAR                   wftc
270500000000     c                   CLEAR                   d1wdftc
270600000000     c                   CLEAR                   d2wdftc
270700000000     c                   CLEAR                   mgcxe                          turni chiusura
270800000000     c                   CLEAR                   wgcx
270900000000     c                   CLEAR                   d1wdgcx
271000000000     c                   CLEAR                   d2wdgcx
271100010927     c                   CLEAR                   mffd                           fermo deposito
271200010927     c                   CLEAR                   mmncB                          *motivi NON consegna
271300010927     c                   CLEAR                   mmncC
271400010927     c                   CLEAR                   mmncG
271500010927     c                   CLEAR                   mmncH
271600010927     c                   CLEAR                   mmncN
271700010927     c                   CLEAR                   mmncO
271800050701     c                   CLEAR                   mmnc8
271900050701     c                   CLEAR                   mmnc9
272000050701     c                   CLEAR                   a230
272100000000     c                   CLEAR                   we
272200030206     c                   CLEAR                   mtel                           n° tel destinatario
272300030206     c                   CLEAR                   mban                           bancali
272400030207     c                   CLEAR                   mscr                           supermercati
272500030210     c                   CLEAR                   msdace
272600030210     c                   CLEAR                   msorce
272700030210     c                   CLEAR                   msdcre
272800030210     c                   CLEAR                   mshcre
272900030210     c                   CLEAR                   mstcre
273000030210     c                   CLEAR                   msnom
273100030210     c                   CLEAR                   mscrT
273200030210     c                   CLEAR                   wmscrT
273300030210     c                   CLEAR                   s
273400030207     c
273500160127     c*imposta dati data consegna richiesta  DA BLP se bolla in partenza
273600160127     c                   if        esito='P'
273700160127     c                   eval      f_tasdcr=arbdcr
273800160127     c                   eval      f_tastcr=arbtcr
273900160127     c                   eval      f_tashcr=arbhcr
274000160127     c* imposto anche key spedizione
274100160309     c                   eval      f_tasaas=arbaas
274200160309     c                   eval      f_taslnp=arblnp
274300160309     c                   eval      f_tasnrs=arbnrs
274400160309     c                   eval      f_tasnsp=arbnsp
274500160127     c                   endif
274600160127     c
274700140716     c                   if        f_tasdcr > 0
274800140716     c                   MOVEL     f_tasdcr      wdat
274900140619     c                   EXSR      edtdat
275000140619     c                   eval      DTCONRCO74  =  wdate
275100140819if  3c                   IF        f_tastcr = ' '
275200140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
275300140819e   3c                   ENDIF
275400140819     c                   endif
275500140716     c                   if        f_tashcr > 0
275600140716     c                   MOVEL     f_tashcr      wora
275700140619     c                   EXSR      edtora
275800140619     c                   eval      ORACONRO74  =  worae
275900140619     c                   endif
276000140716     c                   eval      TPDATCRO74  =  f_tastcr
276100140619      *reperimento descrizione tipo consegna richiesta
276200140716if  3c                   IF        f_tastcr = 'P'
276300140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
276400140619e   3c                   ENDIF
276500140716if  3c                   IF        f_tastcr = 'D'
276600140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
276700140619e   3c                   ENDIF
276800000000     c*---
276900000000     c* memorizza le "note" che sono presenti nel record bolla
277000000000     c*---
277100000000     c* Consegna richiesta
277200140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
277300140619     c*m                           s_tashcr = *zeros
277400140619x   1c*m                 ELSE                                                   *SI cons.particolare
277500140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
277600140619     c*m                 MOVEL     s_tasdcr      wdat
277700140619     c*m                 EXSR      edtdat                                       *edita data
277800140619     c*m                 MOVEL     wdate         mdcre
277900140619e   2c*m                 ENDIF
278000140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
278100140619     c*m                 MOVEL     s_tashcr      wora
278200140619     c*m                 EXSR      edtora                                       *edita ora
278300140619     c*m                 MOVEL     worae         mhcre
278400140619e   2c*m                 ENDIF
278500140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
278600140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
278700140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
278800140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
278900140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
279000140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
279100140619e   3c*m                 ENDIF
279200140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
279300140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
279400140619e   3c*m                 ENDIF
279500140619if  3c*m                 IF        s_tastcr = ' '                               *il
279600140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
279700140619e   3c*m                 ENDIF
279800140619e   2c*m                 ENDIF
279900140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
280000140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
280100140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
280200140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
280300140619e   3c*m                 ENDIF
280400140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
280500140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
280600140619e   3c*m                 ENDIF
280700140619if  3c*m                 IF        s_tastcr = ' '                               *il
280800140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
280900140619e   3c*m                 ENDIF
281000140619e   2c*m                 ENDIF
281100140619     c*m
281200140619if  2c*m                 IF        i < 10                                       *se c'è spazio
281300140619     c*m                 ADD       1             i                              *memorizza nota
281400140619     c*m                 EVAL      smnot(i) =
281500140619     c*m                           %TRIM(mtcre) +
281600140619     c*m                           ' '          +
281700140619     c*m                           %TRIM(mdcre) +
281800140619     c*m                           ' '          +
281900140619     c*m                           %TRIM(mhcre)
282000140619e   2c*m                 ENDIF
282100140619e   1c*m                 ENDIF
282200000000     c*
282300000000     c* Particolarità consegna -uno/due-
282400160127     c                   if        esito='P'
282500160309     c                   eval      f_tasftc = arbtc1
282600160314     c                   eval      f_tastc2 = arbtc2
282700160127     c                   endif
282800160127     c
282900140716if  1c                   IF        f_tasftc <> *blanks OR
283000140716     c                             f_tastc2 <> *blanks
283100140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
283200000000     c                   EXSR      decftc
283300000000     c                   MOVEL     wdftc         d1wdftc
283400140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
283500000000     c                   EXSR      decftc
283600000000     c                   MOVEL     wdftc         d2wdftc
283700000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
283800000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
283900060612     c                   EVAL      we = ' ' +
284000060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
284100000000x   2c                   ELSE
284200000000     c                   EVAL      we = *blanks
284300000000e   2c                   ENDIF
284400060612     c                   EVAL      mftce =
284500060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
284600060612     c                             %TRIM(d1wdftc)             +
284700060612     c                             we                         +
284800060612     c                             %TRIM(d2wdftc)
284900000000     c*
285000010927if  2c                   IF        i < 10                                       *se c'è spazio
285100000000     c                   ADD       1             i                              *memorizza nota
285200000000     c                   EVAL      smnot(i) = mftce
285300010927e   2c                   ENDIF
285400010927e   1c                   ENDIF
285500000000     c*
285600000000     c* Chiusura per turno
285700160127     c                   if        esito='P'
285800160309     c                   eval      f_tasgc1 = arbgc1
285900160309     c                   eval      f_tasgc1 = arbgc2
286000160127     c                   endif
286100160127     c
286200140716if  1c                   IF        f_tasgc1 <> *blanks OR
286300140716     c                             f_tasgc2 <> *blanks
286400140716if  2c                   IF        f_tasgc1 <> *blanks
286500140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
286600000000     c                   EXSR      decgcx
286700000000     c                   MOVEL     wdgcx         d1wdgcx
286800000000e   2c                   ENDIF
286900140716if  2c                   IF        f_tasgc2 <> *blanks
287000140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
287100000000     c                   EXSR      decgcx
287200000000     c                   MOVEL     wdgcx         d2wdgcx
287300000000e   2c                   ENDIF
287400000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
287500000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
287600060612     c                   EVAL      we = ' ' +
287700060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
287800000000x   2c                   ELSE
287900000000     c                   EVAL      we = *blanks
288000000000e   2c                   ENDIF
288100060612     c                   EVAL      mgcxe =
288200060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
288300060612     c                             %TRIM(d1wdgcx)       +
288400060612     c                             we                   +
288500060612     c                             %TRIM(d2wdgcx)
288600000000     c*
288700010927if  2c                   IF        i < 10                                       *se c'è spazio
288800000000     c                   ADD       1             i                              *memorizza nota
288900000000     c                   EVAL      smnot(i) = mgcxe
289000010927e   2c                   ENDIF
289100010927e   1c                   ENDIF
289200000000     c*
289300000000     c* Fermo deposito
289400160127     c                   if        esito='P'
289500160309     c                   eval      f_tasffd = arbffd
289600160127     c                   endif
289700160127
289800140716if  1c                   IF        f_tasffd = 'S'
289900060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
290000000000     c*
290100010927if  2c                   IF        i < 10                                       *se c'è spazio
290200010927     c                   ADD       1             i                              *memorizza nota
290300010927     c                   EVAL      smnot(i) = mffd
290400010927e   2c                   ENDIF
290500010927e   1c                   ENDIF
290600030206     c*---
290700030206     c* memorizza le "note" che NON sono presenti nel record bolla
290800030206     c* ma sul file estensione "anagrafiche"
290900030206     c*---
291000030206     c* N° telefono destinatario
291100160127     c                   if        esito='P'
291200160309     C                   EVAL      MitOrigO74 = arbrmo
291300160127     c                   else
291400140716     c                   Z-ADD     f_tasaas      kaaaas
291500140716     c                   Z-ADD     f_taslnp      kaalnp
291600140716     c                   Z-ADD     f_tasnrs      kaanrs
291700140716     c                   Z-ADD     f_tasnsp      kaansp
291800030206     c                   MOVEL     'O'           kaatrc
291900030206     c     keytaa        CHAIN     titaa30c                           98
292000030206if  1c                   IF        NOT *in98
292100040330     C                   EVAL      MitOrigO74 = TAARSC
292200030206e   1c                   ENDIF
292300160127e   1c                   ENDIF
292400030206     c*---
292500030206     c* memorizza le "note" che NON sono presenti nel record bolla
292600030206     c* ma sul file estensione testata boolle
292700030206     c*---
292800030206     c* Bancali
292900140716     c                   Z-ADD     f_tasaas      kr5aas
293000140716     c                   Z-ADD     f_taslnp      kr5lnp
293100140716     c                   Z-ADD     f_tasnrs      kr5nrs
293200140716     c                   Z-ADD     f_tasnsp      kr5nsp
293300030206     c                   MOVEL     'BAN'         kr5trd
293400160127     c                   if        esito='P'
293500160127     c     keyar5        CHAIN     fiar501l                           98
293600160127     c                   else
293700040531     c     keyar5        CHAIN     fiar531c                           98
293800160127     c                   endif
293900160127     c
294000030206if  1c                   IF        NOT *in98
294100030206     c                   MOVEL     ar5uni        dar5ban
294200060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
294300060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
294400030206if  2c                   IF        §ar5rb1 > *zeros OR
294500030206     c                             §ar5rb2 > *zeros
294600060613     c                   EVAL      mban = %trim(mban) +
294700060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
294800060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
294900030206e   2c                   ENDIF
295000030206     c*
295100030206if  2c                   IF        i < 10                                       *se c'è spazio
295200030206     c                   ADD       1             i                              *memorizza nota
295300030206     c                   EVAL      smnot(i) = mban
295400030206e   2c                   ENDIF
295500030206e   1c                   ENDIF
295600040330
295700040330     **?Reperisco referente
295800030206     c* Note supermercati
295900140716if  1c     f_tasftc      IFEQ      'S'
296000140716     c     f_tastc2      OREQ      'S'
296100030210     c                   CLEAR                   s                              *indice
296200140716     c                   Z-ADD     f_tasaas      kr5aas
296300140716     c                   Z-ADD     f_taslnp      kr5lnp
296400140716     c                   Z-ADD     f_tasnrs      kr5nrs
296500140716     c                   Z-ADD     f_tasnsp      kr5nsp
296600030206     c                   MOVEL     'SCR'         kr5trd
296700160127     c                   if        esito='P'
296800160127     c     keyar5        SETLL     fiar501l
296900160127     c     keyar5        READE     fiar501l                               98
297000160127     c                   else
297100040531     c     keyar5        SETLL     fiar531c
297200040531     c     keyar5        READE     fiar531c                               98
297300160127     c                   endif
297400160127     c
297500030210do  2c                   DOW       NOT *in98
297600030206     c                   MOVEL     ar5uni        dar5scr
297700030207     c* data nota
297800030210if  3c                   IF        ar5dac > *zeros
297900030207     c                   MOVEL     ar5dac        wdat
298000030207     c                   EXSR      edtdat                                       *edita data
298100030210     c                   MOVEL     wdate         msdace
298200030210e   3c                   ENDIF
298300030207     c* ora nota
298400030210if  3c                   IF        ar5orc > *zeros
298500030207     c                   MOVEL     ar5orc        wora
298600030207     c                   EXSR      edtora                                       *edita ora
298700030210     c                   MOVEL     worae         msorce
298800030210e   3c                   ENDIF
298900030207     c* Consegna richiesta
299000140708if  3c                   IF        §ar5dcr = *zeros AND
299100140708     c                             §ar5hcr = *zeros
299200140708x   3c                   ELSE
299300140708if  4c                   IF        §ar5dcr > *zeros
299400140708     c                   MOVEL     §ar5dcr       wdat
299500140708     c                   EXSR      edtdat                                       *edita data
299600140708     c                   MOVEL     wdate         msdcre
299700140708e   4c                   ENDIF
299800140708if  4c                   IF        §ar5hcr > *zeros
299900140708     c                   MOVEL     §ar5hcr       wora
300000140708     c                   EXSR      edtora                                       *edita ora
300100140708     c                   MOVEL     worae         mshcre
300200140708e   4c                   ENDIF
300300140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
300400140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
300500140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
300600140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
300700140708if  5c                   IF        §ar5tcr = 'P'                                *prima
300800140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
300900140708e   5c                   ENDIF
301000140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
301100140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
301200140708e   5c                   ENDIF
301300140708if  5c                   IF        §ar5tcr = ' '                                *il
301400140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
301500140708e   5c                   ENDIF
301600140708e   4c                   ENDIF
301700140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
301800140708     c                             §ar5dcr = *zeros                             *NO data richiesta
301900140708if  5c                   IF        §ar5tcr = 'P'                                *prima
302000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
302100140708e   5c                   ENDIF
302200140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
302300140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
302400140708e   5c                   ENDIF
302500140708if  5c                   IF        §ar5tcr = ' '                                *il
302600140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
302700140708e   5c                   ENDIF
302800140708e   4c                   ENDIF
302900030210e   3c                   ENDIF
303000030210     c* Riferimento
303100030210if  3c                   IF        §ar5nom <>  *blanks
303200060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
303300060613     c                             + ' ' + §ar5nom
303400030210e   3c                   ENDIF
303500030210     c*
303600030210if  3c                   IF        s < 5                                        *MAX 5 note
303700030210     c                   ADD       1             s
303800030210     c                   EVAL      mscrT(s) = 'Il ' +
303900030210     c                             %trim(msdace) +                              *data nota
304000030210     c                             ' '    +
304100030210     c                             %trim(msorce) +                              *ora nota
304200030210     c                             ' '    +
304300140708     c                             %trim(mstcre) +                              *tipo consegna
304400140708     c                             ' '    +
304500140708     c                             %trim(msdcre) +                              *data consegna
304600140708     c                             ' '    +
304700140708     c                             %trim(mshcre) +                              *ora consegna
304800140708     c                             ' '    +
304900030210     c                             %trim(msnom)  +                              *parlato con
305000030210     c                             ' '    +
305100030210     c                             %trim(§ar5tel) +                             *telefono
305200030210     c                             ' '    +
305300030210     c                             %trim(§ar5mot)                               *motivo
305400030210e   2c                   ENDIF
305500030210     c*
305600160127     c                   if        esito='P'
305700160127     c     keyar5        READE     fiar501l                               98
305800160127     c                   else
305900160127     c     keyar5        READE     fiar531c                               98
306000160127e   1c                   ENDIF
306100030206e   2c                   ENDDO
306200030210e   1c                   ENDIF
306300030210     c* NOTA completa
306400030210if  1c                   IF        mscrT(1) <> *blanks
306500030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
306600030211     c                                      ' ' +
306700030211     C                                      %trim(mscrT(2)) +
306800030211     c                                      ' ' +
306900030211     C                                      %trim(mscrT(3)) +
307000030211     c                                      ' ' +
307100030211     C                                      %trim(mscrT(4)) +
307200030211     c                                      ' ' +
307300030211     C                                      %trim(mscrT(5))
307400030210     c                   Z-ADD     *zeros        s
307500030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
307600030210     c                   ADD       1             i
307700030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
307800030210     c                   EVAL      s = s + 100
307900030210e   2c                   ENDDO
308000030210     c*
308100030210e   1c                   ENDIF
308200010927     c*---
308300010927     c* memorizza le "note" che NON sono presenti nel record bolla
308400160127     c* ma sul file estensione
308500010927     c*---
308600160127     c
308700160127    0c                   if        esito<>'P'
308800010927     c                   CLEAR                   tita4000
308900010927     c                   CLEAR                   tita4p00
309000140716     c                   Z-ADD     f_tasaas      ka4aas
309100140716     c                   Z-ADD     f_taslnp      ka4lnp
309200140716     c                   Z-ADD     f_tasnrs      ka4nrs
309300140716     c                   Z-ADD     f_tasnsp      ka4nsp
309400160127     c
309500010927     c     ke2ta4        SETLL     tita430c
309600010927     c     ke2ta4        READE     tita430c                               98
309700010927do  1c                   DOW       NOT *in98
309800140708     c                   if        ta4trc = '8' OR ta4trc = '9'
309900050701     c*
310000050701if  3c                   IF        ta4trc = '8'
310100050701     c                   EVAL      mmnc8 = ta4not
310200050701e   3c                   ENDIF
310300050701if  3c                   IF        ta4trc = '9'
310400050701     c                   EVAL      mmnc9 = ta4not
310500050701e   3c                   ENDIF
310600050701     c*
310700010927e   2c                   ENDIF
310800010925     c*
310900010926     c     ke2ta4        READE     tita430c                               98
311000010926e   1c                   ENDDO
311100050701     C*
311200140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
311300050701     C*
311400050701     C                   IF        mmnc8 <> *BLANK
311500050701     C                   EVAL      a230 = %TRIML(mmnc8)
311600050701     C                   ENDIF
311700050701     C                   IF        mmnc9 <> *BLANK
311800050701     C                   IF        a230 <> *BLANK
311900050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
312000050701     C                   ELSE
312100050701     C                   EVAL      a230 = %TRIML(mmnc9)
312200050701     C                   ENDIF
312300050701     C                   ENDIF
312400010927     C*
312500050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
312600050701     C*
312700010927if  2c                   IF        i < 10                                       *se c'è spazio
312800010927     c                   ADD       1             i                              *memorizza nota
312900010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
313000010927e   2c                   ENDIF
313100010927     C*
313200010927if  2c                   IF        i < 10                                       *se c'è spazio
313300010927if  3c                   IF        n3 > 100
313400010927     c                   ADD       1             i                              *memorizza nota
313500010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
313600010927e   3c                   ENDIF
313700010927e   2c                   ENDIF
313800010927     C*
313900010927if  2c                   IF        i < 10                                       *se c'è spazio
314000010927if  3c                   IF        n3 > 200
314100010927     c                   ADD       1             i                              *memorizza nota
314200010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
314300010927e   3c                   ENDIF
314400010927e   2C                   ENDIF
314500010927e   1c                   ENDIF
314600160127e   0c                   ENDIF
314700010925     c*
314800000000     c* imposta contatore note da visualizzare nella DS di Output
314900000000     c                   EVAL      cntnoto74 = i
315000000000     c*
315100000000     c* imposta le note nella DS di Output
315200000000do  1c     1             DO        10            j
315300000000     c                   EVAL      nota(j) = smnot(j)
315400000000e   1c                   ENDDO
315500000000     c*
315600000000     c                   ENDSR
315700010716     c*--------------------------------------------------------------------------------------------*
315800010716     c* imposta i campi della DS di Output dalla firma DPD
315900010716     c*--------------------------------------------------------------------------------------------*
316000010716     c     impdsoDPD     BEGSR
316100010717     c*
316200010717     c* reimposta variabili di lavoro
316300010717     c                   RESET                   $AbFirma
316400010717     c                   CLEAR                   depute                         *deposito utente
316500010717     c                   CLEAR                   deppwd                         *deposito password
316600010717     c*---
316700010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
316800010717     c*---
316900010717     c                   Z-ADD     s_taslna      worg
317000010717     c                   EXSR      decorg
317100160406     c
317200020702     c                   IF        wodpd = 'DPD'
317300020702     c                   MOVEL     'S'           dpdo74
317400020702     c                   ELSE
317500020702     c                   MOVEL     *blanks       dpdo74
317600160406     c
317700160406     c* se si tratta di bolla EEX verifico se il partner è abilitato al link
317800160407     c                   IF        wodpd = 'EEX' and ksci74<>*blanks
317900160406     c                   clear                   fidn40ds
318000160406     c                   eval      i40fil=s_taslna
318100160406     c                   z-add     s_tasmgs      i40dsp
318200160406     c                   MOVEL     s_tasaas      i40dsp                          *data spedizione
318300160406     c                   call      'FIDN40R'
318400160406     c                   parm                    fidn40ds
318500160407
318600160406     c                   if        o40lnk='S'
318700160407     c* sommo i giorni alla data spedizione per sapere se ancora visibile
318800160407     c                   Clear                   xgiolavds
318900160407     c                   Eval      Ixglpa  = 'A'
319000160413     c                   Eval      IxglFil = s_taslna
319100160407     c                   Eval      Ixgldata = i40dsp
319200160407     c                   Eval      Ixgladd = 'S'
319300160407     c                   eval      ixglgg  = o40gio
319400160407     c                   Call      'XGIOLAV'
319500160407     c                   parm                    xgiolavds
319600160407     c                   if        oxgldata>=datcor
319700160406     c                   MOVEL     'E'           dpdo74
319800160407     c                   endif
319900160407     c
320000160406     c                   endif
320100160406     c                   endif
320200160406     c
320300020702     c                   ENDIF
320400010717     c*
320500010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
320600060627     c* Riferimento link Lettera di vettura
320700060627     c* Riferimento link Firma
320800010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
320900010717     c                   CLEAR                   tita4000
321000010717     c                   CLEAR                   tita4p00
321100060627     c                   CLEAR                   dsbl4i
321200010717     c                   Z-ADD     s_tasaas      ka4aas
321300010717     c                   Z-ADD     s_taslnp      ka4lnp
321400010717     c                   Z-ADD     s_tasnrs      ka4nrs
321500010717     c                   Z-ADD     s_tasnsp      ka4nsp
321600060627     c                   MOVEL     'I'           ka4trc
321700060627     c     keyta4        CHAIN     tita430c                           98
321800010717if  2c                   IF        NOT *in98
321900060627     c                   MOVEL     ta4not        dsbl4i
322000060627     c                   EVAL      DPDRmnO74 = §b4ipn
322100060627     c                   EVAL      DPDFnpO74 = §b4ipn
322200010717e   2c                   ENDIF
322300010717     c*
322400010717     c* cerca abilitazione firma DPD per questo segnacollo
322500010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
322600010717     c                   EXSR      RepAbilFirma
322700010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
322800010717     c*---
322900010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
323000010717     c*---
323100010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
323200010717     c* dati
323300010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
323400010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
323500010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
323600010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
323700010717     c*
323800010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
323900010717     c                   Z-ADD     s_tasaas      kvbaas
324000010717     c                   Z-ADD     s_taslnp      kvblnp
324100010717     c                   Z-ADD     s_tasnrs      kvbnrs
324200010717     c                   Z-ADD     s_tasnsp      kvbnsp
324300010717     c                   Z-ADD     99999999      kvbdev
324400010717     c                   Z-ADD     9999          kvbhev
324500010717     c     ke2evb        SETGT     fnevb01l
324600010717     c     keyevb        READPE    fnevb01l                               98
324700010717do  4c                   DOW       NOT *in98
324800101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
324900010720     c*
325000060629     c                   EVAL      devB01 = evbNot
325100060628     c                   IF        §NotDep = *BLANK
325200060628     c                   RESET                   DPDFirO74
325300060629     c                   LEAVE
325400060628     c                   ENDIF
325500060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
325600060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
325700080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
325800010720     c                   Z-ADD     *zeros        g08dat
325900010720     c                   MOVEL     '3'           g08err
326000010720     c                   CALL      'XSRDA8'
326100010720     c                   PARM                    wlbda8
326200010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
326300010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
326400010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
326500010720     c*
326600010717     c                   LEAVE                                                  *uscita ciclo
326700010717e   5c                   ENDIF
326800010717     c     keyevb        READPE    fnevb01l                               98
326900010717e   4c                   ENDDO
327000010717     c*
327100010717e   3c                   ENDIF
327200010717e   2c                   ENDIF
327300160323     c
327400160323   x1c                   else
327500010716e   1c                   ENDIF
327600010716     c*
327700010716     c                   ENDSR
327800160309     c*--------------------------------------------------------------------------------------------*
327900160309     c* impostazioni solo da FNBLP
328000160309     c*--------------------------------------------------------------------------------------------*
328100160309     c     impds_soloBLP BEGSR
328200160321     c
328300160321     c                   EVAL      dstaslnp = arblnp
328400160321     c                   EVAL      dstasnrs = arbnrs
328500160321     c                   EVAL      dstasnsp = arbnsp
328600160321     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
328700160314     C
328800160314     c                   Z-ADD     arblna        worg
328900160314     c                   exsr      setLnaItaEst
329000160314     C
329100160309     c* chain ar5 con bolla richiesta a video
329200160309     c                   Z-ADD     kasaas        kr5aas
329300160309     c                   Z-ADD     kaslnp        kr5lnp
329400160309     c                   Z-ADD     kasnrs        kr5nrs
329500160309     c                   Z-ADD     kasnsp        kr5nsp
329600160309     c                   MOVEL     'GEN'         kr5trd
329700160309     c     keyar5        CHAIN     fiar501l
329800160309     C                   IF        %FOUND
329900160309     C                   EVAL      DAR5GEN = AR5Uni
330000160309     C                   ELSE
330100160309     C                   RESET                   DAR5GEN
330200160309     C                   ENDIF
330300160309     C                   EVAL      REFCONSO74 = GEN§AR5REF
330400160309     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
330500160309     c*
330600160309     c* chain tipo bolla
330700160321     c                   eval      tblkey=arbcbo
330800160314     C                   EVAL      ds3A = chainTabel00f('CHAIN3':langTabel
330900160321     C                             :'3A':tblkey:%LEN(tblkey):'TBLUNI':
331000160314     C                             rpyOpCode:rpyEsito)
331100160309     c*
331200160309     c                   MOVEL     §3atb1        a1
331300160309     c                   MOVEL     §3atb1        tpporto74
331400160309if  1c                   IF        a1 = 'A'                                     *Pagamento del
331500160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
331600160309x   1c                   ELSE                                                   *Pagamento del
331700160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
331800160309e   1c                   ENDIF
331900160314     c
332000160314     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
332100160314     C                   IF        KSCI74 <> *BLANK
332200160314     c                   SETOFF                                           98
332300160314     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
332400160314     C                   RESET                   tis7700i0
332500160314     C                   EVAL      tis7700i0.aas = arbAas
332600160314     C                   EVAL      tis7700i0.lnp = arbLnp
332700160314     C                   EVAL      tis7700i0.nrs = arbNrs
332800160314     C                   EVAL      tis7700i0.nsp = arbNsp
332900160314     C                   EVAL      tis7700i0.tbl = §3atb1
333000160314     C                   EVAL      tis7700i0.ksu = kscI74
333100160314     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
333200160314     C***                CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
333300160314     C***                          : rpyEsito
333400160314     C***                          : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
333500160314     C***                          : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
333600160314     C***                          )
333700160314     C***                EVAL      *IN98 = (rpyEsito >= *ZERO AND
333800160314     C***                          tis7700o0.bollValida = *ON)
333900160314     **?Se non gli appartiene, prima di restituire errore controllo il check code.
334000160314if  2c                   IF        NOT *in98                                      *non trovato
334100160314     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
334200160314     C                   reset                   TIS778DSO
334300160314     C                   EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
334400160314     C                   RETURN
334500160314     C                   ENDIF
334600160314e   2c                   ENDIF
334700160314     C                   ENDIF
334800160314
334900160314     c
335000160309     c                   ENDSR
335100160314     c*--------------------------------------------------------------------------------------------*
335200160314     c* impostazioni del C/Assegno
335300160314     c*--------------------------------------------------------------------------------------------*
335400160314     c     ImpdsoAR9     BEGSR
335500160314     c                   Z-ADD     waas          ksbaas
335600160314     c                   Z-ADD     wlnp          ksblnp
335700160314     c                   Z-ADD     wnrs          ksbnrs
335800160314     c                   Z-ADD     wnsp          ksbnsp
335900160314     c     keycsb        CHAIN     fiar901l                           99
336000160314if  2c                   IF        NOT *in99                                    *esistente
336100160314     c* memorizza i dati
336200160314     c                   MOVEL     'S'           cao74                          *esiste C/A
336300160314     c*
336400160314     c                   Z-ADD     ar9cas        caimpo74                       *importo
336500160314     c*
336600160314     c                   EVAL      cadivo74 =  ar9vca                           *divisa
336700160314if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
336800160314     c                             caimpo74 > *zeros
336900160314     c                   EVAL      cadivo74 = 'ITL'
337000160314e   3c                   ENDIF
337100160314
337200160314     ** Visualizzo la descrizione del tipo incasso
337300160314     c**  devo impostare alcuni campi di CSB
337400160314     c                   eval      csbaas=ar9aas
337500160314     c                   eval      csblnp=ar9lnp
337600160314     c                   eval      csbnrs=ar9nrs
337700160314     c                   eval      csbnsp=ar9nsp
337800160314     c                   clear                   csbddc
337900160314     c                   clear                   csbtpa
338000160314     c                   clear                   csbtpi
338100160314     c
338200160314     C                   EVAL      cainco74 =
338300160314     C                                       GetDescrizioneIncassoContrassegno()
338400160314     c* stato C/Assegno : attivo
338500160314     c                   MOVEL     *blanks       castao74                       *stato
338600160314     C                   EVALR     tblKey = '0'
338700160314     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
338800160314     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
338900160314     C                             :rpyOpCode:rpyEsito)
339000160314     C                   IF        rpyOpCode = 'FOUND'
339100160314     c                   EVAL      castao74 = §4SDei
339200160314e   3c                   ENDIF
339300160314
339400160314if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
339500160314     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)     *NO distinta incasso
339600160314     c                   endif
339700160314
339800160314     c                   endif
339900160314     c
340000160314     c                   ENDSR
340100140604     c*--------------------------------------------------------------------------------------------*
340200140604     c* imposta i campi da FNARB se TASDCM = 0
340300140604     c*--------------------------------------------------------------------------------------------*
340400140604     c     impdsoARB     BEGSR
340500150403     c                   clear                   orarich           4 0
340600150611     c                   clear                   wdee
340700150410     c* memorizzo se utilizzo la data prevista consegna
340800150410     c                   clear                   usaprev           1
340900140604     c*
341000140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
341100140604     c                   EVAL      lnaDescO74 = sodes(i)
341200140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
341300140604     c*
341400150116      *eliminata forzatura resta impostato dalla mamma per composizione link
341500150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
341600150116     c*                  MOVE      arbmgs        aasmgsO74
341700140818      *riempimento da verificare
341800140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
341900140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
342000140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
342100140818
342200140818     c                   if        tpporto74 = 'A'
342300140818     c                   if        arbccm > 0
342400140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
342500140818     c                   else
342600140818     c                   movel     arblnp        ccmO74
342700140818     c                   move      8888          ccmO74
342800140818     c                   endif
342900140818     c                   else
343000140818     c                   z-add     arbksc        ccmO74
343100140818     c                   endif
343200140818      *
343300140604     c                   MOVEL     arblna        lineaaro74
343400140604     c                   MOVEL     arbtsp        tpservio74
343500140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
343600140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
343700140604     C                             rpyOpCode:rpyEsito)
343800140604     C                   EVAL      corro74 = §5EDes
343900140604     c*
344000140604     c                   EVAL      destino74 = arbrsd
344100140604     c                   EVAL      desindo74 = arbind
344200140604     c                   EVAL      descapo74 = arbcad
344300140604     c                   EVAL      desloco74 = arblod
344400140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
344500140604if  1c                   IF        arbnzd    = *blanks
344600140604     c                   EVAL      desnazo74 = arbprd
344700140604x   1c                   ELSE
344800150326     c**                 EVAL      desnazo74 = arbnzd
344900150326     c**                 EVAL      desnzdo74 = arbnzd
345000150326     c* 26/03/15: Attiviamo le specifiche per provincia export
345100150326     c                   EVAL      desnazo74 = arbprd
345200150326     c                   EVAL      desnzdo74 = arbnzd
345300150326e   1c                   ENDIF
345400140604     c*
345500140604     c                   EVAL      collio74  = arbncl
345600140604     c                   EVAL      pesoo74   = arbpkf
345700140604     c                   EVAL      volumeo74 = arbvlf
345800140604     c                   EVAL      naturao74 = arbnas
345900140604     c*
346000140604     c                   EVAL      disnumo74  = arbndc
346100140604c    c                   MOVEL     arbddc        wdat
346200140604     c                   EXSR      edtdat
346300140604     c                   EVAL      disdatao74 = wdate
346400140604     c*
346500140604     c                   EVAL      mito74    = arbrsm
346600140604     c                   EVAL      mitindo74 = arbinm
346700140604     c                   EVAL      mitcapo74 = arbcam
346800140604     c                   EVAL      mitloco74 = arblom
346900140604      *specifiche da eliminare dopo la modifica in immissione bolle
347000140604if  3c                   IF        arbnzm = *blanks
347100140604     c                   EVAL      mitnazo74 = arbprm
347200140604x   3c                   ELSE
347300140604     c                   EVAL      mitnazo74 = arbnzm
347400140604e   3c                   ENDIF
347500140604     c* 2a ragione sociale destinatario.
347600140604     c                   Z-ADD     arbaas        ka4aas
347700140604     c                   Z-ADD     arblnp        ka4lnp
347800140604     c                   Z-ADD     arbnrs        ka4nrs
347900140604     c                   Z-ADD     arbnsp        ka4nsp
348000140604     c                   MOVEL     'D'           ka4trc
348100140604     c     keyta4        CHAIN     fiar401l                           99
348200140604if  1c                   IF        NOT *in99
348300140604     c                   MOVEL     ar4not        desti2O74
348400140604e   1c                   ENDIF
348500140604     c                   MOVEL     'E'           ka4trc
348600140604     c     keyta4        CHAIN     fiar401l                           99
348700140604if  1c                   IF        NOT *in99
348800140604     c                   MOVEL     ar4not        dsbl4e
348900140604e   1c                   ENDIF
349000140604     c
349100140604     ** Riferimento mittente numerico (c'è sempre).
349200140604     C                   EVAL      rifero74 = arbrmn
349300140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
349400140604     C                   IF        arbrma <> *blank and
349500140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
349600140604     C                   EVAL      rifero74a = arbrma
349700140604     C                   ENDIF
349800140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
349900140604     C                   IF        §b4erp <> *BLANK AND
350000140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
350100140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
350200140604     C                   EVAL      rpto74 = §b4erp
350300140604     C                   ENDIF
350400140604
350500140604     **?Reperisco il check code della spedizione.
350600140604     C                   EVAL      SChkCdeO74 =
350700140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
350800140604     C                             NSpedizI74:ChkCode:nEsito)
350900140604
351000140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
351100141126     C*m                 IF        KSCI74 <> *BLANK
351200141126     c*m                 SETOFF                                           98
351300140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
351400141126     C*m                 RESET                   tis7700i0
351500141126     C*m                 EVAL      tis7700i0.aas = arbAas
351600141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
351700141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
351800141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
351900141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
352000141126     C*m                 EVAL      tis7700i0.ksu = kscI74
352100141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
352200141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
352300141126     C*m                           : rpyEsito
352400141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
352500141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
352600141126     C*m                           )
352700141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
352800141126     C*m                           tis7700o0.bollValida = *ON)
352900140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
353000141126if  2c*m                 IF        NOT *in98                                      *non trovato
353100141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
353200141126     C*m                 reset                   TIS778DSO
353300141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
353400141126     C*m                 RETURN
353500141126     C*m                 ENDIF
353600141126e   2c*m                 ENDIF
353700141126     C*m                 ENDIF
353800140604     c*
353900140604     ** Importo assicurato.
354000140604     C                   IF        arbIas > 0
354100140604     C                   EVAL      vasO74 = arbVas
354200140604     C                   EVAL      iasO74 = arbIas
354300140604     C                   ENDIF
354400140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
354500140605     c                   if        arbdcm = 0 and wcev <> '704'
354600140617     c                   if        arbdcr > 0
354700140605     c                   MOVEL     arbdcr        wdat
354800140605     c                   EXSR      edtdat
354900140605     c                   eval      DTCONRCO74  =  wdate
355000140819if  3c                   IF        arbtcr = ' '
355100140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
355200140819e   3c                   ENDIF
355300140617     c                   endif
355400140617     c                   if        arbhcr > 0
355500140617     c                   MOVEL     arbhcr        wora
355600140617     c                   EXSR      edtora
355700140617     c                   eval      ORACONRO74  =  worae
355800140617     c                   endif
355900140606     c                   eval      TPDATCRO74  =  arbtcr
356000160407     c
356100140605      *reperimento descrizione tipo consegna richiesta
356200140617if  3c                   IF        arbtcr = 'P'
356300140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
356400140617e   3c                   ENDIF
356500140617if  3c                   IF        arbtcr = 'D'
356600140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
356700140617e   3c                   ENDIF
356800160407     c
356900160407     c
357000160407     c* Per  le bolle in partenza eseguo solo se presente la data entrata
357100160407    0c                   if        esito<>'P' or arbdse>0
357200160407     c
357300140605     C                   clear                   tnsd99ds
357400140605     C                   MOVEL     ' '           D98tla
357500160314     c                   if        esito='P'
357600160314     C                   MOVEL     'P'           D98tbo
357700160314     c                   else
357800140605     C                   MOVEL     'A'           D98tbo
357900160314     c                   endif
358000140605     C                   Z-ADD     arbaas        D98aas
358100140605     C                   Z-ADD     arblnp        D98lnp
358200140605     C                   Z-ADD     arbnrs        D98nrs
358300140605     C                   Z-ADD     arbnsp        D98nsp
358400140605     C*
358500140605     C                   call      'TNSD99R'
358600140605     C                   parm                    tnsd99ds
358700150304     c
358800140605     C*
358900140917      * chiamata a trulors per tempi standard
359000140917     c                   clear                   trulorsds
359100140917     c                   clear                   trulor2ds
359200140917     c                   clear                   trulor3ds
359300150611     c                   eval      wdee=d98dee
359400140917      * solo se questa data è valorizzata
359500150304    1c                   if        d98dee > 0
359600140912     c                   movel     d98dee        wdat
359700140605     c                   exsr      edtdat
359800140605     c                   eval      DTTEOCOO74  =  wdate
359900140819     c
360000140904      * se fermo deposito non richiamo pgm per reperimento orari
360100150304    2c                   if         arbffd = *blank
360200140605     c                   eval       IOREtser = 'C'
360300140605     c                   eval       IOREDDC=arbddc
360400140605     c                   eval       IOREnDC=arbndc
360500140609     c                   eval       IOREfil=arblna
360600140609     c                   eval       IOREdta=d98dce
360700140609     c                   eval       IOREcap=arbcad
360800140609     c                   eval       IOREloc=arblod
360900151019     c*****              eval       IOREtfp=arbtfp
361000151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
361100151019     c* del terminal di partenza originale
361200151019     c                   if         d98flp>*zeros
361300151019     c                   eval       IOREtfp=%INT(d98flp)
361400151019     c                   else
361500151019     c                   eval       IOREtfp=arbtfp
361600151019     c                   endif
361700140609     c                   eval       IOREtfa=arbtfa
361800140610     c                   eval       IOREdti=arbdti
361900140610     c                   eval       IOREhti=arbhti
362000140610     c                   eval       IOREdcr=arbdcr
362100140610     c                   eval       IOREhcr=arbhcr
362200140610     c                   eval       IOREtcr=arbtcr
362300140819     c                   eval       IOREtsp=arbtsp
362400140819     c                   eval       IOREnar=arbnzd
362500140819     c                   eval       IOREdei=d98dei
362600140819     c                   eval       IOREoei=d98oei
362700150819     c                   clear                   diore01
362800150819     c                   movel     d98ttc        §ioretrazC
362900150819     c                   movel     d98tcc        §ioreconsC
363000150819     c                   eval       IOREflo=diore01
363100150325     c* passo la località normalizzata
363200150325     c                   clear                   tita4000
363300150325     c                   clear                   tita4p00
363400150325     c                   Z-ADD     arbaas        ka4aas
363500150325     c                   Z-ADD     arblnp        ka4lnp
363600150325     c                   Z-ADD     arbnrs        ka4nrs
363700150326     c                   Z-ADD     arbnsp        ka4nsp
363800150325     c                   MOVEL     'X'           ka4trc
363900160314     c                   if        esito='P'
364000160314     c     keyta4        CHAIN     fiar401l                           99
364100160314if  1c                   IF        NOT *in99
364200160314     c                   MOVEL     ar4not        OOR3LOC_N
364300160314     c                   else
364400160314     c                   movel     arblod        OOR3LOC_N
364500160314e   1c                   ENDIF
364600160314     c
364700160314     c                   else
364800160314     c     keyta4        CHAIN     tita430c                           99
364900150325if  1c                   IF        NOT *in99
365000150325     c                   MOVEL     ta4not        OOR3LOC_N
365100150325     c                   else
365200150325     c                   movel     arblod        OOR3LOC_N
365300150325e   1c                   ENDIF
365400160314e   1c                   ENDIF
365500140605     c
365600160321     c                   if        esito<>'P'
365700150304    3c                   IF         (ARBfbc = *blanks and
365800140605     c                               ARBddc > 0 and
365900140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
366000140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
366100140605     c                   eval       IOREcons = 'S'
366200150304    3c                   endif
366300160321     c
366400140605     c* Orario presunto ritiro/consegna
366500140605     c                   move      arbndc        pctndc
366600140605     c                   move      arbifp        pctfgs
366700140605     c                   move      'COR'         ttrd
366800140605     c     kcet          setgt     fipct02l
366900140605     c     kcet          readpe    fipct02l
367000150304    3c                   if        not %eof(fipct02l)
367100140605     c                   movel     pctdati       fipctcords
367200140605     c                   eval      ioreorp=§PCTORASTI
367300140605     c                   endif
367400160321    3c                   endif
367500160321
367600140605     c                   call      'TRULORSR'
367700140605     C                   PARM                    kpjba
367800140605     C                   PARM                    trulorsds
367900140605     C                   PARM                    trulor2ds
368000140605     C                   PARM                    trulor3ds
368100150304    3c                   endif
368200140818     c                   clear                   ORSTINIO74
368300150304    3c                   if        OOR2STIS > 0
368400140617     c                   MOVEL     OOR2STIS      wora
368500140617     c                   EXSR      edtora
368600140617     c                   eval      ORSTINIO74  =  worae
368700150304    3c                   endif
368800140818     c                   clear                   ORSTFINO74
368900150304    3c                   if        OOR2STFS > 0
369000140617     c                   MOVEL     OOR2STFS      wora
369100140617     c                   EXSR      edtora
369200140617     c                   eval      ORSTFINO74  =  worae
369300150304    3c                   endif
369400140818     c                   clear                   ORMIINIO74
369500150304    3c                   if        OOR2MIIS > 0
369600140617     c                   MOVEL     OOR2MIIS      wora
369700140617     c                   EXSR      edtora
369800140617     c                   eval      ORMIINIO74  =  worae
369900150304    3c                   endif
370000140818     c                   clear                   ORMAFINO74
370100150304    3c                   if        OOR2MXFS > 0
370200140617     c                   MOVEL     OOR2MXFS      wora
370300140617     c                   EXSR      edtora
370400140617     c                   eval      ORMAFINO74  =  worae
370500150304    3c                   endif
370600150304    3c                   if        wricons = 'S'
370700140818     c                   clear                   ORRIDALO74
370800150304    4c                   if        OOR2RIDS > 0
370900140617     c                   MOVEL     OOR2RIDS      wora
371000140617     c                   EXSR      edtora
371100140617     c                   eval      ORRIDALO74  =  worae
371200150304    4c                   endif
371300140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
371400140818     c                   clear                   ORRIALLO74
371500150304    4c                   if        OOR2RIAS > 0
371600140617     c                   MOVEL     OOR2RIAS      wora
371700140617     c                   EXSR      edtora
371800140617     c                   eval      ORRIALLO74  =  worae
371900150304    4c                   endif
372000140617     c                   eval      FLRICONO74  =  OOR2FRIC
372100150304   x3c                   else
372200140711     c                   clear                   ORRIALLO74
372300140711     c                   clear                   ORRIDALO74
372400140711     c                   eval      FLRICONO74  =  'N'
372500150304    3c                   endif
372600140818     c                   clear                   ORSTICDO74
372700150304    3c                   if        OOR2PRESD > 0
372800140617     c                   MOVEL     OOR2PRESD     wora
372900140617     c                   EXSR      edtora
373000140617     c                   eval      ORSTICDO74  =  worae
373100150304    3c                   endif
373200140818     c                   clear                   ORSTICAO74
373300150304    3c                   if        OOR2PRESa > 0
373400140617     c                   MOVEL     OOR2PRESa     wora
373500140617     c                   EXSR      edtora
373600140617     c                   eval      ORSTICAO74  =  worae
373700150304    3c                   endif
373800140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
373900140605     c                   eval      FLLOCNOO74  =  OOR3NORM
374000140904      *end F.D.
374100150304    2c                   endif
374200140605      * RIFERIMENTI DESTINATARIO
374300140605     c                   MOVEL     'EMD'         kr5trd
374400140605     c     keyAR5        CHAIN     fiar501l
374500150304if  2c                   IF        %found(fiar501l)
374600140605     c                   MOVEL     ar5uni        dar5emd
374700140606     c                   eval      EMLDESTO74  =  §§AR5EML
374800140606     c                   eval      CELDESTO74  =  §§AR5TEL
374900150304e   2c                   ENDIF
375000140604     **
375100150304     c*
375200150304     c* Stato data prevista consegna
375300150305     c                   clear                   stat_n2o74
375400150305     c                   eval      STATUSo74   =  D98SPCDEE
375500150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
375600150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
375700150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
375800150312     c                   eval      statuso74='RIC'
375900150403     c* se presente FIARP --> visualizzo la data richiesta
376000150403     c     karp          chain     fiarp01l
376100150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
376200150611     c                   movel     arpdcr        wdee
376300150611     c                   movel     arpdcr        wdat
376400150403     c                   exsr      edtdat
376500150403     c                   eval      DTTEOCOO74  =  wdate
376600150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
376700150421    3C                   IF        ARPHCR>0
376800150403     c                   eval      orarich=arphcr
376900150421    3c                   endif
377000150421   x2c                   else
377100150403     c*  se presente DOPO IL imposto la data in D98dee
377200150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
377300150403     c                   eval      D98dee=arpdcr
377400150611     c                   eval      wdee=arpdcr
377500150421    3c                   endif
377600150403     c* PRIMA DEL come se non ci fosse...
377700150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
377800150421    3c                   if        d98dee>0
377900150312     c                   Clear                   xgiolavds
378000150312     c                   Eval      Ixglpa  = 'A'
378100150312     c                   Eval      IxglFil = arblna
378200150312     c                   Eval      Ixgldata = d98dee
378300150312     c                   Eval      Ixgladd = 'S'
378400150312     c                   Eval      Ixgllav = 'S'
378500150312     c                   eval      ixglgg  = 1
378600150312     c                   Call      'XGIOLAV'
378700150312     c                   parm                    xgiolavds
378800150611     c                   movel     oxgldata      wdee
378900150611     c                   movel     oxgldata      wdat
379000150312     c                   exsr      edtdat
379100150312     c                   eval      DTTEOCOO74  =  wdate
379200150421    3c                   endif
379300150421    2c                   endif
379400150421    1c                   endif
379500150421     c
379600150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
379700150421     c                   eval      statuso74='ATT'
379800150421     c                   endif
379900150421    1c                   if        PDAgia='S' and statuso74='DDC'
380000150421     c                   eval      statuso74='GIA'
380100150421     c                   endif
380200150401     c
380300150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
380400150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
380500150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
380600150401     c* La data contatto = data distinta + 1
380700150512     c**                 Clear                   xgiolavds
380800150512     c**                 Eval      Ixglpa  = 'A'
380900150512     c**                 Eval      IxglFil = arblna
381000150512     c**                 Eval      Ixgldata = arbddc
381100150512     c**                 Eval      Ixgladd = 'S'
381200150512     c**                 Eval      Ixgllav = 'S'
381300150512     c**                 eval      ixglgg  = 1
381400150512     c**                 Call      'XGIOLAV'
381500150512     c**                 parm                    xgiolavds
381600150512     c**                 movel     oxgldata      wdat
381700150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
381800150512     c**                 endif
381900150317     c
382000150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
382100150326     c*  lo status diventa RIC
382200150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
382300150317     c                   eval      statuso74='RIC'
382400150317     c                   endif
382500150310     c
382600150304     c* decodifica stato da mettere nei campi deco e note
382700150304     c                   clear                   dspc
382800150304     C                   RESET                   tibs02ds
382900150304     C                   EVAL      t02Cod = 'SPC'
383000150312     C                   EVAL      t02Ke1 = statuso74
383100150306     C                   EXSR      getTntbe00f
383200150306     c                   IF        t02Err <>'E'
383300150306     c
383400150305     c                   eval       stat_d1o74=§SPCDEC1
383500150305     c                   eval       stat_d2o74=§SPCDEC2
383600150304     c*
383700150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
383800150305     c                   eval       stat_n1o74=§SPCNOT1
383900150306     c                   eval       stat_n2o74=§SPCNOT2
384000150304     c                   else
384100150305     c                   clear                   stat_d1o74
384200150305     c                   clear                   stat_d2o74
384300150305     c                   clear                   stat_n1o74
384400150306     c                   clear                   stat_n2o74
384500150304     c                   endif
384600160407
384700160407     c                   endif
384800160407
384900150409     c                   else
385000150409     c* stato evento CONSEGNATO
385100150409     c                   eval      statuso74='CON'
385200150415     c                   eval      flgbolcO74  =  'S'
385300150415     c                   eval      flgboldO74  =  'N'
385400150409    1c                   endif
385500150304     c
385600150304     c
385700140919     ** riempimento per campi da arb con data consegna diversa da 0
385800140919     c                   if        arbdcm > 0
385900140919     c                   movel     arbdcm        wdat
386000140919     c                   exsr      edtdat
386100140919     c                   eval      dtconmeo74  = wdate
386200140919     c                   MOVEL     arbdcm        wora
386300140919     c                   EXSR      edtora
386400140919     c                   eval      orconmeO74  =  worae
386500140919     c                   eval      flgbolcO74  =  'S'
386600140919     c                   eval      flgboldO74  =  'N'
386700141015      *firmatario distinta PDA
386800141015     c                   clear                   firmato74
386900141015     c                   if        gen§ar5pdaco <> 'NO'
387000141015     c                   exsr      repfirmatARB
387100141015     c                   else
387200141015     c                   if        arbgma <> *blank
387300141015     C                   EVAL      tblKey = arbgma
387400141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
387500141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
387600141015     C                             :rpyOpCode:rpyEsito)
387700141015     C                   IF        rpyOpCode = 'FOUND'
387800141015     c                   if        §7r2fi = 'S'
387900141015     c                   exsr      repfirmatARB
388000141015     c                   endif
388100141015     c                   endif
388200141015     c                   endif
388300141015     c                   endif
388400140919     c                   else
388500150415     c* solo se non impostato da PdA lo stato CONSEGNA
388600150415     c                   if        statuso74<>'CON'
388700150415     c
388800140919     c                   eval      flgbolcO74  =  'N'
388900140919     c                   if        arbndc > 0 and arbddc > 0
389000150415     c                             and arbddc <= datcor and d98cons='S'
389100140919     c                   eval      flgboldo74 = 'S'
389200140919     c                   else
389300140919     c                   eval      flgboldO74  =  'N'
389400140919     c                   endif
389500150415     c                   endif
389600150415
389700140919     c                   endif
389800150415     c
389900150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
390000140919     c                   eval      orincono74 = orsticdo74
390100140919     c                   eval      orficono74 = ORSticaO74
390200140919     c                   else
390300140919     c                   eval      orincono74 = ORSTINIO74
390400140919     c                   eval      orficono74 = ORSTFINO74
390500140919     c                   endif
390600150326     c* note 1
390700150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
390800150309     c                   clear                   wparm
390900150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
391000150326     c                   if        D98DTCTD<>*blanks
391100150326     c                   movel     D98DTCTD      wdat
391200150326     c                   exsr      edtdat
391300150326     c                   else
391400150326     c                   eval      wdate=dtteocoo74
391500150410     c                   eval      usaprev='S'
391600150326     c                   endif
391700150326     c
391800150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
391900150309     c                   eval      stat_n1o74= rtvMsgLang(
392000150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
392100150309     c                   endif
392200150309     c* note 2
392300150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
392400150309     c                   clear                   wparm
392500150403     c* Se lo stato è PRG passo orario limite uscita aut
392600150403     c                   select
392700150403     c                   when      D98SPCDEE='PRG'
392800150326     c                   IF        d98picklim  >*zeros
392900150309
393000150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
393100150326     c                                   (%subst(d98picklim:3:2))
393200150309     c                   else
393300150309     c                   eval      wparm='09:00'
393400150403     c* reperisco orario di fine picking
393500150403     c* filiale / data
393600150403     C                   RESET                   tibs02ds
393700150403     C                   EVAL      t02Cod = 'OLP'
393800150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
393900150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
394000150403     C                   EXSR      getTntbe00f
394100150403     c* filiale
394200150403     c                   IF        t02Err = 'E'
394300150403     C                   RESET                   tibs02ds
394400150403     C                   EVAL      t02Cod = 'OLP'
394500150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
394600150403     C                   EXSR      getTntbe00f
394700150403     c                   endif
394800150403     c* Generico 000
394900150403     c                   IF        t02Err = 'E'
395000150403     C                   RESET                   tibs02ds
395100150403     C                   EVAL      t02Cod = 'OLP'
395200150403     C                   EVAL      t02Ke1 = '000'
395300150403     C                   EXSR      getTntbe00f
395400150403     c                   endif
395500150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
395600150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
395700150403     c                             %subst(t02uni:3:2)
395800150309     c                   endif
395900150403     c                   endif
396000150403     c                   eval      stat_n2o74= rtvMsgLang(
396100150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
396200150403     c                   other
396300150403     c* altri casi
396400150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
396500150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
396600150611     c                             arbtcr=' ' and arbdcr>0  and
396700150611     c                             arbdcr>=wdee
396800150410     c
396900150403     c                   eval      orarich=arbhcr
397000150410     c                   endif
397100150403     c
397200150403     c                   if        orarich>0
397300150403     c                   exsr      sr_dallealle
397400150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
397500150403     c                   eval      stat_n2o74= rtvMsgLang(
397600150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
397700150403     c
397800150403     c                   else
397900150403     c                   clear                   stat_n2o74
398000150403     c                   endif
398100150403     c
398200150403     c                   endsl
398300150403     c
398400150309     c                   endif
398500150410     c
398600150424     c* Se si tratta di fermo deposito pulisco sempre
398700150424     c                   if        usaprev<>'S' or arbffd='S'
398800150410     c                   clear                   DTTEOCOO74
398900150410     c                   clear                   ORINCONO74
399000150410     c                   clear                   ORFICONO74
399100150410     c                   endif
399200150309     c
399300140604     c                   ENDSR
399400150403      /free
399500150403       //--------------------------------------------------------------
399600150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
399700150403       //--------------------------------------------------------------
399800150403       BEGSR  sr_dallealle;
399900150403       clear  w_dallemsg ;
400000150403       clear  w_allemsg ;
400100150403
400200150403       // Reperimento dati tabella VPO - ORARISER
400300150403       if §VPORST_MW=0  ;
400400150403       clear tibs02ds;
400500150403       clear dvpooraris;
400600150403       t02mod='C';
400700150403       t02cod='VPO';
400800150403       t02ke1='ORARISER';
400900150403       tibs02r(kpjba:tibs02ds);
401000150403       if t02err=*blanks;
401100150403          dvpooraris=t02uni;
401200150403       endif;
401300150403       endif;
401400150403
401500150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
401600150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
401700150403       //   determino orari servizio comprensivi della tolleranza
401800150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
401900150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
402000150403
402100150403            if w_dalle<w_dalleoser;
402200150403               w_dalle=w_dalleoser;
402300150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
402400150403            endif;
402500150403
402600150403            if w_alle>w_alleoser;
402700150403               w_alle=w_alleoser;
402800150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
402900150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
403000150403               endif;
403100150403            endif;
403200150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
403300150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
403400150403
403500150403       endsr;
403600150403      /end-free
403700010717     c*--------------------------------------------------------------------------------------------*
403800010717     c* imposta i campi della DS di Output dagli eventi
403900010717     c*--------------------------------------------------------------------------------------------*
404000010717     c     RepAbilFirma  BEGSR
404100010717     c*
404200010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
404300010717     c* e recupera utente / password
404400010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
404500010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
404600010717     c     keyvtd        SETLL     tivtd01l
404700010717     c     keyvtd        READE     tivtd01l                               98
404800010717do  2c                   DOW       NOT *in98
404900010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
405000010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
405100010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
405200010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
405300010717     c                   EVAL      depute = vtdute                              *utente
405400010717     c                   EVAL      deppwd = vtdpwd                              *password
405500010717     c                   LEAVE                                                  *esce dal ciclo
405600010717e   3c                   ENDIF
405700010717     c     keyvtd        READE     tivtd01l                               98
405800010717e   2c                   ENDDO
405900010717e   1C                   ENDIF
406000010717     c*
406100010717     c                   ENDSR
406200000000     c*--------------------------------------------------------------------------------------------*
406300000000     c* edita data da aaaammgg in gg/mm/aaaa
406400000000     c*--------------------------------------------------------------------------------------------*
406500000000     c     edtdat        BEGSR
406600000000     c*
406700000000     c                   MOVEL     *blanks       wdate
406800000000     c*
406900000000     c                   MOVEL     wdat          dsdat
407000000000     c                   MOVEL     dsgio         dsgioe
407100140528     c                   MOVEL     '.'           dsvd1
407200000000     c                   MOVEL     dsmes         dsmese
407300140528     c                   MOVEL     '.'           dsvd2
407400000000     c                   MOVEL     dsann         dsanne
407500000000     c                   MOVEL     dsdate        wdate
407600000000     c*
407700000000     c                   ENDSR
407800000000     c*--------------------------------------------------------------------------------------------*
407900000000     c* edita ora da hhmm in hh:mm
408000000000     c*--------------------------------------------------------------------------------------------*
408100000000     c     edtora        BEGSR
408200000000     c*
408300000000     c                   MOVEL     *blanks       worae
408400000000     c*
408500000000     c                   MOVEL     wora          dsora
408600000000     c                   MOVEL     dshh          dshhe
408700140617     c                   MOVEL     '.'           dsvo1
408800000000     c                   MOVEL     dsmm          dsmme
408900000000     c                   MOVEL     dsorae        worae
409000000000     c*
409100000000     c                   ENDSR
409200000000     c*--------------------------------------------------------------------------------------------*
409300000000     c* decodifica punto operativo
409400000000     c*--------------------------------------------------------------------------------------------*
409500000000     c     decorg        BEGSR
409600000000     c*
409700000000     c                   MOVEL     *blanks       wodes                          *descrizione
409800000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
409900000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
410000000000     c*
410100000000     c                   SETOFF                                           98
410200041111     c                   Z-ADD     1             O
410300041111     c     worg          LOOKUP    sorg(O)                                98
410400000000if  1c                   IF        *in98                                        *trovata
410500121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
410600121001     c                             %EDITC(wOrg : 'X') + ')'
410700041111     c                   MOVEL     sofl1(O)      wofl1
410800041111     c                   MOVEL     sodpd(O)      wodpd
410900000000e   1c                   ENDIF
411000000000     c*
411100000000     c                   ENDSR
411200000000     c*--------------------------------------------------------------------------------------------*
411300000000     c* decodifica evento per STATI
411400000000     c*--------------------------------------------------------------------------------------------*
411500000000     c     deccevsta     BEGSR
411600000000     c*
411700000000     c                   MOVEL     '0'           werr                           *NO errore
411800050704     c*
411900050704     C                   IF        wcev = 'GEN'
412000050704     C                   EVAL      werr = *ON
412100050704     C                   CLEAR                   wcdex
412200050704     C                   LEAVESR
412300050704     C                   ENDIF
412400000000     c*
412500060622     C                   RESET                   tibs02ds
412600060622     C                   EVAL      t02Cod = 'ICE'
412700060622     C                   EVAL      t02Ke1 = wcev
412800060622     C                   EXSR      getTntbe00f
412900060622     C                   IF        t02Err <> 'E' AND
413000140528     C                             (§icesta = 'S'  OR
413100140528     C                               §icestaP = 'S' )
413200060622     c                   MOVEL     §icedei       wcdex
413300000000x   1c                   ELSE                                                   *non trovato
413400000000     c                   MOVEL     '1'           werr                           *SI errore
413500030130     c                   MOVEL     *blanks       wcdex
413600000000e   1c                   ENDIF
413700000000     c*
413800000000     c                   ENDSR
413900000000     c*--------------------------------------------------------------------------------------------*
414000000000     c* decodifica consegna particolare
414100000000     c*--------------------------------------------------------------------------------------------*
414200000000     c     decftc        BEGSR
414300000000     c*
414400000000     c                   MOVEL     *blanks       wdftc
414500000000     c*
414600000000if  1c                   IF        wftc = 'S'
414700060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
414800000000e   1c                   ENDIF
414900000000if  1c                   IF        wftc = 'P'
415000060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
415100000000e   1c                   ENDIF
415200000000if  1c                   IF        wftc = 'A'
415300060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
415400000000e   1c                   ENDIF
415500000000if  1c                   IF        wftc = 'F'
415600060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
415700000000e   1c                   ENDIF
415800000000     c*
415900000000     c                   ENDSR
416000000000     c*--------------------------------------------------------------------------------------------*
416100000000     c* decodifica turno di chiusura
416200000000     c*--------------------------------------------------------------------------------------------*
416300000000     c     decgcx        BEGSR
416400000000     c*
416500000000     c                   MOVEL     *blanks       wdgcx
416600000000     c* giorno
416700000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
416800000000if  1c                   IF        a1 = '1'
416900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
417000000000e   1c                   ENDIF
417100000000if  1c                   IF        a1 = '2'
417200060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
417300000000e   1c                   ENDIF
417400000000if  1c                   IF        a1 = '3'
417500060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
417600000000e   1c                   ENDIF
417700000000if  1c                   IF        a1 = '4'
417800060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
417900000000e   1c                   ENDIF
418000000000if  1c                   IF        a1 = '5'
418100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
418200000000e   1c                   ENDIF
418300000000if  1c                   IF        a1 = '6'
418400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
418500000000e   1c                   ENDIF
418600000000if  1c                   IF        a1 = '7'
418700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
418800000000e   1c                   ENDIF
418900000000if  1c                   IF        a1 = ' '
419000000000     c                   EVAL      wdgcx = *blanks
419100000000e   1c                   ENDIF
419200000000     c* am/pm
419300000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
419400000000if  1c                   IF        a1 = ' '
419500060612     c                   EVAL      wdgcx =
419600060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
419700000000e   1c                   ENDIF
419800000000if  1c                   IF        a1 = 'M'
419900060612     c                   EVAL      wdgcx =
420000060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
420100000000e   1c                   ENDIF
420200000000if  1c                   IF        a1 = 'P'
420300060612     c                   EVAL      wdgcx =
420400060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
420500000000e   1c                   ENDIF
420600000000     c*
420700000000     c                   ENDSR
420800000000     c*--------------------------------------------------------------------------------------------*
420900000000     c* decodifica evento per GIACENZE
421000000000     c*--------------------------------------------------------------------------------------------*
421100000000     c     deccevgia     BEGSR
421200000000     c*
421300000000     c                   MOVEL     '0'           werr                           *NO errore
421400000000     c*
421500060622     C                   RESET                   tibs02ds
421600060622     C                   EVAL      t02Cod = 'ICE'
421700060622     C                   EVAL      t02Ke1 = wcev
421800060622     C                   EXSR      getTntbe00f
421900060622     C                   IF        t02Err <> 'E' AND                            *trovata
422000140528     C                             (§icegia = 'S'  OR
422100140528     C                               §icegiap = 'S')
422200060622     c                   MOVEL     §icedei       wcdex
422300000000x   1c                   ELSE                                                   *non trovato
422400000000     c                   MOVEL     '1'           werr                           *SI errore
422500030130     c                   MOVEL     *blanks       wcdex
422600000000e   1c                   ENDIF
422700000000     c*
422800000000     c                   ENDSR
422900000000     c*--------------------------------------------------------------------------------------------*
423000000000     c* Controlla la DS dsi Input
423100000000     c*--------------------------------------------------------------------------------------------*
423200000000     c     chkdsinput    BEGSR
423300000000     c*
423400000000     c                   MOVEL     '0'           werr                           *NO errore
423500000000     c*
423600000000     c* dalla DS di Input scompone il codice spedizione
423700000000     c                   MOVEL     nspedizi74    dsspedizi74
423800000000     c*
423900000000     c* controlla che la lina di partenza sia TUTTA numerica
424000000000     c                   SETOFF                                       99
424100000000     c                   TESTN                   dslnpi74             99
424200000000if  1c                   IF        NOT *in99                                    *non è un numero
424300000000     c                   MOVEL     '1'           werr                           *SI errore
424400140617     c                   eval      widmsg = 'TIS0769'
424500140617     c                   eval      widcampo = '*LNP'
424600140617     c                   clear                   wparm
424700140617     c                   exsr      messaggi
424800000000x   1c                   ELSE                                                   *è un numero
424900000000     c                   MOVE      dslnpi74      a1
425000000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
425100000000     c                   MOVEL     '1'           werr                           *SI errore
425200140617     c                   eval      widmsg = 'TIS0769'
425300140617     c                   eval      widcampo = '*LNP'
425400140617     c                   clear                   wparm
425500140617     c                   exsr      messaggi
425600000000e   2c                   ENDIF
425700000000e   1c                   ENDIF
425800000000     c*
425900000000     c* controlla che la serie spedizione sia TUTTA numerica
426000000000     c                   SETOFF                                       99
426100000000     c                   TESTN                   dsnrsi74             99
426200000000if  1c                   IF        NOT *in99                                    *non è un numero
426300000000     c                   MOVEL     '1'           werr                           *SI errore
426400140617     c                   eval      widmsg = 'TIS0769'
426500140617     c                   eval      widcampo = '*NRS'
426600140617     c                   clear                   wparm
426700140617     c                   exsr      messaggi
426800000000x   1c                   ELSE                                                   *è un numero
426900000000     c                   MOVE      dsnrsi74      a1
427000000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
427100000000     c                   MOVEL     '1'           werr                           *SI errore
427200140617     c                   eval      widmsg = 'TIS0769'
427300140617     c                   eval      widcampo = '*NRS'
427400140617     c                   clear                   wparm
427500140617     c                   exsr      messaggi
427600000000e   2c                   ENDIF
427700000000e   1c                   ENDIF
427800000000     c*
427900000000     c* controlla che il numero spedizione sia TUTTO numerico
428000000000     c                   SETOFF                                       99
428100000000     c                   TESTN                   dsnspi74             99
428200000000if  1c                   IF        NOT *in99                                    *non è un numero
428300000000     c                   MOVEL     '1'           werr                           *SI errore
428400140617     c                   eval      widmsg = 'TIS0769'
428500140617     c                   eval      widcampo = '*NSP'
428600140617     c                   clear                   wparm
428700140617     c                   exsr      messaggi
428800000000x   1c                   ELSE                                                   *è un numero
428900000000     c                   MOVE      dsnspi74      a1
429000000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
429100000000     c                   MOVEL     '1'           werr                           *SI errore
429200140617     c                   eval      widmsg = 'TIS0769'
429300140617     c                   eval      widcampo = '*NSP'
429400140617     c                   clear                   wparm
429500140617     c                   exsr      messaggi
429600000000e   2c                   ENDIF
429700000000e   1c                   ENDIF
429800110104     c*
429900110104     c* controlla che il codice cliente sia TUTTO numerico
430000110104     c                   IF        kscI74 <> *BLANK
430100110104     c                   SETOFF                                       99
430200110104     c                   TESTN                   kscI74               99
430300110104     c                   IF        NOT *in99                                    *non è un numero
430400110104     c                   MOVEL     '1'           werr                           *SI errore
430500140617     c                   eval      widmsg = 'TIS0769'
430600140617     c                   eval      widcampo = 'KSCI74'
430700140617     c                   clear                   wparm
430800140617     c                   exsr      messaggi
430900110104     c                   ELSE                                                   *è un numero
431000110104     c                   MOVE      kscI74        a1
431100110104     c                   IF        a1 < '0'                                     *trovata una lettera
431200110104     c                   MOVEL     '1'           werr                           *SI errore
431300140617     c                   eval      widmsg = 'TIS0769'
431400140617     c                   eval      widcampo = 'KSCI74'
431500140617     c                   clear                   wparm
431600140617     c                   exsr      messaggi
431700110104     c                   ENDIF
431800110104     c                   ENDIF
431900110104     c                   ENDIF
432000110104     c*
432100110104     c* controlla che il codice mittente sia TUTTO numerico
432200110104     c                   IF        ccmI74 <> *BLANK
432300110104     c                   SETOFF                                       99
432400110104     c                   TESTN                   ccmI74               99
432500110104     c                   IF        NOT *in99                                    *non è un numero
432600110104     c                   MOVEL     '1'           werr                           *SI errore
432700140617     c                   eval      widmsg = 'TIS0769'
432800140617     c                   eval      widcampo = 'CCMI74'
432900140617     c                   clear                   wparm
433000140617     c                   exsr      messaggi
433100110104     c                   ELSE                                                   *è un numero
433200110104     c                   MOVE      ccmI74        a1
433300110104     c                   IF        a1 < '0'                                     *trovata una lettera
433400110104     c                   MOVEL     '1'           werr                           *SI errore
433500140617     c                   eval      widmsg = 'TIS0769'
433600140617     c                   eval      widcampo = 'CCMI74'
433700140617     c                   clear                   wparm
433800140617     c                   exsr      messaggi
433900110104     c                   ENDIF
434000110104     c                   ENDIF
434100110104     c                   ENDIF
434200050104     c*
434300050104     c* controlla anno spedizione
434400050104     c                   TESTN                   cAASI74              99
434500050104if  1c                   IF        NOT *in99                                    *non è un numero
434600050104     c                   EVAL      cAASI74 = *ZERO
434700050104x   1c                   ENDIF
434800100222     c*
434900100223     c* Estraggo l'anno dalla data spedizione.
435000100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
435100100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
435200100223     C                   IF        dspI74 <> *BLANK
435300100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
435400100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
435500100223     C                   MONITOR
435600100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
435700100223     C                   ON-ERROR
435800100223     C                   ENDMON
435900100222     C                   ENDIF
436000000000     c*
436100000000     c* se errore, esce dal programma
436200140610if  1c                   IF        werr = '1'                                   *SI errore
436300140610     c                   eval      esito = werr                                 *errore
436400140610e   1c                   ENDIF
436500000000     c*
436600000000     c                   ENDSR
436700000000     c*--------------------------------------------------------------------------------------------*
436800000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
436900000000     c* Input:   Walfa7 = Alfanumerico di 7
437000000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
437100000000     c*--------------------------------------------------------------------------------------------*
437200000000     c     xallinea1     BEGSR
437300000000     c*
437400000000     c* reimposta variabli di lavoro
437500000000     c                   CLEAR                   i
437600000000     c                   CLEAR                   z
437700000000     c                   CLEAR                   walfa7bis
437800000000     c                   EVAL      wzeri = *ALL'0'
437900000000     c*
438000000000     c     ' '           CHECKR    walfa7:7      i
438100000000if  1c                   IF        i <> 0
438200000000     c     i             SUBST     walfa7        walfa7bis
438300000000e   1c                   ENDIF
438400000000if  1c                   IF        walfa7bis <> *blanks
438500000000     c     7             SUB       i             z
438600000000if  2c                   IF        z <> 0
438700000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
438800000000e   2c                   ENDIF
438900000000     c                   EVAL      z = z+ 1
439000000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
439100000000e   1c                   ENDIF
439200000000     c*
439300000000     c                   ENDSR
439400000000     c*--------------------------------------------------------------------------------------------*
439500000000     c* caricamento tabelle occorrenti
439600000000     c*--------------------------------------------------------------------------------------------*
439700000000     c     cartab        BEGSR
439800000000     c*---
439900000000     c* organigrama
440000000000     c*---
440100000000     c                   CLEAR                   i                              *indice
440200000000     C                   CLEAR                   sorg
440300000000     C                   CLEAR                   sodes
440400000000     C                   CLEAR                   sofl1
440500000000     C                   CLEAR                   sodpd
440600060623     C                   OPEN      tntbe01l
440700000000     c     *loval        SETLL     azorg01l
440800000000     c                   READ      azorg01l                               99
440900000000do  1c                   DOW       NOT *in99
441000000000     c                   ADD       1             i
441100000000     c                   Z-ADD     orgfil        sorg(i)
441200000000     c                   MOVEL     orgdes        sodes(i)
441300000000     c                   MOVEL     orgde3        og143
441400020702     C*
441500020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
441600020702     C                   CLEAR                   dntw
441700020702     C*
441800020702     C                   MOVEL(P)  'NTW'         kbecod
441900020702     C                   MOVEL(P)  §ogntw        kbeke1
442000020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
442100020702     c                   MOVEL     *blanks       kbelin                         *lingua
442200020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
442300020702     C     keytbe        CHAIN     tntbe01l
442400020702     C                   IF        %found(tntbe01l)
442500020702     C                   MOVEL     tbeuni        dntw
442600020702     C                   ENDIF
442700020702     C*
442800020702     C                   MOVEL     §ntwfie       sofl1(i)
442900160406     C*
443000020702     c                   MOVEL     §ogntw        sodpd(i)
443100160406     C*
443200000000     c                   READ      azorg01l                               99
443300000000e   1c                   ENDDO
443400060623     C                   CLOSE     tntbe01l
443500000000     c*---
443600000000     c                   ENDSR
443700000000     c*--------------------------------------------------------------------------------------------*
443800000000     c* operazioni iniziali -da fare sempre-
443900000000     c*--------------------------------------------------------------------------------------------*
444000000000     c     rutinz        BEGSR
444100000000     c*
444200000000     c* reperimento data corrente
444300000000     C                   TIME                    n14                            *ora (6)+ data (8)
444400000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
444500000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
444600000000     C                   Z-ADD     n8            g08dat
444700000000     C                   Z-ADD     *zeros        g08inv
444800000000     C                   MOVEL     '0'           g08err
444900000000     c                   CALL      'XSRDA8'
445000000000     c                   PARM                    wlbda8
445100000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
445200000000     C                   MOVEL     datcor        anncor                         *anno corrente
445300000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
445400000000     c*
445500000000     c* calcola data corrente - 1 anno
445600000000     c                   Z-ADD     datcor        datpre                         *data precedente
445700000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
445800000000     c*
445900010716     c* azzera la DS di Output e le variabili di lavoro / controllo
446000000000     c                   MOVEL     '0'           esito
446100140603     c                   reset                   tis778dso                      *ds Output
446200140603     c                   reset                   tis778dsm                      *ds Output
446300000000     c                   CLEAR                   smi                            *di memorizzazione
446400000000     c                   CLEAR                   smseql
446500000000     c                   CLEAR                   smseqe
446600000000     c                   CLEAR                   smdevhev
446700000000     c                   CLEAR                   smdeve
446800000000     c                   CLEAR                   smheve
446900000000     c                   CLEAR                   smfle
447000000000     c                   CLEAR                   smdev
447100000000     c                   CLEAR                   smcev
447200130312     c                   CLEAR                   smspe
447300000000     c                   CLEAR                   smdmc
447400000000     c                   CLEAR                   smdmcR
447500000000     c                   CLEAR                   tote                           *contatore eventi
447600000000     c                   CLEAR                   seql                           *sequenza legami
447700010716     c                   RESET                   $AbTD                          *di lavoro
447800010716     c                   RESET                   $AbFirma
447900150312     c                   clear                   pdaavv
448000150421     c                   clear                   pdagia
448100140603     c* parametri ingresso
448200140603     c                   exsr      Kontrolla
448300000000     c* controlla da DS in Input
448400000000     c                   EXSR      chkdsinput
448500000000     c*
448600000000     c* carica i clienti dettaglio del cliente unificante
448700000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
448800000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
448900000000     c     keyvss        SETLL     tivss02l
449000000000     c     keyvss        READE     tivss02l                               99
449100000000do  1c                   DOW       NOT *in99
449200000000if  2c                   IF        vssvat = *blanks                             *attivo
449300000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
449400000000     c                             datcor <= vssdsc
449500010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
449600010719     c* --> TD per vedere la firma DPD
449700010716     c                   RESET                   $AbTD                          *di lavoro
449800010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
449900010719     c     kssksu        SETLL     tivss02l
450000010719     c     kssksu        READE     tivss02l                               99
450100010719do  4c                   DOW       NOT *in99
450200010716if  5c                   IF        vssvat = *blanks                             *attivo
450300010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
450400010716     c                             datcor <= vssdsc
450500010719if  7c                   IF        vssisv = 'TD'
450600010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
450700010719e   7c                   ENDIF
450800010719e   6c                   ENDIF
450900010716e   5c                   ENDIF
451000010719     c     kssksu        READE     tivss02l                               99
451100010719e   4c                   ENDDO
451200010716     c*
451300000000     c                   LEAVE                                                  *uscita ciclo
451400000000e   3c                   ENDIF
451500000000e   2c                   ENDIF
451600000000     c     keyvss        READE     tivss02l                               99
451700010716e   1c                   ENDDO                                                  *fine cl.abil.TT
451800000000     c*
451900060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
452000060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
452100151216     c* Se lo strategi user number è = *blanks --> lo imposto a *zeros
452200151216     c                   monitor
452300151216     c                   if        RQSCIDI174>0
452400151216     c                   endif
452500151216     c
452600151216     c                   on-error
452700151216     c                   clear                   RQSCIDI174
452800151216     c                   endmon
452900060621     c*
453000000000     c                   ENDSR
453100040330
453200040330     ***********************************************************************************************
453300040330     **?
453400040330     **?Reperimento ulteriori dati destinatario.
453500040330     **?
453600040330     ***********************************************************************************************
453700040330     C     ImpDSDest     BEGSR
453800040330
453900140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
454000140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
454100100121     C                             ar5Trd <> 'GEN'
454200140716     c                   Z-ADD     f_tasaas      kr5aas
454300140716     c                   Z-ADD     f_taslnp      kr5lnp
454400140716     c                   Z-ADD     f_tasnrs      kr5nrs
454500140716     c                   Z-ADD     f_tasnsp      kr5nsp
454600040330     c                   MOVEL     'GEN'         kr5trd
454700040531     c     keyar5        CHAIN     fiar531c
454800040330     C                   IF        %FOUND
454900040330     C                   EVAL      DAR5GEN = AR5Uni
455000100121     C                   ELSE
455100100121     C                   RESET                   DAR5GEN
455200040330     C                   ENDIF
455300100121     C                   ENDIF
455400100121     C
455500100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
455600100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
455700040330
455800040330     C                   ENDSR
455900040913
456000040913     ***********************************************************************************************
456100040913     **?
456200040913     **?Determino se addebitare l'immagine LDV.
456300040913     **?
456400040913     ***********************************************************************************************
456500040913     C     AddebitoLDV   BEGSR
456600040913
456700040913     C                   IF        LDVO74 = 'S'
456800040913
456900040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
457000040913
457100040913     C                   ENDIF
457200040913
457300040913     C                   ENDSR
457400051110
457500051110     ***********************************************************************************************
457600051110     **?
457700051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
457800051110     **?
457900051110     ***********************************************************************************************
458000051110     C     chkTivpi      BEGSR
458100051110
458200051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
458300051110     C                             esito10)
458400051110     C                   IF        esito10 > 0 AND NOT %ERROR
458500051110     C                   EVAL      gcfdio74 = 'N'
458600051110     C                   ENDIF
458700051110
458800051110     C                   ENDSR
458900051222
459000051222     ***********************************************************************************************
459100051222     **
459200051222     ** Linea arrivo Italia o estera.
459300051222     **
459400051222     ***********************************************************************************************
459500051222     C     setLnaItaEst  BEGSR
459600051222
459700051222     C                   RESET                   wrkLnaItaEst
459800051222     C                   EXSR      decOrg
459900051222     C                   EVAL      wrkLnaItaEst = wofl1
460000051222
460100051222     C                   ENDSR
460200060622
460300060622     ***********************************************************************************************
460400150615     ** Reperisco informazioni stato bolla
460500060622     ***********************************************************************************************
460600130312     C     srarb         BEGSR
460700130312     c                   clear                   wcev
460800140714     c                   clear                   wricons
460900130312     c                   clear                   worg
461000130312     c                   clear                   wdat
461100130312     c                   clear                   wora
461200150615     c                   clear                   ds2a
461300130312     c                   eval      dsevbspe = spe(1)
461400130312     c     karb          chain     fnarb01l
461500130312     c                   if        not %error and
461600130312     c                             %found(fnarb01l)
461700130422     c                   Z-ADD     arblna        worg
461800130312     c                   exsr      setLnaItaEst
461900130312     c* no per estero
462000130312     C                   IF        wrkLnaItaEst = ESTERO
462100130312     c                   leavesr
462200130312     c                   end
462300150617     c
462400130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
462500130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
462600130312     C                   SELECT
462700141120     c                   WHEN      arbdcm <> 0
462800130312     c                   MOVEL     '704'         wcev                           *causale evento
462900130313     c                   move      arbdcm        WDAT
463000130313     c                   move      arbhmc        WORA
463100150617     c
463200130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
463300150615     c                   WHEN      ARBFDC = ' ' and arbndc>0
463400130326     c* solo se distinta NON in test
463500130326     c                   clear                   ddstflr
463600130326     c                   z-add     arbifp        dstfgs
463700130326     c                   z-add     arbndc        dstnfv
463800130326     c     kdst          chain     fidst01l
463900130326     c                   if        not %error and
464000130326     c                             %found(fidst01l)
464100130326     c                   eval      ddstflr = dstflr
464200130326     c                   end
464300130327     c                   if        §DSTTSTPDA = 'C' or
464400130327     c                             §DSTTSTPDA = 'E'
464500130326     c                   leavesr
464600130326     c                   end
464700130326     c*
464800150617     c* Cerco PRIMO evento da PdA
464900130313     c                   move      arbndc        pctndc
465000131105     c                   exsr      cercaceP
465100150617     c*
465200150617     c* Cerco ULTIMO evento da PdA
465300131105     c                   move      'CET'         ttrd
465400130312     c     kcet          setgt     fipct02l
465500130312     c     kcet          readpe    fipct02l
465600130312     c                   if        not %error and
465700130312     c                             not %eof(fipct02l)
465800130312     c                   movel     pctdati       fipctcetds
465900140926      * firmatario se c'è riempie il campo
466000140926     c                   if        §pctnomfir <> *blank
466100140926     c                   eval      firmato74  = §pctnomfir
466200140926     c                   endif
466300130312     c* consegnata
466400130312     c                   if        §pctcmc = ' '
466500130312     c                   MOVEL     '704'         wcev                           *causale evento
466600130312     c                   else
466700140711     c                   move      'S'           wricons
466800130312     c* mancata consegna
466900130313     c                   movel     §PCTcmc       wcev
467000130312     c                   clear                   ds2a
467100130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
467200130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
467300130312     C                             :rpyEsito)
467400130312     c                   if        §2aflg = '§' and §2acpt = 'S'
467500130312     c                   move      'T  '         wcev
467600130312     c                   endif
467700130312     c                   endif
467800130312     c* ??? £6
467900130312     c                   if        §pctdata > 0
468000130313     c                   move      §PCTDATA      WDAT
468100130312     c                   movel     §PCTORA       WORA
468200130312     c                   endif
468300130312     c                   end
468400130312     c                   endSL
468500130312     c                   end
468600130312     c* ho trovato un evento da inserire  al 1° posto
468700130312     c                   if        wcev <> ' '
468800130312     c                   EXSR      deccevsta
468900130312     c                   IF        werr <> '1'                                  *no errore
469000130312     c                   EXSR      decorg
469100130312     c                   EXSR      edtdat
469200130312     c                   EXSR      edtora
469300130313     c* libero il 1° elemnto di schiera
469400150617     c                   eval      j = 49
469500150617     c                   for       j = 49 downto 1
469600130312     c                   if        cev(j) <> ' '
469700130312     c                   eval      cev(j+1) = cev(j)
469800130312     c                   eval      dev(j+1) = dev(j)
469900130312     c                   eval      hev(j+1) = hev(j)
470000130312     c                   eval      fle(j+1) = fle(j)
470100130312     c                   eval      eve(j+1) = eve(j)
470200130312     c                   end
470300130313     c                   endfor
470400130313     c                   eval      dev(1) = wdate
470500130313     c                   eval      hev(1) = worae
470600130312     c                   eval      cev(1)= wcev
470700130313     c                   eval      fle(1) = wodes
470800130313     c                   eval      eve(1) = wcdex
470900130313     c                   EVAL      cntEveO74 += 1
471000150310     c*
471100150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
471200150310     c                   if        §2aftc='A'
471300150310     c                   eval      pdaavv='S'
471400150310     c                   endif
471500150421     c                   if        §2aftc='G'
471600150421     c                   eval      pdagia='S'
471700150421     c                   endif
471800150310     c
471900130312     c                   end
472000130312     c                   end
472100130312     C                   ENDSR
472200140714     **_________________________________________________________
472300140714     c     cercafiglia   begsr
472400140714     **_________________________________________________________
472500140715     c                   clear                   keyForzata
472600140714      * salva chiave originale per eventi
472700140715     c                   eval      kasaasf = kasaas
472800140715     c                   eval      kaslnpf = kaslnp
472900140715     c                   eval      kasnrsf = kasnrs
473000140715     c                   eval      kasnspf = kasnsp
473100140714     c                   do        *hival
473200140715     c     keytasf       chain     fnlbl02l
473300140714     c                   if        %found(fnlbl02l)
473400140715     c                   eval      kasaasf = LBLaan
473500140715     c                   eval      kaslnpf = LBLlpn
473600140715     c                   eval      kasnrsf = LBLnrn
473700140715     c                   eval      kasnspf = LBLnsn
473800140714     c                   move      'X'           keyForzata
473900140714     c                   iter
474000140714     c                   else
474100140714     c                   leave
474200140714     c                   endif
474300140714     c                   enddo
474400140715     c     keytasf       setll     titas30c                               99    *no errore
474500140715     c     keytasf       READE     titas30c                               99    *no errore
474600141117      *se non trova titas con figlia forza di nuovo la mamma
474700140715if  3c                   IF        *in99                                        *non trovato
474800141117     c                   clear                   titasdssavf
474900140715x   3c                   ELSE                                                   *trovata
475000140715if  4c                   IF        *in01                                        *lettura titas000
475100140715     c                   EVAL      titasdssavf = titasds
475200140715e   4c                   ENDIF
475300140715if  4c                   IF        *in02                                        *lettura titas010
475400140715     c                   EVAL      titasdssavf = titasds
475500140715e   4c                   ENDIF
475600140715if  4c                   IF        *in03                                        *lettura titasp00
475700140715     c                   EVAL      titasdssavf = titasds
475800140715e   4c                   ENDIF
475900140715e   3c                   ENDIF
476000140714     C                   ENDSR
476100160318     **_________________________________________________________
476200160318     c     cercamamma    begsr
476300160318     **_________________________________________________________
476400160318     c                   clear                   keyForzata
476500160318     c                   EVAL      kblaan = arbaas
476600160318     c                   EVAL      kbllpn = arblnp
476700160318     c                   EVAL      kblnrn = arbnrs
476800160318     c                   EVAL      kblnsn = arbnsp
476900160318     c     keylbl        CHAIN     fnlbl01l                           99
477000160318if  1c                   IF        NOT *in99                                    *ha la mamma
477100160318     c                   EVAL      kasaas = lblaap
477200160318     c                   EVAL      kaslnp = lbllpp
477300160318     c                   EVAL      kasnrs = lblnrp
477400160318     c                   EVAL      kasnsp = lblnsp
477500160318     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
477600160318if  3c                   IF        not *in99                                        *non trovato
477700160318     c                   move      'X'           keyForzata
477800160318     c                   eval      esito=' '
477900160408     c                   clear                   titasdssavf
478000160318     c                   endif
478100160318     c                   endif
478200160318     c
478300160318     c                   ENDSR
478400131105     **_________________________________________________________
478500131105     c     cercaCEP      begsr
478600131105     **_________________________________________________________
478700131105     c                   move      'CEP'         ttrd
478800131105     c     kcet          setgt     fipct02l
478900131105     c     kcet          readpe    fipct02l
479000131105     c                   if        not %error and
479100131105     c                             not %eof(fipct02l)
479200131105     c                   movel     pctdati       fipctcetds
479300131105     c* consegnata
479400131105     c                   if        §pctcmc = ' '
479500131105     c                   MOVEL     '704'         wcev                           *causale evento
479600131105     c                   else
479700131105     c* mancata consegna
479800131105     c                   movel     §PCTcmc       wcev
479900131105     c                   clear                   ds2a
480000131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
480100131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
480200131105     C                             :rpyEsito)
480300131105     c                   if        §2aflg = '§' and §2acpt = 'S'
480400131105     c                   move      'T  '         wcev
480500131105     c                   endif
480600131105     c                   endif
480700131105     c* ??? £6
480800131105     c                   if        §pctdata > 0
480900131105     c                   move      §PCTDATA      WDAT
481000131105     c                   movel     §PCTORA       WORA
481100131105     c                   endif
481200131105     c                   end
481300131105     c* ho trovato un evento da inserire  al 1° posto
481400131105     c                   if        wcev <> ' '
481500131105     c                   EXSR      deccevsta
481600131105     c                   IF        werr <> '1'                                  *no errore
481700131105     c                   EXSR      decorg
481800131105     c                   EXSR      edtdat
481900131105     c                   EXSR      edtora
482000131105     c* libero il 1° elemnto di schiera
482100150617     c                   eval      j = 49
482200150617     c                   for       j = 49 downto 1
482300131105     c                   if        cev(j) <> ' '
482400131105     c                   eval      cev(j+1) = cev(j)
482500131105     c                   eval      dev(j+1) = dev(j)
482600131105     c                   eval      hev(j+1) = hev(j)
482700131105     c                   eval      fle(j+1) = fle(j)
482800131105     c                   eval      eve(j+1) = eve(j)
482900131105     c                   end
483000131105     c                   endfor
483100131105     c                   eval      dev(1) = wdate
483200131105     c                   eval      hev(1) = worae
483300131105     c                   eval      cev(1)= wcev
483400131105     c                   eval      fle(1) = wodes
483500131105     c                   eval      eve(1) = wcdex
483600131105     c                   EVAL      cntEveO74 += 1
483700131105     c                   end
483800131105     c                   end
483900131105     c                   endsr
484000131105     ***********************************************************************************************
484100130312     **
484200130312     ** Reperisco tabella da TNTBE00F.
484300130312     **
484400130312     ***********************************************************************************************
484500130312     C     getTntbe00f   BEGSR
484600060622     C                   EVAL      t02Lin = langTntbe
484700060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
484800060622     C                   SELECT
484900060622     C                   WHEN      t02Cod = 'ICE'
485000060622     C                   EVAL      dice = t02Uni
485100060622     C                   WHEN      t02Cod = 'CCH'
485200060622     C                   EVAL      dcch = t02Uni
485300060622     C                   WHEN      t02Cod = 'NTW'
485400060622     C                   EVAL      dntw = t02Uni
485500100119     C                   WHEN      t02Cod = 'I1A'
485600100119     C                   EVAL      dI1a = t02Uni
485700150304     C                   WHEN      t02Cod = 'SPC'
485800150304     C                   EVAL      dSPC = t02Uni
485900060622     C                   ENDSL
486000060622     C                   ENDSR
486100060608
486200140529      /free
486300140529       //--------------------------------------------------------------
486400140529       //?Controllo formale dei dati passati.
486500140529       //--------------------------------------------------------------
486600140603       BEGSR  Kontrolla;
486700140529
486800140529       //?OpCode
486900140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
487000140529           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
487100140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
487200140529           exsr RoutEnd;
487300140529         ENDIF;
487400140603           IF  prmRqsDataSize > 0;
487500140603           ELSE;
487600140603             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
487700140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
487800140603             exsr RoutEnd;
487900140603           ENDIF;
488000140529
488100140529       //?Formato e size RQS
488200140603           IF  prmRqsFormato = formatoi74;
488300140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
488400140529           ELSE;
488500140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
488600140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
488700140529             exsr RoutEnd;
488800140529           ENDIF;
488900140529         //?Formato e size RPY
489000140603           IF  prmRpyFormato = formatoo74;
489100140529           ELSE;
489200140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
489300140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
489400140529             exsr RoutEnd;
489500140529           ENDIF;
489600140529           IF  prmRpyDataSize > 0;
489700140529           ELSE;
489800140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
489900140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
490000140529             exsr RoutEnd;
490100140529           ENDIF;
490200140616       //?Formato e size MSG
490300140617         if %parms > 9 ;
490400140617          IF  prmRpyFormMsg = formato;
490500140617          ELSE;
490600140616           prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
490700140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
490800140616           exsr RoutEnd;
490900140617          ENDIF;
491000140617          IF  prmRpyMsgSize > 0;
491100140617          ELSE;
491200140616           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
491300140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
491400140616           exsr RoutEnd;
491500140617          ENDIF;
491600140617         ENDIF;
491700140529
491800140529       ENDSR;
491900140529
492000140616       //--------------------------------------------------------------
492100140617       //?Routine per schierare i messaggi nella ds tis778dsm
492200140616       //--------------------------------------------------------------
492300140616       BEGSR  Messaggi;
492400140616
492500140616       //?Se ho già caricato 99 messaggi esco
492600140616         IF  nrmsg = 99;
492700140616           exsr RoutEnd;
492800140616           clear nrmsg;
492900140616         ENDIF;
493000140616
493100140616         nrmsg += 1;
493200140616         skIdMsg(nrmsg) = wIdMsg;
493300140616         skIdCampo(nrmsg) = wIdCampo;
493400140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
493500140616         IF  wParm <> *blanks;
493600140616           skTextMsg (nrmsg) =
493700140616           rtvMsgLang(wIdMsg:langi74:wParm);
493800140616         ELSE;
493900140616           skTextMsg (nrmsg)=
494000140616           rtvMsgLang(wIdMsg:langi74);
494100140616         ENDIF;
494200140616
494300140616       ENDSR;
494400140616
494500140529       //--------------------------------------------------------------
494600140529       BEGSR  RoutEnd;
494700140530
494800140530
494900160322        if  esito <> '0' and esito<>'P';
495000140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
495100140530             else;
495200140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
495300140530        endif ;
495400140529
495500140530       //?ritorno
495600140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
495700140617         if %parms > 9 ;
495800140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
495900140617         endif;
496000140529         return;
496100140529
496200140529       ENDSR;
496300140529      /end-free
496400000000     c*--------------------------------------------------------------------------------------------*
496500000000     c* operazioni iniziali
496600000000     c*--------------------------------------------------------------------------------------------*
496700000000     c     *inzsr        BEGSR
496800000000     c*
496900000000     c* ricevimento parametri
497000140603     c*m   *ENTRY        PLIST
497100140529     c*m                 PARM                    esito
497200140529     c*m                 PARM                    tis174dsi
497300140529     c*m                 PARM                    tis174dso
497400000000     c*
497500000000     c* chiavi di lettura
497600000000     c     keytas        KLIST                                                  titas30c
497700000000     c                   KFLD                    kasaas                         -anno spedizione
497800000000     c                   KFLD                    kaslnp                         -linea partenza
497900000000     c                   KFLD                    kasnrs                         -serie
498000000000     c                   KFLD                    kasnsp                         -spedizione
498100140715     c     keytasf       KLIST                                                  titas30c
498200140715     c                   KFLD                    kasaasf                        -anno spedizione
498300140715     c                   KFLD                    kaslnpf                        -linea partenza
498400140715     c                   KFLD                    kasnrsf                        -serie
498500140715     c                   KFLD                    kasnspf                        -spedizione
498600130312     c     karb          KLIST                                                  titas30c
498700140716     c                   KFLD                    f_tasaas                         -anno spedizione
498800140716     c                   KFLD                    f_taslnp                         -linea partenza
498900140716     c                   KFLD                    f_tasnrs                         -serie
499000140716     c                   KFLD                    f_tasnsp                         -spedizione
499100130326     C     Kdst          KLIST
499200130326     C                   KFLD                    dstnpg
499300130326     C                   KFLD                    dstnfv
499400130326     C                   KFLD                    dstfgs
499500130326     c                   z-add     4             dstnpg
499600130326     C     Kcet          KLIST
499700130326     C                   KFLD                    arbLNA
499800130326     C                   KFLD                    pctNDC
499900130326     C                   KFLD                    arbpdc
500000131105     C                   KFLD                    TTRD              3
500100130312     C                   KFLD                    arbAAS
500200130312     C                   KFLD                    arbLNP
500300130312     C                   KFLD                    arbNRS
500400130312     C                   KFLD                    arbNSP
500500150403     C     KARP          KLIST
500600150403     C                   KFLD                    arbAAS
500700150403     C                   KFLD                    arbLNP
500800150403     C                   KFLD                    arbNRS
500900150403     C                   KFLD                    arbNSP
501000000000     c     keytaa        KLIST                                                  titaa30c
501100000000     c                   KFLD                    kaaaas                         -anno spedizione
501200000000     c                   KFLD                    kaalnp                         -linea partenza
501300000000     c                   KFLD                    kaanrs                         -serie
501400000000     c                   KFLD                    kaansp                         -spedizione
501500000000     c                   KFLD                    kaatrc                         -tipo anagrafica
501600000000     c     keyta4        KLIST                                                  tita430c
501700000000     c                   KFLD                    ka4aas                         -anno spedizione
501800000000     c                   KFLD                    ka4lnp                         -linea partenza
501900000000     c                   KFLD                    ka4nrs                         -serie
502000000000     c                   KFLD                    ka4nsp                         -spedizione
502100000000     c                   KFLD                    ka4trc                         -tipo riferimento
502200010925     c     ke2ta4        KLIST                                                  tita430c
502300010925     c                   KFLD                    ka4aas                         -anno spedizione
502400010925     c                   KFLD                    ka4lnp                         -linea partenza
502500010925     c                   KFLD                    ka4nrs                         -serie
502600010925     c                   KFLD                    ka4nsp                         -spedizione
502700000000     c     keyevb        KLIST                                                  fnevb01l
502800000000     c                   KFLD                    kvbaas                         -anno spedizione
502900000000     c                   KFLD                    kvblnp                         -linea partenza
503000000000     c                   KFLD                    kvbnrs                         -serie
503100000000     c                   KFLD                    kvbnsp                         -spedizione
503200010717     c     ke2evb        KLIST                                                  fnevb01l
503300010717     c                   KFLD                    kvbaas                         -anno spedizione
503400010717     c                   KFLD                    kvblnp                         -linea partenza
503500010717     c                   KFLD                    kvbnrs                         -serie
503600010717     c                   KFLD                    kvbnsp                         -spedizione
503700010717     c                   KFLD                    kvbdev                         -data evento
503800010717     c                   KFLD                    kvbhev                         -ora  evento
503900000000     c     keylbl        KLIST                                                  fnlbl01l
504000000000     c                   KFLD                    kblaan                         -anno seguente
504100000000     c                   KFLD                    kbllpn                         -linea seguente
504200000000     c                   KFLD                    kblnrn                         -serie seguente
504300000000     c                   KFLD                    kblnsn                         -spedizione seguente
504400000000     c     ke2lbl        KLIST                                                  fnlbl02l
504500000000     c                   KFLD                    kblaap                         -anno precedente
504600000000     c                   KFLD                    kbllpp                         -linea precedente
504700000000     c                   KFLD                    kblnrp                         -serie precedente
504800000000     c                   KFLD                    kblnsp                         -spediz.precedente
504900000000     c     keytbe        KLIST                                                  *tntbe01l
505000000000     c                   KFLD                    kbecod                          -tabella
505100000000     c                   KFLD                    kbeke1                          -chiave uno
505200000000     c                   KFLD                    kbeke2                          -chiave due
505300000000     c                   KFLD                    kbelin                          -lingua
505400000000     c                   KFLD                    kbesif                          -s.informativo
505500000000     c     keyvss        KLIST                                                  *tivss02l
505600000000     c                   KFLD                    kssksu                          -cl.unificante
505700000000     c                   KFLD                    kssisv                          -tipo servizio
505800010717     c     keyvtd        KLIST                                                  *tivtd01l
505900010717     c                   KFLD                    ktdksu                          -cl.unificante
506000000000     c     keycsb        KLIST                                                  *tncsb03l
506100000000     c                   KFLD                    ksbaas                         -anno spedizione
506200000000     c                   KFLD                    ksblnp                         -linea partenza
506300000000     c                   KFLD                    ksbnrs                         -serie
506400000000     c                   KFLD                    ksbnsp                         -spedizione
506500040906     c     keydct        KLIST
506600040906     c                   KFLD                    DCTAAC
506700040906     c                   KFLD                    DCTFIL
506800040906     c                   KFLD                    DCTNCA
506900000000     c     keygcp        KLIST                                                  *figcp01l
507000000000     c                   KFLD                    kcpaas                         -anno spedizione
507100000000     c                   KFLD                    kcplnp                         -linea partenza
507200000000     c                   KFLD                    kcpnrs                         -serie
507300000000     c                   KFLD                    kcpnsp                         -spedizione
507400000000     c                   KFLD                    kcpfrg                         -prg aperture
507500050404     c     K04GCP51      KLIST
507600050404     c                   KFLD                    kcpaas                         -anno spedizione
507700050404     c                   KFLD                    kcplnp                         -linea partenza
507800050404     c                   KFLD                    kcpnrs                         -serie
507900050404     c                   KFLD                    kcpnsp                         -spedizione
508000000000     c     keygnp        KLIST                                                  *fignp01l
508100000000     c                   KFLD                    knpagc                         -anno apertura
508200000000     c                   KFLD                    knpfgc                         -filiale apertura
508300000000     c                   KFLD                    knpngc                         -numero giacenza
508400000000     c                   KFLD                    knpfrg                         -prg apertura
508500040531     c     keyar5        KLIST                                                  *fiar531c
508600030206     c                   KFLD                    kr5aas                         -anno spedizione
508700030206     c                   KFLD                    kr5lnp                         -linea partenza
508800030206     c                   KFLD                    kr5nrs                         -serie
508900030206     c                   KFLD                    kr5nsp                         -spedizione
509000030206     c                   KFLD                    kr5trd                         -tipo record
509100040907
509200000000     c* apre i files
509300000000     c                   OPEN      titas30c
509400000000     c                   OPEN      titaa30c
509500000000     c                   OPEN      tita430c
509600000000     c                   OPEN      fnevb01l
509700000000     c                   OPEN      tivss02l
509800010716     c                   OPEN      tivtd01l
509900000000     c                   OPEN      azorg01l
510000000000     c                   OPEN      fnlbl01l
510100000000     c                   OPEN      fnlbl02l
510200050309     c                   OPEN      tigcp51l
510300050309     c                   OPEN      tignp01l
510400000000     c                   OPEN      tncsb03l
510500040906     c                   OPEN      fndct01l
510600040531     c                   OPEN      fiar531c
510700130312     c                   OPEN      fnarb01l
510800160127     c                   OPEN      fnblp01l
510900160309     c                   OPEN      fiar901l
511000150403     c                   OPEN      fiarp01l
511100140606     c                   OPEN      fiar501l
511200140606     c                   OPEN      fiar401l
511300130326     c                   OPEN      fidst01l
511400130312     c                   OPEN      fipct02l
511500060621     C                   CALLP     openTabel00f()
511600000000     c*
511700000000     c* caricamento tabelle occorrenti
511800000000     c                   EXSR      cartab
511900060621     C                   CALLP     inzLingue()
512000000000     c*
512100000000     c                   ENDSR
512200070109
512300070109     ***********************************************************************************************
512400070109     **
512500070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
512600070109     **
512700070109     ***********************************************************************************************
512800070109     C     eventoAnnulla BEGSR
512900070109
513000070109     C                   IF        §iceEvAn <> *BLANK
513100070109     C                   FOR       j = 1 TO %ELEM(smCev)
513200070109     C                   IF        §iceEvAn = smCev(j) AND
513300070109     C                             evbDevHev = smDevHev(j)
513400070109     C                   CLEAR                   smCev(j)
513500130312     C                   CLEAR                   smspe(j)
513600070109     C                   CLEAR                   smDevHev(j)
513700070109     C                   CLEAR                   smi(j)
513800070109     C                   CLEAR                   smSeql(j)
513900070109     C                   CLEAR                   smSeqe(j)
514000070109     C                   CLEAR                   smDeve(j)
514100070109     C                   CLEAR                   smHeve(j)
514200070109     C                   CLEAR                   smFle(j)
514300070109     C                   CLEAR                   smDev(j)
514400070109     C                   LEAVE
514500070109     C                   ENDIF
514600070109     C                   ENDFOR
514700070109     C                   ENDIF
514800070109
514900070109     C                   ENDSR
515000080131
515100100119     P*--------------------------------------------------
515200100119     P* Procedure name: GetDescrizioneIncassoContrassegno
515300100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
515400100119     P*                          o del contrassegno.
515500100121     P* Returns:        Descrizione modalità incasso contrassegno
515600100119     P*--------------------------------------------------
515700100119     P GetDescrizioneIncassoContrassegno...
515800100119     P                 B
515900100119     D GetDescrizioneIncassoContrassegno...
516000100119     D                 PI            35A
516100100119
516200100121     C                   RESET                   tibs02ds
516300100121     C                   EVAL      t02Cod = 'I1A'
516400100121     C                   EVAL      t02Lin = langTntbe
516500100119     ** Il contrassegno è stato incassato
516600100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
516700100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
516800100121     C                   ELSE
516900100119     ** Il contrassegno non è stato incassato.
517000100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
517100100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
517200100119     C                             ar5Trd <> 'GEN'
517300100121     C                   EVAL      kr5Aas = csbAas
517400100121     C                   EVAL      kr5Lnp = csbLnp
517500100121     C                   EVAL      kr5Nrs = csbNrs
517600100121     C                   EVAL      kr5Nsp = csbNsp
517700100119     C                   EVAL      kr5Trd = 'GEN'
517800160314     c                   if        esito='P'
517900160314     C     keyAr5        CHAIN     fiar501l
518000160314     c                   else
518100160314     C     keyAr5        CHAIN     fiar531c
518200160314     c                   endif
518300160314     c
518400100119     C                   IF        %FOUND()
518500100119     C                   EVAL      dAr5Gen = ar5Uni
518600100119     C                   ELSE
518700100119     C                   RESET                   dAr5Gen
518800100119     C                   RETURN                  *BLANK
518900100119     C                   ENDIF
519000100119     C                   ENDIF
519100100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
519200100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
519300100119     C                   ELSE
519400100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
519500100119     C                   ENDIF
519600100121     C                   ENDIF
519700100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
519800100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
519900100119     C                   IF        t02Err = 'E'
520000100119     C                   RETURN                  *BLANK
520100100119     C                   ENDIF
520200100120     C                   EVAL      dI1a = t02Uni
520300140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
520400140417     C                   RETURN                  dI1a.descrizi2
520500140417     C                   ENDIF
520600100119     C                   RETURN                  dI1a.descrizion
520700100119
520800100119     P GetDescrizioneIncassoContrassegno...
520900100119     P                 E
521000100119
521100110304
521200110304     P*--------------------------------------------------
521300110304     P* Procedure name: IsContrassegnoCompensato
521400110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
521500110304     P*                          to con una fattura.
521600110304     P* Returns:        *ON = Compensato
521700110304     P*--------------------------------------------------
521800110304     P IsContrassegnoCompensato...
521900110304     P                 B
522000110304     D IsContrassegnoCompensato...
522100110304     D                 PI              N
522200110304
522300110304     D retField        S               N   INZ(*OFF)
522400110304
522500110307     ** Il contrassegno non è stato rimborsato.
522600110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
522700110307     C                   RETURN    retField
522800110307     C                   ENDIF
522900110307
523000110307     C/EXEC SQL
523100110307     C+ SELECT '1'
523200110307     C+ INTO :retField
523300110307     C+ FROM tncsv00f
523400110307     C+ WHERE csvAas = :csbAas
523500110307     C+   AND csvLnp = :csbLnp
523600110307     C+   AND csvNrs = :csbNrs
523700110307     C+   AND csvNsp = :csbNsp
523800110307     C+   AND csvCav = 'COMP'
523900110307     C+ FETCH FIRST ROW ONLY
524000110307     C/END-EXEC
524100110304
524200110307     C                   SELECT
524300110307     C                   WHEN      sqlCode < 0
524400110307     C                   DUMP(A)
524500110307     C                   ENDSL
524600110304
524700110307     C                   RETURN    retField
524800110304
524900110304     P IsContrassegnoCompensato...
525000110304     P                 E
525100110304
525200120321
525300120321     P*--------------------------------------------------
525400120321     P* Procedure name: SetIdAssegnoMittente
525500120321     P* Purpose:        Imposta l'ID assegno mittente.
525600120321     P* Returns:        Esito.
525700120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
525800120321     P*                          segni mittente.
525900120321     P* Parameter:      priAbi => ABI assegno.
526000120321     P* Parameter:      priCab => CAB assegno.
526100120321     P*--------------------------------------------------
526200120321     P SetIdAssegnoMittente...
526300120321     P                 B
526400120321     D SetIdAssegnoMittente...
526500120321     D                 PI            10I 0
526600120321     D  priNumeroAssegno...
526700120321     D                                     LIKE(caNumO74)
526800120321     D  priAbi                             LIKE(caAbiO74)
526900120321     D  priCab                             LIKE(caCabO74)
527000120321
527100120321     D retField        S             10I 0 STATIC
527200120321
527300120321      /FREE
527400120321
527500120321       CLEAR retField;
527600120321
527700120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
527800120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
527900120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
528000120321       // In questo caso restituisco l'ID del primo assegno.
528100120321
528200120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
528300120321         RETURN retField;
528400120321       ENDIF;
528500120321
528600120321       EXEC SQL
528700120321         SELECT csmNra, csmAbi, csmCab
528800120321           INTO :priNumeroAssegno, :priAbi, :priCab
528900120321           FROM tncsm00f
529000120321           WHERE csmKey = :priNumeroAssegno
529100120321           FETCH FIRST 1 ROW ONLY
529200120321       ;
529300120321
529400120321       SELECT;
529500120321         WHEN sqlCode = 100;
529600120321           retField = sqlCode;
529700120321         WHEN sqlCode < *ZERO;
529800120321           DUMP(A);
529900120321           retField = sqlCode;
530000120321       ENDSL;
530100120321
530200120321       RETURN retField;
530300120321
530400120321      /END-FREE
530500120321     P SetIdAssegnoMittente...
530600120321     P                 E
530700120321
530800130513
530900130513     P*--------------------------------------------------
531000130513     P* Procedure name: GetFilUrl
531100130513     P* Purpose:        Restituisce l'URL della filiale.
531200130513     P* Returns:        URL filiale.
531300130513     P* Parameter:      piFiliale => ID filiale
531400130513     P*--------------------------------------------------
531500130513     P GetFilUrl       B
531600130513     D GetFilUrl       PI           256A
531700130513     D  piFiliale                     3P 0 CONST
531800130513
531900130513     C                   IF        piFiliale = *ZERO
532000130513     C                   RETURN    *BLANK
532100130513     C                   ENDIF
532200130513     C                   RESET                   tibs02ds
532300130513     C                   EVAL      t02Cod = 'UFI'
532400130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
532500130513     C                   EVAL      t02Lin = langTntbe
532600130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
532700130513     C                   IF        t02Err = 'E'
532800130513     C                   RETURN    *BLANK
532900130513     C                   ENDIF
533000130513     C                   RETURN    t02Uni
533100130513
533200130513     P GetFilUrl       E
533300130513
