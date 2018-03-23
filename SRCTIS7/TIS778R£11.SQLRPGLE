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
119500160818     C******             EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
119600160818     c                   eval      esito='1'
119700160818     c                   eval      widmsg = 'TIS0849'
119800160818     c                   eval      widcampo = 'NSPEDIZI74'
119900160818     c                   clear                   wparm
120000160818     c                   exsr      messaggi
120100160818     c                   exsr      routend
120200160818     C******             RETURN
120300140604     C                   ENDIF
120400140604e   2c                   ENDIF
120500140604     C                   ENDIF
120600140604     c*
120700140604     ** Importo assicurato.
120800140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
120900140716     C                   EVAL      vasO74 = f_tasVas
121000141015     C                   EVAL      iasO74 = f_tasIas
121100140604     C                   ENDIF
121200140604     **
121300140604     c                   ENDSR
121400000000     c*--------------------------------------------------------------------------------------------*
121500000000     c* imposta i campi della DS di Output dagli eventi
121600000000     c*--------------------------------------------------------------------------------------------*
121700000000     c     impdsoevb     BEGSR
121800000000     c*
121900000000     c* azzera le variabili di lavoro
122000000000     c                   CLEAR                   tote                           *contatore eventi
122100000000     c                   CLEAR                   seql                           *sequenza legami
122200000000     c                   CLEAR                   spsort                         *per ordinamento
122300000000     c                   CLEAR                   spseql                         *di memorizzazione
122400000000     c                   CLEAR                   spseqe
122500000000     c                   CLEAR                   spdevhev
122600000000     c                   CLEAR                   spdeve
122700000000     c                   CLEAR                   spheve
122800000000     c                   CLEAR                   spfle
122900000000     c                   CLEAR                   spdev
123000000000     c                   CLEAR                   spcev
123100130312     c                   CLEAR                   spspe
123200150618     c                   CLEAR                   ds7o
123300000000     c*
123400000000     c* imposta la sequenza legami per la bolla di Input
123500000000     c                   Z-ADD     1             seql                           *primo legaqme
123600000000     c*---
123700000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
123800000000     c*---
123900160127     c****
124000160127     c* Bolla presa in carico se ancora da partire
124100160420     c**                 if        esito='P'
124200160420     c**                 MOVEL     '700'         wcev                           *causale evento
124300160420     c**                 EXSR      deccevsta
124400160420if  4c**                 IF        werr <> '1'                                  *no errore
124500160420     c**                 ADD       1             tote
124600160420     c**                 MOVEL     wcdex         smdev(tote)
124700160420     c**                 exsr      evbinca
124800160420     c**                 endif
124900160420     c**                 endif
125000000000     c***
125100000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
125200000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
125300000000     c*   si trasforma nella consegna anomala della mamma
125400000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
125500000000     c***
125600000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
125700000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
125800160318     c
125900160318    0c                   if        esito<>'P'
126000000000     c                   EVAL      kblaan = s_tasaas
126100000000     c                   EVAL      kbllpn = s_taslnp
126200000000     c                   EVAL      kblnrn = s_tasnrs
126300000000     c                   EVAL      kblnsn = s_tasnsp
126400000000     c     keylbl        CHAIN     fnlbl01l                           99
126500000000if  1c                   IF        NOT *in99                                    *ha la mamma
126600000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
126700000000     c                   EVAL      kasaas = lblaap
126800000000     c                   EVAL      kaslnp = lbllpp
126900000000     c                   EVAL      kasnrs = lblnrp
127000000000     c                   EVAL      kasnsp = lblnsp
127100000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
127200000000if  2c                   IF        NOT *in99 AND
127300000000     c                             tascca <> *blanks                            *ha cons.anomala
127400060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
127500060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
127600060622     C                             :rpyEsito)
127700060622if  3c*                  IF        *in98                                        *trovata
127800060626     C                   IF        rpyOpCode = 'FOUND'
127900041222     **?Consegna anomala, POD non visualizzabile.
128000060622     C*                  IF        sapod(jj) = 'N'
128100060622     C                   IF        §7OPODImg = 'N'
128200041222     C                   EVAL      LDVO74 = 'N'
128300041222     C                   ENDIF
128400060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
128500060622     C                   IF        §7oinc <> *blanks                            *ok per internet
128600000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
128700041222e   3c                   ENDIF
128800000000e   3c                   ENDIF
128900000000e   2c                   ENDIF
129000000000e   1c                   ENDIF
129100160318e   0c                   ENDIF
129200000000     c*
129300030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
129400000000     c                   ADD       1             tote
129500060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
129600060622     c                   MOVEL     §7odei        smdev(tote)
129700030203x   3c                   ELSE                                                   *evento telef.P.O.
129800060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
129900030203e   3c                   ENDIF
130000030203e   2c                   ENDIF
130100030203     c*
130200030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
130300060622     c                   MOVEL     §7ocev        wcev                           *causale evento
130400000000     c                   EXSR      evbrit                                       *imposta evento
130500030203x   2c                   ELSE                                                   *NO sost.ritiro
130600160318     c
130700160408     c* per bolle in partenza devo impostare la data ritiro solo se spuntato in entrata
130800160408     c***                if        esito<>'P'
130900160413     c                   if        esito= 'P'
131000160413     c                   if        arbdse>0
131100160408     c                   eval      s_tasdrt=arbdrt
131200160413     c                   else
131300160413     c                   eval      s_tasdrt=00000000
131400160408     c                   endif
131500160413     c                   endif
131600160318     c
131700030203if  3c                   IF        s_tasdrt > *zeros
131800000000     c                   MOVEL     '701'         wcev                           *causale evento
131900000000     c                   EXSR      deccevsta
132000030203if  4c                   IF        werr <> '1'                                  *no errore
132100000000     c                   ADD       1             tote
132200030130     c                   MOVEL     wcdex         smdev(tote)
132300000000     c                   EXSR      evbrit                                       *imposta evento
132400030203e   4c                   ENDIF
132500030203e   3c                   ENDIF
132600030203e   2c                   ENDIF
132700160408e   2c***                ENDIF
132800140710     ** ID bolla madre.                                                          dubito figlia
132900160318     c                   if        esito='P'
133000160318     c                   EVAL      waas = arbaas
133100160318     c                   EVAL      wlnp = arblnp
133200160318     c                   EVAL      wnrs = arbnrs
133300160318     c                   EVAL      wnsp = arbnsp
133400160318     c                   else
133500160318     c
133600160318     c* questi dati per le bolle solo in partenza (non legate)
133700160318     c*  non li devo caricare
133800080131     c                   EVAL      waas = s_tasaas
133900080131     c                   EVAL      wlnp = s_taslnp
134000080131     c                   EVAL      wnrs = s_tasnrs
134100080131     c                   EVAL      wnsp = s_tasnsp
134200000000     c* Partenza merce
134300000000     c                   EVAL      lduc = s_tasduc                              *data partenza
134400000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
134500000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
134600000000     c                   EXSR      evbpar
134700000000     c* Arrivo merce
134800110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
134900110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
135000140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
135100140723     c                             gen§ar5ArrDt <> *ZERO
135200140723     c                   MONITOR
135300140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
135400140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
135500140723     c                   ON-ERROR
135600140723     c                   ENDMON
135700140723     c                   ENDIF
135800100225     c                   EVAL      llna = s_taslna                              *linea arrivo
135900150617     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
136000090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
136100090826     c                   EXSR      evbarr
136200000000     c*
136300000000     c* Consegna merce
136400000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
136500000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
136600000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
136700000000     c                   EVAL      llna = s_taslna                              *linea arrivo
136800150618     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
136900000000     c                   EXSR      evbcon
137000000000     c* Lettera anomalia
137100000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
137200000000     c                   EXSR      memdct
137300000000e   1c                   ENDIF
137400000000     c*---
137500000000     c* memorizza gli eventi dal file eventi della bolla di input
137600000000     c*---
137700160406     c* devo reimpostare la linea di arrivo della bolla richiesta, perchè potrei
137800150618     c*  aver chainato una bolla "mamma" ed aver sporcato il campo
137900150618     c                   eval      taslna=s_taslna
138000000000     c                   EXSR      memevb
138100000000     c*---
138200000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
138300000000     c*---
138400000000     c                   EXSR      elaevelbl
138500160318     c                   endif
138600000000     c*
138700000000     c* imposta contatore eventi da visualizzare nella DS di Output
138800000000     c                   EVAL      cnteveo74 = tote
138900000000     c*---
139000000000     c* ordina gli eventi per sequenza legami / Data+ora evento
139100000000     c*---
139200000000     c* compone la schiera da ordinare
139300000000do  1c     1             DO        200           j
139400000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
139500000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
139600000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
139700000000     c                   Z-ADD     smi(j)        dsi                             -indice
139800000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
139900000000e   1c                   ENDDO
140000000000     c*
140100000000     c* ordina la schiera composta
140200000000     c                   SORTA     spsort
140300000000     c*
140400000000     c* ricompone le schiere da quella ordinata
140500000000     c                   Z-ADD     *zeros        jj
140600000000do  1c     1             DO        200           j
140700000000if  2c                   IF        spsort(j) >*zeros
140800000000     c                   ADD       1             jj
140900000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
141000000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
141100000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
141200000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
141300000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
141400000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
141500000000     c                   MOVEL     smheve(dsi)   spheve(jj)
141600000000     c                   MOVEL     smfle(dsi)    spfle(jj)
141700000000     c                   MOVEL     smdev(dsi)    spdev(jj)
141800000000     c                   MOVEL     smcev(dsi)    spcev(jj)
141900130312     c                   MOVEL     smspe(dsi)    spspe(jj)
142000000000e   2c                   ENDIF
142100000000e   1c                   ENDDO
142200101104     **
142300101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
142400101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
142500101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
142600101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
142700101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
142800101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
142900101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
143000101104     ** Ricordati che l'ordine per data e ora è discendente.
143100101104     C                   EVAL      j = 0
143200101104     C                   DOU       j = 0
143300101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
143400101104     C                                        : spcev : j+1)
143500101104     C                   IF        j = 0
143600101104     C                   LEAVE
143700101104     C                   ENDIF
143800101104     C                   IF        j > 1 AND
143900101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
144000101104     C                   EVAL      dev(1) = spdeve(j-1)
144100101104     C                   EVAL      hev(1) = spheve(j-1)
144200101104     C                   EVAL      fle(1) = spfle(j-1)
144300101104     C                   EVAL      eve(1) = spdev(j-1)
144400101104     C                   EVAL      cev(1) = spcev(j-1)
144500130312     C                   EVAL      spe(1) = spspe(j-1)
144600101104     C                   EVAL      spdeve(j-1) = spdeve(j)
144700101104     C                   EVAL      spheve(j-1) = spheve(j)
144800101104     C                   EVAL      spfle(j-1) = spfle(j)
144900101104     C                   EVAL      spdev(j-1) = spdev(j)
145000101104     C                   EVAL      spcev(j-1) = spcev(j)
145100130312     C                   EVAL      spspe(j-1) = spspe(j)
145200101104     C                   EVAL      spdeve(j) = dev(1)
145300101104     C                   EVAL      spheve(j) = hev(1)
145400101104     C                   EVAL      spfle(j) = fle(1)
145500101104     C                   EVAL      spdev(j) = eve(1)
145600101104     C                   EVAL      spcev(j) = cev(1)
145700130312     C                   EVAL      spspe(j) = spe(1)
145800101104     C                   ENDIF
145900101104     C                   ENDDO
146000000000     c*
146100120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
146200120831     c                   CLEAR                   cntEveO74
146300000000do  1c     1             DO        50            j
146400120831     c                   IF        spcev(j) = *BLANK
146500120831     c                   LEAVE
146600120831     c                   ENDIF
146700000000     c                   EVAL      dev(j) = spdeve(j)
146800000000     c                   EVAL      hev(j) = spheve(j)
146900000000     c                   EVAL      fle(j) = spfle(j)
147000041007     c                   EVAL      eve(j) = spdev(j)
147100041007     c                   EVAL      cev(j) = spcev(j)
147200130312     c                   EVAL      spe(j) = spspe(j)
147300101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
147400100909     c                   CLEAR                   hev(j)
147500100909     c                   ENDIF
147600120831     c                   EVAL      cntEveO74 += 1
147700000000e   1c                   ENDDO
147800000000     c*
147900000000     c                   ENDSR
148000000000     c*--------------------------------------------------------------------------------------------*
148100000000     c* memorizza gli eventi dal file eventi della bolla di input
148200000000     c*--------------------------------------------------------------------------------------------*
148300000000     c     memevb        BEGSR
148400150617     c* il programma usa il campo TASLNA per attribuire gli eventi della bolla
148500150617     c*  usa il TITAS che ha letto per ultimo che può essere quello della bolla  mamma
148600150617     c*  o della bolla figlia
148700150617     c*  per sapere se la bolla è italia o estera non devo usare l'ultima figlia
148800150617     c*  ma lna della bolla che sta leggendo in questo momento
148900150617     c                   Z-ADD     tasLna        worg
149000150617     c                   exsr      decorg
149100150617     c                   eval      wtasfl1 =  wofl1
149200000000     c***
149300000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
149400000000     c* . se SI lo aggiorna solo nella sequenza legame
149500000000     c*   se NO lo inserisce
149600000000     c***
149700000000     c                   Z-ADD     waas          kvbaas
149800000000     c                   Z-ADD     wlnp          kvblnp
149900000000     c                   Z-ADD     wnrs          kvbnrs
150000000000     c                   Z-ADD     wnsp          kvbnsp
150100000000     c     keyevb        SETLL     fnevb01l
150200000000     c     keyevb        READE     fnevb01l                               99
150300000000do  1c                   DOW       NOT *in99
150400000000if  2c                   IF        evbatb = *blanks                             *valido
150500140108     c                   MOVEL     evbdev        wdev
150600140108     c                   MOVEL     evbhev        whev
150700080131if  3c                   IF        evbDev > *zeros
150800000000     c                   MOVEL     evbcev        wcev
150900000000     c                   EXSR      deccevsta
151000070109     c                   EXSR      eventoAnnulla
151100000000if  4c                   IF        werr <> '1'                                  *no errore
151200000000     c*
151300000000     c* controlla se evento già inserito
151400000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
151500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
151600000000do  5c     1             DO        200           j
151700000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
151800080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
151900000000     c                   MOVEL     'N'           newevb                         *già inserito
152000000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
152100150618     C                   IF        wtasfl1 = ESTERO
152200150618     c                   Z-ADD     tasLna        worg
152300150618     c                   else
152400150618     c                   Z-ADD     evbfle        worg
152500150618     c                   endif
152600000000     c                   EXSR      decorg
152700000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
152800080201     c                   MOVEL     wDev          wdat
152900080201     c                   EXSR      edtdat
153000080201     c                   MOVEL     wdate         smdeve(j)
153100080201     c                   MOVEL     wHev          wora
153200080201     c                   EXSR      edtora
153300080201     c                   MOVEL     worae         smheve(j)
153400130313     c                   MOVEL     dsevbspe      smspe(j)
153500000000     c                   LEAVE                                                  *uscita ciclo
153600000000e   6c                   ENDIF
153700000000e   5c                   ENDDO
153800000000     c*
153900000000     c* incrementa contatore eventi
154000000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
154100000000     c                   ADD       1             tote
154200000000     c* indice schiera
154300000000     c                   Z-ADD     tote          smi(tote)
154400000000     c* sequenza legame
154500000000     c                   Z-ADD     seql          smseql(tote)
154600000000     c* sequenza evento
154700000000     c                   Z-ADD     020           smseqe(tote)
154800000000     c* descrizione
154900030130     c                   MOVEL     wcdex         smdev(tote)
155000000000     c* data
155100080201     c                   MOVEL     wDev          wdat
155200000000     c                   EXSR      edtdat
155300000000     c                   MOVEL     wdate         smdeve(tote)
155400000000     c* ora
155500080201     c                   MOVEL     wHev          wora
155600000000     c                   EXSR      edtora
155700000000     c                   MOVEL     worae         smheve(tote)
155800000000     c* filiale
155900150617     C***                IF        wrkLnaItaEst = ESTERO
156000150617     C                   IF        wtasfl1 = ESTERO
156100140722     c                   Z-ADD     tasLna        worg
156200140722     c                   else
156300140722     c                   Z-ADD     evbfle        worg
156400140722     c                   endif
156500140722     c                   EXSR      decorg
156600140722     c                   MOVEL     wodes         smfle(tote)
156700000000     c* causale
156800000000     c                   MOVEL     evbcev        smcev(tote)
156900130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
157000130312     c* bolla legata
157100130312     c                   MOVEL     dsevbspe      smspe(tote)
157200000000     c* data+ora evento
157300080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
157400000000e   5c                   ENDIF
157500000000e   4c                   ENDIF
157600000000e   3c                   ENDIF
157700000000e   2c                   ENDIF
157800010717     c     keyevb        READE     fnevb01l                               99
157900000000e   1c                   ENDDO
158000000000     c*
158100000000     c                   ENDSR
158200000000     c*--------------------------------------------------------------------------------------------*
158300000000     c* memorizza gli "eventi fissi"
158400000000     c*--------------------------------------------------------------------------------------------*
158500000000     c     memevefix     BEGSR
158600080131     ** ID bolla in canna.
158700080131     c                   EVAL      waas = tasaas
158800080131     c                   EVAL      wlnp = taslnp
158900080131     c                   EVAL      wnrs = tasnrs
159000080131     c                   EVAL      wnsp = tasnsp
159100000000     c*
159200000000     c* Partenza merce
159300000000     c                   EVAL      lduc = tasduc                                *data partenza
159400000000     c                   EVAL      llnp = taslnp                                *filiale partenza
159500000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
159600000000     c                   EXSR      evbpar
159700000000     c* Consegna merce
159800000000     c                   EVAL      lcca = tascca                                *consegna anomala
159900000000     c                   EVAL      ldcm = tasdcm                                *data consegna
160000000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
160100000000     c                   EVAL      llna = taslna                                *linea arrivo
160200150618     c                   EVAL      ltfa = tastfa                                *linea arrivo
160300000000     c                   EXSR      evbcon
160400000000     c*
160500000000     c                   ENDSR
160600000000     c*--------------------------------------------------------------------------------------------*
160700000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
160800000000     c*--------------------------------------------------------------------------------------------*
160900000000     c     elaevelbl     BEGSR
161000000000     c*
161100000000     c* azzera le variabili di lavoro
161200000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
161300000000     c                   EVAL      deplpn = *zeros
161400000000     c                   EVAL      depnrn = *zeros
161500000000     c                   EVAL      depnsn = *zeros
161600000000     c*
161700000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
161800000000     c                   EVAL      kbllpp =  s_taslnp
161900000000     c                   EVAL      kblnrp =  s_tasnrs
162000000000     c                   EVAL      kblnsp =  s_tasnsp
162100000000     c*
162200000000     c* ciclo fino a fine mamme
162300000000     c     ke2lbl        SETLL     fnlbl02l
162400000000     c     ke2lbl        READE     fnlbl02l                               99
162500000000do  1c                   DOW       NOT *in99
162600000000     c*
162700000000     c* ciclo fino a fine figlie della mamma
162800000000do  2c                   DOW       NOT *in99
162900000000     c*
163000000000     c* legge la bolla dal legame
163100000000     c                   EVAL      kasaas = lblaan                              *figlia
163200000000     c                   EVAL      kaslnp = lbllpn
163300000000     c                   EVAL      kasnrs = lblnrn
163400000000     c                   EVAL      kasnsp = lblnsn
163500000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
163600000000if  3c                   IF        NOT *in99 AND                                *esistente
163700000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
163800000000     c                             tastbl <> 'A3' AND
163900000000     c                             tastbl <> 'F3' AND
164000000000     c                             tastbl <> 'AP' AND
164100000000     c                             tastbl <> 'FC'
164200041222     **?Immagine lettera di vettura.
164300041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
164400041222     C                   IF        LDVO74 <> 'N'
164500040330     c                   MOVEL     tasflo        dtasflo
164600040330     c                   IF        §floiml <> *BLANK
164700050325     c                   EVAL      ldvo74 = 'S'
164800040330     C                   EVAL      SpImLDVO74 =
164900040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
165000040330     C                             %EDITC(taslnp:'X') +
165100040330     C                             %EDITC(tasnrs:'X') +
165200040330     C                             %EDITC(tasnsp:'X')
165300041223     **?Reperisco il check code della spedizione POD
165400041223     C                   EVAL      PChkCdeO74 =
165500041223     C                             GetSpeChkCde(
165600041223     C                             %EDITC(tasaas:'X'):
165700041223     C                             %EDITC(taslnp:'X') +
165800041223     C                             %EDITC(tasnrs:'X') +
165900041223     C                             %EDITC(tasnsp:'X'):
166000041223     C                             ChkCode:nEsito)
166100040330     C                   ENDIF
166200041222     C                   ENDIF
166300000000     c* incrementa la sequenza legami per le bolle legate
166400000000     c                   ADD       1             seql                           *legaqme successivo
166500000000     c*---
166600000000     c* memorizza gli "eventi fissi"
166700000000     c*---
166800000000     c                   EXSR      memevefix
166900000000     c*---
167000000000     c* memorizza gli eventi
167100000000     c*---
167200000000     c                   EVAL      waas = lblaan                                *figlia
167300000000     c                   EVAL      wlnp = lbllpn
167400000000     c                   EVAL      wnrs = lblnrn
167500000000     c                   EVAL      wnsp = lblnsn
167600000000     c                   EXSR      memevb
167700000000     c*
167800000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
167900000000     c                   EVAL      depaan = lblaan                              *ultima figlia
168000000000     c                   EVAL      deplpn = lbllpn
168100000000     c                   EVAL      depnrn = lblnrn
168200000000     c                   EVAL      depnsn = lblnsn
168300000000e   3c                   ENDIF
168400000000     c*
168500000000     c* lettura successiva legami
168600000000     c     ke2lbl        READE     fnlbl02l                               99
168700000000e   2c                   ENDDO                                                  *fine figlie mamma
168800000000     c*
168900000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
169000000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
169100000000     c                             kbllpp <> deplpn OR
169200000000     c                             kblnrp <> depnrn OR
169300000000     c                             kblnsp <> depnsn
169400000000     c* nuova chiave
169500000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
169600000000     c                   EVAL      kbllpp =  deplpn
169700000000     c                   EVAL      kblnrp =  depnrn
169800000000     c                   EVAL      kblnsp =  depnsn
169900000000     c* controlla figlie
170000000000     c     ke2lbl        SETLL     fnlbl02l
170100000000     c     ke2lbl        READE     fnlbl02l                               99
170200000000if  3c                   IF        *in99                                        *non ha figlie
170300000000     c                   LEAVE                                                  *esce dal ciclo
170400000000e   3c                   ENDIF
170500000000x   2c                   ELSE                                                   *sempre stessa mamma
170600000000     c                   LEAVE                                                  *esce dal ciclo
170700000000e   2c                   ENDIF
170800000000e   1c                   ENDDO                                                  *fine mamme
170900000000     c*
171000000000     c                   ENDSR
171100160127     c*--------------------------------------------------------------------------------------------*
171200160127     c* Imposta l'evento presa in carico se bolla in partenza
171300160127     c*--------------------------------------------------------------------------------------------*
171400160127     c     evbinca       BEGSR
171500160127     c*
171600160127     c* indice schiera
171700160127     c                   Z-ADD     tote          smi(tote)
171800160127     c* sequenza legame
171900160127     c                   Z-ADD     seql          smseql(tote)
172000160127     c* sequenza evento
172100160127     c                   Z-ADD     008           smseqe(tote)                   *primo evento
172200160127     c* data
172300160127if  1c                   IF        arbdim   > *zeros
172400160127     c                   MOVEL     arbdim        wdat
172500160127     c                   EXSR      edtdat
172600160127     c                   MOVEL     wdate         smdeve(tote)
172700160127e   1c                   ENDIF
172800160127     c* filiale
172900160127     c                   Z-ADD     arblnp        worg
173000160127     c                   EXSR      decorg
173100160127     c
173200160127     c                   MOVEL     wodes         smfle(tote)
173300160127     c* causale
173400160127     c                   MOVEL     wcev          smcev(tote)
173500160127     c* data+ora evento
173600160127     c                   MOVEL     wdat          smdevhev(tote)
173700160127     c                   MOVE      wora          smdevhev(tote)
173800160127     c*
173900160127     c                   ENDSR
174000000000     c*--------------------------------------------------------------------------------------------*
174100000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
174200000000     c*--------------------------------------------------------------------------------------------*
174300000000     c     evbrit        BEGSR
174400160318     c
174500000000     c*
174600000000     c* indice schiera
174700000000     c                   Z-ADD     tote          smi(tote)
174800000000     c* sequenza legame
174900000000     c                   Z-ADD     seql          smseql(tote)
175000000000     c* sequenza evento
175100000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
175200160407     c* data ritiro solo se spuntato
175300160318     c                   if        esito='P'
175400160407     c                   if        arbdse>0
175500160318if  1c                   IF        arbdrt > *zeros
175600160318     c                   MOVEL     arbdrt        wdat
175700160318     c                   EXSR      edtdat
175800160318     c                   MOVEL     wdate         smdeve(tote)
175900160318e   1c                   ENDIF
176000160318     c* ora  da FPP
176100160318if  2c                   IF        arbfpp = 'P'                                 *PM
176200160318     c                   MOVEL     1800          wora
176300160318x   2c                   ELSE                                                   *AM
176400160318     c                   MOVEL     1200          wora
176500160318e   1c                   ENDIF
176600160318     c                   EXSR      edtora
176700160318     c                   MOVEL     worae         smheve(tote)
176800160318     c* filiale
176900160318     c                   Z-ADD     arblnp        worg
177000160318     c                   EXSR      decorg
177100160407
177200160407     c                   endif
177300160407     c
177400160318     c
177500160318     c                   else
177600160318     c
177700000000if  1c                   IF        s_tasdrt > *zeros
177800000000     c                   MOVEL     s_tasdrt      wdat
177900000000     c                   EXSR      edtdat
178000000000     c                   MOVEL     wdate         smdeve(tote)
178100000000e   1c                   ENDIF
178200000000     c* ora
178300000000if  1c                   IF        s_tashrt > *zeros
178400000000     c                   MOVEL     s_tashrt      wora
178500000000x   1c                   ELSE
178600000000if  2c                   IF        s_tasfpp = 'P'                               *PM
178700000000     c                   MOVEL     1800          wora
178800000000x   2c                   ELSE                                                   *AM
178900000000     c                   MOVEL     1200          wora
179000000000e   2c                   ENDIF
179100000000e   1c                   ENDIF
179200000000     c                   EXSR      edtora
179300000000     c                   MOVEL     worae         smheve(tote)
179400000000     c* filiale
179500000000     c                   Z-ADD     s_taslnp      worg
179600000000     c                   EXSR      decorg
179700150618     c
179800150618     c* Se la bolla è import e devo emettere l'evento di telefonare
179900150618     c*  non posso indciare la filiale esterna, ma l'hub italiano
180000150618     c                   if        wofl1=ESTERO and sostrit='S' and
180100150618     c                             §7oinc='T'
180200150618     c                   z-add     s_tastfp      worg
180300150618     c                   EXSR      decorg
180400150618     c                   endif
180500160318     c                   endif
180600160318
180700000000     c                   MOVEL     wodes         smfle(tote)
180800000000     c* causale
180900000000     c                   MOVEL     wcev          smcev(tote)
181000000000     c* data+ora evento
181100000000     c                   MOVEL     wdat          smdevhev(tote)
181200000000     c                   MOVE      wora          smdevhev(tote)
181300000000     c*
181400000000     c                   ENDSR
181500000000     c*--------------------------------------------------------------------------------------------*
181600000000     c* Imposta l'evento Partenza
181700000000     c*--------------------------------------------------------------------------------------------*
181800000000     c     evbpar        BEGSR
181900000000     c*
182000000000     c* controlla SE e COME inserire l'evento
182100000000if  1c                   IF        lduc > *zeros
182200000000     c* imposta causale
182300000000     c                   MOVEL     '702'         wcev
182400000000     c                   EXSR      deccevsta
182500000000if  2c                   IF        werr <> '1'                                  *no errore
182600000000     c* imposta filiale
182700000000     c                   Z-ADD     llnp          worg
182800000000     c                   EXSR      decorg
182900000000     c* imposta data
183000101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
183100000000     c                   MOVEL     ldrt          wdat                           *data ritiro
183200000000x   3c                   ELSE                                                   partenza ITALIA
183300000000     c                   MOVEL     lduc          wdat                           *data partenza
183400000000e   3c                   ENDIF
183500000000     c                   EXSR      edtdat
183600000000     c* imposta ora
183700000000     c                   MOVEL     2100          wora                           *ora imposta
183800000000     c                   EXSR      edtora
183900000000     c*
184000000000     c* controlla se l'evento è già stato inserito
184100000000     c                   MOVEL     'S'           newevb                         *nuovo evento
184200000000do  3c     1             DO        200           j
184300000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
184400000000     c                   MOVE      wora          lavdevhev
184500000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
184600000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
184700000000     c                   MOVEL     'N'           newevb                         *già inserito
184800000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
184900000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
185000000000     c                   LEAVE                                                  *uscita ciclo
185100000000e   4c                   ENDIF
185200000000e   3c                   ENDDO
185300000000     c*
185400000000     c* incrementa contatore eventi
185500000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
185600000000     c                   ADD       1             tote
185700000000     c* indice schiera
185800000000     c                   Z-ADD     tote          smi(tote)
185900000000     c* sequenza legame
186000000000     c                   Z-ADD     seql          smseql(tote)
186100000000     c* sequenza evento
186200000000     c                   Z-ADD     020           smseqe(tote)
186300000000     c* descrizione
186400030130     c                   MOVEL     wcdex         smdev(tote)
186500000000     c* filiale -impostata sopra-
186600000000     c                   MOVEL     wodes         smfle(tote)
186700000000     c* data    -impostata sopra-
186800000000     c                   MOVEL     wdate         smdeve(tote)
186900000000     c* ora     -impostata sopra-
187000000000     c                   MOVEL     worae         smheve(tote)
187100000000     c* causale -impostata sopra-
187200000000     c                   MOVEL     wcev          smcev(tote)
187300000000     c* data+ora evento
187400000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
187500000000     c                   MOVE      wora          smdevhev(tote)
187600000000e   3c                   ENDIF
187700000000e   2c                   ENDIF
187800000000e   1c                   ENDIF
187900000000     c*
188000000000     c                   ENDSR
188100101129     c*------------------------------------------------------------------------*
188200101129     c*
188300101129     c* Imposta l'evento Arrivo
188400101129     c*
188500101129     c*------------------------------------------------------------------------*
188600000000     c     evbarr        BEGSR
188700101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
188800101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
188900101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
189000101129     ** sommato inutile.
189100150618     C                   IF        wrkLnaItaEst = ESTERO
189200150618     C                   LEAVESR
189300150618     C                   ENDIF
189400101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
189500101129     C                   IF        lhti = *ZERO
189600101129     C                   LEAVESR
189700101129     C                   ENDIF
189800000000     c*
189900000000     c* controlla SE e COME inserire l'evento
190000000000if  1c                   IF        ldti > *zeros OR
190100000000     c                             ldcm > *zeros
190200000000     c* imposta causale
190300101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
190400000000     c                   EXSR      deccevsta
190500000000if  2c                   IF        werr <> '1'                                  *no errore
190600000000     c* imposta filiale
190700150617     c                   Z-ADD     llna          worg
190800000000     c                   EXSR      decorg
190900000000     c* imposta data
191000000000if  3c                   IF        ldti > *zeros
191100000000     c                   MOVEL     ldti          wdat                           *data arrivo
191200000000x   3c                   ELSE
191300000000     c                   MOVEL     ldcm          wdat                           *data consegna
191400000000e   3c                   ENDIF
191500000000     c                   EXSR      edtdat
191600000000     c* imposta ora
191700000000if  3c                   IF        lhti > *zeros
191800000000     c                   MOVEL     lhti          wora                           *ora arrivo
191900000000x   3c                   ELSE
192000000000     c                   MOVEL     0600          wora                           *ora imposta
192100000000e   3c                   ENDIF
192200000000     c                   EXSR      edtora
192300000000     c*
192400000000     c* controlla se l'evento è già stato inserito
192500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
192600000000do  3c     1             DO        200           j
192700000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
192800000000     c                   MOVE      wora          lavdevhev
192900000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
193000000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
193100000000     c                   MOVEL     'N'           newevb                         *già inserito
193200000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
193300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
193400000000     c                   LEAVE                                                  *uscita ciclo
193500000000e   4c                   ENDIF
193600000000e   3c                   ENDDO
193700000000     c*
193800000000     c* incrementa contatore eventi
193900000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
194000000000     c                   ADD       1             tote
194100000000     c* indice schiera
194200000000     c                   Z-ADD     tote          smi(tote)
194300000000     c* sequenza legame
194400000000     c                   Z-ADD     seql          smseql(tote)
194500000000     c* sequenza evento
194600000000     c                   Z-ADD     020           smseqe(tote)
194700000000     c* descrizione
194800030130     c                   MOVEL     wcdex         smdev(tote)
194900000000     c* data    -impostata sopra-
195000000000     c                   MOVEL     wdate         smdeve(tote)
195100000000     c* ora     -impostata sopra-
195200000000     c                   MOVEL     worae         smheve(tote)
195300000000     c* filiale -impostata sopra-
195400000000     c                   MOVEL     wodes         smfle(tote)
195500000000     c* causale -impostata sopra-
195600000000     c                   MOVEL     wcev          smcev(tote)
195700000000     c* data+ora evento
195800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
195900000000     c                   MOVE      wora          smdevhev(tote)
196000000000e   3c                   ENDIF
196100000000e   2c                   ENDIF
196200000000e   1c                   ENDIF
196300000000     c*
196400000000     c                   ENDSR
196500000000     c*--------------------------------------------------------------------------------------------*
196600000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
196700000000     c*--------------------------------------------------------------------------------------------*
196800140714     c     evbcon        BEGSR
196900000000     c*
197000000000     c* azzera variabili di lavoro
197100000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
197200000000     c****
197300000000     c* Consegna merce ->
197400000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
197500000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
197600000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
197700000000     c*   nè lo stato di consegna nè quello di consegna anomala.
197800000000     c****
197900000000     c* controlla SE e COME inserire l'evento
198000000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
198100060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
198200060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
198300060622     C                             :rpyEsito)
198400060622     C                   IF        rpyOpCode = 'FOUND'
198500041222     **?Consegna anomala, POD non visualizzabile.
198600060622     C                   IF        §7OPODImg = 'N'
198700041222     C                   EVAL      LDVO74 = 'N'
198800041222     C                   ENDIF
198900060622     c                   IF        §7oinc <> *blanks                            *ok per internet
199000060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
199100000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
199200000000x   3c                   ELSE                                                   *ha record FNEVB
199300000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
199400000000e   3c                   ENDIF
199500041222e   3c                   ENDIF
199600000000e   2c                   ENDIF
199700000000e   1c                   ENDIF
199800080131     c* imposta data
199900080131if  1c                   IF        ldcm > *zeros
200000080131     c                   MOVEL     ldcm          wdat
200100080131e   1c                   ENDIF
200200080131     c* imposta ora
200300080131if  1c                   IF        lhmc > *zeros
200400080131     c                   MOVEL     lhmc          wora
200500080131e   1c                   ENDIF
200600000000     c* imposta causale
200700000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
200800060622     c                   MOVEL     §7ocev        wcev                           *causale evento
200900080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
201000080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
201100080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
201200080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
201300080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
201400080131     c* ritiro merce) dalla bolla figlia.
201500080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
201600080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
201700080131     c                             AND s_tasNdc < *HIVAL
201800080131     c                   EVAL      kblAap = wAas
201900080131     c                   EVAL      kblLpp = wLnp
202000080131     c                   EVAL      kblNrp = wNrs
202100080131     c                   EVAL      kblNsp = wNsp
202200080131     c     ke2lbl        CHAIN     fnlbl02l
202300080131     c                   IF        %FOUND()
202400080131     c                   EVAL      kasAas = lblAan
202500080131     c                   EVAL      kasLnp = lblLpn
202600080131     c                   EVAL      kasNrs = lblNrn
202700080131     c                   EVAL      kasNsp = lblNsn
202800080131     C     keyTas        CHAIN     titas000      titas000Figlia
202900080131     C                   IF        %FOUND()
203000080131     C                   EVAL      wDat = titas000Figlia.tasDrt
203100080131     C                   EVAL      wOra = titas000Figlia.tasHrt
203200080131     C                   ELSE
203300080131     C     keyTas        CHAIN     titas010      titas010Figlia
203400080131     C                   IF        %FOUND()
203500080131     C                   EVAL      wDat = titas010Figlia.tasDrt
203600080131     C                   EVAL      wOra = titas010Figlia.tasHrt
203700080131     C                   ELSE
203800080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
203900080131     C                   IF        %FOUND()
204000080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
204100080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
204200080131     C                   ENDIF
204300080131     C                   ENDIF
204400080131     C                   ENDIF
204500080131     c                   ENDIF
204600080131     c                   ENDIF
204700000000x   1c                   ELSE                                                   *NO sost.Consegna
204800000000     c                   MOVEL     '704'         wcev                           *causale evento
204900000000e   1c                   ENDIF
205000080131     c* Editazione data e ora evento.
205100080131     c                   EXSR      edtdat
205200080131     c                   EXSR      edtora
205300000000     c* imposta filiale
205400000000     c                   Z-ADD     llna          worg
205500000000     c                   EXSR      decorg
205600150618     c* se si tratta di lna estera e la bolla ha consegna anomala
205700150618     c*  in cui mettere msg generico, allora devo sostituire la filiale
205800150618     c*  con il suo hub
205900150618     c                   if        wofl1=estero and lcca<>' ' and
206000150618     c                             §7oinc='T'
206100150618     c                   Z-ADD     ltfa          worg
206200150618     c                   EXSR      decorg
206300150618     c                   endif
206400150618     c
206500000000     c* controlla se l'evento è già stato inserito
206600000000     c                   MOVEL     'S'           newevb                         *nuovo evento
206700000000do  1c     1             DO        200           j
206800000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
206900000000     c                   MOVE      wora          lavdevhev
207000000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
207100000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
207200000000     c                   MOVEL     'N'           newevb                         *già inserito
207300000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
207400000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
207500000000     c                   LEAVE                                                  *uscita ciclo
207600000000e   2c                   ENDIF
207700000000e   1c                   ENDDO
207800000000     c*
207900000000     c* incrementa contatore eventi
208000030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
208100030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
208200000000     c                   ADD       1             tote                           incrementa contatore
208300060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
208400060622     c                   MOVEL     §7odei        smdev(tote)
208500030203x   4c                   ELSE                                                   *evento telef.P.O.
208600060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
208700030203e   4c                   ENDIF
208800060622     c                   MOVEL     §7ocev        wcev                           *causale evento
208900000000     c                   EXSR      evbcon2                                      *imposta evento
209000030203x   3c                   ELSE                                                   *NO sost.Consegna
209100000000     c*
209200030203if  4c                   IF        ldcm > *zeros
209300030203if  5c                   IF        sostcon <> ' '                               *considera consegna
209400000000     c                   EXSR      deccevsta
209500030203if  6c                   IF        werr <> '1'                                  *no errore
209600000000     c                   ADD       1             tote                           incrementa contatore
209700030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
209800000000     c                   EXSR      evbcon2                                      *imposta evento
209900030203e   6c                   ENDIF
210000030203e   5c                   ENDIF
210100030203e   4c                   ENDIF
210200030203e   3c                   ENDIF
210300000000     c*
210400030203e   1c                   ENDIF
210500030203     c*
210600000000     c                   ENDSR
210700000000     c*--------------------------------------------------------------------------------------------*
210800000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
210900000000     c*--------------------------------------------------------------------------------------------*
211000000000     c     evbcon2       BEGSR
211100000000     c*
211200000000     c* indice schiera
211300000000     c                   Z-ADD     tote          smi(tote)
211400000000     c* sequenza legame
211500000000     c                   Z-ADD     seql          smseql(tote)
211600000000     c* sequenza evento
211700000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
211800000000     c* data    -impostata sopra-
211900000000     c                   MOVEL     wdate         smdeve(tote)
212000000000     c* ora     -impostata sopra-
212100000000     c                   MOVEL     worae         smheve(tote)
212200000000     c* filiale -impostata sopra-
212300000000     c                   MOVEL     wodes         smfle(tote)
212400000000     c* causale -impostata sopra-
212500000000     c                   MOVEL     wcev          smcev(tote)
212600000000     c* data+ora evento
212700000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
212800000000     c                   MOVE      wora          smdevhev(tote)
212900000000     c*
213000000000     c                   ENDSR
213100000000     c*--------------------------------------------------------------------------------------------*
213200000000     c* memorizza i dati della lettera di anomalia
213300000000     c*--------------------------------------------------------------------------------------------*
213400000000     c     memdct        BEGSR
213500000000     c*
213600040906     C                   RESET                   FIDN12DS
213700040906     c                   Z-ADD     waas          I12AAS
213800040906     c                   Z-ADD     wlnp          I12LNP
213900040906     c                   Z-ADD     wnrs          I12NRS
214000040906     c                   Z-ADD     wnsp          I12NSP
214100040906     C                   CALL      'FIDN12R'
214200040906     C                   PARM                    FIDN12DS
214300000000     c* memorizza la prima lettera di anomalia valida
214400040906do  1c                   IF        O12Err = *BLANK
214500040906     C                   DO        O12NCA        K
214600040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
214700040906     C                   EVAL      DCTAAC = O12KeyAAC
214800040906     C                   EVAL      DCTFIL = O12KeyFil
214900040906     C                   EVAL      DCTNCA = O12KeyNCA
215000040906     c     keydct        CHAIN     fndct01l
215100040906do  1c                   IF        %FOUND
215200000000     c*
215300000000     c* controlla validità CA
215400000000     c                   MOVEL     '0'           werr                           *NO errore
215500000000     c                   EXSR      chkdct
215600000000if  2c                   IF        werr = '0'                                   *CA valida
215700000000     c*
215800000000     c* Imposta l'evento lettera di anomalia
215900000000     c                   EXSR      evbdct
216000000000     C                   LEAVE                                                  *uscita ciclo
216100000000e   2c                   ENDIF
216200000000     c*
216300040906e   1c                   ENDIF
216400040906e   1c                   ENDDO
216500040906e   1c                   ENDIF
216600000000     C*
216700000000     c                   ENDSR
216800000000     c*--------------------------------------------------------------------------------------------*
216900000000     c* controlla validità lettera di anomalia
217000000000     c*--------------------------------------------------------------------------------------------*
217100000000     c     chkdct        BEGSR
217200000000     c*
217300000000     c* reimposta variabili di lavoro
217400000000     c                   MOVEL     '0'           werr                           *NO errore
217500000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
217600000000     c*
217700000000     c* CA annullata, esclude
217800000000if  1c                   IF        dctatb <> ' '
217900000000     c                   MOVEL     '1'           werr                           *SI errore
218000000000e   1C                   ENDIF
218100000000     c*
218200000000     c* CA non confermata, esclude
218300000000if  1c                   IF        dctfpr = ' '
218400000000     c                   MOVEL     '1'           werr                           *SI errore
218500000000e   1C                   ENDIF
218600000000     c*
218700000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
218800000000if  1c                   IF        dctfpr = 'T' AND
218900000000     c                             dctfca >= 215 OR
219000000000     c                             dctfpr = 'P' AND
219100000000     c                             dctfca >= 400
219200000000x   1c                   ELSE
219300000000     c                   MOVEL     '1'           werr                           *SI errore
219400000000e   1C                   ENDIF
219500000000     c*
219600000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
219700000000if  1c                   IF        werr = '0'
219800000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
219900060622     C                   RESET                   tibs02ds
220000060622     C                   EVAL      t02Cod = 'CCH'
220100060622     C                   EVAL      t02Ke1 = dctcch
220200060622     C                   EXSR      getTntbe00f
220300060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
220400000000     c                   MOVEL     '1'           werr                           *SI errore
220500000000e   3C                   ENDIF
220600000000e   2C                   ENDIF
220700000000e   1C                   ENDIF
220800000000     c*
220900000000     c                   ENDSR
221000000000     c*--------------------------------------------------------------------------------------------*
221100000000     c* Imposta l'evento lettera di anomalia
221200000000     c*--------------------------------------------------------------------------------------------*
221300000000     c     evbdct        BEGSR
221400000000     C*
221500000000     c* imposta causale
221600000000if  1c                   IF        wfpr = 'T'
221700000000     c                   MOVEL     '709'         wcev                           *causale evento
221800000000x   1c                   ELSE
221900000000     c                   MOVEL     '710'         wcev                           *causale evento
222000000000e   1c                   ENDIF
222100000000     c                   EXSR      deccevsta                                    *decodifica evento
222200000000     c*
222300000000     c* imposta filiale
222400000000if  1c                   IF        werr <> '1'                                  *no errore
222500000000     c                   Z-ADD     dctfil        worg
222600000000     c                   EXSR      decorg
222700000000     c* imposta data
222800000000     c                   MOVEL     dctaac        wdat
222900000000     c                   MOVE      dctmgc        wdat
223000000000     c                   EXSR      edtdat
223100000000     c* imposta ora
223200000000     c                   MOVEL     1100          wora                           *FISSA
223300000000     c                   EXSR      edtora
223400000000     c*
223500000000     c* incrementa contatore eventi
223600000000     c                   ADD       1             tote
223700000000     c* indice schiera
223800000000     c                   Z-ADD     tote          smi(tote)
223900000000     c* sequenza legame
224000000000     c                   Z-ADD     seql          smseql(tote)
224100000000     c* sequenza evento
224200000000     c                   Z-ADD     999           smseqe(tote)
224300000000     c* descrizione -impostata sopra-
224400030130     c                   MOVEL     wcdex         smdev(tote)
224500000000     c* filiale -impostata sopra-
224600000000     c                   MOVEL     wodes         smfle(tote)
224700000000     c* data    -impostata sopra-
224800000000     c                   MOVEL     wdate         smdeve(tote)
224900000000     c* ora     -impostata sopra-
225000000000     c                   MOVEL     worae         smheve(tote)
225100000000     c* causale -impostata sopra-
225200000000     c                   MOVEL     wcev          smcev(tote)
225300000000     c* data+ora evento
225400000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
225500000000     c                   MOVE      wora          smdevhev(tote)
225600000000e   1c                   ENDIF
225700000000     c*
225800000000     c                   ENDSR
225900000000     c*--------------------------------------------------------------------------------------------*
226000000000     c* imposta i campi della DS di Output dal contrassegno
226100000000     c*--------------------------------------------------------------------------------------------*
226200000000     c     impdsocsb     BEGSR
226300000000     c*
226400000000     c* azzera le variabili di lavoro
226500000000     c*
226600000000     c****
226700000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
226800000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
226900000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
227000000000     c****
227100000000     c*
227200000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
227300090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
227400000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
227500000000x   1c                   ELSE                                                     *trovato
227600000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
227700000000e   1C                   ENDIF
227800000000     c*---
227900000000     c* memorizza il contrassegno della bolla di Input
228000000000     c*---
228100000000     c                   EVAL      waas = s_tasaas                              *bolla Input
228200000000     c                   EVAL      wlnp = s_taslnp
228300000000     c                   EVAL      wnrs = s_tasnrs
228400000000     c                   EVAL      wnsp = s_tasnsp
228500000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
228600000000     c                   EXSR      memcsb
228700000000     c*
228800000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
228900000000     c                   EXSR      elacsblbl
229000000000     c*
229100000000     c                   ENDSR
229200000000     c*--------------------------------------------------------------------------------------------*
229300000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
229400000000     c*--------------------------------------------------------------------------------------------*
229500000000     c     elacsblbl     BEGSR
229600000000     c*
229700000000     c* azzera le variabili di lavoro
229800000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
229900000000     c                   EVAL      deplpn = *zeros
230000000000     c                   EVAL      depnrn = *zeros
230100000000     c                   EVAL      depnsn = *zeros
230200000000     c*
230300000000     c* legge i legami delle figlie successive alla bolla di input
230400000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
230500000000     c                   EVAL      kbllpp =  s_taslnp
230600000000     c                   EVAL      kblnrp =  s_tasnrs
230700000000     c                   EVAL      kblnsp =  s_tasnsp
230800000000     c*
230900000000     c* ciclo fino a fine mamme
231000000000     c     ke2lbl        SETLL     fnlbl02l
231100000000     c     ke2lbl        READE     fnlbl02l                               99
231200000000do  1c                   DOW       NOT *in99
231300000000     c*
231400000000     c* ciclo fino a fine figlie della mamma
231500000000do  2c                   DOW       NOT *in99
231600000000     c*
231700000000     c* legge la bolla dal legame
231800000000     c                   EVAL      kasaas = lblaan                              *figlia
231900000000     c                   EVAL      kaslnp = lbllpn
232000000000     c                   EVAL      kasnrs = lblnrn
232100000000     c                   EVAL      kasnsp = lblnsn
232200000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
232300000000     c*---
232400000000     c* memorizza il contrassegno della bolla figlia legata
232500000000     c*---
232600000000if  4c                   IF        NOT *in99                                    *esistente
232700000000     c                   EVAL      waas = lblaan                                *figlia
232800000000     c                   EVAL      wlnp = lbllpn
232900000000     c                   EVAL      wnrs = lblnrn
233000000000     c                   EVAL      wnsp = lblnsn
233100000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
233200000000     c                   EXSR      memcsb
233300000000e   4c                   ENDIF
233400000000     c*
233500000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
233600000000     c                   EVAL      depaan = lblaan                              *ultima figlia
233700000000     c                   EVAL      deplpn = lbllpn
233800000000     c                   EVAL      depnrn = lblnrn
233900000000     c                   EVAL      depnsn = lblnsn
234000000000     c*
234100000000     c* lettura successiva legami
234200000000     c     ke2lbl        READE     fnlbl02l                               99
234300000000e   2c                   ENDDO                                                  *fine figlie mamma
234400000000     c*
234500000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
234600000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
234700000000     c                             kbllpp <> deplpn OR
234800000000     c                             kblnrp <> depnrn OR
234900000000     c                             kblnsp <> depnsn
235000000000     c* nuova chiave
235100000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
235200000000     c                   EVAL      kbllpp =  deplpn
235300000000     c                   EVAL      kblnrp =  depnrn
235400000000     c                   EVAL      kblnsp =  depnsn
235500000000     c* controlla figlie
235600000000     c     ke2lbl        SETLL     fnlbl02l
235700000000     c     ke2lbl        READE     fnlbl02l                               99
235800000000if  3c                   IF        *in99                                        *non ha figlie
235900000000     c                   LEAVE                                                  *esce dal ciclo
236000000000e   3c                   ENDIF
236100000000x   2c                   ELSE                                                   *sempre stessa mamma
236200000000     c                   LEAVE                                                  *esce dal ciclo
236300000000e   2c                   ENDIF
236400000000e   1c                   ENDDO                                                  *fine mamme
236500000000     c*
236600000000     c                   ENDSR
236700000000     c*--------------------------------------------------------------------------------------------*
236800000000     c* memorizza il contrassegno
236900000000     c*--------------------------------------------------------------------------------------------*
237000000000     c     memcsb        BEGSR
237100000000     c*
237200000000     c* legge il contrassegno, se avuto
237300000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
237400000000     c                   Z-ADD     waas          ksbaas
237500000000     c                   Z-ADD     wlnp          ksblnp
237600000000     c                   Z-ADD     wnrs          ksbnrs
237700000000     c                   Z-ADD     wnsp          ksbnsp
237800000000     c     keycsb        CHAIN     tncsb03l                           99
237900000000if  2c                   IF        NOT *in99                                    *esistente
238000000000     c* memorizza i dati
238100000000     c                   MOVEL     'S'           cao74                          *esiste C/A
238200000000     c*
238300000000     c                   Z-ADD     csbcas        caimpo74                       *importo
238400000000     c*
238500000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
238600010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
238700010820     c                             caimpo74 > *zeros
238800010820     c                   EVAL      cadivo74 = 'ITL'
238900010820e   3c                   ENDIF
239000160314
239100160314     ** Visualizzo la descrizione del tipo incasso
239200100121     C                   EVAL      cainco74 =
239300100121     C                                       GetDescrizioneIncassoContrassegno()
239400000000     c*
239500000000     c                   MOVEL     *blanks       castao74                       *stato
239600060622     C                   EVALR     tblKey = %CHAR(csbSta)
239700060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
239800060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
239900060622     C                             :rpyOpCode:rpyEsito)
240000060622     C                   IF        rpyOpCode = 'FOUND'
240100060622     c                   EVAL      castao74 = §4SDei
240200000000e   3c                   ENDIF
240300100119     c
240400000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
240500011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
240600060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
240700000000x   4c                   ELSE
240800000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
240900060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
241000000000x   5c                   ELSE
241100060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
241200000000e   5c                   ENDIF
241300000000e   4c                   ENDIF
241400000000e   3c                   ENDIF
241500000000     c*
241600000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
241700000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
241800000000if  4c                   IF        csbtpa = 'B'
241900060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
242000000000e   4c                   ENDIF
242100000000if  4c                   IF        csbtpa = 'C'
242200060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
242300000000e   4c                   ENDIF
242400000000x   3c                   ELSE                                                   *incasso Bartolini
242500000000if  4c                   IF        csbfpc = *blanks
242600110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
242700000000e   4c                   ENDIF
242800000000if  4c                   IF        csbfpc = '2'
242900060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
243000000000e   4c                   ENDIF
243100000000e   3c                   ENDIF
243200000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
243300000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
243400000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
243500000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
243600000000     c                             csbfpc = *blanks                              assegno di traenza
243700060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
243800000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
243900060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
244000000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
244100060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
244200000000e   4c                   ENDIF
244300000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
244400060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
244500060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
244600000000e   3c                   ENDIF
244700000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
244800000000     c                   Z-ADD     *zeros        caabio74                       *abi
244900000000     c                   Z-ADD     *zeros        cacabo74                       *cab
245000000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
245100000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
245200000000     c                   MOVEL     csbddp        wdat
245300000000     c                   EXSR      edtdat
245400000000     c                   EVAL      cadato74 = wdate
245500000000     C*
245600000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
245700000000     c                   Z-ADD     csbabi        caabio74
245800000000     c                   Z-ADD     csbcab        cacabo74
245900000000     c                   MOVEL     csbnra        canumo74
246000120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
246100120321     c                             : caCabO74)
246200000000x   4c                   ELSE                                                   *incasso Bartolini
246300000000     c                   MOVEL     csbndp        canumo74
246400000000e   4c                   ENDIF
246500000000e   3c                   ENDIF
246600110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
246700110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
246800110307     C                   IF        IsContrassegnoCompensato()
246900110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
247000110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
247100110304     C                   CLEAR                   caabio74
247200110304     C                   CLEAR                   cacabo74
247300110304     C                   CLEAR                   canumo74
247400110304     C                   ENDIF
247500110304     C*
247600000000e   2c                   ENDIF
247700000000e   1c                   ENDIF
247800000000     c*
247900000000     c                   ENDSR
248000000000     c*--------------------------------------------------------------------------------------------*
248100000000     c* imposta i campi della DS di Output dalla giacenza
248200000000     c*--------------------------------------------------------------------------------------------*
248300000000     c     impdsogcp     BEGSR
248400000000     c*
248500000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
248600000000     c                   EVAL      waas = s_tasaas                              *bolla Input
248700000000     c                   EVAL      wlnp = s_taslnp
248800000000     c                   EVAL      wnrs = s_tasnrs
248900000000     c                   EVAL      wnsp = s_tasnsp
249000000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
249100000000     c                   EXSR      memgcp
249200000000     c*
249300000000     c                   ENDSR
249400000000     c*--------------------------------------------------------------------------------------------*
249500000000     c* memorizza la giacenza
249600000000     c*--------------------------------------------------------------------------------------------*
249700000000     c     memgcp        BEGSR
249800000000     c*
249900000000     c* imposta le variabili di lavoro
250000000000     c                   CLEAR                   i                              *indice
250100000000     c                   CLEAR                   ii
250200000000     c                   CLEAR                   smdmc                          *note fase -20-
250300000000     c                   CLEAR                   smdmcR
250400000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
250500000000     c                   MOVEL     'N'           gco74                          *flag di controllo
250600000000     c                   MOVEL     'N'           gcfdio74
250700000000     c                   MOVEL     'N'           gcmito74
250800000000     c*
250900000000     c* legge la giacenza, se avuta
251000000000     c                   Z-ADD     waas          kcpaas
251100000000     c                   Z-ADD     wlnp          kcplnp
251200000000     c                   Z-ADD     wnrs          kcpnrs
251300000000     c                   Z-ADD     wnsp          kcpnsp
251400000000     c                   Z-ADD     *zeros        kcpfrg
251500050309     c     keygcp        CHAIN     tigcp51l                           99
251600050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
251700050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
251800000000if  2c                   IF        NOT *in99        AND                         *esistente
251900000000     c                             gcpatb = *blanks AND                         *no annullata
252000050415     c                             gcptcm <> 'Z'    AND
252100050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
252200050419     C
252300050419     C                   EVAL      gcRiapO74 = gcpRiap
252400050418     **?Descrizione della fase.
252500050418     C                   SELECT
252600050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
252700060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
252800050418     C                   WHEN      gcpFas = 20
252900060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
253000050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
253100060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
253200050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
253300060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
253400050418     C                   ENDSL
253500000000     c* memorizza i dati dalle NOTE GIACENZE
253600000000     c                   Z-ADD     *zeros        i
253700000000     c                   Z-ADD     *zeros        ii
253800000000     c                   Z-ADD     gcpagc        knpagc
253900000000     c                   Z-ADD     gcpfgc        knpfgc
254000000000     c                   Z-ADD     gcpngc        knpngc
254100000000     c                   Z-ADD     gcpfrg        knpfrg
254200050309     c     keygnp        SETLL     tignp01l
254300050309     c     keygnp        READE     tignp01l                               98
254400000000do  3c                   DOW       NOT *in98
254500050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
254600000000     c* fase -10-
254700000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
254800000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
254900000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
255000000000e   5c                   ENDIF
255100000000     c* fase -20-
255200000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
255300000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
255400051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
255500000000     c                   ADD       1             i
255600000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
255700000000e   6c                   ENDIF
255800000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
255900051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
256000050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
256100050405     c                   CLEAR                   ii
256200050405     c                   CLEAR                   smdmcR
256300050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
256400050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
256500050919     C                   IF        NOT %ERROR
256600140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
256700050919     C                   ENDIF
256800050405     C                   ELSE
256900000000     c                   ADD       1             ii
257000000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
257100050405     C                   ENDIF
257200000000e   6c                   ENDIF
257300000000e   5c                   ENDIF
257400000000e   4c                   ENDIF
257500050309     c     keygnp        READE     tignp01l                               98
257600000000e   3c                   ENDDO
257700000000     c*
257800000000     c* memorizza i dati dalle GIACENZE
257900000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
258000050404     c* Disposizione da immettere.
258100051110     c                   IF        gcpfas = 20
258200051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
258300051110     C                   EXSR      chkTivpi
258400051110e   3c                   ENDIF
258500000000     c*
258600000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
258700000000     c*
258800000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
258900000000     c*
259000060103     c                   Z-ADD     gcpagc        dsannB
259100060103     c                   Z-ADD     gcpmgc        dsmgsB
259200060103     c                   Z-ADD     dsdatB        wdat
259300000000if  3c                   IF        wdat > *zeros
259400000000     c                   EXSR      edtdat
259500000000     c                   EVAL      gcdago74 = wdate                             *data apertura
259600000000e   3C                   ENDIF
259700060104     c*
259800060104     c                   Z-ADD     gcpdur        wdat
259900060104     c                   IF        wdat > *zeros
260000060104     c                   EXSR      edtdat
260100060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
260200060104     C                   ENDIF
260300000000     c*
260400000000if  3c                   IF        gcpcmc <> *blanks
260500000000     c                   MOVEL     gcpcmc        wcev
260600000000     c                   EXSR      deccevgia
260700030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
260800000000e   3C                   ENDIF
260900000000     c*
261000000000     c                   MOVEL     *blanks       gcdcgo74
261100000000if  3c                   IF        gcpdcg > *zeros
261200000000     c                   Z-ADD     gcpdcg        wdat
261300000000     c                   EXSR      edtdat
261400000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
261500000000e   3c                   ENDIF
261600000000     c*---
261700000000     c* Disposizioni già immesse
261800000000     c*---
261900000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
262000000000     c*
262100000000if  4c                   IF        gcpddm > *zeros
262200000000     c                   Z-ADD     gcpddm        wdat
262300000000     c                   EXSR      edtdat
262400000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
262500000000e   4C                   ENDIF
262600000000     c*
262700000000if  4c                   IF        gcpdis <> *blanks
262800060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
262900060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
263000060626     C                             :rpyOpCode:rpyEsito)
263100060622     C                   EVAL      gcdiso74 = §2DDes
263200000000e   4C                   ENDIF
263300000000     c*
263400000000if  4c                   IF        gcpasg <> *blanks
263500000000if  5c                   IF        gcpasg = 'S'
263600060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
263700000000x   5c                   ELSE
263800060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
263900000000e   5C                   ENDIF
264000000000     c*
264100000000if  5c                   IF        gcppsg = 'M'
264200060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
264300000000e   5C                   ENDIF
264400000000if  5c                   IF        gcppsg = 'D'
264500060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
264600000000e   5C                   ENDIF
264700000000e   4C                   ENDIF
264800000000     c*
264900000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
265000000000     c                   MOVEL     gcpvcs        gcvcso74
265100000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
265200000000     c                   Z-ADD     gcpcas        gccaso74
265300000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
265400000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
265500000000e   5C                   ENDIF
265600000000e   4C                   ENDIF
265700050404     C                   IF        smdmc(1) <> *BLANK
265800050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
265900050404     C                             smdmc(3) + smdmc(4)
266000051014     C                   EVAL      gcdmc5O74 = smdmc(5)
266100050404     C                   ELSE
266200050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
266300050404     C                             smdmcr(3) + smdmcr(4)
266400051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
266500050404e   3C                   ENDIF
266600000000e   3C                   ENDIF
266700000000     c*---
266800000000     c* Disposizioni da immettere
266900000000     c*---
267000010719     c* se le disposizioni sono da immettere controlla che:
267100010719     c* - il cliente è il mittente della giacenza
267200000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
267300040726if  4C                   IF        keygiac <> *blanks
267400040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
267500040726x   4c                   ELSE
267600090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
267700090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
267800090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
267900090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
268000090914     C                             )
268100040726e   4C                   ENDIF
268200000000e   3C                   ENDIF
268300050404     C* Controllo esistenza storia giacenza.
268400050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
268500050404     C                   DO        *HIVAL
268600050404     C     K04GCP51      READE     TIGCP51L
268700050404     C                   IF        %EOF
268800050404     C                   LEAVE
268900050404     C                   ENDIF
269000050404     C                   IF        GCPATB = *BLANK
269100050404     C                   EVAL      StoGiaO74 = 'S'
269200050404     C                   LEAVE
269300050404     C                   ENDIF
269400050404     C                   ENDDO
269500050404     c*
269600000000e   2c                   ENDIF
269700000000     c*
269800000000     c                   ENDSR
269900000000     c*--------------------------------------------------------------------------------------------*
270000000000     c* imposta i campi della DS di Output dalle note
270100000000     c*--------------------------------------------------------------------------------------------*
270200000000     c     impdsonot     BEGSR
270300000000     c*
270400000000     c* azzera le variabili di lavoro
270500000000     c                   CLEAR                   i
270600000000     c                   CLEAR                   mdcre                          consegna richiesta
270700000000     c                   CLEAR                   mhcre
270800030210     c                   CLEAR                   mtcre
270900000000     c                   CLEAR                   mftce                          consegna particolare
271000000000     c                   CLEAR                   wftc
271100000000     c                   CLEAR                   d1wdftc
271200000000     c                   CLEAR                   d2wdftc
271300000000     c                   CLEAR                   mgcxe                          turni chiusura
271400000000     c                   CLEAR                   wgcx
271500000000     c                   CLEAR                   d1wdgcx
271600000000     c                   CLEAR                   d2wdgcx
271700010927     c                   CLEAR                   mffd                           fermo deposito
271800010927     c                   CLEAR                   mmncB                          *motivi NON consegna
271900010927     c                   CLEAR                   mmncC
272000010927     c                   CLEAR                   mmncG
272100010927     c                   CLEAR                   mmncH
272200010927     c                   CLEAR                   mmncN
272300010927     c                   CLEAR                   mmncO
272400050701     c                   CLEAR                   mmnc8
272500050701     c                   CLEAR                   mmnc9
272600050701     c                   CLEAR                   a230
272700000000     c                   CLEAR                   we
272800030206     c                   CLEAR                   mtel                           n° tel destinatario
272900030206     c                   CLEAR                   mban                           bancali
273000030207     c                   CLEAR                   mscr                           supermercati
273100030210     c                   CLEAR                   msdace
273200030210     c                   CLEAR                   msorce
273300030210     c                   CLEAR                   msdcre
273400030210     c                   CLEAR                   mshcre
273500030210     c                   CLEAR                   mstcre
273600030210     c                   CLEAR                   msnom
273700030210     c                   CLEAR                   mscrT
273800030210     c                   CLEAR                   wmscrT
273900030210     c                   CLEAR                   s
274000030207     c
274100160127     c*imposta dati data consegna richiesta  DA BLP se bolla in partenza
274200160127     c                   if        esito='P'
274300160127     c                   eval      f_tasdcr=arbdcr
274400160127     c                   eval      f_tastcr=arbtcr
274500160127     c                   eval      f_tashcr=arbhcr
274600160127     c* imposto anche key spedizione
274700160309     c                   eval      f_tasaas=arbaas
274800160309     c                   eval      f_taslnp=arblnp
274900160309     c                   eval      f_tasnrs=arbnrs
275000160309     c                   eval      f_tasnsp=arbnsp
275100160127     c                   endif
275200160127     c
275300140716     c                   if        f_tasdcr > 0
275400140716     c                   MOVEL     f_tasdcr      wdat
275500140619     c                   EXSR      edtdat
275600140619     c                   eval      DTCONRCO74  =  wdate
275700140819if  3c                   IF        f_tastcr = ' '
275800140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
275900140819e   3c                   ENDIF
276000140819     c                   endif
276100140716     c                   if        f_tashcr > 0
276200140716     c                   MOVEL     f_tashcr      wora
276300140619     c                   EXSR      edtora
276400140619     c                   eval      ORACONRO74  =  worae
276500140619     c                   endif
276600140716     c                   eval      TPDATCRO74  =  f_tastcr
276700140619      *reperimento descrizione tipo consegna richiesta
276800140716if  3c                   IF        f_tastcr = 'P'
276900140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
277000140619e   3c                   ENDIF
277100140716if  3c                   IF        f_tastcr = 'D'
277200140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
277300140619e   3c                   ENDIF
277400000000     c*---
277500000000     c* memorizza le "note" che sono presenti nel record bolla
277600000000     c*---
277700000000     c* Consegna richiesta
277800140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
277900140619     c*m                           s_tashcr = *zeros
278000140619x   1c*m                 ELSE                                                   *SI cons.particolare
278100140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
278200140619     c*m                 MOVEL     s_tasdcr      wdat
278300140619     c*m                 EXSR      edtdat                                       *edita data
278400140619     c*m                 MOVEL     wdate         mdcre
278500140619e   2c*m                 ENDIF
278600140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
278700140619     c*m                 MOVEL     s_tashcr      wora
278800140619     c*m                 EXSR      edtora                                       *edita ora
278900140619     c*m                 MOVEL     worae         mhcre
279000140619e   2c*m                 ENDIF
279100140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
279200140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
279300140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
279400140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
279500140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
279600140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
279700140619e   3c*m                 ENDIF
279800140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
279900140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
280000140619e   3c*m                 ENDIF
280100140619if  3c*m                 IF        s_tastcr = ' '                               *il
280200140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
280300140619e   3c*m                 ENDIF
280400140619e   2c*m                 ENDIF
280500140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
280600140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
280700140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
280800140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
280900140619e   3c*m                 ENDIF
281000140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
281100140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
281200140619e   3c*m                 ENDIF
281300140619if  3c*m                 IF        s_tastcr = ' '                               *il
281400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
281500140619e   3c*m                 ENDIF
281600140619e   2c*m                 ENDIF
281700140619     c*m
281800140619if  2c*m                 IF        i < 10                                       *se c'è spazio
281900140619     c*m                 ADD       1             i                              *memorizza nota
282000140619     c*m                 EVAL      smnot(i) =
282100140619     c*m                           %TRIM(mtcre) +
282200140619     c*m                           ' '          +
282300140619     c*m                           %TRIM(mdcre) +
282400140619     c*m                           ' '          +
282500140619     c*m                           %TRIM(mhcre)
282600140619e   2c*m                 ENDIF
282700140619e   1c*m                 ENDIF
282800000000     c*
282900000000     c* Particolarità consegna -uno/due-
283000160127     c                   if        esito='P'
283100160309     c                   eval      f_tasftc = arbtc1
283200160314     c                   eval      f_tastc2 = arbtc2
283300160127     c                   endif
283400160127     c
283500140716if  1c                   IF        f_tasftc <> *blanks OR
283600140716     c                             f_tastc2 <> *blanks
283700140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
283800000000     c                   EXSR      decftc
283900000000     c                   MOVEL     wdftc         d1wdftc
284000140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
284100000000     c                   EXSR      decftc
284200000000     c                   MOVEL     wdftc         d2wdftc
284300000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
284400000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
284500060612     c                   EVAL      we = ' ' +
284600060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
284700000000x   2c                   ELSE
284800000000     c                   EVAL      we = *blanks
284900000000e   2c                   ENDIF
285000060612     c                   EVAL      mftce =
285100060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
285200060612     c                             %TRIM(d1wdftc)             +
285300060612     c                             we                         +
285400060612     c                             %TRIM(d2wdftc)
285500000000     c*
285600010927if  2c                   IF        i < 10                                       *se c'è spazio
285700000000     c                   ADD       1             i                              *memorizza nota
285800000000     c                   EVAL      smnot(i) = mftce
285900010927e   2c                   ENDIF
286000010927e   1c                   ENDIF
286100000000     c*
286200000000     c* Chiusura per turno
286300160127     c                   if        esito='P'
286400160309     c                   eval      f_tasgc1 = arbgc1
286500160309     c                   eval      f_tasgc1 = arbgc2
286600160127     c                   endif
286700160127     c
286800140716if  1c                   IF        f_tasgc1 <> *blanks OR
286900140716     c                             f_tasgc2 <> *blanks
287000140716if  2c                   IF        f_tasgc1 <> *blanks
287100140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
287200000000     c                   EXSR      decgcx
287300000000     c                   MOVEL     wdgcx         d1wdgcx
287400000000e   2c                   ENDIF
287500140716if  2c                   IF        f_tasgc2 <> *blanks
287600140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
287700000000     c                   EXSR      decgcx
287800000000     c                   MOVEL     wdgcx         d2wdgcx
287900000000e   2c                   ENDIF
288000000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
288100000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
288200060612     c                   EVAL      we = ' ' +
288300060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
288400000000x   2c                   ELSE
288500000000     c                   EVAL      we = *blanks
288600000000e   2c                   ENDIF
288700060612     c                   EVAL      mgcxe =
288800060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
288900060612     c                             %TRIM(d1wdgcx)       +
289000060612     c                             we                   +
289100060612     c                             %TRIM(d2wdgcx)
289200000000     c*
289300010927if  2c                   IF        i < 10                                       *se c'è spazio
289400000000     c                   ADD       1             i                              *memorizza nota
289500000000     c                   EVAL      smnot(i) = mgcxe
289600010927e   2c                   ENDIF
289700010927e   1c                   ENDIF
289800000000     c*
289900000000     c* Fermo deposito
290000160127     c                   if        esito='P'
290100160309     c                   eval      f_tasffd = arbffd
290200160127     c                   endif
290300160127
290400140716if  1c                   IF        f_tasffd = 'S'
290500060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
290600000000     c*
290700010927if  2c                   IF        i < 10                                       *se c'è spazio
290800010927     c                   ADD       1             i                              *memorizza nota
290900010927     c                   EVAL      smnot(i) = mffd
291000010927e   2c                   ENDIF
291100010927e   1c                   ENDIF
291200030206     c*---
291300030206     c* memorizza le "note" che NON sono presenti nel record bolla
291400030206     c* ma sul file estensione "anagrafiche"
291500030206     c*---
291600030206     c* N° telefono destinatario
291700160127     c                   if        esito='P'
291800160309     C                   EVAL      MitOrigO74 = arbrmo
291900160127     c                   else
292000140716     c                   Z-ADD     f_tasaas      kaaaas
292100140716     c                   Z-ADD     f_taslnp      kaalnp
292200140716     c                   Z-ADD     f_tasnrs      kaanrs
292300140716     c                   Z-ADD     f_tasnsp      kaansp
292400030206     c                   MOVEL     'O'           kaatrc
292500030206     c     keytaa        CHAIN     titaa30c                           98
292600030206if  1c                   IF        NOT *in98
292700040330     C                   EVAL      MitOrigO74 = TAARSC
292800030206e   1c                   ENDIF
292900160127e   1c                   ENDIF
293000030206     c*---
293100030206     c* memorizza le "note" che NON sono presenti nel record bolla
293200030206     c* ma sul file estensione testata boolle
293300030206     c*---
293400030206     c* Bancali
293500140716     c                   Z-ADD     f_tasaas      kr5aas
293600140716     c                   Z-ADD     f_taslnp      kr5lnp
293700140716     c                   Z-ADD     f_tasnrs      kr5nrs
293800140716     c                   Z-ADD     f_tasnsp      kr5nsp
293900030206     c                   MOVEL     'BAN'         kr5trd
294000160127     c                   if        esito='P'
294100160127     c     keyar5        CHAIN     fiar501l                           98
294200160127     c                   else
294300040531     c     keyar5        CHAIN     fiar531c                           98
294400160127     c                   endif
294500160127     c
294600030206if  1c                   IF        NOT *in98
294700030206     c                   MOVEL     ar5uni        dar5ban
294800060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
294900060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
295000030206if  2c                   IF        §ar5rb1 > *zeros OR
295100030206     c                             §ar5rb2 > *zeros
295200060613     c                   EVAL      mban = %trim(mban) +
295300060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
295400060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
295500030206e   2c                   ENDIF
295600030206     c*
295700030206if  2c                   IF        i < 10                                       *se c'è spazio
295800030206     c                   ADD       1             i                              *memorizza nota
295900030206     c                   EVAL      smnot(i) = mban
296000030206e   2c                   ENDIF
296100030206e   1c                   ENDIF
296200040330
296300040330     **?Reperisco referente
296400030206     c* Note supermercati
296500140716if  1c     f_tasftc      IFEQ      'S'
296600140716     c     f_tastc2      OREQ      'S'
296700030210     c                   CLEAR                   s                              *indice
296800140716     c                   Z-ADD     f_tasaas      kr5aas
296900140716     c                   Z-ADD     f_taslnp      kr5lnp
297000140716     c                   Z-ADD     f_tasnrs      kr5nrs
297100140716     c                   Z-ADD     f_tasnsp      kr5nsp
297200030206     c                   MOVEL     'SCR'         kr5trd
297300160127     c                   if        esito='P'
297400160127     c     keyar5        SETLL     fiar501l
297500160127     c     keyar5        READE     fiar501l                               98
297600160127     c                   else
297700040531     c     keyar5        SETLL     fiar531c
297800040531     c     keyar5        READE     fiar531c                               98
297900160127     c                   endif
298000160127     c
298100030210do  2c                   DOW       NOT *in98
298200030206     c                   MOVEL     ar5uni        dar5scr
298300030207     c* data nota
298400030210if  3c                   IF        ar5dac > *zeros
298500030207     c                   MOVEL     ar5dac        wdat
298600030207     c                   EXSR      edtdat                                       *edita data
298700030210     c                   MOVEL     wdate         msdace
298800030210e   3c                   ENDIF
298900030207     c* ora nota
299000030210if  3c                   IF        ar5orc > *zeros
299100030207     c                   MOVEL     ar5orc        wora
299200030207     c                   EXSR      edtora                                       *edita ora
299300030210     c                   MOVEL     worae         msorce
299400030210e   3c                   ENDIF
299500030207     c* Consegna richiesta
299600140708if  3c                   IF        §ar5dcr = *zeros AND
299700140708     c                             §ar5hcr = *zeros
299800140708x   3c                   ELSE
299900140708if  4c                   IF        §ar5dcr > *zeros
300000140708     c                   MOVEL     §ar5dcr       wdat
300100140708     c                   EXSR      edtdat                                       *edita data
300200140708     c                   MOVEL     wdate         msdcre
300300140708e   4c                   ENDIF
300400140708if  4c                   IF        §ar5hcr > *zeros
300500140708     c                   MOVEL     §ar5hcr       wora
300600140708     c                   EXSR      edtora                                       *edita ora
300700140708     c                   MOVEL     worae         mshcre
300800140708e   4c                   ENDIF
300900140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
301000140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
301100140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
301200140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
301300140708if  5c                   IF        §ar5tcr = 'P'                                *prima
301400140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
301500140708e   5c                   ENDIF
301600140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
301700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
301800140708e   5c                   ENDIF
301900140708if  5c                   IF        §ar5tcr = ' '                                *il
302000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
302100140708e   5c                   ENDIF
302200140708e   4c                   ENDIF
302300140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
302400140708     c                             §ar5dcr = *zeros                             *NO data richiesta
302500140708if  5c                   IF        §ar5tcr = 'P'                                *prima
302600140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
302700140708e   5c                   ENDIF
302800140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
302900140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
303000140708e   5c                   ENDIF
303100140708if  5c                   IF        §ar5tcr = ' '                                *il
303200140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
303300140708e   5c                   ENDIF
303400140708e   4c                   ENDIF
303500030210e   3c                   ENDIF
303600030210     c* Riferimento
303700030210if  3c                   IF        §ar5nom <>  *blanks
303800060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
303900060613     c                             + ' ' + §ar5nom
304000030210e   3c                   ENDIF
304100030210     c*
304200030210if  3c                   IF        s < 5                                        *MAX 5 note
304300030210     c                   ADD       1             s
304400030210     c                   EVAL      mscrT(s) = 'Il ' +
304500030210     c                             %trim(msdace) +                              *data nota
304600030210     c                             ' '    +
304700030210     c                             %trim(msorce) +                              *ora nota
304800030210     c                             ' '    +
304900140708     c                             %trim(mstcre) +                              *tipo consegna
305000140708     c                             ' '    +
305100140708     c                             %trim(msdcre) +                              *data consegna
305200140708     c                             ' '    +
305300140708     c                             %trim(mshcre) +                              *ora consegna
305400140708     c                             ' '    +
305500030210     c                             %trim(msnom)  +                              *parlato con
305600030210     c                             ' '    +
305700030210     c                             %trim(§ar5tel) +                             *telefono
305800030210     c                             ' '    +
305900030210     c                             %trim(§ar5mot)                               *motivo
306000030210e   2c                   ENDIF
306100030210     c*
306200160127     c                   if        esito='P'
306300160127     c     keyar5        READE     fiar501l                               98
306400160127     c                   else
306500160127     c     keyar5        READE     fiar531c                               98
306600160127e   1c                   ENDIF
306700030206e   2c                   ENDDO
306800030210e   1c                   ENDIF
306900030210     c* NOTA completa
307000030210if  1c                   IF        mscrT(1) <> *blanks
307100030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
307200030211     c                                      ' ' +
307300030211     C                                      %trim(mscrT(2)) +
307400030211     c                                      ' ' +
307500030211     C                                      %trim(mscrT(3)) +
307600030211     c                                      ' ' +
307700030211     C                                      %trim(mscrT(4)) +
307800030211     c                                      ' ' +
307900030211     C                                      %trim(mscrT(5))
308000030210     c                   Z-ADD     *zeros        s
308100030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
308200030210     c                   ADD       1             i
308300030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
308400030210     c                   EVAL      s = s + 100
308500030210e   2c                   ENDDO
308600030210     c*
308700030210e   1c                   ENDIF
308800010927     c*---
308900010927     c* memorizza le "note" che NON sono presenti nel record bolla
309000160127     c* ma sul file estensione
309100010927     c*---
309200160127     c
309300160127    0c                   if        esito<>'P'
309400010927     c                   CLEAR                   tita4000
309500010927     c                   CLEAR                   tita4p00
309600140716     c                   Z-ADD     f_tasaas      ka4aas
309700140716     c                   Z-ADD     f_taslnp      ka4lnp
309800140716     c                   Z-ADD     f_tasnrs      ka4nrs
309900140716     c                   Z-ADD     f_tasnsp      ka4nsp
310000160127     c
310100010927     c     ke2ta4        SETLL     tita430c
310200010927     c     ke2ta4        READE     tita430c                               98
310300010927do  1c                   DOW       NOT *in98
310400140708     c                   if        ta4trc = '8' OR ta4trc = '9'
310500050701     c*
310600050701if  3c                   IF        ta4trc = '8'
310700050701     c                   EVAL      mmnc8 = ta4not
310800050701e   3c                   ENDIF
310900050701if  3c                   IF        ta4trc = '9'
311000050701     c                   EVAL      mmnc9 = ta4not
311100050701e   3c                   ENDIF
311200050701     c*
311300010927e   2c                   ENDIF
311400010925     c*
311500010926     c     ke2ta4        READE     tita430c                               98
311600010926e   1c                   ENDDO
311700050701     C*
311800140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
311900050701     C*
312000050701     C                   IF        mmnc8 <> *BLANK
312100050701     C                   EVAL      a230 = %TRIML(mmnc8)
312200050701     C                   ENDIF
312300050701     C                   IF        mmnc9 <> *BLANK
312400050701     C                   IF        a230 <> *BLANK
312500050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
312600050701     C                   ELSE
312700050701     C                   EVAL      a230 = %TRIML(mmnc9)
312800050701     C                   ENDIF
312900050701     C                   ENDIF
313000010927     C*
313100050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
313200050701     C*
313300010927if  2c                   IF        i < 10                                       *se c'è spazio
313400010927     c                   ADD       1             i                              *memorizza nota
313500010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
313600010927e   2c                   ENDIF
313700010927     C*
313800010927if  2c                   IF        i < 10                                       *se c'è spazio
313900010927if  3c                   IF        n3 > 100
314000010927     c                   ADD       1             i                              *memorizza nota
314100010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
314200010927e   3c                   ENDIF
314300010927e   2c                   ENDIF
314400010927     C*
314500010927if  2c                   IF        i < 10                                       *se c'è spazio
314600010927if  3c                   IF        n3 > 200
314700010927     c                   ADD       1             i                              *memorizza nota
314800010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
314900010927e   3c                   ENDIF
315000010927e   2C                   ENDIF
315100010927e   1c                   ENDIF
315200160127e   0c                   ENDIF
315300010925     c*
315400000000     c* imposta contatore note da visualizzare nella DS di Output
315500000000     c                   EVAL      cntnoto74 = i
315600000000     c*
315700000000     c* imposta le note nella DS di Output
315800000000do  1c     1             DO        10            j
315900000000     c                   EVAL      nota(j) = smnot(j)
316000000000e   1c                   ENDDO
316100000000     c*
316200000000     c                   ENDSR
316300010716     c*--------------------------------------------------------------------------------------------*
316400010716     c* imposta i campi della DS di Output dalla firma DPD
316500010716     c*--------------------------------------------------------------------------------------------*
316600010716     c     impdsoDPD     BEGSR
316700010717     c*
316800010717     c* reimposta variabili di lavoro
316900010717     c                   RESET                   $AbFirma
317000010717     c                   CLEAR                   depute                         *deposito utente
317100010717     c                   CLEAR                   deppwd                         *deposito password
317200010717     c*---
317300010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
317400010717     c*---
317500010717     c                   Z-ADD     s_taslna      worg
317600010717     c                   EXSR      decorg
317700160406     c
317800020702     c                   IF        wodpd = 'DPD'
317900020702     c                   MOVEL     'S'           dpdo74
318000020702     c                   ELSE
318100020702     c                   MOVEL     *blanks       dpdo74
318200160406     c
318300160406     c* se si tratta di bolla EEX verifico se il partner è abilitato al link
318400160407     c                   IF        wodpd = 'EEX' and ksci74<>*blanks
318500160406     c                   clear                   fidn40ds
318600160406     c                   eval      i40fil=s_taslna
318700160406     c                   z-add     s_tasmgs      i40dsp
318800160406     c                   MOVEL     s_tasaas      i40dsp                          *data spedizione
318900160406     c                   call      'FIDN40R'
319000160406     c                   parm                    fidn40ds
319100160407
319200160406     c                   if        o40lnk='S'
319300160407     c* sommo i giorni alla data spedizione per sapere se ancora visibile
319400160407     c                   Clear                   xgiolavds
319500160407     c                   Eval      Ixglpa  = 'A'
319600160413     c                   Eval      IxglFil = s_taslna
319700160407     c                   Eval      Ixgldata = i40dsp
319800160407     c                   Eval      Ixgladd = 'S'
319900160407     c                   eval      ixglgg  = o40gio
320000160407     c                   Call      'XGIOLAV'
320100160407     c                   parm                    xgiolavds
320200160407     c                   if        oxgldata>=datcor
320300160406     c                   MOVEL     'E'           dpdo74
320400160407     c                   endif
320500160407     c
320600160406     c                   endif
320700160406     c                   endif
320800160406     c
320900020702     c                   ENDIF
321000010717     c*
321100010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
321200060627     c* Riferimento link Lettera di vettura
321300060627     c* Riferimento link Firma
321400010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
321500010717     c                   CLEAR                   tita4000
321600010717     c                   CLEAR                   tita4p00
321700060627     c                   CLEAR                   dsbl4i
321800010717     c                   Z-ADD     s_tasaas      ka4aas
321900010717     c                   Z-ADD     s_taslnp      ka4lnp
322000010717     c                   Z-ADD     s_tasnrs      ka4nrs
322100010717     c                   Z-ADD     s_tasnsp      ka4nsp
322200060627     c                   MOVEL     'I'           ka4trc
322300060627     c     keyta4        CHAIN     tita430c                           98
322400010717if  2c                   IF        NOT *in98
322500060627     c                   MOVEL     ta4not        dsbl4i
322600060627     c                   EVAL      DPDRmnO74 = §b4ipn
322700060627     c                   EVAL      DPDFnpO74 = §b4ipn
322800010717e   2c                   ENDIF
322900010717     c*
323000010717     c* cerca abilitazione firma DPD per questo segnacollo
323100010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
323200010717     c                   EXSR      RepAbilFirma
323300010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
323400010717     c*---
323500010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
323600010717     c*---
323700010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
323800010717     c* dati
323900010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
324000010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
324100010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
324200010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
324300010717     c*
324400010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
324500010717     c                   Z-ADD     s_tasaas      kvbaas
324600010717     c                   Z-ADD     s_taslnp      kvblnp
324700010717     c                   Z-ADD     s_tasnrs      kvbnrs
324800010717     c                   Z-ADD     s_tasnsp      kvbnsp
324900010717     c                   Z-ADD     99999999      kvbdev
325000010717     c                   Z-ADD     9999          kvbhev
325100010717     c     ke2evb        SETGT     fnevb01l
325200010717     c     keyevb        READPE    fnevb01l                               98
325300010717do  4c                   DOW       NOT *in98
325400101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
325500010720     c*
325600060629     c                   EVAL      devB01 = evbNot
325700060628     c                   IF        §NotDep = *BLANK
325800060628     c                   RESET                   DPDFirO74
325900060629     c                   LEAVE
326000060628     c                   ENDIF
326100060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
326200060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
326300080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
326400010720     c                   Z-ADD     *zeros        g08dat
326500010720     c                   MOVEL     '3'           g08err
326600010720     c                   CALL      'XSRDA8'
326700010720     c                   PARM                    wlbda8
326800010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
326900010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
327000010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
327100010720     c*
327200010717     c                   LEAVE                                                  *uscita ciclo
327300010717e   5c                   ENDIF
327400010717     c     keyevb        READPE    fnevb01l                               98
327500010717e   4c                   ENDDO
327600010717     c*
327700010717e   3c                   ENDIF
327800010717e   2c                   ENDIF
327900160323     c
328000160323   x1c                   else
328100010716e   1c                   ENDIF
328200010716     c*
328300010716     c                   ENDSR
328400160309     c*--------------------------------------------------------------------------------------------*
328500160309     c* impostazioni solo da FNBLP
328600160309     c*--------------------------------------------------------------------------------------------*
328700160309     c     impds_soloBLP BEGSR
328800160321     c
328900160321     c                   EVAL      dstaslnp = arblnp
329000160321     c                   EVAL      dstasnrs = arbnrs
329100160321     c                   EVAL      dstasnsp = arbnsp
329200160321     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
329300160314     C
329400160314     c                   Z-ADD     arblna        worg
329500160314     c                   exsr      setLnaItaEst
329600160314     C
329700160309     c* chain ar5 con bolla richiesta a video
329800160309     c                   Z-ADD     kasaas        kr5aas
329900160309     c                   Z-ADD     kaslnp        kr5lnp
330000160309     c                   Z-ADD     kasnrs        kr5nrs
330100160309     c                   Z-ADD     kasnsp        kr5nsp
330200160309     c                   MOVEL     'GEN'         kr5trd
330300160309     c     keyar5        CHAIN     fiar501l
330400160309     C                   IF        %FOUND
330500160309     C                   EVAL      DAR5GEN = AR5Uni
330600160309     C                   ELSE
330700160309     C                   RESET                   DAR5GEN
330800160309     C                   ENDIF
330900160309     C                   EVAL      REFCONSO74 = GEN§AR5REF
331000160309     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
331100160309     c*
331200160309     c* chain tipo bolla
331300160321     c                   eval      tblkey=arbcbo
331400160314     C                   EVAL      ds3A = chainTabel00f('CHAIN3':langTabel
331500160321     C                             :'3A':tblkey:%LEN(tblkey):'TBLUNI':
331600160314     C                             rpyOpCode:rpyEsito)
331700160309     c*
331800160309     c                   MOVEL     §3atb1        a1
331900160309     c                   MOVEL     §3atb1        tpporto74
332000160309if  1c                   IF        a1 = 'A'                                     *Pagamento del
332100160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
332200160309x   1c                   ELSE                                                   *Pagamento del
332300160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
332400160309e   1c                   ENDIF
332500160314     c
332600160314     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
332700160314     C                   IF        KSCI74 <> *BLANK
332800160314     c                   SETOFF                                           98
332900160314     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
333000160314     C                   RESET                   tis7700i0
333100160314     C                   EVAL      tis7700i0.aas = arbAas
333200160314     C                   EVAL      tis7700i0.lnp = arbLnp
333300160314     C                   EVAL      tis7700i0.nrs = arbNrs
333400160314     C                   EVAL      tis7700i0.nsp = arbNsp
333500160314     C                   EVAL      tis7700i0.tbl = §3atb1
333600160314     C                   EVAL      tis7700i0.ksu = kscI74
333700160314     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
333800160314     C***                CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
333900160314     C***                          : rpyEsito
334000160314     C***                          : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
334100160314     C***                          : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
334200160314     C***                          )
334300160314     C***                EVAL      *IN98 = (rpyEsito >= *ZERO AND
334400160314     C***                          tis7700o0.bollValida = *ON)
334500160314     **?Se non gli appartiene, prima di restituire errore controllo il check code.
334600160314if  2c                   IF        NOT *in98                                      *non trovato
334700160314     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
334800160314     C                   reset                   TIS778DSO
334900160818     C*****              EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
335000160818     c                   eval      esito='1'
335100160818     c                   eval      widmsg = 'TIS0849'
335200160818     c                   eval      widcampo = 'NSPEDIZI74'
335300160818     c                   clear                   wparm
335400160818     c                   exsr      messaggi
335500160818     c                   exsr      routend
335600160818     C******             RETURN
335700160314     C                   ENDIF
335800160314e   2c                   ENDIF
335900160314     C                   ENDIF
336000160314
336100160314     c
336200160309     c                   ENDSR
336300160314     c*--------------------------------------------------------------------------------------------*
336400160314     c* impostazioni del C/Assegno
336500160314     c*--------------------------------------------------------------------------------------------*
336600160314     c     ImpdsoAR9     BEGSR
336700160314     c                   Z-ADD     waas          ksbaas
336800160314     c                   Z-ADD     wlnp          ksblnp
336900160314     c                   Z-ADD     wnrs          ksbnrs
337000160314     c                   Z-ADD     wnsp          ksbnsp
337100160314     c     keycsb        CHAIN     fiar901l                           99
337200160314if  2c                   IF        NOT *in99                                    *esistente
337300160314     c* memorizza i dati
337400160314     c                   MOVEL     'S'           cao74                          *esiste C/A
337500160314     c*
337600160314     c                   Z-ADD     ar9cas        caimpo74                       *importo
337700160314     c*
337800160314     c                   EVAL      cadivo74 =  ar9vca                           *divisa
337900160314if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
338000160314     c                             caimpo74 > *zeros
338100160314     c                   EVAL      cadivo74 = 'ITL'
338200160314e   3c                   ENDIF
338300160314
338400160314     ** Visualizzo la descrizione del tipo incasso
338500160314     c**  devo impostare alcuni campi di CSB
338600160314     c                   eval      csbaas=ar9aas
338700160314     c                   eval      csblnp=ar9lnp
338800160314     c                   eval      csbnrs=ar9nrs
338900160314     c                   eval      csbnsp=ar9nsp
339000160314     c                   clear                   csbddc
339100160314     c                   clear                   csbtpa
339200160314     c                   clear                   csbtpi
339300160314     c
339400160314     C                   EVAL      cainco74 =
339500160314     C                                       GetDescrizioneIncassoContrassegno()
339600160314     c* stato C/Assegno : attivo
339700160314     c                   MOVEL     *blanks       castao74                       *stato
339800160314     C                   EVALR     tblKey = '0'
339900160314     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
340000160314     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
340100160314     C                             :rpyOpCode:rpyEsito)
340200160314     C                   IF        rpyOpCode = 'FOUND'
340300160314     c                   EVAL      castao74 = §4SDei
340400160314e   3c                   ENDIF
340500160314
340600160314if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
340700160314     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)     *NO distinta incasso
340800160314     c                   endif
340900160314
341000160314     c                   endif
341100160314     c
341200160314     c                   ENDSR
341300140604     c*--------------------------------------------------------------------------------------------*
341400140604     c* imposta i campi da FNARB se TASDCM = 0
341500140604     c*--------------------------------------------------------------------------------------------*
341600140604     c     impdsoARB     BEGSR
341700150403     c                   clear                   orarich           4 0
341800150611     c                   clear                   wdee
341900150410     c* memorizzo se utilizzo la data prevista consegna
342000150410     c                   clear                   usaprev           1
342100140604     c*
342200140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
342300140604     c                   EVAL      lnaDescO74 = sodes(i)
342400140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
342500140604     c*
342600150116      *eliminata forzatura resta impostato dalla mamma per composizione link
342700150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
342800150116     c*                  MOVE      arbmgs        aasmgsO74
342900140818      *riempimento da verificare
343000140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
343100140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
343200140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
343300140818
343400140818     c                   if        tpporto74 = 'A'
343500140818     c                   if        arbccm > 0
343600140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
343700140818     c                   else
343800140818     c                   movel     arblnp        ccmO74
343900140818     c                   move      8888          ccmO74
344000140818     c                   endif
344100140818     c                   else
344200140818     c                   z-add     arbksc        ccmO74
344300140818     c                   endif
344400140818      *
344500140604     c                   MOVEL     arblna        lineaaro74
344600140604     c                   MOVEL     arbtsp        tpservio74
344700140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
344800140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
344900140604     C                             rpyOpCode:rpyEsito)
345000140604     C                   EVAL      corro74 = §5EDes
345100140604     c*
345200140604     c                   EVAL      destino74 = arbrsd
345300140604     c                   EVAL      desindo74 = arbind
345400140604     c                   EVAL      descapo74 = arbcad
345500140604     c                   EVAL      desloco74 = arblod
345600140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
345700140604if  1c                   IF        arbnzd    = *blanks
345800140604     c                   EVAL      desnazo74 = arbprd
345900140604x   1c                   ELSE
346000150326     c**                 EVAL      desnazo74 = arbnzd
346100150326     c**                 EVAL      desnzdo74 = arbnzd
346200150326     c* 26/03/15: Attiviamo le specifiche per provincia export
346300150326     c                   EVAL      desnazo74 = arbprd
346400150326     c                   EVAL      desnzdo74 = arbnzd
346500150326e   1c                   ENDIF
346600140604     c*
346700140604     c                   EVAL      collio74  = arbncl
346800140604     c                   EVAL      pesoo74   = arbpkf
346900140604     c                   EVAL      volumeo74 = arbvlf
347000140604     c                   EVAL      naturao74 = arbnas
347100140604     c*
347200140604     c                   EVAL      disnumo74  = arbndc
347300140604c    c                   MOVEL     arbddc        wdat
347400140604     c                   EXSR      edtdat
347500140604     c                   EVAL      disdatao74 = wdate
347600140604     c*
347700140604     c                   EVAL      mito74    = arbrsm
347800140604     c                   EVAL      mitindo74 = arbinm
347900140604     c                   EVAL      mitcapo74 = arbcam
348000140604     c                   EVAL      mitloco74 = arblom
348100140604      *specifiche da eliminare dopo la modifica in immissione bolle
348200140604if  3c                   IF        arbnzm = *blanks
348300140604     c                   EVAL      mitnazo74 = arbprm
348400140604x   3c                   ELSE
348500140604     c                   EVAL      mitnazo74 = arbnzm
348600140604e   3c                   ENDIF
348700140604     c* 2a ragione sociale destinatario.
348800140604     c                   Z-ADD     arbaas        ka4aas
348900140604     c                   Z-ADD     arblnp        ka4lnp
349000140604     c                   Z-ADD     arbnrs        ka4nrs
349100140604     c                   Z-ADD     arbnsp        ka4nsp
349200140604     c                   MOVEL     'D'           ka4trc
349300140604     c     keyta4        CHAIN     fiar401l                           99
349400140604if  1c                   IF        NOT *in99
349500140604     c                   MOVEL     ar4not        desti2O74
349600140604e   1c                   ENDIF
349700140604     c                   MOVEL     'E'           ka4trc
349800140604     c     keyta4        CHAIN     fiar401l                           99
349900140604if  1c                   IF        NOT *in99
350000140604     c                   MOVEL     ar4not        dsbl4e
350100140604e   1c                   ENDIF
350200140604     c
350300140604     ** Riferimento mittente numerico (c'è sempre).
350400140604     C                   EVAL      rifero74 = arbrmn
350500140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
350600140604     C                   IF        arbrma <> *blank and
350700140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
350800140604     C                   EVAL      rifero74a = arbrma
350900140604     C                   ENDIF
351000140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
351100140604     C                   IF        §b4erp <> *BLANK AND
351200140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
351300140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
351400140604     C                   EVAL      rpto74 = §b4erp
351500140604     C                   ENDIF
351600140604
351700140604     **?Reperisco il check code della spedizione.
351800140604     C                   EVAL      SChkCdeO74 =
351900140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
352000140604     C                             NSpedizI74:ChkCode:nEsito)
352100140604
352200140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
352300141126     C*m                 IF        KSCI74 <> *BLANK
352400141126     c*m                 SETOFF                                           98
352500140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
352600141126     C*m                 RESET                   tis7700i0
352700141126     C*m                 EVAL      tis7700i0.aas = arbAas
352800141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
352900141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
353000141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
353100141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
353200141126     C*m                 EVAL      tis7700i0.ksu = kscI74
353300141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
353400141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
353500141126     C*m                           : rpyEsito
353600141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
353700141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
353800141126     C*m                           )
353900141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
354000141126     C*m                           tis7700o0.bollValida = *ON)
354100140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
354200141126if  2c*m                 IF        NOT *in98                                      *non trovato
354300141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
354400141126     C*m                 reset                   TIS778DSO
354500141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
354600141126     C*m                 RETURN
354700141126     C*m                 ENDIF
354800141126e   2c*m                 ENDIF
354900141126     C*m                 ENDIF
355000140604     c*
355100140604     ** Importo assicurato.
355200140604     C                   IF        arbIas > 0
355300140604     C                   EVAL      vasO74 = arbVas
355400140604     C                   EVAL      iasO74 = arbIas
355500140604     C                   ENDIF
355600140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
355700140605     c                   if        arbdcm = 0 and wcev <> '704'
355800140617     c                   if        arbdcr > 0
355900140605     c                   MOVEL     arbdcr        wdat
356000140605     c                   EXSR      edtdat
356100140605     c                   eval      DTCONRCO74  =  wdate
356200140819if  3c                   IF        arbtcr = ' '
356300140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
356400140819e   3c                   ENDIF
356500140617     c                   endif
356600140617     c                   if        arbhcr > 0
356700140617     c                   MOVEL     arbhcr        wora
356800140617     c                   EXSR      edtora
356900140617     c                   eval      ORACONRO74  =  worae
357000140617     c                   endif
357100140606     c                   eval      TPDATCRO74  =  arbtcr
357200160407     c
357300140605      *reperimento descrizione tipo consegna richiesta
357400140617if  3c                   IF        arbtcr = 'P'
357500140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
357600140617e   3c                   ENDIF
357700140617if  3c                   IF        arbtcr = 'D'
357800140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
357900140617e   3c                   ENDIF
358000160407     c
358100160407     c
358200160407     c* Per  le bolle in partenza eseguo solo se presente la data entrata
358300160407    0c                   if        esito<>'P' or arbdse>0
358400160407     c
358500140605     C                   clear                   tnsd99ds
358600140605     C                   MOVEL     ' '           D98tla
358700160314     c                   if        esito='P'
358800160314     C                   MOVEL     'P'           D98tbo
358900160314     c                   else
359000140605     C                   MOVEL     'A'           D98tbo
359100160314     c                   endif
359200140605     C                   Z-ADD     arbaas        D98aas
359300140605     C                   Z-ADD     arblnp        D98lnp
359400140605     C                   Z-ADD     arbnrs        D98nrs
359500140605     C                   Z-ADD     arbnsp        D98nsp
359600140605     C*
359700140605     C                   call      'TNSD99R'
359800140605     C                   parm                    tnsd99ds
359900150304     c
360000140605     C*
360100140917      * chiamata a trulors per tempi standard
360200140917     c                   clear                   trulorsds
360300140917     c                   clear                   trulor2ds
360400140917     c                   clear                   trulor3ds
360500150611     c                   eval      wdee=d98dee
360600140917      * solo se questa data è valorizzata
360700150304    1c                   if        d98dee > 0
360800140912     c                   movel     d98dee        wdat
360900140605     c                   exsr      edtdat
361000140605     c                   eval      DTTEOCOO74  =  wdate
361100140819     c
361200140904      * se fermo deposito non richiamo pgm per reperimento orari
361300150304    2c                   if         arbffd = *blank
361400140605     c                   eval       IOREtser = 'C'
361500140605     c                   eval       IOREDDC=arbddc
361600140605     c                   eval       IOREnDC=arbndc
361700140609     c                   eval       IOREfil=arblna
361800140609     c                   eval       IOREdta=d98dce
361900140609     c                   eval       IOREcap=arbcad
362000140609     c                   eval       IOREloc=arblod
362100151019     c*****              eval       IOREtfp=arbtfp
362200151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
362300151019     c* del terminal di partenza originale
362400151019     c                   if         d98flp>*zeros
362500151019     c                   eval       IOREtfp=%INT(d98flp)
362600151019     c                   else
362700151019     c                   eval       IOREtfp=arbtfp
362800151019     c                   endif
362900140609     c                   eval       IOREtfa=arbtfa
363000140610     c                   eval       IOREdti=arbdti
363100140610     c                   eval       IOREhti=arbhti
363200140610     c                   eval       IOREdcr=arbdcr
363300140610     c                   eval       IOREhcr=arbhcr
363400140610     c                   eval       IOREtcr=arbtcr
363500140819     c                   eval       IOREtsp=arbtsp
363600140819     c                   eval       IOREnar=arbnzd
363700140819     c                   eval       IOREdei=d98dei
363800140819     c                   eval       IOREoei=d98oei
363900150819     c                   clear                   diore01
364000150819     c                   movel     d98ttc        §ioretrazC
364100150819     c                   movel     d98tcc        §ioreconsC
364200150819     c                   eval       IOREflo=diore01
364300150325     c* passo la località normalizzata
364400150325     c                   clear                   tita4000
364500150325     c                   clear                   tita4p00
364600150325     c                   Z-ADD     arbaas        ka4aas
364700150325     c                   Z-ADD     arblnp        ka4lnp
364800150325     c                   Z-ADD     arbnrs        ka4nrs
364900150326     c                   Z-ADD     arbnsp        ka4nsp
365000150325     c                   MOVEL     'X'           ka4trc
365100160314     c                   if        esito='P'
365200160314     c     keyta4        CHAIN     fiar401l                           99
365300160314if  1c                   IF        NOT *in99
365400160314     c                   MOVEL     ar4not        OOR3LOC_N
365500160314     c                   else
365600160314     c                   movel     arblod        OOR3LOC_N
365700160314e   1c                   ENDIF
365800160314     c
365900160314     c                   else
366000160314     c     keyta4        CHAIN     tita430c                           99
366100150325if  1c                   IF        NOT *in99
366200150325     c                   MOVEL     ta4not        OOR3LOC_N
366300150325     c                   else
366400150325     c                   movel     arblod        OOR3LOC_N
366500150325e   1c                   ENDIF
366600160314e   1c                   ENDIF
366700140605     c
366800160321     c                   if        esito<>'P'
366900150304    3c                   IF         (ARBfbc = *blanks and
367000140605     c                               ARBddc > 0 and
367100140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
367200140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
367300140605     c                   eval       IOREcons = 'S'
367400150304    3c                   endif
367500160321     c
367600140605     c* Orario presunto ritiro/consegna
367700140605     c                   move      arbndc        pctndc
367800140605     c                   move      arbifp        pctfgs
367900140605     c                   move      'COR'         ttrd
368000140605     c     kcet          setgt     fipct02l
368100140605     c     kcet          readpe    fipct02l
368200150304    3c                   if        not %eof(fipct02l)
368300140605     c                   movel     pctdati       fipctcords
368400140605     c                   eval      ioreorp=§PCTORASTI
368500140605     c                   endif
368600160321    3c                   endif
368700160321
368800140605     c                   call      'TRULORSR'
368900140605     C                   PARM                    kpjba
369000140605     C                   PARM                    trulorsds
369100140605     C                   PARM                    trulor2ds
369200140605     C                   PARM                    trulor3ds
369300150304    3c                   endif
369400140818     c                   clear                   ORSTINIO74
369500150304    3c                   if        OOR2STIS > 0
369600140617     c                   MOVEL     OOR2STIS      wora
369700140617     c                   EXSR      edtora
369800140617     c                   eval      ORSTINIO74  =  worae
369900150304    3c                   endif
370000140818     c                   clear                   ORSTFINO74
370100150304    3c                   if        OOR2STFS > 0
370200140617     c                   MOVEL     OOR2STFS      wora
370300140617     c                   EXSR      edtora
370400140617     c                   eval      ORSTFINO74  =  worae
370500150304    3c                   endif
370600140818     c                   clear                   ORMIINIO74
370700150304    3c                   if        OOR2MIIS > 0
370800140617     c                   MOVEL     OOR2MIIS      wora
370900140617     c                   EXSR      edtora
371000140617     c                   eval      ORMIINIO74  =  worae
371100150304    3c                   endif
371200140818     c                   clear                   ORMAFINO74
371300150304    3c                   if        OOR2MXFS > 0
371400140617     c                   MOVEL     OOR2MXFS      wora
371500140617     c                   EXSR      edtora
371600140617     c                   eval      ORMAFINO74  =  worae
371700150304    3c                   endif
371800150304    3c                   if        wricons = 'S'
371900140818     c                   clear                   ORRIDALO74
372000150304    4c                   if        OOR2RIDS > 0
372100140617     c                   MOVEL     OOR2RIDS      wora
372200140617     c                   EXSR      edtora
372300140617     c                   eval      ORRIDALO74  =  worae
372400150304    4c                   endif
372500140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
372600140818     c                   clear                   ORRIALLO74
372700150304    4c                   if        OOR2RIAS > 0
372800140617     c                   MOVEL     OOR2RIAS      wora
372900140617     c                   EXSR      edtora
373000140617     c                   eval      ORRIALLO74  =  worae
373100150304    4c                   endif
373200140617     c                   eval      FLRICONO74  =  OOR2FRIC
373300150304   x3c                   else
373400140711     c                   clear                   ORRIALLO74
373500140711     c                   clear                   ORRIDALO74
373600140711     c                   eval      FLRICONO74  =  'N'
373700150304    3c                   endif
373800140818     c                   clear                   ORSTICDO74
373900150304    3c                   if        OOR2PRESD > 0
374000140617     c                   MOVEL     OOR2PRESD     wora
374100140617     c                   EXSR      edtora
374200140617     c                   eval      ORSTICDO74  =  worae
374300150304    3c                   endif
374400140818     c                   clear                   ORSTICAO74
374500150304    3c                   if        OOR2PRESa > 0
374600140617     c                   MOVEL     OOR2PRESa     wora
374700140617     c                   EXSR      edtora
374800140617     c                   eval      ORSTICAO74  =  worae
374900150304    3c                   endif
375000140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
375100140605     c                   eval      FLLOCNOO74  =  OOR3NORM
375200140904      *end F.D.
375300150304    2c                   endif
375400140605      * RIFERIMENTI DESTINATARIO
375500140605     c                   MOVEL     'EMD'         kr5trd
375600140605     c     keyAR5        CHAIN     fiar501l
375700150304if  2c                   IF        %found(fiar501l)
375800140605     c                   MOVEL     ar5uni        dar5emd
375900140606     c                   eval      EMLDESTO74  =  §§AR5EML
376000140606     c                   eval      CELDESTO74  =  §§AR5TEL
376100150304e   2c                   ENDIF
376200140604     **
376300150304     c*
376400150304     c* Stato data prevista consegna
376500150305     c                   clear                   stat_n2o74
376600150305     c                   eval      STATUSo74   =  D98SPCDEE
376700150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
376800150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
376900150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
377000150312     c                   eval      statuso74='RIC'
377100150403     c* se presente FIARP --> visualizzo la data richiesta
377200150403     c     karp          chain     fiarp01l
377300150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
377400150611     c                   movel     arpdcr        wdee
377500150611     c                   movel     arpdcr        wdat
377600150403     c                   exsr      edtdat
377700150403     c                   eval      DTTEOCOO74  =  wdate
377800150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
377900150421    3C                   IF        ARPHCR>0
378000150403     c                   eval      orarich=arphcr
378100150421    3c                   endif
378200150421   x2c                   else
378300150403     c*  se presente DOPO IL imposto la data in D98dee
378400150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
378500150403     c                   eval      D98dee=arpdcr
378600150611     c                   eval      wdee=arpdcr
378700150421    3c                   endif
378800150403     c* PRIMA DEL come se non ci fosse...
378900150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
379000150421    3c                   if        d98dee>0
379100150312     c                   Clear                   xgiolavds
379200150312     c                   Eval      Ixglpa  = 'A'
379300150312     c                   Eval      IxglFil = arblna
379400150312     c                   Eval      Ixgldata = d98dee
379500150312     c                   Eval      Ixgladd = 'S'
379600150312     c                   Eval      Ixgllav = 'S'
379700150312     c                   eval      ixglgg  = 1
379800150312     c                   Call      'XGIOLAV'
379900150312     c                   parm                    xgiolavds
380000150611     c                   movel     oxgldata      wdee
380100150611     c                   movel     oxgldata      wdat
380200150312     c                   exsr      edtdat
380300150312     c                   eval      DTTEOCOO74  =  wdate
380400150421    3c                   endif
380500150421    2c                   endif
380600150421    1c                   endif
380700150421     c
380800150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
380900150421     c                   eval      statuso74='ATT'
381000150421     c                   endif
381100150421    1c                   if        PDAgia='S' and statuso74='DDC'
381200150421     c                   eval      statuso74='GIA'
381300150421     c                   endif
381400150401     c
381500150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
381600150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
381700150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
381800150401     c* La data contatto = data distinta + 1
381900150512     c**                 Clear                   xgiolavds
382000150512     c**                 Eval      Ixglpa  = 'A'
382100150512     c**                 Eval      IxglFil = arblna
382200150512     c**                 Eval      Ixgldata = arbddc
382300150512     c**                 Eval      Ixgladd = 'S'
382400150512     c**                 Eval      Ixgllav = 'S'
382500150512     c**                 eval      ixglgg  = 1
382600150512     c**                 Call      'XGIOLAV'
382700150512     c**                 parm                    xgiolavds
382800150512     c**                 movel     oxgldata      wdat
382900150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
383000150512     c**                 endif
383100150317     c
383200150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
383300150326     c*  lo status diventa RIC
383400150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
383500150317     c                   eval      statuso74='RIC'
383600150317     c                   endif
383700150310     c
383800150304     c* decodifica stato da mettere nei campi deco e note
383900150304     c                   clear                   dspc
384000150304     C                   RESET                   tibs02ds
384100150304     C                   EVAL      t02Cod = 'SPC'
384200150312     C                   EVAL      t02Ke1 = statuso74
384300150306     C                   EXSR      getTntbe00f
384400150306     c                   IF        t02Err <>'E'
384500150306     c
384600150305     c                   eval       stat_d1o74=§SPCDEC1
384700150305     c                   eval       stat_d2o74=§SPCDEC2
384800150304     c*
384900150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
385000150305     c                   eval       stat_n1o74=§SPCNOT1
385100150306     c                   eval       stat_n2o74=§SPCNOT2
385200150304     c                   else
385300150305     c                   clear                   stat_d1o74
385400150305     c                   clear                   stat_d2o74
385500150305     c                   clear                   stat_n1o74
385600150306     c                   clear                   stat_n2o74
385700150304     c                   endif
385800160407
385900160407     c                   endif
386000160407
386100150409     c                   else
386200150409     c* stato evento CONSEGNATO
386300150409     c                   eval      statuso74='CON'
386400150415     c                   eval      flgbolcO74  =  'S'
386500150415     c                   eval      flgboldO74  =  'N'
386600150409    1c                   endif
386700150304     c
386800150304     c
386900140919     ** riempimento per campi da arb con data consegna diversa da 0
387000140919     c                   if        arbdcm > 0
387100140919     c                   movel     arbdcm        wdat
387200140919     c                   exsr      edtdat
387300140919     c                   eval      dtconmeo74  = wdate
387400140919     c                   MOVEL     arbdcm        wora
387500140919     c                   EXSR      edtora
387600140919     c                   eval      orconmeO74  =  worae
387700140919     c                   eval      flgbolcO74  =  'S'
387800140919     c                   eval      flgboldO74  =  'N'
387900141015      *firmatario distinta PDA
388000141015     c                   clear                   firmato74
388100141015     c                   if        gen§ar5pdaco <> 'NO'
388200141015     c                   exsr      repfirmatARB
388300141015     c                   else
388400141015     c                   if        arbgma <> *blank
388500141015     C                   EVAL      tblKey = arbgma
388600141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
388700141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
388800141015     C                             :rpyOpCode:rpyEsito)
388900141015     C                   IF        rpyOpCode = 'FOUND'
389000141015     c                   if        §7r2fi = 'S'
389100141015     c                   exsr      repfirmatARB
389200141015     c                   endif
389300141015     c                   endif
389400141015     c                   endif
389500141015     c                   endif
389600140919     c                   else
389700150415     c* solo se non impostato da PdA lo stato CONSEGNA
389800150415     c                   if        statuso74<>'CON'
389900150415     c
390000140919     c                   eval      flgbolcO74  =  'N'
390100140919     c                   if        arbndc > 0 and arbddc > 0
390200150415     c                             and arbddc <= datcor and d98cons='S'
390300140919     c                   eval      flgboldo74 = 'S'
390400140919     c                   else
390500140919     c                   eval      flgboldO74  =  'N'
390600140919     c                   endif
390700150415     c                   endif
390800150415
390900140919     c                   endif
391000150415     c
391100150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
391200140919     c                   eval      orincono74 = orsticdo74
391300140919     c                   eval      orficono74 = ORSticaO74
391400140919     c                   else
391500140919     c                   eval      orincono74 = ORSTINIO74
391600140919     c                   eval      orficono74 = ORSTFINO74
391700140919     c                   endif
391800150326     c* note 1
391900150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
392000150309     c                   clear                   wparm
392100150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
392200150326     c                   if        D98DTCTD<>*blanks
392300150326     c                   movel     D98DTCTD      wdat
392400150326     c                   exsr      edtdat
392500150326     c                   else
392600150326     c                   eval      wdate=dtteocoo74
392700150410     c                   eval      usaprev='S'
392800150326     c                   endif
392900150326     c
393000150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
393100150309     c                   eval      stat_n1o74= rtvMsgLang(
393200150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
393300150309     c                   endif
393400150309     c* note 2
393500150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
393600150309     c                   clear                   wparm
393700150403     c* Se lo stato è PRG passo orario limite uscita aut
393800150403     c                   select
393900150403     c                   when      D98SPCDEE='PRG'
394000150326     c                   IF        d98picklim  >*zeros
394100150309
394200150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
394300150326     c                                   (%subst(d98picklim:3:2))
394400150309     c                   else
394500150309     c                   eval      wparm='09:00'
394600150403     c* reperisco orario di fine picking
394700150403     c* filiale / data
394800150403     C                   RESET                   tibs02ds
394900150403     C                   EVAL      t02Cod = 'OLP'
395000150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
395100150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
395200150403     C                   EXSR      getTntbe00f
395300150403     c* filiale
395400150403     c                   IF        t02Err = 'E'
395500150403     C                   RESET                   tibs02ds
395600150403     C                   EVAL      t02Cod = 'OLP'
395700150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
395800150403     C                   EXSR      getTntbe00f
395900150403     c                   endif
396000150403     c* Generico 000
396100150403     c                   IF        t02Err = 'E'
396200150403     C                   RESET                   tibs02ds
396300150403     C                   EVAL      t02Cod = 'OLP'
396400150403     C                   EVAL      t02Ke1 = '000'
396500150403     C                   EXSR      getTntbe00f
396600150403     c                   endif
396700150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
396800150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
396900150403     c                             %subst(t02uni:3:2)
397000150309     c                   endif
397100150403     c                   endif
397200150403     c                   eval      stat_n2o74= rtvMsgLang(
397300150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
397400150403     c                   other
397500150403     c* altri casi
397600150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
397700150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
397800150611     c                             arbtcr=' ' and arbdcr>0  and
397900150611     c                             arbdcr>=wdee
398000150410     c
398100150403     c                   eval      orarich=arbhcr
398200150410     c                   endif
398300150403     c
398400150403     c                   if        orarich>0
398500150403     c                   exsr      sr_dallealle
398600150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
398700150403     c                   eval      stat_n2o74= rtvMsgLang(
398800150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
398900150403     c
399000150403     c                   else
399100150403     c                   clear                   stat_n2o74
399200150403     c                   endif
399300150403     c
399400150403     c                   endsl
399500150403     c
399600150309     c                   endif
399700150410     c
399800150424     c* Se si tratta di fermo deposito pulisco sempre
399900150424     c                   if        usaprev<>'S' or arbffd='S'
400000150410     c                   clear                   DTTEOCOO74
400100150410     c                   clear                   ORINCONO74
400200150410     c                   clear                   ORFICONO74
400300150410     c                   endif
400400150309     c
400500140604     c                   ENDSR
400600150403      /free
400700150403       //--------------------------------------------------------------
400800150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
400900150403       //--------------------------------------------------------------
401000150403       BEGSR  sr_dallealle;
401100150403       clear  w_dallemsg ;
401200150403       clear  w_allemsg ;
401300150403
401400150403       // Reperimento dati tabella VPO - ORARISER
401500150403       if §VPORST_MW=0  ;
401600150403       clear tibs02ds;
401700150403       clear dvpooraris;
401800150403       t02mod='C';
401900150403       t02cod='VPO';
402000150403       t02ke1='ORARISER';
402100150403       tibs02r(kpjba:tibs02ds);
402200150403       if t02err=*blanks;
402300150403          dvpooraris=t02uni;
402400150403       endif;
402500150403       endif;
402600150403
402700150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
402800150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
402900150403       //   determino orari servizio comprensivi della tolleranza
403000150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
403100150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
403200150403
403300150403            if w_dalle<w_dalleoser;
403400150403               w_dalle=w_dalleoser;
403500150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
403600150403            endif;
403700150403
403800150403            if w_alle>w_alleoser;
403900150403               w_alle=w_alleoser;
404000150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
404100150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
404200150403               endif;
404300150403            endif;
404400150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
404500150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
404600150403
404700150403       endsr;
404800150403      /end-free
404900010717     c*--------------------------------------------------------------------------------------------*
405000010717     c* imposta i campi della DS di Output dagli eventi
405100010717     c*--------------------------------------------------------------------------------------------*
405200010717     c     RepAbilFirma  BEGSR
405300010717     c*
405400010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
405500010717     c* e recupera utente / password
405600010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
405700010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
405800010717     c     keyvtd        SETLL     tivtd01l
405900010717     c     keyvtd        READE     tivtd01l                               98
406000010717do  2c                   DOW       NOT *in98
406100010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
406200010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
406300010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
406400010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
406500010717     c                   EVAL      depute = vtdute                              *utente
406600010717     c                   EVAL      deppwd = vtdpwd                              *password
406700010717     c                   LEAVE                                                  *esce dal ciclo
406800010717e   3c                   ENDIF
406900010717     c     keyvtd        READE     tivtd01l                               98
407000010717e   2c                   ENDDO
407100010717e   1C                   ENDIF
407200010717     c*
407300010717     c                   ENDSR
407400000000     c*--------------------------------------------------------------------------------------------*
407500000000     c* edita data da aaaammgg in gg/mm/aaaa
407600000000     c*--------------------------------------------------------------------------------------------*
407700000000     c     edtdat        BEGSR
407800000000     c*
407900000000     c                   MOVEL     *blanks       wdate
408000000000     c*
408100000000     c                   MOVEL     wdat          dsdat
408200000000     c                   MOVEL     dsgio         dsgioe
408300140528     c                   MOVEL     '.'           dsvd1
408400000000     c                   MOVEL     dsmes         dsmese
408500140528     c                   MOVEL     '.'           dsvd2
408600000000     c                   MOVEL     dsann         dsanne
408700000000     c                   MOVEL     dsdate        wdate
408800000000     c*
408900000000     c                   ENDSR
409000000000     c*--------------------------------------------------------------------------------------------*
409100000000     c* edita ora da hhmm in hh:mm
409200000000     c*--------------------------------------------------------------------------------------------*
409300000000     c     edtora        BEGSR
409400000000     c*
409500000000     c                   MOVEL     *blanks       worae
409600000000     c*
409700000000     c                   MOVEL     wora          dsora
409800000000     c                   MOVEL     dshh          dshhe
409900140617     c                   MOVEL     '.'           dsvo1
410000000000     c                   MOVEL     dsmm          dsmme
410100000000     c                   MOVEL     dsorae        worae
410200000000     c*
410300000000     c                   ENDSR
410400000000     c*--------------------------------------------------------------------------------------------*
410500000000     c* decodifica punto operativo
410600000000     c*--------------------------------------------------------------------------------------------*
410700000000     c     decorg        BEGSR
410800000000     c*
410900000000     c                   MOVEL     *blanks       wodes                          *descrizione
411000000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
411100000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
411200000000     c*
411300000000     c                   SETOFF                                           98
411400041111     c                   Z-ADD     1             O
411500041111     c     worg          LOOKUP    sorg(O)                                98
411600000000if  1c                   IF        *in98                                        *trovata
411700121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
411800121001     c                             %EDITC(wOrg : 'X') + ')'
411900041111     c                   MOVEL     sofl1(O)      wofl1
412000041111     c                   MOVEL     sodpd(O)      wodpd
412100000000e   1c                   ENDIF
412200000000     c*
412300000000     c                   ENDSR
412400000000     c*--------------------------------------------------------------------------------------------*
412500000000     c* decodifica evento per STATI
412600000000     c*--------------------------------------------------------------------------------------------*
412700000000     c     deccevsta     BEGSR
412800000000     c*
412900000000     c                   MOVEL     '0'           werr                           *NO errore
413000050704     c*
413100050704     C                   IF        wcev = 'GEN'
413200050704     C                   EVAL      werr = *ON
413300050704     C                   CLEAR                   wcdex
413400050704     C                   LEAVESR
413500050704     C                   ENDIF
413600000000     c*
413700060622     C                   RESET                   tibs02ds
413800060622     C                   EVAL      t02Cod = 'ICE'
413900060622     C                   EVAL      t02Ke1 = wcev
414000060622     C                   EXSR      getTntbe00f
414100060622     C                   IF        t02Err <> 'E' AND
414200140528     C                             (§icesta = 'S'  OR
414300140528     C                               §icestaP = 'S' )
414400060622     c                   MOVEL     §icedei       wcdex
414500000000x   1c                   ELSE                                                   *non trovato
414600000000     c                   MOVEL     '1'           werr                           *SI errore
414700030130     c                   MOVEL     *blanks       wcdex
414800000000e   1c                   ENDIF
414900000000     c*
415000000000     c                   ENDSR
415100000000     c*--------------------------------------------------------------------------------------------*
415200000000     c* decodifica consegna particolare
415300000000     c*--------------------------------------------------------------------------------------------*
415400000000     c     decftc        BEGSR
415500000000     c*
415600000000     c                   MOVEL     *blanks       wdftc
415700000000     c*
415800000000if  1c                   IF        wftc = 'S'
415900060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
416000000000e   1c                   ENDIF
416100000000if  1c                   IF        wftc = 'P'
416200060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
416300000000e   1c                   ENDIF
416400000000if  1c                   IF        wftc = 'A'
416500060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
416600000000e   1c                   ENDIF
416700000000if  1c                   IF        wftc = 'F'
416800060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
416900000000e   1c                   ENDIF
417000000000     c*
417100000000     c                   ENDSR
417200000000     c*--------------------------------------------------------------------------------------------*
417300000000     c* decodifica turno di chiusura
417400000000     c*--------------------------------------------------------------------------------------------*
417500000000     c     decgcx        BEGSR
417600000000     c*
417700000000     c                   MOVEL     *blanks       wdgcx
417800000000     c* giorno
417900000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
418000000000if  1c                   IF        a1 = '1'
418100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
418200000000e   1c                   ENDIF
418300000000if  1c                   IF        a1 = '2'
418400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
418500000000e   1c                   ENDIF
418600000000if  1c                   IF        a1 = '3'
418700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
418800000000e   1c                   ENDIF
418900000000if  1c                   IF        a1 = '4'
419000060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
419100000000e   1c                   ENDIF
419200000000if  1c                   IF        a1 = '5'
419300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
419400000000e   1c                   ENDIF
419500000000if  1c                   IF        a1 = '6'
419600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
419700000000e   1c                   ENDIF
419800000000if  1c                   IF        a1 = '7'
419900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
420000000000e   1c                   ENDIF
420100000000if  1c                   IF        a1 = ' '
420200000000     c                   EVAL      wdgcx = *blanks
420300000000e   1c                   ENDIF
420400000000     c* am/pm
420500000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
420600000000if  1c                   IF        a1 = ' '
420700060612     c                   EVAL      wdgcx =
420800060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
420900000000e   1c                   ENDIF
421000000000if  1c                   IF        a1 = 'M'
421100060612     c                   EVAL      wdgcx =
421200060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
421300000000e   1c                   ENDIF
421400000000if  1c                   IF        a1 = 'P'
421500060612     c                   EVAL      wdgcx =
421600060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
421700000000e   1c                   ENDIF
421800000000     c*
421900000000     c                   ENDSR
422000000000     c*--------------------------------------------------------------------------------------------*
422100000000     c* decodifica evento per GIACENZE
422200000000     c*--------------------------------------------------------------------------------------------*
422300000000     c     deccevgia     BEGSR
422400000000     c*
422500000000     c                   MOVEL     '0'           werr                           *NO errore
422600000000     c*
422700060622     C                   RESET                   tibs02ds
422800060622     C                   EVAL      t02Cod = 'ICE'
422900060622     C                   EVAL      t02Ke1 = wcev
423000060622     C                   EXSR      getTntbe00f
423100060622     C                   IF        t02Err <> 'E' AND                            *trovata
423200140528     C                             (§icegia = 'S'  OR
423300140528     C                               §icegiap = 'S')
423400060622     c                   MOVEL     §icedei       wcdex
423500000000x   1c                   ELSE                                                   *non trovato
423600000000     c                   MOVEL     '1'           werr                           *SI errore
423700030130     c                   MOVEL     *blanks       wcdex
423800000000e   1c                   ENDIF
423900000000     c*
424000000000     c                   ENDSR
424100000000     c*--------------------------------------------------------------------------------------------*
424200000000     c* Controlla la DS dsi Input
424300000000     c*--------------------------------------------------------------------------------------------*
424400000000     c     chkdsinput    BEGSR
424500000000     c*
424600000000     c                   MOVEL     '0'           werr                           *NO errore
424700000000     c*
424800000000     c* dalla DS di Input scompone il codice spedizione
424900000000     c                   MOVEL     nspedizi74    dsspedizi74
425000000000     c*
425100000000     c* controlla che la lina di partenza sia TUTTA numerica
425200000000     c                   SETOFF                                       99
425300000000     c                   TESTN                   dslnpi74             99
425400000000if  1c                   IF        NOT *in99                                    *non è un numero
425500000000     c                   MOVEL     '1'           werr                           *SI errore
425600140617     c                   eval      widmsg = 'TIS0769'
425700140617     c                   eval      widcampo = '*LNP'
425800140617     c                   clear                   wparm
425900140617     c                   exsr      messaggi
426000000000x   1c                   ELSE                                                   *è un numero
426100000000     c                   MOVE      dslnpi74      a1
426200000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
426300000000     c                   MOVEL     '1'           werr                           *SI errore
426400140617     c                   eval      widmsg = 'TIS0769'
426500140617     c                   eval      widcampo = '*LNP'
426600140617     c                   clear                   wparm
426700140617     c                   exsr      messaggi
426800000000e   2c                   ENDIF
426900000000e   1c                   ENDIF
427000000000     c*
427100000000     c* controlla che la serie spedizione sia TUTTA numerica
427200000000     c                   SETOFF                                       99
427300000000     c                   TESTN                   dsnrsi74             99
427400000000if  1c                   IF        NOT *in99                                    *non è un numero
427500000000     c                   MOVEL     '1'           werr                           *SI errore
427600140617     c                   eval      widmsg = 'TIS0769'
427700140617     c                   eval      widcampo = '*NRS'
427800140617     c                   clear                   wparm
427900140617     c                   exsr      messaggi
428000000000x   1c                   ELSE                                                   *è un numero
428100000000     c                   MOVE      dsnrsi74      a1
428200000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
428300000000     c                   MOVEL     '1'           werr                           *SI errore
428400140617     c                   eval      widmsg = 'TIS0769'
428500140617     c                   eval      widcampo = '*NRS'
428600140617     c                   clear                   wparm
428700140617     c                   exsr      messaggi
428800000000e   2c                   ENDIF
428900000000e   1c                   ENDIF
429000000000     c*
429100000000     c* controlla che il numero spedizione sia TUTTO numerico
429200000000     c                   SETOFF                                       99
429300000000     c                   TESTN                   dsnspi74             99
429400000000if  1c                   IF        NOT *in99                                    *non è un numero
429500000000     c                   MOVEL     '1'           werr                           *SI errore
429600140617     c                   eval      widmsg = 'TIS0769'
429700140617     c                   eval      widcampo = '*NSP'
429800140617     c                   clear                   wparm
429900140617     c                   exsr      messaggi
430000000000x   1c                   ELSE                                                   *è un numero
430100000000     c                   MOVE      dsnspi74      a1
430200000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
430300000000     c                   MOVEL     '1'           werr                           *SI errore
430400140617     c                   eval      widmsg = 'TIS0769'
430500140617     c                   eval      widcampo = '*NSP'
430600140617     c                   clear                   wparm
430700140617     c                   exsr      messaggi
430800000000e   2c                   ENDIF
430900000000e   1c                   ENDIF
431000110104     c*
431100110104     c* controlla che il codice cliente sia TUTTO numerico
431200110104     c                   IF        kscI74 <> *BLANK
431300110104     c                   SETOFF                                       99
431400110104     c                   TESTN                   kscI74               99
431500110104     c                   IF        NOT *in99                                    *non è un numero
431600110104     c                   MOVEL     '1'           werr                           *SI errore
431700140617     c                   eval      widmsg = 'TIS0769'
431800140617     c                   eval      widcampo = 'KSCI74'
431900140617     c                   clear                   wparm
432000140617     c                   exsr      messaggi
432100110104     c                   ELSE                                                   *è un numero
432200110104     c                   MOVE      kscI74        a1
432300110104     c                   IF        a1 < '0'                                     *trovata una lettera
432400110104     c                   MOVEL     '1'           werr                           *SI errore
432500140617     c                   eval      widmsg = 'TIS0769'
432600140617     c                   eval      widcampo = 'KSCI74'
432700140617     c                   clear                   wparm
432800140617     c                   exsr      messaggi
432900110104     c                   ENDIF
433000110104     c                   ENDIF
433100110104     c                   ENDIF
433200110104     c*
433300110104     c* controlla che il codice mittente sia TUTTO numerico
433400110104     c                   IF        ccmI74 <> *BLANK
433500110104     c                   SETOFF                                       99
433600110104     c                   TESTN                   ccmI74               99
433700110104     c                   IF        NOT *in99                                    *non è un numero
433800110104     c                   MOVEL     '1'           werr                           *SI errore
433900140617     c                   eval      widmsg = 'TIS0769'
434000140617     c                   eval      widcampo = 'CCMI74'
434100140617     c                   clear                   wparm
434200140617     c                   exsr      messaggi
434300110104     c                   ELSE                                                   *è un numero
434400110104     c                   MOVE      ccmI74        a1
434500110104     c                   IF        a1 < '0'                                     *trovata una lettera
434600110104     c                   MOVEL     '1'           werr                           *SI errore
434700140617     c                   eval      widmsg = 'TIS0769'
434800140617     c                   eval      widcampo = 'CCMI74'
434900140617     c                   clear                   wparm
435000140617     c                   exsr      messaggi
435100110104     c                   ENDIF
435200110104     c                   ENDIF
435300110104     c                   ENDIF
435400050104     c*
435500050104     c* controlla anno spedizione
435600050104     c                   TESTN                   cAASI74              99
435700050104if  1c                   IF        NOT *in99                                    *non è un numero
435800050104     c                   EVAL      cAASI74 = *ZERO
435900050104x   1c                   ENDIF
436000100222     c*
436100100223     c* Estraggo l'anno dalla data spedizione.
436200100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
436300100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
436400100223     C                   IF        dspI74 <> *BLANK
436500100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
436600100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
436700100223     C                   MONITOR
436800100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
436900100223     C                   ON-ERROR
437000100223     C                   ENDMON
437100100222     C                   ENDIF
437200000000     c*
437300000000     c* se errore, esce dal programma
437400140610if  1c                   IF        werr = '1'                                   *SI errore
437500140610     c                   eval      esito = werr                                 *errore
437600140610e   1c                   ENDIF
437700000000     c*
437800000000     c                   ENDSR
437900000000     c*--------------------------------------------------------------------------------------------*
438000000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
438100000000     c* Input:   Walfa7 = Alfanumerico di 7
438200000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
438300000000     c*--------------------------------------------------------------------------------------------*
438400000000     c     xallinea1     BEGSR
438500000000     c*
438600000000     c* reimposta variabli di lavoro
438700000000     c                   CLEAR                   i
438800000000     c                   CLEAR                   z
438900000000     c                   CLEAR                   walfa7bis
439000000000     c                   EVAL      wzeri = *ALL'0'
439100000000     c*
439200000000     c     ' '           CHECKR    walfa7:7      i
439300000000if  1c                   IF        i <> 0
439400000000     c     i             SUBST     walfa7        walfa7bis
439500000000e   1c                   ENDIF
439600000000if  1c                   IF        walfa7bis <> *blanks
439700000000     c     7             SUB       i             z
439800000000if  2c                   IF        z <> 0
439900000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
440000000000e   2c                   ENDIF
440100000000     c                   EVAL      z = z+ 1
440200000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
440300000000e   1c                   ENDIF
440400000000     c*
440500000000     c                   ENDSR
440600000000     c*--------------------------------------------------------------------------------------------*
440700000000     c* caricamento tabelle occorrenti
440800000000     c*--------------------------------------------------------------------------------------------*
440900000000     c     cartab        BEGSR
441000000000     c*---
441100000000     c* organigrama
441200000000     c*---
441300000000     c                   CLEAR                   i                              *indice
441400000000     C                   CLEAR                   sorg
441500000000     C                   CLEAR                   sodes
441600000000     C                   CLEAR                   sofl1
441700000000     C                   CLEAR                   sodpd
441800060623     C                   OPEN      tntbe01l
441900000000     c     *loval        SETLL     azorg01l
442000000000     c                   READ      azorg01l                               99
442100000000do  1c                   DOW       NOT *in99
442200000000     c                   ADD       1             i
442300000000     c                   Z-ADD     orgfil        sorg(i)
442400000000     c                   MOVEL     orgdes        sodes(i)
442500000000     c                   MOVEL     orgde3        og143
442600020702     C*
442700020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
442800020702     C                   CLEAR                   dntw
442900020702     C*
443000020702     C                   MOVEL(P)  'NTW'         kbecod
443100020702     C                   MOVEL(P)  §ogntw        kbeke1
443200020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
443300020702     c                   MOVEL     *blanks       kbelin                         *lingua
443400020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
443500020702     C     keytbe        CHAIN     tntbe01l
443600020702     C                   IF        %found(tntbe01l)
443700020702     C                   MOVEL     tbeuni        dntw
443800020702     C                   ENDIF
443900020702     C*
444000020702     C                   MOVEL     §ntwfie       sofl1(i)
444100160406     C*
444200020702     c                   MOVEL     §ogntw        sodpd(i)
444300160406     C*
444400000000     c                   READ      azorg01l                               99
444500000000e   1c                   ENDDO
444600060623     C                   CLOSE     tntbe01l
444700000000     c*---
444800000000     c                   ENDSR
444900000000     c*--------------------------------------------------------------------------------------------*
445000000000     c* operazioni iniziali -da fare sempre-
445100000000     c*--------------------------------------------------------------------------------------------*
445200000000     c     rutinz        BEGSR
445300000000     c*
445400000000     c* reperimento data corrente
445500000000     C                   TIME                    n14                            *ora (6)+ data (8)
445600000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
445700000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
445800000000     C                   Z-ADD     n8            g08dat
445900000000     C                   Z-ADD     *zeros        g08inv
446000000000     C                   MOVEL     '0'           g08err
446100000000     c                   CALL      'XSRDA8'
446200000000     c                   PARM                    wlbda8
446300000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
446400000000     C                   MOVEL     datcor        anncor                         *anno corrente
446500000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
446600000000     c*
446700000000     c* calcola data corrente - 1 anno
446800000000     c                   Z-ADD     datcor        datpre                         *data precedente
446900000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
447000000000     c*
447100010716     c* azzera la DS di Output e le variabili di lavoro / controllo
447200000000     c                   MOVEL     '0'           esito
447300140603     c                   reset                   tis778dso                      *ds Output
447400140603     c                   reset                   tis778dsm                      *ds Output
447500000000     c                   CLEAR                   smi                            *di memorizzazione
447600000000     c                   CLEAR                   smseql
447700000000     c                   CLEAR                   smseqe
447800000000     c                   CLEAR                   smdevhev
447900000000     c                   CLEAR                   smdeve
448000000000     c                   CLEAR                   smheve
448100000000     c                   CLEAR                   smfle
448200000000     c                   CLEAR                   smdev
448300000000     c                   CLEAR                   smcev
448400130312     c                   CLEAR                   smspe
448500000000     c                   CLEAR                   smdmc
448600000000     c                   CLEAR                   smdmcR
448700000000     c                   CLEAR                   tote                           *contatore eventi
448800000000     c                   CLEAR                   seql                           *sequenza legami
448900010716     c                   RESET                   $AbTD                          *di lavoro
449000010716     c                   RESET                   $AbFirma
449100150312     c                   clear                   pdaavv
449200150421     c                   clear                   pdagia
449300140603     c* parametri ingresso
449400140603     c                   exsr      Kontrolla
449500000000     c* controlla da DS in Input
449600000000     c                   EXSR      chkdsinput
449700000000     c*
449800000000     c* carica i clienti dettaglio del cliente unificante
449900000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
450000000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
450100000000     c     keyvss        SETLL     tivss02l
450200000000     c     keyvss        READE     tivss02l                               99
450300000000do  1c                   DOW       NOT *in99
450400000000if  2c                   IF        vssvat = *blanks                             *attivo
450500000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
450600000000     c                             datcor <= vssdsc
450700010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
450800010719     c* --> TD per vedere la firma DPD
450900010716     c                   RESET                   $AbTD                          *di lavoro
451000010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
451100010719     c     kssksu        SETLL     tivss02l
451200010719     c     kssksu        READE     tivss02l                               99
451300010719do  4c                   DOW       NOT *in99
451400010716if  5c                   IF        vssvat = *blanks                             *attivo
451500010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
451600010716     c                             datcor <= vssdsc
451700010719if  7c                   IF        vssisv = 'TD'
451800010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
451900010719e   7c                   ENDIF
452000010719e   6c                   ENDIF
452100010716e   5c                   ENDIF
452200010719     c     kssksu        READE     tivss02l                               99
452300010719e   4c                   ENDDO
452400010716     c*
452500000000     c                   LEAVE                                                  *uscita ciclo
452600000000e   3c                   ENDIF
452700000000e   2c                   ENDIF
452800000000     c     keyvss        READE     tivss02l                               99
452900010716e   1c                   ENDDO                                                  *fine cl.abil.TT
453000000000     c*
453100060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
453200060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
453300151216     c* Se lo strategi user number è = *blanks --> lo imposto a *zeros
453400151216     c                   monitor
453500151216     c                   if        RQSCIDI174>0
453600151216     c                   endif
453700151216     c
453800151216     c                   on-error
453900151216     c                   clear                   RQSCIDI174
454000151216     c                   endmon
454100060621     c*
454200000000     c                   ENDSR
454300040330
454400040330     ***********************************************************************************************
454500040330     **?
454600040330     **?Reperimento ulteriori dati destinatario.
454700040330     **?
454800040330     ***********************************************************************************************
454900040330     C     ImpDSDest     BEGSR
455000040330
455100140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
455200140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
455300100121     C                             ar5Trd <> 'GEN'
455400140716     c                   Z-ADD     f_tasaas      kr5aas
455500140716     c                   Z-ADD     f_taslnp      kr5lnp
455600140716     c                   Z-ADD     f_tasnrs      kr5nrs
455700140716     c                   Z-ADD     f_tasnsp      kr5nsp
455800040330     c                   MOVEL     'GEN'         kr5trd
455900040531     c     keyar5        CHAIN     fiar531c
456000040330     C                   IF        %FOUND
456100040330     C                   EVAL      DAR5GEN = AR5Uni
456200100121     C                   ELSE
456300100121     C                   RESET                   DAR5GEN
456400040330     C                   ENDIF
456500100121     C                   ENDIF
456600100121     C
456700100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
456800100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
456900040330
457000040330     C                   ENDSR
457100040913
457200040913     ***********************************************************************************************
457300040913     **?
457400040913     **?Determino se addebitare l'immagine LDV.
457500040913     **?
457600040913     ***********************************************************************************************
457700040913     C     AddebitoLDV   BEGSR
457800040913
457900040913     C                   IF        LDVO74 = 'S'
458000040913
458100040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
458200040913
458300040913     C                   ENDIF
458400040913
458500040913     C                   ENDSR
458600051110
458700051110     ***********************************************************************************************
458800051110     **?
458900051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
459000051110     **?
459100051110     ***********************************************************************************************
459200051110     C     chkTivpi      BEGSR
459300051110
459400051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
459500051110     C                             esito10)
459600051110     C                   IF        esito10 > 0 AND NOT %ERROR
459700051110     C                   EVAL      gcfdio74 = 'N'
459800051110     C                   ENDIF
459900051110
460000051110     C                   ENDSR
460100051222
460200051222     ***********************************************************************************************
460300051222     **
460400051222     ** Linea arrivo Italia o estera.
460500051222     **
460600051222     ***********************************************************************************************
460700051222     C     setLnaItaEst  BEGSR
460800051222
460900051222     C                   RESET                   wrkLnaItaEst
461000051222     C                   EXSR      decOrg
461100051222     C                   EVAL      wrkLnaItaEst = wofl1
461200051222
461300051222     C                   ENDSR
461400060622
461500060622     ***********************************************************************************************
461600150615     ** Reperisco informazioni stato bolla
461700060622     ***********************************************************************************************
461800130312     C     srarb         BEGSR
461900130312     c                   clear                   wcev
462000140714     c                   clear                   wricons
462100130312     c                   clear                   worg
462200130312     c                   clear                   wdat
462300130312     c                   clear                   wora
462400150615     c                   clear                   ds2a
462500130312     c                   eval      dsevbspe = spe(1)
462600130312     c     karb          chain     fnarb01l
462700130312     c                   if        not %error and
462800130312     c                             %found(fnarb01l)
462900130422     c                   Z-ADD     arblna        worg
463000130312     c                   exsr      setLnaItaEst
463100130312     c* no per estero
463200130312     C                   IF        wrkLnaItaEst = ESTERO
463300130312     c                   leavesr
463400130312     c                   end
463500150617     c
463600130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
463700130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
463800130312     C                   SELECT
463900141120     c                   WHEN      arbdcm <> 0
464000130312     c                   MOVEL     '704'         wcev                           *causale evento
464100130313     c                   move      arbdcm        WDAT
464200130313     c                   move      arbhmc        WORA
464300150617     c
464400130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
464500150615     c                   WHEN      ARBFDC = ' ' and arbndc>0
464600130326     c* solo se distinta NON in test
464700130326     c                   clear                   ddstflr
464800130326     c                   z-add     arbifp        dstfgs
464900130326     c                   z-add     arbndc        dstnfv
465000130326     c     kdst          chain     fidst01l
465100130326     c                   if        not %error and
465200130326     c                             %found(fidst01l)
465300130326     c                   eval      ddstflr = dstflr
465400130326     c                   end
465500130327     c                   if        §DSTTSTPDA = 'C' or
465600130327     c                             §DSTTSTPDA = 'E'
465700130326     c                   leavesr
465800130326     c                   end
465900130326     c*
466000150617     c* Cerco PRIMO evento da PdA
466100130313     c                   move      arbndc        pctndc
466200131105     c                   exsr      cercaceP
466300150617     c*
466400150617     c* Cerco ULTIMO evento da PdA
466500131105     c                   move      'CET'         ttrd
466600130312     c     kcet          setgt     fipct02l
466700130312     c     kcet          readpe    fipct02l
466800130312     c                   if        not %error and
466900130312     c                             not %eof(fipct02l)
467000130312     c                   movel     pctdati       fipctcetds
467100140926      * firmatario se c'è riempie il campo
467200140926     c                   if        §pctnomfir <> *blank
467300140926     c                   eval      firmato74  = §pctnomfir
467400140926     c                   endif
467500130312     c* consegnata
467600130312     c                   if        §pctcmc = ' '
467700130312     c                   MOVEL     '704'         wcev                           *causale evento
467800130312     c                   else
467900140711     c                   move      'S'           wricons
468000130312     c* mancata consegna
468100130313     c                   movel     §PCTcmc       wcev
468200130312     c                   clear                   ds2a
468300130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
468400130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
468500130312     C                             :rpyEsito)
468600130312     c                   if        §2aflg = '§' and §2acpt = 'S'
468700130312     c                   move      'T  '         wcev
468800130312     c                   endif
468900130312     c                   endif
469000130312     c* ??? £6
469100130312     c                   if        §pctdata > 0
469200130313     c                   move      §PCTDATA      WDAT
469300130312     c                   movel     §PCTORA       WORA
469400130312     c                   endif
469500130312     c                   end
469600130312     c                   endSL
469700130312     c                   end
469800130312     c* ho trovato un evento da inserire  al 1° posto
469900130312     c                   if        wcev <> ' '
470000130312     c                   EXSR      deccevsta
470100130312     c                   IF        werr <> '1'                                  *no errore
470200130312     c                   EXSR      decorg
470300130312     c                   EXSR      edtdat
470400130312     c                   EXSR      edtora
470500130313     c* libero il 1° elemnto di schiera
470600150617     c                   eval      j = 49
470700150617     c                   for       j = 49 downto 1
470800130312     c                   if        cev(j) <> ' '
470900130312     c                   eval      cev(j+1) = cev(j)
471000130312     c                   eval      dev(j+1) = dev(j)
471100130312     c                   eval      hev(j+1) = hev(j)
471200130312     c                   eval      fle(j+1) = fle(j)
471300130312     c                   eval      eve(j+1) = eve(j)
471400130312     c                   end
471500130313     c                   endfor
471600130313     c                   eval      dev(1) = wdate
471700130313     c                   eval      hev(1) = worae
471800130312     c                   eval      cev(1)= wcev
471900130313     c                   eval      fle(1) = wodes
472000130313     c                   eval      eve(1) = wcdex
472100130313     c                   EVAL      cntEveO74 += 1
472200150310     c*
472300150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
472400150310     c                   if        §2aftc='A'
472500150310     c                   eval      pdaavv='S'
472600150310     c                   endif
472700150421     c                   if        §2aftc='G'
472800150421     c                   eval      pdagia='S'
472900150421     c                   endif
473000150310     c
473100130312     c                   end
473200130312     c                   end
473300130312     C                   ENDSR
473400140714     **_________________________________________________________
473500140714     c     cercafiglia   begsr
473600140714     **_________________________________________________________
473700140715     c                   clear                   keyForzata
473800140714      * salva chiave originale per eventi
473900140715     c                   eval      kasaasf = kasaas
474000140715     c                   eval      kaslnpf = kaslnp
474100140715     c                   eval      kasnrsf = kasnrs
474200140715     c                   eval      kasnspf = kasnsp
474300140714     c                   do        *hival
474400140715     c     keytasf       chain     fnlbl02l
474500140714     c                   if        %found(fnlbl02l)
474600140715     c                   eval      kasaasf = LBLaan
474700140715     c                   eval      kaslnpf = LBLlpn
474800140715     c                   eval      kasnrsf = LBLnrn
474900140715     c                   eval      kasnspf = LBLnsn
475000140714     c                   move      'X'           keyForzata
475100140714     c                   iter
475200140714     c                   else
475300140714     c                   leave
475400140714     c                   endif
475500140714     c                   enddo
475600140715     c     keytasf       setll     titas30c                               99    *no errore
475700140715     c     keytasf       READE     titas30c                               99    *no errore
475800141117      *se non trova titas con figlia forza di nuovo la mamma
475900140715if  3c                   IF        *in99                                        *non trovato
476000141117     c                   clear                   titasdssavf
476100140715x   3c                   ELSE                                                   *trovata
476200140715if  4c                   IF        *in01                                        *lettura titas000
476300140715     c                   EVAL      titasdssavf = titasds
476400140715e   4c                   ENDIF
476500140715if  4c                   IF        *in02                                        *lettura titas010
476600140715     c                   EVAL      titasdssavf = titasds
476700140715e   4c                   ENDIF
476800140715if  4c                   IF        *in03                                        *lettura titasp00
476900140715     c                   EVAL      titasdssavf = titasds
477000140715e   4c                   ENDIF
477100140715e   3c                   ENDIF
477200140714     C                   ENDSR
477300160318     **_________________________________________________________
477400160318     c     cercamamma    begsr
477500160318     **_________________________________________________________
477600160318     c                   clear                   keyForzata
477700160318     c                   EVAL      kblaan = arbaas
477800160318     c                   EVAL      kbllpn = arblnp
477900160318     c                   EVAL      kblnrn = arbnrs
478000160318     c                   EVAL      kblnsn = arbnsp
478100160318     c     keylbl        CHAIN     fnlbl01l                           99
478200160318if  1c                   IF        NOT *in99                                    *ha la mamma
478300160318     c                   EVAL      kasaas = lblaap
478400160318     c                   EVAL      kaslnp = lbllpp
478500160318     c                   EVAL      kasnrs = lblnrp
478600160318     c                   EVAL      kasnsp = lblnsp
478700160318     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
478800160318if  3c                   IF        not *in99                                        *non trovato
478900160318     c                   move      'X'           keyForzata
479000160318     c                   eval      esito=' '
479100160408     c                   clear                   titasdssavf
479200160318     c                   endif
479300160318     c                   endif
479400160318     c
479500160318     c                   ENDSR
479600131105     **_________________________________________________________
479700131105     c     cercaCEP      begsr
479800131105     **_________________________________________________________
479900131105     c                   move      'CEP'         ttrd
480000131105     c     kcet          setgt     fipct02l
480100131105     c     kcet          readpe    fipct02l
480200131105     c                   if        not %error and
480300131105     c                             not %eof(fipct02l)
480400131105     c                   movel     pctdati       fipctcetds
480500131105     c* consegnata
480600131105     c                   if        §pctcmc = ' '
480700131105     c                   MOVEL     '704'         wcev                           *causale evento
480800131105     c                   else
480900131105     c* mancata consegna
481000131105     c                   movel     §PCTcmc       wcev
481100131105     c                   clear                   ds2a
481200131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
481300131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
481400131105     C                             :rpyEsito)
481500131105     c                   if        §2aflg = '§' and §2acpt = 'S'
481600131105     c                   move      'T  '         wcev
481700131105     c                   endif
481800131105     c                   endif
481900131105     c* ??? £6
482000131105     c                   if        §pctdata > 0
482100131105     c                   move      §PCTDATA      WDAT
482200131105     c                   movel     §PCTORA       WORA
482300131105     c                   endif
482400131105     c                   end
482500131105     c* ho trovato un evento da inserire  al 1° posto
482600131105     c                   if        wcev <> ' '
482700131105     c                   EXSR      deccevsta
482800131105     c                   IF        werr <> '1'                                  *no errore
482900131105     c                   EXSR      decorg
483000131105     c                   EXSR      edtdat
483100131105     c                   EXSR      edtora
483200131105     c* libero il 1° elemnto di schiera
483300150617     c                   eval      j = 49
483400150617     c                   for       j = 49 downto 1
483500131105     c                   if        cev(j) <> ' '
483600131105     c                   eval      cev(j+1) = cev(j)
483700131105     c                   eval      dev(j+1) = dev(j)
483800131105     c                   eval      hev(j+1) = hev(j)
483900131105     c                   eval      fle(j+1) = fle(j)
484000131105     c                   eval      eve(j+1) = eve(j)
484100131105     c                   end
484200131105     c                   endfor
484300131105     c                   eval      dev(1) = wdate
484400131105     c                   eval      hev(1) = worae
484500131105     c                   eval      cev(1)= wcev
484600131105     c                   eval      fle(1) = wodes
484700131105     c                   eval      eve(1) = wcdex
484800131105     c                   EVAL      cntEveO74 += 1
484900131105     c                   end
485000131105     c                   end
485100131105     c                   endsr
485200131105     ***********************************************************************************************
485300130312     **
485400130312     ** Reperisco tabella da TNTBE00F.
485500130312     **
485600130312     ***********************************************************************************************
485700130312     C     getTntbe00f   BEGSR
485800060622     C                   EVAL      t02Lin = langTntbe
485900060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
486000060622     C                   SELECT
486100060622     C                   WHEN      t02Cod = 'ICE'
486200060622     C                   EVAL      dice = t02Uni
486300060622     C                   WHEN      t02Cod = 'CCH'
486400060622     C                   EVAL      dcch = t02Uni
486500060622     C                   WHEN      t02Cod = 'NTW'
486600060622     C                   EVAL      dntw = t02Uni
486700100119     C                   WHEN      t02Cod = 'I1A'
486800100119     C                   EVAL      dI1a = t02Uni
486900150304     C                   WHEN      t02Cod = 'SPC'
487000150304     C                   EVAL      dSPC = t02Uni
487100060622     C                   ENDSL
487200060622     C                   ENDSR
487300060608
487400140529      /free
487500140529       //--------------------------------------------------------------
487600140529       //?Controllo formale dei dati passati.
487700140529       //--------------------------------------------------------------
487800140603       BEGSR  Kontrolla;
487900140529
488000140529       //?OpCode
488100140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
488200160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
488300140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
488400160818           esito='1'    ;
488500140529           exsr RoutEnd;
488600140529         ENDIF;
488700140603           IF  prmRqsDataSize > 0;
488800140603           ELSE;
488900160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
489000140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
489100160818             esito='1'    ;
489200140603             exsr RoutEnd;
489300140603           ENDIF;
489400140529
489500140529       //?Formato e size RQS
489600140603           IF  prmRqsFormato = formatoi74;
489700140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
489800140529           ELSE;
489900160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
490000140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
490100160818             esito='1'    ;
490200140529             exsr RoutEnd;
490300140529           ENDIF;
490400140529         //?Formato e size RPY
490500140603           IF  prmRpyFormato = formatoo74;
490600140529           ELSE;
490700160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
490800140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
490900160818             esito='1'    ;
491000140529             exsr RoutEnd;
491100140529           ENDIF;
491200140529           IF  prmRpyDataSize > 0;
491300140529           ELSE;
491400160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
491500140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
491600160818             esito='1'    ;
491700140529             exsr RoutEnd;
491800140529           ENDIF;
491900140616       //?Formato e size MSG
492000140617         if %parms > 9 ;
492100140617          IF  prmRpyFormMsg = formato;
492200140617          ELSE;
492300160818           // prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
492400140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
492500160818           esito='1'    ;
492600140616           exsr RoutEnd;
492700140617          ENDIF;
492800140617          IF  prmRpyMsgSize > 0;
492900140617          ELSE;
493000160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
493100140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
493200160818           esito='1'    ;
493300140616           exsr RoutEnd;
493400140617          ENDIF;
493500140617         ENDIF;
493600140529
493700140529       ENDSR;
493800140529
493900140616       //--------------------------------------------------------------
494000140617       //?Routine per schierare i messaggi nella ds tis778dsm
494100140616       //--------------------------------------------------------------
494200140616       BEGSR  Messaggi;
494300140616
494400140616       //?Se ho già caricato 99 messaggi esco
494500140616         IF  nrmsg = 99;
494600160818           esito='1'  ;
494700140616           exsr RoutEnd;
494800140616           clear nrmsg;
494900140616         ENDIF;
495000140616
495100140616         nrmsg += 1;
495200140616         skIdMsg(nrmsg) = wIdMsg;
495300140616         skIdCampo(nrmsg) = wIdCampo;
495400140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
495500140616         IF  wParm <> *blanks;
495600140616           skTextMsg (nrmsg) =
495700140616           rtvMsgLang(wIdMsg:langi74:wParm);
495800140616         ELSE;
495900140616           skTextMsg (nrmsg)=
496000140616           rtvMsgLang(wIdMsg:langi74);
496100140616         ENDIF;
496200140616
496300140616       ENDSR;
496400140616
496500140529       //--------------------------------------------------------------
496600140529       BEGSR  RoutEnd;
496700140530
496800140530
496900160322        if  esito <> '0' and esito<>'P';
497000140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
497100140530             else;
497200140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
497300140530        endif ;
497400140529
497500140530       //?ritorno
497600140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
497700140617         if %parms > 9 ;
497800140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
497900140617         endif;
498000140529         return;
498100140529
498200140529       ENDSR;
498300140529      /end-free
498400000000     c*--------------------------------------------------------------------------------------------*
498500000000     c* operazioni iniziali
498600000000     c*--------------------------------------------------------------------------------------------*
498700000000     c     *inzsr        BEGSR
498800000000     c*
498900000000     c* ricevimento parametri
499000140603     c*m   *ENTRY        PLIST
499100140529     c*m                 PARM                    esito
499200140529     c*m                 PARM                    tis174dsi
499300140529     c*m                 PARM                    tis174dso
499400000000     c*
499500000000     c* chiavi di lettura
499600000000     c     keytas        KLIST                                                  titas30c
499700000000     c                   KFLD                    kasaas                         -anno spedizione
499800000000     c                   KFLD                    kaslnp                         -linea partenza
499900000000     c                   KFLD                    kasnrs                         -serie
500000000000     c                   KFLD                    kasnsp                         -spedizione
500100140715     c     keytasf       KLIST                                                  titas30c
500200140715     c                   KFLD                    kasaasf                        -anno spedizione
500300140715     c                   KFLD                    kaslnpf                        -linea partenza
500400140715     c                   KFLD                    kasnrsf                        -serie
500500140715     c                   KFLD                    kasnspf                        -spedizione
500600130312     c     karb          KLIST                                                  titas30c
500700140716     c                   KFLD                    f_tasaas                         -anno spedizione
500800140716     c                   KFLD                    f_taslnp                         -linea partenza
500900140716     c                   KFLD                    f_tasnrs                         -serie
501000140716     c                   KFLD                    f_tasnsp                         -spedizione
501100130326     C     Kdst          KLIST
501200130326     C                   KFLD                    dstnpg
501300130326     C                   KFLD                    dstnfv
501400130326     C                   KFLD                    dstfgs
501500130326     c                   z-add     4             dstnpg
501600130326     C     Kcet          KLIST
501700130326     C                   KFLD                    arbLNA
501800130326     C                   KFLD                    pctNDC
501900130326     C                   KFLD                    arbpdc
502000131105     C                   KFLD                    TTRD              3
502100130312     C                   KFLD                    arbAAS
502200130312     C                   KFLD                    arbLNP
502300130312     C                   KFLD                    arbNRS
502400130312     C                   KFLD                    arbNSP
502500150403     C     KARP          KLIST
502600150403     C                   KFLD                    arbAAS
502700150403     C                   KFLD                    arbLNP
502800150403     C                   KFLD                    arbNRS
502900150403     C                   KFLD                    arbNSP
503000000000     c     keytaa        KLIST                                                  titaa30c
503100000000     c                   KFLD                    kaaaas                         -anno spedizione
503200000000     c                   KFLD                    kaalnp                         -linea partenza
503300000000     c                   KFLD                    kaanrs                         -serie
503400000000     c                   KFLD                    kaansp                         -spedizione
503500000000     c                   KFLD                    kaatrc                         -tipo anagrafica
503600000000     c     keyta4        KLIST                                                  tita430c
503700000000     c                   KFLD                    ka4aas                         -anno spedizione
503800000000     c                   KFLD                    ka4lnp                         -linea partenza
503900000000     c                   KFLD                    ka4nrs                         -serie
504000000000     c                   KFLD                    ka4nsp                         -spedizione
504100000000     c                   KFLD                    ka4trc                         -tipo riferimento
504200010925     c     ke2ta4        KLIST                                                  tita430c
504300010925     c                   KFLD                    ka4aas                         -anno spedizione
504400010925     c                   KFLD                    ka4lnp                         -linea partenza
504500010925     c                   KFLD                    ka4nrs                         -serie
504600010925     c                   KFLD                    ka4nsp                         -spedizione
504700000000     c     keyevb        KLIST                                                  fnevb01l
504800000000     c                   KFLD                    kvbaas                         -anno spedizione
504900000000     c                   KFLD                    kvblnp                         -linea partenza
505000000000     c                   KFLD                    kvbnrs                         -serie
505100000000     c                   KFLD                    kvbnsp                         -spedizione
505200010717     c     ke2evb        KLIST                                                  fnevb01l
505300010717     c                   KFLD                    kvbaas                         -anno spedizione
505400010717     c                   KFLD                    kvblnp                         -linea partenza
505500010717     c                   KFLD                    kvbnrs                         -serie
505600010717     c                   KFLD                    kvbnsp                         -spedizione
505700010717     c                   KFLD                    kvbdev                         -data evento
505800010717     c                   KFLD                    kvbhev                         -ora  evento
505900000000     c     keylbl        KLIST                                                  fnlbl01l
506000000000     c                   KFLD                    kblaan                         -anno seguente
506100000000     c                   KFLD                    kbllpn                         -linea seguente
506200000000     c                   KFLD                    kblnrn                         -serie seguente
506300000000     c                   KFLD                    kblnsn                         -spedizione seguente
506400000000     c     ke2lbl        KLIST                                                  fnlbl02l
506500000000     c                   KFLD                    kblaap                         -anno precedente
506600000000     c                   KFLD                    kbllpp                         -linea precedente
506700000000     c                   KFLD                    kblnrp                         -serie precedente
506800000000     c                   KFLD                    kblnsp                         -spediz.precedente
506900000000     c     keytbe        KLIST                                                  *tntbe01l
507000000000     c                   KFLD                    kbecod                          -tabella
507100000000     c                   KFLD                    kbeke1                          -chiave uno
507200000000     c                   KFLD                    kbeke2                          -chiave due
507300000000     c                   KFLD                    kbelin                          -lingua
507400000000     c                   KFLD                    kbesif                          -s.informativo
507500000000     c     keyvss        KLIST                                                  *tivss02l
507600000000     c                   KFLD                    kssksu                          -cl.unificante
507700000000     c                   KFLD                    kssisv                          -tipo servizio
507800010717     c     keyvtd        KLIST                                                  *tivtd01l
507900010717     c                   KFLD                    ktdksu                          -cl.unificante
508000000000     c     keycsb        KLIST                                                  *tncsb03l
508100000000     c                   KFLD                    ksbaas                         -anno spedizione
508200000000     c                   KFLD                    ksblnp                         -linea partenza
508300000000     c                   KFLD                    ksbnrs                         -serie
508400000000     c                   KFLD                    ksbnsp                         -spedizione
508500040906     c     keydct        KLIST
508600040906     c                   KFLD                    DCTAAC
508700040906     c                   KFLD                    DCTFIL
508800040906     c                   KFLD                    DCTNCA
508900000000     c     keygcp        KLIST                                                  *figcp01l
509000000000     c                   KFLD                    kcpaas                         -anno spedizione
509100000000     c                   KFLD                    kcplnp                         -linea partenza
509200000000     c                   KFLD                    kcpnrs                         -serie
509300000000     c                   KFLD                    kcpnsp                         -spedizione
509400000000     c                   KFLD                    kcpfrg                         -prg aperture
509500050404     c     K04GCP51      KLIST
509600050404     c                   KFLD                    kcpaas                         -anno spedizione
509700050404     c                   KFLD                    kcplnp                         -linea partenza
509800050404     c                   KFLD                    kcpnrs                         -serie
509900050404     c                   KFLD                    kcpnsp                         -spedizione
510000000000     c     keygnp        KLIST                                                  *fignp01l
510100000000     c                   KFLD                    knpagc                         -anno apertura
510200000000     c                   KFLD                    knpfgc                         -filiale apertura
510300000000     c                   KFLD                    knpngc                         -numero giacenza
510400000000     c                   KFLD                    knpfrg                         -prg apertura
510500040531     c     keyar5        KLIST                                                  *fiar531c
510600030206     c                   KFLD                    kr5aas                         -anno spedizione
510700030206     c                   KFLD                    kr5lnp                         -linea partenza
510800030206     c                   KFLD                    kr5nrs                         -serie
510900030206     c                   KFLD                    kr5nsp                         -spedizione
511000030206     c                   KFLD                    kr5trd                         -tipo record
511100040907
511200000000     c* apre i files
511300000000     c                   OPEN      titas30c
511400000000     c                   OPEN      titaa30c
511500000000     c                   OPEN      tita430c
511600000000     c                   OPEN      fnevb01l
511700000000     c                   OPEN      tivss02l
511800010716     c                   OPEN      tivtd01l
511900000000     c                   OPEN      azorg01l
512000000000     c                   OPEN      fnlbl01l
512100000000     c                   OPEN      fnlbl02l
512200050309     c                   OPEN      tigcp51l
512300050309     c                   OPEN      tignp01l
512400000000     c                   OPEN      tncsb03l
512500040906     c                   OPEN      fndct01l
512600040531     c                   OPEN      fiar531c
512700130312     c                   OPEN      fnarb01l
512800160127     c                   OPEN      fnblp01l
512900160309     c                   OPEN      fiar901l
513000150403     c                   OPEN      fiarp01l
513100140606     c                   OPEN      fiar501l
513200140606     c                   OPEN      fiar401l
513300130326     c                   OPEN      fidst01l
513400130312     c                   OPEN      fipct02l
513500060621     C                   CALLP     openTabel00f()
513600000000     c*
513700000000     c* caricamento tabelle occorrenti
513800000000     c                   EXSR      cartab
513900060621     C                   CALLP     inzLingue()
514000000000     c*
514100000000     c                   ENDSR
514200070109
514300070109     ***********************************************************************************************
514400070109     **
514500070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
514600070109     **
514700070109     ***********************************************************************************************
514800070109     C     eventoAnnulla BEGSR
514900070109
515000070109     C                   IF        §iceEvAn <> *BLANK
515100070109     C                   FOR       j = 1 TO %ELEM(smCev)
515200070109     C                   IF        §iceEvAn = smCev(j) AND
515300070109     C                             evbDevHev = smDevHev(j)
515400070109     C                   CLEAR                   smCev(j)
515500130312     C                   CLEAR                   smspe(j)
515600070109     C                   CLEAR                   smDevHev(j)
515700070109     C                   CLEAR                   smi(j)
515800070109     C                   CLEAR                   smSeql(j)
515900070109     C                   CLEAR                   smSeqe(j)
516000070109     C                   CLEAR                   smDeve(j)
516100070109     C                   CLEAR                   smHeve(j)
516200070109     C                   CLEAR                   smFle(j)
516300070109     C                   CLEAR                   smDev(j)
516400070109     C                   LEAVE
516500070109     C                   ENDIF
516600070109     C                   ENDFOR
516700070109     C                   ENDIF
516800070109
516900070109     C                   ENDSR
517000080131
517100100119     P*--------------------------------------------------
517200100119     P* Procedure name: GetDescrizioneIncassoContrassegno
517300100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
517400100119     P*                          o del contrassegno.
517500100121     P* Returns:        Descrizione modalità incasso contrassegno
517600100119     P*--------------------------------------------------
517700100119     P GetDescrizioneIncassoContrassegno...
517800100119     P                 B
517900100119     D GetDescrizioneIncassoContrassegno...
518000100119     D                 PI            35A
518100100119
518200100121     C                   RESET                   tibs02ds
518300100121     C                   EVAL      t02Cod = 'I1A'
518400100121     C                   EVAL      t02Lin = langTntbe
518500100119     ** Il contrassegno è stato incassato
518600100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
518700100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
518800100121     C                   ELSE
518900100119     ** Il contrassegno non è stato incassato.
519000100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
519100100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
519200100119     C                             ar5Trd <> 'GEN'
519300100121     C                   EVAL      kr5Aas = csbAas
519400100121     C                   EVAL      kr5Lnp = csbLnp
519500100121     C                   EVAL      kr5Nrs = csbNrs
519600100121     C                   EVAL      kr5Nsp = csbNsp
519700100119     C                   EVAL      kr5Trd = 'GEN'
519800160314     c                   if        esito='P'
519900160314     C     keyAr5        CHAIN     fiar501l
520000160314     c                   else
520100160314     C     keyAr5        CHAIN     fiar531c
520200160314     c                   endif
520300160314     c
520400100119     C                   IF        %FOUND()
520500100119     C                   EVAL      dAr5Gen = ar5Uni
520600100119     C                   ELSE
520700100119     C                   RESET                   dAr5Gen
520800100119     C                   RETURN                  *BLANK
520900100119     C                   ENDIF
521000100119     C                   ENDIF
521100100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
521200100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
521300100119     C                   ELSE
521400100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
521500100119     C                   ENDIF
521600100121     C                   ENDIF
521700100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
521800100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
521900100119     C                   IF        t02Err = 'E'
522000100119     C                   RETURN                  *BLANK
522100100119     C                   ENDIF
522200100120     C                   EVAL      dI1a = t02Uni
522300140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
522400140417     C                   RETURN                  dI1a.descrizi2
522500140417     C                   ENDIF
522600100119     C                   RETURN                  dI1a.descrizion
522700100119
522800100119     P GetDescrizioneIncassoContrassegno...
522900100119     P                 E
523000100119
523100110304
523200110304     P*--------------------------------------------------
523300110304     P* Procedure name: IsContrassegnoCompensato
523400110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
523500110304     P*                          to con una fattura.
523600110304     P* Returns:        *ON = Compensato
523700110304     P*--------------------------------------------------
523800110304     P IsContrassegnoCompensato...
523900110304     P                 B
524000110304     D IsContrassegnoCompensato...
524100110304     D                 PI              N
524200110304
524300110304     D retField        S               N   INZ(*OFF)
524400110304
524500110307     ** Il contrassegno non è stato rimborsato.
524600110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
524700110307     C                   RETURN    retField
524800110307     C                   ENDIF
524900110307
525000110307     C/EXEC SQL
525100110307     C+ SELECT '1'
525200110307     C+ INTO :retField
525300110307     C+ FROM tncsv00f
525400110307     C+ WHERE csvAas = :csbAas
525500110307     C+   AND csvLnp = :csbLnp
525600110307     C+   AND csvNrs = :csbNrs
525700110307     C+   AND csvNsp = :csbNsp
525800110307     C+   AND csvCav = 'COMP'
525900110307     C+ FETCH FIRST ROW ONLY
526000110307     C/END-EXEC
526100110304
526200110307     C                   SELECT
526300110307     C                   WHEN      sqlCode < 0
526400110307     C                   DUMP(A)
526500110307     C                   ENDSL
526600110304
526700110307     C                   RETURN    retField
526800110304
526900110304     P IsContrassegnoCompensato...
527000110304     P                 E
527100110304
527200120321
527300120321     P*--------------------------------------------------
527400120321     P* Procedure name: SetIdAssegnoMittente
527500120321     P* Purpose:        Imposta l'ID assegno mittente.
527600120321     P* Returns:        Esito.
527700120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
527800120321     P*                          segni mittente.
527900120321     P* Parameter:      priAbi => ABI assegno.
528000120321     P* Parameter:      priCab => CAB assegno.
528100120321     P*--------------------------------------------------
528200120321     P SetIdAssegnoMittente...
528300120321     P                 B
528400120321     D SetIdAssegnoMittente...
528500120321     D                 PI            10I 0
528600120321     D  priNumeroAssegno...
528700120321     D                                     LIKE(caNumO74)
528800120321     D  priAbi                             LIKE(caAbiO74)
528900120321     D  priCab                             LIKE(caCabO74)
529000120321
529100120321     D retField        S             10I 0 STATIC
529200120321
529300120321      /FREE
529400120321
529500120321       CLEAR retField;
529600120321
529700120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
529800120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
529900120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
530000120321       // In questo caso restituisco l'ID del primo assegno.
530100120321
530200120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
530300120321         RETURN retField;
530400120321       ENDIF;
530500120321
530600120321       EXEC SQL
530700120321         SELECT csmNra, csmAbi, csmCab
530800120321           INTO :priNumeroAssegno, :priAbi, :priCab
530900120321           FROM tncsm00f
531000120321           WHERE csmKey = :priNumeroAssegno
531100120321           FETCH FIRST 1 ROW ONLY
531200120321       ;
531300120321
531400120321       SELECT;
531500120321         WHEN sqlCode = 100;
531600120321           retField = sqlCode;
531700120321         WHEN sqlCode < *ZERO;
531800120321           DUMP(A);
531900120321           retField = sqlCode;
532000120321       ENDSL;
532100120321
532200120321       RETURN retField;
532300120321
532400120321      /END-FREE
532500120321     P SetIdAssegnoMittente...
532600120321     P                 E
532700120321
532800130513
532900130513     P*--------------------------------------------------
533000130513     P* Procedure name: GetFilUrl
533100130513     P* Purpose:        Restituisce l'URL della filiale.
533200130513     P* Returns:        URL filiale.
533300130513     P* Parameter:      piFiliale => ID filiale
533400130513     P*--------------------------------------------------
533500130513     P GetFilUrl       B
533600130513     D GetFilUrl       PI           256A
533700130513     D  piFiliale                     3P 0 CONST
533800130513
533900130513     C                   IF        piFiliale = *ZERO
534000130513     C                   RETURN    *BLANK
534100130513     C                   ENDIF
534200130513     C                   RESET                   tibs02ds
534300130513     C                   EVAL      t02Cod = 'UFI'
534400130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
534500130513     C                   EVAL      t02Lin = langTntbe
534600130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
534700130513     C                   IF        t02Err = 'E'
534800130513     C                   RETURN    *BLANK
534900130513     C                   ENDIF
535000130513     C                   RETURN    t02Uni
535100130513
535200130513     P GetFilUrl       E
535300130513
