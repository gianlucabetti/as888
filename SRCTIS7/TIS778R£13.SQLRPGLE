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
013700060626     D chainTabel00f   PR           109A
013800060626     D  rqsOpCode                    10A   CONST
013900060626     D  rqsKut                        1P 0
014000060626     D  rqsCod                        2A   CONST
014100060626     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
014200060626     D  rqsLengthOfKey...
014300060626     D                               10I 0 CONST
014400060626     D  rqsFormat                    10A   CONST
014500060626     D  rpyOpCode                    10A
014600060626     D  rpyEsito                     10I 0
014700060626     D  rpyData                     109A   OPTIONS(*NOPASS:*OMIT)
014800060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
014900060622     D  kpjba                              LIKEDS(kpjba)
015000060622     D  tibs02ds                           LIKEDS(tibs02ds)
015100090828     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
015200090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7702R
015300090828
015400051222     ***********************************************************************************************
015500051222     **?
015600051222     **?Definizione costanti.
015700051222     **?
015800051222     ***********************************************************************************************
015900101104     D ITALIA          C                   ' '
016000101104     D ESTERO          C                   'E'
016100101104     D EVENTO_ARRIVATA_IN_FILIALE...
016200101104     D                 C                   '703'
016300101104     D EVENTO_MESSA_IN_CONSEGNA...
016400101104     D                 C                   'MIC'
016500150612     D EVENTO_ARRIVATA_HUB...
016600150612     D                 C                   '712'
016700000000     d*--------------------------------------------------------------------------------------------*
016800000000     d* Data structure
016900000000     d*--------------------------------------------------------------------------------------------*
017000150403     d dvpooraris    e ds                  inz
017100150403     d fipctcetds    e ds
017200140605     d fipctcords    e ds
017300090914     D cndizion      E DS                  QUALIFIED BASED(nullPtr)
017400090914     D azlin00f      E DS                  BASED(nullPtr)
017500060622     D tabel00f      E DS                  INZ
017600060622     D kpjba         E DS                  INZ
017700140605     D trulorsds     E DS                  INZ
017800140605     D trulor2ds     E DS                  INZ
017900140605     D trulor3ds     E DS                  INZ
018000150819     D Diore01       E DS                  INZ
018100140606     d dar5emd       e ds                  inz prefix(§)
018200140605     D tnsd99ds      E DS                  INZ
018300160323     D fidn40ds      E DS                  INZ
018400160323     D tibs02ds      E DS                  INZ
018500060622     D  t02Mod       E                     INZ('C')                             Controllo
018600090828     D tis7700i0     E DS                  QUALIFIED
018700090828     D                                     INZ(*EXTDFT)
018800090828     D tis7700o0     E DS                  QUALIFIED
018900090828     d*---
019000000000     d* Ridenominazione formato record -TITAS-
019100000000     d*---
019200000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
019300000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
019400000000     d                                     PREFIX(s_)
019500140715     d titasdssavF   E DS                  EXTNAME(titas00f)                    *depisito titas
019600140715     d                                     PREFIX(f_)
019700080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
019800080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
019900080131     D                                     BASED(titas000FigliaPtr)
020000080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
020100080131     D                                     BASED(titas000FigliaPtr)
020200080131     d*---
020300000000     d* Variabili riferite al data base
020400000000     d*---
020500141126     d kasaasm         S                   LIKE(tasaas)                         *lettura titas30c
020600141126     d kaslnpm         S                   LIKE(taslnp)
020700141126     d kasnrsm         S                   LIKE(tasnrs)
020800141126     d kasnspm         S                   LIKE(tasnsp)
020900140715     d kasaasf         S                   LIKE(tasaas)                         *lettura titas30c
021000140715     d kaslnpf         S                   LIKE(taslnp)
021100140715     d kasnrsf         S                   LIKE(tasnrs)
021200140715     d kasnspf         S                   LIKE(tasnsp)
021300000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
021400000000     d kaslnp          S                   LIKE(taslnp)
021500000000     d kasnrs          S                   LIKE(tasnrs)
021600000000     d kasnsp          S                   LIKE(tasnsp)
021700000000     d kastbl          S                   LIKE(tastbl)
021800000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
021900000000     d kaalnp          S                   LIKE(taalnp)
022000000000     d kaanrs          S                   LIKE(taanrs)
022100000000     d kaansp          S                   LIKE(taansp)
022200000000     d kaatrc          S                   LIKE(taatrc)
022300000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
022400000000     d ka4lnp          S                   LIKE(ta4lnp)
022500000000     d ka4nrs          S                   LIKE(ta4nrs)
022600000000     d ka4nsp          S                   LIKE(ta4nsp)
022700000000     d ka4trc          S                   LIKE(ta4trc)
022800000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
022900000000     d kvblnp          S                   LIKE(evblnp)
023000000000     d kvbnrs          S                   LIKE(evbnrs)
023100000000     d kvbnsp          S                   LIKE(evbnsp)
023200000000     d kvbdev          S                   LIKE(evbdev)
023300000000     d kvbhev          S                   LIKE(evbhev)
023400000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
023500000000     d kcplnp          S                   LIKE(gcplnp)
023600000000     d kcpnrs          S                   LIKE(gcpnrs)
023700000000     d kcpnsp          S                   LIKE(gcpnsp)
023800000000     d kcpfrg          S                   LIKE(gcpfrg)
023900000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
024000000000     d knpfgc          S                   LIKE(gnpfgc)
024100000000     d knpngc          S                   LIKE(gnpngc)
024200000000     d knpfrg          S                   LIKE(gnpfrg)
024300000000     d knpfas          S                   LIKE(gnpfas)
024400000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
024500000000     d ksblnp          S                   LIKE(csblnp)
024600000000     d ksbnrs          S                   LIKE(csbnrs)
024700000000     d ksbnsp          S                   LIKE(csbnsp)
024800000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
024900000000     d kbllpn          S                   LIKE(lbllpn)
025000000000     d kblnrn          S                   LIKE(lblnrn)
025100000000     d kblnsn          S                   LIKE(lblnsn)
025200000000     d kblaap          S                   LIKE(lblaap)
025300000000     d kbllpp          S                   LIKE(lbllpp)
025400000000     d kblnrp          S                   LIKE(lblnrp)
025500000000     d kblnsp          S                   LIKE(lblnsp)
025600000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
025700000000     d kctlnp          S                   LIKE(dctlnp)
025800000000     d kctnrs          S                   LIKE(dctnrs)
025900000000     d kctnsp          S                   LIKE(dctnsp)
026000040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
026100030206     d kr5lnp          S                   LIKE(ar5lnp)
026200030206     d kr5nrs          S                   LIKE(ar5nrs)
026300030206     d kr5nsp          S                   LIKE(ar5nsp)
026400030206     d kr5trd          S                   LIKE(ar5trd)
026500000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
026600000000     d kssisv          S                   LIKE(vssisv)
026700010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
026800000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
026900000000     d kbeke1          S                   LIKE(tbeke1)
027000000000     d kbeke2          S                   LIKE(tbeke2)
027100000000     d kbelin          S                   LIKE(tbelin)
027200000000     d kbesif          S                   LIKE(tbesif)
027300000000     d waas            S                   LIKE(tasaas)                         *di lavoro
027400000000     d wlnp            S                   LIKE(taslnp)
027500000000     d wnrs            S                   LIKE(tasnrs)
027600000000     d wnsp            S                   LIKE(tasnsp)
027700000000     d wfca            S                   LIKE(tasfca)
027800000000     d wfgc            S                   LIKE(tasfgc)
027900000000     d wfpr            S                   LIKE(dctfpr)
028000000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
028100000000     d deplpn          S                   LIKE(lbllpn)
028200000000     d depnrn          S                   LIKE(lblnrn)
028300000000     d depnsn          S                   LIKE(lblnsn)
028400010716     d depute          S                   LIKE(vtdute)
028500010716     d deppwd          S                   LIKE(vtdpwd)
028600000000     d lcca            S                   LIKE(tascca)                         *di lavoro
028700000000     d lduc            S                   LIKE(tasduc)
028800000000     d llnp            S                   LIKE(taslnp)
028900000000     d ldti            S                   LIKE(tasdti)
029000000000     d lhti            S                   LIKE(tashti)
029100000000     d ldrt            S                   LIKE(tasdrt)
029200000000     d llna            S                   LIKE(taslna)
029300150617     d ltfa            S                   LIKE(tastfa)
029400000000     d ldcm            S                   LIKE(tasdcm)
029500000000     d lhmc            S                   LIKE(tashmc)
029600060621     d langTabel       S                   LIKE(tblkut)
029700060621     d langTntbe       S                   LIKE(tbelin)
029800140108     d wDev            S                   LIKE(evbdev)
029900140108     d wHev            S                   LIKE(evbhev)
030000150611     d wDEE            S                   LIKE(d98dee)
030100140616     d wParm           s            512a   inz
030200150403     D w_dallemsg      S              5a
030300150403     D w_allemsg       S              5a
030400150403     D w_dalle         S               t
030500150403     D w_alle          S               t
030600150403     D w_dalleoser     S               t
030700150403     D w_alleoser      S               t
030800000000     d*---
030900000000     d* Ds
031000000000     d*---
031100000000      * controllo data
031200000000     d wlbda8          DS                  INZ                                  *controlla data
031300000000     d  g08dat                        8  0                                       -data dritta
031400000000     d  g08inv                        8  0                                       -data invertita
031500000000     d  g08err                        1                                          -errore
031600000000     d  g08tgi                        5  0                                       -giorni fra date
031700000000      * scomposizione data generica
031800000000     d dsdat           DS                  INZ                                  *data
031900000000     d  dsann                         4  0                                       -anno
032000000000     d  dsmes                         2  0                                       -mese
032100000000     d  dsgio                         2  0                                       -giorno
032200000000      * scomposizione data generica (anno + mese/giorno)
032300000000     d                 DS                  INZ                                  *data aa + mm/gg
032400000000     d  dsannB                 1      4  0                                       -anno
032500000000     d  dsmgsB                 5      8  0                                       -mese/giorno
032600000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
032700000000      * composizione data generica editata
032800000000     d dsdate          DS                  INZ                                  *data
032900000000     d  dsgioe                        2  0                                       -giorno
033000000000     d  dsvd1                         1                                          -'/'
033100000000     d  dsmese                        2  0                                       -mese
033200000000     d  dsvd2                         1                                          -'/'
033300000000     d  dsanne                        4  0                                       -anno
033400000000      * scomposizione ora generica
033500000000     d dsora           DS                  INZ                                  *ora
033600000000     d  dshh                          2  0                                       -ore
033700000000     d  dsmm                          2  0                                       -minuti
033800000000      * composizione ora generica editata
033900000000     d dsorae          DS                  INZ                                  *ora
034000000000     d  dshhe                         2  0                                       -ore
034100000000     d  dsvo1                         1                                          -':'
034200000000     d  dsmme                         2  0                                       -minuti
034300030206      * lettura FIAR5 - record bancali
034400030206     d dar5ban       E DS
034500030206      * lettura FIAR5 - record supermercati
034600030206     d dar5scr       E DS
034700040330     d DAR5GEN       E DS
034800040330     D                                     INZ
034900040330     D                                     PREFIX(GEN)
035000000000      * tipi servizio
035100000000     d ds5e          E DS
035200000000      * disposizioni giacenza
035300000000     d ds2d          E DS
035400000000      * causale eventi
035500000000     d dice          E DS
035600000000      * causali chiusura CA
035700000000     d dcch          E DS
035800100119     d dI1a          E DS                  QUALIFIED
035900000000      * consegne anomale
036000000000     d ds7o          E DS
036100130312      * eventi
036200130312     d ds2a          E DS
036300160314     d
036400160314     d* cod bolla
036500160314     d ds3a          E DS
036600000000      * riferimenti
036700000000     d dta4a         E DS
036800070927     d dsbl4e        E DS
036900060627      * Estensione blp/arb tipo record "I"
037000060627     d dsbl4i        E DS
037100040913      * Addebito immagine POD
037200040913     d DLVA1         E DS
037300040913     D                                     INZ
037400000000      * lettura campo orgde3 di AZORG00F
037500000000     d og143         E DS
037600000000      * stati contrassegno
037700000000     d ds4s          E DS
037800141014      * particolarità consegna
037900141014     d ds7r          E DS
038000031217      * DS lettura campo TASFLO di TITAS
038100031217     d dtasflo       E DS
038200000000      * Ds ricerca cliente P.d.C.
038300000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
038400000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
038500000000     d indds         E DS                  EXTNAME(cnind00f) INZ
038600000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
038700000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
038800150304      * stato data prev cons
038900150304     d dspc          E DS
039000170301     d
039100170301     d dcli          E DS
039200150312     d
039300150312     d xgiolavds     e ds
039400000000      * reperimento clienti da cliente unificante
039500090903     d*tibs10ds      E DS
039600090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
039700000000      * scomposizione codice spedizione -DS Input-
039800000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
039900000000     d  dslnpi74                      3                                          -linea partenza
040000000000     d  dsnrsi74                      2                                          -serie
040100000000     d  dsnspi74                      7                                          -spedizione
040200000000      * composizione codice spedizione -bolle-
040300000000     d                 DS
040400000000     d  dstaslnp               1      3  0                                       -linea partenza
040500000000     d  dstasnrs               4      5  0                                       -serie
040600000000     d  dstasnsp               6     12  0                                       -spedizione
040700000000     d dstasspe                1     12                                         *ds cod spedizione
040800000000      * composizione data + ora evento
040900080131     d                 DS                  INZ
041000080131     d  evbdev                 1      8  0                                       -data evento
041100080131     d  evbhev                 9     12  0                                       -ora  evento
041200080131     d evbDevHev               1     12  0                                      *ds data+ora evento
041300130312      * composizione spedizione evento
041400130312     d                 DS                  INZ
041500130312     d  evbaas                 1      4  0                                       -data evento
041600130312     d  evblnp                 5      7  0                                       -ora  evento
041700130312     d  evbnrs                 8      9  0                                       -ora  evento
041800130312     d  evbnsp                10     16  0                                       -ora  evento
041900130312     d dsevbspe                1     16  0                                      *ds data+ora evento
042000060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
042100000000      * ordinamento schiere eventi
042200000000     d                 DS
042300000000     d  dsseql                 1      3  0                                       -sequenza legame
042400000000     d  dsseqe                 4      6  0                                       -sequenza evento
042500000000     d  dsdevhev               7     18  0                                       -data + ora
042600000000     d  dsi                   19     21  0                                       -indice
042700000000     d dssort                  1     21  0                                      *ds cod spedizione
042800000000     d* Input routine XALLINEA
042900000000     d walfa7          S              7                                         *alfanumerico 7
043000000000     d walfa7bis       S              7                                         *alfanumerico 7
043100000000     d wzeri           S              7                                         *alfa con zeri
043200000000     d*---
043300000000     d* Variabili
043400000000     d*---
043500170301     d k_ntcapl        s                   like(ntcapl)
043600170301     d k_ntcnk1        s                   like(ntcnk1)
043700170301     d k_ntcnk2        s                   like(ntcnk2)
043800170301     d k_ntctnt        s                   like(ntctnt)
043900101104     d i               S             10I 0                                      *indici
044000101104     d ii              S             10I 0
044100101104     d j               S             10I 0
044200101104     d jj              S             10I 0
044300101104     d z               S             10I 0
044400101104     d s               S             10I 0
044500101104     d k               S             10I 0
044600101104     d o               S             10I 0
044700101104     d tote            S             10I 0                                      *totale eventi memor
044800000000     d seql            S              5  0                                      *sequenza legami
044900000000     d n14             S             14  0                                      *Numerico 14
045000000000     d n8              S              8  0                                      *Numerico 8
045100000000     d n7              S              7  0                                      *Numerico 7
045200170310     d alfa4           S              4                                         *Numerico 4
045300010927     d n3              S              3  0                                      *Numerico 3
045400000000     d a1              S              1                                         *Alfanumerico 1
045500000000     d a3              S              3                                         *Alfanumerico 3
045600000000     d a5              S              5                                         *Alfanumerico 5
045700010717     d a8              S              8                                         *Alfanumerico 8
045800000000     d a15             S             15                                         *Alfanumerico 15
045900000000     d a200            S            200                                         *Alfanumerico 200
046000000000     d a200bis         S            200                                         *Alfanumerico 200
046100010927     d a230            S            230                                         *Alfanumerico 230
046200000000     d a400            S            400                                         *Alfanumerico 400
046300000000     d datcor          S              8  0                                      *data corrente
046400000000     d datpre          S              8  0                                      *data precedente
046500000000     d oracor          S              6  0                                      *ora corrente
046600000000     d anncor          S              4  0                                      *anno corrente
046700000000     d annpre          S              4  0                                      *anno precedente
046800140617     d wIdMsg          s              7a
046900140617     d wIdCampo        s             10a
047000000000     d werr            S              1                                         *errore generico
047100000000     d wdat            S              8  0                                      *data (aaaammgg)
047200000000     d wdate           S             10                                         *data (gg/mm/aaaa)
047300000000     d wora            S              4  0                                      *ora  (hhmm)
047400000000     d worae           S              5                                         *ora  (hh:mm)
047500000000     d worg            S              3  0                                      *filiale
047600000000     d wodes           S             30                                         *descrizione filiale
047700000000     d wofl1           S              1                                         *flag italia/estero
047800150617     d wtasfl1         S              1                                         *flag italia/estero
047900020702     d wodpd           S              3                                         *flag filiale DPD
048000000000     d wcev            S              3                                         *causale evento
048100030130     d wcdex           S             30                                         *descrizione bar/pt
048200000000     d wpgm            S             20                                         *programma
048300000000     d wftc            S              1                                         *cons.partic.
048400140714     d wricons         S              1                                         *wrk per riconsegna
048500140715     d keyforzata      S              1                                         *wrk per riconsegna
048600000000     d wdftc           S             50                                         *descriz.cons.partic
048700000000     d d1wdftc         S             50                                         *descriz.cons.part.1
048800000000     d d2wdftc         S             50                                         *descriz.cons.part.2
048900000000     d wgcx            S              2                                         *turno chiusura
049000000000     d wdgcx           S             20                                         *descriz.turno chius
049100000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
049200000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
049300000000     d we              S              3                                         *descrizione 'e'
049400000000     d bollaorig       S              1                                         *bolla originaria
049500150310     d PDAavv          S              1                                         *evento AVV a PDA
049600150421     d PDAgia          S              1                                         *evento GIA a PDA
049700000000     d sostrit         S              1                                         sostituisce Ritiro
049800000000     d sostcon         S              1                                         sostituisce Consegna
049900000000     d newevb          S              1                                         *nuovo evento
050000000000     d lavdevhev       S             12  0                                      *data + ora
050100040913     D WrkCurDate      S               D                                        Data corrente
050200060620     D wrkDtInvDisp    S              8
050300060620     D wrkLnaItaEst    S              1
050400060620     D rpyOpCode       S             10A
050500060620     D rpyEsito        S             10I 0
050600060621     D nullPtr         S               *
050700080131     D titas000FigliaPtr...
050800080131     D                 S               *   INZ(%ADDR(titas000Figlia))
050900010716     d*---
051000010716     d* Variabili di controlli
051100010716     d*---
051200010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
051300010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
051400000000     d*---
051500000000     d* Variabili di memorizzazione
051600000000     d*---
051700000000     d mhcre           S              5                                         *consegna richiesta
051800000000     d mdcre           S             10
051900000000     d mtcre           S             50
052000000000     d mftce           S            100                                         *consegna partic.
052100000000     d mgcxe           S            100                                         *turno chiusura
052200000000     d mffd            S            100                                         *fermo deposito
052300010927     d mmncB           S             35                                         *NON consegna LAV
052400010927     d mmncC           S             35
052500010927     d mmncG           S             35                                         *NON consegna GIAC
052600010927     d mmncH           S             35
052700010927     d mmncN           S             35                                         *NON consegna NOFATT
052800010927     d mmncO           S             35
052900050701     d mmnc8           S             35
053000050701     d mmnc9           S             35
053100030206     d mtel            S             20                                         *N° tel destinatario
053200030206     d mban            S             35                                         *bancali
053300030207     d mscr            S            500                                         *supermercati
053400030210     d msdace          S             10
053500030210     d msorce          S              5
053600030210     d msdcre          S             10
053700030210     d mshcre          S              5
053800030210     d mstcre          S             30
053900030210     d msnom           S             50
054000030210     d mscrT           S            500    DIM(5)
054100030211     d wmscrT          S           2504
054200000000     d*---
054300000000     d* Schiere
054400000000     d*---
054500000000      * taabella clienti del cliente unificante
054600000000      * tabella tipi servizio
054700000000     d stsp            S              1    DIM(20)                              *tipi servizio
054800000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
054900000000      * tabella consegne anomale
055000030130     d scca            S              1    DIM(30)                              *consegna anomala
055100000000      * tabella disposizioni giacenza
055200000000     d sdis            S              1    DIM(10)                              *codice
055300000000     d sddis           S             20    DIM(10)                              *descrizione italian
055400000000      * tabella filiali
055500000000     d sorg            S              3  0 DIM(300)                             *filiale
055600000000     d sodes           S             30    DIM(300)                             *descrizione filiale
055700000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
055800160406     d sodpd           S              3    DIM(300)                             ntw
055900000000      * eventi memorizzati
056000000000     d smi             S              3  0 DIM(200)                             *indice
056100000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
056200000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
056300000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
056400000000     d smdeve          S             10    DIM(200)                             *data evento editata
056500000000     d smheve          S              5    DIM(200)                             *ora  evento editata
056600000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
056700000000     d smdev           S             30    DIM(200)                             *descrizione evento
056800000000     d smcev           S              3    DIM(200)                             *causale evento
056900130313     d smspe           S             16  0 DIM(200)                             *causale evento
057000000000      * eventi memorizzati x ordinamento
057100000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
057200000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
057300000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
057400000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
057500000000     d spdeve          S             10    DIM(200)                             *data evento editata
057600000000     d spheve          S              5    DIM(200)                             *ora  evento editata
057700000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
057800000000     d spdev           S             30    DIM(200)                             *descrizione evento
057900000000     d spcev           S              3    DIM(200)                             *causale evento
058000130313     d spspe           S             16  0 DIM(200)                             *causale evento
058100000000      * note giacenze memorizzate
058200051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
058300051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
058400000000      * note memorizzate
058500000000     d smnot           S            100    DIM(10)
058600030206     d*---
058700030206     d* definizioni per passaggio parametri
058800030206     d*---
058900000000     d esito           s              1
059000000000      * DS contenente i dati di INPUT
059100140603     d tis778dsi     e DS                  INZ(*EXTDFT)
059200050104     D  cAASI74                       4
059300050104     D                                     OVERLAY(AASI74)
059400000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
059500140530     d tis778dso     e DS                  INZ(*EXTDFT)
059600000000     d dev                           10    DIM(50) OVERLAY(dato74)
059700000000     d hev                            5    DIM(50) OVERLAY(orao74)
059800000000     d fle                           30    DIM(50) OVERLAY(opeo74)
059900041007     d eve                           30    DIM(50) OVERLAY(eveo74)
060000041007     d cev                            3    DIM(50) OVERLAY(cevo74)
060100000000     d nota                         100    DIM(10) OVERLAY(notao74)
060200130313     d spe             S             16  0 DIM(50)
060300040906
060400140616     d tis778dsM     e DS                  INZ(*EXTDFT)
060500140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
060600140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
060700140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
060800140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
060900140530      *
061000040906     D FIDN12DS      E DS
061100040906     D                                     INZ
061200040906     D  I12FEL       E
061300040906     D                                     INZ('E')
061400040906     D  I12FAN       E
061500040906     D                                     INZ('E')
061600040906     D  O12KeyAry                    14
061700040906     D                                     DIM(20)
061800040906     D                                     OVERLAY(O12KEY)
061900040906
062000040906     D O12KeyDS        DS
062100040906     D                                     INZ
062200040906     D  O12KeyAAC                     4S 0
062300040906     D  O12KeyFil                     3S 0
062400040906     D  O12KeyNCA                     7S 0
062500040906
062600020702      * DS lettura tabella "NTW"
062700020702     d DNTW          E DS
062800130326     d Ddstflr       E DS
062900041223
063000041223     D ChkCode         S             10U 0
063100041223     D nEsito          S              5I 0
063200051110     D esito10         S             10I 0
063300080131
063400100119     D*--------------------------------------------------
063500100119     D* Procedure name: GetDescrizioneIncassoContrassegno
063600100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
063700100119     D*                          o del contrassegno.
063800100121     D* Returns:        Descrizione modalità incasso contrassegno.
063900100119     D*--------------------------------------------------
064000100119     D GetDescrizioneIncassoContrassegno...
064100100119     D                 PR            35A
064200110304
064300110304     D*--------------------------------------------------
064400110304     D* Procedure name: IsContrassegnoCompensato
064500110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
064600110304     D*                          to con una fattura.
064700110304     D* Returns:        *ON = Compensato
064800110304     D*--------------------------------------------------
064900110304     D IsContrassegnoCompensato...
065000110304     D                 PR              N
065100120321
065200120321     D*--------------------------------------------------
065300120321     D* Procedure name: SetIdAssegnoMittente
065400120321     D* Purpose:        Imposta l'ID assegno mittente.
065500120321     D* Returns:        Esito.
065600120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
065700120321     D*                          segni mittente.
065800120321     D* Parameter:      priAbi => ABI assegno.
065900120321     D* Parameter:      priCab => CAB assegno.
066000120321     D*--------------------------------------------------
066100120321     D SetIdAssegnoMittente...
066200120321     D                 PR            10I 0
066300120321     D  priNumeroAssegno...
066400120321     D                                     LIKE(caNumO74)
066500120321     D  priAbi                             LIKE(caAbiO74)
066600120321     D  priCab                             LIKE(caCabO74)
066700130513
066800130513     D*--------------------------------------------------
066900130513     D* Procedure name: GetFilUrl
067000130513     D* Purpose:        Restituisce l'URL della filiale.
067100130513     D* Returns:        URL filiale.
067200130513     D* Parameter:      piFiliale => ID filiale
067300130513     D*--------------------------------------------------
067400130513     D GetFilUrl       PR           256A
067500130513     D  piFiliale                     3P 0 CONST
067600130513
067700130513
067800140527       //--------------------------------------------------------------
067900140527       //?Definizione prototipi.
068000140527       //--------------------------------------------------------------
068100140527      /copy gaitrasrc/srcprotopr,tis778R
068200140529      /copy gaitrasrc/srcconst,tis778R
068300140527       //--------------------------------------------------------------
068400140527       //?Definizione parametri programma.
068500140527       //--------------------------------------------------------------
068600140529     d tis778_GetBolla...
068700140527     d                 PI
068800140527     d prmRqsOpCode...
068900140527     d                               10I 0 CONST
069000140527     d prmRpyOpCode...
069100140527     d                               10I 0
069200140527     d prmRpyIdMsg...
069300140527     d                               10I 0
069400140527     d prmRqsFormato...
069500140527     d                               10A   CONST
069600140527     d prmRqsData...
069700140527     d                            32767A   OPTIONS(*VARSIZE)
069800140527     d prmRqsDataSize...
069900140527     d                               10I 0 CONST
070000140527     d prmRpyFormato...
070100140527     d                               10A   CONST
070200140527     d prmRpyData...
070300140527     d                            32767A   OPTIONS(*VARSIZE)
070400140527     d prmRpyDataSize...
070500140527     d                               10I 0 CONST
070600140527     d prmRpyFormMsg...
070700140527     d                               10A   CONST OPTIONS(*NOPASS)
070800140527     d prmRpyMessage...
070900140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
071000140527     d prmRpyMsgSize...
071100140527     d                               10I 0 CONST OPTIONS(*NOPASS)
071200080131
071300000000     i*--------------------------------------------------------------------------------------------*
071400000000     i* Input
071500000000     i*--------------------------------------------------------------------------------------------*
071600000000     i*---
071700000000     i* Indicatori lettura formati record -TITAS-
071800000000     i*---
071900000000     ititas000      01
072000000000     ititas010      02
072100000000     ititasp00      03
072200000000     c*--------------------------------------------------------------------------------------------*
072300000000     c* Main lines
072400000000     c*--------------------------------------------------------------------------------------------*
072500000000     c*
072600000000     c* operazioni iniziali -da fare sempre-
072700000000     c                   EXSR      rutinz
072800000000     c*
072900000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
073000000000if  1c                   IF        esito <> '1'                                 *no errori
073100100219     c*
073200100219     c                   MOVEL     dslnpi74      kaslnp
073300100219     c                   MOVEL     dsnrsi74      kasnrs
073400100219     c                   MOVEL     dsnspi74      kasnsp
073500100219     c*
073600111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
073700111103     c* restituisco errore.
073800111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
073900111103     c* e se non la trovo provo con l'anno precedente.
074000160318    1c                   IF        aasI74 <> *ZERO
074100100219     c                   Z-ADD     aasI74        kasaas
074200100219     c     keytas        SETLL     titas30c                               99
074300160318    2c                   IF        NOT *IN99
074400160318
074500160318     c     keytas        chain     fnblp01l
074600160318    3c                   if        %found(fnblp01l) and arbft1=' '
074700160318     c                   MOVEL     'P'           esito                          *bolla in part
074800160318   x3c                   else
074900111103     c                   EVAL      esito = '1'
075000160318    3c                   ENDIF
075100160318    2c                   ENDIF
075200160318
075300160318   x1c                   ELSE
075400000000     c                   Z-ADD     anncor        kasaas
075500000000     c     keytas        SETLL     titas30c                               99
075600000000if  2c                   IF        NOT *in99                                    *non trovato
075700160318
075800160318     c     keytas        chain     fnblp01l
075900160318    3c                   if        %found(fnblp01l) and arbft1=' '
076000160318     c                   MOVEL     'P'           esito                          *bolla in part
076100160318   x3c                   else
076200160318
076300160318     C* verifico se in partenza: se non è in partenza allora verifico anno precedente
076400000000     c                   Z-ADD     annpre        kasaas
076500000000     c     keytas        SETLL     titas30c                               99
076600160318if  4c                   IF        NOT *in99                                    *non trovato
076700000000     c                   MOVEL     '1'           esito                          *errore
076800160318e   4c                   ENDIF
076900160318e   3c                   ENDIF
077000000000e   2c                   ENDIF
077100160318    1c                   ENDIF
077200160127     c
077300160314    2c                   if        esito = '1'
077400140617     c                   eval      widmsg = 'TIS0564'
077500140616     c                   eval      widcampo = 'NSPEDIZI74'
077600140616     c                   clear                   wparm
077700140616     c                   exsr      messaggi
077800160314    2c                   endif
077900160318     c
078000141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
078100141126     c                   eval      kasaasm = kasaas
078200141126     c                   eval      kaslnpm = kaslnp
078300141126     c                   eval      kasnrsm = kasnrs
078400141126     c                   eval      kasnspm = kasnsp
078500141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
078600140715      * ultima figlia per forzare i dati in output
078700160318     c                   if        esito<>'P'
078800140714     c                   exsr      cercafiglia
078900160318     c                   else
079000160318     c                   exsr      cercamamma
079100160318     c                   endif
079200000000     c*
079300160127     c* Se bolla in FNBLP in partenza eseguo routine a parte
079400160314    2c                   if        esito='P'
079500160127     c
079600160127     c* imposta i campi della DS di Output dagli eventi
079700160127     c                   EXSR      impdsoevb
079800160127     c
079900160127     c* imposta i campi del C%Ass dal fiar9
080000160127     c                   EXSR      impdsoar9
080100160127     c
080200160127     c* impostazione campi in DS di output
080300160309     c                   exsr      impds_soloblp
080400160309     c                   exsr      impdsoarb
080500160309     c
080600160309     c* imposta i campi della DS di Output dalle note
080700160309     c                   EXSR      impdsonot
080800160314    2c                   endif
080900160127     c
081000000000     c* se trovata, legge la bolla
081100160127if  2c                   IF        esito <> '1'  and esito<>'P'
081200000000     C                   SETOFF                                       010203
081300140715     c     keytas        SETLL     titas30c
081400000000     c     keytas        READE     titas30c                               99    *no errore
081500000000if  3c                   IF        *in99                                        *non trovato
081600000000     c                   MOVEL     '1'           esito                          *errore
081700140617     c                   eval      widmsg = 'TIS0734'
081800140616     c                   eval      widcampo = 'NSPEDIZI74'
081900140616     c                   clear                   wparm
082000140616     c                   exsr      messaggi
082100000000x   3c                   ELSE                                                   *trovata
082200000000if  4c                   IF        *in01                                        *lettura titas000
082300000000     c                   EVAL      titasdssav = titasds
082400000000e   4c                   ENDIF
082500000000if  4c                   IF        *in02                                        *lettura titas010
082600000000     c                   EVAL      titasdssav = titasds
082700000000e   4c                   ENDIF
082800000000if  4c                   IF        *in03                                        *lettura titasp00
082900000000     c                   EVAL      titasdssav = titasds
083000000000e   4c                   ENDIF
083100141117      * se trovo la figlia ma ancora non è su titas
083200150617      * Se la bolla non è legata, nei campi della "figlia" ovvero F_  ci sono
083300150617      *  i campi della bolla originale passata
083400141117     c                   if        keyforzata <> *blank and
083500141117     c                             f_tasnsp = 0
083600141117     c                   eval      titasdssavf = titasdssav
083700141120     c                   else
083800141120     c                   clear                   keyforzata
083900141117     c                   endif
084000000000e   3c                   ENDIF
084100000000     c*
084200000000     c* controlla validità bolla
084300000000     c                   EXSR      chktas
084400000000     c*
084500030130     c* imposta i campi della DS di Output
084600000000if  3c                   IF        esito <> '1'                                 *no errore
084700140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
084800140714      * spedizione richiesta a video sempre
084900100225     c*
085000140716     c                   EVAL      dstaslnp = s_taslnp
085100140716     c                   EVAL      dstasnrs = s_tasnrs
085200140716     c                   EVAL      dstasnsp = s_tasnsp
085300140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
085400140723     c* chain ar5 con bolla richiesta a video
085500160127     c                   Z-ADD     s_tasaas      kr5aas
085600140723     c                   Z-ADD     s_taslnp      kr5lnp
085700140723     c                   Z-ADD     s_tasnrs      kr5nrs
085800140723     c                   Z-ADD     s_tasnsp      kr5nsp
085900100225     c                   MOVEL     'GEN'         kr5trd
086000100225     c     keyar5        CHAIN     fiar531c
086100100225     C                   IF        %FOUND
086200100225     C                   EVAL      DAR5GEN = AR5Uni
086300100225     C                   ELSE
086400100225     C                   RESET                   DAR5GEN
086500100225     C                   ENDIF
086600000000     c*
086700000000     c* imposta i campi della DS di Output dalle bolle
086800000000     c                   EXSR      impdsotas
086900000000     c*
087000000000     c* imposta i campi della DS di Output dagli eventi
087100000000     c                   EXSR      impdsoevb
087200000000     c*
087300000000     c* imposta i campi della DS di Output dal contrassegno
087400000000     c                   EXSR      impdsocsb
087500000000     c*
087600000000     c* imposta i campi della DS di Output dalla giacenza
087700000000     c                   EXSR      impdsogcp
087800000000     c*
087900000000     c* imposta i campi della DS di Output dalle note
088000000000     c                   EXSR      impdsonot
088100040330     c*
088200040330     c* imposta i campi della DS di Output del destinatario.
088300040330     c                   EXSR      ImpDSDest
088400010716     C*
088500160323     c* imposta i campi della DS di Output dalla firma DPD/EEX
088600010716     c                   EXSR      ImpdsoDPD
088700040913     C*
088800040913     c* imposta i campi della DS di Output di addebito immagine LDV.
088900040913     c                   EXSR      AddebitoLDV
089000000000e   3c                   ENDIF
089100000000e   2c                   ENDIF
089200140613e   1c                   ENDIF
089300160314     c
089400140919      *riempie campi con riferimenti alla consegna
089500160314      *  non per bolla ancora in partenza
089600160314     c                   if        esito <> '1'  and esito <>'P'
089700160314     c
089800140919     c                   if        f_tasdcm > 0
089900140919     c                   movel     f_tasdcm      wdat
090000140919     c                   exsr      edtdat
090100140919     c                   eval      dtconmeo74  = wdate
090200140919     c                   MOVEL     f_tashmc      wora
090300140919     c                   EXSR      edtora
090400140919     c                   eval      orconmeO74  =  worae
090500140919     c                   eval      flgbolcO74  =  'S'
090600140919     c                   eval      flgboldO74  =  'N'
090700141014      *firmatario distinta PDA
090800141014     c                   clear                   firmato74
090900141015     c                   if        gen§ar5pdaco <> 'NO'
091000141014     c                   exsr      repfirmat
091100141014     c                   else
091200141014     c                   if        f_tasgma <> *blank
091300141015     C                   EVAL      tblKey = f_tasgma
091400141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
091500141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
091600141014     C                             :rpyOpCode:rpyEsito)
091700141014     C                   IF        rpyOpCode = 'FOUND'
091800141014     c                   if        §7r2fi = 'S'
091900141014     c                   exsr      repfirmat
092000141014     c                   endif
092100141014     c                   endif
092200141014     c                   endif
092300141014     c                   endif
092400141014      *else consegna
092500140919     c                   else
092600140919     c                   eval      flgbolcO74  =  'N'
092700140919     c                   if        F_tasndc > 0 and f_tasddc > 0
092800141119     c                             and f_tasddc <= datcor
092900140919     c                   eval      flgboldo74 = 'S'
093000140919     c                   else
093100140919     c                   eval      flgboldO74  =  'N'
093200140919     c                   endif
093300140919     c                   endif
093400150617     c
093500140613if  3c                   IF        esito <> '1'                                 *no errore
093600150615     c***********        IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
093700150615     c************                 and cev(49) = ' ' and cev(50) = ' ' or
093800150615     c
093900150615     c                   if        f_tasdcm = 0 and keyforzata = *blank
094000130312     c                   exsr      srarb
094100140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
094200140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
094300140605     c                   exsr      impdsoarb
094400140604     c                   endif
094500140613     c                   endif
094600140613     c                   endif
094700141016     c                   endif
094800000000     c*
094900140530     C                   exsr      routend
095000141014      *__________________________________________________________________
095100141014     c     repfirmat     begsr
095200141014      *__________________________________________________________________
095300141014     c                   Z-ADD     f_tasaas      ka4aas
095400141014     c                   Z-ADD     f_taslnp      ka4lnp
095500141014     c                   Z-ADD     f_tasnrs      ka4nrs
095600141014     c                   Z-ADD     f_tasnsp      ka4nsp
095700141014     c                   MOVEL     '1'           ka4trc
095800141014     c     keyta4        CHAIN     tita430c                           99
095900141014if  1c                   IF        NOT *in99
096000141014     c                   MOVEL     ta4not        firmato74
096100141014e   1c                   ENDIF
096200141014     c                   endsr
096300141015      *__________________________________________________________________
096400141015     c     repfirmatARB  begsr
096500141015      *__________________________________________________________________
096600141015      *firmatario da arb
096700141015     c                   Z-ADD     arbaas        ka4aas
096800141015     c                   Z-ADD     arblnp        ka4lnp
096900141015     c                   Z-ADD     arbnrs        ka4nrs
097000141015     c                   Z-ADD     arbnsp        ka4nsp
097100141015     c                   MOVEL     '1'           ka4trc
097200141015     c     keyta4        CHAIN     fiar401l                           99
097300141015if  1c                   IF        NOT *in99
097400141015     c                   MOVEL     ar4not        firmato74
097500141015e   1c                   ENDIF
097600141015     c                   endsr
097700000000     c*--------------------------------------------------------------------------------------------*
097800000000     c* controlla validità bolla
097900000000     c*--------------------------------------------------------------------------------------------*
098000000000     c     chktas        BEGSR
098100000000     c*
098200000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
098300000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
098400000000     c                   Z-ADD     s_tasmgs      dsmgsB
098500000000if  1c                   IF        dsdatB < datpre
098600000000     c                   MOVEL     '1'           esito                          *errore
098700000000e   1c                   ENDIF
098800030130     c* se l'esito è positivo, salva alcuni dati della bolla
098900150617     c*********          EVAL      savtsp = f_tastsp
099000000000     c*
099100140718     C                   EVAL      wOrg = f_tasLna
099200051222     C                   EXSR      setLnaItaEst
099300051222     C
099400000000     c                   ENDSR
099500000000     c*--------------------------------------------------------------------------------------------*
099600000000     c* imposta i campi della DS di Output dalle bolle e files correlati
099700000000     c*--------------------------------------------------------------------------------------------*
099800140604     c     impdsotas     BEGSR
099900170301     c                   clear                   dcli
100000140604     c*
100100140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
100200140604     c                   EVAL      lnaDescO74 = sodes(i)
100300140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
100400150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
100500150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
100600150116     c                   MOVE      s_tasmgs      aasmgsO74
100700150116      *
100800140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
100900140718     c                   MOVEL     f_taslna      lineaaro74
101000140604     c*
101100140718     c                   MOVEL     f_tastbl      a1
101200140718     c                   MOVEL     f_tastbl      tpporto74
101300140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
101400140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
101500140604x   1c                   ELSE                                                   *Pagamento del
101600140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
101700140604e   1c                   ENDIF
101800140716     c                   MOVEL     f_tastsp      tpservio74
101900140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
102000140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
102100140604     C                             rpyOpCode:rpyEsito)
102200140604     C                   EVAL      corro74 = §5EDes
102300140604     c*
102400140716     c                   EVAL      destino74 = f_tasrsd
102500140716     c                   EVAL      desindo74 = f_tasind
102600140716     c                   EVAL      descapo74 = f_tascad
102700140716     c                   EVAL      desloco74 = f_taslod
102800140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
102900150326if  1c                   IF        f_tasnzd  = *blanks
103000150326     c                   EVAL      desnazo74 = f_tasprd
103100150326x   1c                   ELSE
103200150326     c***                EVAL      desnazo74 = f_tasnzd
103300150326     c***                EVAL      desnzdo74 = f_tasnzd
103400150326     c* 26/03/15: Attiviamo le specifiche per provincia export
103500150326     c                   EVAL      desnazo74 = f_tasprd
103600150326     c                   EVAL      desnzdo74 = f_tasnzd
103700150326e   1c                   ENDIF
103800140604     c*
103900140716     c                   EVAL      collio74  = f_tasncl
104000140716     c                   EVAL      pesoo74   = f_taspkf
104100140716     c                   EVAL      volumeo74 = f_tasvlf
104200160122     c***                EVAL      naturao74 = f_tasnas
104300140604     c*
104400140716     c                   EVAL      fatnumo74  = f_tasnft
104500140716     c                   MOVEL     f_tasdft      wdat
104600140604     c                   EXSR      edtdat
104700140604     c                   EVAL      fatdatao74 = wdate
104800140604     c*
104900140716     c                   EVAL      disnumo74  = f_tasndc
105000140716c    c                   MOVEL     f_tasddc      wdat
105100140604     c                   EXSR      edtdat
105200140604     c                   EVAL      disdatao74 = wdate
105300140604     **?Immagine lettera di vettura.
105400140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
105500140604     C                   IF        LDVO74 <> 'N'
105600140604     c                   CLEAR                   dtasflo
105700140716     c                   MOVEL     f_tasflo      dtasflo
105800140604     c                   IF        §floiml <> *BLANK
105900140604     c                   EVAL      ldvo74 = 'S'
106000140604     C                   EVAL      SpImLDVO74 =
106100140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
106200140716     C                             %EDITC(f_taslnp:'X') +
106300140716     C                             %EDITC(f_tasnrs:'X') +
106400140716     C                             %EDITC(f_tasnsp:'X')
106500140604     **?Reperisco il check code della spedizione POD
106600140604     C                   EVAL      PChkCdeO74 =
106700140604     C                             GetSpeChkCde(
106800140716     C                             %EDITC(f_tasaas:'X'):
106900140716     C                             %EDITC(f_taslnp:'X') +
107000140716     C                             %EDITC(f_tasnrs:'X') +
107100140716     C                             %EDITC(f_tasnsp:'X'):
107200140604     C                             ChkCode:nEsito)
107300140604     C                   ENDIF
107400140604     C                   ENDIF
107500140604     c*
107600170310     c                   MOVE      f_tasccm      alfa4
107700170301     c                   clear                   mito74
107800170310     c                   clear                   solomito74
107900170310if  1c                   IF        alfa4 <> '8888'                               *codificato
108000170301
108100170301     c* Verifico da tabella CLI quale ragione sociale mittente devo visualizzare
108200170301     c                   exsr      Tabcli
108300170301
108400170301x   1c                   ELSE                                                   *non codificato
108500170301     c                   Z-ADD     f_tasaas      kaaaas
108600170301     c                   Z-ADD     f_taslnp      kaalnp
108700170301     c                   Z-ADD     f_tasnrs      kaanrs
108800170301     c                   Z-ADD     f_tasnsp      kaansp
108900170301     c                   MOVEL     'M'           kaatrc
109000170301     c     keytaa        CHAIN     titaa30c                           99
109100170301if  2c                   IF        NOT *in99
109200170301     c                   EVAL      mito74    = taarsc
109300170301     c                   EVAL      mitindo74 = taaind
109400170301     c                   EVAL      mitcapo74 = taacap
109500170301     c                   EVAL      mitloco74 = taaloc
109600170301if  3c                   IF        taanaz = *blanks
109700170301     c                   EVAL      mitnazo74 = taaprv
109800170301x   3c                   ELSE
109900170301     c                   EVAL      mitnazo74 = taanaz
110000170301e   3c                   ENDIF
110100170301e   2c                   ENDIF
110200170301e   1c                   ENDIF
110300170301     c
110400140604     c* 2a ragione sociale destinatario.
110500140604     c                   CLEAR                   tita4000
110600140604     c                   CLEAR                   tita4p00
110700140716     c                   Z-ADD     f_tasaas      ka4aas
110800140716     c                   Z-ADD     f_taslnp      ka4lnp
110900140716     c                   Z-ADD     f_tasnrs      ka4nrs
111000140716     c                   Z-ADD     f_tasnsp      ka4nsp
111100140604     c                   MOVEL     'D'           ka4trc
111200140604     c     keyta4        CHAIN     tita430c                           99
111300140604if  1c                   IF        NOT *in99
111400140604     c                   MOVEL     ta4not        desti2O74
111500140604e   1c                   ENDIF
111600140604     c* rif partner estero
111700140604     c                   CLEAR                   tita4000
111800140604     c                   CLEAR                   tita4p00
111900140604     c                   CLEAR                   dsbl4e
112000140716     c                   Z-ADD     f_tasaas      ka4aas
112100140716     c                   Z-ADD     f_taslnp      ka4lnp
112200140716     c                   Z-ADD     f_tasnrs      ka4nrs
112300140716     c                   Z-ADD     f_tasnsp      ka4nsp
112400140604     c                   MOVEL     'E'           ka4trc
112500140604     c     keyta4        CHAIN     tita430c                           99
112600140604if  1c                   IF        NOT *in99
112700140604     c                   MOVEL     ta4not        dsbl4e
112800140604e   1c                   ENDIF
112900140604     c
113000140604     c                   CLEAR                   tita4000
113100140604     c                   CLEAR                   tita4p00
113200140604     c                   CLEAR                   dta4a
113300140716     c                   Z-ADD     f_tasaas      ka4aas
113400140716     c                   Z-ADD     f_taslnp      ka4lnp
113500140716     c                   Z-ADD     f_tasnrs      ka4nrs
113600140716     c                   Z-ADD     f_tasnsp      ka4nsp
113700140604     c                   MOVEL     'A'           ka4trc
113800140604     c     keyta4        CHAIN     tita430c                           99
113900140604if  1c                   IF        NOT *in99
114000140604     c                   MOVEL     ta4not        dta4a
114100140604e   1c                   ENDIF
114200140604     ** Riferimento mittente numerico (c'è sempre).
114300140716     C                   EVAL      rifero74 = f_tasrmn
114400140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
114500140604     C                   IF        §ta4arma <> *BLANK AND
114600140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
114700140604     C                   EVAL      rifero74a = §ta4arma
114800140604     C                   ENDIF
114900160122     c** Natura merce
115000160122     c                   EVAL      naturao74 = §ta4anas
115100140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
115200140604     C                   IF        §b4erp <> *BLANK AND
115300140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
115400140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
115500140604     C                   EVAL      rpto74 = §b4erp
115600140604     C                   ENDIF
115700140604
115800140604     **?Reperisco il check code della spedizione.
115900140604     C                   EVAL      SChkCdeO74 =
116000140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
116100140604     C                             NSpedizI74:ChkCode:nEsito)
116200140604
116300140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
116400140604     C                   IF        KSCI74 <> *BLANK
116500140604     c                   SETOFF                                           98
116600140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
116700140604     C                   RESET                   tis7700i0
116800141126     C                   EVAL      tis7700i0.aas = kasaasm
116900141126     C                   EVAL      tis7700i0.lnp = kaslnpm
117000141126     C                   EVAL      tis7700i0.nrs = kasnrsm
117100141126     C                   EVAL      tis7700i0.nsp = kasnspm
117200140604     C                   EVAL      tis7700i0.ksu = kscI74
117300140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
117400140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
117500140604     C                             : rpyEsito
117600140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
117700140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
117800140604     C                             )
117900140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
118000140604     C                             tis7700o0.bollValida = *ON)
118100140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
118200140604if  2c                   IF        NOT *in98                                      *non trovato
118300140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
118400140604     C                   reset                   TIS778DSO
118500160818     C******             EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
118600160818     c                   eval      esito='1'
118700160818     c                   eval      widmsg = 'TIS0849'
118800160818     c                   eval      widcampo = 'NSPEDIZI74'
118900160818     c                   clear                   wparm
119000160818     c                   exsr      messaggi
119100160818     c                   exsr      routend
119200160818     C******             RETURN
119300140604     C                   ENDIF
119400140604e   2c                   ENDIF
119500140604     C                   ENDIF
119600140604     c*
119700140604     ** Importo assicurato.
119800140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
119900140716     C                   EVAL      vasO74 = f_tasVas
120000141015     C                   EVAL      iasO74 = f_tasIas
120100140604     C                   ENDIF
120200140604     **
120300140604     c                   ENDSR
120400170301     c*--------------------------------------------------------------------------------------------*
120500170301     c* imposto campi del mittente da tabella CLI
120600170301     c*--------------------------------------------------------------------------------------------*
120700170301     c     tabcli        BEGSR
120800170310     c                   clear                   wusarmo           1
120900170301     C                   RESET                   tibs02ds
121000170301     C                   EVAL      t02Cod = 'CLI'
121100170301     C                   EVAL      t02Ke1 = %editc(ccmo74:'X')
121200170301     C                   EXSR      getTntbe00f
121300170310     c
121400170310     c* e mi hanno passarto una versione A o B --> significa che NON devo sostituire RSM
121500170313     c                   if        VERSIONI74<'C'
121600170310     c                   eval      t02err='E'
121700170310     c                   endif
121800170301
121900170301    0c                   IF        t02Err=' ' and t02atb=' '
122000170310    c
122100170310     c                   eval      solomito74=§CLINOIMTT
122200170310     c
122300170301     c* vedo se presente mittente originale
122400170301    1c                   if        §CLIMITOR='S'
122500170301     c* mittente originale da ARB
122600170301    2c                   if        wbol='ARB'
122700170301     c                   eval      mito74=arbrmo
122800170310
122900170310     c                   if        arbrmo<>*blanks
123000170310     c                   eval      wusarmo='S'
123100170310     c                   endif
123200170301   x2c                   else
123300170301     c                   Z-ADD     f_tasaas      kaaaas
123400170301     c                   Z-ADD     f_taslnp      kaalnp
123500170301     c                   Z-ADD     f_tasnrs      kaanrs
123600170301     c                   Z-ADD     f_tasnsp      kaansp
123700170301     c                   MOVEL     'O'           kaatrc
123800170301     c     keytaa        CHAIN     titaa30c
123900170301if  3c                   IF        %found(titaa30c)
124000170301     c                   eval      mito74=taarsc
124100170310     c                   eval      wusarmo='S'
124200170301    3c                   endif
124300170301    2c                   endif
124400170301    1c                   endif
124500170301
124600170301    1c                   if        mito74=*blanks and §CLITFNTC='S'
124700170301     c                   eval      k_NTCAPL =  'C'
124800170301     c                   eval      k_NTCNK1 =  '0151' + %editc(ccmo74:'X')
124900170301     c                   eval      k_NTCNK2 =  *BLANK
125000170301     c                   eval      k_NTCTNT =  'AM'
125100170301     c     ktfntc        chain     tfntc01l
125200170301    2c                   if        %found(tfntc01l)
125300170301     c                   eval      mito74=ntcrnt
125400170301    2c                   endif
125500170301    1c                   endif
125600170301    0c                   endif
125700170301     c
125800170301     c* questo pezzo lo faccio solo per TITAS
125900170310    1c                   if        wbol<>'ARB'
126000170301     c                   CLEAR                   bs69ds
126100170301     c                   CLEAR                   acods
126200170301     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
126300170301     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
126400170301     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
126500170301     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
126600170301     c                   PARM                    bs69ds
126700170301     c                   PARM                    acods
126800170301     c                   PARM                    indds
126900170301     c                   PARM                    clpds
127000170301     c                   PARM                    clsds
127100170310
127200170301if  2c                   IF        o69err <> '1'                                *no errore
127300170310
127400170310    1c                   if        mito74=*blanks
127500170301     c                   EVAL      mito74    = acorag
127600170310     c                   endif
127700170301     c                   EVAL      mitindo74 = indvia
127800170301     c                   EVAL      mitloco74 = indcit
127900170301     c                   MOVEL     indsta        a3
128000170301if  3c                   IF        a3 = *blanks
128100170301     c                   EVAL      mitnazo74 = indprv
128200170301x   3c                   ELSE
128300170301     c                   EVAL      mitnazo74 = a3
128400170301e   3c                   ENDIF
128500170301if  3c                   IF        indcae <> *blanks
128600170301     c                   EVAL      mitcapo74 = indcae
128700170301X   3c                   ELSE
128800170301     c                   MOVEL     indcap        a5
128900170301     c                   EVAL      mitcapo74 = a5
129000170301e   3c                   ENDIF
129100170301e   2c                   ENDIF
129200170301e   1c                   ENDIF
129300170310     c
129400170310     c* questo pezzo lo faccio solo per blp/arb
129500170310    1c                   if        mito74=*blanks and wbol= 'ARB'
129600170310     c                   EVAL      mito74    = arbrsm
129700170310     c                   endif
129800170310     c
129900170301     c                   ENDSR
130000000000     c*--------------------------------------------------------------------------------------------*
130100000000     c* imposta i campi della DS di Output dagli eventi
130200000000     c*--------------------------------------------------------------------------------------------*
130300000000     c     impdsoevb     BEGSR
130400000000     c*
130500000000     c* azzera le variabili di lavoro
130600000000     c                   CLEAR                   tote                           *contatore eventi
130700000000     c                   CLEAR                   seql                           *sequenza legami
130800000000     c                   CLEAR                   spsort                         *per ordinamento
130900000000     c                   CLEAR                   spseql                         *di memorizzazione
131000000000     c                   CLEAR                   spseqe
131100000000     c                   CLEAR                   spdevhev
131200000000     c                   CLEAR                   spdeve
131300000000     c                   CLEAR                   spheve
131400000000     c                   CLEAR                   spfle
131500000000     c                   CLEAR                   spdev
131600000000     c                   CLEAR                   spcev
131700130312     c                   CLEAR                   spspe
131800150618     c                   CLEAR                   ds7o
131900000000     c*
132000000000     c* imposta la sequenza legami per la bolla di Input
132100000000     c                   Z-ADD     1             seql                           *primo legaqme
132200000000     c*---
132300000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
132400000000     c*---
132500160127     c****
132600160127     c* Bolla presa in carico se ancora da partire
132700160420     c**                 if        esito='P'
132800160420     c**                 MOVEL     '700'         wcev                           *causale evento
132900160420     c**                 EXSR      deccevsta
133000160420if  4c**                 IF        werr <> '1'                                  *no errore
133100160420     c**                 ADD       1             tote
133200160420     c**                 MOVEL     wcdex         smdev(tote)
133300160420     c**                 exsr      evbinca
133400160420     c**                 endif
133500160420     c**                 endif
133600000000     c***
133700000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
133800000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
133900000000     c*   si trasforma nella consegna anomala della mamma
134000000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
134100000000     c***
134200000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
134300000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
134400160318     c
134500160318    0c                   if        esito<>'P'
134600000000     c                   EVAL      kblaan = s_tasaas
134700000000     c                   EVAL      kbllpn = s_taslnp
134800000000     c                   EVAL      kblnrn = s_tasnrs
134900000000     c                   EVAL      kblnsn = s_tasnsp
135000000000     c     keylbl        CHAIN     fnlbl01l                           99
135100000000if  1c                   IF        NOT *in99                                    *ha la mamma
135200000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
135300000000     c                   EVAL      kasaas = lblaap
135400000000     c                   EVAL      kaslnp = lbllpp
135500000000     c                   EVAL      kasnrs = lblnrp
135600000000     c                   EVAL      kasnsp = lblnsp
135700000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
135800000000if  2c                   IF        NOT *in99 AND
135900000000     c                             tascca <> *blanks                            *ha cons.anomala
136000060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
136100060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
136200060622     C                             :rpyEsito)
136300060622if  3c*                  IF        *in98                                        *trovata
136400060626     C                   IF        rpyOpCode = 'FOUND'
136500041222     **?Consegna anomala, POD non visualizzabile.
136600060622     C*                  IF        sapod(jj) = 'N'
136700060622     C                   IF        §7OPODImg = 'N'
136800041222     C                   EVAL      LDVO74 = 'N'
136900041222     C                   ENDIF
137000060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
137100060622     C                   IF        §7oinc <> *blanks                            *ok per internet
137200000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
137300041222e   3c                   ENDIF
137400000000e   3c                   ENDIF
137500000000e   2c                   ENDIF
137600000000e   1c                   ENDIF
137700160318e   0c                   ENDIF
137800000000     c*
137900030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
138000000000     c                   ADD       1             tote
138100060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
138200060622     c                   MOVEL     §7odei        smdev(tote)
138300030203x   3c                   ELSE                                                   *evento telef.P.O.
138400060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
138500030203e   3c                   ENDIF
138600030203e   2c                   ENDIF
138700030203     c*
138800030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
138900060622     c                   MOVEL     §7ocev        wcev                           *causale evento
139000000000     c                   EXSR      evbrit                                       *imposta evento
139100030203x   2c                   ELSE                                                   *NO sost.ritiro
139200160318     c
139300160408     c* per bolle in partenza devo impostare la data ritiro solo se spuntato in entrata
139400160408     c***                if        esito<>'P'
139500160413     c                   if        esito= 'P'
139600160413     c                   if        arbdse>0
139700160408     c                   eval      s_tasdrt=arbdrt
139800160413     c                   else
139900160413     c                   eval      s_tasdrt=00000000
140000160408     c                   endif
140100160413     c                   endif
140200160318     c
140300030203if  3c                   IF        s_tasdrt > *zeros
140400000000     c                   MOVEL     '701'         wcev                           *causale evento
140500000000     c                   EXSR      deccevsta
140600030203if  4c                   IF        werr <> '1'                                  *no errore
140700000000     c                   ADD       1             tote
140800030130     c                   MOVEL     wcdex         smdev(tote)
140900000000     c                   EXSR      evbrit                                       *imposta evento
141000030203e   4c                   ENDIF
141100030203e   3c                   ENDIF
141200030203e   2c                   ENDIF
141300160408e   2c***                ENDIF
141400140710     ** ID bolla madre.                                                          dubito figlia
141500160318     c                   if        esito='P'
141600160318     c                   EVAL      waas = arbaas
141700160318     c                   EVAL      wlnp = arblnp
141800160318     c                   EVAL      wnrs = arbnrs
141900160318     c                   EVAL      wnsp = arbnsp
142000160318     c                   else
142100160318     c
142200160318     c* questi dati per le bolle solo in partenza (non legate)
142300160318     c*  non li devo caricare
142400080131     c                   EVAL      waas = s_tasaas
142500080131     c                   EVAL      wlnp = s_taslnp
142600080131     c                   EVAL      wnrs = s_tasnrs
142700080131     c                   EVAL      wnsp = s_tasnsp
142800000000     c* Partenza merce
142900000000     c                   EVAL      lduc = s_tasduc                              *data partenza
143000000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
143100000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
143200000000     c                   EXSR      evbpar
143300000000     c* Arrivo merce
143400110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
143500110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
143600140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
143700140723     c                             gen§ar5ArrDt <> *ZERO
143800140723     c                   MONITOR
143900140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
144000140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
144100140723     c                   ON-ERROR
144200140723     c                   ENDMON
144300140723     c                   ENDIF
144400100225     c                   EVAL      llna = s_taslna                              *linea arrivo
144500150617     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
144600090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
144700090826     c                   EXSR      evbarr
144800000000     c*
144900000000     c* Consegna merce
145000000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
145100000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
145200000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
145300000000     c                   EVAL      llna = s_taslna                              *linea arrivo
145400150618     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
145500000000     c                   EXSR      evbcon
145600000000     c* Lettera anomalia
145700000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
145800000000     c                   EXSR      memdct
145900000000e   1c                   ENDIF
146000000000     c*---
146100000000     c* memorizza gli eventi dal file eventi della bolla di input
146200000000     c*---
146300160406     c* devo reimpostare la linea di arrivo della bolla richiesta, perchè potrei
146400150618     c*  aver chainato una bolla "mamma" ed aver sporcato il campo
146500150618     c                   eval      taslna=s_taslna
146600000000     c                   EXSR      memevb
146700000000     c*---
146800000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
146900000000     c*---
147000000000     c                   EXSR      elaevelbl
147100160318     c                   endif
147200000000     c*
147300000000     c* imposta contatore eventi da visualizzare nella DS di Output
147400000000     c                   EVAL      cnteveo74 = tote
147500000000     c*---
147600000000     c* ordina gli eventi per sequenza legami / Data+ora evento
147700000000     c*---
147800000000     c* compone la schiera da ordinare
147900000000do  1c     1             DO        200           j
148000000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
148100000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
148200000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
148300000000     c                   Z-ADD     smi(j)        dsi                             -indice
148400000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
148500000000e   1c                   ENDDO
148600000000     c*
148700000000     c* ordina la schiera composta
148800000000     c                   SORTA     spsort
148900000000     c*
149000000000     c* ricompone le schiere da quella ordinata
149100000000     c                   Z-ADD     *zeros        jj
149200000000do  1c     1             DO        200           j
149300000000if  2c                   IF        spsort(j) >*zeros
149400000000     c                   ADD       1             jj
149500000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
149600000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
149700000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
149800000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
149900000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
150000000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
150100000000     c                   MOVEL     smheve(dsi)   spheve(jj)
150200000000     c                   MOVEL     smfle(dsi)    spfle(jj)
150300000000     c                   MOVEL     smdev(dsi)    spdev(jj)
150400000000     c                   MOVEL     smcev(dsi)    spcev(jj)
150500130312     c                   MOVEL     smspe(dsi)    spspe(jj)
150600000000e   2c                   ENDIF
150700000000e   1c                   ENDDO
150800101104     **
150900101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
151000101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
151100101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
151200101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
151300101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
151400101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
151500101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
151600101104     ** Ricordati che l'ordine per data e ora è discendente.
151700101104     C                   EVAL      j = 0
151800101104     C                   DOU       j = 0
151900101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
152000101104     C                                        : spcev : j+1)
152100101104     C                   IF        j = 0
152200101104     C                   LEAVE
152300101104     C                   ENDIF
152400101104     C                   IF        j > 1 AND
152500101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
152600101104     C                   EVAL      dev(1) = spdeve(j-1)
152700101104     C                   EVAL      hev(1) = spheve(j-1)
152800101104     C                   EVAL      fle(1) = spfle(j-1)
152900101104     C                   EVAL      eve(1) = spdev(j-1)
153000101104     C                   EVAL      cev(1) = spcev(j-1)
153100130312     C                   EVAL      spe(1) = spspe(j-1)
153200101104     C                   EVAL      spdeve(j-1) = spdeve(j)
153300101104     C                   EVAL      spheve(j-1) = spheve(j)
153400101104     C                   EVAL      spfle(j-1) = spfle(j)
153500101104     C                   EVAL      spdev(j-1) = spdev(j)
153600101104     C                   EVAL      spcev(j-1) = spcev(j)
153700130312     C                   EVAL      spspe(j-1) = spspe(j)
153800101104     C                   EVAL      spdeve(j) = dev(1)
153900101104     C                   EVAL      spheve(j) = hev(1)
154000101104     C                   EVAL      spfle(j) = fle(1)
154100101104     C                   EVAL      spdev(j) = eve(1)
154200101104     C                   EVAL      spcev(j) = cev(1)
154300130312     C                   EVAL      spspe(j) = spe(1)
154400101104     C                   ENDIF
154500101104     C                   ENDDO
154600000000     c*
154700120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
154800120831     c                   CLEAR                   cntEveO74
154900000000do  1c     1             DO        50            j
155000120831     c                   IF        spcev(j) = *BLANK
155100120831     c                   LEAVE
155200120831     c                   ENDIF
155300000000     c                   EVAL      dev(j) = spdeve(j)
155400000000     c                   EVAL      hev(j) = spheve(j)
155500000000     c                   EVAL      fle(j) = spfle(j)
155600041007     c                   EVAL      eve(j) = spdev(j)
155700041007     c                   EVAL      cev(j) = spcev(j)
155800130312     c                   EVAL      spe(j) = spspe(j)
155900101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
156000100909     c                   CLEAR                   hev(j)
156100100909     c                   ENDIF
156200120831     c                   EVAL      cntEveO74 += 1
156300000000e   1c                   ENDDO
156400000000     c*
156500000000     c                   ENDSR
156600000000     c*--------------------------------------------------------------------------------------------*
156700000000     c* memorizza gli eventi dal file eventi della bolla di input
156800000000     c*--------------------------------------------------------------------------------------------*
156900000000     c     memevb        BEGSR
157000150617     c* il programma usa il campo TASLNA per attribuire gli eventi della bolla
157100150617     c*  usa il TITAS che ha letto per ultimo che può essere quello della bolla  mamma
157200150617     c*  o della bolla figlia
157300150617     c*  per sapere se la bolla è italia o estera non devo usare l'ultima figlia
157400150617     c*  ma lna della bolla che sta leggendo in questo momento
157500150617     c                   Z-ADD     tasLna        worg
157600150617     c                   exsr      decorg
157700150617     c                   eval      wtasfl1 =  wofl1
157800000000     c***
157900000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
158000000000     c* . se SI lo aggiorna solo nella sequenza legame
158100000000     c*   se NO lo inserisce
158200000000     c***
158300000000     c                   Z-ADD     waas          kvbaas
158400000000     c                   Z-ADD     wlnp          kvblnp
158500000000     c                   Z-ADD     wnrs          kvbnrs
158600000000     c                   Z-ADD     wnsp          kvbnsp
158700000000     c     keyevb        SETLL     fnevb01l
158800000000     c     keyevb        READE     fnevb01l                               99
158900000000do  1c                   DOW       NOT *in99
159000000000if  2c                   IF        evbatb = *blanks                             *valido
159100140108     c                   MOVEL     evbdev        wdev
159200140108     c                   MOVEL     evbhev        whev
159300080131if  3c                   IF        evbDev > *zeros
159400000000     c                   MOVEL     evbcev        wcev
159500000000     c                   EXSR      deccevsta
159600070109     c                   EXSR      eventoAnnulla
159700000000if  4c                   IF        werr <> '1'                                  *no errore
159800000000     c*
159900000000     c* controlla se evento già inserito
160000000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
160100000000     c                   MOVEL     'S'           newevb                         *nuovo evento
160200000000do  5c     1             DO        200           j
160300000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
160400080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
160500000000     c                   MOVEL     'N'           newevb                         *già inserito
160600000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
160700150618     C                   IF        wtasfl1 = ESTERO
160800150618     c                   Z-ADD     tasLna        worg
160900150618     c                   else
161000150618     c                   Z-ADD     evbfle        worg
161100150618     c                   endif
161200000000     c                   EXSR      decorg
161300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
161400080201     c                   MOVEL     wDev          wdat
161500080201     c                   EXSR      edtdat
161600080201     c                   MOVEL     wdate         smdeve(j)
161700080201     c                   MOVEL     wHev          wora
161800080201     c                   EXSR      edtora
161900080201     c                   MOVEL     worae         smheve(j)
162000130313     c                   MOVEL     dsevbspe      smspe(j)
162100000000     c                   LEAVE                                                  *uscita ciclo
162200000000e   6c                   ENDIF
162300000000e   5c                   ENDDO
162400000000     c*
162500000000     c* incrementa contatore eventi
162600000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
162700000000     c                   ADD       1             tote
162800000000     c* indice schiera
162900000000     c                   Z-ADD     tote          smi(tote)
163000000000     c* sequenza legame
163100000000     c                   Z-ADD     seql          smseql(tote)
163200000000     c* sequenza evento
163300000000     c                   Z-ADD     020           smseqe(tote)
163400000000     c* descrizione
163500030130     c                   MOVEL     wcdex         smdev(tote)
163600000000     c* data
163700080201     c                   MOVEL     wDev          wdat
163800000000     c                   EXSR      edtdat
163900000000     c                   MOVEL     wdate         smdeve(tote)
164000000000     c* ora
164100080201     c                   MOVEL     wHev          wora
164200000000     c                   EXSR      edtora
164300000000     c                   MOVEL     worae         smheve(tote)
164400000000     c* filiale
164500150617     C***                IF        wrkLnaItaEst = ESTERO
164600150617     C                   IF        wtasfl1 = ESTERO
164700140722     c                   Z-ADD     tasLna        worg
164800140722     c                   else
164900140722     c                   Z-ADD     evbfle        worg
165000140722     c                   endif
165100140722     c                   EXSR      decorg
165200140722     c                   MOVEL     wodes         smfle(tote)
165300000000     c* causale
165400000000     c                   MOVEL     evbcev        smcev(tote)
165500130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
165600130312     c* bolla legata
165700130312     c                   MOVEL     dsevbspe      smspe(tote)
165800000000     c* data+ora evento
165900080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
166000000000e   5c                   ENDIF
166100000000e   4c                   ENDIF
166200000000e   3c                   ENDIF
166300000000e   2c                   ENDIF
166400010717     c     keyevb        READE     fnevb01l                               99
166500000000e   1c                   ENDDO
166600000000     c*
166700000000     c                   ENDSR
166800000000     c*--------------------------------------------------------------------------------------------*
166900000000     c* memorizza gli "eventi fissi"
167000000000     c*--------------------------------------------------------------------------------------------*
167100000000     c     memevefix     BEGSR
167200080131     ** ID bolla in canna.
167300080131     c                   EVAL      waas = tasaas
167400080131     c                   EVAL      wlnp = taslnp
167500080131     c                   EVAL      wnrs = tasnrs
167600080131     c                   EVAL      wnsp = tasnsp
167700000000     c*
167800000000     c* Partenza merce
167900000000     c                   EVAL      lduc = tasduc                                *data partenza
168000000000     c                   EVAL      llnp = taslnp                                *filiale partenza
168100000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
168200000000     c                   EXSR      evbpar
168300000000     c* Consegna merce
168400000000     c                   EVAL      lcca = tascca                                *consegna anomala
168500000000     c                   EVAL      ldcm = tasdcm                                *data consegna
168600000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
168700000000     c                   EVAL      llna = taslna                                *linea arrivo
168800150618     c                   EVAL      ltfa = tastfa                                *linea arrivo
168900000000     c                   EXSR      evbcon
169000000000     c*
169100000000     c                   ENDSR
169200000000     c*--------------------------------------------------------------------------------------------*
169300000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
169400000000     c*--------------------------------------------------------------------------------------------*
169500000000     c     elaevelbl     BEGSR
169600000000     c*
169700000000     c* azzera le variabili di lavoro
169800000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
169900000000     c                   EVAL      deplpn = *zeros
170000000000     c                   EVAL      depnrn = *zeros
170100000000     c                   EVAL      depnsn = *zeros
170200000000     c*
170300000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
170400000000     c                   EVAL      kbllpp =  s_taslnp
170500000000     c                   EVAL      kblnrp =  s_tasnrs
170600000000     c                   EVAL      kblnsp =  s_tasnsp
170700000000     c*
170800000000     c* ciclo fino a fine mamme
170900000000     c     ke2lbl        SETLL     fnlbl02l
171000000000     c     ke2lbl        READE     fnlbl02l                               99
171100000000do  1c                   DOW       NOT *in99
171200000000     c*
171300000000     c* ciclo fino a fine figlie della mamma
171400000000do  2c                   DOW       NOT *in99
171500000000     c*
171600000000     c* legge la bolla dal legame
171700000000     c                   EVAL      kasaas = lblaan                              *figlia
171800000000     c                   EVAL      kaslnp = lbllpn
171900000000     c                   EVAL      kasnrs = lblnrn
172000000000     c                   EVAL      kasnsp = lblnsn
172100000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
172200000000if  3c                   IF        NOT *in99 AND                                *esistente
172300000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
172400000000     c                             tastbl <> 'A3' AND
172500000000     c                             tastbl <> 'F3' AND
172600000000     c                             tastbl <> 'AP' AND
172700000000     c                             tastbl <> 'FC'
172800041222     **?Immagine lettera di vettura.
172900041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
173000041222     C                   IF        LDVO74 <> 'N'
173100040330     c                   MOVEL     tasflo        dtasflo
173200040330     c                   IF        §floiml <> *BLANK
173300050325     c                   EVAL      ldvo74 = 'S'
173400040330     C                   EVAL      SpImLDVO74 =
173500040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
173600040330     C                             %EDITC(taslnp:'X') +
173700040330     C                             %EDITC(tasnrs:'X') +
173800040330     C                             %EDITC(tasnsp:'X')
173900041223     **?Reperisco il check code della spedizione POD
174000041223     C                   EVAL      PChkCdeO74 =
174100041223     C                             GetSpeChkCde(
174200041223     C                             %EDITC(tasaas:'X'):
174300041223     C                             %EDITC(taslnp:'X') +
174400041223     C                             %EDITC(tasnrs:'X') +
174500041223     C                             %EDITC(tasnsp:'X'):
174600041223     C                             ChkCode:nEsito)
174700040330     C                   ENDIF
174800041222     C                   ENDIF
174900000000     c* incrementa la sequenza legami per le bolle legate
175000000000     c                   ADD       1             seql                           *legaqme successivo
175100000000     c*---
175200000000     c* memorizza gli "eventi fissi"
175300000000     c*---
175400000000     c                   EXSR      memevefix
175500000000     c*---
175600000000     c* memorizza gli eventi
175700000000     c*---
175800000000     c                   EVAL      waas = lblaan                                *figlia
175900000000     c                   EVAL      wlnp = lbllpn
176000000000     c                   EVAL      wnrs = lblnrn
176100000000     c                   EVAL      wnsp = lblnsn
176200000000     c                   EXSR      memevb
176300000000     c*
176400000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
176500000000     c                   EVAL      depaan = lblaan                              *ultima figlia
176600000000     c                   EVAL      deplpn = lbllpn
176700000000     c                   EVAL      depnrn = lblnrn
176800000000     c                   EVAL      depnsn = lblnsn
176900000000e   3c                   ENDIF
177000000000     c*
177100000000     c* lettura successiva legami
177200000000     c     ke2lbl        READE     fnlbl02l                               99
177300000000e   2c                   ENDDO                                                  *fine figlie mamma
177400000000     c*
177500000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
177600000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
177700000000     c                             kbllpp <> deplpn OR
177800000000     c                             kblnrp <> depnrn OR
177900000000     c                             kblnsp <> depnsn
178000000000     c* nuova chiave
178100000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
178200000000     c                   EVAL      kbllpp =  deplpn
178300000000     c                   EVAL      kblnrp =  depnrn
178400000000     c                   EVAL      kblnsp =  depnsn
178500000000     c* controlla figlie
178600000000     c     ke2lbl        SETLL     fnlbl02l
178700000000     c     ke2lbl        READE     fnlbl02l                               99
178800000000if  3c                   IF        *in99                                        *non ha figlie
178900000000     c                   LEAVE                                                  *esce dal ciclo
179000000000e   3c                   ENDIF
179100000000x   2c                   ELSE                                                   *sempre stessa mamma
179200000000     c                   LEAVE                                                  *esce dal ciclo
179300000000e   2c                   ENDIF
179400000000e   1c                   ENDDO                                                  *fine mamme
179500000000     c*
179600000000     c                   ENDSR
179700160127     c*--------------------------------------------------------------------------------------------*
179800160127     c* Imposta l'evento presa in carico se bolla in partenza
179900160127     c*--------------------------------------------------------------------------------------------*
180000160127     c     evbinca       BEGSR
180100160127     c*
180200160127     c* indice schiera
180300160127     c                   Z-ADD     tote          smi(tote)
180400160127     c* sequenza legame
180500160127     c                   Z-ADD     seql          smseql(tote)
180600160127     c* sequenza evento
180700160127     c                   Z-ADD     008           smseqe(tote)                   *primo evento
180800160127     c* data
180900160127if  1c                   IF        arbdim   > *zeros
181000160127     c                   MOVEL     arbdim        wdat
181100160127     c                   EXSR      edtdat
181200160127     c                   MOVEL     wdate         smdeve(tote)
181300160127e   1c                   ENDIF
181400160127     c* filiale
181500160127     c                   Z-ADD     arblnp        worg
181600160127     c                   EXSR      decorg
181700160127     c
181800160127     c                   MOVEL     wodes         smfle(tote)
181900160127     c* causale
182000160127     c                   MOVEL     wcev          smcev(tote)
182100160127     c* data+ora evento
182200160127     c                   MOVEL     wdat          smdevhev(tote)
182300160127     c                   MOVE      wora          smdevhev(tote)
182400160127     c*
182500160127     c                   ENDSR
182600000000     c*--------------------------------------------------------------------------------------------*
182700000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
182800000000     c*--------------------------------------------------------------------------------------------*
182900000000     c     evbrit        BEGSR
183000160318     c
183100000000     c*
183200000000     c* indice schiera
183300000000     c                   Z-ADD     tote          smi(tote)
183400000000     c* sequenza legame
183500000000     c                   Z-ADD     seql          smseql(tote)
183600000000     c* sequenza evento
183700000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
183800160407     c* data ritiro solo se spuntato
183900160318     c                   if        esito='P'
184000160407     c                   if        arbdse>0
184100160318if  1c                   IF        arbdrt > *zeros
184200160318     c                   MOVEL     arbdrt        wdat
184300160318     c                   EXSR      edtdat
184400160318     c                   MOVEL     wdate         smdeve(tote)
184500160318e   1c                   ENDIF
184600160318     c* ora  da FPP
184700160318if  2c                   IF        arbfpp = 'P'                                 *PM
184800160318     c                   MOVEL     1800          wora
184900160318x   2c                   ELSE                                                   *AM
185000160318     c                   MOVEL     1200          wora
185100160318e   1c                   ENDIF
185200160318     c                   EXSR      edtora
185300160318     c                   MOVEL     worae         smheve(tote)
185400160318     c* filiale
185500160318     c                   Z-ADD     arblnp        worg
185600160318     c                   EXSR      decorg
185700160407
185800160407     c                   endif
185900160407     c
186000160318     c
186100160318     c                   else
186200160318     c
186300000000if  1c                   IF        s_tasdrt > *zeros
186400000000     c                   MOVEL     s_tasdrt      wdat
186500000000     c                   EXSR      edtdat
186600000000     c                   MOVEL     wdate         smdeve(tote)
186700000000e   1c                   ENDIF
186800000000     c* ora
186900000000if  1c                   IF        s_tashrt > *zeros
187000000000     c                   MOVEL     s_tashrt      wora
187100000000x   1c                   ELSE
187200000000if  2c                   IF        s_tasfpp = 'P'                               *PM
187300000000     c                   MOVEL     1800          wora
187400000000x   2c                   ELSE                                                   *AM
187500000000     c                   MOVEL     1200          wora
187600000000e   2c                   ENDIF
187700000000e   1c                   ENDIF
187800000000     c                   EXSR      edtora
187900000000     c                   MOVEL     worae         smheve(tote)
188000000000     c* filiale
188100000000     c                   Z-ADD     s_taslnp      worg
188200000000     c                   EXSR      decorg
188300150618     c
188400150618     c* Se la bolla è import e devo emettere l'evento di telefonare
188500150618     c*  non posso indciare la filiale esterna, ma l'hub italiano
188600150618     c                   if        wofl1=ESTERO and sostrit='S' and
188700150618     c                             §7oinc='T'
188800150618     c                   z-add     s_tastfp      worg
188900150618     c                   EXSR      decorg
189000150618     c                   endif
189100160318     c                   endif
189200160318
189300000000     c                   MOVEL     wodes         smfle(tote)
189400000000     c* causale
189500000000     c                   MOVEL     wcev          smcev(tote)
189600000000     c* data+ora evento
189700000000     c                   MOVEL     wdat          smdevhev(tote)
189800000000     c                   MOVE      wora          smdevhev(tote)
189900000000     c*
190000000000     c                   ENDSR
190100000000     c*--------------------------------------------------------------------------------------------*
190200000000     c* Imposta l'evento Partenza
190300000000     c*--------------------------------------------------------------------------------------------*
190400000000     c     evbpar        BEGSR
190500000000     c*
190600000000     c* controlla SE e COME inserire l'evento
190700000000if  1c                   IF        lduc > *zeros
190800000000     c* imposta causale
190900000000     c                   MOVEL     '702'         wcev
191000000000     c                   EXSR      deccevsta
191100000000if  2c                   IF        werr <> '1'                                  *no errore
191200000000     c* imposta filiale
191300000000     c                   Z-ADD     llnp          worg
191400000000     c                   EXSR      decorg
191500000000     c* imposta data
191600101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
191700000000     c                   MOVEL     ldrt          wdat                           *data ritiro
191800000000x   3c                   ELSE                                                   partenza ITALIA
191900000000     c                   MOVEL     lduc          wdat                           *data partenza
192000000000e   3c                   ENDIF
192100000000     c                   EXSR      edtdat
192200000000     c* imposta ora
192300000000     c                   MOVEL     2100          wora                           *ora imposta
192400000000     c                   EXSR      edtora
192500000000     c*
192600000000     c* controlla se l'evento è già stato inserito
192700000000     c                   MOVEL     'S'           newevb                         *nuovo evento
192800000000do  3c     1             DO        200           j
192900000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
193000000000     c                   MOVE      wora          lavdevhev
193100000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
193200000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
193300000000     c                   MOVEL     'N'           newevb                         *già inserito
193400000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
193500000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
193600000000     c                   LEAVE                                                  *uscita ciclo
193700000000e   4c                   ENDIF
193800000000e   3c                   ENDDO
193900000000     c*
194000000000     c* incrementa contatore eventi
194100000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
194200000000     c                   ADD       1             tote
194300000000     c* indice schiera
194400000000     c                   Z-ADD     tote          smi(tote)
194500000000     c* sequenza legame
194600000000     c                   Z-ADD     seql          smseql(tote)
194700000000     c* sequenza evento
194800000000     c                   Z-ADD     020           smseqe(tote)
194900000000     c* descrizione
195000030130     c                   MOVEL     wcdex         smdev(tote)
195100000000     c* filiale -impostata sopra-
195200000000     c                   MOVEL     wodes         smfle(tote)
195300000000     c* data    -impostata sopra-
195400000000     c                   MOVEL     wdate         smdeve(tote)
195500000000     c* ora     -impostata sopra-
195600000000     c                   MOVEL     worae         smheve(tote)
195700000000     c* causale -impostata sopra-
195800000000     c                   MOVEL     wcev          smcev(tote)
195900000000     c* data+ora evento
196000000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
196100000000     c                   MOVE      wora          smdevhev(tote)
196200000000e   3c                   ENDIF
196300000000e   2c                   ENDIF
196400000000e   1c                   ENDIF
196500000000     c*
196600000000     c                   ENDSR
196700101129     c*------------------------------------------------------------------------*
196800101129     c*
196900101129     c* Imposta l'evento Arrivo
197000101129     c*
197100101129     c*------------------------------------------------------------------------*
197200000000     c     evbarr        BEGSR
197300101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
197400101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
197500101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
197600101129     ** sommato inutile.
197700150618     C                   IF        wrkLnaItaEst = ESTERO
197800150618     C                   LEAVESR
197900150618     C                   ENDIF
198000101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
198100101129     C                   IF        lhti = *ZERO
198200101129     C                   LEAVESR
198300101129     C                   ENDIF
198400000000     c*
198500000000     c* controlla SE e COME inserire l'evento
198600000000if  1c                   IF        ldti > *zeros OR
198700000000     c                             ldcm > *zeros
198800000000     c* imposta causale
198900101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
199000000000     c                   EXSR      deccevsta
199100000000if  2c                   IF        werr <> '1'                                  *no errore
199200000000     c* imposta filiale
199300150617     c                   Z-ADD     llna          worg
199400000000     c                   EXSR      decorg
199500000000     c* imposta data
199600000000if  3c                   IF        ldti > *zeros
199700000000     c                   MOVEL     ldti          wdat                           *data arrivo
199800000000x   3c                   ELSE
199900000000     c                   MOVEL     ldcm          wdat                           *data consegna
200000000000e   3c                   ENDIF
200100000000     c                   EXSR      edtdat
200200000000     c* imposta ora
200300000000if  3c                   IF        lhti > *zeros
200400000000     c                   MOVEL     lhti          wora                           *ora arrivo
200500000000x   3c                   ELSE
200600000000     c                   MOVEL     0600          wora                           *ora imposta
200700000000e   3c                   ENDIF
200800000000     c                   EXSR      edtora
200900000000     c*
201000000000     c* controlla se l'evento è già stato inserito
201100000000     c                   MOVEL     'S'           newevb                         *nuovo evento
201200000000do  3c     1             DO        200           j
201300000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
201400000000     c                   MOVE      wora          lavdevhev
201500000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
201600000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
201700000000     c                   MOVEL     'N'           newevb                         *già inserito
201800000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
201900000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
202000000000     c                   LEAVE                                                  *uscita ciclo
202100000000e   4c                   ENDIF
202200000000e   3c                   ENDDO
202300000000     c*
202400000000     c* incrementa contatore eventi
202500000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
202600000000     c                   ADD       1             tote
202700000000     c* indice schiera
202800000000     c                   Z-ADD     tote          smi(tote)
202900000000     c* sequenza legame
203000000000     c                   Z-ADD     seql          smseql(tote)
203100000000     c* sequenza evento
203200000000     c                   Z-ADD     020           smseqe(tote)
203300000000     c* descrizione
203400030130     c                   MOVEL     wcdex         smdev(tote)
203500000000     c* data    -impostata sopra-
203600000000     c                   MOVEL     wdate         smdeve(tote)
203700000000     c* ora     -impostata sopra-
203800000000     c                   MOVEL     worae         smheve(tote)
203900000000     c* filiale -impostata sopra-
204000000000     c                   MOVEL     wodes         smfle(tote)
204100000000     c* causale -impostata sopra-
204200000000     c                   MOVEL     wcev          smcev(tote)
204300000000     c* data+ora evento
204400000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
204500000000     c                   MOVE      wora          smdevhev(tote)
204600000000e   3c                   ENDIF
204700000000e   2c                   ENDIF
204800000000e   1c                   ENDIF
204900000000     c*
205000000000     c                   ENDSR
205100000000     c*--------------------------------------------------------------------------------------------*
205200000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
205300000000     c*--------------------------------------------------------------------------------------------*
205400140714     c     evbcon        BEGSR
205500000000     c*
205600000000     c* azzera variabili di lavoro
205700000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
205800000000     c****
205900000000     c* Consegna merce ->
206000000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
206100000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
206200000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
206300000000     c*   nè lo stato di consegna nè quello di consegna anomala.
206400000000     c****
206500000000     c* controlla SE e COME inserire l'evento
206600000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
206700060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
206800060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
206900060622     C                             :rpyEsito)
207000060622     C                   IF        rpyOpCode = 'FOUND'
207100041222     **?Consegna anomala, POD non visualizzabile.
207200060622     C                   IF        §7OPODImg = 'N'
207300041222     C                   EVAL      LDVO74 = 'N'
207400041222     C                   ENDIF
207500060622     c                   IF        §7oinc <> *blanks                            *ok per internet
207600060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
207700000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
207800000000x   3c                   ELSE                                                   *ha record FNEVB
207900000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
208000000000e   3c                   ENDIF
208100041222e   3c                   ENDIF
208200000000e   2c                   ENDIF
208300000000e   1c                   ENDIF
208400080131     c* imposta data
208500080131if  1c                   IF        ldcm > *zeros
208600080131     c                   MOVEL     ldcm          wdat
208700080131e   1c                   ENDIF
208800080131     c* imposta ora
208900080131if  1c                   IF        lhmc > *zeros
209000080131     c                   MOVEL     lhmc          wora
209100080131e   1c                   ENDIF
209200000000     c* imposta causale
209300000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
209400060622     c                   MOVEL     §7ocev        wcev                           *causale evento
209500080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
209600080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
209700080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
209800080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
209900080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
210000080131     c* ritiro merce) dalla bolla figlia.
210100080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
210200080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
210300080131     c                             AND s_tasNdc < *HIVAL
210400080131     c                   EVAL      kblAap = wAas
210500080131     c                   EVAL      kblLpp = wLnp
210600080131     c                   EVAL      kblNrp = wNrs
210700080131     c                   EVAL      kblNsp = wNsp
210800080131     c     ke2lbl        CHAIN     fnlbl02l
210900080131     c                   IF        %FOUND()
211000080131     c                   EVAL      kasAas = lblAan
211100080131     c                   EVAL      kasLnp = lblLpn
211200080131     c                   EVAL      kasNrs = lblNrn
211300080131     c                   EVAL      kasNsp = lblNsn
211400080131     C     keyTas        CHAIN     titas000      titas000Figlia
211500080131     C                   IF        %FOUND()
211600080131     C                   EVAL      wDat = titas000Figlia.tasDrt
211700080131     C                   EVAL      wOra = titas000Figlia.tasHrt
211800080131     C                   ELSE
211900080131     C     keyTas        CHAIN     titas010      titas010Figlia
212000080131     C                   IF        %FOUND()
212100080131     C                   EVAL      wDat = titas010Figlia.tasDrt
212200080131     C                   EVAL      wOra = titas010Figlia.tasHrt
212300080131     C                   ELSE
212400080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
212500080131     C                   IF        %FOUND()
212600080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
212700080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
212800080131     C                   ENDIF
212900080131     C                   ENDIF
213000080131     C                   ENDIF
213100080131     c                   ENDIF
213200080131     c                   ENDIF
213300000000x   1c                   ELSE                                                   *NO sost.Consegna
213400000000     c                   MOVEL     '704'         wcev                           *causale evento
213500000000e   1c                   ENDIF
213600080131     c* Editazione data e ora evento.
213700080131     c                   EXSR      edtdat
213800080131     c                   EXSR      edtora
213900000000     c* imposta filiale
214000000000     c                   Z-ADD     llna          worg
214100000000     c                   EXSR      decorg
214200150618     c* se si tratta di lna estera e la bolla ha consegna anomala
214300150618     c*  in cui mettere msg generico, allora devo sostituire la filiale
214400150618     c*  con il suo hub
214500150618     c                   if        wofl1=estero and lcca<>' ' and
214600150618     c                             §7oinc='T'
214700150618     c                   Z-ADD     ltfa          worg
214800150618     c                   EXSR      decorg
214900150618     c                   endif
215000150618     c
215100000000     c* controlla se l'evento è già stato inserito
215200000000     c                   MOVEL     'S'           newevb                         *nuovo evento
215300000000do  1c     1             DO        200           j
215400000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
215500000000     c                   MOVE      wora          lavdevhev
215600000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
215700000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
215800000000     c                   MOVEL     'N'           newevb                         *già inserito
215900000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
216000000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
216100000000     c                   LEAVE                                                  *uscita ciclo
216200000000e   2c                   ENDIF
216300000000e   1c                   ENDDO
216400000000     c*
216500000000     c* incrementa contatore eventi
216600030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
216700030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
216800000000     c                   ADD       1             tote                           incrementa contatore
216900060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
217000060622     c                   MOVEL     §7odei        smdev(tote)
217100030203x   4c                   ELSE                                                   *evento telef.P.O.
217200060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
217300030203e   4c                   ENDIF
217400060622     c                   MOVEL     §7ocev        wcev                           *causale evento
217500000000     c                   EXSR      evbcon2                                      *imposta evento
217600030203x   3c                   ELSE                                                   *NO sost.Consegna
217700000000     c*
217800030203if  4c                   IF        ldcm > *zeros
217900030203if  5c                   IF        sostcon <> ' '                               *considera consegna
218000000000     c                   EXSR      deccevsta
218100030203if  6c                   IF        werr <> '1'                                  *no errore
218200000000     c                   ADD       1             tote                           incrementa contatore
218300030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
218400000000     c                   EXSR      evbcon2                                      *imposta evento
218500030203e   6c                   ENDIF
218600030203e   5c                   ENDIF
218700030203e   4c                   ENDIF
218800030203e   3c                   ENDIF
218900000000     c*
219000030203e   1c                   ENDIF
219100030203     c*
219200000000     c                   ENDSR
219300000000     c*--------------------------------------------------------------------------------------------*
219400000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
219500000000     c*--------------------------------------------------------------------------------------------*
219600000000     c     evbcon2       BEGSR
219700000000     c*
219800000000     c* indice schiera
219900000000     c                   Z-ADD     tote          smi(tote)
220000000000     c* sequenza legame
220100000000     c                   Z-ADD     seql          smseql(tote)
220200000000     c* sequenza evento
220300000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
220400000000     c* data    -impostata sopra-
220500000000     c                   MOVEL     wdate         smdeve(tote)
220600000000     c* ora     -impostata sopra-
220700000000     c                   MOVEL     worae         smheve(tote)
220800000000     c* filiale -impostata sopra-
220900000000     c                   MOVEL     wodes         smfle(tote)
221000000000     c* causale -impostata sopra-
221100000000     c                   MOVEL     wcev          smcev(tote)
221200000000     c* data+ora evento
221300000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
221400000000     c                   MOVE      wora          smdevhev(tote)
221500000000     c*
221600000000     c                   ENDSR
221700000000     c*--------------------------------------------------------------------------------------------*
221800000000     c* memorizza i dati della lettera di anomalia
221900000000     c*--------------------------------------------------------------------------------------------*
222000000000     c     memdct        BEGSR
222100000000     c*
222200040906     C                   RESET                   FIDN12DS
222300040906     c                   Z-ADD     waas          I12AAS
222400040906     c                   Z-ADD     wlnp          I12LNP
222500040906     c                   Z-ADD     wnrs          I12NRS
222600040906     c                   Z-ADD     wnsp          I12NSP
222700040906     C                   CALL      'FIDN12R'
222800040906     C                   PARM                    FIDN12DS
222900000000     c* memorizza la prima lettera di anomalia valida
223000040906do  1c                   IF        O12Err = *BLANK
223100040906     C                   DO        O12NCA        K
223200040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
223300040906     C                   EVAL      DCTAAC = O12KeyAAC
223400040906     C                   EVAL      DCTFIL = O12KeyFil
223500040906     C                   EVAL      DCTNCA = O12KeyNCA
223600040906     c     keydct        CHAIN     fndct01l
223700040906do  1c                   IF        %FOUND
223800000000     c*
223900000000     c* controlla validità CA
224000000000     c                   MOVEL     '0'           werr                           *NO errore
224100000000     c                   EXSR      chkdct
224200000000if  2c                   IF        werr = '0'                                   *CA valida
224300000000     c*
224400000000     c* Imposta l'evento lettera di anomalia
224500000000     c                   EXSR      evbdct
224600000000     C                   LEAVE                                                  *uscita ciclo
224700000000e   2c                   ENDIF
224800000000     c*
224900040906e   1c                   ENDIF
225000040906e   1c                   ENDDO
225100040906e   1c                   ENDIF
225200000000     C*
225300000000     c                   ENDSR
225400000000     c*--------------------------------------------------------------------------------------------*
225500000000     c* controlla validità lettera di anomalia
225600000000     c*--------------------------------------------------------------------------------------------*
225700000000     c     chkdct        BEGSR
225800000000     c*
225900000000     c* reimposta variabili di lavoro
226000000000     c                   MOVEL     '0'           werr                           *NO errore
226100000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
226200000000     c*
226300000000     c* CA annullata, esclude
226400000000if  1c                   IF        dctatb <> ' '
226500000000     c                   MOVEL     '1'           werr                           *SI errore
226600000000e   1C                   ENDIF
226700000000     c*
226800000000     c* CA non confermata, esclude
226900000000if  1c                   IF        dctfpr = ' '
227000000000     c                   MOVEL     '1'           werr                           *SI errore
227100000000e   1C                   ENDIF
227200000000     c*
227300000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
227400000000if  1c                   IF        dctfpr = 'T' AND
227500000000     c                             dctfca >= 215 OR
227600000000     c                             dctfpr = 'P' AND
227700000000     c                             dctfca >= 400
227800000000x   1c                   ELSE
227900000000     c                   MOVEL     '1'           werr                           *SI errore
228000000000e   1C                   ENDIF
228100000000     c*
228200000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
228300000000if  1c                   IF        werr = '0'
228400000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
228500060622     C                   RESET                   tibs02ds
228600060622     C                   EVAL      t02Cod = 'CCH'
228700060622     C                   EVAL      t02Ke1 = dctcch
228800060622     C                   EXSR      getTntbe00f
228900060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
229000000000     c                   MOVEL     '1'           werr                           *SI errore
229100000000e   3C                   ENDIF
229200000000e   2C                   ENDIF
229300000000e   1C                   ENDIF
229400000000     c*
229500000000     c                   ENDSR
229600000000     c*--------------------------------------------------------------------------------------------*
229700000000     c* Imposta l'evento lettera di anomalia
229800000000     c*--------------------------------------------------------------------------------------------*
229900000000     c     evbdct        BEGSR
230000000000     C*
230100000000     c* imposta causale
230200000000if  1c                   IF        wfpr = 'T'
230300000000     c                   MOVEL     '709'         wcev                           *causale evento
230400000000x   1c                   ELSE
230500000000     c                   MOVEL     '710'         wcev                           *causale evento
230600000000e   1c                   ENDIF
230700000000     c                   EXSR      deccevsta                                    *decodifica evento
230800000000     c*
230900000000     c* imposta filiale
231000000000if  1c                   IF        werr <> '1'                                  *no errore
231100000000     c                   Z-ADD     dctfil        worg
231200000000     c                   EXSR      decorg
231300000000     c* imposta data
231400000000     c                   MOVEL     dctaac        wdat
231500000000     c                   MOVE      dctmgc        wdat
231600000000     c                   EXSR      edtdat
231700000000     c* imposta ora
231800000000     c                   MOVEL     1100          wora                           *FISSA
231900000000     c                   EXSR      edtora
232000000000     c*
232100000000     c* incrementa contatore eventi
232200000000     c                   ADD       1             tote
232300000000     c* indice schiera
232400000000     c                   Z-ADD     tote          smi(tote)
232500000000     c* sequenza legame
232600000000     c                   Z-ADD     seql          smseql(tote)
232700000000     c* sequenza evento
232800000000     c                   Z-ADD     999           smseqe(tote)
232900000000     c* descrizione -impostata sopra-
233000030130     c                   MOVEL     wcdex         smdev(tote)
233100000000     c* filiale -impostata sopra-
233200000000     c                   MOVEL     wodes         smfle(tote)
233300000000     c* data    -impostata sopra-
233400000000     c                   MOVEL     wdate         smdeve(tote)
233500000000     c* ora     -impostata sopra-
233600000000     c                   MOVEL     worae         smheve(tote)
233700000000     c* causale -impostata sopra-
233800000000     c                   MOVEL     wcev          smcev(tote)
233900000000     c* data+ora evento
234000000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
234100000000     c                   MOVE      wora          smdevhev(tote)
234200000000e   1c                   ENDIF
234300000000     c*
234400000000     c                   ENDSR
234500000000     c*--------------------------------------------------------------------------------------------*
234600000000     c* imposta i campi della DS di Output dal contrassegno
234700000000     c*--------------------------------------------------------------------------------------------*
234800000000     c     impdsocsb     BEGSR
234900000000     c*
235000000000     c* azzera le variabili di lavoro
235100000000     c*
235200000000     c****
235300000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
235400000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
235500000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
235600000000     c****
235700000000     c*
235800000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
235900090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
236000000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
236100000000x   1c                   ELSE                                                     *trovato
236200000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
236300000000e   1C                   ENDIF
236400000000     c*---
236500000000     c* memorizza il contrassegno della bolla di Input
236600000000     c*---
236700000000     c                   EVAL      waas = s_tasaas                              *bolla Input
236800000000     c                   EVAL      wlnp = s_taslnp
236900000000     c                   EVAL      wnrs = s_tasnrs
237000000000     c                   EVAL      wnsp = s_tasnsp
237100000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
237200000000     c                   EXSR      memcsb
237300000000     c*
237400000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
237500000000     c                   EXSR      elacsblbl
237600000000     c*
237700000000     c                   ENDSR
237800000000     c*--------------------------------------------------------------------------------------------*
237900000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
238000000000     c*--------------------------------------------------------------------------------------------*
238100000000     c     elacsblbl     BEGSR
238200000000     c*
238300000000     c* azzera le variabili di lavoro
238400000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
238500000000     c                   EVAL      deplpn = *zeros
238600000000     c                   EVAL      depnrn = *zeros
238700000000     c                   EVAL      depnsn = *zeros
238800000000     c*
238900000000     c* legge i legami delle figlie successive alla bolla di input
239000000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
239100000000     c                   EVAL      kbllpp =  s_taslnp
239200000000     c                   EVAL      kblnrp =  s_tasnrs
239300000000     c                   EVAL      kblnsp =  s_tasnsp
239400000000     c*
239500000000     c* ciclo fino a fine mamme
239600000000     c     ke2lbl        SETLL     fnlbl02l
239700000000     c     ke2lbl        READE     fnlbl02l                               99
239800000000do  1c                   DOW       NOT *in99
239900000000     c*
240000000000     c* ciclo fino a fine figlie della mamma
240100000000do  2c                   DOW       NOT *in99
240200000000     c*
240300000000     c* legge la bolla dal legame
240400000000     c                   EVAL      kasaas = lblaan                              *figlia
240500000000     c                   EVAL      kaslnp = lbllpn
240600000000     c                   EVAL      kasnrs = lblnrn
240700000000     c                   EVAL      kasnsp = lblnsn
240800000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
240900000000     c*---
241000000000     c* memorizza il contrassegno della bolla figlia legata
241100000000     c*---
241200000000if  4c                   IF        NOT *in99                                    *esistente
241300000000     c                   EVAL      waas = lblaan                                *figlia
241400000000     c                   EVAL      wlnp = lbllpn
241500000000     c                   EVAL      wnrs = lblnrn
241600000000     c                   EVAL      wnsp = lblnsn
241700000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
241800000000     c                   EXSR      memcsb
241900000000e   4c                   ENDIF
242000000000     c*
242100000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
242200000000     c                   EVAL      depaan = lblaan                              *ultima figlia
242300000000     c                   EVAL      deplpn = lbllpn
242400000000     c                   EVAL      depnrn = lblnrn
242500000000     c                   EVAL      depnsn = lblnsn
242600000000     c*
242700000000     c* lettura successiva legami
242800000000     c     ke2lbl        READE     fnlbl02l                               99
242900000000e   2c                   ENDDO                                                  *fine figlie mamma
243000000000     c*
243100000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
243200000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
243300000000     c                             kbllpp <> deplpn OR
243400000000     c                             kblnrp <> depnrn OR
243500000000     c                             kblnsp <> depnsn
243600000000     c* nuova chiave
243700000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
243800000000     c                   EVAL      kbllpp =  deplpn
243900000000     c                   EVAL      kblnrp =  depnrn
244000000000     c                   EVAL      kblnsp =  depnsn
244100000000     c* controlla figlie
244200000000     c     ke2lbl        SETLL     fnlbl02l
244300000000     c     ke2lbl        READE     fnlbl02l                               99
244400000000if  3c                   IF        *in99                                        *non ha figlie
244500000000     c                   LEAVE                                                  *esce dal ciclo
244600000000e   3c                   ENDIF
244700000000x   2c                   ELSE                                                   *sempre stessa mamma
244800000000     c                   LEAVE                                                  *esce dal ciclo
244900000000e   2c                   ENDIF
245000000000e   1c                   ENDDO                                                  *fine mamme
245100000000     c*
245200000000     c                   ENDSR
245300000000     c*--------------------------------------------------------------------------------------------*
245400000000     c* memorizza il contrassegno
245500000000     c*--------------------------------------------------------------------------------------------*
245600000000     c     memcsb        BEGSR
245700000000     c*
245800000000     c* legge il contrassegno, se avuto
245900000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
246000000000     c                   Z-ADD     waas          ksbaas
246100000000     c                   Z-ADD     wlnp          ksblnp
246200000000     c                   Z-ADD     wnrs          ksbnrs
246300000000     c                   Z-ADD     wnsp          ksbnsp
246400000000     c     keycsb        CHAIN     tncsb03l                           99
246500000000if  2c                   IF        NOT *in99                                    *esistente
246600000000     c* memorizza i dati
246700000000     c                   MOVEL     'S'           cao74                          *esiste C/A
246800000000     c*
246900000000     c                   Z-ADD     csbcas        caimpo74                       *importo
247000000000     c*
247100000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
247200010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
247300010820     c                             caimpo74 > *zeros
247400010820     c                   EVAL      cadivo74 = 'ITL'
247500010820e   3c                   ENDIF
247600160314
247700160314     ** Visualizzo la descrizione del tipo incasso
247800100121     C                   EVAL      cainco74 =
247900100121     C                                       GetDescrizioneIncassoContrassegno()
248000000000     c*
248100000000     c                   MOVEL     *blanks       castao74                       *stato
248200060622     C                   EVALR     tblKey = %CHAR(csbSta)
248300060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
248400060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
248500060622     C                             :rpyOpCode:rpyEsito)
248600060622     C                   IF        rpyOpCode = 'FOUND'
248700060622     c                   EVAL      castao74 = §4SDei
248800000000e   3c                   ENDIF
248900100119     c
249000000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
249100011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
249200060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
249300000000x   4c                   ELSE
249400000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
249500060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
249600000000x   5c                   ELSE
249700060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
249800000000e   5c                   ENDIF
249900000000e   4c                   ENDIF
250000000000e   3c                   ENDIF
250100000000     c*
250200000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
250300000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
250400000000if  4c                   IF        csbtpa = 'B'
250500060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
250600000000e   4c                   ENDIF
250700000000if  4c                   IF        csbtpa = 'C'
250800060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
250900000000e   4c                   ENDIF
251000000000x   3c                   ELSE                                                   *incasso Bartolini
251100000000if  4c                   IF        csbfpc = *blanks
251200110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
251300000000e   4c                   ENDIF
251400000000if  4c                   IF        csbfpc = '2'
251500060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
251600000000e   4c                   ENDIF
251700000000e   3c                   ENDIF
251800000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
251900000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
252000000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
252100000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
252200000000     c                             csbfpc = *blanks                              assegno di traenza
252300060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
252400000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
252500060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
252600000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
252700060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
252800000000e   4c                   ENDIF
252900000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
253000060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
253100060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
253200000000e   3c                   ENDIF
253300000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
253400000000     c                   Z-ADD     *zeros        caabio74                       *abi
253500000000     c                   Z-ADD     *zeros        cacabo74                       *cab
253600000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
253700000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
253800000000     c                   MOVEL     csbddp        wdat
253900000000     c                   EXSR      edtdat
254000000000     c                   EVAL      cadato74 = wdate
254100000000     C*
254200000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
254300000000     c                   Z-ADD     csbabi        caabio74
254400000000     c                   Z-ADD     csbcab        cacabo74
254500000000     c                   MOVEL     csbnra        canumo74
254600120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
254700120321     c                             : caCabO74)
254800000000x   4c                   ELSE                                                   *incasso Bartolini
254900000000     c                   MOVEL     csbndp        canumo74
255000000000e   4c                   ENDIF
255100000000e   3c                   ENDIF
255200110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
255300110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
255400110307     C                   IF        IsContrassegnoCompensato()
255500110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
255600110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
255700110304     C                   CLEAR                   caabio74
255800110304     C                   CLEAR                   cacabo74
255900110304     C                   CLEAR                   canumo74
256000110304     C                   ENDIF
256100110304     C*
256200000000e   2c                   ENDIF
256300000000e   1c                   ENDIF
256400000000     c*
256500000000     c                   ENDSR
256600000000     c*--------------------------------------------------------------------------------------------*
256700000000     c* imposta i campi della DS di Output dalla giacenza
256800000000     c*--------------------------------------------------------------------------------------------*
256900000000     c     impdsogcp     BEGSR
257000000000     c*
257100000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
257200000000     c                   EVAL      waas = s_tasaas                              *bolla Input
257300000000     c                   EVAL      wlnp = s_taslnp
257400000000     c                   EVAL      wnrs = s_tasnrs
257500000000     c                   EVAL      wnsp = s_tasnsp
257600000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
257700000000     c                   EXSR      memgcp
257800000000     c*
257900000000     c                   ENDSR
258000000000     c*--------------------------------------------------------------------------------------------*
258100000000     c* memorizza la giacenza
258200000000     c*--------------------------------------------------------------------------------------------*
258300000000     c     memgcp        BEGSR
258400000000     c*
258500000000     c* imposta le variabili di lavoro
258600000000     c                   CLEAR                   i                              *indice
258700000000     c                   CLEAR                   ii
258800000000     c                   CLEAR                   smdmc                          *note fase -20-
258900000000     c                   CLEAR                   smdmcR
259000000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
259100000000     c                   MOVEL     'N'           gco74                          *flag di controllo
259200000000     c                   MOVEL     'N'           gcfdio74
259300000000     c                   MOVEL     'N'           gcmito74
259400000000     c*
259500000000     c* legge la giacenza, se avuta
259600000000     c                   Z-ADD     waas          kcpaas
259700000000     c                   Z-ADD     wlnp          kcplnp
259800000000     c                   Z-ADD     wnrs          kcpnrs
259900000000     c                   Z-ADD     wnsp          kcpnsp
260000000000     c                   Z-ADD     *zeros        kcpfrg
260100050309     c     keygcp        CHAIN     tigcp51l                           99
260200050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
260300050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
260400000000if  2c                   IF        NOT *in99        AND                         *esistente
260500000000     c                             gcpatb = *blanks AND                         *no annullata
260600050415     c                             gcptcm <> 'Z'    AND
260700050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
260800050419     C
260900050419     C                   EVAL      gcRiapO74 = gcpRiap
261000050418     **?Descrizione della fase.
261100050418     C                   SELECT
261200050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
261300060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
261400050418     C                   WHEN      gcpFas = 20
261500060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
261600050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
261700060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
261800050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
261900060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
262000050418     C                   ENDSL
262100000000     c* memorizza i dati dalle NOTE GIACENZE
262200000000     c                   Z-ADD     *zeros        i
262300000000     c                   Z-ADD     *zeros        ii
262400000000     c                   Z-ADD     gcpagc        knpagc
262500000000     c                   Z-ADD     gcpfgc        knpfgc
262600000000     c                   Z-ADD     gcpngc        knpngc
262700000000     c                   Z-ADD     gcpfrg        knpfrg
262800050309     c     keygnp        SETLL     tignp01l
262900050309     c     keygnp        READE     tignp01l                               98
263000000000do  3c                   DOW       NOT *in98
263100050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
263200000000     c* fase -10-
263300000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
263400000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
263500000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
263600000000e   5c                   ENDIF
263700000000     c* fase -20-
263800000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
263900000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
264000051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
264100000000     c                   ADD       1             i
264200000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
264300000000e   6c                   ENDIF
264400000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
264500051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
264600050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
264700050405     c                   CLEAR                   ii
264800050405     c                   CLEAR                   smdmcR
264900050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
265000050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
265100050919     C                   IF        NOT %ERROR
265200140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
265300050919     C                   ENDIF
265400050405     C                   ELSE
265500000000     c                   ADD       1             ii
265600000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
265700050405     C                   ENDIF
265800000000e   6c                   ENDIF
265900000000e   5c                   ENDIF
266000000000e   4c                   ENDIF
266100050309     c     keygnp        READE     tignp01l                               98
266200000000e   3c                   ENDDO
266300000000     c*
266400000000     c* memorizza i dati dalle GIACENZE
266500000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
266600050404     c* Disposizione da immettere.
266700051110     c                   IF        gcpfas = 20
266800051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
266900051110     C                   EXSR      chkTivpi
267000051110e   3c                   ENDIF
267100000000     c*
267200000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
267300000000     c*
267400000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
267500000000     c*
267600060103     c                   Z-ADD     gcpagc        dsannB
267700060103     c                   Z-ADD     gcpmgc        dsmgsB
267800060103     c                   Z-ADD     dsdatB        wdat
267900000000if  3c                   IF        wdat > *zeros
268000000000     c                   EXSR      edtdat
268100000000     c                   EVAL      gcdago74 = wdate                             *data apertura
268200000000e   3C                   ENDIF
268300060104     c*
268400060104     c                   Z-ADD     gcpdur        wdat
268500060104     c                   IF        wdat > *zeros
268600060104     c                   EXSR      edtdat
268700060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
268800060104     C                   ENDIF
268900000000     c*
269000000000if  3c                   IF        gcpcmc <> *blanks
269100000000     c                   MOVEL     gcpcmc        wcev
269200000000     c                   EXSR      deccevgia
269300030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
269400000000e   3C                   ENDIF
269500000000     c*
269600000000     c                   MOVEL     *blanks       gcdcgo74
269700000000if  3c                   IF        gcpdcg > *zeros
269800000000     c                   Z-ADD     gcpdcg        wdat
269900000000     c                   EXSR      edtdat
270000000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
270100000000e   3c                   ENDIF
270200000000     c*---
270300000000     c* Disposizioni già immesse
270400000000     c*---
270500000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
270600000000     c*
270700000000if  4c                   IF        gcpddm > *zeros
270800000000     c                   Z-ADD     gcpddm        wdat
270900000000     c                   EXSR      edtdat
271000000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
271100000000e   4C                   ENDIF
271200000000     c*
271300000000if  4c                   IF        gcpdis <> *blanks
271400060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
271500060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
271600060626     C                             :rpyOpCode:rpyEsito)
271700060622     C                   EVAL      gcdiso74 = §2DDes
271800000000e   4C                   ENDIF
271900000000     c*
272000000000if  4c                   IF        gcpasg <> *blanks
272100000000if  5c                   IF        gcpasg = 'S'
272200060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
272300000000x   5c                   ELSE
272400060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
272500000000e   5C                   ENDIF
272600000000     c*
272700000000if  5c                   IF        gcppsg = 'M'
272800060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
272900000000e   5C                   ENDIF
273000000000if  5c                   IF        gcppsg = 'D'
273100060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
273200000000e   5C                   ENDIF
273300000000e   4C                   ENDIF
273400000000     c*
273500000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
273600000000     c                   MOVEL     gcpvcs        gcvcso74
273700000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
273800000000     c                   Z-ADD     gcpcas        gccaso74
273900000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
274000000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
274100000000e   5C                   ENDIF
274200000000e   4C                   ENDIF
274300050404     C                   IF        smdmc(1) <> *BLANK
274400050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
274500050404     C                             smdmc(3) + smdmc(4)
274600051014     C                   EVAL      gcdmc5O74 = smdmc(5)
274700050404     C                   ELSE
274800050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
274900050404     C                             smdmcr(3) + smdmcr(4)
275000051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
275100050404e   3C                   ENDIF
275200000000e   3C                   ENDIF
275300000000     c*---
275400000000     c* Disposizioni da immettere
275500000000     c*---
275600010719     c* se le disposizioni sono da immettere controlla che:
275700010719     c* - il cliente è il mittente della giacenza
275800000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
275900040726if  4C                   IF        keygiac <> *blanks
276000040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
276100040726x   4c                   ELSE
276200090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
276300090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
276400090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
276500090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
276600090914     C                             )
276700040726e   4C                   ENDIF
276800000000e   3C                   ENDIF
276900050404     C* Controllo esistenza storia giacenza.
277000050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
277100050404     C                   DO        *HIVAL
277200050404     C     K04GCP51      READE     TIGCP51L
277300050404     C                   IF        %EOF
277400050404     C                   LEAVE
277500050404     C                   ENDIF
277600050404     C                   IF        GCPATB = *BLANK
277700050404     C                   EVAL      StoGiaO74 = 'S'
277800050404     C                   LEAVE
277900050404     C                   ENDIF
278000050404     C                   ENDDO
278100050404     c*
278200000000e   2c                   ENDIF
278300000000     c*
278400000000     c                   ENDSR
278500000000     c*--------------------------------------------------------------------------------------------*
278600000000     c* imposta i campi della DS di Output dalle note
278700000000     c*--------------------------------------------------------------------------------------------*
278800000000     c     impdsonot     BEGSR
278900000000     c*
279000000000     c* azzera le variabili di lavoro
279100000000     c                   CLEAR                   i
279200000000     c                   CLEAR                   mdcre                          consegna richiesta
279300000000     c                   CLEAR                   mhcre
279400030210     c                   CLEAR                   mtcre
279500000000     c                   CLEAR                   mftce                          consegna particolare
279600000000     c                   CLEAR                   wftc
279700000000     c                   CLEAR                   d1wdftc
279800000000     c                   CLEAR                   d2wdftc
279900000000     c                   CLEAR                   mgcxe                          turni chiusura
280000000000     c                   CLEAR                   wgcx
280100000000     c                   CLEAR                   d1wdgcx
280200000000     c                   CLEAR                   d2wdgcx
280300010927     c                   CLEAR                   mffd                           fermo deposito
280400010927     c                   CLEAR                   mmncB                          *motivi NON consegna
280500010927     c                   CLEAR                   mmncC
280600010927     c                   CLEAR                   mmncG
280700010927     c                   CLEAR                   mmncH
280800010927     c                   CLEAR                   mmncN
280900010927     c                   CLEAR                   mmncO
281000050701     c                   CLEAR                   mmnc8
281100050701     c                   CLEAR                   mmnc9
281200050701     c                   CLEAR                   a230
281300000000     c                   CLEAR                   we
281400030206     c                   CLEAR                   mtel                           n° tel destinatario
281500030206     c                   CLEAR                   mban                           bancali
281600030207     c                   CLEAR                   mscr                           supermercati
281700030210     c                   CLEAR                   msdace
281800030210     c                   CLEAR                   msorce
281900030210     c                   CLEAR                   msdcre
282000030210     c                   CLEAR                   mshcre
282100030210     c                   CLEAR                   mstcre
282200030210     c                   CLEAR                   msnom
282300030210     c                   CLEAR                   mscrT
282400030210     c                   CLEAR                   wmscrT
282500030210     c                   CLEAR                   s
282600030207     c
282700160127     c*imposta dati data consegna richiesta  DA BLP se bolla in partenza
282800160127     c                   if        esito='P'
282900160127     c                   eval      f_tasdcr=arbdcr
283000160127     c                   eval      f_tastcr=arbtcr
283100160127     c                   eval      f_tashcr=arbhcr
283200160127     c* imposto anche key spedizione
283300160309     c                   eval      f_tasaas=arbaas
283400160309     c                   eval      f_taslnp=arblnp
283500160309     c                   eval      f_tasnrs=arbnrs
283600160309     c                   eval      f_tasnsp=arbnsp
283700160127     c                   endif
283800160127     c
283900140716     c                   if        f_tasdcr > 0
284000140716     c                   MOVEL     f_tasdcr      wdat
284100140619     c                   EXSR      edtdat
284200140619     c                   eval      DTCONRCO74  =  wdate
284300140819if  3c                   IF        f_tastcr = ' '
284400140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
284500140819e   3c                   ENDIF
284600140819     c                   endif
284700140716     c                   if        f_tashcr > 0
284800140716     c                   MOVEL     f_tashcr      wora
284900140619     c                   EXSR      edtora
285000140619     c                   eval      ORACONRO74  =  worae
285100140619     c                   endif
285200140716     c                   eval      TPDATCRO74  =  f_tastcr
285300140619      *reperimento descrizione tipo consegna richiesta
285400140716if  3c                   IF        f_tastcr = 'P'
285500140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
285600140619e   3c                   ENDIF
285700140716if  3c                   IF        f_tastcr = 'D'
285800140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
285900140619e   3c                   ENDIF
286000000000     c*---
286100000000     c* memorizza le "note" che sono presenti nel record bolla
286200000000     c*---
286300000000     c* Consegna richiesta
286400140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
286500140619     c*m                           s_tashcr = *zeros
286600140619x   1c*m                 ELSE                                                   *SI cons.particolare
286700140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
286800140619     c*m                 MOVEL     s_tasdcr      wdat
286900140619     c*m                 EXSR      edtdat                                       *edita data
287000140619     c*m                 MOVEL     wdate         mdcre
287100140619e   2c*m                 ENDIF
287200140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
287300140619     c*m                 MOVEL     s_tashcr      wora
287400140619     c*m                 EXSR      edtora                                       *edita ora
287500140619     c*m                 MOVEL     worae         mhcre
287600140619e   2c*m                 ENDIF
287700140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
287800140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
287900140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
288000140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
288100140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
288200140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
288300140619e   3c*m                 ENDIF
288400140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
288500140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
288600140619e   3c*m                 ENDIF
288700140619if  3c*m                 IF        s_tastcr = ' '                               *il
288800140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
288900140619e   3c*m                 ENDIF
289000140619e   2c*m                 ENDIF
289100140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
289200140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
289300140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
289400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
289500140619e   3c*m                 ENDIF
289600140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
289700140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
289800140619e   3c*m                 ENDIF
289900140619if  3c*m                 IF        s_tastcr = ' '                               *il
290000140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
290100140619e   3c*m                 ENDIF
290200140619e   2c*m                 ENDIF
290300140619     c*m
290400140619if  2c*m                 IF        i < 10                                       *se c'è spazio
290500140619     c*m                 ADD       1             i                              *memorizza nota
290600140619     c*m                 EVAL      smnot(i) =
290700140619     c*m                           %TRIM(mtcre) +
290800140619     c*m                           ' '          +
290900140619     c*m                           %TRIM(mdcre) +
291000140619     c*m                           ' '          +
291100140619     c*m                           %TRIM(mhcre)
291200140619e   2c*m                 ENDIF
291300140619e   1c*m                 ENDIF
291400000000     c*
291500000000     c* Particolarità consegna -uno/due-
291600160127     c                   if        esito='P'
291700160309     c                   eval      f_tasftc = arbtc1
291800160314     c                   eval      f_tastc2 = arbtc2
291900160127     c                   endif
292000160127     c
292100140716if  1c                   IF        f_tasftc <> *blanks OR
292200140716     c                             f_tastc2 <> *blanks
292300140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
292400000000     c                   EXSR      decftc
292500000000     c                   MOVEL     wdftc         d1wdftc
292600140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
292700000000     c                   EXSR      decftc
292800000000     c                   MOVEL     wdftc         d2wdftc
292900000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
293000000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
293100060612     c                   EVAL      we = ' ' +
293200060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
293300000000x   2c                   ELSE
293400000000     c                   EVAL      we = *blanks
293500000000e   2c                   ENDIF
293600060612     c                   EVAL      mftce =
293700060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
293800060612     c                             %TRIM(d1wdftc)             +
293900060612     c                             we                         +
294000060612     c                             %TRIM(d2wdftc)
294100000000     c*
294200010927if  2c                   IF        i < 10                                       *se c'è spazio
294300000000     c                   ADD       1             i                              *memorizza nota
294400000000     c                   EVAL      smnot(i) = mftce
294500010927e   2c                   ENDIF
294600010927e   1c                   ENDIF
294700000000     c*
294800000000     c* Chiusura per turno
294900160127     c                   if        esito='P'
295000160309     c                   eval      f_tasgc1 = arbgc1
295100160309     c                   eval      f_tasgc1 = arbgc2
295200160127     c                   endif
295300160127     c
295400140716if  1c                   IF        f_tasgc1 <> *blanks OR
295500140716     c                             f_tasgc2 <> *blanks
295600140716if  2c                   IF        f_tasgc1 <> *blanks
295700140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
295800000000     c                   EXSR      decgcx
295900000000     c                   MOVEL     wdgcx         d1wdgcx
296000000000e   2c                   ENDIF
296100140716if  2c                   IF        f_tasgc2 <> *blanks
296200140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
296300000000     c                   EXSR      decgcx
296400000000     c                   MOVEL     wdgcx         d2wdgcx
296500000000e   2c                   ENDIF
296600000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
296700000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
296800060612     c                   EVAL      we = ' ' +
296900060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
297000000000x   2c                   ELSE
297100000000     c                   EVAL      we = *blanks
297200000000e   2c                   ENDIF
297300060612     c                   EVAL      mgcxe =
297400060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
297500060612     c                             %TRIM(d1wdgcx)       +
297600060612     c                             we                   +
297700060612     c                             %TRIM(d2wdgcx)
297800000000     c*
297900010927if  2c                   IF        i < 10                                       *se c'è spazio
298000000000     c                   ADD       1             i                              *memorizza nota
298100000000     c                   EVAL      smnot(i) = mgcxe
298200010927e   2c                   ENDIF
298300010927e   1c                   ENDIF
298400000000     c*
298500000000     c* Fermo deposito
298600160127     c                   if        esito='P'
298700160309     c                   eval      f_tasffd = arbffd
298800160127     c                   endif
298900160127
299000140716if  1c                   IF        f_tasffd = 'S'
299100060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
299200000000     c*
299300010927if  2c                   IF        i < 10                                       *se c'è spazio
299400010927     c                   ADD       1             i                              *memorizza nota
299500010927     c                   EVAL      smnot(i) = mffd
299600010927e   2c                   ENDIF
299700010927e   1c                   ENDIF
299800030206     c*---
299900030206     c* memorizza le "note" che NON sono presenti nel record bolla
300000030206     c* ma sul file estensione "anagrafiche"
300100030206     c*---
300200030206     c* N° telefono destinatario
300300160127     c                   if        esito='P'
300400160309     C                   EVAL      MitOrigO74 = arbrmo
300500160127     c                   else
300600140716     c                   Z-ADD     f_tasaas      kaaaas
300700140716     c                   Z-ADD     f_taslnp      kaalnp
300800140716     c                   Z-ADD     f_tasnrs      kaanrs
300900140716     c                   Z-ADD     f_tasnsp      kaansp
301000030206     c                   MOVEL     'O'           kaatrc
301100030206     c     keytaa        CHAIN     titaa30c                           98
301200030206if  1c                   IF        NOT *in98
301300040330     C                   EVAL      MitOrigO74 = TAARSC
301400030206e   1c                   ENDIF
301500160127e   1c                   ENDIF
301600170310     c*
301700170310     c* se ho già usato il mitt originale come RSM --> pulisco il campo
301800170310     c                   if        wusarmo='S'
301900170310     c                   clear                   MitOrigO74
302000170310     c                   endif
302100030206     c*---
302200030206     c* memorizza le "note" che NON sono presenti nel record bolla
302300030206     c* ma sul file estensione testata boolle
302400030206     c*---
302500030206     c* Bancali
302600140716     c                   Z-ADD     f_tasaas      kr5aas
302700140716     c                   Z-ADD     f_taslnp      kr5lnp
302800140716     c                   Z-ADD     f_tasnrs      kr5nrs
302900140716     c                   Z-ADD     f_tasnsp      kr5nsp
303000030206     c                   MOVEL     'BAN'         kr5trd
303100160127     c                   if        esito='P'
303200160127     c     keyar5        CHAIN     fiar501l                           98
303300160127     c                   else
303400040531     c     keyar5        CHAIN     fiar531c                           98
303500160127     c                   endif
303600160127     c
303700030206if  1c                   IF        NOT *in98
303800030206     c                   MOVEL     ar5uni        dar5ban
303900060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
304000060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
304100030206if  2c                   IF        §ar5rb1 > *zeros OR
304200030206     c                             §ar5rb2 > *zeros
304300060613     c                   EVAL      mban = %trim(mban) +
304400060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
304500060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
304600030206e   2c                   ENDIF
304700030206     c*
304800030206if  2c                   IF        i < 10                                       *se c'è spazio
304900030206     c                   ADD       1             i                              *memorizza nota
305000030206     c                   EVAL      smnot(i) = mban
305100030206e   2c                   ENDIF
305200030206e   1c                   ENDIF
305300040330
305400040330     **?Reperisco referente
305500030206     c* Note supermercati
305600140716if  1c     f_tasftc      IFEQ      'S'
305700140716     c     f_tastc2      OREQ      'S'
305800030210     c                   CLEAR                   s                              *indice
305900140716     c                   Z-ADD     f_tasaas      kr5aas
306000140716     c                   Z-ADD     f_taslnp      kr5lnp
306100140716     c                   Z-ADD     f_tasnrs      kr5nrs
306200140716     c                   Z-ADD     f_tasnsp      kr5nsp
306300030206     c                   MOVEL     'SCR'         kr5trd
306400160127     c                   if        esito='P'
306500160127     c     keyar5        SETLL     fiar501l
306600160127     c     keyar5        READE     fiar501l                               98
306700160127     c                   else
306800040531     c     keyar5        SETLL     fiar531c
306900040531     c     keyar5        READE     fiar531c                               98
307000160127     c                   endif
307100160127     c
307200030210do  2c                   DOW       NOT *in98
307300030206     c                   MOVEL     ar5uni        dar5scr
307400030207     c* data nota
307500030210if  3c                   IF        ar5dac > *zeros
307600030207     c                   MOVEL     ar5dac        wdat
307700030207     c                   EXSR      edtdat                                       *edita data
307800030210     c                   MOVEL     wdate         msdace
307900030210e   3c                   ENDIF
308000030207     c* ora nota
308100030210if  3c                   IF        ar5orc > *zeros
308200030207     c                   MOVEL     ar5orc        wora
308300030207     c                   EXSR      edtora                                       *edita ora
308400030210     c                   MOVEL     worae         msorce
308500030210e   3c                   ENDIF
308600030207     c* Consegna richiesta
308700140708if  3c                   IF        §ar5dcr = *zeros AND
308800140708     c                             §ar5hcr = *zeros
308900140708x   3c                   ELSE
309000140708if  4c                   IF        §ar5dcr > *zeros
309100140708     c                   MOVEL     §ar5dcr       wdat
309200140708     c                   EXSR      edtdat                                       *edita data
309300140708     c                   MOVEL     wdate         msdcre
309400140708e   4c                   ENDIF
309500140708if  4c                   IF        §ar5hcr > *zeros
309600140708     c                   MOVEL     §ar5hcr       wora
309700140708     c                   EXSR      edtora                                       *edita ora
309800140708     c                   MOVEL     worae         mshcre
309900140708e   4c                   ENDIF
310000140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
310100140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
310200140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
310300140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
310400140708if  5c                   IF        §ar5tcr = 'P'                                *prima
310500140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
310600140708e   5c                   ENDIF
310700140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
310800140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
310900140708e   5c                   ENDIF
311000140708if  5c                   IF        §ar5tcr = ' '                                *il
311100140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
311200140708e   5c                   ENDIF
311300140708e   4c                   ENDIF
311400140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
311500140708     c                             §ar5dcr = *zeros                             *NO data richiesta
311600140708if  5c                   IF        §ar5tcr = 'P'                                *prima
311700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
311800140708e   5c                   ENDIF
311900140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
312000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
312100140708e   5c                   ENDIF
312200140708if  5c                   IF        §ar5tcr = ' '                                *il
312300140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
312400140708e   5c                   ENDIF
312500140708e   4c                   ENDIF
312600030210e   3c                   ENDIF
312700030210     c* Riferimento
312800030210if  3c                   IF        §ar5nom <>  *blanks
312900060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
313000060613     c                             + ' ' + §ar5nom
313100030210e   3c                   ENDIF
313200030210     c*
313300030210if  3c                   IF        s < 5                                        *MAX 5 note
313400030210     c                   ADD       1             s
313500030210     c                   EVAL      mscrT(s) = 'Il ' +
313600030210     c                             %trim(msdace) +                              *data nota
313700030210     c                             ' '    +
313800030210     c                             %trim(msorce) +                              *ora nota
313900030210     c                             ' '    +
314000140708     c                             %trim(mstcre) +                              *tipo consegna
314100140708     c                             ' '    +
314200140708     c                             %trim(msdcre) +                              *data consegna
314300140708     c                             ' '    +
314400140708     c                             %trim(mshcre) +                              *ora consegna
314500140708     c                             ' '    +
314600030210     c                             %trim(msnom)  +                              *parlato con
314700030210     c                             ' '    +
314800030210     c                             %trim(§ar5tel) +                             *telefono
314900030210     c                             ' '    +
315000030210     c                             %trim(§ar5mot)                               *motivo
315100030210e   2c                   ENDIF
315200030210     c*
315300160127     c                   if        esito='P'
315400160127     c     keyar5        READE     fiar501l                               98
315500160127     c                   else
315600160127     c     keyar5        READE     fiar531c                               98
315700160127e   1c                   ENDIF
315800030206e   2c                   ENDDO
315900030210e   1c                   ENDIF
316000030210     c* NOTA completa
316100030210if  1c                   IF        mscrT(1) <> *blanks
316200030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
316300030211     c                                      ' ' +
316400030211     C                                      %trim(mscrT(2)) +
316500030211     c                                      ' ' +
316600030211     C                                      %trim(mscrT(3)) +
316700030211     c                                      ' ' +
316800030211     C                                      %trim(mscrT(4)) +
316900030211     c                                      ' ' +
317000030211     C                                      %trim(mscrT(5))
317100030210     c                   Z-ADD     *zeros        s
317200030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
317300030210     c                   ADD       1             i
317400030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
317500030210     c                   EVAL      s = s + 100
317600030210e   2c                   ENDDO
317700030210     c*
317800030210e   1c                   ENDIF
317900010927     c*---
318000010927     c* memorizza le "note" che NON sono presenti nel record bolla
318100160127     c* ma sul file estensione
318200010927     c*---
318300160127     c
318400160127    0c                   if        esito<>'P'
318500010927     c                   CLEAR                   tita4000
318600010927     c                   CLEAR                   tita4p00
318700140716     c                   Z-ADD     f_tasaas      ka4aas
318800140716     c                   Z-ADD     f_taslnp      ka4lnp
318900140716     c                   Z-ADD     f_tasnrs      ka4nrs
319000140716     c                   Z-ADD     f_tasnsp      ka4nsp
319100160127     c
319200010927     c     ke2ta4        SETLL     tita430c
319300010927     c     ke2ta4        READE     tita430c                               98
319400010927do  1c                   DOW       NOT *in98
319500140708     c                   if        ta4trc = '8' OR ta4trc = '9'
319600050701     c*
319700050701if  3c                   IF        ta4trc = '8'
319800050701     c                   EVAL      mmnc8 = ta4not
319900050701e   3c                   ENDIF
320000050701if  3c                   IF        ta4trc = '9'
320100050701     c                   EVAL      mmnc9 = ta4not
320200050701e   3c                   ENDIF
320300050701     c*
320400010927e   2c                   ENDIF
320500010925     c*
320600010926     c     ke2ta4        READE     tita430c                               98
320700010926e   1c                   ENDDO
320800050701     C*
320900140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
321000050701     C*
321100050701     C                   IF        mmnc8 <> *BLANK
321200050701     C                   EVAL      a230 = %TRIML(mmnc8)
321300050701     C                   ENDIF
321400050701     C                   IF        mmnc9 <> *BLANK
321500050701     C                   IF        a230 <> *BLANK
321600050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
321700050701     C                   ELSE
321800050701     C                   EVAL      a230 = %TRIML(mmnc9)
321900050701     C                   ENDIF
322000050701     C                   ENDIF
322100010927     C*
322200050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
322300050701     C*
322400010927if  2c                   IF        i < 10                                       *se c'è spazio
322500010927     c                   ADD       1             i                              *memorizza nota
322600010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
322700010927e   2c                   ENDIF
322800010927     C*
322900010927if  2c                   IF        i < 10                                       *se c'è spazio
323000010927if  3c                   IF        n3 > 100
323100010927     c                   ADD       1             i                              *memorizza nota
323200010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
323300010927e   3c                   ENDIF
323400010927e   2c                   ENDIF
323500010927     C*
323600010927if  2c                   IF        i < 10                                       *se c'è spazio
323700010927if  3c                   IF        n3 > 200
323800010927     c                   ADD       1             i                              *memorizza nota
323900010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
324000010927e   3c                   ENDIF
324100010927e   2C                   ENDIF
324200010927e   1c                   ENDIF
324300160127e   0c                   ENDIF
324400010925     c*
324500000000     c* imposta contatore note da visualizzare nella DS di Output
324600000000     c                   EVAL      cntnoto74 = i
324700000000     c*
324800000000     c* imposta le note nella DS di Output
324900000000do  1c     1             DO        10            j
325000000000     c                   EVAL      nota(j) = smnot(j)
325100000000e   1c                   ENDDO
325200000000     c*
325300000000     c                   ENDSR
325400010716     c*--------------------------------------------------------------------------------------------*
325500010716     c* imposta i campi della DS di Output dalla firma DPD
325600010716     c*--------------------------------------------------------------------------------------------*
325700010716     c     impdsoDPD     BEGSR
325800010717     c*
325900010717     c* reimposta variabili di lavoro
326000010717     c                   RESET                   $AbFirma
326100010717     c                   CLEAR                   depute                         *deposito utente
326200010717     c                   CLEAR                   deppwd                         *deposito password
326300010717     c*---
326400010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
326500010717     c*---
326600010717     c                   Z-ADD     s_taslna      worg
326700010717     c                   EXSR      decorg
326800160406     c
326900020702     c                   IF        wodpd = 'DPD'
327000020702     c                   MOVEL     'S'           dpdo74
327100020702     c                   ELSE
327200020702     c                   MOVEL     *blanks       dpdo74
327300160406     c
327400160406     c* se si tratta di bolla EEX verifico se il partner è abilitato al link
327500160407     c                   IF        wodpd = 'EEX' and ksci74<>*blanks
327600160406     c                   clear                   fidn40ds
327700160406     c                   eval      i40fil=s_taslna
327800160406     c                   z-add     s_tasmgs      i40dsp
327900160406     c                   MOVEL     s_tasaas      i40dsp                          *data spedizione
328000160406     c                   call      'FIDN40R'
328100160406     c                   parm                    fidn40ds
328200160407
328300160406     c                   if        o40lnk='S'
328400160407     c* sommo i giorni alla data spedizione per sapere se ancora visibile
328500160407     c                   Clear                   xgiolavds
328600160407     c                   Eval      Ixglpa  = 'A'
328700160413     c                   Eval      IxglFil = s_taslna
328800160407     c                   Eval      Ixgldata = i40dsp
328900160407     c                   Eval      Ixgladd = 'S'
329000160407     c                   eval      ixglgg  = o40gio
329100160407     c                   Call      'XGIOLAV'
329200160407     c                   parm                    xgiolavds
329300160407     c                   if        oxgldata>=datcor
329400160406     c                   MOVEL     'E'           dpdo74
329500160407     c                   endif
329600160407     c
329700160406     c                   endif
329800160406     c                   endif
329900160406     c
330000020702     c                   ENDIF
330100010717     c*
330200010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
330300060627     c* Riferimento link Lettera di vettura
330400060627     c* Riferimento link Firma
330500010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
330600010717     c                   CLEAR                   tita4000
330700010717     c                   CLEAR                   tita4p00
330800060627     c                   CLEAR                   dsbl4i
330900010717     c                   Z-ADD     s_tasaas      ka4aas
331000010717     c                   Z-ADD     s_taslnp      ka4lnp
331100010717     c                   Z-ADD     s_tasnrs      ka4nrs
331200010717     c                   Z-ADD     s_tasnsp      ka4nsp
331300060627     c                   MOVEL     'I'           ka4trc
331400060627     c     keyta4        CHAIN     tita430c                           98
331500010717if  2c                   IF        NOT *in98
331600060627     c                   MOVEL     ta4not        dsbl4i
331700060627     c                   EVAL      DPDRmnO74 = §b4ipn
331800060627     c                   EVAL      DPDFnpO74 = §b4ipn
331900010717e   2c                   ENDIF
332000010717     c*
332100010717     c* cerca abilitazione firma DPD per questo segnacollo
332200010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
332300010717     c                   EXSR      RepAbilFirma
332400010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
332500010717     c*---
332600010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
332700010717     c*---
332800010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
332900010717     c* dati
333000010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
333100010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
333200010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
333300010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
333400010717     c*
333500010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
333600010717     c                   Z-ADD     s_tasaas      kvbaas
333700010717     c                   Z-ADD     s_taslnp      kvblnp
333800010717     c                   Z-ADD     s_tasnrs      kvbnrs
333900010717     c                   Z-ADD     s_tasnsp      kvbnsp
334000010717     c                   Z-ADD     99999999      kvbdev
334100010717     c                   Z-ADD     9999          kvbhev
334200010717     c     ke2evb        SETGT     fnevb01l
334300010717     c     keyevb        READPE    fnevb01l                               98
334400010717do  4c                   DOW       NOT *in98
334500101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
334600010720     c*
334700060629     c                   EVAL      devB01 = evbNot
334800060628     c                   IF        §NotDep = *BLANK
334900060628     c                   RESET                   DPDFirO74
335000060629     c                   LEAVE
335100060628     c                   ENDIF
335200060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
335300060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
335400080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
335500010720     c                   Z-ADD     *zeros        g08dat
335600010720     c                   MOVEL     '3'           g08err
335700010720     c                   CALL      'XSRDA8'
335800010720     c                   PARM                    wlbda8
335900010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
336000010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
336100010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
336200010720     c*
336300010717     c                   LEAVE                                                  *uscita ciclo
336400010717e   5c                   ENDIF
336500010717     c     keyevb        READPE    fnevb01l                               98
336600010717e   4c                   ENDDO
336700010717     c*
336800010717e   3c                   ENDIF
336900010717e   2c                   ENDIF
337000160323     c
337100160323   x1c                   else
337200010716e   1c                   ENDIF
337300010716     c*
337400010716     c                   ENDSR
337500160309     c*--------------------------------------------------------------------------------------------*
337600160309     c* impostazioni solo da FNBLP
337700160309     c*--------------------------------------------------------------------------------------------*
337800160309     c     impds_soloBLP BEGSR
337900160321     c
338000160321     c                   EVAL      dstaslnp = arblnp
338100160321     c                   EVAL      dstasnrs = arbnrs
338200160321     c                   EVAL      dstasnsp = arbnsp
338300160321     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
338400161117
338500161117     c                   MOVEL     arbaas        aasmgsO74                       *data spedizione
338600161117     c                   MOVE      arbmgs        aasmgsO74
338700161117     c
338800161117     c                   MOVEL     'N'           gco74                          *flag di controllo
338900161117     c                   MOVEL     'N'           gcfdio74
339000161117     c                   MOVEL     'N'           gcmito74
339100161117     c*
339200160314     C
339300160314     c                   Z-ADD     arblna        worg
339400160314     c                   exsr      setLnaItaEst
339500160314     C
339600160309     c* chain ar5 con bolla richiesta a video
339700160309     c                   Z-ADD     kasaas        kr5aas
339800160309     c                   Z-ADD     kaslnp        kr5lnp
339900160309     c                   Z-ADD     kasnrs        kr5nrs
340000160309     c                   Z-ADD     kasnsp        kr5nsp
340100160309     c                   MOVEL     'GEN'         kr5trd
340200160309     c     keyar5        CHAIN     fiar501l
340300160309     C                   IF        %FOUND
340400160309     C                   EVAL      DAR5GEN = AR5Uni
340500160309     C                   ELSE
340600160309     C                   RESET                   DAR5GEN
340700160309     C                   ENDIF
340800160309     C                   EVAL      REFCONSO74 = GEN§AR5REF
340900160309     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
341000160309     c*
341100160309     c* chain tipo bolla
341200160321     c                   eval      tblkey=arbcbo
341300160314     C                   EVAL      ds3A = chainTabel00f('CHAIN3':langTabel
341400160321     C                             :'3A':tblkey:%LEN(tblkey):'TBLUNI':
341500160314     C                             rpyOpCode:rpyEsito)
341600160309     c*
341700160309     c                   MOVEL     §3atb1        a1
341800160309     c                   MOVEL     §3atb1        tpporto74
341900160309if  1c                   IF        a1 = 'A'                                     *Pagamento del
342000160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
342100160309x   1c                   ELSE                                                   *Pagamento del
342200160309     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
342300160309e   1c                   ENDIF
342400160314     c
342500160314     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
342600160314     C                   IF        KSCI74 <> *BLANK
342700160314     c                   SETOFF                                           98
342800160314     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
342900160314     C                   RESET                   tis7700i0
343000160314     C                   EVAL      tis7700i0.aas = arbAas
343100160314     C                   EVAL      tis7700i0.lnp = arbLnp
343200160314     C                   EVAL      tis7700i0.nrs = arbNrs
343300160314     C                   EVAL      tis7700i0.nsp = arbNsp
343400160314     C                   EVAL      tis7700i0.tbl = §3atb1
343500160314     C                   EVAL      tis7700i0.ksu = kscI74
343600160314     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
343700160314     C***                CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
343800160314     C***                          : rpyEsito
343900160314     C***                          : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
344000160314     C***                          : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
344100160314     C***                          )
344200160314     C***                EVAL      *IN98 = (rpyEsito >= *ZERO AND
344300160314     C***                          tis7700o0.bollValida = *ON)
344400160314     **?Se non gli appartiene, prima di restituire errore controllo il check code.
344500160314if  2c                   IF        NOT *in98                                      *non trovato
344600160314     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
344700160314     C                   reset                   TIS778DSO
344800160818     C*****              EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
344900160818     c                   eval      esito='1'
345000160818     c                   eval      widmsg = 'TIS0849'
345100160818     c                   eval      widcampo = 'NSPEDIZI74'
345200160818     c                   clear                   wparm
345300160818     c                   exsr      messaggi
345400160818     c                   exsr      routend
345500160818     C******             RETURN
345600160314     C                   ENDIF
345700160314e   2c                   ENDIF
345800160314     C                   ENDIF
345900160314
346000160314     c
346100160309     c                   ENDSR
346200160314     c*--------------------------------------------------------------------------------------------*
346300160314     c* impostazioni del C/Assegno
346400160314     c*--------------------------------------------------------------------------------------------*
346500160314     c     ImpdsoAR9     BEGSR
346600160314     c                   Z-ADD     waas          ksbaas
346700160314     c                   Z-ADD     wlnp          ksblnp
346800160314     c                   Z-ADD     wnrs          ksbnrs
346900160314     c                   Z-ADD     wnsp          ksbnsp
347000160314     c     keycsb        CHAIN     fiar901l                           99
347100160314if  2c                   IF        NOT *in99                                    *esistente
347200160314     c* memorizza i dati
347300160314     c                   MOVEL     'S'           cao74                          *esiste C/A
347400160314     c*
347500160314     c                   Z-ADD     ar9cas        caimpo74                       *importo
347600160314     c*
347700160314     c                   EVAL      cadivo74 =  ar9vca                           *divisa
347800160314if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
347900160314     c                             caimpo74 > *zeros
348000160314     c                   EVAL      cadivo74 = 'ITL'
348100160314e   3c                   ENDIF
348200160314
348300160314     ** Visualizzo la descrizione del tipo incasso
348400160314     c**  devo impostare alcuni campi di CSB
348500160314     c                   eval      csbaas=ar9aas
348600160314     c                   eval      csblnp=ar9lnp
348700160314     c                   eval      csbnrs=ar9nrs
348800160314     c                   eval      csbnsp=ar9nsp
348900160314     c                   clear                   csbddc
349000160314     c                   clear                   csbtpa
349100160314     c                   clear                   csbtpi
349200160314     c
349300160314     C                   EVAL      cainco74 =
349400160314     C                                       GetDescrizioneIncassoContrassegno()
349500160314     c* stato C/Assegno : attivo
349600160314     c                   MOVEL     *blanks       castao74                       *stato
349700160314     C                   EVALR     tblKey = '0'
349800160314     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
349900160314     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
350000160314     C                             :rpyOpCode:rpyEsito)
350100160314     C                   IF        rpyOpCode = 'FOUND'
350200160314     c                   EVAL      castao74 = §4SDei
350300160314e   3c                   ENDIF
350400160314
350500160314if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
350600160314     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)     *NO distinta incasso
350700160314     c                   endif
350800160314
350900160314     c                   endif
351000160314     c
351100160314     c                   ENDSR
351200140604     c*--------------------------------------------------------------------------------------------*
351300140604     c* imposta i campi da FNARB se TASDCM = 0
351400140604     c*--------------------------------------------------------------------------------------------*
351500140604     c     impdsoARB     BEGSR
351600150403     c                   clear                   orarich           4 0
351700150611     c                   clear                   wdee
351800170301     c                   clear                   mito74
351900150410     c* memorizzo se utilizzo la data prevista consegna
352000150410     c                   clear                   usaprev           1
352100170301     c                   eval      wbol='ARB'
352200140604     c*
352300140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
352400140604     c                   EVAL      lnaDescO74 = sodes(i)
352500140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
352600140604     c*
352700150116      *eliminata forzatura resta impostato dalla mamma per composizione link
352800150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
352900150116     c*                  MOVE      arbmgs        aasmgsO74
353000140818      *riempimento da verificare
353100140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
353200140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
353300140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
353400140818
353500140818     c                   if        tpporto74 = 'A'
353600140818     c                   if        arbccm > 0
353700140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
353800140818     c                   else
353900140818     c                   movel     arblnp        ccmO74
354000140818     c                   move      8888          ccmO74
354100140818     c                   endif
354200140818     c                   else
354300140818     c                   z-add     arbksc        ccmO74
354400140818     c                   endif
354500140818      *
354600140604     c                   MOVEL     arblna        lineaaro74
354700140604     c                   MOVEL     arbtsp        tpservio74
354800140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
354900140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
355000140604     C                             rpyOpCode:rpyEsito)
355100140604     C                   EVAL      corro74 = §5EDes
355200140604     c*
355300140604     c                   EVAL      destino74 = arbrsd
355400140604     c                   EVAL      desindo74 = arbind
355500140604     c                   EVAL      descapo74 = arbcad
355600140604     c                   EVAL      desloco74 = arblod
355700140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
355800140604if  1c                   IF        arbnzd    = *blanks
355900140604     c                   EVAL      desnazo74 = arbprd
356000140604x   1c                   ELSE
356100150326     c**                 EVAL      desnazo74 = arbnzd
356200150326     c**                 EVAL      desnzdo74 = arbnzd
356300150326     c* 26/03/15: Attiviamo le specifiche per provincia export
356400150326     c                   EVAL      desnazo74 = arbprd
356500150326     c                   EVAL      desnzdo74 = arbnzd
356600150326e   1c                   ENDIF
356700140604     c*
356800140604     c                   EVAL      collio74  = arbncl
356900140604     c                   EVAL      pesoo74   = arbpkf
357000140604     c                   EVAL      volumeo74 = arbvlf
357100140604     c                   EVAL      naturao74 = arbnas
357200140604     c*
357300140604     c                   EVAL      disnumo74  = arbndc
357400140604c    c                   MOVEL     arbddc        wdat
357500140604     c                   EXSR      edtdat
357600140604     c                   EVAL      disdatao74 = wdate
357700140604     c*
357800170310     c                   MOVE      ccmo74        alfa4
357900170310if  1c                   IF        alfa4 <> '8888'                                *codificato
358000170310     c                   clear                   mito74
358100170310     c                   clear                   solomito74
358200170301     c                   exsr      tabcli
358300170310     c                   else
358400140604     c                   EVAL      mito74    = arbrsm
358500170310     c                   endif
358600170310
358700140604     c                   EVAL      mitindo74 = arbinm
358800140604     c                   EVAL      mitcapo74 = arbcam
358900140604     c                   EVAL      mitloco74 = arblom
359000140604      *specifiche da eliminare dopo la modifica in immissione bolle
359100140604if  3c                   IF        arbnzm = *blanks
359200140604     c                   EVAL      mitnazo74 = arbprm
359300140604x   3c                   ELSE
359400140604     c                   EVAL      mitnazo74 = arbnzm
359500140604e   3c                   ENDIF
359600140604     c* 2a ragione sociale destinatario.
359700140604     c                   Z-ADD     arbaas        ka4aas
359800140604     c                   Z-ADD     arblnp        ka4lnp
359900140604     c                   Z-ADD     arbnrs        ka4nrs
360000140604     c                   Z-ADD     arbnsp        ka4nsp
360100140604     c                   MOVEL     'D'           ka4trc
360200140604     c     keyta4        CHAIN     fiar401l                           99
360300140604if  1c                   IF        NOT *in99
360400140604     c                   MOVEL     ar4not        desti2O74
360500140604e   1c                   ENDIF
360600140604     c                   MOVEL     'E'           ka4trc
360700140604     c     keyta4        CHAIN     fiar401l                           99
360800140604if  1c                   IF        NOT *in99
360900140604     c                   MOVEL     ar4not        dsbl4e
361000140604e   1c                   ENDIF
361100140604     c
361200140604     ** Riferimento mittente numerico (c'è sempre).
361300140604     C                   EVAL      rifero74 = arbrmn
361400140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
361500140604     C                   IF        arbrma <> *blank and
361600140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
361700140604     C                   EVAL      rifero74a = arbrma
361800140604     C                   ENDIF
361900140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
362000140604     C                   IF        §b4erp <> *BLANK AND
362100140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
362200140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
362300140604     C                   EVAL      rpto74 = §b4erp
362400140604     C                   ENDIF
362500140604
362600140604     **?Reperisco il check code della spedizione.
362700140604     C                   EVAL      SChkCdeO74 =
362800140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
362900140604     C                             NSpedizI74:ChkCode:nEsito)
363000140604
363100140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
363200141126     C*m                 IF        KSCI74 <> *BLANK
363300141126     c*m                 SETOFF                                           98
363400140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
363500141126     C*m                 RESET                   tis7700i0
363600141126     C*m                 EVAL      tis7700i0.aas = arbAas
363700141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
363800141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
363900141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
364000141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
364100141126     C*m                 EVAL      tis7700i0.ksu = kscI74
364200141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
364300141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
364400141126     C*m                           : rpyEsito
364500141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
364600141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
364700141126     C*m                           )
364800141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
364900141126     C*m                           tis7700o0.bollValida = *ON)
365000140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
365100141126if  2c*m                 IF        NOT *in98                                      *non trovato
365200141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
365300141126     C*m                 reset                   TIS778DSO
365400141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
365500141126     C*m                 RETURN
365600141126     C*m                 ENDIF
365700141126e   2c*m                 ENDIF
365800141126     C*m                 ENDIF
365900140604     c*
366000140604     ** Importo assicurato.
366100140604     C                   IF        arbIas > 0
366200140604     C                   EVAL      vasO74 = arbVas
366300140604     C                   EVAL      iasO74 = arbIas
366400140604     C                   ENDIF
366500140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
366600140605     c                   if        arbdcm = 0 and wcev <> '704'
366700140617     c                   if        arbdcr > 0
366800140605     c                   MOVEL     arbdcr        wdat
366900140605     c                   EXSR      edtdat
367000140605     c                   eval      DTCONRCO74  =  wdate
367100140819if  3c                   IF        arbtcr = ' '
367200140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
367300140819e   3c                   ENDIF
367400140617     c                   endif
367500140617     c                   if        arbhcr > 0
367600140617     c                   MOVEL     arbhcr        wora
367700140617     c                   EXSR      edtora
367800140617     c                   eval      ORACONRO74  =  worae
367900140617     c                   endif
368000140606     c                   eval      TPDATCRO74  =  arbtcr
368100160407     c
368200140605      *reperimento descrizione tipo consegna richiesta
368300140617if  3c                   IF        arbtcr = 'P'
368400140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
368500140617e   3c                   ENDIF
368600140617if  3c                   IF        arbtcr = 'D'
368700140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
368800140617e   3c                   ENDIF
368900160407     c
369000160407     c
369100160407     c* Per  le bolle in partenza eseguo solo se presente la data entrata
369200160407    0c                   if        esito<>'P' or arbdse>0
369300160407     c
369400140605     C                   clear                   tnsd99ds
369500140605     C                   MOVEL     ' '           D98tla
369600160314     c                   if        esito='P'
369700160314     C                   MOVEL     'P'           D98tbo
369800160314     c                   else
369900140605     C                   MOVEL     'A'           D98tbo
370000160314     c                   endif
370100140605     C                   Z-ADD     arbaas        D98aas
370200140605     C                   Z-ADD     arblnp        D98lnp
370300140605     C                   Z-ADD     arbnrs        D98nrs
370400140605     C                   Z-ADD     arbnsp        D98nsp
370500140605     C*
370600140605     C                   call      'TNSD99R'
370700140605     C                   parm                    tnsd99ds
370800150304     c
370900140605     C*
371000140917      * chiamata a trulors per tempi standard
371100140917     c                   clear                   trulorsds
371200140917     c                   clear                   trulor2ds
371300140917     c                   clear                   trulor3ds
371400150611     c                   eval      wdee=d98dee
371500140917      * solo se questa data è valorizzata
371600150304    1c                   if        d98dee > 0
371700140912     c                   movel     d98dee        wdat
371800140605     c                   exsr      edtdat
371900140605     c                   eval      DTTEOCOO74  =  wdate
372000140819     c
372100140904      * se fermo deposito non richiamo pgm per reperimento orari
372200150304    2c                   if         arbffd = *blank
372300140605     c                   eval       IOREtser = 'C'
372400140605     c                   eval       IOREDDC=arbddc
372500140605     c                   eval       IOREnDC=arbndc
372600140609     c                   eval       IOREfil=arblna
372700140609     c                   eval       IOREdta=d98dce
372800140609     c                   eval       IOREcap=arbcad
372900140609     c                   eval       IOREloc=arblod
373000151019     c*****              eval       IOREtfp=arbtfp
373100151019     c* se il campo "terminal didisguido" è pieno, passo questo al posto
373200151019     c* del terminal di partenza originale
373300151019     c                   if         d98flp>*zeros
373400151019     c                   eval       IOREtfp=%INT(d98flp)
373500151019     c                   else
373600151019     c                   eval       IOREtfp=arbtfp
373700151019     c                   endif
373800140609     c                   eval       IOREtfa=arbtfa
373900140610     c                   eval       IOREdti=arbdti
374000140610     c                   eval       IOREhti=arbhti
374100140610     c                   eval       IOREdcr=arbdcr
374200140610     c                   eval       IOREhcr=arbhcr
374300140610     c                   eval       IOREtcr=arbtcr
374400140819     c                   eval       IOREtsp=arbtsp
374500140819     c                   eval       IOREnar=arbnzd
374600140819     c                   eval       IOREdei=d98dei
374700140819     c                   eval       IOREoei=d98oei
374800150819     c                   clear                   diore01
374900150819     c                   movel     d98ttc        §ioretrazC
375000150819     c                   movel     d98tcc        §ioreconsC
375100150819     c                   eval       IOREflo=diore01
375200150325     c* passo la località normalizzata
375300150325     c                   clear                   tita4000
375400150325     c                   clear                   tita4p00
375500150325     c                   Z-ADD     arbaas        ka4aas
375600150325     c                   Z-ADD     arblnp        ka4lnp
375700150325     c                   Z-ADD     arbnrs        ka4nrs
375800150326     c                   Z-ADD     arbnsp        ka4nsp
375900150325     c                   MOVEL     'X'           ka4trc
376000160314     c                   if        esito='P'
376100160314     c     keyta4        CHAIN     fiar401l                           99
376200160314if  1c                   IF        NOT *in99
376300160314     c                   MOVEL     ar4not        OOR3LOC_N
376400160314     c                   else
376500160314     c                   movel     arblod        OOR3LOC_N
376600160314e   1c                   ENDIF
376700160314     c
376800160314     c                   else
376900160314     c     keyta4        CHAIN     tita430c                           99
377000150325if  1c                   IF        NOT *in99
377100150325     c                   MOVEL     ta4not        OOR3LOC_N
377200150325     c                   else
377300150325     c                   movel     arblod        OOR3LOC_N
377400150325e   1c                   ENDIF
377500160314e   1c                   ENDIF
377600140605     c
377700160321     c                   if        esito<>'P'
377800150304    3c                   IF         (ARBfbc = *blanks and
377900140605     c                               ARBddc > 0 and
378000140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
378100140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
378200140605     c                   eval       IOREcons = 'S'
378300150304    3c                   endif
378400160321     c
378500140605     c* Orario presunto ritiro/consegna
378600140605     c                   move      arbndc        pctndc
378700140605     c                   move      arbifp        pctfgs
378800140605     c                   move      'COR'         ttrd
378900140605     c     kcet          setgt     fipct02l
379000140605     c     kcet          readpe    fipct02l
379100150304    3c                   if        not %eof(fipct02l)
379200140605     c                   movel     pctdati       fipctcords
379300140605     c                   eval      ioreorp=§PCTORASTI
379400140605     c                   endif
379500160321    3c                   endif
379600160321
379700140605     c                   call      'TRULORSR'
379800140605     C                   PARM                    kpjba
379900140605     C                   PARM                    trulorsds
380000140605     C                   PARM                    trulor2ds
380100140605     C                   PARM                    trulor3ds
380200150304    3c                   endif
380300140818     c                   clear                   ORSTINIO74
380400150304    3c                   if        OOR2STIS > 0
380500140617     c                   MOVEL     OOR2STIS      wora
380600140617     c                   EXSR      edtora
380700140617     c                   eval      ORSTINIO74  =  worae
380800150304    3c                   endif
380900140818     c                   clear                   ORSTFINO74
381000150304    3c                   if        OOR2STFS > 0
381100140617     c                   MOVEL     OOR2STFS      wora
381200140617     c                   EXSR      edtora
381300140617     c                   eval      ORSTFINO74  =  worae
381400150304    3c                   endif
381500140818     c                   clear                   ORMIINIO74
381600150304    3c                   if        OOR2MIIS > 0
381700140617     c                   MOVEL     OOR2MIIS      wora
381800140617     c                   EXSR      edtora
381900140617     c                   eval      ORMIINIO74  =  worae
382000150304    3c                   endif
382100140818     c                   clear                   ORMAFINO74
382200150304    3c                   if        OOR2MXFS > 0
382300140617     c                   MOVEL     OOR2MXFS      wora
382400140617     c                   EXSR      edtora
382500140617     c                   eval      ORMAFINO74  =  worae
382600150304    3c                   endif
382700150304    3c                   if        wricons = 'S'
382800140818     c                   clear                   ORRIDALO74
382900150304    4c                   if        OOR2RIDS > 0
383000140617     c                   MOVEL     OOR2RIDS      wora
383100140617     c                   EXSR      edtora
383200140617     c                   eval      ORRIDALO74  =  worae
383300150304    4c                   endif
383400140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
383500140818     c                   clear                   ORRIALLO74
383600150304    4c                   if        OOR2RIAS > 0
383700140617     c                   MOVEL     OOR2RIAS      wora
383800140617     c                   EXSR      edtora
383900140617     c                   eval      ORRIALLO74  =  worae
384000150304    4c                   endif
384100140617     c                   eval      FLRICONO74  =  OOR2FRIC
384200150304   x3c                   else
384300140711     c                   clear                   ORRIALLO74
384400140711     c                   clear                   ORRIDALO74
384500140711     c                   eval      FLRICONO74  =  'N'
384600150304    3c                   endif
384700140818     c                   clear                   ORSTICDO74
384800150304    3c                   if        OOR2PRESD > 0
384900140617     c                   MOVEL     OOR2PRESD     wora
385000140617     c                   EXSR      edtora
385100140617     c                   eval      ORSTICDO74  =  worae
385200150304    3c                   endif
385300140818     c                   clear                   ORSTICAO74
385400150304    3c                   if        OOR2PRESa > 0
385500140617     c                   MOVEL     OOR2PRESa     wora
385600140617     c                   EXSR      edtora
385700140617     c                   eval      ORSTICAO74  =  worae
385800150304    3c                   endif
385900140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
386000140605     c                   eval      FLLOCNOO74  =  OOR3NORM
386100140904      *end F.D.
386200150304    2c                   endif
386300140605      * RIFERIMENTI DESTINATARIO
386400140605     c                   MOVEL     'EMD'         kr5trd
386500140605     c     keyAR5        CHAIN     fiar501l
386600150304if  2c                   IF        %found(fiar501l)
386700140605     c                   MOVEL     ar5uni        dar5emd
386800140606     c                   eval      EMLDESTO74  =  §§AR5EML
386900140606     c                   eval      CELDESTO74  =  §§AR5TEL
387000150304e   2c                   ENDIF
387100140604     **
387200150304     c*
387300150304     c* Stato data prevista consegna
387400150305     c                   clear                   stat_n2o74
387500150305     c                   eval      STATUSo74   =  D98SPCDEE
387600150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
387700150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
387800150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
387900150312     c                   eval      statuso74='RIC'
388000150403     c* se presente FIARP --> visualizzo la data richiesta
388100150403     c     karp          chain     fiarp01l
388200150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
388300150611     c                   movel     arpdcr        wdee
388400150611     c                   movel     arpdcr        wdat
388500150403     c                   exsr      edtdat
388600150403     c                   eval      DTTEOCOO74  =  wdate
388700150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
388800150421    3C                   IF        ARPHCR>0
388900150403     c                   eval      orarich=arphcr
389000150421    3c                   endif
389100150421   x2c                   else
389200150403     c*  se presente DOPO IL imposto la data in D98dee
389300150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
389400150403     c                   eval      D98dee=arpdcr
389500150611     c                   eval      wdee=arpdcr
389600150421    3c                   endif
389700150403     c* PRIMA DEL come se non ci fosse...
389800150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
389900150421    3c                   if        d98dee>0
390000150312     c                   Clear                   xgiolavds
390100150312     c                   Eval      Ixglpa  = 'A'
390200150312     c                   Eval      IxglFil = arblna
390300150312     c                   Eval      Ixgldata = d98dee
390400150312     c                   Eval      Ixgladd = 'S'
390500150312     c                   Eval      Ixgllav = 'S'
390600150312     c                   eval      ixglgg  = 1
390700150312     c                   Call      'XGIOLAV'
390800150312     c                   parm                    xgiolavds
390900150611     c                   movel     oxgldata      wdee
391000150611     c                   movel     oxgldata      wdat
391100150312     c                   exsr      edtdat
391200150312     c                   eval      DTTEOCOO74  =  wdate
391300150421    3c                   endif
391400150421    2c                   endif
391500150421    1c                   endif
391600150421     c
391700150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
391800150421     c                   eval      statuso74='ATT'
391900150421     c                   endif
392000150421    1c                   if        PDAgia='S' and statuso74='DDC'
392100150421     c                   eval      statuso74='GIA'
392200150421     c                   endif
392300150401     c
392400150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
392500150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
392600150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
392700150401     c* La data contatto = data distinta + 1
392800150512     c**                 Clear                   xgiolavds
392900150512     c**                 Eval      Ixglpa  = 'A'
393000150512     c**                 Eval      IxglFil = arblna
393100150512     c**                 Eval      Ixgldata = arbddc
393200150512     c**                 Eval      Ixgladd = 'S'
393300150512     c**                 Eval      Ixgllav = 'S'
393400150512     c**                 eval      ixglgg  = 1
393500150512     c**                 Call      'XGIOLAV'
393600150512     c**                 parm                    xgiolavds
393700150512     c**                 movel     oxgldata      wdat
393800150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
393900150512     c**                 endif
394000150317     c
394100150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
394200150326     c*  lo status diventa RIC
394300150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
394400150317     c                   eval      statuso74='RIC'
394500150317     c                   endif
394600150310     c
394700150304     c* decodifica stato da mettere nei campi deco e note
394800150304     c                   clear                   dspc
394900150304     C                   RESET                   tibs02ds
395000150304     C                   EVAL      t02Cod = 'SPC'
395100150312     C                   EVAL      t02Ke1 = statuso74
395200150306     C                   EXSR      getTntbe00f
395300150306     c                   IF        t02Err <>'E'
395400150306     c
395500150305     c                   eval       stat_d1o74=§SPCDEC1
395600150305     c                   eval       stat_d2o74=§SPCDEC2
395700150304     c*
395800150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
395900150305     c                   eval       stat_n1o74=§SPCNOT1
396000150306     c                   eval       stat_n2o74=§SPCNOT2
396100150304     c                   else
396200150305     c                   clear                   stat_d1o74
396300150305     c                   clear                   stat_d2o74
396400150305     c                   clear                   stat_n1o74
396500150306     c                   clear                   stat_n2o74
396600150304     c                   endif
396700160407
396800160407     c                   endif
396900160407
397000150409     c                   else
397100150409     c* stato evento CONSEGNATO
397200150409     c                   eval      statuso74='CON'
397300150415     c                   eval      flgbolcO74  =  'S'
397400150415     c                   eval      flgboldO74  =  'N'
397500150409    1c                   endif
397600150304     c
397700150304     c
397800140919     ** riempimento per campi da arb con data consegna diversa da 0
397900140919     c                   if        arbdcm > 0
398000140919     c                   movel     arbdcm        wdat
398100140919     c                   exsr      edtdat
398200140919     c                   eval      dtconmeo74  = wdate
398300140919     c                   MOVEL     arbdcm        wora
398400140919     c                   EXSR      edtora
398500140919     c                   eval      orconmeO74  =  worae
398600140919     c                   eval      flgbolcO74  =  'S'
398700140919     c                   eval      flgboldO74  =  'N'
398800141015      *firmatario distinta PDA
398900141015     c                   clear                   firmato74
399000141015     c                   if        gen§ar5pdaco <> 'NO'
399100141015     c                   exsr      repfirmatARB
399200141015     c                   else
399300141015     c                   if        arbgma <> *blank
399400141015     C                   EVAL      tblKey = arbgma
399500141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
399600141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
399700141015     C                             :rpyOpCode:rpyEsito)
399800141015     C                   IF        rpyOpCode = 'FOUND'
399900141015     c                   if        §7r2fi = 'S'
400000141015     c                   exsr      repfirmatARB
400100141015     c                   endif
400200141015     c                   endif
400300141015     c                   endif
400400141015     c                   endif
400500140919     c                   else
400600150415     c* solo se non impostato da PdA lo stato CONSEGNA
400700150415     c                   if        statuso74<>'CON'
400800150415     c
400900140919     c                   eval      flgbolcO74  =  'N'
401000140919     c                   if        arbndc > 0 and arbddc > 0
401100150415     c                             and arbddc <= datcor and d98cons='S'
401200140919     c                   eval      flgboldo74 = 'S'
401300140919     c                   else
401400140919     c                   eval      flgboldO74  =  'N'
401500140919     c                   endif
401600150415     c                   endif
401700150415
401800140919     c                   endif
401900150415     c
402000150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
402100140919     c                   eval      orincono74 = orsticdo74
402200140919     c                   eval      orficono74 = ORSticaO74
402300140919     c                   else
402400140919     c                   eval      orincono74 = ORSTINIO74
402500140919     c                   eval      orficono74 = ORSTFINO74
402600140919     c                   endif
402700150326     c* note 1
402800150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
402900150309     c                   clear                   wparm
403000150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
403100150326     c                   if        D98DTCTD<>*blanks
403200150326     c                   movel     D98DTCTD      wdat
403300150326     c                   exsr      edtdat
403400150326     c                   else
403500150326     c                   eval      wdate=dtteocoo74
403600150410     c                   eval      usaprev='S'
403700150326     c                   endif
403800150326     c
403900150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
404000150309     c                   eval      stat_n1o74= rtvMsgLang(
404100150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
404200150309     c                   endif
404300150309     c* note 2
404400150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
404500150309     c                   clear                   wparm
404600150403     c* Se lo stato è PRG passo orario limite uscita aut
404700150403     c                   select
404800150403     c                   when      D98SPCDEE='PRG'
404900150326     c                   IF        d98picklim  >*zeros
405000150309
405100150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
405200150326     c                                   (%subst(d98picklim:3:2))
405300150309     c                   else
405400150309     c                   eval      wparm='09:00'
405500150403     c* reperisco orario di fine picking
405600150403     c* filiale / data
405700150403     C                   RESET                   tibs02ds
405800150403     C                   EVAL      t02Cod = 'OLP'
405900150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
406000150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
406100150403     C                   EXSR      getTntbe00f
406200150403     c* filiale
406300150403     c                   IF        t02Err = 'E'
406400150403     C                   RESET                   tibs02ds
406500150403     C                   EVAL      t02Cod = 'OLP'
406600150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
406700150403     C                   EXSR      getTntbe00f
406800150403     c                   endif
406900150403     c* Generico 000
407000150403     c                   IF        t02Err = 'E'
407100150403     C                   RESET                   tibs02ds
407200150403     C                   EVAL      t02Cod = 'OLP'
407300150403     C                   EVAL      t02Ke1 = '000'
407400150403     C                   EXSR      getTntbe00f
407500150403     c                   endif
407600150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
407700150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
407800150403     c                             %subst(t02uni:3:2)
407900150309     c                   endif
408000150403     c                   endif
408100150403     c                   eval      stat_n2o74= rtvMsgLang(
408200150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
408300150403     c                   other
408400150403     c* altri casi
408500150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
408600150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
408700150611     c                             arbtcr=' ' and arbdcr>0  and
408800150611     c                             arbdcr>=wdee
408900150410     c
409000150403     c                   eval      orarich=arbhcr
409100150410     c                   endif
409200150403     c
409300150403     c                   if        orarich>0
409400150403     c                   exsr      sr_dallealle
409500150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
409600150403     c                   eval      stat_n2o74= rtvMsgLang(
409700150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
409800150403     c
409900150403     c                   else
410000150403     c                   clear                   stat_n2o74
410100150403     c                   endif
410200150403     c
410300150403     c                   endsl
410400150403     c
410500150309     c                   endif
410600150410     c
410700150424     c* Se si tratta di fermo deposito pulisco sempre
410800150424     c                   if        usaprev<>'S' or arbffd='S'
410900150410     c                   clear                   DTTEOCOO74
411000150410     c                   clear                   ORINCONO74
411100150410     c                   clear                   ORFICONO74
411200150410     c                   endif
411300150309     c
411400170301     c                   clear                   wbol              3
411500140604     c                   ENDSR
411600150403      /free
411700150403       //--------------------------------------------------------------
411800150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
411900150403       //--------------------------------------------------------------
412000150403       BEGSR  sr_dallealle;
412100150403       clear  w_dallemsg ;
412200150403       clear  w_allemsg ;
412300150403
412400150403       // Reperimento dati tabella VPO - ORARISER
412500150403       if §VPORST_MW=0  ;
412600150403       clear tibs02ds;
412700150403       clear dvpooraris;
412800150403       t02mod='C';
412900150403       t02cod='VPO';
413000150403       t02ke1='ORARISER';
413100150403       tibs02r(kpjba:tibs02ds);
413200150403       if t02err=*blanks;
413300150403          dvpooraris=t02uni;
413400150403       endif;
413500150403       endif;
413600150403
413700150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
413800150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
413900150403       //   determino orari servizio comprensivi della tolleranza
414000150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
414100150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
414200150403
414300150403            if w_dalle<w_dalleoser;
414400150403               w_dalle=w_dalleoser;
414500150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
414600150403            endif;
414700150403
414800150403            if w_alle>w_alleoser;
414900150403               w_alle=w_alleoser;
415000150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
415100150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
415200150403               endif;
415300150403            endif;
415400150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
415500150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
415600150403
415700150403       endsr;
415800150403      /end-free
415900010717     c*--------------------------------------------------------------------------------------------*
416000010717     c* imposta i campi della DS di Output dagli eventi
416100010717     c*--------------------------------------------------------------------------------------------*
416200010717     c     RepAbilFirma  BEGSR
416300010717     c*
416400010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
416500010717     c* e recupera utente / password
416600010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
416700010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
416800010717     c     keyvtd        SETLL     tivtd01l
416900010717     c     keyvtd        READE     tivtd01l                               98
417000010717do  2c                   DOW       NOT *in98
417100010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
417200010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
417300010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
417400010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
417500010717     c                   EVAL      depute = vtdute                              *utente
417600010717     c                   EVAL      deppwd = vtdpwd                              *password
417700010717     c                   LEAVE                                                  *esce dal ciclo
417800010717e   3c                   ENDIF
417900010717     c     keyvtd        READE     tivtd01l                               98
418000010717e   2c                   ENDDO
418100010717e   1C                   ENDIF
418200010717     c*
418300010717     c                   ENDSR
418400000000     c*--------------------------------------------------------------------------------------------*
418500000000     c* edita data da aaaammgg in gg/mm/aaaa
418600000000     c*--------------------------------------------------------------------------------------------*
418700000000     c     edtdat        BEGSR
418800000000     c*
418900000000     c                   MOVEL     *blanks       wdate
419000000000     c*
419100000000     c                   MOVEL     wdat          dsdat
419200000000     c                   MOVEL     dsgio         dsgioe
419300140528     c                   MOVEL     '.'           dsvd1
419400000000     c                   MOVEL     dsmes         dsmese
419500140528     c                   MOVEL     '.'           dsvd2
419600000000     c                   MOVEL     dsann         dsanne
419700000000     c                   MOVEL     dsdate        wdate
419800000000     c*
419900000000     c                   ENDSR
420000000000     c*--------------------------------------------------------------------------------------------*
420100000000     c* edita ora da hhmm in hh:mm
420200000000     c*--------------------------------------------------------------------------------------------*
420300000000     c     edtora        BEGSR
420400000000     c*
420500000000     c                   MOVEL     *blanks       worae
420600000000     c*
420700000000     c                   MOVEL     wora          dsora
420800000000     c                   MOVEL     dshh          dshhe
420900140617     c                   MOVEL     '.'           dsvo1
421000000000     c                   MOVEL     dsmm          dsmme
421100000000     c                   MOVEL     dsorae        worae
421200000000     c*
421300000000     c                   ENDSR
421400000000     c*--------------------------------------------------------------------------------------------*
421500000000     c* decodifica punto operativo
421600000000     c*--------------------------------------------------------------------------------------------*
421700000000     c     decorg        BEGSR
421800000000     c*
421900000000     c                   MOVEL     *blanks       wodes                          *descrizione
422000000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
422100000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
422200000000     c*
422300000000     c                   SETOFF                                           98
422400041111     c                   Z-ADD     1             O
422500041111     c     worg          LOOKUP    sorg(O)                                98
422600000000if  1c                   IF        *in98                                        *trovata
422700121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
422800121001     c                             %EDITC(wOrg : 'X') + ')'
422900041111     c                   MOVEL     sofl1(O)      wofl1
423000041111     c                   MOVEL     sodpd(O)      wodpd
423100000000e   1c                   ENDIF
423200000000     c*
423300000000     c                   ENDSR
423400000000     c*--------------------------------------------------------------------------------------------*
423500000000     c* decodifica evento per STATI
423600000000     c*--------------------------------------------------------------------------------------------*
423700000000     c     deccevsta     BEGSR
423800000000     c*
423900000000     c                   MOVEL     '0'           werr                           *NO errore
424000050704     c*
424100050704     C                   IF        wcev = 'GEN'
424200050704     C                   EVAL      werr = *ON
424300050704     C                   CLEAR                   wcdex
424400050704     C                   LEAVESR
424500050704     C                   ENDIF
424600000000     c*
424700060622     C                   RESET                   tibs02ds
424800060622     C                   EVAL      t02Cod = 'ICE'
424900060622     C                   EVAL      t02Ke1 = wcev
425000060622     C                   EXSR      getTntbe00f
425100060622     C                   IF        t02Err <> 'E' AND
425200140528     C                             (§icesta = 'S'  OR
425300140528     C                               §icestaP = 'S' )
425400060622     c                   MOVEL     §icedei       wcdex
425500000000x   1c                   ELSE                                                   *non trovato
425600000000     c                   MOVEL     '1'           werr                           *SI errore
425700030130     c                   MOVEL     *blanks       wcdex
425800000000e   1c                   ENDIF
425900000000     c*
426000000000     c                   ENDSR
426100000000     c*--------------------------------------------------------------------------------------------*
426200000000     c* decodifica consegna particolare
426300000000     c*--------------------------------------------------------------------------------------------*
426400000000     c     decftc        BEGSR
426500000000     c*
426600000000     c                   MOVEL     *blanks       wdftc
426700000000     c*
426800000000if  1c                   IF        wftc = 'S'
426900060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
427000000000e   1c                   ENDIF
427100000000if  1c                   IF        wftc = 'P'
427200060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
427300000000e   1c                   ENDIF
427400000000if  1c                   IF        wftc = 'A'
427500060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
427600000000e   1c                   ENDIF
427700000000if  1c                   IF        wftc = 'F'
427800060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
427900000000e   1c                   ENDIF
428000000000     c*
428100000000     c                   ENDSR
428200000000     c*--------------------------------------------------------------------------------------------*
428300000000     c* decodifica turno di chiusura
428400000000     c*--------------------------------------------------------------------------------------------*
428500000000     c     decgcx        BEGSR
428600000000     c*
428700000000     c                   MOVEL     *blanks       wdgcx
428800000000     c* giorno
428900000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
429000000000if  1c                   IF        a1 = '1'
429100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
429200000000e   1c                   ENDIF
429300000000if  1c                   IF        a1 = '2'
429400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
429500000000e   1c                   ENDIF
429600000000if  1c                   IF        a1 = '3'
429700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
429800000000e   1c                   ENDIF
429900000000if  1c                   IF        a1 = '4'
430000060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
430100000000e   1c                   ENDIF
430200000000if  1c                   IF        a1 = '5'
430300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
430400000000e   1c                   ENDIF
430500000000if  1c                   IF        a1 = '6'
430600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
430700000000e   1c                   ENDIF
430800000000if  1c                   IF        a1 = '7'
430900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
431000000000e   1c                   ENDIF
431100000000if  1c                   IF        a1 = ' '
431200000000     c                   EVAL      wdgcx = *blanks
431300000000e   1c                   ENDIF
431400000000     c* am/pm
431500000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
431600000000if  1c                   IF        a1 = ' '
431700060612     c                   EVAL      wdgcx =
431800060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
431900000000e   1c                   ENDIF
432000000000if  1c                   IF        a1 = 'M'
432100060612     c                   EVAL      wdgcx =
432200060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
432300000000e   1c                   ENDIF
432400000000if  1c                   IF        a1 = 'P'
432500060612     c                   EVAL      wdgcx =
432600060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
432700000000e   1c                   ENDIF
432800000000     c*
432900000000     c                   ENDSR
433000000000     c*--------------------------------------------------------------------------------------------*
433100000000     c* decodifica evento per GIACENZE
433200000000     c*--------------------------------------------------------------------------------------------*
433300000000     c     deccevgia     BEGSR
433400000000     c*
433500000000     c                   MOVEL     '0'           werr                           *NO errore
433600000000     c*
433700060622     C                   RESET                   tibs02ds
433800060622     C                   EVAL      t02Cod = 'ICE'
433900060622     C                   EVAL      t02Ke1 = wcev
434000060622     C                   EXSR      getTntbe00f
434100060622     C                   IF        t02Err <> 'E' AND                            *trovata
434200140528     C                             (§icegia = 'S'  OR
434300140528     C                               §icegiap = 'S')
434400060622     c                   MOVEL     §icedei       wcdex
434500000000x   1c                   ELSE                                                   *non trovato
434600000000     c                   MOVEL     '1'           werr                           *SI errore
434700030130     c                   MOVEL     *blanks       wcdex
434800000000e   1c                   ENDIF
434900000000     c*
435000000000     c                   ENDSR
435100000000     c*--------------------------------------------------------------------------------------------*
435200000000     c* Controlla la DS dsi Input
435300000000     c*--------------------------------------------------------------------------------------------*
435400000000     c     chkdsinput    BEGSR
435500000000     c*
435600000000     c                   MOVEL     '0'           werr                           *NO errore
435700000000     c*
435800000000     c* dalla DS di Input scompone il codice spedizione
435900000000     c                   MOVEL     nspedizi74    dsspedizi74
436000000000     c*
436100000000     c* controlla che la lina di partenza sia TUTTA numerica
436200000000     c                   SETOFF                                       99
436300000000     c                   TESTN                   dslnpi74             99
436400000000if  1c                   IF        NOT *in99                                    *non è un numero
436500000000     c                   MOVEL     '1'           werr                           *SI errore
436600140617     c                   eval      widmsg = 'TIS0769'
436700140617     c                   eval      widcampo = '*LNP'
436800140617     c                   clear                   wparm
436900140617     c                   exsr      messaggi
437000000000x   1c                   ELSE                                                   *è un numero
437100000000     c                   MOVE      dslnpi74      a1
437200000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
437300000000     c                   MOVEL     '1'           werr                           *SI errore
437400140617     c                   eval      widmsg = 'TIS0769'
437500140617     c                   eval      widcampo = '*LNP'
437600140617     c                   clear                   wparm
437700140617     c                   exsr      messaggi
437800000000e   2c                   ENDIF
437900000000e   1c                   ENDIF
438000000000     c*
438100000000     c* controlla che la serie spedizione sia TUTTA numerica
438200000000     c                   SETOFF                                       99
438300000000     c                   TESTN                   dsnrsi74             99
438400000000if  1c                   IF        NOT *in99                                    *non è un numero
438500000000     c                   MOVEL     '1'           werr                           *SI errore
438600140617     c                   eval      widmsg = 'TIS0769'
438700140617     c                   eval      widcampo = '*NRS'
438800140617     c                   clear                   wparm
438900140617     c                   exsr      messaggi
439000000000x   1c                   ELSE                                                   *è un numero
439100000000     c                   MOVE      dsnrsi74      a1
439200000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
439300000000     c                   MOVEL     '1'           werr                           *SI errore
439400140617     c                   eval      widmsg = 'TIS0769'
439500140617     c                   eval      widcampo = '*NRS'
439600140617     c                   clear                   wparm
439700140617     c                   exsr      messaggi
439800000000e   2c                   ENDIF
439900000000e   1c                   ENDIF
440000000000     c*
440100000000     c* controlla che il numero spedizione sia TUTTO numerico
440200000000     c                   SETOFF                                       99
440300000000     c                   TESTN                   dsnspi74             99
440400000000if  1c                   IF        NOT *in99                                    *non è un numero
440500000000     c                   MOVEL     '1'           werr                           *SI errore
440600140617     c                   eval      widmsg = 'TIS0769'
440700140617     c                   eval      widcampo = '*NSP'
440800140617     c                   clear                   wparm
440900140617     c                   exsr      messaggi
441000000000x   1c                   ELSE                                                   *è un numero
441100000000     c                   MOVE      dsnspi74      a1
441200000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
441300000000     c                   MOVEL     '1'           werr                           *SI errore
441400140617     c                   eval      widmsg = 'TIS0769'
441500140617     c                   eval      widcampo = '*NSP'
441600140617     c                   clear                   wparm
441700140617     c                   exsr      messaggi
441800000000e   2c                   ENDIF
441900000000e   1c                   ENDIF
442000110104     c*
442100110104     c* controlla che il codice cliente sia TUTTO numerico
442200110104     c                   IF        kscI74 <> *BLANK
442300110104     c                   SETOFF                                       99
442400110104     c                   TESTN                   kscI74               99
442500110104     c                   IF        NOT *in99                                    *non è un numero
442600110104     c                   MOVEL     '1'           werr                           *SI errore
442700140617     c                   eval      widmsg = 'TIS0769'
442800140617     c                   eval      widcampo = 'KSCI74'
442900140617     c                   clear                   wparm
443000140617     c                   exsr      messaggi
443100110104     c                   ELSE                                                   *è un numero
443200110104     c                   MOVE      kscI74        a1
443300110104     c                   IF        a1 < '0'                                     *trovata una lettera
443400110104     c                   MOVEL     '1'           werr                           *SI errore
443500140617     c                   eval      widmsg = 'TIS0769'
443600140617     c                   eval      widcampo = 'KSCI74'
443700140617     c                   clear                   wparm
443800140617     c                   exsr      messaggi
443900110104     c                   ENDIF
444000110104     c                   ENDIF
444100110104     c                   ENDIF
444200110104     c*
444300110104     c* controlla che il codice mittente sia TUTTO numerico
444400110104     c                   IF        ccmI74 <> *BLANK
444500110104     c                   SETOFF                                       99
444600110104     c                   TESTN                   ccmI74               99
444700110104     c                   IF        NOT *in99                                    *non è un numero
444800110104     c                   MOVEL     '1'           werr                           *SI errore
444900140617     c                   eval      widmsg = 'TIS0769'
445000140617     c                   eval      widcampo = 'CCMI74'
445100140617     c                   clear                   wparm
445200140617     c                   exsr      messaggi
445300110104     c                   ELSE                                                   *è un numero
445400110104     c                   MOVE      ccmI74        a1
445500110104     c                   IF        a1 < '0'                                     *trovata una lettera
445600110104     c                   MOVEL     '1'           werr                           *SI errore
445700140617     c                   eval      widmsg = 'TIS0769'
445800140617     c                   eval      widcampo = 'CCMI74'
445900140617     c                   clear                   wparm
446000140617     c                   exsr      messaggi
446100110104     c                   ENDIF
446200110104     c                   ENDIF
446300110104     c                   ENDIF
446400050104     c*
446500050104     c* controlla anno spedizione
446600050104     c                   TESTN                   cAASI74              99
446700050104if  1c                   IF        NOT *in99                                    *non è un numero
446800050104     c                   EVAL      cAASI74 = *ZERO
446900050104x   1c                   ENDIF
447000100222     c*
447100100223     c* Estraggo l'anno dalla data spedizione.
447200100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
447300100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
447400100223     C                   IF        dspI74 <> *BLANK
447500100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
447600100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
447700100223     C                   MONITOR
447800100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
447900100223     C                   ON-ERROR
448000100223     C                   ENDMON
448100100222     C                   ENDIF
448200000000     c*
448300000000     c* se errore, esce dal programma
448400140610if  1c                   IF        werr = '1'                                   *SI errore
448500140610     c                   eval      esito = werr                                 *errore
448600140610e   1c                   ENDIF
448700000000     c*
448800000000     c                   ENDSR
448900000000     c*--------------------------------------------------------------------------------------------*
449000000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
449100000000     c* Input:   Walfa7 = Alfanumerico di 7
449200000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
449300000000     c*--------------------------------------------------------------------------------------------*
449400000000     c     xallinea1     BEGSR
449500000000     c*
449600000000     c* reimposta variabli di lavoro
449700000000     c                   CLEAR                   i
449800000000     c                   CLEAR                   z
449900000000     c                   CLEAR                   walfa7bis
450000000000     c                   EVAL      wzeri = *ALL'0'
450100000000     c*
450200000000     c     ' '           CHECKR    walfa7:7      i
450300000000if  1c                   IF        i <> 0
450400000000     c     i             SUBST     walfa7        walfa7bis
450500000000e   1c                   ENDIF
450600000000if  1c                   IF        walfa7bis <> *blanks
450700000000     c     7             SUB       i             z
450800000000if  2c                   IF        z <> 0
450900000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
451000000000e   2c                   ENDIF
451100000000     c                   EVAL      z = z+ 1
451200000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
451300000000e   1c                   ENDIF
451400000000     c*
451500000000     c                   ENDSR
451600000000     c*--------------------------------------------------------------------------------------------*
451700000000     c* caricamento tabelle occorrenti
451800000000     c*--------------------------------------------------------------------------------------------*
451900000000     c     cartab        BEGSR
452000000000     c*---
452100000000     c* organigrama
452200000000     c*---
452300000000     c                   CLEAR                   i                              *indice
452400000000     C                   CLEAR                   sorg
452500000000     C                   CLEAR                   sodes
452600000000     C                   CLEAR                   sofl1
452700000000     C                   CLEAR                   sodpd
452800060623     C                   OPEN      tntbe01l
452900000000     c     *loval        SETLL     azorg01l
453000000000     c                   READ      azorg01l                               99
453100000000do  1c                   DOW       NOT *in99
453200000000     c                   ADD       1             i
453300000000     c                   Z-ADD     orgfil        sorg(i)
453400000000     c                   MOVEL     orgdes        sodes(i)
453500000000     c                   MOVEL     orgde3        og143
453600020702     C*
453700020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
453800020702     C                   CLEAR                   dntw
453900020702     C*
454000020702     C                   MOVEL(P)  'NTW'         kbecod
454100020702     C                   MOVEL(P)  §ogntw        kbeke1
454200020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
454300020702     c                   MOVEL     *blanks       kbelin                         *lingua
454400020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
454500020702     C     keytbe        CHAIN     tntbe01l
454600020702     C                   IF        %found(tntbe01l)
454700020702     C                   MOVEL     tbeuni        dntw
454800020702     C                   ENDIF
454900020702     C*
455000020702     C                   MOVEL     §ntwfie       sofl1(i)
455100160406     C*
455200020702     c                   MOVEL     §ogntw        sodpd(i)
455300160406     C*
455400000000     c                   READ      azorg01l                               99
455500000000e   1c                   ENDDO
455600060623     C                   CLOSE     tntbe01l
455700000000     c*---
455800000000     c                   ENDSR
455900000000     c*--------------------------------------------------------------------------------------------*
456000000000     c* operazioni iniziali -da fare sempre-
456100000000     c*--------------------------------------------------------------------------------------------*
456200000000     c     rutinz        BEGSR
456300000000     c*
456400000000     c* reperimento data corrente
456500000000     C                   TIME                    n14                            *ora (6)+ data (8)
456600000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
456700000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
456800000000     C                   Z-ADD     n8            g08dat
456900000000     C                   Z-ADD     *zeros        g08inv
457000000000     C                   MOVEL     '0'           g08err
457100000000     c                   CALL      'XSRDA8'
457200000000     c                   PARM                    wlbda8
457300000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
457400000000     C                   MOVEL     datcor        anncor                         *anno corrente
457500000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
457600000000     c*
457700000000     c* calcola data corrente - 1 anno
457800000000     c                   Z-ADD     datcor        datpre                         *data precedente
457900000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
458000000000     c*
458100010716     c* azzera la DS di Output e le variabili di lavoro / controllo
458200000000     c                   MOVEL     '0'           esito
458300140603     c                   reset                   tis778dso                      *ds Output
458400140603     c                   reset                   tis778dsm                      *ds Output
458500000000     c                   CLEAR                   smi                            *di memorizzazione
458600000000     c                   CLEAR                   smseql
458700000000     c                   CLEAR                   smseqe
458800000000     c                   CLEAR                   smdevhev
458900000000     c                   CLEAR                   smdeve
459000000000     c                   CLEAR                   smheve
459100000000     c                   CLEAR                   smfle
459200000000     c                   CLEAR                   smdev
459300000000     c                   CLEAR                   smcev
459400130312     c                   CLEAR                   smspe
459500000000     c                   CLEAR                   smdmc
459600000000     c                   CLEAR                   smdmcR
459700000000     c                   CLEAR                   tote                           *contatore eventi
459800000000     c                   CLEAR                   seql                           *sequenza legami
459900010716     c                   RESET                   $AbTD                          *di lavoro
460000010716     c                   RESET                   $AbFirma
460100150312     c                   clear                   pdaavv
460200150421     c                   clear                   pdagia
460300140603     c* parametri ingresso
460400140603     c                   exsr      Kontrolla
460500000000     c* controlla da DS in Input
460600000000     c                   EXSR      chkdsinput
460700000000     c*
460800000000     c* carica i clienti dettaglio del cliente unificante
460900000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
461000000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
461100000000     c     keyvss        SETLL     tivss02l
461200000000     c     keyvss        READE     tivss02l                               99
461300000000do  1c                   DOW       NOT *in99
461400000000if  2c                   IF        vssvat = *blanks                             *attivo
461500000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
461600000000     c                             datcor <= vssdsc
461700010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
461800010719     c* --> TD per vedere la firma DPD
461900010716     c                   RESET                   $AbTD                          *di lavoro
462000010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
462100010719     c     kssksu        SETLL     tivss02l
462200010719     c     kssksu        READE     tivss02l                               99
462300010719do  4c                   DOW       NOT *in99
462400010716if  5c                   IF        vssvat = *blanks                             *attivo
462500010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
462600010716     c                             datcor <= vssdsc
462700010719if  7c                   IF        vssisv = 'TD'
462800010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
462900010719e   7c                   ENDIF
463000010719e   6c                   ENDIF
463100010716e   5c                   ENDIF
463200010719     c     kssksu        READE     tivss02l                               99
463300010719e   4c                   ENDDO
463400010716     c*
463500000000     c                   LEAVE                                                  *uscita ciclo
463600000000e   3c                   ENDIF
463700000000e   2c                   ENDIF
463800000000     c     keyvss        READE     tivss02l                               99
463900010716e   1c                   ENDDO                                                  *fine cl.abil.TT
464000000000     c*
464100060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
464200060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
464300151216     c* Se lo strategi user number è = *blanks --> lo imposto a *zeros
464400151216     c                   monitor
464500151216     c                   if        RQSCIDI174>0
464600151216     c                   endif
464700151216     c
464800151216     c                   on-error
464900151216     c                   clear                   RQSCIDI174
465000151216     c                   endmon
465100060621     c*
465200000000     c                   ENDSR
465300040330
465400040330     ***********************************************************************************************
465500040330     **?
465600040330     **?Reperimento ulteriori dati destinatario.
465700040330     **?
465800040330     ***********************************************************************************************
465900040330     C     ImpDSDest     BEGSR
466000040330
466100140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
466200140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
466300100121     C                             ar5Trd <> 'GEN'
466400140716     c                   Z-ADD     f_tasaas      kr5aas
466500140716     c                   Z-ADD     f_taslnp      kr5lnp
466600140716     c                   Z-ADD     f_tasnrs      kr5nrs
466700140716     c                   Z-ADD     f_tasnsp      kr5nsp
466800040330     c                   MOVEL     'GEN'         kr5trd
466900040531     c     keyar5        CHAIN     fiar531c
467000040330     C                   IF        %FOUND
467100040330     C                   EVAL      DAR5GEN = AR5Uni
467200100121     C                   ELSE
467300100121     C                   RESET                   DAR5GEN
467400040330     C                   ENDIF
467500100121     C                   ENDIF
467600100121     C
467700100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
467800100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
467900040330
468000040330     C                   ENDSR
468100040913
468200040913     ***********************************************************************************************
468300040913     **?
468400040913     **?Determino se addebitare l'immagine LDV.
468500040913     **?
468600040913     ***********************************************************************************************
468700040913     C     AddebitoLDV   BEGSR
468800040913
468900040913     C                   IF        LDVO74 = 'S'
469000040913
469100040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
469200040913
469300040913     C                   ENDIF
469400040913
469500040913     C                   ENDSR
469600051110
469700051110     ***********************************************************************************************
469800051110     **?
469900051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
470000051110     **?
470100051110     ***********************************************************************************************
470200051110     C     chkTivpi      BEGSR
470300051110
470400051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
470500051110     C                             esito10)
470600051110     C                   IF        esito10 > 0 AND NOT %ERROR
470700051110     C                   EVAL      gcfdio74 = 'N'
470800051110     C                   ENDIF
470900051110
471000051110     C                   ENDSR
471100051222
471200051222     ***********************************************************************************************
471300051222     **
471400051222     ** Linea arrivo Italia o estera.
471500051222     **
471600051222     ***********************************************************************************************
471700051222     C     setLnaItaEst  BEGSR
471800051222
471900051222     C                   RESET                   wrkLnaItaEst
472000051222     C                   EXSR      decOrg
472100051222     C                   EVAL      wrkLnaItaEst = wofl1
472200051222
472300051222     C                   ENDSR
472400060622
472500060622     ***********************************************************************************************
472600150615     ** Reperisco informazioni stato bolla
472700060622     ***********************************************************************************************
472800130312     C     srarb         BEGSR
472900130312     c                   clear                   wcev
473000140714     c                   clear                   wricons
473100130312     c                   clear                   worg
473200130312     c                   clear                   wdat
473300130312     c                   clear                   wora
473400150615     c                   clear                   ds2a
473500130312     c                   eval      dsevbspe = spe(1)
473600130312     c     karb          chain     fnarb01l
473700130312     c                   if        not %error and
473800130312     c                             %found(fnarb01l)
473900130422     c                   Z-ADD     arblna        worg
474000130312     c                   exsr      setLnaItaEst
474100130312     c* no per estero
474200130312     C                   IF        wrkLnaItaEst = ESTERO
474300130312     c                   leavesr
474400130312     c                   end
474500150617     c
474600130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
474700130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
474800130312     C                   SELECT
474900141120     c                   WHEN      arbdcm <> 0
475000130312     c                   MOVEL     '704'         wcev                           *causale evento
475100130313     c                   move      arbdcm        WDAT
475200130313     c                   move      arbhmc        WORA
475300150617     c
475400130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
475500150615     c                   WHEN      ARBFDC = ' ' and arbndc>0
475600130326     c* solo se distinta NON in test
475700130326     c                   clear                   ddstflr
475800130326     c                   z-add     arbifp        dstfgs
475900130326     c                   z-add     arbndc        dstnfv
476000130326     c     kdst          chain     fidst01l
476100130326     c                   if        not %error and
476200130326     c                             %found(fidst01l)
476300130326     c                   eval      ddstflr = dstflr
476400130326     c                   end
476500130327     c                   if        §DSTTSTPDA = 'C' or
476600130327     c                             §DSTTSTPDA = 'E'
476700130326     c                   leavesr
476800130326     c                   end
476900130326     c*
477000150617     c* Cerco PRIMO evento da PdA
477100130313     c                   move      arbndc        pctndc
477200131105     c                   exsr      cercaceP
477300150617     c*
477400150617     c* Cerco ULTIMO evento da PdA
477500131105     c                   move      'CET'         ttrd
477600130312     c     kcet          setgt     fipct02l
477700130312     c     kcet          readpe    fipct02l
477800130312     c                   if        not %error and
477900130312     c                             not %eof(fipct02l)
478000130312     c                   movel     pctdati       fipctcetds
478100140926      * firmatario se c'è riempie il campo
478200140926     c                   if        §pctnomfir <> *blank
478300140926     c                   eval      firmato74  = §pctnomfir
478400140926     c                   endif
478500130312     c* consegnata
478600130312     c                   if        §pctcmc = ' '
478700130312     c                   MOVEL     '704'         wcev                           *causale evento
478800130312     c                   else
478900140711     c                   move      'S'           wricons
479000130312     c* mancata consegna
479100130313     c                   movel     §PCTcmc       wcev
479200130312     c                   clear                   ds2a
479300130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
479400130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
479500130312     C                             :rpyEsito)
479600130312     c                   if        §2aflg = '§' and §2acpt = 'S'
479700130312     c                   move      'T  '         wcev
479800130312     c                   endif
479900130312     c                   endif
480000130312     c* ??? £6
480100130312     c                   if        §pctdata > 0
480200130313     c                   move      §PCTDATA      WDAT
480300130312     c                   movel     §PCTORA       WORA
480400130312     c                   endif
480500130312     c                   end
480600130312     c                   endSL
480700130312     c                   end
480800130312     c* ho trovato un evento da inserire  al 1° posto
480900130312     c                   if        wcev <> ' '
481000130312     c                   EXSR      deccevsta
481100130312     c                   IF        werr <> '1'                                  *no errore
481200130312     c                   EXSR      decorg
481300130312     c                   EXSR      edtdat
481400130312     c                   EXSR      edtora
481500130313     c* libero il 1° elemnto di schiera
481600150617     c                   eval      j = 49
481700150617     c                   for       j = 49 downto 1
481800130312     c                   if        cev(j) <> ' '
481900130312     c                   eval      cev(j+1) = cev(j)
482000130312     c                   eval      dev(j+1) = dev(j)
482100130312     c                   eval      hev(j+1) = hev(j)
482200130312     c                   eval      fle(j+1) = fle(j)
482300130312     c                   eval      eve(j+1) = eve(j)
482400130312     c                   end
482500130313     c                   endfor
482600130313     c                   eval      dev(1) = wdate
482700130313     c                   eval      hev(1) = worae
482800130312     c                   eval      cev(1)= wcev
482900130313     c                   eval      fle(1) = wodes
483000130313     c                   eval      eve(1) = wcdex
483100130313     c                   EVAL      cntEveO74 += 1
483200150310     c*
483300150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
483400150310     c                   if        §2aftc='A'
483500150310     c                   eval      pdaavv='S'
483600150310     c                   endif
483700150421     c                   if        §2aftc='G'
483800150421     c                   eval      pdagia='S'
483900150421     c                   endif
484000150310     c
484100130312     c                   end
484200130312     c                   end
484300130312     C                   ENDSR
484400140714     **_________________________________________________________
484500140714     c     cercafiglia   begsr
484600140714     **_________________________________________________________
484700140715     c                   clear                   keyForzata
484800140714      * salva chiave originale per eventi
484900140715     c                   eval      kasaasf = kasaas
485000140715     c                   eval      kaslnpf = kaslnp
485100140715     c                   eval      kasnrsf = kasnrs
485200140715     c                   eval      kasnspf = kasnsp
485300140714     c                   do        *hival
485400140715     c     keytasf       chain     fnlbl02l
485500140714     c                   if        %found(fnlbl02l)
485600140715     c                   eval      kasaasf = LBLaan
485700140715     c                   eval      kaslnpf = LBLlpn
485800140715     c                   eval      kasnrsf = LBLnrn
485900140715     c                   eval      kasnspf = LBLnsn
486000140714     c                   move      'X'           keyForzata
486100140714     c                   iter
486200140714     c                   else
486300140714     c                   leave
486400140714     c                   endif
486500140714     c                   enddo
486600140715     c     keytasf       setll     titas30c                               99    *no errore
486700140715     c     keytasf       READE     titas30c                               99    *no errore
486800141117      *se non trova titas con figlia forza di nuovo la mamma
486900140715if  3c                   IF        *in99                                        *non trovato
487000141117     c                   clear                   titasdssavf
487100140715x   3c                   ELSE                                                   *trovata
487200140715if  4c                   IF        *in01                                        *lettura titas000
487300140715     c                   EVAL      titasdssavf = titasds
487400140715e   4c                   ENDIF
487500140715if  4c                   IF        *in02                                        *lettura titas010
487600140715     c                   EVAL      titasdssavf = titasds
487700140715e   4c                   ENDIF
487800140715if  4c                   IF        *in03                                        *lettura titasp00
487900140715     c                   EVAL      titasdssavf = titasds
488000140715e   4c                   ENDIF
488100140715e   3c                   ENDIF
488200140714     C                   ENDSR
488300160318     **_________________________________________________________
488400160318     c     cercamamma    begsr
488500160318     **_________________________________________________________
488600160318     c                   clear                   keyForzata
488700160318     c                   EVAL      kblaan = arbaas
488800160318     c                   EVAL      kbllpn = arblnp
488900160318     c                   EVAL      kblnrn = arbnrs
489000160318     c                   EVAL      kblnsn = arbnsp
489100160318     c     keylbl        CHAIN     fnlbl01l                           99
489200160318if  1c                   IF        NOT *in99                                    *ha la mamma
489300160318     c                   EVAL      kasaas = lblaap
489400160318     c                   EVAL      kaslnp = lbllpp
489500160318     c                   EVAL      kasnrs = lblnrp
489600160318     c                   EVAL      kasnsp = lblnsp
489700160318     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
489800160318if  3c                   IF        not *in99                                        *non trovato
489900160318     c                   move      'X'           keyForzata
490000160318     c                   eval      esito=' '
490100160408     c                   clear                   titasdssavf
490200160318     c                   endif
490300160318     c                   endif
490400160318     c
490500160318     c                   ENDSR
490600131105     **_________________________________________________________
490700131105     c     cercaCEP      begsr
490800131105     **_________________________________________________________
490900131105     c                   move      'CEP'         ttrd
491000131105     c     kcet          setgt     fipct02l
491100131105     c     kcet          readpe    fipct02l
491200131105     c                   if        not %error and
491300131105     c                             not %eof(fipct02l)
491400131105     c                   movel     pctdati       fipctcetds
491500131105     c* consegnata
491600131105     c                   if        §pctcmc = ' '
491700131105     c                   MOVEL     '704'         wcev                           *causale evento
491800131105     c                   else
491900131105     c* mancata consegna
492000131105     c                   movel     §PCTcmc       wcev
492100131105     c                   clear                   ds2a
492200131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
492300131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
492400131105     C                             :rpyEsito)
492500131105     c                   if        §2aflg = '§' and §2acpt = 'S'
492600131105     c                   move      'T  '         wcev
492700131105     c                   endif
492800131105     c                   endif
492900131105     c* ??? £6
493000131105     c                   if        §pctdata > 0
493100131105     c                   move      §PCTDATA      WDAT
493200131105     c                   movel     §PCTORA       WORA
493300131105     c                   endif
493400131105     c                   end
493500131105     c* ho trovato un evento da inserire  al 1° posto
493600131105     c                   if        wcev <> ' '
493700131105     c                   EXSR      deccevsta
493800131105     c                   IF        werr <> '1'                                  *no errore
493900131105     c                   EXSR      decorg
494000131105     c                   EXSR      edtdat
494100131105     c                   EXSR      edtora
494200131105     c* libero il 1° elemnto di schiera
494300150617     c                   eval      j = 49
494400150617     c                   for       j = 49 downto 1
494500131105     c                   if        cev(j) <> ' '
494600131105     c                   eval      cev(j+1) = cev(j)
494700131105     c                   eval      dev(j+1) = dev(j)
494800131105     c                   eval      hev(j+1) = hev(j)
494900131105     c                   eval      fle(j+1) = fle(j)
495000131105     c                   eval      eve(j+1) = eve(j)
495100131105     c                   end
495200131105     c                   endfor
495300131105     c                   eval      dev(1) = wdate
495400131105     c                   eval      hev(1) = worae
495500131105     c                   eval      cev(1)= wcev
495600131105     c                   eval      fle(1) = wodes
495700131105     c                   eval      eve(1) = wcdex
495800131105     c                   EVAL      cntEveO74 += 1
495900131105     c                   end
496000131105     c                   end
496100131105     c                   endsr
496200131105     ***********************************************************************************************
496300130312     **
496400130312     ** Reperisco tabella da TNTBE00F.
496500130312     **
496600130312     ***********************************************************************************************
496700130312     C     getTntbe00f   BEGSR
496800060622     C                   EVAL      t02Lin = langTntbe
496900060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
497000060622     C                   SELECT
497100060622     C                   WHEN      t02Cod = 'ICE'
497200060622     C                   EVAL      dice = t02Uni
497300060622     C                   WHEN      t02Cod = 'CCH'
497400060622     C                   EVAL      dcch = t02Uni
497500060622     C                   WHEN      t02Cod = 'NTW'
497600060622     C                   EVAL      dntw = t02Uni
497700100119     C                   WHEN      t02Cod = 'I1A'
497800100119     C                   EVAL      dI1a = t02Uni
497900150304     C                   WHEN      t02Cod = 'SPC'
498000150304     C                   EVAL      dSPC = t02Uni
498100170301     C                   WHEN      t02Cod = 'CLI'
498200170301     C                   EVAL      dcli = t02Uni
498300060622     C                   ENDSL
498400060622     C                   ENDSR
498500060608
498600140529      /free
498700140529       //--------------------------------------------------------------
498800140529       //?Controllo formale dei dati passati.
498900140529       //--------------------------------------------------------------
499000140603       BEGSR  Kontrolla;
499100140529
499200140529       //?OpCode
499300140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
499400160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
499500140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
499600160818           esito='1'    ;
499700140529           exsr RoutEnd;
499800140529         ENDIF;
499900140603           IF  prmRqsDataSize > 0;
500000140603           ELSE;
500100160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
500200140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
500300160818             esito='1'    ;
500400140603             exsr RoutEnd;
500500140603           ENDIF;
500600140529
500700140529       //?Formato e size RQS
500800140603           IF  prmRqsFormato = formatoi74;
500900140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
501000140529           ELSE;
501100160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
501200140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
501300160818             esito='1'    ;
501400140529             exsr RoutEnd;
501500140529           ENDIF;
501600140529         //?Formato e size RPY
501700140603           IF  prmRpyFormato = formatoo74;
501800140529           ELSE;
501900160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
502000140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
502100160818             esito='1'    ;
502200140529             exsr RoutEnd;
502300140529           ENDIF;
502400140529           IF  prmRpyDataSize > 0;
502500140529           ELSE;
502600160818             // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
502700140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
502800160818             esito='1'    ;
502900140529             exsr RoutEnd;
503000140529           ENDIF;
503100140616       //?Formato e size MSG
503200140617         if %parms > 9 ;
503300140617          IF  prmRpyFormMsg = formato;
503400140617          ELSE;
503500160818           // prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
503600140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
503700160818           esito='1'    ;
503800140616           exsr RoutEnd;
503900140617          ENDIF;
504000140617          IF  prmRpyMsgSize > 0;
504100140617          ELSE;
504200160818           // prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
504300140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
504400160818           esito='1'    ;
504500140616           exsr RoutEnd;
504600140617          ENDIF;
504700140617         ENDIF;
504800140529
504900140529       ENDSR;
505000140529
505100140616       //--------------------------------------------------------------
505200140617       //?Routine per schierare i messaggi nella ds tis778dsm
505300140616       //--------------------------------------------------------------
505400140616       BEGSR  Messaggi;
505500140616
505600140616       //?Se ho già caricato 99 messaggi esco
505700140616         IF  nrmsg = 99;
505800160818           esito='1'  ;
505900140616           exsr RoutEnd;
506000140616           clear nrmsg;
506100140616         ENDIF;
506200140616
506300140616         nrmsg += 1;
506400140616         skIdMsg(nrmsg) = wIdMsg;
506500140616         skIdCampo(nrmsg) = wIdCampo;
506600140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
506700140616         IF  wParm <> *blanks;
506800140616           skTextMsg (nrmsg) =
506900140616           rtvMsgLang(wIdMsg:langi74:wParm);
507000140616         ELSE;
507100140616           skTextMsg (nrmsg)=
507200140616           rtvMsgLang(wIdMsg:langi74);
507300140616         ENDIF;
507400140616
507500140616       ENDSR;
507600140616
507700140529       //--------------------------------------------------------------
507800140529       BEGSR  RoutEnd;
507900140530
508000140530
508100160322        if  esito <> '0' and esito<>'P';
508200140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
508300140530             else;
508400140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
508500140530        endif ;
508600140529
508700140530       //?ritorno
508800140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
508900140617         if %parms > 9 ;
509000140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
509100140617         endif;
509200140529         return;
509300140529
509400140529       ENDSR;
509500140529      /end-free
509600000000     c*--------------------------------------------------------------------------------------------*
509700000000     c* operazioni iniziali
509800000000     c*--------------------------------------------------------------------------------------------*
509900000000     c     *inzsr        BEGSR
510000000000     c*
510100000000     c* ricevimento parametri
510200140603     c*m   *ENTRY        PLIST
510300140529     c*m                 PARM                    esito
510400140529     c*m                 PARM                    tis174dsi
510500140529     c*m                 PARM                    tis174dso
510600000000     c*
510700000000     c* chiavi di lettura
510800000000     c     keytas        KLIST                                                  titas30c
510900000000     c                   KFLD                    kasaas                         -anno spedizione
511000000000     c                   KFLD                    kaslnp                         -linea partenza
511100000000     c                   KFLD                    kasnrs                         -serie
511200000000     c                   KFLD                    kasnsp                         -spedizione
511300140715     c     keytasf       KLIST                                                  titas30c
511400140715     c                   KFLD                    kasaasf                        -anno spedizione
511500140715     c                   KFLD                    kaslnpf                        -linea partenza
511600140715     c                   KFLD                    kasnrsf                        -serie
511700140715     c                   KFLD                    kasnspf                        -spedizione
511800130312     c     karb          KLIST                                                  titas30c
511900140716     c                   KFLD                    f_tasaas                         -anno spedizione
512000140716     c                   KFLD                    f_taslnp                         -linea partenza
512100140716     c                   KFLD                    f_tasnrs                         -serie
512200140716     c                   KFLD                    f_tasnsp                         -spedizione
512300130326     C     Kdst          KLIST
512400130326     C                   KFLD                    dstnpg
512500130326     C                   KFLD                    dstnfv
512600130326     C                   KFLD                    dstfgs
512700130326     c                   z-add     4             dstnpg
512800130326     C     Kcet          KLIST
512900130326     C                   KFLD                    arbLNA
513000130326     C                   KFLD                    pctNDC
513100130326     C                   KFLD                    arbpdc
513200131105     C                   KFLD                    TTRD              3
513300130312     C                   KFLD                    arbAAS
513400130312     C                   KFLD                    arbLNP
513500130312     C                   KFLD                    arbNRS
513600130312     C                   KFLD                    arbNSP
513700150403     C     KARP          KLIST
513800150403     C                   KFLD                    arbAAS
513900150403     C                   KFLD                    arbLNP
514000150403     C                   KFLD                    arbNRS
514100150403     C                   KFLD                    arbNSP
514200000000     c     keytaa        KLIST                                                  titaa30c
514300000000     c                   KFLD                    kaaaas                         -anno spedizione
514400000000     c                   KFLD                    kaalnp                         -linea partenza
514500000000     c                   KFLD                    kaanrs                         -serie
514600000000     c                   KFLD                    kaansp                         -spedizione
514700000000     c                   KFLD                    kaatrc                         -tipo anagrafica
514800000000     c     keyta4        KLIST                                                  tita430c
514900000000     c                   KFLD                    ka4aas                         -anno spedizione
515000000000     c                   KFLD                    ka4lnp                         -linea partenza
515100000000     c                   KFLD                    ka4nrs                         -serie
515200000000     c                   KFLD                    ka4nsp                         -spedizione
515300000000     c                   KFLD                    ka4trc                         -tipo riferimento
515400010925     c     ke2ta4        KLIST                                                  tita430c
515500010925     c                   KFLD                    ka4aas                         -anno spedizione
515600010925     c                   KFLD                    ka4lnp                         -linea partenza
515700010925     c                   KFLD                    ka4nrs                         -serie
515800010925     c                   KFLD                    ka4nsp                         -spedizione
515900000000     c     keyevb        KLIST                                                  fnevb01l
516000000000     c                   KFLD                    kvbaas                         -anno spedizione
516100000000     c                   KFLD                    kvblnp                         -linea partenza
516200000000     c                   KFLD                    kvbnrs                         -serie
516300000000     c                   KFLD                    kvbnsp                         -spedizione
516400010717     c     ke2evb        KLIST                                                  fnevb01l
516500010717     c                   KFLD                    kvbaas                         -anno spedizione
516600010717     c                   KFLD                    kvblnp                         -linea partenza
516700010717     c                   KFLD                    kvbnrs                         -serie
516800010717     c                   KFLD                    kvbnsp                         -spedizione
516900010717     c                   KFLD                    kvbdev                         -data evento
517000010717     c                   KFLD                    kvbhev                         -ora  evento
517100000000     c     keylbl        KLIST                                                  fnlbl01l
517200000000     c                   KFLD                    kblaan                         -anno seguente
517300000000     c                   KFLD                    kbllpn                         -linea seguente
517400000000     c                   KFLD                    kblnrn                         -serie seguente
517500000000     c                   KFLD                    kblnsn                         -spedizione seguente
517600000000     c     ke2lbl        KLIST                                                  fnlbl02l
517700000000     c                   KFLD                    kblaap                         -anno precedente
517800000000     c                   KFLD                    kbllpp                         -linea precedente
517900000000     c                   KFLD                    kblnrp                         -serie precedente
518000000000     c                   KFLD                    kblnsp                         -spediz.precedente
518100000000     c     keytbe        KLIST                                                  *tntbe01l
518200000000     c                   KFLD                    kbecod                          -tabella
518300000000     c                   KFLD                    kbeke1                          -chiave uno
518400000000     c                   KFLD                    kbeke2                          -chiave due
518500000000     c                   KFLD                    kbelin                          -lingua
518600000000     c                   KFLD                    kbesif                          -s.informativo
518700000000     c     keyvss        KLIST                                                  *tivss02l
518800000000     c                   KFLD                    kssksu                          -cl.unificante
518900000000     c                   KFLD                    kssisv                          -tipo servizio
519000010717     c     keyvtd        KLIST                                                  *tivtd01l
519100010717     c                   KFLD                    ktdksu                          -cl.unificante
519200000000     c     keycsb        KLIST                                                  *tncsb03l
519300000000     c                   KFLD                    ksbaas                         -anno spedizione
519400000000     c                   KFLD                    ksblnp                         -linea partenza
519500000000     c                   KFLD                    ksbnrs                         -serie
519600000000     c                   KFLD                    ksbnsp                         -spedizione
519700040906     c     keydct        KLIST
519800040906     c                   KFLD                    DCTAAC
519900040906     c                   KFLD                    DCTFIL
520000040906     c                   KFLD                    DCTNCA
520100000000     c     keygcp        KLIST                                                  *figcp01l
520200000000     c                   KFLD                    kcpaas                         -anno spedizione
520300000000     c                   KFLD                    kcplnp                         -linea partenza
520400000000     c                   KFLD                    kcpnrs                         -serie
520500000000     c                   KFLD                    kcpnsp                         -spedizione
520600000000     c                   KFLD                    kcpfrg                         -prg aperture
520700050404     c     K04GCP51      KLIST
520800050404     c                   KFLD                    kcpaas                         -anno spedizione
520900050404     c                   KFLD                    kcplnp                         -linea partenza
521000050404     c                   KFLD                    kcpnrs                         -serie
521100050404     c                   KFLD                    kcpnsp                         -spedizione
521200000000     c     keygnp        KLIST                                                  *fignp01l
521300000000     c                   KFLD                    knpagc                         -anno apertura
521400000000     c                   KFLD                    knpfgc                         -filiale apertura
521500000000     c                   KFLD                    knpngc                         -numero giacenza
521600000000     c                   KFLD                    knpfrg                         -prg apertura
521700040531     c     keyar5        KLIST                                                  *fiar531c
521800030206     c                   KFLD                    kr5aas                         -anno spedizione
521900030206     c                   KFLD                    kr5lnp                         -linea partenza
522000030206     c                   KFLD                    kr5nrs                         -serie
522100030206     c                   KFLD                    kr5nsp                         -spedizione
522200030206     c                   KFLD                    kr5trd                         -tipo record
522300170301     c     ktfntc        KLIST                                                  titas30c
522400170301     c                   KFLD                    k_NTCAPL
522500170301     c                   KFLD                    k_NTCnk1
522600170301     c                   KFLD                    k_NTCnk2
522700170301     c                   KFLD                    k_NTCtnt
522800040907
522900000000     c* apre i files
523000000000     c                   OPEN      titas30c
523100000000     c                   OPEN      titaa30c
523200000000     c                   OPEN      tita430c
523300000000     c                   OPEN      fnevb01l
523400000000     c                   OPEN      tivss02l
523500010716     c                   OPEN      tivtd01l
523600000000     c                   OPEN      azorg01l
523700170301     c                   OPEN      tfntc01l
523800000000     c                   OPEN      fnlbl01l
523900000000     c                   OPEN      fnlbl02l
524000050309     c                   OPEN      tigcp51l
524100050309     c                   OPEN      tignp01l
524200000000     c                   OPEN      tncsb03l
524300040906     c                   OPEN      fndct01l
524400040531     c                   OPEN      fiar531c
524500130312     c                   OPEN      fnarb01l
524600160127     c                   OPEN      fnblp01l
524700160309     c                   OPEN      fiar901l
524800150403     c                   OPEN      fiarp01l
524900140606     c                   OPEN      fiar501l
525000140606     c                   OPEN      fiar401l
525100130326     c                   OPEN      fidst01l
525200130312     c                   OPEN      fipct02l
525300060621     C                   CALLP     openTabel00f()
525400000000     c*
525500000000     c* caricamento tabelle occorrenti
525600000000     c                   EXSR      cartab
525700060621     C                   CALLP     inzLingue()
525800000000     c*
525900000000     c                   ENDSR
526000070109
526100070109     ***********************************************************************************************
526200070109     **
526300070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
526400070109     **
526500070109     ***********************************************************************************************
526600070109     C     eventoAnnulla BEGSR
526700070109
526800070109     C                   IF        §iceEvAn <> *BLANK
526900070109     C                   FOR       j = 1 TO %ELEM(smCev)
527000070109     C                   IF        §iceEvAn = smCev(j) AND
527100070109     C                             evbDevHev = smDevHev(j)
527200070109     C                   CLEAR                   smCev(j)
527300130312     C                   CLEAR                   smspe(j)
527400070109     C                   CLEAR                   smDevHev(j)
527500070109     C                   CLEAR                   smi(j)
527600070109     C                   CLEAR                   smSeql(j)
527700070109     C                   CLEAR                   smSeqe(j)
527800070109     C                   CLEAR                   smDeve(j)
527900070109     C                   CLEAR                   smHeve(j)
528000070109     C                   CLEAR                   smFle(j)
528100070109     C                   CLEAR                   smDev(j)
528200070109     C                   LEAVE
528300070109     C                   ENDIF
528400070109     C                   ENDFOR
528500070109     C                   ENDIF
528600070109
528700070109     C                   ENDSR
528800080131
528900100119     P*--------------------------------------------------
529000100119     P* Procedure name: GetDescrizioneIncassoContrassegno
529100100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
529200100119     P*                          o del contrassegno.
529300100121     P* Returns:        Descrizione modalità incasso contrassegno
529400100119     P*--------------------------------------------------
529500100119     P GetDescrizioneIncassoContrassegno...
529600100119     P                 B
529700100119     D GetDescrizioneIncassoContrassegno...
529800100119     D                 PI            35A
529900100119
530000100121     C                   RESET                   tibs02ds
530100100121     C                   EVAL      t02Cod = 'I1A'
530200100121     C                   EVAL      t02Lin = langTntbe
530300100119     ** Il contrassegno è stato incassato
530400100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
530500100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
530600100121     C                   ELSE
530700100119     ** Il contrassegno non è stato incassato.
530800100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
530900100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
531000100119     C                             ar5Trd <> 'GEN'
531100100121     C                   EVAL      kr5Aas = csbAas
531200100121     C                   EVAL      kr5Lnp = csbLnp
531300100121     C                   EVAL      kr5Nrs = csbNrs
531400100121     C                   EVAL      kr5Nsp = csbNsp
531500100119     C                   EVAL      kr5Trd = 'GEN'
531600160314     c                   if        esito='P'
531700160314     C     keyAr5        CHAIN     fiar501l
531800160314     c                   else
531900160314     C     keyAr5        CHAIN     fiar531c
532000160314     c                   endif
532100160314     c
532200100119     C                   IF        %FOUND()
532300100119     C                   EVAL      dAr5Gen = ar5Uni
532400100119     C                   ELSE
532500100119     C                   RESET                   dAr5Gen
532600100119     C                   RETURN                  *BLANK
532700100119     C                   ENDIF
532800100119     C                   ENDIF
532900100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
533000100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
533100100119     C                   ELSE
533200100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
533300100119     C                   ENDIF
533400100121     C                   ENDIF
533500100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
533600100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
533700100119     C                   IF        t02Err = 'E'
533800100119     C                   RETURN                  *BLANK
533900100119     C                   ENDIF
534000100120     C                   EVAL      dI1a = t02Uni
534100140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
534200140417     C                   RETURN                  dI1a.descrizi2
534300140417     C                   ENDIF
534400100119     C                   RETURN                  dI1a.descrizion
534500100119
534600100119     P GetDescrizioneIncassoContrassegno...
534700100119     P                 E
534800100119
534900110304
535000110304     P*--------------------------------------------------
535100110304     P* Procedure name: IsContrassegnoCompensato
535200110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
535300110304     P*                          to con una fattura.
535400110304     P* Returns:        *ON = Compensato
535500110304     P*--------------------------------------------------
535600110304     P IsContrassegnoCompensato...
535700110304     P                 B
535800110304     D IsContrassegnoCompensato...
535900110304     D                 PI              N
536000110304
536100110304     D retField        S               N   INZ(*OFF)
536200110304
536300110307     ** Il contrassegno non è stato rimborsato.
536400110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
536500110307     C                   RETURN    retField
536600110307     C                   ENDIF
536700110307
536800110307     C/EXEC SQL
536900110307     C+ SELECT '1'
537000110307     C+ INTO :retField
537100110307     C+ FROM tncsv00f
537200110307     C+ WHERE csvAas = :csbAas
537300110307     C+   AND csvLnp = :csbLnp
537400110307     C+   AND csvNrs = :csbNrs
537500110307     C+   AND csvNsp = :csbNsp
537600110307     C+   AND csvCav = 'COMP'
537700110307     C+ FETCH FIRST ROW ONLY
537800110307     C/END-EXEC
537900110304
538000110307     C                   SELECT
538100110307     C                   WHEN      sqlCode < 0
538200110307     C                   DUMP(A)
538300110307     C                   ENDSL
538400110304
538500110307     C                   RETURN    retField
538600110304
538700110304     P IsContrassegnoCompensato...
538800110304     P                 E
538900110304
539000120321
539100120321     P*--------------------------------------------------
539200120321     P* Procedure name: SetIdAssegnoMittente
539300120321     P* Purpose:        Imposta l'ID assegno mittente.
539400120321     P* Returns:        Esito.
539500120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
539600120321     P*                          segni mittente.
539700120321     P* Parameter:      priAbi => ABI assegno.
539800120321     P* Parameter:      priCab => CAB assegno.
539900120321     P*--------------------------------------------------
540000120321     P SetIdAssegnoMittente...
540100120321     P                 B
540200120321     D SetIdAssegnoMittente...
540300120321     D                 PI            10I 0
540400120321     D  priNumeroAssegno...
540500120321     D                                     LIKE(caNumO74)
540600120321     D  priAbi                             LIKE(caAbiO74)
540700120321     D  priCab                             LIKE(caCabO74)
540800120321
540900120321     D retField        S             10I 0 STATIC
541000120321
541100120321      /FREE
541200120321
541300120321       CLEAR retField;
541400120321
541500120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
541600120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
541700120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
541800120321       // In questo caso restituisco l'ID del primo assegno.
541900120321
542000120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
542100120321         RETURN retField;
542200120321       ENDIF;
542300120321
542400120321       EXEC SQL
542500120321         SELECT csmNra, csmAbi, csmCab
542600120321           INTO :priNumeroAssegno, :priAbi, :priCab
542700120321           FROM tncsm00f
542800120321           WHERE csmKey = :priNumeroAssegno
542900120321           FETCH FIRST 1 ROW ONLY
543000120321       ;
543100120321
543200120321       SELECT;
543300120321         WHEN sqlCode = 100;
543400120321           retField = sqlCode;
543500120321         WHEN sqlCode < *ZERO;
543600120321           DUMP(A);
543700120321           retField = sqlCode;
543800120321       ENDSL;
543900120321
544000120321       RETURN retField;
544100120321
544200120321      /END-FREE
544300120321     P SetIdAssegnoMittente...
544400120321     P                 E
544500120321
544600130513
544700130513     P*--------------------------------------------------
544800130513     P* Procedure name: GetFilUrl
544900130513     P* Purpose:        Restituisce l'URL della filiale.
545000130513     P* Returns:        URL filiale.
545100130513     P* Parameter:      piFiliale => ID filiale
545200130513     P*--------------------------------------------------
545300130513     P GetFilUrl       B
545400130513     D GetFilUrl       PI           256A
545500130513     D  piFiliale                     3P 0 CONST
545600130513
545700130513     C                   IF        piFiliale = *ZERO
545800130513     C                   RETURN    *BLANK
545900130513     C                   ENDIF
546000130513     C                   RESET                   tibs02ds
546100130513     C                   EVAL      t02Cod = 'UFI'
546200130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
546300130513     C                   EVAL      t02Lin = langTntbe
546400130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
546500130513     C                   IF        t02Err = 'E'
546600130513     C                   RETURN    *BLANK
546700130513     C                   ENDIF
546800130513     C                   RETURN    t02Uni
546900130513
547000130513     P GetFilUrl       E
547100130513
