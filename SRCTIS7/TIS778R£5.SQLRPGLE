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
003600150403     ffiarp01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIARP01L')
003700140226     ffipct02l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIPCT02L')
003800140226     ffidst01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIDST01L')
003900140605     ffiar501l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIAR501L')
004000140606     f                                     RENAME(fiar5000:fiar5f)
004100140605     ffiar401l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIAR401L')
004200000000     f*---
004300000000     f* Giacenze
004400000000     f*---
004500050309     ftigcp51l  IF   E           K DISK    USROPN
004600050309     ftignp01l  IF   E           K DISK    USROPN
004700000000     f*---
004800000000     f* Contrassegni
004900000000     f*---
005000000000     ftncsb03l  IF   E           K DISK    USROPN
005100000000     f*---
005200000000     f* Danni testata CA
005300000000     f*---
005400040906     ffndct01l  IF   E           K DISK    USROPN
005500030206     f*---
005600030206     f* Estensione testata bolle
005700030206     f*---
005800040531     ffiar531c  IF   E           K DISK    USROPN
005900000000     f*---
006000000000     f* Clienti abilitati
006100000000     f*---
006200000000     ftivss02l  IF   E           K DISK    USROPN
006300010716     f*---
006400010716     f* Estensione DPD
006500010716     f*---
006600010716     ftivtd01L  if   e           k disk    USROPN
006700000000     f*---
006800000000     f* Tabelle -NEW-
006900000000     f*---
007000000000     ftntbe01l  IF   E           K DISK    USROPN
007100000000     f*---
007200000000     f* Organigramma
007300000000     f*---
007400000000     fazorg01l  IF   E           K DISK    USROPN                               *organigramma
007500051110
007600051110     ***********************************************************************************************
007700051110     **?
007800051110     **?Definizione procedure.
007900051110     **?
008000051110     ***********************************************************************************************
008100051110     D GetSpeChkCde    PR            10U 0
008200051110     D  Anno                          4
008300051110     D                                     VALUE
008400051110     D  IdSpedizione                 12
008500051110     D                                     VALUE
008600051110     D  ChkCode                      10U 0
008700051110     D  nEsito                        5I 0
008800051110     D tis7653r        PR
008900051110     D                                     EXTPGM('TIS7653R')
009000051110     D  ksu
009100051110     D                                     LIKE(kscI74)
009200051110     D  agc
009300051110     D                                     LIKE(gcpAgc)
009400051110     D  fgc
009500051110     D                                     LIKE(gcpFgc)
009600051110     D  ngc
009700051110     D                                     LIKE(gcpNgc)
009800051110     D  esito10                      10I 0
009900060612     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
010000060612     D  msgId                         7A   CONST
010100060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
010200060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
010300060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
010400060612     D                                     VARYING
010500060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
010600060612     D                                     VARYING
010700060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
010800060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
010900060621     D inzLingue       PR            10I 0
011000060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
011100060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
011200060621     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
011300060621     D cvtLinguaISO2ToTabel...
011400060621     D                 PR                  LIKE(linTabel)
011500060621     D  rqsISO2                            LIKE(linISO2)
011600060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
011700060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
011800060621     D  rpyTabel                           LIKE(linTabel)
011900060621     D                                     OPTIONS(*NOPASS:*OMIT)
012000060621     D cvtLinguaISO2ToTntbe...
012100060621     D                 PR                  LIKE(linTntbe)
012200060621     D  rqsISO2                            LIKE(linISO2)
012300060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
012400060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012500060621     D  rpyTntbe                           LIKE(linTntbe)
012600060621     D                                     OPTIONS(*NOPASS:*OMIT)
012700060621     D openTabel00f    PR
012800060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
012900060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
013000060626     D chainTabel00f   PR           109A
013100060626     D  rqsOpCode                    10A   CONST
013200060626     D  rqsKut                        1P 0
013300060626     D  rqsCod                        2A   CONST
013400060626     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
013500060626     D  rqsLengthOfKey...
013600060626     D                               10I 0 CONST
013700060626     D  rqsFormat                    10A   CONST
013800060626     D  rpyOpCode                    10A
013900060626     D  rpyEsito                     10I 0
014000060626     D  rpyData                     109A   OPTIONS(*NOPASS:*OMIT)
014100060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
014200060622     D  kpjba                              LIKEDS(kpjba)
014300060622     D  tibs02ds                           LIKEDS(tibs02ds)
014400090828     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
014500090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7702R
014600090828
014700051222     ***********************************************************************************************
014800051222     **?
014900051222     **?Definizione costanti.
015000051222     **?
015100051222     ***********************************************************************************************
015200101104     D ITALIA          C                   ' '
015300101104     D ESTERO          C                   'E'
015400101104     D EVENTO_ARRIVATA_IN_FILIALE...
015500101104     D                 C                   '703'
015600101104     D EVENTO_MESSA_IN_CONSEGNA...
015700101104     D                 C                   'MIC'
015800150612     D EVENTO_ARRIVATA_HUB...
015900150612     D                 C                   '712'
016000150612     D EVENTO_ARRIVATA_PARTNER...
016100150612     D                 C                   '713'
016200051110
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
017600140606     d dar5emd       e ds                  inz prefix(§)
017700140605     D tnsd99ds      E DS                  INZ
017800140606     D tibs02ds      E DS                  INZ
017900060622     D  t02Mod       E                     INZ('C')                             Controllo
018000090828     D tis7700i0     E DS                  QUALIFIED
018100090828     D                                     INZ(*EXTDFT)
018200090828     D tis7700o0     E DS                  QUALIFIED
018300090828     d*---
018400000000     d* Ridenominazione formato record -TITAS-
018500000000     d*---
018600000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
018700000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
018800000000     d                                     PREFIX(s_)
018900140715     d titasdssavF   E DS                  EXTNAME(titas00f)                    *depisito titas
019000140715     d                                     PREFIX(f_)
019100080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
019200080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
019300080131     D                                     BASED(titas000FigliaPtr)
019400080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
019500080131     D                                     BASED(titas000FigliaPtr)
019600080131     d*---
019700000000     d* Variabili riferite al data base
019800000000     d*---
019900141126     d kasaasm         S                   LIKE(tasaas)                         *lettura titas30c
020000141126     d kaslnpm         S                   LIKE(taslnp)
020100141126     d kasnrsm         S                   LIKE(tasnrs)
020200141126     d kasnspm         S                   LIKE(tasnsp)
020300140715     d kasaasf         S                   LIKE(tasaas)                         *lettura titas30c
020400140715     d kaslnpf         S                   LIKE(taslnp)
020500140715     d kasnrsf         S                   LIKE(tasnrs)
020600140715     d kasnspf         S                   LIKE(tasnsp)
020700000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
020800000000     d kaslnp          S                   LIKE(taslnp)
020900000000     d kasnrs          S                   LIKE(tasnrs)
021000000000     d kasnsp          S                   LIKE(tasnsp)
021100000000     d kastbl          S                   LIKE(tastbl)
021200000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
021300000000     d kaalnp          S                   LIKE(taalnp)
021400000000     d kaanrs          S                   LIKE(taanrs)
021500000000     d kaansp          S                   LIKE(taansp)
021600000000     d kaatrc          S                   LIKE(taatrc)
021700000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
021800000000     d ka4lnp          S                   LIKE(ta4lnp)
021900000000     d ka4nrs          S                   LIKE(ta4nrs)
022000000000     d ka4nsp          S                   LIKE(ta4nsp)
022100000000     d ka4trc          S                   LIKE(ta4trc)
022200000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
022300000000     d kvblnp          S                   LIKE(evblnp)
022400000000     d kvbnrs          S                   LIKE(evbnrs)
022500000000     d kvbnsp          S                   LIKE(evbnsp)
022600000000     d kvbdev          S                   LIKE(evbdev)
022700000000     d kvbhev          S                   LIKE(evbhev)
022800000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
022900000000     d kcplnp          S                   LIKE(gcplnp)
023000000000     d kcpnrs          S                   LIKE(gcpnrs)
023100000000     d kcpnsp          S                   LIKE(gcpnsp)
023200000000     d kcpfrg          S                   LIKE(gcpfrg)
023300000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
023400000000     d knpfgc          S                   LIKE(gnpfgc)
023500000000     d knpngc          S                   LIKE(gnpngc)
023600000000     d knpfrg          S                   LIKE(gnpfrg)
023700000000     d knpfas          S                   LIKE(gnpfas)
023800000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
023900000000     d ksblnp          S                   LIKE(csblnp)
024000000000     d ksbnrs          S                   LIKE(csbnrs)
024100000000     d ksbnsp          S                   LIKE(csbnsp)
024200000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
024300000000     d kbllpn          S                   LIKE(lbllpn)
024400000000     d kblnrn          S                   LIKE(lblnrn)
024500000000     d kblnsn          S                   LIKE(lblnsn)
024600000000     d kblaap          S                   LIKE(lblaap)
024700000000     d kbllpp          S                   LIKE(lbllpp)
024800000000     d kblnrp          S                   LIKE(lblnrp)
024900000000     d kblnsp          S                   LIKE(lblnsp)
025000000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
025100000000     d kctlnp          S                   LIKE(dctlnp)
025200000000     d kctnrs          S                   LIKE(dctnrs)
025300000000     d kctnsp          S                   LIKE(dctnsp)
025400040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
025500030206     d kr5lnp          S                   LIKE(ar5lnp)
025600030206     d kr5nrs          S                   LIKE(ar5nrs)
025700030206     d kr5nsp          S                   LIKE(ar5nsp)
025800030206     d kr5trd          S                   LIKE(ar5trd)
025900000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
026000000000     d kssisv          S                   LIKE(vssisv)
026100010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
026200000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
026300000000     d kbeke1          S                   LIKE(tbeke1)
026400000000     d kbeke2          S                   LIKE(tbeke2)
026500000000     d kbelin          S                   LIKE(tbelin)
026600000000     d kbesif          S                   LIKE(tbesif)
026700000000     d waas            S                   LIKE(tasaas)                         *di lavoro
026800000000     d wlnp            S                   LIKE(taslnp)
026900000000     d wnrs            S                   LIKE(tasnrs)
027000000000     d wnsp            S                   LIKE(tasnsp)
027100000000     d wfca            S                   LIKE(tasfca)
027200000000     d wfgc            S                   LIKE(tasfgc)
027300000000     d wfpr            S                   LIKE(dctfpr)
027400000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
027500000000     d deplpn          S                   LIKE(lbllpn)
027600000000     d depnrn          S                   LIKE(lblnrn)
027700000000     d depnsn          S                   LIKE(lblnsn)
027800010716     d depute          S                   LIKE(vtdute)
027900010716     d deppwd          S                   LIKE(vtdpwd)
028000030130     d savtsp          S                   LIKE(tastsp)                         *di salvataggio
028100000000     d lcca            S                   LIKE(tascca)                         *di lavoro
028200000000     d lduc            S                   LIKE(tasduc)
028300000000     d llnp            S                   LIKE(taslnp)
028400000000     d ldti            S                   LIKE(tasdti)
028500000000     d lhti            S                   LIKE(tashti)
028600000000     d ldrt            S                   LIKE(tasdrt)
028700000000     d llna            S                   LIKE(taslna)
028800000000     d ldcm            S                   LIKE(tasdcm)
028900000000     d lhmc            S                   LIKE(tashmc)
029000060621     d langTabel       S                   LIKE(tblkut)
029100060621     d langTntbe       S                   LIKE(tbelin)
029200140108     d wDev            S                   LIKE(evbdev)
029300140108     d wHev            S                   LIKE(evbhev)
029400150611     d wDEE            S                   LIKE(d98dee)
029500140616     d wParm           s            512a   inz
029600150403     D w_dallemsg      S              5a
029700150403     D w_allemsg       S              5a
029800150403     D w_dalle         S               t
029900150403     D w_alle          S               t
030000150403     D w_dalleoser     S               t
030100150403     D w_alleoser      S               t
030200000000     d*---
030300000000     d* Ds
030400000000     d*---
030500000000      * controllo data
030600000000     d wlbda8          DS                  INZ                                  *controlla data
030700000000     d  g08dat                        8  0                                       -data dritta
030800000000     d  g08inv                        8  0                                       -data invertita
030900000000     d  g08err                        1                                          -errore
031000000000     d  g08tgi                        5  0                                       -giorni fra date
031100000000      * scomposizione data generica
031200000000     d dsdat           DS                  INZ                                  *data
031300000000     d  dsann                         4  0                                       -anno
031400000000     d  dsmes                         2  0                                       -mese
031500000000     d  dsgio                         2  0                                       -giorno
031600000000      * scomposizione data generica (anno + mese/giorno)
031700000000     d                 DS                  INZ                                  *data aa + mm/gg
031800000000     d  dsannB                 1      4  0                                       -anno
031900000000     d  dsmgsB                 5      8  0                                       -mese/giorno
032000000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
032100000000      * composizione data generica editata
032200000000     d dsdate          DS                  INZ                                  *data
032300000000     d  dsgioe                        2  0                                       -giorno
032400000000     d  dsvd1                         1                                          -'/'
032500000000     d  dsmese                        2  0                                       -mese
032600000000     d  dsvd2                         1                                          -'/'
032700000000     d  dsanne                        4  0                                       -anno
032800000000      * scomposizione ora generica
032900000000     d dsora           DS                  INZ                                  *ora
033000000000     d  dshh                          2  0                                       -ore
033100000000     d  dsmm                          2  0                                       -minuti
033200000000      * composizione ora generica editata
033300000000     d dsorae          DS                  INZ                                  *ora
033400000000     d  dshhe                         2  0                                       -ore
033500000000     d  dsvo1                         1                                          -':'
033600000000     d  dsmme                         2  0                                       -minuti
033700030206      * lettura FIAR5 - record bancali
033800030206     d dar5ban       E DS
033900030206      * lettura FIAR5 - record supermercati
034000030206     d dar5scr       E DS
034100040330     d DAR5GEN       E DS
034200040330     D                                     INZ
034300040330     D                                     PREFIX(GEN)
034400000000      * tipi servizio
034500000000     d ds5e          E DS
034600000000      * disposizioni giacenza
034700000000     d ds2d          E DS
034800000000      * causale eventi
034900000000     d dice          E DS
035000000000      * causali chiusura CA
035100000000     d dcch          E DS
035200100119     d dI1a          E DS                  QUALIFIED
035300000000      * consegne anomale
035400000000     d ds7o          E DS
035500130312      * eventi
035600130312     d ds2a          E DS
035700000000      * riferimenti
035800000000     d dta4a         E DS
035900070927     d dsbl4e        E DS
036000060627      * Estensione blp/arb tipo record "I"
036100060627     d dsbl4i        E DS
036200040913      * Addebito immagine POD
036300040913     d DLVA1         E DS
036400040913     D                                     INZ
036500000000      * lettura campo orgde3 di AZORG00F
036600000000     d og143         E DS
036700000000      * stati contrassegno
036800000000     d ds4s          E DS
036900141014      * particolarità consegna
037000141014     d ds7r          E DS
037100031217      * DS lettura campo TASFLO di TITAS
037200031217     d dtasflo       E DS
037300000000      * Ds ricerca cliente P.d.C.
037400000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
037500000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
037600000000     d indds         E DS                  EXTNAME(cnind00f) INZ
037700000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
037800000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
037900150304      * stato data prev cons
038000150304     d dspc          E DS
038100150312     d
038200150312     d xgiolavds     e ds
038300000000      * reperimento clienti da cliente unificante
038400090903     d*tibs10ds      E DS
038500090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
038600000000      * scomposizione codice spedizione -DS Input-
038700000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
038800000000     d  dslnpi74                      3                                          -linea partenza
038900000000     d  dsnrsi74                      2                                          -serie
039000000000     d  dsnspi74                      7                                          -spedizione
039100000000      * composizione codice spedizione -bolle-
039200000000     d                 DS
039300000000     d  dstaslnp               1      3  0                                       -linea partenza
039400000000     d  dstasnrs               4      5  0                                       -serie
039500000000     d  dstasnsp               6     12  0                                       -spedizione
039600000000     d dstasspe                1     12                                         *ds cod spedizione
039700000000      * composizione data + ora evento
039800080131     d                 DS                  INZ
039900080131     d  evbdev                 1      8  0                                       -data evento
040000080131     d  evbhev                 9     12  0                                       -ora  evento
040100080131     d evbDevHev               1     12  0                                      *ds data+ora evento
040200130312      * composizione spedizione evento
040300130312     d                 DS                  INZ
040400130312     d  evbaas                 1      4  0                                       -data evento
040500130312     d  evblnp                 5      7  0                                       -ora  evento
040600130312     d  evbnrs                 8      9  0                                       -ora  evento
040700130312     d  evbnsp                10     16  0                                       -ora  evento
040800130312     d dsevbspe                1     16  0                                      *ds data+ora evento
040900060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
041000000000      * ordinamento schiere eventi
041100000000     d                 DS
041200000000     d  dsseql                 1      3  0                                       -sequenza legame
041300000000     d  dsseqe                 4      6  0                                       -sequenza evento
041400000000     d  dsdevhev               7     18  0                                       -data + ora
041500000000     d  dsi                   19     21  0                                       -indice
041600000000     d dssort                  1     21  0                                      *ds cod spedizione
041700000000     d* Input routine XALLINEA
041800000000     d walfa7          S              7                                         *alfanumerico 7
041900000000     d walfa7bis       S              7                                         *alfanumerico 7
042000000000     d wzeri           S              7                                         *alfa con zeri
042100000000     d*---
042200000000     d* Variabili
042300000000     d*---
042400101104     d i               S             10I 0                                      *indici
042500101104     d ii              S             10I 0
042600101104     d j               S             10I 0
042700101104     d jj              S             10I 0
042800101104     d z               S             10I 0
042900101104     d s               S             10I 0
043000101104     d k               S             10I 0
043100101104     d o               S             10I 0
043200101104     d tote            S             10I 0                                      *totale eventi memor
043300000000     d seql            S              5  0                                      *sequenza legami
043400000000     d n14             S             14  0                                      *Numerico 14
043500000000     d n8              S              8  0                                      *Numerico 8
043600000000     d n7              S              7  0                                      *Numerico 7
043700000000     d n4              S              4  0                                      *Numerico 4
043800010927     d n3              S              3  0                                      *Numerico 3
043900000000     d a1              S              1                                         *Alfanumerico 1
044000000000     d a3              S              3                                         *Alfanumerico 3
044100000000     d a5              S              5                                         *Alfanumerico 5
044200010717     d a8              S              8                                         *Alfanumerico 8
044300000000     d a15             S             15                                         *Alfanumerico 15
044400000000     d a200            S            200                                         *Alfanumerico 200
044500000000     d a200bis         S            200                                         *Alfanumerico 200
044600010927     d a230            S            230                                         *Alfanumerico 230
044700000000     d a400            S            400                                         *Alfanumerico 400
044800000000     d datcor          S              8  0                                      *data corrente
044900000000     d datpre          S              8  0                                      *data precedente
045000000000     d oracor          S              6  0                                      *ora corrente
045100000000     d anncor          S              4  0                                      *anno corrente
045200000000     d annpre          S              4  0                                      *anno precedente
045300140617     d wIdMsg          s              7a
045400140617     d wIdCampo        s             10a
045500000000     d werr            S              1                                         *errore generico
045600000000     d wdat            S              8  0                                      *data (aaaammgg)
045700000000     d wdate           S             10                                         *data (gg/mm/aaaa)
045800000000     d wora            S              4  0                                      *ora  (hhmm)
045900000000     d worae           S              5                                         *ora  (hh:mm)
046000000000     d worg            S              3  0                                      *filiale
046100000000     d wodes           S             30                                         *descrizione filiale
046200000000     d wofl1           S              1                                         *flag italia/estero
046300020702     d wodpd           S              3                                         *flag filiale DPD
046400000000     d wcev            S              3                                         *causale evento
046500030130     d wcdex           S             30                                         *descrizione bar/pt
046600000000     d wpgm            S             20                                         *programma
046700000000     d wftc            S              1                                         *cons.partic.
046800140714     d wricons         S              1                                         *wrk per riconsegna
046900140715     d keyforzata      S              1                                         *wrk per riconsegna
047000000000     d wdftc           S             50                                         *descriz.cons.partic
047100000000     d d1wdftc         S             50                                         *descriz.cons.part.1
047200000000     d d2wdftc         S             50                                         *descriz.cons.part.2
047300000000     d wgcx            S              2                                         *turno chiusura
047400000000     d wdgcx           S             20                                         *descriz.turno chius
047500000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
047600000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
047700000000     d we              S              3                                         *descrizione 'e'
047800000000     d bollaorig       S              1                                         *bolla originaria
047900150310     d PDAavv          S              1                                         *evento AVV a PDA
048000150421     d PDAgia          S              1                                         *evento GIA a PDA
048100000000     d sostrit         S              1                                         sostituisce Ritiro
048200000000     d sostcon         S              1                                         sostituisce Consegna
048300000000     d newevb          S              1                                         *nuovo evento
048400000000     d lavdevhev       S             12  0                                      *data + ora
048500040913     D WrkCurDate      S               D                                        Data corrente
048600060620     D wrkDtInvDisp    S              8
048700060620     D wrkLnaItaEst    S              1
048800060620     D rpyOpCode       S             10A
048900060620     D rpyEsito        S             10I 0
049000060621     D nullPtr         S               *
049100080131     D titas000FigliaPtr...
049200080131     D                 S               *   INZ(%ADDR(titas000Figlia))
049300010716     d*---
049400010716     d* Variabili di controlli
049500010716     d*---
049600010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
049700010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
049800000000     d*---
049900000000     d* Variabili di memorizzazione
050000000000     d*---
050100000000     d mhcre           S              5                                         *consegna richiesta
050200000000     d mdcre           S             10
050300000000     d mtcre           S             50
050400000000     d mftce           S            100                                         *consegna partic.
050500000000     d mgcxe           S            100                                         *turno chiusura
050600000000     d mffd            S            100                                         *fermo deposito
050700010927     d mmncB           S             35                                         *NON consegna LAV
050800010927     d mmncC           S             35
050900010927     d mmncG           S             35                                         *NON consegna GIAC
051000010927     d mmncH           S             35
051100010927     d mmncN           S             35                                         *NON consegna NOFATT
051200010927     d mmncO           S             35
051300050701     d mmnc8           S             35
051400050701     d mmnc9           S             35
051500030206     d mtel            S             20                                         *N° tel destinatario
051600030206     d mban            S             35                                         *bancali
051700030207     d mscr            S            500                                         *supermercati
051800030210     d msdace          S             10
051900030210     d msorce          S              5
052000030210     d msdcre          S             10
052100030210     d mshcre          S              5
052200030210     d mstcre          S             30
052300030210     d msnom           S             50
052400030210     d mscrT           S            500    DIM(5)
052500030211     d wmscrT          S           2504
052600000000     d*---
052700000000     d* Schiere
052800000000     d*---
052900000000      * taabella clienti del cliente unificante
053000000000      * tabella tipi servizio
053100000000     d stsp            S              1    DIM(20)                              *tipi servizio
053200000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
053300000000      * tabella consegne anomale
053400030130     d scca            S              1    DIM(30)                              *consegna anomala
053500000000      * tabella disposizioni giacenza
053600000000     d sdis            S              1    DIM(10)                              *codice
053700000000     d sddis           S             20    DIM(10)                              *descrizione italian
053800000000      * tabella filiali
053900000000     d sorg            S              3  0 DIM(300)                             *filiale
054000000000     d sodes           S             30    DIM(300)                             *descrizione filiale
054100000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
054200020702     d sopt            S              3    DIM(300)                             *flag poste/no poste
054300020702     d sodpd           S              3    DIM(300)                             *flag DPD/no DPD
054400000000      * eventi memorizzati
054500000000     d smi             S              3  0 DIM(200)                             *indice
054600000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
054700000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
054800000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
054900000000     d smdeve          S             10    DIM(200)                             *data evento editata
055000000000     d smheve          S              5    DIM(200)                             *ora  evento editata
055100000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
055200000000     d smdev           S             30    DIM(200)                             *descrizione evento
055300000000     d smcev           S              3    DIM(200)                             *causale evento
055400130313     d smspe           S             16  0 DIM(200)                             *causale evento
055500000000      * eventi memorizzati x ordinamento
055600000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
055700000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
055800000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
055900000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
056000000000     d spdeve          S             10    DIM(200)                             *data evento editata
056100000000     d spheve          S              5    DIM(200)                             *ora  evento editata
056200000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
056300000000     d spdev           S             30    DIM(200)                             *descrizione evento
056400000000     d spcev           S              3    DIM(200)                             *causale evento
056500130313     d spspe           S             16  0 DIM(200)                             *causale evento
056600000000      * note giacenze memorizzate
056700051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
056800051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
056900000000      * note memorizzate
057000000000     d smnot           S            100    DIM(10)
057100030206     d*---
057200030206     d* definizioni per passaggio parametri
057300030206     d*---
057400000000     d esito           s              1
057500000000      * DS contenente i dati di INPUT
057600140603     d tis778dsi     e DS                  INZ(*EXTDFT)
057700050104     D  cAASI74                       4
057800050104     D                                     OVERLAY(AASI74)
057900000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
058000140530     d tis778dso     e DS                  INZ(*EXTDFT)
058100000000     d dev                           10    DIM(50) OVERLAY(dato74)
058200000000     d hev                            5    DIM(50) OVERLAY(orao74)
058300000000     d fle                           30    DIM(50) OVERLAY(opeo74)
058400041007     d eve                           30    DIM(50) OVERLAY(eveo74)
058500041007     d cev                            3    DIM(50) OVERLAY(cevo74)
058600000000     d nota                         100    DIM(10) OVERLAY(notao74)
058700130313     d spe             S             16  0 DIM(50)
058800040906
058900140616     d tis778dsM     e DS                  INZ(*EXTDFT)
059000140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
059100140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
059200140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
059300140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
059400140530      *
059500040906     D FIDN12DS      E DS
059600040906     D                                     INZ
059700040906     D  I12FEL       E
059800040906     D                                     INZ('E')
059900040906     D  I12FAN       E
060000040906     D                                     INZ('E')
060100040906     D  O12KeyAry                    14
060200040906     D                                     DIM(20)
060300040906     D                                     OVERLAY(O12KEY)
060400040906
060500040906     D O12KeyDS        DS
060600040906     D                                     INZ
060700040906     D  O12KeyAAC                     4S 0
060800040906     D  O12KeyFil                     3S 0
060900040906     D  O12KeyNCA                     7S 0
061000040906
061100020702      * DS lettura tabella "NTW"
061200020702     d DNTW          E DS
061300130326     d Ddstflr       E DS
061400041223
061500041223     D ChkCode         S             10U 0
061600041223     D nEsito          S              5I 0
061700051110     D esito10         S             10I 0
061800080131
061900100119     D*--------------------------------------------------
062000100119     D* Procedure name: GetDescrizioneIncassoContrassegno
062100100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
062200100119     D*                          o del contrassegno.
062300100121     D* Returns:        Descrizione modalità incasso contrassegno.
062400100119     D*--------------------------------------------------
062500100119     D GetDescrizioneIncassoContrassegno...
062600100119     D                 PR            35A
062700110304
062800110304     D*--------------------------------------------------
062900110304     D* Procedure name: IsContrassegnoCompensato
063000110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
063100110304     D*                          to con una fattura.
063200110304     D* Returns:        *ON = Compensato
063300110304     D*--------------------------------------------------
063400110304     D IsContrassegnoCompensato...
063500110304     D                 PR              N
063600120321
063700120321     D*--------------------------------------------------
063800120321     D* Procedure name: SetIdAssegnoMittente
063900120321     D* Purpose:        Imposta l'ID assegno mittente.
064000120321     D* Returns:        Esito.
064100120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
064200120321     D*                          segni mittente.
064300120321     D* Parameter:      priAbi => ABI assegno.
064400120321     D* Parameter:      priCab => CAB assegno.
064500120321     D*--------------------------------------------------
064600120321     D SetIdAssegnoMittente...
064700120321     D                 PR            10I 0
064800120321     D  priNumeroAssegno...
064900120321     D                                     LIKE(caNumO74)
065000120321     D  priAbi                             LIKE(caAbiO74)
065100120321     D  priCab                             LIKE(caCabO74)
065200130513
065300130513     D*--------------------------------------------------
065400130513     D* Procedure name: GetFilUrl
065500130513     D* Purpose:        Restituisce l'URL della filiale.
065600130513     D* Returns:        URL filiale.
065700130513     D* Parameter:      piFiliale => ID filiale
065800130513     D*--------------------------------------------------
065900130513     D GetFilUrl       PR           256A
066000130513     D  piFiliale                     3P 0 CONST
066100130513
066200130513
066300140527       //--------------------------------------------------------------
066400140527       //?Definizione prototipi.
066500140527       //--------------------------------------------------------------
066600140527      /copy gaitrasrc/srcprotopr,tis778R
066700140529      /copy gaitrasrc/srcconst,tis778R
066800140527       //--------------------------------------------------------------
066900140527       //?Definizione parametri programma.
067000140527       //--------------------------------------------------------------
067100140529     d tis778_GetBolla...
067200140527     d                 PI
067300140527     d prmRqsOpCode...
067400140527     d                               10I 0 CONST
067500140527     d prmRpyOpCode...
067600140527     d                               10I 0
067700140527     d prmRpyIdMsg...
067800140527     d                               10I 0
067900140527     d prmRqsFormato...
068000140527     d                               10A   CONST
068100140527     d prmRqsData...
068200140527     d                            32767A   OPTIONS(*VARSIZE)
068300140527     d prmRqsDataSize...
068400140527     d                               10I 0 CONST
068500140527     d prmRpyFormato...
068600140527     d                               10A   CONST
068700140527     d prmRpyData...
068800140527     d                            32767A   OPTIONS(*VARSIZE)
068900140527     d prmRpyDataSize...
069000140527     d                               10I 0 CONST
069100140527     d prmRpyFormMsg...
069200140527     d                               10A   CONST OPTIONS(*NOPASS)
069300140527     d prmRpyMessage...
069400140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
069500140527     d prmRpyMsgSize...
069600140527     d                               10I 0 CONST OPTIONS(*NOPASS)
069700080131
069800000000     i*--------------------------------------------------------------------------------------------*
069900000000     i* Input
070000000000     i*--------------------------------------------------------------------------------------------*
070100000000     i*---
070200000000     i* Indicatori lettura formati record -TITAS-
070300000000     i*---
070400000000     ititas000      01
070500000000     ititas010      02
070600000000     ititasp00      03
070700000000     c*--------------------------------------------------------------------------------------------*
070800000000     c* Main lines
070900000000     c*--------------------------------------------------------------------------------------------*
071000000000     c*
071100000000     c* operazioni iniziali -da fare sempre-
071200000000     c                   EXSR      rutinz
071300000000     c*
071400000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
071500000000if  1c                   IF        esito <> '1'                                 *no errori
071600100219     c*
071700100219     c                   MOVEL     dslnpi74      kaslnp
071800100219     c                   MOVEL     dsnrsi74      kasnrs
071900100219     c                   MOVEL     dsnspi74      kasnsp
072000100219     c*
072100111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
072200111103     c* restituisco errore.
072300111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
072400111103     c* e se non la trovo provo con l'anno precedente.
072500111103     c                   IF        aasI74 <> *ZERO
072600100219     c                   Z-ADD     aasI74        kasaas
072700100219     c     keytas        SETLL     titas30c                               99
072800111103     c                   IF        NOT *IN99
072900111103     c                   EVAL      esito = '1'
073000111103     c                   ENDIF
073100111103     c                   ELSE
073200000000     c                   Z-ADD     anncor        kasaas
073300000000     c     keytas        SETLL     titas30c                               99
073400000000if  2c                   IF        NOT *in99                                    *non trovato
073500000000     c                   Z-ADD     annpre        kasaas
073600000000     c     keytas        SETLL     titas30c                               99
073700000000if  3c                   IF        NOT *in99                                    *non trovato
073800000000     c                   MOVEL     '1'           esito                          *errore
073900000000e   3c                   ENDIF
074000000000e   2c                   ENDIF
074100100219     c                   ENDIF
074200140616     c                   if        esito = '1'
074300140617     c                   eval      widmsg = 'TIS0564'
074400140616     c                   eval      widcampo = 'NSPEDIZI74'
074500140616     c                   clear                   wparm
074600140616     c                   exsr      messaggi
074700140616     c                   endif
074800141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
074900141126     c                   eval      kasaasm = kasaas
075000141126     c                   eval      kaslnpm = kaslnp
075100141126     c                   eval      kasnrsm = kasnrs
075200141126     c                   eval      kasnspm = kasnsp
075300141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
075400140715      * ultima figlia per forzare i dati in output
075500140714     c                   exsr      cercafiglia
075600000000     c*
075700000000     c* se trovata, legge la bolla
075800000000if  2c                   IF        esito <> '1'
075900000000     C                   SETOFF                                       010203
076000140715     c     keytas        SETLL     titas30c
076100000000     c     keytas        READE     titas30c                               99    *no errore
076200000000if  3c                   IF        *in99                                        *non trovato
076300000000     c                   MOVEL     '1'           esito                          *errore
076400140617     c                   eval      widmsg = 'TIS0734'
076500140616     c                   eval      widcampo = 'NSPEDIZI74'
076600140616     c                   clear                   wparm
076700140616     c                   exsr      messaggi
076800000000x   3c                   ELSE                                                   *trovata
076900000000if  4c                   IF        *in01                                        *lettura titas000
077000000000     c                   EVAL      titasdssav = titasds
077100000000e   4c                   ENDIF
077200000000if  4c                   IF        *in02                                        *lettura titas010
077300000000     c                   EVAL      titasdssav = titasds
077400000000e   4c                   ENDIF
077500000000if  4c                   IF        *in03                                        *lettura titasp00
077600000000     c                   EVAL      titasdssav = titasds
077700000000e   4c                   ENDIF
077800141117      * se trovo la figlia ma ancora non è su titas
077900141117     c                   if        keyforzata <> *blank and
078000141117     c                             f_tasnsp = 0
078100141117     c                   eval      titasdssavf = titasdssav
078200141120     c                   else
078300141120     c                   clear                   keyforzata
078400141117     c                   endif
078500000000e   3c                   ENDIF
078600000000     c*
078700000000     c* controlla validità bolla
078800000000     c                   EXSR      chktas
078900000000     c*
079000030130     c* imposta i campi della DS di Output
079100000000if  3c                   IF        esito <> '1'                                 *no errore
079200140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
079300140714      * spedizione richiesta a video sempre
079400100225     c*
079500140716     c                   EVAL      dstaslnp = s_taslnp
079600140716     c                   EVAL      dstasnrs = s_tasnrs
079700140716     c                   EVAL      dstasnsp = s_tasnsp
079800140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
079900140723     c* chain ar5 con bolla richiesta a video
080000140723     c                   Z-ADD     s_tasaas      kr5aas
080100140723     c                   Z-ADD     s_taslnp      kr5lnp
080200140723     c                   Z-ADD     s_tasnrs      kr5nrs
080300140723     c                   Z-ADD     s_tasnsp      kr5nsp
080400100225     c                   MOVEL     'GEN'         kr5trd
080500100225     c     keyar5        CHAIN     fiar531c
080600100225     C                   IF        %FOUND
080700100225     C                   EVAL      DAR5GEN = AR5Uni
080800100225     C                   ELSE
080900100225     C                   RESET                   DAR5GEN
081000100225     C                   ENDIF
081100000000     c*
081200000000     c* imposta i campi della DS di Output dalle bolle
081300000000     c                   EXSR      impdsotas
081400000000     c*
081500000000     c* imposta i campi della DS di Output dagli eventi
081600000000     c                   EXSR      impdsoevb
081700000000     c*
081800000000     c* imposta i campi della DS di Output dal contrassegno
081900000000     c                   EXSR      impdsocsb
082000000000     c*
082100000000     c* imposta i campi della DS di Output dalla giacenza
082200000000     c                   EXSR      impdsogcp
082300000000     c*
082400000000     c* imposta i campi della DS di Output dalle note
082500000000     c                   EXSR      impdsonot
082600040330     c*
082700040330     c* imposta i campi della DS di Output del destinatario.
082800040330     c                   EXSR      ImpDSDest
082900010716     C*
083000010716     c* imposta i campi della DS di Output dalla firma DPD
083100010716     c                   EXSR      ImpdsoDPD
083200040913     C*
083300040913     c* imposta i campi della DS di Output di addebito immagine LDV.
083400040913     c                   EXSR      AddebitoLDV
083500000000e   3c                   ENDIF
083600000000e   2c                   ENDIF
083700140613e   1c                   ENDIF
083800140919      *riempie campi con riferimenti alla consegna
083900141016     c                   if        esito <> '1'
084000140919     c                   if        f_tasdcm > 0
084100140919     c                   movel     f_tasdcm      wdat
084200140919     c                   exsr      edtdat
084300140919     c                   eval      dtconmeo74  = wdate
084400140919     c                   MOVEL     f_tashmc      wora
084500140919     c                   EXSR      edtora
084600140919     c                   eval      orconmeO74  =  worae
084700140919     c                   eval      flgbolcO74  =  'S'
084800140919     c                   eval      flgboldO74  =  'N'
084900141014      *firmatario distinta PDA
085000141014     c                   clear                   firmato74
085100141015     c                   if        gen§ar5pdaco <> 'NO'
085200141014     c                   exsr      repfirmat
085300141014     c                   else
085400141014     c                   if        f_tasgma <> *blank
085500141015     C                   EVAL      tblKey = f_tasgma
085600141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
085700141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
085800141014     C                             :rpyOpCode:rpyEsito)
085900141014     C                   IF        rpyOpCode = 'FOUND'
086000141014     c                   if        §7r2fi = 'S'
086100141014     c                   exsr      repfirmat
086200141014     c                   endif
086300141014     c                   endif
086400141014     c                   endif
086500141014     c                   endif
086600141014      *else consegna
086700140919     c                   else
086800140919     c                   eval      flgbolcO74  =  'N'
086900140919     c                   if        F_tasndc > 0 and f_tasddc > 0
087000141119     c                             and f_tasddc <= datcor
087100140919     c                   eval      flgboldo74 = 'S'
087200140919     c                   else
087300140919     c                   eval      flgboldO74  =  'N'
087400140919     c                   endif
087500140919     c                   endif
087600130312     c* se ultimo evento inserito è un MIC (1° della schiera perchè
087700130312     c* è discendente) verifico situazione delle spedizione in filiale
087800140613if  3c                   IF        esito <> '1'                                 *no errore
087900130312     c                   IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
088000140609     c                             and cev(49) = ' ' and cev(50) = ' ' or
088100141120     c                             f_tasdcm = 0 and keyforzata = *blank
088200130312     c                   exsr      srarb
088300140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
088400140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
088500140605     c                   exsr      impdsoarb
088600140604     c                   endif
088700140613     c                   endif
088800140613     c                   endif
088900141016     c                   endif
089000000000     c*
089100140530     C                   exsr      routend
089200141014      *__________________________________________________________________
089300141014     c     repfirmat     begsr
089400141014      *__________________________________________________________________
089500141014     c                   Z-ADD     f_tasaas      ka4aas
089600141014     c                   Z-ADD     f_taslnp      ka4lnp
089700141014     c                   Z-ADD     f_tasnrs      ka4nrs
089800141014     c                   Z-ADD     f_tasnsp      ka4nsp
089900141014     c                   MOVEL     '1'           ka4trc
090000141014     c     keyta4        CHAIN     tita430c                           99
090100141014if  1c                   IF        NOT *in99
090200141014     c                   MOVEL     ta4not        firmato74
090300141014e   1c                   ENDIF
090400141014     c                   endsr
090500141015      *__________________________________________________________________
090600141015     c     repfirmatARB  begsr
090700141015      *__________________________________________________________________
090800141015      *firmatario da arb
090900141015     c                   Z-ADD     arbaas        ka4aas
091000141015     c                   Z-ADD     arblnp        ka4lnp
091100141015     c                   Z-ADD     arbnrs        ka4nrs
091200141015     c                   Z-ADD     arbnsp        ka4nsp
091300141015     c                   MOVEL     '1'           ka4trc
091400141015     c     keyta4        CHAIN     fiar401l                           99
091500141015if  1c                   IF        NOT *in99
091600141015     c                   MOVEL     ar4not        firmato74
091700141015e   1c                   ENDIF
091800141015     c                   endsr
091900000000     c*--------------------------------------------------------------------------------------------*
092000000000     c* controlla validità bolla
092100000000     c*--------------------------------------------------------------------------------------------*
092200000000     c     chktas        BEGSR
092300000000     c*
092400000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
092500000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
092600000000     c                   Z-ADD     s_tasmgs      dsmgsB
092700000000if  1c                   IF        dsdatB < datpre
092800000000     c                   MOVEL     '1'           esito                          *errore
092900000000e   1c                   ENDIF
093000030130     c* se l'esito è positivo, salva alcuni dati della bolla
093100140718     c                   EVAL      savtsp = f_tastsp
093200000000     c*
093300140718     C                   EVAL      wOrg = f_tasLna
093400051222     C                   EXSR      setLnaItaEst
093500051222     C
093600000000     c                   ENDSR
093700000000     c*--------------------------------------------------------------------------------------------*
093800000000     c* imposta i campi della DS di Output dalle bolle e files correlati
093900000000     c*--------------------------------------------------------------------------------------------*
094000140604     c     impdsotas     BEGSR
094100140604     c*
094200140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
094300140604     c                   EVAL      lnaDescO74 = sodes(i)
094400140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
094500150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
094600150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
094700150116     c                   MOVE      s_tasmgs      aasmgsO74
094800150116      *
094900140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
095000140718     c                   MOVEL     f_taslna      lineaaro74
095100140604     c*
095200140718     c                   MOVEL     f_tastbl      a1
095300140718     c                   MOVEL     f_tastbl      tpporto74
095400140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
095500140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
095600140604x   1c                   ELSE                                                   *Pagamento del
095700140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
095800140604e   1c                   ENDIF
095900140716     c                   MOVEL     f_tastsp      tpservio74
096000140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
096100140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
096200140604     C                             rpyOpCode:rpyEsito)
096300140604     C                   EVAL      corro74 = §5EDes
096400140604     c*
096500140716     c                   EVAL      destino74 = f_tasrsd
096600140716     c                   EVAL      desindo74 = f_tasind
096700140716     c                   EVAL      descapo74 = f_tascad
096800140716     c                   EVAL      desloco74 = f_taslod
096900140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
097000150326if  1c                   IF        f_tasnzd  = *blanks
097100150326     c                   EVAL      desnazo74 = f_tasprd
097200150326x   1c                   ELSE
097300150326     c***                EVAL      desnazo74 = f_tasnzd
097400150326     c***                EVAL      desnzdo74 = f_tasnzd
097500150326     c* 26/03/15: Attiviamo le specifiche per provincia export
097600150326     c                   EVAL      desnazo74 = f_tasprd
097700150326     c                   EVAL      desnzdo74 = f_tasnzd
097800150326e   1c                   ENDIF
097900140604     c*
098000140716     c                   EVAL      collio74  = f_tasncl
098100140716     c                   EVAL      pesoo74   = f_taspkf
098200140716     c                   EVAL      volumeo74 = f_tasvlf
098300140716     c                   EVAL      naturao74 = f_tasnas
098400140604     c*
098500140716     c                   EVAL      fatnumo74  = f_tasnft
098600140716     c                   MOVEL     f_tasdft      wdat
098700140604     c                   EXSR      edtdat
098800140604     c                   EVAL      fatdatao74 = wdate
098900140604     c*
099000140716     c                   EVAL      disnumo74  = f_tasndc
099100140716c    c                   MOVEL     f_tasddc      wdat
099200140604     c                   EXSR      edtdat
099300140604     c                   EVAL      disdatao74 = wdate
099400140604     **?Immagine lettera di vettura.
099500140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
099600140604     C                   IF        LDVO74 <> 'N'
099700140604     c                   CLEAR                   dtasflo
099800140716     c                   MOVEL     f_tasflo      dtasflo
099900140604     c                   IF        §floiml <> *BLANK
100000140604     c                   EVAL      ldvo74 = 'S'
100100140604     C                   EVAL      SpImLDVO74 =
100200140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
100300140716     C                             %EDITC(f_taslnp:'X') +
100400140716     C                             %EDITC(f_tasnrs:'X') +
100500140716     C                             %EDITC(f_tasnsp:'X')
100600140604     **?Reperisco il check code della spedizione POD
100700140604     C                   EVAL      PChkCdeO74 =
100800140604     C                             GetSpeChkCde(
100900140716     C                             %EDITC(f_tasaas:'X'):
101000140716     C                             %EDITC(f_taslnp:'X') +
101100140716     C                             %EDITC(f_tasnrs:'X') +
101200140716     C                             %EDITC(f_tasnsp:'X'):
101300140604     C                             ChkCode:nEsito)
101400140604     C                   ENDIF
101500140604     C                   ENDIF
101600140604     c*
101700140716     c                   MOVE      f_tasccm      n4
101800140604if  1c                   IF        n4 <> 8888                                   *codificato
101900140604     c                   CLEAR                   bs69ds
102000140604     c                   CLEAR                   acods
102100140604     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
102200140716     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
102300140716     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
102400140604     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
102500140604     c                   PARM                    bs69ds
102600140604     c                   PARM                    acods
102700140604     c                   PARM                    indds
102800140604     c                   PARM                    clpds
102900140604     c                   PARM                    clsds
103000140604if  2c                   IF        o69err <> '1'                                *no errore
103100140604     c                   EVAL      mito74    = acorag
103200140604     c                   EVAL      mitindo74 = indvia
103300140604     c                   EVAL      mitloco74 = indcit
103400140604     c                   MOVEL     indsta        a3
103500140604if  3c                   IF        a3 = *blanks
103600140604     c                   EVAL      mitnazo74 = indprv
103700140604x   3c                   ELSE
103800140604     c                   EVAL      mitnazo74 = a3
103900140604e   3c                   ENDIF
104000140604if  3c                   IF        indcae <> *blanks
104100140604     c                   EVAL      mitcapo74 = indcae
104200140604X   3c                   ELSE
104300140604     c                   MOVEL     indcap        a5
104400140604     c                   EVAL      mitcapo74 = a5
104500140604e   3c                   ENDIF
104600140604e   2c                   ENDIF
104700140604x   1c                   ELSE                                                   *non codificato
104800140716     c                   Z-ADD     f_tasaas      kaaaas
104900140716     c                   Z-ADD     f_taslnp      kaalnp
105000140716     c                   Z-ADD     f_tasnrs      kaanrs
105100140716     c                   Z-ADD     f_tasnsp      kaansp
105200140604     c                   MOVEL     'M'           kaatrc
105300140604     c     keytaa        CHAIN     titaa30c                           99
105400140604if  2c                   IF        NOT *in99
105500140604     c                   EVAL      mito74    = taarsc
105600140604     c                   EVAL      mitindo74 = taaind
105700140604     c                   EVAL      mitcapo74 = taacap
105800140604     c                   EVAL      mitloco74 = taaloc
105900140604if  3c                   IF        taanaz = *blanks
106000140604     c                   EVAL      mitnazo74 = taaprv
106100140604x   3c                   ELSE
106200140604     c                   EVAL      mitnazo74 = taanaz
106300140604e   3c                   ENDIF
106400140604e   2c                   ENDIF
106500140604e   1c                   ENDIF
106600140604     c* 2a ragione sociale destinatario.
106700140604     c                   CLEAR                   tita4000
106800140604     c                   CLEAR                   tita4p00
106900140716     c                   Z-ADD     f_tasaas      ka4aas
107000140716     c                   Z-ADD     f_taslnp      ka4lnp
107100140716     c                   Z-ADD     f_tasnrs      ka4nrs
107200140716     c                   Z-ADD     f_tasnsp      ka4nsp
107300140604     c                   MOVEL     'D'           ka4trc
107400140604     c     keyta4        CHAIN     tita430c                           99
107500140604if  1c                   IF        NOT *in99
107600140604     c                   MOVEL     ta4not        desti2O74
107700140604e   1c                   ENDIF
107800140604     c* rif partner estero
107900140604     c                   CLEAR                   tita4000
108000140604     c                   CLEAR                   tita4p00
108100140604     c                   CLEAR                   dsbl4e
108200140716     c                   Z-ADD     f_tasaas      ka4aas
108300140716     c                   Z-ADD     f_taslnp      ka4lnp
108400140716     c                   Z-ADD     f_tasnrs      ka4nrs
108500140716     c                   Z-ADD     f_tasnsp      ka4nsp
108600140604     c                   MOVEL     'E'           ka4trc
108700140604     c     keyta4        CHAIN     tita430c                           99
108800140604if  1c                   IF        NOT *in99
108900140604     c                   MOVEL     ta4not        dsbl4e
109000140604e   1c                   ENDIF
109100140604     c
109200140604     c                   CLEAR                   tita4000
109300140604     c                   CLEAR                   tita4p00
109400140604     c                   CLEAR                   dta4a
109500140716     c                   Z-ADD     f_tasaas      ka4aas
109600140716     c                   Z-ADD     f_taslnp      ka4lnp
109700140716     c                   Z-ADD     f_tasnrs      ka4nrs
109800140716     c                   Z-ADD     f_tasnsp      ka4nsp
109900140604     c                   MOVEL     'A'           ka4trc
110000140604     c     keyta4        CHAIN     tita430c                           99
110100140604if  1c                   IF        NOT *in99
110200140604     c                   MOVEL     ta4not        dta4a
110300140604e   1c                   ENDIF
110400140604     ** Riferimento mittente numerico (c'è sempre).
110500140716     C                   EVAL      rifero74 = f_tasrmn
110600140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
110700140604     C                   IF        §ta4arma <> *BLANK AND
110800140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
110900140604     C                   EVAL      rifero74a = §ta4arma
111000140604     C                   ENDIF
111100140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
111200140604     C                   IF        §b4erp <> *BLANK AND
111300140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
111400140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
111500140604     C                   EVAL      rpto74 = §b4erp
111600140604     C                   ENDIF
111700140604
111800140604     **?Reperisco il check code della spedizione.
111900140604     C                   EVAL      SChkCdeO74 =
112000140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
112100140604     C                             NSpedizI74:ChkCode:nEsito)
112200140604
112300140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
112400140604     C                   IF        KSCI74 <> *BLANK
112500140604     c                   SETOFF                                           98
112600140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
112700140604     C                   RESET                   tis7700i0
112800141126     C                   EVAL      tis7700i0.aas = kasaasm
112900141126     C                   EVAL      tis7700i0.lnp = kaslnpm
113000141126     C                   EVAL      tis7700i0.nrs = kasnrsm
113100141126     C                   EVAL      tis7700i0.nsp = kasnspm
113200140604     C                   EVAL      tis7700i0.ksu = kscI74
113300140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
113400140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
113500140604     C                             : rpyEsito
113600140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
113700140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
113800140604     C                             )
113900140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
114000140604     C                             tis7700o0.bollValida = *ON)
114100140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
114200140604if  2c                   IF        NOT *in98                                      *non trovato
114300140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
114400140604     C                   reset                   TIS778DSO
114500140604     C                   EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
114600140604     C                   RETURN
114700140604     C                   ENDIF
114800140604e   2c                   ENDIF
114900140604     C                   ENDIF
115000140604     c*
115100140604     ** Importo assicurato.
115200140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
115300140716     C                   EVAL      vasO74 = f_tasVas
115400141015     C                   EVAL      iasO74 = f_tasIas
115500140604     C                   ENDIF
115600140604     **
115700140604     c                   ENDSR
115800000000     c*--------------------------------------------------------------------------------------------*
115900000000     c* imposta i campi della DS di Output dagli eventi
116000000000     c*--------------------------------------------------------------------------------------------*
116100000000     c     impdsoevb     BEGSR
116200000000     c*
116300000000     c* azzera le variabili di lavoro
116400000000     c                   CLEAR                   tote                           *contatore eventi
116500000000     c                   CLEAR                   seql                           *sequenza legami
116600000000     c                   CLEAR                   spsort                         *per ordinamento
116700000000     c                   CLEAR                   spseql                         *di memorizzazione
116800000000     c                   CLEAR                   spseqe
116900000000     c                   CLEAR                   spdevhev
117000000000     c                   CLEAR                   spdeve
117100000000     c                   CLEAR                   spheve
117200000000     c                   CLEAR                   spfle
117300000000     c                   CLEAR                   spdev
117400000000     c                   CLEAR                   spcev
117500130312     c                   CLEAR                   spspe
117600000000     c*
117700000000     c* imposta la sequenza legami per la bolla di Input
117800000000     c                   Z-ADD     1             seql                           *primo legaqme
117900000000     c*---
118000000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
118100000000     c*---
118200000000     c***
118300000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
118400000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
118500000000     c*   si trasforma nella consegna anomala della mamma
118600000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
118700000000     c***
118800000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
118900000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
119000000000     c                   EVAL      kblaan = s_tasaas
119100000000     c                   EVAL      kbllpn = s_taslnp
119200000000     c                   EVAL      kblnrn = s_tasnrs
119300000000     c                   EVAL      kblnsn = s_tasnsp
119400000000     c     keylbl        CHAIN     fnlbl01l                           99
119500000000if  1c                   IF        NOT *in99                                    *ha la mamma
119600000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
119700000000     c                   EVAL      kasaas = lblaap
119800000000     c                   EVAL      kaslnp = lbllpp
119900000000     c                   EVAL      kasnrs = lblnrp
120000000000     c                   EVAL      kasnsp = lblnsp
120100000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
120200000000if  2c                   IF        NOT *in99 AND
120300000000     c                             tascca <> *blanks                            *ha cons.anomala
120400060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
120500060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
120600060622     C                             :rpyEsito)
120700060622if  3c*                  IF        *in98                                        *trovata
120800060626     C                   IF        rpyOpCode = 'FOUND'
120900041222     **?Consegna anomala, POD non visualizzabile.
121000060622     C*                  IF        sapod(jj) = 'N'
121100060622     C                   IF        §7OPODImg = 'N'
121200041222     C                   EVAL      LDVO74 = 'N'
121300041222     C                   ENDIF
121400060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
121500060622     C                   IF        §7oinc <> *blanks                            *ok per internet
121600000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
121700041222e   3c                   ENDIF
121800000000e   3c                   ENDIF
121900000000e   2c                   ENDIF
122000000000e   1c                   ENDIF
122100000000     c*
122200030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
122300000000     c                   ADD       1             tote
122400060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
122500060622     c                   MOVEL     §7odei        smdev(tote)
122600030203x   3c                   ELSE                                                   *evento telef.P.O.
122700060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
122800030203e   3c                   ENDIF
122900030203e   2c                   ENDIF
123000030203     c*
123100030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
123200060622     c                   MOVEL     §7ocev        wcev                           *causale evento
123300000000     c                   EXSR      evbrit                                       *imposta evento
123400030203x   2c                   ELSE                                                   *NO sost.ritiro
123500030203if  3c                   IF        s_tasdrt > *zeros
123600000000     c                   MOVEL     '701'         wcev                           *causale evento
123700000000     c                   EXSR      deccevsta
123800030203if  4c                   IF        werr <> '1'                                  *no errore
123900000000     c                   ADD       1             tote
124000030130     c                   MOVEL     wcdex         smdev(tote)
124100000000     c                   EXSR      evbrit                                       *imposta evento
124200030203e   4c                   ENDIF
124300030203e   3c                   ENDIF
124400030203e   2c                   ENDIF
124500140710     ** ID bolla madre.                                                          dubito figlia
124600080131     c                   EVAL      waas = s_tasaas
124700080131     c                   EVAL      wlnp = s_taslnp
124800080131     c                   EVAL      wnrs = s_tasnrs
124900080131     c                   EVAL      wnsp = s_tasnsp
125000000000     c* Partenza merce
125100000000     c                   EVAL      lduc = s_tasduc                              *data partenza
125200000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
125300000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
125400000000     c                   EXSR      evbpar
125500000000     c* Arrivo merce
125600110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
125700110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
125800140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
125900140723     c                             gen§ar5ArrDt <> *ZERO
126000140723     c                   MONITOR
126100140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
126200140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
126300140723     c                   ON-ERROR
126400140723     c                   ENDMON
126500140723     c                   ENDIF
126600100225     c                   EVAL      llna = s_taslna                              *linea arrivo
126700090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
126800090826     c                   EXSR      evbarr
126900000000     c*
127000000000     c* Consegna merce
127100000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
127200000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
127300000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
127400000000     c                   EVAL      llna = s_taslna                              *linea arrivo
127500000000     c                   EXSR      evbcon
127600000000     c* Lettera anomalia
127700000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
127800000000     c                   EXSR      memdct
127900000000e   1c                   ENDIF
128000000000     c*---
128100000000     c* memorizza gli eventi dal file eventi della bolla di input
128200000000     c*---
128300000000     c                   EXSR      memevb
128400000000     c*---
128500000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
128600000000     c*---
128700000000     c                   EXSR      elaevelbl
128800000000     c*
128900000000     c* imposta contatore eventi da visualizzare nella DS di Output
129000000000     c                   EVAL      cnteveo74 = tote
129100000000     c*---
129200000000     c* ordina gli eventi per sequenza legami / Data+ora evento
129300000000     c*---
129400000000     c* compone la schiera da ordinare
129500000000do  1c     1             DO        200           j
129600000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
129700000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
129800000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
129900000000     c                   Z-ADD     smi(j)        dsi                             -indice
130000000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
130100000000e   1c                   ENDDO
130200000000     c*
130300000000     c* ordina la schiera composta
130400000000     c                   SORTA     spsort
130500000000     c*
130600000000     c* ricompone le schiere da quella ordinata
130700000000     c                   Z-ADD     *zeros        jj
130800000000do  1c     1             DO        200           j
130900000000if  2c                   IF        spsort(j) >*zeros
131000000000     c                   ADD       1             jj
131100000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
131200000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
131300000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
131400000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
131500000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
131600000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
131700000000     c                   MOVEL     smheve(dsi)   spheve(jj)
131800000000     c                   MOVEL     smfle(dsi)    spfle(jj)
131900000000     c                   MOVEL     smdev(dsi)    spdev(jj)
132000000000     c                   MOVEL     smcev(dsi)    spcev(jj)
132100130312     c                   MOVEL     smspe(dsi)    spspe(jj)
132200000000e   2c                   ENDIF
132300000000e   1c                   ENDDO
132400101104     **
132500101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
132600101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
132700101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
132800101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
132900101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
133000101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
133100101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
133200101104     ** Ricordati che l'ordine per data e ora è discendente.
133300101104     C                   EVAL      j = 0
133400101104     C                   DOU       j = 0
133500101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
133600101104     C                                        : spcev : j+1)
133700101104     C                   IF        j = 0
133800101104     C                   LEAVE
133900101104     C                   ENDIF
134000101104     C                   IF        j > 1 AND
134100101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
134200101104     C                   EVAL      dev(1) = spdeve(j-1)
134300101104     C                   EVAL      hev(1) = spheve(j-1)
134400101104     C                   EVAL      fle(1) = spfle(j-1)
134500101104     C                   EVAL      eve(1) = spdev(j-1)
134600101104     C                   EVAL      cev(1) = spcev(j-1)
134700130312     C                   EVAL      spe(1) = spspe(j-1)
134800101104     C                   EVAL      spdeve(j-1) = spdeve(j)
134900101104     C                   EVAL      spheve(j-1) = spheve(j)
135000101104     C                   EVAL      spfle(j-1) = spfle(j)
135100101104     C                   EVAL      spdev(j-1) = spdev(j)
135200101104     C                   EVAL      spcev(j-1) = spcev(j)
135300130312     C                   EVAL      spspe(j-1) = spspe(j)
135400101104     C                   EVAL      spdeve(j) = dev(1)
135500101104     C                   EVAL      spheve(j) = hev(1)
135600101104     C                   EVAL      spfle(j) = fle(1)
135700101104     C                   EVAL      spdev(j) = eve(1)
135800101104     C                   EVAL      spcev(j) = cev(1)
135900130312     C                   EVAL      spspe(j) = spe(1)
136000101104     C                   ENDIF
136100101104     C                   ENDDO
136200000000     c*
136300120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
136400120831     c                   CLEAR                   cntEveO74
136500000000do  1c     1             DO        50            j
136600120831     c                   IF        spcev(j) = *BLANK
136700120831     c                   LEAVE
136800120831     c                   ENDIF
136900000000     c                   EVAL      dev(j) = spdeve(j)
137000000000     c                   EVAL      hev(j) = spheve(j)
137100000000     c                   EVAL      fle(j) = spfle(j)
137200041007     c                   EVAL      eve(j) = spdev(j)
137300041007     c                   EVAL      cev(j) = spcev(j)
137400130312     c                   EVAL      spe(j) = spspe(j)
137500101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
137600100909     c                   CLEAR                   hev(j)
137700100909     c                   ENDIF
137800120831     c                   EVAL      cntEveO74 += 1
137900000000e   1c                   ENDDO
138000000000     c*
138100000000     c                   ENDSR
138200000000     c*--------------------------------------------------------------------------------------------*
138300000000     c* memorizza gli eventi dal file eventi della bolla di input
138400000000     c*--------------------------------------------------------------------------------------------*
138500000000     c     memevb        BEGSR
138600000000     c***
138700000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
138800000000     c* . se SI lo aggiorna solo nella sequenza legame
138900000000     c*   se NO lo inserisce
139000000000     c***
139100000000     c                   Z-ADD     waas          kvbaas
139200000000     c                   Z-ADD     wlnp          kvblnp
139300000000     c                   Z-ADD     wnrs          kvbnrs
139400000000     c                   Z-ADD     wnsp          kvbnsp
139500000000     c     keyevb        SETLL     fnevb01l
139600000000     c     keyevb        READE     fnevb01l                               99
139700000000do  1c                   DOW       NOT *in99
139800000000if  2c                   IF        evbatb = *blanks                             *valido
139900140108     c                   MOVEL     evbdev        wdev
140000140108     c                   MOVEL     evbhev        whev
140100080131if  3c                   IF        evbDev > *zeros
140200000000     c                   MOVEL     evbcev        wcev
140300000000     c                   EXSR      deccevsta
140400070109     c                   EXSR      eventoAnnulla
140500000000if  4c                   IF        werr <> '1'                                  *no errore
140600000000     c*
140700000000     c* controlla se evento già inserito
140800000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
140900000000     c                   MOVEL     'S'           newevb                         *nuovo evento
141000000000do  5c     1             DO        200           j
141100000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
141200080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
141300000000     c                   MOVEL     'N'           newevb                         *già inserito
141400000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
141500080130     c                   Z-ADD     tasLna        worg
141600000000     c                   EXSR      decorg
141700000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
141800080201     c                   MOVEL     wDev          wdat
141900080201     c                   EXSR      edtdat
142000080201     c                   MOVEL     wdate         smdeve(j)
142100080201     c                   MOVEL     wHev          wora
142200080201     c                   EXSR      edtora
142300080201     c                   MOVEL     worae         smheve(j)
142400130313     c                   MOVEL     dsevbspe      smspe(j)
142500000000     c                   LEAVE                                                  *uscita ciclo
142600000000e   6c                   ENDIF
142700000000e   5c                   ENDDO
142800000000     c*
142900000000     c* incrementa contatore eventi
143000000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
143100000000     c                   ADD       1             tote
143200000000     c* indice schiera
143300000000     c                   Z-ADD     tote          smi(tote)
143400000000     c* sequenza legame
143500000000     c                   Z-ADD     seql          smseql(tote)
143600000000     c* sequenza evento
143700000000     c                   Z-ADD     020           smseqe(tote)
143800000000     c* descrizione
143900030130     c                   MOVEL     wcdex         smdev(tote)
144000000000     c* data
144100080201     c                   MOVEL     wDev          wdat
144200000000     c                   EXSR      edtdat
144300000000     c                   MOVEL     wdate         smdeve(tote)
144400000000     c* ora
144500080201     c                   MOVEL     wHev          wora
144600000000     c                   EXSR      edtora
144700000000     c                   MOVEL     worae         smheve(tote)
144800000000     c* filiale
144900140722     C                   IF        wrkLnaItaEst = ESTERO
145000140722     c                   Z-ADD     tasLna        worg
145100140722     c                   else
145200140722     c                   Z-ADD     evbfle        worg
145300140722     c                   endif
145400140722     c                   EXSR      decorg
145500140722     c                   MOVEL     wodes         smfle(tote)
145600000000     c* causale
145700000000     c                   MOVEL     evbcev        smcev(tote)
145800130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
145900130312     c* bolla legata
146000130312     c                   MOVEL     dsevbspe      smspe(tote)
146100000000     c* data+ora evento
146200080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
146300000000e   5c                   ENDIF
146400000000e   4c                   ENDIF
146500000000e   3c                   ENDIF
146600000000e   2c                   ENDIF
146700010717     c     keyevb        READE     fnevb01l                               99
146800000000e   1c                   ENDDO
146900000000     c*
147000000000     c                   ENDSR
147100000000     c*--------------------------------------------------------------------------------------------*
147200000000     c* memorizza gli "eventi fissi"
147300000000     c*--------------------------------------------------------------------------------------------*
147400000000     c     memevefix     BEGSR
147500080131     ** ID bolla in canna.
147600080131     c                   EVAL      waas = tasaas
147700080131     c                   EVAL      wlnp = taslnp
147800080131     c                   EVAL      wnrs = tasnrs
147900080131     c                   EVAL      wnsp = tasnsp
148000000000     c*
148100000000     c* Partenza merce
148200000000     c                   EVAL      lduc = tasduc                                *data partenza
148300000000     c                   EVAL      llnp = taslnp                                *filiale partenza
148400000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
148500000000     c                   EXSR      evbpar
148600000000     c* Consegna merce
148700000000     c                   EVAL      lcca = tascca                                *consegna anomala
148800000000     c                   EVAL      ldcm = tasdcm                                *data consegna
148900000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
149000000000     c                   EVAL      llna = taslna                                *linea arrivo
149100000000     c                   EXSR      evbcon
149200000000     c*
149300000000     c                   ENDSR
149400000000     c*--------------------------------------------------------------------------------------------*
149500000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
149600000000     c*--------------------------------------------------------------------------------------------*
149700000000     c     elaevelbl     BEGSR
149800000000     c*
149900000000     c* azzera le variabili di lavoro
150000000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
150100000000     c                   EVAL      deplpn = *zeros
150200000000     c                   EVAL      depnrn = *zeros
150300000000     c                   EVAL      depnsn = *zeros
150400000000     c*
150500000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
150600000000     c                   EVAL      kbllpp =  s_taslnp
150700000000     c                   EVAL      kblnrp =  s_tasnrs
150800000000     c                   EVAL      kblnsp =  s_tasnsp
150900000000     c*
151000000000     c* ciclo fino a fine mamme
151100000000     c     ke2lbl        SETLL     fnlbl02l
151200000000     c     ke2lbl        READE     fnlbl02l                               99
151300000000do  1c                   DOW       NOT *in99
151400000000     c*
151500000000     c* ciclo fino a fine figlie della mamma
151600000000do  2c                   DOW       NOT *in99
151700000000     c*
151800000000     c* legge la bolla dal legame
151900000000     c                   EVAL      kasaas = lblaan                              *figlia
152000000000     c                   EVAL      kaslnp = lbllpn
152100000000     c                   EVAL      kasnrs = lblnrn
152200000000     c                   EVAL      kasnsp = lblnsn
152300000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
152400000000if  3c                   IF        NOT *in99 AND                                *esistente
152500000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
152600000000     c                             tastbl <> 'A3' AND
152700000000     c                             tastbl <> 'F3' AND
152800000000     c                             tastbl <> 'AP' AND
152900000000     c                             tastbl <> 'FC'
153000041222     **?Immagine lettera di vettura.
153100041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
153200041222     C                   IF        LDVO74 <> 'N'
153300040330     c                   MOVEL     tasflo        dtasflo
153400040330     c                   IF        §floiml <> *BLANK
153500050325     c                   EVAL      ldvo74 = 'S'
153600040330     C                   EVAL      SpImLDVO74 =
153700040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
153800040330     C                             %EDITC(taslnp:'X') +
153900040330     C                             %EDITC(tasnrs:'X') +
154000040330     C                             %EDITC(tasnsp:'X')
154100041223     **?Reperisco il check code della spedizione POD
154200041223     C                   EVAL      PChkCdeO74 =
154300041223     C                             GetSpeChkCde(
154400041223     C                             %EDITC(tasaas:'X'):
154500041223     C                             %EDITC(taslnp:'X') +
154600041223     C                             %EDITC(tasnrs:'X') +
154700041223     C                             %EDITC(tasnsp:'X'):
154800041223     C                             ChkCode:nEsito)
154900040330     C                   ENDIF
155000041222     C                   ENDIF
155100000000     c* incrementa la sequenza legami per le bolle legate
155200000000     c                   ADD       1             seql                           *legaqme successivo
155300000000     c*---
155400000000     c* memorizza gli "eventi fissi"
155500000000     c*---
155600000000     c                   EXSR      memevefix
155700000000     c*---
155800000000     c* memorizza gli eventi
155900000000     c*---
156000000000     c                   EVAL      waas = lblaan                                *figlia
156100000000     c                   EVAL      wlnp = lbllpn
156200000000     c                   EVAL      wnrs = lblnrn
156300000000     c                   EVAL      wnsp = lblnsn
156400000000     c                   EXSR      memevb
156500000000     c*
156600000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
156700000000     c                   EVAL      depaan = lblaan                              *ultima figlia
156800000000     c                   EVAL      deplpn = lbllpn
156900000000     c                   EVAL      depnrn = lblnrn
157000000000     c                   EVAL      depnsn = lblnsn
157100000000e   3c                   ENDIF
157200000000     c*
157300000000     c* lettura successiva legami
157400000000     c     ke2lbl        READE     fnlbl02l                               99
157500000000e   2c                   ENDDO                                                  *fine figlie mamma
157600000000     c*
157700000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
157800000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
157900000000     c                             kbllpp <> deplpn OR
158000000000     c                             kblnrp <> depnrn OR
158100000000     c                             kblnsp <> depnsn
158200000000     c* nuova chiave
158300000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
158400000000     c                   EVAL      kbllpp =  deplpn
158500000000     c                   EVAL      kblnrp =  depnrn
158600000000     c                   EVAL      kblnsp =  depnsn
158700000000     c* controlla figlie
158800000000     c     ke2lbl        SETLL     fnlbl02l
158900000000     c     ke2lbl        READE     fnlbl02l                               99
159000000000if  3c                   IF        *in99                                        *non ha figlie
159100000000     c                   LEAVE                                                  *esce dal ciclo
159200000000e   3c                   ENDIF
159300000000x   2c                   ELSE                                                   *sempre stessa mamma
159400000000     c                   LEAVE                                                  *esce dal ciclo
159500000000e   2c                   ENDIF
159600000000e   1c                   ENDDO                                                  *fine mamme
159700000000     c*
159800000000     c                   ENDSR
159900000000     c*--------------------------------------------------------------------------------------------*
160000000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
160100000000     c*--------------------------------------------------------------------------------------------*
160200000000     c     evbrit        BEGSR
160300000000     c*
160400000000     c* indice schiera
160500000000     c                   Z-ADD     tote          smi(tote)
160600000000     c* sequenza legame
160700000000     c                   Z-ADD     seql          smseql(tote)
160800000000     c* sequenza evento
160900000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
161000000000     c* data
161100000000if  1c                   IF        s_tasdrt > *zeros
161200000000     c                   MOVEL     s_tasdrt      wdat
161300000000     c                   EXSR      edtdat
161400000000     c                   MOVEL     wdate         smdeve(tote)
161500000000e   1c                   ENDIF
161600000000     c* ora
161700000000if  1c                   IF        s_tashrt > *zeros
161800000000     c                   MOVEL     s_tashrt      wora
161900000000x   1c                   ELSE
162000000000if  2c                   IF        s_tasfpp = 'P'                               *PM
162100000000     c                   MOVEL     1800          wora
162200000000x   2c                   ELSE                                                   *AM
162300000000     c                   MOVEL     1200          wora
162400000000e   2c                   ENDIF
162500000000e   1c                   ENDIF
162600000000     c                   EXSR      edtora
162700000000     c                   MOVEL     worae         smheve(tote)
162800000000     c* filiale
162900000000     c                   Z-ADD     s_taslnp      worg
163000000000     c                   EXSR      decorg
163100000000     c                   MOVEL     wodes         smfle(tote)
163200000000     c* causale
163300000000     c                   MOVEL     wcev          smcev(tote)
163400000000     c* data+ora evento
163500000000     c                   MOVEL     wdat          smdevhev(tote)
163600000000     c                   MOVE      wora          smdevhev(tote)
163700000000     c*
163800000000     c                   ENDSR
163900000000     c*--------------------------------------------------------------------------------------------*
164000000000     c* Imposta l'evento Partenza
164100000000     c*--------------------------------------------------------------------------------------------*
164200000000     c     evbpar        BEGSR
164300000000     c*
164400000000     c* controlla SE e COME inserire l'evento
164500000000if  1c                   IF        lduc > *zeros
164600000000     c* imposta causale
164700000000     c                   MOVEL     '702'         wcev
164800000000     c                   EXSR      deccevsta
164900000000if  2c                   IF        werr <> '1'                                  *no errore
165000000000     c* imposta filiale
165100000000     c                   Z-ADD     llnp          worg
165200000000     c                   EXSR      decorg
165300000000     c* imposta data
165400101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
165500000000     c                   MOVEL     ldrt          wdat                           *data ritiro
165600000000x   3c                   ELSE                                                   partenza ITALIA
165700000000     c                   MOVEL     lduc          wdat                           *data partenza
165800000000e   3c                   ENDIF
165900000000     c                   EXSR      edtdat
166000000000     c* imposta ora
166100000000     c                   MOVEL     2100          wora                           *ora imposta
166200000000     c                   EXSR      edtora
166300000000     c*
166400000000     c* controlla se l'evento è già stato inserito
166500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
166600000000do  3c     1             DO        200           j
166700000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
166800000000     c                   MOVE      wora          lavdevhev
166900000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
167000000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
167100000000     c                   MOVEL     'N'           newevb                         *già inserito
167200000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
167300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
167400000000     c                   LEAVE                                                  *uscita ciclo
167500000000e   4c                   ENDIF
167600000000e   3c                   ENDDO
167700000000     c*
167800000000     c* incrementa contatore eventi
167900000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
168000000000     c                   ADD       1             tote
168100000000     c* indice schiera
168200000000     c                   Z-ADD     tote          smi(tote)
168300000000     c* sequenza legame
168400000000     c                   Z-ADD     seql          smseql(tote)
168500000000     c* sequenza evento
168600000000     c                   Z-ADD     020           smseqe(tote)
168700000000     c* descrizione
168800030130     c                   MOVEL     wcdex         smdev(tote)
168900000000     c* filiale -impostata sopra-
169000000000     c                   MOVEL     wodes         smfle(tote)
169100000000     c* data    -impostata sopra-
169200000000     c                   MOVEL     wdate         smdeve(tote)
169300000000     c* ora     -impostata sopra-
169400000000     c                   MOVEL     worae         smheve(tote)
169500000000     c* causale -impostata sopra-
169600000000     c                   MOVEL     wcev          smcev(tote)
169700000000     c* data+ora evento
169800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
169900000000     c                   MOVE      wora          smdevhev(tote)
170000000000e   3c                   ENDIF
170100000000e   2c                   ENDIF
170200000000e   1c                   ENDIF
170300000000     c*
170400000000     c                   ENDSR
170500101129     c*------------------------------------------------------------------------*
170600101129     c*
170700101129     c* Imposta l'evento Arrivo
170800101129     c*
170900101129     c*------------------------------------------------------------------------*
171000000000     c     evbarr        BEGSR
171100101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
171200101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
171300101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
171400101129     ** sommato inutile.
171500130312     C                   IF        wrkLnaItaEst = ESTERO
171600101104     C                   LEAVESR
171700101104     C                   ENDIF
171800101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
171900101129     C                   IF        lhti = *ZERO
172000101129     C                   LEAVESR
172100101129     C                   ENDIF
172200000000     c*
172300000000     c* controlla SE e COME inserire l'evento
172400000000if  1c                   IF        ldti > *zeros OR
172500000000     c                             ldcm > *zeros
172600000000     c* imposta causale
172700101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
172800000000     c                   EXSR      deccevsta
172900000000if  2c                   IF        werr <> '1'                                  *no errore
173000000000     c* imposta filiale
173100000000     c                   Z-ADD     llna          worg
173200000000     c                   EXSR      decorg
173300000000     c* imposta data
173400000000if  3c                   IF        ldti > *zeros
173500000000     c                   MOVEL     ldti          wdat                           *data arrivo
173600000000x   3c                   ELSE
173700000000     c                   MOVEL     ldcm          wdat                           *data consegna
173800000000e   3c                   ENDIF
173900000000     c                   EXSR      edtdat
174000000000     c* imposta ora
174100000000if  3c                   IF        lhti > *zeros
174200000000     c                   MOVEL     lhti          wora                           *ora arrivo
174300000000x   3c                   ELSE
174400000000     c                   MOVEL     0600          wora                           *ora imposta
174500000000e   3c                   ENDIF
174600000000     c                   EXSR      edtora
174700000000     c*
174800000000     c* controlla se l'evento è già stato inserito
174900000000     c                   MOVEL     'S'           newevb                         *nuovo evento
175000000000do  3c     1             DO        200           j
175100000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
175200000000     c                   MOVE      wora          lavdevhev
175300000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
175400000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
175500000000     c                   MOVEL     'N'           newevb                         *già inserito
175600000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
175700000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
175800000000     c                   LEAVE                                                  *uscita ciclo
175900000000e   4c                   ENDIF
176000000000e   3c                   ENDDO
176100000000     c*
176200000000     c* incrementa contatore eventi
176300000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
176400000000     c                   ADD       1             tote
176500000000     c* indice schiera
176600000000     c                   Z-ADD     tote          smi(tote)
176700000000     c* sequenza legame
176800000000     c                   Z-ADD     seql          smseql(tote)
176900000000     c* sequenza evento
177000000000     c                   Z-ADD     020           smseqe(tote)
177100000000     c* descrizione
177200030130     c                   MOVEL     wcdex         smdev(tote)
177300000000     c* data    -impostata sopra-
177400000000     c                   MOVEL     wdate         smdeve(tote)
177500000000     c* ora     -impostata sopra-
177600000000     c                   MOVEL     worae         smheve(tote)
177700000000     c* filiale -impostata sopra-
177800000000     c                   MOVEL     wodes         smfle(tote)
177900000000     c* causale -impostata sopra-
178000000000     c                   MOVEL     wcev          smcev(tote)
178100000000     c* data+ora evento
178200000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
178300000000     c                   MOVE      wora          smdevhev(tote)
178400000000e   3c                   ENDIF
178500000000e   2c                   ENDIF
178600000000e   1c                   ENDIF
178700000000     c*
178800000000     c                   ENDSR
178900000000     c*--------------------------------------------------------------------------------------------*
179000000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
179100000000     c*--------------------------------------------------------------------------------------------*
179200140714     c     evbcon        BEGSR
179300000000     c*
179400000000     c* azzera variabili di lavoro
179500000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
179600000000     c****
179700000000     c* Consegna merce ->
179800000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
179900000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
180000000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
180100000000     c*   nè lo stato di consegna nè quello di consegna anomala.
180200000000     c****
180300000000     c* controlla SE e COME inserire l'evento
180400000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
180500060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
180600060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
180700060622     C                             :rpyEsito)
180800060622     C                   IF        rpyOpCode = 'FOUND'
180900041222     **?Consegna anomala, POD non visualizzabile.
181000060622     C                   IF        §7OPODImg = 'N'
181100041222     C                   EVAL      LDVO74 = 'N'
181200041222     C                   ENDIF
181300060622     c                   IF        §7oinc <> *blanks                            *ok per internet
181400060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
181500000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
181600000000x   3c                   ELSE                                                   *ha record FNEVB
181700000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
181800000000e   3c                   ENDIF
181900041222e   3c                   ENDIF
182000000000e   2c                   ENDIF
182100000000e   1c                   ENDIF
182200080131     c* imposta data
182300080131if  1c                   IF        ldcm > *zeros
182400080131     c                   MOVEL     ldcm          wdat
182500080131e   1c                   ENDIF
182600080131     c* imposta ora
182700080131if  1c                   IF        lhmc > *zeros
182800080131     c                   MOVEL     lhmc          wora
182900080131e   1c                   ENDIF
183000000000     c* imposta causale
183100000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
183200060622     c                   MOVEL     §7ocev        wcev                           *causale evento
183300080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
183400080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
183500080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
183600080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
183700080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
183800080131     c* ritiro merce) dalla bolla figlia.
183900080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
184000080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
184100080131     c                             AND s_tasNdc < *HIVAL
184200080131     c                   EVAL      kblAap = wAas
184300080131     c                   EVAL      kblLpp = wLnp
184400080131     c                   EVAL      kblNrp = wNrs
184500080131     c                   EVAL      kblNsp = wNsp
184600080131     c     ke2lbl        CHAIN     fnlbl02l
184700080131     c                   IF        %FOUND()
184800080131     c                   EVAL      kasAas = lblAan
184900080131     c                   EVAL      kasLnp = lblLpn
185000080131     c                   EVAL      kasNrs = lblNrn
185100080131     c                   EVAL      kasNsp = lblNsn
185200080131     C     keyTas        CHAIN     titas000      titas000Figlia
185300080131     C                   IF        %FOUND()
185400080131     C                   EVAL      wDat = titas000Figlia.tasDrt
185500080131     C                   EVAL      wOra = titas000Figlia.tasHrt
185600080131     C                   ELSE
185700080131     C     keyTas        CHAIN     titas010      titas010Figlia
185800080131     C                   IF        %FOUND()
185900080131     C                   EVAL      wDat = titas010Figlia.tasDrt
186000080131     C                   EVAL      wOra = titas010Figlia.tasHrt
186100080131     C                   ELSE
186200080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
186300080131     C                   IF        %FOUND()
186400080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
186500080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
186600080131     C                   ENDIF
186700080131     C                   ENDIF
186800080131     C                   ENDIF
186900080131     c                   ENDIF
187000080131     c                   ENDIF
187100000000x   1c                   ELSE                                                   *NO sost.Consegna
187200000000     c                   MOVEL     '704'         wcev                           *causale evento
187300000000e   1c                   ENDIF
187400080131     c* Editazione data e ora evento.
187500080131     c                   EXSR      edtdat
187600080131     c                   EXSR      edtora
187700000000     c* imposta filiale
187800000000     c                   Z-ADD     llna          worg
187900000000     c                   EXSR      decorg
188000000000     c* controlla se l'evento è già stato inserito
188100000000     c                   MOVEL     'S'           newevb                         *nuovo evento
188200000000do  1c     1             DO        200           j
188300000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
188400000000     c                   MOVE      wora          lavdevhev
188500000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
188600000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
188700000000     c                   MOVEL     'N'           newevb                         *già inserito
188800000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
188900000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
189000000000     c                   LEAVE                                                  *uscita ciclo
189100000000e   2c                   ENDIF
189200000000e   1c                   ENDDO
189300000000     c*
189400000000     c* incrementa contatore eventi
189500030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
189600030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
189700000000     c                   ADD       1             tote                           incrementa contatore
189800060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
189900060622     c                   MOVEL     §7odei        smdev(tote)
190000030203x   4c                   ELSE                                                   *evento telef.P.O.
190100060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
190200030203e   4c                   ENDIF
190300060622     c                   MOVEL     §7ocev        wcev                           *causale evento
190400000000     c                   EXSR      evbcon2                                      *imposta evento
190500030203x   3c                   ELSE                                                   *NO sost.Consegna
190600000000     c*
190700030203if  4c                   IF        ldcm > *zeros
190800030203if  5c                   IF        sostcon <> ' '                               *considera consegna
190900000000     c                   EXSR      deccevsta
191000030203if  6c                   IF        werr <> '1'                                  *no errore
191100000000     c                   ADD       1             tote                           incrementa contatore
191200030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
191300000000     c                   EXSR      evbcon2                                      *imposta evento
191400030203e   6c                   ENDIF
191500030203e   5c                   ENDIF
191600030203e   4c                   ENDIF
191700030203e   3c                   ENDIF
191800000000     c*
191900030203e   1c                   ENDIF
192000030203     c*
192100000000     c                   ENDSR
192200000000     c*--------------------------------------------------------------------------------------------*
192300000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
192400000000     c*--------------------------------------------------------------------------------------------*
192500000000     c     evbcon2       BEGSR
192600000000     c*
192700000000     c* indice schiera
192800000000     c                   Z-ADD     tote          smi(tote)
192900000000     c* sequenza legame
193000000000     c                   Z-ADD     seql          smseql(tote)
193100000000     c* sequenza evento
193200000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
193300000000     c* data    -impostata sopra-
193400000000     c                   MOVEL     wdate         smdeve(tote)
193500000000     c* ora     -impostata sopra-
193600000000     c                   MOVEL     worae         smheve(tote)
193700000000     c* filiale -impostata sopra-
193800000000     c                   MOVEL     wodes         smfle(tote)
193900000000     c* causale -impostata sopra-
194000000000     c                   MOVEL     wcev          smcev(tote)
194100000000     c* data+ora evento
194200000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
194300000000     c                   MOVE      wora          smdevhev(tote)
194400000000     c*
194500000000     c                   ENDSR
194600000000     c*--------------------------------------------------------------------------------------------*
194700000000     c* memorizza i dati della lettera di anomalia
194800000000     c*--------------------------------------------------------------------------------------------*
194900000000     c     memdct        BEGSR
195000000000     c*
195100040906     C                   RESET                   FIDN12DS
195200040906     c                   Z-ADD     waas          I12AAS
195300040906     c                   Z-ADD     wlnp          I12LNP
195400040906     c                   Z-ADD     wnrs          I12NRS
195500040906     c                   Z-ADD     wnsp          I12NSP
195600040906     C                   CALL      'FIDN12R'
195700040906     C                   PARM                    FIDN12DS
195800000000     c* memorizza la prima lettera di anomalia valida
195900040906do  1c                   IF        O12Err = *BLANK
196000040906     C                   DO        O12NCA        K
196100040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
196200040906     C                   EVAL      DCTAAC = O12KeyAAC
196300040906     C                   EVAL      DCTFIL = O12KeyFil
196400040906     C                   EVAL      DCTNCA = O12KeyNCA
196500040906     c     keydct        CHAIN     fndct01l
196600040906do  1c                   IF        %FOUND
196700000000     c*
196800000000     c* controlla validità CA
196900000000     c                   MOVEL     '0'           werr                           *NO errore
197000000000     c                   EXSR      chkdct
197100000000if  2c                   IF        werr = '0'                                   *CA valida
197200000000     c*
197300000000     c* Imposta l'evento lettera di anomalia
197400000000     c                   EXSR      evbdct
197500000000     C                   LEAVE                                                  *uscita ciclo
197600000000e   2c                   ENDIF
197700000000     c*
197800040906e   1c                   ENDIF
197900040906e   1c                   ENDDO
198000040906e   1c                   ENDIF
198100000000     C*
198200000000     c                   ENDSR
198300000000     c*--------------------------------------------------------------------------------------------*
198400000000     c* controlla validità lettera di anomalia
198500000000     c*--------------------------------------------------------------------------------------------*
198600000000     c     chkdct        BEGSR
198700000000     c*
198800000000     c* reimposta variabili di lavoro
198900000000     c                   MOVEL     '0'           werr                           *NO errore
199000000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
199100000000     c*
199200000000     c* CA annullata, esclude
199300000000if  1c                   IF        dctatb <> ' '
199400000000     c                   MOVEL     '1'           werr                           *SI errore
199500000000e   1C                   ENDIF
199600000000     c*
199700000000     c* CA non confermata, esclude
199800000000if  1c                   IF        dctfpr = ' '
199900000000     c                   MOVEL     '1'           werr                           *SI errore
200000000000e   1C                   ENDIF
200100000000     c*
200200000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
200300000000if  1c                   IF        dctfpr = 'T' AND
200400000000     c                             dctfca >= 215 OR
200500000000     c                             dctfpr = 'P' AND
200600000000     c                             dctfca >= 400
200700000000x   1c                   ELSE
200800000000     c                   MOVEL     '1'           werr                           *SI errore
200900000000e   1C                   ENDIF
201000000000     c*
201100000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
201200000000if  1c                   IF        werr = '0'
201300000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
201400060622     C                   RESET                   tibs02ds
201500060622     C                   EVAL      t02Cod = 'CCH'
201600060622     C                   EVAL      t02Ke1 = dctcch
201700060622     C                   EXSR      getTntbe00f
201800060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
201900000000     c                   MOVEL     '1'           werr                           *SI errore
202000000000e   3C                   ENDIF
202100000000e   2C                   ENDIF
202200000000e   1C                   ENDIF
202300000000     c*
202400000000     c                   ENDSR
202500000000     c*--------------------------------------------------------------------------------------------*
202600000000     c* Imposta l'evento lettera di anomalia
202700000000     c*--------------------------------------------------------------------------------------------*
202800000000     c     evbdct        BEGSR
202900000000     C*
203000000000     c* imposta causale
203100000000if  1c                   IF        wfpr = 'T'
203200000000     c                   MOVEL     '709'         wcev                           *causale evento
203300000000x   1c                   ELSE
203400000000     c                   MOVEL     '710'         wcev                           *causale evento
203500000000e   1c                   ENDIF
203600000000     c                   EXSR      deccevsta                                    *decodifica evento
203700000000     c*
203800000000     c* imposta filiale
203900000000if  1c                   IF        werr <> '1'                                  *no errore
204000000000     c                   Z-ADD     dctfil        worg
204100000000     c                   EXSR      decorg
204200000000     c* imposta data
204300000000     c                   MOVEL     dctaac        wdat
204400000000     c                   MOVE      dctmgc        wdat
204500000000     c                   EXSR      edtdat
204600000000     c* imposta ora
204700000000     c                   MOVEL     1100          wora                           *FISSA
204800000000     c                   EXSR      edtora
204900000000     c*
205000000000     c* incrementa contatore eventi
205100000000     c                   ADD       1             tote
205200000000     c* indice schiera
205300000000     c                   Z-ADD     tote          smi(tote)
205400000000     c* sequenza legame
205500000000     c                   Z-ADD     seql          smseql(tote)
205600000000     c* sequenza evento
205700000000     c                   Z-ADD     999           smseqe(tote)
205800000000     c* descrizione -impostata sopra-
205900030130     c                   MOVEL     wcdex         smdev(tote)
206000000000     c* filiale -impostata sopra-
206100000000     c                   MOVEL     wodes         smfle(tote)
206200000000     c* data    -impostata sopra-
206300000000     c                   MOVEL     wdate         smdeve(tote)
206400000000     c* ora     -impostata sopra-
206500000000     c                   MOVEL     worae         smheve(tote)
206600000000     c* causale -impostata sopra-
206700000000     c                   MOVEL     wcev          smcev(tote)
206800000000     c* data+ora evento
206900000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
207000000000     c                   MOVE      wora          smdevhev(tote)
207100000000e   1c                   ENDIF
207200000000     c*
207300000000     c                   ENDSR
207400000000     c*--------------------------------------------------------------------------------------------*
207500000000     c* imposta i campi della DS di Output dal contrassegno
207600000000     c*--------------------------------------------------------------------------------------------*
207700000000     c     impdsocsb     BEGSR
207800000000     c*
207900000000     c* azzera le variabili di lavoro
208000000000     c*
208100000000     c****
208200000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
208300000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
208400000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
208500000000     c****
208600000000     c*
208700000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
208800090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
208900000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
209000000000x   1c                   ELSE                                                     *trovato
209100000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
209200000000e   1C                   ENDIF
209300000000     c*---
209400000000     c* memorizza il contrassegno della bolla di Input
209500000000     c*---
209600000000     c                   EVAL      waas = s_tasaas                              *bolla Input
209700000000     c                   EVAL      wlnp = s_taslnp
209800000000     c                   EVAL      wnrs = s_tasnrs
209900000000     c                   EVAL      wnsp = s_tasnsp
210000000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
210100000000     c                   EXSR      memcsb
210200000000     c*
210300000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
210400000000     c                   EXSR      elacsblbl
210500000000     c*
210600000000     c                   ENDSR
210700000000     c*--------------------------------------------------------------------------------------------*
210800000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
210900000000     c*--------------------------------------------------------------------------------------------*
211000000000     c     elacsblbl     BEGSR
211100000000     c*
211200000000     c* azzera le variabili di lavoro
211300000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
211400000000     c                   EVAL      deplpn = *zeros
211500000000     c                   EVAL      depnrn = *zeros
211600000000     c                   EVAL      depnsn = *zeros
211700000000     c*
211800000000     c* legge i legami delle figlie successive alla bolla di input
211900000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
212000000000     c                   EVAL      kbllpp =  s_taslnp
212100000000     c                   EVAL      kblnrp =  s_tasnrs
212200000000     c                   EVAL      kblnsp =  s_tasnsp
212300000000     c*
212400000000     c* ciclo fino a fine mamme
212500000000     c     ke2lbl        SETLL     fnlbl02l
212600000000     c     ke2lbl        READE     fnlbl02l                               99
212700000000do  1c                   DOW       NOT *in99
212800000000     c*
212900000000     c* ciclo fino a fine figlie della mamma
213000000000do  2c                   DOW       NOT *in99
213100000000     c*
213200000000     c* legge la bolla dal legame
213300000000     c                   EVAL      kasaas = lblaan                              *figlia
213400000000     c                   EVAL      kaslnp = lbllpn
213500000000     c                   EVAL      kasnrs = lblnrn
213600000000     c                   EVAL      kasnsp = lblnsn
213700000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
213800000000     c*---
213900000000     c* memorizza il contrassegno della bolla figlia legata
214000000000     c*---
214100000000if  4c                   IF        NOT *in99                                    *esistente
214200000000     c                   EVAL      waas = lblaan                                *figlia
214300000000     c                   EVAL      wlnp = lbllpn
214400000000     c                   EVAL      wnrs = lblnrn
214500000000     c                   EVAL      wnsp = lblnsn
214600000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
214700000000     c                   EXSR      memcsb
214800000000e   4c                   ENDIF
214900000000     c*
215000000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
215100000000     c                   EVAL      depaan = lblaan                              *ultima figlia
215200000000     c                   EVAL      deplpn = lbllpn
215300000000     c                   EVAL      depnrn = lblnrn
215400000000     c                   EVAL      depnsn = lblnsn
215500000000     c*
215600000000     c* lettura successiva legami
215700000000     c     ke2lbl        READE     fnlbl02l                               99
215800000000e   2c                   ENDDO                                                  *fine figlie mamma
215900000000     c*
216000000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
216100000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
216200000000     c                             kbllpp <> deplpn OR
216300000000     c                             kblnrp <> depnrn OR
216400000000     c                             kblnsp <> depnsn
216500000000     c* nuova chiave
216600000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
216700000000     c                   EVAL      kbllpp =  deplpn
216800000000     c                   EVAL      kblnrp =  depnrn
216900000000     c                   EVAL      kblnsp =  depnsn
217000000000     c* controlla figlie
217100000000     c     ke2lbl        SETLL     fnlbl02l
217200000000     c     ke2lbl        READE     fnlbl02l                               99
217300000000if  3c                   IF        *in99                                        *non ha figlie
217400000000     c                   LEAVE                                                  *esce dal ciclo
217500000000e   3c                   ENDIF
217600000000x   2c                   ELSE                                                   *sempre stessa mamma
217700000000     c                   LEAVE                                                  *esce dal ciclo
217800000000e   2c                   ENDIF
217900000000e   1c                   ENDDO                                                  *fine mamme
218000000000     c*
218100000000     c                   ENDSR
218200000000     c*--------------------------------------------------------------------------------------------*
218300000000     c* memorizza il contrassegno
218400000000     c*--------------------------------------------------------------------------------------------*
218500000000     c     memcsb        BEGSR
218600000000     c*
218700000000     c* legge il contrassegno, se avuto
218800000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
218900000000     c                   Z-ADD     waas          ksbaas
219000000000     c                   Z-ADD     wlnp          ksblnp
219100000000     c                   Z-ADD     wnrs          ksbnrs
219200000000     c                   Z-ADD     wnsp          ksbnsp
219300000000     c     keycsb        CHAIN     tncsb03l                           99
219400000000if  2c                   IF        NOT *in99                                    *esistente
219500000000     c* memorizza i dati
219600000000     c                   MOVEL     'S'           cao74                          *esiste C/A
219700000000     c*
219800000000     c                   Z-ADD     csbcas        caimpo74                       *importo
219900000000     c*
220000000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
220100010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
220200010820     c                             caimpo74 > *zeros
220300010820     c                   EVAL      cadivo74 = 'ITL'
220400010820e   3c                   ENDIF
220500100119     ** Visualizzo la descrizione del tipo incasso contrassegno solo se il
220600100119     ** contrassegno è stato incassato.
220700100121     C                   EVAL      cainco74 =
220800100121     C                                       GetDescrizioneIncassoContrassegno()
220900000000     c*
221000000000     c                   MOVEL     *blanks       castao74                       *stato
221100060622     C                   EVALR     tblKey = %CHAR(csbSta)
221200060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
221300060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
221400060622     C                             :rpyOpCode:rpyEsito)
221500060622     C                   IF        rpyOpCode = 'FOUND'
221600060622     c                   EVAL      castao74 = §4SDei
221700000000e   3c                   ENDIF
221800100119     c
221900000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
222000011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
222100060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
222200000000x   4c                   ELSE
222300000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
222400060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
222500000000x   5c                   ELSE
222600060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
222700000000e   5c                   ENDIF
222800000000e   4c                   ENDIF
222900000000e   3c                   ENDIF
223000000000     c*
223100000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
223200000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
223300000000if  4c                   IF        csbtpa = 'B'
223400060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
223500000000e   4c                   ENDIF
223600000000if  4c                   IF        csbtpa = 'C'
223700060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
223800000000e   4c                   ENDIF
223900000000x   3c                   ELSE                                                   *incasso Bartolini
224000000000if  4c                   IF        csbfpc = *blanks
224100110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
224200000000e   4c                   ENDIF
224300000000if  4c                   IF        csbfpc = '2'
224400060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
224500000000e   4c                   ENDIF
224600000000e   3c                   ENDIF
224700000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
224800000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
224900000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
225000000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
225100000000     c                             csbfpc = *blanks                              assegno di traenza
225200060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
225300000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
225400060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
225500000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
225600060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
225700000000e   4c                   ENDIF
225800000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
225900060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
226000060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
226100000000e   3c                   ENDIF
226200000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
226300000000     c                   Z-ADD     *zeros        caabio74                       *abi
226400000000     c                   Z-ADD     *zeros        cacabo74                       *cab
226500000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
226600000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
226700000000     c                   MOVEL     csbddp        wdat
226800000000     c                   EXSR      edtdat
226900000000     c                   EVAL      cadato74 = wdate
227000000000     C*
227100000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
227200000000     c                   Z-ADD     csbabi        caabio74
227300000000     c                   Z-ADD     csbcab        cacabo74
227400000000     c                   MOVEL     csbnra        canumo74
227500120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
227600120321     c                             : caCabO74)
227700000000x   4c                   ELSE                                                   *incasso Bartolini
227800000000     c                   MOVEL     csbndp        canumo74
227900000000e   4c                   ENDIF
228000000000e   3c                   ENDIF
228100110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
228200110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
228300110307     C                   IF        IsContrassegnoCompensato()
228400110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
228500110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
228600110304     C                   CLEAR                   caabio74
228700110304     C                   CLEAR                   cacabo74
228800110304     C                   CLEAR                   canumo74
228900110304     C                   ENDIF
229000110304     C*
229100000000e   2c                   ENDIF
229200000000e   1c                   ENDIF
229300000000     c*
229400000000     c                   ENDSR
229500000000     c*--------------------------------------------------------------------------------------------*
229600000000     c* imposta i campi della DS di Output dalla giacenza
229700000000     c*--------------------------------------------------------------------------------------------*
229800000000     c     impdsogcp     BEGSR
229900000000     c*
230000000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
230100000000     c                   EVAL      waas = s_tasaas                              *bolla Input
230200000000     c                   EVAL      wlnp = s_taslnp
230300000000     c                   EVAL      wnrs = s_tasnrs
230400000000     c                   EVAL      wnsp = s_tasnsp
230500000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
230600000000     c                   EXSR      memgcp
230700000000     c*
230800000000     c                   ENDSR
230900000000     c*--------------------------------------------------------------------------------------------*
231000000000     c* memorizza la giacenza
231100000000     c*--------------------------------------------------------------------------------------------*
231200000000     c     memgcp        BEGSR
231300000000     c*
231400000000     c* imposta le variabili di lavoro
231500000000     c                   CLEAR                   i                              *indice
231600000000     c                   CLEAR                   ii
231700000000     c                   CLEAR                   smdmc                          *note fase -20-
231800000000     c                   CLEAR                   smdmcR
231900000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
232000000000     c                   MOVEL     'N'           gco74                          *flag di controllo
232100000000     c                   MOVEL     'N'           gcfdio74
232200000000     c                   MOVEL     'N'           gcmito74
232300000000     c*
232400000000     c* legge la giacenza, se avuta
232500000000     c                   Z-ADD     waas          kcpaas
232600000000     c                   Z-ADD     wlnp          kcplnp
232700000000     c                   Z-ADD     wnrs          kcpnrs
232800000000     c                   Z-ADD     wnsp          kcpnsp
232900000000     c                   Z-ADD     *zeros        kcpfrg
233000050309     c     keygcp        CHAIN     tigcp51l                           99
233100050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
233200050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
233300000000if  2c                   IF        NOT *in99        AND                         *esistente
233400000000     c                             gcpatb = *blanks AND                         *no annullata
233500050415     c                             gcptcm <> 'Z'    AND
233600050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
233700050419     C
233800050419     C                   EVAL      gcRiapO74 = gcpRiap
233900050418     **?Descrizione della fase.
234000050418     C                   SELECT
234100050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
234200060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
234300050418     C                   WHEN      gcpFas = 20
234400060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
234500050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
234600060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
234700050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
234800060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
234900050418     C                   ENDSL
235000000000     c* memorizza i dati dalle NOTE GIACENZE
235100000000     c                   Z-ADD     *zeros        i
235200000000     c                   Z-ADD     *zeros        ii
235300000000     c                   Z-ADD     gcpagc        knpagc
235400000000     c                   Z-ADD     gcpfgc        knpfgc
235500000000     c                   Z-ADD     gcpngc        knpngc
235600000000     c                   Z-ADD     gcpfrg        knpfrg
235700050309     c     keygnp        SETLL     tignp01l
235800050309     c     keygnp        READE     tignp01l                               98
235900000000do  3c                   DOW       NOT *in98
236000050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
236100000000     c* fase -10-
236200000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
236300000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
236400000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
236500000000e   5c                   ENDIF
236600000000     c* fase -20-
236700000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
236800000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
236900051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
237000000000     c                   ADD       1             i
237100000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
237200000000e   6c                   ENDIF
237300000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
237400051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
237500050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
237600050405     c                   CLEAR                   ii
237700050405     c                   CLEAR                   smdmcR
237800050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
237900050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
238000050919     C                   IF        NOT %ERROR
238100140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
238200050919     C                   ENDIF
238300050405     C                   ELSE
238400000000     c                   ADD       1             ii
238500000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
238600050405     C                   ENDIF
238700000000e   6c                   ENDIF
238800000000e   5c                   ENDIF
238900000000e   4c                   ENDIF
239000050309     c     keygnp        READE     tignp01l                               98
239100000000e   3c                   ENDDO
239200000000     c*
239300000000     c* memorizza i dati dalle GIACENZE
239400000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
239500050404     c* Disposizione da immettere.
239600051110     c                   IF        gcpfas = 20
239700051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
239800051110     C                   EXSR      chkTivpi
239900051110e   3c                   ENDIF
240000000000     c*
240100000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
240200000000     c*
240300000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
240400000000     c*
240500060103     c                   Z-ADD     gcpagc        dsannB
240600060103     c                   Z-ADD     gcpmgc        dsmgsB
240700060103     c                   Z-ADD     dsdatB        wdat
240800000000if  3c                   IF        wdat > *zeros
240900000000     c                   EXSR      edtdat
241000000000     c                   EVAL      gcdago74 = wdate                             *data apertura
241100000000e   3C                   ENDIF
241200060104     c*
241300060104     c                   Z-ADD     gcpdur        wdat
241400060104     c                   IF        wdat > *zeros
241500060104     c                   EXSR      edtdat
241600060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
241700060104     C                   ENDIF
241800000000     c*
241900000000if  3c                   IF        gcpcmc <> *blanks
242000000000     c                   MOVEL     gcpcmc        wcev
242100000000     c                   EXSR      deccevgia
242200030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
242300000000e   3C                   ENDIF
242400000000     c*
242500000000     c                   MOVEL     *blanks       gcdcgo74
242600000000if  3c                   IF        gcpdcg > *zeros
242700000000     c                   Z-ADD     gcpdcg        wdat
242800000000     c                   EXSR      edtdat
242900000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
243000000000e   3c                   ENDIF
243100000000     c*---
243200000000     c* Disposizioni già immesse
243300000000     c*---
243400000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
243500000000     c*
243600000000if  4c                   IF        gcpddm > *zeros
243700000000     c                   Z-ADD     gcpddm        wdat
243800000000     c                   EXSR      edtdat
243900000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
244000000000e   4C                   ENDIF
244100000000     c*
244200000000if  4c                   IF        gcpdis <> *blanks
244300060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
244400060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
244500060626     C                             :rpyOpCode:rpyEsito)
244600060622     C                   EVAL      gcdiso74 = §2DDes
244700000000e   4C                   ENDIF
244800000000     c*
244900000000if  4c                   IF        gcpasg <> *blanks
245000000000if  5c                   IF        gcpasg = 'S'
245100060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
245200000000x   5c                   ELSE
245300060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
245400000000e   5C                   ENDIF
245500000000     c*
245600000000if  5c                   IF        gcppsg = 'M'
245700060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
245800000000e   5C                   ENDIF
245900000000if  5c                   IF        gcppsg = 'D'
246000060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
246100000000e   5C                   ENDIF
246200000000e   4C                   ENDIF
246300000000     c*
246400000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
246500000000     c                   MOVEL     gcpvcs        gcvcso74
246600000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
246700000000     c                   Z-ADD     gcpcas        gccaso74
246800000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
246900000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
247000000000e   5C                   ENDIF
247100000000e   4C                   ENDIF
247200050404     C                   IF        smdmc(1) <> *BLANK
247300050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
247400050404     C                             smdmc(3) + smdmc(4)
247500051014     C                   EVAL      gcdmc5O74 = smdmc(5)
247600050404     C                   ELSE
247700050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
247800050404     C                             smdmcr(3) + smdmcr(4)
247900051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
248000050404e   3C                   ENDIF
248100000000e   3C                   ENDIF
248200000000     c*---
248300000000     c* Disposizioni da immettere
248400000000     c*---
248500010719     c* se le disposizioni sono da immettere controlla che:
248600010719     c* - il cliente è il mittente della giacenza
248700000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
248800040726if  4C                   IF        keygiac <> *blanks
248900040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
249000040726x   4c                   ELSE
249100090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
249200090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
249300090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
249400090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
249500090914     C                             )
249600040726e   4C                   ENDIF
249700000000e   3C                   ENDIF
249800050404     C* Controllo esistenza storia giacenza.
249900050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
250000050404     C                   DO        *HIVAL
250100050404     C     K04GCP51      READE     TIGCP51L
250200050404     C                   IF        %EOF
250300050404     C                   LEAVE
250400050404     C                   ENDIF
250500050404     C                   IF        GCPATB = *BLANK
250600050404     C                   EVAL      StoGiaO74 = 'S'
250700050404     C                   LEAVE
250800050404     C                   ENDIF
250900050404     C                   ENDDO
251000050404     c*
251100000000e   2c                   ENDIF
251200000000     c*
251300000000     c                   ENDSR
251400000000     c*--------------------------------------------------------------------------------------------*
251500000000     c* imposta i campi della DS di Output dalle note
251600000000     c*--------------------------------------------------------------------------------------------*
251700000000     c     impdsonot     BEGSR
251800000000     c*
251900000000     c* azzera le variabili di lavoro
252000000000     c                   CLEAR                   i
252100000000     c                   CLEAR                   mdcre                          consegna richiesta
252200000000     c                   CLEAR                   mhcre
252300030210     c                   CLEAR                   mtcre
252400000000     c                   CLEAR                   mftce                          consegna particolare
252500000000     c                   CLEAR                   wftc
252600000000     c                   CLEAR                   d1wdftc
252700000000     c                   CLEAR                   d2wdftc
252800000000     c                   CLEAR                   mgcxe                          turni chiusura
252900000000     c                   CLEAR                   wgcx
253000000000     c                   CLEAR                   d1wdgcx
253100000000     c                   CLEAR                   d2wdgcx
253200010927     c                   CLEAR                   mffd                           fermo deposito
253300010927     c                   CLEAR                   mmncB                          *motivi NON consegna
253400010927     c                   CLEAR                   mmncC
253500010927     c                   CLEAR                   mmncG
253600010927     c                   CLEAR                   mmncH
253700010927     c                   CLEAR                   mmncN
253800010927     c                   CLEAR                   mmncO
253900050701     c                   CLEAR                   mmnc8
254000050701     c                   CLEAR                   mmnc9
254100050701     c                   CLEAR                   a230
254200000000     c                   CLEAR                   we
254300030206     c                   CLEAR                   mtel                           n° tel destinatario
254400030206     c                   CLEAR                   mban                           bancali
254500030207     c                   CLEAR                   mscr                           supermercati
254600030210     c                   CLEAR                   msdace
254700030210     c                   CLEAR                   msorce
254800030210     c                   CLEAR                   msdcre
254900030210     c                   CLEAR                   mshcre
255000030210     c                   CLEAR                   mstcre
255100030210     c                   CLEAR                   msnom
255200030210     c                   CLEAR                   mscrT
255300030210     c                   CLEAR                   wmscrT
255400030210     c                   CLEAR                   s
255500030207     c
255600140619     c*imposta dati data consegna richiesta
255700140716     c                   if        f_tasdcr > 0
255800140716     c                   MOVEL     f_tasdcr      wdat
255900140619     c                   EXSR      edtdat
256000140619     c                   eval      DTCONRCO74  =  wdate
256100140819if  3c                   IF        f_tastcr = ' '
256200140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
256300140819e   3c                   ENDIF
256400140819     c                   endif
256500140716     c                   if        f_tashcr > 0
256600140716     c                   MOVEL     f_tashcr      wora
256700140619     c                   EXSR      edtora
256800140619     c                   eval      ORACONRO74  =  worae
256900140619     c                   endif
257000140716     c                   eval      TPDATCRO74  =  f_tastcr
257100140619      *reperimento descrizione tipo consegna richiesta
257200140716if  3c                   IF        f_tastcr = 'P'
257300140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
257400140619e   3c                   ENDIF
257500140716if  3c                   IF        f_tastcr = 'D'
257600140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
257700140619e   3c                   ENDIF
257800000000     c*---
257900000000     c* memorizza le "note" che sono presenti nel record bolla
258000000000     c*---
258100000000     c* Consegna richiesta
258200140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
258300140619     c*m                           s_tashcr = *zeros
258400140619x   1c*m                 ELSE                                                   *SI cons.particolare
258500140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
258600140619     c*m                 MOVEL     s_tasdcr      wdat
258700140619     c*m                 EXSR      edtdat                                       *edita data
258800140619     c*m                 MOVEL     wdate         mdcre
258900140619e   2c*m                 ENDIF
259000140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
259100140619     c*m                 MOVEL     s_tashcr      wora
259200140619     c*m                 EXSR      edtora                                       *edita ora
259300140619     c*m                 MOVEL     worae         mhcre
259400140619e   2c*m                 ENDIF
259500140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
259600140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
259700140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
259800140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
259900140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
260000140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
260100140619e   3c*m                 ENDIF
260200140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
260300140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
260400140619e   3c*m                 ENDIF
260500140619if  3c*m                 IF        s_tastcr = ' '                               *il
260600140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
260700140619e   3c*m                 ENDIF
260800140619e   2c*m                 ENDIF
260900140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
261000140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
261100140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
261200140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
261300140619e   3c*m                 ENDIF
261400140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
261500140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
261600140619e   3c*m                 ENDIF
261700140619if  3c*m                 IF        s_tastcr = ' '                               *il
261800140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
261900140619e   3c*m                 ENDIF
262000140619e   2c*m                 ENDIF
262100140619     c*m
262200140619if  2c*m                 IF        i < 10                                       *se c'è spazio
262300140619     c*m                 ADD       1             i                              *memorizza nota
262400140619     c*m                 EVAL      smnot(i) =
262500140619     c*m                           %TRIM(mtcre) +
262600140619     c*m                           ' '          +
262700140619     c*m                           %TRIM(mdcre) +
262800140619     c*m                           ' '          +
262900140619     c*m                           %TRIM(mhcre)
263000140619e   2c*m                 ENDIF
263100140619e   1c*m                 ENDIF
263200000000     c*
263300000000     c* Particolarità consegna -uno/due-
263400140716if  1c                   IF        f_tasftc <> *blanks OR
263500140716     c                             f_tastc2 <> *blanks
263600140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
263700000000     c                   EXSR      decftc
263800000000     c                   MOVEL     wdftc         d1wdftc
263900140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
264000000000     c                   EXSR      decftc
264100000000     c                   MOVEL     wdftc         d2wdftc
264200000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
264300000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
264400060612     c                   EVAL      we = ' ' +
264500060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
264600000000x   2c                   ELSE
264700000000     c                   EVAL      we = *blanks
264800000000e   2c                   ENDIF
264900060612     c                   EVAL      mftce =
265000060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
265100060612     c                             %TRIM(d1wdftc)             +
265200060612     c                             we                         +
265300060612     c                             %TRIM(d2wdftc)
265400000000     c*
265500010927if  2c                   IF        i < 10                                       *se c'è spazio
265600000000     c                   ADD       1             i                              *memorizza nota
265700000000     c                   EVAL      smnot(i) = mftce
265800010927e   2c                   ENDIF
265900010927e   1c                   ENDIF
266000000000     c*
266100000000     c* Chiusura per turno
266200140716if  1c                   IF        f_tasgc1 <> *blanks OR
266300140716     c                             f_tasgc2 <> *blanks
266400140716if  2c                   IF        f_tasgc1 <> *blanks
266500140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
266600000000     c                   EXSR      decgcx
266700000000     c                   MOVEL     wdgcx         d1wdgcx
266800000000e   2c                   ENDIF
266900140716if  2c                   IF        f_tasgc2 <> *blanks
267000140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
267100000000     c                   EXSR      decgcx
267200000000     c                   MOVEL     wdgcx         d2wdgcx
267300000000e   2c                   ENDIF
267400000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
267500000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
267600060612     c                   EVAL      we = ' ' +
267700060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
267800000000x   2c                   ELSE
267900000000     c                   EVAL      we = *blanks
268000000000e   2c                   ENDIF
268100060612     c                   EVAL      mgcxe =
268200060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
268300060612     c                             %TRIM(d1wdgcx)       +
268400060612     c                             we                   +
268500060612     c                             %TRIM(d2wdgcx)
268600000000     c*
268700010927if  2c                   IF        i < 10                                       *se c'è spazio
268800000000     c                   ADD       1             i                              *memorizza nota
268900000000     c                   EVAL      smnot(i) = mgcxe
269000010927e   2c                   ENDIF
269100010927e   1c                   ENDIF
269200000000     c*
269300000000     c* Fermo deposito
269400140716if  1c                   IF        f_tasffd = 'S'
269500060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
269600000000     c*
269700010927if  2c                   IF        i < 10                                       *se c'è spazio
269800010927     c                   ADD       1             i                              *memorizza nota
269900010927     c                   EVAL      smnot(i) = mffd
270000010927e   2c                   ENDIF
270100010927e   1c                   ENDIF
270200030206     c*---
270300030206     c* memorizza le "note" che NON sono presenti nel record bolla
270400030206     c* ma sul file estensione "anagrafiche"
270500030206     c*---
270600030206     c* N° telefono destinatario
270700140716     c                   Z-ADD     f_tasaas      kaaaas
270800140716     c                   Z-ADD     f_taslnp      kaalnp
270900140716     c                   Z-ADD     f_tasnrs      kaanrs
271000140716     c                   Z-ADD     f_tasnsp      kaansp
271100030206     c                   MOVEL     'O'           kaatrc
271200030206     c     keytaa        CHAIN     titaa30c                           98
271300030206if  1c                   IF        NOT *in98
271400040330     C                   EVAL      MitOrigO74 = TAARSC
271500030206e   1c                   ENDIF
271600030206     c*---
271700030206     c* memorizza le "note" che NON sono presenti nel record bolla
271800030206     c* ma sul file estensione testata boolle
271900030206     c*---
272000030206     c* Bancali
272100140716     c                   Z-ADD     f_tasaas      kr5aas
272200140716     c                   Z-ADD     f_taslnp      kr5lnp
272300140716     c                   Z-ADD     f_tasnrs      kr5nrs
272400140716     c                   Z-ADD     f_tasnsp      kr5nsp
272500030206     c                   MOVEL     'BAN'         kr5trd
272600040531     c     keyar5        CHAIN     fiar531c                           98
272700030206if  1c                   IF        NOT *in98
272800030206     c                   MOVEL     ar5uni        dar5ban
272900060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
273000060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
273100030206if  2c                   IF        §ar5rb1 > *zeros OR
273200030206     c                             §ar5rb2 > *zeros
273300060613     c                   EVAL      mban = %trim(mban) +
273400060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
273500060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
273600030206e   2c                   ENDIF
273700030206     c*
273800030206if  2c                   IF        i < 10                                       *se c'è spazio
273900030206     c                   ADD       1             i                              *memorizza nota
274000030206     c                   EVAL      smnot(i) = mban
274100030206e   2c                   ENDIF
274200030206e   1c                   ENDIF
274300040330
274400040330     **?Reperisco referente
274500030206     c* Note supermercati
274600140716if  1c     f_tasftc      IFEQ      'S'
274700140716     c     f_tastc2      OREQ      'S'
274800030210     c                   CLEAR                   s                              *indice
274900140716     c                   Z-ADD     f_tasaas      kr5aas
275000140716     c                   Z-ADD     f_taslnp      kr5lnp
275100140716     c                   Z-ADD     f_tasnrs      kr5nrs
275200140716     c                   Z-ADD     f_tasnsp      kr5nsp
275300030206     c                   MOVEL     'SCR'         kr5trd
275400040531     c     keyar5        SETLL     fiar531c
275500040531     c     keyar5        READE     fiar531c                               98
275600030210do  2c                   DOW       NOT *in98
275700030206     c                   MOVEL     ar5uni        dar5scr
275800030207     c* data nota
275900030210if  3c                   IF        ar5dac > *zeros
276000030207     c                   MOVEL     ar5dac        wdat
276100030207     c                   EXSR      edtdat                                       *edita data
276200030210     c                   MOVEL     wdate         msdace
276300030210e   3c                   ENDIF
276400030207     c* ora nota
276500030210if  3c                   IF        ar5orc > *zeros
276600030207     c                   MOVEL     ar5orc        wora
276700030207     c                   EXSR      edtora                                       *edita ora
276800030210     c                   MOVEL     worae         msorce
276900030210e   3c                   ENDIF
277000030207     c* Consegna richiesta
277100140708if  3c                   IF        §ar5dcr = *zeros AND
277200140708     c                             §ar5hcr = *zeros
277300140708x   3c                   ELSE
277400140708if  4c                   IF        §ar5dcr > *zeros
277500140708     c                   MOVEL     §ar5dcr       wdat
277600140708     c                   EXSR      edtdat                                       *edita data
277700140708     c                   MOVEL     wdate         msdcre
277800140708e   4c                   ENDIF
277900140708if  4c                   IF        §ar5hcr > *zeros
278000140708     c                   MOVEL     §ar5hcr       wora
278100140708     c                   EXSR      edtora                                       *edita ora
278200140708     c                   MOVEL     worae         mshcre
278300140708e   4c                   ENDIF
278400140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
278500140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
278600140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
278700140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
278800140708if  5c                   IF        §ar5tcr = 'P'                                *prima
278900140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
279000140708e   5c                   ENDIF
279100140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
279200140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
279300140708e   5c                   ENDIF
279400140708if  5c                   IF        §ar5tcr = ' '                                *il
279500140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
279600140708e   5c                   ENDIF
279700140708e   4c                   ENDIF
279800140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
279900140708     c                             §ar5dcr = *zeros                             *NO data richiesta
280000140708if  5c                   IF        §ar5tcr = 'P'                                *prima
280100140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
280200140708e   5c                   ENDIF
280300140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
280400140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
280500140708e   5c                   ENDIF
280600140708if  5c                   IF        §ar5tcr = ' '                                *il
280700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
280800140708e   5c                   ENDIF
280900140708e   4c                   ENDIF
281000030210e   3c                   ENDIF
281100030210     c* Riferimento
281200030210if  3c                   IF        §ar5nom <>  *blanks
281300060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
281400060613     c                             + ' ' + §ar5nom
281500030210e   3c                   ENDIF
281600030210     c*
281700030210if  3c                   IF        s < 5                                        *MAX 5 note
281800030210     c                   ADD       1             s
281900030210     c                   EVAL      mscrT(s) = 'Il ' +
282000030210     c                             %trim(msdace) +                              *data nota
282100030210     c                             ' '    +
282200030210     c                             %trim(msorce) +                              *ora nota
282300030210     c                             ' '    +
282400140708     c                             %trim(mstcre) +                              *tipo consegna
282500140708     c                             ' '    +
282600140708     c                             %trim(msdcre) +                              *data consegna
282700140708     c                             ' '    +
282800140708     c                             %trim(mshcre) +                              *ora consegna
282900140708     c                             ' '    +
283000030210     c                             %trim(msnom)  +                              *parlato con
283100030210     c                             ' '    +
283200030210     c                             %trim(§ar5tel) +                             *telefono
283300030210     c                             ' '    +
283400030210     c                             %trim(§ar5mot)                               *motivo
283500030210e   2c                   ENDIF
283600030210     c*
283700040531     c     keyar5        READE     fiar531c                               98
283800030206e   2c                   ENDDO
283900030210e   1c                   ENDIF
284000030210     c* NOTA completa
284100030210if  1c                   IF        mscrT(1) <> *blanks
284200030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
284300030211     c                                      ' ' +
284400030211     C                                      %trim(mscrT(2)) +
284500030211     c                                      ' ' +
284600030211     C                                      %trim(mscrT(3)) +
284700030211     c                                      ' ' +
284800030211     C                                      %trim(mscrT(4)) +
284900030211     c                                      ' ' +
285000030211     C                                      %trim(mscrT(5))
285100030210     c                   Z-ADD     *zeros        s
285200030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
285300030210     c                   ADD       1             i
285400030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
285500030210     c                   EVAL      s = s + 100
285600030210e   2c                   ENDDO
285700030210     c*
285800030210e   1c                   ENDIF
285900010927     c*---
286000010927     c* memorizza le "note" che NON sono presenti nel record bolla
286100030206     c* ma sul file estensione "riferimenti"
286200010927     c*---
286300010927     c* motivi della NON consegna
286400010927     c                   CLEAR                   tita4000
286500010927     c                   CLEAR                   tita4p00
286600140716     c                   Z-ADD     f_tasaas      ka4aas
286700140716     c                   Z-ADD     f_taslnp      ka4lnp
286800140716     c                   Z-ADD     f_tasnrs      ka4nrs
286900140716     c                   Z-ADD     f_tasnsp      ka4nsp
287000010927     c     ke2ta4        SETLL     tita430c
287100010927     c     ke2ta4        READE     tita430c                               98
287200010927do  1c                   DOW       NOT *in98
287300140708     c                   if        ta4trc = '8' OR ta4trc = '9'
287400050701     c*
287500050701if  3c                   IF        ta4trc = '8'
287600050701     c                   EVAL      mmnc8 = ta4not
287700050701e   3c                   ENDIF
287800050701if  3c                   IF        ta4trc = '9'
287900050701     c                   EVAL      mmnc9 = ta4not
288000050701e   3c                   ENDIF
288100050701     c*
288200010927e   2c                   ENDIF
288300010925     c*
288400010926     c     ke2ta4        READE     tita430c                               98
288500010926e   1c                   ENDDO
288600050701     C*
288700140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
288800050701     C*
288900050701     C                   IF        mmnc8 <> *BLANK
289000050701     C                   EVAL      a230 = %TRIML(mmnc8)
289100050701     C                   ENDIF
289200050701     C                   IF        mmnc9 <> *BLANK
289300050701     C                   IF        a230 <> *BLANK
289400050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
289500050701     C                   ELSE
289600050701     C                   EVAL      a230 = %TRIML(mmnc9)
289700050701     C                   ENDIF
289800050701     C                   ENDIF
289900010927     C*
290000050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
290100050701     C*
290200010927if  2c                   IF        i < 10                                       *se c'è spazio
290300010927     c                   ADD       1             i                              *memorizza nota
290400010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
290500010927e   2c                   ENDIF
290600010927     C*
290700010927if  2c                   IF        i < 10                                       *se c'è spazio
290800010927if  3c                   IF        n3 > 100
290900010927     c                   ADD       1             i                              *memorizza nota
291000010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
291100010927e   3c                   ENDIF
291200010927e   2c                   ENDIF
291300010927     C*
291400010927if  2c                   IF        i < 10                                       *se c'è spazio
291500010927if  3c                   IF        n3 > 200
291600010927     c                   ADD       1             i                              *memorizza nota
291700010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
291800010927e   3c                   ENDIF
291900010927e   2C                   ENDIF
292000010927e   1c                   ENDIF
292100010925     c*
292200000000     c* imposta contatore note da visualizzare nella DS di Output
292300000000     c                   EVAL      cntnoto74 = i
292400000000     c*
292500000000     c* imposta le note nella DS di Output
292600000000do  1c     1             DO        10            j
292700000000     c                   EVAL      nota(j) = smnot(j)
292800000000e   1c                   ENDDO
292900000000     c*
293000000000     c                   ENDSR
293100010716     c*--------------------------------------------------------------------------------------------*
293200010716     c* imposta i campi della DS di Output dalla firma DPD
293300010716     c*--------------------------------------------------------------------------------------------*
293400010716     c     impdsoDPD     BEGSR
293500010717     c*
293600010717     c* reimposta variabili di lavoro
293700010717     c                   RESET                   $AbFirma
293800010717     c                   CLEAR                   depute                         *deposito utente
293900010717     c                   CLEAR                   deppwd                         *deposito password
294000010717     c*---
294100010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
294200010717     c*---
294300010717     c                   Z-ADD     s_taslna      worg
294400010717     c                   EXSR      decorg
294500020702     c                   IF        wodpd = 'DPD'
294600020702     c                   MOVEL     'S'           dpdo74
294700020702     c                   ELSE
294800020702     c                   MOVEL     *blanks       dpdo74
294900020702     c                   ENDIF
295000010717     c*
295100010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
295200060627     c* Riferimento link Lettera di vettura
295300060627     c* Riferimento link Firma
295400010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
295500010717     c                   CLEAR                   tita4000
295600010717     c                   CLEAR                   tita4p00
295700060627     c                   CLEAR                   dsbl4i
295800010717     c                   Z-ADD     s_tasaas      ka4aas
295900010717     c                   Z-ADD     s_taslnp      ka4lnp
296000010717     c                   Z-ADD     s_tasnrs      ka4nrs
296100010717     c                   Z-ADD     s_tasnsp      ka4nsp
296200060627     c                   MOVEL     'I'           ka4trc
296300060627     c     keyta4        CHAIN     tita430c                           98
296400010717if  2c                   IF        NOT *in98
296500060627     c                   MOVEL     ta4not        dsbl4i
296600060627     c                   EVAL      DPDRmnO74 = §b4ipn
296700060627     c                   EVAL      DPDFnpO74 = §b4ipn
296800010717e   2c                   ENDIF
296900010717     c*
297000010717     c* cerca abilitazione firma DPD per questo segnacollo
297100010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
297200010717     c                   EXSR      RepAbilFirma
297300010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
297400010717     c*---
297500010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
297600010717     c*---
297700010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
297800010717     c* dati
297900010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
298000010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
298100010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
298200010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
298300010717     c*
298400010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
298500010717     c                   Z-ADD     s_tasaas      kvbaas
298600010717     c                   Z-ADD     s_taslnp      kvblnp
298700010717     c                   Z-ADD     s_tasnrs      kvbnrs
298800010717     c                   Z-ADD     s_tasnsp      kvbnsp
298900010717     c                   Z-ADD     99999999      kvbdev
299000010717     c                   Z-ADD     9999          kvbhev
299100010717     c     ke2evb        SETGT     fnevb01l
299200010717     c     keyevb        READPE    fnevb01l                               98
299300010717do  4c                   DOW       NOT *in98
299400101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
299500010720     c*
299600060629     c                   EVAL      devB01 = evbNot
299700060628     c                   IF        §NotDep = *BLANK
299800060628     c                   RESET                   DPDFirO74
299900060629     c                   LEAVE
300000060628     c                   ENDIF
300100060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
300200060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
300300080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
300400010720     c                   Z-ADD     *zeros        g08dat
300500010720     c                   MOVEL     '3'           g08err
300600010720     c                   CALL      'XSRDA8'
300700010720     c                   PARM                    wlbda8
300800010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
300900010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
301000010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
301100010720     c*
301200010717     c                   LEAVE                                                  *uscita ciclo
301300010717e   5c                   ENDIF
301400010717     c     keyevb        READPE    fnevb01l                               98
301500010717e   4c                   ENDDO
301600010717     c*
301700010717e   3c                   ENDIF
301800010717e   2c                   ENDIF
301900010716e   1c                   ENDIF
302000010716     c*
302100010716     c                   ENDSR
302200140604     c*--------------------------------------------------------------------------------------------*
302300140604     c* imposta i campi da FNARB se TASDCM = 0
302400140604     c*--------------------------------------------------------------------------------------------*
302500140604     c     impdsoARB     BEGSR
302600150403     c                   clear                   orarich           4 0
302700150611     c                   clear                   wdee
302800150410     c* memorizzo se utilizzo la data prevista consegna
302900150410     c                   clear                   usaprev           1
303000140604     c*
303100140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
303200140604     c                   EVAL      lnaDescO74 = sodes(i)
303300140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
303400140604     c*
303500150116      *eliminata forzatura resta impostato dalla mamma per composizione link
303600150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
303700150116     c*                  MOVE      arbmgs        aasmgsO74
303800140818      *riempimento da verificare
303900140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
304000140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
304100140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
304200140818
304300140818     c                   if        tpporto74 = 'A'
304400140818     c                   if        arbccm > 0
304500140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
304600140818     c                   else
304700140818     c                   movel     arblnp        ccmO74
304800140818     c                   move      8888          ccmO74
304900140818     c                   endif
305000140818     c                   else
305100140818     c                   z-add     arbksc        ccmO74
305200140818     c                   endif
305300140818      *
305400140604     c                   MOVEL     arblna        lineaaro74
305500140604     c                   MOVEL     arbtsp        tpservio74
305600140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
305700140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
305800140604     C                             rpyOpCode:rpyEsito)
305900140604     C                   EVAL      corro74 = §5EDes
306000140604     c*
306100140604     c                   EVAL      destino74 = arbrsd
306200140604     c                   EVAL      desindo74 = arbind
306300140604     c                   EVAL      descapo74 = arbcad
306400140604     c                   EVAL      desloco74 = arblod
306500140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
306600140604if  1c                   IF        arbnzd    = *blanks
306700140604     c                   EVAL      desnazo74 = arbprd
306800140604x   1c                   ELSE
306900150326     c**                 EVAL      desnazo74 = arbnzd
307000150326     c**                 EVAL      desnzdo74 = arbnzd
307100150326     c* 26/03/15: Attiviamo le specifiche per provincia export
307200150326     c                   EVAL      desnazo74 = arbprd
307300150326     c                   EVAL      desnzdo74 = arbnzd
307400150326e   1c                   ENDIF
307500140604     c*
307600140604     c                   EVAL      collio74  = arbncl
307700140604     c                   EVAL      pesoo74   = arbpkf
307800140604     c                   EVAL      volumeo74 = arbvlf
307900140604     c                   EVAL      naturao74 = arbnas
308000140604     c*
308100140604     c                   EVAL      disnumo74  = arbndc
308200140604c    c                   MOVEL     arbddc        wdat
308300140604     c                   EXSR      edtdat
308400140604     c                   EVAL      disdatao74 = wdate
308500140604     c*
308600140604     c                   EVAL      mito74    = arbrsm
308700140604     c                   EVAL      mitindo74 = arbinm
308800140604     c                   EVAL      mitcapo74 = arbcam
308900140604     c                   EVAL      mitloco74 = arblom
309000140604      *specifiche da eliminare dopo la modifica in immissione bolle
309100140604if  3c                   IF        arbnzm = *blanks
309200140604     c                   EVAL      mitnazo74 = arbprm
309300140604x   3c                   ELSE
309400140604     c                   EVAL      mitnazo74 = arbnzm
309500140604e   3c                   ENDIF
309600140604     c* 2a ragione sociale destinatario.
309700140604     c                   CLEAR                   tita4000
309800140604     c                   CLEAR                   tita4p00
309900140604     c                   Z-ADD     arbaas        ka4aas
310000140604     c                   Z-ADD     arblnp        ka4lnp
310100140604     c                   Z-ADD     arbnrs        ka4nrs
310200140604     c                   Z-ADD     arbnsp        ka4nsp
310300140604     c                   MOVEL     'D'           ka4trc
310400140604     c     keyta4        CHAIN     fiar401l                           99
310500140604if  1c                   IF        NOT *in99
310600140604     c                   MOVEL     ar4not        desti2O74
310700140604e   1c                   ENDIF
310800140604     c                   MOVEL     'E'           ka4trc
310900140604     c     keyta4        CHAIN     fiar401l                           99
311000140604if  1c                   IF        NOT *in99
311100140604     c                   MOVEL     ar4not        dsbl4e
311200140604e   1c                   ENDIF
311300140604     c
311400140604     ** Riferimento mittente numerico (c'è sempre).
311500140604     C                   EVAL      rifero74 = arbrmn
311600140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
311700140604     C                   IF        arbrma <> *blank and
311800140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
311900140604     C                   EVAL      rifero74a = arbrma
312000140604     C                   ENDIF
312100140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
312200140604     C                   IF        §b4erp <> *BLANK AND
312300140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
312400140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
312500140604     C                   EVAL      rpto74 = §b4erp
312600140604     C                   ENDIF
312700140604
312800140604     **?Reperisco il check code della spedizione.
312900140604     C                   EVAL      SChkCdeO74 =
313000140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
313100140604     C                             NSpedizI74:ChkCode:nEsito)
313200140604
313300140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
313400141126     C*m                 IF        KSCI74 <> *BLANK
313500141126     c*m                 SETOFF                                           98
313600140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
313700141126     C*m                 RESET                   tis7700i0
313800141126     C*m                 EVAL      tis7700i0.aas = arbAas
313900141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
314000141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
314100141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
314200141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
314300141126     C*m                 EVAL      tis7700i0.ksu = kscI74
314400141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
314500141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
314600141126     C*m                           : rpyEsito
314700141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
314800141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
314900141126     C*m                           )
315000141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
315100141126     C*m                           tis7700o0.bollValida = *ON)
315200140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
315300141126if  2c*m                 IF        NOT *in98                                      *non trovato
315400141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
315500141126     C*m                 reset                   TIS778DSO
315600141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
315700141126     C*m                 RETURN
315800141126     C*m                 ENDIF
315900141126e   2c*m                 ENDIF
316000141126     C*m                 ENDIF
316100140604     c*
316200140604     ** Importo assicurato.
316300140604     C                   IF        arbIas > 0
316400140604     C                   EVAL      vasO74 = arbVas
316500140604     C                   EVAL      iasO74 = arbIas
316600140604     C                   ENDIF
316700140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
316800140605     c                   if        arbdcm = 0 and wcev <> '704'
316900140617     c                   if        arbdcr > 0
317000140605     c                   MOVEL     arbdcr        wdat
317100140605     c                   EXSR      edtdat
317200140605     c                   eval      DTCONRCO74  =  wdate
317300140819if  3c                   IF        arbtcr = ' '
317400140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
317500140819e   3c                   ENDIF
317600140617     c                   endif
317700140617     c                   if        arbhcr > 0
317800140617     c                   MOVEL     arbhcr        wora
317900140617     c                   EXSR      edtora
318000140617     c                   eval      ORACONRO74  =  worae
318100140617     c                   endif
318200140606     c                   eval      TPDATCRO74  =  arbtcr
318300140605      *reperimento descrizione tipo consegna richiesta
318400140617if  3c                   IF        arbtcr = 'P'
318500140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
318600140617e   3c                   ENDIF
318700140617if  3c                   IF        arbtcr = 'D'
318800140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
318900140617e   3c                   ENDIF
319000140605     C                   clear                   tnsd99ds
319100140605     C                   MOVEL     ' '           D98tla
319200140605     C                   MOVEL     'A'           D98tbo
319300140605     C                   Z-ADD     arbaas        D98aas
319400140605     C                   Z-ADD     arblnp        D98lnp
319500140605     C                   Z-ADD     arbnrs        D98nrs
319600140605     C                   Z-ADD     arbnsp        D98nsp
319700140605     C*
319800140605     C                   call      'TNSD99R'
319900140605     C                   parm                    tnsd99ds
320000150304     c
320100140605     C*
320200140917      * chiamata a trulors per tempi standard
320300140917     c                   clear                   trulorsds
320400140917     c                   clear                   trulor2ds
320500140917     c                   clear                   trulor3ds
320600150611     c                   eval      wdee=d98dee
320700140917      * solo se questa data è valorizzata
320800150304    1c                   if        d98dee > 0
320900140912     c                   movel     d98dee        wdat
321000140605     c                   exsr      edtdat
321100140605     c                   eval      DTTEOCOO74  =  wdate
321200140819     c
321300140904      * se fermo deposito non richiamo pgm per reperimento orari
321400150304    2c                   if         arbffd = *blank
321500140605     c                   eval       IOREtser = 'C'
321600140605     c                   eval       IOREDDC=arbddc
321700140605     c                   eval       IOREnDC=arbndc
321800140609     c                   eval       IOREfil=arblna
321900140609     c                   eval       IOREdta=d98dce
322000140609     c                   eval       IOREcap=arbcad
322100140609     c                   eval       IOREloc=arblod
322200140609     c                   eval       IOREtfp=arbtfp
322300140609     c                   eval       IOREtfa=arbtfa
322400140610     c                   eval       IOREdti=arbdti
322500140610     c                   eval       IOREhti=arbhti
322600140610     c                   eval       IOREdcr=arbdcr
322700140610     c                   eval       IOREhcr=arbhcr
322800140610     c                   eval       IOREtcr=arbtcr
322900140819     c                   eval       IOREtsp=arbtsp
323000140819     c                   eval       IOREnar=arbnzd
323100140819     c                   eval       IOREdei=d98dei
323200140819     c                   eval       IOREoei=d98oei
323300150325     c* passo la località normalizzata
323400150325     c                   clear                   tita4000
323500150325     c                   clear                   tita4p00
323600150325     c                   Z-ADD     arbaas        ka4aas
323700150325     c                   Z-ADD     arblnp        ka4lnp
323800150325     c                   Z-ADD     arbnrs        ka4nrs
323900150326     c                   Z-ADD     arbnsp        ka4nsp
324000150325     c                   MOVEL     'X'           ka4trc
324100150325     c     keyta4        CHAIN     tita430c                           99
324200150325if  1c                   IF        NOT *in99
324300150325     c                   MOVEL     ta4not        OOR3LOC_N
324400150325     c                   else
324500150325     c                   movel     arblod        OOR3LOC_N
324600150325e   1c                   ENDIF
324700140605     c
324800150304    3c                   IF         (ARBfbc = *blanks and
324900140605     c                               ARBddc > 0 and
325000140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
325100140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
325200140605     c                   eval       IOREcons = 'S'
325300150304    3c                   endif
325400140605     c* Orario presunto ritiro/consegna
325500140605     c                   move      arbndc        pctndc
325600140605     c                   move      arbifp        pctfgs
325700140605     c                   move      'COR'         ttrd
325800140605     c     kcet          setgt     fipct02l
325900140605     c     kcet          readpe    fipct02l
326000150304    3c                   if        not %eof(fipct02l)
326100140605     c                   movel     pctdati       fipctcords
326200140605     c                   eval      ioreorp=§PCTORASTI
326300140605     c                   endif
326400140605     c                   call      'TRULORSR'
326500140605     C                   PARM                    kpjba
326600140605     C                   PARM                    trulorsds
326700140605     C                   PARM                    trulor2ds
326800140605     C                   PARM                    trulor3ds
326900150304    3c                   endif
327000140818     c                   clear                   ORSTINIO74
327100150304    3c                   if        OOR2STIS > 0
327200140617     c                   MOVEL     OOR2STIS      wora
327300140617     c                   EXSR      edtora
327400140617     c                   eval      ORSTINIO74  =  worae
327500150304    3c                   endif
327600140818     c                   clear                   ORSTFINO74
327700150304    3c                   if        OOR2STFS > 0
327800140617     c                   MOVEL     OOR2STFS      wora
327900140617     c                   EXSR      edtora
328000140617     c                   eval      ORSTFINO74  =  worae
328100150304    3c                   endif
328200140818     c                   clear                   ORMIINIO74
328300150304    3c                   if        OOR2MIIS > 0
328400140617     c                   MOVEL     OOR2MIIS      wora
328500140617     c                   EXSR      edtora
328600140617     c                   eval      ORMIINIO74  =  worae
328700150304    3c                   endif
328800140818     c                   clear                   ORMAFINO74
328900150304    3c                   if        OOR2MXFS > 0
329000140617     c                   MOVEL     OOR2MXFS      wora
329100140617     c                   EXSR      edtora
329200140617     c                   eval      ORMAFINO74  =  worae
329300150304    3c                   endif
329400150304    3c                   if        wricons = 'S'
329500140818     c                   clear                   ORRIDALO74
329600150304    4c                   if        OOR2RIDS > 0
329700140617     c                   MOVEL     OOR2RIDS      wora
329800140617     c                   EXSR      edtora
329900140617     c                   eval      ORRIDALO74  =  worae
330000150304    4c                   endif
330100140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
330200140818     c                   clear                   ORRIALLO74
330300150304    4c                   if        OOR2RIAS > 0
330400140617     c                   MOVEL     OOR2RIAS      wora
330500140617     c                   EXSR      edtora
330600140617     c                   eval      ORRIALLO74  =  worae
330700150304    4c                   endif
330800140617     c                   eval      FLRICONO74  =  OOR2FRIC
330900150304   x3c                   else
331000140711     c                   clear                   ORRIALLO74
331100140711     c                   clear                   ORRIDALO74
331200140711     c                   eval      FLRICONO74  =  'N'
331300150304    3c                   endif
331400140818     c                   clear                   ORSTICDO74
331500150304    3c                   if        OOR2PRESD > 0
331600140617     c                   MOVEL     OOR2PRESD     wora
331700140617     c                   EXSR      edtora
331800140617     c                   eval      ORSTICDO74  =  worae
331900150304    3c                   endif
332000140818     c                   clear                   ORSTICAO74
332100150304    3c                   if        OOR2PRESa > 0
332200140617     c                   MOVEL     OOR2PRESa     wora
332300140617     c                   EXSR      edtora
332400140617     c                   eval      ORSTICAO74  =  worae
332500150304    3c                   endif
332600140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
332700140605     c                   eval      FLLOCNOO74  =  OOR3NORM
332800140904      *end F.D.
332900150304    2c                   endif
333000140605      * RIFERIMENTI DESTINATARIO
333100140605     c                   MOVEL     'EMD'         kr5trd
333200140605     c     keyAR5        CHAIN     fiar501l
333300150304if  2c                   IF        %found(fiar501l)
333400140605     c                   MOVEL     ar5uni        dar5emd
333500140606     c                   eval      EMLDESTO74  =  §§AR5EML
333600140606     c                   eval      CELDESTO74  =  §§AR5TEL
333700150304e   2c                   ENDIF
333800140604     **
333900150304     c*
334000150304     c* Stato data prevista consegna
334100150305     c                   clear                   stat_n2o74
334200150305     c                   eval      STATUSo74   =  D98SPCDEE
334300150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
334400150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
334500150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
334600150312     c                   eval      statuso74='RIC'
334700150403     c* se presente FIARP --> visualizzo la data richiesta
334800150403     c     karp          chain     fiarp01l
334900150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
335000150611     c                   movel     arpdcr        wdee
335100150611     c                   movel     arpdcr        wdat
335200150403     c                   exsr      edtdat
335300150403     c                   eval      DTTEOCOO74  =  wdate
335400150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
335500150421    3C                   IF        ARPHCR>0
335600150403     c                   eval      orarich=arphcr
335700150421    3c                   endif
335800150421   x2c                   else
335900150403     c*  se presente DOPO IL imposto la data in D98dee
336000150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
336100150403     c                   eval      D98dee=arpdcr
336200150611     c                   eval      wdee=arpdcr
336300150421    3c                   endif
336400150403     c* PRIMA DEL come se non ci fosse...
336500150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
336600150421    3c                   if        d98dee>0
336700150312     c                   Clear                   xgiolavds
336800150312     c                   Eval      Ixglpa  = 'A'
336900150312     c                   Eval      IxglFil = arblna
337000150312     c                   Eval      Ixgldata = d98dee
337100150312     c                   Eval      Ixgladd = 'S'
337200150312     c                   Eval      Ixgllav = 'S'
337300150312     c                   eval      ixglgg  = 1
337400150312     c                   Call      'XGIOLAV'
337500150312     c                   parm                    xgiolavds
337600150611     c                   movel     oxgldata      wdee
337700150611     c                   movel     oxgldata      wdat
337800150312     c                   exsr      edtdat
337900150312     c                   eval      DTTEOCOO74  =  wdate
338000150421    3c                   endif
338100150421    2c                   endif
338200150421    1c                   endif
338300150421     c
338400150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
338500150421     c                   eval      statuso74='ATT'
338600150421     c                   endif
338700150421    1c                   if        PDAgia='S' and statuso74='DDC'
338800150421     c                   eval      statuso74='GIA'
338900150421     c                   endif
339000150401     c
339100150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
339200150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
339300150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
339400150401     c* La data contatto = data distinta + 1
339500150512     c**                 Clear                   xgiolavds
339600150512     c**                 Eval      Ixglpa  = 'A'
339700150512     c**                 Eval      IxglFil = arblna
339800150512     c**                 Eval      Ixgldata = arbddc
339900150512     c**                 Eval      Ixgladd = 'S'
340000150512     c**                 Eval      Ixgllav = 'S'
340100150512     c**                 eval      ixglgg  = 1
340200150512     c**                 Call      'XGIOLAV'
340300150512     c**                 parm                    xgiolavds
340400150512     c**                 movel     oxgldata      wdat
340500150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
340600150512     c**                 endif
340700150317     c
340800150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
340900150326     c*  lo status diventa RIC
341000150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
341100150317     c                   eval      statuso74='RIC'
341200150317     c                   endif
341300150310     c
341400150304     c* decodifica stato da mettere nei campi deco e note
341500150304     c                   clear                   dspc
341600150304     C                   RESET                   tibs02ds
341700150304     C                   EVAL      t02Cod = 'SPC'
341800150312     C                   EVAL      t02Ke1 = statuso74
341900150306     C                   EXSR      getTntbe00f
342000150306     c                   IF        t02Err <>'E'
342100150306     c
342200150305     c                   eval       stat_d1o74=§SPCDEC1
342300150305     c                   eval       stat_d2o74=§SPCDEC2
342400150304     c*
342500150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
342600150305     c                   eval       stat_n1o74=§SPCNOT1
342700150306     c                   eval       stat_n2o74=§SPCNOT2
342800150304     c                   else
342900150305     c                   clear                   stat_d1o74
343000150305     c                   clear                   stat_d2o74
343100150305     c                   clear                   stat_n1o74
343200150306     c                   clear                   stat_n2o74
343300150304     c                   endif
343400150409     c                   else
343500150409     c* stato evento CONSEGNATO
343600150409     c                   eval      statuso74='CON'
343700150415     c                   eval      flgbolcO74  =  'S'
343800150415     c                   eval      flgboldO74  =  'N'
343900150409    1c                   endif
344000150304     c
344100150304     c
344200140919     ** riempimento per campi da arb con data consegna diversa da 0
344300140919     c                   if        arbdcm > 0
344400140919     c                   movel     arbdcm        wdat
344500140919     c                   exsr      edtdat
344600140919     c                   eval      dtconmeo74  = wdate
344700140919     c                   MOVEL     arbdcm        wora
344800140919     c                   EXSR      edtora
344900140919     c                   eval      orconmeO74  =  worae
345000140919     c                   eval      flgbolcO74  =  'S'
345100140919     c                   eval      flgboldO74  =  'N'
345200141015      *firmatario distinta PDA
345300141015     c                   clear                   firmato74
345400141015     c                   if        gen§ar5pdaco <> 'NO'
345500141015     c                   exsr      repfirmatARB
345600141015     c                   else
345700141015     c                   if        arbgma <> *blank
345800141015     C                   EVAL      tblKey = arbgma
345900141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
346000141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
346100141015     C                             :rpyOpCode:rpyEsito)
346200141015     C                   IF        rpyOpCode = 'FOUND'
346300141015     c                   if        §7r2fi = 'S'
346400141015     c                   exsr      repfirmatARB
346500141015     c                   endif
346600141015     c                   endif
346700141015     c                   endif
346800141015     c                   endif
346900140919     c                   else
347000150415     c* solo se non impostato da PdA lo stato CONSEGNA
347100150415     c                   if        statuso74<>'CON'
347200150415     c
347300140919     c                   eval      flgbolcO74  =  'N'
347400140919     c                   if        arbndc > 0 and arbddc > 0
347500150415     c                             and arbddc <= datcor and d98cons='S'
347600140919     c                   eval      flgboldo74 = 'S'
347700140919     c                   else
347800140919     c                   eval      flgboldO74  =  'N'
347900140919     c                   endif
348000150415     c                   endif
348100150415
348200140919     c                   endif
348300150415     c
348400150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
348500140919     c                   eval      orincono74 = orsticdo74
348600140919     c                   eval      orficono74 = ORSticaO74
348700140919     c                   else
348800140919     c                   eval      orincono74 = ORSTINIO74
348900140919     c                   eval      orficono74 = ORSTFINO74
349000140919     c                   endif
349100150326     c* note 1
349200150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
349300150309     c                   clear                   wparm
349400150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
349500150326     c                   if        D98DTCTD<>*blanks
349600150326     c                   movel     D98DTCTD      wdat
349700150326     c                   exsr      edtdat
349800150326     c                   else
349900150326     c                   eval      wdate=dtteocoo74
350000150410     c                   eval      usaprev='S'
350100150326     c                   endif
350200150326     c
350300150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
350400150309     c                   eval      stat_n1o74= rtvMsgLang(
350500150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
350600150309     c                   endif
350700150309     c* note 2
350800150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
350900150309     c                   clear                   wparm
351000150403     c* Se lo stato è PRG passo orario limite uscita aut
351100150403     c                   select
351200150403     c                   when      D98SPCDEE='PRG'
351300150326     c                   IF        d98picklim  >*zeros
351400150309
351500150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
351600150326     c                                   (%subst(d98picklim:3:2))
351700150309     c                   else
351800150309     c                   eval      wparm='09:00'
351900150403     c* reperisco orario di fine picking
352000150403     c* filiale / data
352100150403     C                   RESET                   tibs02ds
352200150403     C                   EVAL      t02Cod = 'OLP'
352300150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
352400150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
352500150403     C                   EXSR      getTntbe00f
352600150403     c* filiale
352700150403     c                   IF        t02Err = 'E'
352800150403     C                   RESET                   tibs02ds
352900150403     C                   EVAL      t02Cod = 'OLP'
353000150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
353100150403     C                   EXSR      getTntbe00f
353200150403     c                   endif
353300150403     c* Generico 000
353400150403     c                   IF        t02Err = 'E'
353500150403     C                   RESET                   tibs02ds
353600150403     C                   EVAL      t02Cod = 'OLP'
353700150403     C                   EVAL      t02Ke1 = '000'
353800150403     C                   EXSR      getTntbe00f
353900150403     c                   endif
354000150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
354100150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
354200150403     c                             %subst(t02uni:3:2)
354300150309     c                   endif
354400150403     c                   endif
354500150403     c                   eval      stat_n2o74= rtvMsgLang(
354600150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
354700150403     c                   other
354800150403     c* altri casi
354900150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
355000150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
355100150611     c                             arbtcr=' ' and arbdcr>0  and
355200150611     c                             arbdcr>=wdee
355300150410     c
355400150403     c                   eval      orarich=arbhcr
355500150410     c                   endif
355600150403     c
355700150403     c                   if        orarich>0
355800150403     c                   exsr      sr_dallealle
355900150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
356000150403     c                   eval      stat_n2o74= rtvMsgLang(
356100150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
356200150403     c
356300150403     c                   else
356400150403     c                   clear                   stat_n2o74
356500150403     c                   endif
356600150403     c
356700150403     c                   endsl
356800150403     c
356900150309     c                   endif
357000150410     c
357100150424     c* Se si tratta di fermo deposito pulisco sempre
357200150424     c                   if        usaprev<>'S' or arbffd='S'
357300150410     c                   clear                   DTTEOCOO74
357400150410     c                   clear                   ORINCONO74
357500150410     c                   clear                   ORFICONO74
357600150410     c                   endif
357700150309     c
357800140604     c                   ENDSR
357900150403      /free
358000150403       //--------------------------------------------------------------
358100150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
358200150403       //--------------------------------------------------------------
358300150403       BEGSR  sr_dallealle;
358400150403       clear  w_dallemsg ;
358500150403       clear  w_allemsg ;
358600150403
358700150403       // Reperimento dati tabella VPO - ORARISER
358800150403       if §VPORST_MW=0  ;
358900150403       clear tibs02ds;
359000150403       clear dvpooraris;
359100150403       t02mod='C';
359200150403       t02cod='VPO';
359300150403       t02ke1='ORARISER';
359400150403       tibs02r(kpjba:tibs02ds);
359500150403       if t02err=*blanks;
359600150403          dvpooraris=t02uni;
359700150403       endif;
359800150403       endif;
359900150403
360000150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
360100150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
360200150403       //   determino orari servizio comprensivi della tolleranza
360300150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
360400150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
360500150403
360600150403            if w_dalle<w_dalleoser;
360700150403               w_dalle=w_dalleoser;
360800150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
360900150403            endif;
361000150403
361100150403            if w_alle>w_alleoser;
361200150403               w_alle=w_alleoser;
361300150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
361400150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
361500150403               endif;
361600150403            endif;
361700150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
361800150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
361900150403
362000150403       endsr;
362100150403      /end-free
362200010717     c*--------------------------------------------------------------------------------------------*
362300010717     c* imposta i campi della DS di Output dagli eventi
362400010717     c*--------------------------------------------------------------------------------------------*
362500010717     c     RepAbilFirma  BEGSR
362600010717     c*
362700010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
362800010717     c* e recupera utente / password
362900010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
363000010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
363100010717     c     keyvtd        SETLL     tivtd01l
363200010717     c     keyvtd        READE     tivtd01l                               98
363300010717do  2c                   DOW       NOT *in98
363400010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
363500010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
363600010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
363700010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
363800010717     c                   EVAL      depute = vtdute                              *utente
363900010717     c                   EVAL      deppwd = vtdpwd                              *password
364000010717     c                   LEAVE                                                  *esce dal ciclo
364100010717e   3c                   ENDIF
364200010717     c     keyvtd        READE     tivtd01l                               98
364300010717e   2c                   ENDDO
364400010717e   1C                   ENDIF
364500010717     c*
364600010717     c                   ENDSR
364700000000     c*--------------------------------------------------------------------------------------------*
364800000000     c* edita data da aaaammgg in gg/mm/aaaa
364900000000     c*--------------------------------------------------------------------------------------------*
365000000000     c     edtdat        BEGSR
365100000000     c*
365200000000     c                   MOVEL     *blanks       wdate
365300000000     c*
365400000000     c                   MOVEL     wdat          dsdat
365500000000     c                   MOVEL     dsgio         dsgioe
365600140528     c                   MOVEL     '.'           dsvd1
365700000000     c                   MOVEL     dsmes         dsmese
365800140528     c                   MOVEL     '.'           dsvd2
365900000000     c                   MOVEL     dsann         dsanne
366000000000     c                   MOVEL     dsdate        wdate
366100000000     c*
366200000000     c                   ENDSR
366300000000     c*--------------------------------------------------------------------------------------------*
366400000000     c* edita ora da hhmm in hh:mm
366500000000     c*--------------------------------------------------------------------------------------------*
366600000000     c     edtora        BEGSR
366700000000     c*
366800000000     c                   MOVEL     *blanks       worae
366900000000     c*
367000000000     c                   MOVEL     wora          dsora
367100000000     c                   MOVEL     dshh          dshhe
367200140617     c                   MOVEL     '.'           dsvo1
367300000000     c                   MOVEL     dsmm          dsmme
367400000000     c                   MOVEL     dsorae        worae
367500000000     c*
367600000000     c                   ENDSR
367700000000     c*--------------------------------------------------------------------------------------------*
367800000000     c* decodifica punto operativo
367900000000     c*--------------------------------------------------------------------------------------------*
368000000000     c     decorg        BEGSR
368100000000     c*
368200000000     c                   MOVEL     *blanks       wodes                          *descrizione
368300000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
368400000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
368500000000     c*
368600000000     c                   SETOFF                                           98
368700041111     c                   Z-ADD     1             O
368800041111     c     worg          LOOKUP    sorg(O)                                98
368900000000if  1c                   IF        *in98                                        *trovata
369000121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
369100121001     c                             %EDITC(wOrg : 'X') + ')'
369200041111     c                   MOVEL     sofl1(O)      wofl1
369300041111     c                   MOVEL     sodpd(O)      wodpd
369400000000e   1c                   ENDIF
369500000000     c*
369600000000     c                   ENDSR
369700000000     c*--------------------------------------------------------------------------------------------*
369800000000     c* decodifica evento per STATI
369900000000     c*--------------------------------------------------------------------------------------------*
370000000000     c     deccevsta     BEGSR
370100000000     c*
370200000000     c                   MOVEL     '0'           werr                           *NO errore
370300050704     c*
370400050704     C                   IF        wcev = 'GEN'
370500050704     C                   EVAL      werr = *ON
370600050704     C                   CLEAR                   wcdex
370700050704     C                   LEAVESR
370800050704     C                   ENDIF
370900000000     c*
371000060622     C                   RESET                   tibs02ds
371100060622     C                   EVAL      t02Cod = 'ICE'
371200060622     C                   EVAL      t02Ke1 = wcev
371300060622     C                   EXSR      getTntbe00f
371400060622     C                   IF        t02Err <> 'E' AND
371500140528     C                             (§icesta = 'S'  OR
371600140528     C                               §icestaP = 'S' )
371700060622     c                   MOVEL     §icedei       wcdex
371800000000x   1c                   ELSE                                                   *non trovato
371900000000     c                   MOVEL     '1'           werr                           *SI errore
372000030130     c                   MOVEL     *blanks       wcdex
372100000000e   1c                   ENDIF
372200000000     c*
372300000000     c                   ENDSR
372400000000     c*--------------------------------------------------------------------------------------------*
372500000000     c* decodifica consegna particolare
372600000000     c*--------------------------------------------------------------------------------------------*
372700000000     c     decftc        BEGSR
372800000000     c*
372900000000     c                   MOVEL     *blanks       wdftc
373000000000     c*
373100000000if  1c                   IF        wftc = 'S'
373200060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
373300000000e   1c                   ENDIF
373400000000if  1c                   IF        wftc = 'P'
373500060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
373600000000e   1c                   ENDIF
373700000000if  1c                   IF        wftc = 'A'
373800060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
373900000000e   1c                   ENDIF
374000000000if  1c                   IF        wftc = 'F'
374100060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
374200000000e   1c                   ENDIF
374300000000     c*
374400000000     c                   ENDSR
374500000000     c*--------------------------------------------------------------------------------------------*
374600000000     c* decodifica turno di chiusura
374700000000     c*--------------------------------------------------------------------------------------------*
374800000000     c     decgcx        BEGSR
374900000000     c*
375000000000     c                   MOVEL     *blanks       wdgcx
375100000000     c* giorno
375200000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
375300000000if  1c                   IF        a1 = '1'
375400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
375500000000e   1c                   ENDIF
375600000000if  1c                   IF        a1 = '2'
375700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
375800000000e   1c                   ENDIF
375900000000if  1c                   IF        a1 = '3'
376000060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
376100000000e   1c                   ENDIF
376200000000if  1c                   IF        a1 = '4'
376300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
376400000000e   1c                   ENDIF
376500000000if  1c                   IF        a1 = '5'
376600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
376700000000e   1c                   ENDIF
376800000000if  1c                   IF        a1 = '6'
376900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
377000000000e   1c                   ENDIF
377100000000if  1c                   IF        a1 = '7'
377200060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
377300000000e   1c                   ENDIF
377400000000if  1c                   IF        a1 = ' '
377500000000     c                   EVAL      wdgcx = *blanks
377600000000e   1c                   ENDIF
377700000000     c* am/pm
377800000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
377900000000if  1c                   IF        a1 = ' '
378000060612     c                   EVAL      wdgcx =
378100060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
378200000000e   1c                   ENDIF
378300000000if  1c                   IF        a1 = 'M'
378400060612     c                   EVAL      wdgcx =
378500060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
378600000000e   1c                   ENDIF
378700000000if  1c                   IF        a1 = 'P'
378800060612     c                   EVAL      wdgcx =
378900060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
379000000000e   1c                   ENDIF
379100000000     c*
379200000000     c                   ENDSR
379300000000     c*--------------------------------------------------------------------------------------------*
379400000000     c* decodifica evento per GIACENZE
379500000000     c*--------------------------------------------------------------------------------------------*
379600000000     c     deccevgia     BEGSR
379700000000     c*
379800000000     c                   MOVEL     '0'           werr                           *NO errore
379900000000     c*
380000060622     C                   RESET                   tibs02ds
380100060622     C                   EVAL      t02Cod = 'ICE'
380200060622     C                   EVAL      t02Ke1 = wcev
380300060622     C                   EXSR      getTntbe00f
380400060622     C                   IF        t02Err <> 'E' AND                            *trovata
380500140528     C                             (§icegia = 'S'  OR
380600140528     C                               §icegiap = 'S')
380700060622     c                   MOVEL     §icedei       wcdex
380800000000x   1c                   ELSE                                                   *non trovato
380900000000     c                   MOVEL     '1'           werr                           *SI errore
381000030130     c                   MOVEL     *blanks       wcdex
381100000000e   1c                   ENDIF
381200000000     c*
381300000000     c                   ENDSR
381400000000     c*--------------------------------------------------------------------------------------------*
381500000000     c* Controlla la DS dsi Input
381600000000     c*--------------------------------------------------------------------------------------------*
381700000000     c     chkdsinput    BEGSR
381800000000     c*
381900000000     c                   MOVEL     '0'           werr                           *NO errore
382000000000     c*
382100000000     c* dalla DS di Input scompone il codice spedizione
382200000000     c                   MOVEL     nspedizi74    dsspedizi74
382300000000     c*
382400000000     c* controlla che la lina di partenza sia TUTTA numerica
382500000000     c                   SETOFF                                       99
382600000000     c                   TESTN                   dslnpi74             99
382700000000if  1c                   IF        NOT *in99                                    *non è un numero
382800000000     c                   MOVEL     '1'           werr                           *SI errore
382900140617     c                   eval      widmsg = 'TIS0769'
383000140617     c                   eval      widcampo = '*LNP'
383100140617     c                   clear                   wparm
383200140617     c                   exsr      messaggi
383300000000x   1c                   ELSE                                                   *è un numero
383400000000     c                   MOVE      dslnpi74      a1
383500000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
383600000000     c                   MOVEL     '1'           werr                           *SI errore
383700140617     c                   eval      widmsg = 'TIS0769'
383800140617     c                   eval      widcampo = '*LNP'
383900140617     c                   clear                   wparm
384000140617     c                   exsr      messaggi
384100000000e   2c                   ENDIF
384200000000e   1c                   ENDIF
384300000000     c*
384400000000     c* controlla che la serie spedizione sia TUTTA numerica
384500000000     c                   SETOFF                                       99
384600000000     c                   TESTN                   dsnrsi74             99
384700000000if  1c                   IF        NOT *in99                                    *non è un numero
384800000000     c                   MOVEL     '1'           werr                           *SI errore
384900140617     c                   eval      widmsg = 'TIS0769'
385000140617     c                   eval      widcampo = '*NRS'
385100140617     c                   clear                   wparm
385200140617     c                   exsr      messaggi
385300000000x   1c                   ELSE                                                   *è un numero
385400000000     c                   MOVE      dsnrsi74      a1
385500000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
385600000000     c                   MOVEL     '1'           werr                           *SI errore
385700140617     c                   eval      widmsg = 'TIS0769'
385800140617     c                   eval      widcampo = '*NRS'
385900140617     c                   clear                   wparm
386000140617     c                   exsr      messaggi
386100000000e   2c                   ENDIF
386200000000e   1c                   ENDIF
386300000000     c*
386400000000     c* controlla che il numero spedizione sia TUTTO numerico
386500000000     c                   SETOFF                                       99
386600000000     c                   TESTN                   dsnspi74             99
386700000000if  1c                   IF        NOT *in99                                    *non è un numero
386800000000     c                   MOVEL     '1'           werr                           *SI errore
386900140617     c                   eval      widmsg = 'TIS0769'
387000140617     c                   eval      widcampo = '*NSP'
387100140617     c                   clear                   wparm
387200140617     c                   exsr      messaggi
387300000000x   1c                   ELSE                                                   *è un numero
387400000000     c                   MOVE      dsnspi74      a1
387500000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
387600000000     c                   MOVEL     '1'           werr                           *SI errore
387700140617     c                   eval      widmsg = 'TIS0769'
387800140617     c                   eval      widcampo = '*NSP'
387900140617     c                   clear                   wparm
388000140617     c                   exsr      messaggi
388100000000e   2c                   ENDIF
388200000000e   1c                   ENDIF
388300110104     c*
388400110104     c* controlla che il codice cliente sia TUTTO numerico
388500110104     c                   IF        kscI74 <> *BLANK
388600110104     c                   SETOFF                                       99
388700110104     c                   TESTN                   kscI74               99
388800110104     c                   IF        NOT *in99                                    *non è un numero
388900110104     c                   MOVEL     '1'           werr                           *SI errore
389000140617     c                   eval      widmsg = 'TIS0769'
389100140617     c                   eval      widcampo = 'KSCI74'
389200140617     c                   clear                   wparm
389300140617     c                   exsr      messaggi
389400110104     c                   ELSE                                                   *è un numero
389500110104     c                   MOVE      kscI74        a1
389600110104     c                   IF        a1 < '0'                                     *trovata una lettera
389700110104     c                   MOVEL     '1'           werr                           *SI errore
389800140617     c                   eval      widmsg = 'TIS0769'
389900140617     c                   eval      widcampo = 'KSCI74'
390000140617     c                   clear                   wparm
390100140617     c                   exsr      messaggi
390200110104     c                   ENDIF
390300110104     c                   ENDIF
390400110104     c                   ENDIF
390500110104     c*
390600110104     c* controlla che il codice mittente sia TUTTO numerico
390700110104     c                   IF        ccmI74 <> *BLANK
390800110104     c                   SETOFF                                       99
390900110104     c                   TESTN                   ccmI74               99
391000110104     c                   IF        NOT *in99                                    *non è un numero
391100110104     c                   MOVEL     '1'           werr                           *SI errore
391200140617     c                   eval      widmsg = 'TIS0769'
391300140617     c                   eval      widcampo = 'CCMI74'
391400140617     c                   clear                   wparm
391500140617     c                   exsr      messaggi
391600110104     c                   ELSE                                                   *è un numero
391700110104     c                   MOVE      ccmI74        a1
391800110104     c                   IF        a1 < '0'                                     *trovata una lettera
391900110104     c                   MOVEL     '1'           werr                           *SI errore
392000140617     c                   eval      widmsg = 'TIS0769'
392100140617     c                   eval      widcampo = 'CCMI74'
392200140617     c                   clear                   wparm
392300140617     c                   exsr      messaggi
392400110104     c                   ENDIF
392500110104     c                   ENDIF
392600110104     c                   ENDIF
392700050104     c*
392800050104     c* controlla anno spedizione
392900050104     c                   TESTN                   cAASI74              99
393000050104if  1c                   IF        NOT *in99                                    *non è un numero
393100050104     c                   EVAL      cAASI74 = *ZERO
393200050104x   1c                   ENDIF
393300100222     c*
393400100223     c* Estraggo l'anno dalla data spedizione.
393500100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
393600100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
393700100223     C                   IF        dspI74 <> *BLANK
393800100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
393900100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
394000100223     C                   MONITOR
394100100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
394200100223     C                   ON-ERROR
394300100223     C                   ENDMON
394400100222     C                   ENDIF
394500000000     c*
394600000000     c* se errore, esce dal programma
394700140610if  1c                   IF        werr = '1'                                   *SI errore
394800140610     c                   eval      esito = werr                                 *errore
394900140610e   1c                   ENDIF
395000000000     c*
395100000000     c                   ENDSR
395200000000     c*--------------------------------------------------------------------------------------------*
395300000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
395400000000     c* Input:   Walfa7 = Alfanumerico di 7
395500000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
395600000000     c*--------------------------------------------------------------------------------------------*
395700000000     c     xallinea1     BEGSR
395800000000     c*
395900000000     c* reimposta variabli di lavoro
396000000000     c                   CLEAR                   i
396100000000     c                   CLEAR                   z
396200000000     c                   CLEAR                   walfa7bis
396300000000     c                   EVAL      wzeri = *ALL'0'
396400000000     c*
396500000000     c     ' '           CHECKR    walfa7:7      i
396600000000if  1c                   IF        i <> 0
396700000000     c     i             SUBST     walfa7        walfa7bis
396800000000e   1c                   ENDIF
396900000000if  1c                   IF        walfa7bis <> *blanks
397000000000     c     7             SUB       i             z
397100000000if  2c                   IF        z <> 0
397200000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
397300000000e   2c                   ENDIF
397400000000     c                   EVAL      z = z+ 1
397500000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
397600000000e   1c                   ENDIF
397700000000     c*
397800000000     c                   ENDSR
397900000000     c*--------------------------------------------------------------------------------------------*
398000000000     c* caricamento tabelle occorrenti
398100000000     c*--------------------------------------------------------------------------------------------*
398200000000     c     cartab        BEGSR
398300000000     c*---
398400000000     c* organigrama
398500000000     c*---
398600000000     c                   CLEAR                   i                              *indice
398700000000     C                   CLEAR                   sorg
398800000000     C                   CLEAR                   sodes
398900000000     C                   CLEAR                   sofl1
399000000000     C                   CLEAR                   sopt
399100000000     C                   CLEAR                   sodpd
399200060623     C                   OPEN      tntbe01l
399300000000     c     *loval        SETLL     azorg01l
399400000000     c                   READ      azorg01l                               99
399500000000do  1c                   DOW       NOT *in99
399600000000     c                   ADD       1             i
399700000000     c                   Z-ADD     orgfil        sorg(i)
399800000000     c                   MOVEL     orgdes        sodes(i)
399900000000     c                   MOVEL     orgde3        og143
400000020702     C*
400100020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
400200020702     C                   CLEAR                   dntw
400300020702     C*
400400020702     C                   MOVEL(P)  'NTW'         kbecod
400500020702     C                   MOVEL(P)  §ogntw        kbeke1
400600020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
400700020702     c                   MOVEL     *blanks       kbelin                         *lingua
400800020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
400900020702     C     keytbe        CHAIN     tntbe01l
401000020702     C                   IF        %found(tntbe01l)
401100020702     C                   MOVEL     tbeuni        dntw
401200020702     C                   ENDIF
401300020702     C*
401400020702     C                   MOVEL     §ntwfie       sofl1(i)
401500020702     c                   MOVEL     §ogntw        sopt(i)
401600020702     c                   MOVEL     §ogntw        sodpd(i)
401700000000     c                   READ      azorg01l                               99
401800000000e   1c                   ENDDO
401900060623     C                   CLOSE     tntbe01l
402000000000     c*---
402100000000     c                   ENDSR
402200000000     c*--------------------------------------------------------------------------------------------*
402300000000     c* operazioni iniziali -da fare sempre-
402400000000     c*--------------------------------------------------------------------------------------------*
402500000000     c     rutinz        BEGSR
402600000000     c*
402700000000     c* reperimento data corrente
402800000000     C                   TIME                    n14                            *ora (6)+ data (8)
402900000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
403000000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
403100000000     C                   Z-ADD     n8            g08dat
403200000000     C                   Z-ADD     *zeros        g08inv
403300000000     C                   MOVEL     '0'           g08err
403400000000     c                   CALL      'XSRDA8'
403500000000     c                   PARM                    wlbda8
403600000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
403700000000     C                   MOVEL     datcor        anncor                         *anno corrente
403800000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
403900000000     c*
404000000000     c* calcola data corrente - 1 anno
404100000000     c                   Z-ADD     datcor        datpre                         *data precedente
404200000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
404300000000     c*
404400010716     c* azzera la DS di Output e le variabili di lavoro / controllo
404500000000     c                   MOVEL     '0'           esito
404600140603     c                   reset                   tis778dso                      *ds Output
404700140603     c                   reset                   tis778dsm                      *ds Output
404800000000     c                   CLEAR                   smi                            *di memorizzazione
404900000000     c                   CLEAR                   smseql
405000000000     c                   CLEAR                   smseqe
405100000000     c                   CLEAR                   smdevhev
405200000000     c                   CLEAR                   smdeve
405300000000     c                   CLEAR                   smheve
405400000000     c                   CLEAR                   smfle
405500000000     c                   CLEAR                   smdev
405600000000     c                   CLEAR                   smcev
405700130312     c                   CLEAR                   smspe
405800000000     c                   CLEAR                   smdmc
405900000000     c                   CLEAR                   smdmcR
406000000000     c                   CLEAR                   tote                           *contatore eventi
406100000000     c                   CLEAR                   seql                           *sequenza legami
406200010716     c                   RESET                   $AbTD                          *di lavoro
406300010716     c                   RESET                   $AbFirma
406400150312     c                   clear                   pdaavv
406500150421     c                   clear                   pdagia
406600140603     c* parametri ingresso
406700140603     c                   exsr      Kontrolla
406800000000     c* controlla da DS in Input
406900000000     c                   EXSR      chkdsinput
407000000000     c*
407100000000     c* carica i clienti dettaglio del cliente unificante
407200000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
407300000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
407400000000     c     keyvss        SETLL     tivss02l
407500000000     c     keyvss        READE     tivss02l                               99
407600000000do  1c                   DOW       NOT *in99
407700000000if  2c                   IF        vssvat = *blanks                             *attivo
407800000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
407900000000     c                             datcor <= vssdsc
408000010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
408100010719     c* --> TD per vedere la firma DPD
408200010716     c                   RESET                   $AbTD                          *di lavoro
408300010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
408400010719     c     kssksu        SETLL     tivss02l
408500010719     c     kssksu        READE     tivss02l                               99
408600010719do  4c                   DOW       NOT *in99
408700010716if  5c                   IF        vssvat = *blanks                             *attivo
408800010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
408900010716     c                             datcor <= vssdsc
409000010719if  7c                   IF        vssisv = 'TD'
409100010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
409200010719e   7c                   ENDIF
409300010719e   6c                   ENDIF
409400010716e   5c                   ENDIF
409500010719     c     kssksu        READE     tivss02l                               99
409600010719e   4c                   ENDDO
409700010716     c*
409800000000     c                   LEAVE                                                  *uscita ciclo
409900000000e   3c                   ENDIF
410000000000e   2c                   ENDIF
410100000000     c     keyvss        READE     tivss02l                               99
410200010716e   1c                   ENDDO                                                  *fine cl.abil.TT
410300000000     c*
410400060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
410500060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
410600060621     c*
410700000000     c                   ENDSR
410800040330
410900040330     ***********************************************************************************************
411000040330     **?
411100040330     **?Reperimento ulteriori dati destinatario.
411200040330     **?
411300040330     ***********************************************************************************************
411400040330     C     ImpDSDest     BEGSR
411500040330
411600140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
411700140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
411800100121     C                             ar5Trd <> 'GEN'
411900140716     c                   Z-ADD     f_tasaas      kr5aas
412000140716     c                   Z-ADD     f_taslnp      kr5lnp
412100140716     c                   Z-ADD     f_tasnrs      kr5nrs
412200140716     c                   Z-ADD     f_tasnsp      kr5nsp
412300040330     c                   MOVEL     'GEN'         kr5trd
412400040531     c     keyar5        CHAIN     fiar531c
412500040330     C                   IF        %FOUND
412600040330     C                   EVAL      DAR5GEN = AR5Uni
412700100121     C                   ELSE
412800100121     C                   RESET                   DAR5GEN
412900040330     C                   ENDIF
413000100121     C                   ENDIF
413100100121     C
413200100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
413300100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
413400040330
413500040330     C                   ENDSR
413600040913
413700040913     ***********************************************************************************************
413800040913     **?
413900040913     **?Determino se addebitare l'immagine LDV.
414000040913     **?
414100040913     ***********************************************************************************************
414200040913     C     AddebitoLDV   BEGSR
414300040913
414400040913     C                   IF        LDVO74 = 'S'
414500040913
414600040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
414700040913
414800040913     C                   ENDIF
414900040913
415000040913     C                   ENDSR
415100051110
415200051110     ***********************************************************************************************
415300051110     **?
415400051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
415500051110     **?
415600051110     ***********************************************************************************************
415700051110     C     chkTivpi      BEGSR
415800051110
415900051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
416000051110     C                             esito10)
416100051110     C                   IF        esito10 > 0 AND NOT %ERROR
416200051110     C                   EVAL      gcfdio74 = 'N'
416300051110     C                   ENDIF
416400051110
416500051110     C                   ENDSR
416600051222
416700051222     ***********************************************************************************************
416800051222     **
416900051222     ** Linea arrivo Italia o estera.
417000051222     **
417100051222     ***********************************************************************************************
417200051222     C     setLnaItaEst  BEGSR
417300051222
417400051222     C                   RESET                   wrkLnaItaEst
417500051222     C                   EXSR      decOrg
417600051222     C                   EVAL      wrkLnaItaEst = wofl1
417700051222
417800051222     C                   ENDSR
417900060622
418000060622     ***********************************************************************************************
418100060622     **
418200130312     ** Reperisco informazioni stato bolla in filiale se ultimo evento
418300130312     ** è un MIC
418400060622     **
418500060622     ***********************************************************************************************
418600130312     C     srarb         BEGSR
418700130312     c                   clear                   wcev
418800140714     c                   clear                   wricons
418900130312     c                   clear                   worg
419000130312     c                   clear                   wdat
419100130312     c                   clear                   wora
419200130312     c                   eval      dsevbspe = spe(1)
419300130312     c     karb          chain     fnarb01l
419400130312     c                   if        not %error and
419500130312     c                             %found(fnarb01l)
419600130422     c                   Z-ADD     arblna        worg
419700130312     c                   exsr      setLnaItaEst
419800130312     c* no per estero
419900130312     C                   IF        wrkLnaItaEst = ESTERO
420000130312     c                   leavesr
420100130312     c                   end
420200130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
420300130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
420400130312     C                   SELECT
420500141120     c                   WHEN      arbdcm <> 0
420600130312     c                   MOVEL     '704'         wcev                           *causale evento
420700130313     c                   move      arbdcm        WDAT
420800130313     c                   move      arbhmc        WORA
420900130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
421000130312     c                   WHEN      ARBFDC = ' '
421100130326     c* solo se distinta NON in test
421200130326     c                   clear                   ddstflr
421300130326     c                   z-add     arbifp        dstfgs
421400130326     c                   z-add     arbndc        dstnfv
421500130326     c     kdst          chain     fidst01l
421600130326     c                   if        not %error and
421700130326     c                             %found(fidst01l)
421800130326     c                   eval      ddstflr = dstflr
421900130326     c                   end
422000130327     c                   if        §DSTTSTPDA = 'C' or
422100130327     c                             §DSTTSTPDA = 'E'
422200130326     c                   leavesr
422300130326     c                   end
422400130326     c*
422500130313     c                   move      arbndc        pctndc
422600131105     c                   exsr      cercaceP
422700131105     c                   move      'CET'         ttrd
422800130312     c     kcet          setgt     fipct02l
422900130312     c     kcet          readpe    fipct02l
423000130312     c                   if        not %error and
423100130312     c                             not %eof(fipct02l)
423200130312     c                   movel     pctdati       fipctcetds
423300140926      * firmatario se c'è riempie il campo
423400140926     c                   if        §pctnomfir <> *blank
423500140926     c                   eval      firmato74  = §pctnomfir
423600140926     c                   endif
423700130312     c* consegnata
423800130312     c                   if        §pctcmc = ' '
423900130312     c                   MOVEL     '704'         wcev                           *causale evento
424000130312     c                   else
424100140711     c                   move      'S'           wricons
424200130312     c* mancata consegna
424300130313     c                   movel     §PCTcmc       wcev
424400130312     c                   clear                   ds2a
424500130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
424600130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
424700130312     C                             :rpyEsito)
424800130312     c                   if        §2aflg = '§' and §2acpt = 'S'
424900130312     c                   move      'T  '         wcev
425000130312     c                   endif
425100130312     c                   endif
425200130312     c* ??? £6
425300130312     c                   if        §pctdata > 0
425400130313     c                   move      §PCTDATA      WDAT
425500130312     c                   movel     §PCTORA       WORA
425600130312     c                   endif
425700130312     c                   end
425800130312     c                   endSL
425900130312     c                   end
426000130312     c* ho trovato un evento da inserire  al 1° posto
426100130312     c                   if        wcev <> ' '
426200130312     c                   EXSR      deccevsta
426300130312     c                   IF        werr <> '1'                                  *no errore
426400130312     c                   EXSR      decorg
426500130312     c                   EXSR      edtdat
426600130312     c                   EXSR      edtora
426700130313     c* libero il 1° elemnto di schiera
426800130313     c                   eval      j = 50
426900130313     c                   for       j = 50 downto 1
427000130312     c                   if        cev(j) <> ' '
427100130312     c                   eval      cev(j+1) = cev(j)
427200130312     c                   eval      dev(j+1) = dev(j)
427300130312     c                   eval      hev(j+1) = hev(j)
427400130312     c                   eval      fle(j+1) = fle(j)
427500130312     c                   eval      eve(j+1) = eve(j)
427600130312     c                   end
427700130313     c                   endfor
427800130313     c                   eval      dev(1) = wdate
427900130313     c                   eval      hev(1) = worae
428000130312     c                   eval      cev(1)= wcev
428100130313     c                   eval      fle(1) = wodes
428200130313     c                   eval      eve(1) = wcdex
428300130313     c                   EVAL      cntEveO74 += 1
428400150310     c*
428500150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
428600150310     c                   if        §2aftc='A'
428700150310     c                   eval      pdaavv='S'
428800150310     c                   endif
428900150421     c                   if        §2aftc='G'
429000150421     c                   eval      pdagia='S'
429100150421     c                   endif
429200150310     c
429300130312     c                   end
429400130312     c                   end
429500130312     C                   ENDSR
429600140714     **_________________________________________________________
429700140714     c     cercafiglia   begsr
429800140714     **_________________________________________________________
429900140715     c                   clear                   keyForzata
430000140714      * salva chiave originale per eventi
430100140715     c                   eval      kasaasf = kasaas
430200140715     c                   eval      kaslnpf = kaslnp
430300140715     c                   eval      kasnrsf = kasnrs
430400140715     c                   eval      kasnspf = kasnsp
430500140714     c                   do        *hival
430600140715     c     keytasf       chain     fnlbl02l
430700140714     c                   if        %found(fnlbl02l)
430800140715     c                   eval      kasaasf = LBLaan
430900140715     c                   eval      kaslnpf = LBLlpn
431000140715     c                   eval      kasnrsf = LBLnrn
431100140715     c                   eval      kasnspf = LBLnsn
431200140714     c                   move      'X'           keyForzata
431300140714     c                   iter
431400140714     c                   else
431500140714     c                   leave
431600140714     c                   endif
431700140714     c                   enddo
431800140715     c     keytasf       setll     titas30c                               99    *no errore
431900140715     c     keytasf       READE     titas30c                               99    *no errore
432000141117      *se non trova titas con figlia forza di nuovo la mamma
432100140715if  3c                   IF        *in99                                        *non trovato
432200141117     c                   clear                   titasdssavf
432300140715x   3c                   ELSE                                                   *trovata
432400140715if  4c                   IF        *in01                                        *lettura titas000
432500140715     c                   EVAL      titasdssavf = titasds
432600140715e   4c                   ENDIF
432700140715if  4c                   IF        *in02                                        *lettura titas010
432800140715     c                   EVAL      titasdssavf = titasds
432900140715e   4c                   ENDIF
433000140715if  4c                   IF        *in03                                        *lettura titasp00
433100140715     c                   EVAL      titasdssavf = titasds
433200140715e   4c                   ENDIF
433300140715e   3c                   ENDIF
433400140714     C                   ENDSR
433500131105     **_________________________________________________________
433600131105     c     cercaCEP      begsr
433700131105     **_________________________________________________________
433800131105     c                   move      'CEP'         ttrd
433900131105     c     kcet          setgt     fipct02l
434000131105     c     kcet          readpe    fipct02l
434100131105     c                   if        not %error and
434200131105     c                             not %eof(fipct02l)
434300131105     c                   movel     pctdati       fipctcetds
434400131105     c* consegnata
434500131105     c                   if        §pctcmc = ' '
434600131105     c                   MOVEL     '704'         wcev                           *causale evento
434700131105     c                   else
434800131105     c* mancata consegna
434900131105     c                   movel     §PCTcmc       wcev
435000131105     c                   clear                   ds2a
435100131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
435200131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
435300131105     C                             :rpyEsito)
435400131105     c                   if        §2aflg = '§' and §2acpt = 'S'
435500131105     c                   move      'T  '         wcev
435600131105     c                   endif
435700131105     c                   endif
435800131105     c* ??? £6
435900131105     c                   if        §pctdata > 0
436000131105     c                   move      §PCTDATA      WDAT
436100131105     c                   movel     §PCTORA       WORA
436200131105     c                   endif
436300131105     c                   end
436400131105     c* ho trovato un evento da inserire  al 1° posto
436500131105     c                   if        wcev <> ' '
436600131105     c                   EXSR      deccevsta
436700131105     c                   IF        werr <> '1'                                  *no errore
436800131105     c                   EXSR      decorg
436900131105     c                   EXSR      edtdat
437000131105     c                   EXSR      edtora
437100131105     c* libero il 1° elemnto di schiera
437200131105     c                   eval      j = 50
437300131105     c                   for       j = 50 downto 1
437400131105     c                   if        cev(j) <> ' '
437500131105     c                   eval      cev(j+1) = cev(j)
437600131105     c                   eval      dev(j+1) = dev(j)
437700131105     c                   eval      hev(j+1) = hev(j)
437800131105     c                   eval      fle(j+1) = fle(j)
437900131105     c                   eval      eve(j+1) = eve(j)
438000131105     c                   end
438100131105     c                   endfor
438200131105     c                   eval      dev(1) = wdate
438300131105     c                   eval      hev(1) = worae
438400131105     c                   eval      cev(1)= wcev
438500131105     c                   eval      fle(1) = wodes
438600131105     c                   eval      eve(1) = wcdex
438700131105     c                   EVAL      cntEveO74 += 1
438800131105     c                   end
438900131105     c                   end
439000131105     c                   endsr
439100131105     ***********************************************************************************************
439200130312     **
439300130312     ** Reperisco tabella da TNTBE00F.
439400130312     **
439500130312     ***********************************************************************************************
439600130312     C     getTntbe00f   BEGSR
439700060622     C                   EVAL      t02Lin = langTntbe
439800060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
439900060622     C                   SELECT
440000060622     C                   WHEN      t02Cod = 'ICE'
440100060622     C                   EVAL      dice = t02Uni
440200060622     C                   WHEN      t02Cod = 'CCH'
440300060622     C                   EVAL      dcch = t02Uni
440400060622     C                   WHEN      t02Cod = 'NTW'
440500060622     C                   EVAL      dntw = t02Uni
440600100119     C                   WHEN      t02Cod = 'I1A'
440700100119     C                   EVAL      dI1a = t02Uni
440800150304     C                   WHEN      t02Cod = 'SPC'
440900150304     C                   EVAL      dSPC = t02Uni
441000060622     C                   ENDSL
441100060622     C                   ENDSR
441200060608
441300140529      /free
441400140529       //--------------------------------------------------------------
441500140529       //?Controllo formale dei dati passati.
441600140529       //--------------------------------------------------------------
441700140603       BEGSR  Kontrolla;
441800140529
441900140529       //?OpCode
442000140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
442100140529           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
442200140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
442300140529           exsr RoutEnd;
442400140529         ENDIF;
442500140603           IF  prmRqsDataSize > 0;
442600140603           ELSE;
442700140603             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
442800140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
442900140603             exsr RoutEnd;
443000140603           ENDIF;
443100140529
443200140529       //?Formato e size RQS
443300140603           IF  prmRqsFormato = formatoi74;
443400140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
443500140529           ELSE;
443600140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
443700140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
443800140529             exsr RoutEnd;
443900140529           ENDIF;
444000140529         //?Formato e size RPY
444100140603           IF  prmRpyFormato = formatoo74;
444200140529           ELSE;
444300140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
444400140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
444500140529             exsr RoutEnd;
444600140529           ENDIF;
444700140529           IF  prmRpyDataSize > 0;
444800140529           ELSE;
444900140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
445000140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
445100140529             exsr RoutEnd;
445200140529           ENDIF;
445300140616       //?Formato e size MSG
445400140617         if %parms > 9 ;
445500140617          IF  prmRpyFormMsg = formato;
445600140617          ELSE;
445700140616           prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
445800140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
445900140616           exsr RoutEnd;
446000140617          ENDIF;
446100140617          IF  prmRpyMsgSize > 0;
446200140617          ELSE;
446300140616           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
446400140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
446500140616           exsr RoutEnd;
446600140617          ENDIF;
446700140617         ENDIF;
446800140529
446900140529       ENDSR;
447000140529
447100140616       //--------------------------------------------------------------
447200140617       //?Routine per schierare i messaggi nella ds tis778dsm
447300140616       //--------------------------------------------------------------
447400140616       BEGSR  Messaggi;
447500140616
447600140616       //?Se ho già caricato 99 messaggi esco
447700140616         IF  nrmsg = 99;
447800140616           exsr RoutEnd;
447900140616           clear nrmsg;
448000140616         ENDIF;
448100140616
448200140616         nrmsg += 1;
448300140616         skIdMsg(nrmsg) = wIdMsg;
448400140616         skIdCampo(nrmsg) = wIdCampo;
448500140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
448600140616         IF  wParm <> *blanks;
448700140616           skTextMsg (nrmsg) =
448800140616           rtvMsgLang(wIdMsg:langi74:wParm);
448900140616         ELSE;
449000140616           skTextMsg (nrmsg)=
449100140616           rtvMsgLang(wIdMsg:langi74);
449200140616         ENDIF;
449300140616
449400140616       ENDSR;
449500140616
449600140529       //--------------------------------------------------------------
449700140529       BEGSR  RoutEnd;
449800140530
449900140530
450000140530        if  esito <> '0' ;
450100140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
450200140530             else;
450300140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
450400140530        endif ;
450500140529
450600140530       //?ritorno
450700140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
450800140617         if %parms > 9 ;
450900140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
451000140617         endif;
451100140529         return;
451200140529
451300140529       ENDSR;
451400140529      /end-free
451500000000     c*--------------------------------------------------------------------------------------------*
451600000000     c* operazioni iniziali
451700000000     c*--------------------------------------------------------------------------------------------*
451800000000     c     *inzsr        BEGSR
451900000000     c*
452000000000     c* ricevimento parametri
452100140603     c*m   *ENTRY        PLIST
452200140529     c*m                 PARM                    esito
452300140529     c*m                 PARM                    tis174dsi
452400140529     c*m                 PARM                    tis174dso
452500000000     c*
452600000000     c* chiavi di lettura
452700000000     c     keytas        KLIST                                                  titas30c
452800000000     c                   KFLD                    kasaas                         -anno spedizione
452900000000     c                   KFLD                    kaslnp                         -linea partenza
453000000000     c                   KFLD                    kasnrs                         -serie
453100000000     c                   KFLD                    kasnsp                         -spedizione
453200140715     c     keytasf       KLIST                                                  titas30c
453300140715     c                   KFLD                    kasaasf                        -anno spedizione
453400140715     c                   KFLD                    kaslnpf                        -linea partenza
453500140715     c                   KFLD                    kasnrsf                        -serie
453600140715     c                   KFLD                    kasnspf                        -spedizione
453700130312     c     karb          KLIST                                                  titas30c
453800140716     c                   KFLD                    f_tasaas                         -anno spedizione
453900140716     c                   KFLD                    f_taslnp                         -linea partenza
454000140716     c                   KFLD                    f_tasnrs                         -serie
454100140716     c                   KFLD                    f_tasnsp                         -spedizione
454200130326     C     Kdst          KLIST
454300130326     C                   KFLD                    dstnpg
454400130326     C                   KFLD                    dstnfv
454500130326     C                   KFLD                    dstfgs
454600130326     c                   z-add     4             dstnpg
454700130326     C     Kcet          KLIST
454800130326     C                   KFLD                    arbLNA
454900130326     C                   KFLD                    pctNDC
455000130326     C                   KFLD                    arbpdc
455100131105     C                   KFLD                    TTRD              3
455200130312     C                   KFLD                    arbAAS
455300130312     C                   KFLD                    arbLNP
455400130312     C                   KFLD                    arbNRS
455500130312     C                   KFLD                    arbNSP
455600150403     C     KARP          KLIST
455700150403     C                   KFLD                    arbAAS
455800150403     C                   KFLD                    arbLNP
455900150403     C                   KFLD                    arbNRS
456000150403     C                   KFLD                    arbNSP
456100000000     c     keytaa        KLIST                                                  titaa30c
456200000000     c                   KFLD                    kaaaas                         -anno spedizione
456300000000     c                   KFLD                    kaalnp                         -linea partenza
456400000000     c                   KFLD                    kaanrs                         -serie
456500000000     c                   KFLD                    kaansp                         -spedizione
456600000000     c                   KFLD                    kaatrc                         -tipo anagrafica
456700000000     c     keyta4        KLIST                                                  tita430c
456800000000     c                   KFLD                    ka4aas                         -anno spedizione
456900000000     c                   KFLD                    ka4lnp                         -linea partenza
457000000000     c                   KFLD                    ka4nrs                         -serie
457100000000     c                   KFLD                    ka4nsp                         -spedizione
457200000000     c                   KFLD                    ka4trc                         -tipo riferimento
457300010925     c     ke2ta4        KLIST                                                  tita430c
457400010925     c                   KFLD                    ka4aas                         -anno spedizione
457500010925     c                   KFLD                    ka4lnp                         -linea partenza
457600010925     c                   KFLD                    ka4nrs                         -serie
457700010925     c                   KFLD                    ka4nsp                         -spedizione
457800000000     c     keyevb        KLIST                                                  fnevb01l
457900000000     c                   KFLD                    kvbaas                         -anno spedizione
458000000000     c                   KFLD                    kvblnp                         -linea partenza
458100000000     c                   KFLD                    kvbnrs                         -serie
458200000000     c                   KFLD                    kvbnsp                         -spedizione
458300010717     c     ke2evb        KLIST                                                  fnevb01l
458400010717     c                   KFLD                    kvbaas                         -anno spedizione
458500010717     c                   KFLD                    kvblnp                         -linea partenza
458600010717     c                   KFLD                    kvbnrs                         -serie
458700010717     c                   KFLD                    kvbnsp                         -spedizione
458800010717     c                   KFLD                    kvbdev                         -data evento
458900010717     c                   KFLD                    kvbhev                         -ora  evento
459000000000     c     keylbl        KLIST                                                  fnlbl01l
459100000000     c                   KFLD                    kblaan                         -anno seguente
459200000000     c                   KFLD                    kbllpn                         -linea seguente
459300000000     c                   KFLD                    kblnrn                         -serie seguente
459400000000     c                   KFLD                    kblnsn                         -spedizione seguente
459500000000     c     ke2lbl        KLIST                                                  fnlbl02l
459600000000     c                   KFLD                    kblaap                         -anno precedente
459700000000     c                   KFLD                    kbllpp                         -linea precedente
459800000000     c                   KFLD                    kblnrp                         -serie precedente
459900000000     c                   KFLD                    kblnsp                         -spediz.precedente
460000000000     c     keytbe        KLIST                                                  *tntbe01l
460100000000     c                   KFLD                    kbecod                          -tabella
460200000000     c                   KFLD                    kbeke1                          -chiave uno
460300000000     c                   KFLD                    kbeke2                          -chiave due
460400000000     c                   KFLD                    kbelin                          -lingua
460500000000     c                   KFLD                    kbesif                          -s.informativo
460600000000     c     keyvss        KLIST                                                  *tivss02l
460700000000     c                   KFLD                    kssksu                          -cl.unificante
460800000000     c                   KFLD                    kssisv                          -tipo servizio
460900010717     c     keyvtd        KLIST                                                  *tivtd01l
461000010717     c                   KFLD                    ktdksu                          -cl.unificante
461100000000     c     keycsb        KLIST                                                  *tncsb03l
461200000000     c                   KFLD                    ksbaas                         -anno spedizione
461300000000     c                   KFLD                    ksblnp                         -linea partenza
461400000000     c                   KFLD                    ksbnrs                         -serie
461500000000     c                   KFLD                    ksbnsp                         -spedizione
461600040906     c     keydct        KLIST
461700040906     c                   KFLD                    DCTAAC
461800040906     c                   KFLD                    DCTFIL
461900040906     c                   KFLD                    DCTNCA
462000000000     c     keygcp        KLIST                                                  *figcp01l
462100000000     c                   KFLD                    kcpaas                         -anno spedizione
462200000000     c                   KFLD                    kcplnp                         -linea partenza
462300000000     c                   KFLD                    kcpnrs                         -serie
462400000000     c                   KFLD                    kcpnsp                         -spedizione
462500000000     c                   KFLD                    kcpfrg                         -prg aperture
462600050404     c     K04GCP51      KLIST
462700050404     c                   KFLD                    kcpaas                         -anno spedizione
462800050404     c                   KFLD                    kcplnp                         -linea partenza
462900050404     c                   KFLD                    kcpnrs                         -serie
463000050404     c                   KFLD                    kcpnsp                         -spedizione
463100000000     c     keygnp        KLIST                                                  *fignp01l
463200000000     c                   KFLD                    knpagc                         -anno apertura
463300000000     c                   KFLD                    knpfgc                         -filiale apertura
463400000000     c                   KFLD                    knpngc                         -numero giacenza
463500000000     c                   KFLD                    knpfrg                         -prg apertura
463600040531     c     keyar5        KLIST                                                  *fiar531c
463700030206     c                   KFLD                    kr5aas                         -anno spedizione
463800030206     c                   KFLD                    kr5lnp                         -linea partenza
463900030206     c                   KFLD                    kr5nrs                         -serie
464000030206     c                   KFLD                    kr5nsp                         -spedizione
464100030206     c                   KFLD                    kr5trd                         -tipo record
464200040907
464300000000     c* apre i files
464400000000     c                   OPEN      titas30c
464500000000     c                   OPEN      titaa30c
464600000000     c                   OPEN      tita430c
464700000000     c                   OPEN      fnevb01l
464800000000     c                   OPEN      tivss02l
464900010716     c                   OPEN      tivtd01l
465000000000     c                   OPEN      azorg01l
465100000000     c                   OPEN      fnlbl01l
465200000000     c                   OPEN      fnlbl02l
465300050309     c                   OPEN      tigcp51l
465400050309     c                   OPEN      tignp01l
465500000000     c                   OPEN      tncsb03l
465600040906     c                   OPEN      fndct01l
465700040531     c                   OPEN      fiar531c
465800130312     c                   OPEN      fnarb01l
465900150403     c                   OPEN      fiarp01l
466000140606     c                   OPEN      fiar501l
466100140606     c                   OPEN      fiar401l
466200130326     c                   OPEN      fidst01l
466300130312     c                   OPEN      fipct02l
466400060621     C                   CALLP     openTabel00f()
466500000000     c*
466600000000     c* caricamento tabelle occorrenti
466700000000     c                   EXSR      cartab
466800060621     C                   CALLP     inzLingue()
466900000000     c*
467000000000     c                   ENDSR
467100070109
467200070109     ***********************************************************************************************
467300070109     **
467400070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
467500070109     **
467600070109     ***********************************************************************************************
467700070109     C     eventoAnnulla BEGSR
467800070109
467900070109     C                   IF        §iceEvAn <> *BLANK
468000070109     C                   FOR       j = 1 TO %ELEM(smCev)
468100070109     C                   IF        §iceEvAn = smCev(j) AND
468200070109     C                             evbDevHev = smDevHev(j)
468300070109     C                   CLEAR                   smCev(j)
468400130312     C                   CLEAR                   smspe(j)
468500070109     C                   CLEAR                   smDevHev(j)
468600070109     C                   CLEAR                   smi(j)
468700070109     C                   CLEAR                   smSeql(j)
468800070109     C                   CLEAR                   smSeqe(j)
468900070109     C                   CLEAR                   smDeve(j)
469000070109     C                   CLEAR                   smHeve(j)
469100070109     C                   CLEAR                   smFle(j)
469200070109     C                   CLEAR                   smDev(j)
469300070109     C                   LEAVE
469400070109     C                   ENDIF
469500070109     C                   ENDFOR
469600070109     C                   ENDIF
469700070109
469800070109     C                   ENDSR
469900080131
470000100119     P*--------------------------------------------------
470100100119     P* Procedure name: GetDescrizioneIncassoContrassegno
470200100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
470300100119     P*                          o del contrassegno.
470400100121     P* Returns:        Descrizione modalità incasso contrassegno
470500100119     P*--------------------------------------------------
470600100119     P GetDescrizioneIncassoContrassegno...
470700100119     P                 B
470800100119     D GetDescrizioneIncassoContrassegno...
470900100119     D                 PI            35A
471000100119
471100100121     C                   RESET                   tibs02ds
471200100121     C                   EVAL      t02Cod = 'I1A'
471300100121     C                   EVAL      t02Lin = langTntbe
471400100119     ** Il contrassegno è stato incassato
471500100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
471600100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
471700100121     C                   ELSE
471800100119     ** Il contrassegno non è stato incassato.
471900100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
472000100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
472100100119     C                             ar5Trd <> 'GEN'
472200100121     C                   EVAL      kr5Aas = csbAas
472300100121     C                   EVAL      kr5Lnp = csbLnp
472400100121     C                   EVAL      kr5Nrs = csbNrs
472500100121     C                   EVAL      kr5Nsp = csbNsp
472600100119     C                   EVAL      kr5Trd = 'GEN'
472700100119     C     keyAr5        CHAIN     fiar531c
472800100119     C                   IF        %FOUND()
472900100119     C                   EVAL      dAr5Gen = ar5Uni
473000100119     C                   ELSE
473100100119     C                   RESET                   dAr5Gen
473200100119     C                   RETURN                  *BLANK
473300100119     C                   ENDIF
473400100119     C                   ENDIF
473500100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
473600100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
473700100119     C                   ELSE
473800100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
473900100119     C                   ENDIF
474000100121     C                   ENDIF
474100100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
474200100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
474300100119     C                   IF        t02Err = 'E'
474400100119     C                   RETURN                  *BLANK
474500100119     C                   ENDIF
474600100120     C                   EVAL      dI1a = t02Uni
474700140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
474800140417     C                   RETURN                  dI1a.descrizi2
474900140417     C                   ENDIF
475000100119     C                   RETURN                  dI1a.descrizion
475100100119
475200100119     P GetDescrizioneIncassoContrassegno...
475300100119     P                 E
475400100119
475500110304
475600110304     P*--------------------------------------------------
475700110304     P* Procedure name: IsContrassegnoCompensato
475800110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
475900110304     P*                          to con una fattura.
476000110304     P* Returns:        *ON = Compensato
476100110304     P*--------------------------------------------------
476200110304     P IsContrassegnoCompensato...
476300110304     P                 B
476400110304     D IsContrassegnoCompensato...
476500110304     D                 PI              N
476600110304
476700110304     D retField        S               N   INZ(*OFF)
476800110304
476900110307     ** Il contrassegno non è stato rimborsato.
477000110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
477100110307     C                   RETURN    retField
477200110307     C                   ENDIF
477300110307
477400110307     C/EXEC SQL
477500110307     C+ SELECT '1'
477600110307     C+ INTO :retField
477700110307     C+ FROM tncsv00f
477800110307     C+ WHERE csvAas = :csbAas
477900110307     C+   AND csvLnp = :csbLnp
478000110307     C+   AND csvNrs = :csbNrs
478100110307     C+   AND csvNsp = :csbNsp
478200110307     C+   AND csvCav = 'COMP'
478300110307     C+ FETCH FIRST ROW ONLY
478400110307     C/END-EXEC
478500110304
478600110307     C                   SELECT
478700110307     C                   WHEN      sqlCode < 0
478800110307     C                   DUMP(A)
478900110307     C                   ENDSL
479000110304
479100110307     C                   RETURN    retField
479200110304
479300110304     P IsContrassegnoCompensato...
479400110304     P                 E
479500110304
479600120321
479700120321     P*--------------------------------------------------
479800120321     P* Procedure name: SetIdAssegnoMittente
479900120321     P* Purpose:        Imposta l'ID assegno mittente.
480000120321     P* Returns:        Esito.
480100120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
480200120321     P*                          segni mittente.
480300120321     P* Parameter:      priAbi => ABI assegno.
480400120321     P* Parameter:      priCab => CAB assegno.
480500120321     P*--------------------------------------------------
480600120321     P SetIdAssegnoMittente...
480700120321     P                 B
480800120321     D SetIdAssegnoMittente...
480900120321     D                 PI            10I 0
481000120321     D  priNumeroAssegno...
481100120321     D                                     LIKE(caNumO74)
481200120321     D  priAbi                             LIKE(caAbiO74)
481300120321     D  priCab                             LIKE(caCabO74)
481400120321
481500120321     D retField        S             10I 0 STATIC
481600120321
481700120321      /FREE
481800120321
481900120321       CLEAR retField;
482000120321
482100120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
482200120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
482300120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
482400120321       // In questo caso restituisco l'ID del primo assegno.
482500120321
482600120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
482700120321         RETURN retField;
482800120321       ENDIF;
482900120321
483000120321       EXEC SQL
483100120321         SELECT csmNra, csmAbi, csmCab
483200120321           INTO :priNumeroAssegno, :priAbi, :priCab
483300120321           FROM tncsm00f
483400120321           WHERE csmKey = :priNumeroAssegno
483500120321           FETCH FIRST 1 ROW ONLY
483600120321       ;
483700120321
483800120321       SELECT;
483900120321         WHEN sqlCode = 100;
484000120321           retField = sqlCode;
484100120321         WHEN sqlCode < *ZERO;
484200120321           DUMP(A);
484300120321           retField = sqlCode;
484400120321       ENDSL;
484500120321
484600120321       RETURN retField;
484700120321
484800120321      /END-FREE
484900120321     P SetIdAssegnoMittente...
485000120321     P                 E
485100120321
485200130513
485300130513     P*--------------------------------------------------
485400130513     P* Procedure name: GetFilUrl
485500130513     P* Purpose:        Restituisce l'URL della filiale.
485600130513     P* Returns:        URL filiale.
485700130513     P* Parameter:      piFiliale => ID filiale
485800130513     P*--------------------------------------------------
485900130513     P GetFilUrl       B
486000130513     D GetFilUrl       PI           256A
486100130513     D  piFiliale                     3P 0 CONST
486200130513
486300130513     C                   IF        piFiliale = *ZERO
486400130513     C                   RETURN    *BLANK
486500130513     C                   ENDIF
486600130513     C                   RESET                   tibs02ds
486700130513     C                   EVAL      t02Cod = 'UFI'
486800130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
486900130513     C                   EVAL      t02Lin = langTntbe
487000130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
487100130513     C                   IF        t02Err = 'E'
487200130513     C                   RETURN    *BLANK
487300130513     C                   ENDIF
487400130513     C                   RETURN    t02Uni
487500130513
487600130513     P GetFilUrl       E
487700130513
