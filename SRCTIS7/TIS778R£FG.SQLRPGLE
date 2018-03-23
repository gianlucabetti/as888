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
015800051110
015900000000     d*--------------------------------------------------------------------------------------------*
016000000000     d* Data structure
016100000000     d*--------------------------------------------------------------------------------------------*
016200150403     d dvpooraris    e ds                  inz
016300150403     d fipctcetds    e ds
016400140605     d fipctcords    e ds
016500090914     D cndizion      E DS                  QUALIFIED BASED(nullPtr)
016600090914     D azlin00f      E DS                  BASED(nullPtr)
016700060622     D tabel00f      E DS                  INZ
016800060622     D kpjba         E DS                  INZ
016900140605     D trulorsds     E DS                  INZ
017000140605     D trulor2ds     E DS                  INZ
017100140605     D trulor3ds     E DS                  INZ
017200140606     d dar5emd       e ds                  inz prefix(§)
017300140605     D tnsd99ds      E DS                  INZ
017400140606     D tibs02ds      E DS                  INZ
017500060622     D  t02Mod       E                     INZ('C')                             Controllo
017600090828     D tis7700i0     E DS                  QUALIFIED
017700090828     D                                     INZ(*EXTDFT)
017800090828     D tis7700o0     E DS                  QUALIFIED
017900090828     d*---
018000000000     d* Ridenominazione formato record -TITAS-
018100000000     d*---
018200000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
018300000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
018400000000     d                                     PREFIX(s_)
018500140715     d titasdssavF   E DS                  EXTNAME(titas00f)                    *depisito titas
018600140715     d                                     PREFIX(f_)
018700080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
018800080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
018900080131     D                                     BASED(titas000FigliaPtr)
019000080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
019100080131     D                                     BASED(titas000FigliaPtr)
019200080131     d*---
019300000000     d* Variabili riferite al data base
019400000000     d*---
019500141126     d kasaasm         S                   LIKE(tasaas)                         *lettura titas30c
019600141126     d kaslnpm         S                   LIKE(taslnp)
019700141126     d kasnrsm         S                   LIKE(tasnrs)
019800141126     d kasnspm         S                   LIKE(tasnsp)
019900140715     d kasaasf         S                   LIKE(tasaas)                         *lettura titas30c
020000140715     d kaslnpf         S                   LIKE(taslnp)
020100140715     d kasnrsf         S                   LIKE(tasnrs)
020200140715     d kasnspf         S                   LIKE(tasnsp)
020300000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
020400000000     d kaslnp          S                   LIKE(taslnp)
020500000000     d kasnrs          S                   LIKE(tasnrs)
020600000000     d kasnsp          S                   LIKE(tasnsp)
020700000000     d kastbl          S                   LIKE(tastbl)
020800000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
020900000000     d kaalnp          S                   LIKE(taalnp)
021000000000     d kaanrs          S                   LIKE(taanrs)
021100000000     d kaansp          S                   LIKE(taansp)
021200000000     d kaatrc          S                   LIKE(taatrc)
021300000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
021400000000     d ka4lnp          S                   LIKE(ta4lnp)
021500000000     d ka4nrs          S                   LIKE(ta4nrs)
021600000000     d ka4nsp          S                   LIKE(ta4nsp)
021700000000     d ka4trc          S                   LIKE(ta4trc)
021800000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
021900000000     d kvblnp          S                   LIKE(evblnp)
022000000000     d kvbnrs          S                   LIKE(evbnrs)
022100000000     d kvbnsp          S                   LIKE(evbnsp)
022200000000     d kvbdev          S                   LIKE(evbdev)
022300000000     d kvbhev          S                   LIKE(evbhev)
022400000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
022500000000     d kcplnp          S                   LIKE(gcplnp)
022600000000     d kcpnrs          S                   LIKE(gcpnrs)
022700000000     d kcpnsp          S                   LIKE(gcpnsp)
022800000000     d kcpfrg          S                   LIKE(gcpfrg)
022900000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
023000000000     d knpfgc          S                   LIKE(gnpfgc)
023100000000     d knpngc          S                   LIKE(gnpngc)
023200000000     d knpfrg          S                   LIKE(gnpfrg)
023300000000     d knpfas          S                   LIKE(gnpfas)
023400000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
023500000000     d ksblnp          S                   LIKE(csblnp)
023600000000     d ksbnrs          S                   LIKE(csbnrs)
023700000000     d ksbnsp          S                   LIKE(csbnsp)
023800000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
023900000000     d kbllpn          S                   LIKE(lbllpn)
024000000000     d kblnrn          S                   LIKE(lblnrn)
024100000000     d kblnsn          S                   LIKE(lblnsn)
024200000000     d kblaap          S                   LIKE(lblaap)
024300000000     d kbllpp          S                   LIKE(lbllpp)
024400000000     d kblnrp          S                   LIKE(lblnrp)
024500000000     d kblnsp          S                   LIKE(lblnsp)
024600000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
024700000000     d kctlnp          S                   LIKE(dctlnp)
024800000000     d kctnrs          S                   LIKE(dctnrs)
024900000000     d kctnsp          S                   LIKE(dctnsp)
025000040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
025100030206     d kr5lnp          S                   LIKE(ar5lnp)
025200030206     d kr5nrs          S                   LIKE(ar5nrs)
025300030206     d kr5nsp          S                   LIKE(ar5nsp)
025400030206     d kr5trd          S                   LIKE(ar5trd)
025500000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
025600000000     d kssisv          S                   LIKE(vssisv)
025700010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
025800000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
025900000000     d kbeke1          S                   LIKE(tbeke1)
026000000000     d kbeke2          S                   LIKE(tbeke2)
026100000000     d kbelin          S                   LIKE(tbelin)
026200000000     d kbesif          S                   LIKE(tbesif)
026300000000     d waas            S                   LIKE(tasaas)                         *di lavoro
026400000000     d wlnp            S                   LIKE(taslnp)
026500000000     d wnrs            S                   LIKE(tasnrs)
026600000000     d wnsp            S                   LIKE(tasnsp)
026700000000     d wfca            S                   LIKE(tasfca)
026800000000     d wfgc            S                   LIKE(tasfgc)
026900000000     d wfpr            S                   LIKE(dctfpr)
027000000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
027100000000     d deplpn          S                   LIKE(lbllpn)
027200000000     d depnrn          S                   LIKE(lblnrn)
027300000000     d depnsn          S                   LIKE(lblnsn)
027400010716     d depute          S                   LIKE(vtdute)
027500010716     d deppwd          S                   LIKE(vtdpwd)
027600030130     d savtsp          S                   LIKE(tastsp)                         *di salvataggio
027700000000     d lcca            S                   LIKE(tascca)                         *di lavoro
027800000000     d lduc            S                   LIKE(tasduc)
027900000000     d llnp            S                   LIKE(taslnp)
028000000000     d ldti            S                   LIKE(tasdti)
028100000000     d lhti            S                   LIKE(tashti)
028200000000     d ldrt            S                   LIKE(tasdrt)
028300000000     d llna            S                   LIKE(taslna)
028400000000     d ldcm            S                   LIKE(tasdcm)
028500000000     d lhmc            S                   LIKE(tashmc)
028600060621     d langTabel       S                   LIKE(tblkut)
028700060621     d langTntbe       S                   LIKE(tbelin)
028800140108     d wDev            S                   LIKE(evbdev)
028900140108     d wHev            S                   LIKE(evbhev)
029000150611     d wDEE            S                   LIKE(d98dee)
029100140616     d wParm           s            512a   inz
029200150403     D w_dallemsg      S              5a
029300150403     D w_allemsg       S              5a
029400150403     D w_dalle         S               t
029500150403     D w_alle          S               t
029600150403     D w_dalleoser     S               t
029700150403     D w_alleoser      S               t
029800000000     d*---
029900000000     d* Ds
030000000000     d*---
030100000000      * controllo data
030200000000     d wlbda8          DS                  INZ                                  *controlla data
030300000000     d  g08dat                        8  0                                       -data dritta
030400000000     d  g08inv                        8  0                                       -data invertita
030500000000     d  g08err                        1                                          -errore
030600000000     d  g08tgi                        5  0                                       -giorni fra date
030700000000      * scomposizione data generica
030800000000     d dsdat           DS                  INZ                                  *data
030900000000     d  dsann                         4  0                                       -anno
031000000000     d  dsmes                         2  0                                       -mese
031100000000     d  dsgio                         2  0                                       -giorno
031200000000      * scomposizione data generica (anno + mese/giorno)
031300000000     d                 DS                  INZ                                  *data aa + mm/gg
031400000000     d  dsannB                 1      4  0                                       -anno
031500000000     d  dsmgsB                 5      8  0                                       -mese/giorno
031600000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
031700000000      * composizione data generica editata
031800000000     d dsdate          DS                  INZ                                  *data
031900000000     d  dsgioe                        2  0                                       -giorno
032000000000     d  dsvd1                         1                                          -'/'
032100000000     d  dsmese                        2  0                                       -mese
032200000000     d  dsvd2                         1                                          -'/'
032300000000     d  dsanne                        4  0                                       -anno
032400000000      * scomposizione ora generica
032500000000     d dsora           DS                  INZ                                  *ora
032600000000     d  dshh                          2  0                                       -ore
032700000000     d  dsmm                          2  0                                       -minuti
032800000000      * composizione ora generica editata
032900000000     d dsorae          DS                  INZ                                  *ora
033000000000     d  dshhe                         2  0                                       -ore
033100000000     d  dsvo1                         1                                          -':'
033200000000     d  dsmme                         2  0                                       -minuti
033300030206      * lettura FIAR5 - record bancali
033400030206     d dar5ban       E DS
033500030206      * lettura FIAR5 - record supermercati
033600030206     d dar5scr       E DS
033700040330     d DAR5GEN       E DS
033800040330     D                                     INZ
033900040330     D                                     PREFIX(GEN)
034000000000      * tipi servizio
034100000000     d ds5e          E DS
034200000000      * disposizioni giacenza
034300000000     d ds2d          E DS
034400000000      * causale eventi
034500000000     d dice          E DS
034600000000      * causali chiusura CA
034700000000     d dcch          E DS
034800100119     d dI1a          E DS                  QUALIFIED
034900000000      * consegne anomale
035000000000     d ds7o          E DS
035100130312      * eventi
035200130312     d ds2a          E DS
035300000000      * riferimenti
035400000000     d dta4a         E DS
035500070927     d dsbl4e        E DS
035600060627      * Estensione blp/arb tipo record "I"
035700060627     d dsbl4i        E DS
035800040913      * Addebito immagine POD
035900040913     d DLVA1         E DS
036000040913     D                                     INZ
036100000000      * lettura campo orgde3 di AZORG00F
036200000000     d og143         E DS
036300000000      * stati contrassegno
036400000000     d ds4s          E DS
036500141014      * particolarità consegna
036600141014     d ds7r          E DS
036700031217      * DS lettura campo TASFLO di TITAS
036800031217     d dtasflo       E DS
036900000000      * Ds ricerca cliente P.d.C.
037000000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
037100000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
037200000000     d indds         E DS                  EXTNAME(cnind00f) INZ
037300000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
037400000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
037500150304      * stato data prev cons
037600150304     d dspc          E DS
037700150312     d
037800150312     d xgiolavds     e ds
037900000000      * reperimento clienti da cliente unificante
038000090903     d*tibs10ds      E DS
038100090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
038200000000      * scomposizione codice spedizione -DS Input-
038300000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
038400000000     d  dslnpi74                      3                                          -linea partenza
038500000000     d  dsnrsi74                      2                                          -serie
038600000000     d  dsnspi74                      7                                          -spedizione
038700000000      * composizione codice spedizione -bolle-
038800000000     d                 DS
038900000000     d  dstaslnp               1      3  0                                       -linea partenza
039000000000     d  dstasnrs               4      5  0                                       -serie
039100000000     d  dstasnsp               6     12  0                                       -spedizione
039200000000     d dstasspe                1     12                                         *ds cod spedizione
039300000000      * composizione data + ora evento
039400080131     d                 DS                  INZ
039500080131     d  evbdev                 1      8  0                                       -data evento
039600080131     d  evbhev                 9     12  0                                       -ora  evento
039700080131     d evbDevHev               1     12  0                                      *ds data+ora evento
039800130312      * composizione spedizione evento
039900130312     d                 DS                  INZ
040000130312     d  evbaas                 1      4  0                                       -data evento
040100130312     d  evblnp                 5      7  0                                       -ora  evento
040200130312     d  evbnrs                 8      9  0                                       -ora  evento
040300130312     d  evbnsp                10     16  0                                       -ora  evento
040400130312     d dsevbspe                1     16  0                                      *ds data+ora evento
040500060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
040600000000      * ordinamento schiere eventi
040700000000     d                 DS
040800000000     d  dsseql                 1      3  0                                       -sequenza legame
040900000000     d  dsseqe                 4      6  0                                       -sequenza evento
041000000000     d  dsdevhev               7     18  0                                       -data + ora
041100000000     d  dsi                   19     21  0                                       -indice
041200000000     d dssort                  1     21  0                                      *ds cod spedizione
041300000000     d* Input routine XALLINEA
041400000000     d walfa7          S              7                                         *alfanumerico 7
041500000000     d walfa7bis       S              7                                         *alfanumerico 7
041600000000     d wzeri           S              7                                         *alfa con zeri
041700000000     d*---
041800000000     d* Variabili
041900000000     d*---
042000101104     d i               S             10I 0                                      *indici
042100101104     d ii              S             10I 0
042200101104     d j               S             10I 0
042300101104     d jj              S             10I 0
042400101104     d z               S             10I 0
042500101104     d s               S             10I 0
042600101104     d k               S             10I 0
042700101104     d o               S             10I 0
042800101104     d tote            S             10I 0                                      *totale eventi memor
042900000000     d seql            S              5  0                                      *sequenza legami
043000000000     d n14             S             14  0                                      *Numerico 14
043100000000     d n8              S              8  0                                      *Numerico 8
043200000000     d n7              S              7  0                                      *Numerico 7
043300000000     d n4              S              4  0                                      *Numerico 4
043400010927     d n3              S              3  0                                      *Numerico 3
043500000000     d a1              S              1                                         *Alfanumerico 1
043600000000     d a3              S              3                                         *Alfanumerico 3
043700000000     d a5              S              5                                         *Alfanumerico 5
043800010717     d a8              S              8                                         *Alfanumerico 8
043900000000     d a15             S             15                                         *Alfanumerico 15
044000000000     d a200            S            200                                         *Alfanumerico 200
044100000000     d a200bis         S            200                                         *Alfanumerico 200
044200010927     d a230            S            230                                         *Alfanumerico 230
044300000000     d a400            S            400                                         *Alfanumerico 400
044400000000     d datcor          S              8  0                                      *data corrente
044500000000     d datpre          S              8  0                                      *data precedente
044600000000     d oracor          S              6  0                                      *ora corrente
044700000000     d anncor          S              4  0                                      *anno corrente
044800000000     d annpre          S              4  0                                      *anno precedente
044900140617     d wIdMsg          s              7a
045000140617     d wIdCampo        s             10a
045100000000     d werr            S              1                                         *errore generico
045200000000     d wdat            S              8  0                                      *data (aaaammgg)
045300000000     d wdate           S             10                                         *data (gg/mm/aaaa)
045400000000     d wora            S              4  0                                      *ora  (hhmm)
045500000000     d worae           S              5                                         *ora  (hh:mm)
045600000000     d worg            S              3  0                                      *filiale
045700000000     d wodes           S             30                                         *descrizione filiale
045800000000     d wofl1           S              1                                         *flag italia/estero
045900020702     d wodpd           S              3                                         *flag filiale DPD
046000000000     d wcev            S              3                                         *causale evento
046100030130     d wcdex           S             30                                         *descrizione bar/pt
046200000000     d wpgm            S             20                                         *programma
046300000000     d wftc            S              1                                         *cons.partic.
046400140714     d wricons         S              1                                         *wrk per riconsegna
046500140715     d keyforzata      S              1                                         *wrk per riconsegna
046600000000     d wdftc           S             50                                         *descriz.cons.partic
046700000000     d d1wdftc         S             50                                         *descriz.cons.part.1
046800000000     d d2wdftc         S             50                                         *descriz.cons.part.2
046900000000     d wgcx            S              2                                         *turno chiusura
047000000000     d wdgcx           S             20                                         *descriz.turno chius
047100000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
047200000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
047300000000     d we              S              3                                         *descrizione 'e'
047400000000     d bollaorig       S              1                                         *bolla originaria
047500150310     d PDAavv          S              1                                         *evento AVV a PDA
047600150421     d PDAgia          S              1                                         *evento GIA a PDA
047700000000     d sostrit         S              1                                         sostituisce Ritiro
047800000000     d sostcon         S              1                                         sostituisce Consegna
047900000000     d newevb          S              1                                         *nuovo evento
048000000000     d lavdevhev       S             12  0                                      *data + ora
048100040913     D WrkCurDate      S               D                                        Data corrente
048200060620     D wrkDtInvDisp    S              8
048300060620     D wrkLnaItaEst    S              1
048400060620     D rpyOpCode       S             10A
048500060620     D rpyEsito        S             10I 0
048600060621     D nullPtr         S               *
048700080131     D titas000FigliaPtr...
048800080131     D                 S               *   INZ(%ADDR(titas000Figlia))
048900010716     d*---
049000010716     d* Variabili di controlli
049100010716     d*---
049200010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
049300010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
049400000000     d*---
049500000000     d* Variabili di memorizzazione
049600000000     d*---
049700000000     d mhcre           S              5                                         *consegna richiesta
049800000000     d mdcre           S             10
049900000000     d mtcre           S             50
050000000000     d mftce           S            100                                         *consegna partic.
050100000000     d mgcxe           S            100                                         *turno chiusura
050200000000     d mffd            S            100                                         *fermo deposito
050300010927     d mmncB           S             35                                         *NON consegna LAV
050400010927     d mmncC           S             35
050500010927     d mmncG           S             35                                         *NON consegna GIAC
050600010927     d mmncH           S             35
050700010927     d mmncN           S             35                                         *NON consegna NOFATT
050800010927     d mmncO           S             35
050900050701     d mmnc8           S             35
051000050701     d mmnc9           S             35
051100030206     d mtel            S             20                                         *N° tel destinatario
051200030206     d mban            S             35                                         *bancali
051300030207     d mscr            S            500                                         *supermercati
051400030210     d msdace          S             10
051500030210     d msorce          S              5
051600030210     d msdcre          S             10
051700030210     d mshcre          S              5
051800030210     d mstcre          S             30
051900030210     d msnom           S             50
052000030210     d mscrT           S            500    DIM(5)
052100030211     d wmscrT          S           2504
052200000000     d*---
052300000000     d* Schiere
052400000000     d*---
052500000000      * taabella clienti del cliente unificante
052600000000      * tabella tipi servizio
052700000000     d stsp            S              1    DIM(20)                              *tipi servizio
052800000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
052900000000      * tabella consegne anomale
053000030130     d scca            S              1    DIM(30)                              *consegna anomala
053100000000      * tabella disposizioni giacenza
053200000000     d sdis            S              1    DIM(10)                              *codice
053300000000     d sddis           S             20    DIM(10)                              *descrizione italian
053400000000      * tabella filiali
053500000000     d sorg            S              3  0 DIM(300)                             *filiale
053600000000     d sodes           S             30    DIM(300)                             *descrizione filiale
053700000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
053800020702     d sopt            S              3    DIM(300)                             *flag poste/no poste
053900020702     d sodpd           S              3    DIM(300)                             *flag DPD/no DPD
054000000000      * eventi memorizzati
054100000000     d smi             S              3  0 DIM(200)                             *indice
054200000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
054300000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
054400000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
054500000000     d smdeve          S             10    DIM(200)                             *data evento editata
054600000000     d smheve          S              5    DIM(200)                             *ora  evento editata
054700000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
054800000000     d smdev           S             30    DIM(200)                             *descrizione evento
054900000000     d smcev           S              3    DIM(200)                             *causale evento
055000130313     d smspe           S             16  0 DIM(200)                             *causale evento
055100000000      * eventi memorizzati x ordinamento
055200000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
055300000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
055400000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
055500000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
055600000000     d spdeve          S             10    DIM(200)                             *data evento editata
055700000000     d spheve          S              5    DIM(200)                             *ora  evento editata
055800000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
055900000000     d spdev           S             30    DIM(200)                             *descrizione evento
056000000000     d spcev           S              3    DIM(200)                             *causale evento
056100130313     d spspe           S             16  0 DIM(200)                             *causale evento
056200000000      * note giacenze memorizzate
056300051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
056400051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
056500000000      * note memorizzate
056600000000     d smnot           S            100    DIM(10)
056700030206     d*---
056800030206     d* definizioni per passaggio parametri
056900030206     d*---
057000000000     d esito           s              1
057100000000      * DS contenente i dati di INPUT
057200140603     d tis778dsi     e DS                  INZ(*EXTDFT)
057300050104     D  cAASI74                       4
057400050104     D                                     OVERLAY(AASI74)
057500000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
057600140530     d tis778dso     e DS                  INZ(*EXTDFT)
057700000000     d dev                           10    DIM(50) OVERLAY(dato74)
057800000000     d hev                            5    DIM(50) OVERLAY(orao74)
057900000000     d fle                           30    DIM(50) OVERLAY(opeo74)
058000041007     d eve                           30    DIM(50) OVERLAY(eveo74)
058100041007     d cev                            3    DIM(50) OVERLAY(cevo74)
058200000000     d nota                         100    DIM(10) OVERLAY(notao74)
058300130313     d spe             S             16  0 DIM(50)
058400040906
058500140616     d tis778dsM     e DS                  INZ(*EXTDFT)
058600140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
058700140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
058800140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
058900140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
059000140530      *
059100040906     D FIDN12DS      E DS
059200040906     D                                     INZ
059300040906     D  I12FEL       E
059400040906     D                                     INZ('E')
059500040906     D  I12FAN       E
059600040906     D                                     INZ('E')
059700040906     D  O12KeyAry                    14
059800040906     D                                     DIM(20)
059900040906     D                                     OVERLAY(O12KEY)
060000040906
060100040906     D O12KeyDS        DS
060200040906     D                                     INZ
060300040906     D  O12KeyAAC                     4S 0
060400040906     D  O12KeyFil                     3S 0
060500040906     D  O12KeyNCA                     7S 0
060600040906
060700020702      * DS lettura tabella "NTW"
060800020702     d DNTW          E DS
060900130326     d Ddstflr       E DS
061000041223
061100041223     D ChkCode         S             10U 0
061200041223     D nEsito          S              5I 0
061300051110     D esito10         S             10I 0
061400080131
061500100119     D*--------------------------------------------------
061600100119     D* Procedure name: GetDescrizioneIncassoContrassegno
061700100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
061800100119     D*                          o del contrassegno.
061900100121     D* Returns:        Descrizione modalità incasso contrassegno.
062000100119     D*--------------------------------------------------
062100100119     D GetDescrizioneIncassoContrassegno...
062200100119     D                 PR            35A
062300110304
062400110304     D*--------------------------------------------------
062500110304     D* Procedure name: IsContrassegnoCompensato
062600110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
062700110304     D*                          to con una fattura.
062800110304     D* Returns:        *ON = Compensato
062900110304     D*--------------------------------------------------
063000110304     D IsContrassegnoCompensato...
063100110304     D                 PR              N
063200120321
063300120321     D*--------------------------------------------------
063400120321     D* Procedure name: SetIdAssegnoMittente
063500120321     D* Purpose:        Imposta l'ID assegno mittente.
063600120321     D* Returns:        Esito.
063700120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
063800120321     D*                          segni mittente.
063900120321     D* Parameter:      priAbi => ABI assegno.
064000120321     D* Parameter:      priCab => CAB assegno.
064100120321     D*--------------------------------------------------
064200120321     D SetIdAssegnoMittente...
064300120321     D                 PR            10I 0
064400120321     D  priNumeroAssegno...
064500120321     D                                     LIKE(caNumO74)
064600120321     D  priAbi                             LIKE(caAbiO74)
064700120321     D  priCab                             LIKE(caCabO74)
064800130513
064900130513     D*--------------------------------------------------
065000130513     D* Procedure name: GetFilUrl
065100130513     D* Purpose:        Restituisce l'URL della filiale.
065200130513     D* Returns:        URL filiale.
065300130513     D* Parameter:      piFiliale => ID filiale
065400130513     D*--------------------------------------------------
065500130513     D GetFilUrl       PR           256A
065600130513     D  piFiliale                     3P 0 CONST
065700130513
065800130513
065900140527       //--------------------------------------------------------------
066000140527       //?Definizione prototipi.
066100140527       //--------------------------------------------------------------
066200140527      /copy gaitrasrc/srcprotopr,tis778R
066300140529      /copy gaitrasrc/srcconst,tis778R
066400140527       //--------------------------------------------------------------
066500140527       //?Definizione parametri programma.
066600140527       //--------------------------------------------------------------
066700140529     d tis778_GetBolla...
066800140527     d                 PI
066900140527     d prmRqsOpCode...
067000140527     d                               10I 0 CONST
067100140527     d prmRpyOpCode...
067200140527     d                               10I 0
067300140527     d prmRpyIdMsg...
067400140527     d                               10I 0
067500140527     d prmRqsFormato...
067600140527     d                               10A   CONST
067700140527     d prmRqsData...
067800140527     d                            32767A   OPTIONS(*VARSIZE)
067900140527     d prmRqsDataSize...
068000140527     d                               10I 0 CONST
068100140527     d prmRpyFormato...
068200140527     d                               10A   CONST
068300140527     d prmRpyData...
068400140527     d                            32767A   OPTIONS(*VARSIZE)
068500140527     d prmRpyDataSize...
068600140527     d                               10I 0 CONST
068700140527     d prmRpyFormMsg...
068800140527     d                               10A   CONST OPTIONS(*NOPASS)
068900140527     d prmRpyMessage...
069000140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
069100140527     d prmRpyMsgSize...
069200140527     d                               10I 0 CONST OPTIONS(*NOPASS)
069300080131
069400000000     i*--------------------------------------------------------------------------------------------*
069500000000     i* Input
069600000000     i*--------------------------------------------------------------------------------------------*
069700000000     i*---
069800000000     i* Indicatori lettura formati record -TITAS-
069900000000     i*---
070000000000     ititas000      01
070100000000     ititas010      02
070200000000     ititasp00      03
070300000000     c*--------------------------------------------------------------------------------------------*
070400000000     c* Main lines
070500000000     c*--------------------------------------------------------------------------------------------*
070600000000     c*
070700000000     c* operazioni iniziali -da fare sempre-
070800000000     c                   EXSR      rutinz
070900000000     c*
071000000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
071100000000if  1c                   IF        esito <> '1'                                 *no errori
071200100219     c*
071300100219     c                   MOVEL     dslnpi74      kaslnp
071400100219     c                   MOVEL     dsnrsi74      kasnrs
071500100219     c                   MOVEL     dsnspi74      kasnsp
071600100219     c*
071700111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
071800111103     c* restituisco errore.
071900111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
072000111103     c* e se non la trovo provo con l'anno precedente.
072100111103     c                   IF        aasI74 <> *ZERO
072200100219     c                   Z-ADD     aasI74        kasaas
072300100219     c     keytas        SETLL     titas30c                               99
072400111103     c                   IF        NOT *IN99
072500111103     c                   EVAL      esito = '1'
072600111103     c                   ENDIF
072700111103     c                   ELSE
072800000000     c                   Z-ADD     anncor        kasaas
072900000000     c     keytas        SETLL     titas30c                               99
073000000000if  2c                   IF        NOT *in99                                    *non trovato
073100000000     c                   Z-ADD     annpre        kasaas
073200000000     c     keytas        SETLL     titas30c                               99
073300000000if  3c                   IF        NOT *in99                                    *non trovato
073400000000     c                   MOVEL     '1'           esito                          *errore
073500000000e   3c                   ENDIF
073600000000e   2c                   ENDIF
073700100219     c                   ENDIF
073800140616     c                   if        esito = '1'
073900140617     c                   eval      widmsg = 'TIS0564'
074000140616     c                   eval      widcampo = 'NSPEDIZI74'
074100140616     c                   clear                   wparm
074200140616     c                   exsr      messaggi
074300140616     c                   endif
074400141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
074500141126     c                   eval      kasaasm = kasaas
074600141126     c                   eval      kaslnpm = kaslnp
074700141126     c                   eval      kasnrsm = kasnrs
074800141126     c                   eval      kasnspm = kasnsp
074900141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
075000140715      * ultima figlia per forzare i dati in output
075100140714     c                   exsr      cercafiglia
075200000000     c*
075300000000     c* se trovata, legge la bolla
075400000000if  2c                   IF        esito <> '1'
075500000000     C                   SETOFF                                       010203
075600140715     c     keytas        SETLL     titas30c
075700000000     c     keytas        READE     titas30c                               99    *no errore
075800000000if  3c                   IF        *in99                                        *non trovato
075900000000     c                   MOVEL     '1'           esito                          *errore
076000140617     c                   eval      widmsg = 'TIS0734'
076100140616     c                   eval      widcampo = 'NSPEDIZI74'
076200140616     c                   clear                   wparm
076300140616     c                   exsr      messaggi
076400000000x   3c                   ELSE                                                   *trovata
076500000000if  4c                   IF        *in01                                        *lettura titas000
076600000000     c                   EVAL      titasdssav = titasds
076700000000e   4c                   ENDIF
076800000000if  4c                   IF        *in02                                        *lettura titas010
076900000000     c                   EVAL      titasdssav = titasds
077000000000e   4c                   ENDIF
077100000000if  4c                   IF        *in03                                        *lettura titasp00
077200000000     c                   EVAL      titasdssav = titasds
077300000000e   4c                   ENDIF
077400141117      * se trovo la figlia ma ancora non è su titas
077500141117     c                   if        keyforzata <> *blank and
077600141117     c                             f_tasnsp = 0
077700141117     c                   eval      titasdssavf = titasdssav
077800141120     c                   else
077900141120     c                   clear                   keyforzata
078000141117     c                   endif
078100000000e   3c                   ENDIF
078200000000     c*
078300000000     c* controlla validità bolla
078400000000     c                   EXSR      chktas
078500000000     c*
078600030130     c* imposta i campi della DS di Output
078700000000if  3c                   IF        esito <> '1'                                 *no errore
078800140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
078900140714      * spedizione richiesta a video sempre
079000100225     c*
079100140716     c                   EVAL      dstaslnp = s_taslnp
079200140716     c                   EVAL      dstasnrs = s_tasnrs
079300140716     c                   EVAL      dstasnsp = s_tasnsp
079400140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
079500140723     c* chain ar5 con bolla richiesta a video
079600140723     c                   Z-ADD     s_tasaas      kr5aas
079700140723     c                   Z-ADD     s_taslnp      kr5lnp
079800140723     c                   Z-ADD     s_tasnrs      kr5nrs
079900140723     c                   Z-ADD     s_tasnsp      kr5nsp
080000100225     c                   MOVEL     'GEN'         kr5trd
080100100225     c     keyar5        CHAIN     fiar531c
080200100225     C                   IF        %FOUND
080300100225     C                   EVAL      DAR5GEN = AR5Uni
080400100225     C                   ELSE
080500100225     C                   RESET                   DAR5GEN
080600100225     C                   ENDIF
080700000000     c*
080800000000     c* imposta i campi della DS di Output dalle bolle
080900000000     c                   EXSR      impdsotas
081000000000     c*
081100000000     c* imposta i campi della DS di Output dagli eventi
081200000000     c                   EXSR      impdsoevb
081300000000     c*
081400000000     c* imposta i campi della DS di Output dal contrassegno
081500000000     c                   EXSR      impdsocsb
081600000000     c*
081700000000     c* imposta i campi della DS di Output dalla giacenza
081800000000     c                   EXSR      impdsogcp
081900000000     c*
082000000000     c* imposta i campi della DS di Output dalle note
082100000000     c                   EXSR      impdsonot
082200040330     c*
082300040330     c* imposta i campi della DS di Output del destinatario.
082400040330     c                   EXSR      ImpDSDest
082500010716     C*
082600010716     c* imposta i campi della DS di Output dalla firma DPD
082700010716     c                   EXSR      ImpdsoDPD
082800040913     C*
082900040913     c* imposta i campi della DS di Output di addebito immagine LDV.
083000040913     c                   EXSR      AddebitoLDV
083100000000e   3c                   ENDIF
083200000000e   2c                   ENDIF
083300140613e   1c                   ENDIF
083400140919      *riempie campi con riferimenti alla consegna
083500141016     c                   if        esito <> '1'
083600140919     c                   if        f_tasdcm > 0
083700140919     c                   movel     f_tasdcm      wdat
083800140919     c                   exsr      edtdat
083900140919     c                   eval      dtconmeo74  = wdate
084000140919     c                   MOVEL     f_tashmc      wora
084100140919     c                   EXSR      edtora
084200140919     c                   eval      orconmeO74  =  worae
084300140919     c                   eval      flgbolcO74  =  'S'
084400140919     c                   eval      flgboldO74  =  'N'
084500141014      *firmatario distinta PDA
084600141014     c                   clear                   firmato74
084700141015     c                   if        gen§ar5pdaco <> 'NO'
084800141014     c                   exsr      repfirmat
084900141014     c                   else
085000141014     c                   if        f_tasgma <> *blank
085100141015     C                   EVAL      tblKey = f_tasgma
085200141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
085300141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
085400141014     C                             :rpyOpCode:rpyEsito)
085500141014     C                   IF        rpyOpCode = 'FOUND'
085600141014     c                   if        §7r2fi = 'S'
085700141014     c                   exsr      repfirmat
085800141014     c                   endif
085900141014     c                   endif
086000141014     c                   endif
086100141014     c                   endif
086200141014      *else consegna
086300140919     c                   else
086400140919     c                   eval      flgbolcO74  =  'N'
086500140919     c                   if        F_tasndc > 0 and f_tasddc > 0
086600141119     c                             and f_tasddc <= datcor
086700140919     c                   eval      flgboldo74 = 'S'
086800140919     c                   else
086900140919     c                   eval      flgboldO74  =  'N'
087000140919     c                   endif
087100140919     c                   endif
087200130312     c* se ultimo evento inserito è un MIC (1° della schiera perchè
087300130312     c* è discendente) verifico situazione delle spedizione in filiale
087400140613if  3c                   IF        esito <> '1'                                 *no errore
087500130312     c                   IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
087600140609     c                             and cev(49) = ' ' and cev(50) = ' ' or
087700141120     c                             f_tasdcm = 0 and keyforzata = *blank
087800130312     c                   exsr      srarb
087900140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
088000140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
088100140605     c                   exsr      impdsoarb
088200140604     c                   endif
088300140613     c                   endif
088400140613     c                   endif
088500141016     c                   endif
088600000000     c*
088700140530     C                   exsr      routend
088800141014      *__________________________________________________________________
088900141014     c     repfirmat     begsr
089000141014      *__________________________________________________________________
089100141014     c                   Z-ADD     f_tasaas      ka4aas
089200141014     c                   Z-ADD     f_taslnp      ka4lnp
089300141014     c                   Z-ADD     f_tasnrs      ka4nrs
089400141014     c                   Z-ADD     f_tasnsp      ka4nsp
089500141014     c                   MOVEL     '1'           ka4trc
089600141014     c     keyta4        CHAIN     tita430c                           99
089700141014if  1c                   IF        NOT *in99
089800141014     c                   MOVEL     ta4not        firmato74
089900141014e   1c                   ENDIF
090000141014     c                   endsr
090100141015      *__________________________________________________________________
090200141015     c     repfirmatARB  begsr
090300141015      *__________________________________________________________________
090400141015      *firmatario da arb
090500141015     c                   Z-ADD     arbaas        ka4aas
090600141015     c                   Z-ADD     arblnp        ka4lnp
090700141015     c                   Z-ADD     arbnrs        ka4nrs
090800141015     c                   Z-ADD     arbnsp        ka4nsp
090900141015     c                   MOVEL     '1'           ka4trc
091000141015     c     keyta4        CHAIN     fiar401l                           99
091100141015if  1c                   IF        NOT *in99
091200141015     c                   MOVEL     ar4not        firmato74
091300141015e   1c                   ENDIF
091400141015     c                   endsr
091500000000     c*--------------------------------------------------------------------------------------------*
091600000000     c* controlla validità bolla
091700000000     c*--------------------------------------------------------------------------------------------*
091800000000     c     chktas        BEGSR
091900000000     c*
092000000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
092100000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
092200000000     c                   Z-ADD     s_tasmgs      dsmgsB
092300000000if  1c                   IF        dsdatB < datpre
092400000000     c                   MOVEL     '1'           esito                          *errore
092500000000e   1c                   ENDIF
092600030130     c* se l'esito è positivo, salva alcuni dati della bolla
092700140718     c                   EVAL      savtsp = f_tastsp
092800000000     c*
092900140718     C                   EVAL      wOrg = f_tasLna
093000051222     C                   EXSR      setLnaItaEst
093100051222     C
093200000000     c                   ENDSR
093300000000     c*--------------------------------------------------------------------------------------------*
093400000000     c* imposta i campi della DS di Output dalle bolle e files correlati
093500000000     c*--------------------------------------------------------------------------------------------*
093600140604     c     impdsotas     BEGSR
093700140604     c*
093800140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
093900140604     c                   EVAL      lnaDescO74 = sodes(i)
094000140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
094100150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
094200150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
094300150116     c                   MOVE      s_tasmgs      aasmgsO74
094400150116      *
094500140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
094600140718     c                   MOVEL     f_taslna      lineaaro74
094700140604     c*
094800140718     c                   MOVEL     f_tastbl      a1
094900140718     c                   MOVEL     f_tastbl      tpporto74
095000140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
095100140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
095200140604x   1c                   ELSE                                                   *Pagamento del
095300140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
095400140604e   1c                   ENDIF
095500140716     c                   MOVEL     f_tastsp      tpservio74
095600140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
095700140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
095800140604     C                             rpyOpCode:rpyEsito)
095900140604     C                   EVAL      corro74 = §5EDes
096000140604     c*
096100140716     c                   EVAL      destino74 = f_tasrsd
096200140716     c                   EVAL      desindo74 = f_tasind
096300140716     c                   EVAL      descapo74 = f_tascad
096400140716     c                   EVAL      desloco74 = f_taslod
096500140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
096600150326if  1c                   IF        f_tasnzd  = *blanks
096700150326     c                   EVAL      desnazo74 = f_tasprd
096800150326x   1c                   ELSE
096900150326     c***                EVAL      desnazo74 = f_tasnzd
097000150326     c***                EVAL      desnzdo74 = f_tasnzd
097100150326     c* 26/03/15: Attiviamo le specifiche per provincia export
097200150326     c                   EVAL      desnazo74 = f_tasprd
097300150326     c                   EVAL      desnzdo74 = f_tasnzd
097400150326e   1c                   ENDIF
097500140604     c*
097600140716     c                   EVAL      collio74  = f_tasncl
097700140716     c                   EVAL      pesoo74   = f_taspkf
097800140716     c                   EVAL      volumeo74 = f_tasvlf
097900140716     c                   EVAL      naturao74 = f_tasnas
098000140604     c*
098100140716     c                   EVAL      fatnumo74  = f_tasnft
098200140716     c                   MOVEL     f_tasdft      wdat
098300140604     c                   EXSR      edtdat
098400140604     c                   EVAL      fatdatao74 = wdate
098500140604     c*
098600140716     c                   EVAL      disnumo74  = f_tasndc
098700140716c    c                   MOVEL     f_tasddc      wdat
098800140604     c                   EXSR      edtdat
098900140604     c                   EVAL      disdatao74 = wdate
099000140604     **?Immagine lettera di vettura.
099100140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
099200140604     C                   IF        LDVO74 <> 'N'
099300140604     c                   CLEAR                   dtasflo
099400140716     c                   MOVEL     f_tasflo      dtasflo
099500140604     c                   IF        §floiml <> *BLANK
099600140604     c                   EVAL      ldvo74 = 'S'
099700140604     C                   EVAL      SpImLDVO74 =
099800140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
099900140716     C                             %EDITC(f_taslnp:'X') +
100000140716     C                             %EDITC(f_tasnrs:'X') +
100100140716     C                             %EDITC(f_tasnsp:'X')
100200140604     **?Reperisco il check code della spedizione POD
100300140604     C                   EVAL      PChkCdeO74 =
100400140604     C                             GetSpeChkCde(
100500140716     C                             %EDITC(f_tasaas:'X'):
100600140716     C                             %EDITC(f_taslnp:'X') +
100700140716     C                             %EDITC(f_tasnrs:'X') +
100800140716     C                             %EDITC(f_tasnsp:'X'):
100900140604     C                             ChkCode:nEsito)
101000140604     C                   ENDIF
101100140604     C                   ENDIF
101200140604     c*
101300140716     c                   MOVE      f_tasccm      n4
101400140604if  1c                   IF        n4 <> 8888                                   *codificato
101500140604     c                   CLEAR                   bs69ds
101600140604     c                   CLEAR                   acods
101700140604     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
101800140716     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
101900140716     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
102000140604     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
102100140604     c                   PARM                    bs69ds
102200140604     c                   PARM                    acods
102300140604     c                   PARM                    indds
102400140604     c                   PARM                    clpds
102500140604     c                   PARM                    clsds
102600140604if  2c                   IF        o69err <> '1'                                *no errore
102700140604     c                   EVAL      mito74    = acorag
102800140604     c                   EVAL      mitindo74 = indvia
102900140604     c                   EVAL      mitloco74 = indcit
103000140604     c                   MOVEL     indsta        a3
103100140604if  3c                   IF        a3 = *blanks
103200140604     c                   EVAL      mitnazo74 = indprv
103300140604x   3c                   ELSE
103400140604     c                   EVAL      mitnazo74 = a3
103500140604e   3c                   ENDIF
103600140604if  3c                   IF        indcae <> *blanks
103700140604     c                   EVAL      mitcapo74 = indcae
103800140604X   3c                   ELSE
103900140604     c                   MOVEL     indcap        a5
104000140604     c                   EVAL      mitcapo74 = a5
104100140604e   3c                   ENDIF
104200140604e   2c                   ENDIF
104300140604x   1c                   ELSE                                                   *non codificato
104400140716     c                   Z-ADD     f_tasaas      kaaaas
104500140716     c                   Z-ADD     f_taslnp      kaalnp
104600140716     c                   Z-ADD     f_tasnrs      kaanrs
104700140716     c                   Z-ADD     f_tasnsp      kaansp
104800140604     c                   MOVEL     'M'           kaatrc
104900140604     c     keytaa        CHAIN     titaa30c                           99
105000140604if  2c                   IF        NOT *in99
105100140604     c                   EVAL      mito74    = taarsc
105200140604     c                   EVAL      mitindo74 = taaind
105300140604     c                   EVAL      mitcapo74 = taacap
105400140604     c                   EVAL      mitloco74 = taaloc
105500140604if  3c                   IF        taanaz = *blanks
105600140604     c                   EVAL      mitnazo74 = taaprv
105700140604x   3c                   ELSE
105800140604     c                   EVAL      mitnazo74 = taanaz
105900140604e   3c                   ENDIF
106000140604e   2c                   ENDIF
106100140604e   1c                   ENDIF
106200140604     c* 2a ragione sociale destinatario.
106300140604     c                   CLEAR                   tita4000
106400140604     c                   CLEAR                   tita4p00
106500140716     c                   Z-ADD     f_tasaas      ka4aas
106600140716     c                   Z-ADD     f_taslnp      ka4lnp
106700140716     c                   Z-ADD     f_tasnrs      ka4nrs
106800140716     c                   Z-ADD     f_tasnsp      ka4nsp
106900140604     c                   MOVEL     'D'           ka4trc
107000140604     c     keyta4        CHAIN     tita430c                           99
107100140604if  1c                   IF        NOT *in99
107200140604     c                   MOVEL     ta4not        desti2O74
107300140604e   1c                   ENDIF
107400140604     c* rif partner estero
107500140604     c                   CLEAR                   tita4000
107600140604     c                   CLEAR                   tita4p00
107700140604     c                   CLEAR                   dsbl4e
107800140716     c                   Z-ADD     f_tasaas      ka4aas
107900140716     c                   Z-ADD     f_taslnp      ka4lnp
108000140716     c                   Z-ADD     f_tasnrs      ka4nrs
108100140716     c                   Z-ADD     f_tasnsp      ka4nsp
108200140604     c                   MOVEL     'E'           ka4trc
108300140604     c     keyta4        CHAIN     tita430c                           99
108400140604if  1c                   IF        NOT *in99
108500140604     c                   MOVEL     ta4not        dsbl4e
108600140604e   1c                   ENDIF
108700140604     c
108800140604     c                   CLEAR                   tita4000
108900140604     c                   CLEAR                   tita4p00
109000140604     c                   CLEAR                   dta4a
109100140716     c                   Z-ADD     f_tasaas      ka4aas
109200140716     c                   Z-ADD     f_taslnp      ka4lnp
109300140716     c                   Z-ADD     f_tasnrs      ka4nrs
109400140716     c                   Z-ADD     f_tasnsp      ka4nsp
109500140604     c                   MOVEL     'A'           ka4trc
109600140604     c     keyta4        CHAIN     tita430c                           99
109700140604if  1c                   IF        NOT *in99
109800140604     c                   MOVEL     ta4not        dta4a
109900140604e   1c                   ENDIF
110000140604     ** Riferimento mittente numerico (c'è sempre).
110100140716     C                   EVAL      rifero74 = f_tasrmn
110200140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
110300140604     C                   IF        §ta4arma <> *BLANK AND
110400140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
110500140604     C                   EVAL      rifero74a = §ta4arma
110600140604     C                   ENDIF
110700140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
110800140604     C                   IF        §b4erp <> *BLANK AND
110900140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
111000140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
111100140604     C                   EVAL      rpto74 = §b4erp
111200140604     C                   ENDIF
111300140604
111400140604     **?Reperisco il check code della spedizione.
111500140604     C                   EVAL      SChkCdeO74 =
111600140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
111700140604     C                             NSpedizI74:ChkCode:nEsito)
111800140604
111900140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
112000140604     C                   IF        KSCI74 <> *BLANK
112100140604     c                   SETOFF                                           98
112200140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
112300140604     C                   RESET                   tis7700i0
112400141126     C                   EVAL      tis7700i0.aas = kasaasm
112500141126     C                   EVAL      tis7700i0.lnp = kaslnpm
112600141126     C                   EVAL      tis7700i0.nrs = kasnrsm
112700141126     C                   EVAL      tis7700i0.nsp = kasnspm
112800140604     C                   EVAL      tis7700i0.ksu = kscI74
112900140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
113000140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
113100140604     C                             : rpyEsito
113200140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
113300140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
113400140604     C                             )
113500140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
113600140604     C                             tis7700o0.bollValida = *ON)
113700140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
113800140604if  2c                   IF        NOT *in98                                      *non trovato
113900140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
114000140604     C                   reset                   TIS778DSO
114100140604     C                   EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
114200140604     C                   RETURN
114300140604     C                   ENDIF
114400140604e   2c                   ENDIF
114500140604     C                   ENDIF
114600140604     c*
114700140604     ** Importo assicurato.
114800140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
114900140716     C                   EVAL      vasO74 = f_tasVas
115000141015     C                   EVAL      iasO74 = f_tasIas
115100140604     C                   ENDIF
115200140604     **
115300140604     c                   ENDSR
115400000000     c*--------------------------------------------------------------------------------------------*
115500000000     c* imposta i campi della DS di Output dagli eventi
115600000000     c*--------------------------------------------------------------------------------------------*
115700000000     c     impdsoevb     BEGSR
115800000000     c*
115900000000     c* azzera le variabili di lavoro
116000000000     c                   CLEAR                   tote                           *contatore eventi
116100000000     c                   CLEAR                   seql                           *sequenza legami
116200000000     c                   CLEAR                   spsort                         *per ordinamento
116300000000     c                   CLEAR                   spseql                         *di memorizzazione
116400000000     c                   CLEAR                   spseqe
116500000000     c                   CLEAR                   spdevhev
116600000000     c                   CLEAR                   spdeve
116700000000     c                   CLEAR                   spheve
116800000000     c                   CLEAR                   spfle
116900000000     c                   CLEAR                   spdev
117000000000     c                   CLEAR                   spcev
117100130312     c                   CLEAR                   spspe
117200000000     c*
117300000000     c* imposta la sequenza legami per la bolla di Input
117400000000     c                   Z-ADD     1             seql                           *primo legaqme
117500000000     c*---
117600000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
117700000000     c*---
117800000000     c***
117900000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
118000000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
118100000000     c*   si trasforma nella consegna anomala della mamma
118200000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
118300000000     c***
118400000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
118500000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
118600000000     c                   EVAL      kblaan = s_tasaas
118700000000     c                   EVAL      kbllpn = s_taslnp
118800000000     c                   EVAL      kblnrn = s_tasnrs
118900000000     c                   EVAL      kblnsn = s_tasnsp
119000000000     c     keylbl        CHAIN     fnlbl01l                           99
119100000000if  1c                   IF        NOT *in99                                    *ha la mamma
119200000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
119300000000     c                   EVAL      kasaas = lblaap
119400000000     c                   EVAL      kaslnp = lbllpp
119500000000     c                   EVAL      kasnrs = lblnrp
119600000000     c                   EVAL      kasnsp = lblnsp
119700000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
119800000000if  2c                   IF        NOT *in99 AND
119900000000     c                             tascca <> *blanks                            *ha cons.anomala
120000060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
120100060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
120200060622     C                             :rpyEsito)
120300060622if  3c*                  IF        *in98                                        *trovata
120400060626     C                   IF        rpyOpCode = 'FOUND'
120500041222     **?Consegna anomala, POD non visualizzabile.
120600060622     C*                  IF        sapod(jj) = 'N'
120700060622     C                   IF        §7OPODImg = 'N'
120800041222     C                   EVAL      LDVO74 = 'N'
120900041222     C                   ENDIF
121000060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
121100060622     C                   IF        §7oinc <> *blanks                            *ok per internet
121200000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
121300041222e   3c                   ENDIF
121400000000e   3c                   ENDIF
121500000000e   2c                   ENDIF
121600000000e   1c                   ENDIF
121700000000     c*
121800030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
121900000000     c                   ADD       1             tote
122000060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
122100060622     c                   MOVEL     §7odei        smdev(tote)
122200030203x   3c                   ELSE                                                   *evento telef.P.O.
122300060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
122400030203e   3c                   ENDIF
122500030203e   2c                   ENDIF
122600030203     c*
122700030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
122800060622     c                   MOVEL     §7ocev        wcev                           *causale evento
122900000000     c                   EXSR      evbrit                                       *imposta evento
123000030203x   2c                   ELSE                                                   *NO sost.ritiro
123100030203if  3c                   IF        s_tasdrt > *zeros
123200000000     c                   MOVEL     '701'         wcev                           *causale evento
123300000000     c                   EXSR      deccevsta
123400030203if  4c                   IF        werr <> '1'                                  *no errore
123500000000     c                   ADD       1             tote
123600030130     c                   MOVEL     wcdex         smdev(tote)
123700000000     c                   EXSR      evbrit                                       *imposta evento
123800030203e   4c                   ENDIF
123900030203e   3c                   ENDIF
124000030203e   2c                   ENDIF
124100140710     ** ID bolla madre.                                                          dubito figlia
124200080131     c                   EVAL      waas = s_tasaas
124300080131     c                   EVAL      wlnp = s_taslnp
124400080131     c                   EVAL      wnrs = s_tasnrs
124500080131     c                   EVAL      wnsp = s_tasnsp
124600000000     c* Partenza merce
124700000000     c                   EVAL      lduc = s_tasduc                              *data partenza
124800000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
124900000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
125000000000     c                   EXSR      evbpar
125100000000     c* Arrivo merce
125200110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
125300110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
125400140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
125500140723     c                             gen§ar5ArrDt <> *ZERO
125600140723     c                   MONITOR
125700140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
125800140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
125900140723     c                   ON-ERROR
126000140723     c                   ENDMON
126100140723     c                   ENDIF
126200100225     c                   EVAL      llna = s_taslna                              *linea arrivo
126300090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
126400090826     c                   EXSR      evbarr
126500000000     c*
126600000000     c* Consegna merce
126700000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
126800000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
126900000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
127000000000     c                   EVAL      llna = s_taslna                              *linea arrivo
127100000000     c                   EXSR      evbcon
127200000000     c* Lettera anomalia
127300000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
127400000000     c                   EXSR      memdct
127500000000e   1c                   ENDIF
127600000000     c*---
127700000000     c* memorizza gli eventi dal file eventi della bolla di input
127800000000     c*---
127900000000     c                   EXSR      memevb
128000000000     c*---
128100000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
128200000000     c*---
128300000000     c                   EXSR      elaevelbl
128400000000     c*
128500000000     c* imposta contatore eventi da visualizzare nella DS di Output
128600000000     c                   EVAL      cnteveo74 = tote
128700000000     c*---
128800000000     c* ordina gli eventi per sequenza legami / Data+ora evento
128900000000     c*---
129000000000     c* compone la schiera da ordinare
129100000000do  1c     1             DO        200           j
129200000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
129300000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
129400000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
129500000000     c                   Z-ADD     smi(j)        dsi                             -indice
129600000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
129700000000e   1c                   ENDDO
129800000000     c*
129900000000     c* ordina la schiera composta
130000000000     c                   SORTA     spsort
130100000000     c*
130200000000     c* ricompone le schiere da quella ordinata
130300000000     c                   Z-ADD     *zeros        jj
130400000000do  1c     1             DO        200           j
130500000000if  2c                   IF        spsort(j) >*zeros
130600000000     c                   ADD       1             jj
130700000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
130800000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
130900000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
131000000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
131100000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
131200000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
131300000000     c                   MOVEL     smheve(dsi)   spheve(jj)
131400000000     c                   MOVEL     smfle(dsi)    spfle(jj)
131500000000     c                   MOVEL     smdev(dsi)    spdev(jj)
131600000000     c                   MOVEL     smcev(dsi)    spcev(jj)
131700130312     c                   MOVEL     smspe(dsi)    spspe(jj)
131800000000e   2c                   ENDIF
131900000000e   1c                   ENDDO
132000101104     **
132100101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
132200101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
132300101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
132400101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
132500101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
132600101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
132700101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
132800101104     ** Ricordati che l'ordine per data e ora è discendente.
132900101104     C                   EVAL      j = 0
133000101104     C                   DOU       j = 0
133100101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
133200101104     C                                        : spcev : j+1)
133300101104     C                   IF        j = 0
133400101104     C                   LEAVE
133500101104     C                   ENDIF
133600101104     C                   IF        j > 1 AND
133700101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
133800101104     C                   EVAL      dev(1) = spdeve(j-1)
133900101104     C                   EVAL      hev(1) = spheve(j-1)
134000101104     C                   EVAL      fle(1) = spfle(j-1)
134100101104     C                   EVAL      eve(1) = spdev(j-1)
134200101104     C                   EVAL      cev(1) = spcev(j-1)
134300130312     C                   EVAL      spe(1) = spspe(j-1)
134400101104     C                   EVAL      spdeve(j-1) = spdeve(j)
134500101104     C                   EVAL      spheve(j-1) = spheve(j)
134600101104     C                   EVAL      spfle(j-1) = spfle(j)
134700101104     C                   EVAL      spdev(j-1) = spdev(j)
134800101104     C                   EVAL      spcev(j-1) = spcev(j)
134900130312     C                   EVAL      spspe(j-1) = spspe(j)
135000101104     C                   EVAL      spdeve(j) = dev(1)
135100101104     C                   EVAL      spheve(j) = hev(1)
135200101104     C                   EVAL      spfle(j) = fle(1)
135300101104     C                   EVAL      spdev(j) = eve(1)
135400101104     C                   EVAL      spcev(j) = cev(1)
135500130312     C                   EVAL      spspe(j) = spe(1)
135600101104     C                   ENDIF
135700101104     C                   ENDDO
135800000000     c*
135900120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
136000120831     c                   CLEAR                   cntEveO74
136100000000do  1c     1             DO        50            j
136200120831     c                   IF        spcev(j) = *BLANK
136300120831     c                   LEAVE
136400120831     c                   ENDIF
136500000000     c                   EVAL      dev(j) = spdeve(j)
136600000000     c                   EVAL      hev(j) = spheve(j)
136700000000     c                   EVAL      fle(j) = spfle(j)
136800041007     c                   EVAL      eve(j) = spdev(j)
136900041007     c                   EVAL      cev(j) = spcev(j)
137000130312     c                   EVAL      spe(j) = spspe(j)
137100101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
137200100909     c                   CLEAR                   hev(j)
137300100909     c                   ENDIF
137400120831     c                   EVAL      cntEveO74 += 1
137500000000e   1c                   ENDDO
137600000000     c*
137700000000     c                   ENDSR
137800000000     c*--------------------------------------------------------------------------------------------*
137900000000     c* memorizza gli eventi dal file eventi della bolla di input
138000000000     c*--------------------------------------------------------------------------------------------*
138100000000     c     memevb        BEGSR
138200000000     c***
138300000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
138400000000     c* . se SI lo aggiorna solo nella sequenza legame
138500000000     c*   se NO lo inserisce
138600000000     c***
138700000000     c                   Z-ADD     waas          kvbaas
138800000000     c                   Z-ADD     wlnp          kvblnp
138900000000     c                   Z-ADD     wnrs          kvbnrs
139000000000     c                   Z-ADD     wnsp          kvbnsp
139100000000     c     keyevb        SETLL     fnevb01l
139200000000     c     keyevb        READE     fnevb01l                               99
139300000000do  1c                   DOW       NOT *in99
139400000000if  2c                   IF        evbatb = *blanks                             *valido
139500140108     c                   MOVEL     evbdev        wdev
139600140108     c                   MOVEL     evbhev        whev
139700080131if  3c                   IF        evbDev > *zeros
139800000000     c                   MOVEL     evbcev        wcev
139900000000     c                   EXSR      deccevsta
140000070109     c                   EXSR      eventoAnnulla
140100000000if  4c                   IF        werr <> '1'                                  *no errore
140200000000     c*
140300000000     c* controlla se evento già inserito
140400000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
140500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
140600000000do  5c     1             DO        200           j
140700000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
140800080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
140900000000     c                   MOVEL     'N'           newevb                         *già inserito
141000000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
141100080130     c                   Z-ADD     tasLna        worg
141200000000     c                   EXSR      decorg
141300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
141400080201     c                   MOVEL     wDev          wdat
141500080201     c                   EXSR      edtdat
141600080201     c                   MOVEL     wdate         smdeve(j)
141700080201     c                   MOVEL     wHev          wora
141800080201     c                   EXSR      edtora
141900080201     c                   MOVEL     worae         smheve(j)
142000130313     c                   MOVEL     dsevbspe      smspe(j)
142100000000     c                   LEAVE                                                  *uscita ciclo
142200000000e   6c                   ENDIF
142300000000e   5c                   ENDDO
142400000000     c*
142500000000     c* incrementa contatore eventi
142600000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
142700000000     c                   ADD       1             tote
142800000000     c* indice schiera
142900000000     c                   Z-ADD     tote          smi(tote)
143000000000     c* sequenza legame
143100000000     c                   Z-ADD     seql          smseql(tote)
143200000000     c* sequenza evento
143300000000     c                   Z-ADD     020           smseqe(tote)
143400000000     c* descrizione
143500030130     c                   MOVEL     wcdex         smdev(tote)
143600000000     c* data
143700080201     c                   MOVEL     wDev          wdat
143800000000     c                   EXSR      edtdat
143900000000     c                   MOVEL     wdate         smdeve(tote)
144000000000     c* ora
144100080201     c                   MOVEL     wHev          wora
144200000000     c                   EXSR      edtora
144300000000     c                   MOVEL     worae         smheve(tote)
144400000000     c* filiale
144500140722     C                   IF        wrkLnaItaEst = ESTERO
144600140722     c                   Z-ADD     tasLna        worg
144700140722     c                   else
144800140722     c                   Z-ADD     evbfle        worg
144900140722     c                   endif
145000140722     c                   EXSR      decorg
145100140722     c                   MOVEL     wodes         smfle(tote)
145200000000     c* causale
145300000000     c                   MOVEL     evbcev        smcev(tote)
145400130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
145500130312     c* bolla legata
145600130312     c                   MOVEL     dsevbspe      smspe(tote)
145700000000     c* data+ora evento
145800080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
145900000000e   5c                   ENDIF
146000000000e   4c                   ENDIF
146100000000e   3c                   ENDIF
146200000000e   2c                   ENDIF
146300010717     c     keyevb        READE     fnevb01l                               99
146400000000e   1c                   ENDDO
146500000000     c*
146600000000     c                   ENDSR
146700000000     c*--------------------------------------------------------------------------------------------*
146800000000     c* memorizza gli "eventi fissi"
146900000000     c*--------------------------------------------------------------------------------------------*
147000000000     c     memevefix     BEGSR
147100080131     ** ID bolla in canna.
147200080131     c                   EVAL      waas = tasaas
147300080131     c                   EVAL      wlnp = taslnp
147400080131     c                   EVAL      wnrs = tasnrs
147500080131     c                   EVAL      wnsp = tasnsp
147600000000     c*
147700000000     c* Partenza merce
147800000000     c                   EVAL      lduc = tasduc                                *data partenza
147900000000     c                   EVAL      llnp = taslnp                                *filiale partenza
148000000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
148100000000     c                   EXSR      evbpar
148200000000     c* Consegna merce
148300000000     c                   EVAL      lcca = tascca                                *consegna anomala
148400000000     c                   EVAL      ldcm = tasdcm                                *data consegna
148500000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
148600000000     c                   EVAL      llna = taslna                                *linea arrivo
148700000000     c                   EXSR      evbcon
148800000000     c*
148900000000     c                   ENDSR
149000000000     c*--------------------------------------------------------------------------------------------*
149100000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
149200000000     c*--------------------------------------------------------------------------------------------*
149300000000     c     elaevelbl     BEGSR
149400000000     c*
149500000000     c* azzera le variabili di lavoro
149600000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
149700000000     c                   EVAL      deplpn = *zeros
149800000000     c                   EVAL      depnrn = *zeros
149900000000     c                   EVAL      depnsn = *zeros
150000000000     c*
150100000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
150200000000     c                   EVAL      kbllpp =  s_taslnp
150300000000     c                   EVAL      kblnrp =  s_tasnrs
150400000000     c                   EVAL      kblnsp =  s_tasnsp
150500000000     c*
150600000000     c* ciclo fino a fine mamme
150700000000     c     ke2lbl        SETLL     fnlbl02l
150800000000     c     ke2lbl        READE     fnlbl02l                               99
150900000000do  1c                   DOW       NOT *in99
151000000000     c*
151100000000     c* ciclo fino a fine figlie della mamma
151200000000do  2c                   DOW       NOT *in99
151300000000     c*
151400000000     c* legge la bolla dal legame
151500000000     c                   EVAL      kasaas = lblaan                              *figlia
151600000000     c                   EVAL      kaslnp = lbllpn
151700000000     c                   EVAL      kasnrs = lblnrn
151800000000     c                   EVAL      kasnsp = lblnsn
151900000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
152000000000if  3c                   IF        NOT *in99 AND                                *esistente
152100000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
152200000000     c                             tastbl <> 'A3' AND
152300000000     c                             tastbl <> 'F3' AND
152400000000     c                             tastbl <> 'AP' AND
152500000000     c                             tastbl <> 'FC'
152600041222     **?Immagine lettera di vettura.
152700041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
152800041222     C                   IF        LDVO74 <> 'N'
152900040330     c                   MOVEL     tasflo        dtasflo
153000040330     c                   IF        §floiml <> *BLANK
153100050325     c                   EVAL      ldvo74 = 'S'
153200040330     C                   EVAL      SpImLDVO74 =
153300040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
153400040330     C                             %EDITC(taslnp:'X') +
153500040330     C                             %EDITC(tasnrs:'X') +
153600040330     C                             %EDITC(tasnsp:'X')
153700041223     **?Reperisco il check code della spedizione POD
153800041223     C                   EVAL      PChkCdeO74 =
153900041223     C                             GetSpeChkCde(
154000041223     C                             %EDITC(tasaas:'X'):
154100041223     C                             %EDITC(taslnp:'X') +
154200041223     C                             %EDITC(tasnrs:'X') +
154300041223     C                             %EDITC(tasnsp:'X'):
154400041223     C                             ChkCode:nEsito)
154500040330     C                   ENDIF
154600041222     C                   ENDIF
154700000000     c* incrementa la sequenza legami per le bolle legate
154800000000     c                   ADD       1             seql                           *legaqme successivo
154900000000     c*---
155000000000     c* memorizza gli "eventi fissi"
155100000000     c*---
155200000000     c                   EXSR      memevefix
155300000000     c*---
155400000000     c* memorizza gli eventi
155500000000     c*---
155600000000     c                   EVAL      waas = lblaan                                *figlia
155700000000     c                   EVAL      wlnp = lbllpn
155800000000     c                   EVAL      wnrs = lblnrn
155900000000     c                   EVAL      wnsp = lblnsn
156000000000     c                   EXSR      memevb
156100000000     c*
156200000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
156300000000     c                   EVAL      depaan = lblaan                              *ultima figlia
156400000000     c                   EVAL      deplpn = lbllpn
156500000000     c                   EVAL      depnrn = lblnrn
156600000000     c                   EVAL      depnsn = lblnsn
156700000000e   3c                   ENDIF
156800000000     c*
156900000000     c* lettura successiva legami
157000000000     c     ke2lbl        READE     fnlbl02l                               99
157100000000e   2c                   ENDDO                                                  *fine figlie mamma
157200000000     c*
157300000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
157400000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
157500000000     c                             kbllpp <> deplpn OR
157600000000     c                             kblnrp <> depnrn OR
157700000000     c                             kblnsp <> depnsn
157800000000     c* nuova chiave
157900000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
158000000000     c                   EVAL      kbllpp =  deplpn
158100000000     c                   EVAL      kblnrp =  depnrn
158200000000     c                   EVAL      kblnsp =  depnsn
158300000000     c* controlla figlie
158400000000     c     ke2lbl        SETLL     fnlbl02l
158500000000     c     ke2lbl        READE     fnlbl02l                               99
158600000000if  3c                   IF        *in99                                        *non ha figlie
158700000000     c                   LEAVE                                                  *esce dal ciclo
158800000000e   3c                   ENDIF
158900000000x   2c                   ELSE                                                   *sempre stessa mamma
159000000000     c                   LEAVE                                                  *esce dal ciclo
159100000000e   2c                   ENDIF
159200000000e   1c                   ENDDO                                                  *fine mamme
159300000000     c*
159400000000     c                   ENDSR
159500000000     c*--------------------------------------------------------------------------------------------*
159600000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
159700000000     c*--------------------------------------------------------------------------------------------*
159800000000     c     evbrit        BEGSR
159900000000     c*
160000000000     c* indice schiera
160100000000     c                   Z-ADD     tote          smi(tote)
160200000000     c* sequenza legame
160300000000     c                   Z-ADD     seql          smseql(tote)
160400000000     c* sequenza evento
160500000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
160600000000     c* data
160700000000if  1c                   IF        s_tasdrt > *zeros
160800000000     c                   MOVEL     s_tasdrt      wdat
160900000000     c                   EXSR      edtdat
161000000000     c                   MOVEL     wdate         smdeve(tote)
161100000000e   1c                   ENDIF
161200000000     c* ora
161300000000if  1c                   IF        s_tashrt > *zeros
161400000000     c                   MOVEL     s_tashrt      wora
161500000000x   1c                   ELSE
161600000000if  2c                   IF        s_tasfpp = 'P'                               *PM
161700000000     c                   MOVEL     1800          wora
161800000000x   2c                   ELSE                                                   *AM
161900000000     c                   MOVEL     1200          wora
162000000000e   2c                   ENDIF
162100000000e   1c                   ENDIF
162200000000     c                   EXSR      edtora
162300000000     c                   MOVEL     worae         smheve(tote)
162400000000     c* filiale
162500000000     c                   Z-ADD     s_taslnp      worg
162600000000     c                   EXSR      decorg
162700000000     c                   MOVEL     wodes         smfle(tote)
162800000000     c* causale
162900000000     c                   MOVEL     wcev          smcev(tote)
163000000000     c* data+ora evento
163100000000     c                   MOVEL     wdat          smdevhev(tote)
163200000000     c                   MOVE      wora          smdevhev(tote)
163300000000     c*
163400000000     c                   ENDSR
163500000000     c*--------------------------------------------------------------------------------------------*
163600000000     c* Imposta l'evento Partenza
163700000000     c*--------------------------------------------------------------------------------------------*
163800000000     c     evbpar        BEGSR
163900000000     c*
164000000000     c* controlla SE e COME inserire l'evento
164100000000if  1c                   IF        lduc > *zeros
164200000000     c* imposta causale
164300000000     c                   MOVEL     '702'         wcev
164400000000     c                   EXSR      deccevsta
164500000000if  2c                   IF        werr <> '1'                                  *no errore
164600000000     c* imposta filiale
164700000000     c                   Z-ADD     llnp          worg
164800000000     c                   EXSR      decorg
164900000000     c* imposta data
165000101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
165100000000     c                   MOVEL     ldrt          wdat                           *data ritiro
165200000000x   3c                   ELSE                                                   partenza ITALIA
165300000000     c                   MOVEL     lduc          wdat                           *data partenza
165400000000e   3c                   ENDIF
165500000000     c                   EXSR      edtdat
165600000000     c* imposta ora
165700000000     c                   MOVEL     2100          wora                           *ora imposta
165800000000     c                   EXSR      edtora
165900000000     c*
166000000000     c* controlla se l'evento è già stato inserito
166100000000     c                   MOVEL     'S'           newevb                         *nuovo evento
166200000000do  3c     1             DO        200           j
166300000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
166400000000     c                   MOVE      wora          lavdevhev
166500000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
166600000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
166700000000     c                   MOVEL     'N'           newevb                         *già inserito
166800000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
166900000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
167000000000     c                   LEAVE                                                  *uscita ciclo
167100000000e   4c                   ENDIF
167200000000e   3c                   ENDDO
167300000000     c*
167400000000     c* incrementa contatore eventi
167500000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
167600000000     c                   ADD       1             tote
167700000000     c* indice schiera
167800000000     c                   Z-ADD     tote          smi(tote)
167900000000     c* sequenza legame
168000000000     c                   Z-ADD     seql          smseql(tote)
168100000000     c* sequenza evento
168200000000     c                   Z-ADD     020           smseqe(tote)
168300000000     c* descrizione
168400030130     c                   MOVEL     wcdex         smdev(tote)
168500000000     c* filiale -impostata sopra-
168600000000     c                   MOVEL     wodes         smfle(tote)
168700000000     c* data    -impostata sopra-
168800000000     c                   MOVEL     wdate         smdeve(tote)
168900000000     c* ora     -impostata sopra-
169000000000     c                   MOVEL     worae         smheve(tote)
169100000000     c* causale -impostata sopra-
169200000000     c                   MOVEL     wcev          smcev(tote)
169300000000     c* data+ora evento
169400000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
169500000000     c                   MOVE      wora          smdevhev(tote)
169600000000e   3c                   ENDIF
169700000000e   2c                   ENDIF
169800000000e   1c                   ENDIF
169900000000     c*
170000000000     c                   ENDSR
170100101129     c*------------------------------------------------------------------------*
170200101129     c*
170300101129     c* Imposta l'evento Arrivo
170400101129     c*
170500101129     c*------------------------------------------------------------------------*
170600000000     c     evbarr        BEGSR
170700101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
170800101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
170900101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
171000101129     ** sommato inutile.
171100130312     C                   IF        wrkLnaItaEst = ESTERO
171200101104     C                   LEAVESR
171300101104     C                   ENDIF
171400101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
171500101129     C                   IF        lhti = *ZERO
171600101129     C                   LEAVESR
171700101129     C                   ENDIF
171800000000     c*
171900000000     c* controlla SE e COME inserire l'evento
172000000000if  1c                   IF        ldti > *zeros OR
172100000000     c                             ldcm > *zeros
172200000000     c* imposta causale
172300101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
172400000000     c                   EXSR      deccevsta
172500000000if  2c                   IF        werr <> '1'                                  *no errore
172600000000     c* imposta filiale
172700000000     c                   Z-ADD     llna          worg
172800000000     c                   EXSR      decorg
172900000000     c* imposta data
173000000000if  3c                   IF        ldti > *zeros
173100000000     c                   MOVEL     ldti          wdat                           *data arrivo
173200000000x   3c                   ELSE
173300000000     c                   MOVEL     ldcm          wdat                           *data consegna
173400000000e   3c                   ENDIF
173500000000     c                   EXSR      edtdat
173600000000     c* imposta ora
173700000000if  3c                   IF        lhti > *zeros
173800000000     c                   MOVEL     lhti          wora                           *ora arrivo
173900000000x   3c                   ELSE
174000000000     c                   MOVEL     0600          wora                           *ora imposta
174100000000e   3c                   ENDIF
174200000000     c                   EXSR      edtora
174300000000     c*
174400000000     c* controlla se l'evento è già stato inserito
174500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
174600000000do  3c     1             DO        200           j
174700000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
174800000000     c                   MOVE      wora          lavdevhev
174900000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
175000000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
175100000000     c                   MOVEL     'N'           newevb                         *già inserito
175200000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
175300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
175400000000     c                   LEAVE                                                  *uscita ciclo
175500000000e   4c                   ENDIF
175600000000e   3c                   ENDDO
175700000000     c*
175800000000     c* incrementa contatore eventi
175900000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
176000000000     c                   ADD       1             tote
176100000000     c* indice schiera
176200000000     c                   Z-ADD     tote          smi(tote)
176300000000     c* sequenza legame
176400000000     c                   Z-ADD     seql          smseql(tote)
176500000000     c* sequenza evento
176600000000     c                   Z-ADD     020           smseqe(tote)
176700000000     c* descrizione
176800030130     c                   MOVEL     wcdex         smdev(tote)
176900000000     c* data    -impostata sopra-
177000000000     c                   MOVEL     wdate         smdeve(tote)
177100000000     c* ora     -impostata sopra-
177200000000     c                   MOVEL     worae         smheve(tote)
177300000000     c* filiale -impostata sopra-
177400000000     c                   MOVEL     wodes         smfle(tote)
177500000000     c* causale -impostata sopra-
177600000000     c                   MOVEL     wcev          smcev(tote)
177700000000     c* data+ora evento
177800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
177900000000     c                   MOVE      wora          smdevhev(tote)
178000000000e   3c                   ENDIF
178100000000e   2c                   ENDIF
178200000000e   1c                   ENDIF
178300000000     c*
178400000000     c                   ENDSR
178500000000     c*--------------------------------------------------------------------------------------------*
178600000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
178700000000     c*--------------------------------------------------------------------------------------------*
178800140714     c     evbcon        BEGSR
178900000000     c*
179000000000     c* azzera variabili di lavoro
179100000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
179200000000     c****
179300000000     c* Consegna merce ->
179400000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
179500000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
179600000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
179700000000     c*   nè lo stato di consegna nè quello di consegna anomala.
179800000000     c****
179900000000     c* controlla SE e COME inserire l'evento
180000000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
180100060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
180200060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
180300060622     C                             :rpyEsito)
180400060622     C                   IF        rpyOpCode = 'FOUND'
180500041222     **?Consegna anomala, POD non visualizzabile.
180600060622     C                   IF        §7OPODImg = 'N'
180700041222     C                   EVAL      LDVO74 = 'N'
180800041222     C                   ENDIF
180900060622     c                   IF        §7oinc <> *blanks                            *ok per internet
181000060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
181100000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
181200000000x   3c                   ELSE                                                   *ha record FNEVB
181300000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
181400000000e   3c                   ENDIF
181500041222e   3c                   ENDIF
181600000000e   2c                   ENDIF
181700000000e   1c                   ENDIF
181800080131     c* imposta data
181900080131if  1c                   IF        ldcm > *zeros
182000080131     c                   MOVEL     ldcm          wdat
182100080131e   1c                   ENDIF
182200080131     c* imposta ora
182300080131if  1c                   IF        lhmc > *zeros
182400080131     c                   MOVEL     lhmc          wora
182500080131e   1c                   ENDIF
182600000000     c* imposta causale
182700000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
182800060622     c                   MOVEL     §7ocev        wcev                           *causale evento
182900080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
183000080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
183100080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
183200080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
183300080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
183400080131     c* ritiro merce) dalla bolla figlia.
183500080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
183600080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
183700080131     c                             AND s_tasNdc < *HIVAL
183800080131     c                   EVAL      kblAap = wAas
183900080131     c                   EVAL      kblLpp = wLnp
184000080131     c                   EVAL      kblNrp = wNrs
184100080131     c                   EVAL      kblNsp = wNsp
184200080131     c     ke2lbl        CHAIN     fnlbl02l
184300080131     c                   IF        %FOUND()
184400080131     c                   EVAL      kasAas = lblAan
184500080131     c                   EVAL      kasLnp = lblLpn
184600080131     c                   EVAL      kasNrs = lblNrn
184700080131     c                   EVAL      kasNsp = lblNsn
184800080131     C     keyTas        CHAIN     titas000      titas000Figlia
184900080131     C                   IF        %FOUND()
185000080131     C                   EVAL      wDat = titas000Figlia.tasDrt
185100080131     C                   EVAL      wOra = titas000Figlia.tasHrt
185200080131     C                   ELSE
185300080131     C     keyTas        CHAIN     titas010      titas010Figlia
185400080131     C                   IF        %FOUND()
185500080131     C                   EVAL      wDat = titas010Figlia.tasDrt
185600080131     C                   EVAL      wOra = titas010Figlia.tasHrt
185700080131     C                   ELSE
185800080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
185900080131     C                   IF        %FOUND()
186000080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
186100080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
186200080131     C                   ENDIF
186300080131     C                   ENDIF
186400080131     C                   ENDIF
186500080131     c                   ENDIF
186600080131     c                   ENDIF
186700000000x   1c                   ELSE                                                   *NO sost.Consegna
186800000000     c                   MOVEL     '704'         wcev                           *causale evento
186900000000e   1c                   ENDIF
187000080131     c* Editazione data e ora evento.
187100080131     c                   EXSR      edtdat
187200080131     c                   EXSR      edtora
187300000000     c* imposta filiale
187400000000     c                   Z-ADD     llna          worg
187500000000     c                   EXSR      decorg
187600000000     c* controlla se l'evento è già stato inserito
187700000000     c                   MOVEL     'S'           newevb                         *nuovo evento
187800000000do  1c     1             DO        200           j
187900000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
188000000000     c                   MOVE      wora          lavdevhev
188100000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
188200000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
188300000000     c                   MOVEL     'N'           newevb                         *già inserito
188400000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
188500000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
188600000000     c                   LEAVE                                                  *uscita ciclo
188700000000e   2c                   ENDIF
188800000000e   1c                   ENDDO
188900000000     c*
189000000000     c* incrementa contatore eventi
189100030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
189200030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
189300000000     c                   ADD       1             tote                           incrementa contatore
189400060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
189500060622     c                   MOVEL     §7odei        smdev(tote)
189600030203x   4c                   ELSE                                                   *evento telef.P.O.
189700060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
189800030203e   4c                   ENDIF
189900060622     c                   MOVEL     §7ocev        wcev                           *causale evento
190000000000     c                   EXSR      evbcon2                                      *imposta evento
190100030203x   3c                   ELSE                                                   *NO sost.Consegna
190200000000     c*
190300030203if  4c                   IF        ldcm > *zeros
190400030203if  5c                   IF        sostcon <> ' '                               *considera consegna
190500000000     c                   EXSR      deccevsta
190600030203if  6c                   IF        werr <> '1'                                  *no errore
190700000000     c                   ADD       1             tote                           incrementa contatore
190800030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
190900000000     c                   EXSR      evbcon2                                      *imposta evento
191000030203e   6c                   ENDIF
191100030203e   5c                   ENDIF
191200030203e   4c                   ENDIF
191300030203e   3c                   ENDIF
191400000000     c*
191500030203e   1c                   ENDIF
191600030203     c*
191700000000     c                   ENDSR
191800000000     c*--------------------------------------------------------------------------------------------*
191900000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
192000000000     c*--------------------------------------------------------------------------------------------*
192100000000     c     evbcon2       BEGSR
192200000000     c*
192300000000     c* indice schiera
192400000000     c                   Z-ADD     tote          smi(tote)
192500000000     c* sequenza legame
192600000000     c                   Z-ADD     seql          smseql(tote)
192700000000     c* sequenza evento
192800000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
192900000000     c* data    -impostata sopra-
193000000000     c                   MOVEL     wdate         smdeve(tote)
193100000000     c* ora     -impostata sopra-
193200000000     c                   MOVEL     worae         smheve(tote)
193300000000     c* filiale -impostata sopra-
193400000000     c                   MOVEL     wodes         smfle(tote)
193500000000     c* causale -impostata sopra-
193600000000     c                   MOVEL     wcev          smcev(tote)
193700000000     c* data+ora evento
193800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
193900000000     c                   MOVE      wora          smdevhev(tote)
194000000000     c*
194100000000     c                   ENDSR
194200000000     c*--------------------------------------------------------------------------------------------*
194300000000     c* memorizza i dati della lettera di anomalia
194400000000     c*--------------------------------------------------------------------------------------------*
194500000000     c     memdct        BEGSR
194600000000     c*
194700040906     C                   RESET                   FIDN12DS
194800040906     c                   Z-ADD     waas          I12AAS
194900040906     c                   Z-ADD     wlnp          I12LNP
195000040906     c                   Z-ADD     wnrs          I12NRS
195100040906     c                   Z-ADD     wnsp          I12NSP
195200040906     C                   CALL      'FIDN12R'
195300040906     C                   PARM                    FIDN12DS
195400000000     c* memorizza la prima lettera di anomalia valida
195500040906do  1c                   IF        O12Err = *BLANK
195600040906     C                   DO        O12NCA        K
195700040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
195800040906     C                   EVAL      DCTAAC = O12KeyAAC
195900040906     C                   EVAL      DCTFIL = O12KeyFil
196000040906     C                   EVAL      DCTNCA = O12KeyNCA
196100040906     c     keydct        CHAIN     fndct01l
196200040906do  1c                   IF        %FOUND
196300000000     c*
196400000000     c* controlla validità CA
196500000000     c                   MOVEL     '0'           werr                           *NO errore
196600000000     c                   EXSR      chkdct
196700000000if  2c                   IF        werr = '0'                                   *CA valida
196800000000     c*
196900000000     c* Imposta l'evento lettera di anomalia
197000000000     c                   EXSR      evbdct
197100000000     C                   LEAVE                                                  *uscita ciclo
197200000000e   2c                   ENDIF
197300000000     c*
197400040906e   1c                   ENDIF
197500040906e   1c                   ENDDO
197600040906e   1c                   ENDIF
197700000000     C*
197800000000     c                   ENDSR
197900000000     c*--------------------------------------------------------------------------------------------*
198000000000     c* controlla validità lettera di anomalia
198100000000     c*--------------------------------------------------------------------------------------------*
198200000000     c     chkdct        BEGSR
198300000000     c*
198400000000     c* reimposta variabili di lavoro
198500000000     c                   MOVEL     '0'           werr                           *NO errore
198600000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
198700000000     c*
198800000000     c* CA annullata, esclude
198900000000if  1c                   IF        dctatb <> ' '
199000000000     c                   MOVEL     '1'           werr                           *SI errore
199100000000e   1C                   ENDIF
199200000000     c*
199300000000     c* CA non confermata, esclude
199400000000if  1c                   IF        dctfpr = ' '
199500000000     c                   MOVEL     '1'           werr                           *SI errore
199600000000e   1C                   ENDIF
199700000000     c*
199800000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
199900000000if  1c                   IF        dctfpr = 'T' AND
200000000000     c                             dctfca >= 215 OR
200100000000     c                             dctfpr = 'P' AND
200200000000     c                             dctfca >= 400
200300000000x   1c                   ELSE
200400000000     c                   MOVEL     '1'           werr                           *SI errore
200500000000e   1C                   ENDIF
200600000000     c*
200700000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
200800000000if  1c                   IF        werr = '0'
200900000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
201000060622     C                   RESET                   tibs02ds
201100060622     C                   EVAL      t02Cod = 'CCH'
201200060622     C                   EVAL      t02Ke1 = dctcch
201300060622     C                   EXSR      getTntbe00f
201400060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
201500000000     c                   MOVEL     '1'           werr                           *SI errore
201600000000e   3C                   ENDIF
201700000000e   2C                   ENDIF
201800000000e   1C                   ENDIF
201900000000     c*
202000000000     c                   ENDSR
202100000000     c*--------------------------------------------------------------------------------------------*
202200000000     c* Imposta l'evento lettera di anomalia
202300000000     c*--------------------------------------------------------------------------------------------*
202400000000     c     evbdct        BEGSR
202500000000     C*
202600000000     c* imposta causale
202700000000if  1c                   IF        wfpr = 'T'
202800000000     c                   MOVEL     '709'         wcev                           *causale evento
202900000000x   1c                   ELSE
203000000000     c                   MOVEL     '710'         wcev                           *causale evento
203100000000e   1c                   ENDIF
203200000000     c                   EXSR      deccevsta                                    *decodifica evento
203300000000     c*
203400000000     c* imposta filiale
203500000000if  1c                   IF        werr <> '1'                                  *no errore
203600000000     c                   Z-ADD     dctfil        worg
203700000000     c                   EXSR      decorg
203800000000     c* imposta data
203900000000     c                   MOVEL     dctaac        wdat
204000000000     c                   MOVE      dctmgc        wdat
204100000000     c                   EXSR      edtdat
204200000000     c* imposta ora
204300000000     c                   MOVEL     1100          wora                           *FISSA
204400000000     c                   EXSR      edtora
204500000000     c*
204600000000     c* incrementa contatore eventi
204700000000     c                   ADD       1             tote
204800000000     c* indice schiera
204900000000     c                   Z-ADD     tote          smi(tote)
205000000000     c* sequenza legame
205100000000     c                   Z-ADD     seql          smseql(tote)
205200000000     c* sequenza evento
205300000000     c                   Z-ADD     999           smseqe(tote)
205400000000     c* descrizione -impostata sopra-
205500030130     c                   MOVEL     wcdex         smdev(tote)
205600000000     c* filiale -impostata sopra-
205700000000     c                   MOVEL     wodes         smfle(tote)
205800000000     c* data    -impostata sopra-
205900000000     c                   MOVEL     wdate         smdeve(tote)
206000000000     c* ora     -impostata sopra-
206100000000     c                   MOVEL     worae         smheve(tote)
206200000000     c* causale -impostata sopra-
206300000000     c                   MOVEL     wcev          smcev(tote)
206400000000     c* data+ora evento
206500000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
206600000000     c                   MOVE      wora          smdevhev(tote)
206700000000e   1c                   ENDIF
206800000000     c*
206900000000     c                   ENDSR
207000000000     c*--------------------------------------------------------------------------------------------*
207100000000     c* imposta i campi della DS di Output dal contrassegno
207200000000     c*--------------------------------------------------------------------------------------------*
207300000000     c     impdsocsb     BEGSR
207400000000     c*
207500000000     c* azzera le variabili di lavoro
207600000000     c*
207700000000     c****
207800000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
207900000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
208000000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
208100000000     c****
208200000000     c*
208300000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
208400090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
208500000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
208600000000x   1c                   ELSE                                                     *trovato
208700000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
208800000000e   1C                   ENDIF
208900000000     c*---
209000000000     c* memorizza il contrassegno della bolla di Input
209100000000     c*---
209200000000     c                   EVAL      waas = s_tasaas                              *bolla Input
209300000000     c                   EVAL      wlnp = s_taslnp
209400000000     c                   EVAL      wnrs = s_tasnrs
209500000000     c                   EVAL      wnsp = s_tasnsp
209600000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
209700000000     c                   EXSR      memcsb
209800000000     c*
209900000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
210000000000     c                   EXSR      elacsblbl
210100000000     c*
210200000000     c                   ENDSR
210300000000     c*--------------------------------------------------------------------------------------------*
210400000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
210500000000     c*--------------------------------------------------------------------------------------------*
210600000000     c     elacsblbl     BEGSR
210700000000     c*
210800000000     c* azzera le variabili di lavoro
210900000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
211000000000     c                   EVAL      deplpn = *zeros
211100000000     c                   EVAL      depnrn = *zeros
211200000000     c                   EVAL      depnsn = *zeros
211300000000     c*
211400000000     c* legge i legami delle figlie successive alla bolla di input
211500000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
211600000000     c                   EVAL      kbllpp =  s_taslnp
211700000000     c                   EVAL      kblnrp =  s_tasnrs
211800000000     c                   EVAL      kblnsp =  s_tasnsp
211900000000     c*
212000000000     c* ciclo fino a fine mamme
212100000000     c     ke2lbl        SETLL     fnlbl02l
212200000000     c     ke2lbl        READE     fnlbl02l                               99
212300000000do  1c                   DOW       NOT *in99
212400000000     c*
212500000000     c* ciclo fino a fine figlie della mamma
212600000000do  2c                   DOW       NOT *in99
212700000000     c*
212800000000     c* legge la bolla dal legame
212900000000     c                   EVAL      kasaas = lblaan                              *figlia
213000000000     c                   EVAL      kaslnp = lbllpn
213100000000     c                   EVAL      kasnrs = lblnrn
213200000000     c                   EVAL      kasnsp = lblnsn
213300000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
213400000000     c*---
213500000000     c* memorizza il contrassegno della bolla figlia legata
213600000000     c*---
213700000000if  4c                   IF        NOT *in99                                    *esistente
213800000000     c                   EVAL      waas = lblaan                                *figlia
213900000000     c                   EVAL      wlnp = lbllpn
214000000000     c                   EVAL      wnrs = lblnrn
214100000000     c                   EVAL      wnsp = lblnsn
214200000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
214300000000     c                   EXSR      memcsb
214400000000e   4c                   ENDIF
214500000000     c*
214600000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
214700000000     c                   EVAL      depaan = lblaan                              *ultima figlia
214800000000     c                   EVAL      deplpn = lbllpn
214900000000     c                   EVAL      depnrn = lblnrn
215000000000     c                   EVAL      depnsn = lblnsn
215100000000     c*
215200000000     c* lettura successiva legami
215300000000     c     ke2lbl        READE     fnlbl02l                               99
215400000000e   2c                   ENDDO                                                  *fine figlie mamma
215500000000     c*
215600000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
215700000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
215800000000     c                             kbllpp <> deplpn OR
215900000000     c                             kblnrp <> depnrn OR
216000000000     c                             kblnsp <> depnsn
216100000000     c* nuova chiave
216200000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
216300000000     c                   EVAL      kbllpp =  deplpn
216400000000     c                   EVAL      kblnrp =  depnrn
216500000000     c                   EVAL      kblnsp =  depnsn
216600000000     c* controlla figlie
216700000000     c     ke2lbl        SETLL     fnlbl02l
216800000000     c     ke2lbl        READE     fnlbl02l                               99
216900000000if  3c                   IF        *in99                                        *non ha figlie
217000000000     c                   LEAVE                                                  *esce dal ciclo
217100000000e   3c                   ENDIF
217200000000x   2c                   ELSE                                                   *sempre stessa mamma
217300000000     c                   LEAVE                                                  *esce dal ciclo
217400000000e   2c                   ENDIF
217500000000e   1c                   ENDDO                                                  *fine mamme
217600000000     c*
217700000000     c                   ENDSR
217800000000     c*--------------------------------------------------------------------------------------------*
217900000000     c* memorizza il contrassegno
218000000000     c*--------------------------------------------------------------------------------------------*
218100000000     c     memcsb        BEGSR
218200000000     c*
218300000000     c* legge il contrassegno, se avuto
218400000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
218500000000     c                   Z-ADD     waas          ksbaas
218600000000     c                   Z-ADD     wlnp          ksblnp
218700000000     c                   Z-ADD     wnrs          ksbnrs
218800000000     c                   Z-ADD     wnsp          ksbnsp
218900000000     c     keycsb        CHAIN     tncsb03l                           99
219000000000if  2c                   IF        NOT *in99                                    *esistente
219100000000     c* memorizza i dati
219200000000     c                   MOVEL     'S'           cao74                          *esiste C/A
219300000000     c*
219400000000     c                   Z-ADD     csbcas        caimpo74                       *importo
219500000000     c*
219600000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
219700010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
219800010820     c                             caimpo74 > *zeros
219900010820     c                   EVAL      cadivo74 = 'ITL'
220000010820e   3c                   ENDIF
220100100119     ** Visualizzo la descrizione del tipo incasso contrassegno solo se il
220200100119     ** contrassegno è stato incassato.
220300100121     C                   EVAL      cainco74 =
220400100121     C                                       GetDescrizioneIncassoContrassegno()
220500000000     c*
220600000000     c                   MOVEL     *blanks       castao74                       *stato
220700060622     C                   EVALR     tblKey = %CHAR(csbSta)
220800060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
220900060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
221000060622     C                             :rpyOpCode:rpyEsito)
221100060622     C                   IF        rpyOpCode = 'FOUND'
221200060622     c                   EVAL      castao74 = §4SDei
221300000000e   3c                   ENDIF
221400100119     c
221500000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
221600011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
221700060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
221800000000x   4c                   ELSE
221900000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
222000060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
222100000000x   5c                   ELSE
222200060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
222300000000e   5c                   ENDIF
222400000000e   4c                   ENDIF
222500000000e   3c                   ENDIF
222600000000     c*
222700000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
222800000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
222900000000if  4c                   IF        csbtpa = 'B'
223000060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
223100000000e   4c                   ENDIF
223200000000if  4c                   IF        csbtpa = 'C'
223300060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
223400000000e   4c                   ENDIF
223500000000x   3c                   ELSE                                                   *incasso Bartolini
223600000000if  4c                   IF        csbfpc = *blanks
223700110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
223800000000e   4c                   ENDIF
223900000000if  4c                   IF        csbfpc = '2'
224000060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
224100000000e   4c                   ENDIF
224200000000e   3c                   ENDIF
224300000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
224400000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
224500000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
224600000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
224700000000     c                             csbfpc = *blanks                              assegno di traenza
224800060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
224900000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
225000060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
225100000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
225200060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
225300000000e   4c                   ENDIF
225400000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
225500060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
225600060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
225700000000e   3c                   ENDIF
225800000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
225900000000     c                   Z-ADD     *zeros        caabio74                       *abi
226000000000     c                   Z-ADD     *zeros        cacabo74                       *cab
226100000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
226200000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
226300000000     c                   MOVEL     csbddp        wdat
226400000000     c                   EXSR      edtdat
226500000000     c                   EVAL      cadato74 = wdate
226600000000     C*
226700000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
226800000000     c                   Z-ADD     csbabi        caabio74
226900000000     c                   Z-ADD     csbcab        cacabo74
227000000000     c                   MOVEL     csbnra        canumo74
227100120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
227200120321     c                             : caCabO74)
227300000000x   4c                   ELSE                                                   *incasso Bartolini
227400000000     c                   MOVEL     csbndp        canumo74
227500000000e   4c                   ENDIF
227600000000e   3c                   ENDIF
227700110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
227800110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
227900110307     C                   IF        IsContrassegnoCompensato()
228000110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
228100110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
228200110304     C                   CLEAR                   caabio74
228300110304     C                   CLEAR                   cacabo74
228400110304     C                   CLEAR                   canumo74
228500110304     C                   ENDIF
228600110304     C*
228700000000e   2c                   ENDIF
228800000000e   1c                   ENDIF
228900000000     c*
229000000000     c                   ENDSR
229100000000     c*--------------------------------------------------------------------------------------------*
229200000000     c* imposta i campi della DS di Output dalla giacenza
229300000000     c*--------------------------------------------------------------------------------------------*
229400000000     c     impdsogcp     BEGSR
229500000000     c*
229600000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
229700000000     c                   EVAL      waas = s_tasaas                              *bolla Input
229800000000     c                   EVAL      wlnp = s_taslnp
229900000000     c                   EVAL      wnrs = s_tasnrs
230000000000     c                   EVAL      wnsp = s_tasnsp
230100000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
230200000000     c                   EXSR      memgcp
230300000000     c*
230400000000     c                   ENDSR
230500000000     c*--------------------------------------------------------------------------------------------*
230600000000     c* memorizza la giacenza
230700000000     c*--------------------------------------------------------------------------------------------*
230800000000     c     memgcp        BEGSR
230900000000     c*
231000000000     c* imposta le variabili di lavoro
231100000000     c                   CLEAR                   i                              *indice
231200000000     c                   CLEAR                   ii
231300000000     c                   CLEAR                   smdmc                          *note fase -20-
231400000000     c                   CLEAR                   smdmcR
231500000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
231600000000     c                   MOVEL     'N'           gco74                          *flag di controllo
231700000000     c                   MOVEL     'N'           gcfdio74
231800000000     c                   MOVEL     'N'           gcmito74
231900000000     c*
232000000000     c* legge la giacenza, se avuta
232100000000     c                   Z-ADD     waas          kcpaas
232200000000     c                   Z-ADD     wlnp          kcplnp
232300000000     c                   Z-ADD     wnrs          kcpnrs
232400000000     c                   Z-ADD     wnsp          kcpnsp
232500000000     c                   Z-ADD     *zeros        kcpfrg
232600050309     c     keygcp        CHAIN     tigcp51l                           99
232700050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
232800050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
232900000000if  2c                   IF        NOT *in99        AND                         *esistente
233000000000     c                             gcpatb = *blanks AND                         *no annullata
233100050415     c                             gcptcm <> 'Z'    AND
233200050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
233300050419     C
233400050419     C                   EVAL      gcRiapO74 = gcpRiap
233500050418     **?Descrizione della fase.
233600050418     C                   SELECT
233700050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
233800060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
233900050418     C                   WHEN      gcpFas = 20
234000060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
234100050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
234200060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
234300050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
234400060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
234500050418     C                   ENDSL
234600000000     c* memorizza i dati dalle NOTE GIACENZE
234700000000     c                   Z-ADD     *zeros        i
234800000000     c                   Z-ADD     *zeros        ii
234900000000     c                   Z-ADD     gcpagc        knpagc
235000000000     c                   Z-ADD     gcpfgc        knpfgc
235100000000     c                   Z-ADD     gcpngc        knpngc
235200000000     c                   Z-ADD     gcpfrg        knpfrg
235300050309     c     keygnp        SETLL     tignp01l
235400050309     c     keygnp        READE     tignp01l                               98
235500000000do  3c                   DOW       NOT *in98
235600050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
235700000000     c* fase -10-
235800000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
235900000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
236000000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
236100000000e   5c                   ENDIF
236200000000     c* fase -20-
236300000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
236400000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
236500051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
236600000000     c                   ADD       1             i
236700000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
236800000000e   6c                   ENDIF
236900000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
237000051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
237100050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
237200050405     c                   CLEAR                   ii
237300050405     c                   CLEAR                   smdmcR
237400050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
237500050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
237600050919     C                   IF        NOT %ERROR
237700140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
237800050919     C                   ENDIF
237900050405     C                   ELSE
238000000000     c                   ADD       1             ii
238100000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
238200050405     C                   ENDIF
238300000000e   6c                   ENDIF
238400000000e   5c                   ENDIF
238500000000e   4c                   ENDIF
238600050309     c     keygnp        READE     tignp01l                               98
238700000000e   3c                   ENDDO
238800000000     c*
238900000000     c* memorizza i dati dalle GIACENZE
239000000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
239100050404     c* Disposizione da immettere.
239200051110     c                   IF        gcpfas = 20
239300051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
239400051110     C                   EXSR      chkTivpi
239500051110e   3c                   ENDIF
239600000000     c*
239700000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
239800000000     c*
239900000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
240000000000     c*
240100060103     c                   Z-ADD     gcpagc        dsannB
240200060103     c                   Z-ADD     gcpmgc        dsmgsB
240300060103     c                   Z-ADD     dsdatB        wdat
240400000000if  3c                   IF        wdat > *zeros
240500000000     c                   EXSR      edtdat
240600000000     c                   EVAL      gcdago74 = wdate                             *data apertura
240700000000e   3C                   ENDIF
240800060104     c*
240900060104     c                   Z-ADD     gcpdur        wdat
241000060104     c                   IF        wdat > *zeros
241100060104     c                   EXSR      edtdat
241200060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
241300060104     C                   ENDIF
241400000000     c*
241500000000if  3c                   IF        gcpcmc <> *blanks
241600000000     c                   MOVEL     gcpcmc        wcev
241700000000     c                   EXSR      deccevgia
241800030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
241900000000e   3C                   ENDIF
242000000000     c*
242100000000     c                   MOVEL     *blanks       gcdcgo74
242200000000if  3c                   IF        gcpdcg > *zeros
242300000000     c                   Z-ADD     gcpdcg        wdat
242400000000     c                   EXSR      edtdat
242500000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
242600000000e   3c                   ENDIF
242700000000     c*---
242800000000     c* Disposizioni già immesse
242900000000     c*---
243000000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
243100000000     c*
243200000000if  4c                   IF        gcpddm > *zeros
243300000000     c                   Z-ADD     gcpddm        wdat
243400000000     c                   EXSR      edtdat
243500000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
243600000000e   4C                   ENDIF
243700000000     c*
243800000000if  4c                   IF        gcpdis <> *blanks
243900060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
244000060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
244100060626     C                             :rpyOpCode:rpyEsito)
244200060622     C                   EVAL      gcdiso74 = §2DDes
244300000000e   4C                   ENDIF
244400000000     c*
244500000000if  4c                   IF        gcpasg <> *blanks
244600000000if  5c                   IF        gcpasg = 'S'
244700060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
244800000000x   5c                   ELSE
244900060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
245000000000e   5C                   ENDIF
245100000000     c*
245200000000if  5c                   IF        gcppsg = 'M'
245300060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
245400000000e   5C                   ENDIF
245500000000if  5c                   IF        gcppsg = 'D'
245600060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
245700000000e   5C                   ENDIF
245800000000e   4C                   ENDIF
245900000000     c*
246000000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
246100000000     c                   MOVEL     gcpvcs        gcvcso74
246200000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
246300000000     c                   Z-ADD     gcpcas        gccaso74
246400000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
246500000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
246600000000e   5C                   ENDIF
246700000000e   4C                   ENDIF
246800050404     C                   IF        smdmc(1) <> *BLANK
246900050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
247000050404     C                             smdmc(3) + smdmc(4)
247100051014     C                   EVAL      gcdmc5O74 = smdmc(5)
247200050404     C                   ELSE
247300050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
247400050404     C                             smdmcr(3) + smdmcr(4)
247500051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
247600050404e   3C                   ENDIF
247700000000e   3C                   ENDIF
247800000000     c*---
247900000000     c* Disposizioni da immettere
248000000000     c*---
248100010719     c* se le disposizioni sono da immettere controlla che:
248200010719     c* - il cliente è il mittente della giacenza
248300000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
248400040726if  4C                   IF        keygiac <> *blanks
248500040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
248600040726x   4c                   ELSE
248700090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
248800090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
248900090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
249000090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
249100090914     C                             )
249200040726e   4C                   ENDIF
249300000000e   3C                   ENDIF
249400050404     C* Controllo esistenza storia giacenza.
249500050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
249600050404     C                   DO        *HIVAL
249700050404     C     K04GCP51      READE     TIGCP51L
249800050404     C                   IF        %EOF
249900050404     C                   LEAVE
250000050404     C                   ENDIF
250100050404     C                   IF        GCPATB = *BLANK
250200050404     C                   EVAL      StoGiaO74 = 'S'
250300050404     C                   LEAVE
250400050404     C                   ENDIF
250500050404     C                   ENDDO
250600050404     c*
250700000000e   2c                   ENDIF
250800000000     c*
250900000000     c                   ENDSR
251000000000     c*--------------------------------------------------------------------------------------------*
251100000000     c* imposta i campi della DS di Output dalle note
251200000000     c*--------------------------------------------------------------------------------------------*
251300000000     c     impdsonot     BEGSR
251400000000     c*
251500000000     c* azzera le variabili di lavoro
251600000000     c                   CLEAR                   i
251700000000     c                   CLEAR                   mdcre                          consegna richiesta
251800000000     c                   CLEAR                   mhcre
251900030210     c                   CLEAR                   mtcre
252000000000     c                   CLEAR                   mftce                          consegna particolare
252100000000     c                   CLEAR                   wftc
252200000000     c                   CLEAR                   d1wdftc
252300000000     c                   CLEAR                   d2wdftc
252400000000     c                   CLEAR                   mgcxe                          turni chiusura
252500000000     c                   CLEAR                   wgcx
252600000000     c                   CLEAR                   d1wdgcx
252700000000     c                   CLEAR                   d2wdgcx
252800010927     c                   CLEAR                   mffd                           fermo deposito
252900010927     c                   CLEAR                   mmncB                          *motivi NON consegna
253000010927     c                   CLEAR                   mmncC
253100010927     c                   CLEAR                   mmncG
253200010927     c                   CLEAR                   mmncH
253300010927     c                   CLEAR                   mmncN
253400010927     c                   CLEAR                   mmncO
253500050701     c                   CLEAR                   mmnc8
253600050701     c                   CLEAR                   mmnc9
253700050701     c                   CLEAR                   a230
253800000000     c                   CLEAR                   we
253900030206     c                   CLEAR                   mtel                           n° tel destinatario
254000030206     c                   CLEAR                   mban                           bancali
254100030207     c                   CLEAR                   mscr                           supermercati
254200030210     c                   CLEAR                   msdace
254300030210     c                   CLEAR                   msorce
254400030210     c                   CLEAR                   msdcre
254500030210     c                   CLEAR                   mshcre
254600030210     c                   CLEAR                   mstcre
254700030210     c                   CLEAR                   msnom
254800030210     c                   CLEAR                   mscrT
254900030210     c                   CLEAR                   wmscrT
255000030210     c                   CLEAR                   s
255100030207     c
255200140619     c*imposta dati data consegna richiesta
255300140716     c                   if        f_tasdcr > 0
255400140716     c                   MOVEL     f_tasdcr      wdat
255500140619     c                   EXSR      edtdat
255600140619     c                   eval      DTCONRCO74  =  wdate
255700140819if  3c                   IF        f_tastcr = ' '
255800140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
255900140819e   3c                   ENDIF
256000140819     c                   endif
256100140716     c                   if        f_tashcr > 0
256200140716     c                   MOVEL     f_tashcr      wora
256300140619     c                   EXSR      edtora
256400140619     c                   eval      ORACONRO74  =  worae
256500140619     c                   endif
256600140716     c                   eval      TPDATCRO74  =  f_tastcr
256700140619      *reperimento descrizione tipo consegna richiesta
256800140716if  3c                   IF        f_tastcr = 'P'
256900140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
257000140619e   3c                   ENDIF
257100140716if  3c                   IF        f_tastcr = 'D'
257200140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
257300140619e   3c                   ENDIF
257400000000     c*---
257500000000     c* memorizza le "note" che sono presenti nel record bolla
257600000000     c*---
257700000000     c* Consegna richiesta
257800140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
257900140619     c*m                           s_tashcr = *zeros
258000140619x   1c*m                 ELSE                                                   *SI cons.particolare
258100140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
258200140619     c*m                 MOVEL     s_tasdcr      wdat
258300140619     c*m                 EXSR      edtdat                                       *edita data
258400140619     c*m                 MOVEL     wdate         mdcre
258500140619e   2c*m                 ENDIF
258600140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
258700140619     c*m                 MOVEL     s_tashcr      wora
258800140619     c*m                 EXSR      edtora                                       *edita ora
258900140619     c*m                 MOVEL     worae         mhcre
259000140619e   2c*m                 ENDIF
259100140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
259200140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
259300140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
259400140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
259500140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
259600140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
259700140619e   3c*m                 ENDIF
259800140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
259900140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
260000140619e   3c*m                 ENDIF
260100140619if  3c*m                 IF        s_tastcr = ' '                               *il
260200140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
260300140619e   3c*m                 ENDIF
260400140619e   2c*m                 ENDIF
260500140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
260600140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
260700140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
260800140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
260900140619e   3c*m                 ENDIF
261000140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
261100140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
261200140619e   3c*m                 ENDIF
261300140619if  3c*m                 IF        s_tastcr = ' '                               *il
261400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
261500140619e   3c*m                 ENDIF
261600140619e   2c*m                 ENDIF
261700140619     c*m
261800140619if  2c*m                 IF        i < 10                                       *se c'è spazio
261900140619     c*m                 ADD       1             i                              *memorizza nota
262000140619     c*m                 EVAL      smnot(i) =
262100140619     c*m                           %TRIM(mtcre) +
262200140619     c*m                           ' '          +
262300140619     c*m                           %TRIM(mdcre) +
262400140619     c*m                           ' '          +
262500140619     c*m                           %TRIM(mhcre)
262600140619e   2c*m                 ENDIF
262700140619e   1c*m                 ENDIF
262800000000     c*
262900000000     c* Particolarità consegna -uno/due-
263000140716if  1c                   IF        f_tasftc <> *blanks OR
263100140716     c                             f_tastc2 <> *blanks
263200140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
263300000000     c                   EXSR      decftc
263400000000     c                   MOVEL     wdftc         d1wdftc
263500140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
263600000000     c                   EXSR      decftc
263700000000     c                   MOVEL     wdftc         d2wdftc
263800000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
263900000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
264000060612     c                   EVAL      we = ' ' +
264100060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
264200000000x   2c                   ELSE
264300000000     c                   EVAL      we = *blanks
264400000000e   2c                   ENDIF
264500060612     c                   EVAL      mftce =
264600060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
264700060612     c                             %TRIM(d1wdftc)             +
264800060612     c                             we                         +
264900060612     c                             %TRIM(d2wdftc)
265000000000     c*
265100010927if  2c                   IF        i < 10                                       *se c'è spazio
265200000000     c                   ADD       1             i                              *memorizza nota
265300000000     c                   EVAL      smnot(i) = mftce
265400010927e   2c                   ENDIF
265500010927e   1c                   ENDIF
265600000000     c*
265700000000     c* Chiusura per turno
265800140716if  1c                   IF        f_tasgc1 <> *blanks OR
265900140716     c                             f_tasgc2 <> *blanks
266000140716if  2c                   IF        f_tasgc1 <> *blanks
266100140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
266200000000     c                   EXSR      decgcx
266300000000     c                   MOVEL     wdgcx         d1wdgcx
266400000000e   2c                   ENDIF
266500140716if  2c                   IF        f_tasgc2 <> *blanks
266600140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
266700000000     c                   EXSR      decgcx
266800000000     c                   MOVEL     wdgcx         d2wdgcx
266900000000e   2c                   ENDIF
267000000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
267100000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
267200060612     c                   EVAL      we = ' ' +
267300060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
267400000000x   2c                   ELSE
267500000000     c                   EVAL      we = *blanks
267600000000e   2c                   ENDIF
267700060612     c                   EVAL      mgcxe =
267800060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
267900060612     c                             %TRIM(d1wdgcx)       +
268000060612     c                             we                   +
268100060612     c                             %TRIM(d2wdgcx)
268200000000     c*
268300010927if  2c                   IF        i < 10                                       *se c'è spazio
268400000000     c                   ADD       1             i                              *memorizza nota
268500000000     c                   EVAL      smnot(i) = mgcxe
268600010927e   2c                   ENDIF
268700010927e   1c                   ENDIF
268800000000     c*
268900000000     c* Fermo deposito
269000140716if  1c                   IF        f_tasffd = 'S'
269100060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
269200000000     c*
269300010927if  2c                   IF        i < 10                                       *se c'è spazio
269400010927     c                   ADD       1             i                              *memorizza nota
269500010927     c                   EVAL      smnot(i) = mffd
269600010927e   2c                   ENDIF
269700010927e   1c                   ENDIF
269800030206     c*---
269900030206     c* memorizza le "note" che NON sono presenti nel record bolla
270000030206     c* ma sul file estensione "anagrafiche"
270100030206     c*---
270200030206     c* N° telefono destinatario
270300140716     c                   Z-ADD     f_tasaas      kaaaas
270400140716     c                   Z-ADD     f_taslnp      kaalnp
270500140716     c                   Z-ADD     f_tasnrs      kaanrs
270600140716     c                   Z-ADD     f_tasnsp      kaansp
270700030206     c                   MOVEL     'O'           kaatrc
270800030206     c     keytaa        CHAIN     titaa30c                           98
270900030206if  1c                   IF        NOT *in98
271000040330     C                   EVAL      MitOrigO74 = TAARSC
271100030206e   1c                   ENDIF
271200030206     c*---
271300030206     c* memorizza le "note" che NON sono presenti nel record bolla
271400030206     c* ma sul file estensione testata boolle
271500030206     c*---
271600030206     c* Bancali
271700140716     c                   Z-ADD     f_tasaas      kr5aas
271800140716     c                   Z-ADD     f_taslnp      kr5lnp
271900140716     c                   Z-ADD     f_tasnrs      kr5nrs
272000140716     c                   Z-ADD     f_tasnsp      kr5nsp
272100030206     c                   MOVEL     'BAN'         kr5trd
272200040531     c     keyar5        CHAIN     fiar531c                           98
272300030206if  1c                   IF        NOT *in98
272400030206     c                   MOVEL     ar5uni        dar5ban
272500060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
272600060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
272700030206if  2c                   IF        §ar5rb1 > *zeros OR
272800030206     c                             §ar5rb2 > *zeros
272900060613     c                   EVAL      mban = %trim(mban) +
273000060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
273100060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
273200030206e   2c                   ENDIF
273300030206     c*
273400030206if  2c                   IF        i < 10                                       *se c'è spazio
273500030206     c                   ADD       1             i                              *memorizza nota
273600030206     c                   EVAL      smnot(i) = mban
273700030206e   2c                   ENDIF
273800030206e   1c                   ENDIF
273900040330
274000040330     **?Reperisco referente
274100030206     c* Note supermercati
274200140716if  1c     f_tasftc      IFEQ      'S'
274300140716     c     f_tastc2      OREQ      'S'
274400030210     c                   CLEAR                   s                              *indice
274500140716     c                   Z-ADD     f_tasaas      kr5aas
274600140716     c                   Z-ADD     f_taslnp      kr5lnp
274700140716     c                   Z-ADD     f_tasnrs      kr5nrs
274800140716     c                   Z-ADD     f_tasnsp      kr5nsp
274900030206     c                   MOVEL     'SCR'         kr5trd
275000040531     c     keyar5        SETLL     fiar531c
275100040531     c     keyar5        READE     fiar531c                               98
275200030210do  2c                   DOW       NOT *in98
275300030206     c                   MOVEL     ar5uni        dar5scr
275400030207     c* data nota
275500030210if  3c                   IF        ar5dac > *zeros
275600030207     c                   MOVEL     ar5dac        wdat
275700030207     c                   EXSR      edtdat                                       *edita data
275800030210     c                   MOVEL     wdate         msdace
275900030210e   3c                   ENDIF
276000030207     c* ora nota
276100030210if  3c                   IF        ar5orc > *zeros
276200030207     c                   MOVEL     ar5orc        wora
276300030207     c                   EXSR      edtora                                       *edita ora
276400030210     c                   MOVEL     worae         msorce
276500030210e   3c                   ENDIF
276600030207     c* Consegna richiesta
276700140708if  3c                   IF        §ar5dcr = *zeros AND
276800140708     c                             §ar5hcr = *zeros
276900140708x   3c                   ELSE
277000140708if  4c                   IF        §ar5dcr > *zeros
277100140708     c                   MOVEL     §ar5dcr       wdat
277200140708     c                   EXSR      edtdat                                       *edita data
277300140708     c                   MOVEL     wdate         msdcre
277400140708e   4c                   ENDIF
277500140708if  4c                   IF        §ar5hcr > *zeros
277600140708     c                   MOVEL     §ar5hcr       wora
277700140708     c                   EXSR      edtora                                       *edita ora
277800140708     c                   MOVEL     worae         mshcre
277900140708e   4c                   ENDIF
278000140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
278100140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
278200140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
278300140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
278400140708if  5c                   IF        §ar5tcr = 'P'                                *prima
278500140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
278600140708e   5c                   ENDIF
278700140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
278800140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
278900140708e   5c                   ENDIF
279000140708if  5c                   IF        §ar5tcr = ' '                                *il
279100140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
279200140708e   5c                   ENDIF
279300140708e   4c                   ENDIF
279400140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
279500140708     c                             §ar5dcr = *zeros                             *NO data richiesta
279600140708if  5c                   IF        §ar5tcr = 'P'                                *prima
279700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
279800140708e   5c                   ENDIF
279900140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
280000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
280100140708e   5c                   ENDIF
280200140708if  5c                   IF        §ar5tcr = ' '                                *il
280300140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
280400140708e   5c                   ENDIF
280500140708e   4c                   ENDIF
280600030210e   3c                   ENDIF
280700030210     c* Riferimento
280800030210if  3c                   IF        §ar5nom <>  *blanks
280900060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
281000060613     c                             + ' ' + §ar5nom
281100030210e   3c                   ENDIF
281200030210     c*
281300030210if  3c                   IF        s < 5                                        *MAX 5 note
281400030210     c                   ADD       1             s
281500030210     c                   EVAL      mscrT(s) = 'Il ' +
281600030210     c                             %trim(msdace) +                              *data nota
281700030210     c                             ' '    +
281800030210     c                             %trim(msorce) +                              *ora nota
281900030210     c                             ' '    +
282000140708     c                             %trim(mstcre) +                              *tipo consegna
282100140708     c                             ' '    +
282200140708     c                             %trim(msdcre) +                              *data consegna
282300140708     c                             ' '    +
282400140708     c                             %trim(mshcre) +                              *ora consegna
282500140708     c                             ' '    +
282600030210     c                             %trim(msnom)  +                              *parlato con
282700030210     c                             ' '    +
282800030210     c                             %trim(§ar5tel) +                             *telefono
282900030210     c                             ' '    +
283000030210     c                             %trim(§ar5mot)                               *motivo
283100030210e   2c                   ENDIF
283200030210     c*
283300040531     c     keyar5        READE     fiar531c                               98
283400030206e   2c                   ENDDO
283500030210e   1c                   ENDIF
283600030210     c* NOTA completa
283700030210if  1c                   IF        mscrT(1) <> *blanks
283800030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
283900030211     c                                      ' ' +
284000030211     C                                      %trim(mscrT(2)) +
284100030211     c                                      ' ' +
284200030211     C                                      %trim(mscrT(3)) +
284300030211     c                                      ' ' +
284400030211     C                                      %trim(mscrT(4)) +
284500030211     c                                      ' ' +
284600030211     C                                      %trim(mscrT(5))
284700030210     c                   Z-ADD     *zeros        s
284800030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
284900030210     c                   ADD       1             i
285000030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
285100030210     c                   EVAL      s = s + 100
285200030210e   2c                   ENDDO
285300030210     c*
285400030210e   1c                   ENDIF
285500010927     c*---
285600010927     c* memorizza le "note" che NON sono presenti nel record bolla
285700030206     c* ma sul file estensione "riferimenti"
285800010927     c*---
285900010927     c* motivi della NON consegna
286000010927     c                   CLEAR                   tita4000
286100010927     c                   CLEAR                   tita4p00
286200140716     c                   Z-ADD     f_tasaas      ka4aas
286300140716     c                   Z-ADD     f_taslnp      ka4lnp
286400140716     c                   Z-ADD     f_tasnrs      ka4nrs
286500140716     c                   Z-ADD     f_tasnsp      ka4nsp
286600010927     c     ke2ta4        SETLL     tita430c
286700010927     c     ke2ta4        READE     tita430c                               98
286800010927do  1c                   DOW       NOT *in98
286900140708     c                   if        ta4trc = '8' OR ta4trc = '9'
287000050701     c*
287100050701if  3c                   IF        ta4trc = '8'
287200050701     c                   EVAL      mmnc8 = ta4not
287300050701e   3c                   ENDIF
287400050701if  3c                   IF        ta4trc = '9'
287500050701     c                   EVAL      mmnc9 = ta4not
287600050701e   3c                   ENDIF
287700050701     c*
287800010927e   2c                   ENDIF
287900010925     c*
288000010926     c     ke2ta4        READE     tita430c                               98
288100010926e   1c                   ENDDO
288200050701     C*
288300140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
288400050701     C*
288500050701     C                   IF        mmnc8 <> *BLANK
288600050701     C                   EVAL      a230 = %TRIML(mmnc8)
288700050701     C                   ENDIF
288800050701     C                   IF        mmnc9 <> *BLANK
288900050701     C                   IF        a230 <> *BLANK
289000050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
289100050701     C                   ELSE
289200050701     C                   EVAL      a230 = %TRIML(mmnc9)
289300050701     C                   ENDIF
289400050701     C                   ENDIF
289500010927     C*
289600050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
289700050701     C*
289800010927if  2c                   IF        i < 10                                       *se c'è spazio
289900010927     c                   ADD       1             i                              *memorizza nota
290000010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
290100010927e   2c                   ENDIF
290200010927     C*
290300010927if  2c                   IF        i < 10                                       *se c'è spazio
290400010927if  3c                   IF        n3 > 100
290500010927     c                   ADD       1             i                              *memorizza nota
290600010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
290700010927e   3c                   ENDIF
290800010927e   2c                   ENDIF
290900010927     C*
291000010927if  2c                   IF        i < 10                                       *se c'è spazio
291100010927if  3c                   IF        n3 > 200
291200010927     c                   ADD       1             i                              *memorizza nota
291300010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
291400010927e   3c                   ENDIF
291500010927e   2C                   ENDIF
291600010927e   1c                   ENDIF
291700010925     c*
291800000000     c* imposta contatore note da visualizzare nella DS di Output
291900000000     c                   EVAL      cntnoto74 = i
292000000000     c*
292100000000     c* imposta le note nella DS di Output
292200000000do  1c     1             DO        10            j
292300000000     c                   EVAL      nota(j) = smnot(j)
292400000000e   1c                   ENDDO
292500000000     c*
292600000000     c                   ENDSR
292700010716     c*--------------------------------------------------------------------------------------------*
292800010716     c* imposta i campi della DS di Output dalla firma DPD
292900010716     c*--------------------------------------------------------------------------------------------*
293000010716     c     impdsoDPD     BEGSR
293100010717     c*
293200010717     c* reimposta variabili di lavoro
293300010717     c                   RESET                   $AbFirma
293400010717     c                   CLEAR                   depute                         *deposito utente
293500010717     c                   CLEAR                   deppwd                         *deposito password
293600010717     c*---
293700010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
293800010717     c*---
293900010717     c                   Z-ADD     s_taslna      worg
294000010717     c                   EXSR      decorg
294100020702     c                   IF        wodpd = 'DPD'
294200020702     c                   MOVEL     'S'           dpdo74
294300020702     c                   ELSE
294400020702     c                   MOVEL     *blanks       dpdo74
294500020702     c                   ENDIF
294600010717     c*
294700010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
294800060627     c* Riferimento link Lettera di vettura
294900060627     c* Riferimento link Firma
295000010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
295100010717     c                   CLEAR                   tita4000
295200010717     c                   CLEAR                   tita4p00
295300060627     c                   CLEAR                   dsbl4i
295400010717     c                   Z-ADD     s_tasaas      ka4aas
295500010717     c                   Z-ADD     s_taslnp      ka4lnp
295600010717     c                   Z-ADD     s_tasnrs      ka4nrs
295700010717     c                   Z-ADD     s_tasnsp      ka4nsp
295800060627     c                   MOVEL     'I'           ka4trc
295900060627     c     keyta4        CHAIN     tita430c                           98
296000010717if  2c                   IF        NOT *in98
296100060627     c                   MOVEL     ta4not        dsbl4i
296200060627     c                   EVAL      DPDRmnO74 = §b4ipn
296300060627     c                   EVAL      DPDFnpO74 = §b4ipn
296400010717e   2c                   ENDIF
296500010717     c*
296600010717     c* cerca abilitazione firma DPD per questo segnacollo
296700010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
296800010717     c                   EXSR      RepAbilFirma
296900010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
297000010717     c*---
297100010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
297200010717     c*---
297300010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
297400010717     c* dati
297500010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
297600010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
297700010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
297800010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
297900010717     c*
298000010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
298100010717     c                   Z-ADD     s_tasaas      kvbaas
298200010717     c                   Z-ADD     s_taslnp      kvblnp
298300010717     c                   Z-ADD     s_tasnrs      kvbnrs
298400010717     c                   Z-ADD     s_tasnsp      kvbnsp
298500010717     c                   Z-ADD     99999999      kvbdev
298600010717     c                   Z-ADD     9999          kvbhev
298700010717     c     ke2evb        SETGT     fnevb01l
298800010717     c     keyevb        READPE    fnevb01l                               98
298900010717do  4c                   DOW       NOT *in98
299000101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
299100010720     c*
299200060629     c                   EVAL      devB01 = evbNot
299300060628     c                   IF        §NotDep = *BLANK
299400060628     c                   RESET                   DPDFirO74
299500060629     c                   LEAVE
299600060628     c                   ENDIF
299700060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
299800060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
299900080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
300000010720     c                   Z-ADD     *zeros        g08dat
300100010720     c                   MOVEL     '3'           g08err
300200010720     c                   CALL      'XSRDA8'
300300010720     c                   PARM                    wlbda8
300400010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
300500010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
300600010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
300700010720     c*
300800010717     c                   LEAVE                                                  *uscita ciclo
300900010717e   5c                   ENDIF
301000010717     c     keyevb        READPE    fnevb01l                               98
301100010717e   4c                   ENDDO
301200010717     c*
301300010717e   3c                   ENDIF
301400010717e   2c                   ENDIF
301500010716e   1c                   ENDIF
301600010716     c*
301700010716     c                   ENDSR
301800140604     c*--------------------------------------------------------------------------------------------*
301900140604     c* imposta i campi da FNARB se TASDCM = 0
302000140604     c*--------------------------------------------------------------------------------------------*
302100140604     c     impdsoARB     BEGSR
302200150403     c                   clear                   orarich           4 0
302300150611     c                   clear                   wdee
302400150410     c* memorizzo se utilizzo la data prevista consegna
302500150410     c                   clear                   usaprev           1
302600140604     c*
302700140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
302800140604     c                   EVAL      lnaDescO74 = sodes(i)
302900140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
303000140604     c*
303100150116      *eliminata forzatura resta impostato dalla mamma per composizione link
303200150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
303300150116     c*                  MOVE      arbmgs        aasmgsO74
303400140818      *riempimento da verificare
303500140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
303600140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
303700140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
303800140818
303900140818     c                   if        tpporto74 = 'A'
304000140818     c                   if        arbccm > 0
304100140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
304200140818     c                   else
304300140818     c                   movel     arblnp        ccmO74
304400140818     c                   move      8888          ccmO74
304500140818     c                   endif
304600140818     c                   else
304700140818     c                   z-add     arbksc        ccmO74
304800140818     c                   endif
304900140818      *
305000140604     c                   MOVEL     arblna        lineaaro74
305100140604     c                   MOVEL     arbtsp        tpservio74
305200140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
305300140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
305400140604     C                             rpyOpCode:rpyEsito)
305500140604     C                   EVAL      corro74 = §5EDes
305600140604     c*
305700140604     c                   EVAL      destino74 = arbrsd
305800140604     c                   EVAL      desindo74 = arbind
305900140604     c                   EVAL      descapo74 = arbcad
306000140604     c                   EVAL      desloco74 = arblod
306100140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
306200140604if  1c                   IF        arbnzd    = *blanks
306300140604     c                   EVAL      desnazo74 = arbprd
306400140604x   1c                   ELSE
306500150326     c**                 EVAL      desnazo74 = arbnzd
306600150326     c**                 EVAL      desnzdo74 = arbnzd
306700150326     c* 26/03/15: Attiviamo le specifiche per provincia export
306800150326     c                   EVAL      desnazo74 = arbprd
306900150326     c                   EVAL      desnzdo74 = arbnzd
307000150326e   1c                   ENDIF
307100140604     c*
307200140604     c                   EVAL      collio74  = arbncl
307300140604     c                   EVAL      pesoo74   = arbpkf
307400140604     c                   EVAL      volumeo74 = arbvlf
307500140604     c                   EVAL      naturao74 = arbnas
307600140604     c*
307700140604     c                   EVAL      disnumo74  = arbndc
307800140604c    c                   MOVEL     arbddc        wdat
307900140604     c                   EXSR      edtdat
308000140604     c                   EVAL      disdatao74 = wdate
308100140604     c*
308200140604     c                   EVAL      mito74    = arbrsm
308300140604     c                   EVAL      mitindo74 = arbinm
308400140604     c                   EVAL      mitcapo74 = arbcam
308500140604     c                   EVAL      mitloco74 = arblom
308600140604      *specifiche da eliminare dopo la modifica in immissione bolle
308700140604if  3c                   IF        arbnzm = *blanks
308800140604     c                   EVAL      mitnazo74 = arbprm
308900140604x   3c                   ELSE
309000140604     c                   EVAL      mitnazo74 = arbnzm
309100140604e   3c                   ENDIF
309200140604     c* 2a ragione sociale destinatario.
309300140604     c                   CLEAR                   tita4000
309400140604     c                   CLEAR                   tita4p00
309500140604     c                   Z-ADD     arbaas        ka4aas
309600140604     c                   Z-ADD     arblnp        ka4lnp
309700140604     c                   Z-ADD     arbnrs        ka4nrs
309800140604     c                   Z-ADD     arbnsp        ka4nsp
309900140604     c                   MOVEL     'D'           ka4trc
310000140604     c     keyta4        CHAIN     fiar401l                           99
310100140604if  1c                   IF        NOT *in99
310200140604     c                   MOVEL     ar4not        desti2O74
310300140604e   1c                   ENDIF
310400140604     c                   MOVEL     'E'           ka4trc
310500140604     c     keyta4        CHAIN     fiar401l                           99
310600140604if  1c                   IF        NOT *in99
310700140604     c                   MOVEL     ar4not        dsbl4e
310800140604e   1c                   ENDIF
310900140604     c
311000140604     ** Riferimento mittente numerico (c'è sempre).
311100140604     C                   EVAL      rifero74 = arbrmn
311200140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
311300140604     C                   IF        arbrma <> *blank and
311400140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
311500140604     C                   EVAL      rifero74a = arbrma
311600140604     C                   ENDIF
311700140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
311800140604     C                   IF        §b4erp <> *BLANK AND
311900140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
312000140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
312100140604     C                   EVAL      rpto74 = §b4erp
312200140604     C                   ENDIF
312300140604
312400140604     **?Reperisco il check code della spedizione.
312500140604     C                   EVAL      SChkCdeO74 =
312600140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
312700140604     C                             NSpedizI74:ChkCode:nEsito)
312800140604
312900140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
313000141126     C*m                 IF        KSCI74 <> *BLANK
313100141126     c*m                 SETOFF                                           98
313200140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
313300141126     C*m                 RESET                   tis7700i0
313400141126     C*m                 EVAL      tis7700i0.aas = arbAas
313500141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
313600141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
313700141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
313800141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
313900141126     C*m                 EVAL      tis7700i0.ksu = kscI74
314000141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
314100141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
314200141126     C*m                           : rpyEsito
314300141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
314400141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
314500141126     C*m                           )
314600141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
314700141126     C*m                           tis7700o0.bollValida = *ON)
314800140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
314900141126if  2c*m                 IF        NOT *in98                                      *non trovato
315000141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
315100141126     C*m                 reset                   TIS778DSO
315200141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
315300141126     C*m                 RETURN
315400141126     C*m                 ENDIF
315500141126e   2c*m                 ENDIF
315600141126     C*m                 ENDIF
315700140604     c*
315800140604     ** Importo assicurato.
315900140604     C                   IF        arbIas > 0
316000140604     C                   EVAL      vasO74 = arbVas
316100140604     C                   EVAL      iasO74 = arbIas
316200140604     C                   ENDIF
316300140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
316400140605     c                   if        arbdcm = 0 and wcev <> '704'
316500140617     c                   if        arbdcr > 0
316600140605     c                   MOVEL     arbdcr        wdat
316700140605     c                   EXSR      edtdat
316800140605     c                   eval      DTCONRCO74  =  wdate
316900140819if  3c                   IF        arbtcr = ' '
317000140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
317100140819e   3c                   ENDIF
317200140617     c                   endif
317300140617     c                   if        arbhcr > 0
317400140617     c                   MOVEL     arbhcr        wora
317500140617     c                   EXSR      edtora
317600140617     c                   eval      ORACONRO74  =  worae
317700140617     c                   endif
317800140606     c                   eval      TPDATCRO74  =  arbtcr
317900140605      *reperimento descrizione tipo consegna richiesta
318000140617if  3c                   IF        arbtcr = 'P'
318100140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
318200140617e   3c                   ENDIF
318300140617if  3c                   IF        arbtcr = 'D'
318400140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
318500140617e   3c                   ENDIF
318600140605     C                   clear                   tnsd99ds
318700140605     C                   MOVEL     ' '           D98tla
318800140605     C                   MOVEL     'A'           D98tbo
318900140605     C                   Z-ADD     arbaas        D98aas
319000140605     C                   Z-ADD     arblnp        D98lnp
319100140605     C                   Z-ADD     arbnrs        D98nrs
319200140605     C                   Z-ADD     arbnsp        D98nsp
319300140605     C*
319400140605     C                   call      'TNSD99R'
319500140605     C                   parm                    tnsd99ds
319600150304     c
319700140605     C*
319800140917      * chiamata a trulors per tempi standard
319900140917     c                   clear                   trulorsds
320000140917     c                   clear                   trulor2ds
320100140917     c                   clear                   trulor3ds
320200150611     c                   eval      wdee=d98dee
320300140917      * solo se questa data è valorizzata
320400150304    1c                   if        d98dee > 0
320500140912     c                   movel     d98dee        wdat
320600140605     c                   exsr      edtdat
320700140605     c                   eval      DTTEOCOO74  =  wdate
320800140819     c
320900140904      * se fermo deposito non richiamo pgm per reperimento orari
321000150304    2c                   if         arbffd = *blank
321100140605     c                   eval       IOREtser = 'C'
321200140605     c                   eval       IOREDDC=arbddc
321300140605     c                   eval       IOREnDC=arbndc
321400140609     c                   eval       IOREfil=arblna
321500140609     c                   eval       IOREdta=d98dce
321600140609     c                   eval       IOREcap=arbcad
321700140609     c                   eval       IOREloc=arblod
321800140609     c                   eval       IOREtfp=arbtfp
321900140609     c                   eval       IOREtfa=arbtfa
322000140610     c                   eval       IOREdti=arbdti
322100140610     c                   eval       IOREhti=arbhti
322200140610     c                   eval       IOREdcr=arbdcr
322300140610     c                   eval       IOREhcr=arbhcr
322400140610     c                   eval       IOREtcr=arbtcr
322500140819     c                   eval       IOREtsp=arbtsp
322600140819     c                   eval       IOREnar=arbnzd
322700140819     c                   eval       IOREdei=d98dei
322800140819     c                   eval       IOREoei=d98oei
322900150325     c* passo la località normalizzata
323000150325     c                   clear                   tita4000
323100150325     c                   clear                   tita4p00
323200150325     c                   Z-ADD     arbaas        ka4aas
323300150325     c                   Z-ADD     arblnp        ka4lnp
323400150325     c                   Z-ADD     arbnrs        ka4nrs
323500150326     c                   Z-ADD     arbnsp        ka4nsp
323600150325     c                   MOVEL     'X'           ka4trc
323700150325     c     keyta4        CHAIN     tita430c                           99
323800150325if  1c                   IF        NOT *in99
323900150325     c                   MOVEL     ta4not        OOR3LOC_N
324000150325     c                   else
324100150325     c                   movel     arblod        OOR3LOC_N
324200150325e   1c                   ENDIF
324300140605     c
324400150304    3c                   IF         (ARBfbc = *blanks and
324500140605     c                               ARBddc > 0 and
324600140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
324700140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
324800140605     c                   eval       IOREcons = 'S'
324900150304    3c                   endif
325000140605     c* Orario presunto ritiro/consegna
325100140605     c                   move      arbndc        pctndc
325200140605     c                   move      arbifp        pctfgs
325300140605     c                   move      'COR'         ttrd
325400140605     c     kcet          setgt     fipct02l
325500140605     c     kcet          readpe    fipct02l
325600150304    3c                   if        not %eof(fipct02l)
325700140605     c                   movel     pctdati       fipctcords
325800140605     c                   eval      ioreorp=§PCTORASTI
325900140605     c                   endif
326000140605     c                   call      'TRULORSR'
326100140605     C                   PARM                    kpjba
326200140605     C                   PARM                    trulorsds
326300140605     C                   PARM                    trulor2ds
326400140605     C                   PARM                    trulor3ds
326500150304    3c                   endif
326600140818     c                   clear                   ORSTINIO74
326700150304    3c                   if        OOR2STIS > 0
326800140617     c                   MOVEL     OOR2STIS      wora
326900140617     c                   EXSR      edtora
327000140617     c                   eval      ORSTINIO74  =  worae
327100150304    3c                   endif
327200140818     c                   clear                   ORSTFINO74
327300150304    3c                   if        OOR2STFS > 0
327400140617     c                   MOVEL     OOR2STFS      wora
327500140617     c                   EXSR      edtora
327600140617     c                   eval      ORSTFINO74  =  worae
327700150304    3c                   endif
327800140818     c                   clear                   ORMIINIO74
327900150304    3c                   if        OOR2MIIS > 0
328000140617     c                   MOVEL     OOR2MIIS      wora
328100140617     c                   EXSR      edtora
328200140617     c                   eval      ORMIINIO74  =  worae
328300150304    3c                   endif
328400140818     c                   clear                   ORMAFINO74
328500150304    3c                   if        OOR2MXFS > 0
328600140617     c                   MOVEL     OOR2MXFS      wora
328700140617     c                   EXSR      edtora
328800140617     c                   eval      ORMAFINO74  =  worae
328900150304    3c                   endif
329000150304    3c                   if        wricons = 'S'
329100140818     c                   clear                   ORRIDALO74
329200150304    4c                   if        OOR2RIDS > 0
329300140617     c                   MOVEL     OOR2RIDS      wora
329400140617     c                   EXSR      edtora
329500140617     c                   eval      ORRIDALO74  =  worae
329600150304    4c                   endif
329700140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
329800140818     c                   clear                   ORRIALLO74
329900150304    4c                   if        OOR2RIAS > 0
330000140617     c                   MOVEL     OOR2RIAS      wora
330100140617     c                   EXSR      edtora
330200140617     c                   eval      ORRIALLO74  =  worae
330300150304    4c                   endif
330400140617     c                   eval      FLRICONO74  =  OOR2FRIC
330500150304   x3c                   else
330600140711     c                   clear                   ORRIALLO74
330700140711     c                   clear                   ORRIDALO74
330800140711     c                   eval      FLRICONO74  =  'N'
330900150304    3c                   endif
331000140818     c                   clear                   ORSTICDO74
331100150304    3c                   if        OOR2PRESD > 0
331200140617     c                   MOVEL     OOR2PRESD     wora
331300140617     c                   EXSR      edtora
331400140617     c                   eval      ORSTICDO74  =  worae
331500150304    3c                   endif
331600140818     c                   clear                   ORSTICAO74
331700150304    3c                   if        OOR2PRESa > 0
331800140617     c                   MOVEL     OOR2PRESa     wora
331900140617     c                   EXSR      edtora
332000140617     c                   eval      ORSTICAO74  =  worae
332100150304    3c                   endif
332200140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
332300140605     c                   eval      FLLOCNOO74  =  OOR3NORM
332400140904      *end F.D.
332500150304    2c                   endif
332600140605      * RIFERIMENTI DESTINATARIO
332700140605     c                   MOVEL     'EMD'         kr5trd
332800140605     c     keyAR5        CHAIN     fiar501l
332900150304if  2c                   IF        %found(fiar501l)
333000140605     c                   MOVEL     ar5uni        dar5emd
333100140606     c                   eval      EMLDESTO74  =  §§AR5EML
333200140606     c                   eval      CELDESTO74  =  §§AR5TEL
333300150304e   2c                   ENDIF
333400140604     **
333500150304     c*
333600150304     c* Stato data prevista consegna
333700150305     c                   clear                   stat_n2o74
333800150305     c                   eval      STATUSo74   =  D98SPCDEE
333900150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
334000150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
334100150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
334200150312     c                   eval      statuso74='RIC'
334300150403     c* se presente FIARP --> visualizzo la data richiesta
334400150403     c     karp          chain     fiarp01l
334500150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
334600150611     c                   movel     arpdcr        wdee
334700150611     c                   movel     arpdcr        wdat
334800150403     c                   exsr      edtdat
334900150403     c                   eval      DTTEOCOO74  =  wdate
335000150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
335100150421    3C                   IF        ARPHCR>0
335200150403     c                   eval      orarich=arphcr
335300150421    3c                   endif
335400150421   x2c                   else
335500150403     c*  se presente DOPO IL imposto la data in D98dee
335600150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
335700150403     c                   eval      D98dee=arpdcr
335800150611     c                   eval      wdee=arpdcr
335900150421    3c                   endif
336000150403     c* PRIMA DEL come se non ci fosse...
336100150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
336200150421    3c                   if        d98dee>0
336300150312     c                   Clear                   xgiolavds
336400150312     c                   Eval      Ixglpa  = 'A'
336500150312     c                   Eval      IxglFil = arblna
336600150312     c                   Eval      Ixgldata = d98dee
336700150312     c                   Eval      Ixgladd = 'S'
336800150312     c                   Eval      Ixgllav = 'S'
336900150312     c                   eval      ixglgg  = 1
337000150312     c                   Call      'XGIOLAV'
337100150312     c                   parm                    xgiolavds
337200150611     c                   movel     oxgldata      wdee
337300150611     c                   movel     oxgldata      wdat
337400150312     c                   exsr      edtdat
337500150312     c                   eval      DTTEOCOO74  =  wdate
337600150421    3c                   endif
337700150421    2c                   endif
337800150421    1c                   endif
337900150421     c
338000150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
338100150421     c                   eval      statuso74='ATT'
338200150421     c                   endif
338300150421    1c                   if        PDAgia='S' and statuso74='DDC'
338400150421     c                   eval      statuso74='GIA'
338500150421     c                   endif
338600150401     c
338700150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
338800150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
338900150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
339000150401     c* La data contatto = data distinta + 1
339100150512     c**                 Clear                   xgiolavds
339200150512     c**                 Eval      Ixglpa  = 'A'
339300150512     c**                 Eval      IxglFil = arblna
339400150512     c**                 Eval      Ixgldata = arbddc
339500150512     c**                 Eval      Ixgladd = 'S'
339600150512     c**                 Eval      Ixgllav = 'S'
339700150512     c**                 eval      ixglgg  = 1
339800150512     c**                 Call      'XGIOLAV'
339900150512     c**                 parm                    xgiolavds
340000150512     c**                 movel     oxgldata      wdat
340100150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
340200150512     c**                 endif
340300150317     c
340400150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
340500150326     c*  lo status diventa RIC
340600150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
340700150317     c                   eval      statuso74='RIC'
340800150317     c                   endif
340900150310     c
341000150304     c* decodifica stato da mettere nei campi deco e note
341100150304     c                   clear                   dspc
341200150304     C                   RESET                   tibs02ds
341300150304     C                   EVAL      t02Cod = 'SPC'
341400150312     C                   EVAL      t02Ke1 = statuso74
341500150306     C                   EXSR      getTntbe00f
341600150306     c                   IF        t02Err <>'E'
341700150306     c
341800150305     c                   eval       stat_d1o74=§SPCDEC1
341900150305     c                   eval       stat_d2o74=§SPCDEC2
342000150304     c*
342100150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
342200150305     c                   eval       stat_n1o74=§SPCNOT1
342300150306     c                   eval       stat_n2o74=§SPCNOT2
342400150304     c                   else
342500150305     c                   clear                   stat_d1o74
342600150305     c                   clear                   stat_d2o74
342700150305     c                   clear                   stat_n1o74
342800150306     c                   clear                   stat_n2o74
342900150304     c                   endif
343000150409     c                   else
343100150409     c* stato evento CONSEGNATO
343200150409     c                   eval      statuso74='CON'
343300150415     c                   eval      flgbolcO74  =  'S'
343400150415     c                   eval      flgboldO74  =  'N'
343500150409    1c                   endif
343600150304     c
343700150304     c
343800140919     ** riempimento per campi da arb con data consegna diversa da 0
343900140919     c                   if        arbdcm > 0
344000140919     c                   movel     arbdcm        wdat
344100140919     c                   exsr      edtdat
344200140919     c                   eval      dtconmeo74  = wdate
344300140919     c                   MOVEL     arbdcm        wora
344400140919     c                   EXSR      edtora
344500140919     c                   eval      orconmeO74  =  worae
344600140919     c                   eval      flgbolcO74  =  'S'
344700140919     c                   eval      flgboldO74  =  'N'
344800141015      *firmatario distinta PDA
344900141015     c                   clear                   firmato74
345000141015     c                   if        gen§ar5pdaco <> 'NO'
345100141015     c                   exsr      repfirmatARB
345200141015     c                   else
345300141015     c                   if        arbgma <> *blank
345400141015     C                   EVAL      tblKey = arbgma
345500141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
345600141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
345700141015     C                             :rpyOpCode:rpyEsito)
345800141015     C                   IF        rpyOpCode = 'FOUND'
345900141015     c                   if        §7r2fi = 'S'
346000141015     c                   exsr      repfirmatARB
346100141015     c                   endif
346200141015     c                   endif
346300141015     c                   endif
346400141015     c                   endif
346500140919     c                   else
346600150415     c* solo se non impostato da PdA lo stato CONSEGNA
346700150415     c                   if        statuso74<>'CON'
346800150415     c
346900140919     c                   eval      flgbolcO74  =  'N'
347000140919     c                   if        arbndc > 0 and arbddc > 0
347100150415     c                             and arbddc <= datcor and d98cons='S'
347200140919     c                   eval      flgboldo74 = 'S'
347300140919     c                   else
347400140919     c                   eval      flgboldO74  =  'N'
347500140919     c                   endif
347600150415     c                   endif
347700150415
347800140919     c                   endif
347900150415     c
348000150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
348100140919     c                   eval      orincono74 = orsticdo74
348200140919     c                   eval      orficono74 = ORSticaO74
348300140919     c                   else
348400140919     c                   eval      orincono74 = ORSTINIO74
348500140919     c                   eval      orficono74 = ORSTFINO74
348600140919     c                   endif
348700150326     c* note 1
348800150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
348900150309     c                   clear                   wparm
349000150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
349100150326     c                   if        D98DTCTD<>*blanks
349200150326     c                   movel     D98DTCTD      wdat
349300150326     c                   exsr      edtdat
349400150326     c                   else
349500150326     c                   eval      wdate=dtteocoo74
349600150410     c                   eval      usaprev='S'
349700150326     c                   endif
349800150326     c
349900150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
350000150309     c                   eval      stat_n1o74= rtvMsgLang(
350100150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
350200150309     c                   endif
350300150309     c* note 2
350400150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
350500150309     c                   clear                   wparm
350600150403     c* Se lo stato è PRG passo orario limite uscita aut
350700150403     c                   select
350800150403     c                   when      D98SPCDEE='PRG'
350900150326     c                   IF        d98picklim  >*zeros
351000150309
351100150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
351200150326     c                                   (%subst(d98picklim:3:2))
351300150309     c                   else
351400150309     c                   eval      wparm='09:00'
351500150403     c* reperisco orario di fine picking
351600150403     c* filiale / data
351700150403     C                   RESET                   tibs02ds
351800150403     C                   EVAL      t02Cod = 'OLP'
351900150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
352000150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
352100150403     C                   EXSR      getTntbe00f
352200150403     c* filiale
352300150403     c                   IF        t02Err = 'E'
352400150403     C                   RESET                   tibs02ds
352500150403     C                   EVAL      t02Cod = 'OLP'
352600150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
352700150403     C                   EXSR      getTntbe00f
352800150403     c                   endif
352900150403     c* Generico 000
353000150403     c                   IF        t02Err = 'E'
353100150403     C                   RESET                   tibs02ds
353200150403     C                   EVAL      t02Cod = 'OLP'
353300150403     C                   EVAL      t02Ke1 = '000'
353400150403     C                   EXSR      getTntbe00f
353500150403     c                   endif
353600150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
353700150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
353800150403     c                             %subst(t02uni:3:2)
353900150309     c                   endif
354000150403     c                   endif
354100150403     c                   eval      stat_n2o74= rtvMsgLang(
354200150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
354300150403     c                   other
354400150403     c* altri casi
354500150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
354600150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
354700150611     c                             arbtcr=' ' and arbdcr>0  and
354800150611     c                             arbdcr>=wdee
354900150410     c
355000150403     c                   eval      orarich=arbhcr
355100150410     c                   endif
355200150403     c
355300150403     c                   if        orarich>0
355400150403     c                   exsr      sr_dallealle
355500150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
355600150403     c                   eval      stat_n2o74= rtvMsgLang(
355700150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
355800150403     c
355900150403     c                   else
356000150403     c                   clear                   stat_n2o74
356100150403     c                   endif
356200150403     c
356300150403     c                   endsl
356400150403     c
356500150309     c                   endif
356600150410     c
356700150424     c* Se si tratta di fermo deposito pulisco sempre
356800150424     c                   if        usaprev<>'S' or arbffd='S'
356900150410     c                   clear                   DTTEOCOO74
357000150410     c                   clear                   ORINCONO74
357100150410     c                   clear                   ORFICONO74
357200150410     c                   endif
357300150309     c
357400140604     c                   ENDSR
357500150403      /free
357600150403       //--------------------------------------------------------------
357700150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
357800150403       //--------------------------------------------------------------
357900150403       BEGSR  sr_dallealle;
358000150403       clear  w_dallemsg ;
358100150403       clear  w_allemsg ;
358200150403
358300150403       // Reperimento dati tabella VPO - ORARISER
358400150403       if §VPORST_MW=0  ;
358500150403       clear tibs02ds;
358600150403       clear dvpooraris;
358700150403       t02mod='C';
358800150403       t02cod='VPO';
358900150403       t02ke1='ORARISER';
359000150403       tibs02r(kpjba:tibs02ds);
359100150403       if t02err=*blanks;
359200150403          dvpooraris=t02uni;
359300150403       endif;
359400150403       endif;
359500150403
359600150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
359700150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
359800150403       //   determino orari servizio comprensivi della tolleranza
359900150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
360000150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
360100150403
360200150403            if w_dalle<w_dalleoser;
360300150403               w_dalle=w_dalleoser;
360400150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
360500150403            endif;
360600150403
360700150403            if w_alle>w_alleoser;
360800150403               w_alle=w_alleoser;
360900150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
361000150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
361100150403               endif;
361200150403            endif;
361300150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
361400150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
361500150403
361600150403       endsr;
361700150403      /end-free
361800010717     c*--------------------------------------------------------------------------------------------*
361900010717     c* imposta i campi della DS di Output dagli eventi
362000010717     c*--------------------------------------------------------------------------------------------*
362100010717     c     RepAbilFirma  BEGSR
362200010717     c*
362300010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
362400010717     c* e recupera utente / password
362500010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
362600010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
362700010717     c     keyvtd        SETLL     tivtd01l
362800010717     c     keyvtd        READE     tivtd01l                               98
362900010717do  2c                   DOW       NOT *in98
363000010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
363100010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
363200010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
363300010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
363400010717     c                   EVAL      depute = vtdute                              *utente
363500010717     c                   EVAL      deppwd = vtdpwd                              *password
363600010717     c                   LEAVE                                                  *esce dal ciclo
363700010717e   3c                   ENDIF
363800010717     c     keyvtd        READE     tivtd01l                               98
363900010717e   2c                   ENDDO
364000010717e   1C                   ENDIF
364100010717     c*
364200010717     c                   ENDSR
364300000000     c*--------------------------------------------------------------------------------------------*
364400000000     c* edita data da aaaammgg in gg/mm/aaaa
364500000000     c*--------------------------------------------------------------------------------------------*
364600000000     c     edtdat        BEGSR
364700000000     c*
364800000000     c                   MOVEL     *blanks       wdate
364900000000     c*
365000000000     c                   MOVEL     wdat          dsdat
365100000000     c                   MOVEL     dsgio         dsgioe
365200140528     c                   MOVEL     '.'           dsvd1
365300000000     c                   MOVEL     dsmes         dsmese
365400140528     c                   MOVEL     '.'           dsvd2
365500000000     c                   MOVEL     dsann         dsanne
365600000000     c                   MOVEL     dsdate        wdate
365700000000     c*
365800000000     c                   ENDSR
365900000000     c*--------------------------------------------------------------------------------------------*
366000000000     c* edita ora da hhmm in hh:mm
366100000000     c*--------------------------------------------------------------------------------------------*
366200000000     c     edtora        BEGSR
366300000000     c*
366400000000     c                   MOVEL     *blanks       worae
366500000000     c*
366600000000     c                   MOVEL     wora          dsora
366700000000     c                   MOVEL     dshh          dshhe
366800140617     c                   MOVEL     '.'           dsvo1
366900000000     c                   MOVEL     dsmm          dsmme
367000000000     c                   MOVEL     dsorae        worae
367100000000     c*
367200000000     c                   ENDSR
367300000000     c*--------------------------------------------------------------------------------------------*
367400000000     c* decodifica punto operativo
367500000000     c*--------------------------------------------------------------------------------------------*
367600000000     c     decorg        BEGSR
367700000000     c*
367800000000     c                   MOVEL     *blanks       wodes                          *descrizione
367900000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
368000000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
368100000000     c*
368200000000     c                   SETOFF                                           98
368300041111     c                   Z-ADD     1             O
368400041111     c     worg          LOOKUP    sorg(O)                                98
368500000000if  1c                   IF        *in98                                        *trovata
368600121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
368700121001     c                             %EDITC(wOrg : 'X') + ')'
368800041111     c                   MOVEL     sofl1(O)      wofl1
368900041111     c                   MOVEL     sodpd(O)      wodpd
369000000000e   1c                   ENDIF
369100000000     c*
369200000000     c                   ENDSR
369300000000     c*--------------------------------------------------------------------------------------------*
369400000000     c* decodifica evento per STATI
369500000000     c*--------------------------------------------------------------------------------------------*
369600000000     c     deccevsta     BEGSR
369700000000     c*
369800000000     c                   MOVEL     '0'           werr                           *NO errore
369900050704     c*
370000050704     C                   IF        wcev = 'GEN'
370100050704     C                   EVAL      werr = *ON
370200050704     C                   CLEAR                   wcdex
370300050704     C                   LEAVESR
370400050704     C                   ENDIF
370500000000     c*
370600060622     C                   RESET                   tibs02ds
370700060622     C                   EVAL      t02Cod = 'ICE'
370800060622     C                   EVAL      t02Ke1 = wcev
370900060622     C                   EXSR      getTntbe00f
371000060622     C                   IF        t02Err <> 'E' AND
371100140528     C                             (§icesta = 'S'  OR
371200140528     C                               §icestaP = 'S' )
371300060622     c                   MOVEL     §icedei       wcdex
371400000000x   1c                   ELSE                                                   *non trovato
371500000000     c                   MOVEL     '1'           werr                           *SI errore
371600030130     c                   MOVEL     *blanks       wcdex
371700000000e   1c                   ENDIF
371800000000     c*
371900000000     c                   ENDSR
372000000000     c*--------------------------------------------------------------------------------------------*
372100000000     c* decodifica consegna particolare
372200000000     c*--------------------------------------------------------------------------------------------*
372300000000     c     decftc        BEGSR
372400000000     c*
372500000000     c                   MOVEL     *blanks       wdftc
372600000000     c*
372700000000if  1c                   IF        wftc = 'S'
372800060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
372900000000e   1c                   ENDIF
373000000000if  1c                   IF        wftc = 'P'
373100060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
373200000000e   1c                   ENDIF
373300000000if  1c                   IF        wftc = 'A'
373400060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
373500000000e   1c                   ENDIF
373600000000if  1c                   IF        wftc = 'F'
373700060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
373800000000e   1c                   ENDIF
373900000000     c*
374000000000     c                   ENDSR
374100000000     c*--------------------------------------------------------------------------------------------*
374200000000     c* decodifica turno di chiusura
374300000000     c*--------------------------------------------------------------------------------------------*
374400000000     c     decgcx        BEGSR
374500000000     c*
374600000000     c                   MOVEL     *blanks       wdgcx
374700000000     c* giorno
374800000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
374900000000if  1c                   IF        a1 = '1'
375000060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
375100000000e   1c                   ENDIF
375200000000if  1c                   IF        a1 = '2'
375300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
375400000000e   1c                   ENDIF
375500000000if  1c                   IF        a1 = '3'
375600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
375700000000e   1c                   ENDIF
375800000000if  1c                   IF        a1 = '4'
375900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
376000000000e   1c                   ENDIF
376100000000if  1c                   IF        a1 = '5'
376200060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
376300000000e   1c                   ENDIF
376400000000if  1c                   IF        a1 = '6'
376500060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
376600000000e   1c                   ENDIF
376700000000if  1c                   IF        a1 = '7'
376800060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
376900000000e   1c                   ENDIF
377000000000if  1c                   IF        a1 = ' '
377100000000     c                   EVAL      wdgcx = *blanks
377200000000e   1c                   ENDIF
377300000000     c* am/pm
377400000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
377500000000if  1c                   IF        a1 = ' '
377600060612     c                   EVAL      wdgcx =
377700060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
377800000000e   1c                   ENDIF
377900000000if  1c                   IF        a1 = 'M'
378000060612     c                   EVAL      wdgcx =
378100060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
378200000000e   1c                   ENDIF
378300000000if  1c                   IF        a1 = 'P'
378400060612     c                   EVAL      wdgcx =
378500060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
378600000000e   1c                   ENDIF
378700000000     c*
378800000000     c                   ENDSR
378900000000     c*--------------------------------------------------------------------------------------------*
379000000000     c* decodifica evento per GIACENZE
379100000000     c*--------------------------------------------------------------------------------------------*
379200000000     c     deccevgia     BEGSR
379300000000     c*
379400000000     c                   MOVEL     '0'           werr                           *NO errore
379500000000     c*
379600060622     C                   RESET                   tibs02ds
379700060622     C                   EVAL      t02Cod = 'ICE'
379800060622     C                   EVAL      t02Ke1 = wcev
379900060622     C                   EXSR      getTntbe00f
380000060622     C                   IF        t02Err <> 'E' AND                            *trovata
380100140528     C                             (§icegia = 'S'  OR
380200140528     C                               §icegiap = 'S')
380300060622     c                   MOVEL     §icedei       wcdex
380400000000x   1c                   ELSE                                                   *non trovato
380500000000     c                   MOVEL     '1'           werr                           *SI errore
380600030130     c                   MOVEL     *blanks       wcdex
380700000000e   1c                   ENDIF
380800000000     c*
380900000000     c                   ENDSR
381000000000     c*--------------------------------------------------------------------------------------------*
381100000000     c* Controlla la DS dsi Input
381200000000     c*--------------------------------------------------------------------------------------------*
381300000000     c     chkdsinput    BEGSR
381400000000     c*
381500000000     c                   MOVEL     '0'           werr                           *NO errore
381600000000     c*
381700000000     c* dalla DS di Input scompone il codice spedizione
381800000000     c                   MOVEL     nspedizi74    dsspedizi74
381900000000     c*
382000000000     c* controlla che la lina di partenza sia TUTTA numerica
382100000000     c                   SETOFF                                       99
382200000000     c                   TESTN                   dslnpi74             99
382300000000if  1c                   IF        NOT *in99                                    *non è un numero
382400000000     c                   MOVEL     '1'           werr                           *SI errore
382500140617     c                   eval      widmsg = 'TIS0769'
382600140617     c                   eval      widcampo = '*LNP'
382700140617     c                   clear                   wparm
382800140617     c                   exsr      messaggi
382900000000x   1c                   ELSE                                                   *è un numero
383000000000     c                   MOVE      dslnpi74      a1
383100000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
383200000000     c                   MOVEL     '1'           werr                           *SI errore
383300140617     c                   eval      widmsg = 'TIS0769'
383400140617     c                   eval      widcampo = '*LNP'
383500140617     c                   clear                   wparm
383600140617     c                   exsr      messaggi
383700000000e   2c                   ENDIF
383800000000e   1c                   ENDIF
383900000000     c*
384000000000     c* controlla che la serie spedizione sia TUTTA numerica
384100000000     c                   SETOFF                                       99
384200000000     c                   TESTN                   dsnrsi74             99
384300000000if  1c                   IF        NOT *in99                                    *non è un numero
384400000000     c                   MOVEL     '1'           werr                           *SI errore
384500140617     c                   eval      widmsg = 'TIS0769'
384600140617     c                   eval      widcampo = '*NRS'
384700140617     c                   clear                   wparm
384800140617     c                   exsr      messaggi
384900000000x   1c                   ELSE                                                   *è un numero
385000000000     c                   MOVE      dsnrsi74      a1
385100000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
385200000000     c                   MOVEL     '1'           werr                           *SI errore
385300140617     c                   eval      widmsg = 'TIS0769'
385400140617     c                   eval      widcampo = '*NRS'
385500140617     c                   clear                   wparm
385600140617     c                   exsr      messaggi
385700000000e   2c                   ENDIF
385800000000e   1c                   ENDIF
385900000000     c*
386000000000     c* controlla che il numero spedizione sia TUTTO numerico
386100000000     c                   SETOFF                                       99
386200000000     c                   TESTN                   dsnspi74             99
386300000000if  1c                   IF        NOT *in99                                    *non è un numero
386400000000     c                   MOVEL     '1'           werr                           *SI errore
386500140617     c                   eval      widmsg = 'TIS0769'
386600140617     c                   eval      widcampo = '*NSP'
386700140617     c                   clear                   wparm
386800140617     c                   exsr      messaggi
386900000000x   1c                   ELSE                                                   *è un numero
387000000000     c                   MOVE      dsnspi74      a1
387100000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
387200000000     c                   MOVEL     '1'           werr                           *SI errore
387300140617     c                   eval      widmsg = 'TIS0769'
387400140617     c                   eval      widcampo = '*NSP'
387500140617     c                   clear                   wparm
387600140617     c                   exsr      messaggi
387700000000e   2c                   ENDIF
387800000000e   1c                   ENDIF
387900110104     c*
388000110104     c* controlla che il codice cliente sia TUTTO numerico
388100110104     c                   IF        kscI74 <> *BLANK
388200110104     c                   SETOFF                                       99
388300110104     c                   TESTN                   kscI74               99
388400110104     c                   IF        NOT *in99                                    *non è un numero
388500110104     c                   MOVEL     '1'           werr                           *SI errore
388600140617     c                   eval      widmsg = 'TIS0769'
388700140617     c                   eval      widcampo = 'KSCI74'
388800140617     c                   clear                   wparm
388900140617     c                   exsr      messaggi
389000110104     c                   ELSE                                                   *è un numero
389100110104     c                   MOVE      kscI74        a1
389200110104     c                   IF        a1 < '0'                                     *trovata una lettera
389300110104     c                   MOVEL     '1'           werr                           *SI errore
389400140617     c                   eval      widmsg = 'TIS0769'
389500140617     c                   eval      widcampo = 'KSCI74'
389600140617     c                   clear                   wparm
389700140617     c                   exsr      messaggi
389800110104     c                   ENDIF
389900110104     c                   ENDIF
390000110104     c                   ENDIF
390100110104     c*
390200110104     c* controlla che il codice mittente sia TUTTO numerico
390300110104     c                   IF        ccmI74 <> *BLANK
390400110104     c                   SETOFF                                       99
390500110104     c                   TESTN                   ccmI74               99
390600110104     c                   IF        NOT *in99                                    *non è un numero
390700110104     c                   MOVEL     '1'           werr                           *SI errore
390800140617     c                   eval      widmsg = 'TIS0769'
390900140617     c                   eval      widcampo = 'CCMI74'
391000140617     c                   clear                   wparm
391100140617     c                   exsr      messaggi
391200110104     c                   ELSE                                                   *è un numero
391300110104     c                   MOVE      ccmI74        a1
391400110104     c                   IF        a1 < '0'                                     *trovata una lettera
391500110104     c                   MOVEL     '1'           werr                           *SI errore
391600140617     c                   eval      widmsg = 'TIS0769'
391700140617     c                   eval      widcampo = 'CCMI74'
391800140617     c                   clear                   wparm
391900140617     c                   exsr      messaggi
392000110104     c                   ENDIF
392100110104     c                   ENDIF
392200110104     c                   ENDIF
392300050104     c*
392400050104     c* controlla anno spedizione
392500050104     c                   TESTN                   cAASI74              99
392600050104if  1c                   IF        NOT *in99                                    *non è un numero
392700050104     c                   EVAL      cAASI74 = *ZERO
392800050104x   1c                   ENDIF
392900100222     c*
393000100223     c* Estraggo l'anno dalla data spedizione.
393100100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
393200100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
393300100223     C                   IF        dspI74 <> *BLANK
393400100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
393500100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
393600100223     C                   MONITOR
393700100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
393800100223     C                   ON-ERROR
393900100223     C                   ENDMON
394000100222     C                   ENDIF
394100000000     c*
394200000000     c* se errore, esce dal programma
394300140610if  1c                   IF        werr = '1'                                   *SI errore
394400140610     c                   eval      esito = werr                                 *errore
394500140610e   1c                   ENDIF
394600000000     c*
394700000000     c                   ENDSR
394800000000     c*--------------------------------------------------------------------------------------------*
394900000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
395000000000     c* Input:   Walfa7 = Alfanumerico di 7
395100000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
395200000000     c*--------------------------------------------------------------------------------------------*
395300000000     c     xallinea1     BEGSR
395400000000     c*
395500000000     c* reimposta variabli di lavoro
395600000000     c                   CLEAR                   i
395700000000     c                   CLEAR                   z
395800000000     c                   CLEAR                   walfa7bis
395900000000     c                   EVAL      wzeri = *ALL'0'
396000000000     c*
396100000000     c     ' '           CHECKR    walfa7:7      i
396200000000if  1c                   IF        i <> 0
396300000000     c     i             SUBST     walfa7        walfa7bis
396400000000e   1c                   ENDIF
396500000000if  1c                   IF        walfa7bis <> *blanks
396600000000     c     7             SUB       i             z
396700000000if  2c                   IF        z <> 0
396800000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
396900000000e   2c                   ENDIF
397000000000     c                   EVAL      z = z+ 1
397100000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
397200000000e   1c                   ENDIF
397300000000     c*
397400000000     c                   ENDSR
397500000000     c*--------------------------------------------------------------------------------------------*
397600000000     c* caricamento tabelle occorrenti
397700000000     c*--------------------------------------------------------------------------------------------*
397800000000     c     cartab        BEGSR
397900000000     c*---
398000000000     c* organigrama
398100000000     c*---
398200000000     c                   CLEAR                   i                              *indice
398300000000     C                   CLEAR                   sorg
398400000000     C                   CLEAR                   sodes
398500000000     C                   CLEAR                   sofl1
398600000000     C                   CLEAR                   sopt
398700000000     C                   CLEAR                   sodpd
398800060623     C                   OPEN      tntbe01l
398900000000     c     *loval        SETLL     azorg01l
399000000000     c                   READ      azorg01l                               99
399100000000do  1c                   DOW       NOT *in99
399200000000     c                   ADD       1             i
399300000000     c                   Z-ADD     orgfil        sorg(i)
399400000000     c                   MOVEL     orgdes        sodes(i)
399500000000     c                   MOVEL     orgde3        og143
399600020702     C*
399700020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
399800020702     C                   CLEAR                   dntw
399900020702     C*
400000020702     C                   MOVEL(P)  'NTW'         kbecod
400100020702     C                   MOVEL(P)  §ogntw        kbeke1
400200020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
400300020702     c                   MOVEL     *blanks       kbelin                         *lingua
400400020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
400500020702     C     keytbe        CHAIN     tntbe01l
400600020702     C                   IF        %found(tntbe01l)
400700020702     C                   MOVEL     tbeuni        dntw
400800020702     C                   ENDIF
400900020702     C*
401000020702     C                   MOVEL     §ntwfie       sofl1(i)
401100020702     c                   MOVEL     §ogntw        sopt(i)
401200020702     c                   MOVEL     §ogntw        sodpd(i)
401300000000     c                   READ      azorg01l                               99
401400000000e   1c                   ENDDO
401500060623     C                   CLOSE     tntbe01l
401600000000     c*---
401700000000     c                   ENDSR
401800000000     c*--------------------------------------------------------------------------------------------*
401900000000     c* operazioni iniziali -da fare sempre-
402000000000     c*--------------------------------------------------------------------------------------------*
402100000000     c     rutinz        BEGSR
402200000000     c*
402300000000     c* reperimento data corrente
402400000000     C                   TIME                    n14                            *ora (6)+ data (8)
402500000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
402600000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
402700000000     C                   Z-ADD     n8            g08dat
402800000000     C                   Z-ADD     *zeros        g08inv
402900000000     C                   MOVEL     '0'           g08err
403000000000     c                   CALL      'XSRDA8'
403100000000     c                   PARM                    wlbda8
403200000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
403300000000     C                   MOVEL     datcor        anncor                         *anno corrente
403400000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
403500000000     c*
403600000000     c* calcola data corrente - 1 anno
403700000000     c                   Z-ADD     datcor        datpre                         *data precedente
403800000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
403900000000     c*
404000010716     c* azzera la DS di Output e le variabili di lavoro / controllo
404100000000     c                   MOVEL     '0'           esito
404200140603     c                   reset                   tis778dso                      *ds Output
404300140603     c                   reset                   tis778dsm                      *ds Output
404400000000     c                   CLEAR                   smi                            *di memorizzazione
404500000000     c                   CLEAR                   smseql
404600000000     c                   CLEAR                   smseqe
404700000000     c                   CLEAR                   smdevhev
404800000000     c                   CLEAR                   smdeve
404900000000     c                   CLEAR                   smheve
405000000000     c                   CLEAR                   smfle
405100000000     c                   CLEAR                   smdev
405200000000     c                   CLEAR                   smcev
405300130312     c                   CLEAR                   smspe
405400000000     c                   CLEAR                   smdmc
405500000000     c                   CLEAR                   smdmcR
405600000000     c                   CLEAR                   tote                           *contatore eventi
405700000000     c                   CLEAR                   seql                           *sequenza legami
405800010716     c                   RESET                   $AbTD                          *di lavoro
405900010716     c                   RESET                   $AbFirma
406000150312     c                   clear                   pdaavv
406100150421     c                   clear                   pdagia
406200140603     c* parametri ingresso
406300140603     c                   exsr      Kontrolla
406400000000     c* controlla da DS in Input
406500000000     c                   EXSR      chkdsinput
406600000000     c*
406700000000     c* carica i clienti dettaglio del cliente unificante
406800000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
406900000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
407000000000     c     keyvss        SETLL     tivss02l
407100000000     c     keyvss        READE     tivss02l                               99
407200000000do  1c                   DOW       NOT *in99
407300000000if  2c                   IF        vssvat = *blanks                             *attivo
407400000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
407500000000     c                             datcor <= vssdsc
407600010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
407700010719     c* --> TD per vedere la firma DPD
407800010716     c                   RESET                   $AbTD                          *di lavoro
407900010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
408000010719     c     kssksu        SETLL     tivss02l
408100010719     c     kssksu        READE     tivss02l                               99
408200010719do  4c                   DOW       NOT *in99
408300010716if  5c                   IF        vssvat = *blanks                             *attivo
408400010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
408500010716     c                             datcor <= vssdsc
408600010719if  7c                   IF        vssisv = 'TD'
408700010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
408800010719e   7c                   ENDIF
408900010719e   6c                   ENDIF
409000010716e   5c                   ENDIF
409100010719     c     kssksu        READE     tivss02l                               99
409200010719e   4c                   ENDDO
409300010716     c*
409400000000     c                   LEAVE                                                  *uscita ciclo
409500000000e   3c                   ENDIF
409600000000e   2c                   ENDIF
409700000000     c     keyvss        READE     tivss02l                               99
409800010716e   1c                   ENDDO                                                  *fine cl.abil.TT
409900000000     c*
410000060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
410100060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
410200060621     c*
410300000000     c                   ENDSR
410400040330
410500040330     ***********************************************************************************************
410600040330     **?
410700040330     **?Reperimento ulteriori dati destinatario.
410800040330     **?
410900040330     ***********************************************************************************************
411000040330     C     ImpDSDest     BEGSR
411100040330
411200140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
411300140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
411400100121     C                             ar5Trd <> 'GEN'
411500140716     c                   Z-ADD     f_tasaas      kr5aas
411600140716     c                   Z-ADD     f_taslnp      kr5lnp
411700140716     c                   Z-ADD     f_tasnrs      kr5nrs
411800140716     c                   Z-ADD     f_tasnsp      kr5nsp
411900040330     c                   MOVEL     'GEN'         kr5trd
412000040531     c     keyar5        CHAIN     fiar531c
412100040330     C                   IF        %FOUND
412200040330     C                   EVAL      DAR5GEN = AR5Uni
412300100121     C                   ELSE
412400100121     C                   RESET                   DAR5GEN
412500040330     C                   ENDIF
412600100121     C                   ENDIF
412700100121     C
412800100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
412900100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
413000040330
413100040330     C                   ENDSR
413200040913
413300040913     ***********************************************************************************************
413400040913     **?
413500040913     **?Determino se addebitare l'immagine LDV.
413600040913     **?
413700040913     ***********************************************************************************************
413800040913     C     AddebitoLDV   BEGSR
413900040913
414000040913     C                   IF        LDVO74 = 'S'
414100040913
414200040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
414300040913
414400040913     C                   ENDIF
414500040913
414600040913     C                   ENDSR
414700051110
414800051110     ***********************************************************************************************
414900051110     **?
415000051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
415100051110     **?
415200051110     ***********************************************************************************************
415300051110     C     chkTivpi      BEGSR
415400051110
415500051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
415600051110     C                             esito10)
415700051110     C                   IF        esito10 > 0 AND NOT %ERROR
415800051110     C                   EVAL      gcfdio74 = 'N'
415900051110     C                   ENDIF
416000051110
416100051110     C                   ENDSR
416200051222
416300051222     ***********************************************************************************************
416400051222     **
416500051222     ** Linea arrivo Italia o estera.
416600051222     **
416700051222     ***********************************************************************************************
416800051222     C     setLnaItaEst  BEGSR
416900051222
417000051222     C                   RESET                   wrkLnaItaEst
417100051222     C                   EXSR      decOrg
417200051222     C                   EVAL      wrkLnaItaEst = wofl1
417300051222
417400051222     C                   ENDSR
417500060622
417600060622     ***********************************************************************************************
417700060622     **
417800130312     ** Reperisco informazioni stato bolla in filiale se ultimo evento
417900130312     ** è un MIC
418000060622     **
418100060622     ***********************************************************************************************
418200130312     C     srarb         BEGSR
418300130312     c                   clear                   wcev
418400140714     c                   clear                   wricons
418500130312     c                   clear                   worg
418600130312     c                   clear                   wdat
418700130312     c                   clear                   wora
418800130312     c                   eval      dsevbspe = spe(1)
418900130312     c     karb          chain     fnarb01l
419000130312     c                   if        not %error and
419100130312     c                             %found(fnarb01l)
419200130422     c                   Z-ADD     arblna        worg
419300130312     c                   exsr      setLnaItaEst
419400130312     c* no per estero
419500130312     C                   IF        wrkLnaItaEst = ESTERO
419600130312     c                   leavesr
419700130312     c                   end
419800130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
419900130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
420000130312     C                   SELECT
420100141120     c                   WHEN      arbdcm <> 0
420200130312     c                   MOVEL     '704'         wcev                           *causale evento
420300130313     c                   move      arbdcm        WDAT
420400130313     c                   move      arbhmc        WORA
420500130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
420600130312     c                   WHEN      ARBFDC = ' '
420700130326     c* solo se distinta NON in test
420800130326     c                   clear                   ddstflr
420900130326     c                   z-add     arbifp        dstfgs
421000130326     c                   z-add     arbndc        dstnfv
421100130326     c     kdst          chain     fidst01l
421200130326     c                   if        not %error and
421300130326     c                             %found(fidst01l)
421400130326     c                   eval      ddstflr = dstflr
421500130326     c                   end
421600130327     c                   if        §DSTTSTPDA = 'C' or
421700130327     c                             §DSTTSTPDA = 'E'
421800130326     c                   leavesr
421900130326     c                   end
422000130326     c*
422100130313     c                   move      arbndc        pctndc
422200131105     c                   exsr      cercaceP
422300131105     c                   move      'CET'         ttrd
422400130312     c     kcet          setgt     fipct02l
422500130312     c     kcet          readpe    fipct02l
422600130312     c                   if        not %error and
422700130312     c                             not %eof(fipct02l)
422800130312     c                   movel     pctdati       fipctcetds
422900140926      * firmatario se c'è riempie il campo
423000140926     c                   if        §pctnomfir <> *blank
423100140926     c                   eval      firmato74  = §pctnomfir
423200140926     c                   endif
423300130312     c* consegnata
423400130312     c                   if        §pctcmc = ' '
423500130312     c                   MOVEL     '704'         wcev                           *causale evento
423600130312     c                   else
423700140711     c                   move      'S'           wricons
423800130312     c* mancata consegna
423900130313     c                   movel     §PCTcmc       wcev
424000130312     c                   clear                   ds2a
424100130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
424200130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
424300130312     C                             :rpyEsito)
424400130312     c                   if        §2aflg = '§' and §2acpt = 'S'
424500130312     c                   move      'T  '         wcev
424600130312     c                   endif
424700130312     c                   endif
424800130312     c* ??? £6
424900130312     c                   if        §pctdata > 0
425000130313     c                   move      §PCTDATA      WDAT
425100130312     c                   movel     §PCTORA       WORA
425200130312     c                   endif
425300130312     c                   end
425400130312     c                   endSL
425500130312     c                   end
425600130312     c* ho trovato un evento da inserire  al 1° posto
425700130312     c                   if        wcev <> ' '
425800130312     c                   EXSR      deccevsta
425900130312     c                   IF        werr <> '1'                                  *no errore
426000130312     c                   EXSR      decorg
426100130312     c                   EXSR      edtdat
426200130312     c                   EXSR      edtora
426300130313     c* libero il 1° elemnto di schiera
426400150612     c                   eval      j = 49
426500150612     c                   for       j = 49 downto 1
426600130312     c                   if        cev(j) <> ' '
426700130312     c                   eval      cev(j+1) = cev(j)
426800130312     c                   eval      dev(j+1) = dev(j)
426900130312     c                   eval      hev(j+1) = hev(j)
427000130312     c                   eval      fle(j+1) = fle(j)
427100130312     c                   eval      eve(j+1) = eve(j)
427200130312     c                   end
427300130313     c                   endfor
427400130313     c                   eval      dev(1) = wdate
427500130313     c                   eval      hev(1) = worae
427600130312     c                   eval      cev(1)= wcev
427700130313     c                   eval      fle(1) = wodes
427800130313     c                   eval      eve(1) = wcdex
427900130313     c                   EVAL      cntEveO74 += 1
428000150310     c*
428100150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
428200150310     c                   if        §2aftc='A'
428300150310     c                   eval      pdaavv='S'
428400150310     c                   endif
428500150421     c                   if        §2aftc='G'
428600150421     c                   eval      pdagia='S'
428700150421     c                   endif
428800150310     c
428900130312     c                   end
429000130312     c                   end
429100130312     C                   ENDSR
429200140714     **_________________________________________________________
429300140714     c     cercafiglia   begsr
429400140714     **_________________________________________________________
429500140715     c                   clear                   keyForzata
429600140714      * salva chiave originale per eventi
429700140715     c                   eval      kasaasf = kasaas
429800140715     c                   eval      kaslnpf = kaslnp
429900140715     c                   eval      kasnrsf = kasnrs
430000140715     c                   eval      kasnspf = kasnsp
430100140714     c                   do        *hival
430200140715     c     keytasf       chain     fnlbl02l
430300140714     c                   if        %found(fnlbl02l)
430400140715     c                   eval      kasaasf = LBLaan
430500140715     c                   eval      kaslnpf = LBLlpn
430600140715     c                   eval      kasnrsf = LBLnrn
430700140715     c                   eval      kasnspf = LBLnsn
430800140714     c                   move      'X'           keyForzata
430900140714     c                   iter
431000140714     c                   else
431100140714     c                   leave
431200140714     c                   endif
431300140714     c                   enddo
431400140715     c     keytasf       setll     titas30c                               99    *no errore
431500140715     c     keytasf       READE     titas30c                               99    *no errore
431600141117      *se non trova titas con figlia forza di nuovo la mamma
431700140715if  3c                   IF        *in99                                        *non trovato
431800141117     c                   clear                   titasdssavf
431900140715x   3c                   ELSE                                                   *trovata
432000140715if  4c                   IF        *in01                                        *lettura titas000
432100140715     c                   EVAL      titasdssavf = titasds
432200140715e   4c                   ENDIF
432300140715if  4c                   IF        *in02                                        *lettura titas010
432400140715     c                   EVAL      titasdssavf = titasds
432500140715e   4c                   ENDIF
432600140715if  4c                   IF        *in03                                        *lettura titasp00
432700140715     c                   EVAL      titasdssavf = titasds
432800140715e   4c                   ENDIF
432900140715e   3c                   ENDIF
433000140714     C                   ENDSR
433100131105     **_________________________________________________________
433200131105     c     cercaCEP      begsr
433300131105     **_________________________________________________________
433400131105     c                   move      'CEP'         ttrd
433500131105     c     kcet          setgt     fipct02l
433600131105     c     kcet          readpe    fipct02l
433700131105     c                   if        not %error and
433800131105     c                             not %eof(fipct02l)
433900131105     c                   movel     pctdati       fipctcetds
434000131105     c* consegnata
434100131105     c                   if        §pctcmc = ' '
434200131105     c                   MOVEL     '704'         wcev                           *causale evento
434300131105     c                   else
434400131105     c* mancata consegna
434500131105     c                   movel     §PCTcmc       wcev
434600131105     c                   clear                   ds2a
434700131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
434800131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
434900131105     C                             :rpyEsito)
435000131105     c                   if        §2aflg = '§' and §2acpt = 'S'
435100131105     c                   move      'T  '         wcev
435200131105     c                   endif
435300131105     c                   endif
435400131105     c* ??? £6
435500131105     c                   if        §pctdata > 0
435600131105     c                   move      §PCTDATA      WDAT
435700131105     c                   movel     §PCTORA       WORA
435800131105     c                   endif
435900131105     c                   end
436000131105     c* ho trovato un evento da inserire  al 1° posto
436100131105     c                   if        wcev <> ' '
436200131105     c                   EXSR      deccevsta
436300131105     c                   IF        werr <> '1'                                  *no errore
436400131105     c                   EXSR      decorg
436500131105     c                   EXSR      edtdat
436600131105     c                   EXSR      edtora
436700131105     c* libero il 1° elemnto di schiera
436800150612     c                   eval      j = 49
436900150612     c                   for       j = 49 downto 1
437000131105     c                   if        cev(j) <> ' '
437100131105     c                   eval      cev(j+1) = cev(j)
437200131105     c                   eval      dev(j+1) = dev(j)
437300131105     c                   eval      hev(j+1) = hev(j)
437400131105     c                   eval      fle(j+1) = fle(j)
437500131105     c                   eval      eve(j+1) = eve(j)
437600131105     c                   end
437700131105     c                   endfor
437800131105     c                   eval      dev(1) = wdate
437900131105     c                   eval      hev(1) = worae
438000131105     c                   eval      cev(1)= wcev
438100131105     c                   eval      fle(1) = wodes
438200131105     c                   eval      eve(1) = wcdex
438300131105     c                   EVAL      cntEveO74 += 1
438400131105     c                   end
438500131105     c                   end
438600131105     c                   endsr
438700131105     ***********************************************************************************************
438800130312     **
438900130312     ** Reperisco tabella da TNTBE00F.
439000130312     **
439100130312     ***********************************************************************************************
439200130312     C     getTntbe00f   BEGSR
439300060622     C                   EVAL      t02Lin = langTntbe
439400060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
439500060622     C                   SELECT
439600060622     C                   WHEN      t02Cod = 'ICE'
439700060622     C                   EVAL      dice = t02Uni
439800060622     C                   WHEN      t02Cod = 'CCH'
439900060622     C                   EVAL      dcch = t02Uni
440000060622     C                   WHEN      t02Cod = 'NTW'
440100060622     C                   EVAL      dntw = t02Uni
440200100119     C                   WHEN      t02Cod = 'I1A'
440300100119     C                   EVAL      dI1a = t02Uni
440400150304     C                   WHEN      t02Cod = 'SPC'
440500150304     C                   EVAL      dSPC = t02Uni
440600060622     C                   ENDSL
440700060622     C                   ENDSR
440800060608
440900140529      /free
441000140529       //--------------------------------------------------------------
441100140529       //?Controllo formale dei dati passati.
441200140529       //--------------------------------------------------------------
441300140603       BEGSR  Kontrolla;
441400140529
441500140529       //?OpCode
441600140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
441700140529           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
441800140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
441900140529           exsr RoutEnd;
442000140529         ENDIF;
442100140603           IF  prmRqsDataSize > 0;
442200140603           ELSE;
442300140603             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
442400140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
442500140603             exsr RoutEnd;
442600140603           ENDIF;
442700140529
442800140529       //?Formato e size RQS
442900140603           IF  prmRqsFormato = formatoi74;
443000140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
443100140529           ELSE;
443200140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
443300140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
443400140529             exsr RoutEnd;
443500140529           ENDIF;
443600140529         //?Formato e size RPY
443700140603           IF  prmRpyFormato = formatoo74;
443800140529           ELSE;
443900140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
444000140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
444100140529             exsr RoutEnd;
444200140529           ENDIF;
444300140529           IF  prmRpyDataSize > 0;
444400140529           ELSE;
444500140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
444600140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
444700140529             exsr RoutEnd;
444800140529           ENDIF;
444900140616       //?Formato e size MSG
445000140617         if %parms > 9 ;
445100140617          IF  prmRpyFormMsg = formato;
445200140617          ELSE;
445300140616           prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
445400140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
445500140616           exsr RoutEnd;
445600140617          ENDIF;
445700140617          IF  prmRpyMsgSize > 0;
445800140617          ELSE;
445900140616           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
446000140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
446100140616           exsr RoutEnd;
446200140617          ENDIF;
446300140617         ENDIF;
446400140529
446500140529       ENDSR;
446600140529
446700140616       //--------------------------------------------------------------
446800140617       //?Routine per schierare i messaggi nella ds tis778dsm
446900140616       //--------------------------------------------------------------
447000140616       BEGSR  Messaggi;
447100140616
447200140616       //?Se ho già caricato 99 messaggi esco
447300140616         IF  nrmsg = 99;
447400140616           exsr RoutEnd;
447500140616           clear nrmsg;
447600140616         ENDIF;
447700140616
447800140616         nrmsg += 1;
447900140616         skIdMsg(nrmsg) = wIdMsg;
448000140616         skIdCampo(nrmsg) = wIdCampo;
448100140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
448200140616         IF  wParm <> *blanks;
448300140616           skTextMsg (nrmsg) =
448400140616           rtvMsgLang(wIdMsg:langi74:wParm);
448500140616         ELSE;
448600140616           skTextMsg (nrmsg)=
448700140616           rtvMsgLang(wIdMsg:langi74);
448800140616         ENDIF;
448900140616
449000140616       ENDSR;
449100140616
449200140529       //--------------------------------------------------------------
449300140529       BEGSR  RoutEnd;
449400140530
449500140530
449600140530        if  esito <> '0' ;
449700140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
449800140530             else;
449900140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
450000140530        endif ;
450100140529
450200140530       //?ritorno
450300140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
450400140617         if %parms > 9 ;
450500140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
450600140617         endif;
450700140529         return;
450800140529
450900140529       ENDSR;
451000140529      /end-free
451100000000     c*--------------------------------------------------------------------------------------------*
451200000000     c* operazioni iniziali
451300000000     c*--------------------------------------------------------------------------------------------*
451400000000     c     *inzsr        BEGSR
451500000000     c*
451600000000     c* ricevimento parametri
451700140603     c*m   *ENTRY        PLIST
451800140529     c*m                 PARM                    esito
451900140529     c*m                 PARM                    tis174dsi
452000140529     c*m                 PARM                    tis174dso
452100000000     c*
452200000000     c* chiavi di lettura
452300000000     c     keytas        KLIST                                                  titas30c
452400000000     c                   KFLD                    kasaas                         -anno spedizione
452500000000     c                   KFLD                    kaslnp                         -linea partenza
452600000000     c                   KFLD                    kasnrs                         -serie
452700000000     c                   KFLD                    kasnsp                         -spedizione
452800140715     c     keytasf       KLIST                                                  titas30c
452900140715     c                   KFLD                    kasaasf                        -anno spedizione
453000140715     c                   KFLD                    kaslnpf                        -linea partenza
453100140715     c                   KFLD                    kasnrsf                        -serie
453200140715     c                   KFLD                    kasnspf                        -spedizione
453300130312     c     karb          KLIST                                                  titas30c
453400140716     c                   KFLD                    f_tasaas                         -anno spedizione
453500140716     c                   KFLD                    f_taslnp                         -linea partenza
453600140716     c                   KFLD                    f_tasnrs                         -serie
453700140716     c                   KFLD                    f_tasnsp                         -spedizione
453800130326     C     Kdst          KLIST
453900130326     C                   KFLD                    dstnpg
454000130326     C                   KFLD                    dstnfv
454100130326     C                   KFLD                    dstfgs
454200130326     c                   z-add     4             dstnpg
454300130326     C     Kcet          KLIST
454400130326     C                   KFLD                    arbLNA
454500130326     C                   KFLD                    pctNDC
454600130326     C                   KFLD                    arbpdc
454700131105     C                   KFLD                    TTRD              3
454800130312     C                   KFLD                    arbAAS
454900130312     C                   KFLD                    arbLNP
455000130312     C                   KFLD                    arbNRS
455100130312     C                   KFLD                    arbNSP
455200150403     C     KARP          KLIST
455300150403     C                   KFLD                    arbAAS
455400150403     C                   KFLD                    arbLNP
455500150403     C                   KFLD                    arbNRS
455600150403     C                   KFLD                    arbNSP
455700000000     c     keytaa        KLIST                                                  titaa30c
455800000000     c                   KFLD                    kaaaas                         -anno spedizione
455900000000     c                   KFLD                    kaalnp                         -linea partenza
456000000000     c                   KFLD                    kaanrs                         -serie
456100000000     c                   KFLD                    kaansp                         -spedizione
456200000000     c                   KFLD                    kaatrc                         -tipo anagrafica
456300000000     c     keyta4        KLIST                                                  tita430c
456400000000     c                   KFLD                    ka4aas                         -anno spedizione
456500000000     c                   KFLD                    ka4lnp                         -linea partenza
456600000000     c                   KFLD                    ka4nrs                         -serie
456700000000     c                   KFLD                    ka4nsp                         -spedizione
456800000000     c                   KFLD                    ka4trc                         -tipo riferimento
456900010925     c     ke2ta4        KLIST                                                  tita430c
457000010925     c                   KFLD                    ka4aas                         -anno spedizione
457100010925     c                   KFLD                    ka4lnp                         -linea partenza
457200010925     c                   KFLD                    ka4nrs                         -serie
457300010925     c                   KFLD                    ka4nsp                         -spedizione
457400000000     c     keyevb        KLIST                                                  fnevb01l
457500000000     c                   KFLD                    kvbaas                         -anno spedizione
457600000000     c                   KFLD                    kvblnp                         -linea partenza
457700000000     c                   KFLD                    kvbnrs                         -serie
457800000000     c                   KFLD                    kvbnsp                         -spedizione
457900010717     c     ke2evb        KLIST                                                  fnevb01l
458000010717     c                   KFLD                    kvbaas                         -anno spedizione
458100010717     c                   KFLD                    kvblnp                         -linea partenza
458200010717     c                   KFLD                    kvbnrs                         -serie
458300010717     c                   KFLD                    kvbnsp                         -spedizione
458400010717     c                   KFLD                    kvbdev                         -data evento
458500010717     c                   KFLD                    kvbhev                         -ora  evento
458600000000     c     keylbl        KLIST                                                  fnlbl01l
458700000000     c                   KFLD                    kblaan                         -anno seguente
458800000000     c                   KFLD                    kbllpn                         -linea seguente
458900000000     c                   KFLD                    kblnrn                         -serie seguente
459000000000     c                   KFLD                    kblnsn                         -spedizione seguente
459100000000     c     ke2lbl        KLIST                                                  fnlbl02l
459200000000     c                   KFLD                    kblaap                         -anno precedente
459300000000     c                   KFLD                    kbllpp                         -linea precedente
459400000000     c                   KFLD                    kblnrp                         -serie precedente
459500000000     c                   KFLD                    kblnsp                         -spediz.precedente
459600000000     c     keytbe        KLIST                                                  *tntbe01l
459700000000     c                   KFLD                    kbecod                          -tabella
459800000000     c                   KFLD                    kbeke1                          -chiave uno
459900000000     c                   KFLD                    kbeke2                          -chiave due
460000000000     c                   KFLD                    kbelin                          -lingua
460100000000     c                   KFLD                    kbesif                          -s.informativo
460200000000     c     keyvss        KLIST                                                  *tivss02l
460300000000     c                   KFLD                    kssksu                          -cl.unificante
460400000000     c                   KFLD                    kssisv                          -tipo servizio
460500010717     c     keyvtd        KLIST                                                  *tivtd01l
460600010717     c                   KFLD                    ktdksu                          -cl.unificante
460700000000     c     keycsb        KLIST                                                  *tncsb03l
460800000000     c                   KFLD                    ksbaas                         -anno spedizione
460900000000     c                   KFLD                    ksblnp                         -linea partenza
461000000000     c                   KFLD                    ksbnrs                         -serie
461100000000     c                   KFLD                    ksbnsp                         -spedizione
461200040906     c     keydct        KLIST
461300040906     c                   KFLD                    DCTAAC
461400040906     c                   KFLD                    DCTFIL
461500040906     c                   KFLD                    DCTNCA
461600000000     c     keygcp        KLIST                                                  *figcp01l
461700000000     c                   KFLD                    kcpaas                         -anno spedizione
461800000000     c                   KFLD                    kcplnp                         -linea partenza
461900000000     c                   KFLD                    kcpnrs                         -serie
462000000000     c                   KFLD                    kcpnsp                         -spedizione
462100000000     c                   KFLD                    kcpfrg                         -prg aperture
462200050404     c     K04GCP51      KLIST
462300050404     c                   KFLD                    kcpaas                         -anno spedizione
462400050404     c                   KFLD                    kcplnp                         -linea partenza
462500050404     c                   KFLD                    kcpnrs                         -serie
462600050404     c                   KFLD                    kcpnsp                         -spedizione
462700000000     c     keygnp        KLIST                                                  *fignp01l
462800000000     c                   KFLD                    knpagc                         -anno apertura
462900000000     c                   KFLD                    knpfgc                         -filiale apertura
463000000000     c                   KFLD                    knpngc                         -numero giacenza
463100000000     c                   KFLD                    knpfrg                         -prg apertura
463200040531     c     keyar5        KLIST                                                  *fiar531c
463300030206     c                   KFLD                    kr5aas                         -anno spedizione
463400030206     c                   KFLD                    kr5lnp                         -linea partenza
463500030206     c                   KFLD                    kr5nrs                         -serie
463600030206     c                   KFLD                    kr5nsp                         -spedizione
463700030206     c                   KFLD                    kr5trd                         -tipo record
463800040907
463900000000     c* apre i files
464000000000     c                   OPEN      titas30c
464100000000     c                   OPEN      titaa30c
464200000000     c                   OPEN      tita430c
464300000000     c                   OPEN      fnevb01l
464400000000     c                   OPEN      tivss02l
464500010716     c                   OPEN      tivtd01l
464600000000     c                   OPEN      azorg01l
464700000000     c                   OPEN      fnlbl01l
464800000000     c                   OPEN      fnlbl02l
464900050309     c                   OPEN      tigcp51l
465000050309     c                   OPEN      tignp01l
465100000000     c                   OPEN      tncsb03l
465200040906     c                   OPEN      fndct01l
465300040531     c                   OPEN      fiar531c
465400130312     c                   OPEN      fnarb01l
465500150403     c                   OPEN      fiarp01l
465600140606     c                   OPEN      fiar501l
465700140606     c                   OPEN      fiar401l
465800130326     c                   OPEN      fidst01l
465900130312     c                   OPEN      fipct02l
466000060621     C                   CALLP     openTabel00f()
466100000000     c*
466200000000     c* caricamento tabelle occorrenti
466300000000     c                   EXSR      cartab
466400060621     C                   CALLP     inzLingue()
466500000000     c*
466600000000     c                   ENDSR
466700070109
466800070109     ***********************************************************************************************
466900070109     **
467000070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
467100070109     **
467200070109     ***********************************************************************************************
467300070109     C     eventoAnnulla BEGSR
467400070109
467500070109     C                   IF        §iceEvAn <> *BLANK
467600070109     C                   FOR       j = 1 TO %ELEM(smCev)
467700070109     C                   IF        §iceEvAn = smCev(j) AND
467800070109     C                             evbDevHev = smDevHev(j)
467900070109     C                   CLEAR                   smCev(j)
468000130312     C                   CLEAR                   smspe(j)
468100070109     C                   CLEAR                   smDevHev(j)
468200070109     C                   CLEAR                   smi(j)
468300070109     C                   CLEAR                   smSeql(j)
468400070109     C                   CLEAR                   smSeqe(j)
468500070109     C                   CLEAR                   smDeve(j)
468600070109     C                   CLEAR                   smHeve(j)
468700070109     C                   CLEAR                   smFle(j)
468800070109     C                   CLEAR                   smDev(j)
468900070109     C                   LEAVE
469000070109     C                   ENDIF
469100070109     C                   ENDFOR
469200070109     C                   ENDIF
469300070109
469400070109     C                   ENDSR
469500080131
469600100119     P*--------------------------------------------------
469700100119     P* Procedure name: GetDescrizioneIncassoContrassegno
469800100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
469900100119     P*                          o del contrassegno.
470000100121     P* Returns:        Descrizione modalità incasso contrassegno
470100100119     P*--------------------------------------------------
470200100119     P GetDescrizioneIncassoContrassegno...
470300100119     P                 B
470400100119     D GetDescrizioneIncassoContrassegno...
470500100119     D                 PI            35A
470600100119
470700100121     C                   RESET                   tibs02ds
470800100121     C                   EVAL      t02Cod = 'I1A'
470900100121     C                   EVAL      t02Lin = langTntbe
471000100119     ** Il contrassegno è stato incassato
471100100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
471200100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
471300100121     C                   ELSE
471400100119     ** Il contrassegno non è stato incassato.
471500100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
471600100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
471700100119     C                             ar5Trd <> 'GEN'
471800100121     C                   EVAL      kr5Aas = csbAas
471900100121     C                   EVAL      kr5Lnp = csbLnp
472000100121     C                   EVAL      kr5Nrs = csbNrs
472100100121     C                   EVAL      kr5Nsp = csbNsp
472200100119     C                   EVAL      kr5Trd = 'GEN'
472300100119     C     keyAr5        CHAIN     fiar531c
472400100119     C                   IF        %FOUND()
472500100119     C                   EVAL      dAr5Gen = ar5Uni
472600100119     C                   ELSE
472700100119     C                   RESET                   dAr5Gen
472800100119     C                   RETURN                  *BLANK
472900100119     C                   ENDIF
473000100119     C                   ENDIF
473100100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
473200100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
473300100119     C                   ELSE
473400100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
473500100119     C                   ENDIF
473600100121     C                   ENDIF
473700100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
473800100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
473900100119     C                   IF        t02Err = 'E'
474000100119     C                   RETURN                  *BLANK
474100100119     C                   ENDIF
474200100120     C                   EVAL      dI1a = t02Uni
474300140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
474400140417     C                   RETURN                  dI1a.descrizi2
474500140417     C                   ENDIF
474600100119     C                   RETURN                  dI1a.descrizion
474700100119
474800100119     P GetDescrizioneIncassoContrassegno...
474900100119     P                 E
475000100119
475100110304
475200110304     P*--------------------------------------------------
475300110304     P* Procedure name: IsContrassegnoCompensato
475400110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
475500110304     P*                          to con una fattura.
475600110304     P* Returns:        *ON = Compensato
475700110304     P*--------------------------------------------------
475800110304     P IsContrassegnoCompensato...
475900110304     P                 B
476000110304     D IsContrassegnoCompensato...
476100110304     D                 PI              N
476200110304
476300110304     D retField        S               N   INZ(*OFF)
476400110304
476500110307     ** Il contrassegno non è stato rimborsato.
476600110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
476700110307     C                   RETURN    retField
476800110307     C                   ENDIF
476900110307
477000110307     C/EXEC SQL
477100110307     C+ SELECT '1'
477200110307     C+ INTO :retField
477300110307     C+ FROM tncsv00f
477400110307     C+ WHERE csvAas = :csbAas
477500110307     C+   AND csvLnp = :csbLnp
477600110307     C+   AND csvNrs = :csbNrs
477700110307     C+   AND csvNsp = :csbNsp
477800110307     C+   AND csvCav = 'COMP'
477900110307     C+ FETCH FIRST ROW ONLY
478000110307     C/END-EXEC
478100110304
478200110307     C                   SELECT
478300110307     C                   WHEN      sqlCode < 0
478400110307     C                   DUMP(A)
478500110307     C                   ENDSL
478600110304
478700110307     C                   RETURN    retField
478800110304
478900110304     P IsContrassegnoCompensato...
479000110304     P                 E
479100110304
479200120321
479300120321     P*--------------------------------------------------
479400120321     P* Procedure name: SetIdAssegnoMittente
479500120321     P* Purpose:        Imposta l'ID assegno mittente.
479600120321     P* Returns:        Esito.
479700120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
479800120321     P*                          segni mittente.
479900120321     P* Parameter:      priAbi => ABI assegno.
480000120321     P* Parameter:      priCab => CAB assegno.
480100120321     P*--------------------------------------------------
480200120321     P SetIdAssegnoMittente...
480300120321     P                 B
480400120321     D SetIdAssegnoMittente...
480500120321     D                 PI            10I 0
480600120321     D  priNumeroAssegno...
480700120321     D                                     LIKE(caNumO74)
480800120321     D  priAbi                             LIKE(caAbiO74)
480900120321     D  priCab                             LIKE(caCabO74)
481000120321
481100120321     D retField        S             10I 0 STATIC
481200120321
481300120321      /FREE
481400120321
481500120321       CLEAR retField;
481600120321
481700120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
481800120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
481900120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
482000120321       // In questo caso restituisco l'ID del primo assegno.
482100120321
482200120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
482300120321         RETURN retField;
482400120321       ENDIF;
482500120321
482600120321       EXEC SQL
482700120321         SELECT csmNra, csmAbi, csmCab
482800120321           INTO :priNumeroAssegno, :priAbi, :priCab
482900120321           FROM tncsm00f
483000120321           WHERE csmKey = :priNumeroAssegno
483100120321           FETCH FIRST 1 ROW ONLY
483200120321       ;
483300120321
483400120321       SELECT;
483500120321         WHEN sqlCode = 100;
483600120321           retField = sqlCode;
483700120321         WHEN sqlCode < *ZERO;
483800120321           DUMP(A);
483900120321           retField = sqlCode;
484000120321       ENDSL;
484100120321
484200120321       RETURN retField;
484300120321
484400120321      /END-FREE
484500120321     P SetIdAssegnoMittente...
484600120321     P                 E
484700120321
484800130513
484900130513     P*--------------------------------------------------
485000130513     P* Procedure name: GetFilUrl
485100130513     P* Purpose:        Restituisce l'URL della filiale.
485200130513     P* Returns:        URL filiale.
485300130513     P* Parameter:      piFiliale => ID filiale
485400130513     P*--------------------------------------------------
485500130513     P GetFilUrl       B
485600130513     D GetFilUrl       PI           256A
485700130513     D  piFiliale                     3P 0 CONST
485800130513
485900130513     C                   IF        piFiliale = *ZERO
486000130513     C                   RETURN    *BLANK
486100130513     C                   ENDIF
486200130513     C                   RESET                   tibs02ds
486300130513     C                   EVAL      t02Cod = 'UFI'
486400130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
486500130513     C                   EVAL      t02Lin = langTntbe
486600130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
486700130513     C                   IF        t02Err = 'E'
486800130513     C                   RETURN    *BLANK
486900130513     C                   ENDIF
487000130513     C                   RETURN    t02Uni
487100130513
487200130513     P GetFilUrl       E
487300130513
