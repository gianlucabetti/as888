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
016000000000     d*--------------------------------------------------------------------------------------------*
016100000000     d* Data structure
016200000000     d*--------------------------------------------------------------------------------------------*
016300150403     d dvpooraris    e ds                  inz
016400150403     d fipctcetds    e ds
016500140605     d fipctcords    e ds
016600090914     D cndizion      E DS                  QUALIFIED BASED(nullPtr)
016700090914     D azlin00f      E DS                  BASED(nullPtr)
016800060622     D tabel00f      E DS                  INZ
016900060622     D kpjba         E DS                  INZ
017000140605     D trulorsds     E DS                  INZ
017100140605     D trulor2ds     E DS                  INZ
017200140605     D trulor3ds     E DS                  INZ
017300150819     D Diore01       E DS                  INZ
017400140606     d dar5emd       e ds                  inz prefix(§)
017500140605     D tnsd99ds      E DS                  INZ
017600140606     D tibs02ds      E DS                  INZ
017700060622     D  t02Mod       E                     INZ('C')                             Controllo
017800090828     D tis7700i0     E DS                  QUALIFIED
017900090828     D                                     INZ(*EXTDFT)
018000090828     D tis7700o0     E DS                  QUALIFIED
018100090828     d*---
018200000000     d* Ridenominazione formato record -TITAS-
018300000000     d*---
018400000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
018500000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
018600000000     d                                     PREFIX(s_)
018700140715     d titasdssavF   E DS                  EXTNAME(titas00f)                    *depisito titas
018800140715     d                                     PREFIX(f_)
018900080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
019000080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
019100080131     D                                     BASED(titas000FigliaPtr)
019200080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
019300080131     D                                     BASED(titas000FigliaPtr)
019400080131     d*---
019500000000     d* Variabili riferite al data base
019600000000     d*---
019700141126     d kasaasm         S                   LIKE(tasaas)                         *lettura titas30c
019800141126     d kaslnpm         S                   LIKE(taslnp)
019900141126     d kasnrsm         S                   LIKE(tasnrs)
020000141126     d kasnspm         S                   LIKE(tasnsp)
020100140715     d kasaasf         S                   LIKE(tasaas)                         *lettura titas30c
020200140715     d kaslnpf         S                   LIKE(taslnp)
020300140715     d kasnrsf         S                   LIKE(tasnrs)
020400140715     d kasnspf         S                   LIKE(tasnsp)
020500000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
020600000000     d kaslnp          S                   LIKE(taslnp)
020700000000     d kasnrs          S                   LIKE(tasnrs)
020800000000     d kasnsp          S                   LIKE(tasnsp)
020900000000     d kastbl          S                   LIKE(tastbl)
021000000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
021100000000     d kaalnp          S                   LIKE(taalnp)
021200000000     d kaanrs          S                   LIKE(taanrs)
021300000000     d kaansp          S                   LIKE(taansp)
021400000000     d kaatrc          S                   LIKE(taatrc)
021500000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
021600000000     d ka4lnp          S                   LIKE(ta4lnp)
021700000000     d ka4nrs          S                   LIKE(ta4nrs)
021800000000     d ka4nsp          S                   LIKE(ta4nsp)
021900000000     d ka4trc          S                   LIKE(ta4trc)
022000000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
022100000000     d kvblnp          S                   LIKE(evblnp)
022200000000     d kvbnrs          S                   LIKE(evbnrs)
022300000000     d kvbnsp          S                   LIKE(evbnsp)
022400000000     d kvbdev          S                   LIKE(evbdev)
022500000000     d kvbhev          S                   LIKE(evbhev)
022600000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
022700000000     d kcplnp          S                   LIKE(gcplnp)
022800000000     d kcpnrs          S                   LIKE(gcpnrs)
022900000000     d kcpnsp          S                   LIKE(gcpnsp)
023000000000     d kcpfrg          S                   LIKE(gcpfrg)
023100000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
023200000000     d knpfgc          S                   LIKE(gnpfgc)
023300000000     d knpngc          S                   LIKE(gnpngc)
023400000000     d knpfrg          S                   LIKE(gnpfrg)
023500000000     d knpfas          S                   LIKE(gnpfas)
023600000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
023700000000     d ksblnp          S                   LIKE(csblnp)
023800000000     d ksbnrs          S                   LIKE(csbnrs)
023900000000     d ksbnsp          S                   LIKE(csbnsp)
024000000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
024100000000     d kbllpn          S                   LIKE(lbllpn)
024200000000     d kblnrn          S                   LIKE(lblnrn)
024300000000     d kblnsn          S                   LIKE(lblnsn)
024400000000     d kblaap          S                   LIKE(lblaap)
024500000000     d kbllpp          S                   LIKE(lbllpp)
024600000000     d kblnrp          S                   LIKE(lblnrp)
024700000000     d kblnsp          S                   LIKE(lblnsp)
024800000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
024900000000     d kctlnp          S                   LIKE(dctlnp)
025000000000     d kctnrs          S                   LIKE(dctnrs)
025100000000     d kctnsp          S                   LIKE(dctnsp)
025200040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
025300030206     d kr5lnp          S                   LIKE(ar5lnp)
025400030206     d kr5nrs          S                   LIKE(ar5nrs)
025500030206     d kr5nsp          S                   LIKE(ar5nsp)
025600030206     d kr5trd          S                   LIKE(ar5trd)
025700000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
025800000000     d kssisv          S                   LIKE(vssisv)
025900010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
026000000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
026100000000     d kbeke1          S                   LIKE(tbeke1)
026200000000     d kbeke2          S                   LIKE(tbeke2)
026300000000     d kbelin          S                   LIKE(tbelin)
026400000000     d kbesif          S                   LIKE(tbesif)
026500000000     d waas            S                   LIKE(tasaas)                         *di lavoro
026600000000     d wlnp            S                   LIKE(taslnp)
026700000000     d wnrs            S                   LIKE(tasnrs)
026800000000     d wnsp            S                   LIKE(tasnsp)
026900000000     d wfca            S                   LIKE(tasfca)
027000000000     d wfgc            S                   LIKE(tasfgc)
027100000000     d wfpr            S                   LIKE(dctfpr)
027200000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
027300000000     d deplpn          S                   LIKE(lbllpn)
027400000000     d depnrn          S                   LIKE(lblnrn)
027500000000     d depnsn          S                   LIKE(lblnsn)
027600010716     d depute          S                   LIKE(vtdute)
027700010716     d deppwd          S                   LIKE(vtdpwd)
027800000000     d lcca            S                   LIKE(tascca)                         *di lavoro
027900000000     d lduc            S                   LIKE(tasduc)
028000000000     d llnp            S                   LIKE(taslnp)
028100000000     d ldti            S                   LIKE(tasdti)
028200000000     d lhti            S                   LIKE(tashti)
028300000000     d ldrt            S                   LIKE(tasdrt)
028400000000     d llna            S                   LIKE(taslna)
028500150617     d ltfa            S                   LIKE(tastfa)
028600000000     d ldcm            S                   LIKE(tasdcm)
028700000000     d lhmc            S                   LIKE(tashmc)
028800060621     d langTabel       S                   LIKE(tblkut)
028900060621     d langTntbe       S                   LIKE(tbelin)
029000140108     d wDev            S                   LIKE(evbdev)
029100140108     d wHev            S                   LIKE(evbhev)
029200150611     d wDEE            S                   LIKE(d98dee)
029300140616     d wParm           s            512a   inz
029400150403     D w_dallemsg      S              5a
029500150403     D w_allemsg       S              5a
029600150403     D w_dalle         S               t
029700150403     D w_alle          S               t
029800150403     D w_dalleoser     S               t
029900150403     D w_alleoser      S               t
030000000000     d*---
030100000000     d* Ds
030200000000     d*---
030300000000      * controllo data
030400000000     d wlbda8          DS                  INZ                                  *controlla data
030500000000     d  g08dat                        8  0                                       -data dritta
030600000000     d  g08inv                        8  0                                       -data invertita
030700000000     d  g08err                        1                                          -errore
030800000000     d  g08tgi                        5  0                                       -giorni fra date
030900000000      * scomposizione data generica
031000000000     d dsdat           DS                  INZ                                  *data
031100000000     d  dsann                         4  0                                       -anno
031200000000     d  dsmes                         2  0                                       -mese
031300000000     d  dsgio                         2  0                                       -giorno
031400000000      * scomposizione data generica (anno + mese/giorno)
031500000000     d                 DS                  INZ                                  *data aa + mm/gg
031600000000     d  dsannB                 1      4  0                                       -anno
031700000000     d  dsmgsB                 5      8  0                                       -mese/giorno
031800000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
031900000000      * composizione data generica editata
032000000000     d dsdate          DS                  INZ                                  *data
032100000000     d  dsgioe                        2  0                                       -giorno
032200000000     d  dsvd1                         1                                          -'/'
032300000000     d  dsmese                        2  0                                       -mese
032400000000     d  dsvd2                         1                                          -'/'
032500000000     d  dsanne                        4  0                                       -anno
032600000000      * scomposizione ora generica
032700000000     d dsora           DS                  INZ                                  *ora
032800000000     d  dshh                          2  0                                       -ore
032900000000     d  dsmm                          2  0                                       -minuti
033000000000      * composizione ora generica editata
033100000000     d dsorae          DS                  INZ                                  *ora
033200000000     d  dshhe                         2  0                                       -ore
033300000000     d  dsvo1                         1                                          -':'
033400000000     d  dsmme                         2  0                                       -minuti
033500030206      * lettura FIAR5 - record bancali
033600030206     d dar5ban       E DS
033700030206      * lettura FIAR5 - record supermercati
033800030206     d dar5scr       E DS
033900040330     d DAR5GEN       E DS
034000040330     D                                     INZ
034100040330     D                                     PREFIX(GEN)
034200000000      * tipi servizio
034300000000     d ds5e          E DS
034400000000      * disposizioni giacenza
034500000000     d ds2d          E DS
034600000000      * causale eventi
034700000000     d dice          E DS
034800000000      * causali chiusura CA
034900000000     d dcch          E DS
035000100119     d dI1a          E DS                  QUALIFIED
035100000000      * consegne anomale
035200000000     d ds7o          E DS
035300130312      * eventi
035400130312     d ds2a          E DS
035500000000      * riferimenti
035600000000     d dta4a         E DS
035700070927     d dsbl4e        E DS
035800060627      * Estensione blp/arb tipo record "I"
035900060627     d dsbl4i        E DS
036000040913      * Addebito immagine POD
036100040913     d DLVA1         E DS
036200040913     D                                     INZ
036300000000      * lettura campo orgde3 di AZORG00F
036400000000     d og143         E DS
036500000000      * stati contrassegno
036600000000     d ds4s          E DS
036700141014      * particolarità consegna
036800141014     d ds7r          E DS
036900031217      * DS lettura campo TASFLO di TITAS
037000031217     d dtasflo       E DS
037100000000      * Ds ricerca cliente P.d.C.
037200000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
037300000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
037400000000     d indds         E DS                  EXTNAME(cnind00f) INZ
037500000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
037600000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
037700150304      * stato data prev cons
037800150304     d dspc          E DS
037900150312     d
038000150312     d xgiolavds     e ds
038100000000      * reperimento clienti da cliente unificante
038200090903     d*tibs10ds      E DS
038300090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
038400000000      * scomposizione codice spedizione -DS Input-
038500000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
038600000000     d  dslnpi74                      3                                          -linea partenza
038700000000     d  dsnrsi74                      2                                          -serie
038800000000     d  dsnspi74                      7                                          -spedizione
038900000000      * composizione codice spedizione -bolle-
039000000000     d                 DS
039100000000     d  dstaslnp               1      3  0                                       -linea partenza
039200000000     d  dstasnrs               4      5  0                                       -serie
039300000000     d  dstasnsp               6     12  0                                       -spedizione
039400000000     d dstasspe                1     12                                         *ds cod spedizione
039500000000      * composizione data + ora evento
039600080131     d                 DS                  INZ
039700080131     d  evbdev                 1      8  0                                       -data evento
039800080131     d  evbhev                 9     12  0                                       -ora  evento
039900080131     d evbDevHev               1     12  0                                      *ds data+ora evento
040000130312      * composizione spedizione evento
040100130312     d                 DS                  INZ
040200130312     d  evbaas                 1      4  0                                       -data evento
040300130312     d  evblnp                 5      7  0                                       -ora  evento
040400130312     d  evbnrs                 8      9  0                                       -ora  evento
040500130312     d  evbnsp                10     16  0                                       -ora  evento
040600130312     d dsevbspe                1     16  0                                      *ds data+ora evento
040700060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
040800000000      * ordinamento schiere eventi
040900000000     d                 DS
041000000000     d  dsseql                 1      3  0                                       -sequenza legame
041100000000     d  dsseqe                 4      6  0                                       -sequenza evento
041200000000     d  dsdevhev               7     18  0                                       -data + ora
041300000000     d  dsi                   19     21  0                                       -indice
041400000000     d dssort                  1     21  0                                      *ds cod spedizione
041500000000     d* Input routine XALLINEA
041600000000     d walfa7          S              7                                         *alfanumerico 7
041700000000     d walfa7bis       S              7                                         *alfanumerico 7
041800000000     d wzeri           S              7                                         *alfa con zeri
041900000000     d*---
042000000000     d* Variabili
042100000000     d*---
042200101104     d i               S             10I 0                                      *indici
042300101104     d ii              S             10I 0
042400101104     d j               S             10I 0
042500101104     d jj              S             10I 0
042600101104     d z               S             10I 0
042700101104     d s               S             10I 0
042800101104     d k               S             10I 0
042900101104     d o               S             10I 0
043000101104     d tote            S             10I 0                                      *totale eventi memor
043100000000     d seql            S              5  0                                      *sequenza legami
043200000000     d n14             S             14  0                                      *Numerico 14
043300000000     d n8              S              8  0                                      *Numerico 8
043400000000     d n7              S              7  0                                      *Numerico 7
043500000000     d n4              S              4  0                                      *Numerico 4
043600010927     d n3              S              3  0                                      *Numerico 3
043700000000     d a1              S              1                                         *Alfanumerico 1
043800000000     d a3              S              3                                         *Alfanumerico 3
043900000000     d a5              S              5                                         *Alfanumerico 5
044000010717     d a8              S              8                                         *Alfanumerico 8
044100000000     d a15             S             15                                         *Alfanumerico 15
044200000000     d a200            S            200                                         *Alfanumerico 200
044300000000     d a200bis         S            200                                         *Alfanumerico 200
044400010927     d a230            S            230                                         *Alfanumerico 230
044500000000     d a400            S            400                                         *Alfanumerico 400
044600000000     d datcor          S              8  0                                      *data corrente
044700000000     d datpre          S              8  0                                      *data precedente
044800000000     d oracor          S              6  0                                      *ora corrente
044900000000     d anncor          S              4  0                                      *anno corrente
045000000000     d annpre          S              4  0                                      *anno precedente
045100140617     d wIdMsg          s              7a
045200140617     d wIdCampo        s             10a
045300000000     d werr            S              1                                         *errore generico
045400000000     d wdat            S              8  0                                      *data (aaaammgg)
045500000000     d wdate           S             10                                         *data (gg/mm/aaaa)
045600000000     d wora            S              4  0                                      *ora  (hhmm)
045700000000     d worae           S              5                                         *ora  (hh:mm)
045800000000     d worg            S              3  0                                      *filiale
045900000000     d wodes           S             30                                         *descrizione filiale
046000000000     d wofl1           S              1                                         *flag italia/estero
046100150617     d wtasfl1         S              1                                         *flag italia/estero
046200020702     d wodpd           S              3                                         *flag filiale DPD
046300000000     d wcev            S              3                                         *causale evento
046400030130     d wcdex           S             30                                         *descrizione bar/pt
046500000000     d wpgm            S             20                                         *programma
046600000000     d wftc            S              1                                         *cons.partic.
046700140714     d wricons         S              1                                         *wrk per riconsegna
046800140715     d keyforzata      S              1                                         *wrk per riconsegna
046900000000     d wdftc           S             50                                         *descriz.cons.partic
047000000000     d d1wdftc         S             50                                         *descriz.cons.part.1
047100000000     d d2wdftc         S             50                                         *descriz.cons.part.2
047200000000     d wgcx            S              2                                         *turno chiusura
047300000000     d wdgcx           S             20                                         *descriz.turno chius
047400000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
047500000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
047600000000     d we              S              3                                         *descrizione 'e'
047700000000     d bollaorig       S              1                                         *bolla originaria
047800150310     d PDAavv          S              1                                         *evento AVV a PDA
047900150421     d PDAgia          S              1                                         *evento GIA a PDA
048000000000     d sostrit         S              1                                         sostituisce Ritiro
048100000000     d sostcon         S              1                                         sostituisce Consegna
048200000000     d newevb          S              1                                         *nuovo evento
048300000000     d lavdevhev       S             12  0                                      *data + ora
048400040913     D WrkCurDate      S               D                                        Data corrente
048500060620     D wrkDtInvDisp    S              8
048600060620     D wrkLnaItaEst    S              1
048700060620     D rpyOpCode       S             10A
048800060620     D rpyEsito        S             10I 0
048900060621     D nullPtr         S               *
049000080131     D titas000FigliaPtr...
049100080131     D                 S               *   INZ(%ADDR(titas000Figlia))
049200010716     d*---
049300010716     d* Variabili di controlli
049400010716     d*---
049500010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
049600010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
049700000000     d*---
049800000000     d* Variabili di memorizzazione
049900000000     d*---
050000000000     d mhcre           S              5                                         *consegna richiesta
050100000000     d mdcre           S             10
050200000000     d mtcre           S             50
050300000000     d mftce           S            100                                         *consegna partic.
050400000000     d mgcxe           S            100                                         *turno chiusura
050500000000     d mffd            S            100                                         *fermo deposito
050600010927     d mmncB           S             35                                         *NON consegna LAV
050700010927     d mmncC           S             35
050800010927     d mmncG           S             35                                         *NON consegna GIAC
050900010927     d mmncH           S             35
051000010927     d mmncN           S             35                                         *NON consegna NOFATT
051100010927     d mmncO           S             35
051200050701     d mmnc8           S             35
051300050701     d mmnc9           S             35
051400030206     d mtel            S             20                                         *N° tel destinatario
051500030206     d mban            S             35                                         *bancali
051600030207     d mscr            S            500                                         *supermercati
051700030210     d msdace          S             10
051800030210     d msorce          S              5
051900030210     d msdcre          S             10
052000030210     d mshcre          S              5
052100030210     d mstcre          S             30
052200030210     d msnom           S             50
052300030210     d mscrT           S            500    DIM(5)
052400030211     d wmscrT          S           2504
052500000000     d*---
052600000000     d* Schiere
052700000000     d*---
052800000000      * taabella clienti del cliente unificante
052900000000      * tabella tipi servizio
053000000000     d stsp            S              1    DIM(20)                              *tipi servizio
053100000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
053200000000      * tabella consegne anomale
053300030130     d scca            S              1    DIM(30)                              *consegna anomala
053400000000      * tabella disposizioni giacenza
053500000000     d sdis            S              1    DIM(10)                              *codice
053600000000     d sddis           S             20    DIM(10)                              *descrizione italian
053700000000      * tabella filiali
053800000000     d sorg            S              3  0 DIM(300)                             *filiale
053900000000     d sodes           S             30    DIM(300)                             *descrizione filiale
054000000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
054100020702     d sopt            S              3    DIM(300)                             *flag poste/no poste
054200020702     d sodpd           S              3    DIM(300)                             *flag DPD/no DPD
054300000000      * eventi memorizzati
054400000000     d smi             S              3  0 DIM(200)                             *indice
054500000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
054600000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
054700000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
054800000000     d smdeve          S             10    DIM(200)                             *data evento editata
054900000000     d smheve          S              5    DIM(200)                             *ora  evento editata
055000000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
055100000000     d smdev           S             30    DIM(200)                             *descrizione evento
055200000000     d smcev           S              3    DIM(200)                             *causale evento
055300130313     d smspe           S             16  0 DIM(200)                             *causale evento
055400000000      * eventi memorizzati x ordinamento
055500000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
055600000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
055700000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
055800000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
055900000000     d spdeve          S             10    DIM(200)                             *data evento editata
056000000000     d spheve          S              5    DIM(200)                             *ora  evento editata
056100000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
056200000000     d spdev           S             30    DIM(200)                             *descrizione evento
056300000000     d spcev           S              3    DIM(200)                             *causale evento
056400130313     d spspe           S             16  0 DIM(200)                             *causale evento
056500000000      * note giacenze memorizzate
056600051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
056700051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
056800000000      * note memorizzate
056900000000     d smnot           S            100    DIM(10)
057000030206     d*---
057100030206     d* definizioni per passaggio parametri
057200030206     d*---
057300000000     d esito           s              1
057400000000      * DS contenente i dati di INPUT
057500140603     d tis778dsi     e DS                  INZ(*EXTDFT)
057600050104     D  cAASI74                       4
057700050104     D                                     OVERLAY(AASI74)
057800000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
057900140530     d tis778dso     e DS                  INZ(*EXTDFT)
058000000000     d dev                           10    DIM(50) OVERLAY(dato74)
058100000000     d hev                            5    DIM(50) OVERLAY(orao74)
058200000000     d fle                           30    DIM(50) OVERLAY(opeo74)
058300041007     d eve                           30    DIM(50) OVERLAY(eveo74)
058400041007     d cev                            3    DIM(50) OVERLAY(cevo74)
058500000000     d nota                         100    DIM(10) OVERLAY(notao74)
058600130313     d spe             S             16  0 DIM(50)
058700040906
058800140616     d tis778dsM     e DS                  INZ(*EXTDFT)
058900140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
059000140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
059100140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
059200140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
059300140530      *
059400040906     D FIDN12DS      E DS
059500040906     D                                     INZ
059600040906     D  I12FEL       E
059700040906     D                                     INZ('E')
059800040906     D  I12FAN       E
059900040906     D                                     INZ('E')
060000040906     D  O12KeyAry                    14
060100040906     D                                     DIM(20)
060200040906     D                                     OVERLAY(O12KEY)
060300040906
060400040906     D O12KeyDS        DS
060500040906     D                                     INZ
060600040906     D  O12KeyAAC                     4S 0
060700040906     D  O12KeyFil                     3S 0
060800040906     D  O12KeyNCA                     7S 0
060900040906
061000020702      * DS lettura tabella "NTW"
061100020702     d DNTW          E DS
061200130326     d Ddstflr       E DS
061300041223
061400041223     D ChkCode         S             10U 0
061500041223     D nEsito          S              5I 0
061600051110     D esito10         S             10I 0
061700080131
061800100119     D*--------------------------------------------------
061900100119     D* Procedure name: GetDescrizioneIncassoContrassegno
062000100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
062100100119     D*                          o del contrassegno.
062200100121     D* Returns:        Descrizione modalità incasso contrassegno.
062300100119     D*--------------------------------------------------
062400100119     D GetDescrizioneIncassoContrassegno...
062500100119     D                 PR            35A
062600110304
062700110304     D*--------------------------------------------------
062800110304     D* Procedure name: IsContrassegnoCompensato
062900110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
063000110304     D*                          to con una fattura.
063100110304     D* Returns:        *ON = Compensato
063200110304     D*--------------------------------------------------
063300110304     D IsContrassegnoCompensato...
063400110304     D                 PR              N
063500120321
063600120321     D*--------------------------------------------------
063700120321     D* Procedure name: SetIdAssegnoMittente
063800120321     D* Purpose:        Imposta l'ID assegno mittente.
063900120321     D* Returns:        Esito.
064000120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
064100120321     D*                          segni mittente.
064200120321     D* Parameter:      priAbi => ABI assegno.
064300120321     D* Parameter:      priCab => CAB assegno.
064400120321     D*--------------------------------------------------
064500120321     D SetIdAssegnoMittente...
064600120321     D                 PR            10I 0
064700120321     D  priNumeroAssegno...
064800120321     D                                     LIKE(caNumO74)
064900120321     D  priAbi                             LIKE(caAbiO74)
065000120321     D  priCab                             LIKE(caCabO74)
065100130513
065200130513     D*--------------------------------------------------
065300130513     D* Procedure name: GetFilUrl
065400130513     D* Purpose:        Restituisce l'URL della filiale.
065500130513     D* Returns:        URL filiale.
065600130513     D* Parameter:      piFiliale => ID filiale
065700130513     D*--------------------------------------------------
065800130513     D GetFilUrl       PR           256A
065900130513     D  piFiliale                     3P 0 CONST
066000130513
066100130513
066200140527       //--------------------------------------------------------------
066300140527       //?Definizione prototipi.
066400140527       //--------------------------------------------------------------
066500140527      /copy gaitrasrc/srcprotopr,tis778R
066600140529      /copy gaitrasrc/srcconst,tis778R
066700140527       //--------------------------------------------------------------
066800140527       //?Definizione parametri programma.
066900140527       //--------------------------------------------------------------
067000140529     d tis778_GetBolla...
067100140527     d                 PI
067200140527     d prmRqsOpCode...
067300140527     d                               10I 0 CONST
067400140527     d prmRpyOpCode...
067500140527     d                               10I 0
067600140527     d prmRpyIdMsg...
067700140527     d                               10I 0
067800140527     d prmRqsFormato...
067900140527     d                               10A   CONST
068000140527     d prmRqsData...
068100140527     d                            32767A   OPTIONS(*VARSIZE)
068200140527     d prmRqsDataSize...
068300140527     d                               10I 0 CONST
068400140527     d prmRpyFormato...
068500140527     d                               10A   CONST
068600140527     d prmRpyData...
068700140527     d                            32767A   OPTIONS(*VARSIZE)
068800140527     d prmRpyDataSize...
068900140527     d                               10I 0 CONST
069000140527     d prmRpyFormMsg...
069100140527     d                               10A   CONST OPTIONS(*NOPASS)
069200140527     d prmRpyMessage...
069300140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
069400140527     d prmRpyMsgSize...
069500140527     d                               10I 0 CONST OPTIONS(*NOPASS)
069600080131
069700000000     i*--------------------------------------------------------------------------------------------*
069800000000     i* Input
069900000000     i*--------------------------------------------------------------------------------------------*
070000000000     i*---
070100000000     i* Indicatori lettura formati record -TITAS-
070200000000     i*---
070300000000     ititas000      01
070400000000     ititas010      02
070500000000     ititasp00      03
070600000000     c*--------------------------------------------------------------------------------------------*
070700000000     c* Main lines
070800000000     c*--------------------------------------------------------------------------------------------*
070900000000     c*
071000000000     c* operazioni iniziali -da fare sempre-
071100000000     c                   EXSR      rutinz
071200000000     c*
071300000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
071400000000if  1c                   IF        esito <> '1'                                 *no errori
071500100219     c*
071600100219     c                   MOVEL     dslnpi74      kaslnp
071700100219     c                   MOVEL     dsnrsi74      kasnrs
071800100219     c                   MOVEL     dsnspi74      kasnsp
071900100219     c*
072000111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
072100111103     c* restituisco errore.
072200111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
072300111103     c* e se non la trovo provo con l'anno precedente.
072400111103     c                   IF        aasI74 <> *ZERO
072500100219     c                   Z-ADD     aasI74        kasaas
072600100219     c     keytas        SETLL     titas30c                               99
072700111103     c                   IF        NOT *IN99
072800111103     c                   EVAL      esito = '1'
072900111103     c                   ENDIF
073000111103     c                   ELSE
073100000000     c                   Z-ADD     anncor        kasaas
073200000000     c     keytas        SETLL     titas30c                               99
073300000000if  2c                   IF        NOT *in99                                    *non trovato
073400000000     c                   Z-ADD     annpre        kasaas
073500000000     c     keytas        SETLL     titas30c                               99
073600000000if  3c                   IF        NOT *in99                                    *non trovato
073700000000     c                   MOVEL     '1'           esito                          *errore
073800000000e   3c                   ENDIF
073900000000e   2c                   ENDIF
074000100219     c                   ENDIF
074100140616     c                   if        esito = '1'
074200140617     c                   eval      widmsg = 'TIS0564'
074300140616     c                   eval      widcampo = 'NSPEDIZI74'
074400140616     c                   clear                   wparm
074500140616     c                   exsr      messaggi
074600140616     c                   endif
074700141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
074800141126     c                   eval      kasaasm = kasaas
074900141126     c                   eval      kaslnpm = kaslnp
075000141126     c                   eval      kasnrsm = kasnrs
075100141126     c                   eval      kasnspm = kasnsp
075200141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
075300140715      * ultima figlia per forzare i dati in output
075400140714     c                   exsr      cercafiglia
075500000000     c*
075600000000     c* se trovata, legge la bolla
075700000000if  2c                   IF        esito <> '1'
075800000000     C                   SETOFF                                       010203
075900140715     c     keytas        SETLL     titas30c
076000000000     c     keytas        READE     titas30c                               99    *no errore
076100000000if  3c                   IF        *in99                                        *non trovato
076200000000     c                   MOVEL     '1'           esito                          *errore
076300140617     c                   eval      widmsg = 'TIS0734'
076400140616     c                   eval      widcampo = 'NSPEDIZI74'
076500140616     c                   clear                   wparm
076600140616     c                   exsr      messaggi
076700000000x   3c                   ELSE                                                   *trovata
076800000000if  4c                   IF        *in01                                        *lettura titas000
076900000000     c                   EVAL      titasdssav = titasds
077000000000e   4c                   ENDIF
077100000000if  4c                   IF        *in02                                        *lettura titas010
077200000000     c                   EVAL      titasdssav = titasds
077300000000e   4c                   ENDIF
077400000000if  4c                   IF        *in03                                        *lettura titasp00
077500000000     c                   EVAL      titasdssav = titasds
077600000000e   4c                   ENDIF
077700141117      * se trovo la figlia ma ancora non è su titas
077800150617      * Se la bolla non è legata, nei campi della "figlia" ovvero F_  ci sono
077900150617      *  i campi della bolla originale passata
078000141117     c                   if        keyforzata <> *blank and
078100141117     c                             f_tasnsp = 0
078200141117     c                   eval      titasdssavf = titasdssav
078300141120     c                   else
078400141120     c                   clear                   keyforzata
078500141117     c                   endif
078600000000e   3c                   ENDIF
078700000000     c*
078800000000     c* controlla validità bolla
078900000000     c                   EXSR      chktas
079000000000     c*
079100030130     c* imposta i campi della DS di Output
079200000000if  3c                   IF        esito <> '1'                                 *no errore
079300140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
079400140714      * spedizione richiesta a video sempre
079500100225     c*
079600140716     c                   EVAL      dstaslnp = s_taslnp
079700140716     c                   EVAL      dstasnrs = s_tasnrs
079800140716     c                   EVAL      dstasnsp = s_tasnsp
079900140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
080000140723     c* chain ar5 con bolla richiesta a video
080100140723     c                   Z-ADD     s_tasaas      kr5aas
080200140723     c                   Z-ADD     s_taslnp      kr5lnp
080300140723     c                   Z-ADD     s_tasnrs      kr5nrs
080400140723     c                   Z-ADD     s_tasnsp      kr5nsp
080500100225     c                   MOVEL     'GEN'         kr5trd
080600100225     c     keyar5        CHAIN     fiar531c
080700100225     C                   IF        %FOUND
080800100225     C                   EVAL      DAR5GEN = AR5Uni
080900100225     C                   ELSE
081000100225     C                   RESET                   DAR5GEN
081100100225     C                   ENDIF
081200000000     c*
081300000000     c* imposta i campi della DS di Output dalle bolle
081400000000     c                   EXSR      impdsotas
081500000000     c*
081600000000     c* imposta i campi della DS di Output dagli eventi
081700000000     c                   EXSR      impdsoevb
081800000000     c*
081900000000     c* imposta i campi della DS di Output dal contrassegno
082000000000     c                   EXSR      impdsocsb
082100000000     c*
082200000000     c* imposta i campi della DS di Output dalla giacenza
082300000000     c                   EXSR      impdsogcp
082400000000     c*
082500000000     c* imposta i campi della DS di Output dalle note
082600000000     c                   EXSR      impdsonot
082700040330     c*
082800040330     c* imposta i campi della DS di Output del destinatario.
082900040330     c                   EXSR      ImpDSDest
083000010716     C*
083100010716     c* imposta i campi della DS di Output dalla firma DPD
083200010716     c                   EXSR      ImpdsoDPD
083300040913     C*
083400040913     c* imposta i campi della DS di Output di addebito immagine LDV.
083500040913     c                   EXSR      AddebitoLDV
083600000000e   3c                   ENDIF
083700000000e   2c                   ENDIF
083800140613e   1c                   ENDIF
083900140919      *riempie campi con riferimenti alla consegna
084000141016     c                   if        esito <> '1'
084100140919     c                   if        f_tasdcm > 0
084200140919     c                   movel     f_tasdcm      wdat
084300140919     c                   exsr      edtdat
084400140919     c                   eval      dtconmeo74  = wdate
084500140919     c                   MOVEL     f_tashmc      wora
084600140919     c                   EXSR      edtora
084700140919     c                   eval      orconmeO74  =  worae
084800140919     c                   eval      flgbolcO74  =  'S'
084900140919     c                   eval      flgboldO74  =  'N'
085000141014      *firmatario distinta PDA
085100141014     c                   clear                   firmato74
085200141015     c                   if        gen§ar5pdaco <> 'NO'
085300141014     c                   exsr      repfirmat
085400141014     c                   else
085500141014     c                   if        f_tasgma <> *blank
085600141015     C                   EVAL      tblKey = f_tasgma
085700141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
085800141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
085900141014     C                             :rpyOpCode:rpyEsito)
086000141014     C                   IF        rpyOpCode = 'FOUND'
086100141014     c                   if        §7r2fi = 'S'
086200141014     c                   exsr      repfirmat
086300141014     c                   endif
086400141014     c                   endif
086500141014     c                   endif
086600141014     c                   endif
086700141014      *else consegna
086800140919     c                   else
086900140919     c                   eval      flgbolcO74  =  'N'
087000140919     c                   if        F_tasndc > 0 and f_tasddc > 0
087100141119     c                             and f_tasddc <= datcor
087200140919     c                   eval      flgboldo74 = 'S'
087300140919     c                   else
087400140919     c                   eval      flgboldO74  =  'N'
087500140919     c                   endif
087600140919     c                   endif
087700150617     c
087800140613if  3c                   IF        esito <> '1'                                 *no errore
087900150615     c***********        IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
088000150615     c************                 and cev(49) = ' ' and cev(50) = ' ' or
088100150615     c
088200150615     c                   if        f_tasdcm = 0 and keyforzata = *blank
088300130312     c                   exsr      srarb
088400140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
088500140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
088600140605     c                   exsr      impdsoarb
088700140604     c                   endif
088800140613     c                   endif
088900140613     c                   endif
089000141016     c                   endif
089100000000     c*
089200140530     C                   exsr      routend
089300141014      *__________________________________________________________________
089400141014     c     repfirmat     begsr
089500141014      *__________________________________________________________________
089600141014     c                   Z-ADD     f_tasaas      ka4aas
089700141014     c                   Z-ADD     f_taslnp      ka4lnp
089800141014     c                   Z-ADD     f_tasnrs      ka4nrs
089900141014     c                   Z-ADD     f_tasnsp      ka4nsp
090000141014     c                   MOVEL     '1'           ka4trc
090100141014     c     keyta4        CHAIN     tita430c                           99
090200141014if  1c                   IF        NOT *in99
090300141014     c                   MOVEL     ta4not        firmato74
090400141014e   1c                   ENDIF
090500141014     c                   endsr
090600141015      *__________________________________________________________________
090700141015     c     repfirmatARB  begsr
090800141015      *__________________________________________________________________
090900141015      *firmatario da arb
091000141015     c                   Z-ADD     arbaas        ka4aas
091100141015     c                   Z-ADD     arblnp        ka4lnp
091200141015     c                   Z-ADD     arbnrs        ka4nrs
091300141015     c                   Z-ADD     arbnsp        ka4nsp
091400141015     c                   MOVEL     '1'           ka4trc
091500141015     c     keyta4        CHAIN     fiar401l                           99
091600141015if  1c                   IF        NOT *in99
091700141015     c                   MOVEL     ar4not        firmato74
091800141015e   1c                   ENDIF
091900141015     c                   endsr
092000000000     c*--------------------------------------------------------------------------------------------*
092100000000     c* controlla validità bolla
092200000000     c*--------------------------------------------------------------------------------------------*
092300000000     c     chktas        BEGSR
092400000000     c*
092500000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
092600000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
092700000000     c                   Z-ADD     s_tasmgs      dsmgsB
092800000000if  1c                   IF        dsdatB < datpre
092900000000     c                   MOVEL     '1'           esito                          *errore
093000000000e   1c                   ENDIF
093100030130     c* se l'esito è positivo, salva alcuni dati della bolla
093200150617     c*********          EVAL      savtsp = f_tastsp
093300000000     c*
093400140718     C                   EVAL      wOrg = f_tasLna
093500051222     C                   EXSR      setLnaItaEst
093600051222     C
093700000000     c                   ENDSR
093800000000     c*--------------------------------------------------------------------------------------------*
093900000000     c* imposta i campi della DS di Output dalle bolle e files correlati
094000000000     c*--------------------------------------------------------------------------------------------*
094100140604     c     impdsotas     BEGSR
094200140604     c*
094300140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
094400140604     c                   EVAL      lnaDescO74 = sodes(i)
094500140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
094600150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
094700150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
094800150116     c                   MOVE      s_tasmgs      aasmgsO74
094900150116      *
095000140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
095100140718     c                   MOVEL     f_taslna      lineaaro74
095200140604     c*
095300140718     c                   MOVEL     f_tastbl      a1
095400140718     c                   MOVEL     f_tastbl      tpporto74
095500140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
095600140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
095700140604x   1c                   ELSE                                                   *Pagamento del
095800140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
095900140604e   1c                   ENDIF
096000140716     c                   MOVEL     f_tastsp      tpservio74
096100140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
096200140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
096300140604     C                             rpyOpCode:rpyEsito)
096400140604     C                   EVAL      corro74 = §5EDes
096500140604     c*
096600140716     c                   EVAL      destino74 = f_tasrsd
096700140716     c                   EVAL      desindo74 = f_tasind
096800140716     c                   EVAL      descapo74 = f_tascad
096900140716     c                   EVAL      desloco74 = f_taslod
097000140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
097100150326if  1c                   IF        f_tasnzd  = *blanks
097200150326     c                   EVAL      desnazo74 = f_tasprd
097300150326x   1c                   ELSE
097400150326     c***                EVAL      desnazo74 = f_tasnzd
097500150326     c***                EVAL      desnzdo74 = f_tasnzd
097600150326     c* 26/03/15: Attiviamo le specifiche per provincia export
097700150326     c                   EVAL      desnazo74 = f_tasprd
097800150326     c                   EVAL      desnzdo74 = f_tasnzd
097900150326e   1c                   ENDIF
098000140604     c*
098100140716     c                   EVAL      collio74  = f_tasncl
098200140716     c                   EVAL      pesoo74   = f_taspkf
098300140716     c                   EVAL      volumeo74 = f_tasvlf
098400140716     c                   EVAL      naturao74 = f_tasnas
098500140604     c*
098600140716     c                   EVAL      fatnumo74  = f_tasnft
098700140716     c                   MOVEL     f_tasdft      wdat
098800140604     c                   EXSR      edtdat
098900140604     c                   EVAL      fatdatao74 = wdate
099000140604     c*
099100140716     c                   EVAL      disnumo74  = f_tasndc
099200140716c    c                   MOVEL     f_tasddc      wdat
099300140604     c                   EXSR      edtdat
099400140604     c                   EVAL      disdatao74 = wdate
099500140604     **?Immagine lettera di vettura.
099600140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
099700140604     C                   IF        LDVO74 <> 'N'
099800140604     c                   CLEAR                   dtasflo
099900140716     c                   MOVEL     f_tasflo      dtasflo
100000140604     c                   IF        §floiml <> *BLANK
100100140604     c                   EVAL      ldvo74 = 'S'
100200140604     C                   EVAL      SpImLDVO74 =
100300140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
100400140716     C                             %EDITC(f_taslnp:'X') +
100500140716     C                             %EDITC(f_tasnrs:'X') +
100600140716     C                             %EDITC(f_tasnsp:'X')
100700140604     **?Reperisco il check code della spedizione POD
100800140604     C                   EVAL      PChkCdeO74 =
100900140604     C                             GetSpeChkCde(
101000140716     C                             %EDITC(f_tasaas:'X'):
101100140716     C                             %EDITC(f_taslnp:'X') +
101200140716     C                             %EDITC(f_tasnrs:'X') +
101300140716     C                             %EDITC(f_tasnsp:'X'):
101400140604     C                             ChkCode:nEsito)
101500140604     C                   ENDIF
101600140604     C                   ENDIF
101700140604     c*
101800140716     c                   MOVE      f_tasccm      n4
101900140604if  1c                   IF        n4 <> 8888                                   *codificato
102000140604     c                   CLEAR                   bs69ds
102100140604     c                   CLEAR                   acods
102200140604     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
102300140716     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
102400140716     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
102500140604     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
102600140604     c                   PARM                    bs69ds
102700140604     c                   PARM                    acods
102800140604     c                   PARM                    indds
102900140604     c                   PARM                    clpds
103000140604     c                   PARM                    clsds
103100140604if  2c                   IF        o69err <> '1'                                *no errore
103200140604     c                   EVAL      mito74    = acorag
103300140604     c                   EVAL      mitindo74 = indvia
103400140604     c                   EVAL      mitloco74 = indcit
103500140604     c                   MOVEL     indsta        a3
103600140604if  3c                   IF        a3 = *blanks
103700140604     c                   EVAL      mitnazo74 = indprv
103800140604x   3c                   ELSE
103900140604     c                   EVAL      mitnazo74 = a3
104000140604e   3c                   ENDIF
104100140604if  3c                   IF        indcae <> *blanks
104200140604     c                   EVAL      mitcapo74 = indcae
104300140604X   3c                   ELSE
104400140604     c                   MOVEL     indcap        a5
104500140604     c                   EVAL      mitcapo74 = a5
104600140604e   3c                   ENDIF
104700140604e   2c                   ENDIF
104800140604x   1c                   ELSE                                                   *non codificato
104900140716     c                   Z-ADD     f_tasaas      kaaaas
105000140716     c                   Z-ADD     f_taslnp      kaalnp
105100140716     c                   Z-ADD     f_tasnrs      kaanrs
105200140716     c                   Z-ADD     f_tasnsp      kaansp
105300140604     c                   MOVEL     'M'           kaatrc
105400140604     c     keytaa        CHAIN     titaa30c                           99
105500140604if  2c                   IF        NOT *in99
105600140604     c                   EVAL      mito74    = taarsc
105700140604     c                   EVAL      mitindo74 = taaind
105800140604     c                   EVAL      mitcapo74 = taacap
105900140604     c                   EVAL      mitloco74 = taaloc
106000140604if  3c                   IF        taanaz = *blanks
106100140604     c                   EVAL      mitnazo74 = taaprv
106200140604x   3c                   ELSE
106300140604     c                   EVAL      mitnazo74 = taanaz
106400140604e   3c                   ENDIF
106500140604e   2c                   ENDIF
106600140604e   1c                   ENDIF
106700140604     c* 2a ragione sociale destinatario.
106800140604     c                   CLEAR                   tita4000
106900140604     c                   CLEAR                   tita4p00
107000140716     c                   Z-ADD     f_tasaas      ka4aas
107100140716     c                   Z-ADD     f_taslnp      ka4lnp
107200140716     c                   Z-ADD     f_tasnrs      ka4nrs
107300140716     c                   Z-ADD     f_tasnsp      ka4nsp
107400140604     c                   MOVEL     'D'           ka4trc
107500140604     c     keyta4        CHAIN     tita430c                           99
107600140604if  1c                   IF        NOT *in99
107700140604     c                   MOVEL     ta4not        desti2O74
107800140604e   1c                   ENDIF
107900140604     c* rif partner estero
108000140604     c                   CLEAR                   tita4000
108100140604     c                   CLEAR                   tita4p00
108200140604     c                   CLEAR                   dsbl4e
108300140716     c                   Z-ADD     f_tasaas      ka4aas
108400140716     c                   Z-ADD     f_taslnp      ka4lnp
108500140716     c                   Z-ADD     f_tasnrs      ka4nrs
108600140716     c                   Z-ADD     f_tasnsp      ka4nsp
108700140604     c                   MOVEL     'E'           ka4trc
108800140604     c     keyta4        CHAIN     tita430c                           99
108900140604if  1c                   IF        NOT *in99
109000140604     c                   MOVEL     ta4not        dsbl4e
109100140604e   1c                   ENDIF
109200140604     c
109300140604     c                   CLEAR                   tita4000
109400140604     c                   CLEAR                   tita4p00
109500140604     c                   CLEAR                   dta4a
109600140716     c                   Z-ADD     f_tasaas      ka4aas
109700140716     c                   Z-ADD     f_taslnp      ka4lnp
109800140716     c                   Z-ADD     f_tasnrs      ka4nrs
109900140716     c                   Z-ADD     f_tasnsp      ka4nsp
110000140604     c                   MOVEL     'A'           ka4trc
110100140604     c     keyta4        CHAIN     tita430c                           99
110200140604if  1c                   IF        NOT *in99
110300140604     c                   MOVEL     ta4not        dta4a
110400140604e   1c                   ENDIF
110500140604     ** Riferimento mittente numerico (c'è sempre).
110600140716     C                   EVAL      rifero74 = f_tasrmn
110700140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
110800140604     C                   IF        §ta4arma <> *BLANK AND
110900140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
111000140604     C                   EVAL      rifero74a = §ta4arma
111100140604     C                   ENDIF
111200140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
111300140604     C                   IF        §b4erp <> *BLANK AND
111400140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
111500140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
111600140604     C                   EVAL      rpto74 = §b4erp
111700140604     C                   ENDIF
111800140604
111900140604     **?Reperisco il check code della spedizione.
112000140604     C                   EVAL      SChkCdeO74 =
112100140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
112200140604     C                             NSpedizI74:ChkCode:nEsito)
112300140604
112400140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
112500140604     C                   IF        KSCI74 <> *BLANK
112600140604     c                   SETOFF                                           98
112700140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
112800140604     C                   RESET                   tis7700i0
112900141126     C                   EVAL      tis7700i0.aas = kasaasm
113000141126     C                   EVAL      tis7700i0.lnp = kaslnpm
113100141126     C                   EVAL      tis7700i0.nrs = kasnrsm
113200141126     C                   EVAL      tis7700i0.nsp = kasnspm
113300140604     C                   EVAL      tis7700i0.ksu = kscI74
113400140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
113500140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
113600140604     C                             : rpyEsito
113700140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
113800140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
113900140604     C                             )
114000140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
114100140604     C                             tis7700o0.bollValida = *ON)
114200140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
114300140604if  2c                   IF        NOT *in98                                      *non trovato
114400140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
114500140604     C                   reset                   TIS778DSO
114600140604     C                   EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
114700140604     C                   RETURN
114800140604     C                   ENDIF
114900140604e   2c                   ENDIF
115000140604     C                   ENDIF
115100140604     c*
115200140604     ** Importo assicurato.
115300140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
115400140716     C                   EVAL      vasO74 = f_tasVas
115500141015     C                   EVAL      iasO74 = f_tasIas
115600140604     C                   ENDIF
115700140604     **
115800140604     c                   ENDSR
115900000000     c*--------------------------------------------------------------------------------------------*
116000000000     c* imposta i campi della DS di Output dagli eventi
116100000000     c*--------------------------------------------------------------------------------------------*
116200000000     c     impdsoevb     BEGSR
116300000000     c*
116400000000     c* azzera le variabili di lavoro
116500000000     c                   CLEAR                   tote                           *contatore eventi
116600000000     c                   CLEAR                   seql                           *sequenza legami
116700000000     c                   CLEAR                   spsort                         *per ordinamento
116800000000     c                   CLEAR                   spseql                         *di memorizzazione
116900000000     c                   CLEAR                   spseqe
117000000000     c                   CLEAR                   spdevhev
117100000000     c                   CLEAR                   spdeve
117200000000     c                   CLEAR                   spheve
117300000000     c                   CLEAR                   spfle
117400000000     c                   CLEAR                   spdev
117500000000     c                   CLEAR                   spcev
117600130312     c                   CLEAR                   spspe
117700150618     c                   CLEAR                   ds7o
117800000000     c*
117900000000     c* imposta la sequenza legami per la bolla di Input
118000000000     c                   Z-ADD     1             seql                           *primo legaqme
118100000000     c*---
118200000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
118300000000     c*---
118400000000     c***
118500000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
118600000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
118700000000     c*   si trasforma nella consegna anomala della mamma
118800000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
118900000000     c***
119000000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
119100000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
119200000000     c                   EVAL      kblaan = s_tasaas
119300000000     c                   EVAL      kbllpn = s_taslnp
119400000000     c                   EVAL      kblnrn = s_tasnrs
119500000000     c                   EVAL      kblnsn = s_tasnsp
119600000000     c     keylbl        CHAIN     fnlbl01l                           99
119700000000if  1c                   IF        NOT *in99                                    *ha la mamma
119800000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
119900000000     c                   EVAL      kasaas = lblaap
120000000000     c                   EVAL      kaslnp = lbllpp
120100000000     c                   EVAL      kasnrs = lblnrp
120200000000     c                   EVAL      kasnsp = lblnsp
120300000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
120400000000if  2c                   IF        NOT *in99 AND
120500000000     c                             tascca <> *blanks                            *ha cons.anomala
120600060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
120700060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
120800060622     C                             :rpyEsito)
120900060622if  3c*                  IF        *in98                                        *trovata
121000060626     C                   IF        rpyOpCode = 'FOUND'
121100041222     **?Consegna anomala, POD non visualizzabile.
121200060622     C*                  IF        sapod(jj) = 'N'
121300060622     C                   IF        §7OPODImg = 'N'
121400041222     C                   EVAL      LDVO74 = 'N'
121500041222     C                   ENDIF
121600060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
121700060622     C                   IF        §7oinc <> *blanks                            *ok per internet
121800000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
121900041222e   3c                   ENDIF
122000000000e   3c                   ENDIF
122100000000e   2c                   ENDIF
122200000000e   1c                   ENDIF
122300000000     c*
122400030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
122500000000     c                   ADD       1             tote
122600060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
122700060622     c                   MOVEL     §7odei        smdev(tote)
122800030203x   3c                   ELSE                                                   *evento telef.P.O.
122900060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
123000030203e   3c                   ENDIF
123100030203e   2c                   ENDIF
123200030203     c*
123300030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
123400060622     c                   MOVEL     §7ocev        wcev                           *causale evento
123500000000     c                   EXSR      evbrit                                       *imposta evento
123600030203x   2c                   ELSE                                                   *NO sost.ritiro
123700030203if  3c                   IF        s_tasdrt > *zeros
123800000000     c                   MOVEL     '701'         wcev                           *causale evento
123900000000     c                   EXSR      deccevsta
124000030203if  4c                   IF        werr <> '1'                                  *no errore
124100000000     c                   ADD       1             tote
124200030130     c                   MOVEL     wcdex         smdev(tote)
124300000000     c                   EXSR      evbrit                                       *imposta evento
124400030203e   4c                   ENDIF
124500030203e   3c                   ENDIF
124600030203e   2c                   ENDIF
124700140710     ** ID bolla madre.                                                          dubito figlia
124800080131     c                   EVAL      waas = s_tasaas
124900080131     c                   EVAL      wlnp = s_taslnp
125000080131     c                   EVAL      wnrs = s_tasnrs
125100080131     c                   EVAL      wnsp = s_tasnsp
125200000000     c* Partenza merce
125300000000     c                   EVAL      lduc = s_tasduc                              *data partenza
125400000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
125500000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
125600000000     c                   EXSR      evbpar
125700000000     c* Arrivo merce
125800110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
125900110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
126000140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
126100140723     c                             gen§ar5ArrDt <> *ZERO
126200140723     c                   MONITOR
126300140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
126400140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
126500140723     c                   ON-ERROR
126600140723     c                   ENDMON
126700140723     c                   ENDIF
126800100225     c                   EVAL      llna = s_taslna                              *linea arrivo
126900150617     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
127000090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
127100090826     c                   EXSR      evbarr
127200000000     c*
127300000000     c* Consegna merce
127400000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
127500000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
127600000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
127700000000     c                   EVAL      llna = s_taslna                              *linea arrivo
127800150618     c                   EVAL      ltfa = s_tastfa                              *term  arrivo
127900000000     c                   EXSR      evbcon
128000000000     c* Lettera anomalia
128100000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
128200000000     c                   EXSR      memdct
128300000000e   1c                   ENDIF
128400000000     c*---
128500000000     c* memorizza gli eventi dal file eventi della bolla di input
128600000000     c*---
128700150618     c* devo reimpostare la linea di arrivo della bolla riicheista, perchè potrei
128800150618     c*  aver chainato una bolla "mamma" ed aver sporcato il campo
128900150618     c                   eval      taslna=s_taslna
129000000000     c                   EXSR      memevb
129100000000     c*---
129200000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
129300000000     c*---
129400000000     c                   EXSR      elaevelbl
129500000000     c*
129600000000     c* imposta contatore eventi da visualizzare nella DS di Output
129700000000     c                   EVAL      cnteveo74 = tote
129800000000     c*---
129900000000     c* ordina gli eventi per sequenza legami / Data+ora evento
130000000000     c*---
130100000000     c* compone la schiera da ordinare
130200000000do  1c     1             DO        200           j
130300000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
130400000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
130500000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
130600000000     c                   Z-ADD     smi(j)        dsi                             -indice
130700000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
130800000000e   1c                   ENDDO
130900000000     c*
131000000000     c* ordina la schiera composta
131100000000     c                   SORTA     spsort
131200000000     c*
131300000000     c* ricompone le schiere da quella ordinata
131400000000     c                   Z-ADD     *zeros        jj
131500000000do  1c     1             DO        200           j
131600000000if  2c                   IF        spsort(j) >*zeros
131700000000     c                   ADD       1             jj
131800000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
131900000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
132000000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
132100000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
132200000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
132300000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
132400000000     c                   MOVEL     smheve(dsi)   spheve(jj)
132500000000     c                   MOVEL     smfle(dsi)    spfle(jj)
132600000000     c                   MOVEL     smdev(dsi)    spdev(jj)
132700000000     c                   MOVEL     smcev(dsi)    spcev(jj)
132800130312     c                   MOVEL     smspe(dsi)    spspe(jj)
132900000000e   2c                   ENDIF
133000000000e   1c                   ENDDO
133100101104     **
133200101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
133300101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
133400101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
133500101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
133600101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
133700101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
133800101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
133900101104     ** Ricordati che l'ordine per data e ora è discendente.
134000101104     C                   EVAL      j = 0
134100101104     C                   DOU       j = 0
134200101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
134300101104     C                                        : spcev : j+1)
134400101104     C                   IF        j = 0
134500101104     C                   LEAVE
134600101104     C                   ENDIF
134700101104     C                   IF        j > 1 AND
134800101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
134900101104     C                   EVAL      dev(1) = spdeve(j-1)
135000101104     C                   EVAL      hev(1) = spheve(j-1)
135100101104     C                   EVAL      fle(1) = spfle(j-1)
135200101104     C                   EVAL      eve(1) = spdev(j-1)
135300101104     C                   EVAL      cev(1) = spcev(j-1)
135400130312     C                   EVAL      spe(1) = spspe(j-1)
135500101104     C                   EVAL      spdeve(j-1) = spdeve(j)
135600101104     C                   EVAL      spheve(j-1) = spheve(j)
135700101104     C                   EVAL      spfle(j-1) = spfle(j)
135800101104     C                   EVAL      spdev(j-1) = spdev(j)
135900101104     C                   EVAL      spcev(j-1) = spcev(j)
136000130312     C                   EVAL      spspe(j-1) = spspe(j)
136100101104     C                   EVAL      spdeve(j) = dev(1)
136200101104     C                   EVAL      spheve(j) = hev(1)
136300101104     C                   EVAL      spfle(j) = fle(1)
136400101104     C                   EVAL      spdev(j) = eve(1)
136500101104     C                   EVAL      spcev(j) = cev(1)
136600130312     C                   EVAL      spspe(j) = spe(1)
136700101104     C                   ENDIF
136800101104     C                   ENDDO
136900000000     c*
137000120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
137100120831     c                   CLEAR                   cntEveO74
137200000000do  1c     1             DO        50            j
137300120831     c                   IF        spcev(j) = *BLANK
137400120831     c                   LEAVE
137500120831     c                   ENDIF
137600000000     c                   EVAL      dev(j) = spdeve(j)
137700000000     c                   EVAL      hev(j) = spheve(j)
137800000000     c                   EVAL      fle(j) = spfle(j)
137900041007     c                   EVAL      eve(j) = spdev(j)
138000041007     c                   EVAL      cev(j) = spcev(j)
138100130312     c                   EVAL      spe(j) = spspe(j)
138200101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
138300100909     c                   CLEAR                   hev(j)
138400100909     c                   ENDIF
138500120831     c                   EVAL      cntEveO74 += 1
138600000000e   1c                   ENDDO
138700000000     c*
138800000000     c                   ENDSR
138900000000     c*--------------------------------------------------------------------------------------------*
139000000000     c* memorizza gli eventi dal file eventi della bolla di input
139100000000     c*--------------------------------------------------------------------------------------------*
139200000000     c     memevb        BEGSR
139300150617     c* il programma usa il campo TASLNA per attribuire gli eventi della bolla
139400150617     c*  usa il TITAS che ha letto per ultimo che può essere quello della bolla  mamma
139500150617     c*  o della bolla figlia
139600150617     c*  per sapere se la bolla è italia o estera non devo usare l'ultima figlia
139700150617     c*  ma lna della bolla che sta leggendo in questo momento
139800150617     c                   Z-ADD     tasLna        worg
139900150617     c                   exsr      decorg
140000150617     c                   eval      wtasfl1 =  wofl1
140100000000     c***
140200000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
140300000000     c* . se SI lo aggiorna solo nella sequenza legame
140400000000     c*   se NO lo inserisce
140500000000     c***
140600000000     c                   Z-ADD     waas          kvbaas
140700000000     c                   Z-ADD     wlnp          kvblnp
140800000000     c                   Z-ADD     wnrs          kvbnrs
140900000000     c                   Z-ADD     wnsp          kvbnsp
141000000000     c     keyevb        SETLL     fnevb01l
141100000000     c     keyevb        READE     fnevb01l                               99
141200000000do  1c                   DOW       NOT *in99
141300000000if  2c                   IF        evbatb = *blanks                             *valido
141400140108     c                   MOVEL     evbdev        wdev
141500140108     c                   MOVEL     evbhev        whev
141600080131if  3c                   IF        evbDev > *zeros
141700000000     c                   MOVEL     evbcev        wcev
141800000000     c                   EXSR      deccevsta
141900070109     c                   EXSR      eventoAnnulla
142000000000if  4c                   IF        werr <> '1'                                  *no errore
142100000000     c*
142200000000     c* controlla se evento già inserito
142300000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
142400000000     c                   MOVEL     'S'           newevb                         *nuovo evento
142500000000do  5c     1             DO        200           j
142600000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
142700080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
142800000000     c                   MOVEL     'N'           newevb                         *già inserito
142900000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
143000150618     C                   IF        wtasfl1 = ESTERO
143100150618     c                   Z-ADD     tasLna        worg
143200150618     c                   else
143300150618     c                   Z-ADD     evbfle        worg
143400150618     c                   endif
143500000000     c                   EXSR      decorg
143600000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
143700080201     c                   MOVEL     wDev          wdat
143800080201     c                   EXSR      edtdat
143900080201     c                   MOVEL     wdate         smdeve(j)
144000080201     c                   MOVEL     wHev          wora
144100080201     c                   EXSR      edtora
144200080201     c                   MOVEL     worae         smheve(j)
144300130313     c                   MOVEL     dsevbspe      smspe(j)
144400000000     c                   LEAVE                                                  *uscita ciclo
144500000000e   6c                   ENDIF
144600000000e   5c                   ENDDO
144700000000     c*
144800000000     c* incrementa contatore eventi
144900000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
145000000000     c                   ADD       1             tote
145100000000     c* indice schiera
145200000000     c                   Z-ADD     tote          smi(tote)
145300000000     c* sequenza legame
145400000000     c                   Z-ADD     seql          smseql(tote)
145500000000     c* sequenza evento
145600000000     c                   Z-ADD     020           smseqe(tote)
145700000000     c* descrizione
145800030130     c                   MOVEL     wcdex         smdev(tote)
145900000000     c* data
146000080201     c                   MOVEL     wDev          wdat
146100000000     c                   EXSR      edtdat
146200000000     c                   MOVEL     wdate         smdeve(tote)
146300000000     c* ora
146400080201     c                   MOVEL     wHev          wora
146500000000     c                   EXSR      edtora
146600000000     c                   MOVEL     worae         smheve(tote)
146700000000     c* filiale
146800150617     C***                IF        wrkLnaItaEst = ESTERO
146900150617     C                   IF        wtasfl1 = ESTERO
147000140722     c                   Z-ADD     tasLna        worg
147100140722     c                   else
147200140722     c                   Z-ADD     evbfle        worg
147300140722     c                   endif
147400140722     c                   EXSR      decorg
147500140722     c                   MOVEL     wodes         smfle(tote)
147600000000     c* causale
147700000000     c                   MOVEL     evbcev        smcev(tote)
147800130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
147900130312     c* bolla legata
148000130312     c                   MOVEL     dsevbspe      smspe(tote)
148100000000     c* data+ora evento
148200080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
148300000000e   5c                   ENDIF
148400000000e   4c                   ENDIF
148500000000e   3c                   ENDIF
148600000000e   2c                   ENDIF
148700010717     c     keyevb        READE     fnevb01l                               99
148800000000e   1c                   ENDDO
148900000000     c*
149000000000     c                   ENDSR
149100000000     c*--------------------------------------------------------------------------------------------*
149200000000     c* memorizza gli "eventi fissi"
149300000000     c*--------------------------------------------------------------------------------------------*
149400000000     c     memevefix     BEGSR
149500080131     ** ID bolla in canna.
149600080131     c                   EVAL      waas = tasaas
149700080131     c                   EVAL      wlnp = taslnp
149800080131     c                   EVAL      wnrs = tasnrs
149900080131     c                   EVAL      wnsp = tasnsp
150000000000     c*
150100000000     c* Partenza merce
150200000000     c                   EVAL      lduc = tasduc                                *data partenza
150300000000     c                   EVAL      llnp = taslnp                                *filiale partenza
150400000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
150500000000     c                   EXSR      evbpar
150600000000     c* Consegna merce
150700000000     c                   EVAL      lcca = tascca                                *consegna anomala
150800000000     c                   EVAL      ldcm = tasdcm                                *data consegna
150900000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
151000000000     c                   EVAL      llna = taslna                                *linea arrivo
151100150618     c                   EVAL      ltfa = tastfa                                *linea arrivo
151200000000     c                   EXSR      evbcon
151300000000     c*
151400000000     c                   ENDSR
151500000000     c*--------------------------------------------------------------------------------------------*
151600000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
151700000000     c*--------------------------------------------------------------------------------------------*
151800000000     c     elaevelbl     BEGSR
151900000000     c*
152000000000     c* azzera le variabili di lavoro
152100000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
152200000000     c                   EVAL      deplpn = *zeros
152300000000     c                   EVAL      depnrn = *zeros
152400000000     c                   EVAL      depnsn = *zeros
152500000000     c*
152600000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
152700000000     c                   EVAL      kbllpp =  s_taslnp
152800000000     c                   EVAL      kblnrp =  s_tasnrs
152900000000     c                   EVAL      kblnsp =  s_tasnsp
153000000000     c*
153100000000     c* ciclo fino a fine mamme
153200000000     c     ke2lbl        SETLL     fnlbl02l
153300000000     c     ke2lbl        READE     fnlbl02l                               99
153400000000do  1c                   DOW       NOT *in99
153500000000     c*
153600000000     c* ciclo fino a fine figlie della mamma
153700000000do  2c                   DOW       NOT *in99
153800000000     c*
153900000000     c* legge la bolla dal legame
154000000000     c                   EVAL      kasaas = lblaan                              *figlia
154100000000     c                   EVAL      kaslnp = lbllpn
154200000000     c                   EVAL      kasnrs = lblnrn
154300000000     c                   EVAL      kasnsp = lblnsn
154400000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
154500000000if  3c                   IF        NOT *in99 AND                                *esistente
154600000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
154700000000     c                             tastbl <> 'A3' AND
154800000000     c                             tastbl <> 'F3' AND
154900000000     c                             tastbl <> 'AP' AND
155000000000     c                             tastbl <> 'FC'
155100041222     **?Immagine lettera di vettura.
155200041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
155300041222     C                   IF        LDVO74 <> 'N'
155400040330     c                   MOVEL     tasflo        dtasflo
155500040330     c                   IF        §floiml <> *BLANK
155600050325     c                   EVAL      ldvo74 = 'S'
155700040330     C                   EVAL      SpImLDVO74 =
155800040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
155900040330     C                             %EDITC(taslnp:'X') +
156000040330     C                             %EDITC(tasnrs:'X') +
156100040330     C                             %EDITC(tasnsp:'X')
156200041223     **?Reperisco il check code della spedizione POD
156300041223     C                   EVAL      PChkCdeO74 =
156400041223     C                             GetSpeChkCde(
156500041223     C                             %EDITC(tasaas:'X'):
156600041223     C                             %EDITC(taslnp:'X') +
156700041223     C                             %EDITC(tasnrs:'X') +
156800041223     C                             %EDITC(tasnsp:'X'):
156900041223     C                             ChkCode:nEsito)
157000040330     C                   ENDIF
157100041222     C                   ENDIF
157200000000     c* incrementa la sequenza legami per le bolle legate
157300000000     c                   ADD       1             seql                           *legaqme successivo
157400000000     c*---
157500000000     c* memorizza gli "eventi fissi"
157600000000     c*---
157700000000     c                   EXSR      memevefix
157800000000     c*---
157900000000     c* memorizza gli eventi
158000000000     c*---
158100000000     c                   EVAL      waas = lblaan                                *figlia
158200000000     c                   EVAL      wlnp = lbllpn
158300000000     c                   EVAL      wnrs = lblnrn
158400000000     c                   EVAL      wnsp = lblnsn
158500000000     c                   EXSR      memevb
158600000000     c*
158700000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
158800000000     c                   EVAL      depaan = lblaan                              *ultima figlia
158900000000     c                   EVAL      deplpn = lbllpn
159000000000     c                   EVAL      depnrn = lblnrn
159100000000     c                   EVAL      depnsn = lblnsn
159200000000e   3c                   ENDIF
159300000000     c*
159400000000     c* lettura successiva legami
159500000000     c     ke2lbl        READE     fnlbl02l                               99
159600000000e   2c                   ENDDO                                                  *fine figlie mamma
159700000000     c*
159800000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
159900000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
160000000000     c                             kbllpp <> deplpn OR
160100000000     c                             kblnrp <> depnrn OR
160200000000     c                             kblnsp <> depnsn
160300000000     c* nuova chiave
160400000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
160500000000     c                   EVAL      kbllpp =  deplpn
160600000000     c                   EVAL      kblnrp =  depnrn
160700000000     c                   EVAL      kblnsp =  depnsn
160800000000     c* controlla figlie
160900000000     c     ke2lbl        SETLL     fnlbl02l
161000000000     c     ke2lbl        READE     fnlbl02l                               99
161100000000if  3c                   IF        *in99                                        *non ha figlie
161200000000     c                   LEAVE                                                  *esce dal ciclo
161300000000e   3c                   ENDIF
161400000000x   2c                   ELSE                                                   *sempre stessa mamma
161500000000     c                   LEAVE                                                  *esce dal ciclo
161600000000e   2c                   ENDIF
161700000000e   1c                   ENDDO                                                  *fine mamme
161800000000     c*
161900000000     c                   ENDSR
162000000000     c*--------------------------------------------------------------------------------------------*
162100000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
162200000000     c*--------------------------------------------------------------------------------------------*
162300000000     c     evbrit        BEGSR
162400000000     c*
162500000000     c* indice schiera
162600000000     c                   Z-ADD     tote          smi(tote)
162700000000     c* sequenza legame
162800000000     c                   Z-ADD     seql          smseql(tote)
162900000000     c* sequenza evento
163000000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
163100000000     c* data
163200000000if  1c                   IF        s_tasdrt > *zeros
163300000000     c                   MOVEL     s_tasdrt      wdat
163400000000     c                   EXSR      edtdat
163500000000     c                   MOVEL     wdate         smdeve(tote)
163600000000e   1c                   ENDIF
163700000000     c* ora
163800000000if  1c                   IF        s_tashrt > *zeros
163900000000     c                   MOVEL     s_tashrt      wora
164000000000x   1c                   ELSE
164100000000if  2c                   IF        s_tasfpp = 'P'                               *PM
164200000000     c                   MOVEL     1800          wora
164300000000x   2c                   ELSE                                                   *AM
164400000000     c                   MOVEL     1200          wora
164500000000e   2c                   ENDIF
164600000000e   1c                   ENDIF
164700000000     c                   EXSR      edtora
164800000000     c                   MOVEL     worae         smheve(tote)
164900000000     c* filiale
165000000000     c                   Z-ADD     s_taslnp      worg
165100000000     c                   EXSR      decorg
165200150618     c
165300150618     c* Se la bolla è import e devo emettere l'evento di telefonare
165400150618     c*  non posso indciare la filiale esterna, ma l'hub italiano
165500150618     c                   if        wofl1=ESTERO and sostrit='S' and
165600150618     c                             §7oinc='T'
165700150618     c                   z-add     s_tastfp      worg
165800150618     c                   EXSR      decorg
165900150618     c                   endif
166000000000     c                   MOVEL     wodes         smfle(tote)
166100000000     c* causale
166200000000     c                   MOVEL     wcev          smcev(tote)
166300000000     c* data+ora evento
166400000000     c                   MOVEL     wdat          smdevhev(tote)
166500000000     c                   MOVE      wora          smdevhev(tote)
166600000000     c*
166700000000     c                   ENDSR
166800000000     c*--------------------------------------------------------------------------------------------*
166900000000     c* Imposta l'evento Partenza
167000000000     c*--------------------------------------------------------------------------------------------*
167100000000     c     evbpar        BEGSR
167200000000     c*
167300000000     c* controlla SE e COME inserire l'evento
167400000000if  1c                   IF        lduc > *zeros
167500000000     c* imposta causale
167600000000     c                   MOVEL     '702'         wcev
167700000000     c                   EXSR      deccevsta
167800000000if  2c                   IF        werr <> '1'                                  *no errore
167900000000     c* imposta filiale
168000000000     c                   Z-ADD     llnp          worg
168100000000     c                   EXSR      decorg
168200000000     c* imposta data
168300101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
168400000000     c                   MOVEL     ldrt          wdat                           *data ritiro
168500000000x   3c                   ELSE                                                   partenza ITALIA
168600000000     c                   MOVEL     lduc          wdat                           *data partenza
168700000000e   3c                   ENDIF
168800000000     c                   EXSR      edtdat
168900000000     c* imposta ora
169000000000     c                   MOVEL     2100          wora                           *ora imposta
169100000000     c                   EXSR      edtora
169200000000     c*
169300000000     c* controlla se l'evento è già stato inserito
169400000000     c                   MOVEL     'S'           newevb                         *nuovo evento
169500000000do  3c     1             DO        200           j
169600000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
169700000000     c                   MOVE      wora          lavdevhev
169800000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
169900000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
170000000000     c                   MOVEL     'N'           newevb                         *già inserito
170100000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
170200000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
170300000000     c                   LEAVE                                                  *uscita ciclo
170400000000e   4c                   ENDIF
170500000000e   3c                   ENDDO
170600000000     c*
170700000000     c* incrementa contatore eventi
170800000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
170900000000     c                   ADD       1             tote
171000000000     c* indice schiera
171100000000     c                   Z-ADD     tote          smi(tote)
171200000000     c* sequenza legame
171300000000     c                   Z-ADD     seql          smseql(tote)
171400000000     c* sequenza evento
171500000000     c                   Z-ADD     020           smseqe(tote)
171600000000     c* descrizione
171700030130     c                   MOVEL     wcdex         smdev(tote)
171800000000     c* filiale -impostata sopra-
171900000000     c                   MOVEL     wodes         smfle(tote)
172000000000     c* data    -impostata sopra-
172100000000     c                   MOVEL     wdate         smdeve(tote)
172200000000     c* ora     -impostata sopra-
172300000000     c                   MOVEL     worae         smheve(tote)
172400000000     c* causale -impostata sopra-
172500000000     c                   MOVEL     wcev          smcev(tote)
172600000000     c* data+ora evento
172700000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
172800000000     c                   MOVE      wora          smdevhev(tote)
172900000000e   3c                   ENDIF
173000000000e   2c                   ENDIF
173100000000e   1c                   ENDIF
173200000000     c*
173300000000     c                   ENDSR
173400101129     c*------------------------------------------------------------------------*
173500101129     c*
173600101129     c* Imposta l'evento Arrivo
173700101129     c*
173800101129     c*------------------------------------------------------------------------*
173900000000     c     evbarr        BEGSR
174000101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
174100101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
174200101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
174300101129     ** sommato inutile.
174400150618     C                   IF        wrkLnaItaEst = ESTERO
174500150618     C                   LEAVESR
174600150618     C                   ENDIF
174700101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
174800101129     C                   IF        lhti = *ZERO
174900101129     C                   LEAVESR
175000101129     C                   ENDIF
175100000000     c*
175200000000     c* controlla SE e COME inserire l'evento
175300000000if  1c                   IF        ldti > *zeros OR
175400000000     c                             ldcm > *zeros
175500000000     c* imposta causale
175600101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
175700000000     c                   EXSR      deccevsta
175800000000if  2c                   IF        werr <> '1'                                  *no errore
175900000000     c* imposta filiale
176000150617     c                   Z-ADD     llna          worg
176100000000     c                   EXSR      decorg
176200000000     c* imposta data
176300000000if  3c                   IF        ldti > *zeros
176400000000     c                   MOVEL     ldti          wdat                           *data arrivo
176500000000x   3c                   ELSE
176600000000     c                   MOVEL     ldcm          wdat                           *data consegna
176700000000e   3c                   ENDIF
176800000000     c                   EXSR      edtdat
176900000000     c* imposta ora
177000000000if  3c                   IF        lhti > *zeros
177100000000     c                   MOVEL     lhti          wora                           *ora arrivo
177200000000x   3c                   ELSE
177300000000     c                   MOVEL     0600          wora                           *ora imposta
177400000000e   3c                   ENDIF
177500000000     c                   EXSR      edtora
177600000000     c*
177700000000     c* controlla se l'evento è già stato inserito
177800000000     c                   MOVEL     'S'           newevb                         *nuovo evento
177900000000do  3c     1             DO        200           j
178000000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
178100000000     c                   MOVE      wora          lavdevhev
178200000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
178300000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
178400000000     c                   MOVEL     'N'           newevb                         *già inserito
178500000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
178600000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
178700000000     c                   LEAVE                                                  *uscita ciclo
178800000000e   4c                   ENDIF
178900000000e   3c                   ENDDO
179000000000     c*
179100000000     c* incrementa contatore eventi
179200000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
179300000000     c                   ADD       1             tote
179400000000     c* indice schiera
179500000000     c                   Z-ADD     tote          smi(tote)
179600000000     c* sequenza legame
179700000000     c                   Z-ADD     seql          smseql(tote)
179800000000     c* sequenza evento
179900000000     c                   Z-ADD     020           smseqe(tote)
180000000000     c* descrizione
180100030130     c                   MOVEL     wcdex         smdev(tote)
180200000000     c* data    -impostata sopra-
180300000000     c                   MOVEL     wdate         smdeve(tote)
180400000000     c* ora     -impostata sopra-
180500000000     c                   MOVEL     worae         smheve(tote)
180600000000     c* filiale -impostata sopra-
180700000000     c                   MOVEL     wodes         smfle(tote)
180800000000     c* causale -impostata sopra-
180900000000     c                   MOVEL     wcev          smcev(tote)
181000000000     c* data+ora evento
181100000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
181200000000     c                   MOVE      wora          smdevhev(tote)
181300000000e   3c                   ENDIF
181400000000e   2c                   ENDIF
181500000000e   1c                   ENDIF
181600000000     c*
181700000000     c                   ENDSR
181800000000     c*--------------------------------------------------------------------------------------------*
181900000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
182000000000     c*--------------------------------------------------------------------------------------------*
182100140714     c     evbcon        BEGSR
182200000000     c*
182300000000     c* azzera variabili di lavoro
182400000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
182500000000     c****
182600000000     c* Consegna merce ->
182700000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
182800000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
182900000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
183000000000     c*   nè lo stato di consegna nè quello di consegna anomala.
183100000000     c****
183200000000     c* controlla SE e COME inserire l'evento
183300000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
183400060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
183500060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
183600060622     C                             :rpyEsito)
183700060622     C                   IF        rpyOpCode = 'FOUND'
183800041222     **?Consegna anomala, POD non visualizzabile.
183900060622     C                   IF        §7OPODImg = 'N'
184000041222     C                   EVAL      LDVO74 = 'N'
184100041222     C                   ENDIF
184200060622     c                   IF        §7oinc <> *blanks                            *ok per internet
184300060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
184400000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
184500000000x   3c                   ELSE                                                   *ha record FNEVB
184600000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
184700000000e   3c                   ENDIF
184800041222e   3c                   ENDIF
184900000000e   2c                   ENDIF
185000000000e   1c                   ENDIF
185100080131     c* imposta data
185200080131if  1c                   IF        ldcm > *zeros
185300080131     c                   MOVEL     ldcm          wdat
185400080131e   1c                   ENDIF
185500080131     c* imposta ora
185600080131if  1c                   IF        lhmc > *zeros
185700080131     c                   MOVEL     lhmc          wora
185800080131e   1c                   ENDIF
185900000000     c* imposta causale
186000000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
186100060622     c                   MOVEL     §7ocev        wcev                           *causale evento
186200080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
186300080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
186400080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
186500080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
186600080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
186700080131     c* ritiro merce) dalla bolla figlia.
186800080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
186900080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
187000080131     c                             AND s_tasNdc < *HIVAL
187100080131     c                   EVAL      kblAap = wAas
187200080131     c                   EVAL      kblLpp = wLnp
187300080131     c                   EVAL      kblNrp = wNrs
187400080131     c                   EVAL      kblNsp = wNsp
187500080131     c     ke2lbl        CHAIN     fnlbl02l
187600080131     c                   IF        %FOUND()
187700080131     c                   EVAL      kasAas = lblAan
187800080131     c                   EVAL      kasLnp = lblLpn
187900080131     c                   EVAL      kasNrs = lblNrn
188000080131     c                   EVAL      kasNsp = lblNsn
188100080131     C     keyTas        CHAIN     titas000      titas000Figlia
188200080131     C                   IF        %FOUND()
188300080131     C                   EVAL      wDat = titas000Figlia.tasDrt
188400080131     C                   EVAL      wOra = titas000Figlia.tasHrt
188500080131     C                   ELSE
188600080131     C     keyTas        CHAIN     titas010      titas010Figlia
188700080131     C                   IF        %FOUND()
188800080131     C                   EVAL      wDat = titas010Figlia.tasDrt
188900080131     C                   EVAL      wOra = titas010Figlia.tasHrt
189000080131     C                   ELSE
189100080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
189200080131     C                   IF        %FOUND()
189300080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
189400080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
189500080131     C                   ENDIF
189600080131     C                   ENDIF
189700080131     C                   ENDIF
189800080131     c                   ENDIF
189900080131     c                   ENDIF
190000000000x   1c                   ELSE                                                   *NO sost.Consegna
190100000000     c                   MOVEL     '704'         wcev                           *causale evento
190200000000e   1c                   ENDIF
190300080131     c* Editazione data e ora evento.
190400080131     c                   EXSR      edtdat
190500080131     c                   EXSR      edtora
190600000000     c* imposta filiale
190700000000     c                   Z-ADD     llna          worg
190800000000     c                   EXSR      decorg
190900150618     c* se si tratta di lna estera e la bolla ha consegna anomala
191000150618     c*  in cui mettere msg generico, allora devo sostituire la filiale
191100150618     c*  con il suo hub
191200150618     c                   if        wofl1=estero and lcca<>' ' and
191300150618     c                             §7oinc='T'
191400150618     c                   Z-ADD     ltfa          worg
191500150618     c                   EXSR      decorg
191600150618     c                   endif
191700150618     c
191800000000     c* controlla se l'evento è già stato inserito
191900000000     c                   MOVEL     'S'           newevb                         *nuovo evento
192000000000do  1c     1             DO        200           j
192100000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
192200000000     c                   MOVE      wora          lavdevhev
192300000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
192400000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
192500000000     c                   MOVEL     'N'           newevb                         *già inserito
192600000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
192700000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
192800000000     c                   LEAVE                                                  *uscita ciclo
192900000000e   2c                   ENDIF
193000000000e   1c                   ENDDO
193100000000     c*
193200000000     c* incrementa contatore eventi
193300030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
193400030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
193500000000     c                   ADD       1             tote                           incrementa contatore
193600060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
193700060622     c                   MOVEL     §7odei        smdev(tote)
193800030203x   4c                   ELSE                                                   *evento telef.P.O.
193900060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
194000030203e   4c                   ENDIF
194100060622     c                   MOVEL     §7ocev        wcev                           *causale evento
194200000000     c                   EXSR      evbcon2                                      *imposta evento
194300030203x   3c                   ELSE                                                   *NO sost.Consegna
194400000000     c*
194500030203if  4c                   IF        ldcm > *zeros
194600030203if  5c                   IF        sostcon <> ' '                               *considera consegna
194700000000     c                   EXSR      deccevsta
194800030203if  6c                   IF        werr <> '1'                                  *no errore
194900000000     c                   ADD       1             tote                           incrementa contatore
195000030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
195100000000     c                   EXSR      evbcon2                                      *imposta evento
195200030203e   6c                   ENDIF
195300030203e   5c                   ENDIF
195400030203e   4c                   ENDIF
195500030203e   3c                   ENDIF
195600000000     c*
195700030203e   1c                   ENDIF
195800030203     c*
195900000000     c                   ENDSR
196000000000     c*--------------------------------------------------------------------------------------------*
196100000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
196200000000     c*--------------------------------------------------------------------------------------------*
196300000000     c     evbcon2       BEGSR
196400000000     c*
196500000000     c* indice schiera
196600000000     c                   Z-ADD     tote          smi(tote)
196700000000     c* sequenza legame
196800000000     c                   Z-ADD     seql          smseql(tote)
196900000000     c* sequenza evento
197000000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
197100000000     c* data    -impostata sopra-
197200000000     c                   MOVEL     wdate         smdeve(tote)
197300000000     c* ora     -impostata sopra-
197400000000     c                   MOVEL     worae         smheve(tote)
197500000000     c* filiale -impostata sopra-
197600000000     c                   MOVEL     wodes         smfle(tote)
197700000000     c* causale -impostata sopra-
197800000000     c                   MOVEL     wcev          smcev(tote)
197900000000     c* data+ora evento
198000000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
198100000000     c                   MOVE      wora          smdevhev(tote)
198200000000     c*
198300000000     c                   ENDSR
198400000000     c*--------------------------------------------------------------------------------------------*
198500000000     c* memorizza i dati della lettera di anomalia
198600000000     c*--------------------------------------------------------------------------------------------*
198700000000     c     memdct        BEGSR
198800000000     c*
198900040906     C                   RESET                   FIDN12DS
199000040906     c                   Z-ADD     waas          I12AAS
199100040906     c                   Z-ADD     wlnp          I12LNP
199200040906     c                   Z-ADD     wnrs          I12NRS
199300040906     c                   Z-ADD     wnsp          I12NSP
199400040906     C                   CALL      'FIDN12R'
199500040906     C                   PARM                    FIDN12DS
199600000000     c* memorizza la prima lettera di anomalia valida
199700040906do  1c                   IF        O12Err = *BLANK
199800040906     C                   DO        O12NCA        K
199900040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
200000040906     C                   EVAL      DCTAAC = O12KeyAAC
200100040906     C                   EVAL      DCTFIL = O12KeyFil
200200040906     C                   EVAL      DCTNCA = O12KeyNCA
200300040906     c     keydct        CHAIN     fndct01l
200400040906do  1c                   IF        %FOUND
200500000000     c*
200600000000     c* controlla validità CA
200700000000     c                   MOVEL     '0'           werr                           *NO errore
200800000000     c                   EXSR      chkdct
200900000000if  2c                   IF        werr = '0'                                   *CA valida
201000000000     c*
201100000000     c* Imposta l'evento lettera di anomalia
201200000000     c                   EXSR      evbdct
201300000000     C                   LEAVE                                                  *uscita ciclo
201400000000e   2c                   ENDIF
201500000000     c*
201600040906e   1c                   ENDIF
201700040906e   1c                   ENDDO
201800040906e   1c                   ENDIF
201900000000     C*
202000000000     c                   ENDSR
202100000000     c*--------------------------------------------------------------------------------------------*
202200000000     c* controlla validità lettera di anomalia
202300000000     c*--------------------------------------------------------------------------------------------*
202400000000     c     chkdct        BEGSR
202500000000     c*
202600000000     c* reimposta variabili di lavoro
202700000000     c                   MOVEL     '0'           werr                           *NO errore
202800000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
202900000000     c*
203000000000     c* CA annullata, esclude
203100000000if  1c                   IF        dctatb <> ' '
203200000000     c                   MOVEL     '1'           werr                           *SI errore
203300000000e   1C                   ENDIF
203400000000     c*
203500000000     c* CA non confermata, esclude
203600000000if  1c                   IF        dctfpr = ' '
203700000000     c                   MOVEL     '1'           werr                           *SI errore
203800000000e   1C                   ENDIF
203900000000     c*
204000000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
204100000000if  1c                   IF        dctfpr = 'T' AND
204200000000     c                             dctfca >= 215 OR
204300000000     c                             dctfpr = 'P' AND
204400000000     c                             dctfca >= 400
204500000000x   1c                   ELSE
204600000000     c                   MOVEL     '1'           werr                           *SI errore
204700000000e   1C                   ENDIF
204800000000     c*
204900000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
205000000000if  1c                   IF        werr = '0'
205100000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
205200060622     C                   RESET                   tibs02ds
205300060622     C                   EVAL      t02Cod = 'CCH'
205400060622     C                   EVAL      t02Ke1 = dctcch
205500060622     C                   EXSR      getTntbe00f
205600060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
205700000000     c                   MOVEL     '1'           werr                           *SI errore
205800000000e   3C                   ENDIF
205900000000e   2C                   ENDIF
206000000000e   1C                   ENDIF
206100000000     c*
206200000000     c                   ENDSR
206300000000     c*--------------------------------------------------------------------------------------------*
206400000000     c* Imposta l'evento lettera di anomalia
206500000000     c*--------------------------------------------------------------------------------------------*
206600000000     c     evbdct        BEGSR
206700000000     C*
206800000000     c* imposta causale
206900000000if  1c                   IF        wfpr = 'T'
207000000000     c                   MOVEL     '709'         wcev                           *causale evento
207100000000x   1c                   ELSE
207200000000     c                   MOVEL     '710'         wcev                           *causale evento
207300000000e   1c                   ENDIF
207400000000     c                   EXSR      deccevsta                                    *decodifica evento
207500000000     c*
207600000000     c* imposta filiale
207700000000if  1c                   IF        werr <> '1'                                  *no errore
207800000000     c                   Z-ADD     dctfil        worg
207900000000     c                   EXSR      decorg
208000000000     c* imposta data
208100000000     c                   MOVEL     dctaac        wdat
208200000000     c                   MOVE      dctmgc        wdat
208300000000     c                   EXSR      edtdat
208400000000     c* imposta ora
208500000000     c                   MOVEL     1100          wora                           *FISSA
208600000000     c                   EXSR      edtora
208700000000     c*
208800000000     c* incrementa contatore eventi
208900000000     c                   ADD       1             tote
209000000000     c* indice schiera
209100000000     c                   Z-ADD     tote          smi(tote)
209200000000     c* sequenza legame
209300000000     c                   Z-ADD     seql          smseql(tote)
209400000000     c* sequenza evento
209500000000     c                   Z-ADD     999           smseqe(tote)
209600000000     c* descrizione -impostata sopra-
209700030130     c                   MOVEL     wcdex         smdev(tote)
209800000000     c* filiale -impostata sopra-
209900000000     c                   MOVEL     wodes         smfle(tote)
210000000000     c* data    -impostata sopra-
210100000000     c                   MOVEL     wdate         smdeve(tote)
210200000000     c* ora     -impostata sopra-
210300000000     c                   MOVEL     worae         smheve(tote)
210400000000     c* causale -impostata sopra-
210500000000     c                   MOVEL     wcev          smcev(tote)
210600000000     c* data+ora evento
210700000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
210800000000     c                   MOVE      wora          smdevhev(tote)
210900000000e   1c                   ENDIF
211000000000     c*
211100000000     c                   ENDSR
211200000000     c*--------------------------------------------------------------------------------------------*
211300000000     c* imposta i campi della DS di Output dal contrassegno
211400000000     c*--------------------------------------------------------------------------------------------*
211500000000     c     impdsocsb     BEGSR
211600000000     c*
211700000000     c* azzera le variabili di lavoro
211800000000     c*
211900000000     c****
212000000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
212100000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
212200000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
212300000000     c****
212400000000     c*
212500000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
212600090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
212700000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
212800000000x   1c                   ELSE                                                     *trovato
212900000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
213000000000e   1C                   ENDIF
213100000000     c*---
213200000000     c* memorizza il contrassegno della bolla di Input
213300000000     c*---
213400000000     c                   EVAL      waas = s_tasaas                              *bolla Input
213500000000     c                   EVAL      wlnp = s_taslnp
213600000000     c                   EVAL      wnrs = s_tasnrs
213700000000     c                   EVAL      wnsp = s_tasnsp
213800000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
213900000000     c                   EXSR      memcsb
214000000000     c*
214100000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
214200000000     c                   EXSR      elacsblbl
214300000000     c*
214400000000     c                   ENDSR
214500000000     c*--------------------------------------------------------------------------------------------*
214600000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
214700000000     c*--------------------------------------------------------------------------------------------*
214800000000     c     elacsblbl     BEGSR
214900000000     c*
215000000000     c* azzera le variabili di lavoro
215100000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
215200000000     c                   EVAL      deplpn = *zeros
215300000000     c                   EVAL      depnrn = *zeros
215400000000     c                   EVAL      depnsn = *zeros
215500000000     c*
215600000000     c* legge i legami delle figlie successive alla bolla di input
215700000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
215800000000     c                   EVAL      kbllpp =  s_taslnp
215900000000     c                   EVAL      kblnrp =  s_tasnrs
216000000000     c                   EVAL      kblnsp =  s_tasnsp
216100000000     c*
216200000000     c* ciclo fino a fine mamme
216300000000     c     ke2lbl        SETLL     fnlbl02l
216400000000     c     ke2lbl        READE     fnlbl02l                               99
216500000000do  1c                   DOW       NOT *in99
216600000000     c*
216700000000     c* ciclo fino a fine figlie della mamma
216800000000do  2c                   DOW       NOT *in99
216900000000     c*
217000000000     c* legge la bolla dal legame
217100000000     c                   EVAL      kasaas = lblaan                              *figlia
217200000000     c                   EVAL      kaslnp = lbllpn
217300000000     c                   EVAL      kasnrs = lblnrn
217400000000     c                   EVAL      kasnsp = lblnsn
217500000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
217600000000     c*---
217700000000     c* memorizza il contrassegno della bolla figlia legata
217800000000     c*---
217900000000if  4c                   IF        NOT *in99                                    *esistente
218000000000     c                   EVAL      waas = lblaan                                *figlia
218100000000     c                   EVAL      wlnp = lbllpn
218200000000     c                   EVAL      wnrs = lblnrn
218300000000     c                   EVAL      wnsp = lblnsn
218400000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
218500000000     c                   EXSR      memcsb
218600000000e   4c                   ENDIF
218700000000     c*
218800000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
218900000000     c                   EVAL      depaan = lblaan                              *ultima figlia
219000000000     c                   EVAL      deplpn = lbllpn
219100000000     c                   EVAL      depnrn = lblnrn
219200000000     c                   EVAL      depnsn = lblnsn
219300000000     c*
219400000000     c* lettura successiva legami
219500000000     c     ke2lbl        READE     fnlbl02l                               99
219600000000e   2c                   ENDDO                                                  *fine figlie mamma
219700000000     c*
219800000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
219900000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
220000000000     c                             kbllpp <> deplpn OR
220100000000     c                             kblnrp <> depnrn OR
220200000000     c                             kblnsp <> depnsn
220300000000     c* nuova chiave
220400000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
220500000000     c                   EVAL      kbllpp =  deplpn
220600000000     c                   EVAL      kblnrp =  depnrn
220700000000     c                   EVAL      kblnsp =  depnsn
220800000000     c* controlla figlie
220900000000     c     ke2lbl        SETLL     fnlbl02l
221000000000     c     ke2lbl        READE     fnlbl02l                               99
221100000000if  3c                   IF        *in99                                        *non ha figlie
221200000000     c                   LEAVE                                                  *esce dal ciclo
221300000000e   3c                   ENDIF
221400000000x   2c                   ELSE                                                   *sempre stessa mamma
221500000000     c                   LEAVE                                                  *esce dal ciclo
221600000000e   2c                   ENDIF
221700000000e   1c                   ENDDO                                                  *fine mamme
221800000000     c*
221900000000     c                   ENDSR
222000000000     c*--------------------------------------------------------------------------------------------*
222100000000     c* memorizza il contrassegno
222200000000     c*--------------------------------------------------------------------------------------------*
222300000000     c     memcsb        BEGSR
222400000000     c*
222500000000     c* legge il contrassegno, se avuto
222600000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
222700000000     c                   Z-ADD     waas          ksbaas
222800000000     c                   Z-ADD     wlnp          ksblnp
222900000000     c                   Z-ADD     wnrs          ksbnrs
223000000000     c                   Z-ADD     wnsp          ksbnsp
223100000000     c     keycsb        CHAIN     tncsb03l                           99
223200000000if  2c                   IF        NOT *in99                                    *esistente
223300000000     c* memorizza i dati
223400000000     c                   MOVEL     'S'           cao74                          *esiste C/A
223500000000     c*
223600000000     c                   Z-ADD     csbcas        caimpo74                       *importo
223700000000     c*
223800000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
223900010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
224000010820     c                             caimpo74 > *zeros
224100010820     c                   EVAL      cadivo74 = 'ITL'
224200010820e   3c                   ENDIF
224300100119     ** Visualizzo la descrizione del tipo incasso contrassegno solo se il
224400100119     ** contrassegno è stato incassato.
224500100121     C                   EVAL      cainco74 =
224600100121     C                                       GetDescrizioneIncassoContrassegno()
224700000000     c*
224800000000     c                   MOVEL     *blanks       castao74                       *stato
224900060622     C                   EVALR     tblKey = %CHAR(csbSta)
225000060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
225100060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
225200060622     C                             :rpyOpCode:rpyEsito)
225300060622     C                   IF        rpyOpCode = 'FOUND'
225400060622     c                   EVAL      castao74 = §4SDei
225500000000e   3c                   ENDIF
225600100119     c
225700000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
225800011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
225900060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
226000000000x   4c                   ELSE
226100000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
226200060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
226300000000x   5c                   ELSE
226400060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
226500000000e   5c                   ENDIF
226600000000e   4c                   ENDIF
226700000000e   3c                   ENDIF
226800000000     c*
226900000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
227000000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
227100000000if  4c                   IF        csbtpa = 'B'
227200060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
227300000000e   4c                   ENDIF
227400000000if  4c                   IF        csbtpa = 'C'
227500060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
227600000000e   4c                   ENDIF
227700000000x   3c                   ELSE                                                   *incasso Bartolini
227800000000if  4c                   IF        csbfpc = *blanks
227900110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
228000000000e   4c                   ENDIF
228100000000if  4c                   IF        csbfpc = '2'
228200060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
228300000000e   4c                   ENDIF
228400000000e   3c                   ENDIF
228500000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
228600000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
228700000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
228800000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
228900000000     c                             csbfpc = *blanks                              assegno di traenza
229000060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
229100000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
229200060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
229300000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
229400060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
229500000000e   4c                   ENDIF
229600000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
229700060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
229800060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
229900000000e   3c                   ENDIF
230000000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
230100000000     c                   Z-ADD     *zeros        caabio74                       *abi
230200000000     c                   Z-ADD     *zeros        cacabo74                       *cab
230300000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
230400000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
230500000000     c                   MOVEL     csbddp        wdat
230600000000     c                   EXSR      edtdat
230700000000     c                   EVAL      cadato74 = wdate
230800000000     C*
230900000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
231000000000     c                   Z-ADD     csbabi        caabio74
231100000000     c                   Z-ADD     csbcab        cacabo74
231200000000     c                   MOVEL     csbnra        canumo74
231300120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
231400120321     c                             : caCabO74)
231500000000x   4c                   ELSE                                                   *incasso Bartolini
231600000000     c                   MOVEL     csbndp        canumo74
231700000000e   4c                   ENDIF
231800000000e   3c                   ENDIF
231900110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
232000110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
232100110307     C                   IF        IsContrassegnoCompensato()
232200110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
232300110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
232400110304     C                   CLEAR                   caabio74
232500110304     C                   CLEAR                   cacabo74
232600110304     C                   CLEAR                   canumo74
232700110304     C                   ENDIF
232800110304     C*
232900000000e   2c                   ENDIF
233000000000e   1c                   ENDIF
233100000000     c*
233200000000     c                   ENDSR
233300000000     c*--------------------------------------------------------------------------------------------*
233400000000     c* imposta i campi della DS di Output dalla giacenza
233500000000     c*--------------------------------------------------------------------------------------------*
233600000000     c     impdsogcp     BEGSR
233700000000     c*
233800000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
233900000000     c                   EVAL      waas = s_tasaas                              *bolla Input
234000000000     c                   EVAL      wlnp = s_taslnp
234100000000     c                   EVAL      wnrs = s_tasnrs
234200000000     c                   EVAL      wnsp = s_tasnsp
234300000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
234400000000     c                   EXSR      memgcp
234500000000     c*
234600000000     c                   ENDSR
234700000000     c*--------------------------------------------------------------------------------------------*
234800000000     c* memorizza la giacenza
234900000000     c*--------------------------------------------------------------------------------------------*
235000000000     c     memgcp        BEGSR
235100000000     c*
235200000000     c* imposta le variabili di lavoro
235300000000     c                   CLEAR                   i                              *indice
235400000000     c                   CLEAR                   ii
235500000000     c                   CLEAR                   smdmc                          *note fase -20-
235600000000     c                   CLEAR                   smdmcR
235700000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
235800000000     c                   MOVEL     'N'           gco74                          *flag di controllo
235900000000     c                   MOVEL     'N'           gcfdio74
236000000000     c                   MOVEL     'N'           gcmito74
236100000000     c*
236200000000     c* legge la giacenza, se avuta
236300000000     c                   Z-ADD     waas          kcpaas
236400000000     c                   Z-ADD     wlnp          kcplnp
236500000000     c                   Z-ADD     wnrs          kcpnrs
236600000000     c                   Z-ADD     wnsp          kcpnsp
236700000000     c                   Z-ADD     *zeros        kcpfrg
236800050309     c     keygcp        CHAIN     tigcp51l                           99
236900050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
237000050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
237100000000if  2c                   IF        NOT *in99        AND                         *esistente
237200000000     c                             gcpatb = *blanks AND                         *no annullata
237300050415     c                             gcptcm <> 'Z'    AND
237400050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
237500050419     C
237600050419     C                   EVAL      gcRiapO74 = gcpRiap
237700050418     **?Descrizione della fase.
237800050418     C                   SELECT
237900050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
238000060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
238100050418     C                   WHEN      gcpFas = 20
238200060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
238300050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
238400060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
238500050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
238600060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
238700050418     C                   ENDSL
238800000000     c* memorizza i dati dalle NOTE GIACENZE
238900000000     c                   Z-ADD     *zeros        i
239000000000     c                   Z-ADD     *zeros        ii
239100000000     c                   Z-ADD     gcpagc        knpagc
239200000000     c                   Z-ADD     gcpfgc        knpfgc
239300000000     c                   Z-ADD     gcpngc        knpngc
239400000000     c                   Z-ADD     gcpfrg        knpfrg
239500050309     c     keygnp        SETLL     tignp01l
239600050309     c     keygnp        READE     tignp01l                               98
239700000000do  3c                   DOW       NOT *in98
239800050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
239900000000     c* fase -10-
240000000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
240100000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
240200000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
240300000000e   5c                   ENDIF
240400000000     c* fase -20-
240500000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
240600000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
240700051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
240800000000     c                   ADD       1             i
240900000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
241000000000e   6c                   ENDIF
241100000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
241200051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
241300050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
241400050405     c                   CLEAR                   ii
241500050405     c                   CLEAR                   smdmcR
241600050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
241700050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
241800050919     C                   IF        NOT %ERROR
241900140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
242000050919     C                   ENDIF
242100050405     C                   ELSE
242200000000     c                   ADD       1             ii
242300000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
242400050405     C                   ENDIF
242500000000e   6c                   ENDIF
242600000000e   5c                   ENDIF
242700000000e   4c                   ENDIF
242800050309     c     keygnp        READE     tignp01l                               98
242900000000e   3c                   ENDDO
243000000000     c*
243100000000     c* memorizza i dati dalle GIACENZE
243200000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
243300050404     c* Disposizione da immettere.
243400051110     c                   IF        gcpfas = 20
243500051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
243600051110     C                   EXSR      chkTivpi
243700051110e   3c                   ENDIF
243800000000     c*
243900000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
244000000000     c*
244100000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
244200000000     c*
244300060103     c                   Z-ADD     gcpagc        dsannB
244400060103     c                   Z-ADD     gcpmgc        dsmgsB
244500060103     c                   Z-ADD     dsdatB        wdat
244600000000if  3c                   IF        wdat > *zeros
244700000000     c                   EXSR      edtdat
244800000000     c                   EVAL      gcdago74 = wdate                             *data apertura
244900000000e   3C                   ENDIF
245000060104     c*
245100060104     c                   Z-ADD     gcpdur        wdat
245200060104     c                   IF        wdat > *zeros
245300060104     c                   EXSR      edtdat
245400060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
245500060104     C                   ENDIF
245600000000     c*
245700000000if  3c                   IF        gcpcmc <> *blanks
245800000000     c                   MOVEL     gcpcmc        wcev
245900000000     c                   EXSR      deccevgia
246000030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
246100000000e   3C                   ENDIF
246200000000     c*
246300000000     c                   MOVEL     *blanks       gcdcgo74
246400000000if  3c                   IF        gcpdcg > *zeros
246500000000     c                   Z-ADD     gcpdcg        wdat
246600000000     c                   EXSR      edtdat
246700000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
246800000000e   3c                   ENDIF
246900000000     c*---
247000000000     c* Disposizioni già immesse
247100000000     c*---
247200000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
247300000000     c*
247400000000if  4c                   IF        gcpddm > *zeros
247500000000     c                   Z-ADD     gcpddm        wdat
247600000000     c                   EXSR      edtdat
247700000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
247800000000e   4C                   ENDIF
247900000000     c*
248000000000if  4c                   IF        gcpdis <> *blanks
248100060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
248200060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
248300060626     C                             :rpyOpCode:rpyEsito)
248400060622     C                   EVAL      gcdiso74 = §2DDes
248500000000e   4C                   ENDIF
248600000000     c*
248700000000if  4c                   IF        gcpasg <> *blanks
248800000000if  5c                   IF        gcpasg = 'S'
248900060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
249000000000x   5c                   ELSE
249100060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
249200000000e   5C                   ENDIF
249300000000     c*
249400000000if  5c                   IF        gcppsg = 'M'
249500060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
249600000000e   5C                   ENDIF
249700000000if  5c                   IF        gcppsg = 'D'
249800060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
249900000000e   5C                   ENDIF
250000000000e   4C                   ENDIF
250100000000     c*
250200000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
250300000000     c                   MOVEL     gcpvcs        gcvcso74
250400000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
250500000000     c                   Z-ADD     gcpcas        gccaso74
250600000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
250700000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
250800000000e   5C                   ENDIF
250900000000e   4C                   ENDIF
251000050404     C                   IF        smdmc(1) <> *BLANK
251100050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
251200050404     C                             smdmc(3) + smdmc(4)
251300051014     C                   EVAL      gcdmc5O74 = smdmc(5)
251400050404     C                   ELSE
251500050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
251600050404     C                             smdmcr(3) + smdmcr(4)
251700051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
251800050404e   3C                   ENDIF
251900000000e   3C                   ENDIF
252000000000     c*---
252100000000     c* Disposizioni da immettere
252200000000     c*---
252300010719     c* se le disposizioni sono da immettere controlla che:
252400010719     c* - il cliente è il mittente della giacenza
252500000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
252600040726if  4C                   IF        keygiac <> *blanks
252700040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
252800040726x   4c                   ELSE
252900090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
253000090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
253100090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
253200090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
253300090914     C                             )
253400040726e   4C                   ENDIF
253500000000e   3C                   ENDIF
253600050404     C* Controllo esistenza storia giacenza.
253700050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
253800050404     C                   DO        *HIVAL
253900050404     C     K04GCP51      READE     TIGCP51L
254000050404     C                   IF        %EOF
254100050404     C                   LEAVE
254200050404     C                   ENDIF
254300050404     C                   IF        GCPATB = *BLANK
254400050404     C                   EVAL      StoGiaO74 = 'S'
254500050404     C                   LEAVE
254600050404     C                   ENDIF
254700050404     C                   ENDDO
254800050404     c*
254900000000e   2c                   ENDIF
255000000000     c*
255100000000     c                   ENDSR
255200000000     c*--------------------------------------------------------------------------------------------*
255300000000     c* imposta i campi della DS di Output dalle note
255400000000     c*--------------------------------------------------------------------------------------------*
255500000000     c     impdsonot     BEGSR
255600000000     c*
255700000000     c* azzera le variabili di lavoro
255800000000     c                   CLEAR                   i
255900000000     c                   CLEAR                   mdcre                          consegna richiesta
256000000000     c                   CLEAR                   mhcre
256100030210     c                   CLEAR                   mtcre
256200000000     c                   CLEAR                   mftce                          consegna particolare
256300000000     c                   CLEAR                   wftc
256400000000     c                   CLEAR                   d1wdftc
256500000000     c                   CLEAR                   d2wdftc
256600000000     c                   CLEAR                   mgcxe                          turni chiusura
256700000000     c                   CLEAR                   wgcx
256800000000     c                   CLEAR                   d1wdgcx
256900000000     c                   CLEAR                   d2wdgcx
257000010927     c                   CLEAR                   mffd                           fermo deposito
257100010927     c                   CLEAR                   mmncB                          *motivi NON consegna
257200010927     c                   CLEAR                   mmncC
257300010927     c                   CLEAR                   mmncG
257400010927     c                   CLEAR                   mmncH
257500010927     c                   CLEAR                   mmncN
257600010927     c                   CLEAR                   mmncO
257700050701     c                   CLEAR                   mmnc8
257800050701     c                   CLEAR                   mmnc9
257900050701     c                   CLEAR                   a230
258000000000     c                   CLEAR                   we
258100030206     c                   CLEAR                   mtel                           n° tel destinatario
258200030206     c                   CLEAR                   mban                           bancali
258300030207     c                   CLEAR                   mscr                           supermercati
258400030210     c                   CLEAR                   msdace
258500030210     c                   CLEAR                   msorce
258600030210     c                   CLEAR                   msdcre
258700030210     c                   CLEAR                   mshcre
258800030210     c                   CLEAR                   mstcre
258900030210     c                   CLEAR                   msnom
259000030210     c                   CLEAR                   mscrT
259100030210     c                   CLEAR                   wmscrT
259200030210     c                   CLEAR                   s
259300030207     c
259400140619     c*imposta dati data consegna richiesta
259500140716     c                   if        f_tasdcr > 0
259600140716     c                   MOVEL     f_tasdcr      wdat
259700140619     c                   EXSR      edtdat
259800140619     c                   eval      DTCONRCO74  =  wdate
259900140819if  3c                   IF        f_tastcr = ' '
260000140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
260100140819e   3c                   ENDIF
260200140819     c                   endif
260300140716     c                   if        f_tashcr > 0
260400140716     c                   MOVEL     f_tashcr      wora
260500140619     c                   EXSR      edtora
260600140619     c                   eval      ORACONRO74  =  worae
260700140619     c                   endif
260800140716     c                   eval      TPDATCRO74  =  f_tastcr
260900140619      *reperimento descrizione tipo consegna richiesta
261000140716if  3c                   IF        f_tastcr = 'P'
261100140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
261200140619e   3c                   ENDIF
261300140716if  3c                   IF        f_tastcr = 'D'
261400140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
261500140619e   3c                   ENDIF
261600000000     c*---
261700000000     c* memorizza le "note" che sono presenti nel record bolla
261800000000     c*---
261900000000     c* Consegna richiesta
262000140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
262100140619     c*m                           s_tashcr = *zeros
262200140619x   1c*m                 ELSE                                                   *SI cons.particolare
262300140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
262400140619     c*m                 MOVEL     s_tasdcr      wdat
262500140619     c*m                 EXSR      edtdat                                       *edita data
262600140619     c*m                 MOVEL     wdate         mdcre
262700140619e   2c*m                 ENDIF
262800140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
262900140619     c*m                 MOVEL     s_tashcr      wora
263000140619     c*m                 EXSR      edtora                                       *edita ora
263100140619     c*m                 MOVEL     worae         mhcre
263200140619e   2c*m                 ENDIF
263300140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
263400140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
263500140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
263600140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
263700140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
263800140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
263900140619e   3c*m                 ENDIF
264000140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
264100140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
264200140619e   3c*m                 ENDIF
264300140619if  3c*m                 IF        s_tastcr = ' '                               *il
264400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
264500140619e   3c*m                 ENDIF
264600140619e   2c*m                 ENDIF
264700140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
264800140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
264900140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
265000140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
265100140619e   3c*m                 ENDIF
265200140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
265300140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
265400140619e   3c*m                 ENDIF
265500140619if  3c*m                 IF        s_tastcr = ' '                               *il
265600140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
265700140619e   3c*m                 ENDIF
265800140619e   2c*m                 ENDIF
265900140619     c*m
266000140619if  2c*m                 IF        i < 10                                       *se c'è spazio
266100140619     c*m                 ADD       1             i                              *memorizza nota
266200140619     c*m                 EVAL      smnot(i) =
266300140619     c*m                           %TRIM(mtcre) +
266400140619     c*m                           ' '          +
266500140619     c*m                           %TRIM(mdcre) +
266600140619     c*m                           ' '          +
266700140619     c*m                           %TRIM(mhcre)
266800140619e   2c*m                 ENDIF
266900140619e   1c*m                 ENDIF
267000000000     c*
267100000000     c* Particolarità consegna -uno/due-
267200140716if  1c                   IF        f_tasftc <> *blanks OR
267300140716     c                             f_tastc2 <> *blanks
267400140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
267500000000     c                   EXSR      decftc
267600000000     c                   MOVEL     wdftc         d1wdftc
267700140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
267800000000     c                   EXSR      decftc
267900000000     c                   MOVEL     wdftc         d2wdftc
268000000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
268100000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
268200060612     c                   EVAL      we = ' ' +
268300060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
268400000000x   2c                   ELSE
268500000000     c                   EVAL      we = *blanks
268600000000e   2c                   ENDIF
268700060612     c                   EVAL      mftce =
268800060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
268900060612     c                             %TRIM(d1wdftc)             +
269000060612     c                             we                         +
269100060612     c                             %TRIM(d2wdftc)
269200000000     c*
269300010927if  2c                   IF        i < 10                                       *se c'è spazio
269400000000     c                   ADD       1             i                              *memorizza nota
269500000000     c                   EVAL      smnot(i) = mftce
269600010927e   2c                   ENDIF
269700010927e   1c                   ENDIF
269800000000     c*
269900000000     c* Chiusura per turno
270000140716if  1c                   IF        f_tasgc1 <> *blanks OR
270100140716     c                             f_tasgc2 <> *blanks
270200140716if  2c                   IF        f_tasgc1 <> *blanks
270300140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
270400000000     c                   EXSR      decgcx
270500000000     c                   MOVEL     wdgcx         d1wdgcx
270600000000e   2c                   ENDIF
270700140716if  2c                   IF        f_tasgc2 <> *blanks
270800140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
270900000000     c                   EXSR      decgcx
271000000000     c                   MOVEL     wdgcx         d2wdgcx
271100000000e   2c                   ENDIF
271200000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
271300000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
271400060612     c                   EVAL      we = ' ' +
271500060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
271600000000x   2c                   ELSE
271700000000     c                   EVAL      we = *blanks
271800000000e   2c                   ENDIF
271900060612     c                   EVAL      mgcxe =
272000060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
272100060612     c                             %TRIM(d1wdgcx)       +
272200060612     c                             we                   +
272300060612     c                             %TRIM(d2wdgcx)
272400000000     c*
272500010927if  2c                   IF        i < 10                                       *se c'è spazio
272600000000     c                   ADD       1             i                              *memorizza nota
272700000000     c                   EVAL      smnot(i) = mgcxe
272800010927e   2c                   ENDIF
272900010927e   1c                   ENDIF
273000000000     c*
273100000000     c* Fermo deposito
273200140716if  1c                   IF        f_tasffd = 'S'
273300060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
273400000000     c*
273500010927if  2c                   IF        i < 10                                       *se c'è spazio
273600010927     c                   ADD       1             i                              *memorizza nota
273700010927     c                   EVAL      smnot(i) = mffd
273800010927e   2c                   ENDIF
273900010927e   1c                   ENDIF
274000030206     c*---
274100030206     c* memorizza le "note" che NON sono presenti nel record bolla
274200030206     c* ma sul file estensione "anagrafiche"
274300030206     c*---
274400030206     c* N° telefono destinatario
274500140716     c                   Z-ADD     f_tasaas      kaaaas
274600140716     c                   Z-ADD     f_taslnp      kaalnp
274700140716     c                   Z-ADD     f_tasnrs      kaanrs
274800140716     c                   Z-ADD     f_tasnsp      kaansp
274900030206     c                   MOVEL     'O'           kaatrc
275000030206     c     keytaa        CHAIN     titaa30c                           98
275100030206if  1c                   IF        NOT *in98
275200040330     C                   EVAL      MitOrigO74 = TAARSC
275300030206e   1c                   ENDIF
275400030206     c*---
275500030206     c* memorizza le "note" che NON sono presenti nel record bolla
275600030206     c* ma sul file estensione testata boolle
275700030206     c*---
275800030206     c* Bancali
275900140716     c                   Z-ADD     f_tasaas      kr5aas
276000140716     c                   Z-ADD     f_taslnp      kr5lnp
276100140716     c                   Z-ADD     f_tasnrs      kr5nrs
276200140716     c                   Z-ADD     f_tasnsp      kr5nsp
276300030206     c                   MOVEL     'BAN'         kr5trd
276400040531     c     keyar5        CHAIN     fiar531c                           98
276500030206if  1c                   IF        NOT *in98
276600030206     c                   MOVEL     ar5uni        dar5ban
276700060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
276800060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
276900030206if  2c                   IF        §ar5rb1 > *zeros OR
277000030206     c                             §ar5rb2 > *zeros
277100060613     c                   EVAL      mban = %trim(mban) +
277200060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
277300060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
277400030206e   2c                   ENDIF
277500030206     c*
277600030206if  2c                   IF        i < 10                                       *se c'è spazio
277700030206     c                   ADD       1             i                              *memorizza nota
277800030206     c                   EVAL      smnot(i) = mban
277900030206e   2c                   ENDIF
278000030206e   1c                   ENDIF
278100040330
278200040330     **?Reperisco referente
278300030206     c* Note supermercati
278400140716if  1c     f_tasftc      IFEQ      'S'
278500140716     c     f_tastc2      OREQ      'S'
278600030210     c                   CLEAR                   s                              *indice
278700140716     c                   Z-ADD     f_tasaas      kr5aas
278800140716     c                   Z-ADD     f_taslnp      kr5lnp
278900140716     c                   Z-ADD     f_tasnrs      kr5nrs
279000140716     c                   Z-ADD     f_tasnsp      kr5nsp
279100030206     c                   MOVEL     'SCR'         kr5trd
279200040531     c     keyar5        SETLL     fiar531c
279300040531     c     keyar5        READE     fiar531c                               98
279400030210do  2c                   DOW       NOT *in98
279500030206     c                   MOVEL     ar5uni        dar5scr
279600030207     c* data nota
279700030210if  3c                   IF        ar5dac > *zeros
279800030207     c                   MOVEL     ar5dac        wdat
279900030207     c                   EXSR      edtdat                                       *edita data
280000030210     c                   MOVEL     wdate         msdace
280100030210e   3c                   ENDIF
280200030207     c* ora nota
280300030210if  3c                   IF        ar5orc > *zeros
280400030207     c                   MOVEL     ar5orc        wora
280500030207     c                   EXSR      edtora                                       *edita ora
280600030210     c                   MOVEL     worae         msorce
280700030210e   3c                   ENDIF
280800030207     c* Consegna richiesta
280900140708if  3c                   IF        §ar5dcr = *zeros AND
281000140708     c                             §ar5hcr = *zeros
281100140708x   3c                   ELSE
281200140708if  4c                   IF        §ar5dcr > *zeros
281300140708     c                   MOVEL     §ar5dcr       wdat
281400140708     c                   EXSR      edtdat                                       *edita data
281500140708     c                   MOVEL     wdate         msdcre
281600140708e   4c                   ENDIF
281700140708if  4c                   IF        §ar5hcr > *zeros
281800140708     c                   MOVEL     §ar5hcr       wora
281900140708     c                   EXSR      edtora                                       *edita ora
282000140708     c                   MOVEL     worae         mshcre
282100140708e   4c                   ENDIF
282200140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
282300140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
282400140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
282500140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
282600140708if  5c                   IF        §ar5tcr = 'P'                                *prima
282700140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
282800140708e   5c                   ENDIF
282900140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
283000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
283100140708e   5c                   ENDIF
283200140708if  5c                   IF        §ar5tcr = ' '                                *il
283300140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
283400140708e   5c                   ENDIF
283500140708e   4c                   ENDIF
283600140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
283700140708     c                             §ar5dcr = *zeros                             *NO data richiesta
283800140708if  5c                   IF        §ar5tcr = 'P'                                *prima
283900140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
284000140708e   5c                   ENDIF
284100140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
284200140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
284300140708e   5c                   ENDIF
284400140708if  5c                   IF        §ar5tcr = ' '                                *il
284500140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
284600140708e   5c                   ENDIF
284700140708e   4c                   ENDIF
284800030210e   3c                   ENDIF
284900030210     c* Riferimento
285000030210if  3c                   IF        §ar5nom <>  *blanks
285100060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
285200060613     c                             + ' ' + §ar5nom
285300030210e   3c                   ENDIF
285400030210     c*
285500030210if  3c                   IF        s < 5                                        *MAX 5 note
285600030210     c                   ADD       1             s
285700030210     c                   EVAL      mscrT(s) = 'Il ' +
285800030210     c                             %trim(msdace) +                              *data nota
285900030210     c                             ' '    +
286000030210     c                             %trim(msorce) +                              *ora nota
286100030210     c                             ' '    +
286200140708     c                             %trim(mstcre) +                              *tipo consegna
286300140708     c                             ' '    +
286400140708     c                             %trim(msdcre) +                              *data consegna
286500140708     c                             ' '    +
286600140708     c                             %trim(mshcre) +                              *ora consegna
286700140708     c                             ' '    +
286800030210     c                             %trim(msnom)  +                              *parlato con
286900030210     c                             ' '    +
287000030210     c                             %trim(§ar5tel) +                             *telefono
287100030210     c                             ' '    +
287200030210     c                             %trim(§ar5mot)                               *motivo
287300030210e   2c                   ENDIF
287400030210     c*
287500040531     c     keyar5        READE     fiar531c                               98
287600030206e   2c                   ENDDO
287700030210e   1c                   ENDIF
287800030210     c* NOTA completa
287900030210if  1c                   IF        mscrT(1) <> *blanks
288000030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
288100030211     c                                      ' ' +
288200030211     C                                      %trim(mscrT(2)) +
288300030211     c                                      ' ' +
288400030211     C                                      %trim(mscrT(3)) +
288500030211     c                                      ' ' +
288600030211     C                                      %trim(mscrT(4)) +
288700030211     c                                      ' ' +
288800030211     C                                      %trim(mscrT(5))
288900030210     c                   Z-ADD     *zeros        s
289000030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
289100030210     c                   ADD       1             i
289200030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
289300030210     c                   EVAL      s = s + 100
289400030210e   2c                   ENDDO
289500030210     c*
289600030210e   1c                   ENDIF
289700010927     c*---
289800010927     c* memorizza le "note" che NON sono presenti nel record bolla
289900030206     c* ma sul file estensione "riferimenti"
290000010927     c*---
290100010927     c* motivi della NON consegna
290200010927     c                   CLEAR                   tita4000
290300010927     c                   CLEAR                   tita4p00
290400140716     c                   Z-ADD     f_tasaas      ka4aas
290500140716     c                   Z-ADD     f_taslnp      ka4lnp
290600140716     c                   Z-ADD     f_tasnrs      ka4nrs
290700140716     c                   Z-ADD     f_tasnsp      ka4nsp
290800010927     c     ke2ta4        SETLL     tita430c
290900010927     c     ke2ta4        READE     tita430c                               98
291000010927do  1c                   DOW       NOT *in98
291100140708     c                   if        ta4trc = '8' OR ta4trc = '9'
291200050701     c*
291300050701if  3c                   IF        ta4trc = '8'
291400050701     c                   EVAL      mmnc8 = ta4not
291500050701e   3c                   ENDIF
291600050701if  3c                   IF        ta4trc = '9'
291700050701     c                   EVAL      mmnc9 = ta4not
291800050701e   3c                   ENDIF
291900050701     c*
292000010927e   2c                   ENDIF
292100010925     c*
292200010926     c     ke2ta4        READE     tita430c                               98
292300010926e   1c                   ENDDO
292400050701     C*
292500140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
292600050701     C*
292700050701     C                   IF        mmnc8 <> *BLANK
292800050701     C                   EVAL      a230 = %TRIML(mmnc8)
292900050701     C                   ENDIF
293000050701     C                   IF        mmnc9 <> *BLANK
293100050701     C                   IF        a230 <> *BLANK
293200050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
293300050701     C                   ELSE
293400050701     C                   EVAL      a230 = %TRIML(mmnc9)
293500050701     C                   ENDIF
293600050701     C                   ENDIF
293700010927     C*
293800050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
293900050701     C*
294000010927if  2c                   IF        i < 10                                       *se c'è spazio
294100010927     c                   ADD       1             i                              *memorizza nota
294200010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
294300010927e   2c                   ENDIF
294400010927     C*
294500010927if  2c                   IF        i < 10                                       *se c'è spazio
294600010927if  3c                   IF        n3 > 100
294700010927     c                   ADD       1             i                              *memorizza nota
294800010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
294900010927e   3c                   ENDIF
295000010927e   2c                   ENDIF
295100010927     C*
295200010927if  2c                   IF        i < 10                                       *se c'è spazio
295300010927if  3c                   IF        n3 > 200
295400010927     c                   ADD       1             i                              *memorizza nota
295500010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
295600010927e   3c                   ENDIF
295700010927e   2C                   ENDIF
295800010927e   1c                   ENDIF
295900010925     c*
296000000000     c* imposta contatore note da visualizzare nella DS di Output
296100000000     c                   EVAL      cntnoto74 = i
296200000000     c*
296300000000     c* imposta le note nella DS di Output
296400000000do  1c     1             DO        10            j
296500000000     c                   EVAL      nota(j) = smnot(j)
296600000000e   1c                   ENDDO
296700000000     c*
296800000000     c                   ENDSR
296900010716     c*--------------------------------------------------------------------------------------------*
297000010716     c* imposta i campi della DS di Output dalla firma DPD
297100010716     c*--------------------------------------------------------------------------------------------*
297200010716     c     impdsoDPD     BEGSR
297300010717     c*
297400010717     c* reimposta variabili di lavoro
297500010717     c                   RESET                   $AbFirma
297600010717     c                   CLEAR                   depute                         *deposito utente
297700010717     c                   CLEAR                   deppwd                         *deposito password
297800010717     c*---
297900010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
298000010717     c*---
298100010717     c                   Z-ADD     s_taslna      worg
298200010717     c                   EXSR      decorg
298300020702     c                   IF        wodpd = 'DPD'
298400020702     c                   MOVEL     'S'           dpdo74
298500020702     c                   ELSE
298600020702     c                   MOVEL     *blanks       dpdo74
298700020702     c                   ENDIF
298800010717     c*
298900010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
299000060627     c* Riferimento link Lettera di vettura
299100060627     c* Riferimento link Firma
299200010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
299300010717     c                   CLEAR                   tita4000
299400010717     c                   CLEAR                   tita4p00
299500060627     c                   CLEAR                   dsbl4i
299600010717     c                   Z-ADD     s_tasaas      ka4aas
299700010717     c                   Z-ADD     s_taslnp      ka4lnp
299800010717     c                   Z-ADD     s_tasnrs      ka4nrs
299900010717     c                   Z-ADD     s_tasnsp      ka4nsp
300000060627     c                   MOVEL     'I'           ka4trc
300100060627     c     keyta4        CHAIN     tita430c                           98
300200010717if  2c                   IF        NOT *in98
300300060627     c                   MOVEL     ta4not        dsbl4i
300400060627     c                   EVAL      DPDRmnO74 = §b4ipn
300500060627     c                   EVAL      DPDFnpO74 = §b4ipn
300600010717e   2c                   ENDIF
300700010717     c*
300800010717     c* cerca abilitazione firma DPD per questo segnacollo
300900010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
301000010717     c                   EXSR      RepAbilFirma
301100010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
301200010717     c*---
301300010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
301400010717     c*---
301500010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
301600010717     c* dati
301700010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
301800010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
301900010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
302000010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
302100010717     c*
302200010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
302300010717     c                   Z-ADD     s_tasaas      kvbaas
302400010717     c                   Z-ADD     s_taslnp      kvblnp
302500010717     c                   Z-ADD     s_tasnrs      kvbnrs
302600010717     c                   Z-ADD     s_tasnsp      kvbnsp
302700010717     c                   Z-ADD     99999999      kvbdev
302800010717     c                   Z-ADD     9999          kvbhev
302900010717     c     ke2evb        SETGT     fnevb01l
303000010717     c     keyevb        READPE    fnevb01l                               98
303100010717do  4c                   DOW       NOT *in98
303200101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
303300010720     c*
303400060629     c                   EVAL      devB01 = evbNot
303500060628     c                   IF        §NotDep = *BLANK
303600060628     c                   RESET                   DPDFirO74
303700060629     c                   LEAVE
303800060628     c                   ENDIF
303900060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
304000060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
304100080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
304200010720     c                   Z-ADD     *zeros        g08dat
304300010720     c                   MOVEL     '3'           g08err
304400010720     c                   CALL      'XSRDA8'
304500010720     c                   PARM                    wlbda8
304600010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
304700010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
304800010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
304900010720     c*
305000010717     c                   LEAVE                                                  *uscita ciclo
305100010717e   5c                   ENDIF
305200010717     c     keyevb        READPE    fnevb01l                               98
305300010717e   4c                   ENDDO
305400010717     c*
305500010717e   3c                   ENDIF
305600010717e   2c                   ENDIF
305700010716e   1c                   ENDIF
305800010716     c*
305900010716     c                   ENDSR
306000140604     c*--------------------------------------------------------------------------------------------*
306100140604     c* imposta i campi da FNARB se TASDCM = 0
306200140604     c*--------------------------------------------------------------------------------------------*
306300140604     c     impdsoARB     BEGSR
306400150403     c                   clear                   orarich           4 0
306500150611     c                   clear                   wdee
306600150410     c* memorizzo se utilizzo la data prevista consegna
306700150410     c                   clear                   usaprev           1
306800140604     c*
306900140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
307000140604     c                   EVAL      lnaDescO74 = sodes(i)
307100140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
307200140604     c*
307300150116      *eliminata forzatura resta impostato dalla mamma per composizione link
307400150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
307500150116     c*                  MOVE      arbmgs        aasmgsO74
307600140818      *riempimento da verificare
307700140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
307800140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
307900140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
308000140818
308100140818     c                   if        tpporto74 = 'A'
308200140818     c                   if        arbccm > 0
308300140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
308400140818     c                   else
308500140818     c                   movel     arblnp        ccmO74
308600140818     c                   move      8888          ccmO74
308700140818     c                   endif
308800140818     c                   else
308900140818     c                   z-add     arbksc        ccmO74
309000140818     c                   endif
309100140818      *
309200140604     c                   MOVEL     arblna        lineaaro74
309300140604     c                   MOVEL     arbtsp        tpservio74
309400140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
309500140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
309600140604     C                             rpyOpCode:rpyEsito)
309700140604     C                   EVAL      corro74 = §5EDes
309800140604     c*
309900140604     c                   EVAL      destino74 = arbrsd
310000140604     c                   EVAL      desindo74 = arbind
310100140604     c                   EVAL      descapo74 = arbcad
310200140604     c                   EVAL      desloco74 = arblod
310300140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
310400140604if  1c                   IF        arbnzd    = *blanks
310500140604     c                   EVAL      desnazo74 = arbprd
310600140604x   1c                   ELSE
310700150326     c**                 EVAL      desnazo74 = arbnzd
310800150326     c**                 EVAL      desnzdo74 = arbnzd
310900150326     c* 26/03/15: Attiviamo le specifiche per provincia export
311000150326     c                   EVAL      desnazo74 = arbprd
311100150326     c                   EVAL      desnzdo74 = arbnzd
311200150326e   1c                   ENDIF
311300140604     c*
311400140604     c                   EVAL      collio74  = arbncl
311500140604     c                   EVAL      pesoo74   = arbpkf
311600140604     c                   EVAL      volumeo74 = arbvlf
311700140604     c                   EVAL      naturao74 = arbnas
311800140604     c*
311900140604     c                   EVAL      disnumo74  = arbndc
312000140604c    c                   MOVEL     arbddc        wdat
312100140604     c                   EXSR      edtdat
312200140604     c                   EVAL      disdatao74 = wdate
312300140604     c*
312400140604     c                   EVAL      mito74    = arbrsm
312500140604     c                   EVAL      mitindo74 = arbinm
312600140604     c                   EVAL      mitcapo74 = arbcam
312700140604     c                   EVAL      mitloco74 = arblom
312800140604      *specifiche da eliminare dopo la modifica in immissione bolle
312900140604if  3c                   IF        arbnzm = *blanks
313000140604     c                   EVAL      mitnazo74 = arbprm
313100140604x   3c                   ELSE
313200140604     c                   EVAL      mitnazo74 = arbnzm
313300140604e   3c                   ENDIF
313400140604     c* 2a ragione sociale destinatario.
313500140604     c                   CLEAR                   tita4000
313600140604     c                   CLEAR                   tita4p00
313700140604     c                   Z-ADD     arbaas        ka4aas
313800140604     c                   Z-ADD     arblnp        ka4lnp
313900140604     c                   Z-ADD     arbnrs        ka4nrs
314000140604     c                   Z-ADD     arbnsp        ka4nsp
314100140604     c                   MOVEL     'D'           ka4trc
314200140604     c     keyta4        CHAIN     fiar401l                           99
314300140604if  1c                   IF        NOT *in99
314400140604     c                   MOVEL     ar4not        desti2O74
314500140604e   1c                   ENDIF
314600140604     c                   MOVEL     'E'           ka4trc
314700140604     c     keyta4        CHAIN     fiar401l                           99
314800140604if  1c                   IF        NOT *in99
314900140604     c                   MOVEL     ar4not        dsbl4e
315000140604e   1c                   ENDIF
315100140604     c
315200140604     ** Riferimento mittente numerico (c'è sempre).
315300140604     C                   EVAL      rifero74 = arbrmn
315400140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
315500140604     C                   IF        arbrma <> *blank and
315600140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
315700140604     C                   EVAL      rifero74a = arbrma
315800140604     C                   ENDIF
315900140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
316000140604     C                   IF        §b4erp <> *BLANK AND
316100140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
316200140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
316300140604     C                   EVAL      rpto74 = §b4erp
316400140604     C                   ENDIF
316500140604
316600140604     **?Reperisco il check code della spedizione.
316700140604     C                   EVAL      SChkCdeO74 =
316800140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
316900140604     C                             NSpedizI74:ChkCode:nEsito)
317000140604
317100140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
317200141126     C*m                 IF        KSCI74 <> *BLANK
317300141126     c*m                 SETOFF                                           98
317400140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
317500141126     C*m                 RESET                   tis7700i0
317600141126     C*m                 EVAL      tis7700i0.aas = arbAas
317700141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
317800141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
317900141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
318000141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
318100141126     C*m                 EVAL      tis7700i0.ksu = kscI74
318200141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
318300141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
318400141126     C*m                           : rpyEsito
318500141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
318600141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
318700141126     C*m                           )
318800141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
318900141126     C*m                           tis7700o0.bollValida = *ON)
319000140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
319100141126if  2c*m                 IF        NOT *in98                                      *non trovato
319200141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
319300141126     C*m                 reset                   TIS778DSO
319400141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
319500141126     C*m                 RETURN
319600141126     C*m                 ENDIF
319700141126e   2c*m                 ENDIF
319800141126     C*m                 ENDIF
319900140604     c*
320000140604     ** Importo assicurato.
320100140604     C                   IF        arbIas > 0
320200140604     C                   EVAL      vasO74 = arbVas
320300140604     C                   EVAL      iasO74 = arbIas
320400140604     C                   ENDIF
320500140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
320600140605     c                   if        arbdcm = 0 and wcev <> '704'
320700140617     c                   if        arbdcr > 0
320800140605     c                   MOVEL     arbdcr        wdat
320900140605     c                   EXSR      edtdat
321000140605     c                   eval      DTCONRCO74  =  wdate
321100140819if  3c                   IF        arbtcr = ' '
321200140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
321300140819e   3c                   ENDIF
321400140617     c                   endif
321500140617     c                   if        arbhcr > 0
321600140617     c                   MOVEL     arbhcr        wora
321700140617     c                   EXSR      edtora
321800140617     c                   eval      ORACONRO74  =  worae
321900140617     c                   endif
322000140606     c                   eval      TPDATCRO74  =  arbtcr
322100140605      *reperimento descrizione tipo consegna richiesta
322200140617if  3c                   IF        arbtcr = 'P'
322300140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
322400140617e   3c                   ENDIF
322500140617if  3c                   IF        arbtcr = 'D'
322600140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
322700140617e   3c                   ENDIF
322800140605     C                   clear                   tnsd99ds
322900140605     C                   MOVEL     ' '           D98tla
323000140605     C                   MOVEL     'A'           D98tbo
323100140605     C                   Z-ADD     arbaas        D98aas
323200140605     C                   Z-ADD     arblnp        D98lnp
323300140605     C                   Z-ADD     arbnrs        D98nrs
323400140605     C                   Z-ADD     arbnsp        D98nsp
323500140605     C*
323600140605     C                   call      'TNSD99R'
323700140605     C                   parm                    tnsd99ds
323800150304     c
323900140605     C*
324000140917      * chiamata a trulors per tempi standard
324100140917     c                   clear                   trulorsds
324200140917     c                   clear                   trulor2ds
324300140917     c                   clear                   trulor3ds
324400150611     c                   eval      wdee=d98dee
324500140917      * solo se questa data è valorizzata
324600150304    1c                   if        d98dee > 0
324700140912     c                   movel     d98dee        wdat
324800140605     c                   exsr      edtdat
324900140605     c                   eval      DTTEOCOO74  =  wdate
325000140819     c
325100140904      * se fermo deposito non richiamo pgm per reperimento orari
325200150304    2c                   if         arbffd = *blank
325300140605     c                   eval       IOREtser = 'C'
325400140605     c                   eval       IOREDDC=arbddc
325500140605     c                   eval       IOREnDC=arbndc
325600140609     c                   eval       IOREfil=arblna
325700140609     c                   eval       IOREdta=d98dce
325800140609     c                   eval       IOREcap=arbcad
325900140609     c                   eval       IOREloc=arblod
326000140609     c                   eval       IOREtfp=arbtfp
326100140609     c                   eval       IOREtfa=arbtfa
326200140610     c                   eval       IOREdti=arbdti
326300140610     c                   eval       IOREhti=arbhti
326400140610     c                   eval       IOREdcr=arbdcr
326500140610     c                   eval       IOREhcr=arbhcr
326600140610     c                   eval       IOREtcr=arbtcr
326700140819     c                   eval       IOREtsp=arbtsp
326800140819     c                   eval       IOREnar=arbnzd
326900140819     c                   eval       IOREdei=d98dei
327000140819     c                   eval       IOREoei=d98oei
327100150819     c                   clear                   diore01
327200150819     c                   movel     d98ttc        §ioretrazC
327300150819     c                   movel     d98tcc        §ioreconsC
327400150819     c                   eval       IOREflo=diore01
327500150325     c* passo la località normalizzata
327600150325     c                   clear                   tita4000
327700150325     c                   clear                   tita4p00
327800150325     c                   Z-ADD     arbaas        ka4aas
327900150325     c                   Z-ADD     arblnp        ka4lnp
328000150325     c                   Z-ADD     arbnrs        ka4nrs
328100150326     c                   Z-ADD     arbnsp        ka4nsp
328200150325     c                   MOVEL     'X'           ka4trc
328300150325     c     keyta4        CHAIN     tita430c                           99
328400150325if  1c                   IF        NOT *in99
328500150325     c                   MOVEL     ta4not        OOR3LOC_N
328600150325     c                   else
328700150325     c                   movel     arblod        OOR3LOC_N
328800150325e   1c                   ENDIF
328900140605     c
329000150304    3c                   IF         (ARBfbc = *blanks and
329100140605     c                               ARBddc > 0 and
329200140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
329300140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
329400140605     c                   eval       IOREcons = 'S'
329500150304    3c                   endif
329600140605     c* Orario presunto ritiro/consegna
329700140605     c                   move      arbndc        pctndc
329800140605     c                   move      arbifp        pctfgs
329900140605     c                   move      'COR'         ttrd
330000140605     c     kcet          setgt     fipct02l
330100140605     c     kcet          readpe    fipct02l
330200150304    3c                   if        not %eof(fipct02l)
330300140605     c                   movel     pctdati       fipctcords
330400140605     c                   eval      ioreorp=§PCTORASTI
330500140605     c                   endif
330600140605     c                   call      'TRULORSR'
330700140605     C                   PARM                    kpjba
330800140605     C                   PARM                    trulorsds
330900140605     C                   PARM                    trulor2ds
331000140605     C                   PARM                    trulor3ds
331100150304    3c                   endif
331200140818     c                   clear                   ORSTINIO74
331300150304    3c                   if        OOR2STIS > 0
331400140617     c                   MOVEL     OOR2STIS      wora
331500140617     c                   EXSR      edtora
331600140617     c                   eval      ORSTINIO74  =  worae
331700150304    3c                   endif
331800140818     c                   clear                   ORSTFINO74
331900150304    3c                   if        OOR2STFS > 0
332000140617     c                   MOVEL     OOR2STFS      wora
332100140617     c                   EXSR      edtora
332200140617     c                   eval      ORSTFINO74  =  worae
332300150304    3c                   endif
332400140818     c                   clear                   ORMIINIO74
332500150304    3c                   if        OOR2MIIS > 0
332600140617     c                   MOVEL     OOR2MIIS      wora
332700140617     c                   EXSR      edtora
332800140617     c                   eval      ORMIINIO74  =  worae
332900150304    3c                   endif
333000140818     c                   clear                   ORMAFINO74
333100150304    3c                   if        OOR2MXFS > 0
333200140617     c                   MOVEL     OOR2MXFS      wora
333300140617     c                   EXSR      edtora
333400140617     c                   eval      ORMAFINO74  =  worae
333500150304    3c                   endif
333600150304    3c                   if        wricons = 'S'
333700140818     c                   clear                   ORRIDALO74
333800150304    4c                   if        OOR2RIDS > 0
333900140617     c                   MOVEL     OOR2RIDS      wora
334000140617     c                   EXSR      edtora
334100140617     c                   eval      ORRIDALO74  =  worae
334200150304    4c                   endif
334300140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
334400140818     c                   clear                   ORRIALLO74
334500150304    4c                   if        OOR2RIAS > 0
334600140617     c                   MOVEL     OOR2RIAS      wora
334700140617     c                   EXSR      edtora
334800140617     c                   eval      ORRIALLO74  =  worae
334900150304    4c                   endif
335000140617     c                   eval      FLRICONO74  =  OOR2FRIC
335100150304   x3c                   else
335200140711     c                   clear                   ORRIALLO74
335300140711     c                   clear                   ORRIDALO74
335400140711     c                   eval      FLRICONO74  =  'N'
335500150304    3c                   endif
335600140818     c                   clear                   ORSTICDO74
335700150304    3c                   if        OOR2PRESD > 0
335800140617     c                   MOVEL     OOR2PRESD     wora
335900140617     c                   EXSR      edtora
336000140617     c                   eval      ORSTICDO74  =  worae
336100150304    3c                   endif
336200140818     c                   clear                   ORSTICAO74
336300150304    3c                   if        OOR2PRESa > 0
336400140617     c                   MOVEL     OOR2PRESa     wora
336500140617     c                   EXSR      edtora
336600140617     c                   eval      ORSTICAO74  =  worae
336700150304    3c                   endif
336800140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
336900140605     c                   eval      FLLOCNOO74  =  OOR3NORM
337000140904      *end F.D.
337100150304    2c                   endif
337200140605      * RIFERIMENTI DESTINATARIO
337300140605     c                   MOVEL     'EMD'         kr5trd
337400140605     c     keyAR5        CHAIN     fiar501l
337500150304if  2c                   IF        %found(fiar501l)
337600140605     c                   MOVEL     ar5uni        dar5emd
337700140606     c                   eval      EMLDESTO74  =  §§AR5EML
337800140606     c                   eval      CELDESTO74  =  §§AR5TEL
337900150304e   2c                   ENDIF
338000140604     **
338100150304     c*
338200150304     c* Stato data prevista consegna
338300150305     c                   clear                   stat_n2o74
338400150305     c                   eval      STATUSo74   =  D98SPCDEE
338500150310     c* Se lo STATUS è "in distinta" e da pda ho ricevuto l'evento AVV da PDA
338600150312     c* se si tratta del primo evento scrivo ( quindi tentativi = 0)
338700150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc=0
338800150312     c                   eval      statuso74='RIC'
338900150403     c* se presente FIARP --> visualizzo la data richiesta
339000150403     c     karp          chain     fiarp01l
339100150421    2c                   if        %found(fiarp01l) and arpdcr>0 and arptcr=' '
339200150611     c                   movel     arpdcr        wdee
339300150611     c                   movel     arpdcr        wdat
339400150403     c                   exsr      edtdat
339500150403     c                   eval      DTTEOCOO74  =  wdate
339600150403     c* Se immessa ORA RICHIESTA --> LA MEMORIZZO SOLO PER DATA "IL"
339700150421    3C                   IF        ARPHCR>0
339800150403     c                   eval      orarich=arphcr
339900150421    3c                   endif
340000150421   x2c                   else
340100150403     c*  se presente DOPO IL imposto la data in D98dee
340200150421    3c                   if        %found(fiarp01l) and arpdcr>0 and arptcr='D'
340300150403     c                   eval      D98dee=arpdcr
340400150611     c                   eval      wdee=arpdcr
340500150421    3c                   endif
340600150403     c* PRIMA DEL come se non ci fosse...
340700150403     c* altrimenti la data prevista consegna viene aumentata di un giorno lavorativo
340800150421    3c                   if        d98dee>0
340900150312     c                   Clear                   xgiolavds
341000150312     c                   Eval      Ixglpa  = 'A'
341100150312     c                   Eval      IxglFil = arblna
341200150312     c                   Eval      Ixgldata = d98dee
341300150312     c                   Eval      Ixgladd = 'S'
341400150312     c                   Eval      Ixgllav = 'S'
341500150312     c                   eval      ixglgg  = 1
341600150312     c                   Call      'XGIOLAV'
341700150312     c                   parm                    xgiolavds
341800150611     c                   movel     oxgldata      wdee
341900150611     c                   movel     oxgldata      wdat
342000150312     c                   exsr      edtdat
342100150312     c                   eval      DTTEOCOO74  =  wdate
342200150421    3c                   endif
342300150421    2c                   endif
342400150421    1c                   endif
342500150421     c
342600150421    1c                   if        PDAavv='S' and statuso74='DDC'  and arbntc>0
342700150421     c                   eval      statuso74='ATT'
342800150421     c                   endif
342900150421    1c                   if        PDAgia='S' and statuso74='DDC'
343000150421     c                   eval      statuso74='GIA'
343100150421     c                   endif
343200150401     c
343300150401     c* Se lo STATUS è "in distinta" ed è fermo deposito
343400150401     c*  calcolo data distinta + 1 per metterlo nel messaggio
343500150512     c**                 if        statuso74='FD2' and d98dtctd<=*zeros
343600150401     c* La data contatto = data distinta + 1
343700150512     c**                 Clear                   xgiolavds
343800150512     c**                 Eval      Ixglpa  = 'A'
343900150512     c**                 Eval      IxglFil = arblna
344000150512     c**                 Eval      Ixgldata = arbddc
344100150512     c**                 Eval      Ixgladd = 'S'
344200150512     c**                 Eval      Ixgllav = 'S'
344300150512     c**                 eval      ixglgg  = 1
344400150512     c**                 Call      'XGIOLAV'
344500150512     c**                 parm                    xgiolavds
344600150512     c**                 movel     oxgldata      wdat
344700150512     c**                 eval      D98DTCTD    =  %editc(wdat:'X')
344800150512     c**                 endif
344900150317     c
345000150317     c* Se lo status è prevista consegna "PRV" e l'ultimo evento è RIC
345100150326     c*  lo status diventa RIC
345200150317     c                   if        statuso74='PRV'  and cev(1)='RIC'
345300150317     c                   eval      statuso74='RIC'
345400150317     c                   endif
345500150310     c
345600150304     c* decodifica stato da mettere nei campi deco e note
345700150304     c                   clear                   dspc
345800150304     C                   RESET                   tibs02ds
345900150304     C                   EVAL      t02Cod = 'SPC'
346000150312     C                   EVAL      t02Ke1 = statuso74
346100150306     C                   EXSR      getTntbe00f
346200150306     c                   IF        t02Err <>'E'
346300150306     c
346400150305     c                   eval       stat_d1o74=§SPCDEC1
346500150305     c                   eval       stat_d2o74=§SPCDEC2
346600150304     c*
346700150304     c* se il campo note iniziano con TIS significa che si tratta di un msg
346800150305     c                   eval       stat_n1o74=§SPCNOT1
346900150306     c                   eval       stat_n2o74=§SPCNOT2
347000150304     c                   else
347100150305     c                   clear                   stat_d1o74
347200150305     c                   clear                   stat_d2o74
347300150305     c                   clear                   stat_n1o74
347400150306     c                   clear                   stat_n2o74
347500150304     c                   endif
347600150409     c                   else
347700150409     c* stato evento CONSEGNATO
347800150409     c                   eval      statuso74='CON'
347900150415     c                   eval      flgbolcO74  =  'S'
348000150415     c                   eval      flgboldO74  =  'N'
348100150409    1c                   endif
348200150304     c
348300150304     c
348400140919     ** riempimento per campi da arb con data consegna diversa da 0
348500140919     c                   if        arbdcm > 0
348600140919     c                   movel     arbdcm        wdat
348700140919     c                   exsr      edtdat
348800140919     c                   eval      dtconmeo74  = wdate
348900140919     c                   MOVEL     arbdcm        wora
349000140919     c                   EXSR      edtora
349100140919     c                   eval      orconmeO74  =  worae
349200140919     c                   eval      flgbolcO74  =  'S'
349300140919     c                   eval      flgboldO74  =  'N'
349400141015      *firmatario distinta PDA
349500141015     c                   clear                   firmato74
349600141015     c                   if        gen§ar5pdaco <> 'NO'
349700141015     c                   exsr      repfirmatARB
349800141015     c                   else
349900141015     c                   if        arbgma <> *blank
350000141015     C                   EVAL      tblKey = arbgma
350100141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
350200141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
350300141015     C                             :rpyOpCode:rpyEsito)
350400141015     C                   IF        rpyOpCode = 'FOUND'
350500141015     c                   if        §7r2fi = 'S'
350600141015     c                   exsr      repfirmatARB
350700141015     c                   endif
350800141015     c                   endif
350900141015     c                   endif
351000141015     c                   endif
351100140919     c                   else
351200150415     c* solo se non impostato da PdA lo stato CONSEGNA
351300150415     c                   if        statuso74<>'CON'
351400150415     c
351500140919     c                   eval      flgbolcO74  =  'N'
351600140919     c                   if        arbndc > 0 and arbddc > 0
351700150415     c                             and arbddc <= datcor and d98cons='S'
351800140919     c                   eval      flgboldo74 = 'S'
351900140919     c                   else
352000140919     c                   eval      flgboldO74  =  'N'
352100140919     c                   endif
352200150415     c                   endif
352300150415
352400140919     c                   endif
352500150415     c
352600150325     c                   if        ORSTICDO74 <> *blank and wricons=' '
352700140919     c                   eval      orincono74 = orsticdo74
352800140919     c                   eval      orficono74 = ORSticaO74
352900140919     c                   else
353000140919     c                   eval      orincono74 = ORSTINIO74
353100140919     c                   eval      orficono74 = ORSTFINO74
353200140919     c                   endif
353300150326     c* note 1
353400150309     c                   if        %subst(stat_n1o74:1:3)='TIS'
353500150309     c                   clear                   wparm
353600150326     c* Se la data contatto è piena passo questa, altrimenti la consegna prevista
353700150326     c                   if        D98DTCTD<>*blanks
353800150326     c                   movel     D98DTCTD      wdat
353900150326     c                   exsr      edtdat
354000150326     c                   else
354100150326     c                   eval      wdate=dtteocoo74
354200150410     c                   eval      usaprev='S'
354300150326     c                   endif
354400150326     c
354500150326                         eval      wparm=wdate + ORINCONO74 + orficono74 ;
354600150309     c                   eval      stat_n1o74= rtvMsgLang(
354700150309     c                             %subst(stat_n1o74:1:7):langI74:wparm)
354800150309     c                   endif
354900150309     c* note 2
355000150309     c                   if        %subst(stat_n2o74:1:3)='TIS'
355100150309     c                   clear                   wparm
355200150403     c* Se lo stato è PRG passo orario limite uscita aut
355300150403     c                   select
355400150403     c                   when      D98SPCDEE='PRG'
355500150326     c                   IF        d98picklim  >*zeros
355600150309
355700150326     c                   eval      wparm=(%subst(d98picklim:1:2))+':'+
355800150326     c                                   (%subst(d98picklim:3:2))
355900150309     c                   else
356000150309     c                   eval      wparm='09:00'
356100150403     c* reperisco orario di fine picking
356200150403     c* filiale / data
356300150403     C                   RESET                   tibs02ds
356400150403     C                   EVAL      t02Cod = 'OLP'
356500150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
356600150403     C                   EVAL      t02Ke2 = %editc(datcor:'X')
356700150403     C                   EXSR      getTntbe00f
356800150403     c* filiale
356900150403     c                   IF        t02Err = 'E'
357000150403     C                   RESET                   tibs02ds
357100150403     C                   EVAL      t02Cod = 'OLP'
357200150403     C                   EVAL      t02Ke1 = %editc(arblna:'X')
357300150403     C                   EXSR      getTntbe00f
357400150403     c                   endif
357500150403     c* Generico 000
357600150403     c                   IF        t02Err = 'E'
357700150403     C                   RESET                   tibs02ds
357800150403     C                   EVAL      t02Cod = 'OLP'
357900150403     C                   EVAL      t02Ke1 = '000'
358000150403     C                   EXSR      getTntbe00f
358100150403     c                   endif
358200150403     c                   if        t02err<>'E'  and %subst(t02uni:1:4)>*zeros
358300150403     c                   eval      wparm=%subst(t02uni:1:2)+':'+
358400150403     c                             %subst(t02uni:3:2)
358500150309     c                   endif
358600150403     c                   endif
358700150403     c                   eval      stat_n2o74= rtvMsgLang(
358800150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
358900150403     c                   other
359000150403     c* altri casi
359100150410     c* se c'è almeno un tentativo di consegna e presente orario per IL e la data >= alla prevista
359200150410     c                   if        arbntc>0 and arbhcr>0 and orarich=0 and
359300150611     c                             arbtcr=' ' and arbdcr>0  and
359400150611     c                             arbdcr>=wdee
359500150410     c
359600150403     c                   eval      orarich=arbhcr
359700150410     c                   endif
359800150403     c
359900150403     c                   if        orarich>0
360000150403     c                   exsr      sr_dallealle
360100150403     c                   eval      wparm=w_dALLEmsg + W_ALLEMSG
360200150403     c                   eval      stat_n2o74= rtvMsgLang(
360300150403     c                             %subst(stat_n2o74:1:7):langI74:wparm)
360400150403     c
360500150403     c                   else
360600150403     c                   clear                   stat_n2o74
360700150403     c                   endif
360800150403     c
360900150403     c                   endsl
361000150403     c
361100150309     c                   endif
361200150410     c
361300150424     c* Se si tratta di fermo deposito pulisco sempre
361400150424     c                   if        usaprev<>'S' or arbffd='S'
361500150410     c                   clear                   DTTEOCOO74
361600150410     c                   clear                   ORINCONO74
361700150410     c                   clear                   ORFICONO74
361800150410     c                   endif
361900150309     c
362000140604     c                   ENDSR
362100150403      /free
362200150403       //--------------------------------------------------------------
362300150403       //?Determinazione del "Dalle - Alle" per messagio di conferma   ?
362400150403       //--------------------------------------------------------------
362500150403       BEGSR  sr_dallealle;
362600150403       clear  w_dallemsg ;
362700150403       clear  w_allemsg ;
362800150403
362900150403       // Reperimento dati tabella VPO - ORARISER
363000150403       if §VPORST_MW=0  ;
363100150403       clear tibs02ds;
363200150403       clear dvpooraris;
363300150403       t02mod='C';
363400150403       t02cod='VPO';
363500150403       t02ke1='ORARISER';
363600150403       tibs02r(kpjba:tibs02ds);
363700150403       if t02err=*blanks;
363800150403          dvpooraris=t02uni;
363900150403       endif;
364000150403       endif;
364100150403
364200150403            w_dalle=%time((%dec(orarich:4:0)*100):*hms)-%minutes(§VPORST_MW);
364300150403            w_alle =%time((%dec(orarich:4:0)*100):*hms)+%minutes(§VPORST_PW);
364400150403       //   determino orari servizio comprensivi della tolleranza
364500150403               w_dalleoser=%time((oor2stis *100):*hms)-%minutes(§VPORST_MT);
364600150403               w_alleoser=%time((oor2stfs *100):*hms)+%minutes(§VPORST_MT);
364700150403
364800150403            if w_dalle<w_dalleoser;
364900150403               w_dalle=w_dalleoser;
365000150403               w_alle=w_dalle+%minutes(§VPORST_MW + §VPORST_PW);
365100150403            endif;
365200150403
365300150403            if w_alle>w_alleoser;
365400150403               w_alle=w_alleoser;
365500150403               if w_alle-%minutes(§VPORST_MW + §VPORST_PW)>=w_dalleoser;
365600150403                  w_dalle=w_alle-%minutes(§VPORST_MW + §VPORST_PW);
365700150403               endif;
365800150403            endif;
365900150403            w_dallemsg=%subst(%editW(%dec(w_dalle):'  :  :  '):1:5);
366000150403            w_allemsg=%subst(%editw(%dec(w_alle):'  :  :  '):1:5);
366100150403
366200150403       endsr;
366300150403      /end-free
366400010717     c*--------------------------------------------------------------------------------------------*
366500010717     c* imposta i campi della DS di Output dagli eventi
366600010717     c*--------------------------------------------------------------------------------------------*
366700010717     c     RepAbilFirma  BEGSR
366800010717     c*
366900010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
367000010717     c* e recupera utente / password
367100010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
367200010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
367300010717     c     keyvtd        SETLL     tivtd01l
367400010717     c     keyvtd        READE     tivtd01l                               98
367500010717do  2c                   DOW       NOT *in98
367600010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
367700010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
367800010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
367900010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
368000010717     c                   EVAL      depute = vtdute                              *utente
368100010717     c                   EVAL      deppwd = vtdpwd                              *password
368200010717     c                   LEAVE                                                  *esce dal ciclo
368300010717e   3c                   ENDIF
368400010717     c     keyvtd        READE     tivtd01l                               98
368500010717e   2c                   ENDDO
368600010717e   1C                   ENDIF
368700010717     c*
368800010717     c                   ENDSR
368900000000     c*--------------------------------------------------------------------------------------------*
369000000000     c* edita data da aaaammgg in gg/mm/aaaa
369100000000     c*--------------------------------------------------------------------------------------------*
369200000000     c     edtdat        BEGSR
369300000000     c*
369400000000     c                   MOVEL     *blanks       wdate
369500000000     c*
369600000000     c                   MOVEL     wdat          dsdat
369700000000     c                   MOVEL     dsgio         dsgioe
369800140528     c                   MOVEL     '.'           dsvd1
369900000000     c                   MOVEL     dsmes         dsmese
370000140528     c                   MOVEL     '.'           dsvd2
370100000000     c                   MOVEL     dsann         dsanne
370200000000     c                   MOVEL     dsdate        wdate
370300000000     c*
370400000000     c                   ENDSR
370500000000     c*--------------------------------------------------------------------------------------------*
370600000000     c* edita ora da hhmm in hh:mm
370700000000     c*--------------------------------------------------------------------------------------------*
370800000000     c     edtora        BEGSR
370900000000     c*
371000000000     c                   MOVEL     *blanks       worae
371100000000     c*
371200000000     c                   MOVEL     wora          dsora
371300000000     c                   MOVEL     dshh          dshhe
371400140617     c                   MOVEL     '.'           dsvo1
371500000000     c                   MOVEL     dsmm          dsmme
371600000000     c                   MOVEL     dsorae        worae
371700000000     c*
371800000000     c                   ENDSR
371900000000     c*--------------------------------------------------------------------------------------------*
372000000000     c* decodifica punto operativo
372100000000     c*--------------------------------------------------------------------------------------------*
372200000000     c     decorg        BEGSR
372300000000     c*
372400000000     c                   MOVEL     *blanks       wodes                          *descrizione
372500000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
372600000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
372700000000     c*
372800000000     c                   SETOFF                                           98
372900041111     c                   Z-ADD     1             O
373000041111     c     worg          LOOKUP    sorg(O)                                98
373100000000if  1c                   IF        *in98                                        *trovata
373200121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
373300121001     c                             %EDITC(wOrg : 'X') + ')'
373400041111     c                   MOVEL     sofl1(O)      wofl1
373500041111     c                   MOVEL     sodpd(O)      wodpd
373600000000e   1c                   ENDIF
373700000000     c*
373800000000     c                   ENDSR
373900000000     c*--------------------------------------------------------------------------------------------*
374000000000     c* decodifica evento per STATI
374100000000     c*--------------------------------------------------------------------------------------------*
374200000000     c     deccevsta     BEGSR
374300000000     c*
374400000000     c                   MOVEL     '0'           werr                           *NO errore
374500050704     c*
374600050704     C                   IF        wcev = 'GEN'
374700050704     C                   EVAL      werr = *ON
374800050704     C                   CLEAR                   wcdex
374900050704     C                   LEAVESR
375000050704     C                   ENDIF
375100000000     c*
375200060622     C                   RESET                   tibs02ds
375300060622     C                   EVAL      t02Cod = 'ICE'
375400060622     C                   EVAL      t02Ke1 = wcev
375500060622     C                   EXSR      getTntbe00f
375600060622     C                   IF        t02Err <> 'E' AND
375700140528     C                             (§icesta = 'S'  OR
375800140528     C                               §icestaP = 'S' )
375900060622     c                   MOVEL     §icedei       wcdex
376000000000x   1c                   ELSE                                                   *non trovato
376100000000     c                   MOVEL     '1'           werr                           *SI errore
376200030130     c                   MOVEL     *blanks       wcdex
376300000000e   1c                   ENDIF
376400000000     c*
376500000000     c                   ENDSR
376600000000     c*--------------------------------------------------------------------------------------------*
376700000000     c* decodifica consegna particolare
376800000000     c*--------------------------------------------------------------------------------------------*
376900000000     c     decftc        BEGSR
377000000000     c*
377100000000     c                   MOVEL     *blanks       wdftc
377200000000     c*
377300000000if  1c                   IF        wftc = 'S'
377400060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
377500000000e   1c                   ENDIF
377600000000if  1c                   IF        wftc = 'P'
377700060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
377800000000e   1c                   ENDIF
377900000000if  1c                   IF        wftc = 'A'
378000060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
378100000000e   1c                   ENDIF
378200000000if  1c                   IF        wftc = 'F'
378300060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
378400000000e   1c                   ENDIF
378500000000     c*
378600000000     c                   ENDSR
378700000000     c*--------------------------------------------------------------------------------------------*
378800000000     c* decodifica turno di chiusura
378900000000     c*--------------------------------------------------------------------------------------------*
379000000000     c     decgcx        BEGSR
379100000000     c*
379200000000     c                   MOVEL     *blanks       wdgcx
379300000000     c* giorno
379400000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
379500000000if  1c                   IF        a1 = '1'
379600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
379700000000e   1c                   ENDIF
379800000000if  1c                   IF        a1 = '2'
379900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
380000000000e   1c                   ENDIF
380100000000if  1c                   IF        a1 = '3'
380200060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
380300000000e   1c                   ENDIF
380400000000if  1c                   IF        a1 = '4'
380500060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
380600000000e   1c                   ENDIF
380700000000if  1c                   IF        a1 = '5'
380800060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
380900000000e   1c                   ENDIF
381000000000if  1c                   IF        a1 = '6'
381100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
381200000000e   1c                   ENDIF
381300000000if  1c                   IF        a1 = '7'
381400060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
381500000000e   1c                   ENDIF
381600000000if  1c                   IF        a1 = ' '
381700000000     c                   EVAL      wdgcx = *blanks
381800000000e   1c                   ENDIF
381900000000     c* am/pm
382000000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
382100000000if  1c                   IF        a1 = ' '
382200060612     c                   EVAL      wdgcx =
382300060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
382400000000e   1c                   ENDIF
382500000000if  1c                   IF        a1 = 'M'
382600060612     c                   EVAL      wdgcx =
382700060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
382800000000e   1c                   ENDIF
382900000000if  1c                   IF        a1 = 'P'
383000060612     c                   EVAL      wdgcx =
383100060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
383200000000e   1c                   ENDIF
383300000000     c*
383400000000     c                   ENDSR
383500000000     c*--------------------------------------------------------------------------------------------*
383600000000     c* decodifica evento per GIACENZE
383700000000     c*--------------------------------------------------------------------------------------------*
383800000000     c     deccevgia     BEGSR
383900000000     c*
384000000000     c                   MOVEL     '0'           werr                           *NO errore
384100000000     c*
384200060622     C                   RESET                   tibs02ds
384300060622     C                   EVAL      t02Cod = 'ICE'
384400060622     C                   EVAL      t02Ke1 = wcev
384500060622     C                   EXSR      getTntbe00f
384600060622     C                   IF        t02Err <> 'E' AND                            *trovata
384700140528     C                             (§icegia = 'S'  OR
384800140528     C                               §icegiap = 'S')
384900060622     c                   MOVEL     §icedei       wcdex
385000000000x   1c                   ELSE                                                   *non trovato
385100000000     c                   MOVEL     '1'           werr                           *SI errore
385200030130     c                   MOVEL     *blanks       wcdex
385300000000e   1c                   ENDIF
385400000000     c*
385500000000     c                   ENDSR
385600000000     c*--------------------------------------------------------------------------------------------*
385700000000     c* Controlla la DS dsi Input
385800000000     c*--------------------------------------------------------------------------------------------*
385900000000     c     chkdsinput    BEGSR
386000000000     c*
386100000000     c                   MOVEL     '0'           werr                           *NO errore
386200000000     c*
386300000000     c* dalla DS di Input scompone il codice spedizione
386400000000     c                   MOVEL     nspedizi74    dsspedizi74
386500000000     c*
386600000000     c* controlla che la lina di partenza sia TUTTA numerica
386700000000     c                   SETOFF                                       99
386800000000     c                   TESTN                   dslnpi74             99
386900000000if  1c                   IF        NOT *in99                                    *non è un numero
387000000000     c                   MOVEL     '1'           werr                           *SI errore
387100140617     c                   eval      widmsg = 'TIS0769'
387200140617     c                   eval      widcampo = '*LNP'
387300140617     c                   clear                   wparm
387400140617     c                   exsr      messaggi
387500000000x   1c                   ELSE                                                   *è un numero
387600000000     c                   MOVE      dslnpi74      a1
387700000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
387800000000     c                   MOVEL     '1'           werr                           *SI errore
387900140617     c                   eval      widmsg = 'TIS0769'
388000140617     c                   eval      widcampo = '*LNP'
388100140617     c                   clear                   wparm
388200140617     c                   exsr      messaggi
388300000000e   2c                   ENDIF
388400000000e   1c                   ENDIF
388500000000     c*
388600000000     c* controlla che la serie spedizione sia TUTTA numerica
388700000000     c                   SETOFF                                       99
388800000000     c                   TESTN                   dsnrsi74             99
388900000000if  1c                   IF        NOT *in99                                    *non è un numero
389000000000     c                   MOVEL     '1'           werr                           *SI errore
389100140617     c                   eval      widmsg = 'TIS0769'
389200140617     c                   eval      widcampo = '*NRS'
389300140617     c                   clear                   wparm
389400140617     c                   exsr      messaggi
389500000000x   1c                   ELSE                                                   *è un numero
389600000000     c                   MOVE      dsnrsi74      a1
389700000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
389800000000     c                   MOVEL     '1'           werr                           *SI errore
389900140617     c                   eval      widmsg = 'TIS0769'
390000140617     c                   eval      widcampo = '*NRS'
390100140617     c                   clear                   wparm
390200140617     c                   exsr      messaggi
390300000000e   2c                   ENDIF
390400000000e   1c                   ENDIF
390500000000     c*
390600000000     c* controlla che il numero spedizione sia TUTTO numerico
390700000000     c                   SETOFF                                       99
390800000000     c                   TESTN                   dsnspi74             99
390900000000if  1c                   IF        NOT *in99                                    *non è un numero
391000000000     c                   MOVEL     '1'           werr                           *SI errore
391100140617     c                   eval      widmsg = 'TIS0769'
391200140617     c                   eval      widcampo = '*NSP'
391300140617     c                   clear                   wparm
391400140617     c                   exsr      messaggi
391500000000x   1c                   ELSE                                                   *è un numero
391600000000     c                   MOVE      dsnspi74      a1
391700000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
391800000000     c                   MOVEL     '1'           werr                           *SI errore
391900140617     c                   eval      widmsg = 'TIS0769'
392000140617     c                   eval      widcampo = '*NSP'
392100140617     c                   clear                   wparm
392200140617     c                   exsr      messaggi
392300000000e   2c                   ENDIF
392400000000e   1c                   ENDIF
392500110104     c*
392600110104     c* controlla che il codice cliente sia TUTTO numerico
392700110104     c                   IF        kscI74 <> *BLANK
392800110104     c                   SETOFF                                       99
392900110104     c                   TESTN                   kscI74               99
393000110104     c                   IF        NOT *in99                                    *non è un numero
393100110104     c                   MOVEL     '1'           werr                           *SI errore
393200140617     c                   eval      widmsg = 'TIS0769'
393300140617     c                   eval      widcampo = 'KSCI74'
393400140617     c                   clear                   wparm
393500140617     c                   exsr      messaggi
393600110104     c                   ELSE                                                   *è un numero
393700110104     c                   MOVE      kscI74        a1
393800110104     c                   IF        a1 < '0'                                     *trovata una lettera
393900110104     c                   MOVEL     '1'           werr                           *SI errore
394000140617     c                   eval      widmsg = 'TIS0769'
394100140617     c                   eval      widcampo = 'KSCI74'
394200140617     c                   clear                   wparm
394300140617     c                   exsr      messaggi
394400110104     c                   ENDIF
394500110104     c                   ENDIF
394600110104     c                   ENDIF
394700110104     c*
394800110104     c* controlla che il codice mittente sia TUTTO numerico
394900110104     c                   IF        ccmI74 <> *BLANK
395000110104     c                   SETOFF                                       99
395100110104     c                   TESTN                   ccmI74               99
395200110104     c                   IF        NOT *in99                                    *non è un numero
395300110104     c                   MOVEL     '1'           werr                           *SI errore
395400140617     c                   eval      widmsg = 'TIS0769'
395500140617     c                   eval      widcampo = 'CCMI74'
395600140617     c                   clear                   wparm
395700140617     c                   exsr      messaggi
395800110104     c                   ELSE                                                   *è un numero
395900110104     c                   MOVE      ccmI74        a1
396000110104     c                   IF        a1 < '0'                                     *trovata una lettera
396100110104     c                   MOVEL     '1'           werr                           *SI errore
396200140617     c                   eval      widmsg = 'TIS0769'
396300140617     c                   eval      widcampo = 'CCMI74'
396400140617     c                   clear                   wparm
396500140617     c                   exsr      messaggi
396600110104     c                   ENDIF
396700110104     c                   ENDIF
396800110104     c                   ENDIF
396900050104     c*
397000050104     c* controlla anno spedizione
397100050104     c                   TESTN                   cAASI74              99
397200050104if  1c                   IF        NOT *in99                                    *non è un numero
397300050104     c                   EVAL      cAASI74 = *ZERO
397400050104x   1c                   ENDIF
397500100222     c*
397600100223     c* Estraggo l'anno dalla data spedizione.
397700100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
397800100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
397900100223     C                   IF        dspI74 <> *BLANK
398000100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
398100100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
398200100223     C                   MONITOR
398300100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
398400100223     C                   ON-ERROR
398500100223     C                   ENDMON
398600100222     C                   ENDIF
398700000000     c*
398800000000     c* se errore, esce dal programma
398900140610if  1c                   IF        werr = '1'                                   *SI errore
399000140610     c                   eval      esito = werr                                 *errore
399100140610e   1c                   ENDIF
399200000000     c*
399300000000     c                   ENDSR
399400000000     c*--------------------------------------------------------------------------------------------*
399500000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
399600000000     c* Input:   Walfa7 = Alfanumerico di 7
399700000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
399800000000     c*--------------------------------------------------------------------------------------------*
399900000000     c     xallinea1     BEGSR
400000000000     c*
400100000000     c* reimposta variabli di lavoro
400200000000     c                   CLEAR                   i
400300000000     c                   CLEAR                   z
400400000000     c                   CLEAR                   walfa7bis
400500000000     c                   EVAL      wzeri = *ALL'0'
400600000000     c*
400700000000     c     ' '           CHECKR    walfa7:7      i
400800000000if  1c                   IF        i <> 0
400900000000     c     i             SUBST     walfa7        walfa7bis
401000000000e   1c                   ENDIF
401100000000if  1c                   IF        walfa7bis <> *blanks
401200000000     c     7             SUB       i             z
401300000000if  2c                   IF        z <> 0
401400000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
401500000000e   2c                   ENDIF
401600000000     c                   EVAL      z = z+ 1
401700000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
401800000000e   1c                   ENDIF
401900000000     c*
402000000000     c                   ENDSR
402100000000     c*--------------------------------------------------------------------------------------------*
402200000000     c* caricamento tabelle occorrenti
402300000000     c*--------------------------------------------------------------------------------------------*
402400000000     c     cartab        BEGSR
402500000000     c*---
402600000000     c* organigrama
402700000000     c*---
402800000000     c                   CLEAR                   i                              *indice
402900000000     C                   CLEAR                   sorg
403000000000     C                   CLEAR                   sodes
403100000000     C                   CLEAR                   sofl1
403200000000     C                   CLEAR                   sopt
403300000000     C                   CLEAR                   sodpd
403400060623     C                   OPEN      tntbe01l
403500000000     c     *loval        SETLL     azorg01l
403600000000     c                   READ      azorg01l                               99
403700000000do  1c                   DOW       NOT *in99
403800000000     c                   ADD       1             i
403900000000     c                   Z-ADD     orgfil        sorg(i)
404000000000     c                   MOVEL     orgdes        sodes(i)
404100000000     c                   MOVEL     orgde3        og143
404200020702     C*
404300020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
404400020702     C                   CLEAR                   dntw
404500020702     C*
404600020702     C                   MOVEL(P)  'NTW'         kbecod
404700020702     C                   MOVEL(P)  §ogntw        kbeke1
404800020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
404900020702     c                   MOVEL     *blanks       kbelin                         *lingua
405000020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
405100020702     C     keytbe        CHAIN     tntbe01l
405200020702     C                   IF        %found(tntbe01l)
405300020702     C                   MOVEL     tbeuni        dntw
405400020702     C                   ENDIF
405500020702     C*
405600020702     C                   MOVEL     §ntwfie       sofl1(i)
405700020702     c                   MOVEL     §ogntw        sopt(i)
405800020702     c                   MOVEL     §ogntw        sodpd(i)
405900000000     c                   READ      azorg01l                               99
406000000000e   1c                   ENDDO
406100060623     C                   CLOSE     tntbe01l
406200000000     c*---
406300000000     c                   ENDSR
406400000000     c*--------------------------------------------------------------------------------------------*
406500000000     c* operazioni iniziali -da fare sempre-
406600000000     c*--------------------------------------------------------------------------------------------*
406700000000     c     rutinz        BEGSR
406800000000     c*
406900000000     c* reperimento data corrente
407000000000     C                   TIME                    n14                            *ora (6)+ data (8)
407100000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
407200000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
407300000000     C                   Z-ADD     n8            g08dat
407400000000     C                   Z-ADD     *zeros        g08inv
407500000000     C                   MOVEL     '0'           g08err
407600000000     c                   CALL      'XSRDA8'
407700000000     c                   PARM                    wlbda8
407800000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
407900000000     C                   MOVEL     datcor        anncor                         *anno corrente
408000000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
408100000000     c*
408200000000     c* calcola data corrente - 1 anno
408300000000     c                   Z-ADD     datcor        datpre                         *data precedente
408400000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
408500000000     c*
408600010716     c* azzera la DS di Output e le variabili di lavoro / controllo
408700000000     c                   MOVEL     '0'           esito
408800140603     c                   reset                   tis778dso                      *ds Output
408900140603     c                   reset                   tis778dsm                      *ds Output
409000000000     c                   CLEAR                   smi                            *di memorizzazione
409100000000     c                   CLEAR                   smseql
409200000000     c                   CLEAR                   smseqe
409300000000     c                   CLEAR                   smdevhev
409400000000     c                   CLEAR                   smdeve
409500000000     c                   CLEAR                   smheve
409600000000     c                   CLEAR                   smfle
409700000000     c                   CLEAR                   smdev
409800000000     c                   CLEAR                   smcev
409900130312     c                   CLEAR                   smspe
410000000000     c                   CLEAR                   smdmc
410100000000     c                   CLEAR                   smdmcR
410200000000     c                   CLEAR                   tote                           *contatore eventi
410300000000     c                   CLEAR                   seql                           *sequenza legami
410400010716     c                   RESET                   $AbTD                          *di lavoro
410500010716     c                   RESET                   $AbFirma
410600150312     c                   clear                   pdaavv
410700150421     c                   clear                   pdagia
410800140603     c* parametri ingresso
410900140603     c                   exsr      Kontrolla
411000000000     c* controlla da DS in Input
411100000000     c                   EXSR      chkdsinput
411200000000     c*
411300000000     c* carica i clienti dettaglio del cliente unificante
411400000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
411500000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
411600000000     c     keyvss        SETLL     tivss02l
411700000000     c     keyvss        READE     tivss02l                               99
411800000000do  1c                   DOW       NOT *in99
411900000000if  2c                   IF        vssvat = *blanks                             *attivo
412000000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
412100000000     c                             datcor <= vssdsc
412200010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
412300010719     c* --> TD per vedere la firma DPD
412400010716     c                   RESET                   $AbTD                          *di lavoro
412500010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
412600010719     c     kssksu        SETLL     tivss02l
412700010719     c     kssksu        READE     tivss02l                               99
412800010719do  4c                   DOW       NOT *in99
412900010716if  5c                   IF        vssvat = *blanks                             *attivo
413000010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
413100010716     c                             datcor <= vssdsc
413200010719if  7c                   IF        vssisv = 'TD'
413300010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
413400010719e   7c                   ENDIF
413500010719e   6c                   ENDIF
413600010716e   5c                   ENDIF
413700010719     c     kssksu        READE     tivss02l                               99
413800010719e   4c                   ENDDO
413900010716     c*
414000000000     c                   LEAVE                                                  *uscita ciclo
414100000000e   3c                   ENDIF
414200000000e   2c                   ENDIF
414300000000     c     keyvss        READE     tivss02l                               99
414400010716e   1c                   ENDDO                                                  *fine cl.abil.TT
414500000000     c*
414600060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
414700060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
414800060621     c*
414900000000     c                   ENDSR
415000040330
415100040330     ***********************************************************************************************
415200040330     **?
415300040330     **?Reperimento ulteriori dati destinatario.
415400040330     **?
415500040330     ***********************************************************************************************
415600040330     C     ImpDSDest     BEGSR
415700040330
415800140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
415900140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
416000100121     C                             ar5Trd <> 'GEN'
416100140716     c                   Z-ADD     f_tasaas      kr5aas
416200140716     c                   Z-ADD     f_taslnp      kr5lnp
416300140716     c                   Z-ADD     f_tasnrs      kr5nrs
416400140716     c                   Z-ADD     f_tasnsp      kr5nsp
416500040330     c                   MOVEL     'GEN'         kr5trd
416600040531     c     keyar5        CHAIN     fiar531c
416700040330     C                   IF        %FOUND
416800040330     C                   EVAL      DAR5GEN = AR5Uni
416900100121     C                   ELSE
417000100121     C                   RESET                   DAR5GEN
417100040330     C                   ENDIF
417200100121     C                   ENDIF
417300100121     C
417400100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
417500100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
417600040330
417700040330     C                   ENDSR
417800040913
417900040913     ***********************************************************************************************
418000040913     **?
418100040913     **?Determino se addebitare l'immagine LDV.
418200040913     **?
418300040913     ***********************************************************************************************
418400040913     C     AddebitoLDV   BEGSR
418500040913
418600040913     C                   IF        LDVO74 = 'S'
418700040913
418800040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
418900040913
419000040913     C                   ENDIF
419100040913
419200040913     C                   ENDSR
419300051110
419400051110     ***********************************************************************************************
419500051110     **?
419600051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
419700051110     **?
419800051110     ***********************************************************************************************
419900051110     C     chkTivpi      BEGSR
420000051110
420100051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
420200051110     C                             esito10)
420300051110     C                   IF        esito10 > 0 AND NOT %ERROR
420400051110     C                   EVAL      gcfdio74 = 'N'
420500051110     C                   ENDIF
420600051110
420700051110     C                   ENDSR
420800051222
420900051222     ***********************************************************************************************
421000051222     **
421100051222     ** Linea arrivo Italia o estera.
421200051222     **
421300051222     ***********************************************************************************************
421400051222     C     setLnaItaEst  BEGSR
421500051222
421600051222     C                   RESET                   wrkLnaItaEst
421700051222     C                   EXSR      decOrg
421800051222     C                   EVAL      wrkLnaItaEst = wofl1
421900051222
422000051222     C                   ENDSR
422100060622
422200060622     ***********************************************************************************************
422300150615     ** Reperisco informazioni stato bolla
422400060622     ***********************************************************************************************
422500130312     C     srarb         BEGSR
422600130312     c                   clear                   wcev
422700140714     c                   clear                   wricons
422800130312     c                   clear                   worg
422900130312     c                   clear                   wdat
423000130312     c                   clear                   wora
423100150615     c                   clear                   ds2a
423200130312     c                   eval      dsevbspe = spe(1)
423300130312     c     karb          chain     fnarb01l
423400130312     c                   if        not %error and
423500130312     c                             %found(fnarb01l)
423600130422     c                   Z-ADD     arblna        worg
423700130312     c                   exsr      setLnaItaEst
423800130312     c* no per estero
423900130312     C                   IF        wrkLnaItaEst = ESTERO
424000130312     c                   leavesr
424100130312     c                   end
424200150617     c
424300130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
424400130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
424500130312     C                   SELECT
424600141120     c                   WHEN      arbdcm <> 0
424700130312     c                   MOVEL     '704'         wcev                           *causale evento
424800130313     c                   move      arbdcm        WDAT
424900130313     c                   move      arbhmc        WORA
425000150617     c
425100130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
425200150615     c                   WHEN      ARBFDC = ' ' and arbndc>0
425300130326     c* solo se distinta NON in test
425400130326     c                   clear                   ddstflr
425500130326     c                   z-add     arbifp        dstfgs
425600130326     c                   z-add     arbndc        dstnfv
425700130326     c     kdst          chain     fidst01l
425800130326     c                   if        not %error and
425900130326     c                             %found(fidst01l)
426000130326     c                   eval      ddstflr = dstflr
426100130326     c                   end
426200130327     c                   if        §DSTTSTPDA = 'C' or
426300130327     c                             §DSTTSTPDA = 'E'
426400130326     c                   leavesr
426500130326     c                   end
426600130326     c*
426700150617     c* Cerco PRIMO evento da PdA
426800130313     c                   move      arbndc        pctndc
426900131105     c                   exsr      cercaceP
427000150617     c*
427100150617     c* Cerco ULTIMO evento da PdA
427200131105     c                   move      'CET'         ttrd
427300130312     c     kcet          setgt     fipct02l
427400130312     c     kcet          readpe    fipct02l
427500130312     c                   if        not %error and
427600130312     c                             not %eof(fipct02l)
427700130312     c                   movel     pctdati       fipctcetds
427800140926      * firmatario se c'è riempie il campo
427900140926     c                   if        §pctnomfir <> *blank
428000140926     c                   eval      firmato74  = §pctnomfir
428100140926     c                   endif
428200130312     c* consegnata
428300130312     c                   if        §pctcmc = ' '
428400130312     c                   MOVEL     '704'         wcev                           *causale evento
428500130312     c                   else
428600140711     c                   move      'S'           wricons
428700130312     c* mancata consegna
428800130313     c                   movel     §PCTcmc       wcev
428900130312     c                   clear                   ds2a
429000130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
429100130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
429200130312     C                             :rpyEsito)
429300130312     c                   if        §2aflg = '§' and §2acpt = 'S'
429400130312     c                   move      'T  '         wcev
429500130312     c                   endif
429600130312     c                   endif
429700130312     c* ??? £6
429800130312     c                   if        §pctdata > 0
429900130313     c                   move      §PCTDATA      WDAT
430000130312     c                   movel     §PCTORA       WORA
430100130312     c                   endif
430200130312     c                   end
430300130312     c                   endSL
430400130312     c                   end
430500130312     c* ho trovato un evento da inserire  al 1° posto
430600130312     c                   if        wcev <> ' '
430700130312     c                   EXSR      deccevsta
430800130312     c                   IF        werr <> '1'                                  *no errore
430900130312     c                   EXSR      decorg
431000130312     c                   EXSR      edtdat
431100130312     c                   EXSR      edtora
431200130313     c* libero il 1° elemnto di schiera
431300150617     c                   eval      j = 49
431400150617     c                   for       j = 49 downto 1
431500130312     c                   if        cev(j) <> ' '
431600130312     c                   eval      cev(j+1) = cev(j)
431700130312     c                   eval      dev(j+1) = dev(j)
431800130312     c                   eval      hev(j+1) = hev(j)
431900130312     c                   eval      fle(j+1) = fle(j)
432000130312     c                   eval      eve(j+1) = eve(j)
432100130312     c                   end
432200130313     c                   endfor
432300130313     c                   eval      dev(1) = wdate
432400130313     c                   eval      hev(1) = worae
432500130312     c                   eval      cev(1)= wcev
432600130313     c                   eval      fle(1) = wodes
432700130313     c                   eval      eve(1) = wcdex
432800130313     c                   EVAL      cntEveO74 += 1
432900150310     c*
433000150421     c* memorizzo l'evento PDA se lasciato avviso o giacenza
433100150310     c                   if        §2aftc='A'
433200150310     c                   eval      pdaavv='S'
433300150310     c                   endif
433400150421     c                   if        §2aftc='G'
433500150421     c                   eval      pdagia='S'
433600150421     c                   endif
433700150310     c
433800130312     c                   end
433900130312     c                   end
434000130312     C                   ENDSR
434100140714     **_________________________________________________________
434200140714     c     cercafiglia   begsr
434300140714     **_________________________________________________________
434400140715     c                   clear                   keyForzata
434500140714      * salva chiave originale per eventi
434600140715     c                   eval      kasaasf = kasaas
434700140715     c                   eval      kaslnpf = kaslnp
434800140715     c                   eval      kasnrsf = kasnrs
434900140715     c                   eval      kasnspf = kasnsp
435000140714     c                   do        *hival
435100140715     c     keytasf       chain     fnlbl02l
435200140714     c                   if        %found(fnlbl02l)
435300140715     c                   eval      kasaasf = LBLaan
435400140715     c                   eval      kaslnpf = LBLlpn
435500140715     c                   eval      kasnrsf = LBLnrn
435600140715     c                   eval      kasnspf = LBLnsn
435700140714     c                   move      'X'           keyForzata
435800140714     c                   iter
435900140714     c                   else
436000140714     c                   leave
436100140714     c                   endif
436200140714     c                   enddo
436300140715     c     keytasf       setll     titas30c                               99    *no errore
436400140715     c     keytasf       READE     titas30c                               99    *no errore
436500141117      *se non trova titas con figlia forza di nuovo la mamma
436600140715if  3c                   IF        *in99                                        *non trovato
436700141117     c                   clear                   titasdssavf
436800140715x   3c                   ELSE                                                   *trovata
436900140715if  4c                   IF        *in01                                        *lettura titas000
437000140715     c                   EVAL      titasdssavf = titasds
437100140715e   4c                   ENDIF
437200140715if  4c                   IF        *in02                                        *lettura titas010
437300140715     c                   EVAL      titasdssavf = titasds
437400140715e   4c                   ENDIF
437500140715if  4c                   IF        *in03                                        *lettura titasp00
437600140715     c                   EVAL      titasdssavf = titasds
437700140715e   4c                   ENDIF
437800140715e   3c                   ENDIF
437900140714     C                   ENDSR
438000131105     **_________________________________________________________
438100131105     c     cercaCEP      begsr
438200131105     **_________________________________________________________
438300131105     c                   move      'CEP'         ttrd
438400131105     c     kcet          setgt     fipct02l
438500131105     c     kcet          readpe    fipct02l
438600131105     c                   if        not %error and
438700131105     c                             not %eof(fipct02l)
438800131105     c                   movel     pctdati       fipctcetds
438900131105     c* consegnata
439000131105     c                   if        §pctcmc = ' '
439100131105     c                   MOVEL     '704'         wcev                           *causale evento
439200131105     c                   else
439300131105     c* mancata consegna
439400131105     c                   movel     §PCTcmc       wcev
439500131105     c                   clear                   ds2a
439600131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
439700131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
439800131105     C                             :rpyEsito)
439900131105     c                   if        §2aflg = '§' and §2acpt = 'S'
440000131105     c                   move      'T  '         wcev
440100131105     c                   endif
440200131105     c                   endif
440300131105     c* ??? £6
440400131105     c                   if        §pctdata > 0
440500131105     c                   move      §PCTDATA      WDAT
440600131105     c                   movel     §PCTORA       WORA
440700131105     c                   endif
440800131105     c                   end
440900131105     c* ho trovato un evento da inserire  al 1° posto
441000131105     c                   if        wcev <> ' '
441100131105     c                   EXSR      deccevsta
441200131105     c                   IF        werr <> '1'                                  *no errore
441300131105     c                   EXSR      decorg
441400131105     c                   EXSR      edtdat
441500131105     c                   EXSR      edtora
441600131105     c* libero il 1° elemnto di schiera
441700150617     c                   eval      j = 49
441800150617     c                   for       j = 49 downto 1
441900131105     c                   if        cev(j) <> ' '
442000131105     c                   eval      cev(j+1) = cev(j)
442100131105     c                   eval      dev(j+1) = dev(j)
442200131105     c                   eval      hev(j+1) = hev(j)
442300131105     c                   eval      fle(j+1) = fle(j)
442400131105     c                   eval      eve(j+1) = eve(j)
442500131105     c                   end
442600131105     c                   endfor
442700131105     c                   eval      dev(1) = wdate
442800131105     c                   eval      hev(1) = worae
442900131105     c                   eval      cev(1)= wcev
443000131105     c                   eval      fle(1) = wodes
443100131105     c                   eval      eve(1) = wcdex
443200131105     c                   EVAL      cntEveO74 += 1
443300131105     c                   end
443400131105     c                   end
443500131105     c                   endsr
443600131105     ***********************************************************************************************
443700130312     **
443800130312     ** Reperisco tabella da TNTBE00F.
443900130312     **
444000130312     ***********************************************************************************************
444100130312     C     getTntbe00f   BEGSR
444200060622     C                   EVAL      t02Lin = langTntbe
444300060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
444400060622     C                   SELECT
444500060622     C                   WHEN      t02Cod = 'ICE'
444600060622     C                   EVAL      dice = t02Uni
444700060622     C                   WHEN      t02Cod = 'CCH'
444800060622     C                   EVAL      dcch = t02Uni
444900060622     C                   WHEN      t02Cod = 'NTW'
445000060622     C                   EVAL      dntw = t02Uni
445100100119     C                   WHEN      t02Cod = 'I1A'
445200100119     C                   EVAL      dI1a = t02Uni
445300150304     C                   WHEN      t02Cod = 'SPC'
445400150304     C                   EVAL      dSPC = t02Uni
445500060622     C                   ENDSL
445600060622     C                   ENDSR
445700060608
445800140529      /free
445900140529       //--------------------------------------------------------------
446000140529       //?Controllo formale dei dati passati.
446100140529       //--------------------------------------------------------------
446200140603       BEGSR  Kontrolla;
446300140529
446400140529       //?OpCode
446500140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
446600140529           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
446700140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
446800140529           exsr RoutEnd;
446900140529         ENDIF;
447000140603           IF  prmRqsDataSize > 0;
447100140603           ELSE;
447200140603             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
447300140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
447400140603             exsr RoutEnd;
447500140603           ENDIF;
447600140529
447700140529       //?Formato e size RQS
447800140603           IF  prmRqsFormato = formatoi74;
447900140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
448000140529           ELSE;
448100140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
448200140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
448300140529             exsr RoutEnd;
448400140529           ENDIF;
448500140529         //?Formato e size RPY
448600140603           IF  prmRpyFormato = formatoo74;
448700140529           ELSE;
448800140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
448900140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
449000140529             exsr RoutEnd;
449100140529           ENDIF;
449200140529           IF  prmRpyDataSize > 0;
449300140529           ELSE;
449400140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
449500140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
449600140529             exsr RoutEnd;
449700140529           ENDIF;
449800140616       //?Formato e size MSG
449900140617         if %parms > 9 ;
450000140617          IF  prmRpyFormMsg = formato;
450100140617          ELSE;
450200140616           prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
450300140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
450400140616           exsr RoutEnd;
450500140617          ENDIF;
450600140617          IF  prmRpyMsgSize > 0;
450700140617          ELSE;
450800140616           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
450900140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
451000140616           exsr RoutEnd;
451100140617          ENDIF;
451200140617         ENDIF;
451300140529
451400140529       ENDSR;
451500140529
451600140616       //--------------------------------------------------------------
451700140617       //?Routine per schierare i messaggi nella ds tis778dsm
451800140616       //--------------------------------------------------------------
451900140616       BEGSR  Messaggi;
452000140616
452100140616       //?Se ho già caricato 99 messaggi esco
452200140616         IF  nrmsg = 99;
452300140616           exsr RoutEnd;
452400140616           clear nrmsg;
452500140616         ENDIF;
452600140616
452700140616         nrmsg += 1;
452800140616         skIdMsg(nrmsg) = wIdMsg;
452900140616         skIdCampo(nrmsg) = wIdCampo;
453000140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
453100140616         IF  wParm <> *blanks;
453200140616           skTextMsg (nrmsg) =
453300140616           rtvMsgLang(wIdMsg:langi74:wParm);
453400140616         ELSE;
453500140616           skTextMsg (nrmsg)=
453600140616           rtvMsgLang(wIdMsg:langi74);
453700140616         ENDIF;
453800140616
453900140616       ENDSR;
454000140616
454100140529       //--------------------------------------------------------------
454200140529       BEGSR  RoutEnd;
454300140530
454400140530
454500140530        if  esito <> '0' ;
454600140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
454700140530             else;
454800140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
454900140530        endif ;
455000140529
455100140530       //?ritorno
455200140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
455300140617         if %parms > 9 ;
455400140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
455500140617         endif;
455600140529         return;
455700140529
455800140529       ENDSR;
455900140529      /end-free
456000000000     c*--------------------------------------------------------------------------------------------*
456100000000     c* operazioni iniziali
456200000000     c*--------------------------------------------------------------------------------------------*
456300000000     c     *inzsr        BEGSR
456400000000     c*
456500000000     c* ricevimento parametri
456600140603     c*m   *ENTRY        PLIST
456700140529     c*m                 PARM                    esito
456800140529     c*m                 PARM                    tis174dsi
456900140529     c*m                 PARM                    tis174dso
457000000000     c*
457100000000     c* chiavi di lettura
457200000000     c     keytas        KLIST                                                  titas30c
457300000000     c                   KFLD                    kasaas                         -anno spedizione
457400000000     c                   KFLD                    kaslnp                         -linea partenza
457500000000     c                   KFLD                    kasnrs                         -serie
457600000000     c                   KFLD                    kasnsp                         -spedizione
457700140715     c     keytasf       KLIST                                                  titas30c
457800140715     c                   KFLD                    kasaasf                        -anno spedizione
457900140715     c                   KFLD                    kaslnpf                        -linea partenza
458000140715     c                   KFLD                    kasnrsf                        -serie
458100140715     c                   KFLD                    kasnspf                        -spedizione
458200130312     c     karb          KLIST                                                  titas30c
458300140716     c                   KFLD                    f_tasaas                         -anno spedizione
458400140716     c                   KFLD                    f_taslnp                         -linea partenza
458500140716     c                   KFLD                    f_tasnrs                         -serie
458600140716     c                   KFLD                    f_tasnsp                         -spedizione
458700130326     C     Kdst          KLIST
458800130326     C                   KFLD                    dstnpg
458900130326     C                   KFLD                    dstnfv
459000130326     C                   KFLD                    dstfgs
459100130326     c                   z-add     4             dstnpg
459200130326     C     Kcet          KLIST
459300130326     C                   KFLD                    arbLNA
459400130326     C                   KFLD                    pctNDC
459500130326     C                   KFLD                    arbpdc
459600131105     C                   KFLD                    TTRD              3
459700130312     C                   KFLD                    arbAAS
459800130312     C                   KFLD                    arbLNP
459900130312     C                   KFLD                    arbNRS
460000130312     C                   KFLD                    arbNSP
460100150403     C     KARP          KLIST
460200150403     C                   KFLD                    arbAAS
460300150403     C                   KFLD                    arbLNP
460400150403     C                   KFLD                    arbNRS
460500150403     C                   KFLD                    arbNSP
460600000000     c     keytaa        KLIST                                                  titaa30c
460700000000     c                   KFLD                    kaaaas                         -anno spedizione
460800000000     c                   KFLD                    kaalnp                         -linea partenza
460900000000     c                   KFLD                    kaanrs                         -serie
461000000000     c                   KFLD                    kaansp                         -spedizione
461100000000     c                   KFLD                    kaatrc                         -tipo anagrafica
461200000000     c     keyta4        KLIST                                                  tita430c
461300000000     c                   KFLD                    ka4aas                         -anno spedizione
461400000000     c                   KFLD                    ka4lnp                         -linea partenza
461500000000     c                   KFLD                    ka4nrs                         -serie
461600000000     c                   KFLD                    ka4nsp                         -spedizione
461700000000     c                   KFLD                    ka4trc                         -tipo riferimento
461800010925     c     ke2ta4        KLIST                                                  tita430c
461900010925     c                   KFLD                    ka4aas                         -anno spedizione
462000010925     c                   KFLD                    ka4lnp                         -linea partenza
462100010925     c                   KFLD                    ka4nrs                         -serie
462200010925     c                   KFLD                    ka4nsp                         -spedizione
462300000000     c     keyevb        KLIST                                                  fnevb01l
462400000000     c                   KFLD                    kvbaas                         -anno spedizione
462500000000     c                   KFLD                    kvblnp                         -linea partenza
462600000000     c                   KFLD                    kvbnrs                         -serie
462700000000     c                   KFLD                    kvbnsp                         -spedizione
462800010717     c     ke2evb        KLIST                                                  fnevb01l
462900010717     c                   KFLD                    kvbaas                         -anno spedizione
463000010717     c                   KFLD                    kvblnp                         -linea partenza
463100010717     c                   KFLD                    kvbnrs                         -serie
463200010717     c                   KFLD                    kvbnsp                         -spedizione
463300010717     c                   KFLD                    kvbdev                         -data evento
463400010717     c                   KFLD                    kvbhev                         -ora  evento
463500000000     c     keylbl        KLIST                                                  fnlbl01l
463600000000     c                   KFLD                    kblaan                         -anno seguente
463700000000     c                   KFLD                    kbllpn                         -linea seguente
463800000000     c                   KFLD                    kblnrn                         -serie seguente
463900000000     c                   KFLD                    kblnsn                         -spedizione seguente
464000000000     c     ke2lbl        KLIST                                                  fnlbl02l
464100000000     c                   KFLD                    kblaap                         -anno precedente
464200000000     c                   KFLD                    kbllpp                         -linea precedente
464300000000     c                   KFLD                    kblnrp                         -serie precedente
464400000000     c                   KFLD                    kblnsp                         -spediz.precedente
464500000000     c     keytbe        KLIST                                                  *tntbe01l
464600000000     c                   KFLD                    kbecod                          -tabella
464700000000     c                   KFLD                    kbeke1                          -chiave uno
464800000000     c                   KFLD                    kbeke2                          -chiave due
464900000000     c                   KFLD                    kbelin                          -lingua
465000000000     c                   KFLD                    kbesif                          -s.informativo
465100000000     c     keyvss        KLIST                                                  *tivss02l
465200000000     c                   KFLD                    kssksu                          -cl.unificante
465300000000     c                   KFLD                    kssisv                          -tipo servizio
465400010717     c     keyvtd        KLIST                                                  *tivtd01l
465500010717     c                   KFLD                    ktdksu                          -cl.unificante
465600000000     c     keycsb        KLIST                                                  *tncsb03l
465700000000     c                   KFLD                    ksbaas                         -anno spedizione
465800000000     c                   KFLD                    ksblnp                         -linea partenza
465900000000     c                   KFLD                    ksbnrs                         -serie
466000000000     c                   KFLD                    ksbnsp                         -spedizione
466100040906     c     keydct        KLIST
466200040906     c                   KFLD                    DCTAAC
466300040906     c                   KFLD                    DCTFIL
466400040906     c                   KFLD                    DCTNCA
466500000000     c     keygcp        KLIST                                                  *figcp01l
466600000000     c                   KFLD                    kcpaas                         -anno spedizione
466700000000     c                   KFLD                    kcplnp                         -linea partenza
466800000000     c                   KFLD                    kcpnrs                         -serie
466900000000     c                   KFLD                    kcpnsp                         -spedizione
467000000000     c                   KFLD                    kcpfrg                         -prg aperture
467100050404     c     K04GCP51      KLIST
467200050404     c                   KFLD                    kcpaas                         -anno spedizione
467300050404     c                   KFLD                    kcplnp                         -linea partenza
467400050404     c                   KFLD                    kcpnrs                         -serie
467500050404     c                   KFLD                    kcpnsp                         -spedizione
467600000000     c     keygnp        KLIST                                                  *fignp01l
467700000000     c                   KFLD                    knpagc                         -anno apertura
467800000000     c                   KFLD                    knpfgc                         -filiale apertura
467900000000     c                   KFLD                    knpngc                         -numero giacenza
468000000000     c                   KFLD                    knpfrg                         -prg apertura
468100040531     c     keyar5        KLIST                                                  *fiar531c
468200030206     c                   KFLD                    kr5aas                         -anno spedizione
468300030206     c                   KFLD                    kr5lnp                         -linea partenza
468400030206     c                   KFLD                    kr5nrs                         -serie
468500030206     c                   KFLD                    kr5nsp                         -spedizione
468600030206     c                   KFLD                    kr5trd                         -tipo record
468700040907
468800000000     c* apre i files
468900000000     c                   OPEN      titas30c
469000000000     c                   OPEN      titaa30c
469100000000     c                   OPEN      tita430c
469200000000     c                   OPEN      fnevb01l
469300000000     c                   OPEN      tivss02l
469400010716     c                   OPEN      tivtd01l
469500000000     c                   OPEN      azorg01l
469600000000     c                   OPEN      fnlbl01l
469700000000     c                   OPEN      fnlbl02l
469800050309     c                   OPEN      tigcp51l
469900050309     c                   OPEN      tignp01l
470000000000     c                   OPEN      tncsb03l
470100040906     c                   OPEN      fndct01l
470200040531     c                   OPEN      fiar531c
470300130312     c                   OPEN      fnarb01l
470400150403     c                   OPEN      fiarp01l
470500140606     c                   OPEN      fiar501l
470600140606     c                   OPEN      fiar401l
470700130326     c                   OPEN      fidst01l
470800130312     c                   OPEN      fipct02l
470900060621     C                   CALLP     openTabel00f()
471000000000     c*
471100000000     c* caricamento tabelle occorrenti
471200000000     c                   EXSR      cartab
471300060621     C                   CALLP     inzLingue()
471400000000     c*
471500000000     c                   ENDSR
471600070109
471700070109     ***********************************************************************************************
471800070109     **
471900070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
472000070109     **
472100070109     ***********************************************************************************************
472200070109     C     eventoAnnulla BEGSR
472300070109
472400070109     C                   IF        §iceEvAn <> *BLANK
472500070109     C                   FOR       j = 1 TO %ELEM(smCev)
472600070109     C                   IF        §iceEvAn = smCev(j) AND
472700070109     C                             evbDevHev = smDevHev(j)
472800070109     C                   CLEAR                   smCev(j)
472900130312     C                   CLEAR                   smspe(j)
473000070109     C                   CLEAR                   smDevHev(j)
473100070109     C                   CLEAR                   smi(j)
473200070109     C                   CLEAR                   smSeql(j)
473300070109     C                   CLEAR                   smSeqe(j)
473400070109     C                   CLEAR                   smDeve(j)
473500070109     C                   CLEAR                   smHeve(j)
473600070109     C                   CLEAR                   smFle(j)
473700070109     C                   CLEAR                   smDev(j)
473800070109     C                   LEAVE
473900070109     C                   ENDIF
474000070109     C                   ENDFOR
474100070109     C                   ENDIF
474200070109
474300070109     C                   ENDSR
474400080131
474500100119     P*--------------------------------------------------
474600100119     P* Procedure name: GetDescrizioneIncassoContrassegno
474700100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
474800100119     P*                          o del contrassegno.
474900100121     P* Returns:        Descrizione modalità incasso contrassegno
475000100119     P*--------------------------------------------------
475100100119     P GetDescrizioneIncassoContrassegno...
475200100119     P                 B
475300100119     D GetDescrizioneIncassoContrassegno...
475400100119     D                 PI            35A
475500100119
475600100121     C                   RESET                   tibs02ds
475700100121     C                   EVAL      t02Cod = 'I1A'
475800100121     C                   EVAL      t02Lin = langTntbe
475900100119     ** Il contrassegno è stato incassato
476000100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
476100100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
476200100121     C                   ELSE
476300100119     ** Il contrassegno non è stato incassato.
476400100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
476500100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
476600100119     C                             ar5Trd <> 'GEN'
476700100121     C                   EVAL      kr5Aas = csbAas
476800100121     C                   EVAL      kr5Lnp = csbLnp
476900100121     C                   EVAL      kr5Nrs = csbNrs
477000100121     C                   EVAL      kr5Nsp = csbNsp
477100100119     C                   EVAL      kr5Trd = 'GEN'
477200100119     C     keyAr5        CHAIN     fiar531c
477300100119     C                   IF        %FOUND()
477400100119     C                   EVAL      dAr5Gen = ar5Uni
477500100119     C                   ELSE
477600100119     C                   RESET                   dAr5Gen
477700100119     C                   RETURN                  *BLANK
477800100119     C                   ENDIF
477900100119     C                   ENDIF
478000100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
478100100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
478200100119     C                   ELSE
478300100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
478400100119     C                   ENDIF
478500100121     C                   ENDIF
478600100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
478700100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
478800100119     C                   IF        t02Err = 'E'
478900100119     C                   RETURN                  *BLANK
479000100119     C                   ENDIF
479100100120     C                   EVAL      dI1a = t02Uni
479200140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
479300140417     C                   RETURN                  dI1a.descrizi2
479400140417     C                   ENDIF
479500100119     C                   RETURN                  dI1a.descrizion
479600100119
479700100119     P GetDescrizioneIncassoContrassegno...
479800100119     P                 E
479900100119
480000110304
480100110304     P*--------------------------------------------------
480200110304     P* Procedure name: IsContrassegnoCompensato
480300110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
480400110304     P*                          to con una fattura.
480500110304     P* Returns:        *ON = Compensato
480600110304     P*--------------------------------------------------
480700110304     P IsContrassegnoCompensato...
480800110304     P                 B
480900110304     D IsContrassegnoCompensato...
481000110304     D                 PI              N
481100110304
481200110304     D retField        S               N   INZ(*OFF)
481300110304
481400110307     ** Il contrassegno non è stato rimborsato.
481500110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
481600110307     C                   RETURN    retField
481700110307     C                   ENDIF
481800110307
481900110307     C/EXEC SQL
482000110307     C+ SELECT '1'
482100110307     C+ INTO :retField
482200110307     C+ FROM tncsv00f
482300110307     C+ WHERE csvAas = :csbAas
482400110307     C+   AND csvLnp = :csbLnp
482500110307     C+   AND csvNrs = :csbNrs
482600110307     C+   AND csvNsp = :csbNsp
482700110307     C+   AND csvCav = 'COMP'
482800110307     C+ FETCH FIRST ROW ONLY
482900110307     C/END-EXEC
483000110304
483100110307     C                   SELECT
483200110307     C                   WHEN      sqlCode < 0
483300110307     C                   DUMP(A)
483400110307     C                   ENDSL
483500110304
483600110307     C                   RETURN    retField
483700110304
483800110304     P IsContrassegnoCompensato...
483900110304     P                 E
484000110304
484100120321
484200120321     P*--------------------------------------------------
484300120321     P* Procedure name: SetIdAssegnoMittente
484400120321     P* Purpose:        Imposta l'ID assegno mittente.
484500120321     P* Returns:        Esito.
484600120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
484700120321     P*                          segni mittente.
484800120321     P* Parameter:      priAbi => ABI assegno.
484900120321     P* Parameter:      priCab => CAB assegno.
485000120321     P*--------------------------------------------------
485100120321     P SetIdAssegnoMittente...
485200120321     P                 B
485300120321     D SetIdAssegnoMittente...
485400120321     D                 PI            10I 0
485500120321     D  priNumeroAssegno...
485600120321     D                                     LIKE(caNumO74)
485700120321     D  priAbi                             LIKE(caAbiO74)
485800120321     D  priCab                             LIKE(caCabO74)
485900120321
486000120321     D retField        S             10I 0 STATIC
486100120321
486200120321      /FREE
486300120321
486400120321       CLEAR retField;
486500120321
486600120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
486700120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
486800120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
486900120321       // In questo caso restituisco l'ID del primo assegno.
487000120321
487100120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
487200120321         RETURN retField;
487300120321       ENDIF;
487400120321
487500120321       EXEC SQL
487600120321         SELECT csmNra, csmAbi, csmCab
487700120321           INTO :priNumeroAssegno, :priAbi, :priCab
487800120321           FROM tncsm00f
487900120321           WHERE csmKey = :priNumeroAssegno
488000120321           FETCH FIRST 1 ROW ONLY
488100120321       ;
488200120321
488300120321       SELECT;
488400120321         WHEN sqlCode = 100;
488500120321           retField = sqlCode;
488600120321         WHEN sqlCode < *ZERO;
488700120321           DUMP(A);
488800120321           retField = sqlCode;
488900120321       ENDSL;
489000120321
489100120321       RETURN retField;
489200120321
489300120321      /END-FREE
489400120321     P SetIdAssegnoMittente...
489500120321     P                 E
489600120321
489700130513
489800130513     P*--------------------------------------------------
489900130513     P* Procedure name: GetFilUrl
490000130513     P* Purpose:        Restituisce l'URL della filiale.
490100130513     P* Returns:        URL filiale.
490200130513     P* Parameter:      piFiliale => ID filiale
490300130513     P*--------------------------------------------------
490400130513     P GetFilUrl       B
490500130513     D GetFilUrl       PI           256A
490600130513     D  piFiliale                     3P 0 CONST
490700130513
490800130513     C                   IF        piFiliale = *ZERO
490900130513     C                   RETURN    *BLANK
491000130513     C                   ENDIF
491100130513     C                   RESET                   tibs02ds
491200130513     C                   EVAL      t02Cod = 'UFI'
491300130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
491400130513     C                   EVAL      t02Lin = langTntbe
491500130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
491600130513     C                   IF        t02Err = 'E'
491700130513     C                   RETURN    *BLANK
491800130513     C                   ENDIF
491900130513     C                   RETURN    t02Uni
492000130513
492100130513     P GetFilUrl       E
492200130513
