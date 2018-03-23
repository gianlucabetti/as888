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
003600140226     ffipct02l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIPCT02L')
003700140226     ffidst01l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIDST01L')
003800140605     ffiar501l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIAR501L')
003900140606     f                                     RENAME(fiar5000:fiar5f)
004000140605     ffiar401l  IF   E           K DISK    USROPN EXTDESC('FILTRAPRD/FIAR401L')
004100000000     f*---
004200000000     f* Giacenze
004300000000     f*---
004400050309     ftigcp51l  IF   E           K DISK    USROPN
004500050309     ftignp01l  IF   E           K DISK    USROPN
004600000000     f*---
004700000000     f* Contrassegni
004800000000     f*---
004900000000     ftncsb03l  IF   E           K DISK    USROPN
005000000000     f*---
005100000000     f* Danni testata CA
005200000000     f*---
005300040906     ffndct01l  IF   E           K DISK    USROPN
005400030206     f*---
005500030206     f* Estensione testata bolle
005600030206     f*---
005700040531     ffiar531c  IF   E           K DISK    USROPN
005800000000     f*---
005900000000     f* Clienti abilitati
006000000000     f*---
006100000000     ftivss02l  IF   E           K DISK    USROPN
006200010716     f*---
006300010716     f* Estensione DPD
006400010716     f*---
006500010716     ftivtd01L  if   e           k disk    USROPN
006600000000     f*---
006700000000     f* Tabelle -NEW-
006800000000     f*---
006900000000     ftntbe01l  IF   E           K DISK    USROPN
007000000000     f*---
007100000000     f* Organigramma
007200000000     f*---
007300000000     fazorg01l  IF   E           K DISK    USROPN                               *organigramma
007400051110
007500051110     ***********************************************************************************************
007600051110     **?
007700051110     **?Definizione procedure.
007800051110     **?
007900051110     ***********************************************************************************************
008000051110     D GetSpeChkCde    PR            10U 0
008100051110     D  Anno                          4
008200051110     D                                     VALUE
008300051110     D  IdSpedizione                 12
008400051110     D                                     VALUE
008500051110     D  ChkCode                      10U 0
008600051110     D  nEsito                        5I 0
008700051110     D tis7653r        PR
008800051110     D                                     EXTPGM('TIS7653R')
008900051110     D  ksu
009000051110     D                                     LIKE(kscI74)
009100051110     D  agc
009200051110     D                                     LIKE(gcpAgc)
009300051110     D  fgc
009400051110     D                                     LIKE(gcpFgc)
009500051110     D  ngc
009600051110     D                                     LIKE(gcpNgc)
009700051110     D  esito10                      10I 0
009800060612     D rtvMsgLang      PR          3512A                                        Messaggio in lingua
009900060612     D  msgId                         7A   CONST
010000060612     D  piLinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
010100060613     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
010200060613     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
010300060612     D                                     VARYING
010400060612     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
010500060612     D                                     VARYING
010600060612     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
010700060612     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
010800060621     D inzLingue       PR            10I 0
010900060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
011000060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
011100060621     D  rpyElementi                  10I 0 OPTIONS(*NOPASS:*OMIT)
011200060621     D cvtLinguaISO2ToTabel...
011300060621     D                 PR                  LIKE(linTabel)
011400060621     D  rqsISO2                            LIKE(linISO2)
011500060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
011600060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
011700060621     D  rpyTabel                           LIKE(linTabel)
011800060621     D                                     OPTIONS(*NOPASS:*OMIT)
011900060621     D cvtLinguaISO2ToTntbe...
012000060621     D                 PR                  LIKE(linTntbe)
012100060621     D  rqsISO2                            LIKE(linISO2)
012200060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
012300060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012400060621     D  rpyTntbe                           LIKE(linTntbe)
012500060621     D                                     OPTIONS(*NOPASS:*OMIT)
012600060621     D openTabel00f    PR
012700060621     D  rpyOpCode                    10A   OPTIONS(*NOPASS:*OMIT)
012800060621     D  rpyEsito                     10I 0 OPTIONS(*NOPASS:*OMIT)
012900060626     D chainTabel00f   PR           109A
013000060626     D  rqsOpCode                    10A   CONST
013100060626     D  rqsKut                        1P 0
013200060626     D  rqsCod                        2A   CONST
013300060626     D  rqsKey                        8A   OPTIONS(*VARSIZE) CONST
013400060626     D  rqsLengthOfKey...
013500060626     D                               10I 0 CONST
013600060626     D  rqsFormat                    10A   CONST
013700060626     D  rpyOpCode                    10A
013800060626     D  rpyEsito                     10I 0
013900060626     D  rpyData                     109A   OPTIONS(*NOPASS:*OMIT)
014000060626     D tibs02r         PR                  EXTPGM('TIBS02R')                    Lettura TNTBE00F
014100060622     D  kpjba                              LIKEDS(kpjba)
014200060622     D  tibs02ds                           LIKEDS(tibs02ds)
014300090828     D/COPY GAITRASRC/SRCPROTOPR,TIS7700R
014400090914     D/COPY GAITRASRC/SRCPROTOPR,TIS7702R
014500090828
014600051222     ***********************************************************************************************
014700051222     **?
014800051222     **?Definizione costanti.
014900051222     **?
015000051222     ***********************************************************************************************
015100101104     D ITALIA          C                   ' '
015200101104     D ESTERO          C                   'E'
015300101104     D EVENTO_ARRIVATA_IN_FILIALE...
015400101104     D                 C                   '703'
015500101104     D EVENTO_MESSA_IN_CONSEGNA...
015600101104     D                 C                   'MIC'
015700051110
015800000000     d*--------------------------------------------------------------------------------------------*
015900000000     d* Data structure
016000000000     d*--------------------------------------------------------------------------------------------*
016100130312     d fipctcetds    e ds
016200140605     d fipctcords    e ds
016300090914     D cndizion      E DS                  QUALIFIED BASED(nullPtr)
016400090914     D azlin00f      E DS                  BASED(nullPtr)
016500060622     D tabel00f      E DS                  INZ
016600060622     D kpjba         E DS                  INZ
016700140605     D trulorsds     E DS                  INZ
016800140605     D trulor2ds     E DS                  INZ
016900140605     D trulor3ds     E DS                  INZ
017000140606     d dar5emd       e ds                  inz prefix(§)
017100140605     D tnsd99ds      E DS                  INZ
017200140606     D tibs02ds      E DS                  INZ
017300060622     D  t02Mod       E                     INZ('C')                             Controllo
017400090828     D tis7700i0     E DS                  QUALIFIED
017500090828     D                                     INZ(*EXTDFT)
017600090828     D tis7700o0     E DS                  QUALIFIED
017700090828     d*---
017800000000     d* Ridenominazione formato record -TITAS-
017900000000     d*---
018000000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
018100000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
018200000000     d                                     PREFIX(s_)
018300140715     d titasdssavF   E DS                  EXTNAME(titas00f)                    *depisito titas
018400140715     d                                     PREFIX(f_)
018500080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
018600080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
018700080131     D                                     BASED(titas000FigliaPtr)
018800080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
018900080131     D                                     BASED(titas000FigliaPtr)
019000080131     d*---
019100000000     d* Variabili riferite al data base
019200000000     d*---
019300141126     d kasaasm         S                   LIKE(tasaas)                         *lettura titas30c
019400141126     d kaslnpm         S                   LIKE(taslnp)
019500141126     d kasnrsm         S                   LIKE(tasnrs)
019600141126     d kasnspm         S                   LIKE(tasnsp)
019700140715     d kasaasf         S                   LIKE(tasaas)                         *lettura titas30c
019800140715     d kaslnpf         S                   LIKE(taslnp)
019900140715     d kasnrsf         S                   LIKE(tasnrs)
020000140715     d kasnspf         S                   LIKE(tasnsp)
020100000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
020200000000     d kaslnp          S                   LIKE(taslnp)
020300000000     d kasnrs          S                   LIKE(tasnrs)
020400000000     d kasnsp          S                   LIKE(tasnsp)
020500000000     d kastbl          S                   LIKE(tastbl)
020600000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
020700000000     d kaalnp          S                   LIKE(taalnp)
020800000000     d kaanrs          S                   LIKE(taanrs)
020900000000     d kaansp          S                   LIKE(taansp)
021000000000     d kaatrc          S                   LIKE(taatrc)
021100000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
021200000000     d ka4lnp          S                   LIKE(ta4lnp)
021300000000     d ka4nrs          S                   LIKE(ta4nrs)
021400000000     d ka4nsp          S                   LIKE(ta4nsp)
021500000000     d ka4trc          S                   LIKE(ta4trc)
021600000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
021700000000     d kvblnp          S                   LIKE(evblnp)
021800000000     d kvbnrs          S                   LIKE(evbnrs)
021900000000     d kvbnsp          S                   LIKE(evbnsp)
022000000000     d kvbdev          S                   LIKE(evbdev)
022100000000     d kvbhev          S                   LIKE(evbhev)
022200000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
022300000000     d kcplnp          S                   LIKE(gcplnp)
022400000000     d kcpnrs          S                   LIKE(gcpnrs)
022500000000     d kcpnsp          S                   LIKE(gcpnsp)
022600000000     d kcpfrg          S                   LIKE(gcpfrg)
022700000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
022800000000     d knpfgc          S                   LIKE(gnpfgc)
022900000000     d knpngc          S                   LIKE(gnpngc)
023000000000     d knpfrg          S                   LIKE(gnpfrg)
023100000000     d knpfas          S                   LIKE(gnpfas)
023200000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
023300000000     d ksblnp          S                   LIKE(csblnp)
023400000000     d ksbnrs          S                   LIKE(csbnrs)
023500000000     d ksbnsp          S                   LIKE(csbnsp)
023600000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
023700000000     d kbllpn          S                   LIKE(lbllpn)
023800000000     d kblnrn          S                   LIKE(lblnrn)
023900000000     d kblnsn          S                   LIKE(lblnsn)
024000000000     d kblaap          S                   LIKE(lblaap)
024100000000     d kbllpp          S                   LIKE(lbllpp)
024200000000     d kblnrp          S                   LIKE(lblnrp)
024300000000     d kblnsp          S                   LIKE(lblnsp)
024400000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
024500000000     d kctlnp          S                   LIKE(dctlnp)
024600000000     d kctnrs          S                   LIKE(dctnrs)
024700000000     d kctnsp          S                   LIKE(dctnsp)
024800040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
024900030206     d kr5lnp          S                   LIKE(ar5lnp)
025000030206     d kr5nrs          S                   LIKE(ar5nrs)
025100030206     d kr5nsp          S                   LIKE(ar5nsp)
025200030206     d kr5trd          S                   LIKE(ar5trd)
025300000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
025400000000     d kssisv          S                   LIKE(vssisv)
025500010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
025600000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
025700000000     d kbeke1          S                   LIKE(tbeke1)
025800000000     d kbeke2          S                   LIKE(tbeke2)
025900000000     d kbelin          S                   LIKE(tbelin)
026000000000     d kbesif          S                   LIKE(tbesif)
026100000000     d waas            S                   LIKE(tasaas)                         *di lavoro
026200000000     d wlnp            S                   LIKE(taslnp)
026300000000     d wnrs            S                   LIKE(tasnrs)
026400000000     d wnsp            S                   LIKE(tasnsp)
026500000000     d wfca            S                   LIKE(tasfca)
026600000000     d wfgc            S                   LIKE(tasfgc)
026700000000     d wfpr            S                   LIKE(dctfpr)
026800000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
026900000000     d deplpn          S                   LIKE(lbllpn)
027000000000     d depnrn          S                   LIKE(lblnrn)
027100000000     d depnsn          S                   LIKE(lblnsn)
027200010716     d depute          S                   LIKE(vtdute)
027300010716     d deppwd          S                   LIKE(vtdpwd)
027400030130     d savtsp          S                   LIKE(tastsp)                         *di salvataggio
027500000000     d lcca            S                   LIKE(tascca)                         *di lavoro
027600000000     d lduc            S                   LIKE(tasduc)
027700000000     d llnp            S                   LIKE(taslnp)
027800000000     d ldti            S                   LIKE(tasdti)
027900000000     d lhti            S                   LIKE(tashti)
028000000000     d ldrt            S                   LIKE(tasdrt)
028100000000     d llna            S                   LIKE(taslna)
028200000000     d ldcm            S                   LIKE(tasdcm)
028300000000     d lhmc            S                   LIKE(tashmc)
028400060621     d langTabel       S                   LIKE(tblkut)
028500060621     d langTntbe       S                   LIKE(tbelin)
028600140108     d wDev            S                   LIKE(evbdev)
028700140108     d wHev            S                   LIKE(evbhev)
028800140616     d wParm           s            512a   inz
028900000000     d*---
029000000000     d* Ds
029100000000     d*---
029200000000      * controllo data
029300000000     d wlbda8          DS                  INZ                                  *controlla data
029400000000     d  g08dat                        8  0                                       -data dritta
029500000000     d  g08inv                        8  0                                       -data invertita
029600000000     d  g08err                        1                                          -errore
029700000000     d  g08tgi                        5  0                                       -giorni fra date
029800000000      * scomposizione data generica
029900000000     d dsdat           DS                  INZ                                  *data
030000000000     d  dsann                         4  0                                       -anno
030100000000     d  dsmes                         2  0                                       -mese
030200000000     d  dsgio                         2  0                                       -giorno
030300000000      * scomposizione data generica (anno + mese/giorno)
030400000000     d                 DS                  INZ                                  *data aa + mm/gg
030500000000     d  dsannB                 1      4  0                                       -anno
030600000000     d  dsmgsB                 5      8  0                                       -mese/giorno
030700000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
030800000000      * composizione data generica editata
030900000000     d dsdate          DS                  INZ                                  *data
031000000000     d  dsgioe                        2  0                                       -giorno
031100000000     d  dsvd1                         1                                          -'/'
031200000000     d  dsmese                        2  0                                       -mese
031300000000     d  dsvd2                         1                                          -'/'
031400000000     d  dsanne                        4  0                                       -anno
031500000000      * scomposizione ora generica
031600000000     d dsora           DS                  INZ                                  *ora
031700000000     d  dshh                          2  0                                       -ore
031800000000     d  dsmm                          2  0                                       -minuti
031900000000      * composizione ora generica editata
032000000000     d dsorae          DS                  INZ                                  *ora
032100000000     d  dshhe                         2  0                                       -ore
032200000000     d  dsvo1                         1                                          -':'
032300000000     d  dsmme                         2  0                                       -minuti
032400030206      * lettura FIAR5 - record bancali
032500030206     d dar5ban       E DS
032600030206      * lettura FIAR5 - record supermercati
032700030206     d dar5scr       E DS
032800040330     d DAR5GEN       E DS
032900040330     D                                     INZ
033000040330     D                                     PREFIX(GEN)
033100000000      * tipi servizio
033200000000     d ds5e          E DS
033300000000      * disposizioni giacenza
033400000000     d ds2d          E DS
033500000000      * causale eventi
033600000000     d dice          E DS
033700000000      * causali chiusura CA
033800000000     d dcch          E DS
033900100119     d dI1a          E DS                  QUALIFIED
034000000000      * consegne anomale
034100000000     d ds7o          E DS
034200130312      * eventi
034300130312     d ds2a          E DS
034400000000      * riferimenti
034500000000     d dta4a         E DS
034600070927     d dsbl4e        E DS
034700060627      * Estensione blp/arb tipo record "I"
034800060627     d dsbl4i        E DS
034900040913      * Addebito immagine POD
035000040913     d DLVA1         E DS
035100040913     D                                     INZ
035200000000      * lettura campo orgde3 di AZORG00F
035300000000     d og143         E DS
035400000000      * stati contrassegno
035500000000     d ds4s          E DS
035600141014      * particolarità consegna
035700141014     d ds7r          E DS
035800031217      * DS lettura campo TASFLO di TITAS
035900031217     d dtasflo       E DS
036000000000      * Ds ricerca cliente P.d.C.
036100000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
036200000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
036300000000     d indds         E DS                  EXTNAME(cnind00f) INZ
036400000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
036500000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
036600000000      * reperimento clienti da cliente unificante
036700090903     d*tibs10ds      E DS
036800090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
036900000000      * scomposizione codice spedizione -DS Input-
037000000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
037100000000     d  dslnpi74                      3                                          -linea partenza
037200000000     d  dsnrsi74                      2                                          -serie
037300000000     d  dsnspi74                      7                                          -spedizione
037400000000      * composizione codice spedizione -bolle-
037500000000     d                 DS
037600000000     d  dstaslnp               1      3  0                                       -linea partenza
037700000000     d  dstasnrs               4      5  0                                       -serie
037800000000     d  dstasnsp               6     12  0                                       -spedizione
037900000000     d dstasspe                1     12                                         *ds cod spedizione
038000000000      * composizione data + ora evento
038100080131     d                 DS                  INZ
038200080131     d  evbdev                 1      8  0                                       -data evento
038300080131     d  evbhev                 9     12  0                                       -ora  evento
038400080131     d evbDevHev               1     12  0                                      *ds data+ora evento
038500130312      * composizione spedizione evento
038600130312     d                 DS                  INZ
038700130312     d  evbaas                 1      4  0                                       -data evento
038800130312     d  evblnp                 5      7  0                                       -ora  evento
038900130312     d  evbnrs                 8      9  0                                       -ora  evento
039000130312     d  evbnsp                10     16  0                                       -ora  evento
039100130312     d dsevbspe                1     16  0                                      *ds data+ora evento
039200060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
039300000000      * ordinamento schiere eventi
039400000000     d                 DS
039500000000     d  dsseql                 1      3  0                                       -sequenza legame
039600000000     d  dsseqe                 4      6  0                                       -sequenza evento
039700000000     d  dsdevhev               7     18  0                                       -data + ora
039800000000     d  dsi                   19     21  0                                       -indice
039900000000     d dssort                  1     21  0                                      *ds cod spedizione
040000000000     d* Input routine XALLINEA
040100000000     d walfa7          S              7                                         *alfanumerico 7
040200000000     d walfa7bis       S              7                                         *alfanumerico 7
040300000000     d wzeri           S              7                                         *alfa con zeri
040400000000     d*---
040500000000     d* Variabili
040600000000     d*---
040700101104     d i               S             10I 0                                      *indici
040800101104     d ii              S             10I 0
040900101104     d j               S             10I 0
041000101104     d jj              S             10I 0
041100101104     d z               S             10I 0
041200101104     d s               S             10I 0
041300101104     d k               S             10I 0
041400101104     d o               S             10I 0
041500101104     d tote            S             10I 0                                      *totale eventi memor
041600000000     d seql            S              5  0                                      *sequenza legami
041700000000     d n14             S             14  0                                      *Numerico 14
041800000000     d n8              S              8  0                                      *Numerico 8
041900000000     d n7              S              7  0                                      *Numerico 7
042000000000     d n4              S              4  0                                      *Numerico 4
042100010927     d n3              S              3  0                                      *Numerico 3
042200000000     d a1              S              1                                         *Alfanumerico 1
042300000000     d a3              S              3                                         *Alfanumerico 3
042400000000     d a5              S              5                                         *Alfanumerico 5
042500010717     d a8              S              8                                         *Alfanumerico 8
042600000000     d a15             S             15                                         *Alfanumerico 15
042700000000     d a200            S            200                                         *Alfanumerico 200
042800000000     d a200bis         S            200                                         *Alfanumerico 200
042900010927     d a230            S            230                                         *Alfanumerico 230
043000000000     d a400            S            400                                         *Alfanumerico 400
043100000000     d datcor          S              8  0                                      *data corrente
043200000000     d datpre          S              8  0                                      *data precedente
043300000000     d oracor          S              6  0                                      *ora corrente
043400000000     d anncor          S              4  0                                      *anno corrente
043500000000     d annpre          S              4  0                                      *anno precedente
043600140617     d wIdMsg          s              7a
043700140617     d wIdCampo        s             10a
043800000000     d werr            S              1                                         *errore generico
043900000000     d wdat            S              8  0                                      *data (aaaammgg)
044000000000     d wdate           S             10                                         *data (gg/mm/aaaa)
044100000000     d wora            S              4  0                                      *ora  (hhmm)
044200000000     d worae           S              5                                         *ora  (hh:mm)
044300000000     d worg            S              3  0                                      *filiale
044400000000     d wodes           S             30                                         *descrizione filiale
044500000000     d wofl1           S              1                                         *flag italia/estero
044600020702     d wodpd           S              3                                         *flag filiale DPD
044700000000     d wcev            S              3                                         *causale evento
044800030130     d wcdex           S             30                                         *descrizione bar/pt
044900000000     d wpgm            S             20                                         *programma
045000000000     d wftc            S              1                                         *cons.partic.
045100140714     d wricons         S              1                                         *wrk per riconsegna
045200140715     d keyforzata      S              1                                         *wrk per riconsegna
045300000000     d wdftc           S             50                                         *descriz.cons.partic
045400000000     d d1wdftc         S             50                                         *descriz.cons.part.1
045500000000     d d2wdftc         S             50                                         *descriz.cons.part.2
045600000000     d wgcx            S              2                                         *turno chiusura
045700000000     d wdgcx           S             20                                         *descriz.turno chius
045800000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
045900000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
046000000000     d we              S              3                                         *descrizione 'e'
046100000000     d bollaorig       S              1                                         *bolla originaria
046200000000     d sostrit         S              1                                         sostituisce Ritiro
046300000000     d sostcon         S              1                                         sostituisce Consegna
046400000000     d newevb          S              1                                         *nuovo evento
046500000000     d lavdevhev       S             12  0                                      *data + ora
046600040913     D WrkCurDate      S               D                                        Data corrente
046700060620     D wrkDtInvDisp    S              8
046800060620     D wrkLnaItaEst    S              1
046900060620     D rpyOpCode       S             10A
047000060620     D rpyEsito        S             10I 0
047100060621     D nullPtr         S               *
047200080131     D titas000FigliaPtr...
047300080131     D                 S               *   INZ(%ADDR(titas000Figlia))
047400010716     d*---
047500010716     d* Variabili di controlli
047600010716     d*---
047700010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
047800010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
047900000000     d*---
048000000000     d* Variabili di memorizzazione
048100000000     d*---
048200000000     d mhcre           S              5                                         *consegna richiesta
048300000000     d mdcre           S             10
048400000000     d mtcre           S             50
048500000000     d mftce           S            100                                         *consegna partic.
048600000000     d mgcxe           S            100                                         *turno chiusura
048700000000     d mffd            S            100                                         *fermo deposito
048800010927     d mmncB           S             35                                         *NON consegna LAV
048900010927     d mmncC           S             35
049000010927     d mmncG           S             35                                         *NON consegna GIAC
049100010927     d mmncH           S             35
049200010927     d mmncN           S             35                                         *NON consegna NOFATT
049300010927     d mmncO           S             35
049400050701     d mmnc8           S             35
049500050701     d mmnc9           S             35
049600030206     d mtel            S             20                                         *N° tel destinatario
049700030206     d mban            S             35                                         *bancali
049800030207     d mscr            S            500                                         *supermercati
049900030210     d msdace          S             10
050000030210     d msorce          S              5
050100030210     d msdcre          S             10
050200030210     d mshcre          S              5
050300030210     d mstcre          S             30
050400030210     d msnom           S             50
050500030210     d mscrT           S            500    DIM(5)
050600030211     d wmscrT          S           2504
050700000000     d*---
050800000000     d* Schiere
050900000000     d*---
051000000000      * taabella clienti del cliente unificante
051100000000      * tabella tipi servizio
051200000000     d stsp            S              1    DIM(20)                              *tipi servizio
051300000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
051400000000      * tabella consegne anomale
051500030130     d scca            S              1    DIM(30)                              *consegna anomala
051600000000      * tabella disposizioni giacenza
051700000000     d sdis            S              1    DIM(10)                              *codice
051800000000     d sddis           S             20    DIM(10)                              *descrizione italian
051900000000      * tabella filiali
052000000000     d sorg            S              3  0 DIM(300)                             *filiale
052100000000     d sodes           S             30    DIM(300)                             *descrizione filiale
052200000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
052300020702     d sopt            S              3    DIM(300)                             *flag poste/no poste
052400020702     d sodpd           S              3    DIM(300)                             *flag DPD/no DPD
052500000000      * eventi memorizzati
052600000000     d smi             S              3  0 DIM(200)                             *indice
052700000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
052800000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
052900000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
053000000000     d smdeve          S             10    DIM(200)                             *data evento editata
053100000000     d smheve          S              5    DIM(200)                             *ora  evento editata
053200000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
053300000000     d smdev           S             30    DIM(200)                             *descrizione evento
053400000000     d smcev           S              3    DIM(200)                             *causale evento
053500130313     d smspe           S             16  0 DIM(200)                             *causale evento
053600000000      * eventi memorizzati x ordinamento
053700000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
053800000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
053900000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
054000000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
054100000000     d spdeve          S             10    DIM(200)                             *data evento editata
054200000000     d spheve          S              5    DIM(200)                             *ora  evento editata
054300000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
054400000000     d spdev           S             30    DIM(200)                             *descrizione evento
054500000000     d spcev           S              3    DIM(200)                             *causale evento
054600130313     d spspe           S             16  0 DIM(200)                             *causale evento
054700000000      * note giacenze memorizzate
054800051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
054900051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
055000000000      * note memorizzate
055100000000     d smnot           S            100    DIM(10)
055200030206     d*---
055300030206     d* definizioni per passaggio parametri
055400030206     d*---
055500000000     d esito           s              1
055600000000      * DS contenente i dati di INPUT
055700140603     d tis778dsi     e DS                  INZ(*EXTDFT)
055800050104     D  cAASI74                       4
055900050104     D                                     OVERLAY(AASI74)
056000000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
056100140530     d tis778dso     e DS                  INZ(*EXTDFT)
056200000000     d dev                           10    DIM(50) OVERLAY(dato74)
056300000000     d hev                            5    DIM(50) OVERLAY(orao74)
056400000000     d fle                           30    DIM(50) OVERLAY(opeo74)
056500041007     d eve                           30    DIM(50) OVERLAY(eveo74)
056600041007     d cev                            3    DIM(50) OVERLAY(cevo74)
056700000000     d nota                         100    DIM(10) OVERLAY(notao74)
056800130313     d spe             S             16  0 DIM(50)
056900040906
057000140616     d tis778dsM     e DS                  INZ(*EXTDFT)
057100140616     d  skIdMsg                       7a   dim(99) overlay(IdMsg)
057200140616     d  skIdCampo                    10a   dim(99) overlay(IdCampo)
057300140616     d  skErrWarn                     1a   dim(99) overlay(ErrWarn)
057400140616     d  skTextMsg                   255a   dim(99) overlay(TextMsg)
057500140530      *
057600040906     D FIDN12DS      E DS
057700040906     D                                     INZ
057800040906     D  I12FEL       E
057900040906     D                                     INZ('E')
058000040906     D  I12FAN       E
058100040906     D                                     INZ('E')
058200040906     D  O12KeyAry                    14
058300040906     D                                     DIM(20)
058400040906     D                                     OVERLAY(O12KEY)
058500040906
058600040906     D O12KeyDS        DS
058700040906     D                                     INZ
058800040906     D  O12KeyAAC                     4S 0
058900040906     D  O12KeyFil                     3S 0
059000040906     D  O12KeyNCA                     7S 0
059100040906
059200020702      * DS lettura tabella "NTW"
059300020702     d DNTW          E DS
059400130326     d Ddstflr       E DS
059500041223
059600041223     D ChkCode         S             10U 0
059700041223     D nEsito          S              5I 0
059800051110     D esito10         S             10I 0
059900080131
060000100119     D*--------------------------------------------------
060100100119     D* Procedure name: GetDescrizioneIncassoContrassegno
060200100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
060300100119     D*                          o del contrassegno.
060400100121     D* Returns:        Descrizione modalità incasso contrassegno.
060500100119     D*--------------------------------------------------
060600100119     D GetDescrizioneIncassoContrassegno...
060700100119     D                 PR            35A
060800110304
060900110304     D*--------------------------------------------------
061000110304     D* Procedure name: IsContrassegnoCompensato
061100110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
061200110304     D*                          to con una fattura.
061300110304     D* Returns:        *ON = Compensato
061400110304     D*--------------------------------------------------
061500110304     D IsContrassegnoCompensato...
061600110304     D                 PR              N
061700120321
061800120321     D*--------------------------------------------------
061900120321     D* Procedure name: SetIdAssegnoMittente
062000120321     D* Purpose:        Imposta l'ID assegno mittente.
062100120321     D* Returns:        Esito.
062200120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
062300120321     D*                          segni mittente.
062400120321     D* Parameter:      priAbi => ABI assegno.
062500120321     D* Parameter:      priCab => CAB assegno.
062600120321     D*--------------------------------------------------
062700120321     D SetIdAssegnoMittente...
062800120321     D                 PR            10I 0
062900120321     D  priNumeroAssegno...
063000120321     D                                     LIKE(caNumO74)
063100120321     D  priAbi                             LIKE(caAbiO74)
063200120321     D  priCab                             LIKE(caCabO74)
063300130513
063400130513     D*--------------------------------------------------
063500130513     D* Procedure name: GetFilUrl
063600130513     D* Purpose:        Restituisce l'URL della filiale.
063700130513     D* Returns:        URL filiale.
063800130513     D* Parameter:      piFiliale => ID filiale
063900130513     D*--------------------------------------------------
064000130513     D GetFilUrl       PR           256A
064100130513     D  piFiliale                     3P 0 CONST
064200130513
064300130513
064400140527       //--------------------------------------------------------------
064500140527       //?Definizione prototipi.
064600140527       //--------------------------------------------------------------
064700140527      /copy gaitrasrc/srcprotopr,tis778R
064800140529      /copy gaitrasrc/srcconst,tis778R
064900140527       //--------------------------------------------------------------
065000140527       //?Definizione parametri programma.
065100140527       //--------------------------------------------------------------
065200140529     d tis778_GetBolla...
065300140527     d                 PI
065400140527     d prmRqsOpCode...
065500140527     d                               10I 0 CONST
065600140527     d prmRpyOpCode...
065700140527     d                               10I 0
065800140527     d prmRpyIdMsg...
065900140527     d                               10I 0
066000140527     d prmRqsFormato...
066100140527     d                               10A   CONST
066200140527     d prmRqsData...
066300140527     d                            32767A   OPTIONS(*VARSIZE)
066400140527     d prmRqsDataSize...
066500140527     d                               10I 0 CONST
066600140527     d prmRpyFormato...
066700140527     d                               10A   CONST
066800140527     d prmRpyData...
066900140527     d                            32767A   OPTIONS(*VARSIZE)
067000140527     d prmRpyDataSize...
067100140527     d                               10I 0 CONST
067200140527     d prmRpyFormMsg...
067300140527     d                               10A   CONST OPTIONS(*NOPASS)
067400140527     d prmRpyMessage...
067500140527     d                            32767A   OPTIONS(*VARSIZE : *NOPASS)
067600140527     d prmRpyMsgSize...
067700140527     d                               10I 0 CONST OPTIONS(*NOPASS)
067800080131
067900000000     i*--------------------------------------------------------------------------------------------*
068000000000     i* Input
068100000000     i*--------------------------------------------------------------------------------------------*
068200000000     i*---
068300000000     i* Indicatori lettura formati record -TITAS-
068400000000     i*---
068500000000     ititas000      01
068600000000     ititas010      02
068700000000     ititasp00      03
068800000000     c*--------------------------------------------------------------------------------------------*
068900000000     c* Main lines
069000000000     c*--------------------------------------------------------------------------------------------*
069100000000     c*
069200000000     c* operazioni iniziali -da fare sempre-
069300000000     c                   EXSR      rutinz
069400000000     c*
069500000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
069600000000if  1c                   IF        esito <> '1'                                 *no errori
069700100219     c*
069800100219     c                   MOVEL     dslnpi74      kaslnp
069900100219     c                   MOVEL     dsnrsi74      kasnrs
070000100219     c                   MOVEL     dsnspi74      kasnsp
070100100219     c*
070200111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
070300111103     c* restituisco errore.
070400111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
070500111103     c* e se non la trovo provo con l'anno precedente.
070600111103     c                   IF        aasI74 <> *ZERO
070700100219     c                   Z-ADD     aasI74        kasaas
070800100219     c     keytas        SETLL     titas30c                               99
070900111103     c                   IF        NOT *IN99
071000111103     c                   EVAL      esito = '1'
071100111103     c                   ENDIF
071200111103     c                   ELSE
071300000000     c                   Z-ADD     anncor        kasaas
071400000000     c     keytas        SETLL     titas30c                               99
071500000000if  2c                   IF        NOT *in99                                    *non trovato
071600000000     c                   Z-ADD     annpre        kasaas
071700000000     c     keytas        SETLL     titas30c                               99
071800000000if  3c                   IF        NOT *in99                                    *non trovato
071900000000     c                   MOVEL     '1'           esito                          *errore
072000000000e   3c                   ENDIF
072100000000e   2c                   ENDIF
072200100219     c                   ENDIF
072300140616     c                   if        esito = '1'
072400140617     c                   eval      widmsg = 'TIS0564'
072500140616     c                   eval      widcampo = 'NSPEDIZI74'
072600140616     c                   clear                   wparm
072700140616     c                   exsr      messaggi
072800140616     c                   endif
072900141126      *imposta chiave d'ingresso originale per chiamate con riferimenti da input senza forzatura
073000141126     c                   eval      kasaasm = kasaas
073100141126     c                   eval      kaslnpm = kaslnp
073200141126     c                   eval      kasnrsm = kasnrs
073300141126     c                   eval      kasnspm = kasnsp
073400141126      * se trovata con setll verifico se si tratta di bolla mamma nel caso forzo lettura da
073500140715      * ultima figlia per forzare i dati in output
073600140714     c                   exsr      cercafiglia
073700000000     c*
073800000000     c* se trovata, legge la bolla
073900000000if  2c                   IF        esito <> '1'
074000000000     C                   SETOFF                                       010203
074100140715     c     keytas        SETLL     titas30c
074200000000     c     keytas        READE     titas30c                               99    *no errore
074300000000if  3c                   IF        *in99                                        *non trovato
074400000000     c                   MOVEL     '1'           esito                          *errore
074500140617     c                   eval      widmsg = 'TIS0734'
074600140616     c                   eval      widcampo = 'NSPEDIZI74'
074700140616     c                   clear                   wparm
074800140616     c                   exsr      messaggi
074900000000x   3c                   ELSE                                                   *trovata
075000000000if  4c                   IF        *in01                                        *lettura titas000
075100000000     c                   EVAL      titasdssav = titasds
075200000000e   4c                   ENDIF
075300000000if  4c                   IF        *in02                                        *lettura titas010
075400000000     c                   EVAL      titasdssav = titasds
075500000000e   4c                   ENDIF
075600000000if  4c                   IF        *in03                                        *lettura titasp00
075700000000     c                   EVAL      titasdssav = titasds
075800000000e   4c                   ENDIF
075900141117      * se trovo la figlia ma ancora non è su titas
076000141117     c                   if        keyforzata <> *blank and
076100141117     c                             f_tasnsp = 0
076200141117     c                   eval      titasdssavf = titasdssav
076300141120     c                   else
076400141120     c                   clear                   keyforzata
076500141117     c                   endif
076600000000e   3c                   ENDIF
076700000000     c*
076800000000     c* controlla validità bolla
076900000000     c                   EXSR      chktas
077000000000     c*
077100030130     c* imposta i campi della DS di Output
077200000000if  3c                   IF        esito <> '1'                                 *no errore
077300140714      * imposta campi da restituire nella dsoutput non imposto più dati di titas ma quelli della
077400140714      * spedizione richiesta a video sempre
077500100225     c*
077600140716     c                   EVAL      dstaslnp = s_taslnp
077700140716     c                   EVAL      dstasnrs = s_tasnrs
077800140716     c                   EVAL      dstasnsp = s_tasnsp
077900140716     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
078000140723     c* chain ar5 con bolla richiesta a video
078100140723     c                   Z-ADD     s_tasaas      kr5aas
078200140723     c                   Z-ADD     s_taslnp      kr5lnp
078300140723     c                   Z-ADD     s_tasnrs      kr5nrs
078400140723     c                   Z-ADD     s_tasnsp      kr5nsp
078500100225     c                   MOVEL     'GEN'         kr5trd
078600100225     c     keyar5        CHAIN     fiar531c
078700100225     C                   IF        %FOUND
078800100225     C                   EVAL      DAR5GEN = AR5Uni
078900100225     C                   ELSE
079000100225     C                   RESET                   DAR5GEN
079100100225     C                   ENDIF
079200000000     c*
079300000000     c* imposta i campi della DS di Output dalle bolle
079400000000     c                   EXSR      impdsotas
079500000000     c*
079600000000     c* imposta i campi della DS di Output dagli eventi
079700000000     c                   EXSR      impdsoevb
079800000000     c*
079900000000     c* imposta i campi della DS di Output dal contrassegno
080000000000     c                   EXSR      impdsocsb
080100000000     c*
080200000000     c* imposta i campi della DS di Output dalla giacenza
080300000000     c                   EXSR      impdsogcp
080400000000     c*
080500000000     c* imposta i campi della DS di Output dalle note
080600000000     c                   EXSR      impdsonot
080700040330     c*
080800040330     c* imposta i campi della DS di Output del destinatario.
080900040330     c                   EXSR      ImpDSDest
081000010716     C*
081100010716     c* imposta i campi della DS di Output dalla firma DPD
081200010716     c                   EXSR      ImpdsoDPD
081300040913     C*
081400040913     c* imposta i campi della DS di Output di addebito immagine LDV.
081500040913     c                   EXSR      AddebitoLDV
081600000000e   3c                   ENDIF
081700000000e   2c                   ENDIF
081800140613e   1c                   ENDIF
081900140919      *riempie campi con riferimenti alla consegna
082000141016     c                   if        esito <> '1'
082100140919     c                   if        f_tasdcm > 0
082200140919     c                   movel     f_tasdcm      wdat
082300140919     c                   exsr      edtdat
082400140919     c                   eval      dtconmeo74  = wdate
082500140919     c                   MOVEL     f_tashmc      wora
082600140919     c                   EXSR      edtora
082700140919     c                   eval      orconmeO74  =  worae
082800140919     c                   eval      flgbolcO74  =  'S'
082900140919     c                   eval      flgboldO74  =  'N'
083000141014      *firmatario distinta PDA
083100141014     c                   clear                   firmato74
083200141015     c                   if        gen§ar5pdaco <> 'NO'
083300141014     c                   exsr      repfirmat
083400141014     c                   else
083500141014     c                   if        f_tasgma <> *blank
083600141015     C                   EVAL      tblKey = f_tasgma
083700141014     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
083800141014     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
083900141014     C                             :rpyOpCode:rpyEsito)
084000141014     C                   IF        rpyOpCode = 'FOUND'
084100141014     c                   if        §7r2fi = 'S'
084200141014     c                   exsr      repfirmat
084300141014     c                   endif
084400141014     c                   endif
084500141014     c                   endif
084600141014     c                   endif
084700141014      *else consegna
084800140919     c                   else
084900140919     c                   eval      flgbolcO74  =  'N'
085000140919     c                   if        F_tasndc > 0 and f_tasddc > 0
085100141119     c                             and f_tasddc <= datcor
085200140919     c                   eval      flgboldo74 = 'S'
085300140919     c                   else
085400140919     c                   eval      flgboldO74  =  'N'
085500140919     c                   endif
085600140919     c                   endif
085700130312     c* se ultimo evento inserito è un MIC (1° della schiera perchè
085800130312     c* è discendente) verifico situazione delle spedizione in filiale
085900140613if  3c                   IF        esito <> '1'                                 *no errore
086000130312     c                   IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
086100140609     c                             and cev(49) = ' ' and cev(50) = ' ' or
086200141120     c                             f_tasdcm = 0 and keyforzata = *blank
086300130312     c                   exsr      srarb
086400140604      *se spedizione non consegna forzo i dati precedentemente impostati da TAS con ARB
086500140718     c                   if        f_tasdcm = 0 and %found(fnarb01l)
086600140605     c                   exsr      impdsoarb
086700140604     c                   endif
086800140613     c                   endif
086900140613     c                   endif
087000141016     c                   endif
087100000000     c*
087200140530     C                   exsr      routend
087300141014      *__________________________________________________________________
087400141014     c     repfirmat     begsr
087500141014      *__________________________________________________________________
087600141014     c                   Z-ADD     f_tasaas      ka4aas
087700141014     c                   Z-ADD     f_taslnp      ka4lnp
087800141014     c                   Z-ADD     f_tasnrs      ka4nrs
087900141014     c                   Z-ADD     f_tasnsp      ka4nsp
088000141014     c                   MOVEL     '1'           ka4trc
088100141014     c     keyta4        CHAIN     tita430c                           99
088200141014if  1c                   IF        NOT *in99
088300141014     c                   MOVEL     ta4not        firmato74
088400141014e   1c                   ENDIF
088500141014     c                   endsr
088600141015      *__________________________________________________________________
088700141015     c     repfirmatARB  begsr
088800141015      *__________________________________________________________________
088900141015      *firmatario da arb
089000141015     c                   Z-ADD     arbaas        ka4aas
089100141015     c                   Z-ADD     arblnp        ka4lnp
089200141015     c                   Z-ADD     arbnrs        ka4nrs
089300141015     c                   Z-ADD     arbnsp        ka4nsp
089400141015     c                   MOVEL     '1'           ka4trc
089500141015     c     keyta4        CHAIN     fiar401l                           99
089600141015if  1c                   IF        NOT *in99
089700141015     c                   MOVEL     ar4not        firmato74
089800141015e   1c                   ENDIF
089900141015     c                   endsr
090000000000     c*--------------------------------------------------------------------------------------------*
090100000000     c* controlla validità bolla
090200000000     c*--------------------------------------------------------------------------------------------*
090300000000     c     chktas        BEGSR
090400000000     c*
090500000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
090600000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
090700000000     c                   Z-ADD     s_tasmgs      dsmgsB
090800000000if  1c                   IF        dsdatB < datpre
090900000000     c                   MOVEL     '1'           esito                          *errore
091000000000e   1c                   ENDIF
091100030130     c* se l'esito è positivo, salva alcuni dati della bolla
091200140718     c                   EVAL      savtsp = f_tastsp
091300000000     c*
091400140718     C                   EVAL      wOrg = f_tasLna
091500051222     C                   EXSR      setLnaItaEst
091600051222     C
091700000000     c                   ENDSR
091800000000     c*--------------------------------------------------------------------------------------------*
091900000000     c* imposta i campi della DS di Output dalle bolle e files correlati
092000000000     c*--------------------------------------------------------------------------------------------*
092100140604     c     impdsotas     BEGSR
092200140604     c*
092300140718     c                   EVAL      i = %LOOKUP(f_tasLna : sorg)
092400140604     c                   EVAL      lnaDescO74 = sodes(i)
092500140718     c                   EVAL      lnaUrlO74 = GetFilUrl(f_tasLna)
092600150116     c*forzo anno (mamma) altrimenti viene composto male il link di instradamento
092700150116     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
092800150116     c                   MOVE      s_tasmgs      aasmgsO74
092801150116      *
092900140718     c                   Z-ADD     f_tasccm      ccmO74                          *cliente mittente
093000140718     c                   MOVEL     f_taslna      lineaaro74
093100140604     c*
093200140718     c                   MOVEL     f_tastbl      a1
093300140718     c                   MOVEL     f_tastbl      tpporto74
093400140604if  1c                   IF        a1 = 'A'                                     *Pagamento del
093500140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
093600140604x   1c                   ELSE                                                   *Pagamento del
093700140604     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
093800140604e   1c                   ENDIF
093900140716     c                   MOVEL     f_tastsp      tpservio74
094000140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
094100140716     C                             :'5E':f_tastsp:%LEN(f_tastsp):'TBLUNI':
094200140604     C                             rpyOpCode:rpyEsito)
094300140604     C                   EVAL      corro74 = §5EDes
094400140604     c*
094500140716     c                   EVAL      destino74 = f_tasrsd
094600140716     c                   EVAL      desindo74 = f_tasind
094700140716     c                   EVAL      descapo74 = f_tascad
094800140716     c                   EVAL      desloco74 = f_taslod
094900140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
095000140716if  1c                   IF        f_tasnzd  = *blanks
095100140716     c                   EVAL      desnazo74 = f_tasprd
095200140604x   1c                   ELSE
095300140716     c                   EVAL      desnazo74 = f_tasnzd
095400140716     c                   EVAL      desnzdo74 = f_tasnzd
095500140604e   1c                   ENDIF
095600140716     c*new               EVAL      desnazo74 = f_tasprd
095700140716     c*new               EVAL      desnzdo74 = f_tasnzd
095800140604     c*
095900140716     c                   EVAL      collio74  = f_tasncl
096000140716     c                   EVAL      pesoo74   = f_taspkf
096100140716     c                   EVAL      volumeo74 = f_tasvlf
096200140716     c                   EVAL      naturao74 = f_tasnas
096300140604     c*
096400140716     c                   EVAL      fatnumo74  = f_tasnft
096500140716     c                   MOVEL     f_tasdft      wdat
096600140604     c                   EXSR      edtdat
096700140604     c                   EVAL      fatdatao74 = wdate
096800140604     c*
096900140716     c                   EVAL      disnumo74  = f_tasndc
097000140716c    c                   MOVEL     f_tasddc      wdat
097100140604     c                   EXSR      edtdat
097200140604     c                   EVAL      disdatao74 = wdate
097300140604     **?Immagine lettera di vettura.
097400140604     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
097500140604     C                   IF        LDVO74 <> 'N'
097600140604     c                   CLEAR                   dtasflo
097700140716     c                   MOVEL     f_tasflo      dtasflo
097800140604     c                   IF        §floiml <> *BLANK
097900140604     c                   EVAL      ldvo74 = 'S'
098000140604     C                   EVAL      SpImLDVO74 =
098100140716     C                             %SUBST(%EDITC(f_tasaas:'X'):3:2) +
098200140716     C                             %EDITC(f_taslnp:'X') +
098300140716     C                             %EDITC(f_tasnrs:'X') +
098400140716     C                             %EDITC(f_tasnsp:'X')
098500140604     **?Reperisco il check code della spedizione POD
098600140604     C                   EVAL      PChkCdeO74 =
098700140604     C                             GetSpeChkCde(
098800140716     C                             %EDITC(f_tasaas:'X'):
098900140716     C                             %EDITC(f_taslnp:'X') +
099000140716     C                             %EDITC(f_tasnrs:'X') +
099100140716     C                             %EDITC(f_tasnsp:'X'):
099200140604     C                             ChkCode:nEsito)
099300140604     C                   ENDIF
099400140604     C                   ENDIF
099500140604     c*
099600140716     c                   MOVE      f_tasccm      n4
099700140604if  1c                   IF        n4 <> 8888                                   *codificato
099800140604     c                   CLEAR                   bs69ds
099900140604     c                   CLEAR                   acods
100000140604     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
100100140716     c                   MOVEL     f_tasccm      i69kac                         *cl.mittente x CNACO
100200140716     c                   MOVEL     f_tasccm      i69kin                         *cl.mittente x CNIND
100300140604     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
100400140604     c                   PARM                    bs69ds
100500140604     c                   PARM                    acods
100600140604     c                   PARM                    indds
100700140604     c                   PARM                    clpds
100800140604     c                   PARM                    clsds
100900140604if  2c                   IF        o69err <> '1'                                *no errore
101000140604     c                   EVAL      mito74    = acorag
101100140604     c                   EVAL      mitindo74 = indvia
101200140604     c                   EVAL      mitloco74 = indcit
101300140604     c                   MOVEL     indsta        a3
101400140604if  3c                   IF        a3 = *blanks
101500140604     c                   EVAL      mitnazo74 = indprv
101600140604x   3c                   ELSE
101700140604     c                   EVAL      mitnazo74 = a3
101800140604e   3c                   ENDIF
101900140604if  3c                   IF        indcae <> *blanks
102000140604     c                   EVAL      mitcapo74 = indcae
102100140604X   3c                   ELSE
102200140604     c                   MOVEL     indcap        a5
102300140604     c                   EVAL      mitcapo74 = a5
102400140604e   3c                   ENDIF
102500140604e   2c                   ENDIF
102600140604x   1c                   ELSE                                                   *non codificato
102700140716     c                   Z-ADD     f_tasaas      kaaaas
102800140716     c                   Z-ADD     f_taslnp      kaalnp
102900140716     c                   Z-ADD     f_tasnrs      kaanrs
103000140716     c                   Z-ADD     f_tasnsp      kaansp
103100140604     c                   MOVEL     'M'           kaatrc
103200140604     c     keytaa        CHAIN     titaa30c                           99
103300140604if  2c                   IF        NOT *in99
103400140604     c                   EVAL      mito74    = taarsc
103500140604     c                   EVAL      mitindo74 = taaind
103600140604     c                   EVAL      mitcapo74 = taacap
103700140604     c                   EVAL      mitloco74 = taaloc
103800140604if  3c                   IF        taanaz = *blanks
103900140604     c                   EVAL      mitnazo74 = taaprv
104000140604x   3c                   ELSE
104100140604     c                   EVAL      mitnazo74 = taanaz
104200140604e   3c                   ENDIF
104300140604e   2c                   ENDIF
104400140604e   1c                   ENDIF
104500140604     c* 2a ragione sociale destinatario.
104600140604     c                   CLEAR                   tita4000
104700140604     c                   CLEAR                   tita4p00
104800140716     c                   Z-ADD     f_tasaas      ka4aas
104900140716     c                   Z-ADD     f_taslnp      ka4lnp
105000140716     c                   Z-ADD     f_tasnrs      ka4nrs
105100140716     c                   Z-ADD     f_tasnsp      ka4nsp
105200140604     c                   MOVEL     'D'           ka4trc
105300140604     c     keyta4        CHAIN     tita430c                           99
105400140604if  1c                   IF        NOT *in99
105500140604     c                   MOVEL     ta4not        desti2O74
105600140604e   1c                   ENDIF
105700140604     c* rif partner estero
105800140604     c                   CLEAR                   tita4000
105900140604     c                   CLEAR                   tita4p00
106000140604     c                   CLEAR                   dsbl4e
106100140716     c                   Z-ADD     f_tasaas      ka4aas
106200140716     c                   Z-ADD     f_taslnp      ka4lnp
106300140716     c                   Z-ADD     f_tasnrs      ka4nrs
106400140716     c                   Z-ADD     f_tasnsp      ka4nsp
106500140604     c                   MOVEL     'E'           ka4trc
106600140604     c     keyta4        CHAIN     tita430c                           99
106700140604if  1c                   IF        NOT *in99
106800140604     c                   MOVEL     ta4not        dsbl4e
106900140604e   1c                   ENDIF
107000140604     c
107100140604     c                   CLEAR                   tita4000
107200140604     c                   CLEAR                   tita4p00
107300140604     c                   CLEAR                   dta4a
107400140716     c                   Z-ADD     f_tasaas      ka4aas
107500140716     c                   Z-ADD     f_taslnp      ka4lnp
107600140716     c                   Z-ADD     f_tasnrs      ka4nrs
107700140716     c                   Z-ADD     f_tasnsp      ka4nsp
107800140604     c                   MOVEL     'A'           ka4trc
107900140604     c     keyta4        CHAIN     tita430c                           99
108000140604if  1c                   IF        NOT *in99
108100140604     c                   MOVEL     ta4not        dta4a
108200140604e   1c                   ENDIF
108300140604     ** Riferimento mittente numerico (c'è sempre).
108400140716     C                   EVAL      rifero74 = f_tasrmn
108500140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
108600140604     C                   IF        §ta4arma <> *BLANK AND
108700140716     C                             %TRIML(§ta4arma) <> %CHAR(f_tasrmn)
108800140604     C                   EVAL      rifero74a = §ta4arma
108900140604     C                   ENDIF
109000140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
109100140604     C                   IF        §b4erp <> *BLANK AND
109200140716     C                             %TRIML(§b4erp) <> %CHAR(f_tasrmn) AND
109300140604     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
109400140604     C                   EVAL      rpto74 = §b4erp
109500140604     C                   ENDIF
109600140604
109700140604     **?Reperisco il check code della spedizione.
109800140604     C                   EVAL      SChkCdeO74 =
109900140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
110000140604     C                             NSpedizI74:ChkCode:nEsito)
110100140604
110200140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
110300140604     C                   IF        KSCI74 <> *BLANK
110400140604     c                   SETOFF                                           98
110500140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
110600140604     C                   RESET                   tis7700i0
110700141126     C                   EVAL      tis7700i0.aas = kasaasm
110800141126     C                   EVAL      tis7700i0.lnp = kaslnpm
110900141126     C                   EVAL      tis7700i0.nrs = kasnrsm
111000141126     C                   EVAL      tis7700i0.nsp = kasnspm
111100140604     C                   EVAL      tis7700i0.ksu = kscI74
111200140604     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
111300140604     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
111400140604     C                             : rpyEsito
111500140604     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
111600140604     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
111700140604     C                             )
111800140604     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
111900140604     C                             tis7700o0.bollValida = *ON)
112000140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
112100140604if  2c                   IF        NOT *in98                                      *non trovato
112200140604     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
112300140604     C                   reset                   TIS778DSO
112400140604     C                   EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
112500140604     C                   RETURN
112600140604     C                   ENDIF
112700140604e   2c                   ENDIF
112800140604     C                   ENDIF
112900140604     c*
113000140604     ** Importo assicurato.
113100140716     C                   IF         f_tasIas > 0 AND f_tasFcm <> 'F'
113200140716     C                   EVAL      vasO74 = f_tasVas
113300141015     C                   EVAL      iasO74 = f_tasIas
113400140604     C                   ENDIF
113500140604     **
113600140604     c                   ENDSR
113700000000     c*--------------------------------------------------------------------------------------------*
113800000000     c* imposta i campi della DS di Output dagli eventi
113900000000     c*--------------------------------------------------------------------------------------------*
114000000000     c     impdsoevb     BEGSR
114100000000     c*
114200000000     c* azzera le variabili di lavoro
114300000000     c                   CLEAR                   tote                           *contatore eventi
114400000000     c                   CLEAR                   seql                           *sequenza legami
114500000000     c                   CLEAR                   spsort                         *per ordinamento
114600000000     c                   CLEAR                   spseql                         *di memorizzazione
114700000000     c                   CLEAR                   spseqe
114800000000     c                   CLEAR                   spdevhev
114900000000     c                   CLEAR                   spdeve
115000000000     c                   CLEAR                   spheve
115100000000     c                   CLEAR                   spfle
115200000000     c                   CLEAR                   spdev
115300000000     c                   CLEAR                   spcev
115400130312     c                   CLEAR                   spspe
115500000000     c*
115600000000     c* imposta la sequenza legami per la bolla di Input
115700000000     c                   Z-ADD     1             seql                           *primo legaqme
115800000000     c*---
115900000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
116000000000     c*---
116100000000     c***
116200000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
116300000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
116400000000     c*   si trasforma nella consegna anomala della mamma
116500000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
116600000000     c***
116700000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
116800000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
116900000000     c                   EVAL      kblaan = s_tasaas
117000000000     c                   EVAL      kbllpn = s_taslnp
117100000000     c                   EVAL      kblnrn = s_tasnrs
117200000000     c                   EVAL      kblnsn = s_tasnsp
117300000000     c     keylbl        CHAIN     fnlbl01l                           99
117400000000if  1c                   IF        NOT *in99                                    *ha la mamma
117500000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
117600000000     c                   EVAL      kasaas = lblaap
117700000000     c                   EVAL      kaslnp = lbllpp
117800000000     c                   EVAL      kasnrs = lblnrp
117900000000     c                   EVAL      kasnsp = lblnsp
118000000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
118100000000if  2c                   IF        NOT *in99 AND
118200000000     c                             tascca <> *blanks                            *ha cons.anomala
118300060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
118400060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
118500060622     C                             :rpyEsito)
118600060622if  3c*                  IF        *in98                                        *trovata
118700060626     C                   IF        rpyOpCode = 'FOUND'
118800041222     **?Consegna anomala, POD non visualizzabile.
118900060622     C*                  IF        sapod(jj) = 'N'
119000060622     C                   IF        §7OPODImg = 'N'
119100041222     C                   EVAL      LDVO74 = 'N'
119200041222     C                   ENDIF
119300060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
119400060622     C                   IF        §7oinc <> *blanks                            *ok per internet
119500000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
119600041222e   3c                   ENDIF
119700000000e   3c                   ENDIF
119800000000e   2c                   ENDIF
119900000000e   1c                   ENDIF
120000000000     c*
120100030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
120200000000     c                   ADD       1             tote
120300060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
120400060622     c                   MOVEL     §7odei        smdev(tote)
120500030203x   3c                   ELSE                                                   *evento telef.P.O.
120600060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
120700030203e   3c                   ENDIF
120800030203e   2c                   ENDIF
120900030203     c*
121000030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
121100060622     c                   MOVEL     §7ocev        wcev                           *causale evento
121200000000     c                   EXSR      evbrit                                       *imposta evento
121300030203x   2c                   ELSE                                                   *NO sost.ritiro
121400030203if  3c                   IF        s_tasdrt > *zeros
121500000000     c                   MOVEL     '701'         wcev                           *causale evento
121600000000     c                   EXSR      deccevsta
121700030203if  4c                   IF        werr <> '1'                                  *no errore
121800000000     c                   ADD       1             tote
121900030130     c                   MOVEL     wcdex         smdev(tote)
122000000000     c                   EXSR      evbrit                                       *imposta evento
122100030203e   4c                   ENDIF
122200030203e   3c                   ENDIF
122300030203e   2c                   ENDIF
122400140710     ** ID bolla madre.                                                          dubito figlia
122500080131     c                   EVAL      waas = s_tasaas
122600080131     c                   EVAL      wlnp = s_taslnp
122700080131     c                   EVAL      wnrs = s_tasnrs
122800080131     c                   EVAL      wnsp = s_tasnsp
122900000000     c* Partenza merce
123000000000     c                   EVAL      lduc = s_tasduc                              *data partenza
123100000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
123200000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
123300000000     c                   EXSR      evbpar
123400000000     c* Arrivo merce
123500110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
123600110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
123700140723     c                   IF        gen§ar5ArrDt <> *BLANK AND
123800140723     c                             gen§ar5ArrDt <> *ZERO
123900140723     c                   MONITOR
124000140723     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
124100140723     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
124200140723     c                   ON-ERROR
124300140723     c                   ENDMON
124400140723     c                   ENDIF
124500100225     c                   EVAL      llna = s_taslna                              *linea arrivo
124600090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
124700090826     c                   EXSR      evbarr
124800000000     c*
124900000000     c* Consegna merce
125000000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
125100000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
125200000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
125300000000     c                   EVAL      llna = s_taslna                              *linea arrivo
125400000000     c                   EXSR      evbcon
125500000000     c* Lettera anomalia
125600000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
125700000000     c                   EXSR      memdct
125800000000e   1c                   ENDIF
125900000000     c*---
126000000000     c* memorizza gli eventi dal file eventi della bolla di input
126100000000     c*---
126200000000     c                   EXSR      memevb
126300000000     c*---
126400000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
126500000000     c*---
126600000000     c                   EXSR      elaevelbl
126700000000     c*
126800000000     c* imposta contatore eventi da visualizzare nella DS di Output
126900000000     c                   EVAL      cnteveo74 = tote
127000000000     c*---
127100000000     c* ordina gli eventi per sequenza legami / Data+ora evento
127200000000     c*---
127300000000     c* compone la schiera da ordinare
127400000000do  1c     1             DO        200           j
127500000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
127600000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
127700000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
127800000000     c                   Z-ADD     smi(j)        dsi                             -indice
127900000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
128000000000e   1c                   ENDDO
128100000000     c*
128200000000     c* ordina la schiera composta
128300000000     c                   SORTA     spsort
128400000000     c*
128500000000     c* ricompone le schiere da quella ordinata
128600000000     c                   Z-ADD     *zeros        jj
128700000000do  1c     1             DO        200           j
128800000000if  2c                   IF        spsort(j) >*zeros
128900000000     c                   ADD       1             jj
129000000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
129100000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
129200000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
129300000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
129400000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
129500000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
129600000000     c                   MOVEL     smheve(dsi)   spheve(jj)
129700000000     c                   MOVEL     smfle(dsi)    spfle(jj)
129800000000     c                   MOVEL     smdev(dsi)    spdev(jj)
129900000000     c                   MOVEL     smcev(dsi)    spcev(jj)
130000130312     c                   MOVEL     smspe(dsi)    spspe(jj)
130100000000e   2c                   ENDIF
130200000000e   1c                   ENDDO
130300101104     **
130400101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
130500101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
130600101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
130700101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
130800101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
130900101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
131000101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
131100101104     ** Ricordati che l'ordine per data e ora è discendente.
131200101104     C                   EVAL      j = 0
131300101104     C                   DOU       j = 0
131400101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
131500101104     C                                        : spcev : j+1)
131600101104     C                   IF        j = 0
131700101104     C                   LEAVE
131800101104     C                   ENDIF
131900101104     C                   IF        j > 1 AND
132000101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
132100101104     C                   EVAL      dev(1) = spdeve(j-1)
132200101104     C                   EVAL      hev(1) = spheve(j-1)
132300101104     C                   EVAL      fle(1) = spfle(j-1)
132400101104     C                   EVAL      eve(1) = spdev(j-1)
132500101104     C                   EVAL      cev(1) = spcev(j-1)
132600130312     C                   EVAL      spe(1) = spspe(j-1)
132700101104     C                   EVAL      spdeve(j-1) = spdeve(j)
132800101104     C                   EVAL      spheve(j-1) = spheve(j)
132900101104     C                   EVAL      spfle(j-1) = spfle(j)
133000101104     C                   EVAL      spdev(j-1) = spdev(j)
133100101104     C                   EVAL      spcev(j-1) = spcev(j)
133200130312     C                   EVAL      spspe(j-1) = spspe(j)
133300101104     C                   EVAL      spdeve(j) = dev(1)
133400101104     C                   EVAL      spheve(j) = hev(1)
133500101104     C                   EVAL      spfle(j) = fle(1)
133600101104     C                   EVAL      spdev(j) = eve(1)
133700101104     C                   EVAL      spcev(j) = cev(1)
133800130312     C                   EVAL      spspe(j) = spe(1)
133900101104     C                   ENDIF
134000101104     C                   ENDDO
134100000000     c*
134200120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
134300120831     c                   CLEAR                   cntEveO74
134400000000do  1c     1             DO        50            j
134500120831     c                   IF        spcev(j) = *BLANK
134600120831     c                   LEAVE
134700120831     c                   ENDIF
134800000000     c                   EVAL      dev(j) = spdeve(j)
134900000000     c                   EVAL      hev(j) = spheve(j)
135000000000     c                   EVAL      fle(j) = spfle(j)
135100041007     c                   EVAL      eve(j) = spdev(j)
135200041007     c                   EVAL      cev(j) = spcev(j)
135300130312     c                   EVAL      spe(j) = spspe(j)
135400101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
135500100909     c                   CLEAR                   hev(j)
135600100909     c                   ENDIF
135700120831     c                   EVAL      cntEveO74 += 1
135800000000e   1c                   ENDDO
135900000000     c*
136000000000     c                   ENDSR
136100000000     c*--------------------------------------------------------------------------------------------*
136200000000     c* memorizza gli eventi dal file eventi della bolla di input
136300000000     c*--------------------------------------------------------------------------------------------*
136400000000     c     memevb        BEGSR
136500000000     c***
136600000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
136700000000     c* . se SI lo aggiorna solo nella sequenza legame
136800000000     c*   se NO lo inserisce
136900000000     c***
137000000000     c                   Z-ADD     waas          kvbaas
137100000000     c                   Z-ADD     wlnp          kvblnp
137200000000     c                   Z-ADD     wnrs          kvbnrs
137300000000     c                   Z-ADD     wnsp          kvbnsp
137400000000     c     keyevb        SETLL     fnevb01l
137500000000     c     keyevb        READE     fnevb01l                               99
137600000000do  1c                   DOW       NOT *in99
137700000000if  2c                   IF        evbatb = *blanks                             *valido
137800140108     c                   MOVEL     evbdev        wdev
137900140108     c                   MOVEL     evbhev        whev
138000080131if  3c                   IF        evbDev > *zeros
138100000000     c                   MOVEL     evbcev        wcev
138200000000     c                   EXSR      deccevsta
138300070109     c                   EXSR      eventoAnnulla
138400000000if  4c                   IF        werr <> '1'                                  *no errore
138500000000     c*
138600000000     c* controlla se evento già inserito
138700000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
138800000000     c                   MOVEL     'S'           newevb                         *nuovo evento
138900000000do  5c     1             DO        200           j
139000000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
139100080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
139200000000     c                   MOVEL     'N'           newevb                         *già inserito
139300000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
139400080130     c                   Z-ADD     tasLna        worg
139500000000     c                   EXSR      decorg
139600000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
139700080201     c                   MOVEL     wDev          wdat
139800080201     c                   EXSR      edtdat
139900080201     c                   MOVEL     wdate         smdeve(j)
140000080201     c                   MOVEL     wHev          wora
140100080201     c                   EXSR      edtora
140200080201     c                   MOVEL     worae         smheve(j)
140300130313     c                   MOVEL     dsevbspe      smspe(j)
140400000000     c                   LEAVE                                                  *uscita ciclo
140500000000e   6c                   ENDIF
140600000000e   5c                   ENDDO
140700000000     c*
140800000000     c* incrementa contatore eventi
140900000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
141000000000     c                   ADD       1             tote
141100000000     c* indice schiera
141200000000     c                   Z-ADD     tote          smi(tote)
141300000000     c* sequenza legame
141400000000     c                   Z-ADD     seql          smseql(tote)
141500000000     c* sequenza evento
141600000000     c                   Z-ADD     020           smseqe(tote)
141700000000     c* descrizione
141800030130     c                   MOVEL     wcdex         smdev(tote)
141900000000     c* data
142000080201     c                   MOVEL     wDev          wdat
142100000000     c                   EXSR      edtdat
142200000000     c                   MOVEL     wdate         smdeve(tote)
142300000000     c* ora
142400080201     c                   MOVEL     wHev          wora
142500000000     c                   EXSR      edtora
142600000000     c                   MOVEL     worae         smheve(tote)
142700000000     c* filiale
142800140722     C                   IF        wrkLnaItaEst = ESTERO
142900140722     c                   Z-ADD     tasLna        worg
143000140722     c                   else
143100140722     c                   Z-ADD     evbfle        worg
143200140722     c                   endif
143300140722     c                   EXSR      decorg
143400140722     c                   MOVEL     wodes         smfle(tote)
143500000000     c* causale
143600000000     c                   MOVEL     evbcev        smcev(tote)
143700130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
143800130312     c* bolla legata
143900130312     c                   MOVEL     dsevbspe      smspe(tote)
144000000000     c* data+ora evento
144100080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
144200000000e   5c                   ENDIF
144300000000e   4c                   ENDIF
144400000000e   3c                   ENDIF
144500000000e   2c                   ENDIF
144600010717     c     keyevb        READE     fnevb01l                               99
144700000000e   1c                   ENDDO
144800000000     c*
144900000000     c                   ENDSR
145000000000     c*--------------------------------------------------------------------------------------------*
145100000000     c* memorizza gli "eventi fissi"
145200000000     c*--------------------------------------------------------------------------------------------*
145300000000     c     memevefix     BEGSR
145400080131     ** ID bolla in canna.
145500080131     c                   EVAL      waas = tasaas
145600080131     c                   EVAL      wlnp = taslnp
145700080131     c                   EVAL      wnrs = tasnrs
145800080131     c                   EVAL      wnsp = tasnsp
145900000000     c*
146000000000     c* Partenza merce
146100000000     c                   EVAL      lduc = tasduc                                *data partenza
146200000000     c                   EVAL      llnp = taslnp                                *filiale partenza
146300000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
146400000000     c                   EXSR      evbpar
146500000000     c* Consegna merce
146600000000     c                   EVAL      lcca = tascca                                *consegna anomala
146700000000     c                   EVAL      ldcm = tasdcm                                *data consegna
146800000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
146900000000     c                   EVAL      llna = taslna                                *linea arrivo
147000000000     c                   EXSR      evbcon
147100000000     c*
147200000000     c                   ENDSR
147300000000     c*--------------------------------------------------------------------------------------------*
147400000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
147500000000     c*--------------------------------------------------------------------------------------------*
147600000000     c     elaevelbl     BEGSR
147700000000     c*
147800000000     c* azzera le variabili di lavoro
147900000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
148000000000     c                   EVAL      deplpn = *zeros
148100000000     c                   EVAL      depnrn = *zeros
148200000000     c                   EVAL      depnsn = *zeros
148300000000     c*
148400000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
148500000000     c                   EVAL      kbllpp =  s_taslnp
148600000000     c                   EVAL      kblnrp =  s_tasnrs
148700000000     c                   EVAL      kblnsp =  s_tasnsp
148800000000     c*
148900000000     c* ciclo fino a fine mamme
149000000000     c     ke2lbl        SETLL     fnlbl02l
149100000000     c     ke2lbl        READE     fnlbl02l                               99
149200000000do  1c                   DOW       NOT *in99
149300000000     c*
149400000000     c* ciclo fino a fine figlie della mamma
149500000000do  2c                   DOW       NOT *in99
149600000000     c*
149700000000     c* legge la bolla dal legame
149800000000     c                   EVAL      kasaas = lblaan                              *figlia
149900000000     c                   EVAL      kaslnp = lbllpn
150000000000     c                   EVAL      kasnrs = lblnrn
150100000000     c                   EVAL      kasnsp = lblnsn
150200000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
150300000000if  3c                   IF        NOT *in99 AND                                *esistente
150400000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
150500000000     c                             tastbl <> 'A3' AND
150600000000     c                             tastbl <> 'F3' AND
150700000000     c                             tastbl <> 'AP' AND
150800000000     c                             tastbl <> 'FC'
150900041222     **?Immagine lettera di vettura.
151000041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
151100041222     C                   IF        LDVO74 <> 'N'
151200040330     c                   MOVEL     tasflo        dtasflo
151300040330     c                   IF        §floiml <> *BLANK
151400050325     c                   EVAL      ldvo74 = 'S'
151500040330     C                   EVAL      SpImLDVO74 =
151600040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
151700040330     C                             %EDITC(taslnp:'X') +
151800040330     C                             %EDITC(tasnrs:'X') +
151900040330     C                             %EDITC(tasnsp:'X')
152000041223     **?Reperisco il check code della spedizione POD
152100041223     C                   EVAL      PChkCdeO74 =
152200041223     C                             GetSpeChkCde(
152300041223     C                             %EDITC(tasaas:'X'):
152400041223     C                             %EDITC(taslnp:'X') +
152500041223     C                             %EDITC(tasnrs:'X') +
152600041223     C                             %EDITC(tasnsp:'X'):
152700041223     C                             ChkCode:nEsito)
152800040330     C                   ENDIF
152900041222     C                   ENDIF
153000000000     c* incrementa la sequenza legami per le bolle legate
153100000000     c                   ADD       1             seql                           *legaqme successivo
153200000000     c*---
153300000000     c* memorizza gli "eventi fissi"
153400000000     c*---
153500000000     c                   EXSR      memevefix
153600000000     c*---
153700000000     c* memorizza gli eventi
153800000000     c*---
153900000000     c                   EVAL      waas = lblaan                                *figlia
154000000000     c                   EVAL      wlnp = lbllpn
154100000000     c                   EVAL      wnrs = lblnrn
154200000000     c                   EVAL      wnsp = lblnsn
154300000000     c                   EXSR      memevb
154400000000     c*
154500000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
154600000000     c                   EVAL      depaan = lblaan                              *ultima figlia
154700000000     c                   EVAL      deplpn = lbllpn
154800000000     c                   EVAL      depnrn = lblnrn
154900000000     c                   EVAL      depnsn = lblnsn
155000000000e   3c                   ENDIF
155100000000     c*
155200000000     c* lettura successiva legami
155300000000     c     ke2lbl        READE     fnlbl02l                               99
155400000000e   2c                   ENDDO                                                  *fine figlie mamma
155500000000     c*
155600000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
155700000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
155800000000     c                             kbllpp <> deplpn OR
155900000000     c                             kblnrp <> depnrn OR
156000000000     c                             kblnsp <> depnsn
156100000000     c* nuova chiave
156200000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
156300000000     c                   EVAL      kbllpp =  deplpn
156400000000     c                   EVAL      kblnrp =  depnrn
156500000000     c                   EVAL      kblnsp =  depnsn
156600000000     c* controlla figlie
156700000000     c     ke2lbl        SETLL     fnlbl02l
156800000000     c     ke2lbl        READE     fnlbl02l                               99
156900000000if  3c                   IF        *in99                                        *non ha figlie
157000000000     c                   LEAVE                                                  *esce dal ciclo
157100000000e   3c                   ENDIF
157200000000x   2c                   ELSE                                                   *sempre stessa mamma
157300000000     c                   LEAVE                                                  *esce dal ciclo
157400000000e   2c                   ENDIF
157500000000e   1c                   ENDDO                                                  *fine mamme
157600000000     c*
157700000000     c                   ENDSR
157800000000     c*--------------------------------------------------------------------------------------------*
157900000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
158000000000     c*--------------------------------------------------------------------------------------------*
158100000000     c     evbrit        BEGSR
158200000000     c*
158300000000     c* indice schiera
158400000000     c                   Z-ADD     tote          smi(tote)
158500000000     c* sequenza legame
158600000000     c                   Z-ADD     seql          smseql(tote)
158700000000     c* sequenza evento
158800000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
158900000000     c* data
159000000000if  1c                   IF        s_tasdrt > *zeros
159100000000     c                   MOVEL     s_tasdrt      wdat
159200000000     c                   EXSR      edtdat
159300000000     c                   MOVEL     wdate         smdeve(tote)
159400000000e   1c                   ENDIF
159500000000     c* ora
159600000000if  1c                   IF        s_tashrt > *zeros
159700000000     c                   MOVEL     s_tashrt      wora
159800000000x   1c                   ELSE
159900000000if  2c                   IF        s_tasfpp = 'P'                               *PM
160000000000     c                   MOVEL     1800          wora
160100000000x   2c                   ELSE                                                   *AM
160200000000     c                   MOVEL     1200          wora
160300000000e   2c                   ENDIF
160400000000e   1c                   ENDIF
160500000000     c                   EXSR      edtora
160600000000     c                   MOVEL     worae         smheve(tote)
160700000000     c* filiale
160800000000     c                   Z-ADD     s_taslnp      worg
160900000000     c                   EXSR      decorg
161000000000     c                   MOVEL     wodes         smfle(tote)
161100000000     c* causale
161200000000     c                   MOVEL     wcev          smcev(tote)
161300000000     c* data+ora evento
161400000000     c                   MOVEL     wdat          smdevhev(tote)
161500000000     c                   MOVE      wora          smdevhev(tote)
161600000000     c*
161700000000     c                   ENDSR
161800000000     c*--------------------------------------------------------------------------------------------*
161900000000     c* Imposta l'evento Partenza
162000000000     c*--------------------------------------------------------------------------------------------*
162100000000     c     evbpar        BEGSR
162200000000     c*
162300000000     c* controlla SE e COME inserire l'evento
162400000000if  1c                   IF        lduc > *zeros
162500000000     c* imposta causale
162600000000     c                   MOVEL     '702'         wcev
162700000000     c                   EXSR      deccevsta
162800000000if  2c                   IF        werr <> '1'                                  *no errore
162900000000     c* imposta filiale
163000000000     c                   Z-ADD     llnp          worg
163100000000     c                   EXSR      decorg
163200000000     c* imposta data
163300101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
163400000000     c                   MOVEL     ldrt          wdat                           *data ritiro
163500000000x   3c                   ELSE                                                   partenza ITALIA
163600000000     c                   MOVEL     lduc          wdat                           *data partenza
163700000000e   3c                   ENDIF
163800000000     c                   EXSR      edtdat
163900000000     c* imposta ora
164000000000     c                   MOVEL     2100          wora                           *ora imposta
164100000000     c                   EXSR      edtora
164200000000     c*
164300000000     c* controlla se l'evento è già stato inserito
164400000000     c                   MOVEL     'S'           newevb                         *nuovo evento
164500000000do  3c     1             DO        200           j
164600000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
164700000000     c                   MOVE      wora          lavdevhev
164800000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
164900000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
165000000000     c                   MOVEL     'N'           newevb                         *già inserito
165100000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
165200000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
165300000000     c                   LEAVE                                                  *uscita ciclo
165400000000e   4c                   ENDIF
165500000000e   3c                   ENDDO
165600000000     c*
165700000000     c* incrementa contatore eventi
165800000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
165900000000     c                   ADD       1             tote
166000000000     c* indice schiera
166100000000     c                   Z-ADD     tote          smi(tote)
166200000000     c* sequenza legame
166300000000     c                   Z-ADD     seql          smseql(tote)
166400000000     c* sequenza evento
166500000000     c                   Z-ADD     020           smseqe(tote)
166600000000     c* descrizione
166700030130     c                   MOVEL     wcdex         smdev(tote)
166800000000     c* filiale -impostata sopra-
166900000000     c                   MOVEL     wodes         smfle(tote)
167000000000     c* data    -impostata sopra-
167100000000     c                   MOVEL     wdate         smdeve(tote)
167200000000     c* ora     -impostata sopra-
167300000000     c                   MOVEL     worae         smheve(tote)
167400000000     c* causale -impostata sopra-
167500000000     c                   MOVEL     wcev          smcev(tote)
167600000000     c* data+ora evento
167700000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
167800000000     c                   MOVE      wora          smdevhev(tote)
167900000000e   3c                   ENDIF
168000000000e   2c                   ENDIF
168100000000e   1c                   ENDIF
168200000000     c*
168300000000     c                   ENDSR
168400101129     c*------------------------------------------------------------------------*
168500101129     c*
168600101129     c* Imposta l'evento Arrivo
168700101129     c*
168800101129     c*------------------------------------------------------------------------*
168900000000     c     evbarr        BEGSR
169000101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
169100101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
169200101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
169300101129     ** sommato inutile.
169400130312     C                   IF        wrkLnaItaEst = ESTERO
169500101104     C                   LEAVESR
169600101104     C                   ENDIF
169700101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
169800101129     C                   IF        lhti = *ZERO
169900101129     C                   LEAVESR
170000101129     C                   ENDIF
170100000000     c*
170200000000     c* controlla SE e COME inserire l'evento
170300000000if  1c                   IF        ldti > *zeros OR
170400000000     c                             ldcm > *zeros
170500000000     c* imposta causale
170600101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
170700000000     c                   EXSR      deccevsta
170800000000if  2c                   IF        werr <> '1'                                  *no errore
170900000000     c* imposta filiale
171000000000     c                   Z-ADD     llna          worg
171100000000     c                   EXSR      decorg
171200000000     c* imposta data
171300000000if  3c                   IF        ldti > *zeros
171400000000     c                   MOVEL     ldti          wdat                           *data arrivo
171500000000x   3c                   ELSE
171600000000     c                   MOVEL     ldcm          wdat                           *data consegna
171700000000e   3c                   ENDIF
171800000000     c                   EXSR      edtdat
171900000000     c* imposta ora
172000000000if  3c                   IF        lhti > *zeros
172100000000     c                   MOVEL     lhti          wora                           *ora arrivo
172200000000x   3c                   ELSE
172300000000     c                   MOVEL     0600          wora                           *ora imposta
172400000000e   3c                   ENDIF
172500000000     c                   EXSR      edtora
172600000000     c*
172700000000     c* controlla se l'evento è già stato inserito
172800000000     c                   MOVEL     'S'           newevb                         *nuovo evento
172900000000do  3c     1             DO        200           j
173000000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
173100000000     c                   MOVE      wora          lavdevhev
173200000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
173300000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
173400000000     c                   MOVEL     'N'           newevb                         *già inserito
173500000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
173600000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
173700000000     c                   LEAVE                                                  *uscita ciclo
173800000000e   4c                   ENDIF
173900000000e   3c                   ENDDO
174000000000     c*
174100000000     c* incrementa contatore eventi
174200000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
174300000000     c                   ADD       1             tote
174400000000     c* indice schiera
174500000000     c                   Z-ADD     tote          smi(tote)
174600000000     c* sequenza legame
174700000000     c                   Z-ADD     seql          smseql(tote)
174800000000     c* sequenza evento
174900000000     c                   Z-ADD     020           smseqe(tote)
175000000000     c* descrizione
175100030130     c                   MOVEL     wcdex         smdev(tote)
175200000000     c* data    -impostata sopra-
175300000000     c                   MOVEL     wdate         smdeve(tote)
175400000000     c* ora     -impostata sopra-
175500000000     c                   MOVEL     worae         smheve(tote)
175600000000     c* filiale -impostata sopra-
175700000000     c                   MOVEL     wodes         smfle(tote)
175800000000     c* causale -impostata sopra-
175900000000     c                   MOVEL     wcev          smcev(tote)
176000000000     c* data+ora evento
176100000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
176200000000     c                   MOVE      wora          smdevhev(tote)
176300000000e   3c                   ENDIF
176400000000e   2c                   ENDIF
176500000000e   1c                   ENDIF
176600000000     c*
176700000000     c                   ENDSR
176800000000     c*--------------------------------------------------------------------------------------------*
176900000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
177000000000     c*--------------------------------------------------------------------------------------------*
177100140714     c     evbcon        BEGSR
177200000000     c*
177300000000     c* azzera variabili di lavoro
177400000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
177500000000     c****
177600000000     c* Consegna merce ->
177700000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
177800000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
177900000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
178000000000     c*   nè lo stato di consegna nè quello di consegna anomala.
178100000000     c****
178200000000     c* controlla SE e COME inserire l'evento
178300000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
178400060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
178500060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
178600060622     C                             :rpyEsito)
178700060622     C                   IF        rpyOpCode = 'FOUND'
178800041222     **?Consegna anomala, POD non visualizzabile.
178900060622     C                   IF        §7OPODImg = 'N'
179000041222     C                   EVAL      LDVO74 = 'N'
179100041222     C                   ENDIF
179200060622     c                   IF        §7oinc <> *blanks                            *ok per internet
179300060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
179400000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
179500000000x   3c                   ELSE                                                   *ha record FNEVB
179600000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
179700000000e   3c                   ENDIF
179800041222e   3c                   ENDIF
179900000000e   2c                   ENDIF
180000000000e   1c                   ENDIF
180100080131     c* imposta data
180200080131if  1c                   IF        ldcm > *zeros
180300080131     c                   MOVEL     ldcm          wdat
180400080131e   1c                   ENDIF
180500080131     c* imposta ora
180600080131if  1c                   IF        lhmc > *zeros
180700080131     c                   MOVEL     lhmc          wora
180800080131e   1c                   ENDIF
180900000000     c* imposta causale
181000000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
181100060622     c                   MOVEL     §7ocev        wcev                           *causale evento
181200080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
181300080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
181400080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
181500080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
181600080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
181700080131     c* ritiro merce) dalla bolla figlia.
181800080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
181900080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
182000080131     c                             AND s_tasNdc < *HIVAL
182100080131     c                   EVAL      kblAap = wAas
182200080131     c                   EVAL      kblLpp = wLnp
182300080131     c                   EVAL      kblNrp = wNrs
182400080131     c                   EVAL      kblNsp = wNsp
182500080131     c     ke2lbl        CHAIN     fnlbl02l
182600080131     c                   IF        %FOUND()
182700080131     c                   EVAL      kasAas = lblAan
182800080131     c                   EVAL      kasLnp = lblLpn
182900080131     c                   EVAL      kasNrs = lblNrn
183000080131     c                   EVAL      kasNsp = lblNsn
183100080131     C     keyTas        CHAIN     titas000      titas000Figlia
183200080131     C                   IF        %FOUND()
183300080131     C                   EVAL      wDat = titas000Figlia.tasDrt
183400080131     C                   EVAL      wOra = titas000Figlia.tasHrt
183500080131     C                   ELSE
183600080131     C     keyTas        CHAIN     titas010      titas010Figlia
183700080131     C                   IF        %FOUND()
183800080131     C                   EVAL      wDat = titas010Figlia.tasDrt
183900080131     C                   EVAL      wOra = titas010Figlia.tasHrt
184000080131     C                   ELSE
184100080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
184200080131     C                   IF        %FOUND()
184300080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
184400080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
184500080131     C                   ENDIF
184600080131     C                   ENDIF
184700080131     C                   ENDIF
184800080131     c                   ENDIF
184900080131     c                   ENDIF
185000000000x   1c                   ELSE                                                   *NO sost.Consegna
185100000000     c                   MOVEL     '704'         wcev                           *causale evento
185200000000e   1c                   ENDIF
185300080131     c* Editazione data e ora evento.
185400080131     c                   EXSR      edtdat
185500080131     c                   EXSR      edtora
185600000000     c* imposta filiale
185700000000     c                   Z-ADD     llna          worg
185800000000     c                   EXSR      decorg
185900000000     c* controlla se l'evento è già stato inserito
186000000000     c                   MOVEL     'S'           newevb                         *nuovo evento
186100000000do  1c     1             DO        200           j
186200000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
186300000000     c                   MOVE      wora          lavdevhev
186400000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
186500000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
186600000000     c                   MOVEL     'N'           newevb                         *già inserito
186700000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
186800000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
186900000000     c                   LEAVE                                                  *uscita ciclo
187000000000e   2c                   ENDIF
187100000000e   1c                   ENDDO
187200000000     c*
187300000000     c* incrementa contatore eventi
187400030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
187500030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
187600000000     c                   ADD       1             tote                           incrementa contatore
187700060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
187800060622     c                   MOVEL     §7odei        smdev(tote)
187900030203x   4c                   ELSE                                                   *evento telef.P.O.
188000060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
188100030203e   4c                   ENDIF
188200060622     c                   MOVEL     §7ocev        wcev                           *causale evento
188300000000     c                   EXSR      evbcon2                                      *imposta evento
188400030203x   3c                   ELSE                                                   *NO sost.Consegna
188500000000     c*
188600030203if  4c                   IF        ldcm > *zeros
188700030203if  5c                   IF        sostcon <> ' '                               *considera consegna
188800000000     c                   EXSR      deccevsta
188900030203if  6c                   IF        werr <> '1'                                  *no errore
189000000000     c                   ADD       1             tote                           incrementa contatore
189100030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
189200000000     c                   EXSR      evbcon2                                      *imposta evento
189300030203e   6c                   ENDIF
189400030203e   5c                   ENDIF
189500030203e   4c                   ENDIF
189600030203e   3c                   ENDIF
189700000000     c*
189800030203e   1c                   ENDIF
189900030203     c*
190000000000     c                   ENDSR
190100000000     c*--------------------------------------------------------------------------------------------*
190200000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
190300000000     c*--------------------------------------------------------------------------------------------*
190400000000     c     evbcon2       BEGSR
190500000000     c*
190600000000     c* indice schiera
190700000000     c                   Z-ADD     tote          smi(tote)
190800000000     c* sequenza legame
190900000000     c                   Z-ADD     seql          smseql(tote)
191000000000     c* sequenza evento
191100000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
191200000000     c* data    -impostata sopra-
191300000000     c                   MOVEL     wdate         smdeve(tote)
191400000000     c* ora     -impostata sopra-
191500000000     c                   MOVEL     worae         smheve(tote)
191600000000     c* filiale -impostata sopra-
191700000000     c                   MOVEL     wodes         smfle(tote)
191800000000     c* causale -impostata sopra-
191900000000     c                   MOVEL     wcev          smcev(tote)
192000000000     c* data+ora evento
192100000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
192200000000     c                   MOVE      wora          smdevhev(tote)
192300000000     c*
192400000000     c                   ENDSR
192500000000     c*--------------------------------------------------------------------------------------------*
192600000000     c* memorizza i dati della lettera di anomalia
192700000000     c*--------------------------------------------------------------------------------------------*
192800000000     c     memdct        BEGSR
192900000000     c*
193000040906     C                   RESET                   FIDN12DS
193100040906     c                   Z-ADD     waas          I12AAS
193200040906     c                   Z-ADD     wlnp          I12LNP
193300040906     c                   Z-ADD     wnrs          I12NRS
193400040906     c                   Z-ADD     wnsp          I12NSP
193500040906     C                   CALL      'FIDN12R'
193600040906     C                   PARM                    FIDN12DS
193700000000     c* memorizza la prima lettera di anomalia valida
193800040906do  1c                   IF        O12Err = *BLANK
193900040906     C                   DO        O12NCA        K
194000040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
194100040906     C                   EVAL      DCTAAC = O12KeyAAC
194200040906     C                   EVAL      DCTFIL = O12KeyFil
194300040906     C                   EVAL      DCTNCA = O12KeyNCA
194400040906     c     keydct        CHAIN     fndct01l
194500040906do  1c                   IF        %FOUND
194600000000     c*
194700000000     c* controlla validità CA
194800000000     c                   MOVEL     '0'           werr                           *NO errore
194900000000     c                   EXSR      chkdct
195000000000if  2c                   IF        werr = '0'                                   *CA valida
195100000000     c*
195200000000     c* Imposta l'evento lettera di anomalia
195300000000     c                   EXSR      evbdct
195400000000     C                   LEAVE                                                  *uscita ciclo
195500000000e   2c                   ENDIF
195600000000     c*
195700040906e   1c                   ENDIF
195800040906e   1c                   ENDDO
195900040906e   1c                   ENDIF
196000000000     C*
196100000000     c                   ENDSR
196200000000     c*--------------------------------------------------------------------------------------------*
196300000000     c* controlla validità lettera di anomalia
196400000000     c*--------------------------------------------------------------------------------------------*
196500000000     c     chkdct        BEGSR
196600000000     c*
196700000000     c* reimposta variabili di lavoro
196800000000     c                   MOVEL     '0'           werr                           *NO errore
196900000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
197000000000     c*
197100000000     c* CA annullata, esclude
197200000000if  1c                   IF        dctatb <> ' '
197300000000     c                   MOVEL     '1'           werr                           *SI errore
197400000000e   1C                   ENDIF
197500000000     c*
197600000000     c* CA non confermata, esclude
197700000000if  1c                   IF        dctfpr = ' '
197800000000     c                   MOVEL     '1'           werr                           *SI errore
197900000000e   1C                   ENDIF
198000000000     c*
198100000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
198200000000if  1c                   IF        dctfpr = 'T' AND
198300000000     c                             dctfca >= 215 OR
198400000000     c                             dctfpr = 'P' AND
198500000000     c                             dctfca >= 400
198600000000x   1c                   ELSE
198700000000     c                   MOVEL     '1'           werr                           *SI errore
198800000000e   1C                   ENDIF
198900000000     c*
199000000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
199100000000if  1c                   IF        werr = '0'
199200000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
199300060622     C                   RESET                   tibs02ds
199400060622     C                   EVAL      t02Cod = 'CCH'
199500060622     C                   EVAL      t02Ke1 = dctcch
199600060622     C                   EXSR      getTntbe00f
199700060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
199800000000     c                   MOVEL     '1'           werr                           *SI errore
199900000000e   3C                   ENDIF
200000000000e   2C                   ENDIF
200100000000e   1C                   ENDIF
200200000000     c*
200300000000     c                   ENDSR
200400000000     c*--------------------------------------------------------------------------------------------*
200500000000     c* Imposta l'evento lettera di anomalia
200600000000     c*--------------------------------------------------------------------------------------------*
200700000000     c     evbdct        BEGSR
200800000000     C*
200900000000     c* imposta causale
201000000000if  1c                   IF        wfpr = 'T'
201100000000     c                   MOVEL     '709'         wcev                           *causale evento
201200000000x   1c                   ELSE
201300000000     c                   MOVEL     '710'         wcev                           *causale evento
201400000000e   1c                   ENDIF
201500000000     c                   EXSR      deccevsta                                    *decodifica evento
201600000000     c*
201700000000     c* imposta filiale
201800000000if  1c                   IF        werr <> '1'                                  *no errore
201900000000     c                   Z-ADD     dctfil        worg
202000000000     c                   EXSR      decorg
202100000000     c* imposta data
202200000000     c                   MOVEL     dctaac        wdat
202300000000     c                   MOVE      dctmgc        wdat
202400000000     c                   EXSR      edtdat
202500000000     c* imposta ora
202600000000     c                   MOVEL     1100          wora                           *FISSA
202700000000     c                   EXSR      edtora
202800000000     c*
202900000000     c* incrementa contatore eventi
203000000000     c                   ADD       1             tote
203100000000     c* indice schiera
203200000000     c                   Z-ADD     tote          smi(tote)
203300000000     c* sequenza legame
203400000000     c                   Z-ADD     seql          smseql(tote)
203500000000     c* sequenza evento
203600000000     c                   Z-ADD     999           smseqe(tote)
203700000000     c* descrizione -impostata sopra-
203800030130     c                   MOVEL     wcdex         smdev(tote)
203900000000     c* filiale -impostata sopra-
204000000000     c                   MOVEL     wodes         smfle(tote)
204100000000     c* data    -impostata sopra-
204200000000     c                   MOVEL     wdate         smdeve(tote)
204300000000     c* ora     -impostata sopra-
204400000000     c                   MOVEL     worae         smheve(tote)
204500000000     c* causale -impostata sopra-
204600000000     c                   MOVEL     wcev          smcev(tote)
204700000000     c* data+ora evento
204800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
204900000000     c                   MOVE      wora          smdevhev(tote)
205000000000e   1c                   ENDIF
205100000000     c*
205200000000     c                   ENDSR
205300000000     c*--------------------------------------------------------------------------------------------*
205400000000     c* imposta i campi della DS di Output dal contrassegno
205500000000     c*--------------------------------------------------------------------------------------------*
205600000000     c     impdsocsb     BEGSR
205700000000     c*
205800000000     c* azzera le variabili di lavoro
205900000000     c*
206000000000     c****
206100000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
206200000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
206300000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
206400000000     c****
206500000000     c*
206600000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
206700090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
206800000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
206900000000x   1c                   ELSE                                                     *trovato
207000000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
207100000000e   1C                   ENDIF
207200000000     c*---
207300000000     c* memorizza il contrassegno della bolla di Input
207400000000     c*---
207500000000     c                   EVAL      waas = s_tasaas                              *bolla Input
207600000000     c                   EVAL      wlnp = s_taslnp
207700000000     c                   EVAL      wnrs = s_tasnrs
207800000000     c                   EVAL      wnsp = s_tasnsp
207900000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
208000000000     c                   EXSR      memcsb
208100000000     c*
208200000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
208300000000     c                   EXSR      elacsblbl
208400000000     c*
208500000000     c                   ENDSR
208600000000     c*--------------------------------------------------------------------------------------------*
208700000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
208800000000     c*--------------------------------------------------------------------------------------------*
208900000000     c     elacsblbl     BEGSR
209000000000     c*
209100000000     c* azzera le variabili di lavoro
209200000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
209300000000     c                   EVAL      deplpn = *zeros
209400000000     c                   EVAL      depnrn = *zeros
209500000000     c                   EVAL      depnsn = *zeros
209600000000     c*
209700000000     c* legge i legami delle figlie successive alla bolla di input
209800000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
209900000000     c                   EVAL      kbllpp =  s_taslnp
210000000000     c                   EVAL      kblnrp =  s_tasnrs
210100000000     c                   EVAL      kblnsp =  s_tasnsp
210200000000     c*
210300000000     c* ciclo fino a fine mamme
210400000000     c     ke2lbl        SETLL     fnlbl02l
210500000000     c     ke2lbl        READE     fnlbl02l                               99
210600000000do  1c                   DOW       NOT *in99
210700000000     c*
210800000000     c* ciclo fino a fine figlie della mamma
210900000000do  2c                   DOW       NOT *in99
211000000000     c*
211100000000     c* legge la bolla dal legame
211200000000     c                   EVAL      kasaas = lblaan                              *figlia
211300000000     c                   EVAL      kaslnp = lbllpn
211400000000     c                   EVAL      kasnrs = lblnrn
211500000000     c                   EVAL      kasnsp = lblnsn
211600000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
211700000000     c*---
211800000000     c* memorizza il contrassegno della bolla figlia legata
211900000000     c*---
212000000000if  4c                   IF        NOT *in99                                    *esistente
212100000000     c                   EVAL      waas = lblaan                                *figlia
212200000000     c                   EVAL      wlnp = lbllpn
212300000000     c                   EVAL      wnrs = lblnrn
212400000000     c                   EVAL      wnsp = lblnsn
212500000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
212600000000     c                   EXSR      memcsb
212700000000e   4c                   ENDIF
212800000000     c*
212900000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
213000000000     c                   EVAL      depaan = lblaan                              *ultima figlia
213100000000     c                   EVAL      deplpn = lbllpn
213200000000     c                   EVAL      depnrn = lblnrn
213300000000     c                   EVAL      depnsn = lblnsn
213400000000     c*
213500000000     c* lettura successiva legami
213600000000     c     ke2lbl        READE     fnlbl02l                               99
213700000000e   2c                   ENDDO                                                  *fine figlie mamma
213800000000     c*
213900000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
214000000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
214100000000     c                             kbllpp <> deplpn OR
214200000000     c                             kblnrp <> depnrn OR
214300000000     c                             kblnsp <> depnsn
214400000000     c* nuova chiave
214500000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
214600000000     c                   EVAL      kbllpp =  deplpn
214700000000     c                   EVAL      kblnrp =  depnrn
214800000000     c                   EVAL      kblnsp =  depnsn
214900000000     c* controlla figlie
215000000000     c     ke2lbl        SETLL     fnlbl02l
215100000000     c     ke2lbl        READE     fnlbl02l                               99
215200000000if  3c                   IF        *in99                                        *non ha figlie
215300000000     c                   LEAVE                                                  *esce dal ciclo
215400000000e   3c                   ENDIF
215500000000x   2c                   ELSE                                                   *sempre stessa mamma
215600000000     c                   LEAVE                                                  *esce dal ciclo
215700000000e   2c                   ENDIF
215800000000e   1c                   ENDDO                                                  *fine mamme
215900000000     c*
216000000000     c                   ENDSR
216100000000     c*--------------------------------------------------------------------------------------------*
216200000000     c* memorizza il contrassegno
216300000000     c*--------------------------------------------------------------------------------------------*
216400000000     c     memcsb        BEGSR
216500000000     c*
216600000000     c* legge il contrassegno, se avuto
216700000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
216800000000     c                   Z-ADD     waas          ksbaas
216900000000     c                   Z-ADD     wlnp          ksblnp
217000000000     c                   Z-ADD     wnrs          ksbnrs
217100000000     c                   Z-ADD     wnsp          ksbnsp
217200000000     c     keycsb        CHAIN     tncsb03l                           99
217300000000if  2c                   IF        NOT *in99                                    *esistente
217400000000     c* memorizza i dati
217500000000     c                   MOVEL     'S'           cao74                          *esiste C/A
217600000000     c*
217700000000     c                   Z-ADD     csbcas        caimpo74                       *importo
217800000000     c*
217900000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
218000010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
218100010820     c                             caimpo74 > *zeros
218200010820     c                   EVAL      cadivo74 = 'ITL'
218300010820e   3c                   ENDIF
218400100119     ** Visualizzo la descrizione del tipo incasso contrassegno solo se il
218500100119     ** contrassegno è stato incassato.
218600100121     C                   EVAL      cainco74 =
218700100121     C                                       GetDescrizioneIncassoContrassegno()
218800000000     c*
218900000000     c                   MOVEL     *blanks       castao74                       *stato
219000060622     C                   EVALR     tblKey = %CHAR(csbSta)
219100060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
219200060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
219300060622     C                             :rpyOpCode:rpyEsito)
219400060622     C                   IF        rpyOpCode = 'FOUND'
219500060622     c                   EVAL      castao74 = §4SDei
219600000000e   3c                   ENDIF
219700100119     c
219800000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
219900011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
220000060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
220100000000x   4c                   ELSE
220200000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
220300060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
220400000000x   5c                   ELSE
220500060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
220600000000e   5c                   ENDIF
220700000000e   4c                   ENDIF
220800000000e   3c                   ENDIF
220900000000     c*
221000000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
221100000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
221200000000if  4c                   IF        csbtpa = 'B'
221300060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
221400000000e   4c                   ENDIF
221500000000if  4c                   IF        csbtpa = 'C'
221600060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
221700000000e   4c                   ENDIF
221800000000x   3c                   ELSE                                                   *incasso Bartolini
221900000000if  4c                   IF        csbfpc = *blanks
222000110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
222100000000e   4c                   ENDIF
222200000000if  4c                   IF        csbfpc = '2'
222300060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
222400000000e   4c                   ENDIF
222500000000e   3c                   ENDIF
222600000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
222700000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
222800000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
222900000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
223000000000     c                             csbfpc = *blanks                              assegno di traenza
223100060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
223200000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
223300060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
223400000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
223500060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
223600000000e   4c                   ENDIF
223700000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
223800060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
223900060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
224000000000e   3c                   ENDIF
224100000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
224200000000     c                   Z-ADD     *zeros        caabio74                       *abi
224300000000     c                   Z-ADD     *zeros        cacabo74                       *cab
224400000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
224500000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
224600000000     c                   MOVEL     csbddp        wdat
224700000000     c                   EXSR      edtdat
224800000000     c                   EVAL      cadato74 = wdate
224900000000     C*
225000000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
225100000000     c                   Z-ADD     csbabi        caabio74
225200000000     c                   Z-ADD     csbcab        cacabo74
225300000000     c                   MOVEL     csbnra        canumo74
225400120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
225500120321     c                             : caCabO74)
225600000000x   4c                   ELSE                                                   *incasso Bartolini
225700000000     c                   MOVEL     csbndp        canumo74
225800000000e   4c                   ENDIF
225900000000e   3c                   ENDIF
226000110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
226100110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
226200110307     C                   IF        IsContrassegnoCompensato()
226300110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
226400110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
226500110304     C                   CLEAR                   caabio74
226600110304     C                   CLEAR                   cacabo74
226700110304     C                   CLEAR                   canumo74
226800110304     C                   ENDIF
226900110304     C*
227000000000e   2c                   ENDIF
227100000000e   1c                   ENDIF
227200000000     c*
227300000000     c                   ENDSR
227400000000     c*--------------------------------------------------------------------------------------------*
227500000000     c* imposta i campi della DS di Output dalla giacenza
227600000000     c*--------------------------------------------------------------------------------------------*
227700000000     c     impdsogcp     BEGSR
227800000000     c*
227900000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
228000000000     c                   EVAL      waas = s_tasaas                              *bolla Input
228100000000     c                   EVAL      wlnp = s_taslnp
228200000000     c                   EVAL      wnrs = s_tasnrs
228300000000     c                   EVAL      wnsp = s_tasnsp
228400000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
228500000000     c                   EXSR      memgcp
228600000000     c*
228700000000     c                   ENDSR
228800000000     c*--------------------------------------------------------------------------------------------*
228900000000     c* memorizza la giacenza
229000000000     c*--------------------------------------------------------------------------------------------*
229100000000     c     memgcp        BEGSR
229200000000     c*
229300000000     c* imposta le variabili di lavoro
229400000000     c                   CLEAR                   i                              *indice
229500000000     c                   CLEAR                   ii
229600000000     c                   CLEAR                   smdmc                          *note fase -20-
229700000000     c                   CLEAR                   smdmcR
229800000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
229900000000     c                   MOVEL     'N'           gco74                          *flag di controllo
230000000000     c                   MOVEL     'N'           gcfdio74
230100000000     c                   MOVEL     'N'           gcmito74
230200000000     c*
230300000000     c* legge la giacenza, se avuta
230400000000     c                   Z-ADD     waas          kcpaas
230500000000     c                   Z-ADD     wlnp          kcplnp
230600000000     c                   Z-ADD     wnrs          kcpnrs
230700000000     c                   Z-ADD     wnsp          kcpnsp
230800000000     c                   Z-ADD     *zeros        kcpfrg
230900050309     c     keygcp        CHAIN     tigcp51l                           99
231000050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
231100050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
231200000000if  2c                   IF        NOT *in99        AND                         *esistente
231300000000     c                             gcpatb = *blanks AND                         *no annullata
231400050415     c                             gcptcm <> 'Z'    AND
231500050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
231600050419     C
231700050419     C                   EVAL      gcRiapO74 = gcpRiap
231800050418     **?Descrizione della fase.
231900050418     C                   SELECT
232000050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
232100060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
232200050418     C                   WHEN      gcpFas = 20
232300060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
232400050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
232500060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
232600050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
232700060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
232800050418     C                   ENDSL
232900000000     c* memorizza i dati dalle NOTE GIACENZE
233000000000     c                   Z-ADD     *zeros        i
233100000000     c                   Z-ADD     *zeros        ii
233200000000     c                   Z-ADD     gcpagc        knpagc
233300000000     c                   Z-ADD     gcpfgc        knpfgc
233400000000     c                   Z-ADD     gcpngc        knpngc
233500000000     c                   Z-ADD     gcpfrg        knpfrg
233600050309     c     keygnp        SETLL     tignp01l
233700050309     c     keygnp        READE     tignp01l                               98
233800000000do  3c                   DOW       NOT *in98
233900050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
234000000000     c* fase -10-
234100000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
234200000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
234300000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
234400000000e   5c                   ENDIF
234500000000     c* fase -20-
234600000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
234700000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
234800051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
234900000000     c                   ADD       1             i
235000000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
235100000000e   6c                   ENDIF
235200000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
235300051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
235400050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
235500050405     c                   CLEAR                   ii
235600050405     c                   CLEAR                   smdmcR
235700050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
235800050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
235900050919     C                   IF        NOT %ERROR
236000140617     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
236100050919     C                   ENDIF
236200050405     C                   ELSE
236300000000     c                   ADD       1             ii
236400000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
236500050405     C                   ENDIF
236600000000e   6c                   ENDIF
236700000000e   5c                   ENDIF
236800000000e   4c                   ENDIF
236900050309     c     keygnp        READE     tignp01l                               98
237000000000e   3c                   ENDDO
237100000000     c*
237200000000     c* memorizza i dati dalle GIACENZE
237300000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
237400050404     c* Disposizione da immettere.
237500051110     c                   IF        gcpfas = 20
237600051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
237700051110     C                   EXSR      chkTivpi
237800051110e   3c                   ENDIF
237900000000     c*
238000000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
238100000000     c*
238200000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
238300000000     c*
238400060103     c                   Z-ADD     gcpagc        dsannB
238500060103     c                   Z-ADD     gcpmgc        dsmgsB
238600060103     c                   Z-ADD     dsdatB        wdat
238700000000if  3c                   IF        wdat > *zeros
238800000000     c                   EXSR      edtdat
238900000000     c                   EVAL      gcdago74 = wdate                             *data apertura
239000000000e   3C                   ENDIF
239100060104     c*
239200060104     c                   Z-ADD     gcpdur        wdat
239300060104     c                   IF        wdat > *zeros
239400060104     c                   EXSR      edtdat
239500060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
239600060104     C                   ENDIF
239700000000     c*
239800000000if  3c                   IF        gcpcmc <> *blanks
239900000000     c                   MOVEL     gcpcmc        wcev
240000000000     c                   EXSR      deccevgia
240100030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
240200000000e   3C                   ENDIF
240300000000     c*
240400000000     c                   MOVEL     *blanks       gcdcgo74
240500000000if  3c                   IF        gcpdcg > *zeros
240600000000     c                   Z-ADD     gcpdcg        wdat
240700000000     c                   EXSR      edtdat
240800000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
240900000000e   3c                   ENDIF
241000000000     c*---
241100000000     c* Disposizioni già immesse
241200000000     c*---
241300000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
241400000000     c*
241500000000if  4c                   IF        gcpddm > *zeros
241600000000     c                   Z-ADD     gcpddm        wdat
241700000000     c                   EXSR      edtdat
241800000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
241900000000e   4C                   ENDIF
242000000000     c*
242100000000if  4c                   IF        gcpdis <> *blanks
242200060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
242300060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
242400060626     C                             :rpyOpCode:rpyEsito)
242500060622     C                   EVAL      gcdiso74 = §2DDes
242600000000e   4C                   ENDIF
242700000000     c*
242800000000if  4c                   IF        gcpasg <> *blanks
242900000000if  5c                   IF        gcpasg = 'S'
243000060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
243100000000x   5c                   ELSE
243200060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
243300000000e   5C                   ENDIF
243400000000     c*
243500000000if  5c                   IF        gcppsg = 'M'
243600060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
243700000000e   5C                   ENDIF
243800000000if  5c                   IF        gcppsg = 'D'
243900060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
244000000000e   5C                   ENDIF
244100000000e   4C                   ENDIF
244200000000     c*
244300000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
244400000000     c                   MOVEL     gcpvcs        gcvcso74
244500000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
244600000000     c                   Z-ADD     gcpcas        gccaso74
244700000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
244800000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
244900000000e   5C                   ENDIF
245000000000e   4C                   ENDIF
245100050404     C                   IF        smdmc(1) <> *BLANK
245200050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
245300050404     C                             smdmc(3) + smdmc(4)
245400051014     C                   EVAL      gcdmc5O74 = smdmc(5)
245500050404     C                   ELSE
245600050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
245700050404     C                             smdmcr(3) + smdmcr(4)
245800051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
245900050404e   3C                   ENDIF
246000000000e   3C                   ENDIF
246100000000     c*---
246200000000     c* Disposizioni da immettere
246300000000     c*---
246400010719     c* se le disposizioni sono da immettere controlla che:
246500010719     c* - il cliente è il mittente della giacenza
246600000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
246700040726if  4C                   IF        keygiac <> *blanks
246800040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
246900040726x   4c                   ELSE
247000090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
247100090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
247200090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
247300090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
247400090914     C                             )
247500040726e   4C                   ENDIF
247600000000e   3C                   ENDIF
247700050404     C* Controllo esistenza storia giacenza.
247800050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
247900050404     C                   DO        *HIVAL
248000050404     C     K04GCP51      READE     TIGCP51L
248100050404     C                   IF        %EOF
248200050404     C                   LEAVE
248300050404     C                   ENDIF
248400050404     C                   IF        GCPATB = *BLANK
248500050404     C                   EVAL      StoGiaO74 = 'S'
248600050404     C                   LEAVE
248700050404     C                   ENDIF
248800050404     C                   ENDDO
248900050404     c*
249000000000e   2c                   ENDIF
249100000000     c*
249200000000     c                   ENDSR
249300000000     c*--------------------------------------------------------------------------------------------*
249400000000     c* imposta i campi della DS di Output dalle note
249500000000     c*--------------------------------------------------------------------------------------------*
249600000000     c     impdsonot     BEGSR
249700000000     c*
249800000000     c* azzera le variabili di lavoro
249900000000     c                   CLEAR                   i
250000000000     c                   CLEAR                   mdcre                          consegna richiesta
250100000000     c                   CLEAR                   mhcre
250200030210     c                   CLEAR                   mtcre
250300000000     c                   CLEAR                   mftce                          consegna particolare
250400000000     c                   CLEAR                   wftc
250500000000     c                   CLEAR                   d1wdftc
250600000000     c                   CLEAR                   d2wdftc
250700000000     c                   CLEAR                   mgcxe                          turni chiusura
250800000000     c                   CLEAR                   wgcx
250900000000     c                   CLEAR                   d1wdgcx
251000000000     c                   CLEAR                   d2wdgcx
251100010927     c                   CLEAR                   mffd                           fermo deposito
251200010927     c                   CLEAR                   mmncB                          *motivi NON consegna
251300010927     c                   CLEAR                   mmncC
251400010927     c                   CLEAR                   mmncG
251500010927     c                   CLEAR                   mmncH
251600010927     c                   CLEAR                   mmncN
251700010927     c                   CLEAR                   mmncO
251800050701     c                   CLEAR                   mmnc8
251900050701     c                   CLEAR                   mmnc9
252000050701     c                   CLEAR                   a230
252100000000     c                   CLEAR                   we
252200030206     c                   CLEAR                   mtel                           n° tel destinatario
252300030206     c                   CLEAR                   mban                           bancali
252400030207     c                   CLEAR                   mscr                           supermercati
252500030210     c                   CLEAR                   msdace
252600030210     c                   CLEAR                   msorce
252700030210     c                   CLEAR                   msdcre
252800030210     c                   CLEAR                   mshcre
252900030210     c                   CLEAR                   mstcre
253000030210     c                   CLEAR                   msnom
253100030210     c                   CLEAR                   mscrT
253200030210     c                   CLEAR                   wmscrT
253300030210     c                   CLEAR                   s
253400030207     c
253500140619     c*imposta dati data consegna richiesta
253600140716     c                   if        f_tasdcr > 0
253700140716     c                   MOVEL     f_tasdcr      wdat
253800140619     c                   EXSR      edtdat
253900140619     c                   eval      DTCONRCO74  =  wdate
254000140819if  3c                   IF        f_tastcr = ' '
254100140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
254200140819e   3c                   ENDIF
254300140819     c                   endif
254400140716     c                   if        f_tashcr > 0
254500140716     c                   MOVEL     f_tashcr      wora
254600140619     c                   EXSR      edtora
254700140619     c                   eval      ORACONRO74  =  worae
254800140619     c                   endif
254900140716     c                   eval      TPDATCRO74  =  f_tastcr
255000140619      *reperimento descrizione tipo consegna richiesta
255100140716if  3c                   IF        f_tastcr = 'P'
255200140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
255300140619e   3c                   ENDIF
255400140716if  3c                   IF        f_tastcr = 'D'
255500140619     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
255600140619e   3c                   ENDIF
255700000000     c*---
255800000000     c* memorizza le "note" che sono presenti nel record bolla
255900000000     c*---
256000000000     c* Consegna richiesta
256100140619if  1c*m                 IF        s_tasdcr = *zeros AND                        *NO cons.particolare
256200140619     c*m                           s_tashcr = *zeros
256300140619x   1c*m                 ELSE                                                   *SI cons.particolare
256400140619if  2c*m                 IF        s_tasdcr > *zeros                            *data richiesta
256500140619     c*m                 MOVEL     s_tasdcr      wdat
256600140619     c*m                 EXSR      edtdat                                       *edita data
256700140619     c*m                 MOVEL     wdate         mdcre
256800140619e   2c*m                 ENDIF
256900140619if  2c*m                 IF        s_tashcr > *zeros                            *ora  richiesta
257000140619     c*m                 MOVEL     s_tashcr      wora
257100140619     c*m                 EXSR      edtora                                       *edita ora
257200140619     c*m                 MOVEL     worae         mhcre
257300140619e   2c*m                 ENDIF
257400140619if  2c*m                 IF        s_tasdcr > *zeros AND                        *SI data richiesta
257500140619     c*m                           s_tashcr = *zeros OR                         *NO ora  richiesta
257600140619     c*m                           s_tasdcr > *zeros AND                        *SI data richiesta
257700140619     c*m                           s_tashcr > *zeros                            *SI ora  richiesta
257800140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
257900140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
258000140619e   3c*m                 ENDIF
258100140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
258200140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
258300140619e   3c*m                 ENDIF
258400140619if  3c*m                 IF        s_tastcr = ' '                               *il
258500140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
258600140619e   3c*m                 ENDIF
258700140619e   2c*m                 ENDIF
258800140619if  2c*m                 IF        s_tashcr > *zeros AND                        *SI ora  richiesta
258900140619     c*m                           s_tasdcr = *zeros                            *NO data richiesta
259000140619if  3c*m                 IF        s_tastcr = 'P'                               *prima
259100140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
259200140619e   3c*m                 ENDIF
259300140619if  3c*m                 IF        s_tastcr = 'D'                               *dopo
259400140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
259500140619e   3c*m                 ENDIF
259600140619if  3c*m                 IF        s_tastcr = ' '                               *il
259700140619     c*m                 EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
259800140619e   3c*m                 ENDIF
259900140619e   2c*m                 ENDIF
260000140619     c*m
260100140619if  2c*m                 IF        i < 10                                       *se c'è spazio
260200140619     c*m                 ADD       1             i                              *memorizza nota
260300140619     c*m                 EVAL      smnot(i) =
260400140619     c*m                           %TRIM(mtcre) +
260500140619     c*m                           ' '          +
260600140619     c*m                           %TRIM(mdcre) +
260700140619     c*m                           ' '          +
260800140619     c*m                           %TRIM(mhcre)
260900140619e   2c*m                 ENDIF
261000140619e   1c*m                 ENDIF
261100000000     c*
261200000000     c* Particolarità consegna -uno/due-
261300140716if  1c                   IF        f_tasftc <> *blanks OR
261400140716     c                             f_tastc2 <> *blanks
261500140716     c                   MOVEL     f_tasftc      wftc                           *particolarità uno
261600000000     c                   EXSR      decftc
261700000000     c                   MOVEL     wdftc         d1wdftc
261800140716     c                   MOVEL     f_tastc2      wftc                           *particolarità due
261900000000     c                   EXSR      decftc
262000000000     c                   MOVEL     wdftc         d2wdftc
262100000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
262200000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
262300060612     c                   EVAL      we = ' ' +
262400060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
262500000000x   2c                   ELSE
262600000000     c                   EVAL      we = *blanks
262700000000e   2c                   ENDIF
262800060612     c                   EVAL      mftce =
262900060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
263000060612     c                             %TRIM(d1wdftc)             +
263100060612     c                             we                         +
263200060612     c                             %TRIM(d2wdftc)
263300000000     c*
263400010927if  2c                   IF        i < 10                                       *se c'è spazio
263500000000     c                   ADD       1             i                              *memorizza nota
263600000000     c                   EVAL      smnot(i) = mftce
263700010927e   2c                   ENDIF
263800010927e   1c                   ENDIF
263900000000     c*
264000000000     c* Chiusura per turno
264100140716if  1c                   IF        f_tasgc1 <> *blanks OR
264200140716     c                             f_tasgc2 <> *blanks
264300140716if  2c                   IF        f_tasgc1 <> *blanks
264400140716     c                   MOVEL     f_tasgc1      wgcx                           *turno chiusura uno
264500000000     c                   EXSR      decgcx
264600000000     c                   MOVEL     wdgcx         d1wdgcx
264700000000e   2c                   ENDIF
264800140716if  2c                   IF        f_tasgc2 <> *blanks
264900140716     c                   MOVEL     f_tasgc2      wgcx                           *turno chiusura due
265000000000     c                   EXSR      decgcx
265100000000     c                   MOVEL     wdgcx         d2wdgcx
265200000000e   2c                   ENDIF
265300000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
265400000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
265500060612     c                   EVAL      we = ' ' +
265600060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
265700000000x   2c                   ELSE
265800000000     c                   EVAL      we = *blanks
265900000000e   2c                   ENDIF
266000060612     c                   EVAL      mgcxe =
266100060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
266200060612     c                             %TRIM(d1wdgcx)       +
266300060612     c                             we                   +
266400060612     c                             %TRIM(d2wdgcx)
266500000000     c*
266600010927if  2c                   IF        i < 10                                       *se c'è spazio
266700000000     c                   ADD       1             i                              *memorizza nota
266800000000     c                   EVAL      smnot(i) = mgcxe
266900010927e   2c                   ENDIF
267000010927e   1c                   ENDIF
267100000000     c*
267200000000     c* Fermo deposito
267300140716if  1c                   IF        f_tasffd = 'S'
267400060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
267500000000     c*
267600010927if  2c                   IF        i < 10                                       *se c'è spazio
267700010927     c                   ADD       1             i                              *memorizza nota
267800010927     c                   EVAL      smnot(i) = mffd
267900010927e   2c                   ENDIF
268000010927e   1c                   ENDIF
268100030206     c*---
268200030206     c* memorizza le "note" che NON sono presenti nel record bolla
268300030206     c* ma sul file estensione "anagrafiche"
268400030206     c*---
268500030206     c* N° telefono destinatario
268600140716     c                   Z-ADD     f_tasaas      kaaaas
268700140716     c                   Z-ADD     f_taslnp      kaalnp
268800140716     c                   Z-ADD     f_tasnrs      kaanrs
268900140716     c                   Z-ADD     f_tasnsp      kaansp
269000030206     c                   MOVEL     'O'           kaatrc
269100030206     c     keytaa        CHAIN     titaa30c                           98
269200030206if  1c                   IF        NOT *in98
269300040330     C                   EVAL      MitOrigO74 = TAARSC
269400030206e   1c                   ENDIF
269500030206     c*---
269600030206     c* memorizza le "note" che NON sono presenti nel record bolla
269700030206     c* ma sul file estensione testata boolle
269800030206     c*---
269900030206     c* Bancali
270000140716     c                   Z-ADD     f_tasaas      kr5aas
270100140716     c                   Z-ADD     f_taslnp      kr5lnp
270200140716     c                   Z-ADD     f_tasnrs      kr5nrs
270300140716     c                   Z-ADD     f_tasnsp      kr5nsp
270400030206     c                   MOVEL     'BAN'         kr5trd
270500040531     c     keyar5        CHAIN     fiar531c                           98
270600030206if  1c                   IF        NOT *in98
270700030206     c                   MOVEL     ar5uni        dar5ban
270800060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
270900060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
271000030206if  2c                   IF        §ar5rb1 > *zeros OR
271100030206     c                             §ar5rb2 > *zeros
271200060613     c                   EVAL      mban = %trim(mban) +
271300060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
271400060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
271500030206e   2c                   ENDIF
271600030206     c*
271700030206if  2c                   IF        i < 10                                       *se c'è spazio
271800030206     c                   ADD       1             i                              *memorizza nota
271900030206     c                   EVAL      smnot(i) = mban
272000030206e   2c                   ENDIF
272100030206e   1c                   ENDIF
272200040330
272300040330     **?Reperisco referente
272400030206     c* Note supermercati
272500140716if  1c     f_tasftc      IFEQ      'S'
272600140716     c     f_tastc2      OREQ      'S'
272700030210     c                   CLEAR                   s                              *indice
272800140716     c                   Z-ADD     f_tasaas      kr5aas
272900140716     c                   Z-ADD     f_taslnp      kr5lnp
273000140716     c                   Z-ADD     f_tasnrs      kr5nrs
273100140716     c                   Z-ADD     f_tasnsp      kr5nsp
273200030206     c                   MOVEL     'SCR'         kr5trd
273300040531     c     keyar5        SETLL     fiar531c
273400040531     c     keyar5        READE     fiar531c                               98
273500030210do  2c                   DOW       NOT *in98
273600030206     c                   MOVEL     ar5uni        dar5scr
273700030207     c* data nota
273800030210if  3c                   IF        ar5dac > *zeros
273900030207     c                   MOVEL     ar5dac        wdat
274000030207     c                   EXSR      edtdat                                       *edita data
274100030210     c                   MOVEL     wdate         msdace
274200030210e   3c                   ENDIF
274300030207     c* ora nota
274400030210if  3c                   IF        ar5orc > *zeros
274500030207     c                   MOVEL     ar5orc        wora
274600030207     c                   EXSR      edtora                                       *edita ora
274700030210     c                   MOVEL     worae         msorce
274800030210e   3c                   ENDIF
274900030207     c* Consegna richiesta
275000140708if  3c                   IF        §ar5dcr = *zeros AND
275100140708     c                             §ar5hcr = *zeros
275200140708x   3c                   ELSE
275300140708if  4c                   IF        §ar5dcr > *zeros
275400140708     c                   MOVEL     §ar5dcr       wdat
275500140708     c                   EXSR      edtdat                                       *edita data
275600140708     c                   MOVEL     wdate         msdcre
275700140708e   4c                   ENDIF
275800140708if  4c                   IF        §ar5hcr > *zeros
275900140708     c                   MOVEL     §ar5hcr       wora
276000140708     c                   EXSR      edtora                                       *edita ora
276100140708     c                   MOVEL     worae         mshcre
276200140708e   4c                   ENDIF
276300140708if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
276400140708     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
276500140708     c                             §ar5dcr > *zeros AND                         *SI data richiesta
276600140708     c                             §ar5hcr > *zeros                             *SI ora  richiesta
276700140708if  5c                   IF        §ar5tcr = 'P'                                *prima
276800140708     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
276900140708e   5c                   ENDIF
277000140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
277100140708     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
277200140708e   5c                   ENDIF
277300140708if  5c                   IF        §ar5tcr = ' '                                *il
277400140708     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
277500140708e   5c                   ENDIF
277600140708e   4c                   ENDIF
277700140708if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
277800140708     c                             §ar5dcr = *zeros                             *NO data richiesta
277900140708if  5c                   IF        §ar5tcr = 'P'                                *prima
278000140708     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
278100140708e   5c                   ENDIF
278200140708if  5c                   IF        §ar5tcr = 'D'                                *dopo
278300140708     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
278400140708e   5c                   ENDIF
278500140708if  5c                   IF        §ar5tcr = ' '                                *il
278600140708     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
278700140708e   5c                   ENDIF
278800140708e   4c                   ENDIF
278900030210e   3c                   ENDIF
279000030210     c* Riferimento
279100030210if  3c                   IF        §ar5nom <>  *blanks
279200060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
279300060613     c                             + ' ' + §ar5nom
279400030210e   3c                   ENDIF
279500030210     c*
279600030210if  3c                   IF        s < 5                                        *MAX 5 note
279700030210     c                   ADD       1             s
279800030210     c                   EVAL      mscrT(s) = 'Il ' +
279900030210     c                             %trim(msdace) +                              *data nota
280000030210     c                             ' '    +
280100030210     c                             %trim(msorce) +                              *ora nota
280200030210     c                             ' '    +
280300140708     c                             %trim(mstcre) +                              *tipo consegna
280400140708     c                             ' '    +
280500140708     c                             %trim(msdcre) +                              *data consegna
280600140708     c                             ' '    +
280700140708     c                             %trim(mshcre) +                              *ora consegna
280800140708     c                             ' '    +
280900030210     c                             %trim(msnom)  +                              *parlato con
281000030210     c                             ' '    +
281100030210     c                             %trim(§ar5tel) +                             *telefono
281200030210     c                             ' '    +
281300030210     c                             %trim(§ar5mot)                               *motivo
281400030210e   2c                   ENDIF
281500030210     c*
281600040531     c     keyar5        READE     fiar531c                               98
281700030206e   2c                   ENDDO
281800030210e   1c                   ENDIF
281900030210     c* NOTA completa
282000030210if  1c                   IF        mscrT(1) <> *blanks
282100030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
282200030211     c                                      ' ' +
282300030211     C                                      %trim(mscrT(2)) +
282400030211     c                                      ' ' +
282500030211     C                                      %trim(mscrT(3)) +
282600030211     c                                      ' ' +
282700030211     C                                      %trim(mscrT(4)) +
282800030211     c                                      ' ' +
282900030211     C                                      %trim(mscrT(5))
283000030210     c                   Z-ADD     *zeros        s
283100030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
283200030210     c                   ADD       1             i
283300030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
283400030210     c                   EVAL      s = s + 100
283500030210e   2c                   ENDDO
283600030210     c*
283700030210e   1c                   ENDIF
283800010927     c*---
283900010927     c* memorizza le "note" che NON sono presenti nel record bolla
284000030206     c* ma sul file estensione "riferimenti"
284100010927     c*---
284200010927     c* motivi della NON consegna
284300010927     c                   CLEAR                   tita4000
284400010927     c                   CLEAR                   tita4p00
284500140716     c                   Z-ADD     f_tasaas      ka4aas
284600140716     c                   Z-ADD     f_taslnp      ka4lnp
284700140716     c                   Z-ADD     f_tasnrs      ka4nrs
284800140716     c                   Z-ADD     f_tasnsp      ka4nsp
284900010927     c     ke2ta4        SETLL     tita430c
285000010927     c     ke2ta4        READE     tita430c                               98
285100010927do  1c                   DOW       NOT *in98
285200140708     c                   if        ta4trc = '8' OR ta4trc = '9'
285300050701     c*
285400050701if  3c                   IF        ta4trc = '8'
285500050701     c                   EVAL      mmnc8 = ta4not
285600050701e   3c                   ENDIF
285700050701if  3c                   IF        ta4trc = '9'
285800050701     c                   EVAL      mmnc9 = ta4not
285900050701e   3c                   ENDIF
286000050701     c*
286100010927e   2c                   ENDIF
286200010925     c*
286300010926     c     ke2ta4        READE     tita430c                               98
286400010926e   1c                   ENDDO
286500050701     C*
286600140708     c                   if        mmnc8 <> *blanks OR mmnc9 <> *blanks
286700050701     C*
286800050701     C                   IF        mmnc8 <> *BLANK
286900050701     C                   EVAL      a230 = %TRIML(mmnc8)
287000050701     C                   ENDIF
287100050701     C                   IF        mmnc9 <> *BLANK
287200050701     C                   IF        a230 <> *BLANK
287300050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
287400050701     C                   ELSE
287500050701     C                   EVAL      a230 = %TRIML(mmnc9)
287600050701     C                   ENDIF
287700050701     C                   ENDIF
287800010927     C*
287900050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
288000050701     C*
288100010927if  2c                   IF        i < 10                                       *se c'è spazio
288200010927     c                   ADD       1             i                              *memorizza nota
288300010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
288400010927e   2c                   ENDIF
288500010927     C*
288600010927if  2c                   IF        i < 10                                       *se c'è spazio
288700010927if  3c                   IF        n3 > 100
288800010927     c                   ADD       1             i                              *memorizza nota
288900010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
289000010927e   3c                   ENDIF
289100010927e   2c                   ENDIF
289200010927     C*
289300010927if  2c                   IF        i < 10                                       *se c'è spazio
289400010927if  3c                   IF        n3 > 200
289500010927     c                   ADD       1             i                              *memorizza nota
289600010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
289700010927e   3c                   ENDIF
289800010927e   2C                   ENDIF
289900010927e   1c                   ENDIF
290000010925     c*
290100000000     c* imposta contatore note da visualizzare nella DS di Output
290200000000     c                   EVAL      cntnoto74 = i
290300000000     c*
290400000000     c* imposta le note nella DS di Output
290500000000do  1c     1             DO        10            j
290600000000     c                   EVAL      nota(j) = smnot(j)
290700000000e   1c                   ENDDO
290800000000     c*
290900000000     c                   ENDSR
291000010716     c*--------------------------------------------------------------------------------------------*
291100010716     c* imposta i campi della DS di Output dalla firma DPD
291200010716     c*--------------------------------------------------------------------------------------------*
291300010716     c     impdsoDPD     BEGSR
291400010717     c*
291500010717     c* reimposta variabili di lavoro
291600010717     c                   RESET                   $AbFirma
291700010717     c                   CLEAR                   depute                         *deposito utente
291800010717     c                   CLEAR                   deppwd                         *deposito password
291900010717     c*---
292000010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
292100010717     c*---
292200010717     c                   Z-ADD     s_taslna      worg
292300010717     c                   EXSR      decorg
292400020702     c                   IF        wodpd = 'DPD'
292500020702     c                   MOVEL     'S'           dpdo74
292600020702     c                   ELSE
292700020702     c                   MOVEL     *blanks       dpdo74
292800020702     c                   ENDIF
292900010717     c*
293000010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
293100060627     c* Riferimento link Lettera di vettura
293200060627     c* Riferimento link Firma
293300010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
293400010717     c                   CLEAR                   tita4000
293500010717     c                   CLEAR                   tita4p00
293600060627     c                   CLEAR                   dsbl4i
293700010717     c                   Z-ADD     s_tasaas      ka4aas
293800010717     c                   Z-ADD     s_taslnp      ka4lnp
293900010717     c                   Z-ADD     s_tasnrs      ka4nrs
294000010717     c                   Z-ADD     s_tasnsp      ka4nsp
294100060627     c                   MOVEL     'I'           ka4trc
294200060627     c     keyta4        CHAIN     tita430c                           98
294300010717if  2c                   IF        NOT *in98
294400060627     c                   MOVEL     ta4not        dsbl4i
294500060627     c                   EVAL      DPDRmnO74 = §b4ipn
294600060627     c                   EVAL      DPDFnpO74 = §b4ipn
294700010717e   2c                   ENDIF
294800010717     c*
294900010717     c* cerca abilitazione firma DPD per questo segnacollo
295000010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
295100010717     c                   EXSR      RepAbilFirma
295200010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
295300010717     c*---
295400010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
295500010717     c*---
295600010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
295700010717     c* dati
295800010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
295900010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
296000010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
296100010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
296200010717     c*
296300010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
296400010717     c                   Z-ADD     s_tasaas      kvbaas
296500010717     c                   Z-ADD     s_taslnp      kvblnp
296600010717     c                   Z-ADD     s_tasnrs      kvbnrs
296700010717     c                   Z-ADD     s_tasnsp      kvbnsp
296800010717     c                   Z-ADD     99999999      kvbdev
296900010717     c                   Z-ADD     9999          kvbhev
297000010717     c     ke2evb        SETGT     fnevb01l
297100010717     c     keyevb        READPE    fnevb01l                               98
297200010717do  4c                   DOW       NOT *in98
297300101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
297400010720     c*
297500060629     c                   EVAL      devB01 = evbNot
297600060628     c                   IF        §NotDep = *BLANK
297700060628     c                   RESET                   DPDFirO74
297800060629     c                   LEAVE
297900060628     c                   ENDIF
298000060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
298100060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
298200080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
298300010720     c                   Z-ADD     *zeros        g08dat
298400010720     c                   MOVEL     '3'           g08err
298500010720     c                   CALL      'XSRDA8'
298600010720     c                   PARM                    wlbda8
298700010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
298800010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
298900010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
299000010720     c*
299100010717     c                   LEAVE                                                  *uscita ciclo
299200010717e   5c                   ENDIF
299300010717     c     keyevb        READPE    fnevb01l                               98
299400010717e   4c                   ENDDO
299500010717     c*
299600010717e   3c                   ENDIF
299700010717e   2c                   ENDIF
299800010716e   1c                   ENDIF
299900010716     c*
300000010716     c                   ENDSR
300100140604     c*--------------------------------------------------------------------------------------------*
300200140604     c* imposta i campi da FNARB se TASDCM = 0
300300140604     c*--------------------------------------------------------------------------------------------*
300400140604     c     impdsoARB     BEGSR
300500140604     c*
300600140604     c                   EVAL      i = %LOOKUP(arblna : sorg)
300700140604     c                   EVAL      lnaDescO74 = sodes(i)
300800140604     c                   EVAL      lnaUrlO74 = GetFilUrl(arblna)
300900140604     c*
300901150116      *eliminata forzatura resta impostato dalla mamma per composizione link
301000150116     c*                  MOVEL     arbaas        aasmgsO74                       *data spedizione
301100150116     c*                  MOVE      arbmgs        aasmgsO74
301200140818      *riempimento da verificare
301300140818      *se bolla in assegnato si usa ARBCCM in franco ARBKSC
301400140818      *inoltre ARBCCM potrebbe essere vuoto se non codificato (arbksc invece è sempre pieno)
301500140818      * In questo caso imposterei arblnp + 8888  PER NON LASCIARE VUOTO
301600140818
301700140818     c                   if        tpporto74 = 'A'
301800140818     c                   if        arbccm > 0
301900140604     c                   Z-ADD     arbccm        ccmO74                          *cliente mittente
302000140818     c                   else
302100140818     c                   movel     arblnp        ccmO74
302200140818     c                   move      8888          ccmO74
302300140818     c                   endif
302400140818     c                   else
302500140818     c                   z-add     arbksc        ccmO74
302600140818     c                   endif
302700140818      *
302800140604     c                   MOVEL     arblna        lineaaro74
302900140604     c                   MOVEL     arbtsp        tpservio74
303000140604     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
303100140604     C                             :'5E':arbtsp:%LEN(arbtsp):'TBLUNI':
303200140604     C                             rpyOpCode:rpyEsito)
303300140604     C                   EVAL      corro74 = §5EDes
303400140604     c*
303500140604     c                   EVAL      destino74 = arbrsd
303600140604     c                   EVAL      desindo74 = arbind
303700140604     c                   EVAL      descapo74 = arbcad
303800140604     c                   EVAL      desloco74 = arblod
303900140604      *specifiche da eliminare dopo modifica immissione bolle attivare quelle sotto con *new
304000140604if  1c                   IF        arbnzd    = *blanks
304100140604     c                   EVAL      desnazo74 = arbprd
304200140604x   1c                   ELSE
304300140604     c                   EVAL      desnazo74 = arbnzd
304400140604     c                   EVAL      desnzdo74 = arbnzd
304500140604e   1c                   ENDIF
304600140604     c*new               EVAL      desnazo74 = arbprd
304700140604     c*new               EVAL      desnzdo74 = arbnzd
304800140604     c*
304900140604     c                   EVAL      collio74  = arbncl
305000140604     c                   EVAL      pesoo74   = arbpkf
305100140604     c                   EVAL      volumeo74 = arbvlf
305200140604     c                   EVAL      naturao74 = arbnas
305300140604     c*
305400140604     c                   EVAL      disnumo74  = arbndc
305500140604c    c                   MOVEL     arbddc        wdat
305600140604     c                   EXSR      edtdat
305700140604     c                   EVAL      disdatao74 = wdate
305800140604     c*
305900140604     c                   EVAL      mito74    = arbrsm
306000140604     c                   EVAL      mitindo74 = arbinm
306100140604     c                   EVAL      mitcapo74 = arbcam
306200140604     c                   EVAL      mitloco74 = arblom
306300140604      *specifiche da eliminare dopo la modifica in immissione bolle
306400140604if  3c                   IF        arbnzm = *blanks
306500140604     c                   EVAL      mitnazo74 = arbprm
306600140604x   3c                   ELSE
306700140604     c                   EVAL      mitnazo74 = arbnzm
306800140604e   3c                   ENDIF
306900140604     c* 2a ragione sociale destinatario.
307000140604     c                   CLEAR                   tita4000
307100140604     c                   CLEAR                   tita4p00
307200140604     c                   Z-ADD     arbaas        ka4aas
307300140604     c                   Z-ADD     arblnp        ka4lnp
307400140604     c                   Z-ADD     arbnrs        ka4nrs
307500140604     c                   Z-ADD     arbnsp        ka4nsp
307600140604     c                   MOVEL     'D'           ka4trc
307700140604     c     keyta4        CHAIN     fiar401l                           99
307800140604if  1c                   IF        NOT *in99
307900140604     c                   MOVEL     ar4not        desti2O74
308000140604e   1c                   ENDIF
308100140604     c                   MOVEL     'E'           ka4trc
308200140604     c     keyta4        CHAIN     fiar401l                           99
308300140604if  1c                   IF        NOT *in99
308400140604     c                   MOVEL     ar4not        dsbl4e
308500140604e   1c                   ENDIF
308600140604     c
308700140604     ** Riferimento mittente numerico (c'è sempre).
308800140604     C                   EVAL      rifero74 = arbrmn
308900140604     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
309000140604     C                   IF        arbrma <> *blank and
309100140604     C                             %TRIML(arbrma) <> %CHAR(arbrmn)
309200140604     C                   EVAL      rifero74a = arbrma
309300140604     C                   ENDIF
309400140604     ** Visualizzo il riferimento partner solo se diverso dagli altri.
309500140604     C                   IF        §b4erp <> *BLANK AND
309600140604     C                             %TRIML(§b4erp) <> %CHAR(arbrmn) AND
309700140604     C                             %TRIML(§b4erp) <> %TRIML(arbrma)
309800140604     C                   EVAL      rpto74 = §b4erp
309900140604     C                   ENDIF
310000140604
310100140604     **?Reperisco il check code della spedizione.
310200140604     C                   EVAL      SChkCdeO74 =
310300140604     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
310400140604     C                             NSpedizI74:ChkCode:nEsito)
310500140604
310600140604     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
310700141126     C*m                 IF        KSCI74 <> *BLANK
310800141126     c*m                 SETOFF                                           98
310900140604     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
311000141126     C*m                 RESET                   tis7700i0
311100141126     C*m                 EVAL      tis7700i0.aas = arbAas
311200141126     C*m                 EVAL      tis7700i0.lnp = arbLnp
311300141126     C*m                 EVAL      tis7700i0.nrs = arbNrs
311400141126     C*m                 EVAL      tis7700i0.nsp = arbNsp
311500141126     C*m                 EVAL      tis7700i0.tbl = s_tastbl
311600141126     C*m                 EVAL      tis7700i0.ksu = kscI74
311700141126     C*m                 EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
311800141126     C*m                 CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
311900141126     C*m                           : rpyEsito
312000141126     C*m                           : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
312100141126     C*m                           : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
312200141126     C*m                           )
312300141126     C*m                 EVAL      *IN98 = (rpyEsito >= *ZERO AND
312400141126     C*m                           tis7700o0.bollValida = *ON)
312500140604     **?Se non gli appartiene, prima di restituire errore controllo il check code.
312600141126if  2c*m                 IF        NOT *in98                                      *non trovato
312700141126     C*m                 IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
312800141126     C*m                 reset                   TIS778DSO
312900141126     C*m                 EVAL      prmrpyopcode = tis778_RPYOPCODE_ERROR
313000141126     C*m                 RETURN
313100141126     C*m                 ENDIF
313200141126e   2c*m                 ENDIF
313300141126     C*m                 ENDIF
313400140604     c*
313500140604     ** Importo assicurato.
313600140604     C                   IF        arbIas > 0
313700140604     C                   EVAL      vasO74 = arbVas
313800140604     C                   EVAL      iasO74 = arbIas
313900140604     C                   ENDIF
314000140605      * se non consegnata in filiale reperisco dati riferimenti di consegna altrimenti non valorizza
314100140605     c                   if        arbdcm = 0 and wcev <> '704'
314200140617     c                   if        arbdcr > 0
314300140605     c                   MOVEL     arbdcr        wdat
314400140605     c                   EXSR      edtdat
314500140605     c                   eval      DTCONRCO74  =  wdate
314600140819if  3c                   IF        arbtcr = ' '
314700140819     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0774':langI74)
314800140819e   3c                   ENDIF
314900140617     c                   endif
315000140617     c                   if        arbhcr > 0
315100140617     c                   MOVEL     arbhcr        wora
315200140617     c                   EXSR      edtora
315300140617     c                   eval      ORACONRO74  =  worae
315400140617     c                   endif
315500140606     c                   eval      TPDATCRO74  =  arbtcr
315600140605      *reperimento descrizione tipo consegna richiesta
315700140617if  3c                   IF        arbtcr = 'P'
315800140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0775':langI74)
315900140617e   3c                   ENDIF
316000140617if  3c                   IF        arbtcr = 'D'
316100140617     c                   EVAL      DEDATCRO74  =  rtvMsgLang('TIS0773':langI74)
316200140617e   3c                   ENDIF
316300140605     C                   clear                   tnsd99ds
316400140605     C                   MOVEL     ' '           D98tla
316500140605     C                   MOVEL     'A'           D98tbo
316600140605     C                   Z-ADD     arbaas        D98aas
316700140605     C                   Z-ADD     arblnp        D98lnp
316800140605     C                   Z-ADD     arbnrs        D98nrs
316900140605     C                   Z-ADD     arbnsp        D98nsp
317000140605     C*
317100140605     C                   call      'TNSD99R'
317200140605     C                   parm                    tnsd99ds
317300140605     C*
317400140917      * chiamata a trulors per tempi standard
317500140917     c                   clear                   trulorsds
317600140917     c                   clear                   trulor2ds
317700140917     c                   clear                   trulor3ds
317800140917      * solo se questa data è valorizzata
317900140912     c                   if        d98dee > 0
318000140912     c                   movel     d98dee        wdat
318100140605     c                   exsr      edtdat
318200140605     c                   eval      DTTEOCOO74  =  wdate
318300140819     c
318400140904      * se fermo deposito non richiamo pgm per reperimento orari
318500140904     c                   if         arbffd = *blank
318600140605     c                   eval       IOREtser = 'C'
318700140605     c                   eval       IOREDDC=arbddc
318800140605     c                   eval       IOREnDC=arbndc
318900140609     c                   eval       IOREfil=arblna
319000140609     c                   eval       IOREdta=d98dce
319100140609     c                   eval       IOREcap=arbcad
319200140609     c                   eval       IOREloc=arblod
319300140609     c                   eval       IOREtfp=arbtfp
319400140609     c                   eval       IOREtfa=arbtfa
319500140610     c                   eval       IOREdti=arbdti
319600140610     c                   eval       IOREhti=arbhti
319700140610     c                   eval       IOREdcr=arbdcr
319800140610     c                   eval       IOREhcr=arbhcr
319900140610     c                   eval       IOREtcr=arbtcr
320000140819     c                   eval       IOREtsp=arbtsp
320100140819     c                   eval       IOREnar=arbnzd
320200140819     c                   eval       IOREdei=d98dei
320300140819     c                   eval       IOREoei=d98oei
320400140605     c
320500140605     c                   IF         (ARBfbc = *blanks and
320600140605     c                               ARBddc > 0 and
320700140605     c                               ARBndc > 0 and (ARBdcm = 0 or arbica=' ' or
320800140605     c                               arbica='R' or arbicc=' ' or arbicc='R'))
320900140605     c                   eval       IOREcons = 'S'
321000140612     c                   endif
321100140605     c* Orario presunto ritiro/consegna
321200140605     c                   move      arbndc        pctndc
321300140605     c                   move      arbifp        pctfgs
321400140605     c                   move      'COR'         ttrd
321500140605     c     kcet          setgt     fipct02l
321600140605     c     kcet          readpe    fipct02l
321700140605     c                   if        not %eof(fipct02l)
321800140605     c                   movel     pctdati       fipctcords
321900140605     c                   eval      ioreorp=§PCTORASTI
322000140605     c                   endif
322100140605     c                   call      'TRULORSR'
322200140605     C                   PARM                    kpjba
322300140605     C                   PARM                    trulorsds
322400140605     C                   PARM                    trulor2ds
322500140605     C                   PARM                    trulor3ds
322600140917     c                   endif
322700140818     c                   clear                   ORSTINIO74
322800140617     c                   if        OOR2STIS > 0
322900140617     c                   MOVEL     OOR2STIS      wora
323000140617     c                   EXSR      edtora
323100140617     c                   eval      ORSTINIO74  =  worae
323200140617     c                   endif
323300140818     c                   clear                   ORSTFINO74
323400140617     c                   if        OOR2STFS > 0
323500140617     c                   MOVEL     OOR2STFS      wora
323600140617     c                   EXSR      edtora
323700140617     c                   eval      ORSTFINO74  =  worae
323800140617     c                   endif
323900140818     c                   clear                   ORMIINIO74
324000140617     c                   if        OOR2MIIS > 0
324100140617     c                   MOVEL     OOR2MIIS      wora
324200140617     c                   EXSR      edtora
324300140617     c                   eval      ORMIINIO74  =  worae
324400140617     c                   endif
324500140818     c                   clear                   ORMAFINO74
324600140617     c                   if        OOR2MXFS > 0
324700140617     c                   MOVEL     OOR2MXFS      wora
324800140617     c                   EXSR      edtora
324900140617     c                   eval      ORMAFINO74  =  worae
325000140617     c                   endif
325100140711     c                   if        wricons = 'S'
325200140818     c                   clear                   ORRIDALO74
325300140617     c                   if        OOR2RIDS > 0
325400140617     c                   MOVEL     OOR2RIDS      wora
325500140617     c                   EXSR      edtora
325600140617     c                   eval      ORRIDALO74  =  worae
325700140617     c                   endif
325800140711      *se esito flag di mancata consegna nella stessa distinta imposto dati per eventuale riconsegna
325900140818     c                   clear                   ORRIALLO74
326000140617     c                   if        OOR2RIAS > 0
326100140617     c                   MOVEL     OOR2RIAS      wora
326200140617     c                   EXSR      edtora
326300140617     c                   eval      ORRIALLO74  =  worae
326400140617     c                   endif
326500140617     c                   eval      FLRICONO74  =  OOR2FRIC
326600140711     c                   else
326700140711     c                   clear                   ORRIALLO74
326800140711     c                   clear                   ORRIDALO74
326900140711     c                   eval      FLRICONO74  =  'N'
327000140711     c                   endif
327100140818     c                   clear                   ORSTICDO74
327200140617     c                   if        OOR2PRESD > 0
327300140617     c                   MOVEL     OOR2PRESD     wora
327400140617     c                   EXSR      edtora
327500140617     c                   eval      ORSTICDO74  =  worae
327600140617     c                   endif
327700140818     c                   clear                   ORSTICAO74
327800140617     c                   if        OOR2PRESa > 0
327900140617     c                   MOVEL     OOR2PRESa     wora
328000140617     c                   EXSR      edtora
328100140617     c                   eval      ORSTICAO74  =  worae
328200140617     c                   endif
328300140605     c                   eval      LOCNORMO74  =  OOR3LOC_N
328400140605     c                   eval      FLLOCNOO74  =  OOR3NORM
328500140904      *end F.D.
328600140904     c                   endif
328700140605      * RIFERIMENTI DESTINATARIO
328800140605     c                   MOVEL     'EMD'         kr5trd
328900140605     c     keyAR5        CHAIN     fiar501l
329000140605if  1c                   IF        %found(fiar501l)
329100140605     c                   MOVEL     ar5uni        dar5emd
329200140606     c                   eval      EMLDESTO74  =  §§AR5EML
329300140606     c                   eval      CELDESTO74  =  §§AR5TEL
329400140605e   1c                   ENDIF
329500140604     **
329600140605     c                   endif
329700140919     ** riempimento per campi da arb con data consegna diversa da 0
329800140919     c                   if        arbdcm > 0
329900140919     c                   movel     arbdcm        wdat
330000140919     c                   exsr      edtdat
330100140919     c                   eval      dtconmeo74  = wdate
330200140919     c                   MOVEL     arbdcm        wora
330300140919     c                   EXSR      edtora
330400140919     c                   eval      orconmeO74  =  worae
330500140919     c                   eval      flgbolcO74  =  'S'
330600140919     c                   eval      flgboldO74  =  'N'
330700141015      *firmatario distinta PDA
330800141015     c                   clear                   firmato74
330900141015     c                   if        gen§ar5pdaco <> 'NO'
331000141015     c                   exsr      repfirmatARB
331100141015     c                   else
331200141015     c                   if        arbgma <> *blank
331300141015     C                   EVAL      tblKey = arbgma
331400141015     C                   EVAL      ds7r = chainTabel00f('CHAIN3':langTabel
331500141015     C                             :'7R':tblKey:%LEN(tblKey):'TBLUNI'
331600141015     C                             :rpyOpCode:rpyEsito)
331700141015     C                   IF        rpyOpCode = 'FOUND'
331800141015     c                   if        §7r2fi = 'S'
331900141015     c                   exsr      repfirmatARB
332000141015     c                   endif
332100141015     c                   endif
332200141015     c                   endif
332300141015     c                   endif
332400140919     c                   else
332500140919     c                   eval      flgbolcO74  =  'N'
332600140919     c                   if        arbndc > 0 and arbddc > 0
332700141119     c                             and arbddc <= datcor
332800140919     c                   eval      flgboldo74 = 'S'
332900140919     c                   else
333000140919     c                   eval      flgboldO74  =  'N'
333100140919     c                   endif
333200140919     c                   endif
333300140919     c                   if        ORSTICDO74 <> *blank
333400140919     c                   eval      orincono74 = orsticdo74
333500140919     c                   eval      orficono74 = ORSticaO74
333600140919     c                   else
333700140919     c                   eval      orincono74 = ORSTINIO74
333800140919     c                   eval      orficono74 = ORSTFINO74
333900140919     c                   endif
334000140604     c                   ENDSR
334100010717     c*--------------------------------------------------------------------------------------------*
334200010717     c* imposta i campi della DS di Output dagli eventi
334300010717     c*--------------------------------------------------------------------------------------------*
334400010717     c     RepAbilFirma  BEGSR
334500010717     c*
334600010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
334700010717     c* e recupera utente / password
334800010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
334900010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
335000010717     c     keyvtd        SETLL     tivtd01l
335100010717     c     keyvtd        READE     tivtd01l                               98
335200010717do  2c                   DOW       NOT *in98
335300010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
335400010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
335500010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
335600010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
335700010717     c                   EVAL      depute = vtdute                              *utente
335800010717     c                   EVAL      deppwd = vtdpwd                              *password
335900010717     c                   LEAVE                                                  *esce dal ciclo
336000010717e   3c                   ENDIF
336100010717     c     keyvtd        READE     tivtd01l                               98
336200010717e   2c                   ENDDO
336300010717e   1C                   ENDIF
336400010717     c*
336500010717     c                   ENDSR
336600000000     c*--------------------------------------------------------------------------------------------*
336700000000     c* edita data da aaaammgg in gg/mm/aaaa
336800000000     c*--------------------------------------------------------------------------------------------*
336900000000     c     edtdat        BEGSR
337000000000     c*
337100000000     c                   MOVEL     *blanks       wdate
337200000000     c*
337300000000     c                   MOVEL     wdat          dsdat
337400000000     c                   MOVEL     dsgio         dsgioe
337500140528     c                   MOVEL     '.'           dsvd1
337600000000     c                   MOVEL     dsmes         dsmese
337700140528     c                   MOVEL     '.'           dsvd2
337800000000     c                   MOVEL     dsann         dsanne
337900000000     c                   MOVEL     dsdate        wdate
338000000000     c*
338100000000     c                   ENDSR
338200000000     c*--------------------------------------------------------------------------------------------*
338300000000     c* edita ora da hhmm in hh:mm
338400000000     c*--------------------------------------------------------------------------------------------*
338500000000     c     edtora        BEGSR
338600000000     c*
338700000000     c                   MOVEL     *blanks       worae
338800000000     c*
338900000000     c                   MOVEL     wora          dsora
339000000000     c                   MOVEL     dshh          dshhe
339100140617     c                   MOVEL     '.'           dsvo1
339200000000     c                   MOVEL     dsmm          dsmme
339300000000     c                   MOVEL     dsorae        worae
339400000000     c*
339500000000     c                   ENDSR
339600000000     c*--------------------------------------------------------------------------------------------*
339700000000     c* decodifica punto operativo
339800000000     c*--------------------------------------------------------------------------------------------*
339900000000     c     decorg        BEGSR
340000000000     c*
340100000000     c                   MOVEL     *blanks       wodes                          *descrizione
340200000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
340300000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
340400000000     c*
340500000000     c                   SETOFF                                           98
340600041111     c                   Z-ADD     1             O
340700041111     c     worg          LOOKUP    sorg(O)                                98
340800000000if  1c                   IF        *in98                                        *trovata
340900121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
341000121001     c                             %EDITC(wOrg : 'X') + ')'
341100041111     c                   MOVEL     sofl1(O)      wofl1
341200041111     c                   MOVEL     sodpd(O)      wodpd
341300000000e   1c                   ENDIF
341400000000     c*
341500000000     c                   ENDSR
341600000000     c*--------------------------------------------------------------------------------------------*
341700000000     c* decodifica evento per STATI
341800000000     c*--------------------------------------------------------------------------------------------*
341900000000     c     deccevsta     BEGSR
342000000000     c*
342100000000     c                   MOVEL     '0'           werr                           *NO errore
342200050704     c*
342300050704     C                   IF        wcev = 'GEN'
342400050704     C                   EVAL      werr = *ON
342500050704     C                   CLEAR                   wcdex
342600050704     C                   LEAVESR
342700050704     C                   ENDIF
342800000000     c*
342900060622     C                   RESET                   tibs02ds
343000060622     C                   EVAL      t02Cod = 'ICE'
343100060622     C                   EVAL      t02Ke1 = wcev
343200060622     C                   EXSR      getTntbe00f
343300060622     C                   IF        t02Err <> 'E' AND
343400140528     C                             (§icesta = 'S'  OR
343500140528     C                               §icestaP = 'S' )
343600060622     c                   MOVEL     §icedei       wcdex
343700000000x   1c                   ELSE                                                   *non trovato
343800000000     c                   MOVEL     '1'           werr                           *SI errore
343900030130     c                   MOVEL     *blanks       wcdex
344000000000e   1c                   ENDIF
344100000000     c*
344200000000     c                   ENDSR
344300000000     c*--------------------------------------------------------------------------------------------*
344400000000     c* decodifica consegna particolare
344500000000     c*--------------------------------------------------------------------------------------------*
344600000000     c     decftc        BEGSR
344700000000     c*
344800000000     c                   MOVEL     *blanks       wdftc
344900000000     c*
345000000000if  1c                   IF        wftc = 'S'
345100060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
345200000000e   1c                   ENDIF
345300000000if  1c                   IF        wftc = 'P'
345400060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
345500000000e   1c                   ENDIF
345600000000if  1c                   IF        wftc = 'A'
345700060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
345800000000e   1c                   ENDIF
345900000000if  1c                   IF        wftc = 'F'
346000060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
346100000000e   1c                   ENDIF
346200000000     c*
346300000000     c                   ENDSR
346400000000     c*--------------------------------------------------------------------------------------------*
346500000000     c* decodifica turno di chiusura
346600000000     c*--------------------------------------------------------------------------------------------*
346700000000     c     decgcx        BEGSR
346800000000     c*
346900000000     c                   MOVEL     *blanks       wdgcx
347000000000     c* giorno
347100000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
347200000000if  1c                   IF        a1 = '1'
347300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
347400000000e   1c                   ENDIF
347500000000if  1c                   IF        a1 = '2'
347600060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
347700000000e   1c                   ENDIF
347800000000if  1c                   IF        a1 = '3'
347900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
348000000000e   1c                   ENDIF
348100000000if  1c                   IF        a1 = '4'
348200060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
348300000000e   1c                   ENDIF
348400000000if  1c                   IF        a1 = '5'
348500060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
348600000000e   1c                   ENDIF
348700000000if  1c                   IF        a1 = '6'
348800060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
348900000000e   1c                   ENDIF
349000000000if  1c                   IF        a1 = '7'
349100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
349200000000e   1c                   ENDIF
349300000000if  1c                   IF        a1 = ' '
349400000000     c                   EVAL      wdgcx = *blanks
349500000000e   1c                   ENDIF
349600000000     c* am/pm
349700000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
349800000000if  1c                   IF        a1 = ' '
349900060612     c                   EVAL      wdgcx =
350000060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
350100000000e   1c                   ENDIF
350200000000if  1c                   IF        a1 = 'M'
350300060612     c                   EVAL      wdgcx =
350400060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
350500000000e   1c                   ENDIF
350600000000if  1c                   IF        a1 = 'P'
350700060612     c                   EVAL      wdgcx =
350800060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
350900000000e   1c                   ENDIF
351000000000     c*
351100000000     c                   ENDSR
351200000000     c*--------------------------------------------------------------------------------------------*
351300000000     c* decodifica evento per GIACENZE
351400000000     c*--------------------------------------------------------------------------------------------*
351500000000     c     deccevgia     BEGSR
351600000000     c*
351700000000     c                   MOVEL     '0'           werr                           *NO errore
351800000000     c*
351900060622     C                   RESET                   tibs02ds
352000060622     C                   EVAL      t02Cod = 'ICE'
352100060622     C                   EVAL      t02Ke1 = wcev
352200060622     C                   EXSR      getTntbe00f
352300060622     C                   IF        t02Err <> 'E' AND                            *trovata
352400140528     C                             (§icegia = 'S'  OR
352500140528     C                               §icegiap = 'S')
352600060622     c                   MOVEL     §icedei       wcdex
352700000000x   1c                   ELSE                                                   *non trovato
352800000000     c                   MOVEL     '1'           werr                           *SI errore
352900030130     c                   MOVEL     *blanks       wcdex
353000000000e   1c                   ENDIF
353100000000     c*
353200000000     c                   ENDSR
353300000000     c*--------------------------------------------------------------------------------------------*
353400000000     c* Controlla la DS dsi Input
353500000000     c*--------------------------------------------------------------------------------------------*
353600000000     c     chkdsinput    BEGSR
353700000000     c*
353800000000     c                   MOVEL     '0'           werr                           *NO errore
353900000000     c*
354000000000     c* dalla DS di Input scompone il codice spedizione
354100000000     c                   MOVEL     nspedizi74    dsspedizi74
354200000000     c*
354300000000     c* controlla che la lina di partenza sia TUTTA numerica
354400000000     c                   SETOFF                                       99
354500000000     c                   TESTN                   dslnpi74             99
354600000000if  1c                   IF        NOT *in99                                    *non è un numero
354700000000     c                   MOVEL     '1'           werr                           *SI errore
354800140617     c                   eval      widmsg = 'TIS0769'
354900140617     c                   eval      widcampo = '*LNP'
355000140617     c                   clear                   wparm
355100140617     c                   exsr      messaggi
355200000000x   1c                   ELSE                                                   *è un numero
355300000000     c                   MOVE      dslnpi74      a1
355400000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
355500000000     c                   MOVEL     '1'           werr                           *SI errore
355600140617     c                   eval      widmsg = 'TIS0769'
355700140617     c                   eval      widcampo = '*LNP'
355800140617     c                   clear                   wparm
355900140617     c                   exsr      messaggi
356000000000e   2c                   ENDIF
356100000000e   1c                   ENDIF
356200000000     c*
356300000000     c* controlla che la serie spedizione sia TUTTA numerica
356400000000     c                   SETOFF                                       99
356500000000     c                   TESTN                   dsnrsi74             99
356600000000if  1c                   IF        NOT *in99                                    *non è un numero
356700000000     c                   MOVEL     '1'           werr                           *SI errore
356800140617     c                   eval      widmsg = 'TIS0769'
356900140617     c                   eval      widcampo = '*NRS'
357000140617     c                   clear                   wparm
357100140617     c                   exsr      messaggi
357200000000x   1c                   ELSE                                                   *è un numero
357300000000     c                   MOVE      dsnrsi74      a1
357400000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
357500000000     c                   MOVEL     '1'           werr                           *SI errore
357600140617     c                   eval      widmsg = 'TIS0769'
357700140617     c                   eval      widcampo = '*NRS'
357800140617     c                   clear                   wparm
357900140617     c                   exsr      messaggi
358000000000e   2c                   ENDIF
358100000000e   1c                   ENDIF
358200000000     c*
358300000000     c* controlla che il numero spedizione sia TUTTO numerico
358400000000     c                   SETOFF                                       99
358500000000     c                   TESTN                   dsnspi74             99
358600000000if  1c                   IF        NOT *in99                                    *non è un numero
358700000000     c                   MOVEL     '1'           werr                           *SI errore
358800140617     c                   eval      widmsg = 'TIS0769'
358900140617     c                   eval      widcampo = '*NSP'
359000140617     c                   clear                   wparm
359100140617     c                   exsr      messaggi
359200000000x   1c                   ELSE                                                   *è un numero
359300000000     c                   MOVE      dsnspi74      a1
359400000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
359500000000     c                   MOVEL     '1'           werr                           *SI errore
359600140617     c                   eval      widmsg = 'TIS0769'
359700140617     c                   eval      widcampo = '*NSP'
359800140617     c                   clear                   wparm
359900140617     c                   exsr      messaggi
360000000000e   2c                   ENDIF
360100000000e   1c                   ENDIF
360200110104     c*
360300110104     c* controlla che il codice cliente sia TUTTO numerico
360400110104     c                   IF        kscI74 <> *BLANK
360500110104     c                   SETOFF                                       99
360600110104     c                   TESTN                   kscI74               99
360700110104     c                   IF        NOT *in99                                    *non è un numero
360800110104     c                   MOVEL     '1'           werr                           *SI errore
360900140617     c                   eval      widmsg = 'TIS0769'
361000140617     c                   eval      widcampo = 'KSCI74'
361100140617     c                   clear                   wparm
361200140617     c                   exsr      messaggi
361300110104     c                   ELSE                                                   *è un numero
361400110104     c                   MOVE      kscI74        a1
361500110104     c                   IF        a1 < '0'                                     *trovata una lettera
361600110104     c                   MOVEL     '1'           werr                           *SI errore
361700140617     c                   eval      widmsg = 'TIS0769'
361800140617     c                   eval      widcampo = 'KSCI74'
361900140617     c                   clear                   wparm
362000140617     c                   exsr      messaggi
362100110104     c                   ENDIF
362200110104     c                   ENDIF
362300110104     c                   ENDIF
362400110104     c*
362500110104     c* controlla che il codice mittente sia TUTTO numerico
362600110104     c                   IF        ccmI74 <> *BLANK
362700110104     c                   SETOFF                                       99
362800110104     c                   TESTN                   ccmI74               99
362900110104     c                   IF        NOT *in99                                    *non è un numero
363000110104     c                   MOVEL     '1'           werr                           *SI errore
363100140617     c                   eval      widmsg = 'TIS0769'
363200140617     c                   eval      widcampo = 'CCMI74'
363300140617     c                   clear                   wparm
363400140617     c                   exsr      messaggi
363500110104     c                   ELSE                                                   *è un numero
363600110104     c                   MOVE      ccmI74        a1
363700110104     c                   IF        a1 < '0'                                     *trovata una lettera
363800110104     c                   MOVEL     '1'           werr                           *SI errore
363900140617     c                   eval      widmsg = 'TIS0769'
364000140617     c                   eval      widcampo = 'CCMI74'
364100140617     c                   clear                   wparm
364200140617     c                   exsr      messaggi
364300110104     c                   ENDIF
364400110104     c                   ENDIF
364500110104     c                   ENDIF
364600050104     c*
364700050104     c* controlla anno spedizione
364800050104     c                   TESTN                   cAASI74              99
364900050104if  1c                   IF        NOT *in99                                    *non è un numero
365000050104     c                   EVAL      cAASI74 = *ZERO
365100050104x   1c                   ENDIF
365200100222     c*
365300100223     c* Estraggo l'anno dalla data spedizione.
365400100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
365500100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
365600100223     C                   IF        dspI74 <> *BLANK
365700100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
365800100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
365900100223     C                   MONITOR
366000100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
366100100223     C                   ON-ERROR
366200100223     C                   ENDMON
366300100222     C                   ENDIF
366400000000     c*
366500000000     c* se errore, esce dal programma
366600140610if  1c                   IF        werr = '1'                                   *SI errore
366700140610     c                   eval      esito = werr                                 *errore
366800140610e   1c                   ENDIF
366900000000     c*
367000000000     c                   ENDSR
367100000000     c*--------------------------------------------------------------------------------------------*
367200000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
367300000000     c* Input:   Walfa7 = Alfanumerico di 7
367400000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
367500000000     c*--------------------------------------------------------------------------------------------*
367600000000     c     xallinea1     BEGSR
367700000000     c*
367800000000     c* reimposta variabli di lavoro
367900000000     c                   CLEAR                   i
368000000000     c                   CLEAR                   z
368100000000     c                   CLEAR                   walfa7bis
368200000000     c                   EVAL      wzeri = *ALL'0'
368300000000     c*
368400000000     c     ' '           CHECKR    walfa7:7      i
368500000000if  1c                   IF        i <> 0
368600000000     c     i             SUBST     walfa7        walfa7bis
368700000000e   1c                   ENDIF
368800000000if  1c                   IF        walfa7bis <> *blanks
368900000000     c     7             SUB       i             z
369000000000if  2c                   IF        z <> 0
369100000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
369200000000e   2c                   ENDIF
369300000000     c                   EVAL      z = z+ 1
369400000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
369500000000e   1c                   ENDIF
369600000000     c*
369700000000     c                   ENDSR
369800000000     c*--------------------------------------------------------------------------------------------*
369900000000     c* caricamento tabelle occorrenti
370000000000     c*--------------------------------------------------------------------------------------------*
370100000000     c     cartab        BEGSR
370200000000     c*---
370300000000     c* organigrama
370400000000     c*---
370500000000     c                   CLEAR                   i                              *indice
370600000000     C                   CLEAR                   sorg
370700000000     C                   CLEAR                   sodes
370800000000     C                   CLEAR                   sofl1
370900000000     C                   CLEAR                   sopt
371000000000     C                   CLEAR                   sodpd
371100060623     C                   OPEN      tntbe01l
371200000000     c     *loval        SETLL     azorg01l
371300000000     c                   READ      azorg01l                               99
371400000000do  1c                   DOW       NOT *in99
371500000000     c                   ADD       1             i
371600000000     c                   Z-ADD     orgfil        sorg(i)
371700000000     c                   MOVEL     orgdes        sodes(i)
371800000000     c                   MOVEL     orgde3        og143
371900020702     C*
372000020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
372100020702     C                   CLEAR                   dntw
372200020702     C*
372300020702     C                   MOVEL(P)  'NTW'         kbecod
372400020702     C                   MOVEL(P)  §ogntw        kbeke1
372500020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
372600020702     c                   MOVEL     *blanks       kbelin                         *lingua
372700020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
372800020702     C     keytbe        CHAIN     tntbe01l
372900020702     C                   IF        %found(tntbe01l)
373000020702     C                   MOVEL     tbeuni        dntw
373100020702     C                   ENDIF
373200020702     C*
373300020702     C                   MOVEL     §ntwfie       sofl1(i)
373400020702     c                   MOVEL     §ogntw        sopt(i)
373500020702     c                   MOVEL     §ogntw        sodpd(i)
373600000000     c                   READ      azorg01l                               99
373700000000e   1c                   ENDDO
373800060623     C                   CLOSE     tntbe01l
373900000000     c*---
374000000000     c                   ENDSR
374100000000     c*--------------------------------------------------------------------------------------------*
374200000000     c* operazioni iniziali -da fare sempre-
374300000000     c*--------------------------------------------------------------------------------------------*
374400000000     c     rutinz        BEGSR
374500000000     c*
374600000000     c* reperimento data corrente
374700000000     C                   TIME                    n14                            *ora (6)+ data (8)
374800000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
374900000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
375000000000     C                   Z-ADD     n8            g08dat
375100000000     C                   Z-ADD     *zeros        g08inv
375200000000     C                   MOVEL     '0'           g08err
375300000000     c                   CALL      'XSRDA8'
375400000000     c                   PARM                    wlbda8
375500000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
375600000000     C                   MOVEL     datcor        anncor                         *anno corrente
375700000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
375800000000     c*
375900000000     c* calcola data corrente - 1 anno
376000000000     c                   Z-ADD     datcor        datpre                         *data precedente
376100000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
376200000000     c*
376300010716     c* azzera la DS di Output e le variabili di lavoro / controllo
376400000000     c                   MOVEL     '0'           esito
376500140603     c                   reset                   tis778dso                      *ds Output
376600140603     c                   reset                   tis778dsm                      *ds Output
376700000000     c                   CLEAR                   smi                            *di memorizzazione
376800000000     c                   CLEAR                   smseql
376900000000     c                   CLEAR                   smseqe
377000000000     c                   CLEAR                   smdevhev
377100000000     c                   CLEAR                   smdeve
377200000000     c                   CLEAR                   smheve
377300000000     c                   CLEAR                   smfle
377400000000     c                   CLEAR                   smdev
377500000000     c                   CLEAR                   smcev
377600130312     c                   CLEAR                   smspe
377700000000     c                   CLEAR                   smdmc
377800000000     c                   CLEAR                   smdmcR
377900000000     c                   CLEAR                   tote                           *contatore eventi
378000000000     c                   CLEAR                   seql                           *sequenza legami
378100010716     c                   RESET                   $AbTD                          *di lavoro
378200010716     c                   RESET                   $AbFirma
378300140603     c* parametri ingresso
378400140603     c                   exsr      Kontrolla
378500000000     c* controlla da DS in Input
378600000000     c                   EXSR      chkdsinput
378700000000     c*
378800000000     c* carica i clienti dettaglio del cliente unificante
378900000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
379000000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
379100000000     c     keyvss        SETLL     tivss02l
379200000000     c     keyvss        READE     tivss02l                               99
379300000000do  1c                   DOW       NOT *in99
379400000000if  2c                   IF        vssvat = *blanks                             *attivo
379500000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
379600000000     c                             datcor <= vssdsc
379700010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
379800010719     c* --> TD per vedere la firma DPD
379900010716     c                   RESET                   $AbTD                          *di lavoro
380000010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
380100010719     c     kssksu        SETLL     tivss02l
380200010719     c     kssksu        READE     tivss02l                               99
380300010719do  4c                   DOW       NOT *in99
380400010716if  5c                   IF        vssvat = *blanks                             *attivo
380500010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
380600010716     c                             datcor <= vssdsc
380700010719if  7c                   IF        vssisv = 'TD'
380800010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
380900010719e   7c                   ENDIF
381000010719e   6c                   ENDIF
381100010716e   5c                   ENDIF
381200010719     c     kssksu        READE     tivss02l                               99
381300010719e   4c                   ENDDO
381400010716     c*
381500000000     c                   LEAVE                                                  *uscita ciclo
381600000000e   3c                   ENDIF
381700000000e   2c                   ENDIF
381800000000     c     keyvss        READE     tivss02l                               99
381900010716e   1c                   ENDDO                                                  *fine cl.abil.TT
382000000000     c*
382100060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
382200060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
382300060621     c*
382400000000     c                   ENDSR
382500040330
382600040330     ***********************************************************************************************
382700040330     **?
382800040330     **?Reperimento ulteriori dati destinatario.
382900040330     **?
383000040330     ***********************************************************************************************
383100040330     C     ImpDSDest     BEGSR
383200040330
383300140716     C                   IF        ar5Nsp <> f_tasnsp OR ar5Lnp <> f_taslnp OR
383400140716     C                             ar5Nrs <> f_tasnrs OR ar5Aas <> f_tasaas OR
383500100121     C                             ar5Trd <> 'GEN'
383600140716     c                   Z-ADD     f_tasaas      kr5aas
383700140716     c                   Z-ADD     f_taslnp      kr5lnp
383800140716     c                   Z-ADD     f_tasnrs      kr5nrs
383900140716     c                   Z-ADD     f_tasnsp      kr5nsp
384000040330     c                   MOVEL     'GEN'         kr5trd
384100040531     c     keyar5        CHAIN     fiar531c
384200040330     C                   IF        %FOUND
384300040330     C                   EVAL      DAR5GEN = AR5Uni
384400100121     C                   ELSE
384500100121     C                   RESET                   DAR5GEN
384600040330     C                   ENDIF
384700100121     C                   ENDIF
384800100121     C
384900100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
385000100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
385100040330
385200040330     C                   ENDSR
385300040913
385400040913     ***********************************************************************************************
385500040913     **?
385600040913     **?Determino se addebitare l'immagine LDV.
385700040913     **?
385800040913     ***********************************************************************************************
385900040913     C     AddebitoLDV   BEGSR
386000040913
386100040913     C                   IF        LDVO74 = 'S'
386200040913
386300040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
386400040913
386500040913     C                   ENDIF
386600040913
386700040913     C                   ENDSR
386800051110
386900051110     ***********************************************************************************************
387000051110     **?
387100051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
387200051110     **?
387300051110     ***********************************************************************************************
387400051110     C     chkTivpi      BEGSR
387500051110
387600051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
387700051110     C                             esito10)
387800051110     C                   IF        esito10 > 0 AND NOT %ERROR
387900051110     C                   EVAL      gcfdio74 = 'N'
388000051110     C                   ENDIF
388100051110
388200051110     C                   ENDSR
388300051222
388400051222     ***********************************************************************************************
388500051222     **
388600051222     ** Linea arrivo Italia o estera.
388700051222     **
388800051222     ***********************************************************************************************
388900051222     C     setLnaItaEst  BEGSR
389000051222
389100051222     C                   RESET                   wrkLnaItaEst
389200051222     C                   EXSR      decOrg
389300051222     C                   EVAL      wrkLnaItaEst = wofl1
389400051222
389500051222     C                   ENDSR
389600060622
389700060622     ***********************************************************************************************
389800060622     **
389900130312     ** Reperisco informazioni stato bolla in filiale se ultimo evento
390000130312     ** è un MIC
390100060622     **
390200060622     ***********************************************************************************************
390300130312     C     srarb         BEGSR
390400130312     c                   clear                   wcev
390500140714     c                   clear                   wricons
390600130312     c                   clear                   worg
390700130312     c                   clear                   wdat
390800130312     c                   clear                   wora
390900130312     c                   eval      dsevbspe = spe(1)
391000130312     c     karb          chain     fnarb01l
391100130312     c                   if        not %error and
391200130312     c                             %found(fnarb01l)
391300130422     c                   Z-ADD     arblna        worg
391400130312     c                   exsr      setLnaItaEst
391500130312     c* no per estero
391600130312     C                   IF        wrkLnaItaEst = ESTERO
391700130312     c                   leavesr
391800130312     c                   end
391900130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
392000130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
392100130312     C                   SELECT
392200141120     c                   WHEN      arbdcm <> 0
392300130312     c                   MOVEL     '704'         wcev                           *causale evento
392400130313     c                   move      arbdcm        WDAT
392500130313     c                   move      arbhmc        WORA
392600130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
392700130312     c                   WHEN      ARBFDC = ' '
392800130326     c* solo se distinta NON in test
392900130326     c                   clear                   ddstflr
393000130326     c                   z-add     arbifp        dstfgs
393100130326     c                   z-add     arbndc        dstnfv
393200130326     c     kdst          chain     fidst01l
393300130326     c                   if        not %error and
393400130326     c                             %found(fidst01l)
393500130326     c                   eval      ddstflr = dstflr
393600130326     c                   end
393700130327     c                   if        §DSTTSTPDA = 'C' or
393800130327     c                             §DSTTSTPDA = 'E'
393900130326     c                   leavesr
394000130326     c                   end
394100130326     c*
394200130313     c                   move      arbndc        pctndc
394300131105     c                   exsr      cercaceP
394400131105     c                   move      'CET'         ttrd
394500130312     c     kcet          setgt     fipct02l
394600130312     c     kcet          readpe    fipct02l
394700130312     c                   if        not %error and
394800130312     c                             not %eof(fipct02l)
394900130312     c                   movel     pctdati       fipctcetds
395000140926      * firmatario se c'è riempie il campo
395100140926     c                   if        §pctnomfir <> *blank
395200140926     c                   eval      firmato74  = §pctnomfir
395300140926     c                   endif
395400130312     c* consegnata
395500130312     c                   if        §pctcmc = ' '
395600130312     c                   MOVEL     '704'         wcev                           *causale evento
395700130312     c                   else
395800140711     c                   move      'S'           wricons
395900130312     c* mancata consegna
396000130313     c                   movel     §PCTcmc       wcev
396100130312     c                   clear                   ds2a
396200130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
396300130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
396400130312     C                             :rpyEsito)
396500130312     c                   if        §2aflg = '§' and §2acpt = 'S'
396600130312     c                   move      'T  '         wcev
396700130312     c                   endif
396800130312     c                   endif
396900130312     c* ??? £6
397000130312     c                   if        §pctdata > 0
397100130313     c                   move      §PCTDATA      WDAT
397200130312     c                   movel     §PCTORA       WORA
397300130312     c                   endif
397400130312     c                   end
397500130312     c                   endSL
397600130312     c                   end
397700130312     c* ho trovato un evento da inserire  al 1° posto
397800130312     c                   if        wcev <> ' '
397900130312     c                   EXSR      deccevsta
398000130312     c                   IF        werr <> '1'                                  *no errore
398100130312     c                   EXSR      decorg
398200130312     c                   EXSR      edtdat
398300130312     c                   EXSR      edtora
398400130313     c* libero il 1° elemnto di schiera
398500130313     c                   eval      j = 50
398600130313     c                   for       j = 50 downto 1
398700130312     c                   if        cev(j) <> ' '
398800130312     c                   eval      cev(j+1) = cev(j)
398900130312     c                   eval      dev(j+1) = dev(j)
399000130312     c                   eval      hev(j+1) = hev(j)
399100130312     c                   eval      fle(j+1) = fle(j)
399200130312     c                   eval      eve(j+1) = eve(j)
399300130312     c                   end
399400130313     c                   endfor
399500130313     c                   eval      dev(1) = wdate
399600130313     c                   eval      hev(1) = worae
399700130312     c                   eval      cev(1)= wcev
399800130313     c                   eval      fle(1) = wodes
399900130313     c                   eval      eve(1) = wcdex
400000130313     c                   EVAL      cntEveO74 += 1
400100130312     c                   end
400200130312     c                   end
400300130312     C                   ENDSR
400400140714     **_________________________________________________________
400500140714     c     cercafiglia   begsr
400600140714     **_________________________________________________________
400700140715     c                   clear                   keyForzata
400800140714      * salva chiave originale per eventi
400900140715     c                   eval      kasaasf = kasaas
401000140715     c                   eval      kaslnpf = kaslnp
401100140715     c                   eval      kasnrsf = kasnrs
401200140715     c                   eval      kasnspf = kasnsp
401300140714     c                   do        *hival
401400140715     c     keytasf       chain     fnlbl02l
401500140714     c                   if        %found(fnlbl02l)
401600140715     c                   eval      kasaasf = LBLaan
401700140715     c                   eval      kaslnpf = LBLlpn
401800140715     c                   eval      kasnrsf = LBLnrn
401900140715     c                   eval      kasnspf = LBLnsn
402000140714     c                   move      'X'           keyForzata
402100140714     c                   iter
402200140714     c                   else
402300140714     c                   leave
402400140714     c                   endif
402500140714     c                   enddo
402600140715     c     keytasf       setll     titas30c                               99    *no errore
402700140715     c     keytasf       READE     titas30c                               99    *no errore
402800141117      *se non trova titas con figlia forza di nuovo la mamma
402900140715if  3c                   IF        *in99                                        *non trovato
403000141117     c                   clear                   titasdssavf
403100140715x   3c                   ELSE                                                   *trovata
403200140715if  4c                   IF        *in01                                        *lettura titas000
403300140715     c                   EVAL      titasdssavf = titasds
403400140715e   4c                   ENDIF
403500140715if  4c                   IF        *in02                                        *lettura titas010
403600140715     c                   EVAL      titasdssavf = titasds
403700140715e   4c                   ENDIF
403800140715if  4c                   IF        *in03                                        *lettura titasp00
403900140715     c                   EVAL      titasdssavf = titasds
404000140715e   4c                   ENDIF
404100140715e   3c                   ENDIF
404200140714     C                   ENDSR
404300131105     **_________________________________________________________
404400131105     c     cercaCEP      begsr
404500131105     **_________________________________________________________
404600131105     c                   move      'CEP'         ttrd
404700131105     c     kcet          setgt     fipct02l
404800131105     c     kcet          readpe    fipct02l
404900131105     c                   if        not %error and
405000131105     c                             not %eof(fipct02l)
405100131105     c                   movel     pctdati       fipctcetds
405200131105     c* consegnata
405300131105     c                   if        §pctcmc = ' '
405400131105     c                   MOVEL     '704'         wcev                           *causale evento
405500131105     c                   else
405600131105     c* mancata consegna
405700131105     c                   movel     §PCTcmc       wcev
405800131105     c                   clear                   ds2a
405900131105     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
406000131105     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
406100131105     C                             :rpyEsito)
406200131105     c                   if        §2aflg = '§' and §2acpt = 'S'
406300131105     c                   move      'T  '         wcev
406400131105     c                   endif
406500131105     c                   endif
406600131105     c* ??? £6
406700131105     c                   if        §pctdata > 0
406800131105     c                   move      §PCTDATA      WDAT
406900131105     c                   movel     §PCTORA       WORA
407000131105     c                   endif
407100131105     c                   end
407200131105     c* ho trovato un evento da inserire  al 1° posto
407300131105     c                   if        wcev <> ' '
407400131105     c                   EXSR      deccevsta
407500131105     c                   IF        werr <> '1'                                  *no errore
407600131105     c                   EXSR      decorg
407700131105     c                   EXSR      edtdat
407800131105     c                   EXSR      edtora
407900131105     c* libero il 1° elemnto di schiera
408000131105     c                   eval      j = 50
408100131105     c                   for       j = 50 downto 1
408200131105     c                   if        cev(j) <> ' '
408300131105     c                   eval      cev(j+1) = cev(j)
408400131105     c                   eval      dev(j+1) = dev(j)
408500131105     c                   eval      hev(j+1) = hev(j)
408600131105     c                   eval      fle(j+1) = fle(j)
408700131105     c                   eval      eve(j+1) = eve(j)
408800131105     c                   end
408900131105     c                   endfor
409000131105     c                   eval      dev(1) = wdate
409100131105     c                   eval      hev(1) = worae
409200131105     c                   eval      cev(1)= wcev
409300131105     c                   eval      fle(1) = wodes
409400131105     c                   eval      eve(1) = wcdex
409500131105     c                   EVAL      cntEveO74 += 1
409600131105     c                   end
409700131105     c                   end
409800131105     c                   endsr
409900131105     ***********************************************************************************************
410000130312     **
410100130312     ** Reperisco tabella da TNTBE00F.
410200130312     **
410300130312     ***********************************************************************************************
410400130312     C     getTntbe00f   BEGSR
410500060622     C                   EVAL      t02Lin = langTntbe
410600060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
410700060622     C                   SELECT
410800060622     C                   WHEN      t02Cod = 'ICE'
410900060622     C                   EVAL      dice = t02Uni
411000060622     C                   WHEN      t02Cod = 'CCH'
411100060622     C                   EVAL      dcch = t02Uni
411200060622     C                   WHEN      t02Cod = 'NTW'
411300060622     C                   EVAL      dntw = t02Uni
411400100119     C                   WHEN      t02Cod = 'I1A'
411500100119     C                   EVAL      dI1a = t02Uni
411600060622     C                   ENDSL
411700060622     C                   ENDSR
411800060608
411900140529      /free
412000140529       //--------------------------------------------------------------
412100140529       //?Controllo formale dei dati passati.
412200140529       //--------------------------------------------------------------
412300140603       BEGSR  Kontrolla;
412400140529
412500140529       //?OpCode
412600140529         IF  prmRqsOpCode <> TIS778_RQSOPCODE_GETBOLLA ;
412700140529           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
412800140529           prmRpyIdMsg  = TIS778_ESITO_RQSOPCODE_SCONOSCIUTO ;
412900140529           exsr RoutEnd;
413000140529         ENDIF;
413100140603           IF  prmRqsDataSize > 0;
413200140603           ELSE;
413300140603             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
413400140603             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
413500140603             exsr RoutEnd;
413600140603           ENDIF;
413700140529
413800140529       //?Formato e size RQS
413900140603           IF  prmRqsFormato = formatoi74;
414000140529             tis778dsi = %SUBST(prmRqsData:1:prmRqsDataSize);
414100140529           ELSE;
414200140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR;
414300140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
414400140529             exsr RoutEnd;
414500140529           ENDIF;
414600140529         //?Formato e size RPY
414700140603           IF  prmRpyFormato = formatoo74;
414800140529           ELSE;
414900140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
415000140529             prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
415100140529             exsr RoutEnd;
415200140529           ENDIF;
415300140529           IF  prmRpyDataSize > 0;
415400140529           ELSE;
415500140529             prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
415600140529             prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
415700140529             exsr RoutEnd;
415800140529           ENDIF;
415900140616       //?Formato e size MSG
416000140617         if %parms > 9 ;
416100140617          IF  prmRpyFormMsg = formato;
416200140617          ELSE;
416300140616           prmRpyOpCode  = TIS778_RPYOPCODE_ERROR;
416400140616           prmRpyIdMsg = TIS778_ESITO_NOME_FORMATO_SCONOSCIUTO ;
416500140616           exsr RoutEnd;
416600140617          ENDIF;
416700140617          IF  prmRpyMsgSize > 0;
416800140617          ELSE;
416900140616           prmRpyOpCode = TIS778_RPYOPCODE_ERROR ;
417000140616           prmRpyIdMsg = TIS778_ESITO_SIZE_DATA_ERRATO ;
417100140616           exsr RoutEnd;
417200140617          ENDIF;
417300140617         ENDIF;
417400140529
417500140529       ENDSR;
417600140529
417700140616       //--------------------------------------------------------------
417800140617       //?Routine per schierare i messaggi nella ds tis778dsm
417900140616       //--------------------------------------------------------------
418000140616       BEGSR  Messaggi;
418100140616
418200140616       //?Se ho già caricato 99 messaggi esco
418300140616         IF  nrmsg = 99;
418400140616           exsr RoutEnd;
418500140616           clear nrmsg;
418600140616         ENDIF;
418700140616
418800140616         nrmsg += 1;
418900140616         skIdMsg(nrmsg) = wIdMsg;
419000140616         skIdCampo(nrmsg) = wIdCampo;
419100140616         skErrWarn(nrmsg) = tis778_MSG_ERRORE;
419200140616         IF  wParm <> *blanks;
419300140616           skTextMsg (nrmsg) =
419400140616           rtvMsgLang(wIdMsg:langi74:wParm);
419500140616         ELSE;
419600140616           skTextMsg (nrmsg)=
419700140616           rtvMsgLang(wIdMsg:langi74);
419800140616         ENDIF;
419900140616
420000140616       ENDSR;
420100140616
420200140529       //--------------------------------------------------------------
420300140529       BEGSR  RoutEnd;
420400140530
420500140530
420600140530        if  esito <> '0' ;
420700140603           prmrpyOpcode = tis778_RPYOPCODE_error ;
420800140530             else;
420900140610           prmrpyOpcode = tis778_RPYOPCODE_Done ;
421000140530        endif ;
421100140529
421200140530       //?ritorno
421300140530         %subst(prmRpyData:1:prmRpyDataSize) = tis778dso;
421400140617         if %parms > 9 ;
421500140617          %subst(prmRpyMessage:1:prmRpyMsgSize) = tis778dsm;
421600140617         endif;
421700140529         return;
421800140529
421900140529       ENDSR;
422000140529      /end-free
422100000000     c*--------------------------------------------------------------------------------------------*
422200000000     c* operazioni iniziali
422300000000     c*--------------------------------------------------------------------------------------------*
422400000000     c     *inzsr        BEGSR
422500000000     c*
422600000000     c* ricevimento parametri
422700140603     c*m   *ENTRY        PLIST
422800140529     c*m                 PARM                    esito
422900140529     c*m                 PARM                    tis174dsi
423000140529     c*m                 PARM                    tis174dso
423100000000     c*
423200000000     c* chiavi di lettura
423300000000     c     keytas        KLIST                                                  titas30c
423400000000     c                   KFLD                    kasaas                         -anno spedizione
423500000000     c                   KFLD                    kaslnp                         -linea partenza
423600000000     c                   KFLD                    kasnrs                         -serie
423700000000     c                   KFLD                    kasnsp                         -spedizione
423800140715     c     keytasf       KLIST                                                  titas30c
423900140715     c                   KFLD                    kasaasf                        -anno spedizione
424000140715     c                   KFLD                    kaslnpf                        -linea partenza
424100140715     c                   KFLD                    kasnrsf                        -serie
424200140715     c                   KFLD                    kasnspf                        -spedizione
424300130312     c     karb          KLIST                                                  titas30c
424400140716     c                   KFLD                    f_tasaas                         -anno spedizione
424500140716     c                   KFLD                    f_taslnp                         -linea partenza
424600140716     c                   KFLD                    f_tasnrs                         -serie
424700140716     c                   KFLD                    f_tasnsp                         -spedizione
424800130326     C     Kdst          KLIST
424900130326     C                   KFLD                    dstnpg
425000130326     C                   KFLD                    dstnfv
425100130326     C                   KFLD                    dstfgs
425200130326     c                   z-add     4             dstnpg
425300130326     C     Kcet          KLIST
425400130326     C                   KFLD                    arbLNA
425500130326     C                   KFLD                    pctNDC
425600130326     C                   KFLD                    arbpdc
425700131105     C                   KFLD                    TTRD              3
425800130312     C                   KFLD                    arbAAS
425900130312     C                   KFLD                    arbLNP
426000130312     C                   KFLD                    arbNRS
426100130312     C                   KFLD                    arbNSP
426200000000     c     keytaa        KLIST                                                  titaa30c
426300000000     c                   KFLD                    kaaaas                         -anno spedizione
426400000000     c                   KFLD                    kaalnp                         -linea partenza
426500000000     c                   KFLD                    kaanrs                         -serie
426600000000     c                   KFLD                    kaansp                         -spedizione
426700000000     c                   KFLD                    kaatrc                         -tipo anagrafica
426800000000     c     keyta4        KLIST                                                  tita430c
426900000000     c                   KFLD                    ka4aas                         -anno spedizione
427000000000     c                   KFLD                    ka4lnp                         -linea partenza
427100000000     c                   KFLD                    ka4nrs                         -serie
427200000000     c                   KFLD                    ka4nsp                         -spedizione
427300000000     c                   KFLD                    ka4trc                         -tipo riferimento
427400010925     c     ke2ta4        KLIST                                                  tita430c
427500010925     c                   KFLD                    ka4aas                         -anno spedizione
427600010925     c                   KFLD                    ka4lnp                         -linea partenza
427700010925     c                   KFLD                    ka4nrs                         -serie
427800010925     c                   KFLD                    ka4nsp                         -spedizione
427900000000     c     keyevb        KLIST                                                  fnevb01l
428000000000     c                   KFLD                    kvbaas                         -anno spedizione
428100000000     c                   KFLD                    kvblnp                         -linea partenza
428200000000     c                   KFLD                    kvbnrs                         -serie
428300000000     c                   KFLD                    kvbnsp                         -spedizione
428400010717     c     ke2evb        KLIST                                                  fnevb01l
428500010717     c                   KFLD                    kvbaas                         -anno spedizione
428600010717     c                   KFLD                    kvblnp                         -linea partenza
428700010717     c                   KFLD                    kvbnrs                         -serie
428800010717     c                   KFLD                    kvbnsp                         -spedizione
428900010717     c                   KFLD                    kvbdev                         -data evento
429000010717     c                   KFLD                    kvbhev                         -ora  evento
429100000000     c     keylbl        KLIST                                                  fnlbl01l
429200000000     c                   KFLD                    kblaan                         -anno seguente
429300000000     c                   KFLD                    kbllpn                         -linea seguente
429400000000     c                   KFLD                    kblnrn                         -serie seguente
429500000000     c                   KFLD                    kblnsn                         -spedizione seguente
429600000000     c     ke2lbl        KLIST                                                  fnlbl02l
429700000000     c                   KFLD                    kblaap                         -anno precedente
429800000000     c                   KFLD                    kbllpp                         -linea precedente
429900000000     c                   KFLD                    kblnrp                         -serie precedente
430000000000     c                   KFLD                    kblnsp                         -spediz.precedente
430100000000     c     keytbe        KLIST                                                  *tntbe01l
430200000000     c                   KFLD                    kbecod                          -tabella
430300000000     c                   KFLD                    kbeke1                          -chiave uno
430400000000     c                   KFLD                    kbeke2                          -chiave due
430500000000     c                   KFLD                    kbelin                          -lingua
430600000000     c                   KFLD                    kbesif                          -s.informativo
430700000000     c     keyvss        KLIST                                                  *tivss02l
430800000000     c                   KFLD                    kssksu                          -cl.unificante
430900000000     c                   KFLD                    kssisv                          -tipo servizio
431000010717     c     keyvtd        KLIST                                                  *tivtd01l
431100010717     c                   KFLD                    ktdksu                          -cl.unificante
431200000000     c     keycsb        KLIST                                                  *tncsb03l
431300000000     c                   KFLD                    ksbaas                         -anno spedizione
431400000000     c                   KFLD                    ksblnp                         -linea partenza
431500000000     c                   KFLD                    ksbnrs                         -serie
431600000000     c                   KFLD                    ksbnsp                         -spedizione
431700040906     c     keydct        KLIST
431800040906     c                   KFLD                    DCTAAC
431900040906     c                   KFLD                    DCTFIL
432000040906     c                   KFLD                    DCTNCA
432100000000     c     keygcp        KLIST                                                  *figcp01l
432200000000     c                   KFLD                    kcpaas                         -anno spedizione
432300000000     c                   KFLD                    kcplnp                         -linea partenza
432400000000     c                   KFLD                    kcpnrs                         -serie
432500000000     c                   KFLD                    kcpnsp                         -spedizione
432600000000     c                   KFLD                    kcpfrg                         -prg aperture
432700050404     c     K04GCP51      KLIST
432800050404     c                   KFLD                    kcpaas                         -anno spedizione
432900050404     c                   KFLD                    kcplnp                         -linea partenza
433000050404     c                   KFLD                    kcpnrs                         -serie
433100050404     c                   KFLD                    kcpnsp                         -spedizione
433200000000     c     keygnp        KLIST                                                  *fignp01l
433300000000     c                   KFLD                    knpagc                         -anno apertura
433400000000     c                   KFLD                    knpfgc                         -filiale apertura
433500000000     c                   KFLD                    knpngc                         -numero giacenza
433600000000     c                   KFLD                    knpfrg                         -prg apertura
433700040531     c     keyar5        KLIST                                                  *fiar531c
433800030206     c                   KFLD                    kr5aas                         -anno spedizione
433900030206     c                   KFLD                    kr5lnp                         -linea partenza
434000030206     c                   KFLD                    kr5nrs                         -serie
434100030206     c                   KFLD                    kr5nsp                         -spedizione
434200030206     c                   KFLD                    kr5trd                         -tipo record
434300040907
434400000000     c* apre i files
434500000000     c                   OPEN      titas30c
434600000000     c                   OPEN      titaa30c
434700000000     c                   OPEN      tita430c
434800000000     c                   OPEN      fnevb01l
434900000000     c                   OPEN      tivss02l
435000010716     c                   OPEN      tivtd01l
435100000000     c                   OPEN      azorg01l
435200000000     c                   OPEN      fnlbl01l
435300000000     c                   OPEN      fnlbl02l
435400050309     c                   OPEN      tigcp51l
435500050309     c                   OPEN      tignp01l
435600000000     c                   OPEN      tncsb03l
435700040906     c                   OPEN      fndct01l
435800040531     c                   OPEN      fiar531c
435900130312     c                   OPEN      fnarb01l
436000140606     c                   OPEN      fiar501l
436100140606     c                   OPEN      fiar401l
436200130326     c                   OPEN      fidst01l
436300130312     c                   OPEN      fipct02l
436400060621     C                   CALLP     openTabel00f()
436500000000     c*
436600000000     c* caricamento tabelle occorrenti
436700000000     c                   EXSR      cartab
436800060621     C                   CALLP     inzLingue()
436900000000     c*
437000000000     c                   ENDSR
437100070109
437200070109     ***********************************************************************************************
437300070109     **
437400070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
437500070109     **
437600070109     ***********************************************************************************************
437700070109     C     eventoAnnulla BEGSR
437800070109
437900070109     C                   IF        §iceEvAn <> *BLANK
438000070109     C                   FOR       j = 1 TO %ELEM(smCev)
438100070109     C                   IF        §iceEvAn = smCev(j) AND
438200070109     C                             evbDevHev = smDevHev(j)
438300070109     C                   CLEAR                   smCev(j)
438400130312     C                   CLEAR                   smspe(j)
438500070109     C                   CLEAR                   smDevHev(j)
438600070109     C                   CLEAR                   smi(j)
438700070109     C                   CLEAR                   smSeql(j)
438800070109     C                   CLEAR                   smSeqe(j)
438900070109     C                   CLEAR                   smDeve(j)
439000070109     C                   CLEAR                   smHeve(j)
439100070109     C                   CLEAR                   smFle(j)
439200070109     C                   CLEAR                   smDev(j)
439300070109     C                   LEAVE
439400070109     C                   ENDIF
439500070109     C                   ENDFOR
439600070109     C                   ENDIF
439700070109
439800070109     C                   ENDSR
439900080131
440000100119     P*--------------------------------------------------
440100100119     P* Procedure name: GetDescrizioneIncassoContrassegno
440200100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
440300100119     P*                          o del contrassegno.
440400100121     P* Returns:        Descrizione modalità incasso contrassegno
440500100119     P*--------------------------------------------------
440600100119     P GetDescrizioneIncassoContrassegno...
440700100119     P                 B
440800100119     D GetDescrizioneIncassoContrassegno...
440900100119     D                 PI            35A
441000100119
441100100121     C                   RESET                   tibs02ds
441200100121     C                   EVAL      t02Cod = 'I1A'
441300100121     C                   EVAL      t02Lin = langTntbe
441400100119     ** Il contrassegno è stato incassato
441500100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
441600100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
441700100121     C                   ELSE
441800100119     ** Il contrassegno non è stato incassato.
441900100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
442000100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
442100100119     C                             ar5Trd <> 'GEN'
442200100121     C                   EVAL      kr5Aas = csbAas
442300100121     C                   EVAL      kr5Lnp = csbLnp
442400100121     C                   EVAL      kr5Nrs = csbNrs
442500100121     C                   EVAL      kr5Nsp = csbNsp
442600100119     C                   EVAL      kr5Trd = 'GEN'
442700100119     C     keyAr5        CHAIN     fiar531c
442800100119     C                   IF        %FOUND()
442900100119     C                   EVAL      dAr5Gen = ar5Uni
443000100119     C                   ELSE
443100100119     C                   RESET                   dAr5Gen
443200100119     C                   RETURN                  *BLANK
443300100119     C                   ENDIF
443400100119     C                   ENDIF
443500100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
443600100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
443700100119     C                   ELSE
443800100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
443900100119     C                   ENDIF
444000100121     C                   ENDIF
444100100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
444200100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
444300100119     C                   IF        t02Err = 'E'
444400100119     C                   RETURN                  *BLANK
444500100119     C                   ENDIF
444600100120     C                   EVAL      dI1a = t02Uni
444700140506     C                   IF        dI1a.descrizi2 <> *BLANK AND csbDdc > 0
444800140417     C                   RETURN                  dI1a.descrizi2
444900140417     C                   ENDIF
445000100119     C                   RETURN                  dI1a.descrizion
445100100119
445200100119     P GetDescrizioneIncassoContrassegno...
445300100119     P                 E
445400100119
445500110304
445600110304     P*--------------------------------------------------
445700110304     P* Procedure name: IsContrassegnoCompensato
445800110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
445900110304     P*                          to con una fattura.
446000110304     P* Returns:        *ON = Compensato
446100110304     P*--------------------------------------------------
446200110304     P IsContrassegnoCompensato...
446300110304     P                 B
446400110304     D IsContrassegnoCompensato...
446500110304     D                 PI              N
446600110304
446700110304     D retField        S               N   INZ(*OFF)
446800110304
446900110307     ** Il contrassegno non è stato rimborsato.
447000110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
447100110307     C                   RETURN    retField
447200110307     C                   ENDIF
447300110307
447400110307     C/EXEC SQL
447500110307     C+ SELECT '1'
447600110307     C+ INTO :retField
447700110307     C+ FROM tncsv00f
447800110307     C+ WHERE csvAas = :csbAas
447900110307     C+   AND csvLnp = :csbLnp
448000110307     C+   AND csvNrs = :csbNrs
448100110307     C+   AND csvNsp = :csbNsp
448200110307     C+   AND csvCav = 'COMP'
448300110307     C+ FETCH FIRST ROW ONLY
448400110307     C/END-EXEC
448500110304
448600110307     C                   SELECT
448700110307     C                   WHEN      sqlCode < 0
448800110307     C                   DUMP(A)
448900110307     C                   ENDSL
449000110304
449100110307     C                   RETURN    retField
449200110304
449300110304     P IsContrassegnoCompensato...
449400110304     P                 E
449500110304
449600120321
449700120321     P*--------------------------------------------------
449800120321     P* Procedure name: SetIdAssegnoMittente
449900120321     P* Purpose:        Imposta l'ID assegno mittente.
450000120321     P* Returns:        Esito.
450100120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
450200120321     P*                          segni mittente.
450300120321     P* Parameter:      priAbi => ABI assegno.
450400120321     P* Parameter:      priCab => CAB assegno.
450500120321     P*--------------------------------------------------
450600120321     P SetIdAssegnoMittente...
450700120321     P                 B
450800120321     D SetIdAssegnoMittente...
450900120321     D                 PI            10I 0
451000120321     D  priNumeroAssegno...
451100120321     D                                     LIKE(caNumO74)
451200120321     D  priAbi                             LIKE(caAbiO74)
451300120321     D  priCab                             LIKE(caCabO74)
451400120321
451500120321     D retField        S             10I 0 STATIC
451600120321
451700120321      /FREE
451800120321
451900120321       CLEAR retField;
452000120321
452100120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
452200120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
452300120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
452400120321       // In questo caso restituisco l'ID del primo assegno.
452500120321
452600120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
452700120321         RETURN retField;
452800120321       ENDIF;
452900120321
453000120321       EXEC SQL
453100120321         SELECT csmNra, csmAbi, csmCab
453200120321           INTO :priNumeroAssegno, :priAbi, :priCab
453300120321           FROM tncsm00f
453400120321           WHERE csmKey = :priNumeroAssegno
453500120321           FETCH FIRST 1 ROW ONLY
453600120321       ;
453700120321
453800120321       SELECT;
453900120321         WHEN sqlCode = 100;
454000120321           retField = sqlCode;
454100120321         WHEN sqlCode < *ZERO;
454200120321           DUMP(A);
454300120321           retField = sqlCode;
454400120321       ENDSL;
454500120321
454600120321       RETURN retField;
454700120321
454800120321      /END-FREE
454900120321     P SetIdAssegnoMittente...
455000120321     P                 E
455100120321
455200130513
455300130513     P*--------------------------------------------------
455400130513     P* Procedure name: GetFilUrl
455500130513     P* Purpose:        Restituisce l'URL della filiale.
455600130513     P* Returns:        URL filiale.
455700130513     P* Parameter:      piFiliale => ID filiale
455800130513     P*--------------------------------------------------
455900130513     P GetFilUrl       B
456000130513     D GetFilUrl       PI           256A
456100130513     D  piFiliale                     3P 0 CONST
456200130513
456300130513     C                   IF        piFiliale = *ZERO
456400130513     C                   RETURN    *BLANK
456500130513     C                   ENDIF
456600130513     C                   RESET                   tibs02ds
456700130513     C                   EVAL      t02Cod = 'UFI'
456800130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
456900130513     C                   EVAL      t02Lin = langTntbe
457000130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
457100130513     C                   IF        t02Err = 'E'
457200130513     C                   RETURN    *BLANK
457300130513     C                   ENDIF
457400130513     C                   RETURN    t02Uni
457500130513
457600130513     P GetFilUrl       E
457700130513
