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
002600080131     ffnevb22l  IF   E           K DISK    USROPN
002700000000     f*---
002800000000     f* Legami bolla
002900000000     f*---
003000000000     ffnlbl01l  IF   E           K DISK    USROPN
003100000000     ffnlbl02l  IF   E           K DISK    USROPN
003200000000     f                                     RENAME(fnlbl000:fnlbl2)
003300130312     f*---
003400130312     f* bolle in arrivo e esiti PDA
003500130312     f*---
003600130312     ffnarb01l  IF   E           K DISK    USROPN
003700130312     ffipct02l  IF   E           K DISK    USROPN
003800130326     ffidst01l  IF   E           K DISK    USROPN
003900000000     f*---
004000000000     f* Giacenze
004100000000     f*---
004200050309     ftigcp51l  IF   E           K DISK    USROPN
004300050309     ftignp01l  IF   E           K DISK    USROPN
004400000000     f*---
004500000000     f* Contrassegni
004600000000     f*---
004700000000     ftncsb03l  IF   E           K DISK    USROPN
004800000000     f*---
004900000000     f* Danni testata CA
005000000000     f*---
005100040906     ffndct01l  IF   E           K DISK    USROPN
005200030206     f*---
005300030206     f* Estensione testata bolle
005400030206     f*---
005500040531     ffiar531c  IF   E           K DISK    USROPN
005600000000     f*---
005700000000     f* Clienti abilitati
005800000000     f*---
005900000000     ftivss02l  IF   E           K DISK    USROPN
006000010716     f*---
006100010716     f* Estensione DPD
006200010716     f*---
006300010716     ftivtd01L  if   e           k disk    USROPN
006400000000     f*---
006500000000     f* Tabelle
006600000000     f*---
006700060622     f*tabel00f  IF   E           K DISK    USROPN
006800000000     f*---
006900000000     f* Tabelle -NEW-
007000000000     f*---
007100000000     ftntbe01l  IF   E           K DISK    USROPN
007200060623     f*tntbe02l  IF   E           K DISK    USROPN
007300060623     f*                                    RENAME(tntbe000:tntbe2)
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
016100051110
016200000000     d*--------------------------------------------------------------------------------------------*
016300000000     d* Data structure
016400000000     d*--------------------------------------------------------------------------------------------*
016500130312     d fipctcetds    e ds
016600090914     D cndizion      E DS                  QUALIFIED BASED(nullPtr)
016700090914     D azlin00f      E DS                  BASED(nullPtr)
016800060622     D tabel00f      E DS                  INZ
016900060622     D kpjba         E DS                  INZ
017000060622     D tibs02ds      E DS                  INZ
017100060622     D  t02Mod       E                     INZ('C')                             Controllo
017200090828     D tis7700i0     E DS                  QUALIFIED
017300090828     D                                     INZ(*EXTDFT)
017400090828     D tis7700o0     E DS                  QUALIFIED
017500090828     d*---
017600000000     d* Ridenominazione formato record -TITAS-
017700000000     d*---
017800000000     d titasds       E DS                  EXTNAME(titas00f)                    *record  titas
017900000000     d titasdssav    E DS                  EXTNAME(titas00f)                    *depisito titas
018000000000     d                                     PREFIX(s_)
018100080131     D titas000Figlia  DS                  LIKEREC(titas000:*INPUT) INZ
018200080131     D titas010Figlia  DS                  LIKEREC(titas010:*INPUT)
018300080131     D                                     BASED(titas000FigliaPtr)
018400080131     D titasp00Figlia  DS                  LIKEREC(titasp00:*INPUT)
018500080131     D                                     BASED(titas000FigliaPtr)
018600080131     d*---
018700000000     d* Variabili riferite al data base
018800000000     d*---
018900000000     d kasaas          S                   LIKE(tasaas)                         *lettura titas30c
019000000000     d kaslnp          S                   LIKE(taslnp)
019100000000     d kasnrs          S                   LIKE(tasnrs)
019200000000     d kasnsp          S                   LIKE(tasnsp)
019300000000     d kastbl          S                   LIKE(tastbl)
019400000000     d kaaaas          S                   LIKE(taaaas)                         *lettura titaa30c
019500000000     d kaalnp          S                   LIKE(taalnp)
019600000000     d kaanrs          S                   LIKE(taanrs)
019700000000     d kaansp          S                   LIKE(taansp)
019800000000     d kaatrc          S                   LIKE(taatrc)
019900000000     d ka4aas          S                   LIKE(ta4aas)                         *lettura tita430c
020000000000     d ka4lnp          S                   LIKE(ta4lnp)
020100000000     d ka4nrs          S                   LIKE(ta4nrs)
020200000000     d ka4nsp          S                   LIKE(ta4nsp)
020300000000     d ka4trc          S                   LIKE(ta4trc)
020400000000     d kvbaas          S                   LIKE(evbaas)                         *lettura fnevb01l
020500000000     d kvblnp          S                   LIKE(evblnp)
020600000000     d kvbnrs          S                   LIKE(evbnrs)
020700000000     d kvbnsp          S                   LIKE(evbnsp)
020800000000     d kvbdev          S                   LIKE(evbdev)
020900000000     d kvbhev          S                   LIKE(evbhev)
021000000000     d kcpaas          S                   LIKE(gcpaas)                         *lettura figcp00f
021100000000     d kcplnp          S                   LIKE(gcplnp)
021200000000     d kcpnrs          S                   LIKE(gcpnrs)
021300000000     d kcpnsp          S                   LIKE(gcpnsp)
021400000000     d kcpfrg          S                   LIKE(gcpfrg)
021500000000     d knpagc          S                   LIKE(gnpagc)                         *lettura fignp00f
021600000000     d knpfgc          S                   LIKE(gnpfgc)
021700000000     d knpngc          S                   LIKE(gnpngc)
021800000000     d knpfrg          S                   LIKE(gnpfrg)
021900000000     d knpfas          S                   LIKE(gnpfas)
022000000000     d ksbaas          S                   LIKE(csbaas)                         *lettura tncsb00f
022100000000     d ksblnp          S                   LIKE(csblnp)
022200000000     d ksbnrs          S                   LIKE(csbnrs)
022300000000     d ksbnsp          S                   LIKE(csbnsp)
022400000000     d kblaan          S                   LIKE(lblaan)                         *lettura fnlbl00f
022500000000     d kbllpn          S                   LIKE(lbllpn)
022600000000     d kblnrn          S                   LIKE(lblnrn)
022700000000     d kblnsn          S                   LIKE(lblnsn)
022800000000     d kblaap          S                   LIKE(lblaap)
022900000000     d kbllpp          S                   LIKE(lbllpp)
023000000000     d kblnrp          S                   LIKE(lblnrp)
023100000000     d kblnsp          S                   LIKE(lblnsp)
023200000000     d kctaas          S                   LIKE(dctaas)                         *lettura fndct00f
023300000000     d kctlnp          S                   LIKE(dctlnp)
023400000000     d kctnrs          S                   LIKE(dctnrs)
023500000000     d kctnsp          S                   LIKE(dctnsp)
023600040531     d kr5aas          S                   LIKE(ar5aas)                         *lettura fiar531c
023700030206     d kr5lnp          S                   LIKE(ar5lnp)
023800030206     d kr5nrs          S                   LIKE(ar5nrs)
023900030206     d kr5nsp          S                   LIKE(ar5nsp)
024000030206     d kr5trd          S                   LIKE(ar5trd)
024100000000     d kssksu          S                   LIKE(vssksu)                         *lettura tivss00f
024200000000     d kssisv          S                   LIKE(vssisv)
024300010717     d ktdksu          s                   LIKE(vtdksu)                         *lettura tivtd00f
024400000000     d kblkut          S                   LIKE(tblkut)                         *per tabel00f
024500000000     d kblcod          S                   LIKE(tblcod)
024600000000     d kbecod          S                   LIKE(tbecod)                         *tntbe01l
024700000000     d kbeke1          S                   LIKE(tbeke1)
024800000000     d kbeke2          S                   LIKE(tbeke2)
024900000000     d kbelin          S                   LIKE(tbelin)
025000000000     d kbesif          S                   LIKE(tbesif)
025100000000     d waas            S                   LIKE(tasaas)                         *di lavoro
025200000000     d wlnp            S                   LIKE(taslnp)
025300000000     d wnrs            S                   LIKE(tasnrs)
025400000000     d wnsp            S                   LIKE(tasnsp)
025500000000     d wfca            S                   LIKE(tasfca)
025600000000     d wfgc            S                   LIKE(tasfgc)
025700000000     d wfpr            S                   LIKE(dctfpr)
025800000000     d depaan          S                   LIKE(lblaan)                         *dei deposito
025900000000     d deplpn          S                   LIKE(lbllpn)
026000000000     d depnrn          S                   LIKE(lblnrn)
026100000000     d depnsn          S                   LIKE(lblnsn)
026200010716     d depute          S                   LIKE(vtdute)
026300010716     d deppwd          S                   LIKE(vtdpwd)
026400030130     d savtsp          S                   LIKE(tastsp)                         *di salvataggio
026500000000     d lcca            S                   LIKE(tascca)                         *di lavoro
026600000000     d lduc            S                   LIKE(tasduc)
026700000000     d llnp            S                   LIKE(taslnp)
026800000000     d ldti            S                   LIKE(tasdti)
026900000000     d lhti            S                   LIKE(tashti)
027000000000     d ldrt            S                   LIKE(tasdrt)
027100000000     d llna            S                   LIKE(taslna)
027200000000     d ldcm            S                   LIKE(tasdcm)
027300000000     d lhmc            S                   LIKE(tashmc)
027400060621     d langTabel       S                   LIKE(tblkut)
027500060621     d langTntbe       S                   LIKE(tbelin)
027600080201     d wDev            S                   LIKE(evb2dev)
027700080201     d wHev            S                   LIKE(evb2hev)
027800000000     d*---
027900000000     d* Ds
028000000000     d*---
028100000000      * controllo data
028200000000     d wlbda8          DS                  INZ                                  *controlla data
028300000000     d  g08dat                        8  0                                       -data dritta
028400000000     d  g08inv                        8  0                                       -data invertita
028500000000     d  g08err                        1                                          -errore
028600000000     d  g08tgi                        5  0                                       -giorni fra date
028700000000      * scomposizione data generica
028800000000     d dsdat           DS                  INZ                                  *data
028900000000     d  dsann                         4  0                                       -anno
029000000000     d  dsmes                         2  0                                       -mese
029100000000     d  dsgio                         2  0                                       -giorno
029200000000      * scomposizione data generica (anno + mese/giorno)
029300000000     d                 DS                  INZ                                  *data aa + mm/gg
029400000000     d  dsannB                 1      4  0                                       -anno
029500000000     d  dsmgsB                 5      8  0                                       -mese/giorno
029600000000     d dsdatB                  1      8  0                                      *data aa + mm/gg
029700000000      * composizione data generica editata
029800000000     d dsdate          DS                  INZ                                  *data
029900000000     d  dsgioe                        2  0                                       -giorno
030000000000     d  dsvd1                         1                                          -'/'
030100000000     d  dsmese                        2  0                                       -mese
030200000000     d  dsvd2                         1                                          -'/'
030300000000     d  dsanne                        4  0                                       -anno
030400000000      * scomposizione ora generica
030500000000     d dsora           DS                  INZ                                  *ora
030600000000     d  dshh                          2  0                                       -ore
030700000000     d  dsmm                          2  0                                       -minuti
030800000000      * composizione ora generica editata
030900000000     d dsorae          DS                  INZ                                  *ora
031000000000     d  dshhe                         2  0                                       -ore
031100000000     d  dsvo1                         1                                          -':'
031200000000     d  dsmme                         2  0                                       -minuti
031300030206      * lettura FIAR5 - record bancali
031400030206     d dar5ban       E DS
031500030206      * lettura FIAR5 - record supermercati
031600030206     d dar5scr       E DS
031700040330     d DAR5GEN       E DS
031800040330     D                                     INZ
031900040330     D                                     PREFIX(GEN)
032000000000      * tipi servizio
032100000000     d ds5e          E DS
032200000000      * disposizioni giacenza
032300000000     d ds2d          E DS
032400000000      * causale eventi
032500000000     d dice          E DS
032600000000      * causali chiusura CA
032700000000     d dcch          E DS
032800100119     d dI1a          E DS                  QUALIFIED
032900000000      * consegne anomale
033000000000     d ds7o          E DS
033100130312      * eventi
033200130312     d ds2a          E DS
033300000000      * riferimenti
033400000000     d dta4a         E DS
033500070927     d dsbl4e        E DS
033600060627      * Estensione blp/arb tipo record "I"
033700060627     d dsbl4i        E DS
033800040913      * Addebito immagine POD
033900040913     d DLVA1         E DS
034000040913     D                                     INZ
034100000000      * lettura campo orgde3 di AZORG00F
034200000000     d og143         E DS
034300000000      * stati contrassegno
034400000000     d ds4s          E DS
034500031217      * DS lettura campo TASFLO di TITAS
034600031217     d dtasflo       E DS
034700000000      * Ds ricerca cliente P.d.C.
034800000000     d bs69ds        E DS                  EXTNAME(tibs69ds) INZ
034900000000     d acods         E DS                  EXTNAME(cnaco00f) INZ
035000000000     d indds         E DS                  EXTNAME(cnind00f) INZ
035100000000     d clpds         E DS                  EXTNAME(cnclp00f) INZ
035200000000     d clsds         E DS                  EXTNAME(fncls00f) INZ
035300000000      * reperimento clienti da cliente unificante
035400090903     d*tibs10ds      E DS
035500090903     d* sksc11                21   5520  0 DIM(500)                             *schiera clienti
035600000000      * scomposizione codice spedizione -DS Input-
035700000000     d dsspedizi74     DS                  INZ                                  *ds cod spedizione
035800000000     d  dslnpi74                      3                                          -linea partenza
035900000000     d  dsnrsi74                      2                                          -serie
036000000000     d  dsnspi74                      7                                          -spedizione
036100000000      * composizione codice spedizione -bolle-
036200000000     d                 DS
036300000000     d  dstaslnp               1      3  0                                       -linea partenza
036400000000     d  dstasnrs               4      5  0                                       -serie
036500000000     d  dstasnsp               6     12  0                                       -spedizione
036600000000     d dstasspe                1     12                                         *ds cod spedizione
036700000000      * composizione data + ora evento
036800080131     d                 DS                  INZ
036900080131     d  evbdev                 1      8  0                                       -data evento
037000080131     d  evbhev                 9     12  0                                       -ora  evento
037100080131     d evbDevHev               1     12  0                                      *ds data+ora evento
037200130312      * composizione spedizione evento
037300130312     d                 DS                  INZ
037400130312     d  evbaas                 1      4  0                                       -data evento
037500130312     d  evblnp                 5      7  0                                       -ora  evento
037600130312     d  evbnrs                 8      9  0                                       -ora  evento
037700130312     d  evbnsp                10     16  0                                       -ora  evento
037800130312     d dsevbspe                1     16  0                                      *ds data+ora evento
037900060629     d devB01        E DS                  INZ                                  Evento MIC di DPD
038000000000      * ordinamento schiere eventi
038100000000     d                 DS
038200000000     d  dsseql                 1      3  0                                       -sequenza legame
038300000000     d  dsseqe                 4      6  0                                       -sequenza evento
038400000000     d  dsdevhev               7     18  0                                       -data + ora
038500000000     d  dsi                   19     21  0                                       -indice
038600000000     d dssort                  1     21  0                                      *ds cod spedizione
038700000000     d* Input routine XALLINEA
038800000000     d walfa7          S              7                                         *alfanumerico 7
038900000000     d walfa7bis       S              7                                         *alfanumerico 7
039000000000     d wzeri           S              7                                         *alfa con zeri
039100000000     d*---
039200000000     d* Variabili
039300000000     d*---
039400101104     d i               S             10I 0                                      *indici
039500101104     d ii              S             10I 0
039600101104     d j               S             10I 0
039700101104     d jj              S             10I 0
039800101104     d z               S             10I 0
039900101104     d s               S             10I 0
040000101104     d k               S             10I 0
040100101104     d o               S             10I 0
040200101104     d tote            S             10I 0                                      *totale eventi memor
040300000000     d seql            S              5  0                                      *sequenza legami
040400000000     d n14             S             14  0                                      *Numerico 14
040500000000     d n8              S              8  0                                      *Numerico 8
040600000000     d n7              S              7  0                                      *Numerico 7
040700000000     d n4              S              4  0                                      *Numerico 4
040800010927     d n3              S              3  0                                      *Numerico 3
040900000000     d a1              S              1                                         *Alfanumerico 1
041000000000     d a3              S              3                                         *Alfanumerico 3
041100000000     d a5              S              5                                         *Alfanumerico 5
041200010717     d a8              S              8                                         *Alfanumerico 8
041300000000     d a15             S             15                                         *Alfanumerico 15
041400000000     d a200            S            200                                         *Alfanumerico 200
041500000000     d a200bis         S            200                                         *Alfanumerico 200
041600010927     d a230            S            230                                         *Alfanumerico 230
041700000000     d a400            S            400                                         *Alfanumerico 400
041800000000     d datcor          S              8  0                                      *data corrente
041900000000     d datpre          S              8  0                                      *data precedente
042000000000     d oracor          S              6  0                                      *ora corrente
042100000000     d anncor          S              4  0                                      *anno corrente
042200000000     d annpre          S              4  0                                      *anno precedente
042300000000     d werr            S              1                                         *errore generico
042400000000     d wdat            S              8  0                                      *data (aaaammgg)
042500000000     d wdate           S             10                                         *data (gg/mm/aaaa)
042600000000     d wora            S              4  0                                      *ora  (hhmm)
042700000000     d worae           S              5                                         *ora  (hh:mm)
042800000000     d worg            S              3  0                                      *filiale
042900000000     d wodes           S             30                                         *descrizione filiale
043000000000     d wofl1           S              1                                         *flag italia/estero
043100020702     d wodpd           S              3                                         *flag filiale DPD
043200000000     d wcev            S              3                                         *causale evento
043300030130     d wcdex           S             30                                         *descrizione bar/pt
043400000000     d wpgm            S             20                                         *programma
043500000000     d wftc            S              1                                         *cons.partic.
043600000000     d wdftc           S             50                                         *descriz.cons.partic
043700000000     d d1wdftc         S             50                                         *descriz.cons.part.1
043800000000     d d2wdftc         S             50                                         *descriz.cons.part.2
043900000000     d wgcx            S              2                                         *turno chiusura
044000000000     d wdgcx           S             20                                         *descriz.turno chius
044100000000     d d1wdgcx         S             50                                         *descriz.turno chi.1
044200000000     d d2wdgcx         S             50                                         *descriz.turno chi.2
044300000000     d we              S              3                                         *descrizione 'e'
044400000000     d bollaorig       S              1                                         *bolla originaria
044500000000     d sostrit         S              1                                         sostituisce Ritiro
044600000000     d sostcon         S              1                                         sostituisce Consegna
044700000000     d newevb          S              1                                         *nuovo evento
044800000000     d lavdevhev       S             12  0                                      *data + ora
044900040913     D WrkCurDate      S               D                                        Data corrente
045000060620     D wrkDtInvDisp    S              8
045100060620     D wrkLnaItaEst    S              1
045200060620     D rpyOpCode       S             10A
045300060620     D rpyEsito        S             10I 0
045400060621     D nullPtr         S               *
045500080131     D titas000FigliaPtr...
045600080131     D                 S               *   INZ(%ADDR(titas000Figlia))
045700010716     d*---
045800010716     d* Variabili di controlli
045900010716     d*---
046000010716     d $AbTD           S              1    INZ('N')                             *abilit.servizio TD
046100010716     d $AbFirma        S              1    INZ('N')                             *abilit. firma DPD
046200000000     d*---
046300000000     d* Variabili di memorizzazione
046400000000     d*---
046500000000     d mhcre           S              5                                         *consegna richiesta
046600000000     d mdcre           S             10
046700000000     d mtcre           S             50
046800000000     d mftce           S            100                                         *consegna partic.
046900000000     d mgcxe           S            100                                         *turno chiusura
047000000000     d mffd            S            100                                         *fermo deposito
047100010927     d mmncB           S             35                                         *NON consegna LAV
047200010927     d mmncC           S             35
047300010927     d mmncG           S             35                                         *NON consegna GIAC
047400010927     d mmncH           S             35
047500010927     d mmncN           S             35                                         *NON consegna NOFATT
047600010927     d mmncO           S             35
047700050701     d mmnc8           S             35
047800050701     d mmnc9           S             35
047900030206     d mtel            S             20                                         *N° tel destinatario
048000030206     d mban            S             35                                         *bancali
048100030207     d mscr            S            500                                         *supermercati
048200030210     d msdace          S             10
048300030210     d msorce          S              5
048400030210     d msdcre          S             10
048500030210     d mshcre          S              5
048600030210     d mstcre          S             30
048700030210     d msnom           S             50
048800030210     d mscrT           S            500    DIM(5)
048900030211     d wmscrT          S           2504
049000000000     d*---
049100000000     d* Schiere
049200000000     d*---
049300000000      * taabella clienti del cliente unificante
049400090903     d*sksc7           S              7  0 DIM(500)                             *clienti
049500000000      * tabella tipi servizio
049600000000     d stsp            S              1    DIM(20)                              *tipi servizio
049700000000     d sdtsp           S             25    DIM(20)                              *descrizione tsp
049800000000      * tabella consegne anomale
049900030130     d scca            S              1    DIM(30)                              *consegna anomala
050000060622     d*sadei           S             20    DIM(30)                              *descrizione BAR
050100060622     d*sadep           S             20    DIM(30)                              *descrizione PT
050200060622     d*sainc           S              1    DIM(30)                              *valida per internet
050300060622     d*saicv           S              1    DIM(30)                              *C.A. con evento
050400060622     d*sacev           S              3    DIM(30)                              *causale evento ICE
050500060622     d*sapod           S              1    DIM(30)                              POD image visualizz.
050600000000      * tabella eventi
050700060622     d*scev            S              3    DIM(200)                             *evento
050800060622     d*scdei           S             30    DIM(200)                             *descrizione italian
050900060622     d*scdep           S             30    DIM(200)                             *descrizione poste
051000060622     d*scsta           S              1    DIM(200)                             *S=Usare in Stati
051100060622     d*scgia           S              1    DIM(200)                             *S=Usare in Giacenze
051200060622     d*scstaP          S              1    DIM(200)                             *S=Usare in Stati PT
051300060622     d*scgiaP          S              1    DIM(200)                             *S=Usare in Giac  PT
051400000000      * tabella causali chiusura CA
051500060622     d*scch            S              2    DIM(30)                              *codice
051600000000      * tabella disposizioni giacenza
051700000000     d sdis            S              1    DIM(10)                              *codice
051800000000     d sddis           S             20    DIM(10)                              *descrizione italian
051900000000      * tabella filiali
052000000000     d sorg            S              3  0 DIM(300)                             *filiale
052100000000     d sodes           S             30    DIM(300)                             *descrizione filiale
052200000000     d sofl1           S              1    DIM(300)                             *flag italia/estero
052300020702     d sopt            S              3    DIM(300)                             *flag poste/no poste
052400020702     d sodpd           S              3    DIM(300)                             *flag DPD/no DPD
052500000000      * tabella stati contrassegno
052600060622     d*ssta            S              1    DIM(20)                              *stato contrassegno
052700060622     d*sdsta           S             35    DIM(20)                              *descrizione stato
052800060622     d*sdstaP          S             20    DIM(20)                              *descriz. stato PT
052900000000      * eventi memorizzati
053000000000     d smi             S              3  0 DIM(200)                             *indice
053100000000     d smseql          S              3  0 DIM(200)                             *sequenza legami
053200000000     d smseqe          S              3  0 DIM(200)                             *sequenza eventi
053300000000     d smdevhev        S             12  0 DIM(200)                             *data + ora evento
053400000000     d smdeve          S             10    DIM(200)                             *data evento editata
053500000000     d smheve          S              5    DIM(200)                             *ora  evento editata
053600000000     d smfle           S             30    DIM(200)                             *descrizione p.o.
053700000000     d smdev           S             30    DIM(200)                             *descrizione evento
053800000000     d smcev           S              3    DIM(200)                             *causale evento
053900130313     d smspe           S             16  0 DIM(200)                             *causale evento
054000000000      * eventi memorizzati x ordinamento
054100000000     d spsort          S             21  0 DIM(200) DESCEND                     ordinamento (DSsort)
054200000000     d spseql          S              3  0 DIM(200)                             *sequenza legami
054300000000     d spseqe          S              3  0 DIM(200)                             *sequenza eventi
054400000000     d spdevhev        S             12  0 DIM(200)                             *data + ora evento
054500000000     d spdeve          S             10    DIM(200)                             *data evento editata
054600000000     d spheve          S              5    DIM(200)                             *ora  evento editata
054700000000     d spfle           S             30    DIM(200)                             *descrizione p.o.
054800000000     d spdev           S             30    DIM(200)                             *descrizione evento
054900000000     d spcev           S              3    DIM(200)                             *causale evento
055000130313     d spspe           S             16  0 DIM(200)                             *causale evento
055100000000      * note giacenze memorizzate
055200051014     d smdmc           S             50    DIM(5)                               *nota -di filiale-
055300051014     d smdmcR          S             50    DIM(5)                               *nota -da cliente-
055400000000      * note memorizzate
055500000000     d smnot           S            100    DIM(10)
055600030206     d*---
055700030206     d* definizioni per passaggio parametri
055800030206     d*---
055900000000     d esito           s              1
056000000000      * DS contenente i dati di INPUT
056100000000     d tis174dsi     e DS
056200050104     D  cAASI74                       4
056300050104     D                                     OVERLAY(AASI74)
056400000000      * DS contenente i dati della pagina: ds attesa dal server di STRATEGI
056500000000     d tis174dso     e DS
056600000000     d dev                           10    DIM(50) OVERLAY(dato74)
056700000000     d hev                            5    DIM(50) OVERLAY(orao74)
056800000000     d fle                           30    DIM(50) OVERLAY(opeo74)
056900041007     d eve                           30    DIM(50) OVERLAY(eveo74)
057000041007     d cev                            3    DIM(50) OVERLAY(cevo74)
057100000000     d nota                         100    DIM(10) OVERLAY(notao74)
057200130313     d spe             S             16  0 DIM(50)
057300040906
057400040906     D FIDN12DS      E DS
057500040906     D                                     INZ
057600040906     D  I12FEL       E
057700040906     D                                     INZ('E')
057800040906     D  I12FAN       E
057900040906     D                                     INZ('E')
058000040906     D  O12KeyAry                    14
058100040906     D                                     DIM(20)
058200040906     D                                     OVERLAY(O12KEY)
058300040906
058400040906     D O12KeyDS        DS
058500040906     D                                     INZ
058600040906     D  O12KeyAAC                     4S 0
058700040906     D  O12KeyFil                     3S 0
058800040906     D  O12KeyNCA                     7S 0
058900040906
059000020702      * DS lettura tabella "NTW"
059100020702     d DNTW          E DS
059200130326     d Ddstflr       E DS
059300041223
059400041223     D ChkCode         S             10U 0
059500041223     D nEsito          S              5I 0
059600051110     D esito10         S             10I 0
059700080131
059800080131     D*--------------------------------------------------
059900080131     D* Procedure name: getRealeDataOraEvento
060000080131     D* Purpose:        Reperisce la reale data e ora evento da estensione ...
060100080131     D*                          eventi bolle.
060200080131     D* Returns:
060300080131     D* Parameter:      piEvb2aas => ID bolla: anno.
060400080131     D* Parameter:      piEvb2lnp => ID bolla: filiale partenza.
060500080131     D* Parameter:      piEvb2nrs => ID bolla: serie.
060600080131     D* Parameter:      piEvb2nsp => ID bolla: numero spedizione.
060700080131     D* Parameter:      piEvb2dtv => Data evento.
060800080131     D* Parameter:      piEvb2orv => Ora evento.
060900080131     D* Parameter:      piEvb2cev => Causale evento.
061000080131     D* Parameter:      piEsito => Esito.
061100080131     D* Parameter:      piEvb2dev => Data evento reale.
061200080131     D* Parameter:      piEvb2hev => Ora evento reale.
061300080131     D*--------------------------------------------------
061400080131     D getRealeDataOraEvento...
061500080131     D                 PR
061600080131     D  piEvb2aas                          LIKE(evb2aas)
061700080131     D                                     CONST
061800080131     D  piEvb2lnp                          LIKE(evb2lnp)
061900080131     D                                     CONST
062000080131     D  piEvb2nrs                          LIKE(evb2nrs)
062100080131     D                                     CONST
062200080131     D  piEvb2nsp                          LIKE(evb2nsp)
062300080131     D                                     CONST
062400080131     D  piEvb2dtv                          LIKE(evb2dtv)
062500080131     D                                     CONST
062600080131     D  piEvb2orv                          LIKE(evb2orv)
062700080131     D                                     CONST
062800080131     D  piEvb2cev                          LIKE(evb2cev)
062900080131     D                                     CONST
063000080201     D  piEvbDev                           LIKE(evb2Dev)
063100080201     D                                     CONST
063200080201     D  piEvbHev                           LIKE(evb2Hev)
063300080201     D                                     CONST
063400080131     D  piEsito                      10I 0
063500080201     D  piEvb2Dev                          LIKE(evb2Dev)
063600080201     D  piEvb2Hev                          LIKE(evb2Hev)
063700100119
063800100119     D*--------------------------------------------------
063900100119     D* Procedure name: GetDescrizioneIncassoContrassegno
064000100119     D* Purpose:        Restituisce la descrizione della modalità di incass...
064100100119     D*                          o del contrassegno.
064200100121     D* Returns:        Descrizione modalità incasso contrassegno.
064300100119     D*--------------------------------------------------
064400100119     D GetDescrizioneIncassoContrassegno...
064500100119     D                 PR            35A
064600110304
064700110304     D*--------------------------------------------------
064800110304     D* Procedure name: IsContrassegnoCompensato
064900110304     D* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
065000110304     D*                          to con una fattura.
065100110304     D* Returns:        *ON = Compensato
065200110304     D*--------------------------------------------------
065300110304     D IsContrassegnoCompensato...
065400110304     D                 PR              N
065500120321
065600120321     D*--------------------------------------------------
065700120321     D* Procedure name: SetIdAssegnoMittente
065800120321     D* Purpose:        Imposta l'ID assegno mittente.
065900120321     D* Returns:        Esito.
066000120321     D* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
066100120321     D*                          segni mittente.
066200120321     D* Parameter:      priAbi => ABI assegno.
066300120321     D* Parameter:      priCab => CAB assegno.
066400120321     D*--------------------------------------------------
066500120321     D SetIdAssegnoMittente...
066600120321     D                 PR            10I 0
066700120321     D  priNumeroAssegno...
066800120321     D                                     LIKE(caNumO74)
066900120321     D  priAbi                             LIKE(caAbiO74)
067000120321     D  priCab                             LIKE(caCabO74)
067100130513
067200130513     D*--------------------------------------------------
067300130513     D* Procedure name: GetFilUrl
067400130513     D* Purpose:        Restituisce l'URL della filiale.
067500130513     D* Returns:        URL filiale.
067600130513     D* Parameter:      piFiliale => ID filiale
067700130513     D*--------------------------------------------------
067800130513     D GetFilUrl       PR           256A
067900130513     D  piFiliale                     3P 0 CONST
068000130513
068100130513
068200120321
068300110304
068400080131
068500000000     i*--------------------------------------------------------------------------------------------*
068600000000     i* Input
068700000000     i*--------------------------------------------------------------------------------------------*
068800000000     i*---
068900000000     i* Indicatori lettura formati record -TITAS-
069000000000     i*---
069100000000     ititas000      01
069200000000     ititas010      02
069300000000     ititasp00      03
069400000000     c*--------------------------------------------------------------------------------------------*
069500000000     c* Main lines
069600000000     c*--------------------------------------------------------------------------------------------*
069700000000     c*
069800000000     c* operazioni iniziali -da fare sempre-
069900000000     c                   EXSR      rutinz
070000000000     c*
070100000000     c* imposta la lettura bolle per l'anno corrente (1° tentativo)
070200000000if  1c                   IF        esito <> '1'                                 *no errori
070300100219     c*
070400100219     c                   MOVEL     dslnpi74      kaslnp
070500100219     c                   MOVEL     dsnrsi74      kasnrs
070600100219     c                   MOVEL     dsnspi74      kasnsp
070700100219     c*
070800111103     c* Se ho ricevuto l'anno della bolla provo con quello e se non la trovo
070900111103     c* restituisco errore.
071000111103     c* Se non ho ricevuto l'anno della bolla, prima provo con l'anno corrente
071100111103     c* e se non la trovo provo con l'anno precedente.
071200111103     c                   IF        aasI74 <> *ZERO
071300100219     c                   Z-ADD     aasI74        kasaas
071400100219     c     keytas        SETLL     titas30c                               99
071500111103     c                   IF        NOT *IN99
071600111103     c                   EVAL      esito = '1'
071700111103     c                   ENDIF
071800111103     c                   ELSE
071900000000     c                   Z-ADD     anncor        kasaas
072000000000     c     keytas        SETLL     titas30c                               99
072100000000if  2c                   IF        NOT *in99                                    *non trovato
072200000000     c                   Z-ADD     annpre        kasaas
072300000000     c     keytas        SETLL     titas30c                               99
072400000000if  3c                   IF        NOT *in99                                    *non trovato
072500000000     c                   MOVEL     '1'           esito                          *errore
072600000000e   3c                   ENDIF
072700000000e   2c                   ENDIF
072800100219     c                   ENDIF
072900000000     c*
073000000000     c* se trovata, legge la bolla
073100000000if  2c                   IF        esito <> '1'
073200000000     C                   SETOFF                                       010203
073300000000     c     keytas        READE     titas30c                               99    *no errore
073400000000if  3c                   IF        *in99                                        *non trovato
073500000000     c                   MOVEL     '1'           esito                          *errore
073600000000x   3c                   ELSE                                                   *trovata
073700000000if  4c                   IF        *in01                                        *lettura titas000
073800000000     c                   EVAL      titasdssav = titasds
073900000000e   4c                   ENDIF
074000000000if  4c                   IF        *in02                                        *lettura titas010
074100000000     c                   EVAL      titasdssav = titasds
074200000000e   4c                   ENDIF
074300000000if  4c                   IF        *in03                                        *lettura titasp00
074400000000     c                   EVAL      titasdssav = titasds
074500000000e   4c                   ENDIF
074600000000e   3c                   ENDIF
074700000000     c*
074800000000     c* controlla validità bolla
074900000000     c                   EXSR      chktas
075000000000     c*
075100030130     c* imposta i campi della DS di Output
075200000000if  3c                   IF        esito <> '1'                                 *no errore
075300100225     c*
075400000000     c                   EVAL      dstaslnp = s_taslnp
075500000000     c                   EVAL      dstasnrs = s_tasnrs
075600000000     c                   EVAL      dstasnsp = s_tasnsp
075700000000     c                   EVAL      nspedizo74 = dstasspe                        *n° spedizione
075800100225     c*
075900100225     c                   Z-ADD     s_tasaas      kr5aas
076000100225     c                   Z-ADD     s_taslnp      kr5lnp
076100100225     c                   Z-ADD     s_tasnrs      kr5nrs
076200100225     c                   Z-ADD     s_tasnsp      kr5nsp
076300100225     c                   MOVEL     'GEN'         kr5trd
076400100225     c     keyar5        CHAIN     fiar531c
076500100225     C                   IF        %FOUND
076600100225     C                   EVAL      DAR5GEN = AR5Uni
076700100225     C                   ELSE
076800100225     C                   RESET                   DAR5GEN
076900100225     C                   ENDIF
077000000000     c*
077100000000     c* imposta i campi della DS di Output dalle bolle
077200000000     c                   EXSR      impdsotas
077300000000     c*
077400000000     c* imposta i campi della DS di Output dagli eventi
077500000000     c                   EXSR      impdsoevb
077600000000     c*
077700000000     c* imposta i campi della DS di Output dal contrassegno
077800000000     c                   EXSR      impdsocsb
077900000000     c*
078000000000     c* imposta i campi della DS di Output dalla giacenza
078100000000     c                   EXSR      impdsogcp
078200000000     c*
078300000000     c* imposta i campi della DS di Output dalle note
078400000000     c                   EXSR      impdsonot
078500040330     c*
078600040330     c* imposta i campi della DS di Output del destinatario.
078700040330     c                   EXSR      ImpDSDest
078800010716     C*
078900010716     c* imposta i campi della DS di Output dalla firma DPD
079000010716     c                   EXSR      ImpdsoDPD
079100040913     C*
079200040913     c* imposta i campi della DS di Output di addebito immagine LDV.
079300040913     c                   EXSR      AddebitoLDV
079400000000e   3c                   ENDIF
079500000000e   2c                   ENDIF
079600000000e   1c                   ENDIF
079700130312     c* se ultimo evento inserito è un MIC (1° della schiera perchè
079800130312     c* è discendente) verifico situazione delle spedizione in filiale
079900130312     c                   IF        cev(1) = EVENTO_MESSA_IN_CONSEGNA
080000130312     c                             and cev(50) = ' '
080100130312     c                   exsr      srarb
080200130312     c                   end
080300000000     c*
080400000000     C                   RETURN
080500000000     c*--------------------------------------------------------------------------------------------*
080600000000     c* controlla validità bolla
080700000000     c*--------------------------------------------------------------------------------------------*
080800000000     c     chktas        BEGSR
080900000000     c*
081000000000     c* se la bolla è più vecchia di un anno (dalla data corrente), esclude
081100000000     c                   Z-ADD     s_tasaas      dsannB                         *data spedizione
081200000000     c                   Z-ADD     s_tasmgs      dsmgsB
081300000000if  1c                   IF        dsdatB < datpre
081400000000     c                   MOVEL     '1'           esito                          *errore
081500000000e   1c                   ENDIF
081600030129     c*
081700030129     c* tipo servizio POSTE, esclude se la bolla è più vecchia del 27/01/2003
081800030129if  1c                   IF        s_tastsp = 'P' AND
081900030204     c                             dsdatB < 20030127
082000030129     c                   MOVEL     '1'           esito                          *errore
082100030129e   1c                   ENDIF
082200030130     c*
082300030130     c* se l'esito è positivo, salva alcuni dati della bolla
082400030130     c                   EVAL      savtsp = s_tastsp
082500000000     c*
082600130423     C                   EVAL      wOrg = s_tasLna
082700051222     C                   EXSR      setLnaItaEst
082800051222     C
082900000000     c                   ENDSR
083000000000     c*--------------------------------------------------------------------------------------------*
083100000000     c* imposta i campi della DS di Output dalle bolle e files correlati
083200000000     c*--------------------------------------------------------------------------------------------*
083300000000     c     impdsotas     BEGSR
083400000000     c*
083500130513     c                   EVAL      i = %LOOKUP(s_tasLna : sorg)
083600130513     c                   EVAL      lnaDescO74 = sodes(i)
083700130513     c                   EVAL      lnaUrlO74 = GetFilUrl(s_tasLna)
083800130510     c*
083900010808     c                   MOVEL     s_tasaas      aasmgsO74                       *data spedizione
084000010808     c                   MOVE      s_tasmgs      aasmgsO74
084100010808     c                   Z-ADD     s_tasccm      ccmO74                          *cliente mittente
084200010808     c*
084300000000     c                   MOVEL     s_tastbl      a1
084400000000if  1c                   IF        a1 = 'A'                                     *Pagamento del
084500060613     c*                  EVAL      portoo74 =
084600060613     c*                            'Destinatario'
084700060613     c                   EVAL      portoo74 = rtvMsgLang('TIS0145':langI74)
084800000000x   1c                   ELSE                                                   *Pagamento del
084900060612     c*                  EVAL      portoo74 =
085000060612     c*                            'Mittente'
085100060612     c                   EVAL      portoo74 = rtvMsgLang('TIS0357':langI74)
085200000000e   1c                   ENDIF
085300060622     c*                  SETOFF                                           98
085400060622     c*                  Z-ADD     1             i
085500060622     c*    s_tastsp      LOOKUP    stsp(i)                                98
085600060622if  1c*                  IF        *in98                                        *trovata
085700060622     c*                  EVAL      corro74 = sdtsp(i)
085800060622e   1c*                  ENDIF
085900060626     C                   EVAL      ds5E = chainTabel00f('CHAIN3':langTabel
086000060626     C                             :'5E':s_tastsp:%LEN(s_tastsp):'TBLUNI':
086100060626     C                             rpyOpCode:rpyEsito)
086200060622     C                   EVAL      corro74 = §5EDes
086300000000     c*
086400000000     c                   EVAL      destino74 = s_tasrsd
086500000000     c                   EVAL      desindo74 = s_tasind
086600000000     c                   EVAL      descapo74 = s_tascad
086700000000     c                   EVAL      desloco74 = s_taslod
086800000000if  1c                   IF        s_tasnzd  = *blanks
086900000000     c                   EVAL      desnazo74 = s_tasprd
087000000000x   1c                   ELSE
087100000000     c                   EVAL      desnazo74 = s_tasnzd
087200051011     c                   EVAL      desnzdo74 = s_tasnzd
087300000000e   1c                   ENDIF
087400000000     c*
087500000000     c                   EVAL      collio74  = s_tasncl
087600000000     c                   EVAL      pesoo74   = s_taspkf
087700000000     c                   EVAL      volumeo74 = s_tasvlf
087800000000     c                   EVAL      naturao74 = s_tasnas
087900000000     c*
088000000000     c                   EVAL      fatnumo74  = s_tasnft
088100000000     c                   MOVEL     s_tasdft      wdat
088200000000     c                   EXSR      edtdat
088300000000     c                   EVAL      fatdatao74 = wdate
088400000000     c*
088500000000     c                   EVAL      disnumo74  = s_tasndc
088600010716c    c                   MOVEL     s_tasddc      wdat
088700000000     c                   EXSR      edtdat
088800000000     c                   EVAL      disdatao74 = wdate
088900041222     **?Immagine lettera di vettura.
089000041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
089100041222     C                   IF        LDVO74 <> 'N'
089200031217     c                   CLEAR                   dtasflo
089300031217     c                   MOVEL     s_tasflo      dtasflo
089400050325     c*                  MOVEL     §floiml       ldvo74
089500040330     c                   IF        §floiml <> *BLANK
089600050325     c                   EVAL      ldvo74 = 'S'
089700040330     C                   EVAL      SpImLDVO74 =
089800040330     C                             %SUBST(%EDITC(s_tasaas:'X'):3:2) +
089900040330     C                             %EDITC(s_taslnp:'X') +
090000040330     C                             %EDITC(s_tasnrs:'X') +
090100040330     C                             %EDITC(s_tasnsp:'X')
090200041223     **?Reperisco il check code della spedizione POD
090300041223     C                   EVAL      PChkCdeO74 =
090400041223     C                             GetSpeChkCde(
090500041223     C                             %EDITC(s_tasaas:'X'):
090600041223     C                             %EDITC(s_taslnp:'X') +
090700041223     C                             %EDITC(s_tasnrs:'X') +
090800041223     C                             %EDITC(s_tasnsp:'X'):
090900041223     C                             ChkCode:nEsito)
091000040330     C                   ENDIF
091100041222     C                   ENDIF
091200031217     c*
091300000000     c                   MOVE      s_tasccm      n4
091400000000if  1c                   IF        n4 <> 8888                                   *codificato
091500000000     c                   CLEAR                   bs69ds
091600000000     c                   CLEAR                   acods
091700000000     c                   MOVEL     'GAITRA201'   i69sif                         *s.informativo
091800000000     c                   MOVEL     s_tasccm      i69kac                         *cl.mittente x CNACO
091900000000     c                   MOVEL     s_tasccm      i69kin                         *cl.mittente x CNIND
092000000000     c                   CALL      'TIBS69R'                                    *lettura P.d.C.
092100000000     c                   PARM                    bs69ds
092200000000     c                   PARM                    acods
092300000000     c                   PARM                    indds
092400000000     c                   PARM                    clpds
092500000000     c                   PARM                    clsds
092600000000if  2c                   IF        o69err <> '1'                                *no errore
092700000000     c                   EVAL      mito74    = acorag
092800000000     c                   EVAL      mitindo74 = indvia
092900000000     c                   EVAL      mitloco74 = indcit
093000000000     c                   MOVEL     indsta        a3
093100000000if  3c                   IF        a3 = *blanks
093200000000     c                   EVAL      mitnazo74 = indprv
093300000000x   3c                   ELSE
093400030130     c                   EVAL      mitnazo74 = a3
093500000000e   3c                   ENDIF
093600000000if  3c                   IF        indcae <> *blanks
093700000000     c                   EVAL      mitcapo74 = indcae
093800000000X   3c                   ELSE
093900000000     c                   MOVEL     indcap        a5
094000000000     c                   EVAL      mitcapo74 = a5
094100000000e   3c                   ENDIF
094200000000e   2c                   ENDIF
094300000000x   1c                   ELSE                                                   *non codificato
094400000000     c                   Z-ADD     s_tasaas      kaaaas
094500000000     c                   Z-ADD     s_taslnp      kaalnp
094600000000     c                   Z-ADD     s_tasnrs      kaanrs
094700000000     c                   Z-ADD     s_tasnsp      kaansp
094800000000     c                   MOVEL     'M'           kaatrc
094900000000     c     keytaa        CHAIN     titaa30c                           99
095000000000if  2c                   IF        NOT *in99
095100000000     c                   EVAL      mito74    = taarsc
095200000000     c                   EVAL      mitindo74 = taaind
095300000000     c                   EVAL      mitcapo74 = taacap
095400000000     c                   EVAL      mitloco74 = taaloc
095500000000if  3c                   IF        taanaz = *blanks
095600000000     c                   EVAL      mitnazo74 = taaprv
095700000000x   3c                   ELSE
095800000000     c                   EVAL      mitnazo74 = taanaz
095900000000e   3c                   ENDIF
096000000000e   2c                   ENDIF
096100000000e   1c                   ENDIF
096200090112     c* 2a ragione sociale destinatario.
096300090112     c                   CLEAR                   tita4000
096400090112     c                   CLEAR                   tita4p00
096500090112     c                   Z-ADD     s_tasaas      ka4aas
096600090112     c                   Z-ADD     s_taslnp      ka4lnp
096700090112     c                   Z-ADD     s_tasnrs      ka4nrs
096800090112     c                   Z-ADD     s_tasnsp      ka4nsp
096900090112     c                   MOVEL     'D'           ka4trc
097000090112     c     keyta4        CHAIN     tita430c                           99
097100090112if  1c                   IF        NOT *in99
097200090112     c                   MOVEL     ta4not        desti2O74
097300090112e   1c                   ENDIF
097400070927     c* rif partner estero
097500070927     c                   CLEAR                   tita4000
097600070927     c                   CLEAR                   tita4p00
097700070927     c                   CLEAR                   dsbl4e
097800070927     c                   Z-ADD     s_tasaas      ka4aas
097900070927     c                   Z-ADD     s_taslnp      ka4lnp
098000070927     c                   Z-ADD     s_tasnrs      ka4nrs
098100070927     c                   Z-ADD     s_tasnsp      ka4nsp
098200070927     c                   MOVEL     'E'           ka4trc
098300070927     c     keyta4        CHAIN     tita430c                           99
098400070927if  1c                   IF        NOT *in99
098500070927     c                   MOVEL     ta4not        dsbl4e
098600070927e   1c                   ENDIF
098700070927     c
098800010717     c                   CLEAR                   tita4000
098900010717     c                   CLEAR                   tita4p00
099000010717     c                   CLEAR                   dta4a
099100000000     c                   Z-ADD     s_tasaas      ka4aas
099200000000     c                   Z-ADD     s_taslnp      ka4lnp
099300000000     c                   Z-ADD     s_tasnrs      ka4nrs
099400000000     c                   Z-ADD     s_tasnsp      ka4nsp
099500000000     c                   MOVEL     'A'           ka4trc
099600000000     c     keyta4        CHAIN     tita430c                           99
099700000000if  1c                   IF        NOT *in99
099800000000     c                   MOVEL     ta4not        dta4a
099900000000e   1c                   ENDIF
100000090825     ** Riferimento mittente numerico (c'è sempre).
100100090825     C                   EVAL      rifero74 = %CHAR(s_tasrmn)
100200090825     ** Aggiungo il riferimento mittente alfabetico solo se diverso dal numerico.
100300090825     C                   IF        §ta4arma <> *BLANK AND
100400090825     C                             %TRIML(§ta4arma) <> %CHAR(s_tasrmn)
100500110127     C                   EVAL      %SUBST(rifero74:16) = §ta4arma
100600090825     C                   ENDIF
100700090825     ** Visualizzo il riferimento partner solo se diverso dagli altri.
100800090825     C                   IF        §b4erp <> *BLANK AND
100900090825     C                             %TRIML(§b4erp) <> %CHAR(s_tasrmn) AND
101000090825     C                             %TRIML(§b4erp) <> %TRIML(§ta4arma)
101100090825     C                   EVAL      rpto74 = §b4erp
101200090825     C                   ENDIF
101300090828
101400041223     **?Reperisco il check code della spedizione.
101500041223     C                   EVAL      SChkCdeO74 =
101600041223     C                             GetSpeChkCde(%EDITC(AASI74:'X'):
101700041223     C                             NSpedizI74:ChkCode:nEsito)
101800090828
101900040916     **?Se l'utente non è anonimo controllo che la spedizione gli appartenga.
102000040916     C                   IF        KSCI74 <> *BLANK
102100000000     c                   SETOFF                                           98
102200090903     c*                  Z-ADD     1             i
102300090903     c*    s_tasksc      LOOKUP    sksc7(i)                               98      *cl.fatturazione
102400090828     ** Appartiene alla famiglia, eseguo ulteriore test con subunificante.
102500090903     C*                  IF        *IN98
102600090828     C                   RESET                   tis7700i0
102700090828     C                   EVAL      tis7700i0.aas = s_tasAas
102800090828     C                   EVAL      tis7700i0.lnp = s_tasLnp
102900090828     C                   EVAL      tis7700i0.nrs = s_tasNrs
103000090828     C                   EVAL      tis7700i0.nsp = s_tasNsp
103100090828     C                   EVAL      tis7700i0.tbl = s_tasTbl
103200090828     C                   EVAL      tis7700i0.ksu = kscI74
103300090828     C                   EVAL      tis7700i0.sun = %EDITC(rqsCidI174 : 'X')
103400090828     C                   CALLP     Selettore_bolla_subUnificante('CHKBOLLA'
103500090828     C                             : rpyEsito
103600090828     C                             : 'TIS7700I0' : tis7700i0 : %SIZE(tis7700i0)
103700090828     C                             : 'TIS7700O0' : tis7700o0 : %SIZE(tis7700o0)
103800090828     C                             )
103900090903     C                   EVAL      *IN98 = (rpyEsito >= *ZERO AND
104000090903     C                             tis7700o0.bollValida = *ON)
104100090903     C*                  ENDIF
104200090828     ** Non appartiene alla famiglia.
104300090903if  1c*                  IF        NOT *in98                                      *non trovato
104400090903if  2c*                  IF        s_tasccm <> s_tasksc                           *cl.fatt<>cl.mitt
104500090903     c*                  Z-ADD     1             i
104600090903     c*                  SETOFF                                           98
104700090903     c*    s_tasccm      LOOKUP    sksc7(i)                               98      *cl.fatturazione
104800090903e   2c*                  ENDIF
104900041223     **?Se non gli appartiene, prima di restituire errore controllo il check code.
105000090828if  2c                   IF        NOT *in98                                      *non trovato
105100041223     C                   IF        SChkCdeI74 = 0 OR SChkCdeI74 <> SChkCdeO74
105200040916     C                   CLEAR                   TIS174DSO
105300040916     C                   EVAL      Esito = *ON
105400040916     C                   RETURN
105500041223     C                   ENDIF
105600000000e   2c                   ENDIF
105700090903e   1c*                  ENDIF
105800040916     C                   ENDIF
105900000000     c*
106000100513     ** Importo assicurato.
106100100513     C                   IF         s_tasIas > 0 AND s_tasFcm <> 'F'
106200100513     C                   EVAL      vasO74 = s_tasVas
106300100513     C                   EVAL      iasO74 =
106400100513     C                             %EDITC(%DEC(s_tasIas : 9 : 2) : '2')
106500100513     C                   ENDIF
106600100513     **
106700000000     c                   ENDSR
106800000000     c*--------------------------------------------------------------------------------------------*
106900000000     c* imposta i campi della DS di Output dagli eventi
107000000000     c*--------------------------------------------------------------------------------------------*
107100000000     c     impdsoevb     BEGSR
107200000000     c*
107300000000     c* azzera le variabili di lavoro
107400000000     c                   CLEAR                   tote                           *contatore eventi
107500000000     c                   CLEAR                   seql                           *sequenza legami
107600000000     c                   CLEAR                   spsort                         *per ordinamento
107700000000     c                   CLEAR                   spseql                         *di memorizzazione
107800000000     c                   CLEAR                   spseqe
107900000000     c                   CLEAR                   spdevhev
108000000000     c                   CLEAR                   spdeve
108100000000     c                   CLEAR                   spheve
108200000000     c                   CLEAR                   spfle
108300000000     c                   CLEAR                   spdev
108400000000     c                   CLEAR                   spcev
108500130312     c                   CLEAR                   spspe
108600000000     c*
108700000000     c* imposta la sequenza legami per la bolla di Input
108800000000     c                   Z-ADD     1             seql                           *primo legaqme
108900000000     c*---
109000000000     c* memorizza gli "eventi fissi" che sono presenti nel record bolla
109100000000     c*---
109200000000     c***
109300000000     c* Ritiro merce -> Controlla se la bolla ha una mamma:
109400000000     c* . se SI e la mamma ha una consegna anomala l'evento di ritiro di questa bolla (la figlia)
109500000000     c*   si trasforma nella consegna anomala della mamma
109600000000     c* . se NO l'evento di questa bolla rimane quello di ritiro
109700000000     c***
109800000000     c                   MOVEL     'S'           bollaorig                      *SI bolla orginaria
109900000000     c                   MOVEL     'N'           sostrit                        *NO sost. Ritiro
110000000000     c                   EVAL      kblaan = s_tasaas
110100000000     c                   EVAL      kbllpn = s_taslnp
110200000000     c                   EVAL      kblnrn = s_tasnrs
110300000000     c                   EVAL      kblnsn = s_tasnsp
110400000000     c     keylbl        CHAIN     fnlbl01l                           99
110500000000if  1c                   IF        NOT *in99                                    *ha la mamma
110600000000     c                   MOVEL     'N'           bollaorig                      *NO bolla orginaria
110700000000     c                   EVAL      kasaas = lblaap
110800000000     c                   EVAL      kaslnp = lbllpp
110900000000     c                   EVAL      kasnrs = lblnrp
111000000000     c                   EVAL      kasnsp = lblnsp
111100000000     c     keytas        CHAIN     titas30c                           99        *leggo la 1^
111200000000if  2c                   IF        NOT *in99 AND
111300000000     c                             tascca <> *blanks                            *ha cons.anomala
111400060622     c*                  SETOFF                                           98
111500060622     c*                  Z-ADD     1             jj
111600060622     c*    tascca        LOOKUP    scca(jj)                               98
111700060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
111800060626     C                             :'7O':tascca:%LEN(tascca):'TBLUNI':rpyOpCode
111900060622     C                             :rpyEsito)
112000060622if  3c*                  IF        *in98                                        *trovata
112100060626     C                   IF        rpyOpCode = 'FOUND'
112200041222     **?Consegna anomala, POD non visualizzabile.
112300060622     C*                  IF        sapod(jj) = 'N'
112400060622     C                   IF        §7OPODImg = 'N'
112500041222     C                   EVAL      LDVO74 = 'N'
112600041222     C                   ENDIF
112700060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
112800060622     C                   IF        §7oinc <> *blanks                            *ok per internet
112900000000     c                   MOVEL     'S'           sostrit                        *SI sost. Ritiro
113000041222e   3c                   ENDIF
113100000000e   3c                   ENDIF
113200000000e   2c                   ENDIF
113300000000e   1c                   ENDIF
113400000000     c*
113500030203if  1c                   IF        savtsp='P' AND
113600030203     c                             tascca='5'
113700030203x   1c                   ELSE
113800030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
113900000000     c                   ADD       1             tote
114000060622if  3c*                  IF        sainc(jj) = 'S'                              *evento cons.anomala
114100060622     C                   IF        §7oinc = 'S'                                 *evento cons.anomala
114200030203if  4c                   IF        savtsp = 'P'                                 *bolle poste
114300060622     c*                  MOVEL     sadep(jj)     smdev(tote)
114400060622     c                   MOVEL     §7odep        smdev(tote)
114500030203x   4c                   ELSE                                                   *bolle bartolini
114600060622     c*                  MOVEL     sadei(jj)     smdev(tote)
114700060622     c                   MOVEL     §7odei        smdev(tote)
114800030203e   4c                   ENDIF
114900030203x   3c                   ELSE                                                   *evento telef.P.O.
115000030203if  4c                   IF        savtsp = 'P'                                 *bolle poste
115100060622     c*                  EVAL      smdev(tote) = sadep(jj)
115200060622     c                   EVAL      smdev(tote) = §7odep
115300030203x   4c                   ELSE                                                   *bolle bartolini
115400060608     c*                  EVAL      smdev(tote) = 'TELEFONARE AL PUNTO OPERATIVO'
115500060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
115600060608e   4c                   ENDIF
115700030203e   3c                   ENDIF
115800030203e   2c                   ENDIF
115900030203e   1c                   ENDIF
116000030203     c*
116100030203     c*
116200030203if  1c                   IF        savtsp='P' AND
116300030203     c                             tascca='5'
116400030203x   1c                   ELSE
116500030203if  2c                   IF        sostrit = 'S'                                *SI sost.ritiro
116600060622     c*                  MOVEL     sacev(jj)     wcev                           *causale evento
116700060622     c                   MOVEL     §7ocev        wcev                           *causale evento
116800000000     c                   EXSR      evbrit                                       *imposta evento
116900030203x   2c                   ELSE                                                   *NO sost.ritiro
117000030203if  3c                   IF        s_tasdrt > *zeros
117100000000     c                   MOVEL     '701'         wcev                           *causale evento
117200000000     c                   EXSR      deccevsta
117300030203if  4c                   IF        werr <> '1'                                  *no errore
117400000000     c                   ADD       1             tote
117500030130     c                   MOVEL     wcdex         smdev(tote)
117600000000     c                   EXSR      evbrit                                       *imposta evento
117700030203e   4c                   ENDIF
117800030203e   3c                   ENDIF
117900030203e   2c                   ENDIF
118000030203e   1c                   ENDIF
118100080131     ** ID bolla madre.
118200080131     c                   EVAL      waas = s_tasaas
118300080131     c                   EVAL      wlnp = s_taslnp
118400080131     c                   EVAL      wnrs = s_tasnrs
118500080131     c                   EVAL      wnsp = s_tasnsp
118600000000     c* Partenza merce
118700000000     c                   EVAL      lduc = s_tasduc                              *data partenza
118800000000     c                   EVAL      llnp = s_taslnp                              *filiale partenza
118900000000     c                   EVAL      ldrt = s_tasdrt                              *data ritiro
119000000000     c                   EXSR      evbpar
119100000000     c* Arrivo merce
119200110914     c                   EVAL      ldti = s_tasdti                              *data arrivo
119300110914     c                   EVAL      lhti = s_tashti                              *ora  arrivo
119400110914     c                   IF        gen§ar5ArrDt <> *BLANK AND
119500110914     c                             gen§ar5ArrDt <> *ZERO
119600100225     c                   MONITOR
119700100225     c                   EVAL      ldti = %DEC(gen§ar5ArrDt:8:0)                *data arrivo
119800100225     c                   EVAL      lhti = %DEC(gen§ar5ArrHm:4:0)                *ora  arrivo
119900100225     c                   ON-ERROR
120000100225     c                   ENDMON
120100110914     c                   ENDIF
120200100225     c                   EVAL      llna = s_taslna                              *linea arrivo
120300090826     c                   EVAL      ldcm = s_tasdcm                              *data consegna
120400090826     c                   EXSR      evbarr
120500000000     c*
120600000000     c* Consegna merce
120700000000     c                   EVAL      lcca = s_tascca                              *consegna anomala
120800000000     c                   EVAL      ldcm = s_tasdcm                              *data consegna
120900000000     c                   EVAL      lhmc = s_tashmc                              *ora  consegna
121000000000     c                   EVAL      llna = s_taslna                              *linea arrivo
121100000000     c                   EXSR      evbcon
121200000000     c* Lettera anomalia
121300000000if  1c                   IF        s_tasfda = 'S'                               *ha avuto CA
121400080131     c*                  EVAL      waas = s_tasaas                              *bolla Input
121500080131     c*                  EVAL      wlnp = s_taslnp
121600080131     c*                  EVAL      wnrs = s_tasnrs
121700080131     c*                  EVAL      wnsp = s_tasnsp
121800000000     c                   EXSR      memdct
121900000000e   1c                   ENDIF
122000000000     c*---
122100000000     c* memorizza gli eventi dal file eventi della bolla di input
122200000000     c*---
122300080131     c*                  Z-ADD     s_tasaas      waas
122400080131     c*                  Z-ADD     s_taslnp      wlnp
122500080131     c*                  Z-ADD     s_tasnrs      wnrs
122600080131     c*                  Z-ADD     s_tasnsp      wnsp
122700000000     c                   EXSR      memevb
122800000000     c*---
122900000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
123000000000     c*---
123100000000     c                   EXSR      elaevelbl
123200000000     c*
123300000000     c* imposta contatore eventi da visualizzare nella DS di Output
123400000000     c                   EVAL      cnteveo74 = tote
123500000000     c*---
123600000000     c* ordina gli eventi per sequenza legami / Data+ora evento
123700000000     c*---
123800000000     c* compone la schiera da ordinare
123900000000do  1c     1             DO        200           j
124000000000     c                   Z-ADD     smseql(j)     dsseql                          -sequenza legami
124100000000     c                   Z-ADD     smseqe(j)     dsseqe                          -sequenza evento
124200000000     c                   Z-ADD     smdevhev(j)   dsdevhev                        -data + ora
124300000000     c                   Z-ADD     smi(j)        dsi                             -indice
124400000000     c                   Z-ADD     dssort        spsort(j)                      *schiera da ordinare
124500000000e   1c                   ENDDO
124600000000     c*
124700000000     c* ordina la schiera composta
124800000000     c                   SORTA     spsort
124900000000     c*
125000000000     c* ricompone le schiere da quella ordinata
125100000000     c                   Z-ADD     *zeros        jj
125200000000do  1c     1             DO        200           j
125300000000if  2c                   IF        spsort(j) >*zeros
125400000000     c                   ADD       1             jj
125500000000     c                   Z-ADD     spsort(j)     dssort                         *schiera ordinata
125600000000     c                   Z-ADD     dsseql        spseql(jj)                      -sequenza legami
125700000000     c                   Z-ADD     dsseqe        spseqe(jj)                      -sequenza eventi
125800000000     c                   Z-ADD     dsdevhev      spdevhev(jj)                    -data+ora evento
125900000000     c                   Z-ADD     smseqe(dsi)   spseqe(jj)                     *altri dati
126000000000     c                   MOVEL     smdeve(dsi)   spdeve(jj)
126100000000     c                   MOVEL     smheve(dsi)   spheve(jj)
126200000000     c                   MOVEL     smfle(dsi)    spfle(jj)
126300000000     c                   MOVEL     smdev(dsi)    spdev(jj)
126400000000     c                   MOVEL     smcev(dsi)    spcev(jj)
126500130312     c                   MOVEL     smspe(dsi)    spspe(jj)
126600000000e   2c                   ENDIF
126700000000e   1c                   ENDDO
126800101104     **
126900101104     ** A questo punto devo aggiustare l'ordine degli eventi "arrivata in filiale"
127000101104     ** e "messa in consegna". L'ora della messa in consegna è fissa 08.00, quindi
127100101104     ** può capitare che sia antecedente all'arrivo in filiale, quando invece la
127200101104     ** messa in consegna è sempre successiva all'arrivo in filiale.
127300101104     ** Quindi cerco nella schiera degli eventi un evento messa in consegna immediatamente
127400101104     ** seguito da un evento arrivo in filiale e gli inverto la posizione.
127500101104     ** Non cambio l'ora della messa in consegna perchè dopo sarà oscurata.
127600101104     ** Ricordati che l'ordine per data e ora è discendente.
127700101104     C                   EVAL      j = 0
127800101104     C                   DOU       j = 0
127900101104     C                   EVAL      j = %LOOKUP(EVENTO_MESSA_IN_CONSEGNA
128000101104     C                                        : spcev : j+1)
128100101104     C                   IF        j = 0
128200101104     C                   LEAVE
128300101104     C                   ENDIF
128400101104     C                   IF        j > 1 AND
128500101104     C                             spcev(j-1) = EVENTO_ARRIVATA_IN_FILIALE
128600101104     C                   EVAL      dev(1) = spdeve(j-1)
128700101104     C                   EVAL      hev(1) = spheve(j-1)
128800101104     C                   EVAL      fle(1) = spfle(j-1)
128900101104     C                   EVAL      eve(1) = spdev(j-1)
129000101104     C                   EVAL      cev(1) = spcev(j-1)
129100130312     C                   EVAL      spe(1) = spspe(j-1)
129200101104     C                   EVAL      spdeve(j-1) = spdeve(j)
129300101104     C                   EVAL      spheve(j-1) = spheve(j)
129400101104     C                   EVAL      spfle(j-1) = spfle(j)
129500101104     C                   EVAL      spdev(j-1) = spdev(j)
129600101104     C                   EVAL      spcev(j-1) = spcev(j)
129700130312     C                   EVAL      spspe(j-1) = spspe(j)
129800101104     C                   EVAL      spdeve(j) = dev(1)
129900101104     C                   EVAL      spheve(j) = hev(1)
130000101104     C                   EVAL      spfle(j) = fle(1)
130100101104     C                   EVAL      spdev(j) = eve(1)
130200101104     C                   EVAL      spcev(j) = cev(1)
130300130312     C                   EVAL      spspe(j) = spe(1)
130400101104     C                   ENDIF
130500101104     C                   ENDDO
130600000000     c*
130700120831     c* imposta gli eventi nella DS di Output, ricontandoli perchè a volte il conteggio sbaglia.
130800120831     c                   CLEAR                   cntEveO74
130900000000do  1c     1             DO        50            j
131000120831     c                   IF        spcev(j) = *BLANK
131100120831     c                   LEAVE
131200120831     c                   ENDIF
131300000000     c                   EVAL      dev(j) = spdeve(j)
131400000000     c                   EVAL      hev(j) = spheve(j)
131500000000     c                   EVAL      fle(j) = spfle(j)
131600041007     c                   EVAL      eve(j) = spdev(j)
131700041007     c                   EVAL      cev(j) = spcev(j)
131800130312     c                   EVAL      spe(j) = spspe(j)
131900101104     c                   IF        cev(j) = EVENTO_MESSA_IN_CONSEGNA
132000100909     c                   CLEAR                   hev(j)
132100100909     c                   ENDIF
132200120831     c                   EVAL      cntEveO74 += 1
132300000000e   1c                   ENDDO
132400000000     c*
132500000000     c                   ENDSR
132600000000     c*--------------------------------------------------------------------------------------------*
132700000000     c* memorizza gli eventi dal file eventi della bolla di input
132800000000     c*--------------------------------------------------------------------------------------------*
132900000000     c     memevb        BEGSR
133000000000     c***
133100000000     c* Scrittura evento -> Controlla se è già stato inserito (codice+data+ora) da un'altra bolla:
133200000000     c* . se SI lo aggiorna solo nella sequenza legame
133300000000     c*   se NO lo inserisce
133400000000     c***
133500000000     c                   Z-ADD     waas          kvbaas
133600000000     c                   Z-ADD     wlnp          kvblnp
133700000000     c                   Z-ADD     wnrs          kvbnrs
133800000000     c                   Z-ADD     wnsp          kvbnsp
133900000000     c     keyevb        SETLL     fnevb01l
134000000000     c     keyevb        READE     fnevb01l                               99
134100000000do  1c                   DOW       NOT *in99
134200000000if  2c                   IF        evbatb = *blanks                             *valido
134300080131     C                   CALLP     getRealeDataOraEvento( evbAas : evbLnp
134400080131     C                             : evbNrs : evbNsp : evbDtv : evbOrv : evbCev
134500080201     C                             : evbDev : evbHev : esito10 : wDev : wHev )
134600080131if  3c                   IF        evbDev > *zeros
134700000000     c                   MOVEL     evbcev        wcev
134800000000     c                   EXSR      deccevsta
134900070109     c                   EXSR      eventoAnnulla
135000000000if  4c                   IF        werr <> '1'                                  *no errore
135100000000     c*
135200000000     c* controlla se evento già inserito
135300000000     c* . se SI aggiorna la sequenza legami e la filiale di arrivo
135400000000     c                   MOVEL     'S'           newevb                         *nuovo evento
135500000000do  5c     1             DO        200           j
135600000000if  6c                   IF        evbcev = smcev(j) AND                        *stesso codice
135700080131     c                             evbDevHev = smdevhev(j)                      *stessa data+ora
135800000000     c                   MOVEL     'N'           newevb                         *già inserito
135900000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
136000080130     c                   Z-ADD     tasLna        worg
136100000000     c                   EXSR      decorg
136200000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
136300080201     c                   MOVEL     wDev          wdat
136400080201     c                   EXSR      edtdat
136500080201     c                   MOVEL     wdate         smdeve(j)
136600080201     c                   MOVEL     wHev          wora
136700080201     c                   EXSR      edtora
136800080201     c                   MOVEL     worae         smheve(j)
136900130313     c                   MOVEL     dsevbspe      smspe(j)
137000000000     c                   LEAVE                                                  *uscita ciclo
137100000000e   6c                   ENDIF
137200000000e   5c                   ENDDO
137300000000     c*
137400000000     c* incrementa contatore eventi
137500000000if  5c                   IF        newevb = 'S'                                 NUOVO evento
137600000000     c                   ADD       1             tote
137700000000     c* indice schiera
137800000000     c                   Z-ADD     tote          smi(tote)
137900000000     c* sequenza legame
138000000000     c                   Z-ADD     seql          smseql(tote)
138100000000     c* sequenza evento
138200000000     c                   Z-ADD     020           smseqe(tote)
138300000000     c* descrizione
138400030130     c                   MOVEL     wcdex         smdev(tote)
138500000000     c* data
138600080201     c                   MOVEL     wDev          wdat
138700000000     c                   EXSR      edtdat
138800000000     c                   MOVEL     wdate         smdeve(tote)
138900000000     c* ora
139000080201     c                   MOVEL     wHev          wora
139100000000     c                   EXSR      edtora
139200000000     c                   MOVEL     worae         smheve(tote)
139300000000     c* filiale
139400080130     c                   Z-ADD     tasLna        worg
139500000000     c                   EXSR      decorg
139600000000     c                   MOVEL     wodes         smfle(tote)
139700000000     c* causale
139800000000     c                   MOVEL     evbcev        smcev(tote)
139900130312     c* spedizione evento mic, mi serve per poi agganciare ARB in caso di
140000130312     c* bolla legata
140100130312     c                   MOVEL     dsevbspe      smspe(tote)
140200000000     c* data+ora evento
140300080131     c                   Z-ADD     evbDevHev     smdevhev(tote)
140400000000e   5c                   ENDIF
140500000000e   4c                   ENDIF
140600000000e   3c                   ENDIF
140700000000e   2c                   ENDIF
140800010717     c     keyevb        READE     fnevb01l                               99
140900000000e   1c                   ENDDO
141000000000     c*
141100000000     c                   ENDSR
141200000000     c*--------------------------------------------------------------------------------------------*
141300000000     c* memorizza gli "eventi fissi"
141400000000     c*--------------------------------------------------------------------------------------------*
141500000000     c     memevefix     BEGSR
141600080131     ** ID bolla in canna.
141700080131     c                   EVAL      waas = tasaas
141800080131     c                   EVAL      wlnp = taslnp
141900080131     c                   EVAL      wnrs = tasnrs
142000080131     c                   EVAL      wnsp = tasnsp
142100000000     c*
142200000000     c* Partenza merce
142300000000     c                   EVAL      lduc = tasduc                                *data partenza
142400000000     c                   EVAL      llnp = taslnp                                *filiale partenza
142500000000     c                   EVAL      ldrt = tasdrt                                *data ritiro
142600000000     c                   EXSR      evbpar
142700000000     c* Arrivo merce
142800000000     c*---               EVAL      ldti = tasdti                                *data arrivo
142900000000     c*---               EVAL      lhti = tashti                                *ora  arrivo
143000000000     c*---               EVAL      llna = taslna                                *linea arrivo
143100000000     c*---               EVAL      ldcm = tasdcm                                *data consegna
143200000000     c*---               EXSR      evbarr
143300000000     c*
143400000000     c* Consegna merce
143500000000     c                   EVAL      lcca = tascca                                *consegna anomala
143600000000     c                   EVAL      ldcm = tasdcm                                *data consegna
143700000000     c                   EVAL      lhmc = tashmc                                *ora  consegna
143800000000     c                   EVAL      llna = taslna                                *linea arrivo
143900000000     c                   EXSR      evbcon
144000000000     c*
144100000000     c                   ENDSR
144200000000     c*--------------------------------------------------------------------------------------------*
144300000000     c* elabora gli eventi -fissi e non- delle bolle legate (successivamente) alla bolla di input
144400000000     c*--------------------------------------------------------------------------------------------*
144500000000     c     elaevelbl     BEGSR
144600000000     c*
144700000000     c* azzera le variabili di lavoro
144800000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
144900000000     c                   EVAL      deplpn = *zeros
145000000000     c                   EVAL      depnrn = *zeros
145100000000     c                   EVAL      depnsn = *zeros
145200000000     c*
145300000000     c* legge i legami delle figlie successive alla bolla di input
145400000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
145500000000     c                   EVAL      kbllpp =  s_taslnp
145600000000     c                   EVAL      kblnrp =  s_tasnrs
145700000000     c                   EVAL      kblnsp =  s_tasnsp
145800000000     c*
145900000000     c* ciclo fino a fine mamme
146000000000     c     ke2lbl        SETLL     fnlbl02l
146100000000     c     ke2lbl        READE     fnlbl02l                               99
146200000000do  1c                   DOW       NOT *in99
146300000000     c*
146400000000     c* ciclo fino a fine figlie della mamma
146500000000do  2c                   DOW       NOT *in99
146600000000     c*
146700000000     c* legge la bolla dal legame
146800000000     c                   EVAL      kasaas = lblaan                              *figlia
146900000000     c                   EVAL      kaslnp = lbllpn
147000000000     c                   EVAL      kasnrs = lblnrn
147100000000     c                   EVAL      kasnsp = lblnsn
147200000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
147300000000if  3c                   IF        NOT *in99 AND                                *esistente
147400000000     c                             tastbl <> 'AR' AND                           *NO bolla RECUPERO
147500000000     c                             tastbl <> 'A3' AND
147600000000     c                             tastbl <> 'F3' AND
147700000000     c                             tastbl <> 'AP' AND
147800000000     c                             tastbl <> 'FC'
147900041222     **?Immagine lettera di vettura.
148000041222     **?LDVO74 = 'N' significa che il POD image non deve essere visualizzato.
148100041222     C                   IF        LDVO74 <> 'N'
148200040330     c                   MOVEL     tasflo        dtasflo
148300040330     c                   IF        §floiml <> *BLANK
148400050325     c*                  MOVEL     §floiml       ldvo74
148500050325     c                   EVAL      ldvo74 = 'S'
148600040330     C                   EVAL      SpImLDVO74 =
148700040330     C                             %SUBST(%EDITC(tasaas:'X'):3:2) +
148800040330     C                             %EDITC(taslnp:'X') +
148900040330     C                             %EDITC(tasnrs:'X') +
149000040330     C                             %EDITC(tasnsp:'X')
149100041223     **?Reperisco il check code della spedizione POD
149200041223     C                   EVAL      PChkCdeO74 =
149300041223     C                             GetSpeChkCde(
149400041223     C                             %EDITC(tasaas:'X'):
149500041223     C                             %EDITC(taslnp:'X') +
149600041223     C                             %EDITC(tasnrs:'X') +
149700041223     C                             %EDITC(tasnsp:'X'):
149800041223     C                             ChkCode:nEsito)
149900040330     C                   ENDIF
150000041222     C                   ENDIF
150100000000     c* incrementa la sequenza legami per le bolle legate
150200000000     c                   ADD       1             seql                           *legaqme successivo
150300000000     c*---
150400000000     c* memorizza gli "eventi fissi"
150500000000     c*---
150600000000     c                   EXSR      memevefix
150700000000     c*---
150800000000     c* memorizza gli eventi
150900000000     c*---
151000000000     c                   EVAL      waas = lblaan                                *figlia
151100000000     c                   EVAL      wlnp = lbllpn
151200000000     c                   EVAL      wnrs = lblnrn
151300000000     c                   EVAL      wnsp = lblnsn
151400000000     c                   EXSR      memevb
151500000000     c*
151600000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
151700000000     c                   EVAL      depaan = lblaan                              *ultima figlia
151800000000     c                   EVAL      deplpn = lbllpn
151900000000     c                   EVAL      depnrn = lblnrn
152000000000     c                   EVAL      depnsn = lblnsn
152100000000e   3c                   ENDIF
152200000000     c*
152300000000     c* lettura successiva legami
152400000000     c     ke2lbl        READE     fnlbl02l                               99
152500000000e   2c                   ENDDO                                                  *fine figlie mamma
152600000000     c*
152700000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
152800000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
152900000000     c                             kbllpp <> deplpn OR
153000000000     c                             kblnrp <> depnrn OR
153100000000     c                             kblnsp <> depnsn
153200000000     c* nuova chiave
153300000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
153400000000     c                   EVAL      kbllpp =  deplpn
153500000000     c                   EVAL      kblnrp =  depnrn
153600000000     c                   EVAL      kblnsp =  depnsn
153700000000     c* controlla figlie
153800000000     c     ke2lbl        SETLL     fnlbl02l
153900000000     c     ke2lbl        READE     fnlbl02l                               99
154000000000if  3c                   IF        *in99                                        *non ha figlie
154100000000     c                   LEAVE                                                  *esce dal ciclo
154200000000e   3c                   ENDIF
154300000000x   2c                   ELSE                                                   *sempre stessa mamma
154400000000     c                   LEAVE                                                  *esce dal ciclo
154500000000e   2c                   ENDIF
154600000000e   1c                   ENDDO                                                  *fine mamme
154700000000     c*
154800000000     c                   ENDSR
154900000000     c*--------------------------------------------------------------------------------------------*
155000000000     c* Imposta l'evento Ritiro (per consegna anomala mamma o ritiro figlia) -GESTIONE PARTICOLARE-
155100000000     c*--------------------------------------------------------------------------------------------*
155200000000     c     evbrit        BEGSR
155300000000     c*
155400000000     c* indice schiera
155500000000     c                   Z-ADD     tote          smi(tote)
155600000000     c* sequenza legame
155700000000     c                   Z-ADD     seql          smseql(tote)
155800000000     c* sequenza evento
155900000000     c                   Z-ADD     010           smseqe(tote)                   *primo evento
156000000000     c* data
156100000000if  1c                   IF        s_tasdrt > *zeros
156200000000     c                   MOVEL     s_tasdrt      wdat
156300000000     c                   EXSR      edtdat
156400000000     c                   MOVEL     wdate         smdeve(tote)
156500000000e   1c                   ENDIF
156600000000     c* ora
156700000000if  1c                   IF        s_tashrt > *zeros
156800000000     c                   MOVEL     s_tashrt      wora
156900000000x   1c                   ELSE
157000000000if  2c                   IF        s_tasfpp = 'P'                               *PM
157100000000     c                   MOVEL     1800          wora
157200000000x   2c                   ELSE                                                   *AM
157300000000     c                   MOVEL     1200          wora
157400000000e   2c                   ENDIF
157500000000e   1c                   ENDIF
157600000000     c                   EXSR      edtora
157700000000     c                   MOVEL     worae         smheve(tote)
157800000000     c* filiale
157900000000     c                   Z-ADD     s_taslnp      worg
158000000000     c                   EXSR      decorg
158100000000     c                   MOVEL     wodes         smfle(tote)
158200000000     c* causale
158300000000     c                   MOVEL     wcev          smcev(tote)
158400000000     c* data+ora evento
158500000000     c                   MOVEL     wdat          smdevhev(tote)
158600000000     c                   MOVE      wora          smdevhev(tote)
158700000000     c*
158800000000     c                   ENDSR
158900000000     c*--------------------------------------------------------------------------------------------*
159000000000     c* Imposta l'evento Partenza
159100000000     c*--------------------------------------------------------------------------------------------*
159200000000     c     evbpar        BEGSR
159300000000     c*
159400000000     c* controlla SE e COME inserire l'evento
159500000000if  1c                   IF        lduc > *zeros
159600000000     c* imposta causale
159700000000     c                   MOVEL     '702'         wcev
159800000000     c                   EXSR      deccevsta
159900000000if  2c                   IF        werr <> '1'                                  *no errore
160000000000     c* imposta filiale
160100000000     c                   Z-ADD     llnp          worg
160200000000     c                   EXSR      decorg
160300000000     c* imposta data
160400101104if  3c                   IF        wofl1 = ESTERO                               partenza ESTERO
160500000000     c                   MOVEL     ldrt          wdat                           *data ritiro
160600000000x   3c                   ELSE                                                   partenza ITALIA
160700000000     c                   MOVEL     lduc          wdat                           *data partenza
160800000000e   3c                   ENDIF
160900000000     c                   EXSR      edtdat
161000000000     c* imposta ora
161100000000     c                   MOVEL     2100          wora                           *ora imposta
161200000000     c                   EXSR      edtora
161300000000     c*
161400000000     c* controlla se l'evento è già stato inserito
161500000000     c                   MOVEL     'S'           newevb                         *nuovo evento
161600000000do  3c     1             DO        200           j
161700000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
161800000000     c                   MOVE      wora          lavdevhev
161900000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
162000000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
162100000000     c                   MOVEL     'N'           newevb                         *già inserito
162200000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
162300000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
162400000000     c                   LEAVE                                                  *uscita ciclo
162500000000e   4c                   ENDIF
162600000000e   3c                   ENDDO
162700000000     c*
162800000000     c* incrementa contatore eventi
162900000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
163000000000     c                   ADD       1             tote
163100000000     c* indice schiera
163200000000     c                   Z-ADD     tote          smi(tote)
163300000000     c* sequenza legame
163400000000     c                   Z-ADD     seql          smseql(tote)
163500000000     c* sequenza evento
163600000000     c                   Z-ADD     020           smseqe(tote)
163700000000     c* descrizione
163800030130     c                   MOVEL     wcdex         smdev(tote)
163900000000     c* filiale -impostata sopra-
164000000000     c                   MOVEL     wodes         smfle(tote)
164100000000     c* data    -impostata sopra-
164200000000     c                   MOVEL     wdate         smdeve(tote)
164300000000     c* ora     -impostata sopra-
164400000000     c                   MOVEL     worae         smheve(tote)
164500000000     c* causale -impostata sopra-
164600000000     c                   MOVEL     wcev          smcev(tote)
164700000000     c* data+ora evento
164800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
164900000000     c                   MOVE      wora          smdevhev(tote)
165000000000e   3c                   ENDIF
165100000000e   2c                   ENDIF
165200000000e   1c                   ENDIF
165300000000     c*
165400000000     c                   ENDSR
165500101129     c*------------------------------------------------------------------------*
165600101129     c*
165700101129     c* Imposta l'evento Arrivo
165800101129     c*
165900101129     c*------------------------------------------------------------------------*
166000000000     c     evbarr        BEGSR
166100101129     ** Visualizzo l'evento Arrivo solo se la filiale di arrivo è italiana,
166200101129     ** perchè in caso di spedizione export la filiale di arrivo è la nostra HUB
166300101129     ** italiana, non la filiale del partner, quindi mostrerei un evento tutto
166400101129     ** sommato inutile.
166500130312     C                   IF        wrkLnaItaEst = ESTERO
166600101104     C                   LEAVESR
166700101104     C                   ENDIF
166800101129     ** Visualizzo l'evento arrivo solo se esiste l'ora di arrivo.
166900101129     C                   IF        lhti = *ZERO
167000101129     C                   LEAVESR
167100101129     C                   ENDIF
167200000000     c*
167300000000     c* controlla SE e COME inserire l'evento
167400000000if  1c                   IF        ldti > *zeros OR
167500000000     c                             ldcm > *zeros
167600000000     c* imposta causale
167700101104     c                   EVAL      wcev = EVENTO_ARRIVATA_IN_FILIALE
167800000000     c                   EXSR      deccevsta
167900000000if  2c                   IF        werr <> '1'                                  *no errore
168000000000     c* imposta filiale
168100000000     c                   Z-ADD     llna          worg
168200000000     c                   EXSR      decorg
168300000000     c* imposta data
168400000000if  3c                   IF        ldti > *zeros
168500000000     c                   MOVEL     ldti          wdat                           *data arrivo
168600000000x   3c                   ELSE
168700000000     c                   MOVEL     ldcm          wdat                           *data consegna
168800000000e   3c                   ENDIF
168900000000     c                   EXSR      edtdat
169000000000     c* imposta ora
169100000000if  3c                   IF        lhti > *zeros
169200000000     c                   MOVEL     lhti          wora                           *ora arrivo
169300000000x   3c                   ELSE
169400000000     c                   MOVEL     0600          wora                           *ora imposta
169500000000e   3c                   ENDIF
169600000000     c                   EXSR      edtora
169700000000     c*
169800000000     c* controlla se l'evento è già stato inserito
169900000000     c                   MOVEL     'S'           newevb                         *nuovo evento
170000000000do  3c     1             DO        200           j
170100000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
170200000000     c                   MOVE      wora          lavdevhev
170300000000if  4c                   IF        smcev(j)= wcev AND                           *stesso codice
170400000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
170500000000     c                   MOVEL     'N'           newevb                         *già inserito
170600000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
170700000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
170800000000     c                   LEAVE                                                  *uscita ciclo
170900000000e   4c                   ENDIF
171000000000e   3c                   ENDDO
171100000000     c*
171200000000     c* incrementa contatore eventi
171300000000if  3c                   IF        newevb = 'S'                                 NUOVO evento
171400000000     c                   ADD       1             tote
171500000000     c* indice schiera
171600000000     c                   Z-ADD     tote          smi(tote)
171700000000     c* sequenza legame
171800000000     c                   Z-ADD     seql          smseql(tote)
171900000000     c* sequenza evento
172000000000     c                   Z-ADD     020           smseqe(tote)
172100000000     c* descrizione
172200030130     c                   MOVEL     wcdex         smdev(tote)
172300000000     c* data    -impostata sopra-
172400000000     c                   MOVEL     wdate         smdeve(tote)
172500000000     c* ora     -impostata sopra-
172600000000     c                   MOVEL     worae         smheve(tote)
172700000000     c* filiale -impostata sopra-
172800000000     c                   MOVEL     wodes         smfle(tote)
172900000000     c* causale -impostata sopra-
173000000000     c                   MOVEL     wcev          smcev(tote)
173100000000     c* data+ora evento
173200000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
173300000000     c                   MOVE      wora          smdevhev(tote)
173400000000e   3c                   ENDIF
173500000000e   2c                   ENDIF
173600000000e   1c                   ENDIF
173700000000     c*
173800000000     c                   ENDSR
173900000000     c*--------------------------------------------------------------------------------------------*
174000000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -prima parte-
174100000000     c*--------------------------------------------------------------------------------------------*
174200000000     c     evbcon        BEGSR
174300000000     c*
174400000000     c* azzera variabili di lavoro
174500000000     c                   MOVEL     'N'           sostcon                        *NO sost. Consegna
174600000000     c****
174700000000     c* Consegna merce ->
174800000000     c* . se la bolla ha una consegna anomala l'evento di consegna si trasforma nella cons.anom
174900000000     c*   NON per le c.a di DIROTTAMENTO o CAMBIO DI PORTO per le quali viene scritto sempre
175000000000     c*   l'evento e che quindi avrebbero due segnalazioni per la stessa cosa: per queste non s
175100000000     c*   nè lo stato di consegna nè quello di consegna anomala.
175200000000     c****
175300000000     c* controlla SE e COME inserire l'evento
175400000000if  1c                   IF        lcca <> *blanks                              *ha cons.anomala
175500060622     c*                  SETOFF                                           98
175600060622     c*                  Z-ADD     1             jj
175700060622     c*    lcca          LOOKUP    scca(jj)                               98
175800060626     C                   EVAL      ds7O = chainTabel00f('CHAIN3':langTabel
175900060626     C                             :'7O':lcca:%LEN(lcca):'TBLUNI':rpyOpCode
176000060622     C                             :rpyEsito)
176100060622if  2c*                  IF        *in98                                        *trovata
176200060622     C                   IF        rpyOpCode = 'FOUND'
176300041222     **?Consegna anomala, POD non visualizzabile.
176400060622     C*                  IF        sapod(jj) = 'N'
176500060622     C                   IF        §7OPODImg = 'N'
176600041222     C                   EVAL      LDVO74 = 'N'
176700041222     C                   ENDIF
176800060622     c*                  IF        sainc(jj) <> *blanks                         *ok per internet
176900060622     c                   IF        §7oinc <> *blanks                            *ok per internet
177000060622if  3c*                  IF        saicv(jj) <> 'S'                             *NON ha record FNEVB
177100060622if  3c                   IF        §7oicv <> 'S'                                *NON ha record FNEVB
177200000000     c                   MOVEL     'S'           sostcon                        *SI sost. Consegna
177300000000x   3c                   ELSE                                                   *ha record FNEVB
177400000000     c                   MOVEL     ' '           sostcon                        *non considera c.a.
177500000000e   3c                   ENDIF
177600041222e   3c                   ENDIF
177700000000e   2c                   ENDIF
177800000000e   1c                   ENDIF
177900080131     c* imposta data
178000080131if  1c                   IF        ldcm > *zeros
178100080131     c                   MOVEL     ldcm          wdat
178200080131e   1c                   ENDIF
178300080131     c* imposta ora
178400080131if  1c                   IF        lhmc > *zeros
178500080131     c                   MOVEL     lhmc          wora
178600080131e   1c                   ENDIF
178700000000     c* imposta causale
178800000000if  1c                   IF        sostcon = 'S'                                *SI sost.Consegna
178900060622     c                   MOVEL     §7ocev        wcev                           *causale evento
179000080131     c* Il seguente pezzo di codice serve per reperire la giusta data dell'evento
179100080131     c* "reso mittente" della bolla originale. Normalmente il campo TASDCM (data
179200080131     c* consegna merce reale) contiene la data giusta, ma quando il collo viene
179300080131     c* reso e consegnato al mittente il campo TASDCM della bolla originale viene
179400080131     c* aggiornato con la data di consegna; in questo caso reperisco TASDRT (data
179500080131     c* ritiro merce) dalla bolla figlia.
179600080131     c                   IF        waas = s_tasaas AND wlnp = s_taslnp
179700080131     c                             AND wnrs = s_tasnrs AND wnsp = s_tasnsp
179800080131     c                             AND s_tasNdc < *HIVAL
179900080131     c                   EVAL      kblAap = wAas
180000080131     c                   EVAL      kblLpp = wLnp
180100080131     c                   EVAL      kblNrp = wNrs
180200080131     c                   EVAL      kblNsp = wNsp
180300080131     c     ke2lbl        CHAIN     fnlbl02l
180400080131     c                   IF        %FOUND()
180500080131     c                   EVAL      kasAas = lblAan
180600080131     c                   EVAL      kasLnp = lblLpn
180700080131     c                   EVAL      kasNrs = lblNrn
180800080131     c                   EVAL      kasNsp = lblNsn
180900080131     C     keyTas        CHAIN     titas000      titas000Figlia
181000080131     C                   IF        %FOUND()
181100080131     C                   EVAL      wDat = titas000Figlia.tasDrt
181200080131     C                   EVAL      wOra = titas000Figlia.tasHrt
181300080131     C                   ELSE
181400080131     C     keyTas        CHAIN     titas010      titas010Figlia
181500080131     C                   IF        %FOUND()
181600080131     C                   EVAL      wDat = titas010Figlia.tasDrt
181700080131     C                   EVAL      wOra = titas010Figlia.tasHrt
181800080131     C                   ELSE
181900080131     C     keyTas        CHAIN     titasp00      titasp00Figlia
182000080131     C                   IF        %FOUND()
182100080131     C                   EVAL      wDat = titasp00Figlia.tasDrt
182200080131     C                   EVAL      wOra = titasp00Figlia.tasHrt
182300080131     C                   ENDIF
182400080131     C                   ENDIF
182500080131     C                   ENDIF
182600080131     c                   ENDIF
182700080131     c                   ENDIF
182800000000x   1c                   ELSE                                                   *NO sost.Consegna
182900000000     c                   MOVEL     '704'         wcev                           *causale evento
183000000000e   1c                   ENDIF
183100080131     c* Editazione data e ora evento.
183200080131     c                   EXSR      edtdat
183300080131     c                   EXSR      edtora
183400000000     c* imposta filiale
183500000000     c                   Z-ADD     llna          worg
183600000000     c                   EXSR      decorg
183700000000     c* controlla se l'evento è già stato inserito
183800000000     c                   MOVEL     'S'           newevb                         *nuovo evento
183900000000do  1c     1             DO        200           j
184000000000     c                   MOVEL     wdat          lavdevhev                      *data+ora evento
184100000000     c                   MOVE      wora          lavdevhev
184200000000if  2c                   IF        smcev(j)= wcev AND                           *stesso codice
184300000000     c                             lavdevhev = smdevhev(j)                      *stessa data+ora
184400000000     c                   MOVEL     'N'           newevb                         *già inserito
184500000000     c                   Z-ADD     seql          smseql(j)                      *AGGIORNA sequenza
184600000000     c                   MOVEL     wodes         smfle(j)                       *AGGIORNA filiale
184700000000     c                   LEAVE                                                  *uscita ciclo
184800000000e   2c                   ENDIF
184900000000e   1c                   ENDDO
185000000000     c*
185100000000     c* incrementa contatore eventi
185200030203if  1c                   IF        savtsp='P' AND
185300030203     c                             lcca='5'
185400030203x   1c                   ELSE
185500030203if  2c                   IF        newevb = 'S'                                 NUOVO evento
185600030203if  3c                   IF        sostcon = 'S'                                *SI sost.Consegna
185700000000     c                   ADD       1             tote                           incrementa contatore
185800060622if  4c*                  IF        sainc(jj) = 'S'                              *evento cons.anomala
185900060622if  4c                   IF        §7oinc = 'S'                                 *evento cons.anomala
186000030203if  5c                   IF        savtsp = 'P'                                 *bolle poste
186100060622     c*                  MOVEL     sadep(jj)     smdev(tote)                    *descrizione
186200060622     c                   MOVEL     §7odep        smdev(tote)                    *descrizione
186300030203x   5c                   ELSE                                                   *evento telef.P.O.
186400060622     c*                  MOVEL     sadei(jj)     smdev(tote)                    *descrizione
186500060622     c                   MOVEL     §7odei        smdev(tote)
186600030203e   5c                   ENDIF
186700030203x   4c                   ELSE                                                   *evento telef.P.O.
186800030203if  5c                   IF        savtsp = 'P'                                 *bolle poste
186900060622     c*                  EVAL      smdev(tote) = sadep(jj)
187000060622     c                   EVAL      smdev(tote) = §7odep
187100030203x   5c                   ELSE                                                   *bolle bartolini
187200060608     c*                  EVAL      smdev(tote) = 'TELEFONARE AL PUNTO OPERATIVO'
187300060612     C                   EVAL      smdev(tote) = rtvMsgLang('TIS0579':langI74)
187400030203e   5c                   ENDIF
187500030203e   4c                   ENDIF
187600060622     c*                  MOVEL     sacev(jj)     wcev                           *causale evento
187700060622     c                   MOVEL     §7ocev        wcev                           *causale evento
187800000000     c                   EXSR      evbcon2                                      *imposta evento
187900000000     c*
188000030203x   3c                   ELSE                                                   *NO sost.Consegna
188100000000     c*
188200030203if  4c                   IF        ldcm > *zeros
188300030203if  5c                   IF        sostcon <> ' '                               *considera consegna
188400000000     c                   EXSR      deccevsta
188500030203if  6c                   IF        werr <> '1'                                  *no errore
188600000000     c                   ADD       1             tote                           incrementa contatore
188700030130     c                   MOVEL     wcdex         smdev(tote)                    *descrizione
188800000000     c                   EXSR      evbcon2                                      *imposta evento
188900030203e   6c                   ENDIF
189000030203e   5c                   ENDIF
189100030203e   4c                   ENDIF
189200030203e   3c                   ENDIF
189300030203e   2c                   ENDIF
189400000000     c*
189500030203e   1c                   ENDIF
189600030203     c*
189700000000     c                   ENDSR
189800000000     c*--------------------------------------------------------------------------------------------*
189900000000     c* Imposta l'evento Consegna (per consegna anomala bolla)                       -seconda parte-
190000000000     c*--------------------------------------------------------------------------------------------*
190100000000     c     evbcon2       BEGSR
190200000000     c*
190300000000     c* indice schiera
190400000000     c                   Z-ADD     tote          smi(tote)
190500000000     c* sequenza legame
190600000000     c                   Z-ADD     seql          smseql(tote)
190700000000     c* sequenza evento
190800000000     c                   Z-ADD     999           smseqe(tote)                   *ultimo evento
190900000000     c* data    -impostata sopra-
191000000000     c                   MOVEL     wdate         smdeve(tote)
191100000000     c* ora     -impostata sopra-
191200000000     c                   MOVEL     worae         smheve(tote)
191300000000     c* filiale -impostata sopra-
191400000000     c                   MOVEL     wodes         smfle(tote)
191500000000     c* causale -impostata sopra-
191600000000     c                   MOVEL     wcev          smcev(tote)
191700000000     c* data+ora evento
191800000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
191900000000     c                   MOVE      wora          smdevhev(tote)
192000000000     c*
192100000000     c                   ENDSR
192200000000     c*--------------------------------------------------------------------------------------------*
192300000000     c* memorizza i dati della lettera di anomalia
192400000000     c*--------------------------------------------------------------------------------------------*
192500000000     c     memdct        BEGSR
192600000000     c*
192700040906     C                   RESET                   FIDN12DS
192800040906     c                   Z-ADD     waas          I12AAS
192900040906     c                   Z-ADD     wlnp          I12LNP
193000040906     c                   Z-ADD     wnrs          I12NRS
193100040906     c                   Z-ADD     wnsp          I12NSP
193200040906     C                   CALL      'FIDN12R'
193300040906     C                   PARM                    FIDN12DS
193400000000     c* memorizza la prima lettera di anomalia valida
193500040906do  1c                   IF        O12Err = *BLANK
193600040906     C                   DO        O12NCA        K
193700040906     C                   EVAL      O12KeyDS = O12KeyAry(K)
193800040906     C                   EVAL      DCTAAC = O12KeyAAC
193900040906     C                   EVAL      DCTFIL = O12KeyFil
194000040906     C                   EVAL      DCTNCA = O12KeyNCA
194100040906     c     keydct        CHAIN     fndct01l
194200040906do  1c                   IF        %FOUND
194300000000     c*
194400000000     c* controlla validità CA
194500000000     c                   MOVEL     '0'           werr                           *NO errore
194600000000     c                   EXSR      chkdct
194700000000if  2c                   IF        werr = '0'                                   *CA valida
194800000000     c*
194900000000     c* Imposta l'evento lettera di anomalia
195000000000     c                   EXSR      evbdct
195100000000     C                   LEAVE                                                  *uscita ciclo
195200000000e   2c                   ENDIF
195300000000     c*
195400040906e   1c                   ENDIF
195500040906e   1c                   ENDDO
195600040906e   1c                   ENDIF
195700000000     C*
195800000000     c                   ENDSR
195900000000     c*--------------------------------------------------------------------------------------------*
196000000000     c* controlla validità lettera di anomalia
196100000000     c*--------------------------------------------------------------------------------------------*
196200000000     c     chkdct        BEGSR
196300000000     c*
196400000000     c* reimposta variabili di lavoro
196500000000     c                   MOVEL     '0'           werr                           *NO errore
196600000000     c                   MOVEL     dctfpr        wfpr                           *tipo gestione CA
196700000000     c*
196800000000     c* CA annullata, esclude
196900000000if  1c                   IF        dctatb <> ' '
197000000000     c                   MOVEL     '1'           werr                           *SI errore
197100000000e   1C                   ENDIF
197200000000     c*
197300000000     c* CA non confermata, esclude
197400000000if  1c                   IF        dctfpr = ' '
197500000000     c                   MOVEL     '1'           werr                           *SI errore
197600000000e   1C                   ENDIF
197700000000     c*
197800000000     c* CA transattiva -> fase 215, CA pratica -> fase 400, altimenti esclude
197900000000if  1c                   IF        dctfpr = 'T' AND
198000000000     c                             dctfca >= 215 OR
198100000000     c                             dctfpr = 'P' AND
198200000000     c                             dctfca >= 400
198300000000x   1c                   ELSE
198400000000     c                   MOVEL     '1'           werr                           *SI errore
198500000000e   1C                   ENDIF
198600000000     c*
198700000000     c* CA chiusa deve avere una causale di chiusura valida per Internet, altirmenti esclude
198800000000if  1c                   IF        werr = '0'
198900000000if  2c                   IF        dctcch <> '  '                               *chiusura ok x INT
199000060622     c*                  SETOFF                                           98
199100060622     c*                  Z-ADD     1             jj
199200060622     c*    dctcch        LOOKUP    scch(jj)                               98
199300060622     C                   RESET                   tibs02ds
199400060622     C                   EVAL      t02Cod = 'CCH'
199500060622     C                   EVAL      t02Ke1 = dctcch
199600060622     C                   EXSR      getTntbe00f
199700060622if  3c*                  IF        NOT *in98                                    *non trovata
199800060622     c                   IF        t02Err = 'E' OR §cchinte <> 'S'              *non trovata
199900000000     c                   MOVEL     '1'           werr                           *SI errore
200000000000e   3C                   ENDIF
200100000000e   2C                   ENDIF
200200000000e   1C                   ENDIF
200300000000     c*
200400000000     c                   ENDSR
200500000000     c*--------------------------------------------------------------------------------------------*
200600000000     c* Imposta l'evento lettera di anomalia
200700000000     c*--------------------------------------------------------------------------------------------*
200800000000     c     evbdct        BEGSR
200900000000     C*
201000000000     c* imposta causale
201100000000if  1c                   IF        wfpr = 'T'
201200000000     c                   MOVEL     '709'         wcev                           *causale evento
201300000000x   1c                   ELSE
201400000000     c                   MOVEL     '710'         wcev                           *causale evento
201500000000e   1c                   ENDIF
201600000000     c                   EXSR      deccevsta                                    *decodifica evento
201700000000     c*
201800000000     c* imposta filiale
201900000000if  1c                   IF        werr <> '1'                                  *no errore
202000000000     c                   Z-ADD     dctfil        worg
202100000000     c                   EXSR      decorg
202200000000     c* imposta data
202300000000     c                   MOVEL     dctaac        wdat
202400000000     c                   MOVE      dctmgc        wdat
202500000000     c                   EXSR      edtdat
202600000000     c* imposta ora
202700000000     c                   MOVEL     1100          wora                           *FISSA
202800000000     c                   EXSR      edtora
202900000000     c*
203000000000     c* incrementa contatore eventi
203100000000     c                   ADD       1             tote
203200000000     c* indice schiera
203300000000     c                   Z-ADD     tote          smi(tote)
203400000000     c* sequenza legame
203500000000     c                   Z-ADD     seql          smseql(tote)
203600000000     c* sequenza evento
203700000000     c                   Z-ADD     999           smseqe(tote)
203800000000     c* descrizione -impostata sopra-
203900030130     c                   MOVEL     wcdex         smdev(tote)
204000000000     c* filiale -impostata sopra-
204100000000     c                   MOVEL     wodes         smfle(tote)
204200000000     c* data    -impostata sopra-
204300000000     c                   MOVEL     wdate         smdeve(tote)
204400000000     c* ora     -impostata sopra-
204500000000     c                   MOVEL     worae         smheve(tote)
204600000000     c* causale -impostata sopra-
204700000000     c                   MOVEL     wcev          smcev(tote)
204800000000     c* data+ora evento
204900000000     c                   MOVEL     wdat          smdevhev(tote)                 *data consegna
205000000000     c                   MOVE      wora          smdevhev(tote)
205100000000e   1c                   ENDIF
205200000000     c*
205300000000     c                   ENDSR
205400000000     c*--------------------------------------------------------------------------------------------*
205500000000     c* imposta i campi della DS di Output dal contrassegno
205600000000     c*--------------------------------------------------------------------------------------------*
205700000000     c     impdsocsb     BEGSR
205800000000     c*
205900000000     c* azzera le variabili di lavoro
206000000000     c*
206100000000     c****
206200000000     c* . Bolla NON locale -> il contrassegno, se c'è, è da prendere dall' ultima figlia
206300000000     c* . Bolla locale     -> il contrassegno, se c'è, è su questa bolla
206400000000     c* NOTE: In ogni caso i campi sono sempre quelli, vince quindi l'ultimo riempimento
206500000000     c****
206600000000     c*
206700000000     c* reperisce se il cliente di entrata è il mittente del contrassegno
206800090903     c*                  SETOFF                                           98
206900090903     c*                  Z-ADD     1             i
207000090903     c*    s_tasccm      LOOKUP    sksc7(i)                               98      *cliente mittente
207100090903if  1c*                  IF        NOT *in98                                      *non trovato
207200090903if  1c                   IF        tis7700o0.flg_kscCcm = 'K'
207300000000     c                   MOVEL     'N'           camito74                         *NO mittente C/A
207400000000x   1c                   ELSE                                                     *trovato
207500000000     c                   MOVEL     'S'           camito74                         *SI mittente C/A
207600000000e   1C                   ENDIF
207700000000     c*---
207800000000     c* memorizza il contrassegno della bolla di Input
207900000000     c*---
208000000000     c                   EVAL      waas = s_tasaas                              *bolla Input
208100000000     c                   EVAL      wlnp = s_taslnp
208200000000     c                   EVAL      wnrs = s_tasnrs
208300000000     c                   EVAL      wnsp = s_tasnsp
208400000000     c                   EVAL      wfca = s_tasfca                               -flag avuto c/a
208500000000     c                   EXSR      memcsb
208600000000     c*
208700000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
208800000000     c                   EXSR      elacsblbl
208900000000     c*
209000000000     c                   ENDSR
209100000000     c*--------------------------------------------------------------------------------------------*
209200000000     c* elabora i contrassegni delle bolle legate (successivamente) alla bolla di input
209300000000     c*--------------------------------------------------------------------------------------------*
209400000000     c     elacsblbl     BEGSR
209500000000     c*
209600000000     c* azzera le variabili di lavoro
209700000000     c                   EVAL      depaan = *zeros                              *dep.ultima filgia
209800000000     c                   EVAL      deplpn = *zeros
209900000000     c                   EVAL      depnrn = *zeros
210000000000     c                   EVAL      depnsn = *zeros
210100000000     c*
210200000000     c* legge i legami delle figlie successive alla bolla di input
210300000000     c                   EVAL      kblaap =  s_tasaas                           *bolla input
210400000000     c                   EVAL      kbllpp =  s_taslnp
210500000000     c                   EVAL      kblnrp =  s_tasnrs
210600000000     c                   EVAL      kblnsp =  s_tasnsp
210700000000     c*
210800000000     c* ciclo fino a fine mamme
210900000000     c     ke2lbl        SETLL     fnlbl02l
211000000000     c     ke2lbl        READE     fnlbl02l                               99
211100000000do  1c                   DOW       NOT *in99
211200000000     c*
211300000000     c* ciclo fino a fine figlie della mamma
211400000000do  2c                   DOW       NOT *in99
211500000000if  3c*---               IF        lbllan <> s_taslna                           *NO figlia locale
211600000000     c*
211700000000     c* legge la bolla dal legame
211800000000     c                   EVAL      kasaas = lblaan                              *figlia
211900000000     c                   EVAL      kaslnp = lbllpn
212000000000     c                   EVAL      kasnrs = lblnrn
212100000000     c                   EVAL      kasnsp = lblnsn
212200000000     c     keytas        CHAIN     titas30c                           99        *legge la 1^
212300000000     c*---
212400000000     c* memorizza il contrassegno della bolla figlia legata
212500000000     c*---
212600000000if  4c                   IF        NOT *in99                                    *esistente
212700000000     c                   EVAL      waas = lblaan                                *figlia
212800000000     c                   EVAL      wlnp = lbllpn
212900000000     c                   EVAL      wnrs = lblnrn
213000000000     c                   EVAL      wnsp = lblnsn
213100000000     c                   EVAL      wfca = tasfca                                 -flag avuto c/a
213200000000     c                   EXSR      memcsb
213300000000e   4c                   ENDIF
213400000000e   3c*---               ENDIF
213500000000     c*
213600000000     c* deposita la figlia (che SE ULTIMA puo' essere una nuova mamma)
213700000000     c                   EVAL      depaan = lblaan                              *ultima figlia
213800000000     c                   EVAL      deplpn = lbllpn
213900000000     c                   EVAL      depnrn = lblnrn
214000000000     c                   EVAL      depnsn = lblnsn
214100000000     c*
214200000000     c* lettura successiva legami
214300000000     c     ke2lbl        READE     fnlbl02l                               99
214400000000e   2c                   ENDDO                                                  *fine figlie mamma
214500000000     c*
214600000000     c* imposta la figlia come nuova mamma e controlla se ha davvero dei figli
214700000000if  2c                   IF        kblaap <> depaan OR                          *NO sempre la stessa
214800000000     c                             kbllpp <> deplpn OR
214900000000     c                             kblnrp <> depnrn OR
215000000000     c                             kblnsp <> depnsn
215100000000     c* nuova chiave
215200000000     c                   EVAL      kblaap =  depaan                             *nuova mamma
215300000000     c                   EVAL      kbllpp =  deplpn
215400000000     c                   EVAL      kblnrp =  depnrn
215500000000     c                   EVAL      kblnsp =  depnsn
215600000000     c* controlla figlie
215700000000     c     ke2lbl        SETLL     fnlbl02l
215800000000     c     ke2lbl        READE     fnlbl02l                               99
215900000000if  3c                   IF        *in99                                        *non ha figlie
216000000000     c                   LEAVE                                                  *esce dal ciclo
216100000000e   3c                   ENDIF
216200000000x   2c                   ELSE                                                   *sempre stessa mamma
216300000000     c                   LEAVE                                                  *esce dal ciclo
216400000000e   2c                   ENDIF
216500000000e   1c                   ENDDO                                                  *fine mamme
216600000000     c*
216700000000     c                   ENDSR
216800000000     c*--------------------------------------------------------------------------------------------*
216900000000     c* memorizza il contrassegno
217000000000     c*--------------------------------------------------------------------------------------------*
217100000000     c     memcsb        BEGSR
217200000000     c*
217300000000     c* legge il contrassegno, se avuto
217400000000if  1c                   IF        wfca = 'S'                                   *avuto c/assegno
217500000000     c                   Z-ADD     waas          ksbaas
217600000000     c                   Z-ADD     wlnp          ksblnp
217700000000     c                   Z-ADD     wnrs          ksbnrs
217800000000     c                   Z-ADD     wnsp          ksbnsp
217900000000     c     keycsb        CHAIN     tncsb03l                           99
218000000000if  2c                   IF        NOT *in99                                    *esistente
218100000000     c* memorizza i dati
218200000000     c                   MOVEL     'S'           cao74                          *esiste C/A
218300000000     c*
218400000000     c                   Z-ADD     csbcas        caimpo74                       *importo
218500000000     c*
218600000000     c                   EVAL      cadivo74 =  csbvca                           *divisa
218700010820if  3c                   IF        cadivo74 = *blanks AND                       *' ' = 'ITL'
218800010820     c                             caimpo74 > *zeros
218900010820     c                   EVAL      cadivo74 = 'ITL'
219000010820e   3c                   ENDIF
219100100119     ** Visualizzo la descrizione del tipo incasso contrassegno solo se il
219200100119     ** contrassegno è stato incassato.
219300100121     C                   EVAL      cainco74 =
219400100121     C                                       GetDescrizioneIncassoContrassegno()
219500000000     c*
219600000000     c                   MOVEL     *blanks       castao74                       *stato
219700060622     C                   EVALR     tblKey = %CHAR(csbSta)
219800060626     C                   EVAL      ds4S = chainTabel00f('CHAIN3':langTabel
219900060626     C                             :'4S':tblKey:%LEN(tblKey):'TBLUNI'
220000060622     C                             :rpyOpCode:rpyEsito)
220100060622     C                   IF        rpyOpCode = 'FOUND'
220200030130ia  4c     savtsp        IFEQ      'P'                                          *bolle poste
220300060622     c*                  EVAL      castao74 = sdstaP(i)
220400060622     c                   EVAL      castao74 = §4SDep
220500030130x   4c                   ELSE                                                   *bolle bartolini
220600060622     c*                  EVAL      castao74 = sdsta(i)
220700060622     c                   EVAL      castao74 = §4SDei
220800030130e   4c                   ENDIF
220900000000e   3c                   ENDIF
221000100119     c
221100000000if  3c                   IF        castao74 = *blanks                           *NO stato partic.re
221200011001IF  4c                   IF        csbddc = *zeros                              *NO distinta incasso
221300060608     c*                  EVAL      castao74 = 'C/ASSEGNO DA INCASSARE'
221400060612     C                   EVAL      castao74 = rtvMsgLang('TIS0048':langI74)
221500000000x   4c                   ELSE
221600000000IF  5c                   IF        csbddp = *zeros                              *NO pagamento
221700060608     c*                  EVAL      castao74 = 'C/ASSEGNO DA PAGARE'
221800060612     C                   EVAL      castao74 = rtvMsgLang('TIS0049':langI74)
221900000000x   5c                   ELSE
222000060608     c*                  EVAL      castao74 = 'C/ASSEGNO PAGATO'
222100060612     C                   EVAL      castao74 = rtvMsgLang('TIS0050':langI74)
222200000000e   5c                   ENDIF
222300000000e   4c                   ENDIF
222400000000e   3c                   ENDIF
222500000000     c*
222600000000     c                   MOVEL     *blanks       capago74                       *tipo pagamento
222700000000if  3c                   IF        csbtpi = 'M'                                 *incasso Mittente
222800000000if  4c                   IF        csbtpa = 'B'
222900060608     c*                  EVAL      capago74 = 'Assegno bancario'
223000060612     C                   EVAL      capago74 = rtvMsgLang('TIS0025':langI74)
223100000000e   4c                   ENDIF
223200000000if  4c                   IF        csbtpa = 'C'
223300060608     c*                  EVAL      capago74 = 'Assegno circolare'
223400060612     C                   EVAL      capago74 = rtvMsgLang('TIS0026':langI74)
223500000000e   4c                   ENDIF
223600000000x   3c                   ELSE                                                   *incasso Bartolini
223700000000if  4c                   IF        csbfpc = *blanks
223800060608     c*                  EVAL      capago74 = 'Assegno di traenza'
223900110304     C                   EVAL      capago74 = rtvMsgLang('TIS0027':langI74)
224000000000e   4c                   ENDIF
224100000000if  4c                   IF        csbfpc = '2'
224200060608     c*                  EVAL      capago74 = 'Bonifico'
224300060612     C                   EVAL      capago74 = rtvMsgLang('TIS0047':langI74)
224400000000e   4c                   ENDIF
224500000000e   3c                   ENDIF
224600000000     c*
224700000000     c                   MOVEL     *blanks       ca2dato74                      *decod. data pagam.
224800000000     c                   MOVEL     *blanks       ca2numo74                      *decod. num. pagam.
224900000000if  3c                   IF        csbtpi = 'M' OR                              *incasso Mittente  O
225000000000     c                             csbtpi = ' ' AND                             *incasso Bartolini E
225100000000     c                             csbfpc = *blanks                              assegno di traenza
225200060608     c*                  EVAL      ca2dato74 = 'Data invio assegno:'
225300060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0129':langI74)
225400000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
225500060608     c*                  EVAL      ca2numo74 = 'ABI CAB n° assegno:'
225600060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0013':langI74)
225700000000x   4c                   ELSE                                                   *inc.Bar E ass.traen
225800060608     c*                  EVAL      ca2numo74 = 'N° assegno'
225900060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0417':langI74)
226000000000e   4c                   ENDIF
226100000000x   3c                   ELSE                                                   *inc.Bar E pag.bonif
226200060608     c*                  EVAL      ca2dato74 = 'Data disposizione:'
226300060612     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0117':langI74)
226400060608     c*                  EVAL      ca2numo74 = 'N° disposizione:'
226500060612     C                   EVAL      ca2numo74 = rtvMsgLang('TIS0418':langI74)
226600000000e   3c                   ENDIF
226700000000     c*
226800000000     c                   MOVEL     *blanks       cadato74                       *data pagamento
226900000000     c                   Z-ADD     *zeros        caabio74                       *abi
227000000000     c                   Z-ADD     *zeros        cacabo74                       *cab
227100000000     c                   MOVEL     *blanks       canumo74                       *n° pagamento
227200000000if  3c                   IF        csbddp > *zeros                              *SOLO SE pagato
227300000000     c                   MOVEL     csbddp        wdat
227400000000     c                   EXSR      edtdat
227500000000     c                   EVAL      cadato74 = wdate
227600000000     C*
227700000000if  4c                   IF        csbtpi = 'M'                                 *incasso Mittente
227800000000     c                   Z-ADD     csbabi        caabio74
227900000000     c                   Z-ADD     csbcab        cacabo74
228000000000     c                   MOVEL     csbnra        canumo74
228100120321     c                   CALLP     SetIdAssegnoMittente(caNumO74 : caAbiO74
228200120321     c                             : caCabO74)
228300000000x   4c                   ELSE                                                   *incasso Bartolini
228400000000     c                   MOVEL     csbndp        canumo74
228500000000e   4c                   ENDIF
228600000000e   3c                   ENDIF
228700110304     ** Il contrassegno non è stato rimborsato ma girato in compensazione con
228800110304     ** il debito del cliente; aggiusto le descrizioni relative al rimborso.
228900110307     C                   IF        IsContrassegnoCompensato()
229000110307     C                   EVAL      capago74 = rtvMsgLang('TIS0696':langI74)     Compens.ne con fatt.
229100110304     C                   EVAL      ca2dato74 = rtvMsgLang('TIS0697':langI74)    Data compensazione
229200110304     C                   CLEAR                   caabio74
229300110304     C                   CLEAR                   cacabo74
229400110304     C                   CLEAR                   canumo74
229500110304     C                   ENDIF
229600110304     C*
229700000000e   2c                   ENDIF
229800000000e   1c                   ENDIF
229900000000     c*
230000000000     c                   ENDSR
230100000000     c*--------------------------------------------------------------------------------------------*
230200000000     c* imposta i campi della DS di Output dalla giacenza
230300000000     c*--------------------------------------------------------------------------------------------*
230400000000     c     impdsogcp     BEGSR
230500000000     c*
230600000000     c* memorizza l'ultima giacenza della bolla di Input (progressivo apertura = 0)
230700000000     c                   EVAL      waas = s_tasaas                              *bolla Input
230800000000     c                   EVAL      wlnp = s_taslnp
230900000000     c                   EVAL      wnrs = s_tasnrs
231000000000     c                   EVAL      wnsp = s_tasnsp
231100000000     c                   EVAL      wfgc = s_tasfgc                              -flag avuto giacenza
231200000000     c                   EXSR      memgcp
231300000000     c*
231400000000     c                   ENDSR
231500000000     c*--------------------------------------------------------------------------------------------*
231600000000     c* memorizza la giacenza
231700000000     c*--------------------------------------------------------------------------------------------*
231800000000     c     memgcp        BEGSR
231900000000     c*
232000000000     c* imposta le variabili di lavoro
232100000000     c                   CLEAR                   i                              *indice
232200000000     c                   CLEAR                   ii
232300000000     c                   CLEAR                   smdmc                          *note fase -20-
232400000000     c                   CLEAR                   smdmcR
232500000000     c                   CLEAR                   gccm2o74                       *nota fase -10-
232600000000     c                   MOVEL     'N'           gco74                          *flag di controllo
232700000000     c                   MOVEL     'N'           gcfdio74
232800000000     c                   MOVEL     'N'           gcmito74
232900000000     c*
233000000000     c* legge la giacenza, se avuta
233100000000     c                   Z-ADD     waas          kcpaas
233200000000     c                   Z-ADD     wlnp          kcplnp
233300000000     c                   Z-ADD     wnrs          kcpnrs
233400000000     c                   Z-ADD     wnsp          kcpnsp
233500000000     c                   Z-ADD     *zeros        kcpfrg
233600050309     c     keygcp        CHAIN     tigcp51l                           99
233700050419     **?Alla prima apertura ignoro la giacenza fin quando raggiunge la fase 20, perchè è inutile
233800050419     **?far vedere all'utente la giacenza se poi non può immettere le disposizioni.
233900000000if  2c                   IF        NOT *in99        AND                         *esistente
234000000000     c                             gcpatb = *blanks AND                         *no annullata
234100050415     c                             gcptcm <> 'Z'    AND
234200050415if  5C                             (gcpFas > 19 OR gcpRiap = 'R')
234300050419     C
234400050419     C                   EVAL      gcRiapO74 = gcpRiap
234500050418     **?Descrizione della fase.
234600050418     C                   SELECT
234700050418     C                   WHEN      gcpFas < 20 AND gcpRiap = 'R'
234800060612     C*                  EVAL      gcFasO74 = 'Comunicazione in fase di invio'
234900060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0076':langI74)
235000050418     C                   WHEN      gcpFas = 20
235100060612     C*                  EVAL      gcFasO74 = 'In attesa di disposizioni'
235200060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0279':langI74)
235300050419     C                   WHEN      gcpFas > 20 AND tasDcm = 0
235400060612     C*                  EVAL      gcFasO74 = 'Disposizioni in esecuzione'
235500060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0154':langI74)
235600050419     C                   WHEN      gcpFas > 20 AND tasDcm > 0
235700060612     C*                  EVAL      gcFasO74 = 'Evasa'
235800060612     C                   EVAL      gcFasO74 = rtvMsgLang('TIS0185':langI74)
235900050418     C                   ENDSL
236000000000     c* memorizza i dati dalle NOTE GIACENZE
236100000000     c                   Z-ADD     *zeros        i
236200000000     c                   Z-ADD     *zeros        ii
236300000000     c                   Z-ADD     gcpagc        knpagc
236400000000     c                   Z-ADD     gcpfgc        knpfgc
236500000000     c                   Z-ADD     gcpngc        knpngc
236600000000     c                   Z-ADD     gcpfrg        knpfrg
236700050309     c     keygnp        SETLL     tignp01l
236800050309     c     keygnp        READE     tignp01l                               98
236900000000do  3c                   DOW       NOT *in98
237000050404if  4c                   IF        gnptpn <> 'I'                                *NO note interne
237100000000     c* fase -10-
237200000000if  5C                   IF        gnpfas = 10 AND                              *APERTURA
237300000000     c                             gccm2o74 = *blanks                           *solo 1^parte motiv.
237400000000     c                   EVAL      gccm2o74 = gnpdmc                            *motivaz.apert.giac.
237500000000e   5c                   ENDIF
237600000000     c* fase -20-
237700000000if  5C                   IF        gnpfas = 20                                  *IMMISSIONE DISPOSIZ
237800000000if  6c                   IF        gnptpn = *blanks AND                         *immesse da filiale
237900051014     C                             i < 5 AND gnpdmc <> *BLANK                   *NON più di 5 note
238000000000     c                   ADD       1             i
238100000000     c                   MOVEL     gnpdmc        smdmc(i)                       *nota
238200000000e   6c                   ENDIF
238300000000if  6c                   IF        gnptpn = 'R' AND                             *ricevute da cliente
238400051014     C                             ii < 5 AND gnpdmc <> *BLANK                  *NON più di 5 note
238500050405     C                   IF        %SUBST(GNPDMC:1:20) = 'DATA DISPOS.CLIENTE:'
238600050405     c                   CLEAR                   ii
238700050405     c                   CLEAR                   smdmcR
238800050919     C                   EVAL      wrkDtInvDisp = %SUBST(gnpDmc:21:8)
238900050919     C     *ISO0         TEST(DE)                wrkDtInvDisp
239000050919     C                   IF        NOT %ERROR
239100050919     C     *ISO0         MOVE      wrkDtInvDisp  gcDtInvO74
239200050919     C                   ENDIF
239300050405     C                   ELSE
239400000000     c                   ADD       1             ii
239500000000     c                   MOVEL     gnpdmc        smdmcR(ii)                     *nota
239600050405     C                   ENDIF
239700000000e   6c                   ENDIF
239800000000e   5c                   ENDIF
239900000000e   4c                   ENDIF
240000050309     c     keygnp        READE     tignp01l                               98
240100000000e   3c                   ENDDO
240200000000     c*
240300000000     c* memorizza i dati dalle GIACENZE
240400000000     c                   MOVEL     'S'           gco74                          *esiste giacenza
240500050404     c* Disposizione da immettere.
240600051110     c                   IF        gcpfas = 20
240700051110     c                   MOVEL     'S'           gcfdio74                       *stato disposizioni
240800051110     C                   EXSR      chkTivpi
240900051110e   3c                   ENDIF
241000000000     c*
241100000000     c                   Z-ADD     gcpfgc        gcfgco74                       *filiale apertura
241200000000     c*
241300000000     c                   Z-ADD     gcpngc        gcngco74                       *numero giacenza
241400000000     c*
241500060103     c                   Z-ADD     gcpagc        dsannB
241600060103     c                   Z-ADD     gcpmgc        dsmgsB
241700060103     c                   Z-ADD     dsdatB        wdat
241800000000if  3c                   IF        wdat > *zeros
241900000000     c                   EXSR      edtdat
242000000000     c                   EVAL      gcdago74 = wdate                             *data apertura
242100000000e   3C                   ENDIF
242200060104     c*
242300060104     c                   Z-ADD     gcpdur        wdat
242400060104     c                   IF        wdat > *zeros
242500060104     c                   EXSR      edtdat
242600060104     C                   EVAL      gcduro74 = wdate                             Data ultima riapert.
242700060104     C                   ENDIF
242800000000     c*
242900000000if  3c                   IF        gcpcmc <> *blanks
243000000000     c                   MOVEL     gcpcmc        wcev
243100000000     c                   EXSR      deccevgia
243200030130     c                   MOVEL     wcdex         gccmco74                       *motivo apertura
243300000000e   3C                   ENDIF
243400000000     c*
243500000000     c                   MOVEL     *blanks       gcdcgo74
243600000000if  3c                   IF        gcpdcg > *zeros
243700000000     c                   Z-ADD     gcpdcg        wdat
243800000000     c                   EXSR      edtdat
243900000000     c                   EVAL      gcdcgo74 = wdate                             *data chiusura
244000000000e   3c                   ENDIF
244100000000     c*---
244200000000     c* Disposizioni già immesse
244300000000     c*---
244400000000if  3c                   IF        gcfdio74 = 'N'                               *dispos.già immesse
244500000000     c*
244600000000if  4c                   IF        gcpddm > *zeros
244700000000     c                   Z-ADD     gcpddm        wdat
244800000000     c                   EXSR      edtdat
244900000000     c                   EVAL      gcddmo74 = wdate                             *dt.immiss.disposiz.
245000000000e   4C                   ENDIF
245100000000     c*
245200000000if  4c                   IF        gcpdis <> *blanks
245300060620     c*                  SETOFF                                           98
245400060620     c*                  Z-ADD     1             i
245500060620     c*    gcpdis        LOOKUP    sdis(i)                                98
245600060620if  5c*                  IF        *in98
245700060620     c*                  EVAL      gcdiso74 = sddis(i)                          *disposizione
245800060620e   5C*                  ENDIF
245900060626     C                   EVAL      ds2D = chainTabel00f('CHAIN3':langTabel
246000060626     C                             :'2D':gcpdis:%LEN(gcpdis):'TBLUNI'
246100060626     C                             :rpyOpCode:rpyEsito)
246200060622     C                   EVAL      gcdiso74 = §2DDes
246300000000e   4C                   ENDIF
246400000000     c*
246500000000if  4c                   IF        gcpasg <> *blanks
246600000000if  5c                   IF        gcpasg = 'S'
246700060612     c*                  EVAL      gcasgo74 = 'SI'                              *addebito spese
246800060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0690':langI74)     *addebito spese
246900000000x   5c                   ELSE
247000060612     c*                  EVAL      gcasgo74 = 'NO'                              *addebito spese
247100060612     c                   EVAL      gcasgo74 = rtvMsgLang('TIS0691':langI74)     *addebito spese
247200000000e   5C                   ENDIF
247300000000     c*
247400000000if  5c                   IF        gcppsg = 'M'
247500060612     c*                  EVAL      gcpsgo74 = 'al Mittente'                     *a chi addebito
247600060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0638':langI74)    *a chi addebito
247700000000e   5C                   ENDIF
247800000000if  5c                   IF        gcppsg = 'D'
247900060612     c*                  EVAL      gcpsgo74 = 'al Destinatario'                 *a chi addebito
248000060612     c                   EVAL      gcpsgo74 =  rtvMsgLang('TIS0637':langI74)    *a chi addebito
248100000000e   5C                   ENDIF
248200000000e   4C                   ENDIF
248300000000     c*
248400000000if  4c                   IF        gcpvcs = 'S'                                 *variazione CAssegno
248500000000     c                   MOVEL     gcpvcs        gcvcso74
248600000000     c                   MOVEL     gcpvca        gcvcao74                       *valuta C/Assegno
248700000000     c                   Z-ADD     gcpcas        gccaso74
248800000000if  5c                   IF        gcpcas = 0                                   *C/A annullato
248900000000     c                   EVAL      gcvcao74 = '0  '                             *x vedere variaz.ne
249000000000e   5C                   ENDIF
249100000000e   4C                   ENDIF
249200000000     c*
249300050404     C*                  EVAL      a200 = smdmcR(1) +                           *note CLIENTI
249400050404     C*                                   smdmcR(2) +
249500050404     C*                                   smdmcR(3) +
249600050404     C*                                   smdmcR(4)
249700050404     C*                  EVAL      a200bis = smdmc(1) +                         *note FILIALE
249800050404     C*                                   smdmc(2) +
249900050404     C*                                   smdmc(3) +
250000050404     C*                                   smdmc(4)
250100050404     C*                  EVAL      a400= %TRIM(a200) + ' ' +                    *note
250200050404     C*                                   %TRIM(a200bis)
250300050404     C*                  MOVEL     a400          gcdmcO74
250400050404     C                   IF        smdmc(1) <> *BLANK
250500050404     C                   EVAL      gcdmcO74 = smdmc(1) + smdmc(2) +             *note FILIALE
250600050404     C                             smdmc(3) + smdmc(4)
250700051014     C                   EVAL      gcdmc5O74 = smdmc(5)
250800050404     C                   ELSE
250900050404     C                   EVAL      gcdmcO74 = smdmcr(1) + smdmcr(2) +           *note CLIENTI
251000050404     C                             smdmcr(3) + smdmcr(4)
251100051014     C                   EVAL      gcdmc5O74 = smdmcr(5)
251200050404e   3C                   ENDIF
251300000000e   3C                   ENDIF
251400000000     c*---
251500000000     c* Disposizioni da immettere
251600000000     c*---
251700010719     c* se le disposizioni sono da immettere controlla che:
251800010719     c* - il cliente è il mittente della giacenza
251900000000if  3c                   IF        gcfdio74 = 'S'                               *dispos.da immettere
252000040726if  4C                   IF        keygiac <> *blanks
252100040726     c                   MOVEL     'S'           gcmito74                       SI mittente GIACENZA
252200040726x   4c                   ELSE
252300090903     c*                  SETOFF                                           98
252400090903     c*                  Z-ADD     1             i
252500090903     c*    s_tasccm      LOOKUP    sksc7(i)                               98
252600090903if  5c*                  IF        *in98
252700090914if  1c*                  IF        tis7700o0.flg_kscCcm = 'E' OR
252800090914if  1c*                            tis7700o0.flg_kscCcm = 'C'
252900090914     c*                  MOVEL     'S'           gcmito74                       SI mittente GIACENZA
253000090914e   5C*                  ENDIF
253100090914     C                   CALLP     Verifica_gestione_giacenza( 'GETFLGGIAC'
253200090914     C                             : s_tasAas : s_tasLnp : s_tasNrs : s_tasNsp
253300090914     C                             : s_tasTbl : kscI74 : %EDITC(rqsCidI174:'X')
253400090914     C                             : tis7700o0.flg_kscCcm : gcMitO74 : rpyEsito
253500090914     C                             )
253600040726e   4C                   ENDIF
253700000000e   3C                   ENDIF
253800050404     C* Controllo esistenza storia giacenza.
253900050404     C* Se esiste più di 1 record attivo il link per visualizzare la storia della giacenza.
254000050404     C                   DO        *HIVAL
254100050404     C     K04GCP51      READE     TIGCP51L
254200050404     C                   IF        %EOF
254300050404     C                   LEAVE
254400050404     C                   ENDIF
254500050404     C                   IF        GCPATB = *BLANK
254600050404     C                   EVAL      StoGiaO74 = 'S'
254700050404     C                   LEAVE
254800050404     C                   ENDIF
254900050404     C                   ENDDO
255000050404     c*
255100000000e   2c                   ENDIF
255200000000     c*
255300000000     c                   ENDSR
255400000000     c*--------------------------------------------------------------------------------------------*
255500000000     c* imposta i campi della DS di Output dalle note
255600000000     c*--------------------------------------------------------------------------------------------*
255700000000     c     impdsonot     BEGSR
255800000000     c*
255900000000     c* azzera le variabili di lavoro
256000000000     c                   CLEAR                   i
256100000000     c                   CLEAR                   mdcre                          consegna richiesta
256200000000     c                   CLEAR                   mhcre
256300030210     c                   CLEAR                   mtcre
256400000000     c                   CLEAR                   mftce                          consegna particolare
256500000000     c                   CLEAR                   wftc
256600000000     c                   CLEAR                   d1wdftc
256700000000     c                   CLEAR                   d2wdftc
256800000000     c                   CLEAR                   mgcxe                          turni chiusura
256900000000     c                   CLEAR                   wgcx
257000000000     c                   CLEAR                   d1wdgcx
257100000000     c                   CLEAR                   d2wdgcx
257200010927     c                   CLEAR                   mffd                           fermo deposito
257300010927     c                   CLEAR                   mmncB                          *motivi NON consegna
257400010927     c                   CLEAR                   mmncC
257500010927     c                   CLEAR                   mmncG
257600010927     c                   CLEAR                   mmncH
257700010927     c                   CLEAR                   mmncN
257800010927     c                   CLEAR                   mmncO
257900050701     c                   CLEAR                   mmnc8
258000050701     c                   CLEAR                   mmnc9
258100050701     c                   CLEAR                   a230
258200000000     c                   CLEAR                   we
258300030206     c                   CLEAR                   mtel                           n° tel destinatario
258400030206     c                   CLEAR                   mban                           bancali
258500030207     c                   CLEAR                   mscr                           supermercati
258600030210     c                   CLEAR                   msdace
258700030210     c                   CLEAR                   msorce
258800030210     c                   CLEAR                   msdcre
258900030210     c                   CLEAR                   mshcre
259000030210     c                   CLEAR                   mstcre
259100030210     c                   CLEAR                   msnom
259200030210     c                   CLEAR                   mscrT
259300030210     c                   CLEAR                   wmscrT
259400030210     c                   CLEAR                   s
259500030207     c
259600000000     c*---
259700000000     c* memorizza le "note" che sono presenti nel record bolla
259800000000     c*---
259900000000     c* Consegna richiesta
260000000000if  1c                   IF        s_tasdcr = *zeros AND                        *NO cons.particolare
260100000000     c                             s_tashcr = *zeros
260200000000x   1c                   ELSE                                                   *SI cons.particolare
260300000000if  2c                   IF        s_tasdcr > *zeros                            *data richiesta
260400000000     c                   MOVEL     s_tasdcr      wdat
260500000000     c                   EXSR      edtdat                                       *edita data
260600000000     c                   MOVEL     wdate         mdcre
260700000000e   2c                   ENDIF
260800000000if  2c                   IF        s_tashcr > *zeros                            *ora  richiesta
260900000000     c                   MOVEL     s_tashcr      wora
261000000000     c                   EXSR      edtora                                       *edita ora
261100000000     c                   MOVEL     worae         mhcre
261200000000e   2c                   ENDIF
261300000000if  2c                   IF        s_tasdcr > *zeros AND                        *SI data richiesta
261400000000     c                             s_tashcr = *zeros OR                         *NO ora  richiesta
261500000000     c                             s_tasdcr > *zeros AND                        *SI data richiesta
261600000000     c                             s_tashcr > *zeros                            *SI ora  richiesta
261700000000if  3c                   IF        s_tastcr = 'P'                               *prima
261800060612     c*                  EVAL      mtcre = 'Consegna richiesta prima del '
261900060612     c                   EVAL      mtcre =  rtvMsgLang('TIS0089':langI74)
262000000000e   3c                   ENDIF
262100000000if  3c                   IF        s_tastcr = 'D'                               *dopo
262200060612     c*                  EVAL      mtcre = 'Consegna richiesta dopo il '
262300060612     c                   EVAL      mtcre =  rtvMsgLang('TIS0086':langI74)
262400000000e   3c                   ENDIF
262500000000if  3c                   IF        s_tastcr = ' '                               *il
262600060612     c*                  EVAL      mtcre = 'Consegna richiesta il '
262700060612     c                   EVAL      mtcre =  rtvMsgLang('TIS0088':langI74)
262800000000e   3c                   ENDIF
262900000000e   2c                   ENDIF
263000000000if  2c                   IF        s_tashcr > *zeros AND                        *SI ora  richiesta
263100000000     c                             s_tasdcr = *zeros                            *NO data richiesta
263200000000if  3c                   IF        s_tastcr = 'P'                               *prima
263300060612     c*                  EVAL      mtcre = 'Consegna richiesta prima delle '
263400060612     c                   EVAL      mtcre =  rtvMsgLang('TIS0090':langI74)
263500000000e   3c                   ENDIF
263600000000if  3c                   IF        s_tastcr = 'D'                               *dopo
263700060612     c*                  EVAL      mtcre = 'Consegna richiesta dopo le '
263800060612     c                   EVAL      mtcre =  rtvMsgLang('TIS0087':langI74)
263900000000e   3c                   ENDIF
264000000000if  3c                   IF        s_tastcr = ' '                               *il
264100060612     c*                  EVAL      mtcre = 'Consegna richiesta alle '
264200060612     c                   EVAL      mtcre =  rtvMsgLang('TIS0085':langI74)
264300000000e   3c                   ENDIF
264400000000e   2c                   ENDIF
264500000000     c*
264600010927if  2c                   IF        i < 10                                       *se c'è spazio
264700000000     c                   ADD       1             i                              *memorizza nota
264800000000     c                   EVAL      smnot(i) =
264900000000     c                             %TRIM(mtcre) +
265000000000     c                             ' '          +
265100000000     c                             %TRIM(mdcre) +
265200000000     c                             ' '          +
265300000000     c                             %TRIM(mhcre)
265400010927e   2c                   ENDIF
265500010927e   1c                   ENDIF
265600000000     c*
265700000000     c* Particolarità consegna -uno/due-
265800000000if  1c                   IF        s_tasftc <> *blanks OR
265900000000     c                             s_tastc2 <> *blanks
266000000000     c                   MOVEL     s_tasftc      wftc                           *particolarità uno
266100000000     c                   EXSR      decftc
266200000000     c                   MOVEL     wdftc         d1wdftc
266300000000     c                   MOVEL     s_tastc2      wftc                           *particolarità due
266400000000     c                   EXSR      decftc
266500000000     c                   MOVEL     wdftc         d2wdftc
266600000000if  2c                   IF        d1wdftc <> *blanks  AND                      *2 particolarità
266700000000     c                             d2wdftc <> *blanks                           *aggiunta ' e'
266800060612     c*                  EVAL      we = ' e '
266900060612     c                   EVAL      we = ' ' +
267000060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
267100000000x   2c                   ELSE
267200000000     c                   EVAL      we = *blanks
267300000000e   2c                   ENDIF
267400060612     c*                  EVAL      mftce =
267500060612     c*                            'Particolarità consegna: ' +
267600060612     c*                            %TRIM(d1wdftc)             +
267700060612     c*                            we                         +
267800060612     c*                            %TRIM(d2wdftc)
267900060612     c                   EVAL      mftce =
268000060613     c                             %TRIMR(rtvMsgLang('TIS0442':langI74)) + ' ' +
268100060612     c                             %TRIM(d1wdftc)             +
268200060612     c                             we                         +
268300060612     c                             %TRIM(d2wdftc)
268400000000     c*
268500010927if  2c                   IF        i < 10                                       *se c'è spazio
268600000000     c                   ADD       1             i                              *memorizza nota
268700000000     c                   EVAL      smnot(i) = mftce
268800010927e   2c                   ENDIF
268900010927e   1c                   ENDIF
269000000000     c*
269100000000     c* Chiusura per turno
269200000000if  1c                   IF        s_tasgc1 <> *blanks OR
269300000000     c                             s_tasgc2 <> *blanks
269400000000if  2c                   IF        s_tasgc1 <> *blanks
269500000000     c                   MOVEL     s_tasgc1      wgcx                           *turno chiusura uno
269600000000     c                   EXSR      decgcx
269700000000     c                   MOVEL     wdgcx         d1wdgcx
269800000000e   2c                   ENDIF
269900000000if  2c                   IF        s_tasgc2 <> *blanks
270000000000     c                   MOVEL     s_tasgc2      wgcx                           *turno chiusura due
270100000000     c                   EXSR      decgcx
270200000000     c                   MOVEL     wdgcx         d2wdgcx
270300000000e   2c                   ENDIF
270400000000if  2c                   IF        d1wdgcx <> *blanks  AND                      *2 particolarità
270500000000     c                             d2wdgcx <> *blanks                           *aggiunta ' e'
270600060612     c*                  EVAL      we = ' e '
270700060612     c                   EVAL      we = ' ' +
270800060613     c                             %TRIMR(rtvMsgLang('TIS0692':langI74)) + ' '
270900000000x   2c                   ELSE
271000000000     c                   EVAL      we = *blanks
271100000000e   2c                   ENDIF
271200060612     c*                  EVAL      mgcxe =
271300060612     c*                            'Chiuso per turno: ' +
271400060612     c*                            %TRIM(d1wdgcx)       +
271500060612     c*                            we                   +
271600060612     c*                            %TRIM(d2wdgcx)
271700060612     c                   EVAL      mgcxe =
271800060613     c                             %TRIMR(rtvMsgLang('TIS0062':langI74)) + ' ' +
271900060612     c                             %TRIM(d1wdgcx)       +
272000060612     c                             we                   +
272100060612     c                             %TRIM(d2wdgcx)
272200000000     c*
272300010927if  2c                   IF        i < 10                                       *se c'è spazio
272400000000     c                   ADD       1             i                              *memorizza nota
272500000000     c                   EVAL      smnot(i) = mgcxe
272600010927e   2c                   ENDIF
272700010927e   1c                   ENDIF
272800000000     c*
272900000000     c* Fermo deposito
273000000000if  1c                   IF        s_tasffd = 'S'
273100060612     c*                  EVAL      mffd = 'Merce in fermo deposito'
273200060612     c                   EVAL      mffd = rtvMsgLang('TIS0355':langI74)
273300000000     c*
273400010927if  2c                   IF        i < 10                                       *se c'è spazio
273500010927     c                   ADD       1             i                              *memorizza nota
273600010927     c                   EVAL      smnot(i) = mffd
273700010927e   2c                   ENDIF
273800010927e   1c                   ENDIF
273900030206     c*---
274000030206     c* memorizza le "note" che NON sono presenti nel record bolla
274100030206     c* ma sul file estensione "anagrafiche"
274200030206     c*---
274300030206     c* N° telefono destinatario
274400030206     c                   Z-ADD     s_tasaas      kaaaas
274500030206     c                   Z-ADD     s_taslnp      kaalnp
274600030206     c                   Z-ADD     s_tasnrs      kaanrs
274700030206     c                   Z-ADD     s_tasnsp      kaansp
274800030206     c                   MOVEL     'O'           kaatrc
274900030206     c     keytaa        CHAIN     titaa30c                           98
275000030206if  1c                   IF        NOT *in98
275100040330if  2c*                  IF        %SUBST(taarsc:1:4) = 'Tel:'
275200040330     c*                  EVAL      mtel = %SUBST(taarsc:1:19)
275300030206     c*
275400040330if  3c*                  IF        i < 10                                       *se c'è spazio
275500040330     c*                  ADD       1             i                              *memorizza nota
275600040330     c*                  EVAL      smnot(i) = mtel
275700040330e   3c*                  ENDIF
275800040330e   2c*                  ENDIF
275900040330     C                   EVAL      MitOrigO74 = TAARSC
276000030206e   1c                   ENDIF
276100030206     c*---
276200030206     c* memorizza le "note" che NON sono presenti nel record bolla
276300030206     c* ma sul file estensione testata boolle
276400030206     c*---
276500030206     c* Bancali
276600030206     c                   Z-ADD     s_tasaas      kr5aas
276700030206     c                   Z-ADD     s_taslnp      kr5lnp
276800030206     c                   Z-ADD     s_tasnrs      kr5nrs
276900030206     c                   Z-ADD     s_tasnsp      kr5nsp
277000030206     c                   MOVEL     'BAN'         kr5trd
277100040531     c     keyar5        CHAIN     fiar531c                           98
277200030206if  1c                   IF        NOT *in98
277300030206     c                   MOVEL     ar5uni        dar5ban
277400060612     c*                  EVAL      mban = 'Bancali: ' +
277500060612     C*                            %trim(%editc(§ar5nba+§ar5nb2:'Z'))
277600060613     c                   EVAL      mban = %TRIMR(rtvMsgLang('TIS0043':langI74))
277700060612     C                             + ' ' + %trim(%editc(§ar5nba+§ar5nb2:'Z'))
277800030206if  2c                   IF        §ar5rb1 > *zeros OR
277900030206     c                             §ar5rb2 > *zeros
278000060613     c*                  EVAL      mban = %trim(mban) +
278100060613     C*                            ' ma non restituiti: ' +
278200060613     C*                            %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
278300060613     c                   EVAL      mban = %trim(mban) +
278400060613     C                             ' ' + %TRIMR(rtvMsgLang('TIS0665':langI74))
278500060613     C                             + ' ' + %trim(%editc(§ar5rb1+§ar5rb2:'Z'))
278600030206e   2c                   ENDIF
278700030206     c*
278800030206if  2c                   IF        i < 10                                       *se c'è spazio
278900030206     c                   ADD       1             i                              *memorizza nota
279000030206     c                   EVAL      smnot(i) = mban
279100030206e   2c                   ENDIF
279200030206e   1c                   ENDIF
279300040330
279400040330     **?Reperisco referente
279500030206     c* Note supermercati
279600030210if  1c     s_tasftc      IFEQ      'S'
279700030206     c     s_tastc2      OREQ      'S'
279800030210     c                   CLEAR                   s                              *indice
279900030206     c                   Z-ADD     s_tasaas      kr5aas
280000030206     c                   Z-ADD     s_taslnp      kr5lnp
280100030206     c                   Z-ADD     s_tasnrs      kr5nrs
280200030206     c                   Z-ADD     s_tasnsp      kr5nsp
280300030206     c                   MOVEL     'SCR'         kr5trd
280400040531     c     keyar5        SETLL     fiar531c
280500040531     c     keyar5        READE     fiar531c                               98
280600030210do  2c                   DOW       NOT *in98
280700030206     c                   MOVEL     ar5uni        dar5scr
280800030207     c* data nota
280900030210if  3c                   IF        ar5dac > *zeros
281000030207     c                   MOVEL     ar5dac        wdat
281100030207     c                   EXSR      edtdat                                       *edita data
281200030210     c                   MOVEL     wdate         msdace
281300030210e   3c                   ENDIF
281400030207     c* ora nota
281500030210if  3c                   IF        ar5orc > *zeros
281600030207     c                   MOVEL     ar5orc        wora
281700030207     c                   EXSR      edtora                                       *edita ora
281800030210     c                   MOVEL     worae         msorce
281900030210e   3c                   ENDIF
282000030207     c* Consegna richiesta
282100030210if  3c                   IF        §ar5dcr = *zeros AND
282200030207     c                             §ar5hcr = *zeros
282300030210x   3c                   ELSE
282400030210if  4c                   IF        §ar5dcr > *zeros
282500030210     c                   MOVEL     §ar5dcr       wdat
282600030207     c                   EXSR      edtdat                                       *edita data
282700030210     c                   MOVEL     wdate         msdcre
282800030210e   4c                   ENDIF
282900030210if  4c                   IF        §ar5hcr > *zeros
283000030210     c                   MOVEL     §ar5hcr       wora
283100030207     c                   EXSR      edtora                                       *edita ora
283200030210     c                   MOVEL     worae         mshcre
283300030210e   4c                   ENDIF
283400030210if  4c                   IF        §ar5dcr > *zeros AND                         *SI data richiesta
283500030210     c                             §ar5hcr = *zeros OR                          *NO ora  richiesta
283600030210     c                             §ar5dcr > *zeros AND                         *SI data richiesta
283700030210     c                             §ar5hcr > *zeros                             *SI ora  richiesta
283800030210if  5c                   IF        §ar5tcr = 'P'                                *prima
283900060613     c*                  EVAL      mstcre = 'Consegna richiesta prima del'
284000060613     c                   EVAL      mstcre = rtvMsgLang('TIS0089':langI74)
284100030210e   5c                   ENDIF
284200030210if  5c                   IF        §ar5tcr = 'D'                                *dopo
284300060613     c*                  EVAL      mstcre = 'Consegna richiesta dopo il'
284400060613     c                   EVAL      mstcre = rtvMsgLang('TIS0086':langI74)
284500030210e   5c                   ENDIF
284600030210if  5c                   IF        §ar5tcr = ' '                                *il
284700060613     c*                  EVAL      mstcre = 'Consegna richiesta il'
284800060613     c                   EVAL      mstcre = rtvMsgLang('TIS0088':langI74)
284900030210e   5c                   ENDIF
285000030210e   4c                   ENDIF
285100030210if  4c                   IF        §ar5hcr > *zeros AND                         *SI ora  richiesta
285200030210     c                             §ar5dcr = *zeros                             *NO data richiesta
285300030210if  5c                   IF        §ar5tcr = 'P'                                *prima
285400060613     c*                  EVAL      mstcre = 'Consegna richiesta prima delle'
285500060613     c                   EVAL      mstcre = rtvMsgLang('TIS0090':langI74)
285600030210e   5c                   ENDIF
285700030210if  5c                   IF        §ar5tcr = 'D'                                *dopo
285800060613     c*                  EVAL      mstcre = 'Consegna richiesta dopo le'
285900060613     c                   EVAL      mstcre = rtvMsgLang('TIS0087':langI74)
286000030210e   5c                   ENDIF
286100030210if  5c                   IF        §ar5tcr = ' '                                *il
286200060613     c*                  EVAL      mstcre = 'Consegna richiesta alle'
286300060613     c                   EVAL      mstcre = rtvMsgLang('TIS0085':langI74)
286400030210e   5c                   ENDIF
286500030210e   4c                   ENDIF
286600030210e   3c                   ENDIF
286700030210     c* Riferimento
286800030210if  3c                   IF        §ar5nom <>  *blanks
286900060613     c*                  EVAL      msnom = 'Parlato con ' + §ar5nom
287000060613     c                   EVAL      msnom = %TRIMR(rtvMsgLang('TIS0441':langI74))
287100060613     c                             + ' ' + §ar5nom
287200030210e   3c                   ENDIF
287300030210     c*
287400030210if  3c                   IF        s < 5                                        *MAX 5 note
287500030210     c                   ADD       1             s
287600030210     c                   EVAL      mscrT(s) = 'Il ' +
287700030210     c                             %trim(msdace) +                              *data nota
287800030210     c                             ' '    +
287900030210     c                             %trim(msorce) +                              *ora nota
288000030210     c                             ' '    +
288100030210     c                             %trim(mstcre) +                              *tipo consegna
288200030210     c                             ' '    +
288300030210     c                             %trim(msdcre) +                              *data consegna
288400030210     c                             ' '    +
288500030210     c                             %trim(mshcre) +                              *ora consegna
288600030210     c                             ' '    +
288700030210     c                             %trim(msnom)  +                              *parlato con
288800030210     c                             ' '    +
288900030210     c                             %trim(§ar5tel) +                             *telefono
289000030210     c                             ' '    +
289100030210     c                             %trim(§ar5mot)                               *motivo
289200030210e   2c                   ENDIF
289300030210     c*
289400040531     c     keyar5        READE     fiar531c                               98
289500030206e   2c                   ENDDO
289600030210e   1c                   ENDIF
289700030210     c* NOTA completa
289800030210if  1c                   IF        mscrT(1) <> *blanks
289900030211     c                   EVAL      wmscrT = %trim(mscrT(1)) +
290000030211     c                                      ' ' +
290100030211     C                                      %trim(mscrT(2)) +
290200030211     c                                      ' ' +
290300030211     C                                      %trim(mscrT(3)) +
290400030211     c                                      ' ' +
290500030211     C                                      %trim(mscrT(4)) +
290600030211     c                                      ' ' +
290700030211     C                                      %trim(mscrT(5))
290800030210     c                   Z-ADD     *zeros        s
290900030211do  2c                   DOW       (s+1) < %len(wmscrT) AND i < 10
291000030210     c                   ADD       1             i
291100030211     c                   EVAL      smnot(i) = %trim(%subst(wmscrT:s+1:100))
291200030210     c                   EVAL      s = s + 100
291300030210e   2c                   ENDDO
291400030210     c*
291500030210     c*
291600030210e   1c                   ENDIF
291700010927     c*---
291800010927     c* memorizza le "note" che NON sono presenti nel record bolla
291900030206     c* ma sul file estensione "riferimenti"
292000010927     c*---
292100010927     c* motivi della NON consegna
292200010927     c                   CLEAR                   tita4000
292300010927     c                   CLEAR                   tita4p00
292400010927     c                   Z-ADD     s_tasaas      ka4aas
292500010927     c                   Z-ADD     s_taslnp      ka4lnp
292600010927     c                   Z-ADD     s_tasnrs      ka4nrs
292700010927     c                   Z-ADD     s_tasnsp      ka4nsp
292800010927     c     ke2ta4        SETLL     tita430c
292900010927     c     ke2ta4        READE     tita430c                               98
293000010927do  1c                   DOW       NOT *in98
293100010927if  2c                   IF        ta4trc = 'B' OR                              *Lasciato avviso
293200010927     c                             ta4trc = 'C' OR
293300010927     c                             ta4trc = 'G' OR                              *Giacenza
293400010927     c                             ta4trc = 'H' OR
293500010927     c                             ta4trc = 'N' OR                              *Non fatta
293600050701     c                             ta4trc = 'O' OR
293700050701     c                             ta4trc = '8' OR ta4trc = '9'
293800010927     c* Lasciato avviso
293900010927if  3c                   IF        ta4trc = 'B'
294000050704     C                   EVAL      wcev = %SUBST(ta4not:1:3)
294100050704     C                   EXSR      deccevsta
294200050704     C                   IF        werr = *OFF
294300050704     c                   EVAL      mmncB = wcdex
294400050704     C                   ELSE
294500010927     c                   EVAL      mmncB = %SUBST(ta4not:5:31)
294600050704     C                   ENDIF
294700010927e   3c                   ENDIF
294800010927if  3c                   IF        ta4trc = 'C'
294900010927     c                   EVAL      mmncC = ta4not
295000010927e   3c                   ENDIF
295100010927     c* Giacenza
295200010927if  3c                   IF        ta4trc = 'G'
295300050704     C                   EVAL      wcev = %SUBST(ta4not:1:3)
295400050704     C                   EXSR      deccevsta
295500050704     C                   IF        werr = *OFF
295600050704     c                   EVAL      mmncG = wcdex
295700050704     C                   ELSE
295800010927     c                   EVAL      mmncG = %SUBST(ta4not:5:31)
295900050704     C                   ENDIF
296000010927e   3c                   ENDIF
296100010927if  3c                   IF        ta4trc = 'H'
296200010927     c                   EVAL      mmncH = ta4not
296300010927e   3c                   ENDIF
296400010927     c* Non fatta
296500010927if  3c                   IF        ta4trc = 'N'
296600050704     C                   EVAL      wcev = %SUBST(ta4not:1:3)
296700050704     C                   EXSR      deccevsta
296800050704     C                   IF        werr = *OFF
296900050704     c                   EVAL      mmncN = wcdex
297000050704     C                   ELSE
297100010927     c                   EVAL      mmncN = %SUBST(ta4not:5:31)
297200050704     C                   ENDIF
297300010927e   3c                   ENDIF
297400010927if  3c                   IF        ta4trc = 'O'
297500010927     c                   EVAL      mmncO = ta4not
297600010927e   3c                   ENDIF
297700050701     c*
297800050701if  3c                   IF        ta4trc = '8'
297900050701     c                   EVAL      mmnc8 = ta4not
298000050701e   3c                   ENDIF
298100050701if  3c                   IF        ta4trc = '9'
298200050701     c                   EVAL      mmnc9 = ta4not
298300050701e   3c                   ENDIF
298400050701     c*
298500010927e   2c                   ENDIF
298600010925     c*
298700010926     c     ke2ta4        READE     tita430c                               98
298800010926e   1c                   ENDDO
298900050701     C*
299000010927if  1c                   IF        mmncB <> *blanks OR
299100010927     c                             mmncC <> *blanks OR
299200010927     c                             mmncG <> *blanks OR
299300010927     c                             mmncH <> *blanks OR
299400010927     c                             mmncN <> *blanks OR
299500050701     c                             mmncO <> *blanks OR
299600050701     c                             mmnc8 <> *blanks OR mmnc9 <> *blanks
299700050701     C*
299800050701     C                   IF        mmnc8 <> *BLANK
299900050701     C                   EVAL      a230 = %TRIML(mmnc8)
300000050701     C                   ENDIF
300100050701     C                   IF        mmnc9 <> *BLANK
300200050701     C                   IF        a230 <> *BLANK
300300050701     C                   EVAL      a230 = %TRIMR(a230) + ' ' + %TRIML(mmnc9)
300400050701     C                   ELSE
300500050701     C                   EVAL      a230 = %TRIML(mmnc9)
300600050701     C                   ENDIF
300700050701     C                   ENDIF
300800050701     C*
300900050701if  1c                   IF        mmncB <> *blanks OR
301000050701     c                             mmncC <> *blanks OR
301100050701     c                             mmncG <> *blanks OR
301200050701     c                             mmncH <> *blanks OR
301300050701     c                             mmncN <> *blanks OR
301400050701     c                             mmncO <> *blanks
301500050701     c                   IF        a230 = *BLANK
301600060613     c*                  EVAL      a230 = 'Motivi della non consegna:'
301700060613     c                   EVAL      a230 = rtvMsgLang('TIS0367':langI74)
301800050701     C                   ELSE
301900060613     c*                  EVAL      a230 = %TRIMR(a230) +
302000060613     c*                            ' Motivi della non consegna:'
302100060613     c                   EVAL      a230 = %TRIMR(a230) +
302200060613     c                             ' ' + rtvMsgLang('TIS0367':langI74)
302300050701     C                   ENDIF
302400050701     c                   EVAL      a230 = %TRIMR(a230) + ' '
302500010927     c                             + %TRIM(mmncB) + ' '
302600010927     c                             + %TRIM(mmncC) + ' '
302700010927     c                             + %TRIM(mmncG) + ' '
302800010927     c                             + %TRIM(mmncH) + ' '
302900010927     c                             + %TRIM(mmncN) + ' '
303000010927     c                             + %TRIM(mmncO)
303100050701     C                   ENDIF
303200010927     C*
303300050701     c                   EVAL      n3 = %LEN(%TRIM(a230))                       *lunghezza campo
303400050701     C*
303500010927if  2c                   IF        i < 10                                       *se c'è spazio
303600010927     c                   ADD       1             i                              *memorizza nota
303700010927     c                   EVAL      smnot(i) = %SUBST(a230:1:100)                *UNO
303800010927e   2c                   ENDIF
303900010927     C*
304000010927if  2c                   IF        i < 10                                       *se c'è spazio
304100010927if  3c                   IF        n3 > 100
304200010927     c                   ADD       1             i                              *memorizza nota
304300010927     c                   EVAL      smnot(i) = %SUBST(a230:101:100)              *DUE
304400010927e   3c                   ENDIF
304500010927e   2c                   ENDIF
304600010927     C*
304700010927if  2c                   IF        i < 10                                       *se c'è spazio
304800010927if  3c                   IF        n3 > 200
304900010927     c                   ADD       1             i                              *memorizza nota
305000010927     c                   EVAL      smnot(i) = %SUBST(a230:201:30)               *TRE
305100010927e   3c                   ENDIF
305200010927e   2C                   ENDIF
305300010927e   1c                   ENDIF
305400010925     c*
305500000000     c* imposta contatore note da visualizzare nella DS di Output
305600000000     c                   EVAL      cntnoto74 = i
305700000000     c*
305800000000     c* imposta le note nella DS di Output
305900000000do  1c     1             DO        10            j
306000000000     c                   EVAL      nota(j) = smnot(j)
306100000000e   1c                   ENDDO
306200000000     c*
306300000000     c                   ENDSR
306400010716     c*--------------------------------------------------------------------------------------------*
306500010716     c* imposta i campi della DS di Output dalla firma DPD
306600010716     c*--------------------------------------------------------------------------------------------*
306700010716     c     impdsoDPD     BEGSR
306800010717     c*
306900010717     c* reimposta variabili di lavoro
307000010717     c                   RESET                   $AbFirma
307100010717     c                   CLEAR                   depute                         *deposito utente
307200010717     c                   CLEAR                   deppwd                         *deposito password
307300010717     c*---
307400010717     c* se bolla EXPORT DPD --> attiva il link per vedere la Lettera di vettura
307500010717     c*---
307600010717     c                   Z-ADD     s_taslna      worg
307700010717     c                   EXSR      decorg
307800020702     c                   IF        wodpd = 'DPD'
307900020702     c                   MOVEL     'S'           dpdo74
308000020702     c                   ELSE
308100020702     c                   MOVEL     *blanks       dpdo74
308200020702     c                   ENDIF
308300010717     c*
308400010717     c* legge l'estensione segnacolli bolla per prendere alcuni dati DPD
308500060627     c* Riferimento link Lettera di vettura
308600060627     c* Riferimento link Firma
308700010717if  1c                   IF        dpdo74 = 'S'                                 *bolla EXPORT DPD
308800010717     c                   CLEAR                   tita4000
308900010717     c                   CLEAR                   tita4p00
309000060627     c                   CLEAR                   dsbl4i
309100010717     c                   Z-ADD     s_tasaas      ka4aas
309200010717     c                   Z-ADD     s_taslnp      ka4lnp
309300010717     c                   Z-ADD     s_tasnrs      ka4nrs
309400010717     c                   Z-ADD     s_tasnsp      ka4nsp
309500060627     c                   MOVEL     'I'           ka4trc
309600060627     c     keyta4        CHAIN     tita430c                           98
309700010717if  2c                   IF        NOT *in98
309800060627     c                   MOVEL     ta4not        dsbl4i
309900060627     c                   EVAL      DPDRmnO74 = §b4ipn
310000060627     c                   EVAL      DPDFnpO74 = §b4ipn
310100010717e   2c                   ENDIF
310200010717     c*
310300010717     c* cerca abilitazione firma DPD per questo segnacollo
310400010717if  2c                   IF        $AbTD = 'S'                                  *SI abilitaz.firma
310500010717     c                   EXSR      RepAbilFirma
310600010717     c                   EVAL      DPDFirO74 = $AbFirma                         *SI/NO abilitazione
310700010717     c*---
310800010717     c* se bolla EXPORT DPD e cliente abilitato --> attiva il link per vedere la Firma del collo
310900010717     c*---
311000010717if  3c                   IF        $AbFirma = 'S'                               *SI alitazione
311100010717     c* dati
311200010720     c                   EVAL      DPDUteO74 =  depute                          *Utente
311300010720     c                   EVAL      DPDPwdO74 =  deppwd                          *Password
311400010717     c                   EVAL      DPDId1O74 = %SUBST(depute:1:5)               *1^part of user-ID
311500010717     c                   EVAL      DPDId2O74 = %SUBST(depute:6:3)               *2^part of user-ID
311600010717     c*
311700010720     c* altri dati --> nell' ultimo evento "MIC" della bolla
311800010717     c                   Z-ADD     s_tasaas      kvbaas
311900010717     c                   Z-ADD     s_taslnp      kvblnp
312000010717     c                   Z-ADD     s_tasnrs      kvbnrs
312100010717     c                   Z-ADD     s_tasnsp      kvbnsp
312200010717     c                   Z-ADD     99999999      kvbdev
312300010717     c                   Z-ADD     9999          kvbhev
312400010717     c     ke2evb        SETGT     fnevb01l
312500010717     c     keyevb        READPE    fnevb01l                               98
312600010717do  4c                   DOW       NOT *in98
312700101104if  5C                   IF        evbcev = EVENTO_MESSA_IN_CONSEGNA
312800010720     c*
312900060628     c*                  EVAL      DPDDepO74 = %SUBST(evbnot:1:3)               *Depot of delivery
313000060628     c*                  EVAL      DPDPodO74 = %SUBST(evbnot:4:4)               *POD   of delivery
313100060629     c                   EVAL      devB01 = evbNot
313200060628     c                   IF        §NotDep = *BLANK
313300060628     c                   RESET                   DPDFirO74
313400060629     c                   LEAVE
313500060628     c                   ENDIF
313600060628     c                   EVAL      DPDDepO74 = §NotDep                          *Depot of delivery
313700060629     c                   EVAL      DPDPodO74 = §NotNdc                          *POD   of delivery
313800080131     c                   Z-ADD     evbDev        g08inv                         *data consegna
313900010720     c                   Z-ADD     *zeros        g08dat
314000010720     c                   MOVEL     '3'           g08err
314100010720     c                   CALL      'XSRDA8'
314200010720     c                   PARM                    wlbda8
314300010720     c                   MOVEL     g08dat        a8                             *Data gg/mm/aaaa
314400010720     c                   EVAL      DPDDodO74 = %SUBST(a8:1:4)                   *Date delivery (g/m)
314500010720     c                   EVAL      DPDYodO74 = %SUBST(a8:5:4)                   *Year delivery
314600010720     c*
314700010717     c                   LEAVE                                                  *uscita ciclo
314800010717e   5c                   ENDIF
314900010717     c     keyevb        READPE    fnevb01l                               98
315000010717e   4c                   ENDDO
315100010717     c*
315200010717e   3c                   ENDIF
315300010717e   2c                   ENDIF
315400010716e   1c                   ENDIF
315500010716     c*
315600010716     c                   ENDSR
315700010717     c*--------------------------------------------------------------------------------------------*
315800010717     c* imposta i campi della DS di Output dagli eventi
315900010717     c*--------------------------------------------------------------------------------------------*
316000010717     c     RepAbilFirma  BEGSR
316100010717     c*
316200010717     c* controlla se il cliente è abilitato a vedere la firma DPD per questa bolla
316300010717     c* e recupera utente / password
316400010717if  1C                   IF        kscI74 > *zeros                              *solo se CODIFICATI
316500010717     c                   EVAL      ktdksu = ksci74                              *cliente unificante
316600010717     c     keyvtd        SETLL     tivtd01l
316700010717     c     keyvtd        READE     tivtd01l                               98
316800010717do  2c                   DOW       NOT *in98
316900010717if  3c                   IF        vtdatb = *blanks AND                         *NO annullati
317000010717     c                             DPDFnpO74  >= vtdsed    AND                  *segancollo compreso
317100010717     c                             DPDFnpO74  <= vtdsea                         *segancollo compreso
317200010717     c                   EVAL      $AbFirma = 'S'                               *SI abil. FIRMA DPD
317300010717     c                   EVAL      depute = vtdute                              *utente
317400010717     c                   EVAL      deppwd = vtdpwd                              *password
317500010717     c                   LEAVE                                                  *esce dal ciclo
317600010717e   3c                   ENDIF
317700010717     c     keyvtd        READE     tivtd01l                               98
317800010717e   2c                   ENDDO
317900010717e   1C                   ENDIF
318000010717     c*
318100010717     c                   ENDSR
318200000000     c*--------------------------------------------------------------------------------------------*
318300000000     c* edita data da aaaammgg in gg/mm/aaaa
318400000000     c*--------------------------------------------------------------------------------------------*
318500000000     c     edtdat        BEGSR
318600000000     c*
318700000000     c                   MOVEL     *blanks       wdate
318800000000     c*
318900000000     c                   MOVEL     wdat          dsdat
319000000000     c                   MOVEL     dsgio         dsgioe
319100000000     c                   MOVEL     '/'           dsvd1
319200000000     c                   MOVEL     dsmes         dsmese
319300000000     c                   MOVEL     '/'           dsvd2
319400000000     c                   MOVEL     dsann         dsanne
319500000000     c                   MOVEL     dsdate        wdate
319600000000     c*
319700000000     c                   ENDSR
319800000000     c*--------------------------------------------------------------------------------------------*
319900000000     c* edita ora da hhmm in hh:mm
320000000000     c*--------------------------------------------------------------------------------------------*
320100000000     c     edtora        BEGSR
320200000000     c*
320300000000     c                   MOVEL     *blanks       worae
320400000000     c*
320500000000     c                   MOVEL     wora          dsora
320600000000     c                   MOVEL     dshh          dshhe
320700000000     c                   MOVEL     ':'           dsvo1
320800000000     c                   MOVEL     dsmm          dsmme
320900000000     c                   MOVEL     dsorae        worae
321000000000     c*
321100000000     c                   ENDSR
321200000000     c*--------------------------------------------------------------------------------------------*
321300000000     c* decodifica punto operativo
321400000000     c*--------------------------------------------------------------------------------------------*
321500000000     c     decorg        BEGSR
321600000000     c*
321700000000     c                   MOVEL     *blanks       wodes                          *descrizione
321800000000     c                   MOVEL     *blanks       wofl1                          *flag italia/estero
321900000000     c                   MOVEL     *blanks       wodpd                          *flag filiale DPD
322000000000     c*
322100000000     c                   SETOFF                                           98
322200041111     c                   Z-ADD     1             O
322300041111     c     worg          LOOKUP    sorg(O)                                98
322400000000if  1c                   IF        *in98                                        *trovata
322500121001     c*                  MOVEL     sodes(O)      wodes
322600121001     c                   EVAL      wodes = %TRIMR(soDes(o)) + ' (' +
322700121001     c                             %EDITC(wOrg : 'X') + ')'
322800041111     c                   MOVEL     sofl1(O)      wofl1
322900041111     c                   MOVEL     sodpd(O)      wodpd
323000000000e   1c                   ENDIF
323100000000     c*
323200000000     c                   ENDSR
323300000000     c*--------------------------------------------------------------------------------------------*
323400000000     c* decodifica evento per STATI
323500000000     c*--------------------------------------------------------------------------------------------*
323600000000     c     deccevsta     BEGSR
323700000000     c*
323800000000     c                   MOVEL     '0'           werr                           *NO errore
323900050704     c*
324000050704     C                   IF        wcev = 'GEN'
324100050704     C                   EVAL      werr = *ON
324200050704     C                   CLEAR                   wcdex
324300050704     C                   LEAVESR
324400050704     C                   ENDIF
324500000000     c*
324600060622     c*                  SETOFF                                           98
324700060622     c*                  Z-ADD     1             j
324800060622     c*    wcev          LOOKUP    scev(j)                                98
324900060622     C                   RESET                   tibs02ds
325000060622     C                   EVAL      t02Cod = 'ICE'
325100060622     C                   EVAL      t02Ke1 = wcev
325200060622     C                   EXSR      getTntbe00f
325300060622if  1c*                  IF        *in98 AND                                    *trovata
325400060622     c*                            ((scsta(j)  = 'S' AND savtsp<>'P') OR
325500060622     c*                             (scstaP(j) = 'S' AND savtsp= 'P'))
325600060622     C                   IF        t02Err <> 'E' AND
325700060622     C                             ((§icesta = 'S' AND savtsp <> 'P') OR
325800060622     C                              (§icestaP = 'S' AND savtsp = 'P'))
325900030130if  2c     savtsp        IFEQ      'P'                                          *bolla poste
326000060622     c*                  MOVEL     scdep(j)      wcdex
326100060622     c                   MOVEL     §icedep       wcdex
326200030130x   2c                   ELSE                                                   *bolla bartolini
326300060622     c*                  MOVEL     scdei(j)      wcdex
326400060622     c                   MOVEL     §icedei       wcdex
326500030130e   2c                   ENDIF
326600000000x   1c                   ELSE                                                   *non trovato
326700000000     c                   MOVEL     '1'           werr                           *SI errore
326800030130     c                   MOVEL     *blanks       wcdex
326900000000e   1c                   ENDIF
327000000000     c*
327100000000     c                   ENDSR
327200000000     c*--------------------------------------------------------------------------------------------*
327300000000     c* decodifica consegna particolare
327400000000     c*--------------------------------------------------------------------------------------------*
327500000000     c     decftc        BEGSR
327600000000     c*
327700000000     c                   MOVEL     *blanks       wdftc
327800000000     c*
327900000000if  1c                   IF        wftc = 'S'
328000060612     c*                  EVAL      wdftc = 'ai supermercati'
328100060612     c                   EVAL      wdftc = rtvMsgLang('TIS0636':langI74)
328200000000e   1c                   ENDIF
328300000000if  1c                   IF        wftc = 'P'
328400060612     c*                  EVAL      wdftc = 'ai piani'
328500060612     c                   EVAL      wdftc = rtvMsgLang('TIS0635':langI74)
328600000000e   1c                   ENDIF
328700000000if  1c                   IF        wftc = 'A'
328800060612     c*                  EVAL      wdftc = 'su appuntamento'
328900060612     c                   EVAL      wdftc = rtvMsgLang('TIS0686':langI74)
329000000000e   1c                   ENDIF
329100000000if  1c                   IF        wftc = 'F'
329200060612     c*                  EVAL      wdftc = 'fuori misura'
329300060612     c                   EVAL      wdftc = rtvMsgLang('TIS0658':langI74)
329400000000e   1c                   ENDIF
329500000000     c*
329600000000     c                   ENDSR
329700000000     c*--------------------------------------------------------------------------------------------*
329800000000     c* decodifica turno di chiusura
329900000000     c*--------------------------------------------------------------------------------------------*
330000000000     c     decgcx        BEGSR
330100000000     c*
330200000000     c                   MOVEL     *blanks       wdgcx
330300000000     c* giorno
330400000000     c                   MOVEL     wgcx          a1                             *giorno chiusura
330500000000if  1c                   IF        a1 = '1'
330600060612     c*                  EVAL      wdgcx = 'Lunedì'
330700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0351':langI74)
330800000000e   1c                   ENDIF
330900000000if  1c                   IF        a1 = '2'
331000060612     c*                  EVAL      wdgcx = 'Martedì'
331100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0354':langI74)
331200000000e   1c                   ENDIF
331300000000if  1c                   IF        a1 = '3'
331400060612     c*                  EVAL      wdgcx = 'Mercoledì'
331500060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0356':langI74)
331600000000e   1c                   ENDIF
331700000000if  1c                   IF        a1 = '4'
331800060612     c*                  EVAL      wdgcx = 'Giovedì'
331900060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0203':langI74)
332000000000e   1c                   ENDIF
332100000000if  1c                   IF        a1 = '5'
332200060612     c*                  EVAL      wdgcx = 'Venerdì'
332300060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0607':langI74)
332400000000e   1c                   ENDIF
332500000000if  1c                   IF        a1 = '6'
332600060612     c*                  EVAL      wdgcx = 'Sabato'
332700060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0533':langI74)
332800000000e   1c                   ENDIF
332900000000if  1c                   IF        a1 = '7'
333000060612     c*                  EVAL      wdgcx = 'Domenica'
333100060612     c                   EVAL      wdgcx = rtvMsgLang('TIS0159':langI74)
333200000000e   1c                   ENDIF
333300000000if  1c                   IF        a1 = ' '
333400000000     c                   EVAL      wdgcx = *blanks
333500000000e   1c                   ENDIF
333600000000     c* am/pm
333700000000     c                   MOVE      wgcx          a1                             *mattino/pomeriggio
333800000000if  1c                   IF        a1 = ' '
333900060612     c*                  EVAL      wdgcx =
334000060612     c*                            %TRIM(wdgcx) + ' ' + 'mat/pom'
334100060612     c                   EVAL      wdgcx =
334200060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0666':langI74)
334300000000e   1c                   ENDIF
334400000000if  1c                   IF        a1 = 'M'
334500060612     c*                  EVAL      wdgcx =
334600060612     c*                            %TRIM(wdgcx) + ' ' + 'mattino'
334700060612     c                   EVAL      wdgcx =
334800060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0667':langI74)
334900000000e   1c                   ENDIF
335000000000if  1c                   IF        a1 = 'P'
335100060612     c*                  EVAL      wdgcx =
335200060612     c*                            %TRIM(wdgcx) + ' ' + 'pomeriggio'
335300060612     c                   EVAL      wdgcx =
335400060612     c                             %TRIM(wdgcx) + rtvMsgLang('TIS0676':langI74)
335500000000e   1c                   ENDIF
335600000000     c*
335700000000     c                   ENDSR
335800000000     c*--------------------------------------------------------------------------------------------*
335900000000     c* decodifica evento per GIACENZE
336000000000     c*--------------------------------------------------------------------------------------------*
336100000000     c     deccevgia     BEGSR
336200000000     c*
336300000000     c                   MOVEL     '0'           werr                           *NO errore
336400000000     c*
336500060622     c*                  SETOFF                                           98
336600060622     c*                  Z-ADD     1             j
336700060622     c*    wcev          LOOKUP    scev(j)                                98
336800060622     C                   RESET                   tibs02ds
336900060622     C                   EVAL      t02Cod = 'ICE'
337000060622     C                   EVAL      t02Ke1 = wcev
337100060622     C                   EXSR      getTntbe00f
337200060622if  1c*                  IF        *in98 AND                                    *trovata
337300060622     c*                            ((scgia(j)  = 'S' AND savtsp<>'P') OR
337400060622     c*                             (scgiaP(j) = 'S' AND savtsp= 'P'))
337500060622     C                   IF        t02Err <> 'E' AND                            *trovata
337600060622     C                             ((§icegia = 'S' AND savtsp <> 'P') OR
337700060622     C                              (§icegiap = 'S' AND savtsp = 'P'))
337800030130if  2c     savtsp        IFEQ      'P'                                          *bolla poste
337900060622     c*                  MOVEL     scdep(j)      wcdex
338000060622     c                   MOVEL     §icedep       wcdex
338100030130x   2c                   ELSE                                                   *bolla bartolini
338200060622     c*                  MOVEL     scdei(j)      wcdex
338300060622     c                   MOVEL     §icedei       wcdex
338400030130e   2c                   ENDIF
338500000000x   1c                   ELSE                                                   *non trovato
338600000000     c                   MOVEL     '1'           werr                           *SI errore
338700030130     c                   MOVEL     *blanks       wcdex
338800000000e   1c                   ENDIF
338900000000     c*
339000000000     c                   ENDSR
339100000000     c*--------------------------------------------------------------------------------------------*
339200000000     c* Controlla la DS dsi Input
339300000000     c*--------------------------------------------------------------------------------------------*
339400000000     c     chkdsinput    BEGSR
339500000000     c*
339600000000     c                   MOVEL     '0'           werr                           *NO errore
339700000000     c*
339800000000     c* dalla DS di Input scompone il codice spedizione
339900000000     c                   MOVEL     nspedizi74    dsspedizi74
340000000000     c*
340100000000     c* controlla che la lina di partenza sia TUTTA numerica
340200000000     c                   SETOFF                                       99
340300000000     c                   TESTN                   dslnpi74             99
340400000000if  1c                   IF        NOT *in99                                    *non è un numero
340500000000     c                   MOVEL     '1'           werr                           *SI errore
340600000000x   1c                   ELSE                                                   *è un numero
340700000000     c                   MOVE      dslnpi74      a1
340800000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
340900000000     c                   MOVEL     '1'           werr                           *SI errore
341000000000e   2c                   ENDIF
341100000000e   1c                   ENDIF
341200000000     c*
341300000000     c* controlla che la serie spedizione sia TUTTA numerica
341400000000     c                   SETOFF                                       99
341500000000     c                   TESTN                   dsnrsi74             99
341600000000if  1c                   IF        NOT *in99                                    *non è un numero
341700000000     c                   MOVEL     '1'           werr                           *SI errore
341800000000x   1c                   ELSE                                                   *è un numero
341900000000     c                   MOVE      dsnrsi74      a1
342000000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
342100000000     c                   MOVEL     '1'           werr                           *SI errore
342200000000e   2c                   ENDIF
342300000000e   1c                   ENDIF
342400000000     c*
342500000000     c* controlla che il numero spedizione sia TUTTO numerico
342600000000     c                   SETOFF                                       99
342700000000     c                   TESTN                   dsnspi74             99
342800000000if  1c                   IF        NOT *in99                                    *non è un numero
342900000000     c                   MOVEL     '1'           werr                           *SI errore
343000000000x   1c                   ELSE                                                   *è un numero
343100000000     c                   MOVE      dsnspi74      a1
343200000000if  2c                   IF        a1 < '0'                                     *trovata una lettera
343300000000     c                   MOVEL     '1'           werr                           *SI errore
343400000000e   2c                   ENDIF
343500000000e   1c                   ENDIF
343600110104     c*
343700110104     c* controlla che il codice cliente sia TUTTO numerico
343800110104     c                   IF        kscI74 <> *BLANK
343900110104     c                   SETOFF                                       99
344000110104     c                   TESTN                   kscI74               99
344100110104     c                   IF        NOT *in99                                    *non è un numero
344200110104     c                   MOVEL     '1'           werr                           *SI errore
344300110104     c                   ELSE                                                   *è un numero
344400110104     c                   MOVE      kscI74        a1
344500110104     c                   IF        a1 < '0'                                     *trovata una lettera
344600110104     c                   MOVEL     '1'           werr                           *SI errore
344700110104     c                   ENDIF
344800110104     c                   ENDIF
344900110104     c                   ENDIF
345000110104     c*
345100110104     c* controlla che il codice mittente sia TUTTO numerico
345200110104     c                   IF        ccmI74 <> *BLANK
345300110104     c                   SETOFF                                       99
345400110104     c                   TESTN                   ccmI74               99
345500110104     c                   IF        NOT *in99                                    *non è un numero
345600110104     c                   MOVEL     '1'           werr                           *SI errore
345700110104     c                   ELSE                                                   *è un numero
345800110104     c                   MOVE      ccmI74        a1
345900110104     c                   IF        a1 < '0'                                     *trovata una lettera
346000110104     c                   MOVEL     '1'           werr                           *SI errore
346100110104     c                   ENDIF
346200110104     c                   ENDIF
346300110104     c                   ENDIF
346400050104     c*
346500050104     c* controlla anno spedizione
346600050104     c                   TESTN                   cAASI74              99
346700050104if  1c                   IF        NOT *in99                                    *non è un numero
346800050104     c                   EVAL      cAASI74 = *ZERO
346900050104x   1c                   ENDIF
347000100222     c*
347100100223     c* Estraggo l'anno dalla data spedizione.
347200100223     c* La data dovrebbe essere in formato dd.mm.yyyy ma potrebbe essere dd/mm/yyyy,
347300100223     c* in tal caso cambio '/' in '.' così ottengo un formato *EUR.
347400100223     C                   IF        dspI74 <> *BLANK
347500100223     C                   EVAL      %SUBST(dspI74 : 3 : 1) = '.'
347600100223     C                   EVAL      %SUBST(dspI74 : 6 : 1) = '.'
347700100223     C                   MONITOR
347800100223     C                   EVAL      aasI74 = %SUBDT(%DATE(dspI74 : *EUR) : *Y)
347900100223     C                   ON-ERROR
348000100223     C                   ENDMON
348100100222     C                   ENDIF
348200000000     c*
348300000000     c* se errore, esce dal programma
348400000000if  1c                   IF        werr = '1'                                   *SI errore
348500000000     c                   MOVEL     '1'           esito                          *errore
348600000000e   1c                   ENDIF
348700000000     c*
348800000000     c                   ENDSR
348900000000     c*--------------------------------------------------------------------------------------------*
349000000000     c* Allineamento a Dx con riempimento di '0' di un campo alfanumerico
349100000000     c* Input:   Walfa7 = Alfanumerico di 7
349200000000     c* Output:  Walfa7 = Alfanumerico di 7 allineato a Dx con zeri
349300000000     c*--------------------------------------------------------------------------------------------*
349400000000     c     xallinea1     BEGSR
349500000000     c*
349600000000     c* reimposta variabli di lavoro
349700000000     c                   CLEAR                   i
349800000000     c                   CLEAR                   z
349900000000     c                   CLEAR                   walfa7bis
350000000000     c                   EVAL      wzeri = *ALL'0'
350100000000     c*
350200000000     c     ' '           CHECKR    walfa7:7      i
350300000000if  1c                   IF        i <> 0
350400000000     c     i             SUBST     walfa7        walfa7bis
350500000000e   1c                   ENDIF
350600000000if  1c                   IF        walfa7bis <> *blanks
350700000000     c     7             SUB       i             z
350800000000if  2c                   IF        z <> 0
350900000000     c                   eval      %subst(walfa7:1:z) = %subst(wzeri:1:z)
351000000000e   2c                   ENDIF
351100000000     c                   EVAL      z = z+ 1
351200000000     c                   EVAL      %subst(walfa7:z:i) = %subst(walfa7bis:1:i)
351300000000e   1c                   ENDIF
351400000000     c*
351500000000     c                   ENDSR
351600000000     c*--------------------------------------------------------------------------------------------*
351700000000     c* caricamento tabelle occorrenti
351800000000     c*--------------------------------------------------------------------------------------------*
351900000000     c     cartab        BEGSR
352000000000     c*---
352100000000     c* tipi servizio
352200000000     c*---
352300060622     c*                  CLEAR                   i                              *indice
352400060622     C*                  CLEAR                   ds5e
352500060622     C*                  CLEAR                   stsp
352600060622     C*                  CLEAR                   sdtsp
352700060622     C*                  Z-ADD     1             kblkut
352800060622     C*                  MOVEL     '5E'          kblcod
352900060622     c*    keytab        SETLL     tabel00f
353000060622     c*    keytab        READE     tabel00f                               99
353100060622do  1c*                  DOW       NOT *in99
353200060622if  2c*                  IF        tblflg = *blanks
353300060622     c*                  MOVEL     tbluni        ds5e
353400060622     c*                  ADD       1             i
353500060622     c*                  MOVEL     tblkey        stsp(i)
353600060622     c*                  MOVEL     §5edes        sdtsp(i)
353700060622e   2c*                  ENDIF
353800060622     c*    keytab        READE     tabel00f                               99
353900060622e   1c*                  ENDDO
354000000000     c*---
354100000000     c* causali eventi
354200000000     c*---
354300060622     c*                  CLEAR                   scev                           *azzera
354400060622     c*                  CLEAR                   scdei
354500060622     c*                  CLEAR                   scdep
354600060622     c*                  CLEAR                   scgia
354700060622     c*                  CLEAR                   scsta
354800060622     c*                  CLEAR                   scgiaP
354900060622     c*                  CLEAR                   scstaP
355000060622     c*                  Z-ADD     *zeros        i
355100060622     c*                  MOVEL     *blanks       kbecod                         *tabella
355200060622     c*                  MOVEL     *all'0'       kbeke1                         *chiave uno
355300060622     c*                  MOVE      'ICE'         kbeke1
355400060622     c*                  MOVEL     *blanks       kbeke2                         *chiave due
355500060622     c*                  MOVEL     *blanks       kbelin                         *lingua
355600060622     c*                  MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
355700060622     c*    keytbe        CHAIN     tntbe01l                           99
355800060622if  1c*                  IF        *in99                                        *non trovato
355900060622     c*                  MOVEL(P)  'GAITRA201'   kbesif                         *s.i. di AZIENDA
356000060622     c*    keytbe        CHAIN     tntbe01l                           99
356100060622e   1c*                  ENDIF
356200060622     c*                  MOVEL     'ICE'         kbecod                         *tabella
356300060622     c*    ke2tbe        SETLL     tntbe02l
356400060622     c*    ke2tbe        READE     tntbe02l                               99
356500060622do  1c*                  DOW       NOT *in99
356600060622if  2c*                  IF        tbeatb = *blanks                             *no annullati
356700060622     c*                  MOVEL     tbeuni        dice
356800060622if  3c*                  IF        §icevat = *blanks                            *solo attivi
356900060622     c*                  ADD       1             i
357000060622     c*                  MOVEL     tbeke1        scev(i)                        *causale evento
357100060622     c*                  MOVEL     §icedei       scdei(i)                       *descrizione italian
357200060622     c*                  MOVEL     §icedep       scdep(i)                       *descrizione poste
357300060622     c*                  MOVEL     §icesta       scsta(i)                       *S=Usare in Stati
357400060622     c*                  MOVEL     §icegia       scgia(i)                       *S=Usare in Giacenze
357500060622     c*                  MOVEL     §icestaP      scstaP(i)                      *S=Usare in Stati PT
357600060622     c*                  MOVEL     §icegiaP      scgiaP(i)                      *S=Usare in Giac  PT
357700060622e   3c*                  ENDIF
357800060622e   2c*                  ENDIF
357900060622     c*    ke2tbe        READE     tntbe02l                               99
358000060622e   1c*                  ENDDO
358100000000     c*---
358200000000     c* consegne anomale
358300000000     c*---
358400060622     c*                  CLEAR                   scca                           *azzera
358500060622     c*                  CLEAR                   sadei
358600060622     c*                  CLEAR                   sadep
358700060622     c*                  CLEAR                   sainc
358800060622     c*                  CLEAR                   saicv
358900060622     c*                  CLEAR                   sacev
359000060622     c*                  Z-ADD     *zeros        i
359100060622     C*                  Z-ADD     1             kblkut
359200060622     C*                  MOVEL     '7O'          kblcod
359300060622     c*    keytab        SETLL     tabel00f
359400060622     c*    keytab        READE     tabel00f                               99
359500060622do  1c*                  DOW       NOT *in99
359600060622if  2c*                  IF        tblflg = *blanks
359700060622     c*                  MOVEL     tbluni        ds7o
359800060622     c*                  ADD       1             i
359900060622     c*                  MOVEL     tblkey        scca(i)
360000060622     c*                  MOVEL     §7odei        sadei(i)
360100060622     c*                  MOVEL     §7odep        sadep(i)
360200060622     c*                  MOVEL     §7oinc        sainc(i)
360300060622     c*                  MOVEL     §7oicv        saicv(i)
360400060622     c*                  MOVEL     §7ocev        sacev(i)
360500060622     c*                  MOVEL     §7OPODImg     sapod(i)
360600060622e   2c*                  ENDIF
360700060622     c*    keytab        READE     tabel00f                               99
360800060622e   1c*                  ENDDO
360900000000     c*---
361000000000     c* Causali chiusura CA
361100000000     c*---
361200060622     c*                  CLEAR                   scch                           *azzera
361300060622     c*                  Z-ADD     *zeros        i
361400060622     c*                  MOVEL     *blanks       kbecod                         *tabella
361500060622     c*                  MOVEL     *all'0'       kbeke1                         *chiave uno
361600060622     c*                  MOVE      'CCH'         kbeke1
361700060622     c*                  MOVEL     *blanks       kbeke2                         *chiave due
361800060622     c*                  MOVEL     *blanks       kbelin                         *lingua
361900060622     c*                  MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
362000060622     c*    keytbe        CHAIN     tntbe01l                           99
362100060622if  1c*                  IF        *in99                                        *non trovato
362200060622     c*                  MOVEL(P)  'GAITRA201'   kbesif                         *s.i. di AZIENDA
362300060622     c*    keytbe        CHAIN     tntbe01l                           99
362400060622e   1c*                  ENDIF
362500060622     c*                  MOVEL     'CCH'         kbecod                         *tabella
362600060622     c*    ke2tbe        SETLL     tntbe02l
362700060622     c*    ke2tbe        READE     tntbe02l                               99
362800060622do  1c*                  DOW       NOT *in99
362900060622if  2c*                  IF        tbeatb = *blanks                             *no annullati
363000060622     c*                  MOVEL     tbeuni        dcch
363100060622if  3c*                  IF        §cchinte = 'S'                               *validi x Inernet
363200060622     c*                  ADD       1             i
363300060622     c*                  MOVEL     tbeke1        scch(i)                        *causale evento
363400060622e   3c*                  ENDIF
363500060622e   2c*                  ENDIF
363600060622     c*    ke2tbe        READE     tntbe02l                               99
363700060622e   1c*                  ENDDO
363800000000     c*---
363900000000     c* organigrama
364000000000     c*---
364100000000     c                   CLEAR                   i                              *indice
364200000000     C                   CLEAR                   sorg
364300000000     C                   CLEAR                   sodes
364400000000     C                   CLEAR                   sofl1
364500000000     C                   CLEAR                   sopt
364600000000     C                   CLEAR                   sodpd
364700060623     C                   OPEN      tntbe01l
364800000000     c     *loval        SETLL     azorg01l
364900000000     c                   READ      azorg01l                               99
365000000000do  1c                   DOW       NOT *in99
365100000000     c                   ADD       1             i
365200000000     c                   Z-ADD     orgfil        sorg(i)
365300000000     c                   MOVEL     orgdes        sodes(i)
365400000000     c                   MOVEL     orgde3        og143
365500020702     C*
365600020702     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
365700020702     C                   CLEAR                   dntw
365800020702     C*
365900020702     C                   MOVEL(P)  'NTW'         kbecod
366000020702     C                   MOVEL(P)  §ogntw        kbeke1
366100020702     c                   MOVEL     *blanks       kbeke2                         *chiave due
366200020702     c                   MOVEL     *blanks       kbelin                         *lingua
366300020702     c                   MOVEL     *blanks       kbesif                         *s.i. di GRUPPO
366400020702     C     keytbe        CHAIN     tntbe01l
366500020702     C                   IF        %found(tntbe01l)
366600020702     C                   MOVEL     tbeuni        dntw
366700020702     C                   ENDIF
366800020702     C*
366900020702     C                   MOVEL     §ntwfie       sofl1(i)
367000020702     C                   IF        §ogntw = 'DPD'
367100020702     C                   MOVEL     'E'           sofl1(i)
367200020702     C                   ENDIF
367300020702     C*
367400020702     c                   MOVEL     §ogntw        sopt(i)
367500020702     c                   MOVEL     §ogntw        sodpd(i)
367600000000     c                   READ      azorg01l                               99
367700000000e   1c                   ENDDO
367800060623     C                   CLOSE     tntbe01l
367900000000     c*---
368000000000     c* stati contrassegno
368100000000     c*---
368200060622     c*                  CLEAR                   ssta                           *azzera
368300060622     c*                  CLEAR                   sdsta
368400060622     c*                  CLEAR                   sdstaP
368500060622     c*                  Z-ADD     *zeros        i
368600060622     C*                  Z-ADD     1             kblkut
368700060622     C*                  MOVEL     '4S'          kblcod
368800060622     c*    keytab        SETLL     tabel00f
368900060622     c*    keytab        READE     tabel00f                               99
369000060622do  1c*                  DOW       NOT *in99
369100060622if  2c*                  IF        tblflg = *blanks
369200060622     c*                  MOVEL     tbluni        ds4s
369300060622     c*                  ADD       1             i
369400060622     c*                  MOVE      tblkey        ssta(i)
369500060622     c*                  MOVEL     §4sdei        sdsta(i)
369600060622     c*                  MOVEL     §4sdep        sdstaP(i)
369700060622e   2c*                  ENDIF
369800060622     c*    keytab        READE     tabel00f                               99
369900060622e   1c*                  ENDDO
370000000000     c*---
370100000000     c* disposizioni giacenza
370200000000     c*---
370300060622     c*                  CLEAR                   i                              *indice
370400060622     C*                  CLEAR                   ds2d
370500060622     C*                  CLEAR                   sdis
370600060622     C*                  CLEAR                   sddis
370700060622     C*                  Z-ADD     1             kblkut
370800060622     C*                  MOVEL     '2D'          kblcod
370900060622     c*    keytab        SETLL     tabel00f
371000060622     c*    keytab        READE     tabel00f                               99
371100060622do  1c*                  DOW       NOT *in99
371200060622if  2c*                  IF        tblflg = *blanks
371300060622     c*                  MOVEL     tbluni        ds2d
371400060622     c*                  ADD       1             i
371500060622     c*                  MOVEL     tblkey        sdis(i)
371600060622     c*                  MOVEL     §2ddes        sddis(i)
371700060622e   2c*                  ENDIF
371800060622     c*    keytab        READE     tabel00f                               99
371900060622e   1c*                  ENDDO
372000000000     c*
372100000000     c                   ENDSR
372200000000     c*--------------------------------------------------------------------------------------------*
372300000000     c* operazioni iniziali -da fare sempre-
372400000000     c*--------------------------------------------------------------------------------------------*
372500000000     c     rutinz        BEGSR
372600000000     c*
372700000000     c* reperimento data corrente
372800000000     C                   TIME                    n14                            *ora (6)+ data (8)
372900000000     C                   MOVEL     n14           oracor                         *ora  (6) in h:m:s
373000000000     C                   MOVE      n14           n8                             *data (8) in g/m/a
373100000000     C                   Z-ADD     n8            g08dat
373200000000     C                   Z-ADD     *zeros        g08inv
373300000000     C                   MOVEL     '0'           g08err
373400000000     c                   CALL      'XSRDA8'
373500000000     c                   PARM                    wlbda8
373600000000     C                   Z-ADD     g08inv        datcor                         *Data corrente a/m/g
373700000000     C                   MOVEL     datcor        anncor                         *anno corrente
373800000000     c                   EVAL      annpre = anncor - 1                          *anno cprecedente
373900000000     c*
374000000000     c* calcola data corrente - 1 anno
374100000000     c                   Z-ADD     datcor        datpre                         *data precedente
374200000000     c                   MOVEL     annpre        datpre                         *d.precedente - 1aa
374300000000     c*
374400010716     c* azzera la DS di Output e le variabili di lavoro / controllo
374500000000     c                   MOVEL     '0'           esito
374600000000     c                   CLEAR                   tis174dso                      *ds Output
374700000000     c                   CLEAR                   smi                            *di memorizzazione
374800000000     c                   CLEAR                   smseql
374900000000     c                   CLEAR                   smseqe
375000000000     c                   CLEAR                   smdevhev
375100000000     c                   CLEAR                   smdeve
375200000000     c                   CLEAR                   smheve
375300000000     c                   CLEAR                   smfle
375400000000     c                   CLEAR                   smdev
375500000000     c                   CLEAR                   smcev
375600130312     c                   CLEAR                   smspe
375700000000     c                   CLEAR                   smdmc
375800000000     c                   CLEAR                   smdmcR
375900000000     c                   CLEAR                   tote                           *contatore eventi
376000000000     c                   CLEAR                   seql                           *sequenza legami
376100010716     c                   RESET                   $AbTD                          *di lavoro
376200010716     c                   RESET                   $AbFirma
376300000000     c*
376400000000     c* controlla da DS in Input
376500000000     c                   EXSR      chkdsinput
376600000000     c*
376700000000     c* carica i clienti dettaglio del cliente unificante
376800090903     c*                  CLEAR                   sksc7                          *clienti dettaglio
376900000000     c                   EVAL      kssksu = ksci74                              *cliente unificante
377000000000     c                   EVAL      kssisv = 'TT'                                *tipo servizio
377100000000     c     keyvss        SETLL     tivss02l
377200000000     c     keyvss        READE     tivss02l                               99
377300000000do  1c                   DOW       NOT *in99
377400000000if  2c                   IF        vssvat = *blanks                             *attivo
377500000000if  3c                   IF        datcor >= vssdde AND                         *in decorrenza
377600000000     c                             datcor <= vssdsc
377700000000     c*
377800000000     c* controlla se per il cliente unificante ci sono clienti figli da elaborare
377900090903     c*                  CLEAR                   tibs10ds
378000090903     c*                  EVAL      d10drf = datcor                              *data riferimento
378100090903     c*                  EVAL      d10tle = vsstle                              *legame
378200090903     c*                  EVAL      d10paf = 'F'                                 *cerca i figli
378300090903     c*                  MOVEL     ksci74        n8
378400090903     c*                  Z-ADD     n8            d10cod                         *cliente unificante
378500090903     c*                  CALL      'TIBS10R'                                    *cerca figli
378600090903     c*                  PARM                    tibs10ds
378700090903if  4c*                  IF        d10err <> ' '                                *SI errore: no figli
378800090903     c*                  MOVE      ksci74        sksc7(1)                       *cliente padre=unif
378900090903x   4c*                  ELSE                                                   *NO errore: si figli
379000090903     c*                  Z-ADD     sksc11        sksc7                          *cliente padre+figli
379100090903e   4c*                  ENDIF
379200000000     c*
379300010831     c* se trovato cliente abilitato, guarda anche se è abilitato al servizio:
379400010719     c* --> TD per vedere la firma DPD
379500010716     c                   RESET                   $AbTD                          *di lavoro
379600010716     c                   EVAL      kssksu = ksci74                              *cliente unificante
379700010719     c     kssksu        SETLL     tivss02l
379800010719     c     kssksu        READE     tivss02l                               99
379900010719do  4c                   DOW       NOT *in99
380000010716if  5c                   IF        vssvat = *blanks                             *attivo
380100010716if  6c                   IF        datcor >= vssdde AND                         *in decorrenza
380200010716     c                             datcor <= vssdsc
380300010719if  7c                   IF        vssisv = 'TD'
380400010716     c                   EVAL      $AbTD = 'S'                                  *SI abilitazione TD
380500010719e   7c                   ENDIF
380600010719e   6c                   ENDIF
380700010716e   5c                   ENDIF
380800010719     c     kssksu        READE     tivss02l                               99
380900010719e   4c                   ENDDO
381000010716     c*
381100000000     c                   LEAVE                                                  *uscita ciclo
381200000000e   3c                   ENDIF
381300000000e   2c                   ENDIF
381400000000     c     keyvss        READE     tivss02l                               99
381500010716e   1c                   ENDDO                                                  *fine cl.abil.TT
381600000000     c*
381700060621     c                   EVAL      langTabel = cvtLinguaISO2ToTabel(langI74)
381800060621     c                   EVAL      langTntbe = cvtLinguaISO2ToTntbe(langI74)
381900060621     c*
382000000000     c                   ENDSR
382100040330
382200040330     ***********************************************************************************************
382300040330     **?
382400040330     **?Reperimento ulteriori dati destinatario.
382500040330     **?
382600040330     ***********************************************************************************************
382700040330     C     ImpDSDest     BEGSR
382800040330
382900100121     C                   IF        ar5Nsp <> s_tasnsp OR ar5Lnp <> s_taslnp OR
383000100121     C                             ar5Nrs <> s_tasnrs OR ar5Aas <> s_tasaas OR
383100100121     C                             ar5Trd <> 'GEN'
383200040330     c                   Z-ADD     s_tasaas      kr5aas
383300040330     c                   Z-ADD     s_taslnp      kr5lnp
383400040330     c                   Z-ADD     s_tasnrs      kr5nrs
383500040330     c                   Z-ADD     s_tasnsp      kr5nsp
383600040330     c                   MOVEL     'GEN'         kr5trd
383700040531     c     keyar5        CHAIN     fiar531c
383800040330     C                   IF        %FOUND
383900040330     C                   EVAL      DAR5GEN = AR5Uni
384000100121     C                   ELSE
384100100121     C                   RESET                   DAR5GEN
384200040330     C                   ENDIF
384300100121     C                   ENDIF
384400100121     C
384500100121     C                   EVAL      REFCONSO74 = GEN§AR5REF
384600100121     C                   EVAL      TLFDESTO74 = GEN§AR5TELD
384700040330
384800040330     C                   ENDSR
384900040913
385000040913     ***********************************************************************************************
385100040913     **?
385200040913     **?Determino se addebitare l'immagine LDV.
385300040913     **?
385400040913     ***********************************************************************************************
385500040913     C     AddebitoLDV   BEGSR
385600040913
385700040913     C                   IF        LDVO74 = 'S'
385800040913
385900060623     C*                  EVAL      kbecod = 'LVA'
386000060623     C*                  EVAL      kbeke1 = '1'
386100060623     C*                  CLEAR                   kbeke2
386200060623     C*                  CLEAR                   kbelin
386300060623     C*                  CLEAR                   kbesif
386400060623     C*    keytbe        CHAIN     tntbe01l
386500060623     C*
386600060623     C*                  IF        %FOUND
386700060623     C*                  EVAL      DLVA1 = TBEUni
386800060623     C*                  ELSE
386900060623     C*                  CLEAR                   DLVA1
387000060623     C*                  ENDIF
387100060623     C*
387200060623     C*                  MOVE      §LVADtGrts    DtGrLDVO74
387300060623     C*                  TIME                    WrkCurDate
387400060623     C*
387500060623     C*                  IF        WrkCurDate > §LVADtGrts
387600040913     C                   EVAL      FlAdLDVO74 = *ON                             Addebitare
387700060623     C*                  ELSE
387800060623     C*                  EVAL      FlAdLDVO74 = *OFF                            Gratuito
387900060623     C*                  ENDIF
388000040913
388100040913     C                   ENDIF
388200040913
388300040913     C                   ENDSR
388400051110
388500051110     ***********************************************************************************************
388600051110     **?
388700051110     **?Controllo se in TIVPI00F ci sono delle disposizioni per la giacenza.
388800051110     **?
388900051110     ***********************************************************************************************
389000051110     C     chkTivpi      BEGSR
389100051110
389200051110     C                   CALLP(E)  tis7653r(kscI74:gcpAgc:gcpFgc:gcpNgc:
389300051110     C                             esito10)
389400051110     C                   IF        esito10 > 0 AND NOT %ERROR
389500051110     C                   EVAL      gcfdio74 = 'N'
389600051110     C                   ENDIF
389700051110
389800051110     C                   ENDSR
389900051222
390000051222     ***********************************************************************************************
390100051222     **
390200051222     ** Linea arrivo Italia o estera.
390300051222     **
390400051222     ***********************************************************************************************
390500051222     C     setLnaItaEst  BEGSR
390600051222
390700051222     C                   RESET                   wrkLnaItaEst
390800051222     C                   EXSR      decOrg
390900051222     C                   EVAL      wrkLnaItaEst = wofl1
391000051222
391100051222     C                   ENDSR
391200060622
391300060622     ***********************************************************************************************
391400060622     **
391500130312     ** Reperisco informazioni stato bolla in filiale se ultimo evento
391600130312     ** è un MIC
391700060622     **
391800060622     ***********************************************************************************************
391900130312     C     srarb         BEGSR
392000130312     c                   clear                   wcev
392100130312     c                   clear                   worg
392200130312     c                   clear                   wdat
392300130312     c                   clear                   wora
392400130312     c                   eval      dsevbspe = spe(1)
392500130312     c     karb          chain     fnarb01l
392600130312     c                   if        not %error and
392700130312     c                             %found(fnarb01l)
392800130422     c                   Z-ADD     arblna        worg
392900130312     c                   exsr      setLnaItaEst
393000130312     c* no per estero
393100130312     C                   IF        wrkLnaItaEst = ESTERO
393200130312     c                   leavesr
393300130312     c                   end
393400130312     c* consegnata (se non avessi consegnato mi sarei dovuta travare
393500130312     c* un evento diverso dal MIC nel 1° elemento della schiera)
393600130312     C                   SELECT
393700130312     c                   WHEN      arbdcm <> 0
393800130312     c                   MOVEL     '704'         wcev                           *causale evento
393900130313     c                   move      arbdcm        WDAT
394000130313     c                   move      arbhmc        WORA
394100130312     C* SE LA SPEDIZONE È ANCORA APERTA CERCO ESITI PDA
394200130312     c                   WHEN      ARBFDC = ' '
394300130326     c* solo se distinta NON in test
394400130326     c                   clear                   ddstflr
394500130326     c                   z-add     arbifp        dstfgs
394600130326     c                   z-add     arbndc        dstnfv
394700130326     c     kdst          chain     fidst01l
394800130326     c                   if        not %error and
394900130326     c                             %found(fidst01l)
395000130326     c                   eval      ddstflr = dstflr
395100130326     c                   end
395200130327     c                   if        §DSTTSTPDA = 'C' or
395300130327     c                             §DSTTSTPDA = 'E'
395400130326     c                   leavesr
395500130326     c                   end
395600130326     c*
395700130313     c                   move      arbndc        pctndc
395800130312     c     kcet          setgt     fipct02l
395900130312     c     kcet          readpe    fipct02l
396000130312     c                   if        not %error and
396100130312     c                             not %eof(fipct02l)
396200130312     c                   movel     pctdati       fipctcetds
396300130312     c* consegnata
396400130312     c                   if        §pctcmc = ' '
396500130312     c                   MOVEL     '704'         wcev                           *causale evento
396600130312     c                   else
396700130312     c* mancata consegna
396800130313     c                   movel     §PCTcmc       wcev
396900130312     c                   clear                   ds2a
397000130312     C                   EVAL      ds2a = chainTabel00f('CHAIN3':langTabel
397100130312     C                             :'2A':wcev:%LEN(wcev):'TBLUNI':rpyOpCode
397200130312     C                             :rpyEsito)
397300130312     c                   if        §2aflg = '§' and §2acpt = 'S'
397400130312     c                   move      'T  '         wcev
397500130312     c                   endif
397600130312     c                   endif
397700130312     c* ??? £6
397800130312     c                   if        §pctdata > 0
397900130313     c                   move      §PCTDATA      WDAT
398000130312     c                   movel     §PCTORA       WORA
398100130312     c                   endif
398200130312     c                   end
398300130312     c                   endSL
398400130312     c                   end
398500130312     c* ho trovato un evento da inserire  al 1° posto
398600130312     c                   if        wcev <> ' '
398700130312     c                   EXSR      deccevsta
398800130312     c                   IF        werr <> '1'                                  *no errore
398900130312     c                   EXSR      decorg
399000130312     c                   EXSR      edtdat
399100130312     c                   EXSR      edtora
399200130313     c* libero il 1° elemnto di schiera
399300130313     c                   eval      j = 50
399400130313     c                   for       j = 50 downto 1
399500130312     c                   if        cev(j) <> ' '
399600130312     c                   eval      cev(j+1) = cev(j)
399700130312     c                   eval      dev(j+1) = dev(j)
399800130312     c                   eval      hev(j+1) = hev(j)
399900130312     c                   eval      fle(j+1) = fle(j)
400000130312     c                   eval      eve(j+1) = eve(j)
400100130312     c                   end
400200130313     c                   endfor
400300130313     c                   eval      dev(1) = wdate
400400130313     c                   eval      hev(1) = worae
400500130312     c                   eval      cev(1)= wcev
400600130313     c                   eval      fle(1) = wodes
400700130313     c                   eval      eve(1) = wcdex
400800130313     c                   EVAL      cntEveO74 += 1
400900130312     c                   end
401000130312     c                   end
401100130312     C                   ENDSR
401200130312     ***********************************************************************************************
401300130312     **
401400130312     ** Reperisco tabella da TNTBE00F.
401500130312     **
401600130312     ***********************************************************************************************
401700130312     C     getTntbe00f   BEGSR
401800060622     C                   EVAL      t02Lin = langTntbe
401900060622     C                   CALLP     tibs02r(kpjba:tibs02ds)
402000060622     C                   SELECT
402100060622     C                   WHEN      t02Cod = 'ICE'
402200060622     C                   EVAL      dice = t02Uni
402300060622     C                   WHEN      t02Cod = 'CCH'
402400060622     C                   EVAL      dcch = t02Uni
402500060622     C                   WHEN      t02Cod = 'NTW'
402600060622     C                   EVAL      dntw = t02Uni
402700100119     C                   WHEN      t02Cod = 'I1A'
402800100119     C                   EVAL      dI1a = t02Uni
402900060622     C                   ENDSL
403000060622     C                   ENDSR
403100060608
403200000000     c*--------------------------------------------------------------------------------------------*
403300000000     c* operazioni iniziali
403400000000     c*--------------------------------------------------------------------------------------------*
403500000000     c     *inzsr        BEGSR
403600000000     c*
403700000000     c* ricevimento parametri
403800000000     c     *ENTRY        PLIST
403900000000     c                   PARM                    esito
404000000000     c                   PARM                    tis174dsi
404100000000     c                   PARM                    tis174dso
404200000000     c*
404300000000     c* chiavi di lettura
404400000000     c     keytas        KLIST                                                  titas30c
404500000000     c                   KFLD                    kasaas                         -anno spedizione
404600000000     c                   KFLD                    kaslnp                         -linea partenza
404700000000     c                   KFLD                    kasnrs                         -serie
404800000000     c                   KFLD                    kasnsp                         -spedizione
404900130312     c     karb          KLIST                                                  titas30c
405000130312     c                   KFLD                    evbaas                         -anno spedizione
405100130312     c                   KFLD                    evblnp                         -linea partenza
405200130312     c                   KFLD                    evbnrs                         -serie
405300130312     c                   KFLD                    evbnsp                         -spedizione
405400130326     C     Kdst          KLIST
405500130326     C                   KFLD                    dstnpg
405600130326     C                   KFLD                    dstnfv
405700130326     C                   KFLD                    dstfgs
405800130326     c                   z-add     4             dstnpg
405900130326     C     Kcet          KLIST
406000130326     C                   KFLD                    arbLNA
406100130326     C                   KFLD                    pctNDC
406200130326     C                   KFLD                    arbpdc
406300130312     C                   KFLD                    TTRD
406400130312     C                   KFLD                    arbAAS
406500130312     C                   KFLD                    arbLNP
406600130312     C                   KFLD                    arbNRS
406700130312     C                   KFLD                    arbNSP
406800130312     C                   move      'CET'         TTRD              3
406900000000     c     keytaa        KLIST                                                  titaa30c
407000000000     c                   KFLD                    kaaaas                         -anno spedizione
407100000000     c                   KFLD                    kaalnp                         -linea partenza
407200000000     c                   KFLD                    kaanrs                         -serie
407300000000     c                   KFLD                    kaansp                         -spedizione
407400000000     c                   KFLD                    kaatrc                         -tipo anagrafica
407500000000     c     keyta4        KLIST                                                  tita430c
407600000000     c                   KFLD                    ka4aas                         -anno spedizione
407700000000     c                   KFLD                    ka4lnp                         -linea partenza
407800000000     c                   KFLD                    ka4nrs                         -serie
407900000000     c                   KFLD                    ka4nsp                         -spedizione
408000000000     c                   KFLD                    ka4trc                         -tipo riferimento
408100010925     c     ke2ta4        KLIST                                                  tita430c
408200010925     c                   KFLD                    ka4aas                         -anno spedizione
408300010925     c                   KFLD                    ka4lnp                         -linea partenza
408400010925     c                   KFLD                    ka4nrs                         -serie
408500010925     c                   KFLD                    ka4nsp                         -spedizione
408600000000     c     keyevb        KLIST                                                  fnevb01l
408700000000     c                   KFLD                    kvbaas                         -anno spedizione
408800000000     c                   KFLD                    kvblnp                         -linea partenza
408900000000     c                   KFLD                    kvbnrs                         -serie
409000000000     c                   KFLD                    kvbnsp                         -spedizione
409100010717     c     ke2evb        KLIST                                                  fnevb01l
409200010717     c                   KFLD                    kvbaas                         -anno spedizione
409300010717     c                   KFLD                    kvblnp                         -linea partenza
409400010717     c                   KFLD                    kvbnrs                         -serie
409500010717     c                   KFLD                    kvbnsp                         -spedizione
409600010717     c                   KFLD                    kvbdev                         -data evento
409700010717     c                   KFLD                    kvbhev                         -ora  evento
409800000000     c     keylbl        KLIST                                                  fnlbl01l
409900000000     c                   KFLD                    kblaan                         -anno seguente
410000000000     c                   KFLD                    kbllpn                         -linea seguente
410100000000     c                   KFLD                    kblnrn                         -serie seguente
410200000000     c                   KFLD                    kblnsn                         -spedizione seguente
410300000000     c     ke2lbl        KLIST                                                  fnlbl02l
410400000000     c                   KFLD                    kblaap                         -anno precedente
410500000000     c                   KFLD                    kbllpp                         -linea precedente
410600000000     c                   KFLD                    kblnrp                         -serie precedente
410700000000     c                   KFLD                    kblnsp                         -spediz.precedente
410800000000     c     keytab        KLIST                                                  tabel00f
410900000000     c                   KFLD                    kblkut                         -utente
411000000000     c                   KFLD                    kblcod                         -tabella
411100000000     c     keytbe        KLIST                                                  *tntbe01l
411200000000     c                   KFLD                    kbecod                          -tabella
411300000000     c                   KFLD                    kbeke1                          -chiave uno
411400000000     c                   KFLD                    kbeke2                          -chiave due
411500000000     c                   KFLD                    kbelin                          -lingua
411600000000     c                   KFLD                    kbesif                          -s.informativo
411700060623     c*    ke2tbe        KLIST                                                  *tntbe02l
411800060623     c*                  KFLD                    kbecod                          -tabella
411900060623     c*                  KFLD                    kbelin                          -lingua
412000060623     c*                  KFLD                    kbesif                          -s.informativo
412100000000     c     keyvss        KLIST                                                  *tivss02l
412200000000     c                   KFLD                    kssksu                          -cl.unificante
412300000000     c                   KFLD                    kssisv                          -tipo servizio
412400010717     c     keyvtd        KLIST                                                  *tivtd01l
412500010717     c                   KFLD                    ktdksu                          -cl.unificante
412600000000     c     keycsb        KLIST                                                  *tncsb03l
412700000000     c                   KFLD                    ksbaas                         -anno spedizione
412800000000     c                   KFLD                    ksblnp                         -linea partenza
412900000000     c                   KFLD                    ksbnrs                         -serie
413000000000     c                   KFLD                    ksbnsp                         -spedizione
413100040906     c     keydct        KLIST
413200040906     c                   KFLD                    DCTAAC
413300040906     c                   KFLD                    DCTFIL
413400040906     c                   KFLD                    DCTNCA
413500000000     c     keygcp        KLIST                                                  *figcp01l
413600000000     c                   KFLD                    kcpaas                         -anno spedizione
413700000000     c                   KFLD                    kcplnp                         -linea partenza
413800000000     c                   KFLD                    kcpnrs                         -serie
413900000000     c                   KFLD                    kcpnsp                         -spedizione
414000000000     c                   KFLD                    kcpfrg                         -prg aperture
414100050404     c     K04GCP51      KLIST
414200050404     c                   KFLD                    kcpaas                         -anno spedizione
414300050404     c                   KFLD                    kcplnp                         -linea partenza
414400050404     c                   KFLD                    kcpnrs                         -serie
414500050404     c                   KFLD                    kcpnsp                         -spedizione
414600000000     c     keygnp        KLIST                                                  *fignp01l
414700000000     c                   KFLD                    knpagc                         -anno apertura
414800000000     c                   KFLD                    knpfgc                         -filiale apertura
414900000000     c                   KFLD                    knpngc                         -numero giacenza
415000000000     c                   KFLD                    knpfrg                         -prg apertura
415100040531     c     keyar5        KLIST                                                  *fiar531c
415200030206     c                   KFLD                    kr5aas                         -anno spedizione
415300030206     c                   KFLD                    kr5lnp                         -linea partenza
415400030206     c                   KFLD                    kr5nrs                         -serie
415500030206     c                   KFLD                    kr5nsp                         -spedizione
415600030206     c                   KFLD                    kr5trd                         -tipo record
415700040907
415800000000     c* apre i files
415900000000     c                   OPEN      titas30c
416000000000     c                   OPEN      titaa30c
416100000000     c                   OPEN      tita430c
416200000000     c                   OPEN      fnevb01l
416300080131     c                   OPEN      fnevb22l
416400060622     c*                  OPEN      tabel00f
416500060623     c*                  OPEN      tntbe01l
416600060623     c*                  OPEN      tntbe02l
416700000000     c                   OPEN      tivss02l
416800010716     c                   OPEN      tivtd01l
416900000000     c                   OPEN      azorg01l
417000000000     c                   OPEN      fnlbl01l
417100000000     c                   OPEN      fnlbl02l
417200050309     c                   OPEN      tigcp51l
417300050309     c                   OPEN      tignp01l
417400000000     c                   OPEN      tncsb03l
417500040906     c                   OPEN      fndct01l
417600040531     c                   OPEN      fiar531c
417700130312     c                   OPEN      fnarb01l
417800130326     c                   OPEN      fidst01l
417900130312     c                   OPEN      fipct02l
418000060621     C                   CALLP     openTabel00f()
418100000000     c*
418200000000     c* caricamento tabelle occorrenti
418300000000     c                   EXSR      cartab
418400060621     C                   CALLP     inzLingue()
418500000000     c*
418600000000     c                   ENDSR
418700070109
418800070109     ***********************************************************************************************
418900070109     **
419000070109     ** Evento da annullare. Per esempio, l'evento NIC annulla l'evento MIC della stessa ora.
419100070109     **
419200070109     ***********************************************************************************************
419300070109     C     eventoAnnulla BEGSR
419400070109
419500070109     C                   IF        §iceEvAn <> *BLANK
419600070109     C                   FOR       j = 1 TO %ELEM(smCev)
419700070109     C                   IF        §iceEvAn = smCev(j) AND
419800070109     C                             evbDevHev = smDevHev(j)
419900070109     C                   CLEAR                   smCev(j)
420000130312     C                   CLEAR                   smspe(j)
420100070109     C                   CLEAR                   smDevHev(j)
420200070109     C                   CLEAR                   smi(j)
420300070109     C                   CLEAR                   smSeql(j)
420400070109     C                   CLEAR                   smSeqe(j)
420500070109     C                   CLEAR                   smDeve(j)
420600070109     C                   CLEAR                   smHeve(j)
420700070109     C                   CLEAR                   smFle(j)
420800070109     C                   CLEAR                   smDev(j)
420900070109     C                   LEAVE
421000070109     C                   ENDIF
421100070109     C                   ENDFOR
421200070109     C                   ENDIF
421300070109
421400070109     C                   ENDSR
421500080131
421600080131     P*--------------------------------------------------
421700080131     P* Procedure name: getRealeDataOraEvento
421800080131     P* Purpose:        Reperisce la reale data e ora evento da estensione ...
421900080131     P*                          eventi bolle.
422000080131     P* Returns:
422100080131     P* Parameter:      piEvb2aas => ID bolla: anno.
422200080131     P* Parameter:      piEvb2lnp => ID bolla: filiale partenza.
422300080131     P* Parameter:      piEvb2nrs => ID bolla: serie.
422400080131     P* Parameter:      piEvb2nsp => ID bolla: numero spedizione.
422500080131     P* Parameter:      piEvb2dtv => Data evento.
422600080131     P* Parameter:      piEvb2orv => Ora evento.
422700080131     P* Parameter:      piEvb2cev => Causale evento.
422800080131     P* Parameter:      piEsito => Esito.
422900080131     P* Parameter:      piEvb2dev => Data evento reale.
423000080131     P* Parameter:      piEvb2hev => Ora evento reale.
423100080131     P*--------------------------------------------------
423200080131     P getRealeDataOraEvento...
423300080131     P                 B
423400080131     D getRealeDataOraEvento...
423500080131     D                 PI
423600080131     D  piEvb2aas                          LIKE(evb2aas)
423700080131     D                                     CONST
423800080131     D  piEvb2lnp                          LIKE(evb2lnp)
423900080131     D                                     CONST
424000080131     D  piEvb2nrs                          LIKE(evb2nrs)
424100080131     D                                     CONST
424200080131     D  piEvb2nsp                          LIKE(evb2nsp)
424300080131     D                                     CONST
424400080131     D  piEvb2dtv                          LIKE(evb2dtv)
424500080131     D                                     CONST
424600080131     D  piEvb2orv                          LIKE(evb2orv)
424700080131     D                                     CONST
424800080131     D  piEvb2cev                          LIKE(evb2cev)
424900080131     D                                     CONST
425000080201     D  piEvbDev                           LIKE(evb2Dev)
425100080201     D                                     CONST
425200080201     D  piEvbHev                           LIKE(evb2Hev)
425300080201     D                                     CONST
425400080131     D  piEsito                      10I 0
425500080201     D  piEvb2Dev                          LIKE(evb2Dev)
425600080201     D  piEvb2Hev                          LIKE(evb2Hev)
425700080131
425800080131     C     k07evb22      KLIST
425900080131     C                   KFLD                    piEvb2Aas
426000080131     C                   KFLD                    piEvb2Lnp
426100080131     C                   KFLD                    piEvb2Nrs
426200080131     C                   KFLD                    piEvb2Nsp
426300080131     C                   KFLD                    piEvb2Dtv
426400080131     C                   KFLD                    piEvb2Orv
426500080131     C                   KFLD                    piEvb2Cev
426600080131
426700080131     C                   CLEAR                   piEsito
426800080131     C     k07evb22      CHAIN     fnevb22l
426900080131     C                   IF        NOT %FOUND()
427000080131     C                   EVAL      piEsito = 1
427100080201     C                   EVAL      piEvb2Dev = piEvbDev
427200080201     C                   EVAL      piEvb2Hev = piEvbHev
427300080131     C                   RETURN
427400080131     C                   ENDIF
427500080131     C                   EVAL      piEvb2Dev = evb2Dev
427600080131     C                   EVAL      piEvb2Hev = evb2Hev
427700080131
427800080131     P getRealeDataOraEvento...
427900080131     P                 E
428000080131
428100100119
428200100119     P*--------------------------------------------------
428300100119     P* Procedure name: GetDescrizioneIncassoContrassegno
428400100119     P* Purpose:        Restituisce la descrizione della modalità di incass...
428500100119     P*                          o del contrassegno.
428600100121     P* Returns:        Descrizione modalità incasso contrassegno
428700100119     P*--------------------------------------------------
428800100119     P GetDescrizioneIncassoContrassegno...
428900100119     P                 B
429000100119     D GetDescrizioneIncassoContrassegno...
429100100119     D                 PI            35A
429200100119
429300100121     C                   RESET                   tibs02ds
429400100121     C                   EVAL      t02Cod = 'I1A'
429500100121     C                   EVAL      t02Lin = langTntbe
429600100119     ** Il contrassegno è stato incassato
429700100121     C                   IF        csbDdc > *ZERO OR csbAas < 2010
429800100121     C                   EVAL      t02Ke1 = csbTpa + csbTpi
429900100121     C                   ELSE
430000100119     ** Il contrassegno non è stato incassato.
430100100121     C                   IF        ar5Nsp <> csbNsp OR ar5Lnp <> csbLnp OR
430200100121     C                             ar5Nrs <> csbNrs OR ar5Aas <> csbAas OR
430300100119     C                             ar5Trd <> 'GEN'
430400100121     C                   EVAL      kr5Aas = csbAas
430500100121     C                   EVAL      kr5Lnp = csbLnp
430600100121     C                   EVAL      kr5Nrs = csbNrs
430700100121     C                   EVAL      kr5Nsp = csbNsp
430800100119     C                   EVAL      kr5Trd = 'GEN'
430900100119     C     keyAr5        CHAIN     fiar531c
431000100119     C                   IF        %FOUND()
431100100119     C                   EVAL      dAr5Gen = ar5Uni
431200100119     C                   ELSE
431300100119     C                   RESET                   dAr5Gen
431400100119     C                   RETURN                  *BLANK
431500100119     C                   ENDIF
431600100119     C                   ENDIF
431700100121     C                   IF        gen§Ar5TicMb <> *BLANK OR gen§Ar5FlgMb = 'S'
431800100119     C                   EVAL      t02Ke1 = gen§Ar5TicMb
431900100119     C                   ELSE
432000100119     C                   EVAL      t02Ke1 = gen§Ar5TicI
432100100119     C                   ENDIF
432200100121     C                   ENDIF
432300100121     ** Recupero la descrizione della modalità di incasso del contrassegno.
432400100120     C                   CALLP     tibs02r(kpjba : tibs02ds)
432500100119     C                   IF        t02Err = 'E'
432600100119     C                   RETURN                  *BLANK
432700100119     C                   ENDIF
432800100120     C                   EVAL      dI1a = t02Uni
432900100119     C                   RETURN                  dI1a.descrizion
433000100119
433100100119     P GetDescrizioneIncassoContrassegno...
433200100119     P                 E
433300100119
433400110304
433500110304     P*--------------------------------------------------
433600110304     P* Procedure name: IsContrassegnoCompensato
433700110304     P* Purpose:        Restituisce *ON se il contrassegno è stato compensa...
433800110304     P*                          to con una fattura.
433900110304     P* Returns:        *ON = Compensato
434000110304     P*--------------------------------------------------
434100110304     P IsContrassegnoCompensato...
434200110304     P                 B
434300110304     D IsContrassegnoCompensato...
434400110304     D                 PI              N
434500110304
434600110304     D retField        S               N   INZ(*OFF)
434700110304
434800110307     ** Il contrassegno non è stato rimborsato.
434900110307     C                   IF        csbBna <> 9999999 OR csbNdp <> 9999999
435000110307     C                   RETURN    retField
435100110307     C                   ENDIF
435200110307
435300110307     C/EXEC SQL
435400110307     C+ SELECT '1'
435500110307     C+ INTO :retField
435600110307     C+ FROM tncsv00f
435700110307     C+ WHERE csvAas = :csbAas
435800110307     C+   AND csvLnp = :csbLnp
435900110307     C+   AND csvNrs = :csbNrs
436000110307     C+   AND csvNsp = :csbNsp
436100110307     C+   AND csvCav = 'COMP'
436200110307     C+ FETCH FIRST ROW ONLY
436300110307     C/END-EXEC
436400110304
436500110307     C                   SELECT
436600110307     C                   WHEN      sqlCode < 0
436700110307     C                   DUMP(A)
436800110307     C                   ENDSL
436900110304
437000110307     C                   RETURN    retField
437100110304
437200110304     P IsContrassegnoCompensato...
437300110304     P                 E
437400110304
437500120321
437600120321     P*--------------------------------------------------
437700120321     P* Procedure name: SetIdAssegnoMittente
437800120321     P* Purpose:        Imposta l'ID assegno mittente.
437900120321     P* Returns:        Esito.
438000120321     P* Parameter:      priNumeroAssegno => Numero assegno oppure chiave as...
438100120321     P*                          segni mittente.
438200120321     P* Parameter:      priAbi => ABI assegno.
438300120321     P* Parameter:      priCab => CAB assegno.
438400120321     P*--------------------------------------------------
438500120321     P SetIdAssegnoMittente...
438600120321     P                 B
438700120321     D SetIdAssegnoMittente...
438800120321     D                 PI            10I 0
438900120321     D  priNumeroAssegno...
439000120321     D                                     LIKE(caNumO74)
439100120321     D  priAbi                             LIKE(caAbiO74)
439200120321     D  priCab                             LIKE(caCabO74)
439300120321
439400120321     D retField        S             10I 0 STATIC
439500120321
439600120321      /FREE
439700120321
439800120321       CLEAR retField;
439900120321
440000120321       // Quando il numero assegno è pieno con 10 cifre e l'ABI è vuoto
440100120321       // significa che abbiamo ricevuto dal destinatario più di 1 assegno
440200120321       // e il numero assegno contiene la chiave di accesso a TNCSM00F.
440300120321       // In questo caso restituisco l'ID del primo assegno.
440400120321
440500120321       IF %SUBST(priNumeroAssegno : 10 : 1) = *BLANK OR priAbi > *ZERO;
440600120321         RETURN retField;
440700120321       ENDIF;
440800120321
440900120321       EXEC SQL
441000120321         SELECT csmNra, csmAbi, csmCab
441100120321           INTO :priNumeroAssegno, :priAbi, :priCab
441200120321           FROM tncsm00f
441300120321           WHERE csmKey = :priNumeroAssegno
441400120321           FETCH FIRST 1 ROW ONLY
441500120321       ;
441600120321
441700120321       SELECT;
441800120321         WHEN sqlCode = 100;
441900120321           retField = sqlCode;
442000120321         WHEN sqlCode < *ZERO;
442100120321           DUMP(A);
442200120321           retField = sqlCode;
442300120321       ENDSL;
442400120321
442500120321       RETURN retField;
442600120321
442700120321      /END-FREE
442800120321     P SetIdAssegnoMittente...
442900120321     P                 E
443000120321
443100130513
443200130513     P*--------------------------------------------------
443300130513     P* Procedure name: GetFilUrl
443400130513     P* Purpose:        Restituisce l'URL della filiale.
443500130513     P* Returns:        URL filiale.
443600130513     P* Parameter:      piFiliale => ID filiale
443700130513     P*--------------------------------------------------
443800130513     P GetFilUrl       B
443900130513     D GetFilUrl       PI           256A
444000130513     D  piFiliale                     3P 0 CONST
444100130513
444200130513     C                   IF        piFiliale = *ZERO
444300130513     C                   RETURN    *BLANK
444400130513     C                   ENDIF
444500130513     C                   RESET                   tibs02ds
444600130513     C                   EVAL      t02Cod = 'UFI'
444700130513     C                   EVAL      t02Ke1 = %EDITC(piFiliale : 'X')
444800130513     C                   EVAL      t02Lin = langTntbe
444900130513     C                   CALLP     tibs02r(kpjba : tibs02ds)
445000130513     C                   IF        t02Err = 'E'
445100130513     C                   RETURN    *BLANK
445200130513     C                   ENDIF
445300130513     C                   RETURN    t02Uni
445400130513
445500130513     P GetFilUrl       E
445600130513
