000100050322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200051130     H*PARMS BNDDIR(PJXBND PJCBND)
000300050322     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400050322     H/TITLE        -- FATTURAZIONE AUTOMATICA --                    *
000500050322     H*--------------------------------------------------------------*
000600050322     H**  H5 - CHAIN PER NUMERI DI FATTURA
000700050322     H**  H7 - CARICAMENTO TABELLE IN GENERE
000800050322     H**
000900050322     Fansog01l  if   e           k disk
001000050322     Fanind01l  if   e           k disk
001100050322     Fancln01l  if   e           k disk
001200050322     Fandic01l  if   e           k disk
001300050322     Fancba01l  if   e           k disk
001400050322     Fancdp01l  if   e           k disk
001500050322     Fanriv01l  if   e           k disk
001600050322     Fybacl01l  uf   e           k disk
001700060220     Fcnclp00f  uf   e           k disk    infds(cnclpinf)
001800050322     Fndbhm00f  o    e           k disk
001900050322     Fndbhi00f  o    e           k disk
002000050322     Fndbha00f  o    e           k disk
002100050322     Ftitas02l  uf   e           k disk
002200050429     Ftitas30c  uf   e           k disk    ignore(titas010)
002300050429     F                                     ignore(titasp00)
002400050429     F                                     rename(titas000:titas30)
002500050322     Ftita730c  uf a e           k disk    ignore(tita7p00)
002600050322     Ftitaa30c  if   e           k disk    ignore(titaap00)
002700050322     Ftita430c  if   e           k disk    Ignore(tita4p00)
002800050322     Fwfrtas1l  uf a e           k disk    prefix(wf_)
002900070117     Fwfrig00f  uf a e           k disk
003000050322     Ftncsb03l  if   e           k disk
003100060601     F******fnevb04l  if   e           k disk
003200050322     Fazorg01l  if   e           k disk
003300050322     Ftabel00f  if   e           k disk
003400050322     Ftntbe01l  if   e           k disk
003500050322     Ffnspe03l  if   e           k disk
003600051110     Ffnsp201l  if   e           k disk
003700051118     Ftfntc01l  if   e           k disk
003800050322     Ffiar531c  if   e           k disk
003900050322     Fetlav00f  o  a e             disk
004000060214     FTiift00f  o  a e             disk
004100060428     Ftitai01l  uf a e           k disk
004200060227
004300060227     fWfmtc00f  o    e             Disk    usropn
004400050322
004500050421     FPRTF198   O    F  198        PRINTER usropn
004600050322     FFTWCODFM  O    F  198        PRINTER USROPN
004700050322     F                                     FORMLEN(100)
004800050322     F                                     FORMOFL(100)
004900050322     FFTWCODIM  O    F  198        PRINTER USROPN
005000050322     F                                     FORMLEN(100)
005100050322     F                                     FORMOFL(100)
005200050322     FFTWCODDS  O    F  198        PRINTER USROPN
005300050322     F                                     FORMLEN(100)
005400050322     F                                     FORMOFL(100)
005500050322     F                                     INFDS(PRTPAG)
005600050322     FDISSPE    O    F  132        PRINTER USROPN
005700050421     FQSYSPRT   O    F  132        PRINTER usropn
005800050421     FSYSPRT    O    F  132        PRINTER usropn
005900991223     d*
006000991223
006100050322     D x59abids      e ds                  inz
006200050322
006300981021     D AI              S              8    DIM(5)                               ALIQUOTE IVA O ES
006400000118     D IMDW            S             15  3 DIM(5)                               IMPONIB.IVA DEC wrk
006500000114     D IMD             S             14  2 DIM(5)                               IMPONIBILE IVA  DEC
006600000105     D IMC             S             14  2 DIM(5)                               IMPON.IVA mon/conto
006700000105     D IVD             S             14  2 DIM(5)                               IVA  X AL.IVA   DEC
006800000105     D IVC             S             14  2 DIM(5)                               IVAXAL.IVAmon/conto
006900000118     D V1D             S             14  3 DIM(5)                               VARIE1 X AL.IVA DEC
007000000118     D POD             S             15  3 DIM(5)                               PORTO X AL.IVA  DEC
007100000000     D CEI             S              1    DIM(100)                             ESENZIONI IVA-TAB
007200000000     D DES             S             15    DIM(100)                             DESCR.ES.IVA
007300041220     D CTS             S              2    DIM(600)                             C.TASS -TABELLA
007400981021     D CC              S              8    DIM(50)                              CONTI XCC-TABELLA
007500020320     D CB              S              8    DIM(50)                              CONTI XCC-TABELLA
007600070220     D SVR             S              1    DIM(24)                              SIG.VARIE CONTARIGHE
007700070220     D WSVR            S              1    DIM(24)                              SIG.VARIE CONTARIGHE
007800960906     D S               S              1    DIM(24)                              SIGLA VARIE ST
007900000124     D ID              S              7  3 DIM(24)                              IMPORTI VARIE ST DEC
008000960905     D S2              S              1    DIM(3)                               SIGLA VARIE ST
008100000124     D I2D             S              7  3 DIM(3)                               IMPORTI VARIE ST DEC
008200000000     D TB              S              2    DIM(20)                              TIPI BOLLA
008300981021     D CCP             S             22    DIM(50)                              CONTO/S-CONTO C/P
008400000114     D ICPD            S             10  2 DIM(50)                              IMPORTI CONTROPARDEC
008500000120     D ICPDW           S             11  3 DIM(50)                              IMPORTI CONTROPARDEC
008600000114     D ICPC            S             13  3 DIM(50)                              IMP.CONTROP.mon/cont
008700891027     D FCP             S              1  0 DIM(50)                              FLAG COMBIN.C/FIL
008800941113     D TS              S              1    DIM(10)                              TIPI SERVIZIO
008900030121     D CTR             S              1  0 DIM(50)                              CODICI TARIFFA
009000000207     D DV              S              3    DIM(5)                               DIVISE
009100050218     D VUS             S              1    DIM(10)                              TASXCO DA ESCLUDERE
009200020320     D* schiera per stampa legende
009300941118     D LES             S              1    DIM(20)                              LEGENDA VARIE SIGLE
009400941118     D LED             S             10    DIM(20)                              LEGENDA VARIE DESCR.
009500000322     D LIN             S              1    DIM(20)                              SEPARATORE
009600961108     D*
009700020320     D* schiera per legenda fedex
009800020320     D FES             S              1    DIM(20)                              LEGENDA VARIE SIGLE
009900020320     D* schiera per legenda altri network
010000020320     D NES             S              1    DIM(20)                              LEGENDA VARIE SIGLE
010100020320     d*
010200000322     D LDPD            S              3  0 DIM(20)                              LINEE DPD
010300020416     D* codici bolla che prevedono un imponibile =0
010400020416     D IM0             S              2    DIM(50)                              LEGENDA VARIE SIGLE
010500020416     D* codici bolla che prevedono la tassazione di una singola varia
010600020416     D CBO             S              2    DIM(50)                              LEGENDA VARIE SIGLE
010700020416     D CBV             S              1    DIM(50)                              LEGENDA VARIE SIGLE
010800020416     D*
010900011217     D NAZE            S              3    DIM(500)                             NAZIONI ESENTI
011000011217     D ESEN            S              3    DIM(500)                             NAZIONI ESENTI
011100050419      ** schiera totali codici di bollettazione
011200050419     d Sktot           s                   Like(dssktot) dim(300)
011300050419      *
011400020416     D E               S             52    DIM(10) CTDATA PERRCD(1)             ERRORI FATTURAZIO
011500000107     D SC              S             66    DIM(1)  CTDATA PERRCD(1)             STAMPA TOTALI GENERA
011600061011     D cmdc            s             90    dim(3) ctdata perrcd(1)              Cpyf Crtdupobj
011700060227
011800991201
011900001025     D PRTPAG          DS
012000001025     D  cur_page             369    372I 0
012100951031     D DS15          E DS                  INZ
012200951031     D* TABELLA NAZIONI
012300020531     D DTA4A         E DS                  INZ
012400000000     D PARAM           DS
012500981023     D  VIDDFI                        8  0
012600981023     D  VIDDFF                        8  0
012700981023     D  VIDDSP                        8  0
012800981023     D  VIDTFT                        1  0
012900981023     D  VIDTCL                        1  0
013000981023     D  VIDTSC                        1
013100981023     D  FIL                           3  0 dim(40)
013200981023     D  VIDRB                         2
013300981023     D  VIDDFT                        8  0
013400981023     D  VIDKSC                        7  0
013500981023     D  wsocieta                      3
013600000103     D  VIDDIV                        3                                         *divisa da fatturare
013700000103     D  VIDAGG                        1                                         *AGG.bolle altre div
013800000103     D  VIDSTA                        1                                         *stampa manca tariff
013900000103     D
014000060220      * - Per gestione rcd allocato
014100060220     d Trul82ds      e ds
014200060220
014300060324      * - Per recupero indirizzo mail del mittente
014400060324     d Trul44ds      e ds
014500060324
014600060228      * - Per manca tariffa
014700060228     d Tnsb44ds      e ds
014800060228
014900060220      * numero relativo di record per controllo allocazione record
015000060220     D cnclpinf        ds
015100060220     D  cnclpnrri            397    400i 0
015200060220
015300991223      * PASSO AL PGM DI TASSAZIONE UN FLAG PER IL CONTROLLO SOLO DELLA
015400991223      *  ASSICURAZIONE
015500980526     D PARAMT          DS
015600980526     D  PARCA                256    256
015700030121     D DSTR          E DS                  OCCURS(50) INZ
015800991223      ** DS CALCOLO TASSAZIONE
015900991109     D DTAS          E DS
016000011129     D  dstasflo     E                     extfld(tasflo)
016100060922      ** DS Flag operativi DS DTAS
016200060922     d Dtas01        e ds
016300050325      ** DS Calcolo tassazione  - Varie
016400050325     d Dtasv         e ds
016500050325     d  sv                     1     20
016600050325     d                                     Dim(20)                              Sigle varie
016700050325     d  va                    21    140P 3
016800050325     d                                     Dim(20)                              Importi varie
016900050325      ** DS  VARIE per scrittura file di lavoro tas
017000050325     d Dtasvwf       e ds                  extname(dtasv) prefix(WF_)
017100060428     d  WF_sv                  1     20
017200060428     d                                     Dim(20)                              Sigle varie
017300060428     d  WF_va                 21    140P 3
017400060428     d                                     Dim(20)                              Importi varie
017500991223      ** SIGLE VARIE
017600941108     D DS$2          E DS
017700991223      * DATI VARI CONTABILITA'
017800941107     D DSQC1         E DS
017900991223      * DATI VARI CONTABILITA'
018000941105     D DSQC2         E DS
018100991223      * DATI VARI TASSAZIONE 1
018200941107     D DSQT1         E DS
018300000103      * DATI VARI TASSAZIONE PER MONETA
018400050329     D DGEI          E DS
018500991223      ** TIPI BOLLA
018600941105     D DSTB          E DS                  OCCURS(20)
018700991223      ** ESENZIONE IVA
018800941104     D DSEI          E DS                  OCCURS(100)
018900991223      ** CODICI DI TASSAZIONE
019000041220     D DSCT          E DS                  OCCURS(600)
019100991223      ** VOCI BOLLA - DESCRIZIONI
019200991223      ** VOCI BOLLA - CONTABILITA'
019300981021     D DSCC          E DS                  OCCURS(50)
019400020320     D DSCB          E DS                  OCCURS(50)
019500991223      ** TIPI SERVIZIO
019600980224     D DS5E          E DS
019700020416      ** Codici bolla
019800020416     D DS3A          E DS
019900000103      ** DIVISA
020000000107     D** CAMBIO DIVISA
020100000111     D YEUR          E DS                  EXTNAME(YEURCODS)
020200000111     D** CONTI CONTABILI PER REGISTRARE ARROTONDAMENTI CAMBI
020300000111     D DARR          E DS                  OCCURS(2)
020400000322     D** ds per recupero parcel DPD
020500060703     D DSBL4I        E DS
020600000322     D** ds per recupero lnp    DPD
020700000322     D OG143         E DS
020800031010     D** ds pilota fatturazione FAT
020900031010     D DFAT          E DS
021000050218     D** ds VALORI TASXCO DA ESCLUDERE DALLA FATTURAZIONE
021100050218     D DVUS          e ds
021200991223      **
021300000000     D KPJBA         E DS
021400050415     D DSLV55        E DS                  EXTNAME(FNLV55DS)
021500050329     D DSBS69        E DS                  EXTNAME(TIBS69DS)
021600050415     D DSBS02        E DS                  EXTNAME(TIBS02DS)
021700990715     D DSACO         E DS                  EXTNAME(CNACO00F) prefix(c)
021800990715     D DSIND         E DS                  EXTNAME(CNIND00F) prefix(c)
021900990715     D DSCLP         E DS                  EXTNAME(CNCLP00F) prefix(c)
022000990715     D DSCLS         E DS                  EXTNAME(FNCLS00F) prefix(c)
022100030204
022200030204     d dAr5Ban       e ds
022300030908
022400030908     d dAr5Bnb       e ds
022500011115     D*
022600941113     D                 DS                  INZ
022700941114     D  KSCPRE                 1      7  0
022800941107     D  PRECOD                 4      7  0
022900990715     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
023000990715     D  TCU                  398    697
023100990715     D                                     DIM(50)
023200990715     D  KCU                  698    847P 0
023300990715     D                                     DIM(50)
023400990715     D                                     PACKEVEN
023500990715     D TCUDS           DS
023600990715     D  F1                     1      1
023700990715     D  F3                     3      3
023800990715     D  F2                     2      2
023900990715     D  F4                     4      4
024000990715     D  F56                    5      6
024100991223      *
024200991223      * DEFINIZIONE COSTANTI
024300991223      *
024400991223     D CCODIF          C                   CONST('CODIFICATI       ')
024500991223     D CVARI           C                   CONST('VARI             ')
024600991223     D CENTRA          C                   CONST('CODIFICATI E VARI')
024700020320     D Clegenda        C                   CONST('Legenda:            ')
024800020320     D Clegendafed     C                   CONST('Legenda bolle FEDEX:')
024900020827     D Cpesovdl        C                   CONST('Qualora i colli di una spediz-
025000020827     d                                     ione vengano pesati, viene fattur-
025100020827     d                                     ato il peso riscontrato')
025200021104     D Cesenz          C                   CONST('Bollo assolto in modo virtual-
025300021104     d                                     e AUT.AG.ENTRATE BOLOGNA 3 del 27/09-
025400021104     d                                     /02 prot.2002/31599 ')
025500060719     D Cesenz_new      C                   CONST('Bollo assolto in modo virtual-
025600060719     d                                     e AUT.AG.ENTRATE MILANO 2 del 10/07/-
025700060719     d                                     06 prot.2006/45114 ')
025800060220
025900060220     d msgwin          C                   CONST('Si sta bloccando la FATTURAZI-
026000060220     d                                     ONE DI SEDE : USCIRE SUBITO da quest-
026100060220     d                                     o lavoro!')
026200060220
026300060328     d constmail       C                   CONST('Info607@Bartolini.it')
026400020827
026500991223      *---------------------------------------------------------------------------------------------
026600991223
026700991223     D soc001        e ds                  extname(xsoc001ds)
026800000209     D ndc071ds      E DS                  inz
026900991223     D xsocds          DS          1000
027000991223     d dsconti         ds
027100991223     d  wkcc                          6
027200991223     d  wksc                          8
027300991223     d  wvoc                          8
027400991223     d wivads          ds
027500991223     d  fatsci                        3
027600991223     d  fatali                        5
027700050419
027800050419     d dssktot         ds
027900050419     d  Tksc                          7  0
028000050419     d  Trsc                         36
028100050419     d  Tfra                          9  3
028200050419     d  Tass                          9  3
028300050419     d  Timp                         10  3
028400050617     d  Tpgi                          3  0                                      pag. iniziale
028500050617     d  Tpgf                          3  0                                      pag. finale
028600991223      *---------------------------------------------------------------------------------------------
028700991223
028800981021     d dataiso         s               d   datfmt(*iso)
028900981021     d dataeur         s               d   datfmt(*eur)
029000991223     d  XNMDAG         s               d   datfmt(*iso)
029100991223     d  XNMDAT         s               d   datfmt(*iso)
029200000217     d wrkdff          s               d   datfmt(*iso)
029300991223      *---------------------------------------------------------------------------------------------
029400991223
029500050419     d* campi di comodo per stampa riferimento numerico mittente
029600050419     d rma15           s             15
029700050419     d Wrma15          s             15
029800050419     d rma09           s              9
029900050419
030000050323     d tipbol          s              1                                         F= Franco A= Assegna
030100050323     d kscfil          s              3  0                                      P.O. del cliente
030200050331     d filpre          s              3  0                                      P.O. salvato
030300050323     d ksccod          s              4  0                                      ultimi 4 byte codcli
030400050323     d wccm            s              4  0                                      ultimi 4 byte codmit
030500030617     d cal_vard        s              1    inz                                  pilota calc.varia D
030600030922     d stp_vard        s              1    inz                                  pilota stamp.varia D
030700030922     d variadd         s              7  3                                      varia D da stampare
030800991223     d cmp198          s            198    inz
030900000114     d fatbold         s             15  2
031000000105     d totbold         s             15  2
031100000105     d impbold         s             15  2
031200050329     d wimv            s             13  3
031300000111     d w017a           s             17
031400000111     d w0130           s             13  0
031500060220     d msgjob          s             80
031600000103     d wimpor          s             18  3
031700000114     d prtpord         s              8  3
031800000104     d timd            s             14  3
031900001024     d TDSP            s             18  0
032000001024     d TDSPD           s             17  3
032100030617     d*$* fatsind         s              5  3
032200030617     d dirfatt         s             11  3                                      *diritto fatturazion
032300060214     d fatdir          s             11  3                                      *diritto fatturazion
032400000114     d varied          s             14  2
032500000105     d prttpid         s             14  2
032600000105     d fattotd         s             15  2
032700000105     d fattotcnt       s             15  3                                      *tot.fat.moneta/cont
032800000105     d fatbolcnt       s             15  3                                      *bollo moneta/conto
032900000107     d bolra1          s             48                                         *rag.soc.bollettaz.
033000000107     d bolvia          s             30                                         *indiriz."  "  "
033100000107     d bolcit          s             30                                         *città   "  "  "
033200000107     d bolcap          s              5  0                                      *cap     "  "  "
033300000107     d bolcae          s              9                                         *cap estero "  "
033400000107     d bolprv          s              2                                         *provincia  "  "
033500000107     d bolsta          s              3                                         *stato   "  "  "
033600050420     d bscra1          s             48                                         *rag.soc.bollettaz.
033700050420     d bscvia          s             30                                         *indiriz."  "  "
033800050420     d bsccit          s             30                                         *città   "  "  "
033900050420     d bscprv          s              2                                         *provincia  "  "
034000050421     d bsccap          s              5  0                                      *cap     "  "  "
034100050421     d bsccae          s              9                                         *cap estero "  "
034200050421     d bscsta          s              3                                         *stato   "  "  "
034300050411     d fatra1          s             48                                         *rag.soc.fatturazion
034400050411     d fatra2          s             41                                         *per 2 intest
034500050411     d fatifm          s              1                                         *inizio/fine mese
034600050411     d fatcdp          s              3                                         *codice pagamento
034700050411     d fatsta          s              3                                         *nazione
034800050411     d fativa          s             16                                         *partita IVA
034900061108     d fatcdf          s             16                                         *codice fiscale
035000020827     d stapkf          s                   like(taspkf)
035100020827     d div1            s                   like(tasdiv)
035200000104     d div2            s                   like(tasdiv)
035300020827     d stavdl          s             85
035400021104     d staese          s             85
035500020827     d wcalcevdl       s              1
035600000107     d impgen          s             18  3                                      *totale imponibile
035700000107     d ivagen          s             18  3                                      *totale iva
035800000107     d bolgen          s             18  3                                      *totale bolli
035900000107     d totgen          s             18  3                                      *totale fatturato
036000000125     d numgen          s             12  0                                      *Numero fatture emes
036100010927     d numspe          s             12  0                                      *Num. spediz. x dist
036200000124     d impgencnt       s             18  3                                      *totale imponib. mc
036300000124     d ivagencnt       s             18  3                                      *totale iva mc
036400000124     d bolgencnt       s             18  3                                      *totale bolli mc
036500000124     d totgencnt       s             18  3                                      *totale fatturato mc
036600000111     d wtotfat         s             19  3                                      *totale fattura
036700010924     d wtotfatd        s             19  3                                      *tot.fattura decimal
036800000111     d wtotimc         s             19  3                                      *totale imponibile
036900000111     d wtotivc         s             19  3                                      *totale iva
037000000117     d wtoticpc        s             19  3                                      *totale contropartie
037100000111     d diffe           s             19  3                                      *differenze cambio
037200010924     d wtotivd         s             19  3                                      *totale iva decimali
037300010924     d wtoticpd        s             19  3                                      *totale controp.deci
037400060502     d comwf_sv        s             20                                         *comodo varie tassat
037500000110     d stprmo          s              1                                         *flg stp Rif.Mitt.Or
037600000322     d stppar          s              1                                         *flg stp Parcel nbr
037700000322     d lnpdpd          s              1                                         *flg lnp DPD = 'S'
037800000111     d esitoa          s              1
037900000111     d noctrscos       s              1
038000000111     d impocnv         s                   like(yecimi)
038100000111     d wmitori         s             25                                         *mitt.originale stp
038200020523     d wmitalf         s             15                                         *mitt.originale stp
038300060703     d wparcel         s             14                                         *parcel
038400000208     d wrkdiv          s              3                                         *campo lavoro divisa
038500000322     d com12           s             12                                         *campo lavoro
038600001024     d YY              s              3  0                                      *INDICE SCHIERA
038700011116     d XX              s              3  0                                      *INDICE SCHIERA
038800030512     d II              s              2  0                                      *INDICE SCHIERA
038900060428     d VV              s              2  0                                      *INDICE SCHIERA
039000070219     d A0              s              2  0                                      *INDICE SCHIERA
039100070219     d A1              s              2  0                                      *INDICE SCHIERA
039200060227
039300061012     d wlibc           s              9                                          libreria crtdupobj
039400061011     d comman          s             90                                                    "
039500061011     d lenght          s             15  5 inz(90)                                         "
039600060228     d com8a           s              8
039700060227
039800030512      *
039900000322     d §4trc           s                   like(ta4trc) inz('F')                *tipo record ta4 DPD
040000001025     d §nrfd           s             15  0                                      *num records dds
040100030204
040200030909     d desESIVA        s             20                                         *desc.esenzione IVA
040300030909
040400030908     d kAr5Trd         s                   Like(Ar5Trd)
040500030908     d savncl          s                   Like(TasNcl)
040600030908     d stpncl          s                   Like(TasNcl)
040700030908
040800011116     d antepo          s                   like(tasimv)
040900050412     d preimv          s                   like(tasimv)
041000011115     d W_tasdiv        s                   like(tasdiv)
041100011115     d W_dtas          s                   like(dtas)
041200011115     d W_dtasv         s                   like(dtasv)
041300050218
041400050218     D Ktbe            s                   like(tbecod)
041500050325
041600050408     D K£lin           s                   like(clnlineav)
041700050408     D K£fil           s                   like(clnfiliale)
041800050408     D Cdin            s                   like(indcdind)
041900050408     D tpin            s                   like(indtpind)
042000050408
042100051118     D Kapl            s                   LIKE(NTCapl) inz('C')
042200051118     D Knk1            s                   LIKE(NTCnk1)
042300051118     D Knk2            s                   LIKE(NTCnk2)
042400051118     D Ktnt            s                   LIKE(NTCtnt) inz('84')
042500051118
042600050415     D Ktasksc         s                   like(clnksc)
042700050421     D Ktasbsc         s                   like(clnksc)
042800050325     D Savtasksc       s                   like(clnksc)
042900050325     D Bolksc          s                   like(tasksc)
043000050325     D Fatksc          s                   like(tasksc)
043100050331     D Wctprv          s                   like(§ctprv)
043200050331     D cod             s                   like(tblcod)
043300050331     D key             s                   like(tblkey)
043400050404     D tasrscpre       s                   like(wf_tasrsc)
043500050419     D tasbscpre       s                   like(wf_tasbsc)
043600050420     D tasfrgpre       s                   like(wf_tasfrg)
043700050330     D Savragbol       s              1
043800030909
043900050331     D w001a           s              1                                         comodo tipo bolla
044000050331     D datspe          s              8  0                                      data spedizione
044100050325     D Flgta7          s              1  0
044200050331     D cr              s              2  0                                      contatore righe
044300050331     D a               s              4  0                                      indice
044400050412     D a2              s              5  0                                      indice
044500050421     D itot            s              4  0                                      indice
044600050421     D itotW           s              4  0                                      indice
044700050331     D k               s              5  0                                      indice
044800050405     D wconta          s              4  0                                      contatore
044900050405     D como15          s             15  2                                      campo di comodo
045000050405     D wfcp            s              1  0                                      flag comb. fil.
045100050406     D cit2            s              2                                         provincia
045200050406     D wconto          s             22                                         conto
045300050406     D trigtd          s             15  0                                      tot rig autofattura
045400050406     D trigti          s             15  0                                      tot rig inizio mese
045500050406     D trigtf          s             15  0                                      tot rig finr   mese
045600050406     D wiva52          s              5  2                                      aliquota iva
045700050415     D wstcod          s              1  0                                      flag pilota sta test
045800050421     D wstbsc          s              1                                         flag pilota bolletaz
045900050406     D riga            s              9  0
046000050406     D rigai           s              9  0
046100050406     D condis          s              2  0
046200050407     D dspult          s              8  0                                      data spedizione magg
046300050407     D dsppri          s              8  0                                      data spedizione mino
046400050407     D periva          s              3  1                                      percent. IVA
046500050408     D pdfi            s             20                                         descriz. filiale
046600050408     D pvia            s             20                                         via
046700050408     D pcap            s              5                                         cap
046800050408     D ploc            s             20                                         località
046900050408     D ptel            s             13                                         telefono
047000050408     D pprv            s              2                                         provincia
047100050408     D pfax            s             13                                         fax
047200050408     D bolfil          s              3  0
047300070111     D comfil          s              3  0
047400050408     D asbol           s              3  0
047500050411     D asfat           s              3  0
047600050411     D wlbeti          s              3                                         flag postel
047700050412     D wanspe          s              1
047800051110     D wmail           s              1
047900061009     D indmail         s             61
048000050411     D fatkcc          s              4  0                                      capo conto
048100050411     D fatfil          s              3  0                                      filiale di fatturazi
048200050411     D fatscit         s              3
048300050411     D wwwage          s             16
048400050411     D wfades          s             28
048500050411     D nftfm           s              6  0                                      num fattura fine mes
048600050411     D nftim           s              6  0                                      num fattura iniz mes
048700050411     D fatnft          s              6  0                                      num fattura
048800050411     D wfipri          s              6  0                                      primo numero fattura
048900050411     D winpri          s              6  0                                      primo numero fattura
049000050411     D fatdft          s              8  0                                      data fattura
049100050411     D fatfiv          s              3  0                                      libro iva
049200050411     D stadft          s              6  0                                      data fattura
049300050411     D wfine           s              1
049400050411     D winiz           s              1
049500050411     D filpag          s              3  0
049600050412     D nrd             s              2  0
049700050412     D nri             s              2  0
049800050412     D nrf             s              2  0
049900050412     D §nrfi           s              4  0
050000050412     D §nrfm           s              4  0
050100050412     D varfed          s              1
050200050412     D gdsp            s              2  0
050300050412     D mdsp            s              2  0
050400050412     D wtsp            s              1
050500050412     D cit3            s              2
050600050412     D rsc13           s             13
050700050412     D rmn9            s              9  0
050800050412     D wcap            s              5
050900050415     D wwwsoc          s              3
051000050415     D $kcc            s              6
051100050415     D $ksc            s              8
051200050413     D fatsc1          s              3  1
051300050413     D fatvia          s             30
051400050413     D fatcit          s             30
051500050413     D fatcap          s              5  0
051600050413     D fatprv          s              2
051700050418     D fatbna          s             36
051800050413     D fatsbl          s              1
051900060908     d Tfras           s             10  3
052000060908     d Tasss           s             10  3
052100060908     d Timps           s             11  3
052200060908     d Tfrast          s             11  3
052300060908     d Tassst          s             11  3
052400060908     d Timpst          s             12  3
052500050421     d Tfratw          s             11  3
052600050421     d Tasstw          s             11  3
052700050421     d Timptw          s             12  3
052800050413     D fatcae          s                   like(indcap)
052900050413     D savlnp          s                   like(taslnp)
053000050413     D savlna          s                   like(taslna)
053100050413     D lnpfl1          s                   like(orgfl1)
053200050413     D lnafl1          s                   like(orgfl1)
053300050413     D §trc            s                   like(taatrc)
053400050413     D fatfft          s                   like(aclfft)
053500050413     D fattft          s                   like(acltft)
053600050413     D fatip1          s                   like(clngginpa1)
053700050413     D fatip2          s                   like(clngginpa2)
053800050413     D fatnpr          s                   like(dicnrcli)
053900050413     D kcc             s                   like(key)
054000050413     D spwfls          s                   like(spefls)
054100050413     D savfil          s                   like(kscfil)
054200060228     D savcar          s                   like(orgcar)
054300060228     D savage          s                   like(aclage)
054400070118     D savrig          s                   like(wfrrig)
054500070220     D savvar          s              2  0
054600070220
054700070219     D
054800070119     d stp_pcml        s                   like(wfrspc)
054900060601     D******Wcev            s                   like(evbcev) inz('RIC')
055000051110     D comtpe          s                   like(sp2tpe) inz('EM')
055100051110     d stmail1         s             45
055200051110     d stmail2         s             45
055300060324     d mittmail        s             45
055400051128     d stpag           s              5  0                                      pagine fattura
055500051128     d okmail          s              2                                         si/no invio mail
055600051128     d sinoet          s              2                                         si/no etichetta
055700070131     d contarig        s              1                                         S=write wtrig
055800000107      * Prototipazione delle procedure
055900000107     D/COPY *LIBL/SRCCPY,PJX002PR
056000941105     C*********--------********-------*******-------********
056100960905     C* INDICATORI
056200960905     C*********--------********-------*******-------********
056300991223      * 01    - INDICA SE CLIENTE ITALIA O ESTERO
056400991223      * 05/06 - COMODO COME CHAIN
056500991223      * 07    - ERRORE/COMODO
056600991223      * 08    - ERRORE
056700991223      * 09    - TARIFFA A QUANTITA O VALORE
056800991223      * 10    - ON CLIENTE CODIFICATO
056900021104      * 11    - stampa dicitura: bollo assolto virtualamente...
057000050415      * 12    - L I B E R O
057100030618      * 13    - Comodo
057200000104      * 14    - CHAIN SU TITAS000
057300000110      * 15    - STAMPA CAP ESTERO
057400000105      * 16    -
057500000105      * 17    - DIVISA CHE PREVEDE DECIMALI  2 per la contabilita'
057600020523      * 18    - chain
057700000322      * 19    - STAMPA PARCEL
057800991223      * 20    - FATTURAZIONE MENSILE
057900991223      * 21    - CHAIN CNACO00F
058000030512      * 29    - STAMPA bolle richiesta bolla firmata
058100991223      * 30    - COMODO COME CHAIN
058200991223      * 33    - SETLL SU CNAC0000
058300991223      * 34    - ERRORE
058400991223      * 35    -
058500991223      * 36    - RICERCA ESENZIONE IVA DA ESEGUIRE SOLO UNA VOLTA
058600991223      * 37/39 -
058700000124      * 40    - TOTALI ANCHE IN MONETA DI CONTO
058800991223      * 41    - C'E' VOLUME DA FATTURARE
058900000124      * 42    - MONETA DI CONTO  HA I DECIMALI
059000021104      * 45    - stampa dicitura per esenzione da PdC
059100991223      * 50    - DA ESEGUIRE SOLO UNA VOLTA
059200991223      * 55    - COMODO COME CHAIN
059300991223      * 76/78 - PER GLI EFFETTI
059400991223      * 79    - EFFETTI IN PORTAFOGLIO
059500991223      * 80    - RIMESSA DIRETTA
059600991223      * 87    - CLIENTE CODIFICATO MA VARIO PER ASSICURAZIONE PER C/
059700991223      * 89    - STAMPA PERCENTUALE IVA O ESENTE IVA
059800991223      * 92    - SETLL SU CNCLP00F
059900991223      * 92    - SETLL SU TNTA7000
060000020419      * 94    - ERRORE
060100050323      * 95    - ERRORE CLIENTE NON FATTURABILE
060200020419      * 96    -
060300020419      * 97    - ERRORE
060400050323      * 99    - ERRORE DATA FINE RAPPORTO CLIENTE VALORIZZATA NEL FILE
060500960905     C*********--------********-------*******-------********
060600050323      *
060700050323      * Se richiesto a video un solo cliente leggo solo quello
060800050323     c                   If        vidksc = *zeros
060900050323     c     *loval        setll     Titas02l
061000050323     c                   else
061100050323     c     vidksc        setll     titas02l
061200050323     c                   endif
061300050323      **********************************************************************************************
061400050323      *            C I C L O       L E T T U R A
061500050323      **********************************************************************************************
061600991223      *
061700050325Do  1c                   Do        *hival
061800050324     c                   setoff                                       949599
061900050324     c                   setoff                                       0497
062000050323      **
062100050325    2c                   If        Vidksc = 0
062200050323     c                   read      Titas02l
062300050323     c                   else
062400050323     c     Vidksc        reade     Titas02l
062500050325    2c                   endif
062600050323      *
062700050408      * se fine file esco dal ciclo di lettura dopo aver fatto i totali
062800050323      *
062900050325    2c                   if        %eof(Titas02l)
063000070119      * scrivo il file WFRIG per l'ultimo cliente letto
063100070119     c                   exsr      Wtr_totrig
063200070119
063300050323     c                   leave
063400050325    2c                   endif
063500050323
063600050323     c                   movel(p)  tasaas        tasdsp
063700050323     c                   move      tasmgs        tasdsp
063800050323     c                   movel(p)  tastbl        tipbol
063900050323     c                   movel     tasksc        kscfil
064000050323     c                   move      tasksc        ksccod
064100050323      *
064200050323      ** Esclusione delle bolle assegnate
064300050325    2c                   if        ksccod = 9999
064400050421     c     tasksc        setgt     titas02l
064500050419     c                   iter
064600050325    2c                   endif
064700050323      ** Esclusione delle bolle con data spedizione maggiore di quella da filtro
064800050325    2c                   if        tasdsp >   dtdsp
064900991223     c                   iter
065000050325    2c                   endif
065100050323      ** Controllo cliente in base alla richiesta a video di fatturare i codificati o non
065200050323      ** Non accetto 8888 se è stato indicato fatturazione codificati
065300050323      ** Accetto 8888 se è stato indicato fatturazione dei non codificati
065400050325    2c                   if        (vidtcl = 1 and ksccod <> 8888) or
065500991223     c                             (vidtcl = 0 and ksccod = 8888)
065600050419     c                   iter
065700050325    2c                   endif
065800050323      *
065900050323      ** Filiale non compresa nelle scelte da filtro
066000050325    2c                   if        Vidtsc = 'I'
066100050323     c     Kscfil        lookup    Fil                                    02
066200050325    3c                   if        not %found
066300050324      * mi posiziono alla filiale successiva
066400050324     c                   move      9999          Tasksc
066500050324     c     tasksc        setgt     titas02l
066600050323     c                   iter
066700050325    3c                   endif
066800050325    2c                   endif
066900050324      ** Filiale compresa nelle esclusioni da filtro
067000050325    2c                   if        vidtsc = 'E'
067100050324     C     Kscfil        lookup    Fil                                    02
067200050325    3c                   if        %found
067300050324      * mi posiziono alla filiale successiva
067400050324     c                   move      9999          Tasksc
067500050324     c     tasksc        setgt     titas02l
067600050324     c                   iter
067700050325    3c                   endif
067800050325    2c                   endif
067900050324      ** Esclusione delle bolle già contabilizzate
068000050325    2c                   if        tasfic <> *blanks
068100991223     c                   iter
068200050325    2c                   endif
068300050324
068400050324      ** Esclusione delle bolle con tasxco diverso da blank ed uguale alla tabella
068500050324      ** VUS e imponibile a zero
068600050325    2c                   If        Tasimv = 0 and Tasxco <> ' '
068700050218      * Cerco nella schiera degli esclusi
068800050324     c     Tasxco        Lookup    vus                                    02
068900050325    3c                   If        %found
069000050324     c                   iter
069100050325    3c                   endif
069200050325    2c                   endif
069300050324
069400050324      ** Escludo tutte le bolle con N. di fattura di filiale
069500050325    2c                   IF        Tasnft > 0  and tasfiv <> §qcfiv and
069600050324     c                             tasfiv <> §qcfi2
069700050324     c                   iter
069800050325    2c                   endif
069900050324
070000050324      ** Contabilizzo le bolle con tipo bolla valido altrimenti escludo
070100050324     c                   z-add     1             a
070200050324     c     Tastbl        Lookup    TB(a)                                  05
070300050325    2c                   if        not %found
070400050324     c                   iter
070500050324     c                   else
070600050324      * verifico se da contabilizzare
070700050324     c     A             occur     DStb
070800050325    3c                   if        §tbfcb <> '1'
070900050324     c                   exsr      conbol
071000050324     c                   iter
071100050325    3c                   endif
071200050325    2c                   endif
071300050324
071400050324      ** Aggancio organigramma con il punto operativo del cliente per stampa manca tariffa
071500050324
071600050325    2c                   IF        kscfil <> savfil
071700050324     c     Kscfil        chain     Azorg01l
071800050325    3c                   if        %found(azorg01l)
071900060228     c                   movel     orgcar        savcar
072000050324     c                   eval      savfil = kscfil
072100050325    3c                   endif
072200050325    2c                   endif
072300050324
072400050324      ** verifico se c'è rottura di cliente
072500050324
072600050325    2c                   If        tasksc <> kscpre
072700070118
072800070118      * se non è il primo cliente scrivo il record di totale del file di lavoro per contare le
072900070118      * righe
073000070118     c                   if        kscpre <> *zeros
073100070118     c                   exsr      Wtr_totrig
073200070118     c                   endif
073300070119      * pulisco sempre i campi di comodo:
073400070119      *         il numero riga per la prossima rottura
073500070119      *         il numero di varie per la prossima rottura
073600070119      *         flag stampa varia D
073700070119      *         flag stampa frase relativo al peso cml
073800070119
073900070119     c                   clear                   savrig
074000070220     c                   clear                   SVR
074100070119     c                   clear                   stp_vard
074200070119     c                   clear                   stp_pcml
074300070119
074400070118
074500050324      * aggancio la tabella FAT
074600050325    3c                   if        §fatprox = ' ' or §fatprox = 'S'
074700050408     c                   exsr      SR_tabfat
074800050324      * se il flag blocco di fatturazione è = "S" vado a fine
074900050324      *
075000050325    4c                   if        §fatbloc = 'S'
075100050408     c                   leave
075200050325    4c                   endif
075300050325    3c                   endif
075400050325      * se è un 8888 da fatturare stampo sempre manca tariffa
075500050325    3c                   If        Ksccod = 8888
075600050325     c                   setoff                                       10        * non codificato
075700050325      *
075800050325     c                   clear                   Tasspc
075900050325     c                   exsr      Errore
076000050325     c                   iter
076100050325    3c                   endif
076200050325      ** Controllo codice di bollettazione con aggancio ad anagrafiche
076300050325     c                   seton                                        10        * codificato
076400050325     c                   move      *zeros        ktasksc
076500050325     c                   move      *blank        savtasksc
076600050325     c                   clear                   bolksc
076700050330     c                   clear                   bolra1
076800050330     c                   clear                   bolvia
076900050330     c                   clear                   bolcit
077000050330     c                   clear                   bolcap
077100050325     c                   clear                   savragbol
077200050325     c                   clear                   fatksc
077300050325     c                   move      tasksc        ktasksc
077400050325     c     Keycli        chain     ancln01l                           99
077500050325      * se data fine rapporto cliente > 0 errore
077600050325    3c                   if        clndtfirpc > *loval
077700050325     c                   seton                                        99
077800050325    3c                   endif
077900050325      * se non c'è errore
078000050325    3c                   If        not *in99
078100050325     c     clnsogg       chain     ansog01l                           99
078200050325     c     Keyind        chain     anind01l                           99
078300050506     c     Keybac        chain(n)  ybacl01l                           99
078400050325    3c                   endif
078500050325      * errore manca anagrafica
078600050325    3c                   if        *in99
078700050325     c                   clear                   Tasspc
078800050325     c                   exsr      errore
078900050325     c                   setoff                                       99
079000050325     c                   iter
079100050325    3c                   endif
079200050325      * cliente non fatturabile
079300050325    3c                   if        acltft = 9
079400050325     c                   seton                                        95        *indicatore errore
079500050325     c                   clear                   Tasspc
079600050325     c                   exsr      errore
079700050325     c                   setoff                                       95
079800050325     c                   iter
079900050325    3c                   endif
080000050325      ** Escludo cliente con tipo fatturazione maggiore del richiesto
080100050325    3c                   if        aclfft > vidtft
080200050325      * mi posiziono al cliente successivo
080300050325     c     tasksc        setgt     titas02l
080400050325     c                   iter
080500050325    3c                   endif
080600050325      * mi salvo il codice di bollettazione
080700050325     c                   move      tasksc        bolksc
080800050330      * valorizzo i campi per il file di lavoro assicurazione per conto
080900050404     c                   exsr      membol
081000050422     c                   movel     aclflo        savragbol
081100050325      * verifico se esiste codice di fatturazione diverso da cod. bollettazione
081200050325    3c                   if        clnclifatt > *zeros and
081300050325     c                             clnclifatt <> clnksc
081400070119      * sommo 2 righe in caso di bolla non raggruppata per stampa dati anagrafici codice di bolletta
081500070119      * zione
081600070119     c                   if        savragbol = ' '
081700070119     c                   add       2             savrig
081800070119     c                   endif
081900050422      * salvo il codice di bollettazione
082000050325     c                   move      clnclifatt    fatksc
082100050325      *
082200050325     c                   move      ktasksc       savtasksc
082300050325     c                   move      clnclifatt    ktasksc
082400050325     c     Keyfat        CHAIN     ancln01l                           99
082500050325      * se data fine rapporto cliente > 0 errore
082600050325    4c                   if        clndtfirpc > *loval
082700050325     c                   seton                                        99
082800050325    4c                   endif
082900050325      * se non c'è errore
083000050325    4c                   If        not *in99
083100050325     c     Clnsogg       chain     ansog01l                           99
083200050325     c     Keyind        chain     anind01l                           99
083300050506     c     Keybac        chain(n)  ybacl01l                           99
083400050325    4c                   endif
083500050325      * errore manca anagrafica
083600050325    4c                   if        *in99
083700050325     c                   clear                   Tasspc
083800050325     c                   exsr      errore
083900050325     c                   setoff                                       99
084000050325     c                   iter
084100050325    4c                   endif
084200050325    3c                   endif
084300050325      * salvo il codice cliente
084400050325     c                   z-add     tasksc        kscpre
084500070117      *_____________________________________________________________*
084600070117      * verifico alcuni dati necessari per il contarighe in fattura *
084700070117      *_____________________________________________________________*
084800070131
084900070131      * Pulisco il flag che mi segnala se devo scrivere oppure no il conta righe
085000070131     c                   clear                   contarig
085100070117
085200070110      * verifico se il cliente desidera stampare sempre il mittente originale
085300070110      * se c'è raggruppamento prendo il codice di raggruppamento altrimenti il codice bollettazione
085400070117
085500070110     c                   clear                   dsbs69
085600070110     c                   movel     knsif         i69sif
085700070110     c                   movel     kci           i69kcc
085800070110     c                   if        savragbol = 'S' and fatksc > 0
085900070110     c                   movel     fatksc        i69kcs
086000070110     c                   else
086100070110     c                   movel     bolksc        i69kcs
086200070110     c                   endif
086300070110     c                   call      'TIBS69R'
086400070110     c                   parm                    dsbs69
086500070110     c                   parm                    dsaco
086600070110     c                   parm                    dsind
086700070110     c                   parm                    dsclp
086800070110     c                   parm                    dscls
086900070110    3c                   if        o69err <> ' '
087000070110     c                   clear                   dsCLS
087100070110    3c                   endif
087200070110     c                   clear                   stprmo
087300070110    3c                   if        %subst(cclsflo:6:1) <> *blanks
087400070110     c                   eval      stprmo = %subst(cclsflo:6:1)
087500070110    3c                   endif
087600070117
087700070117      * verifico se cliente DPD dal codice di raggruppamento per la stampa Parcel
087800070117
087900070111     c                   clear                   stppar
088000070111     c                   movel     i69kcs        comfil
088100070111     c     comfil        chain     azorg01l
088200070111     c                   if        %found(azorg01l)
088300070111     c                   movel     orgde3        og143
088400070111     c                   if        §ogdpd = 'S'
088500070111     c                   eval      stppar = 'S'
088600070111     c                   endif
088700070111     c                   endif
088800070110     c
088900070118
089000070118      * verifico se devo prevedere il diritto di fatturazione
089100070118
089200070118     c                   move      aclSIN        DIRfatt                        diritto di fatturazi
089300070118
089400070118      * se diritto di fatturazione è uguale a zero imposto flag di stampa  uguale "N"
089500070118
089600070118     c                   If        DIRfatt = 0
089700070118     c                   eval      Stp_vard = 'N'
089800070118     c                   endif
089900070118      *
090000050325    2c                   endif
090100050329      *
090200050325      *---------------------------------------------------------------------------------------------
090300050325      *    T A S S O    L A    S I N G O L A    B O L L A
090400050325      *---------------------------------------------------------------------------------------------
090500050325
090600050325      * carico eventuali varie presenti in TITA7
090700050325     c                   Exsr      Carta7
090800050325      * valorizzo il file di comodo  con immagine titas prima della tassazione
090900050325     c                   Exsr      Wrktas
091000050329      * preparazione di alcuni campi
091100050325     c                   z-add     tasimv        preimv
091200050329     c                   clear                   tascas
091300050329     c                   clear                   tastic
091400050329     c                   clear                   tasvca
091500050329     c                   clear                   tascmb
091600050329     c                   clear                   tassva
091700050329     c                   clear                   parca
091800030204
091900050329      *
092000050329      * Spedizione non ancora Tassata
092100050329      *
092200050329
092300050329    1c                   IF        tasimv = 0
092400050329      ** Ricerca contrassegno
092500050329    2c                   IF        Tasfca <> *blanks
092600050329     c                   Exsr      Carcsb
092700050329    2c                   Endif
092800050329
092900050329      *  se si tratta di codice bolla che prevede solo una varia, passo solo
093000050329      *  quella
093100050329     c                   z-add     1             kk
093200050329     c     tascbo        lookup    cbo(kk)                                05
093300050329     c                   if        *in05
093400050329     c                   eval      tassva=cBV(kk)
093500050329     c                   endif
093600050329
093700050329      * se spedizione già tassata richiamo per la varia "R"
093800050329   X1c                   else
093900050329
094000050329     c                   movel     'R'           Tassva
094100050329     c                   movel     'S'           Parca
094200050329    1c                   endif
094300050329      * pulisco i campi relativi alla variad
094400050329
094500050329     c                   clear                   tasivdf
094600050329     c                   clear                   tassvdf
094700050329      *
094800050329      * Richiamo la tassazione anche se c'è il totale imponibile per controllare
094900050329      * l'assicurazione
095000050329      *
095100050329     c                   Exsr      Tassa
095200050329      * se c'e' errore leggo il successivo
095300050329     c                   if        taserr <> *blank
095400050329      * manca tariffa
095500050329     c                   seton                                        94
095600050329     c                   exsr      errore
095700050329     c                   iter
095800050329     c                   endif
095900050329      *
096000050329     c                   z-add     tasimv        wimv
096100050329      * se tassata per una varia e importo=0, la scrivo ma passo oltre
096200050329     c                   if        tassva<>' ' and tasimv=0
096300050329      * controllo che sia possibile per questo codice bolla
096400050329     c     tascbo        lookup    im0                                    05
096500050329     c                   If        *in05
096600050329     c                   exsr      conbol
096700050329     c                   iter
096800050329      *
096900050329     c                   else
097000050329     c                   seton                                        97
097100050329     c                   Exsr      Errore
097200050329     c                   Unlock    Titas02l
097300050329     c                   Iter
097400050329     c                   endif
097500050329     c                   endif
097600050329      *
097700050329      * se supera l'importo contenuto in §geimf --> Errore
097800050329      *
097900050329     c                   if        tasimv >= §geimf
098000050329     c                   seton                                        04
098100050329     c                   Exsr      Errore
098200050329     c                   Unlock    Titas02l
098300050329     c                   iter
098400050329     c                   endif
098500050330      *  aggiorno la bolla e scrivo il file di lavoro
098600050329     c                   exsr      Aggwrf
098700070131     c                   eval      contarig = 'S'
098800050428      * aggiorno anche i flag di contabilizzato e tassato per evitare la manutenzione e
098900050428      * l'aggiornamento da parte di ricezioni di variazioni dalle filiali
099000050428     c                   movel     '1'           tasfts
099100050428     c                   movel     '1'           tasfic
099200050504     c   20              z-add     dtdfi8        tasdft
099300050504     c  N20              z-add     dtdft8        tasdft
099400050428
099500050429      *
099600050429     c                   update    titas000
099700050429      *
099800050330     c                   exsr      Aggbol
099900050329      *
100000050329     c                   Enddo                                                  fine ciclo lettura
100100050324      * se devo mandare a termine la fatturazione in base alla tabella vado a fine
100200050324     c                   if        §fatbloc = 'S'
100300050324     c                   goto      finefat
100400050324     c                   endif
100500050324     c
100600050331      **********************************************************************************************
100700050331      *         F I N E      C I C L O       L E T T U R A
100800050331      **********************************************************************************************
100900050331      *
101000050404      * elaborazione del file di lavoro per raggruppamento
101100050404      *
101200050404     c                   feod      wfrtas1l
101300070119     c                   feod      wfrig00f
101400050404      *
101500050404      **********************************************************************************************
101600050404      *         S T A M P A     E     C O N T A B I L I Z Z A Z I O N E     F A T T U R E
101700050404      **********************************************************************************************
101800050404
101900050404     c     *loval        setll     wfrtas1l
102000050404
102100050411    1c                   do        *hival
102200050404     c                   read      wfrtas1l
102300050404
102400050404      * fine file elaboro l'ultimo cliente
102500050404
102600050411    2c                   If        %eof(wfrtas1l)
102700050415     c                   exsr      Sr_finele
102800050404     c                   leave
102900050411    2c                   endif
103000050404
103100050408      * verifico se c'è rottura di codice cliente di raggruppamento oppure se si è
103200050404      * superato 990 pagine
103300050404
103400050411    2c                   If        (wf_tasrsc<>tasrscpre and tasrscpre<>*zeros)
103500050620     c                             or  page >= 990 or
103600050620      * verifico se esiste un raggruppamento e se c'è rottura e pagine maggiori = a 800
103700050803     c                             (tasfrgpre = 'S' and tasbscpre <> wf_tasbsc
103800050804     c                             and page >= 800) or
103900050803      * verifico se per codice raggruppamento uguale flag raggruppamento diverso
104000050803     c                             (wf_tasrsc=tasrscpre and
104100050803     c                              wf_tasfrg<>tasfrgpre)
104200050620
104300050404     c                   exsr      totcod
104400050423
104500050423
104600050408      * aggancio la tabella FAT
104700050408    3c                   if        §fatprox = ' ' or §fatprox = 'S'
104800050408     c                   exsr      SR_tabfat
104900050408      * se il flag blocco di fatturazione è = "S" vado a fine
105000050408      *
105100050408    4c                   if        §fatbloc = 'S'
105200050408     c                   exsr      Sr_finele
105300050408     c                   leave
105400050408    4c                   endif
105500050408    3c                   endif
105600050411    2c                   endif
105700050411      * clienti diversi
105800050411    2c                   If        wf_tasrsc <> tasrscpre
105900050420      * salvo codice raggruppamento e codice bollettazione e flag raggruppamento
106000050411     c                   eval      tasrscpre = wf_tasrsc
106100050422     c                   eval      tasfrgpre = wf_tasfrg
106200070119      * verifico numero di righe e calcolo il totale
106300070119     c                   exsr      Ctr_totrig
106400050411
106500050408      * preparo la chiave
106600050408     c                   clear                   savtasksc
106700050415     c                   move      wf_tasrsc     ktasksc
106800050408      * aggancio anagrafiche
106900050408     c     Keycli        chain     ancln01l                           99
107000050408     c     Clnsogg       chain     ansog01l                           99
107100050408     c     Keyind        chain     anind01l                           99
107200050506     c     Keybac        chain(n)  ybacl01l                           99
107300050408
107400050408     c                   z-add     wf_tasrsc     bolksc
107500050408     c                   movel     bolksc        bolfil
107600050408      * recupero AS del codice di bollettazione
107700060707      * chiamo Fnlv55 con tipo terminal "O" = Ottico
107800050408     c                   clear                   dslv55
107900060707     c                   movel     'O'           d55tpt
108000060707     c****               movel     'P'           d55tpt
108100050408     c                   movel     bolfil        d55lin
108200050408     c                   movel     dateu         d55drf
108300050408     c                   call      'FNLV55R'
108400050408     c                   parm                    dslv55
108500050408
108600050408     c                   movel     d55tfp        asbol
108700050510      * verifico se cliente DPD dal codice di raggruppamento
108800050510     c                   clear                   stppar
108900050510     c     bolfil        chain     azorg01l
109000050510     c                   if        %found(azorg01l)
109100050510     c                   movel     orgde3        og143
109200050510     c                   if        §ogdpd = 'S'
109300050510     c                   eval      stppar = 'S'
109400050510     c                   endif
109500050510     c                   endif
109600050408      * se devo stampare il riferimento mittente originale lo prendo dal codice raggruppamento
109700050408      * anche se il codice è diverso dal codice di bollettazione (parlo del codice)
109800050408     c                   clear                   dsbs69
109900050408     c                   movel     knsif         i69sif
110000050408     c                   movel     kci           i69kcc
110100050415     c                   z-add     bolksc        i69kcs
110200050408     c                   call      'TIBS69R'
110300050408     c                   parm                    dsbs69
110400050408     c                   parm                    dsaco
110500050408     c                   parm                    dsind
110600050408     c                   parm                    dsclp
110700050408     c                   parm                    dscls
110800050411    3c                   if        o69err <> ' '
110900050408     c                   clear                   dsCLS
111000050411    3c                   endif
111100050408     c                   clear                   stprmo
111200050411    3c                   if        %subst(cclsflo:6:1) <> *blanks
111300050408     c                   eval      stprmo = %subst(cclsflo:6:1)
111400050411    3c                   endif
111500050408      *
111600050408      * SALVO il diritto fatturazione del cliente di raggruppamento
111700050408     c                   exsr      memdirfat
111800050408      **
111900050411    3c                   if        clnclifatt > *zeros and
112000050408     c                             clnclifatt <> clnksc
112100050408     c                   move      ktasksc       savtasksc         8
112200050408      * SALVO le decodifiche del cliente di raggruppamento/bollettazione
112300050408     c                   exsr      membol
112400050408      * RECUPERO anagarfica di fatturazione
112500050408     c                   move      clnclifatt    ktasksc
112600050408     c     Keyfat        chain     ancln01l                           99
112700050408     c     clnsogg       CHAIN     ansog01l                           99
112800050408     c     Keyind        CHAIN     anind01l                           99
112900050506     c     Keybac        CHAIN(n)  ybacl01l                           99
113000050408
113100050411    3c                   endif
113200050408      * imposto intestazione fattura
113300050408     c                   EXSR      MEMANA
113400050408
113500050411    2c                   endif
113600050420      * verifico se esiste un raggruppamento e se c'è rottura
113700050427     c                   if        wf_tasfrg = 'S' and tasbscpre <> wf_tasbsc
113800050420      * se rottura
113900050420     c                   if        tasbscpre <> *zeros
114000050421     c                   exsr      SR_Totbol
114100050420     c                   endif
114200050420      * ricerco decodifica
114300050420     c                   Exsr      Membsc
114400050421
114500050421     c                   eval      tasbscpre = wf_tasbsc
114600050421
114700050420     c                   endif
114800050420      *
114900050411      * Aggancio TITAS
115000050429     c     Ktas30        chain     titas30c
115100050411      * se non lo trovo leggo il successivo (COMUNQUE E'''' IMPOSSIBBBBBBILE)
115200050429    2c                   If        not %found(titas30c)
115300050411     c                   iter
115400050411    2c                   endif
115500050419      ** mi salvo alcuni campi
115600050419     c                   clear                   tasdsp
115700050419     c                   movel     tasaas        tasdsp
115800050419     c                   move      tasmgs        tasdsp
115900050419     c                   movel     tastbl        tipbol
116000050419     c                   move      tasccm        wccm
116100050419     c     wccm          ifeq      8888
116200050419     c                   clear                   titaa000
116300050419     c                   movel     'M'           §trc
116400050419     c     ktaa          chain     titaa000                           05
116500050419     c                   end
116600050412      * mi salvo il totale imponibile
116700050412     c                   z-add     tasimv        preimv
116800050411      * carico le varie
116900050411     c                   exsr      carta7
117000050411      * verifico se devo calcolare la varia "D"
117100050411      * se non era tassata fino l'imponibile e non ha la varia 'D' e la devo ancora calcolare
117200050420    2c                   If        cal_vard = ' '
117300050411      * verifico se già calcolata in quella bolla
117400060214     c                   z-add     1             xx
117500060214     c     'D'           lookup    sv(xx)                                 13
117600050411    3c                   If        %found
117700050411     c                   eval      cal_vard = 'N'
117800050411     c                   eval      stp_vard = 'N'
117900060214
118000060214      * mi salvo il diritto di fatturazione da mettere in TIIFT
118100060214     c                   z-add     va(xx)        fatdir
118200060214
118300050411     c                   else
118400050411      * se non trovata la devo calcolare
118500050411     c                   eval      tassva  = 'D'
118600050415     c                   z-add     dirfatt       tasivdf
118700050411     c                   eval      tassvdf = 'D'
118800050418      *
118900050411     c                   call      'TNSF20R'
119000050418     C                   parm                    Kpjba
119100050411     c                   parm                    dtas
119200050411     c                   parm                    dtasv
119300050411      *
119400050411     c                   if        taserr = ' '
119500050411     c                   eval      cal_vard = 'N'
119600060214
119700060214      * mi salvo il diritto di fatturazione da mettere in TIIFT
119800060214     c                   z-add     dirfatt       fatdir
119900060214
120000050411     c                   endif
120100050411      *
120200050411    3c                   endif
120300050411
120400050411    2c                   endif
120500050411
120600050411     c                   Exsr      stacod
120700050415
120800050415     c                   enddo
120900050415
121000050415
121100000000     C     FINE          TAG
121200891025     C** AGGIORNAMENTO FILE N.FATTURA
121300970523     C     *INH5         IFEQ      *OFF
121400970523     C     *INH7         ANDEQ     *OFF
121500961029     C                   EXSR      AGGNUM
121600961104     C**
121700961104     C* STAMPO IL PRIMO E L'ULTIMO NUMERO FATTURA INIZIO MESE E FINE ME
121800961104     C     WINIZ         IFEQ      '1'
121900961104     C                   Z-ADD     NFTIM         WINULT            6 0
122000961104     C                   ENDIF
122100961104     C     WFINE         IFEQ      '1'
122200961104     C                   Z-ADD     NFTFM         WFIULT            6 0
122300961104     C                   ENDIF
122400961104     C                   EXCEPT    NUMFAT
122500970523     C                   ENDIF
122600000107     c**
122700000107     c* STAMPO I TOTALI GENERALI
122800000107     C                   IF        TOTGEN > 0
122900000124     c* se sono valorizzati anche le conversioni in mineta di conto le stampo
123000000124     c                   eval      *in40  = (totgencnt > 0)
123100050415      * imposto che la moneta di conto prevede i decimali
123200050415     C                   eval      *in42 = *on
123300000124     c*
123400000107     C                   EXCEPT    GEN
123500000107     C                   ENDIF
123600000107     C***
123700000107     C*
123800050415     C                   CLEAR                   dslv55
123900980105     C                   MOVEL     'C'           D55TLA
124000980105     C                   CALL      'FNLV55R'
124100050415     C                   PARM                    dslv55
124200990715     C*
124300990715     C                   CLEAR                   DSBS69
124400990715     C                   MOVEL     'C'           I69TLA
124500990715     C                   CALL      'TIBS69R'
124600990715     C                   PARM                    DSBS69
124700990715     C                   PARM                    DSACO
124800990715     C                   PARM                    DSIND
124900990715     C                   PARM                    DSCLP
125000990715     C                   PARM                    DSCLS
125100000103     C*
125200000103     C                   CLEAR                   DSBS02
125300000111     C                   MOVEL     'C'           T02TLA
125400000103     C                   CALL      'TIBS02R'
125500000103     C                   PARM                    KPJBA
125600000103     C                   PARM                    DSBS02
125700050413      *
125800050413      ** chiudo i files di stampa
125900050413     c                   If        %open(ftwcodfm)
126000050413     c                   close     ftwcodfm
126100050413     c                   endif
126200050413      *
126300050413     c                   If        %open(ftwcodim)
126400050413     c                   close     ftwcodim
126500050413     c                   endif
126600050413      *
126700050413     c                   If        %open(ftwcodds)
126800050413     c                   close     ftwcodds
126900050413     c                   endif
127000050413      *
127100050413     c                   If        %open(disspe)
127200050413     c                   close     disspe
127300050413     c                   endif
127400050421      *
127500050421     c                   If        %open(prtf198)
127600050421     c                   close     prtf198
127700050421     c                   endif
127800050421      *
127900050421     c                   If        %open(qsysprt)
128000050421     c                   close     qsysprt
128100050421     c                   endif
128200050421      *
128300050421     c                   If        %open(sysprt)
128400050421     c                   close     sysprt
128500050421     c                   endif
128600050413      *
128700050421     c                   exsr      primanota
128800060228      *
128900060228
129000060228      * se fatturazione mensile stampo manca tariffa
129100060314     c   20              exsr      mancatar
129200050324      * fine fatturazione
129300050324     c     finefat       tag
129400000000     C                   SETON                                        LR
129500991223
129600050415      ****************************************************
129700050415      ** Fine elaborazione stampa fatture
129800050415      ****************************************************
129900050415     c     SR_finele     Begsr
130000050418      *
130100050418     c                   exsr      totcod
130200050415
130300050415      * totale distinte di spedizione da fare a fine stampa
130400050415     c                   if        tdsp > 0 or tdspd > 0
130500050415     c                   except    statds
130600050415     c                   endif
130700050415      *
130800050415     c                   endsr
130900050415
131000050415      ****************************************************
131100050415      ** Aggancio tabella FAT
131200050415      ****************************************************
131300050415     c     SR_tabfat     Begsr
131400050415
131500050415     c                   clear                   dsbs02
131600050415     c                   movel     'L'           T02tla
131700050415     c                   movel     'C'           T02mod
131800050415     c                   movel     knsif         t02sif
131900050415     c                   movel     'FAT'         t02cod
132000050415     c                   movel     'FAT'         t02ke1
132100050415     c                   call      'TIBS02R'
132200050415     c                   parm                    KPJBA
132300050415     c                   parm                    DSBS02
132400050415     c                   movel(P)  t02uni        DFAT
132500050415
132600050415     c                   Endsr
132700050325      ****************************************************
132800050325      ** Scrittura file di lavoro wfrtas0f
132900050325      ****************************************************
133000050325     c     Wrktas        Begsr
133100050325
133200050325     c                   clear                   wfrtas00
133300050325      * se c'è raggruppamento + codici di bollettazione in uno di fatturazione
133400050329      * imposto il codice raggruppamento con il codice di fatturazione
133500050427     c                   if        savragbol = 'S' and fatksc > 0
133600050329     c                   movel     fatksc        wf_tasrsc
133700050329     c                   else
133800050329     c                   movel     bolksc        wf_tasrsc
133900050329     c                   endif
134000050420      *
134100050420     c                   eval      wf_tasfrg = savragbol
134200050329      *
134300050329     c                   eval      wf_tasbsc = tasksc
134400050329     c                   eval      wf_tastbl = tastbl
134500050329     c                   eval      wf_tasaas = tasaas
134600050329     c                   eval      wf_tasmgs = tasmgs
134700050329     c                   eval      wf_taslnp = taslnp
134800050329     c                   eval      wf_tasnrs = tasnrs
134900050329     c                   eval      wf_tasnsp = tasnsp
135000050329     c                   eval      wf_taspvl = taspvl
135100050329      * tassazione
135200050329      * porto
135300050329     c                   eval      wf_tasdiv = tasdiv
135400050329     c                   eval      wf_taspor = taspor
135500050329      * varie
135600050329     c                   eval      Dtasvwf   = Dtasv
135700050329      * imponibile
135800050329     c                   eval      wf_tasimv = tasimv
135900050329      * il record viene scritto solo se bolla valida per la fatturazione
136000050329     c                   endsr
136100050329      *************************************************
136200050329      **  Aggiorna archivio di lavoro WFRTAS
136300050329      *************************************************
136400050330     c     Aggwrf        Begsr
136500050330
136600050330      * valorizzo campi nel file di lavoro utili per la stampa
136700050330      * per aggiornare file assicurazione o per una eventuale ripartenza
136800050330
136900050330     c                   eval      wf_taspkcn = taspkcn
137000050330     c                   eval      wf_tasspc  = tasspc
137100070130     c                   If        tasspc = 'S'
137200070119     c                   eval      Stp_pcml   = tasspc
137300070130     c                   endif
137400050330     c                   eval      wf_tasncl  = stpncl
137500070110      * verifico assicurazione per conto
137600050330
137700050330     c                   z-add     1             a
137800050330     c     tastbl        lookup    tb(a)                                  05
137900050330     c   05a             occur     dstb
138000050330     c                   if        *in05 and §tbfas <> '0'
138100070110      ** Se l'importo d'assicurare non c'era lo imposto e imposto anche il flag di messo in fatturaz
138200050330     c                   if        Tasias = 0
138300050330     c                   z-add     tasial        tasias
138400050330      *
138500050330     c                   if        tasial > 0
138600050330     c                   movel     'F'           Tasfcm
138700050330     c                   eval      wf_tasfcm = 'F'
138800050330      * aggiunta valorizzazione della divisa dell'assicurazione con la stessa moneta del cliente
138900050330     c                   move      tasdiv        tasvas
139000050330     c                   endif
139100050330     c                   endif
139200050330      **
139300050330     c                   endif
139400070110      * se devo stampare il riferimento mittente recupero da TAA
139500070110
139600070110     c                   clear                   wmitori
139700070110     c                   clear                   wparcel
139800070110
139900070110     c                   If        stprmo <> *blank or
140000070110      * oppure se bolla di recupero dei ritiri annullati
140100070110     c                             tascbo='FY'
140200070110     c                   eval      §trc = 'O'
140300070110     c     ktaa          chain     Titaa30c
140400070110     c                   if        %found(titaa30c)
140500070110     c                   movel     taarsc        wmitori
140600070110     c                   endif
140700070110      *
140800070110     c                   endif
140900070110
141000070110      * Se bolla di recupero ritiri annullati, stampo anche il rif.alfabetic
141100070110     c                   If        tascbo='FY'
141200070110     c                   eval      §4trc = 'A'
141300070110     c     Kta4          chain     tita430c
141400070110     c                   If        %found(tita430c)
141500070110     c                   movel     ta4not        dta4a
141600070110     c                   else
141700070110     c                   clear                   dta4a
141800070110     c                   endif
141900070110     c                   movel     §ta4arma      Wmitalf
142000070111     c                   eval      *in22 = '1'
142100070110     c                   endif
142200070110
142300070110
142400070110      * stampa parcel perchè DPD e non è distinta di spedizioni
142500070110
142600070111     c                   if        stppar <> *blank and acltft <> 1
142700070110
142800070110     c                   eval      §4trc = 'I'
142900070110     c     Kta4          chain     tita430c
143000070110     c                   If        %found(tita430c)
143100070110     c                   movel     ta4not        dsbl4i
143200070110     c                   eval      wparcel = §b4ipn
143300070110     c                   endif
143400070110     c                   endif
143500070110
143600070110      * se non è stato valorizzato nel mittente originale e neppure il parcel
143700070110      * verifico se si tratta di bolla fuori misura da addebitare a cliente DPD
143800070110      * riconosco un cliente dpd se stppar è diverso da blank (stampa numero parcel)
143900070110      * in più la bolla deve essere F3 codice bolla "B "
144000070110      * riconosco un fuori misura se nel tipo record O di titaa c'è la scritta OVERWEIGHT
144100070110     c                   If        (Wmitori = *blanks and wparcel = *blanks)
144200070110     c                             and stppar <> ' ' and tastbl = 'F3' and
144300070110     c                             tascbo = 'B '
144400070110     c                   eval      §trc = 'O'
144500070110     c     ktaa          chain     Titaa30c
144600070110     c                   if        %found(titaa30c) and
144700070110     c                             %subst(taarsc:1:10)= 'OVERWEIGHT'
144800070110     c
144900070110     c                   eval      %subst(wmitori:15:11) = 'OVERWEIGHT '
145000070110      * recupero numero parcel
145100070110     c                   eval      §4trc = 'A'
145200070110     c     Kta4          chain     tita430c
145300070110     c                   If        %found(tita430c)
145400070110     c                   movel     ta4not        dta4a
145500070110     c                   else
145600070110     c                   clear                   dta4a
145700070110     c                   endif
145800070110     c                   movel     §ta4arpt      Wparcel
145900070110
146000070110     c                   endif
146100070110
146200070110     c                   endif
146300070219      * conto le righe
146400070122     c                   z-add     1             Wf_wfrtrig
146500070219      * verifico il numero di varie e le memorizzo
146600070130     c                   If        sv(4) = *blank and ((wmitori <> *blank) or
146700070130     c                             (wparcel <> *blank)) or sv(4) <> *blank
146800070111
146900070122     c                   add       1             Wf_wfrtrig
147000070111     c                   endif
147100070115      * aggiungo una riga per ogni gruppo di 3 varioe valorizzate
147200070115     c                   z-add     7             a
147300070130     c                   dow       sv(a) <> *blanks
147400070122     c                   add       1             Wf_wfrtrig
147500070115     c                   add       3             a
147600070115     c                   enddo
147700070220      * Salvo le sigle della spedizione
147800070220     c                   do        20            a
147900070220     c                   if        sv(a) <> ' '
148000070220      * verifico se esiste la sigla nella schiera di salvataggio
148100070220     c                   z-add     1             a0
148200070220     c     sv(a)         lookup    svr                                    30
148300070220     c                   if        not %found
148400070220     c     ' '           lookup    svr(a0)                                30
148500070220     c                   if        %found
148600070220     c                   movel     sv(a)         svr(a0)
148700070220     c                   endif
148800070220
148900070220     c                   endif
149000070220     c                   endif
149100070220     c                   enddo
149200070115      * verifico se devo stampare numero colli originali
149300070115     c                   if        wf_tasncl > 0
149400070122     c                   add       1             Wf_wfrtrig
149500070115     c                   endif
149600070115      * verifico se bolla esente
149700070115     c                   If        tasfei = *blank
149800070115     c                   z-add     1             yy
149900070115     c                   setoff                                       43
150000070115     c     tasnzd        lookup    naze(yy)                               43
150100070115     c   43              movel     esen(yy)      tasfei
150200070115     c                   endif
150300070115      * in questo modo aggiorno già il campo in TITAS
150400070115     c                   if        tasfei <> *blanks
150500070122     c                   add       1             Wf_wfrtrig
150600070115     c                   endif
150700070118
150800050330      * WFRTAS
150900050330     c                   write     wfrtas00
151000070122     c                   add       Wf_wfrtrig    savrig
151100060428      *
151200060428      * verifico se la bolla era stata già tassata in tutto o in parte prima della
151300060428      * fatturazione
151400060502     c                   movea     WF_sv         comwf_sv
151500060502      * se sigle  delle varie diverso da blank o il porto maggiore di zero scrivo il
151600060502      * file in verticale per ogni importo o sigla varia valorizzato
151700060428      * la sigla del tipo importo per le varie è uguale alla sigla delle varie per il
151800060502      * porto è " ".
151900060428
152000060502     c                   If        comwf_sv <> *blanks or wf_taspor  > 0
152100060428      * imposto spedizione
152200060428     c                   clear                   titai000
152300060428     c                   eval      taiaas = wf_tasaas
152400060502     c                   eval      tailnp = wf_taslnp
152500060428     c                   eval      tainsp = wf_tasnsp
152600060428     c                   eval      tainrs = wf_tasnrs
152700060428     c                   eval      taitbl = wf_tastbl
152800060519      * Imponibile
152900060519     c                   If        wf_tasimv > 0
153000060519     c                   eval      taisvt = '£'
153100060519      * verifico se esiste il record
153200060519     c     ktai          setll     titai01l
153300060519     c                   If        not %equal(titai01l)
153400060519     c                   z-add     wf_tasimv     taivat
153500060519     c                   write     titai000
153600060519     c                   endif
153700060519     c                   endif
153800060428      * Porto
153900060428     c                   If        wf_taspor > 0
154000060502     c                   eval      taisvt = ' '
154100060502      * verifico se esiste il record
154200060502     c     ktai          setll     titai01l
154300060502     c                   If        not %equal(titai01l)
154400060428     c                   z-add     wf_taspor     taivat
154500060428     c                   write     titai000
154600060428     c                   endif
154700060502     c                   endif
154800060428      * Varie
154900060428     c                   Do        20            vv
155000060428     c                   If        wf_sv(vv)  <> ' '
155100060502     c                   eval      taisvt = wf_sv(vv)
155200060502      * verifico se esiste il record
155300060502     c     ktai          setll     titai01l
155400060502     c                   If        not %equal(titai01l)
155500060428     c                   z-add     wf_va(vv)     taivat
155600060428     c                   write     titai000
155700060502     c                   endif
155800060428     c                   endif
155900060428     c                   enddo
156000060428
156100060428     c                   endif
156200050330      *
156300050330     c                   endsr
156400070118      ****************************************************
156500070118      ** Scrittura file di lavoro wfrig00f conta righe
156600070118      ****************************************************
156700070118     c     Wtr_totrig    Begsr
156800070118
156900070131      * solo se ho tassato almeno una bolla per il cliente
157000070131     c                   If        contarig = 'S'
157100070118      * calcolo le righe da aggiungere per ogni cambio codice bollettazione
157200070118      * se c'è raggruppamento
157300070118     c                   If        WF_tasfrg = 'S'
157400070118      * 3 righe dati anagrafici
157500070119      * 3 righe per i totali
157600070119     c                   add       6             savrig
157700070119     c                   endif
157800070119
157900070118      * verifico se esiste già un record
158000070118     c     Wf_tasrsc     chain     Wfrig00f
158100070118     c                   If        not %found(Wfrig00f)
158200070118
158300070118     c                   clear                   wfrig000
158400070118     c                   eval      wfrrsc = wf_tasrsc
158500070118     c                   z-add     savrig        wfrrig
158600070220     c                   movea     svr           wfrsva
158700070118     c                   eval      wfrdir = Stp_vard
158800070118     c                   eval      wfrspc = stp_pcml
158900070122     c                   If        WF_tasfrg = 'S'
159000070119     c                   add       1             wfrcod
159100070122     c                   endif
159200070118
159300070122     c                   write     Wfrig000
159400070118
159500070118     c                   else
159600070130      * valorizzo il flag di stampa peso Cml se non già valorizzato in precedenza
159700070130     c                   If        wfrspc = ' '
159800070118     c                   eval      wfrspc = stp_pcml
159900070118     c                   endif
160000070118
160100070220      * memorizzo tutte le varie della fattura non ancora memorizzate
160200070220     c                   movea     wfrsva        wsvr
160300070220     c                   do        24            a
160400070220     c                   if        svr(a) <> *blank
160500070220     c     svr(a)        lookup    wsvr                                   30
160600070220     c                   if        not %found
160700070220     c                   z-add     1             a0
160800070220     c     ' '           lookup    wsvr(a0)                               30
160900070220     c                   if        %found
161000070220     c                   movel     svr(a)        wsvr(a0)
161100070220     c                   endif
161200070220     c                   endif
161300070220     c                   endif
161400070220     c                   enddo
161500070119
161600070118     c                   add       savrig        wfrrig
161700070122     c                   If        WF_tasfrg = 'S'
161800070122     c                   add       1             wfrcod
161900070122     c                   endif
162000070122     c                   update    wfrig000
162100070118
162200070118     c                   endif
162300070131
162400070131     c                   endif
162500070118     c                   endsr
162600070119      ****************************************************
162700070119      ** Verifico  file di lavoro wfrig00f conta righe
162800070119      ****************************************************
162900070119     c     Ctr_totrig    Begsr
163000070220
163100070220     c                   clear                   savvar
163200070220
163300070119      * aggancio il file con il codice di raggruppamento
163400070119     c     Wf_tasrsc     chain     Wfrig00f
163500070119     c                   If        %found(Wfrig00f)
163600070122      * Diritto Fisso
163700070130     c                   If        wfrdir = ' '
163800070122      * varia D   aggiungo  1 riga
163900070122     c                   add       1             wfrrig
164000070122      * varia D   aggiungo una varia
164100070220     c                   add       1             savvar
164200070122     c                   endif
164300070122      * Stampa frase peso cml
164400070122     c                   If        wfrspc = 'S'
164500070122      *           aggiungo  1 riga
164600070122     c                   add       1             wfrrig
164700070122     c                   endif
164800070122      * varie
164900070220     c                   movea     wfrsva        wsvr
165000070220     c                   do        24            a
165100070220     c                   if        wsvr(a) = *blank
165200070220     c                   eval      savvar = a-1
165300070220     c                   leave
165400070220     c                   endif
165500070220     c                   enddo
165600070220      *
165700070220     c                   If        savvar > 0
165800070122      *           aggiungo  3 righe
165900070122     c                   add       3             wfrrig
166000070122      * se sono più di 8 varie aggiungo 1 riga
166100070220     c                   If        savvar > 8
166200070122     c                   eval      wfrrig = wfrrig + 1
166300070122     c                   endif
166400070122      * se sono più di 16 varie aggiungo un'altra riga
166500070220     c                   If        savvar > 16
166600070122     c                   eval      wfrrig = wfrrig + 1
166700070122     c                   endif
166800070122
166900070122     c                   endif
167000070122      * se c'è raggruppamento è valorizzato il numero di codici raggruppati
167100070122      * e mi serve per calcolare il numero di righe da stampare per eventuali
167200070122      * totali e sottototali
167300070122     c                   If        wfrcod > 0
167400070219     c                   eval      wfrrig = wfrrig + (3*wfrcod)
167500070122     c                   endif
167600070122
167700070122      * addiziono sempre 2 righe per eventuali esenzione dei bolli e stampa di dichiarazioni
167800070122      * d'intenti
167900070122     c                   add       2             wfrrig
168000070122
168100070122      * calcolo il numero di pagine
168200070122
168300070219      * sommo 1 pagina per le 51 righe precedenti
168400070130     c                   add       1             wfrpag
168500070219      * se righe maggiori di 51 (1° pagina) calcolo il numero delle pagine successive
168600070131      * divendo il numero di righe - la 1° pagine , diviso 71 il numero delle righe per le
168700070131      * pagine successive
168800070219     c                   if        wfrrig > 51
168900070219     c                   eval      wfrpag = wfrpag + ((wfrrig - 51) / 71)
169000070131      * se il totale della righe calcolate dal numero pagine è minore del totale
169100070131      * sommo 1 per le righe restanti
169200070219     c                   If        ((wfrpag * 71) - 20) < wfrrig
169300070122      * sommo 1 pagina per le righe restanti
169400070122     c                   add       1             wfrpag
169500070122     c                   endif
169600070131
169700070131     c                   endif
169800070122      * aggiorno il file con il numero pagina
169900070122     c                   update    wfrig000
170000070122
170100070122     c                   endif
170200070119     c                   endsr
170300050325      ****************************************************
170400050325      ** Carica importi varie da tita7000
170500050325      ****************************************************
170600050325     c     Carta7        Begsr
170700050325      * pulisco le schiere delle varie
170800050325     c                   Do        20            k
170900050325     c                   If        k > 3
171000050325     c                   clear                   VA(K)
171100050325     c                   clear                   SV(K)
171200050325     c                   Endif
171300050325     c                   Enddo
171400050325      * pulisco il falg che mi indica se ci sono delle varie su tita7
171500050325     c                   clear                   flgta7
171600050325      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
171700050325     c                   If        tassv3 <> *blanks
171800050325      * mi posiziono su tita7
171900050415     c     Kta7          setll     tita730c
172000050415     c                   if        %equal(tita730c)
172100050325     c                   z-add     3             k
172200050325      **
172300050325     c                   do        *hival
172400050415     c     Kta7          reade     tita730c
172500050325      * fine file
172600050415     c                   if        %eof(tita730c)
172700050325     c                   leave
172800050325     c                   endif
172900050325      * carico le varie
173000050325     c                   add       1             k
173100050325     c                   movel     ta7svn        sv(k)
173200050325     c                   z-add     ta7van        va(k)
173300050325     c                   z-add     1             flgta7
173400050325     c                   enddo
173500050325
173600050325     c                   endif
173700050325
173800050325     c                   endif
173900050325
174000050325     c                   Endsr
174100050331      ****************************************************
174200050331      ** Riverca Contrassegno
174300050331      ****************************************************
174400050331     c     Carcsb        Begsr
174500050331
174600050415     c     Kcsb          chain     Tncsb03l
174700050415     c                   If        %found(tncsb03l) and
174800050331      * solo per stesso tipo bolla
174900050419     c                             tastbl = csbtbl
175000050331     c                   movel     csbsta        Tassta
175100050331     c                   z-add     csbcas        Tascas
175200061016     c******             movel     csbtpa        Tastic
175300050331     c                   if        csbfus <> *blanks
175400050331     c                   move      csbfus        tastic
175500050331     c                   else
175600050331     c                   move      csbtpi        tastic
175700050331     c                   end
175800050331      * mittente
175900050331     c                   movel     csbvca        tasvca
176000050331     c                   z-add     csbcmb        tascmb
176100050331
176200050331     c                   endif
176300050331
176400050331     c                   endsr
176500050331      ***********************************************
176600050331      ** Aggiornamento bolle non contabilizzabili
176700050331      ***********************************************
176800050331     c     Conbol        Begsr
176900050331
177000050331      * aggiorno flag di tassazione / contabilizzazione / data fattura (inizio mese per
177100050331      * la mensile e data fattura per la fattura settimanale)
177200050331     c                   movel     '1'           tasfts
177300050331     c                   movel     '1'           tasfic
177400050331     c   20              z-add     dtdfi8        tasdft
177500050331     c  N20              z-add     dtdft8        tasdft
177600050331     c                   update    titas000
177700050331
177800050331     c                   endsr
177900050331      ****************************************************
178000050331      ** Tassazione Bolla
178100050331      ****************************************************
178200050331     c     Tassa         Begsr
178300050331
178400050331      * Clearizzo campi "VARIABILI" di DTAS
178500050331
178600050331     c                   movel     *Blanks       tastla
178700050331     c                   movel     *Blanks       taserr
178800050331     c                   movel     *Blanks       tasmsg
178900050331     c                   movel     Tasftc        tastc1
179000050331     c                   clear                   tasial
179100050331     c                   clear                   task88
179200050331     c                   clear                   tasfcaa
179300050331     c                   clear                   taspkcn
179400050929     c                   clear                   tasric
179500050331     c                   clear                   tasspc
179600050331     c                   clear                   tasBan
179700050331     c                   clear                   SavNcl
179800050331     c                   clear                   StpNcl
179900050331     c                   clear                   dAr5Ban
180000050331     c                   Clear                   dAr5Bnb
180100050331      *
180200050331      * Per la fatturazione     MENSILE uso la data fatturazione FINE MESE
180300050331      *
180400050331      * Per la fatturazione NON MENSILE uso la data fattura
180500050331      *
180600050331      *     Per cariacre la tariffa di cartello decorrente
180700050331      *
180800050331     c   20              Z-add     dtdff8        tasdct
180900050331     c  N20              Z-add     dtdft8        tasdct
181000050331
181100050331      * Flag SI NO DDT
181200050331    1C                   If        tasll1 = 'C' or tasll1 = 'S'
181300050331     C                   movel     'S'           Tasddt
181400050331   x1C                   Else
181500050331     C                   Clear                   Tasddt
181600050331    1C                   Endif
181700060922
181800060922      * valorizzo flag bolla di rientro e la consegna anomala da passare al pgm di tassazione
181900060922
182000060922     c******             movel     tasfbr        dstasflo
182100060922     c                   clear                   dtas01
182200060922     c                   movel     tasfbr        §asfbr
182300060922     c                   movel     tascca        §ascca
182400060922     c                   eval      dstasflo = dtas01
182500050331
182600050331      * Bancali
182700050331    1c                   If        %Subst(TasGva:1:1) = 'B'
182800050331
182900050331     c                   eval      kAr5Trd  = 'BAN'
183000050331     c     kFiar5        Chain     Fiar531c
183100050331If  1c                   If        %Found(Fiar531c)
183200050331     c                   Movel     Ar5Uni        dAr5Ban
183300050331    1c                   EndIf
183400050331      * Bancali
183500050331     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
183600050331    1c                   EndIf
183700050331      **
183800050331      * Colli Originali
183900050331    1c                   If        %Subst(TasGva:1:1) = 'O'
184000050331
184100050331     c                   eval      SavNcl = TasNcl
184200050331     c                   eval      kAr5Trd  = 'BNB'
184300050331     c     kFiar5        Chain     Fiar531c
184400050331If  1c                   If        %Found(Fiar531c)
184500050331     c                   Movel     Ar5Uni        dAr5Bnb
184600050331    1c                   EndIf
184700050331      * Colli
184800050331     c                   Eval      TasNcl = §Ar5bcor
184900050331     c                   Eval      StpNcl = §Ar5bcor
185000050331    1c                   EndIf
185100050331      **
185200050331     c                   Movel     Paramt        Kpjbu
185300050331      *
185400050331      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
185500050331      * con VARIA 'N' valorizzata con 888888 chiamo il TNSF20R per il calcolo dell'anteporto
185600050331      *
185700050331    1c                   if        tasimv = 0 and tasfbr = 'S'
185800050331     c                   z-add     1             xx
185900050331     c     'N'           lookup    sv(xx)                                 30
186000050331    2c                   if        *in30 and va(xx) =  88888888
186100050331      * tasso l'anteporto
186200050331     c                   exsr      Varian
186300050331    2c                   endif
186400050331      *
186500050331    1c                   endif
186600050331      *
186700050331      * se ritorno dal calcolo della varia N e c'è errore non proseguo
186800050331      *
186900050331     c                   Setoff                                       05
187000050331    1c                   if        taserr = ' '
187100050506      * Se devo tassare per una varia e c'e' gia', non richiamo la tassazione  se importo
187200050506      * della varia uguale  a zero
187300050331    2c                   if        tassva<>*blanks and tasimv=0
187400050506     c                   z-add     1             kk
187500050506     c     tassva        lookup    sv(kk)                                 05
187600050506     c                   if        *in05 and va(kk) > 0
187700050506     c                   setoff                                       05
187800050506     c                   endif
187900050331    2c                   endif
188000050331      * verifico se esiste la varia & con importo uguale a 8888888 e se si passo al
188100050331      * TNSF20R il flag di calcolo varia &
188200050331      *
188300050331     c                   setoff                                       51
188400050331     c                   z-add     1             kk
188500050331     c     '&'           lookup    sv(kk)                                 51
188600050331    2c                   if        *in51 and va(kk) = 88888888
188700050331     c                   eval      tasfcaa = 'S'
188800050331     c                   clear                   sv(kk)
188900050331     c                   clear                   va(kk)
189000050331    2c                   endif
189100050929      * verifico se devo calcolare l'addebito di lasciato avviso
189200060601      * dalla Fatturazione di giugno non si deve + calcolare per nessuno
189300050929      * Cliente di  Villorba   133
189400060601     c*****              If        Kscfil = 133
189500050929      * verifico se c'è evento
189600060601     c**** Kevb4         chain     Fnevb04l
189700050929
189800060601     c****               If        %found(Fnevb04l)
189900060601     c****               eval      tasric = 'S'
190000060601     c****               endif
190100050929
190200060601     c****               endif
190300050929
190400050331      **
190500050331      ** DATA FATT X CARIC.DATI TARIFFA CARTELLO 1° VOLTA
190600050331    2c                   if        tassva=*blanks or not *in05
190700050331     C                   call      'TNSF20R'
190800050331     C                   parm                    Kpjba
190900050331     C                   parm                    Dtas
191000050331     C                   parm                    Dtasv
191100050331      *
191200050331      * se ho tassato con il peso vld imposto il flag tasfpf
191300050331    3c                   if        Tasspc='S'
191400050331     c                   eval      tasfpf='V'
191500050331    3c                   endif
191600050331      *
191700050331     c* se ho utilizzato i colli originali per tassare ripristino quelli del file titas
191800050331      * Colli Originali
191900050331    1c                   If        %Subst(TasGva:1:1) = 'O' and savncl > 0
192000050331     c                   eval      TasNcl = SavNcl
192100050331     c                   endif
192200050419
192300050419     c                   If        tasspc= 'S'
192400050419      * mi salvo il peso da stampare
192500050419     c                   eval      stapkf=TasPkcN
192600050419     c                   else
192700050419     c                   eval      stapkf=taspkf
192800050419     c                   endif
192900050331      *
193000050331    2c                   endif
193100050331    1c                   endif
193200050331      **
193300050331     c                   Endsr
193400050331      ***************************************************
193500050331      ** Calcolo anteporto VARIA 'N'
193600050331      ***************************************************
193700050331     c     Varian        Begsr
193800050331
193900050331      * richiamo il programma di tassazione
194000050331      * senza passare le varie e il porto
194100050331      *
194200050331      * mi salvo le ds di tassazione
194300050331     c                   eval      w_dtas = dtas
194400050331     c                   eval      w_dtasv = dtasv
194500050331      * mi salvo la divisa
194600050331     c                   eval      w_tasdiv = tasdiv
194700050331
194800050331     c                   clear                   dtasv
194900050331     c                   clear                   tasdiv
195000050331     c                   clear                   taspor
195100050331
195200050331     c                   clear                   antepo
195300050331
195400050331     c                   call      'TNSF20R'
195500050331     c                   parm                    kpjba
195600050331     c                   parm                    dtas
195700050331     c                   parm                    dtasv
195800050331      * se non ci sono errori valorizzo la varia 'N'
195900050331     c                   if        taserr = ' '
196000050331     c                   z-add     tasimv        antepo
196100050331     c                   endif
196200050331      * ripristino le ds salvate prima
196300050331     c                   eval      dtas = w_dtas
196400050331     c                   eval      dtasv = w_dtasv
196500050331      * valorizzo l'anteporto nella varia N al posto di 88888888  se maggiore di zero
196600050331     c                   if        antepo > 0
196700050331     c                   z-add     antepo        va(xx)
196800050331     c                   endif
196900050331
197000050331     c                   endsr
197100050331      *************************************************
197200050331      **  Aggiorna spedizione Titas Tita7
197300050331      *************************************************
197400050331     c     Aggbol        Begsr
197500050331
197600050331      ** Se era già tassata e l'imponibile è uguale al precedente non
197700050331      ** aggiorno Tita7
197800050331     c                   if        preimv <> tasimv
197900050331
198000050331      ** Cancellazione di tutti i record esistenti prima della scrittura
198100050331
198200050331     c                   If        Flgta7 <> 0
198300050415     c     Kta7          setll     Tita7000
198400050331     c                   do        *hival
198500050415     c     Kta7          Reade     Tita730c
198600050415     c                   if        %eof(tita730c)
198700050331     c                   leave
198800050331     c                   endif
198900050415     c                   delete    Tita730c
199000050331     c                   enddo
199100050331     c                   endif
199200050331
199300050331      * carico le varie su tita7
199400050331
199500050331     c                   If        sv(4) <> *blanks
199600050331     c                   do        20            k
199700050331     c                   If        k > 3  and sv(k) <> *blanks
199800050331     c                   z-add     tasaas        ta7aas
199900050331     c                   z-add     taslnp        ta7lnp
200000050331     c                   movel     tasnrs        ta7nrs
200100050331     c                   z-add     tasnsp        ta7nsp
200200050331     c                   movel     tastbl        ta7tbl
200300050331     c                   movel     sv(k)         ta7svn
200400050331     c                   z-add     va(k)         ta7van
200500050415     c                   write     Tita7000
200600050331     c                   endif
200700050331     c                   enddo
200800050331     c                   Endif
200900050331
201000050331     c                   endif
201100050331
201200050331     c                   Endsr
201300050331      **************************************************
201400050331      ** Stampa Errori Manca tariffa
201500050331      *************************************************
201600050331     c     Errore        Begsr
201700050419
201800060228     c                   If        tasspc= 'S'
201900060228      * mi salvo il peso da stampare
202000060228     c                   eval      stapkf=TasPkcN
202100060228     c                   else
202200060228     c                   eval      stapkf=taspkf
202300060228     c                   endif
202400060228      *
202500060228     c                   clear                   Wfmtc000
202600060228
202700060228     c                   eval      Mtcdel = dateu
202800060228     c                   eval      Mtcksc = tasksc
202900060228     c                   eval      Mtcpoc = savfil
203000060228     c                   eval      Mtccar = Savcar
203100060228      * se cliente codificato
203200060228     c                   if        not *in10
203300060228     c                   eval      Mtcrag = taarsc
203400060228     c                   else
203500060228     c                   eval      Mtcrag = Bolra1
203600060228     c                   endif
203700060228
203800060228     c                   eval      Mtcage = Savage
203900060228     c                   eval      Mtcaas = Tasaas
204000060228     c                   eval      Mtclnp = Taslnp
204100060228     c                   eval      Mtcnrs = Tasnrs
204200060228     c                   eval      Mtcnsp = Tasnsp
204300060228     c                   eval      Mtctbl = Tastbl
204400060228     c                   movel     tasaas        Mtcdsp
204500060228     c                   move      tasmgs        Mtcdsp
204600060228     c                   eval      Mtcncl = Tasncl
204700060228     c                   eval      Mtcpkf = Stapkf
204800060228     c                   eval      Mtcvlf = Tasvlf
204900060228     c                   eval      Mtcfap = Tasfap
205000060228     c                   eval      Mtclna = Taslna
205100060228     c                   eval      Mtccts = Tascts
205200060228     c                   eval      Mtcfin = Tasfin
205300060301      * recupero la provincia
205400060301     c                   z-add     1             a
205500060301     c                   movel     *blanks       wctprv
205600060301     c     tascts        lookup    cts(a)                                 05
205700060301     c   05a             occur     dsct
205800060301      ** recupero provincia codice tassazione
205900060301     c   05              movel     §ctprv        wctprv
206000060228     c                   eval      Mtcpro = wctprv
206100060228     c                   eval      Mtctsp = Tastsp
206200060228     c                   eval      Mtcctr = Tasctr
206300060228     c                   eval      Mtcdiv = Tasdiv
206400060228     c                   eval      Mtcpor = Taspor
206500060228     c                   eval      Mtcimv = Tasimv
206600060228
206700060228     c                   if        not *in95 and not *in99
206800060228     c                   eval      Mtccas = Tascas
206900060228     c                   eval      Mtcvca = Tasvca
207000060228     c                   eval      Mtcbap = Tasbap
207100060228     c                   endif
207200060228
207300050331      ** valorizzo il messaggio di errore in base all'errore da stampare
207400050331
207500060301      * se manca tariffa
207600060301     c                   If        *in94
207700060301     c                   If        tasmsg <> *blanks
207800060301     c                   movel     tasmsg        mtcerr
207900060301     c                   else
208000050331      * se manca tariffa e non c'è un msg risultante ne impostiamo uno fisso
208100060228     c                   movel     e(6)          mtcerr
208200050331     c                   endif
208300060301     c                   endif
208400060228
208500060228     C   99              movel(p)  e(1)          mtcerr
208600060228     C   95              movel(p)  e(3)          mtcerr
208700050331      *
208800050331      * imponibile maggiore del massimo fatturabile
208900050331      *
209000050331     c                   If        *in04
209100050331     c                   eval      w017a = %editc(wimv:'2')
209200050331      * preparo il campo del messaggio
209300060228     c                   clear                   mtcerr
209400060228     c                   eval      mtcerr = 'IMPONIBILE ' + tasdiv + ' ' +
209500050331     c                             %trim(w017a) + ' MAGGIORE DEL LIMITE ' +
209600050331     c                             'FATTURABILE'
209700050331     c                   endif
209800050331      *
209900050331      * se è  un 8888 sempre manca tariffa
210000050331     c                   if        Ksccod = 8888
210100060228     c                   movel(p)  e(8)          mtcerr
210200050331     c                   endif
210300060228
210400060228     c                   write     Wfmtc000
210500060228
210600050331     c                   endsr
210700050413      **************************************************
210800050413      ** Stampa Errori Contabili
210900050413      *************************************************
211000050413     c     Errcon        Begsr
211100050413      *
211200050413      **
211300050413      ** Testata del manca Tariffa
211400050413     c                   If        prima = ' '
211500050413     c                   Except    testcon
211600050413     c                   z-add     10            crcon             3 0
211700050413     c                   movel     'N'           prima             1
211800050413     c                   endif
211900050413      ** Nuova filiale cambio pagina
212000050413      **
212100050413     c                   If        crcon > 62
212200050413     c                   Except    Testcon
212300050413     c                   z-add     10            cr
212400050413     c                   endif
212500050413      **
212600050413     c                   If        *in34
212700050413     c                   except    detfat
212800050413     c                   else
212900050413     c                   except    detcon
213000050413     c                   endif
213100050413      **
213200050413     c                   add       2             cr
213300050413      **
213400050413     c                   endsr
213500050408      ********************************************************
213600050408      ** Memorizzazione Anagrafica cliente intestazione fattura
213700050408      ********************************************************
213800050408     c     Memana        begsr
213900050408
214000050411     c                   clear                   wlbeti
214100050411     c                   move      clnKCC        fatkcc
214200050411     c                   move      clnKSC        fatksc
214300050411     c                   movel     fatksc        fatfil
214400060707      * cerco terminal di partenza
214500060707      * chiamo Fnlv55 con tipo terminal "O" = Ottico
214600050415     c                   clear                   dslv55
214700060707     c                   movel     'O'           d55tpt
214800060707     c****               movel     'P'           d55tpt
214900050411     c                   moveL     fatfil        d55lin
215000050411     c                   movel     dateu         d55drf
215100050411     C                   call      'FNLV55R'
215200050415     c                   parm                    dslv55
215300050411     c                   movel     d55tfp        asfat
215400050411
215500050411     c                   movel     sogdes        fatra1                         RAG.SOCIALE
215600050504     c                   movel     sogdes        fatra2                         PER 2 INTEST
215700050411     c                   movel     aclTFT        fattft                         TIPO FATTURA
215800050411
215900050411     c                   movel     acltdf        fatifm                         INIZIO/FINE MESE
216000050411     c                   move      clncondpag    fatcdp                         CODPAGAMENTO
216100050411     c                   z-add     clnscopag1    fatsc1               16        SCONTO CASSA
216200050411     c                   z-add     clngginpa1    fatip1                         INIZIO PAG
216300050411     c                   z-add     clngginpa2    fatip2                          ==     ==
216400050411     c                   movel     indindriz     fatvia                         VIA
216500050411     c                   movel     indlocalit    fatcit                         CITTA
216600050411     c                   movel     indcap        fatcap                         CAP
216700050411     c                   movel     indstato      fatsta                         NAZIONE
216800050411
216900050411      * Se nazione estera stampo il cap a nove
217000050411     c                   IF        fatsta <> *blanks and fatsta <> 'I' and
217100050411     c                             fatsta <> 'IT'
217200050411     c                   movel     indcap        fatcae
217300050411     c                   seton                                        01
217400050411     c                   else
217500050411     c                   setoff                                       01
217600050411     c                   endif
217700050411
217800050411     c                   movel     sogpartiva    fativa                         P.IVA
217900061108      * codice fiscle
218000061108     c                   movel     sogcdfisc     fatcdf
218100061108      * se codice fiscale a blank lo imposto uguale alla partita iva
218200061108     c                   if        fatcdf = *blanks
218300061108     c                   eval      fatcdf = fativa
218400061108     c                   endif
218500061108
218600050411     c                   movel     indprov       fatprv                         PROVINCIA
218700050411     c     kcba          chain     ancba01l
218800050411     c                   if        %found(ancba01l)
218900050411     c                   movel     *blank        fatbna
219000050411     c                   clear                   X59Abids
219100050411     c                   move      cbasocieta    x59soc
219200050411     c                   move      cbaabi        x59ABI
219300050411     c                   move      Cbacab        x59CAB
219400050411     c                   CALL      'X59ABI'
219500050411     c                   Parm                    x59abids
219600050411     c                   if        x59err<>'1'
219700050411     c                   movel(p)  x59desist     fatbna                         BANCA APP.
219800050411     c                   movel(p)  x59desbrag    wwwage                         BANCA APP.
219900050411     c                   move      wwwage        fatbna                         BANCA APP.
220000981021     c                   endif
220100981021     c                   endif
220200050411     c                   clear                   fatscit
220300050411     c                   clear                   fatsci
220400050411     c                   clear                   fatali
220500050411     c                   if        clncdiva <> *blank
220600050411     c                   move      clncdiva      fatsci
220700050411     c                   move      clncdiva      fatscit
220800050411     c                   else
220900050411     c                   move      §qtali        fatali
221000050411     c                   endif
221100050411     c                   exsr      protocollo
221200070117     c                   clear                   staese
221300050411
221400050411     c                   movel     clnspeboll    fatsbl                         SPESE BOLLO
221500050411      ** condizioni di pagamento
221600050411     c     keycdp        chain     ancdp01l
221700050411     c                   if        %found(ancdp01l)
221800050411     c                   movel     cdpdesbrev    wfades
221900050411     c                   else
222000050411     c                   move      *blank        wfades
222100050411     c                   endif
222200050411     c                   if        cdpmdpag <> '1'
222300050411     c                   movel     *blank        fatbna
222400050411     c                   endif
222500050411
222600011129      *
222700050411     c                   ENDSR
222800050408      ********************************************************************
222900050408      ** Memorizzazione diritto fatturazione dal cliente di raggruppamento
223000050408      ********************************************************************
223100050408     c     memdirfat     Begsr
223200030804      * salvo il diritto di fatturazione
223300050408     c                   move      aclSIN        DIRfatt                        diritto di fatturazi
223400030922      * se diritto di fatturazione è uguale a zero imposto flag di calcolo uguale "N"
223500030922      * e il flag di stampa uguale a "N"
223600030922     c                   If        DIRfatt = 0
223700030922     c                   eval      Cal_vard = 'N'
223800030922     c                   eval      Stp_vard = 'N'
223900030922     c                   endif
224000050408      *
224100050408     c                   endsr
224200050408      ********************************************************************
224300050408      ** Memorizzazione Anagrafica Clienti Raggruppamento / bollettazione
224400050408      ********************************************************************
224500050404     c     Membol        Begsr
224600030617      *
224700050404     c                   movel     sogdes        bolra1                         RAG.SOCIALE
224800050404     c                   movel     indindriz     bolvia                         VIA
224900050404     c                   movel     indlocalit    bolcit                         CITTA
225000050404     c                   movel     indcap        bolcap                         CAP
225100050404     c                   movel     indprov       bolprv                         PROVINCIA
225200050404     c                   movel     indstato      bolsta                         NAZIONE
225300050404      * Se nazione estera stampo il cap a nove
225400050404     c                   If        bolsta <> *blanks and bolsta <> 'I'  and
225500050404     c                             bolsta <> 'IT'
225600050404     c                   movel     indcap        bolcae
225700000110     c                   seton                                        15
225800050404     c                   endif
225900060214      * Frequenza fattura
226000060214     c                   eval      fatfft = aclfft
226100060228      * codice agente
226200060228     c                   eval      savage = aclage
226300050404      *
226400050404     c                   Endsr
226500050420      ********************************************************************
226600050420      ** Memorizzazione Anagrafica Clienti bollettazione
226700050420      ********************************************************************
226800050420     c     Membsc        Begsr
226900050421      *
227000050421     c                   setoff                                       16
227100050421     c                   move      *zeros        ktasbsc
227200050421     c                   move      wf_tasbsc     ktasbsc
227300050420      *
227400050421     c     Keybol        chain     ancln01l
227500050421     c     Clnsogg       chain     ansog01l
227600050421     c     Keyind        chain     anind01l
227700050420
227800050420     c                   movel     sogdes        bscra1                         RAG.SOCIALE
227900050420     c                   movel     indindriz     bscvia                         VIA
228000050420     c                   movel     indlocalit    bsccit                         CITTA
228100050421     c                   movel     indcap        bsccap                         CAP
228200050420     c                   movel     indprov       bscprv                         PROVINCIA
228300050420     c                   movel     indstato      bscsta                         NAZIONE
228400050421      * Se nazione estera stampo il cap a nove
228500050421     c                   If        bscsta <> *blanks and bscsta <> 'I'  and
228600050421     c                             bscsta <> 'IT'
228700050421     c                   movel     indcap        bsccae
228800050421     c                   seton                                        16
228900050421     c                   endif
229000050421      * velorizzo il fatto ceh lo devo stampare
229100050421     c                   eval      wstbsc = 'Y'
229200050421      * mi preparo la ds con  i dati del codice
229300050421     c                   clear                   dssktot
229400050421     c                   eval      tksc = wf_tasbsc
229500050421     c                   eval      trsc = bscra1
229600050421      *
229700050420      * reimposto i dati  di prima
229800050420     c     Keycli        chain     ancln01l                           99
229900050420     c     Clnsogg       chain     ansog01l                           99
230000050420     c     Keyind        chain     anind01l                           99
230100050420
230200050420     c                   endsr
230300050405      ***************************************************
230400050405      ** Calcolo bollo se esente IVA
230500050405      ***************************************************
230600050405     c     Calbol        Begsr
230700050405
230800050405      * recupero i valori per il calcolo del bollo
230900050405
231000050405     c                   If        impbold >= §geimb
231100050601      * se fattura con data <= 20050531 bollo fisso a 1,29 altrimenti prende da tabella
231200050601     c                   if        fatdft <= 20050531
231300050601     c                   z-add     1,29          totbold
231400050601     c                   else
231500050405     c                   z-add     §geibl        totbold
231600050601     c                   endif
231700050601
231800050405     c                   endif
231900050405
232000050405     c                   Endsr
232100050421      *****************************************************
232200050421      ** Stampa totale per codice di bollettazione
232300050421      *****************************************************
232400050421     c     SR_Totbol     begsr
232500050421      *
232600050421      * diritto di fatturazione
232700050421      * se deve essere ancora stampato lo faccio ora
232800050421     c                   if        stp_vard = ' '
232900050421      ** salto pagina
233000050421     c                   exsr      salpag
233100050421      *
233200050421     c                   select
233300050421      ** autofattura
233400050421     c     fattft        wheneq    1
233500050421     c                   except    ddvdd2
233600050421      ** Fattura inizio mese
233700050421     c     fatfiv        wheneq    §qcfiv
233800050421     c                   except    ddvdi2
233900050421      ** Fattura fine mese
234000050421     c                   other
234100050421     c                   except    ddvdf2
234200050421     c                   endsl
234300050421      *
234400050421     c                   eval      stp_vard = 'N'
234500050421     c                   endif
234600050421
234700050421      **  STAMPO I TOTALI
234800050422      ** 1 riga vuota
234900050421      ** salto pagina
235000050421     c                   exsr      salpag
235100050620
235200050620      * mi salvo i valori in schiera
235300050620     c                   add       1             itot
235400050620     c                   z-add     page          tpgf
235500050620     c                   eval      sktot(itot) = dssktot
235600050620      * preparo campi di stampa
235700050620     c                   z-add     tass          tasss
235800050620     c                   z-add     tfra          tfras
235900050620     c                   z-add     timp          timps
236000050421      *
236100050421     c                   select
236200050421      ** autofattura
236300050421     c     fattft        wheneq    1
236400050422     c                   except    tbodd0
236500050421      ** Fattura inizio mese
236600050421     c     fatfiv        wheneq    §qcfiv
236700050422     c                   except    tbodi0
236800050421      ** Fattura fine mese
236900050421     c                   other
237000050422     c                   except    tbodf0
237100050421     c                   endsl
237200050422      ** salto pagina
237300050422     c                   exsr      salpag
237400050422      *
237500050422     c                   select
237600050422      ** autofattura
237700050422     c     fattft        wheneq    1
237800050422     c                   except    tbodd1
237900050422      ** Fattura inizio mese
238000050422     c     fatfiv        wheneq    §qcfiv
238100050422     c                   except    tbodi1
238200050422      ** Fattura fine mese
238300050422     c                   other
238400050422     c                   except    tbodf1
238500050422     c                   endsl
238600050422      ** 1 riga vuota
238700050422      ** salto pagina
238800050422     c                   exsr      salpag
238900050422      *
239000050422     c                   select
239100050422      ** autofattura
239200050422     c     fattft        wheneq    1
239300050422     c                   except    tbodd0
239400050422      ** Fattura inizio mese
239500050422     c     fatfiv        wheneq    §qcfiv
239600050422     c                   except    tbodi0
239700050422      ** Fattura fine mese
239800050422     c                   other
239900050422     c                   except    tbodf0
240000050422     c                   endsl
240100050422      ** 1 riga vuota
240200050422      ** salto pagina
240300050422     c                   exsr      salpag
240400050422      *
240500050422     c                   select
240600050422      ** autofattura
240700050422     c     fattft        wheneq    1
240800050422     c                   except    tbodd0
240900050422      ** Fattura inizio mese
241000050422     c     fatfiv        wheneq    §qcfiv
241100050422     c                   except    tbodi0
241200050422      ** Fattura fine mese
241300050422     c                   other
241400050422     c                   except    tbodf0
241500050422     c                   endsl
241600050421
241700050421     c                   clear                   dssktot
241800050509
241900050509      * Aggancio YBACL con codiec di bollettazione
242000050509
242100050509     c                   move      tasbscpre     clnksc
242200050509
242300050509     c     Keybac        chain     ybacl01l
242400050509
242500050509     c                   exsr      aggbcl
242600050421      *
242700050421     c                   ENDSR
242800050421      *****************************************************
242900050421      ** Stampa riepilogo totale per codice di bollettazione
243000050421      *****************************************************
243100050421     c     SR_Riepil     begsr
243200050421
243300050421      **  STAMPO I TOTALI
243400050421
243500050421      ** salto pagina
243600050421     c                   exsr      salpag
243700050421      *
243800050421     c                   select
243900050421      ** autofattura
244000050421     c     fattft        wheneq    1
244100050421     c                   except    tbodd2
244200050421      ** Fattura inizio mese
244300050421     c     fatfiv        wheneq    §qcfiv
244400050421     c                   except    tbodi2
244500050421      ** Fattura fine mese
244600050421     c                   other
244700050421     c                   except    tbodf2
244800050421     c                   endsl
244900050421
245000050421      * stampo i valori in schiera
245100050421      * elaboro ogni elemenmto della schiera
245200050421
245300050421     c                   z-add     1             itotw
245400050421     c                   clear                   tasstw
245500050421     c                   clear                   tfratw
245600050421     c                   clear                   timptw
245700050421     c                   Dow       itotw<= itot
245800050421     c                   eval      dssktot = sktot(itotw)
245900050422      * preparo campi di stampa
246000050422     c                   z-add     tass          tasss
246100050422     c                   z-add     tfra          tfras
246200050422     c                   z-add     timp          timps
246300050422
246400050421
246500050421      **  STAMPO I TOTALI
246600050421
246700050421      ** salto pagina
246800050421     c                   exsr      salpag
246900050421      *
247000050421     c                   select
247100050421      ** autofattura
247200050421     c     fattft        wheneq    1
247300050421     c                   except    tbodd1
247400050421      ** Fattura inizio mese
247500050421     c     fatfiv        wheneq    §qcfiv
247600050421     c                   except    tbodi1
247700050421      ** Fattura fine mese
247800050421     c                   other
247900050421     c                   except    tbodf1
248000050421     c                   endsl
248100050620      ** salto pagina
248200050620     c                   exsr      salpag
248300050620      *
248400050620     c                   select
248500050620      ** autofattura
248600050620     c     fattft        wheneq    1
248700050620     c                   except    tbodd1P
248800050620      ** Fattura inizio mese
248900050620     c     fatfiv        wheneq    §qcfiv
249000050620     c                   except    tbodi1P
249100050620      ** Fattura fine mese
249200050620     c                   other
249300050620     c                   except    tbodf1P
249400050620     c                   endsl
249500050421
249600050421     c                   add       tass          tasstw
249700050421     c                   add       tfra          tfratw
249800050421     c                   add       timp          timptw
249900050421
250000050421     c                   add       1             itotw
250100050421
250200050421     c                   enddo
250300050421      *
250400050421      * stampo la riga dei totali riepilogo
250500050421      *
250600050421     c                   z-add     tasstw        tassst
250700050421     c                   z-add     tfratw        tfrast
250800050421     c                   z-add     timptw        timpst
250900050421
251000050421      ** salto pagina
251100050421     c                   exsr      salpag
251200050421      *
251300050421     c                   select
251400050421      ** autofattura
251500050421     c     fattft        wheneq    1
251600050421     c                   except    tbodd3
251700050421      ** Fattura inizio mese
251800050421     c     fatfiv        wheneq    §qcfiv
251900050421     c                   except    tbodi3
252000050421      ** Fattura fine mese
252100050421     c                   other
252200050421     c                   except    tbodf3
252300050421     c                   endsl
252400050421
252500050421
252600050421     c                   ENDSR
252700050404      *****************************************************
252800050404      ** Stampa totale fattura e contabilizza
252900050404      *****************************************************
253000050404     c     Totcod        begsr
253100050404      *
253200050404      * diritto di fatturazione
253300050404      * se deve essere ancora stampato lo faccio ora
253400050404     c                   if        stp_vard = ' '
253500050404      ** salto pagina
253600050404     c                   exsr      salpag
253700030923      *
253800030923     c                   select
253900030923      ** autofattura
254000050404     c     fattft        wheneq    1
254100050404     c                   except    ddvdd2
254200050404      ** Fattura inizio mese
254300050404     c     fatfiv        wheneq    §qcfiv
254400050404     c                   except    ddvdi2
254500050404      ** Fattura fine mese
254600050404     c                   other
254700050404     c                   except    ddvdf2
254800050404     c                   endsl
254900030923      *
255000050404     c                   eval      stp_vard = 'N'
255100050404     c                   endif
255200050427      * se c'è fatturazione raggruppata eseguo il totale dell'ultimo codice di bollettazione
255300050427     c                   if        tasfrgpre =  'S'
255400050421     c                   exsr      SR_totbol
255500050421      * se ci sono più di un codice di bollettazine raggruppato faccio anche il riepilogo
255600050421     c                   If        itot > 1
255700050421     c                   exsr      SR_riepil
255800050421     c                   endif
255900050421      * pulisco la schiera dei subtotali per raggruppamento
256000050421     c                   clear                   sktot
256100050426     c                   clear                   itot
256200050421
256300050421     c                   endif
256400050421
256500050404      * se ci sono degli importi con decimali < 0,03 --> porto a 0,03
256600050404      *  per avere il minimo del calcolo dell'iva
256700050405     c                   do        5             ww                2 0
256800050405     c                   if        imdw(ww)>0 and imdw(ww)<0,03
256900050405     c                   eval      imdw(ww)=0,03
257000050405     c                   eval      v1d(ww) =0,03
257100050405     c                   endif
257200050405     c                   enddo
257300050404     c
257400050404      *
257500050404      ** Esegue Calcolo IVA
257600050407     c                   Exsr      Caliva
257700050404      *
257800050405      *
257900050405      * 11 ON - stampo dicitura : bollo assoloto virtualmente...
258000050405     c                   eval      *in11='0'
258100050405     c                   If         impbold > 0
258200050405     c                   z-add     0             totbold
258300050405      ** calcolo bollo esenzione
258400050405     c                   exsr      calbol
258500050415     c                   add(h)    totbold       fatbold
258600050405     c                   endif
258700050405      ** memorizza importi bolli in contropartita
258800050405     c                   IF        fatbold > 0
258900050405     c                   eval      *in11='1'
259000050405     c                   movel     'VARIE   '    Kcc
259100050405     c                   move      '6'           Kcc
259200050415     c                   z-add     fatbold       wimpor
259300050405     c                   exsr      conric
259400050405     c                   exsr      conmem
259500050405     c                   Endif
259600050405      *
259700050405      *------------------------------------------------------------------
259800050405     c                   z-add     0             fattotd
259900050405      * sposto  gli importi con 3 decimali in un campo con 2 decimali
260000050405     c                   do        5             a
260100050405     c                   add(h)    imdw(a)       imd(a)
260200050405     c                   end
260300050405     c
260400050405      * Se importi con decimali e c'e' una solo controp.<0,03 --> la porto
260500050405      *  a 0,03
260600050405     c                   clear                   wconta
260700050405     c
260800050405     c                   xfoot     icpdw         wtoticpd
260900050405     c
261000050405    1c                   if        wtoticpd<0,03
261100050405
261200050405    2c                   do        50            a
261300050405    3c                   if        icpdw(a)>0
261400050405     c                   add       1             wconta
261500050405    3c                   endif
261600050405    2c                   enddo
261700050405
261800050405    2c                   if        wconta=1
261900050405
262000050405    3c                   do        50            a
262100050405    4c                   if        icpdw(a)>0
262200050405     c                   z-add     0,03          icpdw(a)
262300050405    4c                   endif
262400050405    3c                   enddo
262500050405    2c                   endif
262600050405    1c                   endif
262700050405     c
262800050405     c
262900050405    1c                   do        50            a
263000050405     c                   add(h)    icpdw(a)      icpd(a)
263100050405    1c                   enddo
263200050405
263300050405      ** Calcolo Totale Fattura
263400050405
263500050405     c                   do        5             a
263600050405     c                   add       imd(a)        fattotd
263700050405     c                   add       ivd(a)        fattotd
263800050405     c                   enddo
263900050405
264000050405     c                   add(h)    fatbold       fattotd
264100050405      **
264200050405      *------------------------------------------------------------------
264300050405      ** Stampo totale fattura
264400050405     c                   exsr      wtrtot
264500050405      *------------------------------------------------------------------
264600050405      ** Imputazione contabile per le sole fatture (escludo le distinte)
264700050405     c                   If        Fattft = 0
264800050405     c                   exsr      crepn
264900050405     c                   endif
265000050405      *------------------------------------------------------------------
265100050405      ** Elenco distinte spedizione
265200050405     c                   If        fattft = 1
265300050405     c                   exsr      wtrdis
265400050405     c                   endif
265500060214      *------------------------------------------------------------------
265600060214      ** Scrittura riepilogo tipo invio fattura
265700060214     c                   exsr      wrtift
265800050405
265900050405     c                   clear                   wanspe
266000050405      *------------------------------------------------------------------
266100050509      * aggiorna cliente di bollettazione se non c'è raggruppamento
266200050509     c                   if        tasfrgpre =  ' '
266300050509      * recupero il codice di raggruppamento/bollettazione
266400050509     c                   if        savtasksc <> *blank
266500050509     c                   move      savtasksc     clnksc
266600050509     c                   endif
266700050509
266800050509     c     Keybac        chain     ybacl01l
266900050509
267000050405     c                   exsr      aggbcl
267100050509     c                   endif
267200050405      *------------------------------------------------------------------
267300050405      ** pulisco i campi di lavoro
267400050405     c                   exsr      blank
267500050405
267600050405     c                   Endsr
267700050405
267800050405      *******************************************************
267900050405      * Aggiorna Data ultima e prima spedizione
268000050405      *******************************************************
268100050405     c     aggbcl        begsr
268200050506
268300050405
268400050405      ** Data Ultima Spedizione
268500050405     c                   if        dspult > acldus
268600050405     c                   move      dspult        acldus
268700050405     c                   endif
268800050405
268900050405      ** Data prima spedizione
269000050405     c                   if        acldps = 0 or dsppri < acldps
269100050405     c                   move      dsppri        acldps
269200050405     c                   end
269300050405
269400050405     c                   Update    ybacl000
269500050509
269600050509      ** aggancio anche cnclp00f per aggiornare anche ORION senza trigger
269700050509
269800050509     c                   move      aclkcc        clpkcc
269900050509     c                   move      aclksc        clpksc
270000050509
270100060220     c     keyclp        Chain(e)  cnclp00f
270200060220    1c                   if        %error
270300060220     c     keyclp        Chain(n)  cnclp00f
270400060220     c                   Clear                   Trul82ds
270500060220     c                   Eval      ul82§rrn = cnclpnrri
270600060220     c                   Eval      UL82§FIL = 'CNCLP00F'
270700060220      * Effettuo la chiamata al *PGM d utilità
270800060220     c                   Call(e)   'TRUL82R'
270900060220     c                   Parm                    Trul82ds
271000060220      * se alloccato recupero utente e programma e preaparo il msg da inviare
271100060220    2c                   If        UL82§sts = 'A'
271200060220     c                   eval      msgjob = 'Il lavoro ' + %trim(UL82§JNAM) +
271300060220     c                             ',Utente ' + %trim(UL82§JUSR) + ' sta -
271400060220     c                             bloccando la FATTURAZIONE : TERMINARE'
271500060220     c                   Clear                   Trul82ds
271600060220     c                   Eval      ul82§rrn = cnclpnrri
271700060220     c                   Eval      UL82§FIL = 'CNCLP00F'
271800060220     c                   Eval      UL82§F5  = 'S'
271900060220     c                   Eval      UL82§F7  = 'S'
272000060220     c                   Eval      UL82§mss = msgjob
272100060220     c                   Eval      UL82§num = 99
272200060221     c                   Eval      UL82§att = 60
272300060220      * Effettuo la chiamata al *PGM d utilità
272400060220     c                   Call(e)   'TRUL82R'
272500060220     c                   Parm                    Trul82ds
272600060220    2c                   endif
272700060220
272800060220     c     keyclp        Chain     cnclp00f
272900060220
273000060220    1c                   endif
273100060220
273200050509     c                   If        %found(cnclp00f)
273300050509     c                   move      acldps        clpdps
273400050509     c                   move      acldus        clpdus
273500050509     c                   update    cnclp000
273600050509     c                   endif
273700050405
273800050509     c                   clear                   dspult
273900050509     c                   clear                   dsppri
274000050509
274100050405     c                   endsr
274200050411      ********************************************************
274300050411      ** Stampa testata e dettaglio e memorizzo dati  X CTB
274400050411      ********************************************************
274500050411     c     stacod        Begsr
274600050412      * memorizzo se devo stampare la dicitura del calcolo del peso in fondo alla fattura
274700050411     c                   If        Wf_tasspc = 'S'
274800050411     c                   eval      wcalcevdl = 'S'
274900050411      * mi salvo il peso da fatturare per stamparlo in fattura
275000050411     c                   eval      stapkf=wf_TaspkcN
275100050411     c                   else
275200050411     c                   eval      stapkf=taspkf
275300050411     c                   endif
275400050411      * verifico se è stata stampata la testata
275500050411    1c                   If        Wstcod = 0
275600050411      ** AUTOFATTURA
275700050411    2c                   IF        Fattft = 1
275800050411     c                   z-add     999999        fatnft
275900050411     c                   z-add     dtdfi8        fatdft
276000050411     c                   z-add     §qcfiv        fatfiv
276100050411     c                   move      viddfi        dataeur
276200050411     c     *dmy          move      dataeur       stadft
276300050411   X2c                   else
276400050411      ** Fattura Fine Mese
276500050411    3c                   If        Fatifm = 'F' and *in20                                FINE MESE
276600050411     c                   add       1             nftfm
276700050411     c                   z-add     nftfm         fatnft
276800050411      * memorizzo il primo numero fattura fine mese
276900050426    4c                   If        Wfine =  ' '
277000050411     c                   z-add     nftfm         wfipri
277100050411     c                   movel     '1'           wfine
277200050411    4c                   endif
277300050411     c                   z-add     dtdff8        fatdft
277400050411     c                   z-add     §qcfi2        fatfiv
277500050411     c                   move      viddff        dataeur
277600050411     c     *dmy          move      dataeur       stadft
277700050411   X3c                   else                                                   INIZIO MESE
277800050411      ** Fattura inizio mese
277900050411     c                   add       1             nftim
278000050411     c                   z-add     nftim         fatnft
278100050411      * memorizzo il primo numero fattura inizio mese
278200050411    4c                   if        winiz = ' '
278300050411     c                   z-add     nftim         winpri
278400050411     c                   movel     '1'           winiz
278500050411    4c                   endif
278600050411      *
278700050411     c                   z-add     §qcfiv        fatfiv
278800050411      * fatturazione mensile
278900050411    4c                   if        *in20                                        FINE MESE
279000050411     c                   z-add     dtdfi8        fatdft
279100050411     c                   move      viddfi        dataeur
279200050411     c     *dmy          move      dataeur       stadft
279300050411     c                   else                                                   NO FINE MESE
279400050411     c                   z-add     dtdft8        fatdft
279500050411     c                   move      viddft        dataeur
279600050411     c     *dmy          move      dataeur       stadft
279700050411    4c                   endif
279800050411    3c                   end
279900050411    2c                   end
280000050411      **
280100050411     c                   movel     fatksc        filpag
280200050411      * decodfica inn Azorg
280300050411     c                   exsr      organi
280400050411      ** stampa testata fattura
280500050411     c                   exsr      wtrhdg
280600050411      ** valorizzo il flag di stampa testata
280700050411     c                   z-add     1             wstcod
280800050411    1c                   endif
280900050411
281000050421      * verifico se devo stampare anagrafica bollettazione
281100050421    1c                   If        Wstbsc = 'Y'
281200050421      ** stampa testata fattura
281300050421     c                   exsr      wtrbsc
281400050421      ** valorizzo il flag di stampa testata
281500050421     c                   clear                   wstbsc
281600050421    1c                   endif
281700050411      ** Valorizzo i campi del TITAS
281800050411
281900050411     c                   movel     '1'           tasfts
282000050411     c                   movel     '1'           tasfic
282100050411     c                   z-add     fatnft        tasnft
282200050411     c                   z-add     fatdft        tasdft
282300050411     c                   z-add     fatfiv        tasfiv
282400050421
282500050421      ** contabilizzo
282600050421     c                   Exsr      Con001
282700050411      ** stampa fattura dettaglio spediixone
282800050411     c                   Exsr      Wtrdet
282900050421      ** se bolla raggruppata mi salvo i valori in schiera
283000050427     c                   If        wf_tasfrg = 'S'
283100050421     c                   if        Tipbol = 'F'
283200050421     c                   add       tasimv        tfra
283300050421     c                   else
283400050421     c                   add       tasimv        tass
283500050421     c                   endif
283600050421     c                   add       tasimv        timp
283700050421     c                   endif
283800050411
283900050411      * aggiorno titas
284000050429     c                   update    Titas30
284100050429      *
284200050412     c                   exsr      aggbol
284300050412C     * imposto ultima e prima data spedizione per codice raggruppamento
284400050412     c                   if        tasdsp > dspult
284500050412     c                   z-add     tasdsp        dspult
284600050412     c                   endif
284700050412
284800050412     c                   if        dsppri = 0 or tasdsp < dsppri
284900050412     c                   z-add     tasdsp        dsppri
285000050412     c                   endif
285100050412
285200050412C     *
285300050412     c                   endsr
285400050412      *****************************************************
285500050412      ** Stampa testata fattura
285600050412      *****************************************************
285700050412     c     wtrhdg        Begsr
285800050412
285900050412     c                   z-add     0             page              5 0
286000050412      * numero righe di stampa dettaglio in prima pagina
286100050412     c                   z-add     50            wrighe
286200050412      * Pulizia scheire della legenda
286300050412     c                   movel     *blanks       fes
286400050412     c                   movel     *blanks       nes
286500050412     c                   movel     *blanks       les
286600050412     c                   movel     *blanks       led
286700050412     c                   movel     *blanks       lin
286800051110
286900051110     c                   clear                   wmail
287000051118     c                   clear                   indmail
287100051110
287200050412     c                   movel     '001'         comlsf
287300050412     c                   movel     'L'           spwfls
287400050412      * aggancio FNSPE prima con il codice di raggruppamento  se manca con il codice di fatturazione
287500050412      *
287600051110    1c                   if        savtasksc <> *blank
287700050412     c                   move      savtasksc     Wclpksc                        * cod.raggruppamento
287800051118
287900051118     c                   exsr      RIC_fnspe
288000051110      * se non ho trovato ne l'indirizzo mail che l'etichetta
288100051110    2c                   If        wmail <> 'S' and wanspe <> 'S'
288200051110      * cerco con il codice di fatturazione
288300051110     c                   move      clnksc        wclpksc                        * cod fatturazione
288400051118     c                   exsr      RIC_fnspe
288500051110      *
288600051110    2c                   endif
288700051110      *
288800051110  x 1c                   else
288900050412     c                   move      clnksc        wclpksc                        * cod fatturazione
289000051118     c                   exsr      RIC_fnspe
289100051110    1c                   endif
289200051110      * se c'è indirizzo di spedizione e non c'è indirizzo mail stampo l'etichetta e la fattura
289300051110     c                   If        wanspe = 'S' and wmail <> 'S'
289400050412     c                   movel     '§*§'         wlbeti
289500051110     c                   endif
289600050412
289700050412      ** stampa etichetta per invio fattura
289800050412     c                   If        fatsta <> *blanks
289900050412     c                   clear                   ds15
290000050412     c                   move      '15'          cod
290100050412     c                   movel     *blanks       key
290200050412     c                   movel     fatsta        key
290300050412     c     KTabc         chain     tabel                              30
290400050412     c  N30              movel     tbluni        ds15
290500051125      ** se stato non italia e non esiste la mail stampo etichetta
290600051125     c                   if        §15ita <> 'I' and not *in30 and wmail = ' '
290700050412     c                   movel     '§*§'         wlbeti
290800050412     c                   endif
290900050412     c                   endif
291000051110      ** mail
291100051110     c                   If        wmail = 'S'
291200060324
291300060324      * cerco indirizzo mail mittente in base al P.O. di appartenenza del codice di
291400060324      * BOLLETTAZIONE  SE NON TROVATO NON MANDO VIA MAIL
291500060324
291600060324     c                   movel     bolksc        kscfil
291700060324     c                   clear                   trul44ds
291800060324     c                   eval      d44pgm = 'TNSF07R'
291900060324     c                   eval      d44po  = Kscfil
292000060324     c                   call      'TRUL44R'
292100060324     c                   parm                    trul44ds
292200060328      * se non trovato
292300060328     c                   If        d44mit = *blanks
292400060328     c                   eval      d44mit = constmail
292500060328     c                   endif
292600060324      * stampo indirizzo mittente
292700060324     c                   eval      mittmail = 'E-mail: ' + %trim(d44mit) + ';'
292800060324
292900060324     c                   eval      stmail1 = 'EMAIL1:'+ %subst(indmail: 1: 38)
293000061009     c                   eval      stmail2 = 'EMAIL2:'+ %subst(indmail: 39: 23)
293100060324     c                   endif
293200060324
293300060324     c                   If        wmail = ' '
293400051110     c                   clear                   stmail1
293500051110     c                   clear                   stmail2
293600060324     c                   clear                   mittmail
293700051110     c                   endif
293800050412      ** Autofattura
293900050412    1c                   if        fattft = 1
294000050412      *
294100050412     c                   if        not *in37
294200050412
294300050412     c                   movel     'DIS.SPE'     kmdst
294400050412     c                   move      kcdsi         kmdst
294500050412     c                   open      ftwcodds
294600050412     c                   seton                                        37
294700050412
294800050412     c                   endif
294900050412      * verifico quanti records ho stampato
295000050412     c                   if        nrd > 0
295100050412     c                   eval      §nrfd = (cur_page * 74)
295200050412     c                   if        §nrfd >= 600000
295300050412     c                   close     ftwcodds
295400050412     c                   open      ftwcodds
295500050412     c                   endif
295600050412     c                   endif
295700050412      *
295800050412     c                   except    hcodd
295900050412     c                   z-add     0             nrd
296000050412      * se codice bolletazione diverso da codice fatturazione
296100050421      * e non c'è codice raggruppamento
296200050412     c                   if        savtasksc <> ktasksc and savtasksc <> *blanks
296300050421     c                             and wf_tasfrg = ' '
296400050412     c                   except    h2codd
296500050412     c                   add       2             nrd
296600050412     c                   endif
296700050412      *
296800050412   x1c                   else
296900050412
297000050412      ** fattura inizio mese
297100050412
297200050412    2c                   if        fatfiv = §qcfiv
297300050412
297400050412     c                   if        not *in35
297500050412     c                   movel     'FT.C.I.'     kmdst
297600050412     c                   move      kcdsi         kmdst
297700050412     c                   open      ftwcodim
297800050412     c                   seton                                        35
297900050412     c                   endif
298000050412
298100050412     c                   if        *in35
298200000414      * sommo la fattura solo se non va stampata in sede
298300050412     c                   if        wlbeti = *blanks
298400050412     c                   add       1             §NRFI
298500050412     c                   endif
298600050412     c                   if        §nrfi > 1100
298700050412     c                   close     FTWCODIM
298800050412     c                   open      FTWCODIM
298900050412     c                   z-add     1             §NRFI
299000050412     c                   endif
299100050412     c                   endif
299200050412
299300050412     c                   except    hcodi
299400050412     c                   z-add     0             nri
299500050412      * se codice bolletazione diverso da codice fatturazione
299600050421      * e non c'è codice raggruppamento
299700050412     c                   if        savtasksc <> ktasksc and savtasksc <> *blanks
299800050421     c                             and wf_tasfrg = ' '
299900050412     c                   except    h2codi
300000050412     c                   add       2             nri
300100050412     c                   endif
300200050412
300300050412   x2c                   else
300400050412
300500050412      ** Fattura fine mese
300600050412
300700050412     c                   if        not *in38
300800050412     c                   movel     'FT.C.F.'     kmdst
300900050412     c                   move      kcdsi         kmdst
301000050412     c                   open      ftwcodfm
301100050412     c                   seton                                        38
301200050412     c                   endif
301300050412
301400050412     c                   If        *in38
301500000414      * sommo la fattura solo se non va stampata in sede
301600050412     c                   if        wlbeti = *blanks
301700050412     c                   add       1             §NRFM
301800050412     c                   endif
301900050412     c                   if        §nrfm > 1100
302000050412     c                   close     FTWCODFM
302100050412     c                   open      FTWCODfM
302200050412     c                   z-add     1             §NRFM
302300050412     c                   endif
302400050412     c                   endif
302500050412
302600050412     c                   except    hcodf
302700050412     c                   z-add     0             nrf
302800050412      * se codice bolletazione diverso da codice fatturazione
302900050421      * e non c'è codice raggruppamento
303000050412     c                   if        savtasksc <> ktasksc and savtasksc <> *blanks
303100050421     c                             and wf_tasfrg = ' '
303200050412     c                   except    h2codf
303300050412     c                   add       2             nrf
303400050412     c                   endif
303500050412
303600050412    2c                   endif
303700050412
303800050412    1c                   endif
303900050412
304000050412     c                   endsr
304100051118      *****************************************************************
304200051118      * Ricerca in FNSPE sia l'etichetta che la mail oppure nelle note
304300051118      *****************************************************************
304400051118     c     RIC_fnspe     begsr
304500051118
304600051118     c     kspe          chain     fnspe03l                           91
304700051118      *
304800051118    1c                   if        %found(fnspe03l)
304900051118      * memorizzo che c'e' Fnspe  se ragione sociale diversa da blank
305000051118    2c                   if        sperag <> *blanks
305100051118     c                   movel     'S'           wanspe
305200051118    2c                   endif
305300051118      * verifico se c'è indirizzo mail
305400051118     c     ksp2          chain     fnsp201l
305500051118    2c                   if        %found(fnsp201l) and SP2EST <> *BLANKS
305600051118     c                   eval      wmail = 'S'
305700060324     c                   eval      indmail = %trim(sp2est) + ';'
305800051118    2c                   endif
305900051118
306000051118    1c                   endif
306100051129      * se non trovato FNSPE  cerco nella nota "84"
306200051118
306300051129    1c                   If        not %found(fnspe03l)
306400051118     c                   eval      Knk1 = '0151' + %editc(WCLPksc:'X')
306500051118     c     Kntc          chain     Tfntc01l
306600051118    2c                   If        %found(tfntc01l) and ntcrnt <> *blanks
306700051118     c                   eval      wmail = 'S'
306800060324     c                   eval      indmail = %trim(ntcrnt) + ';'
306900051118    2c                   endif
307000051118    1c                   endif
307100051118
307200051118     c                   endsr
307300050421      ******************************************************
307400050421      * stampa dati anagrafici codice bollettazione raggruppato
307500050421      ******************************************************
307600050421     c     wtrbsc        begsr
307700050421
307800050421      ** salto pagina
307900050421     c                   exsr      salpag
308000050421
308100050421      * S T A M P A   : 1 riga anagrafica
308200050421
308300050421     c                   select
308400050421      * Autofattura
308500050421     c                   when      fattft = 1
308600050421     c                   except    db1odd
308700050421      * Fattura inizio mese
308800050421     c                   when      fatfiv = §qcfiv
308900050421     c                   except    db1odi
309000050421      * Fattura fine mese
309100050421     c                   other
309200050421     c                   except    db1odf
309300050421     c                   endsl
309400050421
309500050620      * valorizzo il primo numero pagina da
309600050620     c                   eval      tpgi = page
309700050421      ** salto pagina
309800050421     c                   exsr      salpag
309900050421
310000050421      * S T A M P A   : 2 riga anagrafica
310100050421
310200050421     c                   select
310300050421      * Autofattura
310400050421     c                   when      fattft = 1
310500050421     c                   except    db2odd
310600050421      * Fattura inizio mese
310700050421     c                   when      fatfiv = §qcfiv
310800050421     c                   except    db2odi
310900050421      * Fattura fine mese
311000050421     c                   other
311100050421     c                   except    db2odf
311200050421     c                   endsl
311300050421
311400050421     c                   endsr
311500050421
311600050412      ******************************************************
311700050412      * stampa dettaglio
311800050412      ******************************************************
311900050412     c     wtrdet        begsr
312000050412
312100050412      * prepara stampa dettaglio
312200050412
312300050412     c                   exsr      presta
312400050412     c                   clear                   wrma15
312500050412     c                   clear                   rma15
312600050412     c                   clear                   rma09
312700050412     c                   setoff                                       29
312800050412      * se    bolle di richiesta bolle firmate cerco il record su titaa
312900050412      * valorizzo il nominativo del destinatario da stampare se taarsc diverso da blanks
313000050412     c                   if        tascbo = 'FO' or tascbo = 'FF'
313100050412     c                             or  tascbo = 'FI'
313200050412     c                   eval      §trc = 'O'
313300050412     c     ktaa          chain     Titaa30c
313400050412     c                   if        %found(titaa30c)  and
313500050412     c                             taarsc <> *blanks
313600050412     c                   eval      rsc13 = taarsc
313700050412     c                   endif
313800030512      * cerco il riferimento mittente alfanumerico
313900050412     c                   eval      §4trc = 'A'
314000050412     c     kta4          chain     tita430c
314100050412     c                   If        %found(tita430c)
314200050412     c                   movel     ta4not        dta4a
314300050412     c                   else
314400050412     c                   clear                   dta4a
314500050412     c                   endif
314600050412
314700050412     c                   If        §ta4arma <> *blanks
314800050412      * TA4Arma è alfanumerico di 15 se devono prendere
314900050412      * si devono spostare a destra e prendere gli ultimi 9 caratteri
315000050412      * e metterli in un campo di 9 alfanumerico che prenderà il posto
315100050412      * in caso di bolle firmate di Rmn9
315200050412     c                   movel     §ta4aRMA      Rma15
315300050412      * tolgo i blank non significativi
315400050412     c                   eval      II = 1
315500050412     c                   dow       II <= %len(%trim(Rma15))
315600050412      *
315700050412     c                   If        %subst(%trim(Rma15):II:1) <> *blank
315800050412     c                   eval      WRma15 = (%trim(Wrma15)
315900050412     c                                      + %subst(%trim(Rma15):II:1))
316000050412     c                   endif
316100050412      *
316200050412     c                   eval      II = II + 1
316300050412      *
316400030512     c                   enddo
316500050412     c                   evalr     rma09 = %trim(Wrma15)
316600050412     c                   seton                                        29
316700050412      *
316800050412     c                   endif
316900050412      *
317000050412     c                   endif
317100050412
317200050412      ** Carico schiera sigle varie X stampa legenda
317300050412    1C                   do        20            a
317400050412     c                   clear                   varfed
317500050412      * se varia valorizzata
317600050412    2C                   If        S(a) <> *blanks and id(a) > 0
317700050412      * se si tratta di bolla fedex controllo se la varia ha descrizione div
317800050412    3c                   if        Tasfie='F'
317900050412     c                   movel     'VARIE   '    KCC
318000050412     c                   move      S(A)          KCC
318100050412     c                   z-add     1             A2
318200050412     c     kcc           Lookup    cc(A2)                                 05
318300020320    4c                   if        *in05
318400050412     c     a2            occur     dscc
318500050412      * descrizione  a parte memorizzo in altra schiera
318600050412    5c                   if        §ccdfe='S'
318700050412     c                   eval      varfed='S'
318800050412     c                   z-add     1             A2
318900050412     c     s(a)          lookup    fes(a2)                                05
319000050412      ** non trovata in legenda la carico
319100050412    6c                   if        not *in05
319200050412     c                   z-add     1             a2
319300050412     c     *blanks       lookup    fes(a2)                                06
319400050412     c   06              movel     s(a)          fes(a2)
319500050412    6c                   endif
319600050412    5c                   endif
319700050412    4c                   endif
319800050412    3c                   endif
319900050412
320000020320    3c                   if        varfed=*blanks
320100050412     c                   z-add     1             A2
320200050412     c     s(a)          lookup    nes(a2)                                05
320300050412      ** non trovato in legenda la carico
320400050412    4c                   if        not *in05
320500050412     c                   z-add     1             a2
320600050412     c     *Blanks       lookup    nes(a2)                                06
320700050412     c   06              movel     s(a)          nes(a2)
320800050412    4c                   endif
320900050412      *
321000050412    3c                   endif
321100050412    2c                   endif
321200050412    1c                   enddo
321300030924      ** se devo ancora stampare la varia D e l'ho già calcolata la tolgo dalle varie
321400050412      ** da stampare  e la memorizzo per stamparla al momento giusto
321500050412     c                   if        stp_vard = ' ' and cal_vard = 'N'
321600050412     c                   z-add     1             dd                3 0
321700050412     c     'D'           lookup    S(dd)                                  13
321800050412     c                   if        %found
321900050413     c                   z-add     id(dd)        variadd
322000050413     c                   z-add     0             id(dd)
322100050412     c                   clear                   s(dd)
322200050412     c                   endif
322300050412      *
322400050412     c                   endif
322500050412      *
322600050412      ** salto pagina
322700050412     c                   exsr      salpag
322800050412
322900050412      * S T A M P A   : 1 riga dettaglio
323000050412
323100050412     c                   select
323200050412      * Autofattura
323300050412     c                   when      fattft = 1
323400050412     c                   except    dcodd
323500050412      * sommo numero spedizioni per autofatture da stampare di fianco al totale nella
323600050412      * lista delle distinte di spedizione
323700050412     c                   add       1             numspe
323800050412      * Fattura inizio mese
323900050412     c                   when      fatfiv = §qcfiv
324000050412     c                   except    dcodi
324100050412      * Fattura fine mese
324200050412     c                   other
324300050412     c                   except    dcodf
324400050412     c                   endsl
324500050412
324600050412      * se devo stampare il riferimento mittente recupero da TAA
324700050412
324800000119     c                   clear                   wmitori
324900050412     c                   clear                   wparcel
325000050412     c                   setoff                                       192223
325100050412     c                   setoff                                       24
325200050412
325300050412     c                   If        stprmo <> *blank or
325400050412      * oppure se bolla di recupero dei ritiri annullati
325500050412     c                             tascbo='FY'
325600050412     c                   eval      §trc = 'O'
325700050412     c     ktaa          chain     Titaa30c
325800050412     c                   if        %found(titaa30c)
325900050412     c                   movel     taarsc        wmitori
326000050412     c                   endif
326100050412      *
326200050412     c                   endif
326300050412
326400050412      * Se bolla di recupero ritiri annullati, stampo anche il rif.alfabetic
326500050412     c                   If        tascbo='FY'
326600050412     c                   eval      §4trc = 'A'
326700050412     c     Kta4          chain     tita430c
326800050412     c                   If        %found(tita430c)
326900050412     c                   movel     ta4not        dta4a
327000050412     c                   else
327100050412     c                   clear                   dta4a
327200051003     c                   endif
327300050412     c                   movel     §ta4arma      Wmitalf
327400050412     c                   seton                                        22
327500050412     c                   endif
327600050412
327700050412
327800050412      * stampa parcel perchè DPD e non è distinta di spedizioni
327900050412
328000050412     c                   if        stppar <> *blank and Fattft <> 1
328100050412
328200060703     c                   eval      §4trc = 'I'
328300060703     c     Kta4          chain     tita430c
328400060703     c                   If        %found(tita430c)
328500060703     c                   movel     ta4not        dsbl4i
328600060703     c                   eval      wparcel = §b4ipn
328700060703     c                   endif
328800050412     c                   endif
328900050412
329000060724      * se non è stato valorizzato nel mittente originale e neppure il parcel
329100060724      * verifico se si tratta di bolla fuori misura da addebitare a cliente DPD
329200060724      * riconosco un cliente dpd se stppar è diverso da blank (stampa numero parcel)
329300060724      * in più la bolla deve essere F3 codice bolla "B "
329400060724      * riconosco un fuori misura se nel tipo record O di titaa c'è la scritta OVERWEIGHT
329500060724     c                   If        (Wmitori = *blanks and wparcel = *blanks)
329600060724     c                             and stppar <> ' ' and tastbl = 'F3' and
329700060724     c                             tascbo = 'B '
329800060724     c                   eval      §trc = 'O'
329900060724     c     ktaa          chain     Titaa30c
330000060724     c                   if        %found(titaa30c) and
330100060724     c                             %subst(taarsc:1:10)= 'OVERWEIGHT'
330200060724     c
330300060724     c                   eval      %subst(wmitori:15:11) = 'OVERWEIGHT '
330400060724      * recupero numero parcel
330500060724     c                   eval      §4trc = 'A'
330600060724     c     Kta4          chain     tita430c
330700060724     c                   If        %found(tita430c)
330800060724     c                   movel     ta4not        dta4a
330900060724     c                   else
331000060724     c                   clear                   dta4a
331100060724     c                   endif
331200060724     c                   movel     §ta4arpt      Wparcel
331300060724
331400060724     c                   endif
331500060724
331600060724     c                   endif
331700050412      * se non ci sono varie da stampare stampo solo il riferimento mittente se valorizzato
331800050412      * o se valorizzato il parcel
331900050412     c                   If        s(4) = *blank and ((wmitori <> *blank) or
332000050412     c                             (wparcel <> *blank))
332100050412
332200050412     c                   eval      *in19 = (wparcel <> *blanks)
332300050412
332400050412      ** salto pagina
332500050412     c                   exsr      salpag
332600050412
332700050412     c                   select
332800050412      ** autofattura
332900050412     c                   When      fattft = 1
333000050412     c                   except    dmiod2
333100050412      ** fattura inizio mese
333200050412     c                   When      fatfiv = §qcfiv
333300050412     c                   except    dmioi2
333400050412      ** fattura fine mese
333500050412     c                   other
333600050412     c                   except    dmiof2
333700050412     c                   endsl
333800050412
333900050412     c                   endif
334000050412      **
334100050412      * se ci sono ancora varie
334200050412     c                   z-add     4             a
334300050415     c                   dow       s(a) <> *blanks
334400050412     c                   movea     S(A)          S2
334500050412     c                   movea     ID(A)         I2D
334600050412      ** salto pagina
334700050412     c                   exsr      salpag
334800050412
334900050412     c                   select
335000050412      ** autofattura
335100050412     c                   When      fattft = 1
335200050412     c                   except    dcodd2
335300050412      ** fattura inizio mese
335400050412     c                   When      fatfiv = §qcfiv
335500050412     c                   except    dcodi2
335600050412      ** fattura fine mese
335700050412     c                   other
335800050412     c                   except    dcodf2
335900050412     c                   endsl
336000050412
336100050412     c                   movel     *blanks       wmitori
336200050412     c                   add       3             a
336300050412     c                   enddo
336400050412      *
336500050412      * se ci sono colli originali da stampare !!!
336600050412      *
336700050412    1c                   if        stpncl > 0
336800050412      ** Salto pagina
336900050412     c                   exsr      salpag
337000050412      *
337100050412     c                   select
337200050412      ** autofattura
337300050412     c                   When      fattft = 1
337400050412     c                   except    dcood2
337500050412      ** fattura inizio mese
337600050412     c                   When      fatfiv = §qcfiv
337700050412     c                   except    dcooi2
337800050412      ** fattura fine mese
337900050412     c                   other
338000050412     c                   except    dcoof2
338100050412     c                   endsl
338200050412
338300050412    1c                   endif
338400050412      *
338500050412      * se bolla con esenzione IVA stampo la desc. della tab. EI del codice esenzione
338600050412      *
338700050412     c                   if        tasfei <> ' '  and  desesiva <> *blank
338800030909      ** Salto pagina
338900050412     c                   exsr      salpag
339000050412      *
339100050412     c                   select
339200050412      ** autofattura
339300050412     c                   When      fattft = 1
339400050415     c                   except    ddeid2
339500050412      ** fattura inizio mese
339600050412     c                   When      fatfiv = §qcfiv
339700050415     c                   except    ddeii2
339800050412      ** fattura fine mese
339900050412     c                   other
340000050415     c                   except    ddeif2
340100050412     c                   endsl
340200050412
340300050412     c                   endif
340400030923      * se sono in seconda pagina della fattura e devo ancora stampare la varia D stampo
340500030923     c                   if        stp_vard = ' ' and page = 2
340600050412      ** Salto pagina
340700050412     c                   exsr      salpag
340800050412      *
340900050412     c                   select
341000050412      ** autofattura
341100050412     c                   When      fattft = 1
341200050412     c                   except    ddvdd2
341300050412      ** fattura inizio mese
341400050412     c                   When      fatfiv = §qcfiv
341500050412     c                   except    ddvdi2
341600050412      ** fattura fine mese
341700050412     c                   other
341800050412     c                   except    ddvdf2
341900050412     c                   endsl
342000050412
342100050412     c                   eval      stp_vard = 'N'
342200050412
342300050412     c                   endif
342400050412
342500050412      ** pulizia campi
342600050412     c                   z-add     0             mdsp
342700050412     c                   z-add     0             gdsp
342800050412     c                   movel     *blanks       cit3
342900050412     c                   movel     *blanks       s
343000050412     c                   z-add     0             id
343100050412     c                   movel     *blanks       s2
343200050412     c                   z-add     0             i2d
343300050412     c                   z-add     0             prtpord
343400050412     c                   movel     *blanks       rsc13
343500050412     c                   movel     *blanks       wmitori
343600050412     c                   z-add     0             rmn9
343700050412     c                   z-add     0             StpNcl
343800050412     c                   clear                   DesEsIVA
343900050412
344000050412     c                   endsr
344100050412      ******************************************************
344200050412      * Prepara i campi per la stampa dettaglio
344300050412      ******************************************************
344400050412     c     presta        begsr
344500050412
344600050412      ** data spedizione
344700050412     c                   move      tasmgs        gdsp
344800050412     c                   movel     tasmgs        mdsp
344900050412     c                   movel     *blanks       wtsp
345000050412      **
345100050412      ** tipo tariffa a valore o quantità
345200050412     c     tasctr        comp      400                                09  09
345300050412      ** tipi servizio da stampare
345400050412     c     tastsp        lookup    ts                                     05
345500050412     c   05              movel     tastsp        wtsp
345600050412
345700050412      *
345800050412      ** Sigla autom.filiale di partenmza
345900050412
346000050412     c                   if        taslnp <> savlnp
346100050412     c     taslnp        chain     azorg01l                           05
346200050412     c                   If        %found(azorg01l)
346300050412     c                   movel     orgpro        cit2
346400050412     c                   movel     orgfl1        lnpfl1
346500050412     c                   movel     orgde3        og143
346600050412     c                   eval      lnpdpd = §ogdpd
346700050412     c                   else
346800050412     c                   clear                   cit2
346900050412     c                   clear                   lnpfl1
347000050412     c                   clear                   lnpdpd
347100050412     c                   movel     taslnp        savlnp
347200050412     c                   endif
347300050412     c                   endif
347400050412      *
347500050412      ** Stampa cap
347600050412     c                   If        tipbol = 'F'
347700050412     c                   movel     tascad        wcap
347800050412     c                   else
347900050412     c                   movel     tascam        wcap
348000050412     c                   endif
348100050412      ** Provincia codice tassazione destino
348200050412     c                   z-add     1             a
348300050412     c     tascts        Lookup    cts(a)                                 05
348400050412     c   05A             occur     dsct
348500050412     c   05              movel     §ctprv        cit3
348600050412      *
348700050412     c                   z-add     taspor        prtpord
348800050412      ** caricamento varie per stampe
348900050412     c                   z-add     0             ID
349000050412     c                   movel     *BLANKS       S
349100050412     c                   movea     SV            S
349200050412     c                   z-add     VA            ID
349300050412      *
349400050412      ** MITT-DESTIN STAMPA
349500050412     c     tipbol        ifeq      'F'
349600050412     c                   movel     tasrsd        rsc13            13
349700050412     c                   else
349800050412     c     wccm          ifeq      8888
349900050412     c                   movel     taarsc        rsc13
350000050412     c                   else
350100050412     C                   CLEAR                   DSBS69
350200050412     C                   MOVEL     KNSIF         I69SIF
350300050412     C                   MOVEL     KCI           I69KCC
350400050412     C                   Z-ADD     TASCCM        I69KAC
350500050412     C                   CALL      'TIBS69R'
350600050412     C                   PARM                    DSBS69
350700050412     C                   PARM                    DSACO
350800050412     C                   PARM                    DSIND
350900050412     C                   PARM                    DSCLP
351000050412     C                   PARM                    DSCLS
351100050412     C     O69ERR        IFNE      ' '
351200050412     C                   CLEAR                   dsaco
351300050412     C                   endif
351400050412     C                   MOVEL     CACORAG       RSC13
351500050412     c                   end
351600050412     c                   end
351700050412      ** RIF.MITTENTE 8
351800960906     C                   Z-ADD     TASRMN        RMN9              9 0
351900050412      *
352000050412      * VOLUME
352100050412     C     TASVLF        COMP      0                                  4141
352200050412      * verificp se devo stampare il riferimento mittente
352300960906     C                   ENDSR
352400050413      ******************************************************
352500050413      * SALTO PAGINA
352600050413      ******************************************************
352700050413     c     SALPAG        BEGSR
352800050413     c                   SELECT
352900050413      ** AUTOFATTURA
353000050413    1c     FATTFT        WHENEQ    1
353100050413     c                   ADD       1             NRD
353200050413    2c     NRD           IFGT      WRIGHE
353300050413     c                   EXCEPT    RCODD
353400050413     c                   EXCEPT    HCODD2
353500050413     c                   Z-ADD     1             NRD
353600050413      * LA PRIMA PAGINA SONO 50 RIGHE, POI  74
353700050413     c                   Z-ADD     74            WRIGHE            3 0
353800050413    2c                   ENDIF
353900050413      ** FATTURA INIZIO MESE
354000050413    1c     FATFIV        WHENEQ    §QCFIV
354100050413     c                   ADD       1             NRI
354200050413    2c     NRI           IFGT      WRIGHE
354300050413     c                   EXCEPT    RCODI
354400050413     c                   EXCEPT    HCODI2
354500050413     c                   Z-ADD     1             NRI
354600050413     c                   Z-ADD     74            WRIGHE            3 0
354700050413    2c                   ENDIF
354800050413      ** FATTURA FINE MESE
354900050413   X1c                   OTHER
355000050413     c                   ADD       1             NRF
355100050413    2c     NRF           IFGT      WRIGHE
355200050413     c                   EXCEPT    RCODF
355300050413     c                   EXCEPT    HCODF2
355400050413     c                   Z-ADD     1             NRF
355500050413     c                   Z-ADD     74            WRIGHE            3 0
355600050413    2c                   ENDIF
355700050413      *
355800050413    1c                   ENDSL
355900050413     c                   ENDSR
356000050406      ********************************************************
356100050406      ** Stampa totale fattura codificati
356200050406      ********************************************************
356300050406     c     Wtrtot        begsr
356400050406
356500050406      * stampo legenda tutti netwrk tranne fedex
356600050406     c                   movea     nes           les
356700050406     c                   eval      varfed=' '
356800050406     c                   exsr      staleg
356900050406      * stampo legenda per fedex
357000050406     c                   movea     fes           les
357100050406     c                   clear                   led
357200050406     c                   clear                   lin
357300050406     c                   eval      varfed='S'
357400050406     c                   exsr      staleg
357500050406      *
357600050406      ** controllo se ci sono importi esenti iva in fattura
357700050406     c                   setoff                                       45
357800050406     c                   do        5             a
357900050406
358000050406     c                   If        Imd(a) > 0 and ivd(a) = 0 and fatnpr > 0
358100050406     c                   eval      *in45 = *on
358200050406     c                   endif
358300050406
358400050406     c                   enddo
358500050406      **
358600050406
358700050406      ** STAMPA PROT. ESENZIONE IVA ANAGRAFICA CLIENTE
358800050406
358900050406    1c                   if        *in45 or *in11
359000050406      * stampa dicitura della esenzione
359100060719      * verifico data fattura se < 1/08/06 stampo la vecchia dicitura altrimenti la nuova
359200060719     c                   if        fatdft < 20060801
359300050406     c                   eval      staese=cesenz
359400060719     c                   else
359500060719     c                   eval      staese=cesenz_new
359600060719     c                   endif
359700050406
359800050406     c                   SELECT
359900050406      ** autofattura
360000050406    2c     fattft        wheneq    1
360100050406     c                   if        *in45
360200050406     c  n11              add       2             nrd
360300050406     c   11              add       3             nrd
360400050406     c                   else
360500050406     c                   add       1             nrd
360600050406     c                   endif
360700050406      * controllo over
360800050406    3c                   if        nrd > wrighe
360900050406     c                   except    rcodd
361000050406     c                   except    hcodd2
361100050406
361200050406     c                   if        *in45
361300050406     c  n11              z-add     2             nrd
361400050406     c   11              z-add     3             nrd
361500050406     c                   else
361600050406     c                   z-add     1             nrd
361700050406     c                   endif
361800050406     c                   z-add     74            wrighe
361900050406    3c                   endif
362000050406
362100050415     c                   Except    tcodd4
362200050406
362300050406      ** Fattura inizio mese
362400050406    2c     fatfiv        wheneq    §qcfiv
362500050406     c                   if        *in45
362600050406     c  n11              add       2             nri
362700050406     c   11              add       3             nri
362800050406     c                   else
362900050406     c                   add       1             nri
363000050406     c                   endif
363100050406
363200050406    3c                   If        nri > wrighe
363300050406     c                   except    rcodi
363400050406     c                   except    hcodi2
363500050406     c                   if        *in45
363600050406     c  n11              z-add     2             nri
363700050406     c   11              z-add     3             nri
363800050406     c                   else
363900050406     c                   z-add     1             nri
364000050406     c                   endif
364100050406     c
364200050406     c                   z-add     74            wrighe
364300050406    3c                   endif
364400050406
364500050406     c                   except    tcodi4
364600050406
364700050406      ** fattura fine mese
364800050406   X2c                   other
364900050406
365000050406     c  n11              add       2             nrf
365100050406     c   11              add       3             nrf
365200050406
365300050406    3c                   If        nrf > wrighe
365400050406     c                   except    rcodf
365500050406     c                   except    hcodf2
365600050406     c                   if        *in45
365700050406     c  n11              z-add     2             nrf
365800050406     c   11              z-add     3             nrf
365900050406     c                   else
366000050406     c                   z-add     1             nrf
366100050406     c                   endif
366200050406
366300050406     c                   z-add     74            wrighe
366400050406    3c                   endif
366500050406
366600050406     c                   except    tcodf4
366700050406    2c                   endsl
366800050406    1c                   endif
366900050406
367000050406      ** STAMPA dicitura del peso vdl
367100050406
367200050406     c                   if        wcalcevdl='S'
367300050406     c                   eval      stavdl=cpesovdl
367400050406
367500050406     c                   select
367600050406      ** autofattura
367700050406    2c     fattft        wheneq    1
367800050406     c                   add       1             nrd
367900050406    3c                   If        nrd > wrighe
368000050406     c                   except    rcodd
368100050406     c                   except    hcodd2
368200050406     c                   z-add     1             nrd
368300050406     c                   z-add     74            wrighe
368400050406    3c                   endif
368500050406
368600050406     c                   except    tcodd5
368700050406
368800050406      ** fattura inizio mese
368900050406    2c     fatfiv        wheneq    §qcfiv
369000050406     c                   add       1             nri
369100050406    3c                   if        nri > wrighe
369200050406     c                   except    rcodi
369300050406     c                   except    hcodi2
369400050406     c                   z-add     1             nri
369500050406     c                   z-add     74            wrighe
369600050406    3c                   endif
369700050406
369800050406     c                   except    tcodi5
369900050406      ** fattura fine mese
370000050406   X2c                   other
370100050406     c                   add       1             nrf
370200050406    3c                   if        nrf > wrighe
370300050406     c                   except    rcodf
370400050406     c                   except    hcodf2
370500050406     c                   z-add     1             nrf
370600050406     c                   z-add     74            wrighe
370700050406    3c                   endif
370800050406
370900050406     c                   except    tcodf5
371000050406
371100050406    2c                   endsl
371200050406
371300050406     c                   endif
371400050406      ** verifco se la fattura ha più di tre righe di totale
371500050406     c                   setoff                                       34
371600050406     c     imd(4)        comp      0                                  34
371700050406     c   34              exsr      errcon
371800050406
371900050406      ** salto fine pagina
372000050406      * autofattura
372100050406     c                   If        Fattft = 1
372200050406     c                   z-add     0             trigtd
372300050406     c                   except    tcodd
372400050406     c                   else
372500050406      ** inizio mese
372600050406     c                   If        fatfiv = §qcfiv
372700050406     c                   z-add     0             trigti
372800050406     c                   except    tcodi
372900050406      ** fine mese
373000050406     c                   else
373100050406     c                   z-add     0             trigtf
373200050406     c                   except    tcodf
373300050406     c                   endif
373400050406
373500050406     c                   endif
373600050406      **
373700050406      ** Stampo totali per aliquota IVA
373800050406    1c                   do        5             a
373900050406     c                   setoff                                       89
374000050406     c                   z-add     0             varied
374100050406     c                   z-add     0             periva
374200050406    2c                   if        imd(a) > 0
374300050406    3c                   if        ivd(a) > 0
374400050406     c                   seton                                        89
374500050406     c                   move      ai(a)         wiva52
374600050406     c                   z-add     wiva52        periva
374700050406    3c                   endif
374800050406      * Varie
374900050406     c                   add(h)    v1d(a)        varied
375000050406      * Porto
375100050406     c                   z-add(h)  pod(a)        prttpid
375200060908      * Faccio in modo che le somme del traposto e delle varie dia lo stesso importo dell'imponibile
375300060908     c                   If        varied > 0 and prttpid > 0
375400060908     c                   eval      varied = imd(a) - prttpid
375500060908     c                   else
375600060908     c                   if        varied > 0
375700060908     c                   z-add     imd(a)        varied
375800060908     c                   endif
375900060908     c                   if        prttpid> 0
376000060908     c                   z-add     imd(a)        prttpid
376100060908     c                   endif
376200060908     c                   endif
376300050406      * Autofattura
376400050406    3c                   if        fattft = 1
376500050406     c                   add       1             trigtd
376600050406    4c                   if        trigtd > 3
376700050406     c                   except    rcodd2
376800050406     c                   except    hcodd2
376900050406     c                   except    tcodd
377000050406     c                   z-add     0             trigtd
377100050406    4c                   endif
377200050406     c                   except    tcodd2
377300050406      * Inizio mese
377400050406   x3c                   else
377500050406    4c                   if        fatfiv = §qcfiv
377600050406     c                   add       1             trigti
377700050406    5c                   if        trigti > 3
377800050406     c                   except    rcodi2
377900050406     c                   except    hcodi2
378000050406     c                   except    tcodi
378100050406     c                   z-add     0             trigti
378200050406    5c                   endif
378300050406     c                   except    tcodi2
378400050406      * Fine mese
378500050406   x4c                   else
378600050406     c                   add       1             trigtf
378700050406    5c                   if        trigtf > 3
378800050406     c                   except    rcodf2
378900050406     c                   except    hcodf2
379000050406     c                   except    tcodf
379100050406     c                   z-add     0             trigtf
379200050406    5c                   endif
379300050406     c                   except    tcodf2
379400050406
379500050406    4c                   endif
379600050406    3c                   endif
379700050406    2c                   endif
379800050406    1c                   enddo
379900050406
380000050406      ** Stampo totale generale fattura
380100000126     c                   setoff                                       60
380200050406     c                   if        fattft = 1
380300050406      * controllo se ho scritto 1 sola riga di aliquote iva
380400050406     c                   eval      *in60 =  (trigtd =  1)
380500050406     c                   except    tcodd3
380600050406     c                   else
380700050406     c                   if        fatfiv = §qcfiv
380800050406     c                   eval      *in60 =  (trigti =  1)
380900050406     c                   except    tcodi3
381000050406     c                   else
381100050406     c                   eval      *in60 =  (trigtf =  1)
381200050406     c                   except    tcodf3
381300050406     c                   endif
381400050406     c                   endif
381500050406
381600050406     c                   z-add     0             wstcod
381700050406
381800050406     c                   endsr
381900020320     C/SPACE
382000050407      ******************************************************             1
382100050407      ** Stampo legenda mettendo le decodifiche
382200050407      ******************************************************
382300050407     c     staleg        begsr
382400050407
382500050407    1c                   if        les(1) <> *blanks
382600050407
382700050407      ** ricerca delle descrizioni
382800050407    3c                   do        20            a
382900050407    1c                   if        les(a) <> *blanks
383000050407     c                   z-add     1             a2
383100050407     c                   movel     'VARIE   '    kcc
383200050407     c                   move      les(a)        kcc
383300050407     c                   if        varfed=' '
383400050407     c     kcc           lookup    cc(a2)                                 05
383500050407     c   05a2            occur     dscc
383600050407     c   05              movel     §ccdes        led(a)
383700050407     c                   else
383800050407     c     kcc           lookup    cb(a2)                                 05
383900050407     c   05a2            occur     dscb
384000050407     c   05              movel     §cbdsf        led(a)
384100050407     c                   endif
384200050407     c   05              movel     '='           lin(a)
384300050407    4c                   endif
384400050407    3c                   enddo
384500050407      **
384600050407      * imposto la dicitura iniziale
384700050407     c                   if        varfed=' '
384800050407     c                   movel     clegenda      decleg
384900050407     c                   else
385000050407     c                   movel     clegendafed   decleg           20
385100050407     c                   endif
385200050407      *
385300050407     c                   z-add     1             a
385400050407     c                   dow       A <= 20 and les(a) <> *blank
385500050407      *
385600050407     c                   select
385700050407      ** Autofattura
385800050407    4c     Fattft        Wheneq    1
385900050407    5c                   If        a = 1
386000050407     c                   add       2             nrd
386100050407     c                   else
386200050407     c                   add       1             nrd
386300050407    5c                   endif
386400050407      *
386500050407    5c                   if        nrd > wrighe
386600050407     c                   except    rcodd
386700050407     c                   except    hcodd2
386800050407     c                   z-add     2             nrd
386900050407     c                   z-add     74            wrighe
387000050407    5c                   endif
387100050407      *
387200050407    5c                   If        a = 1
387300050407     c                   except    dcodd4
387400050407     c                   else
387500050407     c                   if        a = 9
387600050407     c                   except    dcodd5
387700050407     c                   else
387800050407     c                   except    dcodd6
387900050407     c                   endif
388000050407    5c                   endif
388100050407      ** Fattura inizio mese
388200050407    4c     Fatfiv        Wheneq    §qcfiv
388300050407    5c                   if        a = 1
388400050407     c                   add       2             nri
388500050407     c                   else
388600050407     c                   add       1             nri
388700050407    5c                   endif
388800050407      *
388900050407    5c                   if        nri > wrighe
389000050407     c                   except    rcodi
389100050407     c                   except    hcodi2
389200050407     c                   z-add     2             nri
389300050407     c                   z-add     74            wrighe
389400050407    5c                   endif
389500050407      *
389600050407    5c                   If        a = 1
389700050407     c                   except    dcodi4
389800050407     c                   else
389900050407     c                   if        a = 9
390000050407     c                   except    dcodi5
390100050407     c                   else
390200050407     c                   except    dcodi6
390300050407     c                   endif
390400050407    5c                   endif
390500050407      ** Fattura fine mese
390600020320   X4C                   OTHER
390700050407    5c                   if        a = 1
390800050407     c                   add       2             nrf
390900050407     c                   else
391000050407     c                   add       1             nrf
391100050407    5c                   endif
391200050407      *
391300050407    5c                   if        nrf > wrighe
391400050407     c                   except    rcodf
391500050407     c                   except    hcodf2
391600050407     c                   z-add     2             nrf
391700050407     c                   z-add     74            wrighe
391800050407    5c                   endif
391900050407      *
392000050407    5c                   If        a = 1
392100050407     c                   except    dcodf4
392200050407     c                   else
392300050407     c                   if        a = 9
392400050407     c                   except    dcodf5
392500050407     c                   else
392600050407     c                   except    dcodf6
392700050407     c                   endif
392800050407    5c                   endif
392900050407      *
393000050407    4c                   endsl
393100050407      *
393200050407     c                   add       8             a
393300050407    3c                   enddo
393400050407      *
393500050407    1c                   endif
393600050407      *
393700050407     c                   endsr
393800050406      ******************************************************
393900050406      ** Scrittura    NDBH*    Movimenti contabili
394000050406      ******************************************************
394100050406     c     crepn         begsr
394200050406     c                   z-add     0             riga
394300050406      * gestione del controllo e conversione nella moneta di conto
394400050406     c                   exsr      preppn
394500050406      *
394600050406     c                   z-add     0             rigai
394700050406     c                   setoff                                       39
394800050406     c                   clear                   ndbhm000
394900050406      ** TIPO RECORD *A*
395000050406      * ctb generale ndbhm
395100050406     c                   movel     §qccaf        bhmcaustes
395200050406     c  n10              movel     §qcca8        bhmcaustes
395300050406     c                   movel     knraz         bhmrifint
395400050406     c                   z-add     fatnft        bhmsubnum
395500050406     C                   movel     fatfiv        bhmsubnum
395600050406     c                   move      wsocieta      bhmsocieta
395700050406     c                   move      'CG'          bhmctb
395800050406     c                   move      'CG'          bhmclasse
395900050406     c                   movel     fatksc        uni3              3
396000050406     c                   movel(p)  uni3          bhmunita                       uff.telecomunicaz
396100050406     c                   move      'D'           bhmtpregz
396200050406     c                   move      $kcc          bhmkcc
396300050406     c                   move      *zeros        bhmksc
396400050406     c                   move      fatksc        bhmksc
396500050406     c                   move      fatdft        bhmdtreg
396600050406     c                   z-add     fatnft        bhmnrreg
396700050406     c                   move      fatdft        bhmdtdoc
396800050406     C                   Z-ADD     fatNFT        bhmnrdoc
396900050406     C                   move      fatDFT        bhmdtpar
397000050406     C                   Z-ADD     fatNFT        bhmnrpar
397100050406     c                   add       1             riga
397200050406     c                   z-add     riga          bhmsubrig
397300050406     C                   movel     '2'           bhmserreg
397400050406     C                   move      fatFIV        bhmserreg
397500050406      *
397600050406     c                   movel     fatra1        bhmdesbrev
397700050406     c                   if        xscdiv = viddiv
397800050406     c                   movel     xscdiv        bhmdivisa
397900050406     c                   z-add     fattotd       bhmimportd
398000050406     c                   z-add     fattotd       bhmimporto
398100050406     c                   z-add     1             bhmcambio
398200050406     c                   else
398300050406     c                   movel     viddiv        bhmdivisa
398400050406     c                   z-add     fattotd       bhmimportd
398500050406     c                   z-add     fattotcnt     bhmimporto
398600050406     c                   z-add     cambio0202    bhmcambio
398700050406     c                   endif
398800050406     c                   move      'D'           bhmsegno
398900050406     c                   movel     fatcdp        bhmcondpag
399000050406     c                   write     ndbhm000
399100050406      **
399200050406      ** TIPO RECORD  *C*
399300050406      * rek  IVA ndbhi
399400050406     c                   do        5             a
399500050406     c                   clear                   ndbhi000
399600050406     c                   if        ai(a) <> *blank
399700050406     c                   move      ai(a)         wivads
399800050406     c                   if        fatsci <> *blank
399900050406     c                   move      fatsci        bhirifiva
400000050406     c                   else
400100050406     c                   move      fatali        bhiali
400200050406     c                   endif
400300050406     c                   movel     knraz         bhirifint
400400050406     c                   z-add     fatnft        bhisubnum
400500050406     c                   movel     fatFIV        bhisubnum
400600050406     c                   movel     wsocieta      bhisocieta
400700050406     c                   add       1             rigai
400800050406     c                   z-add     rigai         bhisubrig
400900050406     c                   move      'A'           bhisegno
401000050406     c                   z-add     imd(a)        bhiimpon
401100050406     c                   z-add     imd(a)        bhiimpond
401200050406     c                   z-add     ivd(a)        bhiimportd
401300050406     c                   z-add     ivd(a)        bhiimporto
401400050406     c                   if        xscdiv <> viddiv
401500050406     c                   z-add     imc(a)        bhiimpon
401600050406     c                   z-add     ivc(a)        bhiimporto
401700050406     c                   endif
401800050406     c                   move      fatFIV        bhilibro
401900050406     c                   move      '2'           bhitpreg
402000050406     c                   write     ndbhi000
402100050406     c                   endif
402200050406     c                   enddo
402300050406      **  BOLLI
402400050406     c                   if        fatbold > 0
402500050406     c                   z-add     1             a
402600050406     c                   move      'VARIE   '    kcc
402700050406     c                   move      '6'           kcc
402800050406     c     kcc           lookup    cc(a)                                  05
402900050406     c   05a             occur     dscc
403000050406     c   05§ccjei        ifne      *blank
403100050406     c                   move      §ccjei        bhirifiva
403200050406     c                   z-add     0             bhiali
403300050406     c                   movel     knraz         bhirifint
403400050406     c                   z-add     fatnft        bhisubnum
403500050406     C                   movel     fatFIV        bhisubnum
403600050406     c                   movel     wsocieta      bhisocieta
403700050406     c                   add       1             rigai
403800050406     c                   z-add     rigai         bhisubrig
403900050406     c                   move      'A'           bhisegno
404000050406     c                   z-add     fatBOLD       bhiimpon
404100050406     c                   z-add     fatBOLD       bhiimpond
404200050406     c                   if        xscdiv <> viddiv
404300050406     c                   z-add     fatbolcnt     bhiimpon
404400050406     c                   endif
404500050406     c                   z-add     0             bhiimporto
404600050406     c                   z-add     0             bhiimportd
404700050406     c                   move      fatFIV        bhilibro
404800050406     c                   move      '2'           bhitpreg
404900050406     c                   write     ndbhi000
405000050406     c                   endif
405100050406     c                   endif
405200050406      **
405300050406      ** TIPO RECORD  *D*
405400050406      * analitica ndbha
405500050406     c                   clear                   ndbha000
405600050406     c                   do        50            a
405700050406     c                   if        icpd(a) <> 0
405800050406      *
405900050406     c                   move      ccp(a)        dsconti
406000050406     c                   move      wkcc          bhmkcc
406100050406     c                   move      wksc          bhmksc
406200050406     c                   move      wvoc          bhmvoce
406300050406     c                   z-add     icpd(a)       bhmimporto
406400050406     c                   z-add     icpd(a)       bhmimportd
406500050406     c                   if        xscdiv <> viddiv
406600050406     c                   z-add     icpc(a)       bhmimporto
406700050406     c                   endif
406800050406     c                   z-add     fatNFT        bhmnrpar
406900050406     c                   move      fatDFT        bhmdtpar
407000050406     c                   add       1             riga
407100050406     c                   z-add     riga          bhmsubrig
407200050406     c                   move      'A'           bhmsegno
407300050406      * cond pagamento
407400050406     c                   movel     fatcdp        bhmcondpag
407500050406      * ricavo data di competenza = data spedizione
407600050406      * a cambio anno = data fatturazione
407700050406     c                   move      dtdsp         bhmdtcoan
407800050406     c                   movel     fatdft        wannoft           4 0
407900050406     c                   if        wannoft <> tasaas
408000050406     c                   move      fatdft        bhmdtcoan
408100050406     c                   endif
408200050406     c                   write     ndbhm000
408300050406      *
408400050406     c                   move      bhmdtcoan     bhadtcoan
408500050406     c                   move      bhmrifint     bharifint
408600050406     c                   move      bhmsubnum     bhasubnum
408700050406     c                   move      bhmsubrig     bhasubrig
408800050406     c                   move      bhmvoce       bhavoce
408900050406     c                   move      'D1'          bhadimen
409000050406     c                   movel     bhmunita      bhaentit1
409100050406     c                   move      wsocieta      bhasocieta
409200050406     c                   z-add     icpd(a)       bhaimporto
409300050406     c                   z-add     icpd(a)       bhaimportd
409400050406     c                   if        xscdiv <> viddiv
409500050406     c                   z-add     icpc(a)       bhaimporto
409600050406     c                   endif
409700050406     c                   move      'A'           bhasegno
409800050406     c                   add       1             bhasubsubr
409900050406     c                   write     ndbha000
410000050406     c                   endif
410100050406     c                   enddo
410200050406      ** TIPO RECORD  *D*
410300050406      * se moneta diversa tra la divisa di fatturazione e la moneta di conto verifico se esistono
410400050406      * delle differenze di cambio
410500050406     c                   if        xscdiv <> viddiv
410600050406     c                   xfoot     icpc          wtoticpc
410700050406      * sommo i  valori che mi devono dare il totale fattura
410800050406     c                   eval      wtotfat = wtoticpc + wtotivc
410900050406      * calcolo la differenza dei totali fattura tra il convertito ed il calcolato
411000050406     c                   eval      diffe = fattotcnt - wtotfat
411100050406      * SE ESISTE DIFFERENZA
411200050406      *
411300050406     c                   if        diffe <> 0
411400050406      *
411500050406      * controllo se diffe è positivo o negativo
411600050406     c                   if        diffe < 0                                     * ARR. PASSIVO
411700050406      * recupero conto + causale contabile
411800050406     c     2             OCCUR     DARR
411900050406     c                   move      'D'           bhmtpregz
412000050406     c                   mult      -1            diffe
412100050406     c                   else                                                    * ARR. ATTIVO
412200050406     c     1             OCCUR     DARR
412300050406     c                   move      'A'           bhmtpregz
412400050406     c                   endif
412500050406      * ctb generale ndbhm
412600050406     c                   movel     §arrcau       bhmcausrig
412700050406      * Danilo ha detto di lasciare nft0 (ma io faccio di testa  mia)
412800050406     c                   move      §arrkcc       bhmkcc
412900050406     c                   move      §arrksc       bhmksc
413000050406     c                   move      §arrvoc       bhmvoce
413100050406     c                   add       1             riga
413200050406     c                   z-add     riga          bhmsubrig
413300050406      *
413400050406     c                   movel     xscdiv        bhmdivisa
413500050406     c                   movel     xscdiv        bhmdivisam
413600050406     c                   z-add     1             bhmcambio
413700050406     c                   z-add     diffe         bhmimporto
413800050406     c                   z-add     diffe         bhmimportd
413900050406     c                   move      bhmtpregz     bhmsegno
414000050406     c                   write     ndbhm000
414100050406      *
414200050406     c                   clear                   ndbha000
414300050406     c                   move      bhmdtcoan     bhadtcoan
414400050406     c                   move      bhmrifint     bharifint
414500050406     c                   move      bhmsubnum     bhasubnum
414600050406     c                   move      bhmsubrig     bhasubrig
414700050406     c                   move      bhmvoce       bhavoce
414800050406     c                   move      'D1'          bhadimen
414900050406     c                   movel     §arrcdc       bhaentit1
415000050406     c                   move      wsocieta      bhasocieta
415100050406     c                   z-add     diffe         bhaimporto
415200050406     c                   move      bhmtpregz     bhasegno
415300050406     c                   add       1             bhasubsubr
415400050406     c                   write     ndbha000
415500050406      *
415600050406     c                   endif
415700050406      *
415800050406     c                   endif
415900050406      *----------------------------------*
416000050406      ** Creazione totali fatturazione   *
416100050406      *----------------------------------*
416200050406      * totale imponile
416300000107     c                   do        5             A
416400050406     c                   add       IMD(A)        impgen
416500050406      * totale iva
416600050406     c                   add       IVD(A)        ivagen
416700050406      * in moneta di conto
416800000124     c                   if        viddiv <> xscdiv
416900000124     c                   add       imc(a)        impgencnt
417000000125     c                   add       ivc(a)        ivagencnt
417100000124     c                   endif
417200050406      *
417300050406     c                   enddo
417400050406      * totale bolli
417500050406     c                   add       fatbold       bolgen
417600050406      * totale fatturato
417700050406     c                   add       fattotd       totgen
417800050406      * in moneta di conto
417900050406     c                   if        viddiv <> xscdiv
418000050406     c                   add       fatbolcnt     bolgencnt
418100050406     c                   add       fattotcnt     totgencnt
418200050406     c                   endif
418300050406      * numero fatture
418400050406     c                   add       1             numgen
418500050406      *----------------------------------*
418600050406      ** stampa etichette                *
418700050406      *----------------------------------*
418800050406      * se ha l'etichetta
418900051128     c                   if        wanspe = 'S'  and
419000051130     c                             (wmail = ' ' or (wmail = 'S' and page >= 50))
419100050406      * scrivo record su ETLAV00F
419200050406     c                   clear                   etlav
419300050406      * codice cliente bollettazione se esiste uno di fatturazione prendo quello salvato
419400050406      * altrimenti prendo quello che ho in linea
419500050406     c                   IF        savtasksc > *blanks
419600050406     c                   move      savtasksc     ETLBOL
419700050406     c                   move      ktasksc       ETLFAT
419800050406     c                   else
419900050406     c                   move      ktasksc       ETLBOL
420000050406     c                   move      ktasksc       ETLFAT
420100050406     c                   endif
420200050406      * numero fattura
420300050406     c                   z-add     fatnft        ETLNDC
420400050406      * AS400 FATTURA
420500050406     c                   move      asfat         ETLASF
420600050406      *
420700060214      * scrivo record su ETLAV00F
420800050406     c                   write     ETLAV
420900050406      *
421000050406     c                   endif
421100051128
421200050406     c                   endsr
421300060214      ***********************************************************************
421400060214      ** PREPARAZIONE riepilogo tipo invio fattura
421500060214      ***********************************************************************
421600060214     c     Wrtift        begsr                                                                     i
421700060214      *
421800060214     c                   clear                   Tiift000
421900060214
422000060214     c                   z-add     fatnft        iftnft                         numero fattura
422100060214     c                   z-add     fatdft        iftdft                         data fattura
422200060214     c                   z-add     bolksc        iftcbo                         codice bollettazione
422300060214     c                   z-add     fatksc        iftcfa                         codice fatturazione
422400060214     c                   move      fatifm        ifttdf                         Inizio fine mese
422500060214     c                   move      fattft        ifttft                         tipo 1=Dist. 0=fatt.
422600060217     c                   move      fatfft        iftfft                         Freq.fattura (bol)
422700060217     c                   z-add     dtdsp         iftdsp                         data spedizione
422800060214     c                   z-add     page          iftpag                         pagine
422900060214     c                   z-add     fatdir        iftsif                         spese invio fattura
423000060307      * verifico se c'è etichetta da stampare oppure no
423100060307     c                   if        wanspe = 'S'
423200060307     c                   eval      ifteti = 'SI'
423300060307     c                   else
423400060307     c                   eval      ifteti = 'NO'
423500060307     c                   endif
423600060307      * tipo invio
423700060307     c                   SELECT
423800060307      * se è una distinta va su carta
423900060307     c                   when      fattft = 1
424000060307     c                   eval      ifttif = 'C'                                 Tipo invio Carta
424100060307      * se c'è §*§ carta
424200060307     c                   When      wlbeti <> *blank
424300060307     c                   eval      ifttif = 'C'                                 Tipo invio Carta
424400060307      * se c'è invio mail ed ho meno di 50 pagine
424500060307     c                   When      wmail = 'S' and page < 50
424600060307     c                   eval      ifttif = 'M'                                 Tipo invio Mail
424700060307      * se c'è invio mail ed ho più  di 49 pagine
424800060307     c                   When      wmail = 'S' and page > 49
424900060307     c                   eval      ifttif = 'C'                                 Tipo invio Carta
425000060307
425100060307      * altrimenti
425200060307     c                   OTHER
425300060307
425400060307      * postel se ho meno di 50 pagine
425500060307     c                   if        page <  50
425600060307     c                   eval      ifttif = 'P'                                 Tipo invio Postel
425700060307     c                   else
425800060307      * carta se ho più di 50 pagine
425900060307     c                   eval      ifttif = 'C'                                 Tipo invio Carta
426000060307     c                   endif
426100060307
426200060307     c                   ENDSL
426300060214
426400060217     c                   write     Tiift000
426500060214
426600060214     c                   endsr
426700050406      ***********************************************************************
426800050406      ** PREPARAZIONE IMPORTI PER REGISTRAZIONI PRIMA NOTA IN MONETA DI CONTO
426900050406      ***********************************************************************
427000060217     c     preppn        begsr
427100050406      *
427200050406     c                   clear                   fattotcnt
427300050406     c                   clear                   imc
427400050406     c                   clear                   ivc
427500050406     c                   clear                   icpc
427600050406     c                   clear                   fatbolcnt
427700050406     c                   clear                   wtotimc
427800050406      *
427900050406      * se moneta di conto diversa dalla moneta di fatturazione  conevrsione degli
428000050406      * importi
428100050406     c                   if        xscdiv <> viddiv
428200050406      *
428300050406      * CONVERSIONE
428400050406      * TOTALE FATTUTA
428500050406      *******   POTREBBE NON SERVIRE ********************************************
428600050406     c                   eval      div1 = viddiv                                *
428700050406     c                   eval      div2 = xscdiv                                *
428800050406     c                   reset                   PJX00202DS                     *
428900050406     c                   z-add     fattotd       IMPORD0202                     *
429000050406     c                   exsr      cnveur                                       *
429100050406     c                   z-add     imporm0202    fattotcnt                      *
429200050406      ***************************************************************************
429300050406      * BOLLI
429400050406     c                   if        fatbold > 0
429500050406     c                   eval      div1 = viddiv
429600050406     c                   eval      div2 = xscdiv
429700050406     c                   reset                   PJX00202DS
429800050406     c                   Z-ADD     fatbold       IMPORD0202
429900050406     c                   exsr      cnveur
430000000107     c                   z-add     IMPORM0202    fatbolcnt
430100000105     c                   endif
430200050406      * IMPONIBILI IVA
430300050406      *
430400050406     c                   do        5             a
430500050406     c                   if        imd(a) > 0
430600050406     c                   eval      div1 = viddiv
430700050406     c                   eval      div2 = xscdiv
430800050406     c                   reset                   PJX00202DS
430900050406     c                   z-add     imd(a)        IMPORD0202
431000050406     c                   exsr      cnveur
431100050406     c                   z-add     IMPORM0202    imc(a)
431200050406     c                   endif
431300050406     c                   enddo
431400050406      * IVA
431500050406     c                   DO        5             A
431600050406     c                   IF        ivd(a) > 0
431700050406     c                   eval      div1 = viddiv
431800050406     c                   eval      div2 = xscdiv
431900050406     c                   reset                   PJX00202DS
432000050406     c                   z-add     ivd(a)        IMPORD0202
432100050406     c                   exsr      cnveur
432200050406     c                   z-add     IMPORM0202    ivc(a)
432300050406     c                   endif
432400050406     c                   enddo
432500050406      * IMPORTI CONTROPARTITE
432600050406     c                   DO        50            A
432700050406     c                   IF        icpd(a) > 0
432800050406     c                   eval      div1 = viddiv
432900050406     c                   eval      div2 = xscdiv
433000050406     c                   reset                   PJX00202DS
433100050406     c                   Z-ADD     icpd(a)       IMPORD0202
433200050406     c                   exsr      cnveur
433300050406     c                   z-add     IMPORM0202    icpc(a)
433400050406     c                   endif
433500050406     c                   enddo
433600050406      *
433700050406     c                   endif
433800050406      * calcolo il totale fattura da attribuire al cliente
433900050406      * sommo imponibli iva
434000050406     c                   xfoot     imc           wtotimc
434100050406      * sommo iva
434200050406     c                   xfoot     ivc           wtotivc
434300050406      * sommo i  valori che mi devono dare il totale fattura
434400050406     c                   eval      fattotcnt = wtotimc + wtotivc + fatbolcnt
434500050406      *
434600050406      * SE MONETA DI CONTO UGUALE ALLA MONETA DI FATTURAZIONE
434700050406      * VERIFICO SE CI SONO DIFFERENZE TRA LE IMPUTAZIONI DI CONTO E IL TOTALE FATTURA
434800050406      * DATA LA PRESENZA DI IMPORTI CON NUMERI DI DECIMALI DIVERSI
434900050406      *
435000050406     c                   if        xscdiv = viddiv
435100050406      * calcolo totale contropartite
435200050406     c                   xfoot     icpd          wtoticpd
435300050406      * calcolo totale iva
435400050406     c                   xfoot     ivd           wtotivd
435500050406      * calcolo il totale fattura
435600050406     c                   eval      wtotfatd = wtoticpd + wtotivd
435700050406      * calcolo la differenza tra il totale fattura calcolato e quello da imputare
435800050406      * al cliente ....
435900050406     c                   eval      diffe    = fattotd - wtotfatd
436000050406      *
436100050406      * se differenza diversa da zero aggiusto il primo importo che trovo nella contropartita
436200050406      *
436300050406     c                   if        diffe <> 0
436400050406      * cerco il primo importo maggiore di zero
436500050406     c                   DO        50            A
436600050406     C                   IF        icpd(a) > 0
436700050406     C                   add       diffe         icpd(a)
436800050406     c                   z-add     51            a
436900050406     c                   endif
437000050406     c                   enddo
437100050406      *
437200050406     c                   clear                   diffe
437300050406      *
437400050406     c                   endif
437500050406     c                   endif
437600050406
437700050406      *
437800050406     c                   endsr
437900050406      *
438000050330      ***********************************************************
438100050330      ** Memorizzo su WFASC00F le bolle con Ass. e Ass. per conto
438200050330      ***********************************************************
438300050620     c***!!!***    Memass        begsr
438400050330
438500050330      * valorizzo i campi del file di lavoro
438600050330
438700050620     c***!!!***          clear                   wfasc000
438800050330
438900050620     c***!!!***          z-add     taslnp        asclnp
439000050620     c***!!!***          z-add     tasnrs        ascnrs
439100050620     c***!!!***          z-add     tasnsp        ascnsp
439200050620     c***!!!***          movel     tastbl        asctbl
439300050620     c***!!!***          z-add     tasksc        ascksc
439400050620     c***!!!***          z-add     tasmgs        ascmgs
439500050620     c***!!!***          z-add     tasaas        ascaas
439600050620     c***!!!***          movel     tasdiv        ascvas
439700050620     c***!!!***          z-add     tasial        ascias
439800050620     c***!!!***          z-add     stapkf        ascpkg
439900050330      * 87 ON --> Cliente codificato ma vario per l'assicurazione
440000050330      *   senza mandato o con mandato inferiore all'importo indicato
440100050330      *   in bolla
440200050620     c***!!!***          If        *in87
440300050620     c***!!!***          move      8888          ascksc
440400050620     c***!!!***          movel     bolra1        ascrsd
440500050620     c***!!!***          movel     bolvia        ascind
440600050620     c***!!!***          movel     bolcap        asccap
440700050620     c***!!!***          movel     bolcit        ascloc
440800050620     c***!!!***          endif
440900050330
441000050620     c***!!!***          z-add     1             a
441100050620     c***!!!***    §$2vrr        lookup    sv(a)                                  05
441200050620     c***!!!***  05              z-add     va(a)         ascvrr
441300050330      * valorizzo la divisa della varia "R"
441400050620     c***!!!***  05              move      tasdiv        ascvvr
441500050330
441600050620     c***!!!***          write     wfasc000
441700050620     c***!!!***          setoff                                       87
441800050330
441900050620     c***!!!***          endsr
442000050406      *********************************************************
442100050406      ** STAMPA ELENCO FATTURE - DISTINTE SPEDIZIONE
442200050406      *********************************************************
442300050406     c     wtrdis        begsr
442400050406
442500050406      ** apertura file di stampa
442600050406     c                   If        not *in50
442700050406     c                   open      disspe
442800050406     c                   z-add     66            condis
442900050406     c                   seton                                        50
443000050406     c                   endif
443100050406      **
443200050406     c                   xfoot     imd           timd
443300050406      ** sommo gli importi totale distinte di spedizione
443400050406     c                   add       timd          tdspd
443500050406      **
443600050406     c                   add       2             condis
443700050406     c                   if        condis > 60
443800050406     c                   except    hdgdis
443900050406     c                   z-add     2             condis
444000050406     c                   endif
444100050406     c                   except    detdis
444200050406      *
444300050406     c                   clear                   numspe
444400050406      *
444500050406     c                   endsr
444600050407      **---------------------------------------------------
444700050407      ** AZZERAMENTO CAMPI COMODO
444800050407      **---------------------------------------------------
444900050407xxxx c     blank         begsr
445000050407
445100050407     c                   clear                   fatbold
445200050407     c                   clear                   totbold
445300050407     c                   clear                   impbold
445400050407     c                   clear                   prtpord
445500050407     c                   clear                   timd
445600050407     c                   clear                   prttpid
445700050407     c                   clear                   fattotcnt
445800050407     c                   clear                   fatbolcnt
445900050407     c                   clear                   dspult
446000050407     c                   clear                   dsppri
446100050407     c                   clear                   fatra1
446200050407     c                   clear                   fatra2
446300050407     c                   clear                   fatcit
446400050407     c                   clear                   fatvia
446500050407     c                   clear                   fatprv
446600050407     c                   clear                   fatbna
446700050407     c                   clear                   fatcap
446800050407     c                   clear                   fatsta
446900050407     c                   clear                   fatnpr
447000050407     c                   clear                   fatdpr
447100050407     c                   clear                   ai
447200050407     c                   clear                   imd
447300050407     c                   clear                   imdw
447400050407     c                   clear                   ivd
447500050407     c                   clear                   v1d
447600050407     c                   clear                   pod
447700050407     c                   clear                   imc
447800050407     c                   clear                   ivc
447900050407     c                   clear                   icpd
448000050407     c                   clear                   icpdw
448100050407     c                   clear                   icpc
448200050407     c                   clear                   ccp
448300050407     c                   clear                   fcp
448400050407     c                   clear                   S
448500050407     c                   clear                   id
448600050407     c                   clear                   s2
448700050407     c                   clear                   i2d
448800050407     c                   clear                   dirfatt
448900060214     c                   clear                   fatdir
449000050407     c                   clear                   CAL_vard
449100050407     c                   clear                   STP_vard
449200050407     c                   clear                   variadd
449300050407     c                   clear                   fatsbl
449400050407     c                   clear                   fattotd
449500050407     c                   clear                   varied
449600050407     c                   clear                   periva
449700050407
449800050408     c                   clear                   pdfi
449900050408     c                   clear                   pvia
450000050408     c                   clear                   pcap
450100050408     c                   clear                   ploc
450200050408     c                   clear                   ptel
450300050408     c                   clear                   pprv
450400050408     c                   clear                   pfax
450500050407     c                   clear                   cit3
450600050407
450700050408     c                   setoff                                       15
450800050408     c                   clear                   wstcod
450900050421     c                   clear                   wstbsc
451000050408
451100050408     c                   clear                   bolra1                         RAG.SOCIALE
451200050408     c                   clear                   bolVIA                         VIA
451300050408     c                   clear                   bolCIT                         CITTA
451400050408     c                   clear                   bolCAP                         CAP
451500050408     c                   clear                   bolPRV                         PROVINCIA
451600050408     c                   clear                   bolSTA                         NAZIONE
451700050408     c                   clear                   bolCAE
451800050408     c                   clear                   stpRMO
451900050423     c                   clear                   bscra1                         RAG.SOCIALE
452000050423     c                   clear                   bscVIA                         VIA
452100050423     c                   clear                   bscCIT                         CITTA
452200050423     c                   clear                   bscCAP                         CAP
452300050423     c                   clear                   bscPRV                         PROVINCIA
452400050423     c                   clear                   bscSTA                         NAZIONE
452500050423     c                   clear                   bscCAE
452600050423
452700050423     c                   clear                   tasbscpre
452800050423     c                   clear                   tasrscpre
452900050423     c                   clear                   tasfrgpre
453000050408
453100050408     c                   clear                   wcalcevdl
453200050620
453300050408      *
453400050408     c                   endsr
453500050407      ********************************************************
453600050407      **  Ricerca in Azorg filiale incasso
453700050407      ********************************************************
453800050407     c     organi        begsr
453900050407
454000050407     c     FILPAG        chain     azorg01l
454100050407     c                   If        %found(azorg01l)
454200050408     c                   movel     orgdes        pdfi                           DESCR.FILIALE
454300050408     c                   movel     orgind        pvia                           VIA
454400050408     c                   movel     orgcpf        pcap                           CAP
454500050408     c                   movel     orgloc        ploc                           LOCALITA
454600050408     c                   movel     orgtel        ptel                           TELEFONO
454700050408     c                   movel     orgpro        pprv                           PROV.
454800050408     c                   movel     orgfax        pfax                           FAX
454900050415     c                   endif
455000050415
455100050407     c                   ENDSR
455200050405      ******************************************************
455300050405      **    Eseguo calcolo  IVA
455400050405      ******************************************************
455500050405     c     caliva        begsr
455600050405
455700050405     c                   z-add     0             impbold
455800050405     c                   z-add     0             aliiva
455900050405
456000050405      ** massimo 5 aliquote IVA
456100050405
456200050405     c                   do        5             a
456300050405     c                   movel     ai(a)         wivads
456400050405     c                   if        fatsci <> *blank
456500050405     c                   move      fatsci        rivrifiva
456600050405     c                   z-add     0             rivaliq
456700050405     c                   movel     §qcriv        wtpreg                         REG.IVA VEND.
456800050405      ** descrizione esenzione IVA
456900050405     c     Kriv          chain     anriv01l                           21
457000050405     c                   If        not  %found(anriv01l)
457100050405     c                   movel(p)  '********'    des(a)
457200050405     c                   else
457300050405     c                   movel(p)  rivdesbrev    des(a)
457400050405     c                   endif
457500050405      * se intestatario della fattura Estero (01 ON) non calcolo imponibile imposta bollo
457600050405     c                   If        not *IN01
457700050622      * verifico se codice di esenzione soggetto all'imposta di bollo oppure no
457800050622     c                   clear                   dsbs02
457900050622     c                   movel     'L'           T02tla
458000050622     c                   movel     'C'           T02mod
458100050622     c                   movel     knsif         t02sif
458200050622     c                   movel     'YBO'         t02cod
458300050622     c                   movel     fatsci        t02ke1
458400050622     c                   call      'TIBS02R'
458500050622     c                   parm                    KPJBA
458600050622     c                   parm                    DSBS02
458700050622      * se esenzione iva non presente in tabella calcolo imponibile per applicazione del bollo
458800050622     c                   if        t02err <> *blanks
458900050405     c                   add       imdw(a)       impbold
459000050622     c                   Endif
459100050622
459200050405     c                   Endif
459300050405      ** se esente IVA   memorizzo imponibile per calcolo bollo
459400050405     c                   else
459500050405     c                   z-add     0             como15
459600050405     c                   move      fatali        aliiva
459700050405      ** calcolo IVA
459800050405     c     imdw(a)       mult      aliiva        como15
459900050405     c                   If        como15 > 0
460000050405     c     como15        div(h)    100           ivd(a)
460100050405     c                   endif
460200050405     c                   endif
460300050405
460400050405     c                   enddo
460500050405
460600050405     c                   z-add     0             aliiva
460700050405
460800050405     c                   endsr
460900050405
461000050413      ********************************************************
461100050413      * ESEGUE IMPOSTAZIONE, RICERCA, MEMORIZZAZIONE
461200050413      * CONTI E IMPORTI DI CONTROPARTITA CONTABILITA' E IVA
461300050413      *  CONRIC:
461400050413      ** RICERCA IL CONTO NELLA DS -DSFC
461500050413      ** RELATIVA AL TIPO VOCE /TIPO SERVIZIO/TIPO BOLLA/IT-EST
461600050413      *  CONMEM:
461700050413      ** MEMORIZZO IL CONTO E IL RELATIVO IMPORTO
461800050413      ********************************************************
461900050413     c     CON001        BEGSR
462000050413     c                   SETOFF                                       08
462100050413      * rimetto l'"esenzione di testata" se cli codificato
462200050413     c                   move      FATSCIt       fatsci
462300050413      * SOLO PER CODIFICATO ESENTE PRENDO FATSCI
462400050413     c                   If        fatsci > *blank
462500050413     c                   move      FATSCI        eseiva
462600050413     c                   z-add     0             aliiva
462700050413     c                   ELSE
462800050413      *
462900050413      * ALTRIMENTI VEDO QUALE IVA APPLICARE
463000050413     c                   z-add     §Qtali        aliIVA            5 2
463100050413     c                   MOVEL     *blank        eseIVA            3
463200050413     c                   ENDIF
463300050413      *
463400050413      * SE NON ESISTE NESSUNA ESENZIONE DELLA SPEDIZIONE
463500050413      * RECUPERO L'ESENZIONE IVA ASSOCIATA ALLA NAZIONE
463600050413     c                   IF        TASFEI = *BLANKS
463700050413     c                   Z-ADD     1             YY
463800050413     c                   SETOFF                                       43
463900050413     c     TASNZD        LOOKUP    NAZE(YY)                               43
464000050413     c   43              MOVEL     ESEN(YY)      TASFEI
464100050413     c                   ENDIF
464200050413      *
464300050413      **
464400050413      * IMPOSTO CONTO IVA DELLA SPEDIZIONE
464500050413     c     TASFEI        IFNE      *BLANKS
464600050413     c                   Z-ADD     1             A
464700050413     c     TASFEI        LOOKUP    CEI(A)                                 05
464800050413     c  N05              DO
464900050413     c                   MOVEL     E(5)          TASMSG
465000050413     c                   SETON                                        08
465100050413     c                   EXSR      errcon
465200050413     c                   SETOFF                                       08
465300050413     c                   END
465400050413     c   05              DO
465500050413     c     A             OCCUR     DSEI
465600050413     c                   z-add     0             aliIVA
465700050413     c                   MOVEL     §EIjei        eseIVA
465800050413     c                   MOVEL     §EIdes        DESESIVA
465900050413     c                   END
466000050413     c                   END
466100050413      ** CONTO IVA DELLA SPEDIZIONE
466200050413      *---------------------------------------------------------
466300050413     c                   MOVE      *BLANKS       KCC                            PORTO
466400050413     c                   MOVE      '1'           KCC
466500050413     c                   Z-ADD     TASPOR        WIMPOR
466600050413     c                   EXSR      CONRIC
466700050413     c                   EXSR      CONMEM
466800050413      *
466900981022     c                   exsr      bucoiva
467000050413      ** B INDICE ALIQUOTA IVA
467100050413     c                   ADD       TASPOR        POD(B)
467200050413     c                   ADD       TASPOR        IMDW(B)
467300050413      *---------------------------------------------------------
467400050413     c                   DO        20            K
467500050413     c     SV(K)         IFNE      *BLANKS
467600050413     c                   MOVEL     'VARIE   '    KCC
467700050413     c                   MOVE      SV(K)         KCC                            VARIA 1
467800050413     c                   Z-ADD     VA(K)         WIMPOR
467900050413     c                   EXSR      CONRIC
468000050413     c                   EXSR      CONMEM
468100050413      *  CTB
468200981022     c                   exsr      bucoiva
468300050413      ** B INDICE ALIQUOTA IVA
468400050415     c                   ADD       VA(K)         V1D(B)
468500050415     c                   ADD       VA(K)         IMDW(B)
468600050413     c                   END
468700050413     c                   END
468800050413      *
468900050413     c                   ENDSR
469000050413      *--------------------------------------------------
469100050413     c     bucoiva       begsr
469200050413      *--------------------------------------------------
469300050413     c     §ccjei        IFNE      *blank
469400050413     c                   MOVEL     §ccjei        fatsci
469500050413     c                   move      *zeros        fatali
469600050413     c                   ELSE
469700050413     c                   move      aliiva        fatali
469800050413     c                   move      eseiva        fatsci
469900050413     c                   END
470000050413      ** CONTO IVA
470100050413     c                   DO        5             B                 3 0
470200050413     c     AI(B)         COMP      WIVAds                                 05
470300050413     c   05              GOTO      EX1
470400050413     c     AI(B)         IFEQ      *blank
470500050413     c                   move      WIVAds        AI(B)
470600050413     c                   GOTO      EX1
470700050413     c                   END
470800050413     c                   END
470900050413     c     EX1           TAG
471000981021     c                   endsr
471100050405      *****************************************************************
471200050405      *    Ricerca conto relativo al tipo importo e spedizione        *
471300050405      *****************************************************************
471400050405     c     Conric        Begsr
471500050405
471600050405     c                   Setoff                                       08
471700050405     c                   z-add     1             a
471800050405     c     kcc           lookup    cc(a)                                  05
471900050405      * se non trovato in tabella CC Errore
472000050405     c                   If        not *in05
472100050405     c                   movel     e(4)          tasmsg
472200050405     c                   seton                                        08
472300050405     c                   exsr      errcon
472400050405     c                   setoff                                       08
472500050405     c                   z-add     1             a
472600050405     c                   endif
472700050405      **
472800050405     c     A             occur     dscc
472900050405     c                   move      §ccflp        wfcp                           FLAG COMB. FIL.
473000050406      **  verifico linea di partenza
473100050406     c                   If        taslnp <> savlnp
473200050406     c     taslnp        chain     azorg01l
473300050406     c                   if        %found(azorg01l)
473400050406     c                   movel     orgpro        cit2
473500050406     c                   movel     orgfl1        lnpfl1
473600050406     c                   movel     orgde3        og143
473700050406     c                   eval      lnpdpd = §ogdpd
473800050406     c                   else
473900050406     c                   clear                   cit2
474000050406     c                   clear                   lnpfl1
474100050406     c                   clear                   lnpdpd
474200050406     c                   endif
474300050406
474400050406     c                   movel     taslnp        savlnp
474500050406     c                   endif
474600050406      ** verifico linea di arrivo
474700050406     c                   If        taslna <> savlna
474800050406     c     taslna        chain     azorg01l
474900050406     c                   if        %found(azorg01l)
475000050406     c                   movel     orgfl1        lnafl1
475100050406     c                   else
475200050406     c                   clear                   lnafl1
475300050406     c                   endif
475400050406
475500050406     c                   movel     taslna        savlna
475600050406     c                   endif
475700050406      **
475800050406     c                   if        lnpfl1 = 'E' or lnafl1 = 'E'
475900050406      * estero
476000050406     c                   move      §ccjce        WKCC
476100050406     c                   move      §ccjse        WKSC
476200050406     c                   move      §ccjve        Wvoc
476300050406
476400050406     c                   else
476500050406      * italia
476600050406     c                   move      §ccjci        WKCC
476700050406     c                   move      §ccjsi        WKSC
476800050406     c                   move      §ccjvi        Wvoc
476900050406     c                   endif
477000050406      *
477100050406     c                   endsr
477200050406
477300050406      *****************************************************************
477400050406      *    memorizza importi nella schiera dei conti
477500050406      *****************************************************************
477600050406     c     conmem        begsr
477700050406
477800050406     c                   movel     dsconti       wconto
477900050406
478000050406     c                   z-add     1             a
478100050406     c     wconto        lookup    ccp(a)                                 05
478200050406     c  n05              z-add     1             a
478300050406     c  n05*blank        lookup    ccp(A)                                 05
478400050406     c                   movel     wconto        ccp(a)
478500050406     c                   add       wimpor        icpdw(a)
478600050406     c                   z-add     wfcp          fcp(a)
478700050406
478800050406     c                   endsr
478900050413      ********************************************************
479000050413      *   CONTROLLO protocollo dichiarazione d'intento
479100050413      ********************************************************
479200050413     c     protocollo    begsr
479300050413      *
479400000217     c                   clear                   wrkdff
479500981021      * testa data fine se piena e valida
479600981026     c                   if        *in20
479700981026     c     *eur          test(d)                 viddff                 86
479800981026     c                   if        *in86
479900981026     c     *dmy          test(d)                 viddff                 86
480000981026     c     *dmy          move      viddff        dataeur
480100981026     c                   else
480200981026     c                   move      viddff        dataeur
480300981026     c                   endif
480400981026     c                   else
480500981026     c     *eur          test(d)                 viddft                 86
480600981021     c                   if        *in86
480700981026     c     *dmy          test(d)                 viddft                 86
480800981026     c     *dmy          move      viddft        dataeur
480900981021     c                   else
481000981026     c                   move      viddft        dataeur
481100981021     c                   endif
481200981026     c                   endif
481300981021      * data corretta controllo andic
481400981021     c                   if        not *in86
481500000217     c                   move      dataeur       wrkdff
481600981021     c     kandic        setll     andic01l
481700981021     c                   do        *hival
481800981021     c     kandic        reade     andic01l                               88
481900991229     c   88              leave
482000000217     c                   if        wrkdff  >= dicdtindic and
482100981021     c                             dicdtrevoc > *loval  and
482200000217     c                             wrkdff  <= dicdtrevoc or
482300000217     c                             wrkdff  >= dicdtindic and
482400981021     c                             dicdtrevoc = *loval  and
482500000217     c                             wrkdff  <= dicdtfidic
482600981021     c                   z-add     dicnrcli      fatnpr
482700981021     c                   move      dicdtcli      dataiso
482800981021     c                   move      dataiso       dataeur
482900981021     c                   move      dataeur       fatdpr            8 0
483000981021      *forzata fattura fine mese
483100981021     C                   MOVEL     'F'           FATIFM
483200981021     c                   move      diccdiva      fatsci
483300990218     c                   move      diccdiva      fatscit
483400981021     c                   clear                   fatali
483500981021     c                   endif
483600991229     c                   enddo
483700981021     c                   endif
483800981021      *
483900981021     C                   endsr
484000050413      *----------------------------------------------------*
484100050413      * Reperisco numeratore
484200050413      *----------------------------------------------------*
484300050413     c     repnum        begsr
484400050413     c                   MOVE      wsocieta      XNMSOC            3
484500050413     c                   MOVE      *BLANKS       XNMUNI            8
484600050413     c                   MOVE      'CG'          XNMCTB            2
484700050413     c                   MOVE      *BLANKS       XNMKEY            8
484800050413     c                   MOVE      WSERIE        XNMSER            4
484900050413     c                   MOVE      *OFF          XNMCMT            1
485000050413     c                   MOVE      *On           XNMIVA            1
485100050413     c                   MOVE      *OFF          XNMREG            1
485200050413     c                   MOVE      '0'           XNMerr            1
485300050413      *
485400050413     c                   CALLB     'XNMR'        PNMR
485500050413      *
485600050413     c     xnmerr        ifne      '0'
485700050413     c                   seton                                        h5
485800050413     c                   end
485900981023      *
486000050413     c                   endsr
486100050413      *----------------------------------------------------*
486200050413      * Reperimento dati società
486300050413      *----------------------------------------------------*
486400050413     c     REPSOC        BEGSR
486500050413      *
486600050413     c                   CALL      'XSOC'
486700050413     c                   PARM                    TIPXSC            6
486800050413     c                   PARM      wsocieta      SOCXSC            3
486900050413     c                   PARM                    CDSXSC            9 0
487000050413     c                   PARM                    MODXSC            3
487100050413     c                   PARM      *blanks       RTNXSC            1
487200050413     c                   PARM                    XSOCDS
487300050413     c                   PARM                    KPJBA
487400050413      *
487500050413     c                   ENDSR
487600050413      *****************************************************************
487700050413     c     *INZSR        BEGSR
487800050413      *****************************************************************
487900050413     c     *ENTRY        PLIST
488000050413     c                   PARM                    KPJBA
488100050413     c                   MOVEL     *ZEROS        PARAM
488200050413     c                   MOVEL     KPJBU         PARAM
488300050413      *
488400050413     c                   Z-ADD     1             CODUT
488500050413     c                   CALL      'X§PARUT'
488600050413     c                   PARM                    UTEDSE
488700050413      * RICERCA CAPOCONTI
488800050413     c                   DO        50            X                 2 0
488900050413     c                   MOVE      TCU(X)        TCUDS
489000050413     c     F56           CABNE     'CG'          END1
489100050413     c     F4            COMP      '1'                                    21
489200050413     c     F4            COMP      '2'                                    22
489300050413     c     F4            COMP      '3'                                    23
489400050413     c     F4            COMP      '6'                                    27
489500050413      ** 1 CLIENTI   21
489600050413      ** 2 FORNITORI 22
489700050413      ** 3 AGENTI    23
489800050413     c     F3            COMP      '0'                                242425
489900050413     c     F3            COMP      'I'                                    26
490000050413      ** 0 ITALIA   25
490100050413      ** 1 ESTERO   24
490200050413      *  I CAPO CONTO IVA
490300050413     c   21
490400050413     cAN 24              Z-ADD     KCU(X)        KCE               4 0
490500050413     c   21
490600050413     cAN 25              Z-ADD     KCU(X)        KCI               4 0
490700050413     c   22
490800050413     cAN 24              Z-ADD     KCU(X)        KFE               4 0
490900050413     c   22
491000050413     cAN 25              Z-ADD     KCU(X)        KFI               4 0
491100050413     c   23
491200050413     cAN 24              Z-ADD     KCU(X)        KAE               4 0
491300050413     c   23
491400050413     cAN 25              Z-ADD     KCU(X)        KAI               4 0
491500050413     c   26              Z-ADD     KCU(X)        KIVA              4 0
491600050413     c   27              Z-ADD     KCU(X)        KBNA              4 0
491700050413     c     END1          TAG
491800050413     c                   END
491900990715     c                   setoff                                       212223
492000990715     c                   setoff                                       242526
492100990715     c                   setoff                                       27
492200981023      *parametri per richiamo numeratori
492300050413     c     PNMR          PLIST
492400050413     c                   PARM                    XNMRIC            1
492500050413     c                   PARM                    XNMSOC
492600050413     c                   PARM                    XNMUNI
492700050413     c                   PARM                    XNMCTB
492800050413     c                   PARM                    XNMKEY
492900050413     c                   PARM                    XNMSER
493000050413     c                   PARM                    XNMCMT            1
493100050413     c                   PARM                    XNMDAT
493200050413     c                   PARM                    XNMIVA            1
493300050413     c                   PARM                    XNMREG            1
493400050413     c                   PARM                    XNMERR            1
493500050413     c                   PARM                    XNMNUM            9 0
493600050413     c                   PARM                    XNMAUT            1
493700050413     c                   PARM                    XNMDAG                         opzionale
493800050413      *---------- RICERCA DITTA :
493900050413     c                   MOVEL     'SOC001'      TIPXSC
494000050413     c                   MOVEL     *BLANK        SOCXSC
494100050413     c                   EXSR      REPSOC
494200050413     c     RTNXSC        IFNE      '1'
494300050413     c                   MOVEL     XSOCDS        SOC001
494400050413     c                   MOVEL     xscrgs        RSUT             20
494500050413     c                   end
494600050413     c                   movel     wsocieta      WWWSoc
494700050413     c                   movel     'C     '      $kcc
494800050413     c                   movel     *blanks       $ksc
494900050413     c                   callb     'XCAPCLIFOR'
495000050413     c                   parm                    WWWSoc
495100050413     c                   parm                    $kcc
495200050413     c                   parm                    $ksc
495300050413     c                   Z-ADD     1             CODUT
495400050413      * IMPOSTAZIONE CAMPI
495500050413     c                   CLEAR                   SAVLNP
495600050413     c                   CLEAR                   SAVLNA
495700050413     C                   move      *zero         FATKCC
495800050413     C                   move      *zero         FATKSC
495900050413     C                   move      *zero         FATSC1
496000050413     C                   move      *blank        FATVIA
496100050413     C                   move      *blank        FATcit
496200050413     C                   move      *zero         FATcap
496300050413     C                   move      *blank        FATprv
496400050413     C                   move      *blank        FATbna
496500050413     C                   move      *blank        FATsbl
496600050413      *  KLIST PER ACCESSO FILES
496700050413     c     KTA7          KLIST
496800050413     c                   KFLD                    TASAAS
496900050413     c                   KFLD                    TASLNP
497000050413     c                   KFLD                    TASNRS
497100050413     c                   KFLD                    TASNSP
497200050413     c                   KFLD                    TASTBL
497300050413      *  file di lavoro wfasc01l
497400050411     c     Kasc          klist
497500050411     c                   kfld                    tasaas
497600050411     c                   kfld                    taslnp
497700050411     c                   kfld                    tasnrs
497800050411     c                   kfld                    tasnsp
497900050413      *
498000050413     c     KTAA          KLIST
498100050413     c                   KFLD                    TASAAS
498200050413     c                   KFLD                    TASLNP
498300050413     c                   KFLD                    TASNRS
498400050413     c                   KFLD                    TASNSP
498500050413     c                   KFLD                    §trc
498600050413      *
498700050413     c     KTA4          KLIST
498800050413     c                   KFLD                    TASAAS
498900050413     c                   KFLD                    TASLNP
499000050413     c                   KFLD                    TASNRS
499100050413     c                   KFLD                    TASNSP
499200050413     c                   KFLD                    §4trc
499300050929      * Chiave file eventi FNEVB
499400060601     c***  Kevb4         Klist
499500060601     c***                Kfld                    Tasaas
499600060601     c***                Kfld                    Taslnp
499700060601     c***                Kfld                    Tasnrs
499800060601     c***                Kfld                    Tasnsp
499900060601     c***                Kfld                    Wcev
500000050929      *
500100050413     c     KCSB          KLIST
500200050413     c                   KFLD                    TASAAS
500300050413     c                   KFLD                    TASLNP
500400050413     c                   KFLD                    TASNRS
500500050413     c                   KFLD                    TASNSP
500600941108     C** RICERCA NUMERI FATTURA
500700050411      ** Accesso TITAS per stampa e contabilizzazione
500800050429     c     Ktas30        klist
500900050411     c                   kfld                    wf_tasAAS
501000050411     c                   kfld                    wf_tasLNP
501100050411     c                   kfld                    wf_tasNRS
501200050411     c                   kfld                    wf_tasNSP
501300050429     c                   kfld                    wf_tasTBL
501400050413      ** ACCESSO TABEL
501500050413     c     KTABC         KLIST
501600050413     c                   KFLD                    CODUT
501700050413     c                   KFLD                    COD
501800050413     c                   KFLD                    KEY
501900050331
502000050331     C     KTAB          KLIST
502100050331     C                   KFLD                    CODUT
502200050331     C                   KFLD                    COD
502300981021     C     Keycli        klist
502400981021     C                   kfld                    wsocieta
502500981021     C                   kfld                    ktasksc
502600981021     C                   kfld                    k£fil
502700981021     C                   kfld                    k£lin
502800050420     C     Keybol        klist
502900050420     C                   kfld                    wsocieta
503000050421     C                   kfld                    ktasbsc
503100050420     C                   kfld                    k£fil
503200050420     C                   kfld                    k£lin
503300981021     C     Keyfat        klist
503400981021     C                   kfld                    wsocieta
503500981023     C                   kfld                    clnclifatt
503600981021     C                   kfld                    k£fil
503700981021     C                   kfld                    k£lin
503800981021     C     Keyind        KLIST
503900981021     C                   KFLD                    sogsogg
504000981021     C                   KFLD                    tpin
504100981021     C                   KFLD                    cdin
504200981021     c                   move      *blank        tpin
504300981021     c                   move      *blank        cdin
504400981021     C     Keybac        klist
504500981021     C                   kfld                    wsocieta
504600981021     C                   kfld                    clnkcc
504700981021     C                   kfld                    clnksc
504800050509     C     Keyclp        klist
504900050509     C                   kfld                    codut
505000050509     C                   kfld                    clpkcc
505100050509     C                   kfld                    clpksc
505200981021     C     Kcba          klist
505300981021     C                   kfld                    wsocieta
505400981021     C                   kfld                    wcbacli           1
505500981021     C                   kfld                    wccblank          6
505600981021     c                   kfld                    clnksc
505700981021     c                   kfld                    entita            8
505800981021     c                   kfld                    wappog            2
505900981021     c                   move      *blank        entita
506000981021     c                   move      *blank        wccblank
506100981021     c                   move      *blank        wappog
506200981021     c                   move      'C'           wcbacli
506300981021     C     Kandic        klist
506400981021     C                   kfld                    wsocieta
506500981021     C                   kfld                    wcbacli
506600981021     C                   kfld                    clnksc
506700981021     C     Keycdp        klist
506800981021     C                   kfld                    wsocieta
506900981021     C                   kfld                    fatcdp
507000981022     C     Kriv          klist
507100981022     C                   kfld                    wsocieta
507200981022     C                   kfld                    wtpreg            1
507300981022     C                   kfld                    rivaliq
507400981022     C                   kfld                    rivrifiva
507500030204
507600030204     c     kFiar5        Klist
507700030204     c                   Kfld                    TasAas
507800030204     c                   Kfld                    TasLnp
507900030204     c                   Kfld                    TasNrs
508000030204     c                   Kfld                    TasNsp
508100030204     c                   Kfld                    kAr5Trd
508200050413      ** ACCESSO ANAGRAFICHE CLIENTI
508300050331     C     KSPE          KLIST
508400050331     C                   KFLD                    SPWFLS
508500050331     C                   KFLD                    wCLPKSC           7 0
508600050331     C                   KFLD                    COMLSF            3
508700051110      * accesso al file luoghi con indirizzo mail
508800051110     c     Ksp2          Klist
508900051110     c                   kfld                    specli
509000051110     c                   kfld                    specod
509100051110     c                   kfld                    comtpe
509200051118      *  file note tfntc01l
509300051118     C     Kntc          KLIST
509400051118     C                   KFLD                    Kapl
509500051118     C                   KFLD                    Knk1
509600051118     C                   KFLD                    Knk2
509700051118     C                   KFLD                    Ktnt
509800060502      *  file immagine bolle tassate
509900060502     c     Ktai          Klist
510000060502     c                   Kfld                    wf_TASaas
510100060502     c                   Kfld                    wf_TASlnp
510200060502     c                   Kfld                    wf_TASnrs
510300060502     c                   Kfld                    wf_TASnsp
510400060502     c                   Kfld                    wf_TAStbl
510500060502     c                   Kfld                    taisvt
510600051118      *
510700050413      *----------------------------------------------------
510800941108     C                   MOVE      KNSIF         COMOD             4
510900941108     C                   MOVEL     COMOD         KCDSI             3
511000941108     C                   Z-ADD     66            CR                2 0
511100050413      **
511200050413      ** CARICAMENTO TABELLE
511300941108     C                   EXSR      CARTAB
511400050413      ** RIBALTA DATE DA FILTRO
511500941108     C                   EXSR      RIBDAT
511600050413     C** CARICAMENTO NUMERI DI FATTURA
511700050413      *  SOLO PER LE SPEDIZ.DI RECUPERO E' TRA DATA G. - 3  E  + §3IGGS
511800941108     C                   EXSR      CARNFT
511900050413
512000050413     c                   TIME                    W0140            14 0
512100050413      * UDATE IN GGMMAAAA
512200980105     C                   MOVE      W0140         WDTGIO            8 0
512300050413      *
512400050413      * UDATE IN AAAAMMGG
512500981023     C                   move      WDTGIO        dataeur
512600981023     C                   MOVE      dataeur       dataiso
512700981023     C                   MOVE      dataiso       DATEU             8 0
512800050329      ** Carica limiti di fatturazione per divisa di fatturazione (EUR)
512900050329     c                   exsr      cardiv
513000050421      * apro i file di stampa
513100060228     c                   open      prtf198
513200050421     c                   open      qsysprt
513300050421     c                   open      sysprt
513400060227      *
513500060227
513600060227      * Se sono in ambiente buono - GAITRAAZM
513700060227     c                   If        knsif = 'GAITRA201 '
513800061012     c                   Eval      wlibc= 'GAITRAAZM'
513900060227      *
514000060227     c                   Else
514100060227     c                   Eval      wlibc= 'GAITRAFIL'
514200060227     c                   EndIf
514300060227      * file di lavoro wfmtc se sono in filiale duplico in QTEMP
514400061011     c                   clear                   comman
514500061011     c                   movel(p)  cmdc(1)       comman
514600061012     c                   Eval      %Subst(comman:15:9) = wlibc
514700061011     c                   call      'QCMDEXC'
514800061011     c                   parm                    comman
514900061011     c                   parm                    lenght
515000061011
515100060227     c                   clear                   comman
515200061011     c                   movel(p)  cmdc(2)       comman
515300061012     c                   Eval      %Subst(comman:34:9) = wlibc
515400060227     c                   call      'QCMDEXC'
515500060227     c                   parm                    comman
515600060227     c                   parm                    lenght
515700061011
515800061011     c                   clear                   comman
515900061011     c                   movel(p)  cmdc(3)       comman
516000061011     c                   call      'QCMDEXC'
516100061011     c                   parm                    comman
516200061011     c                   parm                    lenght
516300061011
516400060228     c                   open      wfmtc00f
516500050413      **
516600941108     C                   ENDSR
516700050413      **----------------------------------------------------
516800050413      ** AGGIORNAMENTO FILE CON ULTIMO NUMERO/DATA FATTURA *
516900050413      **----------------------------------------------------
517000050413     c     AGGNUM        BEGSR
517100981027     c                   move      '5'           xnmric
517200050413      ** FATTURA INIZIO MESE
517300050413      **  SIA PER FATTURAZ. MENSILE CHE NON
517400050413     c                   MOVEl     §QCRIV        wserie                         REG.IVA VEND.
517500050413     c                   MOVE      §QCFIV        wserie                         LIBRO IVA VEND.
517600050413     c     NFTIM         IFNE      wnftim
517700981026     c   20              move      dtdfi8        xnmdat
517800981026     c  n20              move      dtdft8        xnmdat
517900981027     c   20              move      dtdfi8        xnmdag
518000981027     c  n20              move      dtdft8        xnmdag
518100981023     c                   z-add     nftim         xnmnum
518200981023     c                   exsr      repnum
518300050413     c                   ENDif
518400050413      **
518500050413      ** FATTURA FINE MESE
518600981023     C     *IN20         IFEQ      *ON
518700981026     c                   move      dtdff8        xnmdat
518800981027     c                   move      dtdff8        xnmdag
518900050413     c                   MOVE      §QCFI2        wserie                         LIBRO IVA VEND.
519000050413     c     NFTFM         IFNE      wnftfm
519100050413     c                   Z-ADD     NFTFM         xnmnum
519200050413     c                   exsr      repnum
519300050413     c                   END
519400050413     c                   END
519500050413     c                   ENDSR
519600941108     C*****************************************************
519700941108     C** CARICAMENTO TABELLE
519800941108     C*****************************************************
519900050413     c     CARTAB        BEGSR
520000050413      ** CARICAMENTO TIPI TARIFFA
520100941113     C                   EXSR      CARTR
520200050413      ** CARICAMENTO SIGLE VARIE
520300941108     C                   EXSR      CAR$2
520400050413      ** CARICAMENTO DATI VARI TASSAZIONE
520500941108     C                   EXSR      CARQT
520600050413      ** CARICAMENTO TIPO BOLLA
520700941108     C                   EXSR      CARTB
520800050413      ** ESENZIONI IVA
520900941108     C                   EXSR      CAREI
521000050413      ** CODICI TASSAZIONE
521100941108     C                   EXSR      CARCT
521200050413      ** CONTI CONTABILITA
521300941108     C                   EXSR      CARCC
521400050413      ** TABELLA DATI VARI CONTABILITA'
521500941108     C                   EXSR      CARQC
521600050413      ** CARICA TIPI SERVIZIO
521700941113     C                   EXSR      CAR5E
521800050413      ** CARICA CONTI CONTAB. ARROTONDAMENTI CAMBI
521900050413     C                   EXSR      CARARR
522000050413      ** CARICA NAZIONI E LE SUE ESENZIONI IVA
522100050413     C                   EXSR      CARNAZ
522200050413      ** CARICo codici bolla che prevedono solo una varia
522300020416     C                   EXSR      CARcbo
522400050413      ** CARICO VALORI DI TASXCO DA ESCLUDERE DALLA FATTURAZIONE SE IMPONIBILE A ZERO
522500050218     C                   EXSR      CARVUS
522600050413
522700941108     C                   ENDSR
522800050413      ******************************************************
522900050413      ** CARICAMENTO TIPI TARIFFA
523000050413      ******************************************************
523100050413     c     CARTR         BEGSR
523200050413
523300050413     c                   DO        50            A
523400050413     c     A             OCCUR     DSTR
523500050413     c                   CLEAR                   DSTR
523600050413     c                   END
523700050413      **
523800050413     c                   MOVEL     'TR'          COD
523900050413     c                   Z-ADD     0             K                 5 0
524000050413     c     KTAB          SETLL     TABEL                                  07
524100050413     c  N07              SETON                                          H7
524200050413     c   H7              GOTO      FINE
524300050413     c                   DO        *HIVAL        A
524400050413     c     KTAB          READE     TABEL                                  07
524500050413     c   07              GOTO      TAB1
524600050413     c     TBLKEY        IFNE      *BLANKS
524700050413     c                   ADD       1             K
524800050413     c                   MOVEL     TBLKEY        CTR(K)
524900050413     c     K             OCCUR     DSTR
525000050413     c                   MOVEL     TBLUNI        DSTR
525100050413     c                   END
525200050413     c                   END
525300050413
525400050413     c     TAB1          ENDSR
525500941113     C**********************************************************
525600980224     C** CARICAMENTO TIPI SERVIZIO DA STAMPARE
525700980224     C*  QUELLI CHE HANNO UNA TARIFFA PARTICOLARE COLLEGATA
525800941113     C**********************************************************
525900050413     c     CAR5E         BEGSR
526000050413      ** PULIZIA DS
526100050413     c                   MOVEL     '5E'          COD
526200050413     c                   Z-ADD     0             K
526300050413     c     KTAB          SETLL     TABEL                                  07
526400050413     c     KTAB          READE     TABEL                                  07
526500050413    1c     *IN07         DOWEQ     *OFF
526600050413    2c     TBLKEY        IFNE      *BLANKS
526700050413     c                   MOVEL     TBLUNI        DS5E
526800050413    3c     §5EFTC        IFNE      ' '
526900050413     c                   ADD       1             K
527000050413     c                   MOVEL     TBLKEY        TS(K)
527100050413    3c                   ENDIF
527200050413    2c                   ENDIF
527300050413     c     KTAB          READE     TABEL                                  07
527400050413    1c                   ENDDO
527500050413     c                   ENDSR
527600020416     C**********************************************************
527700020416     C** CARICAMENTO codici bolla che prevedono di tassare solo 1 bolla
527800020416     C*   e che permettono imponibile =0
527900020416     C**********************************************************
528000050413     c     CARCBO        BEGSR
528100050413      ** PULIZIA DS
528200020416     C                   MOVEL     '3A'          COD
528300020416     C                   Z-ADD     0             K
528400020416     C                   Z-ADD     0             KK                3 0
528500020416     C     KTAB          SETLL     TABEL                                  07
528600020416     C     KTAB          READE     TABEL                                  07
528700020416    1C     *IN07         DOWEQ     *OFF
528800020416    2C     TBLKEY        IFNE      *BLANKS
528900020416     C                   MOVEL     TBLUNI        DS3a
529000020416    3C     §3aim0        IFeq      'S'
529100020416     C                   ADD       1             K
529200020416     C                   MOVEL     TBLKEY        im0(k)
529300020416    3C                   ENDIF
529400020416     c     §3asva        ifne      *blanks
529500020416     C                   ADD       1             Kk
529600020416     C                   MOVEL     TBLKEY        cbo(kk)
529700020416     C                   MOVEL     §3asva        cbv(kk)
529800020416    3c                   endif
529900050413      **
530000020416    2C                   ENDIF
530100020416     C     KTAB          READE     TABEL                                  07
530200020416    1C                   ENDDO
530300020416     C                   ENDSR
530400050413      ********************************************************
530500050413      ** CARICAMENTO SIGLE VARIE
530600050413      ********************************************************
530700050413     c     CAR$2         BEGSR
530800050413     c                   CLEAR                   DS$2
530900050413     c                   MOVEL     '$2'          COD
531000050413     c                   MOVEL     *BLANKS       KEY
531100050413     c                   MOVEL     '1'           KEY
531200050413     c     Ktabc         CHAIN     TABEL                              07
531300050413     c   07              SETON                                          H7
531400050413     c   H7              GOTO      FINE
531500050413     c                   MOVEL     TBLUNI        DS$2
531600050413     c                   ENDSR
531700941108     C****************************************************
531800941108     C** CARICAMENTO DATI VARI CONTABILITA'
531900941108     C****************************************************
532000050413     c     CARQC         BEGSR
532100050413     c                   CLEAR                   DSQC1
532200050413     c                   CLEAR                   DSQC2
532300050413     c                   MOVEL     'QC'          COD
532400050413     c                   MOVEL     *BLANKS       KEY
532500050413     c                   MOVEL     '2'           KEY
532600050413     c     KTABC         CHAIN     TABEL                              07
532700050413     c   07              SETON                                        H7
532800050413     c   H7              GOTO      FINE
532900050413     c                   MOVEL     TBLUNI        DSQC2
533000050413     c                   MOVEL     *BLANKS       KEY
533100050413     c                   MOVEL     '1'           KEY
533200050413     c     KTABC         CHAIN     TABEL                              07
533300050413     c   07              SETON                                        H7
533400050413     c   H7              GOTO      FINE
533500050413     c                   MOVEL     TBLUNI        DSQC1
533600050413      **
533700050413     c                   ENDSR
533800050413      ****************************************************
533900050413      ** CARICAMENTO DATI VARI TASSAZIONE
534000050413      ****************************************************
534100050413     c     CARQT         BEGSR
534200050413     c                   CLEAR                   DSQT1
534300050413     c                   MOVEL     'QT'          COD
534400050413     c                   MOVEL     *BLANKS       KEY
534500050413     c                   MOVEL     '1'           KEY
534600050413     c     KTABC         CHAIN     TABEL                              07
534700050413     c   07              SETON                                        H7
534800050413     c   H7              GOTO      FINE
534900050413     c                   MOVEL     TBLUNI        DSQT1
535000050413     c                   ENDSR
535100050329      ****************************************************
535200050329      ** Caricamento importi standard moneta
535300050329      ****************************************************
535400050329     c     Cardiv        Begsr
535500050329      *
535600050329     c                   Clear                   Dsbs02
535700050329
535800050329     c                   Movel     'L'           t02tla
535900050329     c                   Movel     'C'           t02mod
536000050329     c                   Movel     Knsif         t02sif
536100050329     c                   Movel     'GEI'         t02cod
536200050329     c                   Movel     Viddiv        t02ke1
536300050329
536400050329     c                   call      'TIBS02R'
536500050329     c                   parm                    kpjba
536600050415     c                   parm                    dsbs02
536700050329
536800050418     c                   movel     t02uni        dgei
536900050418
537000050329     c                   endsr
537100001024     C*******************************************************************
537200001024     C** CARICO TABELLA DELLE NAZIONI E LE RELATIVE ESENZIONI IVA
537300001024     C*******************************************************************
537400050413     c     CARNAZ        BEGSR
537500050413      * PULISCO LE SCHIERE
537600001024     C                   CLEAR                   NAZE
537700001024     C                   CLEAR                   ESEN
537800001024     C**
537900001024     C                   MOVEL     '15'          COD
538000001024     C                   Z-ADD     0             YY
538100001024     C     KTAB          SETLL     TABEL                                  07
538200001024     C  N07              SETON                                          H7
538300001024     C   H7              GOTO      FINE
538400001024     C                   DO        *HIVAL
538500001024     C     KTAB          READE     TABEL                                  07
538600001024     C   07              LEAVE
538700001024     C                   ADD       1             YY
538800001024     C                   MOVEL     TBLKEY        NAZE(YY)
538900001024     C                   MOVEL     TBLUNI        DS15
539000001024     c                   movel     §15CEI        ESEN(YY)
539100001024     C*
539200001024     C                   ENDDO
539300050413      *
539400001024     C                   ENDSR
539500050218     C**********************************************************
539600050218     C** CARICAMENTO VALORI DI TASXCO DA ESCLUDERE DALLA FATTURAZIONE
539700050218     C*  SE IMPONIBILE UGUALE A ZERO
539800050218     C**********************************************************
539900050218     C     CARVUS        BEGSR
540000050413      ** PULIZIA DS
540100050218     C                   MOVEL     'VUS'         KTBE
540200050218     C                   Z-ADD     0             K
540300050218     C     KTBE          SETLL     TNTBE01L
540400050218     C                   DO        *HIVAL
540500050218     C     KTBE          READE     TNTBE01L
540600050218    1C                   IF        %EOF(TNTBE01L)
540700050218     C                   LEAVE
540800050218     C                   ENDIF
540900050218
541000050218     C                   IF        TBEATB <> ' '
541100050218     C                   ITER
541200050218     C                   ENDIF
541300050218
541400050218     C                   MOVEL     TBEUNI        DVUS
541500050218      * SE IL FLAG DI ESCLUSIONE DA FATTURAZIONE È = S CARICO LA SCHIERA
541600050218    3C                   IF        §VUEFT = 'S'
541700050218     C                   ADD       1             K
541800050218     C                   MOVEL     TBEKE1        VUS(K)
541900050218    3C                   ENDIF
542000050218    2C                   ENDDO
542100050218     C                   ENDSR
542200000104     C****************************************************
542300000104     C** CONVERSIONE IMPORTI IN UNA MONETA DIVERSA
542400000104     C****************************************************
542500000104     C     CNVEUR        BEGSR
542600000107      * conversione in moneta di conto e non
542700000107     C                   Movel     div2          DivMC0202
542800000107     C                   Movel     div1          Divisa0202
542900000107     C                   Move      *Loval        DtRif0202
543000000107     C                   Move      *On           NoCtrScos
543100030701     C                   if        fatdft > 0
543200000107     C                   move      fatdft        dtrif0202
543300030701     C                   endif
543400000107     c**
543500000107     C                   CallP     X0202TestC    (PJX00202DS       :
543600000107     C                                            %Size(PJX00202DS):
543700000107     C                                            *Off             :
543800000107     C                                            EsitoA           :
543900000107     C                                            NoCtrScos        :
544000000107     C                                            *Omit            :
544100000107     C                                            %Size(ANDVS00F)  :
544200000107     C                                            %Size(ANDVX00F)  :
544300000107     C                                            ANDVS00F         :
544400000107     C                                            ANDVX00F          )
544500000107     C                   endsr
544600000104      *
544700000111     C****************************************************
544800000111     C** CARICAMENTO TABELLA ARR
544900000111     C****************************************************
545000050413     c     CARARR        BEGSR
545100050413      *  CARICO DS ARROTONDAMENTI ATTIVI
545200000111     C                   CLEAR                   DSBS02
545300000111     C                   CLEAR                   DARR
545400000111      *
545500000111     C                   MOVEL     'L'           T02TLA
545600000111     C                   MOVEL     'C'           T02MOD
545700000111     C                   MOVEL     KNSIF         T02SIF
545800000111     C                   MOVEL     'ARR'         T02COD
545900000111     C                   MOVEL     'ATTIVO '     T02KE1
546000000111      *
546100000111     C                   CALL      'TIBS02R'
546200000111     C                   PARM                    KPJBA
546300000111     C                   PARM                    DSBS02
546400000111      *
546500000111     C     T02ERR        IFEQ      *BLANKS
546600000111     C     1             OCCUR     DARR
546700000111     C                   MOVEL     T02UNI        DARR
546800000111     C                   ENDIF
546900000111     C*  CARICO DS ARROTONDAMENTI ATTIVI
547000000111     C                   CLEAR                   DSBS02
547100000111      *
547200000111     C                   MOVEL     'L'           T02TLA
547300000111     C                   MOVEL     'C'           T02MOD
547400000111     C                   MOVEL     KNSIF         T02SIF
547500000111     C                   MOVEL     'ARR'         T02COD
547600000111     C                   MOVEL     'PASSIVO'     T02KE1
547700000111      *
547800000111     C                   CALL      'TIBS02R'
547900000111     C                   PARM                    KPJBA
548000000111     C                   PARM                    DSBS02
548100000111      *
548200000111     C     T02ERR        IFEQ      *BLANKS
548300000111     C     2             OCCUR     DARR
548400000111     C                   MOVEL     T02UNI        DARR
548500000111     C                   ENDIF
548600000111      *
548700000111     C                   ENDSR
548800941108     C******************************************************
548900941108     C** RIBALTAMENTO DATE DA FILTRO
549000941108     C******************************************************
549100050413     c     RIBDAT        BEGSR
549200050413      ** DATA FATTURA AMG INIZIO MESE
549300981023     C                   move      VIDDFI        dataeur
549400981023     C                   move      dataeur       dataiso
549500981023     C                   MOVE      dataiso       DTDFI8            8 0
549600050413      **DATA DECORRENZA AMG FINE MESE
549700981023     C                   move      VIDDFF        dataeur
549800981023     C                   move      dataeur       dataiso
549900981023     C                   MOVE      dataiso       DTDFF8            8 0
550000050413      ** DATA FATTURA AMG PER FATTURAZIONE SETTIMANALE O QUINDICINALE
550100981023     C                   move      VIDDFT        dataeur
550200981023     C                   move      dataeur       dataiso
550300981023     C                   MOVE      dataiso       DTDFT8            8 0
550400050413      **DATA SPEDIZIONE AMG
550500981023     C                   move      VIDDSP        dataeur
550600981023     C                   move      dataeur       dataiso
550700981023     C                   MOVE      dataiso       DTDSP             8 0
550800050413      * VEDO SE SIT RATTA DI FATTURAZIONE MENSILE O MENO
550900971023     C     VIDTFT        IFEQ      4
551000971023     C                   SETON                                        20
551100971023     C                   ELSE
551200971023     C                   SETOFF                                       20
551300971023     C                   ENDIF
551400941108     C                   ENDSR
551500941108     C*********************************************************
551600941108     C** CARICAMENTO DS MULTIPLA  CONTI CONTABILITA
551700941108     C*********************************************************
551800050413     c     CARCC         BEGSR
551900050413     c                   DO        50            A
552000050413     c     A             OCCUR     DSCC
552100050413     c                   CLEAR                   DSCC
552200050413     c                   END
552300050413      ** PULIZIA DS
552400050413     c                   MOVEL     'CC'          COD
552500050413      ** ACCESSO TABEL X CODICE
552600050413     c     KTAB          SETLL     TABEL                                  07
552700050413     c  N07              SETON                                          H7
552800050413     c   H7              GOTO      FINE
552900050413     c                   Z-ADD     0             K
553000050413     c                   DO        *HIVAL        A
553100050413     c     KTAB          READE     TABEL                                  07
553200050413     c   07              GOTO      ENDCC
553300050413     c     TBLKEY        IFNE      *BLANKS
553400050413     c                   ADD       1             K
553500050413     c     K             OCCUR     DSCC
553600050413     c                   MOVEL     TBLKEY        CC(K)
553700050413     c                   MOVEL     TBLUNI        DSCC
553800050413     c                   END
553900050413     c                   END
554000050413     c     ENDCC         TAG
554100050413      ** tabella cb estensione per descrizione  varie fedex
554200050413     c                   DO        50            A
554300050413     c     A             OCCUR     DSCB
554400050413     c                   CLEAR                   DSCB
554500050413     c                   END
554600050413      ** PULIZIA DS
554700050413     c                   MOVEL     'CB'          COD
554800050413      ** ACCESSO TABEL X CODICE
554900050413     c     KTAB          SETLL     TABEL                                  07
555000050413     c  N07              goto      endcb
555100050413     c                   Z-ADD     0             K
555200050413     c                   DO        *HIVAL        A
555300050413     c     KTAB          READE     TABEL                                  07
555400050413     c   07              GOTO      ENDCB
555500050413     c     TBLKEY        IFNE      *BLANKS
555600050413     c                   ADD       1             K
555700050413     c     K             OCCUR     DSCB
555800050413     c                   MOVEL     TBLKEY        CB(K)
555900050413     c                   MOVEL     TBLUNI        DSCB
556000050413     c                   END
556100050413     c                   END
556200050413     c     ENDCB         TAG
556300050413     c                   ENDSR
556400941108     C**********************************************************
556500941108     C** CARICAMENTO DS MULTIPLA  ESENZIONI IVA
556600941108     C**********************************************************
556700941108     C     CAREI         BEGSR
556800050413     c                   DO        100           A
556900050413     c     A             OCCUR     DSEI
557000050413     c                   CLEAR                   DSEI
557100050413     c                   END
557200050413      ** PULIZIA DS
557300050413     c                   MOVEL     'EI'          COD
557400050413     c     KTAB          SETLL     TABEL                                  07
557500050413     c  N07              SETON                                          H7
557600050413     c   H7              GOTO      FINE
557700050413     c                   Z-ADD     0             K
557800050413     c                   DO        *HIVAL        A
557900050413     c     KTAB          READE     TABEL                                  07
558000050413     c   07              GOTO      ENDEI
558100050413     c     TBLKEY        IFNE      *BLANKS
558200050413     c                   ADD       1             K
558300050413     c     K             OCCUR     DSEI
558400050413     c                   MOVEL     TBLKEY        CEI(K)
558500050413     c                   MOVEL     TBLUNI        DSEI
558600050413     c                   END
558700050413     c                   END
558800050413     c     ENDEI         TAG
558900050413     c                   ENDSR
559000050331      **********************************************************
559100050331      ** Caricamento DS Multipla Codici Tassazione
559200050331      **********************************************************
559300050331     c     Carct         Begsr
559400050331      ** pulizia
559500050331     c                   do        600           A
559600050331     c     a             occur     dsct
559700050331     c                   clear                   dsct
559800050331     c                   enddo
559900050331      ** Carico la schiera
560000050331     c                   movel     'CT'          cod
560100050331     c     ktab          setll     tabel                                  07
560200050331     c  N07              seton                                          H7
560300050331     c   H7              goto      fine
560400050331     c                   z-add     0             k
560500050331     c                   do        *hival        a
560600050331     c     ktab          reade     tabel00f
560700050418     c                   If        %eof(tabel00f)
560800050418     c                   leave
560900050418     c                   endif
561000050418     c                   If        tblkey <> *blanks
561100050331     c                   add       1             k
561200050331     c                   movel     tblkey        cts(k)
561300050331     c     k             occur     dsct
561400050331     c                   movel     tbluni        dsct
561500050331     c                   endif
561600050331     c                   enddo
561700050331
561800050331     c                   endsr
561900981023     C**----------------------------------------
562000981023     C**    CARICAMENTO NUMERI DI FATTURA
562100981023     C**----------------------------------------
562200050413     c     CARNFT        BEGSR
562300050413      **
562400050413      ** PER FATTURE FINE MESE
562500981027     C                   MOVE      '1'           XNMRIC            1
562600981023     C                   MOVEl     §QCRIV        wserie            4            REGISTRO VEND.
562700981023     C     *IN20         IFEQ      *ON
562800981026     C                   MOVEL     DTDFF8        xnmdat                         ANNO FATTURA
562900981027     C                   MOVEL     DTDFF8        xnmdag                         ANNO FATTURA
563000981023     C                   MOVE      §QCFI2        wserie                         LIBRO IVA FT.A.
563100981023     c                   exsr      repnum
563200981023     C                   z-add     xnmnum        nftfm
563300981023     C                   z-add     xnmnum        wnftfm            9 0
563400981023     C                   ENDIF
563500050413      ** PER FATTURE INIZIO MESE
563600050413      **  E FATTURAZIONE NON MENSILE
563700981026     C   20              MOVEL     DTDFI8        xnmdat                         ANNO FATTURA
563800981026     C  N20              MOVEL     DTDFT8        xnmdat                         ANNO FATTURA
563900981027     C   20              MOVEL     DTDFI8        xnmdag                         ANNO FATTURA
564000981027     C  N20              MOVEL     DTDFT8        xnmdag                         ANNO FATTURA
564100981023     C                   MOVE      §QCFIV        wserie                         LIBRO IVA VEND.
564200981023     c                   exsr      repnum
564300981023     C                   Z-ADD     xnmnum        NFTIM
564400981023     C                   z-add     xnmnum        wnftim            9 0
564500981023     C                   ENDSR
564600050413      *****************************************************
564700050413      ** CARICAMENTO DS TIPO BOLLA
564800050413      *****************************************************
564900050413     c     CARTB         BEGSR
565000050413     c                   DO        20            A
565100050413     c     A             OCCUR     DSTB
565200050413     c                   CLEAR                   DSTB
565300050413     c                   END
565400050413      ** PULIZIA DS
565500050413     c                   MOVEL     'TB'          COD
565600050413     c     KTAB          SETLL     TABEL                                  07
565700050413     c  N07              SETON                                          H7
565800050413     c   H7              GOTO      FINE
565900050413     c                   Z-ADD     0             K
566000050413     c                   DO        *HIVAL        A
566100050413     c     KTAB          READE     TABEL                                  07
566200050413     c   07              GOTO      ENDTB
566300050413     c     TBLKEY        IFNE      *BLANKS
566400050413     c                   ADD       1             K
566500050413     c                   MOVEL     TBLKEY        TB(K)
566600050413     c     K             OCCUR     DSTB
566700050413     c                   MOVEL     TBLUNI        DSTB
566800050413     c                   END
566900050413     c                   END
567000050413     c     ENDTB         TAG
567100050413     c                   ENDSR
567200981022      *----------------------------------------------------
567300981022     C     primanota     BEGSR
567400981022     C*----------------------------------------------------
567500981022     C*
567600981022     C* contabilizzazione batch
567700981022     c                   reset                   ndc071ds
567800981022     C                   MOVE      wsocieta      SOCieta071
567900981022     C                   MOVE      KNRAZ         rifint071
568000981022     c                   move      *loval        dtregda071
568100981022     c                   move      *hival        dtrega071
568200981022     c                   move      *loval        subnumd071
568300981022     c                   move      *hival        subnuma071
568400990506     C                   MOVE      *off          solcont071                     contab.
568500990506     C***                MOVE      *on           solcont071                     solo stampa
568600981022    >C                   MOVE      *off          interat071
568700981022     C                   move      *off          err071
568800981022     c                   move      *off          control071
568900981022     C                   MOVEL     NDC071DS      kpjbu
569000981022     C*
569100981022     C                   CALL      'NDC071C'
569200981022     C                   parm                    kpjba
569300981022     C                   movel     kpjbu         ndc071ds
569400981022     C*
569500981022     C                   ENDSR
569600060228
569700060228      ****************************************************
569800060228      ** Richiamo programma stampa manca tariffa
569900060228      ****************************************************
570000060228     c     mancatar      Begsr
570100060228      *
570200060228     c                   clear                   tnsb44ds
570300060228
570400060228     c                   move      00010101      D44dsd
570500060228     c                   move      dtdsp         D44dsa
570600060228      * creazione file
570700060228     c                   eval      d44crf = 'S'
570800060228      *
570900060228     c                   movel     d44dsa        com8a
571000060301     c                   eval      D44mbr = 'F'+ com8a
571100060228      * stampa imponibile
571200060228     c                   eval      d44fsi = 'N'
571300060228      * stampa separata per P.O.
571400060228     c                   eval      d44spp = 'S'
571500060228      * stampa unica
571600060228     c                   eval      d44sun = 'N'
571700060228      * coda di stampa
571800060228     c                   eval      d44out = 'O'
571900060228      * totali per area
572000060228     c                   eval      d44tot = 'S'
572100060228      * stampante totali per area
572200060228     c                   eval      d44Tou = 'T'
572300060228
572400060228
572500060228     c                   close     wfmtc00f
572600060228
572700060228      * LANCIO
572800060228     c                   movel     tnsb44ds      Kpjbu
572900060228
573000060228     c                   call      'TNSB46R'
573100060228     c                   parm                    kpjba
573200060228      *
573300060228     c                   endsr
573400060228
573500941121     O******************************************************************
573600060228     OPRTF198   E            TESTCON          01
573700050413     O                       RSUT                20
573800060228     O                                           83 'CONTROLLO FATTURAZIONE AL'
573900060228     O                       VIDDFI             + 3 '  /  /    '
574000050413     O                                           96 'TNSF07R'
574100050413     O                       UDATE              112 '  /  /  '
574200050413     O          E            TESTCON          04
574300050413     O                                              '-------------------------'
574400050413     O                                              '-------------------------'
574500050413     O                                              '-------------------------'
574600050413     O                                              '-------------------------'
574700050413     O                                              '-------------------------'
574800050413     O                                              '-------'
574900050413     O          E            TESTCON          05
575000050413     O                                              '                         '
575100050413     O                                              '             '
575200050413     O                                              'ERRORI CONTABILIZZAZIONE'
575300050413     O                                              ' FATTURE'
575400050413     O                                              '                         '
575500050413     O                                              '             '
575600050413     O          E            TESTCON          06
575700050413     O                                              '-------------------------'
575800050413     O                                              '-------------------------'
575900050413     O                                              '-------------------------'
576000050413     O                                              '-------------------------'
576100050413     O                                              '-------------------------'
576200050413     O                                              '-------'
576300050413     O          E            DETFAT      2
576400050413     O                                              'Fattura n° '
576500050413     O                       fatnft        z   +  2
576600050413     O                                              'del '
576700050413     O                       stadft            +  2 '  /  /  '
576800050413     o                                              'Cliente '
576900050413     O                       tasksc            +  2
577000050413     O                                         +  2 'più di tre righe di totale'
577100050413     O          E            DETcon      2
577200050413     O                                              'Spedizione '
577300050413     O                       tasaas            +  2
577400050413     O                                              '--'
577500050413     O                       taslnp
577600050413     O                                              '--'
577700050413     O                       tasnrs
577800050413     O                                              '--'
577900050413     O                       tasnsp
578000050413     O
578100050413     O                       tasmsg            +  5
578200000000     ODISSPE    E            HDGDIS           02
578300000000     O                       RSUT                20
578400060228     O                                           83 'ELENCO DISTINTE SPEDIZIONI'
578500060228     O                                           +3 'Al '
578600060228     O                       VIDDFI              +1 '  /  /    '
578700000000     O                       UDATE              112 '  /  /  '
578800000000     O          E            HDGDIS           04 06
578900000724     O                                           24 '---CLIENTE--------------'
579000000000     O                                           48 '------------------------'
579100000724     O                                           72 '------------------------'
579200010924     O                                           98 'IMPONIBILE--DIVISA--N.SPED'
579300010927     O                                          104 'IZIONI'
579400000000     O          E            DETDIS      2
579500000724     O                       BOLKSC
579600000724     O                       FATKSC            +  1
579700000000     O                       FATRA1            +  1
579800050413     O                       TIMD          2     84
579900010924     O                       VIDDIV              88
580000010924      * numero totale spedizioni in distinta
580100010927     O                       numspe        z    104
580200001024     o* totali distinta spedizione
580300001024     O          E            statds      2
580400001024     O                                           72 '            ------------'
580500010926     o                                           88 '----------------'
580600001027     O          E            statds      2
580700050413     o                       tdspd         2     84
580800010924     o                       viddiv              88
580900960904     OFTWCODIM  E            HCODI            04
581000960927     O                       STADFT              68 '  /  /  '
581100960927     O                       FATNFT        Z     85
581200960927     O                       PAGE          Z    101
581300960927     O                       WLBETI             113
581400960904     O          E            HCODI            17
581500960927     O                                              'CODICE CLIENTE:'
581600960904     O                       FATKSC            +  2
581700960904     O                                         +  4 '('
581800960904     O                       ASFAT             +  0
581900960904     O                                         +  0 ')'
582000960904     O          E            HCODI            19
582100960927     O                                              'Per informazioni'
582200900926     O                                         +  1 'rivolgersi presso:'
582300960904     O          E            HCODI            21
582400960927     O                       PDFI
582500960927     O                       FATRA1             103
582600960904     O          E            HCODI            22
582700960927     O                       PVIA
582800960904     O          E            HCODI            23
582900960927     O                       PCAP
583000900704     O                       PLOC              +  2
583100900704     O                       PPRV              +  2
583200960927A    O                       FATVIA              85
583300960912     O          E            HCODI            24
583400960927     O                                              'Telef.:'
583500960912     O                       PTEL              +  2
583600960912     O          E   N01      HCODI            25
583700960927     O                                              'Fax   :'
583800960912     O                       PFAX              +  2
583900960927     O                       FATCAP              60
584000960905     O                       FATCIT            +  2
584100960905     O                       FATPRV            +  2
584200960905     O          E    01      HCODI            25
584300960927     O                                              'Fax   :'
584400960912     O                       PFAX              +  2
584500960927     O                       FATCAE              64
584600960905     O                       FATCIT            +  2
584700960905     O                       FATPRV            +  2
584800960905     O                       FATSTA            +  2
584900060324     O          E            HCODI            26
585000060324     O                       mittmail
585100960912     O          E            HCODI            27
585200960927     O                                              'Cod.Cli.Bollettazione:'
585300950901     O                       BOLKSC            +  1
585400960905     O                                         +  4 '('
585500960905     O                       ASBOL             +  0
585600960905     O                                         +  0 ')'
585700070508     O          E   N01      HCODI            28
585800061108     O                                              'C.F.  : '
585900061108     O                       FATCDF            +  1
586000960912     O          E            HCODI            29
586100960927     O                                              'P.IVA : '
586200950901     O                       FATIVA            +  1
586300051110     O          E            HCODI            30
586400051110     O                       stmail1
586500051110     O          E            HCODI            31
586600051110     O                       stmail2
586700960912     O          E            HCODI            35 38
586800960927     O                       WFADES
586900960912     O                       FATBNA            + 12
587000960905     O** TESTATA DALLA 2 PAGINA
587100961028     O          E            HCODI2           08
587200960927     O                       STADFT              68 '  /  /  '
587300960927     O                       FATNFT        Z     85
587400960927     O                       PAGE          Z    101
587500960927     O                       WLBETI             113
587600961028     O          E            HCODI2           10 14
587700970108     O                       FATKSC            + 61
587800961029     O                       FATRA2            +  1
587900960905     O**
588000000110     O          E            H2CODI      1
588100000110     O                       bolra1              48
588200000110     O          E            H2CODI      1
588300000110     O                       bolvia              30
588400000110     O              n15      bolcap              36
588500000110     O               15      bolcae              40
588600000110     O                       bolcit              71
588700000110     O                       bolprv              74
588800050421      ** bsc codice bollettazione di raggruppamento
588900050421     O          E            db1ODI      1
589000050426     O                       wf_tasbsc     Z
589100050426     O                       bscra1            +  1
589200050421     O          E            db2ODI      1
589300050421     O                       bscvia              30
589400050421     O              n16      bsccap              36
589500050421     O               16      bsccae              40
589600050421     O                       bsccit              71
589700050421     O                       bscprv              74
589800000110     O          E            DCODI       1
589900050413     O                       ID(3)         4    113
590000000124     O                       S(3)               105
590100050413     O                       ID(2)         4    104
590200000124     O                       S(2)                96
590300050413     O                       ID(1)         4     95
590400000124     O                       S(1)                87
590500050413     O                       PRTPORD       4     86
590600960927     O                     09TASQFT        4     77
590700960927     O                    N09TASPVL        Z     77
590800020827     O                    N09STApkf        4     72
590900960927     O                  41N09TASVLF        2     65
591000960927     O                       TASNCL        Z     58
591100960927     O                       WCAP                53
591200960927     O                       RSC13               47
591300960927     O                       CIT3                33
591400960927     O                       CIT2                30
591500030512     O              n29      RMN9          Z     27
591600030512     O               29      RMa09               27
591700960927     O                       WTSP                18
591800960927     O                       MDSP                17
591900960927     O                       GDSP                15
592000960927     O                       TASNSP        Z     12
592100960927     O                       TASNRS        Z      5
592200960927     O                       TASLNP               3
592300941110     O          E            DCODI2      1
592400050413     O                       I2D(3)        4    113
592500000124     O                       S2(3)              105
592600050413     O                       I2D(2)        4    104
592700000124     O                       S2(2)               96
592800050413     O                       I2D(1)        4     95
592900000124     O                       S2(1)               87
593000020527     O               22                          50 'Rif:'
593100020527     O               22      WMITalf             66
593200020527     O               22                          19 'Mancato ritiro'
593300020527     O               22      WMITORI             45
593400020527     O              n22      WMITORI             30
593500000324     O               19                          42 'PARCEL :'
593600060703     O               19      WPARCEL             57
593700000110     O          E            DMIOI2      1
593800020527     O               22                          50 'Rif:'
593900020527     O               22      WMITalf             66
594000020527     O               22                          19 'Mancato ritiro'
594100020527     O               22      WMITORI             45
594200020527     O              N22      WMITORI             30
594300000324     O               19                          42 'PARCEL :'
594400060703     O               19      WPARCEL             57
594500030909     O          E            DCOOI2      1
594600030909     O                                        +   5 'Colli Originali :'
594700030909     O                       StpNcl        Z  +   1
594800030909     O          E            DDEII2      1
594900030909     O                       DesEsIva         +   5
595000030923      * diritto di fatturazione VARIA D
595100030923     O          E            DDVDI2      1
595200050413     O                       variadd       4     95
595300030923     O                                           87 'D'
595400050422     O          E            tboDI0      1
595500050422     O          E            tboDI1      1
595600050422     o                                         +  0 'Tot.Cli.:'
595700050421     o                       tksc          z   +  1
595800050422     o                       trsc                54
595900060908     o                                           59 'Tot.'
596000050421     o                       timps         4     76
596100050421     o                                           84 'Franchi'
596200050421     o                       tfras         4     95
596300060908     o                                          102 'Asseg.'
596400050421     o                       tasss         4    113
596500050620
596600050620     O          E            tboDI1P     1
596700050620     o                                           29 '   Pag. da '
596800050620     o                       tpgi          z   +  0
596900050620     o                                         +  1 'a  '
597000050620     o                       tpgf          z   +  0
597100050620
597200050421     O          E            tboDi2      1
597300050421     o                                         +  0 'TOTALI   '
597400050421     O          E            tboDi3      1
597500050421     o                                           61 'Totale'
597600050421     o                       timpst        4     76
597700050421     o                       tfrast        4     95
597800050421     o                       tassst        4    113
597900941119     O          E            DCODI4      1
598000020320     O                       decleg
598100941118     O          E            DCODI4      1
598200960927     O                       LES(1)
598300941119     O                       LIN(1)            +  0
598400941118     O                       LED(1)            +  0
598500941119     O                       LES(2)            +  2
598600941119     O                       LIN(2)            +  0
598700941118     O                       LED(2)            +  0
598800941119     O                       LES(3)            +  2
598900941119     O                       LIN(3)            +  0
599000941118     O                       LED(3)            +  0
599100941119     O                       LES(4)            +  2
599200941119     O                       LIN(4)            +  0
599300941118     O                       LED(4)            +  0
599400941119     O                       LES(5)            +  2
599500941119     O                       LIN(5)            +  0
599600941118     O                       LED(5)            +  0
599700941119     O                       LES(6)            +  2
599800941119     O                       LIN(6)            +  0
599900941118     O                       LED(6)            +  0
600000941119     O                       LES(7)            +  2
600100941119     O                       LIN(7)            +  0
600200941118     O                       LED(7)            +  0
600300941119     O                       LES(8)            +  2
600400941119     O                       LIN(8)            +  0
600500941118     O                       LED(8)            +  0
600600960906     O          E            DCODI5      1
600700960927     O                       LES(9)
600800960906     O                       LIN(9)            +  0
600900960906     O                       LED(9)            +  0
601000960906     O                       LES(10)           +  2
601100960906     O                       LIN(10)           +  0
601200960906     O                       LED(10)           +  0
601300960906     O                       LES(11)           +  2
601400941119     O                       LIN(11)           +  0
601500941118     O                       LED(11)           +  0
601600941119     O                       LES(12)           +  2
601700941119     O                       LIN(12)           +  0
601800941118     O                       LED(12)           +  0
601900941119     O                       LES(13)           +  2
602000941119     O                       LIN(13)           +  0
602100941118     O                       LED(13)           +  0
602200941119     O                       LES(14)           +  2
602300941119     O                       LIN(14)           +  0
602400941118     O                       LED(14)           +  0
602500941119     O                       LES(15)           +  2
602600941119     O                       LIN(15)           +  0
602700941118     O                       LED(15)           +  0
602800941119     O                       LES(16)           +  2
602900941119     O                       LIN(16)           +  0
603000941118     O                       LED(16)           +  0
603100960906     O          E            DCODI6      1
603200960927     O                       LES(17)
603300941119     O                       LIN(17)           +  0
603400941118     O                       LED(17)           +  0
603500941119     O                       LES(18)           +  2
603600941119     O                       LIN(18)           +  0
603700941118     O                       LED(18)           +  0
603800941119     O                       LES(19)           +  2
603900941119     O                       LIN(19)           +  0
604000941118     O                       LED(19)           +  0
604100941119     O                       LES(20)           +  2
604200941119     O                       LIN(20)           +  0
604300941118     O                       LED(20)           +  0
604400021104     O          E    45      TCODI4      2
604500960927     O                                           82 'Come da Vs.dichiarazione'
604600960927     O                                          101 'Prot.N.        del'
604700960927     O                       FATNPR        Z     96
604800960927     O                       FATDPR        Y    113
604900021104     O          E    11      tCODI4      1
605000021104     O                       staese              85
605100020830     O          E            tCODI5      1
605200020830     O                       stavdl              85
605300960905     O          E            TCODI            90
605400941120     O          E            TCODI2      1
605500050413     O               89      IVD(A)        4     73
605600960927     O               89      PERIVA        4     61
605700960927     O              N89      DES(A)              73
605800050413     O                       IMD(A)        4     55
605900050413     O                       VARIED        4     40
606000050413     O                       PRTTPID       4     25
606100941110     O          E            TCODI3      0
606200050413     O                       FATTOTD       4    112
606300050413     O                       FATBOLD       4     97
606400000126     O          E    60      TCODI3      1
606500000124     O                       VIDDIV               6
606600000124     O                                            8 ':'
606700000126     O          E   n60      TCODI3      0
606800000126     O                       VIDDIV               6
606900000126     O                                            8 ':'
607000960905     O          E            RCODI            92
607100960927     O                                          110 'Riporto'
607200951031     O          E            RCODI2      0
607300960927     O                                          113 'SEGUE TOTALI'
607400960906     OFTWCODFM  E            HCODF            04
607500960927     O                       STADFT              68 '  /  /  '
607600960927     O                       FATNFT        Z     85
607700960927     O                       PAGE          Z    101
607800960927     O                       WLBETI             113
607900960906     O          E            HCODF            17
608000960927     O                                              'CODICE CLIENTE:'
608100960906     O                       FATKSC            +  2
608200960906     O                                         +  4 '('
608300960906     O                       ASFAT             +  0
608400960906     O                                         +  0 ')'
608500960906     O          E            HCODF            19
608600960927     O                                              'Per informazioni'
608700960906     O                                         +  1 'rivolgersi presso:'
608800960906     O          E            HCODF            21
608900960927     O                       PDFI
609000960930     O                       FATRA1             103
609100960906     O          E            HCODF            22
609200960927     O                       PVIA
609300960906     O          E            HCODF            23
609400960927     O                       PCAP
609500960906     O                       PLOC              +  2
609600960906     O                       PPRV              +  2
609700960930     O                       FATVIA              85
609800960912     O          E            HCODF            24
609900960927     O                                              'Telef.:'
610000960906     O                       PTEL              +  2
610100960912     O          E   N01      HCODF            25
610200960927     O                                              'Fax   :'
610300960912     O                       PFAX              +  2
610400960930     O                       FATCAP              60
610500960906     O                       FATCIT            +  2
610600960906     O                       FATPRV            +  2
610700960906     O          E    01      HCODF            25
610800960927     O                                              'Fax   :'
610900960912     O                       PFAX              +  2
611000960930     O                       FATCAE              64
611100960906     O                       FATCIT            +  2
611200960906     O                       FATPRV            +  2
611300960906     O                       FATSTA            +  2
611400060324     O          E            HCODF            26
611500060324     O                       mittmail
611600960912     O          E            HCODF            27
611700960927     O                                              'Cod.Cli.Bollettazione:'
611800960906     O                       BOLKSC            +  1
611900960906     O                                         +  4 '('
612000960906     O                       ASBOL             +  0
612100960906     O                                         +  0 ')'
612200070508     O          E   N01      HCODF            28
612300061108     O                                              'C.F.  : '
612400061108     O                       FATCDF            +  1
612500960912     O          E            HCODF            29
612600960927     O                                              'P.IVA : '
612700960906     O                       FATIVA            +  1
612800051110     O          E            HCODF            30
612900051110     O                       stmail1
613000051110     O          E            HCODF            31
613100051110     O                       stmail2
613200960912     O          E            HCODF            35 38
613300960927     O                       WFADES
613400960912     O                       FATBNA            + 12
613500960906     O** TESTATA DALLA 2 PAGINA
613600961029     O          E            HCODF2           08
613700960927     O                       STADFT              68 '  /  /  '
613800960927     O                       FATNFT        Z     85
613900960927     O                       PAGE          Z    101
614000960927     O                       WLBETI             113
614100961029     O          E            HCODF2           10 14
614200970108     O                       FATKSC            + 61
614300961029     O                       FATRA2            +  1
614400000110     O**
614500000110     O          E            H2CODF      1
614600000110     O                       bolra1              48
614700000110     O          E            H2CODF      1
614800000110     O                       bolvia              30
614900000110     O              n15      bolcap              36
615000000110     O               15      bolcae              40
615100000110     O                       bolcit              71
615200000110     O                       bolprv              74
615300050421      ** bsc codice bollettazione di raggruppamento
615400050421     O          E            db1ODf      1
615500050426     O                       wf_tasbsc     Z
615600050426     O                       bscra1            +  1
615700050421     O          E            db2ODf      1
615800050421     O                       bscvia              30
615900050421     O              n16      bsccap              36
616000050421     O               16      bsccae              40
616100050421     O                       bsccit              71
616200050421     O                       bscprv              74
616300000110     O**
616400960906     O          E            DCODF       1
616500050413     O                       ID(3)         4    113
616600000124     O                       S(3)               105
616700050413     O                       ID(2)         4    104
616800000124     O                       S(2)                96
616900050413     O                       ID(1)         4     95
617000000124     O                       S(1)                87
617100050413     O                       PRTPORD       4     86
617200960927     O                     09TASQFT        4     77
617300960927     O                    N09TASPVL        Z     77
617400020827     O                    N09STApkf        4     72
617500960927     O                  41N09TASVLF        2     65
617600960927     O                       TASNCL        Z     58
617700960927     O                       WCAP                53
617800960927     O                       RSC13               47
617900960927     O                       CIT3                33
618000960927     O                       CIT2                30
618100030512     O              n29      RMN9          Z     27
618200030512     O               29      RMa09               27
618300960927     O                       WTSP                18
618400960927     O                       MDSP                17
618500960927     O                       GDSP                15
618600960927     O                       TASNSP        Z     12
618700960927     O                       TASNRS        Z      5
618800960927     O                       TASLNP               3
618900960906     O          E            DCODF2      1
619000050413     O                       I2D(3)        4    113
619100000124     O                       S2(3)              105
619200050413     O                       I2D(2)        4    104
619300000124     O                       S2(2)               96
619400050413     O                       I2D(1)        4     95
619500000124     O                       S2(1)               87
619600020527     O               22                          50 'Rif:'
619700020527     O               22      WMITalf             66
619800020527     O               22                          19 'Mancato ritiro'
619900020527     O               22      WMITORI             45
620000020527     O              N22      WMITORI             30
620100000324     O               19                          42 'PARCEL :'
620200060703     O               19      WPARCEL             57
620300000110     O          E            DMIOF2      1
620400020527     O               22                          50 'Rif:'
620500020527     O               22      WMITalf             66
620600020527     O               22                          19 'Mancato ritiro'
620700020527     O               22      WMITORI             45
620800020527     O              N22      WMITORI             30
620900000324     O               19                          42 'PARCEL :'
621000060703     O               19      WPARCEL             57
621100030909     O          E            DCOOF2      1
621200030909     O                                        +   5 'Colli Originali :'
621300030909     O                       StpNcl        Z  +   1
621400030909     O          E            DDEIF2      1
621500030909     O                       DesEsIva         +   5
621600030923      * diritto di fatturazione VARIA D
621700030923     O          E            DDVDF2      1
621800050413     O                       variadd       4     95
621900030923     O                                           87 'D'
622000050422     O          E            tboDf0      1
622100050422     O          E            tboDf1      1
622200050422     o                                         +  0 'Tot.Cli.:'
622300050421     o                       tksc          z   +  1
622400050422     o                       trsc                54
622500060908     o                                           59 'Tot.'
622600060908     o                       timps         4     76
622700060908     o                                           84 'Franchi'
622800060908     o                       tfras         4     95
622900060908     o                                          102 'Asseg.'
623000060908     o                       tasss         4    113
623100050620
623200050620     O          E            tboDf1P     1
623300050620     o                                           29 '   Pag. da '
623400050620     o                       tpgi          z   +  0
623500050620     o                                         +  1 'a  '
623600050620     o                       tpgf          z   +  0
623700050620
623800050421     O          E            tboDf2      1
623900050421     o                                         +  0 'TOTALI   '
624000050421     O          E            tboDf3      1
624100050421     o                                           61 'Totale'
624200050421     o                       timpst        4     76
624300050421     o                       tfrast        4     95
624400050421     o                       tassst        4    113
624500960906     O          E            DCODF4      1
624600020320     O                       decleg
624700960906     O          E            DCODF4      1
624800960927     O                       LES(1)
624900960906     O                       LIN(1)            +  0
625000960906     O                       LED(1)            +  0
625100960906     O                       LES(2)            +  2
625200960906     O                       LIN(2)            +  0
625300960906     O                       LED(2)            +  0
625400960906     O                       LES(3)            +  2
625500960906     O                       LIN(3)            +  0
625600960906     O                       LED(3)            +  0
625700960906     O                       LES(4)            +  2
625800960906     O                       LIN(4)            +  0
625900960906     O                       LED(4)            +  0
626000960906     O                       LES(5)            +  2
626100960906     O                       LIN(5)            +  0
626200960906     O                       LED(5)            +  0
626300960906     O                       LES(6)            +  2
626400960906     O                       LIN(6)            +  0
626500960906     O                       LED(6)            +  0
626600960906     O                       LES(7)            +  2
626700960906     O                       LIN(7)            +  0
626800960906     O                       LED(7)            +  0
626900960906     O                       LES(8)            +  2
627000960906     O                       LIN(8)            +  0
627100960906     O                       LED(8)            +  0
627200960906     O          E            DCODF5      1
627300960927     O                       LES(9)
627400960906     O                       LIN(9)            +  0
627500960906     O                       LED(9)            +  0
627600960906     O                       LES(10)           +  2
627700960906     O                       LIN(10)           +  0
627800960906     O                       LED(10)           +  0
627900960906     O                       LES(11)           +  2
628000960906     O                       LIN(11)           +  0
628100960906     O                       LED(11)           +  0
628200960906     O                       LES(12)           +  2
628300960906     O                       LIN(12)           +  0
628400960906     O                       LED(12)           +  0
628500960906     O                       LES(13)           +  2
628600960906     O                       LIN(13)           +  0
628700960906     O                       LED(13)           +  0
628800960906     O                       LES(14)           +  2
628900960906     O                       LIN(14)           +  0
629000960906     O                       LED(14)           +  0
629100960906     O                       LES(15)           +  2
629200960906     O                       LIN(15)           +  0
629300960906     O                       LED(15)           +  0
629400960906     O                       LES(16)           +  2
629500960906     O                       LIN(16)           +  0
629600960906     O                       LED(16)           +  0
629700960906     O          E            DCODF6      1
629800960927     O                       LES(17)
629900960906     O                       LIN(17)           +  0
630000960906     O                       LED(17)           +  0
630100960906     O                       LES(18)           +  2
630200960906     O                       LIN(18)           +  0
630300960906     O                       LED(18)           +  0
630400960906     O                       LES(19)           +  2
630500960906     O                       LIN(19)           +  0
630600960906     O                       LED(19)           +  0
630700960906     O                       LES(20)           +  2
630800960906     O                       LIN(20)           +  0
630900960906     O                       LED(20)           +  0
631000021104     O          E    45      TCODF4      2
631100960927     O                                           82 'Come da Vs.dichiarazione'
631200960927     O                                          101 'Prot.N.        del'
631300960927     O                       FATNPR        Z     96
631400960927     O                       FATDPR        Y    113
631500021104     O          E    11      tCODF4      1
631600021104     O                       staese              85
631700020830     O          E            tCODf5      1
631800020830     O                       stavdl              85
631900960906     O          E            TCODF            90
632000960906     O          E            TCODF2      1
632100050413     O               89      IVD(A)        4     73
632200960927     O               89      PERIVA        4     61
632300960927     O              N89      DES(A)              73
632400050413     O                       IMD(A)        4     55
632500050413     O                       VARIED        4     40
632600050413     O                       PRTTPID       4     25
632700960906     O          E            TCODF3      0
632800050413     O                       FATTOTD       4    112
632900050413     O                       FATBOLD       4     97
633000000126     O          E    60      TCODF3      1
633100000124     O                       VIDDIV               6
633200000124     O                                            8 ':'
633300000126     O          E   n60      TCODF3      0
633400000126     O                       VIDDIV               6
633500000126     O                                            8 ':'
633600960906     O          E            RCODF            92
633700960927     O                                          110 'Riporto'
633800960906     O          E            RCODF2      0
633900960927     O                                          113 'SEGUE TOTALI'
634000960906     O**
634100960906     OFTWCODDS  E            HCODD            04
634200960927     O                       STADFT              68 '  /  /  '
634300960927     O                       PAGE          Z    101
634400960927     O                       WLBETI             113
634500960917     O          E            HCODD            05
634600960927     O                                           92 'DISTINTA SPEDIZIONI'
634700960906     O          E            HCODD            17
634800960927     O                                              'CODICE CLIENTE:'
634900960906     O                       FATKSC            +  2
635000960906     O                                         +  4 '('
635100960906     O                       ASFAT             +  0
635200960906     O                                         +  0 ')'
635300960906     O          E            HCODD            19
635400960927     O                                              'Per informazioni'
635500960906     O                                         +  1 'rivolgersi presso:'
635600960906     O          E            HCODD            21
635700960927     O                       PDFI
635800960930     O                       FATRA1             103
635900960906     O          E            HCODD            22
636000960927     O                       PVIA
636100960906     O          E            HCODD            23
636200960927     O                       PCAP
636300960906     O                       PLOC              +  2
636400960906     O                       PPRV              +  2
636500960930     O                       FATVIA              85
636600960912     O          E            HCODD            24
636700960927     O                                              'Telef.:'
636800960906     O                       PTEL              +  2
636900960912     O          E   N01      HCODD            25
637000960927     O                                              'Fax   :'
637100960912     O                       PFAX              +  2
637200960930     O                       FATCAP              60
637300960906     O                       FATCIT            +  2
637400960906     O                       FATPRV            +  2
637500960906     O          E    01      HCODD            25
637600960927     O                                              'Fax   :'
637700960912     O                       PFAX              +  2
637800960930     O                       FATCAE              64
637900960906     O                       FATCIT            +  2
638000960906     O                       FATPRV            +  2
638100960906     O                       FATSTA            +  2
638200060406     O          E            HCODD            26
638300060324     O                       mittmail
638400960912     O          E            HCODD            27
638500960927     O                                              'Cod.Cli.Bollettazione:'
638600960906     O                       BOLKSC            +  1
638700960906     O                                         +  4 '('
638800960906     O                       ASBOL             +  0
638900960906     O                                         +  0 ')'
639000070508     O          E   N01      HCODD            28
639100061108     O                                              'C.F.  : '
639200061108     O                       FATCDF            +  1
639300960912     O          E            HCODD            29
639400960927     O                                              'P.IVA : '
639500960906     O                       FATIVA            +  1
639600051110     O          E            HCODD            30
639700051110     O                       stmail1
639800051110     O          E            HCODD            31
639900051110     O                       stmail2
640000960912     O          E            HCODD            35 38
640100960927     O                       WFADES
640200960912     O                       FATBNA            + 12
640300960906     O** TESTATA DALLA 2 PAGINA
640400960917     O          E            HCODD2           08
640500960927     O                       STADFT              68 '  /  /  '
640600960927     O                       PAGE          Z    101
640700960927     O                       WLBETI             113
640800961029     O          E            HCODD2           09
640900960927     O                                           92 'DISTINTA SPEDIZIONI'
641000961029     O          E            HCODD2           10 14
641100970108     O                       FATKSC            + 61
641200961029     O                       FATRA2            +  1
641300000110     O**
641400000110     O          E            H2CODD      1
641500000110     O                       bolra1              48
641600000110     O          E            H2CODD      1
641700000110     O                       bolvia              30
641800000110     O              n15      bolcap              36
641900000110     O               15      bolcae              40
642000000110     O                       bolcit              71
642100000110     O                       bolprv              74
642200050421      ** bsc codice bollettazione di raggruppamento
642300050421     O          E            db1ODd      1
642400050426     O                       wf_tasbsc     Z
642500050426     O                       bscra1            +  1
642600050421     O          E            db2ODd      1
642700050421     O                       bscvia              30
642800050421     O              n16      bsccap              36
642900050421     O               16      bsccae              40
643000050421     O                       bsccit              71
643100050421     O                       bscprv              74
643200000110     O**
643300960906     O**
643400960906     O          E            DCODD       1
643500050413     O                       ID(3)         4    113
643600000124     O                       S(3)               105
643700050413     O                       ID(2)         4    104
643800000124     O                       S(2)                96
643900050413     O                       ID(1)         4     95
644000000124     O                       S(1)                87
644100050413     O                       PRTPORD       4     86
644200960927     O                     09TASQFT        4     77
644300960927     O                    N09TASPVL        Z     77
644400020827     O                    N09STApkf        4     72
644500960927     O                  41N09TASVLF        2     65
644600960927     O                       TASNCL        Z     58
644700960927     O                       WCAP                53
644800960927     O                       RSC13               47
644900960927     O                       CIT3                33
645000960927     O                       CIT2                30
645100030512     O              n29      RMN9          Z     27
645200030512     O               29      RMa09               27
645300960927     O                       WTSP                18
645400960927     O                       MDSP                17
645500960927     O                       GDSP                15
645600960927     O                       TASNSP        Z     12
645700960927     O                       TASNRS        Z      5
645800960927     O                       TASLNP               3
645900960906     O          E            DCODD2      1
646000050413     O                       I2D(3)        4    113
646100000124     O                       S2(3)              105
646200050413     O                       I2D(2)        4    104
646300000124     O                       S2(2)               96
646400050413     O                       I2D(1)        4     95
646500000124     O                       S2(1)               87
646600020527     O               22                          50 'Rif:'
646700020527     O               22      WMITalf             66
646800020527     O               22                          19 'Mancato ritiro'
646900020527     O               22      WMITORI             45
647000020527     O              N22      WMITORI             30
647100060727     O               19                          42 'PARCEL :'
647200060727     O               19      WPARCEL             57
647300000110     O          E            DMIOD2      1
647400020527     O               22                          50 'Rif:'
647500020527     O               22      WMITalf             66
647600020527     O               22                          19 'Mancato ritiro'
647700020527     O               22      WMITORI             45
647800020527     O              N22      WMITORI             30
647900060727     O               19                          42 'PARCEL :'
648000060727     O               19      WPARCEL             57
648100030909     O          E            DCOOD2      1
648200030909     O                                        +   5 'Colli Originali :'
648300030909     O                       StpNcl        Z  +   1
648400030909     O          E            DDEID2      1
648500030909     O                       DesEsIva         +   5
648600030923      * diritto di fatturazione VARIA D
648700030923     O          E            DDVDD2      1
648800050413     O                       variadd       4     95
648900030923     O                                           87 'D'
649000050422     O          E            tboDd0      1
649100050422     O          E            tboDd1      1
649200050422     o                                         +  0 'Tot.Cli.:'
649300050421     o                       tksc          z   +  1
649400050422     o                       trsc                54
649500060908     o                                           59 'Tot.'
649600060908     o                       timps         4     76
649700060908     o                                           84 'Franchi'
649800060908     o                       tfras         4     95
649900060908     o                                          102 'Asseg.'
650000060908     o                       tasss         4    113
650100050620
650200050620     O          E            tboDd1P     1
650300050620     o                                           29 '   Pag. da '
650400050620     o                       tpgi          z   +  0
650500050620     o                                         +  1 'a  '
650600050620     o                       tpgf          z   +  0
650700050620
650800050421     O          E            tboDd2      1
650900050421     o                                         +  0 'TOTALI   '
651000050421     O          E            tboDd3      1
651100050421     o                                           61 'Totale'
651200050421     o                       timpst        4     76
651300050421     o                       tfrast        4     95
651400050421     o                       tassst        4    113
651500960906     O          E            DCODD4      1
651600020320     O                       decleg
651700960906     O          E            DCODD4      1
651800960927     O                       LES(1)
651900960906     O                       LIN(1)            +  0
652000960906     O                       LED(1)            +  0
652100960906     O                       LES(2)            +  2
652200960906     O                       LIN(2)            +  0
652300960906     O                       LED(2)            +  0
652400960906     O                       LES(3)            +  2
652500960906     O                       LIN(3)            +  0
652600960906     O                       LED(3)            +  0
652700960906     O                       LES(4)            +  2
652800960906     O                       LIN(4)            +  0
652900960906     O                       LED(4)            +  0
653000960906     O                       LES(5)            +  2
653100960906     O                       LIN(5)            +  0
653200960906     O                       LED(5)            +  0
653300960906     O                       LES(6)            +  2
653400960906     O                       LIN(6)            +  0
653500960906     O                       LED(6)            +  0
653600960906     O                       LES(7)            +  2
653700960906     O                       LIN(7)            +  0
653800960906     O                       LED(7)            +  0
653900960906     O                       LES(8)            +  2
654000960906     O                       LIN(8)            +  0
654100960906     O                       LED(8)            +  0
654200960906     O          E            DCODD5      1
654300960927     O                       LES(9)
654400960906     O                       LIN(9)            +  0
654500960906     O                       LED(9)            +  0
654600960906     O                       LES(10)           +  2
654700960906     O                       LIN(10)           +  0
654800960906     O                       LED(10)           +  0
654900960906     O                       LES(11)           +  2
655000960906     O                       LIN(11)           +  0
655100960906     O                       LED(11)           +  0
655200960906     O                       LES(12)           +  2
655300960906     O                       LIN(12)           +  0
655400960906     O                       LED(12)           +  0
655500960906     O                       LES(13)           +  2
655600960906     O                       LIN(13)           +  0
655700960906     O                       LED(13)           +  0
655800960906     O                       LES(14)           +  2
655900960906     O                       LIN(14)           +  0
656000960906     O                       LED(14)           +  0
656100960906     O                       LES(15)           +  2
656200960906     O                       LIN(15)           +  0
656300960906     O                       LED(15)           +  0
656400960906     O                       LES(16)           +  2
656500960906     O                       LIN(16)           +  0
656600960906     O                       LED(16)           +  0
656700960906     O          E            DCODD6      1
656800960927     O                       LES(17)
656900960906     O                       LIN(17)           +  0
657000960906     O                       LED(17)           +  0
657100960906     O                       LES(18)           +  2
657200960906     O                       LIN(18)           +  0
657300960906     O                       LED(18)           +  0
657400960906     O                       LES(19)           +  2
657500960906     O                       LIN(19)           +  0
657600960906     O                       LED(19)           +  0
657700960906     O                       LES(20)           +  2
657800960906     O                       LIN(20)           +  0
657900960906     O                       LED(20)           +  0
658000021104     O          E    45      TCODD4      2
658100960927     O                                           82 'Come da Vs.dichiarazione'
658200960927     O                                          101 'Prot.N.        del'
658300960927     O                       FATNPR        Z     96
658400960927     O                       FATDPR        Y    113
658500021104     O          E    11      tCODD4      1
658600021104     O                       staese              85
658700020830     O          E            tCODd5      1
658800020830     O                       stavdl              85
658900960906     O          E            TCODD            90
659000960906     O          E            TCODD2      1
659100050413     O               89      IVD(A)        4     73
659200960927     O               89      PERIVA        4     61
659300960927     O              N89      DES(A)              73
659400050413     O                       IMD(A)        4     55
659500050413     O                       VARIED        4     40
659600050413     O                       PRTTPID       4     25
659700960906     O          E            TCODD3      0
659800050413     O                       FATTOTD       4    112
659900050413     O                       FATBOLD       4     97
660000000126     O          E    60      TCODD3      1
660100000124     O                       VIDDIV               6
660200000124     O                                            8 ':'
660300000126     O          E   N60      TCODD3      0
660400000126     O                       VIDDIV               6
660500000126     O                                            8 ':'
660600960906     O          E            RCODD            92
660700960927     O                                          110 'Riporto'
660800960906     O          E            RCODD2      0
660900960927     O                                          113 'SEGUE TOTALI'
661000961104     OQSYSPRT   E            NUMFAT           02
661100961104     O                       RSUT                20
661200060308     O                                           83 'FATTURAZIONE AL'
661300060228     O                       VIDDSP              +3 '  /  /    '
661400070207     O                                          110 'TNSF07R'
661500070207     O                       UDATE              130 '  /  /  '
661600981028     O* lotto di contabilizzazione
661700981028     O          E            NUMFAT           06
661800981028     O                                              'Numero lotto fatturaz.:'
661900981028     O                       knraz             +  2
662000971023     O* FATTURAZIONE MENSILE
662100981028     O          E    20      NUMFAT           08
662200961104     O                                              'FATTURE INIZIO MESE:'
662300961104     O                                         +  2 'PRIMO NUMERO'
662400961104     O                       WINPRI        Z   +  2
662500961104     O                                         +  4 'ULTIMO NUMERO'
662600961104     O                       WINULT        Z   +  2
662700971023     O* FATTURAZIONE NON MENSILE
662800981028     O          E   N20      NUMFAT           08
662900971023     O                                              'FATTURE :'
663000971023     O                                         +  2 'PRIMO NUMERO'
663100971023     O                       WINPRI        Z   +  2
663200971023     O                                         +  4 'ULTIMO NUMERO'
663300971023     O                       WINULT        Z   +  2
663400971023     O* FATTURAZIONE MENSILE
663500971023     O          E    20      NUMFAT           10
663600961104     O                                              'FATTURE FINE   MESE:'
663700961104     O                                         +  2 'PRIMO NUMERO'
663800961104     O                       WFIPRI        Z   +  2
663900961104     O                                         +  4 'ULTIMO NUMERO'
664000961104     O                       WFIULT        Z   +  2
664100000107      * TOTALI FATTURE EMESSE
664200000207     OSYSPRT    E            GEN              02
664300000107     O                       RSUT                20
664400040713     O                       SC(1)               86
664500050413     O                                           99 'TNSF07R'
664600040713     O                       WDTGIO             115 '  /  /    '
664700000107     O                       PAGE          Z    132
664800000107     O                                          127 'PAG.'
664900000107     O          E            GEN              03
665000000107     O                                           48 'A L'
665100040713     O               20      VIDDFI              60 '  /  /    '
665200040713     O              n20      VIDDFT              60 '  /  /    '
665300000107     O          E            GEN              06
665400000107     O                                           15 'TOTALI GENERALI'
665500000107     O          E            GEN              10
665600000107     O                                           17 'Totale imponibile'
665700000125     O                       IMPGEN        2     56
665800000125     O                       VIDDIV              60
665900000125     O               40      IMPGENCNT     2     87
666000000125     O              N42 40                       87 '    '
666100000125     O               40      XSCDIV              92
666200000107     O          E            GEN              11
666300000107     O                                           12 'Totale I.V.A'
666400000125     O                       IVAGEN        2     56
666500000125     O                       VIDDIV              60
666600000125     O               40      IVAGENCNT     2     87
666700000125     O              N42 40                       87 '    '
666800000125     O               40      XSCDIV              92
666900000107     O          E            GEN              13
667000000125     O                                           12 'Totale Bolli'
667100000125     O                       BOLGEN        2     56
667200000125     O                       VIDDIV              60
667300000125     O               40      BOLGENCNT     2     87
667400000125     O              N42 40                       87 '    '
667500000125     O               40      XSCDIV              92
667600000107     O          E            GEN              14
667700000107     O                                           16 'Totale fatturato'
667800000125     O                       TOTGEN        2     56
667900000125     O                       VIDDIV              60
668000000125     O               40      TOTGENCNT     2     87
668100000125     O              N42 40                       87 '    '
668200000125     O               40      XSCDIV              92
668300000107     O          E            GEN              16
668400000107     O                                           14 'Numero fatture'
668500050413     O                       NUMGEN        2     56
668600000107     O          E            GEN              18
668700000125     O                                           15 'TOTALE GENERALE'
668800000125     O                       TOTGEN        2     56
668900000125     O                       VIDDIV              60
669000000125     O                       NUMGEN        2     33
669100000125     O               40      TOTGENCNT     2     87
669200000125     O              N42 40                       87 '    '
669300000125     O               40      XSCDIV              92
669400941107** E
669500060227Manca Anagrafica Cliente
669600060227Codice Pagamento Errato
669700060227Cliente   N O N    Fatturabile
669800050405Errore in fase di contabilizzazione
669900050413Errore Esenzione Iva non presente
670000060227Manca Tariffa
670100060227Fattura con troppe Righe di totale
670200060227Impossibile Fatturare Cliente 8888
670300060227Imponibile           Maggiore del limite fatturabile                 9
670400060227Non Ammesso imponibile=0 per questo Cod.Bolla
670500000107**  SC STAMPA TOTALI
670600040713         - DISTINTA DELLE FATTURE EMESSE - CLIENTI CODIFICATI -
670700060227** cmdc crtdupobj
670800061012CPYF FROMFILE(         /WFMTC00F) TOFILE(QTEMP/WFMTC00F) FROMMBR(WFMTC00F) CRTFILE(*YES)
670900061011CRTDUPOBJ OBJ(WFMTC01L) FROMLIB(          ) OBJTYPE(*FILE) TOLIB(QTEMP)
671000061011CLRPFM FILE(QTEMP/WFMTC00F)
