000100040119      *===============================================================*
000200140407      * TicfQ2r    * Confronto fatturazione 15 Aprile
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000060914     fTitas38c  if   e           k disk
001100060914     fTita730c  if   e           k disk
001200060914     fTitaI01L  if   e           k disk
001300060914     Ffiar531c  if   e           k disk
001400060914     Ftncsb03l  if   e           k disk
001500060914     fwfcft01l  uf a e           k disk
001600060914     Ftabel00f  if   e           k disk
001700081105     Ffnevb04l  if   e           k disk
001900140407     fticfq2p   o    e             printer oflind(*in90)
002000040119      *
002100050603     d dAr5Ban       e ds
002200050603     d dAr5Bnb       e ds
002300050526     d kpjba         e ds
002400050609     d dstb          e ds
002500060915     d ds3a          e ds
002600110701      ** DS del tasflo del file TITAS00F
002700110701     D DTASFLO       E DS
002800050527     d dtas          e ds                  inz
002900050527     d  §tasflo      e                     extfld(tasflo)
003000060926      ** DS Flag operativi DS DTAS
003100060926     d Dtas01        e ds
003200050607      ** DS Calcolo tassazione  - Varie
003300050607     d Dtasv         e ds
003400050607     d  sv                     1     20
003500050607     d                                     Dim(20)                              Sigle varie
003600050607     d  va                    21    140P 3
003700050607     d                                     Dim(20)                              Importi varie
003800130124      ** DS data ritassazione DS DTASdta
003900130124     d Dtasdta       e ds
003901140409      ** campi di salvataggio x tassazione precedente
003902140409     d W_Dtasv         s                   like(dtas)
003903140409     d W_taspor        s                   like(taspor)
004000130124
004100050607     d param           ds
004101140407     D  pardrf                 1      8  0
004102140407     D  pardtc                 9     16  0
004103140407     D  pardal                17     24  0
004104140408     D  paraas                17     20  0
004105140408     D  parmgs                21     24  0
004106140407     D  paral                 25     32  0
004107140409
004108140409     D WLBDA8          DS                  INZ
004109140409     D  G08DAT                 1      8  0
004110140409     D  G08INV                 9     16  0
004111140409     D  G08ERR                17     17
004112140409     D  G08TGI                18     22  0
004113140409
005000050607     d
005100050607     dwfcftxxds      e ds                  extname(wfcft00f)
005200050607     dsvpre                   31     50    dim(20)
005300050607     dvapre                   51    170p 3 dim(20)
005400050607     dsvcor                  184    203    dim(20)
005500050607     dvacor                  204    323p 3 dim(20)
005600050607     d*
005700040331      *
005800050526     D***
005900040331      *
006000040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
006100050526      *
006200040331      * - Indici di schiera
006300040331     d xx              s              3  0 inz
006400050607     D k               s              5  0                                      indice
006500040331      * - Campi di comodo
006600050603     d kAr5Trd         s                   Like(Ar5Trd)
006700060914     d kmgs            s                   Like(TasMgs)
006800050607     D Flgta7          s              1  0
006900050609     d codut           s              1  0 Inz(1)
007000060915     D kcod            s                   like(tblcod)
007100060914     D ksvt            s                   like(taisvt)
007200081105     D Wcev            s                   like(evbcev) inz('RIC')
007300060914     D wmm             s              2  0
007400070207     D totspe          s              9  0
007500070207     D tottas          s              9  0
007600130110     D totsmt          s              9  0
007601140408     d antepo          s                   like(tasimv)
007700060914      *
007800060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
007900060914      *
008000060914      * codici bolla da scartare
008100060914     D tbs             s              2    dim(50)
008200060915      * codici bolla che prevedono la tassazione di una singola varia
008300060915     D cbo             s              2    dim(50)
008400130124      * sigle varie salvate
008500130124     D sav_sv          s              1    dim(20)
008600130124      * importo varie salvate
008700130124     D sav_va          s             11  3 dim(20)
008800060915      *
008900040119
009000040119      *
009100140409      * mi posiziono con data iniziale periodo da elaborare
009300140408     c     Ktas          setll     titas38c
009400050607     c                   do        *hival
009500060914     c                   clear                   dtas
009600060914     c                   clear                   dtasv
009700130124     c                   clear                   dtasdta
009701140409     c                   clear                   W_dtasv
009702140409     c                   clear                   W_taspor
009800140407     c                   read      titas38c
009900060914     c                   if        %eof(titas38c)
010000050607     c                   leave
010100050607     c                   endif
010200050614     c
010201140407     c                   movel(p)  tasaas        tasdsp
010202140407     c                   move      tasmgs        tasdsp
010300140407      * vado a fine se data spedizione maggiore del limite finale di elaborazione
010500140407     c                   if        tasdsp > paral
010600060914     c                   leave
010700050614     c                   endif
010800050614     c
011400060915
011500060915      * scarto  se si tratta di codice bolla che prevede solo una varia
011600060918     c                   z-add     1             kk                2 0
011700060915     c     tascbo        lookup    cbo(kk)                                05
011800060915     c                   if        *in05
011900060915     c                   iter
012000060915     c                   endif
012100060915     c
012200060914
012300060914      * scarto le bolle che sono state fatturate in filiale
012400140408     c                   If        tasfiv <> 998 and tasfiv <> 996
012401140408     c                               and tasfiv > 0
012500060914     c                   iter
012600060914     c                   endif
012700060914     c
012800060914
012900140407      * verifico se tassata fino all'imponibile in TITAI se già registrate su TITAI
013000060914     c                   eval      ksvt = '£'
013100060914     c     Ktai06        chain     titai01l
013200060914      * se trovato record imponibile  scarto la bolla
013300060914     c                   If        %found(titai01l)
013400060914     c                   iter
013500060914     c                   endif
013600060914     c
013601140408      * verifico se tassata fino all'imponibile con TASIMV > 0 scarto
013605140408     c                   If        TASIMV > 0 and tasfiv = 0
013606140407     c                   iter
013607140407     c                   endif
013608140407     c
015100060914
015200111025      * scarto bolle di  c/servizi, storni
015300050609     c     tastbl        lookup    tbs                                    30
015400140409     c                   if        *in30
015401140409     c                   iter
015402140409     c                   endif
015403140409
015404140409      * Solo in caso di fatture già contabilizzate vcerifico la presenza delle varie N e &
015405140409      * in TITAI per poter scartare la boll
015406140409     C                   IF        TASFIC <> ' '
015407140409      * se presenti varia "N" o "&" + 88888888 ignoro il record x quelle fatturate
015408140409     c                   eval      ksvt = 'N'
015409140409     c     Ktai06        chain     titai01l
015410140409      * se trovato record imponibile  scarto la bolla
015411140409     c                   If        %found(titai01l)  and taivat = 88888888
015412140409     c                   iter
015413140409     c                   endif
015414140409
015415140409     c                   eval      ksvt = '&'
015416140409     c     Ktai06        chain     titai01l
015417140409      * se trovato record imponibile  scarto la bolla
015418140409     c                   If        %found(titai01l)  and taivat = 88888888
015419140409     c                   iter
015420140409     c                   endif
015421140409
015422140409     c                   endif
015423140409
015424140409      * carico le varie presenti
015425140409     c                   Exsr      Carta7
015426140409      *
015427140409      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
015428140409      * con VARIA 'N' valorizzata con 888888 la scarto
015429140409      *
015430140409    2c                   if        tasimv = 0 and tasfbr = 'S'
015431140409     c                   z-add     1             xx
015432140409     c     'N'           lookup    sv(xx)                                 30
015433140409    3c                   if        *in30 and va(xx) =  88888888
015434140409     c                   iter
015435140409    3c                   endif
015436140409      *
015437140409    2c                   endif
015438140409      *
015439140409      * verifico se è esiste la varia & con importo uguale a 8888888 la scarto
015440140409      *
015441140409    2c                   if        tasimv = 0
015442140409     c                   z-add     1             xx
015443140409     c     '&'           lookup    sv(xx)                                 30
015444140409    3c                   if        *in30 and va(xx) =  88888888
015445140409     c                   iter
015446140409    3c                   endif
015447140409      *
015448140409    2c                   endif
015449140409      *
015450140409      * prima di eseguire la tassazione corrente   mi salvo i valori di titas
015451140409     c                   eval      W_taspor = taspor
015452140409     c                   eval      W_Dtasv = Dtasv
015500060914
015600140407      * carico la tassazione corrente  se imponibile > 0
015700060914     c                   exsr      sr_corrente
015800060914
015801140411      * SOMMO IL NUMERO DI SPEDIZIONE CHE SONO STATE  PRESE IN CONSIDEAZIONE
015802140411     c                   add       1             totspe
015900140409      * eseguo la tassazione precedente se non ci sono stati errori nella tassazione corrente
015903140411
015904140409     c                   if        taserr = ' '
016000130124     c                   exsr      sr_sf21
016001140409     c                   endif
016300060914
016400050607     c                   enddo
016500050526      *
016600060914      *
016700130110      * conto le spedizioni che non sono state tassate
016800130110     c                   eval      totsmt = totspe - tottas
016900070207      * stampo  la fine elaborazione
017000120213     c                   write     cf92s1
017100070207
017200040119     c                   eval      *inLR = *on
017300060914      *
017400050603     c                   movel     'C'           tastla
017500130124     C                   CALL      'TNSF21R'
017600050603     C                   PARM                    KPJBA
017700050603     C                   PARM                    DTAS
017800050603     C                   PARM                    DTASV
017900130124     C                   PARM                    DTASDTA
018000050614     c*
018100050614     c                   movel(p)  param         kpjbu
018200040331      *
018300050526      **-------------------------------------------------------------**
018400130124      ** Richiamo pgm tassaz. tnsf21r                                **
018500050526      **-------------------------------------------------------------**
018600130124     c     sr_sf21       BEGSR
018700050526      *
018800050601     c                   movel     *Blanks       tastla
018900050603     c                   Clear                   kpjbu
018901140409     c                   clear                   dtasdta
018902140409      * verifico se sto calcolando la tassazione precedente di una bolla già fatturata
018903140409     c                   If        tasfic <> ' '
018904140409     c                   clear                   dtasv
018905140409     c                   Clear                   taspor
018907140409      * Assicurazione
018908140409    2c                   IF        Tasfcm = 'F'
018909140409     c                   clear                   tasias
018910140409     c                   clear                   tasvas
018911140409    2c                   Endif
018912140409
018913140409      * recupero i valori  già tassati
018914140409     c                   exsr      Ric_tai
018915140409
018917140409     c                   else
018918140409      * se non è stata ancora fatturata devo recuperare i valori già impostati in bolla
018919140409     c                   eval      dtasv = w_dtasv
018920140409     c                   eval      taspor= W_taspor
018921140409
018922140409     c                   endif
018923140409
018924140409     c                   Clear                   tasimv
019400050603     c                   eval      tassva = *blanks
019500130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
019600130124      * tariffa con data tariffe
019700050614     c                   z-add     pardrf        tasdsp
019800050614     c                   z-add     pardrf        tasdct
019900130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
020000130124      * calcolo istat e calcolo fuel
020100130124     c                   eval      tasdtist = pardrf
020200130124     c                   movel(p)  tasaas        tasdtfue
020300130124     c                   move      tasmgs        tasdtfue
020301140204      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
020302140204      * calcolo fuel
020303140409     c                   If        tasdft = 0
020304140409     c                   movel(p)  tasaas        tasdtfat
020305140409     c                   move      tasmgs        tasdtfat
020306140409     c                   else
020307140204     c                   z-add     tasdft        tasdtfat
020308140409     c                   endif
021300060914
021400130124     c                   call      'TNSF21R'
021500060914     c                   PARM                    KPJBA
021600060914     c                   PARM                    DTAS
021700060914     c                   PARM                    Dtasv
021800130124     c                   PARM                    Dtasdta
021900060914
022100130124
022200050607     c                   select
022300060914     c                   when      taserr = *blanks
022400120113      *----------------------------------------------------------------*
022500090216      * se si tratta di confronto fatturazione
022600090216      * cerco la varia "f" (supplemento carburante)
022700090216      * se c'è la tolgo dall'imponibile e dalle varie
022800090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
022900090216      * e la varia "R" AC PLUS assicurazione
023000120113      *----------------------------------------------------------------*
023100100111      * DAL 2010 TOLGO LA VARIA h PER IL SERVIZIO 10:30 E
023200100111      * SUPPLEMENTO CARBURANTE FEDEX MENTRE RIPRISTINO
023300100111      * IL LASCIATO AVVISO
023400120113      *----------------------------------------------------------------*
023500120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
023600120113      * "R" ac plus
023700120113      * ZTL
023800120113      * Ripristino varia "h" servizio 10:30
023900120113      * come indicato da mail di DE LUCA del 11/01/2012
024000120113      *----------------------------------------------------------------*
024100050607     c                   exsr      SR_riewf
024200130218
024700080714
024800070207     c                   add       1             tottas
024900130111     c                   write     wfcft000
025000050607     c                   endsl
025001140411
025002140411     c                   clear                   taserr
025100130124
025200050607     c     e_sf20        endsr
025300050607      ****************************************************
025400050607      ** Carica importi varie da tita7000
025500050607      ****************************************************
025600050607     c     Carta7        Begsr
025700050607      * pulisco le schiere delle varie
025800050607     c                   Do        20            k
025900050607     c                   If        k > 3
026000050607     c                   clear                   VA(K)
026100050607     c                   clear                   SV(K)
026200050607     c                   Endif
026300050607     c                   Enddo
026400060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
026500050607     c                   clear                   flgta7
026600050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
026700050607     c                   If        tassv3 <> *blanks
026800050607      * mi posiziono su tita7
026900050607     c     Kta7          setll     tita730c
027000050607     c                   if        %equal(tita730c)
027100050607     c                   z-add     3             k
027200050607      **
027300050607     c                   do        *hival
027400050607     c     Kta7          reade     tita730c
027500050607      * fine file
027600050607     c                   if        %eof(tita730c)
027700050607     c                   leave
027800050607     c                   endif
027900050607      * carico le varie
028000050607     c                   add       1             k
028100050607     c                   movel     ta7svn        sv(k)
028200050607     c                   z-add     ta7van        va(k)
028300050607     c                   z-add     1             flgta7
028400050607     c                   enddo
028500050607
028600050607     c                   endif
028700050607
028800050607     c                   endif
028900050607
029000050607     c                   Endsr
029100060914      *
029200060914      *****************************************************************
029300060914      *  Recupero importi di tassazione dal file titai
029400060914      *****************************************************************
029500060914     c     Ric_tai       Begsr
029600060914      *
029700060914     c                   z-add     0             k
029800060914
029900060914     c     ktai05        setll     titai01l
030000060914
030100060914     c                   If        %equal(titai01l)
030200060914
030300060914     c                   do        *hival
030400060914
030500060914     c     ktai05        reade     titai01l
030600060914
030700060914     c                   If        %eof(titai01l)
030800060914     c                   leave
030900060914     c                   endif
031000060914      * imponibile
031100060914     c                   if        taisvt = '£'
031200060914     c                   eval      tasimv = taivat
031300060914     c                   endif
031400060914      * porto
031500060914     c                   if        taisvt = ' '
031600060914     c                   eval      taspor = taivat
031700060914     c                   endif
031800060914      * varie
031900060914     c                   if        taisvt <> ' ' and taisvt <> '£'
032000060914     c                   add       1             k
032100060914     c                   eval      sv(k) = taisvt
032200060914     c                   eval      va(k) = taivat
032300060914     c                   endif
032400060914
032500060914     c                   enddo
032600060914
032700060914     c                   else
032800060914      * se non è pretassata pulisco la divisa
032900060914     c                   clear                   tasdiv
033000060914     c                   endif
033100060914
033200060914     c                   endsr
033300050607      ****************************************************
033400050609      ** Ricerca Contrassegno
033500050607      ****************************************************
033600050607     c     Carcsb        Begsr
033700050607
033800050607     c     Kcsb          chain     Tncsb03l
033900050607     c                   If        %found(tncsb03l) and
034000050607      * solo per stesso tipo bolla
034100050607     c                             tastbl = csbtbl
034200050607     c                   movel     csbsta        Tassta
034300050607     c                   z-add     csbcas        Tascas
034400061016     c****               movel     csbtpa        Tastic
034500050607     c                   if        csbfus <> *blanks
034600050607     c                   move      csbfus        tastic
034700050607     c                   else
034800050607     c                   move      csbtpi        tastic
034900050607     c                   end
035000050607      * mittente
035100050607     c                   movel     csbvca        tasvca
035200050607     c                   z-add     csbcmb        tascmb
035300050607
035400050607     c                   endif
035500050607
035600050607     c                   endsr
035700050607      *************************************************************************
035800060914      * scrittura w-file dati tassazione corrente
035900050607      *************************************************************************
036000060914     C     sr_corrente   BEGSR
036010140408     c                   clear                   wfcft000
036011140408     c                   z-add     tasaas        CFTAAS
036012140408     c                   z-add     taslnp        CFTLNP
036013140408     c                   z-add     tasnrs        CFTNRS
036014140408     c                   z-add     tasnsp        CFTNSP
036015140408     c                   z-add     tasksc        CFTKSC
036016140408     c                   z-add     tasctr        CFTCTR
036017140407      * se si tratta di spedizione già fatturata verifico se devo ricalcolare con una
036018140407      * nuova tariffa
036019140410    Ac                   if        pardtc = 0 and tasimv > 0
036026140408     c                   z-add     tasimv        CFTIMVC
036027140408     c                   z-add     taspor        CFTPORC
036028140408      * carico eventuali varie presenti in TITA7
036029140408     c                   Exsr      Carta7
036030140408
036031140408      * cerco la varia "D" (diritto di fatturazione)
036032140408      * se c'è la tolgo dall'imponibile e dalle varie
036033140408     c                   z-add     1             k
036034140408     c     'D'           lookup    sv(k)                                  30
036035140408     c                   if        %found
036036140408     c                   eval      cftimvc = cftimvc - va(k)
036037140408     c                   clear                   va(k)
036038140408     c                   clear                   sv(k)
036039140408     c                   endif
036041140411      * mi salvo le varie per ricalcolo istat e fuel
036042140411     c                   movea     sv            sav_sv
036043140411     c                   movea     va            sav_va
036044140411      * MI SDALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
036045140409     c                   z-add     1             k
036046140409     c     'f'           lookup    sv(k)                                  30
036047140409     c                   if        %found
036048140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
036049140409     c                   eval      cftfuelc = va(k)
036050140409     c                   endif
036051140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
036052140409     c                   z-add     1             k
036053140409     c     'L'           lookup    sv(k)                                  30
036054140409     c                   if        %found
036055140409     c                   eval      cftistatc = va(k)
036056140409     c                   endif
036057140408      * "R" AC PLUS da togliere dall'imponibile
036058140408     c                   z-add     1             k
036059140408     c     'R'           lookup    sv(k)                                  30
036060140408     c                   if        %found
036061140408     c                   eval      cftimvc = cftimvc - va(k)
036062140408     c                   clear                   va(k)
036063140408     c                   clear                   sv(k)
036064140408     c                   endif
036065140408      * valorizzo le varie della tassazione corrente
036066140408     c                   movea     va            vacor
036067140408     c                   movea     sv            svcor
036068140408     c
036069140410    Ac                   Endif
036070140408
036071140408      * se imponibile non valorizzato lo calcolo o con la data passata a video o con la data
036072140408      * spedizione
036073140408
036074140410    Bc                   If        Pardtc > 0 or tasimv = 0
036075140408      * se imponibile valorizzato devo ricalcolare tutto con la nuova data tariffa corrente
036076140408      * quindi pulisco varie imponibile e porto
036077140408
036078140408     c                   If        tasimv > 0
036079140408
036080140408      * Pulisco i campi dell'assicurazione se calcolata in fattura
036081140408    2c                   IF        Tasfcm = 'F'
036082140408     c                   clear                   tasias
036083140408     c                   clear                   tasvas
036084140408    2c                   Endif
036085140408      *
036086140408      * valorizzo la DS DTAS e DTASV
036087140408     c                   clear                   dtasv
036088140407     c                   clear                   tasimv
036089140408     c                   clear                   taspor
036090140407      *
036091140408      * cerco in TITAI l'immagine della spedizione prima della tassazione  compreso l'imponibile
036092140407     c                   exsr      RIC_tai
036093140408     c                   endif
036094140407      ** Ricerca contrassegno
036095140407    2c                   IF        Tasfca <> *blanks
036096140407     c                   Exsr      Carcsb
036097140407    2c                   Endif
036098140407
036099140407     c                   movel     Tasftc        tastc1
036100140407
036101140408     c                   movel(p)  tasaas        tasdsp
036102140408     c                   move      tasmgs        tasdsp
036103140408     c                   Z-add     tasdsp        tasdct
036104140407
036105140407      * Flag SI NO DDT
036106140407    2C                   If        tasll1 = 'C' or tasll1 = 'S'
036107140407     C                   movel     'S'           Tasddt
036108140407   x2C                   Else
036109140407     C                   Clear                   Tasddt
036110140407    2C                   Endif
036111140407     c                   clear                   dtas01
036112140407     c                   movel     tasfbr        §asfbr
036113140407     c                   movel     tascca        §ascca
036114140407      * valorizzo flag email al destinatario
036115140407     c                   movel     tasflo        dtasflo
036116140407     c                   move      §floemd       §asemd
036117140407
036118140408      * se data tariffa corrente diversa da zero passo nel flo la data tariffa corrente
036119140408     c                   If        pardtc > 0
036120140408     c                   move      pardtc        §asdrt
036121140408     c                   Z-add     pardtc        tasdct
036122140407     c                   endif
036123140407
036124140408     c                   eval      §tasflo = dtas01
036125140407
036126140407      * Bancali
036127140407    2c                   If        %Subst(TasGva:1:1) = 'B'
036128140407
036129140407     c                   eval      kAr5Trd  = 'BAN'
036130140407     c     kFiar5        Chain     Fiar531c
036131140407If  3c                   If        %Found(Fiar531c)
036132140407     c                   Movel     Ar5Uni        dAr5Ban
036133140407    3c                   EndIf
036134140407      * Bancali
036135140407     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
036136140407    2c                   EndIf
036137140407      **
036138140407      * Colli Originali
036139140407    2c                   If        %Subst(TasGva:1:1) = 'O'
036140140407
036141140407     c                   eval      kAr5Trd  = 'BNB'
036142140407     c     kFiar5        Chain     Fiar531c
036143140407If  3c                   If        %Found(Fiar531c)
036144140407     c                   Movel     Ar5Uni        dAr5Bnb
036145140407    3c                   EndIf
036146140407      * Colli
036147140407     c                   Eval      TasNcl = §Ar5bcor
036148140407    2c                   EndIf
036181140407      *
036182140407      * verifico se c'è evento
036183140407     c     Kevb4         chain     Fnevb04l
036184140407
036185140408    2c                   If        %found(Fnevb04l)
036186140407     c                   eval      tasric = 'S'
036187140408    2c                   endif
036188140407
036189140407      **
036190140409     c                   clear                   dtasdta
036191140409      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
036192140409      * calcolo fuel
036193140409     c                   If        tasdft = 0
036194140409     c                   movel(p)  tasaas        tasdtfat
036195140409     c                   move      tasmgs        tasdtfat
036196140409     c                   else
036197140409     c                   z-add     tasdft        tasdtfat
036198140409     c                   endif
036199140409
036200140407      **
036201140409     c                   call      'TNSF21R'
036202140407     c                   parm                    Kpjba
036203140407     c                   parm                    Dtas
036204140407     c                   parm                    Dtasv
036205140409     c                   parm                    dtasdta
036206140409
036207140409     c                   if        taserr <> ' '
036208140409     c                   leavesr
036209140409     c                   endif
036210140409
036232140407
036233140407      * tassazione
036234140407
036800060914     c                   z-add     tasimv        CFTIMVC
036900060914     c                   z-add     taspor        CFTPORC
036901140411      * mi salvo le varie per ricalcolo istat e fuel
036902140411     c                   movea     sv            sav_sv
036903140411     c                   movea     va            sav_va
036904140409      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
036905140409     c                   z-add     1             k
036906140409     c     'f'           lookup    sv(k)                                  30
036907140409     c                   if        %found
036908140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
036909140409     c                   eval      cftfuelc = va(k)
036910140409     c                   endif
036911140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
036912140409     c                   z-add     1             k
036913140409     c     'L'           lookup    sv(k)                                  30
036914140409     c                   if        %found
036915140409     c                   eval      cftistatc = va(k)
036916140409     c                   endif
037200060914
042000130124      * "R" AC PLUS da togliere dall'imponibile
042100090216     c                   z-add     1             k
042200090216     c     'R'           lookup    sv(k)                                  30
042300090216     c                   if        %found
042400090216     c                   eval      cftimvc = cftimvc - va(k)
042500130124     c                   clear                   va(k)
042600130124     c                   clear                   sv(k)
042700130124     c                   endif
042800130124      * valorizzo le varie della tassazione corrente
042900130124     c                   movea     va            vacor
043000130124     c                   movea     sv            svcor
043001140410    1c                   endif
043002140411      * reimposto le varie come da fatturazione
043003140411     c                   movea     sav_va        va
043004140411     c                   movea     sav_sv        sv
043005140411      * ricalcolo ISTAT e FUEL
043006140408
043007140408     c                   exsr      car_sf21ds
043008140408
043700050607     c                   endsr
043800130124      **---------------------------------------------------------------**
043900130124      ** Carico la ds per le ritassazioni e le eseguo per ISTAT e FUEL **
044000130124      **---------------------------------------------------------------**
044100130124     c     car_sf21ds    BEGSR
044200130124      *
044300130124     c                   movel     *Blanks       tastla
044400130124     c                   clear                   dAr5Ban
044500130124     c                   Clear                   dAr5Bnb
044600130124     c                   Clear                   kpjbu
044700130124     c                   clear                   dtasdta
044800130124
044900130124     c                   movel     Tasftc        tastc1
045000130124      * Flag SI NO DDT
045100130124    1C                   If        tasll1 = 'C' or tasll1 = 'S'
045200130124     C                   movel     'S'           Tasddt
045300130124   x1C                   Else
045400130124     C                   Clear                   Tasddt
045500130124    1C                   Endif
045600130124     c                   clear                   dtas01
045700130124     c                   movel     tasfbr        §asfbr
045800130124     c                   movel     tascca        §ascca
045900130124      * valorizzo flag email al destinatario
046000130124     c                   movel     tasflo        dtasflo
046100130124     c                   move      §floemd       §asemd
046200130124
046300130124     c                   eval      §tasflo = dtas01
046400130124
046500130124     c
046600130124     c                   clear                   tasban
046700130124      * Bancali
046800130124    1c                   If        %Subst(TasGva:1:1) = 'B'
046900130124
047000130124     c                   eval      kAr5Trd  = 'BAN'
047100130124     c     kFiar5        Chain     Fiar531c
047200130124If  1c                   If        %Found(Fiar531c)
047300130124     c                   Movel     Ar5Uni        dAr5Ban
047400130124    1c                   EndIf
047500130124      * Bancali
047600130124     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
047700130124    1c                   EndIf
047800130124      **
047900130124      * Colli Originali
048000130124    1c                   If        %Subst(TasGva:1:1) = 'O'
048100130124     c                   eval      kAr5Trd  = 'BNB'
048200130124     c     kFiar5        Chain     Fiar531c
048300130124If  1c                   If        %Found(Fiar531c)
048400130124     c                   Movel     Ar5Uni        dAr5Bnb
048500130124    1c                   EndIf
048600130124      * Colli
048700130124     c                   Eval      TasNcl = §Ar5bcor
048800130124    1c                   EndIf
048900130124
049000130124     c                   clear                   tascas
049100130124     c                   clear                   tastic
049200130124     c                   clear                   tasvca
049300130124     c                   clear                   tascmb
049400130124      * C/Assegno
049500130124    2c                   IF        Tasfca <> *blanks
049600130124     c                   Exsr      Carcsb
049700130124    2c                   Endif
049701140409
050500130124     c     Kevb4         chain     Fnevb04l
050600130124
050700130124     c                   If        %found(Fnevb04l)
050800130124     c                   eval      tasric = 'S'
050900130124     c                   endif
051000130124
051100130124      * Assicurazione
051200130124    2c                   IF        Tasfcm = 'F'
051300130124     c                   clear                   tasias
051400130124     c                   clear                   tasvas
051500130124    2c                   Endif
051600130124
051700130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
051800130124      * tariffa con data tariffe
051900130124     c                   z-add     pardrf        tasdsp
052000130124     c                   z-add     pardrf        tasdct
052100130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
052200130124      * calcolo istat
052300130124     c                   movel(p)  tasaas        tasdtist
052400130124     c                   move      tasmgs        tasdtist
052401140207      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
052402140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
052403140409     C                   IF        TASDFT > 0
052404140207     c                   z-add     tasdft        tasdtfat
052405140409     C                   ELSE
052406140409     c                   movel(p)  tasaas        tasdtfat
052407140409     c                   move      tasmgs        tasdtfat
052409140409     C                   ENDIF
052500130129      * verifico se fuel pretassato dalla filiale
052600130129     c                   eval      ksvt = 'L'
052700130129     c     Ktai06        chain     titai01l
052800130129      * se non trovato record ricalcolo con stessa base imponibile
052900130129     c                   If        not %found(titai01l)
053000130129
053100130124
053200130124      * pulisco ISTAT della fatturazione
053300130124     c                   z-add     1             k
053400130124     c     'L'           lookup    sv(k)                                  30
053500130124     c                   if        %found
053600130124     c                   clear                   va(k)
053700130124     c                   clear                   sv(k)
053800130124     c                   endif
053900130124      * pulisco imponibile
054000130124     c                   clear                   tasimv
054100130124      * imposto la  varia da calcolare
054200130124     c                   eval      tassva = 'L'
054300140409     c                   call      'TNSF21R'
054400130124     c                   PARM                    KPJBA
054500130124     c                   PARM                    DTAS
054600130124     c                   PARM                    Dtasv
054700130124     c                   PARM                    Dtasdta
054800130129     c                   endif
054900130124
055000130124      * valorizzo la varia ISTAT ricalcolata
055100130124     c                   z-add     1             k
055200130124     c     'L'           lookup    sv(k)                                  30
055300130124     c                   if        %found
055400130125     c                   eval      cftistatr = va(k)
055500130124     c                   endif
055600130125      * valorizzo imponibile varia ISTAT ricalcolata
055700130125     c                   eval      cftimpist = tasimv - cftistatr
055800130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
055900130124      * calcolo fuel
056000130124     c                   clear                   tasdtist
056100130124     c                   movel(p)  tasaas        tasdtfue
056200130124     c                   move      tasmgs        tasdtfue
056203140409      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
056204140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
056205140409     C                   IF        TASDFT > 0
056206140409     c                   z-add     tasdft        tasdtfat
056207140409     C                   ELSE
056208140409     c                   movel(p)  tasaas        tasdtfue
056209140409     c                   move      tasmgs        tasdtfue
056211140409     C                   ENDIF
056300130129      * verifico se fuel pretassato dalla filiale
056400130129     c                   eval      ksvt = 'f'
056500130129     c     Ktai06        chain     titai01l
056600130129      * se non trovato record ricalcolo con stessa base imponibile
056700130129     c                   If        not %found(titai01l)
056800130124
056900130124      * pulisco FUEL  della fatturazione
057000130124     c                   z-add     1             k
057100130124     c     'f'           lookup    sv(k)                                  30
057200130124     c                   if        %found
057300130124     c                   clear                   va(k)
057400130124     c                   clear                   sv(k)
057500130124     c                   endif
057600130124      * pulisco imponibile
057700130124     c                   clear                   tasimv
057800130124      * imposto la  varia da calcolare
057900130124     c                   eval      tassva = 'f'
058000140409     c                   call      'TNSF21R'
058100130124     c                   PARM                    KPJBA
058200130124     c                   PARM                    DTAS
058300130124     c                   PARM                    Dtasv
058400130124     c                   PARM                    Dtasdta
058500130129     c                   endif
058600130124
058700130124     c                   z-add     1             k
058800130124     c     'f'           lookup    sv(k)                                  30
058900130124     c                   if        %found
059000130125     c                   eval      cftfuelr = va(k)
059100130124     c                   endif
059200130125      * valorizzo imponibile varia FUEL  ricalcolata
059300130125     c                   eval      cftimpfue= taspor
059400130125      * inoltro
059500130125     c                   z-add     1             k
059600130125     c     '2'           lookup    sv(k)                                  30
059700130125     c                   if        %found
059800130125     c                   eval      cftimpfue= cftimpfue + va(k)
059900130125     c                   endif
060000130125      * isola
060100130125     c                   z-add     1             k
060200130125     c     'J'           lookup    sv(k)                                  30
060300130125     c                   if        %found
060400130125     c                   eval      cftimpfue= cftimpfue + va(k)
060500130125     c                   endif
060600130125      * località disagiata
060700130125     c                   z-add     1             k
060800130125     c     'K'           lookup    sv(k)                                  30
060900130125     c                   if        %found
061000130125     c                   eval      cftimpfue= cftimpfue + va(k)
061100130125     c                   endif
061200130125      * centro storico
061300130125     c                   z-add     1             k
061400130125     c     'Q'           lookup    sv(k)                                  30
061500130125     c                   if        %found
061600130125     c                   eval      cftimpfue= cftimpfue + va(k)
061700130125     c                   endif
061800130125      * espresso/priority
061900130125     c                   z-add     1             k
062000130125     c     'e'           lookup    sv(k)                                  30
062100130125     c                   if        %found
062200130125     c                   eval      cftimpfue= cftimpfue + va(k)
062300130125     c                   endif
062301140207      * Se la data fattura è < o uguale la 30/01/2014 non sommo queste varie aggiuntive
062302140409     c                   if        tasdft > 20140130 or
062303140409     c                             tasdft = 0
062400130125      * h 10,30
062500130125     c                   z-add     1             k
062600130125     c     'h'           lookup    sv(k)                                  30
062700130125     c                   if        %found
062800130125     c                   eval      cftimpfue= cftimpfue + va(k)
062900130125     c                   endif
062901140207      * = reso bancali
062902140207     c                   z-add     1             k
062903140207     c     '='           lookup    sv(k)                                  30
062904140207     c                   if        %found
062905140207     c                   eval      cftimpfue= cftimpfue + va(k)
062906140207     c                   endif
062907140207      * c lasciato avviso
062908140207     c                   z-add     1             k
062909140207     c     'c'           lookup    sv(k)                                  30
062910140207     c                   if        %found
062911140207     c                   eval      cftimpfue= cftimpfue + va(k)
062912140207     c                   endif
062913140207      * A appuntamento
062914140207     c                   z-add     1             k
062915140207     c     'A'           lookup    sv(k)                                  30
062916140207     c                   if        %found
062917140207     c                   eval      cftimpfue= cftimpfue + va(k)
062918140207     c                   endif
062919140207      * B consegna DDT
062920140207     c                   z-add     1             k
062921140207     c     'B'           lookup    sv(k)                                  30
062922140207     c                   if        %found
062923140207     c                   eval      cftimpfue= cftimpfue + va(k)
062924140207     c                   endif
062925140207      * C facch. arr.
062926140207     c                   z-add     1             k
062927140207     c     'C'           lookup    sv(k)                                  30
062928140207     c                   if        %found
062929140207     c                   eval      cftimpfue= cftimpfue + va(k)
062930140207     c                   endif
062931140207      * F fuori misura
062932140207     c                   z-add     1             k
062933140207     c     'F'           lookup    sv(k)                                  30
062934140207     c                   if        %found
062935140207     c                   eval      cftimpfue= cftimpfue + va(k)
062936140207     c                   endif
062937140207      * H diritto fisso
062938140207     c                   z-add     1             k
062939140207     c     'H'           lookup    sv(k)                                  30
062940140207     c                   if        %found
062941140207     c                   eval      cftimpfue= cftimpfue + va(k)
062942140207     c                   endif
062943140207      * I spese di giacenza
062944140207     c                   z-add     1             k
062945140207     c     'I'           lookup    sv(k)                                  30
062946140207     c                   if        %found
062947140207     c                   eval      cftimpfue= cftimpfue + va(k)
062948140207     c                   endif
062949140207      * N anteporto
062950140207     c                   z-add     1             k
062951140207     c     'N'           lookup    sv(k)                                  30
062952140207     c                   if        %found
062953140207     c                   eval      cftimpfue= cftimpfue + va(k)
062954140207     c                   endif
062955140207      * P ai piani
062956140207     c                   z-add     1             k
062957140207     c     'P'           lookup    sv(k)                                  30
062958140207     c                   if        %found
062959140207     c                   eval      cftimpfue= cftimpfue + va(k)
062960140207     c                   endif
062961140207      * S supermercato
062962140207     c                   z-add     1             k
062963140207     c     'S'           lookup    sv(k)                                  30
062964140207     c                   if        %found
062965140207     c                   eval      cftimpfue= cftimpfue + va(k)
062966140207     c                   endif
062967140207      * U ritiro
062968140207     c                   z-add     1             k
062969140207     c     'U'           lookup    sv(k)                                  30
062970140207     c                   if        %found
062971140207     c                   eval      cftimpfue= cftimpfue + va(k)
062972140207     c                   endif
062973140207      * X consegne disagiate
062974140207     c                   z-add     1             k
062975140207     c     'X'           lookup    sv(k)                                  30
062976140207     c                   if        %found
062977140207     c                   eval      cftimpfue= cftimpfue + va(k)
062978140207     c                   endif
062979140207
062980140207     c                   endif
063000130124
063100130124     c                   endsr
063200050607      *************************************************************************
063300060914      * scrittura w-file dati tassazione precdeente
063400050607      *************************************************************************
063500060914     C     sr_riewf      BEGSR
063600061218
063700061218      * cerco la varia "f" (supplemento carburante)
063800061218      * se c'è la tolgo dall'imponibile e dalle varie
063900090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
064000090216      * e la varia "R" AC PLUS assicurazione
064100100108      * dal 2010 togliamo anche la varia "+" supplemento carburante fedex
064200100111      * e il servizio 10,30 "h" mentre ripristiniamo il lasciato
064300100111      * avviso
064400120113      *----------------------------------------------------------------*
064500120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
064600120113      * "R" ac plus
064700120113      * ZTL
064800120113      * Ripristino varia "h" servizio 10:30
064900120113      * come indicato da mail di DE LUCA del 11/01/2012
065000120113      *----------------------------------------------------------------*
065100130124      *----------------------------------------------------------------*
065200130124      * dal 2013 non escludo le varie Fuel e suppl. carburante fedex
065300130124      * perchè vengo calcolate in modo tale da mantenere la ricerca
065400130124      * del prezzo base in base alla data spedizione reale
065500130124      * escludo sempre la varia "R" ac plus
065600130124      * come da nuovo progetto 810 Daniele
065700130124      *----------------------------------------------------------------*
065800061218     c                   z-add     1             k
065900090216      * "R" AC PLUS
066000090216     c                   z-add     1             k
066100090216     c     'R'           lookup    sv(k)                                  30
066200090216     c                   if        %found
066300090216     c                   eval      tasimv  = tasimv - va(k)
066400090216     c                   clear                   va(k)
066500090216     c                   clear                   sv(k)
066600090216     c                   endif
066700061218
066800060914     c                   z-add     tasimv        CFTIMVP
066900060914     c                   z-add     taspor        CFTPORP
067000060914     c                   movea     va            vapre
067100060914     c                   movea     sv            svpre
067200050607     c                   endsr
067300130218
069000050609      *************************************************************************
069100050609      * Operazioni iniziali
069200050609      *************************************************************************
069300050609     C     *inzsr        BEGSR
069400050609     c*
069401140408     c     ktas          klist
069402140408     c                   kfld                    paraas
069403140408     c                   kfld                    parmgs
069800060914
069900060914     c     ktai05        klist
070000060914     c                   kfld                    tasaas
070100060914     c                   kfld                    taslnp
070200060914     c                   kfld                    tasnrs
070300060914     c                   kfld                    tasnsp
070400060914     c                   kfld                    tastbl
070500060914
070600060914     c     ktai06        klist
070700060914     c                   kfld                    tasaas
070800060914     c                   kfld                    taslnp
070900060914     c                   kfld                    tasnrs
071000060914     c                   kfld                    tasnsp
071100060914     c                   kfld                    tastbl
071200060914     c                   kfld                    Ksvt
071300060914
071400050609     c     kta7          klist
071500050609     c                   kfld                    tasaas
071600050609     c                   kfld                    taslnp
071700050609     c                   kfld                    tasnrs
071800050609     c                   kfld                    tasnsp
071900050609     c                   kfld                    tastbl
072000050609
072100050609     c     kFiar5        Klist
072200050609     c                   Kfld                    TasAas
072300050609     c                   Kfld                    TasLnp
072400050609     c                   Kfld                    TasNrs
072500050609     c                   Kfld                    TasNsp
072600050609     c                   Kfld                    kAr5Trd
072700050609
072800050609     c     KCSB          KLIST
072900050609     c                   KFLD                    TASAAS
073000050609     c                   KFLD                    TASLNP
073100050609     c                   KFLD                    TASNRS
073200050609     c                   KFLD                    TASNSP
073300050609
073400050609     c     Kcft          KLIST
073500050609     c                   KFLD                    TASAAS
073600050609     c                   KFLD                    TASLNP
073700050609     c                   KFLD                    TASNRS
073800050609     c                   KFLD                    TASNSP
073900050609
074000050609     C     KTAB          KLIST
074100050609     C                   KFLD                    CODUT
074200050609     C                   KFLD                    KCOD
074300081105
074400081105      * Chiave file eventi FNEVB
074500081105     c     Kevb4         Klist
074600081105     c                   Kfld                    Tasaas
074700081105     c                   Kfld                    Taslnp
074800081105     c                   Kfld                    Tasnrs
074900081105     c                   Kfld                    Tasnsp
075000081105     c                   Kfld                    Wcev
075100081105      *
075200050609     c
075300050609     c     *entry        plist
075400050609     c                   parm                    kpjba
075500050609     c                   movel     kpjbu         param
075600060915
075700090430     c                   time                    wtime            14 0
075800090430     c                   move      wtime         wdata
075900090430     c                   movel     wtime         wora
076000060915      * caricamento schiera dei tipi bolla da scartare
076100060915     c                   eval      Kcod  = 'TB'
076200050609     c                   clear                   tbs
076300050609     c                   z-add     0             xx
076400050609     c     ktab          setll     tabel00f
076500050609     c                   do        *hival
076600050609     c     ktab          reade     tabel00f
076700050609     c                   if        %eof(tabel00f)
076800050609     c                   leave
076900050609     c                   endif
077000050609     c                   movel     tbluni        dstb
077100080715     c****               if        §tbrbl = 'R' or §tbfcb <> '1'
077200080715      * con richiesta di Bocchi includiamo nella ritassazione / confronto di fatturazione
077300080715      * anche le bolle di recupero  §tbrbl = 'R'
077400080715     c                   if        §tbfcb <> '1'
077500050609     c                   add       1             xx
077600050609     c                   movel     tblkey        tbs(xx)
077700050609     c                   endif
077800050609     c                   enddo
077900060915      * caricamento schiera dei codici bolla da scartare
078000060915     c                   eval      Kcod  = '3A'
078100060915     c                   z-add     0             xx
078200060915     c     ktab          setll     tabel00f
078300060915     c                   do        *hival
078400060915     c     ktab          reade     tabel00f
078500060915     c                   if        %eof(tabel00f)
078600060915     c                   leave
078700060915     c                   endif
078800060915     c                   movel     tbluni        ds3a
078900060915     c                   if        §3asva <> *blanks
079000060915     c                   add       1             xx
079100060915     c                   movel     tblkey        cbo(xx)
079200060915    3c                   endif
079300060915     c                   enddo
079301140409      * GIRO LE DATE PER LE STAMPE
079302140409     C                   move      PARDRF        G08INV
079303140410     c                   eval      G08err = '3'
079304140409     C                   CALL      'XSRDA8'
079305140409     C                   PARM                    WLBDA8
079306140409     C                   Z-ADD     G08DAT        WPARDRF
079307140409      *
079308140409     C                   move      PARDTC        G08INV
079309140410     c                   eval      G08err = '3'
079310140409     C                   CALL      'XSRDA8'
079311140409     C                   PARM                    WLBDA8
079312140409     C                   Z-ADD     G08DAT        WPARDTC
079313140409      *
079314140409     C                   move      PARDAL        G08INV
079315140410     c                   eval      G08err = '3'
079316140409     C                   CALL      'XSRDA8'
079317140409     C                   PARM                    WLBDA8
079318140409     C                   Z-ADD     G08DAT        WPARDAL
079319140409      *
079320140409     C                   move      PARAL         G08INV
079321140410     c                   eval      G08err = '3'
079322140409     C                   CALL      'XSRDA8'
079323140409     C                   PARM                    WLBDA8
079324140409     C                   Z-ADD     G08DAT        WPARAL
079400060915
079500050609     c                   endsr
