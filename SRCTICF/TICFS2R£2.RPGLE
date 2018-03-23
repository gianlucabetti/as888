000100040119      *===============================================================*
000200141218      * TicfS2r    * DDT Scartati da Confronto Fatturazione
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
001300141218     fwfcfs01l  uf a e           k disk
001400060914     Ftabel00f  if   e           k disk
001500141218     fticfs2p   o    e             printer oflind(*in90)
001600040119      *
001700050526     d kpjba         e ds
001800050609     d dstb          e ds
001900060915     d ds3a          e ds
002000130124
002100050607     d param           ds
002200140407     D  pardrf                 1      8  0
002300140407     D  pardtc                 9     16  0
002400140407     D  pardal                17     24  0
002500140408     D  paraas                17     20  0
002600140408     D  parmgs                21     24  0
002700140407     D  paral                 25     32  0
002800140409
002900140409     D WLBDA8          DS                  INZ
003000140409     D  G08DAT                 1      8  0
003100140409     D  G08INV                 9     16  0
003200140409     D  G08ERR                17     17
003300140409     D  G08TGI                18     22  0
003400140409
003500141218     D DTASFLO       E DS
003600141218     d dtas          e ds                  inz
003700141218     d  §tasflo      e                     extfld(tasflo)
003800050607     d
003900141218     dwfcfsxxds      e ds                  extname(wfcfs00f)
004000050607     dsvcor                  184    203    dim(20)
004100050607     dvacor                  204    323p 3 dim(20)
004200050607     d*
004300040331      *
004400040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
004500050526      *
004600040331      * - Indici di schiera
004700040331     d xx              s              3  0 inz
004800050607     D k               s              5  0                                      indice
004900040331      * - Campi di comodo
005000060914     d kmgs            s                   Like(TasMgs)
005100050607     D Flgta7          s              1  0
005200050609     d codut           s              1  0 Inz(1)
005300060915     D kcod            s                   like(tblcod)
005400060914     D ksvt            s                   like(taisvt)
005500070207     D totspe          s              9  0
005600070207     D tottas          s              9  0
005700130110     D totsmt          s              9  0
005800140408     d antepo          s                   like(tasimv)
005900141218     d wRCDok          s               n   inz(*off)
006000060914      *
006100060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
006200060914      *
006300060914      * codici bolla da scartare
006400060914     D tbs             s              2    dim(50)
006500060915      * codici bolla che prevedono la tassazione di una singola varia
006600060915     D cbo             s              2    dim(50)
006700130124      * sigle varie salvate
006800130124     D sav_sv          s              1    dim(20)
006900130124      * importo varie salvate
007000130124     D sav_va          s             11  3 dim(20)
007100060915      *
007200141218     d Dtasv         e ds
007300141218     d  sv                     1     20
007400141218     d                                     Dim(20)                              Sigle varie
007500141218     d  va                    21    140P 3
007600141218     d                                     Dim(20)                              Importi varie
007700040119
007800040119      *
007900140409      * mi posiziono con data iniziale periodo da elaborare
008000140408     c     Ktas          setll     titas38c
008100050607     c                   do        *hival
008200140407     c                   read      titas38c
008300060914     c                   if        %eof(titas38c)
008400050607     c                   leave
008500050607     c                   endif
008600141218
008700141218     c                   eval      wRCDok = *off
008800141219
008900141219      * solo le ficcate
009000141219     c                   IF        TASfic = *blanks
009100141219     c                   iter
009200141219     c                   ENDIF
009300141218
009400140407     c                   movel(p)  tasaas        tasdsp
009500140407     c                   move      tasmgs        tasdsp
009600140407      * vado a fine se data spedizione maggiore del limite finale di elaborazione
009700140407     c                   if        tasdsp > paral
009800060914     c                   leave
009900050614     c                   endif
010000060915
010100141218      * tengo  se si tratta di codice bolla che prevede solo una varia
010200060918     c                   z-add     1             kk                2 0
010300060915     c     tascbo        lookup    cbo(kk)                                05
010400141222     c                   if        *in05 and tasfiv > 0
010500141218     c                   eval      wRCDok = *on
010600060915     c                   endif
010700060915     c
010800060914
010900141218      * tengo le bolle che sono state fatturate in filiale
011000140408     c                   If        tasfiv <> 998 and tasfiv <> 996
011100140408     c                               and tasfiv > 0
011200141218     c                   eval      wRCDok = *on
011300060914     c                   endif
011400060914     c
011500060914
011600140407      * verifico se tassata fino all'imponibile in TITAI se già registrate su TITAI
011700060914     c                   eval      ksvt = '£'
011800060914     c     Ktai06        chain     titai01l
011900141218      * se trovato record imponibile  tengo la bolla
012000060914     c                   If        %found(titai01l)
012100141218     c                   eval      wRCDok = *on
012200060914     c                   endif
012300140409
012400141219      * verifico la presenza delle varie N e &
012500141218      * in TITAI per poter tenere la bolla
012600140409     c                   eval      ksvt = 'N'
012700140409     c     Ktai06        chain     titai01l
012800141218      * se trovato record imponibile  tengo la bolla
012900140409     c                   If        %found(titai01l)  and taivat = 88888888
013000141218     c                   eval      wRCDok = *on
013100140409     c                   endif
013200140409
013300140409     c                   eval      ksvt = '&'
013400140409     c     Ktai06        chain     titai01l
013500141218      * se trovato record imponibile  tengo la bolla
013600140409     c                   If        %found(titai01l)  and taivat = 88888888
013700141218     c                   eval      wRCDok = *on
013800140409     c                   endif
013900140409
014000140409      * carico le varie presenti
014100140409     c                   Exsr      Carta7
014200140409      *
014300140409      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
014400141218      * con VARIA 'N' valorizzata con 888888 la tengo
014500140409      *
014600140409    2c                   if        tasimv = 0 and tasfbr = 'S'
014700140409     c                   z-add     1             xx
014800140409     c     'N'           lookup    sv(xx)                                 30
014900140409    3c                   if        *in30 and va(xx) =  88888888
015000141218     c                   eval      wRCDok = *on
015100140409    3c                   endif
015200140409    2c                   endif
015300140409      *
015400141218      * verifico se è esiste la varia & con importo uguale a 8888888 la tengo
015500140409      *
015600140409    2c                   if        tasimv = 0
015700140409     c                   z-add     1             xx
015800140409     c     '&'           lookup    sv(xx)                                 30
015900140409    3c                   if        *in30 and va(xx) =  88888888
016000141218     c                   eval      wRCDok = *on
016100140409    3c                   endif
016200140409    2c                   endif
016300141218
016400141218      * se NON è uno dei rcd scartati torno a leggere
016500141218     c                   IF        not wRCDok
016600141218     c                   iter
016700141218     c                   ENDIF
016800060914
016900141218      * carico la tassazione corrente
017000060914     c                   exsr      sr_corrente
017100060914
017200141218      * SOMMO IL NUMERO DI SPEDIZIONE CHE SONO STATE  PRESE IN CONSIDEArZIONE
017300140411     c                   add       1             totspe
017400141218     c                   write     wfcfs000
017500060914
017600050607     c                   enddo
017700050526      *
017800070207      * stampo  la fine elaborazione
017900120213     c                   write     cf92s1
018000070207
018100040119     c                   eval      *inLR = *on
018200141218
018300050614     c                   movel(p)  param         kpjbu
018400040331      *
018500050607      ****************************************************
018600050607      ** Carica importi varie da tita7000
018700050607      ****************************************************
018800050607     c     Carta7        Begsr
018900050607      * pulisco le schiere delle varie
019000050607     c                   Do        20            k
019100050607     c                   If        k > 3
019200050607     c                   clear                   VA(K)
019300050607     c                   clear                   SV(K)
019400050607     c                   Endif
019500050607     c                   Enddo
019600060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
019700050607     c                   clear                   flgta7
019800050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
019900050607     c                   If        tassv3 <> *blanks
020000050607      * mi posiziono su tita7
020100050607     c     Kta7          setll     tita730c
020200050607     c                   if        %equal(tita730c)
020300050607     c                   z-add     3             k
020400050607      **
020500050607     c                   do        *hival
020600050607     c     Kta7          reade     tita730c
020700050607      * fine file
020800050607     c                   if        %eof(tita730c)
020900050607     c                   leave
021000050607     c                   endif
021100050607      * carico le varie
021200050607     c                   add       1             k
021300050607     c                   movel     ta7svn        sv(k)
021400050607     c                   z-add     ta7van        va(k)
021500050607     c                   z-add     1             flgta7
021600050607     c                   enddo
021700050607
021800050607     c                   endif
021900050607
022000050607     c                   endif
022100050607
022200050607     c                   Endsr
022300050607      *************************************************************************
022400060914      * scrittura w-file dati tassazione corrente
022500050607      *************************************************************************
022600060914     C     sr_corrente   BEGSR
022700141218
022800141218     c                   clear                   wfcfs000
022900141218     c                   z-add     tasaas        CFSAAS
023000141218     c                   z-add     taslnp        CFSLNP
023100141218     c                   z-add     tasnrs        CFSNRS
023200141218     c                   z-add     tasnsp        CFSNSP
023300141218     c                   z-add     tasksc        CFSKSC
023400141218     c                   z-add     tasctr        CFSCTR
023500140407      * se si tratta di spedizione già fatturata verifico se devo ricalcolare con una
023600140407      * nuova tariffa
023700140410    Ac                   if        pardtc = 0 and tasimv > 0
023800141218     c                   z-add     tasimv        CFSIMVC
023900141218     c                   z-add     taspor        CFSPORC
024000140408      * carico eventuali varie presenti in TITA7
024100140408     c                   Exsr      Carta7
024200140408
024300140408      * cerco la varia "D" (diritto di fatturazione)
024400140408      * se c'è la tolgo dall'imponibile e dalle varie
024500140408     c                   z-add     1             k
024600140408     c     'D'           lookup    sv(k)                                  30
024700140408     c                   if        %found
024800141218     c                   eval      CFSimvc = CFSimvc - va(k)
024900140408     c                   clear                   va(k)
025000140408     c                   clear                   sv(k)
025100140408     c                   endif
025200140411      * mi salvo le varie per ricalcolo istat e fuel
025300140411     c                   movea     sv            sav_sv
025400140411     c                   movea     va            sav_va
025500140411      * MI SDALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
025600140409     c                   z-add     1             k
025700140409     c     'f'           lookup    sv(k)                                  30
025800140409     c                   if        %found
025900140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
026000141218     c                   eval      CFSfuelc = va(k)
026100140409     c                   endif
026200140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
026300140409     c                   z-add     1             k
026400140409     c     'L'           lookup    sv(k)                                  30
026500140409     c                   if        %found
026600141218     c                   eval      CFSistatc = va(k)
026700140409     c                   endif
026800140408      * "R" AC PLUS da togliere dall'imponibile
026900140408     c                   z-add     1             k
027000140408     c     'R'           lookup    sv(k)                                  30
027100140408     c                   if        %found
027200141218     c                   eval      CFSimvc = CFSimvc - va(k)
027300140408     c                   clear                   va(k)
027400140408     c                   clear                   sv(k)
027500140408     c                   endif
027600140408      * valorizzo le varie della tassazione corrente
027700140408     c                   movea     va            vacor
027800140408     c                   movea     sv            svcor
027900140408     c
028000140410    Ac                   Endif
028100140408
028200050607     c                   endsr
028300130218
028400050609      *************************************************************************
028500050609      * Operazioni iniziali
028600050609      *************************************************************************
028700050609     C     *inzsr        BEGSR
028800050609     c*
028900140408     c     ktas          klist
029000140408     c                   kfld                    paraas
029100140408     c                   kfld                    parmgs
029200060914
029300060914     c     ktai06        klist
029400060914     c                   kfld                    tasaas
029500060914     c                   kfld                    taslnp
029600060914     c                   kfld                    tasnrs
029700060914     c                   kfld                    tasnsp
029800060914     c                   kfld                    tastbl
029900060914     c                   kfld                    Ksvt
030000060914
030100050609     c     kta7          klist
030200050609     c                   kfld                    tasaas
030300050609     c                   kfld                    taslnp
030400050609     c                   kfld                    tasnrs
030500050609     c                   kfld                    tasnsp
030600050609     c                   kfld                    tastbl
030700050609
030800050609     C     KTAB          KLIST
030900050609     C                   KFLD                    CODUT
031000050609     C                   KFLD                    KCOD
031100081105      *
031200050609     c
031300050609     c     *entry        plist
031400050609     c                   parm                    kpjba
031500050609     c                   movel     kpjbu         param
031600060915
031700090430     c                   time                    wtime            14 0
031800090430     c                   move      wtime         wdata
031900090430     c                   movel     wtime         wora
032000060915      * caricamento schiera dei tipi bolla da scartare
032100060915     c                   eval      Kcod  = 'TB'
032200050609     c                   clear                   tbs
032300050609     c                   z-add     0             xx
032400050609     c     ktab          setll     tabel00f
032500050609     c                   do        *hival
032600050609     c     ktab          reade     tabel00f
032700050609     c                   if        %eof(tabel00f)
032800050609     c                   leave
032900050609     c                   endif
033000050609     c                   movel     tbluni        dstb
033100080715      * con richiesta di Bocchi includiamo nella ritassazione / confronto di fatturazione
033200080715      * anche le bolle di recupero  §tbrbl = 'R'
033300080715     c                   if        §tbfcb <> '1'
033400050609     c                   add       1             xx
033500050609     c                   movel     tblkey        tbs(xx)
033600050609     c                   endif
033700050609     c                   enddo
033800060915      * caricamento schiera dei codici bolla da scartare
033900060915     c                   eval      Kcod  = '3A'
034000060915     c                   z-add     0             xx
034100060915     c     ktab          setll     tabel00f
034200060915     c                   do        *hival
034300060915     c     ktab          reade     tabel00f
034400060915     c                   if        %eof(tabel00f)
034500060915     c                   leave
034600060915     c                   endif
034700060915     c                   movel     tbluni        ds3a
034800060915     c                   if        §3asva <> *blanks
034900060915     c                   add       1             xx
035000060915     c                   movel     tblkey        cbo(xx)
035100060915    3c                   endif
035200060915     c                   enddo
035300140409      * GIRO LE DATE PER LE STAMPE
035400140409     C                   move      PARDRF        G08INV
035500140410     c                   eval      G08err = '3'
035600140409     C                   CALL      'XSRDA8'
035700140409     C                   PARM                    WLBDA8
035800140409     C                   Z-ADD     G08DAT        WPARDRF
035900140409      *
036000140409     C                   move      PARDTC        G08INV
036100140410     c                   eval      G08err = '3'
036200140409     C                   CALL      'XSRDA8'
036300140409     C                   PARM                    WLBDA8
036400140409     C                   Z-ADD     G08DAT        WPARDTC
036500140409      *
036600140409     C                   move      PARDAL        G08INV
036700140410     c                   eval      G08err = '3'
036800140409     C                   CALL      'XSRDA8'
036900140409     C                   PARM                    WLBDA8
037000140409     C                   Z-ADD     G08DAT        WPARDAL
037100140409      *
037200140409     C                   move      PARAL         G08INV
037300140410     c                   eval      G08err = '3'
037400140409     C                   CALL      'XSRDA8'
037500140409     C                   PARM                    WLBDA8
037600140409     C                   Z-ADD     G08DAT        WPARAL
037700060915
037800050609     c                   endsr
