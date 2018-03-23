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
003500050607     d
003600141218     dwfcfsxxds      e ds                  extname(wfcfs00f)
003700050607     dsvcor                  184    203    dim(20)
003800050607     dvacor                  204    323p 3 dim(20)
003900050607     d*
004000040331      *
004100040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
004200050526      *
004300040331      * - Indici di schiera
004400040331     d xx              s              3  0 inz
004500050607     D k               s              5  0                                      indice
004600040331      * - Campi di comodo
004700060914     d kmgs            s                   Like(TasMgs)
004800050607     D Flgta7          s              1  0
004900050609     d codut           s              1  0 Inz(1)
005000060915     D kcod            s                   like(tblcod)
005100060914     D ksvt            s                   like(taisvt)
005200070207     D totspe          s              9  0
005300070207     D tottas          s              9  0
005400130110     D totsmt          s              9  0
005500140408     d antepo          s                   like(tasimv)
005600141218     d wRCDok          s               n   inz(*off)
005700060914      *
005800060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
005900060914      *
006000060914      * codici bolla da scartare
006100060914     D tbs             s              2    dim(50)
006200060915      * codici bolla che prevedono la tassazione di una singola varia
006300060915     D cbo             s              2    dim(50)
006400130124      * sigle varie salvate
006500130124     D sav_sv          s              1    dim(20)
006600130124      * importo varie salvate
006700130124     D sav_va          s             11  3 dim(20)
006800060915      *
006900141218     d Dtasv         e ds
007000141218     d  sv                     1     20
007100141218     d                                     Dim(20)                              Sigle varie
007200141218     d  va                    21    140P 3
007300141218     d                                     Dim(20)                              Importi varie
007400040119
007500040119      *
007600140409      * mi posiziono con data iniziale periodo da elaborare
007700140408     c     Ktas          setll     titas38c
007800050607     c                   do        *hival
007900140407     c                   read      titas38c
008000060914     c                   if        %eof(titas38c)
008100050607     c                   leave
008200050607     c                   endif
008300141218
008400141218     c                   eval      wRCDok = *off
008500141219
008600141219      * solo le ficcate
008700141219     c                   IF        TASfic = *blanks
008800141219     c                   iter
008900141219     c                   ENDIF
009000141218
009100151127     c                   movel(p)  tasaas        tasdsp            8 0
009200140407     c                   move      tasmgs        tasdsp
009300140407      * vado a fine se data spedizione maggiore del limite finale di elaborazione
009400140407     c                   if        tasdsp > paral
009500060914     c                   leave
009600050614     c                   endif
009700060915
009800141218      * tengo  se si tratta di codice bolla che prevede solo una varia
009900060918     c                   z-add     1             kk                2 0
010000060915     c     tascbo        lookup    cbo(kk)                                05
010100141222     c                   if        *in05 and tasfiv > 0
010200141218     c                   eval      wRCDok = *on
010300060915     c                   endif
010400060915     c
010500060914
010600141218      * tengo le bolle che sono state fatturate in filiale
010700140408     c                   If        tasfiv <> 998 and tasfiv <> 996
010800140408     c                               and tasfiv > 0
010900141218     c                   eval      wRCDok = *on
011000060914     c                   endif
011100060914     c
011200060914
011300140407      * verifico se tassata fino all'imponibile in TITAI se già registrate su TITAI
011400060914     c                   eval      ksvt = '£'
011500060914     c     Ktai06        chain     titai01l
011600141218      * se trovato record imponibile  tengo la bolla
011700060914     c                   If        %found(titai01l)
011800141218     c                   eval      wRCDok = *on
011900060914     c                   endif
012000140409
012100141219      * verifico la presenza delle varie N e &
012200141218      * in TITAI per poter tenere la bolla
012300140409     c                   eval      ksvt = 'N'
012400140409     c     Ktai06        chain     titai01l
012500141218      * se trovato record imponibile  tengo la bolla
012600140409     c                   If        %found(titai01l)  and taivat = 88888888
012700141218     c                   eval      wRCDok = *on
012800140409     c                   endif
012900140409
013000140409     c                   eval      ksvt = '&'
013100140409     c     Ktai06        chain     titai01l
013200141218      * se trovato record imponibile  tengo la bolla
013300140409     c                   If        %found(titai01l)  and taivat = 88888888
013400141218     c                   eval      wRCDok = *on
013500140409     c                   endif
013600140409
013700140409      * carico le varie presenti
013800140409     c                   Exsr      Carta7
013900140409      *
014000140409      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
014100141218      * con VARIA 'N' valorizzata con 888888 la tengo
014200140409      *
014300140409    2c                   if        tasimv = 0 and tasfbr = 'S'
014400140409     c                   z-add     1             xx
014500140409     c     'N'           lookup    sv(xx)                                 30
014600140409    3c                   if        *in30 and va(xx) =  88888888
014700141218     c                   eval      wRCDok = *on
014800140409    3c                   endif
014900140409    2c                   endif
015000140409      *
015100141218      * verifico se è esiste la varia & con importo uguale a 8888888 la tengo
015200140409      *
015300140409    2c                   if        tasimv = 0
015400140409     c                   z-add     1             xx
015500140409     c     '&'           lookup    sv(xx)                                 30
015600140409    3c                   if        *in30 and va(xx) =  88888888
015700141218     c                   eval      wRCDok = *on
015800140409    3c                   endif
015900140409    2c                   endif
016000141218
016100141218      * se NON è uno dei rcd scartati torno a leggere
016200141218     c                   IF        not wRCDok
016300141218     c                   iter
016400141218     c                   ENDIF
016500060914
016600141218      * carico la tassazione corrente
016700060914     c                   exsr      sr_corrente
016800060914
016900141218      * SOMMO IL NUMERO DI SPEDIZIONE CHE SONO STATE  PRESE IN CONSIDEArZIONE
017000140411     c                   add       1             totspe
017100141218     c                   write     wfcfs000
017200060914
017300050607     c                   enddo
017400050526      *
017500070207      * stampo  la fine elaborazione
017600120213     c                   write     cf92s1
017700070207
017800040119     c                   eval      *inLR = *on
017900141218
018000050614     c                   movel(p)  param         kpjbu
018100040331      *
018200050607      ****************************************************
018300050607      ** Carica importi varie da tita7000
018400050607      ****************************************************
018500050607     c     Carta7        Begsr
018600050607      * pulisco le schiere delle varie
018700050607     c                   Do        20            k
018800050607     c                   If        k > 3
018900050607     c                   clear                   VA(K)
019000050607     c                   clear                   SV(K)
019100050607     c                   Endif
019200050607     c                   Enddo
019300060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
019400050607     c                   clear                   flgta7
019500050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
019600050607     c                   If        tassv3 <> *blanks
019700050607      * mi posiziono su tita7
019800050607     c     Kta7          setll     tita730c
019900050607     c                   if        %equal(tita730c)
020000050607     c                   z-add     3             k
020100050607      **
020200050607     c                   do        *hival
020300050607     c     Kta7          reade     tita730c
020400050607      * fine file
020500050607     c                   if        %eof(tita730c)
020600050607     c                   leave
020700050607     c                   endif
020800050607      * carico le varie
020900050607     c                   add       1             k
021000050607     c                   movel     ta7svn        sv(k)
021100050607     c                   z-add     ta7van        va(k)
021200050607     c                   z-add     1             flgta7
021300050607     c                   enddo
021400050607
021500050607     c                   endif
021600050607
021700050607     c                   endif
021800050607
021900050607     c                   Endsr
022000050607      *************************************************************************
022100060914      * scrittura w-file dati tassazione corrente
022200050607      *************************************************************************
022300060914     C     sr_corrente   BEGSR
022400141218
022500141218     c                   clear                   wfcfs000
022600141218     c                   z-add     tasaas        CFSAAS
022700141218     c                   z-add     taslnp        CFSLNP
022800141218     c                   z-add     tasnrs        CFSNRS
022900141218     c                   z-add     tasnsp        CFSNSP
023000141218     c                   z-add     tasksc        CFSKSC
023100141218     c                   z-add     tasctr        CFSCTR
023200140407      * se si tratta di spedizione già fatturata verifico se devo ricalcolare con una
023300140407      * nuova tariffa
023400140410    Ac                   if        pardtc = 0 and tasimv > 0
023500141218     c                   z-add     tasimv        CFSIMVC
023600141218     c                   z-add     taspor        CFSPORC
023700140408      * carico eventuali varie presenti in TITA7
023800140408     c                   Exsr      Carta7
023900140408
024000140408      * cerco la varia "D" (diritto di fatturazione)
024100140408      * se c'è la tolgo dall'imponibile e dalle varie
024200140408     c                   z-add     1             k
024300140408     c     'D'           lookup    sv(k)                                  30
024400140408     c                   if        %found
024500141218     c                   eval      CFSimvc = CFSimvc - va(k)
024600140408     c                   clear                   va(k)
024700140408     c                   clear                   sv(k)
024800140408     c                   endif
024900140411      * mi salvo le varie per ricalcolo istat e fuel
025000140411     c                   movea     sv            sav_sv
025100140411     c                   movea     va            sav_va
025200140411      * MI SDALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
025300140409     c                   z-add     1             k
025400140409     c     'f'           lookup    sv(k)                                  30
025500140409     c                   if        %found
025600140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
025700141218     c                   eval      CFSfuelc = va(k)
025800140409     c                   endif
025900140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
026000140409     c                   z-add     1             k
026100140409     c     'L'           lookup    sv(k)                                  30
026200140409     c                   if        %found
026300141218     c                   eval      CFSistatc = va(k)
026400140409     c                   endif
026500140408      * "R" AC PLUS da togliere dall'imponibile
026600140408     c                   z-add     1             k
026700140408     c     'R'           lookup    sv(k)                                  30
026800140408     c                   if        %found
026900141218     c                   eval      CFSimvc = CFSimvc - va(k)
027000140408     c                   clear                   va(k)
027100140408     c                   clear                   sv(k)
027200140408     c                   endif
027300140408      * valorizzo le varie della tassazione corrente
027400140408     c                   movea     va            vacor
027500140408     c                   movea     sv            svcor
027600140408     c
027700140410    Ac                   Endif
027800140408
027900050607     c                   endsr
028000130218
028100050609      *************************************************************************
028200050609      * Operazioni iniziali
028300050609      *************************************************************************
028400050609     C     *inzsr        BEGSR
028500050609     c*
028600140408     c     ktas          klist
028700140408     c                   kfld                    paraas
028800140408     c                   kfld                    parmgs
028900060914
029000060914     c     ktai06        klist
029100060914     c                   kfld                    tasaas
029200060914     c                   kfld                    taslnp
029300060914     c                   kfld                    tasnrs
029400060914     c                   kfld                    tasnsp
029500060914     c                   kfld                    tastbl
029600060914     c                   kfld                    Ksvt
029700060914
029800050609     c     kta7          klist
029900050609     c                   kfld                    tasaas
030000050609     c                   kfld                    taslnp
030100050609     c                   kfld                    tasnrs
030200050609     c                   kfld                    tasnsp
030300050609     c                   kfld                    tastbl
030400050609
030500050609     C     KTAB          KLIST
030600050609     C                   KFLD                    CODUT
030700050609     C                   KFLD                    KCOD
030800081105      *
030900050609     c
031000050609     c     *entry        plist
031100050609     c                   parm                    kpjba
031200050609     c                   movel     kpjbu         param
031300060915
031400090430     c                   time                    wtime            14 0
031500090430     c                   move      wtime         wdata
031600090430     c                   movel     wtime         wora
031700060915      * caricamento schiera dei tipi bolla da scartare
031800060915     c                   eval      Kcod  = 'TB'
031900050609     c                   clear                   tbs
032000050609     c                   z-add     0             xx
032100050609     c     ktab          setll     tabel00f
032200050609     c                   do        *hival
032300050609     c     ktab          reade     tabel00f
032400050609     c                   if        %eof(tabel00f)
032500050609     c                   leave
032600050609     c                   endif
032700050609     c                   movel     tbluni        dstb
032800080715      * con richiesta di Bocchi includiamo nella ritassazione / confronto di fatturazione
032900080715      * anche le bolle di recupero  §tbrbl = 'R'
033000080715     c                   if        §tbfcb <> '1'
033100050609     c                   add       1             xx
033200050609     c                   movel     tblkey        tbs(xx)
033300050609     c                   endif
033400050609     c                   enddo
033500060915      * caricamento schiera dei codici bolla da scartare
033600060915     c                   eval      Kcod  = '3A'
033700060915     c                   z-add     0             xx
033800060915     c     ktab          setll     tabel00f
033900060915     c                   do        *hival
034000060915     c     ktab          reade     tabel00f
034100060915     c                   if        %eof(tabel00f)
034200060915     c                   leave
034300060915     c                   endif
034400060915     c                   movel     tbluni        ds3a
034500060915     c                   if        §3asva <> *blanks
034600060915     c                   add       1             xx
034700060915     c                   movel     tblkey        cbo(xx)
034800060915    3c                   endif
034900060915     c                   enddo
035000140409      * GIRO LE DATE PER LE STAMPE
035100140409     C                   move      PARDRF        G08INV
035200140410     c                   eval      G08err = '3'
035300140409     C                   CALL      'XSRDA8'
035400140409     C                   PARM                    WLBDA8
035500140409     C                   Z-ADD     G08DAT        WPARDRF
035600140409      *
035700140409     C                   move      PARDTC        G08INV
035800140410     c                   eval      G08err = '3'
035900140409     C                   CALL      'XSRDA8'
036000140409     C                   PARM                    WLBDA8
036100140409     C                   Z-ADD     G08DAT        WPARDTC
036200140409      *
036300140409     C                   move      PARDAL        G08INV
036400140410     c                   eval      G08err = '3'
036500140409     C                   CALL      'XSRDA8'
036600140409     C                   PARM                    WLBDA8
036700140409     C                   Z-ADD     G08DAT        WPARDAL
036800140409      *
036900140409     C                   move      PARAL         G08INV
037000140410     c                   eval      G08err = '3'
037100140409     C                   CALL      'XSRDA8'
037200140409     C                   PARM                    WLBDA8
037300140409     C                   Z-ADD     G08DAT        WPARAL
037400060915
037500050609     c                   endsr
