000100040119      *===============================================================*
000200160118      * TicfQ4r    * Confronto fatturazione 15 Gennaio 2016
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000160126     fTITAS41C  if   e           k disk    prefix(t_)
001100060914     fTita730c  if   e           k disk
001200060914     fTitaI01L  if   e           k disk
001300060914     Ffiar531c  if   e           k disk
001400060914     Ftncsb03l  if   e           k disk
001500060914     fwfcft01l  uf a e           k disk
001600060914     Ftabel00f  if   e           k disk
001700081105     Ffnevb04l  if   e           k disk
001800160126     fTICFT01L  if a e           k DISK    prefix(t_)
001900160921     fticfq4p   o    e             printer oflind(*in90)
002000040119      *
002100050603     d dAr5Ban       e ds
002200050603     d dAr5Bnb       e ds
002300151023     d dAr5Fat       e ds
002400151207     d dAr5Gen       e ds
002500050526     d kpjba         e ds
002600050609     d dstb          e ds
002700060915     d ds3a          e ds
002800151207     d ds7r          e ds
002900110701      ** DS del tasflo del file TITAS00F
003000110701     D DTASFLO       E DS
003100160118     d dtas          e ds                  inz qualified
003200060926      ** DS Flag operativi DS DTAS
003300060926     d Dtas01        e ds
003400050607      ** DS Calcolo tassazione  - Varie
003500160118     d Dtasv         e ds                  qualified
003600050607     d  sv                     1     20
003700050607     d                                     Dim(20)                              Sigle varie
003800050607     d  va                    21    140P 3
003900050607     d                                     Dim(20)                              Importi varie
004000130124      ** DS data ritassazione DS DTASdta
004100130124     d Dtasdta       e ds
004200130124
004300050607     d param           ds
004400140407     D  pardrf                 1      8  0
004500140407     D  pardtc                 9     16  0
004600140407     D  pardal                17     24  0
004700140408     D  paraas                17     20  0
004800140408     D  parmgs                21     24  0
004900140407     D  paral                 25     32  0
005000160126     D  paraasl               25     28  0
005100160126     D  parmgsl               29     32  0
005200140409
005300140409     D WLBDA8          DS                  INZ
005400140409     D  G08DAT                 1      8  0
005500140409     D  G08INV                 9     16  0
005600140409     D  G08ERR                17     17
005700140409     D  G08TGI                18     22  0
005800140409
005900050607     d
006000050607     dwfcftxxds      e ds                  extname(wfcft00f)
006100050607     dsvpre                   31     50    dim(20)
006200050607     dvapre                   51    170p 3 dim(20)
006300050607     dsvcor                  184    203    dim(20)
006400050607     dvacor                  204    323p 3 dim(20)
006500050607     d*
006600160118
006700160118     dTitasds        e ds                  extname(TITAS00F)
006800040331      *
006900040331      *
007000040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
007100050526      *
007200040331      * - Indici di schiera
007300040331     d xx              s              3  0 inz
007400050607     D k               s              5  0                                      indice
007500040331      * - Campi di comodo
007600050603     d kAr5Trd         s                   Like(Ar5Trd)
007700060914     d kmgs            s                   Like(TasMgs)
007800050609     d codut           s              1  0 Inz(1)
007900060915     D kcod            s                   like(tblcod)
008000060914     D ksvt            s                   like(taisvt)
008100150528     d ksccod          s              4  0                                      ultimi 4 byte codcli
008200081105     D Wcev            s                   like(evbcev) inz('RIC')
008300060914     D wmm             s              2  0
008400070207     D totspe          s              9  0
008500070207     D tottas          s              9  0
008600130110     D totsmt          s              9  0
008700140408     d antepo          s                   like(tasimv)
008800160118
008900160118     d sav_TASksc      s                   like(TASksc) inz
009000160118     d sav_TASctr      s                   like(TASctr) inz
009100160118     d new_datarif     s              8s 0 inz
009200160118     d GiaTassato      s               n   inz(*off)
009300160128     d wdatadsp        s              8s 0 inz
009400060914      *
009500060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
009600060914      *
009700060914      * codici bolla da scartare
009800060914     D tbs             s              2    dim(50)
009900060915      * codici bolla che prevedono la tassazione di una singola varia
010000060915     D cbo             s              2    dim(50)
010100130124      * sigle varie salvate
010200130124     D sav_sv          s              1    dim(20)
010300130124      * importo varie salvate
010400130124     D sav_va          s             11  3 dim(20)
010500151207      ** Sk part.consegna con abilitazione PIN Code
010600151207     d skGMA           s              2a   dim(20)
010700060915      *
010800160118      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
010900160118      /free
011000160118         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011100160118
011200160118       // Leggo il file delle bolle per Data Spedizione e Cliente/Tariffe
011300160118         exec sql
011400160118         declare C1 cursor for
011500160118         select * from titas00f where
011600160126         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :parmgsl
011700160118         union
011800160118         select * from titas10f where
011900160126         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :parmgsl
012000160118         union
012100160118         select * from titasp0f where
012200160126         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :parmgsl
012300160126         order by TASksc, TASctr
012400160118         for read only;
012500160118
012600160118         exec sql open C1;
012700160118
012800160118         DOW  sqlcod = *zeros;
012900160118           clear dtas;
013000160118           clear dtasv;
013100160118           clear dtasdta;
013200160118           exec sql fetch C1 into :TITASDS;
013300160126
013400160129       // A rottura di Cliente/Tariffa
013500160126         IF  sav_TASksc <> TASksc or sav_TASctr <> TASctr;
013600160129       // recupero la data di entrata del cliente dal TICFT
013700160126           chain (TASksc:TASctr) TICFT01L;
013800160126           IF  %found(TICFT01L);
013900160126             IF  t_CFTdpsf > pardrf;
014000160201               new_datarif = t_CFTdpsf;
014100160126             ELSE;
014200160201               new_datarif = pardrf;
014300160126             ENDIF;
014400160126           ELSE;
014500160126             new_datarif = pardrf;
014600160126           ENDIF;
014700160126           sav_TASksc = TASksc;
014800160126           sav_TASctr = TASctr;
014900160127           GiaTassato = *off;
015000160126         ENDIF;
015100160118      /end-free
015200160126
015300040119
015400160118     c                   movel(p)  tasaas        dtas.tasdsp
015500160118     c                   move      tasmgs        dtas.tasdsp
015600160126
015700150721      * scarto le bolle che non sono state fatturate
015800150721     c                   If        tasfic = ' '
015900150721     c                   iter
016000150721     c                   endif
016100060915
016200060915      * scarto  se si tratta di codice bolla che prevede solo una varia
016300060918     c                   z-add     1             kk                2 0
016400060915     c     tascbo        lookup    cbo(kk)                                05
016500060915     c                   if        *in05
016600060915     c                   iter
016700060915     c                   endif
016800060914
016900060914      * scarto le bolle che sono state fatturate in filiale
017000140408     c                   If        tasfiv <> 998 and tasfiv <> 996
017100140408     c                               and tasfiv > 0
017200060914     c                   iter
017300060914     c                   endif
017400060914
017500140407      * verifico se tassata fino all'imponibile in TITAI se già registrate su TITAI
017600060914     c                   eval      ksvt = '£'
017700060914     c     Ktai06        chain     titai01l
017800060914      * se trovato record imponibile  scarto la bolla
017900060914     c                   If        %found(titai01l)
018000060914     c                   iter
018100060914     c                   endif
018200060914     c
018300140408      * verifico se tassata fino all'imponibile con TASIMV > 0 scarto
018400140408     c                   If        TASIMV > 0 and tasfiv = 0
018500140407     c                   iter
018600140407     c                   endif
018700140407     c
018800060914
018900111025      * scarto bolle di  c/servizi, storni
019000050609     c     tastbl        lookup    tbs                                    30
019100140409     c                   if        *in30
019200140409     c                   iter
019300140409     c                   endif
019400140409
019500160126      * se presenti varia "N" o "&" + 88888888
019600160126      * in TITAI scarto la bolla
019700140409     c                   eval      ksvt = 'N'
019800140409     c     Ktai06        chain     titai01l
019900140409      * se trovato record imponibile  scarto la bolla
020000140409     c                   If        %found(titai01l)  and taivat = 88888888
020100140409     c                   iter
020200140409     c                   endif
020300140409
020400140409     c                   eval      ksvt = '&'
020500140409     c     Ktai06        chain     titai01l
020600140409      * se trovato record imponibile  scarto la bolla
020700140409     c                   If        %found(titai01l)  and taivat = 88888888
020800140409     c                   iter
020900140409     c                   endif
021000140409
021100150528      * verifico se è una bolla non tassata fino l'imponibile se ksc 8888 o 9999
021200150528      * la scarto
021300150528      *
021400150528    2c                   if        tasimv = 0
021500150528     c                   move      tasksc        ksccod
021600150528    2c                   if        ksccod = 9999 or ksccod = 8888
021700150528     c                   iter
021800150528    3c                   endif
021900150528    3c                   endif
022000160125
022100160125      * carico la DTAS con i campi del TITAS
022200160125     c                   exsr      Move_Dati
022300150528
022400140409      * carico le varie presenti
022500140409     c                   Exsr      Carta7
022600140409      *
022700140409      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
022800140409      * con VARIA 'N' valorizzata con 888888 la scarto
022900140409      *
023000140409    2c                   if        tasimv = 0 and tasfbr = 'S'
023100140409     c                   z-add     1             xx
023200160118     c     'N'           lookup    dtasv.sv(xx)                           30
023300160118    3c                   if        *in30 and dtasv.va(xx) =  88888888
023400140409     c                   iter
023500140409    3c                   endif
023600140409    2c                   endif
023700140409      *
023800140409      * verifico se è esiste la varia & con importo uguale a 8888888 la scarto
023900140409      *
024000140409    2c                   if        tasimv = 0
024100140409     c                   z-add     1             xx
024200160118     c     '&'           lookup    dtasv.sv(xx)                           30
024300160118    3c                   if        *in30 and dtasv.va(xx) =  88888888
024400140409     c                   iter
024500140409    3c                   endif
024600140409    2c                   endif
024700060914
024800140407      * carico la tassazione corrente  se imponibile > 0
024900060914     c                   exsr      sr_corrente
025000060914
025100140411      * SOMMO IL NUMERO DI SPEDIZIONE CHE SONO STATE  PRESE IN CONSIDEAZIONE
025200140411     c                   add       1             totspe
025300160126
025400160126      * se dalla tassazione precedente (per fuel e istat) ci sono stati errori di manca tariffa
025500160126      * devo memorizzare la data di entrata del cliente
025600160126      * e provare a ritassare con questa nuova data
025700160126     c                   IF        dtas.TASerr = 'M' and
025800160126     c                             not GiaTassato
025900160126     c                   exsr      CercaData
026000160126     c                   eval      GiaTassato = *on
026100160126      * reimposto le varie come da fatturazione
026200160126     c                   movea     sav_va        dtasv.va
026300160126     c                   movea     sav_sv        dtasv.sv
026400160126      * ricalcolo ISTAT e FUEL
026500160126     c                   exsr      car_sf21ds
026600160126     c                   ENDIF
026700160126
026800140409      * eseguo la tassazione precedente se non ci sono stati errori nella tassazione corrente
026900160118     c                   if        dtas.taserr = ' '
027000130124     c                   exsr      sr_sf21
027100160127      * se dalla nuova tassazione torna errore di tipo M = Manca tariffa
027200160126      * devo ritassare in modo da recuperare la nuova data di riferimento da passare
027300160126      * al tnsf21r
027400160126     c                   IF        dtas.TASerr = 'M'
027500160126     c                             and not GiaTassato
027600160126     c                   clear                   dtas.TASerr
027700160126     c                   eval      GiaTassato = *on
027800160126     c                   exsr      sr_sf21
027900160126     c                   ENDIF
028000140409     c                   endif
028100060914
028200050607     c                   enddo
028300160118      /free
028400160118         exec sql close C1;
028500160118      /end-free
028600050526      *
028700060914      *
028800130110      * conto le spedizioni che non sono state tassate
028900130110     c                   eval      totsmt = totspe - tottas
029000070207      * stampo  la fine elaborazione
029100120213     c                   write     cf92s1
029200070207
029300040119     c                   eval      *inLR = *on
029400060914      *
029500160118     c                   movel     'C'           dtas.tastla
029600160223     C                   CALL      'TNSF21R1'
029700050603     C                   PARM                    KPJBA
029800050603     C                   PARM                    DTAS
029900050603     C                   PARM                    DTASV
030000130124     C                   PARM                    DTASDTA
030100050614     c*
030200050614     c                   movel(p)  param         kpjbu
030300040331      *
030400050526      **-------------------------------------------------------------**
030500130124      ** Richiamo pgm tassaz. tnsf21r                                **
030600050526      **-------------------------------------------------------------**
030700130124     c     sr_sf21       BEGSR
030800050526      *
030900160118     c                   movel     *Blanks       dtas.tastla
031000050603     c                   Clear                   kpjbu
031100140409     c                   clear                   dtasdta
031200140409     c                   clear                   dtasv
031300160126     c                   Clear                   dtas.taspor
031400140409      * Assicurazione
031500140409    2c                   IF        Tasfcm = 'F'
031600160126     c                   clear                   dtas.tasias
031700160126     c                   clear                   dtas.tasvas
031800140409    2c                   Endif
031900140409
032000140409      * recupero i valori  già tassati
032100140409     c                   exsr      Ric_tai
032200140409
032300160126     c                   Clear                   dtas.tasimv
032400160127     c                   clear                   dtas.tassva
032500130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
032600130124      * tariffa con data tariffe
032700160127     c                   z-add     new_datarif   dtas.tasdsp
032800160201     c                   z-add     pardrf        dtas.tasdct
032900160118      * passo la nuova data
033000160201     c                   z-add     new_datarif   tasdtcar
033100130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
033200130124      * calcolo istat e calcolo fuel
033300160126     c                   eval      tasdtist = new_datarif
033400130124     c                   movel(p)  tasaas        tasdtfue
033500130124     c                   move      tasmgs        tasdtfue
033600140204      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
033700140204      * calcolo fuel
033800140409     c                   If        tasdft = 0
033900140409     c                   movel(p)  tasaas        tasdtfat
034000140409     c                   move      tasmgs        tasdtfat
034100140409     c                   else
034200140204     c                   z-add     tasdft        tasdtfat
034300140409     c                   endif
034400060914
034500151023      * se ho record FAT nel file Fiar5 imposto il peso e volume usato in tassazione
034600151023      * nel campo taspkf in modo da tassare con lo stesso peso
034700151023     c                   clear                   dAr5Fat
034800151023     c                   eval      kAr5Trd  = 'FAT'
034900151023     c     kFiar5        Chain     Fiar531c
035000151023If  3c                   If        %Found(Fiar531c)
035100151023     c                   Movel     Ar5Uni        dAr5Fat
035200151023    3c                   EndIf
035300151023     c* non presente il record "FAT" --> tasso con peso e vollume presi da titas
035400151023     c                   if        §AR5PKTAS>0
035500160126     c                   clear                   dtas.TASPKC
035600160126     c                   clear                   dtas.tasncp
035601160530     c                   clear                   dtas.tasncr
035602160530     c                   clear                   dtas.tasvlc
035700151023     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
035800160126     c                   eval      dtas.TASPKF=§ar5pktas
035900160126     c                   eval      dtas.TASVLF=§AR5VLTAS
036000151023     c                   endif
036100151023
036200160223     c                   call      'TNSF21R1'
036300060914     c                   PARM                    KPJBA
036400060914     c                   PARM                    DTAS
036500060914     c                   PARM                    Dtasv
036600130124     c                   PARM                    Dtasdta
036700060914
036800050607     c                   select
036900160126      * se torna errore di Manca tariffa
037000160126      * devo memorizzare la data di entrata del cliente
037100160126     c                   when      dtas.taserr = 'M'
037200160126     c                   exsr      CercaData
037300160126     c                   leavesr
037400160118     c                   when      dtas.taserr = *blanks
037500120113      *----------------------------------------------------------------*
037600090216      * se si tratta di confronto fatturazione
037700090216      * cerco la varia "f" (supplemento carburante)
037800090216      * se c'è la tolgo dall'imponibile e dalle varie
037900090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
038000090216      * e la varia "R" AC PLUS assicurazione
038100120113      *----------------------------------------------------------------*
038200100111      * DAL 2010 TOLGO LA VARIA h PER IL SERVIZIO 10:30 E
038300100111      * SUPPLEMENTO CARBURANTE FEDEX MENTRE RIPRISTINO
038400100111      * IL LASCIATO AVVISO
038500120113      *----------------------------------------------------------------*
038600120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
038700120113      * "R" ac plus
038800120113      * ZTL
038900120113      * Ripristino varia "h" servizio 10:30
039000120113      * come indicato da mail di DE LUCA del 11/01/2012
039100151207      *----------------------------------------------------------------*
039200151207      * dal 2016 escludo le varie:
039300151207      * "t" diritto di chiamata prenotazione ritiro
039400151207      * "k" pascking list
039500151207      * "R" ac plus
039600120113      *----------------------------------------------------------------*
039700050607     c                   exsr      SR_riewf
039800160126
039900160126      * imposto se cliente nuovo
040000160126     c                   IF        new_datarif <> pardrf
040100160126     c                   eval      CFTnewtar = 'S'
040200160126     c                   ENDIF
040300080714
040400070207     c                   add       1             tottas
040500130111     c                   write     wfcft000
040600050607     c                   endsl
040700140411
040800160118     c                   clear                   dtas.taserr
040900130124
041000050607     c     e_sf20        endsr
041100050607      ****************************************************
041200050607      ** Carica importi varie da tita7000
041300050607      ****************************************************
041400050607     c     Carta7        Begsr
041500050607      * pulisco le schiere delle varie
041600050607     c                   Do        20            k
041700160118     c                   clear                   dtasv.VA(K)
041800160118     c                   clear                   dtasv.SV(K)
041900050607     c                   Enddo
042000160125
042100160125      * imposto le 3 varie di TITAS
042200160125     c                   eval      dtasv.SV(1) = TASsv1
042300160125     c                   eval      dtasv.VA(1) = TASva1
042400160125     c                   eval      dtasv.SV(2) = TASsv2
042500160125     c                   eval      dtasv.VA(2) = TASva2
042600160125     c                   eval      dtasv.SV(3) = TASsv3
042700160125     c                   eval      dtasv.VA(3) = TASva3
042800160125
042900050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
043000050607     c                   If        tassv3 <> *blanks
043100050607      * mi posiziono su tita7
043200050607     c     Kta7          setll     tita730c
043300050607     c                   if        %equal(tita730c)
043400050607     c                   z-add     3             k
043500050607      **
043600050607     c                   do        *hival
043700050607     c     Kta7          reade     tita730c
043800050607      * fine file
043900050607     c                   if        %eof(tita730c)
044000050607     c                   leave
044100050607     c                   endif
044200050607      * carico le varie
044300050607     c                   add       1             k
044400160118     c                   movel     ta7svn        dtasv.sv(k)
044500160118     c                   z-add     ta7van        dtasv.va(k)
044600050607     c                   enddo
044700050607
044800050607     c                   endif
044900050607
045000050607     c                   endif
045100050607
045200050607     c                   Endsr
045300060914      *
045400060914      *****************************************************************
045500060914      *  Recupero importi di tassazione dal file titai
045600060914      *****************************************************************
045700060914     c     Ric_tai       Begsr
045800060914      *
045900060914     c                   z-add     0             k
046000060914
046100060914     c     ktai05        setll     titai01l
046200060914
046300060914     c                   If        %equal(titai01l)
046400060914
046500060914     c                   do        *hival
046600060914
046700060914     c     ktai05        reade     titai01l
046800060914
046900060914     c                   If        %eof(titai01l)
047000060914     c                   leave
047100060914     c                   endif
047200060914      * imponibile
047300060914     c                   if        taisvt = '£'
047400160125     c                   eval      dtas.tasimv = taivat
047500060914     c                   endif
047600060914      * porto
047700060914     c                   if        taisvt = ' '
047800160125     c                   eval      dtas.taspor = taivat
047900060914     c                   endif
048000060914      * varie
048100060914     c                   if        taisvt <> ' ' and taisvt <> '£'
048200060914     c                   add       1             k
048300160118     c                   eval      dtasv.sv(k) = taisvt
048400160118     c                   eval      dtasv.va(k) = taivat
048500060914     c                   endif
048600060914
048700060914     c                   enddo
048800060914
048900060914     c                   else
049000060914      * se non è pretassata pulisco la divisa
049100160125     c                   clear                   dtas.tasdiv
049200060914     c                   endif
049300060914
049400060914     c                   endsr
049500050607      ****************************************************
049600050609      ** Ricerca Contrassegno
049700050607      ****************************************************
049800050607     c     Carcsb        Begsr
049900050607
050000050607     c     Kcsb          chain     Tncsb03l
050100050607     c                   If        %found(tncsb03l) and
050200050607      * solo per stesso tipo bolla
050300050607     c                             tastbl = csbtbl
050400160118     c                   movel     csbsta        dtas.Tassta
050500160118     c                   z-add     csbcas        dtas.Tascas
050600050607     c                   if        csbfus <> *blanks
050700160118     c                   move      csbfus        dtas.tastic
050800050607     c                   else
050900160118     c                   move      csbtpi        dtas.tastic
051000050607     c                   end
051100050607      * mittente
051200160118     c                   movel     csbvca        dtas.tasvca
051300160118     c                   z-add     csbcmb        dtas.tascmb
051400050607
051500050607     c                   endif
051600050607
051700050607     c                   endsr
051800050607      *************************************************************************
051900060914      * scrittura w-file dati tassazione corrente
052000050607      *************************************************************************
052100060914     C     sr_corrente   BEGSR
052200160125
052300140408     c                   clear                   wfcft000
052400140408     c                   z-add     tasaas        CFTAAS
052500140408     c                   z-add     taslnp        CFTLNP
052600140408     c                   z-add     tasnrs        CFTNRS
052700140408     c                   z-add     tasnsp        CFTNSP
052800140408     c                   z-add     tasksc        CFTKSC
052900140408     c                   z-add     tasctr        CFTCTR
053000140407      * se si tratta di spedizione già fatturata verifico se devo ricalcolare con una
053100140407      * nuova tariffa
053200140410    Ac                   if        pardtc = 0 and tasimv > 0
053300140408     c                   z-add     tasimv        CFTIMVC
053400140408     c                   z-add     taspor        CFTPORC
053500140408
053600140408      * cerco la varia "D" (diritto di fatturazione)
053700140408      * se c'è la tolgo dall'imponibile e dalle varie
053800140408     c                   z-add     1             k
053900160118     c     'D'           lookup    dtasv.sv(k)                            30
054000140408     c                   if        %found
054100160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
054200160118     c                   clear                   dtasv.va(k)
054300160118     c                   clear                   dtasv.sv(k)
054400140408     c                   endif
054500140411      * mi salvo le varie per ricalcolo istat e fuel
054600160118     c                   movea     dtasv.sv      sav_sv
054700160118     c                   movea     dtasv.va      sav_va
054800160127      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
054900140409     c                   z-add     1             k
055000160118     c     'f'           lookup    dtasv.sv(k)                            30
055100140409     c                   if        %found
055200140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
055300160118     c                   eval      cftfuelc = dtasv.va(k)
055400140409     c                   endif
055500140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
055600140409     c                   z-add     1             k
055700160118     c     'L'           lookup    dtasv.sv(k)                            30
055800140409     c                   if        %found
055900160118     c                   eval      cftistatc = dtasv.va(k)
056000140409     c                   endif
056100140408      * "R" AC PLUS da togliere dall'imponibile
056200140408     c                   z-add     1             k
056300160118     c     'R'           lookup    dtasv.sv(k)                            30
056400140408     c                   if        %found
056500160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
056600160118     c                   clear                   dtasv.va(k)
056700160118     c                   clear                   dtasv.sv(k)
056800140408     c                   endif
058800151207
058900140408      * valorizzo le varie della tassazione corrente
059000160118     c                   movea     dtasv.va      vacor
059100160118     c                   movea     dtasv.sv      svcor
059200140408     c
059300140410    Ac                   Endif
059400140408
059500140408      * se imponibile non valorizzato lo calcolo o con la data passata a video o con la data
059600140408      * spedizione
059700140410    Bc                   If        Pardtc > 0 or tasimv = 0
059800140408      * se imponibile valorizzato devo ricalcolare tutto con la nuova data tariffa corrente
059900140408      * quindi pulisco varie imponibile e porto
060000140408     c                   If        tasimv > 0
060100140408
060200140408      * Pulisco i campi dell'assicurazione se calcolata in fattura
060300140408    2c                   IF        Tasfcm = 'F'
060400160125     c                   clear                   dtas.tasias
060500160125     c                   clear                   dtas.tasvas
060600140408    2c                   Endif
060700140408      *
060800140408      * valorizzo la DS DTAS e DTASV
060900140408     c                   clear                   dtasv
061000160125     c                   clear                   dtas.tasimv
061100160125     c                   clear                   dtas.taspor
061200140407      *
061300140408      * cerco in TITAI l'immagine della spedizione prima della tassazione  compreso l'imponibile
061400140407     c                   exsr      RIC_tai
061500140408     c                   endif
061600160126
061700140407      ** Ricerca contrassegno
061800140407    2c                   IF        Tasfca <> *blanks
061900140407     c                   Exsr      Carcsb
062000140407    2c                   Endif
062100140407
062200140407
062300160118     c                   movel(p)  tasaas        dtas.tasdsp
062400160118     c                   move      tasmgs        dtas.tasdsp
062500160118     c                   Z-add     dtas.tasdsp   dtas.tasdct
062600140407
062700140407      * Flag SI NO DDT
062800160118    2C                   If        tasll1 = 'C' or tasll1 = 'S'
062900160118     C                   movel     'S'           dtas.Tasddt
063000140407   x2C                   Else
063100160118     C                   Clear                   dtas.Tasddt
063200140407    2C                   Endif
063300140407     c                   clear                   dtas01
063400140407     c                   movel     tasfbr        §asfbr
063500140407     c                   movel     tascca        §ascca
063600140407      * valorizzo flag email al destinatario
063700140407     c                   movel     tasflo        dtasflo
063800140407     c                   move      §floemd       §asemd
063900151207      * pin code
064000151207     c                   if        tasgma<>*blanks and %lookup(tasgma:skgma)>0
064100151207     c                   eval      §aspin = 'S'
064200151207     c                   endif
064300151207
064400140408      * se data tariffa corrente diversa da zero passo nel flo la data tariffa corrente
064500140408     c                   If        pardtc > 0
064600140408     c                   move      pardtc        §asdrt
064700160118     c                   Z-add     pardtc        dtas.tasdct
064800140407     c                   endif
064900140407
065000160118     c                   eval      dtas.tasflo = dtas01
065100140407
065200151207      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
065300160118     c                   move      §floado       dtas.tasprt
065400151207      * valorizzo il campo per l'addebito stampa PACKING LIST
065500151207     c                   eval      kAr5Trd  = 'GEN'
065600151207     c                   clear                   DAr5gen
065700160118     c     kFiar5        Chain     Fiar531c
065800151207If  1c                   If        %Found(Fiar531c)
065900151207     c                   Movel     Ar5Uni        dAr5Gen
066000151207    1c                   EndIf
066100151207      *
066200151207     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
066300160118     c                   Eval      dtas.TasSpl = 'S'
066400151207     c                   endif
066500151207
066600140407      * Bancali
066700140407    2c                   If        %Subst(TasGva:1:1) = 'B'
066800140407
066900140407     c                   eval      kAr5Trd  = 'BAN'
067000140407     c     kFiar5        Chain     Fiar531c
067100140407If  3c                   If        %Found(Fiar531c)
067200140407     c                   Movel     Ar5Uni        dAr5Ban
067300140407    3c                   EndIf
067400140407      * Bancali
067500160118     c                   Eval      dtas.TasBan = §Ar5Nba + §Ar5Nb2
067600140407    2c                   EndIf
067700140407      **
067800140407      * Colli Originali
067900140407    2c                   If        %Subst(TasGva:1:1) = 'O'
068000140407
068100140407     c                   eval      kAr5Trd  = 'BNB'
068200140407     c     kFiar5        Chain     Fiar531c
068300140407If  3c                   If        %Found(Fiar531c)
068400140407     c                   Movel     Ar5Uni        dAr5Bnb
068500140407    3c                   EndIf
068600140407      * Colli
068700160125     c                   Eval      dtas.TasNcl = §Ar5bcor
068800140407    2c                   EndIf
068900140407      *
069000140407      * verifico se c'è evento
069100140407     c     Kevb4         chain     Fnevb04l
069200140407
069300140408    2c                   If        %found(Fnevb04l)
069400160118     c                   eval      dtas.tasric = 'S'
069500140408    2c                   endif
069600140407
069700140407      **
069800140409     c                   clear                   dtasdta
069900160127      * imposto la data fattura  per calcolo imponibile
070000140409      * calcolo fuel
070100140409     c                   If        tasdft = 0
070200140409     c                   movel(p)  tasaas        tasdtfat
070300140409     c                   move      tasmgs        tasdtfat
070400140409     c                   else
070500140409     c                   z-add     tasdft        tasdtfat
070600140409     c                   endif
070700140409
070800160223     c                   call      'TNSF21R1'
070900140407     c                   parm                    Kpjba
071000140407     c                   parm                    Dtas
071100140407     c                   parm                    Dtasv
071200140409     c                   parm                    dtasdta
071300140409
071400160118     c                   if        dtas.taserr <> ' '
071500140409     c                   leavesr
071600140409     c                   endif
071700140407
071800140407      * tassazione
071900160125     c                   z-add     dtas.tasimv   CFTIMVC
072000160125     c                   z-add     dtas.taspor   CFTPORC
072100140411      * mi salvo le varie per ricalcolo istat e fuel
072200160118     c                   movea     dtasv.sv      sav_sv
072300160118     c                   movea     dtasv.va      sav_va
072400140409      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
072500140409     c                   z-add     1             k
072600160118     c     'f'           lookup    dtasv.sv(k)                            30
072700140409     c                   if        %found
072800140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
072900160118     c                   eval      cftfuelc = dtasv.va(k)
073000140409     c                   endif
073100140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
073200140409     c                   z-add     1             k
073300160118     c     'L'           lookup    dtasv.sv(k)                            30
073400140409     c                   if        %found
073500160118     c                   eval      cftistatc = dtasv.va(k)
073600140409     c                   endif
073700060914
073800130124      * "R" AC PLUS da togliere dall'imponibile
073900090216     c                   z-add     1             k
074000160118     c     'R'           lookup    dtasv.sv(k)                            30
074100090216     c                   if        %found
074200160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
074300160118     c                   clear                   dtasv.va(k)
074400160118     c                   clear                   dtasv.sv(k)
074500130124     c                   endif
076500130124      * valorizzo le varie della tassazione corrente
076600160118     c                   movea     dtasv.va      vacor
076700160118     c                   movea     dtasv.sv      svcor
076800160126    Bc                   endif
076900160126
077000140411      * reimposto le varie come da fatturazione
077100160118     c                   movea     sav_va        dtasv.va
077200160118     c                   movea     sav_sv        dtasv.sv
077300140411      * ricalcolo ISTAT e FUEL
077400140408
077500140408     c                   exsr      car_sf21ds
077600140408
077700050607     c                   endsr
077800130124      **---------------------------------------------------------------**
077900130124      ** Carico la ds per le ritassazioni e le eseguo per ISTAT e FUEL **
078000130124      **---------------------------------------------------------------**
078100130124     c     car_sf21ds    BEGSR
078200130124      *
078300160118     c                   movel     *Blanks       dtas.tastla
078400130124     c                   clear                   dAr5Ban
078500130124     c                   Clear                   dAr5Bnb
078600130124     c                   Clear                   kpjbu
078700130124     c                   clear                   dtasdta
078800130124
078900160118     c                   movel     Tasftc        dtas.tastc1
079000130124      * Flag SI NO DDT
079100160118    1C                   If        tasll1 = 'C' or tasll1 = 'S'
079200160118     C                   movel     'S'           dtas.Tasddt
079300130124   x1C                   Else
079400160118     C                   Clear                   dtas.Tasddt
079500130124    1C                   Endif
079600160126
079700130124     c                   clear                   dtas01
079800130124     c                   movel     tasfbr        §asfbr
079900130124     c                   movel     tascca        §ascca
080000130124      * valorizzo flag email al destinatario
080100130124     c                   movel     tasflo        dtasflo
080200130124     c                   move      §floemd       §asemd
080300151207      * pin code
080400151207     c                   if        tasgma<>*blanks and %lookup(tasgma:skgma)>0
080500151207     c                   eval      §aspin = 'S'
080600151207     c                   endif
080700130124
080800160118     c                   eval      dtas.tasflo = dtas01
080900130124
080901170120      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
080902170120     c                   move      §floado       dtas.tasprt
080903170120      * valorizzo il campo per l'addebito stampa PACKING LIST
080904170120     c                   eval      kAr5Trd  = 'GEN'
080905170120     c                   clear                   DAr5gen
080906170120     c     kFiar5        Chain     Fiar531c
080907170120If  1c                   If        %Found(Fiar531c)
080908170120     c                   Movel     Ar5Uni        dAr5Gen
080909170120    1c                   EndIf
080910170120      *
080911170120     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
080912170120     c                   Eval      dtas.TasSpl = 'S'
080913170120     c                   endif
080914170120
081000160118     c                   clear                   dtas.tasban
081100130124      * Bancali
081200130124    1c                   If        %Subst(TasGva:1:1) = 'B'
081300130124     c                   eval      kAr5Trd  = 'BAN'
081400130124     c     kFiar5        Chain     Fiar531c
081500130124If  1c                   If        %Found(Fiar531c)
081600130124     c                   Movel     Ar5Uni        dAr5Ban
081700130124    1c                   EndIf
081800160118     c                   Eval      dtas.TasBan = §Ar5Nba + §Ar5Nb2
081900130124    1c                   EndIf
082000130124      **
082100130124      * Colli Originali
082200130124    1c                   If        %Subst(TasGva:1:1) = 'O'
082300130124     c                   eval      kAr5Trd  = 'BNB'
082400130124     c     kFiar5        Chain     Fiar531c
082500130124If  1c                   If        %Found(Fiar531c)
082600130124     c                   Movel     Ar5Uni        dAr5Bnb
082700130124    1c                   EndIf
082800160126     c                   Eval      dtas.TasNcl = §Ar5bcor
082900130124    1c                   EndIf
083000130124
083100160118     c                   clear                   dtas.tascas
083200160118     c                   clear                   dtas.tastic
083300160118     c                   clear                   dtas.tasvca
083400160118     c                   clear                   dtas.tascmb
083500130124      * C/Assegno
083600130124    2c                   IF        Tasfca <> *blanks
083700130124     c                   Exsr      Carcsb
083800130124    2c                   Endif
083900140409
084000130124     c     Kevb4         chain     Fnevb04l
084100130124     c                   If        %found(Fnevb04l)
084200160118     c                   eval      dtas.tasric = 'S'
084300130124     c                   endif
084400130124
084500130124      * Assicurazione
084600130124    2c                   IF        Tasfcm = 'F'
084700160125     c                   clear                   dtas.tasias
084800160125     c                   clear                   dtas.tasvas
084900130124    2c                   Endif
085000130124
085100160126      * imposto le date di riferimento x recupero
085200130124      * tariffa con data tariffe
085300160127     c                   z-add     new_datarif   dtas.tasdsp
085400160201     c                   z-add     pardrf        dtas.TASdct
085500160118      * passo la nuova data
085600160201     c                   z-add     new_datarif   tasdtcar
085700130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
085800130124      * calcolo istat
085900130124     c                   movel(p)  tasaas        tasdtist
086000130124     c                   move      tasmgs        tasdtist
086100140207      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
086200140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
086300140409     C                   IF        TASDFT > 0
086400140207     c                   z-add     tasdft        tasdtfat
086500140409     C                   ELSE
086600140409     c                   movel(p)  tasaas        tasdtfat
086700140409     c                   move      tasmgs        tasdtfat
086800140409     C                   ENDIF
086900160126
087000130129      * verifico se fuel pretassato dalla filiale
087100130129     c                   eval      ksvt = 'L'
087200130129     c     Ktai06        chain     titai01l
087300130129      * se non trovato record ricalcolo con stessa base imponibile
087400130129     c                   If        not %found(titai01l)
087500130124      * pulisco ISTAT della fatturazione
087600130124     c                   z-add     1             k
087700160118     c     'L'           lookup    dtasv.sv(k)                            30
087800130124     c                   if        %found
087900160118     c                   clear                   dtasv.va(k)
088000160118     c                   clear                   dtasv.sv(k)
088100130124     c                   endif
088200130124      * pulisco imponibile
088300160126     c                   clear                   dtas.tasimv
088400130124      * imposto la  varia da calcolare
088500160118     c                   eval      dtas.tassva = 'L'
088600160223     c                   call      'TNSF21R1'
088700130124     c                   PARM                    KPJBA
088800130124     c                   PARM                    DTAS
088900130124     c                   PARM                    Dtasv
089000130124     c                   PARM                    Dtasdta
089100130129     c                   endif
089200130124
089300130124      * valorizzo la varia ISTAT ricalcolata
089400130124     c                   z-add     1             k
089500160118     c     'L'           lookup    dtasv.sv(k)                            30
089600130124     c                   if        %found
089700160118     c                   eval      cftistatr = dtasv.va(k)
089800130124     c                   endif
089900130125      * valorizzo imponibile varia ISTAT ricalcolata
090000160126     c                   eval      cftimpist = dtas.tasimv - cftistatr
090100160126
090200130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
090300130124      * calcolo fuel
090400130124     c                   clear                   tasdtist
090500130124     c                   movel(p)  tasaas        tasdtfue
090600130124     c                   move      tasmgs        tasdtfue
090700140409      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
090800140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
090900140409     C                   IF        TASDFT > 0
091000140409     c                   z-add     tasdft        tasdtfat
091100140409     C                   ELSE
091200150709     c                   movel(p)  tasaas        tasdtfat
091300150709     c                   move      tasmgs        tasdtfat
091400140409     C                   ENDIF
091500130129      * verifico se fuel pretassato dalla filiale
091600130129     c                   eval      ksvt = 'f'
091700130129     c     Ktai06        chain     titai01l
091800130129      * se non trovato record ricalcolo con stessa base imponibile
091900130129     c                   If        not %found(titai01l)
092000130124      * pulisco FUEL  della fatturazione
092100130124     c                   z-add     1             k
092200160118     c     'f'           lookup    dtasv.sv(k)                            30
092300130124     c                   if        %found
092400160118     c                   clear                   dtasv.va(k)
092500160118     c                   clear                   dtasv.sv(k)
092600130124     c                   endif
092700130124      * pulisco imponibile
092800160125     c                   clear                   dtas.tasimv
092900130124      * imposto la  varia da calcolare
093000160118     c                   eval      dtas.tassva = 'f'
093100160223     c                   call      'TNSF21R1'
093200130124     c                   PARM                    KPJBA
093300130124     c                   PARM                    DTAS
093400130124     c                   PARM                    Dtasv
093500130124     c                   PARM                    Dtasdta
093600130129     c                   endif
093700130124
093800130124     c                   z-add     1             k
093900160118     c     'f'           lookup    dtasv.sv(k)                            30
094000130124     c                   if        %found
094100160118     c                   eval      cftfuelr = dtasv.va(k)
094200130124     c                   endif
094300130125      * valorizzo imponibile varia FUEL  ricalcolata
094400160126     c                   eval      cftimpfue= dtas.taspor
094500130125      * inoltro
094600130125     c                   z-add     1             k
094700160118     c     '2'           lookup    dtasv.sv(k)                            30
094800130125     c                   if        %found
094900160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
095000130125     c                   endif
095100130125      * isola
095200130125     c                   z-add     1             k
095300160118     c     'J'           lookup    dtasv.sv(k)                            30
095400130125     c                   if        %found
095500160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
095600130125     c                   endif
095700130125      * località disagiata
095800130125     c                   z-add     1             k
095900160118     c     'K'           lookup    dtasv.sv(k)                            30
096000130125     c                   if        %found
096100160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
096200130125     c                   endif
096300130125      * centro storico
096400130125     c                   z-add     1             k
096500160118     c     'Q'           lookup    dtasv.sv(k)                            30
096600130125     c                   if        %found
096700160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
096800130125     c                   endif
096900130125      * espresso/priority
097000130125     c                   z-add     1             k
097100160118     c     'e'           lookup    dtasv.sv(k)                            30
097200130125     c                   if        %found
097300160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
097400130125     c                   endif
097500140207      * Se la data fattura è < o uguale la 30/01/2014 non sommo queste varie aggiuntive
097600140409     c                   if        tasdft > 20140130 or
097700140409     c                             tasdft = 0
097800130125      * h 10,30
097900130125     c                   z-add     1             k
098000160118     c     'h'           lookup    dtasv.sv(k)                            30
098100130125     c                   if        %found
098200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
098300130125     c                   endif
098400140207      * = reso bancali
098500140207     c                   z-add     1             k
098600160118     c     '='           lookup    dtasv.sv(k)                            30
098700140207     c                   if        %found
098800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
098900140207     c                   endif
099000140207      * c lasciato avviso
099100140207     c                   z-add     1             k
099200160118     c     'c'           lookup    dtasv.sv(k)                            30
099300140207     c                   if        %found
099400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
099500140207     c                   endif
099600140207      * A appuntamento
099700140207     c                   z-add     1             k
099800160118     c     'A'           lookup    dtasv.sv(k)                            30
099900140207     c                   if        %found
100000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
100100140207     c                   endif
100200140207      * B consegna DDT
100300140207     c                   z-add     1             k
100400160118     c     'B'           lookup    dtasv.sv(k)                            30
100500140207     c                   if        %found
100600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
100700140207     c                   endif
100800140207      * C facch. arr.
100900140207     c                   z-add     1             k
101000160118     c     'C'           lookup    dtasv.sv(k)                            30
101100140207     c                   if        %found
101200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
101300140207     c                   endif
101400140207      * F fuori misura
101500140207     c                   z-add     1             k
101600160118     c     'F'           lookup    dtasv.sv(k)                            30
101700140207     c                   if        %found
101800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
101900140207     c                   endif
102000140207      * H diritto fisso
102100140207     c                   z-add     1             k
102200160118     c     'H'           lookup    dtasv.sv(k)                            30
102300140207     c                   if        %found
102400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
102500140207     c                   endif
102600140207      * I spese di giacenza
102700140207     c                   z-add     1             k
102800160118     c     'I'           lookup    dtasv.sv(k)                            30
102900140207     c                   if        %found
103000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
103100140207     c                   endif
103200140207      * N anteporto
103300140207     c                   z-add     1             k
103400160118     c     'N'           lookup    dtasv.sv(k)                            30
103500140207     c                   if        %found
103600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
103700140207     c                   endif
103800140207      * P ai piani
103900140207     c                   z-add     1             k
104000160118     c     'P'           lookup    dtasv.sv(k)                            30
104100140207     c                   if        %found
104200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
104300140207     c                   endif
104400140207      * S supermercato
104500140207     c                   z-add     1             k
104600160118     c     'S'           lookup    dtasv.sv(k)                            30
104700140207     c                   if        %found
104800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
104900140207     c                   endif
105000140207      * U ritiro
105100140207     c                   z-add     1             k
105200160118     c     'U'           lookup    dtasv.sv(k)                            30
105300140207     c                   if        %found
105400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
105500140207     c                   endif
105600140207      * X consegne disagiate
105700140207     c                   z-add     1             k
105800160118     c     'X'           lookup    dtasv.sv(k)                            30
105900140207     c                   if        %found
106000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
106100140207     c                   endif
106200151207      * p Pin code
106300151207     c                   z-add     1             k
106400160118     c     'P'           lookup    dtasv.sv(k)                            30
106500151207     c                   if        %found
106600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
106700151207     c                   endif
106800151207      * Z Expo
106900151207     c                   z-add     1             k
107000160118     c     'z'           lookup    dtasv.sv(k)                            30
107100151207     c                   if        %found
107200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
107300151207     c                   endif
107400151207      * t diritto di chiamata prenotazione telefonica
107500151207     c                   z-add     1             k
107600160118     c     't'           lookup    dtasv.sv(k)                            30
107700151207     c                   if        %found
107800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
107900151207     c                   endif
108000151207      * k PACKING LIST
108100151207     c                   z-add     1             k
108200160118     c     'k'           lookup    dtasv.sv(k)                            30
108300151207     c                   if        %found
108400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
108500151207     c                   endif
108600140207
108700140207     c                   endif
108800130124
108900130124     c                   endsr
109000050607      *************************************************************************
109100060914      * scrittura w-file dati tassazione precdeente
109200050607      *************************************************************************
109300060914     C     sr_riewf      BEGSR
109400061218
109500061218      * cerco la varia "f" (supplemento carburante)
109600061218      * se c'è la tolgo dall'imponibile e dalle varie
109700090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
109800090216      * e la varia "R" AC PLUS assicurazione
109900100108      * dal 2010 togliamo anche la varia "+" supplemento carburante fedex
110000100111      * e il servizio 10,30 "h" mentre ripristiniamo il lasciato
110100100111      * avviso
110200120113      *----------------------------------------------------------------*
110300120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
110400120113      * "R" ac plus
110500120113      * ZTL
110600120113      * Ripristino varia "h" servizio 10:30
110700120113      * come indicato da mail di DE LUCA del 11/01/2012
110800120113      *----------------------------------------------------------------*
110900130124      *----------------------------------------------------------------*
111000130124      * dal 2013 non escludo le varie Fuel e suppl. carburante fedex
111100130124      * perchè vengo calcolate in modo tale da mantenere la ricerca
111200130124      * del prezzo base in base alla data spedizione reale
111300130124      * escludo sempre la varia "R" ac plus
111400130124      * come da nuovo progetto 810 Daniele
111500151207      *----------------------------------------------------------------*
111600151207      * dal 2016 escludo le varie:
111700151207      * "t" diritto di chiamata prenotazione ritiro
111800151207      * "k" pascking list
111900151207      * "R" ac plus
111901170117      *----------------------------------------------------------------*
111902170117      * dal 2017 escludo le varie:
111905170117      * "R" ac plus
112000130124      *----------------------------------------------------------------*
112100151207      * "R" AC PLUS
112200151207     c                   z-add     1             k
112300160118     c     'R'           lookup    dtasv.sv(k)                            30
112400151207     c                   if        %found
112500160125     c                   eval      dtas.tasimv = dtas.tasimv - dtasv.va(k)
112600160118     c                   clear                   dtasv.va(k)
112700160118     c                   clear                   dtasv.sv(k)
112800151207     c                   endif
114800061218
114900160125     c                   z-add     dtas.tasimv   CFTIMVP
115000160125     c                   z-add     dtas.taspor   CFTPORP
115100160118     c                   movea     dtasv.va      vapre
115200160118     c                   movea     dtasv.sv      svpre
115300050607     c                   endsr
115400160118      /free
115500160121       //--------------------------------------------------------------
115600160121       //?Imposto i dati dal file bolle alla DTAS.
115700160121       //--------------------------------------------------------------
115800160121       BEGSR Move_Dati;
115900160121
116000160121         dtas.TASksc = TASksc;
116100160121         dtas.TASpkf = TASpkf;
116200160121         dtas.TASpkc = TASpkc;
116300160121         dtas.TASncp = TASncp;
116400160121         dtas.TASvlf = TASvlf;
116500160121         dtas.TASncl = TASncl;
116501160530         dtas.TASnclb= TASncl;
116600160121         dtas.TASctr = TASctr;
116700160121         dtas.TAScts = TAScts;
116800160121         dtas.TASnzd = TASnzd;
116900160121         dtas.TAScad = TAScad;
117000160121         dtas.TASfin = TASfin;
117100160121         dtas.TASfdn = TASfdn;
117200160121         dtas.TASmct = TASmct;
117300160121         dtas.TASnzm = TASnzm;
117400160121         dtas.TAScam = TAScam;
117500160121         dtas.TASfap = TASfap;
117600160121         dtas.TAStsp = TAStsp;
117700160127         dtas.TAStc1 = TASftc;
117800160121         dtas.TAStc2 = TAStc2;
117900160121         dtas.TASlnp = TASlnp;
118000160121         dtas.TASlna = TASlna;
118100160121         dtas.TAStbl = TAStbl;
118200160125         dtas.TASvas = TASvas;
118300160125         dtas.TASias = TASias;
118400160121         dtas.TASqft = TASqft;
118500160121         dtas.TASdiv = TASdiv;
118600160125         dtas.TASpor = TASpor;
118700160125         dtas.TASimv = TASimv;
118701160530         dtas.TASvlc = TASvlc;
118702160530         dtas.TASncr = TASncr;
118703160530         dtas.TASfvf = TASfvf;
118704160530         dtas.TASpkb = TASpkb;
118800160121
118900160121       ENDSR;
119000160118       //--------------------------------------------------------------
119100160118       //?Cerco la data prima spedizione fatturata.
119200160118       //--------------------------------------------------------------
119300160118       BEGSR CercaData;
119400160118
119500160118         IF  not GiaTassato;
119600160118
119700160118           setll (TASksc:TASctr) TITAS41C;
119800160118           DOW  not %eof(TITAS41C);
119900160118           reade (TASksc:TASctr) TITAS41C;
120000160118           //?Verifico se spedizione tassata fino all'imponibile
120100160118             TAIsvt = '£';
120200160126             chain
120300160126             (t_TASaas:t_TASlnp:t_TASnrs:t_TASnsp:t_TAStbl:TAIsvt) TITAI01L;
120400160118           //?Scarto se tassata fino all'imponibile
120500160118             IF  %found(TITAI01L);
120600160118               iter;
120700160118             ENDIF;
120800160128           //?Solo fatture di sede e con data > alla data di riferimento del lancio
120900160126             IF  t_TASfiv = 998 or t_TASfiv = 996;
121000160128               wdatadsp = t_TASaas * 10000 + t_TASmgs;
121100160128               IF  wdatadsp > pardrf;
121200160128                 exsr NewRcdTICFT;
121300160128                 leave;
121400160128               ENDIF;
121500160118             ENDIF;
121600160118           ENDDO;
121700160201
121800160118         ENDIF;
121900160118
122000160118       ENDSR;
122100160118
122200160118       //--------------------------------------------------------------
122300160118       //?Scrivo nuovo rcd in TICFT.
122400160118       //--------------------------------------------------------------
122500160118       BEGSR NewRcdTICFT;
122600160118
122700160126         chain (t_TASksc:t_TASctr) TICFT01L;
122800160118         IF  not %found(TICFT01L);
122900160118           clear TICFT000;
123000160126           t_CFTksc = t_TASksc;
123100160126           t_CFTctr = t_TASctr;
123200160126           t_CFTdpsf = t_TASaas * 10000 + t_TASmgs;
123300160118           write TICFT000;
123400160118           feod TICFT01L;
123500160126           new_datarif = t_CFTdpsf;
123600160118         ENDIF;
123700160118
123800160118       ENDSR;
123900160118      /end-free
124000130218
124100050609      *************************************************************************
124200050609      * Operazioni iniziali
124300050609      *************************************************************************
124400050609     C     *inzsr        BEGSR
124500050609     c*
124600140408     c     ktas          klist
124700140408     c                   kfld                    paraas
124800140408     c                   kfld                    parmgs
124900060914
125000060914     c     ktai05        klist
125100060914     c                   kfld                    tasaas
125200060914     c                   kfld                    taslnp
125300060914     c                   kfld                    tasnrs
125400060914     c                   kfld                    tasnsp
125500060914     c                   kfld                    tastbl
125600060914
125700060914     c     ktai06        klist
125800060914     c                   kfld                    tasaas
125900060914     c                   kfld                    taslnp
126000060914     c                   kfld                    tasnrs
126100060914     c                   kfld                    tasnsp
126200060914     c                   kfld                    tastbl
126300060914     c                   kfld                    Ksvt
126400060914
126500050609     c     kta7          klist
126600050609     c                   kfld                    tasaas
126700050609     c                   kfld                    taslnp
126800050609     c                   kfld                    tasnrs
126900050609     c                   kfld                    tasnsp
127000050609     c                   kfld                    tastbl
127100050609
127200050609     c     kFiar5        Klist
127300050609     c                   Kfld                    TasAas
127400050609     c                   Kfld                    TasLnp
127500050609     c                   Kfld                    TasNrs
127600050609     c                   Kfld                    TasNsp
127700050609     c                   Kfld                    kAr5Trd
127800050609
127900050609     c     KCSB          KLIST
128000050609     c                   KFLD                    TASAAS
128100050609     c                   KFLD                    TASLNP
128200050609     c                   KFLD                    TASNRS
128300050609     c                   KFLD                    TASNSP
128400050609
128500050609     c     Kcft          KLIST
128600050609     c                   KFLD                    TASAAS
128700050609     c                   KFLD                    TASLNP
128800050609     c                   KFLD                    TASNRS
128900050609     c                   KFLD                    TASNSP
129000050609
129100050609     C     KTAB          KLIST
129200050609     C                   KFLD                    CODUT
129300050609     C                   KFLD                    KCOD
129400081105
129500081105      * Chiave file eventi FNEVB
129600081105     c     Kevb4         Klist
129700081105     c                   Kfld                    Tasaas
129800081105     c                   Kfld                    Taslnp
129900081105     c                   Kfld                    Tasnrs
130000081105     c                   Kfld                    Tasnsp
130100081105     c                   Kfld                    Wcev
130200081105      *
130300050609     c
130400050609     c     *entry        plist
130500050609     c                   parm                    kpjba
130600050609     c                   movel     kpjbu         param
130700060915
130800090430     c                   time                    wtime            14 0
130900090430     c                   move      wtime         wdata
131000090430     c                   movel     wtime         wora
131100060915      * caricamento schiera dei tipi bolla da scartare
131200060915     c                   eval      Kcod  = 'TB'
131300050609     c                   clear                   tbs
131400050609     c                   z-add     0             xx
131500050609     c     ktab          setll     tabel00f
131600050609     c                   do        *hival
131700050609     c     ktab          reade     tabel00f
131800050609     c                   if        %eof(tabel00f)
131900050609     c                   leave
132000050609     c                   endif
132100050609     c                   movel     tbluni        dstb
132200080715      * con richiesta di Bocchi includiamo nella ritassazione / confronto di fatturazione
132300080715      * anche le bolle di recupero  §tbrbl = 'R'
132400080715     c                   if        §tbfcb <> '1'
132500050609     c                   add       1             xx
132600050609     c                   movel     tblkey        tbs(xx)
132700050609     c                   endif
132800050609     c                   enddo
132900060915      * caricamento schiera dei codici bolla da scartare
133000060915     c                   eval      Kcod  = '3A'
133100060915     c                   z-add     0             xx
133200060915     c     ktab          setll     tabel00f
133300060915     c                   do        *hival
133400060915     c     ktab          reade     tabel00f
133500060915     c                   if        %eof(tabel00f)
133600060915     c                   leave
133700060915     c                   endif
133800060915     c                   movel     tbluni        ds3a
133900060915     c                   if        §3asva <> *blanks
134000060915     c                   add       1             xx
134100060915     c                   movel     tblkey        cbo(xx)
134200060915    3c                   endif
134300060915     c                   enddo
134400151207      * caricamento schiera particlarità consegna
134500151207     c                   eval      Kcod  = '7R'
134600151207     c                   z-add     0             xx
134700151207     c     ktab          setll     tabel00f
134800151207     c                   do        *hival
134900151207     c     ktab          reade     tabel00f
135000151207     c                   if        %eof(tabel00f)
135100151207     c                   leave
135200151207     c                   endif
135300151207     c                   if        tblflg = ' '
135400151207     c                   movel     tbluni        ds7r
135500151207     c                   if        §7rpincode = 'S'
135600151207     c                   add       1             xx
135700151207     c                   movel     tblkey        skGma(xx)
135800151207    3c                   endif
135900151207    3c                   endif
136000151207     c                   enddo
136100140409      * GIRO LE DATE PER LE STAMPE
136200140409     C                   move      PARDRF        G08INV
136300140410     c                   eval      G08err = '3'
136400140409     C                   CALL      'XSRDA8'
136500140409     C                   PARM                    WLBDA8
136600140409     C                   Z-ADD     G08DAT        WPARDRF
136700140409      *
136800140409     C                   move      PARDTC        G08INV
136900140410     c                   eval      G08err = '3'
137000140409     C                   CALL      'XSRDA8'
137100140409     C                   PARM                    WLBDA8
137200140409     C                   Z-ADD     G08DAT        WPARDTC
137300140409      *
137400140409     C                   move      PARDAL        G08INV
137500140410     c                   eval      G08err = '3'
137600140409     C                   CALL      'XSRDA8'
137700140409     C                   PARM                    WLBDA8
137800140409     C                   Z-ADD     G08DAT        WPARDAL
137900140409      *
138000140409     C                   move      PARAL         G08INV
138100140410     c                   eval      G08err = '3'
138200140409     C                   CALL      'XSRDA8'
138300140409     C                   PARM                    WLBDA8
138400140409     C                   Z-ADD     G08DAT        WPARAL
138500060915
138600050609     c                   endsr
