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
017000170707     c                   If        tasfiv < 900
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
035700160530     c                   clear                   dtas.tasncr
035800160530     c                   clear                   dtas.tasvlc
035900151023     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
036000160126     c                   eval      dtas.TASPKF=§ar5pktas
036100160126     c                   eval      dtas.TASVLF=§AR5VLTAS
036200151023     c                   endif
036300151023
036400160223     c                   call      'TNSF21R1'
036500060914     c                   PARM                    KPJBA
036600060914     c                   PARM                    DTAS
036700060914     c                   PARM                    Dtasv
036800130124     c                   PARM                    Dtasdta
036900060914
037000050607     c                   select
037100160126      * se torna errore di Manca tariffa
037200160126      * devo memorizzare la data di entrata del cliente
037300160126     c                   when      dtas.taserr = 'M'
037400160126     c                   exsr      CercaData
037500160126     c                   leavesr
037600160118     c                   when      dtas.taserr = *blanks
037700120113      *----------------------------------------------------------------*
037800090216      * se si tratta di confronto fatturazione
037900090216      * cerco la varia "f" (supplemento carburante)
038000090216      * se c'è la tolgo dall'imponibile e dalle varie
038100090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
038200090216      * e la varia "R" AC PLUS assicurazione
038300120113      *----------------------------------------------------------------*
038400100111      * DAL 2010 TOLGO LA VARIA h PER IL SERVIZIO 10:30 E
038500100111      * SUPPLEMENTO CARBURANTE FEDEX MENTRE RIPRISTINO
038600100111      * IL LASCIATO AVVISO
038700120113      *----------------------------------------------------------------*
038800120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
038900120113      * "R" ac plus
039000120113      * ZTL
039100120113      * Ripristino varia "h" servizio 10:30
039200120113      * come indicato da mail di DE LUCA del 11/01/2012
039300151207      *----------------------------------------------------------------*
039400151207      * dal 2016 escludo le varie:
039500151207      * "t" diritto di chiamata prenotazione ritiro
039600151207      * "k" pascking list
039700151207      * "R" ac plus
039800120113      *----------------------------------------------------------------*
039900050607     c                   exsr      SR_riewf
040000160126
040100160126      * imposto se cliente nuovo
040200160126     c                   IF        new_datarif <> pardrf
040300160126     c                   eval      CFTnewtar = 'S'
040400160126     c                   ENDIF
040500080714
040600070207     c                   add       1             tottas
040700130111     c                   write     wfcft000
040800050607     c                   endsl
040900140411
041000160118     c                   clear                   dtas.taserr
041100130124
041200050607     c     e_sf20        endsr
041300050607      ****************************************************
041400050607      ** Carica importi varie da tita7000
041500050607      ****************************************************
041600050607     c     Carta7        Begsr
041700050607      * pulisco le schiere delle varie
041800050607     c                   Do        20            k
041900160118     c                   clear                   dtasv.VA(K)
042000160118     c                   clear                   dtasv.SV(K)
042100050607     c                   Enddo
042200160125
042300160125      * imposto le 3 varie di TITAS
042400160125     c                   eval      dtasv.SV(1) = TASsv1
042500160125     c                   eval      dtasv.VA(1) = TASva1
042600160125     c                   eval      dtasv.SV(2) = TASsv2
042700160125     c                   eval      dtasv.VA(2) = TASva2
042800160125     c                   eval      dtasv.SV(3) = TASsv3
042900160125     c                   eval      dtasv.VA(3) = TASva3
043000160125
043100050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
043200050607     c                   If        tassv3 <> *blanks
043300050607      * mi posiziono su tita7
043400050607     c     Kta7          setll     tita730c
043500050607     c                   if        %equal(tita730c)
043600050607     c                   z-add     3             k
043700050607      **
043800050607     c                   do        *hival
043900050607     c     Kta7          reade     tita730c
044000050607      * fine file
044100050607     c                   if        %eof(tita730c)
044200050607     c                   leave
044300050607     c                   endif
044400050607      * carico le varie
044500050607     c                   add       1             k
044600160118     c                   movel     ta7svn        dtasv.sv(k)
044700160118     c                   z-add     ta7van        dtasv.va(k)
044800050607     c                   enddo
044900050607
045000050607     c                   endif
045100050607
045200050607     c                   endif
045300050607
045400050607     c                   Endsr
045500060914      *
045600060914      *****************************************************************
045700060914      *  Recupero importi di tassazione dal file titai
045800060914      *****************************************************************
045900060914     c     Ric_tai       Begsr
046000060914      *
046100060914     c                   z-add     0             k
046200060914
046300060914     c     ktai05        setll     titai01l
046400060914
046500060914     c                   If        %equal(titai01l)
046600060914
046700060914     c                   do        *hival
046800060914
046900060914     c     ktai05        reade     titai01l
047000060914
047100060914     c                   If        %eof(titai01l)
047200060914     c                   leave
047300060914     c                   endif
047400060914      * imponibile
047500060914     c                   if        taisvt = '£'
047600160125     c                   eval      dtas.tasimv = taivat
047700060914     c                   endif
047800060914      * porto
047900060914     c                   if        taisvt = ' '
048000160125     c                   eval      dtas.taspor = taivat
048100060914     c                   endif
048200060914      * varie
048300060914     c                   if        taisvt <> ' ' and taisvt <> '£'
048400060914     c                   add       1             k
048500160118     c                   eval      dtasv.sv(k) = taisvt
048600160118     c                   eval      dtasv.va(k) = taivat
048700060914     c                   endif
048800060914
048900060914     c                   enddo
049000060914
049100060914     c                   else
049200060914      * se non è pretassata pulisco la divisa
049300160125     c                   clear                   dtas.tasdiv
049400060914     c                   endif
049500060914
049600060914     c                   endsr
049700050607      ****************************************************
049800050609      ** Ricerca Contrassegno
049900050607      ****************************************************
050000050607     c     Carcsb        Begsr
050100050607
050200050607     c     Kcsb          chain     Tncsb03l
050300050607     c                   If        %found(tncsb03l) and
050400050607      * solo per stesso tipo bolla
050500050607     c                             tastbl = csbtbl
050600160118     c                   movel     csbsta        dtas.Tassta
050700160118     c                   z-add     csbcas        dtas.Tascas
050800050607     c                   if        csbfus <> *blanks
050900160118     c                   move      csbfus        dtas.tastic
051000050607     c                   else
051100160118     c                   move      csbtpi        dtas.tastic
051200050607     c                   end
051300050607      * mittente
051400160118     c                   movel     csbvca        dtas.tasvca
051500160118     c                   z-add     csbcmb        dtas.tascmb
051600050607
051700050607     c                   endif
051800050607
051900050607     c                   endsr
052000050607      *************************************************************************
052100060914      * scrittura w-file dati tassazione corrente
052200050607      *************************************************************************
052300060914     C     sr_corrente   BEGSR
052400160125
052500140408     c                   clear                   wfcft000
052600140408     c                   z-add     tasaas        CFTAAS
052700140408     c                   z-add     taslnp        CFTLNP
052800140408     c                   z-add     tasnrs        CFTNRS
052900140408     c                   z-add     tasnsp        CFTNSP
053000140408     c                   z-add     tasksc        CFTKSC
053100140408     c                   z-add     tasctr        CFTCTR
053200140407      * se si tratta di spedizione già fatturata verifico se devo ricalcolare con una
053300140407      * nuova tariffa
053400140410    Ac                   if        pardtc = 0 and tasimv > 0
053500140408     c                   z-add     tasimv        CFTIMVC
053600140408     c                   z-add     taspor        CFTPORC
053700140408
053800140408      * cerco la varia "D" (diritto di fatturazione)
053900140408      * se c'è la tolgo dall'imponibile e dalle varie
054000140408     c                   z-add     1             k
054100160118     c     'D'           lookup    dtasv.sv(k)                            30
054200140408     c                   if        %found
054300160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
054400160118     c                   clear                   dtasv.va(k)
054500160118     c                   clear                   dtasv.sv(k)
054600140408     c                   endif
054700140411      * mi salvo le varie per ricalcolo istat e fuel
054800160118     c                   movea     dtasv.sv      sav_sv
054900160118     c                   movea     dtasv.va      sav_va
055000160127      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
055100140409     c                   z-add     1             k
055200160118     c     'f'           lookup    dtasv.sv(k)                            30
055300140409     c                   if        %found
055400140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
055500160118     c                   eval      cftfuelc = dtasv.va(k)
055600140409     c                   endif
055700140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
055800140409     c                   z-add     1             k
055900160118     c     'L'           lookup    dtasv.sv(k)                            30
056000140409     c                   if        %found
056100160118     c                   eval      cftistatc = dtasv.va(k)
056200140409     c                   endif
056300140408      * "R" AC PLUS da togliere dall'imponibile
056400140408     c                   z-add     1             k
056500160118     c     'R'           lookup    dtasv.sv(k)                            30
056600140408     c                   if        %found
056700160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
056800160118     c                   clear                   dtasv.va(k)
056900160118     c                   clear                   dtasv.sv(k)
057000140408     c                   endif
057100151207
057200140408      * valorizzo le varie della tassazione corrente
057300160118     c                   movea     dtasv.va      vacor
057400160118     c                   movea     dtasv.sv      svcor
057500140408     c
057600140410    Ac                   Endif
057700140408
057800140408      * se imponibile non valorizzato lo calcolo o con la data passata a video o con la data
057900140408      * spedizione
058000140410    Bc                   If        Pardtc > 0 or tasimv = 0
058100140408      * se imponibile valorizzato devo ricalcolare tutto con la nuova data tariffa corrente
058200140408      * quindi pulisco varie imponibile e porto
058300140408     c                   If        tasimv > 0
058400140408
058500140408      * Pulisco i campi dell'assicurazione se calcolata in fattura
058600140408    2c                   IF        Tasfcm = 'F'
058700160125     c                   clear                   dtas.tasias
058800160125     c                   clear                   dtas.tasvas
058900140408    2c                   Endif
059000140408      *
059100140408      * valorizzo la DS DTAS e DTASV
059200140408     c                   clear                   dtasv
059300160125     c                   clear                   dtas.tasimv
059400160125     c                   clear                   dtas.taspor
059500140407      *
059600140408      * cerco in TITAI l'immagine della spedizione prima della tassazione  compreso l'imponibile
059700140407     c                   exsr      RIC_tai
059800140408     c                   endif
059900160126
060000140407      ** Ricerca contrassegno
060100140407    2c                   IF        Tasfca <> *blanks
060200140407     c                   Exsr      Carcsb
060300140407    2c                   Endif
060400140407
060500140407
060600160118     c                   movel(p)  tasaas        dtas.tasdsp
060700160118     c                   move      tasmgs        dtas.tasdsp
060800160118     c                   Z-add     dtas.tasdsp   dtas.tasdct
060900140407
061000140407      * Flag SI NO DDT
061100160118    2C                   If        tasll1 = 'C' or tasll1 = 'S'
061200160118     C                   movel     'S'           dtas.Tasddt
061300140407   x2C                   Else
061400160118     C                   Clear                   dtas.Tasddt
061500140407    2C                   Endif
061600140407     c                   clear                   dtas01
061700140407     c                   movel     tasfbr        §asfbr
061800140407     c                   movel     tascca        §ascca
061900140407      * valorizzo flag email al destinatario
062000140407     c                   movel     tasflo        dtasflo
062100140407     c                   move      §floemd       §asemd
062200151207      * pin code
062300151207     c                   if        tasgma<>*blanks and %lookup(tasgma:skgma)>0
062400151207     c                   eval      §aspin = 'S'
062500151207     c                   endif
062600151207
062700140408      * se data tariffa corrente diversa da zero passo nel flo la data tariffa corrente
062800140408     c                   If        pardtc > 0
062900140408     c                   move      pardtc        §asdrt
063000160118     c                   Z-add     pardtc        dtas.tasdct
063100140407     c                   endif
063200140407
063300160118     c                   eval      dtas.tasflo = dtas01
063400140407
063500151207      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
063600160118     c                   move      §floado       dtas.tasprt
063700151207      * valorizzo il campo per l'addebito stampa PACKING LIST
063800151207     c                   eval      kAr5Trd  = 'GEN'
063900151207     c                   clear                   DAr5gen
064000160118     c     kFiar5        Chain     Fiar531c
064100151207If  1c                   If        %Found(Fiar531c)
064200151207     c                   Movel     Ar5Uni        dAr5Gen
064300151207    1c                   EndIf
064400151207      *
064500151207     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
064600160118     c                   Eval      dtas.TasSpl = 'S'
064700151207     c                   endif
064800151207
064900140407      * Bancali
065000140407    2c                   If        %Subst(TasGva:1:1) = 'B'
065100140407
065200140407     c                   eval      kAr5Trd  = 'BAN'
065300140407     c     kFiar5        Chain     Fiar531c
065400140407If  3c                   If        %Found(Fiar531c)
065500140407     c                   Movel     Ar5Uni        dAr5Ban
065600140407    3c                   EndIf
065700140407      * Bancali
065800160118     c                   Eval      dtas.TasBan = §Ar5Nba + §Ar5Nb2
065900140407    2c                   EndIf
066000140407      **
066100140407      * Colli Originali
066200140407    2c                   If        %Subst(TasGva:1:1) = 'O'
066300140407
066400140407     c                   eval      kAr5Trd  = 'BNB'
066500140407     c     kFiar5        Chain     Fiar531c
066600140407If  3c                   If        %Found(Fiar531c)
066700140407     c                   Movel     Ar5Uni        dAr5Bnb
066800140407    3c                   EndIf
066900140407      * Colli
067000160125     c                   Eval      dtas.TasNcl = §Ar5bcor
067100140407    2c                   EndIf
067200140407      *
067300140407      * verifico se c'è evento
067400140407     c     Kevb4         chain     Fnevb04l
067500140407
067600140408    2c                   If        %found(Fnevb04l)
067700160118     c                   eval      dtas.tasric = 'S'
067800140408    2c                   endif
067900140407
068000140407      **
068100140409     c                   clear                   dtasdta
068200160127      * imposto la data fattura  per calcolo imponibile
068300140409      * calcolo fuel
068400140409     c                   If        tasdft = 0
068500140409     c                   movel(p)  tasaas        tasdtfat
068600140409     c                   move      tasmgs        tasdtfat
068700140409     c                   else
068800140409     c                   z-add     tasdft        tasdtfat
068900140409     c                   endif
069000140409
069100160223     c                   call      'TNSF21R1'
069200140407     c                   parm                    Kpjba
069300140407     c                   parm                    Dtas
069400140407     c                   parm                    Dtasv
069500140409     c                   parm                    dtasdta
069600140409
069700160118     c                   if        dtas.taserr <> ' '
069800140409     c                   leavesr
069900140409     c                   endif
070000140407
070100140407      * tassazione
070200160125     c                   z-add     dtas.tasimv   CFTIMVC
070300160125     c                   z-add     dtas.taspor   CFTPORC
070400140411      * mi salvo le varie per ricalcolo istat e fuel
070500160118     c                   movea     dtasv.sv      sav_sv
070600160118     c                   movea     dtasv.va      sav_va
070700140409      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
070800140409     c                   z-add     1             k
070900160118     c     'f'           lookup    dtasv.sv(k)                            30
071000140409     c                   if        %found
071100140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
071200160118     c                   eval      cftfuelc = dtasv.va(k)
071300140409     c                   endif
071400140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
071500140409     c                   z-add     1             k
071600160118     c     'L'           lookup    dtasv.sv(k)                            30
071700140409     c                   if        %found
071800160118     c                   eval      cftistatc = dtasv.va(k)
071900140409     c                   endif
072000060914
072100130124      * "R" AC PLUS da togliere dall'imponibile
072200090216     c                   z-add     1             k
072300160118     c     'R'           lookup    dtasv.sv(k)                            30
072400090216     c                   if        %found
072500160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
072600160118     c                   clear                   dtasv.va(k)
072700160118     c                   clear                   dtasv.sv(k)
072800130124     c                   endif
072900130124      * valorizzo le varie della tassazione corrente
073000160118     c                   movea     dtasv.va      vacor
073100160118     c                   movea     dtasv.sv      svcor
073200160126    Bc                   endif
073300160126
073400140411      * reimposto le varie come da fatturazione
073500160118     c                   movea     sav_va        dtasv.va
073600160118     c                   movea     sav_sv        dtasv.sv
073700140411      * ricalcolo ISTAT e FUEL
073800140408
073900140408     c                   exsr      car_sf21ds
074000140408
074100050607     c                   endsr
074200130124      **---------------------------------------------------------------**
074300130124      ** Carico la ds per le ritassazioni e le eseguo per ISTAT e FUEL **
074400130124      **---------------------------------------------------------------**
074500130124     c     car_sf21ds    BEGSR
074600130124      *
074700160118     c                   movel     *Blanks       dtas.tastla
074800130124     c                   clear                   dAr5Ban
074900130124     c                   Clear                   dAr5Bnb
075000130124     c                   Clear                   kpjbu
075100130124     c                   clear                   dtasdta
075200130124
075300160118     c                   movel     Tasftc        dtas.tastc1
075400130124      * Flag SI NO DDT
075500160118    1C                   If        tasll1 = 'C' or tasll1 = 'S'
075600160118     C                   movel     'S'           dtas.Tasddt
075700130124   x1C                   Else
075800160118     C                   Clear                   dtas.Tasddt
075900130124    1C                   Endif
076000160126
076100130124     c                   clear                   dtas01
076200130124     c                   movel     tasfbr        §asfbr
076300130124     c                   movel     tascca        §ascca
076400130124      * valorizzo flag email al destinatario
076500130124     c                   movel     tasflo        dtasflo
076600130124     c                   move      §floemd       §asemd
076700151207      * pin code
076800151207     c                   if        tasgma<>*blanks and %lookup(tasgma:skgma)>0
076900151207     c                   eval      §aspin = 'S'
077000151207     c                   endif
077100130124
077200160118     c                   eval      dtas.tasflo = dtas01
077300130124
077400170120      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
077500170120     c                   move      §floado       dtas.tasprt
077600170120      * valorizzo il campo per l'addebito stampa PACKING LIST
077700170120     c                   eval      kAr5Trd  = 'GEN'
077800170120     c                   clear                   DAr5gen
077900170120     c     kFiar5        Chain     Fiar531c
078000170120If  1c                   If        %Found(Fiar531c)
078100170120     c                   Movel     Ar5Uni        dAr5Gen
078200170120    1c                   EndIf
078300170120      *
078400170120     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
078500170120     c                   Eval      dtas.TasSpl = 'S'
078600170120     c                   endif
078700170120
078800160118     c                   clear                   dtas.tasban
078900130124      * Bancali
079000130124    1c                   If        %Subst(TasGva:1:1) = 'B'
079100130124     c                   eval      kAr5Trd  = 'BAN'
079200130124     c     kFiar5        Chain     Fiar531c
079300130124If  1c                   If        %Found(Fiar531c)
079400130124     c                   Movel     Ar5Uni        dAr5Ban
079500130124    1c                   EndIf
079600160118     c                   Eval      dtas.TasBan = §Ar5Nba + §Ar5Nb2
079700130124    1c                   EndIf
079800130124      **
079900130124      * Colli Originali
080000130124    1c                   If        %Subst(TasGva:1:1) = 'O'
080100130124     c                   eval      kAr5Trd  = 'BNB'
080200130124     c     kFiar5        Chain     Fiar531c
080300130124If  1c                   If        %Found(Fiar531c)
080400130124     c                   Movel     Ar5Uni        dAr5Bnb
080500130124    1c                   EndIf
080600160126     c                   Eval      dtas.TasNcl = §Ar5bcor
080700130124    1c                   EndIf
080800130124
080900160118     c                   clear                   dtas.tascas
081000160118     c                   clear                   dtas.tastic
081100160118     c                   clear                   dtas.tasvca
081200160118     c                   clear                   dtas.tascmb
081300130124      * C/Assegno
081400130124    2c                   IF        Tasfca <> *blanks
081500130124     c                   Exsr      Carcsb
081600130124    2c                   Endif
081700140409
081800130124     c     Kevb4         chain     Fnevb04l
081900130124     c                   If        %found(Fnevb04l)
082000160118     c                   eval      dtas.tasric = 'S'
082100130124     c                   endif
082200130124
082300130124      * Assicurazione
082400130124    2c                   IF        Tasfcm = 'F'
082500160125     c                   clear                   dtas.tasias
082600160125     c                   clear                   dtas.tasvas
082700130124    2c                   Endif
082800130124
082900160126      * imposto le date di riferimento x recupero
083000130124      * tariffa con data tariffe
083100160127     c                   z-add     new_datarif   dtas.tasdsp
083200160201     c                   z-add     pardrf        dtas.TASdct
083300160118      * passo la nuova data
083400160201     c                   z-add     new_datarif   tasdtcar
083500130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
083600130124      * calcolo istat
083700130124     c                   movel(p)  tasaas        tasdtist
083800130124     c                   move      tasmgs        tasdtist
083900140207      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
084000140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
084100140409     C                   IF        TASDFT > 0
084200140207     c                   z-add     tasdft        tasdtfat
084300140409     C                   ELSE
084400140409     c                   movel(p)  tasaas        tasdtfat
084500140409     c                   move      tasmgs        tasdtfat
084600140409     C                   ENDIF
084700160126
084800130129      * verifico se fuel pretassato dalla filiale
084900130129     c                   eval      ksvt = 'L'
085000130129     c     Ktai06        chain     titai01l
085100130129      * se non trovato record ricalcolo con stessa base imponibile
085200130129     c                   If        not %found(titai01l)
085300130124      * pulisco ISTAT della fatturazione
085400130124     c                   z-add     1             k
085500160118     c     'L'           lookup    dtasv.sv(k)                            30
085600130124     c                   if        %found
085700160118     c                   clear                   dtasv.va(k)
085800160118     c                   clear                   dtasv.sv(k)
085900130124     c                   endif
086000130124      * pulisco imponibile
086100160126     c                   clear                   dtas.tasimv
086200130124      * imposto la  varia da calcolare
086300160118     c                   eval      dtas.tassva = 'L'
086400160223     c                   call      'TNSF21R1'
086500130124     c                   PARM                    KPJBA
086600130124     c                   PARM                    DTAS
086700130124     c                   PARM                    Dtasv
086800130124     c                   PARM                    Dtasdta
086900130129     c                   endif
087000130124
087100130124      * valorizzo la varia ISTAT ricalcolata
087200130124     c                   z-add     1             k
087300160118     c     'L'           lookup    dtasv.sv(k)                            30
087400130124     c                   if        %found
087500160118     c                   eval      cftistatr = dtasv.va(k)
087600130124     c                   endif
087700130125      * valorizzo imponibile varia ISTAT ricalcolata
087800160126     c                   eval      cftimpist = dtas.tasimv - cftistatr
087900160126
088000130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
088100130124      * calcolo fuel
088200130124     c                   clear                   tasdtist
088300130124     c                   movel(p)  tasaas        tasdtfue
088400130124     c                   move      tasmgs        tasdtfue
088500140409      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
088600140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
088700140409     C                   IF        TASDFT > 0
088800140409     c                   z-add     tasdft        tasdtfat
088900140409     C                   ELSE
089000150709     c                   movel(p)  tasaas        tasdtfat
089100150709     c                   move      tasmgs        tasdtfat
089200140409     C                   ENDIF
089300130129      * verifico se fuel pretassato dalla filiale
089400130129     c                   eval      ksvt = 'f'
089500130129     c     Ktai06        chain     titai01l
089600130129      * se non trovato record ricalcolo con stessa base imponibile
089700130129     c                   If        not %found(titai01l)
089800130124      * pulisco FUEL  della fatturazione
089900130124     c                   z-add     1             k
090000160118     c     'f'           lookup    dtasv.sv(k)                            30
090100130124     c                   if        %found
090200160118     c                   clear                   dtasv.va(k)
090300160118     c                   clear                   dtasv.sv(k)
090400130124     c                   endif
090500130124      * pulisco imponibile
090600160125     c                   clear                   dtas.tasimv
090700130124      * imposto la  varia da calcolare
090800160118     c                   eval      dtas.tassva = 'f'
090900160223     c                   call      'TNSF21R1'
091000130124     c                   PARM                    KPJBA
091100130124     c                   PARM                    DTAS
091200130124     c                   PARM                    Dtasv
091300130124     c                   PARM                    Dtasdta
091400130129     c                   endif
091500130124
091600130124     c                   z-add     1             k
091700160118     c     'f'           lookup    dtasv.sv(k)                            30
091800130124     c                   if        %found
091900160118     c                   eval      cftfuelr = dtasv.va(k)
092000130124     c                   endif
092100130125      * valorizzo imponibile varia FUEL  ricalcolata
092200160126     c                   eval      cftimpfue= dtas.taspor
092300130125      * inoltro
092400130125     c                   z-add     1             k
092500160118     c     '2'           lookup    dtasv.sv(k)                            30
092600130125     c                   if        %found
092700160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
092800130125     c                   endif
092900130125      * isola
093000130125     c                   z-add     1             k
093100160118     c     'J'           lookup    dtasv.sv(k)                            30
093200130125     c                   if        %found
093300160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
093400130125     c                   endif
093500130125      * località disagiata
093600130125     c                   z-add     1             k
093700160118     c     'K'           lookup    dtasv.sv(k)                            30
093800130125     c                   if        %found
093900160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
094000130125     c                   endif
094100130125      * centro storico
094200130125     c                   z-add     1             k
094300160118     c     'Q'           lookup    dtasv.sv(k)                            30
094400130125     c                   if        %found
094500160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
094600130125     c                   endif
094700130125      * espresso/priority
094800130125     c                   z-add     1             k
094900160118     c     'e'           lookup    dtasv.sv(k)                            30
095000130125     c                   if        %found
095100160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
095200130125     c                   endif
095300140207      * Se la data fattura è < o uguale la 30/01/2014 non sommo queste varie aggiuntive
095400140409     c                   if        tasdft > 20140130 or
095500140409     c                             tasdft = 0
095600130125      * h 10,30
095700130125     c                   z-add     1             k
095800160118     c     'h'           lookup    dtasv.sv(k)                            30
095900130125     c                   if        %found
096000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
096100130125     c                   endif
096200140207      * = reso bancali
096300140207     c                   z-add     1             k
096400160118     c     '='           lookup    dtasv.sv(k)                            30
096500140207     c                   if        %found
096600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
096700140207     c                   endif
096800140207      * c lasciato avviso
096900140207     c                   z-add     1             k
097000160118     c     'c'           lookup    dtasv.sv(k)                            30
097100140207     c                   if        %found
097200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
097300140207     c                   endif
097400140207      * A appuntamento
097500140207     c                   z-add     1             k
097600160118     c     'A'           lookup    dtasv.sv(k)                            30
097700140207     c                   if        %found
097800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
097900140207     c                   endif
098000140207      * B consegna DDT
098100140207     c                   z-add     1             k
098200160118     c     'B'           lookup    dtasv.sv(k)                            30
098300140207     c                   if        %found
098400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
098500140207     c                   endif
098600140207      * C facch. arr.
098700140207     c                   z-add     1             k
098800160118     c     'C'           lookup    dtasv.sv(k)                            30
098900140207     c                   if        %found
099000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
099100140207     c                   endif
099200140207      * F fuori misura
099300140207     c                   z-add     1             k
099400160118     c     'F'           lookup    dtasv.sv(k)                            30
099500140207     c                   if        %found
099600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
099700140207     c                   endif
099800140207      * H diritto fisso
099900140207     c                   z-add     1             k
100000160118     c     'H'           lookup    dtasv.sv(k)                            30
100100140207     c                   if        %found
100200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
100300140207     c                   endif
100400140207      * I spese di giacenza
100500140207     c                   z-add     1             k
100600160118     c     'I'           lookup    dtasv.sv(k)                            30
100700140207     c                   if        %found
100800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
100900140207     c                   endif
101000140207      * N anteporto
101100140207     c                   z-add     1             k
101200160118     c     'N'           lookup    dtasv.sv(k)                            30
101300140207     c                   if        %found
101400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
101500140207     c                   endif
101600140207      * P ai piani
101700140207     c                   z-add     1             k
101800160118     c     'P'           lookup    dtasv.sv(k)                            30
101900140207     c                   if        %found
102000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
102100140207     c                   endif
102200140207      * S supermercato
102300140207     c                   z-add     1             k
102400160118     c     'S'           lookup    dtasv.sv(k)                            30
102500140207     c                   if        %found
102600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
102700140207     c                   endif
102800140207      * U ritiro
102900140207     c                   z-add     1             k
103000160118     c     'U'           lookup    dtasv.sv(k)                            30
103100140207     c                   if        %found
103200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
103300140207     c                   endif
103400140207      * X consegne disagiate
103500140207     c                   z-add     1             k
103600160118     c     'X'           lookup    dtasv.sv(k)                            30
103700140207     c                   if        %found
103800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
103900140207     c                   endif
104000151207      * p Pin code
104100151207     c                   z-add     1             k
104200160118     c     'P'           lookup    dtasv.sv(k)                            30
104300151207     c                   if        %found
104400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
104500151207     c                   endif
104600151207      * Z Expo
104700151207     c                   z-add     1             k
104800160118     c     'z'           lookup    dtasv.sv(k)                            30
104900151207     c                   if        %found
105000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
105100151207     c                   endif
105200151207      * t diritto di chiamata prenotazione telefonica
105300151207     c                   z-add     1             k
105400160118     c     't'           lookup    dtasv.sv(k)                            30
105500151207     c                   if        %found
105600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
105700151207     c                   endif
105800151207      * k PACKING LIST
105900151207     c                   z-add     1             k
106000160118     c     'k'           lookup    dtasv.sv(k)                            30
106100151207     c                   if        %found
106200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
106300151207     c                   endif
106400140207
106500140207     c                   endif
106600130124
106700130124     c                   endsr
106800050607      *************************************************************************
106900060914      * scrittura w-file dati tassazione precdeente
107000050607      *************************************************************************
107100060914     C     sr_riewf      BEGSR
107200061218
107300061218      * cerco la varia "f" (supplemento carburante)
107400061218      * se c'è la tolgo dall'imponibile e dalle varie
107500090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
107600090216      * e la varia "R" AC PLUS assicurazione
107700100108      * dal 2010 togliamo anche la varia "+" supplemento carburante fedex
107800100111      * e il servizio 10,30 "h" mentre ripristiniamo il lasciato
107900100111      * avviso
108000120113      *----------------------------------------------------------------*
108100120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
108200120113      * "R" ac plus
108300120113      * ZTL
108400120113      * Ripristino varia "h" servizio 10:30
108500120113      * come indicato da mail di DE LUCA del 11/01/2012
108600120113      *----------------------------------------------------------------*
108700130124      *----------------------------------------------------------------*
108800130124      * dal 2013 non escludo le varie Fuel e suppl. carburante fedex
108900130124      * perchè vengo calcolate in modo tale da mantenere la ricerca
109000130124      * del prezzo base in base alla data spedizione reale
109100130124      * escludo sempre la varia "R" ac plus
109200130124      * come da nuovo progetto 810 Daniele
109300151207      *----------------------------------------------------------------*
109400151207      * dal 2016 escludo le varie:
109500151207      * "t" diritto di chiamata prenotazione ritiro
109600151207      * "k" pascking list
109700151207      * "R" ac plus
109800170117      *----------------------------------------------------------------*
109900170117      * dal 2017 escludo le varie:
110000170117      * "R" ac plus
110100130124      *----------------------------------------------------------------*
110200151207      * "R" AC PLUS
110300151207     c                   z-add     1             k
110400160118     c     'R'           lookup    dtasv.sv(k)                            30
110500151207     c                   if        %found
110600160125     c                   eval      dtas.tasimv = dtas.tasimv - dtasv.va(k)
110700160118     c                   clear                   dtasv.va(k)
110800160118     c                   clear                   dtasv.sv(k)
110900151207     c                   endif
111000061218
111100160125     c                   z-add     dtas.tasimv   CFTIMVP
111200160125     c                   z-add     dtas.taspor   CFTPORP
111300160118     c                   movea     dtasv.va      vapre
111400160118     c                   movea     dtasv.sv      svpre
111500050607     c                   endsr
111600160118      /free
111700160121       //--------------------------------------------------------------
111800160121       //?Imposto i dati dal file bolle alla DTAS.
111900160121       //--------------------------------------------------------------
112000160121       BEGSR Move_Dati;
112100160121
112200160121         dtas.TASksc = TASksc;
112300160121         dtas.TASpkf = TASpkf;
112400160121         dtas.TASpkc = TASpkc;
112500160121         dtas.TASncp = TASncp;
112600160121         dtas.TASvlf = TASvlf;
112700160121         dtas.TASncl = TASncl;
112800160530         dtas.TASnclb= TASncl;
112900160121         dtas.TASctr = TASctr;
113000160121         dtas.TAScts = TAScts;
113100160121         dtas.TASnzd = TASnzd;
113200160121         dtas.TAScad = TAScad;
113300160121         dtas.TASfin = TASfin;
113400160121         dtas.TASfdn = TASfdn;
113500160121         dtas.TASmct = TASmct;
113600160121         dtas.TASnzm = TASnzm;
113700160121         dtas.TAScam = TAScam;
113800160121         dtas.TASfap = TASfap;
113900160121         dtas.TAStsp = TAStsp;
114000160127         dtas.TAStc1 = TASftc;
114100160121         dtas.TAStc2 = TAStc2;
114200160121         dtas.TASlnp = TASlnp;
114300160121         dtas.TASlna = TASlna;
114400160121         dtas.TAStbl = TAStbl;
114500160125         dtas.TASvas = TASvas;
114600160125         dtas.TASias = TASias;
114700160121         dtas.TASqft = TASqft;
114800160121         dtas.TASdiv = TASdiv;
114900160125         dtas.TASpor = TASpor;
115000160125         dtas.TASimv = TASimv;
115100160530         dtas.TASvlc = TASvlc;
115200160530         dtas.TASncr = TASncr;
115300160530         dtas.TASfvf = TASfvf;
115400160530         dtas.TASpkb = TASpkb;
115500160121
115600160121       ENDSR;
115700160118       //--------------------------------------------------------------
115800160118       //?Cerco la data prima spedizione fatturata.
115900160118       //--------------------------------------------------------------
116000160118       BEGSR CercaData;
116100160118
116200160118         IF  not GiaTassato;
116300160118
116400160118           setll (TASksc:TASctr) TITAS41C;
116500160118           DOW  not %eof(TITAS41C);
116600160118           reade (TASksc:TASctr) TITAS41C;
116700160118           //?Verifico se spedizione tassata fino all'imponibile
116800160118             TAIsvt = '£';
116900160126             chain
117000160126             (t_TASaas:t_TASlnp:t_TASnrs:t_TASnsp:t_TAStbl:TAIsvt) TITAI01L;
117100160118           //?Scarto se tassata fino all'imponibile
117200160118             IF  %found(TITAI01L);
117300160118               iter;
117400160118             ENDIF;
117500160128           //?Solo fatture di sede e con data > alla data di riferimento del lancio
117600170707             IF  t_TASfiv >= 900;
117700160128               wdatadsp = t_TASaas * 10000 + t_TASmgs;
117701171115             //per ora non devo scrivere nuove tariffe se il confronto è relativo al 31/12/16
117800171115               IF  wdatadsp > pardrf and pardrf <> 20161231;
117900160128                 exsr NewRcdTICFT;
118000160128                 leave;
118100160128               ENDIF;
118200160118             ENDIF;
118300160118           ENDDO;
118400160201
118500160118         ENDIF;
118600160118
118700160118       ENDSR;
118800160118
118900160118       //--------------------------------------------------------------
119000160118       //?Scrivo nuovo rcd in TICFT.
119100160118       //--------------------------------------------------------------
119200160118       BEGSR NewRcdTICFT;
119300160118
119400160126         chain (t_TASksc:t_TASctr) TICFT01L;
119500160118         IF  not %found(TICFT01L);
119600160118           clear TICFT000;
119700160126           t_CFTksc = t_TASksc;
119800160126           t_CFTctr = t_TASctr;
119900160126           t_CFTdpsf = t_TASaas * 10000 + t_TASmgs;
120000160118           write TICFT000;
120100160118           feod TICFT01L;
120200160126           new_datarif = t_CFTdpsf;
120300160118         ENDIF;
120400160118
120500160118       ENDSR;
120600160118      /end-free
120700130218
120800050609      *************************************************************************
120900050609      * Operazioni iniziali
121000050609      *************************************************************************
121100050609     C     *inzsr        BEGSR
121200050609     c*
121300140408     c     ktas          klist
121400140408     c                   kfld                    paraas
121500140408     c                   kfld                    parmgs
121600060914
121700060914     c     ktai05        klist
121800060914     c                   kfld                    tasaas
121900060914     c                   kfld                    taslnp
122000060914     c                   kfld                    tasnrs
122100060914     c                   kfld                    tasnsp
122200060914     c                   kfld                    tastbl
122300060914
122400060914     c     ktai06        klist
122500060914     c                   kfld                    tasaas
122600060914     c                   kfld                    taslnp
122700060914     c                   kfld                    tasnrs
122800060914     c                   kfld                    tasnsp
122900060914     c                   kfld                    tastbl
123000060914     c                   kfld                    Ksvt
123100060914
123200050609     c     kta7          klist
123300050609     c                   kfld                    tasaas
123400050609     c                   kfld                    taslnp
123500050609     c                   kfld                    tasnrs
123600050609     c                   kfld                    tasnsp
123700050609     c                   kfld                    tastbl
123800050609
123900050609     c     kFiar5        Klist
124000050609     c                   Kfld                    TasAas
124100050609     c                   Kfld                    TasLnp
124200050609     c                   Kfld                    TasNrs
124300050609     c                   Kfld                    TasNsp
124400050609     c                   Kfld                    kAr5Trd
124500050609
124600050609     c     KCSB          KLIST
124700050609     c                   KFLD                    TASAAS
124800050609     c                   KFLD                    TASLNP
124900050609     c                   KFLD                    TASNRS
125000050609     c                   KFLD                    TASNSP
125100050609
125200050609     c     Kcft          KLIST
125300050609     c                   KFLD                    TASAAS
125400050609     c                   KFLD                    TASLNP
125500050609     c                   KFLD                    TASNRS
125600050609     c                   KFLD                    TASNSP
125700050609
125800050609     C     KTAB          KLIST
125900050609     C                   KFLD                    CODUT
126000050609     C                   KFLD                    KCOD
126100081105
126200081105      * Chiave file eventi FNEVB
126300081105     c     Kevb4         Klist
126400081105     c                   Kfld                    Tasaas
126500081105     c                   Kfld                    Taslnp
126600081105     c                   Kfld                    Tasnrs
126700081105     c                   Kfld                    Tasnsp
126800081105     c                   Kfld                    Wcev
126900081105      *
127000050609     c
127100050609     c     *entry        plist
127200050609     c                   parm                    kpjba
127300050609     c                   movel     kpjbu         param
127400060915
127500090430     c                   time                    wtime            14 0
127600090430     c                   move      wtime         wdata
127700090430     c                   movel     wtime         wora
127800060915      * caricamento schiera dei tipi bolla da scartare
127900060915     c                   eval      Kcod  = 'TB'
128000050609     c                   clear                   tbs
128100050609     c                   z-add     0             xx
128200050609     c     ktab          setll     tabel00f
128300050609     c                   do        *hival
128400050609     c     ktab          reade     tabel00f
128500050609     c                   if        %eof(tabel00f)
128600050609     c                   leave
128700050609     c                   endif
128800050609     c                   movel     tbluni        dstb
128900080715      * con richiesta di Bocchi includiamo nella ritassazione / confronto di fatturazione
129000080715      * anche le bolle di recupero  §tbrbl = 'R'
129100080715     c                   if        §tbfcb <> '1'
129200050609     c                   add       1             xx
129300050609     c                   movel     tblkey        tbs(xx)
129400050609     c                   endif
129500050609     c                   enddo
129600060915      * caricamento schiera dei codici bolla da scartare
129700060915     c                   eval      Kcod  = '3A'
129800060915     c                   z-add     0             xx
129900060915     c     ktab          setll     tabel00f
130000060915     c                   do        *hival
130100060915     c     ktab          reade     tabel00f
130200060915     c                   if        %eof(tabel00f)
130300060915     c                   leave
130400060915     c                   endif
130500060915     c                   movel     tbluni        ds3a
130600060915     c                   if        §3asva <> *blanks
130700060915     c                   add       1             xx
130800060915     c                   movel     tblkey        cbo(xx)
130900060915    3c                   endif
131000060915     c                   enddo
131100151207      * caricamento schiera particlarità consegna
131200151207     c                   eval      Kcod  = '7R'
131300151207     c                   z-add     0             xx
131400151207     c     ktab          setll     tabel00f
131500151207     c                   do        *hival
131600151207     c     ktab          reade     tabel00f
131700151207     c                   if        %eof(tabel00f)
131800151207     c                   leave
131900151207     c                   endif
132000151207     c                   if        tblflg = ' '
132100151207     c                   movel     tbluni        ds7r
132200151207     c                   if        §7rpincode = 'S'
132300151207     c                   add       1             xx
132400151207     c                   movel     tblkey        skGma(xx)
132500151207    3c                   endif
132600151207    3c                   endif
132700151207     c                   enddo
132800140409      * GIRO LE DATE PER LE STAMPE
132900140409     C                   move      PARDRF        G08INV
133000140410     c                   eval      G08err = '3'
133100140409     C                   CALL      'XSRDA8'
133200140409     C                   PARM                    WLBDA8
133300140409     C                   Z-ADD     G08DAT        WPARDRF
133400140409      *
133500140409     C                   move      PARDTC        G08INV
133600140410     c                   eval      G08err = '3'
133700140409     C                   CALL      'XSRDA8'
133800140409     C                   PARM                    WLBDA8
133900140409     C                   Z-ADD     G08DAT        WPARDTC
134000140409      *
134100140409     C                   move      PARDAL        G08INV
134200140410     c                   eval      G08err = '3'
134300140409     C                   CALL      'XSRDA8'
134400140409     C                   PARM                    WLBDA8
134500140409     C                   Z-ADD     G08DAT        WPARDAL
134600140409      *
134700140409     C                   move      PARAL         G08INV
134800140410     c                   eval      G08err = '3'
134900140409     C                   CALL      'XSRDA8'
135000140409     C                   PARM                    WLBDA8
135100140409     C                   Z-ADD     G08DAT        WPARAL
135200060915
135300050609     c                   endsr
