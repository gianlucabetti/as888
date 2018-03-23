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
001900140407     fticfq2p   o    e             printer oflind(*in90)
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
007800050607     D Flgta7          s              1  0
007900050609     d codut           s              1  0 Inz(1)
008000060915     D kcod            s                   like(tblcod)
008100060914     D ksvt            s                   like(taisvt)
008200150528     d ksccod          s              4  0                                      ultimi 4 byte codcli
008300081105     D Wcev            s                   like(evbcev) inz('RIC')
008400060914     D wmm             s              2  0
008500070207     D totspe          s              9  0
008600070207     D tottas          s              9  0
008700130110     D totsmt          s              9  0
008800140408     d antepo          s                   like(tasimv)
008900160118
009000160118     d sav_TASksc      s                   like(TASksc) inz
009100160118     d sav_TASctr      s                   like(TASctr) inz
009200160118     d new_datarif     s              8s 0 inz
009300160118     d GiaTassato      s               n   inz(*off)
009400160128     d wdatadsp        s              8s 0 inz
009500060914      *
009600060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
009700060914      *
009800060914      * codici bolla da scartare
009900060914     D tbs             s              2    dim(50)
010000060915      * codici bolla che prevedono la tassazione di una singola varia
010100060915     D cbo             s              2    dim(50)
010200130124      * sigle varie salvate
010300130124     D sav_sv          s              1    dim(20)
010400130124      * importo varie salvate
010500130124     D sav_va          s             11  3 dim(20)
010600151207      ** Sk part.consegna con abilitazione PIN Code
010700151207     d skGMA           s              2a   dim(20)
010800060915      *
010900160118      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
011000160118      /free
011100160118         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011200160118
011300160118       // Leggo il file delle bolle per Data Spedizione e Cliente/Tariffe
011400160118         exec sql
011500160118         declare C1 cursor for
011600160118         select * from titas00f where
011700160126         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :parmgsl
011800160118         union
011900160118         select * from titas10f where
012000160126         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :parmgsl
012100160118         union
012200160118         select * from titasp0f where
012300160126         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :parmgsl
012400160126         order by TASksc, TASctr
012500160118         for read only;
012600160118
012700160118         exec sql open C1;
012800160118
012900160118         DOW  sqlcod = *zeros;
013000160118           clear dtas;
013100160118           clear dtasv;
013200160118           clear dtasdta;
013300160118           exec sql fetch C1 into :TITASDS;
013400160126
013500160126       // A rottura di Cliente/Tariffa chiudo il TNSF21R
013600160126       // per il recupero della tariffa di cartello con la data giusta
013700160126         IF  sav_TASksc <> TASksc or sav_TASctr <> TASctr;
013800160126           IF  sav_TASksc > 0;
013900160126      /end-free
014000160126     c                   eval      dtas.TAStla = 'C'
014100160126     c                   CALL      'TNSF21R'
014200160126     c                   PARM                    KPJBA
014300160126     c                   PARM                    DTAS
014400160126     c                   PARM                    DTASV
014500160126     c                   PARM                    DTASDTA
014600160126      /free
014700160126           ENDIF;
014800160126       // recupero anche la data di entrata del cliente dal TICFT
014900160126           chain (TASksc:TASctr) TICFT01L;
015000160126           IF  %found(TICFT01L);
015100160126             IF  t_CFTdpsf > pardrf;
015200160126               new_datarif = pardrf;
015300160126             ELSE;
015400160126               new_datarif = t_CFTdpsf;
015500160126             ENDIF;
015600160126           ELSE;
015700160126             new_datarif = pardrf;
015800160126           ENDIF;
015900160126           sav_TASksc = TASksc;
016000160126           sav_TASctr = TASctr;
016100160127           GiaTassato = *off;
016200160126         ENDIF;
016300160118      /end-free
016400160126
016500040119
016600160118     c                   movel(p)  tasaas        dtas.tasdsp
016700160118     c                   move      tasmgs        dtas.tasdsp
016800160126
016900150721      * scarto le bolle che non sono state fatturate
017000150721     c                   If        tasfic = ' '
017100150721     c                   iter
017200150721     c                   endif
017300060915
017400060915      * scarto  se si tratta di codice bolla che prevede solo una varia
017500060918     c                   z-add     1             kk                2 0
017600060915     c     tascbo        lookup    cbo(kk)                                05
017700060915     c                   if        *in05
017800060915     c                   iter
017900060915     c                   endif
018000060914
018100060914      * scarto le bolle che sono state fatturate in filiale
018200140408     c                   If        tasfiv <> 998 and tasfiv <> 996
018300140408     c                               and tasfiv > 0
018400060914     c                   iter
018500060914     c                   endif
018600060914
018700140407      * verifico se tassata fino all'imponibile in TITAI se già registrate su TITAI
018800060914     c                   eval      ksvt = '£'
018900060914     c     Ktai06        chain     titai01l
019000060914      * se trovato record imponibile  scarto la bolla
019100060914     c                   If        %found(titai01l)
019200060914     c                   iter
019300060914     c                   endif
019400060914     c
019500140408      * verifico se tassata fino all'imponibile con TASIMV > 0 scarto
019600140408     c                   If        TASIMV > 0 and tasfiv = 0
019700140407     c                   iter
019800140407     c                   endif
019900140407     c
020000060914
020100111025      * scarto bolle di  c/servizi, storni
020200050609     c     tastbl        lookup    tbs                                    30
020300140409     c                   if        *in30
020400140409     c                   iter
020500140409     c                   endif
020600140409
020700160126      * se presenti varia "N" o "&" + 88888888
020800160126      * in TITAI scarto la bolla
020900140409     c                   eval      ksvt = 'N'
021000140409     c     Ktai06        chain     titai01l
021100140409      * se trovato record imponibile  scarto la bolla
021200140409     c                   If        %found(titai01l)  and taivat = 88888888
021300140409     c                   iter
021400140409     c                   endif
021500140409
021600140409     c                   eval      ksvt = '&'
021700140409     c     Ktai06        chain     titai01l
021800140409      * se trovato record imponibile  scarto la bolla
021900140409     c                   If        %found(titai01l)  and taivat = 88888888
022000140409     c                   iter
022100140409     c                   endif
022200140409
022300150528      * verifico se è una bolla non tassata fino l'imponibile se ksc 8888 o 9999
022400150528      * la scarto
022500150528      *
022600150528    2c                   if        tasimv = 0
022700150528     c                   move      tasksc        ksccod
022800150528    2c                   if        ksccod = 9999 or ksccod = 8888
022900150528     c                   iter
023000150528    3c                   endif
023100150528    3c                   endif
023200160125
023300160125      * carico la DTAS con i campi del TITAS
023400160125     c                   exsr      Move_Dati
023500150528
023600140409      * carico le varie presenti
023700140409     c                   Exsr      Carta7
023800140409      *
023900140409      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
024000140409      * con VARIA 'N' valorizzata con 888888 la scarto
024100140409      *
024200140409    2c                   if        tasimv = 0 and tasfbr = 'S'
024300140409     c                   z-add     1             xx
024400160118     c     'N'           lookup    dtasv.sv(xx)                           30
024500160118    3c                   if        *in30 and dtasv.va(xx) =  88888888
024600140409     c                   iter
024700140409    3c                   endif
024800140409    2c                   endif
024900140409      *
025000140409      * verifico se è esiste la varia & con importo uguale a 8888888 la scarto
025100140409      *
025200140409    2c                   if        tasimv = 0
025300140409     c                   z-add     1             xx
025400160118     c     '&'           lookup    dtasv.sv(xx)                           30
025500160118    3c                   if        *in30 and dtasv.va(xx) =  88888888
025600140409     c                   iter
025700140409    3c                   endif
025800140409    2c                   endif
025900060914
026000140407      * carico la tassazione corrente  se imponibile > 0
026100060914     c                   exsr      sr_corrente
026200060914
026300140411      * SOMMO IL NUMERO DI SPEDIZIONE CHE SONO STATE  PRESE IN CONSIDEAZIONE
026400140411     c                   add       1             totspe
026500160126
026600160126      * se dalla tassazione precedente (per fuel e istat) ci sono stati errori di manca tariffa
026700160126      * devo memorizzare la data di entrata del cliente
026800160126      * e provare a ritassare con questa nuova data
026900160126     c                   IF        dtas.TASerr = 'M' and
027000160126     c                             not GiaTassato
027100160126     c                   exsr      CercaData
027200160126     c                   eval      GiaTassato = *on
027300160126      * reimposto le varie come da fatturazione
027400160126     c                   movea     sav_va        dtasv.va
027500160126     c                   movea     sav_sv        dtasv.sv
027600160126      * ricalcolo ISTAT e FUEL
027700160126     c                   exsr      car_sf21ds
027800160126     c                   ENDIF
027900160126
028000140409      * eseguo la tassazione precedente se non ci sono stati errori nella tassazione corrente
028100160118     c                   if        dtas.taserr = ' '
028200130124     c                   exsr      sr_sf21
028300160127      * se dalla nuova tassazione torna errore di tipo M = Manca tariffa
028400160126      * devo ritassare in modo da recuperare la nuova data di riferimento da passare
028500160126      * al tnsf21r
028600160126     c                   IF        dtas.TASerr = 'M'
028700160126     c                             and not GiaTassato
028800160126     c                   clear                   dtas.TASerr
028900160126     c                   eval      GiaTassato = *on
029000160126     c                   exsr      sr_sf21
029100160126     c                   ENDIF
029200140409     c                   endif
029300060914
029400050607     c                   enddo
029500160118      /free
029600160118         exec sql close C1;
029700160118      /end-free
029800050526      *
029900060914      *
030000130110      * conto le spedizioni che non sono state tassate
030100130110     c                   eval      totsmt = totspe - tottas
030200070207      * stampo  la fine elaborazione
030300120213     c                   write     cf92s1
030400070207
030500040119     c                   eval      *inLR = *on
030600060914      *
030700160118     c                   movel     'C'           dtas.tastla
030800130124     C                   CALL      'TNSF21R'
030900050603     C                   PARM                    KPJBA
031000050603     C                   PARM                    DTAS
031100050603     C                   PARM                    DTASV
031200130124     C                   PARM                    DTASDTA
031300050614     c*
031400050614     c                   movel(p)  param         kpjbu
031500040331      *
031600050526      **-------------------------------------------------------------**
031700130124      ** Richiamo pgm tassaz. tnsf21r                                **
031800050526      **-------------------------------------------------------------**
031900130124     c     sr_sf21       BEGSR
032000050526      *
032100160118     c                   movel     *Blanks       dtas.tastla
032200050603     c                   Clear                   kpjbu
032300140409     c                   clear                   dtasdta
032400140409     c                   clear                   dtasv
032500160126     c                   Clear                   dtas.taspor
032600140409      * Assicurazione
032700140409    2c                   IF        Tasfcm = 'F'
032800160126     c                   clear                   dtas.tasias
032900160126     c                   clear                   dtas.tasvas
033000140409    2c                   Endif
033100140409
033200140409      * recupero i valori  già tassati
033300140409     c                   exsr      Ric_tai
033400140409
033500160126     c                   Clear                   dtas.tasimv
033600160127     c                   clear                   dtas.tassva
033700130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
033800130124      * tariffa con data tariffe
033900160127     c                   z-add     new_datarif   dtas.tasdsp
034000160118      * passo la nuova data
034100160118     c                   z-add     new_datarif   dtas.TASdct
034200130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
034300130124      * calcolo istat e calcolo fuel
034400160126     c                   eval      tasdtist = new_datarif
034500130124     c                   movel(p)  tasaas        tasdtfue
034600130124     c                   move      tasmgs        tasdtfue
034700140204      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
034800140204      * calcolo fuel
034900140409     c                   If        tasdft = 0
035000140409     c                   movel(p)  tasaas        tasdtfat
035100140409     c                   move      tasmgs        tasdtfat
035200140409     c                   else
035300140204     c                   z-add     tasdft        tasdtfat
035400140409     c                   endif
035500060914
035600151023      * se ho record FAT nel file Fiar5 imposto il peso e volume usato in tassazione
035700151023      * nel campo taspkf in modo da tassare con lo stesso peso
035800151023     c                   clear                   dAr5Fat
035900151023     c                   eval      kAr5Trd  = 'FAT'
036000151023     c     kFiar5        Chain     Fiar531c
036100151023If  3c                   If        %Found(Fiar531c)
036200151023     c                   Movel     Ar5Uni        dAr5Fat
036300151023    3c                   EndIf
036400151023     c* non presente il record "FAT" --> tasso con peso e vollume presi da titas
036500151023     c                   if        §AR5PKTAS>0
036600160126     c                   clear                   dtas.TASPKC
036700160126     c                   clear                   dtas.tasncp
036800151023     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
036900160126     c                   eval      dtas.TASPKF=§ar5pktas
037000160126     c                   eval      dtas.TASVLF=§AR5VLTAS
037100151023     c                   endif
037200151023
037300130124     c                   call      'TNSF21R'
037400060914     c                   PARM                    KPJBA
037500060914     c                   PARM                    DTAS
037600060914     c                   PARM                    Dtasv
037700130124     c                   PARM                    Dtasdta
037800060914
037900050607     c                   select
038000160126      * se torna errore di Manca tariffa
038100160126      * devo memorizzare la data di entrata del cliente
038200160126     c                   when      dtas.taserr = 'M'
038300160126     c                   exsr      CercaData
038400160126     c                   leavesr
038500160118     c                   when      dtas.taserr = *blanks
038600120113      *----------------------------------------------------------------*
038700090216      * se si tratta di confronto fatturazione
038800090216      * cerco la varia "f" (supplemento carburante)
038900090216      * se c'è la tolgo dall'imponibile e dalle varie
039000090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
039100090216      * e la varia "R" AC PLUS assicurazione
039200120113      *----------------------------------------------------------------*
039300100111      * DAL 2010 TOLGO LA VARIA h PER IL SERVIZIO 10:30 E
039400100111      * SUPPLEMENTO CARBURANTE FEDEX MENTRE RIPRISTINO
039500100111      * IL LASCIATO AVVISO
039600120113      *----------------------------------------------------------------*
039700120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
039800120113      * "R" ac plus
039900120113      * ZTL
040000120113      * Ripristino varia "h" servizio 10:30
040100120113      * come indicato da mail di DE LUCA del 11/01/2012
040200151207      *----------------------------------------------------------------*
040300151207      * dal 2016 escludo le varie:
040400151207      * "t" diritto di chiamata prenotazione ritiro
040500151207      * "k" pascking list
040600151207      * "R" ac plus
040700120113      *----------------------------------------------------------------*
040800050607     c                   exsr      SR_riewf
040900160126
041000160126      * imposto se cliente nuovo
041100160126     c                   IF        new_datarif <> pardrf
041200160126     c                   eval      CFTnewtar = 'S'
041300160126     c                   ENDIF
041400080714
041500070207     c                   add       1             tottas
041600130111     c                   write     wfcft000
041700050607     c                   endsl
041800140411
041900160118     c                   clear                   dtas.taserr
042000130124
042100050607     c     e_sf20        endsr
042200050607      ****************************************************
042300050607      ** Carica importi varie da tita7000
042400050607      ****************************************************
042500050607     c     Carta7        Begsr
042600050607      * pulisco le schiere delle varie
042700050607     c                   Do        20            k
042800160118     c                   clear                   dtasv.VA(K)
042900160118     c                   clear                   dtasv.SV(K)
043000050607     c                   Enddo
043100160125
043200160125      * imposto le 3 varie di TITAS
043300160125     c                   eval      dtasv.SV(1) = TASsv1
043400160125     c                   eval      dtasv.VA(1) = TASva1
043500160125     c                   eval      dtasv.SV(2) = TASsv2
043600160125     c                   eval      dtasv.VA(2) = TASva2
043700160125     c                   eval      dtasv.SV(3) = TASsv3
043800160125     c                   eval      dtasv.VA(3) = TASva3
043900160125
044000060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
044100050607     c                   clear                   flgta7
044200050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
044300050607     c                   If        tassv3 <> *blanks
044400050607      * mi posiziono su tita7
044500050607     c     Kta7          setll     tita730c
044600050607     c                   if        %equal(tita730c)
044700050607     c                   z-add     3             k
044800050607      **
044900050607     c                   do        *hival
045000050607     c     Kta7          reade     tita730c
045100050607      * fine file
045200050607     c                   if        %eof(tita730c)
045300050607     c                   leave
045400050607     c                   endif
045500050607      * carico le varie
045600050607     c                   add       1             k
045700160118     c                   movel     ta7svn        dtasv.sv(k)
045800160118     c                   z-add     ta7van        dtasv.va(k)
045900050607     c                   z-add     1             flgta7
046000050607     c                   enddo
046100050607
046200050607     c                   endif
046300050607
046400050607     c                   endif
046500050607
046600050607     c                   Endsr
046700060914      *
046800060914      *****************************************************************
046900060914      *  Recupero importi di tassazione dal file titai
047000060914      *****************************************************************
047100060914     c     Ric_tai       Begsr
047200060914      *
047300060914     c                   z-add     0             k
047400060914
047500060914     c     ktai05        setll     titai01l
047600060914
047700060914     c                   If        %equal(titai01l)
047800060914
047900060914     c                   do        *hival
048000060914
048100060914     c     ktai05        reade     titai01l
048200060914
048300060914     c                   If        %eof(titai01l)
048400060914     c                   leave
048500060914     c                   endif
048600060914      * imponibile
048700060914     c                   if        taisvt = '£'
048800160125     c                   eval      dtas.tasimv = taivat
048900060914     c                   endif
049000060914      * porto
049100060914     c                   if        taisvt = ' '
049200160125     c                   eval      dtas.taspor = taivat
049300060914     c                   endif
049400060914      * varie
049500060914     c                   if        taisvt <> ' ' and taisvt <> '£'
049600060914     c                   add       1             k
049700160118     c                   eval      dtasv.sv(k) = taisvt
049800160118     c                   eval      dtasv.va(k) = taivat
049900060914     c                   endif
050000060914
050100060914     c                   enddo
050200060914
050300060914     c                   else
050400060914      * se non è pretassata pulisco la divisa
050500160125     c                   clear                   dtas.tasdiv
050600060914     c                   endif
050700060914
050800060914     c                   endsr
050900050607      ****************************************************
051000050609      ** Ricerca Contrassegno
051100050607      ****************************************************
051200050607     c     Carcsb        Begsr
051300050607
051400050607     c     Kcsb          chain     Tncsb03l
051500050607     c                   If        %found(tncsb03l) and
051600050607      * solo per stesso tipo bolla
051700050607     c                             tastbl = csbtbl
051800160118     c                   movel     csbsta        dtas.Tassta
051900160118     c                   z-add     csbcas        dtas.Tascas
052000050607     c                   if        csbfus <> *blanks
052100160118     c                   move      csbfus        dtas.tastic
052200050607     c                   else
052300160118     c                   move      csbtpi        dtas.tastic
052400050607     c                   end
052500050607      * mittente
052600160118     c                   movel     csbvca        dtas.tasvca
052700160118     c                   z-add     csbcmb        dtas.tascmb
052800050607
052900050607     c                   endif
053000050607
053100050607     c                   endsr
053200050607      *************************************************************************
053300060914      * scrittura w-file dati tassazione corrente
053400050607      *************************************************************************
053500060914     C     sr_corrente   BEGSR
053600160125
053700140408     c                   clear                   wfcft000
053800140408     c                   z-add     tasaas        CFTAAS
053900140408     c                   z-add     taslnp        CFTLNP
054000140408     c                   z-add     tasnrs        CFTNRS
054100140408     c                   z-add     tasnsp        CFTNSP
054200140408     c                   z-add     tasksc        CFTKSC
054300140408     c                   z-add     tasctr        CFTCTR
054400140407      * se si tratta di spedizione già fatturata verifico se devo ricalcolare con una
054500140407      * nuova tariffa
054600140410    Ac                   if        pardtc = 0 and tasimv > 0
054700140408     c                   z-add     tasimv        CFTIMVC
054800140408     c                   z-add     taspor        CFTPORC
054900140408
055000140408      * cerco la varia "D" (diritto di fatturazione)
055100140408      * se c'è la tolgo dall'imponibile e dalle varie
055200140408     c                   z-add     1             k
055300160118     c     'D'           lookup    dtasv.sv(k)                            30
055400140408     c                   if        %found
055500160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
055600160118     c                   clear                   dtasv.va(k)
055700160118     c                   clear                   dtasv.sv(k)
055800140408     c                   endif
055900140411      * mi salvo le varie per ricalcolo istat e fuel
056000160118     c                   movea     dtasv.sv      sav_sv
056100160118     c                   movea     dtasv.va      sav_va
056200160127      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
056300140409     c                   z-add     1             k
056400160118     c     'f'           lookup    dtasv.sv(k)                            30
056500140409     c                   if        %found
056600140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
056700160118     c                   eval      cftfuelc = dtasv.va(k)
056800140409     c                   endif
056900140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
057000140409     c                   z-add     1             k
057100160118     c     'L'           lookup    dtasv.sv(k)                            30
057200140409     c                   if        %found
057300160118     c                   eval      cftistatc = dtasv.va(k)
057400140409     c                   endif
057500140408      * "R" AC PLUS da togliere dall'imponibile
057600140408     c                   z-add     1             k
057700160118     c     'R'           lookup    dtasv.sv(k)                            30
057800140408     c                   if        %found
057900160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
058000160118     c                   clear                   dtasv.va(k)
058100160118     c                   clear                   dtasv.sv(k)
058200140408     c                   endif
058300151207
058400151207      * "t" diritto di chiamata prenotazione ritiro
058500151207     c                   z-add     1             k
058600160118     c     't'           lookup    dtasv.sv(k)                            30
058700151207     c                   if        %found
058800160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
058900160118     c                   clear                   dtasv.va(k)
059000160118     c                   clear                   dtasv.sv(k)
059100151207     c                   endif
059200151207
059300151207     c                   z-add     1             k
059400151207      * "k" PACKING LIST
059500151207     c                   z-add     1             k
059600160118     c     'k'           lookup    dtasv.sv(k)                            30
059700151207     c                   if        %found
059800160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
059900160118     c                   clear                   dtasv.va(k)
060000160118     c                   clear                   dtasv.sv(k)
060100151207     c                   endif
060200151207
060300140408      * valorizzo le varie della tassazione corrente
060400160118     c                   movea     dtasv.va      vacor
060500160118     c                   movea     dtasv.sv      svcor
060600140408     c
060700140410    Ac                   Endif
060800140408
060900140408      * se imponibile non valorizzato lo calcolo o con la data passata a video o con la data
061000140408      * spedizione
061100140410    Bc                   If        Pardtc > 0 or tasimv = 0
061200140408      * se imponibile valorizzato devo ricalcolare tutto con la nuova data tariffa corrente
061300140408      * quindi pulisco varie imponibile e porto
061400140408     c                   If        tasimv > 0
061500140408
061600140408      * Pulisco i campi dell'assicurazione se calcolata in fattura
061700140408    2c                   IF        Tasfcm = 'F'
061800160125     c                   clear                   dtas.tasias
061900160125     c                   clear                   dtas.tasvas
062000140408    2c                   Endif
062100140408      *
062200140408      * valorizzo la DS DTAS e DTASV
062300140408     c                   clear                   dtasv
062400160125     c                   clear                   dtas.tasimv
062500160125     c                   clear                   dtas.taspor
062600140407      *
062700140408      * cerco in TITAI l'immagine della spedizione prima della tassazione  compreso l'imponibile
062800140407     c                   exsr      RIC_tai
062900140408     c                   endif
063000160126
063100140407      ** Ricerca contrassegno
063200140407    2c                   IF        Tasfca <> *blanks
063300140407     c                   Exsr      Carcsb
063400140407    2c                   Endif
063500140407
063600140407
063700160118     c                   movel(p)  tasaas        dtas.tasdsp
063800160118     c                   move      tasmgs        dtas.tasdsp
063900160118     c                   Z-add     dtas.tasdsp   dtas.tasdct
064000140407
064100140407      * Flag SI NO DDT
064200160118    2C                   If        tasll1 = 'C' or tasll1 = 'S'
064300160118     C                   movel     'S'           dtas.Tasddt
064400140407   x2C                   Else
064500160118     C                   Clear                   dtas.Tasddt
064600140407    2C                   Endif
064700140407     c                   clear                   dtas01
064800140407     c                   movel     tasfbr        §asfbr
064900140407     c                   movel     tascca        §ascca
065000140407      * valorizzo flag email al destinatario
065100140407     c                   movel     tasflo        dtasflo
065200140407     c                   move      §floemd       §asemd
065300151207      * pin code
065400151207     c                   if        tasgma<>*blanks and %lookup(tasgma:skgma)>0
065500151207     c                   eval      §aspin = 'S'
065600151207     c                   endif
065700151207
065800140408      * se data tariffa corrente diversa da zero passo nel flo la data tariffa corrente
065900140408     c                   If        pardtc > 0
066000140408     c                   move      pardtc        §asdrt
066100160118     c                   Z-add     pardtc        dtas.tasdct
066200140407     c                   endif
066300140407
066400160118     c                   eval      dtas.tasflo = dtas01
066500140407
066600151207      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
066700160118     c                   move      §floado       dtas.tasprt
066800151207      * valorizzo il campo per l'addebito stampa PACKING LIST
066900151207     c                   eval      kAr5Trd  = 'GEN'
067000151207     c                   clear                   DAr5gen
067100160118     c     kFiar5        Chain     Fiar531c
067200151207If  1c                   If        %Found(Fiar531c)
067300151207     c                   Movel     Ar5Uni        dAr5Gen
067400151207    1c                   EndIf
067500151207      *
067600151207     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
067700160118     c                   Eval      dtas.TasSpl = 'S'
067800151207     c                   endif
067900151207
068000140407      * Bancali
068100140407    2c                   If        %Subst(TasGva:1:1) = 'B'
068200140407
068300140407     c                   eval      kAr5Trd  = 'BAN'
068400140407     c     kFiar5        Chain     Fiar531c
068500140407If  3c                   If        %Found(Fiar531c)
068600140407     c                   Movel     Ar5Uni        dAr5Ban
068700140407    3c                   EndIf
068800140407      * Bancali
068900160118     c                   Eval      dtas.TasBan = §Ar5Nba + §Ar5Nb2
069000140407    2c                   EndIf
069100140407      **
069200140407      * Colli Originali
069300140407    2c                   If        %Subst(TasGva:1:1) = 'O'
069400140407
069500140407     c                   eval      kAr5Trd  = 'BNB'
069600140407     c     kFiar5        Chain     Fiar531c
069700140407If  3c                   If        %Found(Fiar531c)
069800140407     c                   Movel     Ar5Uni        dAr5Bnb
069900140407    3c                   EndIf
070000140407      * Colli
070100160125     c                   Eval      dtas.TasNcl = §Ar5bcor
070200140407    2c                   EndIf
070300140407      *
070400140407      * verifico se c'è evento
070500140407     c     Kevb4         chain     Fnevb04l
070600140407
070700140408    2c                   If        %found(Fnevb04l)
070800160118     c                   eval      dtas.tasric = 'S'
070900140408    2c                   endif
071000140407
071100140407      **
071200140409     c                   clear                   dtasdta
071300160127      * imposto la data fattura  per calcolo imponibile
071400140409      * calcolo fuel
071500140409     c                   If        tasdft = 0
071600140409     c                   movel(p)  tasaas        tasdtfat
071700140409     c                   move      tasmgs        tasdtfat
071800140409     c                   else
071900140409     c                   z-add     tasdft        tasdtfat
072000140409     c                   endif
072100140409
072200140409     c                   call      'TNSF21R'
072300140407     c                   parm                    Kpjba
072400140407     c                   parm                    Dtas
072500140407     c                   parm                    Dtasv
072600140409     c                   parm                    dtasdta
072700140409
072800160118     c                   if        dtas.taserr <> ' '
072900140409     c                   leavesr
073000140409     c                   endif
073100140407
073200140407      * tassazione
073300160125     c                   z-add     dtas.tasimv   CFTIMVC
073400160125     c                   z-add     dtas.taspor   CFTPORC
073500140411      * mi salvo le varie per ricalcolo istat e fuel
073600160118     c                   movea     dtasv.sv      sav_sv
073700160118     c                   movea     dtasv.va      sav_va
073800140409      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
073900140409     c                   z-add     1             k
074000160118     c     'f'           lookup    dtasv.sv(k)                            30
074100140409     c                   if        %found
074200140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
074300160118     c                   eval      cftfuelc = dtasv.va(k)
074400140409     c                   endif
074500140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
074600140409     c                   z-add     1             k
074700160118     c     'L'           lookup    dtasv.sv(k)                            30
074800140409     c                   if        %found
074900160118     c                   eval      cftistatc = dtasv.va(k)
075000140409     c                   endif
075100060914
075200130124      * "R" AC PLUS da togliere dall'imponibile
075300090216     c                   z-add     1             k
075400160118     c     'R'           lookup    dtasv.sv(k)                            30
075500090216     c                   if        %found
075600160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
075700160118     c                   clear                   dtasv.va(k)
075800160118     c                   clear                   dtasv.sv(k)
075900130124     c                   endif
076000151207
076100151207      * "t" diritto di chiamata prenotazione ritiro
076200151207     c                   z-add     1             k
076300160118     c     't'           lookup    dtasv.sv(k)                            30
076400151207     c                   if        %found
076500160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
076600160118     c                   clear                   dtasv.va(k)
076700160118     c                   clear                   dtasv.sv(k)
076800151207     c                   endif
076900151207
077000151207     c                   z-add     1             k
077100151207      * "k" PACKING LIST
077200151207     c                   z-add     1             k
077300160118     c     'k'           lookup    dtasv.sv(k)                            30
077400151207     c                   if        %found
077500160118     c                   eval      cftimvc = cftimvc - dtasv.va(k)
077600160118     c                   clear                   dtasv.va(k)
077700160118     c                   clear                   dtasv.sv(k)
077800151207     c                   endif
077900130124      * valorizzo le varie della tassazione corrente
078000160118     c                   movea     dtasv.va      vacor
078100160118     c                   movea     dtasv.sv      svcor
078200160126    Bc                   endif
078300160126
078400140411      * reimposto le varie come da fatturazione
078500160118     c                   movea     sav_va        dtasv.va
078600160118     c                   movea     sav_sv        dtasv.sv
078700140411      * ricalcolo ISTAT e FUEL
078800140408
078900140408     c                   exsr      car_sf21ds
079000140408
079100050607     c                   endsr
079200130124      **---------------------------------------------------------------**
079300130124      ** Carico la ds per le ritassazioni e le eseguo per ISTAT e FUEL **
079400130124      **---------------------------------------------------------------**
079500130124     c     car_sf21ds    BEGSR
079600130124      *
079700160118     c                   movel     *Blanks       dtas.tastla
079800130124     c                   clear                   dAr5Ban
079900130124     c                   Clear                   dAr5Bnb
080000130124     c                   Clear                   kpjbu
080100130124     c                   clear                   dtasdta
080200130124
080300160118     c                   movel     Tasftc        dtas.tastc1
080400130124      * Flag SI NO DDT
080500160118    1C                   If        tasll1 = 'C' or tasll1 = 'S'
080600160118     C                   movel     'S'           dtas.Tasddt
080700130124   x1C                   Else
080800160118     C                   Clear                   dtas.Tasddt
080900130124    1C                   Endif
081000160126
081100130124     c                   clear                   dtas01
081200130124     c                   movel     tasfbr        §asfbr
081300130124     c                   movel     tascca        §ascca
081400130124      * valorizzo flag email al destinatario
081500130124     c                   movel     tasflo        dtasflo
081600130124     c                   move      §floemd       §asemd
081700151207      * pin code
081800151207     c                   if        tasgma<>*blanks and %lookup(tasgma:skgma)>0
081900151207     c                   eval      §aspin = 'S'
082000151207     c                   endif
082100130124
082200160118     c                   eval      dtas.tasflo = dtas01
082300130124
082400160118     c                   clear                   dtas.tasban
082500130124      * Bancali
082600130124    1c                   If        %Subst(TasGva:1:1) = 'B'
082700130124     c                   eval      kAr5Trd  = 'BAN'
082800130124     c     kFiar5        Chain     Fiar531c
082900130124If  1c                   If        %Found(Fiar531c)
083000130124     c                   Movel     Ar5Uni        dAr5Ban
083100130124    1c                   EndIf
083200160118     c                   Eval      dtas.TasBan = §Ar5Nba + §Ar5Nb2
083300130124    1c                   EndIf
083400130124      **
083500130124      * Colli Originali
083600130124    1c                   If        %Subst(TasGva:1:1) = 'O'
083700130124     c                   eval      kAr5Trd  = 'BNB'
083800130124     c     kFiar5        Chain     Fiar531c
083900130124If  1c                   If        %Found(Fiar531c)
084000130124     c                   Movel     Ar5Uni        dAr5Bnb
084100130124    1c                   EndIf
084200160126     c                   Eval      dtas.TasNcl = §Ar5bcor
084300130124    1c                   EndIf
084400130124
084500160118     c                   clear                   dtas.tascas
084600160118     c                   clear                   dtas.tastic
084700160118     c                   clear                   dtas.tasvca
084800160118     c                   clear                   dtas.tascmb
084900130124      * C/Assegno
085000130124    2c                   IF        Tasfca <> *blanks
085100130124     c                   Exsr      Carcsb
085200130124    2c                   Endif
085300140409
085400130124     c     Kevb4         chain     Fnevb04l
085500130124     c                   If        %found(Fnevb04l)
085600160118     c                   eval      dtas.tasric = 'S'
085700130124     c                   endif
085800130124
085900130124      * Assicurazione
086000130124    2c                   IF        Tasfcm = 'F'
086100160125     c                   clear                   dtas.tasias
086200160125     c                   clear                   dtas.tasvas
086300130124    2c                   Endif
086400130124
086500160126      * imposto le date di riferimento x recupero
086600130124      * tariffa con data tariffe
086700160127     c                   z-add     new_datarif   dtas.tasdsp
086800160118      * passo la nuova data
086900160118     c                   z-add     new_datarif   dtas.TASdct
087000130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
087100130124      * calcolo istat
087200130124     c                   movel(p)  tasaas        tasdtist
087300130124     c                   move      tasmgs        tasdtist
087400140207      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
087500140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
087600140409     C                   IF        TASDFT > 0
087700140207     c                   z-add     tasdft        tasdtfat
087800140409     C                   ELSE
087900140409     c                   movel(p)  tasaas        tasdtfat
088000140409     c                   move      tasmgs        tasdtfat
088100140409     C                   ENDIF
088200160126
088300130129      * verifico se fuel pretassato dalla filiale
088400130129     c                   eval      ksvt = 'L'
088500130129     c     Ktai06        chain     titai01l
088600130129      * se non trovato record ricalcolo con stessa base imponibile
088700130129     c                   If        not %found(titai01l)
088800130124      * pulisco ISTAT della fatturazione
088900130124     c                   z-add     1             k
089000160118     c     'L'           lookup    dtasv.sv(k)                            30
089100130124     c                   if        %found
089200160118     c                   clear                   dtasv.va(k)
089300160118     c                   clear                   dtasv.sv(k)
089400130124     c                   endif
089500130124      * pulisco imponibile
089600160126     c                   clear                   dtas.tasimv
089700130124      * imposto la  varia da calcolare
089800160118     c                   eval      dtas.tassva = 'L'
089900140409     c                   call      'TNSF21R'
090000130124     c                   PARM                    KPJBA
090100130124     c                   PARM                    DTAS
090200130124     c                   PARM                    Dtasv
090300130124     c                   PARM                    Dtasdta
090400130129     c                   endif
090500130124
090600130124      * valorizzo la varia ISTAT ricalcolata
090700130124     c                   z-add     1             k
090800160118     c     'L'           lookup    dtasv.sv(k)                            30
090900130124     c                   if        %found
091000160118     c                   eval      cftistatr = dtasv.va(k)
091100130124     c                   endif
091200130125      * valorizzo imponibile varia ISTAT ricalcolata
091300160126     c                   eval      cftimpist = dtas.tasimv - cftistatr
091400160126
091500130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
091600130124      * calcolo fuel
091700130124     c                   clear                   tasdtist
091800130124     c                   movel(p)  tasaas        tasdtfue
091900130124     c                   move      tasmgs        tasdtfue
092000140409      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
092100140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
092200140409     C                   IF        TASDFT > 0
092300140409     c                   z-add     tasdft        tasdtfat
092400140409     C                   ELSE
092500150709     c                   movel(p)  tasaas        tasdtfat
092600150709     c                   move      tasmgs        tasdtfat
092700140409     C                   ENDIF
092800130129      * verifico se fuel pretassato dalla filiale
092900130129     c                   eval      ksvt = 'f'
093000130129     c     Ktai06        chain     titai01l
093100130129      * se non trovato record ricalcolo con stessa base imponibile
093200130129     c                   If        not %found(titai01l)
093300130124      * pulisco FUEL  della fatturazione
093400130124     c                   z-add     1             k
093500160118     c     'f'           lookup    dtasv.sv(k)                            30
093600130124     c                   if        %found
093700160118     c                   clear                   dtasv.va(k)
093800160118     c                   clear                   dtasv.sv(k)
093900130124     c                   endif
094000130124      * pulisco imponibile
094100160125     c                   clear                   dtas.tasimv
094200130124      * imposto la  varia da calcolare
094300160118     c                   eval      dtas.tassva = 'f'
094400140409     c                   call      'TNSF21R'
094500130124     c                   PARM                    KPJBA
094600130124     c                   PARM                    DTAS
094700130124     c                   PARM                    Dtasv
094800130124     c                   PARM                    Dtasdta
094900130129     c                   endif
095000130124
095100130124     c                   z-add     1             k
095200160118     c     'f'           lookup    dtasv.sv(k)                            30
095300130124     c                   if        %found
095400160118     c                   eval      cftfuelr = dtasv.va(k)
095500130124     c                   endif
095600130125      * valorizzo imponibile varia FUEL  ricalcolata
095700160126     c                   eval      cftimpfue= dtas.taspor
095800130125      * inoltro
095900130125     c                   z-add     1             k
096000160118     c     '2'           lookup    dtasv.sv(k)                            30
096100130125     c                   if        %found
096200160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
096300130125     c                   endif
096400130125      * isola
096500130125     c                   z-add     1             k
096600160118     c     'J'           lookup    dtasv.sv(k)                            30
096700130125     c                   if        %found
096800160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
096900130125     c                   endif
097000130125      * località disagiata
097100130125     c                   z-add     1             k
097200160118     c     'K'           lookup    dtasv.sv(k)                            30
097300130125     c                   if        %found
097400160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
097500130125     c                   endif
097600130125      * centro storico
097700130125     c                   z-add     1             k
097800160118     c     'Q'           lookup    dtasv.sv(k)                            30
097900130125     c                   if        %found
098000160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
098100130125     c                   endif
098200130125      * espresso/priority
098300130125     c                   z-add     1             k
098400160118     c     'e'           lookup    dtasv.sv(k)                            30
098500130125     c                   if        %found
098600160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
098700130125     c                   endif
098800140207      * Se la data fattura è < o uguale la 30/01/2014 non sommo queste varie aggiuntive
098900140409     c                   if        tasdft > 20140130 or
099000140409     c                             tasdft = 0
099100130125      * h 10,30
099200130125     c                   z-add     1             k
099300160118     c     'h'           lookup    dtasv.sv(k)                            30
099400130125     c                   if        %found
099500160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
099600130125     c                   endif
099700140207      * = reso bancali
099800140207     c                   z-add     1             k
099900160118     c     '='           lookup    dtasv.sv(k)                            30
100000140207     c                   if        %found
100100160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
100200140207     c                   endif
100300140207      * c lasciato avviso
100400140207     c                   z-add     1             k
100500160118     c     'c'           lookup    dtasv.sv(k)                            30
100600140207     c                   if        %found
100700160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
100800140207     c                   endif
100900140207      * A appuntamento
101000140207     c                   z-add     1             k
101100160118     c     'A'           lookup    dtasv.sv(k)                            30
101200140207     c                   if        %found
101300160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
101400140207     c                   endif
101500140207      * B consegna DDT
101600140207     c                   z-add     1             k
101700160118     c     'B'           lookup    dtasv.sv(k)                            30
101800140207     c                   if        %found
101900160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
102000140207     c                   endif
102100140207      * C facch. arr.
102200140207     c                   z-add     1             k
102300160118     c     'C'           lookup    dtasv.sv(k)                            30
102400140207     c                   if        %found
102500160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
102600140207     c                   endif
102700140207      * F fuori misura
102800140207     c                   z-add     1             k
102900160118     c     'F'           lookup    dtasv.sv(k)                            30
103000140207     c                   if        %found
103100160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
103200140207     c                   endif
103300140207      * H diritto fisso
103400140207     c                   z-add     1             k
103500160118     c     'H'           lookup    dtasv.sv(k)                            30
103600140207     c                   if        %found
103700160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
103800140207     c                   endif
103900140207      * I spese di giacenza
104000140207     c                   z-add     1             k
104100160118     c     'I'           lookup    dtasv.sv(k)                            30
104200140207     c                   if        %found
104300160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
104400140207     c                   endif
104500140207      * N anteporto
104600140207     c                   z-add     1             k
104700160118     c     'N'           lookup    dtasv.sv(k)                            30
104800140207     c                   if        %found
104900160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
105000140207     c                   endif
105100140207      * P ai piani
105200140207     c                   z-add     1             k
105300160118     c     'P'           lookup    dtasv.sv(k)                            30
105400140207     c                   if        %found
105500160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
105600140207     c                   endif
105700140207      * S supermercato
105800140207     c                   z-add     1             k
105900160118     c     'S'           lookup    dtasv.sv(k)                            30
106000140207     c                   if        %found
106100160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
106200140207     c                   endif
106300140207      * U ritiro
106400140207     c                   z-add     1             k
106500160118     c     'U'           lookup    dtasv.sv(k)                            30
106600140207     c                   if        %found
106700160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
106800140207     c                   endif
106900140207      * X consegne disagiate
107000140207     c                   z-add     1             k
107100160118     c     'X'           lookup    dtasv.sv(k)                            30
107200140207     c                   if        %found
107300160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
107400140207     c                   endif
107500151207      * p Pin code
107600151207     c                   z-add     1             k
107700160118     c     'P'           lookup    dtasv.sv(k)                            30
107800151207     c                   if        %found
107900160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
108000151207     c                   endif
108100151207      * Z Expo
108200151207     c                   z-add     1             k
108300160118     c     'z'           lookup    dtasv.sv(k)                            30
108400151207     c                   if        %found
108500160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
108600151207     c                   endif
108700151207      * t diritto di chiamata prenotazione telefonica
108800151207     c                   z-add     1             k
108900160118     c     't'           lookup    dtasv.sv(k)                            30
109000151207     c                   if        %found
109100160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
109200151207     c                   endif
109300151207      * k PACKING LIST
109400151207     c                   z-add     1             k
109500160118     c     'k'           lookup    dtasv.sv(k)                            30
109600151207     c                   if        %found
109700160118     c                   eval      cftimpfue= cftimpfue + dtasv.va(k)
109800151207     c                   endif
109900140207
110000140207     c                   endif
110100130124
110200130124     c                   endsr
110300050607      *************************************************************************
110400060914      * scrittura w-file dati tassazione precdeente
110500050607      *************************************************************************
110600060914     C     sr_riewf      BEGSR
110700061218
110800061218      * cerco la varia "f" (supplemento carburante)
110900061218      * se c'è la tolgo dall'imponibile e dalle varie
111000090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
111100090216      * e la varia "R" AC PLUS assicurazione
111200100108      * dal 2010 togliamo anche la varia "+" supplemento carburante fedex
111300100111      * e il servizio 10,30 "h" mentre ripristiniamo il lasciato
111400100111      * avviso
111500120113      *----------------------------------------------------------------*
111600120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
111700120113      * "R" ac plus
111800120113      * ZTL
111900120113      * Ripristino varia "h" servizio 10:30
112000120113      * come indicato da mail di DE LUCA del 11/01/2012
112100120113      *----------------------------------------------------------------*
112200130124      *----------------------------------------------------------------*
112300130124      * dal 2013 non escludo le varie Fuel e suppl. carburante fedex
112400130124      * perchè vengo calcolate in modo tale da mantenere la ricerca
112500130124      * del prezzo base in base alla data spedizione reale
112600130124      * escludo sempre la varia "R" ac plus
112700130124      * come da nuovo progetto 810 Daniele
112800151207      *----------------------------------------------------------------*
112900151207      * dal 2016 escludo le varie:
113000151207      * "t" diritto di chiamata prenotazione ritiro
113100151207      * "k" pascking list
113200151207      * "R" ac plus
113300130124      *----------------------------------------------------------------*
113400151207      * "R" AC PLUS
113500151207     c                   z-add     1             k
113600160118     c     'R'           lookup    dtasv.sv(k)                            30
113700151207     c                   if        %found
113800160125     c                   eval      dtas.tasimv = dtas.tasimv - dtasv.va(k)
113900160118     c                   clear                   dtasv.va(k)
114000160118     c                   clear                   dtasv.sv(k)
114100151207     c                   endif
114200151207
114300151207      * "t" diritto di chiamata prenotazione ritiro
114400151207     c                   z-add     1             k
114500160118     c     't'           lookup    dtasv.sv(k)                            30
114600151207     c                   if        %found
114700160125     c                   eval      dtas.tasimv = dtas.tasimv - dtasv.va(k)
114800160118     c                   clear                   dtasv.va(k)
114900160118     c                   clear                   dtasv.sv(k)
115000151207     c                   endif
115100151207
115200061218     c                   z-add     1             k
115300151207      * "k" PACKING LIST
115400090216     c                   z-add     1             k
115500160118     c     'k'           lookup    dtasv.sv(k)                            30
115600090216     c                   if        %found
115700160125     c                   eval      dtas.tasimv = dtas.tasimv - dtasv.va(k)
115800160118     c                   clear                   dtasv.va(k)
115900160118     c                   clear                   dtasv.sv(k)
116000090216     c                   endif
116100061218
116200160125     c                   z-add     dtas.tasimv   CFTIMVP
116300160125     c                   z-add     dtas.taspor   CFTPORP
116400160118     c                   movea     dtasv.va      vapre
116500160118     c                   movea     dtasv.sv      svpre
116600050607     c                   endsr
116700160118      /free
116800160121       //--------------------------------------------------------------
116900160121       //?Imposto i dati dal file bolle alla DTAS.
117000160121       //--------------------------------------------------------------
117100160121       BEGSR Move_Dati;
117200160121
117300160121         dtas.TASksc = TASksc;
117400160121         dtas.TASpkf = TASpkf;
117500160121         dtas.TASpkc = TASpkc;
117600160121         dtas.TASncp = TASncp;
117700160121         dtas.TASvlf = TASvlf;
117800160121         dtas.TASncl = TASncl;
117900160121         dtas.TASctr = TASctr;
118000160121         dtas.TAScts = TAScts;
118100160121         dtas.TASnzd = TASnzd;
118200160121         dtas.TAScad = TAScad;
118300160121         dtas.TASfin = TASfin;
118400160121         dtas.TASfdn = TASfdn;
118500160121         dtas.TASmct = TASmct;
118600160121         dtas.TASnzm = TASnzm;
118700160121         dtas.TAScam = TAScam;
118800160121         dtas.TASfap = TASfap;
118900160121         dtas.TAStsp = TAStsp;
119000160127         dtas.TAStc1 = TASftc;
119100160121         dtas.TAStc2 = TAStc2;
119200160121         dtas.TASlnp = TASlnp;
119300160121         dtas.TASlna = TASlna;
119400160121         dtas.TAStbl = TAStbl;
119500160125         dtas.TASvas = TASvas;
119600160125         dtas.TASias = TASias;
119700160121         dtas.TASqft = TASqft;
119800160121         dtas.TASdiv = TASdiv;
119900160125         dtas.TASpor = TASpor;
120000160125         dtas.TASimv = TASimv;
120100160121
120200160121       ENDSR;
120300160118       //--------------------------------------------------------------
120400160118       //?Cerco la data prima spedizione fatturata.
120500160118       //--------------------------------------------------------------
120600160118       BEGSR CercaData;
120700160118
120800160118         IF  not GiaTassato;
120900160118
121000160118           setll (TASksc:TASctr) TITAS41C;
121100160118           DOW  not %eof(TITAS41C);
121200160118           reade (TASksc:TASctr) TITAS41C;
121300160118           //?Verifico se spedizione tassata fino all'imponibile
121400160118             TAIsvt = '£';
121500160126             chain
121600160126             (t_TASaas:t_TASlnp:t_TASnrs:t_TASnsp:t_TAStbl:TAIsvt) TITAI01L;
121700160118           //?Scarto se tassata fino all'imponibile
121800160118             IF  %found(TITAI01L);
121900160118               iter;
122000160118             ENDIF;
122100160128           //?Solo fatture di sede e con data > alla data di riferimento del lancio
122200160126             IF  t_TASfiv = 998 or t_TASfiv = 996;
122300160128               wdatadsp = t_TASaas * 10000 + t_TASmgs;
122400160128               IF  wdatadsp > pardrf;
122500160128                 exsr NewRcdTICFT;
122600160128                 leave;
122700160128               ENDIF;
122800160118             ENDIF;
122900160118           ENDDO;
123000160118         ENDIF;
123100160118
123200160118       ENDSR;
123300160118
123400160118       //--------------------------------------------------------------
123500160118       //?Scrivo nuovo rcd in TICFT.
123600160118       //--------------------------------------------------------------
123700160118       BEGSR NewRcdTICFT;
123800160118
123900160126         chain (t_TASksc:t_TASctr) TICFT01L;
124000160118         IF  not %found(TICFT01L);
124100160118           clear TICFT000;
124200160126           t_CFTksc = t_TASksc;
124300160126           t_CFTctr = t_TASctr;
124400160126           t_CFTdpsf = t_TASaas * 10000 + t_TASmgs;
124500160118           write TICFT000;
124600160118           feod TICFT01L;
124700160126           new_datarif = t_CFTdpsf;
124800160118         ENDIF;
124900160118
125000160118       ENDSR;
125100160118      /end-free
125200130218
125300050609      *************************************************************************
125400050609      * Operazioni iniziali
125500050609      *************************************************************************
125600050609     C     *inzsr        BEGSR
125700050609     c*
125800140408     c     ktas          klist
125900140408     c                   kfld                    paraas
126000140408     c                   kfld                    parmgs
126100060914
126200060914     c     ktai05        klist
126300060914     c                   kfld                    tasaas
126400060914     c                   kfld                    taslnp
126500060914     c                   kfld                    tasnrs
126600060914     c                   kfld                    tasnsp
126700060914     c                   kfld                    tastbl
126800060914
126900060914     c     ktai06        klist
127000060914     c                   kfld                    tasaas
127100060914     c                   kfld                    taslnp
127200060914     c                   kfld                    tasnrs
127300060914     c                   kfld                    tasnsp
127400060914     c                   kfld                    tastbl
127500060914     c                   kfld                    Ksvt
127600060914
127700050609     c     kta7          klist
127800050609     c                   kfld                    tasaas
127900050609     c                   kfld                    taslnp
128000050609     c                   kfld                    tasnrs
128100050609     c                   kfld                    tasnsp
128200050609     c                   kfld                    tastbl
128300050609
128400050609     c     kFiar5        Klist
128500050609     c                   Kfld                    TasAas
128600050609     c                   Kfld                    TasLnp
128700050609     c                   Kfld                    TasNrs
128800050609     c                   Kfld                    TasNsp
128900050609     c                   Kfld                    kAr5Trd
129000050609
129100050609     c     KCSB          KLIST
129200050609     c                   KFLD                    TASAAS
129300050609     c                   KFLD                    TASLNP
129400050609     c                   KFLD                    TASNRS
129500050609     c                   KFLD                    TASNSP
129600050609
129700050609     c     Kcft          KLIST
129800050609     c                   KFLD                    TASAAS
129900050609     c                   KFLD                    TASLNP
130000050609     c                   KFLD                    TASNRS
130100050609     c                   KFLD                    TASNSP
130200050609
130300050609     C     KTAB          KLIST
130400050609     C                   KFLD                    CODUT
130500050609     C                   KFLD                    KCOD
130600081105
130700081105      * Chiave file eventi FNEVB
130800081105     c     Kevb4         Klist
130900081105     c                   Kfld                    Tasaas
131000081105     c                   Kfld                    Taslnp
131100081105     c                   Kfld                    Tasnrs
131200081105     c                   Kfld                    Tasnsp
131300081105     c                   Kfld                    Wcev
131400081105      *
131500050609     c
131600050609     c     *entry        plist
131700050609     c                   parm                    kpjba
131800050609     c                   movel     kpjbu         param
131900060915
132000090430     c                   time                    wtime            14 0
132100090430     c                   move      wtime         wdata
132200090430     c                   movel     wtime         wora
132300060915      * caricamento schiera dei tipi bolla da scartare
132400060915     c                   eval      Kcod  = 'TB'
132500050609     c                   clear                   tbs
132600050609     c                   z-add     0             xx
132700050609     c     ktab          setll     tabel00f
132800050609     c                   do        *hival
132900050609     c     ktab          reade     tabel00f
133000050609     c                   if        %eof(tabel00f)
133100050609     c                   leave
133200050609     c                   endif
133300050609     c                   movel     tbluni        dstb
133400080715      * con richiesta di Bocchi includiamo nella ritassazione / confronto di fatturazione
133500080715      * anche le bolle di recupero  §tbrbl = 'R'
133600080715     c                   if        §tbfcb <> '1'
133700050609     c                   add       1             xx
133800050609     c                   movel     tblkey        tbs(xx)
133900050609     c                   endif
134000050609     c                   enddo
134100060915      * caricamento schiera dei codici bolla da scartare
134200060915     c                   eval      Kcod  = '3A'
134300060915     c                   z-add     0             xx
134400060915     c     ktab          setll     tabel00f
134500060915     c                   do        *hival
134600060915     c     ktab          reade     tabel00f
134700060915     c                   if        %eof(tabel00f)
134800060915     c                   leave
134900060915     c                   endif
135000060915     c                   movel     tbluni        ds3a
135100060915     c                   if        §3asva <> *blanks
135200060915     c                   add       1             xx
135300060915     c                   movel     tblkey        cbo(xx)
135400060915    3c                   endif
135500060915     c                   enddo
135600151207      * caricamento schiera particlarità consegna
135700151207     c                   eval      Kcod  = '7R'
135800151207     c                   z-add     0             xx
135900151207     c     ktab          setll     tabel00f
136000151207     c                   do        *hival
136100151207     c     ktab          reade     tabel00f
136200151207     c                   if        %eof(tabel00f)
136300151207     c                   leave
136400151207     c                   endif
136500151207     c                   if        tblflg = ' '
136600151207     c                   movel     tbluni        ds7r
136700151207     c                   if        §7rpincode = 'S'
136800151207     c                   add       1             xx
136900151207     c                   movel     tblkey        skGma(xx)
137000151207    3c                   endif
137100151207    3c                   endif
137200151207     c                   enddo
137300140409      * GIRO LE DATE PER LE STAMPE
137400140409     C                   move      PARDRF        G08INV
137500140410     c                   eval      G08err = '3'
137600140409     C                   CALL      'XSRDA8'
137700140409     C                   PARM                    WLBDA8
137800140409     C                   Z-ADD     G08DAT        WPARDRF
137900140409      *
138000140409     C                   move      PARDTC        G08INV
138100140410     c                   eval      G08err = '3'
138200140409     C                   CALL      'XSRDA8'
138300140409     C                   PARM                    WLBDA8
138400140409     C                   Z-ADD     G08DAT        WPARDTC
138500140409      *
138600140409     C                   move      PARDAL        G08INV
138700140410     c                   eval      G08err = '3'
138800140409     C                   CALL      'XSRDA8'
138900140409     C                   PARM                    WLBDA8
139000140409     C                   Z-ADD     G08DAT        WPARDAL
139100140409      *
139200140409     C                   move      PARAL         G08INV
139300140410     c                   eval      G08err = '3'
139400140409     C                   CALL      'XSRDA8'
139500140409     C                   PARM                    WLBDA8
139600140409     C                   Z-ADD     G08DAT        WPARAL
139700060915
139800050609     c                   endsr
