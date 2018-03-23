000100040119      *===============================================================*
000200160115      * TicfQ4r    * Confronto fatturazione 15 Gennaio 2016
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000160118     fTITAS41C  if   e           k disk
001100060914     fTita730c  if   e           k disk
001200060914     fTitaI01L  if   e           k disk
001300060914     Ffiar531c  if   e           k disk
001400060914     Ftncsb03l  if   e           k disk
001500060914     fwfcft01l  uf a e           k disk
001600060914     Ftabel00f  if   e           k disk
001700081105     Ffnevb04l  if   e           k disk
001800160115     fTICFT01L  if a e           k DISK
001900140407     fticfq2p   o    e             printer oflind(*in90)
002000160115
002100050603     d dAr5Ban       e ds
002200050603     d dAr5Bnb       e ds
002300151023     d dAr5Fat       e ds
002400151207     d dAr5Gen       e ds
002500050526     d kpjba         e ds
002600050609     d dstb          e ds
002700060915     d ds3a          e ds
002800151207     d ds7r          e ds
002900160115
003000110701      ** DS del tasflo del file TITAS00F
003100110701     D DTASFLO       E DS
003200160118     d dtas          e ds                  inz prefix(w_)
003300050527     d  §tasflo      e                     extfld(tasflo)
003400160115
003500060926      ** DS Flag operativi DS DTAS
003600060926     d Dtas01        e ds
003700160115
003800050607      ** DS Calcolo tassazione  - Varie
003900050607     d Dtasv         e ds
004000050607     d  sv                     1     20
004100050607     d                                     Dim(20)                              Sigle varie
004200050607     d  va                    21    140P 3
004300050607     d                                     Dim(20)                              Importi varie
004400160115
004500130124      ** DS data ritassazione DS DTASdta
004600130124     d Dtasdta       e ds
004700160115
004800140409      ** campi di salvataggio x tassazione precedente
004900160115     d W_Dtasv         s                   like(dtasv)
005000140409     d W_taspor        s                   like(taspor)
005100130124
005200050607     d param           ds
005300140407     D  pardrf                 1      8  0
005400140407     D  pardtc                 9     16  0
005500140407     D  pardal                17     24  0
005600140408     D  paraas                17     20  0
005700140408     D  parmgs                21     24  0
005800140407     D  paral                 25     32  0
005900140409
006000140409     D WLBDA8          DS                  INZ
006100140409     D  G08DAT                 1      8  0
006200140409     D  G08INV                 9     16  0
006300140409     D  G08ERR                17     17
006400140409     D  G08TGI                18     22  0
006500140409
006600050607     dwfcftxxds      e ds                  extname(wfcft00f)
006700050607     dsvpre                   31     50    dim(20)
006800050607     dvapre                   51    170p 3 dim(20)
006900050607     dsvcor                  184    203    dim(20)
007000050607     dvacor                  204    323p 3 dim(20)
007100160115
007200160118     dTitasds        e ds                  extname(TITAS00F) prefix(w)
007300040331      *
007400040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
007500050526      *
007600040331      * - Indici di schiera
007700040331     d xx              s              3  0 inz
007800050607     D k               s              5  0                                      indice
007900160115
008000040331      * - Campi di comodo
008100050603     d kAr5Trd         s                   Like(Ar5Trd)
008200060914     d kmgs            s                   Like(TasMgs)
008300050607     D Flgta7          s              1  0
008400050609     d codut           s              1  0 Inz(1)
008500060915     D kcod            s                   like(tblcod)
008600060914     D ksvt            s                   like(taisvt)
008700150528     d ksccod          s              4  0                                      ultimi 4 byte codcli
008800081105     D Wcev            s                   like(evbcev) inz('RIC')
008900060914     D wmm             s              2  0
009000070207     D totspe          s              9  0
009100070207     D tottas          s              9  0
009200130110     D totsmt          s              9  0
009300160115     d antepo          s                   like(tasimv)
009400160115
009500160115     d sav_TASksc      s                   like(TASksc) inz
009600160115     d sav_TASctr      s                   like(TASctr) inz
009700160115     d new_datarif     s              8s 0 inz
009800160115     d New_kscctr      s               n   inz(*off)
009900160118     d GiaTassato      s               n   inz(*off)
010000060914      *
010100060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
010200060914      *
010300060914      * codici bolla da scartare
010400060914     D tbs             s              2    dim(50)
010500060915      * codici bolla che prevedono la tassazione di una singola varia
010600060915     D cbo             s              2    dim(50)
010700130124      * sigle varie salvate
010800130124     D sav_sv          s              1    dim(20)
010900130124      * importo varie salvate
011000130124     D sav_va          s             11  3 dim(20)
011100151207      ** Sk part.consegna con abilitazione PIN Code
011200151207     d skGMA           s              2a   dim(20)
011300160115
011400160115      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
011500160115      /free
011600160115         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011700040119
011800160115       // Leggo il file delle bolle per Data Spedizione e Cliente/Tariffe
011900160115         exec sql
012000160115         declare C1 cursor for
012100160115         select * from titas00f where
012200160115         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :paral
012300160115         union
012400160115         select * from titas10f where
012500160115         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :paral
012600160115         union
012700160115         select * from titasp0f where
012800160115         TASaas = :paraas and TASmgs >= :parmgs and TASmgs <= :paral
012900160115         for read only;
013000160115
013100160115         exec sql open C1;
013200160115
013300160115         DOW  sqlcod = *zeros;
013400160115           clear dtas;
013500160115           clear dtasv;
013600160115           clear dtasdta;
013700160115           clear W_dtasv;
013800160115           clear W_taspor;
013900160118           exec sql fetch C1 into :TITASDS;
014000160115      /end-free
014100160115     C*
014200160118
014300160118     c                   eval      GiaTassato = *off
014400050614     c
014500160118     c                   movel(p)  wtasaas       tasdsp
014600160118     c                   move      wtasmgs       tasdsp
014700150721      * scarto le bolle che non sono state fatturate
014800150721      * ho inserito questa selezione in quanto la maggior parte delle volte che facciamo
014900150721      * il confronto fatturazione lo vogliamo fare sulle bolle già fatturate e non sui manca tariffa
015000150721      * anche se il programma lo potrebbe fare
015100160118     c                   If        wtasfic = ' '
015200150721     c                   iter
015300150721     c                   endif
015400060915
015500060915      * scarto  se si tratta di codice bolla che prevede solo una varia
015600060918     c                   z-add     1             kk                2 0
015700160118     c     wtascbo       lookup    cbo(kk)                                05
015800060915     c                   if        *in05
015900060915     c                   iter
016000060915     c                   endif
016100060914
016200060914      * scarto le bolle che sono state fatturate in filiale
016300160118     c                   If        wtasfiv <> 998 and wtasfiv <> 996
016400160118     c                             and wtasfiv > 0
016500060914     c                   iter
016600060914     c                   endif
016700060914
016800140407      * verifico se tassata fino all'imponibile in TITAI se già registrate su TITAI
016900060914     c                   eval      ksvt = '£'
017000060914     c     Ktai06        chain     titai01l
017100060914      * se trovato record imponibile  scarto la bolla
017200060914     c                   If        %found(titai01l)
017300060914     c                   iter
017400060914     c                   endif
017500060914     c
017600140408      * verifico se tassata fino all'imponibile con TASIMV > 0 scarto
017700160118     c                   If        wTASIMV > 0 and wtasfiv = 0
017800140407     c                   iter
017900140407     c                   endif
018000060914
018100111025      * scarto bolle di  c/servizi, storni
018200160118     c     wtastbl       lookup    tbs                                    30
018300140409     c                   if        *in30
018400140409     c                   iter
018500140409     c                   endif
018600140409
018700151207      * Solo in caso di fatture già contabilizzate verifico la presenza delle varie N e &
018800151207      * in TITAI per poter scartare la bolla
018900160118     C                   IF        wTASFIC <> ' '
019000140409      * se presenti varia "N" o "&" + 88888888 ignoro il record x quelle fatturate
019100140409     c                   eval      ksvt = 'N'
019200140409     c     Ktai06        chain     titai01l
019300140409      * se trovato record imponibile  scarto la bolla
019400140409     c                   If        %found(titai01l)  and taivat = 88888888
019500140409     c                   iter
019600140409     c                   endif
019700140409
019800140409     c                   eval      ksvt = '&'
019900140409     c     Ktai06        chain     titai01l
020000140409      * se trovato record imponibile  scarto la bolla
020100140409     c                   If        %found(titai01l)  and taivat = 88888888
020200140409     c                   iter
020300140409     c                   endif
020400140409
020500140409     c                   endif
020600140409
020700150528      * verifico se è una bolla non tassata fino l'imponibile se ksc 8888 o 9999
020800150528      * la scarto
020900150528      *
021000160118    2c                   if        wtasimv = 0
021100160118     c                   move      wtasksc       ksccod
021200150528    2c                   if        ksccod = 9999 or ksccod = 8888
021300150528     c                   iter
021400150528    3c                   endif
021500150528    3c                   endif
021600150528
021700140409      * carico le varie presenti
021800140409     c                   Exsr      Carta7
021900160118
022000160118     c                   eval      taspkf = Wtaspkf
022100160118     c                   eval      taspkc = Wtaspkc
022200160118     c                   eval      tasvlf = Wtasvlf
022300160118     c                   eval      tasksc = Wtasksc
022400160118     c                   eval      tasncp = Wtasncp
022500160118     c                   eval      tasctr = Wtasctr
022600160118     c                   eval      tascts = Wtascts
022700160118     c                   eval      tasnzd = Wtasnzd
022800160118     c                   eval      tascad = Wtascad
022900160118     c                   eval      tasfin = Wtasfin
023000160118     c                   eval      tasfdn = Wtasfdn
023100160118     c                   eval      tasmct = Wtasmct
023200160118     c                   eval      tasnzm = Wtasnzm
023300160118     c                   eval      tascam = Wtascam
023400160118     c                   eval      tasfap = Wtasfap
023500160118     c                   eval      tastsp = Wtastsp
023600160118     c                   eval      tastc2 = Wtastc2
023700160118     c                   eval      taslnp = Wtaslnp
023800160118     c                   eval      taslna = Wtaslna
023900160118     c                   eval      tastbl = Wtastbl
024000160118     c                   eval      tasqft = Wtasqft
024100160118     c                   eval      tasdiv = Wtasdiv
024200140409      *
024300140409      * verifico se è una bolla non tassata fino l'imponibile , una bolla di reso ,
024400140409      * con VARIA 'N' valorizzata con 888888 la scarto
024500140409      *
024600160118    2c                   if        wtasimv = 0 and wtasfbr = 'S'
024700140409     c                   z-add     1             xx
024800140409     c     'N'           lookup    sv(xx)                                 30
024900140409    3c                   if        *in30 and va(xx) =  88888888
025000140409     c                   iter
025100140409    3c                   endif
025200140409      *
025300140409    2c                   endif
025400140409      *
025500140409      * verifico se è esiste la varia & con importo uguale a 8888888 la scarto
025600140409      *
025700160118    2c                   if        wtasimv = 0
025800140409     c                   z-add     1             xx
025900140409     c     '&'           lookup    sv(xx)                                 30
026000140409    3c                   if        *in30 and va(xx) =  88888888
026100140409     c                   iter
026200140409    3c                   endif
026300140409      *
026400140409    2c                   endif
026500140409      *
026600140409      * prima di eseguire la tassazione corrente   mi salvo i valori di titas
026700160118     c                   eval      W_taspor = wtaspor
026800160118     c                   eval      W_Dtasv = Dtasv
026900060914
027000140407      * carico la tassazione corrente  se imponibile > 0
027100060914     c                   exsr      sr_corrente
027200060914
027300140411      * SOMMO IL NUMERO DI SPEDIZIONE CHE SONO STATE  PRESE IN CONSIDEAZIONE
027400140411     c                   add       1             totspe
027500140409      * eseguo la tassazione precedente se non ci sono stati errori nella tassazione corrente
027600140411
027700140409     c                   if        taserr = ' '
027800130124     c                   exsr      sr_sf21
027900160118      * se errore di tipo M = Manca tariffa
028000160118      * devo ritassare in modo da recuperare la nuova data di riferimento da passare
028100160118      * al tnsf21r
028200160118      * se errore di nuovo non faccio più niente
028300160118     c                   IF        TASerr = 'M'
028400160118     c                             and not GiaTassato
028500160118     c                   clear                   TASerr
028600160118     c                   eval      GiaTassato = *on
028700160118     c                   exsr      sr_sf21
028800160118     c                   ENDIF
028900160118
029000140409     c                   endif
029100160115
029200160115     c                   ENDDO
029300160115      /free
029400160115         exec sql close C1;
029500160115      /end-free
029600050526      *
029700060914      *
029800130110      * conto le spedizioni che non sono state tassate
029900130110     c                   eval      totsmt = totspe - tottas
030000070207      * stampo  la fine elaborazione
030100120213     c                   write     cf92s1
030200070207
030300040119     c                   eval      *inLR = *on
030400060914      *
030500050603     c                   movel     'C'           tastla
030600130124     C                   CALL      'TNSF21R'
030700050603     C                   PARM                    KPJBA
030800050603     C                   PARM                    DTAS
030900050603     C                   PARM                    DTASV
031000130124     C                   PARM                    DTASDTA
031100050614     c*
031200050614     c                   movel(p)  param         kpjbu
031300040331      *
031400050526      **-------------------------------------------------------------**
031500130124      ** Richiamo pgm tassaz. tnsf21r                                **
031600050526      **-------------------------------------------------------------**
031700130124     c     sr_sf21       BEGSR
031800160115
031900160115      /free
032000160115       // se cambio KSC/CTR
032100160115       // cerco se KSC/CTR hanno una data di entrata nel file TICFT
032200160118         IF  sav_TASksc <> wTASksc or sav_TASctr <> wTASctr;
032300160118           chain (wTASksc:wTASctr) TICFT01L;
032400160115           IF  %found(TICFT01L);
032500160115             New_kscctr = *on;
032600160118             IF  CFTdpsf > pardrf;
032700160115               new_datarif = pardrf;
032800160115             ELSE;
032900160118               new_datarif = CFTdpsf;
033000160115             ENDIF;
033100160115           ELSE;
033200160115             new_datarif = pardrf;
033300160115             New_kscctr = *off;
033400160115           ENDIF;
033500160118           sav_TASksc = wTASksc;
033600160118           sav_TASctr = wTASctr;
033700160115         ENDIF;
033800160115      /end-free
033900160115
034000050526      *
034100050601     c                   movel     *Blanks       tastla
034200050603     c                   Clear                   kpjbu
034300140409     c                   clear                   dtasdta
034400140409      * verifico se sto calcolando la tassazione precedente di una bolla già fatturata
034500160118     c                   If        wtasfic <> ' '
034600140409     c                   clear                   dtasv
034700140409     c                   Clear                   taspor
034800140409      * Assicurazione
034900160118    2c                   IF        wTasfcm = 'F'
035000140409     c                   clear                   tasias
035100140409     c                   clear                   tasvas
035200140409    2c                   Endif
035300140409
035400140409      * recupero i valori  già tassati
035500140409     c                   exsr      Ric_tai
035600140409
035700140409     c                   else
035800140409      * se non è stata ancora fatturata devo recuperare i valori già impostati in bolla
035900140409     c                   eval      dtasv = w_dtasv
036000140409     c                   eval      taspor= W_taspor
036100140409
036200140409     c                   endif
036300140409
036400140409     c                   Clear                   tasimv
036500050603     c                   eval      tassva = *blanks
036600130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
036700130124      * tariffa con data tariffe
036800050614     c                   z-add     pardrf        tasdsp
036900160118     c************       z-add     pardrf        tasdct
037000160115      * passo la nuova data
037100160115     c                   z-add     new_datarif   TASdct
037200130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
037300130124      * calcolo istat e calcolo fuel
037400130124     c                   eval      tasdtist = pardrf
037500160118     c                   movel(p)  wtasaas       tasdtfue
037600160118     c                   move      wtasmgs       tasdtfue
037700140204      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
037800140204      * calcolo fuel
037900160118     c                   If        wtasdft = 0
038000160118     c                   movel(p)  wtasaas       tasdtfat
038100160118     c                   move      wtasmgs       tasdtfat
038200140409     c                   else
038300160118     c                   z-add     wtasdft       tasdtfat
038400140409     c                   endif
038500160115
038600160115      * imposto la data di riferimento x la tariffa di cartello, impostata nel lancio
038700160115     c                   z-add     pardrf        TASdtcar
038800060914
038900151023      * se ho record FAT nel file Fiar5 imposto il peso e volume usato in tassazione
039000151023      * nel campo taspkf in modo da tassare con lo stesso peso
039100151023     c                   clear                   dAr5Fat
039200151023     c                   eval      kAr5Trd  = 'FAT'
039300151023     c     kFiar5        Chain     Fiar531c
039400151023If  3c                   If        %Found(Fiar531c)
039500151023     c                   Movel     Ar5Uni        dAr5Fat
039600151023    3c                   EndIf
039700151023     c* non presente il record "FAT" --> tasso con peso e vollume presi da titas
039800151023     c                   if        §AR5PKTAS>0
039900151023     c                   clear                   TASPKC
040000151023     c                   clear                   tasncp
040100151023     c*     presente il record "FAT" --> tasso con peso e volume presi da "FAT"
040200151023     c                   eval      TASPKF=§ar5pktas
040300151023     c                   eval      TASVLF=§AR5VLTAS
040400151023     c                   endif
040500151023
040600130124     c                   call      'TNSF21R'
040700060914     c                   PARM                    KPJBA
040800060914     c                   PARM                    DTAS
040900060914     c                   PARM                    Dtasv
041000130124     c                   PARM                    Dtasdta
041100060914
041200130124
041300050607     c                   select
041400160118     c                   when      taserr = 'M'
041500160118     c                   exsr      CercaData
041600160118     c                   leavesr
041700060914     c                   when      taserr = *blanks
041800120113      *----------------------------------------------------------------*
041900090216      * se si tratta di confronto fatturazione
042000090216      * cerco la varia "f" (supplemento carburante)
042100090216      * se c'è la tolgo dall'imponibile e dalle varie
042200090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
042300090216      * e la varia "R" AC PLUS assicurazione
042400120113      *----------------------------------------------------------------*
042500100111      * DAL 2010 TOLGO LA VARIA h PER IL SERVIZIO 10:30 E
042600100111      * SUPPLEMENTO CARBURANTE FEDEX MENTRE RIPRISTINO
042700100111      * IL LASCIATO AVVISO
042800120113      *----------------------------------------------------------------*
042900120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
043000120113      * "R" ac plus
043100120113      * ZTL
043200120113      * Ripristino varia "h" servizio 10:30
043300120113      * come indicato da mail di DE LUCA del 11/01/2012
043400151207      *----------------------------------------------------------------*
043500151207      * dal 2016 escludo le varie:
043600151207      * "t" diritto di chiamata prenotazione ritiro
043700151207      * "k" pascking list
043800151207      * "R" ac plus
043900120113      *----------------------------------------------------------------*
044000050607     c                   exsr      SR_riewf
044100130218
044200080714
044300070207     c                   add       1             tottas
044400130111     c                   write     wfcft000
044500050607     c                   endsl
044600140411
044700140411     c                   clear                   taserr
044800130124
044900050607     c     e_sf20        endsr
045000050607      ****************************************************
045100050607      ** Carica importi varie da tita7000
045200050607      ****************************************************
045300050607     c     Carta7        Begsr
045400160118     c                   movel     Wtassv1       sv(1)
045500160118     c                   z-add     Wtasva1       va(1)
045600160118     c                   movel     Wtassv2       sv(2)
045700160118     c                   z-add     Wtasva2       va(2)
045800160118     c                   movel     Wtassv3       sv(3)
045900160118     c                   z-add     Wtasva3       va(3)
046000160118
046100050607      * pulisco le schiere delle varie
046200050607     c                   Do        20            k
046300050607     c                   If        k > 3
046400050607     c                   clear                   VA(K)
046500050607     c                   clear                   SV(K)
046600050607     c                   Endif
046700050607     c                   Enddo
046800060914      * pulisco il flag che mi indica se ci sono delle varie su tita7
046900050607     c                   clear                   flgta7
047000050607      * leggo tita7 solo se ho la 3° avria di titas maggiore di zero
047100160118     c                   If        wtassv3 <> *blanks
047200050607      * mi posiziono su tita7
047300050607     c     Kta7          setll     tita730c
047400050607     c                   if        %equal(tita730c)
047500050607     c                   z-add     3             k
047600050607      **
047700050607     c                   do        *hival
047800050607     c     Kta7          reade     tita730c
047900050607      * fine file
048000050607     c                   if        %eof(tita730c)
048100050607     c                   leave
048200050607     c                   endif
048300050607      * carico le varie
048400050607     c                   add       1             k
048500050607     c                   movel     ta7svn        sv(k)
048600050607     c                   z-add     ta7van        va(k)
048700050607     c                   z-add     1             flgta7
048800050607     c                   enddo
048900050607
049000050607     c                   endif
049100050607
049200050607     c                   endif
049300050607
049400050607     c                   Endsr
049500060914      *
049600060914      *****************************************************************
049700060914      *  Recupero importi di tassazione dal file titai
049800060914      *****************************************************************
049900060914     c     Ric_tai       Begsr
050000060914      *
050100060914     c                   z-add     0             k
050200060914
050300060914     c     ktai05        setll     titai01l
050400060914
050500060914     c                   If        %equal(titai01l)
050600060914
050700060914     c                   do        *hival
050800060914
050900060914     c     ktai05        reade     titai01l
051000060914
051100060914     c                   If        %eof(titai01l)
051200060914     c                   leave
051300060914     c                   endif
051400060914      * imponibile
051500060914     c                   if        taisvt = '£'
051600060914     c                   eval      tasimv = taivat
051700060914     c                   endif
051800060914      * porto
051900060914     c                   if        taisvt = ' '
052000060914     c                   eval      taspor = taivat
052100060914     c                   endif
052200060914      * varie
052300060914     c                   if        taisvt <> ' ' and taisvt <> '£'
052400060914     c                   add       1             k
052500060914     c                   eval      sv(k) = taisvt
052600060914     c                   eval      va(k) = taivat
052700060914     c                   endif
052800060914
052900060914     c                   enddo
053000060914
053100060914     c                   else
053200060914      * se non è pretassata pulisco la divisa
053300060914     c                   clear                   tasdiv
053400060914     c                   endif
053500060914
053600060914     c                   endsr
053700050607      ****************************************************
053800050609      ** Ricerca Contrassegno
053900050607      ****************************************************
054000050607     c     Carcsb        Begsr
054100050607
054200050607     c     Kcsb          chain     Tncsb03l
054300050607     c                   If        %found(tncsb03l) and
054400050607      * solo per stesso tipo bolla
054500050607     c                             tastbl = csbtbl
054600050607     c                   movel     csbsta        Tassta
054700050607     c                   z-add     csbcas        Tascas
054800050607     c                   if        csbfus <> *blanks
054900050607     c                   move      csbfus        tastic
055000050607     c                   else
055100050607     c                   move      csbtpi        tastic
055200050607     c                   end
055300050607      * mittente
055400050607     c                   movel     csbvca        tasvca
055500050607     c                   z-add     csbcmb        tascmb
055600050607
055700050607     c                   endif
055800050607
055900050607     c                   endsr
056000050607      *************************************************************************
056100060914      * scrittura w-file dati tassazione corrente
056200050607      *************************************************************************
056300060914     C     sr_corrente   BEGSR
056400140408     c                   clear                   wfcft000
056500160118     c                   z-add     wtasaas       CFTAAS
056600160118     c                   z-add     wtaslnp       CFTLNP
056700160118     c                   z-add     wtasnrs       CFTNRS
056800160118     c                   z-add     wtasnsp       CFTNSP
056900160118     c                   z-add     wtasksc       CFTKSC
057000160118     c                   z-add     wtasctr       CFTCTR
057100140407      * se si tratta di spedizione già fatturata verifico se devo ricalcolare con una
057200140407      * nuova tariffa
057300140410    Ac                   if        pardtc = 0 and tasimv > 0
057400160118     c                   z-add     wtasimv       CFTIMVC
057500160118     c                   z-add     wtaspor       CFTPORC
057600140408      * carico eventuali varie presenti in TITA7
057700140408     c                   Exsr      Carta7
057800140408
057900140408      * cerco la varia "D" (diritto di fatturazione)
058000140408      * se c'è la tolgo dall'imponibile e dalle varie
058100140408     c                   z-add     1             k
058200140408     c     'D'           lookup    sv(k)                                  30
058300140408     c                   if        %found
058400140408     c                   eval      cftimvc = cftimvc - va(k)
058500140408     c                   clear                   va(k)
058600140408     c                   clear                   sv(k)
058700140408     c                   endif
058800140411      * mi salvo le varie per ricalcolo istat e fuel
058900140411     c                   movea     sv            sav_sv
059000140411     c                   movea     va            sav_va
059100140411      * MI SDALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
059200140409     c                   z-add     1             k
059300140409     c     'f'           lookup    sv(k)                                  30
059400140409     c                   if        %found
059500140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
059600140409     c                   eval      cftfuelc = va(k)
059700140409     c                   endif
059800140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
059900140409     c                   z-add     1             k
060000140409     c     'L'           lookup    sv(k)                                  30
060100140409     c                   if        %found
060200140409     c                   eval      cftistatc = va(k)
060300140409     c                   endif
060400140408      * "R" AC PLUS da togliere dall'imponibile
060500140408     c                   z-add     1             k
060600140408     c     'R'           lookup    sv(k)                                  30
060700140408     c                   if        %found
060800140408     c                   eval      cftimvc = cftimvc - va(k)
060900140408     c                   clear                   va(k)
061000140408     c                   clear                   sv(k)
061100140408     c                   endif
061200151207
061300151207      * "t" diritto di chiamata prenotazione ritiro
061400151207     c                   z-add     1             k
061500151207     c     't'           lookup    sv(k)                                  30
061600151207     c                   if        %found
061700151207     c                   eval      cftimvc = cftimvc - va(k)
061800151207     c                   clear                   va(k)
061900151207     c                   clear                   sv(k)
062000151207     c                   endif
062100151207
062200151207     c                   z-add     1             k
062300151207      * "k" PACKING LIST
062400151207     c                   z-add     1             k
062500151207     c     'k'           lookup    sv(k)                                  30
062600151207     c                   if        %found
062700151207     c                   eval      cftimvc = cftimvc - va(k)
062800151207     c                   clear                   va(k)
062900151207     c                   clear                   sv(k)
063000151207     c                   endif
063100151207
063200140408      * valorizzo le varie della tassazione corrente
063300140408     c                   movea     va            vacor
063400140408     c                   movea     sv            svcor
063500140408     c
063600140410    Ac                   Endif
063700140408
063800140408      * se imponibile non valorizzato lo calcolo o con la data passata a video o con la data
063900140408      * spedizione
064000140408
064100160118    Bc                   If        Pardtc > 0 or wtasimv = 0
064200140408      * se imponibile valorizzato devo ricalcolare tutto con la nuova data tariffa corrente
064300140408      * quindi pulisco varie imponibile e porto
064400140408
064500160118     c                   If        wtasimv > 0
064600140408
064700140408      * Pulisco i campi dell'assicurazione se calcolata in fattura
064800140408    2c                   IF        Tasfcm = 'F'
064900140408     c                   clear                   tasias
065000140408     c                   clear                   tasvas
065100140408    2c                   Endif
065200140408      *
065300140408      * valorizzo la DS DTAS e DTASV
065400140408     c                   clear                   dtasv
065500140407     c                   clear                   tasimv
065600140408     c                   clear                   taspor
065700140407      *
065800140408      * cerco in TITAI l'immagine della spedizione prima della tassazione  compreso l'imponibile
065900140407     c                   exsr      RIC_tai
066000140408     c                   endif
066100140407      ** Ricerca contrassegno
066200160118    2c                   IF        wTasfca <> *blanks
066300140407     c                   Exsr      Carcsb
066400140407    2c                   Endif
066500140407
066600160118     c                   movel     wTasftc       tastc1
066700140407
066800160118     c                   movel(p)  wtasaas       tasdsp
066900160118     c                   move      wtasmgs       tasdsp
067000140408     c                   Z-add     tasdsp        tasdct
067100140407
067200140407      * Flag SI NO DDT
067300160118    2C                   If        wtasll1 = 'C' or wtasll1 = 'S'
067400140407     C                   movel     'S'           Tasddt
067500140407   x2C                   Else
067600140407     C                   Clear                   Tasddt
067700140407    2C                   Endif
067800140407     c                   clear                   dtas01
067900160118     c                   movel     wtasfbr       §asfbr
068000160118     c                   movel     wtascca       §ascca
068100140407      * valorizzo flag email al destinatario
068200160118     c                   movel     wtasflo       dtasflo
068300140407     c                   move      §floemd       §asemd
068400151207      * pin code
068500160118     c                   if        wtasgma<>*blanks and %lookup(wtasgma:skgma)>0
068600151207     c                   eval      §aspin = 'S'
068700151207     c                   endif
068800151207
068900140407
069000140408      * se data tariffa corrente diversa da zero passo nel flo la data tariffa corrente
069100140408     c                   If        pardtc > 0
069200140408     c                   move      pardtc        §asdrt
069300140408     c                   Z-add     pardtc        tasdct
069400140407     c                   endif
069500140407
069600140408     c                   eval      §tasflo = dtas01
069700140407
069800151207      * valorizzo il campo per l'addebito diritto di chiamata prenotazione ritiro
069900151207     c                   move      §floado       tasprt
070000151207      * valorizzo il campo per l'addebito stampa PACKING LIST
070100151207     c                   eval      kAr5Trd  = 'GEN'
070200151207     c                   clear                   DAr5gen
070300151207     c     kFiar5        Chain(n)  Fiar531c
070400151207If  1c                   If        %Found(Fiar531c)
070500151207     c                   Movel     Ar5Uni        dAr5Gen
070600151207    1c                   EndIf
070700151207      *
070800151207     c                   IF        §AR5als = 'S' or §AR5alx = 'S'
070900151207     c                   Eval      TasSpl = 'S'
071000151207     c                   endif
071100151207
071200140407      * Bancali
071300140407    2c                   If        %Subst(TasGva:1:1) = 'B'
071400140407
071500140407     c                   eval      kAr5Trd  = 'BAN'
071600140407     c     kFiar5        Chain     Fiar531c
071700140407If  3c                   If        %Found(Fiar531c)
071800140407     c                   Movel     Ar5Uni        dAr5Ban
071900140407    3c                   EndIf
072000140407      * Bancali
072100160118     c                   Eval      wTasBan = §Ar5Nba + §Ar5Nb2
072200140407    2c                   EndIf
072300140407      **
072400140407      * Colli Originali
072500140407    2c                   If        %Subst(TasGva:1:1) = 'O'
072600140407
072700140407     c                   eval      kAr5Trd  = 'BNB'
072800140407     c     kFiar5        Chain     Fiar531c
072900140407If  3c                   If        %Found(Fiar531c)
073000140407     c                   Movel     Ar5Uni        dAr5Bnb
073100140407    3c                   EndIf
073200140407      * Colli
073300140407     c                   Eval      TasNcl = §Ar5bcor
073400140407    2c                   EndIf
073500140407      *
073600140407      * verifico se c'è evento
073700140407     c     Kevb4         chain     Fnevb04l
073800140407
073900140408    2c                   If        %found(Fnevb04l)
074000140407     c                   eval      tasric = 'S'
074100140408    2c                   endif
074200140407
074300140407      **
074400140409     c                   clear                   dtasdta
074500140409      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
074600140409      * calcolo fuel
074700160118     c                   If        wtasdft = 0
074800160118     c                   movel(p)  wtasaas       tasdtfat
074900160118     c                   move      wtasmgs       tasdtfat
075000140409     c                   else
075100160118     c                   z-add     wtasdft       tasdtfat
075200140409     c                   endif
075300140409
075400140407      **
075500140409     c                   call      'TNSF21R'
075600140407     c                   parm                    Kpjba
075700140407     c                   parm                    Dtas
075800140407     c                   parm                    Dtasv
075900140409     c                   parm                    dtasdta
076000140409
076100140409     c                   if        taserr <> ' '
076200140409     c                   leavesr
076300140409     c                   endif
076400140409
076500140407
076600140407      * tassazione
076700140407
076800060914     c                   z-add     tasimv        CFTIMVC
076900060914     c                   z-add     taspor        CFTPORC
077000140411      * mi salvo le varie per ricalcolo istat e fuel
077100140411     c                   movea     sv            sav_sv
077200140411     c                   movea     va            sav_va
077300140409      * MI SALVO I VALORI DEL FUEL E ISTAT TASSAZIONE CORRENTE
077400140409     c                   z-add     1             k
077500140409     c     'f'           lookup    sv(k)                                  30
077600140409     c                   if        %found
077700140409      * mi memorizzo il valore del fuel della TASSAZIONE CORRENTE
077800140409     c                   eval      cftfuelc = va(k)
077900140409     c                   endif
078000140409      * mi memorizzo ISTAT della TASSAZIONE CORRENTE
078100140409     c                   z-add     1             k
078200140409     c     'L'           lookup    sv(k)                                  30
078300140409     c                   if        %found
078400140409     c                   eval      cftistatc = va(k)
078500140409     c                   endif
078600060914
078700130124      * "R" AC PLUS da togliere dall'imponibile
078800090216     c                   z-add     1             k
078900090216     c     'R'           lookup    sv(k)                                  30
079000090216     c                   if        %found
079100090216     c                   eval      cftimvc = cftimvc - va(k)
079200130124     c                   clear                   va(k)
079300130124     c                   clear                   sv(k)
079400130124     c                   endif
079500151207
079600151207      * "t" diritto di chiamata prenotazione ritiro
079700151207     c                   z-add     1             k
079800151207     c     't'           lookup    sv(k)                                  30
079900151207     c                   if        %found
080000151207     c                   eval      cftimvc = cftimvc - va(k)
080100151207     c                   clear                   va(k)
080200151207     c                   clear                   sv(k)
080300151207     c                   endif
080400151207
080500151207     c                   z-add     1             k
080600151207      * "k" PACKING LIST
080700151207     c                   z-add     1             k
080800151207     c     'k'           lookup    sv(k)                                  30
080900151207     c                   if        %found
081000151207     c                   eval      cftimvc = cftimvc - va(k)
081100151207     c                   clear                   va(k)
081200151207     c                   clear                   sv(k)
081300151207     c                   endif
081400130124      * valorizzo le varie della tassazione corrente
081500130124     c                   movea     va            vacor
081600160118     c                   movea     sv            svcor
081700140410    1c                   endif
081800140411      * reimposto le varie come da fatturazione
081900140411     c                   movea     sav_va        va
082000140411     c                   movea     sav_sv        sv
082100140411      * ricalcolo ISTAT e FUEL
082200140408
082300140408     c                   exsr      car_sf21ds
082400140408
082500050607     c                   endsr
082600130124      **---------------------------------------------------------------**
082700130124      ** Carico la ds per le ritassazioni e le eseguo per ISTAT e FUEL **
082800130124      **---------------------------------------------------------------**
082900130124     c     car_sf21ds    BEGSR
083000130124      *
083100130124     c                   movel     *Blanks       tastla
083200130124     c                   clear                   dAr5Ban
083300130124     c                   Clear                   dAr5Bnb
083400130124     c                   Clear                   kpjbu
083500130124     c                   clear                   dtasdta
083600130124
083700160118     c                   movel     wTasftc       tastc1
083800130124      * Flag SI NO DDT
083900160118    1C                   If        wtasll1 = 'C' or wtasll1 = 'S'
084000130124     C                   movel     'S'           Tasddt
084100130124   x1C                   Else
084200130124     C                   Clear                   Tasddt
084300130124    1C                   Endif
084400130124     c                   clear                   dtas01
084500160118     c                   movel     wtasfbr       §asfbr
084600160118     c                   movel     wtascca       §ascca
084700130124      * valorizzo flag email al destinatario
084800160118     c                   movel     wtasflo       dtasflo
084900130124     c                   move      §floemd       §asemd
085000151207      * pin code
085100160118     c                   if        wtasgma<>*blanks and %lookup(wtasgma:skgma)>0
085200151207     c                   eval      §aspin = 'S'
085300151207     c                   endif
085400130124
085500130124     c                   eval      §tasflo = dtas01
085600130124
085700130124     c
085800130124     c                   clear                   tasban
085900130124      * Bancali
086000130124    1c                   If        %Subst(TasGva:1:1) = 'B'
086100130124
086200130124     c                   eval      kAr5Trd  = 'BAN'
086300130124     c     kFiar5        Chain     Fiar531c
086400130124If  1c                   If        %Found(Fiar531c)
086500130124     c                   Movel     Ar5Uni        dAr5Ban
086600130124    1c                   EndIf
086700130124      * Bancali
086800130124     c                   Eval      TasBan = §Ar5Nba + §Ar5Nb2
086900130124    1c                   EndIf
087000130124      **
087100130124      * Colli Originali
087200130124    1c                   If        %Subst(TasGva:1:1) = 'O'
087300130124     c                   eval      kAr5Trd  = 'BNB'
087400130124     c     kFiar5        Chain     Fiar531c
087500130124If  1c                   If        %Found(Fiar531c)
087600130124     c                   Movel     Ar5Uni        dAr5Bnb
087700130124    1c                   EndIf
087800130124      * Colli
087900130124     c                   Eval      TasNcl = §Ar5bcor
088000130124    1c                   EndIf
088100130124
088200130124     c                   clear                   tascas
088300130124     c                   clear                   tastic
088400130124     c                   clear                   tasvca
088500130124     c                   clear                   tascmb
088600130124      * C/Assegno
088700160118    2c                   IF        wTasfca <> *blanks
088800130124     c                   Exsr      Carcsb
088900130124    2c                   Endif
089000140409
089100130124     c     Kevb4         chain     Fnevb04l
089200130124
089300130124     c                   If        %found(Fnevb04l)
089400130124     c                   eval      tasric = 'S'
089500130124     c                   endif
089600130124
089700130124      * Assicurazione
089800160118    2c                   IF        wTasfcm = 'F'
089900130124     c                   clear                   tasias
090000130124     c                   clear                   tasvas
090100130124    2c                   Endif
090200130124
090300130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
090400130124      * tariffa con data tariffe
090500130124     c                   z-add     pardrf        tasdsp
090600130124     c                   z-add     pardrf        tasdct
090700130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
090800130124      * calcolo istat
090900160118     c                   movel(p)  wtasaas       tasdtist
091000160118     c                   move      wtasmgs       tasdtist
091100140207      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
091200140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
091300160118     C                   IF        wTASDFT > 0
091400160118     c                   z-add     wtasdft       tasdtfat
091500140409     C                   ELSE
091600160118     c                   movel(p)  wtasaas       tasdtfat
091700160118     c                   move      wtasmgs       tasdtfat
091800140409     C                   ENDIF
091900130129      * verifico se fuel pretassato dalla filiale
092000130129     c                   eval      ksvt = 'L'
092100130129     c     Ktai06        chain     titai01l
092200130129      * se non trovato record ricalcolo con stessa base imponibile
092300130129     c                   If        not %found(titai01l)
092400130129
092500130124
092600130124      * pulisco ISTAT della fatturazione
092700130124     c                   z-add     1             k
092800130124     c     'L'           lookup    sv(k)                                  30
092900130124     c                   if        %found
093000130124     c                   clear                   va(k)
093100130124     c                   clear                   sv(k)
093200130124     c                   endif
093300130124      * pulisco imponibile
093400130124     c                   clear                   tasimv
093500130124      * imposto la  varia da calcolare
093600130124     c                   eval      tassva = 'L'
093700140409     c                   call      'TNSF21R'
093800130124     c                   PARM                    KPJBA
093900130124     c                   PARM                    DTAS
094000130124     c                   PARM                    Dtasv
094100130124     c                   PARM                    Dtasdta
094200130129     c                   endif
094300130124
094400130124      * valorizzo la varia ISTAT ricalcolata
094500130124     c                   z-add     1             k
094600130124     c     'L'           lookup    sv(k)                                  30
094700130124     c                   if        %found
094800130125     c                   eval      cftistatr = va(k)
094900130124     c                   endif
095000130125      * valorizzo imponibile varia ISTAT ricalcolata
095100130125     c                   eval      cftimpist = tasimv - cftistatr
095200130124      * se si tratta di confronto di fatturazione imposto le date di riferimento x recupero
095300130124      * calcolo fuel
095400130124     c                   clear                   tasdtist
095500160118     c                   movel(p)  wtasaas       tasdtfue
095600160118     c                   move      wtasmgs       tasdtfue
095700140409      * se si tratta di confronto di fatturazione imposto la data fattura  per calcolo imponibile
095800140409      * calcolo fuel se maggiore di zero altrimenti data spedizione
095900160118     C                   IF        wTASDFT > 0
096000160118     c                   z-add     wtasdft       tasdtfat
096100140409     C                   ELSE
096200160118     c                   movel(p)  wtasaas       tasdtfat
096300160118     c                   move      wtasmgs       tasdtfat
096400140409     C                   ENDIF
096500130129      * verifico se fuel pretassato dalla filiale
096600130129     c                   eval      ksvt = 'f'
096700130129     c     Ktai06        chain     titai01l
096800130129      * se non trovato record ricalcolo con stessa base imponibile
096900130129     c                   If        not %found(titai01l)
097000130124
097100130124      * pulisco FUEL  della fatturazione
097200130124     c                   z-add     1             k
097300130124     c     'f'           lookup    sv(k)                                  30
097400130124     c                   if        %found
097500130124     c                   clear                   va(k)
097600130124     c                   clear                   sv(k)
097700130124     c                   endif
097800130124      * pulisco imponibile
097900130124     c                   clear                   tasimv
098000130124      * imposto la  varia da calcolare
098100130124     c                   eval      tassva = 'f'
098200140409     c                   call      'TNSF21R'
098300130124     c                   PARM                    KPJBA
098400130124     c                   PARM                    DTAS
098500130124     c                   PARM                    Dtasv
098600130124     c                   PARM                    Dtasdta
098700130129     c                   endif
098800130124
098900130124     c                   z-add     1             k
099000130124     c     'f'           lookup    sv(k)                                  30
099100130124     c                   if        %found
099200130125     c                   eval      cftfuelr = va(k)
099300130124     c                   endif
099400130125      * valorizzo imponibile varia FUEL  ricalcolata
099500130125     c                   eval      cftimpfue= taspor
099600130125      * inoltro
099700130125     c                   z-add     1             k
099800130125     c     '2'           lookup    sv(k)                                  30
099900130125     c                   if        %found
100000130125     c                   eval      cftimpfue= cftimpfue + va(k)
100100130125     c                   endif
100200130125      * isola
100300130125     c                   z-add     1             k
100400130125     c     'J'           lookup    sv(k)                                  30
100500130125     c                   if        %found
100600130125     c                   eval      cftimpfue= cftimpfue + va(k)
100700130125     c                   endif
100800130125      * località disagiata
100900130125     c                   z-add     1             k
101000130125     c     'K'           lookup    sv(k)                                  30
101100130125     c                   if        %found
101200130125     c                   eval      cftimpfue= cftimpfue + va(k)
101300130125     c                   endif
101400130125      * centro storico
101500130125     c                   z-add     1             k
101600130125     c     'Q'           lookup    sv(k)                                  30
101700130125     c                   if        %found
101800130125     c                   eval      cftimpfue= cftimpfue + va(k)
101900130125     c                   endif
102000130125      * espresso/priority
102100130125     c                   z-add     1             k
102200130125     c     'e'           lookup    sv(k)                                  30
102300130125     c                   if        %found
102400130125     c                   eval      cftimpfue= cftimpfue + va(k)
102500130125     c                   endif
102600140207      * Se la data fattura è < o uguale la 30/01/2014 non sommo queste varie aggiuntive
102700140409     c                   if        tasdft > 20140130 or
102800140409     c                             tasdft = 0
102900130125      * h 10,30
103000130125     c                   z-add     1             k
103100130125     c     'h'           lookup    sv(k)                                  30
103200130125     c                   if        %found
103300130125     c                   eval      cftimpfue= cftimpfue + va(k)
103400130125     c                   endif
103500140207      * = reso bancali
103600140207     c                   z-add     1             k
103700140207     c     '='           lookup    sv(k)                                  30
103800140207     c                   if        %found
103900140207     c                   eval      cftimpfue= cftimpfue + va(k)
104000140207     c                   endif
104100140207      * c lasciato avviso
104200140207     c                   z-add     1             k
104300140207     c     'c'           lookup    sv(k)                                  30
104400140207     c                   if        %found
104500140207     c                   eval      cftimpfue= cftimpfue + va(k)
104600140207     c                   endif
104700140207      * A appuntamento
104800140207     c                   z-add     1             k
104900140207     c     'A'           lookup    sv(k)                                  30
105000140207     c                   if        %found
105100140207     c                   eval      cftimpfue= cftimpfue + va(k)
105200140207     c                   endif
105300140207      * B consegna DDT
105400140207     c                   z-add     1             k
105500140207     c     'B'           lookup    sv(k)                                  30
105600140207     c                   if        %found
105700140207     c                   eval      cftimpfue= cftimpfue + va(k)
105800140207     c                   endif
105900140207      * C facch. arr.
106000140207     c                   z-add     1             k
106100140207     c     'C'           lookup    sv(k)                                  30
106200140207     c                   if        %found
106300140207     c                   eval      cftimpfue= cftimpfue + va(k)
106400140207     c                   endif
106500140207      * F fuori misura
106600140207     c                   z-add     1             k
106700140207     c     'F'           lookup    sv(k)                                  30
106800140207     c                   if        %found
106900140207     c                   eval      cftimpfue= cftimpfue + va(k)
107000140207     c                   endif
107100140207      * H diritto fisso
107200140207     c                   z-add     1             k
107300140207     c     'H'           lookup    sv(k)                                  30
107400140207     c                   if        %found
107500140207     c                   eval      cftimpfue= cftimpfue + va(k)
107600140207     c                   endif
107700140207      * I spese di giacenza
107800140207     c                   z-add     1             k
107900140207     c     'I'           lookup    sv(k)                                  30
108000140207     c                   if        %found
108100140207     c                   eval      cftimpfue= cftimpfue + va(k)
108200140207     c                   endif
108300140207      * N anteporto
108400140207     c                   z-add     1             k
108500140207     c     'N'           lookup    sv(k)                                  30
108600140207     c                   if        %found
108700140207     c                   eval      cftimpfue= cftimpfue + va(k)
108800140207     c                   endif
108900140207      * P ai piani
109000140207     c                   z-add     1             k
109100140207     c     'P'           lookup    sv(k)                                  30
109200140207     c                   if        %found
109300140207     c                   eval      cftimpfue= cftimpfue + va(k)
109400140207     c                   endif
109500140207      * S supermercato
109600140207     c                   z-add     1             k
109700140207     c     'S'           lookup    sv(k)                                  30
109800140207     c                   if        %found
109900140207     c                   eval      cftimpfue= cftimpfue + va(k)
110000140207     c                   endif
110100140207      * U ritiro
110200140207     c                   z-add     1             k
110300140207     c     'U'           lookup    sv(k)                                  30
110400140207     c                   if        %found
110500140207     c                   eval      cftimpfue= cftimpfue + va(k)
110600140207     c                   endif
110700140207      * X consegne disagiate
110800140207     c                   z-add     1             k
110900140207     c     'X'           lookup    sv(k)                                  30
111000140207     c                   if        %found
111100140207     c                   eval      cftimpfue= cftimpfue + va(k)
111200140207     c                   endif
111300151207      * p Pin code
111400151207     c                   z-add     1             k
111500151207     c     'P'           lookup    sv(k)                                  30
111600151207     c                   if        %found
111700151207     c                   eval      cftimpfue= cftimpfue + va(k)
111800151207     c                   endif
111900151207      * Z Expo
112000151207     c                   z-add     1             k
112100151207     c     'z'           lookup    sv(k)                                  30
112200151207     c                   if        %found
112300151207     c                   eval      cftimpfue= cftimpfue + va(k)
112400151207     c                   endif
112500151207      * t diritto di chiamata prenotazione telefonica
112600151207     c                   z-add     1             k
112700151207     c     't'           lookup    sv(k)                                  30
112800151207     c                   if        %found
112900151207     c                   eval      cftimpfue= cftimpfue + va(k)
113000151207     c                   endif
113100151207      * k PACKING LIST
113200151207     c                   z-add     1             k
113300151207     c     'k'           lookup    sv(k)                                  30
113400151207     c                   if        %found
113500151207     c                   eval      cftimpfue= cftimpfue + va(k)
113600151207     c                   endif
113700140207
113800140207     c                   endif
113900130124
114000130124     c                   endsr
114100050607      *************************************************************************
114200060914      * scrittura w-file dati tassazione precdeente
114300050607      *************************************************************************
114400060914     C     sr_riewf      BEGSR
114500061218
114600061218      * cerco la varia "f" (supplemento carburante)
114700061218      * se c'è la tolgo dall'imponibile e dalle varie
114800090216      * dal 2009 togliamo anche la varia "c" addebito lascaito avviso
114900090216      * e la varia "R" AC PLUS assicurazione
115000100108      * dal 2010 togliamo anche la varia "+" supplemento carburante fedex
115100100111      * e il servizio 10,30 "h" mentre ripristiniamo il lasciato
115200100111      * avviso
115300120113      *----------------------------------------------------------------*
115400120113      * dal 2012 escludo le varie Fuel e suppl. carburante fedex
115500120113      * "R" ac plus
115600120113      * ZTL
115700120113      * Ripristino varia "h" servizio 10:30
115800120113      * come indicato da mail di DE LUCA del 11/01/2012
115900120113      *----------------------------------------------------------------*
116000130124      *----------------------------------------------------------------*
116100130124      * dal 2013 non escludo le varie Fuel e suppl. carburante fedex
116200130124      * perchè vengo calcolate in modo tale da mantenere la ricerca
116300130124      * del prezzo base in base alla data spedizione reale
116400130124      * escludo sempre la varia "R" ac plus
116500130124      * come da nuovo progetto 810 Daniele
116600151207      *----------------------------------------------------------------*
116700151207      * dal 2016 escludo le varie:
116800151207      * "t" diritto di chiamata prenotazione ritiro
116900151207      * "k" pascking list
117000151207      * "R" ac plus
117100130124      *----------------------------------------------------------------*
117200151207      * "R" AC PLUS
117300151207     c                   z-add     1             k
117400151207     c     'R'           lookup    sv(k)                                  30
117500151207     c                   if        %found
117600151207     c                   eval      tasimv  = tasimv - va(k)
117700151207     c                   clear                   va(k)
117800151207     c                   clear                   sv(k)
117900151207     c                   endif
118000151207
118100151207      * "t" diritto di chiamata prenotazione ritiro
118200151207     c                   z-add     1             k
118300151207     c     't'           lookup    sv(k)                                  30
118400151207     c                   if        %found
118500151207     c                   eval      tasimv  = tasimv - va(k)
118600151207     c                   clear                   va(k)
118700151207     c                   clear                   sv(k)
118800151207     c                   endif
118900151207
119000061218     c                   z-add     1             k
119100151207      * "k" PACKING LIST
119200090216     c                   z-add     1             k
119300151207     c     'k'           lookup    sv(k)                                  30
119400090216     c                   if        %found
119500090216     c                   eval      tasimv  = tasimv - va(k)
119600090216     c                   clear                   va(k)
119700090216     c                   clear                   sv(k)
119800090216     c                   endif
119900061218
120000060914     c                   z-add     tasimv        CFTIMVP
120100060914     c                   z-add     taspor        CFTPORP
120200060914     c                   movea     va            vapre
120300060914     c                   movea     sv            svpre
120400050607     c                   endsr
120500160118      /free
120600160118       //--------------------------------------------------------------
120700160118       //?Cerco la data prima spedizione fatturata.
120800160118       //--------------------------------------------------------------
120900160118       BEGSR CercaData;
121000160118
121100160118         IF  not GiaTassato;
121200160118
121300160118           setll (TASksc:TASctr) TITAS41C;
121400160118           DOW  not %eof(TITAS41C);
121500160118           reade (TASksc:TASctr) TITAS41C;
121600160118           //?Verifico se spedizione tassata fino all'imponibile
121700160118             TAIsvt = '£';
121800160118             chain (TASaas:TASlnp:TASnrs:TASnsp:TAStbl:TAIsvt) TITAI01L;
121900160118           //?Scarto se tassata fino all'imponibile
122000160118             IF  %found(TITAI01L);
122100160118               iter;
122200160118             ENDIF;
122300160118           //?Solo fatture di sede
122400160118             IF  TASfiv = 998 or TASfiv = 996;
122500160118               exsr NewRcdTICFT;
122600160118               leave;
122700160118             ENDIF;
122800160118           ENDDO;
122900160118         ENDIF;
123000160118
123100160118       ENDSR;
123200160118
123300160118       //--------------------------------------------------------------
123400160118       //?Scrivo nuovo rcd in TICFT.
123500160118       //--------------------------------------------------------------
123600160118       BEGSR NewRcdTICFT;
123700160118
123800160118         chain (TASksc:TASctr) TICFT01L;
123900160118         IF  not %found(TICFT01L);
124000160118           clear TICFT000;
124100160118           CFTksc = TASksc;
124200160118           CFTctr = TASctr;
124300160118           CFTdpsf = TASaas * 10000 + TASmgs;
124400160118           write TICFT000;
124500160118           feod TICFT01L;
124600160118         ENDIF;
124700160118
124800160118       ENDSR;
124900160118      /end-free
125000130218
125100050609      *************************************************************************
125200050609      * Operazioni iniziali
125300050609      *************************************************************************
125400050609     C     *inzsr        BEGSR
125500160115
125600140408     c     ktas          klist
125700140408     c                   kfld                    paraas
125800140408     c                   kfld                    parmgs
125900060914
126000060914     c     ktai05        klist
126100160118     c                   kfld                    wtasaas
126200160118     c                   kfld                    wtaslnp
126300160118     c                   kfld                    wtasnrs
126400160118     c                   kfld                    wtasnsp
126500160118     c                   kfld                    wtastbl
126600060914
126700060914     c     ktai06        klist
126800160118     c                   kfld                    wtasaas
126900160118     c                   kfld                    wtaslnp
127000160118     c                   kfld                    wtasnrs
127100160118     c                   kfld                    wtasnsp
127200160118     c                   kfld                    wtastbl
127300060914     c                   kfld                    Ksvt
127400060914
127500050609     c     kta7          klist
127600160118     c                   kfld                    wtasaas
127700160118     c                   kfld                    wtaslnp
127800160118     c                   kfld                    wtasnrs
127900160118     c                   kfld                    wtasnsp
128000160118     c                   kfld                    wtastbl
128100050609
128200050609     c     kFiar5        Klist
128300160118     c                   Kfld                    wTasAas
128400160118     c                   Kfld                    wTasLnp
128500160118     c                   Kfld                    wTasNrs
128600160118     c                   Kfld                    wTasNsp
128700050609     c                   Kfld                    kAr5Trd
128800050609
128900050609     c     KCSB          KLIST
129000160118     c                   KFLD                    wTASAAS
129100160118     c                   KFLD                    wTASLNP
129200160118     c                   KFLD                    wTASNRS
129300160118     c                   KFLD                    wTASNSP
129400050609
129500050609     c     Kcft          KLIST
129600160118     c                   KFLD                    wTASAAS
129700160118     c                   KFLD                    wTASLNP
129800160118     c                   KFLD                    wTASNRS
129900160118     c                   KFLD                    wTASNSP
130000050609
130100050609     C     KTAB          KLIST
130200050609     C                   KFLD                    CODUT
130300050609     C                   KFLD                    KCOD
130400081105
130500081105      * Chiave file eventi FNEVB
130600081105     c     Kevb4         Klist
130700160118     c                   Kfld                    wTasaas
130800160118     c                   Kfld                    wTaslnp
130900160118     c                   Kfld                    wTasnrs
131000160118     c                   Kfld                    wTasnsp
131100081105     c                   Kfld                    Wcev
131200160115
131300050609     c
131400050609     c     *entry        plist
131500050609     c                   parm                    kpjba
131600050609     c                   movel     kpjbu         param
131700060915
131800090430     c                   time                    wtime            14 0
131900090430     c                   move      wtime         wdata
132000090430     c                   movel     wtime         wora
132100160115
132200060915      * caricamento schiera dei tipi bolla da scartare
132300060915     c                   eval      Kcod  = 'TB'
132400050609     c                   clear                   tbs
132500050609     c                   z-add     0             xx
132600050609     c     ktab          setll     tabel00f
132700050609     c                   do        *hival
132800050609     c     ktab          reade     tabel00f
132900050609     c                   if        %eof(tabel00f)
133000050609     c                   leave
133100050609     c                   endif
133200050609     c                   movel     tbluni        dstb
133300160115
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
