000100040119      *===============================================================*
000200171116      *  Creazione archivio di lavoro per tender group seb
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000160126     fTITAS41C  if   e           k disk    prefix(t_)
001100060914     fTita730c  if   e           k disk
001101171116     fTitaa30c  if   e           k disk
001102171120     fFiar531c  if   e           k disk
001500171116     fwfseb00f  o  a e             disk
002000040119      *
002500050526     d kpjba         e ds
002501171120
002502171120     d dAr5Fat       e ds
003400050607      ** DS Calcolo tassazione  - Varie
003500171116     d Dtasv         e ds                  qualified
003600050607     d  sv                     1     20
003700050607     d                                     Dim(20)                              Sigle varie
003800050607     d  va                    21    140P 3
003900050607     d                                     Dim(20)                              Importi varie
003901171116       // - Reperimento dati anagrafici
003902171116     d TIBS69ds      e ds
003903171116     d DS_cnaco      e ds                  inz extname(CNACO00F)
003904171116     d DS_cnind      e ds                  inz extname(CNIND00F)
003905171116     d DS_cnclp      e ds                  inz extname(CNCLP00F)
003906171116     d DS_fncls      e ds                  inz extname(FNCLS00F)
005200140409
005300171116     D wdata           DS                  INZ
005400171116     D  anno                   1      4
005500171116     D  mese                   5      6
005600171116     D  giorno                 7      8
005800140409
006600160118
006700160118     dTitasds        e ds                  extname(TITAS00F)
006800040331      *
006900040331      *
007000040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
007100050526      *
007200040331      * - Indici di schiera
007300040331     d xx              s              3  0 inz
007400050607     D k               s              5  0                                      indice
007401171116     D x               s              3  0 inz                                  indice
007500040331      * - Campi di comodo
008800160118
008900160118     d sav_TASksc      s                   like(TASksc) inz
008901171117     d sav_Ksc         s                   like(TASksc) inz
009301171116
009400060914      *
009500060914      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
009600060914      *
009601171117     D ksc             s              7s 0 dim(10) ctdata perrcd(1)              codici clienti
009602171116
010401171116      /copy gaitrasrc/srcprotopr,tibs69r
010800160118      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
010900160118      /free
011000160118         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
011100160118
011101171116       // Leggo il file delle bolle per ogni ksc della schiera
011102171116         dow  x <= 9  ;
011103171116              x = x+1 ;
011104171117              If  ksc(x) <> 0;
011105171117                  SAV_KSC = KSC(X) ;
011200160118       // Leggo il file delle bolle per Data Spedizione e Cliente/Tariffe
011300171116                  exec sql
011400171116                  declare C1 cursor for
011800171117                  select * from titas10f where tasksc=:sav_ksc and
011801171120                  tastbl in ('F1' , 'A2') and taspor > 0
011802171120                  and tasaas > 2015
012000171116                  union
012100171117                  select * from titasp0f where tasksc =:sav_ksc and
012101171120                  tastbl in ('F1' , 'A2') and taspor > 0
012102171120                  and tasaas > 2015
012300171116                  order by TASksc, TASctr
012400171116                  for read only;
012401171116
012402171116              else ;
012403171116                  iter ;
012404171116              Endif;
012500160118
012600171116              exec sql open C1;
012700160118
012800171116              DOW  sqlcod = *zeros;
013000171116                  clear dtasv;
013200171116                  exec sql fetch C1 into :TITASDS;
013300171117                  if  sqlcod <> *zeros ;
013301171117                      leave ;
013302171117                  endif;
013400160129       // A rottura di Cliente/Tariffa
013500171116                  IF  sav_TASksc <> TASksc ;
013600171116       // recupero la ragione sociale
013601171116                      clear tibs69ds;
013602171116                      clear ACOrag;
013603171116                      I69kac =tasksc;
013604171117                      I69kin =tasksc;
013605171116                      TIBS69R (tibs69ds:ds_cnaco:ds_cnind:ds_cnclp:ds_fncls);
013606171116                      if  O69err <> *blanks;
013607171116                          acorag      = '??????????????';
013610171116                      endif;
014700171116                      sav_TASksc = TASksc;
015000171116                  ENDIF;
015200160126
015201171116                  exsr scrivi ;
022000171116              Enddo;
028400160118         exec sql close C1;
028401171117         Enddo;
028402171116         *inlr = *on;
028502171116      //
028503171116      // Scrivo un record per ogni spedizione
028504171116      //
028505171116         Begsr  Scrivi;
028506171116           clear wfseb000 ;
028507171116           sebrag  = ACOrag;
028508171116           sebcar = 'BRT';
028509171116        //  recupero i dati del mittente
028510171116           chain (tasaas:taslnp:tasnrs:tasnsp:'M') Titaa30c ;
028511171116           If %found(titaa30c) ;
028512171116              sebrsm = taarsc ;
028513171116              sebcam = taacap ;
028514171116              seblom = taaloc ;
028515171116              sebnzm = taanaz ;
028516171117           Else ;
028517171117              sebrsm = acorag ;
028518171117              sebcam = indcae ;
028519171117              seblom = indcit ;
028520171117              sebnzm = indsta ;
028521171117           Endif;
028522171116              sebrsd = tasrsd ;
028523171116              sebcad = tascad ;
028524171116              seblod = taslod ;
028525171116              sebnzd = tasnzd ;
028526171116              sebunit= 'PARCELS';
028527171116              sebrmn = tasrmn;
028528171116              sebpkb = taspkf;
028529171116              sebvlf = tasvlf;
028530171120        // recupero da FIar531c
028531171120              clear dAr5Fat ;
028532171120              chain (tasaas:taslnp:tasnrs:tasnsp:'FAT') Fiar531c ;
028533171120              If %found(fiar531c) ;
028534171120                 dAr5Fat = Ar5uni ;
028535171120                 sebpkb = §ar5pktas;
028536171120                 sebvlf = §ar5vltas;
028537171120              Endif;
028538171120
028539171116              sebncl = tasncl;
028540171116              sebtsp = tastsp;
028541171116              wdata  = %editc(tasdrt:'X');
028542171116              sebdrt = giorno + '/' + mese + '/' + anno ;
028543171116              sebhrt = '17:00';
028544171116              wdata  = %editc(tasdce:'X');
028545171116              sebded = giorno + '/' + mese + '/' + anno ;
028546171116              sebhed = '18:30';
028547171116              wdata  = %editc(tasdcm:'X');
028548171116              sebdde = giorno + '/' + mese + '/' + anno ;
028549171116              sebhde = %editw(tashmc: '0 :  ');
028550171116              sebctre = tasctr;
028551171116              sebctrd = tasctr;
028552171116              sebdiv = tasdiv ;
028553171116              sebimv = tasimv;
028554171116              sebpor = taspor;
028555171116        //  carico le varie presenti
028556171116              Exsr Carta7;
028557171116              xx=%lookup('f': dtasv.sv);
028558171116              if xx > 0  ;
028559171116                 sebfuel= dtasv.va(xx) ;
028560171116              Endif;
028561171116              xx=%lookup('R': dtasv.sv);
028562171116              if xx > 0  ;
028563171116                 sebinsur= dtasv.va(xx) ;
028564171116              Endif;
028565171116              xx=%lookup('d': dtasv.sv);
028566171116              if xx > 0  ;
028567171116                 sebinsur= sebinsur+ dtasv.va(xx) ;
028568171116              Endif;
028569171116              sebcostia= %xfoot(dtasV.va) - sebinsur - sebfuel ;
028570171116         // carico le singole varie
028571171116              xx=%lookup('2': dtasv.sv);
028572171116              if xx > 0  ;
028573171116                 sebvari2 = dtasv.va(xx) ;
028574171116              Endif;
028575171116              xx=%lookup('5': dtasv.sv);
028576171116              if xx > 0  ;
028577171116                 sebvari5 = dtasv.va(xx) ;
028578171116              Endif;
028579171116              xx=%lookup('&': dtasv.sv);
028580171116              if xx > 0  ;
028581171116                 sebvariapa = dtasv.va(xx) ;
028582171116              Endif;
028583171116              xx=%lookup('*': dtasv.sv);
028584171116              if xx > 0  ;
028585171116                 sebvaridir = dtasv.va(xx) ;
028586171116              Endif;
028587171116              xx=%lookup('=': dtasv.sv);
028588171116              if xx > 0  ;
028589171116                 sebvarirba = dtasv.va(xx) ;
028590171116              Endif;
028591171116              xx=%lookup('a': dtasv.sv);
028592171116              if xx > 0  ;
028593171116                 sebvaripi = dtasv.va(xx) ;
028594171116              Endif;
028595171116              xx=%lookup('A': dtasv.sv);
028596171116              if xx > 0  ;
028597171116                 sebvaria = dtasv.va(xx) ;
028598171116              Endif;
028599171116              xx=%lookup('b': dtasv.sv);
028600171116              if xx > 0  ;
028601171116                 sebvaribb = dtasv.va(xx) ;
028602171116              Endif;
028603171116              xx=%lookup('B': dtasv.sv);
028604171116              if xx > 0  ;
028605171116                 sebvarib = dtasv.va(xx) ;
028606171116              Endif;
028607171116              xx=%lookup('c': dtasv.sv);
028608171116              if xx > 0  ;
028609171116                 sebvaricc = dtasv.va(xx) ;
028610171116              Endif;
028611171116              xx=%lookup('D': dtasv.sv);
028612171116              if xx > 0  ;
028613171116                 sebvarid = dtasv.va(xx) ;
028614171116              Endif;
028615171116              xx=%lookup('e': dtasv.sv);
028616171116              if xx > 0  ;
028617171116                 sebvariee = dtasv.va(xx) ;
028618171116              Endif;
028619171116              xx=%lookup('F': dtasv.sv);
028620171116              if xx > 0  ;
028621171116                 sebvarif = dtasv.va(xx) ;
028622171116              Endif;
028623171116              xx=%lookup('G': dtasv.sv);
028624171116              if xx > 0  ;
028625171116                 sebvarig = dtasv.va(xx) ;
028626171116              Endif;
028627171116              xx=%lookup('h': dtasv.sv);
028628171116              if xx > 0  ;
028629171116                 sebvarihh = dtasv.va(xx) ;
028630171116              Endif;
028631171116              xx=%lookup('H': dtasv.sv);
028632171116              if xx > 0  ;
028633171116                 sebvarih = dtasv.va(xx) ;
028634171116              Endif;
028635171116              xx=%lookup('I': dtasv.sv);
028636171116              if xx > 0  ;
028637171116                 sebvarii = dtasv.va(xx) ;
028638171116              Endif;
028639171116              xx=%lookup('J': dtasv.sv);
028640171116              if xx > 0  ;
028641171116                 sebvarij = dtasv.va(xx) ;
028642171116              Endif;
028643171116              xx=%lookup('k': dtasv.sv);
028644171116              if xx > 0  ;
028645171116                 sebvarikk = dtasv.va(xx) ;
028646171116              Endif;
028647171116              xx=%lookup('K': dtasv.sv);
028648171116              if xx > 0  ;
028649171116                 sebvarik = dtasv.va(xx) ;
028650171116              Endif;
028651171116              xx=%lookup('L': dtasv.sv);
028652171116              if xx > 0  ;
028653171116                 sebvaril = dtasv.va(xx) ;
028654171116              Endif;
028655171116              xx=%lookup('m': dtasv.sv);
028656171116              if xx > 0  ;
028657171116                 sebvarimm = dtasv.va(xx) ;
028658171116              Endif;
028659171116              xx=%lookup('M': dtasv.sv);
028660171116              if xx > 0  ;
028661171116                 sebvarim = dtasv.va(xx) ;
028662171116              Endif;
028663171116              xx=%lookup('N': dtasv.sv);
028664171116              if xx > 0  ;
028665171116                 sebvarin = dtasv.va(xx) ;
028666171116              Endif;
028667171116              xx=%lookup('O': dtasv.sv);
028668171116              if xx > 0  ;
028669171116                 sebvario = dtasv.va(xx) ;
028670171116              Endif;
028671171116              xx=%lookup('P': dtasv.sv);
028672171116              if xx > 0  ;
028673171116                 sebvarip = dtasv.va(xx) ;
028674171116              Endif;
028675171116              xx=%lookup('Q': dtasv.sv);
028676171116              if xx > 0  ;
028677171116                 sebvariq = dtasv.va(xx) ;
028678171116              Endif;
028679171116              xx=%lookup('S': dtasv.sv);
028680171116              if xx > 0  ;
028681171116                 sebvaris = dtasv.va(xx) ;
028682171116              Endif;
028683171116              xx=%lookup('t': dtasv.sv);
028684171116              if xx > 0  ;
028685171116                 sebvaritt = dtasv.va(xx) ;
028686171116              Endif;
028687171116              xx=%lookup('T': dtasv.sv);
028688171116              if xx > 0  ;
028689171116                 sebvarit = dtasv.va(xx) ;
028690171116              Endif;
028691171116              xx=%lookup('U': dtasv.sv);
028692171116              if xx > 0  ;
028693171116                 sebvariu = dtasv.va(xx) ;
028694171116              Endif;
028695171116              xx=%lookup('W': dtasv.sv);
028696171116              if xx > 0  ;
028697171116                 sebvariw = dtasv.va(xx) ;
028698171116              Endif;
028699171116              xx=%lookup('X': dtasv.sv);
028700171116              if xx > 0  ;
028701171116                 sebvarix = dtasv.va(xx) ;
028702171116              Endif;
028703171116              xx=%lookup('Y': dtasv.sv);
028704171116              if xx > 0  ;
028705171116                 sebvariy = dtasv.va(xx) ;
028706171116              Endif;
028707171116              xx=%lookup('Z': dtasv.sv);
028708171116              if xx > 0  ;
028709171116                 sebvariz = dtasv.va(xx) ;
028710171116              Endif;
028711171116
028712171116              write wfseb000 ;
028713171116         Endsr;
028714171116      /end-free
030300040331      *
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
120800050609      *************************************************************************
120900050609      * Operazioni iniziali
121000050609      *************************************************************************
121100050609     C     *inzsr        BEGSR
124401171116      *  KLIST PER ACCESSO FILES
124402171116     c     KTA7          KLIST
124403171116     c                   KFLD                    TASAAS
124404171116     c                   KFLD                    TASLNP
124405171116     c                   KFLD                    TASNRS
124406171116     c                   KFLD                    TASNSP
124407171116     c                   KFLD                    TASTBL
124509171116      *
127000050609     c
127100050609     c     *entry        plist
127200050609     c                   parm                    kpjba
127400060915
135300050609     c                   endsr
135400171116** KSC
1355001711161301247 GROUPE SEB ITALIA SPA
1356001711160505378 GROUPE SEB ITALIA SPA
1357001711160505426 GROUPE SEB ITALIA SPA
1358001711160898508 GROUPE SEB ITALIA SPA
1359001711162230395 GROUPE SEB ITALIA SPA
1360001711160220835 GROUPE SEB ITALIA SPA C/O DHL
1361001711160504042 LAGOSTINA S.P.A.
1362001711161560031 LAGOSTINA S.P.A.
1363001711160504159 LAGOSTINA S.P.A.
1364001711161560104 LAGOSTINA S.P.A.
