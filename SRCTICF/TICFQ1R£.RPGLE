000100130918     /*CMD addlible lib(EDPCONFAT)
000200130918     /*CMD addlible lib(EDPCONF_TA)
000300130918     /*PRM dbgview(*source)
000400130918     /*END rmvlible lib(EDPCONFAT)
000500130918     /*END rmvlible lib(EDPCONF_TA)
000600130918     /*END
000700040119      *===============================================================*
000800150908      * TICFQ1R    * Confronto fatturazione (2) + varie (progetto 581)
000900040119      *===============================================================*
001000040119
001100040119     h decedit('0,') datedit(*dmy/)
001200040119     h option(*nodebugio)
001300040119
001400040119      *---------------------------------------------------------------*
001500040119
001600050608     fwfcft02l  if   e           k disk
001700050608     fwfcft10f  uf a e             disk
001800050608     FTABEL00F  IF   E           K DISK
001900130913     fAZCMM01L  if   e           k disk
002000050609     fazorg01l  if   e           k disk
002100080117     ftntam01l  if   e           k disk
002200120208
002300120208      //?File con totali x cliente/tariffa + porto + varie
002400120208     fwfcft20f  uf a e             disk
002500120208
002600120208      //?File di stampa
002700150908     fticfq1p   o    e             printer usropn oflind(*in90)
002800130913
002900130913      *---------------------------------------------------------------*
003000050609
003100050614     d param           ds
003200050614     D  pardrf                 1      8  0
003300141015     D  paraamd               17     22  0
003400141015     D  paraama               25     30  0
003500120208
003600050614     d kpjba         e ds
003700120208
003800050608     d ds17          e ds
003900050608     d ds05          e ds
004000080117     d dsta01        e ds
004100120208
004200050609     d tibs10ds      e ds
004300070516     d tise70ds      e ds
004400120208
004500050608     d TIBS69DS      E DS                  INZ
004600050608     d DS_cnaco      E DS                  extname(CNACO00F)
004700050608     d DS_cnind      E DS                  extname(CNIND00F)
004800050608     d DS_cnclp      E DS                  extname(CNCLP00F)
004900050608     d DS_fncls      E DS                  extname(FNCLS00F)
005000120208
005100050609     D wlbda8          ds                  inz                                  *controlla data
005200050609     D  g08dat                        8  0                                       -data dritta
005300050609     D  g08inv                        8  0                                       -data invertita
005400050609     D  g08err                        1                                          -errore
005500050609     D  g08tgi                        5  0                                       -giorni fra date
005600130913
005700130913      *---------------------------------------------------------------*
005800050608
005900050608     d codut           s              1  0 Inz(1)
006000050608     D kcod            s                   like(tblcod)
006100050608     D kkey            s                   like(tblkey)
006200120208
006300050608     D wnsp            s                   like(cft1tos)
006400050608     D wimvp           s                   like(cft1top)
006500050608     D wimvc           s                   like(cft1toc)
006600130125     D wistatr         s                   like(cft1istatp)
006700130125     D wfuelr          s                   like(cft1fuelp)
006800130114     D wistatc         s                   like(cft1istatc)
006900130114     D wfuelc          s                   like(cft1fuelc)
007000130114     D diffeistat      s                   like(cft1distat)
007100130114     D diffefuel       s                   like(cft1dfuel)
007200130128     D timpistat       s                   like(cft1impist)
007300130128     D timpfuel        s                   like(cft1impfue)
007400120208
007500120208     d wtporp          s                   like(CFT2tporp)
007600120208     d wtporc          s                   like(CFT2tporc)
007700120208     d wpos            s              3s 0
007800120208     d sktvp           s             13s 3 dim(40)
007900120208     d sktvc           s             13s 3 dim(40)
008000120208     d sksv            s             38a   dim(01) ctdata perrcd(1)
008100120208     d wSigla          s              1a
008200120208     d wImporto        s             13s 3
008300120208     d wRif            s              1a
008400120208
008500050608     D s_cftksc        s                   like(cftksc)
008600050609     D s_cftctr        s                   like(cftctr)
008700160308     D s_CFTnewtar     s                   like(CFTnewtar)
008800130913
008900130913      *---------------------------------------------------------------*
009000050608
009100050614     C     *entry        plist
009200050614     C                   parm                    kpjba
009300050614     c                   movel     kpjbu         param
009400050614     c*
009500050608     C     KTAB          KLIST
009600050608     C                   KFLD                    CODUT
009700050608     C                   KFLD                    KCOD
009800050608     C                   KFLD                    KKEY
009900080117      *
010000080117     C     KTAM          KLIST
010100080117     C                   KFLD                    s_cftksc
010200080117     C                   KFLD                    s_cftctr
010300050609     C*--------------------
010400050609     C* CALCOLA LA DATA CORRENTE
010500050609     C*--------------------
010600050609     C                   time                    WN14             14 0          *ORA (6)+ DATA (8)
010700050609     C                   move      WN14          WN8               8 0          *DATA (8) IN G/M/AA
010800050609     C                   z-add     WN8           G08DAT
010900050609     C                   z-add     *zeros        G08INV
011000050609     C                   movel     '0'           G08ERR
011100050609     C                   call      'XSRDA8'
011200050609     C                   parm                    WLBDA8
011300050609     C                   z-add     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
011400130913      *
011500050608     C     *loval        setll     wfcft02l
011600050608     C                   do        *hival
011700050608     C                   read      wfcft02l
011800050608     C                   if        %eof(wfcft02l)
011900050608     C                   leave
012000050608     C                   endif
012100050609     c* a rottura cliente/tariffa memorizzo dati nel file
012200050608     c                   if        cftksc <> s_cftksc
012300050608     c                   if        s_cftksc > 0
012400050608     c                   exsr      sr_wrtcft
012500050608     c                   endif
012600050608     c                   z-add     cftksc        s_cftksc
012700050609     c                   z-add     cftctr        s_cftctr
012800160308     c                   eval      s_CFTnewtar = CFTnewtar
012900050608     c* azzero contatori
013000050608     c                   clear                   wnsp
013100050608     c                   clear                   wimvp
013200050608     c                   clear                   wimvc
013300120208     c                   clear                   wtporp
013400120208     c                   clear                   wtporc
013500130125     c                   clear                   wistatr
013600130114     c                   clear                   wistatc
013700130125     c                   clear                   wfuelr
013800130114     c                   clear                   wfuelc
013900120208     c                   clear                   sktvp
014000120208     c                   clear                   sktvc
014100130114     c                   clear                   diffeistat
014200130114     c                   clear                   diffefuel
014300130125     c                   clear                   timpistat
014400130125     c                   clear                   timpfuel
014500050608     c                   endif
014600050609     c*
014700050609     c                   if        cftctr <> s_cftctr
014800050609     c                   exsr      sr_wrtcft
014900050609     c                   z-add     cftctr        s_cftctr
015000160308     c                   eval      s_CFTnewtar = CFTnewtar
015100050609     c* azzero contatori
015200050609     c                   clear                   wnsp
015300050609     c                   clear                   wimvp
015400050609     c                   clear                   wimvc
015500120208     c                   clear                   wtporp
015600120208     c                   clear                   wtporc
015700130125     c                   clear                   wistatr
015800130114     c                   clear                   wistatc
015900130125     c                   clear                   wfuelr
016000130114     c                   clear                   wfuelc
016100120208     c                   clear                   sktvp
016200120208     c                   clear                   sktvc
016300130114     c                   clear                   diffeistat
016400130114     c                   clear                   diffefuel
016500130125     c                   clear                   timpistat
016600130125     c                   clear                   timpfuel
016700050609     c                   endif
016800130913     c*
016900050608     c* INCREMENTO CONTATORI
017000050608     c* numero spedizioni elaborate
017100050608     c                   add       1             wnsp
017200050608     c                   add       cftimvp       wimvp
017300050608     c                   add       cftimvc       wimvc
017400130114      *sommo il fuel e istat ricalcolati
017500130125     c                   add       cftistatr     wistatr
017600130114     c                   add       cftistatc     wistatc
017700130125     c                   add       cftfuelr      wfuelr
017800130114     c                   add       cftfuelc      wfuelc
017900130125     c                   add       cftimpist     timpistat
018000130125     c                   add       cftimpfue     timpfuel
018100130913      *
018200120208      /free
018300120208       //?Sommo il porto
018400120208        wtporp += CFTporp;
018500120208        wtporc += CFTporc;
018600120208       //?Sommo le varie in posizioni fisse
018700120208       //?precedente
018800120208        wRif = 'P';
018900120208        wSigla = CFTs01p;
019000120208        Wimporto = CFTv01p;
019100120208        exsr Varia;
019200120208        wSigla = CFTs02p;
019300120208        Wimporto = CFTv02p;
019400120208        exsr Varia;
019500120208        wSigla = CFTs03p;
019600120208        Wimporto = CFTv03p;
019700120208        exsr Varia;
019800120208        wSigla = CFTs04p;
019900120208        Wimporto = CFTv04p;
020000120208        exsr Varia;
020100120208        wSigla = CFTs05p;
020200120208        Wimporto = CFTv05p;
020300120208        exsr Varia;
020400120208        wSigla = CFTs06p;
020500120208        Wimporto = CFTv06p;
020600120208        exsr Varia;
020700120208        wSigla = CFTs07p;
020800120208        Wimporto = CFTv07p;
020900120208        exsr Varia;
021000120208        wSigla = CFTs08p;
021100120208        Wimporto = CFTv08p;
021200120208        exsr Varia;
021300120208        wSigla = CFTs09p;
021400120208        Wimporto = CFTv09p;
021500120208        exsr Varia;
021600120208        wSigla = CFTs10p;
021700120208        Wimporto = CFTv10p;
021800120208        exsr Varia;
021900120208        wSigla = CFTs11p;
022000120208        Wimporto = CFTv11p;
022100120208        exsr Varia;
022200120208        wSigla = CFTs12p;
022300120208        Wimporto = CFTv12p;
022400120208        exsr Varia;
022500120208        wSigla = CFTs13p;
022600120208        Wimporto = CFTv13p;
022700120208        exsr Varia;
022800120208        wSigla = CFTs14p;
022900120208        Wimporto = CFTv14p;
023000120208        exsr Varia;
023100120208        wSigla = CFTs15p;
023200120208        Wimporto = CFTv15p;
023300120208        exsr Varia;
023400120208        wSigla = CFTs16p;
023500120208        Wimporto = CFTv16p;
023600120208        exsr Varia;
023700120208        wSigla = CFTs17p;
023800120208        Wimporto = CFTv17p;
023900120208        exsr Varia;
024000120208        wSigla = CFTs18p;
024100120208        Wimporto = CFTv18p;
024200120208        exsr Varia;
024300120208        wSigla = CFTs19p;
024400120208        Wimporto = CFTv19p;
024500120208        exsr Varia;
024600120208        wSigla = CFTs20p;
024700120208        Wimporto = CFTv20p;
024800120208        exsr Varia;
024900120208       //?corrente
025000120208        wRif = 'C';
025100120208        wSigla = CFTs01c;
025200120208        Wimporto = CFTv01c;
025300120208        exsr Varia;
025400120208        wSigla = CFTs02c;
025500120208        Wimporto = CFTv02c;
025600120208        exsr Varia;
025700120208        wSigla = CFTs03c;
025800120208        Wimporto = CFTv03c;
025900120208        exsr Varia;
026000120208        wSigla = CFTs04c;
026100120208        Wimporto = CFTv04c;
026200120208        exsr Varia;
026300120208        wSigla = CFTs05c;
026400120208        Wimporto = CFTv05c;
026500120208        exsr Varia;
026600120208        wSigla = CFTs06c;
026700120208        Wimporto = CFTv06c;
026800120208        exsr Varia;
026900120208        wSigla = CFTs07c;
027000120208        Wimporto = CFTv07c;
027100120208        exsr Varia;
027200120208        wSigla = CFTs08c;
027300120208        Wimporto = CFTv08c;
027400120208        exsr Varia;
027500120208        wSigla = CFTs09c;
027600120208        Wimporto = CFTv09c;
027700120208        exsr Varia;
027800120208        wSigla = CFTs10c;
027900120208        Wimporto = CFTv10c;
028000120208        exsr Varia;
028100120208        wSigla = CFTs11c;
028200120208        Wimporto = CFTv11c;
028300120208        exsr Varia;
028400120208        wSigla = CFTs12c;
028500120208        Wimporto = CFTv12c;
028600120208        exsr Varia;
028700120208        wSigla = CFTs13c;
028800120208        Wimporto = CFTv13c;
028900120208        exsr Varia;
029000120208        wSigla = CFTs14c;
029100120208        Wimporto = CFTv14c;
029200120208        exsr Varia;
029300120208        wSigla = CFTs15c;
029400120208        Wimporto = CFTv15c;
029500120208        exsr Varia;
029600120208        wSigla = CFTs16c;
029700120208        Wimporto = CFTv16c;
029800120208        exsr Varia;
029900120208        wSigla = CFTs17c;
030000120208        Wimporto = CFTv17c;
030100120208        exsr Varia;
030200120208        wSigla = CFTs18c;
030300120208        Wimporto = CFTv18c;
030400120208        exsr Varia;
030500120208        wSigla = CFTs19c;
030600120208        Wimporto = CFTv19c;
030700120208        exsr Varia;
030800120208        wSigla = CFTs20c;
030900120208        Wimporto = CFTv20c;
031000120208        exsr Varia;
031100120208      /end-free
031200130913      *
031300130111      * calcolo le differenze tra istat e fuel ricalcolati con data spedizione uguale alla bolla
031400130111      * e tassazione corrente
031500130125     c                   eval      diffeistat =diffeistat +(cftistatc-cftistatr)
031600130125     c                   eval      diffefuel  =diffefuel  + (cftfuelc-cftfuelr)
031700130913      *
031800130913     c*
031900050608     c                   enddo
032000050609     c* memorizzo ultimi dati nel file
032100050608     c                   exsr      sr_wrtcft
032200130913      *
032300120208      /free
032400120208       //?Stampo il fine elenco
032500150908         IF  %open(TICFQ1P);
032600120208           write  CF91F1;
032700120208         ENDIF;
032800120208      /end-free
032900130913      *
033000050608     c*
033100050608     c                   seton                                        lr
033200130913      *
033300050608     C                   eval      I69TLA  = 'C'
033400050608     C                   call      'TIBS69R'
033500050608     C                   parm                    TIBS69DS
033600050608      **-------------------------------------------------------------**
033700050608      ** Scrittura file                                              **
033800050608      **-------------------------------------------------------------**
033900050608     c     sr_wrtcft     BEGSR
034000050608     c                   clear                   wfcft100
034100080129      * recupero anagrafica cliente
034200080129     C                   clear                   DS_cnaco
034300080129     C                   clear                   DS_cnind
034400080129     C                   clear                   DS_cnclp
034500080129     C                   clear                   DS_fncls
034600080129     c                   eval      I69kac = s_cftksc
034700080129     c                   eval      I69kcp = s_cftksc
034800080129     C                   CALL      'TIBS69R'
034900080129     C                   PARM                    tibs69DS
035000080129     C                   PARM                    DS_cnaco
035100080129     C                   PARM                    DS_cnind
035200080129     C                   PARM                    DS_cnclp
035300080129     C                   PARM                    DS_fncls
035400080129     c                   z-add     s_cftksc      cft1ksc
035500080129     c                   movel     acorag        cft1kscd
035600080129     c                   z-add     s_cftctr      cft1ctr
035700080129      * reperimento del tipo tariffa
035800080129     c     ktam          chain     tntam01l
035900080129     c                   if        %found(tntam01l)
036000080129     c                   eval      dsta01 = tamflo
036100080129      * carico se tariffa I - E - D - F
036200080129     c                   select
036300080129     c                   when      §tadpd = 'S'
036400080129     c                   eval      cft1ttr = 'D'
036500080129     c                   when      §tafed = 'S'
036600080129     c                   eval      cft1ttr = 'F'
036700080129     c                   other
036800080129     c                   eval      cft1ttr = tamfie
036900080129     c                   endsl
037000130913      *
037100080129     c                   endif
037200130913      *
037300080129     c* reperimento cod.cliente unificante
037400080129     c                   clear                   tibs10ds
037500080129     c                   eval      d10tle = 'ST'
037600080129     c                   eval      d10paf = 'P'
037700080129     c                   z-add     cft1ksc       d10cod
037800080129     c                   z-add     datcor        d10drf
037900080129     c                   call      'TIBS10R'
038000080129     c                   Parm                    Tibs10ds
038100080129    1c                   If        d10err =  *Blanks
038200080129     c                   z-add     d10cop        cft1kun
038300080129   x1c                   else
038400080129     c                   z-add     cft1ksc       cft1kun
038500080129    1c                   endif
038600080129    1c                   if        cft1ksc = cft1kun
038700080129     c                   movel     cft1kscd      cft1kund
038800080129     c                   movel     clpclv        cft1clv
038900080129   x1c                   else
039000080129     C                   clear                   DS_cnaco
039100080129     C                   clear                   DS_cnind
039200080129     C                   clear                   DS_cnclp
039300080129     C                   clear                   DS_fncls
039400080129     c                   eval      I69kac = cft1kun
039500080129     c                   eval      I69kcp = cft1kun
039600080129     C                   CALL      'TIBS69R'
039700080129     C                   PARM                    tibs69DS
039800080129     C                   PARM                    DS_cnaco
039900080129     C                   PARM                    DS_cnind
040000080129     C                   PARM                    DS_cnclp
040100080129     C                   PARM                    DS_fncls
040200080129     c                   movel     acorag        cft1kund
040300080129     c                   movel     clpclv        cft1clv
040400080129    1c                   endif
040500080129     c                   z-add     wimvp         cft1top
040600080129     c                   z-add     wimvc         cft1toc
040700080129     c                   z-add     wnsp          cft1tos
040800130125     c                   z-add     wistatr       cft1istatp
040900130114     c                   z-add     wistatc       cft1istatc
041000130125     c                   z-add     wfuelr        cft1fuelp
041100130114     c                   z-add     wfuelc        cft1fuelc
041200130114     c                   z-add     diffeistat    cft1distat
041300130114     c                   z-add     diffefuel     cft1dfuel
041400130128     c                   z-add     timpistat     cft1impist
041500130128     c                   z-add     timpfuel      cft1impfue
041600130913      *
041700080219      * dal 2° confronto fatturazione recuperiamo il codice agente del cliente unificante e non
041800080219      * del figlio
041900080219     c                   z-add     clpage        cft1age
042000130913     C     CFT1age       CHAIN     AZCMM000
042100130913    1c                   if        %found(AZCMM01L)
042200130913     c                   movel     CMMdes        cft1aged
042300130913     c                   z-add     CMMuni        cft1rgf
042400130913    1c                   endif
042500130913     C     CFT1rgf       CHAIN     AZCMM000
042600130913    1c                   if        %found(AZCMM01L)
042700130918     c                   movel     CMMdes        cft1rgfd
042800130913    1c                   endif
042900080129      * recupero p.o. area e distretto del codice commerciale unificante e non + del cliente
043000080129      * come da richiesta del 2008 di Cuffaro Alessandro
043100080129     c                   movel     cft1rgf       cft1fil
043200080129     c* reperisco cod. distretto e area
043300050608     c     cft1fil       chain     azorg01l
043400050608     C* Decodifico distretto
043500050608     C                   EXSR      DECDST
043600050608     C* Decodifico area
043700050608     C                   EXSR      DECCAR
043800050608     c* decodifica p.o.
043900050608     c                   movel     orgdes        cft1fild
044000050608     c                   movel     orgfl3        cft1dit
044100050608     c                   movel     §17des        cft1ditd
044200050608     c                   z-add     orgcar        cft1are
044300050608     c                   movel     §05des        cft1ared
044400050614     c* richiamo pgm per reperimento numero di spedizioni da file saldi
044500070516     c                   clear                   tise70ds
044600050614     c                   movel     'S'           i70tla
044700050614     c                   z-add     cft1ksc       i70ksc
044800050614     c                   move      cft1ctr       i70ctr
044900141015     c                   z-add     paraamd       i70dai
045000141015     c                   z-add     paraama       i70daf
045100070516     c                   call      'TISE70R'
045200070516     c                   parm                    tise70ds
045300050614     c                   z-add     o70nsp        cft1nsp
045400160308      * flag nuova tariffa
045500160308     c                   eval      CFT1newtar = s_CFTnewtar
045600050614     c
045700050610     c                   write     wfcft100
045800130913      *
045900120208      /free
046000120208       //?Scrivo file 20F
046100120208        clear WFCFT200;
046200120208       //?Dati dal file 10F
046300120208        CFT2dit  = CFT1dit;
046400120208        CFT2ditd = CFT1ditd;
046500120208        CFT2are  = CFT1are;
046600120208        CFT2ared = CFT1ared;
046700120208        CFT2fil  = CFT1fil;
046800120208        CFT2fild = CFT1fild;
046900120208        CFT2age  = CFT1age;
047000120208        CFT2aged = CFT1aged;
047100120208        CFT2rgf  = CFT1rgf;
047200120208        CFT2rgfd = CFT1rgfd;
047300120208        CFT2ksc  = CFT1ksc;
047400120208        CFT2kscd = CFT1kscd;
047500120208        CFT2ctr  = CFT1ctr;
047600120208        CFT2ttr  = CFT1ttr;
047700120208        CFT2clv  = CFT1clv;
047800120208        CFT2kun  = CFT1kun;
047900120208        CFT2kund = CFT1kund;
048000120208        CFT2top  = CFT1top;
048100120208        CFT2toc  = CFT1toc;
048200120208        CFT2tos  = CFT1tos;
048300120208        CFT2nsp  = CFT1nsp;
048400120208       //?Porto
048500120208        CFT2tporp = wtporp;
048600120208        CFT2tporc = wtporc;
048700120208       //?Varie
048800120208        CFT2tvp2   = sktvp(01);
048900120208        CFT2tvc2   = sktvc(01);
049000120208        CFT2tvp5   = sktvp(02);
049100120208        CFT2tvc5   = sktvc(02);
049200120208        CFT2tvpapa = sktvp(03);
049300120208        CFT2tvcapa = sktvc(03);
049400120208        CFT2tvpdir = sktvp(04);
049500120208        CFT2tvcdir = sktvc(04);
049600120208        CFT2tvpsca = sktvp(05);
049700120208        CFT2tvcsca = sktvc(05);
049800120208        CFT2tvprba = sktvp(06);
049900120208        CFT2tvcrba = sktvc(06);
050000120208        CFT2tvpaa  = sktvp(07);
050100120208        CFT2tvcaa  = sktvc(07);
050200120208        CFT2tvpa   = sktvp(08);
050300120208        CFT2tvca   = sktvc(08);
050400120208        CFT2tvpbb  = sktvp(09);
050500120208        CFT2tvcbb  = sktvc(09);
050600120208        CFT2tvpb   = sktvp(10);
050700120208        CFT2tvcb   = sktvc(10);
050800120208        CFT2tvpcc  = sktvp(11);
050900120208        CFT2tvccc  = sktvc(11);
051000120208        CFT2tvpc   = sktvp(12);
051100120208        CFT2tvcc   = sktvc(12);
051200120208        CFT2tvpdd  = sktvp(13);
051300120208        CFT2tvcdd  = sktvc(13);
051400120208        CFT2tvpd   = sktvp(14);
051500120208        CFT2tvcd   = sktvc(14);
051600120208        CFT2tvpee  = sktvp(15);
051700120208        CFT2tvcee  = sktvc(15);
051800120208        CFT2tvpff  = sktvp(16);
051900120208        CFT2tvcff  = sktvc(16);
052000120208        CFT2tvpf   = sktvp(17);
052100120208        CFT2tvcf   = sktvc(17);
052200120208        CFT2tvpg   = sktvp(18);
052300120208        CFT2tvcg   = sktvc(18);
052400120208        CFT2tvphh  = sktvp(19);
052500120208        CFT2tvchh  = sktvc(19);
052600120208        CFT2tvph   = sktvp(20);
052700120208        CFT2tvch   = sktvc(20);
052800120208        CFT2tvpi   = sktvp(21);
052900120208        CFT2tvci   = sktvc(21);
053000120208        CFT2tvpj   = sktvp(22);
053100120208        CFT2tvcj   = sktvc(22);
053200120208        CFT2tvpk   = sktvp(23);
053300120208        CFT2tvck   = sktvc(23);
053400120208        CFT2tvpl   = sktvp(24);
053500120208        CFT2tvcl   = sktvc(24);
053600120208        CFT2tvpmm  = sktvp(25);
053700120208        CFT2tvcmm  = sktvc(25);
053800120208        CFT2tvpm   = sktvp(26);
053900120208        CFT2tvcm   = sktvc(26);
054000120208        CFT2tvpn   = sktvp(27);
054100120208        CFT2tvcn   = sktvc(27);
054200120208        CFT2tvpo   = sktvp(28);
054300120208        CFT2tvco   = sktvc(28);
054400120208        CFT2tvpp   = sktvp(29);
054500120208        CFT2tvcp   = sktvc(29);
054600120208        CFT2tvpq   = sktvp(30);
054700120208        CFT2tvcq   = sktvc(30);
054800120208        CFT2tvpr   = sktvp(31);
054900120208        CFT2TVCr   = sktvc(31);
055000120208        CFT2tvps   = sktvp(32);
055100120208        CFT2tvcs   = sktvc(32);
055200120208        CFT2tvpt   = sktvp(33);
055300120208        CFT2tvct   = sktvc(33);
055400120208        CFT2tvpu   = sktvp(34);
055500120208        CFT2tvcu   = sktvc(34);
055600120208        CFT2tvpw   = sktvp(35);
055700120208        CFT2tvcw   = sktvc(35);
055800120208        CFT2tvpx   = sktvp(36);
055900120208        CFT2tvcx   = sktvc(36);
056000120208        CFT2tvpy   = sktvp(37);
056100120208        CFT2tvcy   = sktvc(37);
056200120208        CFT2tvpz   = sktvp(38);
056300120208        CFT2tvcz   = sktvc(38);
056400130114
056500130125        CFT2istap  = wistatr;
056600130114        CFT2istac  = wistatc;
056700130125        CFT2fuelp  = wfuelr;
056800130114        CFT2fuelc  = wfuelc;
056900120208
057000120208        WRITE wfcft200;
057100120208      /end-free
057200130913      *
057300050608     c                   endsr
057400050608     C*****************************************************************
057500050608     C* DECODIFICA CODICE DISTRETTO
057600050608     C*****************************************************************
057700050608     C     DECDST        BEGSR
057800050608     C* Chaino tabella 17 per decodifica
057900050608     C                   MOVEL     '17'          KCOD
058000050608     C                   MOVEL(P)  ORGFL3        KKEY
058100050608     c                   clear                   ds17
058200050608     C     KTAB          CHAIN     TABEL00F                           30
058300050608    2C     *IN30         IFEQ      *OFF
058400050608     C                   MOVEL     TBLUNI        DS17
058500050608     c                   endif
058600050608     C                   ENDSR
058700050608     C*****************************************************************
058800050608     C* DECODIFICA CODICE AREA
058900050608     C*****************************************************************
059000050608     C     DECCAR        BEGSR
059100050608     C* Chaino tabella 05 per decodifica
059200050608     C                   MOVEL     '05'          KCOD
059300050608     C                   MOVEL(P)  ORGCAR        KKEY
059400050608     c                   clear                   ds05
059500050608     C     KTAB          CHAIN     TABEL00F                           30
059600050608    2C     *IN30         IFEQ      *OFF
059700050608     C                   MOVEL     TBLUNI        DS05
059800050608     c                   endif
059900050608     C                   ENDSR
060000130913      *
060100120208      /free
060200120208       //--------------------------------------------------------------
060300120208       //?Incasello le varie.
060400120208       //--------------------------------------------------------------
060500120208       BEGSR Varia;
060600120208
060700120208         IF  wSigla <> *blanks;
060800120208           wpos = %scan(wSigla:sksv(01));
060900120208           IF  wpos > 0;
061000120208             IF  wRif = 'P';
061100120208               sktvp(wpos)+= wImporto;
061200120208             ENDIF;
061300120208             IF  wRif = 'C';
061400120208               sktvc(wpos)+= wImporto;
061500120208             ENDIF;
061600120208           ELSE;
061700120208             exsr Stampa;
061800120208           ENDIF;
061900120208         ENDIF;
062000120208
062100120208       ENDSR;
062200120208       //--------------------------------------------------------------
062300120208       //?Stampa errore.
062400120208       //--------------------------------------------------------------
062500120208       BEGSR Stampa;
062600120208
062700120208       //?File di stampa non ancora aperto
062800150908         IF  not %open(TICFQ1P);
062900150908           open TICFQ1P;
063000120208           write  CF91T1;
063100120208         ENDIF;
063200120208
063300120208       //?Salto pagina
063400120208         IF  *in99;
063500120208           write  CF91T1;
063600120208           *in99 = *off;
063700120208         ENDIF;
063800120208
063900120208       //?Dettaglio
064000120208         Sigla = wSigla;
064100120208         Importo = wImporto;
064200120208         Periodo = wRif;
064300120208         write CF91D1;
064400120208
064500120208       ENDSR;
064600120208
064700120208      /end-free
064800120208
064900120208** -sksv 1---------2---------3---------4---------5---------6---------7-------*
06500012020825&*+=aAbBcCdDefFGhHIJKLmMNOPQRSTUWXYZ
