000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400090615     Ftrmz87d   CF   E             WORKSTN
000500090615     Faitra01l  if   e           k disk
000600090615     Faitrs01l  if   e           k disk
000700090615     Ffiadt01l  if   e           k disk
000800090615     Ffitgt01l  if   e           k disk
000900090615     Ftrmz870f  uf a e           k disk
001000090616     Fsysprt    O    F  132        PRINTER prtctl(prtds)
001100020304     dkpjba          e ds
001200090616     d Tibs36Ds      e ds
001300090616     d prtds           ds
001400090616     d   spab                         3  0
001500090616     d   spaa                         3  0
001600090616     d   skab                         3  0
001700090616     d   skaa                         3  0
001800090616     d   line                         3  0
001900070605     d dataiso         s               d   datfmt(*iso)
002000070605     d dataeur         s               d   datfmt(*eur)
002100070605     D WLBDA8          DS
002200070605     D  G02DAT                 1      8  0
002300070605     D  G02INV                 9     16  0
002400070605     D  G02ERR                17     17
002500070605     D  G02TGI                18     22  0
002600080502      *
002700000000     C*---------------------------------------------------------------*
002800020304      *
002900031105     c                   do        *hival
003000031105     c                   exfmt     video1
003100031105     c                   clear                   $msg
003200031105     c   kc              leave
003300031105     c                   exsr      controv1
003400031105     c   96              iter
003500090616     c     ktrm          chain     trmz870f
003600090616     c                   if        %found(trmz870f)
003700090616     c                   eval      $msg = 'Mese già estratto '
003800090616     c                   iter
003900090616     c                   endif
004000050623     c                   if        *inkf
004100090604
004200090616     c                   clear                   trmz8700
004300050623     c                   exsr      elabora
004400050805     c                   leave
004500050623     c                   endif
004600050805     c                   enddo
004700050802
004800020308     c                   seton                                        lr
004900990113     c*-------------------------------------------------
005000090604     c     elabora       begsr
005100990113     c*-------------------------------------------------
005200090616      * ?contratti
005300090615     c     *loval        setll     aitrs01l
005400981029     c                   do        *hival
005500090615     c                   read      aitrs01l
005600090615     c                   if        %eof(aitrs01l)
005700090604     c                   leave
005800090604     c                   endif
005900090616     c                   if        trsann <> *blank
006000090616     c                   iter
006100090616     c                   endif
006200090616      * contratti stampati
006300090616     c                   if        trsdsc >= iniper  and
006400090616     c                             trsdsc <= finper
006500090616     c                   add       1             t87con
006600090616     c                   endif
006700090616      * contratti rientrati firmati
006800090616     c                   if        trsdrc >= iniper  and
006900090616     c                             trsdrc <= finper
007000090616     c                   add       1             t87conr
007100090616     c                   endif
007200050623     c                   enddo
007300090616      * ?autisti
007400090615     c     *loval        setll     aitra01l
007500090615     c                   do        *hival
007600090615     c                   read      aitra01l
007700090615     c                   if        %eof(aitra01l)
007800090615     c                   leave
007900090615     c                   endif
008000090616     c                   if        traann <> *blank
008100090616     c                   iter
008200090616     c                   endif
008300090616      * autisti accreditati città e aff/def
008400090616     c                   if        tradin >= iniper  and
008500090616     c                             tradin <= finper
008600090616     c                   if        trakau > 0
008700090616     c                   add       1             t87autc
008800090616     c                   endif
008900090616     c                   if        trabpt > 0
009000090616     c                   add       1             t87auta
009100090616     c                   endif
009200090616     c                   endif
009300090616      * autisti disaccreditati città e aff/def
009400090616     c                   if        tradfi >= iniper  and
009500090616     c                             tradfi <= finper
009600090616     c                   if        trakau > 0
009700090616     c                   add       1             t87autcd
009800090616     c                   endif
009900090616     c                   if        trabpt > 0
010000090616     c                   add       1             t87autad
010100090616     c                   endif
010200090616     c                   endif
010300090615     c                   enddo
010400090616      * ?tariffe città
010500090615     c     *loval        setll     fitgt01l
010600090615     c                   do        *hival
010700090615     c                   read      fitgt01l
010800090615     c                   if        %eof(fitgt01l)
010900090615     c                   leave
011000090615     c                   endif
011100090616     c                   if        tgtatb <> *blank
011200090616     c                   iter
011300090616     c                   endif
011400090616      * allegati stampati
011500090616     c                   if        tgtdcn >= iniper  and
011600090616     c                             tgtdcn <= finper
011700090616     c                   add       1             t87all
011800090616     c                   endif
011900090616      * allegati rientrati firmati
012000090616     c                   if        tgtdrc >= iniper  and
012100090616     c                             tgtdrc <= finper
012200090616     c                   add       1             t87allr
012300090616     c                   endif
012400090615     c                   enddo
012500090616      * ?tariffe affluenze defluenze
012600090615     c     *loval        setll     fiadt01l
012700090615     c                   do        *hival
012800090615     c                   read      fiadt01l
012900090615     c                   if        %eof(fiadt01l)
013000090615     c                   leave
013100090615     c                   endif
013200090616     c                   if        adtatb <> *blank
013300090616     c                   iter
013400090616     c                   endif
013500090616      * allegati stampati
013600090616     c                   if        adtdcn >= iniper  and
013700090616     c                             adtdcn <= finper
013800090616     c                   add       1             t87allad
013900090616     c                   endif
014000090616      * allegati rientrati firmati
014100090616     c                   if        adtdrc >= iniper  and
014200090616     c                             adtdrc <= finper
014300090616     c                   add       1             t87alladr
014400090616     c                   endif
014500090615     c                   enddo
014600090616     c                   move      vanno         t87anno
014700090616     c                   move      vmese         t87mese
014800090616     c                   move      wdatg         t87data
014900090617     c  n25              write     trmz8700
015000090616     c                   except    testata
015100020304
015200020304     c                   endsr
015300981029      *
015400031105     c*-------------------------------------------------
015500031105     c     controv1      begsr
015600031105     c*-------------------------------------------------
015700090604     c                   setoff                                       964241
015800051121
015900090617      * 25 off lancio per utenti
016000090617     c                   if        not *in25
016100090617     c                   if        vmese  = 0
016200090604     C                   seton                                        4196
016300090604     c                   goto      endcontr
016400090604     C                   endif
016500090617     c                   if        vanno = 0
016600090615     C                   seton                                        4296
016700090615     c                   goto      endcontr
016800090615     C                   endif
016900090615     c                   move      0101          mesgio
017000090615     c                   movel     vmese         mesgio            4 0
017100090615     c                   movel     vanno         iniper
017200090615     c                   move      mesgio        iniper            8 0
017300090615     c                   move      iniper        dataiso
017400090615     c     dataiso       adddur    1:*m          dataiso
017500090616     c     dataiso       subdur    1:*d          dataiso
017600090616     c                   move      dataiso       finper            8 0
017700090615     c                   move      wdat          dataeur
017800090615     c                   move      dataeur       dataiso
017900090615     c                   move      dataiso       wdatg             8 0
018000090617      * 25 on
018100090617      * lancio per edp senza generazione file x periodo
018200090617     c                   else
018300090617     c                   if        vdatal = 0 or vdatdal = 0
018400090617     c                   eval      $msg = 'Inserire entrambe le date'
018500090617     c                   seton                                        96
018600090617     c                   goto      endcontr
018700090617     c                   endif
018800090617      *
018900090617     c                   if        vdatdal > 0
019000090617     C                   MOVE      Vdatdal       G02DAT
019100090617     C                   MOVEL     *BLANK        G02ERR
019200090617     C                   CALL      'XSRDA8'
019300090617     C                   PARM                    WLBDA8
019400090617     C     G02ERR        IFEQ      '1'
019500090617     C                   SETON                                          96
019600090617     c                   goto      endcontr
019700090617     C                   endif
019800090617     c                   move      g02dat        vdatdal
019900090617     c                   move      g02inv        vdatdalg          8 0
020000090617     c                   endif
020100090617     c                   if        vdatal > 0
020200090617     C                   MOVE      Vdatal        G02DAT
020300090617     C                   MOVEL     *BLANK        G02ERR
020400090617     C                   CALL      'XSRDA8'
020500090617     C                   PARM                    WLBDA8
020600090617     C     G02ERR        IFEQ      '1'
020700090617     C                   SETON                                          96
020800090617     c                   goto      endcontr
020900090617     C                   endif
021000090617     c                   move      g02dat        vdatal
021100090617     c                   move      g02inv        vdatalg           8 0
021200090617     c                   endif
021300090617     c                   if        vdatdalg > vdatalg
021400090617     c                   eval      $msg = 'Incongruenza date periodo'
021500090617     c                   seton                                        96
021600090617     c                   goto      endcontr
021700090617     c                   endif
021800090617     c                   move      vdatdalg      iniper
021900090617     c                   move      vdatalg       finper
022000090617     c                   endif
022100090604     c
022200051122     c     endcontr      endsr
022300011026     c*-------------------------------------------------
022400011026     c     *inzsr        begsr
022500011026     c*-------------------------------------------------
022600020304     c     *entry        plist
022700020304     c                   parm                    kpjba
022800050801     c
022900020218     C                   time                    w0120            14 0
023000020218     C                   move      w0120         wdat              8 0
023100090617     C                   movel     w0120         wora              6 0
023200090616      *
023300090616     c     ktrm          klist
023400090616     c                   kfld                    vanno
023500090616     c                   kfld                    vmese
023600090616      *
023700030630      *profilo di filiale estraggo solo quelli relativi al P.O.
023800030630     C                   CLEAR                   Tibs36Ds
023900030630     C                   EVAL      I36ute = knmus
024000030630     C                   CALL      'TIBS36R'
024100030630     C                   PARM                    tibs36ds
024200090617     c                   if        %subst(knmus:1:3) = 'EDP'
024300090617     c                   seton                                        25
024400090617     c                   endif
024500080502      *
024600011026     c                   endsr
024700090616     C*--------------------------------------------------------------------
024800090616     OSYSPRT    E            testata        1 02
024900090617     o                       knsif               10
025000090617     o                       knmus               22
025100090616     o                                        +   1 'Report contratti allegati -
025200090616     o                                              autisti attività del mese'
025300090616     o                       vmese            +   1
025400090616     o                                        +   1 '/'
025500090616     o                       vanno            +   1
025600090617     o                                        +   1 'TRMZ87R'
025700090617     o                       wora             +   2 '  :  :  '
025800090617     o                       udate         y  +   1
025900090616     O                       page          z  +   2
026000090617     O          E    25      testata     1
026100090617     o                                           50 'Periodo dal'
026200090617     o                       vdatdal           +  2 '  /  /    '
026300090617     o                                         +  1 'al'
026400090617     o                       vdatal            +  2 '  /  /    '
026500090617     O          E            testata     1
026600090617     o                                           12 'CONTRATTI'
026700090617     o                                           37 'ALLEGATI CITTA'
026800090617     o                                           63 'ALLEGATI AF/DE'
026900090617     o                                           95 'AUTISTI CITTÀ'
027000090617     o                                          122 'AUTISTI AF/DE'
027100090617     O          E            testata     0
027200090617     o                                           12 'CONTRATTI'
027300090617     o                                           37 'ALLEGATI CITTA'
027400090617     o                                           63 'ALLEGATI AF/DE'
027500090617     o                                           95 'AUTISTI CITTÀ'
027600090617     o                                          122 'AUTISTI AF/DE'
027700090617     O          E            testata     0
027800090617     o                                           12 'CONTRATTI'
027900090617     o                                           37 'ALLEGATI CITTA'
028000090617     o                                           63 'ALLEGATI AF/DE'
028100090617     o                                           95 'AUTISTI CITTÀ'
028200090617     o                                          122 'AUTISTI AF/DE'
028300090617     O          E            testata     2
028400090616     o                                            8 'Stampati'
028500090616     o                                           18 'Rientrati'
028600090616     o                                           30 'Stampati'
028700090617     o                                           40 'Rientrati'
028800090617     o                                           55 'Stampati'
028900090617     o                                           67 'Rientrati'
029000090617     o                                           90 'Accreditati'
029100090617     o                                          101 'Disaccred.'
029200090617     o                                          115 'Accreditati'
029300090617     o                                          127 'Disaccred.'
029400090616     O          E            testata     1
029500090616     o                       T87con        z      7
029600090616     o                       T87conr       z     16
029700090616     o                       T87all        z     28
029800090617     o                       T87allr       z     38
029900090617     o                       T87allad      z     53
030000090617     o                       T87alladr     z     65
030100090617     o                       T87autc       z     88
030200090617     o                       T87autcd      z     99
030300090617     o                       T87auta       z    113
030400090617     o                       T87autad      z    125
