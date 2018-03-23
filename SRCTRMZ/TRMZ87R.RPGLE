000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400090615     Ftrmz87d   CF   E             WORKSTN
000500090615     Faitra01l  if   e           k disk
000600090615     Faitrs01l  if   e           k disk
000700090615     Ffiadt01l  if   e           k disk
000800090615     Ffitgt01l  if   e           k disk
000900090909     Ftrmz870f  uf a e           k disk    usropn
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
002100090909     d coman           s             99
002200090909     d luncmd          s             15  5
002300070605     D WLBDA8          DS
002400070605     D  G02DAT                 1      8  0
002500070605     D  G02INV                 9     16  0
002600070605     D  G02ERR                17     17
002700070605     D  G02TGI                18     22  0
002800090909     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002900090909     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
003000080502      *
003100000000     C*---------------------------------------------------------------*
003200020304      *
003300031105     c                   do        *hival
003400031105     c                   exfmt     video1
003500031105     c                   clear                   $msg
003600031105     c   kc              leave
003700031105     c                   exsr      controv1
003800031105     c   96              iter
003900090616     c     ktrm          chain     trmz870f
004000090616     c                   if        %found(trmz870f)
004100090616     c                   eval      $msg = 'Mese già estratto '
004200090616     c                   iter
004300090616     c                   endif
004400050623     c                   if        *inkf
004500090604
004600090616     c                   clear                   trmz8700
004700050623     c                   exsr      elabora
004800050805     c                   leave
004900050623     c                   endif
005000050805     c                   enddo
005100090909     c                   close     trmz870f
005200090909     c                   movea     cmd1          coman
005300090909     c                   exsr      comandi
005400090909     c                   movea     cmd2          coman
005500090909     c                   exsr      comandi
005600050802
005700020308     c                   seton                                        lr
005800990113     c*-------------------------------------------------
005900090604     c     elabora       begsr
006000990113     c*-------------------------------------------------
006100090616      * ?contratti
006200090615     c     *loval        setll     aitrs01l
006300981029     c                   do        *hival
006400090615     c                   read      aitrs01l
006500090615     c                   if        %eof(aitrs01l)
006600090604     c                   leave
006700090604     c                   endif
006800090616     c                   if        trsann <> *blank
006900090616     c                   iter
007000090616     c                   endif
007100090616      * contratti stampati
007200090616     c                   if        trsdsc >= iniper  and
007300090616     c                             trsdsc <= finper
007400090616     c                   add       1             t87con
007500090616     c                   endif
007600090616      * contratti rientrati firmati
007700090616     c                   if        trsdrc >= iniper  and
007800090616     c                             trsdrc <= finper
007900090616     c                   add       1             t87conr
008000090616     c                   endif
008100050623     c                   enddo
008200090616      * ?autisti
008300090615     c     *loval        setll     aitra01l
008400090615     c                   do        *hival
008500090615     c                   read      aitra01l
008600090615     c                   if        %eof(aitra01l)
008700090615     c                   leave
008800090615     c                   endif
008900090616     c                   if        traann <> *blank
009000090616     c                   iter
009100090616     c                   endif
009200090616      * autisti accreditati città e aff/def
009300090616     c                   if        tradin >= iniper  and
009400090616     c                             tradin <= finper
009500090616     c                   if        trakau > 0
009600090616     c                   add       1             t87autc
009700090616     c                   endif
009800090616     c                   if        trabpt > 0
009900090616     c                   add       1             t87auta
010000090616     c                   endif
010100090616     c                   endif
010200090616      * autisti disaccreditati città e aff/def
010300090616     c                   if        tradfi >= iniper  and
010400090616     c                             tradfi <= finper
010500090616     c                   if        trakau > 0
010600090616     c                   add       1             t87autcd
010700090616     c                   endif
010800090616     c                   if        trabpt > 0
010900090616     c                   add       1             t87autad
011000090616     c                   endif
011100090616     c                   endif
011200090615     c                   enddo
011300090616      * ?tariffe città
011400090615     c     *loval        setll     fitgt01l
011500090615     c                   do        *hival
011600090615     c                   read      fitgt01l
011700090615     c                   if        %eof(fitgt01l)
011800090615     c                   leave
011900090615     c                   endif
012000090616     c                   if        tgtatb <> *blank
012100090616     c                   iter
012200090616     c                   endif
012300090616      * allegati stampati
012400090616     c                   if        tgtdcn >= iniper  and
012500090616     c                             tgtdcn <= finper
012600090616     c                   add       1             t87all
012700090616     c                   endif
012800090616      * allegati rientrati firmati
012900090616     c                   if        tgtdrc >= iniper  and
013000090616     c                             tgtdrc <= finper
013100090616     c                   add       1             t87allr
013200090616     c                   endif
013300090615     c                   enddo
013400090616      * ?tariffe affluenze defluenze
013500090615     c     *loval        setll     fiadt01l
013600090615     c                   do        *hival
013700090615     c                   read      fiadt01l
013800090615     c                   if        %eof(fiadt01l)
013900090615     c                   leave
014000090615     c                   endif
014100090616     c                   if        adtatb <> *blank
014200090616     c                   iter
014300090616     c                   endif
014400090616      * allegati stampati
014500090616     c                   if        adtdcn >= iniper  and
014600090616     c                             adtdcn <= finper
014700090616     c                   add       1             t87allad
014800090616     c                   endif
014900090616      * allegati rientrati firmati
015000090616     c                   if        adtdrc >= iniper  and
015100090616     c                             adtdrc <= finper
015200090616     c                   add       1             t87alladr
015300090616     c                   endif
015400090615     c                   enddo
015500090616     c                   move      vanno         t87anno
015600090616     c                   move      vmese         t87mese
015700090616     c                   move      wdatg         t87data
015800090617     c  n25              write     trmz8700
015900090616     c                   except    testata
016000020304
016100020304     c                   endsr
016200981029      *
016300031105     c*-------------------------------------------------
016400031105     c     controv1      begsr
016500031105     c*-------------------------------------------------
016600090604     c                   setoff                                       964241
016700051121
016800090617      * 25 off lancio per utenti
016900090617     c                   if        not *in25
017000090617     c                   if        vmese  = 0
017100090604     C                   seton                                        4196
017200090604     c                   goto      endcontr
017300090604     C                   endif
017400090617     c                   if        vanno = 0
017500090615     C                   seton                                        4296
017600090615     c                   goto      endcontr
017700090615     C                   endif
017800090615     c                   move      0101          mesgio
017900090615     c                   movel     vmese         mesgio            4 0
018000090615     c                   movel     vanno         iniper
018100090615     c                   move      mesgio        iniper            8 0
018200090615     c                   move      iniper        dataiso
018300090615     c     dataiso       adddur    1:*m          dataiso
018400090616     c     dataiso       subdur    1:*d          dataiso
018500090616     c                   move      dataiso       finper            8 0
018600090615     c                   move      wdat          dataeur
018700090615     c                   move      dataeur       dataiso
018800090615     c                   move      dataiso       wdatg             8 0
018900090617      * 25 on
019000090617      * lancio per edp senza generazione file x periodo
019100090617     c                   else
019200090617     c                   if        vdatal = 0 or vdatdal = 0
019300090617     c                   eval      $msg = 'Inserire entrambe le date'
019400090617     c                   seton                                        96
019500090617     c                   goto      endcontr
019600090617     c                   endif
019700090617      *
019800090617     c                   if        vdatdal > 0
019900090617     C                   MOVE      Vdatdal       G02DAT
020000090617     C                   MOVEL     *BLANK        G02ERR
020100090617     C                   CALL      'XSRDA8'
020200090617     C                   PARM                    WLBDA8
020300090617     C     G02ERR        IFEQ      '1'
020400090617     C                   SETON                                          96
020500090617     c                   goto      endcontr
020600090617     C                   endif
020700090617     c                   move      g02dat        vdatdal
020800090617     c                   move      g02inv        vdatdalg          8 0
020900090617     c                   endif
021000090617     c                   if        vdatal > 0
021100090617     C                   MOVE      Vdatal        G02DAT
021200090617     C                   MOVEL     *BLANK        G02ERR
021300090617     C                   CALL      'XSRDA8'
021400090617     C                   PARM                    WLBDA8
021500090617     C     G02ERR        IFEQ      '1'
021600090617     C                   SETON                                          96
021700090617     c                   goto      endcontr
021800090617     C                   endif
021900090617     c                   move      g02dat        vdatal
022000090617     c                   move      g02inv        vdatalg           8 0
022100090617     c                   endif
022200090617     c                   if        vdatdalg > vdatalg
022300090617     c                   eval      $msg = 'Incongruenza date periodo'
022400090617     c                   seton                                        96
022500090617     c                   goto      endcontr
022600090617     c                   endif
022700090617     c                   move      vdatdalg      iniper
022800090617     c                   move      vdatalg       finper
022900090617     c                   endif
023000090604     c
023100051122     c     endcontr      endsr
023200090909     c*-------------------------------------------------
023300090909     c     comandi       begsr
023400090909     c*-------------------------------------------------
023500090909      *
023600090909     C                   z-add     99            luncmd
023700090909     C                   CALL      'QCMDEXC'                            34
023800090909     C                   parm                    coman
023900090909     C                   parm                    luncmd
024000090909      *
024100090909     c                   endsr
024200011026     c*-------------------------------------------------
024300011026     c     *inzsr        begsr
024400011026     c*-------------------------------------------------
024500020304     c     *entry        plist
024600020304     c                   parm                    kpjba
024700050801     c
024800020218     C                   time                    w0120            14 0
024900020218     C                   move      w0120         wdat              8 0
025000090617     C                   movel     w0120         wora              6 0
025100090616      *
025200090616     c     ktrm          klist
025300090616     c                   kfld                    vanno
025400090616     c                   kfld                    vmese
025500090909     c                   open      trmz870f
025600090616      *
025700030630      *profilo di filiale estraggo solo quelli relativi al P.O.
025800030630     C                   CLEAR                   Tibs36Ds
025900030630     C                   EVAL      I36ute = knmus
026000030630     C                   CALL      'TIBS36R'
026100030630     C                   PARM                    tibs36ds
026200090617     c                   if        %subst(knmus:1:3) = 'EDP'
026300090617     c                   seton                                        25
026400090617     c                   endif
026500080502      *
026600011026     c                   endsr
026700090616     C*--------------------------------------------------------------------
026800090616     OSYSPRT    E            testata        1 02
026900090617     o                       knsif               10
027000090617     o                       knmus               22
027100090616     o                                        +   1 'Report contratti allegati -
027200090616     o                                              autisti attività del mese'
027300090616     o                       vmese            +   1
027400090616     o                                        +   1 '/'
027500090616     o                       vanno            +   1
027600090617     o                                        +   1 'TRMZ87R'
027700090617     o                       wora             +   2 '  :  :  '
027800090617     o                       udate         y  +   1
027900090616     O                       page          z  +   2
028000090617     O          E    25      testata     1
028100090617     o                                           50 'Periodo dal'
028200090617     o                       vdatdal           +  2 '  /  /    '
028300090617     o                                         +  1 'al'
028400090617     o                       vdatal            +  2 '  /  /    '
028500090617     O          E            testata     1
028600090617     o                                           12 'CONTRATTI'
028700090617     o                                           37 'ALLEGATI CITTA'
028800090617     o                                           63 'ALLEGATI AF/DE'
028900090617     o                                           95 'AUTISTI CITTÀ'
029000090617     o                                          122 'AUTISTI AF/DE'
029100090617     O          E            testata     0
029200090617     o                                           12 'CONTRATTI'
029300090617     o                                           37 'ALLEGATI CITTA'
029400090617     o                                           63 'ALLEGATI AF/DE'
029500090617     o                                           95 'AUTISTI CITTÀ'
029600090617     o                                          122 'AUTISTI AF/DE'
029700090617     O          E            testata     0
029800090617     o                                           12 'CONTRATTI'
029900090617     o                                           37 'ALLEGATI CITTA'
030000090617     o                                           63 'ALLEGATI AF/DE'
030100090617     o                                           95 'AUTISTI CITTÀ'
030200090617     o                                          122 'AUTISTI AF/DE'
030300090617     O          E            testata     2
030400090616     o                                            8 'Stampati'
030500090616     o                                           18 'Rientrati'
030600090616     o                                           30 'Stampati'
030700090617     o                                           40 'Rientrati'
030800090617     o                                           55 'Stampati'
030900090617     o                                           67 'Rientrati'
031000090617     o                                           90 'Accreditati'
031100090617     o                                          101 'Disaccred.'
031200090617     o                                          115 'Accreditati'
031300090617     o                                          127 'Disaccred.'
031400090616     O          E            testata     1
031500090616     o                       T87con        z      7
031600090616     o                       T87conr       z     16
031700090616     o                       T87all        z     28
031800090617     o                       T87allr       z     38
031900090617     o                       T87allad      z     53
032000090617     o                       T87alladr     z     65
032100090617     o                       T87autc       z     88
032200090617     o                       T87autcd      z     99
032300090617     o                       T87auta       z    113
032400090617     o                       T87autad      z    125
032500090909**  CMD1
032600090909DLTF FILE(wrkquery/trmz870F)
032700090909**  CMD2
032800090909CRTDUPOBJ OBJ(trmz870f) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz870f) DATA(*YES)
