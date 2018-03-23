000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400100205     Ftrmz89d   CF   E             WORKSTN
000500090518     Faitrs01l  if   e           k disk
000600090518     Faitra01l  if   e           k disk
000800100205     Ftrmz890f  uf a e           k disk    usropn
001000020304     dkpjba          e ds
001400070605     d dataiso         s               d   datfmt(*iso)
001500070605     d dataeur         s               d   datfmt(*eur)
001501100205     dtibs36ds       e ds
001600070605     D WLBDA8          DS
001700070605     D  G02DAT                 1      8  0
001800070605     D  G02INV                 9     16  0
001900070605     D  G02ERR                17     17
002000070605     D  G02TGI                18     22  0
004200080502      *
004300090519     d coman           s             99
004400090519     d luncmd          s             15  5
004500090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
004600090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
004700090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
004800090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
004900090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
005000000000     C*---------------------------------------------------------------*
005100020304      *
005200031105     c                   do        *hival
005300031105     c                   exfmt     video1
005400031105     c                   clear                   $msg
005500031105     c   kc              leave
005600031105     c                   exsr      controv1
005700031105     c   96              iter
005800050623     c                   if        *inkf
005900050623     c                   exsr      elabora
006000050805     c                   leave
006100050623     c                   endif
006200050805     c                   enddo
006300050802
006900090518      *
007000090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
007100090519     c                   movea     cmd4          coman
007200090519     c                   exsr      comandi
007300090519      *esegue crtdupobj da qtemp a wrkquery
007400090519     c                   movea     cmd5          coman
007500090519     c                   exsr      comandi
007600090519      *elimina file in qtemp x la filiale
007700100205     c                   close     trmz890f
007800090519     c                   movea     cmd3          coman
007900090519     c                   exsr      comandi
008000020308     c                   seton                                        lr
008100050802     c*-------------------------------------------------
008200090518     c     elabora       begsr
008300050802     c*-------------------------------------------------
008400090518     c     *loval        setll     aitrs01l
008500070530     c                   do        *hival
008600090518     c                   read      aitrs01l                               98
008700070530     c   98              leave
008800100205     c                   clear                   trmz8900
008900090518      * filtro se cerca società in forza alla data
009100100205     c                   if        trsdfc > 0
009200070530     c                   iter
009300070530     c                   endif
012300090518      * ragione sociale società
012500090924     c     trsnrc        setll     aitra01l
012600090924     c                   do        *hival
012700090924     c     trsnrc        reade     aitra01l
012800090924     c                   if        %eof(aitra01l)
012900090924     c                   leave
013000090924     c                   endif
013200100224     c                   if        tradfi > 0 or
013201100205     c                             trakau = 0 or tradin > vdatalg
013202100205     c                   iter
013500090518     c                   endif
013501100205     c     k890          chain     trmz890f
013502100205     c                   if        %found(trmz890f)
013503100205     c                   if        tradin < t89din
013504100205     c                   z-add     trafil        t89fil
013505100205     c                   z-add     tradin        t89din
013506100205     c                   update    trmz8900
013507100205     c                   else
013508100205     c                   iter
013509100205     c                   endif
013510100205     c                   else
013511100205     c                   movel     trasoc        t89soc
013512100205     c                   movel     traaut        t89aut
013513100205     c                   movel     trsiva        t89iva
013514100205     c                   movel     knmus         t89usr
013515100205     c                   z-add     tranrc        t89nrc
013516100205     c                   z-add     tradin        t89din
013517100205     c                   z-add     trafil        t89fil
013518111212     c                   move      traspi        t89spi
013519111214     c                   move      trafur        t89fur
013520111214     c                   move      traris        t89ris
013521100205     c                   z-add     trakau        t89kau
013522100208     c                   move      dataiso       t89ela
013523100205     c                   write     trmz8900
013524100205     c                   endif
013600090924     c                   enddo
017800090518     c                   enddo
017900070530      *
018000070530     c                   endsr
018100031105     c*-------------------------------------------------
018200031105     c     controv1      begsr
018300031105     c*-------------------------------------------------
018400070606     c                   setoff                                       9697
018500070605      *data disaccr. per turnover
019100090518     c                   if        vdatal > 0
019200090518     C                   MOVE      Vdatal        G02DAT
019300070605     C                   MOVEL     *BLANK        G02ERR
019400070605     C                   CALL      'XSRDA8'
019500070605     C                   PARM                    WLBDA8
019600070605     C     G02ERR        IFEQ      '1'
019700070605     C                   SETON                                        4295
019800070605     c                   goto      endcontr
019900070605     C                   END
020000090518     c                   move      g02dat        vdatal
020100090518     c                   move      g02inv        vdatalg           8 0
020400070529     c                   endif
020500051121
021800051122     c     endcontr      endsr
021900090519     c*-------------------------------------------------
022000090519     c     comandi       begsr
022100090519     c*-------------------------------------------------
022200090519      *
022300090519     C                   z-add     99            luncmd
022400090519     C                   CALL      'QCMDEXC'                            34
022500090519     C                   parm                    coman
022600090519     C                   parm                    luncmd
022700090519     c                   endsr
022800011026     c*-------------------------------------------------
022900011026     c     *inzsr        begsr
023000011026     c*-------------------------------------------------
023100020304     c     *entry        plist
023200020304     c                   parm                    kpjba
023300050801
023400100205     c     k890          klist
023500110128     c                   kfld                    trsIVA
023600100205     c                   kfld                    traaut
024000050801     c
024100050801
024200020218     C                   time                    w0120            14 0
024201100208     C                   move      w0120         wdat              8 0
024202100208     C                   move      wdat          dataeur
024203100208     C                   move      dataeur       dataiso
024400030630     C                   CLEAR                   Tibs36Ds
024500030630     C                   EVAL      I36ute = knmus
024600030630     C                   CALL      'TIBS36R'
024700030630     C                   PARM                    tibs36ds
025400090519      *clear file
025500090519     c                   movea     cmd1          coman
025600090519     c                   exsr      comandi
025700090519     c                   movea     cmd2          coman
025800090519     c                   exsr      comandi
025900100205     c                   open      trmz890f
026000090519
026100090518     c                   endsr
026200090519**  CMD1
026300100205CLRPFM    FILE(trmz890f)
026400090519**  CMD2
026500100205CRTDUPOBJ OBJ(trmz890F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
026600090519**  CMD3
026700100205DLTF FILE(QTEMP/trmz890F)
026800090519**  CMD4
026900100205dltf file(WRKQUERY/trmz890f)
027000090519**  CMD5
027100100205CRTDUPOBJ OBJ(trmz890F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz890f) DATA(*YES)
