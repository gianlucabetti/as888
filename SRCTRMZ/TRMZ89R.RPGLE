000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400100205     Ftrmz89d   CF   E             WORKSTN
000500090518     Faitrs01l  if   e           k disk
000600090518     Faitra01l  if   e           k disk
000700100205     Ftrmz890f  uf a e           k disk    usropn
000800020304     dkpjba          e ds
000900070605     d dataiso         s               d   datfmt(*iso)
001000070605     d dataeur         s               d   datfmt(*eur)
001100100205     dtibs36ds       e ds
001200070605     D WLBDA8          DS
001300070605     D  G02DAT                 1      8  0
001400070605     D  G02INV                 9     16  0
001500070605     D  G02ERR                17     17
001600070605     D  G02TGI                18     22  0
001700080502      *
001800090519     d coman           s             99
001900090519     d luncmd          s             15  5
002000090519     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
002100090519     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
002200090519     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
002300090519     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
002400090519     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
002500000000     C*---------------------------------------------------------------*
002600020304      *
002700031105     c                   do        *hival
002800031105     c                   exfmt     video1
002900031105     c                   clear                   $msg
003000031105     c   kc              leave
003100031105     c                   exsr      controv1
003200031105     c   96              iter
003300050623     c                   if        *inkf
003400050623     c                   exsr      elabora
003500050805     c                   leave
003600050623     c                   endif
003700050805     c                   enddo
003800050802
003900090518      *
004000090519      *verifica se esiste file in libreria wrkquery se c'è lo elimina
004100090519     c                   movea     cmd4          coman
004200090519     c                   exsr      comandi
004300090519      *esegue crtdupobj da qtemp a wrkquery
004400090519     c                   movea     cmd5          coman
004500090519     c                   exsr      comandi
004600090519      *elimina file in qtemp x la filiale
004700100205     c                   close     trmz890f
004800090519     c                   movea     cmd3          coman
004900090519     c                   exsr      comandi
005000020308     c                   seton                                        lr
005100050802     c*-------------------------------------------------
005200090518     c     elabora       begsr
005300050802     c*-------------------------------------------------
005400090518     c     *loval        setll     aitrs01l
005500070530     c                   do        *hival
005600090518     c                   read      aitrs01l                               98
005700070530     c   98              leave
005800100205     c                   clear                   trmz8900
005900090518      * filtro se cerca società in forza alla data
006000100205     c                   if        trsdfc > 0
006100070530     c                   iter
006200070530     c                   endif
006300090518      * ragione sociale società
006400090924     c     trsnrc        setll     aitra01l
006500090924     c                   do        *hival
006600090924     c     trsnrc        reade     aitra01l
006700090924     c                   if        %eof(aitra01l)
006800090924     c                   leave
006900090924     c                   endif
007000100224     c                   if        tradfi > 0 or
007100100205     c                             trakau = 0 or tradin > vdatalg
007200100205     c                   iter
007300090518     c                   endif
007400100205     c     k890          chain     trmz890f
007500100205     c                   if        %found(trmz890f)
007600100205     c                   if        tradin < t89din
007700100205     c                   z-add     trafil        t89fil
007800100205     c                   z-add     tradin        t89din
007900100205     c                   update    trmz8900
008000100205     c                   else
008100100205     c                   iter
008200100205     c                   endif
008300100205     c                   else
008400100205     c                   movel     trasoc        t89soc
008500100205     c                   movel     traaut        t89aut
008600100205     c                   movel     trsiva        t89iva
008700100205     c                   movel     knmus         t89usr
008800100205     c                   z-add     tranrc        t89nrc
008900100205     c                   z-add     tradin        t89din
009000100205     c                   z-add     trafil        t89fil
009100100205     c                   z-add     trakau        t89kau
009200100208     c                   move      dataiso       t89ela
009300100205     c                   write     trmz8900
009400100205     c                   endif
009500090924     c                   enddo
009600090518     c                   enddo
009700070530      *
009800070530     c                   endsr
009900031105     c*-------------------------------------------------
010000031105     c     controv1      begsr
010100031105     c*-------------------------------------------------
010200070606     c                   setoff                                       9697
010300070605      *data disaccr. per turnover
010400090518     c                   if        vdatal > 0
010500090518     C                   MOVE      Vdatal        G02DAT
010600070605     C                   MOVEL     *BLANK        G02ERR
010700070605     C                   CALL      'XSRDA8'
010800070605     C                   PARM                    WLBDA8
010900070605     C     G02ERR        IFEQ      '1'
011000070605     C                   SETON                                        4295
011100070605     c                   goto      endcontr
011200070605     C                   END
011300090518     c                   move      g02dat        vdatal
011400090518     c                   move      g02inv        vdatalg           8 0
011500070529     c                   endif
011600051121
011700051122     c     endcontr      endsr
011800090519     c*-------------------------------------------------
011900090519     c     comandi       begsr
012000090519     c*-------------------------------------------------
012100090519      *
012200090519     C                   z-add     99            luncmd
012300090519     C                   CALL      'QCMDEXC'                            34
012400090519     C                   parm                    coman
012500090519     C                   parm                    luncmd
012600090519     c                   endsr
012700011026     c*-------------------------------------------------
012800011026     c     *inzsr        begsr
012900011026     c*-------------------------------------------------
013000020304     c     *entry        plist
013100020304     c                   parm                    kpjba
013200050801
013300100205     c     k890          klist
013400110128     c                   kfld                    trsIVA
013500100205     c                   kfld                    traaut
013600050801     c
013700050801
013800020218     C                   time                    w0120            14 0
013900100208     C                   move      w0120         wdat              8 0
014000100208     C                   move      wdat          dataeur
014100100208     C                   move      dataeur       dataiso
014200030630     C                   CLEAR                   Tibs36Ds
014300030630     C                   EVAL      I36ute = knmus
014400030630     C                   CALL      'TIBS36R'
014500030630     C                   PARM                    tibs36ds
014600090519      *clear file
014700090519     c                   movea     cmd1          coman
014800090519     c                   exsr      comandi
014900090519     c                   movea     cmd2          coman
015000090519     c                   exsr      comandi
015100100205     c                   open      trmz890f
015200090519
015300090518     c                   endsr
015400090519**  CMD1
015500100205CLRPFM    FILE(trmz890f)
015600090519**  CMD2
015700100205CRTDUPOBJ OBJ(trmz890F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
015800090519**  CMD3
015900100205DLTF FILE(QTEMP/trmz890F)
016000090519**  CMD4
016100100205dltf file(WRKQUERY/trmz890f)
016200090519**  CMD5
016300100205CRTDUPOBJ OBJ(trmz890F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(trmz890f) DATA(*YES)
