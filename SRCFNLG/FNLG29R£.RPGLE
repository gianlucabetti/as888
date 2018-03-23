000100100204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300170616     Fgiacexd1  CF   E             WORKSTN
001000020304     dkpjba          e ds
002100070605     d dataiso         s               d   datfmt(*iso)
002200070605     d dataeur         s               d   datfmt(*eur)
002400070605     D WLBDA8          DS
002500070605     D  G02DAT                 1      8  0
002600070605     D  G02INV                 9     16  0
002700070605     D  G02ERR                17     17
002800070605     D  G02TGI                18     22  0
002900080502      *
002901170616     D param           DS
002902170616     D  vdatdalg                      8s 0
002903170616     D  vdatalg                       8s 0
003800000000     C*---------------------------------------------------------------*
003900020304      *
004000031105     c                   do        *hival
004100031105     c                   exfmt     video1
004200031105     c                   clear                   $msg
004300031105     c   kc              leave
004400031105     c                   exsr      controv1
004500031105     c   96              iter
004600050623     c                   if        *inkf
004700050802      *clear file
004701170616     c                   movel     param         kpjbu
004800170616     c                   call      'GIACEXTR'
004900170616     c                   parm                    kpjba
006100050623     c                   endif
006101170616     c                   enddo
008400050802
008500020308     c                   seton                                        lr
031600031105     c*-------------------------------------------------
031700031105     c     controv1      begsr
031800031105     c*-------------------------------------------------
031900170616     c                   setoff                                       414243
031901170616     c                   setoff                                       96
031902170616     c                   if        vdatdal = 0
031903170616     C                   seton                                        4296
031904170616     c                   goto      endcontr
031905170616     C                   endif
031906170616     c                   if        vdatdal > 0
031907170616     C                   MOVE      Vdatdal       G02DAT
031908170616     C                   MOVEL     *BLANK        G02ERR
031909170616     C                   CALL      'XSRDA8'
031910170616     C                   PARM                    WLBDA8
031911170616     C     G02ERR        IFEQ      '1'
031912170616     C                   SETON                                        4296
031913170616     c                   goto      endcontr
031914170616     C                   endif
031915170616     c                   move      g02dat        vdatdal
031916170616     c                   move      g02inv        vdatdalg
031917170616     C                   endif
031918170616      *seconda data
032100090604     c                   if        vdatal = 0
032200170616     C                   seton                                        4396
032300090604     c                   goto      endcontr
032400090604     C                   endif
032500090604     c                   if        vdatal > 0
032600090604     C                   MOVE      Vdatal        G02DAT
032700090604     C                   MOVEL     *BLANK        G02ERR
032800090604     C                   CALL      'XSRDA8'
032900090604     C                   PARM                    WLBDA8
033000090604     C     G02ERR        IFEQ      '1'
033100170616     C                   SETON                                        4396
033200090604     c                   goto      endcontr
033300090604     C                   endif
033400090604     c                   move      g02dat        vdatal
033500170616     c                   move      g02inv        vdatalg
033501170616     C                   endif
034900170616     c                   if        vdatalg < vdatdalg
035000170616     c                   seton                                        4196
035100130128     c                   endif
035200051122     c     endcontr      endsr
036300011026     c*-------------------------------------------------
036400011026     c     *inzsr        begsr
036500011026     c*-------------------------------------------------
036600020304     c     *entry        plist
036700020304     c                   parm                    kpjba
036800050801
038000020218     C                   time                    w0120            14 0
038100020218     C                   move      w0120         wdat              8 0
038200130128     C                   movel     01            wdat
038300130128     C                   move      wdat          dataeur
038400130128     c     dataeur       subdur    1:*d          dataiso
038500130128     c                   move      dataiso       ultgmesp          8 0
038600130128     c                   move      dataiso       dataeur
038700170616     c                   move      dataeur       vdatdal
038701170616     c                   move      dataeur       vdatal
041200080502      *
041300011026     c                   endsr
041400050802      *---------------------------------------------------------
