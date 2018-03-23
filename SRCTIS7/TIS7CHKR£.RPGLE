000100100204     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*--------------------------------------------------------------*
000300170802     Ftnvr334d  CF   E             WORKSTN
000301170802     fazorg01l  if   e           k disk
000302170802     ftntbe01l  if   e           k disk
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
002902170802     d  fila                          3s 0
002903170616     D  vdatdalg                      8s 0
002904170616     D  vdatalg                       8s 0
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
004800170802     c                   call      'TNVR334R1'
004900170616     c                   parm                    kpjba
004901170619     c                   leave
006100050623     c                   endif
006101170616     c                   enddo
008400050802
008500020308     c                   seton                                        lr
031600031105     c*-------------------------------------------------
031700031105     c     controv1      begsr
031800031105     c*-------------------------------------------------
031900170802     c                   setoff                                       707172
031901170802     c                   setoff                                       737475
031902170802     c                   setoff                                           96
031903170802      * prima  filiale
031904170802     c                   clear                   vfildad
031905170802     c                   if        vfilda  = 0
031906170802     C                   seton                                        7096
031907170802     c                   leavesr
031908170802     C                   else
031909170802     c     vfilda        chain     azorg01l                           70
031910170802     C   70              seton                                          96
031911170802     c   96              leavesr
031912170802     c                   movel     orgdes        vfildad
031913170802     C                   endif
031914170802      * seconda filiale
031915170802     c                   clear                   vfilad
031916170802     c                   if        vfila  = 0
031917170802     C                   seton                                        7196
031918170802     c                   leavesr
031919170802     C                   else
031920170802     c     vfila         chain     azorg01l                           71
031921170802     C   71              seton                                          96
031922170802     c   96              leavesr
031923170802     c                   movel     orgdes        vfilad
031924170802     C                   endif
031925170802      *verifica forzatura se presente in tabella NON ATTIVO
031926170802     c                   movel     vfilda        tbeke1
031927170802     c                   movel     vfila         tbeke2
031928170802     c     ktbe          chain     tntbe01l
031929170802     c                   if        not %found(tntbe01l)
031930170802     c*m                 seton                                        74
031931170802     c*m                 leavesr
031932170802     c                   endif
031933170802      *
031934170616     c                   if        vdatdal = 0
031935170802     C                   seton                                        7296
031936170616     c                   goto      endcontr
031937170616     C                   endif
031938170802      *
031939170616     c                   if        vdatdal > 0
031940170616     C                   MOVE      Vdatdal       G02DAT
031941170616     C                   MOVEL     *BLANK        G02ERR
031942170616     C                   CALL      'XSRDA8'
031943170616     C                   PARM                    WLBDA8
031944170616     C     G02ERR        IFEQ      '1'
031945170802     C                   SETON                                        7296
031946170616     c                   goto      endcontr
031947170616     C                   endif
031948170616     c                   move      g02dat        vdatdal
031949170616     c                   move      g02inv        vdatdalg
031950170616     C                   endif
031951170616      *seconda data
032100090604     c                   if        vdatal = 0
032200170802     C                   seton                                        7396
032300090604     c                   goto      endcontr
032400090604     C                   endif
032500090604     c                   if        vdatal > 0
032600090604     C                   MOVE      Vdatal        G02DAT
032700090604     C                   MOVEL     *BLANK        G02ERR
032800090604     C                   CALL      'XSRDA8'
032900090604     C                   PARM                    WLBDA8
033000090604     C     G02ERR        IFEQ      '1'
033100170802     C                   SETON                                        7396
033200090604     c                   goto      endcontr
033300090604     C                   endif
033400090604     c                   move      g02dat        vdatal
033500170616     c                   move      g02inv        vdatalg
033501170616     C                   endif
034900170616     c                   if        vdatalg < vdatdalg
035000170802     c                   seton                                        7596
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
038300130128     C                   move      wdat          dataeur
038400170616     c                   move      dataeur       dataiso
038700170616     c                   move      dataeur       vdatdal
038701170616     c                   move      dataeur       vdatal
041200080502      *
041201170802      *
041202170802     C     Ktbe          KLIST
041203170802     C                   KFLD                    TBECOD
041204170802     C                   KFLD                    TBEKE1
041205170802     C                   kfld                    TBEKE2
041206170802     c                   move      'FLA'         tbecod
041300011026     c                   endsr
041400050802      *---------------------------------------------------------
