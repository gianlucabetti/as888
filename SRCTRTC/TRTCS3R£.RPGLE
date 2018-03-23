000100010221     H DECEDIT('0,') DATEDIT(*DMY.)
000200020808
000300010221     Ffnba100f  O  A E           K DISK
000400070123     Ffnbrv02l  IF   E           K DISK
000500070123     Ffnbrv01l  IF   E           K DISK
000600070123     F                                     RENAME(Fnbrv000:Fnbrv001)
000700010221     Ffnfgv01l  IF   E           K DISK
000800020808
000900010221     D KPJBA         E DS
001000010221     D trtcs2ds      E DS
001100010221     D***
001200010221     D WLBDAT          DS                  INZ
001300010221     D  G02DAT                 1      8  0
001400010221     D  G02INV                 9     16  0
001500010221     D  G02ERR                17     17
001600010221     D  G02TGI                18     22  0
001700020808     D***
001800020808     D w0140           S             14  0 inz
001900010221     c*
002000010221     c* per sicurezza controllo che il foglio di defluenza sia presente
002100010221     c* e sia aperto
002200020808     ***c                   z-add     ds2nfd        w0050             5 0
002300020808     ***c     w0050         chain     fnfgv01l                           30
002400020808     c                   z-add     DS2nfd        FGVnfv
002500020808     c                   z-add     DS2fgd        FGVlnp
002600020808     c     K02FGV01      chain     FNFGV01L                           30
002700010221     c     *in30         ifeq      *on
002800010221     c     fgvfcf        orne      *blanks
002900010221     c                   goto      fine
003000010221     c                   endif
003100010221     c
003200070123     C     kbrv          setll     fnbrv02l
003300070123     C     kbrv          reade     fnbrv02l                               30
003400010221     C     *in30         doweq     *off
003500070123     c     kbrv1         setll     fnbrv01l                               31
003600010221     c     *in31         ifeq      *off
003700010221     c* autogenero spunta solo se non esiste
003800010221     c                   z-add     brvlnp        barlnp
003900010221     c                   z-add     brvlna        barlna
004000010221     c                   z-add     brvnrs        barnrs
004100010221     c                   z-add     brvnsc        barnsc
004200010221     c                   z-add     brvznc        barznc
004300010221     c                   z-add     ds2nfd        barnfv
004400010222     c                   z-add     88            barnps
004500010221     c                   z-add     ds2npd        barnpg
004600020808     C                   TIME                    W0140
004700010221     C                   MOVEL     W0140         barhms
004800010221     C                   CLEAR                   WLBDAT
004900010221     C                   MOVE      W0140         G02DAT
005000010221     C                   CALL      'XSRDA8'
005100010221     C                   PARM                    WLBDAT
005200010221     C                   Z-ADD     G02INV        bardfs
005300010221     c                   write     barco
005400010221     c                   endif
005500070123     C     kbrv          reade     fnbrv02l                               30
005600010221     C                   enddo
005700010221     c                   feod      fnba100f
005800010221     c* richiamo pgm di caricamento spunte
005900010221     c                   call      'FNLS44C5'
006000010221     c                   parm      'FNBA100F'    parfile          10
006100010221     c
006200010221     c     fine          tag
006300020808     c                   eval      *inlr = *on
006400010221     c*
006500010221     c     *inzsr        begsr
006600010221     c     kbrv          klist
006700010221     c                   kfld                    ds2npg
006800010221     c                   kfld                    ds2nfv
006900010221     c                   kfld                    ds2fgs
007000010221     c                   kfld                    ds2lna
007100010221     c     kbrv1         klist
007200010221     c                   kfld                    ds2npd
007300010221     c                   kfld                    ds2nfd
007400010221     c                   kfld                    ds2fgd
007500010221     c                   kfld                    brvlnp
007600010221     c                   kfld                    brvlna
007700010221     c                   kfld                    brvnrs
007800010221     c                   kfld                    brvnsc
007900020808     c     K02FGV01      klist
008000020808     c                   kfld                    FGVnfv
008100020808     c                   kfld                    FGVlnp
008200010221     c*
008300010221     C     *ENTRY        PLIST
008400010221     C                   PARM                    KPJBA
008500010221     C                   MOVEL     KPJBU         trtcs2ds
008600010221     c                   endsr
