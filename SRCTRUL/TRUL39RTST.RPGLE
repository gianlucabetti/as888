000100060216     h decedit('0,') datedit(*dmy/)
000200030123
000300060216     fTRUL39DTSTcf   e             workstn
000400060216     f                                     infds(DSFMT)
000500030123
000600060216     d DSFMT           ds           512
000700060216     d  $TASTO               369    369
000800060216     d  NRG                  370    370
000900060216     d  NCL                  371    371
001000060216     d  SFLNRR               378    379B 0
001100060216      *
001200060216     d Status         sds
001300060216     d  SDSpgm           *proc
001400060216     d  SDSprm           *parms
001500060216      *
001600060216     d KPJBA         e ds
001700060216     d TRUL39ds      e ds                  inz
001800060216      *
001900060216      * Tasti di funzione
002000060216     d F01             c                   const(x'31')
002100060216     d F02             c                   const(x'32')
002200060216     d F03             c                   const(x'33')
002300060216     d F04             c                   const(x'34')
002400060216     d F05             c                   const(x'35')
002500060216     d F06             c                   const(x'36')
002600060216     d F07             c                   const(x'37')
002700060216     d F08             c                   const(x'38')
002800060216     d F09             c                   const(x'39')
002900060216     d F10             c                   const(x'3A')
003000060216     d F11             c                   const(x'3B')
003100060216     d F12             c                   const(x'3C')
003200060216     d F13             c                   const(x'B1')
003300060216     d F14             c                   const(x'B2')
003400060216     d F15             c                   const(x'B3')
003500060216     d F16             c                   const(x'B4')
003600060216     d F17             c                   const(x'B5')
003700060216     d F18             c                   const(x'B6')
003800060216     d F19             c                   const(x'B7')
003900060216     d F20             c                   const(x'B8')
004000060216     d F21             c                   const(x'B9')
004100060216     d F22             c                   const(x'BA')
004200060216     d F23             c                   const(x'BB')
004300060216     d F24             c                   const(x'BC')
004400060216     d ENTER           c                   const(x'F1')
004500060216     d ROLDWN          c                   const(x'F4')
004600060216     d ROLLUP          c                   const(x'F5')
004700030123
004800060216     c     *entry        plist
004900060216     c                   parm                    KPJBA
005000060216      *
005100060216     c                   clear                   KPJBU
005200060216     c                   clear                   TRUL39ds
005300060216      *
005400060217     c***                doU       $TASTO = F03 or $TASTO = F12
005500060217     c                   doU       $TASTO = F03
005600060216      *
005700060216     c                   write     TRUL39T1
005800060216     c                   write     TRUL39Z1
005900060216     c                   exfmt     TRUL39D1
006000060216      *
006100060217     c***                if        $TASTO <> F03   and
006200060217     c***                          $TASTO <> F12
006300060217     c                   if        $TASTO <> F03
006400060216     c                   exsr      CallPGM
006500060216     c                   write     TRUL39D1
006600060216     c                   write     TRUL39D2
006700060216     c                   exfmt     PROTECT
006800060216     c                   endIF
006900060216      *
007000060216     c                   endDO
007100060216      *
007200060216     c                   eval      *inLR = *on
007300060216      *
007400030123      ************************************************************
007500060216      * Richiamo pgm. TRUL39R
007600030123      ************************************************************
007700060216     c     CallPGM       BEGSR
007800060216      *
007900060216     c                   movel(p)  TRUL39ds      KPJBU
008000060216      *
008100060216     c                   call      'TRUL39R'                            99
008200060216     c                   parm                    KPJBA
008300060216      *
008400060216     c                   movel     KPJBU         TRUL39ds
008500060216      *
008600060216     c                   if        *in99
008700060216     c                   eval      O39err  = ''
008800060216     c                   eval      O39msg  = 'Errori nel richiamo del pgm.'
008900060216     c                   endif
009000060216      *
009100060216     c                   ENDSR
