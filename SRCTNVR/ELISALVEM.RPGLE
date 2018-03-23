000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200151106     FFNblp31L  IF   E           K DISK
000300151105     Fqsysprt   O    F  132        PRINTER OFLIND(*INOF)
000400070316     d
000500151105     D kpjba         E DS
000600151105     D fnlvemds      E DS
000700151105     d ds_fnblp      e ds                  extname(fnblp00f)
000800120117     d
000900120117     c
001000070316     c     *entry        plist
001100170119     c                   parm                    kpjba
001200170119     c
001300170119     c                   movel     kpjbu         lnpnsp           19
001400170119     c                   movel     lnpnsp        lnp               3
001500170119     c                   move      lnpnsp        nsp              16
001600170119     c                   if        nsp>*zeros
001700161121     c                   eval      blpaas=%int(%subst(nsp:1:4))
001800161121     c                   eval      blplnp=%int(%subst(nsp:5:3))
001900161121     c                   eval      blpnrs=%int(%subst(nsp:8:2))
002000161121     c                   eval      blpnsp=%int(%subst(nsp:10:7))
002100161121     c
002200161121     c                   exsr      chiama
002300161121     c                   else
002400161121     c
002500151106     c                   if        lnp>*zeros
002600151106     c                   movel     lnp           ktfp              3 0
002700151105     c                   else
002800151105     c                   z-add     001           ktfp
002900151105     c                   endif
003000070830     C
003100151105     c     kblp          klist
003200151105     c                   kfld                    ktfp
003300151106     c                   kfld                    kaas              4 0
003400151106     c                   kfld                    kmgs              4 0
003500151106
003600171103     c                   z-add     2017          kaas
003700171103     c                   z-add     0308          kmgs
003800151105
003900151106     c     kblp          setll     fnblp31l
004000151106     c     ktfp          reade     fnblp31l
004100151106    2c                   dow       not %eof(fnblp31l)
004200161121     c
004300161121     c                   exsr      chiama
004400151105     c
004500151106     c     ktfp          reade     fnblp31l
004600151105     c                   enddo
004700161121     c                   endif
004800161121     c
004900171103     c                   except    tot
005000151105     c                   seton                                        LR
005100161121     c
005200161121     c     chiama        BEGSR
005300161121     c                   clear                   fnlvemds
005400161121     c                   eval      iemaas=blpaas
005500161121     c                   eval      iemlnp=blplnp
005600161121     c                   eval      iemnrs=blpnrs
005700161121     c                   eval      iemnsp=blpnsp
005800161121     c                   movel     fnlvemds      kpjbu
005900161121     c
006000161121     c                   call      'FNLVEMR'
006100161121     c                   parm                    kpjba
006200161121     c                   parm                    ds_fnblp
006300161121     c
006400161121     c                   movel     kpjbu         fnlvemds
006500171103     c                   if        oemesito<>'1'
006600161121     c                   except    det
006700171103     c                   add       1             nook              9 0
006800171103     c                   else
006900171103     c                   add       1             ok                9 0
007000161121     c                   endif
007100161121     c                   ENDSR
007200151110     Oqsysprt   E            det         1
007300151105     o                                              'bolla'
007400151105     o                       blpaas        z  +   1
007500151105     o                       blplnp        z  +   1
007600151105     o                       blpnrs        z  +   1
007700151105     o                       blpnsp        z  +   1
007800151201     o                       blptsp           +   1
007900151201     o                       blpksc           +   1
008000170120     o                       blplna           +   1
008100170120     o                       blptfa           +   1
008200151105     o                                        +   2 'esito'
008300151105     o                       oemesito         +   1
008400171103     Oqsysprt   E            tot         1
008500171103     o                                              'Ok'
008600171103     o                       ok            z  +   1
008700171103     o                                        +   3 'no Ok'
008800171103     o                       nook          z  +   1
