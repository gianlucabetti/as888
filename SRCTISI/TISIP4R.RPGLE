000100941109     H*------------------------------------------------------------------------*
000200060424     H* Creazione calendario eventi particolari per anno nuovo - automatismo   *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600060424     FAZcep02L  IF a E           K DISK
000700941109     D*-------------------
000800930210     D* DS PARAMETRI DI LANCIO PGM
000900941109     D*-------------------
001000970619     D PARAM           DS
001100060424     D  PARcepn                1      4  0
001200060424     D  PARcepo                5      8  0
001300060424     D KPJBA         E DS
001400060424     d
001500060421     d kdev            s                   Like(cepdev)
001600060424     d w0040           s                   Like(parcepn)
001700941109     C*------------------------------------------------------------------------*
001800060424     c     *entry        plist
001900060424     c                   parm                    kpjba
002000060424     c                   movel     kpjbu         param
002100060424     C*
002200060424     C* Elaboro per Data
002300060421     c                   z-add     00000101      kdev
002400060424     c                   movel     parcepo       kdev
002500060424     c                   eval      w0040=parcepo
002600060421     c
002700060424     c     kdev          setll     azcep02l
002800060421     c                   read      azcep02l
002900060424     c                   dow       not %eof(azcep02l) and w0040=parcepo
003000060424     c                   movel     cepdev        w0040
003100060424     c
003200060424     c                   if        w0040=parcepo  and cepdup='S'
003300060424     c* duplica per anno nuovo
003400060424     c                   movel     parcepn       cepdev
003500060424     c                   write     azcep000                             99
003600060424     c                   endif
003700060424     c
003800060424     c                   read      azcep02l
003900060424     c                   enddo
004000930128     C*
004100000000     C                   SETON                                        LR
