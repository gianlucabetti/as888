000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300050329     FFNblp01L  uF   E           K DISK
000400050323     FFNblt01L  iF   E           K DISK
000500050329     FFibrv07L  iF   E           K DISK
000600050323     Ffnarbwrk  o  a E             DISK    rename(fnarb000:fnarb00w)
000700021214     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000800021214      *--------------------------------------------------
000900050323     c*
001000050329     c                   z-add     2005          kaaa              4 0
001100050329     C     kaaa          setll     fnblp01l
001200050329     C                   read      fnblp01l
001300050323    1c                   DOW       not %EOF(FNblp01l)
001400050329     c                   if        blpnpi=24 and blpaas=2005 and blpmgs>0215 and
001500050329     c                             blplnp<>180  and blplnp<>115
001600050323     c
001700050323     c     karb          setll     fnblt01l
001800050323     c     karb          reade     fnblt01l
001900050323     c
002000050323     c                   clear                   wesci             1
002100050323    5c                   DOW       not %EOF(FNblt01L) and wesci=' '
002200050323     c                   if        bltdfv>0 and bltdfv>bltdse
002300050323     c                   if        bltnpp=87 or bltnpp=96
002400050329     c                   if        (bltdfv=bltdet) or (bltdfv=bltdam)
002500050329     c* Se ci sono spunte ignoro la bolla ma esco
002600050329     c     kbrv          setll     fibrv07l                               10
002700050329     c                   if        *in10
002800050329     c                   eval      wesci='S'
002900050329     c                   else
003000050323     c                   add       1             cont              7 0
003100050329     c                   except    bolla
003200050323     c                   eval      wesci='S'
003300050329     c                   clear                   blpnpi
003400050329     c                   except    aggblp
003500050329     c*
003600050329     c                   eval      arbaas=blpaas
003700050329     c                   eval      arblnp=blplnp
003800050329     c                   eval      arbnrs=blpnrs
003900050329     c                   eval      arbnsp=blpnsp
004000050329     c                   write     fnarb00w
004100050323     c                   endif
004200050329     c                   endif
004300050329     c                   endif
004400050323     c                   endif
004500050323    6c
004600050323     c     karb          reade     fnblt01l
004700050322    5c                   enddo
004800050323     c                   endif
004900050323     c                   read      fnblp01l
005000050323    5c                   enddo
005100050322     c
005200050323     c                   except    testa
005300021216      *
005400021214     C                   eval       *inlr = *on
005500021214      *---------------------------------------------------------
005600021214     c     *inzsr        BEGSR
005700021214      *
005800050118     C     Karb          KLIST
005900050323     C                   KFLD                    blpaas
006000050323     C                   KFLD                    blplnp
006100050323     C                   KFLD                    blpnrs
006200050323     C                   KFLD                    blpnsp
006300050329     C     Kbrv          KLIST
006400050329     C                   KFLD                    bltfls
006500050329     C                   KFLD                    bltlna
006600050329     C                   KFLD                    bltnrs
006700050329     C                   KFLD                    bltnsc
006800021216     C*
006900021214     C                   ENDSR
007000021214      *---------------------------------------------------------
007100021214     OQSYSPRT   E            TESTA          1 02
007200021214     O                       *DATE            +   5 '  /  /    '
007300050322     O                                        +   5 'Bolle per cui aggiornare  '
007400050322     O                                        +   0 'ARBNPI=0'
007500050322     O                                        +   7 'TNVRAANM050'
007600050323     O                       cont          2  +   5
007700050329     O          E            bolla       1
007800050329     O                                              'SPED.'
007900050329     O                       blpAAS            +  2
008000050329     O                       blpLNP            +  1
008100050329     O                       blpNRS            +  1
008200050329     O                       blpNSP        Z   +  1
008300050329     O                       blplna            +  1
008400050329     O                       blpmgs            +  1 '  /  '
008500050329     O                                         +  2 'BLTdse'
008600050329     O                       bltdse            +  1 '    /  /  '
008700050329     O                       bltnpe            +  1
008800050329     O                                         +  2 'BLTdfv'
008900050329     O                       BltDFV            +  1 '    /  /  '
009000050329     O                       bltnpp            +  1
009100050329     Ofnblp000  E            aggblp
009200050329     O                       blpnpi
