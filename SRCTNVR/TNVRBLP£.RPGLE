000100050118     H DECEDIT('0,') DATEDIT(*YMD.)
000200021214      *
000300130909     Ffnblp00f  UF   E             DISK    rename(fnblp000:fnblpf00)
000400130909     FFNblp01L  iF   E           K DISK    infds(fnblpinf)
000500091125     FFNblt01l  iF   E           K DISK
000600091125     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000700091125
000800130909     D fnblpinf        ds
000900130909     D  fnblpnrri            397    400i 0
001000021214      *--------------------------------------------------
001100091125     c     kblt          klist
001200091125     c                   kfld                    blpaas
001300091125     c                   kfld                    blplnp
001400091125     c                   kfld                    blpnrs
001500091125     c                   kfld                    blpnsp
001600070913     c
001700130909     c                   z-add     2013          kaas              4 0
001800130909     c     kaas          setll     fnblp01l
001900130909     c     kaas          reade     fnblp01l
002000051110
002100130909    1c                   DOW       not %EOF(FNblp01l)
002200100111     c*
002300130909    2c                   if        blpmgs>0701 and blpduc=0
002400130909     c                             and blpctm<>'4 ' and blpctm<>'4F'
002500091125
002600091125     c     kblt          setll     fnblt01l
002700091125     c     kblt          reade     fnblt01l
002800130909     c                   clear                   wok               1
002900130909     c                   clear                   wult              8 0
003000130909    3c                   dow       not %eof(fnblt01l) and wok<>'N'
003100130909    4c                   if        bltdfv=0
003200130909     c                   eval      wok='N'
003300130909   x4c                   else
003400130909    5c                   if        bltdfv>wult
003500130909     c                   eval      wult=bltdfv
003600130909    5c                   endif
003700130909     c
003800130909    4c                   endif
003900130909     c
004000091125     c     kblt          reade     fnblt01l
004100130909    3c                   enddo
004200091125     c
004300130909    3c                   if        wok=' '
004400130909     c                   except    errore
004500130909     c
004600130909     c     fnblpnrri     chain     fnblp00f
004700130909    4c                   if        %found(fnblp00f)
004800130909     c                   eval      blpduc=wult
004900130909     c                   update    fnblpf00
005000130909    4c                   endif
005100130909     c
005200130909    3c                   endif
005300130909    2c                   endif
005400091125
005500091125     c
005600130909     c     kaas          reade     fnblp01l
005700091125     c                   enddo
005800021216      *
005900021214     C                   eval       *inlr = *on
006000091125     OQSYSPRT   E            errore      1
006100091125     O                                              'Bolla:'
006200091125     O                       blpaas           +   1
006300091125     O                       blplnp           +   1
006400091125     O                       blpnrs           +   1
006500091125     O                       blpnsp        2  +   1
006600130909     O                       blpctm           +   1
006700130909     O                                        +   2 'volume '
006800130909     O                       blpvlf        2  +   1
006900130909     O                       blpdpc           +   1 '    /  /  '
007000130909     O                       blpduc           +   1 '    /  /  '
007100130909     O                       blpdcm           +   1 '    /  /  '
