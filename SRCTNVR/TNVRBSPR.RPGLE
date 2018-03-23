000100940224     H*--------------------------------------------------------------*
000200100408     FTNTBE01L  iF   E           K DISK
000300160112     Ffibsp02l  uF a E           K DISK
000400100416     Fqsysprt   O    F  132        PRINTER
000500100408      *---------------------------------------------------------------*
000600160112     d DBSP          e ds
000700160112     d                 ds
000800160112     d bsplin
000900160112     d sbsp                           3  0 dim(30) overlay(bsplin)
001000160112     d DataSYS         s               d   inz(*sys)
001100100408      *---------------------------------------------------------------*
001200100408     C     Tbe01L        Klist
001300100408     c                   Kfld                    TbeCOD
001400100408     c                   Kfld                    TbeKE1
001500160112     c                   eval      tbecod='BSP'
001600100408      *---------------------------------------------------------------*
001700160112     c                   clear                   indx              3 0
001800160112     c
001900100416     C     TbeCOD        setll     TnTbe01l
002000100416     C     TbeCOD        reade     TnTbe01l
002100160112    1c                   dow       not %EoF(TnTbe01l)
002200100416      *
002300160112    2c                   if        tbeATB = *blank
002400160112    3c                   if        %subst(tbeke1:1:3)>'000'
002500160112     c                   eval      dbsp=tbeuni
002600160112     c                   movel     tbeke1        wtfp              3 0
002700160112     c                   movel     tbeke2        wksc              7 0
002800160112     c     wksc          chain     fibsp02l
002900160112    4c                   if        not %found(fibsp02l)
003000160112     c                   clear                   fibsp000
003100160112     c                   eval      bspksc=wksc
003200160112     c                   eval      bspdes=§BSPDES
003300160112     c                   movel     *all'0'       bsplin
003400160112    4c                   endif
003500160112
003600160112     c                   if        %found(fibsp02l) and bspdcrp<>§bspsod
003700160112     c                   except    err1
003800160112     c                   endif
003900160112     c                   if        %found(fibsp02l) and bspaut <>§bspaut
004000160112     c                   except    err2
004100160112     c                   endif
004200160112     c
004300160112    4c                   if        §BSPSOD='S'
004400160112     c                   eval      BSPDCRP='S'
004500160112    4c                   endif
004600160112    4c                   if        §BSPAUT='N'
004700160112     c                   eval      BSPaut ='N'
004800160112    4c                   endif
004900160112
005000160112     c                   if        %lookup(wtfp:sbsp)=0
005100160112     c                   eval      indx=%lookup(000:sbsp)
005200160112     c                   eval      sbsp(indx)=wtfp
005300160112     c                   endif
005400160112     c                   eval      bspduv=%dec(datasys)
005500160112     c
005600160112    4c                   if        not %found(fibsp02l)
005700160112     c                   write     fibsp000
005800160112     c                   else
005900160112     c                   update    fibsp000
006000160112    4c                   endif
006100160112     c
006200160112    3c                   end
006300100416      *
006400160112    2c                   end
006500100416      *
006600100416     C     TbeCOD        reade     TnTbe01l
006700160112    1c                   end
006800100416      *
006900000000     C                   SETON                                        LR
007000100416      *---------------------------------------------------------------*
007100160112     Oqsysprt   E            err1        1
007200100416     o                                            1 ' '
007300160112     O                       wksc               + 1
007400160112     O                                          + 2 'diverso bspdcrp'
007500160112     Oqsysprt   E            err2        1
007600160112     o                                            1 ' '
007700160112     O                       wksc               + 1
007800160112     O                                          + 2 'diverso bspaut '
007900100416      **
