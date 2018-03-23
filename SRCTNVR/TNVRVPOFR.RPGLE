000100040917     H DECEDIT('0,') DATEDIT(*yMd.)
000200940915      *--------------------------------------------------------------*
000300130712     Ftntbe01l  uF   E           K DISK    USROPN extfile(Wazorg)
000400960302     D*
000500130712     d WAZORG          s             21    inz('GAITRAGRU /TNTBE01L')
000600130712     c     KTBE          Klist
000700130712     c                   Kfld                    kcod              3
000800130712     c                   Kfld                    kke1             15
000900050103     C**********************************************************************
001000130712     c                   open(e)   tntbe01l
001100130712     c                   if        not %open(tntbe01l)
001200130712     c                   eval      %subst(Wazorg:7:4)='GRPS'
001300130712     C                   OPEN      tntbe01l
001400101103     c                   endif
001500130712     c
001600130712     c                   movel     'VPO'         kcod
001700130712     c                   movel     'DECOFI1'     kke1
001800130712     c     ktbe          chain     tntbe01l
001900130712     c                   if        %found(tntbe01l)
002000130712     c                   eval      %subst(tbeuni: 1: 3)='999'
002100130712     c                   update    tntbe000
002200130712     c                   endif
002300130712     c
002400130712     c                   close     tntbe01l
002500130712     c
002600130712     c                   eval      %subst(Wazorg:1:10)='FILTRAGRU '
002700130712     c                   open(e)   tntbe01l
002800130712     c                   if        not %open(tntbe01l)
002900130712     c                   eval      %subst(Wazorg:7:4)='GRPF'
003000130712     C                   OPEN      tntbe01l
003100130712     c                   endif
003200130712     c
003300130712     c                   movel     'DECOFI1'     kke1
003400130712     c     ktbe          chain     tntbe01l
003500130712     c                   if        %found(tntbe01l)
003600130712     c                   eval      %subst(tbeuni: 1: 3)='999'
003700130712     c                   update    tntbe000
003800130712     c                   endif
003900130712     c
004000130712     c
004100960129     C*
004200000000     C                   SETON                                        LR
