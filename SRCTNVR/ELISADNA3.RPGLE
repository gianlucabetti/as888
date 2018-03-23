000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300121213     FAZCAE01L  IF   E           K DISK
000400020802     D
000500121211     D KPJBA         E DS
000600980528     I***************************************************************
000700130403     d fidna3ds      e ds
000800980528     c**
000900130403     c                   clear                   fidna3ds
001000130403     c                   eval      ia3mad=' 30'
001100130403     c                   eval      ia3tor='S'
001200130403     c                   eval      ia3ogg='0010003134892013'
001300130403     c* spostato il richiamo
001400130403     c                   eval      ia3no1='pippo      pluto     topolino  minni'
001500130403
001600130403     c* salvataggio knmus prima di modificarlo
001700130403     c                   eval      knmus='EDPFAT    '
001800130403     c                   call      'FIDNA3R'
001900130403     c                   parm                    kpjba
002000130403     c                   parm                    fidna3ds
002100130403
002200020322     C                   SETON                                        LR
