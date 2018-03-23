000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*--------------------------------------------------------------*
000300020322     D KPJBA         E DS
000400131011     D truloreds     E DS
000500140109     D tisio9ds      E DS
000600980528     I***************************************************************
000700131011     c     *entry        plist
000800131011     c                   parm                    serv              1
000900131011     c                   parm                    fil               3
001000131011     c                   parm                    CAP               9
001100131011     c                   parm                    NAZ               3
001200131011     c                   parm                    LOC              35
001300131011     c                   movel     fil           wfil              3 0
001400140109     c                   clear                   tisio9ds
001500131011     c                   eval       do0lna  = wfil
001600131011     c                   eval       do0cap  = cap
001700131011     c                   eval       do0loc  = loc
001800131011     c                   eval       do0nar  = naz
001900131011     c                   eval       do0tser  = serv
002000131011
002100140109     c                   call      'TISIO9R'
002200131011     c                   parm                    kpjba
002300140109     c                   parm                    tisio9ds
002400131011
002500131011     c                   clear                   truloreds
002600131011     c                   eval       IOREfil = wfil
002700131011     c                   eval       IOREcap = cap
002800131011     c                   eval       IOREloc = loc
002900131011     c                   eval       IOREnar = naz
003000131011     c                   eval       IOREtser = serv
003100131011
003200140109     c***                call      'TRULORER'
003300140109     c***                parm                    kpjba
003400140109     c***                parm                    truloreds
003500020322     C*
003600020322     C                   SETON                                        LR
