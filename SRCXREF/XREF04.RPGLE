000100990514      *PARMS DFTACTGRP(*YES)
000200990514      *PARMS
000300990514      *PARMS
000400990514      *PARMS
000500990514      *PARMS
000600990517      /TITLE Normalizzazione XRDBR00F.
000700990525     Fxrdbr00f  o    e             disk
000800990525     Fxrdbr01l  if   e           k disk
000900990525     F                                     rename(qwhdrdbr:xrdbr010)
001000990525
001100990525     D xrdbrds       e ds                  extname(xrdbr00f)
001200990525     D xrdbrsav        s                   like(xrdbrds)
001300990525     D whrfisav        s                   like(whrfi)
001400990525
001500990525     C     *entry        plist
001600990525     C                   parm                    libreria         10
001700990525
001800990525     C     libreria      chain     xrdbr01l                           lr
001900990525     C     *inlr         doweq     *off
002000990525
002100990525     C                   eval      whrfisav = whrfi
002200990525
002300990525     C     libreria      reade     xrdbr01l                               lr
002400990525
002500990525     C     whrfi         ifne      whrfisav
002600990525     C     *inlr         oreq      *on
002700990525     C                   eval      xrdbrsav = xrdbrds
002800990525     C                   clear                   qwhdrdbr
002900990525     C                   eval      whrtyp = 'P'
003000990525     C                   eval      whrfi  = whrfisav
003100990525     C                   eval      whrli  = libreria
003200990525     C                   eval      whrefi = whrfisav
003300990525     C                   eval      whreli = libreria
003400990525     C                   eval      whtype = 'D'
003500990525     C                   write     qwhdrdbr
003600990525     C                   eval      xrdbrds = xrdbrsav
003700990525     C                   endif
003800990525
003900990525     C                   enddo
