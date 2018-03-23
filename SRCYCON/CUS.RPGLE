000100170426     H ACTGRP(*CALLER)
000101170426     Fynfmft01l IF   E           K DISK    alias
000102170426       dcl-f  ff alias
000103170426     Dkpjba          E DS                  qualified
000104170426     DnomeUtente       S             10a
000105170426     C     *entry        plist
000106170426     C                   parm                    kpjba
000107170426     C                   eval      nomeUtente = kpjba.KNMUS
000108170426     C                   eval      *inlr = *on
000109170426     C                   dsply
