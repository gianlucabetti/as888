000100051006     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200051006
000300051006     D OK              C                   '0'
000400051006     D Signoff         C                   '9'
000500051006     D utente          S             10
000600051006     D sisInformativo  S             10
000700051006     D rtnCde          S              1
000800051006     D txtMsg          S             50
000900051006
001000051006     C     *ENTRY        PLIST
001100051006     C                   PARM                    utente
001200051006     C                   PARM                    sisInformativo
001300051006     C                   PARM                    rtnCde
001400051006     C                   PARM      *BLANK        txtMsg
001500051006
001600051006     C                   EVAL      rtnCde = OK
001700051006     C                   EVAL      *INLR = *ON
