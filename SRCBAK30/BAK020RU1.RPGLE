000100051006     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200051006
000300051006     D OK              C                   '0'
000400051006     D Errore          C                   '1'
000500051006     D Signoff         C                   '9'
000600051006     D kpjba         E DS
000700051006     D menu            S              4
000800051006     D rtnCde          S              1
000900051006     D txtMsg          S             50
001000051006
001100051006     C     *ENTRY        PLIST
001200051006     C                   PARM                    kpjba
001300051006     C                   PARM                    menu
001400051006     C                   PARM                    rtnCde
001500051006     C                   PARM      *BLANK        txtMsg
001600051006
001700051006     C                   EVAL      rtnCde = OK
001800051006     C                   RETURN
