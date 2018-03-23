000100051006     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200051006
000300051006     D OK              C                   '0'
000400051006     D Errore1         C                   '1'                                  Non viene lanciata
000500051006     D                                                                          l'azione.
000600051006     D kpjba         E DS
000700051006     D rtnCde          S              1
000800051006     D txtMsg          S             50
000900051006
001000051006     C     *ENTRY        PLIST
001100051006     C                   PARM                    kpjba
001200051006     C                   PARM                    rtnCde
001300051006     C                   PARM      *BLANK        txtMsg
001400051006
001500051006     C                   EVAL      rtnCde = OK
001600051006     C                   RETURN
