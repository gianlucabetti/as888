000100051006     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200051006
000300051006     D OK              C                   '0'
000400051006     D Errore1         C                   '1'                                  Viene ri-emessa la
000500051006     D                                                                          videata.
000600051006     D Errore2         C                   '2'                                  Viene ri-emessa la
000700051006     D                                                                          videata pulita.
000800051006     D kpjba         E DS
000900051006     D rtnCde          S              1
001000051006     D txtMsg          S             50
001100051006
001200051006     C     *ENTRY        PLIST
001300051006     C                   PARM                    kpjba
001400051006     C                   PARM                    rtnCde
001500051006     C                   PARM      *BLANK        txtMsg
001600051006
001700051006     C                   EVAL      rtnCde = OK
001800051006     C                   RETURN
