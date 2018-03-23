000100051006     H DFTACTGRP(*NO) ACTGRP(*CALLER)
000200051006
000300051006     D OK              C                   '0'
000400051006     D Errore1         C                   '1'                                  Non viene eseguita
000500051006     D                                                                          l'azione, viene
000600051006     D                                                                          risottomessa
000700051006     D                                                                          congelata e viene
000800051006     D                                                                          inviato un messaggio
000900051006     D                                                                          di avvertimento.
001000051006     D Errore2         C                   '2'                                  Non viene eseguita
001100051006     D                                                                          l'azione e viene
001200051006     D                                                                          inviato un messaggio
001300051006     D                                                                          di avvertimento.
001400051006     D kpjba         E DS
001500051006     D rtnCde          S              1
001600051006     D txtMsg          S             50
001700051006
001800051006     C     *ENTRY        PLIST
001900051006     C                   PARM                    kpjba
002000051006     C                   PARM                    rtnCde
002100051006     C                   PARM      *BLANK        txtMsg
002200051006
002300051006     C                   EVAL      rtnCde = OK
002400051006     C                   RETURN
