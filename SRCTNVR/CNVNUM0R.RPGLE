000100020702      *--------------------------------------------------------------*
000200020911      *    crea numeratori nuova gestione prendendo l'ultimo         *
000300020911      *    numero gestito su flnuf
000400020911      *    passa al generatore i seguenti parametri
000500020912      *    numeratore  3
000600020911      *
000700020702      *    PER TRASFERIMENTO FILIALE DA AS/400 AD AS UNICO           *
000800020702      *--------------------------------------------------------------*
000900020911     Fcnvnum0d  cf   e             workstn
001000020702
001100020912     D numeratore      s              3
001200020702
001300020801     d stamp           ds
001400020801     d  line                 367    368b 0
001500020702      *---------------------------------------------------------------*
001600020911     c                   do        *hival
001700020911     c                   exfmt     cnvnu01
001800020911     c   kc              leave
001900020911     c                   if        *inkf  and v1cnum > 0
002000020912     c                   move      v1cnum        numeratore
002100020911     c                   call      'CNVNUM1R'
002200020911     C                   PARM                    numeratore
002300020911     C                   ENDIF
002400020911     C                   ENDDO
002500020801
002600020702     C                   eval      *inlr = *on
002700020702      *
