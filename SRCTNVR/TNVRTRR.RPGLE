000100020607     FFNbtp01L  UF   E           K DISK
000200020607     FFNbtt01L  UF   E           K DISK
000300020607     D CNCR80        E DS
000400020607     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
000500020607     C* REPERISCO SIMFEL
000600020607     C                   Z-ADD     1             CODUT
000700020607     c                   call      'X§PARUT'
000800020607     C                   PARM                    UT§DSE
000900020607     C                   MOVEL     REC80         CNCR80
001000020607     c*
001100020607     c     kbtt          klist
001200020607     c                   kfld                    btpaas
001300020607     c                   kfld                    btplnp
001400020607     c                   kfld                    btpnrs
001500020607     c                   kfld                    btpnsp
001600020607     c*
001700020607     C* CICLO SU TESTATA BOLLE TRANSITO
001800020607     C     *loval        setll     fnbtp01l
001900020607     c                   do        *hival
002000020607     c*
002100020607     C                   read      fnbtp01l
002200020607      * fine file
002300020607     c                   if        %eof(fnbtp01l)
002400020607     c                   leave
002500020607     c                   endif
002600020620     c                   z-add     btpflp        btpdt3
002700020607     c                   z-add     simfel        btpflp
002800020607     c* CICLO SU DETTAGLIO COLLI
002900020607     c     kbtt          setll     fnbtt01l
003000020607     c                   do        *hival
003100020607     c     kbtt          reade     fnbtt01l
003200020607      * fine file
003300020607     c                   if        %eof(fnbtt01l)
003400020607     c                   leave
003500020607     c                   endif
003600020607     c                   z-add     simfel        bttflp
003700020607     c                   update    fnbtt000
003800020607     c                   enddo
003900020607     c                   update    fnbtp000
004000020607     c*
004100020607     c                   enddo
004200020607     c*
004300020607     c                   seton                                        lr
