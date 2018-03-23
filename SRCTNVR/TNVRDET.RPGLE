000100040416     FFNDet02L  IF   E           K DISK
000200040416     FFNDct05L  IF   E           K DISK
000300010801     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000400981125
000500010706
000600040416     c     KDET          KLIST
000700040416     c                   kfld                    anno
000800040416     c                   z-add     1999          ANNO              4 0
000900010706
001000040416     c                   except    testa
001100010620
001200040416     C     ANNO          SETLL     FNDET02L
001300040416
001400040416     c                   do        *hival
001500040416     C                   READ      FNDet02l
001600981125
001700040416     C                   if        %EOF(FNDET02L)
001800040416     c                   leave
001900040416     c                   endif
002000040416      * NON DEVE ESSERE UN EVENTO
002100040416     C                   IF        DETNEV > 0
002200040416     c                   iter
002300040416     c                   endif
002400040416      * deve avere data invio in sede uguale a zero
002500040416     C                   IF        DETdds > 0
002600040416     c                   iter
002700040416     c                   endif
002800040416
002900010620
003000040416      * verifico se ci sono C.A. in fase 420
003100040416     c     KDCT          KLIST
003200040416     c                   kfld                    detaad
003300040416     c                   kfld                    detpod
003400040416     c                   kfld                    detnde
003500040416      *
003600040416     c     kdct          setll     fndct05l
003700040416     c                   do        *hival
003800040416     c     kdct          reade     fndct05l
003900040416     c                   if        %eof(fndct05l)
004000040416     c                   leave
004100040416     c                   endif
004200040416      * verifico la fase
004300040416     c                   if        dctfca > 420
004400040416     c                   except    detta
004500040416     c                   endif
004600040416     C                   ENDDO
004700010706
004800010801     C                   ENDDO
004900010620
005000010801     C                   EVAL      *INLR = *ON
005100010706      *-------------------------------------------------
005200010801     OQSYSPRT   E            TESTA             1
005300040416     O                                           66 'Denunce inviate in sede'
005400040416     O                                           76 ' senza data'
005500040416     O                                           98 '*TNVRDET*'
005600010801     O                                          128 'Pag.'
005700010801     O                       PAGE          Z    132
005800010801     O*
005900010801     O          E            TESTA
006000010801     O*
006100010801     O          E            DETTA       1
006200010801     O                       DCTAAC               6
006300010803     O                       DCTFIL              23
006400010801     O                       DCTNCA        Z     41
006500040416     O                       dctfca            +  4
006600040416     O                       DCTaad            +  4
006700040416     O                       dctpod            +  4
006800040416     O                       DCTnde            +  4
