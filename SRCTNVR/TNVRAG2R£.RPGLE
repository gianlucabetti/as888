000100010517     H DECEDIT('0,') DATEDIT(*YMD.)
000200010517
000300010517      **************************************************************************
000400120622      *                                                                        *
000500120622      * controllo dei potenziali codificati con ultimo stato A8                *
000600010517      *                                                                        *
000700010517      **************************************************************************
000800010517
000900010517     FTNCPO01L  UF   E           K DISK
001000120622     FTICPS11L  IF   E           K DISK
001100010517     FQSYSPRT   O    f  132        PRINTER oflind(*inof)
001200010517
001300010517
001400010517      *
001500120622      * leggo il file dei potenziali
001600120622     c                   do        *hival
001700120622     c                   read      TNCPO01l
001800120622     c                   if        %eof(TNCPO01l)
001900120622     c                   leave
002000120622     c                   endif
002100120622     c                   If        cpofls <> 'C'
002200120622     c                   iter
002300120622     c                   endif
002400120622     c     cpocpo        chain     ticps11l
002500120622     c                   if        not %found(ticps11l)
002600010517     c                   iter
002700010517     c                   else
002800120622      * controllo se stato 8
002900120622     c                   if        cpsfst = '8 ' or cpsfst = 'A8'
003000120622     c                   if        *inof or not *in99
003100120622     c                   except    testa
003200120622     c                   setoff                                       of
003300120622     c                   seton                                        99
003400120622     c                   endif
003500120622     c                   except    detta
003600120622     c                   endif
003700120622     c                   iter
003800010517     c                   endif
003900010517      *
004000010517     c                   enddo
004100010517      *
004200010517     c                   seton                                        lr
004300010517
004400010517     Oqsysprt   E            TESTA             2
004500120622     O                                           70 ' ELENCO POTENZIALI CON    '
004600120622     O                                              'STATO  8   '
004700010517     O                                          125 'PAG.'
004800010517     O                       PAGE          Z    132
004900010517     O          E            testa       1
005000010517     O                                              'Cod.Potenziale'
005100010517     O          E            detta       1
005200120622     O                       cpocpo        z  +   2
005300120622     O                       cporag           +   1
005400120622     O                                        +   1 'causale stato : '
005500120622     O                       cpscnc           +   1
