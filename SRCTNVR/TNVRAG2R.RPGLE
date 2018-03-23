000100010517     H DECEDIT('0,') DATEDIT(*YMD.)
000200010517
000300010517      **************************************************************************
000400120622      *                                                                        *
000500120628      * controllo la categoria del potenziale che hanno l'ultimo stato a 8     *
000600010517      *                                                                        *
000700010517      **************************************************************************
000800010517
000900010517     FTNCPO01L  UF   E           K DISK
001000120622     FTICPS11L  IF   E           K DISK
001100120628     fTIATC04L  if   e           k disk
001200010517     FQSYSPRT   O    f  132        PRINTER oflind(*inof)
001300010517
001400120628      // - Campi di comodo
001500120628     d attivita        s              2
001600010517
001700010517      *
001800120622      * leggo il file dei potenziali
001900120622     c                   do        *hival
002000120622     c                   read      TNCPO01l
002100120622     c                   if        %eof(TNCPO01l)
002200120622     c                   leave
002300120622     c                   endif
002400120622     c     cpocpo        chain     ticps11l
002500120622     c                   if        not %found(ticps11l)
002600010517     c                   iter
002700010517     c                   else
002800120628      * controllo se stato A8
002900120628     c                   if        cpsfst =  'A8'
003000120628      * verifico se ci sono attività
003100120628      /free
003200120628           setll CPOcpo TIATC04L;
003300120628           IF  %equal(TIATC04L)  ;
003400120628               attivita = 'SI' ;
003500120628           else ;
003600120628               attivita = 'NO' ;
003700120628           ENDIF;
003800120628      /end-free
003900120628     c                   if        *inof or not *in99
004000120628     c                   except    testa
004100120628     c                   setoff                                       of
004200120628     c                   seton                                        99
004300120628     c                   endif
004400120628     c                   If        attivita = 'NO'
004500120622     c                   except    detta
004600120628     c                   endif
004700120622     c                   endif
004800120622     c                   iter
004900010517     c                   endif
005000010517      *
005100010517     c                   enddo
005200010517      *
005300010517     c                   seton                                        lr
005400010517
005500010517     Oqsysprt   E            TESTA             2
005600120622     O                                           70 ' ELENCO POTENZIALI CON    '
005700120622     O                                              'STATO  8   '
005800010517     O                                          125 'PAG.'
005900010517     O                       PAGE          Z    132
006000010517     O          E            testa       1
006100010517     O                                              'Cod.Potenziale'
006200010517     O          E            detta       1
006300120622     O                       cpocpo        z  +   2
006400120622     O                       cporag           +   1
006500120628     O                                        +   1 'categoria :'
006600120628     o                       cpofls           +   1
006700120628     O                                        +   2 'causale stato : '
006800120622     O                       cpscnc           +   1
006900120628     o                                        +   2 'ci sono attività ?'
007000120628     o                       attivita         +   1
