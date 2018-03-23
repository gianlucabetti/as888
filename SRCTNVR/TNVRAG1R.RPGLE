000100010517     H DECEDIT('0,') DATEDIT(*YMD.)
000200010517
000300010517      **************************************************************************
000400120622      *                                                                        *
000500120622      * controllo dei potenziali codificati con ultimo stato A8                *
000600010517      *                                                                        *
000700010517      **************************************************************************
000800010517
000900120629     FTNCPO01L  iF   E           K DISK
001000120622     FTICPS11L  IF   E           K DISK
001100120629     fTIATC04L  if   e           k disk
001200120629     fTIvis01l  if   e           k disk
001300120629     fWFagp00f  o  a e             disk
001400010517     FQSYSPRT   O    f  132        PRINTER oflind(*inof)
001500010517
001600120629     d attivita        s              2
001700120629     d trattative      s              2
001800010517
001900010517      *
002000120622      * leggo il file dei potenziali
002100120622     c                   do        *hival
002200120622     c                   read      TNCPO01l
002300120622     c                   if        %eof(TNCPO01l)
002400120622     c                   leave
002500120622     c                   endif
002600120629     c                   If        cpofls <> 'P'
002700120622     c                   iter
002800120622     c                   endif
002900120622     c     cpocpo        chain     ticps11l
003000120622     c                   if        not %found(ticps11l)
003100010517     c                   iter
003200010517     c                   else
003300120622      * controllo se stato 8
003400120629     c                   if        cpsfst = 'A8'
003500120629      * verifico se ci sono attività  o trattative
003600120629      /free
003700120629           setll CPOcpo TIATC04L;
003800120629           IF  %equal(TIATC04L)  ;
003900120629               attivita = 'SI' ;
004000120629           else ;
004100120629               attivita = 'NO' ;
004200120629           ENDIF;
004300120629           setll CPOcpo TIVIS01L;
004400120629           IF  %equal(TIVIS01L)  ;
004500120629               trattative  = 'SI' ;
004600120629           else ;
004700120629               trattative = 'NO' ;
004800120629           ENDIF;
004900120629      /end-free
005000120622     c                   if        *inof or not *in99
005100120622     c                   except    testa
005200120622     c                   setoff                                       of
005300120622     c                   seton                                        99
005400120622     c                   endif
005500120629     c                   If        attivita = 'NO' and trattative =  'NO'
005600120622     c                   except    detta
005700120629     c                   write     wfagp000
005800120622     c                   endif
005900120629     c                   endif
006000120622     c                   iter
006100010517     c                   endif
006200010517      *
006300010517     c                   enddo
006400010517      *
006500010517     c                   seton                                        lr
006600010517
006700010517     Oqsysprt   E            TESTA             2
006800120622     O                                           70 ' ELENCO POTENZIALI CON    '
006900120622     O                                              'STATO  8   '
007000010517     O                                          125 'PAG.'
007100010517     O                       PAGE          Z    132
007200010517     O          E            testa       1
007300010517     O                                              'Cod.Potenziale'
007400010517     O          E            detta       1
007500120622     O                       cpocpo        z  +   2
007600120622     O                       cporag           +   1
007700120622     O                                        +   1 'causale stato : '
007800120622     O                       cpscnc           +   1
