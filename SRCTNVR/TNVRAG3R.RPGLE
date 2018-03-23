000100010517     H DECEDIT('0,') DATEDIT(*YMD.)
000200010517
000300010517      **************************************************************************
000400120622      *                                                                        *
000500120628      * controllo la categoria del potenziale che hanno l'ultimo stato a 8     *
000600010517      *                                                                        *
000700010517      **************************************************************************
000800010517
000900120629     FTNCPO01L  iF   E           K DISK
001000120622     FTICPS11L  IF   E           K DISK
001100120628     fTIATC04L  if   e           k disk
001200120629     fTIvis01l  if   e           k disk
001300120629     fWFagp00f  o  a e             disk
001400010517     FQSYSPRT   O    f  132        PRINTER oflind(*inof)
001500010517
001600120628      // - Campi di comodo
001700120628     d attivita        s              2
001800120629     d trattative      s              2
001900120629     d persi           s              7  0
002000120629     d codificati      s              7  0
002100120629     d maicodificati   s              7  0
002200120629     d eliminabili     s              6  0
002300010517
002400010517      *
002500120622      * leggo il file dei potenziali
002600120622     c                   do        *hival
002700120622     c                   read      TNCPO01l
002800120622     c                   if        %eof(TNCPO01l)
002900120622     c                   leave
003000120622     c                   endif
003100120629     c                   clear                   cpsfst
003200120629     c                   clear                   cpscnc
003300120622     c     cpocpo        chain     ticps11l
003400120629      * verifico se ci sono attività  o trattative
003500120628      /free
003600120628           setll CPOcpo TIATC04L;
003700120628           IF  %equal(TIATC04L)  ;
003800120628               attivita = 'SI' ;
003900120628           else ;
004000120628               attivita = 'NO' ;
004100120628           ENDIF;
004200120629           setll CPOcpo TIVIS01L;
004300120629           IF  %equal(TIVIS01L)  ;
004400120629               trattative  = 'SI' ;
004500120629           else ;
004600120629               trattative = 'NO' ;
004700120629           ENDIF;
004800120628      /end-free
004900120628     c                   if        *inof or not *in99
005000120628     c                   except    testa
005100120628     c                   setoff                                       of
005200120628     c                   seton                                        99
005300120628     c                   endif
005400120629     c                   If        attivita = 'NO' and trattative =  'NO'
005500120629     c                   write     wfagp000
005600120622     c                   except    detta
005700120629     c                   if        cpofls = 'P'
005800120629     c                   add       1             persi
005900120629     c                   Endif
006000120629     c                   if        cpofls = 'C'
006100120629     c                   add       1             codificati
006200120629     c                   Endif
006300120629     c                   if        cpofls = 'M'
006400120629     c                   add       1             maicodificati
006500120629     c                   Endif
006600120629     c                   if        cpofls = 'E'
006700120629     c                   add       1             eliminabili
006800120629     c                   Endif
006900120628     c                   endif
007000010517      *
007100010517     c                   enddo
007200010517      *
007300120629     c                   except    totale
007400010517     c                   seton                                        lr
007500010517
007600010517     Oqsysprt   E            TESTA             2
007700120629     O                                           50 ' ELENCO POTENZIALI NO ATTI'
007800120629     O                                              'VITA NO TRATTATIVE'
007900010517     O                                          125 'PAG.'
008000010517     O                       PAGE          Z    132
008100010517     O          E            testa       1
008200010517     O                                              'Cod.Potenziale'
008300010517     O          E            detta       1
008400120622     O                       cpocpo        z  +   2
008500120622     O                       cporag           +   1
008600120628     O                                        +   1 'categoria :'
008700120628     o                       cpofls           +   1
008800120629     O                                        +   2 'STATO + CAUSALE '
008900120629     O                       cpsfst           +   1
009000120629     o                                        +   1 '/'
009100120622     O                       cpscnc           +   1
009200120629     O          E            totale      1
009300120629     O                                        +   2 ' Persi :'
009400120629     O                       persi         z  +   2
009500120629     O                                        +   2 ' Codificati :'
009600120629     O                       codificati    z  +   2
009700120629     O                                        +   2 ' Mai Codificati :'
009800120629     O                       maicodificati z  +   2
009900120629     O                                        +   2 ' Eliminabili :'
010000120629     O                       eliminabili   z  +   2
