000100010517     H DECEDIT('0,') DATEDIT(*YMD.)
000200010517
000300010517      **************************************************************************
000400120622      *                                                                        *
000500120703      * verifico potenziali per pulizia                                        *
000600010517      *                                                                        *
000700010517      **************************************************************************
000800010517
000900120629     FTNCPO01L  iF   E           K DISK
001000120703     fTIATC05L  if   e           k disk
001100120629     fTIvis01l  if   e           k disk
001200120703     fwfagpp0f  o  a e             disk
001300010517     FQSYSPRT   O    f  132        PRINTER oflind(*inof)
001400010517
001500120628      // - Campi di comodo
001600120628     d attivita        s              2
001700120629     d trattative      s              2
001800120703     d tratt_aperte    s              7  0
001900120703     d attiv_aperte    s              7  0
002000120703     d tratt_mag2011   s              7  0
002100120703     d attiv_mag2011   s              7  0
002200120703     d tratt_min2011   s              7  0
002300120703     d attiv_min2011   s              7  0
002400120703     d tratt_cavallo   s              7  0
002500010517
002600010517      *
002700120622      * leggo il file dei potenziali
002800120622     c                   do        *hival
002900120622     c                   read      TNCPO01l
003000120622     c                   if        %eof(TNCPO01l)
003100120622     c                   leave
003200120622     c                   endif
003300120703     c                   clear                   tratt_aperte
003400120703     c                   clear                   attiv_aperte
003500120703     c                   clear                   tratt_mag2011
003600120703     c                   clear                   attiv_mag2011
003700120703     c                   clear                   tratt_min2011
003800120703     c                   clear                   attiv_min2011
003900120703     c                   clear                   tratt_cavallo
004000120629      * verifico se ci sono attività  o trattative
004100120628      /free
004200120703           setll CPOcpo TIATC05L;
004300120703           IF  %equal(TIATC05L)  ;
004400120628               attivita = 'SI' ;
004500120628           else ;
004600120628               attivita = 'NO' ;
004700120628           ENDIF;
004800120703          // verifico se ci sono attività chiuse con data > 31122011
004900120703           reade cpocpo tiatc05l ;
005000120703           dow not %eof(tiatc05l) ;
005100120703               If atcdco >= 20111231 and atcnrv = 0 ;
005200120703                   attiv_mag2011 = attiv_mag2011 + 1 ;
005300120703               Endif ;
005400120703               If atcdco < 20111231 and atcdco > 0  and atcnrv = 0;
005500120703                   attiv_min2011 = attiv_min2011 + 1 ;
005600120703               Endif ;
005700120703               If atcdco = 0 and atcnrv = 0 ;
005800120703                   attiv_aperte  = attiv_aperte  + 1 ;
005900120703               Endif ;
006000120703               reade cpocpo tiatc05l ;
006100120703           enddo ;
006200120629           setll CPOcpo TIVIS01L;
006300120629           IF  %equal(TIVIS01L)  ;
006400120629               trattative  = 'SI' ;
006500120629           else ;
006600120629               trattative = 'NO' ;
006700120629           ENDIF;
006800120703          // verifico se ci sono trattative chiuse con data > 31122011
006900120703           reade cpocpo tivis01l ;
007000120703           dow not %eof(tivis01l) ;
007100120703               If visdch >= 20111231 and visdat < 20111231 ;
007200120703                   tratt_cavallo = tratt_cavallo + 1 ;
007300120703               Endif ;
007400120703               If visdch >= 20111231 and visdat > 20111231 ;
007500120703                   tratt_mag2011 = tratt_mag2011 + 1 ;
007600120703               Endif ;
007700120703               If visdch < 20111231 and visdch > 0 ;
007800120703                   tratt_min2011 = tratt_min2011 + 1 ;
007900120703               Endif ;
008000120703               If visdch = 0 ;
008100120703                   tratt_aperte  = tratt_aperte  + 1 ;
008200120703               Endif ;
008300120703               reade cpocpo tivis01l ;
008400120703           enddo ;
008500120628      /end-free
008600120628     c                   if        *inof or not *in99
008700120628     c                   except    testa
008800120628     c                   setoff                                       of
008900120628     c                   seton                                        99
009000120628     c                   endif
009100120622     c                   except    detta
009200120703     c                   eval      tratt_aper = tratt_aperte
009300120703     c                   eval      attiv_aper = attiv_aperte
009400120703     c                   eval      tratt_magg = tratt_mag2011
009500120703     c                   eval      tratt_mino = tratt_min2011
009600120703     c                   eval      attiv_magg = attiv_mag2011
009700120703     c                   eval      attiv_mino = attiv_min2011
009800120703     c                   eval      tratt_cava = tratt_cavallo
009900120703     c                   write     wfagp000
010000010517      *
010100010517     c                   enddo
010200010517      *
010300010517     c                   seton                                        lr
010400010517
010500010517     Oqsysprt   E            TESTA             2
010600120703     O                                           50 ' ANALISI POTENZIALI DATA P'
010700120703     O                                              'ULIZIA 31/12/2011'
010800010517     O                                          125 'PAG.'
010900010517     O                       PAGE          Z    132
011000010517     O          E            testa       1
011100010517     O                                              'Cod.Potenziale'
011200120703     O                                        +  47 'Trat.Aperte'
011300120703     O                                        +   2 'ch.> 12/11'
011400120703     O                                        +   2 'ch.< 12/11'
011500120703     O                                        +   2 'Att.Aperte'
011600120703     O                                        +   2 'ch.> 12/11'
011700120703     O                                        +   2 'ch.< 12/11'
011800010517     O          E            detta       1
011900120703     O                       cpocpo        z  +   1
012000120622     O                       cporag           +   1
012100120703     o                       tratt_aperte  z  +   2
012200120703     O                       tratt_mag2011 z  +   4
012300120703     O                       tratt_min2011 z  +   5
012400120703     o                       attiv_aperte  z  +   5
012500120703     O                       attiv_mag2011 z  +   5
012600120703     O                       attiv_min2011 z  +   5
