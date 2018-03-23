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
001300121012     Fprtf198   O    f  198        PRINTER oflind(*inof)
001400010517
001500120628      // - Campi di comodo
001600120628     d attivita        s              2
001700120629     d trattative      s              2
001800120703     d tratt_aperte    s              7  0
001900120703     d attiv_aperte    s              7  0
002000120924     d tratt_mag2010   s              7  0
002100120924     d attiv_mag2010   s              7  0
002200120924     d tratt_min2010   s              7  0
002300120924     d attiv_min2010   s              7  0
002400120926     d tratt_chiuse    s              7  0
002500121012     d tipo_pul        s             40
002600010517
002700010517      *
002800120622      * leggo il file dei potenziali
002900120622     c                   do        *hival
003000120622     c                   read      TNCPO01l
003100120622     c                   if        %eof(TNCPO01l)
003200120622     c                   leave
003300120622     c                   endif
003400121011     c                   clear                   VISCPO
003500121011     c                   clear                   ATCCPO
003600120703     c                   clear                   tratt_aperte
003700120703     c                   clear                   attiv_aperte
003800120924     c                   clear                   tratt_mag2010
003900120924     c                   clear                   attiv_mag2010
004000120924     c                   clear                   tratt_min2010
004100120924     c                   clear                   attiv_min2010
004200120926     c                   clear                   tratt_chiuse
004300121012     c                   clear                   tipo_pul
004400120629      * verifico se ci sono attività  o trattative
004500120628      /free
004600120926           setll CPOcpo TIVIS01L;
004700120926          // verifico se ci sono trattative chiuse con data > 31122010
004800120926           reade cpocpo tivis01l ;
004900120926           dow not %eof(tivis01l) ;
005000121011               if visffz <> ' ' ;
005100121011                  ELSE ;
005200120926               If visdch >  20101231 ;
005300120926                   tratt_mag2010 = tratt_mag2010 + 1 ;
005400120926               Endif ;
005500120926               If visdch <= 20101231 and visdch > 0 ;
005600120926                   tratt_min2010 = tratt_min2010 + 1 ;
005700120926               Endif ;
005800121011               If visdch = 0 ;
005900120926                   tratt_aperte  = tratt_aperte  + 1 ;
006000120926               Else ;
006100120926                   tratt_chiuse  = tratt_chiuse  + 1 ;
006200120926               Endif ;
006300121011               Endif ;
006400120926               reade cpocpo tivis01l ;
006500120926           enddo ;
006600120926
006700120703           setll CPOcpo TIATC05L;
006800120924          // verifico se ci sono attività chiuse con data > 31122010
006900120703           reade cpocpo tiatc05l ;
007000121011
007100120703           dow not %eof(tiatc05l) ;
007200121011               If atccac = 'AA' ;
007300121011                  ELSE ;
007400121011               If atcnrv > 0 ;
007500121011                  ELSE ;
007600121011               If atcdco >= 20101231 ;
007700120924                   attiv_mag2010 = attiv_mag2010 + 1 ;
007800120703               Endif ;
007900121011               If atcdco < 20101231 and atcdco > 0  ;
008000120924                   attiv_min2010 = attiv_min2010 + 1 ;
008100120703               Endif ;
008200121011               If atcdco = 0  ;
008300120703                   attiv_aperte  = attiv_aperte  + 1 ;
008400120703               Endif ;
008500121011               Endif ;
008600121011               Endif ;
008700120703               reade cpocpo tiatc05l ;
008800120703           enddo ;
008900120628      /end-free
009000120628     c                   if        *inof or not *in99
009100120628     c                   except    testa
009200120628     c                   setoff                                       of
009300120628     c                   seton                                        99
009400120628     c                   endif
009500120926     c                   eval      tratt_aper = tratt_aperte
009600120926     c                   eval      attiv_aper = attiv_aperte
009700120924     c                   eval      tratt_magg = tratt_mag2010
009800120924     c                   eval      tratt_mino = tratt_min2010
009900120924     c                   eval      attiv_magg = attiv_mag2010
010000120924     c                   eval      attiv_mino = attiv_min2010
010100120926     c                   eval      tratt_chiu = tratt_chiuse
010200121012      *
010300121012      *---------------------------------------------------------------------------------------------
010400121012      * verifico che cosa devo pulire e cosa resta
010500121012      *---------------------------------------------------------------------------------------------
010600121012    1c                   If        TRATT_CHIU = 0
010700121012      * non ho trattative
010800121012      * verifico le attività
010900121012      * se non ci sono attività chiuse non pulisco nulla
011000121012    2c                   if        attiv_magg = 0 and attiv_mino = 0
011100121012    2c                   endif
011200121012      * se la somma delle attività chiuse maggiore data pulizia son maggiore o uguali a 5
011300121012      * pulisco tutto il resto
011400121012    2c                   if        attiv_magg > 4
011500121012    3c                   if        attiv_mino > 0
011600121012     c                   eval      tipo_pul = 'Attività del 2010'
011700121012    3c                   endif
011800121012    2c                   endif
011900121012      *
012000121012   x1C                   ELSE
012100121012      * se ci sono trattative chiuse verifichiamo se sono prima o dopo il 31122010
012200121012      *
012300121012      * verifico se ci sono trattative chiuse con data > 31122010 pulisco tutte quelle chiuse
012400121012      * prima del 01012011
012500121012    2c                   if        tratt_magg > 0
012600121012      * ci sono trattative chiuse prima del 01012011  pulisco e verifico se ci sono anche delle
012700121012      * attività
012800121012    3c                   if        tratt_mino > 0
012900121012
013000121012    4c                   if        attiv_mino > 0
013100121012     c                   eval      tipo_pul = 'Trattative/Attività del 2010'
013200121012   x4c                   else
013300121012     c                   eval      tipo_pul = 'Trattative del 2010'
013400121012    4c                   endif
013500121012
013600121012   x3c                   else
013700121012
013800121012    4c                   if        attiv_mino > 0
013900121012     c                   eval      tipo_pul = 'Attività del 2010'
014000121012    4c                   endif
014100121012
014200121012    3c                   endif
014300121012
014400121012   x2c                   else
014500121012      * se non ci sono trattative chiuse > del 31/12/2010 verifico quante ce ne sono prima
014600121012
014700121012      * se c'è una sola trattativa chiusa prima del 01012011 la tengo e sposto la data
014800121012      * pulizia alla data apertura trattativa
014900121012    3c                   if        tratt_mino = 1
015000121012    4c                   if        attiv_mino > 0
015100121012     c                   eval      tipo_pul = 'Attività prima dta apert. tratt.'
015200121012    4c                   endif
015300121012    3c                   endif
015400121012      * se c'è + di una trattativa chiusa prima del 01012011 tengo la + vicina al 31/12/2010
015500121012      * e sposto la data di pulizia alla data apertura trattativa che tengo
015600121012    3c                   if        tratt_mino > 1
015700121012    4c                   if        attiv_mino > 0
015800121012     c                   eval      tipo_pul = 'Tratt./Attiv.prima dta ap.tratt.'
015900121012   x4c                   else
016000121012     c                   eval      tipo_pul = 'Trattativa/e prima dta ap.tratt.'
016100121012    4c                   endif
016200121012    3c                   endif
016300121012
016400121012    2c                   endif
016500121012
016600121012    1c                   endif
016700121012
016800121012     c                   eval      pulizia_di = tipo_pul
016900121012
017000120703     c                   write     wfagp000
017100121012     c                   except    detta
017200010517     c                   enddo
017300010517      *
017400010517     c                   seton                                        lr
017500010517
017600121012     Oprtf198   E            TESTA             2
017700120703     O                                           50 ' ANALISI POTENZIALI DATA P'
017800120924     O                                              'ULIZIA 31/12/2010'
017900010517     O                                          125 'PAG.'
018000010517     O                       PAGE          Z    132
018100010517     O          E            testa       1
018200010517     O                                              'Cod.Potenziale'
018300120703     O                                        +  47 'Trat.Aperte'
018400120924     O                                        +   2 'ch.> 12/10'
018500120924     O                                        +   2 'ch.< 12/10'
018600120703     O                                        +   2 'Att.Aperte'
018700120924     O                                        +   2 'ch.> 12/10'
018800120924     O                                        +   2 'ch.< 12/10'
018900121012     O                                        +   2 'Cosa pulisco ???'
019000010517     O          E            detta       1
019100120703     O                       cpocpo        z  +   1
019200120622     O                       cporag           +   1
019300120703     o                       tratt_aperte  z  +   2
019400120924     O                       tratt_mag2010 z  +   4
019500120924     O                       tratt_min2010 z  +   5
019600120703     o                       attiv_aperte  z  +   5
019700120924     O                       attiv_mag2010 z  +   5
019800120924     O                       attiv_min2010 z  +   5
019900121012     O                       tipo_pul         +   5
