000100070321      *---------------------------------------------------------------*
000200070321      *?STAMPA LDV PER CODICE GIRO - BATCH                           ?*
000300070321      *---------------------------------------------------------------*
000400070321
000500070321     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000600070321
000700070321      *---------------------------------------------------------------*
000800070321
000900070416     fFIARG52J  if   e           k disk
001000070416     f                                     rename(FIARG000:FIARG052)
001100070416     f                                     usropn
001200070416     fFIARG02L  if   e           k disk
001300070416     f                                     rename(FIARG000:FIARG002)
001400070416     f                                     usropn
001500070321      *
001600070321     fFIAR401L  Uf   e           k disk
001700070321
001800070321      *---------------------------------------------------------------*
001900070321
002000070321      *
002100070321      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002200070321      *
002300070321      *
002400070321      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002500070321      *
002600070321      *
002700070321      *?  D S   - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
002800070321      *
002900070321      * - Parametri
003000070321     d KPJBA         e ds
003100070321     d FIDG33ds      e ds                  inz
003200070321      *
003300070321      * - Parametri x Controllo profilo utenti
003400070321     d TIBS34ds      e ds                  inz
003500070321      * - Ds di riferimento al file esterno AZUTE00F
003600070321     d AZUTEds       e ds                       extname(AZUTE00F)
003700070321      * - Ds per dati organigramma
003800070321     d dDatiUte      e ds
003900070322      *
004000070322      * - Parametri per FNLV55R (reperimento terminal arr/par/£6)
004100070322     d FNLV55ds      e ds                  inz
004200070322     d  D55tpt       e                     inz('6')
004300070321      *
004400070321      * - Parametri x Stampa LdV
004500070321     d FNLSB5ds      e ds                  inz
004600070321     d  DB0tbo       e                     inz('A')
004700070321      *
004800070321      * - Estenzione per AR4NOT/BL4NOT tipo rec. "A"
004900070321     d dsBL4A        e ds                  inz
005000070321      *
005100070321     d Status         sds
005200070321     d  SDSpgm           *proc
005300070321     d  SDSprm           *parms
005400070321      *
005500070321      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
005600070321      *
005700070416      * - Flags booleani
005800070416     d $EoF            s              1    inz(*off)
005900070321      * - Campi di comodo
006000070321     d wCountA4        s              3  0 inz
006100070321     d wCountA5        s              3  0 inz
006200070321     d wDate           s              8  0
006300070321     d wData_Iso       s               d        datfmt(*iso)
006400070510     ddtaiso           s               d
006500070510     ddtaeur           s               d   datfmt(*eur)
006600070321      *
006700070321      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
006800070321      *
006900070416      * - FIARG02L / FIARG52J
007000070416     c     K02ARGx2      klist
007100070322     c                   kfld                    D55tfa
007200070322     c                   kfld                    D33cgi
007300070322      * - FIAR401L
007400070322     c     K05AR401      klist
007500070322     c                   kfld                    ARGaas
007600070322     c                   kfld                    ARGlnp
007700070322     c                   kfld                    ARGnrs
007800070322     c                   kfld                    ARGnsp
007900070322     c                   kfld                    AR4trc
008000070321
008100070321      *---------------------------------------------------------------*
008200070321      *?RIEPILOGO INDICATORI                                         ?*
008300070321      *---------------------------------------------------------------*
008400070321      * 10   - Comodo                                                 *
008500070321      *---------------------------------------------------------------*
008600070321
008700070321     c     *Entry        plist
008800070321     c                   parm                    KPJBA
008900070321     c                   movel     KPJBU         FIDG33ds
009000070322      *
009100070322      * Reperimento capofila (£6)
009200070322     c                   reset                   FNLV55ds
009300070322     c*** già così:      eval      D55tpt  =  '6'
009400070322     c                   eval      D55lin  =  D33fgs
009500070322     c                   eval      D55drf  =  wDate
009600070322     c                   call      'FNLV55R'
009700070322     c                   parm                    FNLV55ds
009800070322if  2c                   if        D55err  <> *blanks
009900070322     c                   eval      D55tfa  =  D33fgs
010000070322e   2c                   endif
010100070321      *
010200070321      * Elaborazione
010300070416if  1c                   if        D33asl   = *zeros
010400070416     c                   open      FIARG52J
010500070416     c     K02ARGx2      setll     FIARG052
010600070416     c     K02ARGx2      reade     FIARG052
010700070416     c                   eval      $EoF     = %eof(FIARG52J)
010800070416x   1c                   else
010900070416     c                   open      FIARG02L
011000070416     c     K02ARGx2      setll     FIARG002
011100070416     c     K02ARGx2      reade     FIARG002
011200070416     c                   eval      $EoF     = %eof(FIARG02L)
011300070416e   1c                   endif
011400070416      *
011500070416do  1c                   DOW       $EoF     = *off
011600070416      *
011700070322     c                   exsr      Elab_FIARG
011800070416if  2c                   if        D33asl   = *zeros
011900070416     c     K02ARGx2      reade     FIARG052
012000070416     c                   eval      $EoF     = %eof(FIARG52J)
012100070416x   2c                   else
012200070416     c     K02ARGx2      reade     FIARG002
012300070416     c                   eval      $EoF     = %eof(FIARG02L)
012400070416e   2c                   endif
012500070416      *
012600070322e   1c                   ENDDO
012700070321      *
012800070321      * Fine
012900070416if  1c                   if        D33asl   = *zeros
013000070416     c                   close     FIARG52J
013100070416x   1c                   else
013200070416     c                   close     FIARG02L
013300070416e   1c                   endif
013400070321     c                   eval      *inLR    = *on
013500070321      *
013600070321      *---------------------------------------------------------------*
013700070321      *?Operazioni iniziali                                          ?*
013800070321      *---------------------------------------------------------------*
013900070321     c     *InzSr        BEGSR
014000070510     c*
014100070510     C                   TIME                    WHHUDT           14 0
014200070510     C                   MOVE      WHHUDT        UDATE8            8 0
014300070510     c                   move      udate8        dtaeur
014400070510     c                   move      dtaeur        dtaiso
014500070321      *
014600070321      * Reperimento dati job
014700070321     c     *dtaara       define    §azute        AZUTEds
014800070321     c     *dtaara       define    §datiute      dDATIUTE
014900070321      *
015000070321     c                   in(E)     *dtaara
015100070321     c                   IF        %ERROR or RSUT = *blanks
015200070321     c                   clear                   Tibs34Ds
015300070321     c                   call      'TIBS34R'
015400070321     c                   parm                    Tibs34Ds
015500070321     c                   in        *dtaara
015600070321     c                   ENDIF
015700070321      *
015800070321      * Impostazione dati fissi
015900070321     c                   eval      AR4trc   = 'A'
016000070510     c******             move      *date         wData_Iso
016100070510     c                   move      dtaiso        wData_Iso
016200070321     c     *iso          move      wData_Iso     wDate
016300070321      *
016400070321     c                   ENDSR
016500070321      *
016600070321      *---------------------------------------------------------------*
016700070321      *?Elaborazione singolo giro TELLUS                             ?*
016800070321      *---------------------------------------------------------------*
016900070322     c     Elab_FIARG    BEGSR
017000070321      *
017100070322      * Reperimento rec. di FIAR401L per reperimento/aggiornamento
017200070322      *   data stampa LdV
017300070322if  1c                   if        D33asl   = *zeros
017400070322     c     K05AR401      chain     FIAR4000
017500070322x   1c                   else
017600070322     c     K05AR401      chain(n)  FIAR4000
017700070322e   1c                   endif
017800070322if  1c                   if        %found(FIAR401L)
017900070322     c                   movel     AR4not        dsBL4A
018000070322x   1c                   else
018100070322     c                   clear                   dsBL4A
018200070322e   1c                   endif
018300070322      *
018400070322      * SE stampa:   Si scartano le LdV (del giro) già stampate
018500070322      * SE RIstampa: Si scartano le LdV (del giro) non ancora stampate
018600070322      *                               o stampate in giorno diverso
018700070322if  1c                   if            (D33asl   = *zeros
018800070322     c                             and  §B4asl   > *zeros)
018900070322     c                             OR  (D33asl   > *zeros
019000070322     c                             and  §B4asl  <> D33asl)
019100070322     c                   leavesr
019200070322e   1c                   endif
019300070322      *
019400070322      * (RI)Stampa LdV
019500070322     c                   exsr      Call_FNLSB5
019600070322      *
019700070322      * SE stampa:   Aggiornamento data stampa LdV
019800070416if  1c                   IF             D33asl   = *zeros
019900070322     c                             and  %found(FIAR401L)
020000070322     c                   z-add     wDate         §B4asl
020100070420      * aggiorno flag stampa DDT se bolla mai stampata in arrivo
020200070420     c                   select
020300070420     c                   when      §b4abm = 'S'
020400070420     c                   eval      §b4abm = 'P'
020500070420     c                   when      §b4abm = 'C'
020600070420     c                   eval      §b4abm = 'Q'
020700070420     c                   when      §b4abm = 'K'
020800070420     c                   eval      §b4abm = 'W'
020900070420     c                   when      §b4abm = 'N' or §b4abm = 'Y'
021000070420     C                   eval      §b4abm = 'J'
021100070420     c                   endsl
021200070322     c                   movel     dsBL4A        AR4not
021300070322      *                  __________________
021400070326     c                   update    FIAR4000
021500070322      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
021600070416e   1c                   ENDIF
021700070322      *
021800070322     c                   ENDSR
021900070322      *
022000070322      *---------------------------------------------------------------*
022100070322      *?Richiamo del pgm. di (RI)stampa LdV (FNLSB5R)                ?*
022200070322      *---------------------------------------------------------------*
022300070322     c     Call_FNLSB5   BEGSR
022400070322      *
022500070322     c                   reset                   FNLSB5ds
022600070322if  1c                   if        §B4asl   > *zeros
022700070322     c                   eval      DB0ris   = 'R'
022800070322e   1c                   endif
022900070322     c                   eval      DB0aas   = ARGaas
023000070322     c                   eval      DB0lnp   = ARGlnp
023100070322     c                   eval      DB0nrs   = ARGnrs
023200070322     c                   eval      DB0nsp   = ARGnsp
023300070322     c*** già così:      eval      DB0tbo   = 'A'
023400070322if  1c                   if        wCountA4 >= 250
023500070322     c                   eval      DB0fl7   = *on
023600070322     c                   clear                   wCountA4
023700070322e   1c                   endif
023800070322if  1c                   if        wCountA5 >= 250
023900070322     c                   eval      DB0fl8   = *on
024000070322     c                   clear                   wCountA5
024100070322e   1c                   endif
024200070322      *
024300070322     c                   call      D33psl
024400070322     c                   parm                    FNLSB5ds
024500070322      *
024600070322      * Conteggio delle pagine finora stampate
024700070322sel 1c                   select
024800070322w   1c                   when      DB0tms   = '4'
024900070322     c                   add       1             wCountA4
025000070322w   1c                   when      DB0tms   = '5'
025100070322     c                   add       1             wCountA5
025200070322e   1c                   endsl
025300070322      *
025400070322     c                   ENDSR
