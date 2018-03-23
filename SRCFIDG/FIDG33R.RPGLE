000100130307     /*PRM dbgview(*source)
000200130307     /*END
000300070321      *---------------------------------------------------------------*
000400070321      *?STAMPA LDV PER CODICE GIRO - BATCH                           ?*
000500070321      *---------------------------------------------------------------*
000600070321
000700070321     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000800070321
000900070321      *---------------------------------------------------------------*
001000070321
001100070416     fFIARG52J  if   e           k disk
001200070416     f                                     rename(FIARG000:FIARG052)
001300070416     f                                     usropn
001400070416     fFIARG02L  if   e           k disk
001500070416     f                                     rename(FIARG000:FIARG002)
001600070416     f                                     usropn
001700070321      *
001800070321     fFIAR401L  Uf   e           k disk
001900070321
002000070321      *---------------------------------------------------------------*
002100070321
002200070321      *
002300070321      *?  C O S T A N T I   - - - - - - - - - - - - - - - - - - - - -?*
002400070321      *
002500070321      *
002600070321      *?  S C H I E R E   - - - - - - - - - - - - - - - - - - - - - -?*
002700070321      *
002800070321      *
002900070321      *?  D S   - - - - - - - - - - - - - - - - - - - - - - - - - - -?*
003000070321      *
003100070321      * - Parametri
003200070321     d KPJBA         e ds
003300070321     d FIDG33ds      e ds                  inz
003400070321      *
003500070321      * - Parametri x Controllo profilo utenti
003600070321     d TIBS34ds      e ds                  inz
003700070321      * - Ds di riferimento al file esterno AZUTE00F
003800070321     d AZUTEds       e ds                       extname(AZUTE00F)
003900070321      * - Ds per dati organigramma
004000070321     d dDatiUte      e ds
004100070322      *
004200070322      * - Parametri per FNLV55R (reperimento terminal arr/par/£6)
004300070322     d FNLV55ds      e ds                  inz
004400070322     d  D55tpt       e                     inz('6')
004500070321      *
004600070321      * - Parametri x Stampa LdV
004700070321     d FNLSB5ds      e ds                  inz
004800070321     d  DB0tbo       e                     inz('A')
004900130307     d FNLSB6ds1     e ds                  inz
005000130307     d  DB6pdf       e                     inz('F')
005100070321      *
005200070321      * - Estenzione per AR4NOT/BL4NOT tipo rec. "A"
005300070321     d dsBL4A        e ds                  inz
005400070321      *
005500070321     d Status         sds
005600070321     d  SDSpgm           *proc
005700070321     d  SDSprm           *parms
005800070321      *
005900070321      *?  V A R I A B I L I   - - - - - - - - - - - - - - - - - - - -?*
006000070321      *
006100070416      * - Flags booleani
006200070416     d $EoF            s              1    inz(*off)
006300070321      * - Campi di comodo
006400070321     d wCountA4        s              3  0 inz
006500070321     d wCountA5        s              3  0 inz
006600070321     d wDate           s              8  0
006700070321     d wData_Iso       s               d        datfmt(*iso)
006800070510     ddtaiso           s               d
006900070510     ddtaeur           s               d   datfmt(*eur)
007000070321      *
007100070321      *?  K E Y - L I S T   - - - - - - - - - - - - - - - - - - - - -?*
007200070321      *
007300070416      * - FIARG02L / FIARG52J
007400070416     c     K02ARGx2      klist
007500070322     c                   kfld                    D55tfa
007600070322     c                   kfld                    D33cgi
007700070322      * - FIAR401L
007800070322     c     K05AR401      klist
007900070322     c                   kfld                    ARGaas
008000070322     c                   kfld                    ARGlnp
008100070322     c                   kfld                    ARGnrs
008200070322     c                   kfld                    ARGnsp
008300070322     c                   kfld                    AR4trc
008400070321
008500070321      *---------------------------------------------------------------*
008600070321      *?RIEPILOGO INDICATORI                                         ?*
008700070321      *---------------------------------------------------------------*
008800070321      * 10   - Comodo                                                 *
008900070321      *---------------------------------------------------------------*
009000070321
009100070321     c     *Entry        plist
009200070321     c                   parm                    KPJBA
009300070321     c                   movel     KPJBU         FIDG33ds
009400070322      *
009500070322      * Reperimento capofila (£6)
009600070322     c                   reset                   FNLV55ds
009700070322     c*** già così:      eval      D55tpt  =  '6'
009800070322     c                   eval      D55lin  =  D33fgs
009900070322     c                   eval      D55drf  =  wDate
010000070322     c                   call      'FNLV55R'
010100070322     c                   parm                    FNLV55ds
010200070322if  2c                   if        D55err  <> *blanks
010300070322     c                   eval      D55tfa  =  D33fgs
010400070322e   2c                   endif
010500070321      *
010600070321      * Elaborazione
010700070416if  1c                   if        D33asl   = *zeros
010800070416     c                   open      FIARG52J
010900070416     c     K02ARGx2      setll     FIARG052
011000070416     c     K02ARGx2      reade     FIARG052
011100070416     c                   eval      $EoF     = %eof(FIARG52J)
011200070416x   1c                   else
011300070416     c                   open      FIARG02L
011400070416     c     K02ARGx2      setll     FIARG002
011500070416     c     K02ARGx2      reade     FIARG002
011600070416     c                   eval      $EoF     = %eof(FIARG02L)
011700070416e   1c                   endif
011800070416      *
011900070416do  1c                   DOW       $EoF     = *off
012000070416      *
012100070322     c                   exsr      Elab_FIARG
012200070416if  2c                   if        D33asl   = *zeros
012300070416     c     K02ARGx2      reade     FIARG052
012400070416     c                   eval      $EoF     = %eof(FIARG52J)
012500070416x   2c                   else
012600070416     c     K02ARGx2      reade     FIARG002
012700070416     c                   eval      $EoF     = %eof(FIARG02L)
012800070416e   2c                   endif
012900070416      *
013000070322e   1c                   ENDDO
013100130307      *
013200130307      * Stampa eventuali documenti PDF abbinati alle spediz. stampate
013300130320if  1c                   if        DB6num > *zero
013400130307     c                   reset                   FNLSB5ds
013500130314     c                   eval      DB0tla   = 'L'
013600130307     c                   eval      DB0fa4   = 'I'
013700130307     c                   eval      DB6pdf   = 'S'
013800130307     c                   call      D33psl
013900130307     c                   parm                    FNLSB5ds
014000130307     c                   parm                    FNLSB6ds1
014100130314x   1c                   else
014200130314     c                   clear                   FNLSB5ds
014300130314     c                   eval      DB0tla = 'C'
014400130314     c                   call      D33psl
014500130314     C                   parm                    FNLSB5ds
014600130307e   1c                   endif
014700070321      *
014800070321      * Fine
014900070416if  1c                   if        D33asl   = *zeros
015000070416     c                   close     FIARG52J
015100070416x   1c                   else
015200070416     c                   close     FIARG02L
015300070416e   1c                   endif
015400070321     c                   eval      *inLR    = *on
015500070321      *
015600070321      *---------------------------------------------------------------*
015700070321      *?Operazioni iniziali                                          ?*
015800070321      *---------------------------------------------------------------*
015900070321     c     *InzSr        BEGSR
016000070510     c*
016100070510     C                   TIME                    WHHUDT           14 0
016200070510     C                   MOVE      WHHUDT        UDATE8            8 0
016300070510     c                   move      udate8        dtaeur
016400070510     c                   move      dtaeur        dtaiso
016500070321      *
016600070321      * Reperimento dati job
016700070321     c     *dtaara       define    §azute        AZUTEds
016800070321     c     *dtaara       define    §datiute      dDATIUTE
016900070321      *
017000070321     c                   in(E)     *dtaara
017100070321     c                   IF        %ERROR or RSUT = *blanks
017200070321     c                   clear                   Tibs34Ds
017300070321     c                   call      'TIBS34R'
017400070321     c                   parm                    Tibs34Ds
017500070321     c                   in        *dtaara
017600070321     c                   ENDIF
017700070321      *
017800070321      * Impostazione dati fissi
017900070321     c                   eval      AR4trc   = 'A'
018000070510     c******             move      *date         wData_Iso
018100070510     c                   move      dtaiso        wData_Iso
018200070321     c     *iso          move      wData_Iso     wDate
018300070321      *
018400070321     c                   ENDSR
018500070321      *
018600070321      *---------------------------------------------------------------*
018700070321      *?Elaborazione singolo giro TELLUS                             ?*
018800070321      *---------------------------------------------------------------*
018900070322     c     Elab_FIARG    BEGSR
019000070321      *
019100070322      * Reperimento rec. di FIAR401L per reperimento/aggiornamento
019200070322      *   data stampa LdV
019300070322if  1c                   if        D33asl   = *zeros
019400070322     c     K05AR401      chain     FIAR4000
019500070322x   1c                   else
019600070322     c     K05AR401      chain(n)  FIAR4000
019700070322e   1c                   endif
019800070322if  1c                   if        %found(FIAR401L)
019900070322     c                   movel     AR4not        dsBL4A
020000070322x   1c                   else
020100070322     c                   clear                   dsBL4A
020200070322e   1c                   endif
020300070322      *
020400070322      * SE stampa:   Si scartano le LdV (del giro) già stampate
020500070322      * SE RIstampa: Si scartano le LdV (del giro) non ancora stampate
020600070322      *                               o stampate in giorno diverso
020700070322if  1c                   if            (D33asl   = *zeros
020800070322     c                             and  §B4asl   > *zeros)
020900070322     c                             OR  (D33asl   > *zeros
021000070322     c                             and  §B4asl  <> D33asl)
021100070322     c                   leavesr
021200070322e   1c                   endif
021300070322      *
021400070322      * (RI)Stampa LdV
021500070322     c                   exsr      Call_FNLSB5
021600070322      *
021700070322      * SE stampa:   Aggiornamento data stampa LdV
021800070416if  1c                   IF             D33asl   = *zeros
021900070322     c                             and  %found(FIAR401L)
022000070322     c                   z-add     wDate         §B4asl
022100070420      * aggiorno flag stampa DDT se bolla mai stampata in arrivo
022200070420     c                   select
022300070420     c                   when      §b4abm = 'S'
022400070420     c                   eval      §b4abm = 'P'
022500070420     c                   when      §b4abm = 'C'
022600070420     c                   eval      §b4abm = 'Q'
022700070420     c                   when      §b4abm = 'K'
022800070420     c                   eval      §b4abm = 'W'
022900070420     c                   when      §b4abm = 'N' or §b4abm = 'Y'
023000070420     C                   eval      §b4abm = 'J'
023100070420     c                   endsl
023200070322     c                   movel     dsBL4A        AR4not
023300070322      *                  __________________
023400070326     c                   update    FIAR4000
023500070322      *                  ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
023600070416e   1c                   ENDIF
023700070322      *
023800070322     c                   ENDSR
023900070322      *
024000070322      *---------------------------------------------------------------*
024100070322      *?Richiamo del pgm. di (RI)stampa LdV (FNLSB5R)                ?*
024200070322      *---------------------------------------------------------------*
024300070322     c     Call_FNLSB5   BEGSR
024400070322      *
024500070322     c                   reset                   FNLSB5ds
024600070322if  1c                   if        §B4asl   > *zeros
024700070322     c                   eval      DB0ris   = 'R'
024800070322e   1c                   endif
024900070322     c                   eval      DB0aas   = ARGaas
025000070322     c                   eval      DB0lnp   = ARGlnp
025100070322     c                   eval      DB0nrs   = ARGnrs
025200070322     c                   eval      DB0nsp   = ARGnsp
025300070322     c*** già così:      eval      DB0tbo   = 'A'
025400070322if  1c                   if        wCountA4 >= 250
025500070322     c                   eval      DB0fl7   = *on
025600070322     c                   clear                   wCountA4
025700070322e   1c                   endif
025800070322if  1c                   if        wCountA5 >= 250
025900070322     c                   eval      DB0fl8   = *on
026000070322     c                   clear                   wCountA5
026100070322e   1c                   endif
026200070322      *
026300070322     c                   call      D33psl
026400070322     c                   parm                    FNLSB5ds
026500130307     c                   parm                    FNLSB6ds1
026600070322      *
026700070322      * Conteggio delle pagine finora stampate
026800070322sel 1c                   select
026900130314w   1c                   when      DB0tms   = '5'  or  DB0tms = 'I'
027000130325     c*//                add       1             wCountA5
027100130325     c                   add       1             wCountA4
027200130325w   1c*//                when      DB0tms   = '4'  or  DB0tms = 'J'
027300130325x   1c                   other
027400130325     c                   add       1             wCountA4
027500070322e   1c                   endsl
027600070322      *
027700070322     c                   ENDSR
