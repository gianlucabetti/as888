000100070712     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200070712      *------------------------------------------------------------------------*
000300070712      *
000400080922      *  Stato avanzamento FUEL
000500070712      *
000600070712      *------------------------------------------------------------------------*
000700070712
000800080922     ftnta86d   cf   e             workstn
000900070712
001000070712      *------------------------------------------------------------------------*
001100070712      *  RIEPILOGO INDICATORI
001200070712      *------------------------------------------------------------------------*
001300070712      * 28 - emissione messaggio di errore
001400070712      * 40 - data iniziale errata
001500070712      * 41 - data finale   errata
001600081008      * 42 - data ricerca codice unificante
001700070712
001800070712      *------------------------------------------------------------------------*
001900070712      *   V A R I A B I L I
002000070712      *------------------------------------------------------------------------*
002100070712     d qcmd            s            100    inz
002200070712     d qlen            s             15  5 inz(100)
002300070712     d wdata           s               d   datfmt(*iso)
002400070712     d $fine           s              1    inz(*off)
002500070712
002600070712      *------------------------------------------------------------------------*
002700070712      *   S C H I E R E
002800070712      *------------------------------------------------------------------------*
002900070712     d msg             s             78    dim(10)  ctdata  perrcd(1)
003000070712
003100070712      *------------------------------------------------------------------------*
003200070712      *   D S   I N T E R N E / E S T E R N E
003300070712      *------------------------------------------------------------------------*
003400080922     d tnta86ds        ds                  inz
003500070712     d  dsdip                         6  0 inz(*loval)
003600070712     d  dsdfp                         6  0 inz(*hival)
003700081008     d  dsdru                         8  0 inz(*hival)
003800070712
003900070712     d                 ds                  inz
004000070712     d wdatamax                1      8  0 inz
004100070712     d maxaa                   1      4  0 inz
004200070712     d maxmm                   5      6  0 inz
004300070712     d maxgg                   7      8  0 inz(01)
004400070712
004500070712     d                 ds                  inz
004600070712     d wdatamin                1      8  0 inz
004700070712     d minaa                   1      4  0 inz
004800070712     d minmm                   5      6  0 inz
004900070712     d mingg                   7      8  0 inz(01)
005000070712
005100070712     d wlbdat          ds                  inz
005200070712     d  g08dat                        8  0 inz
005300070712     d  g08inv                        8  0 inz
005400070712     d  g08err                        1    inz('3')
005500070712     d  g08tgi                        5  0 inz
005600070712
005700070712     d azuteds       e ds                       extname(azute00f)
005800070712     d ddatiute      e ds
005900070712     d kpjba         e ds
006000070712     d tibs34ds      e ds
006100070712
006200070712     d status         sds
006300070712     d  v1cpgm           *proc
006400070712
006500070228      *---------------------------------------------------------------*
006600070712
006700070712     c                   clear                   v1cdip
006800070712     c                   clear                   v1cdfp
006900070712
007000070712     c                   do        *hival
007100070712
007200080922     c                   exfmt     ta86d01
007300070712     c                   clear                   v1cmsg
007400070712     c                   eval      *in28 = *off
007500070712
007600070712      * f3=fine
007700070712     c   kc              leave
007800070712
007900070712     c                   if        dutpou <> 046
008000070712     c                   eval      v1cmsg = msg(03)
008100070712     c                   eval      *in28 = *on
008200070712     c                   iter
008300070712     c                   endif
008400070712
008500080925      * f9=visualizzazione archivio wftfu10f
008600070712     c                   if        *inki
008700070712     c                   exsr      f09
008800070712     c                   iter
008900070712     c                   endif
009000070712
009100070712      * controllo i dati immessi a video
009200070712     c                   exsr      ctrd01
009300070712     c                   if        *in28
009400070712     c                   iter
009500070712     c                   endif
009600070712
009700070712      * f6=lancio batch
009800070712     c                   if        *inkf
009900070712     c                   exsr      sbmjob
010000070712     c                   leave
010100070712     c                   endif
010200070712
010300070712     c                   enddo
010400070712
010500070712     c                   eval      *inlr = *on
010600070712
010700070712      *------------------------------------------------------------------------*
010800070712      * Controlli videata
010900070712      *------------------------------------------------------------------------*
011000070712     c     ctrd01        begsr
011100070712
011200070712     c                   eval      *in40 = *off
011300070712     c                   eval      *in41 = *off
011400081008     c                   eval      *in42 = *off
011500081008     c                   clear                   dsdru
011600070712
011700070712      * data iniziale
011800070712     c                   clear                   wlbdat
011900070712     c                   z-add     v1cdip        g08dat
012000070712     c                   if        v1cdip > 9999
012100070712     c                   add       01000000      g08dat
012200070712     c                   else
012300070712     c                   add       00010000      g08dat
012400070712     c                   endif
012500070712     c                   call      'XSRDA8'
012600070712     c                   parm                    wlbdat
012700070712     c                   if        g08err = *on
012800070712     c                   eval      *in28 = *on
012900070712     c                   eval      *in40 = *on
013000070712     c                   eval      v1cmsg = msg(01)
013100070712     c                   leavesr
013200070712     c                   endif
013300070712      * imposto la data a video
013400070712     c                   z-add     g08dat        v1cdip
013500070712      * non inferiore alla data minima di riferimento
013600070712     c                   if        g08inv < wdatamin
013700070712     c                   eval      *in28 = *on
013800070712     c                   eval      *in40 = *on
013900070712     c                   eval      v1cmsg = msg(04)
014000070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
014100070712     c                                  %editc(minmm:'X') + '/' +
014200070712     c                                  %editc(minaa:'X')
014300070712     c                   leavesr
014400070712     c                   endif
014500070712      * imposto la data per l'elaborazione batch
014600070712     c                   eval      dsdip = g08inv/100
014700070712
014800070712      * data finale
014900070712     c                   clear                   wlbdat
015000070712     c                   z-add     v1cdfp        g08dat
015100070712     c                   if        v1cdfp > 9999
015200070712     c                   add       01000000      g08dat
015300070712     c                   else
015400070712     c                   add       00010000      g08dat
015500070712     c                   endif
015600070712     c                   call      'XSRDA8'
015700070712     c                   parm                    wlbdat
015800070712     c                   if        g08err = *on
015900070712     c                   eval      *in28 = *on
016000070712     c                   eval      *in41 = *on
016100070712     c                   eval      v1cmsg = msg(01)
016200070712     c                   leavesr
016300070712     c                   endif
016400070712      * imposto la data a video
016500070712     c                   z-add     g08dat        v1cdfp
016600070712      * non superiore alla data massima di riferimento
016700070712     c                   if        g08inv > wdatamax
016800070712     c                   eval      *in28 = *on
016900070712     c                   eval      *in41 = *on
017000070712     c                   eval      v1cmsg = msg(05)
017100070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
017200070712     c                                  %editc(maxmm:'X') + '/' +
017300070712     c                                  %editc(maxaa:'X')
017400070712     c                   leavesr
017500070712     c                   endif
017600070712      * imposto la data per l'elaborazione batch
017700070712     c                   eval      dsdfp = g08inv/100
017800070712
017900070712      * range di date
018000070712     c                   if        dsdip > dsdfp
018100070712     c                   eval      *in28 = *on
018200070712     c                   eval      *in40 = *on
018300070712     c                   eval      v1cmsg = msg(02)
018400070712     c                   leavesr
018500070712     c                   endif
018600081008      * data ricerca unificante
018700081008     c                   clear                   wlbdat
018800081008     c                   z-add     v1cdru        g08dat
018900081008     c                   call      'XSRDA8'
019000081008     c                   parm                    wlbdat
019100081008     c                   if        g08err = *on
019200081008     c                   eval      *in28 = *on
019300081008     c                   eval      *in42 = *on
019400081008     c                   eval      v1cmsg = msg(01)
019500081008     c                   leavesr
019600081008     c                   endif
019700081008      * imposto la data a video
019800081008     c                   z-add     g08dat        v1cdru
019900081008      * imposto la data a video
020000081008     c                   z-add     g08inv        dsdru
020100070712
020200070712     c                   endsr
020300070712
020400070712      *------------------------------------------------------------------------*
020500070712      * Sottomissione lavoro batch
020600070712      *------------------------------------------------------------------------*
020700070712     c     sbmjob        begsr
020800070712
020900080922     c                   eval      kpjbu = tnta86ds
021000080922     c                   eval      kcoaz = 'TA77'
021100080922     c**!!!              call      'TNTA77R'
021200070712     c                   call      'BCH10'
021300070712     c                   parm                    kpjba
021400070712
021500070712     c                   endsr
021600070712
021700070712      *------------------------------------------------------------------------*
021800070712      * Gestione tasto funzionale F09
021900070712      *------------------------------------------------------------------------*
022000070712     c     f09           begsr
022100070712
022200070712     c                   eval      qcmd    =  'DSPDBF FILE'
022300080925     c                                     +  '(*LIBL/WFTFU10F) '
022400070712     c                                     +  'MBR(*FIRST) '
022500070712     c                                     +  'OUTPUT(*) '
022600070712     c                   call      'QCMDEXC'                            90
022700070712     c                   parm                    qcmd
022800070712     c                   parm                    qlen
022900070712
023000070712     c                   endsr
023100070712
023200070712      *------------------------------------------------------------------------*
023300070712      * Operazioni iniziali                                          ?
023400070712      *------------------------------------------------------------------------*
023500070712     c     *inzsr        begsr
023600070712
023700070712     c     *entry        plist
023800070712     c                   parm                    kpjba
023900070712
024000070712      * reperimento dati job
024100070712     c     *dtaara       define    §azute        azuteds
024200070712     c     *dtaara       define    §datiute      ddatiute
024300070712
024400070712     c                   in(e)     *dtaara
024500070712     c                   if        %error or rsut = *blanks
024600070712     c                   clear                   tibs34ds
024700070712     c                   call      'TIBS34R'
024800070712     c                   parm                    tibs34ds
024900070712     c                   in        *dtaara
025000070712     c                   endif
025100070712
025200070712      * data minima di riferimento
025300070712     c                   move      *date         wdata
025400070712     c                   subdur    2:*y          wdata
025500070712     c                   extrct    wdata:*y      minaa
025600070712     c                   extrct    wdata:*m      minmm
025700070712      * data massima di riferimento
025800070712     c                   move      *date         wdata
025900070712     c                   subdur    1:*m          wdata
026000070712     c                   extrct    wdata:*y      maxaa
026100070712     c                   extrct    wdata:*m      maxmm
026200070712
026300081008     c                   clear                   wlbdat
026400081008     c                   z-add     *date         G08INV
026500081008     c                   eval      g08err = '3'
026600081008     c                   CALL      'XSRDA8'
026700081008     c                   parm                    WLBDAT
026800081008     c                   move      G08DAT        v1cdru
026900081008
027000070712     c                   endsr
027100070712
027200070712** - msg --------------------------------------------------------------------*
027300070712Data in formato errato                                                         1
027400070712Range di date errato                                                           2
027500070712Funzione eseguibile solo in ambiente di SEDE                                   3
027600070712La data non può essere inferiore al                                            4
027700070712La data non può essere superiore al                                            5
