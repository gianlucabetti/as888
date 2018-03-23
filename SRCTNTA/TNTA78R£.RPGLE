000100070712     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200070712      *------------------------------------------------------------------------*
000300070712      *
000400070713      *  Stato applicazione ISTAT per tariffa
000500070712      *
000600070712      *------------------------------------------------------------------------*
000700070712
000800070712     ftnta78d   cf   e             workstn
000900070712
001000070712      *------------------------------------------------------------------------*
001100070712      *  RIEPILOGO INDICATORI
001200070712      *------------------------------------------------------------------------*
001300070712      * 28 - emissione messaggio di errore
001400070712      * 40 - data iniziale errata
001500070712      * 41 - data finale   errata
001600081009      * 42 - data ricerca codice unificante
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
003400070712     d tnta79ds        ds                  inz
003500070712     d  dsdip                         6  0 inz(*loval)
003600070712     d  dsdfp                         6  0 inz(*hival)
003700081009     d  dsdru                         8  0 inz(*hival)
003800121128     d  ds10F                         1    inz
003900070712
004000070712     d                 ds                  inz
004100070712     d wdatamax                1      8  0 inz
004200070712     d maxaa                   1      4  0 inz
004300070712     d maxmm                   5      6  0 inz
004400070712     d maxgg                   7      8  0 inz(01)
004500070712
004600070712     d                 ds                  inz
004700070712     d wdatamin                1      8  0 inz
004800070712     d minaa                   1      4  0 inz
004900070712     d minmm                   5      6  0 inz
005000070712     d mingg                   7      8  0 inz(01)
005100070712
005200070712     d wlbdat          ds                  inz
005300070712     d  g08dat                        8  0 inz
005400070712     d  g08inv                        8  0 inz
005500070712     d  g08err                        1    inz('3')
005600070712     d  g08tgi                        5  0 inz
005700070712
005800070712     d azuteds       e ds                       extname(azute00f)
005900070712     d ddatiute      e ds
006000070712     d kpjba         e ds
006100070712     d tibs34ds      e ds
006200070712
006300070712     d status         sds
006400070712     d  v1cpgm           *proc
006500070712
006600070228      *---------------------------------------------------------------*
006700070712
006800070712     c                   clear                   v1cdip
006900070712     c                   clear                   v1cdfp
007000070712
007100070712     c                   do        *hival
007200070712
007300070712     c                   exfmt     ta78d01
007400070712     c                   clear                   v1cmsg
007500070712     c                   eval      *in28 = *off
007600070712
007700070712      * f3=fine
007800070712     c   kc              leave
007900070712
008000070712     c                   if        dutpou <> 046
008100070712     c                   eval      v1cmsg = msg(03)
008200070712     c                   eval      *in28 = *on
008300070712     c                   iter
008400070712     c                   endif
008500070712
008600070712      * f9=visualizzazione archivio wfist00f
008700070712     c                   if        *inki
008800070712     c                   exsr      f09
008900070712     c                   iter
009000070712     c                   endif
009100070712
009200070712      * controllo i dati immessi a video
009300070712     c                   exsr      ctrd01
009400070712     c                   if        *in28
009500070712     c                   iter
009600070712     c                   endif
009700070712
009800070712      * f6=lancio batch
009900070712     c                   if        *inkf
010000070712     c                   exsr      sbmjob
010100070712     c                   leave
010200070712     c                   endif
010300070712
010400070712     c                   enddo
010500070712
010600070712     c                   eval      *inlr = *on
010700070712
010800070712      *------------------------------------------------------------------------*
010900070712      * Controlli videata
011000070712      *------------------------------------------------------------------------*
011100070712     c     ctrd01        begsr
011200070712
011300070712     c                   eval      *in40 = *off
011400070712     c                   eval      *in41 = *off
011500081009     c                   eval      *in42 = *off
011600081009     c                   clear                   dsdru
011700070712
011800070712      * data iniziale
011900070712     c                   clear                   wlbdat
012000070712     c                   z-add     v1cdip        g08dat
012100070712     c                   if        v1cdip > 9999
012200070712     c                   add       01000000      g08dat
012300070712     c                   else
012400070712     c                   add       00010000      g08dat
012500070712     c                   endif
012600070712     c                   call      'XSRDA8'
012700070712     c                   parm                    wlbdat
012800070712     c                   if        g08err = *on
012900070712     c                   eval      *in28 = *on
013000070712     c                   eval      *in40 = *on
013100070712     c                   eval      v1cmsg = msg(01)
013200070712     c                   leavesr
013300070712     c                   endif
013400070712      * imposto la data a video
013500070712     c                   z-add     g08dat        v1cdip
013600070712      * non inferiore alla data minima di riferimento
013700070712     c                   if        g08inv < wdatamin
013800070712     c                   eval      *in28 = *on
013900070712     c                   eval      *in40 = *on
014000070712     c                   eval      v1cmsg = msg(04)
014100070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
014200070712     c                                  %editc(minmm:'X') + '/' +
014300070712     c                                  %editc(minaa:'X')
014400070712     c                   leavesr
014500070712     c                   endif
014600070712      * imposto la data per l'elaborazione batch
014700070712     c                   eval      dsdip = g08inv/100
014800070712
014900070712      * data finale
015000070712     c                   clear                   wlbdat
015100070712     c                   z-add     v1cdfp        g08dat
015200070712     c                   if        v1cdfp > 9999
015300070712     c                   add       01000000      g08dat
015400070712     c                   else
015500070712     c                   add       00010000      g08dat
015600070712     c                   endif
015700070712     c                   call      'XSRDA8'
015800070712     c                   parm                    wlbdat
015900070712     c                   if        g08err = *on
016000070712     c                   eval      *in28 = *on
016100070712     c                   eval      *in41 = *on
016200070712     c                   eval      v1cmsg = msg(01)
016300070712     c                   leavesr
016400070712     c                   endif
016500070712      * imposto la data a video
016600070712     c                   z-add     g08dat        v1cdfp
016700070712      * non superiore alla data massima di riferimento
016800070712     c                   if        g08inv > wdatamax
016900070712     c                   eval      *in28 = *on
017000070712     c                   eval      *in41 = *on
017100070712     c                   eval      v1cmsg = msg(05)
017200070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
017300070712     c                                  %editc(maxmm:'X') + '/' +
017400070712     c                                  %editc(maxaa:'X')
017500070712     c                   leavesr
017600070712     c                   endif
017700070712      * imposto la data per l'elaborazione batch
017800070712     c                   eval      dsdfp = g08inv/100
017900070712
018000070712      * range di date
018100070712     c                   if        dsdip > dsdfp
018200070712     c                   eval      *in28 = *on
018300070712     c                   eval      *in40 = *on
018400070712     c                   eval      v1cmsg = msg(02)
018500070712     c                   leavesr
018600070712     c                   endif
018700081009      * data ricerca unificante
018800081009     c                   clear                   wlbdat
018900081009     c                   z-add     v1cdru        g08dat
019000081009     c                   call      'XSRDA8'
019100081009     c                   parm                    wlbdat
019200081009     c                   if        g08err = *on
019300081009     c                   eval      *in28 = *on
019400081009     c                   eval      *in42 = *on
019500081009     c                   eval      v1cmsg = msg(01)
019600081009     c                   leavesr
019700081009     c                   endif
019800081009      * imposto la data a video
019900081009     c                   z-add     g08dat        v1cdru
020000081009      * imposto la data a video
020100081009     c                   z-add     g08inv        dsdru
020200070712
020300070712     c                   endsr
020400070712
020500070712      *------------------------------------------------------------------------*
020600070712      * Sottomissione lavoro batch
020700070712      *------------------------------------------------------------------------*
020800070712     c     sbmjob        begsr
020900070712
021000070712     c                   eval      kpjbu = tnta79ds
021100070712     c                   eval      kcoaz = 'TA79'
021200070712     c**!!!              call      'TNTA79R'
021300070712     c                   call      'BCH10'
021400070712     c                   parm                    kpjba
021500070712
021600070712     c                   endsr
021700070712
021800070712      *------------------------------------------------------------------------*
021900070712      * Gestione tasto funzionale F09
022000070712      *------------------------------------------------------------------------*
022100070712     c     f09           begsr
022200070712
022300070712     c                   eval      qcmd    =  'DSPDBF FILE'
022400070712     c                                     +  '(*LIBL/WFIST00F) '
022500070712     c                                     +  'MBR(*FIRST) '
022600070712     c                                     +  'OUTPUT(*) '
022700070712     c                   call      'QCMDEXC'                            90
022800070712     c                   parm                    qcmd
022900070712     c                   parm                    qlen
023000070712
023100070712     c                   endsr
023200070712
023300070712      *------------------------------------------------------------------------*
023400070712      * Operazioni iniziali                                          ?
023500070712      *------------------------------------------------------------------------*
023600070712     c     *inzsr        begsr
023700070712
023800070712     c     *entry        plist
023900070712     c                   parm                    kpjba
024000070712
024100070712      * reperimento dati job
024200070712     c     *dtaara       define    §azute        azuteds
024300070712     c     *dtaara       define    §datiute      ddatiute
024400070712
024500070712     c                   in(e)     *dtaara
024600070712     c                   if        %error or rsut = *blanks
024700070712     c                   clear                   tibs34ds
024800070712     c                   call      'TIBS34R'
024900070712     c                   parm                    tibs34ds
025000070712     c                   in        *dtaara
025100070712     c                   endif
025200070712
025300070712      * data minima di riferimento
025400070712     c                   move      *date         wdata
025500070712     c                   subdur    2:*y          wdata
025600070712     c                   extrct    wdata:*y      minaa
025700070712     c                   extrct    wdata:*m      minmm
025800070712      * data massima di riferimento
025900070712     c                   move      *date         wdata
026000070712     c                   subdur    1:*m          wdata
026100070712     c                   extrct    wdata:*y      maxaa
026200070712     c                   extrct    wdata:*m      maxmm
026300081009
026400081009     c                   clear                   wlbdat
026500081009     c                   z-add     *date         G08INV
026600081009     c                   eval      g08err = '3'
026700081009     c                   CALL      'XSRDA8'
026800081009     c                   parm                    WLBDAT
026900081009     c                   move      G08DAT        v1cdru
027000081009
027100070712     c                   endsr
027200070712
027300070712** - msg --------------------------------------------------------------------*
027400070712Data in formato errato                                                         1
027500070712Range di date errato                                                           2
027600070712Funzione eseguibile solo in ambiente di SEDE                                   3
027700070712La data non può essere inferiore al                                            4
027800070712La data non può essere superiore al                                            5
