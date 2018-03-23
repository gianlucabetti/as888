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
001700121211      * 43 - data inizio vigore tariffa errata
001800070712
001900070712      *------------------------------------------------------------------------*
002000070712      *   V A R I A B I L I
002100070712      *------------------------------------------------------------------------*
002200070712     d qcmd            s            100    inz
002300070712     d qlen            s             15  5 inz(100)
002400070712     d wdata           s               d   datfmt(*iso)
002500070712     d $fine           s              1    inz(*off)
002600070712
002700070712      *------------------------------------------------------------------------*
002800070712      *   S C H I E R E
002900070712      *------------------------------------------------------------------------*
003000070712     d msg             s             78    dim(10)  ctdata  perrcd(1)
003100070712
003200070712      *------------------------------------------------------------------------*
003300070712      *   D S   I N T E R N E / E S T E R N E
003400070712      *------------------------------------------------------------------------*
003500070712     d tnta79ds        ds                  inz
003600070712     d  dsdip                         6  0 inz(*loval)
003700070712     d  dsdfp                         6  0 inz(*hival)
003800081009     d  dsdru                         8  0 inz(*hival)
003900121211     d  dsDVT                         8  0 inz(*hival)
004000121128     d  ds10F                         1    inz
004100070712
004200070712     d                 ds                  inz
004300070712     d wdatamax                1      8  0 inz
004400070712     d maxaa                   1      4  0 inz
004500070712     d maxmm                   5      6  0 inz
004600070712     d maxgg                   7      8  0 inz(01)
004700070712
004800070712     d                 ds                  inz
004900070712     d wdatamin                1      8  0 inz
005000070712     d minaa                   1      4  0 inz
005100070712     d minmm                   5      6  0 inz
005200070712     d mingg                   7      8  0 inz(01)
005300070712
005400070712     d wlbdat          ds                  inz
005500070712     d  g08dat                        8  0 inz
005600070712     d  g08inv                        8  0 inz
005700070712     d  g08err                        1    inz('3')
005800070712     d  g08tgi                        5  0 inz
005900070712
006000070712     d azuteds       e ds                       extname(azute00f)
006100070712     d ddatiute      e ds
006200070712     d kpjba         e ds
006300070712     d tibs34ds      e ds
006400070712
006500070712     d status         sds
006600070712     d  v1cpgm           *proc
006700070712
006800070228      *---------------------------------------------------------------*
006900070712
007000070712     c                   clear                   v1cdip
007100070712     c                   clear                   v1cdfp
007200070712
007300070712     c                   do        *hival
007400070712
007500070712     c                   exfmt     ta78d01
007600070712     c                   clear                   v1cmsg
007700070712     c                   eval      *in28 = *off
007800070712
007900070712      * f3=fine
008000070712     c   kc              leave
008100070712
008200070712     c                   if        dutpou <> 046
008300070712     c                   eval      v1cmsg = msg(03)
008400070712     c                   eval      *in28 = *on
008500070712     c                   iter
008600070712     c                   endif
008700070712
008800070712      * f9=visualizzazione archivio wfist00f
008900070712     c                   if        *inki
009000070712     c                   exsr      f09
009100070712     c                   iter
009200070712     c                   endif
009300070712
009400070712      * controllo i dati immessi a video
009500070712     c                   exsr      ctrd01
009600070712     c                   if        *in28
009700070712     c                   iter
009800070712     c                   endif
009900070712
010000070712      * f6=lancio batch
010100070712     c                   if        *inkf
010200070712     c                   exsr      sbmjob
010300070712     c                   leave
010400070712     c                   endif
010500070712
010600070712     c                   enddo
010700070712
010800070712     c                   eval      *inlr = *on
010900070712
011000070712      *------------------------------------------------------------------------*
011100070712      * Controlli videata
011200070712      *------------------------------------------------------------------------*
011300070712     c     ctrd01        begsr
011400070712
011500070712     c                   eval      *in40 = *off
011600070712     c                   eval      *in41 = *off
011700081009     c                   eval      *in42 = *off
011800081009     c                   clear                   dsdru
011900070712
012000070712      * data iniziale
012100070712     c                   clear                   wlbdat
012200070712     c                   z-add     v1cdip        g08dat
012300070712     c                   if        v1cdip > 9999
012400070712     c                   add       01000000      g08dat
012500070712     c                   else
012600070712     c                   add       00010000      g08dat
012700070712     c                   endif
012800070712     c                   call      'XSRDA8'
012900070712     c                   parm                    wlbdat
013000070712     c                   if        g08err = *on
013100070712     c                   eval      *in28 = *on
013200070712     c                   eval      *in40 = *on
013300070712     c                   eval      v1cmsg = msg(01)
013400070712     c                   leavesr
013500070712     c                   endif
013600070712      * imposto la data a video
013700070712     c                   z-add     g08dat        v1cdip
013800070712      * non inferiore alla data minima di riferimento
013900070712     c                   if        g08inv < wdatamin
014000070712     c                   eval      *in28 = *on
014100070712     c                   eval      *in40 = *on
014200070712     c                   eval      v1cmsg = msg(04)
014300070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
014400070712     c                                  %editc(minmm:'X') + '/' +
014500070712     c                                  %editc(minaa:'X')
014600070712     c                   leavesr
014700070712     c                   endif
014800070712      * imposto la data per l'elaborazione batch
014900070712     c                   eval      dsdip = g08inv/100
015000070712
015100070712      * data finale
015200070712     c                   clear                   wlbdat
015300070712     c                   z-add     v1cdfp        g08dat
015400070712     c                   if        v1cdfp > 9999
015500070712     c                   add       01000000      g08dat
015600070712     c                   else
015700070712     c                   add       00010000      g08dat
015800070712     c                   endif
015900070712     c                   call      'XSRDA8'
016000070712     c                   parm                    wlbdat
016100070712     c                   if        g08err = *on
016200070712     c                   eval      *in28 = *on
016300070712     c                   eval      *in41 = *on
016400070712     c                   eval      v1cmsg = msg(01)
016500070712     c                   leavesr
016600070712     c                   endif
016700070712      * imposto la data a video
016800070712     c                   z-add     g08dat        v1cdfp
016900070712      * non superiore alla data massima di riferimento
017000070712     c                   if        g08inv > wdatamax
017100070712     c                   eval      *in28 = *on
017200070712     c                   eval      *in41 = *on
017300070712     c                   eval      v1cmsg = msg(05)
017400070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
017500070712     c                                  %editc(maxmm:'X') + '/' +
017600070712     c                                  %editc(maxaa:'X')
017700070712     c                   leavesr
017800070712     c                   endif
017900070712      * imposto la data per l'elaborazione batch
018000070712     c                   eval      dsdfp = g08inv/100
018100070712
018200070712      * range di date
018300070712     c                   if        dsdip > dsdfp
018400070712     c                   eval      *in28 = *on
018500070712     c                   eval      *in40 = *on
018600070712     c                   eval      v1cmsg = msg(02)
018700070712     c                   leavesr
018800070712     c                   endif
018900081009      * data ricerca unificante
019000081009     c                   clear                   wlbdat
019100081009     c                   z-add     v1cdru        g08dat
019200081009     c                   call      'XSRDA8'
019300081009     c                   parm                    wlbdat
019400081009     c                   if        g08err = *on
019500081009     c                   eval      *in28 = *on
019600081009     c                   eval      *in42 = *on
019700081009     c                   eval      v1cmsg = msg(01)
019800081009     c                   leavesr
019900081009     c                   endif
020000081009      * imposto la data a video
020100081009     c                   z-add     g08dat        v1cdru
020200121211      * imposto la data come parametro
020300081009     c                   z-add     g08inv        dsdru
020400121211      * data tariffa in vigore al
020500121211     c                   clear                   wlbdat
020600121211     c                   z-add     V1Cdvt        g08dat
020700121211     c                   call      'XSRDA8'
020800121211     c                   parm                    wlbdat
020900121211     c                   if        g08err = *on
021000121211     c                   eval      *in28 = *on
021100121211     c                   eval      *in43 = *on
021200121211     c                   eval      V1Cmsg = msg(01)
021300121211     c                   leavesr
021400121211     c                   endif
021500121211      * imposto la data a video
021600121211     c                   z-add     g08dat        V1Cdvt
021700121211      * imposto la data come parametro
021800121211     c                   z-add     g08inv        dsDVT
021900070712
022000070712     c                   endsr
022100070712
022200070712      *------------------------------------------------------------------------*
022300070712      * Sottomissione lavoro batch
022400070712      *------------------------------------------------------------------------*
022500070712     c     sbmjob        begsr
022600070712
022700070712     c                   eval      kpjbu = tnta79ds
022800070712     c                   eval      kcoaz = 'TA79'
022900070712     c**!!!              call      'TNTA79R'
023000070712     c                   call      'BCH10'
023100070712     c                   parm                    kpjba
023200070712
023300070712     c                   endsr
023400070712
023500070712      *------------------------------------------------------------------------*
023600070712      * Gestione tasto funzionale F09
023700070712      *------------------------------------------------------------------------*
023800070712     c     f09           begsr
023900070712
024000070712     c                   eval      qcmd    =  'DSPDBF FILE'
024100070712     c                                     +  '(*LIBL/WFIST00F) '
024200070712     c                                     +  'MBR(*FIRST) '
024300070712     c                                     +  'OUTPUT(*) '
024400070712     c                   call      'QCMDEXC'                            90
024500070712     c                   parm                    qcmd
024600070712     c                   parm                    qlen
024700070712
024800070712     c                   endsr
024900070712
025000070712      *------------------------------------------------------------------------*
025100070712      * Operazioni iniziali                                          ?
025200070712      *------------------------------------------------------------------------*
025300070712     c     *inzsr        begsr
025400070712
025500070712     c     *entry        plist
025600070712     c                   parm                    kpjba
025700070712
025800070712      * reperimento dati job
025900070712     c     *dtaara       define    §azute        azuteds
026000070712     c     *dtaara       define    §datiute      ddatiute
026100070712
026200070712     c                   in(e)     *dtaara
026300070712     c                   if        %error or rsut = *blanks
026400070712     c                   clear                   tibs34ds
026500070712     c                   call      'TIBS34R'
026600070712     c                   parm                    tibs34ds
026700070712     c                   in        *dtaara
026800070712     c                   endif
026900070712
027000070712      * data minima di riferimento
027100070712     c                   move      *date         wdata
027200070712     c                   subdur    2:*y          wdata
027300070712     c                   extrct    wdata:*y      minaa
027400070712     c                   extrct    wdata:*m      minmm
027500070712      * data massima di riferimento
027600070712     c                   move      *date         wdata
027700070712     c                   subdur    1:*m          wdata
027800070712     c                   extrct    wdata:*y      maxaa
027900070712     c                   extrct    wdata:*m      maxmm
028000081009
028100081009     c                   clear                   wlbdat
028200081009     c                   z-add     *date         G08INV
028300081009     c                   eval      g08err = '3'
028400081009     c                   CALL      'XSRDA8'
028500081009     c                   parm                    WLBDAT
028600081009     c                   move      G08DAT        v1cdru
028700121211     c                   eval      V1Cdvt = G08dat
028800081009
028900070712     c                   endsr
029000070712
029100070712** - msg --------------------------------------------------------------------*
029200070712Data in formato errato                                                         1
029300070712Range di date errato                                                           2
029400070712Funzione eseguibile solo in ambiente di SEDE                                   3
029500070712La data non può essere inferiore al                                            4
029600070712La data non può essere superiore al                                            5
