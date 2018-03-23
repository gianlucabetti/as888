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
001700130403      * 43 - data inizio vigore tariffa errata
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
003500080922     d tnta86ds        ds                  inz
003600070712     d  dsdip                         6  0 inz(*loval)
003700070712     d  dsdfp                         6  0 inz(*hival)
003800081008     d  dsdru                         8  0 inz(*hival)
003900130403     d  dsDVT                         8  0 inz(*hival)
004000130403     d  ds10F                         1    inz
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
007500080922     c                   exfmt     ta86d01
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
008800080925      * f9=visualizzazione archivio wftfu10f
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
011700081008     c                   eval      *in42 = *off
011800130403     c                   eval      *in43 = *off
011900081008     c                   clear                   dsdru
012000070712
012100070712      * data iniziale
012200070712     c                   clear                   wlbdat
012300070712     c                   z-add     v1cdip        g08dat
012400070712     c                   if        v1cdip > 9999
012500070712     c                   add       01000000      g08dat
012600070712     c                   else
012700070712     c                   add       00010000      g08dat
012800070712     c                   endif
012900070712     c                   call      'XSRDA8'
013000070712     c                   parm                    wlbdat
013100070712     c                   if        g08err = *on
013200070712     c                   eval      *in28 = *on
013300070712     c                   eval      *in40 = *on
013400070712     c                   eval      v1cmsg = msg(01)
013500070712     c                   leavesr
013600070712     c                   endif
013700070712      * imposto la data a video
013800070712     c                   z-add     g08dat        v1cdip
013900070712      * non inferiore alla data minima di riferimento
014000070712     c                   if        g08inv < wdatamin
014100070712     c                   eval      *in28 = *on
014200070712     c                   eval      *in40 = *on
014300070712     c                   eval      v1cmsg = msg(04)
014400070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
014500070712     c                                  %editc(minmm:'X') + '/' +
014600070712     c                                  %editc(minaa:'X')
014700070712     c                   leavesr
014800070712     c                   endif
014900070712      * imposto la data per l'elaborazione batch
015000070712     c                   eval      dsdip = g08inv/100
015100070712
015200070712      * data finale
015300070712     c                   clear                   wlbdat
015400070712     c                   z-add     v1cdfp        g08dat
015500070712     c                   if        v1cdfp > 9999
015600070712     c                   add       01000000      g08dat
015700070712     c                   else
015800070712     c                   add       00010000      g08dat
015900070712     c                   endif
016000070712     c                   call      'XSRDA8'
016100070712     c                   parm                    wlbdat
016200070712     c                   if        g08err = *on
016300070712     c                   eval      *in28 = *on
016400070712     c                   eval      *in41 = *on
016500070712     c                   eval      v1cmsg = msg(01)
016600070712     c                   leavesr
016700070712     c                   endif
016800070712      * imposto la data a video
016900070712     c                   z-add     g08dat        v1cdfp
017000070712      * non superiore alla data massima di riferimento
017100070712     c                   if        g08inv > wdatamax
017200070712     c                   eval      *in28 = *on
017300070712     c                   eval      *in41 = *on
017400070712     c                   eval      v1cmsg = msg(05)
017500070712     c                   eval      v1cmsg = %trim(v1cmsg) + ' ' +
017600070712     c                                  %editc(maxmm:'X') + '/' +
017700070712     c                                  %editc(maxaa:'X')
017800070712     c                   leavesr
017900070712     c                   endif
018000070712      * imposto la data per l'elaborazione batch
018100070712     c                   eval      dsdfp = g08inv/100
018200070712
018300070712      * range di date
018400070712     c                   if        dsdip > dsdfp
018500070712     c                   eval      *in28 = *on
018600070712     c                   eval      *in40 = *on
018700070712     c                   eval      v1cmsg = msg(02)
018800070712     c                   leavesr
018900070712     c                   endif
019000081008      * data ricerca unificante
019100081008     c                   clear                   wlbdat
019200081008     c                   z-add     v1cdru        g08dat
019300081008     c                   call      'XSRDA8'
019400081008     c                   parm                    wlbdat
019500081008     c                   if        g08err = *on
019600081008     c                   eval      *in28 = *on
019700081008     c                   eval      *in42 = *on
019800081008     c                   eval      v1cmsg = msg(01)
019900081008     c                   leavesr
020000081008     c                   endif
020100081008      * imposto la data a video
020200081008     c                   z-add     g08dat        v1cdru
020300081008      * imposto la data a video
020400081008     c                   z-add     g08inv        dsdru
020500130403
020600130403      * data tariffa in vigore al
020700130403     c                   clear                   wlbdat
020800130403     c                   z-add     V1Cdvt        g08dat
020900130403     c                   call      'XSRDA8'
021000130403     c                   parm                    wlbdat
021100130403     c                   if        g08err = *on
021200130403     c                   eval      *in28 = *on
021300130403     c                   eval      *in43 = *on
021400130403     c                   eval      V1Cmsg = msg(01)
021500130403     c                   leavesr
021600130403     c                   endif
021700130403      * imposto la data a video
021800130403     c                   z-add     g08dat        V1Cdvt
021900130403      * imposto la data come parametro
022000130403     c                   z-add     g08inv        dsDVT
022100070712
022200070712     c                   endsr
022300070712
022400070712      *------------------------------------------------------------------------*
022500070712      * Sottomissione lavoro batch
022600070712      *------------------------------------------------------------------------*
022700070712     c     sbmjob        begsr
022800070712
022900080922     c                   eval      kpjbu = tnta86ds
023000080922     c                   eval      kcoaz = 'TA77'
023100080922     c**!!!              call      'TNTA77R'
023200070712     c                   call      'BCH10'
023300070712     c                   parm                    kpjba
023400070712
023500070712     c                   endsr
023600070712
023700070712      *------------------------------------------------------------------------*
023800070712      * Gestione tasto funzionale F09
023900070712      *------------------------------------------------------------------------*
024000070712     c     f09           begsr
024100070712
024200070712     c                   eval      qcmd    =  'DSPDBF FILE'
024300080925     c                                     +  '(*LIBL/WFTFU10F) '
024400070712     c                                     +  'MBR(*FIRST) '
024500070712     c                                     +  'OUTPUT(*) '
024600070712     c                   call      'QCMDEXC'                            90
024700070712     c                   parm                    qcmd
024800070712     c                   parm                    qlen
024900070712
025000070712     c                   endsr
025100070712
025200070712      *------------------------------------------------------------------------*
025300070712      * Operazioni iniziali                                          ?
025400070712      *------------------------------------------------------------------------*
025500070712     c     *inzsr        begsr
025600070712
025700070712     c     *entry        plist
025800070712     c                   parm                    kpjba
025900070712
026000070712      * reperimento dati job
026100070712     c     *dtaara       define    §azute        azuteds
026200070712     c     *dtaara       define    §datiute      ddatiute
026300070712
026400070712     c                   in(e)     *dtaara
026500070712     c                   if        %error or rsut = *blanks
026600070712     c                   clear                   tibs34ds
026700070712     c                   call      'TIBS34R'
026800070712     c                   parm                    tibs34ds
026900070712     c                   in        *dtaara
027000070712     c                   endif
027100070712
027200070712      * data minima di riferimento
027300070712     c                   move      *date         wdata
027400070712     c                   subdur    2:*y          wdata
027500070712     c                   extrct    wdata:*y      minaa
027600070712     c                   extrct    wdata:*m      minmm
027700070712      * data massima di riferimento
027800070712     c                   move      *date         wdata
027900070712     c                   subdur    1:*m          wdata
028000070712     c                   extrct    wdata:*y      maxaa
028100070712     c                   extrct    wdata:*m      maxmm
028200070712
028300081008     c                   clear                   wlbdat
028400081008     c                   z-add     *date         G08INV
028500081008     c                   eval      g08err = '3'
028600081008     c                   CALL      'XSRDA8'
028700081008     c                   parm                    WLBDAT
028800081008     c                   move      G08DAT        v1cdru
028900130403     c                   eval      V1Cdvt = G08dat
029000081008
029100070712     c                   endsr
029200070712
029300070712** - msg --------------------------------------------------------------------*
029400070712Data in formato errato                                                         1
029500070712Range di date errato                                                           2
029600070712Funzione eseguibile solo in ambiente di SEDE                                   3
029700070712La data non può essere inferiore al                                            4
029800070712La data non può essere superiore al                                            5
