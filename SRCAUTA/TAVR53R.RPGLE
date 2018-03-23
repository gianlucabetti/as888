000100070712     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200070712      *------------------------------------------------------------------------*
000300070712      *
000400101221      *  Creazione archivio aumento offerte
000500070712      *
000600070712      *------------------------------------------------------------------------*
000700070712
000800101221     ftnofm00l  if   e           k disk
000801101221     ftiofd01l  if   e           k disk
000802101221     ftivis05l  if   e           k disk
001200101221     fwfauo00f  o    e             disk    usropn extfile('EDPAUMENTI/WFAUO00F')
001300070712
001400070712      *------------------------------------------------------------------------*
001500070712      *  RIEPILOGO INDICATORI
001600070712      *------------------------------------------------------------------------*
001700070712      * 02 - ho scritto almeno 1 rcd
001800070712
001900070712      *------------------------------------------------------------------------*
002000070712      *   V A R I A B I L I
002100070712      *------------------------------------------------------------------------*
002200070712     d comman          s             80
002300070712     d codut           s              1  0 inz(1)
002600070712     d kksc            s                   like(tamksc)
002800070712     d lenght          s             15  5 inz(80)
003100070712     d wctr            s                   like(tamctr)
003200070712     d wdata           s              8  0
003300070712     d wksc            s                   like(tamksc)
003400070712     d wlib            s              9
003500070712     d wok             s              1    inz(*off)
003600070712     d wora            s              6  0
003700070712     d w0030           s              3  0
003800070712     d w0140           s             14  0
004200070712
004300070712      *------------------------------------------------------------------------*
004400070712      *   S C H I E R E
004500070712      *------------------------------------------------------------------------*
004600070712     d cmd             s             80    dim(1) ctdata perrcd(1)
004800070712
004900070712      *------------------------------------------------------------------------*
005000070712      *   D S   I N T E R N E / E S T E R N E
005100070712      *------------------------------------------------------------------------*
005200080924
005300080924     d wlbdat          ds                  inz
005400080924     d  g02dat                 1      8  0
005500080924     d  g02inv                 9     16  0
005600080924     d  g02err                17     17
005700080924     d  g02tgi                18     22  0
005800080924
005900070712
006000070712     d azuteds       e ds                  extname(azute00f)
006100070712     d ddatiute      e ds
006600070712     d kpjba         e ds
006900070712     d dsta01        e ds
007500070712     d tibs34ds      e ds
007900101221     d wfautds       e ds                  extname(wfauo00f)
008000081127     d                                     prefix(DS_)
008100070712
008200070712      *------------------------------------------------------------------------*
008900070712
009000070712     c                   clear                   wksc
009100070712     c                   eval      wctr = 999
009200101216     c                   move      *all'0'       kksc
009300070712
009400101221     c     kksc          setll     tnofm00l
009500070712    1c                   do        *hival
009600101221     c                   read      tnofm00l
009700070712      * fine file
009800101221     c                   if        %eof(tnofm00l)
009900070712     c                   leave
010000070712     c                   endif
010501101216     c                   movel     tamflo        dsta01
010502101216      * se tariffe non FEDEX   leggo il successivo
010504101216     c                   if        §tafed <> 'S'
010505101216     c                   iter
010506101216     c                   endif
010507101216      * se tariffe non FEDEX  MERCI leggo il successivo
010508101216     c                   if        tamctr > 349 or tamctr < 300
010509101216     c                   iter
010510101216     c                   endif
010511101221      * non controllo la data di validità dell'offerta modifico tutto e basta
010900070712
011400070712      * scrivo il file
011500070712     c                   exsr      scrivi
011600070712
012700070712    1c                   enddo
012900081126
013000070712     c                   eval      *inlr = *on
013100081126
013200070712      *------------------------------------------------------------------------*
013300081127      * SCRIVO IL FILE PER GLI AUMENTI
013400070712      *------------------------------------------------------------------------*
013500070712     c     scrivi        begsr
013600070712
013700081127     c                   clear                   wfaut000
013800070712
015700101221      * trattativa
015800101221     c                   eval      wfanrv = tamksc
020700070712      * tariffa
020800081127     c                   eval      wfactrv = tamctr
020900081127     c                   eval      wfaprgv = tamprg
021000081127     c                   eval      wfaddtv = tamddt
021100081127     c                   eval      wfadstv = tamdst
021200101216      * RPV   presente
021601101221     c     ktad          setll     tiofd01l
021602101221     c     ktad          reade     tiofd01l
021603101221     c                   If        not %eof(tiofd01l)
021604101216      * scrivo il record solo se rpv minore di 2
021605101216     c                   if        tadrpv < 2
021800101216     c                   eval      wfarpvv = tadrpv
021801101216     c                   else
021802101216     c                   leavesr
022000101216     c                   endif
022300081125     c                   endif
022301101221      * recupero dei dati dalla trattativa
022302101221     c     tamksc        chain     tivis05l
022303101221     c                   if        %found(tivis05l)
022304101221     c                   eval      wfarag = visrag
022305101221     c                   eval      wfacmm = viscmm
022306101221     c                   endif
022700081125
022800081127     c                   write     wfaut000
022900070712
023000070712     c                   endsr
023100070712
023200081126
049800081126      *
049900070712      *--------------------------------------------------------------*
050000070712      * ROUTINE INIZIALE
050100070712      *--------------------------------------------------------------*
050200070712     c     *inzsr        begsr
050300070712
050400070712     c     *entry        plist
050500070712     c                   parm                    kpjba
050600070712
050700070712     c     *dtaara       define    §azute        azuteds
050800070712     c     *dtaara       define    §datiute      ddatiute
050900070712
051000070712     c                   in(e)     *dtaara
051100070712     c                   if        %error or rsut = *blanks
051200070712     c                   clear                   tibs34ds
051300070712     c                   call      'TIBS34R'
051400070712     c                   parm                    tibs34ds
051500070712     c                   in        *dtaara
051600070712     c                   endif
054400070712
054500070712      * pulisco il file
054600070712     c                   clear                   comman
054700070712     c                   movel(p)  cmd(1)        comman
054800070712     c                   call      'QCMDEXC'
054900070712     c                   parm                    comman
055000070712     c                   parm                    lenght
055100070712
055200101221     c                   open      wfauo00f
055300070712
055400070712      * reperisco data e ora del lancio
055500070712     c                   time                    w0140
055600070712     c                   move      w0140         wdata
055700070712     c                   movel     w0140         wora
055800070712
056700070712
056701101216     c     ktad          klist
056702101216     c                   kfld                    tamksc
056703101216     c                   kfld                    tamctr
056704101216     c                   kfld                    tamprg
056705101216
057200070712
057300070712     c                   endsr
057400070712      *--------------------------------------------------------------*
057500070712**   cmd - comando
057600101221CLRPFM FILE(EDPAUMENTI/WFAUO00F)
